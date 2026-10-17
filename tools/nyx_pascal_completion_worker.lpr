{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

{ Optional CodeTools process adapter. CodeTools carries GPLv2-or-later terms;
  this executable is built separately and is never linked into Nyx's portable
  library, Studio or server. Its private host arguments are not project data.
  One process owns CodeTools' single global manager; queries never execute apps. }

program nyx_pascal_completion_worker;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, CodeCache, CodeToolManager, CodeToolsConfig,
  IdentCompletionTool, CodeTree, BasicCodeTools, PascalParserTool,
  nyx.text, nyx.bytes, nyx.editing, nyx.pascal.names, nyx.pascal.project,
  nyx.pascal.drafts, nyx.pascal.sources, nyx.pascal.completion,
  nyx.pascal.completion.wire;

{ The host owns all files/paths. Source payloads use exact UTF-8 bytes; no ANSI
  string list, process-global codepage change or text-file encoding participates. }
function ReadText(const APath: String; AMaximum: Integer): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if LFile.Size > AMaximum then
    begin
      raise Exception.Create('Completion input exceeds its byte budget');
    end;
    SetLength(LBytes, LFile.Size);

    if Length(LBytes) > 0 then
    begin
      LFile.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;

procedure WriteText(const APath: String; const AText: TNyxText);
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LFile := TFileStream.Create(APath, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LFile.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LFile.Free;
  end;
end;

{ CodeTools strings hold UTF-8 bytes under its native String API. Copy bytes
  explicitly in both directions; assigning CP_UTF8 to an ANSI String can transcode. }
function ToHost(const AText: TNyxText): String;
var
  LBytes: TNyxBytes;
  LAt: Integer;
begin
  LBytes := NyxEncodeUTF8(AText);
  SetLength(Result, Length(LBytes));
  for LAt := 0 to High(LBytes) do
  begin
    Result[LAt + 1] := AnsiChar(LBytes[LAt]);
  end;
end;

function FromHost(const AText: String): TNyxText;
var
  LBytes: TNyxBytes;
  LAt: Integer;
begin
  SetLength(LBytes, Length(AText));
  for LAt := 0 to High(LBytes) do
  begin
    LBytes[LAt] := Byte(AText[LAt + 1]);
  end;
  Result := NyxDecodeUTF8(LBytes);
end;

function SourceName(const ASources: INyxPascalSourceSnapshot;
  const AFile: TNyxPascalSourceRef): String;
begin
  Result := ToHost(AFile.Name);

  if AFile.Role = npsMain then
  begin
    Result := ToHost(ASources.MainUnit.Name) + '.pas';
  end;
end;

{ Native byte boundaries of one full Pascal identifier, including its untyped
  suffix after the caret. UTF-8 continuation bytes stay part of their identifier;
  the validated source/caret conversion guarantees complete scalar boundaries. }
function IdentifierByte(AChar: AnsiChar): Boolean;
begin
  Result := AChar in ['a'..'z', 'A'..'Z', '0'..'9', '_'];

  if Byte(AChar) >= 128 then
  begin
    Result := True;
  end;
end;

function KindOf(AItem: TIdentifierListItem): TNyxPascalCompletionKind;
begin
  Result := pckUnknown;
  case AItem.GetDesc of
    ctnProcedure, ctnProcedureHead: Result := pckMethod;
    ctnProperty: Result := pckProperty;
    ctnTypeDefinition: Result := pckType;
    ctnConstDefinition: Result := pckConstant;
    ctnVarDefinition: Result := pckVariable;
    ctnUnit: Result := pckUnit;
  end;
end;

function Complete(const ARequest: INyxPascalCompletionRequest;
  const ASDK, ACompiler, AFPCSource, ALazarus, ADirectory: String): INyxPascalCompletionResult;
var
  LManager: TCodeToolManager;
  LOptions: TCodeToolsOptions;
  LCode: TCodeBuffer;
  LOwnerCode: TCodeBuffer;
  LBuffer: TCodeBuffer;
  LSource: TNyxText;
  LHost: String;
  LAt: Integer;
  LByteCaret: Integer;
  LScalar: Integer;
  LStart: Integer;
  LFinish: Integer;
  LReplace: Integer;
  LX: Integer;
  LY: Integer;
  LTotal: Integer;
  LCount: Integer;
  LItem: TIdentifierListItem;
  LItems: array of TNyxPascalCompletionItem;
  LRef: TNyxPascalSourceRef;
  LDetail: TNyxText;
  LExpected: TNyxText;
  LRoot: String;
begin
  LManager := CodeToolBoss;
  LOptions := TCodeToolsOptions.Create;
  try
    LRoot := IncludeTrailingPathDelimiter(ADirectory);
    LOptions.FPCPath := ACompiler;
    LOptions.FPCSrcDir := AFPCSource;
    LOptions.LazarusSrcDir := ALazarus;
    LOptions.ProjectDir := LRoot;
    LOptions.TargetOS := 'win32';
    LOptions.TargetProcessor := 'i386';
    LOptions.LCLWidgetType := 'win32';
    LOptions.FPCOptions := '-Mdelphi -Fu"' + IncludeTrailingPathDelimiter(ASDK) +
      'src" -Fi"' + IncludeTrailingPathDelimiter(ASDK) + 'src" -Fu"' + LRoot + '" -Fi"' + LRoot + '"';
    LOptions.TestPascalFile := LRoot + 'context-probe.pas';
    WriteText(LOptions.TestPascalFile, '');
    { CodeTools discovers include/unit membership through its directory cache.
      Materialize only the validated captured logical leaves in this invocation's
      owned directory before initializing that cache; no user file is touched. }
    for LAt := 0 to ARequest.Sources.Count - 1 do
    begin
      LRef := ARequest.Sources.FileReference(LAt);
      WriteText(LRoot + SourceName(ARequest.Sources, LRef), ARequest.Sources.Text(LRef));
    end;
    LManager.Init(LOptions, [ctopSetVariables, ctopScanFPCSrc, ctopAddDefines]);
    LManager.CheckFilesOnDisk := False;
    LCode := nil;
    LOwnerCode := nil;
    for LAt := 0 to ARequest.Sources.Count - 1 do
    begin
      LRef := ARequest.Sources.FileReference(LAt);
      LBuffer := LManager.CreateFile(LRoot + SourceName(ARequest.Sources, LRef));
      LBuffer.Source := ToHost(ARequest.Sources.Text(LRef));

      if LRef.SameFile(ARequest.FileReference) then
      begin
        LCode := LBuffer;
      end;

      if LRef.SameFile(ARequest.Owner) then
      begin
        LOwnerCode := LBuffer;
      end;
    end;

    if (LCode = nil) or (LOwnerCode = nil) then
    begin
      raise Exception.Create('Completion files are not captured');
    end;
    LSource := ARequest.Sources.Text(ARequest.FileReference);
    LHost := ToHost(LSource);
    LByteCaret := 1;
    for LAt := 1 to ARequest.Caret do
    begin
      NyxNextScalar(LSource, LByteCaret, LScalar);
    end;
    LStart := LByteCaret;
    while (LStart > 1) and IdentifierByte(LHost[LStart - 1]) do
    begin
      Dec(LStart);
    end;
    LFinish := LByteCaret;
    while (LFinish <= Length(LHost)) and IdentifierByte(LHost[LFinish]) do
    begin
      Inc(LFinish);
    end;
    LReplace := NyxTextScalarCount(Copy(LSource, 1, LStart - 1));
    LExpected := Copy(LSource, LStart, LFinish - LStart);

    if ARequest.FileReference.Role = npsInclude then
    begin
      LCode.LastIncludedByFile := LOwnerCode.Filename;
      LManager.InitCurCodeTool(LOwnerCode);
    end;
    LCode.AbsoluteToLineCol(LByteCaret, LY, LX);

    if not LManager.GatherIdentifiers(LCode, LX, LY) then
    begin
      WriteLn(StdErr, LManager.ErrorMessage);
      Exit(NewNyxPascalCompletionResult(ARequest, pcsFailed,
        'The assistance engine could not resolve this source context', 0, '', [], 0));
    end;
    LManager.IdentifierList.Prefix := Copy(LHost, LStart, LByteCaret - LStart);
    LTotal := LManager.IdentifierList.GetFilteredCount;
    LCount := LTotal - ARequest.Offset;

    if LCount < 0 then
    begin
      LCount := 0;
    end;

    if LCount > ARequest.Limit then
    begin
      LCount := ARequest.Limit;
    end;
    SetLength(LItems, LCount);
    for LAt := 0 to LCount - 1 do
    begin
      LItem := LManager.IdentifierList.FilteredItems[ARequest.Offset + LAt];
      LDetail := FromHost(LItem.Identifier);

      if LItem.GetDesc = ctnProcedure then
      begin
        LDetail := FromHost(LItem.Tool.ExtractProcHead(LItem.Node,
          [phpWithoutClassName, phpWithStart, phpWithVarModifiers,
          phpWithParameterNames, phpWithResultType, phpWithDefaultValues,
          phpWithCallingSpecs]));
      end;
      LItems[LAt] := NyxPascalCompletion(FromHost(LItem.Identifier),
        FromHost(LItem.Identifier), LDetail, KindOf(LItem));
    end;
    Result := NewNyxPascalCompletionResult(ARequest, pcsReady, '',
      LReplace, LExpected, LItems, LTotal);
  finally
    LOptions.Free;
  end;
end;

var
  LRequest: INyxPascalCompletionRequest;
  LResult: INyxPascalCompletionResult;
  LID: TNyxText;
begin
  try

    if ParamCount <> 7 then
    begin
      raise Exception.Create('Expected request/reply, owned project directory and private SDK/compiler/source paths');
    end;
    LRequest := DecodeNyxCompletionRequest(ReadText(ParamStr(1), 4 * 1024 * 1024), LID);
    try
      LResult := Complete(LRequest, ParamStr(4), ParamStr(5), ParamStr(6),
        ParamStr(7), ParamStr(3));
    except
      on E: Exception do
      begin
        { Detailed private platform errors never become semantic host paths. }
        WriteLn(StdErr, E.ClassName, ': ', E.Message);
        LResult := NewNyxPascalCompletionResult(LRequest, pcsFailed,
          'The assistance engine could not complete this request', 0, '', [], 0);
      end;
    end;
    WriteText(ParamStr(2), EncodeNyxCompletionReply(LResult, LID));
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'Completion worker could not read or produce its bounded private packet');
      ExitCode := 1;
    end;
  end;
end.
