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
unit nyx.pascal.compilertrace;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.context;

type
  { Immutable source-context query. Generated input has one selected MODE and
    no application/library source, includes or execution. Requested symbol names
    are validated Pascal define references. Duplicate requests retain order.
    Mode/FPC identity guards are always queried explicitly. The caller owns the
    actual compiler/configuration and must compile these exact bytes in an
    independent job before passing its terminal transcript. }
  INyxPascalCompilerProbe = interface(IInterface)
    ['{814416FA-B8F7-45DC-B6AC-83D29D238901}']
    function Identity: TNyxPascalDefineRef;
    function Mode: TNyxPascalLanguageMode;
    function Count: Integer;
    function Symbol(AIndex: Integer): TNyxPascalDefineRef;
    function WithSymbol(const AName: TNyxPascalDefineRef): INyxPascalCompilerProbe;
    function Source: TNyxText;
  end;
  { Copied input/work limits. Diagnostics are external input, not trusted facts.
    Defaults admit 4 MiB UTF-8, 65536 lines and 16384 UTF-8 bytes per
    line; no limit truncates an accepted compiler context. }
  TNyxPascalCompilerTraceOptions = record
  private
    FDefined: Boolean;
    FBytes: Integer;
    FLines: Integer;
    FLine: Integer;
  public
    class function Defaults: TNyxPascalCompilerTraceOptions; static;
    function UpToBytes(AValue: Integer): TNyxPascalCompilerTraceOptions;
    function UpToLines(AValue: Integer): TNyxPascalCompilerTraceOptions;
    function UpToLineLength(AValue: Integer): TNyxPascalCompilerTraceOptions;
    procedure Validate;
    property MaximumBytes: Integer read FBytes;
  end;

{ Only FPC/ObjFPC/Delphi/DelphiUnicode/TP source modes have qualified defaults.
  Identity correlates explicit probe messages; it grants no process authority.
  At most 256 explicit name queries keep generated source/row work bounded. }
function NewNyxFPCCompilerProbe(const AIdentity: TNyxPascalDefineRef;
  AMode: TNyxPascalLanguageMode = ppmDelphi): INyxPascalCompilerProbe;

{ Interpret a terminal successful compiler transcript for the exact probe file.
  The host must supply -vc -vi -vn -vq, English numbered messages and the exact source
  bytes. Initial macro define/undef/set events supply observed facts; explicit
  source IFDEF/note replies establish only requested absences. The define set
  remains open. Four consistent integer version macros and FPC identity, ordered
  unique begin/rows/H/end replies and the source's compilation marker are required.
  Malformed, incomplete, foreign, failed, excess or cancelled input publishes no
  partial facts. This parser launches/reads/writes no process or filesystem. }
function ReadNyxFPCCompilerContext(const AProbe: INyxPascalCompilerProbe;
  const AProbeFile, ATranscript: TNyxText; AExitCode: Integer;
  const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalCompilerContext;

implementation

uses SysUtils, nyx.bytes, nyx.pascal.lexing;

type
  { Owns a detached query vector. Source is rebuilt from checked closed mode
    spellings and ASCII names; caller text cannot inject compiler directives. }
  TCompilerProbe = class(TInterfacedObject, INyxPascalCompilerProbe)
  public
    ProbeID: TNyxPascalDefineRef;
    SourceMode: TNyxPascalLanguageMode;
    Names: array of TNyxPascalDefineRef;
    function Identity: TNyxPascalDefineRef;
    function Mode: TNyxPascalLanguageMode;
    function Count: Integer;
    function Symbol(AIndex: Integer): TNyxPascalDefineRef;
    function WithSymbol(const AName: TNyxPascalDefineRef): INyxPascalCompilerProbe;
    function Source: TNyxText;
  end;
  { Synchronous transcript owner. Context candidates and progress state stay
    private until every row/version/terminal check succeeds. Cancellation and
    the supplied request are borrowed only for this construction. }
  TTraceReader = class
  private
    FProbe: INyxPascalCompilerProbe;
    FContext: INyxPascalCompilerContext;
    FOptions: TNyxPascalCompilerTraceOptions;
    FCancellation: INyxCancellationScope;
    FFile: TNyxText;
    FPrefix: TNyxText;
    FStarted: Boolean;
    FEnded: Boolean;
    FCompiled: Boolean;
    FCompleted: Boolean;
    FOption: Boolean;
    FNext: Integer;
    procedure Check;
    procedure Line(const ALine: TNyxText);
    procedure Reply(const AReply: TNyxText);
    procedure Macro(const ABody: TNyxText; ACode: Integer);
  public
    function Read(const AProbe: INyxPascalCompilerProbe;
      const AFile, AText: TNyxText; AExitCode: Integer;
      const AOptions: TNyxPascalCompilerTraceOptions;
      const ACancellation: INyxCancellationScope): INyxPascalCompilerContext;
  end;

function ModeName(AMode: TNyxPascalLanguageMode): TNyxText;
begin
  case AMode of
    ppmFPC:
    begin
      Result := 'fpc';
    end;
    ppmObjFPC:
    begin
      Result := 'objfpc';
    end;
    ppmDelphi:
    begin
      Result := 'delphi';
    end;
    ppmDelphiUnicode:
    begin
      Result := 'delphiunicode';
    end;
    ppmTP:
    begin
      Result := 'tp';
    end;
  else
    raise ENyxPascalContext.Create('This compiler probe needs a qualified FPC source mode');
  end;
end;

function TCompilerProbe.Identity: TNyxPascalDefineRef;
begin
  Result := ProbeID;
end;

function TCompilerProbe.Mode: TNyxPascalLanguageMode;
begin
  Result := SourceMode;
end;

function TCompilerProbe.Count: Integer;
begin
  Result := Length(Names);
end;

function TCompilerProbe.Symbol(AIndex: Integer): TNyxPascalDefineRef;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalContext.Create('Compiler probe name is outside its query vector');
  end;
  Result := Names[AIndex];
end;

function TCompilerProbe.WithSymbol(const AName: TNyxPascalDefineRef): INyxPascalCompilerProbe;
var
  LCopy: TCompilerProbe;
  LAt: Integer;
  LName: TNyxPascalDefineRef;
begin
  LName := NyxPascalDefine(AName.Name);
  for LAt := 0 to Count - 1 do
  begin

    if Names[LAt].Name = LName.Name then
    begin
      Exit(Self);
    end;
  end;

  if Count >= 256 then
  begin
    raise ENyxPascalContext.Create('Compiler probe queries exceed their bounded name vector');
  end;
  LCopy := TCompilerProbe.Create;
  Result := LCopy;
  LCopy.ProbeID := ProbeID;
  LCopy.SourceMode := SourceMode;
  SetLength(LCopy.Names, Count + 1);
  for LAt := 0 to Count - 1 do
  begin
    LCopy.Names[LAt] := Names[LAt];
  end;
  LCopy.Names[Count] := LName;
end;

function TCompilerProbe.Source: TNyxText;
var
  LAt: Integer;
  LPrefix: TNyxText;
begin
  LPrefix := 'NYX_CONTEXT_' + ProbeID.Name + '_';
  Result := 'program NyxCompilerContext;'#10'{$mode ' + ModeName(SourceMode) +
    '}{$codepage utf8}'#10 +
    '{ This declaration forces implicit System setup before fact queries. }'#10 +
    'type TNyxContextMarker = Byte;'#10'{$note ' + LPrefix + 'BEGIN}'#10;
  for LAt := 0 to Count - 1 do
  begin
    Result := Result + '{$ifdef ' + Names[LAt].Name + '}'#10'{$note ' + LPrefix +
      'D_' + TNyxText(IntToStr(LAt)) + '}'#10'{$else}'#10'{$note ' + LPrefix +
      'U_' + TNyxText(IntToStr(LAt)) + '}'#10'{$endif}'#10;
  end;
  Result := Result + '{$ifopt H+}'#10'{$note ' + LPrefix + 'H_TRUE}'#10 +
    '{$else}'#10'{$note ' + LPrefix + 'H_FALSE}'#10'{$endif}'#10 +
    '{$note ' + LPrefix + 'END}'#10'begin'#10'end.'#10;
end;

function NewNyxFPCCompilerProbe(const AIdentity: TNyxPascalDefineRef;
  AMode: TNyxPascalLanguageMode): INyxPascalCompilerProbe;
const
  CGuards: array[0..7] of TNyxText = ('FPC', 'FPC_DELPHI', 'FPC_OBJFPC', 'FPC_TP',
    'FPC_MACPAS', 'FPC_ISO', 'FPC_EXTENDEDPASCAL', 'FPC_UNICODESTRINGS');
var
  LProbe: TCompilerProbe;
  LAt: Integer;
begin
  ModeName(AMode);
  LProbe := TCompilerProbe.Create;
  Result := LProbe;
  LProbe.ProbeID := NyxPascalDefine(AIdentity.Name);
  LProbe.SourceMode := AMode;
  SetLength(LProbe.Names, Length(CGuards));
  for LAt := 0 to High(CGuards) do
  begin
    LProbe.Names[LAt] := NyxPascalDefine(CGuards[LAt]);
  end;
end;

class function TNyxPascalCompilerTraceOptions.Defaults: TNyxPascalCompilerTraceOptions;
begin
  Result.FDefined := True;
  Result.FBytes := 4 * 1024 * 1024;
  Result.FLines := 65536;
  Result.FLine := 16384;
end;

function TNyxPascalCompilerTraceOptions.UpToBytes(AValue: Integer): TNyxPascalCompilerTraceOptions;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerTraceOptions.UpToLines(AValue: Integer): TNyxPascalCompilerTraceOptions;
begin
  Result := Self;
  Result.FLines := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerTraceOptions.UpToLineLength(AValue: Integer): TNyxPascalCompilerTraceOptions;
begin
  Result := Self;
  Result.FLine := AValue;
  Result.Validate;
end;

procedure TNyxPascalCompilerTraceOptions.Validate;
begin

  if not FDefined or (FBytes < 1) or (FLines < 1) or (FLine < 1) then
  begin
    raise ENyxPascalContext.Create('Compiler trace budgets must be explicitly positive');
  end;
end;

procedure TTraceReader.Check;
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Compiler fact capture cancelled');
  end;
end;

function IntegerText(const AText: TNyxText; out AValue: Integer): Boolean;
var
  LAt: Integer;
  LDigit: Integer;
  LValue: Integer;
  LBound: Integer;
  LNegative: Boolean;
begin
  Result := False;
  AValue := 0;
  LAt := 1;
  LNegative := (Length(AText) > 0) and (AText[1] = '-');

  if LNegative then
  begin
    Inc(LAt);
  end;

  if LAt > Length(AText) then
  begin
    Exit;
  end;
  { Accumulate negatively so Low(Integer) is representable on both targets.
    Every multiply/subtract is guarded before it can overflow; no native Int64
    or JavaScript-dependent integer conversion supplies different semantics. }
  LBound := -High(Integer);

  if LNegative then
  begin
    LBound := Low(Integer);
  end;
  LValue := 0;
  while LAt <= Length(AText) do
  begin

    if not (AText[LAt] in ['0'..'9']) then
    begin
      Exit;
    end;
    LDigit := Ord(AText[LAt]) - Ord('0');

    if (LValue < LBound div 10) or
      ((LValue = LBound div 10) and (LDigit > -(LBound mod 10))) then
    begin
      Exit;
    end;
    LValue := LValue * 10 - LDigit;
    Inc(LAt);
  end;

  if not LNegative then
  begin
    LValue := -LValue;
  end;
  AValue := LValue;
  Result := True;
end;

procedure TTraceReader.Macro(const ABody: TNyxText; ACode: Integer);
var
  LName: TNyxPascalDefineRef;
  LAt: Integer;
  LValue: Integer;
  LInteger: TNyxText;
begin

  if FEnded then
  begin
    Exit;
  end;

  if FStarted then
  begin
    raise ENyxPascalContext.Create('A compiler fact changed inside the fixed query block');
  end;
  case ACode of
    3101:
    begin
      LName := NyxPascalDefine(ABody);
      FContext := FContext.WithSymbol(LName, ppdDefined);
    end;
    3102:
    begin
      LName := NyxPascalDefine(ABody);
      FContext := FContext.WithSymbol(LName, ppdUndefined);
    end;
    3103:
    begin
      LAt := Pos(' set to ', ABody);

      if LAt < 2 then
      begin
        raise ENyxPascalContext.Create('A compiler macro value lacks its exact separator');
      end;
      LName := NyxPascalDefine(Copy(ABody, 1, LAt - 1));
      LInteger := Copy(ABody, LAt + 8, Length(ABody));

      if IntegerText(LInteger, LValue) then
      begin
        FContext := FContext.WithSymbol(LName, ppdInteger, LValue);
      end
      else
      begin
        { A replacement/string/large integer is defined, but has no invented
          signed-32 value. Conditional arithmetic therefore remains unresolved. }
        FContext := FContext.WithSymbol(LName, ppdDefined);
      end;
    end;
  end;
end;

procedure TTraceReader.Reply(const AReply: TNyxText);
var
  LName: TNyxPascalDefineRef;
  LExpected: TNyxText;
begin

  if AReply = 'BEGIN' then
  begin

    if FStarted or FEnded or not FCompiled then
    begin
      raise ENyxPascalContext.Create('Compiler query start is duplicated or out of order');
    end;
    FStarted := True;
    Exit;
  end;

  if not FStarted or FEnded then
  begin
    raise ENyxPascalContext.Create('Compiler query row is outside its active block');
  end;

  if FNext < FProbe.Count then
  begin
    LExpected := TNyxText(IntToStr(FNext));

    if (AReply <> 'D_' + LExpected) and (AReply <> 'U_' + LExpected) then
    begin
      raise ENyxPascalContext.Create('Compiler query row is missing, repeated or out of order');
    end;
    LName := FProbe.Symbol(FNext);

    if AReply[1] = 'U' then
    begin
      FContext := FContext.WithSymbol(LName, ppdUndefined);
    end
    else if FContext.Symbol(LName).IsDefined <> ppkTrue then
    begin
      FContext := FContext.WithSymbol(LName, ppdDefined);
    end;
    Inc(FNext);
    Exit;
  end;

  if not FOption then
  begin

    if AReply = 'H_TRUE' then
    begin
      FContext := FContext.WithOption('H', ppkTrue);
    end
    else if AReply = 'H_FALSE' then
    begin
      FContext := FContext.WithOption('H', ppkFalse);
    end
    else
    begin
      raise ENyxPascalContext.Create('The compiler query lacks its unique H-option reply');
    end;
    FOption := True;
    Exit;
  end;

  if AReply <> 'END' then
  begin
    raise ENyxPascalContext.Create('The compiler query lacks its unique end reply');
  end;
  FEnded := True;
end;

procedure TTraceReader.Line(const ALine: TNyxText);
var
  LAt: Integer;
  LMacro: Integer;
  LBody: TNyxText;
begin
  Check;

  if (Pos('Error: (', ALine) > 0) or (Pos('Fatal: (', ALine) > 0) then
  begin
    raise ENyxPascalContext.Create('A failed compiler message cannot supply context');
  end;

  if (Copy(ALine, 1, 7) = '(1008) ') and (Pos(' lines compiled', ALine) > 7) then
  begin

    if not FEnded or FCompleted then
    begin
      raise ENyxPascalContext.Create('Compiler completion is duplicated or precedes query completion');
    end;
    FCompleted := True;
    Exit;
  end;

  if ALine = '(3104) Compiling ' + FFile then
  begin

    if FCompiled then
    begin
      raise ENyxPascalContext.Create('Compiler probe source was compiled more than once');
    end;
    FCompiled := True;
    Exit;
  end;

  if (Pos('(3104) Compiling ', ALine) > 0) then
  begin
    raise ENyxPascalContext.Create('A dependency source compilation cannot supply probe context');
  end;
  LMacro := Pos('(3101) Macro defined: ', ALine);

  if LMacro > 0 then
  begin
    Macro(Copy(ALine, LMacro + 22, Length(ALine)), 3101);
  end
  else if Pos('(3102) Macro undefined: ', ALine) > 0 then
  begin
    LMacro := Pos('(3102) Macro undefined: ', ALine);
    Macro(Copy(ALine, LMacro + 24, Length(ALine)), 3102);
  end
  else if Pos('(3103) Macro ', ALine) > 0 then
  begin
    LMacro := Pos('(3103) Macro ', ALine);
    Macro(Copy(ALine, LMacro + 13, Length(ALine)), 3103);
  end;
  LAt := Pos(FPrefix, ALine);

  if LAt = 0 then
  begin
    Exit;
  end;
  { Root messages must have this file and a source location. Compiler echoes of
    skipped directives or foreign dependencies cannot supply a note reply. }

  if (Copy(ALine, 1, Length(FFile) + 1) <> FFile + '(') or
    (Pos('Note: (2025) User defined: ', Copy(ALine, Length(FFile) + 2, LAt)) = 0) then
  begin
    raise ENyxPascalContext.Create('A compiler query reply has a foreign source/message owner');
  end;
  LBody := Copy(ALine, LAt + Length(FPrefix), Length(ALine));
  Reply(LBody);
end;

function TTraceReader.Read(const AProbe: INyxPascalCompilerProbe;
  const AFile, AText: TNyxText; AExitCode: Integer;
  const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope): INyxPascalCompilerContext;
var
  LAt: Integer;
  LStart: Integer;
  LLines: Integer;
  LVersion: TNyxPascalCompilerVersion;
  LInteger: Integer;
  LName: TNyxPascalDefineRef;
  LQueryCount: Integer;
begin
  AOptions.Validate;
  FOptions := AOptions;
  FCancellation := ACancellation;
  Check;

  if (AProbe = nil) or (AExitCode <> 0) or (AFile = '') or
    (Pos(#10, AFile) > 0) or (Pos(#13, AFile) > 0) then
  begin
    raise ENyxPascalContext.Create('Compiler facts require an explicit successful owned probe');
  end;

  if NyxUTF8ByteCount(AText) > FOptions.MaximumBytes then
  begin
    raise ENyxPascalContext.Create('Compiler transcript exceeds its byte budget');
  end;
  { Capture alternate requests through checked values rather than retaining a
    mutable query producer. The generated source remains a host job invariant. }
  LQueryCount := AProbe.Count;

  if (LQueryCount < 0) or (LQueryCount > 256) then
  begin
    raise ENyxPascalContext.Create('Compiler probe queries exceed their bounded vector');
  end;
  FProbe := NewNyxFPCCompilerProbe(AProbe.Identity, AProbe.Mode);
  for LAt := 0 to LQueryCount - 1 do
  begin
    FProbe := FProbe.WithSymbol(AProbe.Symbol(LAt));
  end;

  if (FProbe.Count <> LQueryCount) or (FProbe.Source <> AProbe.Source) then
  begin
    raise ENyxPascalContext.Create('An alternate compiler probe changed its checked source/query');
  end;
  FFile := AFile;
  FPrefix := 'NYX_CONTEXT_' + FProbe.Identity.Name + '_';
  FContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(0, 0, 0), FProbe.Mode);
  LStart := 1;
  LAt := 1;
  LLines := 0;
  while LAt <= Length(AText) + 1 do
  begin
    Check;

    if (LAt > Length(AText)) or (AText[LAt] in [#10, #13]) then
    begin
      Inc(LLines);

      if (LLines > FOptions.FLines) or (LAt - LStart > FOptions.FLine) or
        (NyxUTF8ByteCount(Copy(AText, LStart, LAt - LStart)) > FOptions.FLine) then
      begin
        raise ENyxPascalContext.Create('Compiler transcript exceeds its line budget');
      end;
      Line(Copy(AText, LStart, LAt - LStart));

      if (LAt < Length(AText)) and (AText[LAt] = #13) and (AText[LAt + 1] = #10) then
      begin
        Inc(LAt);
      end;
      LStart := LAt + 1;
    end;
    Inc(LAt);
  end;

  if not FCompiled or not FStarted or not FEnded or not FCompleted or not FOption or
    (FNext <> FProbe.Count) or
    (FContext.Symbol(NyxPascalDefine('FPC')).IsDefined <> ppkTrue) then
  begin
    raise ENyxPascalContext.Create('Compiler facts are incomplete or lack actual FPC identity');
  end;
  LName := NyxPascalDefine('FPC_FULLVERSION');

  if FContext.Symbol(LName).Kind <> ppdInteger then
  begin
    raise ENyxPascalContext.Create('The compiler did not report its numeric version');
  end;
  LInteger := FContext.Symbol(LName).IntegerValue;

  if LInteger < 10000 then
  begin
    raise ENyxPascalContext.Create('Compiler version must identify an actual supported release');
  end;
  LVersion := TNyxPascalCompilerVersion.Create(LInteger div 10000,
    (LInteger div 100) mod 100, LInteger mod 100);

  if (FContext.Symbol(NyxPascalDefine('FPC_VERSION')).Kind <> ppdInteger) or
    (FContext.Symbol(NyxPascalDefine('FPC_RELEASE')).Kind <> ppdInteger) or
    (FContext.Symbol(NyxPascalDefine('FPC_PATCH')).Kind <> ppdInteger) or
    (FContext.Symbol(NyxPascalDefine('FPC_VERSION')).IntegerValue <> LVersion.Major) or
    (FContext.Symbol(NyxPascalDefine('FPC_RELEASE')).IntegerValue <> LVersion.Minor) or
    (FContext.Symbol(NyxPascalDefine('FPC_PATCH')).IntegerValue <> LVersion.Patch) then
  begin
    raise ENyxPascalContext.Create('Compiler version macros disagree');
  end;
  LName := NyxPascalDefine('FPC_DELPHI');

  if FProbe.Mode = ppmObjFPC then
  begin
    LName := NyxPascalDefine('FPC_OBJFPC');
  end
  else if FProbe.Mode = ppmTP then
  begin
    LName := NyxPascalDefine('FPC_TP');
  end;

  if (FProbe.Mode <> ppmFPC) and (FContext.Symbol(LName).IsDefined <> ppkTrue) then
  begin
    raise ENyxPascalContext.Create('Compiler source-mode identity differs from the probe');
  end;

  if (FContext.Symbol(NyxPascalDefine('FPC_UNICODESTRINGS')).IsDefined = ppkTrue) <>
    (FProbe.Mode = ppmDelphiUnicode) then
  begin
    raise ENyxPascalContext.Create('Compiler Unicode defaults differ from the selected source mode');
  end;
  Result := NewNyxPascalCompilerContext(ppcFPC, LVersion, FProbe.Mode);
  for LAt := 0 to FContext.SymbolCount - 1 do
  begin
    LName := FContext.SymbolName(LAt);
    LInteger := 0;

    if FContext.Symbol(LName).Kind = ppdInteger then
    begin
      LInteger := FContext.Symbol(LName).IntegerValue;
    end;
    Result := Result.WithSymbol(LName, FContext.Symbol(LName).Kind, LInteger);
  end;
  Result := Result.WithOption('H', FContext.Option('H'));
  { Only mode defaults already independently compiler-qualified are restored.
    Neither version capability macros nor this analyzer's build enables closures. }
  Result := Result.WithFeature(ppfNestedComments, ppkFalse)
    .WithFeature(ppfUnicodeStrings, ppkFalse).WithFeature(ppfOutParameters, ppkFalse);

  if FProbe.Mode in [ppmFPC, ppmObjFPC] then
  begin
    Result := Result.WithFeature(ppfNestedComments, ppkTrue);
  end;

  if FProbe.Mode = ppmDelphiUnicode then
  begin
    Result := Result.WithFeature(ppfUnicodeStrings, ppkTrue);
  end;

  if FProbe.Mode in [ppmObjFPC, ppmDelphi, ppmDelphiUnicode] then
  begin
    Result := Result.WithFeature(ppfOutParameters, ppkTrue);
  end;
  Check;
end;

function ReadNyxFPCCompilerContext(const AProbe: INyxPascalCompilerProbe;
  const AProbeFile, ATranscript: TNyxText; AExitCode: Integer;
  const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope): INyxPascalCompilerContext;
var
  LReader: TTraceReader;
begin
  LReader := TTraceReader.Create;
  try
    Result := LReader.Read(AProbe, AProbeFile, ATranscript, AExitCode, AOptions, ACancellation);
  finally
    LReader.Free;
  end;
end;

end.
