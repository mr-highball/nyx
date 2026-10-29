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

program nyx_file_runtime_build;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, fpjson, nyx.text, nyx.bytes, nyx.data, nyx.model,
  nyx.codegen, nyx.codec, nyx.studio.outputs, nyx.studio.directories,
  nyx.studio.buildexecutor, nyx.studio.projects, nyx.test.files.runtime,
  nyx.test.flow.conditionfixture;

var
  LRoot: String;
  LTools: TNyxDataValue;
  LOutputs: TNyxOutputConfiguration;
  LExecutor: TNyxBuildExecutor;
  LDoc: TNyxDocument;
  LReply: TJSONObject;
  LTarget: TNyxText;
  LSource: TNyxText;
  LBefore: TNyxText;
  LJob: TNyxText;
  LIndex: Integer;
  LPass: Integer;
  LOpening: Boolean;
  LHTTP: Boolean;
  LTables: Boolean;
  LLastPass: Integer;
  LPair: TNyxProjectPair;
  LPolicy: TNyxText;
  LLauncher: TNyxText;

function ReadText(const APath: String): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead);
  try
    SetLength(LBytes, LStream.Size);

    if Length(LBytes) > 0 then
    begin
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

procedure Save(const APath: String; const AText: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
end;

begin
  LOutputs := nil;
  LExecutor := nil;
  LDoc := nil;
  try

    if not (ParamCount in [3, 4, 5]) or
      ((ParamCount = 4) and (ParamStr(4) <> '--opening')) or
      ((ParamCount = 5) and (ParamStr(4) <> '--http') and (ParamStr(4) <> '--tables')) then
    begin
      raise Exception.Create('Supply repository, fresh root, toolchain and optional --opening, --http <project> or --tables <project>');
    end;
    LRoot := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));

    if DirectoryExists(LRoot) then
    begin
      raise Exception.Create('Compiler fixture requires a fresh owned runtime root');
    end;
    ForceDirectories(LRoot);
    LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(3)));
    LOutputs := TNyxOutputConfiguration.Create;
    LOutputs.SetField('fpc', LTools.Field('LCL_FPC').AsText);
    LOutputs.SetField('lazarus', LTools.Field('LAZARUS').AsText);
    LOutputs.SetField('platform', 'i386-win32');
    LOutputs.SetField('widgetset', 'win32');
    LOutputs.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LOutputs.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LOpening := (ParamCount = 4) and (ParamStr(4) = '--opening');
    LHTTP := (ParamCount = 5) and (ParamStr(4) = '--http');
    LTables := (ParamCount = 5) and (ParamStr(4) = '--tables');

    if LHTTP or LTables then
    begin
      LPair := DecodeNyxProject(ReadText(ParamStr(5)));
      LDoc := TNyxCodec.Decode(LPair.Design);
      LSource := LPair.Source;

      if LPair.Pending or not LPair.Files.IsEmpty then
      begin
        raise Exception.Create('Runtime qualification requires the exact accepted standalone project pair');
      end;
    end
    else if LOpening then
    begin
      LDoc := ConditionalFileDocument;
    end
    else
    begin
      LDoc := FileRuntimeDocument;
      LOutputs.AllowFileSaving;
    end;
    LBefore := TNyxCodec.Encode(LDoc);

    if not LHTTP and not LTables then
    begin
      LSource := TNyxCodegen.Generate(LDoc);
    end;
    Save(LRoot + 'nyx.generated.view.pas', LSource);
    LLastPass := 2;

    if LTables then
    begin
      LLastPass := 3;
    end;
    for LPass := 0 to LLastPass do
    begin
      if not LOpening and not LHTTP and not LTables and (LPass > 0) then
      begin
        Break;
      end;

      LPolicy := '';

      if LTables then
      begin
        LOutputs.AllowClipboardWriting((LPass and 1) <> 0)
          .AllowFileSaving((LPass and 2) <> 0);
        LPolicy := '-tables-' + TNyxText(IntToStr(LPass));
      end;

      if LHTTP then
      begin
        LOutputs.AllowHTTP(LPass > 0).AllowFileOpening(LPass = 2).AllowFileSaving(LPass = 2);
        LPolicy := '-http-' + TNyxText(IntToStr(LPass));
      end;

      if LOpening then
      begin
        LOutputs.AllowFileOpening(LPass > 0).AllowFileSaving(LPass = 2);
        case LPass of
          0:
            begin
              LPolicy := '-disabled';
            end;
          1:
            begin
              LPolicy := '-open';
            end;
          2:
            begin
              LPolicy := '-both';
            end;
        end;
      end;
      FreeAndNil(LExecutor);
      LExecutor := TNyxBuildExecutor.Create(
        TNyxStudioDirectories.ForRepository(TNyxText(ExpandFileName(ParamStr(1)))).RunningIn(TNyxText(LRoot)),
        LOutputs.Encode);
      for LIndex := 0 to 1 do
      begin

        if LIndex = 0 then
        begin
          LTarget := 'browser';
        end
        else
        begin
          LTarget := 'lcl';
        end;
        LReply := LExecutor.Build(LDoc, LTarget, 'application', 'home', LSource);
        try
          Save(LRoot + String(LTarget + LPolicy) + '-result-private.json', TNyxText(LReply.AsJSON));

          if not LReply.Get('ok', False) then
          begin
            raise Exception.Create('Actual compiler failed for ' + String(LTarget) + '; inspect retained result');
          end;
          LJob := LReply.Get('build', '');
          Save(LRoot + String(LTarget + LPolicy) + '-job.txt', LJob);

          if LOpening or LHTTP or LTables then
          begin

            if LTarget = 'browser' then
            begin
              LLauncher := 'nyx_preview.lpr';
            end
            else
            begin
              LLauncher := 'nyx_native.lpr';
            end;
            LLauncher := ReadText(LRoot + 'build/studio/jobs/' + String(LJob) + '/' + String(LLauncher));

            if ((Pos('ConfigureFileOpening(', LLauncher) > 0) <> LOutputs.FileOpening) or
              ((Pos('ConfigureFileSaving(', LLauncher) > 0) <> LOutputs.FileSaving) or
              ((Pos('ConfigureLogic(', LLauncher) > 0) <> LOutputs.HTTP) then
            begin
              raise Exception.Create('Generated launcher changed independent runtime permissions');
            end;

            if LTables and
              (((Pos('ConfigureTableWorkbench(', LLauncher) > 0) <>
                (LOutputs.ClipboardWriting or LOutputs.FileSaving)) or
              ((Pos('ClipboardWriteHost', LLauncher) > 0) <> LOutputs.ClipboardWriting)) then
            begin
              raise Exception.Create('Generated table services changed independent host choices');
            end;
          end;
        finally
          LReply.Free;
        end;

        if TNyxCodec.Encode(LDoc) <> LBefore then
        begin
          raise Exception.Create('Runtime configuration changed the portable design');
        end;
      end;
    end;

    if LTables then
    begin
      WriteLn('PASS eight real Service monitor compiler jobs; disabled/copy/save/both; exact accepted source/design');
    end
    else if LHTTP then
    begin
      WriteLn('PASS six real Workshop compiler jobs; disabled/HTTP-only/HTTP+files; exact MCP source/design');
    end
    else if LOpening then
    begin
      WriteLn('PASS six real application compiler jobs; disabled/read-only/both authority; exact portable design');
    end
    else
    begin
      WriteLn('PASS two real application compiler jobs with explicit file hosts; exact portable design');
    end;
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
  LDoc.Free;
  LExecutor.Free;
  LOutputs.Free;
end.
