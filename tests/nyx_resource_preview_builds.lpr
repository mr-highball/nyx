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


program nyx_resource_preview_builds;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, fpjson, nyx.text, nyx.data, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.resources, nyx.studio.outputs,
  nyx.studio.builds, nyx.studio.buildexecutor, nyx.studio.directories;

var
  LStream: TFileStream;
  LToolsText: TNyxText;
  LTools: TNyxDataValue;
  LOutputs: TNyxOutputConfiguration;
  LDirectories: TNyxStudioDirectories;
  LBuilder: TNyxBuildExecutor;
  LDocument: TNyxDocument;
  LResult: TJSONObject;
  LTarget: TNyxBuildTarget;
  LChecks: Integer;
begin
  LChecks := 0;
  LOutputs := nil;
  LBuilder := nil;
  LDocument := nil;
  try

    if ParamCount <> 4 then
    begin
      raise Exception.Create('Supply source root, existing toolchain file, NEW runtime and native platform');
    end;

    if DirectoryExists(ParamStr(3)) then
    begin
      raise Exception.Create('Wrapper qualification runtime must be newly owned');
    end;
    LStream := TFileStream.Create(ParamStr(2), fmOpenRead or fmShareDenyNone);
    try
      SetLength(LToolsText, LStream.Size);

      if LToolsText <> '' then
      begin
        LStream.ReadBuffer(LToolsText[1], Length(LToolsText));
      end;
    finally
      LStream.Free;
    end;
    LTools := TNyxDataValue.ParseJSON(LToolsText);
    LOutputs := TNyxOutputConfiguration.Create;
    LOutputs.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LOutputs.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LOutputs.SetField('fpc', LTools.Field('LCL_FPC').AsText);
    LOutputs.SetField('lazarus', LTools.Field('LAZARUS').AsText);
    LOutputs.SetField('platform', ParamStr(4));
    LOutputs.SetField('widgetset', 'win32');
    LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1)).RunningIn(ParamStr(3));
    LBuilder := TNyxBuildExecutor.Create(LDirectories, LOutputs.Encode);
    LDocument := TNyxDocument.Create;
    LDocument.AddPage(NewNyxColumn('home').Add(
      NewNyxLabel('welcome').WithText('A little workshop')).Node);
    LDocument.Resources.Define(NyxResourceRef('copy'), NyxTextResource('Ready to make something'));
    for LTarget := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
    begin
      { Execute only the installed compiler, never the produced application.
        Backend, wrapper and private observation imports compile together. }
      LResult := LBuilder.Build(LDocument, NyxBuildTargetName(LTarget),
        NyxBuildScopeName(bsApplication), '', TNyxCodegen.Generate(LDocument));
      try

        if not LResult.Get('ok', False) then
        begin
          raise Exception.Create('Compiled runtime wrapper failed: ' + LResult.Get('log', ''));
        end;
        Inc(LChecks);
      finally
        LResult.Free;
      end;
    end;
    WriteLn('PASS ', LChecks, ' actual target runtime wrappers compiled');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  LDocument.Free;
  LBuilder.Free;
  LOutputs.Free;
end.
