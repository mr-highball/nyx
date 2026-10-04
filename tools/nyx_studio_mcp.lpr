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

program nyx_studio_mcp;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.studio.mcpconfig, nyx.test.mcp.client;

type
  { Command spellings are confined to this process boundary. Design edits still
    enter the server's typed, revision-aware command pipeline through real MCP. }
  TClientCommand = (ccRegister, ccTools, ccCall);

function Command(const AText: String): TClientCommand;
begin

  if AText = 'register' then
  begin
    Exit(ccRegister);
  end;

  if AText = 'tools' then
  begin
    Exit(ccTools);
  end;

  if AText = 'call' then
  begin
    Exit(ccCall);
  end;
  raise Exception.Create('Use register <repository> <config.toml>, tools <repository> [tool], ' +
    'or call <repository> <tool> <arguments.json>');
end;

function HasField(const AValue: TNyxDataValue; const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if AValue.Kind <> ndObject then
  begin
    Exit;
  end;
  for LIndex := 0 to AValue.Count - 1 do
  begin

    if AValue.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
end;

var
  LCommand: TClientCommand;
  LRepository: TNyxText;
  LConfiguration: TNyxText;
  LClient: TNyxMCPTestClient;
  LArguments: TNyxDataValue;
  LResult: TNyxDataValue;
  LOutput: TNyxDataValue;
  LTool: TNyxText;
  LIndex: Integer;
  LExit: Integer;
  LFound: Boolean;
begin
  LClient := nil;
  LExit := 0;
  try
    LCommand := Command(ParamStr(1));

    if ParamCount < 2 then
    begin
      raise Exception.Create('An explicit Studio repository is required');
    end;
    LRepository := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
    LConfiguration := LRepository + '.codex' + PathDelim + 'config.toml';

    if LCommand = ccRegister then
    begin

      if ParamCount <> 3 then
      begin
        raise Exception.Create('Registration requires the chosen Codex config.toml path');
      end;
      NyxMCPRegisterCodex(LRepository, ExpandFileName(ParamStr(3)));
      WriteLn('Nyx MCP registered; Studio will refresh this managed entry on launch.');
    end
    else
    begin
      { Reuse the qualified native protocol consumer used by HTTP integration
        tests. This developer/demo tool is not a dependency of the product UI.
        Nothing claims/replaces a document or makes an implicit mutation. }
      NyxMCPConfigBlock(NyxMCPReadBytes(LConfiguration));
      LClient := TNyxMCPTestClient.Create(LConfiguration, 'Scooty semantic client');
      try

        if LCommand = ccTools then
        begin

          if (ParamCount < 2) or (ParamCount > 3) then
          begin
            raise Exception.Create('Tool discovery requires the repository and an optional exact tool');
          end;
          LTool := ParamStr(3);
          LFound := False;
          LResult := LClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools');
          for LIndex := 0 to LResult.Count - 1 do
          begin
            LOutput := LResult.Item(LIndex);

            if (LTool = '') or (LOutput.Field('name').AsText = LTool) then
            begin
              LFound := True;
              WriteLn(NyxObject([
                NyxField('name', LOutput.Field('name')),
                NyxField('description', LOutput.Field('description')),
                NyxField('inputSchema', LOutput.Field('inputSchema'))]).ToJSON);
            end;
          end;

          if not LFound then
          begin
            raise Exception.Create('The requested semantic tool is not advertised by this Studio');
          end;
        end
        else
        begin

          if ParamCount <> 4 then
          begin
            raise Exception.Create('Calls require an exact tool name and an arguments JSON file');
          end;
          LTool := ParamStr(3);

          if Pos('nyx_', LTool) <> 1 then
          begin
            raise Exception.Create('Only Nyx semantic tools are supported');
          end;
          LArguments := TNyxDataValue.ParseJSON(NyxMCPReadBytes(ParamStr(4)));

          if LArguments.Kind <> ndObject then
          begin
            raise Exception.Create('Tool arguments must be a JSON object');
          end;
          LResult := LClient.Tool(LTool, LArguments);
          LOutput := LResult;

          if HasField(LResult, 'structuredContent') then
          begin
            LOutput := LResult.Field('structuredContent');
          end;
          WriteLn(LOutput.ToJSON);

          if HasField(LResult, 'isError') and LResult.Field('isError').AsBoolean then
          begin
            LExit := 2;
          end;
        end;
      finally
        { Close once and never retry a timed-out mutation. A caller can resolve
          ambiguous delivery with the same server operationId and exact payload. }
        LClient.Close;
      end;
    end;
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, 'Nyx MCP: ', LException.Message);
      LExit := 1;
    end;
  end;
  LClient.Free;
  ExitCode := LExit;
end.
