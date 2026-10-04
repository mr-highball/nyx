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

program nyx_mcp_config_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.studio.mcpconfig, nyx.test.mcp.client;

var
  LRoot: TNyxText;
  LPath: TNyxText;
  LTarget: TNyxText;
  LBlock: TNyxText;
  LNext: TNyxText;
  LOriginal: TNyxText;
  LID: TGUID;
  LChecks: Integer;
  LClient: TNyxMCPTestClient;
  LRefused: Boolean;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(LChecks);
end;

procedure WriteBytes(const APath, AText: TNyxText);
var
  LFile: TFileStream;
begin
  ForceDirectories(ExtractFileDir(APath));
  LFile := TFileStream.Create(APath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

procedure Refuse(const AText: TNyxText);
var
  LFailed: Boolean;
begin
  WriteBytes(LPath, AText);
  LFailed := False;
  try
    NyxMCPPublishBlock(LPath, LBlock);
  except
    on LException: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (NyxMCPReadBytes(LPath) = AText),
    'Rejected ownership must preserve exact configuration bytes');
end;

begin
  try
    LChecks := 0;
    CreateGUID(LID);
    LRoot := IncludeTrailingPathDelimiter(ExpandFileName('build/mcp-client/config-' +
      GUIDToString(LID)));
    LPath := LRoot + '.codex' + PathDelim + 'config.toml';
    LTarget := LRoot + 'user' + PathDelim + 'config.toml';
    LBlock := NyxMCPConfigBegin + #10 + '[mcp_servers.nyx_studio]' + #10 +
      'url = "http://127.0.0.1:8089/mcp/session-one"' + #10 +
      'http_headers = { Authorization = "Bearer fixture-one" }' + #10 + NyxMCPConfigEnd;
    LNext := StringReplace(LBlock, 'one', 'two', [rfReplaceAll]);
    LOriginal := '# preserved α 😀' + #13#10 + '[mcp_servers.example]' + #13#10 +
      'command = "example"' + #13#10;
    WriteBytes(LPath, LOriginal);
    NyxMCPPublishBlock(LPath, LBlock);
    Check(Pos(LOriginal, NyxMCPReadBytes(LPath)) = 1, 'Unrelated bytes retained');
    Check(NyxMCPReadBytes(LPath + '.nyx-backup') = LOriginal, 'Exact backup retained');
    Check(NyxMCPConfigBlock(NyxMCPReadBytes(LPath)) = LBlock, 'Managed block admitted');
    LOriginal := NyxMCPReadBytes(LPath) + #10 + '[features]' + #10 + 'example = true';
    WriteBytes(LPath, LOriginal);
    NyxMCPPublishBlock(LPath, LNext);
    Check(NyxMCPReadBytes(LPath) = StringReplace(LOriginal, LBlock, LNext, []),
      'Rotation retains exact prefix and suffix');
    Check(NyxMCPReadBytes(LPath + '.nyx-backup') = LOriginal, 'Rotation backup is exact');
    Refuse('[mcp_servers.nyx_studio]' + #10 + 'url = "owned"');
    Refuse('[mcp_servers."nyx_studio"]' + #10 + 'url = "owned"');
    Refuse('[mcp_servers]' + #10 + 'nyx_studio = { url = "owned" }');
    Refuse(NyxMCPConfigBegin + #10);
    Refuse(NyxMCPConfigEnd);
    Refuse(LBlock + #10 + NyxMCPConfigEnd);
    Refuse(LBlock + #10 + NyxMCPConfigBegin);
    Refuse('prefix ' + LBlock);
    Refuse(LBlock + ' suffix');
    Refuse(LBlock + #10 + '[mcp_servers.nyx_studio]' + #10);
    WriteBytes(LPath, LOriginal);
    WriteBytes(LTarget, '# user settings stay here' + #10 + 'example = "retained"' + #10);
    LOriginal := NyxMCPReadBytes(LTarget);
    NyxMCPRegisterCodex(LRoot, LTarget);
    Check(Pos(LOriginal, NyxMCPReadBytes(LTarget)) = 1, 'Explicit enrollment retains user settings');
    Check(NyxMCPConfigBlock(NyxMCPReadBytes(LTarget)) = LBlock,
      'Enrollment mirrors the live project block');
    Check(TNyxDataValue.ParseJSON(NyxMCPReadBytes(LRoot +
      '.local' + PathDelim + 'codex-mcp-registration.json')).Field('configPath').AsText = LTarget,
      'Ignored enrollment records only the selected absolute path');
    NyxMCPRefreshRegistration(LRoot, LNext);
    Check(NyxMCPConfigBlock(NyxMCPReadBytes(LTarget)) = LNext, 'Next launch rotates global connection');
    Check(Pos(LOriginal, NyxMCPReadBytes(LTarget)) = 1, 'Rotation preserves unrelated user settings');
    LOriginal := NyxMCPReadBytes(LTarget);
    WriteBytes(LRoot + '.local' + PathDelim + 'codex-mcp-registration.json',
      '{"configPath":"relative/config.toml"}');
    LRefused := False;
    try
      NyxMCPRefreshRegistration(LRoot, LBlock);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (NyxMCPReadBytes(LTarget) = LOriginal),
      'Malformed enrollment preserves selected config');
    { A modified local entry must not exfiltrate the credential. Constructor
      rejects this endpoint before any network connection can occur. }
    WriteBytes(LPath, StringReplace(LBlock, '127.0.0.1', 'example.invalid', []));
    LRefused := False;
    LClient := nil;
    try
      LClient := TNyxMCPTestClient.Create(LPath);
    except
      on LException: Exception do
      begin
        LRefused := Pos('loopback', LException.Message) > 0;
      end;
    end;
    LClient.Free;
    Check(LRefused, 'Remote endpoint refused before transport');
    WriteLn('PASS MCP configuration: ', LChecks, ' checks');
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, 'FAIL MCP configuration: ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
