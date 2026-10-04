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

program nyx_mcp_diagnostic_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.data, nyx.test.mcp.client;

var
  LClient: TNyxMCPTestClient;
  LSummary: TNyxDataValue;
  LDiagnostics: TNyxDataValue;
  LItem: TNyxDataValue;
  LIndex: Integer;
  LFound: Boolean;
  LStarted: QWord;

begin
  LClient := nil;
  try
    LClient := TNyxMCPTestClient.Create(ParamStr(1));
    LStarted := GetTickCount64;
    repeat
      LSummary := LClient.Tool('nyx_session', NyxObject([])).Field('structuredContent');
      LDiagnostics := LClient.Tool('nyx_diagnostics', NyxObject([
        NyxField('limit', NyxData(50))])).Field('structuredContent');

      if LSummary.Field('pendingDraft').AsBoolean and
        (LDiagnostics.Field('total').AsInteger > 0) then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 15000 then
      begin
        raise Exception.Create('Live editor did not upload its compiler report and pending draft');
      end;
      Sleep(50);
    until False;
    LFound := False;
    for LIndex := 0 to LDiagnostics.Field('items').Count - 1 do
    begin
      LItem := LDiagnostics.Field('items').Item(LIndex);

      if Pos('MissingApplicationFunction', LItem.Field('message').AsText) > 0 then
      begin

        if (LItem.Field('line').AsInteger < 1) or
          (LItem.Field('column').AsInteger <> 11) or
          LItem.Field('navigable').AsBoolean then
        begin
          raise Exception.Create('Agent compiler context lost mapped Unicode coordinates or stale-source guard');
        end;
        LFound := True;
      end;
    end;

    if not LFound then
    begin
      raise Exception.Create('Actual delegated compiler helper error missing from semantic agent context');
    end;
    LClient.Close;
    LClient.Free;
    LClient := nil;
    WriteLn('PASS live MCP compiler report, source coordinates and pending-draft guard');
  except
    on LException: Exception do
    begin
      LClient.Free;
      WriteLn('FAIL ', LException.Message);
      Halt(1);
    end;
  end;
end.
