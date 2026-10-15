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

program nyx_file_open_launch_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.logic, nyx.data, nyx.test.browser.pipe,
  nyx.test.files.openfixture;

var
  LHost: TNyxBrowserPipe;
  LStarted: QWord;
  LBefore: TNyxText;
  LExpected: TNyxText;
  LDirectory: TNyxText;
  LWidth: Integer;
  LEnabled: Boolean;
  LChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(LChecks);
end;

procedure WaitText(const ASelector, AExpected: TNyxText);
begin
  LStarted := GetTickCount64;
  repeat

    if LHost.RuntimeError <> '' then
    begin
      raise Exception.Create('Ordinary compiled browser application failed: ' + String(LHost.RuntimeError));
    end;

    if Pos(AExpected, LHost.ElementHTML(ASelector)) > 0 then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Compiled browser output did not reach its expected complete value');
    end;
    Sleep(20);
  until False;
end;

begin
  LHost := nil;
  try

    if (ParamCount <> 5) or not ((ParamStr(5) = 'enabled') or (ParamStr(5) = 'disabled')) then
    begin
      raise Exception.Create('Supply compiled URL, fresh evidence, CSS width, owned input directory, enabled|disabled');
    end;
    LWidth := StrToInt(ParamStr(3));
    LEnabled := ParamStr(5) = 'enabled';
    LDirectory := TNyxText(ExpandFileName(ParamStr(4))) + TNyxText(PathDelim);
    LHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), LWidth, 900);
    Check(LHost.Attribute('data-nyx-ready') = 'true', 'ordinary generated launcher mounts its application');
    LBefore := LHost.ElementHTML('[data-node="read-output"]');
    Check(Pos('Choose a report to begin.', LBefore) > 0, 'compiled application starts from its saved defaults');

    if not LEnabled then
    begin
      LHost.Click('[data-node="open-report"]');
      Sleep(500);
      Check(not LHost.Exists('input[type="file"]'), 'ungranted application creates no file chooser');
      Check(LHost.ElementHTML('[data-node="read-output"]') = LBefore,
        'ungranted route cannot replace the displayed report');
    end
    else
    begin
      LHost.PickResourceFile('[data-node="open-report"]', LDirectory + OpenFixtureLeaf);
      WaitText('[data-node="read-status"]', 'Read complete');
      LExpected := NewNyxJSONFormat(njlIndented, 2).Format(TNyxDataValue.ParseJSON(OpenFixtureJSON));
      Check(Pos(LExpected, LHost.ElementHTML('[data-node="read-output"]')) > 0,
        'ordinary generated launcher executes exact Unicode/NUL/numeric report formatting');
      Check(Pos(OpenFixtureLeaf, LHost.ElementHTML('[data-node="read-name"]')) > 0,
        'completed selected leaf reaches the normal bound label');
      Check(Pos('1792036800123', LHost.ElementHTML('[data-node="read-metadata"]')) > 0,
        'normal metadata output retains the actual file modification observation');
      LBefore := LHost.ElementHTML('[data-node="read-output"]');
      LHost.PickResourceFile('[data-node="open-report"]', LDirectory + 'large.bin');
      WaitText('[data-node="read-status"]', 'byte budget');
      Check(LHost.ElementHTML('[data-node="read-output"]') = LBefore,
        'over-budget selection preserves the complete preceding report');
    end;
    LHost.Capture('application');
    WriteLn('PASS ', LChecks, ' ordinary compiled browser opening checks / ', ParamStr(5), ' / CSS ', LWidth);
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);

      if LHost <> nil then
      begin
        LHost.Capture('failure');
      end;
      ExitCode := 1;
    end;
  end;
  LHost.Free;
end.
