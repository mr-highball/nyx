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

program nyx_application_navigation_input;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.application.navigation, nyx.test.browser.pipe;

var
  LHost: TNyxBrowserPipe;
  LStarted: QWord;
  LChecks: Integer;
  LWidth: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Application navigation input: ' + AReason);
  end;
  Inc(LChecks);
end;

function Button(AIndex: Integer): TNyxText;
begin
  Result := TNyxText('button[data-node="') + NyxPageNavigationButtonID(AIndex) + '"]';
end;

procedure CheckCurrent(AIndex: Integer);
begin
  Check(LHost.Exists(Button(AIndex) + '[aria-current="page"][data-variant="primary"]') and
    not LHost.Exists(Button(1 - AIndex) + '[aria-current]'),
    'trusted navigation exposes exactly one highlighted current page');
  Check(not LHost.Exists(Button(AIndex) + '[aria-pressed]'),
    'navigation is not exposed as a toggle button');
end;

begin
  LHost := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply owned application URL, NEW capture directory and CSS width');
    end;
    LWidth := StrToInt(ParamStr(3));
    LHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), LWidth, 900);
    LStarted := GetTickCount64;
    repeat

      if LHost.RuntimeError <> '' then
      begin
        LHost.CaptureRuntimeError;
        raise Exception.Create('Application exception; inspect owned runtime-error.json');
      end;

      if GetTickCount64 - LStarted > 30000 then
      begin
        raise Exception.Create('Application ready marker was not observed within its startup budget');
      end;
      Sleep(5);
    until LHost.Attribute('data-nyx-query-workbench-ready') = 'passed';
    Check(Pos('>Tasks<', LHost.ElementHTML(Button(0))) > 0, 'authored Tasks caption reaches its button');
    Check(Pos('>About<', LHost.ElementHTML(Button(1))) > 0, 'authored About caption reaches its button');
    CheckCurrent(0);
    LHost.Capture('tasks');
    LHost.Click(Button(0));
    Check(LHost.Exists('main[data-nyx-page="task-workbench"]'), 'trusted pointer routes by stable page ID');
    LHost.Tab;
    Check(LHost.Exists(Button(1) + ':focus'), 'native browser Tab focuses the next navigation button');
    LHost.Key(nbkEnter);
    Check(LHost.Exists('main[data-nyx-page="about-workbench"]'), 'trusted Enter activates the About page');
    Check(LHost.Exists(Button(1) + ':focus'), 'page replacement preserves keyboard navigation focus');
    CheckCurrent(1);
    LHost.Capture('about');
    LHost.Tab(True);
    Check(LHost.Exists(Button(0) + ':focus'), 'reverse Tab returns to the Tasks button');
    LHost.Key(nbkEnter);
    Check(LHost.Exists('main[data-nyx-page="task-workbench"]'), 'trusted Enter returns to the Tasks page');
    CheckCurrent(0);
    Check(LHost.RuntimeError = '', 'pointer/keyboard navigation emits no browser exception');
    WriteLn('PASS ', LChecks, ' trusted application navigation checks / width ', LWidth);
  except
    on LException: Exception do
    begin

      if LHost <> nil then
      begin
        LHost.Capture('failure');
      end;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  LHost.Free;
end.
