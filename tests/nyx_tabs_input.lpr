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

program nyx_tabs_input;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.test.browser.pipe;

var
  LHost: TNyxBrowserPipe;
  LStarted: QWord;
  LChecks: Integer;
  LText: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Tabs input: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  LHost := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply exact companion URL, NEW captures and CSS width');
    end;
    LHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), StrToInt(ParamStr(3)), 900);
    LStarted := GetTickCount64;
    repeat

      if (LHost.RuntimeError <> '') or (GetTickCount64 - LStarted > 30000) then
      begin
        raise Exception.Create('Exact companion did not become ready; inspect owned receipts');
      end;
      Sleep(5);
    until LHost.Attribute('data-nyx-tabs-demo-ready') = 'passed';
    LHost.Click('button[data-nyx-tab="plan"]');
    Check(LHost.Exists('[data-nyx-tab="plan"][aria-selected="true"]:focus'),
      'trusted pointer focuses and selects Plan');
    LHost.Capture('plan');
    LHost.Tab;
    Check(LHost.Exists('[role="tabpanel"]:not([hidden]):focus'),
      'Tab enters a panel with a leading heading');
    LHost.Tab;
    Check(LHost.Exists('[data-node="plan-notes"] textarea:focus'),
      'next Tab reaches the retained memo');
    Check(LHost.Bounds('[data-node="plan-notes"] textarea').Height > 150,
      'typed memo height allocates a useful editing face below its caption');
    LHost.TypeText('An idea worth keeping');
    LHost.Tab(True);
    LHost.Tab(True);
    Check(LHost.Exists('[data-nyx-tab="plan"]:focus'), 'reverse Tab returns to the single header entry');
    LHost.Key(nbkRight);
    Check(LHost.Exists('[data-nyx-tab="details"][aria-selected="true"]:focus'),
      'horizontal Right automatically selects Details');
    Check(LHost.Exists('[role="tabpanel"][aria-labelledby="idea-sections-tab-header-0"][hidden]'),
      'inactive Plan is retained and hidden');
    LHost.Capture('details');
    LHost.Key(nbkEnd);
    Check(LHost.Exists('[data-nyx-tab="review"][aria-selected="true"]:focus'), 'End reaches Review');
    LHost.Key(nbkRight);
    Check(LHost.Exists('[data-nyx-tab="plan"][aria-selected="true"]:focus'), 'Right wraps to Plan');
    Check(LHost.TryFieldValue('[data-node="plan-notes"] textarea', LText) and
      (LText = 'An idea worth keeping'), 'trusted text survives complete tab navigation');
    LHost.Key(nbkDown);
    Check(LHost.Exists('[data-nyx-tab="plan"][aria-selected="true"]:focus'),
      'horizontal tabs retain unrelated Down behavior');
    LHost.Key(nbkHome);
    Check(LHost.Exists('[data-nyx-tab="plan"][aria-selected="true"]:focus'), 'Home stays at the first tab');
    Check(LHost.RuntimeError = '', 'trusted navigation produces no browser exception');
    WriteLn('PASS ', LChecks, ' trusted retained-tab checks / width ', ParamStr(3));
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
