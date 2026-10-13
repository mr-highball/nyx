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

program nyx_disclosure_input;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.test.browser.pipe;

var
  LHost: TNyxBrowserPipe;
  LStarted: QWord;
  LChecks: Integer;
  LHeader: TNyxText;
  LSecondHeader: TNyxText;
  LMemo: TNyxText;
  LText: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Disclosure input: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  LHost := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply exact review URL, NEW captures and CSS width');
    end;
    { The public component fixture publishes only its exact physical identities.
      This owner sends trusted host keys/pointer input; it never composes a design
      or evaluates scripts. Semantic composition remains a separate MCP contract. }
    LHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), StrToInt(ParamStr(3)), 900);
    LStarted := GetTickCount64;
    repeat

      if (LHost.RuntimeError <> '') or (GetTickCount64 - LStarted > 30000) then
      begin
        raise Exception.Create('Disclosure review did not become ready; inspect owned receipts');
      end;
      Sleep(5);
    until LHost.Attribute('data-nyx-disclosure-review') = 'ready';
    LHeader := '[data-runtime-id="' + LHost.Attribute('data-disclosure-first-header') + '"]';
    LSecondHeader := '[data-runtime-id="' + LHost.Attribute('data-disclosure-second-header') + '"]';
    LMemo := '[data-runtime-id="' + LHost.Attribute('data-disclosure-first-memo') + '"] textarea';
    Check(LHost.Exists(LHeader + '[aria-expanded="false"]'), 'review begins collapsed');
    LHost.Click(LHeader);
    Check(LHost.Exists(LHeader + '[aria-expanded="true"]:focus'),
      'trusted pointer opens and focuses the real button');
    LHost.Key(nbkSpace);
    Check(LHost.Exists(LHeader + '[aria-expanded="false"]:focus'),
      'trusted Space closes once and retains header focus');
    LHost.Tab;
    Check(LHost.Exists(LSecondHeader + ':focus'), 'Tab skips the closed body');
    LHost.Tab(True);
    Check(LHost.Exists(LHeader + ':focus'), 'reverse Tab returns to the first header');
    LHost.Key(nbkEnter);
    Check(LHost.Exists(LHeader + '[aria-expanded="true"]:focus'),
      'trusted Enter opens once and retains header focus');
    LHost.Tab;
    Check(LHost.Exists(LMemo + ':focus'), 'Tab enters the retained open editor');
    LHost.Key(nbkSelectAll);
    LHost.TypeText('A line worth keeping');
    Check(LHost.TryFieldValue(LMemo, LText) and (LText = 'A line worth keeping'),
      'trusted typing edits the actual memo');
    LHost.Key(nbkF2, True);
    Check(LHost.Exists(LHeader + '[aria-expanded="false"]:focus'),
      'application state close recovers focus during a real keyboard callback');
    LHost.Key(nbkEnter);
    LHost.Tab;
    Check(LHost.Exists(LMemo + ':focus') and LHost.TryFieldValue(LMemo, LText) and
      (LText = 'A line worth keeping'), 'reopening preserves the real typed draft');
    LHost.Capture('expanded');
    LHost.Tab(True);
    LHost.Key(nbkSpace);
    Check(LHost.Exists(LHeader + '[aria-expanded="false"]:focus'),
      'second Space activation closes without losing the header');
    LHost.Capture('collapsed');
    Check(LHost.RuntimeError = '', 'trusted disclosure input produces no browser exception');
    WriteLn('PASS ', LChecks, ' trusted disclosure checks / width ', ParamStr(3));
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
