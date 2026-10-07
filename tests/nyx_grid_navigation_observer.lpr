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

program nyx_grid_navigation_observer;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.test.browser.pipe;

var
  GHost: TNyxBrowserPipe;
  GChecks: Integer;

{ The observer authors no layout, schema, source or application defaults. It
  exercises host input against the ordinary exact MCP-exported application. }
procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Host grid input: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Ready;
var
  LStarted: QWord;
  LStatus: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat
    LStatus := GHost.Attribute('data-grid-tests');

    if (GHost.RuntimeError <> '') or (LStatus = 'failed') then
    begin
      raise Exception.Create('Generated control journey refused / ' +
        GHost.Attribute('data-event-error'));
    end;

    if LStatus = 'passed' then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Generated grid did not become ready');
    end;
    Sleep(30);
  until False;
end;

{ Read only three small focus attributes supplied by the fixture's ordinary
  focus listener. The listener reports the target established by the product;
  neither side assigns focus or invokes a private editor method. }
procedure Cell(const ARow: TNyxText; AColumn: Integer; const AMode: TNyxText);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if (GHost.Attribute('data-grid-row') = ARow) and
      (GHost.Attribute('data-grid-column') = IntToStr(AColumn)) and
      (GHost.Attribute('data-grid-mode') = AMode) then
    begin
      Check(True, 'exact focused cell');
      Exit;
    end;

    if (GHost.RuntimeError <> '') or (GetTickCount64 - LStarted > 5000) then
    begin
      raise Exception.Create('Expected ' + ARow + '/' + IntToStr(AColumn) + '/' +
        AMode + ', observed ' + GHost.Attribute('data-grid-row') + '/' +
        GHost.Attribute('data-grid-column') + '/' + GHost.Attribute('data-grid-mode'));
    end;
    Sleep(30);
  until False;
end;

function Value(const ASelector: TNyxText): TNyxText;
begin
  Check(GHost.TryFieldValue(ASelector, Result), 'exact visible editor remains mounted');
end;

procedure Journey;
const
  CPriority = '[data-nyx-item=plan] [data-nyx-column="1"] input';
begin
  Ready;
  Check(GHost.Attribute('data-grid-checks') = '43',
    'the shared generated application completes its current actual-control checks');
  Cell('plan', 0, 'td');
  GHost.Key(nbkRight);
  Cell('plan', 1, 'td');
  GHost.Key(nbkDown);
  Cell('design', 1, 'td');
  GHost.Key(nbkUp, False, True);
  Cell('plan', 1, 'td');
  Check(GHost.Exists('[data-nyx-item=plan][aria-selected=true]') and
    GHost.Exists('[data-nyx-item=design][aria-selected=true]'),
    'Shift vertical navigation publishes the exact two-row range');
  GHost.Key(nbkHome);
  Cell('plan', 0, 'td');
  Check(GHost.Exists('[data-nyx-item=design][aria-selected=true]'),
    'horizontal navigation retains independent row membership');
  GHost.Key(nbkEnd);
  Cell('plan', 2, 'td');
  GHost.Key(nbkEnter);
  Cell('plan', 2, 'td');
  GHost.Key(nbkEnd, True);
  Cell('share', 2, 'td');
  GHost.Key(nbkRight);
  Cell('share', 2, 'td');
  GHost.Key(nbkHome, True);
  Cell('plan', 0, 'td');
  GHost.Key(nbkRight);
  GHost.Key(nbkEnter);
  Cell('plan', 1, 'input');
  GHost.Key(nbkSelectAll);
  GHost.TypeText('9');
  Check(Value(CPriority) = '9', 'trusted text enters the numeric cell draft');
  GHost.Key(nbkEscape);
  Cell('plan', 1, 'td');
  Check(Value(CPriority) = '1', 'Escape discards the draft before native blur');
  GHost.Key(nbkF2);
  Cell('plan', 1, 'input');
  GHost.Key(nbkF2);
  Cell('plan', 1, 'td');
  GHost.Capture('current-cell');
  GHost.Tab;
  Check((GHost.Attribute('data-grid-mode') = 'button') and
    (GHost.Attribute('data-grid-control') = 'after-table'),
    'navigation Tab leaves the grid at the next ordinary control');
  GHost.Tab(True);
  Cell('plan', 1, 'td');
  GHost.Key(nbkHome);
  GHost.Key(nbkEnter);
  Cell('plan', 0, 'input');
  GHost.Tab;
  Cell('plan', 1, 'input');
  GHost.Tab(True);
  Cell('plan', 0, 'input');
  GHost.Tab;
  GHost.Tab;
  Check((GHost.Attribute('data-grid-mode') = 'button') and
    (GHost.Attribute('data-grid-control') = 'after-table'),
    'editor boundary Tab leaves without a keyboard trap');
  GHost.Tab(True);
  Cell('plan', 1, 'td');
  Check(GHost.Attribute('data-grid-source-stable') = 'true',
    'trusted input retains exact document defaults');
  Check(GHost.RuntimeError = '', 'ordinary host input raises no runtime exception');
  GHost.Capture('completed');
end;

var
  LWidth: Integer;
  LHeight: Integer;
begin
  GHost := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Use grid observer <loopback URL> <new evidence directory> <CSS width>');
    end;
    LWidth := StrToInt(ParamStr(3));
    LHeight := 900;

    if LWidth = 390 then
    begin
      LHeight := 640;
    end;
    GHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), LWidth, LHeight);
    Journey;
    FreeAndNil(GHost);
    WriteLn(NyxObject([
      NyxField('result', NyxData('passed')),
      NyxField('checks', NyxData(GChecks)),
      NyxField('width', NyxData(LWidth)),
      NyxField('height', NyxData(LHeight))]).ToJSON);
  except
    on LException: Exception do
    begin

      if GHost <> nil then
      begin
        GHost.Capture('failure');
      end;
      GHost.Free;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
