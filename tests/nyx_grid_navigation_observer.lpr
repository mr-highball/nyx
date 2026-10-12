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

{ Trusted Chromium paging uses the actual semantic large-table companion. Only
  bounded focus/selection attributes are read; no source, layout, focus or data
  is assigned by script. Resize changes the real host viewport. }
procedure PagingJourney;
const
  CPriority = '[data-node="paging-table"] [data-nyx-item=idea-1] [data-nyx-column="1"] input';
var
  LDown: Integer;
  LUp: Integer;
  LSmall: Integer;
  LLarge: Integer;
  LSizing: Boolean;
  LFirstWidth: Double;

  procedure CheckColumnWidths;
  begin

    if LSizing then
    begin
      Check(Abs(GHost.Bounds('[data-node="paging-table"] [data-nyx-item=idea-1] ' +
        '[data-nyx-column="1"][role=gridcell]').Width - 96) <= 1,
        'physical fixed pixels survive actual viewport changes');
    end;
  end;

  function RowNumber: Integer;
  var
    LID: TNyxText;
  begin
    LID := GHost.Attribute('data-grid-row');
    Check(Pos('idea-', LID) = 1, 'paging focus belongs to the exact semantic dataset');
    Result := StrToInt(Copy(LID, 6, Length(LID)));
    Check((Result >= 1) and (Result <= 40), 'paging identity is within admitted membership');
  end;

begin
  LSizing := GHost.Attribute('data-grid-column-sizing') = 'true';
  Check((not LSizing and (GHost.Attribute('data-grid-checks') = '70')) or
    (LSizing and (GHost.Attribute('data-grid-checks') = '72')),
    'the generated browser completes original and paging control checks');
  Cell('idea-1', 0, 'td');
  GHost.Key(nbkRight);
  GHost.Key(nbkPageDown);
  LDown := RowNumber;
  Check((LDown > 2) and (LDown < 40), 'trusted Page Down advances a visible page');
  Cell('idea-' + TNyxText(IntToStr(LDown)), 1, 'td');
  Check(GHost.Attribute('data-grid-selected-count') = '1',
    'ordinary trusted paging selects only its destination');
  GHost.Key(nbkPageUp, True);
  LUp := RowNumber;
  Check(LUp < LDown, 'Control Page Up moves focus toward the first row');
  Cell('idea-' + TNyxText(IntToStr(LUp)), 1, 'td');
  Check(GHost.Attribute('data-grid-selected-count') = '1',
    'Control paging retains independent membership');
  GHost.Key(nbkHome, True);
  GHost.Key(nbkRight);
  GHost.Key(nbkDown);
  GHost.Key(nbkUp);
  GHost.Key(nbkPageDown, False, True);
  LDown := RowNumber;
  Check(GHost.Attribute('data-grid-selected-count') = IntToStr(LDown),
    'Shift paging admits the complete stable range from the first row');
  Cell('idea-' + TNyxText(IntToStr(LDown)), 1, 'td');
  GHost.Key(nbkEnd, True);
  GHost.Key(nbkPageDown);
  Cell('idea-40', 2, 'td');
  GHost.Key(nbkHome, True);
  GHost.Key(nbkPageUp);
  Cell('idea-1', 0, 'td');
  GHost.Key(nbkRight);
  GHost.Key(nbkEnter);
  Cell('idea-1', 1, 'input');
  GHost.Key(nbkSelectAll);
  GHost.TypeText('99');
  GHost.Key(nbkPageDown);
  Cell('idea-1', 1, 'input');
  Check(Value(CPriority) = '99', 'trusted paging retains the owned cell draft');

  if LSizing then
  begin
    GHost.Resize(640, 700);
    Cell('idea-1', 1, 'input');
    Check(Value(CPriority) = '99', 'trusted host resize retains the exact active draft');
    CheckColumnWidths;
  end;
  GHost.Key(nbkEscape);
  Cell('idea-1', 1, 'td');
  Check(Value(CPriority) = '1', 'Escape restores accepted data after editor paging');
  GHost.Capture('paging-input');
  GHost.Resize(900, 240);
  CheckColumnWidths;
  LFirstWidth := GHost.Bounds('[data-node="paging-table"] [data-nyx-item=idea-1] ' +
    '[data-nyx-column="0"][role=gridcell]').Width;
  GHost.Key(nbkHome, True);
  GHost.Key(nbkPageDown);
  LSmall := RowNumber;
  GHost.Capture('short-viewport-page');
  GHost.Resize(900, 900);
  GHost.Key(nbkHome, True);
  GHost.Key(nbkPageDown);
  LLarge := RowNumber;
  Check(LLarge > LSmall, 'trusted page distance follows actual viewport resize');
  GHost.Capture('tall-viewport-page');
  GHost.Resize(390, 640);
  GHost.Key(nbkHome, True);
  CheckColumnWidths;

  if LSizing then
  begin
    Check(GHost.Bounds('[data-node="paging-table"] [data-nyx-item=idea-1] ' +
      '[data-nyx-column="0"][role=gridcell]').Width < LFirstWidth,
      'physical flexible column contracts on the narrow host');
  end;
  GHost.Key(nbkHome, True);
  GHost.Key(nbkPageDown);
  LDown := RowNumber;
  Check((LDown > 1) and (LDown <= 40), 'narrow viewport keeps bounded paging');
  Cell('idea-' + TNyxText(IntToStr(LDown)), 0, 'td');
  GHost.Capture('compact-paging');
  Check(GHost.Attribute('data-grid-source-stable') = 'true',
    'all trusted input retains the exact semantic authored defaults');
  Check(GHost.RuntimeError = '', 'trusted paging raises no browser runtime error');
end;

procedure Journey;
const
  CPriority = '[data-nyx-item=plan] [data-nyx-column="1"] input';
begin
  Ready;

  if GHost.Attribute('data-grid-scene') = 'paging' then
  begin
    PagingJourney;
    Exit;
  end;
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
