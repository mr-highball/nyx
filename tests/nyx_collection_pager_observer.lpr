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
program nyx_collection_pager_observer;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.test.browser.pipe;

var
  GHost: TNyxBrowserPipe;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Trusted pager input: ' + AReason);
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
    LStatus := GHost.Attribute('data-pager-controls');

    if (GHost.RuntimeError <> '') or (LStatus = 'failed') then
    begin
      raise Exception.Create('Compiled pager refused / ' + GHost.Attribute('data-event-error'));
    end;

    if LStatus = 'passed' then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Compiled pager did not become ready');
    end;
    Sleep(30);
  until False;
end;

procedure Page(APage, ACount: Integer);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if (GHost.Attribute('data-pager-page') = IntToStr(APage)) and
      (GHost.Attribute('data-pager-visible') = IntToStr(ACount)) then
    begin
      Check(True, 'exact public page and visible membership');
      Exit;
    end;

    if (GHost.RuntimeError <> '') or (GetTickCount64 - LStarted > 5000) then
    begin
      raise Exception.Create('Expected page ' + IntToStr(APage) + '/' + IntToStr(ACount) +
        ', observed ' + GHost.Attribute('data-pager-page') + '/' +
        GHost.Attribute('data-pager-visible'));
    end;
    Sleep(30);
  until False;
end;

function Selector(const AAttribute: TNyxText): TNyxText;
begin
  Result := '[data-node="' + GHost.Attribute(AAttribute) + '"]';
end;

var
  LPrevious: TNyxText;
  LNext: TNyxText;
  LInput: TNyxText;
  LText: TNyxText;
  LBox: TNyxBrowserBox;
begin
  GHost := nil;
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Use pager observer <exact HTTP host> <new evidence directory>');
    end;
    GHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), 1200, 900);
    Ready;
    LPrevious := Selector('data-pager-previous-id');
    LNext := Selector('data-pager-next-id');
    LInput := Selector('data-pager-input-id') + ' input';
    Page(1, 7);
    Check(GHost.Attribute('data-pager-pages') = '6', 'complete filtered page count');
    GHost.Click(LNext);
    Page(2, 7);
    GHost.Click(LInput);
    GHost.Key(nbkSelectAll);
    GHost.TypeText('6');
    GHost.Tab;
    Page(6, 5);
    Check(GHost.TryFieldValue(LInput, LText) and (LText = '6'), 'trusted numeric commit retains its accepted value');
    Check(Pos(' disabled', GHost.ElementHTML(LNext)) > 0, 'physical last-page Next is disabled');
    GHost.Capture('desktop');
    GHost.Click(LPrevious);
    Page(5, 7);
    GHost.Resize(390, 844);
    GHost.Click(LPrevious);
    Page(4, 7);
    LBox := GHost.Bounds(LPrevious);
    Check((LBox.Width > 0) and (LBox.Height > 0), 'narrow viewport keeps a physical pager command');
    GHost.Capture('narrow');
    WriteLn('PASS ', GChecks, ' trusted collection-pager input/resize checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);

      if GHost <> nil then
      begin
        GHost.Capture('failure');
      end;
      ExitCode := 1;
    end;
  end;
  GHost.Free;
end.
