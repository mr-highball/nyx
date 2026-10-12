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
program nyx_virtual_list_observer;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.test.browser.pipe;

var
  GHost: TNyxBrowserPipe;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition or (GHost.RuntimeError <> '') then
  begin
    raise Exception.Create('Trusted virtual list: ' + AReason + ' / ' + GHost.RuntimeError);
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
    LStatus := GHost.Attribute('data-virtual-list');

    if (LStatus = 'failed') or (GHost.RuntimeError <> '') then
    begin
      raise Exception.Create('Compiled list refused: ' + GHost.Attribute('data-event-error'));
    end;

    if LStatus = 'passed' then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Compiled list readiness exceeded the ordinary host budget');
    end;
    Sleep(30);
  until False;
end;

{ Read public observations; never evaluate scripts, compose controls or mutate
  an editor document. Actual pointer/keyboard input acts on this owned app only. }
procedure Position(AIndex, ASelected: Integer);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if (GHost.Attribute('data-list-focus') = 'idea-' + IntToStr(AIndex)) and
      (GHost.Attribute('data-list-selected') = IntToStr(ASelected)) then
    begin
      Check(True, 'exact logical focus and membership');
      Exit;
    end;

    if (GHost.RuntimeError <> '') or (GetTickCount64 - LStarted > 5000) then
    begin
      raise Exception.Create('Expected focus/membership ' + IntToStr(AIndex) + '/' +
        IntToStr(ASelected) + ', observed ' + GHost.Attribute('data-list-focus') + '/' +
        GHost.Attribute('data-list-selected'));
    end;
    Sleep(30);
  until False;
end;

var
  LList: TNyxText;
  LOption: TNyxBrowserBox;
  LBounds: TNyxBrowserBox;
begin
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Use list observer <exact HTTP app> <new evidence directory>');
    end;
    GHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), 1200, 900);
    Ready;
    LList := '[data-runtime-id="' + GHost.Attribute('data-list-id') + '"]';
    GHost.Click(LList + ' [role=option][tabindex="0"]');
    Position(0, 1);
    GHost.Key(nbkEnd);
    Position(4095, 1);
    LOption := GHost.Bounds(LList + ' [aria-posinset="4096"]');
    LBounds := GHost.Bounds(LList);
    Check((LOption.Top >= LBounds.Top - 1) and
      (LOption.Top + LOption.Height <= LBounds.Top + LBounds.Height + 1),
      'trusted End reveals the physical distant option');
    GHost.Key(nbkHome, False, True);
    Position(0, 4096);
    GHost.Key(nbkDown, True);
    Position(1, 4096);
    GHost.Key(nbkHome);
    Position(0, 1);
    Check(not GHost.Exists('[data-runtime-id="second-card/idea-options"] [aria-selected="true"]'),
      'other reusable instance has no selection side effects');
    GHost.Capture('desktop');
    GHost.Resize(390, 844);
    GHost.Key(nbkEnd);
    Position(4095, 1);
    LOption := GHost.Bounds(LList + ' [aria-posinset="4096"]');
    LBounds := GHost.Bounds(LList);
    Check((LOption.Top >= LBounds.Top - 1) and
      (LOption.Top + LOption.Height <= LBounds.Top + LBounds.Height + 1),
      'narrow host retains visible logical keyboard navigation');
    GHost.Tab;
    Check(GHost.Exists('[data-runtime-id="first-card/idea-card-action"]:focus'),
      'ordinary Tab leaves the virtualized composite for its compound action');
    GHost.Capture('narrow');
    FreeAndNil(GHost);
    WriteLn('PASS ', GChecks, ' trusted virtual-list input/resize checks');
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
