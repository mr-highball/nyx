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
program nyx_virtual_tree_observer;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.test.browser.pipe;

const
  CPhysicalDraft: TNyxText = 'A physical draft / 🌙';
  CAcceptedCaption: TNyxText = 'A moonlit handbook / 🌙';

var
  GHost: TNyxBrowserPipe;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition or (GHost.RuntimeError <> '') then
  begin
    raise Exception.Create('Trusted tree: ' + AReason + ' / ' + GHost.RuntimeError);
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
    LStatus := GHost.Attribute('data-virtual-tree');

    if (LStatus = 'failed') or (GHost.RuntimeError <> '') then
    begin
      raise Exception.Create('Compiled tree refused: ' + GHost.Attribute('data-event-error'));
    end;

    if LStatus = 'passed' then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Compiled tree readiness exceeded the ordinary host budget');
    end;
    Sleep(30);
  until False;
end;

{ Public fixture observations are read-only. No editor design is composed by
  this driver, and no scripts are evaluated to substitute for host input. }
procedure Position(AIndex, ASelected: Integer);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if (GHost.Attribute('data-tree-focus') = 'chapter-' + IntToStr(AIndex)) and
      (GHost.Attribute('data-tree-selected') = IntToStr(ASelected)) then
    begin
      Check(True, 'exact logical focus and complete membership');
      Exit;
    end;

    if (GHost.RuntimeError <> '') or (GetTickCount64 - LStarted > 5000) then
    begin
      raise Exception.Create('Expected tree focus/membership ' + IntToStr(AIndex) + '/' +
        IntToStr(ASelected) + ', observed ' + GHost.Attribute('data-tree-focus') + '/' +
        GHost.Attribute('data-tree-selected'));
    end;
    Sleep(30);
  until False;
end;

var
  LTree: TNyxText;
  LLast: TNyxText;
  LValue: TNyxText;
  LSummary: TNyxBrowserBox;
  LBounds: TNyxBrowserBox;
begin
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Use tree observer <exact HTTP app> <new evidence directory>');
    end;
    GHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), 1200, 900);
    Ready;
    LTree := '[data-runtime-id="' + GHost.Attribute('data-tree-id') + '"]';
    LLast := LTree + ' [data-nyx-item="chapter-4095"]';
    GHost.Key(nbkHome);
    Position(0, 1);
    GHost.Key(nbkLeft);
    Check(GHost.Exists(LTree + ' [data-nyx-item="chapter-0"][aria-expanded="false"]'),
      'trusted Left closes the current branch');
    GHost.Key(nbkRight);
    Check(GHost.Exists(LTree + ' [data-nyx-item="chapter-0"][aria-expanded="true"]'),
      'trusted Right opens a branch with unrealized logical children');
    GHost.Key(nbkRight);
    Position(1, 1);
    GHost.Key(nbkEnd);
    Position(4095, 1);
    LSummary := GHost.Bounds(LLast + ' > summary');
    LBounds := GHost.Bounds(LTree);
    Check((LSummary.Top >= LBounds.Top - 1) and
      (LSummary.Top + LSummary.Height <= LBounds.Top + LBounds.Height + 1),
      'trusted End reveals the distant summary within its own tree viewport');
    GHost.Key(nbkHome, False, True);
    Position(0, 4096);
    GHost.Key(nbkDown, True);
    Position(1, 4096);
    GHost.Key(nbkHome);
    Position(0, 1);
    GHost.Capture('desktop');
    GHost.Resize(390, 844);
    GHost.Key(nbkEnd);
    Position(4095, 1);
    LSummary := GHost.Bounds(LLast + ' > summary');
    LBounds := GHost.Bounds(LTree);
    Check((LSummary.Top >= LBounds.Top - 1) and
      (LSummary.Top + LSummary.Height <= LBounds.Top + LBounds.Height + 1),
      'narrow host reveals the complete logical keyboard destination');
    GHost.Click(LLast + ' .nyx-cell-value');
    Position(4095, 1);
    GHost.Key(nbkF2);
    Check(GHost.Exists(LLast + ' input:focus:not([hidden])'),
      'trusted F2 enters the actual owned editor');
    GHost.Key(nbkSelectAll);
    GHost.TypeText(CPhysicalDraft);
    Check(GHost.TryFieldValue(LLast + ' input', LValue) and
      (LValue = CPhysicalDraft), 'trusted text preserves supplementary Unicode');
    GHost.Key(nbkEscape);
    Check(GHost.TryFieldValue(LLast + ' input', LValue) and
      (LValue = CAcceptedCaption), 'Escape restores the accepted runtime value');
    Check(GHost.Exists(LLast + ':focus') and not GHost.Exists(LLast + ' input:not([hidden])'),
      'Escape returns navigation focus to the row and closes editing');
    GHost.Tab;
    Check(not GHost.Exists(LTree + ' :focus') and not GHost.Exists(LTree + ':focus'),
      'ordinary Tab leaves the composite instead of trapping browser focus');
    GHost.Capture('narrow');
    FreeAndNil(GHost);
    WriteLn('PASS ', GChecks, ' trusted tree pointer/keyboard/editing/resize/Tab checks');
  except
    on LError: Exception do
    begin
      FreeAndNil(GHost);
      WriteLn('FAIL ', LError.Message);
      ExitCode := 1;
    end;
  end;
end.
