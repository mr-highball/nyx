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

program nyx_studio_menu_observer;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.test.browser.pipe, nyx.test.mcp.client;

var
  GHost: TNyxBrowserPipe;
  GClient: TNyxMCPTestClient;
  GWorkspace: TNyxText;
  GSelection: TNyxText;
  GRevision: Integer;

{ Optional observing qualification uses only bounded semantic queries. A local
  editor renders its starter before it attaches to the authoritative workspace;
  its first visible help button cannot establish which component is selected. }
function Session: TNyxDataValue;
var
  LPacket: TNyxDataValue;
begin
  LPacket := GClient.Tool('nyx_session', NyxObject([
    NyxField('workspace', NyxData(GWorkspace))]));

  if LPacket.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Bounded observing session query refused');
  end;
  Result := LPacket.Field('structuredContent');
end;

procedure WaitText(const ASelector, AExpected: TNyxText);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if GHost.RuntimeError <> '' then
    begin
      raise Exception.Create('Actual observing Studio raised');
    end;

    if Pos(AExpected, GHost.ElementHTML(ASelector)) > 0 then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Studio did not observe the semantic workspace context');
    end;
    Sleep(50);
  until False;
end;

procedure WaitFor(const ASelector: TNyxText; AExists: Boolean = True);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if GHost.RuntimeError <> '' then
    begin
      raise Exception.Create('Actual Studio raised; inspect runtime-error.json');
    end;

    if (GHost.ElementHTML(ASelector) <> '') = AExists then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Studio menu did not reach expected host state / ' + ASelector);
    end;
    Sleep(50);
  until False;
end;

{ Physical chrome geometry is distinct from the design document. Read a bounded
  set of ordinary toolbar faces through CDP, without authoring through the UI.
  Pairwise checks catch overlap even when wrapping moves an action to a new line. }
procedure ObserveToolbar;
const
  CActionIDs: array[0..10] of TNyxText = ('action-undo', 'action-redo',
    'action-code', 'action-import', 'action-save', 'action-outputs',
    'action-agents', 'action-actions', 'action-builds', 'action-build-view',
    'action-build-app');
var
  LHeader: TNyxBrowserBox;
  LBoxes: array[0..10] of TNyxBrowserBox;
  LPresent: array[0..10] of Boolean;
  LIndex: Integer;
  LOther: Integer;
  LCompact: Boolean;
begin
  LCompact := GHost.Exists('[data-node=studio-panelbar]');
  LHeader := GHost.Bounds('[data-node=studio-header]');
  for LIndex := 0 to High(CActionIDs) do
  begin
    LPresent[LIndex] := GHost.ElementHTML(
      '[data-node=' + CActionIDs[LIndex] + ']') <> '';

    if not LPresent[LIndex] then
    begin
      { Builds is capability-dependent in disconnected Studio. Every other
        declared action must exist; absence must not earn a geometry pass. }

      if CActionIDs[LIndex] <> 'action-builds' then
      begin
        GHost.Capture('toolbar-missing');
        raise Exception.Create('Toolbar action is absent / ' + CActionIDs[LIndex]);
      end;
      Continue;
    end;
    { Compact navigation intentionally moves these operations into typed menu
      branches. Only the three visible header actions own physical geometry. }

    if LCompact and not (CActionIDs[LIndex] = 'action-undo') and
      not (CActionIDs[LIndex] = 'action-redo') and
      not (CActionIDs[LIndex] = 'action-actions') then
    begin
      LPresent[LIndex] := False;
      Continue;
    end;
    LBoxes[LIndex] := GHost.Bounds('[data-node=' + CActionIDs[LIndex] + ']');
    WriteLn('Toolbar / ', CActionIDs[LIndex], ' / ',
      LBoxes[LIndex].Left:0:2, ',', LBoxes[LIndex].Top:0:2, ' / ',
      LBoxes[LIndex].Width:0:2, ' x ', LBoxes[LIndex].Height:0:2);

    if (LBoxes[LIndex].Width <= 0) or (LBoxes[LIndex].Height <= 0) or
      (LBoxes[LIndex].Left < LHeader.Left - 1) or
      (LBoxes[LIndex].Left + LBoxes[LIndex].Width > LHeader.Left + LHeader.Width + 1) or
      (LBoxes[LIndex].Top < LHeader.Top - 1) or
      (LBoxes[LIndex].Top + LBoxes[LIndex].Height > LHeader.Top + LHeader.Height + 1) then
    begin
      GHost.Capture('toolbar-overflow');
      raise Exception.Create('Toolbar action exceeds its header / ' +
        CActionIDs[LIndex]);
    end;
    for LOther := 0 to LIndex - 1 do
    begin

      if LPresent[LOther] and
        (LBoxes[LIndex].Left < LBoxes[LOther].Left + LBoxes[LOther].Width - 1) and
        (LBoxes[LOther].Left < LBoxes[LIndex].Left + LBoxes[LIndex].Width - 1) and
        (LBoxes[LIndex].Top < LBoxes[LOther].Top + LBoxes[LOther].Height - 1) and
        (LBoxes[LOther].Top < LBoxes[LIndex].Top + LBoxes[LIndex].Height - 1) then
      begin
        GHost.Capture('toolbar-overlap');
        raise Exception.Create('Toolbar actions overlap / ' + CActionIDs[LOther] +
          ' / ' + CActionIDs[LIndex]);
      end;
    end;
  end;
  GHost.Capture('toolbar-geometry');
end;

var
  LWidth: Integer;
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LPacket: TNyxDataValue;
  LKind: TNyxText;
  LNextAction: TNyxText;
  LPreviousAction: TNyxText;
begin
  try

    if not (ParamCount in [3, 5]) then
    begin
      raise Exception.Create('Use menu observer <loopback URL> <capture directory> <CSS width> ' +
        '[enrolled repository] [exact ordinary workspace]');
    end;
    LWidth := StrToInt(ParamStr(3));

    if ParamCount = 5 then
    begin
      GWorkspace := ParamStr(5);

      if (GWorkspace = '') or (Pos('?workspace=' + GWorkspace, ParamStr(1)) = 0) then
      begin
        raise Exception.Create('Observing URL must identify the supplied ordinary workspace');
      end;
      GClient := TNyxMCPTestClient.Create(IncludeTrailingPathDelimiter(ParamStr(4)) +
        '.codex' + PathDelim + 'config.toml', 'Scooty command menu observer');
      LBefore := Session;
      GSelection := LBefore.Field('selection').AsText;
      GRevision := LBefore.Field('revision').AsInteger;
      LPacket := GClient.Tool('nyx_node', NyxObject([
        NyxField('workspace', NyxData(GWorkspace)), NyxField('id', NyxData(GSelection)),
        NyxField('limit', NyxData(1))]));

      if LPacket.Field('isError').AsBoolean or
        (LPacket.Field('structuredContent').Field('revision').AsInteger <> GRevision) then
      begin
        raise Exception.Create('Bounded selected component query refused or changed revision');
      end;
      LKind := LPacket.Field('structuredContent').Field('node').Field('kind').AsText;
    end;
    GHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), LWidth);

    if GClient <> nil then
    begin
      WaitText('[data-node=studio-subtitle]', LBefore.Field('title').AsText);
    end;

    WaitFor('[data-node=action-actions]');
    ObserveToolbar;
    GHost.Click('[data-node=action-actions]');
    WaitFor('.nyx-popover:popover-open[role=menu] [data-node=studio-menu-inspect]');
    GHost.Capture('studio-component-actions');
    { A connected editor exposes build-job controls immediately after Actions.
      Local optional-service editors continue directly to Build view. Observe
      the mounted toolbar instead of assuming the disconnected Tab order. }
    LNextAction := 'action-build-view';
    LPreviousAction := 'action-agents';

    if GHost.ElementHTML('[data-node=action-builds]') <> '' then
    begin
      LNextAction := 'action-builds';
    end;

    if GHost.Exists('[data-node=studio-panelbar]') then
    begin
      LNextAction := 'action-panel-project';
      LPreviousAction := 'action-redo';
    end;
    GHost.Tab;
    WaitFor('.nyx-popover:popover-open', False);
    WaitFor('[data-node=' + LNextAction + ']:focus');
    GHost.Click('[data-node=action-actions]');
    WaitFor('.nyx-popover:popover-open[role=menu]');
    GHost.Tab(True);
    WaitFor('.nyx-popover:popover-open', False);
    WaitFor('[data-node=' + LPreviousAction + ']:focus');
    GHost.Click('[data-node=action-actions]');
    WaitFor('.nyx-popover:popover-open[role=menu] [data-node=studio-menu-inspect]');
    GHost.Click('.nyx-popover:popover-open [data-node=studio-menu-inspect]');
    WaitFor('[data-node=studio-menu-inspect][aria-expanded=true]');
    WaitFor('.nyx-popover:popover-open [data-node=studio-menu-events]');
    GHost.Capture('studio-inspector-submenu');
    { Real host Tab must leave every submenu from the original menu button. }
    GHost.Tab;
    WaitFor('.nyx-popover:popover-open', False);
    WaitFor('[data-node=' + LNextAction + ']:focus');
    GHost.Click('[data-node=action-actions]');
    WaitFor('.nyx-popover:popover-open [data-node=studio-menu-inspect]');
    GHost.Click('.nyx-popover:popover-open [data-node=studio-menu-inspect]');
    WaitFor('.nyx-popover:popover-open [data-node=studio-menu-events]');
    GHost.Tab(True);
    WaitFor('.nyx-popover:popover-open', False);
    WaitFor('[data-node=' + LPreviousAction + ']:focus');
    GHost.Click('[data-node=action-actions]');
    WaitFor('.nyx-popover:popover-open [data-node=studio-menu-inspect]');
    GHost.Click('.nyx-popover:popover-open [data-node=studio-menu-inspect]');
    WaitFor('.nyx-popover:popover-open [data-node=studio-menu-events]');
    GHost.Click('.nyx-popover:popover-open [data-node=studio-menu-events]');
    WaitFor('.nyx-popover:popover-open', False);
    WaitFor('[data-node=event-click-add]');

    if GClient <> nil then
    begin
      { The compact Inspector does not exist until the menu mounts its pane.
        Verify the shared selection here, after bounded title readiness and the
        actual menu command, instead of waiting for an absent initial control. }
      WaitText('[data-node=selected-label]', LKind + ' / ' + GSelection);
    end;
    GHost.Capture('studio-menu-events');
    GHost.Click('[data-node=action-actions]');
    WaitFor('.nyx-popover:popover-open [data-node=studio-menu-inspect]');
    GHost.Click('.nyx-popover:popover-open [data-node=studio-menu-inspect]');
    WaitFor('.nyx-popover:popover-open [data-node=studio-menu-help]');
    GHost.Click('.nyx-popover:popover-open [data-node=studio-menu-help]');
    WaitFor('.nyx-popover:popover-open [data-node=component-help-description]');
    GHost.Capture('studio-menu-component-help');
    GHost.Click('.nyx-popover:popover-open [data-node=component-help-close]');
    WaitFor('.nyx-popover:popover-open', False);
    GHost.Capture('studio-menu-help-closed');

    if GClient <> nil then
    begin
      LAfter := Session;

      if (LAfter.Field('revision').AsInteger <> GRevision) or
        (LAfter.Field('selection').AsText <> GSelection) or
        (LAfter.Field('view').AsText <> LBefore.Field('view').AsText) or
        (LAfter.Field('pendingDraft').AsBoolean <> LBefore.Field('pendingDraft').AsBoolean) or
        (LAfter.Field('canUndo').AsBoolean <> LBefore.Field('canUndo').AsBoolean) or
        (LAfter.Field('canRedo').AsBoolean <> LBefore.Field('canRedo').AsBoolean) then
      begin
        raise Exception.Create('Menu navigation changed the semantic authoring context');
      end;
      GClient.Close;
      FreeAndNil(GClient);
    end;
    FreeAndNil(GHost);
    WriteLn('PASS actual full browser Studio menu / host Tab / Events / help / CSS ', LWidth);
  except
    on E: Exception do
    begin

      if GHost <> nil then
      begin
        GHost.Capture('failure');
      end;
      GHost.Free;

      if GClient <> nil then
      begin
        GClient.Close;
      end;
      GClient.Free;
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
