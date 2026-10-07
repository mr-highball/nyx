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

program nyx_studio_workspace_observer;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.studio.view, nyx.test.browser.pipe,
  nyx.test.mcp.client;

var
  GHost: TNyxBrowserPipe;
  GClient: TNyxMCPTestClient;
  GWorkspace: TNyxText;
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Workspace allocation: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure WaitFor(const ASelector: TNyxText; AExists: Boolean = True);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if GHost.RuntimeError <> '' then
    begin
      raise Exception.Create('Ordinary Studio raised a browser exception');
    end;

    if GHost.Exists(ASelector) = AExists then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Studio presentation did not settle / ' + ASelector);
    end;
    Sleep(30);
  until False;
end;

function Session: TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  if GWorkspace = '' then
  begin
    LReply := GClient.Tool('nyx_session', NyxObject([]));
  end
  else
  begin
    LReply := GClient.Tool('nyx_session', NyxObject([
      NyxField('workspace', NyxData(GWorkspace))]));
  end;
  Check(not LReply.Field('isError').AsBoolean, 'bounded semantic session query admits / ' +
    LReply.Field('content').Item(0).Field('text').AsText);
  Result := LReply.Field('structuredContent');
end;

{ Navigate only editor chrome. Application design composition, source and state
  are never changed through the browser. These are actual host pointer events. }
procedure Menu(const ABranch, ACommand: TNyxText);
begin
  GHost.Click('[data-node=action-actions]');
  WaitFor('.nyx-popover:popover-open [data-node=studio-menu-' + ABranch + ']');
  GHost.Click('.nyx-popover:popover-open [data-node=studio-menu-' + ABranch + ']');
  WaitFor('.nyx-popover:popover-open [data-node=studio-menu-' + ACommand + ']');
  GHost.Click('.nyx-popover:popover-open [data-node=studio-menu-' + ACommand + ']');
  WaitFor('.nyx-popover:popover-open', False);
end;

var
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LCanvas: TNyxBrowserBox;
  LHeader: TNyxBrowserBox;
  LWidth: Integer;
  LHeight: Integer;
  LCompact: Boolean;
  LCollapsedHeight: Double;
  LExpandedDetailHeight: Double;
  LKey: TNyxText;
  LSource: TNyxText;
  LRestoredSource: TNyxText;
  LIndex: Integer;
const
  CContextFields: array[0..6] of TNyxText = ('revision', 'selection', 'view',
    'pendingDraft', 'canUndo', 'canRedo', 'title');
begin
  try

    if not (ParamCount in [4, 6]) then
    begin
      raise Exception.Create('Use workspace observer <loopback URL> <captures> <width> <height> ' +
        '[enrolled root] [workspace]');
    end;
    LWidth := StrToInt(ParamStr(3));
    LHeight := StrToInt(ParamStr(4));
    LCompact := NyxStudioCompactHost(LWidth, LHeight);

    if ParamCount = 6 then
    begin
      GWorkspace := ParamStr(6);

      if GWorkspace = 'primary' then
      begin
        GWorkspace := '';
      end;
      GClient := TNyxMCPTestClient.Create(IncludeTrailingPathDelimiter(ParamStr(5)) +
        '.codex' + PathDelim + 'config.toml', 'Scooty workspace allocation observer');
      LBefore := Session;
    end;
    GHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), LWidth, LHeight);
    WaitFor('[data-node=studio-canvas-wrap]');
    WaitFor('[data-node=action-actions]');
    Sleep(150);
    Check(GHost.Exists('[data-node=studio-panelbar]') = LCompact,
      'both width and short-host policy select the ordinary shell');
    LHeader := GHost.Bounds('[data-node=studio-header]');

    if LCompact then
    begin
      Check(LHeader.Height <= 64, 'compact header occupies one touch-friendly row');
    end;
    GHost.Capture('initial-workspace');
    { Make optional details reachable through the same managed action family.
      A connected project/conflict may already have its disclosure strip. }

    if not GHost.Exists('[data-node=studio-details-summary]') then
    begin
      Menu('project', 'agents');
    end;

    if LCompact then
    begin
      WaitFor('[data-node=studio-details-summary]');

      if GHost.Exists('[data-node=studio-details-split]') then
      begin
        GHost.Click('[data-node=action-details-toggle]');
        WaitFor('[data-node=studio-details-split]', False);
      end;
      LCanvas := GHost.Bounds('[data-node=studio-canvas-wrap]');
      LCollapsedHeight := LCanvas.Height;
      WriteLn('Collapsed canvas / ', LWidth, ' x ', LHeight, ' / ', LCanvas.Height:0:2);
      Check(LCanvas.Height >= LHeight * 0.50,
        'collapsed details retain at least half of a short host for the design');
      GHost.Capture('collapsed-details');
      GHost.Click('[data-node=action-details-toggle]');
      WaitFor('[data-node=studio-details-split] .nyx-split-divider');
      { A deliberately different shared title reproduces the phone's retained
        local-project conflict through semantic authoring, not browser injection. }

      if (GClient <> nil) and
        (LBefore.Field('title').AsText = 'Mobile workspace qualification') then
      begin
        WaitFor('[data-node=studio-details-split] [data-node=action-agent-pause]');
        Check(GHost.Exists('[data-node=studio-details-split] [data-node=action-agent-accept]'),
          'both explicit sync decisions remain reachable without automatic acceptance');
        GHost.Capture('retained-sync-choices');
      end;
      LCanvas := GHost.Bounds('[data-node=studio-canvas-wrap]');
      LExpandedDetailHeight := LCanvas.Height;
      Check((LCanvas.Height > 80) and (LCanvas.Height < LCollapsedHeight),
        'expanded details share bounded space with a useful design');
      GHost.DragTouch('[data-node=studio-details-split] > .nyx-split-divider', 0, -42);
      Sleep(100);
      LCanvas := GHost.Bounds('[data-node=studio-canvas-wrap]');
      Check(LCanvas.Height > LExpandedDetailHeight,
        'actual host touch grip gives space back to the canvas');
      GHost.Capture('resized-details');
      GHost.Click('[data-node=action-details-toggle]');
      WaitFor('[data-node=studio-details-split]', False);
    end;
    { Public expansion is independent of optional panels and source state. }
    GHost.Click('[data-node=action-canvas-expand]');
    WaitFor('[data-node=action-workspace-restore]');
    LCanvas := GHost.Bounds('[data-node=studio-canvas-wrap]');
    WriteLn('Expanded canvas / ', LWidth, ' x ', LHeight, ' / ', LCanvas.Height:0:2);
    Check(LCanvas.Height >= LHeight * 0.85,
      'expanded canvas receives at least eighty-five percent of host height');
    GHost.Capture('expanded-canvas');
    GHost.Click('[data-node=action-workspace-restore]');
    WaitFor('[data-node=action-workspace-restore]', False);

    if LCompact then
    begin
      GHost.Click('[data-node=action-canvas-tools]');
      WaitFor('[data-node=studio-drop-position]:not([style*="display: none"])');
      Check(GHost.Bounds('[data-node=studio-drop-position]').Height > 0,
        'placement tools are reachable on demand');
      GHost.Click('[data-node=action-canvas-tools]');
    end;
    Menu('view', 'code');
    WaitFor('[data-node=studio-code]');
    Check(GHost.TryFieldValue('[data-node=studio-code]', LSource),
      'bounded observation reads the current source pane');
    GHost.Click('[data-node=action-canvas-expand]');
    WaitFor('[data-node=action-workspace-restore]');
    GHost.Click('[data-node=action-workspace-restore]');
    WaitFor('[data-node=studio-code]');
    Check(GHost.Bounds('[data-node=studio-code]').Height > 0,
      'restoring the workspace restores the optional source pane');
    Check(GHost.TryFieldValue('[data-node=studio-code]', LRestoredSource) and
      (LRestoredSource = LSource), 'canvas expansion retains exact source text');
    Menu('view', 'code');
    { Cross the physical host boundary without replacing a designed project. }
    GHost.Resize(1100, 900);
    WaitFor('[data-node=studio-panelbar]', False);
    Check(GHost.Bounds('[data-node=action-build-app]').Height > 0,
      'desktop restores direct toolbar actions');
    GHost.Resize(1100, 450);
    WaitFor('[data-node=studio-panelbar]');
    Check(GHost.Bounds('[data-node=studio-header]').Height <= 64,
      'short landscape host receives compact navigation');
    GHost.Capture('short-landscape');
    GHost.Resize(LWidth, LHeight);

    if GClient <> nil then
    begin
      LAfter := Session;
      for LIndex := 0 to High(CContextFields) do
      begin
        LKey := CContextFields[LIndex];
        Check(LAfter.Field(LKey).ToJSON = LBefore.Field(LKey).ToJSON,
          'chrome allocation preserves semantic ' + LKey);
      end;
      GClient.Close;
      FreeAndNil(GClient);
    end;
    FreeAndNil(GHost);
    WriteLn('PASS ', GChecks, ' actual browser workspace checks / ', LWidth, ' x ', LHeight);
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
