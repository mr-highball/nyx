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


program nyx_studio_deployment_observer;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.data, nyx.studio.projects,
  nyx.resources.workspace, nyx.resources.editor, nyx.test.browser.pipe;

{ Observe the actual preserved primary in an ordinary browser. Trusted input
  joins its existing pair and opens presentation panels only; this fixture never
  authors, compiles, accepts source or modifies a protected design. The external
  deployment checker and guard verify exact durable pairs before and afterward. }

var
  GBrowser: TNyxBrowserPipe;

procedure Pump(ADuration: Integer);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    GBrowser.Attribute('data-nyx-studio-ready');

    if GBrowser.RuntimeError <> '' then
    begin
      GBrowser.CaptureRuntimeError;
      raise Exception.Create('Installed Studio runtime failed; inspect private receipts');
    end;
    Sleep(25);
  until GetTickCount64 - LStarted >= QWord(ADuration);
end;

procedure WaitFace(const ASelector: TNyxText);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if GBrowser.Exists(ASelector) then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Installed observing Studio face did not become ready');
    end;
    Pump(50);
  until False;
end;

{ The optional installed Resources journey exercises public Nyx pane identities
  through trusted presentation input only. Empty user catalogs stay empty: no
  import, proposal, binding, source acceptance or document transaction is made.
  Exact protected pairs are still checked externally before and after this host.
  Bounds establish usable visible pane allocation on this desktop/browser pair,
  separately from hardware, phone, IME and accessibility qualification. }
procedure RequireResourceArea(APane: TNyxResourceWorkspacePane;
  AWidth, AHeight: Integer);
var
  LBox: TNyxBrowserBox;
  LTop: Double;
  LBottom: Double;
begin
  LBox := GBrowser.Bounds('[data-node="' +
    NyxResourceWorkspaceScrollID('studio-resource-workspace', APane) + '"]');
  LTop := LBox.Top;
  LBottom := LBox.Top + LBox.Height;

  if LTop < 0 then
  begin
    LTop := 0;
  end;

  if LBottom > AHeight then
  begin
    LBottom := AHeight;
  end;

  if (LBox.Left < -1) or (LBox.Left + LBox.Width > AWidth + 1) or
    (LBox.Width < 240) or (LBottom - LTop < 140) then
  begin
    GBrowser.Capture('installed-resource-allocation-refusal');
    raise Exception.Create('Installed Resources pane lacks usable visible allocation');
  end;
end;

procedure ObserveResources;
begin
  GBrowser.Click('[data-node="action-resources-toggle"]');
  WaitFace('[data-node="studio-resource-workspace"]');
  Pump(600);
  RequireResourceArea(rwpFiles, 1280, 960);
  RequireResourceArea(rwpEditor, 1280, 960);
  GBrowser.Capture('installed-resources-desktop');
  GBrowser.Resize(390, 844);
  Pump(600);
  GBrowser.Click('[data-node="' +
    NyxResourceWorkspaceActionID('studio-resource-workspace', rwpFiles) + '"]');
  Pump(400);
  RequireResourceArea(rwpFiles, 390, 844);
  GBrowser.Capture('installed-resources-files-narrow');
  GBrowser.Click('[data-node="' +
    NyxResourceWorkspaceActionID('studio-resource-workspace', rwpEditor) + '"]');
  WaitFace('[data-node="' +
    NyxResourceEditorFieldID('studio-resource-editor', refName) + '"]');
  Pump(400);
  RequireResourceArea(rwpEditor, 390, 844);
  GBrowser.Capture('installed-resources-editor-narrow');
  GBrowser.Resize(1280, 960);
  Pump(600);
  GBrowser.Click('[data-node="action-resources-close"]');
  Pump(400);
end;

var
  LFile: TFileStream;
  LText: TNyxText;
  LExpected: TNyxDataValue;
  LPair: TNyxProjectPair;
  LActual: TNyxText;
  LStarted: QWord;
begin
  GBrowser := nil;
  try

    if (ParamCount < 3) or (ParamCount > 4) or
      ((ParamCount = 4) and (ParamStr(4) <> '--resources')) then
    begin
      raise Exception.Create('Supply loopback origin, exact protected pairs, ' +
        'owned capture directory and optional --resources');
    end;
    LFile := TFileStream.Create(ParamStr(2), fmOpenRead or fmShareDenyNone);
    try

      if (LFile.Size < 1) or (LFile.Size > 4 * 1024 * 1024) then
      begin
        raise Exception.Create('Observing baseline exceeds its private byte bound');
      end;
      SetLength(LText, LFile.Size);
      LFile.ReadBuffer(LText[1], Length(LText));
    finally
      LFile.Free;
    end;
    LExpected := TNyxDataValue.ParseJSON(LText).Item(0);
    LPair := DecodeNyxProject(LExpected.Field('project').AsText);
    GBrowser := TNyxBrowserPipe.Create(ParamStr(1) + '/', ParamStr(3), 1280, 960);
    WaitFace('[data-node="action-code"]');
    Pump(1200);

    if GBrowser.Exists('[data-node="action-agent-accept"]') then
    begin
      GBrowser.Click('[data-node="action-agent-accept"]');
      Pump(1200);
    end;
    GBrowser.Click('[data-node="action-code"]');
    WaitFace('[data-node="action-expand-source"]');
    GBrowser.Click('[data-node="action-expand-source"]');
    LStarted := GetTickCount64;
    repeat

      if GBrowser.TryFieldValue('[data-node="studio-code"]', LActual) and
        (LActual = LPair.Source) then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 15000 then
      begin
        GBrowser.Capture('installed-source-refusal');
        raise Exception.Create('Installed ordinary source does not match the exact retained companion');
      end;
      Pump(50);
    until False;
    GBrowser.Capture('installed-source-desktop');
    GBrowser.Click('[data-node="action-expand-source"]');
    GBrowser.Click('[data-node="action-code"]');
    GBrowser.Resize(390, 844);
    Pump(800);
    GBrowser.Capture('installed-primary-narrow');
    GBrowser.Click('[data-node="action-panel-inspector"]');
    Pump(400);
    GBrowser.Capture('installed-inspector-narrow');
    GBrowser.Click('[data-node="action-panel-design"]');
    Pump(400);
    GBrowser.Resize(1280, 960);
    Pump(600);

    if ParamCount = 4 then
    begin
      ObserveResources;
    end;
    GBrowser.Capture('installed-primary-desktop');
    WriteLn('Installed ordinary observer: exact source, modal, narrow panels and desktop return passed');
  finally
    GBrowser.Free;
  end;
end.
