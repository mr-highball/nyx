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

program nyx_menu_bar_declarations_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$IFDEF PAS2JS}{$modeswitch externalclass}{$ENDIF}

uses
  {$IFDEF PAS2JS}JS, Web, nyx.application.browser, nyx.menu.browser,
    nyx.test.keyboard.browser,
  {$ELSE}Interfaces, Classes, Forms, Controls, StdCtrls, LCLType, LMessages,
    Graphics, FPImage, IntfGraphics, FPWritePNG, nyx.application.lcl, nyx.menu.lcl,
  {$ENDIF}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.menu, nyx.menu.bar,
  nyx.menu.button, nyx.behavior, nyx.events, nyx.scheduler, nyx.generated.view;

type
  TCompletion = class(TNyxEventCallback)
  private
    FMark: TNyxText;
  public
    constructor Create(const AMark: TNyxText);
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$IFNDEF PAS2JS}TControlAccess = class(TWinControl);{$ENDIF}

var
  GApplication: {$IFDEF PAS2JS}TNyxBrowserApplication{$ELSE}TNyxLCLApplication{$ENDIF};
  GDocument: TNyxDocument;
  GBarBindings: INyxMenuBarBindings;
  GBar: INyxMenuBar;
  GFile: INyxMenu;
  GEdit: INyxMenu;
  GTokens: array of INyxEventSubscription;
  GOrder: TNyxText;
  GOrigin: TNyxText;
  GTarget: TNyxText;
  GCommand: TNyxText;
  GChecks: Integer;
  GWire: TNyxText;
  {$IFDEF PAS2JS}GTimer: NativeInt;
  GStage: Integer;{$ENDIF}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

constructor TCompletion.Create(const AMark: TNyxText);
begin
  inherited Create;
  FMark := AMark;
end;

procedure TCompletion.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  GOrder := GOrder + FMark;
  GOrigin := AEvent.OriginID;
  GTarget := AEvent.TargetID;
  GCommand := NyxMenuInvocation(AEvent).Command.Name;
end;

procedure Pump;
begin
  {$IFNDEF PAS2JS}Application.ProcessMessages;{$ENDIF}
end;

function FocusedID: TNyxText;
begin
  {$IFDEF PAS2JS}
  Result := '';

  if document.activeElement <> nil then
  begin
    Result := TJSHTMLElement(document.activeElement).getAttribute('data-node');
  end;
  {$ELSE}
  Result := '';

  if Screen.ActiveControl <> nil then
  begin
    Result := TNyxText(Screen.ActiveControl.Name);
  end;
  { Renderer controls expose authored/runtime IDs through their Hint metadata
    rather than Pascal component names. Exact target identity is authoritative. }

  if Screen.ActiveControl = GApplication.View.FocusFor('before-bar') then
  begin
    Result := 'before-bar';
  end;

  if Screen.ActiveControl = GApplication.View.FocusFor('after-bar') then
  begin
    Result := 'after-bar';
  end;
  {$ENDIF}
end;

procedure Key(AKey: TNyxKey; AShift: Boolean = False);
{$IFDEF PAS2JS}
var
  LName: TNyxText;
  LModifiers: TNyxKeyModifiers;
{$ELSE}
var
  LKey: Word;
  LOriginal: Word;
  LFace: TWinControl;
  LShift: TShiftState;
{$ENDIF}
begin
  {$IFDEF PAS2JS}
  case AKey of
    nkLeftKey:
      begin
        LName := 'ArrowLeft';
      end;
    nkRightKey:
      begin
        LName := 'ArrowRight';
      end;
    nkUpKey:
      begin
        LName := 'ArrowUp';
      end;
    nkDownKey:
      begin
        LName := 'ArrowDown';
      end;
    nkHomeKey:
      begin
        LName := 'Home';
      end;
    nkEndKey:
      begin
        LName := 'End';
      end;
    nkEnterKey:
      begin
        LName := 'Enter';
      end;
    nkSpaceKey:
      begin
        LName := ' ';
      end;
    nkEscapeKey:
      begin
        LName := 'Escape';
      end;
    nkTabKey:
      begin
        LName := 'Tab';
      end;
  else
    raise Exception.Create('Unknown fixture key');
  end;
  LModifiers := [];

  if AShift then
  begin
    Include(LModifiers, nmShift);
  end;
  document.activeElement.dispatchEvent(NyxTestKeyboard(ntKeyDown, LName, LModifiers));
  document.activeElement.dispatchEvent(NyxTestKeyboard(ntKeyUp, LName, LModifiers));
  {$ELSE}
  case AKey of
    nkLeftKey:
      begin
        LKey := VK_LEFT;
      end;
    nkRightKey:
      begin
        LKey := VK_RIGHT;
      end;
    nkUpKey:
      begin
        LKey := VK_UP;
      end;
    nkDownKey:
      begin
        LKey := VK_DOWN;
      end;
    nkHomeKey:
      begin
        LKey := VK_HOME;
      end;
    nkEndKey:
      begin
        LKey := VK_END;
      end;
    nkEnterKey:
      begin
        LKey := VK_RETURN;
      end;
    nkSpaceKey:
      begin
        LKey := VK_SPACE;
      end;
    nkEscapeKey:
      begin
        LKey := VK_ESCAPE;
      end;
    nkTabKey:
      begin
        LKey := VK_TAB;
      end;
  else
    raise Exception.Create('Unknown fixture key');
  end;
  LFace := Screen.ActiveControl;
  LOriginal := LKey;
  LShift := [];

  if AShift then
  begin
    Include(LShift, ssShift);
  end;
  TControlAccess(LFace).OnKeyDown(LFace, LKey, LShift);
  LKey := LOriginal;

  if Assigned(TControlAccess(LFace).OnKeyUp) then
  begin
    TControlAccess(LFace).OnKeyUp(LFace, LKey, LShift);
  end;
  {$ENDIF}
  Pump;
end;

procedure ClickHeading(const AID: TNyxText);
begin
  {$IFDEF PAS2JS}GApplication.View.FocusFor(AID).click;
  {$ELSE}TControlAccess(GApplication.View.FocusFor(AID)).Click;{$ENDIF}
  Pump;
end;

procedure ClickPart(const AMenu: INyxMenu; const APart: TNyxPartRef);
begin
  {$IFDEF PAS2JS}
  (AMenu as INyxBrowserMenu).Presentation.Renderer
    .FocusFor(AMenu.Content.Part(APart).ID).click;
  {$ELSE}
  TControlAccess((AMenu as INyxLCLMenu).Presentation.Renderer
    .FocusFor(AMenu.Content.Part(APart).ID)).Click;
  {$ENDIF}
  Pump;
end;

{ Scope factory-return temporaries before remount/retirement assertions. }
procedure ObserveBindings;
begin
  Check(Supports(GApplication.Menus, INyxMenuBarBindings, GBarBindings),
    'Ordinary application exposes coordinated saved bindings');
  Check((GBarBindings.BarCount = 1) and (GApplication.Menus.Count = 4),
    'Application automatically mounts one bar and four independent families');
  GBar := GBarBindings.Bar(NyxControl('workspace-menu-bar'));
  GFile := GApplication.Menus.Menu(NyxControl('bar-file'));
  GEdit := GApplication.Menus.Menu(NyxControl('bar-edit'));
end;

procedure ReleaseObservations;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(GTokens) do
  begin
    GTokens[LIndex].Cancel;
  end;
  GTokens := nil;
  GEdit := nil;
  GFile := nil;
  GBar := nil;
  GBarBindings := nil;
end;

procedure Cleanup;
begin
  {$IFDEF PAS2JS}

  if GTimer <> 0 then
  begin
    window.clearInterval(GTimer);
    GTimer := 0;
  end;
  {$ENDIF}
  ReleaseObservations;
  FreeAndNil(GApplication);
  FreeAndNil(GDocument);
end;

procedure Finish;
begin
  Check(GWire = TNyxCodec.Encode(GDocument),
    'Navigation, completion and runtime checks never rewrite saved defaults');
  ReleaseObservations;
  GApplication.ShowPage('menu-workspace');
  Pump;
  ObserveBindings;
  Check(not GEdit.Checked(NyxPart('guides')), 'Remount creates a fresh independent saved family');
  ClickHeading('bar-file');
  Check(GFile.IsOpen, 'Retired coordinator leaves no stale registration on the new view');
  Cleanup;
  {$IFDEF PAS2JS}
  document.body.setAttribute('data-menu-bar', 'passed');
  document.body.setAttribute('data-menu-bar-checks', IntToStr(GChecks));
  {$ELSE}
  WriteLn('PASS ', GChecks, ' automatically bound native menu bar checks');
  {$ENDIF}
end;

{$IFNDEF PAS2JS}
procedure Capture;
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LWriter := TFPWriterPNG.Create;
  LImage := nil;
  try
    GApplication.Window.Repaint;
    LBitmap.SetSize(GApplication.Window.Width, GApplication.Window.Height);
    GApplication.Window.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(ParamStr(1), LWriter);
  finally
    LImage.Free;
    LWriter.Free;
    LBitmap.Free;
  end;
end;
{$ELSE}
procedure Advance;
begin
  try
    case GStage of
      0:
        begin

          if document.body.getAttribute('data-capture-observed') <> 'menu-bar-family' then
          begin
            Exit;
          end;
          GBar.Close;
          ClickHeading('bar-file');
          document.body.setAttribute('data-host-tab-request', 'forward');
          GStage := 1;
        end;
      1:
        begin

          if document.body.getAttribute('data-host-tab-observed') <> 'forward' then
          begin
            Exit;
          end;
          Check(FocusedID = 'after-bar', 'Actual browser Tab leaves the whole saved group');
          ClickHeading('bar-file');
          document.body.setAttribute('data-host-tab-request', 'backward');
          GStage := 2;
        end;
      2:
        begin

          if document.body.getAttribute('data-host-tab-observed') <> 'backward' then
          begin
            Exit;
          end;
          Check(FocusedID = 'before-bar', 'Actual browser Shift+Tab leaves before the saved group');
          Finish;
        end;
    end;
  except
    on E: Exception do
    begin
      Cleanup;
      document.body.setAttribute('data-menu-bar', 'failed');
      document.body.setAttribute('data-event-error', E.Message);
    end;
  end;
end;
{$ENDIF}

procedure Run;
{$IFDEF PAS2JS}
var
  LHost: TJSHTMLElement;
{$ENDIF}
begin
  { Compile the exact source admitted by the semantic declaration journey.
    No handwritten recipe, Menu.Open, BindMenus or bar factory substitutes for
    ordinary application mounting and actual renderer input here. }
  GDocument := BuildNyxDocument;
  GWire := TNyxCodec.Encode(GDocument);
  Check(GDocument.HasMenuBars, 'Exact compiled source owns its saved grouping');
  {$IFDEF PAS2JS}
  GApplication := TNyxBrowserApplication.Create;
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  GApplication.Run(GDocument, LHost);
  {$ELSE}
  GApplication := TNyxLCLApplication.Create;
  GApplication.Mount(GDocument);
  GApplication.Window.SetBounds(40, 40, 900, 760);
  GApplication.Window.Show;
  {$ENDIF}
  GApplication.ShowPage('menu-workspace');
  Pump;
  ObserveBindings;
  Check(not GDocument.Find('workspace-menu-bar').MenuBar.Options.Wraps,
    'Compiled generated source retains the nondefault typed bar policy');
  SetLength(GTokens, 2);
  GTokens[0] := GApplication.View.Events.OnNamed(NyxControlEvents('workspace-menu-bar'),
    NyxSemantic(nseActivate)).Subscribe(TCompletion.Create('A'));
  GTokens[1] := GApplication.View.Events.OnNamed(NyxControlEvents('workspace-menu-bar'),
    NyxSemantic(nseActivate)).Subscribe(TCompletion.Create('B'));
  ClickHeading('bar-file');
  Check(GFile.IsOpen and (GFile.Focused.Name = 'cut'),
    'Mounted heading opens its saved first item / open=' + BoolToStr(GFile.IsOpen, True) +
      ' / bar=' + BoolToStr(GBar.IsOpen, True) + ' / focused=' + GFile.Focused.Name);
  ClickHeading('bar-file');
  Check(not GBar.IsOpen, 'A second ordinary heading click closes its whole family');
  ClickHeading('bar-file');
  Check(GFile.IsOpen, 'Ordinary heading activation reopens after closing');
  Key(nkRightKey);
  Check(GEdit.IsOpen and not GFile.IsOpen,
    'Physical root-leaf Right switches the automatically bound heading');
  ClickPart(GEdit, NyxPart('guides'));
  Check((GOrder = 'AB') and (GOrigin = 'bar-edit') and
    (GTarget = 'workspace-menu-bar') and (GCommand = 'show-guides'),
    'Saved row completion reaches ordered ordinary callbacks with exact heading origin');
  Check(GEdit.Checked(NyxPart('guides')) and not GFile.Checked(NyxPart('guides')),
    'A reused declaration gives each mounted heading independent check state');
  ClickHeading('bar-file');
  ClickPart(GFile, NyxPart('appearance'));
  ClickPart(GFile.Submenu(NyxPart('appearance')), NyxPart('density'));
  Check(GFile.Submenu(NyxPart('appearance')).Submenu(NyxPart('density')).IsOpen,
    'Saved transitive submenu references mount a real three-level family');
  {$IFDEF PAS2JS}
  document.body.setAttribute('data-capture-checkpoint', 'menu-bar-family');
  GTimer := window.setInterval(@Advance, 30);
  {$ELSE}
  Capture;
  GBar.Close;
  ClickHeading('bar-file');
  Key(nkTabKey);
  Check(FocusedID = 'after-bar', 'Native dialog Tab leaves the whole saved group');
  ClickHeading('bar-file');
  Key(nkTabKey, True);
  Check(FocusedID = 'before-bar', 'Native dialog Shift+Tab leaves before the saved group');
  Finish;
  {$ENDIF}
end;

begin
  {$IFNDEF PAS2JS}Application.Initialize;{$ENDIF}
  try
    Run;
  except
    on E: Exception do
    begin
      Cleanup;
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-menu-bar', 'failed');
      document.body.setAttribute('data-event-error', E.Message);
      {$ELSE}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$ENDIF}
    end;
  end;
end.
