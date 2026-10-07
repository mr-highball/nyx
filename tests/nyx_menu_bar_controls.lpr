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

program nyx_menu_bar_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$IFDEF PAS2JS}{$modeswitch externalclass}{$ENDIF}

uses
  {$IFDEF PAS2JS}
  JS, Web, nyx.menu.browser, nyx.render.browser, nyx.menu.bar.browser,
  nyx.test.keyboard.browser,
  {$ELSE}
  Interfaces, Classes, Forms, Controls, LCLType, Graphics, IntfGraphics, FPWritePNG,
  nyx.menu.lcl, nyx.render.lcl, nyx.menu.bar.lcl,
  {$ENDIF}
  SysUtils, nyx.text, nyx.types, nyx.root.types, nyx.model, nyx.controls,
  nyx.menu, nyx.menu.bar, nyx.popover, nyx.behavior, nyx.events, nyx.scheduler,
  nyx.errors, nyx.typeahead, nyx.generated.view;

type
  TCompletion = class(TNyxEventCallback)
  private
    FMarker: TNyxText;
  public
    constructor Create(const AMarker: TNyxText);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  TConsume = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  TForeignNavigator = class(TInterfacedObject, INyxMenuFamilyNavigator)
  public
    procedure Navigate(ADirection: TNyxMenuFamilyDirection;
      const AExecution: INyxExecution);
  end;
  {$IFDEF PAS2JS}
  TPointer = class external name 'PointerEvent'(TJSPointerEvent)
    constructor new(const AKind: String; AOptions: TJSObject); reintroduce;
  end;
  {$ELSE}
  TControlAccess = class(TWinControl);
  {$ENDIF}

var
  GDocument: TNyxDocument;
  GRenderer: {$IFDEF PAS2JS}TNyxBrowserRenderer{$ELSE}TNyxLCLRenderer{$ENDIF};
  GHost: {$IFDEF PAS2JS}TJSHTMLElement{$ELSE}TForm{$ENDIF};
  GBar: INyxMenuBar;
  GMenus: array of INyxMenu;
  GChild: INyxMenu;
  GLeaf: INyxMenu;
  GChecks: Integer;
  GOrder: TNyxText;
  GOrigin: TNyxText;
  GLast: TNyxMenuInvocation;
  GRelease: Boolean;
  GForeignRequests: Integer;
  {$IFDEF PAS2JS}
  GTimer: NativeInt;
  GStage: Integer;
  {$ENDIF}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Pump;
begin
  {$IFNDEF PAS2JS}
  Application.ProcessMessages;
  CheckSynchronize;
  {$ENDIF}
end;

procedure Cleanup;
begin
  GBar := nil;
  GLeaf := nil;
  GChild := nil;
  GMenus := nil;
  FreeAndNil(GRenderer);
  FreeAndNil(GDocument);
  {$IFNDEF PAS2JS}FreeAndNil(GHost);{$ENDIF}
end;

constructor TCompletion.Create(const AMarker: TNyxText);
begin
  inherited Create;
  FMarker := AMarker;
end;

procedure TCompletion.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  GLast := NyxMenuInvocation(AEvent);
  GOrigin := AEvent.OriginID;
  GOrder := GOrder + FMarker;

  if GRelease then
  begin
    GRelease := False;
    GBar := nil;
  end;
end;

procedure TConsume.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  NyxEventResponse(AExecution).Consume;
end;

procedure TForeignNavigator.Navigate(ADirection: TNyxMenuFamilyDirection;
  const AExecution: INyxExecution);
begin
  Inc(GForeignRequests);
  NyxEventResponse(AExecution).Consume;
end;

function FocusedID: TNyxText;
{$IFNDEF PAS2JS}
var
  LIndex: Integer;
  LMenus: array of INyxMenu;
  LMenu: INyxLCLMenu;
  LPart: TNyxPartRef;
  LNames: array[0..5] of TNyxText;
{$ENDIF}
begin
  Result := '';
  {$IFDEF PAS2JS}
  Result := TJSHTMLElement(document.activeElement).getAttribute('data-node');
  {$ELSE}
  SetLength(LMenus, Length(GMenus) + 2);
  LMenus[0] := GLeaf;
  LMenus[1] := GChild;
  for LIndex := 0 to High(GMenus) do
  begin
    LMenus[LIndex + 2] := GMenus[LIndex];
  end;
  for LIndex := 0 to High(LMenus) do
  begin

    if (LMenus[LIndex] <> nil) and LMenus[LIndex].IsOpen then
    begin
      LMenu := LMenus[LIndex] as INyxLCLMenu;
      LPart := LMenu.Focused;

      if (LPart.Name <> '') and
        (LMenu.Presentation.Renderer.FocusFor(LMenu.Content.Part(LPart).ID) =
          Screen.ActiveControl) then
      begin
        Exit(LMenu.Content.Part(LPart).ID);
      end;
    end;
  end;
  LNames[0] := 'before-bar';
  LNames[1] := 'bar-file';
  LNames[2] := 'bar-edit';
  LNames[3] := 'bar-hidden';
  LNames[4] := 'bar-view';
  LNames[5] := 'after-bar';
  for LIndex := 0 to High(LNames) do
  begin

    if GRenderer.FocusFor(LNames[LIndex]) = Screen.ActiveControl then
    begin
      Exit(LNames[LIndex]);
    end;
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

procedure TextKey(const AText: TNyxText);
{$IFNDEF PAS2JS}
var
  LText: TUTF8Char;
  LFace: TWinControl;
{$ENDIF}
begin
  {$IFDEF PAS2JS}
  document.activeElement.dispatchEvent(NyxTestKeyboard(ntKeyDown, AText));
  {$ELSE}
  LText := String(AText);
  LFace := Screen.ActiveControl;
  TControlAccess(LFace).OnUTF8KeyPress(LFace, LText);
  {$ENDIF}
  Pump;
end;

procedure Hover(const AID: TNyxText; ATouch: Boolean = False);
{$IFDEF PAS2JS}
var
  LOptions: TJSObject;
{$ELSE}
var
  LFace: TWinControl;
{$ENDIF}
begin
  {$IFDEF PAS2JS}
  LOptions := TJSObject.new;
  LOptions['bubbles'] := False;
  LOptions['pointerType'] := 'mouse';

  if ATouch then
  begin
    LOptions['pointerType'] := 'touch';
  end;
  GRenderer.FocusFor(AID).dispatchEvent(TPointer.new('pointerenter', LOptions));
  {$ELSE}
  LFace := GRenderer.FocusFor(AID);

  if Assigned(TControlAccess(LFace).OnMouseEnter) then
  begin
    TControlAccess(LFace).OnMouseEnter(LFace);
  end;
  {$ENDIF}
  Pump;
end;

procedure BindBar;
var
  LRow: INyxRow;
  LRejected: Boolean;
  LStream: INyxEventStream;
  LFamily: INyxMenuFamilyInput;
  LToken: INyxMenuFamilyRegistration;
begin
  LRow := RetainNyxControl(GRenderer.Root.Find('workspace-menu-bar')) as INyxRow;
  {$IFDEF PAS2JS}
  GBar := NewNyxBrowserMenuBar(LRow, GRenderer, NyxMenuBar('Workspace commands'));
  {$ELSE}
  GBar := NewNyxLCLMenuBar(LRow, GRenderer, NyxMenuBar('Workspace commands'));
  {$ENDIF}
  LRejected := False;
  try
    GBar.Add(NyxPart('file'), GMenus[1], NyxMenu('Wrong anchor'));
  except
    on E: ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (GBar.Count = 0),
    'A family anchored to another heading refuses before registration');
  GBar.Add(NyxPart('file'), GMenus[0], NyxMenu('File'));
  GBar.Add(NyxPart('edit'), GMenus[1], NyxMenu('Edit'));
  GBar.Add(NyxPart('hidden'), GMenus[2], NyxMenu('Archive'));
  LStream := GRenderer.Events.OnKeyDown(NyxControlEvents('bar-view'));
  LStream.Policy(neUIQueue);
  LRejected := False;
  try
    GBar.Add(NyxPart('view'), GMenus[3], NyxMenu('View'));
  except
    on E: ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (GBar.Count = 3) and (LStream.Count = 0) and
    (LStream.ExecutionPolicy = neUIQueue),
    'Nonsequential heading input refuses without changing policy or subscriptions');
  LStream.Policy(neSequential);
  Check(Supports(GMenus[3], INyxMenuFamilyInput, LFamily),
    'Built-in menu exposes its optional typed family navigation extension');
  LToken := LFamily.ConnectNavigation(TForeignNavigator.Create);
  try
    LRejected := False;
    try
      GBar.Add(NyxPart('view'), GMenus[3], NyxMenu('View'));
    except
      on E: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (GBar.Count = 3) and (LStream.Count = 0),
      'Foreign family coordinator refuses without partial heading registration');
    GMenus[3].Open(NyxMenu('Independent view'));
    Key(nkRightKey);
    Check(GForeignRequests = 1, 'Refused binding preserves the exact foreign family coordinator');
    GMenus[3].Close;
  finally
    LToken.Cancel;
  end;
  GBar.Add(NyxPart('view'), GMenus[3], NyxMenu('View'));
  GBar.OnInvoke.Subscribe(TCompletion.Create('A'));
  GBar.OnInvoke.Subscribe(TCompletion.Create('B'));
end;

procedure OpenFamily;
begin
  GBar.Open(NyxPart('file'), nmoLast);
  Key(nkRightKey);
  GChild := GMenus[0].Submenu(NyxPart('appearance'));
  Key(nkDownKey);
  Key(nkRightKey);
  GLeaf := GChild.Submenu(NyxPart('density'));
  Check(GMenus[0].IsOpen and GChild.IsOpen and GLeaf.IsOpen,
    'Bar opens a three-level independent menu family');
end;

procedure ChangePolicy(const AOptions: TNyxMenuBarOptions);
begin
  { End fluent interface temporaries before retirement checks on native FPC. }
  GBar.Policy(AOptions);
end;

procedure CheckRebinding;
var
  LRow: INyxRow;
  LOwner: INyxMenuBar;
begin
  LRow := RetainNyxControl(GRenderer.Root.Find('workspace-menu-bar')) as INyxRow;
  {$IFDEF PAS2JS}
  LOwner := NewNyxBrowserMenuBar(LRow, GRenderer, NyxMenuBar('Reused workspace'));
  {$ELSE}
  LOwner := NewNyxLCLMenuBar(LRow, GRenderer, NyxMenuBar('Reused workspace'));
  {$ENDIF}
  LOwner.Add(NyxPart('file'), GMenus[0], NyxMenu('File'));
  Check(LOwner.Count = 1, 'Retired weak family/row leases permit independent rebinding');
end;

procedure Finish;
var
  LCount: Integer;
begin
  GBar.Focus(NyxPart('edit'));
  Key(nkDownKey);
  Key(nkHomeKey);
  Key(nkDownKey);
  Key(nkDownKey);
  Key(nkDownKey);
  GOrder := '';
  Key(nkSpaceKey);
  Check((GOrder = 'AB') and (GOrigin = 'bar-edit') and
    (GLast.Command.Name = 'guides') and GLast.HasChecked and GLast.Checked,
    'Ordered bar completions retain exact heading and typed checked snapshot');
  Check(GMenus[1].IsOpen and not GMenus[0].Checked(NyxPart('guides')),
    'Runtime check state is independent across sibling families');
  Key(nkHomeKey);
  GOrder := '';
  GRelease := True;
  Key(nkEnterKey);
  Check((GBar = nil) and (GOrder = 'AB') and not GMenus[1].IsOpen,
    'Ordered callbacks safely release their bar after family completion');
  LCount := GRenderer.Events.OnKeyDown(NyxControlEvents('bar-file')).Count;
  Check(LCount = 0, 'Bar retirement cancels mounted heading input subscriptions');
  {$IFDEF PAS2JS}
  Check(GRenderer.ElementFor('workspace-menu-bar').getAttribute('role') <> 'menubar',
    'Bar retirement restores previous browser row semantics');
  Check(GRenderer.FocusFor('bar-file').getAttribute('role') <> 'menuitem',
    'Bar retirement restores previous heading semantics');
  {$ELSE}
  Check(GRenderer.ControlFor('workspace-menu-bar').AccessibleRole <> larMenuBar,
    'Bar retirement restores previous native row semantics');
  Check(GRenderer.FocusFor('bar-file').AccessibleRole <> larMenuItem,
    'Bar retirement restores previous native heading semantics');
  {$ENDIF}
  CheckRebinding;
  Cleanup;
  {$IFDEF PAS2JS}
  document.body.setAttribute('data-menu-bar', 'passed');
  document.body.setAttribute('data-menu-bar-checks', IntToStr(GChecks));
  {$ELSE}
  WriteLn('PASS ', GChecks, ' actual native menu bar checks');
  {$ENDIF}
end;

{$IFDEF PAS2JS}
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
          GStage := 1;
          document.body.setAttribute('data-host-tab-request', 'forward');
        end;
      1:
        begin

          if document.body.getAttribute('data-host-tab-observed') <> 'forward' then
          begin
            Exit;
          end;
          Check(not GBar.IsOpen and (FocusedID = 'after-bar'),
            'Real Chromium Tab exits the complete family after the persistent bar');
          OpenFamily;
          GStage := 2;
          document.body.setAttribute('data-host-tab-request', 'backward');
        end;
      2:
        begin

          if document.body.getAttribute('data-host-tab-observed') <> 'backward' then
          begin
            Exit;
          end;
          Check(not GBar.IsOpen and (FocusedID = 'before-bar'),
            'Real Chromium Shift Tab exits the complete family before the bar');
          window.clearInterval(GTimer);
          Finish;
        end;
    end;
  except
    on E: Exception do
    begin
      window.clearInterval(GTimer);
      Cleanup;
      document.body.setAttribute('data-menu-bar', 'failed');
      document.body.setAttribute('data-event-error', E.Message);
    end;
  end;
end;
{$ELSE}
procedure Capture;
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LMenu: INyxLCLMenu;
begin
  LMenu := GMenus[0] as INyxLCLMenu;
  LBitmap := TBitmap.Create;
  LWriter := TFPWriterPNG.Create;
  LImage := nil;
  try
    GHost.Repaint;
    LBitmap.SetSize(GHost.Width, GHost.Height);
    GHost.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(ParamStr(1), LWriter);
    FreeAndNil(LImage);
    LBitmap.SetSize(LMenu.Presentation.Window.Width, LMenu.Presentation.Window.Height);
    LMenu.Presentation.Window.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(ChangeFileExt(ParamStr(1), '.menu.png'), LWriter);
  finally
    LImage.Free;
    LWriter.Free;
    LBitmap.Free;
  end;
end;
{$ENDIF}

procedure Run;
var
  LPlan: TNyxMenuItems;
  LAppearance: INyxMenuRecipe;
  LDensity: INyxMenuRecipe;
  LIndex: Integer;
  LNames: array[0..3] of TNyxText;
  LRejected: Boolean;
  LBefore: INyxEventSubscription;
  LRow: INyxRow;
  LBad: INyxMenuBar;
  LExpectedRole: {$IFDEF PAS2JS}TNyxText{$ELSE}TLazAccessibilityRole{$ENDIF};
begin
  GDocument := BuildNyxDocument;
  {$IFDEF PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  GRenderer := TNyxBrowserRenderer.Create;
  GRenderer.Render(GDocument, GDocument.Find('menu-workspace'), GHost);
  {$ELSE}
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(40, 40, 900, 760);
  GHost.Show;
  GRenderer := TNyxLCLRenderer.Create;
  GRenderer.Render(GDocument, GDocument.Find('menu-workspace'), GHost);
  {$ENDIF}
  Pump;
  LDensity := NewNyxMenuRecipe(GDocument, NyxPageRoot('density-options'), NyxMenuItems
    .Add(NyxMenuRadio(NyxPart('comfortable'), NyxMenuCommand('comfortable'),
      NyxMenuGroup('density'), True))
    .Add(NyxMenuRadio(NyxPart('compact'), NyxMenuCommand('compact'),
      NyxMenuGroup('density'), False)));
  LAppearance := NewNyxMenuRecipe(GDocument, NyxPageRoot('appearance-options'),
    NyxMenuItems.Add(NyxMenuCheck(NyxPart('guides'), NyxMenuCommand('grid'), False))
      .Add(NyxMenuSubmenu(NyxPart('density'), LDensity)));
  LPlan := NyxMenuItems
    .Add(NyxMenuAction(NyxPart('cut'), NyxMenuCommand('cut')))
    .Add(NyxMenuAction(NyxPart('copy'), NyxMenuCommand('copy')))
    .Add(NyxMenuAction(NyxPart('paste'), NyxMenuCommand('paste')).Enabled(False))
    .Add(NyxMenuSeparator(NyxPart('separator')))
    .Add(NyxMenuCheck(NyxPart('guides'), NyxMenuCommand('guides'), False))
    .Add(NyxMenuRadio(NyxPart('comfortable'), NyxMenuCommand('comfortable'),
      NyxMenuGroup('density'), True))
    .Add(NyxMenuRadio(NyxPart('compact'), NyxMenuCommand('compact'),
      NyxMenuGroup('density'), False))
    .Add(NyxMenuAction(NyxPart('hidden'), NyxMenuCommand('archive')))
    .Add(NyxMenuSubmenu(NyxPart('appearance'), LAppearance));
  LNames[0] := 'bar-file';
  LNames[1] := 'bar-edit';
  LNames[2] := 'bar-hidden';
  LNames[3] := 'bar-view';
  SetLength(GMenus, 4);
  for LIndex := 0 to High(GMenus) do
  begin
    {$IFDEF PAS2JS}
    GMenus[LIndex] := NewNyxBrowserMenu(GRenderer.FocusFor(LNames[LIndex]),
      GDocument, NyxPageRoot('thoughtful-actions'), LPlan);
    {$ELSE}
    GMenus[LIndex] := NewNyxLCLMenu(GRenderer.FocusFor(LNames[LIndex]),
      GDocument, NyxPageRoot('thoughtful-actions'), LPlan);
    {$ENDIF}
  end;
  BindBar;
  Check(GBar.Count = 4, 'Typed bar binds four independent named families');
  LRejected := False;
  LRow := GBar.Content;
  {$IFDEF PAS2JS}
  LExpectedRole := GRenderer.ElementFor(LRow.ID).getAttribute('role');
  {$ELSE}
  LExpectedRole := GRenderer.ControlFor(LRow.ID).AccessibleRole;
  {$ENDIF}
  try
    {$IFDEF PAS2JS}
    LBad := NewNyxBrowserMenuBar(LRow, GRenderer, NyxMenuBar('Duplicate'));
    {$ELSE}
    LBad := NewNyxLCLMenuBar(LRow, GRenderer, NyxMenuBar('Duplicate'));
    {$ENDIF}
  except
    on E: ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LBad = nil), 'Duplicate exact-row coordinator refuses before decoration');
  {$IFDEF PAS2JS}
  Check(GRenderer.ElementFor(LRow.ID).getAttribute('role') = LExpectedRole,
    'Refused duplicate preserves the original accessible bar');
  {$ELSE}
  Check(GRenderer.ControlFor(LRow.ID).AccessibleRole = LExpectedRole,
    'Refused duplicate preserves native bar semantics');
  {$ENDIF}
  LRejected := False;
  try
    GBar.Add(NyxPart('file'), GMenus[1], NyxMenu('Duplicate'));
  except
    on E: ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (GBar.Count = 4), 'Duplicate heading/family preserves all accepted bindings');
  LRejected := False;
  try
    ChangePolicy(Default(TNyxMenuBarOptions));
  except
    on E: ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (GBar.Count = 4), 'Undefined typed policy preserves accepted bar bindings');
  GBar.Focus(NyxPart('file'));
  Key(nkRightKey);
  Check(FocusedID = 'bar-edit', 'Right moves real heading focus');
  Key(nkRightKey);
  Check(FocusedID = 'bar-view', 'Right skips the hidden heading');
  Key(nkRightKey);
  Check(FocusedID = 'bar-file', 'Arrow traversal wraps');
  ChangePolicy(NyxMenuBar('Workspace commands').Wrap(False));
  Key(nkLeftKey);
  Check(FocusedID = 'bar-file', 'Nonwrapping boundary retains focus');
  Key(nkEndKey);
  Check(FocusedID = 'bar-view', 'End selects the last visible heading');
  Key(nkHomeKey);
  Check(FocusedID = 'bar-file', 'Home selects the first heading');
  ChangePolicy(NyxMenuBar('Workspace commands'));
  GBar.SetEnabled(NyxPart('edit'), False);
  Key(nkRightKey);
  Key(nkDownKey);
  Check((FocusedID = 'bar-edit') and not GBar.IsOpen,
    'Logical disabled heading retains navigation focus and refuses opening');
  GBar.SetEnabled(NyxPart('edit'), True);
  TextKey('v');
  Check(FocusedID = 'bar-view', 'Shared Unicode typeahead selects a heading');
  (GBar.Content.Part(NyxPart('edit')) as INyxButton).Text := 'Édit 🧭';
  GBar.Focus(NyxPart('file'));
  TextKey('é');
  Check(FocusedID = 'bar-edit', 'Decoded Unicode heading search retains accented text');
  (GBar.Content.Part(NyxPart('edit')) as INyxButton).Text := 'Edit';
  GBar.Focus(NyxPart('file'));
  LBefore := GRenderer.Events.OnBeforeKeyDown(NyxControlEvents('bar-file'))
    .Subscribe(TConsume.Create);
  Key(nkDownKey);
  Check(not GBar.IsOpen, 'Before-consumed heading key cannot open a family');
  LBefore.Cancel;
  LBefore := nil;
  Key(nkDownKey);
  Check(GMenus[0].IsOpen and (FocusedID = 'menu-cut'), 'Down opens first real dropdown command');
  Key(nkRightKey);
  Check(not GMenus[0].IsOpen and GMenus[1].IsOpen and (FocusedID = 'menu-cut'),
    'Right from a leaf switches independent top-level families');
  Key(nkLeftKey);
  Check(GMenus[0].IsOpen and not GMenus[1].IsOpen,
    'Left from a root dropdown switches to the previous heading');
  Hover('bar-view');
  Check(GMenus[3].IsOpen and not GMenus[0].IsOpen, 'Mouse hover switches an open family');
  {$IFDEF PAS2JS}
  Hover('bar-edit', True);
  Check(GMenus[3].IsOpen and not GMenus[1].IsOpen, 'Touch entry never acts as mouse hover');
  {$ENDIF}
  ChangePolicy(NyxMenuBar('Workspace commands').HoverSwitch(False));
  Hover('bar-edit');
  Check(GMenus[3].IsOpen, 'Typed policy can disable hover switching');
  ChangePolicy(NyxMenuBar('Workspace commands'));
  GBar.Close;
  GBar.Focus(NyxPart('file'));
  Key(nkUpKey);
  Check(FocusedID = 'menu-appearance', 'Up opens the last visible dropdown command');
  Key(nkRightKey);
  GChild := GMenus[0].Submenu(NyxPart('appearance'));
  Check(GChild.IsOpen and (FocusedID = 'appearance-guides'), 'Submenu Right retains local hierarchy');
  Key(nkLeftKey);
  Check(GMenus[0].IsOpen and not GChild.IsOpen and (FocusedID = 'menu-appearance'),
    'Nested Left returns one level without switching the bar');
  Key(nkEscapeKey);
  Check(not GBar.IsOpen and (FocusedID = 'bar-file'), 'Escape restores the invoking heading');
  OpenFamily;
  Key(nkRightKey);
  Check(GMenus[1].IsOpen and not GMenus[0].IsOpen and not GChild.IsOpen and not GLeaf.IsOpen,
    'Right from a nested leaf closes all ancestors and switches the bar');
  GBar.Close;
  OpenFamily;
  {$IFDEF PAS2JS}
  Check(GRenderer.ElementFor('workspace-menu-bar').getAttribute('role') = 'menubar',
    'Browser row exposes the persistent menu-bar role');
  Check(GRenderer.FocusFor('bar-file').getAttribute('tabindex') = '0',
    'Only the selected heading is a browser Tab stop');
  Check(GRenderer.FocusFor('bar-edit').getAttribute('tabindex') = '-1',
    'Other headings stay outside the browser Tab sequence');
  document.body.setAttribute('data-capture-checkpoint', 'menu-bar-family');
  GTimer := window.setInterval(@Advance, 30);
  {$ELSE}
  Check(GRenderer.ControlFor('workspace-menu-bar').AccessibleRole = larMenuBar,
    'Native row exposes the menu-bar accessibility role');
  Check(GRenderer.FocusFor('bar-file').TabStop and not GRenderer.FocusFor('bar-edit').TabStop,
    'Native roving Tab stop excludes sibling headings');
  Capture;
  Key(nkTabKey);
  Check(not GBar.IsOpen and (FocusedID = 'after-bar'), 'Native Tab exits the complete bar family');
  OpenFamily;
  Key(nkTabKey, True);
  Check(not GBar.IsOpen and (FocusedID = 'before-bar'), 'Native Shift Tab exits the bar backward');
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
