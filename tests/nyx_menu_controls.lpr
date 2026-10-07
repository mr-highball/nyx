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

program nyx_menu_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$IFDEF PAS2JS}{$modeswitch externalclass}{$ENDIF}

uses
  {$IFDEF PAS2JS}JS, Web, nyx.menu.browser, nyx.render.browser,
  {$ELSE}Interfaces, Classes, Forms, Controls, LCLType, LMessages, Graphics,
    IntfGraphics, FPWritePNG,
    nyx.menu.lcl, nyx.render.lcl,{$ENDIF}
  SysUtils, nyx.text, nyx.types, nyx.root.types, nyx.model, nyx.controls,
  nyx.menu, nyx.popover, nyx.behavior, nyx.events, nyx.scheduler, nyx.generated.view;

type
  TCompletion = class(TNyxEventCallback)
  public
    Marker: TNyxText;
    constructor Create(const AMarker: TNyxText);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  TConsume = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  {$IFDEF PAS2JS}
  TKeyboard = class external name 'KeyboardEvent'(TJSKeyboardEvent)
    constructor new(const AKind: String; AOptions: TJSObject);
  end;
  {$ELSE}
  TControlAccess = class(TWinControl);
  {$ENDIF}

var
  GMenu: {$IFDEF PAS2JS}INyxBrowserMenu{$ELSE}INyxLCLMenu{$ENDIF};
  GRenderer: {$IFDEF PAS2JS}TNyxBrowserRenderer{$ELSE}TNyxLCLRenderer{$ENDIF};
  GHost: {$IFDEF PAS2JS}TJSHTMLElement{$ELSE}TForm{$ENDIF};
  GDocument: TNyxDocument;
  GHostDocument: TNyxDocument;
  GPlan: TNyxMenuItems;
  GChecks: Integer;
  GOrder: TNyxText;
  GLast: TNyxMenuInvocation;
  GReopen: Boolean;
  GRelease: Boolean;
  GResume: NativeInt;
  GToken1: INyxEventSubscription;
  GToken2: INyxEventSubscription;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Failure(const AToken: INyxEventSubscription): TNyxText;
begin
  Result := 'not invoked';

  if (AToken <> nil) and (AToken.LastExecution <> nil) then
  begin
    Result := AToken.LastExecution.Failure;
  end;
end;

procedure Cleanup;
begin
  GMenu := nil;
  GToken1 := nil;
  GToken2 := nil;
  FreeAndNil(GRenderer);
  FreeAndNil(GHostDocument);
  FreeAndNil(GDocument);
  {$IFNDEF PAS2JS}FreeAndNil(GHost);{$ENDIF}
end;

constructor TCompletion.Create(const AMarker: TNyxText);
begin
  inherited Create;
  Marker := AMarker;
end;

procedure TCompletion.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  GLast := NyxMenuInvocation(AEvent);
  GOrder := GOrder + Marker;

  if GReopen then
  begin
    GReopen := False;
    GMenu.Open(NyxMenu('Thoughtful actions'));
  end;

  if GRelease then
  begin
    GRelease := False;
    GMenu := nil;
  end;
end;

procedure TConsume.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  NyxEventResponse(AExecution).Consume;
end;

procedure Pump;
begin
  {$IFNDEF PAS2JS}
  Application.ProcessMessages;
  CheckSynchronize;
  {$ENDIF}
end;

function NewMenu(AHasRendererAction: Boolean = False):
  {$IFDEF PAS2JS}INyxBrowserMenu{$ELSE}INyxLCLMenu{$ENDIF};
var
  LCopy: TNyxDocument;
begin
  LCopy := GDocument.Clone;
  try

    if AHasRendererAction then
    begin
      { Negative admission deliberately enriches a clone through the typed public
        contract; the exact MCP companion and caller-owned document stay intact. }
      LCopy.Find('menu-cut').Configure.Action(naToggle).Done;
    end;
    {$IFDEF PAS2JS}
    Result := NewNyxBrowserMenu(GRenderer.FocusFor('open-actions'), LCopy,
      NyxPageRoot('thoughtful-actions'), GPlan);
    {$ELSE}
    Result := NewNyxLCLMenu(GRenderer.FocusFor('open-actions'), LCopy,
      NyxPageRoot('thoughtful-actions'), GPlan);
    {$ENDIF}
  finally
    { Target must own its independent snapshot before the supplied tree retires. }
    LCopy.Free;
  end;
end;

procedure Invoker;
begin
  {$IFDEF PAS2JS}
  NyxFocusWithoutScroll(GRenderer.FocusFor('open-actions'));
  {$ELSE}
  GRenderer.FocusFor('open-actions').SetFocus;
  {$ENDIF}
end;

function FocusedID: TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  {$IFDEF PAS2JS}
  Result := TJSHTMLElement(document.activeElement).getAttribute('data-node');
  {$ELSE}

  if Screen.ActiveControl = GRenderer.FocusFor('open-actions') then
  begin
    Exit('open-actions');
  end;

  if Screen.ActiveControl = GRenderer.FocusFor('after-actions') then
  begin
    Exit('after-actions');
  end;

  if Screen.ActiveControl = GRenderer.FocusFor('before-actions') then
  begin
    Exit('before-actions');
  end;

  if GMenu <> nil then
  begin
    for LIndex := 0 to GPlan.Count - 1 do
    begin

      if (GPlan[LIndex].Kind <> nmiSeparator) and
        (GMenu.Presentation.Renderer.FocusFor(GMenu.Content.Part(GPlan[LIndex].Part).ID) =
          Screen.ActiveControl) then
      begin
        Exit(GMenu.Content.Part(GPlan[LIndex].Part).ID);
      end;
    end;
  end;
  {$ENDIF}
end;

procedure Key(AKey: TNyxKey; AShift: Boolean = False);
{$IFDEF PAS2JS}
var
  LOptions: TJSObject;
  LName: TNyxText;
  LEvent: TKeyboard;
{$ELSE}
var
  LKey: Word;
  LVirtual: Word;
  LShift: TShiftState;
  LFace: TWinControl;
{$ENDIF}
begin
  {$IFDEF PAS2JS}
  case AKey of
    nkDownKey:
      begin
        LName := 'ArrowDown';
      end;
    nkUpKey:
      begin
        LName := 'ArrowUp';
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
  LOptions := TJSObject.new;
  LOptions['key'] := LName;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LOptions['shiftKey'] := AShift;
  LEvent := TKeyboard.new('keydown', LOptions);
  document.activeElement.dispatchEvent(LEvent);
  {$ELSE}
  case AKey of
    nkDownKey:
      begin
        LKey := VK_DOWN;
      end;
    nkUpKey:
      begin
        LKey := VK_UP;
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
  LShift := [];

  if AShift then
  begin
    Include(LShift, ssShift);
  end;
  LFace := Screen.ActiveControl;
  LVirtual := LKey;
  TControlAccess(LFace).OnKeyDown(LFace, LKey, LShift);
  { Release the renderer's pressed-key bookkeeping as a physical key-up does. }
  LKey := LVirtual;

  if (GMenu <> nil) and Assigned(TControlAccess(LFace).OnKeyUp) then
  begin
    TControlAccess(LFace).OnKeyUp(LFace, LKey, LShift);
  end;
  {$ENDIF}
  Pump;
end;

procedure TextKey(const AText: TNyxText);
{$IFDEF PAS2JS}
var
  LOptions: TJSObject;
{$ELSE}
var
  LText: TUTF8Char;
  LFace: TWinControl;
{$ENDIF}
begin
  {$IFDEF PAS2JS}
  LOptions := TJSObject.new;
  LOptions['key'] := AText;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  document.activeElement.dispatchEvent(TKeyboard.new('keydown', LOptions));
  {$ELSE}
  LText := String(AText);
  LFace := Screen.ActiveControl;
  TControlAccess(LFace).OnUTF8KeyPress(LFace, LText);
  {$ENDIF}
  Pump;
end;

procedure Finish; forward;

{$IFNDEF PAS2JS}
{ Capture the live native presentation before completion tears down its controls.
  PaintTo uses the actual owned window; no pixels are synthesized or edited. }
procedure Capture;
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  GMenu.Presentation.Window.Repaint;
  Pump;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := TFPWriterPNG.Create;
  try
    LBitmap.SetSize(GMenu.Presentation.Window.Width, GMenu.Presentation.Window.Height);
    GMenu.Presentation.Window.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(ParamStr(1), LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;
{$ENDIF}

{$IFDEF PAS2JS}
procedure AwaitCapture;
begin

  if document.body.getAttribute('data-capture-observed') = 'thoughtful-actions' then
  begin
    window.clearInterval(GResume);
    try
      Finish;
    except
      on E: Exception do
      begin
        Cleanup;
        document.body.setAttribute('data-menu', 'failed');
        document.body.setAttribute('data-event-error', E.Message);
      end;
    end;
  end;
end;
{$ENDIF}

procedure Finish;
var
  LToken: INyxEventSubscription;
  LRejected: Boolean;
  LOther: {$IFDEF PAS2JS}INyxBrowserMenu{$ELSE}INyxLCLMenu{$ENDIF};
begin
  Key(nkHomeKey);
  LToken := GMenu.Events.OnBeforeKeyDown(NyxControlEvents('menu-cut')).Subscribe(TConsume.Create);
  Key(nkDownKey);
  Check(FocusedID = 'menu-cut', 'Consumed before key keeps menu focus');
  LToken.Cancel;
  LToken := nil;
  Key(nkDownKey);
  Check(FocusedID = 'menu-copy', 'Cancelled before hook restores navigation');
  GOrder := '';
  GReopen := True;
  Key(nkEnterKey);
  Check(GMenu.IsOpen and (GOrder = 'AB'), 'Ordered completion permits reopening');
  Check(GLast.Command.Name = 'copy', 'Reopen does not replace owned command snapshot');
  GMenu.Close;
  Invoker;
  GMenu.Open(NyxMenu('Thoughtful actions').Opening(nmoLast).Wrap(False));
  Check(FocusedID = 'menu-compact', 'Last opening skips hidden action');
  Key(nkDownKey);
  Check(FocusedID = 'menu-compact', 'No-wrap boundary retains last focus');
  GMenu.Close;
  Invoker;
  GMenu.Open(NyxMenu('Thoughtful actions'));
  TextKey('c');
  Check(FocusedID = 'menu-copy', 'Repeated initial starts after current item');
  TextKey('c');
  Check(FocusedID = 'menu-comfortable', 'Repeated initial cycles visible labels');
  TextKey('o');
  Check(FocusedID = 'menu-comfortable', 'Extended prefix keeps matching current item');
  TextKey('p');
  Check(FocusedID = 'menu-copy', 'Extended prefix resolves another item');
  GMenu.Close;
  Invoker;
  GMenu.Open(NyxMenu('Thoughtful actions'));
  Key(nkEscapeKey);
  Check(not GMenu.IsOpen and (FocusedID = 'open-actions'), 'Escape closes and returns actual focus');
  {$IFNDEF PAS2JS}
  GMenu.Open(NyxMenu('Thoughtful actions'));
  Key(nkTabKey);
  Check(not GMenu.IsOpen and (FocusedID = 'after-actions'), 'Native Tab leaves restored invoker forward');
  Invoker;
  GMenu.Open(NyxMenu('Thoughtful actions'));
  Key(nkTabKey, True);
  Check(not GMenu.IsOpen and (FocusedID = 'before-actions'), 'Native Shift Tab leaves restored invoker backward');
  {$ENDIF}
  LOther := NewMenu;
  LOther.Button(NyxPart('cut')).Text := 'Überblick 🌙';
  Invoker;
  LOther.Open(NyxMenu('Unicode qualification').Opening(nmoLast));
  GMenu.Close;
  GMenu := LOther;
  LOther := nil;
  TextKey('ü');
  Check(FocusedID = 'menu-cut', 'Decoded Unicode typeahead uses folded authored text');
  Check(GMenu.Button(NyxPart('cut')).Text = TNyxText('Überblick 🌙'),
    'Specialized public button preserves exact supplementary text');
  GMenu.Close;
  GMenu := NewMenu;
  GMenu.Button(NyxPart('cut')).Configure.Action(naToggle);
  LRejected := False;
  try
    GMenu.Open(NyxMenu('Rejected renderer action'));
  except
    on E: ENyxModel do
    begin
      LRejected := Pos('OnInvoke', E.Message) > 0;
    end;
  end;
  Check(LRejected and not GMenu.IsOpen, 'Late authored action refuses before mounting');
  GMenu.Button(NyxPart('cut')).Configure.Action(naNone);
  GToken1 := GMenu.OnInvoke.Subscribe(TCompletion.Create('A'));
  GToken2 := GMenu.OnInvoke.Subscribe(TCompletion.Create('B'));
  Invoker;
  GMenu.Open(NyxMenu('Thoughtful actions'));
  GRelease := True;
  GOrder := '';
  Key(nkEnterKey);
  Check((GMenu = nil) and (GOrder = 'AB'), 'Callback release preserves later ordered snapshot delivery');
  GToken1 := nil;
  GToken2 := nil;
  Pump;
  Cleanup;
  {$IFDEF PAS2JS}
  document.body.setAttribute('data-menu', 'passed');
  document.body.setAttribute('data-menu-checks', IntToStr(GChecks));
  {$ELSE}
  WriteLn('PASS ', GChecks, ' actual native menu checks');
  {$ENDIF}
end;

procedure Run;
var
  LPage: INyxPage;
  LCopy: TNyxMenuItems;
  LRejected: Boolean;
begin
  GDocument := BuildNyxDocument;
  GPlan := NyxMenuItems
    .Add(NyxMenuAction(NyxPart('cut'), NyxMenuCommand('cut')))
    .Add(NyxMenuAction(NyxPart('copy'), NyxMenuCommand('copy')))
    .Add(NyxMenuAction(NyxPart('paste'), NyxMenuCommand('paste')).Enabled(False))
    .Add(NyxMenuSeparator(NyxPart('separator')))
    .Add(NyxMenuCheck(NyxPart('guides'), NyxMenuCommand('guides'), False))
    .Add(NyxMenuRadio(NyxPart('comfortable'), NyxMenuCommand('comfortable'), NyxMenuGroup('density'), True))
    .Add(NyxMenuRadio(NyxPart('compact'), NyxMenuCommand('compact'), NyxMenuGroup('density'), False))
    .Add(NyxMenuAction(NyxPart('hidden'), NyxMenuCommand('hidden')));
  LCopy := NyxMenuItems.Add(GPlan[0]);
  Check((LCopy.Count = 1) and (GPlan.Count = 8), 'Menu plans retain independent arrays');
  GHostDocument := TNyxDocument.Create;
  LPage := NewNyxPage('menu-workbench');
  LPage.Configure.Padding(24).Gap(12);
  LPage.Add(NewNyxHeading('menu-title').Configure.Text('Thoughtful actions').Done);
  LPage.Add(NewNyxLabel('menu-intent').Configure.Text('A reusable menu, built from ordinary Nyx controls.').Done);
  LPage.Add(NewNyxButton('before-actions').Configure.Text('Previous control').Done);
  LPage.Add(NewNyxButton('open-actions').Configure.Text('Open actions').Done);
  LPage.Add(NewNyxButton('after-actions').Configure.Text('Keep working').Done);
  GHostDocument.AddPage(LPage);
  {$IFDEF PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  GRenderer := TNyxBrowserRenderer.Create;
  GRenderer.Render(GHostDocument, LPage.Node, GHost);
  {$ELSE}
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(40, 40, 900, 780);
  GHost.Show;
  GRenderer := TNyxLCLRenderer.Create;
  GRenderer.Render(GHostDocument, LPage.Node, GHost);
  {$ENDIF}
  LRejected := False;
  try
    NewMenu(True);
  except
    on E: ENyxModel do
    begin
      LRejected := Pos('OnInvoke', E.Message) > 0;
    end;
  end;
  Check(LRejected, 'Renderer default cannot bypass managed menu command admission');
  Pump;
  GMenu := NewMenu;
  GToken1 := GMenu.OnInvoke.Subscribe(TCompletion.Create('A'));
  GToken2 := GMenu.OnInvoke.Subscribe(TCompletion.Create('B'));
  Invoker;
  GMenu.Open(NyxMenu('Thoughtful actions'));
  Check(FocusedID = 'menu-cut', 'First visible menu command receives actual focus');
  Key(nkDownKey);
  Check(FocusedID = 'menu-copy', 'Down moves actual focus');
  Key(nkDownKey);
  Check((FocusedID = 'menu-paste') and (GMenu.Focused.Name = 'paste'), 'Disabled menu command remains focusable');
  Key(nkEnterKey);
  Check(GMenu.IsOpen and (GOrder = ''), 'Disabled command refuses activation');
  Key(nkDownKey);
  Check(FocusedID = 'menu-guides', 'Navigation skips separator');
  Key(nkSpaceKey);
  Check(GMenu.IsOpen and GMenu.Checked(NyxPart('guides')), 'Space checks without closing');
  Check((GOrder = 'AB') and GLast.HasChecked and GLast.Checked,
    'Typed checked snapshot and ordered callbacks / ' + GOrder + ' / ' +
      Failure(GToken1) + ' / ' + Failure(GToken2));
  Key(nkEndKey);
  Check(FocusedID = 'menu-compact', 'End skips hidden item');
  Key(nkSpaceKey);
  Check(GMenu.Checked(NyxPart('compact')) and not GMenu.Checked(NyxPart('comfortable')), 'Radio group selects exclusively');
  Key(nkHomeKey);
  Key(nkUpKey);
  Check(FocusedID = 'menu-compact', 'Wrap skips hidden entries and separators');
  GMenu.SetEnabled(NyxPart('cut'), False);
  Key(nkHomeKey);
  Check(FocusedID = 'menu-cut', 'Runtime disabled first item keeps focus');
  GMenu.SetEnabled(NyxPart('cut'), True);
  {$IFDEF PAS2JS}
  Check(GMenu.Presentation.Element.getAttribute('role') = 'menu', 'Browser menu container role');
  Check(GMenu.Presentation.Renderer.FocusFor('menu-paste').getAttribute('aria-disabled') = 'true', 'Disabled menu semantics');
  Check(GMenu.Presentation.Renderer.FocusFor('menu-guides').getAttribute('aria-checked') = 'true', 'Checked menu semantics');
  document.body.setAttribute('data-capture-checkpoint', 'thoughtful-actions');
  GResume := window.setInterval(@AwaitCapture, 30);
  {$ELSE}
  Capture;
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
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-menu', 'failed');
      document.body.setAttribute('data-event-error', E.Message);
      {$ELSE}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$ENDIF}
      Cleanup;
    end;
  end;
end.
