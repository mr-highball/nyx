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

program nyx_popover_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$IFDEF PAS2JS}{$modeswitch externalclass}{$ENDIF}

uses
  {$IFDEF PAS2JS}JS, Web, nyx.popover.browser, nyx.render.browser,
  {$ELSE}Interfaces, Classes, Forms, Controls, StdCtrls, LCLType, LCLIntf, LMessages, Types,
    Graphics, IntfGraphics, FPWritePNG, nyx.popover.lcl, nyx.render.lcl,{$ENDIF}
  SysUtils, nyx.text, nyx.types, nyx.root.types, nyx.controls, nyx.model,
  nyx.state, nyx.data, nyx.events, nyx.behavior, nyx.scheduler, nyx.popover,
  nyx.generated.view;

type
  { Completion receivers own snapshots, never widgets or their presenter.
    Raw global observation is the fixture owner, cancelled during cleanup. }
  TCompletion = class(TNyxEventCallback)
  public
    Marker: Integer;
    constructor Create(AMarker: Integer);
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$IFDEF PAS2JS}
  TKeyboard = class external name 'KeyboardEvent'(TJSKeyboardEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  TPointer = class external name 'PointerEvent'(TJSPointerEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  {$ELSE}
  TControlAccess = class(TWinControl);
  {$ENDIF}

var
  GDocument: TNyxDocument;
  GBackground: TNyxDocument;
  GInvoker: INyxButton;
  GOutside: INyxButton;
  GRetained: INyxControl;
  GOrder: TNyxText;
  GChecks: Integer;
  GReopen: Boolean;
  GRelease: Boolean;
  GToken1: INyxEventSubscription;
  GToken2: INyxEventSubscription;
  {$IFDEF PAS2JS}
  GView: TNyxBrowserRenderer;
  GPopover: INyxBrowserPopover;
  GHost: TJSHTMLElement;
  GResumeTimer: NativeInt;
  {$ELSE}
  GView: TNyxLCLRenderer;
  GPopover: INyxLCLPopover;
  GHost: TForm;
  {$ENDIF}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Popover: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TCompletion.Create(AMarker: Integer);
begin
  inherited Create;
  Marker := AMarker;
end;

procedure TCompletion.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Check(AEvent.IsNamed(NyxSemantic(nseDismiss)), 'Typed completion snapshot');
  Check(NyxPopoverDismissReason(AEvent) in [nprEscape, nprAction],
    'Typed reason survives callback-driven reopening and release');
  GOrder := GOrder + TNyxText(IntToStr(Marker));

  if GReopen then
  begin
    GReopen := False;
    GPopover.Open(NyxPopover('Quick notes').Size(360, 400).Focus(NyxPart('notes')));
  end;

  if GRelease then
  begin
    GRelease := False;
    GPopover := nil;
  end;
end;

procedure Pump;
begin
  {$IFNDEF PAS2JS}
  Application.ProcessMessages;
  {$ENDIF}
end;

function NewPopover: {$IFDEF PAS2JS}INyxBrowserPopover{$ELSE}INyxLCLPopover{$ENDIF};
begin
  {$IFDEF PAS2JS}
  Result := NewNyxBrowserPopover(GView.FocusFor('open-notes'), GDocument,
    NyxPageRoot('quick-notes'));
  {$ELSE}
  Result := NewNyxLCLPopover(GView.FocusFor('open-notes'), GDocument,
    NyxPageRoot('quick-notes'));
  {$ENDIF}
end;

procedure FocusInvoker;
begin
  {$IFDEF PAS2JS}
  NyxFocusWithoutScroll(GView.FocusFor('open-notes'));
  {$ELSE}
  GHost.BringToFront;
  GView.FocusFor('open-notes').SetFocus;
  {$ENDIF}
  Pump;
end;

function InvokerFocused: Boolean;
begin
  {$IFDEF PAS2JS}
  Result := document.activeElement = GView.FocusFor('open-notes');
  {$ELSE}
  Result := Screen.ActiveControl = GView.FocusFor('open-notes');
  {$ENDIF}
end;

procedure Geometry;
var
  LBounds: TNyxPopoverRect;
  LOptions: TNyxPopoverOptions;
  LFailed: Boolean;
begin
  LOptions := NyxPopover('Placement').Size(100, 80).Spacing(8, 12);
  LBounds := PlaceNyxPopover(NyxPopoverRect(30, 30, 40, 20),
    NyxPopoverRect(0, 0, 300, 300), LOptions);
  Check((LBounds.Left = 30) and (LBounds.Top = 58), 'Below uses anchor bottom and gap');
  LBounds := PlaceNyxPopover(NyxPopoverRect(220, 250, 40, 20),
    NyxPopoverRect(0, 0, 300, 300), LOptions);
  Check((LBounds.Top = 162) and (LBounds.Left = 188), 'Flip above and clamp horizontally');
  LBounds := PlaceNyxPopover(NyxPopoverRect(-170, 0, 40, 20),
    NyxPopoverRect(-300, -200, 300, 300), LOptions.Placement(npsLeft, npaEnd));
  Check((LBounds.Left = -278) and (LBounds.Top = -60), 'Signed monitor origin and end alignment');
  LBounds := PlaceNyxPopover(NyxPopoverRect(0, 0, 4, 4),
    NyxPopoverRect(0, 0, 5, 3), LOptions);
  Check((LBounds.Width = 3) and (LBounds.Height = 1), 'Tiny viewport admits bounded allocation');
  LFailed := False;
  try
    LOptions.Size(0, 10);
  except
    on ENyxModel do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'Invalid size refuses');
end;

procedure EditMemo;
begin
  {$IFDEF PAS2JS}
  TJSHTMLTextAreaElement(GPopover.Renderer.FocusFor('notes-memo')).value :=
    'A bright idea 🌙';
  GPopover.Renderer.FocusFor('notes-memo').dispatchEvent(TJSEvent.new('input'));
  {$ELSE}
  TMemo(GPopover.Renderer.FocusFor('notes-memo')).Text := 'A bright idea 🌙';
  {$ENDIF}
  Pump;
end;

function MemoValue: TNyxText;
begin
  {$IFDEF PAS2JS}
  Result := TJSHTMLTextAreaElement(GPopover.Renderer.FocusFor('notes-memo')).value;
  {$ELSE}
  Result := TNyxText(TMemo(GPopover.Renderer.FocusFor('notes-memo')).Text);
  {$ENDIF}
end;

procedure Escape;
{$IFDEF PAS2JS}
var
  LOptions: TJSObject;
  LEvent: TKeyboard;
{$ELSE}
var
  LKey: Word;
{$ENDIF}
begin
  {$IFDEF PAS2JS}
  LOptions := TJSObject.new;
  LOptions['key'] := 'Escape';
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LEvent := TKeyboard.new('keydown', LOptions);
  GPopover.Renderer.FocusFor('notes-memo').dispatchEvent(LEvent);
  Check(LEvent.defaultPrevented, 'Browser Escape consumed after child');
  {$ELSE}
  LKey := VK_ESCAPE;
  Application.NotifyKeyDownHandler(GPopover.Renderer.FocusFor('notes-memo'), LKey, []);
  Check(LKey = 0, 'Native application after-key Escape consumed');
  {$ENDIF}
  Pump;
end;

procedure OutsidePress;
{$IFDEF PAS2JS}
var
  LOptions: TJSObject;
{$ELSE}
var
  LPoint: TPoint;
  LMessage: TLMessage;
  LOutside: TWinControl;
{$ENDIF}
begin
  {$IFDEF PAS2JS}
  NyxFocusWithoutScroll(GView.FocusFor('keep-working'));
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  GView.FocusFor('keep-working').dispatchEvent(TPointer.new('pointerdown', LOptions));
  {$ELSE}
  GHost.BringToFront;
  LOutside := GView.FocusFor('keep-working');
  LOutside.SetFocus;
  LPoint := LOutside.ClientToScreen(Point(LOutside.Width div 2, LOutside.Height div 2));
  SetCursorPos(LPoint.X, LPoint.Y);
  LMessage := Default(TLMessage);
  LMessage.Msg := LM_LBUTTONDOWN;
  Application.NotifyUserInputHandler(LOutside, LMessage);
  {$ENDIF}
end;

procedure AfterCapture; forward;

{$IFDEF PAS2JS}
procedure AwaitCapture;
begin

  if document.body.getAttribute('data-capture-observed') = 'quick-notes' then
  begin
    window.clearInterval(GResumeTimer);
    try
      AfterCapture;
    except
      on E: Exception do
      begin
        document.body.setAttribute('data-popover', 'failed');
        document.body.setAttribute('data-event-error', E.Message);
      end;
    end;
  end;
end;
{$ENDIF}

function Capture: Boolean;
{$IFNDEF PAS2JS}
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
{$ENDIF}
begin
  Result := True;
  {$IFDEF PAS2JS}
  document.body.setAttribute('data-capture-checkpoint', 'quick-notes');
  GResumeTimer := window.setInterval(@AwaitCapture, 50);
  Result := False;
  {$ENDIF}
  {$IFNDEF PAS2JS}
  GPopover.Window.Repaint;
  Pump;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := TFPWriterPNG.Create;
  try
    LBitmap.SetSize(GPopover.Window.Width, GPopover.Window.Height);
    GPopover.Window.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(ParamStr(1), LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
  {$ENDIF}
end;

procedure Finish;
begin
  Check(not GPopover.IsOpen and (GPopover.LastReason = nprAnchorUnavailable),
    'Weak anchor retirement dismisses on actual UI timer');
  GPopover := nil;
  GToken1 := nil;
  GToken2 := nil;
  Check(GRetained.ID = 'quick-notes', 'Content handle survives owner retirement');
  GRetained := nil;
  {$IFDEF PAS2JS}
  Check(document.querySelectorAll('.nyx-popover').length = 0, 'Host and timers retire');
  { Keep an independently mounted ordinary view visible for selective validation. }
  GView.Render(GDocument, GDocument.Pages[0], GHost);
  document.body.setAttribute('data-popover-checks', IntToStr(GChecks));
  document.body.setAttribute('data-popover', 'passed');
  {$ELSE}
  WriteLn('PASS ', GChecks, ' actual Win32 popover/control checks');
  {$ENDIF}
end;

procedure Journey;
var
  LPage: INyxPage;
  LFailed: Boolean;
begin
  Geometry;
  GDocument := BuildNyxDocument;
  { Public runtime enrichment is separate from semantic admission: MCP cannot
    currently declare a managed presentation or close action. The exact exported
    companion remains unchanged, shared by both executed target consumers. }
  GDocument.State.SetValue(NyxTextState('note context'), 'Copied default');
  GDocument.Find('notes-close').Configure.OnClick(NyxSemantic(nseDismiss)).Done;
  GBackground := TNyxDocument.Create;
  LPage := NewNyxPage('background');
  GInvoker := NewNyxButton('open-notes');
  GInvoker.Text := 'Quick notes';
  LPage.Add(GInvoker);
  GOutside := NewNyxButton('keep-working');
  GOutside.Text := 'Keep working';
  LPage.Add(GOutside);
  GBackground.AddPage(LPage);
  {$IFDEF PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  GView := TNyxBrowserRenderer.Create;
  GView.Render(GBackground, LPage.Node, GHost);
  {$ELSE}
  Application.Initialize;
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(120, 100, 760, 620);
  GHost.Show;
  GView := TNyxLCLRenderer.Create;
  GView.Render(GBackground, LPage.Node, GHost);
  {$ENDIF}
  Pump;
  FocusInvoker;
  GPopover := NewPopover;
  GRetained := GPopover.Content;
  Check(Supports(GRetained, INyxCard), 'Specialized content survives generic presentation seam');
  Check(GPopover.State.GetValue(NyxTextState('note context')) = 'Copied default',
    'Complete document defaults copied');
  GPopover.State.SetValue(NyxTextState('note context'), 'Independent runtime');
  Check(GDocument.State.GetValue(NyxTextState('note context')) = 'Copied default',
    'Runtime never changes authored defaults');
  GToken1 := GPopover.OnDismiss.Subscribe(TCompletion.Create(1));
  GToken2 := GPopover.OnDismiss.Subscribe(TCompletion.Create(2));
  LFailed := False;
  try
    GPopover.Open(NyxPopover('Unavailable focus').Focus(NyxPart('missing')));
  except
    on ENyxModel do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and not GPopover.IsOpen, 'Missing part refuses without opening');
  GPopover.Open(NyxPopover('Quick notes').Size(360, 400));
  Check(InvokerFocused, 'No-focus policy preserves invoker focus');
  GPopover.Close;
  GPopover.Open(NyxPopover('Quick notes').Size(360, 400).Focus(NyxPart('notes')));
  Pump;
  {$IFDEF PAS2JS}
  Check(document.activeElement = GPopover.Renderer.FocusFor('notes-memo'), 'Real initial focus');
  Check(GPopover.Element.getAttribute('popover') = 'manual', 'Standard nonmodal top layer');
  Check(GHost.closest('[inert]') = nil, 'Background remains interactive');
  Check(GPopover.Element.getBoundingClientRect.width <= window.innerWidth - 24,
    'Actual host caps narrow width');
  {$ELSE}
  Check(Screen.ActiveControl = GPopover.Renderer.FocusFor('notes-memo'), 'Real initial focus');
  Check(GHost.Enabled, 'Native background stays enabled');
  Check(GPopover.Window.BorderStyle = bsNone, 'Native nonmodal contextual host');
  {$ENDIF}
  EditMemo;
  Check(MemoValue = TNyxText('A bright idea 🌙'), 'Actual memo accepts exact supplementary text');

  if not Capture then
  begin
    Exit;
  end;
  AfterCapture;
end;

procedure AfterCapture;
begin
  Escape;
  Check(not GPopover.IsOpen and (GPopover.LastReason = nprEscape), 'Typed Escape dismissal');
  Check(GOrder = '12', 'Multiple callbacks in registration order');
  Check(InvokerFocused, 'Escape returns focus to actual invoker');
  GPopover.Open(NyxPopover('Quick notes').Size(360, 400).Focus(NyxPart('notes')));
  Check(MemoValue = TNyxText('A bright idea 🌙'), 'Unbound draft survives close/reopen');
  GPopover.Close;
  Check(GOrder = '12', 'Silent Close emits no completion');
  GReopen := True;
  GPopover.Open(NyxPopover('Quick notes').Focus(NyxPart('notes')));
  GPopover.Dismiss;
  Check(GPopover.IsOpen and (GOrder = '1212'), 'Completion can reopen before later registrations');
  GRelease := True;
  GPopover.Dismiss;
  Check(GPopover = nil, 'Completion can release safely');
  GToken1 := nil;
  GToken2 := nil;
  Pump;
  GPopover := NewPopover;
  FocusInvoker;
  GPopover.Open(NyxPopover('Quick notes').Size(360, 400).DismissOn([]));
  OutsidePress;
  Check(GPopover.IsOpen, 'Disabled outside dismissal preserves the contextual view');
  GPopover.Close;
  FocusInvoker;
  GPopover.Open(NyxPopover('Quick notes').Size(360, 400).Focus(NyxPart('notes')));
  OutsidePress;
  Check(not GPopover.IsOpen and (GPopover.LastReason = nprOutsidePress),
    'Outside press uses its typed dismissal policy');
  {$IFDEF PAS2JS}
  Check(document.activeElement = GView.FocusFor('keep-working'),
    'Outside dismissal preserves the new focus destination');
  {$ELSE}
  Check(Screen.ActiveControl = GView.FocusFor('keep-working'),
    'Outside dismissal preserves the new focus destination');
  {$ENDIF}
  FocusInvoker;
  GPopover.Open(NyxPopover('Quick notes').Size(360, 400).Focus(NyxPart('notes')));
  {$IFDEF PAS2JS}
  GPopover.Renderer.FocusFor('notes-close').click;
  {$ELSE}
  TControlAccess(GPopover.Renderer.FocusFor('notes-close')).Click;
  {$ENDIF}
  Check(not GPopover.IsOpen and (GPopover.LastReason = nprAction),
    'Ordinary typed Nyx dismiss action closes the contextual view');
  GPopover.Open(NyxPopover('Quick notes').Size(360, 400).Focus(NyxPart('notes')));
  {$IFDEF PAS2JS}
  GView.Unmount;
  window.setTimeout(@Finish, 160);
  {$ELSE}
  GView.Unmount;
  Sleep(160);
  Pump;
  Finish;
  {$ENDIF}
end;

begin
  try
    Journey;
  except
    on E: Exception do
    begin
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-popover', 'failed');
      document.body.setAttribute('data-popover-error', E.Message);
      {$ELSE}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$ENDIF}
    end;
  end;
  {$IFNDEF PAS2JS}
  GPopover := nil;
  GToken1 := nil;
  GToken2 := nil;
  GRetained := nil;
  GView.Free;
  GInvoker := nil;
  GOutside := nil;
  GBackground.Free;
  GDocument.Free;
  GHost.Free;
  {$ENDIF}
end.
