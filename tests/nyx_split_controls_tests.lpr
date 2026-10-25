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
program nyx_split_controls_tests;

{$mode delphi}{$H+}
{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.behavior, nyx.split,
  nyx.test.split, nyx.events, nyx.scheduler,
  {$ifdef PAS2JS}
  JS, Web, nyx.render.browser, nyx.test.keyboard.browser;
  {$else}
  Interfaces, Forms, Controls, StdCtrls, Types, Classes, LMessages, LCLType, LCLIntf,
  nyx.render.lcl, nyx.split.lcl;
  {$endif}

type
  { Observes completed changes and optionally retires the borrowed renderer
    synchronously, exercising callback lifetime without owning the view. }
  TSink = class
    Count: Integer;
    Last: TNyxEventInfo;
    Navigate: Boolean;
    {$ifdef PAS2JS}Renderer: TNyxBrowserRenderer;{$else}Renderer: TNyxLCLRenderer;{$endif}
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;
  { Independent typed registration observes the separator before its default.
    Its renderer is borrowed only during this synchronous fixture. }
  TKeyProbe = class(TNyxEventCallback)
    Renderer: {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
    Count: Integer;
    ExpectedPosition: Integer;
    Consume: Boolean;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$ifdef PAS2JS}
  { Browser PointerEvent bridge creates synthetic fixture input; it does not
    obtain trusted OS pointer capture or prove physical touch behavior. }
  TPointer = class external name 'PointerEvent' (TJSPointerEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  {$else}
  { Exposes protected key handlers for existing callback-order assertions.
    Mouse regression input uses the complete LCL message path instead. }
  TGripAccess = class(TNyxLCLSplitGrip);
  {$endif}

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Split controls: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TSink.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if AEvent.Trigger = ntChange then
  begin
    Inc(Count);
    Last := AEvent.Copy;

    if Navigate then
    begin
      Renderer.Unmount;
    end;
  end;
end;

procedure TKeyProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Check(AEvent.HasKeyboard and (AEvent.Keyboard.Key = nkHomeKey),
    'separator hook carries the typed key');
  Check(StrToInt(Renderer.Root.Find('work-split').Prop('split-position')) = ExpectedPosition,
    'separator hook executes before the resizing default');
  Inc(Count);

  if Consume then
  begin
    NyxEventResponse(AExecution).Consume;
    Check(NyxEventResponse(AExecution).Consumed, 'separator default can be consumed');
  end;
end;

{$ifdef PAS2JS}
procedure Pointer(AHost: TJSHTMLElement; const AType: String;
  AX, AY: Double; AID: Integer = 1);
var
  LOptions: TJSObject;
begin
  LOptions := TJSObject.new;
  LOptions['clientX'] := AX;
  LOptions['clientY'] := AY;
  LOptions['pointerId'] := AID;
  LOptions['pointerType'] := 'touch';
  LOptions['isPrimary'] := True;
  LOptions['button'] := 0;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  AHost.dispatchEvent(TPointer.new(AType, LOptions));
end;
{$endif}

{$ifndef PAS2JS}
{ Send input to the mounted native window: calling MouseUp directly skips
  LCL's automatic capture release and can conceal a drag cancellation defect.
  The platform still supplies its normal window/default routing. This is
  injected native window input, not physical desktop automation. }
procedure NativeMouse(AControl: TWinControl; AMessage: Cardinal;
  AX, AY: Integer; AButtons: PtrInt = 0);
var
  LMessage: TLMMouse;
begin
  LMessage := Default(TLMMouse);
  LMessage.Msg := AMessage;
  LMessage.Keys := AButtons;
  LMessage.XPos := AX;
  LMessage.YPos := AY;
  LCLIntf.SendMessage(AControl.Handle, AMessage, AButtons, LMessage.Dummy);
end;
{$endif}

procedure Run;
var
  LDocument: TNyxDocument;
  LSink: TSink;
  LWire: TNyxText;
  LBefore: Integer;
  LProbe: TKeyProbe;
  LOwner: INyxEventCallback;
  LToken: INyxEventSubscription;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LSplit: TJSHTMLElement;
  LGrip: TJSHTMLElement;
  LInput: TJSHTMLTextAreaElement;
  LX, LY: Double;
  LHeight: Double;
  {$else}
  LHost: TForm;
  LSplit: TNyxLCLSplitView;
  LGrip: TGripAccess;
  LInput: TMemo;
  LPoint: TPoint;
  LStart: TPoint;
  LKey: Word;
  LWidth: Integer;
  {$endif}
begin
  LDocument := CreateNyxSplitFixture;
  LSink := TSink.Create;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  LHost.style.cssText := 'width:700px;max-width:100%;height:700px;';
  document.body.appendChild(LHost);
  LSink.Renderer := TNyxBrowserRenderer.Create;
  {$else}
  Application.Initialize;
  LHost := TForm.Create(nil);
  LHost.SetBounds(20, 20, 700, 700);
  LHost.Show;
  LSink.Renderer := TNyxLCLRenderer.Create;
  {$endif}
  try
    LSink.Renderer.OnEvent := LSink.Event;
    LSink.Renderer.Render(LDocument, LDocument.Pages[0], LHost);
    {$ifdef PAS2JS}
    LSplit := LSink.Renderer.ElementFor('work-split');
    LGrip := TJSHTMLElement(LSplit.querySelector('.nyx-split-divider'));
    LInput := TJSHTMLTextAreaElement(LSink.Renderer.ElementFor('second-pane').querySelector('textarea'));
    Check((LGrip.getAttribute('aria-orientation') = 'horizontal') and
      (LGrip.getAttribute('aria-disabled') = 'false'), 'actual browser rule and separator');
    Check(LGrip.getBoundingClientRect.height = 44, 'touch hit area');
    {$else}
    Application.ProcessMessages;
    LSplit := TNyxLCLSplitView(LSink.Renderer.ControlFor('work-split'));
    Check((LSplit.State.Orientation = nsoSideBySide) and not LSplit.State.Resizable and
      LSplit.Grip.Enabled and LSplit.Grip.TabStop,
      'fixed native separator remains inspectable without enabling resizing');
    LDocument.Find('work-split').Configure.ForPlatform(npfNativeLCL).SplitResizable(True);
    LSink.Renderer.Render(LDocument, LDocument.Pages[0], LHost);
    Application.ProcessMessages;
    LSplit := TNyxLCLSplitView(LSink.Renderer.ControlFor('work-split'));
    LGrip := TGripAccess(LSplit.Grip);
    LInput := TMemo(LSink.Renderer.InputFor('second-pane'));
    Check(LGrip.Enabled and LGrip.TabStop, 'native grip is focusable');
    Check(LGrip.Width = 44, 'native divider extent');
    {$endif}
    LWire := TNyxCodec.Encode(LDocument);
    {$ifdef PAS2JS}
    LInput.value := 'Keep exact edit / 🌙漢字';
    LInput.focus;
    LInput.selectionStart := 5;
    LInput.selectionEnd := 10;
    LHeight := LSink.Renderer.ElementFor('second-pane').getBoundingClientRect.height;
    LX := LGrip.getBoundingClientRect.left + 12;
    LY := LGrip.getBoundingClientRect.top + 12;
    Pointer(LGrip, 'pointerdown', LX, LY);
    Check(LGrip.getAttribute('aria-valuenow') = '65', 'touch begins without snapping');
    Pointer(LGrip, 'pointermove', LX, LY - 96, 2);
    Check(LGrip.getAttribute('aria-valuenow') = '65', 'unrelated pointer refused');
    Pointer(LGrip, 'pointermove', LX, LY - 96);
    Pointer(LGrip, 'pointerup', LX, LY - 96);
    Check((LSink.Renderer.ElementFor('second-pane').querySelector('textarea') = LInput) and
      (LInput.value = 'Keep exact edit / 🌙漢字'), 'touch retains actual input and exact draft');
    Check((LInput.selectionStart = 5) and (LInput.selectionEnd = 10),
      'touch retains caret selection');
    Check(LSink.Renderer.ElementFor('second-pane').getBoundingClientRect.height > LHeight,
      'second pane gains visible height');
    LBefore := StrToInt(LGrip.getAttribute('aria-valuenow'));
    Pointer(LGrip, 'pointerdown', LX, LY);
    Pointer(LGrip, 'pointermove', LX, LY + 100);
    Pointer(LGrip, 'pointercancel', LX, LY + 100);
    Check(StrToInt(LGrip.getAttribute('aria-valuenow')) = LBefore, 'pointer cancellation restores proportion');
    LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Home'));
    Check(LGrip.getAttribute('aria-valuenow') = '10', 'keyboard minimum');
    LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'ArrowDown', [nmShift]));
    Check(LGrip.getAttribute('aria-valuenow') = '20', 'keyboard coarse adjustment');
    LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'End'));
    Check(LGrip.getAttribute('aria-valuenow') = '90', 'keyboard maximum');
    LSplit.style.setProperty('height', '600px');
    Check(LGrip.getAttribute('aria-valuenow') = '90', 'host resize retains proportion');
    LSplit.style.setProperty('height', '20px');
    Check(LGrip.getBoundingClientRect.height <= 20, 'tiny browser host bounds its divider');
    LSplit.style.setProperty('height', '600px');
    {$else}
    LInput.Text := 'Keep exact edit / 🌙漢字';
    LInput.SetFocus;
    LInput.SelStart := 5;
    LInput.SelLength := 5;
    LSink.Count := 0;
    LWidth := LSplit.Panes[1].Width;
    LStart := LGrip.ClientToScreen(Point(12, 12));
    NativeMouse(LGrip, LM_LBUTTONDOWN, 12, 12, MK_LBUTTON);
    Check(LSplit.State.Position = 65, 'native drag begins without snapping');
    NativeMouse(LGrip, LM_MOUSEMOVE, -84, 12, MK_LBUTTON);
    Check(LSplit.State.Dragging and (LSplit.State.Position < 65),
      'native message movement changes the captured position');
    LBefore := LSplit.State.Position;
    LPoint := LGrip.ScreenToClient(Point(LStart.X - 96, LStart.Y));
    NativeMouse(LGrip, LM_LBUTTONUP, LPoint.X, LPoint.Y);
    Check(not LSplit.State.Dragging and (LSplit.State.Position = LBefore),
      'native message release retains the dragged position');
    Check((LSink.Renderer.InputFor('second-pane') = LInput) and
      (TNyxText(LInput.Text) = TNyxText('Keep exact edit / 🌙漢字')),
      'native drag retains actual memo and Unicode draft');
    Check((LInput.SelStart = 5) and (LInput.SelLength = 5), 'native drag retains caret');
    Check(LSplit.Panes[1].Width > LWidth, 'native second pane gains space');
    LBefore := LSplit.State.Position;
    NativeMouse(LGrip, LM_LBUTTONDOWN, 12, 12, MK_LBUTTON);
    NativeMouse(LGrip, LM_MOUSEMOVE, 120, 12, MK_LBUTTON);
    LGrip.MouseCapture := False;
    Check(LSplit.State.Position = LBefore, 'native capture loss cancels');
    LKey := $24;
    LGrip.KeyDown(LKey, []);
    Check((LKey = 0) and (LSplit.State.Position = 10), 'native keyboard minimum consumes key');
    LKey := $27;
    LGrip.KeyDown(LKey, [ssShift]);
    Check(LSplit.State.Position = 20, 'native keyboard coarse adjustment');
    LKey := $23;
    LGrip.KeyDown(LKey, []);
    Check(LSplit.State.Position = 90, 'native keyboard maximum');
    LSplit.Width := 620;
    Check(LSplit.State.Position = 90, 'native host resize retains proportion');
    LSplit.Width := 20;
    Check(LGrip.Width <= 20, 'tiny native host bounds its divider');
    LSplit.Width := 620;
    {$endif}
    Check(LSink.Count = 4, 'only completed changes dispatch');
    Check(LSink.Last.HasValue and (LSink.Last.Value.AsInteger = 90),
      'actual change contains typed percent snapshot');
    Check(TNyxCodec.Encode(LDocument) = LWire, 'widget interaction leaves authored rules unchanged');
    LProbe := TKeyProbe.Create;
    LProbe.Renderer := LSink.Renderer;
    LProbe.ExpectedPosition := 90;
    LOwner := LProbe;
    LToken := LSink.Renderer.Events.On(NyxControlEvents('work-split'),
      ntBeforeKeyDown).Subscribe(LOwner);
    LSink.Renderer.Root.Configure.ReadOnly(True);
    LSink.Renderer.Sync;
    {$ifdef PAS2JS}
    LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Home'));
    Check(LGrip.getAttribute('aria-valuenow') = '90',
      'inherited browser read-only refuses keyboard resizing');
    Check((LGrip.tabIndex = 0) and (LSink.Renderer.FocusFor('work-split') = LGrip),
      'read-only browser separator retains its public focus face');
    {$else}
    LKey := $24;
    LGrip.KeyDown(LKey, []);
    Check((LSplit.State.Position = 90) and LGrip.Enabled and LGrip.TabStop,
      'inherited native read-only refuses keyboard resizing');
    Check(LSink.Renderer.FocusFor('work-split') = LGrip,
      'read-only native separator retains its public focus face');
    {$endif}
    Check(LProbe.Count = 1, 'read-only separator still emits its key notification');
    LSink.Renderer.Root.Configure.ReadOnly(False);
    LSink.Renderer.Sync;
    LProbe.Consume := True;
    {$ifdef PAS2JS}
    LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Home'));
    Check(LGrip.getAttribute('aria-valuenow') = '90', 'consumed browser key preserves proportion');
    {$else}
    LKey := $24;
    LGrip.KeyDown(LKey, []);
    Check((LKey = 0) and (LSplit.State.Position = 90), 'consumed native key preserves proportion');
    {$endif}
    Check((LProbe.Count = 2) and (LSink.Count = 4), 'consumption emits no completed resize');
    LToken.Cancel;
    Check(not LToken.Active, 'separator key registration is independently cancellable');
    {$ifdef PAS2JS}
    Pointer(LGrip, 'pointerdown', LX, LY);
    Pointer(LGrip, 'pointermove', LX, LY - 96);
    Check(LGrip.getAttribute('aria-valuenow') <> '90', 'browser permission test begins a real drag');
    {$else}
    NativeMouse(LGrip, LM_LBUTTONDOWN, 8, 8, MK_LBUTTON);
    NativeMouse(LGrip, LM_MOUSEMOVE, -80, 8, MK_LBUTTON);
    Check(LSplit.State.Position <> 90, 'native permission test begins a real drag');
    {$endif}
    LSink.Renderer.Root.Configure.ReadOnly(True);
    LSink.Renderer.Sync;
    {$ifdef PAS2JS}
    Check(LGrip.getAttribute('aria-valuenow') = '90', 'browser permission loss cancels captured drag');
    {$else}
    Check((LSplit.State.Position = 90) and not LSplit.State.Dragging,
      'native permission loss cancels captured drag');
    {$endif}
    Check(LSink.Count = 4, 'permission cancellation does not dispatch a completed change');
    LSink.Renderer.Root.Configure.ReadOnly(False);
    LSink.Renderer.Sync;
    { Apply public child size constraints to the already mounted view. A forced
      allocation must retain the input while the divider follows physical space. }
    LSink.Renderer.Root.Find('first-pane').Configure.MinimumHeight(80).MinimumWidth(80);
    LSink.Renderer.Root.Find('second-pane').Configure.MinimumHeight(240).MinimumWidth(240);
    LSink.Renderer.Sync;
    {$ifdef PAS2JS}
    Check(LSink.Renderer.ElementFor('second-pane').getBoundingClientRect.height >= 239,
      'browser split honors the second pane minimum height');
    Check(LGrip.getAttribute('aria-valuenow') <> '90',
      'browser accessibility describes the physical constrained divider');
    LX := LGrip.getBoundingClientRect.left + 12;
    LY := LGrip.getBoundingClientRect.top + 12;
    LHeight := LGrip.getBoundingClientRect.top;
    Pointer(LGrip, 'pointerdown', LX, LY);
    Pointer(LGrip, 'pointermove', LX, LY - 24);
    Check(LGrip.getBoundingClientRect.top < LHeight - 10,
      'browser drag responds immediately from a minimum-constrained edge');
    Pointer(LGrip, 'pointercancel', LX, LY - 24);
    Check(Abs(LGrip.getBoundingClientRect.top - LHeight) < 1,
      'browser cancellation restores the constrained physical allocation');
    LSink.Renderer.Root.Find('work-split').Configure.Height(204);
    LSink.Renderer.Sync;
    Check(TJSHTMLElement(LSplit.children[0]).getBoundingClientRect.height +
      LGrip.getBoundingClientRect.height + TJSHTMLElement(LSplit.children[2]).getBoundingClientRect.height <=
        LSplit.clientHeight + 1, 'infeasible browser minima do not enlarge the split host');
    Check((LSink.Renderer.ElementFor('second-pane').querySelector('textarea') = LInput) and
      (LInput.value = 'Keep exact edit / 🌙漢字') and (LInput.selectionStart = 5),
      'constrained browser resize retains the same input, Unicode draft and range');
    {$else}
    Check(LSplit.Panes[1].Width >= 240, 'native split honors the second pane minimum width');
    Check((LSplit.State.Position = 90) and (LSplit.State.EffectivePosition <> 90),
      'native constrained allocation retains the requested preference');
    Check(LGrip.AccessibleValue = IntToStr(LSplit.State.EffectivePosition),
      'native accessibility reports the physical constrained divider');
    LWidth := LSplit.Panes[0].Width;
    NativeMouse(LGrip, LM_LBUTTONDOWN, 12, 12, MK_LBUTTON);
    NativeMouse(LGrip, LM_MOUSEMOVE, -12, 12, MK_LBUTTON);
    Check(LSplit.Panes[0].Width < LWidth - 10,
      'native drag responds immediately from a minimum-constrained edge');
    LGrip.MouseCapture := False;
    Check(LSplit.Panes[0].Width = LWidth,
      'native cancellation restores the constrained physical allocation');
    LSink.Renderer.Root.Find('work-split').Configure.Width(204);
    LSink.Renderer.Sync;
    Check(LSplit.Panes[0].Width + LGrip.Width + LSplit.Panes[1].Width = LSplit.ClientWidth,
      'infeasible native minima do not enlarge the split host');
    Check((LSink.Renderer.InputFor('second-pane') = LInput) and
      (TNyxText(LInput.Text) = TNyxText('Keep exact edit / 🌙漢字')) and (LInput.SelStart = 5),
      'constrained native resize retains the same input, Unicode draft and range');
    {$endif}
    LSink.Renderer.Root.Find('first-pane').Configure.Clear(atMinimumHeight).Clear(atMinimumWidth);
    LSink.Renderer.Root.Find('second-pane').Configure.Clear(atMinimumHeight).Clear(atMinimumWidth);
    LSink.Renderer.Sync;
    LSink.Navigate := True;
    {$ifdef PAS2JS}
    LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Home'));
    {$else}
    LKey := $24;
    LGrip.KeyDown(LKey, []);
    {$endif}
    Check(LSink.Renderer.Root = nil, 'resize callback may dispose mounted view');
    Check(LSink.Last.Value.AsInteger = 10, 'callback snapshot survives disposal');
  finally
    LToken := nil;
    LOwner := nil;
    LSink.Renderer.Free;
    LSink.Free;
    LDocument.Free;
    {$ifdef PAS2JS}
    LHost.remove;
    {$else}
    LHost.Free;
    {$endif}
  end;
end;

{$ifndef PAS2JS}
{ Exercise both axes through complete native messages on ordinary public
  controls. The renderer owns each mounted clone; no widget/state is retained
  after replacement or unmount. Authored configuration is a separate owner. }
procedure RunNativeAxisCases(AOrientation: TNyxSplitOrientation);
var
  LDocument: TNyxDocument;
  LHost: TForm;
  LSink: TSink;
  LOuter: TNyxLCLSplitView;
  LNested: TNyxLCLSplitView;
  LNestedControl: INyxSplitView;
  LNestedOrientation: TNyxSplitOrientation;
  LInput: TMemo;
  LWire: TNyxText;
  LOuterPosition: Integer;
  LNestedPosition: Integer;
  LCount: Integer;

  procedure DragAndKeep(const AID: TNyxText; ADelta: Integer);
  var
    LSplit: TNyxLCLSplitView;
    LGrip: TNyxLCLSplitGrip;
    LPoint: TPoint;
    LScreen: TPoint;
    LBefore: Integer;
    LAccepted: Integer;
    LEvents: Integer;
  begin
    LSplit := TNyxLCLSplitView(LSink.Renderer.ControlFor(AID));
    LGrip := LSplit.Grip;
    LBefore := LSplit.State.Position;
    LEvents := LSink.Count;
    LScreen := LGrip.ClientToScreen(Point(12, 12));
    NativeMouse(LGrip, LM_LBUTTONDOWN, 12, 12, MK_LBUTTON);
    Check(LSplit.State.Dragging and (GetCaptureControl = LGrip),
      'native axis gesture explicitly acquires capture');

    if LSplit.State.Orientation = nsoStacked then
    begin
      Inc(LScreen.Y, ADelta);
    end
    else
    begin
      Inc(LScreen.X, ADelta);
    end;
    LPoint := LGrip.ScreenToClient(LScreen);
    NativeMouse(LGrip, LM_MOUSEMOVE, LPoint.X, LPoint.Y, MK_LBUTTON);
    LAccepted := LSplit.State.Position;
    Check(LAccepted <> LBefore, 'native axis movement updates its position');
    LSink.Renderer.Sync;
    Check(LSink.Renderer.ControlFor(AID) = LSplit,
      'refresh during a drag retains the same native split');
    Check(LSplit.State.Dragging and (LSplit.State.Position = LAccepted),
      'ordinary refresh does not cancel or reset the active drag');
    LPoint := LGrip.ScreenToClient(LScreen);
    NativeMouse(LGrip, LM_LBUTTONUP, LPoint.X, LPoint.Y);
    Check(not LSplit.State.Dragging and (GetCaptureControl <> LGrip) and
      (LSplit.State.Position = LAccepted),
      'native release commits before relinquishing capture on either axis');
    Check(LSink.Count = LEvents + 1,
      'one completed native message gesture emits one change');
    LSink.Renderer.Sync;
    Check(LSink.Renderer.ControlFor(AID) = LSplit,
      'refresh after release retains the same native split');
    Check((LSplit.State.Position = LAccepted) and
      (StrToInt(LSink.Renderer.Root.Find(AID).Prop('split-position')) = LAccepted),
      'refresh retains the accepted view and runtime model position');
  end;

begin
  LDocument := CreateNyxSplitFixture;
  LHost := TForm.CreateNew(nil);
  LSink := TSink.Create;
  LSink.Renderer := TNyxLCLRenderer.Create;
  try
    LDocument.Find('work-split').Configure.ForPlatform(npfNativeLCL)
      .SplitOrientation(AOrientation).SplitResizable(True);
    LNestedOrientation := nsoStacked;

    if AOrientation = nsoStacked then
    begin
      LNestedOrientation := nsoSideBySide;
    end;
    LNestedControl := NewNyxSplitView('nested-split');
    LNestedControl.Configure.SplitOrientation(LNestedOrientation).SplitPosition(50)
      .SplitMinimum(10).SplitMaximum(90).Height(160).Done;
    LNestedControl.Add(NewNyxColumn('nested-first'));
    LNestedControl.Add(NewNyxColumn('nested-second'));
    LDocument.Find('first-pane').Add(LNestedControl);
    LWire := TNyxCodec.Encode(LDocument);
    LHost.SetBounds(20, 20, 800, 650);
    LHost.Show;
    LSink.Renderer.OnEvent := LSink.Event;
    LSink.Renderer.Render(LDocument, LDocument.Pages[0], LHost);
    Application.ProcessMessages;
    LOuter := TNyxLCLSplitView(LSink.Renderer.ControlFor('work-split'));
    LNested := TNyxLCLSplitView(LSink.Renderer.ControlFor('nested-split'));
    LInput := TMemo(LSink.Renderer.InputFor('second-pane'));
    LInput.Text := 'A retained native draft / 🌙';
    LInput.SelStart := 4;
    LInput.SelLength := 8;
    DragAndKeep('work-split', -32);
    DragAndKeep('work-split', 40);
    DragAndKeep('work-split', -24);
    LOuterPosition := LOuter.State.Position;
    DragAndKeep('nested-split', -16);
    DragAndKeep('nested-split', 24);
    LNestedPosition := LNested.State.Position;
    Check(LOuter.State.Position = LOuterPosition,
      'nested dragging leaves its outer split preference independent');
    LHost.ClientWidth := LHost.ClientWidth - 60;
    LHost.ClientHeight := LHost.ClientHeight - 40;
    LSink.Renderer.Sync;
    Application.ProcessMessages;
    Check((LOuter.State.Position = LOuterPosition) and
      (LNested.State.Position = LNestedPosition),
      'host reflow retains independent outer and nested proportions');
    Check((LSink.Renderer.InputFor('second-pane') = LInput) and
      (TNyxText(LInput.Text) = TNyxText('A retained native draft / 🌙')) and
      (LInput.SelStart = 4) and (LInput.SelLength = 8),
      'message drags and reflow retain the same memo, draft and selection');
    LCount := LSink.Count;
    NativeMouse(LInput, LM_LBUTTONDOWN, 5, 5, MK_LBUTTON);
    NativeMouse(LInput, LM_MOUSEMOVE, 20, 10, MK_LBUTTON);
    NativeMouse(LInput, LM_LBUTTONUP, 20, 10);
    Check(not LOuter.State.Dragging and not LNested.State.Dragging and
      (LOuter.State.Position = LOuterPosition) and (LSink.Count = LCount),
      'child content input neither starts nor completes a split gesture');
    NativeMouse(LOuter.Grip, LM_LBUTTONDOWN, 12, 12, MK_LBUTTON);

    if AOrientation = nsoStacked then
    begin
      NativeMouse(LOuter.Grip, LM_MOUSEMOVE, 12, 40, MK_LBUTTON);
    end
    else
    begin
      NativeMouse(LOuter.Grip, LM_MOUSEMOVE, 40, 12, MK_LBUTTON);
    end;
    Check(LOuter.State.Dragging and (LOuter.State.Position <> LOuterPosition),
      'Escape case begins with an actual changed native gesture');
    { Key input must enter the native interface too: LM_KEYDOWN delivered
      directly to Perform is only LCL's after-interface notification. }
    LOuter.Grip.SetFocus;
    LCLIntf.SendMessage(LOuter.Grip.Handle, LM_KEYDOWN, VK_ESCAPE, 0);
    Check(not LOuter.State.Dragging and (GetCaptureControl <> LOuter.Grip) and
      (LOuter.State.Position = LOuterPosition) and (LSink.Count = LCount),
      'Escape restores the starting value without a completed change on either axis');
    NativeMouse(LOuter.Grip, LM_LBUTTONUP, 12, 12);
    Check((LOuter.State.Position = LOuterPosition) and (LSink.Count = LCount),
      'late release after cancellation cannot commit another gesture');
    LSink.Renderer.Root.Find('work-split').Configure.Enabled(False);
    LSink.Renderer.Sync;
    NativeMouse(LOuter.Grip, LM_LBUTTONDOWN, 12, 12, MK_LBUTTON);
    NativeMouse(LOuter.Grip, LM_MOUSEMOVE, 40, 40, MK_LBUTTON);
    NativeMouse(LOuter.Grip, LM_LBUTTONUP, 40, 40);
    Check(not LOuter.State.Dragging and (GetCaptureControl <> LOuter.Grip) and
      (LOuter.State.Position = LOuterPosition) and (LSink.Count = LCount),
      'disabled native message input cannot acquire or commit a gesture');
    Check(TNyxCodec.Encode(LDocument) = LWire,
      'all native axis gestures leave authored configuration unchanged');
    { A caller may deliberately remount changed authored configuration. This
      explicit replacement remains distinct from an ordinary retained Sync. }
    LDocument.Find('work-split').Configure.SplitPosition(52);
    LSink.Renderer.Render(LDocument, LDocument.Pages[0], LHost);
    LOuter := TNyxLCLSplitView(LSink.Renderer.ControlFor('work-split'));
    Check(LOuter.State.Position = 52,
      'explicit caller configuration is honored by a replacement mount');
    NativeMouse(LOuter.Grip, LM_LBUTTONDOWN, 12, 12, MK_LBUTTON);
    Check(LOuter.State.Dragging and (GetCaptureControl = LOuter.Grip),
      'retirement case owns an active native capture');
    LSink.Renderer.Unmount;
    Application.ProcessMessages;
    Application.Idle(False);
    Check((LSink.Renderer.Root = nil) and (GetCaptureControl = nil) and
      (LSink.Count = LCount),
      'unmount retires active capture without completing a stale gesture');
  finally
    LSink.Renderer.Free;
    LSink.Free;
    LHost.Free;
    LNestedControl := nil;
    LDocument.Free;
  end;
end;
{$endif}

begin
  try
    Run;
    {$ifndef PAS2JS}
    RunNativeAxisCases(nsoStacked);
    RunNativeAxisCases(nsoSideBySide);
    {$endif}
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' browser split controls';
    document.body.setAttribute('data-split-controls', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' native split controls');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-split-controls', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
