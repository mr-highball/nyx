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
program nyx_canvas_uniform_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses SysUtils, Math, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.state,
  nyx.events, nyx.behavior, nyx.scheduler, nyx.canvas.geometry, nyx.canvas.types,
  nyx.canvas.scene, nyx.canvas.workspace, nyx.canvas.observers,
  nyx.designer.resize, nyx.designer.placement, nyx.images,
  nyx.button.presentation, nyx.button.configuration,
  nyx.scroll.presentation, nyx.scroll.configuration
  {$ifdef PAS2JS}, JS, Web, nyx.canvas.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, Types, nyx.canvas.lcl,
    LCLIntf, LCLType, LMessages, nyx.viewport.surface.lcl, nyx.widgets.lcl,
    nyx.test.capture.lcl{$endif};

type
  { The router retains this callback, which uses only harness-owned state.
    It retains no view, renderer, scene or UI object through an event cycle. }
  TActionCallback = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { The router owns this receiver. It copies only inert pointer facts into the
    harness; no view/control/renderer is retained through a callback cycle. }
  TPointerCallback = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Wheel requests own copied event facts only. This receiver observes the
    ordinary before/default path and may consume a synchronous UI response;
    it retains no control, renderer, scene or document. }
  TWheelCallback = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  {$ifdef PAS2JS}
  { Standard PointerEvent initialization borrows no application objects. The
    browser creates a synchronous synthetic event, not a trusted hardware action. }
  TUniformPointerOptions = class external name 'Object'(TJSObject)
    clientX: Double;
    clientY: Double;
    pointerId: Integer;
    pointerType: String;
    button: Integer;
    buttons: Integer;
    bubbles: Boolean;
    cancelable: Boolean;
  end;

  { The installed Web unit lacks this constructor overload. Only browsers with
    the standard PointerEvent constructor can run this input qualification. }
  TUniformPointerEvent = class external name 'PointerEvent'(TJSPointerEvent)
    constructor new(const AType: String; AOptions: TUniformPointerOptions);
  end;
  {$endif}

  {$ifndef PAS2JS}
  { Access actual LCL mouse/constraint producers without claiming physical
    hardware input. Casts never create, own or reparent a borrowed control. }
  TControlAccess = class(Controls.TControl);

  { Keep asynchronous LCL failures observable to the test process rather than
    opening an interactive exception dialog. The application borrows this
    receiver until its hook is explicitly cleared before receiver retirement.
    The same test-owned receiver supplies the final synchronous retirement hook. }
  TNativeFailures = class
  public
    MessageText: TNyxText;
    Retirements: Integer;
    procedure Failed(ASender: TObject; AException: Exception);
    { A final synchronous native size query retires the view from a callout.
      The test-owned receiver outlives that borrowed control and its callbacks. }
    procedure RetireOnConstraints(ASender: TObject;
      var AMinimumWidth: TConstraintSize;
      var AMinimumHeight: TConstraintSize;
      var AMaximumWidth: TConstraintSize;
      var AMaximumHeight: TConstraintSize);
  end;
  {$endif}

const
  CNote: TNyxText = 'Notes 🌙 stay yours across every zoom.';
  CEditedNote: TNyxText = 'Notes 🌙 stay yours across every zoom. Edited 🌿';
  CSourcePrefix: TNyxText = '  WriteLn(''A useful Pascal note 🌙 ';
  CSourceSuffix: TNyxText = ''');';
  { Reuse the existing owned button-presentation PNG fixture. Its decoded image
    is an ordinary embedded project resource, not a painted control snapshot. }
  CGlyphPNG: TNyxText =
    'iVBORw0KGgoAAAANSUhEUgAAAGQAAAAyEAIAAAB1xzWqAAAACXBIWXMAAAAAAAAAAACdYiYy' +
    'AAABMElEQVR4nO3OsQ0AIAzAsP7/dOEEtsgSGTxnduf2fbE/gMwHIPsDyHwAsj+AzAcg+wPI' +
    'fACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsD' +
    'yHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7' +
    'A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg' +
    '+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwH' +
    'IPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDxwO6T+sr8laFkAAAAABJRU5E' +
    'rkJggg==';
  CZooms: array[0..4] of Double = (1, 0.5, 1.5, 0.5, 1);

var
  GDocument: TNyxDocument;
  GState: TNyxState;
  GWorkspace: INyxCanvasWorkspace;
  GScene: INyxCanvasScene;
  GSubscription: INyxEventSubscription;
  GPointerSubscription: INyxEventSubscription;
  GWheelSubscription: INyxEventSubscription;
  GWheelCount: Integer;
  GWheelConsume: Boolean;
  GScrollAnchor: Double;
  GCodeSource: TNyxText;
  GPointerCount: Integer;
  GPointer: TNyxPointerSnapshot;
  GItem: TNyxCanvasItemRef;
  GChecks: Integer;
  GStage: Integer;
  GDone: Boolean;
  GActionCount: Integer;
  GLegacy: Boolean;
  GBaseWidth: Double;
  GBaseHeight: Double;
  GBaseChrome: Double;
  GScrollPolicy: TNyxScrollPresentation;
  GHasScrollPolicy: Boolean;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GView: INyxBrowserCanvasWorkspaceView;
  GInput: TJSHTMLTextAreaElement;
  GScroll: TJSHTMLElement;
  {$else}
  GHost: TForm;
  GView: INyxLCLCanvasWorkspaceView;
  GInput: TMemo;
  GScroll: TNyxLogicalScrollBox;
  GBaseFont: Integer;
  GNativeFailures: TNativeFailures;
  {$endif}

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure TNativeFailures.Failed(ASender: TObject; AException: Exception);
begin
  MessageText := AException.Message;
end;

procedure TNativeFailures.RetireOnConstraints(ASender: TObject;
  var AMinimumWidth: TConstraintSize;
  var AMinimumHeight: TConstraintSize;
  var AMaximumWidth: TConstraintSize;
  var AMaximumHeight: TConstraintSize);
begin

  if Retirements = 0 then
  begin
    Inc(Retirements);
    GView.Disconnect;
  end;
end;
{$endif}

procedure TActionCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(GActionCount);
  GState.SetValue(NyxTextState('result'), 'Action received ' + TNyxText(IntToStr(GActionCount)));
end;

procedure TPointerCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(GPointerCount);
  GPointer := AEvent.Pointer;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-uniform-pointer-x', FloatToStr(GPointer.X));
  document.body.setAttribute('data-uniform-pointer-y', FloatToStr(GPointer.Y));
  {$endif}
end;

procedure TWheelCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(GWheelCount);

  if GWheelConsume then
  begin
    NyxEventResponse(AExecution).Consume;
  end;
end;

procedure Build;
var
  LCard: INyxCard;
  LHeading: INyxHeading;
  LHelp: INyxLabel;
  LMemo: INyxMemo;
  LResult: INyxLabel;
  LLayer: TNyxCanvasLayerRef;
  LButton: INyxButton;
  LScroll: INyxScroll;
  LCode: INyxCode;
  LIndex: Integer;
  LSource: TNyxText;
  LScrollChoice: TNyxText;

  procedure ScrollFace(const AControl: INyxControl);
  begin

    if LScrollChoice = 'keep' then
    begin
      NyxScrollConfigure(AControl.Configure).Presentation(
        NyxScrollPresentation.Sizing(nssKeepSize));
    end
    else if LScrollChoice = 'floor' then
    begin
      NyxScrollConfigure(AControl.Configure).Presentation(
        NyxScrollPresentation.MinimumThickness(24));
    end;
  end;
begin
  {$ifdef PAS2JS}
  LScrollChoice := '';

  if Pos('chrome=keep', window.location.search) > 0 then
  begin
    LScrollChoice := 'keep';
  end
  else if Pos('chrome=floor', window.location.search) > 0 then
  begin
    LScrollChoice := 'floor';
  end;
  {$else}
  LScrollChoice := ParamStr(4);
  {$endif}
  GHasScrollPolicy := (LScrollChoice = 'keep') or (LScrollChoice = 'floor');
  GScrollPolicy := NyxScrollPresentation;

  if LScrollChoice = 'keep' then
  begin
    GScrollPolicy := GScrollPolicy.Sizing(nssKeepSize);
  end
  else if LScrollChoice = 'floor' then
  begin
    GScrollPolicy := GScrollPolicy.MinimumThickness(24);
  end;
  GDocument := TNyxDocument.Create;
  GDocument.AddPage(NewNyxPage('home'));
  GDocument.State.SetValue(NyxTextState('note'), CNote);
  GDocument.State.SetValue(NyxTextState('result'), 'Result stays inside the fitted card.');
  GState := GDocument.State.Clone;
  LCard := NewNyxCard('notebook');
  LCard.Configure.Padding(20).Gap(12).Done;
  LHeading := NewNyxHeading('notebook-title');
  LHeading.Text := 'Your workspace';
  LCard.Add(LHeading);
  LHelp := NewNyxLabel('notebook-help');
  LHelp.Text := 'Write a note, keep a result, and try the action. Zooming should retain '
    + 'the same layout, live controls and exact text. The result at the bottom must '
    + 'remain reachable after fitting this tall card into a smaller workspace.';
  LCard.Add(LHelp);
  LMemo := NewNyxMemo('note');
  LMemo.Text := 'Your note';
  LMemo.Configure.Height(170).Done;
  ScrollFace(LMemo);
  LMemo.Binds.Value(NyxTextState('note'));
  LCard.Add(LMemo);
  LScroll := NewNyxScroll('scrolling-notes');
  LScroll.Configure.Height(160).Padding(8).Gap(8).Done;
  ScrollFace(LScroll);
  for LIndex := 0 to 11 do
  begin
    LScroll.Add(NewNyxLabel('entry-' + TNyxText(IntToStr(LIndex)))
      .WithText('Earlier note ' + TNyxText(IntToStr(LIndex)))
      .Configure.Height(32).Done);
  end;
  LSource := '';
  for LIndex := 1 to 60 do
  begin
    LSource := LSource + CSourcePrefix + TNyxText(IntToStr(LIndex)) + CSourceSuffix + #10;
  end;
  LCode := NewNyxCode('source-preview').WithText(LSource);
  GCodeSource := LSource;
  LCode.Configure.Width(650).MinimumWidth(650).Height(150).Done;
  ScrollFace(LCode);
  LScroll.Add(LCode);
  LMemo := NewNyxMemo('nested-note').WithText('Nested notes 🌙 stay yours.');
  LMemo.Configure.Height(90).Done;
  ScrollFace(LMemo);
  LScroll.Add(LMemo);
  LCard.Add(LScroll);
  LMemo := NewNyxMemo('prepared-note');
  LMemo.Text := 'Prepared text';
  LMemo.Configure.Height(170).Done;
  LMemo.Binds.Value(NyxTextState('note'));
  LCard.Add(LMemo);
  LButton := NewNyxButton('try-action').WithText('Try the action');
  NyxButtonConfigure(LButton.Configure).Presentation(NyxButtonPresentation
    .Glyph(NyxEmbeddedImage(nimPNG, CGlyphPNG)).Content(nbcGlyphCaption));
  LCard.Add(LButton);
  LHelp := NewNyxLabel('result-help');
  LHelp.Text := 'This last section belongs to the same live compound. Its text '
    + 'should scale with the rest of the card instead of reflowing outside a '
    + 'container that has become shorter. Camera changes never replace your editor.';
  LCard.Add(LHelp);
  LResult := NewNyxLabel('result');
  LResult.Binds.Text(NyxTextState('result'));
  LCard.Add(LResult);
  GScene := NewNyxCanvasScene;
  LLayer := NyxCanvasLayer('Content');
  GScene.AddLayer(LLayer);
  GItem := NyxCanvasItem('Notebook');
  GScene.AddControl(GItem,
    NyxCanvasPlacement(LLayer, NyxCanvasBounds(0, 0, 450, 1200)), LCard);
  GWorkspace := NewNyxCanvasWorkspace('workspace', GScene)
    .WithScaling(nccsUniform)
    .WithCamera(NyxCanvasCamera(1280, 900, NyxCanvasPoint(225, 600)));

  if GLegacy then
  begin
    GWorkspace.WithScaling(nccsReflow);
  end;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('height', 'calc(100vh - 32px)');
  GHost.style.setProperty('width', '100%');
  document.body.appendChild(GHost);
  GView := MountNyxBrowserCanvasWorkspace(GWorkspace, GDocument, GHost, GState);
  GInput := TJSHTMLTextAreaElement(GView.InputFor(GItem, NyxControl('note')));
  GScroll := GView.RendererFor(GItem).ElementFor('scrolling-notes');
  {$else}
  Application.Initialize;
  GNativeFailures := TNativeFailures.Create;
  Application.OnException := GNativeFailures.Failed;
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(20, 20, StrToIntDef(ParamStr(1), 1280), 800);
  GHost.Show;
  GView := MountNyxLCLCanvasWorkspace(GWorkspace, GDocument, GHost, GState);
  Application.ProcessMessages;
  GInput := TMemo(GView.InputFor(GItem, NyxControl('note')));
  GScroll := TNyxLogicalScrollBox(GView.RendererFor(GItem).ControlFor('scrolling-notes'));
  {$endif}
  GSubscription := GView.EventsFor(GItem).On(NyxControlEvents('try-action'), ntClick)
    .Subscribe(TActionCallback.Create);
  GPointerSubscription := GView.EventsFor(GItem).OnPointerDown(NyxControlEvents('note'))
    .Subscribe(TPointerCallback.Create);
  GWheelSubscription := GView.EventsFor(GItem).OnBeforeWheel(NyxControlEvents('scrolling-notes'))
    .Subscribe(TWheelCallback.Create);
end;

{$ifndef PAS2JS}
{ Received native-LCL wheel input keeps exact owned routing even when a canvas
  child is off screen. Win32 hover redirection and physical devices are separate
  checks; this message still enters the installed adapter/default WindowProc. }
procedure CheckScrollWheel(AZoom: Double);
var
  LMessage: TLMMouseEvent;
  LBefore: Integer;
  LCount: Integer;
begin
  LBefore := GScroll.VertScrollBar.Position;
  LCount := GWheelCount;
  LMessage := Default(TLMMouseEvent);
  LMessage.Msg := LM_MOUSEWHEEL;
  LMessage.X := GScroll.ClientWidth div 2;
  LMessage.Y := GScroll.ClientHeight div 2;
  LMessage.WheelDelta := -120;
  GWheelConsume := True;
  GScroll.WindowProc(PLMessage(@LMessage)^);
  Check((GWheelCount = LCount + 1) and (GScroll.VertScrollBar.Position = LBefore),
    'a consumed scroll-area wheel reaches one callback without changing its viewport');
  GWheelConsume := False;
  LMessage.Result := 0;
  GScroll.WindowProc(PLMessage(@LMessage)^);
  Check((GWheelCount = LCount + 2) and (GScroll.VertScrollBar.Position > LBefore),
    'the ordinary scroll-area wheel default moves its actual native viewport once');
  Check(GInput.Focused and (TNyxText(GInput.Text) = CNote) and
    (GInput.SelStart = 2) and (GInput.SelLength = 3),
    'scroll browsing preserves the other focused memo, Unicode text and caret');
  GScrollAnchor := GScroll.VertScrollBar.Position / AZoom;
end;

{ A real focus entry reveals the retained nested memo. Read-only source keeps
  native keyboard scrolling and refuses typed text without replacing its HWND. }
procedure CheckNestedInput;
var
  LCode: TMemo;
  LNested: TMemo;
  LOrigin: TPoint;
  LPoint: TPoint;
  LPhysicalText: TNyxText;
begin
  { Read-only code is a focusable display face, not an editable InputFor port. }
  LCode := TMemo(GView.RendererFor(GItem).ControlFor('source-preview'));
  LPhysicalText := TNyxText(LCode.Text);
  { Native multiline display uses CRLF; the authored Pascal packet keeps LF.
    Compare only that documented display conversion, preserving every scalar. }
  Check(LCode.ReadOnly and
    (TNyxText(StringReplace(LPhysicalText, #13#10, #10, [rfReplaceAll])) = GCodeSource),
    'the read-only native display retains every source scalar with native line endings');
  LCode.SetFocus;
  LCode.SelStart := 0;
  SendMessage(LCode.Handle, LM_KEYDOWN, VK_NEXT, 0);
  SendMessage(LCode.Handle, LM_KEYUP, VK_NEXT, 0);
  Check(LCode.SelStart > 0, 'ordinary PageDown navigates the focused native read-only source');
  SendMessage(LCode.Handle, LM_CHAR, Ord('x'), 0);
  Check(TNyxText(LCode.Text) = LPhysicalText, 'read-only source refuses typed insertion');
  LNested := TMemo(GView.InputFor(GItem, NyxControl('nested-note')));
  LNested.SetFocus;
  Application.ProcessMessages;
  LOrigin := GScroll.ClientToScreen(Point(0, 0));
  LPoint := LNested.ClientToScreen(Point(0, 0));
  Check(LNested.Focused and (LPoint.Y >= LOrigin.Y) and
    (LPoint.Y + LNested.Height <= LOrigin.Y + GScroll.ClientHeight + 1),
    'actual nested-memo focus reveals its retained editor within the zoomed scroll client');
end;
{$endif}

{ Exercise actual adapter producers at the current zoom, then independently
  compare logical face positions and their inverse against the supplied screen
  point. No event is obtained from hidden renderer state. }
procedure CheckPointer(AZoom: Double);
var
  LBefore: Integer;
  LActualX: Double;
  LActualY: Double;
  LOriginX: Double;
  LOriginY: Double;
  LScreen: TNyxResizePoint;
  LFrame: TNyxDropFrame;
  {$ifdef PAS2JS}
  LInputBox: TJSDOMRect;
  LFaceBox: TJSDOMRect;
  LOptions: TUniformPointerOptions;
  {$else}
  LPoint: TPoint;
  LFace: TControl;
  {$endif}
begin
  LBefore := GPointerCount;
  {$ifdef PAS2JS}
  LInputBox := GInput.getBoundingClientRect;
  LFaceBox := GView.RendererFor(GItem).ElementFor('note').getBoundingClientRect;
  LActualX := LInputBox.left + 12 * AZoom;
  LActualY := LInputBox.top + 18 * AZoom;
  LOriginX := LFaceBox.left;
  LOriginY := LFaceBox.top;
  LOptions := TUniformPointerOptions.new;
  LOptions.clientX := LActualX;
  LOptions.clientY := LActualY;
  LOptions.pointerId := 17;
  LOptions.pointerType := 'mouse';
  LOptions.button := 0;
  LOptions.buttons := 1;
  LOptions.bubbles := True;
  LOptions.cancelable := True;
  GInput.dispatchEvent(TUniformPointerEvent.new('pointerdown', LOptions));
  {$else}
  LPoint := GInput.ClientToScreen(Point(Round(12 * AZoom), Round(18 * AZoom)));
  LActualX := LPoint.X;
  LActualY := LPoint.Y;
  LFace := GView.RendererFor(GItem).ControlFor('note');
  LPoint := LFace.Parent.ClientToScreen(Point(LFace.Left, LFace.Top));
  LOriginX := LPoint.X;
  LOriginY := LPoint.Y;
  TControlAccess(Controls.TControl(GInput)).MouseDown(mbLeft, [ssLeft],
    Round(12 * AZoom), Round(18 * AZoom));
  {$endif}
  Check((GPointerCount = LBefore + 1) and GPointer.HasPosition,
    'one actual adapter pointer producer reaches the owning logical face');
  Check((Abs(GPointer.X - (LActualX - LOriginX) / AZoom) < 1) and
    (Abs(GPointer.Y - (LActualY - LOriginY) / AZoom) < 1),
    'zoomed pointer positions use logical outer-face pixels exactly once');
  LScreen := GView.RendererFor(GItem).ScreenPointFor('note', GPointer);
  Check((Abs(LScreen.X - LActualX) < 0.01) and (Abs(LScreen.Y - LActualY) < 0.01),
    'the pointer snapshot round-trips to its actual screen point');
  LFrame := GView.RendererFor(GItem).DropFrameFor('note');
  Check(LFrame.Defined and (Abs(LFrame.Face.Width - LFrame.Width * AZoom) < 2),
    'drop frames retain distinct physical and logical face extents');
  {$ifndef PAS2JS}
  Check((TNyxLCLButton(GView.RendererFor(GItem).ControlFor('try-action')).ContentScale = AZoom) and
    (TNyxLCLSurface(GView.RendererFor(GItem).ControlFor('notebook')).ContentScale = AZoom),
    'the native action and surface share the absolute paint projection');
  Check(TNyxLCLButton(GView.RendererFor(GItem).ControlFor('try-action')).GlyphAvailable,
    'uniform zoom retains the independently decoded embedded action glyph');
  GInput.SelStart := 2;
  GInput.SelLength := 3;
  {$endif}
end;

procedure Step;
var
  LZoom: Double;
  LGeometry: INyxCanvasControlGeometry;
  LBounds: TNyxCanvasBounds;
  {$ifdef PAS2JS}
  LInputBox: TJSDOMRect;
  LResultBox: TJSDOMRect;
  LClip: TJSDOMRect;
  LElement: TJSHTMLElement;
  LParent: TJSHTMLElement;
  LFormerMaxHeight: TNyxText;
  LFormerOverflow: TNyxText;
  {$else}
  LResult: TControl;
  LPoint: TPoint;
  LSurface: TPoint;
  LAncestor: TWinControl;
  LClip: TPoint;
  LFormerBounds: TRect;
  LMirror: TMemo;
  LButton: TNyxLCLButton;
  LEntryPoint: TPoint;
  LScrollPoint: TPoint;
  {$endif}
begin

  if GStage = 0 then
  begin
    {$ifdef PAS2JS}
    GInput.focus;
    GInput.selectionStart := 2;
    GInput.selectionEnd := 5;
    LInputBox := GInput.getBoundingClientRect;
    GBaseWidth := LInputBox.width;
    GBaseHeight := LInputBox.height;
    {$else}
    GInput.SetFocus;
    GInput.SelStart := 2;
    GInput.SelLength := 3;
    GBaseWidth := GInput.Width;
    GBaseHeight := GInput.Height;
    GBaseFont := GInput.Font.Height;
    {$endif}
    {$ifdef PAS2JS}
    GScroll.scrollTop := 96;
    GScroll.scrollLeft := 64;
    {$else}
    GScroll.VertScrollBar.Position := 96;
    GScroll.HorzScrollBar.Position := 64;
    {$endif}
    GScrollAnchor := 96;
    {$ifdef PAS2JS}
    GBaseChrome := GScroll.offsetWidth - GScroll.clientWidth;
    {$else}
    GBaseChrome := GScroll.Width - GScroll.ClientWidth;
    {$endif}

    if GHasScrollPolicy then
    begin
      Check(GBaseChrome >= 16, 'explicit scrollbar policy owns a measured physical allocation');
    end;

    if GLegacy then
    begin
      { The differential path isolates Fit clipping under the old policy;
        it does not impose uniform geometry on deliberate legacy reflow. }
      GStage := Length(CZooms);
    end;
  end
  else if GStage <= Length(CZooms) then
  begin
    LZoom := CZooms[GStage - 1];
    GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(225, 600), LZoom));

    if GHasScrollPolicy then
    begin
      {$ifdef PAS2JS}
      Check(Abs((GScroll.offsetWidth - GScroll.clientWidth) * LZoom -
        17 * GScrollPolicy.EffectiveScale(LZoom, 17)) <= 2,
        'actual browser gutter honors the physical sizing policy under ancestor zoom');
      {$else}
      Check(Abs((GScroll.Width - GScroll.ClientWidth) - GBaseChrome *
        GScrollPolicy.EffectiveScale(LZoom, LCLIntf.GetSystemMetrics(SM_CXVSCROLL)) /
        GScrollPolicy.EffectiveScale(1, LCLIntf.GetSystemMetrics(SM_CXVSCROLL))) <= 3,
        'actual native client allocation honors the independent chrome policy');
      {$endif}
    end;
    Check(GView.InputFor(GItem, NyxControl('note')) = GInput, 'zoom retains the same editor');
    {$ifdef PAS2JS}
    Check((GInput.value = CNote) and (document.activeElement = GInput),
      'zoom preserves exact text and actual editor focus');
    Check((GInput.selectionStart = 2) and (GInput.selectionEnd = 5),
      'zoom retains the actual native browser selection');
    LInputBox := GInput.getBoundingClientRect;
    Check((Abs(LInputBox.width - GBaseWidth * LZoom) < 2) and
      (Abs(LInputBox.height - GBaseHeight * LZoom) < 2),
      'live editor geometry scales from its stable logical baseline');
    {$else}
    Check((TNyxText(GInput.Text) = CNote) and GInput.Focused,
      'zoom preserves exact text and actual native editor focus');
    Check((GInput.SelStart = 2) and (GInput.SelLength = 3), 'zoom retains native selection');
    Check((Abs(GInput.Width - GBaseWidth * LZoom) < 2) and
      (Abs(GInput.Height - GBaseHeight * LZoom) < 2), 'native editor uses absolute logical geometry');
    Check(Abs(GInput.Font.Height - Round(GBaseFont * LZoom)) <= 1,
      'native editor font projects from the absolute logical baseline');
    {$endif}
    {$ifdef PAS2JS}
    Check((GView.RendererFor(GItem).ElementFor('scrolling-notes') = GScroll) and
      (Abs(GScroll.scrollTop - GScrollAnchor) <= 1),
      'zoom retains the actual browser scroll area and its logical anchor');
    Check(Abs(GScroll.scrollLeft - 64) <= 1,
      'browser horizontal scrolling retains its logical offset through CSS zoom');
    {$else}
    Check((GView.RendererFor(GItem).ControlFor('scrolling-notes') = GScroll) and
      (Abs(GScroll.VertScrollBar.Position - Round(GScrollAnchor * LZoom)) <= 1),
      'zoom retains the actual native scroll area and its projected logical anchor / actual ' +
      TNyxText(IntToStr(GScroll.VertScrollBar.Position)) + ' expected ' +
      TNyxText(IntToStr(Round(GScrollAnchor * LZoom))));
    Check(Abs(GScroll.HorzScrollBar.Position - Round(64 * LZoom)) <= 1,
      'native horizontal scrolling retains its projected logical anchor');
    LEntryPoint := GView.RendererFor(GItem).ControlFor('entry-0').ClientToScreen(Point(0, 0));
    LScrollPoint := GScroll.ClientToScreen(Point(0, 0));
    Check(Abs(LEntryPoint.Y - LScrollPoint.Y - Round((8 - GScrollAnchor) * LZoom)) <= 2,
      'the retained scroll anchor keeps the same content at the projected client origin / actual ' +
      TNyxText(IntToStr(LEntryPoint.Y - LScrollPoint.Y)));
    Check(Abs(LEntryPoint.X - LScrollPoint.X - Round((8 - 64) * LZoom)) <= 2,
      'the horizontal anchor preserves the same content at the projected client origin');
    {$endif}
    CheckPointer(LZoom);
    {$ifndef PAS2JS}
    CheckScrollWheel(LZoom);
    {$endif}
  end
  else
  begin
    GView.Execute(nccFit);
    {$ifdef PAS2JS}
    LElement := GView.RendererFor(GItem).ElementFor('result');
    LResultBox := LElement.getBoundingClientRect;
    LClip := GView.Surface.getBoundingClientRect;
    Check((LResultBox.top >= LClip.top - 1) and (LResultBox.bottom <= LClip.bottom + 1),
      'Fit leaves the actual final result inside the canvas viewport');
    while (LElement <> GView.Surface) and (LElement.parentElement <> nil) do
    begin
      LElement := TJSHTMLElement(LElement.parentElement);

      if window.getComputedStyle(LElement).getPropertyValue('overflow') = 'hidden' then
      begin
        LClip := LElement.getBoundingClientRect;
        Check((LResultBox.top >= LClip.top - 1) and (LResultBox.bottom <= LClip.bottom + 1),
          'the fitted result is not hidden by an ancestor clip');
      end;
    end;
    Check(Supports(GView, INyxCanvasControlGeometry, LGeometry),
      'browser workspace exposes its existing optional geometry facet');
    Check(LGeometry.TryControlBounds(GItem, NyxControl('result'), LBounds),
      'browser bounds query finds the visible fitted result');
    LParent := TJSHTMLElement(GView.RendererFor(GItem).ElementFor('result').parentElement);
    LFormerMaxHeight := LParent.style.getPropertyValue('max-height');
    LFormerOverflow := LParent.style.getPropertyValue('overflow');
    try
      LParent.style.setProperty('max-height', '1px');
      LParent.style.setProperty('overflow', 'hidden');
      Check(not LGeometry.TryControlBounds(GItem, NyxControl('result'), LBounds),
        'browser bounds query refuses a result hidden by an ancestor overflow clip');
    finally
      LParent.style.setProperty('max-height', LFormerMaxHeight);
      LParent.style.setProperty('overflow', LFormerOverflow);
    end;
    document.body.setAttribute('data-uniform-canvas', 'passed');
    document.body.setAttribute('data-uniform-checks', IntToStr(GChecks));
    {$else}
    LResult := GView.RendererFor(GItem).ControlFor('result');
    LPoint := LResult.ClientToScreen(Point(0, 0));
    LSurface := GView.Surface.ClientToScreen(Point(0, 0));
    Check((LPoint.Y >= LSurface.Y) and
      (LPoint.Y + LResult.Height <= LSurface.Y + GView.Surface.ClientHeight + 1),
      'native Fit retains the final live result in the viewport');
    LAncestor := LResult.Parent;
    while LAncestor <> nil do
    begin
      LClip := LAncestor.ClientToScreen(Point(0, 0));
      Check((LPoint.X >= LClip.X) and (LPoint.Y >= LClip.Y) and
        (LPoint.X + LResult.Width <= LClip.X + LAncestor.ClientWidth + 1) and
        (LPoint.Y + LResult.Height <= LClip.Y + LAncestor.ClientHeight + 1),
        'native fitted result survives every ancestor client clip');

      if LAncestor = GView.Surface then
      begin
        Break;
      end;
      LAncestor := LAncestor.Parent;
    end;
    Check(Supports(GView, INyxCanvasControlGeometry, LGeometry),
      'native workspace exposes its existing optional geometry facet');
    Check(LGeometry.TryControlBounds(GItem, NyxControl('result'), LBounds),
      'canvas bounds query finds the actual visible fitted result');
    { A surface intersection alone cannot establish visibility. Temporarily
      crop the immediate native parent without changing the accepted document. }
    LFormerBounds := LResult.Parent.BoundsRect;
    try
      LResult.Parent.Width := LResult.Left;
      Check(not LGeometry.TryControlBounds(GItem, NyxControl('result'), LBounds),
        'canvas bounds query refuses a result hidden entirely by an ancestor');
    finally
      LResult.Parent.BoundsRect := LFormerBounds;
    end;
    GInput.Text := CEditedNote;
    Check(Assigned(GInput.OnChange), 'the retained memo keeps its native input hook');
    GInput.OnChange(GInput);
    Application.ProcessMessages;
    LMirror := TMemo(GView.InputFor(GItem, NyxControl('prepared-note')));
    Check(TNyxText(LMirror.Text) = CEditedNote,
      'editing the retained fitted memo updates the second live bound memo');
    Check(GView.InputFor(GItem, NyxControl('note')) = GInput,
      'a binding update does not replace the zoomed editor');
    LButton := TNyxLCLButton(GView.RendererFor(GItem).ControlFor('try-action'));
    LButton.Click;
    Check((GActionCount = 1) and
      (TNyxText(TLabel(GView.RendererFor(GItem).ControlFor('result')).Caption) =
        'Action received 1'), 'the retained fitted native action updates its bound result');
    CheckNestedInput;
    {$endif}
    GDone := True;
  end;
  Inc(GStage);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if not GDone then
    begin
      window.setTimeout(@Drive, 30);
    end;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-uniform-canvas', 'failed');
      document.body.setAttribute('data-uniform-error', LException.Message);
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}
  GLegacy := Pos('reflow', window.location.search) > 0;
  Build;
  window.setTimeout(@Drive, 30);
  {$else}
  try
    GLegacy := (ParamCount > 0) and (ParamStr(1) = '--reflow');
    Build;
    while not GDone do
    begin
      Application.ProcessMessages;

      if GNativeFailures.MessageText <> '' then
      begin
        raise Exception.Create(GNativeFailures.MessageText);
      end;
      Step;
    end;

    if ParamCount >= 2 then
    begin
      Application.ProcessMessages;
      GHost.Update;

      if ParamStr(3) = '--print' then
      begin
        { Explicit diagnostic redraw. It qualifies paint output, not displayed
          clipping or foreground-window behavior. Display remains the default. }
        SaveNyxNativeCapture(GHost, ParamStr(2), ncmPrint);
      end
      else
      begin
        SaveNyxNativeCapture(GHost, ParamStr(2), ncmDisplay);
      end;
    end;

    if GNativeFailures.MessageText <> '' then
    begin
      raise Exception.Create(GNativeFailures.MessageText);
    end;
    { Last: the actual bounds setter must not continue through the retired
      renderer after this control's resize callback disconnects its view. }
    TControlAccess(Controls.TControl(GScroll)).OnConstrainedResize :=
      GNativeFailures.RetireOnConstraints;
    GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(225, 600), 0.75));
    Check((GNativeFailures.Retirements = 1) and not GView.Connected,
      'native size-query retirement stops the original projection without stale widget access');
    Application.ProcessMessages;
    Check(GNativeFailures.MessageText = '', 'native retirement leaves no deferred LCL error');
    WriteLn('PASS ', GChecks, ' live uniform canvas checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL at stage ', GStage, ' / ', LException.Message);
      ExitCode := 1;
    end;
  end;

  if GView <> nil then
  begin
    GView.Disconnect;
  end;
  GSubscription := nil;
  GPointerSubscription := nil;
  GWheelSubscription := nil;
  GView := nil;
  GWorkspace := nil;
  GScene := nil;
  GHost.Free;
  GState.Free;
  GDocument.Free;
  Application.OnException := nil;
  GNativeFailures.Free;
  {$endif}
end.
