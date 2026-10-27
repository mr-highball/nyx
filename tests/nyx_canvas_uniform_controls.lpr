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
  nyx.button.presentation, nyx.button.configuration
  {$ifdef PAS2JS}, JS, Web, nyx.canvas.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, Types, nyx.canvas.lcl,
    nyx.widgets.lcl, nyx.test.capture.lcl{$endif};

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
  { Access the actual memo's installed LCL mouse producer without claiming
    physical hardware input. The cast never creates, owns or reparents a control. }
  TControlAccess = class(Controls.TControl);

  { Keep asynchronous LCL failures observable to the test process rather than
    opening an interactive exception dialog. The application borrows this
    receiver until its hook is explicitly cleared before receiver retirement. }
  TNativeFailures = class
  public
    MessageText: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}

const
  CNote: TNyxText = 'Notes 🌙 stay yours across every zoom.';
  CEditedNote: TNyxText = 'Notes 🌙 stay yours across every zoom. Edited 🌿';
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
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GView: INyxBrowserCanvasWorkspaceView;
  GInput: TJSHTMLTextAreaElement;
  {$else}
  GHost: TForm;
  GView: INyxLCLCanvasWorkspaceView;
  GInput: TMemo;
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

procedure Build;
var
  LCard: INyxCard;
  LHeading: INyxHeading;
  LHelp: INyxLabel;
  LMemo: INyxMemo;
  LResult: INyxLabel;
  LLayer: TNyxCanvasLayerRef;
  LButton: INyxButton;
begin
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
  LMemo.Binds.Value(NyxTextState('note'));
  LCard.Add(LMemo);
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
    NyxCanvasPlacement(LLayer, NyxCanvasBounds(0, 0, 450, 960)), LCard);
  GWorkspace := NewNyxCanvasWorkspace('workspace', GScene)
    .WithScaling(nccsUniform)
    .WithCamera(NyxCanvasCamera(1280, 900, NyxCanvasPoint(225, 480)));

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
  {$endif}
  GSubscription := GView.EventsFor(GItem).On(NyxControlEvents('try-action'), ntClick)
    .Subscribe(TActionCallback.Create);
  GPointerSubscription := GView.EventsFor(GItem).OnPointerDown(NyxControlEvents('note'))
    .Subscribe(TPointerCallback.Create);
end;

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
    GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(225, 480), LZoom));
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
    CheckPointer(LZoom);
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
