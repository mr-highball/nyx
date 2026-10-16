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
}program nyx_canvas_precision_browser_controls;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}
uses SysUtils, JS, Web, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.canvas.geometry, nyx.canvas.types, nyx.canvas.scene, nyx.canvas.workspace,
  nyx.canvas.precision, nyx.canvas.browser;

type
  { Copied options for the standard PointerEvent constructor. No live host
    object or trusted input authority is retained by this fixture packet. }
  TPointerOptions = class external name 'Object'(TJSObject)
    pointerId: NativeInt;
    pointerType: String;
    clientX: Double;
    clientY: Double;
    button: NativeInt;
    bubbles: Boolean;
    cancelable: Boolean;
  end;
  { Browser PointerEvent bridge for synthetic input through installed listeners.
    Events are browser-owned, untrusted and do not establish physical capture. }
  TPointerInput = class external name 'PointerEvent'(TJSPointerEvent)
    constructor new(const AType: String; AOptions: TPointerOptions);
  end;
  { Copied KeyboardEvent options; this object owns no DOM or callback receiver. }
  TKeyOptions = class external name 'Object'(TJSObject)
    key: String;
    shiftKey: Boolean;
    bubbles: Boolean;
    cancelable: Boolean;
  end;
  { Standard KeyboardEvent bridge. Synthetic key input qualifies attached
    handlers when executed over HTTP, not hardware/IME/assistive technology. }
  TKeyInput = class external name 'KeyboardEvent'(TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TKeyOptions);
  end;
var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Turn(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 50);
end;

procedure Run; async;
var
  LHost: TJSHTMLElement;
  LDocument: TNyxDocument;
  LScene: INyxCanvasScene;
  LWorkspace: INyxCanvasWorkspace;
  LView: INyxBrowserCanvasWorkspaceView;
  LPrecision: INyxCanvasPrecisionView;
  LRulers: INyxBrowserCanvasRulerPresentation;
  LOptions: TNyxCanvasPrecisionOptions;
  LMemo: INyxMemo;
  LInput: TJSHTMLTextAreaElement;
  LTop: TJSHTMLElement;
  LBefore: TNyxText;
  LDesign: TNyxText;
  LCount: Integer;
  LSelection: TNyxCanvasGuideRef;
  LCamera: TNyxCanvasCamera;

  procedure Pointer(const AType: String; AX, AY: Double);
  var
    LOptions: TPointerOptions;
    LBounds: TJSDOMRect;
  begin
    LBounds := LView.Surface.getBoundingClientRect;
    LOptions := TPointerOptions.new;
    LOptions.pointerId := 71;
    LOptions.pointerType := 'mouse';
    LOptions.clientX := LBounds.left + AX;
    LOptions.clientY := LBounds.top + AY;
    LOptions.button := 0;
    LOptions.bubbles := True;
    LOptions.cancelable := True;
    LTop.dispatchEvent(TPointerInput.new(AType, LOptions));
  end;

  procedure Key(AElement: TJSHTMLElement; const AKey: String; AShift: Boolean = False);
  var
    LOptions: TKeyOptions;
  begin
    LOptions := TKeyOptions.new;
    LOptions.key := AKey;
    LOptions.shiftKey := AShift;
    LOptions.bubbles := True;
    LOptions.cancelable := True;
    AElement.dispatchEvent(TKeyInput.new('keydown', LOptions));
  end;
begin
  LHost := TJSHTMLElement(document.createElement('div'));
  LHost.style.setProperty('height', '700px');
  LHost.style.setProperty('width', '100%');
  document.body.appendChild(LHost);
  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxPage('home'));
    LDesign := TNyxCodec.Encode(LDocument);
    LScene := NewNyxCanvasScene;
    LScene.AddLayer(NyxCanvasLayer('Content'));
    LMemo := NewNyxMemo('note');
    LMemo.Configure.Text('Notes').Value('Retained editor').Done;
    LScene.AddControl(NyxCanvasItem('Notes'), NyxCanvasPlacement(NyxCanvasLayer('Content'),
      NyxCanvasBounds(-130, -60, 260, 120)), LMemo);
    LWorkspace := NewNyxCanvasWorkspace('precision', LScene);
    LOptions := NyxCanvasPrecisionOptions.WithRulers(NyxCanvasRulers.Units(1, 'px'))
      .WithGuide(NyxCanvasGuide(NyxCanvasGuideRef('Left margin'), ncaxX, 40))
      .WithGuide(NyxCanvasGuide(NyxCanvasGuideRef('Top margin'), ncaxY, 160).Locked(True))
      .Snapping(ncgspVisible, 8);
    NyxCanvasWorkspacePrecision(LWorkspace).WithPrecision(LOptions);
    LView := MountNyxBrowserCanvasWorkspace(LWorkspace, LDocument, LHost);
    await(TJSPromise.resolve(TJSPromise.new(@Turn)));
    Check(Supports(LView, INyxCanvasPrecisionView, LPrecision), 'runtime typed precision capability');
    Check(Supports(LView, INyxBrowserCanvasRulerPresentation, LRulers), 'borrowed ruler faces capability');
    LTop := LRulers.RulerFor(ncaxX);
    Check((LTop <> nil) and (LRulers.RulerFor(ncaxY) <> nil), 'both DOM ruler strips mounted');
    Check((LTop.querySelector('svg') <> nil) and (LTop.getAttribute('role') = 'button'),
      'DOM ruler has rendered ticks and keyboard semantics');
    Check(LPrecision.Precision.ToData.ToJSON = LOptions.ToData.ToJSON, 'mount snapshots exact setup values');
    LInput := TJSHTMLTextAreaElement(LView.InputFor(NyxCanvasItem('Notes'), NyxControl('note')));
    LInput.value := 'Unfinished notes';
    LInput.selectionStart := 4;
    LInput.selectionEnd := 4;
    LCamera := LView.Camera;
    LView.ZoomAt(2, NyxCanvasPoint(LCamera.ViewportWidth / 2, LCamera.ViewportHeight / 2));
    LView.SetCamera(LCamera);
    Check((LView.InputFor(NyxCanvasItem('Notes'), NyxControl('note')) = LInput) and
      (LInput.value = 'Unfinished notes') and (LInput.selectionStart = 4),
      'ruler camera projection preserves embedded editor, text and caret');
    LBefore := LPrecision.Precision.ToData.ToJSON;
    Pointer('pointerdown', 100, 10);
    Pointer('pointermove', 100, LCamera.ViewportHeight / 2 + 70);
    Check(LPrecision.Precision.ToData.ToJSON = LBefore, 'guide preview preserves exact accepted options');
    Key(LTop, 'Escape');
    Check(LPrecision.Precision.ToData.ToJSON = LBefore, 'Escape discards only the guide preview');
    Pointer('pointerdown', 100, 10);
    Pointer('pointermove', 100, LCamera.ViewportHeight / 2 + 70);
    Pointer('pointercancel', 100, LCamera.ViewportHeight / 2 + 70);
    Check(LPrecision.Precision.ToData.ToJSON = LBefore, 'bubbling pointercancel revokes preview');
    LCount := LPrecision.Precision.Count;
    Pointer('pointerdown', 100, 10);
    Pointer('pointermove', 100, LCamera.ViewportHeight / 2 + 70);
    Pointer('pointerup', 100, LCamera.ViewportHeight / 2 + 70);
    Check(LPrecision.Precision.Count = LCount + 1, 'one bubbling pointer release commits one guide');
    LSelection := LPrecision.SelectedGuide;
    Check(LSelection.Defined and (LPrecision.Precision.Named(LSelection).Axis = ncaxY) and
      (Abs(LPrecision.Precision.Named(LSelection).Position - 70) < 0.001),
      'top ruler creates the projected horizontal guide');
    Key(LView.Surface, 'ArrowDown', True);
    Check(Abs(LPrecision.Precision.Named(LSelection).Position - 80) < 0.001,
      'Shift-arrow moves ten logical pixels');
    Key(LView.Surface, 'Delete');
    Check((LPrecision.Precision.Count = LCount) and not LPrecision.SelectedGuide.Defined,
      'Delete removes only the selected movable guide');
    LPrecision.SelectGuide(NyxCanvasGuideRef('Top margin'));
    LBefore := LPrecision.Precision.ToData.ToJSON;
    Key(LView.Surface, 'Delete');
    Check(LPrecision.Precision.ToData.ToJSON = LBefore, 'locked guide survives Delete');
    LPrecision.SetPrecision(LOptions.WithRulers(NyxCanvasRulers.Axes([])));
    Check((LRulers.RulerFor(ncaxX) = nil) and (LRulers.RulerFor(ncaxY) = nil),
      'rulers independently hidden');
    Check(LView.InputFor(NyxCanvasItem('Notes'), NyxControl('note')) = LInput,
      'precision update retains embedded control');
    Check((TNyxCodec.Encode(LDocument) = LDesign) and
      (NyxCanvasWorkspacePrecision(LWorkspace).Precision.ToData.ToJSON = LOptions.ToData.ToJSON),
      'runtime gestures retain document and shared workspace setup exactly');
    LView.Disconnect;
    Check(not LView.Connected, 'disconnect revokes listeners and borrowed DOM');
  finally

    if LView <> nil then
    begin
      LView.Disconnect;
    end;
    LPrecision := nil;
    LRulers := nil;
    LView := nil;
    LMemo := nil;
    LWorkspace := nil;
    LScene := nil;
    LDocument.Free;
    LHost.remove;
  end;
end;

procedure Start; async;
begin
  try
    await(Run);
    document.body.setAttribute('data-canvas-precision-browser', 'passed');
    document.body.setAttribute('data-canvas-precision-browser-checks', IntToStr(GChecks));
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-canvas-precision-browser', 'failed');
      document.body.setAttribute('data-canvas-precision-browser-error', E.Message);
    end;
  end;
end;
begin
  window.setTimeout(@Start, 100);
end.
