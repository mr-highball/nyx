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
program nyx_drawing_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.data, nyx.model, nyx.state, nyx.codec,
  nyx.controls, nyx.drawing, nyx.design.tokens, nyx.canvas.geometry,
  nyx.canvas.types, nyx.canvas.workspace, nyx.generated.view, nyx.test.drawing.capture
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.canvas.browser, nyx.drawing.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
  FPImage, FPWritePNG, nyx.render.lcl, nyx.canvas.lcl, nyx.drawing.lcl{$endif};

type
  { The provider owns no model/view. Frame values are copied; its immutable plan
    may be rendered independently. Counters qualify actual construction/lifetime. }
  TWorkshopProvider = class(TInterfacedObject, INyxDrawingProvider)
  public
    destructor Destroy; override;
    function Build(const AFrame: TNyxDrawingFrame): INyxDrawingPlan;
  end;
  {$ifndef PAS2JS}
  { Fixture-owned receiver records asynchronous native errors until teardown;
    the application borrows it only while this test's host and globals live. }
  TFailureObserver = class
  public
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}

var
  GChecks: Integer;
  GCalls: Integer;
  GDestroyed: Integer;
  GReject: Boolean;
  GProvider: INyxDrawingProvider;
  GDocument: TNyxDocument;
  GState: TNyxState;
  GBefore: TNyxText;
  GBeforeCalls: Integer;
  GQualifiedChecks: Integer;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GRenderer: TNyxBrowserRenderer;
  GView: INyxBrowserCanvasWorkspaceView;
  GMemo: TJSHTMLTextAreaElement;
  GGraphic: TJSHTMLElement;
  GDynamic: TJSHTMLElement;
  GImage: TJSHTMLImageElement;
  GRetained: TJSElement;
  {$else}
  GHost: TForm;
  GRenderer: TNyxLCLRenderer;
  GView: INyxLCLCanvasWorkspaceView;
  GMemo: TMemo;
  GGraphic: TNyxLCLDrawing;
  GDynamic: TNyxLCLDrawing;
  GImage: TImage;
  GRetained: TControl;
  GObserver: TFailureObserver;
  GHostError: TNyxText;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Actual drawing: ' + AReason);
  end;
  Inc(GChecks);
end;

destructor TWorkshopProvider.Destroy;
begin
  Inc(GDestroyed);
  inherited Destroy;
end;

function TWorkshopProvider.Build(const AFrame: TNyxDrawingFrame): INyxDrawingPlan;
begin
  Inc(GCalls);

  if GReject then
  begin
    raise ENyxDrawing.Create('Rejected provider proposal');
  end;
  if not AFrame.Palette.Has(ntcAccent) or (AFrame.Width < 0) or (AFrame.Height < 0) then
  begin
    raise ENyxDrawing.Create('Actual provider requires a value-only effective frame');
  end;
  Result := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 320, 180))
    .Path(NyxDrawingRectangle(NyxCanvasBounds(12, 12, 296, 156)),
      NyxDrawingStyle.Fill(NyxThemePaint(ntcAccent)))
    .Text('Custom content', NyxCanvasPoint(160, 36), NyxThemePaint(ntcAccentText),
      20, ndtaCenter, True)
    .Text('One portable provider', NyxCanvasPoint(160, 80), NyxThemePaint(ntcAccentText),
      14, ndtaCenter)
    .Build;
  { Both actual adapters must admit an alternate creator implementation, rather
    than depending on the built-in class returned by this fixture's builder. }
  Result := TNyxTestDrawingPlan.Create(Result);
end;

{$ifndef PAS2JS}
procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  GHostError := AException.Message;
end;
{$endif}

procedure Cleanup;
begin
  GView := nil;
  GRenderer.Free;
  GRenderer := nil;
  GProvider := nil;
  GState.Free;
  GState := nil;
  GDocument.Free;
  GDocument := nil;
  {$ifdef PAS2JS}
  GHost.remove;
  GHost := nil;
  {$else}
  GHost.Free;
  GHost := nil;
  Application.OnException := nil;
  GObserver.Free;
  GObserver := nil;
  {$endif}
end;

procedure Setup;
begin
  GDocument := BuildNyxDocument;
  GBefore := TNyxCodec.Encode(GDocument);
  GState := GDocument.State.Clone;
  GProvider := TWorkshopProvider.Create;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('width', '100%');
  GHost.style.setProperty('height', '620px');
  document.body.appendChild(GHost);
  GRenderer := TNyxBrowserRenderer.Create;
  {$else}
  Application.Initialize;
  GObserver := TFailureObserver.Create;
  Application.OnException := GObserver.Failed;
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(30, 30, 1040, 740);
  GHost.Show;
  GRenderer := TNyxLCLRenderer.Create;
  {$endif}
  GRenderer.RegisterDrawingProvider(NyxDrawingProvider('workshop-provider'), GProvider);
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost, False, GState);
  {$ifdef PAS2JS}
  GView := GRenderer.CanvasView('workspace') as INyxBrowserCanvasWorkspaceView;
  {$else}
  Application.ProcessMessages;
  GView := GRenderer.CanvasView('workspace') as INyxLCLCanvasWorkspaceView;
  {$endif}
  GView.Execute(nccFit);
  {$ifdef PAS2JS}
  GGraphic := GView.RendererFor(NyxCanvasItem('graphic')).ElementFor('connection-graphic');
  GDynamic := GView.RendererFor(NyxCanvasItem('provider')).ElementFor('provider-graphic');
  GMemo := TJSHTMLTextAreaElement(GView.InputFor(NyxCanvasItem('memo'), NyxControl('notes-memo')));
  GImage := TJSHTMLImageElement(GView.RendererFor(NyxCanvasItem('image')).ElementFor('resource-image'));
  GRetained := GDynamic.querySelector('svg');
  Check(GGraphic.querySelector('polyline') <> nil, 'browser draws sampled paths');
  Check(GGraphic.querySelector('text').textContent = 'Make connections', 'browser typed text');
  Check(GDynamic.querySelector('text').textContent = 'Custom content', 'nested provider registration copied');
  Check((GGraphic.getAttribute('role') = 'img') and
    (GGraphic.getAttribute('aria-label') = 'Connection graphic'), 'browser named image role');
  {$else}
  Application.ProcessMessages;
  GGraphic := TNyxLCLDrawing(GView.RendererFor(NyxCanvasItem('graphic')).ControlFor('connection-graphic'));
  GDynamic := TNyxLCLDrawing(GView.RendererFor(NyxCanvasItem('provider')).ControlFor('provider-graphic'));
  GMemo := TMemo(GView.InputFor(NyxCanvasItem('memo'), NyxControl('notes-memo')));
  GImage := TImage(GView.RendererFor(NyxCanvasItem('image')).ControlFor('resource-image'));
  GRetained := GDynamic;
  Check(GGraphic.Plan.Count = 4, 'native draws typed display list');
  Check(GGraphic.Plan.Command(2).Field('text').AsText = 'Make connections', 'native typed text');
  Check(GDynamic.Plan.Command(1).Field('text').AsText = 'Custom content', 'nested provider registration copied');
  Check((GGraphic.AccessibleRole = larImage) and
    (TNyxText(GGraphic.AccessibleName) = 'Connection graphic'), 'native named image role');
  {$endif}
  Check(GCalls > 0, 'provider builds actual faces');
  Check(LiveNyxTestDrawingPlans = 0, 'actual target retains admitted geometry after creator retirement');
end;

{ Exercise ordinary renderer synchronization against its realized descriptor,
  leaving the caller document exact. Cached fallback admission must preserve the
  face on invalid edits and never turn a live provider into a cached result. }
procedure FallbackChanges;
var
  LControl: INyxDrawing;
  LBase: INyxDrawingPlan;
  LFailed: Boolean;
  LCalls: Integer;
  {$ifdef PAS2JS}
  LAccepted: TJSElement;
  {$else}
  LAccepted: INyxDrawingPlan;
  {$endif}
begin
  LControl := RetainNyxControl(GView.RendererFor(NyxCanvasItem('graphic')).Root
    .Find('connection-graphic')) as INyxDrawing;
  LBase := LControl.Plan;
  {$ifndef PAS2JS}
  LAccepted := GGraphic.Plan;
  {$endif}
  LControl.WithText('Updated caption');
  GView.RendererFor(NyxCanvasItem('graphic')).Sync;
  {$ifdef PAS2JS}
  Check(GGraphic.getAttribute('aria-label') = 'Updated caption',
    'unchanged fallback still publishes current accessible metadata');
  {$else}
  Check((GGraphic.Plan = LAccepted) and (GGraphic.AccessibleName = 'Updated caption'),
    'unchanged fallback retains exact native geometry while publishing metadata');
  {$endif}
  LControl.WithPlan(NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 320, 180))
    .Text('Updated fallback', NyxCanvasPoint(12, 24), NyxThemePaint(ntcText)).Build);
  GView.RendererFor(NyxCanvasItem('graphic')).Sync;
  {$ifdef PAS2JS}
  LAccepted := GGraphic.querySelector('svg');
  Check((GGraphic.querySelector('text').textContent = 'Updated fallback') and
    (GView.RendererFor(NyxCanvasItem('graphic')).ElementFor('connection-graphic') = GGraphic),
    'changed fallback publishes new geometry in the same browser face');
  {$else}
  LAccepted := GGraphic.Plan;
  Check((GGraphic.Plan.Command(0).Field('text').AsText = 'Updated fallback') and
    (GView.RendererFor(NyxCanvasItem('graphic')).ControlFor('connection-graphic') = GGraphic),
    'changed fallback publishes new geometry in the same native face');
  {$endif}
  LControl.Extensions.SetValue(NyxExtension(NyxDrawingKey), NyxNull);
  LFailed := False;
  try
    GView.RendererFor(NyxCanvasItem('graphic')).Sync;
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  {$ifdef PAS2JS}
  Check(LFailed and (GGraphic.querySelector('svg') = LAccepted),
    'invalid replacement retains the exact accepted SVG');
  {$else}
  Check(LFailed and (GGraphic.Plan = LAccepted),
    'invalid replacement retains the exact accepted native plan');
  {$endif}
  LControl.Extensions.Remove(NyxExtension(NyxDrawingKey));
  GView.RendererFor(NyxCanvasItem('graphic')).Sync;
  {$ifdef PAS2JS}
  Check((GGraphic.querySelector('text') = nil) and (GGraphic.querySelector('polyline') = nil),
    'removed fallback does not revive cached browser geometry');
  {$else}
  Check(GGraphic.Plan.Count = 0, 'removed fallback does not revive cached native geometry');
  {$endif}
  LControl.WithPlan(LBase).WithText('Connection graphic');
  GView.RendererFor(NyxCanvasItem('graphic')).Sync;
  {$ifdef PAS2JS}
  Check(GGraphic.querySelector('text').textContent = 'Make connections',
    'restored authored fallback publishes through the ordinary browser renderer');
  {$else}
  Check(GGraphic.Plan.ToData.ToJSON = LBase.ToData.ToJSON,
    'restored authored fallback publishes its complete exact native geometry');
  {$endif}
  LCalls := GCalls;
  GView.RendererFor(NyxCanvasItem('provider')).Sync;
  GView.RendererFor(NyxCanvasItem('provider')).Sync;
  Check(GCalls >= LCalls + 2, 'unchanged fallback never suppresses fresh live provider calls');
end;

procedure InputAndFailure;
var
  LFailed: Boolean;
  LCamera: TNyxCanvasCamera;
  {$ifndef PAS2JS}
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
  {$endif}
begin
  { Fit only after the target has a real allocated surface. Constructor
    admission at a hidden/zero-size candidate is not a live viewport. }
  GView.Execute(nccFit);
  {$ifdef PAS2JS}
  Check(GImage.complete and (GImage.naturalWidth = 100), 'existing packed image resource loads in canvas');
  Check(GGraphic.getAttribute('aria-label') = 'Connection graphic', 'resize retains the authored accessible caption');
  GMemo.value := 'Ideas remain editable';
  GMemo.dispatchEvent(TJSEvent.new('input'));
  {$else}
  Check(GImage.Picture.Width = 100, 'existing packed image resource loads in canvas');
  GMemo.Text := 'Ideas remain editable';
  Application.ProcessMessages;
  {$endif}
  Check(GState.GetValue(NyxTextState('notes')) = 'Ideas remain editable', 'ordinary canvas input binding');
  LCamera := GView.Camera;
  GView.ZoomAt(LCamera.Zoom * 1.1, NyxCanvasPoint(160, 120));
  Check(GView.Camera.Zoom <> LCamera.Zoom, 'anchored drawing zoom');
  GView.PanBy(8, -5);
  {$ifdef PAS2JS}
  Check(GView.InputFor(NyxCanvasItem('memo'), NyxControl('notes-memo')) = GMemo, 'zoom/pan retains live editor');
  {$else}
  Application.ProcessMessages;
  Check(GView.InputFor(NyxCanvasItem('memo'), NyxControl('notes-memo')) = GMemo, 'zoom/pan retains live editor');
  Check(GHostError = '', 'no unhandled native host exception');
  {$endif}
  FallbackChanges;
  Check(TNyxCodec.Encode(GDocument) = GBefore, 'runtime drawing does not mutate accepted document');
  GReject := True;
  LFailed := False;
  try
    GView.RendererFor(NyxCanvasItem('provider')).Sync;
  except
    on E: Exception do LFailed := Pos('Rejected provider', E.Message) > 0;
  end;
  Check(LFailed, 'provider failure propagates on explicit sync');
  {$ifdef PAS2JS}
  Check(GDynamic.querySelector('text').textContent = 'Custom content', 'failed provider preserves accepted SVG');
  {$else}
  Check(GDynamic.Plan.Command(1).Field('text').AsText = 'Custom content', 'failed provider preserves accepted native plan');
  {$endif}
  LFailed := False;
  try
    GRenderer.Render(GDocument, GDocument.Pages[0], GHost, False, GState);
  except
    on E: Exception do LFailed := Pos('Rejected provider', E.Message) > 0;
  end;
  Check(LFailed and GView.Connected, 'failed ordinary candidate retains the mounted canvas');
  Check(GView.InputFor(NyxCanvasItem('memo'), NyxControl('notes-memo')) = GMemo,
    'failed candidate retains exact live editor');
  GReject := False;
  Check(GDestroyed = 0, 'provider retained by registered and mounted owners');
  {$ifndef PAS2JS}

  if ParamCount = 1 then
  begin
    LBitmap := GHost.GetFormImage;
    LPNG := TPortableNetworkGraphic.Create;
    try
      LPNG.Assign(LBitmap);
      LPNG.SaveToFile(ParamStr(1));
    finally
      LPNG.Free;
      LBitmap.Free;
    end;
  end;
  {$endif}
end;

procedure Finish;
begin
  GView.Disconnect;
  GBeforeCalls := GCalls;
  Cleanup;
  Check(GDestroyed = 1, 'all target providers release after renderer retirement');
  {$ifdef PAS2JS}
  document.body.setAttribute('data-drawing-controls', 'retiring');
  {$else}
  Check(GHostError = '', 'clean native retirement');
  WriteLn('PASS ', GChecks, ' actual drawing/control/resource checks');
  {$endif}
end;

{$ifdef PAS2JS}
procedure Fail(const AReason: TNyxText);
begin
  document.body.setAttribute('data-drawing-controls', 'failed');
  document.body.setAttribute('data-drawing-controls-error', AReason);
  Cleanup;
end;

procedure AfterRetirement;
begin
  try
    Check(GCalls = GBeforeCalls, 'real browser turns retire resize callbacks');
    GQualifiedChecks := GChecks;
    { A fresh ordinary mount remains for visual review after the retired test
      owner has passed. It is not used to assert old-face identity/lifetime. }
    Setup;
    window.setTimeout(procedure
      begin
        GView.Execute(nccFit);
        document.body.setAttribute('data-drawing-preview', 'true');
        document.body.setAttribute('data-drawing-controls', 'passed');
        document.body.setAttribute('data-drawing-controls-checks', IntToStr(GQualifiedChecks));
      end, 120);
  except
    on E: Exception do Fail(E.Message);
  end;
end;

procedure AfterLayout;
begin
  try
    InputAndFailure;
    { Keep the rendered scene visible for the ordinary capture. Retirement is
      checked on the next turn; host SVG is removed by renderer ownership. }
    document.body.setAttribute('data-drawing-controls-stage', 'input-complete');
    Finish;
    window.setTimeout(@AfterRetirement, 120);
  except
    on E: Exception do Fail(E.Message);
  end;
end;
{$endif}

begin
  try
    Setup;
    {$ifdef PAS2JS}
    window.setTimeout(@AfterLayout, 180);
    {$else}
    InputAndFailure;
    Finish;
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      Fail(E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      Cleanup;
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
