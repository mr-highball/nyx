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
program nyx_hosted_image_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.bytes, nyx.data, nyx.state,
  nyx.model, nyx.controls, nyx.images, nyx.image.fixtures, nyx.resources,
  nyx.resource.sources, nyx.resource.cache, nyx.resources.loader,
  nyx.application.resources, nyx.resource.context, nyx.scheduler,
  nyx.diagram, nyx.diagram.declarations, nyx.diagram.content,
  nyx.diagram.editor, nyx.canvas.geometry, nyx.canvas.scene, nyx.canvas.workspace,
  nyx.canvas.declarations, nyx.canvas.types, nyx.root.types,
  nyx.codec, nyx.codegen, nyx.source, nyx.studio.session, nyx.studio.projects
  {$ifdef PAS2JS}, JS, Web, nyx.diagram.editor.browser, nyx.canvas.browser, nyx.application.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, Graphics, Windows, nyx.images.lcl,
    nyx.diagram.editor.lcl, nyx.canvas.lcl, nyx.test.capture.lcl, nyx.application.lcl
  {$endif};

type
  { Alias exercises the real application owner and its ordinary saved Canvas
    projection. The test owns/disposes it; resource receivers retire first. }
  TTestApplication = {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif};
  { Deterministic transport token owns its callback until Complete/Cancel.
    It supplies real PNG bytes to the ordinary resolver, never a loaded model. }
  TReply = class(TNyxResourceRequest)
  public
    procedure Send(const ABytes: TNyxBytes; const AError: TNyxText = '');
  end;

  { The test retains pending platform tokens deliberately to exercise late
    completion. No HTTP request is issued; decoding/cache/publication are real. }
  TTransport = class(TInterfacedObject, INyxResourceTransport)
  private
    FLeases: array of INyxResourceRequest;
    FRequests: array of TReply;
  public
    function Request(const AURL: TNyxResourceURL;
      const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
    function Count: Integer;
    procedure Send(AIndex: Integer; const ABytes: TNyxBytes; const AError: TNyxText = '');
  end;

  { Test-owned borrowed validator exercises deferred publication. Its token
    disconnects before destruction; it never retains the broker or its views. }
  TGate = class
  public
    Blocked: Boolean;
    function Accept(const AContext: INyxResourceContext): Boolean;
  end;

  {$ifndef PAS2JS}
  { Test exception receiver outlives every native deferred UI operation. }
  TFailures = class
  public
    Message: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;
  {$endif}

var
  GChecks: Integer;
  GDocument: TNyxDocument;
  GWire: TNyxText;
  GSource: TNyxText;
  GScheduler: INyxScheduler;
  GResources: INyxApplicationResources;
  GOtherResources: INyxApplicationResources;
  GApplication: TTestApplication;
  GTransport: TTransport;
  GTransportLease: INyxResourceTransport;
  GGate: TGate;
  GGateToken: INyxResourceSubscription;
  GEditor: INyxDiagramEditor;
  GView: INyxDiagramEditorView;
  GOtherView: INyxDiagramEditorView;
  GContent: INyxDiagramContentView;
  GOtherContent: INyxDiagramContentView;
  GCanvas: {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  GOtherCanvas: {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  GHost: {$ifdef PAS2JS}TJSHTMLElement{$else}TForm{$endif};
  GOtherHost: {$ifdef PAS2JS}TJSHTMLElement{$else}TForm{$endif};
  {$ifndef PAS2JS}
  GFailures: TFailures;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if GFailures.Message <> '' then
  begin
    raise Exception.Create(GFailures.Message);
  end;
  {$endif}

  if not AValue then
  begin
    raise Exception.Create('Hosted image controls: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TReply.Send(const ABytes: TNyxBytes; const AError: TNyxText);
var
  LReply: TNyxResourceHTTPResult;
begin
  LReply := Default(TNyxResourceHTTPResult);
  LReply.Status := 200;
  LReply.Bytes := Copy(ABytes);
  LReply.Error := AError;
  LReply.Hints := NyxResourceCacheHeaders('', '');
  Complete(LReply);
end;

function TTransport.Request(const AURL: TNyxResourceURL;
  const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
var
  LRequest: TReply;
  LIndex: Integer;
begin
  LRequest := TReply.Create(AReply);
  Result := LRequest;
  LIndex := Length(FLeases);
  SetLength(FLeases, LIndex + 1);
  SetLength(FRequests, LIndex + 1);
  FLeases[LIndex] := Result;
  FRequests[LIndex] := LRequest;
end;

function TTransport.Count: Integer;
begin
  Result := Length(FLeases);
end;

procedure TTransport.Send(AIndex: Integer; const ABytes: TNyxBytes; const AError: TNyxText);
begin
  FRequests[AIndex].Send(ABytes, AError);
end;

function TGate.Accept(const AContext: INyxResourceContext): Boolean;
begin
  Result := not Blocked;
end;

{$ifndef PAS2JS}
procedure TFailures.Failed(ASender: TObject; AError: Exception);
begin
  Message := AError.ClassName + ': ' + AError.Message;
end;
{$endif}

{$ifdef PAS2JS}
{ Normal browser turns permit real image decoding and UI publication. No private
  scheduler draining or substituted decoder is used. This fixture is compiled
  when hosting is unavailable; compilation alone is not execution evidence. }
procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 25);
end;
{$endif}

procedure Idle; {$ifdef PAS2JS}async;{$endif}
{$ifndef PAS2JS}
var
  LTurn: Integer;
{$endif}
begin
  {$ifdef PAS2JS}
  await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  {$else}
  for LTurn := 1 to 3 do
  begin
    Application.ProcessMessages;
    CheckSynchronize;
    Sleep(10);
  end;
  {$endif}
end;

function FindKind(ARoot: TNyxNode; AKind: TNyxKind): TNyxNode;
var
  LIndex: Integer;
begin
  Result := nil;

  if ARoot.Kind = NyxKindName(AKind) then
  begin
    Exit(ARoot);
  end;
  for LIndex := 0 to ARoot.Count - 1 do
  begin
    Result := FindKind(ARoot.Children[LIndex], AKind);

    if Result <> nil then
    begin
      Exit;
    end;
  end;
end;

function ImageNode(const AVertex: TNyxText; AOther: Boolean = False): TNyxNode;
begin

  if AOther then
  begin
    Result := FindKind(GOtherCanvas.RendererFor(GOtherContent.ContentItem(
      NyxDiagramVertexRef(AVertex))).Root, nkImage);
  end
  else
  begin
    Result := FindKind(GCanvas.RendererFor(GContent.ContentItem(
      NyxDiagramVertexRef(AVertex))).Root, nkImage);
  end;
end;

function HasImage(const AVertex: TNyxText; AOther: Boolean = False): Boolean;
var
  LNode: TNyxNode;
  {$ifdef PAS2JS}
  LImage: TJSHTMLImageElement;
  {$else}
  LImage: TNyxLCLImage;
  {$endif}
begin
  LNode := ImageNode(AVertex, AOther);
  {$ifdef PAS2JS}

  if AOther then
  begin
    LImage := TJSHTMLImageElement(GOtherCanvas.RendererFor(GOtherContent.ContentItem(
      NyxDiagramVertexRef(AVertex))).ElementFor(LNode.ID));
  end
  else
  begin
    LImage := TJSHTMLImageElement(GCanvas.RendererFor(GContent.ContentItem(
      NyxDiagramVertexRef(AVertex))).ElementFor(LNode.ID));
  end;
  Result := LImage.complete and (LImage.naturalWidth = 100) and (LImage.naturalHeight = 50);
  {$else}

  if AOther then
  begin
    LImage := TNyxLCLImage(GOtherCanvas.RendererFor(GOtherContent.ContentItem(
      NyxDiagramVertexRef(AVertex))).ControlFor(LNode.ID));
  end
  else
  begin
    LImage := TNyxLCLImage(GCanvas.RendererFor(GContent.ContentItem(
      NyxDiagramVertexRef(AVertex))).ControlFor(LNode.ID));
  end;
  Result := (LImage.Picture.Width = 100) and (LImage.Picture.Height = 50);
  {$endif}
end;

procedure Build;
var
  LForm: INyxColumn;
  LHome: INyxColumn;
  LImage: INyxImage;
  LPlan: INyxDiagram;
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSelector: TNyxResourceImageRef;
  LValue: TNyxImageSource;
  LFailed: Boolean;
begin
  GDocument := TNyxDocument.Create;
  GDocument.Title := 'Hosted artwork workshop';
  GDocument.Resources.Define(NyxResourceRef('Artwork'),
    NyxHostedResource(nrkImage, NyxResourceURL('https://example.com/artwork.png'))
      .Cache(NyxResourceCache.Bypass));
  GDocument.Resources.Define(NyxResourceRef('Artwork'), NyxLocale('en-GB'),
    NyxImageResource(NyxEmbeddedImage(nimJPEG, ImageJPEG)));
  GDocument.Resources.Define(NyxResourceRef('Text only'), NyxTextResource('English copy'));
  LSelector := NyxResourceImage(NyxResourceRef('Artwork'));
  LSelector.Validate(GDocument.Resources, NyxDefaultLocale, NyxDefaultLocale);
  Check(not LSelector.TryRead(GDocument.Resources, NyxDefaultLocale, NyxDefaultLocale, LValue)
    and (LValue.ToWire = ''), 'valid unresolved hosted metadata has explicit unavailable image');
  LFailed := False;
  try
    LValue := LSelector.Read(GDocument.Resources, NyxDefaultLocale, NyxDefaultLocale);
  except
    on ENyxResource do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'strict synchronous read still refuses unavailable bytes');
  Check(LSelector.Localize(NyxLocale('en-GB'), NyxDefaultLocale)
    .TryRead(GDocument.Resources, NyxDefaultLocale, NyxDefaultLocale, LValue)
    and (LValue.Format = nimJPEG), 'pinned locale overrides runtime locale for readiness');
  LFailed := False;
  try
    NyxResourceImage(NyxResourceRef('Text only')).TryRead(GDocument.Resources,
      NyxDefaultLocale, NyxDefaultLocale, LValue);
  except
    on ENyxResource do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'wrong-kind selector raises rather than masquerading as pending');
  LFailed := False;
  try
    NyxResourceImage(NyxResourceRef('Missing')).Validate(GDocument.Resources,
      NyxDefaultLocale, NyxDefaultLocale);
  except
    on ENyxResource do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'missing metadata refuses before any target or transport');
  LImage := NewNyxImage('page-artwork');
  LImage.Binds.Image(LSelector);
  LImage.Configure.Height(100).Done;
  LHome := NewNyxColumn('home');
  LHome.Add(LImage);
  LHome.Add(NewNyxCanvas('saved-workspace').WithScene(NyxCanvasRef('Artwork scene'))
    .Configure.Height(500).Done);
  GDocument.AddPage(LHome);
  LForm := NewNyxColumn('Artwork form');
  LImage := NewNyxImage('form-artwork');
  LImage.Configure.Height(100).Done;
  LImage.Binds.Image(LSelector);
  LForm.Add(LImage);
  LForm.Add(NewNyxMemo('notes').WithText('Notes for the artwork'));
  GDocument.AddComponent(LForm);
  GDocument.Canvases.Define(NyxCanvasRef('Artwork scene'), NewNyxCanvasDefinition
    .WithCamera(NyxCanvasCamera(700, 400, NyxCanvasPoint(0, 0), 1,
      NyxCanvasZoomLimits(0.25, 4)))
    .WithLayer(NyxCanvasLayer('Cards'))
    .WithItem(NyxCanvasItem('Artwork card'), NyxReusableRoot('Artwork form'),
      NyxCanvasPlacement(NyxCanvasLayer('Cards'), NyxCanvasBounds(-140, -100, 280, 240))));
  LPlan := NewNyxDiagram
    .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('Artwork'), ndsRectangle,
      NyxCanvasBounds(-150, -130, 300, 220)).Captioned('Hosted artwork')
      .WithContent(NyxDiagramImage(LSelector)))
    .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('Form'), ndsRectangle,
      NyxCanvasBounds(190, -130, 300, 300)).Captioned('Live resource binding')
      .WithContent(NyxDiagramComponent(NyxComponent('Artwork form'))));
  GDocument.Diagrams.Define(NyxDiagramRef('Artwork board'), LPlan);
  GDocument.Validate;
  GWire := TNyxCodec.Encode(GDocument);
  LRead := TNyxCodec.Decode(GWire);
  try
    Check(TNyxCodec.Encode(LRead) = GWire, 'unresolved image bindings round-trip exact declarations');
  finally
    LRead.Free;
  end;
  GSource := TNyxCodegen.Generate(GDocument);
  LWorkspace := nil;
  LRead := TNyxSourceWorkspace.PrepareDraft(GSource, LWorkspace);
  try
    Check(TNyxCodec.Encode(LRead) = GWire, 'crafted Pascal admits the entire pending-image document');
  finally
    LRead.Free;
    LWorkspace.Free;
  end;
end;

{$ifndef PAS2JS}
{ Export only this test-owned exact pair. The existing whole-design consumer
  compiles its original companion independently; source admission is a separate
  assertion from compilation and constructor execution. }
procedure ExportPair;
var
  LDirectory: TNyxText;

  procedure Save(const AName, AText: TNyxText);
  var
    LBytes: TNyxBytes;
    LFile: TFileStream;
  begin
    LBytes := NyxEncodeUTF8(AText);
    LFile := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if Length(LBytes) > 0 then
      begin
        LFile.WriteBuffer(LBytes[0], Length(LBytes));
      end;
    finally
      LFile.Free;
    end;
  end;

begin

  if ParamStr(3) = '' then
  begin
    Exit;
  end;
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(3));
  ForceDirectories(LDirectory);
  Save('nyx.generated.view.pas', GSource);
  Save('expected.nyx', GWire);
end;
{$endif}

procedure Setup;
var
  LResolver: INyxResourceResolver;
  LOtherEditor: INyxDiagramEditor;
begin
  GScheduler := NewNyxScheduler;
  GTransport := TTransport.Create;
  GTransportLease := GTransport;
  LResolver := NewNyxResourceResolver(GTransportLease);
  GResources := NewNyxApplicationResources(GDocument.Resources, GScheduler,
    NyxApplicationResourceOptions, LResolver);
  GOtherResources := NewNyxApplicationResources(GDocument.Resources, GScheduler,
    NyxApplicationResourceOptions.Loading(nrlOnDemand), LResolver);
  GEditor := NewNyxDiagramEditor('hosted-artwork', GDocument, NyxDiagramRef('Artwork board'));
  LOtherEditor := NewNyxDiagramEditor('independent-artwork', GDocument, NyxDiagramRef('Artwork board'));
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('height', '100dvh');
  document.body.appendChild(GHost);
  GOtherHost := TJSHTMLElement(document.createElement('div'));
  GOtherHost.style.setProperty('height', '700px');
  document.body.appendChild(GOtherHost);
  GView := MountNyxBrowserDiagramEditor(GEditor, GDocument, GHost, nil, nil, ndcmRuntime,
    nil, nil, GResources);
  GOtherView := MountNyxBrowserDiagramEditor(LOtherEditor, GDocument, GOtherHost, nil, nil,
    ndcmRuntime, nil, nil, GOtherResources);
  Check(Supports(GView.Canvas, INyxBrowserCanvasWorkspaceView, GCanvas), 'browser canvas available');
  Check(Supports(GOtherView.Canvas, INyxBrowserCanvasWorkspaceView, GOtherCanvas), 'independent browser canvas available');
  {$else}
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(20, 20, 1080, 760);

  if ParamStr(2) = '--compact' then
  begin
    GHost.SetBounds(20, 20, 390, 800);
  end;
  GHost.Caption := 'Nyx hosted artwork';
  GHost.Show;
  GOtherHost := TForm.CreateNew(nil);
  GOtherHost.SetBounds(40, 40, 800, 700);
  GOtherHost.Show;
  GView := MountNyxLCLDiagramEditor(GEditor, GDocument, GHost, nil, nil, ndcmRuntime,
    nil, nil, GResources);
  GOtherView := MountNyxLCLDiagramEditor(LOtherEditor, GDocument, GOtherHost, nil, nil,
    ndcmRuntime, nil, nil, GOtherResources);
  Check(Supports(GView.Canvas, INyxLCLCanvasWorkspaceView, GCanvas), 'native canvas available');
  Check(Supports(GOtherView.Canvas, INyxLCLCanvasWorkspaceView, GOtherCanvas), 'independent native canvas available');
  {$endif}
  Check(Supports(GView, INyxDiagramContentView, GContent), 'typed diagram content available');
  Check(Supports(GOtherView, INyxDiagramContentView, GOtherContent), 'independent typed content available');
  GView.Canvas.SetCamera(GView.Canvas.Camera.AtCenter(NyxCanvasPoint(150, 20)));
  GOtherView.Canvas.SetCamera(GOtherView.Canvas.Camera.AtCenter(NyxCanvasPoint(150, 20)));
  GGate := TGate.Create;
  GGate.Blocked := True;
  GGateToken := GResources.Subscribe({$ifdef PAS2JS}@{$endif}GGate.Accept, nil);
end;

procedure AwaitPhase(APhase: TNyxApplicationResourcePhase;
  const AResources: INyxApplicationResources = nil); {$ifdef PAS2JS}async;{$endif}
var
  LTries: Integer;
  LResources: INyxApplicationResources;
begin
  LTries := 0;
  LResources := AResources;

  if LResources = nil then
  begin
    LResources := GResources;
  end;
  while LResources.Status(NyxResourceRef('Artwork'), NyxDefaultLocale).Phase <> APhase do
  begin
    Inc(LTries);

    if LTries > 100 then
    begin
      raise Exception.Create('Timed out waiting for ' + NyxResourcePhaseName(APhase) +
        '; actual ' + NyxResourcePhaseName(LResources.Status(NyxResourceRef('Artwork'),
          NyxDefaultLocale).Phase) + ': ' + LResources.Status(NyxResourceRef('Artwork'),
          NyxDefaultLocale).Error);
    end;
    {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  end;
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LFirst: TNyxNode;
  LNested: TNyxNode;
  LMemo: TNyxNode;
  LFace: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  LRevision: Integer;
  LFailed: Boolean;
  LSession: TNyxStudioSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LExpected: TNyxDocument;
  LApplicationCanvas: {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  LApplicationImage: TNyxNode;
  LPendingImage: TNyxImageSource;
  {$ifdef PAS2JS}
  LApplicationHost: TJSHTMLElement;
  {$endif}
  {$ifndef PAS2JS}
  LBitmap: Graphics.TBitmap;
  {$endif}
begin
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check(not HasImage('Artwork') and not HasImage('Form'), 'both actual targets start empty, without a fake fallback');
  Check(not NyxApplicationResourceDiagnostics(GResources).CaptureRuntime.Entry(0).HasPublishedLoad,
    'empty image is not reported as a completed hosted request');
  LFirst := ImageNode('Artwork');
  LNested := ImageNode('Form');
  LMemo := FindKind(GCanvas.RendererFor(GContent.ContentItem(NyxDiagramVertexRef('Form'))).Root, nkMemo);
  LFace := GCanvas.RendererFor(GContent.ContentItem(NyxDiagramVertexRef('Form'))).InputFor(LMemo.ID);
  {$ifdef PAS2JS}
  TJSHTMLTextAreaElement(LFace).value := 'Unfinished notes';
  LFace.dispatchEvent(TJSEvent.new('input'));
  TJSHTMLTextAreaElement(LFace).selectionStart := 4;
  TJSHTMLTextAreaElement(LFace).selectionEnd := 4;
  {$else}
  TMemo(LFace).Text := 'Unfinished notes';
  TMemo(LFace).SelStart := 4;
  {$endif}
  GResources.Start;
  {$ifdef PAS2JS}await(AwaitPhase(nrpLoading));{$else}AwaitPhase(nrpLoading);{$endif}
  Check(GTransport.Count = 1, 'one existing resolver request serves both images');
  GTransport.Send(0, NyxDecodeBase64(ImagePNG, 1024 * 1024));
  {$ifdef PAS2JS}await(AwaitPhase(nrpWaiting));{$else}AwaitPhase(nrpWaiting);{$endif}
  Check(not HasImage('Artwork') and not HasImage('Form'), 'blocked publication retains both empty accepted targets');
  GGate.Blocked := False;
  GResources.Wake;
  {$ifdef PAS2JS}await(AwaitPhase(nrpReady));await(Idle);{$else}AwaitPhase(nrpReady);Idle;{$endif}
  Check(HasImage('Artwork') and HasImage('Form'), 'prepared resource publication decodes both actual target controls');
  Check(ImageNode('Artwork').ReadResourceImage(NyxResourceImage(NyxResourceRef('Artwork'))).ToWire =
    NyxEmbeddedImage(nimPNG, ImagePNG).ToWire, 'mounted context retains the exact loaded PNG');
  {$ifndef PAS2JS}
  LBitmap := Graphics.TBitmap.Create;
  try
    LBitmap.Assign(TNyxLCLImage(GCanvas.RendererFor(GContent.ContentItem(
      NyxDiagramVertexRef('Artwork'))).ControlFor(LFirst.ID)).Picture.Graphic);
    Check((ColorToRGB(LBitmap.Canvas.Pixels[5, 5]) = ColorToRGB(clRed)) and
      (ColorToRGB(LBitmap.Canvas.Pixels[95, 5]) = ColorToRGB(clBlue)),
      'real native decoding preserves the independent PNG pixel oracle');
  finally
    LBitmap.Free;
  end;
  {$endif}
  Check((ImageNode('Artwork') = LFirst) and (ImageNode('Form') = LNested),
    'loaded pixels retain both mounted node identities');
  Check(GCanvas.RendererFor(GContent.ContentItem(NyxDiagramVertexRef('Form'))).InputFor(LMemo.ID) = LFace,
    'publication keeps the same physical memo');
  Check(({$ifdef PAS2JS}TJSHTMLTextAreaElement(LFace).value{$else}TMemo(LFace).Text{$endif} = 'Unfinished notes')
    and ({$ifdef PAS2JS}TJSHTMLTextAreaElement(LFace).selectionStart{$else}TMemo(LFace).SelStart{$endif} = 4),
    'resource changes retain unfinished nested text and caret');
  Check(not HasImage('Artwork', True) and not HasImage('Form', True),
    'another runtime of the same document stays unresolved');
  Check(NyxApplicationResourceDiagnostics(GResources).CaptureRuntime.Entry(0).HasPublishedLoad,
    'broker reports installed provenance only after publication');
  GView.AddVertex(NyxDiagramVertex(NyxDiagramVertexRef('Later'), ndsRectangle,
    NyxCanvasBounds(-150, 180, 300, 180)).Captioned('Added after loading')
    .WithContent(NyxDiagramImage(NyxResourceImage(NyxResourceRef('Artwork')))), GView.Revision);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check(HasImage('Later') and (GTransport.Count = 1),
    'new item mounts from the accepted runtime context without another request');
  LRevision := GView.Revision;
  LFailed := False;
  try
    GContent.SetContent(NyxDiagramVertexRef('Artwork'),
      NyxDiagramImage(NyxResourceImage(NyxResourceRef('Text only'))), LRevision);
  except
    on Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GView.Revision = LRevision) and (ImageNode('Artwork') = LFirst)
    and HasImage('Artwork'), 'invalid replacement refuses before changing graph or mounted loaded image');
  GResources.Reload;
  {$ifdef PAS2JS}await(AwaitPhase(nrpLoading));{$else}AwaitPhase(nrpLoading);{$endif}
  Check(GTransport.Count = 2, 'explicit bypass reload resolves original hosted declaration');
  GTransport.Send(1, NyxEncodeUTF8('not an image'));
  {$ifdef PAS2JS}await(AwaitPhase(nrpFailed));{$else}AwaitPhase(nrpFailed);{$endif}
  Check(HasImage('Artwork') and HasImage('Form') and HasImage('Later'),
    'failed reload retains the previous loaded publication in every scope');
  Check(GResources.Status(NyxResourceRef('Artwork'), NyxDefaultLocale).Error <> '',
    'failed decode remains observable through ordinary resource diagnostics');
  GResources.Reload;
  {$ifdef PAS2JS}await(AwaitPhase(nrpLoading));{$else}AwaitPhase(nrpLoading);{$endif}
  GResources.Cancel;
  GTransport.Send(2, NyxDecodeBase64(ImageJPEG, 1024 * 1024));
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check((GResources.Status(NyxResourceRef('Artwork'), NyxDefaultLocale).Phase = nrpCancelled)
    and HasImage('Artwork') and (ImageNode('Artwork') = LFirst),
    'cancelled late completion cannot replace retained targets');
  GResources.Localize(NyxLocale('en-GB'), NyxDefaultLocale);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check(HasImage('Artwork') and (ImageNode('Artwork') = LFirst),
    'explicit locale publication keeps native image identity');
  Check(ImageNode('Artwork').ReadResourceImage(NyxResourceImage(NyxResourceRef('Artwork'))).Format = nimJPEG,
    'mounted resource context advances with its exact localized payload');
  Check((TNyxCodec.Encode(GDocument) = GWire) and (TNyxCodegen.Generate(GDocument) = GSource),
    'loading, local geometry and locale leave original design and crafted source byte exact');
  LBefore := NyxProjectPair(GWire, GSource);
  LSession := TNyxStudioSession.Create(LBefore);
  try
    LSession.SetSourceDraft(StringReplace(GSource, 'Hosted artwork workshop', 'Artwork review', []));
    LSession.ApplySourceDraft;
    LAfter := LSession.ProjectSnapshot;
    LExpected := GDocument.Clone;
    try
      LExpected.Title := 'Artwork review';
      Check((LAfter.Design = TNyxCodec.Encode(LExpected)) and
        (LAfter.Source = StringReplace(GSource, 'Hosted artwork workshop', 'Artwork review', [])),
        'ordinary source Apply preserves the complete expected document and exact pending-image Pascal');
    finally
      LExpected.Free;
    end;
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'ordinary Undo restores the entire pending-image design/source pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'ordinary Redo restores the complete accepted pending-image design/source pair');
  finally
    LSession.Free;
  end;
  {$ifndef PAS2JS}

  if (ParamStr(1) <> '') and (ParamStr(1) <> '--no-capture') then
  begin
    GOtherHost.Hide;
    GHost.BringToFront;
    Windows.SetForegroundWindow(GHost.Handle);
    Idle;
    GHost.Update;
    SaveNyxNativeCapture(GHost, ParamStr(1) + '.png', ncmDisplay);
  end;
  {$endif}
  GGateToken.Disconnect;
  GGateToken := nil;
  GView.Disconnect;
  GResources.Reload;
  {$ifdef PAS2JS}await(AwaitPhase(nrpLoading));{$else}AwaitPhase(nrpLoading);{$endif}
  GTransport.Send(3, NyxDecodeBase64(ImagePNG, 1024 * 1024));
  {$ifdef PAS2JS}await(AwaitPhase(nrpReady));{$else}AwaitPhase(nrpReady);{$endif}
  Check(not GView.Connected and not HasImage('Artwork', True),
    'disconnected view receiver is revoked while independent runtime remains intact');
  GApplication := TTestApplication.Create;
  GApplication.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand),
    NewNyxResourceResolver(GTransportLease));
  {$ifdef PAS2JS}
  LApplicationHost := TJSHTMLElement(document.createElement('div'));
  LApplicationHost.style.setProperty('height', '900px');
  document.body.appendChild(LApplicationHost);
  GApplication.Run(GDocument, LApplicationHost);
  {$else}
  GApplication.Mount(GDocument);
  GApplication.Window.SetBounds(20, 20, GHost.Width, GHost.Height);
  GApplication.Window.Show;
  {$endif}
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check(Supports(GApplication.View.CanvasView('saved-workspace'),
    {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif},
    LApplicationCanvas), 'ordinary application mounts its saved public Canvas');
  Check(LApplicationCanvas.MountedCount > 0, 'ordinary application canvas has visible content; viewport ' +
    TNyxText(FloatToStr(LApplicationCanvas.Camera.ViewportWidth)) + ' by ' +
    TNyxText(FloatToStr(LApplicationCanvas.Camera.ViewportHeight)) + ', center ' +
    TNyxText(FloatToStr(LApplicationCanvas.Camera.WorldCenter.X)) + ', ' +
    TNyxText(FloatToStr(LApplicationCanvas.Camera.WorldCenter.Y)) + '; actual surface ' +
    TNyxText(IntToStr(LApplicationCanvas.Surface.ClientWidth)) + ' by ' +
    TNyxText(IntToStr(LApplicationCanvas.Surface.ClientHeight)));
  LApplicationImage := FindKind(LApplicationCanvas.RendererFor(NyxCanvasItem('Artwork card')).Root, nkImage);
  Check(LApplicationImage <> nil, 'ordinary saved canvas realizes its reusable image');
  Check(not LApplicationImage.TryReadResourceImage(NyxResourceImage(NyxResourceRef('Artwork')),
    LPendingImage) and (LPendingImage.Kind = nisEmpty),
    'application canvas is unresolved before any explicit load');
  GApplication.Resources.Reload;
  {$ifdef PAS2JS}await(AwaitPhase(nrpLoading, GApplication.Resources));{$else}AwaitPhase(nrpLoading, GApplication.Resources);{$endif}
  Check(GTransport.Count = 5, 'ordinary application owns an independent request through the same transport');
  GTransport.Send(4, NyxDecodeBase64(ImagePNG, 1024 * 1024));
  {$ifdef PAS2JS}await(AwaitPhase(nrpReady, GApplication.Resources));await(Idle);{$else}AwaitPhase(nrpReady, GApplication.Resources);Idle;{$endif}
  Check(FindKind(LApplicationCanvas.RendererFor(NyxCanvasItem('Artwork card')).Root, nkImage) = LApplicationImage,
    'automatic resource handoff retains the ordinary application canvas image');
  Check(LApplicationImage.TryReadResourceImage(NyxResourceImage(NyxResourceRef('Artwork')), LPendingImage),
    'ordinary saved canvas receives the automatic application publication');
  Check(LPendingImage.ToWire =
    NyxEmbeddedImage(nimPNG, ImagePNG).ToWire, 'automatic canvas resource context contains exact loaded bytes');
  {$ifdef PAS2JS}
  Check(TJSHTMLImageElement(LApplicationCanvas.RendererFor(NyxCanvasItem('Artwork card'))
    .ElementFor(LApplicationImage.ID)).naturalWidth = 100, 'ordinary application canvas decodes actual browser pixels');
  {$else}
  Check(TNyxLCLImage(LApplicationCanvas.RendererFor(NyxCanvasItem('Artwork card'))
    .ControlFor(LApplicationImage.ID)).Picture.Width = 100, 'ordinary application canvas decodes actual native pixels');
  {$endif}
  Check(TNyxCodec.Encode(GDocument) = GWire, 'ordinary application retains exact original resource/scene declarations');
  FreeAndNil(GApplication);
  LApplicationCanvas := nil;
  {$ifdef PAS2JS}
  LApplicationHost.remove;
  {$endif}
end;

procedure Cleanup;
begin
  FreeAndNil(GApplication);

  if GGateToken <> nil then
  begin
    GGateToken.Disconnect;
  end;

  if GResources <> nil then
  begin
    GResources.Stop;
  end;

  if GOtherResources <> nil then
  begin
    GOtherResources.Stop;
  end;

  if GView <> nil then
  begin
    GView.Disconnect;
  end;

  if GOtherView <> nil then
  begin
    GOtherView.Disconnect;
  end;
  GContent := nil;
  GOtherContent := nil;
  GCanvas := nil;
  GOtherCanvas := nil;
  GView := nil;
  GOtherView := nil;
  GEditor := nil;
  GGateToken := nil;
  GGate.Free;
  GResources := nil;
  GOtherResources := nil;
  GTransportLease := nil;
  if GScheduler <> nil then
  begin
    GScheduler.Shutdown;
  end;
  GScheduler := nil;
  GDocument.Free;
  {$ifdef PAS2JS}
  GHost.remove;
  GOtherHost.remove;
  {$else}
  GHost.Free;
  GOtherHost.Free;
  Application.OnException := nil;
  GFailures.Free;
  {$endif}
end;

{$ifdef PAS2JS}
procedure Start; async;
begin
  try
    try
      Build;
      Setup;
      await(Run);
      document.body.setAttribute('data-hosted-images', 'pass');
      document.body.setAttribute('data-hosted-images-checks', IntToStr(GChecks));
    finally
      Cleanup;
    end;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-hosted-images', 'failed');
      document.body.setAttribute('data-hosted-images-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}
  window.setTimeout(@Start, 100);
  {$else}
  Application.Initialize;
  GFailures := TFailures.Create;
  Application.OnException := GFailures.Failed;
  try
    try
      Build;
      ExportPair;
      Setup;
      Run;
    finally
      Cleanup;
    end;
    WriteLn('PASS ', GChecks, ' actual hosted image checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
