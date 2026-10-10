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
program nyx_resource_image_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.bytes, nyx.data, nyx.state,
  nyx.model, nyx.catalog, nyx.controls, nyx.images, nyx.image.fixtures, nyx.image.lifecycle,
  nyx.resources, nyx.resource.sources, nyx.resources.loader, nyx.application.resources,
  nyx.resource.context, nyx.binding.types, nyx.binding, nyx.codec, nyx.codegen,
  nyx.source, nyx.composition, nyx.events, nyx.behavior, nyx.scheduler,
  nyx.resources.editor, nyx.studio.resources, nyx.studio.resourceedits,
  nyx.studio.session, nyx.studio.projects, nyx.studio.agents, nyx.generated.view
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Forms, Controls, ExtCtrls, Graphics, FPWritePNG, LCLIntf,
    nyx.application.lcl{$endif};

type
  TTestApplication = {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif};
  TJourney = class;
  { The callback borrows the journey. Its application/router closes before the
    journey is freed; retained observations contain values only. }
  TReadyCallback = class(TNyxEventCallback)
  private
    FJourney: TJourney;
  public
    constructor Create(AJourney: TJourney);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  TJourney = class
  private
    FDocument: TNyxDocument;
    FApplication: TTestApplication;
    FOther: TTestApplication;
    FResources: INyxApplicationResources;
    FSubscription: INyxEventSubscription;
    FBefore: TNyxText;
    FReadyCount: Integer;
    FReadyBefore: Integer;
    FReadyJPEG: Boolean;
    FStage: Integer;
    FDeadline: Double;
    FRetiredCount: Integer;
    FRetiredAt: Double;
    FFinished: Boolean;
    procedure Mount(AApplication: TTestApplication);
    function FacesReady(AApplication: TTestApplication; ALocalized: Boolean;
      AInitial: Boolean = False): Boolean;
    procedure CheckFaces(AApplication: TTestApplication; ALocalized: Boolean;
      AInitial: Boolean = False);
    function Loaded(const AName: TNyxText): Boolean;
    procedure Next;
    procedure Screenshot;
  public
    Checks: Integer;
    constructor Create(const ABaseURL: TNyxText);
    destructor Destroy; override;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Step;
    property Finished: Boolean read FFinished;
  end;

var
  GJourney: TJourney;

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

constructor TReadyCallback.Create(AJourney: TJourney);
begin
  inherited Create;
  FJourney := AJourney;
end;

procedure TReadyCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  FJourney.Check(AEvent.HasImage and AEvent.Image.Defined and
    (AEvent.Image.Phase = nipReady), 'image binding delivers a typed ready snapshot');
  Inc(FJourney.FReadyCount);

  if AEvent.Image.Source.Format = nimJPEG then
  begin
    FJourney.FReadyJPEG := True;
  end;
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Image resources: ' + AReason);
  end;
  Inc(Checks);
end;

function Workshop(const ABaseURL: TNyxText): TNyxDocument;
var
  LOriginal: TNyxDocument;
  LCatalog: TNyxCatalog;
  LPatch: INyxResourcePatch;
begin
  LOriginal := BuildNyxDocument;
  LCatalog := TNyxCatalog.Create;
  try
    { Enrich the unchanged English MCP seed through one public semantic group.
      Transport addresses are runtime inputs, never committed machine settings. }
    LPatch := NyxResourcePatch([
      NyxDefineResource(NyxResourceRef('cover'), NyxDefaultLocale,
        NyxHostedResource(nrkImage, NyxResourceURL(ABaseURL + TNyxText('cover.jpg')))
          .Fallback(NyxImageResource(NyxEmbeddedImage(nimPNG, ImagePNG)))
          .Cache(NyxResourceCache.Persistent.FreshFor(600).ServerPolicy(rcspOverride))
          .Describe('Project cover', 'One image shared by the page and reusable cards.')),
      NyxDefineResource(NyxResourceRef('cover'), NyxLocale('en-GB'),
        NyxImageResource(NyxEmbeddedImage(nimPNG, ImagePNG))),
      NyxDefineResource(NyxResourceRef('respect'), NyxDefaultLocale,
        NyxHostedResource(nrkImage, NyxResourceURL(ABaseURL + TNyxText('cover.jpg?respect=1')))
          .Fallback(NyxImageResource(NyxEmbeddedImage(nimPNG, ImagePNG)))
          .Cache(NyxResourceCache.Memory.FreshFor(600))),
      NyxDefineResource(NyxResourceRef('bypass'), NyxDefaultLocale,
        NyxHostedResource(nrkImage, NyxResourceURL(ABaseURL + TNyxText('cover.jpg?bypass=1')))
          .Fallback(NyxImageResource(NyxEmbeddedImage(nimPNG, ImagePNG)))
          .Cache(NyxResourceCache.Bypass.ServerPolicy(rcspOverride))),
      NyxDefineResource(NyxResourceRef('bad-image'), NyxDefaultLocale,
        NyxHostedResource(nrkImage, NyxResourceURL(ABaseURL + TNyxText('corrupt.png')))
          .Fallback(NyxImageResource(NyxEmbeddedImage(nimPNG, ImagePNG)))
          .Cache(NyxResourceCache.Bypass)),
      NyxBindResourceImage(NyxControl('hero-image'),
        NyxResourceImage(NyxResourceRef('cover')).Localize(NyxDefaultLocale, NyxDefaultLocale)),
      NyxBindResourceImage(NyxControl('feature-card-part-1'),
        NyxResourceImage(NyxResourceRef('cover')))]);
    Result := LPatch.Candidate(LOriginal, LCatalog);
    Result.Find('home').Add(NewNyxComponent('second-feature')
      .Configure.Component(NyxComponent('feature-card')).Done.Node);
  finally
    LOriginal.Free;
    LCatalog.Free;
  end;
end;

procedure Shared(AJourney: TJourney);
var
  LCatalog: TNyxCatalog;
  LSession: TNyxStudioSession;
  LWorkspace: TNyxSourceWorkspace;
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LCandidate: TNyxDocument;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LForm: INyxCard;
  LFormDocument: TNyxDocument;
  LChange: TNyxResourceEditorChange;
  LBefore: TNyxText;
  LSource: TNyxText;
  LSelector: TNyxResourceImageRef;
  LRejected: Boolean;
  LAgent: TNyxAgentSession;
  LReply: TNyxDataValue;
  LIndex: Integer;
  LFound: Boolean;
  LFields: array of TNyxDataField;
  LData: TNyxDataValue;
  LMask: TNyxDocument;
  {$ifndef PAS2JS}
  LFile: TFileStream;
  LBytes: TNyxBytes;
  {$endif}
begin
  LDocument := AJourney.FDocument;
  LCatalog := TNyxCatalog.Create;
  LBefore := TNyxCodec.Encode(LDocument);
  LDecoded := TNyxCodec.Decode(LBefore);
  LWorkspace := nil;
  LCandidate := nil;
  LContext := nil;
  LFormDocument := nil;
  LSession := nil;
  LAgent := nil;
  try
    AJourney.Check(TNyxCodec.Encode(LDecoded) = LBefore, 'exact image selector persistence');
    LData := TNyxDataValue.ParseJSON(LBefore);
    AJourney.Check(LData.Field('version').AsInteger = 10,
      'image resource bindings select explicit design version ten');
    SetLength(LFields, LData.Count);
    for LIndex := 0 to LData.Count - 1 do
    begin
      LFields[LIndex] := NyxField(LData.Key(LIndex), LData.Field(LData.Key(LIndex)));

      if LData.Key(LIndex) = 'version' then
      begin
        LFields[LIndex] := NyxField('version', NyxData(9));
      end;
    end;
    LRejected := False;
    try
      LCandidate := TNyxCodec.Decode(NyxObject(LFields).ToJSON);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    AJourney.Check(LRejected, 'earlier wire versions refuse the new image binding family');
    LRejected := False;
    try
      TNyxResourceImageRef.FromData(NyxObject([
        NyxField('resource', NyxData('cover')), NyxField('locale', NyxData('en-GB')),
        NyxField('fallback', NyxData('')), NyxField('localized', NyxData(False))]));
    except
      on ENyxState do
      begin
        LRejected := True;
      end;
    end;
    AJourney.Check(LRejected, 'inherited locale intent cannot hide fixed locale names');
    LMask := TNyxDocument.Create;
    try
      LMask.AddPage(NewNyxImage('empty-image').Binds.Clear(bpImage).Done);
      LCandidate := TNyxCodec.Decode(TNyxCodec.Encode(LMask));
      AJourney.Check((LCandidate.Resources.Count = 0) and
        LCandidate.Pages[0].Bindings[0].Cleared,
        'an image unbinding mask retains version ten without inventing resources');
      FreeAndNil(LCandidate);
    finally
      LMask.Free;
    end;
    LSelector := LDecoded.Find('hero-image').Bindings[0].ResourceImage;
    AJourney.Check(LSelector.Localized and not LSelector.Locale.Defined,
      'pinning the default locale survives persistence separately from inheritance');
    AJourney.Check(not LDecoded.Find('feature-card-part-1').Bindings[0].ResourceImage.Localized,
      'reusable media follows the application locale');
    AJourney.Check(LSelector.Read(LDocument.Resources, NyxLocale('en-GB'), NyxDefaultLocale)
      .ToWire = NyxEmbeddedImage(nimPNG, ImagePNG).ToWire, 'authored hosted fallback reads typed bytes');
    LSource := TNyxCodegen.Generate(LDocument);
    AJourney.Check(Pos('.Image(NyxResourceImage(', LSource) > 0,
      'crafted generated source uses the specialized image selector');
    AJourney.Check(Pos('.Localize(NyxDefaultLocale, NyxDefaultLocale)', LSource) > 0,
      'generated source retains explicitly pinned default locale');
    LCandidate := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    AJourney.Check(TNyxCodec.Encode(LCandidate) = LBefore, 'managed source replays image selectors');
    FreeAndNil(LCandidate);
    FreeAndNil(LWorkspace);

    LRejected := False;
    try
      TNyxBindingSpec.Bound(bpImage, 'caption', nskText, bdFromState);
    except
      on ENyxState do
      begin
        LRejected := True;
      end;
    end;
    AJourney.Check(LRejected, 'image binding refuses a scalar text state');

    LRejected := False;
    try
      LCandidate := NyxResourcePatch([NyxBindResourceImage(NyxControl('image-help'),
        NyxResourceImage(NyxResourceRef('cover')))]).Candidate(LDocument, LCatalog);
    except
      on ENyxState do
      begin
        LRejected := True;
      end;
    end;
    AJourney.Check(LRejected and (TNyxCodec.Encode(LDocument) = LBefore),
      'an image selector cannot bind to a label and preserves the accepted document');

    LRejected := False;
    try
      LCandidate := NyxResourcePatch([NyxDefineResource(NyxResourceRef('cover'),
        NyxDefaultLocale, NyxTextResource('wrong kind'))]).Candidate(LDocument, LCatalog);
    except
      on ENyxState do
      begin
        LRejected := True;
      end;
    end;
    AJourney.Check(LRejected and (TNyxCodec.Encode(LDocument) = LBefore),
      'wrong file kind refuses atomically for retained image consumers');
    LCandidate := NyxResourcePatch([
      NyxDefineResource(NyxResourceRef('cover'), NyxDefaultLocale, NyxTextResource('copy')),
      NyxClearResourceBinding(NyxControl('hero-image'), bpImage),
      NyxClearResourceBinding(NyxControl('feature-card-part-1'), bpImage)])
      .Candidate(LDocument, LCatalog);
    AJourney.Check(LCandidate.Resources.Definition(NyxResourceRef('cover'), NyxDefaultLocale)
      .Kind = nrkText, 'one grouped operation can repair all image consumers before admission');
    FreeAndNil(LCandidate);

    LSession := TNyxStudioSession.Create(NyxProjectPair(LBefore, LSource));
    LContext := RealizeNyxContext(LSession.Document, LSession.Document.Find('hero-image'), LProjection);
    LForm := NewNyxResourceEditor('resource-image-editor', LSession.Document.Resources,
      NyxResourceSelection(NyxResourceRef('cover'), NyxDefaultLocale),
      LSession.Document.Find('hero-image'), LProjection);
    LFormDocument := TNyxDocument.Create;
    LFormDocument.AddPage(LForm);
    AJourney.Check(Pos('Image', LForm.Node.Find(
      NyxResourceEditorFieldID('resource-image-editor', refTarget)).Prop('items')) > 0,
      'common Resources form offers the typed image target');
    LForm.Node.Find(NyxResourceEditorFieldID('resource-image-editor', refBind))
      .Configure.Value(True).Done;
    LForm.Node.Find(NyxResourceEditorFieldID('resource-image-editor', refTarget))
      .Configure.Value('Image').Done;
    LForm.Node.Find(NyxResourceEditorFieldID('resource-image-editor', refTitle))
      .Configure.Value('Shared project cover').Done;
    RefreshNyxResourceEditor(LForm.Node, True);
    AJourney.Check(LForm.Node.Find(
      NyxResourceEditorFieldID('resource-image-editor', refPath)).Prop('visible') = 'false',
      'images do not expose scalar JSON paths');
    AJourney.Check(CaptureNyxResourceEditor(LForm.Node.Find(
      NyxResourceEditorActionID('resource-image-editor', reaApply)), LForm.Node, LChange),
      'common form captures one copied resource/image operation');
    LChange := TNyxResourceEditorChange.FromData(LChange.ToData);
    AJourney.Check(LChange.Binding.Source = bsResourceImage, 'copied form intent retains image typing');
    LSession.ApplyPatch(NewNyxStudioResourcePatch(LChange));
    LSession.Undo;
    AJourney.Check(TNyxCodec.Encode(LSession.Document) = LBefore,
      'one Undo restores the exact resource/image design pair');
    LSession.Redo;
    AJourney.Check(LSession.Document.Find('hero-image').Bindings[0].Source = bsResourceImage,
      'Redo restores the typed image binding');
    LSource := LSession.Source;
    LCandidate := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    AJourney.Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LSession.Document),
      'paired source admission retains image selectors after form Apply/history');
    FreeAndNil(LCandidate);
    FreeAndNil(LWorkspace);

    LAgent := TNyxAgentSession.Create(NyxProjectPair(LBefore, TNyxCodegen.Generate(LDocument)));
    LReply := LAgent.Call('nyx_resources', 'Image resource harness', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('hero-image'))]));
    LFound := False;
    for LIndex := 0 to LReply.Field('bindings').Count - 1 do
    begin

      if LReply.Field('bindings').Item(LIndex).Field('target').AsText = NyxBindingPropertyName(bpImage) then
      begin
        LFound := LReply.Field('bindings').Item(LIndex).Field('resourceImage').AsBoolean;
      end;
    end;
    AJourney.Check(LFound, 'bounded semantic discovery identifies the image resource capability');
    LReply := LAgent.Call('nyx_resources', 'Image resource harness', NyxObject([
      NyxField('mode', NyxData('apply')), NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('resource-image-bind')),
      NyxField('changes', NyxResourcePatch([NyxBindResourceImage(NyxControl('hero-image'),
        NyxResourceImage(NyxResourceRef('cover')))]).ToData)]));
    AJourney.Check(LReply.Field('revision').AsInteger = LAgent.Revision,
      'semantic bind-image admits one revision-aware operation');

    {$ifndef PAS2JS}
    if ParamCount >= 2 then
    begin
      LSource := StringReplace(LSession.Source,
        'unit nyx.generated.view;', 'unit nyx.generated.resource.images;', [rfReplaceAll]);
      LBytes := NyxEncodeUTF8(LSource);
      LFile := TFileStream.Create(ParamStr(2), fmCreate);
      try

        if Length(LBytes) > 0 then
        begin
          LFile.WriteBuffer(LBytes[0], Length(LBytes));
        end;
      finally
        LFile.Free;
      end;
    end;
    {$endif}
  finally
    LAgent.Free;
    LSession.Free;
    LForm := nil;
    LFormDocument.Free;
    LContext.Free;
    LCandidate.Free;
    LWorkspace.Free;
    LDecoded.Free;
    LCatalog.Free;
  end;
end;

constructor TJourney.Create(const ABaseURL: TNyxText);
begin
  inherited Create;
  FDocument := Workshop(ABaseURL);
  Shared(Self);
  FBefore := TNyxCodec.Encode(FDocument);
  FApplication := TTestApplication.Create;
  FOther := TTestApplication.Create;
  FApplication.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand));
  FOther.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand));
  Mount(FApplication);
  Mount(FOther);
  FResources := FApplication.Resources;
  FSubscription := FApplication.View.Events.OnImageReady(NyxControlEvents('hero-image', niRuntime))
    .Subscribe(TReadyCallback.Create(Self));
  FDeadline := Clock + 15000;
end;

destructor TJourney.Destroy;
begin

  if FSubscription <> nil then
  begin
    FSubscription.Cancel;
  end;
  FSubscription := nil;
  FResources := nil;
  FApplication.Free;
  FOther.Free;
  FDocument.Free;
  inherited Destroy;
end;

procedure TJourney.Mount(AApplication: TTestApplication);
{$ifdef PAS2JS}
var
  LHost: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  AApplication.Run(FDocument, LHost);
  {$else}
  AApplication.Mount(FDocument);
  AApplication.Window.Width := 660;
  AApplication.Window.Height := 920;
  AApplication.Window.Show;
  {$endif}
end;

function TJourney.FacesReady(AApplication: TTestApplication;
  ALocalized, AInitial: Boolean): Boolean;
var
  LCount: Integer;
  LReady: Boolean;

  procedure Visit(ANode: TNyxNode);
  var
    LIndex: Integer;
    LExpected: TNyxImageFormat;
    LSource: TNyxImageSource;
    {$ifdef PAS2JS}LImage: TJSHTMLImageElement;{$else}LImage: TImage;{$endif}
  begin

    if ANode.ProjectionKind = 'image' then
    begin
      Inc(LCount);
      LExpected := nimJPEG;

      if AInitial or (ALocalized and (ANode.DesignID <> 'hero-image')) then
      begin
        LExpected := nimPNG;
      end;
      LSource := TNyxImageSource.FromWire(ANode.Prop(NyxBindingPropertyName(bpImage)));

      if LSource.Kind <> nisEmbedded then
      begin
        raise Exception.Create('Bound image source is not embedded: ' + ANode.ID +
          TNyxText(' / ') + ANode.Prop(NyxBindingPropertyName(bpImage)));
      end;
      LReady := LReady and (LSource.Format = LExpected);
      {$ifdef PAS2JS}
      LImage := TJSHTMLImageElement(AApplication.View.ElementFor(ANode.ID, niRuntime));
      LReady := LReady and LImage.complete and (LImage.naturalWidth = 100) and
        (LImage.naturalHeight = 50) and
        (LImage.currentSrc = ANode.Prop(NyxBindingPropertyName(bpImage)));
      {$else}
      LImage := TImage(AApplication.View.ControlFor(ANode.ID, niRuntime));
      LReady := LReady and (LImage.Picture.Width = 100) and (LImage.Picture.Height = 50);
      {$endif}
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LIndex]);
    end;
  end;

begin
  LReady := True;
  LCount := 0;
  Visit(AApplication.View.Root);
  Result := LReady and (LCount = 3);
end;

procedure TJourney.CheckFaces(AApplication: TTestApplication;
  ALocalized, AInitial: Boolean);
begin
    Check(FacesReady(AApplication, ALocalized, AInitial),
    'three real image faces retain independent resource/locale projections and decoded dimensions');
end;

function TJourney.Loaded(const AName: TNyxText): Boolean;
begin
  Result := FResources.Status(NyxResourceRef(AName), NyxDefaultLocale).Phase = nrpReady;
end;

procedure TJourney.Next;
begin
  Inc(FStage);
  FDeadline := Clock + 15000;
end;

procedure TJourney.Screenshot;
{$ifndef PAS2JS}
var
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
{$endif}
begin
  {$ifdef PAS2JS}
  document.body.setAttribute('data-capture-checkpoint', 'resource-images');
  {$else}
  if ParamCount >= 3 then
  begin
    LBitmap := TBitmap.Create;
    LPNG := TPortableNetworkGraphic.Create;
    try
      LBitmap.SetSize(FApplication.Window.ClientWidth, FApplication.Window.ClientHeight);
      FApplication.Window.PaintTo(LBitmap.Canvas, 0, 0);
      LPNG.Assign(LBitmap);
      LPNG.SaveToFile(ParamStr(3));
    finally
      LPNG.Free;
      LBitmap.Free;
    end;
  end;
  {$endif}
end;

procedure TJourney.Step;
var
  LStatus: TNyxApplicationResourceStatus;
begin

  if Clock > FDeadline then
  begin
    raise Exception.Create('Image resources timed out at stage ' + IntToStr(FStage));
  end;
  case FStage of
    0:
      begin

        if not FacesReady(FApplication, False, True) or not FacesReady(FOther, False, True) then
        begin
          Exit;
        end;
        CheckFaces(FApplication, False, True);
        CheckFaces(FOther, False, True);
        FResources.Reload(NyxResourceRef('cover'), NyxDefaultLocale);
        Next;
      end;
    1:
      begin

        if not Loaded('cover') or not FacesReady(FApplication, False) or not FReadyJPEG then
        begin
          Exit;
        end;
        LStatus := FResources.Status(NyxResourceRef('cover'), NyxDefaultLocale);
        Check((LStatus.Origin = rloNetwork) and (LStatus.CacheWrite = rcuPersistent),
          'caller override stores actual no-store HTTP image in the private persistent cache' +
          ' (origin=' + IntToStr(Ord(LStatus.Origin)) +
          ', cache=' + IntToStr(Ord(LStatus.CacheWrite)) +
          ', warning=' + LStatus.CacheWarning + ')');
        CheckFaces(FApplication, False);
        CheckFaces(FOther, False, True);
        Check(TNyxCodec.Encode(FDocument) = FBefore, 'hosted publication never changes authored defaults');
        FReadyBefore := FReadyCount;
        FResources.Reload(NyxResourceRef('cover'), NyxDefaultLocale);
        Next;
      end;
    2:
      begin

        if not Loaded('cover') then
        begin
          Exit;
        end;
        LStatus := FResources.Status(NyxResourceRef('cover'), NyxDefaultLocale);
        Check((LStatus.Origin = rloFreshCache) and (LStatus.CacheRead = rcuPersistent),
          'actual target cache supplies the overridden image on reload');
        Check(FReadyCount = FReadyBefore, 'unchanged cached bytes do not restart the image request');
        FResources.Reload(NyxResourceRef('respect'), NyxDefaultLocale);
        Next;
      end;
    3, 4:
      begin

        if not Loaded('respect') then
        begin
          Exit;
        end;
        LStatus := FResources.Status(NyxResourceRef('respect'), NyxDefaultLocale);
        Check((LStatus.Origin = rloNetwork) and (LStatus.CacheWrite = rcuNone) and
          (LStatus.CacheRead = rcuNone), 'respect policy does not store/reuse server no-store image');

        if FStage = 3 then
        begin
          FResources.Reload(NyxResourceRef('respect'), NyxDefaultLocale);
        end
        else
        begin
          FResources.Reload(NyxResourceRef('bypass'), NyxDefaultLocale);
        end;
        Next;
      end;
    5:
      begin

        if not Loaded('bypass') then
        begin
          Exit;
        end;
        LStatus := FResources.Status(NyxResourceRef('bypass'), NyxDefaultLocale);
        Check((LStatus.Origin = rloNetwork) and (LStatus.CacheWrite = rcuNone),
          'bypass wins even with explicit server override');
        FResources.Reload(NyxResourceRef('bad-image'), NyxDefaultLocale);
        Next;
      end;
    6:
      begin

        if not Loaded('bad-image') then
        begin
          Exit;
        end;
        LStatus := FResources.Status(NyxResourceRef('bad-image'), NyxDefaultLocale);
        Check((LStatus.Origin = rloFallback) and (LStatus.Error <> ''),
          'corrupt hosted raster refuses admission and reports its explicit typed fallback');
        CheckFaces(FApplication, False);
        FResources.Localize(NyxLocale('en-GB'), NyxDefaultLocale);
        Next;
      end;
    7:
      begin

        if not FacesReady(FApplication, True) then
        begin
          Exit;
        end;
        CheckFaces(FApplication, True);
        CheckFaces(FOther, False, True);
        Check(FResources.Context.Snapshot.Definition(NyxResourceRef('cover'), NyxDefaultLocale)
          .Image.Format = nimJPEG, 'localization retains the independently loaded default variant');
        FResources.Localize(NyxLocale('en-US'), NyxLocale('en-GB'));
        Next;
      end;
    8:
      begin

        if not FacesReady(FApplication, True) then
        begin
          Exit;
        end;
        Check((FResources.Context.Locale.Name = 'en-US') and
          (FResources.Context.Fallback.Name = 'en-GB'),
          'missing selected image locale uses the explicit fallback variant');
        CheckFaces(FApplication, True);
        CheckFaces(FOther, False, True);
        FResources.Localize(NyxLocale('en-US'), NyxLocale('en-CA'));
        Next;
      end;
    9:
      begin

        if not FacesReady(FApplication, False) then
        begin
          Exit;
        end;
        Check((FResources.Context.Locale.Name = 'en-US') and
          (FResources.Context.Fallback.Name = 'en-CA'),
          'missing selected/fallback image locales use the independently loaded default');
        CheckFaces(FApplication, False);
        CheckFaces(FOther, False, True);
        FResources.Localize(NyxLocale('en-GB'), NyxDefaultLocale);
        Next;
      end;
    10:
      begin

        if not FacesReady(FApplication, True) then
        begin
          Exit;
        end;
        CheckFaces(FApplication, True);
        Screenshot;
        Next;
      end;
    11:
      begin
        {$ifdef PAS2JS}
        if (Pos('capture=1', window.location.search) > 0) and
          (document.body.getAttribute('data-capture-observed') <> 'resource-images') then
        begin
          Exit;
        end;
        {$endif}
        FResources.Reload(NyxResourceRef('respect'), NyxDefaultLocale);
        FResources.Stop;
        FRetiredCount := FReadyCount;
        FRetiredAt := Clock;
        FreeAndNil(FApplication);
        FreeAndNil(FOther);
        Next;
      end;
    12:
      begin

        if Clock - FRetiredAt < 200 then
        begin
          Exit;
        end;
        Check(FReadyCount = FRetiredCount, 'retired resource/image views deliver no late callbacks');
        Check(NyxApplicationResourceDiagnostics(FResources).CaptureRuntime.Stopped,
          'retained diagnostics safely observe a stopped application owner');
        FFinished := True;
      end;
  end;
end;

{$ifdef PAS2JS}
procedure Poll;
begin
  try
    GJourney.Step;

    if GJourney.Finished then
    begin
      document.body.setAttribute('data-resource-image-checks', IntToStr(GJourney.Checks));
      GJourney.Free;
      GJourney := nil;
      document.body.setAttribute('data-test-result', 'passed');
    end
    else
    begin
      window.setTimeout(@Poll, 10);
    end;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-resource-image-error', E.Message);
      GJourney.Free;
      GJourney := nil;
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end;
{$endif}

{$ifndef PAS2JS}
{ Pascal writes fixture bytes into an explicitly owned existing static-host
  child. The shell only confines/copies paths; it never implements image codecs. }
procedure WriteHostedFiles;
var
  LBytes: TNyxBytes;
  procedure Save(const AName: String; const ABytes: TNyxBytes);
  var
    LFile: TFileStream;
  begin
    LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(4)) + AName, fmCreate);
    try
      LFile.WriteBuffer(ABytes[0], Length(ABytes));
    finally
      LFile.Free;
    end;
  end;
begin

  if (ParamCount <> 4) or not DirectoryExists(ParamStr(4)) then
  begin
    raise Exception.Create('Supply HTTP base, emitted source, capture and owned static child');
  end;
  Save('cover.jpg', NyxEmbeddedImage(nimJPEG, ImageJPEG).Bytes);
  LBytes := NyxEmbeddedImage(nimPNG, ImagePNG).Bytes;
  Save('cover.png', LBytes);
  LBytes[64] := LBytes[64] xor 1;
  Save('corrupt.png', LBytes);
end;
{$endif}

begin
  {$ifdef PAS2JS}
  GJourney := TJourney.Create(window.location.origin +
    Copy(window.location.pathname, 1, LastDelimiter('/', window.location.pathname)));
  Poll;
  {$else}
  try
    Application.Initialize;
    WriteHostedFiles;
    GJourney := TJourney.Create(TNyxText(ParamStr(1)));
    try
      while not GJourney.Finished do
      begin
        Application.ProcessMessages;
        CheckSynchronize(0);
        GJourney.Step;
        Sleep(5);
      end;
      WriteLn('PASS / image resource controls / ', GJourney.Checks, ' checks');
    finally
      GJourney.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('FAIL / image resources / ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
