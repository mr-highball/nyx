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

program nyx_resource_persistence_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.resources, nyx.resource.sources, nyx.resource.cache, nyx.resources.loader,
  nyx.application.resources, nyx.resource.context, nyx.scheduler
  {$ifdef PAS2JS}
  , JS, Web, nyx.application.browser, nyx.resource.cache.browser,
    nyx.resources.http.browser
  {$else}
  , Interfaces, Forms, StdCtrls, Graphics, IntfGraphics, FPWritePNG,
    nyx.application.lcl, nyx.resource.cache.lcl, nyx.resources.http.lcl
  {$endif};

type
  { The boundary names are closed qualification phases. Each invocation creates
    a new application/resolver. A separate Pascal driver owns process retirement
    and the fresh private cache/profile reused by Store and Restore. }
  TPersistencePhase = (ppStore, ppRestore, ppQuota, ppCorrupt, ppRespect);
  TPersistenceApplication = {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif};

const
  CPhaseNames: array[TPersistencePhase] of TNyxText = ('store', 'restore', 'quota', 'corrupt', 'respect');
  CLoaded: TNyxText = 'Keep creating 🌙';
  CPrompt: TNyxText = 'Project name 🌙';
  CEnglish: TNyxText = 'Your English workspace';

type
  { Borrows no document from another project. The document outlives its host;
    subscriptions retire before their method receiver and retained runtime
    snapshots safely outlive the host. Ordinary target controls do all painting. }
  TJourney = class
  private
    FPhase: TPersistencePhase;
    FDocument: TNyxDocument;
    FApplication: TPersistenceApplication;
    FResources: INyxApplicationResources;
    FSubscription: INyxResourceSubscription;
    FScheduler: INyxScheduler;
    FBefore: TNyxText;
    FBusy: Boolean;
    FStage: Integer;
    FStarted: Double;
    FChecks: Integer;
    FFinished: Boolean;
    {$ifdef PAS2JS}
    FTimer: NativeInt;
    {$endif}
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    function Caption(const AID: TNyxText): TNyxText;
    function Validate(const AContext: INyxResourceContext): Boolean;
    procedure Finish;
    procedure Capture;
    {$ifdef PAS2JS}
    procedure CorruptAndLoad(const AURL: TNyxText); async;
    {$endif}
  public
    destructor Destroy; override;
    procedure Start;
    procedure Next;
    property Finished: Boolean read FFinished;
  end;

function Milliseconds: Double;
begin
  {$ifdef PAS2JS}
  Result := window.performance.now;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxResource.Create('Application persistence: ' + AReason);
  end;
  Inc(FChecks);
end;

function TJourney.Caption(const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := FApplication.View.ElementFor(AID).textContent;
  {$else}
  Result := TNyxText(RawByteString(TLabel(FApplication.View.ControlFor(AID)).Caption));
  {$endif}
end;

function TJourney.Validate(const AContext: INyxResourceContext): Boolean;
begin
  Result := not FBusy;
end;

{$ifndef PAS2JS}
function ReadOwnedText(const APath: String; AMaximum: Integer): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try

    if (LStream.Size < 1) or (LStream.Size > AMaximum) then
    begin
      raise ENyxResource.Create('Private qualification file exceeds its exact byte budget');
    end;
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

procedure WriteOwnedText(const APath: String; const AText: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LStream := TFileStream.Create(APath, fmCreate);
  try
    LStream.WriteBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
end;

{ Refuse existing user cache directories before any store/corruption action.
  Later phases admit only this fixture's exact origin-bound marker. Corruption
  targets one previously parsed entry belonging to the same owned URL/kind. }
procedure AdmitNativeHome(APhase: TPersistencePhase; const AURL: TNyxText);
const
  CMarker = '.persistence-qualification.json';
var
  LHome: String;
  LMarker: TNyxDataValue;
  LSearch: TSearchRec;
  LEntry: TNyxResourceCacheEntry;
  LPath: String;
  LTarget: String;
  LCount: Integer;
begin
  LHome := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));

  if APhase = ppStore then
  begin

    if DirectoryExists(LHome) then
    begin
      raise ENyxResource.Create('Store requires a fresh private qualification cache home');
    end;
    ForceDirectories(LHome);
    WriteOwnedText(LHome + CMarker, NyxObject([NyxField('version', NyxData(1)),
      NyxField('url', NyxData(AURL))]).ToJSON);
  end
  else
  begin
    LMarker := TNyxDataValue.ParseJSON(ReadOwnedText(LHome + CMarker, 4096));

    if (LMarker.Count <> 2) or (LMarker.Field('version').AsInteger <> 1) or
      (LMarker.Field('url').AsText <> AURL) then
    begin
      raise ENyxResource.Create('Cache home belongs to a different qualification');
    end;
  end;

  if APhase <> ppCorrupt then
  begin
    Exit;
  end;
  LCount := 0;
  LTarget := '';

  if FindFirst(LHome + '*.nyx-resource', faAnyFile, LSearch) = 0 then
  begin
    try
      repeat
        LPath := LHome + LSearch.Name;
        LEntry := TNyxResourceCacheEntry.FromData(TNyxDataValue.ParseJSON(
          ReadOwnedText(LPath, 2 * NyxMaximumPackedBytes + 16384)));

        if (LEntry.URL.Address = AURL) and (LEntry.Kind = nrkJSON) then
        begin
          Inc(LCount);
          LTarget := LPath;
        end;
      until FindNext(LSearch) <> 0;
    finally
      SysUtils.FindClose(LSearch);
    end;
  end;

  if LCount <> 1 then
  begin
    raise ENyxResource.Create('Corruption requires one exact previously stored qualification entry');
  end;
  WriteOwnedText(LTarget, 'damaged');
end;
{$endif}

{$ifdef PAS2JS}
procedure TJourney.CorruptAndLoad(const AURL: TNyxText); async;
var
  LCache: TJSCache;
  LKey: String;
  LResponse: TJSResponse;
  LEntry: TNyxResourceCacheEntry;
begin
  try
    { This fresh driver's private origin/profile owns the cache. Target the
      existing version-one key only after its envelope proves URL/kind identity.
      Real Cache Storage corruption is exercised without replacing host APIs. }
    LCache := TJSCache(await(TJSCacheStorage(TJSObject(window)['caches']).open('nyx-resource-cache-v1')));
    LKey := window.location.origin + '/.nyx/resource-cache?kind=' + IntToStr(Ord(nrkJSON)) +
      '&url=' + encodeURIComponent(AURL);
    LResponse := TJSResponse(await(LCache.match(LKey)));
    Check((LResponse <> nil) and not isUndefined(LResponse), 'owned cached envelope exists before corruption');
    LEntry := TNyxResourceCacheEntry.FromData(TNyxDataValue.ParseJSON(await(LResponse.text())));
    Check((LEntry.URL.Address = AURL) and (LEntry.Kind = nrkJSON), 'corruption targets exact owned bytes');
    LResponse := TJSResponse.new('damaged');
    LResponse.headers.append('x-nyx-cache-bytes', '7');
    await(LCache.put(LKey, LResponse));
    FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
    FTimer := window.setInterval(@Next, 20);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end;
{$endif}

procedure TJourney.Start;
var
  LName: TNyxText;
  LURL: TNyxText;
  LPhase: TPersistencePhase;
  LFound: Boolean;
  LPage: INyxColumn;
  LCopy: TNyxResourceRef;
  LCache: INyxResourceCacheStorage;
  LTransport: INyxResourceTransport;
  LResolver: INyxResourceResolver;
  LLabels: TNyxResourceLabels;
  LPolicy: TNyxResourceCachePolicy;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  {$endif}
begin
  FStarted := Milliseconds;
  {$ifdef PAS2JS}
  LName := Copy(window.location.search, Length('?phase=') + 1, MaxInt);
  LURL := window.location.origin + Copy(window.location.pathname, 1,
    LastDelimiter('/', window.location.pathname)) + 'copy.json';
  {$else}

  if ParamCount <> 4 then
  begin
    raise ENyxResource.Create('Supply phase, owned HTTP file URL, private cache home and PNG path');
  end;
  LName := TNyxText(ParamStr(1));
  LURL := TNyxText(ParamStr(2));
  {$endif}
  LFound := False;
  for LPhase := Low(TPersistencePhase) to High(TPersistencePhase) do
  begin

    if LName = CPhaseNames[LPhase] then
    begin
      FPhase := LPhase;
      LFound := True;
    end;
  end;
  Check(LFound, 'phase is explicit before loading');
  {$ifndef PAS2JS}
  AdmitNativeHome(FPhase, LURL);
  {$endif}

  if FPhase = ppQuota then
  begin
    LURL := LURL + TNyxText('?quota=1');
  end;
  LLabels := NyxResourceLabels.Add(NyxResourceLabel('Current captions'));

  if FPhase = ppStore then
  begin
    LLabels := NyxResourceLabels.Add(NyxResourceLabel('Stored captions'));
  end;
  FDocument := TNyxDocument.Create;
  FDocument.Title := 'Persistent resource workshop';
  LCopy := NyxResourceRef('copy');
  LPolicy := NyxResourceCache.Persistent.FreshFor(3600).ServerPolicy(rcspOverride);

  if FPhase = ppRespect then
  begin
    { A previous caller deliberately stored this server's no-store response.
      Current caller policy must govern reuse as well as the next write. }
    LPolicy := LPolicy.ServerPolicy(rcspRespect);
  end;
  FDocument.Resources.Define(LCopy,
    NyxResourceDiscovery(NyxHostedResource(nrkJSON, NyxResourceURL(LURL))
      .Cache(LPolicy)
      .Fallback(NyxJSONResource('{"headline":"Ready while loading","prompt":"Local project name"}')))
      .WithLabels(LLabels));
  FDocument.Resources.Define(LCopy, NyxLocale('en-GB'),
    NyxJSONResource('{"headline":"Your English workspace","prompt":"Programme name"}'));
  LPage := NewNyxColumn('home');
  LPage.Configure.Padding(24).Gap(16).Done;
  FDocument.AddPage(LPage);
  LPage.Add(NewNyxHeading('workshop-title').WithText('Keep your workspace close'));
  LPage.Add(NewNyxLabel('caption').Binds.Text(NyxResourceValue(LCopy).Field('headline')).Done);
  LPage.Add(NewNyxLabel('pinned-caption').Binds.Text(NyxResourceValue(LCopy)
    .Field('headline').Localize(NyxDefaultLocale, NyxDefaultLocale)).Done);
  LPage.Add(NewNyxInput('project-name').Binds.Placeholder(
    NyxResourceValue(LCopy).Field('prompt')).Done);
  LPage := NewNyxColumn('details');
  FDocument.AddPage(LPage);
  LPage.Add(NewNyxLabel('details-caption').Binds.Text(
    NyxResourceValue(LCopy).Field('headline')).Done);
  FBefore := TNyxCodec.Encode(FDocument);
  FApplication := TPersistenceApplication.Create;
  {$ifdef PAS2JS}
  LTransport := NewNyxBrowserResourceTransport;
  LCache := NewNyxBrowserResourceCache;

  if FPhase = ppQuota then
  begin
    LCache := NewNyxBrowserResourceCache(1, 1);
  end;
  {$else}
  FScheduler := NewNyxScheduler;
  LTransport := NewNyxNativeResourceTransport(FScheduler);
  LCache := NewNyxFileResourceCache(TNyxText(ParamStr(3)));

  if FPhase = ppQuota then
  begin
    LCache := NewNyxFileResourceCache(TNyxText(ParamStr(3)), 1, 1);
  end;
  {$endif}
  LResolver := NewNyxResourceResolver(LTransport, nil, LCache);
  FApplication.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand), LResolver);
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  FApplication.Run(FDocument, LHost);
  {$else}
  FApplication.Mount(FDocument);
  FApplication.Window.Show;
  {$endif}
  FResources := FApplication.Resources;
  Check(Caption('caption') = 'Ready while loading', 'initial fallback is not a completed fetch');
  {$ifdef PAS2JS}

  if FPhase = ppCorrupt then
  begin
    CorruptAndLoad(LURL);
    Exit;
  end;
  {$endif}
  FResources.Reload(LCopy, NyxDefaultLocale);
  {$ifdef PAS2JS}
  FTimer := window.setInterval(@Next, 20);
  {$endif}
end;

procedure TJourney.Capture;
{$ifndef PAS2JS}
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
{$endif}
begin
  {$ifdef PAS2JS}
  document.body.setAttribute('data-capture-checkpoint', 'persistent-' + CPhaseNames[FPhase]);
  {$else}
  Application.ProcessMessages;
  LBitmap := TBitmap.Create;
  LImage := nil;
  try
    LBitmap.SetSize(FApplication.Window.Width, FApplication.Window.Height);
    FApplication.Window.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(ParamStr(4));
  finally
    LImage.Free;
    LBitmap.Free;
  end;
  {$endif}
end;

procedure TJourney.Next;
var
  LStatus: TNyxApplicationResourceStatus;
  LRuntime: INyxResourceRuntimeSnapshot;
  LRefused: Boolean;
begin
  try
    Check(Milliseconds - FStarted < 30000, 'actual application journey stays bounded');
    LStatus := FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale);
    case FStage of
      0:
        begin

          if LStatus.Phase in [nrpQueued, nrpLoading, nrpWaiting] then
          begin
            Exit;
          end;
          Check(LStatus.Phase = nrpReady, 'persistent load publishes atomically');
          Check((Caption('caption') = CLoaded) and (Caption('pinned-caption') = CLoaded),
            'real controls paint exact hosted Unicode');
          {$ifdef PAS2JS}
          Check(TJSHTMLInputElement(FApplication.View.InputFor('project-name')).placeholder = CPrompt,
            'real browser prompt consumes persistent data');
          {$else}
          Check(TNyxText(RawByteString(TEdit(FApplication.View.InputFor('project-name')).TextHint)) = CPrompt,
            'real native prompt consumes persistent data');
          {$endif}

          if FPhase = ppRestore then
          begin
            Check((LStatus.Origin = rloFreshCache) and (LStatus.CacheRead = rcuPersistent),
              'a fresh process consumes persistent bytes without HTTP');
            Check((NyxResourceLabelsOf(FResources.Context.Snapshot.Definition(
              NyxResourceRef('copy'), NyxDefaultLocale)).Count = 1) and
              NyxResourceLabelsOf(FResources.Context.Snapshot.Definition(
              NyxResourceRef('copy'), NyxDefaultLocale)).Contains(NyxResourceLabel('Current captions')),
              'cached bytes use current creator labels');
          end
          else if FPhase = ppQuota then
          begin
            Check((LStatus.Origin = rloNetwork) and (LStatus.CacheWrite = rcuMemory) and
              (LStatus.CacheWarning <> ''), 'real storage budget failure falls back to reported memory');
          end
          else if FPhase = ppRespect then
          begin
            Check((LStatus.Origin = rloNetwork) and (LStatus.CacheRead = rcuNone) and
              (LStatus.CacheWrite = rcuNone), 'current Respect refuses reuse/storage of prior override bytes');
          end
          else if FPhase = ppCorrupt then
          begin
            Check((LStatus.Origin = rloNetwork) and (LStatus.CacheWrite = rcuPersistent) and
              (LStatus.CacheWarning <> ''), 'damaged storage reports refusal and heals through real HTTP');
          end
          else
          begin
            Check((LStatus.Origin = rloNetwork) and (LStatus.CacheWrite = rcuPersistent),
              'caller override stores actual no-store HTTP bytes privately');
          end;
          Check(TNyxCodec.Encode(FDocument) = FBefore, 'loading never rewrites authored declarations');
          Capture;
          FStage := 1;
        end;
      1:
        begin
          {$ifdef PAS2JS}

          if document.body.getAttribute('data-capture-observed') <>
            'persistent-' + CPhaseNames[FPhase] then
          begin
            Exit;
          end;
          {$endif}
          FResources.Localize(NyxLocale('en-GB'), NyxDefaultLocale);
          Check((Caption('caption') = CEnglish) and (Caption('pinned-caption') = CLoaded),
            'locale inheritance changes independently of explicit default pin');
          FApplication.ShowPage('details');
          Check(Caption('details-caption') = CEnglish, 'a new page uses the accepted runtime locale');
          FApplication.ShowPage('home');
          FResources.Localize(NyxLocale('missing'), NyxLocale('en-GB'));
          Check(Caption('caption') = CEnglish, 'missing locale uses explicit caller fallback');
          FResources.Localize(NyxDefaultLocale, NyxDefaultLocale);
          FBusy := True;
          FSubscription := FResources.Subscribe(Validate, nil);
          FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
          FStage := 2;
        end;
      2:
        begin

          if LStatus.Phase <> nrpWaiting then
          begin
            Check(LStatus.Phase in [nrpQueued, nrpLoading], 'reload waits for actual target admission');
            Exit;
          end;
          Check(Caption('caption') = CLoaded, 'a busy publication preserves installed controls');
          FResources.Cancel;
          FSubscription.Disconnect;
          FSubscription := nil;
          Check(FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale).Phase = nrpCancelled,
            'cancellation retires a pending real cache publication');
          LRuntime := NyxApplicationResourceDiagnostics(FResources).CaptureRuntime;
          Check(LRuntime.Entry(0).HasPublishedLoad, 'cancel keeps prior installed load evidence');
          Check(TNyxCodec.Encode(FDocument) = FBefore, 'locale, navigation and cancel preserve saved defaults');
          FreeAndNil(FApplication);
          Check(NyxApplicationResourceDiagnostics(FResources).CaptureRuntime.Stopped,
            'retained runtime owner reports host retirement');
          LRefused := False;
          try
            FResources.Reload;
          except
            on ENyxResource do
            begin
              LRefused := True;
            end;
          end;
          Check(LRefused, 'a retained stopped owner refuses new loading');
          Finish;
        end;
    end;
  except
    on LException: Exception do
    begin
      FFinished := True;
      {$ifdef PAS2JS}
      window.clearInterval(FTimer);
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
      {$else}
      raise;
      {$endif}
    end;
  end;
end;

procedure TJourney.Finish;
begin
  FFinished := True;
  {$ifdef PAS2JS}
  window.clearInterval(FTimer);
  document.body.setAttribute('data-persistence-checks', IntToStr(FChecks));
  document.body.setAttribute('data-test-result', 'passed');
  {$else}
  WriteLn('PASS / application persistence / ', CPhaseNames[FPhase], ' / ', FChecks, ' checks');
  {$endif}
end;

destructor TJourney.Destroy;
begin

  if FSubscription <> nil then
  begin
    FSubscription.Disconnect;
  end;
  FApplication.Free;
  FResources := nil;
  FDocument.Free;

  if FScheduler <> nil then
  begin
    FScheduler.Shutdown;
  end;
  inherited Destroy;
end;

var
  GJourney: TJourney;
begin
  {$ifndef PAS2JS}
  Application.Initialize;
  {$endif}
  GJourney := TJourney.Create;
  try
    GJourney.Start;
    {$ifndef PAS2JS}
    try
      while not GJourney.Finished do
      begin
        CheckSynchronize(0);
        Application.ProcessMessages;
        GJourney.Next;
        Sleep(1);
      end;
    finally
      FreeAndNil(GJourney);
      CheckSynchronize(0);
      Application.ProcessMessages;
    end;
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
      {$else}
      FreeAndNil(GJourney);
      WriteLn('FAIL / ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
