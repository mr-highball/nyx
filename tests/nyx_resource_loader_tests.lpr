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


program nyx_resource_loader_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.resources,
  nyx.resource.sources, nyx.resource.cache, nyx.resources.loader,
  nyx.scheduler, nyx.model, nyx.controls, nyx.binding.types, nyx.codec,
  {$ifdef PAS2JS}Web, nyx.resources.http.browser, nyx.render.browser
  {$else}Interfaces, Forms, StdCtrls, Graphics, IntfGraphics, FPWritePNG, SyncObjs,
    nyx.resources.http.lcl, nyx.resource.cache.lcl, nyx.render.lcl{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxResource.Create('Resource loading: ' + AReason);
  end;
  Inc(GChecks);
end;

type
  TClock = class(TInterfacedObject, INyxResourceClock)
    Seconds: Double;
    function UTCSeconds: Double;
  end;
  TTestRequest = class(TNyxResourceRequest)
    procedure Send(const AResponse: TNyxResourceHTTPResult);
  end;
  TTestTransport = class(TInterfacedObject, INyxResourceTransport)
    Calls: Integer;
    Deferred: Boolean;
    Response: TNyxResourceHTTPResult;
    Pending: TTestRequest;
    Lease: INyxResourceRequest;
    function Request(const AURL: TNyxResourceURL;
      const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
    procedure Send;
  end;
  TProbe = class
    Calls: Integer;
    ResultValue: TNyxResourceLoadResult;
    procedure Loaded(const AResult: TNyxResourceLoadResult);
  end;
  TFaultCache = class(TInterfacedObject, INyxResourceCacheStorage)
    function Read(const AURL: TNyxResourceURL; AKind: TNyxResourceKind;
      AReply: TNyxResourceCacheRead): INyxResourceCacheJob;
    function Write(const AEntry: TNyxResourceCacheEntry;
      AReply: TNyxResourceCacheWrite): INyxResourceCacheJob;
  end;

function TFaultCache.Read(const AURL: TNyxResourceURL; AKind: TNyxResourceKind;
  AReply: TNyxResourceCacheRead): INyxResourceCacheJob;
begin
  Result := nil;
  raise ENyxResource.Create('Persistent storage unavailable');
end;

function TFaultCache.Write(const AEntry: TNyxResourceCacheEntry;
  AReply: TNyxResourceCacheWrite): INyxResourceCacheJob;
begin
  Result := nil;
  raise ENyxResource.Create('Persistent storage quota full');
end;

function TClock.UTCSeconds: Double;
begin
  Result := Seconds;
end;

procedure TTestRequest.Send(const AResponse: TNyxResourceHTTPResult);
begin
  Complete(AResponse);
end;

function TTestTransport.Request(const AURL: TNyxResourceURL;
  const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
var
  LRequest: TTestRequest;
begin
  Inc(Calls);
  LRequest := TTestRequest.Create(AReply);
  Result := LRequest;

  if Deferred then
  begin
    Pending := LRequest;
    Lease := Result;
  end
  else
  begin
    LRequest.Send(Response);
  end;
end;

procedure TTestTransport.Send;
begin
  Pending.Send(Response);
  Pending := nil;
  Lease := nil;
end;

procedure TProbe.Loaded(const AResult: TNyxResourceLoadResult);
begin
  Inc(Calls);
  ResultValue := AResult;
end;

procedure Shared;
var
  LTransport: TTestTransport;
  LTransportLease: INyxResourceTransport;
  LClock: TClock;
  LClockLease: INyxResourceClock;
  LResolver: INyxResourceResolver;
  LMemory: INyxResourceCacheStorage;
  LLoad: INyxResourceLoad;
  LDefinition: INyxResourceDefinition;
  LProbe: TProbe;
  LBefore: Integer;
  LFault: INyxResourceCacheStorage;
begin
  LTransport := TTestTransport.Create;
  LTransportLease := LTransport;
  LTransport.Response.Status := 200;
  LTransport.Response.Bytes := NyxEncodeUTF8('{"headline":"Loaded workshop","prompt":"Your next project"}');
  LTransport.Response.Hints := NyxResourceCacheHeaders('', '');
  LClock := TClock.Create;
  LClockLease := LClock;
  LClock.Seconds := 1000;
  LMemory := NewNyxMemoryResourceCache;
  LResolver := NewNyxResourceResolver(LTransportLease, LMemory, nil, LClockLease);
  LDefinition := NyxHostedResource(nrkJSON, NyxResourceURL('https://example.com/copy.json'))
    .Cache(NyxResourceCache.Memory.FreshFor(10).StaleFor(20))
    .Describe('Copy', 'Loaded application captions.');
  LProbe := TProbe.Create;
  try
    Check(not Default(TNyxResourceLoadResult).Succeeded, 'default result is not success');
    LLoad := LResolver.Load(NyxTextResource('Embedded'), NyxResourceLoadOptions, LProbe.Loaded);
    Check((LProbe.ResultValue.Origin = rloEmbedded) and
      (LProbe.ResultValue.Definition.Text = 'Embedded') and (LTransport.Calls = 0),
      'embedded values never fetch');
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    Check((LProbe.ResultValue.Origin = rloNetwork) and LProbe.ResultValue.Succeeded,
      'network resolves admitted kind');
    Check(LProbe.ResultValue.Definition.Title = 'Copy', 'caller metadata survives resolution');
    Check((LProbe.ResultValue.CacheWrite = rcuMemory) and
      (LProbe.ResultValue.CacheRead = rcuNone), 'network completion reports successful memory storage');
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    Check((LProbe.ResultValue.Origin = rloFreshCache) and (LTransport.Calls = 1),
      'fresh cache avoids transport');
    Check((LProbe.ResultValue.CacheRead = rcuMemory) and
      (LProbe.ResultValue.CacheWrite = rcuNone), 'fresh memory hit does not claim a new cache write');
    LClock.Seconds := 1010;
    LTransport.Response.Error := 'Network offline';
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    Check((LProbe.ResultValue.Origin = rloStaleCache) and
      (LProbe.ResultValue.Error = 'Network offline'), 'stale is explicit failure fallback');
    Check(LProbe.ResultValue.CacheRead = rcuMemory, 'eligible stale origin retains the actual cache tier');
    LClock.Seconds := 1030;
    LLoad := LResolver.Load(LDefinition.Fallback(NyxJSONResource('{"headline":"Offline"}')),
      NyxResourceLoadOptions, LProbe.Loaded);
    Check((LProbe.ResultValue.Origin = rloFallback) and
      (LProbe.ResultValue.Definition.Data.Field('headline').AsText = 'Offline'),
      'expired cache uses authored fallback');
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    Check(not LProbe.ResultValue.Succeeded and (LProbe.ResultValue.Origin = rloFailed),
      'no stale or fallback returns explicit failure');
    LTransport.Response.Error := '';
    LTransport.Response.Hints := NyxResourceCacheHeaders('no-store', '');
    LBefore := LTransport.Calls;
    LDefinition := NyxHostedResource(nrkJSON, NyxResourceURL('https://example.com/private.json'))
      .Cache(NyxResourceCache.Memory);
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    Check(LTransport.Calls = LBefore + 2, 'Respect never stores no-store responses');
    Check(LProbe.ResultValue.CacheWrite = rcuNone, 'respected no-store reports no successful write');
    LDefinition := LDefinition.Cache(NyxResourceCache.Persistent.ServerPolicy(rcspOverride));
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    Check(LProbe.ResultValue.CacheWarning <> '', 'missing persistent storage reports memory fallback');
    Check(LProbe.ResultValue.CacheWrite = rcuMemory, 'missing persistent provider reports the actual fallback write');
    LBefore := LTransport.Calls;
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    Check((LProbe.ResultValue.Origin = rloFreshCache) and (LTransport.Calls = LBefore),
      'Override stores no-store in private cache');
    LFault := TFaultCache.Create;
    LResolver := NewNyxResourceResolver(LTransportLease, LMemory, LFault, LClockLease);
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    Check((LProbe.ResultValue.Origin = rloFreshCache) and
      (LProbe.ResultValue.CacheWarning <> ''), 'throwing persistent provider retains cached memory data');
    LDefinition := LDefinition.Cache(NyxResourceCache.Bypass);
    LTransport.Response.Bytes := NyxEncodeUTF8('{broken');
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    Check(not LProbe.ResultValue.Succeeded, 'wrong JSON never becomes accepted loaded data');
    LTransport.Response.Bytes := NyxEncodeUTF8('{"headline":"New"}');
    LTransport.Deferred := True;
    LBefore := LProbe.Calls;
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    LLoad.Cancel;
    LTransport.Send;
    Check(LProbe.Calls = LBefore, 'cancelled late reply cannot reach the receiver');
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    LLoad.Cancel;
    LProbe.Free;
    LProbe := nil;
    LTransport.Send;
    Check(True, 'retired receiver remains untouched by late reply');
  finally

    if LLoad <> nil then
    begin
      LLoad.Cancel;
    end;
    LProbe.Free;
  end;
end;

{$ifndef PAS2JS}
type
  TDeadlineGate = class(TInterfacedObject, INyxWork)
  private
    FRelease: TEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Release;
    procedure Execute(const AExecution: INyxExecution);
  end;

constructor TDeadlineGate.Create;
begin
  inherited Create;
  FRelease := TEvent.Create(nil, True, False, '');
end;

destructor TDeadlineGate.Destroy;
begin
  FRelease.Free;
  inherited Destroy;
end;

procedure TDeadlineGate.Release;
begin
  FRelease.SetEvent;
end;

procedure TDeadlineGate.Execute(const AExecution: INyxExecution);
begin
  while not AExecution.Cancelled and (FRelease.WaitFor(10) <> wrSignaled) do
  begin
  end;
end;

procedure Pump(const AScheduler: INyxScheduler; AProbe: TProbe; AExpected: Integer);
var
  LStart: QWord;
begin
  LStart := GetTickCount64;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);

    if AProbe.Calls >= AExpected then
    begin
      Exit;
    end;
    Sleep(5);
  until GetTickCount64 - LStart > 20000;
  raise ENyxResource.Create('Hosted completion did not arrive');
end;

procedure Native;
var
  LScheduler: INyxScheduler;
  LMonitor: INyxSchedulerMonitor;
  LTransport: INyxResourceTransport;
  LResolver: INyxResourceResolver;
  LDefinition: INyxResourceDefinition;
  LLoad: INyxResourceLoad;
  LProbe: TProbe;
  LDocument: TNyxDocument;
  LPage: INyxColumn;
  LCaption: INyxLabel;
  LInput: INyxInput;
  LResources: INyxResources;
  LCandidate: INyxResources;
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LBefore: TNyxText;
  LStart: QWord;
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LRefused: Boolean;
  LPersistent: INyxResourceCacheStorage;
  LCopyResolver: INyxResourceResolver;
  LGate1: TDeadlineGate;
  LGate2: TDeadlineGate;
  LGateLease1: INyxWork;
  LGateLease2: INyxWork;
begin
  LScheduler := NewNyxScheduler(TNyxSchedulerOptions.Defaults.Workers(2).PendingCapacity(8));
  LTransport := NewNyxNativeResourceTransport(LScheduler);
  LResolver := NewNyxResourceResolver(LTransport);
  LProbe := TProbe.Create;
  LDocument := TNyxDocument.Create;
  LRenderer := TNyxLCLRenderer.Create;
  LHost := TForm.CreateNew(nil);
  try
    LHost.SetBounds(0, 0, 760, 380);
    LDocument.Title := 'Hosted resource workshop';
    LDefinition := NyxHostedResource(nrkJSON, NyxResourceURL(TNyxText(ParamStr(1))))
      .Cache(NyxResourceCache.Memory.FreshFor(60).ServerPolicy(rcspOverride))
      .Fallback(NyxJSONResource('{"service":"Ready while loading","ok":false}'));
    LDocument.Resources.Define(NyxResourceRef('service-copy'), LDefinition);
    LPage := NewNyxColumn('home');
    LDocument.AddPage(LPage);
    LPage.Configure.Padding(24).Gap(16).Done;
    LCaption := NewNyxLabel('headline');
    LPage.Add(LCaption);
    LCaption.Binds.Text(NyxResourceValue(NyxResourceRef('service-copy')).Field('service')).Done;
    LInput := NewNyxInput('project-name');
    LPage.Add(LInput);
    LInput.Binds.Placeholder(NyxResourceValue(NyxResourceRef('service-copy')).Field('service')).Done;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LResources := LDocument.Resources.Clone;
    LBefore := TNyxCodec.Encode(LDocument);
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    Pump(LScheduler, LProbe, 1);
    Check(LProbe.ResultValue.Origin = rloNetwork, 'actual WinHTTP response resolves through core');
    Check(LProbe.ResultValue.Definition.Data.Field('ok').AsBoolean,
      'actual JSON bytes retain Boolean type');
    LCandidate := LResources.Clone;
    LCandidate.Define(NyxResourceRef('service-copy'), LProbe.ResultValue.Definition);
    LRenderer.ReloadResources(LCandidate, NyxDefaultLocale, NyxDefaultLocale);
    LResources := LCandidate;
    Check(TNyxText(TLabel(LRenderer.ControlFor('headline')).Caption) = 'nyx-studio-server',
      'actual loaded caption reaches existing native control');
    Check(TNyxText(TEdit(LRenderer.InputFor('project-name')).TextHint) = 'nyx-studio-server',
      'actual loaded prompt reaches existing native control');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'loaded bytes never overwrite authored declaration');
    LLoad := LResolver.Load(LDefinition, NyxResourceLoadOptions, LProbe.Loaded);
    Check((LProbe.Calls = 2) and (LProbe.ResultValue.Origin = rloFreshCache),
      'actual loaded response is reused under caller override');
    LCandidate := LResources.Clone;
    LCandidate.Define(NyxResourceRef('service-copy'), NyxJSONResource('{"service":42,"ok":true}'));
    LRefused := False;
    try
      LRenderer.ReloadResources(LCandidate, NyxDefaultLocale, NyxDefaultLocale);
    except
      on ENyxResource do
      begin
        LRefused := True;
      end;
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and
      (TNyxText(TLabel(LRenderer.ControlFor('headline')).Caption) = 'nyx-studio-server'),
      'wrong loaded selector retains accepted control projection');
    LLoad := LResolver.Load(LDefinition.Cache(NyxResourceCache.Bypass.MaximumBytes(1)),
      NyxResourceLoadOptions, LProbe.Loaded);
    Pump(LScheduler, LProbe, 3);
    Check((LProbe.ResultValue.Origin = rloFallback) and (LProbe.ResultValue.Error <> ''),
      'actual reply byte budget falls back without partial publication');
    LLoad := LResolver.Load(NyxHostedResource(nrkText, NyxResourceURL('https://example.com/'))
      .Cache(NyxResourceCache.Bypass), NyxResourceLoadOptions, LProbe.Loaded);
    Pump(LScheduler, LProbe, 4);
    Check(LProbe.ResultValue.Succeeded and (LProbe.ResultValue.Origin = rloNetwork) and
      (LProbe.ResultValue.Definition.ByteCount > 0), 'actual system-validated HTTPS file resolves');
    LPersistent := NewNyxFileResourceCache('build/resource-loader/cache');
    LCopyResolver := NewNyxResourceResolver(LTransport, nil, LPersistent);
    LLoad := LCopyResolver.Load(LDefinition.Cache(NyxResourceCache.Persistent
      .ServerPolicy(rcspOverride)), NyxResourceLoadOptions, LProbe.Loaded);
    Pump(LScheduler, LProbe, 5);
    Check(LProbe.ResultValue.Succeeded, 'actual native persistent resolver accepts hosted data');
    LCopyResolver := NewNyxResourceResolver(LTransport, nil,
      NewNyxFileResourceCache('build/resource-loader/cache'));
    LLoad := LCopyResolver.Load(LDefinition.Cache(NyxResourceCache.Persistent
      .ServerPolicy(rcspOverride)), NyxResourceLoadOptions, LProbe.Loaded);
    Check((LProbe.Calls = 6) and (LProbe.ResultValue.Origin = rloFreshCache),
      'fresh file cache survives resolver/provider restart without HTTP');
    Check(LProbe.ResultValue.CacheRead = rcuPersistent, 'real file-cache restart reports persistent read evidence');
    LCopyResolver := NewNyxResourceResolver(LTransport, nil,
      NewNyxFileResourceCache('build/resource-loader/quota-cache', 1, 1));
    LLoad := LCopyResolver.Load(LDefinition.Cache(NyxResourceCache.Persistent
      .ServerPolicy(rcspOverride)), NyxResourceLoadOptions, LProbe.Loaded);
    Pump(LScheduler, LProbe, 7);
    Check((LProbe.ResultValue.Origin = rloNetwork) and (LProbe.ResultValue.CacheWarning <> ''),
      'actual file quota refusal keeps accepted network bytes with explicit memory fallback');
    Check(LProbe.ResultValue.CacheWrite = rcuMemory, 'real file quota failure reports memory storage rather than requested persistent');
    LLoad := LCopyResolver.Load(LDefinition.Cache(NyxResourceCache.Persistent
      .ServerPolicy(rcspOverride)), NyxResourceLoadOptions, LProbe.Loaded);
    Check((LProbe.Calls = 8) and (LProbe.ResultValue.Origin = rloFreshCache),
      'memory copy after failed persistent write is reused before new HTTP');
    Check(LProbe.ResultValue.CacheRead = rcuMemory, 'real fallback hit reports the memory tier actually consumed');
    LGate1 := TDeadlineGate.Create;
    LGateLease1 := LGate1;
    LGate2 := TDeadlineGate.Create;
    LGateLease2 := LGate2;
    LScheduler.Submit(LGateLease1, neThreaded);
    LScheduler.Submit(LGateLease2, neThreaded);
    LMonitor := LScheduler as INyxSchedulerMonitor;
    LStart := GetTickCount64;
    while (LMonitor.WorkerLoad.Running < 2) and (GetTickCount64 - LStart < 10000) do
    begin
      Application.ProcessMessages;
      CheckSynchronize(0);
      Sleep(5);
    end;
    Check(LMonitor.WorkerLoad.Running = 2, 'deadline fixture occupies the bounded workers');
    LLoad := LResolver.Load(LDefinition.Cache(NyxResourceCache.Bypass),
      NyxResourceLoadOptions.WholeRequest(1), LProbe.Loaded);
    Sleep(25);
    LGate1.Release;
    LGate2.Release;
    Pump(LScheduler, LProbe, 9);
    Check((LProbe.Calls = 9) and (LProbe.ResultValue.Origin = rloFallback) and
      (Pos('deadline', LProbe.ResultValue.Error) > 0),
      'queued deadline expires before network admission and reports one fallback');
    LLoad := LResolver.Load(LDefinition.Cache(NyxResourceCache.Bypass),
      NyxResourceLoadOptions, LProbe.Loaded);
    LLoad.Cancel;
    LScheduler.Shutdown;
    LMonitor := LScheduler as INyxSchedulerMonitor;
    LStart := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize(0);
      Sleep(10);
    until (LMonitor.WorkerLoad.ActiveWorkers = 0) or (GetTickCount64 - LStart > 10000);
    Check((LProbe.Calls = 9) and (LMonitor.WorkerLoad.ActiveWorkers = 0),
      'cancelled native request and bounded workers retire without delivery');
    LHost.Show;
    Application.ProcessMessages;
    LBitmap := TBitmap.Create;
    try
      LBitmap.SetSize(LHost.ClientWidth, LHost.ClientHeight);
      LHost.PaintTo(LBitmap.Canvas.Handle, 0, 0);
      LImage := LBitmap.CreateIntfImage;
      try
        LImage.SaveToFile(ParamStr(2));
      finally
        LImage.Free;
      end;
    finally
      LBitmap.Free;
    end;
  finally

    if LLoad <> nil then
    begin
      LLoad.Cancel;
    end;
    LScheduler.Shutdown;
    LRenderer.Free;
    LHost.Free;
    LDocument.Free;
    LProbe.Free;
  end;
end;
{$endif}

{$ifdef PAS2JS}
type
  TBrowserJourney = class
  private
    FDocument: TNyxDocument;
    FRenderer: TNyxBrowserRenderer;
    FResolver: INyxResourceResolver;
    FDefinition: INyxResourceDefinition;
    FLoad: INyxResourceLoad;
    FStage: Integer;
    procedure Loaded(const AResult: TNyxResourceLoadResult);
  public
    destructor Destroy; override;
    procedure Start;
  end;

var
  GJourney: TBrowserJourney;

destructor TBrowserJourney.Destroy;
begin

  if FLoad <> nil then
  begin
    FLoad.Cancel;
  end;
  FRenderer.Free;
  FDocument.Free;
  inherited Destroy;
end;

procedure TBrowserJourney.Start;
var
  LPage: INyxColumn;
  LCaption: INyxLabel;
  LInput: INyxInput;
  LHost: TJSHTMLElement;
begin
  FDocument := TNyxDocument.Create;
  FDocument.Title := 'Hosted resource workshop';
  FDefinition := NyxHostedResource(nrkJSON,
    NyxResourceURL(window.location.origin + '/api/health'))
    .Cache(NyxResourceCache.Memory.ServerPolicy(rcspOverride))
    .Fallback(NyxJSONResource('{"service":"Ready while loading","ok":false}'));
  FDocument.Resources.Define(NyxResourceRef('service-copy'), FDefinition);
  LPage := NewNyxColumn('home');
  FDocument.AddPage(LPage);
  LPage.Configure.Padding(24).Gap(16).Done;
  LCaption := NewNyxLabel('headline');
  LPage.Add(LCaption);
  LCaption.Binds.Text(NyxResourceValue(NyxResourceRef('service-copy')).Field('service')).Done;
  LInput := NewNyxInput('project-name');
  LPage.Add(LInput);
  LInput.Binds.Placeholder(NyxResourceValue(NyxResourceRef('service-copy')).Field('service')).Done;
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  FRenderer := TNyxBrowserRenderer.Create;
  FRenderer.Render(FDocument, FDocument.Pages[0], LHost);
  FResolver := NewNyxResourceResolver(NewNyxBrowserResourceTransport);
  FStage := 1;
  FLoad := FResolver.Load(FDefinition, NyxResourceLoadOptions, Loaded);
end;

procedure TBrowserJourney.Loaded(const AResult: TNyxResourceLoadResult);
var
  LResources: INyxResources;
begin
  try

    if FStage = 1 then
    begin
      Check(AResult.Origin = rloNetwork, 'actual browser fetch resolves hosted data');
      Check(AResult.Definition.Data.Field('ok').AsBoolean, 'browser JSON retains Boolean type');
      LResources := FDocument.Resources.Clone;
      LResources.Define(NyxResourceRef('service-copy'), AResult.Definition);
      FRenderer.ReloadResources(LResources, NyxDefaultLocale, NyxDefaultLocale);
      Check(FRenderer.ElementFor('headline').textContent = 'nyx-studio-server',
        'actual loaded browser caption');
      Check(TJSHTMLInputElement(FRenderer.InputFor('project-name')).placeholder = 'nyx-studio-server',
        'actual loaded browser prompt');
      FStage := 2;
      FLoad := FResolver.Load(FDefinition, NyxResourceLoadOptions, Loaded);
      Exit;
    end;
    Check(AResult.Origin = rloFreshCache, 'actual browser memory hit avoids another fetch');
    FStage := 3;
    document.body.setAttribute('data-nyx-result', 'passed');
    document.body.setAttribute('data-nyx-checks', IntToStr(GChecks));
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-result', 'failed');
      document.body.setAttribute('data-nyx-error', LException.Message);
      raise;
    end;
  end;
end;
{$endif}


begin
  {$ifndef PAS2JS}
  Application.Initialize;
  {$endif}
  Shared;
  {$ifndef PAS2JS}

  if ParamCount = 2 then
  begin
    Native;
  end;
  WriteLn('PASS / resource loader / ', GChecks, ' checks');
  {$else}
  GJourney := TBrowserJourney.Create;
  GJourney.Start;
  {$endif}
end.
