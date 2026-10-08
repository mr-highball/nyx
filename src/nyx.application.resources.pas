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

unit nyx.application.resources;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.data,
  nyx.bytes,
  nyx.state,
  nyx.resources,
  nyx.resource.sources,
  nyx.resource.context,
  nyx.resources.loader,
  nyx.scheduler,
  nyx.publication;

type
  { Automatic begins after the host has mounted; on-demand never starts a
    transport until Reload. Defaults selects four requests; concurrency is 1..8.
    Options are copied and contain no target handles. Initialize with Defaults;
    zeroed options refuse construction rather than allow unbounded work. }
  TNyxResourceLoading = (nrlAutomatic, nrlOnDemand);
  TNyxApplicationResourceOptions = record
  private
    FLoading: TNyxResourceLoading;
    FConcurrency: Integer;
    FRequest: TNyxResourceLoadOptions;
    FLocale: TNyxLocaleRef;
    FFallback: TNyxLocaleRef;
  public
    class function Defaults: TNyxApplicationResourceOptions; static;
    function Loading(AMode: TNyxResourceLoading): TNyxApplicationResourceOptions;
    function ConcurrentRequests(ACount: Integer): TNyxApplicationResourceOptions;
    function Request(const AOptions: TNyxResourceLoadOptions): TNyxApplicationResourceOptions;
    function Localize(const ALocale, AFallback: TNyxLocaleRef): TNyxApplicationResourceOptions;
    procedure Validate;
    property LoadingMode: TNyxResourceLoading read FLoading;
    property Concurrency: Integer read FConcurrency;
    property RequestOptions: TNyxResourceLoadOptions read FRequest;
    property Locale: TNyxLocaleRef read FLocale;
    property Fallback: TNyxLocaleRef read FFallback;
  end;

  { Ready includes a successful authored fallback; Origin distinguishes that
    from a successful network request. Waiting retains a loaded result while a
    receiver is busy. Rejected means the final catalog failed typed admission.
    Error/CacheWarning retain resolver diagnostics. NotificationError reports a
    receiver failure AFTER publication; it does not imply rolled-back data. }
  TNyxApplicationResourcePhase = (nrpIdle, nrpQueued, nrpLoading, nrpWaiting,
    nrpReady, nrpFailed, nrpRejected, nrpCancelled);
  TNyxApplicationResourceStatus = record
    Phase: TNyxApplicationResourcePhase;
    Origin: TNyxResourceLoadOrigin;
    Error: TNyxText;
    CacheWarning: TNyxText;
    NotificationError: TNyxText;
    CacheRead: TNyxResourceCacheUse;
    CacheWrite: TNyxResourceCacheUse;
  end;

  { A runtime attempt and the last installed load are distinct. Queued, failed,
    rejected and cancelled reloads can leave an earlier publication installed.
    HasPublishedLoad is False for initial authored defaults/fallbacks; callers
    must not present those defaults as a completed hosted request. }
  TNyxResourceRuntimeEntry = record
    Reference: TNyxResourceRef;
    Locale: TNyxLocaleRef;
    Kind: TNyxResourceKind;
    Hosted: Boolean;
    Policy: TNyxResourceCachePolicy;
    Status: TNyxApplicationResourceStatus;
    HasPublishedLoad: Boolean;
    PublishedOrigin: TNyxResourceLoadOrigin;
    PublishedCacheRead: TNyxResourceCacheUse;
    PublishedCacheWrite: TNyxResourceCacheUse;
  end;

  { Immutable diagnostic membership, captured on the owner's UI thread. Entries
    and metadata outlive the owner without retaining a document, scheduler,
    resolver or control. No payload or transport authority is exported. Adapter
    diagnostics may mention addresses/paths. Page returns at most sixteen entries
    within 40 KiB of item JSON and clips each diagnostic at 512 Unicode scalars.
    MatchesDeclarations compares exact immutable authored
    values at a trusted enrollment/publication boundary, not a lossy hash. }
  INyxResourceRuntimeSnapshot = interface
    ['{5D579E29-3A3C-4666-A2B7-7099E0C9D222}']
    function GetCount: Integer;
    function GetStopped: Boolean;
    function Entry(AIndex: Integer): TNyxResourceRuntimeEntry;
    function MatchesDeclarations(const AResources: INyxResources): Boolean;
    function Summary: TNyxDataValue;
    function Page(AOffset: Integer; ALimit: Integer = 8): TNyxDataValue;
    property Count: Integer read GetCount;
    property Stopped: Boolean read GetStopped;
  end;

  { Optional capability preserves the original application interface/GUID.
    Capturing is read-only, including after Stop. It neither starts a transport
    nor grants an agent permission to reload/cancel the application. }
  INyxApplicationResourceDiagnostics = interface
    ['{BF13183B-905B-44A0-955E-2A61F073330E}']
    function CaptureRuntime: INyxResourceRuntimeSnapshot;
  end;

  { Validators are ordered, borrowed UI-thread method receivers: True accepts,
    False waits until Wake; exceptions reject without changing Context.
    Changed runs only after all validators accept and Context is published.
    Callers must disconnect before destroying their receivers. Neither callback
    may reload/localize this owner during dispatch. Disconnect/Stop may retire
    receivers from a callback; remaining revoked callbacks are skipped safely.
    Snapshots safely outlive the owner. }
  TNyxResourceContextValidator = function(
    const AContext: INyxResourceContext): Boolean of object;
  TNyxResourceContextChanged = procedure(const AContext: INyxResourceContext) of object;
  { True supplies a nonnil single-use model preparation; False waits without
    publication. The owner retires every supplied stage on refusal, failure or
    completion. Prepare runs after ordered readiness validators, before any
    install. A receiver must disconnect before destruction; its stage must revoke
    borrowed target/model pointers independently. }
  TNyxResourceContextPrepare = function(const AContext: INyxResourceContext;
    out APrepared: INyxPreparedPublication): Boolean of object;
  INyxResourceSubscription = interface
    ['{A7B3C98D-A747-4181-8514-465522EACF21}']
    procedure Disconnect;
  end;

  { Optional capability leaves the original resource owner interface/GUID
    unchanged. Catalog, scalar projections and all source-backed row scopes
    install before any Changed/store/view observer executes. Physical target
    painting remains sequential notification work. }
  INyxPreparedApplicationResources = interface
    ['{66E4D889-C67E-44C3-A1E2-873148554CA2}']
    function SubscribePrepared(AValidator: TNyxResourceContextValidator;
      APrepare: TNyxResourceContextPrepare): INyxResourceSubscription;
  end;

  { An independent application catalog and bounded loader, never a document or
    renderer owner. Reload always uses immutable authored declarations, even
    after a previous resolution replaced a runtime entry with embedded bytes.
    All methods require the scheduler's UI thread. Loading/result publication
    is coalesced through PostUI; synchronous resolver replies cannot mutate a
    half-built view. Stop revokes callbacks, pending work and loads without
    shutting down the borrowed scheduler. A stopped retained owner remains
    inspectable, but refuses new loading/localization/subscriptions. }
  INyxApplicationResources = interface(INyxResourceUpdateQueue)
    ['{9BE5D90A-2D2B-4474-8F31-161AF09991D9}']
    function Declaration(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): INyxResourceDefinition;
    function Status(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): TNyxApplicationResourceStatus;
    function Subscribe(AValidator: TNyxResourceContextValidator;
      AChanged: TNyxResourceContextChanged): INyxResourceSubscription;
    procedure Start;
    procedure Reload; overload;
    { Embedded declarations already are immutable runtime values: explicit
      reload is a no-op. Hosted variants resolve the original declaration. }
    procedure Reload(const AReference: TNyxResourceRef; const ALocale: TNyxLocaleRef); overload;
    procedure Cancel;
    procedure Stop;
    { Explicit locale changes are atomic and synchronous. Busy receivers refuse;
      callers can retry after the ordinary UI operation has completed. }
    procedure Localize(const ALocale, AFallback: TNyxLocaleRef);
  end;

const
  { Complete status-only reports allow worst-case JSON diagnostic escaping for
    all 128 variants; semantic query pages retain their separate 40-KiB budget. }
  NyxMaximumRuntimeReportBytes = 2 * 1024 * 1024;

function NyxApplicationResourceOptions: TNyxApplicationResourceOptions;
function NyxApplicationResourceDiagnostics(
  const AResources: INyxApplicationResources): INyxApplicationResourceDiagnostics;
function NyxResourcePhaseName(AValue: TNyxApplicationResourcePhase): TNyxText;
function NyxResourceOriginName(AValue: TNyxResourceLoadOrigin): TNyxText;
function NyxResourceCacheUseName(AValue: TNyxResourceCacheUse): TNyxText;
{ Explicit private reporting boundary. Payloads never cross this wire. Decode
  requires the complete trusted declaration catalog and exact variant membership;
  diagnostic text uses the same bounded Unicode windows as semantic pages. }
function EncodeNyxResourceRuntime(const ASnapshot: INyxResourceRuntimeSnapshot): TNyxDataValue;
function DecodeNyxResourceRuntime(const AData: TNyxDataValue;
  const ADeclarations: INyxResources): INyxResourceRuntimeSnapshot;
{ Refuses an alternative owner without coordinated publication support. }
function NyxPreparedApplicationResources(const AResources: INyxApplicationResources): INyxPreparedApplicationResources;
function NewNyxApplicationResources(const ADeclarations: INyxResources;
  const AScheduler: INyxScheduler; const AOptions: TNyxApplicationResourceOptions;
  const AResolver: INyxResourceResolver = nil): INyxApplicationResources;

implementation

uses nyx.editing;

type
  TApplicationResources = class;
  TResourceJob = class;
  TResourceSubscription = class(TInterfacedObject, INyxResourceSubscription)
  private
    FOwner: TApplicationResources; { weak, revoked before owner disposal }
    FValidator: TNyxResourceContextValidator;
    FChanged: TNyxResourceContextChanged;
    FPrepare: TNyxResourceContextPrepare;
  public
    procedure Disconnect;
    function Validate(const AContext: INyxResourceContext): Boolean;
    procedure Changed(const AContext: INyxResourceContext);
    function Prepare(const AContext: INyxResourceContext;
      out APrepared: INyxPreparedPublication): Boolean;
  end;
  TSubscriptions = array of INyxResourceSubscription;
  TSubscriptionObjects = array of TResourceSubscription;

  IResourcePumpPort = interface
    ['{9C5E33C9-2471-426D-8679-AD4CF671D751}']
    procedure RunPump;
  end;
  TResourcePumpPort = class(TInterfacedObject, IResourcePumpPort)
  public
    Owner: TApplicationResources; { weak; work never keeps the application alive }
    procedure RunPump;
  end;
  TResourcePump = class(TInterfacedObject, INyxWork)
  private
    FPort: IResourcePumpPort;
  public
    constructor Create(const APort: IResourcePumpPort);
    procedure Execute(const AExecution: INyxExecution);
  end;
  IResourceJob = interface
    ['{888E717A-F406-4D19-8CA9-84B6C387A630}']
    procedure Cancel;
  end;
  TResourceJob = class(TInterfacedObject, IResourceJob)
  private
    FOwner: TApplicationResources; { weak; Cancel disconnects before transport }
    FIndex: Integer;
    FLoad: INyxResourceLoad;
    procedure Received(const AResult: TNyxResourceLoadResult);
  public
    procedure Start(const AResolver: INyxResourceResolver;
      const ADefinition: INyxResourceDefinition; const AOptions: TNyxResourceLoadOptions);
    procedure Cancel;
  end;
  TResourceSlot = class
  public
    Reference: TNyxResourceRef;
    Locale: TNyxLocaleRef;
    Status: TNyxApplicationResourceStatus;
    Job: IResourceJob;
    JobObject: TResourceJob; { borrowed from Job, exact identity guards late replies }
    Result: TNyxResourceLoadResult;
    HaveResult: Boolean;
    HasPublishedLoad: Boolean;
    PublishedOrigin: TNyxResourceLoadOrigin;
    PublishedCacheRead: TNyxResourceCacheUse;
    PublishedCacheWrite: TNyxResourceCacheUse;
    destructor Destroy; override;
  end;

  TApplicationResources = class(TInterfacedObject, INyxResourceUpdateQueue,
    INyxApplicationResources, INyxPreparedApplicationResources,
    INyxApplicationResourceDiagnostics)
  private
    FDeclarations: INyxResources;
    FContext: INyxResourceContext;
    FScheduler: INyxScheduler;
    FOptions: TNyxApplicationResourceOptions;
    FResolver: INyxResourceResolver;
    FSlots: array of TResourceSlot;
    FTokens: TSubscriptions;
    FTokenObjects: TSubscriptionObjects;
    FPort: IResourcePumpPort;
    FPortObject: TResourcePumpPort;
    FExecution: INyxExecution;
    FStopped: Boolean;
    FDispatching: Boolean;
    FPumping: Boolean;
    FStarted: Boolean;
    FWakeRequested: Boolean;
    procedure RequireOpen;
    function AddSubscription(AValidator: TNyxResourceContextValidator;
      AChanged: TNyxResourceContextChanged; APrepare: TNyxResourceContextPrepare): INyxResourceSubscription;
    function IndexOf(const AReference: TNyxResourceRef; const ALocale: TNyxLocaleRef): Integer;
    procedure Disconnect(AToken: TResourceSubscription);
    procedure Received(AJob: TResourceJob; AIndex: Integer; const AResult: TNyxResourceLoadResult);
    function Publish(const AContext: INyxResourceContext; out ANotificationError: TNyxText;
      ALoadIndex: Integer = -1; AOrigin: TNyxResourceLoadOrigin = rloFailed): Boolean;
    procedure Pump;
    procedure Queue(AIndex: Integer);
    procedure CancelPending;
  public
    constructor Create(const ADeclarations: INyxResources;
      const AScheduler: INyxScheduler; const AOptions: TNyxApplicationResourceOptions;
      const AResolver: INyxResourceResolver);
    destructor Destroy; override;
    function GetContext: INyxResourceContext;
    function CaptureRuntime: INyxResourceRuntimeSnapshot;
    procedure Wake;
    function Declaration(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): INyxResourceDefinition;
    function Status(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): TNyxApplicationResourceStatus;
    function Subscribe(AValidator: TNyxResourceContextValidator;
      AChanged: TNyxResourceContextChanged): INyxResourceSubscription;
    function SubscribePrepared(AValidator: TNyxResourceContextValidator;
      APrepare: TNyxResourceContextPrepare): INyxResourceSubscription;
    procedure Start;
    procedure Reload; overload;
    procedure Reload(const AReference: TNyxResourceRef; const ALocale: TNyxLocaleRef); overload;
    procedure Cancel;
    procedure Stop;
    procedure Localize(const ALocale, AFallback: TNyxLocaleRef);
  end;

  TResourceFramePreparation = class(TInterfacedObject, INyxPreparedPublication)
  private
    FOwner: TApplicationResources;
    FLease: INyxApplicationResources;
    FContext: INyxResourceContext;
    FPrevious: INyxResourceContext;
    FTokens: TSubscriptions;
    FObjects: TSubscriptionObjects;
    FLoadIndex: Integer;
    FOrigin: TNyxResourceLoadOrigin;
  public
    constructor Create(AOwner: TApplicationResources; const AContext: INyxResourceContext;
      const ATokens: TSubscriptions; const AObjects: TSubscriptionObjects;
      ALoadIndex: Integer; AOrigin: TNyxResourceLoadOrigin);
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

{$I nyx.application.resources.diagnostics.inc}

function NyxPreparedApplicationResources(const AResources: INyxApplicationResources): INyxPreparedApplicationResources;
begin

  if (AResources = nil) or not Supports(AResources, INyxPreparedApplicationResources, Result) then
  begin
    raise ENyxResource.Create('Application resources require coordinated publication support');
  end;
end;

constructor TResourceFramePreparation.Create(AOwner: TApplicationResources;
  const AContext: INyxResourceContext; const ATokens: TSubscriptions;
  const AObjects: TSubscriptionObjects; ALoadIndex: Integer; AOrigin: TNyxResourceLoadOrigin);
begin
  inherited Create;
  FOwner := AOwner;
  FLease := AOwner;
  FContext := AContext;
  FPrevious := AOwner.FContext;
  FTokens := Copy(ATokens);
  FObjects := Copy(AObjects);
  FLoadIndex := ALoadIndex;
  FOrigin := AOrigin;
end;

procedure TResourceFramePreparation.Validate;
begin

  if FOwner.FStopped then
  begin
    raise ENyxResource.Create('Application resource publication was stopped');
  end;
end;

procedure TResourceFramePreparation.Install;
begin
  FOwner.FContext := FContext;

  if FLoadIndex >= 0 then
  begin
    { Accepted catalog and installed-load evidence are model state. Exchange
      both before any Changed/control/store observer runs, including one that
      captures a runtime report or destroys the application from that callback. }
    FOwner.FSlots[FLoadIndex].HasPublishedLoad := True;
    FOwner.FSlots[FLoadIndex].PublishedOrigin := FOrigin;
    FOwner.FSlots[FLoadIndex].PublishedCacheRead := FOwner.FSlots[FLoadIndex].Status.CacheRead;
    FOwner.FSlots[FLoadIndex].PublishedCacheWrite := FOwner.FSlots[FLoadIndex].Status.CacheWrite;
    FOwner.FSlots[FLoadIndex].Status.Phase := nrpReady;
  end;
end;

procedure TResourceFramePreparation.Notify;
var
  LIndex: Integer;
  LError: TNyxText;
  LFailed: Boolean;
begin
  LError := '';
  LFailed := False;
  for LIndex := 0 to High(FObjects) do
  begin
    try
      FObjects[LIndex].Changed(FContext);
    except
      on LException: Exception do
      begin

        if not LFailed then
        begin
          LFailed := True;
          {$IFDEF PAS2JS}
          LError := LException.Message;
          {$ELSE}

          if LException is ENyxState then
          begin
            LError := RawByteString(LException.Message);
            SetCodePage(RawByteString(LError), CP_UTF8, False);
          end
          else
          begin
            LError := TNyxText(LException.Message);
          end;
          {$ENDIF}
        end;
      end;
    end;
  end;

  if LFailed then
  begin
    raise ENyxPublicationNotification.CreateReceiverFailure(LError);
  end;
end;

procedure TResourceFramePreparation.Retire;
begin
  FTokens := nil;
  FObjects := nil;
  FContext := nil;
  FPrevious := nil;
  FOwner := nil;
  FLease := nil;
end;

class function TNyxApplicationResourceOptions.Defaults: TNyxApplicationResourceOptions;
begin
  Result := Default(TNyxApplicationResourceOptions);
  Result.FConcurrency := 4;
  Result.FRequest := NyxResourceLoadOptions;
end;

function NyxApplicationResourceOptions: TNyxApplicationResourceOptions;
begin
  Result := TNyxApplicationResourceOptions.Defaults;
end;

function TNyxApplicationResourceOptions.Loading(
  AMode: TNyxResourceLoading): TNyxApplicationResourceOptions;
begin
  Result := Self;
  Result.FLoading := AMode;
end;

function TNyxApplicationResourceOptions.ConcurrentRequests(
  ACount: Integer): TNyxApplicationResourceOptions;
begin

  if (ACount < 1) or (ACount > 8) then
  begin
    raise ENyxResource.Create('Application resource concurrency must be 1..8');
  end;
  Result := Self;
  Result.FConcurrency := ACount;
end;

function TNyxApplicationResourceOptions.Request(
  const AOptions: TNyxResourceLoadOptions): TNyxApplicationResourceOptions;
begin
  Result := Self;
  Result.FRequest := AOptions;
end;

function TNyxApplicationResourceOptions.Localize(
  const ALocale, AFallback: TNyxLocaleRef): TNyxApplicationResourceOptions;
begin
  Result := Self;
  Result.FLocale := ALocale;
  Result.FFallback := AFallback;
end;

procedure TNyxApplicationResourceOptions.Validate;
begin
  FRequest.Validate;

  if (FConcurrency < 1) or (FConcurrency > 8) or (FRequest.DeadlineMS < 1) then
  begin
    raise ENyxResource.Create('Initialize application resource options with Defaults');
  end;
end;

constructor TApplicationResources.Create(const ADeclarations: INyxResources;
  const AScheduler: INyxScheduler; const AOptions: TNyxApplicationResourceOptions;
  const AResolver: INyxResourceResolver);
var
  LIndex: Integer;
begin
  inherited Create;

  if (ADeclarations = nil) or (AScheduler = nil) then
  begin
    raise ENyxResource.Create('Application resources require declarations and a scheduler');
  end;
  AScheduler.RequireUI;
  AOptions.Validate;
  FDeclarations := NyxResourcesFromData(ADeclarations.ToData);
  FScheduler := AScheduler;
  FOptions := AOptions;
  FResolver := AResolver;
  FContext := NewNyxResourceContext(FDeclarations, AOptions.Locale, AOptions.Fallback);
  FPortObject := TResourcePumpPort.Create;
  FPort := FPortObject;
  FPortObject.Owner := Self;
  SetLength(FSlots, FDeclarations.Count);
  for LIndex := 0 to Length(FSlots) - 1 do
  begin
    FSlots[LIndex] := TResourceSlot.Create;
    FSlots[LIndex].Reference := FDeclarations.Reference(LIndex);
    FSlots[LIndex].Locale := FDeclarations.Locale(LIndex);

    if FDeclarations.Definition(FSlots[LIndex].Reference,
      FSlots[LIndex].Locale).Source.Kind = rskEmbedded then
    begin
      FSlots[LIndex].Status.Phase := nrpReady;
      FSlots[LIndex].Status.Origin := rloEmbedded;
    end;
  end;
end;

destructor TApplicationResources.Destroy;
var
  LIndex: Integer;
begin
  Stop;
  for LIndex := 0 to Length(FSlots) - 1 do
  begin
    FSlots[LIndex].Free;
  end;
  inherited Destroy;
end;

procedure TApplicationResources.RequireOpen;
begin
  FScheduler.RequireUI;

  if FStopped or FDispatching then
  begin
    raise ENyxResource.Create('Application resource owner is stopped or dispatching');
  end;
end;

function TApplicationResources.GetContext: INyxResourceContext;
begin
  FScheduler.RequireUI;
  Result := FContext;
end;

function TApplicationResources.IndexOf(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(FSlots) - 1 do
  begin

    if (FSlots[LIndex].Reference.Name = AReference.Name) and
      (FSlots[LIndex].Locale.Name = ALocale.Name) then
    begin
      Exit(LIndex);
    end;
  end;
  raise ENyxResource.Create('Unknown application resource variant: ' + AReference.Name);
end;

function TApplicationResources.Declaration(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): INyxResourceDefinition;
begin
  FScheduler.RequireUI;
  Result := FDeclarations.Definition(AReference, ALocale);
end;

function TApplicationResources.Status(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): TNyxApplicationResourceStatus;
begin
  FScheduler.RequireUI;
  Result := FSlots[IndexOf(AReference, ALocale)].Status;
end;

function TApplicationResources.AddSubscription(AValidator: TNyxResourceContextValidator;
  AChanged: TNyxResourceContextChanged; APrepare: TNyxResourceContextPrepare): INyxResourceSubscription;
var
  LToken: TResourceSubscription;
  LIndex: Integer;
begin
  RequireOpen;

  if (not Assigned(AValidator)) and (not Assigned(AChanged)) and (not Assigned(APrepare)) then
  begin
    raise ENyxResource.Create('A resource subscription requires a receiver');
  end;

  if Length(FTokens) >= 128 then
  begin
    raise ENyxResource.Create('Application resource subscription capacity reached');
  end;
  LToken := TResourceSubscription.Create;
  Result := LToken;
  LToken.FOwner := Self;
  LToken.FValidator := AValidator;
  LToken.FChanged := AChanged;
  LToken.FPrepare := APrepare;
  LIndex := Length(FTokens);
  SetLength(FTokens, LIndex + 1);
  SetLength(FTokenObjects, LIndex + 1);
  FTokens[LIndex] := Result;
  FTokenObjects[LIndex] := LToken;
end;

function TApplicationResources.Subscribe(AValidator: TNyxResourceContextValidator;
  AChanged: TNyxResourceContextChanged): INyxResourceSubscription;
begin
  Result := AddSubscription(AValidator, AChanged, nil);
end;

function TApplicationResources.SubscribePrepared(AValidator: TNyxResourceContextValidator;
  APrepare: TNyxResourceContextPrepare): INyxResourceSubscription;
begin

  if not Assigned(APrepare) then
  begin
    raise ENyxResource.Create('Prepared resource subscription requires a preparer');
  end;
  Result := AddSubscription(AValidator, nil, APrepare);
end;

procedure TApplicationResources.Disconnect(AToken: TResourceSubscription);
var
  LIndex: Integer;
  LNext: Integer;
begin
  FScheduler.RequireUI;
  for LIndex := 0 to Length(FTokens) - 1 do
  begin

    if FTokenObjects[LIndex] = AToken then
    begin
      for LNext := LIndex + 1 to Length(FTokens) - 1 do
      begin
        FTokens[LNext - 1] := FTokens[LNext];
        FTokenObjects[LNext - 1] := FTokenObjects[LNext];
      end;
      SetLength(FTokens, Length(FTokens) - 1);
      SetLength(FTokenObjects, Length(FTokenObjects) - 1);
      Exit;
    end;
  end;
end;

procedure TResourceSubscription.Disconnect;
var
  LOwner: TApplicationResources;
  LLease: INyxResourceSubscription;
begin
  LLease := Self;
  LOwner := FOwner;

  if LOwner <> nil then
  begin
    LOwner.FScheduler.RequireUI;
    FOwner := nil;
    FValidator := nil;
    FChanged := nil;
    FPrepare := nil;
    LOwner.Disconnect(Self);
  end;
end;

function TResourceSubscription.Validate(const AContext: INyxResourceContext): Boolean;
begin
  Result := True;

  if (FOwner <> nil) and Assigned(FValidator) then
  begin
    Result := FValidator(AContext);
  end;
end;

procedure TResourceSubscription.Changed(const AContext: INyxResourceContext);
begin

  if (FOwner <> nil) and Assigned(FChanged) then
  begin
    FChanged(AContext);
  end;
end;

function TResourceSubscription.Prepare(const AContext: INyxResourceContext;
  out APrepared: INyxPreparedPublication): Boolean;
begin
  APrepared := nil;
  Result := True;

  if (FOwner <> nil) and Assigned(FPrepare) then
  begin
    Result := FPrepare(AContext, APrepared);

    if Result and (APrepared = nil) then
    begin
      raise ENyxResource.Create('Accepted resource preparation is missing');
    end;
  end;
end;

function TApplicationResources.Publish(const AContext: INyxResourceContext;
  out ANotificationError: TNyxText; ALoadIndex: Integer;
  AOrigin: TNyxResourceLoadOrigin): Boolean;
var
  LTokens: TSubscriptions;
  LObjects: TSubscriptionObjects;
  LIndex: Integer;
  LNext: Integer;
  LPrepared: TNyxPreparedPublications;
  LStage: INyxPreparedPublication;
begin
  ANotificationError := '';
  LTokens := Copy(FTokens);
  LObjects := Copy(FTokenObjects);
  FDispatching := True;
  try
    for LIndex := 0 to Length(LTokens) - 1 do
    begin

      if not LObjects[LIndex].Validate(AContext) or FStopped then
      begin
        Exit(False);
      end;
    end;
    SetLength(LPrepared, 1);
    LPrepared[0] := TResourceFramePreparation.Create(Self, AContext, LTokens, LObjects,
      ALoadIndex, AOrigin);
    for LIndex := 0 to Length(LTokens) - 1 do
    begin
      LStage := nil;

      if not LObjects[LIndex].Prepare(AContext, LStage) or FStopped then
      begin

        if LStage <> nil then
        begin
          LStage.Retire;
        end;
        Exit(False);
      end;

      if LStage <> nil then
      begin
        LNext := Length(LPrepared);
        SetLength(LPrepared, LNext + 1);
        LPrepared[LNext] := LStage;
      end;
    end;
    try
      PublishNyxGroup(LPrepared);
    except
      on LError: ENyxPublicationNotification do
      begin
        ANotificationError := LError.ReceiverMessage;

        if ANotificationError = '' then
        begin
          ANotificationError := 'Receiver supplied no diagnostic text';
        end;
      end;
    end;
    Result := True;
  finally
    { A failing extension may have supplied an out stage before raising, so it
      has not yet entered the owned vector. Retire that last handoff explicitly. }

    if LStage <> nil then
    begin
      LStage.Retire;
    end;
    for LIndex := 0 to High(LPrepared) do
    begin

      if LPrepared[LIndex] <> nil then
      begin
        LPrepared[LIndex].Retire;
      end;
    end;
    FDispatching := False;
  end;
end;

procedure TResourcePumpPort.RunPump;
var
  LLease: INyxApplicationResources;
  LOwner: TApplicationResources;
begin
  LOwner := Owner;

  if LOwner <> nil then
  begin
    LLease := LOwner;
    LOwner.Pump;
  end;
end;

constructor TResourcePump.Create(const APort: IResourcePumpPort);
begin
  inherited Create;
  FPort := APort;
end;

procedure TResourcePump.Execute(const AExecution: INyxExecution);
begin

  if not AExecution.Cancelled then
  begin
    FPort.RunPump;
  end;
end;

procedure TApplicationResources.Wake;
var
  LWork: INyxWork;
  LIndex: Integer;
  LNeeded: Boolean;
begin
  FScheduler.RequireUI;

  if FStopped then
  begin
    Exit;
  end;
  LNeeded := False;
  for LIndex := 0 to Length(FSlots) - 1 do
  begin

    if FSlots[LIndex].HaveResult or (FSlots[LIndex].Status.Phase = nrpQueued) then
    begin
      LNeeded := True;
      Break;
    end;
  end;

  if not LNeeded then
  begin
    Exit;
  end;

  if FPumping then
  begin
    FWakeRequested := True;
    Exit;
  end;

  if FExecution <> nil then
  begin
    Exit;
  end;
  LWork := TResourcePump.Create(FPort);
  FExecution := FScheduler.PostUI(LWork);
end;

procedure TResourceJob.Start(const AResolver: INyxResourceResolver;
  const ADefinition: INyxResourceDefinition; const AOptions: TNyxResourceLoadOptions);
var
  LLoad: INyxResourceLoad;
  LLease: IResourceJob;
begin
  LLease := Self;
  LLoad := AResolver.Load(ADefinition, AOptions, Received);

  if LLoad = nil then
  begin
    raise ENyxResource.Create('Resource resolver returned no cancellation token');
  end;

  if FOwner <> nil then
  begin
    FLoad := LLoad;
  end
  else
  begin
    { An inline reply already retired this receiver before Load returned. }
    LLoad.Cancel;
  end;
end;

procedure TResourceJob.Cancel;
begin
  FOwner := nil;

  if FLoad <> nil then
  begin
    FLoad.Cancel;
    FLoad := nil;
  end;
end;

procedure TResourceJob.Received(const AResult: TNyxResourceLoadResult);
var
  LLease: IResourceJob;
  LOwnerLease: INyxApplicationResources;
  LOwner: TApplicationResources;
begin
  LLease := Self;
  LOwner := FOwner;

  if LOwner <> nil then
  begin
    LOwnerLease := LOwner;
    LOwner.Received(Self, FIndex, AResult);
  end;
end;

destructor TResourceSlot.Destroy;
begin

  if Job <> nil then
  begin
    Job.Cancel;
  end;
  inherited Destroy;
end;

procedure TApplicationResources.Received(AJob: TResourceJob; AIndex: Integer;
  const AResult: TNyxResourceLoadResult);
var
  LSlot: TResourceSlot;
begin
  FScheduler.RequireUI;

  if FStopped then
  begin
    Exit;
  end;
  LSlot := FSlots[AIndex];

  if LSlot.JobObject <> AJob then
  begin
    Exit;
  end;
  LSlot.Result := AResult;
  LSlot.HaveResult := True;
  LSlot.Status.Origin := AResult.Origin;
  LSlot.Status.Error := AResult.Error;
  LSlot.Status.CacheWarning := AResult.CacheWarning;
  LSlot.Status.CacheRead := AResult.CacheRead;
  LSlot.Status.CacheWrite := AResult.CacheWrite;
  LSlot.Status.Phase := nrpWaiting;
  LSlot.Job.Cancel;
  LSlot.Job := nil;
  LSlot.JobObject := nil;
  Wake;
end;

procedure TApplicationResources.Queue(AIndex: Integer);
var
  LSlot: TResourceSlot;
begin
  LSlot := FSlots[AIndex];

  if LSlot.Job <> nil then
  begin
    LSlot.Job.Cancel;
    LSlot.Job := nil;
    LSlot.JobObject := nil;
  end;
  LSlot.HaveResult := False;
  LSlot.Result := Default(TNyxResourceLoadResult);
  LSlot.Status := Default(TNyxApplicationResourceStatus);
  LSlot.Status.Phase := nrpQueued;
end;

procedure TApplicationResources.Start;
begin
  RequireOpen;

  if FStarted then
  begin
    Exit;
  end;
  FStarted := True;

  if FOptions.LoadingMode = nrlAutomatic then
  begin
    Reload;
  end;
end;

procedure TApplicationResources.Reload;
var
  LIndex: Integer;
begin
  RequireOpen;
  for LIndex := 0 to Length(FSlots) - 1 do
  begin

    if FDeclarations.Definition(FSlots[LIndex].Reference,
      FSlots[LIndex].Locale).Source.Kind = rskHosted then
    begin
      Queue(LIndex);
    end;
  end;
  Wake;
end;

procedure TApplicationResources.Reload(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef);
begin
  RequireOpen;

  if FDeclarations.Definition(AReference, ALocale).Source.Kind = rskEmbedded then
  begin
    Exit;
  end;
  Queue(IndexOf(AReference, ALocale));
  Wake;
end;

procedure TApplicationResources.Pump;
var
  LIndex: Integer;
  LRunning: Integer;
  LSlot: TResourceSlot;
  LResources: INyxResources;
  LCandidate: INyxResourceContext;
  LJob: TResourceJob;
  LError: TNyxText;
  LOrigin: TNyxResourceLoadOrigin;
begin
  FScheduler.RequireUI;
  FExecution := nil;

  if FStopped then
  begin
    Exit;
  end;
  FPumping := True;
  FWakeRequested := False;
  try
    { Each resource variant publishes atomically against the CURRENT catalog.
      Related authored changes still belong to Studio's grouped transaction. }
    for LIndex := 0 to Length(FSlots) - 1 do
    begin
      LSlot := FSlots[LIndex];

      if not LSlot.HaveResult then
      begin
        Continue;
      end;

      if not LSlot.Result.Succeeded then
      begin
        LSlot.Status.Phase := nrpFailed;
        LSlot.HaveResult := False;
        LSlot.Result := Default(TNyxResourceLoadResult);
        Continue;
      end;
      try
        LResources := FContext.Snapshot;
        LResources.Define(LSlot.Reference, LSlot.Locale, LSlot.Result.Definition);
        LCandidate := NewNyxResourceContext(LResources, FContext.Locale, FContext.Fallback);
        LOrigin := LSlot.Result.Origin;

        if not Publish(LCandidate, LError, LIndex, LOrigin) then
        begin
          Continue;
        end;
        LSlot.Status.NotificationError := LError;
      except
        on LException: Exception do
        begin
          LSlot.Status.Phase := nrpRejected;
          LSlot.Status.Error := LException.Message;
        end;
      end;
      LSlot.HaveResult := False;
      LSlot.Result := Default(TNyxResourceLoadResult);
    end;

    LRunning := 0;
    for LIndex := 0 to Length(FSlots) - 1 do
    begin

      if FSlots[LIndex].Job <> nil then
      begin
        Inc(LRunning);
      end;
    end;
    for LIndex := 0 to Length(FSlots) - 1 do
    begin
      LSlot := FSlots[LIndex];

      if (LSlot.Status.Phase <> nrpQueued) or (LRunning >= FOptions.Concurrency) then
      begin
        Continue;
      end;

      if FResolver = nil then
      begin
        LSlot.Status.Phase := nrpFailed;
        LSlot.Status.Error := 'No application resource resolver is configured';
        Continue;
      end;
      LJob := TResourceJob.Create;
      LSlot.Job := LJob;
      LSlot.JobObject := LJob;
      LJob.FOwner := Self;
      LJob.FIndex := LIndex;
      LSlot.Status.Phase := nrpLoading;
      try
        LJob.Start(FResolver, FDeclarations.Definition(LSlot.Reference,
          LSlot.Locale), FOptions.RequestOptions);
      except
        on LException: Exception do
        begin

          if LSlot.Job <> nil then
          begin
            LSlot.Job.Cancel;
          end;
          LSlot.Job := nil;
          LSlot.JobObject := nil;
          LSlot.Status.Phase := nrpFailed;
          LSlot.Status.Error := LException.Message;
        end;
      end;

      if LSlot.Job <> nil then
      begin
        Inc(LRunning);
      end;
    end;
  finally
    FPumping := False;
  end;
  { Completion requests a second pass. Busy admission alone does not spin:
    its receiver must Wake after finishing the render/input transaction. }

  if FWakeRequested then
  begin
    Wake;
  end;
end;

procedure TApplicationResources.Cancel;
begin
  RequireOpen;
  CancelPending;
end;

procedure TApplicationResources.CancelPending;
var
  LIndex: Integer;
  LSlot: TResourceSlot;
begin
  FScheduler.RequireUI;

  if FExecution <> nil then
  begin
    FExecution.Cancel;
    FExecution := nil;
  end;
  for LIndex := 0 to Length(FSlots) - 1 do
  begin
    LSlot := FSlots[LIndex];

    if LSlot.Job <> nil then
    begin
      LSlot.Job.Cancel;
      LSlot.Job := nil;
      LSlot.JobObject := nil;
    end;

    if LSlot.Status.Phase in [nrpQueued, nrpLoading, nrpWaiting] then
    begin
      LSlot.Status.Phase := nrpCancelled;
    end;
    LSlot.HaveResult := False;
    LSlot.Result := Default(TNyxResourceLoadResult);
  end;
end;

procedure TApplicationResources.Stop;
begin

  if FStopped then
  begin
    Exit;
  end;

  if FScheduler <> nil then
  begin
    CancelPending;
  end;
  FStopped := True;

  if FPortObject <> nil then
  begin
    FPortObject.Owner := nil;
  end;
  while Length(FTokens) > 0 do
  begin
    FTokenObjects[Length(FTokens) - 1].Disconnect;
  end;
  FResolver := nil;
end;

procedure TApplicationResources.Localize(const ALocale, AFallback: TNyxLocaleRef);
var
  LCandidate: INyxResourceContext;
  LError: TNyxText;
  LLease: INyxApplicationResources;
begin
  { A changed receiver may destroy its host and release its last ownership
    handle. Keep this synchronous dispatch alive until its weak callbacks retire. }
  LLease := Self;
  RequireOpen;
  LCandidate := NewNyxResourceContext(FContext.Snapshot, ALocale, AFallback);

  try

    if not Publish(LCandidate, LError) then
    begin
      raise ENyxResource.Create('Application resource receivers are busy');
    end;
  except
    on LException: Exception do
    begin
      LError := LException.Message;
      raise ENyxResource.Create(LError);
    end;
  end;

  if LError <> '' then
  begin
    raise ENyxResource.Create('Resource locale published; receiver failed: ' + LError);
  end;
end;

function NewNyxApplicationResources(const ADeclarations: INyxResources;
  const AScheduler: INyxScheduler; const AOptions: TNyxApplicationResourceOptions;
  const AResolver: INyxResourceResolver): INyxApplicationResources;
begin
  Result := TApplicationResources.Create(ADeclarations, AScheduler, AOptions, AResolver);
end;

end.
