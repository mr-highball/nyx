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


unit nyx.resources.loader;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.resources,
  nyx.resource.sources, nyx.resource.cache;

type
  { Immutable transport options. The deadline includes queuing, DNS, handshake,
    headers and every body chunk. Redirects and ambient credentials are refused
    by the built-in public-file adapters; custom transports implement this same
    byte-only contract without borrowing the document or a UI receiver. }
  TNyxResourceLoadOptions = record
  private
    FDeadlineMS: Integer;
  public
    function WholeRequest(AMilliseconds: Integer): TNyxResourceLoadOptions;
    procedure Validate;
    property DeadlineMS: Integer read FDeadlineMS;
  end;

  { Owned reply bytes at the network boundary. Status zero means transport
    failure; Error explains it. A successful HTTP reply still needs exact-kind,
    byte and selector admission before it can replace a mounted resource. }
  TNyxResourceHTTPResult = record
    Status: Integer;
    Bytes: TNyxBytes;
    Hints: TNyxResourceCacheHints;
    Error: TNyxText;
  end;
  TNyxResourceHTTPReply = procedure(const AResult: TNyxResourceHTTPResult) of object;
  INyxResourceRequest = interface
    ['{36FAAC0B-D7A3-4A87-BF6C-D358D49601A1}']
    procedure Cancel;
  end;

  { Request and cancellation are UI-thread operations. Reply occurs once on
    that thread and may complete inline. A retained job never retains the
    receiver. Cancel disconnects it before platform I/O finishes retiring. }
  INyxResourceTransport = interface
    ['{89B18AE9-C1E4-4992-AC10-D1A69138A45A}']
    function Request(const AURL: TNyxResourceURL;
      const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
  end;

  { Platform adapter base with callback retirement before invocation. Pending
    subclasses own a temporary operation lease until their platform work ends.
    Receiver exceptions never become a second transport completion. }
  TNyxResourceRequest = class(TInterfacedObject, INyxResourceRequest)
  private
    FReply: TNyxResourceHTTPReply;
  protected
    function Active: Boolean;
    procedure Complete(const AResult: TNyxResourceHTTPResult);
  public
    constructor Create(AReply: TNyxResourceHTTPReply);
    procedure Cancel; virtual;
  end;

  { Replaceable UTC clock, permitting deterministic freshness qualification.
    Cache envelopes retain absolute seconds; backwards clocks refuse reuse. }
  INyxResourceClock = interface
    ['{1E681791-3462-429A-962F-836AF45B222F}']
    function UTCSeconds: Double;
  end;

  TNyxResourceLoadOrigin = (rloFailed, rloEmbedded, rloNetwork, rloFreshCache,
    rloStaleCache, rloFallback);

  { Evidence of a successful Nyx-managed cache operation, rather than the
    caller's requested tier. None includes bypass, refused server storage and
    unsuccessful writes. Persistent identifies the injected persistent provider;
    it does not promise that a browser's separate HTTP cache was involved. }
  TNyxResourceCacheUse = (rcuNone, rcuMemory, rcuPersistent);

  { Copied result data contains no project, control, store or managed interface
    in a record. Definition returns an independently admitted immutable value.
    Fallback/stale success retains the original loading error for diagnostics;
    cache warnings never turn successful loaded content into a failed load. }
  TNyxResourceLoadResult = record
  private
    FOrigin: TNyxResourceLoadOrigin;
    FData: TNyxDataValue;
    FError: TNyxText;
    FCacheWarning: TNyxText;
    FCacheRead: TNyxResourceCacheUse;
    FCacheWrite: TNyxResourceCacheUse;
  public
    function Succeeded: Boolean;
    function Definition: INyxResourceDefinition;
    property Origin: TNyxResourceLoadOrigin read FOrigin;
    property Error: TNyxText read FError;
    property CacheWarning: TNyxText read FCacheWarning;
    property CacheRead: TNyxResourceCacheUse read FCacheRead;
    property CacheWrite: TNyxResourceCacheUse read FCacheWrite;
  end;
  TNyxResourceLoadReply = procedure(const AResult: TNyxResourceLoadResult) of object;

  INyxResourceLoad = interface
    ['{DFE92AFE-C0C1-438E-AFD8-76DC00C8F7E6}']
    procedure Cancel;
  end;

  { One resolver owns its private providers, never an authored catalog or tree.
    Load normalizes an immutable declaration, checks request policy on every
    hit/store and returns resolved embedded bytes with caller metadata. Failed
    persistent storage explicitly falls back to memory and reports a warning.
    Stale cache is used only after loading failure inside the caller's window;
    otherwise explicit embedded fallback precedes a failed result. The receiver
    owns atomic control publication and must cancel on navigation/destruction. }
  INyxResourceResolver = interface
    ['{326F7314-816C-49F5-B52C-FD26AD1325AE}']
    function Load(const ADefinition: INyxResourceDefinition;
      const AOptions: TNyxResourceLoadOptions;
      AReply: TNyxResourceLoadReply): INyxResourceLoad;
  end;

function NyxResourceLoadOptions: TNyxResourceLoadOptions;
function NewNyxResourceClock: INyxResourceClock;
function NewNyxResourceResolver(const ATransport: INyxResourceTransport;
  const AMemory: INyxResourceCacheStorage = nil;
  const APersistent: INyxResourceCacheStorage = nil;
  const AClock: INyxResourceClock = nil): INyxResourceResolver;

implementation

uses
  {$ifdef PAS2JS}JS{$else}DateUtils{$endif};

type
  TResourceClock = class(TInterfacedObject, INyxResourceClock)
    function UTCSeconds: Double;
  end;
  TResourceLoadStage = (rlsNew, rlsReading, rlsFetching, rlsWriting, rlsRetired);
  TResourceLoad = class(TInterfacedObject, INyxResourceLoad)
  private
    FDefinition: INyxResourceDefinition;
    FTransport: INyxResourceTransport;
    FMemory: INyxResourceCacheStorage;
    FPersistent: INyxResourceCacheStorage;
    FCache: INyxResourceCacheStorage;
    FWriteCache: INyxResourceCacheStorage;
    FClock: INyxResourceClock;
    FOptions: TNyxResourceLoadOptions;
    FReply: TNyxResourceLoadReply;
    FRequest: INyxResourceRequest;
    FCacheJob: INyxResourceCacheJob;
    FLease: INyxResourceLoad;
    FEntry: TNyxResourceCacheEntry;
    FHaveEntry: Boolean;
    FEntryUse: TNyxResourceCacheUse;
    FStoredUse: TNyxResourceCacheUse;
    FResolved: INyxResourceDefinition;
    FWarning: TNyxText;
    FStage: TResourceLoadStage;
    procedure ReadCache;
    procedure CacheRead(AFound: Boolean; const AEntry: TNyxResourceCacheEntry;
      const AError: TNyxText);
    procedure Fetch;
    procedure Received(const AResult: TNyxResourceHTTPResult);
    procedure Store;
    procedure Stored(ASuccess: Boolean; const AError: TNyxText);
    procedure Failed(const AError: TNyxText);
    procedure Finish(AOrigin: TNyxResourceLoadOrigin;
      const ADefinition: INyxResourceDefinition; const AError: TNyxText);
  public
    procedure Start;
    procedure Cancel;
  end;
  TResourceResolver = class(TInterfacedObject, INyxResourceResolver)
  private
    FTransport: INyxResourceTransport;
    FMemory: INyxResourceCacheStorage;
    FPersistent: INyxResourceCacheStorage;
    FClock: INyxResourceClock;
  public
    constructor Create(const ATransport: INyxResourceTransport;
      const AMemory, APersistent: INyxResourceCacheStorage; const AClock: INyxResourceClock);
    function Load(const ADefinition: INyxResourceDefinition;
      const AOptions: TNyxResourceLoadOptions; AReply: TNyxResourceLoadReply): INyxResourceLoad;
  end;

function NyxResourceLoadOptions: TNyxResourceLoadOptions;
begin
  Result.FDeadlineMS := 15000;
end;

function TNyxResourceLoadOptions.WholeRequest(AMilliseconds: Integer): TNyxResourceLoadOptions;
var
  LOptions: TNyxResourceLoadOptions;
begin
  LOptions := Self;
  LOptions.FDeadlineMS := AMilliseconds;
  LOptions.Validate;
  Result := LOptions;
end;

procedure TNyxResourceLoadOptions.Validate;
begin

  if (FDeadlineMS < 1) or (FDeadlineMS > 120000) then
  begin
    raise ENyxResource.Create('Resource request deadline requires 1..120000 milliseconds');
  end;
end;

constructor TNyxResourceRequest.Create(AReply: TNyxResourceHTTPReply);
begin
  inherited Create;
  FReply := AReply;
end;

procedure TNyxResourceRequest.Cancel;
begin
  FReply := nil;
end;

function TNyxResourceRequest.Active: Boolean;
begin
  Result := Assigned(FReply);
end;

procedure TNyxResourceRequest.Complete(const AResult: TNyxResourceHTTPResult);
var
  LReply: TNyxResourceHTTPReply;
  LLease: INyxResourceRequest;
begin
  LLease := Self;
  LReply := FReply;
  FReply := nil;

  if Assigned(LReply) then
  begin
    LReply(AResult);
  end;
  LLease := nil;
end;

function TResourceClock.UTCSeconds: Double;
begin
  {$ifdef PAS2JS}
  Result := TJSDate.now / 1000;
  {$else}
  Result := DateTimeToUnix(Now, False);
  {$endif}
end;

function NewNyxResourceClock: INyxResourceClock;
begin
  Result := TResourceClock.Create;
end;

function TNyxResourceLoadResult.Succeeded: Boolean;
begin
  Result := (FOrigin <> rloFailed) and FData.Defined;
end;

function TNyxResourceLoadResult.Definition: INyxResourceDefinition;
begin

  if not Succeeded or not FData.Defined then
  begin
    raise ENyxResource.Create('A failed resource load has no admitted definition');
  end;
  Result := NyxResourceFromData(FData);
end;

procedure TResourceLoad.Cancel;
var
  LLease: INyxResourceLoad;
begin
  LLease := Self;
  FStage := rlsRetired;
  FReply := nil;

  if FRequest <> nil then
  begin
    FRequest.Cancel;
  end;

  if FCacheJob <> nil then
  begin
    FCacheJob.Cancel;
  end;
  FRequest := nil;
  FCacheJob := nil;
  FLease := nil;
  LLease := nil;
end;

procedure TResourceLoad.Finish(AOrigin: TNyxResourceLoadOrigin;
  const ADefinition: INyxResourceDefinition; const AError: TNyxText);
var
  LResult: TNyxResourceLoadResult;
  LReply: TNyxResourceLoadReply;
  LLease: INyxResourceLoad;
begin

  if FStage = rlsRetired then
  begin
    Exit;
  end;
  LResult := Default(TNyxResourceLoadResult);
  LResult.FOrigin := AOrigin;
  LResult.FError := AError;
  LResult.FCacheWarning := FWarning;
  LResult.FCacheWrite := FStoredUse;

  if AOrigin in [rloFreshCache, rloStaleCache] then
  begin
    LResult.FCacheRead := FEntryUse;
  end;

  if ADefinition <> nil then
  begin
    { Discovery belongs to the current authored definition, including cache hits
      and fallback results. Transport/cache metadata cannot replace its labels. }
    LResult.FData := NyxResourceDiscovery(ADefinition.Describe(
      FDefinition.Title, FDefinition.Description))
      .WithLabels(NyxResourceLabelsOf(FDefinition)).ToData;
  end;
  LLease := Self;
  LReply := FReply;
  Cancel;

  if Assigned(LReply) then
  begin
    LReply(LResult);
  end;
  LLease := nil;
end;

procedure TResourceLoad.Start;
begin
  FLease := Self;

  if FDefinition.Source.Kind = rskEmbedded then
  begin
    Finish(rloEmbedded, FDefinition, '');
    Exit;
  end;

  if FDefinition.Source.CachePolicy.Mode = rcmBypass then
  begin
    Fetch;
    Exit;
  end;
  FCache := FMemory;

  if FDefinition.Source.CachePolicy.Mode = rcmPersistent then
  begin
    FCache := FPersistent;

    if FCache = nil then
    begin
      FCache := FMemory;
      FWarning := 'Persistent resource storage unavailable; using memory';
    end;
  end;
  FWriteCache := FCache;
  ReadCache;
end;

procedure TResourceLoad.ReadCache;
var
  LJob: INyxResourceCacheJob;
begin
  FStage := rlsReading;
  try
    LJob := FCache.Read(FDefinition.Source.URL, FDefinition.Kind, CacheRead);
  except
    on LException: Exception do
    begin

      if FStage <> rlsReading then
      begin
        raise;
      end;
      CacheRead(False, Default(TNyxResourceCacheEntry), LException.Message);
      Exit;
    end;
  end;
  { Inline completion may already have advanced/finished this operation. Do not
    retain an obsolete token over the next pending transport/cache stage. }

  if FStage = rlsReading then
  begin
    FCacheJob := LJob;
  end;
end;

procedure TResourceLoad.CacheRead(AFound: Boolean;
  const AEntry: TNyxResourceCacheEntry; const AError: TNyxText);
var
  LState: TNyxResourceCacheState;
  LDefinition: INyxResourceDefinition;
begin

  if FStage <> rlsReading then
  begin
    Exit;
  end;
  FCacheJob := nil;

  if AError <> '' then
  begin
    FWarning := AError;
    { A failed read can be a damaged envelope rather than unavailable storage.
      Retain the original provider for one policy-approved write after a valid
      network reply. This can replace that owned entry atomically. Stored still
      falls back to memory if the actual write fails; no error-text parsing or
      repeated persistent retry is needed. A fresh memory hit needs no write. }
  end;

  if AFound and (AError = '') then
  begin
    LState := rcsMiss;
    try
      FEntry := TNyxResourceCacheEntry.FromData(AEntry.ToData);

      if (FEntry.URL.Address <> FDefinition.Source.URL.Address) or
        (FEntry.Kind <> FDefinition.Kind) then
      begin
        raise ENyxResource.Create('Cache reply belongs to another resource');
      end;
      FHaveEntry := True;
      FEntryUse := rcuPersistent;

      if FCache = FMemory then
      begin
        FEntryUse := rcuMemory;
      end;
      LState := FEntry.StateAt(FDefinition.Source.CachePolicy, FClock.UTCSeconds);

      if LState = rcsFresh then
      begin
        LDefinition := FEntry.Definition;
      end;
    except
      on LException: Exception do
      begin
        FHaveEntry := False;
        LState := rcsMiss;
        FWarning := LException.Message;
      end;
    end;

    if LState = rcsFresh then
    begin
      Finish(rloFreshCache, LDefinition, '');
      Exit;
    end;
  end;

  if FCache <> FMemory then
  begin
    { A previous failed persistent write may have populated memory. Query it
      before fetching, while retaining the persistent provider for new writes. }
    FCache := FMemory;
    ReadCache;
    Exit;
  end;
  Fetch;
end;

procedure TResourceLoad.Fetch;
var
  LJob: INyxResourceRequest;
begin
  FStage := rlsFetching;
  try
    LJob := FTransport.Request(FDefinition.Source.URL, FOptions,
      FDefinition.Source.CachePolicy.ByteLimit, Received);
  except
    on LException: Exception do
    begin

      if FStage <> rlsFetching then
      begin
        raise;
      end;
      Failed(LException.Message);
      Exit;
    end;
  end;

  if FStage = rlsFetching then
  begin
    FRequest := LJob;
  end;
end;

procedure TResourceLoad.Received(const AResult: TNyxResourceHTTPResult);
var
  LError: TNyxText;
begin

  if FStage <> rlsFetching then
  begin
    Exit;
  end;
  FRequest := nil;
  LError := AResult.Error;

  if (LError = '') and (AResult.Status <> 200) and (AResult.Status <> 204) then
  begin
    LError := 'Resource host returned HTTP ' + TNyxText(IntToStr(AResult.Status));
  end;

  if LError = '' then
  begin
    try

      if Length(AResult.Bytes) > FDefinition.Source.CachePolicy.ByteLimit then
      begin
        raise ENyxResource.Create('Resource reply exceeds the caller byte budget');
      end;
      FResolved := NyxResourceFromBytes(FDefinition.Kind, AResult.Bytes);

      if FDefinition.Source.CachePolicy.Mode <> rcmBypass then
      begin
        FEntry := NyxResourceCacheEntry(FDefinition.Source.URL, FResolved,
          FClock.UTCSeconds, AResult.Hints);
      end;
    except
      on LException: Exception do
      begin
        LError := LException.Message;
      end;
    end;
  end;

  if LError <> '' then
  begin
    Failed(LError);
    Exit;
  end;
  Store;
end;

procedure TResourceLoad.Store;
var
  LJob: INyxResourceCacheJob;
begin

  if not FEntry.CanStore(FDefinition.Source.CachePolicy) then
  begin
    Finish(rloNetwork, FResolved, '');
    Exit;
  end;
  FCache := FWriteCache;
  FStage := rlsWriting;
  try
    LJob := FCache.Write(FEntry, Stored);
  except
    on LException: Exception do
    begin

      if FStage <> rlsWriting then
      begin
        raise;
      end;
      Stored(False, LException.Message);
      Exit;
    end;
  end;

  if FStage = rlsWriting then
  begin
    FCacheJob := LJob;
  end;
end;

procedure TResourceLoad.Stored(ASuccess: Boolean; const AError: TNyxText);
begin

  if FStage <> rlsWriting then
  begin
    Exit;
  end;
  FCacheJob := nil;

  if not ASuccess then
  begin
    FWarning := AError;

    if FCache <> FMemory then
    begin
      FWriteCache := FMemory;
      Store;
      Exit;
    end;
  end;

  if ASuccess then
  begin
    FStoredUse := rcuPersistent;

    if FCache = FMemory then
    begin
      FStoredUse := rcuMemory;
    end;
  end;
  Finish(rloNetwork, FResolved, '');
end;

procedure TResourceLoad.Failed(const AError: TNyxText);
begin

  if FHaveEntry and
    (FEntry.StateAt(FDefinition.Source.CachePolicy, FClock.UTCSeconds) = rcsStale) then
  begin
    Finish(rloStaleCache, FEntry.Definition, AError);
    Exit;
  end;

  if FDefinition.FallbackDefinition <> nil then
  begin
    Finish(rloFallback, FDefinition.FallbackDefinition, AError);
    Exit;
  end;
  Finish(rloFailed, nil, AError);
end;

constructor TResourceResolver.Create(const ATransport: INyxResourceTransport;
  const AMemory, APersistent: INyxResourceCacheStorage; const AClock: INyxResourceClock);
begin
  inherited Create;

  if ATransport = nil then
  begin
    raise ENyxResource.Create('Resource resolver requires a byte transport');
  end;
  FTransport := ATransport;
  FMemory := AMemory;

  if FMemory = nil then
  begin
    FMemory := NewNyxMemoryResourceCache;
  end;
  FPersistent := APersistent;
  FClock := AClock;

  if FClock = nil then
  begin
    FClock := NewNyxResourceClock;
  end;
end;

function TResourceResolver.Load(const ADefinition: INyxResourceDefinition;
  const AOptions: TNyxResourceLoadOptions; AReply: TNyxResourceLoadReply): INyxResourceLoad;
var
  LDefinition: INyxResourceDefinition;
  LLoad: TResourceLoad;
begin
  AOptions.Validate;

  if ADefinition = nil then
  begin
    raise ENyxResource.Create('Resource resolver requires an immutable definition');
  end;
  LDefinition := NyxResourceFromData(ADefinition.ToData);
  LLoad := TResourceLoad.Create;
  Result := LLoad;
  LLoad.FDefinition := LDefinition;
  LLoad.FOptions := AOptions;
  LLoad.FReply := AReply;
  LLoad.FTransport := FTransport;
  LLoad.FMemory := FMemory;
  LLoad.FPersistent := FPersistent;
  LLoad.FClock := FClock;
  try
    LLoad.Start;
  except
    LLoad.Cancel;
    raise;
  end;
end;

function NewNyxResourceResolver(const ATransport: INyxResourceTransport;
  const AMemory, APersistent: INyxResourceCacheStorage;
  const AClock: INyxResourceClock): INyxResourceResolver;
begin
  Result := TResourceResolver.Create(ATransport, AMemory, APersistent, AClock);
end;

end.
