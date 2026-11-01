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
program nyx_resource_logic_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.resources, nyx.resources.loader,
  nyx.resource.sources, nyx.resource.cache, nyx.application.resources,
  nyx.logic, nyx.logic.resources, nyx.logic.catalog, nyx.flow, nyx.flow.types,
  nyx.scheduler, nyx.resource.context, nyx.collections, nyx.collections.registry,
  nyx.collections.view, nyx.collections.view.types, nyx.state, nyx.publication
  {$ifdef PAS2JS}, Web{$endif};

type
  { Controllable UTC evidence makes freshness independent of machine time. }
  TClock = class(TInterfacedObject, INyxResourceClock)
  public
    Seconds: Double;
    function UTCSeconds: Double;
  end;

  { Ordinary request base disconnects its borrowed receiver before completion. }
  TRequest = class(TNyxResourceRequest)
  public
    procedure Deliver(const AResponse: TNyxResourceHTTPResult);
  end;

  { Controlled byte host owns at most one pending request. Inline and deferred
    modes exercise the same loader; no actual network effect is implied. }
  TTransport = class(TInterfacedObject, INyxResourceTransport)
  public
    Calls: Integer;
    Deferred: Boolean;
    Response: TNyxResourceHTTPResult;
    Pending: TRequest;
    Lease: INyxResourceRequest;
    function Request(const AURL: TNyxResourceURL;
      const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
    procedure Deliver;
  end;

  { Retains complete detached frames, never a control or resource owner. }
  TCompletion = class(TInterfacedObject, INyxLogicCompletion)
  public
    Count: Integer;
    Outputs: TNyxFlowValues;
    procedure Succeed(const AOutputs: TNyxFlowValues);
    procedure Fail(const AReason: TNyxText);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function TClock.UTCSeconds: Double;
begin
  Result := Seconds;
end;

procedure TRequest.Deliver(const AResponse: TNyxResourceHTTPResult);
begin
  Complete(AResponse);
end;

function TTransport.Request(const AURL: TNyxResourceURL;
  const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
var
  LRequest: TRequest;
begin
  Inc(Calls);
  LRequest := TRequest.Create(AReply);
  Result := LRequest;

  if Deferred then
  begin
    Pending := LRequest;
    Lease := Result;
  end
  else
  begin
    LRequest.Deliver(Response);
  end;
end;

procedure TTransport.Deliver;
begin
  Pending.Deliver(Response);
  Pending := nil;
  Lease := nil;
end;

procedure TCompletion.Succeed(const AOutputs: TNyxFlowValues);
begin
  Inc(Count);
  Outputs := AOutputs;
end;

procedure TCompletion.Fail(const AReason: TNyxText);
begin
  raise Exception.Create('Unexpected failed completion: ' + AReason);
end;

procedure CollectionScopes;
var
  LDefaults: INyxCollectionDefaults;
  LResources: INyxResourceContext;
  LApplication: INyxCollectionContext;
  LFirst: INyxCollectionContext;
  LSecond: INyxCollectionContext;
  LGlobal: INyxCollection;
  LOne: INyxCollection;
  LTwo: INyxCollection;
  LSpec: TNyxCollectionViewSpec;
  LRootPlan: INyxPreparedPublication;
  LFirstPlan: INyxPreparedPublication;
  LSecondPlan: INyxPreparedPublication;
begin
  LDefaults := NewNyxCollectionDefaults;
  LDefaults.Define(NyxCollection('scopes'), NyxCollectionSchema.Text(NyxTextField('name'), ''), []);
  LResources := NewNyxResourceContext(NewNyxResources, NyxDefaultLocale, NyxDefaultLocale);
  LApplication := NewNyxCollectionContext(LDefaults, LResources);
  LFirst := NewNyxEmbeddedCollectionContext(LDefaults, LResources, LApplication);
  LSecond := NewNyxEmbeddedCollectionContext(LDefaults, LResources, LApplication);
  LSpec := NyxCollectionView(NyxCollection('scopes')).Column(NyxTextField('name'), 'Name');
  LGlobal := LApplication.Collections.Collection(NyxCollection('scopes'));
  LGlobal.Append(NyxCollectionItem(NyxItem(NyxCollection('scopes'), 'public'))
    .WithValue(NyxTextField('name'), 'Shared application row'));
  Check((LFirst.Resolve(LSpec, '') = LGlobal) and (LSecond.Resolve(LSpec, '') = LGlobal),
    'independent embedded contexts share application scope without rematerializing defaults');
  LSpec := LSpec.Scoped(csInstance);
  LOne := LFirst.Resolve(LSpec, 'same-reusable-owner');
  LTwo := LSecond.Resolve(LSpec, 'same-reusable-owner');
  LOne.Append(NyxCollectionItem(NyxItem(NyxCollection('scopes'), 'private'))
    .WithValue(NyxTextField('name'), 'Only the first card'));
  Check((LOne <> LTwo) and (LOne.Snapshot.Count = 1) and (LTwo.Snapshot.Count = 0) and
    (LGlobal.Snapshot.Count = 1), 'identical descendant IDs never merge sibling canvas instance stores');
  Check(LFirst.Resolve(LSpec, 'same-reusable-owner') = LOne,
    'retained fragment context preserves its instance data on repeated admission');
  try
    LRootPlan := PrepareNyxCollectionContextResources(LApplication,
      LResources.Snapshot, NyxDefaultLocale, NyxDefaultLocale);
    LFirstPlan := PrepareNyxCollectionContextResources(LFirst,
      LResources.Snapshot, NyxDefaultLocale, NyxDefaultLocale);
    LSecondPlan := PrepareNyxCollectionContextResources(LSecond,
      LResources.Snapshot, NyxDefaultLocale, NyxDefaultLocale);
    PublishNyxGroup([LRootPlan, LFirstPlan, LSecondPlan]);
    Check((LOne.Snapshot.Count = 1) and (LTwo.Snapshot.Count = 0) and (LGlobal.Snapshot.Count = 1),
      'coordinated resource preparation retains shared/private stores without duplicate reservations');
  finally

    if LRootPlan <> nil then
    begin
      LRootPlan.Retire;
    end;

    if LFirstPlan <> nil then
    begin
      LFirstPlan.Retire;
    end;

    if LSecondPlan <> nil then
    begin
      LSecondPlan.Retire;
    end;
  end;
end;

procedure Run;
var
  LScheduler: INyxScheduler;
  LCatalog: INyxResources;
  LOwner: INyxApplicationResources;
  LOther: INyxApplicationResources;
  LSource: INyxResourceReadSource;
  LPlan: INyxResourceReadPlan;
  LClock: TClock;
  LClockLease: INyxResourceClock;
  LTransport: TTransport;
  LTransportLease: INyxResourceTransport;
  LResolver: INyxResourceResolver;
  LCompletion: TCompletion;
  LCompletionLease: INyxLogicCompletion;
  LOperation: INyxJSONResource;
  LInvocation: INyxLogicInvocation;
  LScope: INyxCancellationScope;
  LInputs: TNyxFlowValues;
  LBefore: Integer;
  LRejected: Boolean;
  LOptions: TNyxJSONResourceOptions;
begin
  LScheduler := NewNyxScheduler;
  try
    LClock := TClock.Create;
    LClockLease := LClock;
    LClock.Seconds := 1000;
    LTransport := TTransport.Create;
    LTransportLease := LTransport;
    LTransport.Response := Default(TNyxResourceHTTPResult);
    LTransport.Response.Status := 200;
    LTransport.Response.Bytes := NyxEncodeUTF8('{"rank":9007199254740993,"name":"A 🌟"}');
    LTransport.Response.Hints := NyxResourceCacheHeaders('', '');
    LResolver := NewNyxResourceResolver(LTransportLease, nil, nil, LClockLease);
    LCatalog := NewNyxResources;
    LCatalog.Define(NyxResourceRef('team'), NyxJSONResource('{"locale":"neutral"}'));
    LCatalog.Define(NyxResourceRef('team'), NyxLocale('en-US'),
      NyxJSONResource('{"locale":"English"}'));
    LCatalog.Define(NyxResourceRef('text'), NyxTextResource('Wrong kind'));
    LCatalog.Define(NyxResourceRef('hosted'),
      NyxHostedResource(nrkJSON, NyxResourceURL('https://example.invalid/team.json'))
        .Cache(NyxResourceCache.Memory.FreshFor(10).StaleFor(20))
        .Fallback(NyxJSONResource('{"offline":true}')));
    LOwner := NewNyxApplicationResources(LCatalog, LScheduler,
      NyxApplicationResourceOptions.Loading(nrlOnDemand), LResolver);
    LSource := NyxApplicationResourceReads(LOwner);
    LCompletion := TCompletion.Create;
    LCompletionLease := LCompletion;
    LScope := NewNyxCancellationScope;
    LInputs := NyxFlowValues.WithValue(NyxJSONResourcePort(njrpTrigger), NyxFlowSignal);
    LOptions := NyxJSONResourceOptions(NyxResourceRef('team')).WholeRequest(1234);
    LOperation := NewNyxJSONResource(LOptions, LSource);
    Check(TNyxJSONResourceOptions.FromData(LOptions.ToData).ToData.ToJSON =
      LOptions.ToData.ToJSON, 'typed options retain nominal identity/deadline');
    Check(NewNyxLogicCatalog.Matches(NewNyxLogicCatalog.Count - 1, 'hosted resource'),
      'resource operation is discoverable by intent');
    LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, LScope, LCompletionLease);
    Check((LCompletion.Count = 1) and
      (LCompletion.Outputs.Value(NyxJSONResourcePort(njrpJSON)).AsJSON.Field('locale').AsText = 'neutral') and
      (LTransport.Calls = 0), 'embedded inline read uses no host');
    LOwner.Localize(NyxLocale('en-US'), NyxDefaultLocale);
    LPlan := LSource.CaptureRead(NyxResourceRef('team'));
    LOwner.Localize(NyxLocale('fr-FR'), NyxDefaultLocale);
    Check(LPlan.Definition.Data.Field('locale').AsText = 'English',
      'detached plan retains the accepted locale at capture');
    LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, LScope, LCompletionLease);
    Check(LCompletion.Outputs.Value(NyxJSONResourcePort(njrpJSON)).AsJSON.Field('locale').AsText = 'neutral',
      'later invocation resolves the current locale and neutral fallback');
    LOperation := NewNyxJSONResource(NyxJSONResourceOptions(NyxResourceRef('hosted')), LSource);
    LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, LScope, LCompletionLease);
    Check((LTransport.Calls = 1) and
      (LCompletion.Outputs.Value(NyxJSONResourcePort(njrpJSON)).AsJSON.Field('rank').ToJSON = '9007199254740993') and
      (LCompletion.Outputs.Value(NyxJSONResourcePort(njrpJSON)).AsJSON.Field('name').AsText = TNyxText('A 🌟')),
      'hosted read preserves exact integers and supplementary Unicode');
    Check(LCompletion.Outputs.Value(NyxJSONResourcePort(njrpCacheWrite)).AsInteger = Ord(rcuMemory),
      'resource write evidence uses the existing private cache');
    LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, LScope, LCompletionLease);
    Check((LTransport.Calls = 1) and
      (LCompletion.Outputs.Value(NyxJSONResourcePort(njrpOrigin)).AsInteger = Ord(rloFreshCache)),
      'a second invocation uses the original hosted declaration and shared freshness policy');
    LClock.Seconds := 1011;
    LTransport.Response.Error := 'Offline';
    LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, LScope, LCompletionLease);
    Check((LTransport.Calls = 2) and
      LCompletion.Outputs.Value(NyxJSONResourcePort(njrpSucceeded)).AsBoolean and
      (LCompletion.Outputs.Value(NyxJSONResourcePort(njrpOrigin)).AsInteger = Ord(rloStaleCache)) and
      (LCompletion.Outputs.Value(NyxJSONResourcePort(njrpError)).AsText = 'Offline'),
      'stale success retains the loading error for connected diagnostics');
    LClock.Seconds := 1031;
    LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, LScope, LCompletionLease);
    Check(LCompletion.Outputs.Value(NyxJSONResourcePort(njrpOrigin)).AsInteger = Ord(rloFallback),
      'expired cache uses the authored fallback');
    LOther := NewNyxApplicationResources(LCatalog, LScheduler,
      NyxApplicationResourceOptions.Loading(nrlOnDemand));
    LOperation := NewNyxJSONResource(LOperation.Options, NyxApplicationResourceReads(LOther));
    LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, LScope, LCompletionLease);
    Check(not LCompletion.Outputs.Value(NyxJSONResourcePort(njrpSucceeded)).AsBoolean and
      (LCompletion.Outputs.Value(NyxJSONResourcePort(njrpJSON)).AsJSON.Kind = ndNull),
      'absent hosted authority is a usable failed frame without fabricating fallback access');
    LOther.Stop;
    LOperation := NewNyxJSONResource(LOperation.Options, LSource);
    LTransport.Deferred := True;
    LTransport.Response.Error := '';
    LBefore := LCompletion.Count;
    LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, LScope, LCompletionLease);
    LInvocation.Cancel;
    LTransport.Deliver;
    Check(LCompletion.Count = LBefore, 'explicit cancellation revokes a late loader reply');
    LInvocation := LOperation.BeginInvoke(LInputs, LScheduler, LScope, LCompletionLease);
    LScope.Cancel;
    LTransport.Deliver;
    Check(LCompletion.Count = LBefore, 'scope cancellation suppresses output after host completion');
    LScope := NewNyxCancellationScope;
    LRejected := False;
    try
      LInvocation := LOperation.BeginInvoke(LInputs.WithValue(
        NyxJSONResourcePort(njrpReference), NyxFlowText('text')),
        LScheduler, LScope, LCompletionLease);
    except
      on E: ENyxFlow do LRejected := True;
    end;
    Check(LRejected, 'wrong declared kind refuses before transport');
    LOwner.Stop;
    LRejected := False;
    try
      LPlan := LSource.CaptureRead(NyxResourceRef('team'));
    except
      on E: ENyxResource do LRejected := True;
    end;
    Check(LRejected, 'stopped source refuses new capture');
    LInvocation := nil;
    LOperation := nil;
    LCompletionLease := nil;
  finally
    LScheduler.Shutdown;
  end;
end;

begin
  try
    CollectionScopes;
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-resource-logic', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' resource Logic checks';
    {$else}
    WriteLn('PASS ', GChecks, ' resource Logic checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-resource-logic', 'failed');
      document.body.textContent := E.Message;
      {$else}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
