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


program nyx_flow_routes_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.bytes, nyx.model, nyx.state,
  nyx.flow.routes, nyx.flow.route.model, nyx.flow.declarations, nyx.flow.types, nyx.flow,
  nyx.logic, nyx.schema, nyx.codec, nyx.codegen, nyx.source, nyx.studio.projects,
  nyx.studio.session, nyx.studio.edits, nyx.events, nyx.behavior, nyx.scheduler, nyx.controls,
  nyx.callbacks, nyx.test.flow.routes, nyx.test.logic.fixture,
  nyx.flow.routing, nyx.application.context, nyx.composition, nyx.collections,
  nyx.collections.registry, nyx.collections.view.types, nyx.master.detail, nyx.resources,
  nyx.publication, nyx.contract
  {$ifdef PAS2JS}, JS, Web{$else}, Classes{$endif};

type
  { Ordinary observer exercises ordered public dispatch without retaining a
    document or route owner. Its only mutation adds another explicit observer. }
  TObserver = class(TNyxEventObserver)
  public
    Mark: TNyxText;
    Expected: Integer;
    AddDuringDispatch: Boolean;
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
  end;
  { Plain application callback records the existing observer phase ordering. }
  TCallback = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { An explicit resolver records authority use and refuses a fixture-only kind.
    Preparing routes must never call it; invocation reports its exact refusal. }
  TDeferredResolver = class(TInterfacedObject, INyxLogicResolver)
  public
    Calls: Integer;
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: TNyxDataValue): INyxLogicOperation;
  end;

  { Public host owns a real source document, realized tree and independent state.
    Its ordinary weak context exposes the prospective event generation. It can
    retire source trees after preparation without invalidating copied routes. }
  TPreparedRouteHost = class
  private
    FOwnEvents: Boolean;
    function ResolveState: TNyxState;
    function ResolveCollections: INyxCollections;
    function ResolveGeneration: INyxCancellationScope;
  public
    Document: TNyxDocument;
    Root: TNyxNode;
    State: TNyxState;
    Collections: INyxCollections;
    Events: INyxEvents;
    Scheduler: INyxScheduler;
    View: INyxPreparedEventView;
    Generation: INyxCancellationScope;
    ContextLease: INyxApplicationContextLease;
    Binding: INyxFlowRouteBinding;
    Plan: INyxPreparedPublication;
    Resolver: INyxLogicResolver;
    ResolverProbe: TDeferredResolver;
    DetailID: TNyxText;
    EditID: TNyxText;
    EditSourceID: TNyxText;
    GenerationReads: Integer;
    CancelOnGenerationRead: Boolean;
    constructor Create(const AEvents: INyxEvents = nil);
    destructor Destroy; override;
    procedure Prepare;
    procedure ReleaseSources;
    procedure Change(const AText: TNyxText);
    procedure Edit(const AText: TNyxText; const ADetails: TNyxDataValue);
  end;

  { A deliberately older prepared-view adapter forwards the real event owner
    but omits optional readiness. Admission must refuse before staging a slot. }
  TViewWithoutReadiness = class(TInterfacedObject, INyxPreparedEventView)
  public
    Inner: INyxPreparedEventView;
    Staged: Integer;
    function GetGeneration: INyxCancellationScope;
    function PrepareCallback(const ATarget: TNyxEventTarget; ATrigger: TNyxTrigger;
      const ACallback: INyxEventCallback;
      const AName: TNyxEventRef): INyxEventSubscription;
    function PrepareObservation(const ATarget: TNyxEventTarget; ATrigger: TNyxTrigger;
      const AObserver: INyxEventObserver; APhase: TNyxObservationPhase;
      const AName: TNyxEventRef): INyxEventSubscription;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { Later group validation revokes a real candidate binding or its future token.
    A notification-only refusal instead proves that installed data stays live. }
  TRoutePublicationProbe = class(TInterfacedObject, INyxPreparedPublication)
  public
    Host: TPreparedRouteHost;
    Events: INyxEvents;
    Binding: INyxFlowRouteBinding;
    Generation: INyxCancellationScope;
    Reads: Integer;
    FailNotify: Boolean;
    Installed: Boolean;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

var
  GChecks: Integer;
  GOrder: TNyxText;
  GObservation: INyxEventObservation;
  GAdded: INyxEventSubscription;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Flow routes: ' + AMessage);
  end;
  Inc(GChecks);
end;

procedure TObserver.Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer);
var
  LObserver: TObserver;
begin
  Check(ACallbackCount = Expected, 'observer sees exact ordinary callback count');
  GOrder := GOrder + Mark;

  if AddDuringDispatch then
  begin
    AddDuringDispatch := False;
    LObserver := TObserver.Create;
    LObserver.Mark := 'N';
    LObserver.Expected := 1;
    GAdded := GObservation.Observe(NyxControlEvents('button'), ntClick,
      LObserver, nopAfterDispatch, NyxEvent(''));
  end;
end;

procedure TCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  GOrder := GOrder + 'C';
end;

procedure EventObservers;
var
  LEvents: INyxEvents;
  LBefore: TObserver;
  LAfter: TObserver;
  LFirst: INyxEventSubscription;
  LSecond: INyxEventSubscription;
  LCallback: INyxEventSubscription;
  LInfo: TNyxEventInfo;
  LRuns: TNyxExecutions;
begin
  LEvents := NewNyxEvents;
  Check(Supports(LEvents, INyxEventObservation, GObservation), 'optional observation capability');
  LBefore := TObserver.Create;
  LBefore.Mark := 'B';
  LBefore.Expected := 1;
  LBefore.AddDuringDispatch := True;
  LAfter := TObserver.Create;
  LAfter.Mark := 'A';
  LAfter.Expected := 1;
  LFirst := GObservation.Observe(NyxControlEvents('button'), ntClick,
    LBefore, nopBeforeCallbacks, NyxEvent(''));
  LSecond := GObservation.Observe(NyxControlEvents('button'), ntClick,
    LAfter, nopAfterDispatch, NyxEvent(''));
  LCallback := LEvents.On(NyxControlEvents('button'), ntClick).Subscribe(TCallback.Create);
  LInfo := Default(TNyxEventInfo);
  LInfo.Value := NyxNull;
  LInfo.Details := NyxNull;
  LInfo.Trigger := ntClick;
  LInfo.Name := NyxEvent('click');
  LInfo.OriginID := 'button';
  LInfo.SourceID := 'button';
  LInfo.TargetID := 'button';
  GOrder := '';
  LRuns := LEvents.Dispatch(LInfo, 'button', 'button');
  Check((GOrder = 'BCA') and (Length(LRuns) = 3),
    'all phases snapshot before callbacks; new observer waits for next dispatch');
  GOrder := '';
  LEvents.Dispatch(LInfo, 'button', 'button');
  Check(GOrder = 'BCAN', 'new observer joins next dispatch in stable phase order');
  LFirst.Cancel;
  LSecond.Cancel;
  GAdded.Cancel;
  GOrder := '';
  LEvents.Dispatch(LInfo, 'button', 'button');
  Check(GOrder = 'C', 'cancelled observers never enter later dispatch');
  LCallback.Cancel;
  Check(not LEvents.HasSubscribers(ntClick), 'cancelled observation groups prune completely');
  LEvents.Close;
  GObservation := nil;
  GAdded := nil;
end;

function TDeferredResolver.Resolve(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
begin
  Inc(Calls);

  if AKind.Name = 'deferred-fixture' then
  begin
    raise ENyxFlow.Create('Explicit fixture resolver refused executable authority');
  end;
  Result := NyxLogicFromData(AKind, AConfiguration);
end;

constructor TPreparedRouteHost.Create(const AEvents: INyxEvents);
var
  LDetail: INyxMasterDetail;
  LInstance: INyxComponent;
  LCreator: INyxButton;
  LDescriptor: TNyxLogicDescriptor;
  LProjection: INyxMasterDetail;
  LPreparation: INyxEventViewPreparation;
begin
  inherited Create;
  Document := RoutedLogicDocument;
  Document.Collections.Define(NyxCollection('people'), NyxCollectionSchema.
    Text(NyxTextField('name'), '').Text(NyxTextField('summary'), ''), []);
  LDetail := NewNyxMasterDetail('directory');
  LDetail.MasterList.Binds.Collection(NyxCollectionView(NyxCollection('people')).
    Column(NyxTextField('name'), 'People').Scoped(csInstance)).Done;
  SetNyxMasterDetailIntent(LDetail, TNyxMasterDetailIntent.Configured(
    NyxMasterDetailOptions(NyxTextField('name'), NyxTextField('summary'))));
  LDetail.DetailEditButton.Configure.Value('scalar source').Done;
  LDetail.DetailEditButton.Contract.On(ntClick, NyxOriginValue, NyxTextDomain);
  Document.Flows.Define(NyxFlowRef('Captured record'), NewNyxFlowGraph.
    WithStep(NyxFlowStep('format'), NewNyxJSONFormat(njlCompact)).
    WithStep(NyxFlowStep('join'), NewNyxTextJoin('|')).
    Connect(LogicEndpoint('format', nlpText), LogicEndpoint('join', nlpRight)).Build);
  SetNyxFlowRoutes(LDetail, NewNyxFlowRoutes.WithRoute(NyxNamedFlowRoute(
    NyxFlowRouteID('Captured channels'), NyxFlowRef('Captured record'),
    NyxSemantic(nseEdit), nfcmAfterDispatch).
    InputEventDetails(LogicEndpoint('format', nlpJSON), NyxResourcePath).
    InputEventValue(LogicEndpoint('join', nlpLeft), nftText).
    OutputState(LogicEndpoint('join', nlpText), NyxTextState('guard')).
    Using(neSequential)));
  Document.AddComponent(LDetail);
  LInstance := NewNyxComponent('first-directory');
  LInstance.Reference := NyxComponent('directory');
  Document.Pages[0].Add(LInstance);
  LDescriptor := NyxLogicDescriptor(NyxLogicKind('deferred-fixture'),
    'Explicit resolver', 'No executable authority during route preparation.', nlgData).
    Input(NyxFlowPort('input'), nftText, 'Captured input').
    Output(NyxFlowPort('output'), nftText, 'Deferred output');
  Document.Flows.Define(NyxFlowRef('Deferred creator'), NewNyxFlowGraph.
    WithStep(NyxFlowStep('deferred'), NewNyxLogicDeclaration(
      LDescriptor, NyxObject([]))).Build);
  LCreator := NewNyxButton('creator-button').WithText('Explicit creator');
  SetNyxFlowRoutes(LCreator, NewNyxFlowRoutes.WithRoute(NyxFlowRoute(
    NyxFlowRouteID('Resolve only on invocation'), NyxFlowRef('Deferred creator'),
    ntClick, nfcmExclusive).InputConstant(
      NyxFlowEndpoint(NyxFlowStep('deferred'), NyxFlowPort('input')),
      NyxFlowText('detached')).OutputState(
      NyxFlowEndpoint(NyxFlowStep('deferred'), NyxFlowPort('output')),
      NyxTextState('guard'))));
  Document.Pages[0].Add(LCreator);
  ValidateNyxDocumentProperties(Document);
  Root := RealizeNyxView(Document, Document.Pages[0]);
  DetailID := NyxQualifiedID('first-directory', 'directory');
  LProjection := RetainNyxControl(Root.Find(DetailID)) as INyxMasterDetail;
  EditID := LProjection.DetailEditButton.Node.ID;
  EditSourceID := LProjection.DetailEditButton.Node.SourceID;
  State := Document.State.Clone;
  Collections := NewNyxCollections(Document.Collections);
  Events := AEvents;
  FOwnEvents := Events = nil;

  if Events = nil then
  begin
    Events := NewNyxEvents;
  end;
  Scheduler := Events.Scheduler;
  Check(Supports(Events, INyxEventViewPreparation, LPreparation),
    'ordinary router supplies prospective view preparation');
  View := LPreparation.PrepareView;
  Generation := View.Generation;
  ContextLease := NewNyxApplicationContext(Scheduler,
    {$ifdef PAS2JS}@{$endif}ResolveState,
    {$ifdef PAS2JS}@{$endif}ResolveCollections,
    {$ifdef PAS2JS}@{$endif}ResolveGeneration);
  ResolverProbe := TDeferredResolver.Create;
  Resolver := ResolverProbe;
end;

destructor TPreparedRouteHost.Destroy;
begin

  if Generation <> nil then
  begin
    Generation.Cancel;
  end;

  if ContextLease <> nil then
  begin
    ContextLease.Close;
  end;

  if Binding <> nil then
  begin
    Binding.Close;
  end;

  if Plan <> nil then
  begin
    Plan.Retire;
  end;

  if View <> nil then
  begin
    View.Retire;
  end;
  Plan := nil;
  Binding := nil;
  View := nil;
  ContextLease := nil;
  Resolver := nil;
  ResolverProbe := nil;

  if FOwnEvents and (Events <> nil) then
  begin
    Events.Close;
  end;
  ReleaseSources;
  Collections := nil;
  State.Free;
  Events := nil;

  if FOwnEvents and (Scheduler <> nil) then
  begin
    Scheduler.Shutdown;
  end;
  Scheduler := nil;
  inherited Destroy;
end;

function TPreparedRouteHost.ResolveState: TNyxState;
begin
  Result := State;
end;

function TPreparedRouteHost.ResolveCollections: INyxCollections;
begin
  Result := Collections;
end;

function TPreparedRouteHost.ResolveGeneration: INyxCancellationScope;
begin
  Inc(GenerationReads);

  if CancelOnGenerationRead then
  begin
    Generation.Cancel;
  end;
  Result := Generation;
end;

procedure TPreparedRouteHost.Prepare;
var
  LBinding: INyxFlowRouteBinding;
  LPlan: INyxPreparedPublication;
begin
  LPlan := PrepareNyxViewFlowRoutes(Document, Root, Events, View,
    ContextLease.Context, Resolver, LBinding);
  Binding := LBinding;
  Plan := LPlan;
end;

procedure TPreparedRouteHost.ReleaseSources;
begin
  ReleaseNyxNode(Root);
  FreeAndNil(Document);
end;

procedure TPreparedRouteHost.Change(const AText: TNyxText);
var
  LInfo: TNyxEventInfo;
begin
  LInfo := Default(TNyxEventInfo);
  LInfo.Trigger := ntChange;
  LInfo.Name := NyxEvent('change');
  LInfo.OriginID := 'source-memo';
  LInfo.SourceID := 'source-memo';
  LInfo.TargetID := 'source-memo';
  LInfo.HasValue := True;
  LInfo.ValueKind := nskText;
  LInfo.Value := NyxData(AText);
  LInfo.Details := NyxNull;
  Events.Dispatch(LInfo, 'source-memo', 'source-memo');
  LInfo.Value := NyxData('caller changed its event slot');
end;

procedure TPreparedRouteHost.Edit(const AText: TNyxText;
  const ADetails: TNyxDataValue);
var
  LInfo: TNyxEventInfo;
begin
  LInfo := Default(TNyxEventInfo);
  LInfo.Trigger := ntNamed;
  LInfo.Name := NyxSemantic(nseEdit);
  LInfo.OriginID := EditID;
  LInfo.SourceID := DetailID;
  LInfo.TargetID := DetailID;
  LInfo.HasValue := True;
  LInfo.ValueKind := nskText;
  LInfo.Value := NyxData(AText);
  LInfo.HasDetails := True;
  LInfo.Details := ADetails.Copy;
  Events.Dispatch(LInfo, EditSourceID, 'directory');
  LInfo.Value := NyxData('later scalar');
  LInfo.Details := NyxObject([NyxField('later', NyxData(True))]);
end;

function TViewWithoutReadiness.GetGeneration: INyxCancellationScope;
begin
  Result := Inner.Generation;
end;

function TViewWithoutReadiness.PrepareCallback(const ATarget: TNyxEventTarget;
  ATrigger: TNyxTrigger; const ACallback: INyxEventCallback;
  const AName: TNyxEventRef): INyxEventSubscription;
begin
  Inc(Staged);
  Result := Inner.PrepareCallback(ATarget, ATrigger, ACallback, AName);
end;

function TViewWithoutReadiness.PrepareObservation(const ATarget: TNyxEventTarget;
  ATrigger: TNyxTrigger; const AObserver: INyxEventObserver;
  APhase: TNyxObservationPhase; const AName: TNyxEventRef): INyxEventSubscription;
begin
  Inc(Staged);
  Result := Inner.PrepareObservation(ATarget, ATrigger, AObserver, APhase, AName);
end;

procedure TViewWithoutReadiness.Validate;
begin
  Inner.Validate;
end;

procedure TViewWithoutReadiness.Install;
begin
  Inner.Install;
end;

procedure TViewWithoutReadiness.Notify;
begin
  Inner.Notify;
end;

procedure TViewWithoutReadiness.Retire;
begin
  Inner.Retire;
end;

procedure TRoutePublicationProbe.Validate;
begin

  if Host <> nil then
  begin
    Reads := Host.GenerationReads;
  end;

  if Binding <> nil then
  begin
    Binding.Close;
  end;

  if Generation <> nil then
  begin
    Generation.Cancel;
  end;

  if Events <> nil then
  begin
    Events.CancelPending;
  end;
end;

procedure TRoutePublicationProbe.Install;
begin
  Installed := True;
end;

procedure TRoutePublicationProbe.Notify;
begin

  if FailNotify then
  begin
    raise ENyxFlow.Create('Fixture notification refused after installation');
  end;
end;

procedure TRoutePublicationProbe.Retire;
begin
end;

{$ifdef PAS2JS}
function RouteUITurn: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(JSValue(True));
        end, 0);
    end);
end;
{$endif}

function RouteReport(const ABinding: INyxFlowRouteBinding;
  const AName: TNyxText): TNyxDataValue; forward;

{ Real graph computation retains detached inputs and still posts publication to
  UI. Await the actual state/report predicate, never an assumed timer delay. }
procedure AwaitRoute(AHost: TPreparedRouteHost; const AState: TNyxTextStateRef;
  const AExpected: TNyxText; const ARoute: TNyxText = 'Live profile');
  {$ifdef PAS2JS}async;{$endif}
var
  LFailure: TNyxText;
  {$ifdef PAS2JS}
  LStarted: Double;
  {$else}
  LStarted: QWord;
  {$endif}
begin
  {$ifdef PAS2JS}LStarted := TJSDate.now;
  {$else}LStarted := GetTickCount64;{$endif}
  while AHost.State.GetValue(AState) <> AExpected do
  begin
    LFailure := RouteReport(AHost.Binding, ARoute).Field('failure').AsText;

    if LFailure <> '' then
    begin
      raise ENyxFlow.Create('Prepared route execution failed: ' + LFailure);
    end;
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 10000 then
    {$else}

    if GetTickCount64 - LStarted > 10000 then
    {$endif}
    begin
      raise ENyxFlow.Create('Prepared route publication timed out / state=' +
        AHost.State.GetValue(AState));
    end;
    {$ifdef PAS2JS}
    await(TJSPromise.resolve(RouteUITurn));
    {$else}
    CheckSynchronize(1);
    {$endif}
  end;
end;

function RouteReport(const ABinding: INyxFlowRouteBinding;
  const AName: TNyxText): TNyxDataValue;
var
  LIndex: Integer;
begin
  Result := NyxNull;
  for LIndex := 0 to ABinding.Count - 1 do
  begin
    Result := ABinding.Report(LIndex);

    if Result.Field('route').AsText = AName then
    begin
      Exit;
    end;
  end;
  raise ENyxFlow.Create('Expected route report is absent: ' + AName);
end;

procedure PreparedRouteRefusals;
var
  LHost: TPreparedRouteHost;
  LBinding: INyxFlowRouteBinding;
  LPlan: INyxPreparedPublication;
  LNoReady: TViewWithoutReadiness;
  LAdapter: INyxPreparedEventView;
  LFuture: INyxCancellationScope;
  LOtherScheduler: INyxScheduler;
  LOtherContext: INyxApplicationContextLease;
  LProbe: TRoutePublicationProbe;
  LParticipant: INyxPreparedPublication;
  LRejected: Boolean;
  LIndex: Integer;
begin
  LHost := TPreparedRouteHost.Create;
  try
    LRejected := False;
    try
      LPlan := PrepareNyxViewFlowRoutes(LHost.Document, LHost.Document.Pages[0],
        LHost.Events, LHost.View, LHost.ContextLease.Context, nil, LBinding);
    except
      on ENyxFlow do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LBinding = nil),
      'authored root refuses instead of silently performing another realization');
    LNoReady := TViewWithoutReadiness.Create;
    LAdapter := LNoReady;
    LNoReady.Inner := LHost.View;
    LRejected := False;
    try
      LPlan := PrepareNyxViewFlowRoutes(LHost.Document, LHost.Root, LHost.Events,
        LAdapter, LHost.ContextLease.Context, nil, LBinding);
    except
      on ENyxFlow do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LBinding = nil) and (LNoReady.Staged = 0),
      'missing event readiness refuses before any delegated registration');
    LAdapter := nil;
    LNoReady := nil;
    LFuture := LHost.Generation;
    LHost.Generation := NewNyxCancellationScope;
    LRejected := False;
    try
      LHost.Prepare;
    except
      on ENyxFlow do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LHost.Binding = nil) and not LFuture.Cancelled,
      'a different context generation cannot stand in for the prospective view');
    LHost.Generation := LFuture;
    LOtherScheduler := NewNyxScheduler;
    LOtherContext := NewNyxApplicationContext(LOtherScheduler,
      {$ifdef PAS2JS}@{$endif}LHost.ResolveState,
      {$ifdef PAS2JS}@{$endif}LHost.ResolveCollections,
      {$ifdef PAS2JS}@{$endif}LHost.ResolveGeneration);
    LRejected := False;
    try
      LPlan := PrepareNyxViewFlowRoutes(LHost.Document, LHost.Root, LHost.Events,
        LHost.View, LOtherContext.Context, nil, LBinding);
    except
      on ENyxFlow do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LBinding = nil), 'an unrelated context scheduler refuses');
    LOtherContext.Close;
    LOtherContext := nil;
    LOtherScheduler.Shutdown;
    LOtherScheduler := nil;
    LHost.Prepare;
    LHost.ContextLease.Close;
    LRejected := False;
    try
      LHost.Plan.Validate;
    except
      on ENyxFlow do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LFuture.Cancelled,
      'Validate notices a closed context; generic Close alone does not cancel its token');
    LHost.Generation.Cancel;
  finally
    LOtherContext := nil;
    LOtherScheduler := nil;
    LHost.Free;
  end;

  LHost := TPreparedRouteHost.Create;
  try
    LHost.Prepare;
    LHost.CancelOnGenerationRead := True;
    LRejected := False;
    try
      LHost.Plan.Validate;
    except
      on ENyxFlow do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and LHost.Generation.Cancelled and
      not LHost.Events.HasSubscribers(ntChange),
      'reentrant context admission cancellation cannot activate a route');
  finally
    LHost.Free;
  end;

  for LIndex := 0 to 2 do
  begin
    LHost := TPreparedRouteHost.Create;
    LProbe := TRoutePublicationProbe.Create;
    LParticipant := LProbe;
    try
      LHost.Prepare;
      LProbe.Host := LHost;
      case LIndex of
        0: LProbe.Generation := LHost.Generation;
        1: LProbe.Binding := LHost.Binding;
        2: LProbe.Events := LHost.Events;
      end;
      LRejected := False;
      try
        PublishNyxGroup([LHost.Plan, LHost.View, LParticipant]);
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and not LProbe.Installed and
        (LHost.GenerationReads = LProbe.Reads),
        'later validator revocation refuses at final fences without context getter replay');
      Check(RouteReport(LHost.Binding, 'Live profile').Field('closed').AsBoolean and
        not LHost.Events.HasSubscribers(ntChange),
        'abandoned route publication closes only its dormant observations');
      Check(LHost.State.GetValue(NyxTextState('summary')) = 'Ready',
        'refused group never computes or installs a graph output');
    finally
      LProbe.Host := nil;
      LParticipant := nil;
      LProbe := nil;
      LHost.Free;
    end;
  end;
end;

{ These are ordinary public host publications, not renderer or Studio stand-ins.
  All hosts, contexts and observers remain owned across browser awaits. }
procedure PreparedRoutes; {$ifdef PAS2JS}async;{$endif}
const
  CScalar: TNyxText = '{"profile":{"name":"Captured scalar"}}';
  CScalarResult: TNyxText = '{"name":"Captured scalar"}';
  CDetailsResult: TNyxText =
    'original scalar|{"record":"one","exact":"9007199254740993"}';
  CFormer: TNyxText = '{"profile":{"name":"Former still live"}}';
  CFormerResult: TNyxText = '{"name":"Former still live"}';
  CNext: TNyxText = '{"profile":{"name":"Accepted successor"}}';
  CNextResult: TNyxText = '{"name":"Accepted successor"}';
var
  LHost: TPreparedRouteHost;
  LAbandoned: TPreparedRouteHost;
  LNext: TPreparedRouteHost;
  LProbe: TRoutePublicationProbe;
  LParticipant: INyxPreparedPublication;
  LInfo: TNyxEventInfo;
  LDetails: TNyxDataValue;
  LBaseline: TNyxText;
  LBefore: TNyxText;
  LReads: Integer;
  LCalls: Integer;
  LRejected: Boolean;
  {$ifndef PAS2JS}
  LDrainTurn: Integer;
  {$endif}
begin
  PreparedRouteRefusals;
  LHost := TPreparedRouteHost.Create;
  LAbandoned := nil;
  LNext := nil;
  LProbe := TRoutePublicationProbe.Create;
  LParticipant := LProbe;
  try
    LBaseline := TNyxCodec.Encode(LHost.Document);
    LHost.Events.On(NyxControlEvents('source-memo', niRuntime), ntChange).
      Policy(neUIQueue);
    LHost.Prepare;
    Check((LHost.Binding.Count = 4) and (LHost.ResolverProbe.Calls = 0),
      'first-view preparation copies all routes without resolving an executable creator');
    Check(not LHost.Document.Contains(LHost.Root) and
      (RouteReport(LHost.Binding, 'Captured channels').Field('owner').AsText =
        LHost.DetailID), 'already-realized reusable runtime ID is collected exactly once');
    Check(TNyxCodec.Encode(LHost.Document) = LBaseline,
      'root-route preparation preserves all source declarations');
    LHost.Change(CScalar);
    Check((RouteReport(LHost.Binding, 'Live profile').Field('started').AsInteger = 0) and
      (LHost.State.GetValue(NyxTextState('summary')) = 'Ready'),
      'staged root observers cannot see an ordinary old-view input');
    LHost.ReleaseSources;
    LProbe.Host := LHost;
    PublishNyxGroup([LHost.Plan, LHost.View, LParticipant]);
    LReads := LProbe.Reads;
    Check(LProbe.Installed and (LHost.GenerationReads = LReads) and
      not LHost.Generation.Cancelled,
      'route/event validators precede local readiness and flag-only installation');
    LProbe.Host := nil;
    LParticipant := nil;
    LProbe := nil;
    LHost.Plan.Retire;
    LHost.View.Retire;
    Check(not RouteReport(LHost.Binding, 'Live profile').Field('closed').AsBoolean and
      (LHost.Events.On(NyxControlEvents('source-memo', niRuntime), ntChange).
        ExecutionPolicy = neUIQueue),
      'installed binding survives preparation retirement and preserves callback policy');
    LHost.Change(CScalar);
    {$ifdef PAS2JS}await(AwaitRoute(LHost, NyxTextState('compact'), CScalarResult));
    {$else}AwaitRoute(LHost, NyxTextState('compact'), CScalarResult);{$endif}
    Check((RouteReport(LHost.Binding, 'Live profile').Field('started').AsInteger = 1) and
      (RouteReport(LHost.Binding, 'Live profile').Field('succeeded').AsInteger = 1),
      'one accepted input publishes one real graph result from copied scalar Value');
    LDetails := NyxObject([NyxField('record', NyxData('one')),
      NyxField('exact', NyxData('9007199254740993'))]);
    LHost.Edit('original scalar', LDetails);
    LDetails := NyxObject([NyxField('record', NyxData('later caller value'))]);
    {$ifdef PAS2JS}
    await(AwaitRoute(LHost, NyxTextState('guard'), CDetailsResult, 'Captured channels'));
    {$else}
    AwaitRoute(LHost, NyxTextState('guard'), CDetailsResult, 'Captured channels');
    {$endif}
    Check((RouteReport(LHost.Binding, 'Captured channels').Field('started').AsInteger = 1) and
      (LHost.State.GetValue(NyxTextState('guard')) = CDetailsResult),
      'same invocation retains independent scalar Value and structured Details');
    LCalls := LHost.ResolverProbe.Calls;
    LInfo := Default(TNyxEventInfo);
    LInfo.Trigger := ntClick;
    LInfo.Name := NyxEvent('click');
    LInfo.OriginID := 'creator-button';
    LInfo.SourceID := 'creator-button';
    LInfo.TargetID := 'creator-button';
    LInfo.Value := NyxNull;
    LInfo.Details := NyxNull;
    LHost.Events.Dispatch(LInfo, 'creator-button', 'creator-button');
    Check((LHost.ResolverProbe.Calls = LCalls + 1) and
      (Pos('Explicit fixture resolver refused', RouteReport(LHost.Binding,
        'Resolve only on invocation').Field('failure').AsText) > 0),
      'only an actual invocation requests explicit resolver authority and reports refusal');

    LAbandoned := TPreparedRouteHost.Create(LHost.Events);
    LAbandoned.Prepare;
    LAbandoned.Plan.Retire;
    LAbandoned.View.Retire;
    Check(LAbandoned.Generation.Cancelled and not LHost.Generation.Cancelled,
      'abandoning a successor cancels its future generation without retiring the former');
    LHost.Change(CFormer);
    {$ifdef PAS2JS}await(AwaitRoute(LHost, NyxTextState('compact'), CFormerResult));
    {$else}AwaitRoute(LHost, NyxTextState('compact'), CFormerResult);{$endif}
    Check((LAbandoned.State.GetValue(NyxTextState('summary')) = 'Ready') and
      (RouteReport(LHost.Binding, 'Live profile').Field('started').AsInteger = 2),
      'former routes remain usable and abandoned candidate state stays untouched');
    FreeAndNil(LAbandoned);

    LBefore := LHost.State.GetValue(NyxTextState('compact'));
    LHost.Change('{"profile":{"name":"cancelled late output"}}');
    LHost.Binding.Close;
    LNext := TPreparedRouteHost.Create(LHost.Events);
    LNext.Prepare;
    LProbe := TRoutePublicationProbe.Create;
    LParticipant := LProbe;
    LProbe.Host := LNext;
    LProbe.FailNotify := True;
    LProbe.Installed := False;
    LRejected := False;
    try
      PublishNyxGroup([LNext.Plan, LNext.View, LParticipant]);
    except
      on ENyxPublicationNotification do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and LProbe.Installed and LHost.Generation.Cancelled and
      not LNext.Generation.Cancelled,
      'postcommit notification failure reports accepted successor authority');
    LNext.Plan.Retire;
    LNext.Change(CNext);
    {$ifdef PAS2JS}await(AwaitRoute(LNext, NyxTextState('compact'), CNextResult));
    {$else}AwaitRoute(LNext, NyxTextState('compact'), CNextResult);{$endif}
    Check((LHost.State.GetValue(NyxTextState('compact')) = LBefore) and
      (RouteReport(LNext.Binding, 'Live profile').Field('succeeded').AsInteger = 1),
      'accepted successor executes once while cancelled former output cannot publish');
    LNext.Binding.Close;
    LNext.Binding.Close;
    LNext.Change(CScalar);
    Check(RouteReport(LNext.Binding, 'Live profile').Field('started').AsInteger = 1,
      'explicit idempotent binding disposal revokes all later route input');
  finally

    if LProbe <> nil then
    begin
      LProbe.Host := nil;
    end;
    LParticipant := nil;
    LProbe := nil;
    LAbandoned.Free;
    LNext.Free;
    LHost.Free;
    {$ifndef PAS2JS}
    { Revoked native couriers release their independent work on UI even when a
      preceding assertion refuses. No live host/model is borrowed by this drain. }
    for LDrainTurn := 1 to 1024 do
    begin

      if not CheckSynchronize(1) then
      begin
        Break;
      end;
    end;
    {$endif}
  end;
end;

{$ifndef PAS2JS}
procedure WriteExact(const APath: String; const AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;
{$endif}

procedure SavedRoutes;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LRoutes: INyxFlowRoutes;
  LOther: INyxFlowRoutes;
  LSource: TNyxText;
  LBefore: TNyxText;
  LContinuation: TNyxText;
  LContinuationDesign: TNyxText;
  LHandwritten: TNyxText;
  LHelper: TNyxText;
  LPosition: Integer;
  LSession: TNyxStudioSession;
  LPair: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LFailed: Boolean;
begin
  LDocument := RoutedLogicDocument;
  LCopy := nil;
  LParsed := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    LDocument.Flows.Define(NyxFlowRef('Constants'), NewNyxFlowGraph.WithStep(
      NyxFlowStep('typed'), NewNyxLogicDeclaration(
        NyxLogicDescriptor(NyxLogicKind('constants'), 'Typed constants',
          'Qualification of every constant family without executable authority.', nlgData)
          .Input(NyxFlowPort('text'), nftText, 'Text')
          .Input(NyxFlowPort('boolean'), nftBoolean, 'Boolean')
          .Input(NyxFlowPort('integer'), nftInteger, 'Integer')
          .Input(NyxFlowPort('number'), nftNumber, 'Exact decimal')
          .Input(NyxFlowPort('json'), nftJSON, 'JSON')
          .Input(NyxFlowPort('bytes'), nftBytes, 'Bytes')
          .Input(NyxFlowPort('signal'), nftSignal, 'Signal'), NyxObject([]))).Build);
    LDocument.Pages[0].Add(NewNyxButton('constants-button').WithText('Typed constants'));
    SetNyxFlowRoutes(LDocument.Find('constants-button'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('All families'), NyxFlowRef('Constants'), ntClick, nfcmExclusive)
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('text')), NyxFlowText('Exact 🐾' + TNyxText(#0)))
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('boolean')), NyxFlowBoolean(True))
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('integer')), NyxFlowInteger(-17))
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('number')), NyxFlowNumber(NyxDecimal('9007199254740993.125')))
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('json')), NyxFlowJSON(NyxObject([NyxField('exact', NyxData('🐾'))])))
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('bytes')), NyxFlowBytes(NyxDecodeBase64('AAH/')))
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('signal')), NyxFlowSignal)
        .Using(neAsynchronous).Concurrent(nfrEvery).Enabled(False).MaximumRuns(4)));
    ValidateNyxDocumentProperties(LDocument);
    LRoutes := NyxControlFlowRoutes(LDocument.Find('run-button'));
    LOther := LRoutes.WithRoute(ProfileRoute.Enabled(False).MaximumRuns(3));
    Check(LRoutes.Route(0).IsEnabled and not LOther.Route(0).IsEnabled,
      'immutable route replacement preserves earlier set');
    Check((LOther.Count = 1) and (LOther.Route(0).RunLimit = 3), 'replace preserves identity/order');
    Check(LOther.WithoutRoute(NyxFlowRouteID('Format profile')).Count = 0, 'exact route removal');
    LBefore := TNyxCodec.Encode(LDocument);
    LCopy := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LCopy) = LBefore, 'route persistence is exact');
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('SetNyxFlowRoutes(', LSource) > 0) and
      (Pos('.InputEventValue(', LSource) > 0) and (Pos('nfcmExclusive', LSource) > 0) and
      (Pos('NyxFlowText(', LSource) > 0), 'crafted source retains typed event routes and constants');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LBefore, 'typed source rebuilds exact complete routes');
    Check(LWorkspace.Render(LParsed) = LSource, 'no-op route source retains exact bytes');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LPair := NyxProjectPair(LBefore, LSource);
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Select('run-button');
    LSession.SetProperty('text', 'Format this profile');
    LAfter := LSession.ProjectSnapshot;
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair), 'route pair Undo exact');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter), 'route pair Redo exact');
    LSession.ApplyPatch(NyxFlowRoutesPatch(NyxControl('run-button'), LOther));
    Check(not NyxControlFlowRoutes(LSession.Document.Find('run-button')).Route(0).IsEnabled,
      'typed route patch shares ordinary paired source publication');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'route patch Undo restores exact complete pair');
    LSession.SetSourceDraft(LAfter.Source + #10 + '{ unfinished route draft }');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LFailed := False;
    try
      LSession.ApplyPatch(NyxFlowRoutesPatch(NyxControl('run-button'), LOther));
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'route draft refusal preserves accepted pair, unfinished buffer and baseline');
    LBefore := TNyxCodec.Encode(LDocument);

    FreeAndNil(LCopy);
    LCopy := LDocument.Clone;
    SetNyxFlowRoutes(LCopy.Find('run-button'), LOther);
    LContinuation := TNyxCodegen.CustomizationRoutine(LCopy,
      TNyxCodegen.CustomizationChanges(LDocument, LCopy));
    Check((Pos('SetNyxFlowRoutes(', LContinuation) > 0) and
      (Pos('.Enabled(False)', LContinuation) > 0),
      'targeted handwritten continuation expresses route changes without replacing helpers');
    LHelper := 'function SavedRouteCapacity: Integer;' + #10 +
      'begin' + #10 + '  Result := 8;' + #10 + 'end;' + #10;
    LPosition := Pos('implementation' + #10, LSource);
    LHandwritten := Copy(LSource, 1, LPosition + Length('implementation' + #10) - 1) +
      #10 + LHelper + Copy(LSource, LPosition + Length('implementation' + #10), MaxInt);
    LPosition := Pos('.MaximumRuns(8)', LHandwritten);
    LHandwritten := Copy(LHandwritten, 1, LPosition - 1) + '.MaximumRuns(SavedRouteCapacity)' +
      Copy(LHandwritten, LPosition + Length('.MaximumRuns(8)'), MaxInt);
    LContinuation := CustomizeNyxExecutedSource(LHandwritten, LDocument, LCopy);
    LContinuationDesign := TNyxCodec.Encode(LCopy);
    Check((Pos(LHelper, LContinuation) > 0) and
      (Pos('.MaximumRuns(SavedRouteCapacity)', LContinuation) > 0),
      'handwritten route helper remains exact behind targeted continuation');
    LCopy.Flows.Remove(NyxFlowRef('Profile'));
    LFailed := False;
    try
      LCopy.Validate;
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'referenced graph removal refuses even for a disabled route');
    FreeAndNil(LCopy);
    LCopy := LDocument.Clone;
    NyxCallbacks(LCopy.Find('run-button')).OnClick.Add(NyxHandler('TExisting'), NyxCallbackID('existing'));
    LFailed := False;
    try
      ValidateNyxDocumentProperties(LCopy);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'exclusive authored callback conflict refuses before mount');
    SetNyxFlowRoutes(LCopy.Find('run-button'),
      NewNyxFlowRoutes.WithRoute(ProfileRoute(nfcmAfterDispatch)));
    ValidateNyxDocumentProperties(LCopy);
    Check(True, 'explicit mixed order admits existing callbacks');
    SetNyxFlowRoutes(LCopy.Find('pretty-code'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('unsupported'), NyxFlowRef('Profile'), ntChange, nfcmExclusive)
        .InputState(LogicEndpoint('parse', nlpText), NyxTextState('source'))
        .InputConstant(LogicEndpoint('summary', nlpLeft), NyxFlowText('Profile'))));
    LFailed := False;
    try
      ValidateNyxDocumentProperties(LCopy);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'unsupported physical event refuses instead of inventing a producer');
    {$ifndef PAS2JS}

    if ParamCount > 0 then
    begin
      ForceDirectories(ParamStr(1));
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', LSource);
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'design.nyx', LBefore);
      ForceDirectories(IncludeTrailingPathDelimiter(ParamStr(1)) + 'continuation');
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'continuation/nyx.generated.view.pas', LContinuation);
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'continuation/design.nyx', LContinuationDesign);
    end;
    {$endif}
  finally
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LCopy.Free;
    LDocument.Free;
  end;
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
begin
  try
    EventObservers;
    SavedRoutes;
    {$ifdef PAS2JS}await(PreparedRoutes);
    {$else}PreparedRoutes;{$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-flow-routes', 'passed');
    document.body.setAttribute('data-flow-routes-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' saved route/observer/source checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-flow-routes', 'failed');
      document.body.setAttribute('data-flow-routes-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  Run;
end.
