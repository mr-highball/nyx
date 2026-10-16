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

program nyx_tool_connection_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec, nyx.state,
  nyx.collections.registry, nyx.application.context, nyx.scheduler, nyx.events,
  nyx.behavior, nyx.view.behavior, nyx.flow.routing, nyx.publication,
  nyx.flow.routes, nyx.flow.route.model, nyx.flow.types, nyx.flow.declarations,
  nyx.test.flow.routes
  {$ifdef PAS2JS}, JS, Web{$else}, Classes{$endif};

type
  { One fixture owns an independent store/document and weak context resolvers.
    Event observation borrows this object only until explicit cancellation.
    No renderer/widget, production hook or external service is substituted. }
  TConnectionOwner = class
  public
    Document: TNyxDocument;
    State: TNyxState;
    Collections: INyxCollections;
    Events: INyxEvents;
    Scheduler: INyxScheduler;
    Generation: INyxCancellationScope;
    Lease: INyxApplicationContextLease;
    Behavior: INyxViewBehavior;
    RaiseGeneration: Boolean;
    ObservedStarted: Integer;
    Observations: Integer;
    constructor Create;
    destructor Destroy; override;
    function ResolveState: TNyxState;
    function ResolveCollections: INyxCollections;
    function ResolveGeneration: INyxCancellationScope;
    function RunIndex: Integer;
    procedure Click;
  end;

  { Read-only application observation registered after the original graph route.
    Its relative slot must survive graph replacement; registration order is part
    of the existing event contract. The owner is borrowed and revoked first. }
  TOrderObserver = class(TNyxEventObserver)
  public
    Owner: TConnectionOwner;
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
  end;

  { Deliberate later-participant rejection checks complete admission, without
    putting any failure hook in production. It owns no model or callback. }
  TReject = class(TInterfacedObject, INyxPreparedPublication)
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Connection contract: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TConnectionOwner.Create;
begin
  inherited Create;
  Document := RoutedLogicDocument;
  SetNyxFlowRoutes(Document.Find('run-button'),
    NewNyxFlowRoutes.WithRoute(ProfileRoute(nfcmAfterDispatch).Using(neSequential)));
  State := Document.State.Clone;
  Collections := NewNyxCollections(Document.Collections);
  Events := NewNyxEvents;
  Scheduler := Events.Scheduler;
  Generation := NewNyxCancellationScope;
  Lease := NewNyxApplicationContext(Scheduler, ResolveState, ResolveCollections, ResolveGeneration);
  Behavior := BindNyxViewBehavior(Document, Document.Pages[0], Events, Lease.Context);
end;

destructor TConnectionOwner.Destroy;
{$ifndef PAS2JS}
var
  LTurn: Integer;
{$endif}
begin

  if Behavior <> nil then
  begin
    Behavior.Close;
  end;
  Behavior := nil;

  if Lease <> nil then
  begin
    Lease.Close;
  end;

  if Events <> nil then
  begin
    Events.Close;
  end;

  if Scheduler <> nil then
  begin
    Scheduler.Shutdown;
  end;
  {$ifndef PAS2JS}
  { The real native scheduler retains deferred result couriers in the RTL UI
    queue. Cancellation revokes their receivers; actual turns retire those
    leases before the console's heap audit, without inventing successful work. }
  for LTurn := 0 to 3 do
  begin
    CheckSynchronize(0);
  end;
  {$endif}
  Collections := nil;
  State.Free;
  Document.Free;
  inherited Destroy;
end;

function TConnectionOwner.ResolveState: TNyxState;
begin
  Result := State;
end;

function TConnectionOwner.ResolveCollections: INyxCollections;
begin
  Result := Collections;
end;

function TConnectionOwner.ResolveGeneration: INyxCancellationScope;
begin

  if RaiseGeneration then
  begin
    raise ENyxPublicationNotification.CreateReceiverFailure(
      'Fixture generation refuses before route publication');
  end;
  Result := Generation;
end;

function TConnectionOwner.RunIndex: Integer;
begin
  for Result := 0 to Behavior.Routes.Count - 1 do
  begin

    if Behavior.Routes.Report(Result).Field('owner').AsText = 'run-button' then
    begin
      Exit;
    end;
  end;
  raise Exception.Create('Fixture button route missing');
end;

procedure TConnectionOwner.Click;
var
  LEvent: TNyxEventInfo;
begin
  LEvent := Default(TNyxEventInfo);
  LEvent.Trigger := ntClick;
  LEvent.Name := NyxEvent('click');
  LEvent.Value := NyxNull;
  LEvent.Details := NyxNull;
  LEvent.SourceID := 'run-button';
  LEvent.OriginID := 'run-button';
  LEvent.TargetID := 'run-button';
  Events.Dispatch(LEvent, 'run-button', 'run-button');
end;

procedure TOrderObserver.Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer);
begin
  Inc(Owner.Observations);
  Owner.ObservedStarted := Owner.Behavior.Routes.Report(Owner.RunIndex).Field('started').AsInteger;
end;

procedure TReject.Validate;
begin
  raise ENyxPublication.Create('Fixture rejects the later prepared participant');
end;

procedure TReject.Install;
begin
end;

procedure TReject.Notify;
begin
end;

procedure TReject.Retire;
begin
end;

procedure Run;
var
  LOwner: TConnectionOwner;
  LEditing: INyxViewFlowEditing;
  LPlan: INyxPreparedPublication;
  LOther: INyxPreparedPublication;
  LBinding: INyxFlowRouteBinding;
  LObservation: INyxEventObservation;
  LSubscription: INyxEventSubscription;
  LObserver: TOrderObserver;
  LBefore: TNyxText;
  LStateRevision: Integer;
  LFailed: Boolean;
  LCandidate: TNyxDocument;
begin
  LOwner := TConnectionOwner.Create;
  LSubscription := nil;
  try
    LBefore := TNyxCodec.Encode(LOwner.Document);
    LStateRevision := LOwner.State.Revision;
    Check(Supports(LOwner.Behavior, INyxViewFlowEditing, LEditing),
      'Default attachment exposes optional prepared route replacement');
    LBinding := LOwner.Behavior.Routes;
    LPlan := LEditing.PrepareFlowRoutes(LOwner.Document, LOwner.Document.Pages[0]);
    LFailed := False;
    try
      LOwner.Behavior.EnableGraphs(False);
    except
      on E: ENyxApplicationContext do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Reserved attachment refuses reentrant graph-policy mutation');
    LPlan := nil;
    LOwner.Behavior.EnableGraphs(False);
    LOwner.Behavior.EnableGraphs(True);
    Check(LOwner.Behavior.Routes = LBinding, 'Abandoned preparation preserves the accepted binding');

    LOwner.RaiseGeneration := True;
    LFailed := False;
    try
      LPlan := LEditing.PrepareFlowRoutes(LOwner.Document, LOwner.Document.Pages[0]);
    except
      on E: ENyxPublicationNotification do
      begin
        LFailed := True;
      end;
    end;
    LOwner.RaiseGeneration := False;
    LOwner.Behavior.EnableGraphs(False);
    LOwner.Behavior.EnableGraphs(True);
    Check(LFailed and (LOwner.Behavior.Routes = LBinding),
      'Pre-install getter exception releases reservation and leaves the binding exact');

    LPlan := LEditing.PrepareFlowRoutes(LOwner.Document, LOwner.Document.Pages[0]);
    LOther := TReject.Create;
    LFailed := False;
    try
      PublishNyxGroup([LPlan, LOther]);
    except
      on E: ENyxPublication do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LOwner.Behavior.Routes = LBinding) and
      (LOwner.State.Revision = LStateRevision),
      'A later participant rejects all prepared graph routes before installation');
    LPlan := nil;
    LOther := nil;

    LCandidate := LOwner.Document.Clone;
    try
      SetNyxFlowRoutes(LCandidate.Find('run-button'),
        NewNyxFlowRoutes.WithRoute(ProfileRoute(nfcmExclusive).Using(neSequential)));
      LFailed := False;
      try
        LPlan := LEditing.PrepareFlowRoutes(LCandidate, LCandidate.Pages[0]);
      except
        on E: ENyxFlow do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed and (LOwner.Behavior.Routes = LBinding),
        'Route-phase changes refuse before touching accepted observation slots');
    finally
      LCandidate.Free;
    end;

    Supports(LOwner.Events, INyxEventObservation, LObservation);
    LObserver := TOrderObserver.Create;
    LObserver.Owner := LOwner;
    LSubscription := LObservation.Observe(NyxControlEvents('run-button', niRuntime),
      ntClick, LObserver, nopAfterDispatch, Default(TNyxEventRef));
    LOwner.Click;
    Check((LOwner.Observations = 1) and (LOwner.ObservedStarted = 1),
      'Ordinary observer initially follows graph dispatch in its registered slot');
    LPlan := LEditing.PrepareFlowRoutes(LOwner.Document, LOwner.Document.Pages[0]);
    PublishNyxGroup([LPlan]);
    LOwner.Click;
    Check((LOwner.Observations = 2) and (LOwner.ObservedStarted = 1),
      'Replacing graph meaning preserves existing external observer registration order');
    Check(TNyxCodec.Encode(LOwner.Document) = LBefore,
      'Prepared runtime replacement preserves every authored byte');

    LBinding := LOwner.Behavior.Routes;
    LPlan := LEditing.PrepareFlowRoutes(LOwner.Document, LOwner.Document.Pages[0]);
    LOwner.Generation.Cancel;
    LFailed := False;
    try
      PublishNyxGroup([LPlan]);
    except
      on E: ENyxApplicationContext do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LOwner.Behavior.Routes = LBinding),
      'Cancelled generation refuses before observer transfer');
    LPlan := nil;
    LOwner.Generation := NewNyxCancellationScope;
    LPlan := LEditing.PrepareFlowRoutes(LOwner.Document, LOwner.Document.Pages[0]);
    LOwner.Behavior.Close;
    LFailed := False;
    try
      PublishNyxGroup([LPlan]);
    except
      on E: ENyxApplicationContext do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LBinding.Report(0).Field('closed').AsBoolean,
      'Retired fragment refuses prepared routes and closes its accepted observations');
  finally

    if LSubscription <> nil then
    begin
      LSubscription.Cancel;
    end;
    LPlan := nil;
    LOther := nil;
    LEditing := nil;
    LBinding := nil;
    LOwner.Free;
  end;
end;

{ Derive a distinct typed fan-out route from the fixture's public endpoints.
  This consumer never copies/rewrites raw route fields or executable creators. }
function AdditionalRoute: TNyxFlowRoute;
var
  LOriginal: TNyxFlowRoute;
  LIndex: Integer;
begin
  LOriginal := ProfileRoute(nfcmAfterDispatch);
  Result := NyxFlowRoute(NyxFlowRouteID('Additional profile'), NyxFlowRef('Profile'),
    ntClick, nfcmAfterDispatch).Using(neSequential)
    .InputState(LOriginal.Input(0).Target, NyxTextState(LOriginal.Input(0).StateName))
    .InputConstant(LOriginal.Input(1).Target, TNyxFlowValue.FromData(LOriginal.Input(1).ConstantData));
  for LIndex := 0 to LOriginal.OutputCount - 1 do
  begin
    Result := Result.OutputState(LOriginal.Output(LIndex).Source,
      NyxTextState(LOriginal.Output(LIndex).StateName));
  end;
end;

{ A real mounted-fragment owner stages route topology through the optional
  capability, retaining its callback context and independent event observers.
  Ordinary router dispatch executes graphs after publication, not before it. }
procedure RunTopology;
var
  LBefore: TNyxText;
  LOwner: TConnectionOwner;
  LCandidate: TNyxDocument;
  LEditing: INyxViewRouteTopology;
  LBinding: INyxFlowRouteBinding;
  LContext: INyxApplicationContext;
  LPlan: INyxPreparedPublication;
  LOther: INyxPreparedPublication;
  LObserver: TOrderObserver;
  LSubscription: INyxEventSubscription;
  LObservation: INyxEventObservation;
  LFailed: Boolean;
  LCount: Integer;
  LIndex: Integer;
  LStarted: Integer;
begin
  LOwner := TConnectionOwner.Create;
  LCandidate := nil;
  try
    LBefore := TNyxCodec.Encode(LOwner.Document);
    Check(Supports(LOwner.Behavior, INyxViewRouteTopology, LEditing),
      'existing fragment offers optional dynamic route publication');
    LBinding := LOwner.Behavior.Routes;
    LContext := LOwner.Behavior.Context;
    LCount := LBinding.Count;
    Supports(LOwner.Events, INyxEventObservation, LObservation);
    LObserver := TOrderObserver.Create;
    LObserver.Owner := LOwner;
    LSubscription := LObservation.Observe(NyxControlEvents('run-button', niRuntime),
      ntClick, LObserver, nopAfterDispatch, NyxEvent(''));
    LCandidate := LOwner.Document.Clone;
    SetNyxFlowRoutes(LCandidate.Find('run-button'),
      NyxControlFlowRoutes(LCandidate.Find('run-button')).WithRoute(AdditionalRoute));
    LPlan := LEditing.PrepareRouteChanges(LCandidate, LCandidate.Pages[0]);
    LOwner.Click;
    Check((LOwner.Behavior.Routes = LBinding) and (LBinding.Count = LCount),
      'staging additional topology keeps the old route owner active');
    Check(LOwner.ObservedStarted = 1, 'independent observer still follows the original slot during staging');
    LPlan := nil;
    LOwner.Click;
    Check((LOwner.Behavior.Routes = LBinding) and (LOwner.ObservedStarted = 2),
      'abandoned new topology preserves old observation slots and routes');
    LPlan := LEditing.PrepareRouteChanges(LCandidate, LCandidate.Pages[0]);
    LOther := TReject.Create;
    LFailed := False;
    try
      PublishNyxGroup([LPlan, LOther]);
    except
      on E: ENyxPublication do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LOwner.Behavior.Routes = LBinding),
      'later rejection cannot publish the new fragment route shape');
    LPlan := nil;
    LOther := nil;
    LPlan := LEditing.PrepareRouteChanges(LCandidate, LCandidate.Pages[0]);
    PublishNyxGroup([LPlan]);
    LPlan := nil;
    Check((LOwner.Behavior.Routes.Count = LCount + 1) and
      LBinding.Report(LOwner.RunIndex).Field('closed').AsBoolean and
      (LOwner.Behavior.Context = LContext),
      'dynamic installation retires old graph authority while keeping the ordinary callback context');
    LOwner.Click;
    LStarted := 0;
    for LIndex := 0 to LOwner.Behavior.Routes.Count - 1 do
    begin

      if LOwner.Behavior.Routes.Report(LIndex).Field('route').AsText = 'Additional profile' then
      begin
        LStarted := LOwner.Behavior.Routes.Report(LIndex).Field('started').AsInteger;
      end;
    end;
    Check((LOwner.ObservedStarted = 1) and (LStarted = 1),
      'surviving slots keep external order and the appended route actually executes');
    SetNyxFlowRoutes(LCandidate.Find('run-button'), NewNyxFlowRoutes
      .WithRoute(AdditionalRoute).WithRoute(ProfileRoute(nfcmAfterDispatch).Using(neSequential)));
    LFailed := False;
    try
      LPlan := LEditing.PrepareRouteChanges(LCandidate, LCandidate.Pages[0]);
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'an unexpressible surviving-slot reordering refuses rather than changing external order');
    SetNyxFlowRoutes(LCandidate.Find('run-button'), NewNyxFlowRoutes);
    LPlan := LEditing.PrepareRouteChanges(LCandidate, LCandidate.Pages[0]);
    PublishNyxGroup([LPlan]);
    LPlan := nil;
    Check((LOwner.Behavior.Routes.Count = LCount - 1) and LSubscription.Active,
      'removing a control route set preserves an unrelated independently owned observer');
    SetNyxFlowRoutes(LCandidate.Find('run-button'), NewNyxFlowRoutes
      .WithRoute(ProfileRoute(nfcmAfterDispatch).Using(neSequential)));
    LPlan := LEditing.PrepareRouteChanges(LCandidate, LCandidate.Pages[0]);
    PublishNyxGroup([LPlan]);
    LPlan := nil;
    LOwner.Click;
    Check((LOwner.Behavior.Routes.Count = LCount) and (LOwner.ObservedStarted = 0),
      'recreated topology receives a fresh appended slot after the retained external observer');
    Check(TNyxCodec.Encode(LOwner.Document) = LBefore,
      'runtime topology does not mutate the authored source document');
  finally

    if LSubscription <> nil then
    begin
      LSubscription.Cancel;
    end;
    LPlan := nil;
    LOther := nil;
    LEditing := nil;
    LBinding := nil;
    LContext := nil;
    LObservation := nil;
    LCandidate.Free;
    LOwner.Free;
  end;
end;

begin
  try
    Run;
    RunTopology;
    WriteLn('PASS ', GChecks, ' prepared connection contract checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-tool-connection-contract', 'passed');
    document.body.setAttribute('data-tool-connection-contract-checks', IntToStr(GChecks));
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-tool-connection-contract', 'failed');
      document.body.setAttribute('data-tool-connection-contract-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
