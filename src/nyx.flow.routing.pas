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


unit nyx.flow.routing;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.events,
  nyx.behavior, nyx.scheduler, nyx.state, nyx.application.context,
  nyx.flow.routes, nyx.flow.declarations, nyx.flow;

type
  { UI-owned application attachment. Close revokes observers and cancels owned
    runs before a host retires its controls. It retains no document, renderer or
    application: the context is a revocable weak port. Reports contain copied
    status only; they confer no execution or host authority. }
  INyxFlowRouteBinding = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000015}']
    procedure Close;
    function GetCount: Integer;
    function Report(AIndex: Integer): TNyxDataValue;
    procedure Cancel(AIndex: Integer);
    property Count: Integer read GetCount;
  end;

{ Bind once after authored callbacks, before mounting the first view. Complete
  effective declarations admit before the first observer is installed. Executable
  creator resolution is lazy on invocation through the explicitly supplied
  resolver. Saved data cannot grant access to HTTP/files/dialogs or host code.
  This attachment covers ordinary realized page/reusable trees. Canvas item
  hosts require their own runtime ownership attachment and are not inferred. }
function BindNyxFlowRoutes(ADocument: TNyxDocument; const AEvents: INyxEvents;
  const AContext: INyxApplicationContext;
  const AResolver: INyxLogicResolver = nil): INyxFlowRouteBinding;
{ Exact embedded page/reusable root. Only that root's effective descendants are
  attached; unrelated document pages must never create phantom item routes. }
function BindNyxViewFlowRoutes(ADocument: TNyxDocument; ARoot: TNyxNode;
  const AEvents: INyxEvents; const AContext: INyxApplicationContext;
  const AResolver: INyxLogicResolver = nil): INyxFlowRouteBinding;

implementation

uses
  nyx.schema, nyx.composition, nyx.flow.route.model, nyx.flow.types,
  nyx.flow.execution, nyx.linq, nyx.linq.execution;

type
  TRouting = class;
  TPlan = class
  public
    OwnerID: TNyxText;
    Target: TNyxEventTarget;
    Route: TNyxFlowRoute;
    Definition: INyxFlowDefinition;
    Graph: INyxFlowGraph;
    Subscription: INyxEventSubscription;
    Runs: array of INyxQueryRun;
    Starting: Boolean;
    Started: Integer;
    Skipped: Integer;
    Succeeded: Integer;
    Discarded: Integer;
    Cancelled: Integer;
    Failed: Integer;
    Failure: TNyxText;
    procedure Prune;
    procedure Cancel;
  end;
  { Owner is borrowed only until explicit revocation. The callback first acquires
    a managed owner lease; a reentrant Close may revoke future callbacks without
    destroying the currently executing plan. There is no reverse managed edge. }
  TRouteObserver = class(TNyxEventObserver)
  public
    Owner: TRouting;
    Index: Integer;
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
  end;
  { A graph with no mapped state output still needs generation-guarded result
    admission. Side effects already performed by explicit host operations cannot
    be rolled back, and are never represented as an atomic state transaction. }
  TCompletionPort = class(TInterfacedObject, INyxQueryResultPort<INyxFlowResult>)
  public
    Context: INyxApplicationContext;
    Generation: INyxCancellationScope;
    function Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
  end;
  TRouting = class(TInterfacedObject, INyxFlowRouteBinding)
  private
    FScheduler: INyxScheduler;
    FContext: INyxApplicationContext;
    FResolver: INyxLogicResolver;
    FPlans: array of TPlan;
    FObservers: array of TRouteObserver;
    FObserverLeases: array of INyxEventObserver;
    FClosed: Boolean;
    procedure Invoke(AIndex: Integer; const AEvent: TNyxEventInfo; ACallbackCount: Integer);
    procedure Collect(ANode: TNyxNode; ADocument: TNyxDocument);
    procedure RequireIndex(AIndex: Integer);
  public
    constructor Create(ADocument: TNyxDocument; ASelectedRoot: TNyxNode; const AEvents: INyxEvents;
      const AContext: INyxApplicationContext; const AResolver: INyxLogicResolver);
    destructor Destroy; override;
    procedure Close;
    function GetCount: Integer;
    function Report(AIndex: Integer): TNyxDataValue;
    procedure Cancel(AIndex: Integer);
  end;

procedure TPlan.Prune;
var
  LIndex: Integer;
  LCount: Integer;
begin
  LCount := 0;
  for LIndex := 0 to High(Runs) do
  begin

    if Runs[LIndex].Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
    begin
      Runs[LCount] := Runs[LIndex];
      Inc(LCount);
    end
    else
    begin
      case Runs[LIndex].Phase of
        nqpSucceeded: Inc(Succeeded);
        nqpDiscarded: Inc(Discarded);
        nqpCancelled: Inc(Cancelled);
        nqpFailed:
          begin
            Inc(Failed);
            Failure := Runs[LIndex].Failure;
          end;
        nqpPending, nqpComputing, nqpAwaitingUI:
          begin
            { The UI read above classified terminal progress. These cases are
              explicitly inert if an alternate run reports changing snapshots. }
          end;
      end;
    end;
  end;
  SetLength(Runs, LCount);
end;

procedure TPlan.Cancel;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(Runs) do
  begin
    Runs[LIndex].Cancel;
  end;
  Prune;
end;

procedure TRouteObserver.Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer);
var
  LOwner: TRouting;
  LLease: INyxFlowRouteBinding;
begin
  LOwner := Owner;

  if LOwner = nil then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.Invoke(Index, AEvent, ACallbackCount);
end;

function TCompletionPort.Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
begin
  Result := Context.Active and not Generation.Cancelled and
    (Context.Generation = Generation) and (Length(AValues) = 1);
end;

constructor TRouting.Create(ADocument: TNyxDocument; ASelectedRoot: TNyxNode; const AEvents: INyxEvents;
  const AContext: INyxApplicationContext; const AResolver: INyxLogicResolver);
var
  LObservation: INyxEventObservation;
  LRoot: TNyxNode;
  LIndex: Integer;
  LObserver: TRouteObserver;
  LPhase: TNyxObservationPhase;
  LEvents: INyxEvents;
  LContext: INyxApplicationContext;
  LResolver: INyxLogicResolver;
  LHasRoutes: Boolean;

  function HasRoutes(ANode: TNyxNode): Boolean;
  var
    LChild: Integer;
  begin
    Result := ANode.Extensions.Has(NyxExtension(NyxFlowRoutesKey));

    if Result then
    begin
      Exit;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin

      if HasRoutes(ANode.Children[LChild]) then
      begin
        Exit(True);
      end;
    end;
  end;

begin
  inherited Create;
  LEvents := AEvents;
  LContext := AContext;
  LResolver := AResolver;

  if (ADocument = nil) or (LEvents = nil) or (LContext = nil) then
  begin
    raise ENyxFlow.Create('Logic routes require a document, event router and application context');
  end;
  FScheduler := LEvents.Scheduler;
  FScheduler.RequireUI;
  FContext := LContext;
  FResolver := LResolver;
  { Ordinary applications without route metadata do not pay for another full
    page/reusable realization or thousands of temporary empty route sets. }
  LHasRoutes := ASelectedRoot <> nil;

  if ASelectedRoot = nil then
  begin
    for LIndex := 0 to ADocument.Count - 1 do
    begin
      LHasRoutes := LHasRoutes or HasRoutes(ADocument.Pages[LIndex]);
    end;
    for LIndex := 0 to ADocument.ComponentCount - 1 do
    begin
      LHasRoutes := LHasRoutes or HasRoutes(ADocument.Components[LIndex]);
    end;
  end;

  if not LHasRoutes then
  begin
    Exit;
  end;

  if ASelectedRoot <> nil then
  begin
    LRoot := RealizeNyxView(ADocument, ASelectedRoot);
    try
      Collect(LRoot, ADocument);
    finally
      ReleaseNyxNode(LRoot);
    end;
  end
  else
  begin
    for LIndex := 0 to ADocument.Count - 1 do
    begin
      LRoot := RealizeNyxView(ADocument, ADocument.Pages[LIndex]);
      try
        Collect(LRoot, ADocument);
      finally
        ReleaseNyxNode(LRoot);
      end;
    end;
  end;

  if Length(FPlans) = 0 then
  begin
    Exit;
  end;

  if not Supports(LEvents, INyxEventObservation, LObservation) then
  begin
    raise ENyxFlow.Create('This event router does not support ordered Logic observation');
  end;
  SetLength(FObservers, Length(FPlans));
  SetLength(FObserverLeases, Length(FPlans));
  for LIndex := 0 to High(FPlans) do
  begin
    LObserver := TRouteObserver.Create;
    FObserverLeases[LIndex] := LObserver;
    FObservers[LIndex] := LObserver;
    LObserver.Owner := Self;
    LObserver.Index := LIndex;
    LPhase := nopBeforeCallbacks;

    if FPlans[LIndex].Route.CallbackMode = nfcmAfterDispatch then
    begin
      LPhase := nopAfterDispatch;
    end;
    FPlans[LIndex].Subscription := LObservation.Observe(FPlans[LIndex].Target,
      FPlans[LIndex].Route.Trigger, LObserver, LPhase, FPlans[LIndex].Route.EventName);
  end;
end;

destructor TRouting.Destroy;
var
  LIndex: Integer;
begin
  Close;
  for LIndex := 0 to High(FPlans) do
  begin
    FPlans[LIndex].Free;
  end;
  inherited Destroy;
end;

procedure TRouting.Collect(ANode: TNyxNode; ADocument: TNyxDocument);
var
  LRoutes: INyxFlowRoutes;
  LIndex: Integer;
  LPlan: TPlan;
begin
  LRoutes := nil;

  if ANode.Extensions.Has(NyxExtension(NyxFlowRoutesKey)) then
  begin
    ValidateNyxControlFlowRoutes(ANode, ADocument);
    LRoutes := NyxControlFlowRoutes(ANode);
  end;

  if LRoutes <> nil then
  begin
    for LIndex := 0 to LRoutes.Count - 1 do
    begin
      LPlan := TPlan.Create;
      SetLength(FPlans, Length(FPlans) + 1);
      FPlans[High(FPlans)] := LPlan;
      LPlan.OwnerID := ANode.ID;
      LPlan.Target := NyxControlEvents(ANode.ID, niRuntime);

      if ANode.Prop(NyxAttributeName(atCompound)) = 'true' then
      begin
        LPlan.Target := NyxCompoundEvents(ANode.ID, niRuntime);
      end;
      LPlan.Route := LRoutes.Route(LIndex);
      LPlan.Definition := NyxFlowDefinitionFromData(
        ADocument.Flows.Definition(LPlan.Route.Flow).ToData);
      FScheduler.Admit(LPlan.Route.Policy);
    end;
  end;
  for LIndex := 0 to ANode.Count - 1 do
  begin
    Collect(ANode.Children[LIndex], ADocument);
  end;
end;

procedure TRouting.Close;
var
  LIndex: Integer;
begin

  if FClosed then
  begin
    Exit;
  end;

  if FScheduler <> nil then
  begin
    FScheduler.RequireUI;
  end;
  FClosed := True;
  for LIndex := 0 to High(FObservers) do
  begin

    if FObservers[LIndex] <> nil then
    begin
      FObservers[LIndex].Owner := nil;
    end;
  end;
  for LIndex := 0 to High(FPlans) do
  begin

    if FPlans[LIndex].Subscription <> nil then
    begin
      FPlans[LIndex].Subscription.Cancel;
      FPlans[LIndex].Subscription := nil;
    end;
    FPlans[LIndex].Cancel;
  end;
  FObserverLeases := nil;
  FObservers := nil;
  FResolver := nil;
end;

procedure TRouting.RequireIndex(AIndex: Integer);
begin
  FScheduler.RequireUI;

  if (AIndex < 0) or (AIndex >= Length(FPlans)) then
  begin
    raise ENyxFlow.Create('Logic route report index is out of bounds');
  end;
end;

function TRouting.GetCount: Integer;
begin
  FScheduler.RequireUI;
  Result := Length(FPlans);
end;

function TRouting.Report(AIndex: Integer): TNyxDataValue;
var
  LPlan: TPlan;
begin
  RequireIndex(AIndex);
  LPlan := FPlans[AIndex];
  LPlan.Prune;
  Result := NyxObject([NyxField('owner', NyxData(LPlan.OwnerID)),
    NyxField('route', NyxData(LPlan.Route.ID.Name)),
    NyxField('flow', NyxData(LPlan.Route.Flow.Name)),
    NyxField('closed', NyxData(FClosed)),
    NyxField('active', NyxData(Length(LPlan.Runs))),
    NyxField('started', NyxData(LPlan.Started)),
    NyxField('skipped', NyxData(LPlan.Skipped)),
    NyxField('succeeded', NyxData(LPlan.Succeeded)),
    NyxField('discarded', NyxData(LPlan.Discarded)),
    NyxField('cancelled', NyxData(LPlan.Cancelled)),
    NyxField('failed', NyxData(LPlan.Failed)),
    NyxField('failure', NyxData(LPlan.Failure))]);
end;

procedure TRouting.Cancel(AIndex: Integer);
begin
  RequireIndex(AIndex);
  FPlans[AIndex].Cancel;
end;

function Scalar(const AValue: TNyxDataValue; AKind: TNyxStateKind): TNyxFlowValue;
begin
  case AKind of
    nskText: Result := NyxFlowText(AValue.AsText);
    nskBoolean: Result := NyxFlowBoolean(AValue.AsBoolean);
    nskInteger: Result := NyxFlowInteger(AValue.AsInteger);
    nskNumber: Result := NyxFlowNumber(AValue.AsDecimal);
  end;
end;

function StateScalar(const AValue: TNyxStateValue): TNyxFlowValue;
begin
  case AValue.Kind of
    nskText: Result := NyxFlowText(AValue.TextValue);
    nskBoolean: Result := NyxFlowBoolean(AValue.BooleanValue);
    nskInteger: Result := NyxFlowInteger(AValue.IntegerValue);
    nskNumber: Result := NyxFlowNumber(NyxDecimal(AValue.NumberText));
  end;
end;

procedure TRouting.Invoke(AIndex: Integer; const AEvent: TNyxEventInfo; ACallbackCount: Integer);
var
  LPlan: TPlan;
  LInputs: array of TNyxFlowInput;
  LAssignments: array of INyxFlowAssignment;
  LRowAssignments: array of INyxFlowRowsAssignment;
  LInput: TNyxFlowRouteInput;
  LOutput: TNyxFlowRouteOutput;
  LRowsOutput: TNyxFlowRouteRowsOutput;
  LValue: TNyxFlowValue;
  LGeneration: INyxCancellationScope;
  LResolver: INyxLogicResolver;
  LRun: INyxQueryRun;
  LCompletion: TCompletionPort;
  LPort: INyxQueryResultPort<INyxFlowResult>;
  LIndex: Integer;
begin
  FScheduler.RequireUI;

  if FClosed or not FContext.Active then
  begin
    Exit;
  end;
  LPlan := FPlans[AIndex];

  if not LPlan.Route.IsEnabled then
  begin
    Exit;
  end;
  LPlan.Prune;

  if LPlan.Starting then
  begin
    LPlan.Failure := 'Logic route refuses reentry during executable/input capture';
    Exit;
  end;

  if (LPlan.Route.Concurrency = nfrIgnoreWhileRunning) and (Length(LPlan.Runs) > 0) then
  begin
    Inc(LPlan.Skipped);
    Exit;
  end;
  LPlan.Starting := True;
  try
    try

      if (LPlan.Route.CallbackMode = nfcmExclusive) and (ACallbackCount > 0) then
      begin
        raise ENyxFlow.Create('Exclusive Logic route refuses matching ordinary callbacks');
      end;

      if LPlan.Route.Concurrency = nfrLatest then
      begin
        LPlan.Cancel;
      end;

      if Length(LPlan.Runs) >= LPlan.Route.RunLimit then
      begin
        raise ENyxFlow.Create('Logic route active-run capacity reached');
      end;
      LGeneration := FContext.Generation;

      if LPlan.Graph = nil then
      begin
        { Closing the attachment from inside a creator resolver revokes future
          authority but must not release that resolver during its own call. }
        LResolver := FResolver;
        LPlan.Graph := LPlan.Definition.Build(LResolver);
      end;
      { A creator resolver may navigate, retire this attachment or dispose the
        application. Recheck weak ownership before reading even one state cell. }

      if FClosed or not FContext.Active or LGeneration.Cancelled or
        (FContext.Generation <> LGeneration) then
      begin
        raise ENyxFlow.Create('Logic route owner changed during executable capture');
      end;
      SetLength(LInputs, LPlan.Route.InputCount);
      for LIndex := 0 to High(LInputs) do
      begin
        LInput := LPlan.Route.Input(LIndex);
        case LInput.Source of
          nfisConstant: LValue := TNyxFlowValue.FromData(LInput.ConstantData);
          nfisState: LValue := StateScalar(FContext.State.Value(LInput.StateName));
          nfisEventValue:
            begin

              if not AEvent.HasValue or
                (NyxFlowStateType(AEvent.ValueKind) <> LInput.ValueType) then
              begin
                raise ENyxFlow.Create('Logic event invocation has no matching scalar payload');
              end;
              LValue := Scalar(AEvent.Value, AEvent.ValueKind);
            end;
          nfisEventDetails:
            begin

              if not AEvent.HasDetails then
              begin
                raise ENyxFlow.Create('Logic event invocation has no structured payload');
              end;
              LValue := NyxFlowJSON(LInput.DetailsPath.Select(AEvent.Details));
            end;
          nfisSignal: LValue := NyxFlowSignal;
        end;
        LInputs[LIndex] := NyxFlowInput(LInput.Target, LValue);
      end;
      SetLength(LAssignments, LPlan.Route.OutputCount);
      for LIndex := 0 to High(LAssignments) do
      begin
        LOutput := LPlan.Route.Output(LIndex);
        case LOutput.StateKind of
          nskText: LAssignments[LIndex] := NyxFlowAssign(NyxTextState(LOutput.StateName), LOutput.Source);
          nskBoolean: LAssignments[LIndex] := NyxFlowAssign(NyxBooleanState(LOutput.StateName), LOutput.Source);
          nskInteger: LAssignments[LIndex] := NyxFlowAssign(NyxIntegerState(LOutput.StateName), LOutput.Source);
          nskNumber: LAssignments[LIndex] := NyxFlowAssign(NyxNumberState(LOutput.StateName), LOutput.Source);
        end;
      end;

      SetLength(LRowAssignments, LPlan.Route.RowOutputCount);
      for LIndex := 0 to High(LRowAssignments) do
      begin
        LRowsOutput := LPlan.Route.RowOutput(LIndex);
        LRowAssignments[LIndex] := NyxFlowAssignRows(LRowsOutput.Collection,
          LRowsOutput.Source, LRowsOutput.Rows);
      end;

      if Length(LAssignments) + Length(LRowAssignments) > 0 then
      begin
        LRun := TNyxFlowApplication.Submit(FContext, LPlan.Graph, LInputs,
          LAssignments, LRowAssignments, LPlan.Route.Policy);
      end
      else
      begin
        LCompletion := TCompletionPort.Create;
        LPort := LCompletion;
        LCompletion.Context := FContext;
        LCompletion.Generation := LGeneration;
        LRun := TNyxFlowExecution.Submit(LPlan.Graph, LInputs, FScheduler, LPort,
          LPlan.Route.Policy, LGeneration);
      end;

      if FClosed or not FContext.Active or LGeneration.Cancelled then
      begin
        LRun.Cancel;
      end
      else
      begin
        SetLength(LPlan.Runs, Length(LPlan.Runs) + 1);
        LPlan.Runs[High(LPlan.Runs)] := LRun;
        Inc(LPlan.Started);
        LPlan.Failure := '';
        LPlan.Prune;
      end;
    except
      on LException: Exception do
      begin
        {$IFDEF PAS2JS}
        LPlan.Failure := LException.Message;
        {$ELSE}
        LPlan.Failure := UTF8Encode(UnicodeString(LException.Message));
        {$ENDIF}
      end;
    end;
  finally
    LPlan.Starting := False;
  end;
end;

function BindNyxFlowRoutes(ADocument: TNyxDocument; const AEvents: INyxEvents;
  const AContext: INyxApplicationContext; const AResolver: INyxLogicResolver): INyxFlowRouteBinding;
begin
  Result := TRouting.Create(ADocument, nil, AEvents, AContext, AResolver);
end;

function BindNyxViewFlowRoutes(ADocument: TNyxDocument; ARoot: TNyxNode;
  const AEvents: INyxEvents; const AContext: INyxApplicationContext;
  const AResolver: INyxLogicResolver): INyxFlowRouteBinding;
begin

  if (ADocument = nil) or (ARoot = nil) or not ADocument.Contains(ARoot) then
  begin
    raise ENyxFlow.Create('Embedded routes require their exact authored context root');
  end;
  Result := TRouting.Create(ADocument, ARoot, AEvents, AContext, AResolver);
end;

end.
