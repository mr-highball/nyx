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
unit nyx.flow.execution;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.state, nyx.flow.types, nyx.flow,
  nyx.linq, nyx.linq.execution, nyx.scheduler, nyx.application.context;

type
  { Scheduler-owned computation and ordinary UI publication. Inputs/graph are
    immutable captured values. A worker never receives an application/state/view.
    UI-only operations refuse worker policies; browser threaded work refuses
    through the existing scheduler. Dropping a run does not cancel its work.
    Cancel cooperates inside graph/operation checkpoints and revokes delivery. }
  TNyxFlowExecution = class
  public
    class function Submit(const AGraph: INyxFlowGraph;
      const AInputs: array of TNyxFlowInput; const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<INyxFlowResult>;
      APolicy: TNyxExecutionPolicy = neUIQueue;
      const AGeneration: INyxCancellationScope = nil): INyxQueryRun; static;
  end;

  { Immutable typed output-to-state route. JSON/bytes are never automatically
    converted into scalar state; compose an explicit formatting/transform step.
    Exact decimal output requires the destination's canonical IEEE spelling;
    otherwise publication refuses instead of silently losing precision/meaning. }
  INyxFlowAssignment = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000010}']
    function GetTarget: TNyxText;
    function GetKind: TNyxStateKind;
    function GetSource: TNyxFlowEndpoint;
    property Target: TNyxText read GetTarget;
    property Kind: TNyxStateKind read GetKind;
    property Source: TNyxFlowEndpoint read GetSource;
  end;

  { Capture/admission on UI, compute independently, publish every mapped output
    with ONE ordinary state Apply at the exact captured store revision. Current
    application/page generation and exact store identity guard delivery. Stale,
    failed or cancelled graphs cannot partially update controls. Application
    observers that commit then throw retain ordinary State.Apply semantics.
    Explicit creator host/UI side effects are outside this result transaction;
    the runner never fabricates rollback for an already-performed operation. }
  TNyxFlowApplication = class
  public
    class function Submit(const AContext: INyxApplicationContext;
      const AGraph: INyxFlowGraph; const AInputs: array of TNyxFlowInput;
      const AAssignments: array of INyxFlowAssignment;
      APolicy: TNyxExecutionPolicy = neUIQueue): INyxQueryRun; static;
  end;

function NyxFlowAssign(const ATarget: TNyxTextStateRef;
  const ASource: TNyxFlowEndpoint): INyxFlowAssignment; overload;
function NyxFlowAssign(const ATarget: TNyxBooleanStateRef;
  const ASource: TNyxFlowEndpoint): INyxFlowAssignment; overload;
function NyxFlowAssign(const ATarget: TNyxIntegerStateRef;
  const ASource: TNyxFlowEndpoint): INyxFlowAssignment; overload;
function NyxFlowAssign(const ATarget: TNyxNumberStateRef;
  const ASource: TNyxFlowEndpoint): INyxFlowAssignment; overload;

implementation

type
  TInputs = array of TNyxFlowInput;
  TAssignments = array of INyxFlowAssignment;
  { One worker-safe token combines explicit run cancellation with owning view
    retirement. It retains neither run, scheduler nor application. }
  TFlowScope = class(TInterfacedObject, INyxCancellationScope)
  public
    Local: INyxCancellationScope;
    Generation: INyxCancellationScope;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;
  TFlowRun = class(TInterfacedObject, INyxQueryRun)
  public
    Run: INyxQueryRun;
    Scope: INyxCancellationScope;
    procedure Cancel;
    function GetPhase: TNyxQueryPhase;
    function GetFailure: TNyxText;
  end;
  TSelector = class(TInterfacedObject, INyxSelector<INyxFlowGraph, INyxFlowResult>)
  private
    FInputs: TInputs;
    FScope: INyxCancellationScope;
  public
    constructor Create(const AInputs: array of TNyxFlowInput;
      const AScope: INyxCancellationScope);
    function Project(const AGraph: INyxFlowGraph): INyxFlowResult;
  end;
  TAssignment = class(TInterfacedObject, INyxFlowAssignment)
  private
    FTarget: TNyxText;
    FKind: TNyxStateKind;
    FSource: TNyxFlowEndpoint;
  public
    constructor Create(const ATarget: TNyxText; AKind: TNyxStateKind;
      const ASource: TNyxFlowEndpoint);
    function GetTarget: TNyxText;
    function GetKind: TNyxStateKind;
    function GetSource: TNyxFlowEndpoint;
  end;
  TApplicationPort = class(TInterfacedObject, INyxQueryResultPort<INyxFlowResult>)
  private
    FContext: INyxApplicationContext;
    FScheduler: INyxScheduler;
    FGeneration: INyxCancellationScope;
    FState: TNyxState;
    FRevision: Integer;
    FAssignments: TAssignments;
  public
    constructor Create(const AContext: INyxApplicationContext;
      const AScheduler: INyxScheduler; const AGeneration: INyxCancellationScope;
      AState: TNyxState; ARevision: Integer; const AAssignments: TAssignments);
    function Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
  end;

procedure TFlowScope.Cancel;
begin
  Local.Cancel;
end;

function TFlowScope.GetCancelled: Boolean;
begin
  Result := Local.Cancelled or ((Generation <> nil) and Generation.Cancelled);
end;

procedure TFlowRun.Cancel;
begin

  if Run.Phase in [nqpSucceeded, nqpDiscarded, nqpCancelled, nqpFailed] then
  begin
    Exit;
  end;
  Run.Cancel;
  Scope.Cancel;
end;

function TFlowRun.GetPhase: TNyxQueryPhase;
begin
  Result := Run.Phase;
end;

function TFlowRun.GetFailure: TNyxText;
begin
  Result := Run.Failure;
end;

constructor TSelector.Create(const AInputs: array of TNyxFlowInput;
  const AScope: INyxCancellationScope);
var
  LIndex: Integer;
begin
  inherited Create;
  FScope := AScope;
  SetLength(FInputs, Length(AInputs));
  for LIndex := 0 to High(AInputs) do
  begin
    FInputs[LIndex] := AInputs[LIndex];
  end;
end;

function TSelector.Project(const AGraph: INyxFlowGraph): INyxFlowResult;
begin
  try
    Result := AGraph.Evaluate(FInputs, FScope);
  except
    on LException: ENyxFlowCancelled do
    begin
      { Query checks its generation immediately after this selector returns.
        Cooperative graph cancellation is an outcome, not a compute failure;
        no placeholder result can reach the UI port through the cancelled token. }
      FScope.Cancel;
      Result := nil;
    end;
  end;
end;

class function TNyxFlowExecution.Submit(const AGraph: INyxFlowGraph;
  const AInputs: array of TNyxFlowInput; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<INyxFlowResult>; APolicy: TNyxExecutionPolicy;
  const AGeneration: INyxCancellationScope): INyxQueryRun;
var
  LRun: TFlowRun;
  LSelector: INyxSelector<INyxFlowGraph, INyxFlowResult>;
  LSequence: INyxSequence<INyxFlowResult>;
  LScope: TFlowScope;
begin

  if (AGraph = nil) or (AScheduler = nil) or (APort = nil) then
  begin
    raise ENyxFlow.Create('Graph execution requires a graph, scheduler and owned result port');
  end;
  AScheduler.RequireUI;
  AScheduler.Admit(APolicy);

  if (APolicy in [neAsynchronous, neThreaded]) and not AGraph.WorkerSafe and
    (nscWorkerThreads in AScheduler.Capabilities) then
  begin
    raise ENyxFlow.Create('UI-affinity logic cannot enter a worker policy');
  end;
  AGraph.ValidateInputs(AInputs);
  LRun := TFlowRun.Create;
  Result := LRun;
  LScope := TFlowScope.Create;
  LRun.Scope := LScope;
  LScope.Local := NewNyxCancellationScope;
  LScope.Generation := AGeneration;
  LSelector := TSelector.Create(AInputs, LRun.Scope);
  LSequence := TNyxSelect<INyxFlowGraph, INyxFlowResult>.From(
    TNyxSequence<INyxFlowGraph>.From([AGraph]), LSelector);
  LRun.Run := TNyxQuery<INyxFlowResult>.Submit(LSequence, AScheduler, APort,
    APolicy, LRun.Scope);
end;

constructor TAssignment.Create(const ATarget: TNyxText; AKind: TNyxStateKind;
  const ASource: TNyxFlowEndpoint);
begin
  inherited Create;
  ASource.Validate;

  if (Ord(AKind) < Ord(Low(TNyxStateKind))) or
    (Ord(AKind) > Ord(High(TNyxStateKind))) then
  begin
    raise ENyxFlow.Create('Unknown graph state destination family');
  end;
  FTarget := ATarget;
  FKind := AKind;
  FSource := ASource;
end;

function TAssignment.GetTarget: TNyxText;
begin
  Result := FTarget;
end;

function TAssignment.GetKind: TNyxStateKind;
begin
  Result := FKind;
end;

function TAssignment.GetSource: TNyxFlowEndpoint;
begin
  Result := FSource;
end;

function NyxFlowAssign(const ATarget: TNyxTextStateRef;
  const ASource: TNyxFlowEndpoint): INyxFlowAssignment;
begin
  Result := TAssignment.Create(ATarget.Name, nskText, ASource);
end;

function NyxFlowAssign(const ATarget: TNyxBooleanStateRef;
  const ASource: TNyxFlowEndpoint): INyxFlowAssignment;
begin
  Result := TAssignment.Create(ATarget.Name, nskBoolean, ASource);
end;

function NyxFlowAssign(const ATarget: TNyxIntegerStateRef;
  const ASource: TNyxFlowEndpoint): INyxFlowAssignment;
begin
  Result := TAssignment.Create(ATarget.Name, nskInteger, ASource);
end;

function NyxFlowAssign(const ATarget: TNyxNumberStateRef;
  const ASource: TNyxFlowEndpoint): INyxFlowAssignment;
begin
  Result := TAssignment.Create(ATarget.Name, nskNumber, ASource);
end;

function FlowType(AKind: TNyxStateKind): TNyxFlowType;
begin
  case AKind of
    nskText: Result := nftText;
    nskBoolean: Result := nftBoolean;
    nskInteger: Result := nftInteger;
    nskNumber: Result := nftNumber;
  end;
end;

constructor TApplicationPort.Create(const AContext: INyxApplicationContext;
  const AScheduler: INyxScheduler; const AGeneration: INyxCancellationScope;
  AState: TNyxState; ARevision: Integer; const AAssignments: TAssignments);
begin
  inherited Create;
  FContext := AContext;
  FScheduler := AScheduler;
  FGeneration := AGeneration;
  FState := AState;
  FRevision := ARevision;
  FAssignments := Copy(AAssignments, 0, Length(AAssignments));
end;

function TApplicationPort.Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
var
  LState: TNyxState;
  LAssignments: array of TNyxStateAssignment;
  LIndex: Integer;
  LValue: TNyxFlowValue;
  LScalar: TNyxStateValue;
begin
  FScheduler.RequireUI;
  Result := FContext.Active and not FGeneration.Cancelled;

  if not Result then
  begin
    Exit;
  end;
  LState := FContext.State;

  if LState <> FState then
  begin
    raise ENyxFlow.Create('Application state owner changed before graph delivery');
  end;

  if (Length(AValues) <> 1) or (AValues[0] = nil) then
  begin
    raise ENyxFlow.Create('Graph publication requires one complete successful output');
  end;
  SetLength(LAssignments, Length(FAssignments));
  for LIndex := 0 to High(FAssignments) do
  begin
    LValue := AValues[0].Value(FAssignments[LIndex].Source);
    case FAssignments[LIndex].Kind of
      nskText: LScalar := TNyxStateValue.FromText(LValue.AsText);
      nskBoolean: LScalar := TNyxStateValue.FromBoolean(LValue.AsBoolean);
      nskInteger: LScalar := TNyxStateValue.FromInteger(LValue.AsInteger);
      nskNumber:
        begin
          LScalar := TNyxStateValue.FromNumber(LValue.AsFloat);

          if LScalar.NumberText <> LValue.AsNumber.Text then
          begin
            raise ENyxFlow.Create('Exact decimal needs an explicit IEEE-number conversion before state publication');
          end;
        end;
    end;
    LAssignments[LIndex] := NyxStateAssign(FAssignments[LIndex].Target, LScalar);
  end;
  LState.Apply(LAssignments, FRevision);
end;

class function TNyxFlowApplication.Submit(const AContext: INyxApplicationContext;
  const AGraph: INyxFlowGraph; const AInputs: array of TNyxFlowInput;
  const AAssignments: array of INyxFlowAssignment; APolicy: TNyxExecutionPolicy): INyxQueryRun;
var
  LState: TNyxState;
  LCapturedState: TNyxState;
  LRevision: Integer;
  LScheduler: INyxScheduler;
  LGeneration: INyxCancellationScope;
  LValues: TAssignments;
  LIndex: Integer;
  LPrevious: Integer;
  LPortInfo: TNyxFlowPort;
  LPort: INyxQueryResultPort<INyxFlowResult>;
  LAssignment: INyxFlowAssignment;
  LTarget: TNyxText;
  LKind: TNyxStateKind;
  LSource: TNyxFlowEndpoint;
begin

  if (AContext = nil) or (AGraph = nil) or (Length(AAssignments) = 0) or
    (Length(AAssignments) > 256) then
  begin
    raise ENyxFlow.Create('Graph publication requires a context and 1..256 typed mappings');
  end;
  LScheduler := AContext.Scheduler;
  LScheduler.RequireUI;
  LGeneration := AContext.Generation;

  if LGeneration = nil then
  begin
    raise ENyxFlow.Create('Application graph publication requires a revocable generation');
  end;
  LState := AContext.State;
  LRevision := LState.Revision;
  { Extension getters can retire the application. Validate destinations against
    an owned snapshot, then revalidate the weak context before reading live state. }
  LCapturedState := LState.Clone;
  try
    SetLength(LValues, Length(AAssignments));
    for LIndex := 0 to High(AAssignments) do
    begin

      if AAssignments[LIndex] = nil then
      begin
        raise ENyxFlow.Create('Graph publication mapping cannot be absent');
      end;
      { Retain the extension independently and copy getters in a defined order;
        no borrowed property-result address may survive another creator callback. }
      LAssignment := AAssignments[LIndex];
      LTarget := LAssignment.Target;
      LKind := LAssignment.Kind;
      LSource := LAssignment.Source;
      LValues[LIndex] := TAssignment.Create(LTarget, LKind, LSource);
      LPortInfo := AGraph.Port(LValues[LIndex].Source);

      if (LPortInfo.Direction <> nfdOutput) or
        (LPortInfo.ValueType <> FlowType(LValues[LIndex].Kind)) or
        (LCapturedState.Value(LValues[LIndex].Target).Kind <> LValues[LIndex].Kind) then
      begin
        raise ENyxFlow.Create('Graph output and existing destination must have the same scalar family');
      end;
      for LPrevious := 0 to LIndex - 1 do
      begin

        if LValues[LPrevious].Target = LValues[LIndex].Target then
        begin
          raise ENyxFlow.Create('Graph publication destinations must be distinct');
        end;
      end;
    end;
    AGraph.ValidateInputs(AInputs);

    if not AContext.Active or LGeneration.Cancelled or (AContext.State <> LState) or
      (LState.Revision <> LRevision) then
    begin
      raise ENyxFlow.Create('Application changed during graph publication capture');
    end;
    LPort := TApplicationPort.Create(AContext, LScheduler, LGeneration, LState, LRevision, LValues);
    Result := TNyxFlowExecution.Submit(AGraph, AInputs, LScheduler, LPort, APolicy, LGeneration);
  finally
    LCapturedState.Free;
  end;
end;

end.
