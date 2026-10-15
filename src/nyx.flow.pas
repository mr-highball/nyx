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
unit nyx.flow;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.bytes, nyx.scheduler, nyx.flow.types, nyx.logic;

type
  { One independent invocation supplies all unwired required inputs. Duplicate
    endpoints refuse rather than silently replacing a prior assignment. }
  TNyxFlowInput = record
    Endpoint: TNyxFlowEndpoint;
    Value: TNyxFlowValue;
  end;
  { Copied typed dependency; sharing a producer never transfers mutable data or
    grants a second producer to an already connected input. }
  TNyxFlowWire = record
    Source: TNyxFlowEndpoint;
    Target: TNyxFlowEndpoint;
  end;

  { Immutable complete successful output. Failed/cancelled runs never expose
    earlier step results as successful. Only executed steps contribute values;
    conditional skip provenance is available through the optional interface. }
  INyxFlowResult = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000006}']
    function GetCount: Integer;
    function Entry(AIndex: Integer): TNyxFlowInput;
    function Value(const AEndpoint: TNyxFlowEndpoint): TNyxFlowValue;
    property Count: Integer read GetCount;
  end;

  { Sealed admitted graph, independent of its builder and all invocations.
    Each selected operation executes once in topological/declaration order;
    fan-out shares immutable output values. Every input has one producer or an
    explicit invocation value. No implicit casts, defaults or accidental feedback.
    Cycles require a future explicit feedback runner and currently refuse.
    Pure operations own immutable/thread-safe implementation state. Alternative
    implementations must honor that contract, not retain control/view cycles.
    Direct Evaluate runs on the caller's thread and requires UI for UI affinity. }
  INyxFlowGraph = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000007}']
    function GetStepCount: Integer;
    function GetWireCount: Integer;
    function GetWorkerSafe: Boolean;
    function Port(const AEndpoint: TNyxFlowEndpoint): TNyxFlowPort;
    procedure ValidateInputs(const AInputs: array of TNyxFlowInput);
    function Evaluate(const AInputs: array of TNyxFlowInput;
      const ACancellation: INyxCancellationScope = nil): INyxFlowResult;
    function ToData: TNyxDataValue;
    property StepCount: Integer read GetStepCount;
    property WireCount: Integer read GetWireCount;
    property WorkerSafe: Boolean read GetWorkerSafe;
  end;

  { One owned sequential invocation, independent of every other continuation.
    Inputs/Operation describe only the current topological step. Accept admits
    its COMPLETE output frame and byte budget before advancing. Result refuses
    until all selected steps succeed, so no successful prefix escapes a failure.
    A continuation belongs to one executing thread; no UI/model is retained. }
  INyxFlowContinuation = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000029}']
    function GetCompleted: Boolean;
    function GetStep: TNyxFlowStepRef;
    function GetDescriptor: TNyxLogicDescriptor;
    function GetOperation: INyxLogicOperation;
    function GetInputs: TNyxFlowValues;
    procedure Accept(const AOutputs: TNyxFlowValues);
    function GetResult: INyxFlowResult;
    property Completed: Boolean read GetCompleted;
    property Step: TNyxFlowStepRef read GetStep;
    property Descriptor: TNyxLogicDescriptor read GetDescriptor;
    property Operation: INyxLogicOperation read GetOperation;
    property Inputs: TNyxFlowValues read GetInputs;
    property Result: INyxFlowResult read GetResult;
  end;
  { Optional capability preserves the original graph interface/implementation
    freedom. Direct Evaluate refuses async graphs before any operation enters;
    the scheduler coordinator consumes this explicit continuation contract. }
  INyxResumableFlowGraph = interface(IInterface)
    ['{D1411014-6120-4C84-A708-00000000002A}']
    function GetHasAsync: Boolean;
    function BeginEvaluation(const AInputs: array of TNyxFlowInput;
      const ACancellation: INyxCancellationScope): INyxFlowContinuation;
    property HasAsync: Boolean read GetHasAsync;
  end;

  { Authoring-thread builder. WithStep/Connect admit locally before mutation;
    Build validates the COMPLETE candidate and captures independent vectors.
    A temporarily cyclic/missing-edge candidate may be assembled, but Build
    refuses it without changing any previously sealed graph. }
  INyxFlowBuilder = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000008}']
    function WithStep(const AStep: TNyxFlowStepRef;
      const AOperation: INyxLogicOperation): INyxFlowBuilder;
    function Connect(const ASource, ATarget: TNyxFlowEndpoint): INyxFlowBuilder;
    function Build: INyxFlowGraph;
  end;

  { Authoring-only conditions derive an independent graph from a sealed graph.
    When replaces the one Boolean condition for a step; Always removes it.
    Build validates condition dependencies together with data wires. Runtime
    implementations/authority stay shared immutably, never reconstructed by name. }
  INyxFlowConditions = interface(IInterface)
    ['{60CAFF21-C6E4-43D0-8855-940000000020}']
    function When(const AStep: TNyxFlowStepRef; const ACondition: TNyxFlowEndpoint;
      AExpected: Boolean = True): INyxFlowConditions;
    function Always(const AStep: TNyxFlowStepRef): INyxFlowConditions;
    function Build: INyxFlowGraph;
  end;

  { Optional derivation capability keeps the original graph interface intact.
    Independent graph implementations may supply their own condition builder;
    unsupported implementations refuse before any original graph is changed. }
  INyxConditionalFlowGraph = interface(IInterface)
    ['{60CAFF21-C6E4-43D0-8855-940000000021}']
    function Conditions: INyxFlowConditions;
  end;

  { Optional complete-result provenance. A skipped output has no invented value.
    WasSkipped first validates an exact declared output, so misspelled endpoints
    cannot silently preserve old application data. Unknown endpoints still refuse. }
  INyxConditionalFlowResult = interface(IInterface)
    ['{60CAFF21-C6E4-43D0-8855-940000000022}']
    function WasSkipped(const AEndpoint: TNyxFlowEndpoint): Boolean;
  end;

  { An explicit resolver reconstructs creator operations at the wire boundary.
    The built-in resolver never executes/imports arbitrary code or host names. }
  INyxLogicResolver = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000009}']
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: TNyxDataValue): INyxLogicOperation;
  end;

function NyxFlowInput(const AEndpoint: TNyxFlowEndpoint;
  const AValue: TNyxFlowValue): TNyxFlowInput;
function NewNyxFlowGraph: INyxFlowBuilder; overload;
function NewNyxFlowGraph(const ALimits: TNyxFlowLimits): INyxFlowBuilder; overload;
function NyxFlowGraphFromData(const AData: TNyxDataValue;
  const AResolver: INyxLogicResolver = nil): INyxFlowGraph;
function NyxFlowConditions(const AGraph: INyxFlowGraph): INyxFlowConditions;
{ Older result implementations retain required-value behavior. Only explicit
  declared skip provenance permits a publisher to retain a destination value. }
function NyxFlowOutputSkipped(const AResult: INyxFlowResult;
  const AEndpoint: TNyxFlowEndpoint): Boolean;

implementation

type
  { Owned immutable step schema/configuration and executable implementation.
    Graphs share the implementation's immutable contract, never a builder/tree. }
  TStep = class
  public
    Reference: TNyxFlowStepRef;
    Operation: INyxLogicOperation;
    Descriptor: TNyxLogicDescriptor;
    Configuration: TNyxDataValue;
  end;
  { Graph-owned step objects; each clone owns a distinct vector and step objects. }
  TSteps = array of TStep;
  { Detached immutable data-wire vector, also used during authoring admission. }
  TWires = array of TNyxFlowWire;
  { Owned invocation/output frames containing no control or renderer references. }
  TInputs = array of TNyxFlowInput;
  { Private finite topological counters/order, independently owned per graph. }
  TIndices = array of Integer;

  { One closed Boolean dependency. It grants neither an output value nor host
    authority; its source participates in the same cycle check as data wires. }
  TCondition = record
    Step: TNyxFlowStepRef;
    Source: TNyxFlowEndpoint;
    Expected: Boolean;
  end;
  { Detached graph-owned condition vector, at most one entry per admitted step. }
  TConditions = array of TCondition;

  { Sealed graph owns independent steps/wires/conditions. Executions and derived
    builders retain it without creating any runtime/UI back reference. }
  TGraph = class(TInterfacedObject, INyxFlowGraph, INyxResumableFlowGraph,
    INyxConditionalFlowGraph)
  private
    FSteps: TSteps;
    FWires: TWires;
    FConditions: TConditions;
    FLimits: TNyxFlowLimits;
    FOrder: TIndices;
    function StepIndex(const AStep: TNyxFlowStepRef): Integer;
    function Wired(const AEndpoint: TNyxFlowEndpoint): Integer;
    procedure AdmitValue(const AValue: TNyxFlowValue; var ATotal: Integer);
    procedure Seal;
    procedure CopyInto(ATarget: TGraph);
  public
    destructor Destroy; override;
    function GetStepCount: Integer;
    function GetWireCount: Integer;
    function GetWorkerSafe: Boolean;
    function Port(const AEndpoint: TNyxFlowEndpoint): TNyxFlowPort;
    procedure ValidateInputs(const AInputs: array of TNyxFlowInput);
    function Evaluate(const AInputs: array of TNyxFlowInput;
      const ACancellation: INyxCancellationScope): INyxFlowResult;
    function ToData: TNyxDataValue;
    function GetHasAsync: Boolean;
    function BeginEvaluation(const AInputs: array of TNyxFlowInput;
      const ACancellation: INyxCancellationScope): INyxFlowContinuation;
    function Conditions: INyxFlowConditions;
  end;
  { Mutable authoring owner. Build copies into an independently sealed graph;
    condition derivation starts with its own step objects and vectors. }
  TBuilder = class(TInterfacedObject, INyxFlowBuilder, INyxFlowConditions)
  private
    FGraph: TGraph;
    FAdmitting: Boolean;
  public
    constructor Create(const ALimits: TNyxFlowLimits);
    destructor Destroy; override;
    function WithStep(const AStep: TNyxFlowStepRef;
      const AOperation: INyxLogicOperation): INyxFlowBuilder;
    function Connect(const ASource, ATarget: TNyxFlowEndpoint): INyxFlowBuilder;
    function Build: INyxFlowGraph;
    function When(const AStep: TNyxFlowStepRef; const ACondition: TNyxFlowEndpoint;
      AExpected: Boolean): INyxFlowConditions;
    function Always(const AStep: TNyxFlowStepRef): INyxFlowConditions;
  end;
  { Complete invocation data/provenance. Schema and skip flags are pure snapshots;
    no graph, operation, builder, scheduler or application is retained. }
  TResult = class(TInterfacedObject, INyxFlowResult, INyxConditionalFlowResult)
  public
    Values: TInputs;
    Steps: array of TNyxFlowStepRef;
    Descriptors: array of TNyxLogicDescriptor;
    Skipped: array of Boolean;
    function GetCount: Integer;
    function Entry(AIndex: Integer): TNyxFlowInput;
    function Value(const AEndpoint: TNyxFlowEndpoint): TNyxFlowValue;
    function WasSkipped(const AEndpoint: TNyxFlowEndpoint): Boolean;
  end;
  { One-thread invocation owner. Only complete accepted step frames advance;
    skipped dependencies are traversed without invoking their implementations. }
  TContinuation = class(TInterfacedObject, INyxFlowContinuation)
  private
    FOwner: INyxFlowGraph;
    FGraph: TGraph;
    FCancellation: INyxCancellationScope;
    FInputs: TInputs;
    FFrames: array of TNyxFlowValues;
    FOrder: Integer;
    FTotal: Integer;
    FReady: Boolean;
    FFrame: TNyxFlowValues;
    FResult: TResult;
    FResultPort: INyxFlowResult;
    procedure RequireStep;
    procedure AdvanceSkipped;
  public
    constructor Create(AGraph: TGraph; const AInputs: array of TNyxFlowInput;
      const ACancellation: INyxCancellationScope);
    function GetCompleted: Boolean;
    function GetStep: TNyxFlowStepRef;
    function GetDescriptor: TNyxLogicDescriptor;
    function GetOperation: INyxLogicOperation;
    function GetInputs: TNyxFlowValues;
    procedure Accept(const AOutputs: TNyxFlowValues);
    function GetResult: INyxFlowResult;
  end;

function NyxFlowInput(const AEndpoint: TNyxFlowEndpoint;
  const AValue: TNyxFlowValue): TNyxFlowInput;
begin
  AEndpoint.Validate;
  AValue.GetType;
  Result.Endpoint := AEndpoint;
  Result.Value := AValue;
end;

function NewNyxFlowGraph: INyxFlowBuilder;
begin
  Result := NewNyxFlowGraph(TNyxFlowLimits.Defaults);
end;

function NewNyxFlowGraph(const ALimits: TNyxFlowLimits): INyxFlowBuilder;
begin
  Result := TBuilder.Create(ALimits);
end;

function NyxFlowConditions(const AGraph: INyxFlowGraph): INyxFlowConditions;
var
  LGraph: INyxConditionalFlowGraph;
begin

  if not Supports(AGraph, INyxConditionalFlowGraph, LGraph) then
  begin
    raise ENyxFlow.Create('This graph implementation does not support conditional derivation');
  end;
  Result := LGraph.Conditions;
end;

function NyxFlowOutputSkipped(const AResult: INyxFlowResult;
  const AEndpoint: TNyxFlowEndpoint): Boolean;
var
  LResult: INyxConditionalFlowResult;
begin
  AEndpoint.Validate;

  if AResult = nil then
  begin
    raise ENyxFlow.Create('Skipped-output inspection requires a complete graph result');
  end;
  Result := Supports(AResult, INyxConditionalFlowResult, LResult);

  if Result then
  begin
    Result := LResult.WasSkipped(AEndpoint);
  end;
end;

constructor TBuilder.Create(const ALimits: TNyxFlowLimits);
begin
  inherited Create;
  ALimits.Validate;
  FGraph := TGraph.Create;
  FGraph.FLimits := ALimits;
end;

destructor TBuilder.Destroy;
begin
  FGraph.Free;
  inherited Destroy;
end;

destructor TGraph.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FSteps) do
  begin
    FSteps[LIndex].Free;
  end;
  inherited Destroy;
end;

function TGraph.StepIndex(const AStep: TNyxFlowStepRef): Integer;
begin
  NyxFlowStep(AStep.Name);
  for Result := 0 to High(FSteps) do
  begin

    if FSteps[Result].Reference.Name = AStep.Name then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TGraph.GetStepCount: Integer;
begin
  Result := Length(FSteps);
end;

function TGraph.GetWireCount: Integer;
begin
  Result := Length(FWires);
end;

function TGraph.GetWorkerSafe: Boolean;
var
  LIndex: Integer;
begin
  Result := True;
  for LIndex := 0 to High(FSteps) do
  begin
    Result := Result and (FSteps[LIndex].Descriptor.Affinity = nlaPure);
  end;
end;

function TGraph.Port(const AEndpoint: TNyxFlowEndpoint): TNyxFlowPort;
var
  LIndex: Integer;
begin
  AEndpoint.Validate;
  LIndex := StepIndex(AEndpoint.Step);

  if LIndex < 0 then
  begin
    raise ENyxFlow.Create('Graph step is not declared: ' + AEndpoint.Step.Name);
  end;
  Result := FSteps[LIndex].Descriptor.Find(AEndpoint.Port);
end;

function TGraph.Wired(const AEndpoint: TNyxFlowEndpoint): Integer;
begin
  for Result := 0 to High(FWires) do
  begin

    if FWires[Result].Target.Same(AEndpoint) then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TBuilder.WithStep(const AStep: TNyxFlowStepRef;
  const AOperation: INyxLogicOperation): INyxFlowBuilder;
var
  LStep: TStep;
  LDescriptor: TNyxLogicDescriptor;
  LConfiguration: TNyxDataValue;
begin

  if FAdmitting then
  begin
    raise ENyxFlow.Create('Graph builder admission cannot reenter');
  end;

  if (AOperation = nil) or (FGraph.StepIndex(AStep) >= 0) or
    (Length(FGraph.FSteps) >= FGraph.FLimits.MaximumSteps) then
  begin
    raise ENyxFlow.Create('A graph requires distinct steps, operations and available capacity');
  end;
  FAdmitting := True;
  try
    LDescriptor := AOperation.Descriptor;
    LDescriptor.Validate;
    LConfiguration := AOperation.Configuration.Copy;
  finally
    FAdmitting := False;
  end;

  if NyxUTF8ByteCount(LConfiguration.ToJSON) > FGraph.FLimits.MaximumValueBytes then
  begin
    raise ENyxFlow.Create('Logic configuration exceeds its admitted byte budget');
  end;
  LStep := TStep.Create;
  try
    LStep.Reference := AStep;
    LStep.Operation := AOperation;
    LStep.Descriptor := LDescriptor;
    LStep.Configuration := LConfiguration;
    SetLength(FGraph.FSteps, Length(FGraph.FSteps) + 1);
    FGraph.FSteps[High(FGraph.FSteps)] := LStep;
    LStep := nil;
  finally
    LStep.Free;
  end;
  Result := Self;
end;

function TBuilder.Connect(const ASource, ATarget: TNyxFlowEndpoint): INyxFlowBuilder;
var
  LSource: TNyxFlowPort;
  LTarget: TNyxFlowPort;
begin

  if FAdmitting then
  begin
    raise ENyxFlow.Create('Graph builder admission cannot reenter');
  end;
  LSource := FGraph.Port(ASource);
  LTarget := FGraph.Port(ATarget);

  if (LSource.Direction <> nfdOutput) or (LTarget.Direction <> nfdInput) or
    (LSource.ValueType <> LTarget.ValueType) or (FGraph.Wired(ATarget) >= 0) or
    (Length(FGraph.FWires) >= FGraph.FLimits.MaximumWires) then
  begin
    raise ENyxFlow.Create('Wire requires matching output/input types and one input producer');
  end;
  SetLength(FGraph.FWires, Length(FGraph.FWires) + 1);
  FGraph.FWires[High(FGraph.FWires)].Source := ASource;
  FGraph.FWires[High(FGraph.FWires)].Target := ATarget;
  Result := Self;
end;

function TBuilder.When(const AStep: TNyxFlowStepRef;
  const ACondition: TNyxFlowEndpoint; AExpected: Boolean): INyxFlowConditions;
var
  LPort: TNyxFlowPort;
  LIndex: Integer;
begin

  if FAdmitting or (FGraph.StepIndex(AStep) < 0) then
  begin
    raise ENyxFlow.Create('A condition requires an existing step outside reentrant admission');
  end;
  LPort := FGraph.Port(ACondition);

  if (LPort.Direction <> nfdOutput) or (LPort.ValueType <> nftBoolean) then
  begin
    raise ENyxFlow.Create('A step condition requires an exact Boolean output');
  end;
  for LIndex := 0 to High(FGraph.FConditions) do
  begin

    if FGraph.FConditions[LIndex].Step.Name = AStep.Name then
    begin
      FGraph.FConditions[LIndex].Source := ACondition;
      FGraph.FConditions[LIndex].Expected := AExpected;
      Exit(Self);
    end;
  end;
  LIndex := Length(FGraph.FConditions);
  SetLength(FGraph.FConditions, LIndex + 1);
  FGraph.FConditions[LIndex].Step := AStep;
  FGraph.FConditions[LIndex].Source := ACondition;
  FGraph.FConditions[LIndex].Expected := AExpected;
  Result := Self;
end;

function TBuilder.Always(const AStep: TNyxFlowStepRef): INyxFlowConditions;
var
  LIndex: Integer;
  LNext: Integer;
begin

  if FAdmitting or (FGraph.StepIndex(AStep) < 0) then
  begin
    raise ENyxFlow.Create('Unconditional derivation requires an existing step');
  end;
  for LIndex := 0 to High(FGraph.FConditions) do
  begin

    if FGraph.FConditions[LIndex].Step.Name = AStep.Name then
    begin
      for LNext := LIndex to High(FGraph.FConditions) - 1 do
      begin
        FGraph.FConditions[LNext] := FGraph.FConditions[LNext + 1];
      end;
      SetLength(FGraph.FConditions, Length(FGraph.FConditions) - 1);
      Break;
    end;
  end;
  Result := Self;
end;

procedure TGraph.CopyInto(ATarget: TGraph);
var
  LIndex: Integer;
begin
  ATarget.FLimits := FLimits;
  ATarget.FWires := Copy(FWires, 0, Length(FWires));
  ATarget.FConditions := Copy(FConditions, 0, Length(FConditions));
  SetLength(ATarget.FSteps, Length(FSteps));
  for LIndex := 0 to High(FSteps) do
  begin
    ATarget.FSteps[LIndex] := TStep.Create;
    ATarget.FSteps[LIndex].Reference := FSteps[LIndex].Reference;
    ATarget.FSteps[LIndex].Operation := FSteps[LIndex].Operation;
    ATarget.FSteps[LIndex].Descriptor := FSteps[LIndex].Descriptor;
    ATarget.FSteps[LIndex].Configuration := FSteps[LIndex].Configuration;
  end;
end;

function TGraph.Conditions: INyxFlowConditions;
var
  LBuilder: TBuilder;
begin
  LBuilder := TBuilder.Create(FLimits);
  Result := LBuilder;
  CopyInto(LBuilder.FGraph);
end;

procedure TGraph.Seal;
var
  LCounts: TIndices;
  LDone: array of Boolean;
  LIndex: Integer;
  LWire: Integer;
  LSelected: Integer;
  LTurn: Integer;
begin

  if Length(FSteps) = 0 then
  begin
    raise ENyxFlow.Create('An executable graph requires at least one step');
  end;
  SetLength(FOrder, Length(FSteps));
  SetLength(LCounts, Length(FSteps));
  SetLength(LDone, Length(FSteps));
  for LWire := 0 to High(FWires) do
  begin
    Inc(LCounts[StepIndex(FWires[LWire].Target.Step)]);
  end;
  for LWire := 0 to High(FConditions) do
  begin
    Inc(LCounts[StepIndex(FConditions[LWire].Step)]);
  end;
  for LTurn := 0 to High(FSteps) do
  begin
    LSelected := -1;
    for LIndex := 0 to High(FSteps) do
    begin

      if not LDone[LIndex] and (LCounts[LIndex] = 0) then
      begin
        LSelected := LIndex;
        Break;
      end;
    end;

    if LSelected < 0 then
    begin
      raise ENyxFlow.Create('Graph cycle requires an explicit feedback runner');
    end;
    FOrder[LTurn] := LSelected;
    LDone[LSelected] := True;
    for LWire := 0 to High(FWires) do
    begin

      if FWires[LWire].Source.Step.Name = FSteps[LSelected].Reference.Name then
      begin
        Dec(LCounts[StepIndex(FWires[LWire].Target.Step)]);
      end;
    end;
    for LWire := 0 to High(FConditions) do
    begin

      if FConditions[LWire].Source.Step.Name = FSteps[LSelected].Reference.Name then
      begin
        Dec(LCounts[StepIndex(FConditions[LWire].Step)]);
      end;
    end;
  end;
end;

function TBuilder.Build: INyxFlowGraph;
var
  LGraph: TGraph;
begin

  if FAdmitting then
  begin
    raise ENyxFlow.Create('Graph builder admission cannot reenter');
  end;
  LGraph := TGraph.Create;
  Result := LGraph;
  FGraph.CopyInto(LGraph);
  LGraph.Seal;
end;

procedure TGraph.AdmitValue(const AValue: TNyxFlowValue; var ATotal: Integer);
var
  LBytes: Integer;
begin
  LBytes := NyxUTF8ByteCount(AValue.ToData.ToJSON);

  if LBytes > FLimits.MaximumValueBytes then
  begin
    raise ENyxFlow.Create('Graph value exceeds its UTF-8 byte budget');
  end;
  Inc(ATotal, LBytes);

  if ATotal > FLimits.MaximumRunBytes then
  begin
    raise ENyxFlow.Create('Graph invocation exceeds its total UTF-8 byte budget');
  end;
end;

procedure TGraph.ValidateInputs(const AInputs: array of TNyxFlowInput);
var
  LIndex: Integer;
  LPrevious: Integer;
  LStep: Integer;
  LPortIndex: Integer;
  LPort: TNyxFlowPort;
  LEndpoint: TNyxFlowEndpoint;
  LFound: Boolean;
  LTotal: Integer;
begin
  LTotal := 0;

  if Length(AInputs) > Length(FSteps) * 16 then
  begin
    raise ENyxFlow.Create('Graph invocation input capacity exceeded');
  end;
  for LIndex := 0 to High(AInputs) do
  begin
    LPort := Port(AInputs[LIndex].Endpoint);

    if (LPort.Direction <> nfdInput) or
      (LPort.ValueType <> AInputs[LIndex].Value.ValueType) or
      (Wired(AInputs[LIndex].Endpoint) >= 0) then
    begin
      raise ENyxFlow.Create('Invocation values require unwired inputs of the exact declared family');
    end;
    AdmitValue(AInputs[LIndex].Value, LTotal);
    for LPrevious := 0 to LIndex - 1 do
    begin

      if AInputs[LPrevious].Endpoint.Same(AInputs[LIndex].Endpoint) then
      begin
        raise ENyxFlow.Create('Invocation inputs must have distinct endpoints');
      end;
    end;
  end;
  for LStep := 0 to High(FSteps) do
  begin
    for LPortIndex := 0 to FSteps[LStep].Descriptor.PortCount - 1 do
    begin
      LPort := FSteps[LStep].Descriptor.Port(LPortIndex);

      if LPort.Direction <> nfdInput then
      begin
        Continue;
      end;
      LEndpoint := NyxFlowEndpoint(FSteps[LStep].Reference, LPort.Reference);

      if Wired(LEndpoint) >= 0 then
      begin
        Continue;
      end;
      LFound := False;
      for LIndex := 0 to High(AInputs) do
      begin
        LFound := LFound or AInputs[LIndex].Endpoint.Same(LEndpoint);
      end;

      if not LFound then
      begin
        raise ENyxFlow.Create('Required graph input is missing: ' +
          LEndpoint.Step.Name + ' / ' + LEndpoint.Port.Name);
      end;
    end;
  end;
end;

function TGraph.GetHasAsync: Boolean;
var
  LIndex: Integer;
  LAsync: INyxAsyncLogicOperation;
begin
  Result := False;
  for LIndex := 0 to High(FSteps) do
  begin

    if Supports(FSteps[LIndex].Operation, INyxAsyncLogicOperation, LAsync) then
    begin
      Exit(True);
    end;
  end;
end;

function TGraph.BeginEvaluation(const AInputs: array of TNyxFlowInput;
  const ACancellation: INyxCancellationScope): INyxFlowContinuation;
begin
  Result := TContinuation.Create(Self, AInputs, ACancellation);
end;

constructor TContinuation.Create(AGraph: TGraph; const AInputs: array of TNyxFlowInput;
  const ACancellation: INyxCancellationScope);
var
  LIndex: Integer;
begin
  inherited Create;
  FOwner := AGraph;
  FGraph := AGraph;
  FCancellation := ACancellation;

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxFlowCancelled.Create('Graph invocation cancelled');
  end;
  FGraph.ValidateInputs(AInputs);
  SetLength(FInputs, Length(AInputs));
  for LIndex := 0 to High(AInputs) do
  begin
    FInputs[LIndex] := NyxFlowInput(AInputs[LIndex].Endpoint, AInputs[LIndex].Value);
    FGraph.AdmitValue(FInputs[LIndex].Value, FTotal);
  end;
  SetLength(FFrames, Length(FGraph.FSteps));
  FResult := TResult.Create;
  FResultPort := FResult;
  SetLength(FResult.Steps, Length(FGraph.FSteps));
  SetLength(FResult.Descriptors, Length(FGraph.FSteps));
  SetLength(FResult.Skipped, Length(FGraph.FSteps));
  for LIndex := 0 to High(FGraph.FSteps) do
  begin
    FResult.Steps[LIndex] := FGraph.FSteps[LIndex].Reference;
    FResult.Descriptors[LIndex] := FGraph.FSteps[LIndex].Descriptor;
  end;
  AdvanceSkipped;
end;

procedure TContinuation.AdvanceSkipped;
var
  LStep: Integer;
  LIndex: Integer;
  LSource: Integer;
  LSkip: Boolean;
begin
  while not GetCompleted do
  begin
    LStep := FGraph.FOrder[FOrder];
    LSkip := False;
    for LIndex := 0 to High(FGraph.FConditions) do
    begin

      if FGraph.FConditions[LIndex].Step.Name = FGraph.FSteps[LStep].Reference.Name then
      begin
        LSource := FGraph.StepIndex(FGraph.FConditions[LIndex].Source.Step);
        LSkip := FResult.Skipped[LSource];

        if not LSkip then
        begin
          LSkip := FFrames[LSource].Value(FGraph.FConditions[LIndex].Source.Port).AsBoolean <>
            FGraph.FConditions[LIndex].Expected;
        end;
        Break;
      end;
    end;
    for LIndex := 0 to High(FGraph.FWires) do
    begin

      if FGraph.FWires[LIndex].Target.Step.Name = FGraph.FSteps[LStep].Reference.Name then
      begin
        LSource := FGraph.StepIndex(FGraph.FWires[LIndex].Source.Step);
        LSkip := LSkip or FResult.Skipped[LSource];
      end;
    end;

    if not LSkip then
    begin
      Exit;
    end;
    FResult.Skipped[LStep] := True;
    Inc(FOrder);
  end;
end;

function TContinuation.GetCompleted: Boolean;
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxFlowCancelled.Create('Graph invocation cancelled');
  end;
  Result := FOrder = Length(FGraph.FOrder);
end;

procedure TContinuation.RequireStep;
begin

  if GetCompleted then
  begin
    raise ENyxFlow.Create('Graph continuation has no pending step');
  end;
end;

function TContinuation.GetStep: TNyxFlowStepRef;
begin
  RequireStep;
  Result := FGraph.FSteps[FGraph.FOrder[FOrder]].Reference;
end;

function TContinuation.GetDescriptor: TNyxLogicDescriptor;
begin
  RequireStep;
  Result := TNyxLogicDescriptor.FromData(FGraph.FSteps[FGraph.FOrder[FOrder]].Descriptor.ToData);
end;

function TContinuation.GetOperation: INyxLogicOperation;
begin
  RequireStep;
  Result := FGraph.FSteps[FGraph.FOrder[FOrder]].Operation;
end;

function TContinuation.GetInputs: TNyxFlowValues;
var
  LStep: Integer;
  LPortIndex: Integer;
  LIndex: Integer;
  LWire: Integer;
  LSourceStep: Integer;
  LPort: TNyxFlowPort;
  LEndpoint: TNyxFlowEndpoint;
  LValue: TNyxFlowValue;
begin
  RequireStep;

  if not FReady then
  begin
    LStep := FGraph.FOrder[FOrder];
    FFrame := NyxFlowValues;
    for LPortIndex := 0 to FGraph.FSteps[LStep].Descriptor.PortCount - 1 do
    begin
      LPort := FGraph.FSteps[LStep].Descriptor.Port(LPortIndex);

      if LPort.Direction <> nfdInput then
      begin
        Continue;
      end;
      LEndpoint := NyxFlowEndpoint(GetStep, LPort.Reference);
      LWire := FGraph.Wired(LEndpoint);

      if LWire >= 0 then
      begin
        LSourceStep := FGraph.StepIndex(FGraph.FWires[LWire].Source.Step);
        LValue := FFrames[LSourceStep].Value(FGraph.FWires[LWire].Source.Port);
      end
      else
      begin
        for LIndex := 0 to High(FInputs) do
        begin

          if FInputs[LIndex].Endpoint.Same(LEndpoint) then
          begin
            LValue := FInputs[LIndex].Value;
            Break;
          end;
        end;
      end;
      FFrame := FFrame.WithValue(LPort.Reference, LValue);
    end;
    ValidateNyxLogicFrame(FGraph.FSteps[LStep].Descriptor, nfdInput, FFrame);
    FReady := True;
  end;
  Result := FFrame;
end;

procedure TContinuation.Accept(const AOutputs: TNyxFlowValues);
var
  LStep: Integer;
  LIndex: Integer;
  LTotal: Integer;
  LCount: Integer;
  LPrevious: Integer;
  LValues: TInputs;
begin
  RequireStep;

  if not FReady then
  begin
    raise ENyxFlow.Create('Read the exact pending input frame before admitting its result');
  end;
  LStep := FGraph.FOrder[FOrder];
  ValidateNyxLogicFrame(FGraph.FSteps[LStep].Descriptor, nfdOutput, AOutputs);
  LTotal := FTotal;
  LCount := AOutputs.Count;
  LPrevious := Length(FResult.Values);
  SetLength(LValues, LPrevious + LCount);
  for LIndex := 0 to LPrevious - 1 do
  begin
    LValues[LIndex] := FResult.Values[LIndex];
  end;
  for LIndex := 0 to LCount - 1 do
  begin
    FGraph.AdmitValue(AOutputs.Entry(LIndex).Value, LTotal);
    LValues[LPrevious + LIndex] := NyxFlowInput(
      NyxFlowEndpoint(GetStep, AOutputs.Entry(LIndex).Port), AOutputs.Entry(LIndex).Value);
  end;
  { Only complete schema/budget admission advances this invocation. Failed
    admission preserves its pending step and cannot expose a partial result. }
  FFrames[LStep] := AOutputs;
  FResult.Values := LValues;
  FTotal := LTotal;
  FFrame := NyxFlowValues;
  FReady := False;
  Inc(FOrder);
  AdvanceSkipped;
end;

function TContinuation.GetResult: INyxFlowResult;
begin

  if not GetCompleted then
  begin
    raise ENyxFlow.Create('Graph result is unavailable until every selected step succeeds');
  end;
  Result := FResultPort;
end;

function TGraph.Evaluate(const AInputs: array of TNyxFlowInput;
  const ACancellation: INyxCancellationScope): INyxFlowResult;
var
  LContinuation: INyxFlowContinuation;
  LStep: TNyxFlowStepRef;
  LInputs: TNyxFlowValues;
  LOutputs: TNyxFlowValues;
begin

  if GetHasAsync then
  begin
    raise ENyxFlow.Create('Asynchronous Logic requires scheduler-owned graph execution');
  end;
  LContinuation := BeginEvaluation(AInputs, ACancellation);
  while not LContinuation.Completed do
  begin
    LStep := LContinuation.Step;
    try
      LInputs := LContinuation.Inputs;
      LOutputs := LContinuation.Operation.Invoke(LInputs, ACancellation);
      LContinuation.Accept(LOutputs);
    except
      on LException: ENyxFlowCancelled do
      begin
        raise;
      end;
      on LException: Exception do
      begin
        raise ENyxFlow.Create('Logic step ' + LStep.Name + ': ' + TNyxText(LException.Message));
      end;
    end;
  end;
  Result := LContinuation.Result;
end;

function TResult.GetCount: Integer;
begin
  Result := Length(Values);
end;

function TResult.Entry(AIndex: Integer): TNyxFlowInput;
begin

  if (AIndex < 0) or (AIndex >= Length(Values)) then
  begin
    raise ENyxFlow.Create('Flow output index out of range');
  end;
  Result := Values[AIndex];
end;

function TResult.Value(const AEndpoint: TNyxFlowEndpoint): TNyxFlowValue;
var
  LIndex: Integer;
begin
  AEndpoint.Validate;
  for LIndex := 0 to High(Values) do
  begin

    if Values[LIndex].Endpoint.Same(AEndpoint) then
    begin
      Exit(Values[LIndex].Value);
    end;
  end;
  raise ENyxFlow.Create('Flow result does not contain the requested output');
end;

function TResult.WasSkipped(const AEndpoint: TNyxFlowEndpoint): Boolean;
var
  LIndex: Integer;
  LPort: TNyxFlowPort;
begin
  AEndpoint.Validate;
  for LIndex := 0 to High(Steps) do
  begin

    if Steps[LIndex].Name = AEndpoint.Step.Name then
    begin
      LPort := Descriptors[LIndex].Find(AEndpoint.Port);

      if LPort.Direction <> nfdOutput then
      begin
        raise ENyxFlow.Create('Skipped-output inspection requires a declared output');
      end;
      Exit(Skipped[LIndex]);
    end;
  end;
  raise ENyxFlow.Create('Skipped-output inspection requires a declared graph step');
end;

function EndpointData(const AEndpoint: TNyxFlowEndpoint): TNyxDataValue;
begin
  Result := NyxObject([NyxField('step', NyxData(AEndpoint.Step.Name)),
    NyxField('port', NyxData(AEndpoint.Port.Name))]);
end;

function ReadEndpoint(const AData: TNyxDataValue): TNyxFlowEndpoint;
begin
  ValidateNyxFlowFields(AData, ['step', 'port']);
  Result := NyxFlowEndpoint(NyxFlowStep(AData.Field('step').AsText),
    NyxFlowPort(AData.Field('port').AsText));
end;

function TGraph.ToData: TNyxDataValue;
var
  LSteps: array of TNyxDataValue;
  LWires: array of TNyxDataValue;
  LConditions: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LSteps, Length(FSteps));
  SetLength(LWires, Length(FWires));
  for LIndex := 0 to High(FSteps) do
  begin
    LSteps[LIndex] := NyxObject([NyxField('id', NyxData(FSteps[LIndex].Reference.Name)),
      NyxField('kind', NyxData(FSteps[LIndex].Descriptor.Kind.Name)),
      NyxField('descriptor', FSteps[LIndex].Descriptor.ToData),
      NyxField('configuration', FSteps[LIndex].Configuration)]);
  end;
  for LIndex := 0 to High(FWires) do
  begin
    LWires[LIndex] := NyxObject([NyxField('source', EndpointData(FWires[LIndex].Source)),
      NyxField('target', EndpointData(FWires[LIndex].Target))]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('limits', NyxArray([NyxData(FLimits.MaximumSteps), NyxData(FLimits.MaximumWires),
      NyxData(FLimits.MaximumValueBytes), NyxData(FLimits.MaximumRunBytes)])),
    NyxField('steps', NyxArray(LSteps)), NyxField('wires', NyxArray(LWires))]);

  if Length(FConditions) > 0 then
  begin
    SetLength(LConditions, Length(FConditions));
    for LIndex := 0 to High(FConditions) do
    begin
      LConditions[LIndex] := NyxObject([
        NyxField('step', NyxData(FConditions[LIndex].Step.Name)),
        NyxField('source', EndpointData(FConditions[LIndex].Source)),
        NyxField('expected', NyxData(FConditions[LIndex].Expected))]);
    end;
    Result := NyxObject([NyxField('version', NyxData(2)),
      NyxField('limits', Result.Field('limits')), NyxField('steps', Result.Field('steps')),
      NyxField('wires', Result.Field('wires')), NyxField('conditions', NyxArray(LConditions))]);
  end;
end;

function NyxFlowGraphFromData(const AData: TNyxDataValue;
  const AResolver: INyxLogicResolver): INyxFlowGraph;
var
  LLimits: TNyxFlowLimits;
  LData: TNyxDataValue;
  LSteps: TNyxDataValue;
  LWires: TNyxDataValue;
  LStep: TNyxDataValue;
  LKind: TNyxLogicKindRef;
  LOperation: INyxLogicOperation;
  LBuilder: INyxFlowBuilder;
  LIndex: Integer;
  LPrevious: Integer;
  LConditions: TNyxDataValue;
  LConditional: INyxFlowConditions;
begin

  if AData.Field('version').AsInteger = 2 then
  begin
    ValidateNyxFlowFields(AData, ['version', 'limits', 'steps', 'wires', 'conditions']);
  end
  else
  begin
    ValidateNyxFlowFields(AData, ['version', 'limits', 'steps', 'wires']);
  end;

  if not (AData.Field('version').AsInteger in [1, 2]) then
  begin
    raise ENyxFlow.Create('Unsupported executable graph version');
  end;
  LData := AData.Field('limits');

  if (LData.Kind <> ndArray) or (LData.Count <> 4) then
  begin
    raise ENyxFlow.Create('Graph limits require four explicit counts');
  end;
  { Every admitted value ceiling fits the default run ceiling; narrow/expand
    the final run budget only after its exact value limit is established. }
  LLimits := TNyxFlowLimits.Defaults.ValueBytes(LData.Item(2).AsInteger)
    .RunBytes(LData.Item(3).AsInteger).Steps(LData.Item(0).AsInteger)
    .Wires(LData.Item(1).AsInteger);
  LSteps := AData.Field('steps');
  LWires := AData.Field('wires');

  if (LSteps.Kind <> ndArray) or (LWires.Kind <> ndArray) or
    (LSteps.Count > LLimits.MaximumSteps) or (LWires.Count > LLimits.MaximumWires) then
  begin
    raise ENyxFlow.Create('Graph declarations exceed admitted capacities');
  end;
  LBuilder := NewNyxFlowGraph(LLimits);
  for LIndex := 0 to LSteps.Count - 1 do
  begin
    LStep := LSteps.Item(LIndex);
    ValidateNyxFlowFields(LStep, ['id', 'kind', 'descriptor', 'configuration']);
    LKind := NyxLogicKind(LStep.Field('kind').AsText);

    if AResolver = nil then
    begin
      LOperation := NyxLogicFromData(LKind, LStep.Field('configuration'));
    end
    else
    begin
      LOperation := AResolver.Resolve(LKind, LStep.Field('configuration'));
    end;

    if (LOperation = nil) or (LOperation.Descriptor.Kind.Name <> LKind.Name) or
      (LOperation.Descriptor.ToData.ToJSON <> LStep.Field('descriptor').ToJSON) or
      (LOperation.Configuration.ToJSON <> LStep.Field('configuration').ToJSON) then
    begin
      raise ENyxFlow.Create('Logic resolver must retain exact admitted kind/configuration');
    end;
    LBuilder.WithStep(NyxFlowStep(LStep.Field('id').AsText), LOperation);
  end;
  for LIndex := 0 to LWires.Count - 1 do
  begin
    LData := LWires.Item(LIndex);
    ValidateNyxFlowFields(LData, ['source', 'target']);
    LBuilder.Connect(ReadEndpoint(LData.Field('source')), ReadEndpoint(LData.Field('target')));
  end;

  if AData.Field('version').AsInteger = 2 then
  begin
    LConditions := AData.Field('conditions');

    if (LConditions.Kind <> ndArray) or (LConditions.Count = 0) or
      (LConditions.Count > LSteps.Count) then
    begin
      raise ENyxFlow.Create('Conditional graphs require one to step-count distinct conditions');
    end;
    LConditional := NyxFlowConditions(LBuilder.Build);
    for LIndex := 0 to LConditions.Count - 1 do
    begin
      LData := LConditions.Item(LIndex);
      ValidateNyxFlowFields(LData, ['step', 'source', 'expected']);
      for LPrevious := 0 to LIndex - 1 do
      begin

        if LConditions.Item(LPrevious).Field('step').AsText = LData.Field('step').AsText then
        begin
          raise ENyxFlow.Create('A persisted step condition must be unique');
        end;
      end;
      LConditional.When(NyxFlowStep(LData.Field('step').AsText),
        ReadEndpoint(LData.Field('source')), LData.Field('expected').AsBoolean);
    end;
    Exit(LConditional.Build);
  end;
  Result := LBuilder.Build;
end;

end.
