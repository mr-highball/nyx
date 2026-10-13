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

unit nyx.linq.reductions;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.linq, nyx.linq.numeric, nyx.linq.aggregates, nyx.linq.parallel,
  nyx.linq.pipeline, nyx.linq.execution, nyx.scheduler;

type
  { One independently owned final merger executes after every partition succeeds.
    Partials arrive in configured source/completion order. Merge must not read UI
    or mutable live stores; its execution context follows run/view cancellation.
    Associativity, floating-point rounding and mutable payload isolation remain
    explicit caller contracts. Empty input supplies an empty partial vector. }
  INyxParallelMerger<TPartial, TResult> = interface(IInterface)
    function Merge(const AValues: TNyxSequenceArray<TPartial>;
      const AExecution: INyxExecution): TResult;
  end;
  INyxParallelMergerFactory<TPartial, TResult> = interface(IInterface)
    { UI-only construction after complete successful partition replies. }
    function NewMerger: INyxParallelMerger<TPartial, TResult>;
  end;

  { Full bounded reduction. Capture reuses the ordinary coordinator. Each worker
    returns ONE complete partial, with O(partition-count) intermediate results.
    The final merge executes under the same selected worker/event-loop policy,
    then publishes ONE result on UI through the ordinary revocable result port.
    Stop/collect failures, source/capacity errors and cancellation publish nothing.
    Empty input enters no accumulator factory and merges once. Unsupported real
    workers refuse synchronously, including empty input. Nil dependencies/options
    refuse before capture; later factory/merge errors belong to the receipt. }
  TNyxParallelReduction<T, TPartial, TResult> = class
  public
    class function Submit(const AValues: array of T;
      const AAccumulators: INyxPartitionAccumulatorFactory<T, TPartial>;
      const AMergers: INyxParallelMergerFactory<TPartial, TResult>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
      const AOptions: TNyxParallelOptions;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; overload; static;
    class function Submit(const ASource: INyxSequence<T>;
      const AAccumulators: INyxPartitionAccumulatorFactory<T, TPartial>;
      const AMergers: INyxParallelMergerFactory<TPartial, TResult>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
      const AOptions: TNyxParallelOptions;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; overload; static;
  end;
  TNyxProgramReduction<TInput, TOutput, TPartial, TResult> = class
  public
    { Fused Where/Select/type-changing stages precede accumulation on each worker.
      Original input ordinals remain intact. Empty/rejected partitions still
      finish their independently owned seed. The program's options govern this
      run; the reusable program and supplied factories are never modified. }
    class function Submit(const AProgram: INyxParallelProgram<TInput, TOutput>;
      const AAccumulators: INyxPartitionAccumulatorFactory<TOutput, TPartial>;
      const AMergers: INyxParallelMergerFactory<TPartial, TResult>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; static;
  end;

  { UI-only fresh seed creation, once per partition and once for final merge.
    Mutable reference-valued seeds must be independent copies, or explicitly
    synchronized immutable state. The ordinary Value factory copies Pascal values;
    it neither deep-clones raw objects nor nested mutable payloads. }
  INyxReductionSeed<TAccumulator> = interface(IInterface)
    function NewSeed: TAccumulator;
  end;
  TNyxReductionSeedFunction<TAccumulator> = function: TAccumulator;
  TNyxReductionSeed<TAccumulator> = class
  public
    class function Value(const AValue: TAccumulator): INyxReductionSeed<TAccumulator>; static;
    class function From(AFunction: TNyxReductionSeedFunction<TAccumulator>):
      INyxReductionSeed<TAccumulator>; static;
  end;

  { Immutable seeded fold plan. Each partition owns a fresh folder and seed;
    final merge owns a fresh combiner and seed. Use an identity seed for the chosen
    associative combine law when partition-independent results are required.
    Partition grouping/order can affect nonassociative operations and overflow.
    Shared mutable callback/seed state is never implicitly cloned/synchronized.
    Existing owned/scoped/compiler-gated selector interfaces retain their contracts. }
  INyxParallelFold<T, TAccumulator> = interface(IInterface)
    function WithOptions(const AOptions: TNyxParallelOptions): INyxParallelFold<T, TAccumulator>;
    function Submit(const ASource: INyxSequence<T>; const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<TAccumulator>;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun;
    function GetAccumulators: INyxPartitionAccumulatorFactory<T, TAccumulator>;
    function GetMergers: INyxParallelMergerFactory<TAccumulator, TAccumulator>;
    function GetOptions: TNyxParallelOptions;
    property Accumulators: INyxPartitionAccumulatorFactory<T, TAccumulator> read GetAccumulators;
    property Mergers: INyxParallelMergerFactory<TAccumulator, TAccumulator> read GetMergers;
    property Options: TNyxParallelOptions read GetOptions;
  end;
  TNyxParallelFold<T, TAccumulator> = class
  public
    class function From(const ASeeds: INyxReductionSeed<TAccumulator>;
      const AFolders: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, T>, TAccumulator>;
      const ACombiners: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, TAccumulator>,
        TAccumulator>): INyxParallelFold<T, TAccumulator>; static;
    class function Seeded(const ASeed: TAccumulator;
      const AFolders: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, T>, TAccumulator>;
      const ACombiners: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, TAccumulator>,
        TAccumulator>): INyxParallelFold<T, TAccumulator>; static;
  end;
  TNyxParallelCounts<T> = class
  public
    { Exact signed-64-domain LongCount plan; no narrowing above 2^53. }
    class function LongCount: INyxParallelFold<T, TNyxQueryCount>; static;
  end;
  TNyxParallelNumeric = class
  public
    { Every partition/final Integer addition is explicitly checked on both targets.
      A partial overflow refuses even when another grouping could fit. }
    class function IntegerSum: INyxParallelFold<Integer, Integer>; static;
  end;

  { Compiler-visible owned support; application code uses the contracts above. }
  TCountCombineFactory = TNyxPartitionSelector<INyxFoldValue<TNyxQueryCount,
    TNyxQueryCount>, TNyxQueryCount>;
  TIntegerFoldFactory = TNyxPartitionSelector<INyxFoldValue<Integer, Integer>, Integer>;
  TReductionCompletion<TPartial, TResult> = class(TInterfacedObject,
    INyxPartitionCompletion<TPartial>)
  private
    FMergers: INyxParallelMergerFactory<TPartial, TResult>;
    FPort: INyxQueryResultPort<TResult>;
  public
    constructor Create(const AMergers: INyxParallelMergerFactory<TPartial, TResult>;
      const APort: INyxQueryResultPort<TResult>);
    function Prepare(const AValues: TNyxSequenceArray<TPartial>;
      const AScheduler: INyxScheduler; const AProgress: INyxQueryProgress): INyxWork;
  end;
  TReductionMerge<TPartial, TResult> = class(TInterfacedObject, INyxWork)
  private
    FValues: TNyxSequenceArray<TPartial>;
    FMerger: INyxParallelMerger<TPartial, TResult>;
    FScheduler: INyxScheduler;
    FPort: INyxQueryResultPort<TResult>;
    FProgress: INyxQueryProgress;
  public
    constructor Create(const AValues: TNyxSequenceArray<TPartial>;
      const AMerger: INyxParallelMerger<TPartial, TResult>; const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<TResult>; const AProgress: INyxQueryProgress);
    procedure Execute(const AExecution: INyxExecution);
  end;
  TProgramAccumulator<TInput, TOutput, TPartial> = class(TInterfacedObject,
    INyxPartitionAccumulator<TInput, TPartial>, INyxPartitionContinuation)
  private
    FProgram: INyxSelector<TInput, INyxOptional<TOutput>>;
    FAccumulator: INyxPartitionAccumulator<TOutput, TPartial>;
    FContinuation: INyxPartitionContinuation;
  public
    constructor Create(const AProgram: INyxSelector<TInput, INyxOptional<TOutput>>;
      const AAccumulator: INyxPartitionAccumulator<TOutput, TPartial>);
    procedure Add(const AValue: TInput; ASourceIndex: Integer; const AExecution: INyxExecution);
    function Finish(const AExecution: INyxExecution): TPartial;
    function ContinuePartition: Boolean;
  end;
  TProgramAccumulatorFactory<TInput, TOutput, TPartial> = class(TInterfacedObject,
    INyxPartitionAccumulatorFactory<TInput, TPartial>)
  private
    FProgram: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
    FAccumulators: INyxPartitionAccumulatorFactory<TOutput, TPartial>;
  public
    constructor Create(const AProgram: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
      const AAccumulators: INyxPartitionAccumulatorFactory<TOutput, TPartial>);
    function NewAccumulator(APartition, ASourceOffset: Integer):
      INyxPartitionAccumulator<TInput, TPartial>;
  end;
  TValueReductionSeed<T> = class(TInterfacedObject, INyxReductionSeed<T>)
  private
    FValue: T;
  public
    constructor Create(const AValue: T);
    function NewSeed: T;
  end;
  TFunctionReductionSeed<T> = class(TInterfacedObject, INyxReductionSeed<T>)
  private
    FFunction: TNyxReductionSeedFunction<T>;
  public
    constructor Create(AFunction: TNyxReductionSeedFunction<T>);
    function NewSeed: T;
  end;
  TFoldAccumulator<T, TAccumulator> = class(TInterfacedObject,
    INyxPartitionAccumulator<T, TAccumulator>)
  private
    FValue: TAccumulator;
    FFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>;
  public
    constructor Create(const ASeed: TAccumulator;
      const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>);
    procedure Add(const AValue: T; ASourceIndex: Integer; const AExecution: INyxExecution);
    function Finish(const AExecution: INyxExecution): TAccumulator;
  end;
  TFoldAccumulatorFactory<T, TAccumulator> = class(TInterfacedObject,
    INyxPartitionAccumulatorFactory<T, TAccumulator>)
  private
    FSeeds: INyxReductionSeed<TAccumulator>;
    FFolders: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, T>, TAccumulator>;
  public
    constructor Create(const ASeeds: INyxReductionSeed<TAccumulator>;
      const AFolders: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, T>, TAccumulator>);
    function NewAccumulator(APartition, ASourceOffset: Integer):
      INyxPartitionAccumulator<T, TAccumulator>;
  end;
  TFoldMerger<TAccumulator> = class(TInterfacedObject,
    INyxParallelMerger<TAccumulator, TAccumulator>)
  private
    FSeed: TAccumulator;
    FCombiner: INyxSelector<INyxFoldValue<TAccumulator, TAccumulator>, TAccumulator>;
  public
    constructor Create(const ASeed: TAccumulator;
      const ACombiner: INyxSelector<INyxFoldValue<TAccumulator, TAccumulator>, TAccumulator>);
    function Merge(const AValues: TNyxSequenceArray<TAccumulator>;
      const AExecution: INyxExecution): TAccumulator;
  end;
  TFoldMergerFactory<TAccumulator> = class(TInterfacedObject,
    INyxParallelMergerFactory<TAccumulator, TAccumulator>)
  private
    FSeeds: INyxReductionSeed<TAccumulator>;
    FCombiners: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, TAccumulator>, TAccumulator>;
  public
    constructor Create(const ASeeds: INyxReductionSeed<TAccumulator>;
      const ACombiners: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, TAccumulator>, TAccumulator>);
    function NewMerger: INyxParallelMerger<TAccumulator, TAccumulator>;
  end;
  TParallelFoldPlan<T, TAccumulator> = class(TInterfacedObject, INyxParallelFold<T, TAccumulator>)
  private
    FAccumulators: INyxPartitionAccumulatorFactory<T, TAccumulator>;
    FMergers: INyxParallelMergerFactory<TAccumulator, TAccumulator>;
    FOptions: TNyxParallelOptions;
  public
    constructor Create(const AAccumulators: INyxPartitionAccumulatorFactory<T, TAccumulator>;
      const AMergers: INyxParallelMergerFactory<TAccumulator, TAccumulator>;
      const AOptions: TNyxParallelOptions);
    function WithOptions(const AOptions: TNyxParallelOptions): INyxParallelFold<T, TAccumulator>;
    function Submit(const ASource: INyxSequence<T>; const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<TAccumulator>;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun;
    function GetAccumulators: INyxPartitionAccumulatorFactory<T, TAccumulator>;
    function GetMergers: INyxParallelMergerFactory<TAccumulator, TAccumulator>;
    function GetOptions: TNyxParallelOptions;
  end;
  TCountReductionFolder<T> = class(TInterfacedObject,
    INyxSelector<INyxFoldValue<TNyxQueryCount, T>, TNyxQueryCount>)
    function Project(const AValue: INyxFoldValue<TNyxQueryCount, T>): TNyxQueryCount;
  end;
  TCountReductionFactory<T> = class(TInterfacedObject,
    INyxPartitionSelectorFactory<INyxFoldValue<TNyxQueryCount, T>, TNyxQueryCount>)
    function NewSelector(APartition, ASourceOffset: Integer):
      INyxSelector<INyxFoldValue<TNyxQueryCount, T>, TNyxQueryCount>;
  end;

{ Compiler-visible numeric callbacks used by expanded factory templates. }
function CombineCount(const AValue: INyxFoldValue<TNyxQueryCount, TNyxQueryCount>): TNyxQueryCount;
function AddInteger(const AValue: INyxFoldValue<Integer, Integer>): Integer;

implementation

constructor TReductionCompletion<TPartial, TResult>.Create(
  const AMergers: INyxParallelMergerFactory<TPartial, TResult>;
  const APort: INyxQueryResultPort<TResult>);
begin
  inherited Create;
  FMergers := AMergers;
  FPort := APort;
end;

function TReductionCompletion<TPartial, TResult>.Prepare(
  const AValues: TNyxSequenceArray<TPartial>; const AScheduler: INyxScheduler;
  const AProgress: INyxQueryProgress): INyxWork;
var
  LMerger: INyxParallelMerger<TPartial, TResult>;
begin
  AScheduler.RequireUI;
  LMerger := FMergers.NewMerger;

  if LMerger = nil then
  begin
    raise ENyxQuery.Create('Reduction factory returned no merger');
  end;
  Result := TReductionMerge<TPartial, TResult>.Create(AValues, LMerger, AScheduler, FPort, AProgress);
end;

constructor TReductionMerge<TPartial, TResult>.Create(
  const AValues: TNyxSequenceArray<TPartial>; const AMerger: INyxParallelMerger<TPartial, TResult>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
  const AProgress: INyxQueryProgress);
var
  LIndex: Integer;
begin
  inherited Create;
  SetLength(FValues, Length(AValues));
  for LIndex := 0 to High(FValues) do
  begin
    FValues[LIndex] := AValues[LIndex];
  end;
  FMerger := AMerger;
  FScheduler := AScheduler;
  FPort := APort;
  FProgress := AProgress;
end;

procedure TReductionMerge<TPartial, TResult>.Execute(const AExecution: INyxExecution);
var
  LValues: TNyxSequenceArray<TResult>;
  LContext: INyxExecution;
  LDelivery: INyxWork;
  LExecution: INyxExecution;
begin
  try

    if FProgress.IsCancelled or AExecution.Cancelled then
    begin
      FProgress.Cancel;
      Exit;
    end;
    LContext := NewNyxQueryExecution(AExecution, FProgress);
    SetLength(LValues, 1);
    LValues[0] := FMerger.Merge(FValues, LContext);
    FValues := nil;
    FMerger := nil;

    if FProgress.IsCancelled or AExecution.Cancelled then
    begin
      FProgress.Cancel;
      Exit;
    end;
    FProgress.AwaitUI;
    LDelivery := TNyxQueryDelivery<TResult>.Create(LValues, FPort, FProgress);
    LExecution := FScheduler.PostUI(LDelivery, AExecution);
    FProgress.Attach(LExecution, True);
  except
    on LException: Exception do
    begin
      FProgress.Fail(NyxQueryDiagnostic(LException));
      FProgress.Cancel;
    end;
  end;
end;

class function TNyxParallelReduction<T, TPartial, TResult>.Submit(const AValues: array of T;
  const AAccumulators: INyxPartitionAccumulatorFactory<T, TPartial>;
  const AMergers: INyxParallelMergerFactory<TPartial, TResult>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<TResult>; const AOptions: TNyxParallelOptions;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
var
  LProgress: INyxQueryProgress;
  LDiagnostics: INyxPartitionDiagnostics;
  LCompletion: INyxPartitionCompletion<TPartial>;
  LWork: INyxWork;
  LExecution: INyxExecution;
begin
  AOptions.Validate;

  if (AAccumulators = nil) or (AMergers = nil) or (AScheduler = nil) or (APort = nil) then
  begin
    raise ENyxQuery.Create('Reduction requires accumulator/merger factories, scheduler and result port');
  end;

  if Length(AValues) > AOptions.InputLimit then
  begin
    raise ENyxQuery.Create('Reduction input exceeds its configured limit');
  end;
  AScheduler.Admit(neUIQueue);

  if AOptions.ExecutionMode = npxWorkers then
  begin
    AScheduler.Admit(neThreaded);
  end;
  LProgress := NewNyxQueryProgress(AGeneration);
  LDiagnostics := NewNyxPartitionDiagnostics;
  LCompletion := TReductionCompletion<TPartial, TResult>.Create(AMergers, APort);
  LWork := TNyxPartitionCoordinator<T, TPartial>.Create(AValues, nil, AScheduler, nil,
    LProgress, LDiagnostics, AOptions, nil, AAccumulators, LCompletion);
  LExecution := AScheduler.Submit(LWork, neUIQueue);
  LProgress.Attach(LExecution, False);
  Result := NewNyxParallelRun(LProgress, LDiagnostics);
end;

class function TNyxParallelReduction<T, TPartial, TResult>.Submit(const ASource: INyxSequence<T>;
  const AAccumulators: INyxPartitionAccumulatorFactory<T, TPartial>;
  const AMergers: INyxParallelMergerFactory<TPartial, TResult>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<TResult>; const AOptions: TNyxParallelOptions;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
var
  LProgress: INyxQueryProgress;
  LDiagnostics: INyxPartitionDiagnostics;
  LCompletion: INyxPartitionCompletion<TPartial>;
  LWork: INyxWork;
  LExecution: INyxExecution;
begin
  AOptions.Validate;

  if (ASource = nil) or (AAccumulators = nil) or (AMergers = nil) or
    (AScheduler = nil) or (APort = nil) then
  begin
    raise ENyxQuery.Create('Deferred reduction requires source, factories, scheduler and result port');
  end;
  AScheduler.Admit(neUIQueue);

  if AOptions.ExecutionMode = npxWorkers then
  begin
    AScheduler.Admit(neThreaded);
  end;
  LProgress := NewNyxQueryProgress(AGeneration);
  LDiagnostics := NewNyxPartitionDiagnostics;
  LCompletion := TReductionCompletion<TPartial, TResult>.Create(AMergers, APort);
  LWork := TNyxPartitionCapture<T, TPartial>.Create(ASource, nil, AScheduler, nil,
    LProgress, LDiagnostics, AOptions, nil, AAccumulators, LCompletion);
  LExecution := AScheduler.Submit(LWork, neUIQueue);
  LProgress.Attach(LExecution, False);
  Result := NewNyxParallelRun(LProgress, LDiagnostics);
end;

constructor TProgramAccumulator<TInput, TOutput, TPartial>.Create(
  const AProgram: INyxSelector<TInput, INyxOptional<TOutput>>;
  const AAccumulator: INyxPartitionAccumulator<TOutput, TPartial>);
begin
  inherited Create;
  FProgram := AProgram;
  FAccumulator := AAccumulator;
  { Capability discovery belongs to partition construction, not every row.
    This second managed reference has no reverse link to its wrapper. }
  Supports(FAccumulator, INyxPartitionContinuation, FContinuation);
end;

procedure TProgramAccumulator<TInput, TOutput, TPartial>.Add(const AValue: TInput;
  ASourceIndex: Integer; const AExecution: INyxExecution);
var
  LValue: INyxOptional<TOutput>;
begin
  LValue := FProgram.Project(AValue);

  if LValue.HasValue and not AExecution.Cancelled then
  begin
    FAccumulator.Add(LValue.Value, ASourceIndex, AExecution);
  end;
end;

function TProgramAccumulator<TInput, TOutput, TPartial>.Finish(
  const AExecution: INyxExecution): TPartial;
begin
  Result := FAccumulator.Finish(AExecution);
end;

function TProgramAccumulator<TInput, TOutput, TPartial>.ContinuePartition: Boolean;
begin
  Result := True;

  if FContinuation <> nil then
  begin
    Result := FContinuation.ContinuePartition;
  end;
end;

constructor TProgramAccumulatorFactory<TInput, TOutput, TPartial>.Create(
  const AProgram: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
  const AAccumulators: INyxPartitionAccumulatorFactory<TOutput, TPartial>);
begin
  inherited Create;
  FProgram := AProgram;
  FAccumulators := AAccumulators;
end;

function TProgramAccumulatorFactory<TInput, TOutput, TPartial>.NewAccumulator(
  APartition, ASourceOffset: Integer): INyxPartitionAccumulator<TInput, TPartial>;
var
  LProgram: INyxSelector<TInput, INyxOptional<TOutput>>;
  LAccumulator: INyxPartitionAccumulator<TOutput, TPartial>;
begin
  LProgram := FProgram.NewSelector(APartition, ASourceOffset);
  LAccumulator := FAccumulators.NewAccumulator(APartition, ASourceOffset);

  if (LProgram = nil) or (LAccumulator = nil) then
  begin
    raise ENyxQuery.Create('Program reduction factory returned an absent callback');
  end;
  Result := TProgramAccumulator<TInput, TOutput, TPartial>.Create(LProgram, LAccumulator);
end;

class function TNyxProgramReduction<TInput, TOutput, TPartial, TResult>.Submit(
  const AProgram: INyxParallelProgram<TInput, TOutput>;
  const AAccumulators: INyxPartitionAccumulatorFactory<TOutput, TPartial>;
  const AMergers: INyxParallelMergerFactory<TPartial, TResult>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
var
  LFactory: INyxPartitionAccumulatorFactory<TInput, TPartial>;
begin

  if (AProgram = nil) or (AAccumulators = nil) then
  begin
    raise ENyxQuery.Create('Program reduction requires a program and accumulator factory');
  end;
  LFactory := TProgramAccumulatorFactory<TInput, TOutput, TPartial>.Create(AProgram.Factory, AAccumulators);
  Result := TNyxParallelReduction<TInput, TPartial, TResult>.Submit(AProgram.Source,
    LFactory, AMergers, AScheduler, APort, AProgram.Options, AGeneration);
end;

constructor TValueReductionSeed<T>.Create(const AValue: T);
begin
  inherited Create;
  FValue := AValue;
end;

function TValueReductionSeed<T>.NewSeed: T;
begin
  Result := FValue;
end;

constructor TFunctionReductionSeed<T>.Create(AFunction: TNyxReductionSeedFunction<T>);
begin
  inherited Create;

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('Reduction seed requires a function');
  end;
  FFunction := AFunction;
end;

function TFunctionReductionSeed<T>.NewSeed: T;
begin
  Result := FFunction();
end;

class function TNyxReductionSeed<TAccumulator>.Value(const AValue: TAccumulator):
  INyxReductionSeed<TAccumulator>;
begin
  Result := TValueReductionSeed<TAccumulator>.Create(AValue);
end;

class function TNyxReductionSeed<TAccumulator>.From(AFunction: TNyxReductionSeedFunction<TAccumulator>):
  INyxReductionSeed<TAccumulator>;
begin
  Result := TFunctionReductionSeed<TAccumulator>.Create(AFunction);
end;

constructor TFoldAccumulator<T, TAccumulator>.Create(const ASeed: TAccumulator;
  const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>);
begin
  inherited Create;
  FValue := ASeed;
  FFolder := AFolder;
end;

procedure TFoldAccumulator<T, TAccumulator>.Add(const AValue: T; ASourceIndex: Integer;
  const AExecution: INyxExecution);
var
  LContext: INyxFoldValue<TAccumulator, T>;
begin
  LContext := TNyxFoldValue<TAccumulator, T>.Create(FValue, AValue);
  FValue := FFolder.Project(LContext);
end;

function TFoldAccumulator<T, TAccumulator>.Finish(const AExecution: INyxExecution): TAccumulator;
begin
  Result := FValue;
end;

constructor TFoldAccumulatorFactory<T, TAccumulator>.Create(
  const ASeeds: INyxReductionSeed<TAccumulator>;
  const AFolders: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, T>, TAccumulator>);
begin
  inherited Create;
  FSeeds := ASeeds;
  FFolders := AFolders;
end;

function TFoldAccumulatorFactory<T, TAccumulator>.NewAccumulator(APartition, ASourceOffset: Integer):
  INyxPartitionAccumulator<T, TAccumulator>;
var
  LSeed: TAccumulator;
  LFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>;
begin
  LSeed := FSeeds.NewSeed;
  LFolder := FFolders.NewSelector(APartition, ASourceOffset);

  if LFolder = nil then
  begin
    raise ENyxQuery.Create('Reduction folder factory returned no callback');
  end;
  Result := TFoldAccumulator<T, TAccumulator>.Create(LSeed, LFolder);
end;

constructor TFoldMerger<TAccumulator>.Create(const ASeed: TAccumulator;
  const ACombiner: INyxSelector<INyxFoldValue<TAccumulator, TAccumulator>, TAccumulator>);
begin
  inherited Create;
  FSeed := ASeed;
  FCombiner := ACombiner;
end;

function TFoldMerger<TAccumulator>.Merge(const AValues: TNyxSequenceArray<TAccumulator>;
  const AExecution: INyxExecution): TAccumulator;
var
  LIndex: Integer;
  LContext: INyxFoldValue<TAccumulator, TAccumulator>;
begin
  Result := FSeed;
  for LIndex := 0 to High(AValues) do
  begin

    if AExecution.Cancelled then
    begin
      Exit;
    end;
    LContext := TNyxFoldValue<TAccumulator, TAccumulator>.Create(Result, AValues[LIndex]);
    Result := FCombiner.Project(LContext);
  end;
end;

constructor TFoldMergerFactory<TAccumulator>.Create(const ASeeds: INyxReductionSeed<TAccumulator>;
  const ACombiners: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, TAccumulator>, TAccumulator>);
begin
  inherited Create;
  FSeeds := ASeeds;
  FCombiners := ACombiners;
end;

function TFoldMergerFactory<TAccumulator>.NewMerger:
  INyxParallelMerger<TAccumulator, TAccumulator>;
var
  LSeed: TAccumulator;
  LCombiner: INyxSelector<INyxFoldValue<TAccumulator, TAccumulator>, TAccumulator>;
begin
  LSeed := FSeeds.NewSeed;
  { The single merge stage has its own callback chain, identified as (0,0). }
  LCombiner := FCombiners.NewSelector(0, 0);

  if LCombiner = nil then
  begin
    raise ENyxQuery.Create('Reduction combiner factory returned no callback');
  end;
  Result := TFoldMerger<TAccumulator>.Create(LSeed, LCombiner);
end;

constructor TParallelFoldPlan<T, TAccumulator>.Create(
  const AAccumulators: INyxPartitionAccumulatorFactory<T, TAccumulator>;
  const AMergers: INyxParallelMergerFactory<TAccumulator, TAccumulator>;
  const AOptions: TNyxParallelOptions);
begin
  inherited Create;
  FAccumulators := AAccumulators;
  FMergers := AMergers;
  FOptions := AOptions;
end;

function TParallelFoldPlan<T, TAccumulator>.WithOptions(const AOptions: TNyxParallelOptions):
  INyxParallelFold<T, TAccumulator>;
begin
  AOptions.Validate;
  Result := TParallelFoldPlan<T, TAccumulator>.Create(FAccumulators, FMergers, AOptions);
end;

function TParallelFoldPlan<T, TAccumulator>.Submit(const ASource: INyxSequence<T>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TAccumulator>;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
begin
  Result := TNyxParallelReduction<T, TAccumulator, TAccumulator>.Submit(ASource,
    FAccumulators, FMergers, AScheduler, APort, FOptions, AGeneration);
end;

function TParallelFoldPlan<T, TAccumulator>.GetAccumulators:
  INyxPartitionAccumulatorFactory<T, TAccumulator>;
begin
  Result := FAccumulators;
end;

function TParallelFoldPlan<T, TAccumulator>.GetMergers:
  INyxParallelMergerFactory<TAccumulator, TAccumulator>;
begin
  Result := FMergers;
end;

function TParallelFoldPlan<T, TAccumulator>.GetOptions: TNyxParallelOptions;
begin
  Result := FOptions;
end;

class function TNyxParallelFold<T, TAccumulator>.From(
  const ASeeds: INyxReductionSeed<TAccumulator>;
  const AFolders: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, T>, TAccumulator>;
  const ACombiners: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, TAccumulator>, TAccumulator>):
  INyxParallelFold<T, TAccumulator>;
var
  LAccumulators: INyxPartitionAccumulatorFactory<T, TAccumulator>;
  LMergers: INyxParallelMergerFactory<TAccumulator, TAccumulator>;
begin

  if (ASeeds = nil) or (AFolders = nil) or (ACombiners = nil) then
  begin
    raise ENyxQuery.Create('Parallel fold requires seed, folder and combiner factories');
  end;
  LAccumulators := TFoldAccumulatorFactory<T, TAccumulator>.Create(ASeeds, AFolders);
  LMergers := TFoldMergerFactory<TAccumulator>.Create(ASeeds, ACombiners);
  Result := TParallelFoldPlan<T, TAccumulator>.Create(LAccumulators, LMergers, TNyxParallelOptions.Defaults);
end;

class function TNyxParallelFold<T, TAccumulator>.Seeded(const ASeed: TAccumulator;
  const AFolders: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, T>, TAccumulator>;
  const ACombiners: INyxPartitionSelectorFactory<INyxFoldValue<TAccumulator, TAccumulator>, TAccumulator>):
  INyxParallelFold<T, TAccumulator>;
begin
  Result := From(TNyxReductionSeed<TAccumulator>.Value(ASeed), AFolders, ACombiners);
end;

function TCountReductionFolder<T>.Project(const AValue: INyxFoldValue<TNyxQueryCount, T>): TNyxQueryCount;
begin
  Result := AValue.Accumulator.Incremented;
end;

function TCountReductionFactory<T>.NewSelector(APartition, ASourceOffset: Integer):
  INyxSelector<INyxFoldValue<TNyxQueryCount, T>, TNyxQueryCount>;
begin
  Result := TCountReductionFolder<T>.Create;
end;

function CombineCount(const AValue: INyxFoldValue<TNyxQueryCount, TNyxQueryCount>): TNyxQueryCount;
begin

  if AValue = nil then
  begin
    raise ENyxQuery.Create('Count combination requires a fold context');
  end;
  Result := AValue.Accumulator.Added(AValue.Value);
end;

class function TNyxParallelCounts<T>.LongCount: INyxParallelFold<T, TNyxQueryCount>;
begin
  Result := TNyxParallelFold<T, TNyxQueryCount>.Seeded(NyxQueryCount(0),
    TCountReductionFactory<T>.Create,
    TCountCombineFactory.From(CombineCount));
end;

function AddInteger(const AValue: INyxFoldValue<Integer, Integer>): Integer;
var
  LLeft: Integer;
  LRight: Integer;
begin

  if AValue = nil then
  begin
    raise ENyxQuery.Create('Integer reduction requires a fold context');
  end;
  LLeft := AValue.Accumulator;
  LRight := AValue.Value;

  if LRight > 0 then
  begin

    if LLeft > High(Integer) - LRight then
    begin
      raise ENyxQuery.Create('Parallel Integer aggregate overflow');
    end;
  end
  else if LRight < 0 then
  begin

    if LLeft < Low(Integer) - LRight then
    begin
      raise ENyxQuery.Create('Parallel Integer aggregate overflow');
    end;
  end;
  Result := LLeft + LRight;
end;

class function TNyxParallelNumeric.IntegerSum: INyxParallelFold<Integer, Integer>;
begin
  Result := TNyxParallelFold<Integer, Integer>.Seeded(0,
    TIntegerFoldFactory.From(AddInteger),
    TIntegerFoldFactory.From(AddInteger));
end;

end.
