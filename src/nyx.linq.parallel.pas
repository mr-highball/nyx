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
unit nyx.linq.parallel;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

interface

uses
  SysUtils, nyx.text, nyx.linq, nyx.linq.execution, nyx.scheduler, nyx.callback.scope;

type
  { Workers requires actual native threads, even for an empty input. DeferredUI
    explicitly chooses browser/native event-loop work, never a thread simulation.
    Source order preserves every input ordinal. Completion order preserves order
    inside each partition, but concatenates partitions as their UI replies arrive.
    Stop cancels siblings at the first observed partition failure. Collect runs
    other partitions and retains one diagnostic per failed partition; the failing
    partition stops at its first failing row. Neither policy publishes partial data. }
  TNyxParallelExecution = (npxWorkers, npxDeferredUI);
  TNyxParallelOrder = (npoSource, npoCompletion);
  TNyxParallelFailurePolicy = (npfStop, npfCollect);
  TNyxParallelOptions = record
  private
    FConcurrency: Integer;
    FPartitionSize: Integer;
    FMaximumInput: Integer;
    FCaptureRows: Integer;
    FExecution: TNyxParallelExecution;
    FOrder: TNyxParallelOrder;
    FFailures: TNyxParallelFailurePolicy;
  public
    { Four in-flight partitions, 128 rows each, at most 1,048,576 input rows,
      required workers, source order and stop-on-first-observed failure. }
    class function Defaults: TNyxParallelOptions; static;
    function Concurrency(ACount: Integer): TNyxParallelOptions;
    function PartitionSize(ACount: Integer): TNyxParallelOptions;
    function MaximumInput(ACount: Integer): TNyxParallelOptions;
    { Deferred sequence submission moves at most this many output rows on each
      UI turn (1..65536, default 256). A caller's MoveNext/preparation itself
      cannot be preempted; expensive source callbacks retain their own contract. }
    function CaptureRowsPerTurn(ACount: Integer): TNyxParallelOptions;
    function Execution(AMode: TNyxParallelExecution): TNyxParallelOptions;
    function Order(AMode: TNyxParallelOrder): TNyxParallelOptions;
    function Failures(APolicy: TNyxParallelFailurePolicy): TNyxParallelOptions;
    procedure Validate;
    property ConcurrentPartitions: Integer read FConcurrency;
    property RowsPerPartition: Integer read FPartitionSize;
    property InputLimit: Integer read FMaximumInput;
    property CaptureQuantum: Integer read FCaptureRows;
    property ExecutionMode: TNyxParallelExecution read FExecution;
    property ResultOrder: TNyxParallelOrder read FOrder;
    property FailurePolicy: TNyxParallelFailurePolicy read FFailures;
  end;

  { An owned action executes on the chosen worker/event-loop policy, with its
    original zero-based input ordinal and revocable query execution context.
    Never touch live UI/stores from workers. PostUI with this context as parent
    and a generation-protected receiver supplies safe UI handoff. Cancellation
    cannot undo effects already committed by an entered action. }
  INyxParallelAction<T> = interface(IInterface)
    procedure Invoke(const AValue: T; ASourceIndex: Integer;
      const AExecution: INyxExecution);
  end;
  TNyxParallelActionProcedure<T> = procedure(const AValue: T; ASourceIndex: Integer;
    const AExecution: INyxExecution);
  TNyxParallelActionNested<T> = procedure(const AValue: T; ASourceIndex: Integer;
    const AExecution: INyxExecution) {$ifndef PAS2JS}is nested{$endif};
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  TNyxParallelActionReference<T> = reference to procedure(const AValue: T;
    ASourceIndex: Integer; const AExecution: INyxExecution);
  {$endif}
  { UI factory creates independent action state per partition, or explicitly
    synchronized shared state. Retained interfaces alone do not make mutable
    objects thread-safe. Nil factories/actions refuse with ordinary diagnostics. }
  INyxPartitionActionFactory<T> = interface(IInterface)
    function NewAction(APartition, ASourceOffset: Integer): INyxParallelAction<T>;
  end;
  TNyxPartitionAction<T> = class
  public
    { Global procedure adapter creates a fresh owned action per partition.
      Application implementations can provide any reference-counted base class. }
    class function From(AProcedure: TNyxParallelActionProcedure<T>):
      INyxPartitionActionFactory<T>; static;
    { Borrowed stack frame: construction, factory entry and EVERY action entry
      check the origin-thread lease before dereferencing its static link. Close
      in finally before leaving the frame. Native worker entry explicitly refuses. }
    class function FromNested(AProcedure: TNyxParallelActionNested<T>;
      const ALease: INyxCallbackLease): INyxPartitionActionFactory<T>; static;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    { Qualified owning reference. Wrappers are independent, captured closure
      state is shared and must be immutable/synchronized for worker execution.
      Pass NyxCapture for interface captures on BOTH targets, including pas2js. }
    class function FromInline(const AProcedure: TNyxParallelActionReference<T>;
      const ACapture: INyxCallbackCapture = nil): INyxPartitionActionFactory<T>; static;
    {$endif}
  end;

  { Factory entry is on UI, once per admitted partition. Each returned managed
    selector owns independent mutable state or explicitly shared synchronized/
    immutable state. It must outlive its work and must not access live UI/stores.
    Sharing a raw class or captured interface does not make that object safe.
    SourceOffset identifies the partition's first zero-based input ordinal. }
  INyxPartitionSelectorFactory<TSource, TResult> = interface(IInterface)
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<TSource, TResult>;
  end;
  TNyxPartitionSelector<TSource, TResult> = class
  public
    { Global pure functions need no borrowed frame. A fresh adapter is created for
      each partition; thread safety of global data remains the caller's contract. }
    class function From(AFunction: TNyxSelectorFunction<TSource, TResult>):
      INyxPartitionSelectorFactory<TSource, TResult>; static;
  end;

  TNyxPartitionFailure = record
    Partition: Integer;
    SourceOffset: Integer;
    Message: TNyxText;
  end;
  { Diagnostics own exact text and outlive all work/UI. Failures appear in UI
    observation order, independent of result order. Getters are native synchronized;
    FailureAt refuses an absent index. Cancellation/failed delivery may have no
    partition diagnostic; the ordinary Phase/Failure still explain that outcome. }
  INyxParallelRun = interface(INyxQueryRun)
    ['{D7BDA0B4-2636-46BD-9401-24B52DAA91A3}']
    function GetFailureCount: Integer;
    function FailureAt(AIndex: Integer): TNyxPartitionFailure;
    property FailureCount: Integer read GetFailureCount;
  end;

  { Capture the outer input vector synchronously on UI, then defer admission of
    selector partitions. Managed elements follow ordinary Pascal ownership: raw
    class pointers and mutable inner arrays are not deep cloned. Worker inputs
    must be independently owned immutable values. An explicit query.ToArray may
    supply the capture; this factory never shares a mutable query enumerator.
    Concurrency is an upper bound on in-flight partitions, also limited by the
    shared scheduler's actual pool. No thread is created/joined by this layer.
    Capacity/factory failures after submission fail the retained receipt and stop
    siblings; there is no silent UI fallback/retry. Callbacks already entered may
    finish, so pure selectors are recommended. Result delivery is once on UI via
    the same revocable/revision-aware ports as ordinary queries. No partial result
    reaches the destination under either failure policy. Cancellation is checked
    between rows and cannot undo a callback/receiver that has already entered.
    Optional AGeneration links both input overloads to the current owning view
    before capture/partition admission. Retired views suppress not-yet-entered
    factories, later rows and UI delivery. nil preserves independent background
    work; capture the mounted event router's generation for page-owned work. }
  TNyxParallelSelect<TSource, TResult> = class
  public
    class function Submit(const AValues: array of TSource;
      const AFactory: INyxPartitionSelectorFactory<TSource, TResult>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
      const AOptions: TNyxParallelOptions;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; overload; static;
    { Retain without evaluating the deferred source during Submit. One owned
      cursor is captured on UI across bounded output-row turns. No partition
      factory enters until EOF and full input-limit admission; source failures,
      cancellation and expired scoped callbacks yield no partial worker work.
      Each submission enumerates independently. Source shaping is serial on UI;
      the admitted projection partitions use the selected execution mode. }
    class function Submit(const ASequence: INyxSequence<TSource>;
      const AFactory: INyxPartitionSelectorFactory<TSource, TResult>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
      const AOptions: TNyxParallelOptions;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; overload; static;
  end;

  { Terminal action execution uses the same bounded capture/partition/scheduler
    policies as projection, without allocating partition or merged result vectors.
    Actions commit independently; failure/cancellation never rolls them back.
    Stop suppresses later admission after the first observed failure; Collect
    continues other partitions, stopping each failed partition at its first error.
    Succeeded means all actions completed and UI observed all partition replies;
    separately posted child work retains its own execution/result diagnostics.
    Result order does not serialize side effects: callbacks within a partition
    enter in input order; order across concurrent partitions is unspecified.
    Explicit DeferredUI works on both targets. Workers still requires real native
    threads, even for empty input. Optional generation follows the owning page.
    Invalid arguments/options/capabilities refuse synchronously. Later factory,
    missing-action and source failures belong to the retained receipt; no action
    starts before complete bounded source admission. Posted child work needs its
    own observed executions when its completion is required by the application. }
  TNyxParallelForAll<T> = class
  public
    class function Submit(const AValues: array of T;
      const AFactory: INyxPartitionActionFactory<T>; const AScheduler: INyxScheduler;
      const AOptions: TNyxParallelOptions;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; overload; static;
    class function Submit(const ASequence: INyxSequence<T>;
      const AFactory: INyxPartitionActionFactory<T>; const AScheduler: INyxScheduler;
      const AOptions: TNyxParallelOptions;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; overload; static;
  end;

  { A partition owns one mutable accumulator, never shared between workers.
    Add receives the original input ordinal and query cancellation context.
    Finish returns an independently owned complete partial state, once, after
    all rows succeed. Neither callback may access live UI/collections. }
  INyxPartitionAccumulator<T, TPartial> = interface(IInterface)
    procedure Add(const AValue: T; ASourceIndex: Integer; const AExecution: INyxExecution);
    function Finish(const AExecution: INyxExecution): TPartial;
  end;
  INyxPartitionAccumulatorFactory<T, TPartial> = interface(IInterface)
    { UI-only creation; unknown/nil accumulators refuse before partition entry. }
    function NewAccumulator(APartition, ASourceOffset: Integer):
      INyxPartitionAccumulator<T, TPartial>;
  end;

  { Internal typed completion extension used by reductions. Prepare runs on UI,
    after ALL successful replies and before admission of the returned owned work.
    It must retain/copy values independently and return no borrowed work/handles.
    Reduction implementations compute the final merge under the selected policy
    and then queue one complete result through ordinary revocable delivery. }
  INyxPartitionCompletion<TPartial> = interface(IInterface)
    function Prepare(const AValues: TNyxSequenceArray<TPartial>;
      const AScheduler: INyxScheduler; const AProgress: INyxQueryProgress): INyxWork;
  end;

  { Compiler-visible support contracts/types for native generic expansion. No
    application, renderer or collection handle crosses the worker completion. }
  INyxPartitionDiagnostics = interface
    ['{DA7819C1-757A-43BE-889C-EAE6DCA92AA9}']
    procedure Add(APartition, ASourceOffset: Integer; const AMessage: TNyxText);
    function Count: Integer;
    function Item(AIndex: Integer): TNyxPartitionFailure;
  end;
  INyxPartitionSink<TResult> = interface(IInterface)
    procedure Completed(APartition, ASourceOffset: Integer;
      const AValues: TNyxSequenceArray<TResult>; AFailed: Boolean;
      const AFailure: TNyxText);
  end;
  TNyxGlobalPartitionFactory<TSource, TResult> = class(TInterfacedObject,
    INyxPartitionSelectorFactory<TSource, TResult>)
  private
    FFunction: TNyxSelectorFunction<TSource, TResult>;
  public
    constructor Create(AFunction: TNyxSelectorFunction<TSource, TResult>);
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<TSource, TResult>;
  end;
  TNyxScopedParallelAction<T> = class(TInterfacedObject, INyxParallelAction<T>)
  private
    FProcedure: TNyxParallelActionNested<T>;
    FLifetime: INyxCallbackLifetime;
  public
    constructor Create(AProcedure: TNyxParallelActionNested<T>;
      const ALifetime: INyxCallbackLifetime);
    procedure Invoke(const AValue: T; ASourceIndex: Integer; const AExecution: INyxExecution);
  end;
  TNyxScopedActionFactory<T> = class(TInterfacedObject, INyxPartitionActionFactory<T>)
  private
    FProcedure: TNyxParallelActionNested<T>;
    FLifetime: INyxCallbackLifetime;
  public
    constructor Create(AProcedure: TNyxParallelActionNested<T>; const ALease: INyxCallbackLease);
    function NewAction(APartition, ASourceOffset: Integer): INyxParallelAction<T>;
  end;
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  TNyxInlineParallelAction<T> = class(TInterfacedObject, INyxParallelAction<T>)
  private
    FProcedure: TNyxParallelActionReference<T>;
    FCapture: INyxCallbackCapture;
  public
    constructor Create(const AProcedure: TNyxParallelActionReference<T>;
      const ACapture: INyxCallbackCapture);
    procedure Invoke(const AValue: T; ASourceIndex: Integer; const AExecution: INyxExecution);
  end;
  TNyxInlineActionFactory<T> = class(TInterfacedObject, INyxPartitionActionFactory<T>)
  private
    FProcedure: TNyxParallelActionReference<T>;
    FCapture: INyxCallbackCapture;
  public
    constructor Create(const AProcedure: TNyxParallelActionReference<T>;
      const ACapture: INyxCallbackCapture);
    function NewAction(APartition, ASourceOffset: Integer): INyxParallelAction<T>;
  end;
  {$endif}
  TNyxGlobalParallelAction<T> = class(TInterfacedObject, INyxParallelAction<T>)
  private
    FProcedure: TNyxParallelActionProcedure<T>;
  public
    constructor Create(AProcedure: TNyxParallelActionProcedure<T>);
    procedure Invoke(const AValue: T; ASourceIndex: Integer;
      const AExecution: INyxExecution);
  end;
  TNyxGlobalActionFactory<T> = class(TInterfacedObject, INyxPartitionActionFactory<T>)
  private
    FProcedure: TNyxParallelActionProcedure<T>;
  public
    constructor Create(AProcedure: TNyxParallelActionProcedure<T>);
    function NewAction(APartition, ASourceOffset: Integer): INyxParallelAction<T>;
  end;
  TNyxPartitionReply<TResult> = class(TInterfacedObject, INyxWork)
  private
    FSink: INyxPartitionSink<TResult>;
    FProgress: INyxQueryProgress;
    FValues: TNyxSequenceArray<TResult>;
    FPartition: Integer;
    FOffset: Integer;
    FFailed: Boolean;
    FFailure: TNyxText;
  public
    constructor Create(const ASink: INyxPartitionSink<TResult>; const AProgress: INyxQueryProgress;
      APartition, AOffset: Integer; const AValues: TNyxSequenceArray<TResult>;
      AFailed: Boolean; const AFailure: TNyxText);
    procedure Execute(const AExecution: INyxExecution);
  end;
  TNyxPartitionWork<TSource, TResult> = class(TInterfacedObject, INyxWork)
  private
    FValues: TNyxSequenceArray<TSource>;
    FSelector: INyxSelector<TSource, TResult>;
    FAction: INyxParallelAction<TSource>;
    FAccumulator: INyxPartitionAccumulator<TSource, TResult>;
    FScheduler: INyxScheduler;
    FSink: INyxPartitionSink<TResult>;
    FProgress: INyxQueryProgress;
    FPartition: Integer;
    FOffset: Integer;
  public
    constructor Create(const AValues: array of TSource;
      const ASelector: INyxSelector<TSource, TResult>; const AScheduler: INyxScheduler;
      const ASink: INyxPartitionSink<TResult>; const AProgress: INyxQueryProgress;
      APartition, AOffset: Integer; const AAction: INyxParallelAction<TSource> = nil;
      const AAccumulator: INyxPartitionAccumulator<TSource, TResult> = nil);
    procedure Execute(const AExecution: INyxExecution);
  end;
  TNyxPartitionCoordinator<TSource, TResult> = class(TInterfacedObject,
    INyxWork, INyxPartitionSink<TResult>)
  private
    FInput: TNyxSequenceArray<TSource>;
    FResults: TNyxSequenceArray<TResult>;
    FFactory: INyxPartitionSelectorFactory<TSource, TResult>;
    FActionFactory: INyxPartitionActionFactory<TSource>;
    FAccumulatorFactory: INyxPartitionAccumulatorFactory<TSource, TResult>;
    FCompletion: INyxPartitionCompletion<TResult>;
    FScheduler: INyxScheduler;
    FPort: INyxQueryResultPort<TResult>;
    FProgress: INyxQueryProgress;
    FDiagnostics: INyxPartitionDiagnostics;
    FOptions: TNyxParallelOptions;
    FNext: Integer;
    FPartition: Integer;
    FActive: Integer;
    FWritten: Integer;
    FFinished: Boolean;
    procedure Launch;
    procedure Complete;
  public
    constructor Create(const AValues: array of TSource;
      const AFactory: INyxPartitionSelectorFactory<TSource, TResult>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
      const AProgress: INyxQueryProgress; const ADiagnostics: INyxPartitionDiagnostics;
      const AOptions: TNyxParallelOptions;
      const AActionFactory: INyxPartitionActionFactory<TSource> = nil;
      const AAccumulatorFactory: INyxPartitionAccumulatorFactory<TSource, TResult> = nil;
      const ACompletion: INyxPartitionCompletion<TResult> = nil);
    procedure Execute(const AExecution: INyxExecution);
    procedure Completed(APartition, ASourceOffset: Integer;
      const AValues: TNyxSequenceArray<TResult>; AFailed: Boolean; const AFailure: TNyxText);
  end;
  TNyxPartitionCapture<TSource, TResult> = class(TInterfacedObject, INyxWork)
  private
    FSequence: INyxSequence<TSource>;
    FTurns: INyxSchedulerTurns;
    FCursor: INyxEnumerator<TSource>;
    FValues: TNyxSequenceArray<TSource>;
    FCount: Integer;
    FCapacity: Integer;
    FFactory: INyxPartitionSelectorFactory<TSource, TResult>;
    FActionFactory: INyxPartitionActionFactory<TSource>;
    FAccumulatorFactory: INyxPartitionAccumulatorFactory<TSource, TResult>;
    FCompletion: INyxPartitionCompletion<TResult>;
    FScheduler: INyxScheduler;
    FPort: INyxQueryResultPort<TResult>;
    FProgress: INyxQueryProgress;
    FDiagnostics: INyxPartitionDiagnostics;
    FOptions: TNyxParallelOptions;
    procedure AdmitRow;
    procedure BeginPartitions;
  public
    constructor Create(const ASequence: INyxSequence<TSource>;
      const AFactory: INyxPartitionSelectorFactory<TSource, TResult>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
      const AProgress: INyxQueryProgress; const ADiagnostics: INyxPartitionDiagnostics;
      const AOptions: TNyxParallelOptions;
      const AActionFactory: INyxPartitionActionFactory<TSource> = nil;
      const AAccumulatorFactory: INyxPartitionAccumulatorFactory<TSource, TResult> = nil;
      const ACompletion: INyxPartitionCompletion<TResult> = nil);
    procedure Execute(const AExecution: INyxExecution);
  end;

function NewNyxPartitionDiagnostics: INyxPartitionDiagnostics;
function NewNyxParallelRun(const AProgress: INyxQueryProgress;
  const ADiagnostics: INyxPartitionDiagnostics): INyxParallelRun;

implementation

type
  TPartitionDiagnostics = class(TInterfacedObject, INyxPartitionDiagnostics)
  private
    FItems: array of TNyxPartitionFailure;
    {$ifndef PAS2JS}FLock: TRTLCriticalSection;{$endif}
    procedure Lock;
    procedure Unlock;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(APartition, ASourceOffset: Integer; const AMessage: TNyxText);
    function Count: Integer;
    function Item(AIndex: Integer): TNyxPartitionFailure;
  end;
  TParallelRun = class(TInterfacedObject, INyxParallelRun, INyxQueryRun)
  private
    FProgress: INyxQueryProgress;
    FDiagnostics: INyxPartitionDiagnostics;
  public
    constructor Create(const AProgress: INyxQueryProgress; const ADiagnostics: INyxPartitionDiagnostics);
    procedure Cancel;
    function GetPhase: TNyxQueryPhase;
    function GetFailure: TNyxText;
    function GetFailureCount: Integer;
    function FailureAt(AIndex: Integer): TNyxPartitionFailure;
  end;

class function TNyxParallelOptions.Defaults: TNyxParallelOptions;
begin
  Result.FConcurrency := 4;
  Result.FPartitionSize := 128;
  Result.FMaximumInput := 1048576;
  Result.FCaptureRows := 256;
  Result.FExecution := npxWorkers;
  Result.FOrder := npoSource;
  Result.FFailures := npfStop;
end;

function TNyxParallelOptions.Concurrency(ACount: Integer): TNyxParallelOptions;
begin

  if (ACount < 1) or (ACount > 64) then
  begin
    raise ENyxQuery.Create('Parallel concurrency must be between 1 and 64');
  end;
  Result := Self;
  Result.FConcurrency := ACount;
end;

function TNyxParallelOptions.PartitionSize(ACount: Integer): TNyxParallelOptions;
begin

  if (ACount < 1) or (ACount > 65536) then
  begin
    raise ENyxQuery.Create('Partition size must be between 1 and 65536');
  end;
  Result := Self;
  Result.FPartitionSize := ACount;
end;

function TNyxParallelOptions.MaximumInput(ACount: Integer): TNyxParallelOptions;
begin

  if (ACount < 1) or (ACount > 16777216) then
  begin
    raise ENyxQuery.Create('Parallel input limit must be between 1 and 16777216');
  end;
  Result := Self;
  Result.FMaximumInput := ACount;
end;

function TNyxParallelOptions.Execution(AMode: TNyxParallelExecution): TNyxParallelOptions;
begin
  Result := Self;
  Result.FExecution := AMode;
end;

function TNyxParallelOptions.CaptureRowsPerTurn(ACount: Integer): TNyxParallelOptions;
begin

  if (ACount < 1) or (ACount > 65536) then
  begin
    raise ENyxQuery.Create('Capture rows per turn must be between 1 and 65536');
  end;
  Result := Self;
  Result.FCaptureRows := ACount;
end;

function TNyxParallelOptions.Order(AMode: TNyxParallelOrder): TNyxParallelOptions;
begin
  Result := Self;
  Result.FOrder := AMode;
end;

function TNyxParallelOptions.Failures(APolicy: TNyxParallelFailurePolicy): TNyxParallelOptions;
begin
  Result := Self;
  Result.FFailures := APolicy;
end;

procedure TNyxParallelOptions.Validate;
begin

  if (FConcurrency < 1) or (FConcurrency > 64) or
    (FPartitionSize < 1) or (FPartitionSize > 65536) or
    (FMaximumInput < 1) or (FMaximumInput > 16777216) or
    (FCaptureRows < 1) or (FCaptureRows > 65536) then
  begin
    raise ENyxQuery.Create('Use initialized valid parallel options or Defaults');
  end;
end;

constructor TPartitionDiagnostics.Create;
begin
  inherited Create;
  {$ifndef PAS2JS}InitCriticalSection(FLock);{$endif}
end;

destructor TPartitionDiagnostics.Destroy;
begin
  {$ifndef PAS2JS}DoneCriticalSection(FLock);{$endif}
  inherited Destroy;
end;

procedure TPartitionDiagnostics.Lock;
begin
  {$ifndef PAS2JS}EnterCriticalSection(FLock);{$endif}
end;

procedure TPartitionDiagnostics.Unlock;
begin
  {$ifndef PAS2JS}LeaveCriticalSection(FLock);{$endif}
end;

procedure TPartitionDiagnostics.Add(APartition, ASourceOffset: Integer; const AMessage: TNyxText);
var
  LIndex: Integer;
begin
  Lock;
  try
    LIndex := Length(FItems);
    SetLength(FItems, LIndex + 1);
    FItems[LIndex].Partition := APartition;
    FItems[LIndex].SourceOffset := ASourceOffset;
    FItems[LIndex].Message := AMessage;
  finally
    Unlock;
  end;
end;

function TPartitionDiagnostics.Count: Integer;
begin
  Lock;
  try
    Result := Length(FItems);
  finally
    Unlock;
  end;
end;

function TPartitionDiagnostics.Item(AIndex: Integer): TNyxPartitionFailure;
begin
  Lock;
  try

    if (AIndex < 0) or (AIndex >= Length(FItems)) then
    begin
      raise ENyxQuery.Create('Partition failure index is absent');
    end;
    Result := FItems[AIndex];
  finally
    Unlock;
  end;
end;

constructor TParallelRun.Create(const AProgress: INyxQueryProgress;
  const ADiagnostics: INyxPartitionDiagnostics);
begin
  inherited Create;
  FProgress := AProgress;
  FDiagnostics := ADiagnostics;
end;

procedure TParallelRun.Cancel;
begin
  FProgress.Cancel;
end;

function TParallelRun.GetPhase: TNyxQueryPhase;
begin
  Result := FProgress.Phase;
end;

function TParallelRun.GetFailure: TNyxText;
begin
  Result := FProgress.Failure;
end;

function TParallelRun.GetFailureCount: Integer;
begin
  Result := FDiagnostics.Count;
end;

function TParallelRun.FailureAt(AIndex: Integer): TNyxPartitionFailure;
begin
  Result := FDiagnostics.Item(AIndex);
end;

function NewNyxPartitionDiagnostics: INyxPartitionDiagnostics;
begin
  Result := TPartitionDiagnostics.Create;
end;

function NewNyxParallelRun(const AProgress: INyxQueryProgress;
  const ADiagnostics: INyxPartitionDiagnostics): INyxParallelRun;
begin
  Result := TParallelRun.Create(AProgress, ADiagnostics);
end;

constructor TNyxGlobalPartitionFactory<TSource, TResult>.Create(
  AFunction: TNyxSelectorFunction<TSource, TResult>);
begin
  inherited Create;

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('A partition selector function is required');
  end;
  FFunction := AFunction;
end;

function TNyxGlobalPartitionFactory<TSource, TResult>.NewSelector(APartition,
  ASourceOffset: Integer): INyxSelector<TSource, TResult>;
begin
  Result := TNyxSelector<TSource, TResult>.From(FFunction);
end;

class function TNyxPartitionSelector<TSource, TResult>.From(
  AFunction: TNyxSelectorFunction<TSource, TResult>): INyxPartitionSelectorFactory<TSource, TResult>;
begin
  Result := TNyxGlobalPartitionFactory<TSource, TResult>.Create(AFunction);
end;

constructor TNyxScopedParallelAction<T>.Create(AProcedure: TNyxParallelActionNested<T>;
  const ALifetime: INyxCallbackLifetime);
begin
  inherited Create;

  if not Assigned(AProcedure) or (ALifetime = nil) then
  begin
    raise ENyxQuery.Create('Scoped parallel action requires a procedure and lifetime');
  end;
  ALifetime.RequireEntry;
  FProcedure := AProcedure;
  FLifetime := ALifetime;
end;

procedure TNyxScopedParallelAction<T>.Invoke(const AValue: T; ASourceIndex: Integer;
  const AExecution: INyxExecution);
begin
  FLifetime.RequireEntry;
  FProcedure(AValue, ASourceIndex, AExecution);
end;

constructor TNyxScopedActionFactory<T>.Create(AProcedure: TNyxParallelActionNested<T>;
  const ALease: INyxCallbackLease);
begin
  inherited Create;

  if not Assigned(AProcedure) or (ALease = nil) then
  begin
    raise ENyxQuery.Create('Nested parallel action requires a procedure and lease');
  end;
  FLifetime := ALease.Lifetime;
  FLifetime.RequireEntry;
  FProcedure := AProcedure;
end;

function TNyxScopedActionFactory<T>.NewAction(APartition, ASourceOffset: Integer):
  INyxParallelAction<T>;
begin
  FLifetime.RequireEntry;
  Result := TNyxScopedParallelAction<T>.Create(FProcedure, FLifetime);
end;

class function TNyxPartitionAction<T>.FromNested(AProcedure: TNyxParallelActionNested<T>;
  const ALease: INyxCallbackLease): INyxPartitionActionFactory<T>;
begin
  Result := TNyxScopedActionFactory<T>.Create(AProcedure, ALease);
end;

{$ifdef NYX_HAS_INLINE_CALLBACKS}
constructor TNyxInlineParallelAction<T>.Create(const AProcedure: TNyxParallelActionReference<T>;
  const ACapture: INyxCallbackCapture);
begin
  inherited Create;

  if not Assigned(AProcedure) then
  begin
    raise ENyxQuery.Create('Inline parallel action requires a procedure');
  end;
  FProcedure := AProcedure;
  FCapture := ACapture;
end;

procedure TNyxInlineParallelAction<T>.Invoke(const AValue: T; ASourceIndex: Integer;
  const AExecution: INyxExecution);
begin
  FProcedure(AValue, ASourceIndex, AExecution);
end;

constructor TNyxInlineActionFactory<T>.Create(const AProcedure: TNyxParallelActionReference<T>;
  const ACapture: INyxCallbackCapture);
begin
  inherited Create;

  if not Assigned(AProcedure) then
  begin
    raise ENyxQuery.Create('Inline parallel action requires a procedure');
  end;
  FProcedure := AProcedure;
  FCapture := ACapture;
end;

function TNyxInlineActionFactory<T>.NewAction(APartition, ASourceOffset: Integer):
  INyxParallelAction<T>;
begin
  Result := TNyxInlineParallelAction<T>.Create(FProcedure, FCapture);
end;

class function TNyxPartitionAction<T>.FromInline(const AProcedure: TNyxParallelActionReference<T>;
  const ACapture: INyxCallbackCapture): INyxPartitionActionFactory<T>;
begin
  Result := TNyxInlineActionFactory<T>.Create(AProcedure, ACapture);
end;
{$endif}

constructor TNyxGlobalParallelAction<T>.Create(AProcedure: TNyxParallelActionProcedure<T>);
begin
  inherited Create;

  if not Assigned(AProcedure) then
  begin
    raise ENyxQuery.Create('Parallel action requires a procedure');
  end;
  FProcedure := AProcedure;
end;

procedure TNyxGlobalParallelAction<T>.Invoke(const AValue: T; ASourceIndex: Integer;
  const AExecution: INyxExecution);
begin
  FProcedure(AValue, ASourceIndex, AExecution);
end;

constructor TNyxGlobalActionFactory<T>.Create(AProcedure: TNyxParallelActionProcedure<T>);
begin
  inherited Create;

  if not Assigned(AProcedure) then
  begin
    raise ENyxQuery.Create('Parallel action requires a procedure');
  end;
  FProcedure := AProcedure;
end;

function TNyxGlobalActionFactory<T>.NewAction(APartition, ASourceOffset: Integer):
  INyxParallelAction<T>;
begin
  Result := TNyxGlobalParallelAction<T>.Create(FProcedure);
end;

class function TNyxPartitionAction<T>.From(AProcedure: TNyxParallelActionProcedure<T>):
  INyxPartitionActionFactory<T>;
begin
  Result := TNyxGlobalActionFactory<T>.Create(AProcedure);
end;

constructor TNyxPartitionReply<TResult>.Create(const ASink: INyxPartitionSink<TResult>;
  const AProgress: INyxQueryProgress; APartition, AOffset: Integer;
  const AValues: TNyxSequenceArray<TResult>; AFailed: Boolean; const AFailure: TNyxText);
var
  LIndex: Integer;
begin
  inherited Create;
  FSink := ASink;
  FProgress := AProgress;
  FPartition := APartition;
  FOffset := AOffset;
  FFailed := AFailed;
  FFailure := AFailure;
  SetLength(FValues, Length(AValues));
  for LIndex := 0 to High(FValues) do
  begin
    FValues[LIndex] := AValues[LIndex];
  end;
end;

procedure TNyxPartitionReply<TResult>.Execute(const AExecution: INyxExecution);
begin

  if FProgress.IsCancelled or AExecution.Cancelled then
  begin
    FProgress.Cancel;
    Exit;
  end;
  try
    FSink.Completed(FPartition, FOffset, FValues, FFailed, FFailure);
  except
    on LException: Exception do
    begin
      FProgress.Fail(NyxQueryDiagnostic(LException));
      FProgress.Cancel;
    end;
  end;
end;

constructor TNyxPartitionWork<TSource, TResult>.Create(const AValues: array of TSource;
  const ASelector: INyxSelector<TSource, TResult>; const AScheduler: INyxScheduler;
  const ASink: INyxPartitionSink<TResult>; const AProgress: INyxQueryProgress;
  APartition, AOffset: Integer; const AAction: INyxParallelAction<TSource>;
  const AAccumulator: INyxPartitionAccumulator<TSource, TResult>);
var
  LIndex: Integer;
begin
  inherited Create;
  SetLength(FValues, Length(AValues));
  for LIndex := 0 to High(FValues) do
  begin
    FValues[LIndex] := AValues[LIndex];
  end;
  FSelector := ASelector;
  FAction := AAction;
  FAccumulator := AAccumulator;
  FScheduler := AScheduler;
  FSink := ASink;
  FProgress := AProgress;
  FPartition := APartition;
  FOffset := AOffset;
end;

procedure TNyxPartitionWork<TSource, TResult>.Execute(const AExecution: INyxExecution);
var
  LValues: TNyxSequenceArray<TResult>;
  LIndex: Integer;
  LFailed: Boolean;
  LFailure: TNyxText;
  LReply: INyxWork;
  LExecution: INyxExecution;
  LContext: INyxExecution;
begin
  try
    LFailed := False;
    LFailure := '';
    try

      if (FAction = nil) and (FAccumulator = nil) then
      begin
        SetLength(LValues, Length(FValues));
      end
      else
      begin
        LContext := NewNyxQueryExecution(AExecution, FProgress);
      end;
      for LIndex := 0 to High(FValues) do
      begin

        if FProgress.IsCancelled or AExecution.Cancelled then
        begin
          FProgress.Cancel;
          Exit;
        end;

        if FAccumulator <> nil then
        begin
          FAccumulator.Add(FValues[LIndex], FOffset + LIndex, LContext);
        end
        else if FAction = nil then
        begin
          LValues[LIndex] := FSelector.Project(FValues[LIndex]);
        end
        else
        begin
          FAction.Invoke(FValues[LIndex], FOffset + LIndex, LContext);
        end;
      end;

      if FAccumulator <> nil then
      begin

        if FProgress.IsCancelled or AExecution.Cancelled then
        begin
          FProgress.Cancel;
          Exit;
        end;
        SetLength(LValues, 1);
        LValues[0] := FAccumulator.Finish(LContext);
      end;
    except
      on LException: Exception do
      begin
        LFailed := True;
        LFailure := NyxQueryDiagnostic(LException);
        LValues := nil;
      end;
    end;

    if FProgress.IsCancelled or AExecution.Cancelled then
    begin
      FProgress.Cancel;
      Exit;
    end;
    { Release the partition callback/input before its UI completion can schedule
      a replacement. Workers retain no live collection or target control. }
    FValues := nil;
    FSelector := nil;
    FAction := nil;
    FAccumulator := nil;
    LContext := nil;
    LReply := TNyxPartitionReply<TResult>.Create(FSink, FProgress, FPartition,
      FOffset, LValues, LFailed, LFailure);
    LExecution := FScheduler.PostUI(LReply, AExecution);
    FProgress.Attach(LExecution, False);
  except
    on LException: Exception do
    begin
      FProgress.Fail(NyxQueryDiagnostic(LException));
      FProgress.Cancel;
    end;
  end;
end;

constructor TNyxPartitionCoordinator<TSource, TResult>.Create(const AValues: array of TSource;
  const AFactory: INyxPartitionSelectorFactory<TSource, TResult>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
  const AProgress: INyxQueryProgress; const ADiagnostics: INyxPartitionDiagnostics;
  const AOptions: TNyxParallelOptions;
  const AActionFactory: INyxPartitionActionFactory<TSource>;
  const AAccumulatorFactory: INyxPartitionAccumulatorFactory<TSource, TResult>;
  const ACompletion: INyxPartitionCompletion<TResult>);
var
  LIndex: Integer;
begin
  inherited Create;
  SetLength(FInput, Length(AValues));

  if AAccumulatorFactory <> nil then
  begin
    { Reduction storage grows with partitions, never output rows. This formula
      handles empty input without a negative numerator or an extra partition. }
    LIndex := Length(AValues) div AOptions.RowsPerPartition;

    if Length(AValues) mod AOptions.RowsPerPartition <> 0 then
    begin
      Inc(LIndex);
    end;
    SetLength(FResults, LIndex);
  end
  else if AActionFactory = nil then
  begin
    SetLength(FResults, Length(AValues));
  end;
  for LIndex := 0 to High(FInput) do
  begin
    FInput[LIndex] := AValues[LIndex];
  end;
  FFactory := AFactory;
  FActionFactory := AActionFactory;
  FAccumulatorFactory := AAccumulatorFactory;
  FCompletion := ACompletion;
  FScheduler := AScheduler;
  FPort := APort;
  FProgress := AProgress;
  FDiagnostics := ADiagnostics;
  FOptions := AOptions;
end;

procedure TNyxPartitionCoordinator<TSource, TResult>.Launch;
var
  LCount, LIndex: Integer;
  LValues: TNyxSequenceArray<TSource>;
  LSelector: INyxSelector<TSource, TResult>;
  LAction: INyxParallelAction<TSource>;
  LAccumulator: INyxPartitionAccumulator<TSource, TResult>;
  LWork: INyxWork;
  LExecution: INyxExecution;
  LPolicy: TNyxExecutionPolicy;
begin
  FScheduler.RequireUI;
  LPolicy := neThreaded;

  if FOptions.ExecutionMode = npxDeferredUI then
  begin
    LPolicy := neUIQueue;
  end;
  while (FNext < Length(FInput)) and (FActive < FOptions.ConcurrentPartitions)
    and not FProgress.IsCancelled do
  begin
    LCount := Length(FInput) - FNext;

    if LCount > FOptions.RowsPerPartition then
    begin
      LCount := FOptions.RowsPerPartition;
    end;
    LSelector := nil;
    LAction := nil;
    LAccumulator := nil;

    if FAccumulatorFactory <> nil then
    begin
      LAccumulator := FAccumulatorFactory.NewAccumulator(FPartition, FNext);

      if LAccumulator = nil then
      begin
        raise ENyxQuery.Create('Partition factory returned no accumulator');
      end;
    end
    else if FActionFactory = nil then
    begin
      LSelector := FFactory.NewSelector(FPartition, FNext);

      if LSelector = nil then
      begin
        raise ENyxQuery.Create('Partition factory returned no selector');
      end;
    end
    else
    begin
      LAction := FActionFactory.NewAction(FPartition, FNext);

      if LAction = nil then
      begin
        raise ENyxQuery.Create('Partition factory returned no action');
      end;
    end;
    SetLength(LValues, LCount);
    for LIndex := 0 to LCount - 1 do
    begin
      LValues[LIndex] := FInput[FNext + LIndex];
    end;
    LWork := TNyxPartitionWork<TSource, TResult>.Create(LValues, LSelector,
      FScheduler, Self, FProgress, FPartition, FNext, LAction, LAccumulator);
    LExecution := FScheduler.Submit(LWork, LPolicy);
    FProgress.Attach(LExecution, False);
    Inc(FActive);
    Inc(FPartition);
    Inc(FNext, LCount);
  end;
end;

procedure TNyxPartitionCoordinator<TSource, TResult>.Complete;
var
  LDelivery: INyxWork;
  LExecution: INyxExecution;
  LPolicy: TNyxExecutionPolicy;
begin

  if FFinished or (FNext <> Length(FInput)) or (FActive <> 0) then
  begin
    Exit;
  end;
  FFinished := True;

  if FDiagnostics.Count <> 0 then
  begin
    FProgress.Fail(FDiagnostics.Item(0).Message);
    Exit;
  end;
  FInput := nil;
  FFactory := nil;

  if FAccumulatorFactory <> nil then
  begin
    FAccumulatorFactory := nil;

    if FCompletion = nil then
    begin
      raise ENyxQuery.Create('Reduction requires a completion factory');
    end;
    LDelivery := FCompletion.Prepare(FResults, FScheduler, FProgress);

    if LDelivery = nil then
    begin
      raise ENyxQuery.Create('Reduction completion returned no work');
    end;
    FCompletion := nil;
    FResults := nil;
    LPolicy := neThreaded;

    if FOptions.ExecutionMode = npxDeferredUI then
    begin
      LPolicy := neUIQueue;
    end;
    LExecution := FScheduler.Submit(LDelivery, LPolicy);
    FProgress.Attach(LExecution, False);
    Exit;
  end;

  if FActionFactory <> nil then
  begin
    FActionFactory := nil;
    FProgress.Finish(True);
    Exit;
  end;
  FProgress.AwaitUI;
  LDelivery := TNyxQueryDelivery<TResult>.Create(FResults, FPort, FProgress);
  LExecution := FScheduler.PostUI(LDelivery);
  FProgress.Attach(LExecution, True);
end;

procedure TNyxPartitionCoordinator<TSource, TResult>.Execute(const AExecution: INyxExecution);
begin
  try

    if FProgress.IsCancelled or AExecution.Cancelled then
    begin
      FProgress.Cancel;
      Exit;
    end;
    FProgress.BeginCompute;
    Launch;
    Complete;
  except
    on LException: Exception do
    begin
      FProgress.Fail(NyxQueryDiagnostic(LException));
      FProgress.Cancel;
    end;
  end;
end;

procedure TNyxPartitionCoordinator<TSource, TResult>.Completed(APartition,
  ASourceOffset: Integer; const AValues: TNyxSequenceArray<TResult>;
  AFailed: Boolean; const AFailure: TNyxText);
var
  LIndex, LOffset: Integer;
begin
  FScheduler.RequireUI;
  Dec(FActive);

  if FProgress.IsCancelled then
  begin
    Exit;
  end;

  if AFailed then
  begin
    FDiagnostics.Add(APartition, ASourceOffset, AFailure);

    if FOptions.FailurePolicy = npfStop then
    begin
      FProgress.Fail(AFailure);
      FProgress.Cancel;
      Exit;
    end;
  end
  else if FActionFactory = nil then
  begin
    LOffset := ASourceOffset;

    if FAccumulatorFactory <> nil then
    begin

      if Length(AValues) <> 1 then
      begin
        raise ENyxQuery.Create('Reduction partition returned an invalid partial count');
      end;
      LOffset := APartition;
    end;

    if FOptions.ResultOrder = npoCompletion then
    begin
      LOffset := FWritten;
    end;
    for LIndex := 0 to High(AValues) do
    begin
      FResults[LOffset + LIndex] := AValues[LIndex];
    end;
    Inc(FWritten, Length(AValues));
  end;
  Launch;
  Complete;
end;

class function TNyxParallelSelect<TSource, TResult>.Submit(const AValues: array of TSource;
  const AFactory: INyxPartitionSelectorFactory<TSource, TResult>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
  const AOptions: TNyxParallelOptions;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
var
  LProgress: INyxQueryProgress;
  LDiagnostics: INyxPartitionDiagnostics;
  LWork: INyxWork;
  LExecution: INyxExecution;
begin
  AOptions.Validate;

  if (AFactory = nil) or (AScheduler = nil) or (APort = nil) then
  begin
    raise ENyxQuery.Create('Parallel submission requires a selector factory, scheduler and result port');
  end;

  if Length(AValues) > AOptions.InputLimit then
  begin
    raise ENyxQuery.Create('Parallel input exceeds its configured limit');
  end;
  AScheduler.Admit(neUIQueue);

  if AOptions.ExecutionMode = npxWorkers then
  begin
    AScheduler.Admit(neThreaded);
  end;
  LProgress := NewNyxQueryProgress(AGeneration);
  LDiagnostics := NewNyxPartitionDiagnostics;
  LWork := TNyxPartitionCoordinator<TSource, TResult>.Create(AValues, AFactory,
    AScheduler, APort, LProgress, LDiagnostics, AOptions);
  LExecution := AScheduler.Submit(LWork, neUIQueue);
  LProgress.Attach(LExecution, False);
  Result := NewNyxParallelRun(LProgress, LDiagnostics);
end;

constructor TNyxPartitionCapture<TSource, TResult>.Create(const ASequence: INyxSequence<TSource>;
  const AFactory: INyxPartitionSelectorFactory<TSource, TResult>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
  const AProgress: INyxQueryProgress; const ADiagnostics: INyxPartitionDiagnostics;
  const AOptions: TNyxParallelOptions;
  const AActionFactory: INyxPartitionActionFactory<TSource>;
  const AAccumulatorFactory: INyxPartitionAccumulatorFactory<TSource, TResult>;
  const ACompletion: INyxPartitionCompletion<TResult>);
begin
  inherited Create;
  FSequence := ASequence;
  FTurns := NyxSchedulerTurns(AScheduler);
  FFactory := AFactory;
  FActionFactory := AActionFactory;
  FAccumulatorFactory := AAccumulatorFactory;
  FCompletion := ACompletion;
  FScheduler := AScheduler;
  FPort := APort;
  FProgress := AProgress;
  FDiagnostics := ADiagnostics;
  FOptions := AOptions;
end;

procedure TNyxPartitionCapture<TSource, TResult>.AdmitRow;
begin

  if FCount = FOptions.InputLimit then
  begin
    raise ENyxQuery.Create('Deferred parallel input exceeds its configured limit');
  end;

  if FCount = FCapacity then
  begin

    if FCapacity = 0 then
    begin
      FCapacity := 16;
    end
    else
    begin
      FCapacity := FCapacity * 2;
    end;

    if FCapacity > FOptions.InputLimit then
    begin
      FCapacity := FOptions.InputLimit;
    end;
    SetLength(FValues, FCapacity);
  end;
  FValues[FCount] := FCursor.Current;
  Inc(FCount);
end;

procedure TNyxPartitionCapture<TSource, TResult>.BeginPartitions;
var
  LWork: INyxWork;
  LExecution: INyxExecution;
begin
  SetLength(FValues, FCount);
  FCursor := nil;
  FSequence := nil;
  LWork := TNyxPartitionCoordinator<TSource, TResult>.Create(FValues, FFactory,
    FScheduler, FPort, FProgress, FDiagnostics, FOptions, FActionFactory,
    FAccumulatorFactory, FCompletion);
  FValues := nil;
  FFactory := nil;
  FActionFactory := nil;
  FAccumulatorFactory := nil;
  FCompletion := nil;
  LExecution := FScheduler.PostUI(LWork);
  FProgress.Attach(LExecution, False);
end;

procedure TNyxPartitionCapture<TSource, TResult>.Execute(const AExecution: INyxExecution);
var
  LIndex: Integer;
  LExecution: INyxExecution;
begin
  FScheduler.RequireUI;
  try

    if FProgress.IsCancelled or AExecution.Cancelled then
    begin
      FProgress.Cancel;
      Exit;
    end;

    if FCursor = nil then
    begin
      FProgress.BeginCompute;
      FCursor := FSequence.GetEnumerator;
      FSequence := nil;

      if FCursor = nil then
      begin
        raise ENyxQuery.Create('Deferred parallel source returned no cursor');
      end;
    end;
    for LIndex := 1 to FOptions.CaptureQuantum do
    begin

      if FProgress.IsCancelled or AExecution.Cancelled then
      begin
        FProgress.Cancel;
        Exit;
      end;

      if not FCursor.MoveNext then
      begin
        BeginPartitions;
        Exit;
      end;

      if FProgress.IsCancelled or AExecution.Cancelled then
      begin
        FProgress.Cancel;
        Exit;
      end;
      AdmitRow;
    end;
    { Each continuation owns an independent scheduler ticket. Parenting every
      turn to its predecessor would create a recursive cancellation chain whose
      depth grew with source length; the shared receipt owns cancellation instead. }
    LExecution := FTurns.PostTurn(Self);
    FProgress.Attach(LExecution, False);
  except
    on LException: Exception do
    begin
      FProgress.Fail(NyxQueryDiagnostic(LException));
      FProgress.Cancel;
    end;
  end;
end;

class function TNyxParallelSelect<TSource, TResult>.Submit(const ASequence: INyxSequence<TSource>;
  const AFactory: INyxPartitionSelectorFactory<TSource, TResult>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
  const AOptions: TNyxParallelOptions;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
var
  LProgress: INyxQueryProgress;
  LDiagnostics: INyxPartitionDiagnostics;
  LWork: INyxWork;
  LExecution: INyxExecution;
begin
  AOptions.Validate;

  if (ASequence = nil) or (AFactory = nil) or (AScheduler = nil) or (APort = nil) then
  begin
    raise ENyxQuery.Create('Deferred parallel submission requires a sequence, factory, scheduler and result port');
  end;
  AScheduler.Admit(neUIQueue);

  if AOptions.ExecutionMode = npxWorkers then
  begin
    AScheduler.Admit(neThreaded);
  end;
  LProgress := NewNyxQueryProgress(AGeneration);
  LDiagnostics := NewNyxPartitionDiagnostics;
  LWork := TNyxPartitionCapture<TSource, TResult>.Create(ASequence, AFactory,
    AScheduler, APort, LProgress, LDiagnostics, AOptions);
  LExecution := AScheduler.Submit(LWork, neUIQueue);
  LProgress.Attach(LExecution, False);
  Result := NewNyxParallelRun(LProgress, LDiagnostics);
end;

class function TNyxParallelForAll<T>.Submit(const AValues: array of T;
  const AFactory: INyxPartitionActionFactory<T>; const AScheduler: INyxScheduler;
  const AOptions: TNyxParallelOptions;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
var
  LProgress: INyxQueryProgress;
  LDiagnostics: INyxPartitionDiagnostics;
  LWork: INyxWork;
  LExecution: INyxExecution;
begin
  AOptions.Validate;

  if (AFactory = nil) or (AScheduler = nil) then
  begin
    raise ENyxQuery.Create('Parallel actions require a factory and scheduler');
  end;

  if Length(AValues) > AOptions.InputLimit then
  begin
    raise ENyxQuery.Create('Parallel input exceeds its configured limit');
  end;
  AScheduler.Admit(neUIQueue);

  if AOptions.ExecutionMode = npxWorkers then
  begin
    AScheduler.Admit(neThreaded);
  end;
  LProgress := NewNyxQueryProgress(AGeneration);
  LDiagnostics := NewNyxPartitionDiagnostics;
  { Boolean is only the unused generic result specialization. Action mode
    allocates NO output vector, neither per partition nor for merged completion. }
  LWork := TNyxPartitionCoordinator<T, Boolean>.Create(AValues, nil, AScheduler,
    nil, LProgress, LDiagnostics, AOptions, AFactory);
  LExecution := AScheduler.Submit(LWork, neUIQueue);
  LProgress.Attach(LExecution, False);
  Result := NewNyxParallelRun(LProgress, LDiagnostics);
end;

class function TNyxParallelForAll<T>.Submit(const ASequence: INyxSequence<T>;
  const AFactory: INyxPartitionActionFactory<T>; const AScheduler: INyxScheduler;
  const AOptions: TNyxParallelOptions;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
var
  LProgress: INyxQueryProgress;
  LDiagnostics: INyxPartitionDiagnostics;
  LWork: INyxWork;
  LExecution: INyxExecution;
begin
  AOptions.Validate;

  if (ASequence = nil) or (AFactory = nil) or (AScheduler = nil) then
  begin
    raise ENyxQuery.Create('Deferred parallel actions require a sequence, factory and scheduler');
  end;
  AScheduler.Admit(neUIQueue);

  if AOptions.ExecutionMode = npxWorkers then
  begin
    AScheduler.Admit(neThreaded);
  end;
  LProgress := NewNyxQueryProgress(AGeneration);
  LDiagnostics := NewNyxPartitionDiagnostics;
  LWork := TNyxPartitionCapture<T, Boolean>.Create(ASequence, nil, AScheduler,
    nil, LProgress, LDiagnostics, AOptions, AFactory);
  LExecution := AScheduler.Submit(LWork, neUIQueue);
  LProgress.Attach(LExecution, False);
  Result := NewNyxParallelRun(LProgress, LDiagnostics);
end;

end.
