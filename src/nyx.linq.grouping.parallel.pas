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

unit nyx.linq.grouping.parallel;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

interface

uses
  SysUtils, nyx.callback.scope, nyx.linq, nyx.linq.grouping, nyx.linq.aggregates,
  nyx.linq.numeric, nyx.linq.numeric.parallel, nyx.linq.parallel, nyx.linq.pipeline,
  nyx.linq.reductions, nyx.linq.execution, nyx.scheduler;

type
  { Every return follows the SAME equivalence/hash law. Factory entry is on UI;
    returned equality objects run only in their owning partition/final worker.
    (-1,0) identifies final merge. Hash/equality must not read live UI/stores.
    SharedImmutable explicitly promises immutable/synchronized state. }
  INyxParallelEqualityFactory<T> = interface(IInterface)
    function NewEquality(APartition, ASourceOffset: Integer): INyxEquality<T>;
  end;
  TNyxEqualityFactoryFunction<T> = function: INyxEquality<T>;
  TNyxEqualityFactoryNested<T> = function: INyxEquality<T>
    {$ifndef PAS2JS}is nested{$endif};
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  TNyxEqualityFactoryReference<T> = reference to function: INyxEquality<T>;
  {$endif}
  { Factory authoring adapters. FromNested retains a revocable port, never the
    caller's frame/lease; Close refuses later entry before invoking the function.
    FromInline owns its function reference and explicit interface capture. }
  TNyxParallelEquality<T> = class
  public
    class function From(AFactory: TNyxEqualityFactoryFunction<T>): INyxParallelEqualityFactory<T>; static;
    class function SharedImmutable(const AEquality: INyxEquality<T>): INyxParallelEqualityFactory<T>; static;
    class function FromNested(AFactory: TNyxEqualityFactoryNested<T>;
      const ALease: INyxCallbackLease): INyxParallelEqualityFactory<T>; static;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    class function FromInline(const AFactory: TNyxEqualityFactoryReference<T>;
      const ACapture: INyxCallbackCapture = nil): INyxParallelEqualityFactory<T>; static;
    {$endif}
  end;

  { A partition/final kernel owns independent per-key state. Empty receives the
    first encountered equivalent key and must return a fresh mutable seed where
    needed. Merge reads immutable completed partials; it must never mutate them.
    Associative identity laws and mutable payload isolation remain caller contracts.
    All four operations run under selected worker/event-loop policy. Execution
    allows cooperative checks inside an expensive custom merge/finish. }
  INyxKeyedKernel<T, TKey, TState, TResult> = interface(IInterface)
    function Empty(const AKey: TKey): TState;
    procedure Add(var AState: TState; const AValue: T; const AExecution: INyxExecution);
    procedure Merge(var AState: TState; const AOther: TState; const AExecution: INyxExecution);
    function Finish(const AKey: TKey; const AState: TState; const AExecution: INyxExecution): TResult;
  end;
  INyxKeyedKernelFactory<T, TKey, TState, TResult> = interface(IInterface)
    { UI-only creation; each owned callback instance is independent or explicitly
      shares only immutable/synchronized dependencies. (-1,0) is final merge. }
    function NewKernel(APartition, ASourceOffset: Integer): INyxKeyedKernel<T, TKey, TState, TResult>;
  end;
  { Internal immutable partial. Keys and states preserve partition encounter
    order. Mutable reference states are sealed after Finish and read-only to merge.
    This interface retains no builder/source/UI and never reaches a UI directly. }
  INyxKeyedPartial<TKey, TState> = interface(IInterface)
    function GetCount: Integer;
    function KeyAt(AIndex: Integer): TKey;
    function StateAt(AIndex: Integer): TState;
    property Count: Integer read GetCount;
  end;
  { Reusable immutable configuration owns factories/options, never a run or UI.
    WithOptions validates and returns another plan. Submit uses a complete owned
    source capture and returns ordinary failure/cancellation diagnostics; callers
    own the result receiver lease and its mounted generation. Getters grant no
    mutation authority over already admitted runs. }
  INyxKeyedPlan<T, TKey, TState, TResult> = interface(IInterface)
    function WithOptions(const AOptions: TNyxParallelOptions): INyxKeyedPlan<T, TKey, TState, TResult>;
    function Submit(const ASource: INyxSequence<T>; const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<INyxSequence<TResult>>;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun;
    function GetKeys: INyxPartitionSelectorFactory<T, TKey>;
    function GetEqualities: INyxParallelEqualityFactory<TKey>;
    function GetKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>;
    function GetOptions: TNyxParallelOptions;
    property Keys: INyxPartitionSelectorFactory<T, TKey> read GetKeys;
    property Equalities: INyxParallelEqualityFactory<TKey> read GetEqualities;
    property Kernels: INyxKeyedKernelFactory<T, TKey, TState, TResult> read GetKernels;
    property Options: TNyxParallelOptions read GetOptions;
  end;
  TNyxParallelKeyed<T, TKey, TState, TResult> = class
  public
    { Source order gives ordinary first-key/value encounter semantics. Completion
      order is explicit: first observed partial supplies key representation/group
      order and values/partial folds follow that order. No partial result publishes.
      Empty input returns one owned empty sequence; no partition callbacks enter. }
    class function From(const AKeys: INyxPartitionSelectorFactory<T, TKey>;
      const AEqualities: INyxParallelEqualityFactory<TKey>;
      const AKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>):
      INyxKeyedPlan<T, TKey, TState, TResult>; static;
  end;
  TNyxProgramKeyed<TInput, TOutput, TKey, TState, TResult> = class
  public
    { Filter/map/type-changing output enters the keyed plan. Plan options govern
      a separately configured composition, leaving the reusable program intact. }
    class function Submit(const AProgram: INyxParallelProgram<TInput, TOutput>;
      const APlan: INyxKeyedPlan<TOutput, TKey, TState, TResult>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<INyxSequence<TResult>>;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; static;
  end;

  { Kernel-local single-writer buffer, sealed once a partial finishes. AppendFrom
    reads sealed values cooperatively; CopyValues/Snapshot detach their outer
    vector and retain managed payloads. Payload objects are not deep-cloned.
    Indexed access refuses outside Count; capacity overflow refuses admission. }
  INyxElementBuffer<T> = interface(IInterface)
    procedure Append(const AValue: T);
    procedure AppendFrom(const AOther: INyxElementBuffer<T>; const AExecution: INyxExecution);
    function Snapshot: INyxSequence<T>;
    function CopyValues: TNyxSequenceArray<T>;
    function GetCount: Integer;
    function ValueAt(AIndex: Integer): T;
    property Count: Integer read GetCount;
  end;
  TNyxParallelGroupBy<T, TKey, TElement> = class
  public
    { Full groups retain O(input rows) values; each row projects key/element once.
      Immutable final groups/cursors survive plans, sources and application hosts. }
    class function From(const AKeys: INyxPartitionSelectorFactory<T, TKey>;
      const AElements: INyxPartitionSelectorFactory<T, TElement>;
      const AEqualities: INyxParallelEqualityFactory<TKey>):
      INyxKeyedPlan<T, TKey, INyxElementBuffer<TElement>, INyxGroup<TKey, TElement>>; static;
  end;
  TNyxParallelCountBy<T, TKey> = class
  public
    { Compact states retain one count per distinct partition key, not row values.
      CountBy is checked Integer; LongCounts is exact signed64-domain count. }
    class function From(const AKeys: INyxPartitionSelectorFactory<T, TKey>;
      const AEqualities: INyxParallelEqualityFactory<TKey>):
      INyxKeyedPlan<T, TKey, Integer, INyxKeyValue<TKey, Integer>>; static;
    class function LongCounts(const AKeys: INyxPartitionSelectorFactory<T, TKey>;
      const AEqualities: INyxParallelEqualityFactory<TKey>):
      INyxKeyedPlan<T, TKey, TNyxQueryCount, INyxKeyValue<TKey, TNyxQueryCount>>; static;
  end;
  TNyxParallelAggregateBy<T, TKey, TState, TValue> = class
  public
    { Numeric/custom kernels retain compact per-key state and one completed value.
      Kernel Empty must supply independent state for each newly encountered key. }
    class function From(const AKeys: INyxPartitionSelectorFactory<T, TKey>;
      const AEqualities: INyxParallelEqualityFactory<TKey>;
      const ANumeric: INyxNumericKernel<T, TState, TValue>):
      INyxKeyedPlan<T, TKey, TState, INyxKeyValue<TKey, TValue>>; static;
  end;

  { Compiler-visible owned support. Raw indexed builders remain single-consumer
    and never escape; snapshots/results own their managed values independently. }
  TEqualityFactory<T> = class(TInterfacedObject, INyxParallelEqualityFactory<T>)
  private
    FFunction: TNyxEqualityFactoryFunction<T>;
    FNested: TNyxEqualityFactoryNested<T>;
    FLifetime: INyxCallbackLifetime;
    FShared: INyxEquality<T>;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    FInline: TNyxEqualityFactoryReference<T>;
    FCapture: INyxCallbackCapture;
    {$endif}
  public
    constructor Create(AFunction: TNyxEqualityFactoryFunction<T>; const AShared: INyxEquality<T>);
    constructor CreateNested(AFunction: TNyxEqualityFactoryNested<T>; const ALease: INyxCallbackLease);
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    constructor CreateInline(const AFunction: TNyxEqualityFactoryReference<T>;
      const ACapture: INyxCallbackCapture);
    {$endif}
    function NewEquality(APartition, ASourceOffset: Integer): INyxEquality<T>;
  end;
  TKeyedPartial<TKey, TState> = class(TInterfacedObject, INyxKeyedPartial<TKey, TState>)
  private
    FKeys: TNyxSequenceArray<TKey>;
    FStates: TNyxSequenceArray<TState>;
  public
    constructor Create(const AKeys: TNyxSequenceArray<TKey>;
      const AStates: TNyxSequenceArray<TState>; ACount: Integer);
    function GetCount: Integer;
    function KeyAt(AIndex: Integer): TKey;
    function StateAt(AIndex: Integer): TState;
  end;
  TKeyedBuilder<TKey, TState> = class
  private
    FEquality: INyxEquality<TKey>;
    FKeys: TNyxSequenceArray<TKey>;
    FStates: TNyxSequenceArray<TState>;
    FHashes: array of Cardinal;
    FSlots: array of Integer;
    FCount: Integer;
    procedure GrowSlots;
    function FindSlot(const AKey: TKey; AHash: Cardinal): Integer;
    procedure RequireIndex(AIndex: Integer);
  public
    constructor Create(const AEquality: INyxEquality<TKey>);
    function IndexOf(const AKey: TKey; out AHash: Cardinal): Integer;
    function Insert(const AKey: TKey; AHash: Cardinal; const AState: TState): Integer;
    function KeyAt(AIndex: Integer): TKey;
    function StateAt(AIndex: Integer): TState;
    procedure SetState(AIndex: Integer; const AState: TState);
    function Snapshot: INyxKeyedPartial<TKey, TState>;
    property Count: Integer read FCount;
  end;
  TKeyedAccumulator<T, TKey, TState, TResult> = class(TInterfacedObject,
    INyxPartitionAccumulator<T, INyxKeyedPartial<TKey, TState>>)
  private
    FKeys: INyxSelector<T, TKey>;
    FKernel: INyxKeyedKernel<T, TKey, TState, TResult>;
    FBuilder: TKeyedBuilder<TKey, TState>;
  public
    constructor Create(const AKeys: INyxSelector<T, TKey>; const AEquality: INyxEquality<TKey>;
      const AKernel: INyxKeyedKernel<T, TKey, TState, TResult>);
    destructor Destroy; override;
    procedure Add(const AValue: T; ASourceIndex: Integer; const AExecution: INyxExecution);
    function Finish(const AExecution: INyxExecution): INyxKeyedPartial<TKey, TState>;
  end;
  TKeyedFactory<T, TKey, TState, TResult> = class(TInterfacedObject,
    INyxPartitionAccumulatorFactory<T, INyxKeyedPartial<TKey, TState>>)
  private
    FKeys: INyxPartitionSelectorFactory<T, TKey>;
    FEqualities: INyxParallelEqualityFactory<TKey>;
    FKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>;
  public
    constructor Create(const AKeys: INyxPartitionSelectorFactory<T, TKey>;
      const AEqualities: INyxParallelEqualityFactory<TKey>;
      const AKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>);
    function NewAccumulator(APartition, ASourceOffset: Integer):
      INyxPartitionAccumulator<T, INyxKeyedPartial<TKey, TState>>;
  end;
  TKeyedMerger<T, TKey, TState, TResult> = class(TInterfacedObject,
    INyxParallelMerger<INyxKeyedPartial<TKey, TState>, INyxSequence<TResult>>)
  private
    FEquality: INyxEquality<TKey>;
    FKernel: INyxKeyedKernel<T, TKey, TState, TResult>;
  public
    constructor Create(const AEquality: INyxEquality<TKey>;
      const AKernel: INyxKeyedKernel<T, TKey, TState, TResult>);
    function Merge(const AValues: TNyxSequenceArray<INyxKeyedPartial<TKey, TState>>;
      const AExecution: INyxExecution): INyxSequence<TResult>;
  end;
  TKeyedMergerFactory<T, TKey, TState, TResult> = class(TInterfacedObject,
    INyxParallelMergerFactory<INyxKeyedPartial<TKey, TState>, INyxSequence<TResult>>)
  private
    FEqualities: INyxParallelEqualityFactory<TKey>;
    FKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>;
  public
    constructor Create(const AEqualities: INyxParallelEqualityFactory<TKey>;
      const AKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>);
    function NewMerger: INyxParallelMerger<INyxKeyedPartial<TKey, TState>, INyxSequence<TResult>>;
  end;
  TKeyedPlan<T, TKey, TState, TResult> = class(TInterfacedObject, INyxKeyedPlan<T, TKey, TState, TResult>)
  private
    FKeys: INyxPartitionSelectorFactory<T, TKey>;
    FEqualities: INyxParallelEqualityFactory<TKey>;
    FKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>;
    FOptions: TNyxParallelOptions;
  public
    constructor Create(const AKeys: INyxPartitionSelectorFactory<T, TKey>;
      const AEqualities: INyxParallelEqualityFactory<TKey>;
      const AKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>;
      const AOptions: TNyxParallelOptions);
    function WithOptions(const AOptions: TNyxParallelOptions): INyxKeyedPlan<T, TKey, TState, TResult>;
    function Submit(const ASource: INyxSequence<T>; const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<INyxSequence<TResult>>;
      const AGeneration: INyxCancellationScope): INyxParallelRun;
    function GetKeys: INyxPartitionSelectorFactory<T, TKey>;
    function GetEqualities: INyxParallelEqualityFactory<TKey>;
    function GetKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>;
    function GetOptions: TNyxParallelOptions;
  end;
  TElementBuffer<T> = class(TInterfacedObject, INyxElementBuffer<T>)
  private
    FValues: TNyxSequenceArray<T>;
    FCount: Integer;
  public
    procedure Append(const AValue: T);
    procedure AppendFrom(const AOther: INyxElementBuffer<T>; const AExecution: INyxExecution);
    function Snapshot: INyxSequence<T>;
    function CopyValues: TNyxSequenceArray<T>;
    function GetCount: Integer;
    function ValueAt(AIndex: Integer): T;
  end;
  TGroupKernel<T, TKey, TElement> = class(TInterfacedObject,
    INyxKeyedKernel<T, TKey, INyxElementBuffer<TElement>, INyxGroup<TKey, TElement>>)
  private
    FElement: INyxSelector<T, TElement>;
  public
    constructor Create(const AElement: INyxSelector<T, TElement>);
    function Empty(const AKey: TKey): INyxElementBuffer<TElement>;
    procedure Add(var AState: INyxElementBuffer<TElement>; const AValue: T; const AExecution: INyxExecution);
    procedure Merge(var AState: INyxElementBuffer<TElement>; const AOther: INyxElementBuffer<TElement>;
      const AExecution: INyxExecution);
    function Finish(const AKey: TKey; const AState: INyxElementBuffer<TElement>;
      const AExecution: INyxExecution): INyxGroup<TKey, TElement>;
  end;
  TGroupKernelFactory<T, TKey, TElement> = class(TInterfacedObject,
    INyxKeyedKernelFactory<T, TKey, INyxElementBuffer<TElement>, INyxGroup<TKey, TElement>>)
  private
    FElements: INyxPartitionSelectorFactory<T, TElement>;
  public
    constructor Create(const AElements: INyxPartitionSelectorFactory<T, TElement>);
    function NewKernel(APartition, ASourceOffset: Integer):
      INyxKeyedKernel<T, TKey, INyxElementBuffer<TElement>, INyxGroup<TKey, TElement>>;
  end;
  TCountKernel<T, TKey> = class(TInterfacedObject,
    INyxKeyedKernel<T, TKey, Integer, INyxKeyValue<TKey, Integer>>)
    function Empty(const AKey: TKey): Integer;
    procedure Add(var AState: Integer; const AValue: T; const AExecution: INyxExecution);
    procedure Merge(var AState: Integer; const AOther: Integer; const AExecution: INyxExecution);
    function Finish(const AKey: TKey; const AState: Integer;
      const AExecution: INyxExecution): INyxKeyValue<TKey, Integer>;
  end;
  TCountKernelFactory<T, TKey> = class(TInterfacedObject,
    INyxKeyedKernelFactory<T, TKey, Integer, INyxKeyValue<TKey, Integer>>)
    function NewKernel(APartition, ASourceOffset: Integer):
      INyxKeyedKernel<T, TKey, Integer, INyxKeyValue<TKey, Integer>>;
  end;
  TLongCountKernel<T, TKey> = class(TInterfacedObject,
    INyxKeyedKernel<T, TKey, TNyxQueryCount, INyxKeyValue<TKey, TNyxQueryCount>>)
    function Empty(const AKey: TKey): TNyxQueryCount;
    procedure Add(var AState: TNyxQueryCount; const AValue: T; const AExecution: INyxExecution);
    procedure Merge(var AState: TNyxQueryCount; const AOther: TNyxQueryCount; const AExecution: INyxExecution);
    function Finish(const AKey: TKey; const AState: TNyxQueryCount;
      const AExecution: INyxExecution): INyxKeyValue<TKey, TNyxQueryCount>;
  end;
  TLongCountKernelFactory<T, TKey> = class(TInterfacedObject,
    INyxKeyedKernelFactory<T, TKey, TNyxQueryCount, INyxKeyValue<TKey, TNyxQueryCount>>)
    function NewKernel(APartition, ASourceOffset: Integer):
      INyxKeyedKernel<T, TKey, TNyxQueryCount, INyxKeyValue<TKey, TNyxQueryCount>>;
  end;
  TNumericKeyKernel<T, TKey, TState, TValue> = class(TInterfacedObject,
    INyxKeyedKernel<T, TKey, TState, INyxKeyValue<TKey, TValue>>)
  private
    FNumeric: INyxNumericKernel<T, TState, TValue>;
  public
    constructor Create(const ANumeric: INyxNumericKernel<T, TState, TValue>);
    function Empty(const AKey: TKey): TState;
    procedure Add(var AState: TState; const AValue: T; const AExecution: INyxExecution);
    procedure Merge(var AState: TState; const AOther: TState; const AExecution: INyxExecution);
    function Finish(const AKey: TKey; const AState: TState;
      const AExecution: INyxExecution): INyxKeyValue<TKey, TValue>;
  end;
  TNumericKeyFactory<T, TKey, TState, TValue> = class(TInterfacedObject,
    INyxKeyedKernelFactory<T, TKey, TState, INyxKeyValue<TKey, TValue>>)
  private
    FNumeric: INyxNumericKernel<T, TState, TValue>;
  public
    constructor Create(const ANumeric: INyxNumericKernel<T, TState, TValue>);
    function NewKernel(APartition, ASourceOffset: Integer):
      INyxKeyedKernel<T, TKey, TState, INyxKeyValue<TKey, TValue>>;
  end;

implementation

constructor TEqualityFactory<T>.Create(AFunction: TNyxEqualityFactoryFunction<T>; const AShared: INyxEquality<T>);
begin
  inherited Create;
  FFunction := AFunction;
  FShared := AShared;
end;

constructor TEqualityFactory<T>.CreateNested(AFunction: TNyxEqualityFactoryNested<T>;
  const ALease: INyxCallbackLease);
begin
  inherited Create;

  if (ALease = nil) or not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('Nested equality requires a factory and lifetime lease');
  end;
  FLifetime := ALease.Lifetime;
  FLifetime.RequireEntry;
  FNested := AFunction;
end;

{$ifdef NYX_HAS_INLINE_CALLBACKS}
constructor TEqualityFactory<T>.CreateInline(const AFunction: TNyxEqualityFactoryReference<T>;
  const ACapture: INyxCallbackCapture);
begin
  inherited Create;

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('Inline equality requires a factory');
  end;
  FInline := AFunction;
  FCapture := ACapture;
end;
{$endif}

function TEqualityFactory<T>.NewEquality(APartition, ASourceOffset: Integer): INyxEquality<T>;
begin

  if FLifetime <> nil then
  begin
    FLifetime.RequireEntry;
    Result := FNested();
  end
  else if Assigned(FFunction) then
  begin
    Result := FFunction();
  end
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  else if Assigned(FInline) then
  begin
    Result := FInline();
  end
  {$endif}
  else
  begin
    Result := FShared;
  end;

  if Result = nil then
  begin
    raise ENyxQuery.Create('Parallel equality factory returned no equality');
  end;
end;

class function TNyxParallelEquality<T>.From(AFactory: TNyxEqualityFactoryFunction<T>): INyxParallelEqualityFactory<T>;
begin

  if not Assigned(AFactory) then
  begin
    raise ENyxQuery.Create('Parallel equality requires a factory');
  end;
  Result := TEqualityFactory<T>.Create(AFactory, nil);
end;

class function TNyxParallelEquality<T>.SharedImmutable(const AEquality: INyxEquality<T>): INyxParallelEqualityFactory<T>;
begin

  if AEquality = nil then
  begin
    raise ENyxQuery.Create('Parallel equality requires an immutable equality');
  end;
  Result := TEqualityFactory<T>.Create(nil, AEquality);
end;

class function TNyxParallelEquality<T>.FromNested(AFactory: TNyxEqualityFactoryNested<T>;
  const ALease: INyxCallbackLease): INyxParallelEqualityFactory<T>;
begin
  Result := TEqualityFactory<T>.CreateNested(AFactory, ALease);
end;

{$ifdef NYX_HAS_INLINE_CALLBACKS}
class function TNyxParallelEquality<T>.FromInline(const AFactory: TNyxEqualityFactoryReference<T>;
  const ACapture: INyxCallbackCapture): INyxParallelEqualityFactory<T>;
begin
  Result := TEqualityFactory<T>.CreateInline(AFactory, ACapture);
end;
{$endif}

constructor TKeyedPartial<TKey, TState>.Create(const AKeys: TNyxSequenceArray<TKey>;
  const AStates: TNyxSequenceArray<TState>; ACount: Integer);
var
  LIndex: Integer;
begin
  inherited Create;
  SetLength(FKeys, ACount);
  SetLength(FStates, ACount);
  for LIndex := 0 to ACount - 1 do
  begin
    FKeys[LIndex] := AKeys[LIndex];
    FStates[LIndex] := AStates[LIndex];
  end;
end;

function TKeyedPartial<TKey, TState>.GetCount: Integer;
begin
  Result := Length(FKeys);
end;

function TKeyedPartial<TKey, TState>.KeyAt(AIndex: Integer): TKey;
begin

  if (AIndex < 0) or (AIndex >= Length(FKeys)) then
  begin
    raise ENyxQuery.Create('Partial key index is outside its owned range');
  end;
  Result := FKeys[AIndex];
end;

function TKeyedPartial<TKey, TState>.StateAt(AIndex: Integer): TState;
begin

  if (AIndex < 0) or (AIndex >= Length(FStates)) then
  begin
    raise ENyxQuery.Create('Partial state index is outside its owned range');
  end;
  Result := FStates[AIndex];
end;

constructor TKeyedBuilder<TKey, TState>.Create(const AEquality: INyxEquality<TKey>);
begin
  inherited Create;
  FEquality := AEquality;
  GrowSlots;
end;

procedure TKeyedBuilder<TKey, TState>.GrowSlots;
var
  LSize: Integer;
  LIndex: Integer;
  LSlot: Integer;
begin
  LSize := Length(FSlots);

  if LSize = 0 then
  begin
    LSize := 16;
  end
  else
  begin

    if LSize > High(Integer) div 2 then
    begin
      raise ENyxQuery.Create('Keyed slot capacity exceeds Integer');
    end;
    LSize := LSize * 2;
  end;
  SetLength(FSlots, LSize);
  for LIndex := 0 to High(FSlots) do
  begin
    FSlots[LIndex] := -1;
  end;
  for LIndex := 0 to FCount - 1 do
  begin
    LSlot := Integer(FHashes[LIndex] mod Cardinal(LSize));
    while FSlots[LSlot] >= 0 do
    begin
      LSlot := (LSlot + 1) mod LSize;
    end;
    FSlots[LSlot] := LIndex;
  end;
end;

function TKeyedBuilder<TKey, TState>.FindSlot(const AKey: TKey; AHash: Cardinal): Integer;
var
  LIndex: Integer;
begin
  Result := Integer(AHash mod Cardinal(Length(FSlots)));
  while FSlots[Result] >= 0 do
  begin
    LIndex := FSlots[Result];

    if (FHashes[LIndex] = AHash) and FEquality.Equal(FKeys[LIndex], AKey) then
    begin
      Exit;
    end;
    Result := (Result + 1) mod Length(FSlots);
  end;
end;

function TKeyedBuilder<TKey, TState>.IndexOf(const AKey: TKey; out AHash: Cardinal): Integer;
begin
  AHash := FEquality.Hash(AKey);
  Result := FSlots[FindSlot(AKey, AHash)];
end;

function TKeyedBuilder<TKey, TState>.Insert(const AKey: TKey; AHash: Cardinal; const AState: TState): Integer;
var
  LSlot: Integer;
  LCapacity: Integer;
begin

  if FCount >= Length(FSlots) div 2 then
  begin
    GrowSlots;
  end;
  LSlot := FindSlot(AKey, AHash);

  if FSlots[LSlot] >= 0 then
  begin
    raise ENyxQuery.Create('Keyed insertion requires a new admitted key');
  end;

  if FCount = Length(FKeys) then
  begin
    LCapacity := Length(FKeys);

    if LCapacity = 0 then
    begin
      LCapacity := 8;
    end
    else
    begin

      if LCapacity > High(Integer) div 2 then
      begin
        raise ENyxQuery.Create('Keyed state capacity exceeds Integer');
      end;
      LCapacity := LCapacity * 2;
    end;
    SetLength(FKeys, LCapacity);
    SetLength(FStates, LCapacity);
    SetLength(FHashes, LCapacity);
  end;
  Result := FCount;
  FKeys[Result] := AKey;
  FStates[Result] := AState;
  FHashes[Result] := AHash;
  FSlots[LSlot] := Result;
  Inc(FCount);
end;

procedure TKeyedBuilder<TKey, TState>.RequireIndex(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= FCount) then
  begin
    raise ENyxQuery.Create('Keyed state index is outside its owned range');
  end;
end;

function TKeyedBuilder<TKey, TState>.KeyAt(AIndex: Integer): TKey;
begin
  RequireIndex(AIndex);
  Result := FKeys[AIndex];
end;

function TKeyedBuilder<TKey, TState>.StateAt(AIndex: Integer): TState;
begin
  RequireIndex(AIndex);
  Result := FStates[AIndex];
end;

procedure TKeyedBuilder<TKey, TState>.SetState(AIndex: Integer; const AState: TState);
begin
  RequireIndex(AIndex);
  FStates[AIndex] := AState;
end;

function TKeyedBuilder<TKey, TState>.Snapshot: INyxKeyedPartial<TKey, TState>;
begin
  Result := TKeyedPartial<TKey, TState>.Create(FKeys, FStates, FCount);
end;

constructor TKeyedAccumulator<T, TKey, TState, TResult>.Create(const AKeys: INyxSelector<T, TKey>;
  const AEquality: INyxEquality<TKey>; const AKernel: INyxKeyedKernel<T, TKey, TState, TResult>);
begin
  inherited Create;
  FKeys := AKeys;
  FKernel := AKernel;
  FBuilder := TKeyedBuilder<TKey, TState>.Create(AEquality);
end;

destructor TKeyedAccumulator<T, TKey, TState, TResult>.Destroy;
begin
  FBuilder.Free;
  inherited Destroy;
end;

procedure TKeyedAccumulator<T, TKey, TState, TResult>.Add(const AValue: T;
  ASourceIndex: Integer; const AExecution: INyxExecution);
var
  LKey: TKey;
  LState: TState;
  LHash: Cardinal;
  LIndex: Integer;
begin

  if AExecution.Cancelled then
  begin
    Exit;
  end;
  LKey := FKeys.Project(AValue);
  LIndex := FBuilder.IndexOf(LKey, LHash);

  if LIndex < 0 then
  begin
    LState := FKernel.Empty(LKey);
    LIndex := FBuilder.Insert(LKey, LHash, LState);
  end
  else
  begin
    LState := FBuilder.StateAt(LIndex);
  end;
  FKernel.Add(LState, AValue, AExecution);
  FBuilder.SetState(LIndex, LState);
end;

function TKeyedAccumulator<T, TKey, TState, TResult>.Finish(const AExecution: INyxExecution):
  INyxKeyedPartial<TKey, TState>;
begin
  Result := FBuilder.Snapshot;
end;

constructor TKeyedFactory<T, TKey, TState, TResult>.Create(
  const AKeys: INyxPartitionSelectorFactory<T, TKey>; const AEqualities: INyxParallelEqualityFactory<TKey>;
  const AKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>);
begin
  inherited Create;
  FKeys := AKeys;
  FEqualities := AEqualities;
  FKernels := AKernels;
end;

function TKeyedFactory<T, TKey, TState, TResult>.NewAccumulator(APartition, ASourceOffset: Integer):
  INyxPartitionAccumulator<T, INyxKeyedPartial<TKey, TState>>;
var
  LKey: INyxSelector<T, TKey>;
  LEquality: INyxEquality<TKey>;
  LKernel: INyxKeyedKernel<T, TKey, TState, TResult>;
begin
  LKey := FKeys.NewSelector(APartition, ASourceOffset);
  LEquality := FEqualities.NewEquality(APartition, ASourceOffset);
  LKernel := FKernels.NewKernel(APartition, ASourceOffset);

  if (LKey = nil) or (LEquality = nil) or (LKernel = nil) then
  begin
    raise ENyxQuery.Create('Keyed factory returned an absent callback');
  end;
  Result := TKeyedAccumulator<T, TKey, TState, TResult>.Create(LKey, LEquality, LKernel);
end;

constructor TKeyedMerger<T, TKey, TState, TResult>.Create(const AEquality: INyxEquality<TKey>;
  const AKernel: INyxKeyedKernel<T, TKey, TState, TResult>);
begin
  inherited Create;
  FEquality := AEquality;
  FKernel := AKernel;
end;

function TKeyedMerger<T, TKey, TState, TResult>.Merge(
  const AValues: TNyxSequenceArray<INyxKeyedPartial<TKey, TState>>;
  const AExecution: INyxExecution): INyxSequence<TResult>;
var
  LBuilder: TKeyedBuilder<TKey, TState>;
  LPartial: INyxKeyedPartial<TKey, TState>;
  LKey: TKey;
  LState: TState;
  LHash: Cardinal;
  LIndex: Integer;
  LPart: Integer;
  LRow: Integer;
  LResults: TNyxSequenceArray<TResult>;
begin
  LBuilder := TKeyedBuilder<TKey, TState>.Create(FEquality);
  try
    for LPart := 0 to High(AValues) do
    begin
      LPartial := AValues[LPart];

      if LPartial = nil then
      begin
        raise ENyxQuery.Create('Keyed merge requires complete owned partials');
      end;
      for LRow := 0 to LPartial.Count - 1 do
      begin

        if AExecution.Cancelled then
        begin
          Exit(TNyxSequence<TResult>.Empty);
        end;
        LKey := LPartial.KeyAt(LRow);
        LIndex := LBuilder.IndexOf(LKey, LHash);

        if LIndex < 0 then
        begin
          LState := FKernel.Empty(LKey);
          LIndex := LBuilder.Insert(LKey, LHash, LState);
        end
        else
        begin
          LState := LBuilder.StateAt(LIndex);
        end;
        FKernel.Merge(LState, LPartial.StateAt(LRow), AExecution);
        LBuilder.SetState(LIndex, LState);
      end;
    end;
    SetLength(LResults, LBuilder.Count);
    for LIndex := 0 to LBuilder.Count - 1 do
    begin

      if AExecution.Cancelled then
      begin
        Exit(TNyxSequence<TResult>.Empty);
      end;
      LResults[LIndex] := FKernel.Finish(LBuilder.KeyAt(LIndex), LBuilder.StateAt(LIndex), AExecution);
    end;
    Result := TNyxSequence<TResult>.From(LResults);
  finally
    LBuilder.Free;
  end;
end;

constructor TKeyedMergerFactory<T, TKey, TState, TResult>.Create(
  const AEqualities: INyxParallelEqualityFactory<TKey>;
  const AKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>);
begin
  inherited Create;
  FEqualities := AEqualities;
  FKernels := AKernels;
end;

function TKeyedMergerFactory<T, TKey, TState, TResult>.NewMerger:
  INyxParallelMerger<INyxKeyedPartial<TKey, TState>, INyxSequence<TResult>>;
var
  LEquality: INyxEquality<TKey>;
  LKernel: INyxKeyedKernel<T, TKey, TState, TResult>;
begin
  LEquality := FEqualities.NewEquality(-1, 0);
  LKernel := FKernels.NewKernel(-1, 0);

  if (LEquality = nil) or (LKernel = nil) then
  begin
    raise ENyxQuery.Create('Keyed merger requires complete owned callbacks');
  end;
  Result := TKeyedMerger<T, TKey, TState, TResult>.Create(LEquality, LKernel);
end;

constructor TKeyedPlan<T, TKey, TState, TResult>.Create(
  const AKeys: INyxPartitionSelectorFactory<T, TKey>; const AEqualities: INyxParallelEqualityFactory<TKey>;
  const AKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>; const AOptions: TNyxParallelOptions);
begin
  inherited Create;

  if (AKeys = nil) or (AEqualities = nil) or (AKernels = nil) then
  begin
    raise ENyxQuery.Create('Keyed plan requires keys, equality and kernel factories');
  end;
  AOptions.Validate;
  FKeys := AKeys;
  FEqualities := AEqualities;
  FKernels := AKernels;
  FOptions := AOptions;
end;

function TKeyedPlan<T, TKey, TState, TResult>.WithOptions(const AOptions: TNyxParallelOptions):
  INyxKeyedPlan<T, TKey, TState, TResult>;
begin
  Result := TKeyedPlan<T, TKey, TState, TResult>.Create(FKeys, FEqualities, FKernels, AOptions);
end;

function TKeyedPlan<T, TKey, TState, TResult>.GetKeys: INyxPartitionSelectorFactory<T, TKey>;
begin
  Result := FKeys;
end;

function TKeyedPlan<T, TKey, TState, TResult>.GetEqualities: INyxParallelEqualityFactory<TKey>;
begin
  Result := FEqualities;
end;

function TKeyedPlan<T, TKey, TState, TResult>.GetKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>;
begin
  Result := FKernels;
end;

function TKeyedPlan<T, TKey, TState, TResult>.GetOptions: TNyxParallelOptions;
begin
  Result := FOptions;
end;

function TKeyedPlan<T, TKey, TState, TResult>.Submit(const ASource: INyxSequence<T>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<INyxSequence<TResult>>;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
type
  TPartial = INyxKeyedPartial<TKey, TState>;
  TOutput = INyxSequence<TResult>;
begin
  Result := TNyxParallelReduction<T, TPartial, TOutput>.Submit(ASource,
    TKeyedFactory<T, TKey, TState, TResult>.Create(FKeys, FEqualities, FKernels),
    TKeyedMergerFactory<T, TKey, TState, TResult>.Create(FEqualities, FKernels),
    AScheduler, APort, FOptions, AGeneration);
end;

class function TNyxParallelKeyed<T, TKey, TState, TResult>.From(
  const AKeys: INyxPartitionSelectorFactory<T, TKey>; const AEqualities: INyxParallelEqualityFactory<TKey>;
  const AKernels: INyxKeyedKernelFactory<T, TKey, TState, TResult>):
  INyxKeyedPlan<T, TKey, TState, TResult>;
begin
  Result := TKeyedPlan<T, TKey, TState, TResult>.Create(AKeys, AEqualities, AKernels, TNyxParallelOptions.Defaults);
end;

class function TNyxProgramKeyed<TInput, TOutput, TKey, TState, TResult>.Submit(
  const AProgram: INyxParallelProgram<TInput, TOutput>;
  const APlan: INyxKeyedPlan<TOutput, TKey, TState, TResult>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<INyxSequence<TResult>>; const AGeneration: INyxCancellationScope): INyxParallelRun;
type
  TPartial = INyxKeyedPartial<TKey, TState>;
  TResultSequence = INyxSequence<TResult>;
var
  LProgram: INyxParallelProgram<TInput, TOutput>;
begin

  if (AProgram = nil) or (APlan = nil) then
  begin
    raise ENyxQuery.Create('Keyed program requires a program and plan');
  end;
  LProgram := AProgram.WithOptions(APlan.Options);
  Result := TNyxProgramReduction<TInput, TOutput, TPartial, TResultSequence>.Submit(LProgram,
    TKeyedFactory<TOutput, TKey, TState, TResult>.Create(APlan.Keys, APlan.Equalities, APlan.Kernels),
    TKeyedMergerFactory<TOutput, TKey, TState, TResult>.Create(APlan.Equalities, APlan.Kernels),
    AScheduler, APort, AGeneration);
end;

procedure TElementBuffer<T>.Append(const AValue: T);
var
  LCapacity: Integer;
begin

  if FCount = High(Integer) then
  begin
    raise ENyxQuery.Create('Group value count exceeds Integer');
  end;

  if FCount = Length(FValues) then
  begin
    LCapacity := Length(FValues);

    if LCapacity = 0 then
    begin
      LCapacity := 8;
    end
    else
    begin

      if LCapacity > High(Integer) div 2 then
      begin
        raise ENyxQuery.Create('Group value capacity exceeds Integer');
      end;
      LCapacity := LCapacity * 2;
    end;
    SetLength(FValues, LCapacity);
  end;
  FValues[FCount] := AValue;
  Inc(FCount);
end;

procedure TElementBuffer<T>.AppendFrom(const AOther: INyxElementBuffer<T>; const AExecution: INyxExecution);
var
  LIndex: Integer;
begin
  for LIndex := 0 to AOther.Count - 1 do
  begin

    if AExecution.Cancelled then
    begin
      Exit;
    end;
    Append(AOther.ValueAt(LIndex));
  end;
end;

function TElementBuffer<T>.Snapshot: INyxSequence<T>;
begin
  Result := TNyxSequence<T>.From(CopyValues);
end;

function TElementBuffer<T>.CopyValues: TNyxSequenceArray<T>;
var
  LIndex: Integer;
begin
  Result := nil;
  SetLength(Result, FCount);
  for LIndex := 0 to FCount - 1 do
  begin
    Result[LIndex] := FValues[LIndex];
  end;
end;

function TElementBuffer<T>.GetCount: Integer;
begin
  Result := FCount;
end;

function TElementBuffer<T>.ValueAt(AIndex: Integer): T;
begin

  if (AIndex < 0) or (AIndex >= FCount) then
  begin
    raise ENyxQuery.Create('Group value index is outside its owned range');
  end;
  Result := FValues[AIndex];
end;

constructor TGroupKernel<T, TKey, TElement>.Create(const AElement: INyxSelector<T, TElement>);
begin
  inherited Create;
  FElement := AElement;
end;

function TGroupKernel<T, TKey, TElement>.Empty(const AKey: TKey): INyxElementBuffer<TElement>;
begin
  Result := TElementBuffer<TElement>.Create;
end;

procedure TGroupKernel<T, TKey, TElement>.Add(var AState: INyxElementBuffer<TElement>;
  const AValue: T; const AExecution: INyxExecution);
begin

  if not AExecution.Cancelled then
  begin
    AState.Append(FElement.Project(AValue));
  end;
end;

procedure TGroupKernel<T, TKey, TElement>.Merge(var AState: INyxElementBuffer<TElement>;
  const AOther: INyxElementBuffer<TElement>; const AExecution: INyxExecution);
begin
  AState.AppendFrom(AOther, AExecution);
end;

function TGroupKernel<T, TKey, TElement>.Finish(const AKey: TKey;
  const AState: INyxElementBuffer<TElement>; const AExecution: INyxExecution): INyxGroup<TKey, TElement>;
var
  LValues: TNyxSequenceArray<TElement>;
begin
  LValues := AState.CopyValues;
  Result := TNyxGroup<TKey, TElement>.Create(AKey, LValues);
end;

constructor TGroupKernelFactory<T, TKey, TElement>.Create(
  const AElements: INyxPartitionSelectorFactory<T, TElement>);
begin
  inherited Create;
  FElements := AElements;
end;

function TGroupKernelFactory<T, TKey, TElement>.NewKernel(APartition, ASourceOffset: Integer):
  INyxKeyedKernel<T, TKey, INyxElementBuffer<TElement>, INyxGroup<TKey, TElement>>;
var
  LElement: INyxSelector<T, TElement>;
begin
  LElement := nil;

  if APartition >= 0 then
  begin
    LElement := FElements.NewSelector(APartition, ASourceOffset);

    if LElement = nil then
    begin
      raise ENyxQuery.Create('Group factory returned no element selector');
    end;
  end;
  Result := TGroupKernel<T, TKey, TElement>.Create(LElement);
end;

class function TNyxParallelGroupBy<T, TKey, TElement>.From(
  const AKeys: INyxPartitionSelectorFactory<T, TKey>;
  const AElements: INyxPartitionSelectorFactory<T, TElement>;
  const AEqualities: INyxParallelEqualityFactory<TKey>):
  INyxKeyedPlan<T, TKey, INyxElementBuffer<TElement>, INyxGroup<TKey, TElement>>;
type
  TState = INyxElementBuffer<TElement>;
  TResult = INyxGroup<TKey, TElement>;
begin

  if AElements = nil then
  begin
    raise ENyxQuery.Create('GroupBy requires an element selector factory');
  end;
  Result := TNyxParallelKeyed<T, TKey, TState, TResult>.From(AKeys, AEqualities,
    TGroupKernelFactory<T, TKey, TElement>.Create(AElements));
end;

function TCountKernel<T, TKey>.Empty(const AKey: TKey): Integer;
begin
  Result := 0;
end;

procedure TCountKernel<T, TKey>.Add(var AState: Integer; const AValue: T; const AExecution: INyxExecution);
begin
  AState := NyxAddQueryInteger(AState, 1);
end;

procedure TCountKernel<T, TKey>.Merge(var AState: Integer; const AOther: Integer; const AExecution: INyxExecution);
begin
  AState := NyxAddQueryInteger(AState, AOther);
end;

function TCountKernel<T, TKey>.Finish(const AKey: TKey; const AState: Integer;
  const AExecution: INyxExecution): INyxKeyValue<TKey, Integer>;
begin
  Result := TNyxKeyValue<TKey, Integer>.Create(AKey, AState);
end;

function TCountKernelFactory<T, TKey>.NewKernel(APartition, ASourceOffset: Integer):
  INyxKeyedKernel<T, TKey, Integer, INyxKeyValue<TKey, Integer>>;
begin
  Result := TCountKernel<T, TKey>.Create;
end;

class function TNyxParallelCountBy<T, TKey>.From(const AKeys: INyxPartitionSelectorFactory<T, TKey>;
  const AEqualities: INyxParallelEqualityFactory<TKey>):
  INyxKeyedPlan<T, TKey, Integer, INyxKeyValue<TKey, Integer>>;
type
  TResult = INyxKeyValue<TKey, Integer>;
begin
  Result := TNyxParallelKeyed<T, TKey, Integer, TResult>.From(AKeys, AEqualities, TCountKernelFactory<T, TKey>.Create);
end;

function TLongCountKernel<T, TKey>.Empty(const AKey: TKey): TNyxQueryCount;
begin
  Result := NyxQueryCount(0);
end;

procedure TLongCountKernel<T, TKey>.Add(var AState: TNyxQueryCount; const AValue: T;
  const AExecution: INyxExecution);
begin
  AState := AState.Incremented;
end;

procedure TLongCountKernel<T, TKey>.Merge(var AState: TNyxQueryCount; const AOther: TNyxQueryCount;
  const AExecution: INyxExecution);
begin
  AState := AState.Added(AOther);
end;

function TLongCountKernel<T, TKey>.Finish(const AKey: TKey; const AState: TNyxQueryCount;
  const AExecution: INyxExecution): INyxKeyValue<TKey, TNyxQueryCount>;
begin
  Result := TNyxKeyValue<TKey, TNyxQueryCount>.Create(AKey, AState);
end;

function TLongCountKernelFactory<T, TKey>.NewKernel(APartition, ASourceOffset: Integer):
  INyxKeyedKernel<T, TKey, TNyxQueryCount, INyxKeyValue<TKey, TNyxQueryCount>>;
begin
  Result := TLongCountKernel<T, TKey>.Create;
end;

class function TNyxParallelCountBy<T, TKey>.LongCounts(const AKeys: INyxPartitionSelectorFactory<T, TKey>;
  const AEqualities: INyxParallelEqualityFactory<TKey>):
  INyxKeyedPlan<T, TKey, TNyxQueryCount, INyxKeyValue<TKey, TNyxQueryCount>>;
type
  TResult = INyxKeyValue<TKey, TNyxQueryCount>;
begin
  Result := TNyxParallelKeyed<T, TKey, TNyxQueryCount, TResult>.From(
    AKeys, AEqualities, TLongCountKernelFactory<T, TKey>.Create);
end;

constructor TNumericKeyKernel<T, TKey, TState, TValue>.Create(
  const ANumeric: INyxNumericKernel<T, TState, TValue>);
begin
  inherited Create;
  FNumeric := ANumeric;
end;

function TNumericKeyKernel<T, TKey, TState, TValue>.Empty(const AKey: TKey): TState;
begin
  Result := FNumeric.Empty;
end;

procedure TNumericKeyKernel<T, TKey, TState, TValue>.Add(var AState: TState;
  const AValue: T; const AExecution: INyxExecution);
begin
  FNumeric.Add(AState, AValue);
end;

procedure TNumericKeyKernel<T, TKey, TState, TValue>.Merge(var AState: TState;
  const AOther: TState; const AExecution: INyxExecution);
begin
  FNumeric.Merge(AState, AOther);
end;

function TNumericKeyKernel<T, TKey, TState, TValue>.Finish(const AKey: TKey;
  const AState: TState; const AExecution: INyxExecution): INyxKeyValue<TKey, TValue>;
begin
  Result := TNyxKeyValue<TKey, TValue>.Create(AKey, FNumeric.Finish(AState));
end;

constructor TNumericKeyFactory<T, TKey, TState, TValue>.Create(
  const ANumeric: INyxNumericKernel<T, TState, TValue>);
begin
  inherited Create;
  FNumeric := ANumeric;
end;

function TNumericKeyFactory<T, TKey, TState, TValue>.NewKernel(APartition, ASourceOffset: Integer):
  INyxKeyedKernel<T, TKey, TState, INyxKeyValue<TKey, TValue>>;
begin
  Result := TNumericKeyKernel<T, TKey, TState, TValue>.Create(FNumeric);
end;

class function TNyxParallelAggregateBy<T, TKey, TState, TValue>.From(
  const AKeys: INyxPartitionSelectorFactory<T, TKey>; const AEqualities: INyxParallelEqualityFactory<TKey>;
  const ANumeric: INyxNumericKernel<T, TState, TValue>):
  INyxKeyedPlan<T, TKey, TState, INyxKeyValue<TKey, TValue>>;
type
  TResult = INyxKeyValue<TKey, TValue>;
begin

  if ANumeric = nil then
  begin
    raise ENyxQuery.Create('AggregateBy requires an owned numeric/custom kernel');
  end;
  Result := TNyxParallelKeyed<T, TKey, TState, TResult>.From(
    AKeys, AEqualities, TNumericKeyFactory<T, TKey, TState, TValue>.Create(ANumeric));
end;

end.
