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

unit nyx.linq.terminals;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

interface

uses
  SysUtils, nyx.callback.scope, nyx.linq, nyx.linq.numeric, nyx.linq.parallel, nyx.linq.pipeline,
  nyx.linq.reductions, nyx.linq.execution, nyx.scheduler;

type
  { Decisive skips predicate/program entry after a synchronized global decision.
    Already entered callbacks can finish or fail; failure still dominates the
    logical result. Complete evaluates every admitted row, useful when callback
    evaluation itself matters. Neither mode cancels the query on a true decision.
    Both wait for successful owned replies before publishing one Boolean on UI.
    Deferred source capture still reaches EOF before any partition starts. }
  TNyxQuantifierEvaluation = (nqeDecisive, nqeComplete);

  TNyxParallelQuantifiers<T> = class
  public
    { Any(empty)=False; All(empty)=True. Each submission owns fresh decision state,
      even when factories/source are reused. Predicate factories run on UI;
      returned callbacks run on the selected worker/event-loop policy. }
    class function Any(const ASource: INyxSequence<T>;
      const APredicates: INyxPartitionPredicateFactory<T>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<Boolean>;
      const AOptions: TNyxParallelOptions; const AGeneration: INyxCancellationScope = nil;
      AEvaluation: TNyxQuantifierEvaluation = nqeDecisive): INyxParallelRun; overload; static;
    { Presence only, with no application predicate. Capture/failure policies
      remain unchanged; an accepted nonempty source decides on its first row. }
    class function Any(const ASource: INyxSequence<T>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<Boolean>;
      const AOptions: TNyxParallelOptions;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; overload; static;
    class function All(const ASource: INyxSequence<T>;
      const APredicates: INyxPartitionPredicateFactory<T>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<Boolean>;
      const AOptions: TNyxParallelOptions; const AGeneration: INyxCancellationScope = nil;
      AEvaluation: TNyxQuantifierEvaluation = nqeDecisive): INyxParallelRun; static;
  end;
  TNyxProgramQuantifiers<TInput, TOutput> = class
  public
    { Consume the immutable program's accepted outputs; stopped partitions do
      not enter later filter/map stages. Original options/generation are retained. }
    class function Any(const AProgram: INyxParallelProgram<TInput, TOutput>;
      const APredicates: INyxPartitionPredicateFactory<TOutput>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<Boolean>;
      const AGeneration: INyxCancellationScope = nil;
      AEvaluation: TNyxQuantifierEvaluation = nqeDecisive): INyxParallelRun; overload; static;
    class function Any(const AProgram: INyxParallelProgram<TInput, TOutput>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<Boolean>;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; overload; static;
    class function All(const AProgram: INyxParallelProgram<TInput, TOutput>;
      const APredicates: INyxPartitionPredicateFactory<TOutput>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<Boolean>;
      const AGeneration: INyxCancellationScope = nil;
      AEvaluation: TNyxQuantifierEvaluation = nqeDecisive): INyxParallelRun; static;
  end;

  { Fresh comparator ownership is explicit. Factory entry is UI-only; comparison
    is worker/event-loop-only. (-1,0) denotes the independent final merger.
    A mutable comparator must be independent per return. SharedImmutable is an
    explicit promise of immutable/synchronized comparison, not an implicit clone. }
  INyxParallelComparerFactory<T> = interface(IInterface)
    function NewComparer(APartition, ASourceOffset: Integer): INyxComparer<T>;
  end;
  TNyxComparerFactoryFunction<T> = function: INyxComparer<T>;
  TNyxComparerFactoryNested<T> = function: INyxComparer<T>
    {$ifndef PAS2JS}is nested{$endif};
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  TNyxComparerFactoryReference<T> = reference to function: INyxComparer<T>;
  {$endif}
  TNyxParallelComparer<T> = class
  public
    class function From(AFactory: TNyxComparerFactoryFunction<T>):
      INyxParallelComparerFactory<T>; static;
    { Borrow only the factory's frame behind its origin-thread lifetime port.
      Returned comparers still need independent owned worker-safe captures. }
    class function FromNested(AFactory: TNyxComparerFactoryNested<T>;
      const ALease: INyxCallbackLease): INyxParallelComparerFactory<T>; static;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    class function FromInline(const AFactory: TNyxComparerFactoryReference<T>;
      const ACapture: INyxCallbackCapture = nil): INyxParallelComparerFactory<T>; static;
    {$endif}
    class function SharedImmutable(const AComparer: INyxComparer<T>):
      INyxParallelComparerFactory<T>; static;
  end;

  { One retained candidate keeps value, key and original input ordinal together.
    Nil is internal partition absence, never a present default/nil value. Public
    results use INyxOptional<T>, preserving present zero/nil/interface values. }
  INyxExtremum<T, TKey> = interface(IInterface)
    function GetValue: T;
    function GetKey: TKey;
    function GetOrdinal: Integer;
    property Value: T read GetValue;
    property Key: TKey read GetKey;
    property Ordinal: Integer read GetOrdinal;
  end;
  TNyxParallelExtrema<T, TKey> = class
  public
    { Typed MinBy/MaxBy. Keys run once per accepted row; first SOURCE tie wins
      regardless of completion order. Empty input publishes an explicit None.
      Required callers read Value, which raises the ordinary empty exception.
      Comparer/source/factory failures publish nothing; no UI comparison occurs. }
    class function Minimum(const ASource: INyxSequence<T>;
      const AKeys: INyxPartitionSelectorFactory<T, TKey>;
      const AComparers: INyxParallelComparerFactory<TKey>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<INyxOptional<T>>;
      const AOptions: TNyxParallelOptions;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; static;
    class function Maximum(const ASource: INyxSequence<T>;
      const AKeys: INyxPartitionSelectorFactory<T, TKey>;
      const AComparers: INyxParallelComparerFactory<TKey>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<INyxOptional<T>>;
      const AOptions: TNyxParallelOptions;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; static;
  end;
  TNyxProgramExtrema<TInput, TOutput, TKey> = class
  public
    class function Minimum(const AProgram: INyxParallelProgram<TInput, TOutput>;
      const AKeys: INyxPartitionSelectorFactory<TOutput, TKey>;
      const AComparers: INyxParallelComparerFactory<TKey>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<INyxOptional<TOutput>>;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; static;
    class function Maximum(const AProgram: INyxParallelProgram<TInput, TOutput>;
      const AKeys: INyxPartitionSelectorFactory<TOutput, TKey>;
      const AComparers: INyxParallelComparerFactory<TKey>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<INyxOptional<TOutput>>;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; static;
  end;
  { Whole-value Min/Max convenience, with explicit owned comparer policy.
    Identity keys introduce no application callback or projection allocation. }
  TNyxParallelValueExtrema<T> = class
  public
    class function Minimum(const ASource: INyxSequence<T>;
      const AComparers: INyxParallelComparerFactory<T>; const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<INyxOptional<T>>; const AOptions: TNyxParallelOptions;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; static;
    class function Maximum(const ASource: INyxSequence<T>;
      const AComparers: INyxParallelComparerFactory<T>; const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<INyxOptional<T>>; const AOptions: TNyxParallelOptions;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; static;
  end;

  TTruePredicate<T> = class(TInterfacedObject, INyxPredicate<T>)
    function Test(const AValue: T): Boolean;
  end;
  TTruePredicateFactory<T> = class(TInterfacedObject, INyxPartitionPredicateFactory<T>)
    function NewPredicate(APartition, ASourceOffset: Integer): INyxPredicate<T>;
  end;
  TIdentityKey<T> = class(TInterfacedObject, INyxSelector<T, T>)
    function Project(const AValue: T): T;
  end;
  TIdentityKeyFactory<T> = class(TInterfacedObject, INyxPartitionSelectorFactory<T, T>)
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<T, T>;
  end;

  { Compiler-visible implementation support. Each worker owns its callback and
    candidate. Shared decision scopes contain only the atomic monotone flag. }
  TQuantifierAccumulator<T> = class(TInterfacedObject,
    INyxPartitionAccumulator<T, Boolean>, INyxPartitionContinuation)
  private
    FPredicate: INyxPredicate<T>;
    FDecision: INyxCancellationScope;
    FAll: Boolean;
    FValue: Boolean;
    FEvaluation: TNyxQuantifierEvaluation;
  public
    constructor Create(const APredicate: INyxPredicate<T>; const ADecision: INyxCancellationScope;
      AAll: Boolean; AEvaluation: TNyxQuantifierEvaluation);
    procedure Add(const AValue: T; ASourceIndex: Integer; const AExecution: INyxExecution);
    function Finish(const AExecution: INyxExecution): Boolean;
    function ContinuePartition: Boolean;
  end;
  TQuantifierFactory<T> = class(TInterfacedObject, INyxPartitionAccumulatorFactory<T, Boolean>)
  private
    FPredicates: INyxPartitionPredicateFactory<T>;
    FDecision: INyxCancellationScope;
    FAll: Boolean;
    FEvaluation: TNyxQuantifierEvaluation;
  public
    constructor Create(const APredicates: INyxPartitionPredicateFactory<T>;
      AAll: Boolean; AEvaluation: TNyxQuantifierEvaluation);
    function NewAccumulator(APartition, ASourceOffset: Integer): INyxPartitionAccumulator<T, Boolean>;
  end;
  TBooleanMerger = class(TInterfacedObject, INyxParallelMerger<Boolean, Boolean>)
  private
    FAll: Boolean;
  public
    constructor Create(AAll: Boolean);
    function Merge(const APartials: TNyxSequenceArray<Boolean>; const AExecution: INyxExecution): Boolean;
  end;
  TBooleanMergerFactory = class(TInterfacedObject, INyxParallelMergerFactory<Boolean, Boolean>)
  private
    FAll: Boolean;
  public
    constructor Create(AAll: Boolean);
    function NewMerger: INyxParallelMerger<Boolean, Boolean>;
  end;
  TComparerFactory<T> = class(TInterfacedObject, INyxParallelComparerFactory<T>)
  private
    FFactory: TNyxComparerFactoryFunction<T>;
    FShared: INyxComparer<T>;
  public
    constructor Create(AFactory: TNyxComparerFactoryFunction<T>; const AShared: INyxComparer<T>);
    function NewComparer(APartition, ASourceOffset: Integer): INyxComparer<T>;
  end;
  TScopedComparerFactory<T> = class(TInterfacedObject, INyxParallelComparerFactory<T>)
  private
    FFactory: TNyxComparerFactoryNested<T>;
    FLifetime: INyxCallbackLifetime;
  public
    constructor Create(AFactory: TNyxComparerFactoryNested<T>; const ALease: INyxCallbackLease);
    function NewComparer(APartition, ASourceOffset: Integer): INyxComparer<T>;
  end;
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  TInlineComparerFactory<T> = class(TInterfacedObject, INyxParallelComparerFactory<T>)
  private
    FFactory: TNyxComparerFactoryReference<T>;
    FCapture: INyxCallbackCapture;
  public
    constructor Create(const AFactory: TNyxComparerFactoryReference<T>;
      const ACapture: INyxCallbackCapture);
    function NewComparer(APartition, ASourceOffset: Integer): INyxComparer<T>;
  end;
  {$endif}
  TExtremum<T, TKey> = class(TInterfacedObject, INyxExtremum<T, TKey>)
  private
    FValue: T;
    FKey: TKey;
    FOrdinal: Integer;
  public
    constructor Create(const AValue: T; const AKey: TKey; AOrdinal: Integer);
    function GetValue: T;
    function GetKey: TKey;
    function GetOrdinal: Integer;
  end;
  TExtremaAccumulator<T, TKey> = class(TInterfacedObject,
    INyxPartitionAccumulator<T, INyxExtremum<T, TKey>>)
  private
    FKeys: INyxSelector<T, TKey>;
    FComparer: INyxComparer<TKey>;
    FMaximum: Boolean;
    FValue: T;
    FKey: TKey;
    FOrdinal: Integer;
    FHasValue: Boolean;
  public
    constructor Create(const AKeys: INyxSelector<T, TKey>; const AComparer: INyxComparer<TKey>;
      AMaximum: Boolean);
    procedure Add(const AValue: T; ASourceIndex: Integer; const AExecution: INyxExecution);
    function Finish(const AExecution: INyxExecution): INyxExtremum<T, TKey>;
  end;
  TExtremaFactory<T, TKey> = class(TInterfacedObject,
    INyxPartitionAccumulatorFactory<T, INyxExtremum<T, TKey>>)
  private
    FKeys: INyxPartitionSelectorFactory<T, TKey>;
    FComparers: INyxParallelComparerFactory<TKey>;
    FMaximum: Boolean;
  public
    constructor Create(const AKeys: INyxPartitionSelectorFactory<T, TKey>;
      const AComparers: INyxParallelComparerFactory<TKey>; AMaximum: Boolean);
    function NewAccumulator(APartition, ASourceOffset: Integer):
      INyxPartitionAccumulator<T, INyxExtremum<T, TKey>>;
  end;
  TExtremaMerger<T, TKey> = class(TInterfacedObject,
    INyxParallelMerger<INyxExtremum<T, TKey>, INyxOptional<T>>)
  private
    FComparer: INyxComparer<TKey>;
    FMaximum: Boolean;
  public
    constructor Create(const AComparer: INyxComparer<TKey>; AMaximum: Boolean);
    function Merge(const APartials: TNyxSequenceArray<INyxExtremum<T, TKey>>;
      const AExecution: INyxExecution): INyxOptional<T>;
  end;
  TExtremaMergerFactory<T, TKey> = class(TInterfacedObject,
    INyxParallelMergerFactory<INyxExtremum<T, TKey>, INyxOptional<T>>)
  private
    FComparers: INyxParallelComparerFactory<TKey>;
    FMaximum: Boolean;
  public
    constructor Create(const AComparers: INyxParallelComparerFactory<TKey>; AMaximum: Boolean);
    function NewMerger: INyxParallelMerger<INyxExtremum<T, TKey>, INyxOptional<T>>;
  end;

implementation

function TTruePredicate<T>.Test(const AValue: T): Boolean;
begin
  Result := True;
end;

function TTruePredicateFactory<T>.NewPredicate(APartition, ASourceOffset: Integer): INyxPredicate<T>;
begin
  Result := TTruePredicate<T>.Create;
end;

function TIdentityKey<T>.Project(const AValue: T): T;
begin
  Result := AValue;
end;

function TIdentityKeyFactory<T>.NewSelector(APartition, ASourceOffset: Integer): INyxSelector<T, T>;
begin
  Result := TIdentityKey<T>.Create;
end;

constructor TQuantifierAccumulator<T>.Create(const APredicate: INyxPredicate<T>;
  const ADecision: INyxCancellationScope; AAll: Boolean; AEvaluation: TNyxQuantifierEvaluation);
begin
  inherited Create;
  FPredicate := APredicate;
  FDecision := ADecision;
  FAll := AAll;
  FValue := AAll;
  FEvaluation := AEvaluation;
end;

function TQuantifierAccumulator<T>.ContinuePartition: Boolean;
begin
  Result := (FEvaluation = nqeComplete) or not FDecision.Cancelled;
end;

procedure TQuantifierAccumulator<T>.Add(const AValue: T; ASourceIndex: Integer;
  const AExecution: INyxExecution);
var
  LValue: Boolean;
begin

  if AExecution.Cancelled or not ContinuePartition then
  begin
    Exit;
  end;
  LValue := FPredicate.Test(AValue);

  if FAll then
  begin
    FValue := FValue and LValue;
  end
  else
  begin
    FValue := FValue or LValue;
  end;

  if (FEvaluation = nqeDecisive) and (LValue <> FAll) then
  begin
    FDecision.Cancel;
  end;
end;

function TQuantifierAccumulator<T>.Finish(const AExecution: INyxExecution): Boolean;
begin
  Result := FValue;

  if (FEvaluation = nqeDecisive) and FDecision.Cancelled then
  begin
    Result := not FAll;
  end;
end;

constructor TQuantifierFactory<T>.Create(const APredicates: INyxPartitionPredicateFactory<T>;
  AAll: Boolean; AEvaluation: TNyxQuantifierEvaluation);
begin
  inherited Create;

  if APredicates = nil then
  begin
    raise ENyxQuery.Create('Parallel quantifier requires a predicate factory');
  end;
  FPredicates := APredicates;
  FDecision := NewNyxCancellationScope;
  FAll := AAll;
  FEvaluation := AEvaluation;
end;

function TQuantifierFactory<T>.NewAccumulator(APartition, ASourceOffset: Integer):
  INyxPartitionAccumulator<T, Boolean>;
var
  LPredicate: INyxPredicate<T>;
begin
  { Later partitions require no application callback after a decisive result.
    Already admitted predicates may overlap; the worker checks the same token. }

  if (FEvaluation = nqeComplete) or not FDecision.Cancelled then
  begin
    LPredicate := FPredicates.NewPredicate(APartition, ASourceOffset);

    if LPredicate = nil then
    begin
      raise ENyxQuery.Create('Parallel quantifier factory returned no predicate');
    end;
  end;
  Result := TQuantifierAccumulator<T>.Create(LPredicate, FDecision, FAll, FEvaluation);
end;

constructor TBooleanMerger.Create(AAll: Boolean);
begin
  inherited Create;
  FAll := AAll;
end;

function TBooleanMerger.Merge(const APartials: TNyxSequenceArray<Boolean>;
  const AExecution: INyxExecution): Boolean;
var
  LIndex: Integer;
begin
  Result := FAll;
  for LIndex := 0 to High(APartials) do
  begin

    if AExecution.Cancelled then
    begin
      Exit;
    end;

    if FAll then
    begin
      Result := Result and APartials[LIndex];
    end
    else
    begin
      Result := Result or APartials[LIndex];
    end;
  end;
end;

constructor TBooleanMergerFactory.Create(AAll: Boolean);
begin
  inherited Create;
  FAll := AAll;
end;

function TBooleanMergerFactory.NewMerger: INyxParallelMerger<Boolean, Boolean>;
begin
  Result := TBooleanMerger.Create(FAll);
end;

class function TNyxParallelQuantifiers<T>.Any(const ASource: INyxSequence<T>;
  const APredicates: INyxPartitionPredicateFactory<T>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<Boolean>; const AOptions: TNyxParallelOptions;
  const AGeneration: INyxCancellationScope; AEvaluation: TNyxQuantifierEvaluation): INyxParallelRun;
begin
  Result := TNyxParallelReduction<T, Boolean, Boolean>.Submit(ASource,
    TQuantifierFactory<T>.Create(APredicates, False, AEvaluation),
    TBooleanMergerFactory.Create(False), AScheduler, APort, AOptions, AGeneration);
end;

class function TNyxParallelQuantifiers<T>.All(const ASource: INyxSequence<T>;
  const APredicates: INyxPartitionPredicateFactory<T>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<Boolean>; const AOptions: TNyxParallelOptions;
  const AGeneration: INyxCancellationScope; AEvaluation: TNyxQuantifierEvaluation): INyxParallelRun;
begin
  Result := TNyxParallelReduction<T, Boolean, Boolean>.Submit(ASource,
    TQuantifierFactory<T>.Create(APredicates, True, AEvaluation),
    TBooleanMergerFactory.Create(True), AScheduler, APort, AOptions, AGeneration);
end;

class function TNyxParallelQuantifiers<T>.Any(const ASource: INyxSequence<T>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<Boolean>;
  const AOptions: TNyxParallelOptions; const AGeneration: INyxCancellationScope): INyxParallelRun;
begin
  Result := Any(ASource, TTruePredicateFactory<T>.Create, AScheduler, APort, AOptions, AGeneration);
end;

class function TNyxProgramQuantifiers<TInput, TOutput>.Any(
  const AProgram: INyxParallelProgram<TInput, TOutput>;
  const APredicates: INyxPartitionPredicateFactory<TOutput>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<Boolean>; const AGeneration: INyxCancellationScope;
  AEvaluation: TNyxQuantifierEvaluation): INyxParallelRun;
begin
  Result := TNyxProgramReduction<TInput, TOutput, Boolean, Boolean>.Submit(AProgram,
    TQuantifierFactory<TOutput>.Create(APredicates, False, AEvaluation),
    TBooleanMergerFactory.Create(False), AScheduler, APort, AGeneration);
end;

class function TNyxProgramQuantifiers<TInput, TOutput>.All(
  const AProgram: INyxParallelProgram<TInput, TOutput>;
  const APredicates: INyxPartitionPredicateFactory<TOutput>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<Boolean>; const AGeneration: INyxCancellationScope;
  AEvaluation: TNyxQuantifierEvaluation): INyxParallelRun;
begin
  Result := TNyxProgramReduction<TInput, TOutput, Boolean, Boolean>.Submit(AProgram,
    TQuantifierFactory<TOutput>.Create(APredicates, True, AEvaluation),
    TBooleanMergerFactory.Create(True), AScheduler, APort, AGeneration);
end;

class function TNyxProgramQuantifiers<TInput, TOutput>.Any(
  const AProgram: INyxParallelProgram<TInput, TOutput>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<Boolean>; const AGeneration: INyxCancellationScope): INyxParallelRun;
begin
  Result := Any(AProgram, TTruePredicateFactory<TOutput>.Create, AScheduler, APort, AGeneration);
end;

constructor TComparerFactory<T>.Create(AFactory: TNyxComparerFactoryFunction<T>;
  const AShared: INyxComparer<T>);
begin
  inherited Create;
  FFactory := AFactory;
  FShared := AShared;
end;

function TComparerFactory<T>.NewComparer(APartition, ASourceOffset: Integer): INyxComparer<T>;
begin

  if Assigned(FFactory) then
  begin
    Result := FFactory();
  end
  else
  begin
    Result := FShared;
  end;

  if Result = nil then
  begin
    raise ENyxQuery.Create('Parallel comparer factory returned no comparer');
  end;
end;

class function TNyxParallelComparer<T>.From(AFactory: TNyxComparerFactoryFunction<T>):
  INyxParallelComparerFactory<T>;
begin

  if not Assigned(AFactory) then
  begin
    raise ENyxQuery.Create('Parallel comparer requires a factory');
  end;
  Result := TComparerFactory<T>.Create(AFactory, nil);
end;

class function TNyxParallelComparer<T>.SharedImmutable(const AComparer: INyxComparer<T>):
  INyxParallelComparerFactory<T>;
begin

  if AComparer = nil then
  begin
    raise ENyxQuery.Create('Parallel comparer requires an immutable comparer');
  end;
  Result := TComparerFactory<T>.Create(nil, AComparer);
end;

constructor TScopedComparerFactory<T>.Create(AFactory: TNyxComparerFactoryNested<T>;
  const ALease: INyxCallbackLease);
begin
  inherited Create;

  if (ALease = nil) or not Assigned(AFactory) then
  begin
    raise ENyxQuery.Create('Nested comparer factory requires a function and lifetime lease');
  end;
  FLifetime := ALease.Lifetime;
  FLifetime.RequireEntry;
  FFactory := AFactory;
end;

function TScopedComparerFactory<T>.NewComparer(APartition, ASourceOffset: Integer): INyxComparer<T>;
begin
  FLifetime.RequireEntry;
  Result := FFactory();

  if Result = nil then
  begin
    raise ENyxQuery.Create('Nested comparer factory returned no comparer');
  end;
end;

class function TNyxParallelComparer<T>.FromNested(AFactory: TNyxComparerFactoryNested<T>;
  const ALease: INyxCallbackLease): INyxParallelComparerFactory<T>;
begin
  Result := TScopedComparerFactory<T>.Create(AFactory, ALease);
end;

{$ifdef NYX_HAS_INLINE_CALLBACKS}
constructor TInlineComparerFactory<T>.Create(const AFactory: TNyxComparerFactoryReference<T>;
  const ACapture: INyxCallbackCapture);
begin
  inherited Create;

  if not Assigned(AFactory) then
  begin
    raise ENyxQuery.Create('Inline comparer requires a factory');
  end;
  FFactory := AFactory;
  FCapture := ACapture;
end;

function TInlineComparerFactory<T>.NewComparer(APartition, ASourceOffset: Integer): INyxComparer<T>;
begin
  Result := FFactory();

  if Result = nil then
  begin
    raise ENyxQuery.Create('Inline comparer factory returned no comparer');
  end;
end;

class function TNyxParallelComparer<T>.FromInline(const AFactory: TNyxComparerFactoryReference<T>;
  const ACapture: INyxCallbackCapture): INyxParallelComparerFactory<T>;
begin
  Result := TInlineComparerFactory<T>.Create(AFactory, ACapture);
end;
{$endif}

constructor TExtremum<T, TKey>.Create(const AValue: T; const AKey: TKey; AOrdinal: Integer);
begin
  inherited Create;
  FValue := AValue;
  FKey := AKey;
  FOrdinal := AOrdinal;
end;

function TExtremum<T, TKey>.GetValue: T;
begin
  Result := FValue;
end;

function TExtremum<T, TKey>.GetKey: TKey;
begin
  Result := FKey;
end;

function TExtremum<T, TKey>.GetOrdinal: Integer;
begin
  Result := FOrdinal;
end;

constructor TExtremaAccumulator<T, TKey>.Create(const AKeys: INyxSelector<T, TKey>;
  const AComparer: INyxComparer<TKey>; AMaximum: Boolean);
begin
  inherited Create;
  FKeys := AKeys;
  FComparer := AComparer;
  FMaximum := AMaximum;
end;

procedure TExtremaAccumulator<T, TKey>.Add(const AValue: T; ASourceIndex: Integer;
  const AExecution: INyxExecution);
var
  LKey: TKey;
  LSign: Integer;
  LReplace: Boolean;
begin

  if AExecution.Cancelled then
  begin
    Exit;
  end;
  LKey := FKeys.Project(AValue);
  LReplace := not FHasValue;

  if not LReplace then
  begin
    LSign := FComparer.Compare(LKey, FKey);
    LReplace := (FMaximum and (LSign > 0)) or (not FMaximum and (LSign < 0)) or
      ((LSign = 0) and (ASourceIndex < FOrdinal));
  end;

  if LReplace then
  begin
    { The worker owns one mutable candidate slot. Allocate its immutable
      publication only in Finish, rather than allocating per improving row. }
    FValue := AValue;
    FKey := LKey;
    FOrdinal := ASourceIndex;
    FHasValue := True;
  end;
end;

function TExtremaAccumulator<T, TKey>.Finish(const AExecution: INyxExecution): INyxExtremum<T, TKey>;
begin
  Result := nil;

  if FHasValue then
  begin
    Result := TExtremum<T, TKey>.Create(FValue, FKey, FOrdinal);
  end;
end;

constructor TExtremaFactory<T, TKey>.Create(const AKeys: INyxPartitionSelectorFactory<T, TKey>;
  const AComparers: INyxParallelComparerFactory<TKey>; AMaximum: Boolean);
begin
  inherited Create;

  if (AKeys = nil) or (AComparers = nil) then
  begin
    raise ENyxQuery.Create('Parallel extrema require keys and comparers');
  end;
  FKeys := AKeys;
  FComparers := AComparers;
  FMaximum := AMaximum;
end;

function TExtremaFactory<T, TKey>.NewAccumulator(APartition, ASourceOffset: Integer):
  INyxPartitionAccumulator<T, INyxExtremum<T, TKey>>;
var
  LKey: INyxSelector<T, TKey>;
  LComparer: INyxComparer<TKey>;
begin
  LKey := FKeys.NewSelector(APartition, ASourceOffset);
  LComparer := FComparers.NewComparer(APartition, ASourceOffset);

  if (LKey = nil) or (LComparer = nil) then
  begin
    raise ENyxQuery.Create('Parallel extrema factory returned no callback');
  end;
  Result := TExtremaAccumulator<T, TKey>.Create(LKey, LComparer, FMaximum);
end;

constructor TExtremaMerger<T, TKey>.Create(const AComparer: INyxComparer<TKey>; AMaximum: Boolean);
begin
  inherited Create;
  FComparer := AComparer;
  FMaximum := AMaximum;
end;

function TExtremaMerger<T, TKey>.Merge(const APartials: TNyxSequenceArray<INyxExtremum<T, TKey>>;
  const AExecution: INyxExecution): INyxOptional<T>;
var
  LBest: INyxExtremum<T, TKey>;
  LValue: INyxExtremum<T, TKey>;
  LIndex: Integer;
  LSign: Integer;
  LReplace: Boolean;
begin
  LBest := nil;
  for LIndex := 0 to High(APartials) do
  begin

    if AExecution.Cancelled then
    begin
      Exit(TNyxOptional<T>.None);
    end;
    LValue := APartials[LIndex];

    if LValue = nil then
    begin
      Continue;
    end;
    LReplace := LBest = nil;

    if not LReplace then
    begin
      LSign := FComparer.Compare(LValue.Key, LBest.Key);
      LReplace := (FMaximum and (LSign > 0)) or (not FMaximum and (LSign < 0)) or
        ((LSign = 0) and (LValue.Ordinal < LBest.Ordinal));
    end;

    if LReplace then
    begin
      LBest := LValue;
    end;
  end;

  if LBest = nil then
  begin
    Result := TNyxOptional<T>.None;
  end
  else
  begin
    Result := TNyxOptional<T>.Some(LBest.Value);
  end;
end;

constructor TExtremaMergerFactory<T, TKey>.Create(
  const AComparers: INyxParallelComparerFactory<TKey>; AMaximum: Boolean);
begin
  inherited Create;
  FComparers := AComparers;
  FMaximum := AMaximum;
end;

function TExtremaMergerFactory<T, TKey>.NewMerger:
  INyxParallelMerger<INyxExtremum<T, TKey>, INyxOptional<T>>;
var
  LComparer: INyxComparer<TKey>;
begin
  LComparer := FComparers.NewComparer(-1, 0);

  if LComparer = nil then
  begin
    raise ENyxQuery.Create('Parallel extrema merger requires a comparer');
  end;
  Result := TExtremaMerger<T, TKey>.Create(LComparer, FMaximum);
end;

class function TNyxParallelExtrema<T, TKey>.Minimum(const ASource: INyxSequence<T>;
  const AKeys: INyxPartitionSelectorFactory<T, TKey>;
  const AComparers: INyxParallelComparerFactory<TKey>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<INyxOptional<T>>; const AOptions: TNyxParallelOptions;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
begin
  Result := TNyxParallelReduction<T, INyxExtremum<T, TKey>, INyxOptional<T>>.Submit(ASource,
    TExtremaFactory<T, TKey>.Create(AKeys, AComparers, False),
    TExtremaMergerFactory<T, TKey>.Create(AComparers, False), AScheduler, APort, AOptions, AGeneration);
end;

class function TNyxParallelExtrema<T, TKey>.Maximum(const ASource: INyxSequence<T>;
  const AKeys: INyxPartitionSelectorFactory<T, TKey>;
  const AComparers: INyxParallelComparerFactory<TKey>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<INyxOptional<T>>; const AOptions: TNyxParallelOptions;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
begin
  Result := TNyxParallelReduction<T, INyxExtremum<T, TKey>, INyxOptional<T>>.Submit(ASource,
    TExtremaFactory<T, TKey>.Create(AKeys, AComparers, True),
    TExtremaMergerFactory<T, TKey>.Create(AComparers, True), AScheduler, APort, AOptions, AGeneration);
end;

class function TNyxProgramExtrema<TInput, TOutput, TKey>.Minimum(
  const AProgram: INyxParallelProgram<TInput, TOutput>;
  const AKeys: INyxPartitionSelectorFactory<TOutput, TKey>;
  const AComparers: INyxParallelComparerFactory<TKey>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<INyxOptional<TOutput>>;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
begin
  Result := TNyxProgramReduction<TInput, TOutput, INyxExtremum<TOutput, TKey>,
    INyxOptional<TOutput>>.Submit(AProgram, TExtremaFactory<TOutput, TKey>.Create(AKeys, AComparers, False),
    TExtremaMergerFactory<TOutput, TKey>.Create(AComparers, False), AScheduler, APort, AGeneration);
end;

class function TNyxProgramExtrema<TInput, TOutput, TKey>.Maximum(
  const AProgram: INyxParallelProgram<TInput, TOutput>;
  const AKeys: INyxPartitionSelectorFactory<TOutput, TKey>;
  const AComparers: INyxParallelComparerFactory<TKey>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<INyxOptional<TOutput>>;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
begin
  Result := TNyxProgramReduction<TInput, TOutput, INyxExtremum<TOutput, TKey>,
    INyxOptional<TOutput>>.Submit(AProgram, TExtremaFactory<TOutput, TKey>.Create(AKeys, AComparers, True),
    TExtremaMergerFactory<TOutput, TKey>.Create(AComparers, True), AScheduler, APort, AGeneration);
end;

class function TNyxParallelValueExtrema<T>.Minimum(const ASource: INyxSequence<T>;
  const AComparers: INyxParallelComparerFactory<T>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<INyxOptional<T>>; const AOptions: TNyxParallelOptions;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
begin
  Result := TNyxParallelExtrema<T, T>.Minimum(ASource, TIdentityKeyFactory<T>.Create,
    AComparers, AScheduler, APort, AOptions, AGeneration);
end;

class function TNyxParallelValueExtrema<T>.Maximum(const ASource: INyxSequence<T>;
  const AComparers: INyxParallelComparerFactory<T>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<INyxOptional<T>>; const AOptions: TNyxParallelOptions;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
begin
  Result := TNyxParallelExtrema<T, T>.Maximum(ASource, TIdentityKeyFactory<T>.Create,
    AComparers, AScheduler, APort, AOptions, AGeneration);
end;

end.
