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
unit nyx.linq.pipeline;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.linq, nyx.linq.numeric, nyx.linq.parallel, nyx.linq.execution, nyx.scheduler;

type
  { Parallel factory entry occurs once per partition on UI. AsSequential enters
    its factory on the enumerating caller's thread. Its returned predicate owns
    independent state or caller-synchronized shared immutable state, just like
    partition selectors. Worker callbacks may not read live UI/collections. }
  INyxPartitionPredicateFactory<T> = interface(IInterface)
    function NewPredicate(APartition, ASourceOffset: Integer): INyxPredicate<T>;
  end;
  TNyxPartitionPredicate<T> = class
  public
    { Global function adapter creates a fresh predicate per partition. Global
      state remains the caller's thread-safety/lifetime responsibility. }
    class function From(AFunction: TNyxPredicateFunction<T>):
      INyxPartitionPredicateFactory<T>; static;
  end;

  { Immutable typed program. Where/Select/WithOptions return independently owned
    programs, never alter earlier recipes or enter sources/callbacks. All row
    stages execute on the selected worker/event-loop policy in source order.
    Filtering excludes only explicit None; later stages never enter filtered rows.
    Each partition owns a fresh entire callback chain. Managed results retain
    ordinarily; raw objects/inner mutable arrays retain the existing query contract.
    Submit captures the source through the bounded shared capture/coordinator,
    preserves configured ordering/diagnostics/cancellation, and publishes exactly
    one complete filtered result through the ordinary revocable destination.
    Cancellation is cooperative between input rows; an entered row chain completes.
    AsSequential exposes the same stages through an independent ordinary query.
    Type-changing maps use TNyxParallelMap because Pascal has no generic methods.
    Submit's optional AGeneration ties THIS run to an owning mounted view; it does
    not change the reusable program or later independent submissions. Navigation
    cancels capture, later worker rows and complete delivery cooperatively. }
  INyxParallelProgram<TInput, TOutput> = interface(IInterface)
    function Where(const APredicate: INyxPartitionPredicateFactory<TOutput>):
      INyxParallelProgram<TInput, TOutput>;
    function Select(const ASelector: INyxPartitionSelectorFactory<TOutput, TOutput>):
      INyxParallelProgram<TInput, TOutput>;
    function WithOptions(const AOptions: TNyxParallelOptions):
      INyxParallelProgram<TInput, TOutput>;
    function AsSequential: INyxSequence<TOutput>;
    function Submit(const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<TOutput>;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun;
    { Read-only typed extension ports let new program stages compose without
      untyped casts, mutable program state, source evaluation or UI back-links. }
    function GetSource: INyxSequence<TInput>;
    function GetFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
    function GetOptions: TNyxParallelOptions;
    property Source: INyxSequence<TInput> read GetSource;
    property Factory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>> read GetFactory;
    property Options: TNyxParallelOptions read GetOptions;
  end;
  TNyxParallelSource<T> = class
  public
    { Retain a deferred source, with independent identity stages and defaults. }
    class function From(const ASource: INyxSequence<T>): INyxParallelProgram<T, T>; static;
  end;
  TNyxParallelMap<TInput, TPrevious, TOutput> = class
  public
    { Fuse a typed result-changing stage into the existing per-row worker chain.
      The previous program and its options remain unchanged and independently usable. }
    class function From(const AProgram: INyxParallelProgram<TInput, TPrevious>;
      const ASelector: INyxPartitionSelectorFactory<TPrevious, TOutput>):
      INyxParallelProgram<TInput, TOutput>; static;
  end;

  { Compiler-visible owned support for native template expansion. Constructors
    are internal composition support; applications use the contracts above. }
  TQueryPredicateFactory<T> = class(TInterfacedObject, INyxPartitionPredicateFactory<T>)
  private
    FFunction: TNyxPredicateFunction<T>;
  public
    constructor Create(AFunction: TNyxPredicateFunction<T>);
    function NewPredicate(APartition, ASourceOffset: Integer): INyxPredicate<T>;
  end;
  TQueryIdentityProgram<T> = class(TInterfacedObject, INyxSelector<T, INyxOptional<T>>)
    function Project(const AValue: T): INyxOptional<T>;
  end;
  TQueryIdentityFactory<T> = class(TInterfacedObject,
    INyxPartitionSelectorFactory<T, INyxOptional<T>>)
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<T, INyxOptional<T>>;
  end;
  TQueryFilterProgram<TInput, TOutput> = class(TInterfacedObject,
    INyxSelector<TInput, INyxOptional<TOutput>>)
  private
    FPrevious: INyxSelector<TInput, INyxOptional<TOutput>>;
    FPredicate: INyxPredicate<TOutput>;
  public
    constructor Create(const APrevious: INyxSelector<TInput, INyxOptional<TOutput>>;
      const APredicate: INyxPredicate<TOutput>);
    function Project(const AValue: TInput): INyxOptional<TOutput>;
  end;
  TQueryFilterFactory<TInput, TOutput> = class(TInterfacedObject,
    INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>)
  private
    FPrevious: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
    FPredicate: INyxPartitionPredicateFactory<TOutput>;
  public
    constructor Create(const APrevious: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
      const APredicate: INyxPartitionPredicateFactory<TOutput>);
    function NewSelector(APartition, ASourceOffset: Integer):
      INyxSelector<TInput, INyxOptional<TOutput>>;
  end;
  TQueryMapProgram<TInput, TPrevious, TOutput> = class(TInterfacedObject,
    INyxSelector<TInput, INyxOptional<TOutput>>)
  private
    FPrevious: INyxSelector<TInput, INyxOptional<TPrevious>>;
    FSelector: INyxSelector<TPrevious, TOutput>;
  public
    constructor Create(const APrevious: INyxSelector<TInput, INyxOptional<TPrevious>>;
      const ASelector: INyxSelector<TPrevious, TOutput>);
    function Project(const AValue: TInput): INyxOptional<TOutput>;
  end;
  TQueryMapFactory<TInput, TPrevious, TOutput> = class(TInterfacedObject,
    INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>)
  private
    FPrevious: INyxPartitionSelectorFactory<TInput, INyxOptional<TPrevious>>;
    FSelector: INyxPartitionSelectorFactory<TPrevious, TOutput>;
  public
    constructor Create(const APrevious: INyxPartitionSelectorFactory<TInput, INyxOptional<TPrevious>>;
      const ASelector: INyxPartitionSelectorFactory<TPrevious, TOutput>);
    function NewSelector(APartition, ASourceOffset: Integer):
      INyxSelector<TInput, INyxOptional<TOutput>>;
  end;
  TQueryProgramPort<T> = class(TInterfacedObject, INyxQueryResultPort<INyxOptional<T>>)
  private
    FDestination: INyxQueryResultPort<T>;
  public
    constructor Create(const ADestination: INyxQueryResultPort<T>);
    function Publish(const AValues: TNyxSequenceArray<INyxOptional<T>>): Boolean;
  end;
  TQueryProgramCursor<TInput, TOutput> = class(TEnumerator<TOutput>)
  private
    FSource: INyxSequence<TInput>;
    FCursor: INyxEnumerator<TInput>;
    FFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
    FProgram: INyxSelector<TInput, INyxOptional<TOutput>>;
    procedure Retire;
  protected
    function Advance(out AValue: TOutput): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<TInput>;
      const AFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>);
  end;
  TQueryProgramSequence<TInput, TOutput> = class(TNyxSequence<TOutput>)
  private
    FSource: INyxSequence<TInput>;
    FFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
  public
    constructor Create(const ASource: INyxSequence<TInput>;
      const AFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>);
    function GetEnumerator: INyxEnumerator<TOutput>; override;
  end;
  TQueryParallelProgram<TInput, TOutput> = class(TInterfacedObject,
    INyxParallelProgram<TInput, TOutput>)
  private
    FSource: INyxSequence<TInput>;
    FFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
    FOptions: TNyxParallelOptions;
  public
    constructor Create(const ASource: INyxSequence<TInput>;
      const AFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
      const AOptions: TNyxParallelOptions);
    function Where(const APredicate: INyxPartitionPredicateFactory<TOutput>):
      INyxParallelProgram<TInput, TOutput>;
    function Select(const ASelector: INyxPartitionSelectorFactory<TOutput, TOutput>):
      INyxParallelProgram<TInput, TOutput>;
    function WithOptions(const AOptions: TNyxParallelOptions):
      INyxParallelProgram<TInput, TOutput>;
    function AsSequential: INyxSequence<TOutput>;
    function Submit(const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<TOutput>;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun;
    function GetSource: INyxSequence<TInput>;
    function GetFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
    function GetOptions: TNyxParallelOptions;
  end;

implementation

constructor TQueryPredicateFactory<T>.Create(AFunction: TNyxPredicateFunction<T>);
begin
  inherited Create;

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('Partition predicate requires a function');
  end;
  FFunction := AFunction;
end;

function TQueryPredicateFactory<T>.NewPredicate(APartition,
  ASourceOffset: Integer): INyxPredicate<T>;
begin
  Result := TNyxPredicate<T>.From(FFunction);
end;

class function TNyxPartitionPredicate<T>.From(AFunction: TNyxPredicateFunction<T>):
  INyxPartitionPredicateFactory<T>;
begin
  Result := TQueryPredicateFactory<T>.Create(AFunction);
end;

function TQueryIdentityProgram<T>.Project(const AValue: T): INyxOptional<T>;
begin
  Result := TNyxOptional<T>.Some(AValue);
end;

function TQueryIdentityFactory<T>.NewSelector(APartition, ASourceOffset: Integer):
  INyxSelector<T, INyxOptional<T>>;
begin
  Result := TQueryIdentityProgram<T>.Create;
end;

constructor TQueryFilterProgram<TInput, TOutput>.Create(
  const APrevious: INyxSelector<TInput, INyxOptional<TOutput>>;
  const APredicate: INyxPredicate<TOutput>);
begin
  inherited Create;
  FPrevious := APrevious;
  FPredicate := APredicate;
end;

function TQueryFilterProgram<TInput, TOutput>.Project(const AValue: TInput):
  INyxOptional<TOutput>;
var
  LPrevious: INyxOptional<TOutput>;
  LValue: TOutput;
begin
  LPrevious := FPrevious.Project(AValue);

  if LPrevious = nil then
  begin
    raise ENyxQuery.Create('Program stage returned no optional value');
  end;
  Result := LPrevious;

  if LPrevious.HasValue then
  begin
    LValue := LPrevious.Value;

    if not FPredicate.Test(LValue) then
    begin
      Result := TNyxOptional<TOutput>.None;
    end;
  end;
end;

constructor TQueryFilterFactory<TInput, TOutput>.Create(
  const APrevious: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
  const APredicate: INyxPartitionPredicateFactory<TOutput>);
begin
  inherited Create;
  FPrevious := APrevious;
  FPredicate := APredicate;
end;

function TQueryFilterFactory<TInput, TOutput>.NewSelector(APartition, ASourceOffset: Integer):
  INyxSelector<TInput, INyxOptional<TOutput>>;
var
  LPrevious: INyxSelector<TInput, INyxOptional<TOutput>>;
  LPredicate: INyxPredicate<TOutput>;
begin
  LPrevious := FPrevious.NewSelector(APartition, ASourceOffset);
  LPredicate := FPredicate.NewPredicate(APartition, ASourceOffset);

  if (LPrevious = nil) or (LPredicate = nil) then
  begin
    raise ENyxQuery.Create('Program filter factory returned an incomplete callback chain');
  end;
  Result := TQueryFilterProgram<TInput, TOutput>.Create(LPrevious, LPredicate);
end;

constructor TQueryMapProgram<TInput, TPrevious, TOutput>.Create(
  const APrevious: INyxSelector<TInput, INyxOptional<TPrevious>>;
  const ASelector: INyxSelector<TPrevious, TOutput>);
begin
  inherited Create;
  FPrevious := APrevious;
  FSelector := ASelector;
end;

function TQueryMapProgram<TInput, TPrevious, TOutput>.Project(const AValue: TInput):
  INyxOptional<TOutput>;
var
  LPrevious: INyxOptional<TPrevious>;
  LValue: TPrevious;
  LOutput: TOutput;
begin
  LPrevious := FPrevious.Project(AValue);

  if LPrevious = nil then
  begin
    raise ENyxQuery.Create('Program stage returned no optional value');
  end;

  if not LPrevious.HasValue then
  begin
    Result := TNyxOptional<TOutput>.None;
    Exit;
  end;
  LValue := LPrevious.Value;
  LOutput := FSelector.Project(LValue);
  Result := TNyxOptional<TOutput>.Some(LOutput);
end;

constructor TQueryMapFactory<TInput, TPrevious, TOutput>.Create(
  const APrevious: INyxPartitionSelectorFactory<TInput, INyxOptional<TPrevious>>;
  const ASelector: INyxPartitionSelectorFactory<TPrevious, TOutput>);
begin
  inherited Create;
  FPrevious := APrevious;
  FSelector := ASelector;
end;

function TQueryMapFactory<TInput, TPrevious, TOutput>.NewSelector(APartition,
  ASourceOffset: Integer): INyxSelector<TInput, INyxOptional<TOutput>>;
var
  LPrevious: INyxSelector<TInput, INyxOptional<TPrevious>>;
  LSelector: INyxSelector<TPrevious, TOutput>;
begin
  LPrevious := FPrevious.NewSelector(APartition, ASourceOffset);
  LSelector := FSelector.NewSelector(APartition, ASourceOffset);

  if (LPrevious = nil) or (LSelector = nil) then
  begin
    raise ENyxQuery.Create('Program map factory returned an incomplete callback chain');
  end;
  Result := TQueryMapProgram<TInput, TPrevious, TOutput>.Create(LPrevious, LSelector);
end;

constructor TQueryProgramPort<T>.Create(const ADestination: INyxQueryResultPort<T>);
begin
  inherited Create;
  FDestination := ADestination;
end;

function TQueryProgramPort<T>.Publish(const AValues: TNyxSequenceArray<INyxOptional<T>>): Boolean;
var
  LBuffer: TQueryBuffer<T>;
  LValues: TNyxSequenceArray<T>;
  LIndex: Integer;
  LValue: T;
begin
  { Worker results arrive only on UI after complete successful coordination.
    This detached final vector applies no user callback and touches no live store. }
  LBuffer := TQueryBuffer<T>.Create(High(Integer));
  try
    for LIndex := 0 to High(AValues) do
    begin

      if AValues[LIndex] = nil then
      begin
        raise ENyxQuery.Create('Completed program contains a nil optional value');
      end;

      if AValues[LIndex].HasValue then
      begin
        LValue := AValues[LIndex].Value;
        LBuffer.Add(LValue);
      end;
    end;
    LValues := LBuffer.ToArray;
  finally
    LBuffer.Free;
  end;
  Result := FDestination.Publish(LValues);
end;

constructor TQueryProgramCursor<TInput, TOutput>.Create(const ASource: INyxSequence<TInput>;
  const AFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>);
begin
  inherited Create;
  FSource := ASource;
  FFactory := AFactory;
end;

procedure TQueryProgramCursor<TInput, TOutput>.Retire;
begin
  FSource := nil;
  FCursor := nil;
  FFactory := nil;
  FProgram := nil;
end;

function TQueryProgramCursor<TInput, TOutput>.Advance(out AValue: TOutput): Boolean;
var
  LValue: INyxOptional<TOutput>;
begin
  Result := False;
  try

    if FCursor = nil then
    begin
      FCursor := FSource.GetEnumerator;
      FSource := nil;

      if FCursor = nil then
      begin
        raise ENyxQuery.Create('Sequential program source returned no cursor');
      end;
    end;
    while FCursor.MoveNext do
    begin

      if FProgram = nil then
      begin
        FProgram := FFactory.NewSelector(0, 0);
        FFactory := nil;

        if FProgram = nil then
        begin
          raise ENyxQuery.Create('Sequential program factory returned no callback');
        end;
      end;
      LValue := FProgram.Project(FCursor.Current);

      if LValue = nil then
      begin
        raise ENyxQuery.Create('Sequential program returned no optional value');
      end;

      if LValue.HasValue then
      begin
        AValue := LValue.Value;
        Result := True;
        Exit;
      end;
    end;
    Retire;
  except
    Retire;
    raise;
  end;
end;

constructor TQueryProgramSequence<TInput, TOutput>.Create(const ASource: INyxSequence<TInput>;
  const AFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>);
begin
  inherited Create;
  FSource := ASource;
  FFactory := AFactory;
end;

function TQueryProgramSequence<TInput, TOutput>.GetEnumerator: INyxEnumerator<TOutput>;
begin
  Result := TQueryProgramCursor<TInput, TOutput>.Create(FSource, FFactory);
end;

constructor TQueryParallelProgram<TInput, TOutput>.Create(const ASource: INyxSequence<TInput>;
  const AFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
  const AOptions: TNyxParallelOptions);
begin
  inherited Create;
  FSource := ASource;
  FFactory := AFactory;
  FOptions := AOptions;
end;

function TQueryParallelProgram<TInput, TOutput>.Where(
  const APredicate: INyxPartitionPredicateFactory<TOutput>): INyxParallelProgram<TInput, TOutput>;
var
  LFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
begin

  if APredicate = nil then
  begin
    raise ENyxQuery.Create('Program Where requires a predicate factory');
  end;
  LFactory := TQueryFilterFactory<TInput, TOutput>.Create(FFactory, APredicate);
  Result := TQueryParallelProgram<TInput, TOutput>.Create(FSource, LFactory, FOptions);
end;

function TQueryParallelProgram<TInput, TOutput>.Select(
  const ASelector: INyxPartitionSelectorFactory<TOutput, TOutput>): INyxParallelProgram<TInput, TOutput>;
var
  LFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
begin

  if ASelector = nil then
  begin
    raise ENyxQuery.Create('Program Select requires a selector factory');
  end;
  LFactory := TQueryMapFactory<TInput, TOutput, TOutput>.Create(FFactory, ASelector);
  Result := TQueryParallelProgram<TInput, TOutput>.Create(FSource, LFactory, FOptions);
end;

function TQueryParallelProgram<TInput, TOutput>.WithOptions(
  const AOptions: TNyxParallelOptions): INyxParallelProgram<TInput, TOutput>;
begin
  AOptions.Validate;
  Result := TQueryParallelProgram<TInput, TOutput>.Create(FSource, FFactory, AOptions);
end;

function TQueryParallelProgram<TInput, TOutput>.AsSequential: INyxSequence<TOutput>;
begin
  Result := TQueryProgramSequence<TInput, TOutput>.Create(FSource, FFactory);
end;

function TQueryParallelProgram<TInput, TOutput>.Submit(const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<TOutput>;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
var
  LPort: INyxQueryResultPort<INyxOptional<TOutput>>;
begin

  if APort = nil then
  begin
    raise ENyxQuery.Create('Program submission requires a result port');
  end;
  LPort := TQueryProgramPort<TOutput>.Create(APort);
  Result := TNyxParallelSelect<TInput, INyxOptional<TOutput>>.Submit(
    FSource, FFactory, AScheduler, LPort, FOptions, AGeneration);
end;

function TQueryParallelProgram<TInput, TOutput>.GetSource: INyxSequence<TInput>;
begin
  Result := FSource;
end;

function TQueryParallelProgram<TInput, TOutput>.GetFactory:
  INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
begin
  Result := FFactory;
end;

function TQueryParallelProgram<TInput, TOutput>.GetOptions: TNyxParallelOptions;
begin
  Result := FOptions;
end;

class function TNyxParallelSource<T>.From(const ASource: INyxSequence<T>):
  INyxParallelProgram<T, T>;
var
  LFactory: INyxPartitionSelectorFactory<T, INyxOptional<T>>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Parallel program requires a source');
  end;
  LFactory := TQueryIdentityFactory<T>.Create;
  Result := TQueryParallelProgram<T, T>.Create(ASource, LFactory, TNyxParallelOptions.Defaults);
end;

class function TNyxParallelMap<TInput, TPrevious, TOutput>.From(
  const AProgram: INyxParallelProgram<TInput, TPrevious>;
  const ASelector: INyxPartitionSelectorFactory<TPrevious, TOutput>):
  INyxParallelProgram<TInput, TOutput>;
var
  LFactory: INyxPartitionSelectorFactory<TInput, INyxOptional<TOutput>>;
begin

  if (AProgram = nil) or (ASelector = nil) then
  begin
    raise ENyxQuery.Create('Type-changing program map requires a program and selector factory');
  end;
  LFactory := TQueryMapFactory<TInput, TPrevious, TOutput>.Create(AProgram.Factory, ASelector);
  Result := TQueryParallelProgram<TInput, TOutput>.Create(AProgram.Source, LFactory, AProgram.Options);
end;

end.
