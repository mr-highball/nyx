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

program nyx_linq_grouping_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

uses
  SysUtils, Classes, nyx.text, nyx.model, nyx.codec, nyx.generated.view,
  nyx.collections, nyx.callback.scope, nyx.linq, nyx.linq.grouping,
  nyx.linq.grouping.parallel, nyx.linq.aggregates, nyx.linq.numeric,
  nyx.linq.numeric.parallel, nyx.linq.parallel, nyx.linq.pipeline,
  nyx.linq.execution, nyx.scheduler
  {$ifdef PAS2JS}, Web, nyx.application.browser
  {$else}, Interfaces, Forms, Grids, SyncObjs, nyx.application.lcl{$endif};

type
  {$ifdef PAS2JS}
  TApplicationHost = TNyxBrowserApplication;
  {$else}
  TApplicationHost = TNyxLCLApplication;
  {$endif}
  TRow = record
    ID: Integer;
    Key: TNyxText;
    Value: Integer;
  end;
  TGroup = INyxGroup<TNyxText, Integer>;
  TGroups = INyxSequence<TGroup>;
  TPair = INyxKeyValue<TNyxText, Integer>;
  TPairs = INyxSequence<TPair>;
  TLongPair = INyxKeyValue<TNyxText, TNyxQueryCount>;
  TLongPairs = INyxSequence<TLongPair>;
  TGroupPlan = INyxKeyedPlan<TRow, TNyxText, INyxElementBuffer<Integer>, TGroup>;
  TCountPlan = INyxKeyedPlan<TRow, TNyxText, Integer, TPair>;
  TLongPlan = INyxKeyedPlan<TRow, TNyxText, TNyxQueryCount, TLongPair>;
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  IEqualitySeed = interface(IInterface)
    ['{7B7B5A2A-D00D-4B52-A289-01E68D11FA25}']
    function NewEquality: INyxEquality<TNyxText>;
  end;
  TEqualitySeed = class(TInterfacedObject, IEqualitySeed)
  public
    constructor Create;
    destructor Destroy; override;
    function NewEquality: INyxEquality<TNyxText>;
  end;
  {$endif}
  IProbe = interface(IInterface)
    ['{99A2B9B8-0DE2-4805-AFBB-B337C50C7A3C}']
  end;
  { Immutable settings and synchronized test-only counters/gates. Workers own
    no UI/store/document reference. UI reads counters after actual retirement. }
  TProbe = class(TInterfacedObject, IProbe)
  public
    Keys: LongInt;
    Elements: LongInt;
    Hashes: LongInt;
    Equalities: LongInt;
    Seeds: LongInt;
    Adds: LongInt;
    Merges: LongInt;
    Finishes: LongInt;
    UICalls: LongInt;
    Collisions: Boolean;
    FailKey: Boolean;
    FailHash: Boolean;
    FailMerge: Boolean;
    MissingEquality: Boolean;
    Gate: Boolean;
    {$ifndef PAS2JS}
    FirstEntered: TEvent;
    SecondReady: TEvent;
    ReleaseFirst: TEvent;
    {$endif}
    constructor Create;
    destructor Destroy; override;
  end;
  TKey = class(TInterfacedObject, INyxSelector<TRow, TNyxText>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
    FPartition: Integer;
  public
    constructor Create(AProbe: TProbe; APartition: Integer);
    function Project(const AValue: TRow): TNyxText;
  end;
  TKeys = class(TInterfacedObject, INyxPartitionSelectorFactory<TRow, TNyxText>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<TRow, TNyxText>;
  end;
  TElement = class(TInterfacedObject, INyxSelector<TRow, Integer>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    function Project(const AValue: TRow): Integer;
  end;
  TElements = class(TInterfacedObject, INyxPartitionSelectorFactory<TRow, Integer>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<TRow, Integer>;
  end;
  TEquality = class(TInterfacedObject, INyxEquality<TNyxText>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
    FInner: INyxEquality<TNyxText>;
  public
    constructor Create(AProbe: TProbe);
    function Equal(const ALeft, ARight: TNyxText): Boolean;
    function Hash(const AValue: TNyxText): Cardinal;
  end;
  TEqualities = class(TInterfacedObject, INyxParallelEqualityFactory<TNyxText>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    function NewEquality(APartition, ASourceOffset: Integer): INyxEquality<TNyxText>;
  end;
  TSumRows = class(TInterfacedObject, INyxNumericKernel<TRow, Integer, Integer>)
    function Empty: Integer;
    procedure Add(var AState: Integer; const AValue: TRow);
    procedure Merge(var AState: Integer; const AOther: Integer);
    function Finish(const AState: Integer): Integer;
  end;
  TMeasuredKernel<T, TKeyValue, TState, TResult> = class(TInterfacedObject,
    INyxKeyedKernel<T, TKeyValue, TState, TResult>)
  private
    FInner: INyxKeyedKernel<T, TKeyValue, TState, TResult>;
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(const AInner: INyxKeyedKernel<T, TKeyValue, TState, TResult>; AProbe: TProbe);
    function Empty(const AKey: TKeyValue): TState;
    procedure Add(var AState: TState; const AValue: T; const AExecution: INyxExecution);
    procedure Merge(var AState: TState; const AOther: TState; const AExecution: INyxExecution);
    function Finish(const AKey: TKeyValue; const AState: TState; const AExecution: INyxExecution): TResult;
  end;
  TMeasuredKernels<T, TKeyValue, TState, TResult> = class(TInterfacedObject,
    INyxKeyedKernelFactory<T, TKeyValue, TState, TResult>)
  private
    FInner: INyxKeyedKernelFactory<T, TKeyValue, TState, TResult>;
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(const AInner: INyxKeyedKernelFactory<T, TKeyValue, TState, TResult>; AProbe: TProbe);
    function NewKernel(APartition, ASourceOffset: Integer): INyxKeyedKernel<T, TKeyValue, TState, TResult>;
  end;
  TJourney = class
  private
    FDocument: TNyxDocument;
    FAuthored: TNyxText;
    FApp: TApplicationHost;
    FScheduler: INyxScheduler;
    FStore: INyxCollection;
    FProbe: TProbe;
    FProbeOwner: IProbe;
    FRun: INyxParallelRun;
    FGroupLease: INyxQueryReceiver<TGroups>;
    FPairLease: INyxQueryReceiver<TPairs>;
    FLongLease: INyxQueryReceiver<TLongPairs>;
    FGroups: TGroups;
    FPairs: TPairs;
    FLongPairs: TLongPairs;
    FRetained: TGroup;
    FReceived: Integer;
    FStage: Integer;
    FChecks: Integer;
    FPolls: Integer;
    FSteered: Boolean;
    FFailure: Boolean;
    FGeneration: INyxCancellationScope;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Published;
    procedure ReceiveGroups(const AValues: TNyxSequenceArray<TGroups>);
    procedure ReceivePairs(const AValues: TNyxSequenceArray<TPairs>);
    procedure ReceiveLongs(const AValues: TNyxSequenceArray<TLongPairs>);
    procedure StartStage;
    procedure CompleteStage;
    procedure Finish(const AFailure: TNyxText);
  public
    Done: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Poll;
  end;

var
  GJourney: TJourney;
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  GEqualitySeeds: Integer;
  {$endif}

{$ifdef NYX_HAS_INLINE_CALLBACKS}
constructor TEqualitySeed.Create;
begin
  inherited Create;
  Inc(GEqualitySeeds);
end;

destructor TEqualitySeed.Destroy;
begin
  Dec(GEqualitySeeds);
  inherited Destroy;
end;

function TEqualitySeed.NewEquality: INyxEquality<TNyxText>;
begin
  Result := NewNyxTextEquality(ntoAsciiInsensitive);
end;

{ The returned factory escapes this frame. Explicit owning captures retain the
  interface in pas2js as well as native reference-counted function references. }
function EscapedInlineEquality: INyxParallelEqualityFactory<TNyxText>;
var
  LSeed: IEqualitySeed;
begin
  LSeed := TEqualitySeed.Create;
  Result := TNyxParallelEquality<TNyxText>.FromInline(
    function: INyxEquality<TNyxText>
    begin
      Result := LSeed.NewEquality;
    end, NyxCapture([LSeed]));
end;
{$endif}

function EqualityBoundaries: Integer;
var
  LFactory: INyxParallelEqualityFactory<TNyxText>;
  LEquality: INyxEquality<TNyxText>;
  LLease: INyxCallbackLease;
  LRefused: Boolean;
  LEntries: Integer;
  LChecks: Integer;

  function NestedEquality: INyxEquality<TNyxText>;
  begin
    Inc(LEntries);
    Result := NewNyxTextEquality(ntoAsciiInsensitive);
  end;

  procedure Verify(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxQuery.Create('Equality factory boundary: ' + AReason);
    end;
    Inc(LChecks);
  end;

begin
  LChecks := 0;
  LEntries := 0;
  LLease := NewNyxCallbackLease;
  try
    LFactory := TNyxParallelEquality<TNyxText>.FromNested(NestedEquality, LLease);
    LEquality := LFactory.NewEquality(0, 0);
    Verify((LEntries = 1) and LEquality.Equal('ALPHA', 'alpha'), 'live nested entry returns owned equality');
  finally
    LLease.Close;
  end;
  LRefused := False;
  try
    LEquality := LFactory.NewEquality(1, 3);
  except
    on LException: ENyxCallbackExpired do
    begin
      LRefused := True;
    end;
  end;
  Verify(LRefused and (LEntries = 1), 'closed entry refuses before borrowed frame access');
  LRefused := False;
  try
    LFactory := TNyxParallelEquality<TNyxText>.FromNested(NestedEquality, LLease);
  except
    on LException: ENyxCallbackExpired do
    begin
      LRefused := True;
    end;
  end;
  Verify(LRefused and (LEntries = 1), 'closed factory construction refuses');
  LFactory := nil;
  LRefused := False;
  try
    LFactory := TNyxParallelEquality<TNyxText>.From(nil);
  except
    on LException: ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Verify(LRefused, 'missing named factory refuses');
  LRefused := False;
  try
    LFactory := TNyxParallelEquality<TNyxText>.SharedImmutable(nil);
  except
    on LException: ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Verify(LRefused, 'missing shared equality refuses');
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  LFactory := EscapedInlineEquality;
  Verify(GEqualitySeeds = 1, 'escaped factory retains interface capture');
  LEquality := LFactory.NewEquality(-1, 0);
  Verify(LEquality.Equal('ALPHA', 'alpha'), 'escaped inline factory executes after frame exit');
  LEquality := nil;
  LFactory := nil;
  Verify(GEqualitySeeds = 0, 'final factory release disposes capture');
  {$endif}
  Result := LChecks;
end;

procedure Increment(var AValue: LongInt);
begin
  {$ifdef PAS2JS}Inc(AValue);{$else}InterlockedIncrement(AValue);{$endif}
end;

procedure WorkerCall(AProbe: TProbe);
begin
  {$ifndef PAS2JS}

  if GetCurrentThreadID = MainThreadID then
  begin
    Increment(AProbe.UICalls);
  end;
  {$endif}
end;

constructor TProbe.Create;
begin
  inherited Create;
  {$ifndef PAS2JS}
  FirstEntered := TEvent.Create(nil, True, False, '');
  SecondReady := TEvent.Create(nil, True, False, '');
  ReleaseFirst := TEvent.Create(nil, True, False, '');
  {$endif}
end;

destructor TProbe.Destroy;
begin
  {$ifndef PAS2JS}
  FirstEntered.Free;
  SecondReady.Free;
  ReleaseFirst.Free;
  {$endif}
  inherited Destroy;
end;

constructor TKey.Create(AProbe: TProbe; APartition: Integer);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
  FPartition := APartition;
end;

function TKey.Project(const AValue: TRow): TNyxText;
begin
  WorkerCall(FProbe);
  Increment(FProbe.Keys);

  if FProbe.FailKey and (AValue.ID = 2) then
  begin
    raise ENyxQuery.Create('Key projection refused its row');
  end;
  {$ifndef PAS2JS}

  if FProbe.Gate then
  begin

    if (FPartition = 0) and (AValue.ID = 0) then
    begin
      FProbe.FirstEntered.SetEvent;
      FProbe.ReleaseFirst.WaitFor(INFINITE);
    end;

    if (FPartition = 1) and (AValue.ID = 3) then
    begin
      FProbe.SecondReady.SetEvent;
    end;
  end;
  {$endif}
  Result := AValue.Key;
end;

constructor TKeys.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TKeys.NewSelector(APartition, ASourceOffset: Integer): INyxSelector<TRow, TNyxText>;
begin
  Result := TKey.Create(FProbe, APartition);
end;

constructor TElement.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TElement.Project(const AValue: TRow): Integer;
begin
  WorkerCall(FProbe);
  Increment(FProbe.Elements);
  Result := AValue.ID;
end;

constructor TElements.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TElements.NewSelector(APartition, ASourceOffset: Integer): INyxSelector<TRow, Integer>;
begin
  Result := TElement.Create(FProbe);
end;

constructor TEquality.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
  FInner := NewNyxTextEquality(ntoAsciiInsensitive);
end;

function TEquality.Equal(const ALeft, ARight: TNyxText): Boolean;
begin
  WorkerCall(FProbe);
  Increment(FProbe.Equalities);
  Result := FInner.Equal(ALeft, ARight);
end;

function TEquality.Hash(const AValue: TNyxText): Cardinal;
begin
  WorkerCall(FProbe);
  Increment(FProbe.Hashes);

  if FProbe.FailHash then
  begin
    raise ENyxQuery.Create('Hash callback refused its key');
  end;

  if FProbe.Collisions then
  begin
    Exit(0);
  end;
  Result := FInner.Hash(AValue);
end;

constructor TEqualities.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TEqualities.NewEquality(APartition, ASourceOffset: Integer): INyxEquality<TNyxText>;
begin

  if FProbe.MissingEquality then
  begin
    Exit(nil);
  end;
  Result := TEquality.Create(FProbe);
end;

function TSumRows.Empty: Integer;
begin
  Result := 0;
end;

procedure TSumRows.Add(var AState: Integer; const AValue: TRow);
begin
  AState := NyxAddQueryInteger(AState, AValue.Value);
end;

procedure TSumRows.Merge(var AState: Integer; const AOther: Integer);
begin
  AState := NyxAddQueryInteger(AState, AOther);
end;

function TSumRows.Finish(const AState: Integer): Integer;
begin
  Result := AState;
end;

constructor TMeasuredKernel<T, TKeyValue, TState, TResult>.Create(
  const AInner: INyxKeyedKernel<T, TKeyValue, TState, TResult>; AProbe: TProbe);
begin
  inherited Create;
  FInner := AInner;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TMeasuredKernel<T, TKeyValue, TState, TResult>.Empty(const AKey: TKeyValue): TState;
begin
  WorkerCall(FProbe);
  Increment(FProbe.Seeds);
  Result := FInner.Empty(AKey);
end;

procedure TMeasuredKernel<T, TKeyValue, TState, TResult>.Add(var AState: TState;
  const AValue: T; const AExecution: INyxExecution);
begin
  WorkerCall(FProbe);
  Increment(FProbe.Adds);
  FInner.Add(AState, AValue, AExecution);
end;

procedure TMeasuredKernel<T, TKeyValue, TState, TResult>.Merge(var AState: TState;
  const AOther: TState; const AExecution: INyxExecution);
begin
  WorkerCall(FProbe);
  Increment(FProbe.Merges);

  if FProbe.FailMerge then
  begin
    raise ENyxQuery.Create('Keyed merge refused its partial');
  end;
  FInner.Merge(AState, AOther, AExecution);
end;

function TMeasuredKernel<T, TKeyValue, TState, TResult>.Finish(const AKey: TKeyValue;
  const AState: TState; const AExecution: INyxExecution): TResult;
begin
  WorkerCall(FProbe);
  Increment(FProbe.Finishes);
  Result := FInner.Finish(AKey, AState, AExecution);
end;

constructor TMeasuredKernels<T, TKeyValue, TState, TResult>.Create(
  const AInner: INyxKeyedKernelFactory<T, TKeyValue, TState, TResult>; AProbe: TProbe);
begin
  inherited Create;
  FInner := AInner;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TMeasuredKernels<T, TKeyValue, TState, TResult>.NewKernel(APartition, ASourceOffset: Integer):
  INyxKeyedKernel<T, TKeyValue, TState, TResult>;
begin
  Result := TMeasuredKernel<T, TKeyValue, TState, TResult>.Create(
    FInner.NewKernel(APartition, ASourceOffset), FProbe);
end;

function Row(AID: Integer; const AKey: TNyxText; AValue: Integer): TRow;
begin
  Result.ID := AID;
  Result.Key := AKey;
  Result.Value := AValue;
end;

function MakeRow(const AValue: Integer): TRow;
begin
  Result := Row(AValue, TNyxText(IntToStr(AValue mod 16)), AValue + 1);
end;

function KeepRow(const AValue: TRow): Boolean;
begin
  Result := AValue.Value > 2;
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxQuery.Create('Stage ' + IntToStr(FStage) + ': ' + AReason);
  end;
  Inc(FChecks);
end;

constructor TJourney.Create;
begin
  inherited Create;
  FChecks := EqualityBoundaries;
  FDocument := BuildNyxDocument;
  FAuthored := TNyxCodec.Encode(FDocument);
  FApp := TApplicationHost.Create;
  {$ifdef PAS2JS}FApp.Run(FDocument, TJSHTMLElement(document.body));{$else}FApp.Mount(FDocument);{$endif}
  FScheduler := FApp.Context.Scheduler;
  FStore := FApp.Collections.Collection(NyxCollection('measured'));
  Check(FDocument.Count = 2, 'exact semantic companion supplies two pages');
  StartStage;
end;

destructor TJourney.Destroy;
begin
  {$ifndef PAS2JS}

  if FProbe <> nil then
  begin
    FProbe.ReleaseFirst.SetEvent;
  end;
  {$endif}

  if FRun <> nil then
  begin
    FRun.Cancel;
  end;
  FGroupLease := nil;
  FPairLease := nil;
  FLongLease := nil;
  FGroups := nil;
  FPairs := nil;
  FLongPairs := nil;
  FRetained := nil;
  FApp.Free;
  FStore := nil;
  FDocument.Free;
  FScheduler.Shutdown;
  FProbeOwner := nil;
  inherited Destroy;
end;

procedure TJourney.Published;
var
  LCaption: TNyxText;
begin
  FScheduler.RequireUI;
  Inc(FReceived);
  LCaption := 'Grouped result / ' + IntToStr(FStage);
  FStore.Update(FStore.Snapshot.ItemAt(0).WithValue(NyxTextField('caption'), LCaption));
  {$ifdef PAS2JS}
  Check(Pos(LCaption, FApp.View.ElementFor('measured-table').textContent) > 0,
    'actual browser table observes completed grouped publication');
  {$else}
  Check(TNyxText(TStringGrid(FApp.View.ControlFor('measured-table')).Cells[0, 1]) = LCaption,
    'actual native table observes completed grouped publication');
  {$endif}
end;

procedure TJourney.ReceiveGroups(const AValues: TNyxSequenceArray<TGroups>);
begin
  Check((Length(AValues) = 1) and (AValues[0] <> nil), 'one owned complete group sequence reaches UI');
  FGroups := AValues[0];
  Published;
end;

procedure TJourney.ReceivePairs(const AValues: TNyxSequenceArray<TPairs>);
begin
  Check((Length(AValues) = 1) and (AValues[0] <> nil), 'one owned complete key/value sequence reaches UI');
  FPairs := AValues[0];
  Published;
end;

procedure TJourney.ReceiveLongs(const AValues: TNyxSequenceArray<TLongPairs>);
begin
  Check((Length(AValues) = 1) and (AValues[0] <> nil), 'one owned exact keyed-count sequence reaches UI');
  FLongPairs := AValues[0];
  Published;
end;

procedure TJourney.StartStage;
var
  LSource: INyxSequence<TRow>;
  LKeys: INyxPartitionSelectorFactory<TRow, TNyxText>;
  LEqualities: INyxParallelEqualityFactory<TNyxText>;
  LGroup: TGroupPlan;
  LCount: TCountPlan;
  LLong: TLongPlan;
  LProgram: INyxParallelProgram<Integer, TRow>;
  LRowProgram: INyxParallelProgram<TRow, TRow>;
  LOptions: TNyxParallelOptions;
begin
  FRun := nil;
  FGroupLease := nil;
  FPairLease := nil;
  FLongLease := nil;
  FGroups := nil;
  FPairs := nil;
  FLongPairs := nil;
  FProbeOwner := nil;
  FProbe := TProbe.Create;
  FProbeOwner := FProbe;
  FReceived := 0;
  FPolls := 0;
  FSteered := False;
  FFailure := FStage in [9, 10, 11, 12, 13];
  FGeneration := FApp.Context.Generation;
  FGroupLease := TNyxQueryReceiver<TGroups>.Bind(FScheduler, ReceiveGroups, FGeneration);
  FPairLease := TNyxQueryReceiver<TPairs>.Bind(FScheduler, ReceivePairs, FGeneration);
  FLongLease := TNyxQueryReceiver<TLongPairs>.Bind(FScheduler, ReceiveLongs, FGeneration);
  LOptions := TNyxParallelOptions.Defaults.PartitionSize(2).Concurrency(2);

  if not (nscWorkerThreads in FScheduler.Capabilities) then
  begin
    LOptions := LOptions.Execution(npxDeferredUI);
  end;
  LSource := TNyxSequence<TRow>.From([
    Row(0, 'ALPHA', 1), Row(1, 'beta', 2), Row(2, 'alpha', 3),
    Row(3, 'BETA', 4), Row(4, 'gamma', 5), Row(5, 'ALPHA', 6)]);
  case FStage of
    1:
    begin
      LSource := TNyxSequence<TRow>.Empty;
    end;
    2:
    begin
      FProbe.Collisions := True;
    end;
    3, 4:
    begin
      {$ifndef PAS2JS}FProbe.Gate := True;{$endif}

      if FStage = 4 then
      begin
        LOptions := LOptions.Order(npoCompletion);
      end;
    end;
    8:
    begin
      LSource := TNyxSequence<TRow>.From([
        Row(0, TNyxText('Notes / 🌙') + #0 + 'x', 1),
        Row(1, TNyxText('Notes / 🌙') + #0 + 'x', 2),
        Row(2, 'Notes / 🌙', 3)]);
    end;
    9:
    begin
      FProbe.FailKey := True;
    end;
    10:
    begin
      FProbe.FailHash := True;
      LOptions := LOptions.Failures(npfCollect);
    end;
    11:
    begin
      FProbe.MissingEquality := True;
    end;
    12:
    begin
      LSource := TNyxSequence<TRow>.From([Row(0, 'one', High(Integer)), Row(1, 'ONE', 1)]);
    end;
    13:
    begin
      FProbe.FailMerge := True;
    end;
  end;
  LKeys := TKeys.Create(FProbe);
  LEqualities := TEqualities.Create(FProbe);

  if FStage in [5, 7, 12, 13, 16] then
  begin

    if FStage = 5 then
    begin
      LCount := TNyxParallelCountBy<TRow, TNyxText>.From(LKeys, LEqualities);
    end
    else
    begin
      LCount := TNyxParallelAggregateBy<TRow, TNyxText, Integer, Integer>.From(
        LKeys, LEqualities, TSumRows.Create);
    end;
    LCount := TNyxParallelKeyed<TRow, TNyxText, Integer, TPair>.From(LCount.Keys, LCount.Equalities,
      TMeasuredKernels<TRow, TNyxText, Integer, TPair>.Create(LCount.Kernels, FProbe)).WithOptions(LOptions);

    if FStage = 16 then
    begin
      LOptions := LOptions.PartitionSize(512).MaximumInput(8192).CaptureRowsPerTurn(1024);
      LCount := LCount.WithOptions(LOptions);
      LProgram := TNyxParallelMap<Integer, Integer, TRow>.From(TNyxParallelSource<Integer>.From(NyxRange(0, 4096)),
        TNyxPartitionSelector<Integer, TRow>.From(MakeRow));
      FRun := TNyxProgramKeyed<Integer, TRow, TNyxText, Integer, TPair>.Submit(
        LProgram, LCount, FScheduler, FPairLease.Port, FGeneration);
    end
    else
    begin
      FRun := LCount.Submit(LSource, FScheduler, FPairLease.Port, FGeneration);
    end;
    Exit;
  end;

  if FStage = 6 then
  begin
    LLong := TNyxParallelCountBy<TRow, TNyxText>.LongCounts(LKeys, LEqualities).WithOptions(LOptions);
    FRun := LLong.Submit(LSource, FScheduler, FLongLease.Port, FGeneration);
    Exit;
  end;
  LGroup := TNyxParallelGroupBy<TRow, TNyxText, Integer>.From(
    LKeys, TElements.Create(FProbe), LEqualities).WithOptions(LOptions);

  if FStage = 15 then
  begin
    LRowProgram := TNyxParallelSource<TRow>.From(LSource)
      .Where(TNyxPartitionPredicate<TRow>.From(KeepRow));
    FRun := TNyxProgramKeyed<TRow, TRow, TNyxText, INyxElementBuffer<Integer>, TGroup>.Submit(
      LRowProgram, LGroup, FScheduler, FGroupLease.Port, FGeneration);
  end
  else
  begin
    FRun := LGroup.Submit(LSource, FScheduler, FGroupLease.Port, FGeneration);
  end;

  if FStage = 14 then
  begin
    FApp.ShowPage('next-view');
  end;
end;

procedure TJourney.CompleteStage;
var
  LGroups: TNyxSequenceArray<TGroup>;
  LPairs: TNyxSequenceArray<TPair>;
  LLongs: TNyxSequenceArray<TLongPair>;
  LValues: TNyxSequenceArray<Integer>;
  LIndex: Integer;
  LTotal: Integer;
begin

  if FFailure then
  begin
    Check((FRun.Phase = nqpFailed) and (FReceived = 0), 'grouped failure publishes no partial sequence');

    if FStage = 10 then
    begin
      Check(FRun.FailureCount = 3, 'collect retains every failing hash partition');
    end;
  end
  else if FStage = 14 then
  begin
    Check((FRun.Phase = nqpCancelled) and (FReceived = 0), 'navigation suppresses grouped publication');
    FApp.ShowPage('measured-view');
  end
  else
  begin
    Check((FRun.Phase = nqpSucceeded) and (FReceived = 1), 'keyed plan publishes exactly one completed result');
    case FStage of
      0, 2, 3:
      begin
        LGroups := FGroups.ToArray;
        Check((Length(LGroups) = 3) and (LGroups[0].Key = 'ALPHA') and
          (LGroups[1].Key = 'beta') and (LGroups[2].Key = 'gamma'), 'source-order groups retain first equal key representation');
        LValues := LGroups[0].Values.ToArray;
        Check((Length(LValues) = 3) and (LValues[0] = 0) and (LValues[1] = 2) and (LValues[2] = 5),
          'source-order group values remain stable across partials');
        Check((FProbe.Keys = 6) and (FProbe.Elements = 6), 'keys and elements project exactly once per accepted row');
        FRetained := LGroups[0];
      end;
      1:
      begin
        Check((FGroups.Count = 0) and (FProbe.Keys = 0) and (FProbe.Elements = 0),
          'empty groups enter no partition key/element callback');
      end;
      4:
      begin
        LGroups := FGroups.ToArray;
        {$ifndef PAS2JS}
        Check((LGroups[0].Key = 'alpha') and (LGroups[0].Values.First = 2),
          'actual reverse completion defines explicit completion-order representative/value');
        {$else}
        Check(Length(LGroups) = 3, 'browser completion policy preserves grouping semantics');
        {$endif}
      end;
      5, 7:
      begin
        LPairs := FPairs.ToArray;
        Check((Length(LPairs) = 3) and (LPairs[0].Key = 'ALPHA'), 'compact results retain first-key source order');

        if FStage = 5 then
        begin
          Check((LPairs[0].Value = 3) and (LPairs[1].Value = 2) and (LPairs[2].Value = 1), 'per-key Integer counts are exact');
        end
        else
        begin
          Check((LPairs[0].Value = 10) and (LPairs[1].Value = 6) and (LPairs[2].Value = 5), 'per-key aggregates merge the independent states');
        end;
        Check((FProbe.Seeds = 9) and (FProbe.Adds = 6) and (FProbe.Merges = 6) and (FProbe.Finishes = 3),
          'compact engine stores distinct partition keys and seeds final keys independently');
      end;
      6:
      begin
        LLongs := FLongPairs.ToArray;
        Check((Length(LLongs) = 3) and (LLongs[0].Value.ToText = '3') and
          (LLongs[1].Value.ToText = '2') and (LLongs[2].Value.ToText = '1'), 'exact wide per-key counts retain their typed digits');
      end;
      8:
      begin
        LGroups := FGroups.ToArray;
        Check((Length(LGroups) = 2) and (LGroups[0].Count = 2) and
          (LGroups[0].Key = TNyxText('Notes / 🌙') + #0 + 'x'), 'supplementary text and embedded NUL remain distinct exact keys');
      end;
      15:
      begin
        LGroups := FGroups.ToArray;
        Check((Length(LGroups) = 3) and (LGroups[0].Key = 'alpha') and
          (LGroups[0].Values.First = 2) and (LGroups[0].Count = 2), 'filtered program groups accepted outputs in source order');
      end;
      16:
      begin
        LPairs := FPairs.ToArray;
        Check(Length(LPairs) = 16, 'larger keyed aggregate retains only sixteen final keys');
        LTotal := 0;
        for LIndex := 0 to High(LPairs) do
        begin
          LTotal := NyxAddQueryInteger(LTotal, LPairs[LIndex].Value);
        end;
        Check(LTotal = 8390656, 'larger keyed aggregate preserves the exact total');
        Check((FProbe.Keys = 4096) and (FProbe.Seeds = 144) and
          (FProbe.Merges = 128) and (FProbe.Finishes = 16), 'larger summary admits 128 compact partial keys instead of row result vectors');
      end;
    end;
    {$ifndef PAS2JS}
    Check(FProbe.UICalls = 0, 'all key/hash/equality/kernel operations execute on actual native workers');
    {$endif}
  end;
  Check((FRetained <> nil) and (FRetained.Key = 'ALPHA') and (FRetained.Values.Last = 5),
    'retained immutable group survives later submissions/plans/sources');
  Check(TNyxCodec.Encode(FDocument) = FAuthored, 'grouped runtime leaves exact authored defaults unchanged');
end;

{$ifdef PAS2JS}
procedure ReleaseJourney;
begin
  FreeAndNil(GJourney);
  document.body.setAttribute('data-nyx-keyed-disposed', 'true');
end;
{$endif}

procedure TJourney.Finish(const AFailure: TNyxText);
begin
  Done := True;
  {$ifdef PAS2JS}

  if AFailure = '' then
  begin
    document.body.setAttribute('data-nyx-keyed', 'passed');
    document.body.setAttribute('data-nyx-keyed-checks', IntToStr(FChecks));
  end
  else
  begin
    document.body.textContent := 'FAIL ' + AFailure;
    document.body.setAttribute('data-nyx-keyed', 'failed');
  end;
  window.setTimeout(@ReleaseJourney, 0);
  {$else}

  if AFailure = '' then
  begin
    WriteLn('PASS ', FChecks, ' parallel grouping / actual UI checks');
  end
  else
  begin
    WriteLn('FAIL ', AFailure);
    ExitCode := 1;
  end;
  {$endif}
end;

procedure TJourney.Poll;
var
  LMonitor: INyxSchedulerMonitor;
begin
  try
    Inc(FPolls);

    if FPolls > 2000 then
    begin
      raise ENyxQuery.Create('Grouped observation expired at stage ' + IntToStr(FStage));
    end;
    Supports(FScheduler, INyxSchedulerMonitor, LMonitor);
    {$ifndef PAS2JS}

    if (FStage in [3, 4]) and not FSteered and
      (FProbe.FirstEntered.WaitFor(0) = wrSignaled) and
      (FProbe.SecondReady.WaitFor(0) = wrSignaled) and (LMonitor.WorkerLoad.Running = 1) then
    begin
      { The second worker has retired AFTER posting its reply. Merely reaching
        its last selector can race the first worker; this establishes reply
        order before the blocked first partition may complete. }
      FSteered := True;
      FProbe.ReleaseFirst.SetEvent;
    end;
    {$endif}

    if FRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;

    if LMonitor.WorkerLoad.Running <> 0 then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;

    if FStage <= 16 then
    begin
      CompleteStage;
      Inc(FStage);

      if FStage <= 16 then
      begin
        StartStage;
        {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
        Exit;
      end;
      FScheduler.Shutdown;
    end;

    if LMonitor.WorkerLoad.ActiveWorkers <> 0 then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;
    Finish('');
  except
    on LException: Exception do
    begin
      Finish(NyxQueryDiagnostic(LException));
    end;
  end;
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    GJourney := TJourney.Create;
    {$ifdef PAS2JS}
    GJourney.Poll;
    {$else}
    while not GJourney.Done do
    begin
      CheckSynchronize(1);
      Application.ProcessMessages;
      GJourney.Poll;
      Sleep(1);
    end;
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-keyed', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}GJourney.Free;{$endif}
end.
