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

program nyx_linq_terminal_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

uses
  SysUtils, Classes, nyx.text, nyx.callback.scope, nyx.model, nyx.codec, nyx.generated.view,
  nyx.collections, nyx.linq, nyx.linq.numeric, nyx.linq.parallel, nyx.linq.pipeline,
  nyx.linq.reductions, nyx.linq.terminals, nyx.linq.execution, nyx.scheduler
  {$ifdef PAS2JS}, Web, nyx.application.browser
  {$else}, Interfaces, Forms, Grids, SyncObjs, nyx.application.lcl{$endif};

type
  {$ifdef PAS2JS}
  TApplicationHost = TNyxBrowserApplication;
  {$else}
  TApplicationHost = TNyxLCLApplication;
  {$endif}
  TTask = record
    ID: Integer;
    Score: Integer;
    Caption: TNyxText;
  end;
  TKey = record
    Score: Integer;
    ID: Integer;
  end;
  TOptionalTask = INyxOptional<TTask>;
  TPredicateMode = (pmMatch, pmPositive, pmFault, pmMissing, pmOverlap);
  IProbe = interface(IInterface)
    ['{D1B310CA-4B4A-485F-903C-8D7AF8974893}']
  end;
  { Test-only synchronized counters/gates. No UI object is shared with workers.
    Explicit ownership retains these handles until all entered work has left. }
  TProbe = class(TInterfacedObject, IProbe)
  public
    Calls: LongInt;
    Factories: LongInt;
    Keys: LongInt;
    Maps: LongInt;
    Compares: LongInt;
    UICompares: LongInt;
    Made: LongInt;
    Freed: LongInt;
    MergeLeft: Integer;
    MergeRight: Integer;
    Match: Integer;
    Mode: TPredicateMode;
    GateKeys: Boolean;
    ThrowComparer: Boolean;
    {$ifndef PAS2JS}
    FirstEntered: TEvent;
    SecondEntered: TEvent;
    ReleaseFirst: TEvent;
    ReleaseSecond: TEvent;
    {$endif}
    constructor Create;
    destructor Destroy; override;
  end;
  TPredicate = class(TInterfacedObject, INyxPredicate<Integer>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    destructor Destroy; override;
    function Test(const AValue: Integer): Boolean;
  end;
  TPredicates = class(TInterfacedObject, INyxPartitionPredicateFactory<Integer>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    function NewPredicate(APartition, ASourceOffset: Integer): INyxPredicate<Integer>;
  end;
  TKeys = class(TInterfacedObject, INyxSelector<TTask, TKey>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
    FPartition: Integer;
  public
    constructor Create(AProbe: TProbe; APartition: Integer);
    function Project(const AValue: TTask): TKey;
  end;
  TKeyFactory = class(TInterfacedObject, INyxPartitionSelectorFactory<TTask, TKey>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<TTask, TKey>;
  end;
  TComparer = class(TInterfacedObject, INyxComparer<TKey>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
    FMerge: Boolean;
  public
    constructor Create(AProbe: TProbe; AMerge: Boolean);
    function Compare(const ALeft, ARight: TKey): Integer;
  end;
  TComparers = class(TInterfacedObject, INyxParallelComparerFactory<TKey>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    function NewComparer(APartition, ASourceOffset: Integer): INyxComparer<TKey>;
  end;
  TMap = class(TInterfacedObject, INyxSelector<Integer, Integer>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    function Project(const AValue: Integer): Integer;
  end;
  TMaps = class(TInterfacedObject, INyxPartitionSelectorFactory<Integer, Integer>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<Integer, Integer>;
  end;
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  IComparerSeed = interface(IInterface)
    ['{A8BEF6B2-0A43-41B0-AD4E-5BA81B0458FB}']
    function NewComparer: INyxComparer<Integer>;
  end;
  TComparerSeed = class(TInterfacedObject, IComparerSeed)
    constructor Create;
    destructor Destroy; override;
    function NewComparer: INyxComparer<Integer>;
  end;
  {$endif}
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
    FRun2: INyxParallelRun;
    FBooleanLease: INyxQueryReceiver<Boolean>;
    FSecondLease: INyxQueryReceiver<Boolean>;
    FTaskLease: INyxQueryReceiver<TOptionalTask>;
    FBoolean: Boolean;
    FSecond: Boolean;
    FTask: TOptionalTask;
    FReceived: Integer;
    FStage: Integer;
    FChecks: Integer;
    FPolls: Integer;
    FSteered: Boolean;
    FExpectedFailure: Boolean;
    FGeneration: INyxCancellationScope;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure ReceiveBoolean(const AValues: TNyxSequenceArray<Boolean>);
    procedure ReceiveSecond(const AValues: TNyxSequenceArray<Boolean>);
    procedure ReceiveTask(const AValues: TNyxSequenceArray<TOptionalTask>);
    procedure Caption(const AValue: TNyxText);
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
  GSeeds: Integer;
  {$endif}

procedure Increment(var AValue: LongInt);
begin
  {$ifdef PAS2JS}Inc(AValue);{$else}InterlockedIncrement(AValue);{$endif}
end;

constructor TProbe.Create;
begin
  inherited Create;
  Match := 1;
  {$ifndef PAS2JS}
  FirstEntered := TEvent.Create(nil, True, False, '');
  SecondEntered := TEvent.Create(nil, True, False, '');
  ReleaseFirst := TEvent.Create(nil, True, False, '');
  ReleaseSecond := TEvent.Create(nil, True, False, '');
  {$endif}
end;

destructor TProbe.Destroy;
begin
  {$ifndef PAS2JS}
  FirstEntered.Free;
  SecondEntered.Free;
  ReleaseFirst.Free;
  ReleaseSecond.Free;
  {$endif}
  inherited Destroy;
end;

constructor TPredicate.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
  Increment(FProbe.Made);
end;

destructor TPredicate.Destroy;
begin
  Increment(FProbe.Freed);
  inherited Destroy;
end;

function TPredicate.Test(const AValue: Integer): Boolean;
begin
  Increment(FProbe.Calls);

  if FProbe.Mode = pmFault then
  begin
    raise ENyxQuery.Create('Predicate refused its row');
  end;
  {$ifndef PAS2JS}

  if FProbe.Mode = pmOverlap then
  begin

    if AValue = 0 then
    begin
      FProbe.FirstEntered.SetEvent;
      FProbe.ReleaseFirst.WaitFor(INFINITE);
      raise ENyxQuery.Create('Entered predicate failed');
    end;
    FProbe.SecondEntered.SetEvent;
    FProbe.ReleaseSecond.WaitFor(INFINITE);
    Exit(True);
  end;
  {$endif}

  if FProbe.Mode = pmPositive then
  begin
    Result := AValue > 0;
  end
  else
  begin
    Result := AValue = FProbe.Match;
  end;
end;

constructor TPredicates.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TPredicates.NewPredicate(APartition, ASourceOffset: Integer): INyxPredicate<Integer>;
begin
  Increment(FProbe.Factories);

  if FProbe.Mode = pmMissing then
  begin
    Exit(nil);
  end;
  Result := TPredicate.Create(FProbe);
end;

constructor TKeys.Create(AProbe: TProbe; APartition: Integer);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
  FPartition := APartition;
end;

function TKeys.Project(const AValue: TTask): TKey;
begin
  Increment(FProbe.Keys);
  {$ifndef PAS2JS}

  if FProbe.GateKeys then
  begin

    if FPartition = 0 then
    begin
      FProbe.FirstEntered.SetEvent;
      FProbe.ReleaseFirst.WaitFor(INFINITE);
    end
    else
    begin
      FProbe.SecondEntered.SetEvent;
    end;
  end;
  {$endif}
  Result.Score := AValue.Score;
  Result.ID := AValue.ID;
end;

constructor TKeyFactory.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TKeyFactory.NewSelector(APartition, ASourceOffset: Integer): INyxSelector<TTask, TKey>;
begin
  Result := TKeys.Create(FProbe, APartition);
end;

constructor TComparer.Create(AProbe: TProbe; AMerge: Boolean);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
  FMerge := AMerge;
end;

function TComparer.Compare(const ALeft, ARight: TKey): Integer;
begin
  Increment(FProbe.Compares);
  {$ifndef PAS2JS}

  if GetCurrentThreadID = MainThreadID then
  begin
    Increment(FProbe.UICompares);
  end;
  {$endif}

  if FProbe.ThrowComparer then
  begin
    raise ENyxQuery.Create('Comparer refused its keys');
  end;

  if FMerge then
  begin
    FProbe.MergeLeft := ALeft.ID;
    FProbe.MergeRight := ARight.ID;
  end;

  if ALeft.Score = ARight.Score then
  begin
    Exit(0);
  end;

  if ALeft.Score < ARight.Score then
  begin
    Result := Low(Integer);
  end
  else
  begin
    Result := High(Integer);
  end;
end;

constructor TComparers.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TComparers.NewComparer(APartition, ASourceOffset: Integer): INyxComparer<TKey>;
begin
  Result := TComparer.Create(FProbe, APartition = -1);
end;

constructor TMap.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TMap.Project(const AValue: Integer): Integer;
begin
  Increment(FProbe.Maps);
  Result := AValue * 2;
end;

constructor TMaps.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TMaps.NewSelector(APartition, ASourceOffset: Integer): INyxSelector<Integer, Integer>;
begin
  Result := TMap.Create(FProbe);
end;

function Task(AID, AScore: Integer; const ACaption: TNyxText): TTask;
begin
  Result.ID := AID;
  Result.Score := AScore;
  Result.Caption := ACaption;
end;

function IdentityTask(const AValue: TTask): TTask;
begin
  Result := AValue;
end;

function CompareTasks(const ALeft, ARight: TTask): Integer;
begin
  Result := 0;

  if ALeft.Score < ARight.Score then
  begin
    Result := -1;
  end
  else if ALeft.Score > ARight.Score then
  begin
    Result := 1;
  end;
end;

function TaskComparer: INyxComparer<TTask>;
begin
  Result := TNyxComparer<TTask>.From(CompareTasks);
end;

{$ifdef NYX_HAS_INLINE_CALLBACKS}
constructor TComparerSeed.Create;
begin
  inherited Create;
  Inc(GSeeds);
end;

destructor TComparerSeed.Destroy;
begin
  Dec(GSeeds);
  inherited Destroy;
end;

function TComparerSeed.NewComparer: INyxComparer<Integer>;
begin
  Result := NewNyxIntegerComparer;
end;

function EscapedInlineFactory: INyxParallelComparerFactory<Integer>;
var
  LSeed: IComparerSeed;
begin
  LSeed := TComparerSeed.Create;
  Result := TNyxParallelComparer<Integer>.FromInline(
    function: INyxComparer<Integer>
    begin
      Result := LSeed.NewComparer;
    end, NyxCapture([LSeed]));
end;
{$endif}

function FactoryBoundaries: Integer;
var
  LFactory: INyxParallelComparerFactory<Integer>;
  LComparer: INyxComparer<Integer>;
  LLease: INyxCallbackLease;
  LRefused: Boolean;
  LEntries: Integer;
  LChecks: Integer;

  function NestedFactory: INyxComparer<Integer>;
  begin
    Inc(LEntries);
    Result := NewNyxIntegerComparer;
  end;

  procedure Verify(ACondition: Boolean; const AReason: String);
  begin

    if not ACondition then
    begin
      raise ENyxQuery.Create('Comparer factory boundary: ' + AReason);
    end;
    Inc(LChecks);
  end;

begin
  LChecks := 0;
  LEntries := 0;
  LLease := NewNyxCallbackLease;
  try
    LFactory := TNyxParallelComparer<Integer>.FromNested(NestedFactory, LLease);
    LComparer := LFactory.NewComparer(0, 0);
    Verify((LEntries = 1) and (LComparer.Compare(1, 2) < 0), 'live nested factory returns an owned comparer');
  finally
    LLease.Close;
  end;
  LRefused := False;
  try
    LComparer := LFactory.NewComparer(1, 3);
  except
    on LException: ENyxCallbackExpired do
    begin
      LRefused := True;
    end;
  end;
  Verify(LRefused and (LEntries = 1), 'closed nested entry refuses before static-link access');
  LRefused := False;
  try
    LFactory := TNyxParallelComparer<Integer>.FromNested(NestedFactory, LLease);
  except
    on LException: ENyxCallbackExpired do
    begin
      LRefused := True;
    end;
  end;
  Verify(LRefused and (LEntries = 1), 'constructing through a closed frame refuses');
  LFactory := nil;
  LRefused := False;
  try
    LFactory := TNyxParallelComparer<Integer>.From(nil);
  except
    on LException: ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Verify(LRefused, 'missing named factory refuses');
  LRefused := False;
  try
    LFactory := TNyxParallelComparer<Integer>.SharedImmutable(nil);
  except
    on LException: ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Verify(LRefused, 'missing immutable comparer refuses');
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  LFactory := EscapedInlineFactory;
  Verify(GSeeds = 1, 'owning inline capture survives its departed routine');
  LComparer := LFactory.NewComparer(-1, 0);
  Verify(LComparer.Compare(Low(Integer), High(Integer)) < 0, 'escaped factory executes with exact owned capture');
  LFactory := nil;
  Verify(GSeeds = 0, 'last inline factory release retires its interface capture');
  Verify(LComparer.Compare(4, 4) = 0, 'returned comparer owns its independent lifetime');
  {$endif}
  LComparer := nil;
  LLease := nil;
  Result := LChecks;
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
  FChecks := FactoryBoundaries;
  FDocument := BuildNyxDocument;
  FAuthored := TNyxCodec.Encode(FDocument);
  FApp := TApplicationHost.Create;
  {$ifdef PAS2JS}FApp.Run(FDocument, TJSHTMLElement(document.body));{$else}FApp.Mount(FDocument);{$endif}
  FScheduler := FApp.Context.Scheduler;
  FStore := FApp.Collections.Collection(NyxCollection('measured'));
  Check(FDocument.Count = 2, 'exact semantic companion has both navigation roots');
  StartStage;
end;

destructor TJourney.Destroy;
begin
  {$ifndef PAS2JS}

  if FProbe <> nil then
  begin
    FProbe.ReleaseFirst.SetEvent;
    FProbe.ReleaseSecond.SetEvent;
  end;
  {$endif}

  if FRun <> nil then
  begin
    FRun.Cancel;
  end;

  if FRun2 <> nil then
  begin
    FRun2.Cancel;
  end;
  FBooleanLease := nil;
  FSecondLease := nil;
  FTaskLease := nil;
  FTask := nil;
  FApp.Free;
  FStore := nil;
  FDocument.Free;
  FScheduler.Shutdown;
  FProbeOwner := nil;
  inherited Destroy;
end;

procedure TJourney.Caption(const AValue: TNyxText);
begin
  FScheduler.RequireUI;
  Inc(FReceived);
  FStore.Update(FStore.Snapshot.ItemAt(0).WithValue(NyxTextField('caption'), AValue));
  {$ifdef PAS2JS}
  Check(Pos(AValue, FApp.View.ElementFor('measured-table').textContent) > 0,
    'actual browser table observes one complete terminal publication');
  {$else}
  Check(TNyxText(TStringGrid(FApp.View.ControlFor('measured-table')).Cells[0, 1]) = AValue,
    'actual native table observes one complete terminal publication');
  {$endif}
end;

procedure TJourney.ReceiveBoolean(const AValues: TNyxSequenceArray<Boolean>);
begin
  Check(Length(AValues) = 1, 'one complete typed Boolean reaches UI');
  FBoolean := AValues[0];

  if FBoolean then
  begin
    Caption('A matching task is ready');
  end
  else
  begin
    Caption('No matching task is ready');
  end;
end;

procedure TJourney.ReceiveSecond(const AValues: TNyxSequenceArray<Boolean>);
begin
  Check(Length(AValues) = 1, 'independent run receives one typed Boolean');
  FSecond := AValues[0];
  Inc(FReceived);
end;

procedure TJourney.ReceiveTask(const AValues: TNyxSequenceArray<TOptionalTask>);
begin
  Check((Length(AValues) = 1) and (AValues[0] <> nil), 'one owned optional task reaches UI');
  FTask := AValues[0];

  if FTask.HasValue then
  begin
    Caption(FTask.Value.Caption);
  end
  else
  begin
    Caption('No tasks in this selection');
  end;
end;

procedure TJourney.StartStage;
var
  LRows: INyxSequence<Integer>;
  LTasks: INyxSequence<TTask>;
  LProgram: INyxParallelProgram<Integer, Integer>;
  LTaskProgram: INyxParallelProgram<TTask, TTask>;
  LPredicates: INyxPartitionPredicateFactory<Integer>;
  LOptions: TNyxParallelOptions;
  LEvaluation: TNyxQuantifierEvaluation;
  LComparers: INyxParallelComparerFactory<TTask>;
begin
  FRun := nil;
  FRun2 := nil;
  FTask := nil;
  FBooleanLease := nil;
  FSecondLease := nil;
  FTaskLease := nil;
  FProbeOwner := nil;
  FProbe := TProbe.Create;
  FProbeOwner := FProbe;
  FReceived := 0;
  FPolls := 0;
  FSteered := False;
  FExpectedFailure := FStage in [9, 10, 11, 18, 20];
  FGeneration := FApp.Context.Generation;
  FBooleanLease := TNyxQueryReceiver<Boolean>.Bind(FScheduler, ReceiveBoolean, FGeneration);
  FTaskLease := TNyxQueryReceiver<TOptionalTask>.Bind(FScheduler, ReceiveTask, FGeneration);
  LOptions := TNyxParallelOptions.Defaults.Concurrency(1).PartitionSize(3);

  if not (nscWorkerThreads in FScheduler.Capabilities) then
  begin
    LOptions := LOptions.Execution(npxDeferredUI);
  end;
  LRows := NyxRange(1, 12);
  LPredicates := TPredicates.Create(FProbe);
  LEvaluation := nqeDecisive;

  if FStage in [4, 5] then
  begin
    LEvaluation := nqeComplete;
  end;
  case FStage of
    0, 1:
    begin
      LRows := TNyxSequence<Integer>.Empty;
    end;
    3, 5, 7:
    begin
      FProbe.Mode := pmPositive;

      if FStage <> 7 then
      begin
        LRows := NyxRange(0, 12);
      end;
    end;
    6:
    begin
      FProbe.Match := 99;
    end;
    9, 10:
    begin
      FProbe.Mode := pmFault;

      if FStage = 10 then
      begin
        LOptions := LOptions.Failures(npfCollect);
      end;
    end;
    11:
    begin
      FProbe.Mode := pmMissing;
    end;
  end;

  if FStage <= 11 then
  begin

    if FStage in [1, 3, 5, 7] then
    begin
      FRun := TNyxParallelQuantifiers<Integer>.All(LRows, LPredicates, FScheduler,
        FBooleanLease.Port, LOptions, FGeneration, LEvaluation);
    end
    else
    begin
      FRun := TNyxParallelQuantifiers<Integer>.Any(LRows, LPredicates, FScheduler,
        FBooleanLease.Port, LOptions, FGeneration, LEvaluation);
    end;

    if FStage = 8 then
    begin
      FSecondLease := TNyxQueryReceiver<Boolean>.Bind(FScheduler, ReceiveSecond, FGeneration);
      FRun2 := TNyxParallelQuantifiers<Integer>.Any(NyxRange(20, 12), LPredicates,
        FScheduler, FSecondLease.Port, LOptions, FGeneration);
    end;
    Exit;
  end;

  if FStage in [12, 13, 22] then
  begin
    FProbe.Match := 2;
    LProgram := TNyxParallelSource<Integer>.From(LRows).Select(TMaps.Create(FProbe))
      .WithOptions(LOptions);
    if FStage = 22 then
    begin
      FProbe.Mode := pmPositive;
      FRun := TNyxProgramQuantifiers<Integer, Integer>.All(LProgram, LPredicates,
        FScheduler, FBooleanLease.Port, FGeneration);
    end
    else
    begin
      FRun := TNyxProgramQuantifiers<Integer, Integer>.Any(LProgram, LPredicates,
        FScheduler, FBooleanLease.Port, FGeneration);
    end;

    if FStage = 13 then
    begin
      FApp.ShowPage('next-view');
    end;
    Exit;
  end;

  if FStage in [24, 25] then
  begin

    if FStage = 24 then
    begin
      FRun := TNyxParallelQuantifiers<Integer>.Any(LRows, FScheduler,
        FBooleanLease.Port, LOptions, FGeneration);
    end
    else
    begin
      LProgram := TNyxParallelSource<Integer>.From(LRows).Select(TMaps.Create(FProbe))
        .WithOptions(LOptions);
      FRun := TNyxProgramQuantifiers<Integer, Integer>.Any(LProgram,
        FScheduler, FBooleanLease.Port, FGeneration);
    end;
    Exit;
  end;
  LTasks := TNyxSequence<TTask>.From([
    Task(0, 10, 'First source tie'), Task(1, 10, 'Second source tie'),
    Task(2, 10, 'Later partition tie'), Task(3, 10, 'Last source tie')]);
  LOptions := LOptions.PartitionSize(2).Order(npoCompletion);
  case FStage of
    14:
    begin
      LTasks := TNyxSequence<TTask>.Empty;
    end;
    15:
    begin
      LTasks := TNyxSequence<TTask>.From([Task(0, 0, 'Zero is present')]);
    end;
    16:
    begin
      LTasks := TNyxSequence<TTask>.From([
        Task(0, Low(Integer), 'Smallest task'), Task(1, High(Integer), 'Largest task')]);
    end;
    17:
    begin
      {$ifndef PAS2JS}
      FProbe.GateKeys := True;
      LOptions := LOptions.Concurrency(2);
      {$endif}
    end;
    18:
    begin
      FProbe.ThrowComparer := True;
    end;
    20:
    begin
      {$ifndef PAS2JS}
      FProbe.Mode := pmOverlap;
      LRows := NyxRange(0, 2);
      LOptions := LOptions.PartitionSize(1).Concurrency(2);
      FRun := TNyxParallelQuantifiers<Integer>.Any(LRows, LPredicates,
        FScheduler, FBooleanLease.Port, LOptions, FGeneration);
      {$else}
      FProbe.Mode := pmFault;
      FRun := TNyxParallelQuantifiers<Integer>.Any(LRows, LPredicates,
        FScheduler, FBooleanLease.Port, LOptions, FGeneration);
      {$endif}
      Exit;
    end;
    21:
    begin
      FRun := TNyxParallelQuantifiers<Integer>.Any(LRows, LPredicates,
        FScheduler, FBooleanLease.Port, LOptions, FGeneration);
      FRun.Cancel;
      Exit;
    end;
  end;

  if FStage in [26, 27] then
  begin

    if FStage = 26 then
    begin
      LTasks := TNyxSequence<TTask>.From([Task(0, -9, 'A smaller task'), Task(1, 7, 'A larger task')]);
      LComparers := TNyxParallelComparer<TTask>.From(TaskComparer);
      FRun := TNyxParallelValueExtrema<TTask>.Minimum(LTasks, LComparers,
        FScheduler, FTaskLease.Port, LOptions, FGeneration);
    end
    else
    begin
      LComparers := TNyxParallelComparer<TTask>.SharedImmutable(TaskComparer());
      FRun := TNyxParallelValueExtrema<TTask>.Maximum(LTasks, LComparers,
        FScheduler, FTaskLease.Port, LOptions, FGeneration);
    end;
    Exit;
  end;

  if FStage in [19, 23] then
  begin
    LTaskProgram := TNyxParallelSource<TTask>.From(LTasks)
      .Select(TNyxPartitionSelector<TTask, TTask>.From(IdentityTask)).WithOptions(LOptions);
    if FStage = 23 then
    begin
      FRun := TNyxProgramExtrema<TTask, TTask, TKey>.Minimum(LTaskProgram,
        TKeyFactory.Create(FProbe), TComparers.Create(FProbe), FScheduler, FTaskLease.Port, FGeneration);
    end
    else
    begin
      FRun := TNyxProgramExtrema<TTask, TTask, TKey>.Maximum(LTaskProgram,
        TKeyFactory.Create(FProbe), TComparers.Create(FProbe), FScheduler, FTaskLease.Port, FGeneration);
    end;
  end
  else if FStage = 16 then
  begin
    FRun := TNyxParallelExtrema<TTask, TKey>.Maximum(LTasks, TKeyFactory.Create(FProbe),
      TComparers.Create(FProbe), FScheduler, FTaskLease.Port, LOptions, FGeneration);
  end
  else
  begin
    FRun := TNyxParallelExtrema<TTask, TKey>.Minimum(LTasks, TKeyFactory.Create(FProbe),
      TComparers.Create(FProbe), FScheduler, FTaskLease.Port, LOptions, FGeneration);
  end;
end;

procedure TJourney.CompleteStage;
var
  LRefused: Boolean;
  LTask: TTask;
begin

  if FExpectedFailure then
  begin
    Check((FRun.Phase = nqpFailed) and (FReceived = 0), 'failed terminal publishes no result');

    if FStage = 10 then
    begin
      Check(FRun.FailureCount = 4, 'collect retains every failing partition');
    end;
  end
  else if FStage in [13, 21] then
  begin
    Check((FRun.Phase = nqpCancelled) and (FReceived = 0),
      'navigation/explicit cancellation retires terminal publication');

    if FStage = 13 then
    begin
      FApp.ShowPage('measured-view');
    end;
  end
  else
  begin
    Check(FRun.Phase = nqpSucceeded, 'terminal reaches success only after complete UI publication');
    Check(FReceived = 1 + Ord(FStage = 8), 'each independent terminal publishes exactly once');
    case FStage of
      0, 6:
      begin
        Check(not FBoolean, 'absent Any is False');
      end;
      1, 2, 4, 7, 8, 12, 22, 24, 25:
      begin
        Check(FBoolean, 'matching Any/vacuous All is True');
      end;
      3, 5:
      begin
        Check(not FBoolean, 'counterexample All is False');
      end;
      14:
      begin
        Check(not FTask.HasValue, 'empty extrema preserves typed absence');
        LRefused := False;
        try
          LTask := FTask.Value;
        except
          on LException: ENyxQueryEmpty do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused, 'required Value refuses a genuinely absent terminal');
      end;
      15:
      begin
        Check(FTask.HasValue and (FTask.Value.Score = 0), 'present zero remains distinct from absence');
      end;
      16:
      begin
        Check(FTask.Value.Score = High(Integer), 'extreme comparer signs never overflow');
      end;
      17, 19, 23:
      begin
        Check(FTask.Value.ID = 0, 'first source tie wins independently of completion order');
        Check(FProbe.Keys = 4, 'extrema projects each accepted key exactly once');
      end;
      26:
      begin
        Check(FTask.Value.Score = -9, 'whole-value minimum uses a fresh owned comparer');
      end;
      27:
      begin
        Check(FTask.Value.ID = 0, 'whole-value maximum accepts explicit immutable sharing and retains first tie');
      end;
    end;

    if FStage in [0, 1] then
    begin
      Check((FProbe.Calls = 0) and (FProbe.Factories = 0), 'empty quantifier enters no predicate factory');
    end;

    if FStage in [2, 3] then
    begin
      Check((FProbe.Calls = 1) and (FProbe.Factories = 1), 'decisive result stops rows and later predicate factories');
    end;

    if FStage in [4, 5, 6, 7] then
    begin
      Check((FProbe.Calls = 12) and (FProbe.Factories = 4), 'complete/nondecisive evaluation visits every row');
    end;

    if FStage = 8 then
    begin
      Check((FRun2.Phase = nqpSucceeded) and not FSecond, 'shared factory retains independent per-run decisions');
    end;

    if FStage in [12, 25] then
    begin
      Check(FProbe.Maps = 1, 'global program decision stops later filter/map row entry');
    end;
    {$ifndef PAS2JS}

    if FStage = 17 then
    begin
      Check((FProbe.MergeLeft = 0) and (FProbe.MergeRight = 2),
        'native gate establishes reverse completion before source-tie merging');
      Check(FProbe.UICompares = 0, 'partition and final comparers execute on actual workers');
    end;
    {$endif}
  end;
  Check(FProbe.Made = FProbe.Freed, 'all terminal predicate instances retire before completion');
  Check(TNyxCodec.Encode(FDocument) = FAuthored, 'runtime terminal leaves exact authored defaults unchanged');
end;

{$ifdef PAS2JS}
procedure ReleaseJourney;
begin
  FreeAndNil(GJourney);
  document.body.setAttribute('data-nyx-terminals-disposed', 'true');
end;
{$endif}

procedure TJourney.Finish(const AFailure: TNyxText);
begin
  Done := True;
  {$ifdef PAS2JS}

  if AFailure = '' then
  begin
    document.body.setAttribute('data-nyx-terminals', 'passed');
    document.body.setAttribute('data-nyx-terminal-checks', IntToStr(FChecks));
  end
  else
  begin
    document.body.textContent := 'FAIL ' + AFailure;
    document.body.setAttribute('data-nyx-terminals', 'failed');
  end;
  window.setTimeout(@ReleaseJourney, 0);
  {$else}

  if AFailure = '' then
  begin
    WriteLn('PASS ', FChecks, ' owned parallel terminal / actual UI checks');
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
      raise ENyxQuery.Create('Terminal observation expired at stage ' + IntToStr(FStage));
    end;
    {$ifndef PAS2JS}

    if (FStage = 17) and not FSteered and
      (FProbe.FirstEntered.WaitFor(0) = wrSignaled) and
      (FProbe.SecondEntered.WaitFor(0) = wrSignaled) then
    begin
      FSteered := True;
      FProbe.ReleaseFirst.SetEvent;
    end;

    if (FStage = 20) and not FSteered and
      (FProbe.FirstEntered.WaitFor(0) = wrSignaled) and
      (FProbe.SecondEntered.WaitFor(0) = wrSignaled) then
    begin
      FSteered := True;
      Check(FProbe.Calls = 2, 'native predicates actually overlap before release');
      FProbe.ReleaseSecond.SetEvent;
      FProbe.ReleaseFirst.SetEvent;
    end;
    {$endif}

    if FRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;

    if (FRun2 <> nil) and (FRun2.Phase in [nqpPending, nqpComputing, nqpAwaitingUI]) then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;
    Supports(FScheduler, INyxSchedulerMonitor, LMonitor);

    if LMonitor.WorkerLoad.Running <> 0 then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;
    if FStage <= 27 then
    begin
      CompleteStage;
      Inc(FStage);

      if FStage <= 27 then
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
      document.body.setAttribute('data-nyx-terminals', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}GJourney.Free;{$endif}
end.
