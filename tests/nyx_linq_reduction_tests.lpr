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

program nyx_linq_reduction_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.model, nyx.codec, nyx.generated.view,
  nyx.collections, nyx.linq, nyx.linq.aggregates, nyx.linq.parallel,
  nyx.linq.pipeline, nyx.linq.reductions, nyx.linq.execution,
  nyx.events, nyx.scheduler
  {$ifdef PAS2JS}, Web, nyx.application.browser
  {$else}, Interfaces, Forms, Grids, SyncObjs, nyx.application.lcl{$endif};

type
  {$ifdef PAS2JS}TApplicationHost = TNyxBrowserApplication;
  {$else}TApplicationHost = TNyxLCLApplication;{$endif}
  TMode = (rmSource, rmCompletion, rmNormal, rmCollect, rmStop, rmMergeFailure,
    rmCancelMerge, rmNoAccumulator, rmNoMerger, rmDispose);
  TDisposeHook = procedure of object;
  TTextNumberFactory = TNyxPartitionSelector<INyxFoldValue<TNyxText, Integer>, TNyxText>;
  TTextCombineFactory = TNyxPartitionSelector<INyxFoldValue<TNyxText, TNyxText>, TNyxText>;
  IMetrics = interface
    ['{7F6B8C8A-5CDB-497E-B3C8-23B342C773A0}']
    function Sum: Integer;
    function Count: Integer;
    function Trace: TNyxText;
  end;
  TMeasurements = record
    Factories: Integer;
    Adds: Integer;
    Finishes: Integer;
    MergeFactories: Integer;
    Merges: Integer;
    PartialCreated: Integer;
    PartialDestroyed: Integer;
    FinalCreated: Integer;
    FinalDestroyed: Integer;
    WorkerAdds: Integer;
    WorkerMerges: Integer;
    PartialCount: Integer;
  end;

  { Measurements retain no UI; immutable policy is fixed before workers start.
    Native counters are synchronized. Only explicit DeferredUI disposal can borrow
    the journey's hook. Native disposal is initiated by nonblocking UI polling. }
  TProbe = class(TInterfacedObject)
  private
    FMode: TMode;
    FData: TMeasurements;
    FDispose: TDisposeHook;
    {$ifndef PAS2JS}
    FLock: TRTLCriticalSection;
    FRelease: TEvent;
    FMergeEntered: TEvent;
    {$endif}
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(AMode: TMode; ADispose: TDisposeHook);
    destructor Destroy; override;
    procedure Factory;
    procedure Added;
    procedure Finished;
    procedure MergeFactory;
    procedure Merging(ACount: Integer);
    procedure Created(AFinal: Boolean);
    procedure Destroyed(AFinal: Boolean);
    function Snapshot: TMeasurements;
    {$ifndef PAS2JS}
    procedure WaitFirst;
    procedure WaitMerge;
    procedure Release;
    function MergeEntered: Boolean;
    {$endif}
  end;

  { Every partial/final owns exact detached values and its measurement anchor.
    Nothing points back to the mutable accumulator, application or document. }
  TMetrics = class(TInterfacedObject, IMetrics)
  private
    FSum: Integer;
    FCount: Integer;
    FTrace: TNyxText;
    FProbe: TProbe;
    FAnchor: IInterface;
    FFinal: Boolean;
  public
    constructor Create(AProbe: TProbe; ASum, ACount: Integer; const ATrace: TNyxText;
      AFinal: Boolean);
    destructor Destroy; override;
    function Sum: Integer;
    function Count: Integer;
    function Trace: TNyxText;
  end;
  TAccumulator = class(TInterfacedObject, INyxPartitionAccumulator<Integer, IMetrics>)
  private
    FProbe: TProbe;
    FAnchor: IInterface;
    FPartition: Integer;
    FSum: Integer;
    FCount: Integer;
  public
    constructor Create(AProbe: TProbe; APartition: Integer);
    procedure Add(const AValue: Integer; ASourceIndex: Integer; const AExecution: INyxExecution);
    function Finish(const AExecution: INyxExecution): IMetrics;
  end;
  TMerger = class(TInterfacedObject, INyxParallelMerger<IMetrics, IMetrics>)
  private
    FProbe: TProbe;
    FAnchor: IInterface;
  public
    constructor Create(AProbe: TProbe);
    function Merge(const AValues: TNyxSequenceArray<IMetrics>;
      const AExecution: INyxExecution): IMetrics;
  end;
  TFactory = class(TInterfacedObject, INyxPartitionAccumulatorFactory<Integer, IMetrics>,
    INyxParallelMergerFactory<IMetrics, IMetrics>)
  private
    FProbe: TProbe;
    FAnchor: IInterface;
  public
    constructor Create(AProbe: TProbe);
    function NewAccumulator(APartition, ASourceOffset: Integer):
      INyxPartitionAccumulator<Integer, IMetrics>;
    function NewMerger: INyxParallelMerger<IMetrics, IMetrics>;
  end;

  {$ifndef PAS2JS}
  { The ordinary reply executes BEFORE release of partition zero. This establishes
    actual UI-observed reverse completion without sleeping or guessing timing. }
  TReply = class(TInterfacedObject, INyxWork)
  private
    FInner: INyxWork;
    FProbe: TProbe;
    FAnchor: IInterface;
  public
    constructor Create(const AInner: INyxWork; AProbe: TProbe);
    procedure Execute(const AExecution: INyxExecution);
  end;
  TObservedScheduler = class(TInterfacedObject, INyxScheduler,
    INyxSchedulerTurns, INyxSchedulerMonitor)
  private
    FBase: INyxScheduler;
    FTurns: INyxSchedulerTurns;
  public
    Probe: TProbe;
    Anchor: IInterface;
    constructor Create(const ABase: INyxScheduler);
    function GetCapabilities: TNyxSchedulerCapabilities;
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    function Submit(const AWork: INyxWork; APolicy: TNyxExecutionPolicy): INyxExecution;
    function PostUI(const AWork: INyxWork; const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
    function GetCanYieldUI: Boolean;
    function PostTurn(const AWork: INyxWork): INyxExecution;
    function GetWorkerLoad: TNyxWorkerPoolSnapshot;
  end;
  {$endif}

  TJourney = class
  private
    FApp: TApplicationHost;
    FDocument: TNyxDocument;
    FAuthored: TNyxText;
    FStore: INyxCollection;
    FScheduler: INyxScheduler;
    FGeneration: INyxCancellationScope;
    FRun: INyxParallelRun;
    FStatsLease: INyxQueryReceiver<IMetrics>;
    FIntLease: INyxQueryReceiver<Integer>;
    FCountLease: INyxQueryReceiver<TNyxQueryCount>;
    FTextLease: INyxQueryReceiver<TNyxText>;
    FProbe: TProbe;
    FAnchor: IInterface;
    FLast: IMetrics;
    FBefore: Integer;
    FStage: Integer;
    FChecks: Integer;
    FPolls: Integer;
    FReceived: Integer;
    FValue: Integer;
    FCount: TNyxQueryCount;
    FText: TNyxText;
    FDisposed: Boolean;
    {$ifndef PAS2JS}FObserved: TObservedScheduler;{$endif}
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure ReceiveStats(const AValues: TNyxSequenceArray<IMetrics>);
    procedure ReceiveInteger(const AValues: TNyxSequenceArray<Integer>);
    procedure ReceiveCount(const AValues: TNyxSequenceArray<TNyxQueryCount>);
    procedure ReceiveText(const AValues: TNyxSequenceArray<TNyxText>);
    procedure Caption(const AText: TNyxText);
    procedure Cell(const AText: TNyxText);
    procedure DisposeView;
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
  GSeeds: Integer;

{$ifdef PAS2JS}
procedure ReleaseJourney;
begin
  GJourney.Free;
  GJourney := nil;
  document.body.setAttribute('data-nyx-reduction-disposed', 'true');
end;
{$endif}

function Options: TNyxParallelOptions;
begin
  Result := TNyxParallelOptions.Defaults.PartitionSize(2).Concurrency(2);
  {$ifdef PAS2JS}Result := Result.Execution(npxDeferredUI);{$endif}
end;

procedure RequireUI;
begin
  {$ifndef PAS2JS}

  if GetCurrentThreadID <> MainThreadID then
  begin
    raise ENyxQuery.Create('Factory or receiver escaped UI');
  end;
  {$endif}
end;

function Even(const AValue: Integer): Boolean;
begin
  Result := (AValue mod 2) = 0;
end;

function DoubleValue(const AValue: Integer): Integer;
begin
  Result := AValue * 2;
end;

function Describe(const AValue: Integer): TNyxText;
begin
  Result := 'Item ' + IntToStr(AValue);
end;

function FailSource(const AValue: Integer): Integer;
begin

  if AValue = 3 then
  begin
    raise ENyxQuery.Create('Reduction source failed');
  end;
  Result := AValue;
end;

function FreshTextSeed: TNyxText;
begin
  RequireUI;
  Inc(GSeeds);
  Result := '';
end;

function AppendNumber(const AValue: INyxFoldValue<TNyxText, Integer>): TNyxText;
begin
  Result := AValue.Accumulator + IntToStr(AValue.Value);
end;

function AppendText(const AValue: INyxFoldValue<TNyxText, TNyxText>): TNyxText;
begin
  Result := AValue.Accumulator + AValue.Value;
end;

constructor TProbe.Create(AMode: TMode; ADispose: TDisposeHook);
begin
  inherited Create;
  FMode := AMode;
  FDispose := ADispose;
  {$ifndef PAS2JS}
  InitCriticalSection(FLock);
  FRelease := TEvent.Create(nil, True, False, '');
  FMergeEntered := TEvent.Create(nil, True, False, '');
  {$endif}
end;

destructor TProbe.Destroy;
begin
  {$ifndef PAS2JS}
  FMergeEntered.Free;
  FRelease.Free;
  DoneCriticalSection(FLock);
  {$endif}
  inherited Destroy;
end;

procedure TProbe.Lock;
begin
  {$ifndef PAS2JS}EnterCriticalSection(FLock);{$endif}
end;

procedure TProbe.Unlock;
begin
  {$ifndef PAS2JS}LeaveCriticalSection(FLock);{$endif}
end;

procedure TProbe.Factory;
begin
  RequireUI;
  Lock;
  try
    Inc(FData.Factories);
  finally
    Unlock;
  end;
end;

procedure TProbe.Added;
begin
  Lock;
  try
    Inc(FData.Adds);
    {$ifndef PAS2JS}

    if GetCurrentThreadID <> MainThreadID then
    begin
      Inc(FData.WorkerAdds);
    end;
    {$endif}
  finally
    Unlock;
  end;
end;

procedure TProbe.Finished;
begin
  Lock;
  try
    Inc(FData.Finishes);
  finally
    Unlock;
  end;
end;

procedure TProbe.MergeFactory;
begin
  RequireUI;
  Lock;
  try
    Inc(FData.MergeFactories);
  finally
    Unlock;
  end;
end;

procedure TProbe.Merging(ACount: Integer);
begin
  Lock;
  try
    Inc(FData.Merges);
    FData.PartialCount := ACount;
    {$ifndef PAS2JS}

    if GetCurrentThreadID <> MainThreadID then
    begin
      Inc(FData.WorkerMerges);
    end;
    {$endif}
  finally
    Unlock;
  end;
end;

procedure TProbe.Created(AFinal: Boolean);
begin
  Lock;
  try

    if AFinal then
    begin
      Inc(FData.FinalCreated);
    end
    else
    begin
      Inc(FData.PartialCreated);
    end;
  finally
    Unlock;
  end;
end;

procedure TProbe.Destroyed(AFinal: Boolean);
begin
  Lock;
  try

    if AFinal then
    begin
      Inc(FData.FinalDestroyed);
    end
    else
    begin
      Inc(FData.PartialDestroyed);
    end;
  finally
    Unlock;
  end;
end;

function TProbe.Snapshot: TMeasurements;
begin
  Lock;
  try
    Result := FData;
  finally
    Unlock;
  end;
end;

{$ifndef PAS2JS}
procedure TProbe.WaitFirst;
begin

  if FRelease.WaitFor(10000) <> wrSignaled then
  begin
    raise ENyxQuery.Create('UI did not observe the preceding partition reply');
  end;
end;

procedure TProbe.WaitMerge;
begin
  FMergeEntered.SetEvent;
  WaitFirst;
end;

procedure TProbe.Release;
begin
  FRelease.SetEvent;
end;

function TProbe.MergeEntered: Boolean;
begin
  Result := FMergeEntered.WaitFor(0) = wrSignaled;
end;
{$endif}

constructor TMetrics.Create(AProbe: TProbe; ASum, ACount: Integer;
  const ATrace: TNyxText; AFinal: Boolean);
begin
  inherited Create;
  FProbe := AProbe;
  FAnchor := AProbe;
  FSum := ASum;
  FCount := ACount;
  FTrace := ATrace;
  FFinal := AFinal;
  FProbe.Created(FFinal);
end;

destructor TMetrics.Destroy;
begin
  FProbe.Destroyed(FFinal);
  inherited Destroy;
end;

function TMetrics.Sum: Integer;
begin
  Result := FSum;
end;

function TMetrics.Count: Integer;
begin
  Result := FCount;
end;

function TMetrics.Trace: TNyxText;
begin
  Result := FTrace;
end;

constructor TAccumulator.Create(AProbe: TProbe; APartition: Integer);
begin
  inherited Create;
  FProbe := AProbe;
  FAnchor := AProbe;
  FPartition := APartition;
end;

procedure TAccumulator.Add(const AValue: Integer; ASourceIndex: Integer;
  const AExecution: INyxExecution);
begin
  {$ifndef PAS2JS}

  if (FProbe.FMode in [rmSource, rmCompletion]) and (FPartition = 0) and (FCount = 0) then
  begin
    FProbe.WaitFirst;
  end;
  {$endif}

  if AValue <> ASourceIndex then
  begin
    raise ENyxQuery.Create('Reducer lost its exact input ordinal');
  end;
  FProbe.Added;
  Inc(FCount);
  Inc(FSum, AValue);

  if (FProbe.FMode in [rmCollect, rmStop]) and (ASourceIndex in [1, 3]) then
  begin
    raise ENyxQuery.Create('Reduction failed at ' + IntToStr(ASourceIndex));
  end;
end;

function TAccumulator.Finish(const AExecution: INyxExecution): IMetrics;
begin
  FProbe.Finished;
  Result := TMetrics.Create(FProbe, FSum, FCount,
    TNyxText(IntToStr(FPartition)) + TNyxText('/🌙'), False);
end;

constructor TMerger.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FAnchor := AProbe;
end;

function TMerger.Merge(const AValues: TNyxSequenceArray<IMetrics>;
  const AExecution: INyxExecution): IMetrics;
var
  LIndex: Integer;
  LSum: Integer;
  LCount: Integer;
  LTrace: TNyxText;
begin
  FProbe.Merging(Length(AValues));

  if FProbe.FMode = rmMergeFailure then
  begin
    raise ENyxQuery.Create('Final reduction merge failed');
  end;

  if FProbe.FMode = rmCancelMerge then
  begin
    AExecution.Cancel;
  end;

  if FProbe.FMode = rmDispose then
  begin
    {$ifdef PAS2JS}FProbe.FDispose;{$else}FProbe.WaitMerge;{$endif}
  end;
  LSum := 0;
  LCount := 0;
  LTrace := '';
  for LIndex := 0 to High(AValues) do
  begin
    Inc(LSum, AValues[LIndex].Sum);
    Inc(LCount, AValues[LIndex].Count);

    if LIndex <> 0 then
    begin
      LTrace := LTrace + ',';
    end;
    LTrace := LTrace + AValues[LIndex].Trace;
  end;
  Result := TMetrics.Create(FProbe, LSum, LCount, LTrace, True);
end;

constructor TFactory.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FAnchor := AProbe;
end;

function TFactory.NewAccumulator(APartition, ASourceOffset: Integer):
  INyxPartitionAccumulator<Integer, IMetrics>;
begin
  FProbe.Factory;

  if FProbe.FMode = rmNoAccumulator then
  begin
    Exit(nil);
  end;
  Result := TAccumulator.Create(FProbe, APartition);
end;

function TFactory.NewMerger: INyxParallelMerger<IMetrics, IMetrics>;
begin
  FProbe.MergeFactory;

  if FProbe.FMode = rmNoMerger then
  begin
    Exit(nil);
  end;
  Result := TMerger.Create(FProbe);
end;

{$ifndef PAS2JS}
constructor TReply.Create(const AInner: INyxWork; AProbe: TProbe);
begin
  inherited Create;
  FInner := AInner;
  FProbe := AProbe;
  FAnchor := AProbe;
end;

procedure TReply.Execute(const AExecution: INyxExecution);
begin
  FInner.Execute(AExecution);
  FProbe.Release;
end;

constructor TObservedScheduler.Create(const ABase: INyxScheduler);
begin
  inherited Create;
  FBase := ABase;
  FTurns := NyxSchedulerTurns(ABase);
end;

function TObservedScheduler.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := FBase.Capabilities;
end;

procedure TObservedScheduler.RequireUI;
begin
  FBase.RequireUI;
end;

procedure TObservedScheduler.Admit(APolicy: TNyxExecutionPolicy);
begin
  FBase.Admit(APolicy);
end;

function TObservedScheduler.Submit(const AWork: INyxWork; APolicy: TNyxExecutionPolicy): INyxExecution;
begin
  Result := FBase.Submit(AWork, APolicy);
end;

function TObservedScheduler.PostUI(const AWork: INyxWork; const AParent: INyxExecution): INyxExecution;
var
  LWork: INyxWork;
begin
  LWork := AWork;

  if (Probe <> nil) and (Probe.FMode in [rmSource, rmCompletion]) and (AParent <> nil) then
  begin
    LWork := TReply.Create(AWork, Probe);
  end;
  Result := FBase.PostUI(LWork, AParent);
end;

procedure TObservedScheduler.Shutdown;
begin
  FBase.Shutdown;
end;

function TObservedScheduler.GetCanYieldUI: Boolean;
begin
  Result := FTurns.CanYieldUI;
end;

function TObservedScheduler.PostTurn(const AWork: INyxWork): INyxExecution;
begin
  Result := FTurns.PostTurn(AWork);
end;

function TObservedScheduler.GetWorkerLoad: TNyxWorkerPoolSnapshot;
var
  LMonitor: INyxSchedulerMonitor;
begin
  Supports(FBase, INyxSchedulerMonitor, LMonitor);
  Result := LMonitor.WorkerLoad;
end;
{$endif}

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
  FScheduler := NewNyxScheduler(TNyxSchedulerOptions.Defaults.Workers(2).PendingCapacity(8));
  {$ifndef PAS2JS}
  FObserved := TObservedScheduler.Create(FScheduler);
  FScheduler := FObserved;
  {$endif}
  FDocument := BuildNyxDocument;
  FAuthored := TNyxCodec.Encode(FDocument);
  FApp := TApplicationHost.Create;
  {$ifdef PAS2JS}FApp.Run(FDocument, TJSHTMLElement(document.body));
  {$else}FApp.Mount(FDocument);{$endif}
  FStore := FApp.Collections.Collection(NyxCollection('measured'));
  StartStage;
end;

destructor TJourney.Destroy;
begin
  {$ifndef PAS2JS}

  if FProbe <> nil then
  begin
    FProbe.Release;
  end;
  FObserved.Probe := nil;
  FObserved.Anchor := nil;
  {$endif}

  if FRun <> nil then
  begin
    FRun.Cancel;
  end;
  FLast := nil;
  FStatsLease := nil;
  FIntLease := nil;
  FCountLease := nil;
  FTextLease := nil;
  FAnchor := nil;
  FApp.Free;
  FStore := nil;
  FDocument.Free;
  FScheduler.Shutdown;
  inherited Destroy;
end;

procedure TJourney.Caption(const AText: TNyxText);
begin
  FScheduler.RequireUI;
  Inc(FReceived);
  FStore.Update(FStore.Snapshot.ItemAt(0).WithValue(NyxTextField('caption'), AText));
end;

procedure TJourney.ReceiveStats(const AValues: TNyxSequenceArray<IMetrics>);
var
  LPrefix: TNyxText;
begin
  Check((Length(AValues) = 1) and (AValues[0] <> nil), 'UI receives exactly one complete managed summary');
  FLast := AValues[0];
  LPrefix := 'Summary';

  if FStage = 0 then
  begin
    LPrefix := 'Source order';
  end;

  if FStage = 1 then
  begin
    LPrefix := 'Completion order';
  end;
  Caption(LPrefix + ': ' + IntToStr(FLast.Count) + ' rows / total ' + IntToStr(FLast.Sum));
end;

procedure TJourney.ReceiveInteger(const AValues: TNyxSequenceArray<Integer>);
begin
  Check(Length(AValues) = 1, 'UI receives exactly one typed Integer');
  FValue := AValues[0];
  Caption('Total: ' + IntToStr(FValue));
end;

procedure TJourney.ReceiveCount(const AValues: TNyxSequenceArray<TNyxQueryCount>);
begin
  Check(Length(AValues) = 1, 'UI receives exactly one exact wide count');
  FCount := AValues[0];
  Caption('Count: ' + FCount.ToText);
end;

procedure TJourney.ReceiveText(const AValues: TNyxSequenceArray<TNyxText>);
begin
  Check(Length(AValues) = 1, 'UI receives exactly one typed fold text');
  FText := AValues[0];
  Caption('Fold: ' + FText);
end;

procedure TJourney.Cell(const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  Check(Pos(AText, FApp.View.ElementFor('measured-table').textContent) > 0,
    'actual browser application table displays the final summary');
  {$else}
  Check(TNyxText(TStringGrid(FApp.View.ControlFor('measured-table')).Cells[0, 1]) = AText,
    'actual native application table displays the final summary');
  {$endif}
end;

procedure TJourney.DisposeView;
begin
  FreeAndNil(FApp);
  FDisposed := True;
  Check(FGeneration.Cancelled, 'application disposal cancels the in-flight final merge');
end;

procedure TJourney.StartStage;
var
  LOptions: TNyxParallelOptions;
  LMode: TMode;
  LHook: TDisposeHook;
  LFactory: TFactory;
  LAccumulators: INyxPartitionAccumulatorFactory<Integer, IMetrics>;
  LMergers: INyxParallelMergerFactory<IMetrics, IMetrics>;
  LSource: INyxSequence<Integer>;
  LProgram: INyxParallelProgram<Integer, Integer>;
  LTextProgram: INyxParallelProgram<Integer, TNyxText>;
  LSum: INyxParallelFold<Integer, Integer>;
  LCount: INyxParallelFold<TNyxText, TNyxQueryCount>;
  LText: INyxParallelFold<Integer, TNyxText>;
  LRefused: Boolean;
begin
  FLast := nil;
  FStatsLease := nil;
  FIntLease := nil;
  FCountLease := nil;
  FTextLease := nil;
  FRun := nil;
  FAnchor := nil;
  FReceived := 0;
  FPolls := 0;
  FApp.ShowPage('measured-view');
  FGeneration := NyxEventGeneration(FApp.View.Events);
  FBefore := FStore.Snapshot.Revision;
  LOptions := Options;
  LMode := rmNormal;
  LHook := nil;
  case FStage of
    0: LMode := rmSource;
    1:
    begin
      LMode := rmCompletion;
      LOptions := LOptions.Order(npoCompletion);
    end;
    3:
    begin
      LMode := rmCollect;
      LOptions := LOptions.Failures(npfCollect);
    end;
    4:
    begin
      LMode := rmStop;
      LOptions := LOptions.Concurrency(1);
    end;
    5: LMode := rmMergeFailure;
    6: LMode := rmCancelMerge;
    7: LMode := rmNoAccumulator;
    8: LMode := rmNoMerger;
    17:
    begin
      LMode := rmDispose;
      {$ifdef PAS2JS}LHook := DisposeView;{$endif}
    end;
  end;
  FProbe := TProbe.Create(LMode, LHook);
  FAnchor := FProbe;
  {$ifndef PAS2JS}
  FObserved.Probe := FProbe;
  FObserved.Anchor := FProbe;
  {$endif}
  LFactory := TFactory.Create(FProbe);
  LAccumulators := LFactory;
  LMergers := LFactory;
  FStatsLease := TNyxQueryReceiver<IMetrics>.Bind(FScheduler, ReceiveStats, FGeneration);
  FIntLease := TNyxQueryReceiver<Integer>.Bind(FScheduler, ReceiveInteger, FGeneration);
  FCountLease := TNyxQueryReceiver<TNyxQueryCount>.Bind(FScheduler, ReceiveCount, FGeneration);
  FTextLease := TNyxQueryReceiver<TNyxText>.Bind(FScheduler, ReceiveText, FGeneration);
  LSource := NyxRange(0, 4);
  case FStage of
    2:
    begin
      LSource := NyxRange(0, 4097);
      LOptions := LOptions.PartitionSize(128);
    end;
    9:
    begin
      LProgram := TNyxParallelSource<Integer>.From(NyxRange(0, 8))
        .Where(TNyxPartitionPredicate<Integer>.From(Even))
        .Select(TNyxPartitionSelector<Integer, Integer>.From(DoubleValue))
        .WithOptions(LOptions);
      LSum := TNyxParallelNumeric.IntegerSum;
      FRun := TNyxProgramReduction<Integer, Integer, Integer, Integer>.Submit(LProgram,
        LSum.Accumulators, LSum.Mergers, FScheduler, FIntLease.Port, FGeneration);
    end;
    10:
    begin
      LProgram := TNyxParallelSource<Integer>.From(NyxRange(0, 8))
        .Where(TNyxPartitionPredicate<Integer>.From(Even)).WithOptions(LOptions);
      LTextProgram := TNyxParallelMap<Integer, Integer, TNyxText>.From(
        LProgram, TNyxPartitionSelector<Integer, TNyxText>.From(Describe));
      LCount := TNyxParallelCounts<TNyxText>.LongCount;
      FRun := TNyxProgramReduction<Integer, TNyxText, TNyxQueryCount, TNyxQueryCount>.Submit(
        LTextProgram, LCount.Accumulators, LCount.Mergers, FScheduler, FCountLease.Port, FGeneration);
    end;
    11:
    begin
      LRefused := False;
      try
        TNyxParallelReduction<Integer, IMetrics, IMetrics>.Submit([], nil, LMergers,
          FScheduler, FStatsLease.Port, LOptions);
      except
        on LException: ENyxQuery do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (FProbe.Snapshot.Factories = 0),
        'nil accumulator factory refuses before source/admission entry');
      LRefused := False;
      try
        TNyxParallelReduction<Integer, IMetrics, IMetrics>.Submit([], LAccumulators, nil,
          FScheduler, FStatsLease.Port, LOptions);
      except
        on LException: ENyxQuery do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (FProbe.Snapshot.MergeFactories = 0),
        'nil merger factory refuses before admission entry');
      {$ifdef PAS2JS}
      LRefused := False;
      try
        TNyxParallelReduction<Integer, IMetrics, IMetrics>.Submit([], LAccumulators, LMergers,
          FScheduler, FStatsLease.Port, LOptions.Execution(npxWorkers));
      except
        on LException: ENyxSchedule do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (FProbe.Snapshot.Factories = 0),
        'required browser workers refuse explicitly even for empty reduction');
      {$endif}
      FRun := TNyxParallelNumeric.IntegerSum.WithOptions(LOptions).Submit(
        TNyxSequence<Integer>.Empty, FScheduler, FIntLease.Port, FGeneration);
    end;
    12, 13:
    begin

      if FStage = 13 then
      begin
        LOptions := LOptions.PartitionSize(1);
      end;
      FRun := TNyxParallelReduction<Integer, Integer, Integer>.Submit([High(Integer), 1],
        TNyxParallelNumeric.IntegerSum.Accumulators, TNyxParallelNumeric.IntegerSum.Mergers,
        FScheduler, FIntLease.Port, LOptions, FGeneration);
    end;
    14:
    begin
      GSeeds := 0;
      LText := TNyxParallelFold<Integer, TNyxText>.From(
        TNyxReductionSeed<TNyxText>.From(FreshTextSeed),
        TTextNumberFactory.From(AppendNumber),
        TTextCombineFactory.From(AppendText));
      Check(LText.Options.ExecutionMode = npxWorkers, 'original immutable plan retains worker defaults');
      LText := LText.WithOptions(LOptions);
      FRun := LText.Submit(LSource, FScheduler, FTextLease.Port, FGeneration);
    end;
    15:
    begin
      LSource := TNyxSelect<Integer, Integer>.From(
        LSource, TNyxSelector<Integer, Integer>.From(FailSource));
    end;
  end;

  if FRun = nil then
  begin
    FRun := TNyxParallelReduction<Integer, IMetrics, IMetrics>.Submit(
      LSource, LAccumulators, LMergers, FScheduler, FStatsLease.Port, LOptions, FGeneration);
  end;

  if FStage = 16 then
  begin
    FApp.ShowPage('next-view');
    Check(FRun.Phase = nqpCancelled, 'pending reduction follows actual page navigation');
  end;
  Check(TNyxCodec.Encode(FDocument) = FAuthored, 'submission preserves exact authored defaults');
end;

procedure TJourney.CompleteStage;
var
  LData: TMeasurements;
  LTrace: TNyxText;
  LPrefix: TNyxText;
begin
  LData := FProbe.Snapshot;
  Check(LData.PartialCreated = LData.PartialDestroyed, 'all managed partial states retire');
  Check(TNyxCodec.Encode(FDocument) = FAuthored, 'reduction preserves exact authored defaults');
  case FStage of
    0, 1, 2:
    begin
      Check(FRun.Phase = nqpSucceeded, 'complete owned reduction succeeds');
      Check((FReceived = 1) and (FStore.Snapshot.Revision = FBefore + 1),
        'one final summary makes one admitted UI collection update');

      if FStage = 2 then
      begin
        Check((FLast.Count = 4097) and (FLast.Sum = 8390656), 'large reduction retains exact full input');
        Check((LData.Factories = 33) and (LData.PartialCount = 33) and (LData.Adds = 4097),
          'intermediate summary count follows partitions including short tail');
      end
      else
      begin
        LTrace := '0/🌙,1/🌙';
        {$ifndef PAS2JS}

        if FStage = 1 then
        begin
          LTrace := '1/🌙,0/🌙';
        end;
        {$endif}
        Check((FLast.Trace = LTrace) and (FLast.Sum = 6) and (FLast.Count = 4),
          'configured order survives actual reverse native partition completion');
      end;
      Check((LData.Merges = 1) and (LData.MergeFactories = 1), 'one independently owned final merge');
      {$ifndef PAS2JS}
      Check((LData.WorkerAdds = LData.Adds) and (LData.WorkerMerges = 1),
        'native accumulation and final merge both execute on workers');
      {$endif}
      LPrefix := 'Summary';

      if FStage = 0 then
      begin
        LPrefix := 'Source order';
      end;

      if FStage = 1 then
      begin
        LPrefix := 'Completion order';
      end;
      Cell(LPrefix + ': ' + IntToStr(FLast.Count) + ' rows / total ' + IntToStr(FLast.Sum));
      Check(LData.FinalDestroyed = 0, 'retained result outlives all partial states');
      FLast := nil;
      Check(FProbe.Snapshot.FinalDestroyed = 1, 'final managed summary retires once on caller release');
    end;
    3:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.FailureCount = 2) and (LData.Merges = 0),
        'Collect preserves both partition errors and starts no final merge');
    end;
    4:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.FailureCount = 1) and (LData.Adds = 2),
        'Stop cancels later native admission at first observed failure');
    end;
    5:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.Failure = 'Final reduction merge failed'),
        'final merger error publishes no result');
    end;
    6:
    begin
      Check(FRun.Phase = nqpCancelled, 'merger context cancels its whole run');
    end;
    7, 8:
    begin
      Check((FRun.Phase = nqpFailed) and (LData.Merges = 0), 'missing owned callback refuses explicitly');
    end;
    9:
    begin
      Check((FRun.Phase = nqpSucceeded) and (FValue = 24), 'fused filter/map Integer sum is exact');
      Cell('Total: 24');
    end;
    10:
    begin
      Check((FRun.Phase = nqpSucceeded) and (FCount.ToText = '4'),
        'typed result-changing program reduces to exact wide count');
      Cell('Count: 4');
    end;
    11:
    begin
      Check((FRun.Phase = nqpSucceeded) and (FValue = 0), 'empty seeded Integer sum merges identity once');
      Cell('Total: 0');
    end;
    12:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.FailureCount = 1),
        'partial Integer overflow refuses without final delivery');
    end;
    13:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.FailureCount = 0),
        'final Integer overflow refuses after independently valid partials');
    end;
    14:
    begin
      Check((FRun.Phase = nqpSucceeded) and (FText = '0123') and (GSeeds = 3),
        'fresh partition/merge seeds and ordered noncommutative fold remain exact');
      Cell('Fold: 0123');
    end;
    15:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.Failure = 'Reduction source failed') and
        (LData.Factories = 0), 'source failure starts no partial worker accumulators');
    end;
    16:
    begin
      Check((FRun.Phase = nqpCancelled) and (LData.Factories = 0),
        'navigation suppresses queued reduction admission');
    end;
    17:
    begin
      Check((FRun.Phase = nqpCancelled) and FDisposed and (FReceived = 0),
        'actual application disposal suppresses a finishing worker/UI merge');
    end;
  end;

  if FStage in [3, 4, 5, 6, 7, 8, 12, 13, 15, 16, 17] then
  begin
    Check((FReceived = 0) and (FStore.Snapshot.Revision = FBefore),
      'failed/cancelled reduction permits no partial or late UI collection result');
  end;
end;

procedure TJourney.Finish(const AFailure: TNyxText);
begin
  Done := True;
  {$ifdef PAS2JS}

  if AFailure <> '' then
  begin
    document.body.textContent := 'FAIL ' + AFailure;
    document.body.setAttribute('data-nyx-reductions', 'failed');
  end
  else
  begin
    document.body.setAttribute('data-nyx-reductions', 'passed');
    document.body.setAttribute('data-nyx-reduction-checks', IntToStr(FChecks));
  end;
  window.setTimeout(@ReleaseJourney, 0);
  {$else}

  if AFailure <> '' then
  begin
    WriteLn('FAIL ', AFailure);
    ExitCode := 1;
  end
  else
  begin
    WriteLn('PASS ', FChecks, ' parallel reductions / actual application UI checks');
  end;
  {$endif}
end;

procedure TJourney.Poll;
var
  LMonitor: INyxSchedulerMonitor;
  LData: TMeasurements;
begin
  try
    Inc(FPolls);

    if FPolls > 2000 then
    begin
      raise ENyxQuery.Create('Reduction observation expired');
    end;
    {$ifndef PAS2JS}

    if (FStage = 17) and not FDisposed and FProbe.MergeEntered then
    begin
      DisposeView;
      FProbe.Release;
    end;
    {$endif}

    if FRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;
    Supports(FScheduler, INyxSchedulerMonitor, LMonitor);
    LData := FProbe.Snapshot;

    if (LMonitor.WorkerLoad.Running <> 0) or (LData.PartialCreated <> LData.PartialDestroyed) then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;

    if FStage <= 17 then
    begin
      CompleteStage;
      Inc(FStage);

      if FStage <= 17 then
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
    {$ifdef PAS2JS}GJourney.Poll;{$else}
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
      document.body.setAttribute('data-nyx-reductions', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}GJourney.Free;{$endif}
end.
