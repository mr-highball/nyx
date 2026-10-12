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
program nyx_linq_parallel_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  nyx.linq.pipeline,
  SysUtils, Classes, nyx.text, nyx.model, nyx.codec, nyx.generated.view,
  nyx.collections, nyx.collections.view, nyx.collections.mount,
  nyx.linq, nyx.linq.collections, nyx.linq.execution, nyx.linq.publication,
  nyx.linq.parallel, nyx.scheduler
  {$ifdef PAS2JS}, Web, nyx.render.browser
  {$else}, SyncObjs, Interfaces, Forms, Grids, nyx.render.lcl{$endif};

type
  {$ifdef PAS2JS}TRenderer = TNyxBrowserRenderer; THost = TJSHTMLElement;
  {$else}TRenderer = TNyxLCLRenderer; THost = TForm;{$endif}

  { All shared counters are lock-protected. A native event holds partition zero
    until UI observes another worker's already accepted reply. UI never waits
    on that event. This establishes overlapping threads and controlled reply order. }
  IProbe = interface
    ['{787E3367-135A-47A4-9B2B-67BC09BDE88E}']
    procedure Enter(APartition: Integer; AFirst: Boolean);
    procedure Leave;
    procedure Replied;
    procedure ReleaseGate;
    procedure Created;
    procedure Destroyed;
    function Calls: Integer;
    function Replies: Integer;
    function Peak: Integer;
    function Live: Integer;
    function Made: Integer;
    function Held: Boolean;
  end;
  TProbe = class(TInterfacedObject, IProbe)
  private
    FCalls, FReplies, FActive, FPeak, FMade, FLive: Integer;
    {$ifndef PAS2JS}FLock: TRTLCriticalSection; FGate, FEntered: TEvent;{$endif}
    FHold, FHeld: Boolean;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(AHold: Boolean);
    destructor Destroy; override;
    procedure Enter(APartition: Integer; AFirst: Boolean);
    procedure Leave;
    procedure Replied;
    procedure ReleaseGate;
    procedure Created;
    procedure Destroyed;
    function Calls: Integer;
    function Replies: Integer;
    function Peak: Integer;
    function Live: Integer;
    function Made: Integer;
    function Held: Boolean;
  end;

  { Observe the existing public scheduler's worker handoff, without modifying its
    queues or executing work. The delegate still owns real threads/UI dispatch. }
  TObservedScheduler = class(TInterfacedObject, INyxScheduler, INyxSchedulerTurns)
  private
    FBase: INyxScheduler;
    FProbe: IProbe;
  public
    constructor Create(const ABase: INyxScheduler; const AProbe: IProbe);
    function GetCapabilities: TNyxSchedulerCapabilities;
    function GetCanYieldUI: Boolean;
    function PostTurn(const AWork: INyxWork): INyxExecution;
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    function Submit(const AWork: INyxWork; APolicy: TNyxExecutionPolicy): INyxExecution;
    function PostUI(const AWork: INyxWork; const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
  end;
  TTextSelector = class(TInterfacedObject, INyxSelector<Integer, TNyxText>)
  private
    FProbe: IProbe;
    FPartition: Integer;
    FFirst: Boolean;
    FFailures: Boolean;
  public
    constructor Create(const AProbe: IProbe; APartition: Integer; AFailures: Boolean);
    destructor Destroy; override;
    function Project(const AValue: Integer): TNyxText;
  end;
  TTextFactory = class(TInterfacedObject, INyxPartitionSelectorFactory<Integer, TNyxText>)
  private
    FProbe: IProbe;
    FFailures: Boolean;
    FNil: Boolean;
  public
    constructor Create(const AProbe: IProbe; AFailures, ANil: Boolean);
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<Integer, TNyxText>;
  end;
  TRowSelector = class(TInterfacedObject, INyxSelector<TNyxCollectionItem, TNyxCollectionItem>)
  private
    FProbe: IProbe;
    FPartition: Integer;
    FFirst: Boolean;
  public
    constructor Create(const AProbe: IProbe; APartition: Integer);
    destructor Destroy; override;
    function Project(const AValue: TNyxCollectionItem): TNyxCollectionItem;
  end;
  TRowFactory = class(TInterfacedObject,
    INyxPartitionSelectorFactory<TNyxCollectionItem, TNyxCollectionItem>)
  private
    FProbe: IProbe;
  public
    constructor Create(const AProbe: IProbe);
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<TNyxCollectionItem, TNyxCollectionItem>;
  end;

  TJourney = class
  private
    FDocument: TNyxDocument;
    FAuthored: TNyxText;
    FRenderer: TRenderer;
    FHost: THost;
    FBase: INyxScheduler;
    FScheduler: INyxScheduler;
    FProbe: IProbe;
    FRun: INyxParallelRun;
    FTextReceiver: INyxQueryReceiver<TNyxText>;
    FRowsReceiver: INyxCollectionQueryReceiver;
    FTexts: TNyxSequenceArray<TNyxText>;
    FStore: INyxCollection;
    FMount: INyxCollectionMount;
    FObserver: INyxCollectionSubscription;
    FStage, FChecks, FPolls, FReceived, FNotifies, FRevision, FRefreshes: Integer;
    FSteered: Boolean;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Receive(const AValues: TNyxSequenceArray<TNyxText>);
    procedure Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
    procedure StartStage;
    procedure Admission;
    procedure CompleteStage;
    procedure Cell(const AExpected: TNyxText);
    procedure Finish(const AFailure: TNyxText);
  public
    Done: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Poll;
  end;

var
  GJourney: TJourney;

{$ifdef PAS2JS}
procedure ReleaseJourney;
begin
  GJourney.Free;
  GJourney := nil;
  document.body.setAttribute('data-nyx-parallel-disposed', 'true');
end;
{$endif}

function GlobalProjection(const AValue: Integer): TNyxText;
begin
  {$ifndef PAS2JS}

  if GetCurrentThreadID <> MainThreadID then
  begin
    raise ENyxQuery.Create('Explicit deferred UI selector entered on a worker');
  end;
  {$endif}
  Result := 'Result ' + TNyxText(IntToStr(AValue)) + TNyxText(' / 🌙');
end;

function ProgramEven(const AValue: Integer): Boolean;
begin
  Result := (AValue mod 2) = 0;
end;

constructor TProbe.Create(AHold: Boolean);
begin
  inherited Create;
  FHold := AHold;
  {$ifndef PAS2JS}
  InitCriticalSection(FLock);
  FGate := TEvent.Create(nil, True, False, '');
  FEntered := TEvent.Create(nil, True, False, '');
  {$endif}
end;

destructor TProbe.Destroy;
begin
  {$ifndef PAS2JS}FGate.Free; FEntered.Free; DoneCriticalSection(FLock);{$endif}
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

procedure TProbe.Enter(APartition: Integer; AFirst: Boolean);
begin
  Lock;
  try
    Inc(FCalls);
    Inc(FActive);

    if AFirst and (APartition = 0) then
    begin
      FHeld := True;
    end;

    if FActive > FPeak then
    begin
      FPeak := FActive;
    end;
  finally
    Unlock;
  end;
  {$ifndef PAS2JS}

  if GetCurrentThreadID = MainThreadID then
  begin
    raise ENyxQuery.Create('Required worker callback entered on UI');
  end;

  if FHold and AFirst and (APartition = 0) then
  begin
    FEntered.SetEvent;

    if FGate.WaitFor(5000) <> wrSignaled then
    begin
      raise ENyxQuery.Create('Owned worker gate exceeded its observation deadline');
    end;
  end;

  if FHold and AFirst and (APartition = 1) then
  begin

    if FEntered.WaitFor(5000) <> wrSignaled then
    begin
      raise ENyxQuery.Create('First worker entry exceeded its observation deadline');
    end;
  end;
  {$endif}
end;

procedure TProbe.Leave;
begin
  Lock;
  try
    Dec(FActive);
  finally
    Unlock;
  end;
end;

procedure TProbe.Replied;
begin
  Lock;
  try
    Inc(FReplies);
  finally
    Unlock;
  end;
end;

procedure TProbe.ReleaseGate;
begin
  {$ifndef PAS2JS}FGate.SetEvent; FEntered.SetEvent;{$endif}
end;

procedure TProbe.Created;
begin
  Lock;
  try
    Inc(FMade);
    Inc(FLive);
  finally
    Unlock;
  end;
end;

procedure TProbe.Destroyed;
begin
  Lock;
  try
    Dec(FLive);
  finally
    Unlock;
  end;
end;

function TProbe.Calls: Integer;
begin
  Lock;
  try
    Result := FCalls;
  finally
    Unlock;
  end;
end;

function TProbe.Replies: Integer;
begin
  Lock;
  try
    Result := FReplies;
  finally
    Unlock;
  end;
end;

function TProbe.Peak: Integer;
begin
  Lock;
  try
    Result := FPeak;
  finally
    Unlock;
  end;
end;

function TProbe.Live: Integer;
begin
  Lock;
  try
    Result := FLive;
  finally
    Unlock;
  end;
end;

function TProbe.Made: Integer;
begin
  Lock;
  try
    Result := FMade;
  finally
    Unlock;
  end;
end;

function TProbe.Held: Boolean;
begin
  Lock;
  try
    Result := FHeld;
  finally
    Unlock;
  end;
end;

constructor TObservedScheduler.Create(const ABase: INyxScheduler; const AProbe: IProbe);
begin
  inherited Create;
  FBase := ABase;
  FProbe := AProbe;
end;

function TObservedScheduler.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := FBase.Capabilities;
end;

procedure TObservedScheduler.RequireUI;
begin
  FBase.RequireUI;
end;

function TObservedScheduler.GetCanYieldUI: Boolean;
begin
  Result := NyxSchedulerTurns(FBase).CanYieldUI;
end;

function TObservedScheduler.PostTurn(const AWork: INyxWork): INyxExecution;
begin
  Result := NyxSchedulerTurns(FBase).PostTurn(AWork);
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
begin
  Result := FBase.PostUI(AWork, AParent);
  {$ifndef PAS2JS}

  if GetCurrentThreadID <> MainThreadID then
  begin
    FProbe.Replied;
  end;
  {$endif}
end;

procedure TObservedScheduler.Shutdown;
begin
  FBase.Shutdown;
end;

constructor TTextSelector.Create(const AProbe: IProbe; APartition: Integer; AFailures: Boolean);
begin
  inherited Create;
  FProbe := AProbe;
  FPartition := APartition;
  FFailures := AFailures;
  FFirst := True;
  FProbe.Created;
end;

destructor TTextSelector.Destroy;
begin
  FProbe.Destroyed;
  inherited Destroy;
end;

function TTextSelector.Project(const AValue: Integer): TNyxText;
begin
  FProbe.Enter(FPartition, FFirst);
  FFirst := False;
  try

    if FFailures and (AValue in [0, 2]) then
    begin

      if AValue = 2 then
      begin
        raise ENyxQuery.Create('');
      end;
      raise ENyxQuery.Create('Partition failed / 🌙');
    end;
    Result := 'Result ' + TNyxText(IntToStr(AValue)) + TNyxText(' / 🌙');
  finally
    FProbe.Leave;
  end;
end;

constructor TTextFactory.Create(const AProbe: IProbe; AFailures, ANil: Boolean);
begin
  inherited Create;
  FProbe := AProbe;
  FFailures := AFailures;
  FNil := ANil;
end;

function TTextFactory.NewSelector(APartition, ASourceOffset: Integer): INyxSelector<Integer, TNyxText>;
begin

  if FNil then
  begin
    Result := nil;
    Exit;
  end;
  Result := TTextSelector.Create(FProbe, APartition, FFailures);
end;

constructor TRowSelector.Create(const AProbe: IProbe; APartition: Integer);
begin
  inherited Create;
  FProbe := AProbe;
  FPartition := APartition;
  FFirst := True;
  FProbe.Created;
end;

destructor TRowSelector.Destroy;
begin
  FProbe.Destroyed;
  inherited Destroy;
end;

function TRowSelector.Project(const AValue: TNyxCollectionItem): TNyxCollectionItem;
begin
  FProbe.Enter(FPartition, FFirst);
  FFirst := False;
  try
    Result := AValue.WithValue(NyxTextField('caption'), 'Parallel / 🌙');
  finally
    FProbe.Leave;
  end;
end;

constructor TRowFactory.Create(const AProbe: IProbe);
begin
  inherited Create;
  FProbe := AProbe;
end;

function TRowFactory.NewSelector(APartition, ASourceOffset: Integer): INyxSelector<TNyxCollectionItem, TNyxCollectionItem>;
begin
  Result := TRowSelector.Create(FProbe, APartition);
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxQuery.Create('Parallel query / stage ' + IntToStr(FStage) + ' / ' + AReason);
  end;
  Inc(FChecks);
end;

procedure TJourney.Receive(const AValues: TNyxSequenceArray<TNyxText>);
var
  LIndex: Integer;
begin
  FBase.RequireUI;
  Inc(FReceived);
  SetLength(FTexts, Length(AValues));
  for LIndex := 0 to High(FTexts) do
  begin
    FTexts[LIndex] := AValues[LIndex];
  end;
end;

procedure TJourney.Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
begin
  FBase.RequireUI;
  Inc(FNotifies);
  Check(AChanges.After.Count = 3, 'observer sees complete three-row candidate');
  FRefreshes := FMount.RefreshCount;
end;

constructor TJourney.Create;
begin
  inherited Create;
  FBase := NewNyxScheduler(TNyxSchedulerOptions.Defaults.Workers(2).PendingCapacity(8));
  FDocument := BuildNyxDocument;
  FAuthored := TNyxCodec.Encode(FDocument);
  FRenderer := TRenderer.Create;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(FHost);
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(0, 0, 800, 700);
  {$endif}
  FRenderer.Render(FDocument, FDocument.Pages[0], FHost);
  FStore := FRenderer.CollectionView('measured-table').Store;
  FMount := FRenderer.CollectionMount('measured-table');
  FObserver := FStore.Subscribe(Changed);
  Cell('Explore the collection');
  Admission;
  StartStage;
end;

procedure TJourney.Admission;
var
  LFactory: INyxPartitionSelectorFactory<Integer, TNyxText>;
  LReceiver: INyxQueryReceiver<TNyxText>;
  LOptions, LCopy: TNyxParallelOptions;
  LRun: INyxParallelRun;
  LRefused: Boolean;
begin
  LFactory := TNyxPartitionSelector<Integer, TNyxText>.From(GlobalProjection);
  LReceiver := TNyxQueryReceiver<TNyxText>.Bind(FBase, Receive);
  LOptions := Default(TNyxParallelOptions);
  LRefused := False;
  try
    LRun := TNyxParallelSelect<Integer, TNyxText>.Submit([1], LFactory, FBase, LReceiver.Port, LOptions);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LRun = nil), 'uninitialized options refuse before input/work adoption');
  LOptions := TNyxParallelOptions.Defaults.Execution(npxDeferredUI).MaximumInput(1);
  LRefused := False;
  try
    LRun := TNyxParallelSelect<Integer, TNyxText>.Submit([1, 2], LFactory, FBase, LReceiver.Port, LOptions);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LRun = nil), 'configured input limit refuses the complete oversized input');
  LRefused := False;
  try
    LCopy := LOptions.Concurrency(65);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'concurrency refuses unsupported native worker counts');
  LRefused := False;
  try
    LCopy := LOptions.PartitionSize(0);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'zero-sized partition cannot stall progress');
  LCopy := LOptions.Concurrency(1).PartitionSize(1);
  Check((LCopy.ConcurrentPartitions = 1) and (LOptions.ConcurrentPartitions = 4),
    'fluent options preserve independent earlier configuration');
  Check(FReceived = 0, 'admission refusals neither execute selectors nor deliver results');
end;

destructor TJourney.Destroy;
begin

  if FRun <> nil then
  begin
    FRun.Cancel;
  end;

  if FProbe <> nil then
  begin
    FProbe.ReleaseGate;
  end;
  FTextReceiver := nil;
  FRowsReceiver := nil;
  FObserver := nil;
  FRenderer.Free;
  {$ifdef PAS2JS}

  if FHost <> nil then
  begin
    FHost.remove;
  end;
  {$else}FHost.Free;{$endif}
  FDocument.Free;

  if FBase <> nil then
  begin
    FBase.Shutdown;
  end;
  inherited Destroy;
end;

procedure TJourney.Cell(const AExpected: TNyxText);
begin
  {$ifdef PAS2JS}
  Check(Pos(AExpected, FRenderer.ElementFor('measured-table').textContent) > 0,
    'actual browser table reflects accepted result');
  {$else}
  Check(TNyxText(TStringGrid(FRenderer.ControlFor('measured-table')).Cells[0, 1]) = AExpected,
    'actual LCL table reflects accepted result');
  {$endif}
end;

procedure TJourney.StartStage;
var
  LValues: TNyxSequenceArray<Integer>;
  LRows: TNyxSequenceArray<TNyxCollectionItem>;
  LSnapshot: INyxCollectionSnapshot;
  LTextFactory: INyxPartitionSelectorFactory<Integer, TNyxText>;
  LRowFactory: INyxPartitionSelectorFactory<TNyxCollectionItem, TNyxCollectionItem>;
  LOptions: TNyxParallelOptions;
  LProgram: INyxParallelProgram<Integer, Integer>;
  LTextProgram: INyxParallelProgram<Integer, TNyxText>;
  LSequence: INyxSequence<Integer>;
  LIndex, LCount: Integer;
  {$ifdef PAS2JS}LRefused: Boolean;{$endif}
begin
  FRun := nil;
  FTextReceiver := nil;
  FRowsReceiver := nil;
  FTexts := nil;
  FReceived := 0;
  FNotifies := 0;
  FPolls := 0;
  FSteered := False;
  FRevision := FStore.Snapshot.Revision;
  FProbe := TProbe.Create(FStage in [0, 1, 4, 5, 7, 9, 12]);
  FScheduler := TObservedScheduler.Create(FBase, FProbe);
  FTextReceiver := TNyxQueryReceiver<TNyxText>.Bind(FScheduler, Receive);
  LOptions := TNyxParallelOptions.Defaults.Concurrency(2).PartitionSize(2);
  {$ifdef PAS2JS}LOptions := LOptions.Execution(npxDeferredUI);{$endif}
  LCount := 6;
  case FStage of
    0:
    begin
      LCount := 4097;
      LOptions := LOptions.PartitionSize(128);
    end;
    1:
    begin
      LCount := 3;
      LOptions := LOptions.Order(npoCompletion);
    end;
    2: LOptions := LOptions.Failures(npfCollect);
    3: LOptions := LOptions.Concurrency(1).PartitionSize(1);
    9:
    begin
      LCount := 64;
      LOptions := LOptions.Concurrency(64).PartitionSize(1);
    end;
    10: LCount := 0;
    11: LOptions := LOptions.Execution(npxDeferredUI);
  end;
  SetLength(LValues, LCount);
  for LIndex := 0 to High(LValues) do
  begin
    LValues[LIndex] := LIndex;
  end;
  LTextFactory := TTextFactory.Create(FProbe, FStage in [2, 3], FStage = 8);

  if FStage = 11 then
  begin
    LTextFactory := TNyxPartitionSelector<Integer, TNyxText>.From(GlobalProjection);
  end;

  if FStage in [5, 6, 7] then
  begin
    FRowsReceiver := NyxCollectionQueryReceiver(FScheduler, FStore);
    LSnapshot := FStore.Snapshot;
    LRows := LSnapshot.AsNyx.ToArray;
    LRowFactory := TRowFactory.Create(FProbe);
    FRun := TNyxParallelSelect<TNyxCollectionItem, TNyxCollectionItem>.Submit(LRows,
      LRowFactory, FScheduler, FRowsReceiver.Port, LOptions.PartitionSize(1));
    LRows := nil;
    LRowFactory := nil;

    if FStage = 6 then
    begin
      FStore.Update(FStore.Snapshot.ItemAt(0).WithValue(NyxTextField('caption'), 'Local change'));
      FRevision := FStore.Snapshot.Revision;
      FNotifies := 0;
    end;
    {$ifdef PAS2JS}

    if FStage = 7 then
    begin
      FRowsReceiver.Close;
      FRenderer.Unmount;
      FSteered := True;
    end;
    {$endif}
  end
  else
  begin
    {$ifdef PAS2JS}

    if FStage = 0 then
    begin
      LRefused := False;
      try
        FRun := TNyxParallelSelect<Integer, TNyxText>.Submit(LValues, LTextFactory,
          FScheduler, FTextReceiver.Port, LOptions.Execution(npxWorkers));
      except
        on ENyxSchedule do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (FRun = nil), 'browser refuses required workers without fallback');
    end;
    {$endif}
    if FStage in [2, 3, 11] then
    begin
      LSequence := TNyxSequence<Integer>.From(LValues);
      LProgram := TNyxParallelSource<Integer>.From(LSequence);

      if FStage = 11 then
      begin
        LProgram := LProgram.Where(TNyxPartitionPredicate<Integer>.From(ProgramEven));
      end;
      LTextProgram := TNyxParallelMap<Integer, Integer, TNyxText>.From(LProgram, LTextFactory)
        .WithOptions(LOptions);
      FRun := LTextProgram.Submit(FScheduler, FTextReceiver.Port);
    end
    else
    begin
      FRun := TNyxParallelSelect<Integer, TNyxText>.Submit(LValues, LTextFactory,
        FScheduler, FTextReceiver.Port, LOptions);
    end;
    {$ifdef PAS2JS}

    if FStage = 4 then
    begin
      FRun.Cancel;
      FSteered := True;
    end;

    if FStage = 12 then
    begin
      FBase.Shutdown;
      FSteered := True;
    end;
    {$endif}
  end;
  { Mutation/release of caller arrays/factories must not change admitted inputs. }
  for LIndex := 0 to High(LValues) do
  begin
    LValues[LIndex] := -100;
  end;
  LValues := nil;
  LTextFactory := nil;
end;

procedure TJourney.CompleteStage;
var
  LIndex: Integer;
  LFailure: TNyxPartitionFailure;
begin
  Check(FProbe.Live = 0, 'all independent selectors retire before next stage');
  case FStage of
    0:
    begin
      Check((FRun.Phase = nqpSucceeded) and (FReceived = 1) and (Length(FTexts) = 4097),
        'complete large type-changing result delivered once');
      for LIndex := 0 to High(FTexts) do
      begin
        Check(FTexts[LIndex] = TNyxText('Result ') + TNyxText(IntToStr(LIndex)) + TNyxText(' / 🌙'),
          'source order/Unicode survive independent owned partitions');
      end;
      Check(FProbe.Made = 33, 'partition count includes the final one-row tail');
      {$ifndef PAS2JS}Check(FProbe.Peak = 2, 'two real worker callbacks overlap within concurrency bound');{$endif}
    end;
    1:
    begin
      Check((FRun.Phase = nqpSucceeded) and (Length(FTexts) = 3), 'completion-ordered map succeeds');
      {$ifndef PAS2JS}
      Check((FTexts[0] = TNyxText('Result 2 / 🌙')) and (FTexts[1] = TNyxText('Result 0 / 🌙')),
        'controlled native reply order differs from source order');
      Check(FProbe.Peak = 2, 'completion-order consumer also overlaps actual workers');
      {$else}
      Check((FTexts[0] = 'Result 0 / 🌙') and (FTexts[2] = 'Result 2 / 🌙'),
        'explicit browser UI partitions preserve their observed completion order');
      {$endif}
    end;
    2:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.FailureCount = 2) and (FReceived = 0),
        'collect policy retains both failed partitions and publishes no partial result');
      Check(FProbe.Calls = 4, 'failed partitions stop locally while the independent third completes');
      Check(FProbe.Made = 3, 'collect executes every partition');
      for LIndex := 0 to 1 do
      begin
        LFailure := FRun.FailureAt(LIndex);
        Check(LFailure.SourceOffset = LFailure.Partition * 2, 'failure retains exact source ordinal');

        if LFailure.Partition = 0 then
        begin
          Check(LFailure.Message = TNyxText('Partition failed / 🌙'), 'failure diagnostic preserves Unicode');
        end
        else
        begin
          Check((LFailure.Partition = 1) and (LFailure.Message = ''), 'empty diagnostic remains a failed partition');
        end;
      end;
      Check(FRun.Failure = FRun.FailureAt(0).Message, 'receipt retains first observed partition failure');
    end;
    3:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.FailureCount = 1) and (FReceived = 0),
        'stop policy fails without partial publication');
      Check((FProbe.Made = 1) and (FProbe.Calls = 1), 'one-partition concurrency stops before scheduling siblings');
    end;
    4, 12:
    begin
      Check((FRun.Phase = nqpCancelled) and (FReceived = 0), 'running/queued/shutdown cancellation reaches no receiver');
      {$ifndef PAS2JS}
      Check((FProbe.Calls > 0) and (FProbe.Calls < 6), 'running owned worker finishes cooperatively before remaining rows');
      {$else}Check(FProbe.Calls = 0, 'queued browser alternative never enters cancelled callbacks');{$endif}
    end;
    5:
    begin
      Check((FRun.Phase = nqpSucceeded) and (FNotifies = 1) and
        (FStore.Snapshot.Revision = FRevision + 1), 'complete parallel rows publish one collection revision');
      Check(FRefreshes > 0, 'actual table synchronized before collection observer');
      Cell('Parallel / 🌙');
    end;
    6:
    begin
      Check((FRun.Phase = nqpFailed) and (FNotifies = 0) and
        (FStore.Snapshot.Revision = FRevision), 'changed target revision rejects complete parallel result');
      Cell('Local change');
    end;
    7:
    begin
      Check((FRun.Phase = nqpDiscarded) and (FNotifies = 0) and not FMount.Connected,
        'retired actual view discards late parallel result');
    end;
    8:
    begin
      Check((FRun.Phase = nqpFailed) and (FReceived = 0) and (FProbe.Made = 0),
        'nil selector factory result fails without callback or publication');
    end;
    9:
    begin
      {$ifndef PAS2JS}
      Check((FRun.Phase = nqpFailed) and (FReceived = 0) and
        (FRun.Failure = TNyxText('Native worker pending capacity is exhausted')),
        'shared scheduler capacity refusal stops siblings without UI fallback');
      {$else}
      Check((FRun.Phase = nqpSucceeded) and (Length(FTexts) = 64),
        'explicit UI execution does not claim a native pool capacity');
      {$endif}
    end;
    10:
    begin
      Check((FRun.Phase = nqpSucceeded) and (FReceived = 1) and (Length(FTexts) = 0)
        and (FProbe.Made = 0), 'empty input delivers once without creating a selector');
    end;
    11:
    begin
      Check((FRun.Phase = nqpSucceeded) and (FReceived = 1) and (Length(FTexts) = 3),
        'fluent filter/type-changing map executes explicit UI partitions on both targets');
      Check((FTexts[0] = TNyxText('Result 0 / 🌙')) and (FTexts[2] = TNyxText('Result 4 / 🌙')),
        'fluent program preserves filtered source order and exact typed output');
    end;
  end;
  Check(TNyxCodec.Encode(FDocument) = FAuthored, 'parallel runtime preserves exact authored design');
end;

procedure TJourney.Finish(const AFailure: TNyxText);
begin
  Done := True;
  {$ifdef PAS2JS}

  if AFailure = '' then
  begin
    document.body.setAttribute('data-nyx-parallel', 'passed');
    document.body.setAttribute('data-nyx-parallel-checks', IntToStr(FChecks));
  end
  else
  begin
    document.body.setAttribute('data-nyx-parallel', 'failed');
    document.body.textContent := 'FAIL ' + AFailure;
  end;
  window.setTimeout(@ReleaseJourney, 0);
  {$else}

  if AFailure = '' then
  begin
    WriteLn('PASS ', FChecks, ' partitioned queries / actual collection-control checks');
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

    if FPolls > 1000 then
    begin
      raise ENyxQuery.Create('Partition completion/retirement exceeded its observation deadline');
    end;
    {$ifndef PAS2JS}

    if not FSteered and (FStage in [4, 7, 12]) and FProbe.Held then
    begin
      FSteered := True;
      case FStage of
        4: FRun.Cancel;
        7:
        begin
          FRowsReceiver.Close;
          FRenderer.Unmount;
        end;
        12: FBase.Shutdown;
      end;
      FProbe.ReleaseGate;
    end;

    if (FProbe.Replies > 0) or (FRun.Phase in [nqpFailed, nqpCancelled]) then
    begin
      FProbe.ReleaseGate;
    end;
    {$endif}
    if not Supports(FBase, INyxSchedulerMonitor, LMonitor) then
    begin
      raise ENyxQuery.Create('Bounded pool observation is unavailable');
    end;

    if (FRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI]) or
      (FProbe.Live <> 0) or (LMonitor.WorkerLoad.Running <> 0) or
      (LMonitor.WorkerLoad.Pending <> 0) then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 10);{$endif}
      Exit;
    end;

    if FStage <= 12 then
    begin
      CompleteStage;
      Inc(FStage);
    end;

    if FStage <= 12 then
    begin
      StartStage;
      {$ifdef PAS2JS}window.setTimeout(@Poll, 10);{$endif}
      Exit;
    end;

    if LMonitor.WorkerLoad.ActiveWorkers <> 0 then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 10);{$endif}
      Exit;
    end;
    Finish('');
  except
    on LException: Exception do
    begin
      FProbe.ReleaseGate;
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
      document.body.setAttribute('data-nyx-parallel', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}WriteLn('FAIL ', LException.Message); ExitCode := 1;{$endif}
    end;
  end;
  {$ifndef PAS2JS}GJourney.Free;{$endif}
end.
