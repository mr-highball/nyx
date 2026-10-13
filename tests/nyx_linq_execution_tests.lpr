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
program nyx_linq_execution_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.model, nyx.codec, nyx.generated.view,
  nyx.collections, nyx.collections.view, nyx.collections.mount,
  nyx.collections.view.types, nyx.linq, nyx.linq.collections,
  nyx.linq.execution, nyx.linq.publication, nyx.linq.parallel, nyx.scheduler
  {$ifdef PAS2JS}, Web, nyx.render.browser
  {$else}, Interfaces, Forms, Grids, nyx.render.lcl{$endif};

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  {$endif}

  { Owned immutable-input selector. ThreadID is written only during its single
    enumeration and read after the scheduler's completed UI handoff. No control,
    live store or borrowed stack callback enters the worker. }
  TCaptionSelector = class(TInterfacedObject, INyxSelector<TNyxCollectionItem, TNyxCollectionItem>)
  public
    Caption: TNyxText;
    {$ifndef PAS2JS}ThreadID: TThreadID;{$endif}
    function Project(const AValue: TNyxCollectionItem): TNyxCollectionItem;
  end;
  TFailureSelector = class(TInterfacedObject, INyxSelector<TNyxCollectionItem, TNyxCollectionItem>)
  public
    EmptyFailure: Boolean;
    function Project(const AValue: TNyxCollectionItem): TNyxCollectionItem;
  end;
  TCancelSelector = class(TInterfacedObject, INyxSelector<TNyxCollectionItem, TNyxCollectionItem>)
  public
    Run: INyxQueryRun;
    Calls: Integer;
    function Project(const AValue: TNyxCollectionItem): TNyxCollectionItem;
  end;

  { Measure the actual control synchronization at the exact publication boundary.
    Browser viewport animation frames legitimately refresh outside this operation;
    their counts must not be confused with duplicate dataset notifications. }
  TPublicationProbe = class(TInterfacedObject, INyxQueryResultPort<TNyxCollectionItem>)
  public
    Inner: INyxQueryResultPort<TNyxCollectionItem>;
    Mount: INyxCollectionMount;
    Refreshes: Integer;
    function Publish(const AValues: TNyxSequenceArray<TNyxCollectionItem>): Boolean;
  end;

  { This consumer owns both actual target controls and borrowed observers. Each
    result lease is revoked before teardown; retained jobs/ports cannot enter it. }
  TJourney = class
  private
    FDocument: TNyxDocument;
    FRenderer: TRenderer;
    FHost: THost;
    FScheduler: INyxScheduler;
    FStore: INyxCollection;
    FView: INyxCollectionView;
    FMount: INyxCollectionMount;
    FObserver: INyxCollectionSubscription;
    FReceiver: INyxCollectionQueryReceiver;
    FRun: INyxQueryRun;
    FOutcomeRun: INyxQueryRun;
    FOutcomeLease: INyxQueryReceiver<TNyxQueryOutcome>;
    FOutcomeGeneration: INyxCancellationScope;
    FOutcome: TNyxQueryOutcome;
    FOutcomeCalls: Integer;
    FDelayedSource: INyxQueryProgress;
    FMethodLease: INyxQueryReceiver<Integer>;
    FSelector: INyxSelector<TNyxCollectionItem, TNyxCollectionItem>;
    FSelectorObject: TCaptionSelector;
    FPublication: TPublicationProbe;
    FPublicationPort: INyxQueryResultPort<TNyxCollectionItem>;
    FCancelObject: TCancelSelector;
    FCancelOwner: INyxSelector<TNyxCollectionItem, TNyxCollectionItem>;
    FStage: Integer;
    FChecks: Integer;
    FPolls: Integer;
    FNotifies: Integer;
    FMethodCalls: Integer;
    FBeforeRevision: Integer;
    FAuthored: TNyxText;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
    procedure ReceiveIntegers(const AValues: TNyxSequenceArray<Integer>);
    procedure ReceiveOutcome(const AValues: TNyxSequenceArray<TNyxQueryOutcome>);
    procedure StartStage;
    procedure CompleteStage;
    procedure Cell(const AExpected: TNyxText);
    procedure Finish(const AFailure: TNyxText);
  public
    Done: Boolean;
    Failed: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Poll;
  end;

var
  GJourney: TJourney;

{$ifdef PAS2JS}
{ Dispose on a later event-loop turn after Poll has returned. No method's stack
  runs on an already freed receiver; no timer remains after this release. }
procedure ReleaseJourney;
begin
  GJourney.Free;
  GJourney := nil;
  document.body.setAttribute('data-nyx-query-disposed', 'true');
end;
{$endif}

function TCaptionSelector.Project(const AValue: TNyxCollectionItem): TNyxCollectionItem;
begin
  {$ifndef PAS2JS}ThreadID := GetCurrentThreadID;{$endif}

  if Caption = '' then
  begin
    Result := AValue.WithValue(NyxTextField('caption'), 'Prepared / 🌙');
  end
  else
  begin
    Result := AValue.WithValue(NyxTextField('caption'), Caption);
  end;
end;

function TFailureSelector.Project(const AValue: TNyxCollectionItem): TNyxCollectionItem;
begin

  if AValue.Ref.ID = 'row-1' then
  begin

    if EmptyFailure then
    begin
      raise ENyxQuery.Create('');
    end;
    raise ENyxQuery.Create('Query failed / 🌙');
  end;
  Result := AValue;
end;

function TPublicationProbe.Publish(const AValues: TNyxSequenceArray<TNyxCollectionItem>): Boolean;
var
  LBefore: Integer;
begin
  LBefore := Mount.RefreshCount;
  try
    Result := Inner.Publish(AValues);
  finally
    Refreshes := Mount.RefreshCount - LBefore;
  end;
end;

function TCancelSelector.Project(const AValue: TNyxCollectionItem): TNyxCollectionItem;
begin
  Inc(Calls);

  if Calls = 2 then
  begin
    Run.Cancel;
  end;
  Result := AValue;
end;

{ Pure immutable partition input; no live UI/store or captured frame enters a
  worker. A real coordinator failure must reach the same terminal observer. }
function FailParallelCaption(const AValue: TNyxCollectionItem): TNyxCollectionItem;
begin

  if AValue.Ref.ID = 'row-1' then
  begin
    raise ENyxQuery.Create('Parallel query failed / 🌙');
  end;
  Result := AValue;
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxQuery.Create('Query execution stage ' + IntToStr(FStage) + ': ' + AReason);
  end;
  Inc(FChecks);
end;

procedure TJourney.Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
begin
  FScheduler.RequireUI;
  Inc(FNotifies);
  Check(AChanges.After.Count = 3, 'observer receives complete three-row result');
end;

procedure TJourney.ReceiveIntegers(const AValues: TNyxSequenceArray<Integer>);
begin
  FScheduler.RequireUI;
  Inc(FMethodCalls);
  Check((Length(AValues) = 3) and (AValues[2] = 3), 'method receives complete owned values');
end;

constructor TJourney.Create;
begin
  inherited Create;
  FScheduler := NewNyxScheduler(TNyxSchedulerOptions.Defaults.Workers(2).PendingCapacity(8));
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
  FView := FRenderer.CollectionView('measured-table');
  FStore := FView.Store;
  FMount := FRenderer.CollectionMount('measured-table');
  FObserver := FStore.Subscribe(Changed);
  Cell('Explore the collection');
  StartStage;
end;

procedure TJourney.ReceiveOutcome(const AValues: TNyxSequenceArray<TNyxQueryOutcome>);
begin
  FScheduler.RequireUI;
  Check(Length(AValues) = 1, 'outcome port receives exactly one terminal value on UI');
  Inc(FOutcomeCalls);
  FOutcome := AValues[0];

  if FStage = 16 then
  begin
    raise ENyxQuery.Create('Outcome receiver failed / 🌙');
  end;

  if FStage = 17 then
  begin
    raise ENyxQuery.Create('');
  end;
end;

destructor TJourney.Destroy;
begin

  if FOutcomeRun <> nil then
  begin
    FOutcomeRun.Cancel;
  end;
  FOutcomeLease := nil;
  FOutcomeRun := nil;
  FOutcomeGeneration := nil;
  FDelayedSource := nil;

  if FRun <> nil then
  begin
    FRun.Cancel;
  end;
  FReceiver := nil;
  FMethodLease := nil;
  FObserver := nil;
  FMount := nil;
  FRenderer.Free;
  {$ifdef PAS2JS}

  if FHost <> nil then
  begin
    FHost.remove;
  end;
  {$else}FHost.Free;{$endif}
  FDocument.Free;

  if FScheduler <> nil then
  begin
    FScheduler.Shutdown;
  end;
  inherited Destroy;
end;

procedure TJourney.Cell(const AExpected: TNyxText);
begin
  {$ifdef PAS2JS}
  Check(Pos(AExpected, FRenderer.ElementFor('measured-table').textContent) > 0,
    'actual browser table reflects accepted rows');
  {$else}
  Check(TNyxText(TStringGrid(FRenderer.ControlFor('measured-table')).Cells[0, 1]) = AExpected,
    'actual LCL table reflects accepted rows');
  {$endif}
end;

procedure TJourney.StartStage;
var
  LRows: INyxSequence<TNyxCollectionItem>;
  LSnapshot: INyxCollectionSnapshot;
  LValues: TNyxSequenceArray<TNyxCollectionItem>;
  LPort: INyxQueryResultPort<Integer>;
  LPolicy: TNyxExecutionPolicy;
  LFailure: TFailureSelector;
  LCaption: TCaptionSelector;
  LOptions: TNyxParallelOptions;
  {$ifdef PAS2JS}LRefused: Boolean;{$endif}
begin
  FOutcomeLease := nil;
  FOutcomeRun := nil;
  FOutcomeGeneration := nil;
  FDelayedSource := nil;
  FOutcomeCalls := 0;
  FReceiver := nil;
  FRun := nil;

  if FStage = 13 then
  begin
    { Stage 9 deliberately navigated away. Return through the real renderer
      before qualifying new successful publications, with independent bindings. }
    FObserver := nil;
    FRenderer.Render(FDocument, FDocument.Pages[0], FHost);
    FView := FRenderer.CollectionView('measured-table');
    FStore := FView.Store;
    FMount := FRenderer.CollectionMount('measured-table');
    FObserver := FStore.Subscribe(Changed);
    Check(FMount.Connected, 'returned view owns a new actual collection binding');
  end;
  FBeforeRevision := FStore.Snapshot.Revision;
  FNotifies := 0;
  FPolls := 0;
  FReceiver := NyxCollectionQueryReceiver(FScheduler, FStore);
  FPublication := TPublicationProbe.Create;
  FPublicationPort := FPublication;
  FPublication.Inner := FReceiver.Port;
  FPublication.Mount := FMount;
  LSnapshot := FStore.Snapshot;
  LRows := LSnapshot.AsNyx;
  case FStage of
    0, 6:
    begin
      FSelectorObject := TCaptionSelector.Create;
      FSelector := FSelectorObject;
      LRows := LRows.Select(FSelector);
      LPolicy := neUIQueue;

      if FStage = 6 then
      begin
        {$ifdef PAS2JS}
        LRefused := False;
        try
          FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FPublicationPort, neThreaded);
        except
          on ENyxSchedule do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused and (FRun = nil), 'browser refuses real-worker policy synchronously');
        LPolicy := neAsynchronous;
        {$else}
        LPolicy := neThreaded;
        {$endif}
        { Make a real changed publication distinct from the first success. }
        FStore.Update(FStore.Snapshot.ItemAt(0).WithValue(NyxTextField('caption'), 'Before worker'));
        FReceiver := NyxCollectionQueryReceiver(FScheduler, FStore);
        FPublication.Inner := FReceiver.Port;
        FBeforeRevision := FStore.Snapshot.Revision;
        FNotifies := 0;
      end;
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FPublicationPort, LPolicy);
      LRows := nil;
      Check(FRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI], 'submission remains nonblocking');
    end;
    1:
    begin
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FPublicationPort, neSequential);
      Check(FRun.Phase = nqpAwaitingUI, 'sequential compute still defers publication');
      FStore.Update(FStore.Snapshot.ItemAt(0).WithValue(NyxTextField('caption'), 'Local edit'));
      FBeforeRevision := FStore.Snapshot.Revision;
      FNotifies := 0;
    end;
    2:
    begin
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FPublicationPort, neSequential);
      FReceiver.Close;
    end;
    3:
    begin
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FPublicationPort);
      FRun.Cancel;
    end;
    4:
    begin
      LRows := LRows.Select(TFailureSelector.Create);
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FPublicationPort);
    end;
    5:
    begin
      SetLength(LValues, 2);
      LValues[0] := FStore.Snapshot.ItemAt(0);
      LValues[1] := LValues[0];
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(TNyxSequence<TNyxCollectionItem>.From(LValues),
        FScheduler, FPublicationPort);
    end;
    7:
    begin
      FMethodLease := TNyxQueryReceiver<Integer>.Bind(FScheduler, ReceiveIntegers);
      FRun := TNyxQuery<Integer>.Submit(TNyxSequence<Integer>.From([1, 2, 3]),
        FScheduler, FMethodLease.Port, neSequential);
    end;
    8:
    begin
      FMethodLease := TNyxQueryReceiver<Integer>.Bind(FScheduler, ReceiveIntegers);
      LPort := FMethodLease.Port;
      FRun := TNyxQuery<Integer>.Submit(TNyxSequence<Integer>.From([1, 2, 3]),
        FScheduler, LPort, neSequential);
      FMethodLease := nil;
    end;
    9:
    begin
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FPublicationPort, neSequential);
      FReceiver := nil;
      FRenderer.Unmount;
      Check(not FMount.Connected, 'navigation retires actual control binding');
    end;
    10:
    begin
      FCancelObject := TCancelSelector.Create;
      FCancelOwner := FCancelObject;
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows.Select(FCancelOwner), FScheduler, FPublicationPort);
      FCancelObject.Run := FRun;
    end;
    11:
    begin
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FPublicationPort, neSequential);
      Check(FRun.Phase = nqpAwaitingUI, 'result is complete but publication still queued');
      FRun.Cancel;
    end;
    12:
    begin
      LFailure := TFailureSelector.Create;
      LFailure.EmptyFailure := True;
      LRows := LRows.Select(LFailure);
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FPublicationPort);
    end;
    13, 14, 15, 16, 17:
    begin
      LCaption := TCaptionSelector.Create;
      LCaption.Caption := TNyxText('Prepared / 🌙 / ') + TNyxText(IntToStr(FStage));
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows.Select(LCaption), FScheduler, FPublicationPort);
    end;
    18:
    begin
      { An independently owned pending source makes multiple delayed checks
        deterministic without sleeping/blocking a UI thread or inventing work. }
      FDelayedSource := NewNyxQueryProgress;
      FDelayedSource.BeginCompute;
      FRun := FDelayedSource;
    end;
    19:
    begin
      LOptions := TNyxParallelOptions.Defaults.PartitionSize(1).Concurrency(2);

      if not (nscWorkerThreads in FScheduler.Capabilities) then
      begin
        LOptions := LOptions.Execution(npxDeferredUI);
      end;
      FRun := TNyxParallelSelect<TNyxCollectionItem, TNyxCollectionItem>.Submit(LRows,
        TNyxPartitionSelector<TNyxCollectionItem, TNyxCollectionItem>.From(FailParallelCaption),
        FScheduler, FPublicationPort, LOptions);
    end;
    20:
    begin
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FPublicationPort);
      FOutcomeLease := TNyxQueryReceiver<TNyxQueryOutcome>.Bind(FScheduler, ReceiveOutcome);
      FOutcomeRun := TNyxQueryObservation.Observe(FRun, FScheduler, FOutcomeLease.Port,
        TNyxQueryObservationOptions.Defaults);
      FScheduler.Shutdown;
    end;
  end;

  if FStage < 20 then
  begin
    FOutcomeGeneration := NewNyxCancellationScope;
    FOutcomeLease := TNyxQueryReceiver<TNyxQueryOutcome>.Bind(FScheduler, ReceiveOutcome);
    FOutcomeRun := TNyxQueryObservation.Observe(FRun, FScheduler, FOutcomeLease.Port,
      TNyxQueryObservationOptions.Defaults.CheckEvery(1), FOutcomeGeneration);
    Check(FOutcomeCalls = 0, 'observation never enters the port synchronously');

    if FStage = 13 then
    begin
      FOutcomeRun.Cancel;
    end;

    if FStage = 14 then
    begin
      FOutcomeGeneration.Cancel;
    end;

    if FStage = 15 then
    begin
      FOutcomeLease.Close;
    end;
  end;
end;

procedure TJourney.CompleteStage;
begin
  case FStage of
    0, 6, 13, 14, 15, 16, 17:
    begin
      Check(FRun.Phase = nqpSucceeded, 'complete result reaches current receiver');
      Check((FStore.Snapshot.Revision = FBeforeRevision + 1) and (FNotifies = 1),
        'one revision and one coherent observer delivery');
      Check(FPublication.Refreshes = 1, 'one mounted-table synchronization within successful publication');

      if FStage < 13 then
      begin
        Cell('Prepared / 🌙');
      end
      else
      begin
        Cell(TNyxText('Prepared / 🌙 / ') + TNyxText(IntToStr(FStage)));
      end;
      {$ifndef PAS2JS}

      if FStage in [0, 6] then
      begin
        Check((FSelectorObject.ThreadID = MainThreadID) = (FStage = 0),
          'execution policy chooses UI or an actual native worker');
      end;
      {$endif}
      FSelector := nil;
      FSelectorObject := nil;
    end;
    1:
    begin
      Check((FRun.Phase = nqpFailed) and
        (FRun.Failure = TNyxText('Collection changed before query result delivery')), 'stale revision rejects exact result');
      Cell('Local edit');
    end;
    2, 8, 9:
    begin
      Check(FRun.Phase = nqpDiscarded, 'revoked receiver discards late publication');
      Check(FMethodCalls = Ord(FStage >= 8), 'revocation prevents borrowed receiver entry');
    end;
    3, 10, 11, 20:
    begin
      Check(FRun.Phase = nqpCancelled, 'caller/shutdown cancellation is observable');

      if FStage = 20 then
      begin
        Check((FOutcomeRun.Phase = nqpCancelled) and (FOutcomeCalls = 0),
          'scheduler shutdown retires queued observation without terminal receiver entry');
      end;

      if FStage = 10 then
      begin
        Check(FCancelObject.Calls = 2, 'cancellation stops between rows without entering the third callback');
        FCancelObject.Run := nil;
        FCancelOwner := nil;
        FCancelObject := nil;
      end;
    end;
    4:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.Failure = TNyxText('Query failed / 🌙')),
        'failure after one row preserves exact diagnostic and no partial result');
    end;
    5:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.Failure <> ''), 'invalid whole candidate refuses');
    end;
    12:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.Failure = ''), 'empty exception diagnostic still means failed');
    end;
    7:
    begin
      Check((FRun.Phase = nqpSucceeded) and (FMethodCalls = 1), 'owned method port delivers once on UI');
      FMethodLease.Close;
      FMethodLease := nil;
    end;
    18:
    begin
      Check(FRun.Phase = nqpDiscarded, 'manually completed pending source reports its terminal outcome');
    end;
    19:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.Failure = TNyxText('Parallel query failed / 🌙')),
        'real partition failure retains exact terminal diagnostics without partial publication');
    end;
  end;

  if FStage < 20 then
  begin

    if FStage in [13, 14] then
    begin
      Check((FOutcomeRun.Phase = nqpCancelled) and (FOutcomeCalls = 0),
        'observation cancellation/generation suppresses callback without cancelling successful source');
    end
    else if FStage = 15 then
    begin
      Check((FOutcomeRun.Phase = nqpDiscarded) and (FOutcomeCalls = 0),
        'revoked outcome receiver refuses terminal entry');
    end
    else if FStage in [16, 17] then
    begin
      Check((FOutcomeRun.Phase = nqpFailed) and (FOutcomeCalls = 1) and
        ((FStage = 17) or (FOutcomeRun.Failure = TNyxText('Outcome receiver failed / 🌙'))),
        'outcome receiver failure stays on its independent observation receipt');
      Check((FStage <> 17) or (FOutcomeRun.Failure = ''), 'empty observer diagnostic still means failed');
    end
    else
    begin
      Check((FOutcomeRun.Phase = nqpSucceeded) and (FOutcomeCalls = 1),
        'terminal outcome enters a current receiver exactly once');
      Check((FOutcome.Phase = FRun.Phase) and (FOutcome.Failure = FRun.Failure),
        'observation copies exact success/discard/cancellation/failure including empty diagnostics');
    end;
  end;

  if not (FStage in [0, 6, 13, 14, 15, 16, 17]) then
  begin
    Check((FStore.Snapshot.Revision = FBeforeRevision) and (FNotifies = 0),
      'failed/revoked/cancelled/method result preserves accepted collection');
    Check(FPublication.Refreshes = 0, 'no mounted-table synchronization within refused/absent publication');
  end;
  Check(TNyxCodec.Encode(FDocument) = FAuthored, 'runtime execution preserves exact authored document');
end;

procedure TJourney.Finish(const AFailure: TNyxText);
begin
  Done := True;
  Failed := AFailure <> '';
  {$ifdef PAS2JS}

  if Failed then
  begin
    document.body.textContent := 'FAIL ' + AFailure;
    document.body.setAttribute('data-nyx-query-execution', 'failed');
  end
  else
  begin
    document.body.setAttribute('data-nyx-query-execution', 'passed');
    document.body.setAttribute('data-nyx-query-checks', IntToStr(FChecks));
  end;
  window.setTimeout(@ReleaseJourney, 0);
  {$else}

  if Failed then
  begin
    WriteLn('FAIL ', AFailure);
    ExitCode := 1;
  end
  else
  begin
    WriteLn('PASS ', FChecks, ' owned query / actual collection-control checks');
  end;
  {$endif}
end;

procedure TJourney.Poll;
var
  LMonitor: INyxSchedulerMonitor;
begin
  try
    Inc(FPolls);
    if FPolls >= 1000 then
    begin
      raise ENyxQuery.Create('Execution/worker retirement exceeded the observation deadline');
    end;

    if (FStage = 18) and (FPolls = 3) then
    begin
      Check((FOutcomeCalls = 0) and (FOutcomeRun.Phase = nqpAwaitingUI),
        'delayed pending-source checks remain nonblocking and do not publish progress as completion');
      FDelayedSource.Finish(False);
    end;

    if (FRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI]) or
      ((FOutcomeRun <> nil) and (FOutcomeRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI])) then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 10);{$endif}
      Exit;
    end;
    if FStage <= 20 then
    begin
      CompleteStage;
      Inc(FStage);
    end;

    if FStage <= 20 then
    begin
      StartStage;
      {$ifdef PAS2JS}window.setTimeout(@Poll, 10);{$endif}
      Exit;
    end;
    { Shutdown is nonblocking; observing zero live workers is required before
      native heap/lifetime teardown, rather than assuming a terminal receipt suffices. }
    Check(Supports(FScheduler, INyxSchedulerMonitor, LMonitor), 'scheduler exposes bounded worker load');

    if LMonitor.WorkerLoad.ActiveWorkers <> 0 then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 10);{$endif}
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
      document.body.setAttribute('data-nyx-query-execution', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}GJourney.Free;{$endif}
end.
