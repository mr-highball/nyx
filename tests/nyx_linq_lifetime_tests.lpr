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

program nyx_linq_lifetime_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.model, nyx.codec, nyx.generated.view,
  nyx.collections, nyx.collections.mount, nyx.linq, nyx.linq.collections,
  nyx.linq.execution, nyx.linq.publication, nyx.linq.parallel, nyx.linq.pipeline,
  nyx.events, nyx.scheduler
  {$ifdef PAS2JS}, Web, nyx.application.browser
  {$else}, Interfaces, Forms, Grids, SyncObjs, nyx.application.lcl{$endif};

type
  {$ifdef PAS2JS}
  TApplicationHost = TNyxBrowserApplication;
  {$else}
  TApplicationHost = TNyxLCLApplication;
  {$endif}

  { The method receiver deliberately has a shorter lifetime than its retained
    lease. Its generation must refuse entry after application disposal, before
    any borrowed object/method is touched. }
  TMethodSink = class
    procedure Receive(const AValues: TNyxSequenceArray<Integer>);
  end;

  {$ifndef PAS2JS}
  { This single-partition fixture owns only immutable rows and two synchronization
    handles. It never reads a host/control/store. UI polls entry without waiting,
    navigates, then releases the entered callback. No timing guess proves overlap. }
  TWorkerGate = class(TInterfacedObject,
    INyxPartitionSelectorFactory<TNyxCollectionItem, TNyxCollectionItem>,
    INyxSelector<TNyxCollectionItem, TNyxCollectionItem>)
  private
    FEntered: TEvent;
    FReleased: TEvent;
    FCalls: LongInt;
  public
    constructor Create;
    destructor Destroy; override;
    function NewSelector(APartition, ASourceOffset: Integer):
      INyxSelector<TNyxCollectionItem, TNyxCollectionItem>;
    function Project(const AValue: TNyxCollectionItem): TNyxCollectionItem;
    function Entered: Boolean;
    function Calls: Integer;
    procedure Release;
  end;
  {$endif}

  TJourney = class
  private
    FDocument: TNyxDocument;
    FAuthored: TNyxText;
    FApp: TApplicationHost;
    FScheduler: INyxScheduler;
    FStore: INyxCollection;
    FObserver: INyxCollectionSubscription;
    FGeneration: INyxCancellationScope;
    FReceiver: INyxCollectionQueryReceiver;
    FMethodLease: INyxQueryReceiver<Integer>;
    FMethodPort: INyxQueryResultPort<Integer>;
    FSink: TMethodSink;
    FRun: INyxQueryRun;
    FMount: INyxCollectionMount;
    FBefore: Integer;
    FNotifies: Integer;
    FStage: Integer;
    FChecks: Integer;
    FPolls: Integer;
    FSteered: Boolean;
    {$ifndef PAS2JS}
    FGate: TWorkerGate;
    FGateOwner: INyxPartitionSelectorFactory<TNyxCollectionItem, TNyxCollectionItem>;
    {$endif}
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
    procedure Cell(const AExpected: TNyxText);
    procedure NavigateAway;
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
  GMethodCalls: Integer;

{$ifdef PAS2JS}
procedure ReleaseJourney;
begin
  GJourney.Free;
  GJourney := nil;
  document.body.setAttribute('data-nyx-lifetime-disposed', 'true');
end;
{$endif}

function Prepared(const AValue: TNyxCollectionItem): TNyxCollectionItem;
begin
  Result := AValue.WithValue(NyxTextField('caption'), 'Ready for the next chapter');
end;

procedure TMethodSink.Receive(const AValues: TNyxSequenceArray<Integer>);
begin
  Inc(GMethodCalls);
end;

{$ifndef PAS2JS}
constructor TWorkerGate.Create;
begin
  inherited Create;
  FEntered := TEvent.Create(nil, True, False, '');
  FReleased := TEvent.Create(nil, True, False, '');
end;

destructor TWorkerGate.Destroy;
begin
  FReleased.Free;
  FEntered.Free;
  inherited Destroy;
end;

function TWorkerGate.NewSelector(APartition, ASourceOffset: Integer):
  INyxSelector<TNyxCollectionItem, TNyxCollectionItem>;
begin
  Result := Self;
end;

function TWorkerGate.Project(const AValue: TNyxCollectionItem): TNyxCollectionItem;
begin

  if InterlockedIncrement(FCalls) = 1 then
  begin
    FEntered.SetEvent;

    if FReleased.WaitFor(10000) <> wrSignaled then
    begin
      raise ENyxQuery.Create('UI did not release the entered worker fixture');
    end;
  end;
  Result := Prepared(AValue);
end;

function TWorkerGate.Entered: Boolean;
begin
  Result := FEntered.WaitFor(0) = wrSignaled;
end;

function TWorkerGate.Calls: Integer;
begin
  Result := InterlockedCompareExchange(FCalls, 0, 0);
end;

procedure TWorkerGate.Release;
begin
  FReleased.SetEvent;
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

procedure TJourney.Changed(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
  FScheduler.RequireUI;
  Inc(FNotifies);
  Check(AChanges.After.Count = 3, 'observer sees only one complete admitted vector');
end;

constructor TJourney.Create;
begin
  inherited Create;
  FScheduler := NewNyxScheduler(TNyxSchedulerOptions.Defaults.Workers(1).PendingCapacity(8));
  FDocument := BuildNyxDocument;
  Check(FDocument.Count = 2, 'exact semantic companion supplies two navigation roots');
  FAuthored := TNyxCodec.Encode(FDocument);
  FApp := TApplicationHost.Create;
  {$ifdef PAS2JS}
  FApp.Run(FDocument, TJSHTMLElement(document.body));
  {$else}
  FApp.Mount(FDocument);
  {$endif}
  FStore := FApp.Collections.Collection(NyxCollection('measured'));
  FObserver := FStore.Subscribe(Changed);
  Cell('Explore the collection');
  StartStage;
end;

destructor TJourney.Destroy;
begin
  {$ifndef PAS2JS}

  if FGate <> nil then
  begin
    FGate.Release;
  end;
  {$endif}

  if FRun <> nil then
  begin
    FRun.Cancel;
  end;
  FMethodPort := nil;
  FMethodLease := nil;
  FReceiver := nil;
  FObserver := nil;
  FApp.Free;
  FSink.Free;
  FMount := nil;
  FGeneration := nil;
  FStore := nil;
  FDocument.Free;
  FScheduler.Shutdown;
  inherited Destroy;
end;

procedure TJourney.Cell(const AExpected: TNyxText);
begin
  {$ifdef PAS2JS}
  Check(Pos(AExpected, FApp.View.ElementFor('measured-table').textContent) > 0,
    'actual browser application table displays the exact committed caption');
  {$else}
  Check(TNyxText(TStringGrid(FApp.View.ControlFor('measured-table')).Cells[0, 1]) = AExpected,
    'actual native application table displays the exact committed caption');
  {$endif}
end;

procedure TJourney.NavigateAway;
var
  LRows: TNyxSequenceArray<TNyxCollectionItem>;
begin
  FApp.ShowPage('next-view');
  Check(FGeneration.Cancelled, 'application navigation retires its existing view generation');
  Check(not FMount.Connected, 'navigation disconnects the old actual-control mount');
  Check(FRun.Phase = nqpCancelled, 'retained query receipt observes navigation without manual Cancel');
  SetLength(LRows, 1);
  LRows[0] := FStore.Snapshot.ItemAt(0);
  Check(not FReceiver.Port.Publish(LRows), 'retained direct collection port refuses retired generation');
  Check(FStore.Snapshot.Revision = FBefore, 'direct revoked port preserves exact collection revision');
  {$ifdef PAS2JS}
  Check(Pos('The next chapter', document.body.textContent) > 0, 'actual next page is visible');
  {$else}
  Check(FApp.View.ControlFor('next-title') <> nil, 'actual native next page is mounted');
  {$endif}
end;

procedure TJourney.StartStage;
var
  LRows: INyxSequence<TNyxCollectionItem>;
  LValues: TNyxSequenceArray<TNyxCollectionItem>;
  LProgram: INyxParallelProgram<TNyxCollectionItem, TNyxCollectionItem>;
  LRefused: Boolean;
  LOptions: TNyxParallelOptions;
begin
  FRun := nil;
  FReceiver := nil;
  FPolls := 0;
  FSteered := False;
  FNotifies := 0;

  if FStage > 0 then
  begin
    FApp.ShowPage('measured-view');
  end;
  FGeneration := NyxEventGeneration(FApp.View.Events);
  Check(not FGeneration.Cancelled, 'each successful page mount supplies a live generation');
  FMount := FApp.View.CollectionMount('measured-table');
  FBefore := FStore.Snapshot.Revision;
  FReceiver := NyxCollectionQueryReceiver(FScheduler, FStore, FGeneration);
  LRows := NyxItems(FStore.Snapshot);
  LRows := TNyxSelect<TNyxCollectionItem, TNyxCollectionItem>.From(
    LRows, TNyxSelector<TNyxCollectionItem, TNyxCollectionItem>.From(Prepared));
  case FStage of
    0, 1:
    begin
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FReceiver.Port,
        neUIQueue, FGeneration);

      if FStage = 1 then
      begin
        LRefused := False;
        try
          FApp.ShowPage('missing-page');
        except
          on LException: ENyxModel do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused and not FGeneration.Cancelled,
          'refused navigation preserves the accepted generation and queued query');
      end;
    end;
    2:
    begin
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FReceiver.Port,
        neUIQueue, FGeneration);
      NavigateAway;
    end;
    3:
    begin
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FReceiver.Port,
        neSequential, FGeneration);
      Check(FRun.Phase = nqpAwaitingUI, 'whole serial computation awaits separate UI delivery');
      NavigateAway;
    end;
    4:
    begin
      { Capture must be interrupted after actual cursor entry, before EOF/factories.
        Repeated identities are legal query inputs, never admitted as a collection. }
      LRows := TNyxSequence<TNyxCollectionItem>.RepeatValue(FStore.Snapshot.ItemAt(0), 1000);
      LOptions := TNyxParallelOptions.Defaults.Execution(npxDeferredUI)
        .CaptureRowsPerTurn(1).PartitionSize(3).Concurrency(1);
      LProgram := TNyxParallelSource<TNyxCollectionItem>.From(LRows)
        .Select(TNyxPartitionSelector<TNyxCollectionItem, TNyxCollectionItem>.From(Prepared))
        .WithOptions(LOptions);
      FRun := LProgram.Submit(FScheduler, FReceiver.Port, FGeneration);
    end;
    5:
    begin
      { The eager input overload follows the same generation BEFORE factories. }
      LValues := LRows.ToArray;
      FRun := TNyxParallelSelect<TNyxCollectionItem, TNyxCollectionItem>.Submit(
        LValues, TNyxPartitionSelector<TNyxCollectionItem, TNyxCollectionItem>.From(Prepared),
        FScheduler, FReceiver.Port, TNyxParallelOptions.Defaults.Execution(npxDeferredUI),
        FGeneration);
      NavigateAway;
    end;
    6:
    begin
      {$ifdef PAS2JS}
      FRun := TNyxQuery<TNyxCollectionItem>.Submit(LRows, FScheduler, FReceiver.Port,
        neSequential, FGeneration);
      Check(not (nscWorkerThreads in FScheduler.Capabilities),
        'browser does not claim native threaded qualification');
      NavigateAway;
      {$else}
      FGate := TWorkerGate.Create;
      FGateOwner := FGate;
      LProgram := TNyxParallelSource<TNyxCollectionItem>.From(LRows)
        .Select(FGateOwner).WithOptions(TNyxParallelOptions.Defaults.Execution(npxWorkers)
          .PartitionSize(3).Concurrency(1));
      FRun := LProgram.Submit(FScheduler, FReceiver.Port, FGeneration);
      {$endif}
    end;
    7:
    begin
      FSink := TMethodSink.Create;
      FMethodLease := TNyxQueryReceiver<Integer>.Bind(FScheduler, FSink.Receive, FGeneration);
      FMethodPort := FMethodLease.Port;
      FRun := TNyxQuery<Integer>.Submit(TNyxSequence<Integer>.From([1, 2, 3]),
        FScheduler, FMethodPort, neSequential, FGeneration);
      Check(FRun.Phase = nqpAwaitingUI, 'borrowed receiver computation awaits UI');
      FreeAndNil(FApp);
      FreeAndNil(FSink);
      Check(FGeneration.Cancelled and not FMount.Connected,
        'application disposal retires generation and actual-control mounts');
      Check(FRun.Phase = nqpCancelled, 'disposal cancels a retained receipt without manual Cancel');
      Check(not FMethodPort.Publish([1, 2, 3]),
        'retained method port refuses before dereferencing destroyed receiver');
    end;
  end;
end;

procedure TJourney.CompleteStage;
var
  LRows: TNyxSequenceArray<TNyxCollectionItem>;
begin

  if FStage in [0, 1] then
  begin
    Check(FRun.Phase = nqpSucceeded, 'live application generation delivers complete result');

    if FStage = 0 then
    begin
      Check((FStore.Snapshot.Revision = FBefore + 1) and (FNotifies = 1),
        'successful query makes one atomic collection notification');
    end
    else
    begin
      Check((FStore.Snapshot.Revision = FBefore) and (FNotifies = 0),
        'unchanged complete result retains ordinary collection no-op semantics');
    end;
    Cell('Ready for the next chapter');
    FApp.ShowPage('next-view');
    Check(FRun.Phase = nqpSucceeded, 'later navigation does not rewrite successful history');
  end
  else
  begin
    Check(FRun.Phase = nqpCancelled, 'obsolete query retires as cancelled');
    Check((FStore.Snapshot.Revision = FBefore) and (FNotifies = 0),
      'navigation/disposal permits no partial or late collection notification');
    {$ifndef PAS2JS}

    if FStage = 6 then
    begin
      Check(FGate.Calls = 1, 'entered native worker finishes one row and skips later callbacks');
      FGateOwner := nil;
      FGate := nil;
    end;
    {$endif}
  end;
  Check(TNyxCodec.Encode(FDocument) = FAuthored, 'application queries preserve exact authored defaults/source');

  if FStage = 7 then
  begin
    SetLength(LRows, 1);
    LRows[0] := FStore.Snapshot.ItemAt(0);
    Check(not FReceiver.Port.Publish(LRows), 'collection port also refuses after application disposal');
    Check(GMethodCalls = 0, 'retired borrowed receiver is never entered');
  end;
end;

procedure TJourney.Finish(const AFailure: TNyxText);
begin
  Done := True;
  {$ifdef PAS2JS}

  if AFailure <> '' then
  begin
    document.body.textContent := 'FAIL ' + AFailure;
    document.body.setAttribute('data-nyx-lifetime', 'failed');
  end
  else
  begin
    document.body.setAttribute('data-nyx-lifetime', 'passed');
    document.body.setAttribute('data-nyx-lifetime-checks', IntToStr(FChecks));
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
    WriteLn('PASS ', FChecks, ' application navigation / query generation checks');
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
      raise ENyxQuery.Create('Application query lifetime observation expired');
    end;

    if (FStage = 4) and not FSteered and (FRun.Phase = nqpComputing) then
    begin
      FSteered := True;
      NavigateAway;
    end;
    {$ifndef PAS2JS}

    if (FStage = 6) and not FSteered and FGate.Entered then
    begin
      FSteered := True;
      NavigateAway;
      FGate.Release;
    end;
    {$endif}

    if FRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;
    {$ifndef PAS2JS}

    if (FStage = 6) and Supports(FScheduler, INyxSchedulerMonitor, LMonitor) and
      (LMonitor.WorkerLoad.Running <> 0) then
    begin
      Exit;
    end;
    {$endif}

    if FStage <= 7 then
    begin
      CompleteStage;
      Inc(FStage);

      if FStage <= 7 then
      begin
        StartStage;
        {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
        Exit;
      end;
      FScheduler.Shutdown;
    end;
    Check(Supports(FScheduler, INyxSchedulerMonitor, LMonitor), 'scheduler exposes retirement observation');

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
      document.body.setAttribute('data-nyx-lifetime', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}GJourney.Free;{$endif}
end.
