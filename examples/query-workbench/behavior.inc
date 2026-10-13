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

{ Query workbench helpers. Imported into the owned companion's implementation;
  the managed BuildNyxDocument function remains the exact MCP-generated design. }

type
  TTaskSummary = record
    Count: TNyxQueryCount;
    Points: TNyxInteger64;
  end;
  TTaskSummaryFolder = TNyxPartitionSelector<
    INyxFoldValue<TTaskSummary, TNyxCollectionItem>, TTaskSummary>;
  TTaskSummaryCombiner = TNyxPartitionSelector<
    INyxFoldValue<TTaskSummary, TTaskSummary>, TTaskSummary>;

  { Each partition owns its predicate. The captured flag is immutable; workers
    read only detached collection items, never application state or controls. }
  TTaskPredicate = class(TInterfacedObject, INyxPredicate<TNyxCollectionItem>)
  private
    FImportantOnly: Boolean;
  public
    constructor Create(AImportantOnly: Boolean);
    function Test(const AValue: TNyxCollectionItem): Boolean;
  end;
  TTaskPredicateFactory = class(TInterfacedObject,
    INyxPartitionPredicateFactory<TNyxCollectionItem>)
  private
    FImportantOnly: Boolean;
  public
    constructor Create(AImportantOnly: Boolean);
    function NewPredicate(APartition, ASourceOffset: Integer): INyxPredicate<TNyxCollectionItem>;
  end;

  { Work owns this receiver capability through final delivery. Its application
    context is revocable and retains no host/router/tree. The view generation,
    request version and source revision all qualify the result before State
    changes. One text value publishes the whole completed summary on UI. }
  TTaskSummaryPort = class(TInterfacedObject, INyxQueryResultPort<TTaskSummary>)
  private
    FContext: INyxApplicationContext;
    FScheduler: INyxScheduler;
    FGeneration: INyxCancellationScope;
    FRequest: Integer;
    FRevision: Integer;
  public
    constructor Create(const AContext: INyxApplicationContext; ARequest, ARevision: Integer);
    function Publish(const AValues: TNyxSequenceArray<TTaskSummary>): Boolean;
  end;

constructor TTaskPredicate.Create(AImportantOnly: Boolean);
begin
  inherited Create;
  FImportantOnly := AImportantOnly;
end;

function TTaskPredicate.Test(const AValue: TNyxCollectionItem): Boolean;
begin
  Result := not FImportantOnly or AValue.GetValue(NyxBooleanField('important'));
end;

constructor TTaskPredicateFactory.Create(AImportantOnly: Boolean);
begin
  inherited Create;
  FImportantOnly := AImportantOnly;
end;

function TTaskPredicateFactory.NewPredicate(APartition, ASourceOffset: Integer):
  INyxPredicate<TNyxCollectionItem>;
begin
  Result := TTaskPredicate.Create(FImportantOnly);
end;

function AddTask(const AValue: INyxFoldValue<TTaskSummary, TNyxCollectionItem>): TTaskSummary;
begin
  Result := AValue.Accumulator;
  Result.Count := Result.Count.Incremented;
  Result.Points := Result.Points.Add(
    TNyxInteger64.FromInteger(AValue.Value.GetValue(NyxIntegerField('points'))));
end;

function CombineTaskSummaries(const AValue: INyxFoldValue<TTaskSummary, TTaskSummary>): TTaskSummary;
var
  LRight: TTaskSummary;
begin
  Result := AValue.Accumulator;
  LRight := AValue.Value;
  Result.Count := Result.Count.Added(LRight.Count);
  Result.Points := Result.Points.Add(LRight.Points);
end;

constructor TTaskSummaryPort.Create(const AContext: INyxApplicationContext;
  ARequest, ARevision: Integer);
begin
  inherited Create;
  FContext := AContext;
  FScheduler := AContext.Scheduler;
  FGeneration := AContext.Generation;
  FRequest := ARequest;
  FRevision := ARevision;
end;

function TTaskSummaryPort.Publish(const AValues: TNyxSequenceArray<TTaskSummary>): Boolean;
var
  LSummary: TTaskSummary;
begin
  FScheduler.RequireUI;
  Result := False;

  if FGeneration.Cancelled or not FContext.Active then
  begin
    Exit;
  end;

  if FContext.State.GetValue(NyxIntegerState('query-request')) <> FRequest then
  begin
    Exit;
  end;

  if FContext.Collections.Collection(NyxCollection('tasks')).Snapshot.Revision <> FRevision then
  begin
    FContext.State.SetValue(NyxTextState('summary'), 'Tasks changed. Calculate again.');
    Exit;
  end;

  if Length(AValues) <> 1 then
  begin
    raise ENyxQuery.Create('A task summary requires exactly one completed result');
  end;
  LSummary := AValues[0];
  FContext.State.SetValue(NyxTextState('summary'),
    'Showing ' + LSummary.Count.ToText + ' tasks / ' + LSummary.Points.ToText + ' points');
  Result := True;
end;

{ Authored button/change callbacks call this on UI. All inputs/options are
  captured before submission. No compiler directive or DOM/LCL control enters
  this portable recipe; actual scheduler capability selects native workers or
  the browser's explicit event-loop alternative. }
procedure StartTaskSummary(const AContext: INyxApplicationContext);
var
  LSource: INyxCollectionSnapshot;
  LProgram: INyxParallelProgram<TNyxCollectionItem, TNyxCollectionItem>;
  LFold: INyxParallelFold<TNyxCollectionItem, TTaskSummary>;
  LPort: INyxQueryResultPort<TTaskSummary>;
  LScheduler: INyxScheduler;
  LGeneration: INyxCancellationScope;
  LOptions: TNyxParallelOptions;
  LSeed: TTaskSummary;
  LRequest: Integer;
  LImportantOnly: Boolean;
  LRun: INyxParallelRun;
  LObservation: INyxQueryRun;
begin
  LScheduler := AContext.Scheduler;
  LScheduler.RequireUI;
  LGeneration := AContext.Generation;
  LRequest := AContext.State.GetValue(NyxIntegerState('query-request'));

  if LRequest = High(Integer) then
  begin
    raise ENyxQuery.Create('Task summary request budget exhausted');
  end;
  Inc(LRequest);
  AContext.State.SetValue(NyxIntegerState('query-request'), LRequest);
  AContext.State.SetValue(NyxTextState('summary'), 'Calculating selected tasks...');
  LImportantOnly := AContext.State.GetValue(NyxBooleanState('important-only'));
  LSource := AContext.Collections.Collection(NyxCollection('tasks')).Snapshot;
  LOptions := TNyxParallelOptions.Defaults.PartitionSize(2).Concurrency(2);

  if not (nscWorkerThreads in LScheduler.Capabilities) then
  begin
    LOptions := LOptions.Execution(npxDeferredUI);
  end;
  LProgram := TNyxParallelSource<TNyxCollectionItem>.From(NyxItems(LSource))
    .Where(TTaskPredicateFactory.Create(LImportantOnly)).WithOptions(LOptions);
  LSeed.Count := NyxQueryCount(0);
  LSeed.Points := TNyxInteger64.FromInteger(0);
  LFold := TNyxParallelFold<TNyxCollectionItem, TTaskSummary>.Seeded(LSeed,
    TTaskSummaryFolder.From(AddTask), TTaskSummaryCombiner.From(CombineTaskSummaries));
  LPort := TTaskSummaryPort.Create(AContext, LRequest, LSource.Revision);
  LRun := nil;
  try
    LRun := TNyxProgramReduction<TNyxCollectionItem, TNyxCollectionItem, TTaskSummary,
      TTaskSummary>.Submit(LProgram, LFold.Accumulators, LFold.Mergers,
        LScheduler, LPort, LGeneration);
    LObservation := TNyxQueryObservation.Observe(LRun, LScheduler,
      NyxQueryFeedback(AContext, NyxTextState('summary'), NyxIntegerState('query-request'),
        LRequest, TNyxQueryFeedbackText.Defaults),
      TNyxQueryObservationOptions.Defaults, LGeneration);
  except
    on LException: Exception do
    begin

      if LRun <> nil then
      begin
        LRun.Cancel;
      end;
      AContext.State.SetValue(NyxTextState('summary'),
        TNyxText('Could not start calculation: ') + NyxQueryDiagnostic(LException));
      raise;
    end;
  end;
end;
