{ Nyx application views. BuildNyxDocument returns an owned document. }
unit nyx.generated.view;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.dates,
  nyx.times,
  nyx.colors,
  nyx.images,
  nyx.image.lifecycle,
  nyx.design.tokens,
  nyx.types,
  nyx.responsive,
  nyx.presentations,
  nyx.content,
  nyx.containers,
  nyx.editing,
  nyx.gestures,
  nyx.contract,
  nyx.data,
  nyx.state,
  nyx.collections.view.types,
  nyx.collections.selection,
  nyx.collections.query,
  nyx.collections,
  nyx.collections.registry,
  nyx.binding.types,
  nyx.behavior,
  nyx.scheduler,
  nyx.events,
  nyx.callbacks,
  nyx.model,
  nyx.controls;

function BuildNyxDocument: TNyxDocument;

implementation

uses
  nyx.application.context,
  nyx.linq,
  nyx.linq.collections,
  nyx.linq.aggregates,
  nyx.linq.parallel,
  nyx.linq.pipeline,
  nyx.linq.reductions,
  nyx.linq.execution,
  nyx.numbers.integer64;

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
  LRun := TNyxProgramReduction<TNyxCollectionItem, TNyxCollectionItem, TTaskSummary,
    TTaskSummary>.Submit(LProgram, LFold.Accumulators, LFold.Mergers,
      LScheduler, LPort, LGeneration);
end;

// <nyx:views>
// Studio synchronizes this builder; keep application helpers outside it.
function BuildNyxDocument: TNyxDocument;
var
  LSummaryTextState: TNyxTextStateRef;
  LImportantOnlyBooleanState: TNyxBooleanStateRef;
  LQueryRequestIntegerState: TNyxIntegerStateRef;
  LTaskWorkbenchPage: INyxPage;
  LWorkbenchTitleHeading: INyxHeading;
  LWorkbenchDescriptionLabel: INyxLabel;
  LWorkbenchActionsRow: INyxRow;
  LImportantFilterCheckbox: INyxCheckbox;
  LCalculateSummaryButton: INyxButton;
  LWorkbenchSummaryLabel: INyxLabel;
  LWorkbenchTasksTable: INyxTable;
  LAboutWorkbenchPage: INyxPage;
  LAboutTitleHeading: INyxHeading;
begin
  Result := TNyxDocument.Create;
  try
    // Admit each control to its owner before configuring it.
    // A failed configuration releases the complete owned document.
    Result.Title := 'Untitled Nyx application';

    // Named references give defaults and their controls one typed contract.
    LSummaryTextState := NyxTextState('summary');
    LImportantOnlyBooleanState := NyxBooleanState('important-only');
    LQueryRequestIntegerState := NyxIntegerState('query-request');

    // Authored defaults; each application owns its runtime copy.
    Result.State
      .SetValue(LSummaryTextState, 'Choose a filter, then calculate the summary.')
      .SetValue(LImportantOnlyBooleanState, False)
      .SetValue(LQueryRequestIntegerState, 0);

    // Authored collection defaults; each application owns independent runtime stores.
    Result.Collections.Define(
      NyxCollection('tasks'),
      NyxCollectionSchema
        .Text(NyxTextField('caption'), '', NyxTextDomain)
        .Integer(NyxIntegerField('points'), 0, NyxIntegerDomain)
        .Boolean(NyxBooleanField('important'), False, NyxBooleanDomain),
      [
        NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'layout'))
          .WithValue(NyxTextField('caption'), 'Sketch the layout')
          .WithValue(NyxIntegerField('points'), 3)
          .WithValue(NyxBooleanField('important'), True),
        NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'palette'))
          .WithValue(NyxTextField('caption'), 'Choose a palette')
          .WithValue(NyxIntegerField('points'), 5),
        NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'components'))
          .WithValue(NyxTextField('caption'), 'Compose reusable pieces')
          .WithValue(NyxIntegerField('points'), 8)
          .WithValue(NyxBooleanField('important'), True),
        NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'copy'))
          .WithValue(NyxTextField('caption'), 'Write helpful copy')
          .WithValue(NyxIntegerField('points'), 2),
        NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'interaction'))
          .WithValue(NyxTextField('caption'), 'Try the interactions')
          .WithValue(NyxIntegerField('points'), 13)
          .WithValue(NyxBooleanField('important'), True),
        NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'polish'))
          .WithValue(NyxTextField('caption'), 'Polish the details')
          .WithValue(NyxIntegerField('points'), 1)
      ]);


    LTaskWorkbenchPage := NewNyxPage('task-workbench');
    Result.AddPage(LTaskWorkbenchPage);
    LTaskWorkbenchPage.Configure
      .Layout(nlColumn)
      .Gap(16)
      .Padding(24)
      .Done;

    LWorkbenchTitleHeading := NewNyxHeading('workbench-title');
    LTaskWorkbenchPage.Add(LWorkbenchTitleHeading);
    LWorkbenchTitleHeading.Configure
      .Text('A little room for big ideas')
      .Done;

    LWorkbenchDescriptionLabel := NewNyxLabel('workbench-description');
    LTaskWorkbenchPage.Add(LWorkbenchDescriptionLabel);
    LWorkbenchDescriptionLabel.Configure
      .Text('Edit task points, choose a filter, and calculate a fresh summary.')
      .Done;

    LWorkbenchActionsRow := NewNyxRow('workbench-actions');
    LTaskWorkbenchPage.Add(LWorkbenchActionsRow);
    LWorkbenchActionsRow.Configure
      .Layout(nlRow)
      .Gap(12)
      .Padding(16)
      .Done;

    LImportantFilterCheckbox := NewNyxCheckbox('important-filter');
    LWorkbenchActionsRow.Add(LImportantFilterCheckbox);
    LImportantFilterCheckbox.Configure
      .Text('Important only')
      .Done;
    LImportantFilterCheckbox.Binds
      .Value(LImportantOnlyBooleanState)
      .Done;

    LCalculateSummaryButton := NewNyxButton('calculate-summary');
    LWorkbenchActionsRow.Add(LCalculateSummaryButton);
    LCalculateSummaryButton.Configure
      .Text('Calculate summary')
      .Variant(nvPrimary)
      .Done;

    NyxCallbacks(LCalculateSummaryButton).OnClick
      .Policy(neSequential)
      .Add(NyxHandler('TCalculateSummaryButtonClick'), NyxCallbackID('TCalculateSummaryButtonClick'));

    LWorkbenchSummaryLabel := NewNyxLabel('workbench-summary');
    LTaskWorkbenchPage.Add(LWorkbenchSummaryLabel);
    LWorkbenchSummaryLabel.Configure
      .Text('Choose a filter, then calculate the summary.')
      .Done;
    LWorkbenchSummaryLabel.Binds
      .Text(LSummaryTextState)
      .Done;

    LWorkbenchTasksTable := NewNyxTable('workbench-tasks');
    LTaskWorkbenchPage.Add(LWorkbenchTasksTable);
    LWorkbenchTasksTable.Configure
      .Text('Table')
      .Items(TNyxText('Name') + TNyxText(#9) + TNyxText('Status') + TNyxText(#10) + TNyxText('Example') + TNyxText(#9) + TNyxText('Ready'))
      .Height(320)
      .AccessibleName('Task planning table')
      .Done;
    LWorkbenchTasksTable.Binds
      .Collection(
        NyxCollectionView(NyxCollection('tasks'))
          .Column(NyxTextField('caption'), 'Task')
          .Column(NyxIntegerField('points'), 'Points', cmEditable)
          .Column(NyxBooleanField('important'), 'Important'))
      .Done;

    LAboutWorkbenchPage := NewNyxPage('about-workbench');
    Result.AddPage(LAboutWorkbenchPage);
    LAboutWorkbenchPage.Configure
      .Layout(nlColumn)
      .Gap(12)
      .Padding(24)
      .Done;

    LAboutTitleHeading := NewNyxHeading('about-title');
    LAboutWorkbenchPage.Add(LAboutTitleHeading);
    LAboutTitleHeading.Configure
      .Text('Keep making things')
      .Done;
  except
    Result.Free;
    raise;
  end;
end;
// </nyx:views>


type
  { Application callback for click.
    AEvent owns its payload; use a parent-linked UI job for worker results. }
  TCalculateSummaryButtonClick = class(TNyxApplicationEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

procedure TCalculateSummaryButtonClick.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  // Capture runtime inputs on UI; the library owns worker/merger execution.
  StartTaskSummary(Context);
end;

initialization
  RegisterNyxCallback(NyxHandler('TCalculateSummaryButtonClick'), TCalculateSummaryButtonClick);

end.
