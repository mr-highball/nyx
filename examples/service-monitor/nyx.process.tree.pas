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
unit nyx.process.tree;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.application.context, nyx.events, nyx.collections.view;

const
  ProcessCollectionName = 'processes';
  ProcessTreeID = 'process-tree';
  ProcessPeerID = 'process-peer';
  ProcessSearchID = 'process-search';
  ProcessFindID = 'find-process';
  ProcessNextID = 'next-process-match';
  ProcessClearSearchID = 'clear-process-search';
  ProcessRevealID = 'reveal-selected-process';
  ProcessRelaxID = 'clear-search-and-reveal';
  ProcessClearSelectionID = 'clear-process-selection';
  ProcessCountsID = 'process-counts';
  ProcessMessageID = 'process-message';

type
  { Own beside the mounted application and close before navigation/disposal.
    Refcounted event callbacks use a revocable context and weak view token;
    neither retains an application, renderer, document or widget. Search owns
    only its query contribution. Closing revokes authority without clearing it. }
  TNyxProcessTreeDemo = class
  private
    FCallback: INyxEventCallback;
    FCore: TObject; { Borrowed while FCallback retains the implementation. }
    FSubscriptions: array of INyxEventSubscription;
  public
    constructor Create(const AContext: INyxApplicationContext; const AEvents: INyxEvents;
      const AView: INyxCollectionView);
    destructor Destroy; override;
    procedure Close;
  end;

{ Portable English starter; caller owns the returned document. Runtime search,
  disclosure, focus and selection do not change the saved design or row defaults. }
function BuildProcessTreeDocument: TNyxDocument;

implementation

uses SysUtils, nyx.controls, nyx.types, nyx.state, nyx.behavior, nyx.responsive,
  nyx.scheduler, nyx.collections, nyx.collections.view.types, nyx.collections.selection,
  nyx.collections.tree.inspection, nyx.collections.query, nyx.collections.query.session,
  nyx.publication;

type
  { Router registrations retain this managed receiver; its view token is weak.
    Strong entered-call leases survive handlers which close the owning binding.
    All source/query/focus commands execute on the captured scheduler's UI owner. }
  TProcessCallback = class(TNyxEventCallback)
  private
    FContext: INyxApplicationContext;
    FScheduler: INyxScheduler;
    FGeneration: INyxCancellationScope;
    FView: INyxCollectionView;
    FSession: INyxCollectionQuerySession;
    FOwner: INyxCollectionQueryOwner;
    FToken: INyxCollectionViewSubscription;
    FClosed: Boolean;
    FPublishing: Boolean;
    function Active: Boolean;
    procedure Changed(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
    procedure Publish;
    procedure Search(AClear: Boolean);
    procedure RevealSelected;
  public
    constructor Create(const AContext: INyxApplicationContext; const AView: INyxCollectionView);
    destructor Destroy; override;
    procedure Close;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

function BuildProcessTreeDocument: TNyxDocument;
var
  LPage: INyxPage;
  LSearch: INyxInput;
  LCommands: INyxRow;
  LTree: INyxTree;
  LPeer: INyxTree;
  LSpec: TNyxCollectionViewSpec;
  LKey: TNyxCollectionRef;
  LLabels: array[0..6] of TNyxText;
  LIDs: array[0..6] of TNyxText;
  LParents: array[0..6] of TNyxText;
  LItems: array of TNyxCollectionItem;
  LIndex: Integer;
begin
  Result := TNyxDocument.Create;
  try
    LLabels[0] := 'This computer';
    LIDs[0] := 'computer';
    LParents[0] := '';
    LLabels[1] := 'Services';
    LIDs[1] := 'services';
    LParents[1] := 'computer';
    LLabels[2] := 'Request gateway';
    LIDs[2] := 'gateway';
    LParents[2] := 'services';
    LLabels[3] := 'Background workers';
    LIDs[3] := 'workers';
    LParents[3] := 'gateway';
    LLabels[4] := 'Search index worker';
    LIDs[4] := 'indexer';
    LParents[4] := 'workers';
    LLabels[5] := 'Archive worker';
    LIDs[5] := 'archive';
    LParents[5] := 'services';
    LLabels[6] := 'Desktop applications';
    LIDs[6] := 'desktop';
    LParents[6] := '';
    LKey := NyxCollection(ProcessCollectionName);
    SetLength(LItems, Length(LLabels));
    for LIndex := 0 to High(LLabels) do
    begin
      LItems[LIndex] := NyxCollectionItem(NyxItem(LKey, LIDs[LIndex]))
        .WithValue(NyxTextField('name'), LLabels[LIndex])
        .WithValue(NyxTextField('parent'), LParents[LIndex])
        .WithValue(NyxBooleanField('running'), LIDs[LIndex] <> 'archive');
    end;
    Result.Collections.Define(LKey, NyxCollectionSchema.Text(NyxTextField('name'), '')
      .Text(NyxTextField('parent'), '').Boolean(NyxBooleanField('running'), True), LItems);
    Result.State.SetValue(NyxTextState(ProcessSearchID), 'index')
      .SetValue(NyxTextState(ProcessCountsID), '')
      .SetValue(NyxTextState(ProcessMessageID), 'Find a process, then reveal a matching path.');
    LPage := NewNyxPage('process-inspection');
    Result.AddPage(LPage);
    LPage.Configure.Layout(nlColumn).Padding(16).Gap(12).Done;
    LPage.Add(NewNyxHeading('process-title').WithText('Find your place in a process tree'));
    LPage.Add(NewNyxLabel('process-introduction').WithText(
      'Search filters names. Ancestors provide context; revealing opens a path without changing selection.'));
    LSearch := NewNyxInput(ProcessSearchID).WithText('Search process names');
    LPage.Add(LSearch);
    LSearch.Binds.Value(NyxTextState(ProcessSearchID)).Done;
    LSearch.Configure.AccessibleName('Search process names').Done;
    LCommands := NewNyxRow('process-commands');
    LPage.Add(LCommands);
    LCommands.Configure.Gap(8).WhenViewport(TNyxViewportWidth.Below(560)).Layout(nlColumn).Done;
    LCommands.Add(NewNyxButton(ProcessFindID).WithText('Find'));
    LCommands.Add(NewNyxButton(ProcessNextID).WithText('Reveal next match'));
    LCommands.Add(NewNyxButton(ProcessClearSearchID).WithText('Clear my search'));
    LCommands.Add(NewNyxButton(ProcessRevealID).WithText('Reveal first selected'));
    LCommands.Add(NewNyxButton(ProcessRelaxID).WithText('Clear my search and reveal'));
    LCommands.Add(NewNyxButton(ProcessClearSelectionID).WithText('Clear selection'));
    LPage.Add(NewNyxLabel(ProcessCountsID).Binds.Text(NyxTextState(ProcessCountsID)).Done);
    LPage.Add(NewNyxLabel(ProcessMessageID).Binds.Text(NyxTextState(ProcessMessageID)).Done);
    LSpec := NyxCollectionView(LKey).Column(NyxTextField('name'), 'Process')
      .Parent(NyxTextField('parent')).Selection(nsmMultiple);
    LTree := NewNyxTree(ProcessTreeID);
    LPage.Add(LTree);
    LTree.Configure.Height(320).AccessibleName('Searchable process hierarchy').Done;
    LTree.Binds.Collection(LSpec).Done;
    LPage.Add(NewNyxLabel('process-peer-heading').WithText('Independent view of the same processes'));
    LPeer := NewNyxTree(ProcessPeerID);
    LPage.Add(LPeer);
    LPeer.Configure.Height(200).AccessibleName('Independent process hierarchy').Done;
    LPeer.Binds.Collection(LSpec).Done;
  except
    Result.Free;
    raise;
  end;
end;

constructor TProcessCallback.Create(const AContext: INyxApplicationContext;
  const AView: INyxCollectionView);
begin
  inherited Create;

  if (AContext = nil) or not AContext.Active or (AView = nil) or (AView.Projection <> cpTree) then
  begin
    raise ENyxCollection.Create('Process inspection requires its active context and Tree');
  end;
  FContext := AContext;
  FScheduler := AContext.Scheduler;
  FScheduler.RequireUI;
  FGeneration := AContext.Generation;
  FView := AView;
  FSession := NyxCollectionQuerySession(FView);
  FOwner := FSession.AcquireOwner(NyxQueryOrigin(ProcessSearchID), 'Process name search');
  FToken := FView.Subscribe(Changed);
  Publish;
end;

destructor TProcessCallback.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TProcessCallback.Active: Boolean;
begin
  Result := not FClosed and (FContext <> nil) and FContext.Active and
    (FGeneration <> nil) and not FGeneration.Cancelled;
end;

procedure TProcessCallback.Close;
begin
  FClosed := True;

  if FToken <> nil then
  begin
    FToken.Disconnect;
  end;

  if FOwner <> nil then
  begin
    FOwner.Revoke;
  end;
  FToken := nil;
  FOwner := nil;
  FSession := nil;
  FView := nil;
  FGeneration := nil;
  FContext := nil;
end;

procedure TProcessCallback.Publish;
var
  LLease: INyxEventCallback;
  LCapture: INyxTreeInspectionCapture;
  LFacts: TNyxTreeInspectionObservation;
  LText: TNyxText;
begin
  LLease := Self;

  if not Active or FPublishing then
  begin
    Exit;
  end;
  LCapture := CaptureNyxTreeInspection(FView);
  LFacts := LCapture.Observation;
  LText := TNyxText(IntToStr(LFacts.MatchedRows)) + ' matches / ' +
    TNyxText(IntToStr(LFacts.ContextRows)) + ' context ancestors / ' +
    TNyxText(IntToStr(LFacts.DisclosedRows)) + ' disclosed / ' +
    TNyxText(IntToStr(LFacts.SourceRows)) + ' source rows. ' +
    TNyxText(IntToStr(LFacts.FilteredSelectedRows)) + ' selected hidden by filters; ' +
    TNyxText(IntToStr(LFacts.CollapsedSelectedRows)) + ' selected in closed branches.';

  if Active then
  begin
    FPublishing := True;
    try
      FContext.State.SetValue(NyxTextState(ProcessCountsID), LText);
    finally
      FPublishing := False;
    end;
  end;
end;

procedure TProcessCallback.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Publish;
end;

procedure TProcessCallback.Search(AClear: Boolean);
var
  LText: TNyxText;
  LEdits: TNyxQuerySessionEdits;
  LPlan: INyxPreparedPublication;
  LTextPlan: INyxPreparedPublication;
begin
  LText := '';

  if not AClear then
  begin
    LText := FContext.State.GetValue(NyxTextState(ProcessSearchID));
  end;

  if not FOwner.Connected then
  begin
    FOwner := FSession.AcquireOwner(NyxQueryOrigin(ProcessSearchID), 'Process name search');
  end;
  LEdits := NyxQuerySessionEdits.ClearOwner;

  if LText <> '' then
  begin
    LEdits := LEdits.Filter(NyxQueryChannel('name'),
      NyxWhere(NyxTextField('name')).Contains(LText, nqtAsciiInsensitive));
  end;
  LPlan := FSession.PrepareSession(FSession.Capture, FOwner, LEdits);

  if AClear then
  begin
    { Clearing the text and this owner's predicate is one model publication.
      Defer our status writer until both prepared owners have retired. }
    LTextPlan := FContext.State.PrepareApply([NyxStateValue(NyxTextState(ProcessSearchID), '')]);
    FPublishing := True;
    try
      PublishNyxGroup([LPlan, LTextPlan]);
    finally
      FPublishing := False;
    end;
    Publish;
  end
  else
  begin
    PublishNyxGroup([LPlan]);
  end;
end;

procedure TProcessCallback.RevealSelected;
var
  LItem: TNyxItemRef;
  LMessage: TNyxText;
begin
  LMessage := 'Select a process first. Revealing never selects a different process.';

  if FView.Selection.Count > 0 then
  begin
    LItem := FView.Selection.ItemAt(0);
    case RevealNyxTreeItem(FView, LItem, ntrfTake) of
      ntrrMissing:
        LMessage := 'That process was removed. Current selection is kept.';
      ntrrFiltered:
        LMessage := 'Filters hide this selection. Clear my search and reveal is a deliberate choice; other filters remain.';
      ntrrRevealed:
        LMessage := 'The selected process path is open. Selection and other branches are kept.';
    end;
  end;

  if Active then
  begin
    FContext.State.SetValue(NyxTextState(ProcessMessageID), LMessage);
  end;
end;

procedure TProcessCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LLease: INyxEventCallback;
  LCapture: INyxTreeInspectionCapture;
  LIndex: Integer;
  LNext: Integer;
  LFocus: TNyxItemRef;
begin
  LLease := Self;

  if not Active or (AExecution = nil) or AExecution.Cancelled then
  begin
    Exit;
  end;
  FScheduler.RequireUI;

  if AEvent.OriginID = ProcessFindID then
  begin
    Search(False);
  end
  else if AEvent.OriginID = ProcessClearSearchID then
  begin
    Search(True);
  end
  else if AEvent.OriginID = ProcessRevealID then
  begin
    RevealSelected;
  end
  else if AEvent.OriginID = ProcessRelaxID then
  begin
    Search(True);

    if Active then
    begin
      RevealSelected;
    end;
  end
  else if AEvent.OriginID = ProcessClearSelectionID then
  begin
    FView.ClearSelection;
  end
  else if AEvent.OriginID = ProcessNextID then
  begin
    LCapture := CaptureNyxTreeInspection(FView);
    LFocus := FView.Selection.Focus;
    LNext := 0;
    for LIndex := 0 to LCapture.Observation.MatchedRows - 1 do
    begin

      { A first search may have no focused item. Checked identity access stays
        guarded; absent focus simply starts at the first admitted match. }

      if LFocus.Defined and (LCapture.MatchAt(LIndex).ID = LFocus.ID) then
      begin
        LNext := (LIndex + 1) mod LCapture.Observation.MatchedRows;
        Break;
      end;
    end;

    if LCapture.Observation.MatchedRows > 0 then
    begin
      RevealNyxTreeItem(FView, LCapture.MatchAt(LNext), ntrfTake);
    end;
  end;
end;

constructor TNyxProcessTreeDemo.Create(const AContext: INyxApplicationContext;
  const AEvents: INyxEvents; const AView: INyxCollectionView);
const
  CIDs: array[0..5] of TNyxText = (ProcessFindID, ProcessNextID, ProcessClearSearchID,
    ProcessRevealID, ProcessRelaxID, ProcessClearSelectionID);
var
  LCallback: TProcessCallback;
  LIndex: Integer;
begin
  inherited Create;
  LCallback := TProcessCallback.Create(AContext, AView);
  FCallback := LCallback;
  FCore := LCallback;
  SetLength(FSubscriptions, Length(CIDs));
  for LIndex := 0 to High(CIDs) do
  begin
    FSubscriptions[LIndex] := AEvents.On(NyxControlEvents(CIDs[LIndex]), ntClick)
      .Policy(neSequential).Subscribe(FCallback);
  end;
end;

destructor TNyxProcessTreeDemo.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TNyxProcessTreeDemo.Close;
var
  LIndex: Integer;
begin

  if FCallback <> nil then
  begin
    TProcessCallback(FCore).Close;
  end;
  for LIndex := 0 to High(FSubscriptions) do
  begin

    if FSubscriptions[LIndex] <> nil then
    begin
      FSubscriptions[LIndex].Cancel;
    end;
  end;
  FSubscriptions := nil;
  FCore := nil;
  FCallback := nil;
end;

end.
