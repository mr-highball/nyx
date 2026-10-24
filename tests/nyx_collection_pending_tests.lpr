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



program nyx_collection_pending_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.collections, nyx.collections.view.types,
  nyx.collections.query, nyx.collections.query.policy, nyx.collections.query.editor,
  nyx.collections.arrangement,
  nyx.menu.editor, nyx.projection.refresh, nyx.studio.authoring,
  nyx.studio.projects, nyx.studio.session, nyx.studio.collections,
  nyx.studio.collectionintent, nyx.test.collection.queue
  {$ifdef PAS2JS}, Web{$endif};

var
  LSession: TNyxStudioSession;
  LPending: TNyxStudioPendingDesign;
  LProjection: TNyxNode;
  LRoot: TNyxNode;
  LBefore: TNyxText;
  LChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxCollection.Create('Pending collection presentation: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure PaintProposal;
begin
  LPending.Collections[0].Intent.Validate;
  LProjection := LSession.SelectedProjection;
  LRoot := TNyxNode.Create(nkColumn, 'pending-inspector');
  try
    AddNyxCollectionBindingPanel(LRoot, LSession, LProjection, LPending);
    Check((LRoot.Find('collection-column-1-title').Prop('value') = 'Task') and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'Unadmitted pending metadata cannot throw or replace the accepted typed view');
  finally
    LRoot.Free;
    LProjection.Free;
  end;
end;

{ Exercise the existing pending-presentation boundary, not a physical renderer.
  The shared seed and its original refusal cases above stay unchanged. Accepted
  policies are deliberately explicit so both setter and default proposals would
  change the baseline if the pending panel applied them optimistically. }
procedure PolicyPresentation(AAction: TNyxStudioCollectionAction;
  AInvalidDraft: Boolean);
const
  CEditor = 'inspector-collection-query';
  CColumn = 3;
var
  LAccepted, LWaiting, LRecovered, LCommitted: TNyxNode;
  LSpec, LNextSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
  LIntent: TNyxStudioCollectionIntent;
  LEdit: TNyxStudioDesignEdit;
  LDraft: TNyxQueryEditorDraft;
  LWait: TNyxStudioPendingDesign;
  LBaseline, LPair, LAcceptedStatus, LTableStatus: TNyxText;
  LComparison: TNyxQueryComparison;
  LRefused: Boolean;

  function Panel(const APending: TNyxStudioPendingDesign): TNyxNode;
  var
    LSource: TNyxNode;
  begin
    LSource := LSession.SelectedProjection;
    try
      Result := TNyxNode.Create(nkColumn, 'pending-policy-inspector');
      try
        AddNyxCollectionBindingPanel(Result, LSession, LSource, APending);
      except
        Result.Free;
        raise;
      end;
    finally
      LSource.Free;
    end;
  end;

  procedure CheckDraft(ARoot: TNyxNode);
  begin
    Check(ARoot.Find(NyxStudioQueryPolicyFieldID(-1, sqpfSortMode))
      .Prop(NyxAttributeName(atValue)) = 'Multiple keys',
      'A pending policy preserves the exact table choice draft');
    Check(ARoot.Find(NyxStudioQueryPolicyFieldID(CColumn, sqpfFamilyOperators))
      .Prop(NyxAttributeName(atValue)) = 'false',
      'An explicit operator draft does not become a family default');
    Check((ARoot.Find(NyxStudioQueryPolicyOperatorID(CColumn, nqcEqual))
      .Prop(NyxAttributeName(atValue)) = 'true') = (not AInvalidDraft),
      'Valid and invalid explicit operator drafts remain distinct');
  end;

begin
  LAccepted := nil;
  LWaiting := nil;
  LRecovered := nil;
  LCommitted := nil;
  LSchema := LSession.Document.Collections.Snapshot(NyxCollection('tasks')).Schema;
  LProjection := LSession.SelectedProjection;
  try
    LSpec := LProjection.CollectionView.ColumnQuery(
      NyxColumnQuery.Sorting(False).Operators([nqcEqual]))
      .TableQuery(NyxTableQuery.SortMode(nqsmSingle));
  finally
    FreeAndNil(LProjection);
  end;
  LSession.SetCollectionView(LSpec);
  LPair := EncodeNyxProject(LSession.ProjectSnapshot);
  LBaseline := NyxQueryEditorBaseline(LSchema, LSpec);
  LIntent := Default(TNyxStudioCollectionIntent);
  LIntent.Action := AAction;
  LIntent.Key := NyxCollection('tasks');
  LIntent.Projection := cpTable;
  LIntent.QueryBaseline := LBaseline;

  if AAction in [scaColumnQuery, scaDefaultColumnQuery] then
  begin
    LIntent.Field := TNyxStudioCollectionFieldRef.Number(NyxNumberField('effort'));
  end;

  if AAction = scaTableQuery then
  begin
    LIntent.TableQuery := NyxTableQuery.Filtering(False).SortMode(nqsmMultiple);
  end;

  if AAction = scaColumnQuery then
  begin
    LIntent.ColumnQuery := NyxColumnQuery.Sorting(True).Operators([nqcAtLeast]);
  end;
  LIntent.Validate;
  LWait := Default(TNyxStudioPendingDesign);
  SetLength(LWait.Collections, 1);
  LWait.Collections[0].Owner := 'tasks-table';
  LWait.Collections[0].Intent := LIntent;
  try
    LAccepted := Panel(Default(TNyxStudioPendingDesign));
    LAcceptedStatus := LAccepted.Find(NyxStudioQueryPolicyFormID(CColumn) + '-status').Prop('text');
    LTableStatus := LAccepted.Find(NyxStudioQueryPolicyFormID(-1) + '-status').Prop('text');
    LAccepted.Find(NyxStudioQueryPolicyFieldID(-1, sqpfSortMode))
      .Configure.Value('Multiple keys').Done;
    LAccepted.Find(NyxStudioQueryPolicyFieldID(CColumn, sqpfFamilyOperators))
      .Configure.Value(False).Done;
    for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
    begin

      if LComparison in NyxQueryFamilyComparisons(LSpec.ColumnAt(CColumn).Kind) then
      begin
        LAccepted.Find(NyxStudioQueryPolicyOperatorID(CColumn, LComparison))
          .Configure.Value((LComparison = nqcEqual) and not AInvalidDraft).Done;
      end;
    end;
    LDraft.Capture(CEditor, LAccepted);
    LWaiting := Panel(LWait);
    Check(LWaiting.Find(CEditor).Prop(NyxMenuFormBaselineKey) = LBaseline,
      'All four pending policy actions keep the accepted full binding baseline');
    Check((LWaiting.Find(NyxStudioQueryPolicyFormID(CColumn) + '-status').Prop('text') =
      LAcceptedStatus) and (LWaiting.Find(NyxStudioQueryPolicyFormID(-1) + '-status')
      .Prop('text') = LTableStatus), 'A pending default or setter cannot report admission');
    Check(LWait.CollectionViewLocked('tasks-table') and
      not LWait.CollectionViewLocked('unbound-table'),
      'Policy preparation locks only its exact authored owner');
    Check((LWaiting.Find(NyxStudioQueryPolicyApplyID(-1))
      .Prop(NyxAttributeName(atEnabled)) = 'false') and
      (LWaiting.Find(NyxStudioQueryPolicyFieldID(CColumn, sqpfFiltering))
      .Prop(NyxAttributeName(atEnabled)) = 'false'),
      'Keeping the accepted spec retains disabled table and column controls');
    Check(LDraft.Restore(LWaiting), 'Pending presentation accepts the same owned draft');
    CheckDraft(LWaiting);
    Check(CanRefreshNyxProjection(LAccepted, LWaiting),
      'Restored pending presentation stays eligible for scalar projection refresh');
    LRefused := False;
    try
      CaptureNyxStudioQuery(LSession, LWaiting.Find(NyxStudioQueryPolicyApplyID(CColumn)),
        LWaiting, LWait, LEdit);
    except
      on LError: ENyxCollection do
      begin
        LRefused := Pos('pending edits', LError.Message) > 0;
      end;
    end;
    Check(LRefused, 'Pending capture refuses at the owner lock before draft admission');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LPair,
      'Pending policy paint and capture leave the paired project unchanged');

    { Removing pending metadata models the shared presentation recovery seam.
      It does not claim an actual SourceCommands.Cancel execution. The same
      baseline keeps even an invalid draft available for the user to correct. }
    LDraft.Capture(CEditor, LWaiting);
    LRecovered := Panel(Default(TNyxStudioPendingDesign));
    Check(LDraft.Restore(LRecovered), 'Unlocking restores the same accepted draft');
    CheckDraft(LRecovered);
    Check(LRecovered.Find(NyxStudioQueryPolicyApplyID(CColumn))
      .Prop(NyxAttributeName(atEnabled)) = 'true', 'Policy actions re-enable after pending retirement');
    LRefused := False;
    try
      Check(CaptureNyxStudioQuery(LSession,
        LRecovered.Find(NyxStudioQueryPolicyApplyID(CColumn)), LRecovered,
        Default(TNyxStudioPendingDesign), LEdit), 'Recovered form routes its ordinary typed action');
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
      on LError: EArgumentException do
      begin
        { The immutable policy rejects an enabled, explicitly empty operator
          set before it can become a collection proposal. Preserve this exact
          refusal rather than accepting any unrelated argument failure. }

        if LError.Message <> 'An enabled column filter requires an allowed comparison' then
        begin
          raise;
        end;
        LRefused := True;
      end;
    end;
    Check(LRefused = AInvalidDraft,
      'Recovery admits valid choices and still refuses an invalid explicit-empty operator draft');

    if not AInvalidDraft then
    begin
      Check((LEdit.Action = sdaCollection) and
        (LEdit.Collection.Action = scaColumnQuery) and
        (LEdit.Collection.Field.Name = 'effort') and
        (LEdit.Collection.ColumnQuery.ComparisonCount = 1),
        'Recovered valid input produces the exact typed column-policy proposal');
    end;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LPair,
      'Recovered draft capture is not a paired design mutation');

    LSession.ApplyCollectionIntent(LIntent);
    LProjection := LSession.SelectedProjection;
    try
      LNextSpec := LProjection.CollectionView;
    finally
      FreeAndNil(LProjection);
    end;
    Check((EncodeNyxProject(LSession.ProjectSnapshot) <> LPair) and
      (NyxQueryEditorBaseline(LSchema, LNextSpec) <> LBaseline),
      'Normal typed admission changes the pair and baseline after preparation');
    case AAction of
      scaTableQuery:
        Check(LNextSpec.HasTableQueryPolicy and not LNextSpec.TableQueryPolicy.FilterEnabled,
          'Table setter publishes its declared choice normally');
      scaColumnQuery:
        Check(LNextSpec.ColumnAt(CColumn).HasQueryPolicy and
          LNextSpec.ColumnAt(CColumn).QueryPolicy.SortEnabled,
          'Column setter publishes its declared choice normally');
      scaDefaultTableQuery:
        Check(not LNextSpec.HasTableQueryPolicy, 'Table default removes the raw declaration');
      scaDefaultColumnQuery:
        Check(not LNextSpec.ColumnAt(CColumn).HasQueryPolicy,
          'Column default removes the raw declaration');
    else
      Check(False, 'Pending policy regression requires one of its four typed actions');
    end;
    LCommitted := Panel(Default(TNyxStudioPendingDesign));
    Check(not LDraft.Restore(LCommitted),
      'Committed policy changes retire the old accepted-baseline draft');
  finally
    LCommitted.Free;
    LRecovered.Free;
    LWaiting.Free;
    LAccepted.Free;
  end;
end;

{ A prepared layout policy locks its accepted owner without previewing an
  unaccepted declaration into the inspector. The generic form draft holds raw
  numeric text and resumes only against the exact accepted schema/binding. }
procedure LayoutPresentation(AAction: TNyxStudioCollectionAction; AInvalidDraft: Boolean);
const
  CColumn = 3;
var
  LAccepted: TNyxNode;
  LWaiting: TNyxNode;
  LRecovered: TNyxNode;
  LCommitted: TNyxNode;
  LSource: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LNextSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
  LIntent: TNyxStudioCollectionIntent;
  LEdit: TNyxStudioDesignEdit;
  LWait: TNyxStudioPendingDesign;
  LDraft: TNyxMenuEditorDraft;
  LPair: TNyxText;
  LBaseline: TNyxText;
  LStatus: TNyxText;
  LValue: TNyxText;
  LRefused: Boolean;

  function Panel(const APending: TNyxStudioPendingDesign): TNyxNode;
  var
    LProjection: TNyxNode;
  begin
    LProjection := LSession.SelectedProjection;
    Result := TNyxNode.Create(nkColumn, 'pending-layout-inspector');
    try
      try
        AddNyxCollectionBindingPanel(Result, LSession, LProjection, APending);
      except
        Result.Free;
        raise;
      end;
    finally
      LProjection.Free;
    end;
  end;

begin
  LAccepted := nil;
  LWaiting := nil;
  LRecovered := nil;
  LCommitted := nil;
  LSchema := LSession.Document.Collections.Snapshot(NyxCollection('tasks')).Schema;
  LSource := LSession.SelectedProjection;
  try
    LSpec := LSource.CollectionView.TableLayout(NyxTableLayout.Enabled(False))
      .ColumnLayout(NyxColumnLayout.Visible(False).PinStart);
  finally
    LSource.Free;
  end;
  LSession.SetCollectionView(LSpec);
  LPair := EncodeNyxProject(LSession.ProjectSnapshot);
  LBaseline := NyxQueryEditorBaseline(LSchema, LSpec);
  LIntent := Default(TNyxStudioCollectionIntent);
  LIntent.Action := AAction;
  LIntent.Key := NyxCollection('tasks');
  LIntent.Projection := cpTable;
  LIntent.LayoutBaseline := LBaseline;

  if AAction in [scaColumnLayout, scaDefaultColumnLayout] then
  begin
    LIntent.Field := TNyxStudioCollectionFieldRef.Number(NyxNumberField('effort'));
  end;

  if AAction = scaTableLayout then
  begin
    LIntent.TableLayout := NyxTableLayout.Enabled(True).MinimumCenter(112);
  end;

  if AAction = scaColumnLayout then
  begin
    LIntent.ColumnLayout := NyxColumnLayout.Visible(True).PinEnd;
  end;
  LIntent.Validate;
  LWait := Default(TNyxStudioPendingDesign);
  SetLength(LWait.Collections, 1);
  LWait.Collections[0].Owner := 'tasks-table';
  LWait.Collections[0].Intent := LIntent;
  try
    LAccepted := Panel(Default(TNyxStudioPendingDesign));
    LStatus := LAccepted.Find(NyxStudioLayoutPolicyFormID(-1) + '-status').Prop('text');
    LValue := '128';

    if AInvalidDraft then
    begin
      LValue := '-';
    end;
    LAccepted.Find(NyxStudioLayoutPolicyFieldID(-1, slpfMinimumCenter))
      .Configure.Value(LValue).Done;
    LDraft.Capture(NyxStudioLayoutEditorID, LAccepted);
    LWaiting := Panel(LWait);
    Check((LWaiting.Find(NyxStudioLayoutEditorID).Prop(NyxMenuFormBaselineKey) = LBaseline) and
      (LWaiting.Find(NyxStudioLayoutPolicyFormID(-1) + '-status').Prop('text') = LStatus),
      'all four pending layout actions keep the accepted baseline and raw-declaration status');
    Check(LWait.CollectionViewLocked('tasks-table') and not LWait.CollectionViewLocked('unbound-table'),
      'pending layout admission locks only the original authored owner');
    Check((LWaiting.Find(NyxStudioLayoutPolicyApplyID(-1)).Prop('enabled') = 'false') and
      (LWaiting.Find(NyxStudioLayoutPolicyFieldID(CColumn, slpfPin)).Prop('enabled') = 'false'),
      'pending layout locks both table and independent column forms');
    Check(LDraft.Restore(LWaiting) and
      (LWaiting.Find(NyxStudioLayoutPolicyFieldID(-1, slpfMinimumCenter)).Prop('value') = LValue),
      'pending presentation preserves exact valid or incomplete numeric layout text');
    Check(CanRefreshNyxProjection(LAccepted, LWaiting),
      'restored pending layout retains scalar projection-refresh compatibility');
    LRefused := False;
    try
      CaptureNyxStudioLayout(LSession, LWaiting.Find(NyxStudioLayoutPolicyApplyID(-1)),
        LWaiting, LWait, LEdit);
    except
      on LError: ENyxCollection do
      begin
        LRefused := Pos('pending edits', LError.Message) > 0;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LPair),
      'pending layout capture refuses before draft admission and leaves the paired project unchanged');
    LDraft.Capture(NyxStudioLayoutEditorID, LWaiting);
    LRecovered := Panel(Default(TNyxStudioPendingDesign));
    Check(LDraft.Restore(LRecovered) and
      (LRecovered.Find(NyxStudioLayoutPolicyFieldID(-1, slpfMinimumCenter)).Prop('value') = LValue),
      'retiring pending metadata restores the same accepted raw layout draft');
    LRefused := False;
    try
      Check(CaptureNyxStudioLayout(LSession, LRecovered.Find(NyxStudioLayoutPolicyApplyID(-1)),
        LRecovered, Default(TNyxStudioPendingDesign), LEdit),
        'recovered layout form routes an ordinary typed policy edit');
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
      on EArgumentException do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused = AInvalidDraft,
      'layout recovery admits complete numbers while retaining invalid text for correction');

    if not AInvalidDraft then
    begin
      Check((LEdit.Collection.Action = scaTableLayout) and
        (LEdit.Collection.TableLayout.MinimumScrollableWidth = 128),
        'recovered layout draft produces the exact complete typed numeric policy');
    end;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LPair,
      'capturing a recovered layout proposal is not a paired mutation');
    LSession.ApplyCollectionIntent(LIntent);
    LSource := LSession.SelectedProjection;
    try
      LNextSpec := LSource.CollectionView;
    finally
      LSource.Free;
    end;
    Check((NyxQueryEditorBaseline(LSchema, LNextSpec) <> LBaseline) and
      (EncodeNyxProject(LSession.ProjectSnapshot) <> LPair),
      'ordinary layout intent admission changes the original pair and exact baseline');
    case AAction of
      scaTableLayout:
        Check(LNextSpec.HasTableLayoutPolicy and LNextSpec.TableLayoutPolicy.IsEnabled,
          'table layout setter publishes its complete declared choices');
      scaColumnLayout:
        Check(LNextSpec.ColumnAt(CColumn).HasLayoutPolicy and
          (LNextSpec.ColumnAt(CColumn).LayoutPolicy.Pin = ncpEnd),
          'column layout setter retains its exact scalar field and logical pin');
      scaDefaultTableLayout:
        Check(not LNextSpec.HasTableLayoutPolicy, 'table layout default removes only raw policy');
      scaDefaultColumnLayout:
        Check(not LNextSpec.ColumnAt(CColumn).HasLayoutPolicy,
          'column layout default removes only the chosen raw policy');
    else
      Check(False, 'pending layout regression requires one of its four typed actions');
    end;
    LCommitted := Panel(Default(TNyxStudioPendingDesign));
    Check(not LDraft.Restore(LCommitted), 'changed accepted layout retires an older inspector draft');
  finally
    LCommitted.Free;
    LRecovered.Free;
    LWaiting.Free;
    LAccepted.Free;
  end;
end;

begin
  LSession := nil;
  try
    LSession := TNyxStudioSession.Create(CreateNyxCollectionQueueSeed);
    LSession.Select('tasks-table');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LPending := Default(TNyxStudioPendingDesign);
    SetLength(LPending.Collections, 1);
    LPending.Collections[0].Owner := 'tasks-table';
    LPending.Collections[0].Intent.Action := scaTitle;
    LPending.Collections[0].Intent.Key := NyxCollection('tasks');
    LPending.Collections[0].Intent.Field :=
      TNyxStudioCollectionFieldRef.Boolean(NyxBooleanField('caption'));
    LPending.Collections[0].Intent.Projection := cpTable;
    LPending.Collections[0].Intent.Value := 'Unadmitted title';
    PaintProposal;
    LPending.Collections[0].Intent.Field :=
      TNyxStudioCollectionFieldRef.Text(NyxTextField('absent'));
    PaintProposal;
    LPending.Collections[0].Intent := Default(TNyxStudioCollectionIntent);
    LPending.Collections[0].Intent.Action := scaBind;
    LPending.Collections[0].Intent.Key := NyxCollection('absent');
    LPending.Collections[0].Intent.Projection := cpTable;
    PaintProposal;
    LPending.Collections[0].Owner := 'another-owner';
    PaintProposal;
    PolicyPresentation(scaTableQuery, False);
    PolicyPresentation(scaTableQuery, True);
    PolicyPresentation(scaColumnQuery, False);
    PolicyPresentation(scaColumnQuery, True);
    PolicyPresentation(scaDefaultTableQuery, False);
    PolicyPresentation(scaDefaultTableQuery, True);
    PolicyPresentation(scaDefaultColumnQuery, False);
    PolicyPresentation(scaDefaultColumnQuery, True);
    LayoutPresentation(scaTableLayout, False);
    LayoutPresentation(scaTableLayout, True);
    LayoutPresentation(scaColumnLayout, False);
    LayoutPresentation(scaColumnLayout, True);
    LayoutPresentation(scaDefaultTableLayout, False);
    LayoutPresentation(scaDefaultTableLayout, True);
    LayoutPresentation(scaDefaultColumnLayout, False);
    LayoutPresentation(scaDefaultColumnLayout, True);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-collection-pending', 'passed');
    document.body.setAttribute('data-collection-checks', IntToStr(LChecks));
    {$else}
    WriteLn('PASS ', LChecks, ' pending collection refusal/presentation checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-collection-pending', 'failed');
      document.body.setAttribute('data-collection-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  LSession.Free;
end.
