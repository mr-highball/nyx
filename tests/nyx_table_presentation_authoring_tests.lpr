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

program nyx_table_presentation_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.contract,
  nyx.codec, nyx.codegen, nyx.data, nyx.collections, nyx.collections.columns,
  nyx.collections.paging, nyx.collections.view.types, nyx.collections.query.editor,
  nyx.collections.workbench.policy, nyx.collections.presentation, nyx.collections.alignment,
  nyx.collections.arrangement, nyx.collections.query.policy,
  nyx.collections.inspection, nyx.collections.recovery.policy,
  nyx.studio.projects, nyx.studio.session,
  nyx.studio.authoring, nyx.studio.collectionintent, nyx.studio.collectionedits,
  nyx.studio.collections, nyx.studio.stateedits, nyx.studio.agents
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Presentation authoring: ' + AReason);
  end;
  Inc(GChecks);
end;

function TestPolicy: TNyxTableWorkbenchPolicy;
begin
  Result := NyxTableWorkbench.RangeSelection(True)
    .AllowCopy([nwcaSelectedSource, nwcaSelectedPage, nwcaCellRange])
    .CopyFields(nwcfAllArrangement).Headers(False).LineEnding(nwleCRLF).FocusedDraft(True);
end;

{ One independent accepted project supplies ordinary controls and a reusable
  table part. It includes handwritten source and exact Unicode/NUL cell text;
  neither a live Studio session nor clipboard authority belongs to this seed. }
function Seed: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LRecipe: INyxColumn;
  LInstance: INyxComponent;
  LTable: INyxTable;
  LList: INyxList;
  LKey: TNyxCollectionRef;
  LSpec: TNyxCollectionViewSpec;
begin
  LDocument := TNyxDocument.Create;
  try
    LKey := NyxCollection('inspected-records');
    LDocument.Collections.Define(LKey,
      NyxCollectionSchema.Text(NyxTextField('Caption'), '').Integer(NyxIntegerField('Count'), 0),
      [NyxCollectionItem(NyxItem(LKey, 'first')).WithValue(NyxTextField('Caption'),
        TNyxText('Original 🌙') + #0 + ' text').WithValue(NyxIntegerField('Count'), 7)]);
    LSpec := NyxCollectionView(LKey).Column(NyxTextField('Caption'), 'Caption', cmEditable)
      .ColumnSizing(NyxColumnSizing.Fixed(160)).Column(NyxIntegerField('Count'), 'Count')
      .Paged(NyxCollectionPaging.PageSize(2));
    LPage := NewNyxPage('home');
    LTable := NewNyxTable('records-table');
    LTable.Node.SetCollectionView(LSpec);
    LPage.Add(LTable);
    LList := NewNyxList('records-list');
    LList.Node.SetCollectionView(NyxCollectionView(LKey).Column(NyxTextField('Caption'), 'Caption'));
    LPage.Add(LList);
    LRecipe := NewNyxColumn('record-panel');
    LTable := NewNyxTable('definition-table');
    LTable.Configure.PartName(NyxPart('records')).Done;
    LTable.Node.SetCollectionView(LSpec.TableWorkbench(TestPolicy).TablePresentation(NyxTablePresentation.Density(ntrdComfortable)).ColumnText(NyxColumnText.Ellipsis));
    LRecipe.Add(LTable);
    LDocument.AddComponent(LRecipe);
    LInstance := NewNyxComponent('first-panel');
    LInstance.Configure.Component(NyxComponent('record-panel')).Done;
    LInstance.OverridePart(NyxPart('records'), noProperties).Named('first-records');
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('second-panel');
    LInstance.Configure.Component(NyxComponent('record-panel')).Done;
    LPage.Add(LInstance);
    LDocument.AddPage(LPage);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxText('{ Handwritten companion 🌙 }') + #10 + TNyxCodegen.Generate(LDocument));
  finally
    LInstance := nil;
    LList := nil;
    LTable := nil;
    LRecipe := nil;
    LPage := nil;
    LDocument.Free;
  end;
end;

{ Ordinary Inspector capture is deliberately separate from physical host input.
  These assertions qualify exact intent, data/source pairing and history; the
  mounted Studio journey remains responsible for real focus and publication. }
procedure OrdinaryAuthoring;
var
  LSession: TNyxStudioSession;
  LShell: TNyxNode;
  LProjection: TNyxNode;
  LEdit: TNyxStudioDesignEdit;
  LIntent: TNyxStudioCollectionIntent;
  LSpec: TNyxCollectionViewSpec;
  LBefore: TNyxText;
  LApplied: TNyxText;
  LRows: TNyxText;
  LRefused: Boolean;

  procedure Rebuild;
  begin
    FreeAndNil(LShell);
    LShell := TNyxNode.Create(nkPage, 'presentation-inspector');
    LProjection := LSession.SelectedProjection;
    try
      AddNyxCollectionBindingPanel(LShell, LSession, LProjection);
    finally
      FreeAndNil(LProjection);
    end;
  end;

  function Capture(AColumn: Integer; AReset: Boolean = False): Boolean;
  var
    LID: TNyxText;
  begin
    LID := NyxStudioPresentationPolicyApplyID(AColumn);

    if AReset then
    begin
      LID := NyxStudioPresentationPolicyDefaultID(AColumn);
    end;
    Result := CaptureNyxStudioLayout(LSession, LShell.Find(LID), LShell,
      Default(TNyxStudioPendingDesign), LEdit);
  end;

  procedure Value(AColumn: Integer; AField: TNyxStudioPresentationPolicyField;
    const AValue: TNyxText);
  begin
    LShell.Find(NyxStudioPresentationPolicyFieldID(AColumn, AField)).Configure.Value(AValue).Done;
  end;

  procedure ApplyCaptured;
  begin
    LIntent := TNyxStudioCollectionIntent.FromData(LEdit.Collection.ToData);
    Check(LIntent.SameIntent(LEdit.Collection), 'private wire retains typed policy/reset and baseline');
    LSession.ApplyCollectionIntent(LIntent);
  end;

begin
  LSession := TNyxStudioSession.Create(Seed);
  LShell := nil;
  LProjection := nil;
  try
    LSession.Select('records-table');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LRows := TNyxDataValue.ParseJSON(TNyxCodec.Encode(LSession.Document)).Field('collections').ToJSON;
    Rebuild;
    Check((LShell.Find(NyxStudioPresentationPolicyFormID(-1)).Count = 9) and
      (LShell.Find(NyxStudioPresentationPolicyFormID(0)).Count = 6),
      'bound tables expose complete independent spacing and per-column text forms');
    Check(not LSession.Document.Find('records-table').CollectionView.HasPresentationPolicy,
      'opening forms preserves raw absence and legacy target behavior');
    Value(-1, sppfDensity, 'Comfortable');
    LShell.Find(NyxStudioPresentationPolicyFieldID(-1, sppfHeaderEnabled))
      .Configure.Value(True).Done;
    Value(-1, sppfHeaderLines, '2');
    Value(-1, sppfHeaderMinimumHeight, '41');
    Check(Capture(-1) and (LEdit.Collection.Action = scaTablePresentation) and
      (LEdit.Collection.TablePresentation.RowDensity = ntrdComfortable) and
      LEdit.Collection.TablePresentation.HasHeader and
      (LEdit.Collection.TablePresentation.HeaderPolicy.ReservedLines = 2) and
      (LEdit.Collection.TablePresentation.HeaderPolicy.MinimumLogicalHeight = 41) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'spacing capture returns a typed proposal without publishing');
    ApplyCaptured;
    Rebuild;
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LShell.Find(NyxStudioPresentationPolicyFieldID(-1, sppfHeaderEnabled))
      .Configure.Value(False).Done;
    Value(-1, sppfHeaderLines, '-');
    Value(-1, sppfHeaderMinimumHeight, 'unfinished');
    Check(Capture(-1) and not LEdit.Collection.TablePresentation.HasHeader and
      (LEdit.Collection.TablePresentation.RowDensity = ntrdComfortable) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'inactive header drafts cannot block a typed reset proposal or mutate the pair');
    Rebuild;
    Value(0, sppfTextOverflow, 'Wrap');
    Value(0, sppfMaximumLines, '-');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LRefused := False;
    try
      Capture(0);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore) and
      (LShell.Find(NyxStudioPresentationPolicyFieldID(0, sppfMaximumLines))
        .Prop(NyxAttributeName(atValue)) = '-'),
      'unfinished wrap count stays visible and refuses atomically');
    Value(0, sppfMaximumLines, '2000000000');
    Check(Capture(0) and (LEdit.Collection.ColumnText.MaximumLines = 2000000000),
      'authoring accepts positive typed counts independently of runtime measurement limits');
    ApplyCaptured;
    LApplied := EncodeNyxProject(LSession.ProjectSnapshot);
    LSpec := LSession.Document.Find('records-table').CollectionView;
    Check((LSpec.TablePresentationPolicy.RowDensity = ntrdComfortable) and
      (LSpec.ColumnAt(0).TextPolicy.Overflow = nctoWrap) and not LSpec.ColumnAt(1).HasTextPolicy and
      (Pos('.ColumnText(NyxColumnText.Wrap(2000000000))', LSession.Source) > 0) and
      (Pos('{ Handwritten companion 🌙 }', LSession.Source) > 0),
      'typed Pascal and saved choices preserve independent columns and handwritten source');
    Check(TNyxDataValue.ParseJSON(TNyxCodec.Encode(LSession.Document)).Field('collections').ToJSON = LRows,
      'presentation never rewrites exact Unicode/NUL data');
    LRefused := False;
    try
      Capture(0);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LApplied),
      'an earlier form cannot replace a newer binding');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore, 'Undo restores the exact source/design pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LApplied, 'Redo restores the exact presentation pair');
    Rebuild;
    Value(0, sppfMaximumLines, '-');
    Check(Capture(0, True), 'reset ignores unfinished inactive parameters');
    ApplyCaptured;
    LSpec := LSession.Document.Find('records-table').CollectionView;
    Check(not LSpec.ColumnAt(0).HasTextPolicy and LSpec.HasTablePresentationPolicy and
      (LSpec.PagingPolicy.ItemsPerPage = 2) and (LSpec.ColumnAt(0).Sizing.Value = 160),
      'column reset retains independent density, paging and geometry');
    Rebuild;
    Value(0, sppfTextOverflow, 'Ellipsis');
    Value(0, sppfMaximumLines, '-');
    Check(Capture(0) and (LEdit.Collection.ColumnText.MaximumLines = 1),
      'single-line mode ignores inactive wrap input');
    ApplyCaptured;
    Rebuild;
    Check(Capture(-1, True), 'table spacing reset has its own ordinary intent');
    ApplyCaptured;
    LSpec := LSession.Document.Find('records-table').CollectionView;
    Check(not LSpec.HasTablePresentationPolicy and
      (LSpec.ColumnAt(0).TextPolicy.Overflow = nctoEllipsis),
      'table spacing reset preserves independently authored column text');
    LSession.Undo;
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaTitle;
    LIntent.Key := NyxCollection('inspected-records');
    LIntent.Projection := cpTable;
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('Caption'));
    LIntent.Value := 'Record 🌙';
    LSession.ApplyCollectionIntent(LIntent);
    LSpec := LSession.Document.Find('records-table').CollectionView;
    Check(LSpec.HasTablePresentationPolicy and
      (LSpec.ColumnAt(0).TextPolicy.Overflow = nctoEllipsis),
      'ordinary column reconstruction preserves every presentation declaration');
    Rebuild;
    LShell.Find(NyxStudioPresentationPolicyFormID(0)).SetProp('collection-kind', 'integer');
    LRefused := False;
    try
      Capture(0, True);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'column form positions cannot replace full family-qualified identity');
    Rebuild;
    LSession.SetSourceDraft(LSession.Source + #10 + '{ unfinished companion }');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LRefused := False;
    try
      Capture(-1, True);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'pending Pascal refuses presentation admission without discarding the draft');
    LSession.Select('records-list');
    Rebuild;
    Check(LShell.Find(NyxStudioPresentationPolicyFormID(-1)) = nil,
      'lists do not expose table-only presentation forms');
  finally
    LProjection.Free;
    LShell.Free;
    LSession.Free;
  end;
end;

{ Semantic operations use actual agent-session admission, revision handling and
  paired history. Bounded queries never materialize a whole design document. }
procedure SemanticAuthoring;
var
  LAgent: TNyxAgentSession;
  LRevision: Integer;
  LKey: TNyxCollectionRef;
  LBefore: TNyxText;
  LApplied: TNyxText;
  LPlain: TNyxDataValue;
  LReply: TNyxDataValue;
  LRequest: TNyxDataValue;
  LIntent: TNyxStudioCollectionIntent;
  LRefused: Boolean;

  function Pair: TNyxText;
  begin
    Result := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
  end;

  function ReadBinding(const AOwner: TNyxText; AInclude: Boolean): TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_collections', 'Presentation test', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData(AOwner)),
      NyxField('presentationControls', NyxData(AInclude)), NyxField('limit', NyxData(1))]));
  end;

  procedure Apply(const AID: TNyxText; const AChanges: array of TNyxCollectionChange);
  begin
    LRequest := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData(AID)),
      NyxField('changes', NyxCollectionPatch(AChanges).ToData)]);
    LAgent.Call('nyx_collections', 'Presentation test', LRequest);
    LRevision := LAgent.Revision;
  end;

  procedure History(const ADirection: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Presentation test', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('presentation-' + ADirection)),
      NyxField('direction', NyxData(ADirection))]));
    LRevision := LAgent.Revision;
  end;

begin
  LAgent := TNyxAgentSession.Create(Seed);
  try
    LRevision := LAgent.Revision;
    LKey := NyxCollection('inspected-records');
    LBefore := Pair;
    LPlain := LAgent.Call('nyx_collections', 'Presentation test', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('records-table')),
      NyxField('limit', NyxData(1))]));
    Check((LPlain.ToJSON = ReadBinding('records-table', False).ToJSON) and
      not NyxAgentHas(LPlain, 'presentationControlsSupported'),
      'omitted and false selectors preserve the exact previous response');
    LReply := ReadBinding('records-table', True);
    Check(LReply.Field('presentationControlsSupported').AsBoolean and
      not LReply.Field('local').Field('presentationControls').Field('declared').AsBoolean and
      (LReply.Field('local').Field('presentationControls').Field('policy').Kind = ndNull) and
      (LReply.Field('effective').Field('columns').Count = 1) and
      (LReply.Field('effective').Field('total').AsInteger = 2),
      'one column page reports raw absence without fabricating legacy target metrics');
    Check((Pair = LBefore) and (LAgent.Revision = LRevision), 'bounded reads publish no defaults');
    LRefused := False;
    try
      NyxUseDefaultCollectionColumnText(NyxBindingOwner('records-table'), LKey, cpTable,
        Default(TNyxStudioCollectionFieldRef));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pair = LBefore), 'undefined column identity refuses at public construction');
    LRefused := False;
    try
      NyxSetCollectionTablePresentation(NyxBindingOwner('records-table'), LKey, cpTable,
        Default(TNyxTablePresentationPolicy));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pair = LBefore), 'an undefined spacing setter cannot serialize as a reset');
    LRefused := False;
    try
      NyxSetCollectionColumnText(NyxBindingOwner('records-table'), LKey, cpTable,
        TNyxStudioCollectionFieldRef.Text(NyxTextField('Caption')), Default(TNyxColumnTextPolicy));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pair = LBefore), 'an undefined text setter cannot serialize as a reset');
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaTitle;
    LIntent.Key := LKey;
    LIntent.Projection := cpTable;
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('Caption'));
    LIntent.Value := 'Grouped 🌙';
    Apply('presentation-group', [NyxCollectionIntentChange(NyxBindingOwner('records-table'), LIntent),
      NyxSetCollectionTablePresentation(NyxBindingOwner('records-table'), LKey, cpTable,
        NyxTablePresentation.Density(ntrdCompact).Header(NyxTableHeader.MinimumHeight(32))),
      NyxSetCollectionColumnText(NyxBindingOwner('records-table'), LKey, cpTable,
        LIntent.Field, NyxColumnText.Wrap(4))]);
    LApplied := Pair;
    LReply := ReadBinding('records-table', True);
    Check((LReply.Field('local').Field('presentationControls').Field('policy').ToJSON =
      NyxTablePresentation.Density(ntrdCompact).Header(NyxTableHeader.MinimumHeight(32)).ToData.ToJSON) and
      (LReply.Field('local').Field('columns').Item(0).Field('presentationControls')
        .Field('policy').ToJSON = NyxColumnText.Wrap(4).ToData.ToJSON),
      'ordered semantic group derives the baseline after each preceding change');
    LAgent.Call('nyx_collections', 'Presentation test', LRequest);
    Check((LAgent.Revision = LRevision) and (Pair = LApplied), 'exact operation retry is idempotent');
    LReply := LAgent.Call('nyx_collections', 'Presentation test', NyxObject([
      NyxField('mode', NyxData('column')), NyxField('owner', NyxData('records-table')),
      NyxField('field', NyxData('Caption')), NyxField('source', NyxData('effective')),
      NyxField('presentationControls', NyxData(True)), NyxField('count', NyxData(1))]));
    Check(LReply.Field('presentationControls').Field('declared').AsBoolean and
      (LReply.Field('presentationControls').Field('policy').ToJSON = NyxColumnText.Wrap(4).ToData.ToJSON),
      'exact column reads expose only the requested bounded policy');
    History('undo');
    Check(Pair = LBefore, 'one Undo restores all three grouped changes and exact Pascal');
    History('redo');
    Check(Pair = LApplied, 'one Redo restores the complete authored group');
    LBefore := Pair;
    LRefused := False;
    try
      Apply('presentation-invalid-group', [
        NyxUseDefaultCollectionTablePresentation(NyxBindingOwner('records-table'), LKey, cpTable),
        NyxSetCollectionColumnText(NyxBindingOwner('records-table'), LKey, cpTable,
          TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('Caption')), NyxColumnText.Clip)]);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pair = LBefore) and (LAgent.Revision = LRevision),
      'wrong-family final edit refuses the whole group without partial density reset');
    LReply := ReadBinding('first-records', True);
    Check(LReply.Field('inherited').AsBoolean and (LReply.Field('local').Kind = ndNull),
      'reusable metadata stays inherited without materializing a local binding');
    Apply('presentation-local-reset', [NyxUseDefaultCollectionTablePresentation(
      NyxBindingOwner('first-records'), LKey, cpTable)]);
    LReply := ReadBinding('first-records', True);
    Check(not LReply.Field('inherited').AsBoolean and
      not LReply.Field('local').Field('presentationControls').Field('declared').AsBoolean and
      not LReply.Field('local').Field('columns').Item(0).Field('presentationControls').Field('declared').AsBoolean and
      ReadBinding('definition-table', True).Field('local').Field('presentationControls').Field('declared').AsBoolean,
      'local density reset preserves reusable definition and first-column absence');
    LReply := LAgent.Call('nyx_collections', 'Presentation test', NyxObject([
      NyxField('mode', NyxData('column')), NyxField('owner', NyxData('first-records')),
      NyxField('field', NyxData('Count')), NyxField('source', NyxData('effective')),
      NyxField('presentationControls', NyxData(True)), NyxField('count', NyxData(1))]));
    Check(LReply.Field('presentationControls').Field('declared').AsBoolean and
      (LReply.Field('presentationControls').Field('policy').ToJSON = NyxColumnText.Ellipsis.ToData.ToJSON),
      'local density reset preserves the distinct second-column text declaration');
    LBefore := Pair;
    LRefused := False;
    try
      LAgent.Call('nyx_collections', 'Presentation test', NyxObject([
        NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('records-table')),
        NyxField('presentationControls', NyxData('true'))]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pair = LBefore), 'selectors refuse string Booleans without state changes');
  finally
    LAgent.Free;
  end;
end;

{ Ordinary form capture and actual semantic admission share the same logical
  policy. These checks keep their source/history/currentness proofs distinct
  from the separate mounted browser/LCL paint and editor journeys. }
procedure AlignmentAuthoring;
var
  LSession: TNyxStudioSession;
  LShell: TNyxNode;
  LProjection: TNyxNode;
  LEdit: TNyxStudioDesignEdit;
  LIntent: TNyxStudioCollectionIntent;
  LBefore: TNyxText;
  LApplied: TNyxText;
  LRefused: Boolean;
  LPolicy: TNyxColumnAlignmentPolicy;

  procedure Rebuild;
  begin
    FreeAndNil(LShell);
    LShell := TNyxNode.Create(nkPage, 'alignment-inspector');
    LProjection := LSession.SelectedProjection;
    try
      AddNyxCollectionBindingPanel(LShell, LSession, LProjection);
    finally
      FreeAndNil(LProjection);
    end;
  end;

  function Capture(AReset: Boolean = False): Boolean;
  var
    LID: TNyxText;
  begin
    LID := NyxStudioColumnAlignmentApplyID(0);

    if AReset then
    begin
      LID := NyxStudioColumnAlignmentDefaultID(0);
    end;
    Result := CaptureNyxStudioLayout(LSession, LShell.Find(LID), LShell,
      Default(TNyxStudioPendingDesign), LEdit);
  end;

begin
  LSession := TNyxStudioSession.Create(Seed);
  LShell := nil;
  LProjection := nil;
  try
    LSession.Select('records-table');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    Rebuild;
    Check((LShell.Find(NyxStudioColumnAlignmentFormID(0)).Count = 6) and
      not LSession.Document.Find('records-table').CollectionView.HasColumnAlignment,
      'ordinary alignment forms preserve raw absence until Apply');
    LShell.Find(NyxStudioColumnAlignmentFieldID(0, scafHorizontal)).Configure.Value('End').Done;
    LShell.Find(NyxStudioColumnAlignmentFieldID(0, scafVertical)).Configure.Value('Middle').Done;
    Check(Capture and (LEdit.Collection.Action = scaColumnAlignment) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'alignment capture copies exact typed axes without publishing');
    LIntent := TNyxStudioCollectionIntent.FromData(LEdit.Collection.ToData);
    Check(LIntent.SameIntent(LEdit.Collection), 'alignment intent round-trips its exact baseline and policy');
    LSession.ApplyCollectionIntent(LIntent);
    LApplied := EncodeNyxProject(LSession.ProjectSnapshot);
    LPolicy := LSession.Document.Find('records-table').CollectionView.ColumnAt(0).AlignmentPolicy;
    Check((LPolicy.HorizontalAlignment = nchaEnd) and (LPolicy.VerticalAlignment = ncvaMiddle) and
      not LSession.Document.Find('records-table').CollectionView.ColumnAt(1).HasAlignmentPolicy and
      (Pos('.ColumnAlignment(NyxColumnAlignment.Horizontal(nchaEnd).Vertical(ncvaMiddle))',
        LSession.Source) > 0), 'alignment Apply updates only its typed column and generates crafted Pascal');
    LRefused := False;
    try
      Capture;
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LApplied),
      'an old alignment form cannot overwrite the newer accepted binding');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore, 'one Undo restores exact pre-alignment pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LApplied, 'one Redo restores exact alignment pair');
    Rebuild;
    LShell.Find(NyxStudioColumnAlignmentFieldID(0, scafHorizontal)).Configure.Value('Use host default').Done;
    LShell.Find(NyxStudioColumnAlignmentFieldID(0, scafVertical)).Configure.Value('Bottom').Done;
    Check(Capture and not LEdit.Collection.ColumnAlignment.HasHorizontal and
      (LEdit.Collection.ColumnAlignment.VerticalAlignment = ncvaBottom),
      'one-axis Apply preserves inherited inline alignment');
    LSession.ApplyCollectionIntent(LEdit.Collection);
    Rebuild;
    LShell.Find(NyxStudioColumnAlignmentFieldID(0, scafHorizontal)).Configure.Value('unknown').Done;
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LRefused := False;
    try
      Capture;
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'unknown form choices refuse atomically');
    Check(Capture(True) and (LEdit.Collection.Action = scaDefaultColumnAlignment),
      'explicit reset can discard an inert unsubmitted alignment draft');
    LSession.ApplyCollectionIntent(LEdit.Collection);
    Check(not LSession.Document.Find('records-table').CollectionView.HasColumnAlignment,
      'reset removes alignment without manufacturing explicit default axes');
  finally
    LProjection.Free;
    LShell.Free;
    LSession.Free;
  end;
end;

{ Authored order is separate from runtime column preferences. Exact column
  packets prove that movement preserves all declarations and raw presence. }
procedure ColumnOrdering;
var
  LSession: TNyxStudioSession;
  LShell: TNyxNode;
  LProjection: TNyxNode;
  LEdit: TNyxStudioDesignEdit;
  LIntent: TNyxStudioCollectionIntent;
  LBad: TNyxStudioCollectionIntent;
  LRichIntent: TNyxStudioCollectionIntent;
  LSpec: TNyxCollectionViewSpec;
  LMoved: TNyxCollectionViewSpec;
  LRich: TNyxCollectionViewSpec;
  LRichMoved: TNyxCollectionViewSpec;
  LRichData: TNyxDataValue;
  LRichMovedData: TNyxDataValue;
  LMember: Integer;
  LSchema: TNyxCollectionSchema;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LDefinition: TNyxText;
  LSibling: TNyxText;
  LRefused: Boolean;

  procedure Rebuild;
  begin
    FreeAndNil(LShell);
    LShell := TNyxNode.Create(nkPage, 'column-order-inspector');
    LProjection := LSession.SelectedProjection;
    try
      AddNyxCollectionBindingPanel(LShell, LSession, LProjection);
    finally
      FreeAndNil(LProjection);
    end;
  end;

  procedure Refuses(const AIntent: TNyxStudioCollectionIntent;
    const ASpec: TNyxCollectionViewSpec; const AReason: TNyxText);
  var
    LRejected: Boolean;
  begin
    LRejected := False;
    try
      ApplyNyxStudioCollectionViewIntent(AIntent, ASpec, LSchema);
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, AReason);
  end;

begin
  LSession := TNyxStudioSession.Create(Seed);
  LShell := nil;
  LProjection := nil;
  try
    LSession.Select('first-records');
    LSpec := LSession.Document.Find('definition-table').CollectionView;
    LSchema := LSession.Document.Collections.Snapshot(LSpec.Key).Schema;
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LDefinition := TNyxCodec.NodeSnapshot(LSession.Document.Find('definition-table')).ToJSON;
    LSibling := TNyxCodec.NodeSnapshot(LSession.Document.Find('second-panel')).ToJSON;
    Rebuild;
    Check(LShell.Find(NyxStudioColumnOrderID(0, scaColumnEarlier))
      .Prop(NyxAttributeName(atEnabled)) = 'false', 'first authored column cannot move earlier');
    Check(LShell.Find(NyxStudioColumnOrderID(1, scaColumnLater))
      .Prop(NyxAttributeName(atEnabled)) = 'false', 'last authored column cannot move later');
    Check(CaptureNyxStudioCollection(LSession,
      LShell.Find(NyxStudioColumnOrderID(0, scaColumnLater)), ntClick,
      Default(TNyxStudioPendingDesign), LEdit), 'ordinary later button captures typed intent');
    LIntent := TNyxStudioCollectionIntent.FromData(LEdit.Collection.ToData);
    Check((LIntent.ToData.Count = 12) and LIntent.SameIntent(LEdit.Collection) and
      NyxStudioCollectionStructuralAction(LIntent.Action),
      'move descriptor retains its exact typed field and baseline as structural intent');
    LMoved := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, LSchema);
    Check((LMoved.ToData.Field('columns').Item(0).ToJSON = LSpec.ToData.Field('columns').Item(1).ToJSON) and
      (LMoved.ToData.Field('columns').Item(1).ToJSON = LSpec.ToData.Field('columns').Item(0).ToJSON),
      'every original column member moves intact, including explicit text policy');
    Check((LMoved.ToData.Field('version').AsInteger = LSpec.ToData.Field('version').AsInteger) and
      (LMoved.TableWorkbenchPolicy.ToData.ToJSON = LSpec.TableWorkbenchPolicy.ToData.ToJSON) and
      (LMoved.TablePresentationPolicy.ToData.ToJSON = LSpec.TablePresentationPolicy.ToData.ToJSON),
      'movement preserves descriptor version and table policy presence');
    LRich := LSpec.ColumnQuery(NyxColumnQuery.Sorting(False))
      .ColumnLayout(NyxColumnLayout.PinEnd).ColumnText(NyxColumnText.Wrap(2))
      .ColumnAlignment(NyxColumnAlignment.Horizontal(nchaEnd))
      .TableLayout(NyxTableLayout.Enabled(False)).TableQuery(NyxTableQuery.Enabled(False))
      .Inspection(NyxInspectionPolicy.PausedBytes(2048).RemovedIdentities(2))
      .DraftRecovery(NyxDraftRecoveryPolicy.Entries(0).WarnEntries(0));
    LRichIntent := LIntent;
    LRichIntent.LayoutBaseline := NyxQueryEditorBaseline(LSchema, LRich);
    LRichMoved := ApplyNyxStudioCollectionViewIntent(LRichIntent, LRich, LSchema);
    LRichData := LRich.ToData;
    LRichMovedData := LRichMoved.ToData;
    Check((LRichData.Field('version').AsInteger = 14) and
      (LRichData.Field('columns').Item(1).ToJSON = LRichMovedData.Field('columns').Item(0).ToJSON),
      'rich column query/layout/text/alignment values travel together under version14');
    for LMember := 0 to LRichData.Count - 1 do
    begin

      if LRichData.Key(LMember) <> 'columns' then
      begin
        Check(LRichData.Field(LRichData.Key(LMember)).ToJSON =
          LRichMovedData.Field(LRichData.Key(LMember)).ToJSON,
          'column movement preserves independent top-level policy: ' + LRichData.Key(LMember));
      end;
    end;
    Refuses(LIntent, LMoved, 'stale order baseline refuses replay');
    LBad := LIntent;
    LBad.Action := scaColumnEarlier;
    Refuses(LBad, LSpec, 'first-column earlier move refuses without a no-op publication');
    LBad := LIntent;
    LBad.Field := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('Count'));
    Refuses(LBad, LSpec, 'last-column later move refuses');
    LBad.Field := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('Caption'));
    Refuses(LBad, LSpec, 'wrong scalar family refuses');
    LBad.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('foreign'));
    Refuses(LBad, LSpec, 'foreign field refuses');
    LBad := LIntent;
    LBad.Projection := cpList;
    Refuses(LBad, LSpec, 'table order intent refuses a List projection');
    LBad := LIntent;
    LBad.Value := '1';
    Refuses(LBad, LSpec, 'move direction cannot arrive as an unrelated raw string');
    LBad := LIntent;
    LBad.LayoutBaseline := '';
    Refuses(LBad, LSpec, 'missing exact baseline refuses');
    LSession.ApplyCollectionIntent(LIntent);
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    Check((LAfter <> LBefore) and
      LSession.Document.Find('first-records').HasCollectionView and
      (LSession.Document.Find('first-records').CollectionView.ColumnAt(0).FieldName = 'Count'),
      'moving an inherited column creates only that local binding');
    Check((TNyxCodec.NodeSnapshot(LSession.Document.Find('definition-table')).ToJSON = LDefinition) and
      (TNyxCodec.NodeSnapshot(LSession.Document.Find('second-panel')).ToJSON = LSibling),
      'definition and reusable sibling remain exact');
    LRefused := False;
    try
      CaptureNyxStudioCollection(LSession,
        LShell.Find(NyxStudioColumnOrderID(0, scaColumnLater)), ntClick,
        Default(TNyxStudioPendingDesign), LEdit);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LAfter),
      'old mounted order button refuses after the binding moves');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore, 'Undo restores inherited order and source');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'Redo restores local order and source');
    Rebuild;
    LSession.SetSourceDraft(LSession.Source + #10 + '{ unfinished order draft }');
    LRefused := False;
    try
      CaptureNyxStudioCollection(LSession,
        LShell.Find(NyxStudioColumnOrderID(1, scaColumnEarlier)), ntClick,
        Default(TNyxStudioPendingDesign), LEdit);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LSession.SourceDraftPending, 'move preserves unfinished Pascal on refusal');
  finally
    LShell.Free;
    LSession.Free;
  end;
end;

{ The semantic dispatcher admits a title and subsequent order change in one
  revision-aware paired transaction; a retry cannot move the column twice. }
procedure SemanticColumnOrdering;
var
  LAgent: TNyxAgentSession;
  LDocument: TNyxDocument;
  LSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
  LTitle: TNyxStudioCollectionIntent;
  LMove: TNyxStudioCollectionIntent;
  LWire: TNyxDataValue;
  LRequest: TNyxDataValue;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LRevision: Integer;
  LRefused: Boolean;
begin
  LAgent := TNyxAgentSession.Create(Seed);
  LDocument := nil;
  try
    LRevision := LAgent.Revision;
    LBefore := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
    LDocument := TNyxCodec.Decode(LAgent.PreviewPair(LRevision, 'home').Design);
    LSpec := LDocument.Find('records-table').CollectionView;
    LSchema := LDocument.Collections.Snapshot(LSpec.Key).Schema;
    LTitle := Default(TNyxStudioCollectionIntent);
    LTitle.Action := scaTitle;
    LTitle.Key := LSpec.Key;
    LTitle.Projection := cpTable;
    LTitle.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('Caption'));
    LTitle.Value := 'Display caption';
    LSpec := ApplyNyxStudioCollectionViewIntent(LTitle, LSpec, LSchema);
    LMove := Default(TNyxStudioCollectionIntent);
    LMove.Action := scaColumnLater;
    LMove.Key := LSpec.Key;
    LMove.Projection := cpTable;
    LMove.Field := LTitle.Field;
    LMove.LayoutBaseline := NyxQueryEditorBaseline(LSchema, LSpec);
    LWire := NyxCollectionPatch([
      NyxCollectionIntentChange(NyxBindingOwner('records-table'), LTitle),
      NyxCollectionIntentChange(NyxBindingOwner('records-table'), LMove)]).ToData;
    Check(ReadNyxCollectionPatch(LWire).ToData.ToJSON = LWire.ToJSON,
      'semantic move retains field, kind, projection and exact baseline through transport');
    LRequest := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('column-order-group')), NyxField('changes', LWire)]);
    LAgent.Call('nyx_collections', 'Column order test', LRequest);
    LAfter := EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home'));
    FreeAndNil(LDocument);
    LDocument := TNyxCodec.Decode(LAgent.PreviewPair(LAgent.Revision, 'home').Design);
    Check((LDocument.Find('records-table').CollectionView.ColumnAt(1).FieldName = 'Caption') and
      (LDocument.Find('records-table').CollectionView.ColumnAt(1).Title = 'Display caption'),
      'grouped semantic title and move both reach the same stable field');
    LAgent.Call('nyx_collections', 'Column order test', LRequest);
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = LAfter,
      'exact semantic retry cannot move a column twice');
    LRefused := False;
    try
      LAgent.Call('nyx_collections', 'Column order test', NyxObject([
        NyxField('mode', NyxData('apply')), NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operationId', NyxData('column-order-stale')), NyxField('changes', LWire)]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = LAfter),
      'stale semantic revision refuses the whole group');
    LAgent.Call('nyx_history', 'Column order test', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('column-order-undo')), NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = LBefore,
      'one semantic Undo restores the whole title/order/source group');
  finally
    LDocument.Free;
    LAgent.Free;
  end;
end;

procedure SemanticAlignment;
var
  LAgent: TNyxAgentSession;
  LRevision: Integer;
  LKey: TNyxCollectionRef;
  LField: TNyxStudioCollectionFieldRef;
  LBefore: TNyxText;
  LApplied: TNyxText;
  LReply: TNyxDataValue;
  LPlain: TNyxDataValue;
  LRequest: TNyxDataValue;
  LRefused: Boolean;

  function Pair: TNyxText;
  begin
    Result := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
  end;

  procedure Apply(const AID: TNyxText; const AChanges: array of TNyxCollectionChange);
  begin
    LRequest := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData(AID)),
      NyxField('changes', NyxCollectionPatch(AChanges).ToData)]);
    LAgent.Call('nyx_collections', 'Alignment test', LRequest);
    LRevision := LAgent.Revision;
  end;

begin
  LAgent := TNyxAgentSession.Create(Seed);
  try
    LRevision := LAgent.Revision;
    LKey := NyxCollection('inspected-records');
    LField := TNyxStudioCollectionFieldRef.Text(NyxTextField('Caption'));
    LBefore := Pair;
    LPlain := LAgent.Call('nyx_collections', 'Alignment test', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('records-table')),
      NyxField('limit', NyxData(1))]));
    LReply := LAgent.Call('nyx_collections', 'Alignment test', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('records-table')),
      NyxField('alignmentControls', NyxData(False)), NyxField('limit', NyxData(1))]));
    Check(LPlain.ToJSON = LReply.ToJSON, 'false alignment selector retains exact previous response shape');
    LRefused := False;
    try
      NyxSetCollectionColumnAlignment(NyxBindingOwner('records-table'), LKey, cpTable,
        LField, NyxColumnAlignment);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pair = LBefore), 'empty alignment setters require an explicit reset');
    Apply('alignment-group', [NyxSetCollectionColumnAlignment(NyxBindingOwner('records-table'),
      LKey, cpTable, LField, NyxColumnAlignment.Horizontal(nchaEnd).Vertical(ncvaBottom)),
      NyxSetCollectionTablePresentation(NyxBindingOwner('records-table'), LKey, cpTable,
        NyxTablePresentation.Density(ntrdCompact))]);
    LApplied := Pair;
    LAgent.Call('nyx_collections', 'Alignment test', LRequest);
    Check((LAgent.Revision = LRevision) and (Pair = LApplied), 'alignment grouped retries remain idempotent');
    LReply := LAgent.Call('nyx_collections', 'Alignment test', NyxObject([
      NyxField('mode', NyxData('column')), NyxField('owner', NyxData('records-table')),
      NyxField('field', NyxData('Caption')), NyxField('source', NyxData('effective')),
      NyxField('alignmentControls', NyxData(True)), NyxField('count', NyxData(1))]));
    Check(LReply.Field('alignmentControlsSupported').AsBoolean and
      LReply.Field('alignmentControls').Field('declared').AsBoolean and
      (LReply.Field('alignmentControls').Field('policy').ToJSON =
        NyxColumnAlignment.Horizontal(nchaEnd).Vertical(ncvaBottom).ToData.ToJSON),
      'bounded exact-column read exposes the requested policy after a later independent policy edit');
    LAgent.Call('nyx_history', 'Alignment test', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('alignment-undo')),
      NyxField('direction', NyxData('undo'))]));
    LRevision := LAgent.Revision;
    Check(Pair = LBefore, 'one semantic Undo restores both grouped changes and exact source');
    LRefused := False;
    try
      Apply('alignment-family-refusal', [NyxSetCollectionTablePresentation(
        NyxBindingOwner('records-table'), LKey, cpTable, NyxTablePresentation),
        NyxSetCollectionColumnAlignment(NyxBindingOwner('records-table'), LKey, cpTable,
          TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('Caption')),
          NyxColumnAlignment.Horizontal(nchaStart))]);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pair = LBefore) and (LAgent.Revision = LRevision),
      'wrong scalar family refuses the entire candidate including preceding policy edits');
  finally
    LAgent.Free;
  end;
end;

begin
  try
    OrdinaryAuthoring;
    AlignmentAuthoring;
    ColumnOrdering;
    SemanticColumnOrdering;
    SemanticAlignment;
    SemanticAuthoring;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-presentation-authoring', 'passed');
    document.body.setAttribute('data-presentation-authoring-checks', IntToStr(GChecks));
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' presentation authoring checks';
    {$else}
    WriteLn('PASS ', GChecks, ' presentation authoring checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-presentation-authoring', 'failed');
      document.body.setAttribute('data-presentation-authoring-error', E.Message);
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
