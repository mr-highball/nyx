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

program nyx_table_workbench_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.contract,
  nyx.codec, nyx.codegen, nyx.data, nyx.collections, nyx.collections.columns,
  nyx.collections.paging, nyx.collections.view.types, nyx.collections.query.editor,
  nyx.collections.workbench.policy, nyx.studio.projects, nyx.studio.session,
  nyx.studio.authoring, nyx.studio.collectionintent, nyx.studio.collectionedits,
  nyx.studio.collections, nyx.studio.stateedits, nyx.studio.agents
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Workbench authoring: ' + AReason);
  end;
  Inc(GChecks);
end;

function TestPolicy: TNyxTableWorkbenchPolicy;
begin
  Result := NyxTableWorkbench.RangeSelection(True)
    .AllowCopy([nwcaSelectedSource, nwcaSelectedPage, nwcaCellRange])
    .CopyFields(nwcfAllArrangement).Headers(False).LineEnding(nwleCRLF).FocusedDraft(True)
    .Export(NyxTableExport.Allow([nweaAllFiltered]).Columns(nwcfAllArrangement)
      .Headers(False).LineEnding(nwleCRLF));
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
    LTable.Node.SetCollectionView(LSpec.TableWorkbench(TestPolicy));
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

{ Exercise the ordinary complete Nyx form and session admission. This is a
  portable model consumer; physical click/retained-control publication needs
  its separate actual browser/LCL journey. }
procedure OrdinaryAuthoring;
var
  LSession: TNyxStudioSession;
  LShell: TNyxNode;
  LProjection: TNyxNode;
  LEdit: TNyxStudioDesignEdit;
  LIntent: TNyxStudioCollectionIntent;
  LBefore: TNyxText;
  LApplied: TNyxText;
  LRows: TNyxText;
  LRefused: Boolean;
  LPolicy: TNyxTableWorkbenchPolicy;

  procedure Rebuild;
  begin
    FreeAndNil(LShell);
    LShell := TNyxNode.Create(nkPage, 'workbench-inspector');
    LProjection := LSession.SelectedProjection;
    try
      AddNyxCollectionBindingPanel(LShell, LSession, LProjection);
    finally
      FreeAndNil(LProjection);
    end;
  end;

  function Capture(const AID: TNyxText): Boolean;
  begin
    Result := CaptureNyxStudioLayout(LSession, LShell.Find(AID), LShell,
      Default(TNyxStudioPendingDesign), LEdit);
  end;

  procedure Flag(AField: TNyxStudioWorkbenchPolicyField; AValue: Boolean);
  begin
    LShell.Find(NyxStudioWorkbenchPolicyFieldID(AField)).Configure.Value(AValue).Done;
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
    Check((LShell.Find(NyxStudioWorkbenchPolicyFormID) <> nil) and
      (LShell.Find(NyxStudioWorkbenchPolicyFormID).Count = 20),
      'a bound table exposes one complete typed action form');
    Check(not LSession.Document.Find('records-table').CollectionView.HasTableWorkbenchPolicy,
      'opening the form does not declare resolved defaults');
    Flag(swpfRangeSelection, True);
    Flag(swpfCopyCell, False);
    Flag(swpfCopySelectedSource, True);
    Flag(swpfCopySelectedPage, True);
    Flag(swpfCopyCellRange, True);
    Flag(swpfHeaders, False);
    Flag(swpfFocusedDraft, True);
    Flag(swpfExportDeclared, True);
    Flag(swpfExportPage, False);
    Flag(swpfExportFiltered, True);
    Flag(swpfExportHeaders, False);
    LShell.Find(NyxStudioWorkbenchPolicyFieldID(swpfExportFields)).Configure
      .Value('All authored columns, including hidden').Done;
    LShell.Find(NyxStudioWorkbenchPolicyFieldID(swpfExportLineEnding)).Configure.Value('CRLF').Done;
    LShell.Find(NyxStudioWorkbenchPolicyFieldID(swpfCopyFields)).Configure
      .Value('All authored columns, including hidden').Done;
    LShell.Find(NyxStudioWorkbenchPolicyFieldID(swpfLineEnding)).Configure.Value('CRLF').Done;
    Check(Capture(NyxStudioWorkbenchPolicyApplyID),
      'the existing ordinary layout route captures workbench actions');
    Check((EncodeNyxProject(LSession.ProjectSnapshot) = LBefore) and
      (LEdit.Collection.TableWorkbench.ToData.ToJSON = TestPolicy.ToData.ToJSON),
      'capture returns the exact complete typed policy without publishing state');
    LIntent := TNyxStudioCollectionIntent.FromData(LEdit.Collection.ToData);
    Check(LIntent.SameIntent(LEdit.Collection), 'private intent wire retains policy and full form baseline');
    LSession.ApplyCollectionIntent(LIntent);
    LApplied := EncodeNyxProject(LSession.ProjectSnapshot);
    LPolicy := LSession.Document.Find('records-table').CollectionView.TableWorkbenchPolicy;
    Check((LPolicy.ToData.ToJSON = TestPolicy.ToData.ToJSON) and
      (Pos('.TableWorkbench(NyxTableWorkbench', LSession.Source) > 0) and
      (Pos('{ Handwritten companion 🌙 }', LSession.Source) > 0),
      'one accepted form publishes typed Pascal and exact saved choices beside handwritten code');
    Check(TNyxDataValue.ParseJSON(TNyxCodec.Encode(LSession.Document)).Field('collections').ToJSON = LRows,
      'authored actions do not mutate Unicode/NUL records or acquire runtime selection');
    LRefused := False;
    try
      Capture(NyxStudioWorkbenchPolicyApplyID);
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LApplied),
      'an earlier open form cannot replace a newer accepted binding');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore,
      'one Undo restores the exact complete accepted project/source pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LApplied,
      'one Redo restores the same complete accepted pair');
    Rebuild;
    Flag(swpfExportDeclared, False);
    Check(Capture(NyxStudioWorkbenchPolicyApplyID),
      'the ordinary form can withdraw only saved export choices');
    LSession.ApplyCollectionIntent(LEdit.Collection);
    Check(LSession.Document.Find('records-table').CollectionView.TableWorkbenchPolicy.ToData.ToJSON =
      TestPolicy.UseDefaultExport.ToData.ToJSON,
      'export reset preserves every other authored workbench option');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LApplied,
      'one Undo restores the exact policy and source after export-only reset');
    Rebuild;
    { Deliberately corrupt the explicit property boundary: the typed Boolean
      setter correctly refuses text before a complete form can be captured. }
    LShell.Find(NyxStudioWorkbenchPolicyFieldID(swpfCopyCell))
      .SetProp(NyxAttributeName(atValue), 'yes');
    LRefused := False;
    try
      Capture(NyxStudioWorkbenchPolicyApplyID);
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LApplied),
      'a malformed Boolean draft is inert and cannot partially publish');
    Check(Capture(NyxStudioWorkbenchPolicyDefaultID),
      'explicit reset ignores unfinished parameter text');
    Check(TNyxStudioCollectionIntent.FromData(LEdit.Collection.ToData).SameIntent(LEdit.Collection),
      'reset wire carries its baseline without inventing a policy');
    LSession.ApplyCollectionIntent(LEdit.Collection);
    Check(not LSession.Document.Find('records-table').CollectionView.HasTableWorkbenchPolicy and
      (LSession.Document.Find('records-table').CollectionView.PagingPolicy.ItemsPerPage = 2) and
      (LSession.Document.Find('records-table').CollectionView.ColumnAt(0).Sizing.Value = 160),
      'reset removes only action presence while retaining columns, sizing and paging');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LApplied,
      'reset is independently undoable as a complete paired change');
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaTitle;
    LIntent.Key := NyxCollection('inspected-records');
    LIntent.Projection := cpTable;
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('Caption'));
    LIntent.Value := 'Record 🌙';
    LSession.ApplyCollectionIntent(LIntent);
    Check(LSession.Document.Find('records-table').CollectionView.TableWorkbenchPolicy.ToData.ToJSON =
      TestPolicy.ToData.ToJSON, 'ordinary column-title reconstruction preserves action policy');
    Rebuild;
    LSession.SetSourceDraft(LSession.Source + #10 + '{ unfinished companion }');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LRefused := False;
    try
      Capture(NyxStudioWorkbenchPolicyDefaultID);
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'pending Pascal is retained exactly when an ordinary policy action refuses');
    LSession.Select('records-list');
    Rebuild;
    Check(LShell.Find(NyxStudioWorkbenchPolicyFormID) = nil,
      'a list does not expose table-only action controls');
  finally
    LProjection.Free;
    LShell.Free;
    LSession.Free;
  end;
end;

{ Actual semantic calls use revisions, operation identities and the same paired
  session history. No HTTP host is required to qualify this tool boundary. }
procedure SemanticAuthoring;
var
  LAgent: TNyxAgentSession;
  LRevision: Integer;
  LKey: TNyxCollectionRef;
  LBefore: TNyxText;
  LApplied: TNyxText;
  LReply: TNyxDataValue;
  LPlain: TNyxDataValue;
  LFalse: TNyxDataValue;
  LRequest: TNyxDataValue;
  LChange: TNyxStudioCollectionIntent;
  LRefused: Boolean;

  function Pair: TNyxText;
  begin
    Result := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
  end;

  function ReadBinding(const AOwner: TNyxText; AInclude: Boolean): TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_collections', 'Workbench test', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData(AOwner)),
      NyxField('workbenchControls', NyxData(AInclude)), NyxField('limit', NyxData(1))]));
  end;

  function Request(const AID: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)), NyxField('changes', AChanges)]);
  end;

  procedure History(const ADirection, AID: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Workbench test', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)), NyxField('direction', NyxData(ADirection))]));
    LRevision := LAgent.Revision;
  end;

  procedure Apply(const AID: TNyxText; const AChanges: array of TNyxCollectionChange);
  begin
    LAgent.Call('nyx_collections', 'Workbench test',
      Request(AID, NyxCollectionPatch(AChanges).ToData));
    LRevision := LAgent.Revision;
  end;

begin
  LAgent := TNyxAgentSession.Create(Seed);
  try
    LRevision := LAgent.Revision;
    LKey := NyxCollection('inspected-records');
    LBefore := Pair;
    LPlain := LAgent.Call('nyx_collections', 'Workbench test', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('records-table')),
      NyxField('limit', NyxData(1))]));
    LFalse := ReadBinding('records-table', False);
    Check((LPlain.ToJSON = LFalse.ToJSON) and not NyxAgentHas(LPlain, 'workbenchControlsSupported'),
      'omitted and false selectors preserve the exact legacy bounded response');
    LReply := ReadBinding('records-table', True);
    Check(LReply.Field('workbenchControlsSupported').AsBoolean and
      not LReply.Field('local').Field('workbenchControls').Field('declared').AsBoolean and
      (LReply.Field('local').Field('workbenchControls').Field('policy').ToJSON = NyxTableWorkbench.ToData.ToJSON) and
      (LReply.Field('effective').Field('columns').Count = 1) and
      (LReply.Field('effective').Field('total').AsInteger = 2),
      'one requested column page exposes saved absence and complete resolved action defaults');
    Check((Pair = LBefore) and (LAgent.Revision = LRevision),
      'bounded capability reads neither publish defaults nor revise the project');
    LChange := Default(TNyxStudioCollectionIntent);
    LChange.Action := scaTitle;
    LChange.Key := LKey;
    LChange.Projection := cpTable;
    LChange.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('Caption'));
    LChange.Value := 'Grouped 🌙';
    LRequest := Request('workbench-group', NyxCollectionPatch([
      NyxCollectionIntentChange(NyxBindingOwner('records-table'), LChange),
      NyxSetCollectionTableWorkbench(NyxBindingOwner('records-table'), LKey, cpTable, TestPolicy)]).ToData);
    LAgent.Call('nyx_collections', 'Workbench test', LRequest);
    LRevision := LAgent.Revision;
    LApplied := Pair;
    LReply := ReadBinding('records-table', True);
    Check(LReply.Field('local').Field('workbenchControls').Field('declared').AsBoolean and
      (LReply.Field('local').Field('workbenchControls').Field('policy').ToJSON = TestPolicy.ToData.ToJSON) and
      (LReply.Field('local').Field('columns').Item(0).Field('title').Field('preview').AsText = LChange.Value),
      'one semantic group derives the baseline after its preceding title edit');
    LAgent.Call('nyx_collections', 'Workbench test', LRequest);
    Check((LAgent.Revision = LRevision) and (Pair = LApplied),
      'an exact operation retry cannot duplicate the undoable edit');
    History('undo', 'workbench-undo');
    Check(Pair = LBefore, 'one semantic Undo restores both grouped changes and exact source');
    History('redo', 'workbench-redo');
    Check(Pair = LApplied, 'one semantic Redo restores the exact authored group');
    LBefore := Pair;
    LRefused := False;
    try
      Apply('workbench-invalid-group', [
        NyxUseDefaultCollectionTableWorkbench(NyxBindingOwner('records-table'), LKey, cpTable),
        NyxSetCollectionTableWorkbench(NyxBindingOwner('records-list'), LKey, cpTable, TestPolicy)]);
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pair = LBefore) and (LAgent.Revision = LRevision),
      'a later wrong-projection edit refuses the complete group without partial reset');
    LReply := ReadBinding('first-records', True);
    Check(LReply.Field('inherited').AsBoolean and (LReply.Field('local').Kind = ndNull) and
      (LReply.Field('effective').Field('workbenchControls').Field('policy').ToJSON = TestPolicy.ToData.ToJSON),
      'an unmodified reusable part reports inherited action choices without a local declaration');
    Apply('workbench-local-reset', [NyxUseDefaultCollectionTableWorkbench(
      NyxBindingOwner('first-records'), LKey, cpTable)]);
    LReply := ReadBinding('first-records', True);
    Check(not LReply.Field('inherited').AsBoolean and
      not LReply.Field('local').Field('workbenchControls').Field('declared').AsBoolean and
      ReadBinding('definition-table', True).Field('local').Field('workbenchControls').Field('declared').AsBoolean,
      'local default reset preserves the reusable definition and independent raw ownership');
    LChange := Default(TNyxStudioCollectionIntent);
    LChange.Action := scaClear;
    LChange.Key := LKey;
    LChange.Projection := cpTable;
    Apply('workbench-mask', [NyxCollectionIntentChange(NyxBindingOwner('first-records'), LChange)]);
    LReply := ReadBinding('first-records', True);
    Check(not LReply.Field('inherited').AsBoolean and
      LReply.Field('local').Field('cleared').AsBoolean and
      LReply.Field('effective').Field('cleared').AsBoolean and
      (LReply.Field('restorable').Field('workbenchControls').Field('policy').ToJSON = TestPolicy.ToData.ToJSON),
      'an explicit binding mask retains separately queryable restorable action intent');
    LBefore := Pair;
    LRefused := False;
    try
      Apply('workbench-masked-reset', [NyxUseDefaultCollectionTableWorkbench(
        NyxBindingOwner('first-records'), LKey, cpTable)]);
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pair = LBefore), 'action reset cannot silently rebind a masked reusable part');
    LChange.Action := scaInherit;
    Apply('workbench-inherit', [NyxCollectionIntentChange(NyxBindingOwner('first-records'), LChange)]);
    Check(ReadBinding('first-records', True).Field('inherited').AsBoolean,
      'explicit binding inheritance restores the definition action policy');
    LBefore := Pair;
    LRefused := False;
    try
      LAgent.Call('nyx_collections', 'Workbench test', NyxObject([
        NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('records-table')),
        NyxField('workbenchControls', NyxData('true'))]));
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pair = LBefore), 'semantic capability selectors refuse string Booleans');
    LRefused := False;
    try
      LAgent.Call('nyx_collections', 'Workbench test', NyxObject([
        NyxField('mode', NyxData('column')), NyxField('owner', NyxData('records-table')),
        NyxField('field', NyxData('Caption')), NyxField('workbenchControls', NyxData(True))]));
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pair = LBefore), 'table action metadata does not expand the closed column read');
  finally
    LAgent.Free;
  end;
end;

begin
  try
    OrdinaryAuthoring;
    SemanticAuthoring;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-workbench-authoring', 'passed');
    document.body.setAttribute('data-workbench-authoring-checks', IntToStr(GChecks));
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' workbench authoring checks';
    {$else}
    WriteLn('PASS ', GChecks, ' workbench authoring checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-workbench-authoring', 'failed');
      document.body.setAttribute('data-workbench-authoring-error', E.Message);
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
