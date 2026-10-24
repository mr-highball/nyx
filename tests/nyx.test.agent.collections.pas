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



unit nyx.test.agent.collections;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.studio.projects;

{ Independent English review seed, never a listener or user-owned project. }
function CreateNyxAgentCollectionSeed: TNyxProjectPair;
{ Shared semantic journey returns its exact admitted companion for target input
  consumers. Unicode/domain fixtures are qualification data, not starter demos.
  AQueriesOnly exercises the complete changed query boundary independently of
  the older large domain/source scenarios; APair has the same owned value contract. }
function RunNyxAgentCollectionJourney(out APair: TNyxProjectPair;
  AQueriesOnly: Boolean = False): Integer;

implementation

uses
  SysUtils, nyx.model, nyx.types, nyx.controls, nyx.codec, nyx.codegen, nyx.data,
  nyx.contract, nyx.state, nyx.collections, nyx.collections.view.types,
  nyx.collections.selection, nyx.collections.query, nyx.collections.query.editor,
  { BEGIN QUERY_CONTROL_READ_IMPORT }
  nyx.collections.query.policy,
  nyx.collections.arrangement,
  { END QUERY_CONTROL_READ_IMPORT }
  { BEGIN QUERY_POLICY_EDIT_IMPORTS }
  nyx.collections.columns, nyx.collections.paging,
  { END QUERY_POLICY_EDIT_IMPORTS }
  nyx.studio.collectionintent, nyx.studio.collectionedits,
  nyx.studio.stateedits, nyx.studio.agents, nyx.studio.workspaces, nyx.studio.reviews;

function CreateNyxAgentCollectionSeed: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LDefinition: INyxColumn;
  LList: INyxList;
  LInstance: INyxComponent;
  LSource: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Collection workshop';
    LPage := NewNyxPage('home');
    LPage.Configure.Layout(nlColumn).Gap(12).Padding(20).Done;
    LDocument.AddPage(LPage);
    LPage.Add(NewNyxTable('tasks-table'));
    LPage.Add(NewNyxList('tasks-list'));
    LPage.Add(NewNyxTree('tasks-tree'));
    LDefinition := NewNyxColumn('task-card', ncoDescriptor);
    LDocument.AddComponent(LDefinition);
    LList := NewNyxList('definition-list');
    LList.Configure.PartName(NyxPart('items')).Done;
    LDefinition.Add(LList);
    LInstance := NewNyxComponent('first-card');
    LInstance.Configure.Component(NyxComponent('task-card')).Done;
    LInstance.OverridePart(NyxPart('items'), noProperties).Named('first-items');
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('second-card');
    LInstance.Configure.Component(NyxComponent('task-card')).Done;
    LPage.Add(LInstance);
    LSource := TNyxCodegen.Generate(LDocument);
    LSource := TNyxText(StringReplace(String(LSource), 'implementation' + #10,
      'implementation' + #10 + #10 +
      '{ Handwritten helper retained by collection MCP edits. }' + #10 +
      'function CollectionWorkshopNote: TNyxText;' + #10 + 'begin' + #10 +
      '  Result := ''Keep crafting.'';' + #10 + 'end;' + #10, []));
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
  finally
    LDocument.Free;
  end;
end;

{ BEGIN QUERY_CONTROL_READ_CASES }
{ An independent accepted project exercises the actual bounded agent route.
  Its candidates/history never change the original journey's seed or export.
  Policies describe saved/default intent; projection support is a separate fact. }
function RunQueryControlReadCases: Integer;
var
  LAgent: TNyxAgentSession;
  LKey, LWideKey: TNyxCollectionRef;
  LSchema, LWideSchema: TNyxCollectionSchema;
  LBase, LDeclared, LLane, LWide: TNyxCollectionViewSpec;
  LReply, LAbsent, LFalse, LPage, LState, LWire, LStale: TNyxDataValue;
  LIntent: TNyxStudioCollectionIntent;
  LBefore, LAfter, LDefinition, LPeer, LTitle, LFieldName: TNyxText;
  LRevision, LReadRevision, LIndex: Integer;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxCollection.Create('Query control reads: ' + AReason);
    end;
    Inc(Result);
  end;

  function PairText: TNyxText;
  begin
    Result := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
  end;

  function ReadContext(const AMode: TNyxText;
    const AFields: array of TNyxDataField): TNyxDataValue;
  var
    LMembers: array of TNyxDataField;
    LAt: Integer;
  begin
    SetLength(LMembers, Length(AFields) + 1);
    LMembers[0] := NyxField('mode', NyxData(AMode));
    for LAt := 0 to High(AFields) do
    begin
      LMembers[LAt + 1] := AFields[LAt];
    end;
    Result := LAgent.Call('nyx_collections', 'Policy reader', NyxObject(LMembers));
  end;

  function Binding(const AOwner: TNyxText): TNyxDataValue;
  begin
    Result := ReadContext('bindings', [NyxField('owner', NyxData(AOwner)),
      NyxField('queryControls', NyxData(True)), NyxField('limit', NyxData(1))]);
  end;

  function Request(const AID: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)), NyxField('changes', AChanges)]);
  end;

  procedure Apply(const AID: TNyxText; const AChanges: array of TNyxCollectionChange);
  begin
    LAgent.Call('nyx_collections', 'Policy reader',
      Request(AID, NyxCollectionPatch(AChanges).ToData), 'policy-read-owner');
    LRevision := LAgent.Revision;
  end;

  procedure History(const ADirection, AID: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Policy reader', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)), NyxField('direction', NyxData(ADirection))]));
    LRevision := LAgent.Revision;
  end;

  function Replacing(const AData: TNyxDataValue; const AName: TNyxText;
    const AValue: TNyxDataValue): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
    LAt: Integer;
  begin
    SetLength(LFields, AData.Count);
    for LAt := 0 to AData.Count - 1 do
    begin

      if AData.Key(LAt) = AName then
      begin
        LFields[LAt] := NyxField(AName, AValue);
      end
      else
      begin
        LFields[LAt] := NyxField(AData.Key(LAt), AData.Field(AData.Key(LAt)));
      end;
    end;
    Result := NyxObject(LFields);
  end;

  procedure Refuses(const AArguments: TNyxDataValue; const AReason: TNyxText;
    const AMessage: TNyxText = '');
  var
    LPair: TNyxText;
    LSession, LAfterSession: TNyxDataValue;
    LRejected: Boolean;
  begin
    LPair := PairText;
    LSession := LAgent.Call('nyx_session', 'Policy reader', NyxObject([]));
    LRejected := False;
    try
      LAgent.Call('nyx_collections', 'Policy reader', AArguments, 'policy-read-owner');
    except
      on E: Exception do
      begin
        LRejected := (AMessage = '') or (Pos(AMessage, TNyxText(E.Message)) > 0);
      end;
    end;
    LAfterSession := LAgent.Call('nyx_session', 'Policy reader', NyxObject([]));
    Check(LRejected and (LAgent.Revision = LRevision) and (PairText = LPair) and
      (LSession.Field('canUndo').AsBoolean = LAfterSession.Field('canUndo').AsBoolean) and
      (LSession.Field('canRedo').AsBoolean = LAfterSession.Field('canRedo').AsBoolean) and
      (LSession.Field('selection').AsText = LAfterSession.Field('selection').AsText), AReason);
  end;

  function PolicyIntent(AAction: TNyxStudioCollectionAction): TNyxStudioCollectionIntent;
  var
    LDocument: TNyxDocument;
  begin
    Result := Default(TNyxStudioCollectionIntent);
    Result.Action := AAction;
    Result.Key := LKey;
    Result.Projection := cpTable;
    LDocument := TNyxCodec.Decode(LAgent.PreviewPair(LRevision, 'home').Design);
    try
      Result.QueryBaseline := NyxQueryEditorBaseline(
        LDocument.Collections.Snapshot(LKey).Schema,
        LDocument.Find('tasks-table').CollectionView);
    finally
      LDocument.Free;
    end;

    if AAction in [scaColumnQuery, scaDefaultColumnQuery] then
    begin
      Result.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('caption'));
    end;
  end;

begin
  Result := 0;
  LAgent := TNyxAgentSession.Create(CreateNyxAgentCollectionSeed);
  try
    LRevision := LAgent.Revision;
    LKey := NyxCollection('policy-rows');
    LTitle := TNyxText(StringOfChar('x', 5000)) + TNyxText('🌙');
    LSchema := NyxCollectionSchema.Text(NyxTextField('caption'), '')
      .Integer(NyxIntegerField('priority'), 0).Boolean(NyxBooleanField('done'), False)
      .Number(NyxNumberField('ratio'), 0);
    LBase := NyxCollectionView(LKey).Column(NyxTextField('caption'), LTitle)
      .Column(NyxIntegerField('priority'), 'Priority')
      .Column(NyxBooleanField('done'), 'Done').Column(NyxNumberField('ratio'), 'Ratio');
    Apply('policy-read-seed', [NyxDefineCollection(LKey, LSchema, []),
      NyxBindCollection(NyxBindingOwner('tasks-table'), cpTable, LBase),
      NyxBindCollection(NyxBindingOwner('tasks-list'), cpList,
        NyxCollectionView(LKey).Column(NyxTextField('caption'), 'Peer'))]);
    LBefore := PairText;
    LReadRevision := LRevision;
    LState := LAgent.Call('nyx_session', 'Policy reader', NyxObject([]));
    LAbsent := ReadContext('bindings', [NyxField('owner', NyxData('tasks-table'))]);
    LFalse := ReadContext('bindings', [NyxField('owner', NyxData('tasks-table')),
      NyxField('queryControls', NyxData(False))]);
    Check((LAbsent.ToJSON = LFalse.ToJSON) and
      not NyxAgentHas(LAbsent, 'queryControlsSupported') and
      not NyxAgentHas(LAbsent.Field('local'), 'queryControls') and
      not NyxAgentHas(LAbsent.Field('local').Field('columns').Item(0), 'queryControls'),
      'omitted and false selectors retain the exact legacy binding shape');
    LAbsent := ReadContext('column', [NyxField('owner', NyxData('tasks-table')),
      NyxField('field', NyxData('caption')), NyxField('source', NyxData('local')),
      NyxField('offset', NyxData(5000)), NyxField('count', NyxData(1))]);
    LFalse := ReadContext('column', [NyxField('owner', NyxData('tasks-table')),
      NyxField('field', NyxData('caption')), NyxField('source', NyxData('local')),
      NyxField('offset', NyxData(5000)), NyxField('count', NyxData(1)),
      NyxField('queryControls', NyxData(False))]);
    Check((LAbsent.ToJSON = LFalse.ToJSON) and not NyxAgentHas(LAbsent, 'queryControls'),
      'omitted and false selectors retain the exact legacy column window');
    LReply := Binding('tasks-table');
    Check(LReply.Field('queryControlsSupported').AsBoolean and
      not LReply.Field('local').Field('queryControls').Field('declared').AsBoolean and
      (LReply.Field('local').Field('queryControls').Field('policy').ToJSON =
        NyxTableQuery.ToData.ToJSON), 'an absent table choice exposes a resolved default without declaring it');
    LPage := ReadContext('bindings', [NyxField('owner', NyxData('tasks-table')),
      NyxField('offset', NyxData(1)), NyxField('limit', NyxData(1)),
      NyxField('queryControls', NyxData(True))]).Field('effective');
    Check((LPage.Field('columns').Count = 1) and (LPage.Field('total').AsInteger = 4) and
      (LPage.Field('nextOffset').AsInteger = 2) and
      (LPage.Field('columns').Item(0).Field('field').AsText = 'priority') and
      not LPage.Field('columns').Item(0).Field('queryControls').Field('declared').AsBoolean,
      'only the requested column page carries its default capability metadata');
    LReply := ReadContext('column', [NyxField('owner', NyxData('tasks-table')),
      NyxField('field', NyxData('caption')), NyxField('source', NyxData('effective')),
      NyxField('offset', NyxData(5000)), NyxField('count', NyxData(1)),
      NyxField('queryControls', NyxData(True))]);
    Check((LReply.Field('title').Field('text').AsText = TNyxText('🌙')) and
      (LReply.Field('queryControls').Field('policy').ToJSON = NyxColumnQuery.ToData.ToJSON),
      'capability metadata preserves the independent supplementary title window');
    Check((PairText = LBefore) and (LRevision = LReadRevision) and
      (LAgent.Revision = LReadRevision) and
      (LAgent.Call('nyx_session', 'Policy reader', NyxObject([])).Field('selection').AsText =
        LState.Field('selection').AsText), 'reads do not publish policy defaults or navigate');

    LDeclared := NyxCollectionView(LKey).Column(NyxTextField('caption'), LTitle)
      .ColumnQuery(NyxColumnQuery.Operators([nqcEqual, nqcContains]).TextComparison(nqtAsciiInsensitive))
      .Column(NyxIntegerField('priority'), 'Priority')
      .ColumnQuery(NyxColumnQuery.Filtering(False).Operators([nqcEqual]))
      .Column(NyxBooleanField('done'), 'Done')
      .ColumnQuery(NyxColumnQuery.Filtering(False).Operators([]))
      .Column(NyxNumberField('ratio'), 'Ratio').ColumnQuery(NyxColumnQuery)
      .TableQuery(NyxTableQuery);
    Apply('policy-read-declarations', [NyxBindCollection(NyxBindingOwner('tasks-table'), cpTable, LDeclared)]);
    LReply := ReadContext('bindings', [NyxField('owner', NyxData('tasks-table')),
      NyxField('limit', NyxData(4)), NyxField('queryControls', NyxData(True))]);
    LPage := LReply.Field('local');
    Check(LPage.Field('queryControls').Field('declared').AsBoolean and
      (LPage.Field('queryControls').Field('policy').ToJSON = NyxTableQuery.ToData.ToJSON),
      'an explicitly saved default remains distinct from no table declaration');
    for LIndex := 0 to 3 do
    begin
      Check(LPage.Field('columns').Item(LIndex).Field('queryControls').Field('declared').AsBoolean and
        (LPage.Field('columns').Item(LIndex).Field('queryControls').Field('policy').ToJSON =
          LDeclared.ColumnAt(LIndex).QueryPolicy.ToData.ToJSON),
        'the requested field retains its exact typed saved policy ' + TNyxText(IntToStr(LIndex)));
    end;
    Check((LPage.Field('columns').Item(1).Field('queryControls').Field('policy').Field('operators').Count = 1) and
      not LPage.Field('columns').Item(1).Field('queryControls').Field('policy').Field('filtering').AsBoolean and
      (LPage.Field('columns').Item(2).Field('queryControls').Field('policy').Field('operators').Count = 0) and
      (LPage.Field('columns').Item(3).Field('queryControls').Field('policy').Field('operators').Kind = ndNull),
      'disabled subsets, explicit empty operators and default null operators remain distinct');

    LLane := NyxCollectionView(LKey).Scoped(csInstance).Column(NyxTextField('caption'), 'Lane')
      .ColumnQuery(NyxColumnQuery.Sorting(False).Operators([nqcEqual]))
      .TableQuery(NyxTableQuery.Filtering(False));
    Apply('policy-read-lane', [NyxBindCollection(NyxBindingOwner('definition-list'), cpList, LLane)]);
    LDefinition := Binding('definition-list').Field('local').ToJSON;
    LPeer := Binding('tasks-list').Field('local').ToJSON;
    LReply := Binding('first-items');
    Check(not LReply.Field('queryControlsSupported').AsBoolean and
      LReply.Field('inherited').AsBoolean and (LReply.Field('local').Kind = ndNull) and
      LReply.Field('effective').Field('queryControls').Field('declared').AsBoolean and
      (LReply.Field('effective').Field('queryControls').Field('policy').ToJSON =
        LLane.TableQueryPolicy.ToData.ToJSON), 'a List reports unsupported presentation without hiding inherited saved intent');
    Apply('policy-read-local-lane', [NyxBindCollection(NyxBindingOwner('first-items'), cpList,
      LLane.TableQuery(NyxTableQuery.Sorting(False)))]);
    LReply := Binding('first-items');
    Check(not LReply.Field('inherited').AsBoolean and
      (LReply.Field('local').Field('queryControls').Field('policy').ToJSON =
        NyxTableQuery.Sorting(False).ToData.ToJSON) and
      (Binding('definition-list').Field('local').ToJSON = LDefinition) and
      (Binding('tasks-list').Field('local').ToJSON = LPeer),
      'local reusable policy overrides preserve definition and independent peer metadata');
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaClear;
    LIntent.Key := LKey;
    LIntent.Projection := cpList;
    Apply('policy-read-mask', [NyxCollectionIntentChange(NyxBindingOwner('first-items'), LIntent)]);
    LReply := Binding('first-items');
    Check(LReply.Field('local').Field('cleared').AsBoolean and
      LReply.Field('effective').Field('cleared').AsBoolean and
      (LReply.Field('restorable').Field('queryControls').Field('policy').ToJSON =
        LLane.TableQueryPolicy.ToData.ToJSON), 'a clear mask exposes only its actual restorable inherited policy');
    LReply := ReadContext('column', [NyxField('owner', NyxData('first-items')),
      NyxField('field', NyxData('caption')), NyxField('source', NyxData('restorable')),
      NyxField('queryControls', NyxData(True))]);
    Check(not LReply.Field('queryControlsSupported').AsBoolean and
      LReply.Field('queryControls').Field('declared').AsBoolean and
      (LReply.Field('queryControls').Field('policy').ToJSON = LLane.ColumnAt(0).QueryPolicy.ToData.ToJSON),
      'an exact restorable List column exposes its stored policy without granting a header');
    LReply := Binding('home');
    Check(not LReply.Field('queryControlsSupported').AsBoolean and
      (LReply.Field('local').Kind = ndNull) and (LReply.Field('effective').Kind = ndNull),
      'an unrelated owner never acquires a policy or collection by inspection');

    LBefore := PairText;
    LIntent := PolicyIntent(scaTableQuery);
    LIntent.TableQuery := NyxTableQuery.Filtering(False);
    LStale := Request('policy-read-stale', NyxCollectionPatch([
      NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LIntent)]).ToData);
    Apply('policy-read-set-table', [NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LIntent)]);
    LAfter := PairText;
    Check(not Binding('tasks-table').Field('effective').Field('queryControls').Field('policy')
      .Field('filtering').AsBoolean, 'the ordinary table policy intent is visible through the same read');
    History('undo', 'policy-read-undo');
    Check(PairText = LBefore, 'one Undo restores exact design and handwritten source');
    History('redo', 'policy-read-redo');
    Check(PairText = LAfter, 'one Redo restores the exact policy publication');
    Refuses(LStale, 'stale revision refuses the complete policy change');
    LIntent := PolicyIntent(scaDefaultTableQuery);
    Apply('policy-read-default-table', [NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LIntent)]);
    Check(not Binding('tasks-table').Field('local').Field('queryControls').Field('declared').AsBoolean,
      'default-table removes only the saved table choice');
    LIntent := PolicyIntent(scaColumnQuery);
    LIntent.ColumnQuery := NyxColumnQuery.Sorting(False).Operators([nqcEqual]);
    Apply('policy-read-set-column', [NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LIntent)]);
    LReply := Binding('tasks-table');
    Check(not LReply.Field('local').Field('columns').Item(0).Field('queryControls').Field('policy')
      .Field('sorting').AsBoolean, 'the ordinary exact-field policy intent admits without a replacement binding');
    LIntent := PolicyIntent(scaDefaultColumnQuery);
    Apply('policy-read-default-column', [NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LIntent)]);
    LReply := ReadContext('bindings', [NyxField('owner', NyxData('tasks-table')),
      NyxField('limit', NyxData(4)), NyxField('queryControls', NyxData(True))]);
    Check(not LReply.Field('local').Field('columns').Item(0).Field('queryControls').Field('declared').AsBoolean and
      LReply.Field('local').Field('columns').Item(1).Field('queryControls').Field('declared').AsBoolean and
      (Binding('definition-list').Field('local').ToJSON = LDefinition) and
      (Pos('CollectionWorkshopNote', LAgent.PreviewPair(LRevision, 'home').Source) > 0),
      'default-column preserves peer declarations, reusable definition and authored helper');
    LIntent := PolicyIntent(scaColumnQuery);
    LIntent.ColumnQuery := NyxColumnQuery;
    LWire := NyxCollectionPatch([NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LIntent)]).ToData.Item(0);
    Refuses(Request('policy-read-family', NyxArray([Replacing(LWire, 'kind', NyxData('integer'))])),
      'a changed field family cannot steal the column policy');
    Refuses(Request('policy-read-shape', NyxArray([Replacing(LWire, 'columnQuery', NyxData('default'))])),
      'a raw string cannot replace the closed column policy');
    LIntent.QueryBaseline := LIntent.QueryBaseline + ' stale';
    Refuses(Request('policy-read-baseline', NyxCollectionPatch([
      NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LIntent)]).ToData),
      'a stale full-spec baseline refuses without a partial candidate');
    Refuses(NyxObject([NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('tasks-table')),
      NyxField('queryControls', NyxData('true'))]), 'selector strings are not Boolean values');
    Refuses(NyxObject([NyxField('mode', NyxData('column')), NyxField('owner', NyxData('tasks-table')),
      NyxField('source', NyxData('local')), NyxField('field', NyxData('caption')),
      NyxField('queryControls', NyxData(1))]), 'selector numbers are not Boolean values');
    Refuses(NyxObject([NyxField('mode', NyxData('list')), NyxField('queryControls', NyxData(False))]),
      'even false selectors refuse on an unrelated closed mode');

    LWideKey := NyxCollection('wide-policy-rows');
    LWideSchema := NyxCollectionSchema;
    LWide := NyxCollectionView(LWideKey);
    LTitle := '';
    for LIndex := 1 to 90 do
    begin
      LTitle := LTitle + TNyxText('🌙');
    end;
    for LIndex := 0 to 63 do
    begin
      LFieldName := 'field-' + TNyxText(IntToStr(LIndex));
      LWideSchema := LWideSchema.Text(NyxTextField(LFieldName), '');
      LWide := LWide.Column(NyxTextField(LFieldName), LTitle)
        .ColumnQuery(NyxColumnQuery.Operators([nqcEqual, nqcContains]));
    end;
    Apply('policy-read-wide', [NyxDefineCollection(LWideKey, LWideSchema, []),
      NyxBindCollection(NyxBindingOwner('tasks-table'), cpTable, LWide)]);
    LFalse := ReadContext('bindings', [NyxField('owner', NyxData('tasks-table')),
      NyxField('limit', NyxData(50)), NyxField('queryControls', NyxData(False))]);
    Check((LFalse.Field('local').Field('columns').Count = 50) and
      (LFalse.Field('effective').Field('columns').Count = 50),
      'the legacy page fits before optional policy metadata crosses its byte budget');
    Refuses(NyxObject([NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('tasks-table')),
      NyxField('limit', NyxData(50)), NyxField('queryControls', NyxData(True))]),
      'requested policy metadata still obeys the complete UTF-8 response budget', 'Context exceeds 48 KiB');
    LReply := Binding('tasks-table');
    Check((LReply.Field('local').Field('columns').Count = 1) and
      (LReply.Field('local').Field('total').AsInteger = 64),
      'a smaller requested policy page recovers without changing the accepted project');
  finally
    LAgent.Free;
  end;
end;

{ END QUERY_CONTROL_READ_CASES }
{ BEGIN QUERY_POLICY_EDIT_CASES }
{ Policy request inputs come from bounded semantic reads or deliberately owned
  setup proposals. PreviewPair is used only by setup and exact result/history
  oracles; no query-editor baseline is read, reconstructed or sent to agents. }
function RunQueryPolicyAgentEdits: Integer;
var
  LAgent: TNyxAgentSession;
  LSeed, LEditor: TNyxProjectPair;
  LDocument: TNyxDocument;
  LTable: INyxTable;
  LKey: TNyxCollectionRef;
  LSchema: TNyxCollectionSchema;
  LSpec, LOutputSpec: TNyxCollectionViewSpec;
  LIntent, LPageIntent, LInitialIntent, LClear: TNyxStudioCollectionIntent;
  LReply, LFields, LRequest, LFiles: TNyxDataValue;
  LPolicy: TNyxColumnQueryPolicy;
  LTablePolicy: TNyxTableQueryPolicy;
  LReference: TNyxStudioCollectionFieldRef;
  LBefore, LAfter, LDefinition, LSibling, LList, LSource: TNyxText;
  LFieldName: TNyxText;
  LRevision, LIndex, LOldRevision: Integer;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxCollection.Create('Agent policy edits: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure RefreshRevision;
  begin
    LRevision := LAgent.Call('nyx_session', 'Policy author', NyxObject([]))
      .Field('revision').AsInteger;
  end;

  function Read(const AMode: TNyxText;
    const AFields: array of TNyxDataField): TNyxDataValue;
  var
    LMembers: array of TNyxDataField;
    LAt: Integer;
  begin
    SetLength(LMembers, Length(AFields) + 1);
    LMembers[0] := NyxField('mode', NyxData(AMode));
    for LAt := 0 to High(AFields) do
    begin
      LMembers[LAt + 1] := AFields[LAt];
    end;
    Result := LAgent.Call('nyx_collections', 'Policy author', NyxObject(LMembers));
  end;

  function Binding(const AOwner: TNyxText): TNyxDataValue;
  begin
    Result := Read('bindings', [NyxField('owner', NyxData(AOwner)),
      NyxField('queryControls', NyxData(True)), NyxField('limit', NyxData(4))]);
  end;

  function FieldReference(const AColumn: TNyxDataValue): TNyxStudioCollectionFieldRef;
  var
    LName, LKind: TNyxText;
  begin
    LName := AColumn.Field('field').AsText;
    LKind := AColumn.Field('kind').AsText;

    if LKind = 'text' then
    begin
      Exit(TNyxStudioCollectionFieldRef.Text(NyxTextField(LName)));
    end;

    if LKind = 'boolean' then
    begin
      Exit(TNyxStudioCollectionFieldRef.Boolean(NyxBooleanField(LName)));
    end;

    if LKind = 'integer' then
    begin
      Exit(TNyxStudioCollectionFieldRef.Integer(NyxIntegerField(LName)));
    end;

    if LKind = 'number' then
    begin
      Exit(TNyxStudioCollectionFieldRef.Number(NyxNumberField(LName)));
    end;
    raise ENyxCollection.Create('The policy read returned an unknown scalar family');
  end;

  function TableEdit(const AOwner: TNyxText; const APolicy: TNyxTableQueryPolicy;
    ADefault: Boolean): TNyxCollectionChange;
  var
    LBinding: TNyxDataValue;
    LReadKey: TNyxCollectionRef;
  begin
    LBinding := Binding(AOwner);
    LReadKey := NyxCollection(LBinding.Field('effective').Field('key').AsText);

    if ADefault then
    begin
      Exit(NyxUseDefaultCollectionTableQuery(NyxBindingOwner(LBinding.Field('owner').AsText),
        LReadKey, cpTable));
    end;
    Result := NyxSetCollectionTableQuery(NyxBindingOwner(LBinding.Field('owner').AsText),
      LReadKey, cpTable, APolicy);
  end;

  function ColumnEdit(const AOwner, AField: TNyxText;
    const APolicy: TNyxColumnQueryPolicy; ADefault: Boolean): TNyxCollectionChange;
  var
    LBinding, LColumn: TNyxDataValue;
    LReadKey: TNyxCollectionRef;
    LReadField: TNyxStudioCollectionFieldRef;
  begin
    LBinding := Binding(AOwner);
    LColumn := Read('column', [NyxField('owner', NyxData(AOwner)),
      NyxField('source', NyxData('effective')), NyxField('field', NyxData(AField)),
      NyxField('queryControls', NyxData(True))]);
    LReadKey := NyxCollection(LBinding.Field('effective').Field('key').AsText);
    LReadField := FieldReference(LColumn);

    if ADefault then
    begin
      Exit(NyxUseDefaultCollectionColumnQuery(NyxBindingOwner(LColumn.Field('owner').AsText),
        LReadKey, cpTable, LReadField));
    end;
    Result := NyxSetCollectionColumnQuery(NyxBindingOwner(LColumn.Field('owner').AsText),
      LReadKey, cpTable, LReadField, APolicy);
  end;

  function Arguments(const AID: TNyxText;
    const AChanges: array of TNyxCollectionChange): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData(AID)),
      NyxField('changes', NyxCollectionPatch(AChanges).ToData)]);
  end;

  procedure Apply(const AID: TNyxText; const AChanges: array of TNyxCollectionChange);
  begin
    LAgent.Call('nyx_collections', 'Policy author', Arguments(AID, AChanges), 'policy-author');
    RefreshRevision;
  end;

  function PairOracle: TNyxText;
  begin
    Result := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
  end;

  procedure History(const ADirection, AID: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Policy author', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData(AID)),
      NyxField('direction', NyxData(ADirection))]), 'policy-author');
    RefreshRevision;
  end;

  procedure Refuses(const ATool: TNyxText; const AArguments: TNyxDataValue;
    const AReason: TNyxText; const AMessage: TNyxText = '');
  var
    LPair: TNyxText;
    LBeforeState, LAfterState: TNyxDataValue;
    LRejected: Boolean;
  begin
    LPair := PairOracle;
    LBeforeState := LAgent.Call('nyx_session', 'Policy author', NyxObject([]));
    LRejected := False;
    try
      LAgent.Call(ATool, 'Policy author', AArguments, 'policy-author');
    except
      on E: Exception do
      begin
        LRejected := (AMessage = '') or (Pos(AMessage, TNyxText(E.Message)) > 0);
      end;
    end;
    LAfterState := LAgent.Call('nyx_session', 'Policy author', NyxObject([]));
    Check(LRejected and (PairOracle = LPair) and
      (LAfterState.Field('revision').AsInteger = LRevision) and
      (LBeforeState.Field('canUndo').AsBoolean = LAfterState.Field('canUndo').AsBoolean) and
      (LBeforeState.Field('canRedo').AsBoolean = LAfterState.Field('canRedo').AsBoolean) and
      (LBeforeState.Field('selection').AsText = LAfterState.Field('selection').AsText) and
      (LBeforeState.Field('view').AsText = LAfterState.Field('view').AsText), AReason);
  end;

  { Continue on the same agent, table, query and reusable owners. Layout edits
    must compose with the accepted authoring state, not pass on a store-only
    substitute that omits ordinary source/history and pending-draft admission. }
  procedure LayoutCases;
  var
    LBinding: TNyxDataValue;
    LAbsent: TNyxDataValue;
    LFalse: TNyxDataValue;
    LStale: TNyxDataValue;
    LLayoutTable: TNyxTableLayoutPolicy;
    LLayoutColumn: TNyxColumnLayoutPolicy;
    LLayoutBefore: TNyxText;
    LLayoutAfter: TNyxText;
    LDefinitionBefore: TNyxText;
    LSiblingBefore: TNyxText;
    LListBefore: TNyxText;
    LBeforeQuery: TNyxText;
    LBeforePaging: TNyxText;
    LBeforeColumnQuery: TNyxText;
    LLayoutRevision: Integer;
    LLayoutSpec: TNyxCollectionViewSpec;
    LLayoutIntent: TNyxStudioCollectionIntent;

    function LayoutBinding(const AOwner: TNyxText): TNyxDataValue;
    begin
      Result := Read('bindings', [NyxField('owner', NyxData(AOwner)),
        NyxField('layoutControls', NyxData(True)), NyxField('limit', NyxData(4))]);
    end;

    function TableLayoutEdit(const AOwner: TNyxText;
      const APolicy: TNyxTableLayoutPolicy; ADefault: Boolean): TNyxCollectionChange;
    var
      LOwner: TNyxDataValue;
      LOwnerKey: TNyxCollectionRef;
    begin
      LOwner := LayoutBinding(AOwner);
      LOwnerKey := NyxCollection(LOwner.Field('effective').Field('key').AsText);

      if ADefault then
      begin
        Exit(NyxUseDefaultCollectionTableLayout(NyxBindingOwner(AOwner), LOwnerKey, cpTable));
      end;
      Result := NyxSetCollectionTableLayout(NyxBindingOwner(AOwner), LOwnerKey, cpTable, APolicy);
    end;

    function ColumnLayoutEdit(const AOwner: TNyxText; const AField: TNyxText;
      const APolicy: TNyxColumnLayoutPolicy; ADefault: Boolean): TNyxCollectionChange;
    var
      LOwner: TNyxDataValue;
      LColumn: TNyxDataValue;
      LOwnerKey: TNyxCollectionRef;
      LTypedField: TNyxStudioCollectionFieldRef;
    begin
      LOwner := LayoutBinding(AOwner);
      LOwnerKey := NyxCollection(LOwner.Field('effective').Field('key').AsText);
      LColumn := Read('column', [NyxField('owner', NyxData(AOwner)),
        NyxField('source', NyxData('effective')), NyxField('field', NyxData(AField)),
        NyxField('layoutControls', NyxData(True))]);
      LTypedField := FieldReference(LColumn);

      if ADefault then
      begin
        Exit(NyxUseDefaultCollectionColumnLayout(NyxBindingOwner(AOwner), LOwnerKey,
          cpTable, LTypedField));
      end;
      Result := NyxSetCollectionColumnLayout(NyxBindingOwner(AOwner), LOwnerKey,
        cpTable, LTypedField, APolicy);
    end;

  begin
    LAbsent := Read('bindings', [NyxField('owner', NyxData('tasks-table')),
      NyxField('limit', NyxData(4))]);
    LFalse := Read('bindings', [NyxField('owner', NyxData('tasks-table')),
      NyxField('limit', NyxData(4)), NyxField('layoutControls', NyxData(False))]);
    Check((LAbsent.ToJSON = LFalse.ToJSON) and
      not NyxAgentHas(LAbsent, 'layoutControlsSupported') and
      not NyxAgentHas(LAbsent.Field('local'), 'layoutControls'),
      'omitted and false layout selectors preserve the exact bounded legacy reply');
    LBinding := LayoutBinding('tasks-table');
    Check(LBinding.Field('layoutControlsSupported').AsBoolean and
      not LBinding.Field('local').Field('layoutControls').Field('declared').AsBoolean and
      (LBinding.Field('effective').Field('layoutControls').Field('policy').ToJSON =
        NyxTableLayout.ToData.ToJSON),
      'layout discovery separates an absent authored table policy from resolved library defaults');
    LLayoutBefore := PairOracle;
    LLayoutRevision := LRevision;
    Apply('layout-author-explicit-default', [TableLayoutEdit('tasks-table', NyxTableLayout, False),
      ColumnLayoutEdit('tasks-table', 'caption', NyxColumnLayout, False)]);
    LLayoutAfter := PairOracle;
    LBinding := LayoutBinding('tasks-table');
    Check((LRevision = LLayoutRevision + 1) and
      LBinding.Field('local').Field('layoutControls').Field('declared').AsBoolean and
      LBinding.Field('local').Field('columns').Item(0).Field('layoutControls')
        .Field('declared').AsBoolean,
      'one named layout group admits explicit default objects as real authored declarations');
    History('undo', 'layout-author-undo-default');
    Check(PairOracle = LLayoutBefore, 'one layout Undo restores the exact design/source pair');
    History('redo', 'layout-author-redo-default');
    Check(PairOracle = LLayoutAfter, 'one layout Redo restores the exact explicit declarations');

    LEditor := LAgent.PreviewPair(LRevision, 'home');
    LDocument := TNyxCodec.Decode(LEditor.Design);
    try
      LLayoutSpec := LDocument.Find('tasks-table').CollectionView;
      LBeforeQuery := LLayoutSpec.QueryPolicy.ToData.ToJSON;
      LBeforePaging := LLayoutSpec.PagingPolicy.ToData.ToJSON;
      LBeforeColumnQuery := LLayoutSpec.ColumnAt(0).QueryPolicy.ToData.ToJSON;
    finally
      LDocument.Free;
    end;
    LLayoutTable := NyxTableLayout.Enabled(True).MinimumCenter(112).AutoSizeSample(16)
      .AllowOnly([nclfResize, nclfAutoSize, nclfPinning]);
    LLayoutColumn := NyxColumnLayout.PinStart.AllowOnly([nclfResize, nclfPinning]);
    Apply('layout-author-complete-group', [TableLayoutEdit('tasks-table', LLayoutTable, False),
      ColumnLayoutEdit('tasks-table', 'caption', LLayoutColumn, False),
      ColumnLayoutEdit('tasks-table', 'priority', NyxColumnLayout.Visible(False).PinEnd, False),
      ColumnLayoutEdit('tasks-table', 'done', NyxColumnLayout.AllowOnly([]), False)]);
    LEditor := LAgent.PreviewPair(LRevision, 'home');
    LDocument := TNyxCodec.Decode(LEditor.Design);
    try
      LLayoutSpec := LDocument.Find('tasks-table').CollectionView;
      Check((LLayoutSpec.TableLayoutPolicy.ToData.ToJSON = LLayoutTable.ToData.ToJSON) and
        (LLayoutSpec.ColumnAt(0).LayoutPolicy.ToData.ToJSON = LLayoutColumn.ToData.ToJSON) and
        not LLayoutSpec.ColumnAt(1).LayoutPolicy.IsVisible and
        (LLayoutSpec.ColumnAt(1).LayoutPolicy.Pin = ncpEnd) and
        LLayoutSpec.ColumnAt(2).HasLayoutPolicy and
        (LLayoutSpec.ColumnAt(2).LayoutPolicy.Features = []),
        'typed layout edits retain logical pins, hidden columns and an explicit empty feature set');
      Check((LLayoutSpec.QueryPolicy.ToData.ToJSON = LBeforeQuery) and
        (LLayoutSpec.PagingPolicy.ToData.ToJSON = LBeforePaging) and
        (LLayoutSpec.ColumnAt(0).QueryPolicy.ToData.ToJSON = LBeforeColumnQuery) and
        (Pos('CollectionWorkshopNote', LEditor.Source) > 0) and
        (Pos('TableLayout(', LEditor.Source) > 0),
        'layout authoring preserves query, paging, typed column behavior and handwritten source');
    finally
      LDocument.Free;
    end;
    Apply('layout-author-default-table', [TableLayoutEdit('tasks-table',
      Default(TNyxTableLayoutPolicy), True)]);
    LBinding := LayoutBinding('tasks-table');
    Check(not LBinding.Field('local').Field('layoutControls').Field('declared').AsBoolean and
      LBinding.Field('local').Field('columns').Item(0).Field('layoutControls')
        .Field('declared').AsBoolean and not LBinding.Field('inherited').AsBoolean,
      'table policy reset withdraws only its raw declaration and keeps column choices/local ownership');
    Apply('layout-author-default-column', [ColumnLayoutEdit('tasks-table', 'caption',
      Default(TNyxColumnLayoutPolicy), True)]);
    LBinding := LayoutBinding('tasks-table');
    Check(not LBinding.Field('local').Field('columns').Item(0).Field('layoutControls')
      .Field('declared').AsBoolean and
      LBinding.Field('local').Field('columns').Item(1).Field('layoutControls').Field('declared').AsBoolean,
      'column policy reset preserves another typed column declaration');
    LLayoutRevision := LRevision;
    LLayoutBefore := PairOracle;
    Apply('layout-author-noop-default', [TableLayoutEdit('tasks-table',
      Default(TNyxTableLayoutPolicy), True)]);
    Check((LRevision = LLayoutRevision) and (PairOracle = LLayoutBefore),
      'an already-absent layout reset creates no publication');

    LDefinitionBefore := LayoutBinding('definition-policy-table').Field('local').ToJSON;
    LSiblingBefore := LayoutBinding('second-policy-table').Field('effective').ToJSON;
    LListBefore := LayoutBinding('tasks-list').Field('local').ToJSON;
    Apply('layout-author-local-reusable', [TableLayoutEdit('first-policy-table',
      LLayoutTable, False), ColumnLayoutEdit('first-policy-table', 'caption', LLayoutColumn, False)]);
    LBinding := LayoutBinding('first-policy-table');
    Check(not LBinding.Field('inherited').AsBoolean and
      LBinding.Field('local').Field('layoutControls').Field('declared').AsBoolean and
      (LayoutBinding('definition-policy-table').Field('local').ToJSON = LDefinitionBefore) and
      (LayoutBinding('second-policy-table').Field('effective').ToJSON = LSiblingBefore) and
      (LayoutBinding('tasks-list').Field('local').ToJSON = LListBefore),
      'local reusable layout edits preserve their definition, sibling and non-table metadata');
    Apply('layout-author-default-reusable', [TableLayoutEdit('first-policy-table',
      Default(TNyxTableLayoutPolicy), True), ColumnLayoutEdit('first-policy-table', 'caption',
      Default(TNyxColumnLayoutPolicy), True)]);
    LBinding := LayoutBinding('first-policy-table');
    Check(not LBinding.Field('inherited').AsBoolean and
      not LBinding.Field('local').Field('layoutControls').Field('declared').AsBoolean,
      'use-default layout policy does not silently restore whole-binding inheritance');
    LLayoutIntent := Default(TNyxStudioCollectionIntent);
    LLayoutIntent.Action := scaClear;
    LLayoutIntent.Key := NyxCollection(LBinding.Field('effective').Field('key').AsText);
    LLayoutIntent.Projection := cpTable;
    Apply('layout-author-reusable-mask', [
      NyxCollectionIntentChange(NyxBindingOwner('first-policy-table'), LLayoutIntent)]);
    LBinding := LayoutBinding('first-policy-table');
    Check(LBinding.Field('local').Field('cleared').AsBoolean and
      LBinding.Field('effective').Field('cleared').AsBoolean and
      (LBinding.Field('restorable').Field('layoutControls').Field('policy').ToJSON =
        LayoutBinding('definition-policy-table').Field('effective')
          .Field('layoutControls').Field('policy').ToJSON),
      'a reusable clear mask retains the actual inherited layout as restorable context');
    Refuses('nyx_collections', Arguments('layout-author-mask-default', [
      NyxUseDefaultCollectionTableLayout(NyxBindingOwner('first-policy-table'),
        LLayoutIntent.Key, cpTable)]),
      'a masked reusable table cannot acquire a default policy as an implicit rebind');
    LLayoutIntent.Action := scaInherit;
    Apply('layout-author-explicit-inherit', [
      NyxCollectionIntentChange(NyxBindingOwner('first-policy-table'), LLayoutIntent)]);
    LBinding := LayoutBinding('first-policy-table');
    Check(LBinding.Field('inherited').AsBoolean and (LBinding.Field('local').Kind = ndNull) and
      (LayoutBinding('definition-policy-table').Field('local').ToJSON = LDefinitionBefore) and
      (LayoutBinding('second-policy-table').Field('effective').ToJSON = LSiblingBefore),
      'explicit whole-binding inheritance restores the reusable owner without changing its peers');

    LStale := Arguments('layout-author-stale-revision', [TableLayoutEdit('tasks-table',
      NyxTableLayout, False)]);
    Apply('layout-author-after-stale-read', [ColumnLayoutEdit('tasks-table', 'caption',
      LLayoutColumn, False)]);
    Refuses('nyx_collections', LStale, 'stale layout revisions preserve design, source and history');
    Refuses('nyx_collections', Arguments('layout-author-late-field', [
      TableLayoutEdit('tasks-table', LLayoutTable, False),
      NyxSetCollectionColumnLayout(NyxBindingOwner('tasks-table'), LKey, cpTable,
        TNyxStudioCollectionFieldRef.Text(NyxTextField('missing')), NyxColumnLayout)]),
      'a late missing layout field rolls back earlier valid changes');
    Refuses('nyx_collections', Arguments('layout-author-wrong-family', [
      NyxSetCollectionColumnLayout(NyxBindingOwner('tasks-table'), LKey, cpTable,
        TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('caption')), NyxColumnLayout)]),
      'layout names do not substitute for the exact scalar field family');
    Refuses('nyx_collections', Arguments('layout-author-wrong-key', [
      NyxSetCollectionTableLayout(NyxBindingOwner('tasks-table'), NyxCollection('wrong-key'),
        cpTable, NyxTableLayout)]), 'layout changes require the original current binding key');
    Refuses('nyx_collections', Arguments('layout-author-nontable', [
      NyxSetCollectionTableLayout(NyxBindingOwner('tasks-list'), LKey, cpTable, NyxTableLayout)]),
      'inspectable list layout metadata does not grant table authoring');

    LEditor := LAgent.PreviewPair(LRevision, 'home');
    LEditor.Pending := True;
    LEditor.DraftBase := LEditor.Source;
    LEditor.Draft := LEditor.Source;
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LEditor))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    RefreshRevision;
    Refuses('nyx_collections', Arguments('layout-author-pending-table', [
      TableLayoutEdit('tasks-table', NyxTableLayout, False)]),
      'equal-byte pending Pascal refuses a layout table set', 'pending Pascal draft');
    Refuses('nyx_collections', Arguments('layout-author-pending-column', [
      ColumnLayoutEdit('tasks-table', 'caption', NyxColumnLayout, False)]),
      'equal-byte pending Pascal refuses a layout column set', 'pending Pascal draft');
    Refuses('nyx_collections', Arguments('layout-author-pending-default-table', [
      TableLayoutEdit('tasks-table', Default(TNyxTableLayoutPolicy), True)]),
      'equal-byte pending Pascal refuses a layout table reset', 'pending Pascal draft');
    Refuses('nyx_collections', Arguments('layout-author-pending-default-column', [
      ColumnLayoutEdit('tasks-table', 'caption', Default(TNyxColumnLayoutPolicy), True)]),
      'equal-byte pending Pascal refuses a layout column reset', 'pending Pascal draft');
    LEditor.Pending := False;
    LEditor.Draft := '';
    LEditor.DraftBase := '';
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LEditor))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    RefreshRevision;
    Apply('layout-author-after-resolution', [TableLayoutEdit('tasks-table', LLayoutTable, False)]);
    Check(LayoutBinding('tasks-table').Field('local').Field('layoutControls').Field('declared').AsBoolean,
      'the original layout command succeeds after deliberate pending-source resolution');
  end;

begin
  Result := 0;
  LSeed := CreateNyxAgentCollectionSeed;
  LDocument := TNyxCodec.Decode(LSeed.Design);
  try
    { Extend only this disposable setup with an actual reusable table part.
      Original CreateSeed and its List instances remain unchanged. }
    LTable := NewNyxTable('definition-policy-table');
    LTable.Configure.PartName(NyxPart('records')).Done;
    LDocument.Find('task-card').Add(LTable);
    LTable := nil;
    LDocument.Find('first-card').OverridePart(NyxPart('records'), noProperties)
      .Named('first-policy-table');
    LDocument.Find('second-card').OverridePart(NyxPart('records'), noProperties)
      .Named('second-policy-table');
    LSeed.Design := TNyxCodec.Encode(LDocument);
    LSource := TNyxCodegen.Generate(LDocument);
    LSeed.Source := TNyxText(StringReplace(String(LSource), 'implementation' + #10,
      'implementation' + #10 + #10 +
      '{ Handwritten helper retained by collection MCP edits. }' + #10 +
      'function CollectionWorkshopNote: TNyxText;' + #10 + 'begin' + #10 +
      '  Result := ''Keep crafting.'';' + #10 + 'end;' + #10, []));
  finally
    LDocument.Free;
  end;
  LAgent := TNyxAgentSession.Create(LSeed);
  try
    RefreshRevision;
    LKey := NyxCollection('policy-author-rows');
    LSchema := NyxCollectionSchema.Text(NyxTextField('caption'), 'Task')
      .Integer(NyxIntegerField('priority'), 2, NyxIntegerDomain.Range(0, 10))
      .Boolean(NyxBooleanField('done'), False).Number(NyxNumberField('ratio'), 0);
    LSpec := NyxCollectionView(LKey).Column(NyxTextField('caption'), 'Task')
      .Column(NyxIntegerField('priority'), 'Priority').Column(NyxBooleanField('done'), 'Done')
      .Column(NyxNumberField('ratio'), 'Ratio')
      .ColumnSizing(NyxColumnSizing.Flex(1)).Paged(NyxCollectionPaging.PageSize(5))
      .Query(NyxCollectionQuery.OrderBy(NyxIntegerField('priority')));
    Apply('policy-author-setup', [NyxDefineCollection(LKey, LSchema, [
      NyxCollectionItem(NyxItem(LKey, 'one')).WithValue(NyxTextField('caption'), 'Plan')]),
      NyxBindCollection(NyxBindingOwner('tasks-table'), cpTable, LSpec),
      NyxBindCollection(NyxBindingOwner('tasks-list'), cpList,
        NyxCollectionView(LKey).Column(NyxTextField('caption'), 'List task')
          .ColumnQuery(NyxColumnQuery.Sorting(False).Operators([nqcEqual]))
          .TableQuery(NyxTableQuery.Filtering(False))),
      NyxBindCollection(NyxBindingOwner('definition-policy-table'), cpTable, LSpec.Scoped(csInstance))]);
    LList := Binding('tasks-list').Field('local').ToJSON;
    LDefinition := Binding('definition-policy-table').Field('local').ToJSON;
    LSibling := Binding('second-policy-table').Field('effective').ToJSON;
    LReply := Binding('tasks-table');
    LKey := NyxCollection(LReply.Field('effective').Field('key').AsText);
    Check(LReply.Field('queryControlsSupported').AsBoolean and
      (LReply.Field('projection').AsText = 'table') and
      not LReply.Field('effective').Field('queryControls').Field('declared').AsBoolean,
      'the agent discovers a table and its absent default policy before editing');
    LTablePolicy := TNyxTableQueryPolicy.FromData(LReply.Field('effective')
      .Field('queryControls').Field('policy'));
    Apply('policy-author-explicit-default', [TableEdit('tasks-table', LTablePolicy, False)]);
    Check(Binding('tasks-table').Field('local').Field('queryControls').Field('declared').AsBoolean,
      'an explicit library-default object remains an authored declaration');
    Apply('policy-author-default-table', [TableEdit('tasks-table', Default(TNyxTableQueryPolicy), True)]);
    LReply := Binding('tasks-table');
    Check(not LReply.Field('local').Field('queryControls').Field('declared').AsBoolean and
      not LReply.Field('inherited').AsBoolean and
      (LReply.Field('local').Field('queryControls').Field('policy').ToJSON = LTablePolicy.ToData.ToJSON),
      'null reset removes only the table declaration and retains its local binding');
    LBefore := PairOracle;
    LOldRevision := LRevision;
    Apply('policy-author-noop-default', [TableEdit('tasks-table', Default(TNyxTableQueryPolicy), True)]);
    Check((LRevision = LOldRevision) and (PairOracle = LBefore),
      'an already-default reset does not invent a publication');

    LFields := Binding('tasks-table').Field('effective').Field('columns');
    for LIndex := 0 to LFields.Count - 1 do
    begin
      LFieldName := LFields.Item(LIndex).Field('field').AsText;
      LPolicy := TNyxColumnQueryPolicy.FromData(LFields.Item(LIndex)
        .Field('queryControls').Field('policy')).Sorting(False).Operators([nqcEqual]);
      Apply('policy-author-family-' + TNyxText(IntToStr(LIndex)),
        [ColumnEdit('tasks-table', LFieldName, LPolicy, False)]);
      LReply := Read('column', [NyxField('owner', NyxData('tasks-table')),
        NyxField('source', NyxData('local')), NyxField('field', NyxData(LFieldName)),
        NyxField('queryControls', NyxData(True))]);
      Check(LReply.Field('queryControls').Field('declared').AsBoolean and
        (LReply.Field('queryControls').Field('policy').ToJSON = LPolicy.ToData.ToJSON),
        'bounded field identity admits a policy for scalar family ' + LReply.Field('kind').AsText);
    end;
    Apply('policy-author-disabled-empty', [ColumnEdit('tasks-table', 'done',
      NyxColumnQuery.Filtering(False).Operators([]), False)]);
    LReply := Read('column', [NyxField('owner', NyxData('tasks-table')),
      NyxField('source', NyxData('local')), NyxField('field', NyxData('done')),
      NyxField('queryControls', NyxData(True))]);
    Check(not LReply.Field('queryControls').Field('policy').Field('filtering').AsBoolean and
      (LReply.Field('queryControls').Field('policy').Field('operators').Count = 0),
      'disabled explicit-empty operators survive the agent-only edit');
    Apply('policy-author-default-column', [ColumnEdit('tasks-table', 'done',
      Default(TNyxColumnQueryPolicy), True)]);
    LReply := Binding('tasks-table');
    Check(not LReply.Field('local').Field('columns').Item(2).Field('queryControls')
      .Field('declared').AsBoolean and
      LReply.Field('local').Field('columns').Item(1).Field('queryControls').Field('declared').AsBoolean,
      'null column reset retains independently declared peer columns');

    Apply('policy-author-reusable-local', [TableEdit('first-policy-table',
      NyxTableQuery.Sorting(False), False), ColumnEdit('first-policy-table', 'caption',
      NyxColumnQuery.TextComparison(nqtAsciiInsensitive).Operators([nqcEqual, nqcContains]), False)]);
    LReply := Binding('first-policy-table');
    Check(not LReply.Field('inherited').AsBoolean and
      not LReply.Field('local').Field('queryControls').Field('policy').Field('sorting').AsBoolean and
      (Binding('definition-policy-table').Field('local').ToJSON = LDefinition) and
      (Binding('second-policy-table').Field('effective').ToJSON = LSibling) and
      (Binding('tasks-list').Field('local').ToJSON = LList),
      'local reusable edits preserve definition, sibling and saved non-table policy metadata');
    Apply('policy-author-reusable-default', [TableEdit('first-policy-table',
      Default(TNyxTableQueryPolicy), True), ColumnEdit('first-policy-table', 'caption',
      Default(TNyxColumnQueryPolicy), True)]);
    LReply := Binding('first-policy-table');
    Check(not LReply.Field('inherited').AsBoolean and
      not LReply.Field('local').Field('queryControls').Field('declared').AsBoolean and
      not LReply.Field('local').Field('columns').Item(0).Field('queryControls').Field('declared').AsBoolean,
      'defaulting a local reusable choice does not restore whole-binding inheritance');

    { The first group changes binding, geometry, paging and schema metadata.
      Both named policies in the second group must derive their own current
      baselines, including changes made by the preceding policy in that group. }
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaColumnFixed;
    LIntent.Key := LKey;
    LIntent.Projection := cpTable;
    LIntent.Field := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('priority'));
    LIntent.Value := '196';
    LPageIntent := Default(TNyxStudioCollectionIntent);
    LPageIntent.Action := scaPageSize;
    LPageIntent.Key := LKey;
    LPageIntent.Projection := cpTable;
    LPageIntent.Value := '3';
    LInitialIntent := LPageIntent;
    LInitialIntent.Action := scaInitialPage;
    LInitialIntent.Value := '2';
    LReference := FieldReference(Read('column', [NyxField('owner', NyxData('tasks-table')),
      NyxField('source', NyxData('effective')), NyxField('field', NyxData('priority'))]));
    LPolicy := NyxColumnQuery.Filtering(False).Sorting(False).Operators([nqcEqual, nqcAtLeast]);
    LBefore := PairOracle;
    LRequest := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('policy-author-ordered')),
      NyxField('operations', NyxArray([
        NyxObject([NyxField('op', NyxData('collections')), NyxField('changes', NyxCollectionPatch([
          NyxBindCollection(NyxBindingOwner('tasks-table'), cpTable, LSpec),
          NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LIntent),
          NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LPageIntent),
          NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LInitialIntent),
          NyxSetCollectionField(LKey, NyxCollectionSchema.Integer(NyxIntegerField('priority'),
            5, NyxIntegerDomain.Range(0, 20)))]).ToData)]),
        NyxObject([NyxField('op', NyxData('collections')), NyxField('changes', NyxCollectionPatch([
          NyxSetCollectionTableQuery(NyxBindingOwner('tasks-table'), LKey, cpTable,
            NyxTableQuery.Sorting(False)),
          NyxSetCollectionColumnQuery(NyxBindingOwner('tasks-table'), LKey, cpTable,
            LReference, LPolicy)]).ToData)])]))]);
    LAgent.Call('nyx_transaction', 'Policy author', LRequest, 'policy-author');
    RefreshRevision;
    LAfter := PairOracle;
    LEditor := LAgent.PreviewPair(LRevision, 'home');
    LDocument := TNyxCodec.Decode(LEditor.Design);
    try
      LOutputSpec := LDocument.Find('tasks-table').CollectionView;
      Check((LOutputSpec.ColumnAt(1).Sizing.Value = 196) and
        (LOutputSpec.PagingPolicy.ItemsPerPage = 3) and (LOutputSpec.PagingPolicy.FirstPage = 2) and
        (LDocument.Collections.Snapshot(LKey).Schema.FieldAt(1).DefaultValue.IntegerValue = 5) and
        (LDocument.Collections.Snapshot(LKey).Schema.FieldAt(1).Domain.ToData.ToJSON =
          NyxIntegerDomain.Range(0, 20).Definition.ToData.ToJSON) and
        (LOutputSpec.QueryPolicy.ToData.ToJSON = LSpec.QueryPolicy.ToData.ToJSON) and
        (LOutputSpec.ColumnAt(1).QueryPolicy.ToData.ToJSON = LPolicy.ToData.ToJSON) and
        not LOutputSpec.TableQueryPolicy.SortEnabled and
        (Pos('CollectionWorkshopNote', LEditor.Source) > 0),
        'ordered fresh baselines preserve earlier sizing/paging/domain/query and handwritten source');
    finally
      LDocument.Free;
    end;
    Refuses('nyx_collections', Arguments('policy-author-late-failure', [
      TableEdit('tasks-table', NyxTableQuery.Filtering(False), False),
      NyxSetCollectionColumnQuery(NyxBindingOwner('tasks-table'), LKey, cpTable,
        TNyxStudioCollectionFieldRef.Text(NyxTextField('absent')), NyxColumnQuery)]),
      'a late invalid field rolls back an earlier valid policy edit');
    History('undo', 'policy-author-undo-ordered');
    Check(PairOracle = LBefore, 'one Undo restores the entire successful transaction after a late refusal');
    History('redo', 'policy-author-redo-ordered');
    Check(PairOracle = LAfter, 'one Redo restores the exact ordered design and source');

    Refuses('nyx_collections', Arguments('policy-author-key', [
      NyxSetCollectionTableQuery(NyxBindingOwner('tasks-table'), NyxCollection('wrong-key'),
        cpTable, NyxTableQuery)]), 'a changed current collection key refuses');
    Refuses('nyx_collections', Arguments('policy-author-family', [
      NyxSetCollectionColumnQuery(NyxBindingOwner('tasks-table'), LKey, cpTable,
        TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('caption')), NyxColumnQuery)]),
      'a changed current field family refuses');
    Refuses('nyx_collections', Arguments('policy-author-nontable', [
      NyxSetCollectionTableQuery(NyxBindingOwner('tasks-list'), LKey, cpTable, NyxTableQuery)]),
      'inspectable saved List metadata does not grant table policy authoring');
    LIntent.Action := scaRemoveColumn;
    LIntent.Value := '';
    Refuses('nyx_collections', Arguments('policy-author-removed-column', [
      NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LIntent),
      NyxSetCollectionColumnQuery(NyxBindingOwner('tasks-table'), LKey, cpTable,
        LReference, NyxColumnQuery)]), 'a column removed earlier in the group cannot receive a policy');
    LClear := Default(TNyxStudioCollectionIntent);
    LClear.Action := scaClear;
    LClear.Key := LKey;
    LClear.Projection := cpTable;
    Refuses('nyx_collections', Arguments('policy-author-clear-refusal', [
      NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LClear),
      NyxUseDefaultCollectionTableQuery(NyxBindingOwner('tasks-table'), LKey, cpTable)]),
      'a clear mask earlier in the group cannot acquire a policy default');
    Apply('policy-author-clear-rebind', [
      NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LClear),
      NyxBindCollection(NyxBindingOwner('tasks-table'), cpTable, LSpec),
      TableEdit('tasks-table', NyxTableQuery.Filtering(False), False)]);
    Check(not Binding('tasks-table').Field('local').Field('queryControls').Field('policy')
      .Field('filtering').AsBoolean, 'a fresh rebind earlier in the group supplies the named policy target');
    LRequest := Arguments('policy-author-stale', [TableEdit('tasks-table', NyxTableQuery, False)]);
    Apply('policy-author-after-read', [ColumnEdit('tasks-table', 'caption',
      NyxColumnQuery.Sorting(False), False)]);
    Refuses('nyx_collections', LRequest, 'a stale revision cannot reapply a previously read policy');
    Refuses('nyx_collections', NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('policy-author-malformed')),
      NyxField('changes', NyxArray([NyxObject([NyxField('op', NyxData('table-query')),
        NyxField('owner', NyxData('tasks-table')), NyxField('key', NyxData(LKey.Name)),
        NyxField('projection', NyxData('table')), NyxField('policy', NyxData('default'))])]))]),
      'a policy string cannot masquerade as a typed object or null reset');
    Refuses('nyx_collections', NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('policy-author-unused-baseline')),
      NyxField('changes', NyxArray([NyxObject([NyxField('op', NyxData('table-query')),
        NyxField('owner', NyxData('tasks-table')), NyxField('key', NyxData(LKey.Name)),
        NyxField('projection', NyxData('table')), NyxField('policy', NyxNull),
        NyxField('baseline', NyxData('not an agent input'))])]))]),
      'the named operation refuses an unused fabricated form baseline');
    Check((Binding('tasks-list').Field('local').ToJSON = LList) and
      (Binding('definition-policy-table').Field('local').ToJSON = LDefinition) and
      (Binding('second-policy-table').Field('effective').ToJSON = LSibling),
      'ordered edits and refusals retain independent List/definition/sibling metadata');

    { Private editor transport establishes a deliberate pending frame. Its text
      equals accepted text; policy inputs still come only from semantic reads.
      The authoritative files flags, not the byte-derived session summary,
      establish why the ordinary query-intent guard must refuse publication. }
    LEditor := LAgent.PreviewPair(LRevision, 'home');
    LEditor.Pending := True;
    LEditor.DraftBase := LEditor.Source;
    LEditor.Draft := LEditor.Source;
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LEditor))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    RefreshRevision;
    LFiles := LAgent.Call('nyx_pascal', 'Policy author', NyxObject([
      NyxField('mode', NyxData('files')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('limit', NyxData(1))]));
    Check(LFiles.Field('pendingDrafts').AsBoolean and
      LFiles.Field('files').Item(0).Field('pendingDraft').AsBoolean and
      (LAgent.PreviewPair(LRevision, 'home').Draft = LEditor.Source),
      'ordinary file inspection retains the explicit equal-bytes pending frame');
    Refuses('nyx_collections', Arguments('policy-author-pending-table', [
      TableEdit('tasks-table', NyxTableQuery, False)]),
      'equal-bytes pending source refuses a table set through ordinary intent admission', 'pending Pascal draft');
    Refuses('nyx_collections', Arguments('policy-author-pending-column', [
      ColumnEdit('tasks-table', 'caption', NyxColumnQuery, False)]),
      'equal-bytes pending source refuses a column set through ordinary intent admission', 'pending Pascal draft');
    Refuses('nyx_collections', Arguments('policy-author-pending-default-table', [
      TableEdit('tasks-table', Default(TNyxTableQueryPolicy), True)]),
      'equal-bytes pending source refuses a table default', 'pending Pascal draft');
    Refuses('nyx_collections', Arguments('policy-author-pending-default-column', [
      ColumnEdit('tasks-table', 'caption', Default(TNyxColumnQueryPolicy), True)]),
      'equal-bytes pending source refuses a column default', 'pending Pascal draft');
    LEditor.Pending := False;
    LEditor.Draft := '';
    LEditor.DraftBase := '';
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LEditor))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    RefreshRevision;
    Apply('policy-author-after-resolution', [TableEdit('tasks-table', NyxTableQuery, False)]);
    Check(Binding('tasks-table').Field('local').Field('queryControls').Field('declared').AsBoolean,
      'the same named command can publish after deliberate editor draft resolution');
    LayoutCases;
  finally
    LAgent.Free;
  end;
end;

{ END QUERY_POLICY_EDIT_CASES }
function RunNyxAgentCollectionJourney(out APair: TNyxProjectPair;
  AQueriesOnly: Boolean): Integer;
var
  LAgent: TNyxAgentSession;
  LSeed: TNyxProjectPair;
  LKey: TNyxCollectionRef;
  LQualification: TNyxText;
  LLong: TNyxText;
  LChoices: array of TNyxText;
  LBefore: TNyxText;
  LAdded: TNyxText;
  LReceipt: TNyxDataValue;
  LArgs: TNyxDataValue;
  LReply: TNyxDataValue;
  LSchema: TNyxCollectionSchema;
  LSpec: TNyxCollectionViewSpec;
  LIntent: TNyxStudioCollectionIntent;
  LChange: TNyxCollectionChange;
  LPatch: INyxCollectionPatch;
  LRevision: Integer;
  LIndex: Integer;
  LCurrent: TNyxProjectPair;
  LDocument: TNyxDocument;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxCollection.Create('Semantic collections: ' + AReason);
    end;
    Inc(Result);
  end;

  function PairText: TNyxText;
  begin
    Result := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
  end;

  function Args(const AID: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData(AID)),
      NyxField('changes', AChanges)]);
  end;

  function Apply(const AID: TNyxText; const AChanges: array of TNyxCollectionChange):
    TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_collections', 'Scooty',
      Args(AID, NyxCollectionPatch(AChanges).ToData), 'collection-owner');
    LRevision := LAgent.Revision;
  end;

  function Query(const AMode: TNyxText; const AFields: array of TNyxDataField):
    TNyxDataValue;
  var
    LMembers: array of TNyxDataField;
    LMember: Integer;
  begin
    SetLength(LMembers, Length(AFields) + 1);
    LMembers[0] := NyxField('mode', NyxData(AMode));
    for LMember := 0 to High(AFields) do
    begin
      LMembers[LMember + 1] := AFields[LMember];
    end;
    Result := LAgent.Call('nyx_collections', 'Scooty', NyxObject(LMembers));
  end;

  procedure Refuses(const AArguments: TNyxDataValue; const AReason: TNyxText;
    const AAuthority: TNyxText = 'collection-owner');
  var
    LSnapshot: TNyxText;
    LState: TNyxDataValue;
    LAfter: TNyxDataValue;
    LRejected: Boolean;
  begin
    LSnapshot := PairText;
    LState := LAgent.Call('nyx_session', 'Scooty', NyxObject([]));
    LRejected := False;
    try
      LAgent.Call('nyx_collections', 'Scooty', AArguments, AAuthority);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    LAfter := LAgent.Call('nyx_session', 'Scooty', NyxObject([]));
    Check(LRejected and (LAgent.Revision = LRevision) and (PairText = LSnapshot) and
      (LState.Field('canUndo').AsBoolean = LAfter.Field('canUndo').AsBoolean) and
      (LState.Field('canRedo').AsBoolean = LAfter.Field('canRedo').AsBoolean) and
      (LState.Field('selection').AsText = LAfter.Field('selection').AsText),
      AReason);
  end;

  procedure History(const ADirection, AID: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData(AID)),
      NyxField('direction', NyxData(ADirection))]));
    LRevision := LAgent.Revision;
  end;

  { Exact ordinary intent round trips execute on independent candidates. This
    covers every closed choice without replacing or clicking through Studio. }
  procedure OrdinaryIntents;
  var
    LAction: TNyxStudioCollectionAction;
    LActionPair: TNyxProjectPair;
    LActionDocument: TNyxDocument;
    LEncoded: TNyxDataValue;
    LNote: TNyxText;
  begin
    LCurrent := LAgent.PreviewPair(LRevision, 'home');
    for LAction := Low(TNyxStudioCollectionAction) to High(TNyxStudioCollectionAction) do
    begin
      LIntent := Default(TNyxStudioCollectionIntent);
      LIntent.Action := LAction;
      LIntent.Key := LKey;
      LIntent.Projection := cpTable;
      case LAction of
        scaCreate, scaRemove:
          begin
            LIntent.Key := NyxCollection('scratch');
            LIntent.Projection := cpList;
          end;
        scaAddField:
          begin
            LIntent.Kind := nskInteger;
            LIntent.Projection := cpList;
          end;
        scaDefault, scaCell:
          begin
            LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('caption'));
            LIntent.Value := 'Crafted task';
            LIntent.Projection := cpList;

            if LAction = scaCell then
            begin
              LIntent.Item := NyxItem(LKey, 'alpha');
            end;
          end;
        scaAddRow:
          begin
            LIntent.Item := NyxItem(LKey, 'new-task');
            LIntent.Projection := cpList;
          end;
        scaRemoveRow:
          begin
            LIntent.Item := NyxItem(LKey, 'beta');
            LIntent.Projection := cpList;
          end;
        scaScope:
          begin
            LIntent.Scope := csInstance;
          end;
        scaTitle:
          begin
            LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('caption'));
            LIntent.Value := 'Task details';
          end;
        scaMode:
          begin
            LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('caption'));
            LIntent.Mode := cmReadOnly;
          end;
        scaParent:
          begin
            LIntent.Projection := cpTree;
            LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('parent'));
          end;
        scaRemoveColumn:
          begin
            LIntent.Field := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('priority'));
          end;
        scaAddColumn:
          begin
            LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('parent'));
          end;
        scaSelection:
          begin
            LIntent.SelectionMode := nsmMultiple;
          end;
        scaQuery:
          begin
            LActionDocument := TNyxCodec.Decode(LCurrent.Design);
            try
              LIntent.Query := NyxCollectionQuery.OrderBy(NyxIntegerField('priority'));
              LIntent.QueryBaseline := NyxQueryEditorBaseline(
                LActionDocument.Collections.Snapshot(LKey).Schema,
                LActionDocument.Find('tasks-table').CollectionView);
            finally
              LActionDocument.Free;
            end;
          end;
        { BEGIN CURRENT_INTENT_PAYLOADS }
        scaColumnContent, scaColumnFixed, scaColumnFlex, scaColumnMinimum,
        scaColumnMaximum, scaColumnDefault:
          begin
            LIntent.Field := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('priority'));
            case LAction of
              scaColumnFixed: LIntent.Value := '180';
              scaColumnFlex: LIntent.Value := '2';
              scaColumnMinimum: LIntent.Value := '80';
              scaColumnMaximum: LIntent.Value := '480';
            else
              begin
                { Content/default sizing have no numeric payload. }
              end;
            end;
          end;
        scaPageSize: LIntent.Value := '2';
        scaInitialPage: LIntent.Value := '1';
        scaDefaultPaging:
          begin
            { Removing an absent paging choice is still a valid typed intent. }
          end;
        scaTableQuery, scaColumnQuery, scaDefaultTableQuery, scaDefaultColumnQuery:
          begin
            LActionDocument := TNyxCodec.Decode(LCurrent.Design);
            try
              LIntent.QueryBaseline := NyxQueryEditorBaseline(
                LActionDocument.Collections.Snapshot(LKey).Schema,
                LActionDocument.Find('tasks-table').CollectionView);
            finally
              LActionDocument.Free;
            end;

            if LAction = scaTableQuery then
            begin
              LIntent.TableQuery := NyxTableQuery.Filtering(False);
            end;

            if LAction in [scaColumnQuery, scaDefaultColumnQuery] then
            begin
              LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('caption'));
            end;

            if LAction = scaColumnQuery then
            begin
              LIntent.ColumnQuery := NyxColumnQuery.Operators([nqcEqual]);
            end;
          end;
        scaTableLayout, scaColumnLayout, scaDefaultTableLayout, scaDefaultColumnLayout:
          begin
            { Layout commands carry their own complete binding/schema baseline;
              they must not borrow the independent query editor's draft proof. }
            LActionDocument := TNyxCodec.Decode(LCurrent.Design);
            try
              LIntent.LayoutBaseline := NyxQueryEditorBaseline(
                LActionDocument.Collections.Snapshot(LKey).Schema,
                LActionDocument.Find('tasks-table').CollectionView);
            finally
              LActionDocument.Free;
            end;

            if LAction in [scaColumnLayout, scaDefaultColumnLayout] then
            begin
              LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('caption'));
            end;

            if LAction = scaTableLayout then
            begin
              LIntent.TableLayout := NyxTableLayout.MinimumCenter(112).AutoSizeSample(32)
                .AllowOnly([nclfResize, nclfAutoSize, nclfPinning]);
            end;

            if LAction = scaColumnLayout then
            begin
              LIntent.ColumnLayout := NyxColumnLayout.PinEnd
                .AllowOnly([nclfResize, nclfPinning]);
            end;
          end;
        { END CURRENT_INTENT_PAYLOADS }
        scaBind, scaClear, scaInherit:
          begin
            { These actions use the already supplied exact key/owner/projection. }
          end;
      end;
      LActionPair := LCurrent;

      if LAction = scaRemove then
      begin
        LActionPair := NyxCollectionPatch([NyxDefineCollection(NyxCollection('scratch'),
          NyxCollectionSchema.Text(NyxTextField('caption'), ''), [])]).Candidate(LActionPair);
      end;

      if NyxStudioCollectionViewAction(LAction) then
      begin

        if LAction = scaParent then
        begin
          LChange := NyxCollectionIntentChange(NyxBindingOwner('tasks-tree'), LIntent);
        end
        else
        begin
          LChange := NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LIntent);
        end;
      end
      else
      begin
        LChange := NyxCollectionIntentChange(LIntent);
      end;
      LPatch := NyxCollectionPatch([LChange]);
      LEncoded := LPatch.ToData;
      Check(ReadNyxCollectionPatch(LEncoded).ToData.ToJSON = LEncoded.ToJSON,
        'ordinary typed action wire ' + IntToStr(Ord(LAction)));
      LActionPair := ReadNyxCollectionPatch(LEncoded).Candidate(LActionPair);
      LActionDocument := TNyxCodec.Decode(LActionPair.Design);
      try
        LActionDocument.Validate;
        LNote := LActionPair.Source;
        Check((Pos('CollectionWorkshopNote', LNote) > 0) and
          (EncodeNyxProject(LCurrent) = PairText),
          'ordinary isolated command/helper ownership ' + IntToStr(Ord(LAction)));
      finally
        LActionDocument.Free;
      end;
    end;
  end;

  { Query-only operations preserve unrelated binding fields and row data. Pages
    carry previews/child paths; one requested value window carries exact text.
    This exercises the actual revision/permission/candidate protocol boundary. }
  procedure QueryPolicies;
  var
    LOriginal: TNyxText;
    LChanged: TNyxText;
    LOriginalSpec: TNyxText;
    LPolicy: TNyxCollectionQuery;
    LQueryReply: TNyxDataValue;
    LStale: TNyxDataValue;
    LDocument: TNyxDocument;
  begin
    LOriginal := PairText;
    LDocument := TNyxCodec.Decode(LAgent.PreviewPair(LRevision, 'home').Design);
    try
      LOriginalSpec := LDocument.Find('tasks-table').CollectionView.ToData.ToJSON;
    finally
      LDocument.Free;
    end;
    LPolicy := NyxCollectionQuery.Where(NyxWhere(NyxTextField('caption')).Contains(LLong)
      .AndAlso(NyxWhere(NyxIntegerField('priority')).AtLeast(2)
        .OrElse(NyxWhere(NyxBooleanField('done')).EqualTo(False).Negated)))
      .OrderBy(NyxIntegerField('priority'), nsdDescending)
      .ThenBy(NyxTextField('caption'), nsdAscending, nqtAsciiInsensitive);
    LPatch := NyxCollectionPatch([NyxSetCollectionQuery(
      NyxBindingOwner('tasks-table'), LKey, cpTable, LPolicy)]);
    Check(ReadNyxCollectionPatch(LPatch.ToData).ToData.ToJSON = LPatch.ToData.ToJSON,
      'query-only operation round trips without expanded binding or form baseline');
    LStale := Args('stale-query', LPatch.ToData);
    Apply('query-policy', [NyxSetCollectionQuery(
      NyxBindingOwner('tasks-table'), LKey, cpTable, LPolicy)]);
    LChanged := PairText;
    LQueryReply := Query('query', [NyxField('owner', NyxData('tasks-table')),
      NyxField('source', NyxData('effective')), NyxField('limit', NyxData(3))]).Field('query');
    Check((LQueryReply.Field('total').AsInteger = 6) and
      (LQueryReply.Field('nodes').Count = 3) and
      (LQueryReply.Field('nextOffset').AsInteger = 3), 'bounded preorder predicate page');
    Check((LQueryReply.Field('order').Count = 2) and
      (Length(LQueryReply.ToJSON) < 1500) and
      LQueryReply.Field('nodes').Item(1).Field('expected').Field('truncated').AsBoolean,
      'small query page excludes long predicate value, schema, columns and rows');
    Check(LQueryReply.Field('nodes').Item(1).Field('path').ToJSON = '[0]',
      'value-only exact child path identifies one predicate');
    LQueryReply := Query('query-value', [NyxField('owner', NyxData('tasks-table')),
      NyxField('source', NyxData('effective')), NyxField('path', NyxArray([NyxData(0)])),
      NyxField('offset', NyxData(4999)), NyxField('count', NyxData(2))]);
    Check(LQueryReply.Field('value').Field('text').AsText = TNyxText('x🌙'),
      'requested predicate window retains exact supplementary Unicode');
    LQueryReply := Query('query-value', [NyxField('owner', NyxData('tasks-table')),
      NyxField('source', NyxData('local')),
      NyxField('path', NyxArray([NyxData(1), NyxData(0)]))]);
    Check(LQueryReply.Field('value').Field('value').AsInteger = 2,
      'numeric query values remain exact primitives');
    LQueryReply := Query('query', [NyxField('owner', NyxData('tasks-table')),
      NyxField('source', NyxData('effective')), NyxField('offset', NyxData(3))]).Field('query');
    Check((LQueryReply.Field('nodes').Count = 3) and
      (LQueryReply.Field('nodes').Item(1).Field('op').AsText = 'not'),
      'second page retains preorder and nested operators');
    LQueryReply := Query('query', [NyxField('owner', NyxData('tasks-table')),
      NyxField('source', NyxData('effective')), NyxField('offset', NyxData(64))]).Field('query');
    Check((LQueryReply.Field('nodes').Count = 0) and
      (LQueryReply.Field('nextOffset').AsInteger = 6), 'query end page is bounded');
    Refuses(LStale, 'stale query-only edit cannot change the pair/history');
    Refuses(Args('wrong-query-family', NyxCollectionPatch([NyxSetCollectionQuery(
      NyxBindingOwner('tasks-table'), LKey, cpTable,
      NyxCollectionQuery.Where(NyxWhere(NyxTextField('priority')).EqualTo('2')))]).ToData),
      'schema family disagreement refuses query replacement');
    Refuses(Args('late-query-failure', NyxCollectionPatch([
      NyxUpdateCollectionRow(NyxCollectionItem(NyxItem(LKey, 'alpha'))
        .WithValue(NyxTextField('caption'), 'Unpublished')),
      NyxSetCollectionQuery(NyxBindingOwner('tasks-table'), NyxCollection('Tasks'),
        cpTable, LPolicy)]).ToData), 'late query failure rolls back earlier row change');
    Refuses(NyxObject([NyxField('mode', NyxData('query-value')),
      NyxField('owner', NyxData('tasks-table')), NyxField('source', NyxData('effective')),
      NyxField('path', NyxArray([]))]), 'branch path refuses a scalar value request');
    History('undo', 'query-undo');
    Check(PairText = LOriginal, 'query-only replacement is one exact paired Undo');
    History('redo', 'query-redo');
    Check(PairText = LChanged, 'query-only replacement is one exact paired Redo');
    Apply('query-clear', [NyxSetCollectionQuery(
      NyxBindingOwner('tasks-table'), LKey, cpTable, NyxCollectionQuery)]);
    LDocument := TNyxCodec.Decode(LAgent.PreviewPair(LRevision, 'home').Design);
    try
      Check(LDocument.Find('tasks-table').CollectionView.ToData.ToJSON = LOriginalSpec,
        'clearing query preserves exact query-free binding specification');
    finally
      LDocument.Free;
    end;
    Check(PairText = LOriginal, 'clearing policy preserves exact original design/source/rows');
    Apply('inherited-query', [NyxSetCollectionQuery(
      NyxBindingOwner('first-items'), LKey, cpList, LPolicy)]);
    Check(Query('query', [NyxField('owner', NyxData('first-items')),
      NyxField('source', NyxData('local'))]).Field('query').Field('defined').AsBoolean,
      'inherited owner acquires its independent local query');
    Check(not Query('query', [NyxField('owner', NyxData('definition-list')),
      NyxField('source', NyxData('local'))]).Field('query').Field('defined').AsBoolean,
      'query override preserves reusable definition policy');
    History('undo', 'inherited-query-undo');
    Check(PairText = LOriginal, 'inherited query Undo restores exact original pair');
  end;

  procedure Contexts;
  var
    LWorkspaces: TNyxStudioWorkspaces;
    LReviews: TNyxReviewWorkspaces;
    LWorkspace: TNyxWorkspaceRef;
    LReview: TNyxReviewRef;
    LRequest: TNyxDataValue;
    LResult: TNyxDataValue;
    LPrimary: TNyxText;
    LRejected: Boolean;
  begin
    LPrimary := PairText;
    LWorkspaces := TNyxStudioWorkspaces.Create(LAgent, 'collection-context-fixture');
    LReviews := TNyxReviewWorkspaces.Create(LAgent);
    try
      LWorkspace := LWorkspaces.OpenProject('Independent collection workshop',
        LAgent.PreviewPair(LRevision, 'home'));
      LRequest := NyxWithWorkspace(NyxObject([NyxField('mode', NyxData('apply')),
        NyxField('expectedRevision', NyxData(LWorkspaces.Resolve(LWorkspace).Revision)),
        NyxField('operationId', NyxData('context-edit')),
        NyxField('changes', NyxCollectionPatch([NyxUpdateCollectionRow(
          NyxCollectionItem(NyxItem(LKey, 'alpha')).WithValue(
            NyxTextField('caption'), 'Workspace task'))]).ToData)]), LWorkspace);
      LResult := LWorkspaces.Call('nyx_collections', 'context-owner', 'Scooty', LRequest);
      Check(LResult.Field('workspace').AsText = LWorkspace.ID,
        'mutation receipt retains explicit workspace');
      LResult := LWorkspaces.Call('nyx_collections', 'context-owner', 'Scooty',
        NyxWithWorkspace(NyxObject([NyxField('mode', NyxData('value')),
          NyxField('key', NyxData('tasks')), NyxField('field', NyxData('caption')),
          NyxField('item', NyxData('alpha'))]), LWorkspace));
      Check(LResult.Field('value').Field('text').AsText = 'Workspace task',
        'row query targets explicit independent workspace');
      LWorkspaces.ReleaseOwner('context-owner');
      Check(LWorkspaces.Resolve(LWorkspace).Revision > 1,
        'project collection pair survives disconnection');
      LReview := LReviews.CreateReview('review-owner', 'Scooty', 'Owned collection review',
        nrbAccepted, LRevision);
      LRequest := NyxWithReview(NyxObject([NyxField('mode', NyxData('apply')),
        NyxField('expectedRevision', NyxData(LReviews.Resolve('review-owner', LReview).Revision)),
        NyxField('operationId', NyxData('context-edit')),
        NyxField('changes', NyxCollectionPatch([NyxUpdateCollectionRow(
          NyxCollectionItem(NyxItem(LKey, 'alpha')).WithValue(
            NyxTextField('caption'), 'Review task'))]).ToData)]), LReview);
      LResult := LReviews.Call('nyx_collections', 'review-owner', 'Scooty', LRequest);
      Check(LResult.Field('review').AsText = LReview.ID,
        'same operation ID stays scoped to independent review');
      LRejected := False;
      try
        LReviews.Call('nyx_collections', 'foreign-owner', 'Scooty',
          NyxWithReview(NyxObject([NyxField('mode', NyxData('list'))]), LReview));
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'foreign review refuses collection inspection');
      LReviews.Discard('review-owner', LReview, LResult.Field('revision').AsInteger);
      LRejected := False;
      try
        LReviews.Call('nyx_collections', 'review-owner', 'Scooty', LRequest);
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (PairText = LPrimary), 'retired review refuses without primary fallback');
      LWorkspaces.CloseProject(LWorkspace, LWorkspaces.Resolve(LWorkspace).Revision, True);
      LRejected := False;
      try
        LWorkspaces.Call('nyx_collections', 'context-owner', 'Scooty',
          NyxWithWorkspace(NyxObject([NyxField('mode', NyxData('list'))]), LWorkspace));
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (PairText = LPrimary), 'retired project refuses without implicit retargeting');
    finally
      LReviews.Free;
      LWorkspaces.Free;
    end;
  end;

begin
  Result := 0;
  { BEGIN QUERY_CONTROL_READ_CALL }
  Inc(Result, RunQueryControlReadCases);
  { END QUERY_CONTROL_READ_CALL }
  { BEGIN QUERY_POLICY_EDIT_CALL }
  Inc(Result, RunQueryPolicyAgentEdits);
  { END QUERY_POLICY_EDIT_CALL }
  LSeed := CreateNyxAgentCollectionSeed;
  LAgent := TNyxAgentSession.Create(LSeed);
  try
    LRevision := LAgent.Revision;
    LKey := NyxCollection('tasks');
    LQualification := TNyxText('A🌙') + #0 + TNyxText('éZ');
    LLong := TNyxText(StringOfChar('x', 5000)) + TNyxText('🌙');
    SetLength(LChoices, 10);
    for LIndex := 0 to High(LChoices) do
    begin
      LChoices[LIndex] := LLong + TNyxText(IntToStr(LIndex));
    end;
    LSchema := NyxCollectionSchema.Text(NyxTextField('caption'), 'New task')
      .Integer(NyxIntegerField('priority'), 2, NyxIntegerDomain.Range(0, 10))
      .Boolean(NyxBooleanField('done'), False)
      .Number(NyxNumberField('ratio'), 0.125)
      .Text(NyxTextField('parent'), '');
    LSpec := NyxCollectionView(LKey).Column(NyxIntegerField('priority'), 'Priority', cmEditable)
      .Column(NyxTextField('caption'), 'Task', cmEditable)
      .Column(NyxBooleanField('done'), 'Done', cmEditable)
      .Column(NyxNumberField('ratio'), 'Ratio', cmEditable);

    if AQueriesOnly then
    begin
      { A focused consumer keeps the complete changed query boundary, including
        long exact text windows, without replaying unrelated large domain/source
        fixtures inside the browser's navigation or debugger-command budget. }
      Apply('query-seed', [NyxDefineCollection(LKey, LSchema, [
        NyxCollectionItem(NyxItem(LKey, 'alpha')).WithValue(NyxTextField('caption'), 'Plan'),
        NyxCollectionItem(NyxItem(LKey, 'beta')).WithValue(NyxTextField('caption'), 'Review')]),
        NyxBindCollection(NyxBindingOwner('tasks-table'), cpTable, LSpec),
        NyxBindCollection(NyxBindingOwner('definition-list'), cpList,
          NyxCollectionView(LKey).Scoped(csInstance).Column(NyxTextField('caption'), 'Task'))]);
      QueryPolicies;
      APair := LAgent.PreviewPair(LRevision, 'home');
      Exit;
    end;
    LBefore := PairText;
    LPatch := NyxCollectionPatch([
      NyxDefineCollection(LKey, LSchema, [
        NyxCollectionItem(NyxItem(LKey, 'alpha')).WithValue(NyxTextField('caption'), 'Plan'),
        NyxCollectionItem(NyxItem(LKey, 'beta')).WithValue(NyxTextField('caption'), 'Review')
          .WithValue(NyxTextField('parent'), 'alpha')]),
      NyxBindCollection(NyxBindingOwner('tasks-table'), cpTable, LSpec),
      NyxBindCollection(NyxBindingOwner('tasks-list'), cpList,
        NyxCollectionView(LKey).Column(NyxTextField('caption'), 'Task')),
      NyxBindCollection(NyxBindingOwner('tasks-tree'), cpTree,
        NyxCollectionView(LKey).Column(NyxTextField('caption'), 'Task').Parent(NyxTextField('parent'))),
      NyxBindCollection(NyxBindingOwner('definition-list'), cpList,
        NyxCollectionView(LKey).Scoped(csInstance).Column(NyxTextField('caption'), 'Task')),
      NyxDefineCollection(NyxCollection('qualification'), NyxCollectionSchema
        .Text(NyxTextField('message'), LQualification)
        .Text(NyxTextField('category'), LChoices[0], NyxTextDomain.Choices(LChoices))
        .Integer(NyxIntegerField('minimum'), Low(Integer))
        .Integer(NyxIntegerField('maximum'), High(Integer))
        .Number(NyxNumberField('precise'), 0.123456789012345), []),
      NyxDefineCollection(NyxCollection('Tasks'), NyxCollectionSchema.Text(NyxTextField('caption'),
        'Case-distinct collection'), [])
    ]);
    LArgs := Args('create-bind', LPatch.ToData);
    Check(ReadNyxCollectionPatch(LPatch.ToData).ToData.ToJSON = LPatch.ToData.ToJSON,
      'all typed schema/domain/row/view values round trip exactly');
    LReceipt := LAgent.Call('nyx_collections', 'Scooty', LArgs, 'collection-owner');
    LRevision := LAgent.Revision;
    LAdded := PairText;
    Check((LReceipt.Field('collections').Field('changes').AsInteger = 7) and
      (LReceipt.Field('selection').AsText = 'home'), 'one publication preserves observing navigation');
    Check(LAgent.Call('nyx_collections', 'Different display name', LArgs, 'collection-owner')
      .ToJSON = LReceipt.ToJSON, 'exact authority retry returns original receipt');
    Refuses(LArgs, 'foreign authority cannot reuse stale successful receipt', 'other-owner');
    Refuses(Args('create-bind', NyxCollectionPatch([NyxAppendCollectionRow(
      NyxCollectionItem(NyxItem(LKey, 'late')))]).ToData), 'changed retry payload refuses');
    History('undo', 'undo-create');
    Check(PairText = LBefore, 'one Undo restores exact original pair');
    History('redo', 'redo-create');
    Check(PairText = LAdded, 'one Redo restores all collections and views');
    QueryPolicies;
    LReply := Query('list', [NyxField('limit', NyxData(1))]);
    Check((LReply.Field('collections').Count = 1) and
      (LReply.Field('total').AsInteger = 3) and
      not NyxAgentHas(LReply.Field('collections').Item(0), 'values'),
      'collection pages contain metadata only');
    LReply := Query('list', [NyxField('filter', NyxData('Tasks'))]);
    Check(LReply.Field('total').AsInteger = 1, 'case-sensitive keys stay distinct');
    LReply := Query('list', [NyxField('offset', NyxData(High(Integer)))]);
    Check((LReply.Field('collections').Count = 0) and
      (LReply.Field('nextOffset').AsInteger = 3), 'huge offset is bounded without arithmetic overflow');
    LReply := Query('schema', [NyxField('key', NyxData('qualification')), NyxField('limit', NyxData(2))]);
    Check((LReply.Field('fields').Count = 2) and
      LReply.Field('fields').Item(1).Field('default').Field('truncated').AsBoolean and
      (LReply.Field('fields').Item(1).Field('domain').Field('choiceCount').AsInteger = 10) and
      (Length(LReply.ToJSON) < 2000), 'schema previews do not dump long defaults or domains');
    LReply := Query('value', [NyxField('key', NyxData('qualification')),
      NyxField('field', NyxData('message')), NyxField('offset', NyxData(1)),
      NyxField('count', NyxData(2))]);
    Check((LReply.Field('value').Field('text').AsText = TNyxText('🌙') + #0) and
      (LReply.Field('value').Field('characters').AsInteger = 5),
      'Unicode scalar windows preserve supplementary text and NUL');
    LReply := Query('value', [NyxField('key', NyxData('qualification')),
      NyxField('field', NyxData('minimum'))]);
    Check(LReply.Field('value').Field('value').AsInteger = Low(Integer), 'exact signed integer minimum');
    LReply := Query('value', [NyxField('key', NyxData('qualification')),
      NyxField('field', NyxData('maximum'))]);
    Check(LReply.Field('value').Field('value').AsInteger = High(Integer), 'exact signed integer maximum');
    LReply := Query('value', [NyxField('key', NyxData('qualification')),
      NyxField('field', NyxData('precise'))]);
    Check(LReply.Field('value').Field('value').AsNumber =
      TNyxStateValue.FromNumber(0.123456789012345).NumberValue,
      'exact accepted Double survives semantic query');
    LReply := Query('domain', [NyxField('key', NyxData('qualification')),
      NyxField('field', NyxData('category')), NyxField('offset', NyxData(2)), NyxField('limit', NyxData(1))]);
    Check((LReply.Field('choices').Count = 1) and (LReply.Field('total').AsInteger = 10) and
      (LReply.Field('choices').Item(0).Field('index').AsInteger = 2) and
      (Length(LReply.ToJSON) < 1000), 'domain choices page through previews');
    LReply := Query('value', [NyxField('key', NyxData('qualification')),
      NyxField('field', NyxData('category')), NyxField('choice', NyxData(2)),
      NyxField('offset', NyxData(5000)), NyxField('count', NyxData(2))]);
    Check(LReply.Field('value').Field('text').AsText = TNyxText('🌙2'),
      'exact domain choice scalar window preserves tail identity');
    LReply := Query('rows', [NyxField('key', NyxData('tasks'))]);
    Check((LReply.Field('rows').Count = 2) and
      (LReply.Field('rows').Item(0).Field('values').Count = 0), 'row discovery returns identities by default');
    LReply := Query('rows', [NyxField('key', NyxData('tasks')),
      NyxField('fields', NyxArray([NyxData('done'), NyxData('ratio'), NyxData('caption')]))]);
    Check(not LReply.Field('rows').Item(0).Field('values').Item(0).Field('value').Field('value').AsBoolean and
      (LReply.Field('rows').Item(0).Field('values').Item(1).Field('value').Field('value').AsNumber = 0.125),
      'selected row fields preserve Boolean/number families and requested order');
    LReply := Query('bindings', [NyxField('owner', NyxData('first-items')), NyxField('limit', NyxData(1))]);
    Check(LReply.Field('inherited').AsBoolean and (LReply.Field('local').Kind = ndNull) and
      (LReply.Field('effective').Field('scope').AsText = 'instance'),
      'named-part query distinguishes inherited effective binding');
    LBefore := PairText;
    Apply('change-row-schema', [
      NyxSetCollectionField(LKey, NyxCollectionSchema.Integer(NyxIntegerField('priority'),
        3, NyxIntegerDomain.Range(0, 10))),
      NyxAppendCollectionRow(NyxCollectionItem(NyxItem(LKey, 'gamma'))
        .WithValue(NyxTextField('caption'), 'Ship').WithValue(NyxIntegerField('priority'), 4)),
      NyxUpdateCollectionRow(NyxCollectionItem(NyxItem(LKey, 'alpha'))
        .WithValue(NyxTextField('caption'), 'Plan the release')
        .WithValue(NyxBooleanField('done'), True).WithValue(NyxNumberField('ratio'), 0.375)),
      NyxMoveCollectionRow(NyxItem(LKey, 'gamma'), 0)]);
    LAdded := PairText;
    LReply := Query('rows', [NyxField('key', NyxData('tasks')), NyxField('limit', NyxData(1)),
      NyxField('fields', NyxArray([NyxData('caption'), NyxData('priority')]))]);
    Check((LReply.Field('rows').Item(0).Field('item').AsText = 'gamma') and
      (LReply.Field('rows').Item(0).Field('values').Item(1).Field('value').Field('value').AsInteger = 4),
      'move uses exact scoped identity and final position');
    LReply := Query('value', [NyxField('key', NyxData('tasks')), NyxField('field', NyxData('priority')),
      NyxField('item', NyxData('alpha'))]);
    Check(LReply.Field('value').Field('value').AsInteger = 2,
      'partial row update and changed default preserve materialized existing cells');
    History('undo', 'undo-row-schema');
    Check(PairText = LBefore, 'schema/append/update/move group is one paired Undo');
    History('redo', 'redo-row-schema');
    Check(PairText = LAdded, 'row group Redo restores exact owned sequence');
    Refuses(Args('late-failure', NyxCollectionPatch([
      NyxUpdateCollectionRow(NyxCollectionItem(NyxItem(LKey, 'alpha'))
        .WithValue(NyxTextField('caption'), 'Must not publish')),
      NyxMoveCollectionRow(NyxItem(LKey, 'missing'), 0)]).ToData),
      'late failure discards entire group/source/history');
    Refuses(Args('domain-failure', NyxCollectionPatch([NyxSetCollectionField(LKey,
      NyxCollectionSchema.Integer(NyxIntegerField('priority'), 1,
        NyxIntegerDomain.Range(0, 1)))]).ToData), 'new domain cannot invalidate existing rows');
    Refuses(Args('family-failure', NyxCollectionPatch([NyxSetCollectionField(LKey,
      NyxCollectionSchema.Text(NyxTextField('priority'), 'Wrong family'))]).ToData),
      'named field cannot silently adopt another family');
    Refuses(Args('duplicate-key', NyxCollectionPatch([NyxDefineCollection(LKey, LSchema, [])]).ToData),
      'define never replaces an existing collection');
    Refuses(Args('duplicate-row', NyxCollectionPatch([
      NyxAppendCollectionRow(NyxCollectionItem(NyxItem(LKey, 'alpha')))]).ToData),
      'append refuses occupied scoped row');
    Refuses(Args('missing-field', NyxCollectionPatch([NyxUpdateCollectionRow(
      NyxCollectionItem(NyxItem(LKey, 'alpha')).WithValue(NyxTextField('absent'), 'Value'))]).ToData),
      'partial update refuses unknown field');
    Refuses(Args('wrong-projection', NyxCollectionPatch([
      NyxBindCollection(NyxBindingOwner('tasks-table'), cpTree, LSpec)]).ToData),
      'view family cannot be implicitly retargeted');
    Refuses(Args('missing-owner', NyxCollectionPatch([
      NyxBindCollection(NyxBindingOwner('missing'), cpTable, LSpec)]).ToData),
      'missing authored owner refuses');
    Refuses(Args('dependent-field', NyxCollectionPatch([NyxRemoveCollectionField(LKey,
      TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('priority')))]).ToData),
      'dependent view prevents field removal');
    Refuses(Args('duplicate-cell', TNyxDataValue.ParseJSON(
      '[{"op":"update-row","key":"tasks","item":"alpha","values":[' +
      '{"field":"caption","kind":"text","value":"one"},' +
      '{"field":"caption","kind":"text","value":"two"}]}]')),
      'duplicate descriptor cells refuse before admission');
    Refuses(Args('wrong-primitive', TNyxDataValue.ParseJSON(
      '[{"op":"intent","action":"cell","key":"tasks","item":"alpha",' +
      '"field":"priority","kind":"integer","value":"3"}]')), 'integer string refuses');
    Refuses(Args('extra-member', TNyxDataValue.ParseJSON(
      '[{"op":"move-row","key":"tasks","item":"alpha","index":0,"ignored":true}]')),
      'unknown nested members refuse');
    Refuses(NyxObject([NyxField('mode', NyxData('rows')), NyxField('key', NyxData('tasks')),
      NyxField('fields', NyxArray([NyxData('caption'), NyxData('caption')]))]),
      'duplicate query fields refuse');
    Refuses(NyxObject([NyxField('mode', NyxData('value')), NyxField('key', NyxData('tasks')),
      NyxField('field', NyxData('priority')), NyxField('offset', NyxData(0))]),
      'numeric window never coerces values to text');
    Refuses(NyxObject([NyxField('mode', NyxData('value')), NyxField('key', NyxData('tasks')),
      NyxField('field', NyxData('caption')), NyxField('item', NyxData('alpha')),
      NyxField('choice', NyxData(0))]), 'row and choice context cannot be ambiguous');
    LBefore := PairText;
    Apply('large-title', [NyxBindCollection(NyxBindingOwner('tasks-table'), cpTable,
      NyxCollectionView(LKey).Column(NyxIntegerField('priority'), 'Priority', cmEditable)
        .Column(NyxTextField('caption'), LLong, cmEditable))]);
    LReply := Query('bindings', [NyxField('owner', NyxData('tasks-table')), NyxField('offset', NyxData(1))]);
    Check((Length(LReply.ToJSON) < 2000) and
      LReply.Field('local').Field('columns').Item(0).Field('title').Field('truncated').AsBoolean,
      'local/effective view pages do not dump large column captions');
    LReply := Query('column', [NyxField('owner', NyxData('tasks-table')),
      NyxField('field', NyxData('caption')), NyxField('source', NyxData('local')),
      NyxField('offset', NyxData(5000)), NyxField('count', NyxData(1))]);
    Check(LReply.Field('title').Field('text').AsText = TNyxText('🌙'),
      'exact column title has independent Unicode scalar window');
    History('undo', 'undo-large-title');
    Check(PairText = LBefore, 'long-title inspection preserves ordinary paired Undo');
    OrdinaryIntents;
    LBefore := PairText;
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaClear;
    LIntent.Key := LKey;
    Apply('clear-reusable', [NyxCollectionIntentChange(NyxBindingOwner('first-items'), LIntent)]);
    LReply := Query('bindings', [NyxField('owner', NyxData('first-items'))]);
    Check(not LReply.Field('inherited').AsBoolean and
      LReply.Field('local').Field('cleared').AsBoolean and
      LReply.Field('effective').Field('cleared').AsBoolean and
      (LReply.Field('restorable').Field('key').AsText = 'tasks'),
      'clear masks the view while exposing its exact restorable inherited contract');
    LIntent.Action := scaInherit;
    LIntent.Key := NyxCollection('Tasks');
    Refuses(Args('wrong-inherited-key', NyxCollectionPatch([
      NyxCollectionIntentChange(NyxBindingOwner('first-items'), LIntent)]).ToData),
      'cleared inheritance still refuses a different exact collection key');
    LIntent.Key := LKey;
    Apply('inherit-reusable', [NyxCollectionIntentChange(NyxBindingOwner('first-items'), LIntent)]);
    LReply := Query('bindings', [NyxField('owner', NyxData('first-items'))]);
    Check(LReply.Field('inherited').AsBoolean, 'inherit restores reusable contract');
    Check(PairText = LBefore, 'clear/inherit restores exact paired metadata/source');
    LBefore := PairText;
    LIntent.Action := scaClear;
    LIntent.Projection := cpTable;
    Apply('clear-remove-field', [
      NyxCollectionIntentChange(NyxBindingOwner('tasks-table'), LIntent),
      NyxRemoveCollectionField(LKey,
        TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('priority')))]);
    LReply := Query('schema', [NyxField('key', NyxData('tasks'))]);
    Check(LReply.Field('total').AsInteger = 4, 'ordered dependency clearing permits field removal');
    History('undo', 'undo-clear-remove');
    Check(PairText = LBefore, 'dependency/field removal is one exact paired Undo');
    LAgent.InheritPermission(apReadOnly);
    Refuses(Args('readonly', NyxCollectionPatch([NyxMoveCollectionRow(NyxItem(LKey, 'alpha'), 0)]).ToData),
      'read-only operator permission refuses mutation');
    Check(Query('list', []).Field('total').AsInteger = 3, 'read-only permission still permits bounded context');
    LAgent.InheritPermission(apDisabled);
    LRejected := False;
    try
      Query('list', []);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LAgent.Revision = LRevision), 'disabled access refuses bounded query');
    LAgent.InheritPermission(apEdit);
    LCurrent := LAgent.PreviewPair(LRevision, 'home');
    LCurrent.Pending := True;
    LCurrent.DraftBase := LCurrent.Source;
    LCurrent.Draft := LCurrent.Source + #10 + '{ Independent draft. }';
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LCurrent))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    LRevision := LAgent.Revision;
    Refuses(Args('draft-refusal', NyxCollectionPatch([
      NyxMoveCollectionRow(NyxItem(LKey, 'alpha'), 0)]).ToData), 'pending Pascal draft refuses group');
    Check(Query('rows', [NyxField('key', NyxData('tasks'))]).Field('total').AsInteger = 3,
      'queries inspect accepted defaults while a draft remains pending');
    LCurrent.Pending := False;
    LCurrent.Draft := '';
    LCurrent.DraftBase := '';
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LCurrent))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    LRevision := LAgent.Revision;
    APair := LAgent.PreviewPair(LRevision, 'home');
    LDocument := TNyxCodec.Decode(APair.Design);
    try
      Check((LDocument.Find('tasks-table').CollectionView.Count = 4) and
        (LDocument.Collections.Snapshot(LKey).Count = 3) and
        (Pos('CollectionWorkshopNote', APair.Source) > 0),
        'final exact companion retains typed views, sequence and handwritten helper');
    finally
      LDocument.Free;
    end;
    LReply := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]));
    Check(LReply.Field('activity').Count > 0, 'observing activity includes successes and refusals');
    Contexts;
  finally
    LPatch := nil;
    LAgent.Free;
  end;
end;

end.
