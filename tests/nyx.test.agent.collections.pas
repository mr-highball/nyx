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
  { END QUERY_CONTROL_READ_IMPORT }
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
