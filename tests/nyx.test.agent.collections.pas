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
