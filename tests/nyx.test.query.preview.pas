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

unit nyx.test.query.preview;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.collections.query, nyx.studio.projects;

type
  { Test-only immutable object update at the explicit wire boundary. It copies
    every existing field and replaces/adds one exact name; no model mutation. }
  TNyxPreviewTestObject = record helper for TNyxDataValue
    function WithField(const AName: TNyxText; const AValue: TNyxDataValue): TNyxDataValue;
  end;

{ Owned qualification inputs. Unicode/NUL values exercise the portable contract;
  this seed is never installed as a starter document or a user's project. }
function CreateNyxQueryPreviewSeed: TNyxProjectPair;
{ Wire arguments at the explicit MCP boundary, with a public typed candidate. }
function NyxTestQueryPreviewArguments(ARevision: Integer;
  const ASection: TNyxText; const AQuery: TNyxCollectionQuery;
  const AOwner: TNyxText = 'tasks-table'; const ASource: TNyxText = 'effective'): TNyxDataValue;
{ Executes shared semantic admission, bounded values, context/permission and
  history/draft preservation on an independently owned session. }
function RunNyxSemanticQueryPreview: Integer;

implementation

uses
  SysUtils, {$ifdef PAS2JS}Web,{$endif}nyx.state, nyx.collections, nyx.collections.view.types, nyx.binding.types,
  nyx.studio.collectionedits, nyx.studio.stateedits, nyx.studio.agents, nyx.studio.session,
  nyx.studio.workspaces, nyx.studio.reviews, nyx.model, nyx.controls, nyx.contract,
  nyx.codec, nyx.codegen, nyx.types;

function TNyxPreviewTestObject.WithField(const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LFound: Boolean;
begin
  LFound := False;
  SetLength(LFields, Count);
  for LIndex := 0 to Count - 1 do
  begin
    LFields[LIndex] := NyxField(Key(LIndex), Field(Key(LIndex)));

    if Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField(AName, AValue);
  end;
  Result := NyxObject(LFields);
end;

function CreateNyxQueryPreviewSeed: TNyxProjectPair;
var
  LRows: array of TNyxCollectionItem;
  LKey: TNyxCollectionRef;
  LIndex: Integer;
  LCaption: TNyxText;
  LSpec: TNyxCollectionViewSpec;
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LDefinition: INyxColumn;
  LList: INyxList;
  LInstance: INyxComponent;
  LSeed: TNyxProjectPair;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Query preview workshop';
    LPage := NewNyxPage('home');
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
    LSeed := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  finally
    LDocument.Free;
  end;
  LKey := NyxCollection('tasks');
  SetLength(LRows, 30);
  for LIndex := 0 to High(LRows) do
  begin
    LCaption := 'Task ' + TNyxText(IntToStr(LIndex));

    if LIndex = 29 then
    begin
      LCaption := TNyxText(StringOfChar('x', 90)) + TNyxText('🌙') + #0 + TNyxText('é');
    end;
    LRows[LIndex] := NyxCollectionItem(NyxItem(LKey, 'row-' + TNyxText(IntToStr(LIndex))))
      .WithValue(NyxTextField('caption'), LCaption)
      .WithValue(NyxIntegerField('priority'), LIndex)
      .WithValue(NyxBooleanField('done'), Odd(LIndex))
      .WithValue(NyxNumberField('ratio'), LIndex / 2);

    if LIndex > 0 then
    begin
      LRows[LIndex] := LRows[LIndex].WithValue(NyxTextField('parent'), 'row-0');
    end;
  end;
  LSpec := NyxCollectionView(LKey).Column(NyxTextField('caption'), 'Task')
    .Column(NyxIntegerField('priority'), 'Priority')
    .Column(NyxBooleanField('done'), 'Done').Column(NyxNumberField('ratio'), 'Ratio');
  LDocument := TNyxCodec.Decode(LSeed.Design);
  try
    LDocument.Collections.Define(LKey, NyxCollectionSchema.Text(NyxTextField('caption'), '')
      .Text(NyxTextField('parent'), '').Integer(NyxIntegerField('priority'), 0)
      .Boolean(NyxBooleanField('done'), False).Number(NyxNumberField('ratio'), 0), LRows);
    LDocument.Find('tasks-table').SetCollectionView(LSpec);
    LDocument.Find('tasks-list').SetCollectionView(LSpec);
    LDocument.Find('tasks-tree').SetCollectionView(LSpec.Parent(NyxTextField('parent')));
    LDocument.Find('definition-list').SetCollectionView(LSpec);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    {$ifdef PAS2JS}
    { Exact bounded fixture source aids refusal diagnosis; never user content. }
    document.body.setAttribute('data-preview-seed', Result.Source);
    {$endif}
  finally
    LDocument.Free;
  end;
end;

function NyxTestQueryPreviewArguments(ARevision: Integer;
  const ASection: TNyxText; const AQuery: TNyxCollectionQuery;
  const AOwner: TNyxText; const ASource: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('mode', NyxData('preview')),
    NyxField('expectedRevision', NyxData(ARevision)), NyxField('owner', NyxData(AOwner)),
    NyxField('source', NyxData(ASource)), NyxField('section', NyxData(ASection)),
    NyxField('query', AQuery.ToData)]);
end;

function RunNyxSemanticQueryPreview: Integer;
var
  LAgent: TNyxAgentSession;
  LFrame: TNyxAgentRecoveryFrame;
  LBefore: TNyxText;
  LRevision: Integer;
  LState: TNyxDataValue;
  LReply: TNyxDataValue;
  LArguments: TNyxDataValue;
  LQuery: TNyxCollectionQuery;
  LLongQuery: TNyxCollectionQuery;
  LWorkspaces: TNyxStudioWorkspaces;
  LReviews: TNyxReviewWorkspaces;
  LWorkspace: TNyxWorkspaceRef;
  LReview: TNyxReviewRef;
  LForeign: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxCollection.Create('Semantic preview: ' + AReason);
    end;
    Inc(Result);
  end;

  function Pair: TNyxText;
  begin
    Result := EncodeNyxProject(LAgent.RecoveryFrame.Session.Pair);
  end;

  function Call(const ARequest: TNyxDataValue): TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_collections', 'Scooty', ARequest, 'preview-owner');
    Check((Pair = LBefore) and (LAgent.Revision = LRevision),
      'preview preserves exact accepted files and pending draft/base');
    Check(LAgent.Call('nyx_session', 'Scooty', NyxObject([])).Field('canRedo').AsBoolean =
      LState.Field('canRedo').AsBoolean, 'preview retains existing Redo');
  end;

  procedure Refuses(const ARequest: TNyxDataValue; const AReason: TNyxText);
  var
    LRejected: Boolean;
  begin
    LRejected := False;
    try
      Call(ARequest);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (Pair = LBefore) and (LAgent.Revision = LRevision) and
      (Length(LAgent.RecoveryFrame.Session.Redo) = Length(LFrame.Session.Redo)), AReason);
  end;

begin
  Result := 0;
  LAgent := TNyxAgentSession.Create(CreateNyxQueryPreviewSeed);
  try
    LAgent.Call('nyx_collections', 'Scooty', NyxObject([
      NyxField('mode', NyxData('apply')), NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('seed-history')), NyxField('changes', NyxCollectionPatch([
        NyxUpdateCollectionRow(NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'row-0'))
          .WithValue(NyxTextField('caption'), 'Changed root'))]).ToData)]), 'preview-owner');
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('undo-history')), NyxField('direction', NyxData('undo'))]),
      'preview-owner');
    LFrame := LAgent.RecoveryFrame;
    LFrame.Session.Pair.Pending := True;
    LFrame.Session.Pair.DraftBase := LFrame.Session.Pair.Source;
    LFrame.Session.Pair.Draft := LFrame.Session.Pair.Source + TNyxText(#10 + '// Unfinished 🌙');
    FreeAndNil(LAgent);
    LAgent := TNyxAgentSession.CreateRecovered(LFrame);
    LAgent.InheritPermission(apReadOnly);
    LRevision := LAgent.Revision;
    LBefore := Pair;
    LState := LAgent.Call('nyx_session', 'Scooty', NyxObject([]));
    Check(LState.Field('canRedo').AsBoolean, 'fixture has real retained Redo');
    LQuery := NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(27))
      .OrderBy(NyxIntegerField('priority'), nsdDescending);
    LReply := Call(NyxTestQueryPreviewArguments(LRevision, 'summary', LQuery));
    Check((LReply.Field('visibleCount').AsInteger = 3) and
      (LReply.Field('sourceCount').AsInteger = 30), 'unapplied typed counts');
    Check(not NyxAgentHas(LReply, 'rows') and not NyxAgentHas(LReply, 'pascal') and
      (LReply.Field('origin').AsText = 'project-defaults'), 'summary is small and labels defaults');
    LArguments := NyxTestQueryPreviewArguments(LRevision, 'rows', LQuery);
    LReply := Call(LArguments);
    Check((LReply.Field('rows').Count = 3) and
      (LReply.Field('rows').Item(0).Field('item').AsText = 'row-29') and
      (LReply.Field('rows').Item(0).Field('values').Count = 0), 'ordered IDs omit unrequested cells');
    LArguments := LArguments.WithField('limit', NyxData(1)).WithField('fields', NyxArray([
      NyxData('caption'), NyxData('priority'), NyxData('done'), NyxData('ratio')]));
    LReply := Call(LArguments);
    Check((LReply.Field('nextOffset').AsInteger = 1) and (LReply.Field('rows').Count = 1),
      'bounded row page');
    LReply := LReply.Field('rows').Item(0).Field('values');
    Check(LReply.Item(0).Field('value').Field('characters').AsInteger = 93,
      'text scalar count includes supplementary Unicode and NUL');
    Check(LReply.Item(0).Field('value').Field('truncated').AsBoolean and
      (Length(LReply.Item(0).Field('value').Field('preview').AsText) = 80), '80-scalar cell preview');
    Check((LReply.Item(1).Field('value').Field('value').AsInteger = 29) and
      LReply.Item(2).Field('value').Field('value').AsBoolean and
      (LReply.Item(3).Field('value').Field('value').AsNumber = 14.5), 'exact native scalar families');
    LReply := Call(LArguments.WithField('offset', NyxData(1)));
    Check(LReply.Field('rows').Item(0).Field('item').AsText = 'row-28', 'next page retains exact order');
    LReply := Call(LArguments.WithField('offset', NyxData(5000)));
    Check((LReply.Field('rows').Count = 0) and (LReply.Field('nextOffset').AsInteger = 3),
      'past-end page stays bounded');
    LReply := Call(NyxTestQueryPreviewArguments(LRevision, 'summary', NyxCollectionQuery));
    Check(LReply.Field('visibleCount').AsInteger = 30, 'null candidate explores unfiltered defaults');
    LReply := Call(NyxTestQueryPreviewArguments(LRevision, 'rows', NyxCollectionQuery));
    Check((LReply.Field('rows').Count = 8) and (LReply.Field('nextOffset').AsInteger = 8),
      'default row page returns eight of thirty IDs');
    LReply := Call(NyxTestQueryPreviewArguments(LRevision, 'rows', NyxCollectionQuery)
      .WithField('offset', NyxData(20)).WithField('limit', NyxData(20)));
    Check((LReply.Field('rows').Count = 10) and (LReply.Field('nextOffset').AsInteger = 30),
      'maximum page and short final page');
    LReply := Call(NyxTestQueryPreviewArguments(LRevision, 'summary', LQuery, 'tasks-tree'));
    Check(LReply.Field('tree').AsBoolean and (LReply.Field('visibleCount').AsInteger = 4),
      'tree preview includes one required ancestor');
    LReply := Call(NyxTestQueryPreviewArguments(LRevision, 'summary', LQuery, 'first-items'));
    Check(LReply.Field('visibleCount').AsInteger = 3, 'effective inherited recipe binding');
    Refuses(NyxTestQueryPreviewArguments(LRevision, 'summary', LQuery, 'first-items', 'local'),
      'absent local binding does not silently use inherited binding');
    LLongQuery := NyxCollectionQuery.Where(NyxWhere(NyxTextField('caption'))
      .Contains(TNyxText(StringOfChar('x', 5000)) + TNyxText('🌙')));
    LArguments := NyxTestQueryPreviewArguments(LRevision, 'pascal', LLongQuery)
      .WithField('offset', NyxData(4990)).WithField('count', NyxData(512));
    LReply := Call(LArguments);
    Check(Pos(TNyxText('🌙'), LReply.Field('pascal').Field('text').AsText) > 0,
      'exact Pascal window retains supplementary Unicode');
    Check((LReply.Field('pascal').Field('characters').AsInteger > 5000) and
      not NyxAgentHas(LReply, 'rows'), 'Pascal section omits row payload');
    Refuses(LArguments.WithField('count', NyxData(4097)), 'text window maximum');
    Refuses(LArguments.WithField('offset', NyxData(8000)), 'Pascal past-end refuses');
    Refuses(LArguments.WithField('limit', NyxData(1)), 'cross-section arguments refuse');
    Refuses(NyxTestQueryPreviewArguments(LRevision - 1, 'summary', LQuery), 'stale revision refuses');
    Refuses(NyxTestQueryPreviewArguments(LRevision, 'rows', LQuery).WithField('limit', NyxData(21)),
      'row maximum');
    Refuses(NyxTestQueryPreviewArguments(LRevision, 'rows', LQuery).WithField('fields',
      NyxArray([NyxData('priority'), NyxData('priority')])), 'duplicate field refuses');
    Refuses(NyxTestQueryPreviewArguments(LRevision, 'rows', LQuery).WithField('fields',
      NyxArray([NyxData('missing')])), 'missing requested field refuses');
    Refuses(NyxTestQueryPreviewArguments(LRevision, 'summary', LQuery, 'home'), 'unbound owner refuses');
    Refuses(NyxTestQueryPreviewArguments(LRevision, 'summary', NyxCollectionQuery.Where(
      NyxWhere(NyxTextField('priority')).EqualTo('27'))), 'wrong field family refuses');
    Refuses(NyxTestQueryPreviewArguments(LRevision, 'summary', LQuery).WithField('query',
      NyxData('priority > 27')), 'raw predicate strings refuse');
    Refuses(NyxTestQueryPreviewArguments(LRevision, 'everything', LQuery),
      'unknown response section refuses');
    Refuses(NyxObject([NyxField('mode', NyxData('preview')),
      NyxField('owner', NyxData('tasks-table')), NyxField('source', NyxData('effective')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('section', NyxData('summary'))]),
      'candidate is required; omission cannot substitute the saved query');

    LAgent.InheritPermission(apEdit);
    LWorkspaces := TNyxStudioWorkspaces.Create(LAgent, 'semantic-preview-fixture');
    LReviews := TNyxReviewWorkspaces.Create(LAgent);
    try
      LWorkspace := LWorkspaces.OpenProject('Independent query preview', CreateNyxQueryPreviewSeed);
      LReply := LWorkspaces.Call('nyx_collections', 'preview-owner', 'Scooty', NyxWithWorkspace(
        NyxTestQueryPreviewArguments(LWorkspaces.Resolve(LWorkspace).Revision, 'summary', LQuery),
        LWorkspace));
      Check((LReply.Field('workspace').AsText = LWorkspace.ID) and
        (LReply.Field('visibleCount').AsInteger = 3), 'workspace keeps exact route');
      LReview := LReviews.CreateReview('preview-owner', 'Scooty', 'Owned query preview',
        nrbAccepted, LRevision);
      LArguments := NyxWithReview(NyxTestQueryPreviewArguments(
        LReviews.Resolve('preview-owner', LReview).Revision, 'summary', LQuery), LReview);
      LReply := LReviews.Call('nyx_collections', 'preview-owner', 'Scooty', LArguments);
      Check((LReply.Field('review').AsText = LReview.ID) and
        (LReply.Field('visibleCount').AsInteger = 3), 'private review keeps exact route');
      LForeign := False;
      try
        LReviews.Call('nyx_collections', 'foreign-owner', 'Scooty', LArguments);
      except
        on Exception do
        begin
          LForeign := True;
        end;
      end;
      Check(LForeign and (Pair = LBefore), 'foreign review access refuses and primary pair survives');
    finally
      LReviews.Free;
      LWorkspaces.Free;
    end;
    { The fixture no longer needs its bounded source diagnostic. }
    {$ifdef PAS2JS}document.body.removeAttribute('data-preview-seed');{$endif}
    LAgent.InheritPermission(apDisabled);
    Refuses(NyxTestQueryPreviewArguments(LRevision, 'summary', LQuery), 'disabled access refuses');
  finally
    LAgent.Free;
  end;
end;

end.
