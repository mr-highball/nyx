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
unit nyx.test.typeahead.workflow;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.studio.projects;

{ Runs the ordinary in-process semantic dispatcher against an independent copy
  of the authenticated English seed. Returns owned accepted design/source text
  for actual controls and a separate compiler. This does not qualify HTTP
  authentication, observing Studio, browser execution or a live user mutation.
  Native export uses byte streams; browser callers never access the filesystem. }
function RunNyxTypeAheadWorkflowTests(out APair: TNyxProjectPair;
  const AOutputDirectory: TNyxText = ''): Integer;

implementation

uses
  SysUtils, nyx.data, nyx.model, nyx.types, nyx.codec, nyx.codegen,
  nyx.controls, nyx.collections, nyx.collections.query,
  nyx.collections.view.types, nyx.collections.selection, nyx.typeahead,
  nyx.studio.agents, nyx.studio.collectionedits, nyx.studio.collectionintent,
  nyx.studio.stateedits,
  nyx.generated.view
  {$ifndef PAS2JS}, Classes{$endif};

const
  CView: TNyxText = 'typeahead-review';
  CHelper: TNyxText = '{ Handwritten semantic companion / 🌙 }' + #10 +
    'function SemanticSearchNote: Integer;' + #10 + 'begin' + #10 +
    '  Result := 42;' + #10 + 'end;' + #10;

function ReplaceFirst(const AText, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(ABefore, AText);

  if LPosition = 0 then
  begin
    raise Exception.Create('Semantic search fixture text missing: ' + ABefore);
  end;
  Result := Copy(AText, 1, LPosition - 1) + AAfter +
    Copy(AText, LPosition + Length(ABefore), Length(AText));
end;

function WithField(const AData: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AData.Count);
  for LIndex := 0 to AData.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));

    if AData.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
    end;
  end;
  Result := NyxObject(LFields);
end;

function RunNyxTypeAheadWorkflowTests(out APair: TNyxProjectPair;
  const AOutputDirectory: TNyxText): Integer;
var
  LAgent: TNyxAgentSession;
  LDocument: TNyxDocument;
  LSeed: TNyxProjectPair;
  LOriginal: TNyxProjectPair;
  LCurrent: TNyxProjectPair;
  LChanged: TNyxText;
  LBefore: TNyxText;
  LSource: TNyxText;
  LRevision: Integer;
  LChecks: Integer;
  LKey: TNyxCollectionRef;
  LListBase: TNyxCollectionViewSpec;
  LTreeBase: TNyxCollectionViewSpec;
  LSpec: TNyxCollectionViewSpec;
  LQuery: TNyxCollectionQuery;
  LArgs: TNyxDataValue;
  LReply: TNyxDataValue;
  LData: TNyxDataValue;
  LPolicy: TNyxDataValue;
  LSchema: TNyxDataValue;
  LBranch: TNyxDataValue;
  LIndex: Integer;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Semantic typeahead: ' + AReason);
    end;
    Inc(LChecks);
  end;

  function Pair: TNyxProjectPair;
  begin
    Result := LAgent.PreviewPair(LRevision, CView);
  end;

  function Arguments(const AID: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)), NyxField('changes', AChanges)]);
  end;

  function Apply(const AID: TNyxText;
    const AChanges: array of TNyxCollectionChange): TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_collections', 'Scooty',
      Arguments(AID, NyxCollectionPatch(AChanges).ToData), 'search-owner');
    LRevision := LAgent.Revision;
  end;

  function Context(const AOwner: TNyxText; AOffset: Integer = 0): TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_collections', 'Scooty', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData(AOwner)),
      NyxField('offset', NyxData(AOffset)), NyxField('limit', NyxData(1))]));
  end;

  procedure History(const ADirection, AID: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)), NyxField('direction', NyxData(ADirection))]),
      'search-owner');
    LRevision := LAgent.Revision;
  end;

  { Candidate refusal must preserve more than the design: source helpers,
    pending input, complete history and navigation remain authoritative. Activity
    may record refusal, so it is deliberately excluded from this comparison. }
  procedure Refuses(const ATool: TNyxText; const AArguments: TNyxDataValue;
    const AReason: TNyxText);
  var
    LFrame: TNyxAgentRecoveryFrame;
    LAfter: TNyxAgentRecoveryFrame;
    LRefused: Boolean;
    LHistorySame: Boolean;
    LEntry: Integer;
  begin
    LFrame := LAgent.RecoveryFrame;
    LRefused := False;
    try
      LAgent.Call(ATool, 'Scooty', AArguments, 'search-owner');
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    LAfter := LAgent.RecoveryFrame;
    LHistorySame := (Length(LFrame.Session.Undo) = Length(LAfter.Session.Undo)) and
      (Length(LFrame.Session.Redo) = Length(LAfter.Session.Redo));

    if LHistorySame then
    begin
      for LEntry := 0 to High(LFrame.Session.Undo) do
      begin
        LHistorySame := LHistorySame and
          (LFrame.Session.Undo[LEntry].Design = LAfter.Session.Undo[LEntry].Design) and
          (LFrame.Session.Undo[LEntry].Source = LAfter.Session.Undo[LEntry].Source);
      end;
      for LEntry := 0 to High(LFrame.Session.Redo) do
      begin
        LHistorySame := LHistorySame and
          (LFrame.Session.Redo[LEntry].Design = LAfter.Session.Redo[LEntry].Design) and
          (LFrame.Session.Redo[LEntry].Source = LAfter.Session.Redo[LEntry].Source);
      end;
    end;
    Check(LRefused and (LAgent.Revision = LRevision) and
      (EncodeNyxProject(LAfter.Session.Pair) = EncodeNyxProject(LFrame.Session.Pair)) and
      LHistorySame and (LFrame.Session.Selection = LAfter.Session.Selection) and
      (LFrame.Session.View = LAfter.Session.View) and
      (LFrame.Permission = LAfter.Permission),
      AReason);
  end;

  procedure InheritedPolicies;
  var
    LDefinition: INyxColumn;
    LList: INyxList;
    LInstance: INyxComponent;
    LTable: INyxTable;
    LIntent: TNyxStudioCollectionIntent;
    LBaseline: TNyxText;
  begin
    { Explicit local enrichment supplies two reusable instances; no live project
      is replaced. One instance owns a named override and the other inherits. }
    LDocument := TNyxCodec.Decode(LSeed.Design);
    try
      LDefinition := NewNyxColumn('search-card', ncoDescriptor);
      LDocument.AddComponent(LDefinition);
      LList := NewNyxList('definition-list');
      LList.Configure.PartName(NyxPart('items')).Done;
      LList.Binds.Collection(LListBase.TypeAhead(NyxTypeAhead.Match(ntmExact)));
      LDefinition.Add(LList);
      LInstance := NewNyxComponent('first-card');
      LInstance.Configure.Component(NyxComponent('search-card')).Done;
      LInstance.OverridePart(NyxPart('items'), noProperties).Named('first-items');
      LDocument.Pages[0].Add(LInstance);
      LTable := NewNyxTable('destination-table');
      LTable.Binds.Collection(LListBase);
      LDocument.Pages[0].Add(LTable);
      LDocument.Pages[0].Add(NewNyxList('unbound-list'));
      LInstance := NewNyxComponent('second-card');
      LInstance.Configure.Component(NyxComponent('search-card')).Done;
      LDocument.Pages[0].Add(LInstance);
      LOriginal := NyxProjectPair(TNyxCodec.Encode(LDocument),
        TNyxCodegen.Generate(LDocument));
    finally
      LDocument.Free;
      LDocument := nil;
      LDefinition := nil;
      LList := nil;
      LInstance := nil;
      LTable := nil;
    end;
    LAgent.Free;
    LAgent := nil;
    LAgent := TNyxAgentSession.Create(LOriginal);
    LRevision := LAgent.Revision;
    LBaseline := EncodeNyxProject(Pair);
    LReply := Context('destination-table');
    Check(not LReply.Field('typeAheadSupported').AsBoolean and
      (LReply.Field('effective').Field('typeAhead').Kind = ndNull),
      'table context exposes unsupported search without a fictitious default');
    Refuses('nyx_collections', Arguments('unbound-search', NyxCollectionPatch([
      NyxSetCollectionTypeAhead(NyxBindingOwner('unbound-list'), LKey, cpList,
        NyxTypeAhead)]).ToData), 'an unbound list refuses a policy-only edit');
    LReply := Context('first-items');
    Check(LReply.Field('inherited').AsBoolean and
      (LReply.Field('local').Kind = ndNull) and
      LReply.Field('effective').Field('typeAhead').Field('declared').AsBoolean,
      'effective inherited search is distinct from no local binding');
    Apply('inherited-default', [NyxUseDefaultCollectionTypeAhead(
      NyxBindingOwner('first-items'), LKey, cpList)]);
    LReply := Context('first-items');
    Check(not LReply.Field('inherited').AsBoolean and
      not LReply.Field('local').Field('typeAhead').Field('declared').AsBoolean and
      (LReply.Field('effective').Field('typeAhead').Field('policy').Field('match').AsText = 'folded'),
      'default reset copies an independent local binding, not inheritance');
    Check(Context('definition-list').Field('effective').Field('typeAhead')
      .Field('policy').Field('match').AsText = 'exact', 'definition search stays independent');
    LDocument := TNyxCodec.Decode(Pair.Design);
    try
      Check(not LDocument.Find('second-card').HasCollectionView,
        'sibling instance acquires no local binding');
    finally
      LDocument.Free;
      LDocument := nil;
    end;
    History('undo', 'inherited-undo');
    Check(EncodeNyxProject(Pair) = LBaseline, 'inherited reset is one exact paired Undo');
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaClear;
    LIntent.Key := LKey;
    LIntent.Projection := cpList;
    Apply('clear-inherited', [NyxCollectionIntentChange(NyxBindingOwner('first-items'), LIntent)]);
    LReply := Context('first-items');
    Check(LReply.Field('local').Field('cleared').AsBoolean and
      LReply.Field('restorable').Field('typeAhead').Field('declared').AsBoolean and
      (LReply.Field('restorable').Field('typeAhead').Field('policy').Field('match').AsText = 'exact'),
      'masked inheritance exposes the exact restorable saved policy');
    Refuses('nyx_collections', Arguments('cleared-search', NyxCollectionPatch([
      NyxSetCollectionTypeAhead(NyxBindingOwner('first-items'), LKey, cpList,
        NyxTypeAhead)]).ToData), 'a cleared binding cannot become a search-only binding');
    History('undo', 'clear-inherited-undo');
    Check(EncodeNyxProject(Pair) = LBaseline, 'clear refusal and Undo retain the exact reusable pair');
  end;

  {$ifndef PAS2JS}
  procedure ExportBytes(const AName, AText: TNyxText);
  var
    LStream: TFileStream;
  begin
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(String(AOutputDirectory)) +
      String(AName), fmCreate);
    try

      if AText <> '' then
      begin
        LStream.WriteBuffer(AText[1], Length(AText));
      end;
    finally
      LStream.Free;
    end;
  end;
  {$endif}

begin
  LChecks := 0;
  LAgent := nil;
  LDocument := BuildNyxDocument;
  try
    LKey := NyxCollection('destinations');
    LListBase := LDocument.Find('destination-list').CollectionView;
    LTreeBase := LDocument.Find('destination-tree').CollectionView;
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.generated.workflow');
    LSource := ReplaceFirst(LSource, 'implementation' + #10,
      'function SemanticSearchNote: Integer;' + #10 + #10 + 'implementation' + #10 + #10 + CHelper);
    LSeed := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
    LDocument.Free;
    LDocument := nil;
    LAgent := TNyxAgentSession.Create(LSeed);
    LRevision := LAgent.Revision;
    LBefore := EncodeNyxProject(Pair);
    LReply := Context('destination-list');
    Check(LReply.Field('typeAheadSupported').AsBoolean and
      not LReply.Field('effective').Field('typeAhead').Field('declared').AsBoolean and
      (LReply.Field('effective').Field('typeAhead').Field('policy').ToJSON = NyxTypeAhead.ToData.ToJSON),
      'bounded binding context reports library defaults without a saved declaration');
    Check((LReply.Field('effective').Field('columns').Count = 1) and
      (LReply.Field('revision').AsInteger = LRevision) and
      (EncodeNyxProject(Pair) = LBefore), 'bounded reads retain revision, navigation and the complete pair');

    LSchema := NyxCollectionAgentSchema.Field('oneOf');
    LSchema := LSchema.Item(LSchema.Count - 1).Field('properties')
      .Field('changes').Field('items').Field('oneOf');
    for LIndex := 0 to LSchema.Count - 1 do
    begin
      LBranch := LSchema.Item(LIndex);

      if NyxAgentHas(LBranch.Field('properties').Field('op'), 'const') and
        (LBranch.Field('properties').Field('op').Field('const').AsText = 'bind') then
      begin
        LData := LBranch.Field('properties').Field('spec').Field('oneOf');
        Check((LData.Count = 4) and
          (LData.Item(3).Field('properties').Field('version').Field('const').AsInteger = 4) and
          (LData.Item(3).Field('required').Count = 8) and
          not LData.Item(3).Field('additionalProperties').AsBoolean,
          'discovery advertises all four closed binding versions');
      end;

      if NyxAgentHas(LBranch.Field('properties').Field('op'), 'const') and
        (LBranch.Field('properties').Field('op').Field('const').AsText = 'typeahead') then
      begin
        LData := LBranch.Field('properties').Field('policy').Field('oneOf').Item(1);
        Check((LBranch.Field('required').Count = 5) and
          (LBranch.Field('properties').Field('projection').Field('enum').Count = 2) and
          (LData.Field('properties').Field('enabled').Field('type').AsText = 'boolean') and
          (LData.Field('properties').Field('windowMS').Field('type').AsText = 'integer') and
          (LData.Field('properties').Field('match').Field('enum').Count = 2) and
          not LData.Field('additionalProperties').AsBoolean,
          'policy discovery exposes typed scalars and closed list/tree capability');
      end;
    end;

    LArgs := Arguments('set-both', NyxCollectionPatch([
      NyxSetCollectionTypeAhead(NyxBindingOwner('destination-list'), LKey, cpList,
        NyxTypeAhead.Enabled(False).WindowMilliseconds(700)),
      NyxSetCollectionTypeAhead(NyxBindingOwner('destination-tree'), LKey, cpTree,
        NyxTypeAhead.Match(ntmExact).WindowMilliseconds(800))]).ToData);
    LReply := LAgent.Call('nyx_collections', 'Scooty', LArgs, 'search-owner');
    LRevision := LAgent.Revision;
    LChanged := EncodeNyxProject(Pair);
    Check((LRevision = 2) and (Pos(CHelper, Pair.Source) > 0),
      'two saved changes publish once and retain the exact handwritten Unicode helper');
    Check(Context('destination-list').Field('local').Field('typeAhead').Field('declared').AsBoolean and
      not Context('destination-list').Field('effective').Field('typeAhead').Field('policy').Field('enabled').AsBoolean,
      'grouped edit is visible through ordinary semantic context');
    Check(LAgent.Call('nyx_collections', 'Scooty', LArgs, 'search-owner').ToJSON = LReply.ToJSON,
      'identical retry returns its original bounded receipt');
    Check((LAgent.Revision = LRevision) and (EncodeNyxProject(Pair) = LChanged),
      'retry creates no new pair or history');
    Refuses('nyx_collections', WithField(LArgs, 'changes', NyxCollectionPatch([
      NyxUseDefaultCollectionTypeAhead(NyxBindingOwner('destination-list'), LKey, cpList)]).ToData),
      'operation identity cannot be reused for a different payload');
    History('undo', 'set-both-undo');
    Check(EncodeNyxProject(Pair) = LBefore, 'one paired Undo removes both policies');
    History('redo', 'set-both-redo');
    Check(EncodeNyxProject(Pair) = LChanged, 'one paired Redo restores both exact policies');
    Refuses('nyx_collections', Arguments('wrong-key', NyxCollectionPatch([
      NyxSetCollectionTypeAhead(NyxBindingOwner('destination-list'), NyxCollection('other'),
        cpList, NyxTypeAhead)]).ToData), 'changed collection key refuses');
    Refuses('nyx_collections', Arguments('wrong-owner', NyxCollectionPatch([
      NyxSetCollectionTypeAhead(NyxBindingOwner('missing-list'), LKey, cpList,
        NyxTypeAhead)]).ToData), 'missing exact owner refuses');
    Refuses('nyx_collections', Arguments('wrong-projection', NyxCollectionPatch([
      NyxSetCollectionTypeAhead(NyxBindingOwner('destination-list'), LKey, cpTree,
        NyxTypeAhead)]).ToData), 'changed projection refuses');
    Refuses('nyx_collections', WithField(Arguments('stale', LArgs.Field('changes')),
      'expectedRevision', NyxData(LRevision - 1)), 'stale expected revision refuses');

    LData := NyxCollectionPatch([NyxSetCollectionTypeAhead(
      NyxBindingOwner('destination-list'), LKey, cpList, NyxTypeAhead)]).ToData.Item(0);
    LPolicy := LData.Field('policy');
    Refuses('nyx_collections', Arguments('table-policy', NyxArray([
      WithField(LData, 'projection', NyxData('table'))])), 'table search capability refuses');
    Refuses('nyx_collections', Arguments('string-boolean', NyxArray([
      WithField(LData, 'policy', WithField(LPolicy, 'enabled', NyxData('false')))])),
      'raw string Boolean refuses');
    Refuses('nyx_collections', Arguments('string-integer', NyxArray([
      WithField(LData, 'policy', WithField(LPolicy, 'windowMS', NyxData('700')))])),
      'raw string timing refuses');
    Refuses('nyx_collections', Arguments('unknown-match', NyxArray([
      WithField(LData, 'policy', WithField(LPolicy, 'match', NyxData('fuzzy')))])),
      'unknown match enum refuses');
    Refuses('nyx_collections', Arguments('wrong-version', NyxArray([
      WithField(LData, 'policy', WithField(LPolicy, 'version', NyxData(2)))])),
      'unknown policy version refuses');
    Refuses('nyx_collections', Arguments('too-small', NyxArray([
      WithField(LData, 'policy', WithField(LPolicy, 'windowMS', NyxData(0)))])),
      'timing below the admitted bound refuses');
    Refuses('nyx_collections', Arguments('too-large', NyxArray([
      WithField(LData, 'policy', WithField(LPolicy, 'windowMS', NyxData(60001)))])),
      'timing above the admitted bound refuses');
    Refuses('nyx_collections', Arguments('missing-policy', NyxArray([NyxObject([
      NyxField('op', NyxData('typeahead')), NyxField('owner', NyxData('destination-list')),
      NyxField('key', NyxData('destinations')), NyxField('projection', NyxData('list'))])])),
      'an omitted policy cannot silently reset saved intent');
    Refuses('nyx_collections', Arguments('unknown-member', NyxArray([NyxObject([
      NyxField('op', NyxData('typeahead')), NyxField('owner', NyxData('destination-list')),
      NyxField('key', NyxData('destinations')), NyxField('projection', NyxData('list')),
      NyxField('policy', LPolicy), NyxField('match', NyxData('exact'))])])),
      'unknown operation members refuse instead of changing meaning');
    Refuses('nyx_collections', Arguments('late-failure', NyxArray([
      WithField(LData, 'policy', NyxNull), WithField(LData, 'key', NyxData('other'))])),
      'late invalid search change rolls back the preceding default reset');

    Apply('declared-default', [NyxSetCollectionTypeAhead(
      NyxBindingOwner('destination-list'), LKey, cpList, NyxTypeAhead)]);
    Check(Context('destination-list').Field('effective').Field('typeAhead').Field('declared').AsBoolean,
      'explicit library-equivalent options retain declared intent');
    Apply('default-reset', [NyxUseDefaultCollectionTypeAhead(
      NyxBindingOwner('destination-list'), LKey, cpList)]);
    Check(not Context('destination-list').Field('local').Field('typeAhead').Field('declared').AsBoolean,
      'null reset retains a local binding while clearing only the declaration');
    Apply('version-four-bind', [NyxBindCollection(NyxBindingOwner('destination-list'),
      cpList, LListBase.TypeAhead(NyxTypeAhead.WindowMilliseconds(900)))]);
    Check(Context('destination-list').Field('effective').Field('typeAhead').Field('policy')
      .Field('windowMS').AsInteger = 900, 'full version-four binding shares semantic admission');
    LQuery := NyxCollectionQuery.OrderBy(NyxTextField('title'));
    LSpec := LTreeBase.Scoped(csInstance).Selection(nsmMultiple).Query(LQuery)
      .Column(NyxTextField('parent'), 'Region / 🌙');
    Apply('rich-binding', [NyxBindCollection(NyxBindingOwner('destination-tree'), cpTree, LSpec)]);
    Apply('rich-policy', [NyxSetCollectionTypeAhead(NyxBindingOwner('destination-tree'),
      LKey, cpTree, NyxTypeAhead.Match(ntmExact))]);
    LDocument := TNyxCodec.Decode(Pair.Design);
    try
      Check(LDocument.Find('destination-tree').CollectionView.UseDefaultTypeAhead.ToData.ToJSON =
        LSpec.ToData.ToJSON, 'policy-only edits retain exact scope, parent, columns, selection and query');
    finally
      LDocument.Free;
      LDocument := nil;
    end;
    LReply := Context('destination-tree', 1);
    Check((LReply.Field('effective').Field('total').AsInteger = 2) and
      (LReply.Field('effective').Field('columns').Count = 1) and
      LReply.Field('effective').Field('typeAhead').Field('declared').AsBoolean,
      'column pagination keeps bounded saved policy available');
    Apply('query-preserves-search', [NyxSetCollectionQuery(
      NyxBindingOwner('destination-tree'), LKey, cpTree, NyxCollectionQuery)]);
    Check(Context('destination-tree').Field('effective').Field('typeAhead').Field('policy').Field('match').AsText = 'exact',
      'query-only edits retain the independent saved search policy');
    Apply('reset-rich-policy', [NyxUseDefaultCollectionTypeAhead(
      NyxBindingOwner('destination-tree'), LKey, cpTree)]);
    LDocument := TNyxCodec.Decode(Pair.Design);
    try
      Check(LDocument.Find('destination-tree').CollectionView.ToData.ToJSON =
        LSpec.Query(NyxCollectionQuery).ToData.ToJSON, 'default reset retains every unrelated rich-binding field');
    finally
      LDocument.Free;
      LDocument := nil;
    end;

    LAgent.InheritPermission(apReadOnly);
    Refuses('nyx_collections', Arguments('read-only', NyxArray([LData])),
      'operator read-only permission guards policy mutation');
    LAgent.InheritPermission(apEdit);
    LAgent.InheritPermission(apDisabled);
    Refuses('nyx_collections', Arguments('disabled', NyxArray([LData])),
      'operator disablement guards the same mutation and complete history');
    LAgent.InheritPermission(apEdit);
    LCurrent := Pair;
    LCurrent.Pending := True;
    LCurrent.DraftBase := LCurrent.Source;
    LCurrent.Draft := LCurrent.Source + TNyxText(#10 + '{ Unsubmitted draft / 🌙 }');
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LCurrent))),
      NyxField('selection', NyxData('destination-list')), NyxField('view', NyxData(CView))]));
    LRevision := LAgent.Revision;
    Refuses('nyx_collections', Arguments('pending-draft', NyxArray([LData])),
      'pending exact Unicode draft/base survives refusal');
    LCurrent.Pending := False;
    LCurrent.Draft := '';
    LCurrent.DraftBase := '';
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LCurrent))),
      NyxField('selection', NyxData('destination-list')), NyxField('view', NyxData(CView))]));
    LRevision := LAgent.Revision;

    LBefore := EncodeNyxProject(Pair);
    Refuses('nyx_transaction', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('combined-refusal')),
      NyxField('operations', NyxArray([
        NyxObject([NyxField('op', NyxData('title')), NyxField('value', NyxData('Refuse this entire proposal'))]),
        NyxObject([NyxField('op', NyxData('collections')), NyxField('changes', NyxArray([
          WithField(LData, 'key', NyxData('other'))]))])]))]),
      'late search refusal rolls back the earlier design group and complete history');
    LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('combined-search')),
      NyxField('operations', NyxArray([
        NyxObject([NyxField('op', NyxData('title')), NyxField('value', NyxData('Search your destinations'))]),
        NyxObject([NyxField('op', NyxData('collections')), NyxField('changes', NyxArray([LData]))])]))]),
      'search-owner');
    LRevision := LAgent.Revision;
    LChanged := EncodeNyxProject(Pair);
    Check((Pair.Design <> DecodeNyxProject(LBefore).Design) and (Pos(CHelper, Pair.Source) > 0),
      'combined design and search group retains authored source helpers');
    History('undo', 'combined-undo');
    Check(EncodeNyxProject(Pair) = LBefore, 'combined design/search changes share one paired Undo');
    History('redo', 'combined-redo');
    Check(EncodeNyxProject(Pair) = LChanged, 'combined design/search changes share one paired Redo');
    History('undo', 'combined-restore');

    { Export the accepted semantic pair, not a separately regenerated lookalike.
      Both ordinary adapters subsequently mount precisely this design. }
    Apply('control-companion', [
      NyxBindCollection(NyxBindingOwner('destination-list'), cpList, LListBase),
      NyxBindCollection(NyxBindingOwner('destination-tree'), cpTree, LTreeBase),
      NyxSetCollectionTypeAhead(NyxBindingOwner('destination-list'), LKey, cpList,
        NyxTypeAhead.Enabled(False).WindowMilliseconds(700)),
      NyxSetCollectionTypeAhead(NyxBindingOwner('destination-tree'), LKey, cpTree,
        NyxTypeAhead.Match(ntmExact).WindowMilliseconds(800))]);
    APair := Pair;
    Check((Pos(CHelper, APair.Source) > 0) and (Pos('nyx.typeahead', APair.Source) > 0),
      'accepted semantic source retains helper and its typed search import');
    InheritedPolicies;
    {$ifndef PAS2JS}

    if AOutputDirectory <> '' then
    begin
      ForceDirectories(String(AOutputDirectory));
      ExportBytes('nyx.generated.workflow.pas', APair.Source);
      ExportBytes('design.nyx', APair.Design);
      ExportBytes('project.nyxpair', EncodeNyxProject(APair));
    end;
    {$endif}
    Result := LChecks;
  finally
    LDocument.Free;
    LAgent.Free;
  end;
end;

end.
