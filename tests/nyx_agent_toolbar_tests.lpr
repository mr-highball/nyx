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

program nyx_agent_toolbar_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.state, nyx.contract, nyx.collections, nyx.collections.query,
  nyx.collections.view.types, nyx.binding, nyx.binding.types, nyx.data.toolbar,
  nyx.data.toolbar.query, nyx.studio.agents, nyx.studio.edits,
  nyx.studio.projects, nyx.studio.transactions, nyx.studio.collectionedits,
  nyx.studio.stateedits
  {$ifdef PAS2JS}, Web{$else}, nyx.studio.mcp{$endif};

var
  GAgent: TNyxAgentSession;
  GRevision: Integer;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Semantic DataToolbar: ' + AReason);
  end;
  Inc(GChecks);
end;

function Grid(const AID: TNyxText): INyxTable;
begin
  Result := NewNyxTable(AID);
  Result.Binds.Collection(NyxCollectionView(NyxCollection('people')).
    Column(NyxTextField('name'), 'Name').Column(NyxIntegerField('rank'), 'Rank').
    Scoped(csInstance));
end;

function Options(const ATarget: TNyxText): TNyxDataToolbarOptions;
begin
  Result := NyxDataToolbarOptions(NyxControl(ATarget), NyxTextField('name')).
    QueryContext(NyxDataToolbarQueryOptions.Display(ntqdDetails).Caption('People 🌙')).
    Sort(NyxToolbarSort('Name 🚀 𐐷'), NyxCollectionQuery.OrderBy(NyxTextField('name')).
      ThenBy(NyxIntegerField('rank'), nsdDescending)).
    Sort(NyxToolbarSort('Rank first'), NyxSort(NyxIntegerField('rank'), nsdDescending));
end;

function Fixture: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LCard: INyxCard;
  LToolbar: INyxDataToolbar;
  LInstance: INyxComponent;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Collections.Define(NyxCollection('people'),
      NyxCollectionSchema.Text(NyxTextField('name'), '').Integer(NyxIntegerField('rank'), 0), [
      NyxCollectionItem(NyxItem(NyxCollection('people'), 'ada')).WithValue(NyxTextField('name'), 'Ada')]);
    LPage := NewNyxPage('home');
    LToolbar := NewNyxDataToolbar('local-tools');
    LToolbar.CreateButton.WithText('Invite a person');
    LPage.Add(LToolbar);
    LPage.Add(Grid('people'));
    LCard := NewNyxCard('directory');
    LToolbar := NewNyxDataToolbar('directory-tools');
    LToolbar.Configure.PartName(NyxPart('tools')).Done;
    LCard.Add(LToolbar);
    LCard.Add(Grid('directory-grid'));
    LDocument.AddComponent(LCard);
    LInstance := NewNyxComponent('left');
    LInstance.Reference := NyxComponent('directory');
    LInstance.Node.OverridePart(NyxPart('tools'), noAppend).Named('left-tools').
      Add(NewNyxLabel('local-note').WithText('Keep this note').Node);
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('right');
    LInstance.Reference := NyxComponent('directory');
    LPage.Add(LInstance);
    LToolbar := NewNyxDataToolbar('toolbar-template');
    LToolbar.Add(Grid('inner-grid'));
    LDocument.AddComponent(LToolbar);
    LInstance := NewNyxComponent('whole');
    LInstance.Reference := NyxComponent('toolbar-template');
    LPage.Add(LInstance);
    LDocument.AddPage(LPage);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  finally
    LDocument.Free;
  end;
end;

function PairText: TNyxText;
begin
  Result := EncodeNyxProject(GAgent.PreviewPair(GRevision, 'home'));
end;

function Operations(const APatch: INyxDesignPatch): TNyxDataValue;
begin
  Result := NyxProjectTransaction([NyxDesignStep(APatch)]).ToData;
end;

function Arguments(const AID: TNyxText; const AOperations: TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(AID)), NyxField('operations', AOperations)]);
end;

{ Construct invalid transport fixtures by member identity. JSON formatting is
  deliberately irrelevant: native and browser formatters may use different
  whitespace and must exercise the same actual malformed value. }
function WithMember(const AObject: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LFound: Boolean;
begin
  SetLength(LFields, AObject.Count + 1);
  LFound := False;
  for LIndex := 0 to AObject.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AObject.Key(LIndex), AObject.Field(AObject.Key(LIndex)));

    if AObject.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
      LFound := True;
    end;
  end;

  if LFound then
  begin
    SetLength(LFields, AObject.Count);
  end
  else
  begin
    LFields[AObject.Count] := NyxField(AName, AValue);
  end;
  Result := NyxObject(LFields);
end;

procedure Apply(const AID: TNyxText; const AOperations: TNyxDataValue);
begin
  GAgent.Call('nyx_transaction', 'Scooty', Arguments(AID, AOperations), 'toolbar-owner');
  Check(GAgent.Revision = GRevision + 1, 'a grouped command publishes one revision');
  GRevision := GAgent.Revision;
end;

function Details(const AID: TNyxText; const AExtra: TNyxText = ''): TNyxDataValue;
begin
  Result := GAgent.Call('nyx_node', 'Scooty', TNyxDataValue.ParseJSON(
    '{"expectedRevision":' + IntToStr(GRevision) + ',"id":"' + AID +
    '","dataToolbar":true,"limit":1' + AExtra + '}')).Field('dataToolbar');
end;

procedure Refuses(const AArguments: TNyxDataValue; const AReason: TNyxText);
var
  LBefore: TNyxText;
  LState: TNyxDataValue;
  LAfter: TNyxDataValue;
  LRefused: Boolean;
begin
  LBefore := PairText;
  LState := GAgent.Call('nyx_session', 'Scooty', NyxObject([]));
  LRefused := False;
  try
    GAgent.Call('nyx_transaction', 'Scooty', AArguments, 'toolbar-owner');
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  LAfter := GAgent.Call('nyx_session', 'Scooty', NyxObject([]));
  Check(LRefused and (GAgent.Revision = GRevision) and (PairText = LBefore) and
    (LState.Field('canUndo').AsBoolean = LAfter.Field('canUndo').AsBoolean) and
    (LState.Field('canRedo').AsBoolean = LAfter.Field('canRedo').AsBoolean) and
    (LState.Field('selection').AsText = LAfter.Field('selection').AsText) and
    (LState.Field('view').AsText = LAfter.Field('view').AsText), AReason +
    ' (refused=' + BoolToStr(LRefused, True) + ', expected=' + IntToStr(GRevision) +
    ', actual=' + IntToStr(GAgent.Revision) + ', pair=' + BoolToStr(PairText = LBefore, True) + ')');
end;

procedure History(const ADirection: TNyxText);
begin
  GAgent.Call('nyx_history', 'Scooty', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(ADirection + IntToStr(GRevision))),
    NyxField('direction', NyxData(ADirection))]));
  GRevision := GAgent.Revision;
end;

procedure Run;
var
  LBefore: TNyxText;
  LAccepted: TNyxText;
  LData: TNyxDataValue;
  LPolicy: TNyxDataValue;
  LOperation: TNyxDataValue;
  LWire: TNyxDataValue;
  LPair: TNyxProjectPair;
  LOptions: TNyxDataToolbarOptions;
  LText: TNyxText;
  LIndex: Integer;
  LRefused: Boolean;
  LSteps: array of TNyxTransactionStep;
  {$ifndef PAS2JS}
  LTools: TNyxDataValue;
  {$endif}
begin
  GAgent := TNyxAgentSession.Create(Fixture);
  try
    GRevision := GAgent.Revision;
    LBefore := PairText;
    LData := Details('local-tools');
    Check(not LData.Field('localDeclared').AsBoolean and
      (LData.Field('localMode').AsText = 'inherited') and
      (LData.Field('effectiveMode').AsText = 'static') and
      (LData.Field('policy').Kind = ndNull), 'absence is a static recipe, not a disabled configuration');
    Apply('configure', NyxArray([
      Operations(NyxSetDataToolbarPolicy(NyxControl('local-tools'), Options('people'))).Item(0),
      Operations(NyxSetDataToolbarPolicy(NyxControl('directory-tools'), Options('directory-grid'))).Item(0)]));
    LAccepted := PairText;
    LData := Details('local-tools');
    LPolicy := LData.Field('policy');
    Check(LData.Field('localDeclared').AsBoolean and
      (LData.Field('effectiveMode').AsText = 'configured') and
      (LPolicy.Field('target').Field('value').AsText = 'people') and
      (LPolicy.Field('searchField').Field('value').AsText = 'name'), 'typed target/search readback');
    Check((LPolicy.Field('sortTotal').AsInteger = 2) and
      (LPolicy.Field('sorts').Item(0).Field('keyTotal').AsInteger = 2) and
      (LPolicy.Field('queryContext').Field('display').AsText = 'details'), 'sort keys and opt-in query context');
    Check((LPolicy.Field('sorts').Item(0).Field('name').Field('value').AsText = TNyxText('Name 🚀 𐐷')) and
      (LPolicy.Field('sorts').Item(0).Field('keys').Item(1).Field('kind').AsText = 'integer'),
      'supplementary Unicode and numeric sort families remain exact');
    LData := Details('local-tools', ',"toolbarSortOffset":1,"toolbarSortLimit":1,"toolbarKeyOffset":1,"toolbarKeyLimit":1');
    LPolicy := LData.Field('policy');
    Check((LPolicy.Field('sorts').Count = 1) and
      (LPolicy.Field('sorts').Item(0).Field('index').AsInteger = 1) and
      (LPolicy.Field('sorts').Item(0).Field('keys').Count = 0), 'independent sort/key paging retains absolute indexes');
    LData := Details('local-tools', ',"textOffset":5,"textLimit":1,"toolbarSortLimit":1');
    LWire := LData.Field('policy').Field('sorts').Item(0).Field('name');
    Check((LWire.Field('value').AsText = TNyxText('🚀')) and
      (LWire.Field('totalScalars').AsInteger = 8) and LWire.Field('truncated').AsBoolean,
      'text windows split by Unicode scalar with explicit truncation');
    LData := Details('left-tools');
    Check(not LData.Field('localDeclared').AsBoolean and
      (LData.Field('effectiveMode').AsText = 'configured') and
      (LData.Field('policy').Field('target').Field('value').AsText = 'directory-grid'),
      'named reusable override inspects its inherited scoped target');
    LData := Details('left-tools', ',"toolbarScope":"local"');
    Check(LData.Field('policy').Kind = ndNull, 'local inspection does not flatten inherited policy');
    Apply('local-disable', Operations(NyxDisableDataToolbarPolicy(NyxControl('left-tools'))));
    LData := Details('left-tools');
    Check((LData.Field('localMode').AsText = 'disabled') and
      (LData.Field('effectiveMode').AsText = 'disabled') and (LData.Field('policy').Kind = ndNull),
      'explicit disabled mask is distinct from inheritance');
    Check(Details('directory-tools').Field('effectiveMode').AsText = 'configured',
      'local mask preserves the reusable definition');
    Apply('local-inherit', Operations(NyxInheritDataToolbarPolicy(NyxControl('left-tools'))));
    Check(not Details('left-tools').Field('localDeclared').AsBoolean and
      (Details('left-tools').Field('effectiveMode').AsText = 'configured'), 'inherit restores the definition');
    Apply('whole-configure', Operations(NyxSetDataToolbarPolicy(NyxControl('whole'), Options('inner-grid'))));
    Check((Details('whole').Field('effectiveMode').AsText = 'configured') and
      (Details('toolbar-template').Field('effectiveMode').AsText = 'static'),
      'whole reusable instance configuration preserves the definition');
    LPair := GAgent.PreviewPair(GRevision, 'home');
    Check((Pos('Invite a person', LPair.Design) > 0) and (Pos('Keep this note', LPair.Design) > 0),
      'custom parts survive policy and role changes');
    Check((Pos('INyxDataToolbar', LPair.Source) > 0) and
      (Pos('NyxDataToolbarOptions', LPair.Source) > 0) and
      (Pos('data-toolbar-policy', LPair.Source) = 0), 'source remains crafted specialized Pascal');
    LAccepted := PairText;
    Apply('clear-local', Operations(NyxInheritDataToolbarPolicy(NyxControl('local-tools'))));
    Check(Details('local-tools').Field('effectiveMode').AsText = 'static', 'inherit clears standalone intent');
    LBefore := PairText;
    History('undo');
    Check(PairText = LAccepted, 'one Undo restores the complete policy/roles/source pair');
    History('redo');
    Check(PairText = LBefore, 'one Redo restores the exact admitted pair');
    History('undo');

    { Open captions deliberately have no arbitrary authoring length ceiling.
      Inspection must stay bounded without corrupting their exact identities. }
    LBefore := PairText;
    LText := TNyxText(StringOfChar('x', 6000));
    LOptions := NyxDataToolbarOptions(NyxControl('people'), NyxTextField('name')).
      Baseline(NyxToolbarSort(LText + TNyxText('🌙'))).
      Sort(NyxToolbarSort(LText + TNyxText('🚀')), NyxSort(NyxTextField('name')));
    Apply('long-captions', Operations(NyxSetDataToolbarPolicy(NyxControl('local-tools'), LOptions)));
    LData := Details('local-tools', ',"textLimit":8,"toolbarSortLimit":1,"toolbarKeyLimit":1');
    Check((Length(LData.ToJSON) < 3500) and
      (LData.Field('policy').Field('baseline').Field('totalScalars').AsInteger = 6001),
      'large open captions cannot force an unbounded inspection response');
    LData := Details('local-tools', ',"textOffset":6000,"textLimit":1,"toolbarSortLimit":1');
    Check((LData.Field('policy').Field('baseline').Field('value').AsText = TNyxText('🌙')) and
      (LData.Field('policy').Field('sorts').Item(0).Field('name').Field('value').AsText = TNyxText('🚀')),
      'paged long identities retain their exact supplementary scalar tails');
    History('undo');
    Check(PairText = LBefore, 'long-caption history restores every original selector/domain/source byte');

    LOperation := Operations(NyxSetDataToolbarPolicy(NyxControl('local-tools'), Options('people'))).Item(0);
    LData := NyxObject([NyxField('expectedRevision', NyxData(1)),
      NyxField('operationId', NyxData('stale')), NyxField('operations', NyxArray([LOperation]))]);
    Refuses(LData, 'stale revision refuses without partial role/source mutation');
    Refuses(Arguments('missing-target', Operations(NyxSetDataToolbarPolicy(
      NyxControl('local-tools'), Options('absent')))), 'missing target refuses atomically');
    LOptions := NyxDataToolbarOptions(NyxControl('people'), NyxTextField('rank'));
    Refuses(Arguments('wrong-field-kind', Operations(NyxSetDataToolbarPolicy(
      NyxControl('local-tools'), LOptions))), 'search requires an exact text field');
    Refuses(Arguments('wrong-host', Operations(NyxSetDataToolbarPolicy(
      NyxControl('people'), Options('people')))), 'non-toolbar owner refuses');
    Refuses(Arguments('unknown-field', NyxArray([WithMember(LOperation, 'extra', NyxData(True))])),
      'unknown operation fields refuse');
    LWire := LOperation.Field('options');
    Refuses(Arguments('wrong-scalar', NyxArray([WithMember(LOperation, 'options',
      WithMember(LWire, 'version', NyxData(True)))])), 'Boolean cannot masquerade as a version number');
    Refuses(Arguments('wrong-enum', NyxArray([WithMember(LOperation, 'options',
      WithMember(LWire, 'match', NyxData('fuzzy')))])), 'closed matching vocabulary refuses unknown behavior');
    LData := WithMember(LWire.Field('sorts').Item(0), 'query',
      WithMember(LWire.Field('sorts').Item(0).Field('query'), 'filter',
        NyxCollectionQuery.Where(NyxWhere(NyxTextField('name')).EqualTo('Ada')).ToData.Field('filter')));
    Refuses(Arguments('filter-authority', NyxArray([WithMember(LOperation, 'options',
      WithMember(LWire, 'sorts', NyxArray([LData])))])), 'sort choices cannot inject filter authority');
    Refuses(Arguments('late-refusal', NyxArray([
      Operations(NyxDisableDataToolbarPolicy(NyxControl('local-tools'))).Item(0),
      Operations(NyxSetDataToolbarPolicy(NyxControl('left-tools'), Options('absent'))).Item(0)])),
      'a late failure rolls back every earlier operation in the group');

    { Baselines must be captured at the ordered detached step. The new collection,
      view binding and toolbar do not exist in the session's initial tree. }
    SetLength(LSteps, 3);
    LSteps[0] := NyxDesignStep(ReadNyxDesignPatch(TNyxDataValue.ParseJSON(
      '[{"op":"create","kind":"data-toolbar","id":"fresh-tools","parent":"home"},' +
      '{"op":"create","kind":"table","id":"fresh-grid","parent":"home"}]')));
    LSteps[1] := NyxCollectionStep(NyxCollectionPatch([
      NyxDefineCollection(NyxCollection('fresh'), NyxCollectionSchema.Text(NyxTextField('name'), ''), []),
      NyxBindCollection(NyxBindingOwner('fresh-grid'), cpTable,
        NyxCollectionView(NyxCollection('fresh')).Column(NyxTextField('name'), 'Name'))]));
    LSteps[2] := NyxDesignStep(NyxSetDataToolbarPolicy(NyxControl('fresh-tools'),
      NyxDataToolbarOptions(NyxControl('fresh-grid'), NyxTextField('name'))));
    Apply('fresh-composition', NyxProjectTransaction(LSteps).ToData);
    Check(Details('fresh-tools').Field('policy').Field('target').Field('value').AsText = 'fresh-grid',
      'one mixed transaction creates collection/view and configures its new toolbar');

    LPair := GAgent.PreviewPair(GRevision, 'home');
    LPair.Pending := True;
    LPair.DraftBase := LPair.Source;
    LPair.Draft := LPair.Source + TNyxText(#10 + '{ unfinished custom work }');
    GAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('local-tools')), NyxField('view', NyxData('home'))]));
    GRevision := GAgent.Revision;
    Refuses(Arguments('pending-draft', NyxArray([LOperation])), 'pending custom source survives semantic refusal');

    LRefused := False;
    try
      Details('local-tools', ',"toolbarScope":"runtime"');
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'unknown inspection scope refuses');
    {$ifndef PAS2JS}
      LTools := NyxStudioMCPTools.Field('tools');
      for LIndex := 0 to LTools.Count - 1 do
      begin
        LData := LTools.Item(LIndex);

        if LData.Field('name').AsText = 'nyx_transaction' then
        begin
          LText := LData.Field('inputSchema').ToJSON;
          Check((Pos('data-toolbar-set', LText) > 0) and (Pos('data-toolbar-disable', LText) > 0) and
            (Pos('data-toolbar-inherit', LText) > 0) and (Pos('nyxQuerySort', LText) > 0),
            'actual advertised transaction schema exposes the compact policy and shared typed sort');
        end;

        if LData.Field('name').AsText = 'nyx_node' then
        begin
          Check(Pos('toolbarKeyLimit', LData.Field('inputSchema').ToJSON) > 0,
            'actual node discovery advertises bounded toolbar inspection');
        end;
      end;
    WriteLn('PASS ', GChecks, ' semantic DataToolbar checks');
    {$else}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' semantic DataToolbar checks';
    document.body.setAttribute('data-nyx-agent-toolbars', 'passed');
    {$endif}
  finally
    GAgent.Free;
  end;
end;

begin
  try
    Run;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := LException.Message;
      document.body.setAttribute('data-nyx-agent-toolbars', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
