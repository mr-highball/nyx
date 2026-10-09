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


program nyx_agent_resource_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.bytes, nyx.types, nyx.controls,
  nyx.model, nyx.catalog, nyx.codec, nyx.codegen, nyx.composition, nyx.binding,
  nyx.binding.types, nyx.resources, nyx.resource.sources, nyx.images,
  nyx.studio.projects, nyx.studio.agents, nyx.studio.resourceedits,
  nyx.studio.transactions, nyx.studio.edits
  {$ifdef PAS2JS}, JS, Web{$else}, Classes, nyx.studio.mcp, nyx.studio.directories,
  nyx.studio.outputs, nyx.studio.workspaces, nyx.test.resource.runtime{$endif};

const
  COriginal: TNyxText = '{ "literal.dot": "Ready 🌙", "prompt": "Your project", "price": 9007199254740993.1250, "items": [true, null] }';
  CReplacement: TNyxText = '{ "title": "Tomorrow 🌙", "prompt": "Keep building", "price": 9007199254740993.1250, "items": [true, null] }';
  CExact: TNyxText = 'Hello 🌙' + #0 + ' tomorrow';
  { Pascal-created PNG shared with the maintained image-authoring qualification;
    retain real chunk checksums rather than weakening image admission for a fixture. }
  CPNG: TNyxText = 'iVBORw0KGgoAAAANSUhEUgAAAGQAAAAyEAIAAAB1xzWqAAAACXBIWXMAAAAAAAAAAACdYiYyAAABMElEQVR4nO3OsQ0AIAzAsP7/dOEEtsgSGTxnduf2fbE/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDxwO6T+sr8laFkAAAAABJRU5ErkJggg==';
  CNote: TNyxText = '{ Resource companion: retain this handwritten footer. }';

var
  GChecks: Integer;
  GRevision: Integer;
  GSeed: TNyxProjectPair;
  GAccepted: TNyxText;
  {$ifdef PAS2JS}
  GAgent: TNyxAgentSession;
  {$else}
  GEngine: TNyxStudioMCP;
  GToken: TNyxText;
  GWorkspace: TNyxWorkspaceRef;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Resource semantic workflow: ' + AReason);
  end;
  Inc(GChecks);
end;

function Seed: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LCard: INyxColumn;
  LCaption: INyxLabel;
  LInstance: INyxComponent;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Resource companion';
    LPage := NewNyxPage('home');
    LPage.Configure.Layout(nlColumn).Gap(16).Padding(24).Done;
    LDocument.AddPage(LPage);
    LPage.Add(NewNyxLabel('headline').WithText('Build for tomorrow'));
    LPage.Add(NewNyxInput('project-name').Configure.Placeholder('Project name').Done);
    LCard := NewNyxColumn('welcome-card', ncoDescriptor);
    LCaption := NewNyxLabel('welcome-caption').WithText('Welcome');
    LCaption.Configure.PartName(NyxPart('caption')).Done;
    LCard.Add(LCaption);
    LDocument.AddComponent(LCard);
    LInstance := NewNyxComponent('first-card');
    LInstance.Configure.Component(NyxComponent('welcome-card')).Done;
    LInstance.OverridePart(NyxPart('caption'), noProperties).Named('first-caption');
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('second-card');
    LInstance.Configure.Component(NyxComponent('welcome-card')).Done;
    LPage.Add(LInstance);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument) +
      TNyxText(#10) + CNote + TNyxText(#10));
  finally
    LDocument.Free;
  end;
end;

function Call(const ATool: TNyxText; const AArgs: TNyxDataValue;
  const AOwner: TNyxText = 'resource-owner'): TNyxDataValue;
begin
  {$ifdef PAS2JS}
  Result := GAgent.Call(ATool, 'Scooty', AArgs, AOwner);
  {$else}
  Result := GEngine.InvokeTool(ATool, AOwner, 'Scooty',
    NyxWithWorkspace(AArgs, GWorkspace));
  {$endif}
end;

function Editor(const AArgs: TNyxDataValue): TNyxDataValue;
begin
  {$ifdef PAS2JS}
  Result := GAgent.Exchange(AArgs);
  {$else}
  Result := GEngine.EditorExchange(GToken, NyxWithWorkspace(AArgs, GWorkspace));
  {$endif}
end;

function Snapshot: TNyxDataValue;
begin
  Result := Editor(NyxObject([NyxField('op', NyxData('observe')),
    NyxField('after', NyxData(0))]));
end;

function PairText: TNyxText;
begin
  Result := Snapshot.Field('project').AsText;
end;

function Arguments(const AID: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('mode', NyxData('apply')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(AID)), NyxField('changes', AChanges)]);
end;

function Apply(const AID: TNyxText; const AChanges: array of TNyxResourceChange): TNyxDataValue;
begin
  Result := Call('nyx_resources', Arguments(AID, NyxResourcePatch(AChanges).ToData));
  GRevision := Result.Field('revision').AsInteger;
end;

function Query(const AMode, AName: TNyxText; const AExtra: array of TNyxDataField): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, 3 + Length(AExtra));
  LFields[0] := NyxField('mode', NyxData(AMode));
  LFields[1] := NyxField('name', NyxData(AName));
  LFields[2] := NyxField('locale', NyxData(''));
  for LIndex := 0 to High(AExtra) do
  begin
    LFields[3 + LIndex] := AExtra[LIndex];
  end;
  Result := Call('nyx_resources', NyxObject(LFields));
end;

procedure Refuses(const ATool: TNyxText; const AArgs: TNyxDataValue;
  const AReason: TNyxText; const AOwner: TNyxText = 'resource-owner');
var
  LBefore: TNyxText;
  LState: TNyxDataValue;
  LAfter: TNyxDataValue;
  LFailed: Boolean;
begin
  LBefore := PairText;
  LState := Snapshot.Field('session');
  LFailed := False;
  try
    Call(ATool, AArgs, AOwner);
  except
    on Exception do
    begin
      LFailed := True;
    end;
  end;
  LAfter := Snapshot.Field('session');
  Check(LFailed and (PairText = LBefore) and
    (LState.Field('revision').AsInteger = LAfter.Field('revision').AsInteger) and
    (LState.Field('canUndo').AsBoolean = LAfter.Field('canUndo').AsBoolean) and
    (LState.Field('canRedo').AsBoolean = LAfter.Field('canRedo').AsBoolean) and
    (LState.Field('selection').AsText = LAfter.Field('selection').AsText) and
    (LState.Field('view').AsText = LAfter.Field('view').AsText), AReason);
end;

procedure History(const ADirection, AID: TNyxText); forward;

{$ifdef PAS2JS}
{ Allow the actual host to answer inspection between complete semantic phases.
  No mutation, clock or admission is accelerated and no assertion is skipped. }
procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 25);
end;
{$endif}

{ The existing public MCP/core journey consumes metadata-only changes, then
  checks the observing editor's complete paired value. No listener is started;
  installed/authenticated HTTP and physical input remain separate evidence. }
procedure DiscoveryChecks; {$ifdef PAS2JS}async;{$endif}
const
  CTag: TNyxText = 'Docs, "quick" 🌙';
var
  LBefore: TNyxText;
  LAfter: TNyxText;
  LResult: TNyxDataValue;
  LArgs: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LOldDocument: TNyxDocument;
  LDocument: TNyxDocument;
  LLabels: TNyxResourceLabels;
  LIndex: Integer;
  LQuery: TNyxDataValue;

  function List(const AQuery: TNyxDataValue; AOffset: Integer = 0;
    ALimit: Integer = 8): TNyxDataValue;
  begin
    Result := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('list')),
      NyxField('query', AQuery), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('offset', NyxData(AOffset)), NyxField('limit', NyxData(ALimit))]));
  end;

begin
  LBefore := PairText;
  LLabels := NyxResourceLabels.Add(NyxResourceLabel('Onboarding')).Add(NyxResourceLabel(CTag));
  LArgs := Arguments('tag-project-files', NyxResourcePatch([
    NyxSetResourceLabels(NyxResourceRef('copy'), NyxDefaultLocale, LLabels),
    NyxSetResourceLabels(NyxResourceRef('copy'), NyxLocale('en-US'),
      NyxResourceLabels.Add(NyxResourceLabel('Onboarding'))),
    NyxSetResourceLabels(NyxResourceRef('remote-copy'), NyxDefaultLocale,
      NyxResourceLabels.Add(NyxResourceLabel('Onboarding'))),
    NyxSetResourceLabels(NyxResourceRef('logo'), NyxDefaultLocale,
      NyxResourceLabels.Add(NyxResourceLabel('Media')))]).ToData);
  Check(Pos('"content"', LArgs.ToJSON) = 0,
    'metadata-only semantic group resends no file payload');
  LReceipt := Call('nyx_resources', LArgs);
  GRevision := LReceipt.Field('revision').AsInteger;
  Check(LReceipt.Field('resources').Field('changes').AsInteger = 4,
    'one semantic operation tags exact embedded/hosted/localized/image variants');
  LAfter := PairText;
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@Pause)));{$endif}
  Check(Call('nyx_resources', LArgs).ToJSON = LReceipt.ToJSON,
    'tag operation retries retain the exact actor-scoped receipt');
  Refuses('nyx_resources', LArgs, 'another actor cannot replay tag authority', 'other-owner');
  LOldDocument := TNyxCodec.Decode(DecodeNyxProject(LBefore).Design);
  LDocument := TNyxCodec.Decode(DecodeNyxProject(LAfter).Design);
  try
    for LIndex := 0 to LDocument.Resources.Count - 1 do
    begin
      Check(NyxResourceDiscovery(LDocument.Resources.Definition(LDocument.Resources.Reference(LIndex),
        LDocument.Resources.Locale(LIndex))).WithLabels(NyxResourceLabels).ToData.ToJSON =
        LOldDocument.Resources.Definition(LOldDocument.Resources.Reference(LIndex),
        LOldDocument.Resources.Locale(LIndex)).ToData.ToJSON,
        'tag changes retain complete original content/source/cache/fallback/help');
    end;
    Check(LDocument.Find('headline').Bindings[0].Same(
      LOldDocument.Find('headline').Bindings[0]) and
      LDocument.Find('project-name').Bindings[0].Same(
      LOldDocument.Find('project-name').Bindings[0]),
      'tag-only admission retains consumer binding descriptors');
  finally
    LDocument.Free;
    LOldDocument.Free;
  end;
  History('undo', 'tag-project-undo');
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@Pause)));{$endif}
  Check(PairText = LBefore, 'one Undo restores exact source and all untagged variants');
  History('redo', 'tag-project-redo');
  Check(PairText = LAfter, 'one Redo restores the complete labelled design/source pair');
  Check(Pos(CNote, DecodeNyxProject(LAfter).Source) > 0,
    'tag reconciliation retains handwritten source outside the managed views');
  LResult := List(NyxObject([NyxField('search', NyxData('ONBOARDING'))]), 0, 2);
  Check((LResult.Field('total').AsInteger = 3) and
    (LResult.Field('resources').Count = 2) and (LResult.Field('nextOffset').AsInteger = 2),
    'shared folded discovery pages matching label metadata across variants');
  Check((LResult.Field('resources').Item(0).Field('labelCount').AsInteger = 2) and
    (Pos('"labels"', LResult.ToJSON) = 0) and (Pos('"content"', LResult.ToJSON) = 0),
    'small discovery summaries expose counts without tag arrays or payload');
  Check(List(NyxObject([NyxField('search', NyxData('ONBOARDING'))]), 2, 2)
    .Field('resources').Count = 1, 'next discovery page continues in stable catalog order');
  Check(List(NyxObject([NyxField('search', NyxData('onboarding')),
    NyxField('comparison', NyxData('exact'))])).Field('total').AsInteger = 0,
    'exact scalar search remains an explicit choice');
  Check(List(NyxObject([NyxField('kinds', NyxArray([]))])).Field('total').AsInteger = 0,
    'an explicitly empty category set matches nothing');
  LQuery := NyxObject([NyxField('kinds', NyxArray([NyxData('json')])),
    NyxField('sources', NyxData('hosted')), NyxField('locales', NyxData('default')),
    NyxField('labels', NyxArray([NyxData('Onboarding')]))]);
  LResult := List(LQuery);
  Check((LResult.Field('total').AsInteger = 1) and
    (LResult.Field('resources').Item(0).Field('name').AsText = 'remote-copy'),
    'category/source/locale/tag restrictions intersect without fetching a URL');
  Check(List(NyxObject([NyxField('locales', NyxData('localized')),
    NyxField('labels', NyxArray([NyxData('Onboarding')]))])).Field('total').AsInteger = 1,
    'localized category selects only the exact localized variant');
  Check(List(NyxObject([NyxField('labels', NyxArray([NyxData('Onboarding'), NyxData(CTag)]))]))
    .Field('total').AsInteger = 1, 'all matching uses exact independent comma/quote/Unicode tags');
  Check(List(NyxObject([NyxField('labels', NyxArray([NyxData('Onboarding'), NyxData('Missing')])),
    NyxField('labelMatch', NyxData('any'))])).Field('total').AsInteger = 3,
    'any matching keeps useful alternatives');
  LResult := Query('labels', 'copy', [NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('limit', NyxData(1))]);
  Check((LResult.Field('total').AsInteger = 2) and (LResult.Field('nextOffset').AsInteger = 1) and
    (LResult.Field('labels').Item(0).AsText = 'Onboarding'), 'tag inspection pages exact insertion order');
  LResult := Query('labels', 'copy', [NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('offset', NyxData(1)), NyxField('limit', NyxData(1))]);
  Check(LResult.Field('labels').Item(0).AsText = CTag,
    'a bounded tag page returns a complete Unicode name without delimiter splitting');
  Check(Query('details', 'copy', [NyxField('expectedRevision', NyxData(GRevision))])
    .Field('labelCount').AsInteger = 2, 'exact metadata details disclose tag count');
  LResult := Snapshot;
  Check((LResult.Field('project').AsText = LAfter) and
    (LResult.Field('session').Field('revision').AsInteger = GRevision),
    'observing editor sees the latest complete tagged design/source pair');
  Check(Pos('Scooty', LResult.Field('activity').ToJSON) > 0,
    'operator activity visibly attributes semantic tag work');
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@Pause)));{$endif}
  LBefore := PairText;
  Apply('clear-one-variant-tags', [NyxSetResourceLabels(NyxResourceRef('copy'),
    NyxLocale('en-US'), NyxResourceLabels)]);
  Check(List(NyxObject([NyxField('labels', NyxArray([NyxData('Onboarding')]))]))
    .Field('total').AsInteger = 2, 'clearing exact locale tags leaves other variants untouched');
  History('undo', 'clear-one-variant-undo');
  Check(PairText = LBefore, 'cleared annotations restore with their paired source');
  Refuses('nyx_resources', NyxObject([NyxField('mode', NyxData('list')),
    NyxField('expectedRevision', NyxData(GRevision - 1)), NyxField('query', NyxObject([]))]),
    'stale metadata pages refuse after history moves the revision');
  Refuses('nyx_resources', NyxObject([NyxField('mode', NyxData('labels')),
    NyxField('name', NyxData('copy')), NyxField('locale', NyxData('')),
    NyxField('expectedRevision', NyxData(GRevision - 1))]), 'stale exact tag pages refuse');
  for LIndex := 0 to 8 do
  begin
    case LIndex of
      0: LQuery := TNyxDataValue.ParseJSON('{"labels":["Onboarding","Onboarding"]}');
      1: LQuery := TNyxDataValue.ParseJSON('{"labels":"Onboarding"}');
      2: LQuery := TNyxDataValue.ParseJSON('{"kinds":["json","json"]}');
      3: LQuery := TNyxDataValue.ParseJSON('{"kinds":["memo"]}');
      4: LQuery := TNyxDataValue.ParseJSON('{"sources":true}');
      5: LQuery := TNyxDataValue.ParseJSON('{"locales":"en-US"}');
      6: LQuery := TNyxDataValue.ParseJSON('{"labelMatch":"perhaps"}');
      7: LQuery := TNyxDataValue.ParseJSON('{"comparison":"fold-every-script"}');
      8: LQuery := TNyxDataValue.ParseJSON('{"unknown":true}');
    end;
    Refuses('nyx_resources', NyxObject([NyxField('mode', NyxData('list')),
      NyxField('query', LQuery)]), 'malformed or unknown structured discovery refuses unchanged');
    {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@Pause)));{$endif}
  end;
  Refuses('nyx_resources', TNyxDataValue.ParseJSON('{"mode":"list","filter":"","query":{}}'),
    'legacy and structured filter meanings cannot compete');
  Refuses('nyx_resources', Arguments('tag-late-failure', NyxResourcePatch([
    NyxSetResourceLabels(NyxResourceRef('copy'), NyxDefaultLocale, NyxResourceLabels),
    NyxRemoveResource(NyxResourceRef('missing'), NyxDefaultLocale)]).ToData),
    'a late grouped failure preserves all earlier candidate annotations and source');
  Refuses('nyx_resources', Arguments('tag-missing-locale', NyxResourcePatch([
    NyxSetResourceLabels(NyxResourceRef('copy'), NyxLocale('fr-FR'), LLabels)]).ToData),
    'tag changes require an exact variant instead of editing its default fallback');
  Refuses('nyx_resources', Arguments('tag-duplicate-wire', TNyxDataValue.ParseJSON(
    '[{"op":"set-labels","name":"copy","locale":"","labels":["Onboarding","Onboarding"]}]')),
    'duplicate tag wire refuses before publication');
  LBefore := PairText;
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@Pause)));{$endif}
  Apply('define-labelled-wire', [
    NyxDefineResource(NyxResourceRef('tag-review-embedded'), NyxDefaultLocale,
      NyxTextResource('Embedded review').Tagged(NyxResourceLabel(CTag))),
    NyxDefineResource(NyxResourceRef('tag-review-hosted'), NyxDefaultLocale,
      NyxResourceDiscovery(NyxHostedResource(nrkJSON,
        NyxResourceURL('https://example.invalid/tag-review.json'))
        .Fallback(NyxJSONResource(COriginal).Tagged(NyxResourceLabel('Offline'))))
        .Tagged(NyxResourceLabel('Review')))]);
  Check(Query('labels', 'tag-review-embedded', []).Field('labels').Item(0).AsText = CTag,
    'define dispatch admits canonical embedded version three labels');
  LDocument := TNyxCodec.Decode(DecodeNyxProject(PairText).Design);
  try
    Check(NyxResourceLabelsOf(LDocument.Resources.Definition(
      NyxResourceRef('tag-review-hosted'), NyxDefaultLocale).FallbackDefinition)
      .Item(0).Name = 'Offline',
      'canonical hosted version four preserves labelled embedded fallback version three');
  finally
    LDocument.Free;
  end;
  History('undo', 'define-labelled-wire-undo');
  Check(PairText = LBefore, 'temporary labelled definitions retire in one exact paired Undo');
end;

procedure History(const ADirection, AID: TNyxText);
begin
  GRevision := Call('nyx_history', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(AID)),
    NyxField('direction', NyxData(ADirection))])).Field('revision').AsInteger;
end;

procedure Configure(const APermission: TNyxText);
begin
  Editor(NyxObject([NyxField('op', NyxData('configure')),
    NyxField('after', NyxData(0)), NyxField('permission', NyxData(APermission))]));
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LArgs: TNyxDataValue;
  LResult: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LBindings: TNyxDataValue;
  LSchema: TNyxDataValue;
  LTools: TNyxDataValue;
  LDocument: TNyxDocument;
  LRuntime: TNyxNode;
  LPair: TNyxProjectPair;
  LBytes: TNyxBytes;
  LChanges: array of TNyxResourceChange;
  LSteps: array of TNyxTransactionStep;
  LIndex: Integer;
  LRejected: Boolean;
  LBefore: TNyxText;
  LInitial: TNyxText;
  LPolicy: TNyxResourceCachePolicy;
  {$ifndef PAS2JS}
  LStream: TFileStream;
  LWorkspace: TNyxWorkspaceRef;
  LReview: TNyxText;
  {$endif}
begin
  GRevision := Call('nyx_session', NyxObject([])).Field('revision').AsInteger;
  LInitial := PairText;
  LResult := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('list'))]));
  Check(LResult.Field('total').AsInteger = 0, 'empty list is bounded and meaningful');

  LPolicy := NyxResourceCache.Persistent.FreshFor(120).StaleFor(300)
    .MaximumBytes(8192).ServerPolicy(rcspOverride);
  LReceipt := Apply('all-files', [
    NyxDefineResource(NyxResourceRef('copy'), NyxDefaultLocale,
      NyxJSONResource(COriginal).Describe('Project copy 🌙', 'Captions and prompts for the workshop.')),
    NyxDefineResource(NyxResourceRef('notes'), NyxDefaultLocale, NyxTextResource(CExact)),
    NyxDefineResource(NyxResourceRef('packed'), NyxDefaultLocale, NyxBinaryResource(NyxDecodeBase64('AAH+/w=='))),
    NyxDefineResource(NyxResourceRef('logo'), NyxDefaultLocale,
      NyxResourceFromBytes(nrkImage, NyxDecodeBase64(CPNG))),
    NyxDefineResource(NyxResourceRef('remote-copy'), NyxDefaultLocale,
      NyxHostedResource(nrkJSON, NyxResourceURL('https://example.invalid/project.json'))
        .Cache(LPolicy).Fallback(NyxJSONResource(COriginal))),
    NyxDefineResource(NyxResourceRef('future'), NyxDefaultLocale,
      NyxHostedResource(nrkText, NyxResourceURL('https://example.invalid/future.txt'))),
    NyxDefineResource(NyxResourceRef('copy'), NyxLocale('en-US'), NyxJSONResource(COriginal)),
    NyxBindResource(NyxControl('headline'), bpText, NyxResourceValue(NyxResourceRef('copy')).Field('literal.dot')),
    NyxBindResource(NyxControl('project-name'), bpPlaceholder, NyxResourceValue(NyxResourceRef('copy')).Field('prompt')),
    NyxBindResource(NyxControl('welcome-caption'), bpText, NyxResourceValue(NyxResourceRef('copy')).Field('literal.dot'))]);
  Check(LReceipt.Field('resources').Field('changes').AsInteger = 10,
    'one real MCP operation defines every file kind and binds multiple controls');
  LBefore := PairText;
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@Pause)));{$endif}
  History('undo', 'all-files-undo');
  Check(PairText = LInitial, 'one paired Undo restores the entire original project');
  History('redo', 'all-files-redo');
  Check(PairText = LBefore, 'one paired Redo restores exact file bytes, selectors and source');
  {$ifdef PAS2JS}await(DiscoveryChecks);{$else}DiscoveryChecks;{$endif}

  LArgs := Arguments('idempotent', NyxResourcePatch([
    NyxDefineResource(NyxResourceRef('scratch'), NyxDefaultLocale, NyxTextResource('Later'))]).ToData);
  LReceipt := Call('nyx_resources', LArgs);
  GRevision := LReceipt.Field('revision').AsInteger;
  Check(Call('nyx_resources', LArgs).ToJSON = LReceipt.ToJSON,
    'same transport retry returns exact receipt despite its original revision');
  Refuses('nyx_resources', Arguments('idempotent', NyxResourcePatch([
    NyxDefineResource(NyxResourceRef('scratch'), NyxDefaultLocale, NyxTextResource('Different'))]).ToData),
    'same operation identity cannot change arguments');
  Refuses('nyx_resources', LArgs, 'another authority cannot replay a stale receipt', 'other-owner');
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@Pause)));{$endif}

  LResult := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('list')),
    NyxField('limit', NyxData(2))]));
  Check((LResult.Field('resources').Count = 2) and
    (LResult.Field('nextOffset').AsInteger = 2) and (LResult.Field('total').AsInteger = 8),
    'metadata queries page variants without payloads');
  Check(Pos('"content"', LResult.ToJSON) = 0, 'list never includes full file content');
  LResult := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('list')),
    NyxField('filter', NyxData('prompts'))]));
  Check(LResult.Field('total').AsInteger = 1, 'search uses creator intent');
  LResult := Query('details', 'remote-copy', []);
  Check((LResult.Field('source').Field('cache').ToJSON = LPolicy.ToData.ToJSON) and
    LResult.Field('fallback').AsBoolean, 'hosted policy/override is an authored declaration');
  LResult := Query('content', 'remote-copy', [NyxField('count', NyxData(20))]);
  Check((LResult.Field('origin').AsText = 'authored-fallback') and
    (LResult.Field('content').Field('characters').AsInteger > 20),
    'hosted windows explicitly read authored fallback without network');
  Refuses('nyx_resources', NyxObject([NyxField('mode', NyxData('content')),
    NyxField('name', NyxData('future')), NyxField('locale', NyxData(''))]),
    'unresolved hosted payload is refused rather than fetched');

  LResult := Query('content', 'notes', [NyxField('offset', NyxData(6)), NyxField('count', NyxData(2))]);
  Check(LResult.Field('content').Field('text').AsText = TNyxText('🌙') + #0,
    'Unicode scalar windows retain supplementary text and NUL');
  LResult := Query('content', 'packed', [NyxField('offset', NyxData(1)), NyxField('count', NyxData(2))]);
  Check((LResult.Field('content').Field('base64').AsText = 'Af4=') and
    (LResult.Field('content').Field('nextOffset').AsInteger = 3),
    'binary windows count bytes and independently encode exact slices');
  LResult := Query('content', 'logo', [NyxField('count', NyxData(8))]);
  Check(LResult.Field('content').Field('base64').AsText = 'iVBORw0KGgo=',
    'image bytes use the same bounded resource contract');
  LResult := Query('content', 'copy', []);
  Check(LResult.Field('content').Field('text').AsText = COriginal,
    'JSON source window retains whitespace and decimal spelling');
  LResult := Query('json', 'copy', [NyxField('path', NyxResourcePath.ToData), NyxField('limit', NyxData(2))]);
  Check((LResult.Field('value').Field('children').Count = 2) and
    (LResult.Field('value').Field('total').AsInteger = 4) and
    (LResult.Field('value').Field('children').Item(0).Field('path').ToJSON = '["literal.dot"]'),
    'JSON pages preserve structural field names and immediate children');
  LResult := Query('json', 'copy', [NyxField('path', NyxResourcePath.Field('price').ToData)]);
  Check(LResult.Field('value').Field('value').AsDecimal.Text = '9007199254740993.1250',
    'exact numeric leaf never passes through Double');
  LResult := Query('json', 'copy', [NyxField('path', NyxResourcePath.Field('items').Item(1).ToData)]);
  Check(LResult.Field('value').Field('value').Kind = ndNull, 'JSON null discovery is explicit');
  LResult := Query('json', 'copy', [NyxField('path', NyxResourcePath.Field('literal.dot').ToData),
    NyxField('textOffset', NyxData(6)), NyxField('textCount', NyxData(1))]);
  Check(LResult.Field('value').Field('content').Field('text').AsText = TNyxText('🌙'),
    'structural text leaf uses scalar windows');

  LBindings := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('bindings')),
    NyxField('owner', NyxData('first-caption'))])).Field('bindings');
  Check(LBindings.Item(0).Field('inherited').AsBoolean and
    (LBindings.Item(0).Field('effective').Field('value').Field('resource').AsText = 'copy'),
    'bounded binding context reports inherited resource selectors');
  Apply('mask', [NyxClearResourceBinding(NyxControl('first-caption'), bpText)]);
  LBindings := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('bindings')),
    NyxField('owner', NyxData('first-caption'))])).Field('bindings');
  Check(LBindings.Item(0).Field('local').Field('cleared').AsBoolean,
    'clear deliberately masks the reusable binding');
  Apply('restore-inheritance', [NyxInheritResourceBinding(NyxControl('first-caption'), bpText)]);
  LBindings := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('bindings')),
    NyxField('owner', NyxData('first-caption'))])).Field('bindings');
  Check(LBindings.Item(0).Field('inherited').AsBoolean, 'inherit removes the local mask independently');
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@Pause)));{$endif}

  Refuses('nyx_resources', Arguments('referenced-remove', NyxResourcePatch([
    NyxRemoveResource(NyxResourceRef('copy'), NyxDefaultLocale)]).ToData),
    'referenced file removal retains all accepted consumers');
  Refuses('nyx_resources', Arguments('broken-replace', NyxResourcePatch([
    NyxDefineResource(NyxResourceRef('copy'), NyxDefaultLocale, NyxJSONResource(CReplacement))]).ToData),
    'replacement without dependent selector repairs refuses atomically');
  Apply('replace-and-repair', [
    NyxDefineResource(NyxResourceRef('copy'), NyxDefaultLocale,
      NyxJSONResource(CReplacement).Describe('Project copy 🌙', 'Designed for tomorrow.')),
    NyxBindResource(NyxControl('headline'), bpText, NyxResourceValue(NyxResourceRef('copy')).Field('title')),
    NyxBindResource(NyxControl('welcome-caption'), bpText, NyxResourceValue(NyxResourceRef('copy')).Field('title'))]);
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@Pause)));{$endif}
  LPair := DecodeNyxProject(PairText);
  LDocument := TNyxCodec.Decode(LPair.Design);
  LRuntime := nil;
  try
    LRuntime := RealizeNyxView(LDocument, LDocument.Pages[0]);
    ApplyNyxBindings(LRuntime, LDocument.State);
    Check(LRuntime.Find('headline').Prop('text') = TNyxText('Tomorrow 🌙'),
      'one final resource group repairs multiple consumers and projects a caption');
    Check(LRuntime.Find('project-name').Prop('placeholder') = 'Keep building',
      'retained prompt uses replaced data');
    Check(Pos('TNyxNode.Create', LPair.Source) = 0,
      'semantic resource edits retain specialized crafted generated declarations');
  finally
    LRuntime.Free;
    LDocument.Free;
  end;

  LBefore := PairText;
  LResult := Query('details', 'copy', [NyxField('offset', NyxData(20)), NyxField('count', NyxData(4))]);
  Check((PairText = LBefore) and
    (LResult.Field('title').Field('text').AsText = ''),
    'metadata tail windows clamp shorter titles and preserve the accepted pair');

  Refuses('nyx_resources', Arguments('wrong-scalar', NyxResourcePatch([
    NyxBindResource(NyxControl('headline'), bpText, NyxResourceValue(NyxResourceRef('copy')).Field('price'))]).ToData),
    'wrong scalar family refuses');
  Refuses('nyx_resources', Arguments('wrong-target', NyxResourcePatch([
    NyxBindResource(NyxControl('headline'), bpValue, NyxResourceValue(NyxResourceRef('copy')).Field('title'))]).ToData),
    'unsupported control property refuses');
  Refuses('nyx_resources', Arguments('late-owner', NyxResourcePatch([
    NyxDefineResource(NyxResourceRef('scratch'), NyxDefaultLocale, NyxTextResource('Staged')),
    NyxBindResource(NyxControl('missing-owner'), bpText, NyxResourceValue(NyxResourceRef('scratch')))]).ToData),
    'late group failure rolls back staged files and paired source');

  Apply('final-consumer', [
    NyxBindResource(NyxControl('headline'), bpEnabled,
      NyxResourceValue(NyxResourceRef('copy')).Field('title').AsBoolean),
    NyxBindResource(NyxControl('headline'), bpEnabled,
      NyxResourceValue(NyxResourceRef('copy')).Field('items').Item(0).AsBoolean)]);
  Check(Query('content', 'copy', []).Field('revision').AsInteger = GRevision,
    'a later typed repair admits the final consumer without intermediate publication');
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@Pause)));{$endif}

  Refuses('nyx_resources', TNyxDataValue.ParseJSON('{"mode":"list","limit":17}'), 'oversized page refuses');
  Refuses('nyx_resources', TNyxDataValue.ParseJSON('{"mode":"list","limit":"2"}'), 'numeric string refuses');
  Refuses('nyx_resources', TNyxDataValue.ParseJSON('{"mode":"list","extra":true}'), 'unknown member refuses');
  Refuses('nyx_resources', TNyxDataValue.ParseJSON('{"mode":"json","name":"copy","locale":"","path":["items",-1]}'),
    'negative structural index refuses');
  Refuses('nyx_resources', TNyxDataValue.ParseJSON('{"mode":"content","name":"notes","locale":"","offset":999}'),
    'out-of-range scalar cursor refuses');
  Refuses('nyx_resources', TNyxDataValue.ParseJSON('{"mode":"json","name":"copy","locale":"","path":["price"],"textCount":2}'),
    'irrelevant numeric-leaf text cursor refuses');

  SetLength(LChanges, 33);
  for LIndex := 0 to High(LChanges) do
  begin
    LChanges[LIndex] := NyxDefineResource(NyxResourceRef('scratch'), NyxDefaultLocale, NyxTextResource('Bounded'));
  end;
  LRejected := False;
  try
    NyxResourcePatch(LChanges);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'resource group retains its 32-leaf limit');
  SetLength(LChanges, 32);
  SetLength(LSteps, 3);
  for LIndex := 0 to High(LSteps) do
  begin
    LSteps[LIndex] := NyxResourceStep(NyxResourcePatch(LChanges));
  end;
  LRejected := False;
  try
    NyxProjectTransaction(LSteps);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'resource groups cannot bypass transaction total budget');

  LBefore := PairText;
  LArgs := NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('mixed')), NyxField('operations', NyxArray([
      NyxObject([NyxField('op', NyxData('title')), NyxField('value', NyxData('Resource companion'))]),
      NyxObject([NyxField('op', NyxData('resources')), NyxField('changes',
        NyxResourcePatch([NyxDefineResource(NyxResourceRef('scratch'), NyxDefaultLocale,
          NyxTextResource('Mixed semantic edit')),
          NyxSetResourceLabels(NyxResourceRef('scratch'), NyxDefaultLocale,
            NyxResourceLabels.Add(NyxResourceLabel('Experiment')))]).ToData)])]))]);
  LResult := Call('nyx_transaction', LArgs);
  GRevision := LResult.Field('revision').AsInteger;
  Check(Query('labels', 'scratch', []).Field('labels').Item(0).AsText = 'Experiment',
    'an interleaved design transaction uses the same typed resource annotation operation');
  History('undo', 'mixed-undo');
  Check(PairText = LBefore, 'interleaved resource/design domains publish one paired Undo');
  History('redo', 'mixed-redo');

  Configure('readOnly');
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@Pause)));{$endif}
  Refuses('nyx_resources', Arguments('read-only-tags', NyxResourcePatch([
    NyxSetResourceLabels(NyxResourceRef('scratch'), NyxDefaultLocale, NyxResourceLabels)]).ToData),
    'read-only operator policy refuses payload-free annotation changes too');
  Refuses('nyx_resources', Arguments('read-only', NyxResourcePatch([
    NyxRemoveResource(NyxResourceRef('scratch'), NyxDefaultLocale)]).ToData),
    'read-only operator policy blocks mutations');
  Check(Query('content', 'notes', []).Field('content').Field('text').AsText = CExact,
    'read-only queries remain usable');
  Configure('disabled');
  Refuses('nyx_resources', TNyxDataValue.ParseJSON('{"mode":"list"}'), 'disabled policy blocks discovery');
  Configure('edit');
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@Pause)));{$endif}

  LBefore := PairText;
  LPair := DecodeNyxProject(LBefore);
  LPair.Pending := True;
  LPair.DraftBase := LPair.Source;
  LPair.Draft := LPair.Source + #10 + '{ A retained draft. }';
  Editor(NyxObject([NyxField('op', NyxData('commit')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('after', NyxData(0)),
    NyxField('project', NyxData(EncodeNyxProject(LPair))),
    NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
  GRevision := Snapshot.Field('session').Field('revision').AsInteger;
  Refuses('nyx_resources', Arguments('pending-tags', NyxResourcePatch([
    NyxSetResourceLabels(NyxResourceRef('scratch'), NyxDefaultLocale, NyxResourceLabels)]).ToData),
    'pending Pascal refuses tag changes without losing the retained draft');
  Refuses('nyx_resources', Arguments('pending', NyxResourcePatch([
    NyxRemoveResource(NyxResourceRef('scratch'), NyxDefaultLocale)]).ToData),
    'pending Pascal refuses resource mutations without losing the draft');
  Editor(NyxObject([NyxField('op', NyxData('commit')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('after', NyxData(0)),
    NyxField('project', NyxData(LBefore)),
    NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
  GRevision := Snapshot.Field('session').Field('revision').AsInteger;
  GAccepted := PairText;

  LSchema := NyxResourceAgentSchema;
  Check((LSchema.Field('oneOf').Count = 11) and
    (LSchema.Field('oneOf').Item(2).Field('properties').Field('count')
      .Field('maximum').AsInteger = 4096), 'published schema closes and bounds every mode');
  Check((LSchema.Field('oneOf').Item(7).Field('properties').Field('mode').Field('const').AsText = 'rows') and
    (LSchema.Field('oneOf').Item(7).Field('properties').Field('limit').Field('maximum').AsInteger = 16) and
    not LSchema.Field('oneOf').Item(7).Field('additionalProperties').AsBoolean,
    'saved recipe discovery advertises a closed bounded field page');
  Check((LSchema.Field('oneOf').Item(0).Field('properties').Field('query')
    .Field('additionalProperties').AsBoolean = False) and
    (LSchema.Field('oneOf').Item(10).Field('properties').Field('limit')
    .Field('maximum').AsInteger = 16), 'MCP advertises closed discovery and bounded tag pages');
  LResult := LSchema.Field('oneOf').Item(5).Field('properties').Field('changes')
    .Field('items').Field('oneOf');
  Check((LResult.Item(0).Field('properties').Field('definition').Field('oneOf').Count = 4) and
    (LResult.Item(0).Field('properties').Field('definition').Field('oneOf').Item(1)
      .Field('properties').Field('labels').Field('minItems').AsInteger = 1) and
    (LResult.Item(LResult.Count - 1).Field('properties').Field('op').Field('const').AsText = 'set-labels'),
    'definition discovery advertises canonical labelled versions and payload-free annotation edits');
  {$ifndef PAS2JS}
  LTools := NyxStudioMCPTools.Field('tools');
  LRejected := True;
  for LIndex := 0 to LTools.Count - 1 do
  begin

    if LTools.Item(LIndex).Field('name').AsText = 'nyx_resources' then
    begin
      LRejected := False;
    end;
  end;
  Check(not LRejected, 'actual MCP discovery advertises the resource tool');

  LResult := Call('nyx_workspaces', NyxObject([NyxField('mode', NyxData('create')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('new-workspace')),
    NyxField('base', NyxData('accepted')), NyxField('label', NyxData('Resource experiment'))]));
  LWorkspace := NyxWorkspace(LResult.Field('workspace').AsText);
  GWorkspace := LWorkspace;
  GRevision := Call('nyx_session', NyxObject([])).Field('revision').AsInteger;
  Apply('workspace-resource', [NyxDefineResource(NyxResourceRef('scratch'), NyxDefaultLocale,
    NyxTextResource('A separate project'))]);
  Check(Query('content', 'scratch', []).Field('content').Field('text').AsText = 'A separate project',
    'new tool routes to an independent user workspace');
  GWorkspace := NyxPrimaryWorkspace;
  Check(PairText = GAccepted, 'workspace resource edits preserve the primary paired design');
  GRevision := Call('nyx_session', NyxObject([])).Field('revision').AsInteger;

  LResult := Call('nyx_reviews', NyxObject([NyxField('mode', NyxData('create')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('resource-review')),
    NyxField('base', NyxData('accepted')), NyxField('label', NyxData('Resource review'))]));
  LReview := LResult.Field('review').AsText;
  LResult := GEngine.InvokeTool('nyx_resources', 'resource-owner', 'Scooty', NyxObject([
    NyxField('review', NyxData(LReview)), NyxField('mode', NyxData('list'))]));
  Check(LResult.Field('total').AsInteger = 8, 'resource query routes through an owned review');
  Refuses('nyx_resources', NyxObject([NyxField('review', NyxData(LReview)),
    NyxField('workspace', NyxData(LWorkspace.ID)),
    NyxField('mode', NyxData('list'))]), 'competing workspace/review context refuses');
  LRejected := False;
  try
    GEngine.InvokeTool('nyx_resources', 'other-owner', 'Scooty', NyxObject([
      NyxField('review', NyxData(LReview)), NyxField('mode', NyxData('list'))]));
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (PairText = GAccepted), 'review capability remains transport-bound');
  GEngine.InvokeTool('nyx_reviews', 'resource-owner', 'Scooty', NyxObject([
    NyxField('mode', NyxData('discard')), NyxField('review', NyxData(LReview)),
    NyxField('expectedRevision', LResult.Field('revision')),
    NyxField('operationId', NyxData('resource-review-discard'))]));

  LPair := DecodeNyxProject(GAccepted);
  LBytes := NyxEncodeUTF8(LPair.Source);
  LStream := TFileStream.Create(ParamStr(2), fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
  {$endif}
end;

{$ifndef PAS2JS}
procedure StartEngine;
var
  LProfile: TNyxOutputConfiguration;
  LClaim: TNyxDataValue;
  LRuntime: TNyxText;
begin

  if ParamCount <> 2 then
  begin
    raise Exception.Create('Supply a NEW owned runtime and emitted Pascal destination');
  end;
  LRuntime := ExpandFileName(ParamStr(1));

  if DirectoryExists(LRuntime) then
  begin
    raise Exception.Create('Resource workflow runtime must be new and independently owned');
  end;
  LProfile := TNyxOutputConfiguration.Create;
  try
    { Constructor/ordinary public dispatch only. Do NOT Start: no listener,
      authentication claim, LAN replacement or observing browser is inferred.
      Configuration/recovery stays solely under this explicit private runtime. }
    GEngine := TNyxStudioMCP.Create(TNyxStudioDirectories.ForRepository(LRuntime),
      8638, 8639, LProfile.Encode);
    LClaim := GEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(GSeed))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LClaim.Field('token').AsText;
    GWorkspace := NyxPrimaryWorkspace;
  finally
    LProfile.Free;
  end;
end;
{$endif}

{ Browser qualification returns control to the actual host between complete
  semantic phases, allowing navigation and result inspection. The driver still
  owns its real-clock terminal bound and explicit browser retirement. }
procedure Execute; {$ifdef PAS2JS}async;{$endif}
begin
  try
    GSeed := Seed;
    {$ifdef PAS2JS}
    GAgent := TNyxAgentSession.Create(GSeed);
    {$else}
    StartEngine;
    {$endif}
    try
      {$ifdef PAS2JS}await(Run);{$else}Run;{$endif}
      {$ifndef PAS2JS}
      Inc(GChecks, RunNyxResourceRuntimeProtocol);
      {$endif}
      WriteLn('PASS ', GChecks, ' resource semantic checks');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-workbench-source', encodeURIComponent(DecodeNyxProject(GAccepted).Source));
      document.body.setAttribute('data-agent-resource-checks', IntToStr(GChecks));
      document.body.setAttribute('data-test-result', 'passed');
      {$endif}
    finally
      {$ifdef PAS2JS}GAgent.Free;{$else}GEngine.Free;{$endif}
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-agent-resource-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
      {$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  {$ifdef PAS2JS}
  window.setTimeout(@Execute, 100);
  {$else}
  Execute;
  {$endif}
end.
