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
  {$ifdef PAS2JS}, Web{$else}, Classes, nyx.studio.mcp, nyx.studio.directories,
  nyx.studio.outputs, nyx.studio.workspaces{$endif};

const
  COriginal: TNyxText = '{ "literal.dot": "Ready 🌙", "prompt": "Your project", "price": 9007199254740993.1250, "items": [true, null] }';
  CReplacement: TNyxText = '{ "title": "Tomorrow 🌙", "prompt": "Keep building", "price": 9007199254740993.1250, "items": [true, null] }';
  CExact: TNyxText = 'Hello 🌙' + #0 + ' tomorrow';
  CPNG: TNyxText = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+jB4sAAAAASUVORK5CYII=';

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
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
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

procedure Run;
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
  History('undo', 'all-files-undo');
  Check(PairText = LInitial, 'one paired Undo restores the entire original project');
  History('redo', 'all-files-redo');
  Check(PairText = LBefore, 'one paired Redo restores exact file bytes, selectors and source');

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
          NyxTextResource('Mixed semantic edit'))]).ToData)])]))]);
  LResult := Call('nyx_transaction', LArgs);
  GRevision := LResult.Field('revision').AsInteger;
  History('undo', 'mixed-undo');
  Check(PairText = LBefore, 'interleaved resource/design domains publish one paired Undo');
  History('redo', 'mixed-redo');

  Configure('readOnly');
  Refuses('nyx_resources', Arguments('read-only', NyxResourcePatch([
    NyxRemoveResource(NyxResourceRef('scratch'), NyxDefaultLocale)]).ToData),
    'read-only operator policy blocks mutations');
  Check(Query('content', 'notes', []).Field('content').Field('text').AsText = CExact,
    'read-only queries remain usable');
  Configure('disabled');
  Refuses('nyx_resources', TNyxDataValue.ParseJSON('{"mode":"list"}'), 'disabled policy blocks discovery');
  Configure('edit');

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
  Check((LSchema.Field('oneOf').Count = 8) and
    (LSchema.Field('oneOf').Item(2).Field('properties').Field('count')
      .Field('maximum').AsInteger = 4096), 'published schema closes and bounds every mode');
  Check((LSchema.Field('oneOf').Item(7).Field('properties').Field('mode').Field('const').AsText = 'rows') and
    (LSchema.Field('oneOf').Item(7).Field('properties').Field('limit').Field('maximum').AsInteger = 16) and
    not LSchema.Field('oneOf').Item(7).Field('additionalProperties').AsBoolean,
    'saved recipe discovery advertises a closed bounded field page');
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

begin
  try
    GSeed := Seed;
    {$ifdef PAS2JS}
    GAgent := TNyxAgentSession.Create(GSeed);
    {$else}
    StartEngine;
    {$endif}
    try
      Run;
      WriteLn('PASS ', GChecks, ' resource semantic checks');
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
    finally
      {$ifdef PAS2JS}GAgent.Free;{$else}GEngine.Free;{$endif}
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'failed');{$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
