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

program nyx_agent_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils,
  {$ifdef PAS2JS}Web,{$endif}
  nyx.text, nyx.data, nyx.types, nyx.contract, nyx.codec,
  nyx.studio.session, nyx.studio.projects, nyx.studio.agents;

var
  LSession: TNyxAgentSession;
  LValue: TNyxDataValue;
  LArgs: TNyxDataValue;
  LRevision: Integer;
  LCount: Integer;
  LRejected: Boolean;
  LPair: TNyxProjectPair;
  LBefore: TNyxText;
  LAuthor: TNyxStudioSession;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(LCount);
end;

function Transaction(const AID, AOperations: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData(AID)),
    NyxField('operations', TNyxDataValue.ParseJSON(AOperations))]);
end;

{ A rejected semantic request must preserve the complete accepted/draft pair,
  not merely its visible title. The same assertions execute under both VMs. }
procedure Refuses(const ATool: TNyxText; const AArguments: TNyxDataValue;
  const AReason: TNyxText);
begin
  LBefore := EncodeNyxProject(LSession.PreviewPair(LRevision, 'home'));
  LRejected := False;
  try
    LSession.Call(ATool, 'Scooty', AArguments);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSession.Revision = LRevision) and
    (EncodeNyxProject(LSession.PreviewPair(LRevision, 'home')) = LBefore), AReason);
end;

{ Selectors and dependent values are one atomic property configuration, regardless
  of wire member order. Real HTTP composition separately qualifies this path. }
procedure ContextualProperties;
var
  LAccepted: TNyxText;
begin
  LAccepted := EncodeNyxProject(LSession.PreviewPair(LRevision, 'home'));
  LSession.Call('nyx_transaction', 'Scooty', Transaction('context-create',
    '[{"op":"create","kind":"input","id":"number-first","parent":"agent-column",' +
    '"properties":{"value":0.1250,"input-type":"number"}},' +
    '{"op":"create","kind":"input","id":"selector-first","parent":"agent-column",' +
    '"properties":{"input-type":"number","value":0.1250}},' +
    '{"op":"create","kind":"label","id":"projected-number","parent":"agent-column",' +
    '"properties":{"value":12,"input-type":"number","projection-kind":"input"}}]'));
  Inc(LRevision);
  LValue := LSession.Call('nyx_node', 'Scooty', NyxObject([
    NyxField('id', NyxData('number-first')), NyxField('keys', NyxArray([NyxData('value')]))]));
  Check(LValue.Field('properties').Item(0).Field('value').ToJSON = '0.1250',
    'Value-before-selector numeric creation retains exact decimal spelling');
  LValue := LSession.Call('nyx_node', 'Scooty', NyxObject([
    NyxField('id', NyxData('selector-first')), NyxField('keys', NyxArray([NyxData('value')]))]));
  Check(LValue.Field('properties').Item(0).Field('value').ToJSON = '0.1250',
    'Selector-before-value creation has the same numeric meaning');
  LValue := LSession.Call('nyx_node', 'Scooty', NyxObject([
    NyxField('id', NyxData('projected-number')), NyxField('keys', NyxArray([NyxData('value')]))]));
  Check(LValue.Field('properties').Item(0).Field('value').AsDecimal.Text = '12',
    'Projection and input format jointly determine the final value type');
  LSession.Call('nyx_history', 'Scooty', NyxObject([
    NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData('context-undo')), NyxField('direction', NyxData('undo'))]));
  Inc(LRevision);
  Check(EncodeNyxProject(LSession.PreviewPair(LRevision, 'home')) = LAccepted,
    'One Undo restores the exact pair before all contextual creations');
  Refuses('nyx_transaction', Transaction('context-wrong-scalar',
    '[{"op":"title","value":"must roll back"},' +
    '{"op":"create","kind":"input","id":"bad-number","parent":"agent-column",' +
    '"properties":{"value":"12","input-type":"number"}}]'),
    'Numeric strings still refuse without clearing Redo or creating partial nodes');
  Check(LSession.Call('nyx_session', 'Scooty', NyxObject([])).Field('canRedo').AsBoolean,
    'Rejected contextual configuration retains existing Redo');
  Refuses('nyx_transaction', Transaction('context-unknown',
    '[{"op":"update","id":"agent-badge","properties":{"invented":true}}]'),
    'Staging a scalar does not admit an unpublished property');
  LSession.Call('nyx_history', 'Scooty', NyxObject([
    NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData('context-redo')), NyxField('direction', NyxData('redo'))]));
  Inc(LRevision);
  LSession.Call('nyx_transaction', 'Scooty', Transaction('context-text',
    '[{"op":"update","id":"number-first","properties":{"value":"🌙 text","input-type":"text"}},' +
    '{"op":"update","id":"selector-first","properties":{"input-type":null,"value":"🌙 text"}}]'));
  Inc(LRevision);
  LValue := LSession.Call('nyx_node', 'Scooty', NyxObject([
    NyxField('id', NyxData('number-first')), NyxField('keys', NyxArray([NyxData('value')]))]));
  Check(LValue.Field('properties').Item(0).Field('value').AsText = TNyxText('🌙 text'),
    'Numeric-to-text configuration admits exact Unicode in the same operation');
  LValue := LSession.Call('nyx_node', 'Scooty', NyxObject([
    NyxField('id', NyxData('selector-first')), NyxField('keys', NyxArray([NyxData('value')]))]));
  Check(LValue.Field('properties').Item(0).Field('value').AsText = TNyxText('🌙 text'),
    'Clearing an input format restores its text value domain');
  Refuses('nyx_transaction', Transaction('context-text-number',
    '[{"op":"update","id":"number-first","properties":{"value":12,"input-type":"text"}}]'),
    'Text format still refuses a numeric JSON value');
end;

procedure ExtraCases;
var
  LQuery: TNyxDataValue;
  LStale: Integer;
begin
  LQuery := NyxObject([NyxField('id', NyxData('agent-badge')),
    NyxField('keys', NyxArray([NyxData('text')])),
    NyxField('textOffset', NyxData(1)), NyxField('textLimit', NyxData(1))]);
  LValue := LSession.Call('nyx_node', 'Scooty', LQuery);
  Check((LValue.Field('properties').Count = 1) and
    (LValue.Field('totalProperties').AsInteger = 1), 'Exact property filtering');
  LValue := LValue.Field('properties').Item(0);
  Check((LValue.Field('meaning').AsText = 'Presentation') and
    (LValue.Field('browser').AsText = 'Available') and
    (LValue.Field('native').AsText = 'Available') and
    (LValue.Field('help').AsText <> ''), 'Bounded semantic property query exposes target support');
  Check((LValue.Field('value').AsText = TNyxText('漢')) and
    (LValue.Field('totalScalars').AsInteger = 3) and LValue.Field('truncated').AsBoolean,
    'Supplementary Unicode counts as one scalar when slicing');
  Refuses('nyx_node', NyxObject([NyxField('id', NyxData('agent-badge')),
    NyxField('keys', NyxArray([NyxData('misspelled-property')]))]),
    'Unknown property query preserves the pair');
  Refuses('nyx_transaction', Transaction('root-placement',
    '[{"op":"create","kind":"column","id":"ambiguous","root":"page","parent":"home"}]'),
    'Ambiguous root/child placement refused');
  Refuses('nyx_transaction', Transaction('bad-palette',
    '[{"op":"title","value":"rollback"},{"op":"tokens","values":{"accent":"invalid"}}]'),
    'Invalid palette rolls back all grouped edits');
  Refuses('nyx_transaction', Transaction('craft-1', '[{"op":"title","value":"different retry"}]'),
    'Receipt identity cannot be reused with changed arguments');
  Refuses('nyx_session', NyxObject([NyxField('permission', NyxData('edit'))]),
    'Agent query cannot raise permissions');

  { Ordinary pending typing remains metadata of the accepted pair. Both semantic
    and operator history retain its exact text/base in the opposite immutable
    entry when traversing the preceding editor command. }
  LPair := LSession.PreviewPair(LRevision, 'home');
  LPair.Pending := True;
  LPair.Draft := LPair.Source + #10 + TNyxText('// local draft 🌙漢字');
  LPair.DraftBase := LPair.Source;
  LStale := LRevision;
  LValue := LSession.Exchange(NyxObject([NyxField('op', NyxData('commit')),
    NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('project', NyxData(EncodeNyxProject(LPair))),
    NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
  Inc(LRevision);
  Check(LValue.Field('session').Field('pendingDraft').AsBoolean, 'Exact local draft reaches shared session');
  LSession.Call('nyx_history', 'Scooty', NyxObject([
    NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData('draft-history')), NyxField('direction', NyxData('undo'))]));
  Inc(LRevision);
  Check(not LSession.Call('nyx_session', 'Scooty', NyxObject([])).Field('pendingDraft').AsBoolean,
    'Agent Undo restores the prior accepted editor state');
  LSession.Exchange(NyxObject([NyxField('op', NyxData('history')),
    NyxField('expectedRevision', NyxData(LRevision)), NyxField('direction', NyxData('redo'))]));
  Inc(LRevision);
  Check(EncodeNyxProject(LSession.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LPair),
    'Editor Redo restores the exact saved draft and base');
  LRejected := False;
  try
    LSession.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LStale)), NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSession.Revision = LRevision), 'Stale editor commit preserves revision and draft');

  { Restore the accepted frame and attach a number-domain input through the
    ordinary authored project boundary. Decimal spelling must survive the wire
    mutation and be returned as a JSON number on both Pascal implementations. }
  LAuthor := TNyxStudioSession.Create;
  try
    LPair.Pending := False;
    LPair.Draft := '';
    LPair.DraftBase := '';
    LAuthor.LoadProject(LPair);
    LAuthor.Document.Find('project-name').Configure.InputType(niNumber).Done;
    LAuthor.Document.Find('project-name').Contract.Value(NyxNumberDomain.Range(0, 1));
    LAuthor.Document.Find('project-name').Configure.Value(0.5).Done;
    LPair := LAuthor.ProjectSnapshot;
  finally
    LAuthor.Free;
  end;
  LSession.Exchange(NyxObject([NyxField('op', NyxData('commit')),
    NyxField('expectedRevision', NyxData(LRevision)), NyxField('project', NyxData(EncodeNyxProject(LPair))),
    NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
  Inc(LRevision);
  LSession.Call('nyx_transaction', 'Scooty', Transaction('exact-number',
    '[{"op":"update","id":"project-name","properties":{"value":0.1250}}]'));
  Inc(LRevision);
  LValue := LSession.Call('nyx_node', 'Scooty', NyxObject([NyxField('id', NyxData('project-name')),
    NyxField('keys', NyxArray([NyxData('value')]))]));
  Check(LValue.Field('properties').Item(0).Field('value').ToJSON = '0.1250',
    'Number domain retains exact decimal spelling');
  Check(LSession.Exchange(NyxObject([NyxField('op', NyxData('observe')),
    NyxField('after', NyxData(LRevision))])).Field('activity').Count = 24,
    'Visible activity retention is bounded');
end;

begin
  LSession := nil;
  try
    LSession := TNyxAgentSession.Create;
    LValue := LSession.Call('nyx_session', 'Scooty', NyxObject([]));
    LRevision := LValue.Field('revision').AsInteger;
    Check(LValue.Field('permission').AsText = 'edit', 'Agent editing defaults enabled');
    LArgs := Transaction('craft-1', '[{"op":"create","kind":"column","id":"agent-column","parent":"home"},' +
      '{"op":"create","kind":"badge","id":"agent-badge","parent":"agent-column","properties":{"text":"🌙漢字"}},' +
      '{"op":"title","value":"Agent workshop 🌙"}]');
    LValue := LSession.Call('nyx_transaction', 'Scooty', LArgs);
    Check(LValue.Field('revision').AsInteger = LRevision + 1, 'Grouped edits publish one revision');
    Inc(LRevision);
    LValue := LSession.Call('nyx_transaction', 'Scooty', LArgs);
    Check(LValue.Field('revision').AsInteger = LRevision, 'Retry returns exact original receipt');
    LValue := LSession.Call('nyx_node', 'Scooty', NyxObject([NyxField('id', NyxData('agent-badge'))]));
    Check(LValue.Field('node').Field('kind').AsText = 'badge', 'Specialized badge semantic query');
    LRejected := False;
    try
      LSession.Call('nyx_transaction', 'Scooty', Transaction('reject-1',
        '[{"op":"title","value":"should roll back"},{"op":"update","id":"agent-badge","properties":{"gap":"12"}}]'));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Revision = LRevision), 'Wrong scalar type rejects whole transaction');
    Check(LSession.Call('nyx_session', 'Scooty', NyxObject([])).Field('title').AsText = TNyxText('Agent workshop 🌙'),
      'Rejected transaction retains prior title');
    LSession.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('readOnly'))]));
    LRejected := False;
    try
      LSession.Call('nyx_transaction', 'Scooty', Transaction('permission-1', '[{"op":"title","value":"denied"}]'));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Read-only operator permission refuses mutation');
    LSession.Call('nyx_outline', 'Scooty', NyxObject([NyxField('parent', NyxData('agent-column')), NyxField('limit', NyxData(1))]));
    Check(True, 'Read-only context remains queryable');
    LSession.Exchange(NyxObject([NyxField('op', NyxData('configure')), NyxField('permission', NyxData('edit'))]));
    LValue := LSession.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('undo-1')),
      NyxField('direction', NyxData('undo'))]));
    Inc(LRevision);
    Check(LValue.Field('revision').AsInteger = LRevision, 'Undo revision is monotonic');
    LRejected := False;
    try
      LSession.Call('nyx_node', 'Scooty', NyxObject([NyxField('id', NyxData('agent-column'))]));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'One undo removes every grouped control');
    LValue := LSession.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('redo-1')),
      NyxField('direction', NyxData('redo'))]));
    Inc(LRevision);
    Check(LValue.Field('title').AsText = TNyxText('Agent workshop 🌙'), 'Redo restores exact Unicode pair');
    LArgs := Transaction('tokens-1', '[{"op":"tokens","values":{"accent":"#a020c0","fontSize":18}}]');
    LSession.Call('nyx_transaction', 'Scooty', LArgs);
    Inc(LRevision);
    LValue := LSession.Call('nyx_tokens', 'Scooty', NyxObject([]));
    Check(LValue.Field('tokens').Field('fontSize').AsInteger = 18, 'Typed design tokens query');
    LRejected := False;
    try
      LSession.Call('nyx_transaction', 'Scooty', Transaction('cycle-1',
      '[{"op":"move","id":"agent-column","parent":"agent-badge"}]'));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Revision = LRevision), 'Cycle refusal retains revision');
    LValue := LSession.Exchange(NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(LRevision))]));
    Check(not NyxAgentHas(LValue, 'project'), 'Unchanged observer omits paired document');
    Check(LValue.Field('activity').Count > 0, 'Operator sees agent successes and refusals');
    ContextualProperties;
    ExtraCases;
    LSession.Free;
    LSession := nil;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(LCount) + ' agent checks';
    document.body.setAttribute('data-nyx-agent-tests', 'passed');
    {$else}
    WriteLn('PASS ', LCount, ' agent checks');
    {$endif}
  except
    on LException: Exception do
    begin
      LSession.Free;
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-agent-tests', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      Halt(1);
      {$endif}
    end;
  end;
end.
