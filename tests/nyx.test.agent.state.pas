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

unit nyx.test.agent.state;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.studio.projects;

{ Independent English control/reusable fixture. It is never a user project or
  listener. The returned pair owns only portable accepted text. }
function CreateNyxAgentStateSeed: TNyxProjectPair;
{ Exercises the exact semantic agent command boundary, source/history and strict
  refusals. Returns a qualified accepted pair for actual target consumers. }
function RunNyxAgentStateJourney(out APair: TNyxProjectPair): Integer;

implementation

uses
  SysUtils, nyx.model, nyx.types, nyx.controls, nyx.codec, nyx.codegen,
  nyx.state, nyx.binding.types, nyx.data, nyx.studio.stateedits,
  nyx.studio.agents, nyx.studio.workspaces, nyx.studio.reviews;

function CreateNyxAgentStateSeed: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LMemo: INyxMemo;
  LLabel: INyxLabel;
  LInput: INyxInput;
  LDefinition: INyxColumn;
  LInstance: INyxComponent;
  LOverride: INyxControl;
  LSource: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'State and binding workshop';
    LDocument.State.SetValue(NyxTextState('reply'), 'Ready to compose.')
      .SetValue(NyxBooleanState('checked'), False)
      .SetValue(NyxIntegerState('quantity'), 2)
      .SetValue(NyxNumberState('ratio'), 0.125);
    LPage := NewNyxPage('home');
    LPage.Configure.Layout(nlColumn).Gap(12).Padding(20).Done;
    LDocument.AddPage(LPage);
    LMemo := NewNyxMemo('reply-memo');
    LMemo.Configure.Text('Write a reply').Value('Authored fallback').Done;
    LPage.Add(LMemo);
    LLabel := NewNyxLabel('reply-label');
    LLabel.Text := 'Reply preview';
    LPage.Add(LLabel);
    LPage.Add(NewNyxCheckbox('remember-checkbox'));
    LInput := NewNyxInput('ratio-input');
    LInput.Configure.Text('Ratio').InputType(niNumber).Done;
    LPage.Add(LInput);
    LPage.Add(NewNyxLabel('quantity-label'));
    LDefinition := NewNyxColumn('reply-card', ncoDescriptor);
    LDocument.AddComponent(LDefinition);
    LMemo := NewNyxMemo('definition-editor');
    LMemo.Configure.PartName(NyxPart('editor')).Value('Reusable fallback').Done;
    LMemo.Binds.Value(NyxTextState('reply')).Done;
    LDefinition.Add(LMemo);
    LInstance := NewNyxComponent('first-card');
    LInstance.Configure.Component(NyxComponent('reply-card')).Done;
    LPage.Add(LInstance);
    LOverride := LInstance.OverridePart(NyxPart('editor'), noProperties);
    LOverride.Named('first-editor');
    LInstance := NewNyxComponent('second-card');
    LInstance.Configure.Component(NyxComponent('reply-card')).Done;
    LPage.Add(LInstance);
    LPage := NewNyxPage('review');
    LDocument.AddPage(LPage);
    LLabel := NewNyxLabel('review-label');
    LLabel.Binds.Text(NyxTextState('reply')).Done;
    LPage.Add(LLabel);
    LDocument.Validate;
    LSource := TNyxCodegen.Generate(LDocument);
    { An unrelated handwritten helper must survive every generated update. ASCII
      insertion avoids native ANSI RTL interpretation of qualification values. }
    LSource := TNyxText(StringReplace(String(LSource), 'implementation' + #10,
      'implementation' + #10 + #10 +
      '{ Handwritten application helper retained by semantic edits. }' + #10 +
      'function StateWorkshopNote: TNyxText;' + #10 + 'begin' + #10 +
      '  Result := ''Keep crafting.'';' + #10 + 'end;' + #10, []));
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
  finally
    LDocument.Free;
  end;
end;

function RunNyxAgentStateJourney(out APair: TNyxProjectPair): Integer;
var
  LAgent: TNyxAgentSession;
  LSeed: TNyxProjectPair;
  LBefore: TNyxText;
  LAdded: TNyxText;
  LArgs: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LQuery: TNyxDataValue;
  LValue: TNyxDataValue;
  LSchema: TNyxDataValue;
  LChanges: array of TNyxDataValue;
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
  LRevision: Integer;
  LIndex: Integer;
  LBinding: TNyxBindingSpec;
  LQualification: TNyxText;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxState.Create('Semantic state: ' + AReason);
    end;
    Inc(Result);
  end;

  function PairText: TNyxText;
  begin
    { The private operator observation remains available with agent access
      disabled; it is used only to qualify refusal preservation in this fixture. }
    Result := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
      .Field('project').AsText;
  end;

  function Args(const AID: TNyxText; const AData: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)), NyxField('changes', AData)]);
  end;

  function Apply(const AID: TNyxText;
    const AChanges: array of TNyxStateBindingChange): TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_state', 'Scooty',
      Args(AID, NyxStateBindingPatch(AChanges).ToData), 'authority-a');
    LRevision := Result.Field('revision').AsInteger;
  end;

  procedure Refuses(const AArguments: TNyxDataValue; const AReason: TNyxText;
    const AAuthority: TNyxText = 'authority-a');
  var
    LSnapshot: TNyxText;
    LRejected: Boolean;
  begin
    LSnapshot := PairText;
    LRejected := False;
    try
      LAgent.Call('nyx_state', 'Scooty', AArguments, AAuthority);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LAgent.Revision = LRevision) and
      (PairText = LSnapshot), AReason);
  end;

  procedure History(const ADirection, AID: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)), NyxField('direction', NyxData(ADirection))]));
    LRevision := LAgent.Revision;
  end;

  function BindingRow(const AOwner, ATarget: TNyxText): TNyxDataValue;
  var
    LRows: TNyxDataValue;
    LRow: Integer;
  begin
    LRows := LAgent.Call('nyx_state', 'Scooty', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData(AOwner))])).Field('bindings');
    for LRow := 0 to LRows.Count - 1 do
    begin

      if LRows.Item(LRow).Field('target').AsText = ATarget then
      begin
        Exit(LRows.Item(LRow));
      end;
    end;
    raise ENyxState.Create('Expected binding target was not discoverable');
  end;

  { Exercise the same context wrappers used by HTTP routing without starting a
    replacement listener. Each operation must stay in its explicit project/review
    and retain the borrowed primary pair, even after another owner disconnects. }
  procedure Contexts;
  var
    LWorkspaces: TNyxStudioWorkspaces;
    LReviews: TNyxReviewWorkspaces;
    LWorkspace: TNyxWorkspaceRef;
    LReview: TNyxReviewRef;
    LRequest: TNyxDataValue;
    LReply: TNyxDataValue;
    LPrimary: TNyxText;
    LRejected: Boolean;
  begin
    LPrimary := PairText;
    LWorkspaces := TNyxStudioWorkspaces.Create(LAgent, 'state-context-fixture');
    LReviews := TNyxReviewWorkspaces.Create(LAgent);
    try
      LWorkspace := LWorkspaces.OpenProject('Independent workshop', LSeed);
      LRequest := NyxWithWorkspace(NyxObject([
        NyxField('mode', NyxData('apply')),
        NyxField('expectedRevision', NyxData(LWorkspaces.Resolve(LWorkspace).Revision)),
        NyxField('operationId', NyxData('context-edit')),
        NyxField('changes', NyxStateBindingPatch([
          NyxSetDefault(NyxStateValue(NyxTextState('reply'), 'Workspace reply.'))]).ToData)]), LWorkspace);
      LReply := LWorkspaces.Call('nyx_state', 'context-owner', 'Scooty', LRequest);
      Check(LReply.Field('workspace').AsText = LWorkspace.ID, 'mutation receipt retains workspace scope');
      LReply := LWorkspaces.Call('nyx_state', 'context-owner', 'Scooty', NyxWithWorkspace(
        NyxObject([NyxField('mode', NyxData('value')), NyxField('name', NyxData('reply'))]), LWorkspace));
      Check(LReply.Field('text').AsText = 'Workspace reply.', 'state query resolves the explicit project');
      LWorkspaces.ReleaseOwner('context-owner');
      Check(LWorkspaces.Resolve(LWorkspace).Revision > 1, 'project state survives agent disconnection');
      LReview := LReviews.CreateReview('review-owner', 'Scooty', 'Owned review',
        nrbAccepted, LRevision);
      LRequest := NyxWithReview(NyxObject([
        NyxField('mode', NyxData('apply')),
        NyxField('expectedRevision', NyxData(LReviews.Resolve('review-owner', LReview).Revision)),
        NyxField('operationId', NyxData('context-edit')),
        NyxField('changes', NyxStateBindingPatch([
          NyxSetDefault(NyxStateValue(NyxTextState('response'), 'Review reply.'))]).ToData)]), LReview);
      LReply := LReviews.Call('nyx_state', 'review-owner', 'Scooty', LRequest);
      Check(LReply.Field('review').AsText = LReview.ID, 'same retry name remains independent in a review');
      LRejected := False;
      try
        LReviews.Call('nyx_state', 'foreign-owner', 'Scooty', NyxWithReview(
          NyxObject([NyxField('mode', NyxData('defaults'))]), LReview));
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'foreign review owner refuses bounded state inspection');
      LReviews.Discard('review-owner', LReview, LReply.Field('revision').AsInteger);
      LRejected := False;
      try
        LReviews.Call('nyx_state', 'review-owner', 'Scooty', LRequest);
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'retired review refuses old state requests without fallback');
      Check((PairText = LPrimary) and (LAgent.Revision = LRevision),
        'all explicit context operations preserve the primary pair/history');
    finally
      LReviews.Free;
      LWorkspaces.Free;
    end;
  end;

begin
  Result := 0;
  LSeed := CreateNyxAgentStateSeed;
  LAgent := TNyxAgentSession.Create(LSeed);
  try
    LRevision := LAgent.Revision;
    LQualification := TNyxText('A🌙') + #0 + TNyxText('éZ');
    LSchema := NyxStateAgentSchema;
    Check((LSchema.Field('oneOf').Count = 4) and
      (LSchema.Field('oneOf').Item(3).Field('properties').Field('changes')
        .Field('maxItems').AsInteger = 32), 'published query and mutation budgets');
    LBefore := PairText;
    LArgs := Args('create-bind', NyxStateBindingPatch([
      NyxCreateDefault(NyxStateValue(NyxTextState('qualification-text'), LQualification)),
      NyxCreateDefault(NyxStateValue(NyxTextState('long-context'),
        TNyxText(StringOfChar('a', 90)) + TNyxText('🌙'))),
      NyxCreateDefault(NyxStateValue(NyxBooleanState('enabled'), True)),
      NyxCreateDefault(NyxStateValue(NyxIntegerState('width'), 320)),
      NyxCreateDefault(NyxStateValue(NyxNumberState('threshold'), 0.123456789012345)),
      NyxBindControl(NyxBindingOwner('reply-memo'), bpValue, NyxTextState('reply'), bdTwoWay),
      NyxBindControl(NyxBindingOwner('reply-label'), bpText, NyxTextState('reply'), bdFromState),
      NyxBindControl(NyxBindingOwner('remember-checkbox'), bpValue,
        NyxBooleanState('checked'), bdTwoWay),
      NyxBindControl(NyxBindingOwner('ratio-input'), bpValue, NyxNumberState('ratio'), bdTwoWay),
      NyxBindControl(NyxBindingOwner('quantity-label'), bpText,
        NyxIntegerState('quantity'), bdFromState),
      NyxBindControl(NyxBindingOwner('home'), bpEnabled, NyxBooleanState('enabled'), bdFromState)
    ]).ToData);
    LReceipt := LAgent.Call('nyx_state', 'Scooty', LArgs, 'authority-a');
    LRevision := LAgent.Revision;
    Check((LReceipt.Field('stateBindings').Field('changes').AsInteger = 11) and
      (LReceipt.Field('selection').AsText = 'home'), 'one group preserves operator selection');
    LAdded := PairText;
    Check(LAgent.Call('nyx_state', 'Renamed display actor', LArgs, 'authority-a').ToJSON =
      LReceipt.ToJSON, 'exact authority retry returns the original receipt');
    Refuses(LArgs, 'foreign authority cannot reuse a stale receipt', 'authority-b');
    Refuses(Args('create-bind', NyxStateBindingPatch([
      NyxSetDefault(NyxStateValue(NyxIntegerState('quantity'), 10))]).ToData),
      'successful operation identity refuses changed arguments');
    History('undo', 'undo-create');
    Check(PairText = LBefore, 'one Undo restores the exact source/design baseline');
    History('redo', 'redo-create');
    Check(PairText = LAdded, 'one Redo restores all typed defaults and bindings');
    LQuery := NyxObject([NyxField('mode', NyxData('value')),
      NyxField('name', NyxData('qualification-text')), NyxField('offset', NyxData(1)),
      NyxField('count', NyxData(2))]);
    LValue := LAgent.Call('nyx_state', 'Scooty', LQuery);
    Check((LValue.Field('text').AsText = TNyxText('🌙') + #0) and
      (LValue.Field('characters').AsInteger = 5) and
      (LValue.Field('nextOffset').AsInteger = 3), 'supplementary/NUL scalar windows are exact');
    LValue := LAgent.Call('nyx_state', 'Scooty', NyxObject([
      NyxField('mode', NyxData('defaults')), NyxField('filter', NyxData('qual')),
      NyxField('limit', NyxData(1))]));
    Check((LValue.Field('total').AsInteger = 1) and
      (LValue.Field('defaults').Item(0).Field('preview').AsText = LQualification),
      'bounded filtered defaults keep exact text');
    LValue := LAgent.Call('nyx_state', 'Scooty', NyxObject([
      NyxField('mode', NyxData('defaults')), NyxField('limit', NyxData(2))]));
    Check((LValue.Field('defaults').Count = 2) and
      (LValue.Field('nextOffset').AsInteger = 2) and (LValue.Field('total').AsInteger = 9),
      'default pagination returns only the requested rows');
    LQuery := LAgent.Call('nyx_state', 'Scooty', NyxObject([
      NyxField('mode', NyxData('defaults')), NyxField('offset', NyxData(2)),
      NyxField('limit', NyxData(2))]));
    Check((LQuery.Field('defaults').Count = 2) and
      (LQuery.Field('defaults').Item(0).Field('name').AsText <>
        LValue.Field('defaults').Item(0).Field('name').AsText), 'default windows advance');
    LValue := LAgent.Call('nyx_state', 'Scooty', NyxObject([
      NyxField('mode', NyxData('defaults')), NyxField('filter', NyxData('long-context'))]));
    LValue := LValue.Field('defaults').Item(0);
    Check(LValue.Field('truncated').AsBoolean and
      (LValue.Field('characters').AsInteger = 91) and
      (LValue.Field('preview').AsText = TNyxText(StringOfChar('a', 80))),
      'long text default discovery stays bounded without splitting scalars');
    LValue := LAgent.Call('nyx_state', 'Scooty', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('reply-memo')),
      NyxField('limit', NyxData(1))]));
    Check((LValue.Field('bindings').Count = 1) and
      (LValue.Field('nextOffset').AsInteger = 1) and (LValue.Field('total').AsInteger > 1),
      'binding context pages supported targets');
    LValue := BindingRow('reply-memo', 'value');
    Check((LValue.Field('local').Field('direction').AsText = 'two-way') and
      not LValue.Field('inherited').AsBoolean, 'authored and effective binding discovery');
    LBefore := PairText;
    LValue := BindingRow('first-editor', 'value');
    Check(LValue.Field('inherited').AsBoolean and
      (LValue.Field('effective').Field('name').AsText = 'reply'),
      'explicit reusable override discovers inherited binding without selection edits');
    Check(PairText = LBefore, 'queries leave the accepted pair unchanged');
    Apply('clear-instance', [NyxBindControl(NyxBindingOwner('first-editor'),
      TNyxBindingSpec.Clear(bpValue))]);
    LValue := BindingRow('first-editor', 'value');
    Check(LValue.Field('local').Field('cleared').AsBoolean and
      (LValue.Field('effective').Kind = ndNull), 'clear masks the inherited binding');
    Apply('inherit-instance', [NyxInheritBinding(NyxBindingOwner('first-editor'), bpValue)]);
    Check(BindingRow('first-editor', 'value').Field('inherited').AsBoolean,
      'inherit restores the definition binding');
    Apply('rename-set', [
      NyxRenameDefault(NyxStudioState(NyxTextState('reply')),
        NyxStudioState(NyxTextState('response'))),
      NyxSetDefault(NyxStateValue(NyxTextState('response'), 'A thoughtful reply.')),
      NyxSetDefault(NyxStateValue(NyxIntegerState('quantity'), 7)),
      NyxSetDefault(NyxStateValue(NyxBooleanState('checked'), True)),
      NyxSetDefault(NyxStateValue(NyxNumberState('ratio'), 0.375))]);
    LPair := LAgent.PreviewPair(LRevision, 'home');
    LDocument := TNyxCodec.Decode(LPair.Design);
    try
      Check(not LDocument.State.Has('reply') and
        (LDocument.State.GetValue(NyxTextState('response')) = 'A thoughtful reply.'),
        'typed rename/set retains the exact scalar family');
      Check(LDocument.Find('review-label').FindBinding(bpText, LBinding) and
        (LBinding.StateName = 'response'), 'rename migrates references on another page');
      Check(LDocument.Find('definition-editor').FindBinding(bpValue, LBinding) and
        (LBinding.StateName = 'response'), 'rename migrates reusable definition references');
    finally
      LDocument.Free;
    end;
    Check(Pos('function StateWorkshopNote', LPair.Source) > 0,
      'handwritten helper survives generated source updates');
    Refuses(Args('bad-family', NyxStateBindingPatch([
      NyxSetDefault(NyxStateValue(NyxIntegerState('response'), 1))]).ToData),
      'set refuses family retargeting');
    Refuses(Args('missing-owner', NyxStateBindingPatch([
      NyxSetDefault(NyxStateValue(NyxIntegerState('quantity'), 9)),
      NyxBindControl(NyxBindingOwner('absent'),
        TNyxBindingSpec.Bound(bpText, 'response', nskText, bdFromState))]).ToData),
      'later invalid owner rolls back the earlier default and source');
    Refuses(Args('used-remove', NyxStateBindingPatch([
      NyxRemoveDefault(NyxStudioState(NyxTextState('response')))]).ToData),
      'used default removal refuses without altering history');
    Refuses(Args('wrong-domain', NyxStateBindingPatch([
      NyxBindControl(NyxBindingOwner('remember-checkbox'),
        TNyxBindingSpec.Bound(bpValue, 'response', nskText, bdTwoWay))]).ToData),
      'control value domain refuses a text/Boolean mismatch');
    Refuses(Args('wrong-primitive', TNyxDataValue.ParseJSON(
      '[{"op":"set","name":"quantity","kind":"integer","value":"9"}]')),
      'numeric strings are not coerced');
    Refuses(Args('fraction-integer', TNyxDataValue.ParseJSON(
      '[{"op":"set","name":"quantity","kind":"integer","value":9.5}]')),
      'integer defaults refuse fractions');
    Refuses(Args('unknown-field', TNyxDataValue.ParseJSON(
      '[{"op":"remove","name":"threshold","kind":"number","typo":true}]')),
      'unknown operation fields refuse');
    Refuses(Args('unknown-op', TNyxDataValue.ParseJSON('[{"op":"guess"}]')),
      'unknown operation refuses');
    Refuses(Args('unknown-kind', TNyxDataValue.ParseJSON(
      '[{"op":"set","name":"quantity","kind":"guess","value":9}]')),
      'unknown scalar family refuses');
    Refuses(Args('unknown-target', TNyxDataValue.ParseJSON(
      '[{"op":"inherit-binding","owner":"reply-memo","target":"guess"}]')),
      'unknown binding target refuses');
    Refuses(Args('unknown-flow', TNyxDataValue.ParseJSON(
      '[{"op":"bind","owner":"reply-memo","target":"value","name":"response","kind":"text","direction":"guess"}]')),
      'unknown binding direction refuses');
    Refuses(Args('missing-default', NyxStateBindingPatch([
      NyxSetDefault(NyxStateValue(NyxIntegerState('absent'), 1))]).ToData),
      'set cannot silently create an absent default');
    Refuses(NyxObject([NyxField('mode', NyxData('value')),
      NyxField('name', NyxData('quantity')), NyxField('count', NyxData(1))]),
      'nontext defaults do not accept text windows');
    Refuses(NyxObject([NyxField('mode', NyxData('value')),
      NyxField('name', NyxData('qualification-text')), NyxField('offset', NyxData(6))]),
      'text windows beyond the accepted value refuse');
    Refuses(NyxObject([NyxField('mode', NyxData('value')),
      NyxField('name', NyxData('qualification-text')), NyxField('count', NyxData(4097))]),
      'text query budget refuses oversized responses');
    Refuses(Args('empty', NyxArray([])), 'empty mutation groups refuse');
    SetLength(LChanges, 33);
    for LIndex := 0 to High(LChanges) do
    begin
      LChanges[LIndex] := NyxStateBindingPatch([NyxSetDefault(
        NyxStateValue(NyxIntegerState('quantity'), LIndex))]).ToData.Item(0);
    end;
    Refuses(Args('too-many', NyxArray(LChanges)), 'group budget refuses 33 changes');
    LAgent.InheritPermission(apReadOnly);
    Refuses(Args('readonly', NyxStateBindingPatch([
      NyxSetDefault(NyxStateValue(NyxIntegerState('quantity'), 8))]).ToData),
      'read-only permission refuses mutations');
    Check(BindingRow('reply-memo', 'value').Field('effective').Kind = ndObject,
      'read-only permission retains bounded inspection');
    LAgent.InheritPermission(apDisabled);
    Refuses(NyxObject([NyxField('mode', NyxData('defaults'))]),
      'disabled permission also refuses queries');
    LAgent.InheritPermission(apEdit);
    { Pending draft uses the ordinary private editor commit; accepted source and
      its draft baseline remain intact after refused semantic mutation. }
    LPair := LAgent.PreviewPair(LRevision, 'home');
    LPair.Pending := True;
    LPair.DraftBase := LPair.Source;
    LPair.Draft := LPair.Source + #10 + '{ Pending handwritten work }';
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('reply-memo')), NyxField('view', NyxData('home'))]));
    LRevision := LAgent.Revision;
    Refuses(Args('draft', NyxStateBindingPatch([
      NyxSetDefault(NyxStateValue(NyxIntegerState('quantity'), 8))]).ToData),
      'pending draft refuses semantic regeneration');
    LPair.Pending := False;
    LPair.Draft := '';
    LPair.DraftBase := '';
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('reply-memo')), NyxField('view', NyxData('home'))]));
    LRevision := LAgent.Revision;
    LBefore := PairText;
    Apply('remove-unused', [NyxRemoveDefault(NyxStudioState(NyxNumberState('threshold')))]);
    Check(LAgent.Call('nyx_session', 'Scooty', NyxObject([])).Field('selection').AsText = 'reply-memo',
      'binding/default mutation retains exact operator selection');
    History('undo', 'undo-unused');
    Check(PairText = LBefore, 'removal Undo restores the exact scalar and pair');
    Apply('clear-remove', [
      NyxBindControl(NyxBindingOwner('ratio-input'), TNyxBindingSpec.Clear(bpValue)),
      NyxRemoveDefault(NyxStudioState(NyxNumberState('ratio')))]);
    Check(not LAgent.Call('nyx_session', 'Scooty', NyxObject([])).Field('canRedo').AsBoolean,
      'accepted new group clears the preceding Redo');
    History('undo', 'undo-clear-remove');
    Check(PairText = LBefore, 'clear dependent binding and remove is one paired Undo');
    APair := LAgent.PreviewPair(LRevision, 'home');
    LValue := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]));
    Check(LValue.Field('activity').Count > 0, 'operator activity contains successes and refusals');
    Contexts;
  finally
    LAgent.Free;
  end;
end;

end.
