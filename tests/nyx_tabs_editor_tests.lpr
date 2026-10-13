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

program nyx_tabs_editor_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.tabs,
  nyx.tabs.editor, nyx.codec, nyx.codegen, nyx.source, nyx.catalog,
  nyx.composition, nyx.state, nyx.binding.types, nyx.studio.stateedits,
  nyx.studio.projects, nyx.studio.edits, nyx.studio.session, nyx.studio.inspector,
  nyx.studio.agents, nyx.studio.transactions
  {$ifndef PAS2JS}, nyx.studio.mcp{$endif}
  {$ifdef PAS2JS}, Web{$endif};

var
  LDoc: TNyxDocument;
  LForm: TNyxDocument;
  LSession: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LTabs: INyxTabs;
  LEditor: INyxCard;
  LChange: TNyxTabsEditorChange;
  LEdit: TNyxStudioDesignEdit;
  LPatch: INyxDesignPatch;
  LCandidate: TNyxDocument;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LReply: TNyxDataValue;
  LRevision: Integer;
  LRefused: Boolean;
  LChecks: Integer;
  LDraft: TNyxTabsEditorDraft;
  LWorkspace: TNyxSourceWorkspace;
  LRuntime: TNyxNode;
  LReference: INyxControl;
  LBindingPatch: INyxStateBindingPatch;
  LBoundPair: TNyxProjectPair;
  LUnboundPair: TNyxProjectPair;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LChecks);
end;

begin
  LDoc := nil;
  LForm := nil;
  LSession := nil;
  LAgent := nil;
  LCandidate := nil;
  LWorkspace := nil;
  LRuntime := nil;
  try
    LDoc := TNyxDocument.Create;
    LTabs := NewNyxTabs('project-tabs');
    LTabs.Add(NewNyxTab('plan').Configure.Text('Plan').Done);
    LTabs.Add(NewNyxTab('details').Configure.Text('Details').Done);
    LDoc.AddPage(LTabs);
    LBefore := TNyxCodec.Encode(LDoc);
    LForm := TNyxDocument.Create;
    LEditor := NewNyxTabsEditor('policy', NyxControl('project-tabs'), LDoc);
    LForm.AddPage(LEditor);
    Check(TNyxCodec.Encode(LDoc) = LBefore, 'editor construction preserves authored source');
    Check(Supports(LEditor.Part(NyxPart('initial')), INyxSelect), 'initial choice uses a specialized interface');
    LEditor.Part(NyxPart('activation')).Configure.Value('On Enter or Space').Done;
    LEditor.Part(NyxPart('orientation')).Configure.Value('Vertical').Done;
    LEditor.Part(NyxPart('wrap')).Configure.Value(False).Done;
    Check(CaptureNyxTabsEditor(LEditor.Part(NyxPart('apply')).Node, LForm.Pages[0], LChange),
      'public compound captures independent typed intent');
    Check((LChange.Options.ActivationMode = ntaManual) and
      (LChange.Options.OrientationMode = ntoVertical) and not LChange.Options.WrapFocus,
      'form captions resolve to closed enums and Boolean');
    Check(LChange.ToData.ToJSON = TNyxTabsEditorChange.FromData(LChange.ToData).ToData.ToJSON,
      'complete worker intent round trips exactly');
    LSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDoc), TNyxCodegen.Generate(LDoc)));
    FreeAndNil(LDoc);
    LSession.Activate('project-tabs');
    LSession.Select('project-tabs');
    Check(CaptureNyxTabsInspector(LSession, LEditor.Part(NyxPart('apply')).Node,
      LForm.Pages[0], LEdit) and (LEdit.Action = sdaTransaction),
      'ordinary inspector captures one grouped paired transaction');
    LBefore := LSession.Source;
    LSession.ApplyPatch(NyxTabsPolicyPatch(LChange));
    LAfter := LSession.Source;
    Check(Pos('.Activation(ntaManual)', LAfter) > 0, 'paired publication emits crafted typed policy');
    Check(Pos('.Orientation(ntoVertical)', LAfter) > 0, 'paired publication uses closed orientation');
    LSession.Undo;
    Check(LSession.Source = LBefore, 'one Undo restores exact source');
    LSession.Redo;
    Check(LSession.Source = LAfter, 'one Redo restores exact policy source');
    LRefused := False;
    try
      LSession.ApplyPatch(NyxTabsPolicyPatch(LChange));
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.Source = LAfter), 'stale captured baseline cannot overwrite newer policy');
    LDraft.Capture('policy', LForm.Pages[0]);
    LEditor.Part(NyxPart('orientation')).Configure.Value('Horizontal').Done;
    Check(LDraft.Restore(LForm.Pages[0]) and
      (LEditor.Part(NyxPart('orientation')).Node.Prop('value') = 'Vertical'),
      'ephemeral copied draft restores without a source command');
    LAgent := TNyxAgentSession.Create(LSession.ProjectSnapshot);
    LReply := LAgent.Call('nyx_node', 'Policy reviewer', NyxObject([
      NyxField('id', NyxData('project-tabs')), NyxField('tabs', NyxData(True)),
      NyxField('tabOffset', NyxData(1)), NyxField('tabLimit', NyxData(1))]));
    Check((LReply.Field('tabs').Field('children').Count = 1) and
      (LReply.Field('tabs').Field('total').AsInteger = 2), 'semantic context pages direct children');
    Check(LReply.Field('tabs').Field('children').Item(0).Field('id').AsText = 'details',
      'bounded context returns an exact local child reference');
    LRevision := LAgent.Call('nyx_session', 'Policy reviewer', NyxObject([]))
      .Field('revision').AsInteger;
    LPatch := NyxSetTabsPolicy(NyxControl('project-tabs'),
      TNyxTabsOptions.Defaults.Initial(NyxControl('details')));
    LReply := LAgent.Call('nyx_transaction', 'Policy reviewer', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('tab-policy')),
      NyxField('operations', NyxDesignChanges(LPatch).ToData)]));
    Check(LAgent.Revision > LRevision, 'compact typed semantic policy mutation publishes one revision');
    LReply := LAgent.Call('nyx_node', 'Policy reviewer', NyxObject([
      NyxField('id', NyxData('project-tabs')), NyxField('tabs', NyxData(True))]));
    Check(LReply.Field('tabs').Field('effective').Field('initial').AsText = 'details',
      'semantic mutation changes the selected initial reference');
    LReply := LAgent.Call('nyx_history', 'Policy reviewer', NyxObject([
      NyxField('direction', NyxData('undo')), NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('tab-policy-undo'))]));
    Check(LAgent.PreviewPair(LAgent.Revision, 'project-tabs').Source = LAfter,
      'semantic policy has one exact paired Undo');
    LRefused := False;
    try
      LAgent.Call('nyx_transaction', 'Policy reviewer', NyxObject([
        NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('stale-tab-policy')),
        NyxField('operations', NyxDesignChanges(LPatch).ToData)]));
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LAgent.PreviewPair(LAgent.Revision, 'project-tabs').Source = LAfter),
      'stale semantic revision refuses without a paired change');
    { The current semantic model admits an ordinary state/binding group. This
      in-process consumer is separate from installed transport/schema rollout. }
    LUnboundPair := LAgent.PreviewPair(LAgent.Revision, 'project-tabs');
    LBindingPatch := NyxStateBindingPatch([
      NyxCreateDefault(NyxStateValue(NyxTextState('selected-page'), NyxTabValue(NyxControl('plan')))),
      NyxBindControl(NyxBindingOwner('project-tabs'), bpValue,
        NyxTextState('selected-page'), bdTwoWay)]);
    LReply := LAgent.Call('nyx_state', 'Binding reviewer', NyxObject([
      NyxField('mode', NyxData('apply')), NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('tab-selection-binding')),
      NyxField('changes', LBindingPatch.ToData)]));
    LBoundPair := LAgent.PreviewPair(LAgent.Revision, 'project-tabs');
    Check(Pos('selected-page', LBoundPair.Source) > 0,
      'semantic binding publishes typed state and generated source together');
    LReply := LAgent.Call('nyx_state', 'Binding reviewer', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('project-tabs')),
      NyxField('offset', NyxData(0)), NyxField('limit', NyxData(20))]));
    Check(Pos('selected-page', LReply.ToJSON) > 0, 'bounded binding context exposes the accepted selection key');
    LReply := LAgent.Call('nyx_history', 'Binding reviewer', NyxObject([
      NyxField('direction', NyxData('undo')), NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('tab-selection-undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'project-tabs')) =
      EncodeNyxProject(LUnboundPair), 'one paired Undo removes the complete state/binding group');
    LReply := LAgent.Call('nyx_history', 'Binding reviewer', NyxObject([
      NyxField('direction', NyxData('redo')), NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('tab-selection-redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'project-tabs')) =
      EncodeNyxProject(LBoundPair), 'one paired Redo restores exact bound source and design');
    LBindingPatch := nil;
    LAgent.InheritPermission(apReadOnly);
    LRefused := False;
    try
      LAgent.Call('nyx_transaction', 'Policy reviewer', NyxObject([
        NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('readonly-tab-policy')),
        NyxField('operations', NyxDesignChanges(LPatch).ToData)]));
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'read-only agent permission refuses tab authoring');
    {$ifndef PAS2JS}
    Check(Pos('tabs-set', NyxStudioMCPTools.ToJSON) > 0, 'discovery publishes the compact policy operation');
    {$endif}
    LAgent.Free;
    LAgent := nil;
    LSession.Free;
    LSession := nil;
    LEditor := nil;
    LTabs := nil;
    FreeAndNil(LForm);
    LDoc := TNyxDocument.Create;
    LTabs := NewNyxTabs('reusable-tabs');
    LTabs.Add(NewNyxTab('reusable-plan').Configure.Text('Plan').Done);
    LTabs.Add(NewNyxTab('reusable-details').Configure.Text('Details').Done);
    LDoc.AddComponent(LTabs);
    LDoc.AddPage(NewNyxPage('workspace'));
    LReference := NewNyxComponent('first-instance').Configure.Component(NyxComponent('reusable-tabs')).Done;
    LDoc.Pages[0].Add(LReference);
    LDoc.Pages[0].Add(NewNyxComponent('second-instance').Configure.Component(NyxComponent('reusable-tabs')).Done);
    SetNyxTabsOptions(LReference, TNyxTabsOptions.Defaults.Activation(ntaManual)
      .Initial(NyxControl('reusable-details')));
    LWorkspace := TNyxSourceWorkspace.Create;
    LCandidate := LWorkspace.Candidate(LDoc, TNyxCodegen.Generate(LDoc));
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDoc),
      'ordinary source preserves a typed policy override on a reusable reference');
    LRuntime := RealizeNyxView(LCandidate, LCandidate.Pages[0]);
    Check(NyxTabsOptions(LRuntime.Children[0]).ActivationMode = ntaManual,
      'first reusable instance receives its local override');
    Check(NyxTabsOptions(LRuntime.Children[1]).ActivationMode = ntaAutomatic,
      'local policy cannot change sibling or definition');
    FreeAndNil(LRuntime);
    LChange := Default(TNyxTabsEditorChange);
    LChange.Owner := NyxControl('first-instance');
    LChange.Baseline := NyxTabsEditorBaseline(LDoc, LChange.Owner);
    LChange.Inherit := True;
    FreeAndNil(LCandidate);
    LCandidate := NyxTabsPolicyPatch(LChange).Candidate(LDoc, nil);
    Check(not LCandidate.Find('first-instance').Extensions.Has(NyxExtension(NyxTabsOptionsKey)),
      'inherit removes only the instance policy declaration');
    FreeAndNil(LCandidate);
    FreeAndNil(LWorkspace);
    LReference := nil;
    LTabs := nil;
    FreeAndNil(LDoc);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-tabs-editor', 'passed');
    document.body.setAttribute('data-nyx-tabs-editor-checks', IntToStr(LChecks));
    {$endif}
    WriteLn('PASS ', LChecks, ' typed tab editor/source/semantic checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-tabs-editor', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}DumpExceptionBackTrace(Output);{$endif}
      ExitCode := 1;
    end;
  end;
  LAgent.Free;
  LSession.Free;
  LEditor := nil;
  LTabs := nil;
  LForm.Free;
  LDoc.Free;
  LCandidate.Free;
  LWorkspace.Free;
  LRuntime.Free;
  LReference := nil;
end.
