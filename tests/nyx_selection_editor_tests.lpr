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

program nyx_selection_editor_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.selection, nyx.selection.editor, nyx.contract, nyx.codec, nyx.codegen,
  nyx.studio.projects, nyx.studio.session, nyx.studio.edits,
  nyx.studio.inspector, nyx.studio.agents
  {$ifndef PAS2JS}, nyx.studio.mcp{$else}, Web{$endif};

var
  GDoc: TNyxDocument;
  GForm: TNyxDocument;
  GSession: TNyxStudioSession;
  GAgent: TNyxAgentSession;
  GSteps: INyxStepper;
  GEditor: INyxCard;
  GChange: TNyxSelectionEditorChange;
  GEdit: TNyxStudioDesignEdit;
  GDraft: TNyxSelectionEditorDraft;
  GDraftCopy: TNyxSelectionEditorDraft;
  GPatch: INyxDesignPatch;
  GReply: TNyxDataValue;
  GRevision: Integer;
  GBefore: TNyxText;
  GAfter: TNyxText;
  GRefused: Boolean;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure AgentHistory(const ADirection: TNyxText);
begin
  GAgent.Call('nyx_history', 'Appearance reviewer', NyxObject([
    NyxField('direction', NyxData(ADirection)), NyxField('expectedRevision', NyxData(GAgent.Revision)),
    NyxField('operationId', NyxData('selection-' + ADirection))]));
end;

begin
  try
    GDoc := TNyxDocument.Create;
    GSteps := NewNyxStepper('steps');
    GDoc.AddPage(GSteps);
    GBefore := TNyxCodec.Encode(GDoc);
    GForm := TNyxDocument.Create;
    GEditor := NewNyxSelectionEditor('appearance', NyxControl('steps'), GDoc);
    GForm.AddPage(GEditor);
    Check(TNyxCodec.Encode(GDoc) = GBefore, 'public form construction preserves authored document');
    Check(Supports(GEditor.Part(NyxPart('selected')), INyxSelect), 'choice retains its specialized interface');
    Check(GEditor.Part(NyxPart('inherit')).Node.Prop('enabled') = 'false',
      'missing local declaration disables only inheritance removal');
    GEditor.Part(NyxPart('styling')).Configure.Value('Keep each part''s authored variant').Done;
    GEditor.Part(NyxPart('selected')).Configure.Value('Secondary').Done;
    GEditor.Part(NyxPart('unselected')).Configure.Value('Ghost').Done;
    Check(CaptureNyxSelectionEditor(GEditor.Part(NyxPart('apply')).Node, GForm.Pages[0], GChange),
      'public form captures one copied proposal');
    Check((GChange.Options.StylingMode = nssAuthored) and
      (GChange.Options.SelectedVariant = nvSecondary) and (GChange.Options.UnselectedVariant = nvGhost),
      'display captions resolve to their closed Pascal enum families');
    Check(GChange.ToData.ToJSON = TNyxSelectionEditorChange.FromData(GChange.ToData).ToData.ToJSON,
      'exact worker proposal round-trips');
    GDraft.Capture('appearance', GForm.Pages[0]);
    GDraftCopy := GDraft;
    GDraft.Clear;
    GEditor.Part(NyxPart('selected')).Configure.Value('Primary').Done;
    Check(GDraftCopy.Restore(GForm.Pages[0]) and
      (GEditor.Part(NyxPart('selected')).Node.Prop('value') = 'Secondary'),
      'copied scalar drafts stay independent when another project clears its copy');
    GSession := TNyxStudioSession.Create(NyxProjectPair(GBefore, TNyxCodegen.Generate(GDoc)));
    GSession.Select('steps');
    Check(CaptureNyxSelectionInspector(GSession, GEditor.Part(NyxPart('apply')).Node,
      GForm.Pages[0], GEdit) and (GEdit.Action = sdaTransaction),
      'ordinary inspector captures a typed paired transaction');
    GBefore := EncodeNyxProject(GSession.ProjectSnapshot);
    GSession.ApplyPatch(NyxSelectionPolicyPatch(GChange));
    GAfter := EncodeNyxProject(GSession.ProjectSnapshot);
    Check(Pos('.Styling(nssAuthored)', GSession.Source) > 0, 'publication emits fluent crafted source');
    GSession.Undo;
    Check(EncodeNyxProject(GSession.ProjectSnapshot) = GBefore, 'one Undo restores the exact pair');
    GSession.Redo;
    Check(EncodeNyxProject(GSession.ProjectSnapshot) = GAfter, 'one Redo restores the exact policy pair');
    GRefused := False;
    try
      GSession.ApplyPatch(NyxSelectionPolicyPatch(GChange));
    except
      on ENyxModel do
      begin
        GRefused := True;
      end;
    end;
    Check(GRefused and (EncodeNyxProject(GSession.ProjectSnapshot) = GAfter),
      'stale mounted baseline refuses without a partial publication');
    GSession.SetSourceDraft(GSession.Source + #10 + '// Unfinished appearance work');
    GBefore := EncodeNyxProject(GSession.ProjectSnapshot);
    GRefused := False;
    try
      GSession.ApplyPatch(NyxSetSelectionPolicy(NyxControl('steps'), TNyxSelectionOptions.Defaults));
    except
      on Exception do
      begin
        GRefused := True;
      end;
    end;
    Check(GRefused and GSession.SourceDraftPending and
      (EncodeNyxProject(GSession.ProjectSnapshot) = GBefore), 'pending Pascal retains its ordinary mutation guard');
    GSession.DiscardSourceDraft;
    GAgent := TNyxAgentSession.Create(GSession.ProjectSnapshot);
    GReply := GAgent.Call('nyx_node', 'Appearance reviewer', NyxObject([
      NyxField('id', NyxData('steps')), NyxField('selectionStyle', NyxData(True)),
      NyxField('selectionOffset', NyxData(1)), NyxField('selectionLimit', NyxData(1))]));
    Check((GReply.Field('selectionStyle').Field('parts').Count = 1) and
      (GReply.Field('selectionStyle').Field('total').AsInteger = 4),
      'semantic context returns only one requested part identity');
    Check(GReply.Field('selectionStyle').Field('effective').Field('styling').AsText = 'authored',
      'bounded context distinguishes effective policy from the local declaration');
    GRevision := GAgent.Revision;
    GPatch := NyxSetSelectionPolicy(NyxControl('steps'),
      TNyxSelectionOptions.Defaults.Selected(nvDefault).Unselected(nvPrimary));
    GAgent.Call('nyx_transaction', 'Appearance reviewer', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('selection-policy')),
      NyxField('operations', NyxDesignChanges(GPatch).ToData)]));
    Check(GAgent.Revision > GRevision, 'compact semantic mutation publishes one revision');
    AgentHistory('undo');
    Check(EncodeNyxProject(GAgent.PreviewPair(GAgent.Revision, 'steps')) = GAfter,
      'semantic Undo restores the exact accepted design/source pair');
    AgentHistory('redo');
    GRefused := False;
    try
      GAgent.Call('nyx_transaction', 'Appearance reviewer', NyxObject([
        NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('stale-selection')),
        NyxField('operations', NyxDesignChanges(GPatch).ToData)]));
    except
      on ENyxModel do
      begin
        GRefused := True;
      end;
    end;
    Check(GRefused, 'stale semantic revision refuses before mutation');
    GPatch := NyxInheritSelectionPolicy(NyxControl('steps'));
    GAgent.Call('nyx_transaction', 'Appearance reviewer', NyxObject([
      NyxField('expectedRevision', NyxData(GAgent.Revision)),
      NyxField('operationId', NyxData('selection-inherit')),
      NyxField('operations', NyxDesignChanges(GPatch).ToData)]));
    GReply := GAgent.Call('nyx_node', 'Appearance reviewer', NyxObject([
      NyxField('id', NyxData('steps')), NyxField('selectionStyle', NyxData(True)),
      NyxField('selectionOffset', NyxData(10)), NyxField('selectionLimit', NyxData(1))]));
    Check((GReply.Field('selectionStyle').Field('local').Kind = ndNull) and
      (GReply.Field('selectionStyle').Field('parts').Count = 0) and
      (GReply.Field('selectionStyle').Field('total').AsInteger = 4),
      'inheritance removal and beyond-end paging remain explicit');
    GRevision := GAgent.Revision;
    GBefore := EncodeNyxProject(GAgent.PreviewPair(GRevision, 'steps'));
    GRefused := False;
    try
      GAgent.Call('nyx_transaction', 'Appearance reviewer', NyxObject([
        NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('invalid-selection-group')),
        NyxField('operations', NyxArray([
          NyxObject([NyxField('op', NyxData('selection-set')), NyxField('id', NyxData('steps')),
            NyxField('options', TNyxSelectionOptions.Defaults.ToData)]),
          NyxObject([NyxField('op', NyxData('selection-set')), NyxField('id', NyxData('absent')),
            NyxField('options', TNyxSelectionOptions.Defaults.ToData)])]))]));
    except
      on ENyxModel do
      begin
        GRefused := True;
      end;
    end;
    Check(GRefused and (GAgent.Revision = GRevision) and
      (EncodeNyxProject(GAgent.PreviewPair(GRevision, 'steps')) = GBefore),
      'failed later operation rolls back the complete appearance group');
    GRefused := False;
    try
      GAgent.Call('nyx_node', 'Appearance reviewer', NyxObject([
        NyxField('id', NyxData('steps')), NyxField('selectionStyle', NyxData(True)),
        NyxField('selectionLimit', NyxData(51))]));
    except
      on ENyxModel do
      begin
        GRefused := True;
      end;
    end;
    Check(GRefused, 'bounded semantic policy page rejects an excessive limit');
    GAgent.InheritPermission(apReadOnly);
    GRefused := False;
    try
      GAgent.Call('nyx_transaction', 'Appearance reviewer', NyxObject([
        NyxField('expectedRevision', NyxData(GAgent.Revision)),
        NyxField('operationId', NyxData('readonly-selection')),
        NyxField('operations', NyxDesignChanges(GPatch).ToData)]));
    except
      on ENyxModel do
      begin
        GRefused := True;
      end;
    end;
    Check(GRefused, 'read-only semantic permission refuses appearance authoring');
    {$ifndef PAS2JS}
    Check((Pos('selection-set', NyxStudioMCPTools.ToJSON) > 0) and
      (Pos('selectionStyle', NyxStudioMCPTools.ToJSON) > 0),
      'discovery publishes both compact operations and bounded query fields');
    WriteLn('PASS ', GChecks, ' typed selection editor/semantic checks');
    {$else}
    document.body.setAttribute('data-selection-editor-checks', IntToStr(GChecks));
    document.body.setAttribute('data-selection-editor', 'passed');
    {$endif}
  except
    on GError: Exception do
    begin
      {$ifndef PAS2JS}
      WriteLn('FAIL ', GError.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$else}
      document.body.setAttribute('data-selection-editor', 'failed');
      document.body.setAttribute('data-selection-editor-error', GError.Message);
      {$endif}
    end;
  end;
  GPatch := nil;
  GAgent.Free;
  GSession.Free;
  GEditor := nil;
  GForm.Free;
  GSteps := nil;
  GDoc.Free;
end.
