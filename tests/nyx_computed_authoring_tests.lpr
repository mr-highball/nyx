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

program nyx_computed_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, {$ifndef PAS2JS}Classes,{$else}Web,{$endif}
  nyx.text, nyx.data, nyx.types, nyx.state, nyx.expressions,
  nyx.expressions.editing, nyx.expressions.editor, nyx.controls,
  nyx.model, nyx.codec, nyx.codegen, nyx.source, nyx.composition, nyx.binding.types,
  nyx.studio.projects, nyx.studio.session, nyx.studio.agents,
  nyx.studio.stateedits, nyx.studio.transactions, nyx.studio.inspector,
  nyx.studio.authoring, nyx.studio.view, nyx.generated.view;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Computed authoring: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Verify;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LEditor: INyxExpressionEditor;
  LOtherEditor: INyxExpressionEditor;
  LDraft: TNyxExpressionEditorDraft;
  LProposal: TNyxExpressionProposal;
  LExpression: INyxExpression;
  LCaption: INyxTextExpression;
  LTotal: INyxNumberExpression;
  LCondition: INyxBooleanExpression;
  LInputs: INyxExpressionInputs;
  LPath: TNyxExpressionPath;
  LChangedPath: TNyxExpressionPath;
  LPair: TNyxProjectPair;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LSource: TNyxText;
  LCode: TNyxText;
  LResult: TNyxDataValue;
  LPatch: INyxStateBindingPatch;
  LEdit: TNyxStudioDesignEdit;
  LPreview: Boolean;
  LRefused: Boolean;
  LIndex: Integer;
  LKind: TNyxStateKind;
  LRoot: TNyxNode;
  LProjection: TNyxNode;
  LForeign: TNyxNode;
  LViewState: TNyxStudioViewState;
  LShell: TNyxDocument;
  {$ifndef PAS2JS}
  LStream: TFileStream;
  LDirectory: TNyxText;
  {$endif}

  function PairText: TNyxText;
  begin
    Result := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
      .Field('project').AsText;
  end;

  function Inspect(const AMode: TNyxText; const AOwner: TNyxText;
    ATarget: TNyxBindingProperty): TNyxDataValue;
  begin

    if AMode = 'bindings' then
    begin
      Exit(LAgent.Call('nyx_state', 'Expression qualification', NyxObject([
        NyxField('mode', NyxData(AMode)), NyxField('owner', NyxData(AOwner))])));
    end;
    Result := LAgent.Call('nyx_state', 'Expression qualification', NyxObject([
      NyxField('mode', NyxData(AMode)), NyxField('owner', NyxData(AOwner)),
      NyxField('target', NyxData(NyxBindingPropertyName(ATarget))),
      NyxField('expectedRevision', NyxData(LAgent.Revision))]));
  end;

  procedure Apply(const AChanges: INyxStateBindingPatch; const AID: TNyxText);
  begin
    LAgent.Call('nyx_state', 'Expression qualification', NyxObject([
      NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData(AID)), NyxField('changes', AChanges.ToData)]));
  end;

  procedure History(const ADirection: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Expression qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('expression-history-' + ADirection + '-' +
        TNyxText(IntToStr(LAgent.Revision)))),
      NyxField('direction', NyxData(ADirection))]));
  end;

begin
  LDocument := BuildNyxDocument;
  LSession := nil;
  LAgent := nil;
  LRoot := nil;
  LProjection := nil;
  LForeign := nil;
  LShell := nil;
  LDraft := Default(TNyxExpressionEditorDraft);
  try
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LInputs := CaptureNyxExpressionInputs(LDocument.State);
    LTotal := NyxNumberExpression(NyxNumberState('price'))
      .Times(NyxIntegerExpression(NyxIntegerState('quantity')).AsNumber)
      .Plus(NyxNumberExpression(NyxNumberState('tax')));
    LCaption := NyxTextExpression('Total: ').Append(LTotal.AsText);
    LCondition := NyxBooleanExpression(NyxBooleanState('ready'))
      .AndAlso(NyxIntegerExpression(NyxIntegerState('quantity'))
        .GreaterThan(NyxIntegerExpression(0)));
    Check(LCaption.Evaluate(LInputs) = 'Total: 40', 'semantic seed supplies real calculated defaults');
    for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
    begin
      case LKind of
        nskText:
          begin
            LExpression := NyxTextExpression('A Pascal caption 🌙').Append(
              NyxIntegerExpression(NyxIntegerState('quantity')).AsText);
          end;
        nskBoolean:
          begin
            LExpression := LCondition;
          end;
        nskInteger:
          begin
            LExpression := NyxChoose(LCondition,
              NyxIntegerExpression(12).DividedBy(NyxIntegerExpression(3)),
              NyxTextExpression('🌙').Length);
          end;
        nskNumber:
          begin
            LExpression := LTotal;
          end;
      end;
      LCode := TNyxCodegen.Expression(LExpression);
      Check(ReadNyxExpressionSource(LCode).ToData.ToJSON = LExpression.ToData.ToJSON,
        'standalone fluent source preserves each typed family');
    end;
    for LIndex := 0 to 3 do
    begin
      case LIndex of
        0:
          begin
            LCode := 'NyxTextExpression(''safe''); DeleteFile(''anything'')';
          end;
        1:
          begin
            LCode := 'DeleteFile(''anything'')';
          end;
        2:
          begin
            LCode := 'NyxIntegerExpression(3).Plus(NyxTextExpression(''wrong''))';
          end;
        3:
          begin
            LCode := '''An untyped literal''';
          end;
      end;
      LRefused := False;
      try
        ReadNyxExpressionSource(LCode);
      except
        on Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'standalone reader refuses foreign/wrong/trailing source');
    end;

    LPath := NyxExpressionPath([1]);
    LChangedPath := LPath.Child(0);
    Check((LPath.Count = 1) and (LChangedPath.Count = 2),
      'child paths retain independent array ownership');
    Check(NyxExpressionAt(LCaption, LPath).Kind = nskText,
      'bounded child navigation reads the exact operand');
    Check(TNyxExpressionPath.FromData(LChangedPath.ToData).ToData.ToJSON =
      LChangedPath.ToData.ToJSON, 'owned path wire round trip');

    LAgent := TNyxAgentSession.Create(LPair);
    LAgent.InheritPermission(apEdit);
    LBefore := PairText;
    LPatch := NyxStateBindingPatch([
      NyxBindControl(NyxBindingOwner('order-total'), bpText, LCaption),
      NyxBindControl(NyxBindingOwner('review-total'), bpText, LCaption),
      NyxBindControl(NyxBindingOwner('order-submit'), bpEnabled, LCondition)]);
    Check(ReadNyxStateBindingPatch(LPatch.ToData).ToData.ToJSON = LPatch.ToData.ToJSON,
      'computed grouped patch remains exact across the explicit wire boundary');
    Apply(LPatch, 'compose-calculator');
    LAfter := PairText;
    Check(LAfter <> LBefore, 'one semantic group changes the accepted pair');
    LPair := DecodeNyxProject(LAfter);
    Check(Pos('nyx.expressions', LPair.Source) > 0,
      'first computed binding adds the compiler namespace to an older accepted unit');
    LResult := Inspect('bindings', 'order-total', bpText);
    Check(LResult.Field('bindings').Item(0).Field('effective').Field('source').AsText =
      'expression', 'ordinary binding discovery describes a computed source');
    LResult := Inspect('expression', 'order-total', bpText);
    Check((LResult.Field('operator').AsText = 'concat') and
      (LResult.Field('children').AsInteger = 2) and
      (LResult.Field('totalDependencies').AsInteger = 3),
      'bounded inspection returns root shape and typed dependencies');
    Check(not NyxAgentHas(LResult, 'expression'),
      'root context does not dump the complete descriptor tree');
    LResult := LAgent.Call('nyx_state', 'Expression qualification', NyxObject([
      NyxField('mode', NyxData('expression')), NyxField('owner', NyxData('order-total')),
      NyxField('target', NyxData('text')), NyxField('path', NyxArray([NyxData(0)])),
      NyxField('expectedRevision', NyxData(LAgent.Revision))]));
    Check((LResult.Field('operator').AsText = 'literal') and
      (LResult.Field('literal').Field('preview').AsText = 'Total: '),
      'child inspection returns a bounded exact literal preview');
    LSource := Inspect('expression-source', 'order-total', bpText).Field('source').AsText;
    Check(ReadNyxExpressionSource(LSource).ToData.ToJSON = LCaption.ToData.ToJSON,
      'bounded semantic Pascal reconstructs the same tree');
    Check(PairText = LAfter, 'queries preserve accepted source/design/history');
    History('undo');
    Check(PairText = LBefore, 'one Undo restores the exact original pair');
    History('redo');
    Check(PairText = LAfter, 'one Redo restores the complete expression group');
    LRefused := False;
    try
      LAgent.Call('nyx_state', 'Expression qualification', NyxObject([
        NyxField('mode', NyxData('expression')), NyxField('owner', NyxData('order-total')),
        NyxField('target', NyxData('text')),
        NyxField('expectedRevision', NyxData(LAgent.Revision - 1))]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (PairText = LAfter), 'stale expression windows retain the accepted pair');
    LAgent.InheritPermission(apReadOnly);
    Check(Inspect('expression', 'order-total', bpText).Field('children').AsInteger = 2,
      'read-only permission retains expression inspection');
    LRefused := False;
    try
      Apply(LPatch, 'read-only-refusal');
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (PairText = LAfter), 'read-only computed mutation refuses atomically');
    LAgent.InheritPermission(apEdit);
    Apply(NyxStateBindingPatch([
      NyxRenameDefault(NyxStudioState(NyxIntegerState('quantity')),
        NyxStudioState(NyxIntegerState('units 🌙')))]), 'rename-expression-input');
    LResult := Inspect('expression-source', 'order-total', bpText);
    Check((Pos('units 🌙', LResult.Field('source').AsText) > 0) and
      (Pos('quantity', LResult.Field('source').AsText) = 0),
      'rename migrates every expression reference with exact Unicode');
    History('undo');
    Check(PairText = LAfter, 'rename Undo restores both source and defaults exactly');
    LRefused := False;
    try
      Apply(NyxStateBindingPatch([
        NyxRemoveDefault(NyxStudioState(NyxIntegerState('quantity')))]), 'remove-in-use');
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (PairText = LAfter), 'in-use computed dependencies refuse default removal');

    LSession := TNyxStudioSession.Create(DecodeNyxProject(LAfter));
    LSession.Activate('home');
    LSession.Select('order-total');
    LEditor := NewNyxExpressionEditor(NyxStudioExpressionEditorID,
      LSession.Document, NyxControl('order-total'), bpText);
    Check((LEditor.Owner.ID = 'order-total') and (LEditor.Target = bpText),
      'specialized public form exposes typed owner/property');
    LEditor.Source := TNyxCodegen.Expression(LCaption.Append(NyxTextExpression(' 🌙')));
    LCode := LEditor.Source;
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(ReadNyxExpressionEditor(LEditor.Part(NyxPart('preview')).Node,
      LEditor.Node, LSession.Document, LProposal, LPreview) and LPreview,
      'public preview captures admitted copied intent');
    ShowNyxExpressionPreview(LEditor.Node, LEditor.ID, LProposal, LSession.Document);
    Check(Pos('Default preview: Total: 40 🌙',
      LEditor.Part(NyxPart('summary')).Node.Prop('text')) > 0,
      'preview reports actual default evaluation and dependencies');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore,
      'preview creates no project/history change');

    LDraft.Capture(LEditor.ID, LEditor.Node);
    LOtherEditor := NewNyxExpressionEditor(LEditor.ID, LSession.Document,
      NyxControl('order-total'), bpText);
    Check(LDraft.Restore(LOtherEditor.Node) and (LOtherEditor.Source = LCode),
      'unfinished expression survives independent form reconstruction');
    LEditor := LOtherEditor;
    LOtherEditor := nil;
    LSession.SetSourceDraft(LSession.Source + TNyxText('// Unfinished application Pascal 🌙'));
    Check(PreviewNyxExpressionInspector(LSession,
      LEditor.Part(NyxPart('preview')).Node, LEditor.Node),
      'readonly expression preview remains usable with pending application Pascal');
    LRefused := False;
    try
      CaptureNyxExpressionInspector(LSession, LEditor.Part(NyxPart('apply')).Node,
        LEditor.Node, LEdit);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.DraftSource <> LSession.Source) and
      (LEditor.Source = LCode), 'pending application draft refuses Apply without losing either buffer');
    LSession.DiscardSourceDraft;
    LRefused := False;
    LEditor.Source := 'NyxTextExpression(''unfinished';
    try
      ReadNyxExpressionEditor(LEditor.Part(NyxPart('apply')).Node,
        LEditor.Node, LSession.Document, LProposal, LPreview);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LEditor.Source = 'NyxTextExpression(''unfinished') and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'invalid fluent source preserves its exact draft and accepted pair');
    LEditor.Source := LCode;
    LForeign := LEditor.Part(NyxPart('apply')).Node.Clone;
    LRefused := False;
    try
      ReadNyxExpressionEditor(LForeign, LEditor.Node, LSession.Document, LProposal, LPreview);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'same-named foreign action cannot capture this form');
    Check(CaptureNyxExpressionInspector(LSession, LEditor.Part(NyxPart('apply')).Node,
      LEditor.Node, LEdit), 'ordinary inspector captures one public expression proposal');
    LSession.AdoptProject(LEdit.Transaction.Materialize.Candidate(LSession.ProjectSnapshot));
    LSource := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(LSource <> LBefore, 'captured public form publishes one complete paired edit');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore,
      'ordinary public form Undo restores the exact pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LSource,
      'ordinary public form Redo restores its expression');
    LViewState := DefaultNyxStudioViewState;
    LViewState.BindingsVisible := True;
    LViewState.BindingTarget := bpText;
    LShell := BuildNyxStudioView(LSession, LViewState);
    Check(LShell.Find(NyxStudioExpressionEditorID) <> nil,
      'ordinary shared Studio shell contains the public expression component');

    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      LPair := DecodeNyxProject(LAfter);
      LDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
      ForceDirectories(LDirectory);
      LStream := TFileStream.Create(LDirectory + 'nyx.generated.view.pas', fmCreate);
      try
        LStream.WriteBuffer(LPair.Source[1], Length(LPair.Source));
      finally
        LStream.Free;
      end;
      LStream := TFileStream.Create(LDirectory + 'project.nyx', fmCreate);
      try
        LStream.WriteBuffer(LAfter[1], Length(LAfter));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LShell.Free;
    ReleaseNyxNode(LForeign);
    ReleaseNyxNode(LRoot);
    LOtherEditor := nil;
    LEditor := nil;
    LSession.Free;
    LAgent.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Verify;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-computed-authoring', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' computed authoring checks';
    {$else}
    WriteLn('PASS ', GChecks, ' computed authoring checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-computed-authoring', 'failed');
      document.body.setAttribute('data-computed-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
