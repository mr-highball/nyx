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

{ Exercise the production compiler pool and ordinary durable source command.
  General source deliberately changes constructed meaning; its unchanged-buffer
  capture must retain classes/helpers and reject late user work independently.
  This in-process host never starts a listener or touches observing projects. }
procedure SemanticSourceEdits(const AFailure, ACheckpoint: TNyxText);
const
  CCaption: TNyxText = 'A prepared notebook 🚀 with letter 𐐷.';
var
  LInitial: TNyxDataValue;
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LPair: TNyxProjectPair;
  LAccepted: TNyxText;
  LLock: TFileStream;
  LBytes: TNyxBytes;
  LDocument: TNyxDocument;
  LTitleExpected: TNyxText;
  LTitleReplacement: TNyxText;

  function Change(const AExpected, AReplacement: TNyxText): TNyxDataValue;
  var
    LPosition: Integer;
    LOffset: Integer;
    LWindow: TNyxDataValue;
  begin
    LPosition := Pos(AExpected, LPair.Source);
    Check(LPosition > 0, 'source-range fixture owns the exact requested context');
    LOffset := NyxTextScalarCount(Copy(LPair.Source, 1, LPosition - 1));
    LWindow := GEngine.InvokeTool('nyx_pascal', COwner, CActor, NyxObject([
      NyxField('mode', NyxData('unit')), NyxField('expectedRevision', NyxData(Revision)),
      NyxField('offset', NyxData(LOffset)),
      NyxField('count', NyxData(NyxTextScalarCount(AExpected)))]));
    Check((LWindow.Field('revision').AsInteger = Revision) and
      (LWindow.Field('text').AsText = AExpected),
      'bounded accepted-source windows supply exact scalar range expectations');
    Result := NyxObject([NyxField('offset', NyxData(LOffset)),
      NyxField('expected', LWindow.Field('text')),
      NyxField('replacement', NyxData(AReplacement))]);
  end;

  function Args(const AID: TNyxText; const AChanges: array of TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('edit-unit')),
      NyxField('expectedRevision', NyxData(Revision)), NyxField('operationId', NyxData(AID)),
      NyxField('changes', NyxArray(AChanges))]);
  end;

  function Single(const AID, AReplacement: TNyxText): TNyxDataValue;
  begin
    Result := Args(AID, [Change('  Result := CQualificationText;', AReplacement)]);
  end;

  function Apply(const AArguments: TNyxDataValue): TNyxDataValue;
  begin
    Result := GEngine.InvokeTool('nyx_pascal', COwner, CActor, AArguments);
  end;

  procedure Retained(const AReason: TNyxText);
  begin
    LAfter := Observe;
    Check((LBefore.Field('project').AsText = LAfter.Field('project').AsText) and
      SameSession(LBefore.Field('session'), LAfter.Field('session')), AReason);
  end;

  procedure Refused(const AReason: TNyxText);
  begin
    LStatus := Await(LReceipt.Field('job').AsText);
    Check(LStatus.Field('publication').Field('state').AsText = 'refused', AReason);
    Retained(AReason + ' retains the exact accepted pair and history');
  end;
begin
  LInitial := Observe;
  LPair := DecodeNyxProject(LInitial.Field('project').AsText);
  LTitleExpected := '    LDocument.Title := ''Handwritten notebook'';';
  LTitleReplacement := '    LDocument.Title := TNotebookCards.Caption(''Notes: '');';
  { The full existing journey has already appended a title customization.
    Change that effective setter deliberately; its original constructor title
    no longer controls the final view. The focused fixture has no continuation. }

  if Pos('  ADocument.Title := ''A resource-backed notebook'';', LPair.Source) > 0 then
  begin
    LTitleExpected := '  ADocument.Title := ''A resource-backed notebook'';';
    LTitleReplacement := '  ADocument.Title := TNotebookCards.Caption(''Notes: '');';
  end;
  LArguments := Args('source-range-class-and-view', [
    Change('class function Caption: TNyxText;',
      'class function Caption(const APrefix: TNyxText = ''''): TNyxText;'),
    Change('class function TNotebookCards.Caption: TNyxText;',
      'class function TNotebookCards.Caption(const APrefix: TNyxText): TNyxText;'),
    Change('  Result := CQualificationText;',
      '  Result := APrefix + ''A prepared notebook 🚀 with letter 𐐷.'';'),
    Change(LTitleExpected, LTitleReplacement)]);
  LBefore := Observe;
  LReceipt := Apply(LArguments);
  Check((LReceipt.Field('operation').AsText = 'source-edit') and
    (LReceipt.Field('scope').AsText = 'construction') and
    (LReceipt.Field('sourceEdit').Field('changes').AsInteger = 4),
    'four coordinated ranges queue one bounded constructor receipt');
  Check(not NyxAgentHas(LReceipt, 'source') and not NyxAgentHas(LReceipt, 'changes'),
    'queued source edits do not dump proposed text or range payload');
  Retained('queued source edits keep the visible accepted buffer and draft exact');
  Check(Apply(LArguments).ToJSON = LReceipt.ToJSON, 'queued source retry retains its one receipt');
  LStatus := Await(LReceipt.Field('job').AsText);
  Check((LStatus.Field('state').AsText = 'succeeded') and
    (LStatus.Field('publication').Field('state').AsText = 'committed') and
    (Revision = LArguments.Field('expectedRevision').AsInteger + 1),
    'actual class/helper construction publishes one exact next revision');
  LAfter := Observe;
  LAccepted := LAfter.Field('project').AsText;
  LPair := DecodeNyxProject(LAccepted);
  LDocument := TNyxCodec.Decode(LPair.Design);
  try
    Check((LDocument.Title = 'Notes: ' + CCaption) and
      (LDocument.State.Value('greeting').TextValue = CCaption),
      'general source edits deliberately admit new computed title and typed defaults: ' +
        LDocument.Title + ' / ' + LDocument.State.Value('greeting').TextValue);
    Check(LDocument.Find('note-caption').Prop('text') = CCaption,
      'changed handwritten class implementation reaches its reusable control');
  finally
    LDocument.Free;
  end;
  Check(not LPair.Pending and
    (Pos('class function Caption(const APrefix:', LPair.Source) > 0) and
    (Pos('class function TNotebookCards.Caption(const APrefix:', LPair.Source) > 0) and
    (Pos('function PageName(AIndex: Integer)', LPair.Source) > 0),
    'exact class counterparts and surrounding helpers survive ordinary publication');
  Check((LAfter.Field('session').Field('selection').AsText =
    LBefore.Field('session').Field('selection').AsText) and
    (LAfter.Field('session').Field('view').AsText = LBefore.Field('session').Field('view').AsText),
    'source edits retain surviving selection and active view');
  Check(Apply(LArguments).ToJSON = LReceipt.ToJSON, 'completed source retry retains its original receipt');
  Check(History('undo').Field('project').AsText = LInitial.Field('project').AsText,
    'one Undo restores original handwritten source and computed design');
  Check(History('redo').Field('project').AsText = LAccepted,
    'one Redo restores the exact class/helper/view pair');
  History('undo');
  LPair := DecodeNyxProject(Observe.Field('project').AsText);
  LBefore := Observe;
  LArguments := Args('source-range-late-expectation', [
    Change('class function Caption: TNyxText;', 'class function Wrong: TNyxText;'),
    NyxObject([NyxField('offset', NyxData(NyxTextScalarCount(LPair.Source) - 1)),
      NyxField('expected', NyxData('wrong')), NyxField('replacement', NyxData('last'))])]);
  Refuse('nyx_pascal', LArguments, 'invalid final expected range refuses before queuing');
  Retained('late range refusal preserves complete pair and existing Redo');
  LReceipt := Apply(Single('source-range-compiler-error', '  Result := MissingNyxSourceSymbol;'));
  Refused('ordinary Pascal diagnostics refuse general source publication');
  Check((LStatus.Field('diagnostics').Field('items').Count > 0) and
    not LStatus.Field('currentSource').AsBoolean,
    'failed source diagnostics are bounded and cannot navigate active accepted text');
  Marker(AFailure, True);
  try
    LReceipt := Apply(Single('source-range-constructor-error',
      '  Result := ''A changed constructor'';'));
    Refused('constructor exception refuses complete source publication');
  finally
    Marker(AFailure, False);
  end;
  LBytes := ReadBytes(ACheckpoint);
  LLock := TFileStream.Create(ACheckpoint, fmOpenRead or fmShareDenyWrite);
  try
    LReceipt := Apply(Single('source-range-durable-error', '  Result := ''A durable proposal'';'));
    Refused('durable write refusal rolls back the complete source/design publication');
  finally
    LLock.Free;
  end;
  Check(SameBytes(ReadBytes(ACheckpoint), LBytes), 'source refusal retains exact checkpoint bytes');
  Draft(LPair.Source + CUnfinished);
  Refuse('nyx_pascal', Single('source-range-pending', '  Result := ''A pending proposal'';'),
    'existing unfinished text refuses new range capture');
  ClearDraft;
  LBefore := Observe;
  Marker(GGate, True);
  LReceipt := Apply(Single('source-range-cancelled', '  Result := ''A cancelled proposal'';'));
  GEngine.InvokeBuild(COwner, CActor, NyxObject([
    NyxField('mode', NyxData('cancel')), NyxField('job', LReceipt.Field('job')),
    NyxField('expectedRevision', NyxData(Revision)),
    NyxField('operationId', NyxData('source-range-cancel'))]));
  Marker(GGate, False);
  Refused('cancelled source construction cannot publish');
  Marker(GGate, True);
  LReceipt := Apply(Single('source-range-later-draft', '  Result := ''A later proposal'';'));
  LBefore := Draft(LPair.Source + CNewDraft);
  Marker(GGate, False);
  Refused('later user draft retires the captured unchanged-buffer proposal');
  ClearDraft;
  Marker(GGate, True);
  LReceipt := Apply(Single('source-range-permission', '  Result := ''A revoked proposal'';'));
  Permission('readOnly');
  LBefore := Observe;
  Marker(GGate, False);
  Refused('revoked permission refuses source publication after construction');
  Permission('edit');
  Check(Observe.Field('project').AsText = LInitial.Field('project').AsText,
    'general source journey leaves its original owned pair intact');
end;
