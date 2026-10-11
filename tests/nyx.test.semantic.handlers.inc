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

{ Production engine, real native constructors and ordinary durable history.
  This fixture owns its runtime and never launches a listener or modifies an
  observing project. Body compilation does not establish physical dispatch. }
procedure SemanticHandlers(const AFailure, AMismatch, ACheckpoint: TNyxText);
const
  CFirst: TNyxText = 'begin' + #10 + '  { A crafted response 🚀. }' + #10 +
    '  AExecution.Cancel;' + #10 + 'end;';
  CSecond: TNyxText = 'begin' + #10 + '  { A second response 𐐷. }' + #10 + 'end;';
  CNext: TNyxText = 'begin' + #10 + '  { Later authored work. }' + #10 + 'end;';
var
  LInitial: TNyxDataValue;
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LRows: TNyxDataValue;
  LNoOp: TNyxDataValue;
  LWindow: TNyxDataValue;
  LAccepted: TNyxText;
  LPair: TNyxProjectPair;
  LFirst: TNyxHandlerRef;
  LSecond: TNyxHandlerRef;
  LFirstBody: TNyxHandlerSource;
  LSecondBody: TNyxHandlerSource;
  LLock: TFileStream;
  LBytes: TNyxBytes;

  function Args(const AID: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(Revision)),
      NyxField('operationId', NyxData(AID)), NyxField('changes', AChanges)]);
  end;

  function Changes(const AExpected, ABody: TNyxText): TNyxDataValue;
  begin
    Result := CaptureNyxHandlerBatch(NewNyxHandlerPatch([
      NyxHandlerEdit(LFirst, AExpected, ABody)])).ToData;
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
    Retained(AReason + ' preserves exact pair, revision and history');
  end;
begin
  LInitial := Observe;
  LReceipt := GEngine.InvokeTool('nyx_callbacks', COwner, CActor,
    Args('body-fixture-callbacks', CaptureNyxCallbackBatch(NyxCallbackPatch([
      NyxAddCallback('heading-1', NyxCallbackEvent(ntClick)),
      NyxAddCallback('heading-1', NyxCallbackEvent(ntClick))])).ToData));
  LStatus := Await(LReceipt.Field('job').AsText);
  Check(LStatus.Field('publication').Field('state').AsText = 'committed',
    'body fixture adds both exact callbacks semantically');
  LFirst := NyxHandler(LReceipt.Field('proposedCallbacks').Item(0).Field('handler').AsText);
  LSecond := NyxHandler(LReceipt.Field('proposedCallbacks').Item(1).Field('handler').AsText);
  LPair := DecodeNyxProject(Observe.Field('project').AsText);
  LFirstBody := ReadNyxHandlerSource(LPair.Source, LFirst);
  LSecondBody := ReadNyxHandlerSource(LPair.Source, LSecond);
  { Agents read bounded context at the same revision before supplying exact
    expected bodies. The complete pair above belongs only to the independent
    preservation oracle, never to the editing response. }
  LWindow := GEngine.InvokeTool('nyx_pascal', COwner, CActor, NyxObject([
    NyxField('mode', NyxData('inspect')), NyxField('handler', NyxData(LFirst.Name)),
    NyxField('offset', NyxData(0)), NyxField('count', NyxData(2048))]));
  Check((LWindow.Field('revision').AsInteger = Revision) and
    (LWindow.Field('text').AsText = LFirstBody.Code),
    'bounded semantic inspection returns the exact expected body');
  LBefore := Observe;
  LArguments := Args('body-pair', CaptureNyxHandlerBatch(NewNyxHandlerPatch([
    NyxHandlerEdit(LFirst, LFirstBody.Code, CFirst),
    NyxHandlerEdit(LSecond, LSecondBody.Code, CSecond)])).ToData);
  Marker(GGate, True);
  LReceipt := Apply(LArguments);
  Check((LReceipt.Field('operation').AsText = 'handlers') and
    (LReceipt.Field('proposedHandlers').Count = 2) and (Length(LReceipt.ToJSON) < 8192) and
    (LReceipt.Field('compiledSource').AsText = ''), 'body receipt supplies bounded proposed navigation');
  Retained('enqueue retains the complete baseline');
  Check(Apply(LArguments).ToJSON = LReceipt.ToJSON, 'queued body retry returns the original job');
  Refuse('nyx_pascal', Args('body-invalid-tail', CaptureNyxHandlerBatch(NewNyxHandlerPatch([
    NyxHandlerEdit(LFirst, LFirstBody.Code, CFirst),
    NyxHandlerEdit(LSecond, 'stale expected body', CSecond)])).ToData),
    'an invalid final body refuses the whole group before enqueue');
  Refuse('nyx_transaction', Group(Revision, 'body-pair', NyxArray([Title('Other')])),
    'body operation IDs cannot cross mutation tools');
  Marker(GGate, False);
  LStatus := Await(LReceipt.Field('job').AsText);
  Check((LStatus.Field('state').AsText = 'succeeded') and
    (LStatus.Field('publication').Field('state').AsText = 'committed') and
    (LStatus.Field('publication').Field('revision').AsInteger =
    LBefore.Field('session').Field('revision').AsInteger + 1), 'two actual compiled bodies publish one revision');
  LAfter := Observe;
  LAccepted := LAfter.Field('project').AsText;
  LPair := DecodeNyxProject(LAfter.Field('project').AsText);
  Check((ReadNyxHandlerSource(LPair.Source, LFirst).Code = CFirst) and
    (ReadNyxHandlerSource(LPair.Source, LSecond).Code = CSecond), 'both exact Unicode bodies survive publication');
  Check((LPair.Design = DecodeNyxProject(LBefore.Field('project').AsText).Design) and
    (ReadNyxHandlerSource(LPair.Source, LFirst).Signature = LFirstBody.Signature) and
    (Pos('class function TNotebookCards.Caption', LPair.Source) > 0),
    'body publication preserves whole design, signature and handwritten helpers');
  LRows := LReceipt.Field('proposedHandlers');
  Check((LRows.Item(0).Field('line').AsInteger = ReadNyxHandlerSource(LPair.Source, LFirst).Line) and
    (LRows.Item(1).Field('line').AsInteger = ReadNyxHandlerSource(LPair.Source, LSecond).Line),
    'all navigation rows locate the final retained source');
  Check(Apply(LArguments).ToJSON = LReceipt.ToJSON, 'completed body retry returns its original receipt');
  Check(History('undo').Field('project').AsText = LBefore.Field('project').AsText,
    'one Undo restores both original bodies and complete design');
  LBefore := Observe;
  LArguments := Args('body-no-op', Changes(LFirstBody.Code, LFirstBody.Code));
  LNoOp := Apply(LArguments);
  Check((LNoOp.Field('handlers').Count = 1) and LNoOp.Field('canRedo').AsBoolean and
    (LNoOp.Field('revision').AsInteger = Revision),
    'an exact no-op uses the existing admitted body and preserves Redo without a job');
  Retained('body no-op preserves exact revision, history and pair');
  Check(Apply(LArguments).ToJSON = LNoOp.ToJSON, 'no-op exact retry remains bounded and identical');
  Check(History('redo').Field('project').AsText = LAccepted,
    'Redo remains available after no-op body admission');
  LBefore := Observe;
  LRows := Changes(CFirst, CNext);
  LReceipt := Apply(Args('body-compiler-refusal', Changes(CFirst,
    'begin' + #10 + '  MissingNyxQualificationSymbol;' + #10 + 'end;')));
  Refused('ordinary compiler error refuses body publication');
  Check((LStatus.Field('state').AsText = 'failed') and
    not LStatus.Field('currentSource').AsBoolean, 'failed body diagnostics cannot navigate active source');
  Marker(AFailure, True);
  LReceipt := Apply(Args('body-constructor-refusal', LRows));
  Refused('constructor exception refuses body publication');
  Marker(AFailure, False);
  Marker(AMismatch, True);
  LReceipt := Apply(Args('body-whole-meaning-refusal', LRows));
  Refused('executed constructor must retain the complete design');
  Marker(AMismatch, False);
  LBytes := ReadBytes(ACheckpoint);
  LLock := TFileStream.Create(ACheckpoint, fmOpenRead or fmShareDenyWrite);
  try
    LReceipt := Apply(Args('body-durable-refusal', LRows));
    Refused('durable body replacement refuses atomically');
  finally
    LLock.Free;
  end;
  Check(SameBytes(ReadBytes(ACheckpoint), LBytes), 'durable body refusal preserves checkpoint bytes');
  Marker(GGate, True);
  LReceipt := Apply(Args('body-cancelled', LRows));
  GEngine.InvokeBuild(COwner, CActor, NyxObject([
    NyxField('mode', NyxData('cancel')), NyxField('job', LReceipt.Field('job')),
    NyxField('expectedRevision', NyxData(Revision)),
    NyxField('operationId', NyxData('cancel-body-proof'))]));
  Marker(GGate, False);
  Refused('cancelled body constructor cannot publish');
  Draft(LPair.Source + CUnfinished);
  Refuse('nyx_pascal', Args('body-pending', LRows), 'pending drafts refuse body admission');
  ClearDraft;
  Marker(GGate, True);
  LReceipt := Apply(Args('body-later-draft', LRows));
  LBefore := Draft(LPair.Source + CNewDraft);
  Marker(GGate, False);
  Refused('later draft refuses body completion without discarding typed work');
  ClearDraft;
  Marker(GGate, True);
  LReceipt := Apply(Args('body-revoked', LRows));
  Permission('readOnly');
  LBefore := Observe;
  Marker(GGate, False);
  Refused('revoked permission refuses body completion');
  Permission('edit');
  History('undo');
  Check(History('undo').Field('project').AsText = LInitial.Field('project').AsText,
    'body journey restores its entire original owned pair');
end;

