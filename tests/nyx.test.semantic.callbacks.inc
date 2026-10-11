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

{ Real production semantic dispatch and native compiler/executed-constructor
  admission, under this fixture's independent runtime. No listener or observing
  project is changed. Metadata execution is distinct from physical event input. }
procedure SemanticCallbacks(AExecutor: TNyxBuildExecutor;
  const AFailure, ACheckpoint: TNyxText);
const
  CBody: TNyxText = 'begin' + #10 +
    '  { Handwritten response 🚀 𐐷 stays owned. }' + #10 +
    '  AExecution.Cancel;' + #10 + 'end;';
var
  LInitial: TNyxDataValue;
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LChanges: TNyxDataValue;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LReview: TNyxDataValue;
  LPair: TNyxProjectPair;
  LFirst: TNyxCallbackRef;
  LSecond: TNyxCallbackRef;
  LHandler: TNyxHandlerRef;
  LSource: TNyxHandlerSource;
  LBuild: INyxSourceProjectionBuild;
  LPublication: TNyxStudioSourcePublication;
  LEditedSource: TNyxText;
  LDocument: TNyxDocument;
  LInfos: TNyxAuthoredEventInfos;
  LRejected: Boolean;
  LIndex: Integer;
  LLock: TFileStream;
  LBytes: TNyxBytes;

  function Args(const AID: TNyxText; const AChanges: TNyxDataValue;
    const AReviewID: TNyxText = ''): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
  begin
    SetLength(LFields, 4);
    LFields[0] := NyxField('mode', NyxData('apply'));
    LFields[1] := NyxField('expectedRevision', NyxData(Revision));
    LFields[2] := NyxField('operationId', NyxData(AID));
    LFields[3] := NyxField('changes', AChanges);

    if AReviewID <> '' then
    begin
      SetLength(LFields, 5);
      LFields[4] := NyxField('reviewID', NyxData(AReviewID));
    end;
    Result := NyxObject(LFields);
  end;

  function Apply(const AArguments: TNyxDataValue): TNyxDataValue;
  begin
    Result := GEngine.InvokeTool('nyx_callbacks', COwner, CActor, AArguments);
  end;

  procedure PairedHistory(const AReason: TNyxText);
  begin
    LAfter := Observe;
    Check(History('undo').Field('project').AsText = LBefore.Field('project').AsText,
      AReason + ' has one exact paired Undo');
    Check(History('redo').Field('project').AsText = LAfter.Field('project').AsText,
      AReason + ' has one exact paired Redo');
  end;

  procedure Retained(const AReason: TNyxText);
  begin
    LAfter := Observe;
    Check((LAfter.Field('project').AsText = LBefore.Field('project').AsText) and
      SameSession(LAfter.Field('session'), LBefore.Field('session')), AReason);
  end;

  procedure Committed(const AReason: TNyxText);
  begin
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (LStatus.Field('publication').Field('state').AsText = 'committed'),
      AReason + ': ' + LStatus.ToJSON);
    Check((LStatus.Field('publication').Field('revision').AsInteger =
      LBefore.Field('session').Field('revision').AsInteger + 1) and
      LStatus.Field('currentSource').AsBoolean, AReason + ' publishes exactly one revision');
    PairedHistory(AReason);
  end;
begin
  LInitial := Observe;
  LBefore := LInitial;
  LChanges := CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxAddCallback('heading-1', NyxCallbackEvent(ntClick)),
    NyxAddCallback('heading-1', NyxCallbackEvent(ntClick)),
    NyxSetCallbackPolicy('heading-1', NyxCallbackEvent(ntClick), neUIQueue)])).ToData;
  Marker(GGate, True);
  LArguments := Args('callback-add-pair', LChanges);
  LReceipt := Apply(LArguments);
  Check((LReceipt.Field('operation').AsText = 'callbacks') and
    (LReceipt.Field('purpose').AsText = 'transaction') and
    (LReceipt.Field('publication').Field('state').AsText = 'pending'),
    'callbacks reuse the existing asynchronous constructor lifecycle');
  Check((LReceipt.Field('proposedCallbacks').Count = 3) and
    (Length(LReceipt.ToJSON) < 8192) and (LReceipt.Field('compiledSource').AsText = ''),
    'callback proposal is bounded without returning source');
  LFirst := NyxCallbackID(LReceipt.Field('proposedCallbacks').Item(0).Field('registration').AsText);
  LSecond := NyxCallbackID(LReceipt.Field('proposedCallbacks').Item(1).Field('registration').AsText);
  LHandler := NyxHandler(LReceipt.Field('proposedCallbacks').Item(0).Field('handler').AsText);
  Check((LHandler.Name <> 'THeading1Click') and (LFirst.Name <> LSecond.Name),
    'planner reserves handwritten class names and each new registration');
  Retained('enqueue retains the exact pair, revision and history');
  Check(Apply(LArguments).ToJSON = LReceipt.ToJSON, 'queued callback retry returns the exact original receipt');
  Refuse('nyx_transaction', Group(Revision, 'callback-add-pair', NyxArray([Title('Other')])),
    'callback operation IDs cannot be reused by another mutation tool');
  Refuse('nyx_callbacks', Args('callback-add-pair', CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxAddCallback('heading-2', NyxCallbackEvent(ntClick))])).ToData),
    'changed callback retry refuses atomically');
  Marker(GGate, False);
  Committed('real constructor publishes both registrations and policy together');
  Check(Apply(LArguments).ToJSON = LReceipt.ToJSON, 'completed callback retry keeps its original receipt');
  LPair := DecodeNyxProject(Observe.Field('project').AsText);
  LSource := ReadNyxHandlerSource(LPair.Source, LHandler);
  Check((Pos('TODO', LSource.Code) > 0) and
    (LReceipt.Field('proposedCallbacks').Item(0).Field('line').AsInteger = LSource.Line),
    'proposed line locates the retained source stub after actual publication');
  Check((Pos('class function TNotebookCards.Caption', LPair.Source) > 0) and
    (Pos('for LIndex := 1 to 2 do', LPair.Source) > 0),
    'callback group retains complete handwritten helpers and construction loop');
  { The bounded body writer admits handwritten ownership, while the older
    nyx_pascal candidate still requires literal view boundaries. Record that
    separate consumer gap; this fixture seeds its crafted body through ordinary
    trusted source Apply and the real compiler, never a supplied execution flag. }
  Refuse('nyx_pascal', Args('callback-crafted-body', NyxArray([
    NyxObject([NyxField('handler', NyxData(LHandler.Name)),
      NyxField('expected', NyxData(LSource.Code)), NyxField('implementation', NyxData(CBody))])])),
    'older semantic body candidate refuses handwritten framing without mutation');
  LEditedSource := ReplaceNyxHandlerImplementation(LPair.Source, LHandler, LSource.Code, CBody);
  Draft(LEditedSource);
  LPublication := GEngine.EditorCaptureSourcePublication(GToken, NyxPrimaryWorkspace,
    Revision, LEditedSource);
  LBuild := AExecutor.ProjectSource(LEditedSource, NyxPascalUnit('nyx.projection.fixture'),
    btNativeLCL, spcChecked);
  Check(LBuild.Projection.State = spsExecuted, 'crafted body compiles through ordinary source Apply');
  GEngine.EditorCommitSourceProjection(GToken, LPublication, LBuild.Projection,
    NyxControl('heading-1'), NyxControl('notebook-1'));
  LBefore := Observe;
  LChanges := CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxMoveCallback('heading-1', NyxCallbackEvent(ntClick), LSecond, 0),
    NyxSetCallbackPolicy('heading-1', NyxCallbackEvent(ntClick), neAsynchronous)])).ToData;
  LReceipt := Apply(Args('callback-order-policy', LChanges));
  Committed('order and policy group');
  LPair := DecodeNyxProject(Observe.Field('project').AsText);
  Check(ReadNyxHandlerSource(LPair.Source, LHandler).Code = CBody,
    'moving callbacks retains the exact crafted Unicode implementation');
  LDocument := TNyxCodec.Decode(LPair.Design);
  try
    LInfos := NyxAuthoredEvents(LDocument.Find('heading-1'));
    Check((Length(LInfos) = 1) and (LInfos[0].Policy = neAsynchronous) and
      (Length(LInfos[0].Callbacks) = 2) and (LInfos[0].Callbacks[0].ID.Name = LSecond.Name),
      'executed whole design contains the requested typed order and policy');
  finally
    LDocument.Free;
  end;
  LChanges := CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxRemoveCallback('heading-1', NyxCallbackEvent(ntClick), LFirst),
    NyxSetCallbackPolicy('heading-1', NyxCallbackEvent(ntClick), neSequential)])).ToData;
  Refuse('nyx_callbacks', Args('callback-unreviewed', LChanges),
    'removing a compiled callback still requires an exact current review');
  LBefore := Observe;
  LReview := GEngine.InvokeTool('nyx_callbacks', COwner, CActor, NyxObject([
    NyxField('mode', NyxData('review')), NyxField('expectedRevision', NyxData(Revision)),
    NyxField('changes', LChanges)]));
  Check((LReview.Field('callbacks').Count = 2) and
    (Pos('retained', LReview.Field('callbacks').Item(0).Field('warning').AsText) > 0),
    'executed removal review supplies bounded warnings without compiling or publishing');
  Retained('review retains the exact pair and command history');
  LArguments := Args('callback-foreign-review', LChanges, LReview.Field('reviewID').AsText);
  LRejected := False;
  try
    GEngine.InvokeTool('nyx_callbacks', 'foreign-connection', CActor, LArguments);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'display actor cannot transfer the owning removal ticket');
  Retained('foreign review refusal retains the complete pair');
  Refuse('nyx_callbacks', Args('callback-changed-review', CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxRemoveCallback('heading-1', NyxCallbackEvent(ntClick), LSecond)])).ToData,
    LReview.Field('reviewID').AsText), 'review applies only to the exact ordered changes');
  { A review may expire without a document revision when the bounded review
    store evicts it. Completion must recheck the exact ticket, not only files. }
  Marker(GGate, True);
  LReceipt := Apply(Args('callback-evicted-review', LChanges, LReview.Field('reviewID').AsText));
  for LIndex := 1 to 16 do
  begin
    LReview := GEngine.InvokeTool('nyx_callbacks', COwner, CActor, NyxObject([
      NyxField('mode', NyxData('review')), NyxField('expectedRevision', NyxData(Revision)),
      NyxField('changes', LChanges)]));
  end;
  Marker(GGate, False);
  LStatus := Await(LReceipt.Field('job').AsText);
  Check(LStatus.Field('publication').Field('state').AsText = 'refused',
    'an evicted review refuses completion even at the unchanged revision');
  Retained('review eviction retains the exact compiled pair and history');
  { A cancelled or failed constructor cannot spend the new valid removal review. }
  Marker(GGate, True);
  LReceipt := Apply(Args('callback-cancel', LChanges, LReview.Field('reviewID').AsText));
  GEngine.InvokeBuild(COwner, CActor, NyxObject([
    NyxField('mode', NyxData('cancel')), NyxField('job', LReceipt.Field('job')),
    NyxField('expectedRevision', NyxData(Revision)),
    NyxField('operationId', NyxData('callback-cancel-proof'))]));
  Marker(GGate, False);
  LStatus := Await(LReceipt.Field('job').AsText);
  Check(LStatus.Field('publication').Field('state').AsText = 'refused',
    'cancelled callback job refuses publication');
  Retained('cancelled callback group retains files and history');
  Marker(AFailure, True);
  LReceipt := Apply(Args('callback-constructor-failure', LChanges, LReview.Field('reviewID').AsText));
  LStatus := Await(LReceipt.Field('job').AsText);
  Marker(AFailure, False);
  Check(LStatus.Field('publication').Field('state').AsText = 'refused',
    'runtime constructor failure cannot publish callback intent');
  Retained('constructor failure retains the exact pair and review');
  { Capture before the publication lock to verify byte-exact durable rollback. }
  LBytes := ReadBytes(ACheckpoint);
  LLock := TFileStream.Create(ACheckpoint, fmOpenRead or fmShareDenyWrite);
  try
    LReceipt := Apply(Args('callback-durable-refusal', LChanges, LReview.Field('reviewID').AsText));
    LStatus := Await(LReceipt.Field('job').AsText);
  finally
    LLock.Free;
  end;
  Check(LStatus.Field('publication').Field('state').AsText = 'refused',
    'durable callback replacement failure refuses publication');
  Retained('durable callback failure restores the exact pair, revision and history');
  Check(SameBytes(ReadBytes(ACheckpoint), LBytes), 'durable callback failure preserves checkpoint bytes');
  LArguments := Args('callback-reviewed-removal', LChanges, LReview.Field('reviewID').AsText);
  LReceipt := Apply(LArguments);
  Committed('review remains usable after cancellation and both failures');
  LPair := DecodeNyxProject(Observe.Field('project').AsText);
  Check(ReadNyxHandlerSource(LPair.Source, LHandler).Code = CBody,
    'removal retains the exact original implementation');
  Refuse('nyx_callbacks', Args('callback-spent-review', LChanges, LReview.Field('reviewID').AsText),
    'successful removal cannot reuse a spent ticket at the new revision');
  { Pending input is refused before enqueue; later input refuses completion. }
  LChanges := CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxAddCallback('heading-2', NyxCallbackEvent(ntClick))])).ToData;
  Draft(LPair.Source + CUnfinished);
  Refuse('nyx_callbacks', Args('callback-pending', LChanges),
    'pending draft refuses callback admission before compilation');
  ClearDraft;
  Marker(GGate, True);
  LReceipt := Apply(Args('callback-later-draft', LChanges));
  LBefore := Draft(LPair.Source + CNewDraft);
  Marker(GGate, False);
  LStatus := Await(LReceipt.Field('job').AsText);
  Check(LStatus.Field('publication').Field('state').AsText = 'refused',
    'later draft makes callback completion stale');
  Retained('stale callback completion retains pending draft and history');
  ClearDraft;
  Marker(GGate, True);
  LReceipt := Apply(Args('callback-revoked', LChanges));
  Permission('readOnly');
  LBefore := Observe;
  Marker(GGate, False);
  LStatus := Await(LReceipt.Field('job').AsText);
  Check(LStatus.Field('publication').Field('state').AsText = 'refused',
    'revoked edit permission refuses callback completion');
  Retained('revoked callback completion preserves the entire editor pair');
  Permission('edit');
  { Leave the original pair for the existing application/transaction journeys.
    These exact three commands are body edit, ordering, and removal after add. }
  History('undo');
  History('undo');
  History('undo');
  Check(History('undo').Field('project').AsText = LInitial.Field('project').AsText,
    'callback journey restores its exact original owned pair');
end;
