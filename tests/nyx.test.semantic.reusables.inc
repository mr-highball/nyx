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

{ Production semantic/compiler consumer: derive and customize exact independent
  definitions, then promote a source control without changing its public identity.
  The backend owns source preparation, real execution and paired publication. }
procedure SemanticReusables;
var
  LInitial: TNyxDataValue;
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LDocument: TNyxDocument;
  LCounter: Integer;

  procedure Apply(const AOperations: TNyxDataValue);
  begin
    Inc(LCounter);
    LBefore := Observe;
    LArguments := Group(Revision, 'handwritten-reuse-' + IntToStr(LCounter), AOperations);
    Marker(GGate, True);
    try
      LReceipt := Submit(LArguments);
      Check(LReceipt.Field('publication').Field('state').AsText = 'pending',
        'reusable changes queue execution without publishing their proposed source');
      Check(Observe.Field('project').AsText = LBefore.Field('project').AsText,
        'the current pair remains exact during reusable construction');
      Check(Submit(LArguments).ToJSON = LReceipt.ToJSON,
        'queued reusable retries retain the original bounded receipt');
    finally
      Marker(GGate, False);
    end;
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('publication').Field('state').AsText = 'committed') and
      (Revision = LBefore.Field('session').Field('revision').AsInteger + 1),
      'actual reusable construction publishes one exact next revision');
    LAfter := Observe;
    Check(Submit(LArguments).ToJSON = LReceipt.ToJSON,
      'completed reusable retries retain their original receipt');
    Check(Pos('class function TNotebookCards.Caption',
      DecodeNyxProject(LAfter.Field('project').AsText).Source) > 0,
      'reusable authoring retains the handwritten helper');
    Check(History('undo').Field('project').AsText = LBefore.Field('project').AsText,
      'one Undo restores the whole pre-reusable pair');
    Check(History('redo').Field('project').AsText = LAfter.Field('project').AsText,
      'one Redo restores the whole constructed reusable pair');
  end;

begin
  LInitial := Observe;
  LCounter := 0;
  Apply(TNyxDataValue.ParseJSON(
    '[{"op":"derive","source":"reusable-note","id":"featured-note",' +
    '"identities":{"note-caption":"featured-caption"}},' +
    '{"op":"instance","component":"featured-note","id":"featured-1","parent":"notebook-1"},' +
    '{"op":"override","instance":"featured-1","id":"featured-title",' +
    '"path":"caption","mode":"properties"},' +
    '{"op":"update","id":"featured-title","properties":{"text":"Featured notebook"}}]'));
  LDocument := TNyxCodec.Decode(DecodeNyxProject(LAfter.Field('project').AsText).Design);
  try
    Check((LDocument.FindComponent('featured-note') <> nil) and
      (LDocument.Find('featured-caption').Prop('text') =
        LDocument.Find('note-caption').Prop('text')) and
      (LDocument.Find('featured-title').Prop('text') = 'Featured notebook'),
      'derived parts and a local override preserve the independent definition');
  finally
    LDocument.Free;
  end;
  Apply(NyxDesignChanges(NyxRootOrderPatch([
    NyxMoveRoot(NyxPageRoot('notebook-2'), 0),
    NyxMoveRoot(NyxReusableRoot('featured-note'), 0)])).ToData);
  LDocument := TNyxCodec.Decode(DecodeNyxProject(LAfter.Field('project').AsText).Design);
  try
    Check((LDocument.Pages[0].ID = 'notebook-2') and
      (LDocument.Components[0].ID = 'featured-note') and
      (LDocument.Find('featured-1').Prop('component') = 'featured-note'),
      'actual chronological root movement preserves both partitions and reusable references');
    Check(Pos('ADocument.MoveRoot(NyxPageRoot(',
      DecodeNyxProject(LAfter.Field('project').AsText).Source) > 0,
      'handwritten root ordering emits the existing typed source continuation');
  finally
    LDocument.Free;
  end;
  Apply(TNyxDataValue.ParseJSON(
    '[{"op":"inherit","instance":"featured-1","id":"featured-title","path":"caption"}]'));
  LDocument := TNyxCodec.Decode(DecodeNyxProject(LAfter.Field('project').AsText).Design);
  try
    Check((LDocument.Find('featured-title') = nil) and
      (LDocument.Find('featured-1') <> nil) and
      (LDocument.FindComponent('featured-note') <> nil),
      'restored inheritance removes only the local descriptor and keeps its definition');
  finally
    LDocument.Free;
  end;
  { A leaf promotion is the minimal regression: the authored identity stays
    heading-1 while its specialized type becomes INyxComponent. }
  Apply(TNyxDataValue.ParseJSON(
    '[{"op":"derive","source":"heading-1","id":"page-heading","identities":{}},' +
    '{"op":"delete","id":"heading-1"},' +
    '{"op":"instance","component":"page-heading","id":"heading-1",' +
    '"parent":"notebook-1","index":0}]'));
  LDocument := TNyxCodec.Decode(DecodeNyxProject(LAfter.Field('project').AsText).Design);
  try
    Check((LDocument.Find('heading-1').Kind = NyxKindName(nkComponent)) and
      (LDocument.FindComponent('page-heading').Kind = NyxKindName(nkHeading)),
      'extraction keeps the original identity with a specialized reusable instance');
  finally
    LDocument.Free;
  end;
  { Four deliberate source groups, independent from their tested Undo/Redo,
    return the owned fixture to its original complete pair. }
  History('undo');
  History('undo');
  History('undo');
  Check(History('undo').Field('project').AsText = LInitial.Field('project').AsText,
    'reusable workflow leaves its original fixture restored');
end;

{ Actual executed-source seeds must preserve their opaque accepted checkpoint.
  Files alone cannot re-enter the literal importer. New independent owners copy
  accepted meaning only, while the primary's unfinished draft/history stay exact. }
procedure SemanticAcceptedSeeds;
var
  LInitial: TNyxDataValue;
  LParent: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LContext: TNyxDataValue;
  LReview: TNyxReviewRef;
  LWorkspace: TNyxWorkspaceRef;
  LKept: TNyxWorkspaceRef;
  LKeepArguments: TNyxDataValue;
  LKeepReceipt: TNyxDataValue;
  LReviewSource: TNyxText;
  LStarted: QWord;
  LStage: Integer;

  function InContext(const AArguments: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxWithWorkspace(NyxWithReview(AArguments, LReview), LWorkspace);
  end;

  procedure ParentExact;
  var
    LNow: TNyxDataValue;
  begin
    LNow := Observe;
    Check((LNow.Field('project').AsText = LParent.Field('project').AsText) and
      SameSession(LNow.Field('session'), LParent.Field('session')),
      'accepted seed retains the original primary pair, draft, revision and history');
  end;

  { Even this small gated fixture can exceed one scalar window. Join bounded
    owned-context queries at the same revision; never relax the whole-unit check. }
  function ContextSource(AExpected: Integer = 1): TNyxText;
  var
    LWindow: TNyxDataValue;
    LOffset: Integer;
  begin
    Result := '';
    LOffset := 0;
    repeat
      LWindow := GEngine.InvokeTool('nyx_pascal', COwner, CActor, InContext(NyxObject([
        NyxField('mode', NyxData('unit')), NyxField('expectedRevision', NyxData(AExpected)),
        NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(2048))])));
      Result := Result + LWindow.Field('text').AsText;

      if (LWindow.Field('nextOffset').AsInteger <= LOffset) and
        (LOffset < LWindow.Field('total').AsInteger) then
      begin
        raise Exception.Create('Accepted seed source window made no progress');
      end;
      LOffset := LWindow.Field('nextOffset').AsInteger;
    until LOffset >= LWindow.Field('total').AsInteger;
  end;

begin
  LInitial := Observe;
  Draft(DecodeNyxProject(LInitial.Field('project').AsText).Source + CUnfinished);
  LParent := Observe;
  for LStage := 0 to 1 do
  begin
    LReview := NyxActiveWorkspace;
    LWorkspace := NyxPrimaryWorkspace;

    if LStage = 0 then
    begin
      LReceipt := GEngine.InvokeTool('nyx_reviews', COwner, CActor, NyxObject([
        NyxField('mode', NyxData('create')), NyxField('expectedRevision', NyxData(Revision)),
        NyxField('operationId', NyxData('fork-accepted-review')),
        NyxField('label', NyxData('A reusable notebook review')),
        NyxField('base', NyxData('accepted'))]));
      LReview := NyxReview(LReceipt.Field('review').AsText);
    end
    else
    begin
      LReceipt := GEngine.InvokeTool('nyx_workspaces', COwner, CActor, NyxObject([
        NyxField('mode', NyxData('create')), NyxField('expectedRevision', NyxData(Revision)),
        NyxField('operationId', NyxData('fork-accepted-project')),
        NyxField('label', NyxData('An independent notebook project')),
        NyxField('base', NyxData('accepted'))]));
      LWorkspace := NyxWorkspace(LReceipt.Field('workspace').AsText);
    end;
    LContext := GEngine.InvokeTool('nyx_session', COwner, CActor, InContext(NyxObject([])));
    Check((LContext.Field('revision').AsInteger = 1) and
      not LContext.Field('pendingDraft').AsBoolean and
      not LContext.Field('canUndo').AsBoolean and not LContext.Field('canRedo').AsBoolean,
      'accepted source seed starts an independent clean history without the primary draft');
    Check(ContextSource = DecodeNyxProject(LInitial.Field('project').AsText).Source,
      'accepted source seed retains the exact complete handwritten Pascal');
    ParentExact;
    LReceipt := GEngine.InvokeTool('nyx_transaction', COwner, CActor, InContext(
      Group(1, 'edit-accepted-seed', NyxArray([Title('An independently crafted notebook')]))));
    Check(LReceipt.Field('publication').Field('state').AsText = 'pending',
      'seeded executed source still requires actual compiler admission');
    LStarted := GetTickCount64;
    repeat
      LStatus := GEngine.InvokeBuild(COwner, CActor, InContext(NyxObject([
        NyxField('mode', NyxData('status')), NyxField('job', LReceipt.Field('job')),
        NyxField('limit', NyxData(3))])));

      if LStatus.Field('publication').Field('state').AsText <> 'pending' then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 90000 then
      begin
        raise Exception.Create('Accepted seed constructor exceeded its deadline');
      end;
      Sleep(5);
    until False;
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (LStatus.Field('publication').Field('state').AsText = 'committed') and
      (LStatus.Field('publication').Field('revision').AsInteger = 2),
      'independent accepted seed publishes one actual constructor result');
    ParentExact;

    if LStage = 0 then
    begin
      LReviewSource := ContextSource(2);
      LKeepArguments := NyxObject([
        NyxField('mode', NyxData('create')), NyxField('base', NyxData('accepted')),
        NyxField('sourceReview', NyxData(LReview.ID)), NyxField('expectedRevision', NyxData(2)),
        NyxField('operationId', NyxData('keep-accepted-review')),
        NyxField('label', NyxData('A lasting reviewed notebook'))]);
      LKeepReceipt := GEngine.InvokeTool('nyx_workspaces', COwner, CActor, LKeepArguments);
      LKept := NyxWorkspace(LKeepReceipt.Field('workspace').AsText);
      Check((LKeepReceipt.Field('session').Field('revision').AsInteger = 1) and
        not LKeepReceipt.Field('pendingDraftExcluded').AsBoolean,
        'handwritten review handoff returns a fresh ordinary project with bounded seed metadata');
      Check(GEngine.InvokeTool('nyx_workspaces', COwner, CActor, LKeepArguments).ToJSON =
        LKeepReceipt.ToJSON, 'actual durable handoff retains its exact retry receipt');
      LWorkspace := LKept;
      LReview := NyxActiveWorkspace;
      Check(ContextSource = LReviewSource, 'handoff retains exact executed review source');
      ParentExact;
      LReceipt := GEngine.InvokeTool('nyx_transaction', COwner, CActor,
        InContext(Group(1, 'edit-kept-review', NyxArray([Title('A lasting notebook application')]))));
      Check(LReceipt.Field('publication').Field('state').AsText = 'pending',
        'handed-off source retains actual constructor admission');
      LStarted := GetTickCount64;
      repeat
        LStatus := GEngine.InvokeBuild(COwner, CActor, InContext(NyxObject([
          NyxField('mode', NyxData('status')), NyxField('job', LReceipt.Field('job')),
          NyxField('limit', NyxData(3))])));

        if LStatus.Field('publication').Field('state').AsText <> 'pending' then
        begin
          Break;
        end;

        if GetTickCount64 - LStarted > 90000 then
        begin
          raise Exception.Create('Handed-off source constructor exceeded its deadline');
        end;
        Sleep(5);
      until False;
      Check((LStatus.Field('state').AsText = 'succeeded') and
        (LStatus.Field('publication').Field('state').AsText = 'committed') and
        (LStatus.Field('publication').Field('revision').AsInteger = 2),
        'ordinary project publishes its independent real constructor result');
      GEngine.InvokeTool('nyx_history', COwner, CActor, InContext(NyxObject([
        NyxField('expectedRevision', NyxData(2)), NyxField('direction', NyxData('undo')),
        NyxField('operationId', NyxData('undo-kept-review'))])));
      Check(ContextSource(3) = LReviewSource, 'one handed-off project Undo restores exact review Pascal');
      LWorkspace := NyxPrimaryWorkspace;
      LReview := NyxReview(LKeepArguments.Field('sourceReview').AsText);
      Check(ContextSource(2) = LReviewSource, 'handed-off project edits leave its original review exact');
      Check(GEngine.InvokeTool('nyx_reviews', COwner, CActor, NyxObject([
        NyxField('mode', NyxData('discard')), NyxField('review', NyxData(LReview.ID)),
        NyxField('expectedRevision', NyxData(2)),
        NyxField('operationId', NyxData('discard-accepted-review'))]))
        .Field('disposed').AsBoolean, 'the transport can retire its independent accepted review');
      Check(GEngine.InvokeTool('nyx_workspaces', COwner, CActor, LKeepArguments).ToJSON =
        LKeepReceipt.ToJSON, 'durable handoff retry survives accepted-review retirement');
      ParentExact;
    end;
  end;
  ClearDraft;
  Check(Observe.Field('project').AsText = LInitial.Field('project').AsText,
    'accepted seed qualification restores its original primary fixture');
end;
