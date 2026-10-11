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

{ Authenticated reusable authoring owns a freshly seeded isolated service. All
  design changes use semantic MCP; ordinary browser/LCL UI only observes the
  accepted pair and navigates editor chrome. Temporary reviews expose read-only
  previews, while ordinary named projects mount the complete editor. No reviewed
  pair is injected through private editor/session methods. }
procedure RunReusables(const AOrigin, AFixturePath: TNyxText);
const
  CReviewLabel = 'A reusable notebook review';
  CProjectLabel = 'A reusable notebook project';
var
  LBrowser: TNyxBrowserPipe;
  LParent: TNyxDataValue;
  LState: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LArgs: TNyxDataValue;
  LReply: TNyxDataValue;
  LParentSource: TNyxText;
  LBeforeSource: TNyxText;
  LAfterSource: TNyxText;
  LStage: Integer;
  LIndex: Integer;
  LStarted: QWord;
  LRecoverySelector: TNyxText;

  { Bounded physical observation has no authority to compose the document.
    Wait for exact editor text, not merely a compiler completion or DOM node. }
  procedure BrowserSource(const AExpected: TNyxText);
  var
    LStarted: QWord;
    LActual: TNyxText;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;

      if LBrowser.TryFieldValue('[data-node="studio-code"]', LActual) and
        (LActual = AExpected) then
      begin
        Check(True, 'ordinary browser editor acknowledges exact accepted Pascal');
        Exit;
      end;

      if LBrowser.RuntimeError <> '' then
      begin
        LBrowser.CaptureRuntimeError;
        raise Exception.Create('Observing browser reported a runtime exception');
      end;
      Sleep(50);
    until GetTickCount64 - LStarted >= 180000;
    LBrowser.Capture('source-refusal');
    raise Exception.Create('Observing browser source acknowledgment exceeded its budget');
  end;

  procedure BrowserFace(const ASelector: TNyxText);
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;

      if LBrowser.Exists(ASelector) then
      begin
        Exit;
      end;
      Sleep(50);
    until GetTickCount64 - LStarted >= 180000;
    LBrowser.Capture('face-refusal');
    raise Exception.Create('Observing editor chrome exceeded its budget');
  end;

  { Lifecycle calls deliberately address the isolated primary, not the current
    review/project. A related group carries its explicit context through Call. }
  function Lifecycle(const ATool: TNyxText; const AArgs: TNyxDataValue): TNyxDataValue;
  var
    LResult: TNyxDataValue;
  begin
    LResult := GClient.Tool(ATool, AArgs);

    if LResult.Field('isError').AsBoolean then
    begin
      Save('lifecycle-refusal.json', LResult.ToJSON);
      raise Exception.Create('Isolated semantic lifecycle refused');
    end;
    Result := LResult.Field('structuredContent');
  end;

  procedure ParentExact;
  var
    LOldReview: TNyxReviewRef;
    LOldWorkspace: TNyxWorkspaceRef;
    LOldRevision: Integer;
    LSnapshot: TNyxDataValue;
  begin
    LOldReview := GReview;
    LOldWorkspace := GWorkspace;
    LOldRevision := GRevision;
    GReview := NyxActiveWorkspace;
    GWorkspace := NyxPrimaryWorkspace;
    try
      LSnapshot := Session;
      GRevision := LSnapshot.Field('revision').AsInteger;
      Check((GRevision = LParent.Field('revision').AsInteger) and
        (LSnapshot.Field('selection').AsText = LParent.Field('selection').AsText) and
        (LSnapshot.Field('view').AsText = LParent.Field('view').AsText) and
        (LSnapshot.Field('canUndo').AsBoolean = LParent.Field('canUndo').AsBoolean) and
        (LSnapshot.Field('canRedo').AsBoolean = LParent.Field('canRedo').AsBoolean) and
        (LSnapshot.Field('pendingDraft').AsBoolean = LParent.Field('pendingDraft').AsBoolean) and
        (Source = LParentSource), 'review/project work leaves the isolated parent exact');
    finally
      GReview := LOldReview;
      GWorkspace := LOldWorkspace;
      GRevision := LOldRevision;
    end;
  end;

  procedure ObserveAccepted(const AExpected: TNyxText);
  begin

    if GWorkspace.ID <> '' then
    begin
      AwaitSource(AExpected);
      BrowserSource(AExpected);
      Check(GStudio.Session.Document.Find('featured-1') <> nil,
        'ordinary native editor owns the accepted reusable instance');
    end;
  end;

  { Each semantic group uses the real constructor, preserves exact retries, and
    has exactly one paired Undo/Redo command. No screenshot chooses design edits. }
  procedure Apply(const AOperations: TNyxText);
  var
    LParts: TNyxDataValue;
    LHistory: TNyxDataValue;
  begin
    LBeforeSource := Source;
    LArgs := NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)),
      NyxField('operations', TNyxDataValue.ParseJSON(AOperations))]);
    LReceipt := Call('nyx_transaction', LArgs);
    Check(LReceipt.Field('publication').Field('state').AsText = 'pending',
      'accepted seed retains executed-source compiler authority');
    Check(Call('nyx_transaction', LArgs).ToJSON = LReceipt.ToJSON,
      'same pending semantic group retains its original receipt');
    WaitConstruction(LReceipt);
    Check(Call('nyx_transaction', LArgs).ToJSON = LReceipt.ToJSON,
      'same completed semantic group retains its original receipt');
    LAfterSource := Source;
    Check((LAfterSource <> LBeforeSource) and
      (Pos('class function TNotebookCards.Caption', LAfterSource) > 0) and
      (Pos('.Padding(3 * 4)', LAfterSource) > 0),
      'semantic reuse retains handwritten helpers and computed configuration');
    LParts := Call('nyx_node', NyxObject([
      NyxField('id', NyxData('featured-1')), NyxField('parts', NyxData(True)),
      NyxField('partLimit', NyxData(2)), NyxField('limit', NyxData(1))]));
    Check((LParts.Field('parts').Field('total').AsInteger > 0) and
      (LParts.Field('parts').Field('items').Count <= 2),
      'named-part context stays bounded after compiler publication');
    ObserveAccepted(LAfterSource);
    LHistory := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)),
      NyxField('direction', NyxData('undo'))]));
    GRevision := LHistory.Field('revision').AsInteger;
    Check(Source = LBeforeSource, 'one paired Undo restores the exact previous Pascal');

    if GWorkspace.ID <> '' then
    begin
      AwaitSource(LBeforeSource);
      BrowserSource(LBeforeSource);
    end;
    LHistory := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)),
      NyxField('direction', NyxData('redo'))]));
    GRevision := LHistory.Field('revision').AsInteger;
    Check(Source = LAfterSource, 'one paired Redo restores the exact grouped Pascal');
    ObserveAccepted(LAfterSource);
    ParentExact;
  end;

  { Keep the optional PNG adapter failure visible. The public semantic preview
    URL remains an independent rendering contract: real-clock physical viewing
    qualifies that fallback, never the failed MCP PNG response. }
  procedure Preview;
  var
    LArguments: TNyxDataValue;
    LFailure: TNyxDataValue;
    LPreview: TNyxDataValue;
    LPreviewBrowser: TNyxBrowserPipe;
    LStartedAt: QWord;
  begin
    LArguments := NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('view', NyxData('notebook-1')),
      NyxField('width', NyxData(900)), NyxField('height', NyxData(600)),
      NyxField('capture', NyxData(True))]);
    LReply := GClient.Tool('nyx_preview', NyxWithReview(LArguments, GReview));
    Save('review-preview-receipt.json', LReply.ToJSON);

    if not LReply.Field('isError').AsBoolean then
    begin
      Check(LReply.Field('content').Count = 3,
        'selective semantic review preview returns a rendered image');
      Exit;
    end;
    LFailure := TNyxDataValue.ParseJSON(LReply.Field('content').Item(0).Field('text').AsText);
    Check(LFailure.Field('message').AsText = 'Preview renderer exceeded its 20-second budget',
      'the optional PNG refusal is the recorded bounded renderer timeout');
    LPreview := Call('nyx_preview', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('view', NyxData('notebook-1')),
      NyxField('width', NyxData(900)), NyxField('height', NyxData(600)),
      NyxField('capture', NyxData(False))]));
    Check(Pos(AOrigin + '/', LPreview.Field('url').AsText) = 1,
      'semantic preview URL belongs to the explicitly owned local service');
    LPreviewBrowser := TNyxBrowserPipe.Create(LPreview.Field('url').AsText,
      GEvidence + 'review-preview', 900, 600);
    try
      LStartedAt := GetTickCount64;
      repeat

        if LPreviewBrowser.Attribute('data-nyx-preview-ready') = 'true' then
        begin
          Break;
        end;
        Application.ProcessMessages;
        Sleep(50);
      until GetTickCount64 - LStartedAt >= 30000;
      Check((LPreviewBrowser.Attribute('data-nyx-preview-ready') = 'true') and
        (LPreviewBrowser.Attribute('data-nyx-preview-revision') = IntToStr(GRevision)) and
        (LPreviewBrowser.Attribute('data-nyx-preview-width') = '900') and
        (LPreviewBrowser.Attribute('data-nyx-preview-height') = '600'),
        'real-clock semantic URL preview renders its exact admitted revision and dimensions');
      LPreviewBrowser.Capture('selective-review');
    finally
      LPreviewBrowser.Free;
    end;
  end;

begin
  Check((GWorkspace.ID = '') and (GReview.ID = ''),
    'reusable qualification begins only at its explicitly owned primary');
  LParentSource := Read(AFixturePath);
  Check(DecodeNyxProject(Read(IncludeTrailingPathDelimiter(ExtractFileDir(
    ExtractFileDir(ExpandFileName(ParamStr(2))))) + 'initial-project.nyxpair')).Source =
    LParentSource, 'the isolated runtime owns the exact seeded fixture before physical startup');
  Check(GClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools').Count = 23,
    'authenticated fresh discovery retains twenty-three tools');
  LBrowser := TNyxBrowserPipe.Create(AOrigin + '/', GEvidence + 'browser', 1240, 900);
  try
    BrowserFace('[data-node="action-code"]');
    { Give the normal asynchronous handshake time to expose conflict controls;
      accepting shared work is safe only on this explicitly isolated host. }
    for LIndex := 1 to 30 do
    begin
      Application.ProcessMessages;
      Sleep(100);
    end;
    { Rich-source startup is deliberately operator initiated. The maintained
      public recovery panel executes the actual browser compiler; MCP refuses
      shared editing until publication. This physical startup is not a semantic
      design edit and does not substitute a privileged checkpoint injection. }
    LRecoverySelector := '[data-node="' +
      NyxOperationActionID(NyxStudioRuntimeRecoveryID, noaStart) + '"]';

    if LBrowser.Exists(LRecoverySelector) then
    begin
      LBrowser.Click(LRecoverySelector);
    end;
    LStarted := GetTickCount64;
    repeat
      LReply := GClient.Tool('nyx_session', NyxObject([]));

      if not LReply.Field('isError').AsBoolean then
      begin
        Break;
      end;
      Save('startup-waiting.json', LReply.ToJSON);
      Application.ProcessMessages;
      Sleep(100);
    until GetTickCount64 - LStarted >= 180000;
    Check(not LReply.Field('isError').AsBoolean,
      'ordinary browser startup publishes the exact rich-source runtime before MCP editing');
    LParent := Session;
    GRevision := LParent.Field('revision').AsInteger;
    Check((Source = LParentSource) and
      (Pos('class function TNotebookCards.Caption', LParentSource) > 0) and
      (LParent.Field('pages').AsInteger = 2),
      'owned primary is the exact executed handwritten qualification fixture');

    if LBrowser.Exists('[data-node="action-agent-accept"]') then
    begin
      LBrowser.Click('[data-node="action-agent-accept"]');
    end;
    LBrowser.Click('[data-node="action-code"]');
    BrowserSource(LParentSource);
    for LStage := 0 to 1 do
    begin
      GReview := NyxActiveWorkspace;
      GWorkspace := NyxPrimaryWorkspace;

      if LStage = 0 then
      begin
        LReceipt := Lifecycle('nyx_reviews', NyxObject([
          NyxField('mode', NyxData('create')), NyxField('base', NyxData('accepted')),
          NyxField('label', NyxData(CReviewLabel)), NyxField('operationId', NyxData(Operation)),
          NyxField('expectedRevision', LParent.Field('revision'))]));
        GReview := NyxReview(LReceipt.Field('review').AsText);
        LBrowser.Click('[data-node="action-agents"]');
        BrowserFace('[data-node="studio-agent-review-label-0"]');
        Check(Pos(CReviewLabel, LBrowser.ElementHTML(
          '[data-node="studio-agent-review-label-0"]')) > 0,
          'ordinary Agents panel visibly exposes the independent review');
      end
      else
      begin
        LReceipt := Lifecycle('nyx_workspaces', NyxObject([
          NyxField('mode', NyxData('create')), NyxField('base', NyxData('accepted')),
          NyxField('label', NyxData(CProjectLabel)), NyxField('operationId', NyxData(Operation)),
          NyxField('expectedRevision', LParent.Field('revision'))]));
        GWorkspace := NyxWorkspace(LReceipt.Field('workspace').AsText);
        GStudio := TNyxNativeStudio.Create(GForm, GEvidence + 'native-projects', nil,
          NewNyxSharedNativeSourceCompilerFactory(AOrigin));
        GStudio.Run;
        GStudio.ConnectService(AOrigin, GWorkspace);
        AwaitSource(LParentSource);
        Click('action-code');
        BrowserFace('[data-node="studio-agent-workspace-jump-' + GWorkspace.ID + '"]');
        LBrowser.Click('[data-node="studio-agent-workspace-jump-' + GWorkspace.ID + '"]');
        { Each project retains its own pane configuration. A successful jump
          need not inherit the previous project's open source pane. Observe the
          explicit new context first, then request its ordinary Pascal pane. }
        BrowserFace('[data-node="studio-agent-workspace-current-' + GWorkspace.ID + '"]');

        if not LBrowser.Exists('[data-node="studio-code"]') then
        begin
          LBrowser.Click('[data-node="action-code"]');
        end;
        BrowserSource(LParentSource);
      end;
      LState := Session;
      GRevision := LState.Field('revision').AsInteger;
      Check((GRevision = 1) and not LState.Field('pendingDraft').AsBoolean and
        not LState.Field('canUndo').AsBoolean and not LState.Field('canRedo').AsBoolean and
        (Source = LParentSource), 'accepted semantic owners retain exact source with fresh history');
      Apply('[{"op":"derive","source":"reusable-note","id":"featured-note",' +
        '"identities":{"note-caption":"featured-caption"}},' +
        '{"op":"instance","component":"featured-note","id":"featured-1","parent":"notebook-1"},' +
        '{"op":"override","instance":"featured-1","id":"featured-title","path":"caption","mode":"properties"},' +
        '{"op":"update","id":"featured-title","properties":{"text":"Featured notebook"}}]');
      Apply('[{"op":"inherit","instance":"featured-1","id":"featured-title","path":"caption"}]');
      Apply('[{"op":"derive","source":"heading-1","id":"page-heading","identities":{}},' +
        '{"op":"delete","id":"heading-1"},' +
        '{"op":"instance","component":"page-heading","id":"heading-1","parent":"notebook-1","index":0}]');

      if LStage = 0 then
      begin
        { Rendering validates this already understood semantic result selectively.
          Reviews retain read-only previews; no full-editor mounting is claimed. }
        Preview;
        Check(Lifecycle('nyx_reviews', NyxObject([
          NyxField('mode', NyxData('discard')), NyxField('review', NyxData(GReview.ID)),
          NyxField('expectedRevision', NyxData(GRevision)),
          NyxField('operationId', NyxData(Operation))])).Field('disposed').AsBoolean,
          'the owning transport retires its reusable review');
        GReview := NyxActiveWorkspace;
      end
      else
      begin
        Check((GStudio.Session.Document.Find('heading-1').Kind = 'component') and
          (GStudio.Session.Document.FindComponent('page-heading') <> nil),
          'ordinary native editor observes specialized extraction at the same identity');
        Capture('native-reusable-project.png');
        LBrowser.Capture('desktop-reusable-project');
        LBrowser.Resize(390, 780);
        BrowserSource(Source);
        LBrowser.Capture('compact-reusable-project');
        Check(LBrowser.RuntimeError = '', 'ordinary browser observer reports no runtime exception');
        Save('accepted-source.pas', Source);
        Save('accepted-design.nyx', TNyxCodec.Encode(GStudio.Session.Document));
      end;
    end;
    ParentExact;
  finally
    LBrowser.Free;
  end;
end;
