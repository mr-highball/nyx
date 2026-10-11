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
procedure RunReusables(const AOrigin, AFixturePath: TNyxText;
  AKeepReview: Boolean = False);
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
  LKept: TNyxWorkspaceRef;
  LKeepArguments: TNyxDataValue;
  LKeepReceipt: TNyxDataValue;
  LKeptSource: TNyxText;
  LSeedSource: TNyxText;

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

  { General source editing is semantic, including coordinated class counterparts
    and a computed view change. Ordinary browser/LCL observers must receive the
    exact pair and history; application builds then qualify both compilers. }
  procedure SourceRanges;
  const
    CCaption: TNyxText = 'A prepared notebook 🚀 with letter 𐐷.';
  var
    LPriorSource: TNyxText;
    LPriorDesign: TNyxText;
    LChangedSource: TNyxText;
    LChangedDesign: TNyxText;
    LArguments: TNyxDataValue;
    LReceipt: TNyxDataValue;
    LHistory: TNyxDataValue;
    LOutputs: TNyxDataValue;
    LBuild: TNyxDataValue;

    function Range(const AExpected, AReplacement: TNyxText): TNyxDataValue;
    var
      LPosition: Integer;
      LOffset: Integer;
      LWindow: TNyxDataValue;
    begin
      LPosition := Pos(AExpected, LPriorSource);
      Check(LPosition > 0, 'owned ordinary project retains exact source-edit context');
      LOffset := NyxTextScalarCount(Copy(LPriorSource, 1, LPosition - 1));
      LWindow := Call('nyx_pascal', NyxObject([
        NyxField('mode', NyxData('unit')), NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('offset', NyxData(LOffset)),
        NyxField('count', NyxData(NyxTextScalarCount(AExpected)))]));
      Check(LWindow.Field('text').AsText = AExpected,
        'bounded semantic windows supply exact source expectations');
      Result := NyxObject([NyxField('offset', NyxData(LOffset)),
        NyxField('expected', LWindow.Field('text')),
        NyxField('replacement', NyxData(AReplacement))]);
    end;

  begin
    LPriorSource := Source;
    LPriorDesign := TNyxCodec.Encode(GStudio.Session.Document);
    LArguments := NyxObject([NyxField('mode', NyxData('edit-unit')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)), NyxField('changes', NyxArray([
        Range('class function Caption: TNyxText;',
          'class function Caption(const APrefix: TNyxText = ''''): TNyxText;'),
        Range('class function TNotebookCards.Caption: TNyxText;',
          'class function TNotebookCards.Caption(const APrefix: TNyxText): TNyxText;'),
        Range('  Result := CQualificationText;',
          '  Result := APrefix + ''A prepared notebook 🚀 with letter 𐐷.'';'),
        Range('    LDocument.Title := ''Handwritten notebook'';',
          '    LDocument.Title := TNotebookCards.Caption(''Notes: '');')]))]);
    LReceipt := Call('nyx_pascal', LArguments);
    Check((LReceipt.Field('operation').AsText = 'source-edit') and
      (LReceipt.Field('sourceEdit').Field('changes').AsInteger = 4),
      'authenticated whole-unit ranges share one bounded constructor receipt');
    Check(Call('nyx_pascal', LArguments).ToJSON = LReceipt.ToJSON,
      'ordinary source edit retry retains its one compiler job');
    WaitConstruction(LReceipt);
    Check(Call('nyx_pascal', LArguments).ToJSON = LReceipt.ToJSON,
      'published source edit retry remains exact at the later revision');
    LChangedSource := Source;
    ObserveAccepted(LChangedSource);
    LChangedDesign := TNyxCodec.Encode(GStudio.Session.Document);
    Check((GStudio.Session.Document.Title = 'Notes: ' + CCaption) and
      (GStudio.Session.Document.Find('note-caption').Prop('text') = CCaption),
      'ordinary native Studio receives actual computed class/helper meaning');
    Check((LChangedSource <> LPriorSource) and (LChangedDesign <> LPriorDesign) and
      (Pos('class function Caption(const APrefix:', LChangedSource) > 0),
      'coordinated handwritten class and view changes remain exact in both observers');
    LOutputs := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))]));
    LBuild := BuildApplication('browser', LOutputs.Field('outputID').AsText);
    Check((LBuild.Field('revision').AsInteger = GRevision) and
      (LBuild.Field('sourceFingerprint').AsText = NyxBuildFingerprint(LChangedSource)),
      'browser application consumes the exact handwritten source-edit revision');
    LBuild := BuildApplication('lcl', LOutputs.Field('outputID').AsText);
    Check((LBuild.Field('revision').AsInteger = GRevision) and
      (LBuild.Field('sourceFingerprint').AsText = NyxBuildFingerprint(LChangedSource)),
      'LCL application consumes the same exact handwritten source-edit revision');
    Save('source-range-accepted.pas', LChangedSource);
    Save('source-range-accepted-design.nyx', LChangedDesign);
    LHistory := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)), NyxField('direction', NyxData('undo'))]));
    GRevision := LHistory.Field('revision').AsInteger;
    ObserveAccepted(LPriorSource);
    Check(TNyxCodec.Encode(GStudio.Session.Document) = LPriorDesign,
      'one ordinary Undo restores complete source and computed meaning in both observers');
    LHistory := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)), NyxField('direction', NyxData('redo'))]));
    GRevision := LHistory.Field('revision').AsInteger;
    ObserveAccepted(LChangedSource);
    Check(TNyxCodec.Encode(GStudio.Session.Document) = LChangedDesign,
      'one ordinary Redo restores complete source and computed meaning in both observers');
    LHistory := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)), NyxField('direction', NyxData('undo'))]));
    GRevision := LHistory.Field('revision').AsInteger;
    ObserveAccepted(LPriorSource);
    ParentExact;
  end;

  { Semantic selection supplies an exact target. Only these two real mounted
    adapter actions exercise ordinary editor input; they do not compose a demo
    through browser automation. Both share the production source/history gate. }
  procedure OrdinaryRootMove(const AID: TNyxText; AInBrowser: Boolean);
  var
    LPriorSource: TNyxText;
    LPriorDesign: TNyxText;
    LMovedSource: TNyxText;
    LMovedDesign: TNyxText;
    LSelected: TNyxDataValue;
    LHistory: TNyxDataValue;
    LStarted: QWord;
    LInputBox: TNyxBrowserBox;
  begin
    LSelected := Call('nyx_select', NyxObject([
      NyxField('id', NyxData(AID)), NyxField('activate', NyxData(True)),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation))]));
    GRevision := LSelected.Field('revision').AsInteger;
    LStarted := GetTickCount64;
    repeat
      Pump;

      if (GStudio.Session.SelectedID = AID) and
        (Pos(AID, LBrowser.ElementHTML('[data-node="selected-label"]')) > 0) then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 180000 then
      begin
        raise Exception.Create('Ordinary root selection exceeded its observer budget');
      end;
      Sleep(30);
    until False;
    LPriorSource := Source;
    LPriorDesign := TNyxCodec.Encode(GStudio.Session.Document);

    if AInBrowser then
    begin
      { Retain the actual pre-input geometry/face, so a clipped/covered target
        cannot be confused with a submitted compiler operation. Captures only
        observe this already semantically selected owned root. }
      LInputBox := LBrowser.Bounds('[data-node="action-down"]');
      Save('root-input-before.json', NyxObject([
        NyxField('selection', NyxData(AID)), NyxField('revision', NyxData(GRevision)),
        NyxField('left', NyxData(LInputBox.Left)), NyxField('top', NyxData(LInputBox.Top)),
        NyxField('width', NyxData(LInputBox.Width)), NyxField('height', NyxData(LInputBox.Height))]).ToJSON);
      LBrowser.Capture('root-input-before');
      LBrowser.Click('[data-node="action-down"]');
    end
    else
    begin
      Click('action-down');
    end;
    LStarted := GetTickCount64;
    repeat
      Pump;
      LSelected := Session;

      if LSelected.Field('revision').AsInteger > GRevision then
      begin
        Check(LSelected.Field('revision').AsInteger = GRevision + 1,
          'one ordinary root input publishes one exact next revision');
        GRevision := LSelected.Field('revision').AsInteger;
        Break;
      end;

      if GetTickCount64 - LStarted > 180000 then
      begin
        Save('root-input-source-refusal.pas', GStudio.Session.Source);
        Save('root-input-status-refusal.txt', GStudio.Agents.Status);
        Save('root-input-browser-refusal.html',
          LBrowser.ElementHTML('[data-node="studio-shell"]'));
        Save('root-input-selection-refusal.json', NyxObject([
          NyxField('selection', NyxData(GStudio.Session.SelectedID)),
          NyxField('view', NyxData(GStudio.Session.ActiveViewID)),
          NyxField('session', LSelected)]).ToJSON);
        LBrowser.Capture('root-input-refusal');
        Capture('native-root-input-refusal.png');
        raise Exception.Create('Ordinary root input exceeded its publication budget');
      end;
      Sleep(30);
    until False;
    LMovedSource := Source;
    ObserveAccepted(LMovedSource);
    LMovedDesign := TNyxCodec.Encode(GStudio.Session.Document);
    Check((LMovedSource <> LPriorSource) and (LMovedDesign <> LPriorDesign) and
      (GStudio.Session.SelectedID = AID) and (GStudio.Session.ActiveViewID = AID),
      'ordinary root movement preserves exact active identities while changing source/order');

    if AInBrowser then
    begin
      Check(GStudio.Session.Document.Pages[1].ID = AID,
        'actual browser Move down moves a page within its partition');
    end
    else
    begin
      Check(GStudio.Session.Document.Components[1].ID = AID,
        'actual mounted LCL Move down moves a reusable card within its partition');
    end;
    LHistory := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation)),
      NyxField('direction', NyxData('undo'))]));
    GRevision := LHistory.Field('revision').AsInteger;
    ObserveAccepted(LPriorSource);
    Check((Source = LPriorSource) and
      (TNyxCodec.Encode(GStudio.Session.Document) = LPriorDesign),
      'one ordinary root Undo restores the entire exact source/design pair');
    LHistory := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation)),
      NyxField('direction', NyxData('redo'))]));
    GRevision := LHistory.Field('revision').AsInteger;
    ObserveAccepted(LMovedSource);
    Check((Source = LMovedSource) and
      (TNyxCodec.Encode(GStudio.Session.Document) = LMovedDesign),
      'one ordinary root Redo restores the entire admitted moved pair');
    LHistory := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation)),
      NyxField('direction', NyxData('undo'))]));
    GRevision := LHistory.Field('revision').AsInteger;
    ObserveAccepted(LPriorSource);
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
  LKept := NyxPrimaryWorkspace;
  LKeptSource := '';
  LSeedSource := '';
  Check((GWorkspace.ID = '') and (GReview.ID = ''),
    'reusable qualification begins only at its explicitly owned primary');
  LParentSource := Read(AFixturePath);
  Check(DecodeNyxProject(Read(IncludeTrailingPathDelimiter(ExtractFileDir(
    ExtractFileDir(ExpandFileName(ParamStr(2))))) + 'initial-project.nyxpair')).Source =
    LParentSource, 'the isolated runtime owns the exact seeded fixture before physical startup');
  LReply := GClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools');
  Check(LReply.Count = 23,
    'authenticated fresh discovery retains twenty-three tools');

  if AKeepReview then
  begin
    for LIndex := 0 to LReply.Count - 1 do
    begin

      if LReply.Item(LIndex).Field('name').AsText = 'nyx_workspaces' then
      begin
        LState := LReply.Item(LIndex).Field('inputSchema').Field('oneOf');
        Check((LState.Count = 4) and
          (LState.Item(3).Field('properties').Field('base').Field('const').AsText = 'accepted') and
          (LState.Item(3).Field('properties').Field('sourceReview').Field('maxLength').AsInteger = 120) and
          not LState.Item(3).Field('additionalProperties').AsBoolean,
          'authenticated discovery advertises the strict accepted-review handoff branch');
      end;
    end;
  end;
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
        LSeedSource := LParentSource;

        if AKeepReview then
        begin
          GWorkspace := LKept;
          LSeedSource := LKeptSource;
        end
        else
        begin
          LReceipt := Lifecycle('nyx_workspaces', NyxObject([
            NyxField('mode', NyxData('create')), NyxField('base', NyxData('accepted')),
            NyxField('label', NyxData(CProjectLabel)), NyxField('operationId', NyxData(Operation)),
            NyxField('expectedRevision', LParent.Field('revision'))]));
          GWorkspace := NyxWorkspace(LReceipt.Field('workspace').AsText);
        end;
        GStudio := TNyxNativeStudio.Create(GForm, GEvidence + 'native-projects', nil,
          NewNyxSharedNativeSourceCompilerFactory(AOrigin));
        GStudio.Run;
        GStudio.ConnectService(AOrigin, GWorkspace);
        AwaitSource(LSeedSource);
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
        BrowserSource(LSeedSource);
      end;
      LState := Session;
      GRevision := LState.Field('revision').AsInteger;

      if LStage = 0 then
      begin
        LSeedSource := LParentSource;
      end;
      Check((GRevision = 1) and not LState.Field('pendingDraft').AsBoolean and
        not LState.Field('canUndo').AsBoolean and not LState.Field('canRedo').AsBoolean and
        (Source = LSeedSource), 'accepted semantic owners retain exact source with fresh history');

      if AKeepReview and (LStage = 1) then
      begin
        Apply('[{"op":"update","id":"note-1","properties":{"text":"A lasting edited notebook"}}]');
      end
      else
      begin
        Apply('[{"op":"derive","source":"reusable-note","id":"featured-note",' +
          '"identities":{"note-caption":"featured-caption"}},' +
          '{"op":"instance","component":"featured-note","id":"featured-1","parent":"notebook-1"},' +
          '{"op":"override","instance":"featured-1","id":"featured-title","path":"caption","mode":"properties"},' +
          '{"op":"update","id":"featured-title","properties":{"text":"Featured notebook"}}]');

        if not AKeepReview then
        begin
          Apply(NyxDesignChanges(NyxRootOrderPatch([
            NyxMoveRoot(NyxPageRoot('notebook-2'), 0),
            NyxMoveRoot(NyxReusableRoot('featured-note'), 0)])).ToData.ToJSON);
          LState := Call('nyx_outline', NyxObject([
            NyxField('scope', NyxData('pages')), NyxField('limit', NyxData(1))]));
          Check(LState.Field('items').Item(0).Field('id').AsText = 'notebook-2',
            'bounded semantic outline reports the reordered first page');
          LState := Call('nyx_outline', NyxObject([
            NyxField('scope', NyxData('components')), NyxField('limit', NyxData(1))]));
          Check(LState.Field('items').Item(0).Field('id').AsText = 'featured-note',
            'bounded semantic outline reports the reordered reusable definition');
        end;
        Apply('[{"op":"inherit","instance":"featured-1","id":"featured-title","path":"caption"}]');
        Apply('[{"op":"derive","source":"heading-1","id":"page-heading","identities":{}},' +
          '{"op":"delete","id":"heading-1"},' +
          '{"op":"instance","component":"page-heading","id":"heading-1","parent":"notebook-1","index":0}]');
      end;

      if LStage = 0 then
      begin
        { Rendering validates this already understood semantic result selectively.
          Reviews retain read-only previews; no full-editor mounting is claimed. }
        Preview;

        if AKeepReview then
        begin
          LKeptSource := Source;
          LKeepArguments := NyxObject([
            NyxField('mode', NyxData('create')), NyxField('base', NyxData('accepted')),
            NyxField('sourceReview', NyxData(GReview.ID)), NyxField('expectedRevision', NyxData(GRevision)),
            NyxField('operationId', NyxData(Operation)), NyxField('label', NyxData(CProjectLabel))]);
          LKeepReceipt := Lifecycle('nyx_workspaces', LKeepArguments);
          LKept := NyxWorkspace(LKeepReceipt.Field('workspace').AsText);
          Check((LKeepReceipt.Field('sourceReview').AsText = GReview.ID) and
            (LKeepReceipt.Field('sourceRevision').AsInteger = GRevision) and
            not LKeepReceipt.Field('pendingDraftExcluded').AsBoolean,
            'authenticated handoff identifies only its exact accepted review seed');
          Check(Lifecycle('nyx_workspaces', LKeepArguments).ToJSON = LKeepReceipt.ToJSON,
            'authenticated handoff retry returns its exact ordinary-project receipt');
          Check(Source = LKeptSource, 'handoff leaves the complete accepted review source exact');
          ParentExact;
        end;
        Check(Lifecycle('nyx_reviews', NyxObject([
          NyxField('mode', NyxData('discard')), NyxField('review', NyxData(GReview.ID)),
          NyxField('expectedRevision', NyxData(GRevision)),
          NyxField('operationId', NyxData(Operation))])).Field('disposed').AsBoolean,
          'the owning transport retires its reusable review');

        if AKeepReview then
        begin
          Check(Lifecycle('nyx_workspaces', LKeepArguments).ToJSON = LKeepReceipt.ToJSON,
            'authenticated retry survives disposal without recreating a project');
        end;
        GReview := NyxActiveWorkspace;
      end
      else
      begin
        if not AKeepReview then
        begin
          OrdinaryRootMove('notebook-2', True);
          OrdinaryRootMove('featured-note', False);
          SourceRanges;
        end;
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

    if AKeepReview then
    begin
      { New authenticated transport verifies ordinary project lifetime. Existing
        LCL/browser editor connections stay mounted; only the creating agent
        retires. No new source or model is injected into an observer. }
      LKeptSource := Source;
      GClient.Close;
      GClient.Free;
      GClient := nil;
      GClient := TNyxMCPTestClient.Create(ParamStr(2), 'Scooty lasting project observer');
      Check((Source = LKeptSource) and
        (Session.Field('revision').AsInteger = GRevision),
        'handed-off ordinary project survives its creating transport disconnect');
      AwaitSource(LKeptSource);
      BrowserSource(LKeptSource);
      ParentExact;
      LBrowser.Capture('lasting-review-project');
    end;
  finally
    LBrowser.Free;
  end;
end;
