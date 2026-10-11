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

{ A real semantic producer changes this fixture's owning backend while ordinary
  native Studio observes through its replaceable transport. No listener or LAN
  project is touched. Compiler execution and editor controls are real; this seam
  does not establish authenticated HTTP or application callback dispatch. }
procedure CallbackControls;
const
  COwner: TNyxText = 'native-callback-control-owner';
  CActor: TNyxText = 'Callback workshop';
  CFirst: TNyxText = 'begin' + #10 +
    '  { A crafted response 🚀 𐐷. }' + #10 + '  AExecution.Cancel;' + #10 + 'end;';
  CSecond: TNyxText = 'begin' + #10 +
    '  { Failure belongs to this registration, not its siblings. }' + #10 +
    '  raise Exception.Create(''An authored failure 🚀 𐐷.'');' + #10 + 'end;';
var
  LInitial: TNyxText;
  LAdded: TNyxText;
  LAuthored: TNyxText;
  LMoved: TNyxText;
  LBefore: TNyxText;
  LInput: TMemo;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LChanges: TNyxDataValue;
  LReview: TNyxDataValue;
  LWindow: TNyxDataValue;
  LFirst: TNyxCallbackRef;
  LSecond: TNyxCallbackRef;
  LFirstHandler: TNyxHandlerRef;
  LSecondHandler: TNyxHandlerRef;
  LFirstBody: TNyxHandlerSource;
  LSecondBody: TNyxHandlerSource;
  LHistoryReplies: Integer;
  LRejected: Boolean;
  LJobID: TNyxText;
  LNavigation: Integer;
  {$ifdef WINDOWS}
  LPosition: PtrInt;
  LVertical: SmallInt;
  {$endif}

  function MemoText: TNyxText;
  begin
    { Win32 presents line breaks as CRLF. The adapter admits LF text at its
      input boundary. Compare that exact boundary representation, retaining all
      other Unicode scalars; the independent project oracle stays byte-exact. }
    Result := NyxDecodeUTF8(NyxEncodeUTF8(TNyxText(StringReplace(
      TNyxText(LInput.Text), #13#10, #10, [rfReplaceAll]))));
  end;

  procedure SaveActivity(const AName: TNyxText; const AItem: TNyxDataValue);
  var
    LStream: TFileStream;
    LBytes: TNyxBytes;
  begin
    { These bounded public rows came from the real compiler/semantic producer.
      Browser replay can qualify presentation independently; no checkpoint,
      source, transport authority or private grant is exported in this input. }
    LBytes := NyxEncodeUTF8(AItem.ToJSON);
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(4)) +
      AName + '.json', fmCreate);
    try
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
  end;

  function Revision: Integer;
  begin
    Result := GSharedEngine.InvokeTool('nyx_session', COwner, CActor,
      NyxObject([])).Field('revision').AsInteger;
  end;

  procedure SaveApplication(const AName: TNyxText);
  var
    LDirectory: TNyxText;
    LStream: TFileStream;
    LBytes: TNyxBytes;
  begin
    { Export the accepted application without regenerating, renaming or replacing
      its implementations. A separate runtime consumer compiles these exact
      bytes and clicks the target control on both adapters. The descriptor JSON
      is an independent whole-document oracle, not a substitute constructor. }
    LDirectory := IncludeTrailingPathDelimiter(ParamStr(4)) + AName;
    ForceDirectories(LDirectory);
    LBytes := NyxEncodeUTF8(GStudio.Session.Source);
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(LDirectory) +
      'nyx.projection.fixture.pas', fmCreate);
    try
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
    LBytes := NyxEncodeUTF8(GStudio.Session.ProjectSnapshot.Design);
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(LDirectory) +
      'design.json', fmCreate);
    try
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
  end;

  function Args(const AID: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(Revision)),
      NyxField('operationId', NyxData(AID)), NyxField('changes', AChanges)]);
  end;

  procedure Ready(AHistory: Integer = 0);
  var
    LStarted: QWord;
    LRemote: TNyxDataValue;
  begin
    LStarted := GetTickCount64;
    repeat
      Pump;

      if GStudio.Agents.Conflict then
      begin
        raise Exception.Create('Callback observer conflict: ' + GStudio.Agents.Status);
      end;
      LRemote := GSharedEngine.EditorExchange(GSharedExchange.Token,
        NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));

      if (GSharedExchange.HistoryReplies >= AHistory) and
        not Assigned(GSharedExchange.Reply) and not GStudio.Agents.Busy and
        not GStudio.SourceBusy and not GStudio.PresentationPending and
        (LRemote.Field('session').Field('selection').AsText = GStudio.Session.SelectedID) and
        (LRemote.Field('session').Field('view').AsText = GStudio.Session.ActiveViewID) and
        (LRemote.Field('project').AsText = EncodeNyxProject(GStudio.Session.ProjectSnapshot)) then
      begin
        Exit;
      end;
      Sleep(1);
    until GetTickCount64 - LStarted > 60000;
    raise Exception.Create('Callback observer did not acknowledge the exact pair');
  end;

  procedure AwaitPublication(const AOperation, AExpected: TNyxText);
  var
    LStarted: QWord;
    LSeen: Boolean;
    LItems: TNyxDataValue;
    LIndex: Integer;
    LTitle: TNyxNode;
  begin
    Check(LReceipt.Field('operation').AsText = AOperation,
      'semantic callback receipt names its exact operation');
    LJobID := LReceipt.Field('job').AsText;
    LStarted := GetTickCount64;
    LSeen := False;
    repeat
      Pump;
      LStatus := GSharedEngine.InvokeBuild(COwner, CActor, NyxObject([
        NyxField('mode', NyxData('status')), NyxField('job', NyxData(LJobID)),
        NyxField('limit', NyxData(10))]));
      LItems := GStudio.Agents.BuildJobs.Field('items');
      for LIndex := 0 to LItems.Count - 1 do
      begin

        if LItems.Item(LIndex).Field('job').AsText = LJobID then
        begin
          LTitle := GStudio.ShellView.Root.Find('studio-build-' +
            TNyxText(IntToStr(LIndex)) + '-title');

          if (LTitle <> nil) and not LSeen then
          begin
            Check((Pos('Callbacks', LTitle.Prop('text')) > 0) or
              (Pos('Callback implementations', LTitle.Prop('text')) > 0),
              'ordinary native job row identifies callback work');
            Check((Pos('Complete', LTitle.Prop('text')) = 0),
              'constructor completion is not presented as applied source');
            LSeen := True;
            SaveActivity(AOperation + '-pending', LItems.Item(LIndex));
          end;
        end;
      end;

      if LStatus.Field('publication').Field('state').AsText <> 'pending' then
      begin
        Break;
      end;
      Sleep(1);
    until GetTickCount64 - LStarted > 90000;
    Check(LStatus.Field('publication').Field('state').AsText = AExpected,
      'actual callback compiler reaches the expected publication result');
    Check(LSeen, 'ordinary native observer visibly follows each semantic compiler job');
    Ready;
    Check(GStudio.CodeView.InputFor('studio-code') = LInput,
      'semantic observation retains the real Pascal input identity');

    Check(MemoText = GStudio.Session.DraftSource,
      'semantic observation retains exact Pascal text');
    LItems := GSharedEngine.InvokeBuild(COwner, CActor, NyxObject([
      NyxField('mode', NyxData('jobs')), NyxField('filter', NyxData('all'))])).Field('items');
    for LIndex := 0 to LItems.Count - 1 do
    begin

      if LItems.Item(LIndex).Field('job').AsText = LJobID then
      begin
        SaveActivity(AOperation + '-' + AExpected, LItems.Item(LIndex));
        Break;
      end;
    end;
  end;

  procedure History(const AID: TNyxText; const AExpected: TNyxText);
  begin
    Inc(LHistoryReplies);
    Click(AID);
    Ready(LHistoryReplies);
    Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = AExpected,
      'ordinary ' + AID + ' restores the exact semantic paired command');
  end;

begin
  Ready;
  LInitial := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
  LInput := TMemo(GStudio.CodeView.InputFor('studio-code'));
  Check(LInput <> nil, 'callback observer retains its ordinary Pascal memo');
  TControlAccess(GStudio.CanvasView.ControlFor('heading-1')).Click;
  Ready;
  Click('inspector-tab-events');
  Click('action-agents');
  Click('action-builds');
  Ready;
  LHistoryReplies := GSharedExchange.HistoryReplies;
  LChanges := CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxAddCallback('heading-1', NyxCallbackEvent(ntClick)),
    NyxAddCallback('heading-1', NyxCallbackEvent(ntClick)),
    NyxSetCallbackPolicy('heading-1', NyxCallbackEvent(ntClick), neUIQueue)])).ToData;
  LReceipt := GSharedEngine.InvokeTool('nyx_callbacks', COwner, CActor,
    Args('observing-add', LChanges));
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LInitial,
    'enqueue preserves the observing accepted pair');
  AwaitPublication('callbacks', 'committed');
  LAdded := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
  LFirst := NyxCallbackID(LReceipt.Field('proposedCallbacks').Item(0).Field('registration').AsText);
  LSecond := NyxCallbackID(LReceipt.Field('proposedCallbacks').Item(1).Field('registration').AsText);
  LFirstHandler := NyxHandler(LReceipt.Field('proposedCallbacks').Item(0).Field('handler').AsText);
  LSecondHandler := NyxHandler(LReceipt.Field('proposedCallbacks').Item(1).Field('handler').AsText);
  Check(GStudio.ShellView.Root.Find('event-click-count').Prop('text') = '2 registrations',
    'ordinary Events inspector observes both semantic registrations');
  Check(Pos('TODO:', TNyxText(LInput.Text)) > 0,
    'accepted generated TODO implementations appear in the retained memo');
  LFirstBody := ReadNyxHandlerSource(GStudio.Session.Source, LFirstHandler);
  LSecondBody := ReadNyxHandlerSource(GStudio.Session.Source, LSecondHandler);
  LWindow := GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor, NyxObject([
    NyxField('mode', NyxData('inspect')), NyxField('handler', NyxData(LFirstHandler.Name)),
    NyxField('offset', NyxData(0)), NyxField('count', NyxData(2048))]));
  Check((LWindow.Field('revision').AsInteger = Revision) and
    (LWindow.Field('text').AsText = LFirstBody.Code),
    'semantic inspection supplies exact bounded expected implementation');
  LChanges := CaptureNyxHandlerBatch(NewNyxHandlerPatch([
    NyxHandlerEdit(LFirstHandler, LFirstBody.Code, CFirst),
    NyxHandlerEdit(LSecondHandler, LSecondBody.Code, CSecond)])).ToData;
  LReceipt := GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor,
    Args('observing-bodies', LChanges));
  AwaitPublication('handlers', 'committed');
  LAuthored := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
  SaveApplication('application-queued');
  Check((ReadNyxHandlerSource(GStudio.Session.Source, LFirstHandler).Code = CFirst) and
    (ReadNyxHandlerSource(GStudio.Session.Source, LSecondHandler).Code = CSecond),
    'ordinary code pane receives both exact handwritten bodies');
  LRejected := False;
  for LNavigation := 0 to GStudio.Agents.Activity.Count - 1 do
  begin

    if GStudio.Agents.Activity.Item(LNavigation).Field('operation').AsText = 'nyx_pascal' then
    begin
      Check(Pos('nyx_pascal', GStudio.ShellView.Root.Find('studio-agent-activity-' +
        TNyxText(IntToStr(LNavigation))).Prop('text')) > 0,
        'ordinary Agents pane visibly identifies semantic Pascal work');
      LRejected := True;
      Break;
    end;
  end;
  Check(LRejected, 'observing callback activity includes the semantic body producer');
  Click('event-click-callback-0-source');
  Ready;
  LNavigation := ReadNyxHandlerSource(GStudio.Session.Source, LFirstHandler).Line;
  Check(LInput.CaretPos.Y + 1 = LNavigation,
    'ordinary Events source action navigates the actual retained memo');
  {$ifdef WINDOWS}
  { A correct logical caret alone can remain off-screen in a native memo.
    Qualify the actual Win32 client position, independently of model/location
    metadata and font estimates. Other widgetsets retain separate input gates. }
  LPosition := Windows.SendMessageW(LInput.Handle, EM_POSFROMCHAR, LInput.SelStart, 0);
  LVertical := SmallInt((LPosition shr 16) and $ffff);
  Check((LPosition <> -1) and (LVertical >= 0) and (LVertical < LInput.ClientHeight),
    'ordinary native source navigation reveals the handler inside the visible memo');
  {$endif}
  Capture(IncludeTrailingPathDelimiter(ParamStr(4)) + 'semantic-callback-bodies.png');
  History('action-undo', LAdded);
  LReceipt := GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor,
    Args('observing-no-op', CaptureNyxHandlerBatch(NewNyxHandlerPatch([
      NyxHandlerEdit(LFirstHandler, LFirstBody.Code, LFirstBody.Code)])).ToData));
  Check(LReceipt.Field('canRedo').AsBoolean,
    'observed exact body no-op preserves the ordinary Redo command');
  Ready;
  History('action-redo', LAuthored);
  LChanges := CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxMoveCallback('heading-1', NyxCallbackEvent(ntClick), LSecond, 0),
    NyxSetCallbackPolicy('heading-1', NyxCallbackEvent(ntClick), neSequential)])).ToData;
  LReceipt := GSharedEngine.InvokeTool('nyx_callbacks', COwner, CActor,
    Args('observing-move', LChanges));
  AwaitPublication('callbacks', 'committed');
  LMoved := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
  SaveApplication('application-sequential');
  Check(Pos(LSecondHandler.Name,
    GStudio.ShellView.Root.Find('event-click-callback-0-source').Prop('text')) > 0,
    'ordinary Events list observes the exact new registration order');
  Check(TComboBox(GStudio.ShellView.InputFor('event-click-policy')).Text = 'sequential',
    'ordinary Events policy observes the typed semantic choice');
  LChanges := CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxRemoveCallback('heading-1', NyxCallbackEvent(ntClick), LFirst)])).ToData;
  LReview := GSharedEngine.InvokeTool('nyx_callbacks', COwner, CActor, NyxObject([
    NyxField('mode', NyxData('review')), NyxField('expectedRevision', NyxData(Revision)),
    NyxField('changes', LChanges)]));
  Check(Pos('retained', LReview.Field('callbacks').Item(0).Field('warning').AsText) > 0,
    'semantic review warns before exact callback removal');
  LReceipt := GSharedEngine.InvokeTool('nyx_callbacks', COwner, CActor, NyxObject([
    NyxField('mode', NyxData('apply')), NyxField('expectedRevision', NyxData(Revision)),
    NyxField('operationId', NyxData('observing-remove')), NyxField('changes', LChanges),
    NyxField('reviewID', LReview.Field('reviewID'))]));
  AwaitPublication('callbacks', 'committed');
  Check((GStudio.ShellView.Root.Find('event-click-count').Prop('text') = '1 registrations') and
    (ReadNyxHandlerSource(GStudio.Session.Source, LFirstHandler).Code = CFirst),
    'observing removal retains the complete authored implementation');
  Capture(IncludeTrailingPathDelimiter(ParamStr(4)) + 'semantic-callback-removal.png');
  History('action-undo', LMoved);
  History('action-undo', LAuthored);
  LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
  LChanges := CaptureNyxHandlerBatch(NewNyxHandlerPatch([
    NyxHandlerEdit(LFirstHandler, CFirst,
      'begin' + #10 + '  MissingQualificationSymbol;' + #10 + 'end;')])).ToData;
  LReceipt := GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor,
    Args('observing-compiler-refusal', LChanges));
  AwaitPublication('handlers', 'refused');
  Check((EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore) and
    not LStatus.Field('currentSource').AsBoolean,
    'ordinary observer keeps exact work and rejects navigation into failed source');
  LInput.Text := GStudio.Session.Source + #10 + '{ unfinished callback idea';
  Ready;
  LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
  LRejected := False;
  try
    GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor,
      Args('observing-draft-refusal', LChanges));
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Ready;
  Check(LRejected and (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore) and
    (MemoText = GStudio.Session.DraftSource),
    'semantic mutation refuses while the observing real memo retains unfinished work');
  LInput.Text := GStudio.Session.Source;
  Ready;
  History('action-undo', LAdded);
  History('action-undo', LInitial);
  { Restore presentation through real controls so the enclosing property/resource
    journey owns its original closed optional panes and Properties inspector. }
  Click('inspector-tab-properties');
  Click('action-builds');
  Click('action-agents');
  Ready;
end;
