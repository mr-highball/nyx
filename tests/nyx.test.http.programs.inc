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

{ This journey owns one freshly created ordinary project on an explicitly
  marked isolated host. All design/source composition uses authenticated semantic
  tools; native input qualifies the public form and both observers read the same
  accepted pair. No primary project pair is imported, replaced or exported. }
procedure RunPrograms(const AOrigin, AFixturePair: TNyxText);
const
  CGreeting: TNyxText = 'Greeting';
  CFirst: TNyxText = 'begin Caption := ''Welcome, '' + Name end.';
  COperator: TNyxText = 'begin Caption := ''A personal welcome, '' + Name end.';
var
  LBrowser: TNyxBrowserPipe;
  LParent: TNyxDataValue;
  LReply: TNyxDataValue;
  LArgs: TNyxDataValue;
  LStatus: TNyxDataValue;
  LTools: TNyxDataValue;
  LPair: TNyxProjectPair;
  LImported: TNyxText;
  LInitial: TNyxText;
  LAfter: TNyxText;
  LBefore: TNyxText;
  LImport: TNyxText;
  LTicket: TNyxText;
  LText: TNyxText;
  LWindow: TNyxDataValue;
  LIndex: Integer;
  LOffset: Integer;
  LStart: Integer;
  LCount: Integer;
  LScalar: Integer;
  LFound: Boolean;
  LStarted: QWord;
  LDefinition: INyxProgramDefinition;
  LCard: TComboBox;
  LMarker: TNyxDataValue;

  function Lifecycle(const ATool: TNyxText; const AValue: TNyxDataValue): TNyxDataValue;
  var
    LResult: TNyxDataValue;
  begin
    LResult := GClient.Tool(ATool, AValue);

    if LResult.Field('isError').AsBoolean then
    begin
      Save('program-lifecycle-refusal.json', LResult.ToJSON);
      raise Exception.Create('Owned program lifecycle refused');
    end;
    Result := LResult.Field('structuredContent');
  end;

  function Envelope(const AMode: TNyxText;
    const AFields: array of TNyxDataField): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
    LIndex: Integer;
  begin
    SetLength(LFields, Length(AFields) + 3);
    LFields[0] := NyxField('mode', NyxData(AMode));
    LFields[1] := NyxField('expectedRevision', NyxData(GRevision));
    LFields[2] := NyxField('operationId', NyxData(Operation));
    for LIndex := 0 to High(AFields) do
    begin
      LFields[LIndex + 3] := AFields[LIndex];
    end;
    Result := NyxObject(LFields);
  end;

  function Pair: TNyxText;
  var
    LOffset: Integer;
    LValue: TNyxDataValue;
  begin
    Check(GWorkspace.ID <> '', 'paired export is restricted to this owned project');
    Result := '';
    LOffset := 0;
    repeat
      LValue := Call('nyx_project', NyxObject([
        NyxField('mode', NyxData('export')), NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('part', NyxData('project')), NyxField('offset', NyxData(LOffset)),
        NyxField('count', NyxData(2048))]));
      Result := Result + LValue.Field('text').AsText;
      LOffset := LValue.Field('nextOffset').AsInteger;
    until LOffset = LValue.Field('total').AsInteger;
  end;

  procedure BrowserFace(const ASelector: TNyxText);
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;

      if LBrowser.RuntimeError <> '' then
      begin
        LBrowser.CaptureRuntimeError;
        raise Exception.Create('Ordinary program observer raised');
      end;

      if LBrowser.Exists(ASelector) then
      begin
        Exit;
      end;
      Sleep(30);
    until GetTickCount64 - LStarted > 90000;
    LBrowser.Capture('program-face-refusal');
    raise Exception.Create('Ordinary program observer did not mount ' + ASelector);
  end;

  procedure Observe(const ASource: TNyxText);
  var
    LActual: TNyxText;
    LStarted: QWord;
  begin
    AwaitSource(ASource);
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;

      if LBrowser.TryFieldValue('[data-node="studio-code"]', LActual) and
        (LActual = ASource) then
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = Pair,
          'native observer retains the complete accepted pair');
        Check(True, 'ordinary browser acknowledges the exact semantic Pascal');
        Exit;
      end;

      if LBrowser.RuntimeError <> '' then
      begin
        LBrowser.CaptureRuntimeError;
        raise Exception.Create('Program observer failed during source publication');
      end;
      Sleep(30);
    until GetTickCount64 - LStarted > 120000;
    LBrowser.Capture('program-source-refusal');
    Save('program-native-source-refusal.pas', GStudio.Session.Source);
    raise Exception.Create('Independent program browser did not acknowledge exact source');
  end;

  function ProgramSource(const AName: TNyxText): TNyxText;
  var
    LValue: TNyxDataValue;
  begin
    LValue := Call('nyx_logic', NyxObject([
      NyxField('scope', NyxData('program-source')), NyxField('program', NyxData(AName)),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('textLimit', NyxData(1024))]));
    Check(not LValue.Field('truncated').AsBoolean, 'owned short program is complete in one bounded window');
    Result := LValue.Field('text').AsText;
  end;

  procedure ExactHistory(const ABefore, AAfter: TNyxText);
  var
    LReceipt: TNyxDataValue;
  begin
    LReceipt := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation)),
      NyxField('direction', NyxData('undo'))]));
    GRevision := LReceipt.Field('revision').AsInteger;
    Check(Pair = ABefore, 'one semantic Undo restores the exact complete pair');
    Observe(DecodeNyxProject(ABefore).Source);
    LReceipt := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation)),
      NyxField('direction', NyxData('redo'))]));
    GRevision := LReceipt.Field('revision').AsInteger;
    Check(Pair = AAfter, 'one semantic Redo restores the exact complete pair');
    Observe(DecodeNyxProject(AAfter).Source);
  end;

  procedure Apply(const AChanges: array of TNyxProgramChange);
  var
    LItems: array of TNyxDataValue;
    LIndex: Integer;
    LBefore: TNyxText;
    LAfter: TNyxText;
    LArguments: TNyxDataValue;
    LReceipt: TNyxDataValue;
  begin
    LBefore := Pair;
    SetLength(LItems, Length(AChanges));
    for LIndex := 0 to High(AChanges) do
    begin
      LItems[LIndex] := AChanges[LIndex].ToData;
    end;
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)), NyxField('operations', NyxArray(LItems))]);
    LReceipt := Call('nyx_transaction', LArguments);
    Check(LReceipt.Field('publication').Field('state').AsText = 'pending',
      'handwritten program edits require the owning real compiler');
    Check(Call('nyx_transaction', LArguments).ToJSON = LReceipt.ToJSON,
      'pending exact retry retains the same owning job');
    WaitConstruction(LReceipt);
    Check(Call('nyx_transaction', LArguments).ToJSON = LReceipt.ToJSON,
      'completed exact retry retains the original receipt');
    LAfter := Pair;
    Check((LAfter <> LBefore) and (Pos('class function TWorkshopPrograms.StableSource', Source) > 0) and
      (Pos('.Padding(12 + 4)', Source) > 0),
      'semantic program publication retains handwritten helpers and computed controls');
    Observe(DecodeNyxProject(LAfter).Source);
    ExactHistory(LBefore, LAfter);
    WriteLn('Qualified owning program compiler publication and paired observers');
    Flush(Output);
  end;

  procedure Build(const ATarget, AScope, AView: TNyxText);
  var
    LFields: array of TNyxDataField;
    LReceipt: TNyxDataValue;
    LStatus: TNyxDataValue;
    LStarted: QWord;
    LSource: TNyxText;
    LOutput: TNyxText;
  begin
    LSource := Source;
    LOutput := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('outputs'))])).Field('outputID').AsText;
    SetLength(LFields, 6 + Ord(AView <> ''));
    LFields[0] := NyxField('mode', NyxData('request'));
    LFields[1] := NyxField('expectedRevision', NyxData(GRevision));
    LFields[2] := NyxField('operationId', NyxData(Operation));
    LFields[3] := NyxField('outputID', NyxData(LOutput));
    LFields[4] := NyxField('target', NyxData(ATarget));
    LFields[5] := NyxField('scope', NyxData(AScope));

    if AView <> '' then
    begin
      LFields[6] := NyxField('view', NyxData(AView));
    end;
    LReceipt := Call('nyx_build', NyxObject(LFields));
    LStarted := GetTickCount64;
    repeat
      LStatus := Call('nyx_build', NyxObject([
        NyxField('mode', NyxData('status')), NyxField('job', LReceipt.Field('job')),
        NyxField('severity', NyxData('error')), NyxField('limit', NyxData(3))]));

      if LStatus.Field('state').AsText = 'succeeded' then
      begin
        Break;
      end;

      if (LStatus.Field('state').AsText = 'failed') or
        (LStatus.Field('state').AsText = 'cancelled') then
      begin
        Save('program-build-failure.json', LStatus.ToJSON);
        raise Exception.Create('Owned program compiler failed');
      end;
      Application.ProcessMessages;
      Sleep(50);
    until GetTickCount64 - LStarted > 180000;
    Save('program-build-' + ATarget + '-' + AScope + '.json', LStatus.ToJSON);
    Check((LStatus.Field('state').AsText = 'succeeded') and LStatus.Field('currentSource').AsBoolean and
      (LStatus.Field('sourceFingerprint').AsText = NyxBuildFingerprint(LSource)),
      'real ' + ATarget + '/' + AScope + ' compiler owns the exact accepted Pascal');
    Check(Source = LSource, 'ordinary build does not publish a design edit');
    WriteLn('Qualified program compiler ', ATarget, '/', AScope);
    Flush(Output);
  end;

begin
  LBrowser := nil;
  try
    Check(Pos('http://127.0.0.1:', AOrigin) = 1, 'owned program workshop is local');
    LText := IncludeTrailingPathDelimiter(ExtractFileDir(ExtractFileDir(ExpandFileName(ParamStr(2))))) +
      '.local' + PathDelim + 'runtime-qualification.json';
    LMarker := TNyxDataValue.ParseJSON(Read(LText));
    Check((LMarker.Field('service').AsText = 'nyx-resource-runtime-qualification') and
      (LMarker.Field('origin').AsText = AOrigin), 'program workshop admits only its marked isolated host');
    LParent := Lifecycle('nyx_session', NyxObject([]));
    LTools := GClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools');
    LFound := False;
    for LIndex := 0 to LTools.Count - 1 do
    begin
      LFound := LFound or ((LTools.Item(LIndex).Field('name').AsText = 'nyx_logic') and
        (Pos('program-check', LTools.Item(LIndex).ToJSON) > 0));
    end;
    Check(LFound, 'authenticated discovery exposes the current program scopes');
    LReply := Lifecycle('nyx_workspaces', NyxObject([
      NyxField('mode', NyxData('create')), NyxField('base', NyxData('empty')),
      NyxField('label', NyxData('Pascal behavior workshop')), NyxField('operationId', NyxData(Operation)),
      NyxField('expectedRevision', LParent.Field('revision'))]));
    GWorkspace := NyxWorkspace(LReply.Field('workspace').AsText);
    Check(GWorkspace.ID <> '', 'ordinary program project has an explicit owned identity');
    GRevision := Session.Field('revision').AsInteger;
    LInitial := Pair;
    LImported := Read(AFixturePair);
    LPair := DecodeNyxProject(LImported);
    LArgs := Envelope('begin-import', [NyxField('bytes', NyxData(NyxUTF8ByteCount(LImported)))]);
    LReply := Call('nyx_project', LArgs);
    LImport := LReply.Field('projectImport').Field('import').AsText;
    LIndex := 1;
    LOffset := 0;
    while LIndex <= Length(LImported) do
    begin
      LStart := LIndex;
      LCount := 0;
      while (LIndex <= Length(LImported)) and (LCount < 2048) do
      begin

        if not NyxNextScalar(LImported, LIndex, LScalar) then
        begin
          raise Exception.Create('Owned project packet has malformed Unicode');
        end;
        Inc(LCount);
      end;
      LReply := Call('nyx_project', Envelope('append-import', [
        NyxField('import', NyxData(LImport)), NyxField('offset', NyxData(LOffset)),
        NyxField('text', NyxData(Copy(LImported, LStart, LIndex - LStart)))]));
      LOffset := LReply.Field('projectImport').Field('nextOffset').AsInteger;
    end;
    Check(Pair = LInitial, 'semantic staging preserves the complete ordinary project');
    LArgs := Envelope('review-import', [NyxField('import', NyxData(LImport)),
      NyxField('resolution', NyxData('match')), NyxField('admission', NyxData('compiler'))]);
    LReply := Call('nyx_project', LArgs);
    LStarted := GetTickCount64;
    repeat
      LStatus := Call('nyx_build', NyxObject([
        NyxField('mode', NyxData('status')), NyxField('job', LReply.Field('job')),
        NyxField('severity', NyxData('error')), NyxField('limit', NyxData(3))]));

      if (LStatus.Field('state').AsText = 'succeeded') or
        (LStatus.Field('state').AsText = 'failed') then
      begin
        Break;
      end;
      Application.ProcessMessages;
      Sleep(50);
    until GetTickCount64 - LStarted > 180000;
    Save('program-import-construction.json', LStatus.ToJSON);
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (LStatus.Field('publication').Field('state').AsText = 'reviewed'),
      'real compiler reviews the exact uploaded program project before publication');
    Check(Pair = LInitial, 'compiler review retains active source/history');
    LTicket := LStatus.Field('projectImport').Field('reviewID').AsText;
    LArgs := Envelope('apply', [NyxField('import', NyxData(LImport)), NyxField('reviewID', NyxData(LTicket))]);
    LReply := Call('nyx_project', LArgs);
    GRevision := LReply.Field('revision').AsInteger;
    Check(Pair = LImported, 'semantic import publishes the whole exact handwritten pair');
    Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON, 'exact import Apply retry is one paired step');

    GStudio := TNyxNativeStudio.Create(GForm, GEvidence + 'native-projects', nil,
      NewNyxSharedNativeSourceCompilerFactory(AOrigin));
    GStudio.Run;
    GStudio.ConnectService(AOrigin, GWorkspace);
    AwaitSource(LPair.Source);
    Click('action-code');
    LBrowser := TNyxBrowserPipe.Create(AOrigin + '/', GEvidence + 'program-observer', 1280, 960);
    BrowserFace('[data-node="action-agents"]');
    LBrowser.Click('[data-node="action-agents"]');
    BrowserFace('[data-node="studio-agent-workspace-jump-' + GWorkspace.ID + '"]');
    LBrowser.Click('[data-node="studio-agent-workspace-jump-' + GWorkspace.ID + '"]');
    BrowserFace('[data-node="studio-agent-workspace-current-' + GWorkspace.ID + '"]');

    if not LBrowser.Exists('[data-node="studio-code"]') then
    begin
      LBrowser.Click('[data-node="action-code"]');
    end;
    Observe(LPair.Source);
    LBefore := Pair;
    LWindow := Call('nyx_logic', NyxObject([
      NyxField('scope', NyxData('programs')), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('limit', NyxData(1))]));
    Check((LWindow.Field('items').Count = 1) and (LWindow.Field('total').AsInteger = 2) and
      (Pos('source', LWindow.ToJSON) = 0), 'authenticated program discovery is bounded without source dumps');
    LWindow := Call('nyx_logic', NyxObject([
      NyxField('scope', NyxData('program-ports')), NyxField('program', NyxData(CGreeting)),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('limit', NyxData(1))]));
    Check((LWindow.Field('items').Count = 1) and (LWindow.Field('total').AsInteger = 2),
      'authenticated typed port discovery pages only requested context');
    LWindow := Call('nyx_logic', NyxObject([
      NyxField('scope', NyxData('program-check')), NyxField('definition', NyxObject([
        NyxField('version', NyxData(1)), NyxField('source', NyxData('begin Missing := 1 end.')),
        NyxField('contract', GStudio.Session.Document.Programs.Definition(NyxProgramRef(CGreeting)).Contract.ToData),
        NyxField('limits', NyxProgramLimits.ToData)]))]));
    Check(not LWindow.Field('succeeded').AsBoolean and
      (LWindow.Field('items').Item(0).Field('line').AsInteger = 1) and (Pair = LBefore),
      'authenticated proposed diagnostics preserve the complete accepted pair');
    LDefinition := GStudio.Session.Document.Programs.Definition(NyxProgramRef(CGreeting));
    Apply([NyxDefineProgram(NyxProgramRef(CGreeting), NewNyxProgramDefinition(
      CFirst, LDefinition.Contract, LDefinition.Limits.Steps(5000)))]);
    Check(ProgramSource(CGreeting) = CFirst, 'semantic definition replacement retains exact source');
    Apply([NyxDefineProgram(NyxProgramRef('Canvas layout'), NewNyxProgramDefinition(
      'begin MoveItem(SceneItem(0), 12, 8) end.', NyxCanvasProgramContract))]);
    LWindow := Call('nyx_logic', NyxObject([
      NyxField('scope', NyxData('program-routine-arguments')), NyxField('program', NyxData('Canvas layout')),
      NyxField('routine', NyxData('MoveItem')), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('limit', NyxData(1))]));
    Check((LWindow.Field('items').Count = 1) and (LWindow.Field('total').AsInteger = 3),
      'authenticated canvas vocabulary exposes bounded typed parameters');

    Click('action-programs-toggle');
    LCard := TComboBox(GStudio.ShellView.InputFor('studio-program-editor-choice'));
    LCard.ItemIndex := LCard.Items.IndexOf(CGreeting);
    LCard.OnChange(LCard);
    Click('studio-program-editor-open');
    LBefore := Pair;
    TMemo(GStudio.ShellView.InputFor('studio-program-editor-source')).Text := COperator;
    Click('studio-program-editor-apply');
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      Sleep(25);
    until ((Session.Field('revision').AsInteger > GRevision) and not GStudio.SourceBusy) or
      (GetTickCount64 - LStarted > 180000);
    GRevision := Session.Field('revision').AsInteger;
    Check((GStudio.SourceCommands.State = nssApplied) and (ProgramSource(CGreeting) = COperator),
      'actual public native form delegates handwritten program Save to the owning compiler');
    LAfter := Pair;
    Observe(DecodeNyxProject(LAfter).Source);
    ExactHistory(LBefore, LAfter);
    Capture('program-native-source.png');
    LBrowser.Capture('program-desktop-source');
    LBrowser.Resize(390, 844);
    Observe(Source);
    LBrowser.Capture('program-compact-source');
    LBrowser.Resize(1280, 960);
    Apply([NyxRemoveProgram(NyxProgramRef('Canvas layout'))]);
    Build('browser', 'application', '');
    Build('lcl', 'application', '');
    Build('browser', 'view', 'home');
    Build('lcl', 'view', 'home');
    Build('browser', 'reusable', 'greeting-card');
    Build('lcl', 'reusable', 'greeting-card');
    LAfter := Pair;
    Save('program-accepted.nyxpair', LAfter);
    Save('program-accepted-source.pas', Source);
    LReply := Lifecycle('nyx_session', NyxObject([]));
    Check((LReply.Field('revision').AsInteger = LParent.Field('revision').AsInteger) and
      (LReply.Field('title').AsText = LParent.Field('title').AsText) and
      (LReply.Field('selection').AsText = LParent.Field('selection').AsText) and
      (LReply.Field('canUndo').AsBoolean = LParent.Field('canUndo').AsBoolean),
      'isolated primary remains untouched by the explicit owned workshop');
    Save('program-final-session.json', Session.ToJSON);
    WriteLn('Qualified authenticated programs, ordinary observers and both application/view compilers');
    Flush(Output);
  finally
    LBrowser.Free;
  end;
end;
