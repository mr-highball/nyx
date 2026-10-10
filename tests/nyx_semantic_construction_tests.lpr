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
program nyx_semantic_construction_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model,
  nyx.controls, nyx.codec, nyx.schema, nyx.source, nyx.studio.projects,
  nyx.studio.session, nyx.studio.transactions, nyx.studio.mcp,
  nyx.studio.directories, nyx.studio.outputs, nyx.studio.buildexecutor,
  nyx.studio.builds, nyx.studio.sourceprojection, nyx.studio.workspaces,
  nyx.studio.buildjobs, nyx.studio.reviews, nyx.studio.editorbuild,
  nyx.test.projection;

const
  CTitle: TNyxText = 'A crafted notebook';
  CText: TNyxText = 'An exact rocket 🚀 and letter 𐐷.';
  COwner: TNyxText = 'qualification-connection';
  CActor: TNyxText = 'Semantic qualification';
  CUnfinished: TNyxText = #10 + '{ unfinished 🚀 }';
  CNewDraft: TNyxText = #10 + '{ newly typed 𐐷 }';

var
  GEngine: TNyxStudioMCP;
  GToken: TNyxText;
  GGate: TNyxText;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Semantic construction: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Fixture files are bounded bytes. In particular, checkpoint equality does not
  reinterpret the server's opaque recovery bytes as text. }
function ReadBytes(const APath: TNyxText): TNyxBytes;
var
  LFile: TFileStream;
begin
  Result := nil;
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size < 1) or (LFile.Size > 16 * 1024 * 1024) then
    begin
      raise Exception.Create('Expected bounded nonempty fixture bytes');
    end;
    SetLength(Result, LFile.Size);
    LFile.ReadBuffer(Result[0], Length(Result));
  finally
    LFile.Free;
  end;
end;

function SameBytes(const ALeft, ARight: TNyxBytes): Boolean;
var
  LIndex: Integer;
begin
  Result := Length(ALeft) = Length(ARight);

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to High(ALeft) do
  begin

    if ALeft[LIndex] <> ARight[LIndex] then
    begin
      Exit(False);
    end;
  end;
end;

procedure Marker(const APath: TNyxText; AExists: Boolean);
var
  LFile: TFileStream;
begin

  if AExists then
  begin
    LFile := TFileStream.Create(APath, fmCreate);
    LFile.Free;
  end
  else if FileExists(APath) then
  begin

    if not DeleteFile(APath) then
    begin
      raise Exception.Create('Could not release owned qualification marker');
    end;
  end;
end;

function Observe: TNyxDataValue;
begin
  Result := GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
end;

function Revision: Integer;
begin
  Result := Observe.Field('session').Field('revision').AsInteger;
end;

function Group(ARevision: Integer; const AID: TNyxText;
  const AOperations: TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('expectedRevision', NyxData(ARevision)),
    NyxField('operationId', NyxData(AID)), NyxField('operations', AOperations)]);
end;

function Title(const AText: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('op', NyxData('title')),
    NyxField('value', NyxData(AText))]);
end;

function Submit(const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := GEngine.InvokeTool('nyx_transaction', COwner, CActor, AArguments);
end;

function SameSession(const ALeft, ARight: TNyxDataValue): Boolean;
const
  CFields: array[0..9] of TNyxText = ('revision', 'permission', 'title',
    'selection', 'view', 'pages', 'components', 'pendingDraft', 'canUndo', 'canRedo');
var
  LIndex: Integer;
begin
  { Activity intentionally records enqueue/refusal even when no document command
    commits. Compare every document/history field, not that transient sequence. }
  for LIndex := Low(CFields) to High(CFields) do
  begin

    if ALeft.Field(CFields[LIndex]).ToJSON <> ARight.Field(CFields[LIndex]).ToJSON then
    begin
      Exit(False);
    end;
  end;
  Result := True;
end;

{ Rejections must retain source, design, draft, revision and paired history.
  The gate makes that assertion independent of compiler/host scheduling speed. }
procedure Refuse(const ATool: TNyxText; const AArguments: TNyxDataValue;
  const AReason: TNyxText);
var
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LRejected: Boolean;
begin
  LBefore := Observe;
  LRejected := False;
  try
    GEngine.InvokeTool(ATool, COwner, CActor, AArguments);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  LAfter := Observe;
  Check(LRejected and (LBefore.Field('project').AsText = LAfter.Field('project').AsText) and
    SameSession(LBefore.Field('session'), LAfter.Field('session')), AReason);
end;

function Await(const AJob: TNyxText): TNyxDataValue;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Result := GEngine.InvokeBuild(COwner, CActor, NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', NyxData(AJob)),
      NyxField('limit', NyxData(3))]));

    if Result.Field('publication').Field('state').AsText <> 'pending' then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise Exception.Create('Owned semantic job exceeded the qualification deadline');
    end;
    Sleep(5);
  until False;
end;

function History(const ADirection: TNyxText): TNyxDataValue;
begin
  Result := GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('history')), NyxField('expectedRevision', NyxData(Revision)),
    NyxField('direction', NyxData(ADirection))]));
end;

procedure Permission(const AValue: TNyxText);
begin
  GEngine.EditorExchange(GToken, NyxObject([NyxField('op', NyxData('configure')),
    NyxField('after', NyxData(0)), NyxField('permission', NyxData(AValue))]));
end;

function Draft(const AText: TNyxText): TNyxDataValue;
var
  LPair: TNyxProjectPair;
begin
  LPair := DecodeNyxProject(Observe.Field('project').AsText);
  LPair.Pending := True;
  LPair.Draft := AText;
  LPair.DraftBase := LPair.Source;
  Result := GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(Revision)),
    NyxField('project', NyxData(EncodeNyxProject(LPair))),
    NyxField('selection', NyxData('heading-1')), NyxField('view', NyxData('notebook-1'))]));
end;

procedure ClearDraft;
var
  LPair: TNyxProjectPair;
begin
  { Draft-only synchronization deliberately does not create an Undo command.
    Clear that buffer explicitly; Undo would instead undo the last design group. }
  LPair := DecodeNyxProject(Observe.Field('project').AsText);
  LPair.Pending := False;
  LPair.Draft := '';
  LPair.DraftBase := '';
  GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(Revision)),
    NyxField('project', NyxData(EncodeNyxProject(LPair))),
    NyxField('selection', NyxData('heading-1')), NyxField('view', NyxData('notebook-1'))]));
end;

var
  LDirectories: TNyxStudioDirectories;
  LProfile: TNyxOutputConfiguration;
  LExecutor: TNyxBuildExecutor;
  LLocal: TNyxStudioSession;
  LObserver: TNyxStudioSession;
  LDocument: TNyxDocument;
  LLock: TFileStream;
  LPool: TNyxBuildJobs;
  LUnavailable: TNyxOutputConfiguration;
  LProposal: INyxPreparedDesign;
  LCompletion: TNyxConstructionCompletion;
  LTools: TNyxDataValue;
  LState: TNyxDataValue;
  LBefore: TNyxDataValue;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LOperations: TNyxDataValue;
  LJobs: TNyxDataValue;
  LRequest: TNyxStudioSourcePublication;
  LBuild: INyxSourceProjectionBuild;
  LFrame: TNyxSourceCheckpoint;
  LPair: TNyxProjectPair;
  LBaseline: TNyxProjectPair;
  LEdit: TNyxStudioDesignEdit;
  LIntent: TNyxStudioDesignRequest;
  LSource: TNyxText;
  LExpected: TNyxText;
  LFailure: TNyxText;
  LMismatch: TNyxText;
  LCheckpoint: TNyxText;
  LJob: TNyxText;
  LBeforeBytes: TNyxBytes;
  LRejected: Boolean;
  LRevision: Integer;
  LIndex: Integer;
  LStarted: QWord;
  LCurrentOutput: Boolean;
begin
  LProfile := nil;
  LExecutor := nil;
  LLocal := nil;
  LObserver := nil;
  LDocument := nil;
  LLock := nil;
  LPool := nil;
  LUnavailable := nil;
  try

    if (ParamCount <> 3) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
    begin
      raise Exception.Create('Supply repository, toolchain JSON and a NEW owned runtime');
    end;
    LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
      .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
    ForceDirectories(LDirectories.RuntimeRoot);
    GGate := LDirectories.RuntimeRoot + 'constructor.wait';
    LFailure := LDirectories.RuntimeRoot + 'constructor.fail';
    LMismatch := LDirectories.RuntimeRoot + 'constructor.mismatch';
    LCheckpoint := LDirectories.RuntimeRoot + '.local/studio-session.nyx';
    LTools := TNyxDataValue.ParseJSON(NyxDecodeUTF8(ReadBytes(ParamStr(2))));
    LSource := NyxDecodeUTF8(ReadBytes(LDirectories.SourceRoot +
      'tests/fixtures/nyx.projection.fixture.pas'));
    { Native-only external-input fixture gates actual constructor execution.
      These private paths exist only in generated ignored qualification source.
      Browser qualification compiles the portable companion separately. }
    LSource := StringReplace(LSource, 'function PageName(AIndex: Integer): TNyxText;',
      'procedure WaitForQualification;' + #10 + 'begin' + #10 +
      '  while FileExists(' + TNyxText(QuotedStr(GGate)) + ') do' + #10 +
      '  begin' + #10 + '    Sleep(5);' + #10 + '  end;' + #10 + #10 +
      '  if FileExists(' + TNyxText(QuotedStr(LFailure)) + ') then' + #10 +
      '  begin' + #10 + '    raise Exception.Create(''Deliberate constructor refusal'');' + #10 +
      '  end;' + #10 + 'end;' + #10 + #10 +
      'function PageName(AIndex: Integer): TNyxText;', []);
    LSource := StringReplace(LSource, '  LDocument := TNyxDocument.Create;',
      '  WaitForQualification;' + #10 + '  LDocument := TNyxDocument.Create;', []);
    LSource := StringReplace(LSource, '    LDocument.Validate;',
      #10 + '    if FileExists(' + TNyxText(QuotedStr(LMismatch)) + ') then' + #10 +
      '    begin' + #10 + '      LDocument.State.SetValue(NyxIntegerState(''notes''), 4);' + #10 +
      '    end;' + #10 + '    LDocument.Validate;', []);
    { ANSI RTL StringReplace returns byte-identical text with its native codepage
      label. Re-admit these known UTF-8 fixture bytes before any text comparison;
      do not change the process codepage or relax the backend's exact guard. }
    LSource := NyxDecodeUTF8(NyxEncodeUTF8(LSource));
    LProfile := TNyxOutputConfiguration.Create;
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LLocal := TNyxStudioSession.Create;
    LLocal.SetSourceDraft(LSource);
    LPair := LLocal.ProjectSnapshot;
    Check(LPair.Pending and (LPair.Draft = LSource), 'fixture owns its exact pending source');
    { In-process authenticated hosting seam: no Start/listener, existing project,
      installed enrollment or browser UI automation is involved. }
    GEngine := TNyxStudioMCP.Create(LDirectories, 8762, 8763, LProfile.Encode);
    LState := GEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LState.Field('token').AsText;
    Check(LState.Field('state').Field('project').AsText = EncodeNyxProject(LPair),
      'initial editor claim preserves its exact pending pair');
    LBefore := Observe;
    Check(LBefore.Field('project').AsText = EncodeNyxProject(LPair),
      'independent observation retains the pending source');
    LRevision := LBefore.Field('session').Field('revision').AsInteger;
    GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, LRevision, LBaseline, LFrame);
    Check(LBaseline.Pending and (LBaseline.Draft = LSource),
      'native captured registry retains the same pending buffer');
    LRequest := GEngine.EditorCaptureSourcePublication(GToken, NyxPrimaryWorkspace,
      LRevision, LSource);
    LExecutor := TNyxBuildExecutor.Create(LDirectories, LProfile.Encode);
    LBuild := LExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.projection.fixture'),
      btNativeLCL, spcChecked);

    if LBuild.Projection.State <> spsExecuted then
    begin
      WriteLn('Native constructor state=', Ord(LBuild.Projection.State),
        ' message=', LBuild.Projection.Message);
    end;
    Check((LBuild.Projection.State = spsExecuted) and
      (LBuild.Projection.Design = ExpectedNyxProjectionDesign), 'actual baseline construction');
    Check(Pos('0 unfreed memory blocks', LBuild.RuntimeLog) > 0, 'constructor child clean heap');
    GEngine.EditorCommitSourceProjection(GToken, LRequest, LBuild.Projection,
      NyxControl('heading-1'), NyxControl('notebook-1'));
    GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, Revision, LBaseline, LFrame);
    LLocal.AdoptCapturedProject(LBaseline, LFrame);
    LOperations := NyxArray([Title(CTitle), NyxObject([
      NyxField('op', NyxData('update')), NyxField('id', NyxData('heading-1')),
      NyxField('properties', NyxObject([NyxField('text', NyxData(CText))]))]),
      NyxObject([NyxField('op', NyxData('update')), NyxField('id', NyxData('notebook-1')),
      NyxField('properties', NyxObject([NyxField('padding', NyxData(19))]))])]);
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTransaction;
    LEdit.Selection := 'heading-1';
    LEdit.View := 'notebook-1';
    LEdit.Transaction := CaptureNyxProjectTransaction(ReadNyxProjectTransaction(LOperations));
    LIntent := LLocal.PrepareDesignRequest(LEdit, CaptureNyxSchemas.Revision);
    Check((LIntent.IntentData.Field('version').AsInteger = 16) and
      (ReadNyxStudioDesignIntent(LIntent.IntentData).Transaction.ToData.ToJSON =
      LEdit.Transaction.ToData.ToJSON), 'closed grouped intent round trips independently');
    LDocument := TNyxCodec.Decode(ExpectedNyxProjectionDesign);
    LDocument.Title := CTitle;
    RequireNyxControl(LDocument, NyxControl('heading-1'), nkHeading).Configure.Text(CText);
    RequireNyxControl(LDocument, NyxControl('notebook-1'), nkPage).Configure.Padding(19);
    LExpected := TNyxCodec.Encode(LDocument);
    FreeAndNil(LDocument);
    Marker(GGate, True);
    LRevision := Revision;
    LArguments := Group(LRevision, 'first-semantic-group', LOperations);
    LReceipt := Submit(LArguments);
    LJob := LReceipt.Field('job').AsText;
    Check((LReceipt.Field('purpose').AsText = 'transaction') and
      (LReceipt.Field('scope').AsText = 'construction') and
      (LReceipt.Field('publication').Field('state').AsText = 'pending'),
      'bounded receipt distinguishes compiler job from document publication');
    Check((LReceipt.Field('artifact').AsText = '') and
      (LReceipt.Field('compiledSource').AsText = '') and (Length(LReceipt.ToJSON) < 8192),
      'receipt contains no application artifact or source');
    Check(Observe.Field('project').AsText = EncodeNyxProject(LBaseline),
      'enqueue preserves exact active source/design');
    Check(Submit(LArguments).ToJSON = LReceipt.ToJSON, 'exact retry does not start another worker');
    Refuse('nyx_transaction', Group(LRevision, 'first-semantic-group', NyxArray([Title('Other')])),
      'changed retry arguments refuse atomically');
    Refuse('nyx_transaction', Group(LRevision + 1, 'stale', NyxArray([Title('Other')])),
      'stale revision refuses before compiler admission');
    Refuse('nyx_transaction', Group(LRevision, 'invalid-tail', NyxArray([
      Title('Partial'), NyxObject([NyxField('op', NyxData('update')),
      NyxField('id', NyxData('missing')), NyxField('properties',
      NyxObject([NyxField('text', NyxData('No'))]))])])), 'late invalid operation retains whole pair');
    Refuse('nyx_transaction', NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('foreign-source')), NyxField('operations', LOperations),
      NyxField('source', NyxData(LSource))]), 'caller cannot supply source or execution authority');
    LJobs := GEngine.InvokeBuild(COwner, CActor, NyxObject([
      NyxField('mode', NyxData('jobs')), NyxField('limit', NyxData(2))]));
    Check((LJobs.Field('items').Count = 1) and
      (LJobs.Field('items').Item(0).Field('purpose').AsText = 'transaction') and
      LJobs.Field('items').Item(0).Field('canCancel').AsBoolean and
      not LJobs.Field('items').Item(0).Field('currentSource').AsBoolean,
      'observer metadata distinguishes owned unpublished semantic job');
    LJobs := GEngine.InvokeBuild('foreign-connection', CActor, NyxObject([
      NyxField('mode', NyxData('jobs'))]));
    Check(not LJobs.Field('items').Item(0).Field('canCancel').AsBoolean,
      'display actor cannot grant another transport cancellation');
    LRejected := False;
    try
      GEngine.InvokeBuild('foreign-connection', CActor, NyxObject([
        NyxField('mode', NyxData('cancel')), NyxField('job', NyxData(LJob)),
        NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('steal'))]));
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'foreign transport cannot cancel compiler verification');
    Marker(GGate, False);
    LStatus := Await(LJob);
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (LStatus.Field('publication').Field('state').AsText = 'committed') and
      (LStatus.Field('publication').Field('revision').AsInteger = LRevision + 1),
      'real compiler group publishes exactly the next revision');
    GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, Revision, LPair, LFrame);
    Check((LPair.Design = LExpected) and not LPair.Pending and
      (LFrame.Origin = nsoExecuted), 'whole exact Unicode group and execution origin admitted');
    Check((Pos('for LIndex := 1 to 2 do', LPair.Source) > 0) and
      (Pos('Padding(3 * 4)', LPair.Source) > 0) and
      (Pos('class function TNotebookCards.Caption', LPair.Source) > 0) and
      (Pos('INyxHeading', LPair.Source) > 0), 'helpers/expressions and specialized interfaces retained');
    Check(LStatus.Field('currentSource').AsBoolean and LStatus.Field('currentOutput').AsBoolean,
      'job currentness refers to published pair rather than its previous baseline');
    Check(Submit(LArguments).ToJSON = LReceipt.ToJSON, 'completed exact retry keeps original receipt');
    LObserver := TNyxStudioSession.Create;
    LObserver.AdoptCapturedProject(LPair, LFrame);
    Check((LObserver.Save = LExpected) and (LObserver.Source = LPair.Source),
      'independent observing host receives exact compiled pair');
    Check(History('undo').Field('project').AsText = EncodeNyxProject(LBaseline),
      'one Undo restores the entire previous source/design group');
    Check(History('redo').Field('project').AsText = EncodeNyxProject(LPair),
      'one Redo restores the entire verified group');
    LRejected := False;
    try
      GEngine.InvokeBuild(COwner, CActor, NyxObject([NyxField('mode', NyxData('launch')),
        NyxField('job', NyxData(LJob)), NyxField('expectedRevision', NyxData(Revision)),
        NyxField('operationId', NyxData('not-an-app'))]));
    except
      on LException: Exception do
      begin
        LRejected := Pos('not a launchable', LException.Message) > 0;
      end;
    end;
    Check(LRejected, 'constructor proof cannot be mounted as an application');
    Refuse('nyx_transaction', Group(Revision, 'unsupported-structure', NyxArray([
      Title('Partial'), NyxObject([NyxField('op', NyxData('delete')),
      NyxField('id', NyxData('heading-2'))])])), 'unsupported meaning refuses the complete group');
    Draft(LPair.Source + CUnfinished);
    Refuse('nyx_transaction', Group(Revision, 'pending', NyxArray([Title('No')])),
      'pending draft refuses compiler transaction admission');
    ClearDraft;
    Marker(GGate, True);
    LReceipt := Submit(Group(Revision, 'later-draft', NyxArray([Title('No')])));
    LBefore := Draft(LPair.Source + CNewDraft);
    Marker(GGate, False);
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('publication').Field('state').AsText = 'refused') and
      (Observe.Field('project').AsText = LBefore.Field('project').AsText),
      'later exact draft makes completion stale without replacing it');
    Check(not LStatus.Field('currentSource').AsBoolean,
      'unpublished diagnostics never navigate the active source');
    ClearDraft;
    Marker(GGate, True);
    LReceipt := Submit(Group(Revision, 'permission-change', NyxArray([Title('No')])));
    Permission('readOnly');
    LBefore := Observe;
    Marker(GGate, False);
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('publication').Field('state').AsText = 'refused') and
      (Observe.Field('project').AsText = LBefore.Field('project').AsText),
      'revoked edit permission prevents completed publication');
    Permission('edit');
    Marker(GGate, True);
    LBefore := Observe;
    LReceipt := Submit(Group(Revision, 'cancel-own', NyxArray([Title('No')])));
    GEngine.InvokeBuild(COwner, CActor, NyxObject([NyxField('mode', NyxData('cancel')),
      NyxField('job', LReceipt.Field('job')), NyxField('expectedRevision', NyxData(Revision)),
      NyxField('operationId', NyxData('cancel-owned-proof'))]));
    Marker(GGate, False);
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('state').AsText = 'cancelled') and
      (LStatus.Field('publication').Field('state').AsText = 'refused') and
      (Observe.Field('project').AsText = LBefore.Field('project').AsText),
      'owned cancellation joins and retains exact accepted pair');
    Marker(LFailure, True);
    LBefore := Observe;
    LReceipt := Submit(Group(Revision, 'runtime-failure', NyxArray([Title('No')])));
    LStatus := Await(LReceipt.Field('job').AsText);
    Marker(LFailure, False);
    Check((LStatus.Field('state').AsText = 'failed') and
      (LStatus.Field('publication').Field('state').AsText = 'refused') and
      (Observe.Field('project').AsText = LBefore.Field('project').AsText),
      'actual constructor exception never publishes partial design');
    Marker(LMismatch, True);
    LBefore := Observe;
    LReceipt := Submit(Group(Revision, 'whole-meaning-mismatch', NyxArray([Title('No')])));
    LStatus := Await(LReceipt.Field('job').AsText);
    Marker(LMismatch, False);
    Check((LStatus.Field('publication').Field('state').AsText = 'refused') and
      (Observe.Field('project').AsText = LBefore.Field('project').AsText),
      'same-source real native external-input mismatch refuses complete meaning');
    Marker(GGate, True);
    LBefore := Observe;
    LBeforeBytes := ReadBytes(LCheckpoint);
    LLock := TFileStream.Create(LCheckpoint, fmOpenRead or fmShareExclusive);
    LReceipt := Submit(Group(Revision, 'durable-refusal', NyxArray([Title('No')])));
    Marker(GGate, False);
    LStatus := Await(LReceipt.Field('job').AsText);
    FreeAndNil(LLock);
    Check(LStatus.Field('publication').Field('state').AsText = 'refused',
      'durable replacement failure refuses publication');
    Check(Observe.Field('project').AsText = LBefore.Field('project').AsText,
      'durable replacement failure restores exact source/design/draft');
    Check(SameSession(Observe.Field('session'), LBefore.Field('session')),
      'durable replacement failure restores revision/selection/history');
    Check(SameBytes(ReadBytes(LCheckpoint), LBeforeBytes),
      'durable replacement failure retains exact checkpoint bytes');
    { Direct owning-pool qualification isolates the narrow retirement race: a
      worker can join after the backend's first poll, before another admission.
      Unavailable-verifier jobs require no child or application output target;
      each joined completion deliberately remains unsealed in this test owner. }
    LBefore := Observe;
    LRevision := Revision;
    GEngine.EditorCaptureProject(GToken, NyxPrimaryWorkspace, LRevision, LPair, LFrame);
    LLocal.AdoptCapturedProject(LPair, LFrame);
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTitle;
    LEdit.Selection := 'heading-1';
    LEdit.View := 'notebook-1';
    LEdit.Value := 'Retention qualification';
    LIntent := LLocal.PrepareDesignRequest(LEdit, CaptureNyxSchemas.Revision);
    LProposal := PrepareNyxStudioDesign(LIntent, CaptureNyxSchemas);
    LRequest := GEngine.EditorCaptureVisualPublication(GToken, NyxPrimaryWorkspace,
      LRevision, LIntent.IntentData, LProposal.Source);
    LUnavailable := TNyxOutputConfiguration.Create;
    LUnavailable.SetField('fpc', LDirectories.RuntimeRoot + 'missing-qualification-verifier.exe');
    LPool := TNyxBuildJobs.Create(LDirectories.RunningIn(LDirectories.RuntimeRoot +
      'retention'), LUnavailable.Encode);
    LPool.ConfigureSourceLease(1);
    LJob := '';
    for LIndex := 1 to 16 do
    begin
      LReceipt := LPool.SubmitTransaction(CActor,
        Group(LRevision, 'retention-' + TNyxText(IntToStr(LIndex)),
        NyxArray([Title(LEdit.Value)])), Default(TNyxReviewRef), COwner,
        NyxPrimaryWorkspace, LRequest);

      if LIndex = 1 then
      begin
        LJob := LReceipt.Field('job').AsText;
      end;
      LStarted := GetTickCount64;
      while not LPool.TakeConstructionCompletion(LCompletion) do
      begin

        if GetTickCount64 - LStarted > 10000 then
        begin
          raise Exception.Create('Retained owning pool did not join its unavailable verifier');
        end;
        Sleep(1);
      end;
    end;
    LStatus := LPool.Status(NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(LJob))]), LPair, LCurrentOutput);
    Check(LStatus.Field('publication').Field('state').AsText = 'pending',
      'sixteen joined completions retain their first unsealed semantic edit');
    LRejected := False;
    try
      LPool.SubmitTransaction(CActor, Group(LRevision, 'retention-seventeen',
        NyxArray([Title(LEdit.Value)])), Default(TNyxReviewRef), COwner,
        NyxPrimaryWorkspace, LRequest);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'full pending publication retention refuses another job without eviction');
    LPool.FinishConstruction(NyxBuildJob(LJob), False, 0,
      Default(TNyxProjectPair), 'Qualification owner deliberately refused publication');
    LPool.SubmitTransaction(CActor, Group(LRevision, 'retention-seventeen',
      NyxArray([Title(LEdit.Value)])), Default(TNyxReviewRef), COwner,
      NyxPrimaryWorkspace, LRequest);
    LRejected := False;
    try
      LPool.Status(NyxObject([NyxField('mode', NyxData('status')),
        NyxField('job', NyxData(LJob))]), LPair, LCurrentOutput);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'only a sealed terminal completion becomes eligible for bounded retirement');
    FreeAndNil(LPool);
    Check((Observe.Field('project').AsText = LBefore.Field('project').AsText) and
      SameSession(Observe.Field('session'), LBefore.Field('session')),
      'owning worker pool cannot publish or borrow the active editor');
    WriteLn('PASS ', GChecks, ' semantic construction checks');
  finally
    LLock.Free;
    LPool.Free;
    LUnavailable.Free;
    Marker(GGate, False);
    LDocument.Free;
    LObserver.Free;
    LLocal.Free;
    LExecutor.Free;
    GEngine.Free;
    LProfile.Free;
  end;
end.
