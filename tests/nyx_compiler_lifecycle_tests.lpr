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

program nyx_compiler_lifecycle_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, Windows, fpjson, nyx.text, nyx.data, nyx.codec, nyx.model, nyx.editing,
  nyx.studio.projects, nyx.studio.agents, nyx.studio.builds,
  nyx.studio.buildjobs, nyx.studio.buildexecutor, nyx.studio.directories,
  nyx.studio.outputs, nyx.studio.compiler, nyx.studio.editorbuild,
  nyx.studio.mcp, nyx.studio.workspaces, nyx.studio.reviews;

type
  { Native handles identify only family members whose ready files were produced
    by our owned compiler fixture. No service enumeration/termination is used. }
  TChild = record
    PID: Cardinal;
    Handle: THandle;
    Directory: TNyxText;
    Role: TNyxText;
  end;

  { A real executor on a private thread lets this harness capture living family
    members before releasing their fixture gate. Read results only after join. }
  TExecutorWorker = class(TThread)
  private
    FRoot: TNyxText;
    FProfile: TNyxText;
    FPair: TNyxProjectPair;
    FLimits: TNyxCompilerLimits;
    FCancellation: INyxBuildCancellation;
  protected
    procedure Execute; override;
  public
    Output: TNyxDataValue;
    Error: TNyxText;
    Cancelled: Boolean;
    constructor Create(const ARoot, AProfile: TNyxText;
      const APair: TNyxProjectPair; const ALimits: TNyxCompilerLimits);
    destructor Destroy; override;
    procedure RequestCancel;
  end;

var
  GChecks: Integer;
  GRoot: TNyxText;
  GProfile: TNyxOutputConfiguration;
  GSession: TNyxAgentSession;
  GPair: TNyxProjectPair;
  GJobs: TNyxBuildJobs;
  GChildren: array of TChild;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

constructor TExecutorWorker.Create(const ARoot, AProfile: TNyxText;
  const APair: TNyxProjectPair; const ALimits: TNyxCompilerLimits);
begin
  inherited Create(True);
  FRoot := ARoot;
  FProfile := AProfile;
  FPair := APair;
  FLimits := ALimits;
  Output := NyxNull;
  FCancellation := NewNyxBuildCancellation;
  Start;
end;

procedure TExecutorWorker.Execute;
var
  LExecutor: TNyxBuildExecutor;
  LDocument: TNyxDocument;
  LResult: TJSONObject;
begin
  LExecutor := nil;
  LDocument := nil;
  LResult := nil;
  try
    try
      LExecutor := TNyxBuildExecutor.Create(FRoot, FProfile);
      LExecutor.ConfigureLimits(FLimits);
      LDocument := TNyxCodec.Decode(FPair.Design);
      LResult := LExecutor.Build(LDocument, 'browser', 'view', 'home', FPair.Source,
        FCancellation);
      Output := TNyxDataValue.ParseJSON(TNyxText(LResult.AsJSON));
    except
      on ENyxBuildCancelled do
      begin
        Cancelled := True;
      end;
      on LException: Exception do
      begin
        Error := LException.Message;
      end;
    end;
  finally
    LResult.Free;
    LDocument.Free;
    LExecutor.Free;
  end;
end;

procedure TExecutorWorker.RequestCancel;
begin
  FCancellation.Cancel;
end;

destructor TExecutorWorker.Destroy;
begin

  if FCancellation <> nil then
  begin
    FCancellation.Cancel;
  end;
  WaitFor;
  inherited Destroy;
end;

procedure Policy(const AJobs, AMode: TNyxText);
var
  LText: TStringList;
begin
  ForceDirectories(AJobs);
  LText := TStringList.Create;
  try
    LText.Text := AMode;
    LText.SaveToFile(AJobs + 'fixture.mode');
  finally
    LText.Free;
  end;
end;

procedure CaptureChildren(const AJobs: TNyxText);
var
  LEntry: TSearchRec;
  LText: TStringList;
  LPID: Cardinal;
  LIndex: Integer;
  LKnown: Boolean;
  LHandle: THandle;
  LRoleIndex: Integer;
  LReady: TNyxText;
  LReadyStream: TFileStream;
begin
  LText := TStringList.Create;
  try

    if FindFirst(AJobs + 'job-*', faDirectory, LEntry) = 0 then
    begin
      try
        repeat

          for LRoleIndex := 0 to 2 do
          begin
            case LRoleIndex of
              0: LReady := 'compiler.ready';
              1: LReady := 'helper.ready';
              2: LReady := 'grandchild.ready';
            end;

            if not FileExists(AJobs + LEntry.Name + '/' + LReady) then
            begin
              Continue;
            end;
            { Marker publication/polling crosses a real Windows file boundary.
              A sharing refusal is not an absent/terminal process: leave this
              exact marker pending and retry within WaitChildren's budget. }
            try
              LReadyStream := TFileStream.Create(AJobs + LEntry.Name + '/' + LReady,
                fmOpenRead or fmShareDenyNone);
            except
              on EFOpenError do
              begin
                Continue;
              end;
            end;
            try
              LText.LoadFromStream(LReadyStream);
            finally
              LReadyStream.Free;
            end;
            LPID := StrToInt(Trim(LText.Text));
            LKnown := False;
            for LIndex := 0 to High(GChildren) do
            begin
              LKnown := LKnown or (GChildren[LIndex].PID = LPID);
            end;

            if not LKnown then
            begin
              LHandle := OpenProcess(SYNCHRONIZE, False, LPID);
              Check((LHandle <> 0) and (WaitForSingleObject(LHandle, 0) = WAIT_TIMEOUT),
                'Owned compiler family member is alive when its identity is captured');
              LIndex := Length(GChildren);
              SetLength(GChildren, LIndex + 1);
              GChildren[LIndex].PID := LPID;
              GChildren[LIndex].Handle := LHandle;
              GChildren[LIndex].Directory := AJobs + LEntry.Name + '/';
              GChildren[LIndex].Role := LReady;
            end;
          end;
        until FindNext(LEntry) <> 0;
      finally
        SysUtils.FindClose(LEntry);
      end;
    end;
  finally
    LText.Free;
  end;
end;

procedure WaitChildren(const AJobs: TNyxText; ACount: Integer);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    CaptureChildren(AJobs);

    if Length(GChildren) >= ACount then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 5000 then
    begin
      raise Exception.Create('Owned fixture did not start within its admission budget');
    end;
    Sleep(10);
  until False;
end;

function Request(const AID: TNyxText): TNyxDataValue;
begin
  Result := NewNyxCompilerRequest.Target(btBrowser).Scope(bsView)
    .Root(NyxBuildRoot('home')).AtRevision(GSession.Revision)
    .Output(NyxBuildOutput(NyxBuildFingerprint(GProfile.Encode)))
    .Operation(NyxBuildOperation(AID)).Arguments;
end;

function CancelArguments(const AJob, AOperation: TNyxText): TNyxDataValue;
begin
  Result := NyxCompilerCancel(NyxBuildJob(AJob), GSession.Revision,
    NyxBuildOperation(AOperation));
end;

function Status(const AJob: TNyxText; ACheckPair: Boolean = True): TNyxDataValue;
var
  LPair: TNyxProjectPair;
  LCurrent: Boolean;
begin
  Result := GJobs.Status(NyxCompilerStatus(NyxBuildJob(AJob)), LPair, LCurrent);

  if ACheckPair then
  begin
    Check(EncodeNyxProject(LPair) = EncodeNyxProject(GPair), 'Cancellation retains immutable input pair');
  end;
end;

procedure WaitCancelled(const AJob: TNyxText);
var
  LStarted: QWord;
  LValue: TNyxDataValue;
begin
  LStarted := GetTickCount64;
  repeat
    LValue := Status(AJob, False);

    if NyxBuildJobTerminal(ParseNyxBuildJobState(LValue.Field('state').AsText)) then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted >= 5000 then
    begin
      raise Exception.Create('Cancellation did not join within the qualified Windows budget');
    end;
    Sleep(10);
  until False;
  { Qualify the final pair once; polling frequency must not inflate the number
    of distinct assertions reported by this asynchronous fixture. }
  LValue := Status(AJob);
  Check((LValue.Field('state').AsText = 'cancelled') and
    (LValue.Field('artifact').AsText = '') and (LValue.Field('manifest').Count = 0),
    'Joined cancelled job advertises no artifact or manifest');
end;

procedure SemanticAuthority;
const
  CVisibleActor: TNyxText = 'Same visible actor';
var
  LEngine: TNyxStudioMCP;
  LToken: TNyxText;
  LBefore, LAfter, LClaim, LFirst, LSecond, LValue, LArguments: TNyxDataValue;
  LCancel, LRetry: TNyxDataValue;
  LReport: INyxCompilerReport;
  LRevision: Integer;
  LPair: TNyxProjectPair;
  LStarted: QWord;
  LRefused: Boolean;
  LIndex: Integer;
  LField: TNyxText;

  function Observe: TNyxDataValue;
  begin
    Result := LEngine.EditorExchange(LToken, NyxObject([
      NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
  end;

  function BuildRequest: TNyxDataValue;
  begin
    Result := NewNyxCompilerRequest.Target(btBrowser).Scope(bsApplication)
      .AtRevision(LRevision).Output(NyxBuildOutput(NyxBuildFingerprint(GProfile.Encode)))
      .Operation(NyxBuildOperation('same-request-id')).Arguments;
  end;

  procedure RefuseCancel(const AOwner: TNyxText; const AArguments: TNyxDataValue);
  begin
    LRefused := False;
    try
      LEngine.InvokeBuild(AOwner, CVisibleActor, AArguments);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Semantic cancellation rejects foreign ownership or stale revision');
    Check(Observe.Field('project').AsText = LBefore.Field('project').AsText,
      'Refused semantic cancellation retains the entire accepted pair');
  end;

begin
  Policy(GRoot + 'semantic/build/studio/jobs/', 'hold');
  { The actual native engine remains suspended. Its authenticated HTTP route
    calls InvokeBuild too; this qualification creates no listener/enrollment
    outside its new owned runtime. It does not claim HTTP authentication. }
  LEngine := TNyxStudioMCP.Create(GRoot + 'semantic/', 8628, 8629, GProfile.Encode);
  try
    LClaim := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(GPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    LToken := LClaim.Field('token').AsText;
    LRevision := Observe.Field('session').Field('revision').AsInteger;
    LEngine.InvokeTool('nyx_transaction', 'connection-one', CVisibleActor,
      NyxObject([NyxField('operationId', NyxData('owned-title')),
        NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operations', NyxArray([NyxObject([NyxField('op', NyxData('title')),
          NyxField('value', NyxData('English compiler lifecycle review'))])]))]));
    LBefore := Observe;
    LRevision := LBefore.Field('session').Field('revision').AsInteger;
    LPair := DecodeNyxProject(LBefore.Field('project').AsText);
    LReport := ReadNyxCompilerReport(LPair.Source, LPair.Source, 'accepted.pas',
      'accepted.pas(1,1) Warning: Retained accepted compiler report');
    LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('report')),
      NyxField('after', NyxData(0)),
      NyxField('report', NyxData(LReport.Encode))]));
    LBefore := Observe;
    LArguments := BuildRequest;
    LFirst := LEngine.InvokeBuild('connection-one', CVisibleActor, LArguments);
    LSecond := LEngine.InvokeBuild('connection-two', CVisibleActor, LArguments);
    Check(LFirst.Field('job').AsText <> LSecond.Field('job').AsText,
      'Same display actor and operation ID still admit independent primary jobs');
    LRetry := LEngine.InvokeBuild('connection-one', 'Renamed actor', LArguments);
    Check(LRetry.ToJSON = LFirst.ToJSON, 'Renaming display actor retains connection-owned build retry');
    LValue := LEngine.InvokeBuild('connection-one', CVisibleActor, NyxCompilerJobs(cjfActive, 0, 1));
    Check((LValue.Field('items').Count = 1) and (LValue.Field('total').AsInteger = 2) and
      LValue.Field('items').Item(0).Field('canCancel').AsBoolean,
      'Bounded discovery exposes the admitting connection cancellation capability');
    Check(not NyxAgentHas(LValue.Field('items').Item(0), 'source') and
      not NyxAgentHas(LValue.Field('items').Item(0), 'owner') and
      not NyxAgentHas(LValue.Field('items').Item(0), 'profile') and
      not NyxAgentHas(LValue.Field('items').Item(0), 'log'),
      'Discovery omits accepted source, compiler buffers and private ownership');
    LValue := LEngine.InvokeBuild('connection-one', CVisibleActor, NyxCompilerJobs(cjfActive, 1, 1));
    Check((LValue.Field('items').Item(0).Field('job').AsText = LSecond.Field('job').AsText) and
      not LValue.Field('items').Item(0).Field('canCancel').AsBoolean,
      'Discovery pages exact jobs and refuses foreign connection cancellation capability');
    LValue := Observe;
    Check(LValue.Field('buildJobControl').AsBoolean and
      LValue.Field('buildJobs').Field('items').Item(1).Field('canCancel').AsBoolean,
      'Operator observation advertises context-bounded cancellation independently of agent ownership');
    WaitChildren(GRoot + 'semantic/build/studio/jobs/', 5);
    LCancel := NyxCompilerCancel(NyxBuildJob(LFirst.Field('job').AsText), LRevision,
      NyxBuildOperation('owned-cancel'));
    RefuseCancel('connection-two', LCancel);
    RefuseCancel('connection-one', NyxCompilerCancel(NyxBuildJob(LFirst.Field('job').AsText),
      LRevision - 1, NyxBuildOperation('stale-cancel')));
    { Both actual compilers now belong to an earlier source. Explicit cancel
      requires the new revision, and completion must retain the revised pair. }
    LEngine.InvokeTool('nyx_transaction', 'connection-one', CVisibleActor,
      NyxObject([NyxField('operationId', NyxData('revise-during-build')),
        NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operations', NyxArray([NyxObject([NyxField('op', NyxData('title')),
          NyxField('value', NyxData('English revised application'))])]))]));
    LBefore := Observe;
    LRevision := LBefore.Field('session').Field('revision').AsInteger;
    LValue := LEngine.InvokeBuild('connection-one', CVisibleActor,
      NyxCompilerStatus(NyxBuildJob(LFirst.Field('job').AsText)));
    Check(not LValue.Field('currentSource').AsBoolean,
      'Actual running job becomes stale after a semantic project edit');
    LValue := LEngine.InvokeBuild('connection-one', CVisibleActor, NyxCompilerJobs);
    Check(not LValue.Field('items').Item(0).Field('currentSource').AsBoolean and
      LValue.Field('items').Item(0).Field('canCancel').AsBoolean,
      'Earlier-source jobs remain discoverable and cancellable at the new revision');
    LCancel := NyxCompilerCancel(NyxBuildJob(LFirst.Field('job').AsText), LRevision,
      NyxBuildOperation('owned-cancel'));
    LValue := LEngine.InvokeBuild('connection-one', CVisibleActor, LCancel);
    Check(LValue.Field('state').AsText = 'cancelling', 'Actual semantic path requests owned retirement');
    LRetry := LEngine.InvokeBuild('connection-one', 'Renamed actor', LCancel);
    Check(LRetry.ToJSON = LValue.ToJSON, 'Semantic cancellation retry is immutable');
    LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('configure')),
      NyxField('after', NyxData(0)),
      NyxField('permission', NyxData('readOnly'))]));
    RefuseCancel('connection-two', NyxCompilerCancel(NyxBuildJob(LSecond.Field('job').AsText),
      LRevision, NyxBuildOperation('read-only-cancel')));
    LValue := LEngine.InvokeBuild('connection-two', CVisibleActor, NyxCompilerJobs);
    Check(not LValue.Field('items').Item(LValue.Field('items').Count - 1)
      .Field('canCancel').AsBoolean, 'Read-only discovery never advertises a mutating capability');
    LValue := LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('build')),
      NyxField('after', NyxData(0)),
      NyxField('build', NyxCompilerCancel(NyxBuildJob(LSecond.Field('job').AsText),
        LRevision, NyxBuildOperation('operator-cancel')))])).Field('buildReply');
    Check(LValue.Field('state').AsText = 'cancelling', 'Operator can cancel in read-only agent mode');
    LStarted := GetTickCount64;
    repeat
      LValue := LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('build')),
        NyxField('after', NyxData(0)),
        NyxField('build', NyxCompilerStatus(NyxBuildJob(LSecond.Field('job').AsText)))])).Field('buildReply');

      if LValue.Field('state').AsText = 'cancelled' then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted >= 5000 then
      begin
        raise Exception.Create('Actual semantic cancellation did not join its child');
      end;
      Sleep(10);
    until False;
    LAfter := Observe;
    Check(LAfter.Field('project').AsText = LBefore.Field('project').AsText,
      'Semantic retirement retains exact accepted Pascal and design');
    Check(LAfter.Field('compiler').ToJSON = LBefore.Field('compiler').ToJSON,
      'Canceled completion retains the previous compiler report and its sequence');
    for LIndex := 0 to 5 do
    begin
      case LIndex of
        0: LField := 'revision';
        1: LField := 'selection';
        2: LField := 'view';
        3: LField := 'pendingDraft';
        4: LField := 'canUndo';
        5: LField := 'canRedo';
      end;
      Check(LAfter.Field('session').Field(LField).ToJSON =
        LBefore.Field('session').Field(LField).ToJSON,
        'Cancellation retains project navigation/draft/history availability');
    end;
  finally
    LReport := nil;
    LEngine.Free;
  end;
end;

procedure DiscoveryContexts;
var
  LJobs: TNyxBuildJobs;
  LPrimary, LProject, LReview, LValue: TNyxDataValue;
  LRefused: Boolean;
begin
  { Query independent immutable contexts without changing the existing family
    retirement fixture's timing. These actual held workers belong only here. }
  Policy(GRoot + 'discovery/build/studio/jobs/', 'hold');
  LJobs := TNyxBuildJobs.Create(GRoot + 'discovery/', GProfile.Encode);
  try
    LPrimary := LJobs.Submit('Primary', Request('primary'), GPair,
      NyxActiveWorkspace, 'connection-primary');
    LProject := LJobs.Submit('Project', Request('project'), GPair,
      NyxActiveWorkspace, 'connection-project', NyxWorkspace('qualification.project-1'));
    LReview := LJobs.Submit('Review', Request('review'), GPair,
      NyxReview('qualification-review'), 'connection-review');
    LValue := LJobs.List(NyxCompilerJobs(cjfActive, 0, 1), NyxActiveWorkspace,
      NyxPrimaryWorkspace, 'connection-primary', False, True, GSession.CurrentPair);
    Check((LValue.Field('total').AsInteger = 1) and (LValue.Field('queued').AsInteger = 0) and
      (LValue.Field('items').Item(0).Field('job').AsText = LPrimary.Field('job').AsText),
      'Primary counts and pages exclude both project and review jobs');
    LValue := LJobs.List(NyxCompilerJobs(cjfAll, 0, 1), NyxActiveWorkspace,
      NyxWorkspace('qualification.project-1'), 'connection-project', False, True, GSession.CurrentPair);
    Check((LValue.Field('total').AsInteger = 1) and
      (LValue.Field('items').Item(0).Field('job').AsText = LProject.Field('job').AsText),
      'Independent project discovery never falls back to primary');
    LValue := LJobs.List(NyxCompilerJobs, NyxReview('qualification-review'),
      NyxPrimaryWorkspace, 'connection-review', False, True, GSession.CurrentPair);
    Check((LValue.Field('total').AsInteger = 1) and (LValue.Field('queued').AsInteger = 1) and
      (LValue.Field('running').AsInteger = 0) and
      (LValue.Field('items').Item(0).Field('job').AsText = LReview.Field('job').AsText),
      'Review metadata reports only its own queued job despite shared running slots');
    LRefused := False;
    try
      LJobs.List(NyxObject([NyxField('mode', NyxData('jobs')), NyxField('limit', NyxData(17))]),
        NyxActiveWorkspace, NyxPrimaryWorkspace, 'connection-primary', False, True, GSession.CurrentPair);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Actual service admission refuses a larger metadata window');
  finally
    LJobs.Free;
  end;
end;

procedure QueueAndShutdown;
var
  LReceipts: array[0..9] of TNyxDataValue;
  LIndex: Integer;
  LValue: TNyxDataValue;
  LRetry: TNyxDataValue;
  LCancel: TNyxDataValue;
  LRefused: Boolean;
  LStarted: QWord;
  LActor, LOutcome: TNyxText;
  LPair: TNyxProjectPair;
  LReport: INyxCompilerReport;
  LExited: Integer;
begin
  Policy(GRoot + 'queue/build/studio/jobs/', 'family-hold');
  GJobs := TNyxBuildJobs.Create(GRoot + 'queue/', GProfile.Encode);
  for LIndex := 0 to High(LReceipts) do
  begin
    LReceipts[LIndex] := GJobs.Submit('Scooty', Request('queue-' + IntToStr(LIndex)), GPair);
  end;
  WaitChildren(GRoot + 'queue/build/studio/jobs/', 6);
  LRefused := False;
  try
    GJobs.Submit('Scooty', Request('overflow'), GPair);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Two running plus eight pending bounds admission');
  Check(Status(LReceipts[0].Field('job').AsText).Field('state').AsText = 'running',
    'Actual first compiler retains its slot');
  Check(Status(LReceipts[2].Field('job').AsText).Field('state').AsText = 'queued',
    'Third immutable job waits without spawning');
  LCancel := CancelArguments(LReceipts[2].Field('job').AsText, 'cancel-queued');
  LValue := GJobs.Cancel('Scooty', LCancel, False);
  Check(LValue.Field('state').AsText = 'cancelled', 'Queued cancellation never constructs a child');
  Check(GJobs.Retry('Scooty', LCancel, LRetry) and (LRetry.ToJSON = LValue.ToJSON),
    'Cancellation retry returns its immutable receipt');
  LRefused := False;
  try
    GJobs.Cancel('Other connection', CancelArguments(LReceipts[0].Field('job').AsText,
      'foreign-cancel'), False);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Foreign connection cannot retire the owned compiler');
  LValue := GJobs.Cancel('Scooty', CancelArguments(LReceipts[0].Field('job').AsText,
    'cancel-running'), False);
  Check(LValue.Field('state').AsText = 'cancelling', 'Running cancellation retains its slot');
  WaitCancelled(LReceipts[0].Field('job').AsText);
  LExited := 0;
  for LIndex := 0 to High(GChildren) do
  begin

    if WaitForSingleObject(GChildren[LIndex].Handle, 0) = WAIT_OBJECT_0 then
    begin
      Inc(LExited);
    end;
  end;
  Check(LExited = 3, 'Terminal cancellation means compiler/helper/grandchild exit');
  WaitChildren(GRoot + 'queue/build/studio/jobs/', 9);
  Check(Status(LReceipts[3].Field('job').AsText).Field('state').AsText = 'running',
    'Oldest remaining queued job takes the joined slot');
  Check(Status(LReceipts[4].Field('job').AsText).Field('state').AsText = 'queued',
    'No third actual compiler starts');
  while GJobs.TakeCompletion(LActor, LOutcome, LPair, LReport) do
  begin
    Check(LReport = nil, 'Cancellation notification cannot replace the accepted report');
  end;
  LStarted := GetTickCount64;
  FreeAndNil(GJobs);
  Check(GetTickCount64 - LStarted < 5000, 'Shutdown signals all running children before joining');
  for LIndex := 0 to High(GChildren) do
  begin
    Check(WaitForSingleObject(GChildren[LIndex].Handle, 0) = WAIT_OBJECT_0,
      'Shutdown retires every actually started compiler/helper/grandchild');
  end;
end;

procedure EarlierOutputReport;
var
  LEngine: TNyxStudioMCP;
  LProfile: TNyxOutputConfiguration;
  LToken, LJob: TNyxText;
  LValue, LBefore, LAfter: TNyxDataValue;
  LReport: INyxCompilerReport;
  LStarted: QWord;
begin
  LEngine := TNyxStudioMCP.Create(GRoot + 'output-report/', 8638, 8639, GProfile.Encode);
  LProfile := TNyxOutputConfiguration.Decode(GProfile.Encode);
  try
    LValue := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(GPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    LToken := LValue.Field('token').AsText;
    LReport := ReadNyxCompilerReport(GPair.Source, GPair.Source, 'accepted.pas',
      'accepted.pas(1,1) Warning: Keep the accepted report');
    LBefore := LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('report')),
      NyxField('report', NyxData(LReport.Encode))]));
    LValue := LEngine.InvokeBuild('output-owner', 'English output review',
      NewNyxCompilerRequest.Target(btBrowser).Scope(bsApplication)
        .AtRevision(LBefore.Field('session').Field('revision').AsInteger)
        .Output(NyxBuildOutput(NyxBuildFingerprint(GProfile.Encode)))
        .Operation(NyxBuildOperation('earlier-output')).Arguments);
    LJob := LValue.Field('job').AsText;
    LProfile.SetField('pas2js', '');
    LEngine.ConfigureOutputs(LProfile.Encode);
    LStarted := GetTickCount64;
    repeat
      LValue := LEngine.InvokeBuild('output-owner', 'English output review',
        NyxCompilerStatus(NyxBuildJob(LJob)));

      if NyxBuildJobTerminal(ParseNyxBuildJobState(LValue.Field('state').AsText)) then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 5000 then
      begin
        raise Exception.Create('Earlier output qualification did not complete');
      end;
      Sleep(10);
    until False;
    Check((LValue.Field('state').AsText = 'succeeded') and
      LValue.Field('currentSource').AsBoolean and not LValue.Field('currentOutput').AsBoolean,
      'Actual completed job retains its earlier output independently of source currentness');
    LAfter := LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('observe'))]));
    Check(LAfter.Field('compiler').ToJSON = LBefore.Field('compiler').ToJSON,
      'Earlier output completion never replaces the accepted compiler report');
    Check(LAfter.Field('project').AsText = LBefore.Field('project').AsText,
      'Earlier output completion retains the exact paired design and source');
  finally
    LReport := nil;
    LProfile.Free;
    LEngine.Free;
  end;
end;

procedure BudgetRetirement(const AName, AMode: TNyxText; const ALimits: TNyxCompilerLimits);
var
  LExecutor: TNyxBuildExecutor;
  LDocument: TNyxDocument;
  LResult: TJSONObject;
  LStarted: QWord;
  LRoot: TNyxText;
  LEntry: TSearchRec;
  LText: TStringList;
  LHandle: THandle;
  LReady: Integer;
begin
  LRoot := GRoot + AName + '/';
  Policy(LRoot + 'build/studio/jobs/', AMode);
  LExecutor := TNyxBuildExecutor.Create(LRoot, GProfile.Encode);
  LDocument := TNyxCodec.Decode(GPair.Design);
  LResult := nil;
  try
    LExecutor.ConfigureLimits(ALimits);
    LStarted := GetTickCount64;
    LResult := LExecutor.Build(LDocument, 'browser', 'view', 'home', GPair.Source);
    Check(not LResult.Get('ok', True), AName + ' stops the actual compiler');
    Check(NyxTextScalarCount(TNyxDataValue.ParseJSON(TNyxText(LResult.AsJSON)).Field('log').AsText) > 0,
      AName + ' returns well-formed Unicode diagnostics at its byte boundary');

    if AMode = 'unicode-flood' then
    begin
      Check(Pos(TNyxText('🌙'), TNyxDataValue.ParseJSON(TNyxText(LResult.AsJSON)).Field('log').AsText) > 0,
        'Log truncation retains earlier supplementary Unicode scalars exactly');
    end;

    if AMode = 'hold' then
    begin
      Check(LResult.Get('failure', '') = 'time-budget', 'Deadline has a typed execution failure');
    end
    else
    begin
      Check(LResult.Get('failure', '') = 'log-budget', 'Log cap has a typed execution failure');
    end;
    Check(GetTickCount64 - LStarted < 5000, AName + ' retires within the qualified Windows budget');
    { The process has already been joined, so the ready file's PID must be dead.
      OpenProcess may still obtain a terminated handle; neither result is alive. }
    LText := TStringList.Create;
    try
      LReady := 0;

      if FindFirst(LRoot + 'build/studio/jobs/job-*', faDirectory, LEntry) = 0 then
      begin
        try
          repeat

            if FileExists(LRoot + 'build/studio/jobs/' + LEntry.Name + '/compiler.ready') then
            begin
              Inc(LReady);
              LText.LoadFromFile(LRoot + 'build/studio/jobs/' + LEntry.Name + '/compiler.ready');
              LHandle := OpenProcess(SYNCHRONIZE, False, StrToInt(Trim(LText.Text)));
              try
                Check((LHandle = 0) or (WaitForSingleObject(LHandle, 0) = WAIT_OBJECT_0),
                  AName + ' returns only after actual compiler exit');
              finally

                if LHandle <> 0 then
                begin
                  CloseHandle(LHandle);
                end;
              end;
              Check(not FileExists(LRoot + 'build/studio/jobs/' + LEntry.Name + '/nyx_preview.js'),
                AName + ' refuses the delayed artifact');
            end;
          until FindNext(LEntry) <> 0;
        finally
          SysUtils.FindClose(LEntry);
        end;
      end;
      Check(LReady = 1, AName + ' exercises one actual started compiler');
    finally
      LText.Free;
    end;
  finally
    LResult.Free;
    LDocument.Free;
    LExecutor.Free;
  end;
end;

procedure FamilyCompletion(const AName, AMode, AExpected: TNyxText;
  const ALimits: TNyxCompilerLimits);
var
  LWorker: TExecutorWorker;
  LFirst: Integer;
  LIndex: Integer;
  LRoot: TNyxText;
  LGate: TFileStream;
  LStarted: QWord;
begin
  LRoot := GRoot + AName + '/';
  Policy(LRoot + 'build/studio/jobs/', AMode);
  LFirst := Length(GChildren);
  LWorker := TExecutorWorker.Create(LRoot, GProfile.Encode, GPair, ALimits);
  try
    WaitChildren(LRoot + 'build/studio/jobs/', LFirst + 3);
    Check(Length(GChildren) = LFirst + 3, 'One invocation owns compiler/helper/grandchild');
    { Captured handles prove all three were living before the gate opens. The
      children intentionally do not inherit the compiler pipe handles. }
    LGate := TFileStream.Create(GChildren[LFirst].Directory + 'family.continue', fmCreate);
    LGate.Free;

    if AMode = 'family-root-exit' then
    begin
      for LIndex := LFirst to High(GChildren) do
      begin

        if GChildren[LIndex].Role = 'compiler.ready' then
        begin
          Check(WaitForSingleObject(GChildren[LIndex].Handle, 1000) = WAIT_OBJECT_0,
            'Compiler exits while its detached helpers remain alive');
        end;
      end;
      Check(not LWorker.Finished, 'Compiler exit cannot publish completion while helpers run');
    end;

    if AExpected = 'cancelled' then
    begin
      LWorker.RequestCancel;
    end;
    LStarted := GetTickCount64;
    while not LWorker.Finished do
    begin

      if GetTickCount64 - LStarted > 5000 then
      begin
        raise Exception.Create('Owned process family did not retire within the Windows budget');
      end;
      Sleep(10);
    end;
    LWorker.WaitFor;
    Check(LWorker.Error = '', 'Compiler family completes without a host exception');

    if AExpected = 'cancelled' then
    begin
      Check(LWorker.Cancelled and (LWorker.Output.Kind = ndNull),
        'Cancellation after compiler exit still retires helpers without publishing an artifact');
    end
    else
    begin
      Check(not LWorker.Cancelled and
        (LWorker.Output.Field('failure').AsText = AExpected) and
        (LWorker.Output.Field('ok').AsBoolean = (AExpected = 'none')),
        'Whole-family completion retains the typed compiler outcome');
    end;
    for LIndex := LFirst to High(GChildren) do
    begin
      Check(WaitForSingleObject(GChildren[LIndex].Handle, 0) = WAIT_OBJECT_0,
        'Result publication follows actual retirement of every captured family member');
    end;
  finally
    LWorker.Free;
  end;
end;

var
  LIndex: Integer;
begin
  GProfile := nil;
  GSession := nil;
  GJobs := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply new owned runtime, Pascal fixture executable and matching rtl.js');
    end;
    GRoot := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));
    Check(not DirectoryExists(GRoot), 'Lifecycle qualification owns a new runtime');
    GProfile := TNyxOutputConfiguration.Create;
    GProfile.SetField('pas2js', ExpandFileName(ParamStr(2)));
    GProfile.SetField('runtime', ExpandFileName(ParamStr(3)));
    GSession := TNyxAgentSession.Create;
    GPair := GSession.BuildPair(GSession.Revision, bsView, 'home');
    QueueAndShutdown;
    SemanticAuthority;
    DiscoveryContexts;
    EarlierOutputReport;
    FamilyCompletion('family-cancel', 'family-root-exit', 'cancelled', TNyxCompilerLimits.Default);
    FamilyCompletion('family-success', 'family-success', 'none', TNyxCompilerLimits.Default);
    FamilyCompletion('family-error', 'family-error', 'compiler', TNyxCompilerLimits.Default);
    FamilyCompletion('family-deadline', 'family-root-exit', 'time-budget',
      TNyxCompilerLimits.Default.TimeMilliseconds(2000));
    FamilyCompletion('family-log-budget', 'family-flood', 'log-budget',
      TNyxCompilerLimits.Default.LogBytes(65536));
    BudgetRetirement('deadline', 'hold', TNyxCompilerLimits.Default.TimeMilliseconds(200));
    BudgetRetirement('log-budget', 'flood', TNyxCompilerLimits.Default.LogBytes(65536));
    { Each Windows fixture line is 4093 ASCII bytes, one four-byte moon, CR/LF.
      8194 ends halfway through the second moon. The prefix must stay valid. }
    BudgetRetirement('unicode-log-budget', 'unicode-flood', TNyxCompilerLimits.Default.LogBytes(8194));
    FreeAndNil(GSession);
    FreeAndNil(GProfile);
    WriteLn('PASS ', GChecks, ' actual compiler family/queue/cancellation/join checks');
  except
    on LException: Exception do
    begin
      GJobs.Free;
      GSession.Free;
      GProfile.Free;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  for LIndex := 0 to High(GChildren) do
  begin
    CloseHandle(GChildren[LIndex].Handle);
  end;
end.
