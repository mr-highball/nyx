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

program nyx_source_service_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.model,
  nyx.studio.session, nyx.studio.projects, nyx.studio.directories,
  nyx.studio.outputs, nyx.studio.builds, nyx.studio.buildjobs,
  nyx.studio.mcp, nyx.studio.workspaces, nyx.studio.reviews,
  nyx.studio.sourcebuilds, nyx.studio.sourceprojection;

var
  GChecks: Integer;
  GEngine: TNyxStudioMCP;
  GToken: TNyxText;
  GRevision: Integer;
  GSource: TNyxText;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check(LFile.Size <= NyxProjectionMaximumSourceBytes, 'owned source input has a byte bound');
    SetLength(LBytes, LFile.Size);

    if Length(LBytes) > 0 then
    begin
      LFile.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;

function Request(const AID, ASource: TNyxText;
  ARevision: Integer): TNyxDataValue;
begin
  Result := NyxObject([NyxField('mode', NyxData('request')),
    NyxField('operationId', NyxData(AID)),
    NyxField('expectedRevision', NyxData(ARevision)), NyxField('source', NyxData(ASource))]);
end;

function Exchange(const AArguments: TNyxDataValue;
  const AToken: TNyxText = ''): TNyxDataValue;
var
  LToken: TNyxText;
begin
  LToken := AToken;

  if LToken = '' then
  begin
    LToken := GToken;
  end;
  Result := GEngine.EditorSourceExchange(LToken,
    NyxObject([NyxField('compile', AArguments)]));
end;

procedure Refuse(const AArguments: TNyxDataValue; const AReason: TNyxText;
  const AToken: TNyxText = '');
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    Exchange(AArguments, AToken);
  except
    on LException: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
end;

function WaitJob(const AJob: TNyxText): TNyxDataValue;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Result := Exchange(NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(AJob))]));

    if NyxBuildJobTerminal(ParseNyxBuildJobState(Result.Field('state').AsText)) then
    begin
      Exit;
    end;
    Sleep(5);
  until GetTickCount64 - LStarted > 180000;
  raise Exception.Create('Owned actual source compilation did not reach terminal within its budget');
end;

procedure QueueBudget(const ADirectories: TNyxStudioDirectories;
  const AFixture, ARuntime: TNyxText; const APair: TNyxProjectPair);
var
  LProfile: TNyxOutputConfiguration;
  LJobs: TNyxBuildJobs;
  LFile: TFileStream;
  LMode: TNyxText;
  LAppArgs: TNyxDataValue;
  LApp: TNyxDataValue;
  LSource: array[0..8] of TNyxDataValue;
  LIndex: Integer;
  LRefused: Boolean;
  LPair: TNyxProjectPair;
  LCurrent: Boolean;
  LState: TNyxDataValue;
  LAppState: TNyxDataValue;
  LStarted: QWord;
begin
  LProfile := TNyxOutputConfiguration.Create;
  LJobs := nil;
  try
    LProfile.SetField('pas2js', AFixture);
    LProfile.SetField('runtime', ARuntime);
    LJobs := TNyxBuildJobs.Create(ADirectories, LProfile.Encode);
    LFile := TFileStream.Create(ADirectories.Jobs + 'fixture.mode', fmCreate);
    try
      LMode := 'hold';
      LFile.WriteBuffer(LMode[1], Length(LMode));
    finally
      LFile.Free;
    end;
    LAppArgs := NyxObject([NyxField('mode', NyxData('request')),
      NyxField('expectedRevision', NyxData(1)), NyxField('operationId', NyxData('held-application')),
      NyxField('outputID', NyxData(NyxBuildFingerprint(LProfile.Encode))),
      NyxField('target', NyxData('browser')), NyxField('scope', NyxData('application'))]);
    LJobs.AdmitRequest(LAppArgs);
    LApp := LJobs.Submit('owned-operator', LAppArgs, APair);
    for LIndex := 0 to High(LSource) do
    begin
      LSource[LIndex] := LJobs.RequestSource(Request('held-source-' + IntToStr(LIndex),
        GSource, 1), APair, NyxPrimaryWorkspace, 'private-source:');
    end;
    LRefused := False;
    try
      LJobs.RequestSource(Request('overflow-source', GSource, 1), APair,
        NyxPrimaryWorkspace, 'private-source:');
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'application and source jobs share two slots plus eight queued jobs');
    LRefused := False;
    try
      LJobs.Submit('owned-operator', LAppArgs, APair);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'source admission cannot hide an extra application worker budget');
    LRefused := False;
    try
      LJobs.SourceStatus(LApp.Field('job').AsText, NyxPrimaryWorkspace, 'private-source:');
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'source polling refuses application handles');
    LRefused := False;
    try
      LJobs.Status(NyxObject([NyxField('mode', NyxData('status')),
        NyxField('job', LSource[0].Field('job'))]), LPair, LCurrent);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'application polling refuses source execution handles');
    for LIndex := High(LSource) downto 1 do
    begin
      LState := LJobs.SourceStatus(LSource[LIndex].Field('job').AsText,
        NyxPrimaryWorkspace, 'private-source:', True);
      Check(LState.Field('state').AsText = 'cancelled', 'queued source cancellation never spawns');
    end;
    LJobs.SourceStatus(LSource[0].Field('job').AsText,
      NyxPrimaryWorkspace, 'private-source:', True);
    LJobs.Cancel('owned-operator', NyxObject([
      NyxField('mode', NyxData('cancel')), NyxField('job', LApp.Field('job')),
      NyxField('operationId', NyxData('cancel-held-application')),
      NyxField('expectedRevision', NyxData(1))]), False);
    LStarted := GetTickCount64;
    repeat
      LState := LJobs.SourceStatus(LSource[0].Field('job').AsText,
        NyxPrimaryWorkspace, 'private-source:');
      LAppState := LJobs.Status(NyxObject([NyxField('mode', NyxData('status')),
        NyxField('job', LApp.Field('job'))]), LPair, LCurrent);

      if (LState.Field('state').AsText = 'cancelled') and
        (LAppState.Field('state').AsText = 'cancelled') then
      begin
        Break;
      end;
      Sleep(5);
    until GetTickCount64 - LStarted > 10000;
    Check(LState.Field('state').AsText = 'cancelled', 'source cancellation joins its owned worker');
    Check(LAppState.Field('state').AsText = 'cancelled', 'mixed application worker also joins');
  finally
    LJobs.Free;
    LProfile.Free;
  end;
end;

procedure LeaseExpiry(const ADirectories: TNyxStudioDirectories;
  const AFixture, ARuntime: TNyxText; const APair: TNyxProjectPair);
var
  LProfile: TNyxOutputConfiguration;
  LJobs: TNyxBuildJobs;
  LFile: TFileStream;
  LMode: TNyxText;
  LSource: array[0..2] of TNyxDataValue;
  LEntry: TSearchRec;
  LReady: Integer;
  LIndex: Integer;
  LStarted: QWord;
  LState: TNyxDataValue;
begin
  LProfile := TNyxOutputConfiguration.Create;
  LJobs := nil;
  try
    LProfile.SetField('pas2js', AFixture);
    LProfile.SetField('runtime', ARuntime);
    LJobs := TNyxBuildJobs.Create(ADirectories, LProfile.Encode);
    LJobs.ConfigureSourceLease(1500);
    LFile := TFileStream.Create(ADirectories.Jobs + 'fixture.mode', fmCreate);
    try
      LMode := 'hold';
      LFile.WriteBuffer(LMode[1], Length(LMode));
    finally
      LFile.Free;
    end;
    for LIndex := 0 to High(LSource) do
    begin
      LSource[LIndex] := LJobs.RequestSource(Request('lease-source-' + IntToStr(LIndex),
        GSource, 1), APair, NyxPrimaryWorkspace, 'private-source:');
    end;
    LStarted := GetTickCount64;
    repeat
      LReady := 0;

      if FindFirst(ADirectories.Jobs + 'job-*', faDirectory, LEntry) = 0 then
      begin
        repeat

          if FileExists(ADirectories.Jobs + LEntry.Name + '/compiler.ready') then
          begin
            Inc(LReady);
          end;
        until FindNext(LEntry) <> 0;
        FindClose(LEntry);
      end;

      if LReady = 2 then
      begin
        Break;
      end;
      Sleep(5);
    until GetTickCount64 - LStarted > 10000;
    Check(LReady = 2, 'lease qualification owns two actually started compiler processes');
    { Deliberately pass every captured lease without pumping. Readiness files
      prove physical start; only later status/join establishes terminal state. }
    Sleep(1600);
    LState := LJobs.SourceStatus(LSource[2].Field('job').AsText,
      NyxPrimaryWorkspace, 'private-source:');
    Check(LState.Field('state').AsText = 'cancelled', 'expired queued source never starts');
    LStarted := GetTickCount64;
    repeat
      LState := LJobs.SourceJobs(NyxPrimaryWorkspace, 'private-source:');

      if LState.Field('active').AsInteger = 0 then
      begin
        Break;
      end;
      Sleep(5);
    until GetTickCount64 - LStarted > 10000;
    Check(LState.Field('active').AsInteger = 0, 'expired physical compiler jobs retire and join');
    for LIndex := 0 to 1 do
    begin
      LState := LJobs.SourceStatus(LSource[LIndex].Field('job').AsText,
        NyxPrimaryWorkspace, 'private-source:');
      Check(LState.Field('state').AsText = 'cancelled', 'whole-job lease retires actual running work');
    end;
  finally
    LJobs.Free;
    LProfile.Free;
  end;
end;

var
  LDirectories: TNyxStudioDirectories;
  LProfile: TNyxOutputConfiguration;
  LLocal: TNyxStudioSession;
  LTools: TNyxDataValue;
  LClaim: TNyxDataValue;
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LArgs: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LOriginalReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LBuild: INyxSourceProjectionBuild;
  LFailedSource: TNyxText;
  LRefused: Boolean;
  LPair: TNyxProjectPair;
  LChangedPair: TNyxProjectPair;
  LOtherWorkspace: TNyxWorkspaceRef;
  LMalformed: TNyxDataValue;
begin
  LProfile := nil;
  LLocal := nil;
  try

    if (ParamCount <> 5) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
    begin
      raise Exception.Create('Supply repository, toolchain, NEW runtime, source fixture and compiler fixture');
    end;
    LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
      .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
    LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
    GSource := ReadText(ParamStr(4));
    LProfile := TNyxOutputConfiguration.Create;
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LLocal := TNyxStudioSession.Create;
    LPair := LLocal.ProjectSnapshot;
    LLocal.SetSourceDraft(GSource);
    GEngine := TNyxStudioMCP.Create(LDirectories, 8762, 8763, LProfile.Encode);
    LClaim := GEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LLocal.ProjectSnapshot))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LClaim.Field('token').AsText;
    GRevision := LClaim.Field('state').Field('session').Field('revision').AsInteger;
    Check(LClaim.Field('state').Field('sourceCompilation').AsBoolean,
      'private editor capability advertises source compilation');
    LBefore := GEngine.EditorExchange(GToken, NyxObject([
      NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
    LArgs := Request('actual-source', GSource, GRevision);
    Refuse(LArgs, 'wrong editor capability cannot admit execution', 'not-an-editor-capability');
    Refuse(Request('stale-source', GSource, GRevision + 1), 'fresh jobs require exact project revision');
    Refuse(NyxObject([NyxField('mode', NyxData('request')),
      NyxField('source', NyxData(GSource)), NyxField('operationId', NyxData('untyped')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('shell', NyxData('no'))]),
      'source jobs reject compiler options or shell fields');
    LReceipt := Exchange(LArgs);
    LOriginalReceipt := LReceipt;
    Check(LReceipt.Field('state').AsText = 'queued', 'actual constructor compilation is admitted asynchronously');
    Check(Exchange(LArgs).ToJSON = LReceipt.ToJSON, 'exact source retry returns its original receipt');
    Refuse(Request('actual-source', GSource + #10, GRevision), 'changed source cannot reuse an operation');
    LStatus := WaitJob(LReceipt.Field('job').AsText);
    Check(LStatus.Field('state').AsText = 'succeeded', 'real pas2js compiled the complete handwritten constructor');
    LBuild := DecodeNyxBrowserSourceBuild(GSource, LStatus.Field('receipt'));
    Check((LBuild.Projection.State = spsCompiled) and
      (LBuild.Projection.Design = '') and (LBuild.Artifact <> ''),
      'compiler receipt has a bound worker and no executed design');
    Check((LBuild.Projection.Report <> nil) and (LBuild.Projection.Report.Source = GSource),
      'diagnostics restore exact owned source without echoing it in the receipt');
    Check(LStatus.Field('receipt').Count = 6, 'receipt is a bounded source-free shape');
    LMalformed := LStatus.Field('receipt');
    LRefused := False;
    try
      DecodeNyxBrowserSourceBuild(GSource, NyxObject([
        NyxField('version', LMalformed.Field('version')),
        NyxField('reference', LMalformed.Field('reference')),
        NyxField('state', NyxData('executed')),
        NyxField('artifact', LMalformed.Field('artifact')),
        NyxField('message', LMalformed.Field('message')),
        NyxField('diagnostics', LMalformed.Field('diagnostics'))]));
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'wire receipt cannot forge browser execution');
    LRefused := False;
    try
      DecodeNyxBrowserSourceBuild(GSource, NyxObject([
        NyxField('version', LMalformed.Field('version')),
        NyxField('reference', LMalformed.Field('reference')),
        NyxField('state', LMalformed.Field('state')),
        NyxField('artifact', NyxData('builds/' + LMalformed.Field('reference').AsText + '/substituted.js')),
        NyxField('message', LMalformed.Field('message')),
        NyxField('diagnostics', LMalformed.Field('diagnostics'))]));
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'wire receipt cannot substitute its worker path');
    LRefused := False;
    try
      GEngine.EditorSourceExchange(GToken, NyxWithWorkspace(NyxObject([
        NyxField('compile', NyxObject([NyxField('mode', NyxData('status')),
          NyxField('job', LReceipt.Field('job'))]))]), NyxWorkspace('unrelated-project')));
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'polling cannot fall back from another project to the primary');
    LFailedSource := StringReplace(GSource, 'BuildNyxDocument', 'MissingDocumentBuilder', [rfReplaceAll]);
    Check(LFailedSource <> GSource, 'type failure changes the actual constructor export');
    LReceipt := Exchange(Request('actual-type-failure', LFailedSource, GRevision));
    LStatus := WaitJob(LReceipt.Field('job').AsText);
    LBuild := DecodeNyxBrowserSourceBuild(LFailedSource, LStatus.Field('receipt'));
    Check((LBuild.Projection.State = spsCompilationFailed) and
      (LBuild.Artifact = '') and (LBuild.Projection.Report.Count > 0),
      'actual compiler type failure keeps diagnostics and advertises no worker');
    LAfter := GEngine.EditorExchange(GToken, NyxObject([
      NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
    Check((LAfter.Field('project').AsText = LBefore.Field('project').AsText) and
      (LAfter.Field('session').Field('revision').AsInteger = GRevision),
      'source compilation never publishes, consumes the pending draft or adds history');
    LOtherWorkspace := NyxWorkspace(GEngine.InvokeTool('nyx_workspaces',
      'owned-source-qualification', 'Scooty', NyxObject([
        NyxField('mode', NyxData('create')), NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData('source-other-project')),
        NyxField('label', NyxData('Independent source qualification')),
        NyxField('base', NyxData('empty'))])).Field('workspace').AsText);
    LRefused := False;
    try
      GEngine.EditorSourceExchange(GToken, NyxWithWorkspace(NyxObject([
        NyxField('compile', NyxObject([NyxField('mode', NyxData('status')),
          NyxField('job', LReceipt.Field('job'))]))]), LOtherWorkspace));
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'an existing independent project cannot poll another source job');
    LChangedPair := DecodeNyxProject(LBefore.Field('project').AsText);
    LChangedPair.Draft := LChangedPair.Draft + #10;
    GEngine.EditorExchange(GToken, NyxObject([
      NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LChangedPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    Check(Exchange(LArgs).ToJSON = LOriginalReceipt.ToJSON,
      'exact admitted retry after a new revision never rebuilds');
    Refuse(Request('fresh-old-revision', GSource, GRevision), 'new work refuses the old revised context');
    Check(Exchange(NyxObject([NyxField('mode', NyxData('jobs'))])).Field('active').AsInteger = 0,
      'bounded source metadata confirms terminal producer retirement');
    QueueBudget(LDirectories.RunningIn(LDirectories.RuntimeRoot + 'queue'),
      ExpandFileName(ParamStr(5)), LTools.Field('PAS2JS_RUNTIME').AsText, LPair);
    LeaseExpiry(LDirectories.RunningIn(LDirectories.RuntimeRoot + 'lease'),
      ExpandFileName(ParamStr(5)), LTools.Field('PAS2JS_RUNTIME').AsText, LPair);
    FreeAndNil(GEngine);
    FreeAndNil(LLocal);
    FreeAndNil(LProfile);
    WriteLn('PASS ', GChecks, ' authenticated source service/receipt/shared-queue checks');
  except
    on LException: Exception do
    begin
      GEngine.Free;
      LLocal.Free;
      LProfile.Free;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
