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

program nyx_agent_build_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, {$ifdef PAS2JS}Web,{$endif}
  nyx.text, nyx.data, nyx.studio.agents, nyx.studio.projects, nyx.studio.builds,
  nyx.studio.compiler, nyx.studio.editorbuild, nyx.studio.preview,
  nyx.studio.buildlaunches, nyx.studio.workspaces;

var
  GCount: Integer;
  GSession: TNyxAgentSession;
  GPair: TNyxProjectPair;
  GRevision: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GCount);
end;

procedure Refuse(ARevision: Integer; AScope: TNyxBuildScope;
  const AView, AReason: TNyxText);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    GSession.BuildPair(ARevision, AScope, AView);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
  Check(GSession.Revision = GRevision, 'Refused build retains document revision');
end;

{ Qualify bounded transient execution ownership independently of compiler/UI
  timing. The caller supplies an already admitted successful build; actual
  guarded compiler admission and physical mounting have separate journeys. }
procedure ExerciseLaunchMailbox;
var
  LMailbox: TNyxStudioBuildLaunches;
  LBuild: TNyxDataValue;
  LArguments: TNyxDataValue;
  LOriginal: TNyxDataValue;
  LReply: TNyxDataValue;
  LOwner: TNyxText;
  LOtherOwner: TNyxText;
  LWorkspace: TNyxWorkspaceRef;
  LLaunch: TNyxCompilerLaunch;
  LIndex: Integer;
  LRefused: Boolean;
begin
  LWorkspace := NyxWorkspace('launch-one');
  LOwner := NyxObject([NyxField('owner', NyxData('connection-one')),
    NyxField('workspace', NyxData(LWorkspace.ID))]).ToJSON;
  LOtherOwner := NyxObject([NyxField('owner', NyxData('connection-two')),
    NyxField('workspace', NyxData('launch-two'))]).ToJSON;
  LBuild := NyxObject([NyxField('job', NyxData('successful-job')),
    NyxField('revision', NyxData(7)),
    NyxField('outputID', NyxData('0123456789abcdef0123456789abcdef')),
    NyxField('target', NyxData('browser')), NyxField('scope', NyxData('application')),
    NyxField('view', NyxData(''))]);
  LMailbox := TNyxStudioBuildLaunches.Create;
  try
    LArguments := NyxCompilerLaunch(NyxBuildJob('successful-job'), 7,
      NyxBuildOperation('launch-once'));
    LMailbox.Admit(LArguments);
    LOriginal := LMailbox.Request(LWorkspace, LOwner, 'Scooty', LArguments, LBuild);
    LLaunch := DecodeNyxCompilerLaunch(LOriginal);
    Check((LLaunch.Sequence = 1) and (LLaunch.Target = btBrowser) and
      (LLaunch.Scope = bsApplication), 'Typed launch preserves its exact execution domain');
    Check(LMailbox.Retry(LOwner, LArguments, LReply) and
      (LReply.ToJSON = LOriginal.ToJSON), 'Exact launch retry returns its detached original receipt');
    Check(not LMailbox.Retry(LOtherOwner, LArguments, LReply),
      'Same display actor cannot share another connection retry');
    LRefused := False;
    try
      LMailbox.Retry(LOwner, NyxCompilerLaunch(NyxBuildJob('different-job'), 7,
        NyxBuildOperation('launch-once')), LReply);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Changed arguments refuse an accepted launch operation');
    LReply := LMailbox.Acknowledge(LWorkspace,
      NyxCompilerLaunchResult(LLaunch, btBrowser, clrMounted, 'Ready 🌙'));
    Check(LReply.Field('browser').Field('detail').AsText = TNyxText('Ready 🌙'),
      'Private mount detail preserves supplementary Unicode');
    Check(LOriginal.Field('browser').Kind = ndNull,
      'Later acknowledgment never rewrites an original retry receipt');
    LMailbox.RetireAll;
    Check(LMailbox.Pending(LWorkspace).Field('state').AsText = 'retired',
      'Global retirement includes contexts without observing windows');
    Check(LMailbox.Retry(LOwner, LArguments, LReply) and
      (LMailbox.Pending(LWorkspace).Field('state').AsText = 'retired'),
      'Retry after retirement cannot replay execution');
    LRefused := False;
    try
      LMailbox.Acknowledge(LWorkspace,
        NyxCompilerLaunchResult(LLaunch, btNativeLCL, clrUnavailable));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Retired intent refuses late observer acknowledgment');
    LMailbox.Forget(LWorkspace);
    Check((LMailbox.Pending(LWorkspace).Kind = ndNull) and
      not LMailbox.Retry(LOwner, LArguments, LReply),
      'Closed project releases both its intent and unreachable retries');
    { Fill the receipt budget using two connections. Deleting one connection
      must free its budget while preserving the other connection and intent. }
    for LIndex := 1 to 64 do
    begin
      LArguments := NyxCompilerLaunch(NyxBuildJob('successful-job'), 7,
        NyxBuildOperation('bounded-launch-' + IntToStr(LIndex)));

      if LIndex = 64 then
      begin
        LReply := LMailbox.Request(NyxWorkspace('launch-two'), LOtherOwner,
          'Scooty', LArguments, LBuild);
      end
      else
      begin
        LMailbox.Request(LWorkspace, LOwner, 'Scooty', LArguments, LBuild);
      end;
    end;
    LOriginal := LMailbox.Pending(LWorkspace);
    LRefused := False;
    try
      LMailbox.Request(LWorkspace, LOwner, 'Scooty',
        NyxCompilerLaunch(NyxBuildJob('successful-job'), 7,
          NyxBuildOperation('over-budget')), LBuild);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LMailbox.Pending(LWorkspace).ToJSON = LOriginal.ToJSON),
      'A full launch budget refuses before replacing accepted intent');
    LMailbox.ReleaseOwner('connection-one');
    Check(LMailbox.Retry(LOtherOwner, LArguments, LReply),
      'Deleting one transport retains another connection retry');
    Check(LMailbox.Pending(LWorkspace).ToJSON = LOriginal.ToJSON,
      'Deleting retry ownership does not stop or replace a mounted intent');
    LReply := LMailbox.Request(LWorkspace, LOwner, 'Scooty',
      NyxCompilerLaunch(NyxBuildJob('successful-job'), 7,
        NyxBuildOperation('after-delete')), LBuild);
    Check(LReply.Field('sequence').AsInteger > LOriginal.Field('sequence').AsInteger,
      'Released retry capacity admits a fresh monotonic launch');
    LMailbox.Forget(LWorkspace);
    LMailbox.Forget(NyxWorkspace('launch-two'));
    for LIndex := 1 to 16 do
    begin
      LMailbox.Request(NyxWorkspace('context-' + IntToStr(LIndex)),
        NyxObject([NyxField('owner', NyxData('connection-one')),
          NyxField('workspace', NyxData('context-' + IntToStr(LIndex)))]).ToJSON,
        'Scooty', LArguments, LBuild);
    end;
    LRefused := False;
    try
      LMailbox.Request(NyxWorkspace('context-17'),
        NyxObject([NyxField('owner', NyxData('connection-one')),
          NyxField('workspace', NyxData('context-17'))]).ToJSON,
        'Scooty', LArguments, LBuild);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'A full context budget refuses without evicting another project');
    LMailbox.Forget(NyxWorkspace('context-1'));
    LReply := LMailbox.Request(NyxWorkspace('context-17'),
      NyxObject([NyxField('owner', NyxData('connection-one')),
        NyxField('workspace', NyxData('context-17'))]).ToJSON,
      'Scooty', LArguments, LBuild);
    Check(LReply.Field('state').AsText = 'requested',
      'Confirmed project closure makes its context capacity reusable');
  finally
    LMailbox.Free;
  end;
end;

var
  LPair: TNyxProjectPair;
  LValue: TNyxDataValue;
  LScope: TNyxBuildScope;
  LTarget: TNyxBuildTarget;
  LRefused: Boolean;
  LReport: INyxCompilerReport;
  LOrder: TNyxCompilerDiagnosticIndices;
  LRequest: INyxCompilerRequest;
  LArguments: TNyxDataValue;
  LArtifact: TNyxCompiledArtifact;
  LJobState: TNyxBuildJobState;
begin
  GSession := nil;
  try
    ExerciseLaunchMailbox;
    GSession := TNyxAgentSession.Create;
    GRevision := GSession.Revision;
    GPair := GSession.BuildPair(GRevision, bsApplication, '');
    LPair := GSession.BuildPair(GRevision, bsView, 'home');
    Check(EncodeNyxProject(GPair) = EncodeNyxProject(LPair), 'All scopes capture the complete immutable accepted pair');
    Check(GSession.CurrentPair(GPair), 'Exact source/design currentness');
    LPair := GSession.BuildPair(GRevision, bsReusable, 'welcome-card');
    Check(EncodeNyxProject(LPair) = EncodeNyxProject(GPair), 'Reusable capture preserves full companion frame');
    Refuse(GRevision - 1, bsApplication, '', 'Stale revision refuses');
    Refuse(GRevision, bsApplication, 'home', 'Application refuses a view');
    Refuse(GRevision, bsView, 'welcome-card', 'Page scope refuses reusable root');
    Refuse(GRevision, bsReusable, 'home', 'Reusable scope refuses page root');
    Refuse(GRevision, bsView, 'missing', 'Missing page refuses');
    Refuse(GRevision, bsView, 'welcome-title', 'Descendant is not a page root');
    GSession.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('readOnly'))]));
    Refuse(GRevision, bsView, 'home', 'Read-only cannot launch compilers');
    LPair := GSession.EditorBuildPair(GRevision, bsView, 'home');
    Check(EncodeNyxProject(LPair) = EncodeNyxProject(GPair),
      'Trusted operator capture retains the exact pair with read-only agent access');
    GSession.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('disabled'))]));
    Refuse(GRevision, bsApplication, '', 'Disabled cannot launch compilers');
    LPair := GSession.EditorBuildPair(GRevision, bsApplication, '');
    Check(EncodeNyxProject(LPair) = EncodeNyxProject(GPair),
      'Disabling agents does not disable the operator compiler contract');
    LRefused := False;
    try
      GSession.EditorBuildPair(GRevision - 1, bsView, 'home');
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Private operator admission still refuses a stale revision');
    GSession.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('edit'))]));
    LValue := GSession.Call('nyx_transaction', 'Scooty', NyxObject([
      NyxField('operationId', NyxData('build-currentness')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operations', NyxArray([NyxObject([
        NyxField('op', NyxData('title')), NyxField('value', NyxData('A new application'))])]))]));
    GRevision := LValue.Field('revision').AsInteger;
    Check(not GSession.CurrentPair(GPair), 'Changed design makes the earlier job stale');
    GSession.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('operationId', NyxData('build-currentness-undo')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('direction', NyxData('undo'))]));
    GRevision := GSession.Revision;
    Check(GSession.CurrentPair(GPair), 'Undo restores exact pair independently of monotonic revision');
    LPair := GPair;
    LPair.Pending := True;
    LPair.Draft := GPair.Source + #10 + '// Unaccepted draft';
    LPair.DraftBase := GPair.Source;
    GSession.Exchange(NyxObject([
      NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GRevision := GSession.Revision;
    Refuse(GRevision, bsView, 'home', 'Pending draft refuses a build');
    LRefused := False;
    try
      GSession.EditorBuildPair(GRevision, bsApplication, '');
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Operator authority cannot bypass a pending Pascal draft');
    Check(not GSession.CurrentPair(GPair), 'Pending draft disables current job diagnostics');
    for LScope := Low(TNyxBuildScope) to High(TNyxBuildScope) do
    begin
      Check(ParseNyxBuildScope(NyxBuildScopeName(LScope)) = LScope, 'Closed scope enum round trip');
    end;
    for LTarget := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
    begin
      Check(ParseNyxBuildTarget(NyxBuildTargetName(LTarget)) = LTarget, 'Closed target enum round trip');
    end;
    for LJobState := Low(TNyxBuildJobState) to High(TNyxBuildJobState) do
    begin
      Check(ParseNyxBuildJobState(NyxBuildJobStateName(LJobState)) = LJobState,
        'Closed compiler lifecycle round trip');
      Check(NyxBuildJobTerminal(LJobState) =
        (LJobState in [bjsSucceeded, bjsFailed, bjsCancelled]),
        'Queued and cancelling remain active on both targets');
    end;
    LArguments := NyxCompilerCancel(NyxBuildJob('owned-job'), GRevision,
      NyxBuildOperation('cancel-build'));
    Check((LArguments.Field('mode').AsText = 'cancel') and
      (LArguments.Field('expectedRevision').AsInteger = GRevision) and
      (LArguments.Field('job').AsText = 'owned-job'), 'Typed cancellation packet retains identity and revision');
    LRefused := False;
    try
      ParseNyxBuildTarget('shell');
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Unpublished compiler target refuses');
    LReport := ReadNyxCompilerReport('unit fixture;', 'unit fixture;', 'fixture.pas',
      'fixture.pas(1,1) Warning: first warning' + #10 +
      'fixture.pas(1,1) Note: first note' + #10 +
      'fixture.pas(1,1) Error: actionable error' + #10 +
      'fixture.pas(1,1) Fatal: fatal error' + #10 +
      'fixture.pas(1,1) Warning: second warning');
    LOrder := NyxCompilerDiagnosticOrder(LReport);
    Check((Length(LOrder) = 5) and (LOrder[0] = 2) and (LOrder[1] = 3) and
      (LOrder[2] = 0) and (LOrder[3] = 4) and (LOrder[4] = 1),
      'Bounded presentation prioritizes errors and preserves order within each severity');
    Check(LReport.Item(0).Severity = csWarning, 'Presentation does not mutate the immutable compiler report');
    Check(Length(NyxCompilerDiagnosticOrder(nil)) = 0, 'Absent report has no diagnostic indices');
    LRequest := NewNyxCompilerRequest.Target(btNativeLCL).Scope(bsReusable)
      .Root(NyxBuildRoot('welcome-card')).AtRevision(GRevision)
      .Output(NyxBuildOutput('0123456789abcdef0123456789abcdef'))
      .Operation(NyxBuildOperation('compiled-view'));
    LArguments := LRequest.Arguments;
    Check((LArguments.Field('view').AsText = 'welcome-card') and
      (LArguments.Field('scope').AsText = 'reusable') and
      (LArguments.Field('target').AsText = 'lcl'),
      'Fluent compiler request preserves distinct target, scope and root meanings');
    LRequest.Scope(bsApplication);
    Check((LRequest.Arguments.Count = 6) and (LArguments.Field('view').AsText = 'welcome-card'),
      'Application scope omits its old root; previously captured wire values stay independent');
    Check(NyxCompilerStatus(NyxBuildJob('job-one')).Field('limit').AsInteger = 20,
      'Typed diagnostic paging conforms to the actual service admission limit');
    LValue := NyxObject([NyxField('state', NyxData('succeeded')),
      NyxField('currentSource', NyxData(True)), NyxField('currentOutput', NyxData(True)),
      NyxField('target', NyxData('lcl')), NyxField('job', NyxData('one-job')),
      NyxField('artifact', NyxData('builds/job-01234567-89AB-CDEF-0123-456789ABCDEF/nyx_native.exe')),
      NyxField('manifest', NyxArray([NyxObject([
        NyxField('path', NyxData('builds/job-01234567-89AB-CDEF-0123-456789ABCDEF/nyx_native.exe')),
        NyxField('bytes', NyxData(1234)),
        NyxField('md5', NyxData('0123456789abcdef0123456789abcdef'))])]))]);
    LArtifact := AdmitNyxCompiledArtifact(LValue);
    Check((LArtifact.Target = btNativeLCL) and (LArtifact.ByteCount = 1234),
      'Compiled artifact admission retains typed target and exact byte manifest');
    LRefused := False;
    try
      AdmitNyxCompiledArtifact(TNyxDataValue.ParseJSON(StringReplace(LValue.ToJSON,
        'builds/job-', '../builds/job-', [rfReplaceAll])));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'A matching manifest cannot authorize a traversing artifact path');
    LRefused := False;
    try
      AdmitNyxCompiledArtifact(TNyxDataValue.ParseJSON(StringReplace(LValue.ToJSON,
        '1234', '33554433', [rfReplaceAll])));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'A declared compiler artifact cannot exceed the download budget');
    LRequest := nil;
    LReport := nil;
    GSession.Free;
    GSession := nil;
    WriteLn('PASS ', GCount, ' portable build admission/currentness checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-agent-build-checks', IntToStr(GCount));
    document.body.setAttribute('data-nyx-agent-build-ready', 'passed');
    {$endif}
  except
    on LException: Exception do
    begin
      GSession.Free;
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-agent-build-error', LException.Message);
      {$else}
      Halt(1);
      {$endif}
    end;
  end;
end.
