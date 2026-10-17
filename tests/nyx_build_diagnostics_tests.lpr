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

program nyx_build_diagnostics_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces,{$endif} SysUtils, {$ifdef PAS2JS}JS, Web,{$endif}
  nyx.text, nyx.data, nyx.types, nyx.model, nyx.source,
  nyx.studio.session, nyx.studio.projects, nyx.studio.agents,
  nyx.studio.agentbridge, nyx.studio.agentview, nyx.studio.exchange,
  nyx.studio.workspaces, nyx.studio.editorbuild, nyx.studio.builddiagnostics,
  nyx.studio.builds;

type
  { Closed modeled publication phases keep compiler completion separate from
    paired source admission. They exercise observer routing, not provenance. }
  TFixturePublication = (fpAbsent, fpPending, fpCommitted);

  { Deterministic deferred transport seam. The real session/bridge owns frame
    and queue admission; this fixture controls only delivery order and replies.
    No listener, compiler, protected project or browser automation is involved. }
  TDeferredExchange = class(TNyxStudioEditorExchange)
  private
    FPeer: TNyxAgentSession;
    FBody: TNyxDataValue;
    FReply: TNyxEditorReply;
    FTick: TNyxEditorTick;
    FConnect: Boolean;
  public
    WrongJob: Boolean;
    CompilerState: TNyxBuildJobState;
    Publication: TFixturePublication;
    BuildQueries: Integer;
    Observations: Integer;
    constructor Create(APeer: TNyxAgentSession);
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
    procedure Deliver;
    procedure Advance;
  end;

  { Counts actual bridge refresh deliveries, retaining no session or widgets.
    Its lifetime encloses the bridge's borrowed callback and pending transport. }
  TRefreshCounter = class
  public
    Calls: Integer;
    procedure Refreshed(ADocumentChanged: Boolean);
  end;

procedure TRefreshCounter.Refreshed(ADocumentChanged: Boolean);
begin
  Inc(Calls);
end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Changed(const AValue: TNyxDataValue; const AName: TNyxText;
  const AReplacement: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
  LFound: Boolean;
begin
  LCount := AValue.Count;
  SetLength(LFields, LCount + 1);
  LFound := False;
  for LIndex := 0 to LCount - 1 do
  begin

    if AValue.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AReplacement);
      LFound := True;
    end
    else
    begin
      LFields[LIndex] := NyxField(AValue.Key(LIndex), AValue.Field(AValue.Key(LIndex)));
    end;
  end;

  if LFound then
  begin
    SetLength(LFields, LCount);
  end
  else
  begin
    LFields[LCount] := NyxField(AName, AReplacement);
  end;
  Result := NyxObject(LFields);
end;

{ Modeled immutable producer pages exercise source/page admission independently
  of the real compiler journey. Their status does not establish compilation. }
function Status(const AJob: TNyxText; AOffset: Integer): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LCount: Integer;
  LIndex: Integer;
begin
  LCount := 22 - AOffset;

  if LCount > 20 then
  begin
    LCount := 20;
  end;
  SetLength(LItems, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LItems[LIndex] := NyxObject([
      NyxField('file', NyxData('workshop.pas')),
      NyxField('severity', NyxData('warning')),
      NyxField('message', NyxData('A careful compiler hint.')),
      NyxField('line', NyxData(1)), NyxField('column', NyxData(1)),
      NyxField('navigable', NyxData(True))]);
  end;
  Result := NyxObject([
    NyxField('job', NyxData(AJob)), NyxField('state', NyxData('succeeded')),
    NyxField('currentSource', NyxData(True)), NyxField('error', NyxData('')),
    NyxField('diagnostics', NyxObject([
      NyxField('offset', NyxData(AOffset)), NyxField('total', NyxData(22)),
      NyxField('items', NyxArray(LItems))]))]);
end;

constructor TDeferredExchange.Create(APeer: TNyxAgentSession);
begin
  inherited Create;
  FPeer := APeer;
  CompilerState := bjsSucceeded;
end;

procedure TDeferredExchange.Post(AConnect: Boolean; const AToken, ABody: TNyxText;
  AReply: TNyxEditorReply);
begin
  Check(not Assigned(FReply), 'one deferred request owns its borrowed reply');
  FBody := TNyxDataValue.ParseJSON(ABody);

  if FBody.Field('op').AsText = 'build' then
  begin
    Inc(BuildQueries);
  end
  else if FBody.Field('op').AsText = 'observe' then
  begin
    Inc(Observations);
  end;
  FReply := AReply;
  FConnect := AConnect;
end;

procedure TDeferredExchange.CancelRequest;
begin
  FReply := nil;
end;

procedure TDeferredExchange.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin
  FTick := ATick;
end;

procedure TDeferredExchange.CancelTick;
begin
  FTick := nil;
end;

procedure TDeferredExchange.Advance;
var
  LTick: TNyxEditorTick;
begin
  LTick := FTick;
  FTick := nil;

  if Assigned(LTick) then
  begin
    LTick;
  end;
end;

procedure TDeferredExchange.Deliver;
var
  LReply: TNyxEditorReply;
  LState: TNyxDataValue;
  LBuild: TNyxDataValue;
  LJob: TNyxText;
  LReplyStatus: TNyxDataValue;
  LPublication: TNyxText;
begin
  Check(Assigned(FReply), 'an actual deferred request exists before delivery');
  LReply := FReply;
  FReply := nil;

  if FBody.Field('op').AsText = 'build' then
  begin
    LBuild := FBody.Field('build');
    Check(LBuild.Field('mode').AsText = 'status',
      'inspection uses the existing bounded compiler status packet');
    LJob := LBuild.Field('job').AsText;

    if WrongJob then
    begin
      LJob := 'another-job';
    end;
    LState := FPeer.Exchange(NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', FBody.Field('after'))]));
    LReplyStatus := Changed(Status(LJob, LBuild.Field('offset').AsInteger), 'state',
      NyxData(NyxBuildJobStateName(CompilerState)));

    if CompilerState = bjsCancelled then
    begin
      LReplyStatus := Changed(LReplyStatus, 'diagnostics', NyxObject([
        NyxField('offset', LBuild.Field('offset')), NyxField('total', NyxData(0)),
        NyxField('items', NyxArray([]))]));
    end;

    if Publication <> fpAbsent then
    begin
      LPublication := 'pending';

      if Publication = fpCommitted then
      begin
        LPublication := 'committed';
      end;
      LReplyStatus := Changed(LReplyStatus, 'purpose', NyxData('transaction'));
      LReplyStatus := Changed(LReplyStatus, 'publication', NyxObject([
        NyxField('state', NyxData(LPublication)), NyxField('revision', NyxData(1))]));
    end;
    LState := Changed(LState, 'buildReply', LReplyStatus);
  end
  else
  begin
    LState := FPeer.Exchange(FBody);
  end;
  LState := Changed(LState, 'editorBuilds', NyxData(True));
  LState := Changed(LState, 'buildJobControl', NyxData(True));

  if FConnect then
  begin
    LState := NyxObject([NyxField('token', NyxData('private-fixture')),
      NyxField('endpoint', NyxData('')), NyxField('state', LState)]);
  end;
  LReply(200, LState.ToJSON);
end;

procedure Run;
var
  LSession: TNyxStudioSession;
  LPeer: TNyxAgentSession;
  LBridge: TNyxStudioAgentBridge;
  LExchange: TDeferredExchange;
  LInspection: TNyxBuildDiagnosticInspection;
  LSecond: TNyxBuildDiagnosticInspection;
  LProposed: TNyxDataValue;
  LBad: TNyxDataValue;
  LPanel: TNyxNode;
  LAction: TNyxNode;
  LDiagnostic: TNyxSourceDiagnostic;
  LBefore: TNyxText;
  LRejected: Boolean;
  LSequence: Integer;
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LQueries: Integer;
  LObservations: Integer;
  LCallbacks: Integer;
  LRefreshCounter: TRefreshCounter;

  procedure Refuses(const AValue: TNyxDataValue; const AReason: TNyxText);
  begin
    LRejected := False;
    try
      ReadNyxBuildDiagnostics(AValue, NyxBuildJob('first-job'), 0, LSession.Source, True);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, AReason);
  end;

begin
  LSession := TNyxStudioSession.Create;
  LPeer := TNyxAgentSession.Create;
  LBridge := nil;
  LPanel := nil;
  LRefreshCounter := TRefreshCounter.Create;
  try
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(DefaultNyxStudioAgentView.BuildInspection.Phase = bdpHidden,
      'default observer state has no selected diagnostic inspection');
    LInspection := ReadNyxBuildDiagnostics(Status('first-job', 0),
      NyxBuildJob('first-job'), 0, LSession.Source, True);
    Check((LInspection.Phase = bdpReady) and (LInspection.Origin = bdoAccepted) and
      (LInspection.Count = 20) and (LInspection.Total = 22),
      'bounded accepted inspection retains only its immutable first page');
    LPanel := BuildNyxStudioBuildDiagnostics(LSession, LInspection, True, True);
    LAction := LPanel.Find('studio-job-diagnostic-go-0');
    Check((LAction <> nil) and (LAction.Prop('enabled') = 'true'),
      'exact accepted synchronized source enables its mapped location');
    Check(RouteNyxBuildDiagnostic(LSession, LAction, ntClick, LInspection, True,
      LDiagnostic) and LDiagnostic.Defined and (LDiagnostic.Line = 1),
      'source navigation routes the exact mapped value without editing');
    LBad := Changed(Status('first-job', 0), 'state', NyxData('running'));
    SetLength(LItems, LBad.Field('diagnostics').Field('items').Count);
    for LIndex := 0 to High(LItems) do
    begin
      LItems[LIndex] := LBad.Field('diagnostics').Field('items').Item(LIndex);
    end;
    LItems[0] := Changed(LItems[0], 'message', NyxData('A later live compiler message.'));
    LItems[0] := Changed(LItems[0], 'column', NyxData(2));
    LBad := Changed(LBad, 'diagnostics', Changed(LBad.Field('diagnostics'),
      'items', NyxArray(LItems)));
    LSecond := ReadNyxBuildDiagnostics(LBad, NyxBuildJob('first-job'), 0,
      LSession.Source, True);
    LRejected := False;
    try
      RouteNyxBuildDiagnostic(LSession, LAction, ntClick, LSecond, True, LDiagnostic);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LDiagnostic.Defined,
      'an older row cannot navigate a changed message at the same running job/page/index');
    Check((LPanel.Find('studio-job-diagnostics-next').Extensions
      .Value(NyxStudioInspectOffsetKey).AsInteger = 20) and
      (LPanel.Find('studio-job-diagnostics-next').Extensions
      .Value(NyxStudioInspectBuildKey).AsText = 'first-job'),
      'next page retains exact job identity and bounded progress');
    LSecond := ReadNyxBuildDiagnostics(Status('first-job', 20),
      NyxBuildJob('first-job'), 20, LSession.Source, True);
    LRejected := False;
    try
      RouteNyxBuildDiagnostic(LSession, LAction, ntClick, LSecond, True, LDiagnostic);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'an old first-page action cannot navigate the second page');
    LSession.SetSourceDraft(LSession.Source + #10 + '// A retained draft.');
    LRejected := False;
    try
      RouteNyxBuildDiagnostic(LSession, LAction, ntClick, LInspection, True, LDiagnostic);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (Pos('// A retained draft.', LSession.DraftSource) > 0),
      'a newer draft refuses navigation without losing that text');
    LSession.SetSourceDraft(LSession.Source);
    LProposed := Changed(Status('first-job', 0), 'purpose', NyxData('transaction'));
    LProposed := Changed(LProposed, 'publication', NyxObject([
      NyxField('state', NyxData('refused')), NyxField('revision', NyxNull)]));
    LInspection := ReadNyxBuildDiagnostics(LProposed, NyxBuildJob('first-job'), 0,
      LSession.Source, True);
    Check(LInspection.Origin = bdoProposed,
      'even a forged current-source flag cannot turn an unapplied proposal into accepted source');
    Check(not LInspection.AwaitingPublication,
      'a refused source proposal is settled independently of compiler state');
    LBad := Changed(LProposed, 'publication', NyxObject([
      NyxField('state', NyxData('pending')), NyxField('revision', NyxNull)]));
    LSecond := ReadNyxBuildDiagnostics(LBad, NyxBuildJob('first-job'), 0,
      LSession.Source, True);
    Check(LSecond.AwaitingPublication,
      'compiler success alone does not finish an outstanding paired source publication');
    FreeAndNil(LPanel);
    LPanel := BuildNyxStudioBuildDiagnostics(LSession, LInspection, True, True);
    Check(LPanel.Find('studio-job-diagnostic-go-0').Prop('enabled') = 'false',
      'proposed-source locations visibly refuse accepted-editor navigation');
    Refuses(Status('another-job', 0), 'a different immutable job refuses');
    Refuses(Status('first-job', 20), 'a different diagnostic page refuses');
    LBad := Status('first-job', 0);
    LProposed := Changed(LBad.Field('diagnostics').Field('items').Item(0),
      'mapped', NyxData(True));
    LBad := Changed(LBad, 'diagnostics', NyxObject([
      NyxField('offset', NyxData(0)), NyxField('total', NyxData(1)),
      NyxField('items', NyxArray([LProposed]))]));
    Refuses(LBad, 'internal compiler-pool mapping cannot enter authenticated editor status');
    LProposed := Changed(Status('first-job', 0), 'purpose', NyxData('transaction'));
    LProposed := Changed(LProposed, 'publication', NyxObject([
      NyxField('state', NyxData('refused')), NyxField('revision', NyxNull)]));
    LBad := Changed(LProposed, 'state', NyxData('failed'));
    LBad := Changed(LBad, 'publication', NyxObject([
      NyxField('state', NyxData('committed')), NyxField('revision', NyxData(1))]));
    Refuses(LBad, 'a failed compiler cannot claim committed diagnostic origin');
    LBad := Status('first-job', 0);
    LBad := Changed(LBad, 'diagnostics', NyxObject([
      NyxField('offset', NyxData(0)), NyxField('total', NyxData(1)),
      NyxField('items', NyxArray([]))]));
    Refuses(LBad, 'a remaining diagnostic page cannot silently make zero progress');
    LBad := Status('first-job', 0);
    LProposed := LBad.Field('diagnostics').Field('items').Item(0);
    SetLength(LItems, 21);
    for LIndex := 0 to High(LItems) do
    begin
      LItems[LIndex] := LProposed.Copy;
    end;
    LBad := Changed(LBad, 'diagnostics', NyxObject([
      NyxField('offset', NyxData(0)), NyxField('total', NyxData(21)),
      NyxField('items', NyxArray(LItems))]));
    Refuses(LBad, 'oversized diagnostic pages refuse without entering presentation');
    LBad := Status('first-job', 0);
    LProposed := Changed(LProposed, 'severity', NyxData('future'));
    LBad := Changed(LBad, 'diagnostics', NyxObject([
      NyxField('offset', NyxData(0)), NyxField('total', NyxData(1)),
      NyxField('items', NyxArray([LProposed]))]));
    Refuses(LBad, 'unknown compiler severity refuses at the explicit wire boundary');
    LProposed := Changed(LProposed, 'severity', NyxData('warning'));
    LProposed := Changed(LProposed, 'line', NyxData(0));
    LBad := Changed(LBad, 'diagnostics', NyxObject([
      NyxField('offset', NyxData(0)), NyxField('total', NyxData(1)),
      NyxField('items', NyxArray([LProposed]))]));
    Refuses(LBad, 'mapped locations require positive source coordinates');
    LSecond := ReadNyxBuildDiagnostics(Status('first-job', 0),
      NyxBuildJob('first-job'), 0, LSession.Source, False);
    Check(LSecond.Origin = bdoEarlier,
      'status currentness alone cannot borrow an unacknowledged local source');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore,
      'inspection/navigation/refusal preserve the entire accepted pair and history');

    LExchange := TDeferredExchange.Create(LPeer);
    LBridge := TNyxStudioAgentBridge.Create(LSession,
      {$ifdef PAS2JS}@{$endif}LRefreshCounter.Refreshed, NyxPrimaryWorkspace, LExchange);
    LBridge.Connect;
    LExchange.Deliver;
    Check(LBridge.CanInspectBuild and LBridge.SourceSynchronized,
      'ordinary bridge negotiates independent read-only inspection');
    LBridge.BuildStatus(NyxBuildJob('workflow-job'));
    LExchange.Advance;
    LBridge.InspectBuild(NyxBuildJob('first-job'));
    LExchange.Deliver;
    LSequence := LBridge.State.BuildReplySequence;
    Check((LSequence = 1) and (LBridge.State.BuildReply.Field('job').AsText = 'workflow-job'),
      'application status retains its own reply destination');
    LExchange.Advance;
    LExchange.Deliver;
    Check((LBridge.State.BuildInspection.Phase = bdpReady) and
      (LBridge.State.BuildInspection.Job.ID = 'first-job') and
      (LBridge.State.BuildReplySequence = LSequence),
      'inspection does not steal or advance the application workflow reply');
    LBridge.InspectBuild(NyxBuildJob('first-job'));
    LBridge.CloseBuildInspection;
    LExchange.Deliver;
    Check(LBridge.State.BuildInspection.Phase = bdpHidden,
      'closing the selected report makes its late reply inert');
    LBridge.InspectBuild(NyxBuildJob('first-job'));
    LBridge.InspectBuild(NyxBuildJob('second-job'));
    LExchange.Deliver;
    Check((LBridge.State.BuildInspection.Phase = bdpLoading) and
      (LBridge.State.BuildInspection.Job.ID = 'second-job'),
      'an older reply cannot replace a newer selected job');
    LExchange.Advance;
    LExchange.Deliver;
    Check((LBridge.State.BuildInspection.Phase = bdpReady) and
      (LBridge.State.BuildInspection.Job.ID = 'second-job'),
      'the newer request retains its own queued destination and generation');
    LExchange.CompilerState := bjsRunning;
    LBridge.InspectBuild(NyxBuildJob('first-job'));
    LExchange.Deliver;
    LQueries := LExchange.BuildQueries;
    LSequence := LBridge.State.BuildReplySequence;
    LCallbacks := LRefreshCounter.Calls;
    LExchange.Advance;
    Check(LExchange.BuildQueries = LQueries + 1,
      'the selected running page refreshes on an ordinary idle turn');
    LExchange.Deliver;
    Check((LRefreshCounter.Calls = LCallbacks) and
      (LBridge.State.BuildReplySequence = LSequence),
      'an unchanged live page neither redraws nor steals a workflow reply');
    LExchange.Advance;
    LExchange.Deliver;
    LExchange.Advance;
    LBridge.BuildStatus(NyxBuildJob('workflow-during-live-poll'));
    LExchange.Deliver;
    Check(LBridge.State.BuildReplySequence = LSequence,
      'inspection acknowledges its own poll before a later queued workflow');
    LExchange.Advance;
    LExchange.Deliver;
    Check((LBridge.State.BuildReplySequence = LSequence + 1) and
      (LBridge.State.BuildReply.Field('job').AsText = 'workflow-during-live-poll'),
      'a workflow queued during inspection retains its own exact acknowledgement');
    LObservations := LExchange.Observations;
    LExchange.Advance;
    Check(LExchange.Observations = LObservations + 1,
      'ordinary observation receives an idle turn between live diagnostic polls');
    LExchange.Deliver;
    LExchange.CompilerState := bjsSucceeded;
    LExchange.Advance;
    LExchange.Deliver;
    Check(LBridge.State.BuildInspection.State = bjsSucceeded,
      'automatic inspection admits the final same-job page');
    LQueries := LExchange.BuildQueries;
    LExchange.Advance;
    LExchange.Deliver;
    LExchange.Advance;
    LExchange.Deliver;
    Check(LExchange.BuildQueries = LQueries,
      'terminal pages stop automatic compiler requests while observing continues');
    LExchange.Publication := fpPending;
    LBridge.InspectBuild(NyxBuildJob('first-job'));
    LExchange.Deliver;
    LQueries := LExchange.BuildQueries;
    LExchange.Advance;
    Check(LExchange.BuildQueries = LQueries + 1,
      'finished compilation keeps observing its still-pending paired publication');
    LExchange.Publication := fpCommitted;
    LExchange.Deliver;
    Check(not LBridge.State.BuildInspection.AwaitingPublication and
      (LBridge.State.BuildInspection.Origin = bdoAccepted),
      'settled publication requires a fresh admitted same-job response');
    LExchange.Publication := fpAbsent;
    LExchange.CompilerState := bjsRunning;
    LBridge.InspectBuild(NyxBuildJob('first-job'), 20);
    LExchange.Deliver;
    Check(LBridge.State.BuildInspection.Offset = 20,
      'a live report can retain an explicitly selected later page');
    LExchange.CompilerState := bjsCancelled;
    LExchange.Advance;
    LExchange.Deliver;
    Check(not LBridge.State.Conflict and
      (LBridge.State.BuildInspection.State = bjsCancelled) and
      (LBridge.State.BuildInspection.Offset = 0) and
      (LBridge.State.BuildInspection.Count = 0) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'a cleared cancelled report retires a later page without freezing or replacing work');
    LExchange.CompilerState := bjsSucceeded;
    LBridge.InspectBuild(NyxBuildJob('first-job'));
    LExchange.WrongJob := True;
    LExchange.Deliver;
    Check(LBridge.State.Conflict and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'a substituted job reply refuses without replacing local work');
  finally
    LPanel.Free;
    LBridge.Free;
    LRefreshCounter.Free;
    LPeer.Free;
    LSession.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-job-diagnostics', 'passed');
    document.body.setAttribute('data-nyx-job-diagnostic-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' retained job diagnostic ownership checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-job-diagnostics', 'failed');
      document.body.setAttribute('data-nyx-job-diagnostic-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
