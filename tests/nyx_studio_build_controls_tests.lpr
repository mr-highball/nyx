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

program nyx_studio_build_controls_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.studio.agents,
  nyx.studio.browser, nyx.studio.exchange, nyx.studio.editorbuild,
  nyx.studio.builds;

type
  { Script only the compiler service seam. The real portable editor session
    admits/observes pairs and the ordinary browser Studio handles actual buttons,
    timers and replies. No listener, protected document or real job is changed.
    Actual worker joining is separately qualified by the native control journey. }
  TCompilerExchange = class(TNyxStudioEditorExchange)
  private
    FReply: TNyxEditorReply;
    FTick: TNyxEditorTick;
    FBody: TNyxDataValue;
    FConnect: Boolean;
    FRequestTimer: NativeInt;
    FTickTimer: NativeInt;
    procedure Deliver;
    procedure Tick;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
  end;

  TBuildStudio = class(TNyxStudio)
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;

var
  GStudio: TBuildStudio;
  GSession: TNyxAgentSession;
  GPhase: Integer;
  GChecks: Integer;
  GPolls: Integer;
  GRequests: Integer;
  GCancels: Integer;
  GStatuses: Integer;
  GState: TNyxBuildJobState;
  GMode: TNyxBuildJobState;
  GJob: TNyxText;
  GCancelJob: TNyxText;
  GPair: TNyxText;
  GReport: TNyxText;
  GPreview: TNyxText;
  GOldStatus: TNyxText;
  GStatusJob: TNyxText;
  GCancelRevision: Integer;
  GCancelRefuse: Boolean;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
end;

procedure Click(const AID: TNyxText);
begin

  if Find(AID) = nil then
  begin
    raise Exception.Create('Missing ordinary build control: ' + AID);
  end;
  Find(AID).click;
end;

function WithFields(const AValue: TNyxDataValue;
  const AFields: array of TNyxDataField): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
begin
  { Capture the record getter before index arithmetic: this pas2js revision
    omits its call in a complex array index. Native and browser share this path. }
  LCount := AValue.Count;
  SetLength(LFields, LCount + Length(AFields));
  for LIndex := 0 to LCount - 1 do
  begin
    LFields[LIndex] := NyxField(AValue.Key(LIndex), AValue.Field(AValue.Key(LIndex)));
  end;
  for LIndex := 0 to High(AFields) do
  begin
    LFields[LCount + LIndex] := AFields[LIndex];
  end;
  Result := NyxObject(LFields);
end;

function JobItem(const AJob: TNyxText; AState: TNyxBuildJobState;
  ACancel: Boolean): TNyxDataValue;
begin
  Result := NyxObject([NyxField('job', NyxData(AJob)),
    NyxField('actor', NyxData('English build workshop')),
    NyxField('state', NyxData(NyxBuildJobStateName(AState))),
    NyxField('failure', NyxData('none')), NyxField('revision', NyxData(GSession.Revision)),
    NyxField('target', NyxData('browser')), NyxField('scope', NyxData('application')),
    NyxField('view', NyxData('')), NyxField('currentSource', NyxData(True)),
    NyxField('currentOutput', NyxData(True)), NyxField('canCancel', NyxData(ACancel))]);
end;

function Jobs: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LQueued: Integer;
  LCancelling: Integer;
begin
  LQueued := 0;
  LCancelling := 0;
  SetLength(LItems, 2);
  LItems[0] := JobItem('other-job-one', bjsRunning, True);
  LItems[1] := JobItem('other-job-two', bjsRunning, True);

  if (GJob <> '') and not NyxBuildJobTerminal(GState) then
  begin
    SetLength(LItems, 3);
    LItems[2] := JobItem(GJob, GState, GState in [bjsQueued, bjsRunning]);

    if GState = bjsQueued then
    begin
      LQueued := 1;
    end;

    if GState = bjsCancelling then
    begin
      LCancelling := 1;
    end;
  end;
  Result := NyxObject([NyxField('offset', NyxData(0)),
    NyxField('total', NyxData(Length(LItems))), NyxField('running', NyxData(2)),
    NyxField('queued', NyxData(LQueued)), NyxField('cancelling', NyxData(LCancelling)),
    NyxField('items', NyxArray(LItems))]);
end;

function BuildReply(const ABuild: TNyxDataValue): TNyxDataValue;
var
  LArtifact: TNyxText;
begin
  case ParseNyxCompilerOperation(ABuild.Field('mode').AsText) of
    coOutputs:
      begin
        Result := NyxObject([NyxField('outputID', NyxData('0123456789abcdef0123456789abcdef')),
          NyxField('outputs', NyxArray([NyxObject([
            NyxField('target', NyxData('browser')), NyxField('ready', NyxData(True)),
            NyxField('issue', NyxData(''))])]))]);
      end;
    coRequest:
      begin
        Check(ABuild.Field('expectedRevision').AsInteger = GSession.Revision,
          'ordinary browser build captures its acknowledged revision');
        Check((ABuild.Field('scope').AsText = 'application') and
          (ABuild.Field('target').AsText = 'browser'),
          'ordinary build uses the typed application request');
        Inc(GRequests);
        GJob := 'browser-job-' + IntToStr(GRequests);
        GState := GMode;
        GStatuses := 0;
        Result := NyxObject([NyxField('job', NyxData(GJob)),
          NyxField('state', NyxData(NyxBuildJobStateName(GState)))]);
      end;
    coCancel:
      begin
        Inc(GCancels);
        GCancelJob := ABuild.Field('job').AsText;
        GCancelRevision := ABuild.Field('expectedRevision').AsInteger;

        if GCancelRefuse then
        begin
          Result := NyxObject([NyxField('state', NyxData('rejected')),
            NyxField('error', NyxData('Cancellation revision changed; build retained'))]);
          GCancelRefuse := False;
        end
        else
        begin
          Check(GCancelJob = GJob, 'cancelled row addresses its exact immutable job');
          GState := bjsCancelling;
          GStatuses := 0;
          Result := NyxObject([NyxField('job', NyxData(GJob)),
            NyxField('state', NyxData('cancelling'))]);
        end;
      end;
    coStatus:
      begin
        { Check every reply, but count each immutable job once. Timer frequency
          must not inflate the reported qualification total. }

        if GStatusJob <> GJob then
        begin
          Check(ABuild.Field('job').AsText = GJob,
            'cancellation acknowledgment never retargets the pending status poll');
          GStatusJob := GJob;
        end
        else if ABuild.Field('job').AsText <> GJob then
        begin
          raise Exception.Create('Status poll changed its immutable job');
        end;
        Inc(GStatuses);

        if (GState = bjsCancelling) and (GStatuses >= 2) then
        begin
          GState := bjsCancelled;
        end;
        LArtifact := '';

        if GState = bjsSucceeded then
        begin
          LArtifact := 'builds/job-01234567-89AB-CDEF-0123-456789ABCDEF/index.html';
        end;
        Result := NyxObject([NyxField('job', NyxData(GJob)),
          NyxField('state', NyxData(NyxBuildJobStateName(GState))),
          NyxField('currentSource', NyxData(True)), NyxField('currentOutput', NyxData(True)),
          NyxField('target', NyxData('browser')), NyxField('scope', NyxData('application')),
          NyxField('error', NyxData('')), NyxField('artifact', NyxData(LArtifact)),
          NyxField('manifest', NyxArray([NyxObject([
            NyxField('path', NyxData(LArtifact)), NyxField('bytes', NyxData(12)),
            NyxField('md5', NyxData('0123456789abcdef0123456789abcdef'))])]))]);
      end;
    else
      begin
        raise Exception.Create('Unexpected compiler operation in ordinary browser journey');
      end;
  end;
end;

constructor TCompilerExchange.Create;
begin
  inherited Create;
  FRequestTimer := -1;
  FTickTimer := -1;
end;

destructor TCompilerExchange.Destroy;
begin
  CancelRequest;
  CancelTick;
  inherited Destroy;
end;

procedure TCompilerExchange.Post(AConnect: Boolean; const AToken, ABody: TNyxText;
  AReply: TNyxEditorReply);
begin
  CancelRequest;
  FBody := TNyxDataValue.ParseJSON(ABody);
  FConnect := AConnect;
  FReply := AReply;
  FRequestTimer := window.setTimeout(@Deliver, 1);
end;

procedure TCompilerExchange.Deliver;
var
  LResult: TNyxDataValue;
  LBuild: TNyxDataValue;
  LReply: TNyxEditorReply;
begin
  FRequestTimer := -1;
  LReply := FReply;
  FReply := nil;
  try
    LBuild := NyxNull;

    if FBody.Field('op').AsText = 'build' then
    begin
      LBuild := BuildReply(FBody.Field('build'));
      LResult := GSession.Exchange(NyxObject([NyxField('op', NyxData('observe')),
        NyxField('after', FBody.Field('after'))]));
    end
    else
    begin
      LResult := GSession.Exchange(FBody);
    end;
    LResult := WithFields(LResult, [NyxField('editorBuilds', NyxData(True)),
      NyxField('buildJobControl', NyxData(True)), NyxField('buildJobs', Jobs)]);

    if LBuild.Kind <> ndNull then
    begin
      LResult := WithFields(LResult, [NyxField('buildReply', LBuild)]);
    end;

    if FConnect then
    begin
      LResult := NyxObject([NyxField('token', NyxData('private-fixture')),
        NyxField('endpoint', NyxData('')), NyxField('state', LResult)]);
    end;

    if Assigned(LReply) then
    begin
      LReply(200, LResult.ToJSON);
    end;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-build-exchange-error', LException.Message);

      if Assigned(LReply) then
      begin
        LReply(409, NyxObject([NyxField('error', NyxData(TNyxText(LException.Message)))]).ToJSON);
      end;
    end;
    on LHostError: TJSError do
    begin
      document.body.setAttribute('data-nyx-build-exchange-error',
        LHostError.message);
    end;
  end;
end;

procedure TCompilerExchange.CancelRequest;
begin

  if FRequestTimer >= 0 then
  begin
    window.clearTimeout(FRequestTimer);
  end;
  FRequestTimer := -1;
  FReply := nil;
end;

procedure TCompilerExchange.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin
  CancelTick;
  FTick := ATick;
  FTickTimer := window.setTimeout(@Tick, ADelayMS);
end;

procedure TCompilerExchange.CancelTick;
begin

  if FTickTimer >= 0 then
  begin
    window.clearTimeout(FTickTimer);
  end;
  FTickTimer := -1;
  FTick := nil;
end;

procedure TCompilerExchange.Tick;
var
  LTick: TNyxEditorTick;
begin
  FTickTimer := -1;
  LTick := FTick;
  FTick := nil;

  if Assigned(LTick) then
  begin
    LTick;
  end;
end;

function TBuildStudio.CreateEditorExchange: TNyxStudioEditorExchange;
begin
  Result := TCompilerExchange.Create;
end;

procedure Poll;
var
  LStatus: TNyxText;
  LFrame: TJSHTMLIFrameElement;
begin
  try
    Inc(GPolls);

    if GPolls > 1200 then
    begin
      raise Exception.Create('Browser build controls timed out at phase ' + IntToStr(GPhase));
    end;
    LStatus := '';

    if Find('studio-status') <> nil then
    begin
      LStatus := Find('studio-status').textContent;
    end;
    case GPhase of
      0:
        begin

          if Find('action-builds') <> nil then
          begin
            Click('action-outputs');
            Click('output-browser');
            Click('action-code');
            GPair := GSession.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
              .Field('project').AsText;
            GMode := bjsSucceeded;
            Click('action-build-app');
            GPhase := 1;
          end;
        end;
      1:
        begin
          LFrame := TJSHTMLIFrameElement(document.querySelector('.nyx-compiled-preview'));

          if (LFrame <> nil) and (GStatuses > 0) then
          begin
            GPreview := LFrame.src;
            Check(Pos('job-01234567', GPreview) > 0, 'accepted browser artifact reaches the ordinary preview');
            GReport := GSession.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
              .Field('compiler').ToJSON;
            Click('action-builds');
            GMode := bjsQueued;
            Click('action-build-app');
            GPhase := 2;
          end;
        end;
      2:
        begin

          if (GState = bjsQueued) and (Find('studio-build-2-cancel') <> nil) and
            not TJSHTMLButtonElement(Find('studio-build-2-cancel')).disabled then
          begin
            Check(Pos('1 queued', Find('studio-builds-summary').textContent) > 0,
              'ordinary browser panel exposes queue counts');
            Check(Pos('Queued', Find('studio-build-2-title').textContent) > 0,
              'queued state is visible beside the exact target/scope');
            GCancelRefuse := True;
            Click('studio-build-2-cancel');
            GPhase := 3;
          end;
        end;
      3:
        begin

          if (GCancels = 1) and (Pos('Cancellation revision changed', LStatus) > 0) then
          begin
            Check(GState = bjsQueued, 'rejected cancel leaves the pending request intact');
            GPhase := 4;
          end;
        end;
      4:
        begin

          if not TJSHTMLButtonElement(Find('studio-build-2-cancel')).disabled then
          begin
            Click('studio-build-2-cancel');
            GPhase := 5;
          end;
        end;
      5:
        begin

          if (GState = bjsCancelling) and (Find('studio-build-2-title') <> nil) then
          begin
            Check(TJSHTMLButtonElement(Find('studio-build-2-cancel')).disabled,
              'cancelling row disables duplicate retirement actions');
            Check(GCancelRevision = GSession.Revision, 'cancel uses the synchronized current revision');
            GPhase := 6;
          end;
        end;
      6:
        begin

          if (GState = bjsCancelled) and (Pos('Build cancelled', LStatus) > 0) then
          begin
            Check(Find('studio-build-2-cancel') = nil, 'joined cancelled job leaves the active panel');
            Check(TJSHTMLIFrameElement(document.querySelector('.nyx-compiled-preview')).src = GPreview,
              'cancelling another job retains the last accepted preview');
            GMode := bjsRunning;
            Click('action-build-app');
            GPhase := 7;
          end;
        end;
      7:
        begin

          if (GState = bjsRunning) and (Find('studio-build-2-cancel') <> nil) and
            not TJSHTMLButtonElement(Find('studio-build-2-cancel')).disabled then
          begin
            Check(Pos('Compiling', Find('studio-build-2-title').textContent) > 0,
              'running compiler has a visible ordinary cancel control');
            Click('studio-build-2-cancel');
            GPhase := 8;
          end;
        end;
      8:
        begin

          if (GState = bjsCancelled) and (Pos('Build cancelled', LStatus) > 0) then
          begin
            Check(GSession.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
              .Field('project').AsText = GPair, 'build/cancel interaction preserves the exact accepted project');
            Check(GSession.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
              .Field('compiler').ToJSON = GReport, 'cancellation retains the previous compiler report');
            Check(Find('studio-agent-conflict') = nil, 'build panel leaves the editor unconflicted');
            document.body.setAttribute('data-nyx-build-controls', 'passed');
            document.body.setAttribute('data-nyx-build-checks', IntToStr(GChecks));
            Exit;
          end;
        end;
    end;
    GOldStatus := LStatus;
    window.setTimeout(@Poll, 10);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-build-controls', 'failed');
      document.body.setAttribute('data-nyx-build-error', LException.Message);
      document.body.setAttribute('data-nyx-build-phase', IntToStr(GPhase));
      document.body.setAttribute('data-nyx-build-status', GOldStatus);
    end;
  end;
end;

begin
  { Record the actual host viewport; a narrow desktop window is a responsive
    consumer check, never a claim about phone hardware or its virtual keyboard. }
  document.body.setAttribute('data-nyx-build-viewport', IntToStr(window.innerWidth));

  if Pos('narrow', window.location.search) > 0 then
  begin
    Check(window.innerWidth < 640, 'ordinary browser uses an actual narrow viewport');
  end
  else
  begin
    Check(window.innerWidth >= 900, 'ordinary browser uses an actual desktop viewport');
  end;
  GSession := TNyxAgentSession.Create;
  GStudio := TBuildStudio.Create;
  GStudio.Run(False);
  Click('action-agents');
  GStudio.ConnectAgents;
  Poll;
end.
