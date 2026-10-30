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
program nyx_source_worker_reuse_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, JS, Web, WebOrWorker, nyx.text, nyx.studio.session, nyx.studio.projects,
  nyx.studio.sourcejobs, nyx.studio.worker.transport;

type
  { Sequential real worker journeys, including delayed replies after cancel or
    detach. No phase extends the production worker's thirty-second deadline. }
  TReviewPhase = (rpCold, rpWarm, rpRejected, rpAfterRejected, rpReplayCold, rpReplayWarm, rpCancel,
    rpCancelledWait, rpRecreated, rpDetach, rpDetachedWait);

  { The page owns its session/controller, polling timer and private readiness
    channel. Changed only copies notification counts; destruction occurs after
    notifications return. The channel observes copied helper identity only. }
  TReview = class
  private
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FPhase: TReviewPhase;
    FDraft: TNyxText;
    FBefore: TNyxText;
    FChecks: Integer;
    FApplied: Integer;
    FNotifications: Integer;
    FExpectedApplied: Integer;
    FBeforeNotifications: Integer;
    FTimer: NativeInt;
    FDeadline: Double;
    FSettleAt: Double;
    FReadyChannel: TJSBroadcastChannel;
    FReadyHandler: TJSEventHandler;
    FReadyName: TNyxText;
    FDelayedReady: Boolean;
    procedure Check(AValue: Boolean; const AReason: TNyxText);
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    function DelayedReady(AEvent: TJSEvent): Boolean;
    procedure RetireObserver;
    procedure StartPhase;
    procedure Finish;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Tick;
  end;

var
  GReview: TReview;

constructor TReview.Create;
begin
  inherited Create;
  FTimer := -1;
  FSession := TNyxStudioSession.Create;
  StartPhase;
end;

destructor TReview.Destroy;
begin
  RetireObserver;

  if FTimer >= 0 then
  begin
    window.clearTimeout(FTimer);
  end;
  FCommands.Free;
  FSession.Free;
  inherited Destroy;
end;

procedure TReview.RetireObserver;
begin

  if FReadyChannel <> nil then
  begin
    FReadyChannel.removeEventListener('message', FReadyHandler);
    FReadyChannel.close;
    FReadyChannel := nil;
    FReadyHandler := nil;
  end;
end;

function TReview.DelayedReady(AEvent: TJSEvent): Boolean;
var
  LPacket: TNyxSourceWorkerPacket;
begin
  Result := True;
  LPacket := TNyxSourceWorkerPacket.Read(TJSMessageEvent(AEvent).data);

  if (FPhase in [rpCancel, rpDetach]) and LPacket.HasSequence and
    (LPacket.Text = 'delayed-ready') then
  begin
    FDelayedReady := True;
  end;
end;

procedure TReview.Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(FChecks);
end;

procedure TReview.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin
  Inc(FNotifications);

  if AState = nssApplied then
  begin
    Inc(FApplied);
  end;
end;

procedure TReview.StartPhase;
var
  LURL: TNyxText;
  LID: TGUID;
begin
  FDelayedReady := False;
  document.body.setAttribute('data-worker-reuse-phase', IntToStr(Ord(FPhase)));

  if FPhase in [rpCold, rpReplayCold] then
  begin
    FreeAndNil(FCommands);
    LURL := 'nyx_source_worker.js';

    if FPhase = rpReplayCold then
    begin
      CreateGUID(LID);
      FReadyName := 'nyx-worker-review-' + Copy(GUIDToString(LID), 2, 36);
      FReadyChannel := TJSBroadcastChannel.new(FReadyName);
      FReadyHandler := @DelayedReady;
      FReadyChannel.addEventListener('message', FReadyHandler);
      LURL := 'nyx_source_worker_replay.js?nyx-review=' + FReadyName;
    end;
    FCommands := TNyxSourceCommands.Create(FSession, @Changed, LURL);
    Check(FCommands.BrowserWorkerState = nwsStopped,
      'Constructing a controller does not eagerly launch a helper');
  end;
  FDraft := FSession.Source + #10 + '{ Worker journey ' +
    IntToStr(Ord(FPhase)) + ' retains a moon 🌙. }';

  if FPhase = rpRejected then
  begin
    FDraft := 'this is not Pascal source';
  end;

  if FPhase in [rpCancel, rpDetach] then
  begin
    FDraft := FDraft + #10 + '{ Delay worker reply }';
  end;
  FSession.SetSourceDraft(FDraft);
  FBefore := EncodeNyxProject(FSession.ProjectSnapshot);
  FExpectedApplied := FApplied + 1;
  FDeadline := TJSDate.now + 45000;
  FCommands.Apply;
  FTimer := window.setTimeout(@Tick, 20);
end;

procedure TReview.Finish;
begin
  RetireObserver;
  document.body.textContent := 'PASS ' + IntToStr(FChecks) + ' worker reuse checks';
  document.body.setAttribute('data-test-result', 'passed');
  document.body.setAttribute('data-test-checks', IntToStr(FChecks));
  FreeAndNil(FCommands);
  FreeAndNil(FSession);
end;

procedure TReview.Tick;
begin
  FTimer := -1;
  try

    if TJSDate.now >= FDeadline then
    begin
      raise Exception.Create('Worker reuse journey did not reach its terminal outcome');
    end;

    if FPhase in [rpCancel, rpDetach] then
    begin

      if (FCommands.BrowserWorkerState <> nwsWaiting) or not FDelayedReady then
      begin
        FTimer := window.setTimeout(@Tick, 10);
        Exit;
      end;
      Check(FDelayedReady, 'Revocation follows real preparation with its reply timer armed');
      Check(FCommands.Busy, 'Cancellation targets actual dispatched work');

      if FPhase = rpCancel then
      begin
        FCommands.Cancel;
        Check(FCommands.State = nssCancelled, 'Cancel reports its own terminal state');
        FPhase := rpCancelledWait;
      end
      else
      begin
        FCommands.Detach;
        FPhase := rpDetachedWait;
      end;
      Check(not FCommands.Busy, 'Revocation clears pending command presentation');
      Check(FCommands.BrowserWorkerState = nwsStopped,
        'Cancel and detach terminate active helpers immediately');
      Check(EncodeNyxProject(FSession.ProjectSnapshot) = FBefore,
        'Revocation preserves the exact accepted pair and pending draft');
      FBeforeNotifications := FNotifications;
      FSettleAt := TJSDate.now + 1300;
    end;

    if FPhase in [rpCancelledWait, rpDetachedWait] then
    begin

      if TJSDate.now < FSettleAt then
      begin
        FTimer := window.setTimeout(@Tick, 30);
        Exit;
      end;
      Check(FNotifications = FBeforeNotifications,
        'A revoked helper cannot notify its earlier receiver');
      Check(FApplied = FExpectedApplied - 1,
        'A revoked late reply cannot publish a pair');
      Check(EncodeNyxProject(FSession.ProjectSnapshot) = FBefore,
        'Delayed retired replies leave exact pending source and design alone');

      if FPhase = rpDetachedWait then
      begin
        Finish;
        Exit;
      end;
      FPhase := rpRecreated;
      StartPhase;
      Exit;
    end;

    if FCommands.Busy then
    begin
      FTimer := window.setTimeout(@Tick, 30);
      Exit;
    end;

    if FPhase = rpRejected then
    begin
      Check(FCommands.State = nssRejected, 'Positioned source diagnostics are not helper failures');
      Check(EncodeNyxProject(FSession.ProjectSnapshot) = FBefore,
        'Rejected source retains its exact accepted pair and unfinished draft');
      Check(FApplied = FExpectedApplied - 1, 'Rejected source never publishes a pair');
      Check(FCommands.BrowserWorkerState = nwsIdle, 'Valid diagnostic replies retain the helper');
      FPhase := rpAfterRejected;
      StartPhase;
      Exit;
    end;
    Check(FCommands.State = nssApplied, 'Current source is applied despite stale/duplicate traffic');
    Check(FSession.Source = FDraft, 'The exact current Unicode draft is admitted');
    Check(not FSession.SourceDraftPending, 'Only the admitted pending draft is cleared');
    Check(FApplied = FExpectedApplied, 'Each request publishes exactly once');
    Check(FCommands.BrowserWorkerState = nwsIdle, 'A valid reply leaves its helper reusable');
    case FPhase of
      rpCold: FPhase := rpWarm;
      rpWarm: FPhase := rpRejected;
      rpAfterRejected: FPhase := rpReplayCold;
      rpReplayCold: FPhase := rpReplayWarm;
      rpReplayWarm: FPhase := rpCancel;
      rpRecreated: FPhase := rpDetach;
    else
      begin
        raise Exception.Create('Unexpected worker journey phase');
      end;
    end;
    StartPhase;
  except
    on LException: Exception do
    begin
      RetireObserver;
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-test-result', 'failed');
      FreeAndNil(FCommands);
    end;
  end;
end;

begin
  document.body.setAttribute('data-test-result', 'running');
  GReview := TReview.Create;
end.
