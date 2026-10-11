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
program nyx_http_runtime_recovery_browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

uses
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.operation.panel,
  nyx.studio.view, nyx.studio.recovery.status;

type
  { The fixture owns only this ordinary same-origin Studio frame. All source
    construction, compiler requests, workers and publication remain owned by
    the unmodified product running inside it. No controller or transport is
    replaced. A fresh isolated runtime must hold a rich nine-project checkpoint. }
  TRecoveryStage = (rsWaiting, rsOpenSource, rsEnterDraft, rsObserveDraft,
    rsStart, rsCompiling, rsCancelled, rsRetry, rsRecovered);

  { The matched Web unit omits the Event dictionary constructor. This narrow
    host declaration delivers ordinary bubbling text input without editing RTL. }
  TInputEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;

var
  GFrame: TJSHTMLIFrameElement;
  GRequest: TJSXMLHttpRequest;
  GStage: TRecoveryStage;
  GStatus: TNyxRuntimeRecoveryStatus;
  GStatusReady: Boolean;
  GToken: TNyxText;
  GInitialToken: TNyxText;
  GJob: TNyxText;
  GCancelledJob: TNyxText;
  GDraft: TNyxText;
  GUnits: Integer;
  GChecks: Integer;
  GDeadline: Double;
  GNextProbe: Double;
  GCheckpoint: TNyxText;
  GFinished: Boolean;
  GRetryRotated: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('HTTP Studio recovery: ' + AReason);
  end;
  Inc(GChecks);
  document.body.setAttribute('data-http-recovery-checks', IntToStr(GChecks));
end;

{ Fail without altering the saved registry or attempting a different recovery
  strategy. The owning backend remains inspectable for compiler/join diagnosis. }
procedure Failed(const AReason: TNyxText);
begin
  GFinished := True;
  document.body.setAttribute('data-http-recovery', 'failed');
  document.body.setAttribute('data-event-error', AReason);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := nil;

  if GFrame.contentDocument <> nil then
  begin
    Result := TJSHTMLElement(GFrame.contentDocument.querySelector(
      '[data-node="' + AID + '"]'));
  end;
end;

function Visible(const AID: TNyxText): Boolean;
var
  LFace: TJSHTMLElement;
begin
  LFace := Find(AID);
  Result := (LFace <> nil) and (LFace.getBoundingClientRect.height > 0) and
    (LFace.getBoundingClientRect.width > 0);
end;

procedure Click(const AID: TNyxText);
begin
  Check(Visible(AID), 'operator action is mounted and visible: ' + AID);
  Find(AID).click;
end;

function RecoveryAction(AAction: TNyxOperationAction): TNyxText;
begin
  Result := NyxOperationActionID(NyxStudioRuntimeRecoveryID, AAction);
end;

function TitleContains(const AText: TNyxText): Boolean;
var
  LTitle: TJSHTMLElement;
begin
  LTitle := Find(NyxStudioRuntimeRecoveryID + TNyxText('-title'));
  Result := (LTitle <> nil) and (Pos(AText, LTitle.textContent) > 0);
end;

procedure RequireDraft;
var
  LInput: TJSHTMLTextAreaElement;
begin
  LInput := TJSHTMLTextAreaElement(Find('studio-code'));
  Check((LInput <> nil) and (LInput.value = GDraft),
    'ordinary Studio retains its exact independent unfinished Unicode buffer');
end;

{ Query only host readiness/counters. The fixture never reads saved units or
  supplies a compiler/worker result. Connecting does not rotate recovery authority
  or execute source. Private authority remains in memory and is never displayed. }
procedure Probed;
var
  LReply: TNyxDataValue;
begin
  if GRequest.readyState <> 4 then
  begin
    Exit;
  end;
  try

    if GRequest.status <> 200 then
    begin
      raise Exception.Create('Bounded same-origin readiness refused');
    end;
    LReply := TNyxDataValue.ParseJSON(GRequest.responseText);
    GStatus := DecodeNyxRuntimeRecoveryStatus(LReply.Field('recovery'));
    GToken := LReply.Field('token').AsText;
    GJob := LReply.Field('recovery').Field('job').AsText;
    GStatusReady := True;
    document.body.setAttribute('data-http-recovery-units', IntToStr(GStatus.Units));
    document.body.setAttribute('data-http-recovery-accepted', IntToStr(GStatus.Accepted));
    document.body.setAttribute('data-http-recovery-sessions', IntToStr(GStatus.Sessions));
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
  GRequest.onreadystatechange := nil;
  GRequest := nil;
end;

procedure Probe;
begin
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('POST', NyxRuntimeRecoveryConnectPath, True);
  GRequest.setRequestHeader('Content-Type', 'application/json');
  GRequest.onreadystatechange := @Probed;
  GRequest.send(NyxObject([]).ToJSON);
  GNextProbe := TJSDate.now + 500;
end;

{ The maintained native browser observer captures this actual ordinary Studio
  before acknowledging the next stage. It does not click or modify the editor. }
function Capture(const AName: TNyxText): Boolean;
begin
  Result := False;

  if GCheckpoint <> AName then
  begin
    GCheckpoint := AName;
    document.body.setAttribute('data-capture-checkpoint', AName);
  end;
  Result := document.body.getAttribute('data-capture-observed') = AName;
end;

procedure Pump;
var
  LInput: TJSHTMLTextAreaElement;
  LOptions: TJSObject;
begin

  if GFinished then
  begin
    Exit;
  end;
  try

    if TJSDate.now > GDeadline then
    begin
      raise Exception.Create('Recovery journey exceeded its real-clock budget');
    end;

    if (GRequest = nil) and (TJSDate.now >= GNextProbe) then
    begin
      Probe;
    end;

    if not GStatusReady or (GFrame.contentDocument = nil) or
      (GFrame.contentDocument.body = nil) or
      (GFrame.contentDocument.body.getAttribute('data-nyx-studio-ready') <> 'true') then
    begin
      window.setTimeout(@Pump, 100);
      Exit;
    end;

    if (GStage in [rsCompiling, rsRecovered]) and
      TitleContains('Recovery needs attention') then
    begin
      raise Exception.Create(Find(NyxStudioRuntimeRecoveryID +
        TNyxText('-detail')).textContent);
    end;
    case GStage of
      rsWaiting:
        begin

          if not Visible(RecoveryAction(noaStart)) then
          begin

            if Visible('action-details-toggle') then
            begin
              Check(Pos('Shared projects waiting', Find('studio-details-label').textContent) > 0,
                'compact Studio retains an understandable waiting summary');
              Click('action-details-toggle');
            end;
          end
          else
          begin
            Check(GStatus.Pending and (GStatus.Phase = nrpAwaiting) and
              (GStatus.Sessions = 9) and (GStatus.Units > 0) and (GStatus.Accepted = 0),
              'automatic host waits with the complete rich-source nine-project registry');
            Check(TitleContains('Shared projects waiting'),
              'ordinary Studio presents recovery before requesting shared editing');
            Check(Find('studio-canvas') <> nil, 'local design controls remain mounted');
            GUnits := GStatus.Units;
            GInitialToken := GToken;
            GStage := rsOpenSource;
          end;
        end;
      rsOpenSource:
        begin

          if Visible('action-code') then
          begin
            Click('action-code');
          end
          else
          begin
            Click('action-actions');
            Click('studio-menu-view');
            Click('studio-menu-code');
          end;
          GStage := rsEnterDraft;
        end;
      rsEnterDraft:
        begin
          LInput := TJSHTMLTextAreaElement(Find('studio-code'));

          if (LInput <> nil) and Visible('studio-code') then
          begin
            GDraft := LInput.value + TNyxText(#10'{ Unfinished recovery draft 🚀 𐐷');
            LInput.focus;
            LInput.value := GDraft;
            LOptions := TJSObject.new;
            LOptions['bubbles'] := True;
            LInput.dispatchEvent(TInputEvent.new('input', LOptions));
            GStage := rsObserveDraft;
          end;
        end;
      rsObserveDraft:
        begin
          if Capture('waiting-draft') then
          begin
            RequireDraft;
            GStage := rsStart;
          end;
        end;
      rsStart:
        begin
          Click(RecoveryAction(noaStart));
          GStage := rsCompiling;
        end;
      rsCompiling:
        begin

          if TitleContains('Recovering shared projects') and (GJob <> '') and
            Visible(RecoveryAction(noaCancel)) then
          begin
            Check(GStatus.Pending and (GStatus.Accepted < GUnits),
              'compiler admission does not publish a partial registry');
            RequireDraft;
            GCancelledJob := GJob;
            Click(RecoveryAction(noaCancel));
            GStage := rsCancelled;
          end;
        end;
      rsCancelled:
        begin

          if TitleContains('Recovery cancelled') and (GStatus.Phase = nrpCancelled) and
            Visible(RecoveryAction(noaRetry)) then
          begin
            if Capture('cancelled-draft') then
            begin
              Check(GStatus.Pending and (GStatus.Sessions = 9) and (GCancelledJob <> ''),
                'confirmed cancellation retains all projects after compiler join');
              RequireDraft;
              GStage := rsRetry;
            end;
          end;
        end;
      rsRetry:
        begin
          Click(RecoveryAction(noaRetry));
          GStage := rsRecovered;
        end;
      rsRecovered:
        begin

          if GStatus.Pending and (GToken <> GInitialToken) then
          begin
            GRetryRotated := True;
          end;

          if not GStatus.Pending and TitleContains('Shared projects ready') then
          begin
            if Capture('recovered-draft') then
            begin
              Check((GStatus.Phase = nrpPublished) and (GStatus.Sessions = 9) and
                (GStatus.Units = GUnits) and (GStatus.Accepted = GUnits),
                'actual browser workers publish only the completely executed registry');
              Check(GRetryRotated, 'retry rotates authority before executing retained source');
              RequireDraft;
              GFinished := True;
              document.body.setAttribute('data-http-recovery', 'passed');
            end;
          end;
        end;
    end;
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
      Exit;
    end;
  end;
  window.setTimeout(@Pump, 100);
end;

begin
  document.body.setAttribute('data-http-recovery', 'running');
  GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
  GFrame.setAttribute('title', 'Ordinary Nyx Studio recovery');
  GFrame.style.setProperty('width', '100vw');
  GFrame.style.setProperty('height', '100vh');
  GFrame.style.setProperty('border', '0');
  GFrame.src := '/';
  TJSHTMLElement(document.body).style.setProperty('margin', '0');
  document.body.appendChild(GFrame);
  GDeadline := TJSDate.now + 550000;
  window.setTimeout(@Pump, 100);
end.
