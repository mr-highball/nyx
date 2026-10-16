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

program nyx_observation_publication_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.events, nyx.behavior,
  nyx.scheduler, nyx.publication
  {$ifdef PAS2JS}, Web{$endif};

type
  { Retained ordinary observer records call order only. It owns no router,
    document or receiver and cannot grant model or execution authority. }
  TObserver = class(TNyxEventObserver)
  public
    Caption: TNyxText;
    Calls: Integer;
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
  end;

  { Ordinary callback can publish an already prepared fixture change. The
    dispatch snapshot owns this callback; its field never retains a tree. }
  TPublishCallback = class(TNyxEventCallback)
  public
    Pending: INyxPreparedPublication;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Later participant either refuses admission or explicitly closes the router
    during validation. Neither action is hidden in production code. }
  TRefusal = class(TInterfacedObject, INyxPreparedPublication)
  public
    Closing: INyxEvents;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

var
  GChecks: Integer;
  GTrace: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Observation publication: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TObserver.Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer);
begin
  Inc(Calls);
  GTrace := GTrace + Caption;
end;

procedure TPublishCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LPlan: INyxPreparedPublication;
begin
  GTrace := GTrace + 'C';
  LPlan := Pending;
  Pending := nil;

  if LPlan <> nil then
  begin
    PublishNyxGroup([LPlan]);
  end;
end;

procedure TRefusal.Validate;
begin

  if Closing <> nil then
  begin
    Closing.Close;
    Exit;
  end;
  raise ENyxSchedule.Create('Deliberate later-participant refusal');
end;

procedure TRefusal.Install;
begin
end;

procedure TRefusal.Notify;
begin
end;

procedure TRefusal.Retire;
begin
end;

procedure Click(const AEvents: INyxEvents);
var
  LEvent: TNyxEventInfo;
begin
  LEvent := Default(TNyxEventInfo);
  LEvent.Name := NyxEvent('click');
  LEvent.Trigger := ntClick;
  LEvent.OriginID := 'button';
  LEvent.SourceID := 'button';
  LEvent.TargetID := 'button';
  LEvent.Value := NyxNull;
  LEvent.Details := NyxNull;
  AEvents.Dispatch(LEvent, 'button', 'button');
end;

procedure Run;
var
  LEvents: INyxEvents;
  LOther: INyxEvents;
  LEditing: INyxEventObservationEditing;
  LObservation: INyxEventObservation;
  LFirst: TObserver;
  LNext: TObserver;
  LFirstLease: INyxEventObserver;
  LNextLease: INyxEventObserver;
  LToken: INyxEventSubscription;
  LExternal: INyxEventSubscription;
  LCallbackToken: INyxEventSubscription;
  LCallback: TPublishCallback;
  LCallbackLease: INyxEventCallback;
  LPlan: INyxPreparedPublication;
  LReject: INyxPreparedPublication;
  LClosing: TRefusal;
  LFailed: Boolean;
begin
  LEvents := NewNyxEvents;
  LOther := NewNyxEvents(LEvents.Scheduler);
  try
    Check(Supports(LEvents, INyxEventObservationEditing, LEditing),
      'the standard event router exposes optional prepared observations');
    Supports(LEvents, INyxEventObservation, LObservation);
    LFirst := TObserver.Create;
    LFirstLease := LFirst;
    LFirst.Caption := 'A';
    LNext := TObserver.Create;
    LNextLease := LNext;
    LNext.Caption := 'B';
    LPlan := LEditing.PrepareObservation(NyxControlEvents('button'), ntClick,
      LFirstLease, nopAfterDispatch, NyxEvent(''), LToken);
    Check(not LToken.Active and not LEvents.HasSubscribers(ntClick),
      'reserved slots grant neither active token nor subscriber/default admission');
    Click(LEvents);
    Check(LFirst.Calls = 0, 'ordinary pruning/dispatch cannot execute a reserved observer');
    LExternal := LObservation.Observe(NyxControlEvents('button'), ntClick,
      LNextLease, nopAfterDispatch, NyxEvent(''));
    GTrace := '';
    Click(LEvents);
    Check(GTrace = 'B', 'a later independent observation stays active during preparation');
    PublishNyxGroup([LPlan]);
    LPlan := nil;
    GTrace := '';
    Click(LEvents);
    Check(LToken.Active and (GTrace = 'AB'),
      'installation activates the reserved ordered slot without replacing independent observers');

    LPlan := LEditing.PrepareObservationRemoval(LToken);
    LFailed := False;
    LReject := TRefusal.Create;
    try
      PublishNyxGroup([LPlan, LReject]);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LToken.Active, 'later refusal cannot disable an existing observer');
    LPlan := nil;
    LReject := nil;
    GTrace := '';
    Click(LEvents);
    Check(GTrace = 'AB', 'refused removal retains exact dispatch order');
    LPlan := LEditing.PrepareObservationRemoval(LToken);
    PublishNyxGroup([LPlan]);
    LPlan := nil;
    GTrace := '';
    Click(LEvents);
    Check(not LToken.Active and (GTrace = 'B'), 'committed removal revokes exactly its own observer');
    LExternal.Cancel;
    LExternal := nil;
    Check(not LEvents.HasSubscribers(ntClick), 'retired observations do not keep host defaults opted in');

    LPlan := LEditing.PrepareObservation(NyxControlEvents('button'), ntClick,
      LFirstLease, nopAfterDispatch, NyxEvent(''), LToken);
    LPlan := nil;
    GTrace := '';
    Click(LEvents);
    Check(not LToken.Active and (GTrace = ''), 'abandoned activation retires only its inert slot');

    LPlan := LEditing.PrepareObservation(NyxControlEvents('button'), ntClick,
      LFirstLease, nopAfterDispatch, NyxEvent(''), LToken);
    LToken.Cancel;
    LFailed := False;
    try
      PublishNyxGroup([LPlan]);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and not LToken.Active, 'cancelled reservations cannot reactivate');
    LPlan := nil;

    LCallback := TPublishCallback.Create;
    LCallbackLease := LCallback;
    LCallbackToken := LEvents.On(NyxControlEvents('button'), ntClick).Subscribe(LCallbackLease);
    LCallback.Pending := LEditing.PrepareObservation(NyxControlEvents('button'), ntClick,
      LFirstLease, nopAfterDispatch, NyxEvent(''), LToken);
    GTrace := '';
    Click(LEvents);
    Check(LToken.Active and (GTrace = 'C'), 'mid-dispatch publication starts on the next event snapshot');
    GTrace := '';
    Click(LEvents);
    Check(GTrace = 'CA', 'the next ordinary event observes the newly installed route slot');
    LFailed := False;
    try
      LPlan := LEditing.PrepareObservationRemoval(LCallbackToken);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LCallbackToken.Active, 'observation editing cannot silently edit ordinary callback streams');
    LCallbackToken.Cancel;
    LCallbackToken := nil;
    LCallbackLease := nil;
    LCallback := nil;
    LToken.Cancel;

    Supports(LOther, INyxEventObservation, LObservation);
    LExternal := LObservation.Observe(NyxControlEvents('button'), ntClick,
      LNextLease, nopAfterDispatch, NyxEvent(''));
    LFailed := False;
    try
      LPlan := LEditing.PrepareObservationRemoval(LExternal);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LExternal.Active, 'another router token supplies no removal authority');
    LPlan := LEditing.PrepareObservation(NyxControlEvents('button'), ntClick,
      LFirstLease, nopAfterDispatch, NyxEvent(''), LToken);
    LClosing := TRefusal.Create;
    LReject := LClosing;
    LClosing.Closing := LEvents;
    LFailed := False;
    try
      PublishNyxGroup([LPlan, LReject]);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and not LToken.Active and not LEvents.HasSubscribers(ntClick),
      'later owner retirement is caught by the final readiness fence');
  finally
    LPlan := nil;
    LReject := nil;
    LCallbackToken := nil;
    LCallbackLease := nil;
    LToken := nil;
    LExternal := nil;
    LFirstLease := nil;
    LNextLease := nil;
    LEditing := nil;
    LObservation := nil;
    LOther.Close;
    LEvents.Close;
    LOther := nil;
    LEvents := nil;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-observation-publication', 'passed');
    document.body.setAttribute('data-observation-publication-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' atomic observation publication checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-observation-publication', 'failed');
      document.body.setAttribute('data-observation-publication-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
