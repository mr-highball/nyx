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

type
  { Single reserved successor generation and its staged registrations. Router,
    scopes, subscriptions and execution vectors are managed leases. The weak
    router reservation never owns this plan, and no model or renderer is held.
    Old leases survive Install; cancellation/destruction belongs to later phases. }
  TPreparedEventView = class(TInterfacedObject, INyxPreparedPublication,
    INyxPreparedEventView, INyxPublicationReadiness)
  private
    FRouterLease: INyxEvents;
    FRouter: TNyxEvents;
    FPreviousScope: INyxEventScope;
    FNextScope: INyxEventScope;
    FGeneration: INyxCancellationScope;
    FRevision: Integer;
    FNextRevision: Integer;
    FTokens: array of INyxEventSubscription;
    FEntries: array of TNyxSubscription;
    FExecutions: TNyxExecutions;
    FPreparing: Boolean;
    FSealed: Boolean;
    FValidated: Boolean;
    FInstalled: Boolean;
    FNotified: Boolean;
    FRetired: Boolean;
    procedure CheckOwner;
    procedure RequireOpen;
    procedure Remember(const AToken: INyxEventSubscription;
      AEntry: TNyxSubscription);
    procedure DoNotify;
    procedure DoRetire;
  public
    constructor Create(ARouter: TNyxEvents);
    destructor Destroy; override;
    function GetGeneration: INyxCancellationScope;
    function PrepareCallback(const ATarget: TNyxEventTarget; ATrigger: TNyxTrigger;
      const ACallback: INyxEventCallback;
      const AName: TNyxEventRef): INyxEventSubscription;
    function PrepareObservation(const ATarget: TNyxEventTarget; ATrigger: TNyxTrigger;
      const AObserver: INyxEventObserver; APhase: TNyxObservationPhase;
      const AName: TNyxEventRef): INyxEventSubscription;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

constructor TPreparedEventView.Create(ARouter: TNyxEvents);
begin
  inherited Create;
  FRouterLease := ARouter;
  FRouter := ARouter;
  FPreviousScope := ARouter.FScope;
  FRevision := ARouter.FViewRevision;
  FNextRevision := 0;

  if FRevision < High(Integer) then
  begin
    FNextRevision := FRevision + 1;
  end;
  FNextScope := TNyxEventScope.Create;

  if not Supports(FNextScope, INyxCancellationScope, FGeneration) then
  begin
    raise ENyxSchedule.Create('Prepared event generation has no cancellation capability');
  end;
end;

destructor TPreparedEventView.Destroy;
begin
  DoRetire;
  inherited Destroy;
end;

procedure TPreparedEventView.CheckOwner;
begin

  if FRetired or FInstalled or (FRouter = nil) or FRouter.FClosed or
    (FRouter.FPreparedView <> Self) or (FRouter.FScope <> FPreviousScope) or
    (FRouter.FViewRevision <> FRevision) or FPreviousScope.Cancelled or
    FNextScope.Cancelled then
  begin
    raise ENyxSchedule.Create('Prepared event view owner or generation changed');
  end;
end;

procedure TPreparedEventView.RequireOpen;
begin
  CheckOwner;

  if FSealed or FPreparing then
  begin
    raise ENyxSchedule.Create('Prepared event registrations are sealed or being admitted');
  end;
end;

procedure TPreparedEventView.Remember(const AToken: INyxEventSubscription;
  AEntry: TNyxSubscription);
var
  LCount: Integer;
begin
  LCount := Length(FTokens);
  SetLength(FTokens, LCount + 1);
  SetLength(FEntries, LCount + 1);
  FTokens[LCount] := AToken;
  FEntries[LCount] := AEntry;
end;

function TPreparedEventView.GetGeneration: INyxCancellationScope;
begin
  Result := FGeneration;
end;

function TPreparedEventView.PrepareCallback(const ATarget: TNyxEventTarget;
  ATrigger: TNyxTrigger; const ACallback: INyxEventCallback;
  const AName: TNyxEventRef): INyxEventSubscription;
var
  LKeepAlive: INyxPreparedEventView;
  LTarget: TNyxEventTarget;
  LName: TNyxEventRef;
  LCallback: INyxEventCallback;
  LStream: INyxEventStream;
  LToken: INyxEventSubscription;
  LGroup: TNyxEventGroup;
  LEntry: TNyxSubscription;
  LIndex: Integer;
  LAdopted: Boolean;
begin
  LKeepAlive := Self;
  LTarget := ATarget;
  LName := AName;
  LCallback := ACallback;
  RequireOpen;
  FPreparing := True;
  LAdopted := False;
  LToken := nil;
  try

    if LCallback = nil then
    begin
      raise ENyxSchedule.Create('An event callback is required');
    end;

    if ATrigger = ntNamed then
    begin
      LName := NyxNamedEvent(LName.Name);
    end
    else
    begin

      if not NyxIsRuntimeTrigger(ATrigger) or (LName.Name <> '') then
      begin
        raise ENyxSchedule.Create('Prepared callback requires a physical or exact named event');
      end;
      LName := NyxEvent('');
    end;
    { Reuse the ordinary stream's exact group and policy. Its admission may
      reenter; only the private creation below introduces a staged slot. }
    LStream := FRouter.Stream(LTarget, ATrigger, LName);
    CheckOwner;
    LGroup := nil;
    for LIndex := 0 to High(FRouter.FGroups) do
    begin

      if not FRouter.FGroups[LIndex].Observing and
        SameTarget(FRouter.FGroups[LIndex].Target, LTarget) and
        (FRouter.FGroups[LIndex].Trigger = ATrigger) and
        (FRouter.FGroups[LIndex].Name.Name = LName.Name) then
      begin
        LGroup := FRouter.FGroups[LIndex];
        Break;
      end;
    end;

    if (LGroup = nil) or (FRouter.FSerial = High(Integer)) then
    begin
      raise ENyxSchedule.Create('Prepared callback group or registration identity unavailable');
    end;
    LGroup.Prune;
    Inc(FRouter.FSerial);
    LEntry := TNyxSubscription.Create(TNyxRegistrationID(FRouter.FSerial), LCallback);
    LToken := LEntry;
    LEntry.FStaged := True;
    Remember(LToken, LEntry);
    SetLength(LGroup.Registrations, Length(LGroup.Registrations) + 1);
    SetLength(LGroup.Entries, Length(LGroup.Entries) + 1);
    LGroup.Registrations[High(LGroup.Registrations)] := LToken;
    LGroup.Entries[High(LGroup.Entries)] := LEntry;
    LAdopted := True;
    Result := LToken;
  finally
    FPreparing := False;

    if not LAdopted and (LToken <> nil) then
    begin
      LToken.Cancel;
    end;
  end;
end;

function TPreparedEventView.PrepareObservation(const ATarget: TNyxEventTarget;
  ATrigger: TNyxTrigger; const AObserver: INyxEventObserver;
  APhase: TNyxObservationPhase; const AName: TNyxEventRef): INyxEventSubscription;
var
  LKeepAlive: INyxPreparedEventView;
  LTarget: TNyxEventTarget;
  LName: TNyxEventRef;
  LObserver: INyxEventObserver;
  LToken: INyxEventSubscription;
  LAdopted: Boolean;
begin
  LKeepAlive := Self;
  LTarget := ATarget;
  LName := AName;
  LObserver := AObserver;
  RequireOpen;
  FPreparing := True;
  LAdopted := False;
  LToken := nil;
  try
    LToken := FRouter.AddObservation(LTarget, ATrigger, LObserver, APhase, LName, True);
    CheckOwner;
    Remember(LToken, FRouter.OwnedObservation(LToken));
    LAdopted := True;
    Result := LToken;
  finally
    FPreparing := False;

    if not LAdopted and (LToken <> nil) then
    begin
      LToken.Cancel;
    end;
  end;
end;

procedure TPreparedEventView.Validate;
var
  LKeepAlive: INyxPreparedEventView;
  LIndex: Integer;
begin
  LKeepAlive := Self;
  RequireOpen;
  { Seal before the outward UI admission, including any reentrant callback. }
  FSealed := True;
  FRouter.Admit;
  CheckOwner;
  SetLength(FExecutions, Length(FRouter.FExecutions));
  for LIndex := 0 to High(FExecutions) do
  begin
    FExecutions[LIndex] := FRouter.FExecutions[LIndex];
  end;
  CheckReady;
  FValidated := True;
end;

procedure TPreparedEventView.CheckReady;
var
  LIndex: Integer;
begin
  CheckOwner;
  for LIndex := 0 to High(FEntries) do
  begin

    if not FEntries[LIndex].FStaged or FEntries[LIndex].FDisabled or
      FEntries[LIndex].FScope.Cancelled then
    begin
      raise ENyxSchedule.Create('Prepared event subscription was cancelled');
    end;
  end;

  if Length(FExecutions) <> Length(FRouter.FExecutions) then
  begin
    raise ENyxSchedule.Create('Event work changed after successor view validation');
  end;
  for LIndex := 0 to High(FExecutions) do
  begin

    if FExecutions[LIndex] <> FRouter.FExecutions[LIndex] then
    begin
      raise ENyxSchedule.Create('Event execution identity changed before view publication');
    end;
  end;
end;

procedure TPreparedEventView.Install;
var
  LIndex: Integer;
begin

  if not FValidated or FInstalled or FRetired then
  begin
    Exit;
  end;
  { Every replaced managed value remains leased here. No finalizer, scheduler,
    callback or allocation runs while the renderer retires its old controls. }
  FPreviousScope.Cancel;
  FRouter.FScope := FNextScope;
  FRouter.FViewRevision := FNextRevision;
  FRouter.FExecutions := nil;
  for LIndex := 0 to High(FEntries) do
  begin
    FEntries[LIndex].FStaged := False;
  end;
  FInstalled := True;
end;

procedure TPreparedEventView.DoNotify;
var
  LExecutions: TNyxExecutions;
  LIndex: Integer;
  LFailed: Boolean;
  LFailure: TNyxText;
begin

  if not FInstalled or FNotified then
  begin
    Exit;
  end;
  FNotified := True;
  { A cancellation receiver may retire the plan. Keep the complete old vector
    locally even when retirement releases the owner's copy during this loop. }
  LExecutions := FExecutions;
  LFailed := False;
  LFailure := '';
  for LIndex := 0 to High(LExecutions) do
  begin
    try
      LExecutions[LIndex].Cancel;
    except
      on E: Exception do
      begin

        if not LFailed then
        begin
          {$ifdef PAS2JS}
          LFailure := E.Message;
          {$else}
          LFailure := UTF8Encode(UnicodeString(E.Message));
          {$endif}
        end;
        LFailed := True;
      end;
    end;
  end;

  if LFailed then
  begin
    raise ENyxSchedule.Create('Event view installed; old execution cancellation failed: ' +
      LFailure);
  end;
end;

procedure TPreparedEventView.Notify;
var
  LKeepAlive: INyxPreparedEventView;
begin
  LKeepAlive := Self;
  DoNotify;
end;

procedure TPreparedEventView.DoRetire;
var
  LIndex: Integer;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if (FRouter <> nil) and (FRouter.FPreparedView = Self) then
  begin
    FRouter.FPreparedView := nil;
  end;

  if FInstalled then
  begin
    try
      DoNotify;
    except
      { Explicit Notify reports committed cancellation failure. Retirement and
        destruction still attempt every ticket, but cannot throw or retry it. }
    end;
    FExecutions := nil;
  end
  else
  begin

    if FNextScope <> nil then
    begin
      FNextScope.Cancel;
    end;
    for LIndex := 0 to High(FEntries) do
    begin
      FEntries[LIndex].FDisabled := True;
      FEntries[LIndex].FStaged := False;
      FEntries[LIndex].FScope.Cancel;
      try
        FTokens[LIndex].Cancel;
      except
        { Revocation precedes user-owned finalization. Abandonment never
          reports notification failure or revives a staged registration. }
      end;
    end;
  end;
end;

procedure TPreparedEventView.Retire;
var
  LKeepAlive: INyxPreparedEventView;
begin
  LKeepAlive := Self;
  DoRetire;
end;

function TNyxEvents.PrepareView: INyxPreparedEventView;
var
  LKeepAlive: INyxEvents;
  LPlan: TPreparedEventView;
  LResult: INyxPreparedEventView;
begin
  LKeepAlive := Self;
  Admit;

  if FClosed or (FPreparedView <> nil) then
  begin
    raise ENyxSchedule.Create('An event view already reserves this router');
  end;
  LPlan := TPreparedEventView.Create(Self);
  LResult := LPlan;
  FPreparedView := LPlan;
  LPlan.CheckOwner;
  Result := LResult;
end;
