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
  { One preallocated observer activation or removal, never an ordinary callback.
    Managed router/subscription/scope leases own all raw fields below. Install
    touches only flags; cancellation and interface release happen after the
    whole publication group installs. A changed router generation or cancelled
    token refuses before installation. Retirement never revives an old token. }
  TPreparedObservation = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  public
    RouterLease: INyxEvents;
    Subscription: INyxEventSubscription;
    Scope: INyxEventScope;
    Router: TNyxEvents;
    Entry: TNyxSubscription;
    Revision: Integer;
    Activating: Boolean;
    Validated: Boolean;
    Installed: Boolean;
    Retired: Boolean;
    destructor Destroy; override;
    procedure CheckReady;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

destructor TPreparedObservation.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TPreparedObservation.CheckReady;
begin

  if Retired or Installed or (Router = nil) or Router.FClosed or
    (Router.FViewRevision <> Revision) or (Router.FScope <> Scope) or
    (Entry = nil) or Entry.FDisabled or Entry.FScope.Cancelled or
    (Entry.FStaged <> Activating) then
  begin
    raise ENyxSchedule.Create('Observation owner or reservation changed before publication');
  end;
end;

procedure TPreparedObservation.Validate;
begin

  if Validated then
  begin
    raise ENyxSchedule.Create('Observation publication is single use');
  end;
  Router.Admit;
  CheckReady;
  Validated := True;
end;

procedure TPreparedObservation.Install;
begin

  if not Validated or Installed or Retired then
  begin
    Exit;
  end;

  if Activating then
  begin
    Entry.FStaged := False;
  end
  else
  begin
    Entry.FDisabled := True;
  end;
  Installed := True;
end;

procedure TPreparedObservation.Notify;
begin

  if Installed and not Activating then
  begin
    Entry.Cancel;
  end;
end;

procedure TPreparedObservation.Retire;
begin

  if Retired then
  begin
    Exit;
  end;
  Retired := True;

  if (Entry <> nil) and ((Activating and not Installed) or
    (Installed and not Activating)) then
  begin
    try
      Entry.Cancel;
    except
      { The installation fence already revoked authority. Notify reports any
        cancellation failure; destructor cleanup must not throw or revive it. }
    end;
  end;
end;

function TNyxEvents.OwnedObservation(const ASubscription: INyxEventSubscription): TNyxSubscription;
var
  LGroup: Integer;
  LEntry: Integer;
begin
  Result := nil;

  if ASubscription = nil then
  begin
    raise ENyxSchedule.Create('An exact owned observation subscription is required');
  end;
  for LGroup := 0 to High(FGroups) do
  begin

    if not FGroups[LGroup].Observing then
    begin
      Continue;
    end;
    for LEntry := 0 to High(FGroups[LGroup].Registrations) do
    begin

      if FGroups[LGroup].Registrations[LEntry] = ASubscription then
      begin
        Exit(FGroups[LGroup].Entries[LEntry]);
      end;
    end;
  end;
  raise ENyxSchedule.Create('Observation subscription belongs to another router or has retired');
end;

function TNyxEvents.PrepareObservation(const ATarget: TNyxEventTarget; ATrigger: TNyxTrigger;
  const AObserver: INyxEventObserver; APhase: TNyxObservationPhase;
  const AName: TNyxEventRef; out ASubscription: INyxEventSubscription): INyxPreparedPublication;
var
  LPlan: TPreparedObservation;
begin
  ASubscription := nil;
  Admit;
  LPlan := TPreparedObservation.Create;
  Result := LPlan;
  LPlan.RouterLease := Self;
  LPlan.Router := Self;
  LPlan.Scope := FScope;
  LPlan.Revision := FViewRevision;
  LPlan.Activating := True;
  LPlan.Subscription := AddObservation(ATarget, ATrigger, AObserver, APhase, AName, True);
  LPlan.Entry := OwnedObservation(LPlan.Subscription);
  LPlan.CheckReady;
  ASubscription := LPlan.Subscription;
end;

function TNyxEvents.PrepareObservationRemoval(
  const ASubscription: INyxEventSubscription): INyxPreparedPublication;
var
  LPlan: TPreparedObservation;
begin
  Admit;
  LPlan := TPreparedObservation.Create;
  Result := LPlan;
  LPlan.RouterLease := Self;
  LPlan.Router := Self;
  LPlan.Scope := FScope;
  LPlan.Revision := FViewRevision;
  LPlan.Subscription := ASubscription;
  LPlan.Entry := OwnedObservation(ASubscription);
  LPlan.CheckReady;
end;
