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
  { Single-use publication of admitted route plans and their observer topology.
    Managed leases retain both raw owners through every phase. Preparation keeps
    new slots inactive and does not resolve executable creators. Install moves
    surviving slots, activates additions and disables removals without callouts;
    notification cancels old work after every participating model has installed.
    A weak reservation prevents competing transfers without forming a cycle. }
  TRouteReplacement = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  public
    FormerLease: INyxFlowRouteBinding;
    NextLease: INyxFlowRouteBinding;
    Former: TRouting;
    Next: TRouting;
    { Next-plan index to a surviving former slot, or -1 for a staged new slot.
      All arrays and child preparations are allocated before installation. }
    Transfers: array of Integer;
    SlotChanges: INyxPreparedPublication;
    SlotReady: INyxPublicationReadiness;
    Validated: Boolean;
    Installed: Boolean;
    Retired: Boolean;
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

destructor TRouteReplacement.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TRouteReplacement.CheckReady;
begin

  if Retired or Installed or (Former = nil) or Former.FClosed or
    Former.FTransferred or Former.FStaged or (Former.FPreparation <> Self) or
    (Next = nil) or Next.FClosed or Next.FTransferred or not Next.FStaged then
  begin
    raise ENyxFlow.Create('Graph observation owner changed during replacement');
  end;

  if Validated and (SlotReady <> nil) then
  begin
    SlotReady.CheckReady;
  end;
end;

procedure TRouteReplacement.Validate;
begin

  if Validated then
  begin
    raise ENyxFlow.Create('Graph observation replacement is single use');
  end;
  CheckReady;

  if SlotChanges <> nil then
  begin
    SlotChanges.Validate;
  end;
  Validated := True;
end;

procedure TRouteReplacement.Install;
var
  LIndex: Integer;
  LFormer: Integer;
begin

  if not Validated or Installed or Retired then
  begin
    Exit;
  end;
  { Every new slot and vector is already reserved. Preserve the old owned slots
    in place, activate new slots, and disable retired ones without a callout. }
  for LIndex := 0 to High(Transfers) do
  begin
    LFormer := Transfers[LIndex];

    if LFormer >= 0 then
    begin
      Next.FObservers[LIndex] := Former.FObservers[LFormer];
      Next.FObserverLeases[LIndex] := Former.FObserverLeases[LFormer];
      Next.FPlans[LIndex].Subscription := Former.FPlans[LFormer].Subscription;
      Former.FPlans[LFormer].Subscription := nil;
      Former.FObservers[LFormer] := nil;
      Former.FObserverLeases[LFormer] := nil;
    end;
    Next.FObservers[LIndex].Owner := Next;
    Next.FObservers[LIndex].Index := LIndex;
  end;
  for LIndex := 0 to High(Former.FObservers) do
  begin

    if Former.FObservers[LIndex] <> nil then
    begin
      Former.FObservers[LIndex].Owner := nil;
    end;
  end;

  if SlotChanges <> nil then
  begin
    SlotChanges.Install;
  end;
  Former.FObservers := nil;
  Former.FObserverLeases := nil;
  Former.FTransferred := True;
  Next.FStaged := False;
  Installed := True;
end;

procedure TRouteReplacement.Notify;
begin

  if SlotChanges <> nil then
  begin
    SlotChanges.Notify;
  end;
  Former.Close;
end;

procedure TRouteReplacement.Retire;
begin

  if Retired then
  begin
    Exit;
  end;
  Retired := True;

  if SlotChanges <> nil then
  begin
    SlotChanges.Retire;
  end;

  if Installed then
  begin
    Former.Close;
  end
  else if NextLease <> nil then
  begin
    Next.Close;
  end;

  if (Former <> nil) and (Former.FPreparation = Self) then
  begin
    Former.FPreparation := nil;
  end;
end;

function TRouting.PrepareReplacement(ADocument: TNyxDocument; ARoot: TNyxNode;
  const AContext: INyxApplicationContext; const AResolver: INyxLogicResolver;
  out AReplacement: INyxFlowRouteBinding): INyxPreparedPublication;
begin
  Result := PrepareRoutes(ADocument, ARoot, AContext, AResolver, False, AReplacement);
end;

function TRouting.PrepareRouteChanges(ADocument: TNyxDocument; ARoot: TNyxNode;
  const AContext: INyxApplicationContext; const AResolver: INyxLogicResolver;
  out AReplacement: INyxFlowRouteBinding): INyxPreparedPublication;
begin
  Result := PrepareRoutes(ADocument, ARoot, AContext, AResolver, True, AReplacement);
end;

{ One group is the existing router's exact target/trigger/name/phase, independent
  of route payload/graph meaning. No compound display label becomes identity. }
function SameObservationGroup(AFormer, ANext: TPlan): Boolean;
begin
  Result := (AFormer.OwnerID = ANext.OwnerID) and
    (AFormer.Target.ID = ANext.Target.ID) and
    (AFormer.Target.Identity = ANext.Target.Identity) and
    (AFormer.Target.Role = ANext.Target.Role) and
    (AFormer.Route.Trigger = ANext.Route.Trigger) and
    (AFormer.Route.EventName.Name = ANext.Route.EventName.Name) and
    ((AFormer.Route.CallbackMode = nfcmAfterDispatch) =
      (ANext.Route.CallbackMode = nfcmAfterDispatch));
end;

function TRouting.PrepareRoutes(ADocument: TNyxDocument; ARoot: TNyxNode;
  const AContext: INyxApplicationContext; const AResolver: INyxLogicResolver;
  ATopology: Boolean; out AReplacement: INyxFlowRouteBinding): INyxPreparedPublication;
var
  LPlan: TRouteReplacement;
  LLease: INyxPreparedPublication;
  LIndex: Integer;
  LPrevious: Integer;
  LFormer: Integer;
  LUsed: array of Boolean;
  LChildren: TNyxPreparedPublications;
  LChildCount: Integer;
  LObservation: INyxEventObservationEditing;
  LObserver: TRouteObserver;
  LPhase: TNyxObservationPhase;
  LSubscription: INyxEventSubscription;
  LReady: INyxPublicationReadiness;
begin
  AReplacement := nil;
  FScheduler.RequireUI;

  if FClosed or FTransferred or FStaged or (FPreparation <> nil) or (ADocument = nil) or
    (ARoot = nil) or not ADocument.Contains(ARoot) then
  begin
    raise ENyxFlow.Create('Graph replacement requires an active exact unreserved root');
  end;
  LPlan := TRouteReplacement.Create;
  LLease := LPlan;
  LPlan.FormerLease := Self;
  LPlan.Former := Self;
  FPreparation := LPlan;
  LPlan.Next := TRouting.Create(ADocument, ARoot, FEvents, AContext, AResolver, False);
  LPlan.NextLease := LPlan.Next;

  if not ATopology and (Length(FPlans) <> Length(LPlan.Next.FPlans)) then
  begin
    raise ENyxFlow.Create('Graph replacement must preserve the exact event route topology');
  end;
  SetLength(LPlan.Transfers, Length(LPlan.Next.FPlans));
  SetLength(LPlan.Next.FObservers, Length(LPlan.Next.FPlans));
  SetLength(LPlan.Next.FObserverLeases, Length(LPlan.Next.FPlans));
  SetLength(LUsed, Length(FPlans));

  if ATopology and ((Length(FPlans) > 2048) or (Length(LPlan.Next.FPlans) > 2048)) then
  begin
    raise ENyxFlow.Create('A dynamic route publication admits at most 2048 observation slots');
  end;
  for LIndex := 0 to High(LPlan.Next.FPlans) do
  begin
    LFormer := -1;

    if not ATopology then
    begin

      if (FPlans[LIndex].OwnerID <> LPlan.Next.FPlans[LIndex].OwnerID) or
      (FPlans[LIndex].Target.ID <> LPlan.Next.FPlans[LIndex].Target.ID) or
      (FPlans[LIndex].Target.Identity <> LPlan.Next.FPlans[LIndex].Target.Identity) or
      (FPlans[LIndex].Target.Role <> LPlan.Next.FPlans[LIndex].Target.Role) or
      (FPlans[LIndex].Route.ToData.ToJSON <> LPlan.Next.FPlans[LIndex].Route.ToData.ToJSON) then
      begin
        raise ENyxFlow.Create('Graph replacement cannot change existing event route contracts');
      end;
      LFormer := LIndex;
    end
    else
    begin
      for LPrevious := 0 to High(FPlans) do
      begin

        if not LUsed[LPrevious] and
          SameObservationGroup(FPlans[LPrevious], LPlan.Next.FPlans[LIndex]) and
          (FPlans[LPrevious].Route.ID.Name = LPlan.Next.FPlans[LIndex].Route.ID.Name) then
        begin
          LFormer := LPrevious;
          Break;
        end;
      end;
    end;
    LPlan.Transfers[LIndex] := LFormer;

    if LFormer >= 0 then
    begin
      LUsed[LFormer] := True;
    end;
  end;
  LChildCount := 0;
  for LIndex := 0 to High(LPlan.Transfers) do
  begin

    if LPlan.Transfers[LIndex] < 0 then
    begin
      Inc(LChildCount);
    end;
    for LPrevious := 0 to LIndex - 1 do
    begin

      if SameObservationGroup(LPlan.Next.FPlans[LPrevious], LPlan.Next.FPlans[LIndex]) and
        (LPlan.Transfers[LIndex] >= 0) and ((LPlan.Transfers[LPrevious] < 0) or
          (LPlan.Transfers[LPrevious] > LPlan.Transfers[LIndex])) then
      begin
        raise ENyxFlow.Create('New routes append within their event group; surviving observation order stays fixed');
      end;
    end;
  end;
  for LIndex := 0 to High(LUsed) do
  begin

    if not LUsed[LIndex] then
    begin
      Inc(LChildCount);
    end;
  end;

  if LChildCount > 0 then
  begin

    if (LChildCount > 2048) or not Supports(FEvents, INyxEventObservationEditing, LObservation) then
    begin
      raise ENyxFlow.Create('This router cannot prepare the requested bounded observation topology');
    end;
    SetLength(LChildren, LChildCount);
    LChildCount := 0;
    try
      for LIndex := 0 to High(LPlan.Transfers) do
      begin

        if LPlan.Transfers[LIndex] >= 0 then
        begin
          Continue;
        end;
        LObserver := TRouteObserver.Create;
        LPlan.Next.FObserverLeases[LIndex] := LObserver;
        LPlan.Next.FObservers[LIndex] := LObserver;
        LObserver.Index := LIndex;
        LPhase := nopBeforeCallbacks;

        if LPlan.Next.FPlans[LIndex].Route.CallbackMode = nfcmAfterDispatch then
        begin
          LPhase := nopAfterDispatch;
        end;
        { Admit interface out parameters into locals: pas2js can finalize a
          direct object-field out argument on return, losing its retained lease. }
        LSubscription := nil;
        LChildren[LChildCount] := LObservation.PrepareObservation(LPlan.Next.FPlans[LIndex].Target,
          LPlan.Next.FPlans[LIndex].Route.Trigger, LObserver, LPhase,
          LPlan.Next.FPlans[LIndex].Route.EventName, LSubscription);
        LPlan.Next.FPlans[LIndex].Subscription := LSubscription;
        Inc(LChildCount);
      end;
      for LIndex := 0 to High(LUsed) do
      begin

        if not LUsed[LIndex] then
        begin
          LChildren[LChildCount] := LObservation.PrepareObservationRemoval(FPlans[LIndex].Subscription);
          Inc(LChildCount);
        end;
      end;
      LPlan.SlotChanges := PrepareNyxGroup(LChildren);
      LReady := nil;
      Supports(LPlan.SlotChanges, INyxPublicationReadiness, LReady);
      LPlan.SlotReady := LReady;
    except
      for LIndex := 0 to High(LChildren) do
      begin

        if LChildren[LIndex] <> nil then
        begin
          LChildren[LIndex].Retire;
        end;
      end;
      raise;
    end;
  end;
  LPlan.CheckReady;
  AReplacement := LPlan.NextLease;
  Result := LLease;
end;
