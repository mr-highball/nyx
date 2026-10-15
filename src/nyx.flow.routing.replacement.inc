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
  { A single-use transfer of existing observation slots to admitted plans.
    Managed leases retain both raw owners through every phase. Preparation never
    subscribes or resolves executable creators. Install only moves already owned
    slots; notification cancels the old scope after all models have installed.
    A weak reservation prevents competing transfers without forming a cycle. }
  TRouteReplacement = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  public
    FormerLease: INyxFlowRouteBinding;
    NextLease: INyxFlowRouteBinding;
    Former: TRouting;
    Next: TRouting;
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
end;

procedure TRouteReplacement.Validate;
begin

  if Validated then
  begin
    raise ENyxFlow.Create('Graph observation replacement is single use');
  end;
  CheckReady;
  Validated := True;
end;

procedure TRouteReplacement.Install;
var
  LIndex: Integer;
begin

  if not Validated or Installed or Retired then
  begin
    Exit;
  end;
  { Both vectors and every subscription already exist. Transfer their leases
    before clearing the former owner; no Cancel/Observe/creator call occurs. }
  Next.FObservers := Former.FObservers;
  Next.FObserverLeases := Former.FObserverLeases;
  for LIndex := 0 to High(Former.FPlans) do
  begin
    Next.FPlans[LIndex].Subscription := Former.FPlans[LIndex].Subscription;
    Former.FPlans[LIndex].Subscription := nil;
    Next.FObservers[LIndex].Owner := Next;
  end;
  Former.FObservers := nil;
  Former.FObserverLeases := nil;
  Former.FTransferred := True;
  Next.FStaged := False;
  Installed := True;
end;

procedure TRouteReplacement.Notify;
begin
  Former.Close;
end;

procedure TRouteReplacement.Retire;
begin

  if Retired then
  begin
    Exit;
  end;
  Retired := True;

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
var
  LPlan: TRouteReplacement;
  LLease: INyxPreparedPublication;
  LIndex: Integer;
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

  if Length(FPlans) <> Length(LPlan.Next.FPlans) then
  begin
    raise ENyxFlow.Create('Graph replacement must preserve the exact event route topology');
  end;
  for LIndex := 0 to High(FPlans) do
  begin

    if (FPlans[LIndex].OwnerID <> LPlan.Next.FPlans[LIndex].OwnerID) or
      (FPlans[LIndex].Target.ID <> LPlan.Next.FPlans[LIndex].Target.ID) or
      (FPlans[LIndex].Target.Identity <> LPlan.Next.FPlans[LIndex].Target.Identity) or
      (FPlans[LIndex].Target.Role <> LPlan.Next.FPlans[LIndex].Target.Role) or
      (FPlans[LIndex].Route.ToData.ToJSON <> LPlan.Next.FPlans[LIndex].Route.ToData.ToJSON) then
    begin
      raise ENyxFlow.Create('Graph replacement cannot change existing event route contracts');
    end;
  end;
  LPlan.CheckReady;
  AReplacement := LPlan.NextLease;
  Result := LLease;
end;
