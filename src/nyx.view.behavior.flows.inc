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
  { One reserved fragment route replacement. It owns both route/context leases
    and the attachment through publication. The private raw pointers only access
    objects held by those leases. Inactive candidates stage without subscribing;
    installation transfers the existing observation slots. Old gates revoke before
    any participant notifies, so outstanding old results cannot publish later. }
  TPreparedRoutes = class(TInterfacedObject, INyxPreparedPublication, INyxPublicationReadiness)
  public
    Owner: INyxViewBehavior;
    View: TViewBehavior;
    FormerRoutes: INyxFlowRouteBinding;
    FormerContext: INyxApplicationContext;
    FormerPort: TGraphContext;
    NextRoutes: INyxFlowRouteBinding;
    NextContext: INyxApplicationContext;
    NextPort: TGraphContext;
    Routing: INyxPreparedPublication;
    RoutingReady: INyxPublicationReadiness;
    Generation: INyxCancellationScope;
    Revision: Integer;
    Enabled: Boolean;
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

destructor TPreparedRoutes.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TPreparedRoutes.CheckReady;
begin

  if Retired or Installed or (View = nil) or View.FClosed or
    (View.FRoutePreparation <> Self) or (View.FRouteRevision <> Revision) or
    not View.FLease.Context.Active or Generation.Cancelled or
    (View.FLease.Context.Generation <> Generation) then
  begin
    raise ENyxApplicationContext.Create('Embedded graph owner changed during connection admission');
  end;

  if RoutingReady <> nil then
  begin
    RoutingReady.CheckReady;
  end;
end;

procedure TPreparedRoutes.Validate;
begin

  if Validated then
  begin
    raise ENyxApplicationContext.Create('Graph route preparation is single use');
  end;
  CheckReady;
  Routing.Validate;
  Validated := True;
end;

procedure TPreparedRoutes.Install;
begin
  { Concrete owned gates/leases only; no cancellation, creator or host callout. }

  if not Validated or Retired or Installed then
  begin
    Exit;
  end;
  FormerPort.Closed := True;
  NextPort.Enabled := Enabled;
  Routing.Install;
  View.FGraphPort := NextPort;
  View.FGraphContext := NextContext;
  View.FRoutes := NextRoutes;
  Inc(View.FRouteRevision);
  Installed := True;
end;

procedure TPreparedRoutes.Notify;
begin
  Routing.Notify;
end;

procedure TPreparedRoutes.Retire;
begin

  if Retired then
  begin
    Exit;
  end;
  Retired := True;

  if Routing <> nil then
  begin
    Routing.Retire;
  end;

  if (View <> nil) and (View.FRoutePreparation = Self) then
  begin
    View.FRoutePreparation := nil;
  end;
end;

function TViewBehavior.PrepareFlowRoutes(ADocument: TNyxDocument;
  ARoot: TNyxNode): INyxPreparedPublication;
begin
  Result := PrepareRoutes(ADocument, ARoot, False);
end;

function TViewBehavior.PrepareRouteChanges(ADocument: TNyxDocument;
  ARoot: TNyxNode): INyxPreparedPublication;
begin
  Result := PrepareRoutes(ADocument, ARoot, True);
end;

function TViewBehavior.PrepareRoutes(ADocument: TNyxDocument;
  ARoot: TNyxNode; ATopology: Boolean): INyxPreparedPublication;
var
  LPlan: TPreparedRoutes;
  LLease: INyxPreparedPublication;
  LEditing: INyxFlowRouteReplacement;
  LTopology: INyxFlowRouteTopology;
  LNext: INyxFlowRouteBinding;
  LReady: INyxPublicationReadiness;
begin
  FScheduler.RequireUI;

  if FClosed or (FRoutePreparation <> nil) or (FRouteRevision = High(Integer)) or
    (ADocument = nil) or (ARoot = nil) then
  begin
    raise ENyxApplicationContext.Create('Graph replacement requires one active unreserved fragment');
  end;
  LPlan := TPreparedRoutes.Create;
  LLease := LPlan;
  LPlan.Owner := Self;
  LPlan.View := Self;
  LPlan.FormerRoutes := FRoutes;
  LPlan.FormerContext := FGraphContext;
  LPlan.FormerPort := FGraphPort;
  LPlan.Enabled := FGraphPort.Enabled;
  LPlan.Revision := FRouteRevision;
  FRoutePreparation := LPlan;
  LPlan.Generation := FLease.Context.Generation;
  LPlan.NextPort := TGraphContext.Create;
  LPlan.NextContext := LPlan.NextPort;
  LPlan.NextPort.Base := FLease.Context;
  LPlan.NextPort.Enabled := False;

  if ATopology then
  begin

    if not Supports(FRoutes, INyxFlowRouteTopology, LTopology) then
    begin
      raise ENyxApplicationContext.Create('Graph routes cannot prepare dynamic observation slots');
    end;
    LPlan.Routing := LTopology.PrepareRouteChanges(ADocument, ARoot,
      LPlan.NextContext, FResolver, LNext);
  end
  else
  begin

    if not Supports(FRoutes, INyxFlowRouteReplacement, LEditing) then
    begin
      raise ENyxApplicationContext.Create('Graph routes cannot retain their observation slots');
    end;
    LPlan.Routing := LEditing.PrepareReplacement(ADocument, ARoot,
      LPlan.NextContext, FResolver, LNext);
  end;
  LPlan.NextRoutes := LNext;
  Supports(LPlan.Routing, INyxPublicationReadiness, LReady);
  LPlan.RoutingReady := LReady;
  LPlan.CheckReady;
  Result := LLease;
end;
