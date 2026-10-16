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


unit nyx.view.behavior;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.model, nyx.state, nyx.events, nyx.scheduler,
  nyx.application.context, nyx.collections.registry, nyx.flow, nyx.flow.routing,
  nyx.publication.types;

type
  { One independently mounted authored fragment. The attachment owns only its
    subscriptions, scheduler, weak parent capability and revocable context port;
    no document, tree, renderer, widget or parent application is retained.
    Close BEFORE its physical renderer/store retires. All methods/final release
    belong to UI. Retained callback contexts report inactive and safely refuse
    after Close or parent lease revocation, without owning either host.

    Graph enablement is independent from static callbacks and port chrome.
    Disabling cancels current graph work; enabling admits future events, without
    replaying earlier inputs or pretending to roll back entered host side effects. }
  INyxViewBehavior = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000016}']
    procedure Close;
    function GetContext: INyxApplicationContext;
    function GetRoutes: INyxFlowRouteBinding;
    procedure EnableGraphs(AValue: Boolean);
    function GetGraphsEnabled: Boolean;
    property Context: INyxApplicationContext read GetContext;
    property Routes: INyxFlowRouteBinding read GetRoutes;
    property GraphsEnabled: Boolean read GetGraphsEnabled;
  end;

  { Optional UI-only replacement of this fragment's graph routes. Static
    callbacks, physical controls and application store stay retained. Preparation
    reserves this attachment and stages unobserved plans; existing observation
    slots transfer in order during installation. One prepared group
    installs all affected fragments before any old work is cancelled/notified.
    The document/root are borrowed only during preparation. No host authority
    is acquired from the new metadata. Last release and Retire belong to UI. }
  INyxViewFlowEditing = interface(IInterface)
    ['{04973D8E-C3C7-439E-8A01-ED7993C4AE20}']
    function PrepareFlowRoutes(ADocument: TNyxDocument;
      ARoot: TNyxNode): INyxPreparedPublication;
  end;

  { Optional dynamic route topology for an existing mounted fragment. The same
    prepared context/publication owner stages new inactive event slots and
    transfers survivors in place; physical controls and static callbacks are
    not remounted. Exact group ordering and bounds follow INyxFlowRouteTopology.
    Borrow the candidate context only during preparation; release/Retire on UI. }
  INyxViewRouteTopology = interface(IInterface)
    ['{00B58263-D65F-4268-9CC8-19F2FD4AF5E3}']
    function PrepareRouteChanges(ADocument: TNyxDocument;
      ARoot: TNyxNode): INyxPreparedPublication;
  end;

{ Exact authored page/reusable context; callbacks bind before ordered graph
  observation. Parent supplies the actual application state/collection authority.
  Generation links both the current parent view and this item's event router,
  so removal, replacement, failed staging and navigation cannot deliver stale
  graph output. Creator resolution remains lazy and explicitly host-supplied. }
function BindNyxViewBehavior(ADocument: TNyxDocument; ARoot: TNyxNode;
  const AEvents: INyxEvents; const AParent: INyxApplicationContext;
  const AResolver: INyxLogicResolver = nil): INyxViewBehavior;
{ Pure effective-tree query for hosts without runtime authority. It distinguishes
  an ordinary/static fragment from executable declarations that must refuse
  rather than silently render inactive behavior. No callback factory is entered. }
function NyxViewRequiresBehavior(ADocument: TNyxDocument; ARoot: TNyxNode): Boolean;

implementation

uses nyx.callbacks, nyx.composition, nyx.data, nyx.flow.routes;

type
  { Parent-aware weak capability for one embedded attachment. Base and Parent
    retain only revocable ports, never their application/view/store owners.
    Active and every accessor require both lifetimes; the independent scheduler
    refuses worker access even after either port retires. Local Close revokes
    Base without cancelling or extending the parent's authority. }
  TEmbeddedContext = class(TInterfacedObject, INyxApplicationContext)
  private
    FBase: INyxApplicationContext;
    FParent: INyxApplicationContext;
    FScheduler: INyxScheduler;
    procedure RequireActive;
  public
    constructor Create(const ABase, AParent: INyxApplicationContext;
      const AScheduler: INyxScheduler);
    function GetActive: Boolean;
    function GetState: TNyxState;
    function GetCollections: INyxCollections;
    function GetScheduler: INyxScheduler;
    function GetGeneration: INyxCancellationScope;
  end;

  { Cancellation owns tokens only. It never cancels an ancestor's shared scope;
    local cancellation revokes only this attachment's generation. }
  TLinkedScope = class(TInterfacedObject, INyxCancellationScope)
  public
    Local: INyxCancellationScope;
    Parent: INyxCancellationScope;
    View: INyxCancellationScope;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

  { Gate owns a weak base port. Its lifetime cannot keep the raw receiver alive,
    and graph pause does not affect the static callback context. }
  TGraphContext = class(TInterfacedObject, INyxApplicationContext)
  public
    Base: INyxApplicationContext;
    Enabled: Boolean;
    Closed: Boolean;
    Scope: INyxCancellationScope;
    BaseScope: INyxCancellationScope;
    procedure RequireActive;
    function GetActive: Boolean;
    function GetState: TNyxState;
    function GetCollections: INyxCollections;
    function GetScheduler: INyxScheduler;
    function GetGeneration: INyxCancellationScope;
    procedure Configure(AEnabled: Boolean);
  end;

  { UI-owned revocable fragment attachment. Route preparations retain it only
    until retirement; callbacks retain weak context ports instead of this owner. }
  TViewBehavior = class(TInterfacedObject, INyxViewBehavior, INyxViewFlowEditing,
    INyxViewRouteTopology)
  private
    FEvents: INyxEvents;
    FParent: INyxApplicationContext;
    FScheduler: INyxScheduler;
    FLease: INyxApplicationContextLease;
    FContext: INyxApplicationContext;
    FGraphPort: TGraphContext;
    FGraphContext: INyxApplicationContext;
    FRoutes: INyxFlowRouteBinding;
    FCallbacks: TNyxCallbackSubscriptions;
    FParentGeneration: INyxCancellationScope;
    FViewGeneration: INyxCancellationScope;
    FGeneration: INyxCancellationScope;
    FClosed: Boolean;
    FResolver: INyxLogicResolver;
    FRoutePreparation: TObject;
    FRouteRevision: Integer;
    function ResolveState: TNyxState;
    function ResolveCollections: INyxCollections;
    function ResolveGeneration: INyxCancellationScope;
    function PrepareRoutes(ADocument: TNyxDocument;
      ARoot: TNyxNode; ATopology: Boolean): INyxPreparedPublication;
  public
    constructor Create(ADocument: TNyxDocument; ARoot: TNyxNode;
      const AEvents: INyxEvents; const AParent: INyxApplicationContext;
      const AResolver: INyxLogicResolver);
    destructor Destroy; override;
    procedure Close;
    function GetContext: INyxApplicationContext;
    function GetRoutes: INyxFlowRouteBinding;
    procedure EnableGraphs(AValue: Boolean);
    function GetGraphsEnabled: Boolean;
    function PrepareFlowRoutes(ADocument: TNyxDocument;
      ARoot: TNyxNode): INyxPreparedPublication;
    function PrepareRouteChanges(ADocument: TNyxDocument;
      ARoot: TNyxNode): INyxPreparedPublication;
  end;

{$I nyx.view.behavior.flows.inc}

procedure TLinkedScope.Cancel;
begin
  Local.Cancel;
end;

function TLinkedScope.GetCancelled: Boolean;
begin
  Result := Local.Cancelled or Parent.Cancelled or View.Cancelled;
end;

constructor TEmbeddedContext.Create(const ABase, AParent: INyxApplicationContext;
  const AScheduler: INyxScheduler);
begin
  inherited Create;
  FBase := ABase;
  FParent := AParent;
  FScheduler := AScheduler;
  FScheduler.RequireUI;
end;

function TEmbeddedContext.GetActive: Boolean;
begin
  FScheduler.RequireUI;
  Result := FBase.Active and FParent.Active;
end;

procedure TEmbeddedContext.RequireActive;
begin

  if not GetActive then
  begin
    raise ENyxApplicationContext.Create('Embedded application authority is retired');
  end;
end;

function TEmbeddedContext.GetState: TNyxState;
begin
  RequireActive;
  Result := FBase.State;
end;

function TEmbeddedContext.GetCollections: INyxCollections;
begin
  RequireActive;
  Result := FBase.Collections;
end;

function TEmbeddedContext.GetScheduler: INyxScheduler;
begin
  RequireActive;
  Result := FScheduler;
end;

function TEmbeddedContext.GetGeneration: INyxCancellationScope;
begin
  RequireActive;
  Result := FBase.Generation;
end;

function Linked(const AParent, AView: INyxCancellationScope): INyxCancellationScope;
var
  LScope: TLinkedScope;
begin
  LScope := TLinkedScope.Create;
  Result := LScope;
  LScope.Local := NewNyxCancellationScope;
  LScope.Parent := AParent;
  LScope.View := AView;
end;

function TGraphContext.GetActive: Boolean;
begin
  Result := not Closed and Enabled and Base.Active;
end;

procedure TGraphContext.RequireActive;
begin

  if not GetActive then
  begin
    raise ENyxApplicationContext.Create('Embedded graph behavior is paused or retired');
  end;
  Base.Scheduler.RequireUI;
end;

function TGraphContext.GetState: TNyxState;
begin
  RequireActive;
  Result := Base.State;
end;

function TGraphContext.GetCollections: INyxCollections;
begin
  RequireActive;
  Result := Base.Collections;
end;

function TGraphContext.GetScheduler: INyxScheduler;
begin
  RequireActive;
  Result := Base.Scheduler;
end;

function TGraphContext.GetGeneration: INyxCancellationScope;
var
  LBase: INyxCancellationScope;
begin
  RequireActive;
  LBase := Base.Generation;

  if (Scope = nil) or (LBase <> BaseScope) then
  begin
    BaseScope := LBase;
    Scope := Linked(LBase, NewNyxCancellationScope);
  end;
  Result := Scope;
end;

procedure TGraphContext.Configure(AEnabled: Boolean);
begin
  Base.Scheduler.RequireUI;

  if Closed then
  begin
    raise ENyxApplicationContext.Create('Embedded graph behavior is retired');
  end;

  if Enabled = AEnabled then
  begin
    Exit;
  end;
  Enabled := AEnabled;

  if Scope <> nil then
  begin
    Scope.Cancel;
  end;
  Scope := nil;
  BaseScope := nil;
end;

constructor TViewBehavior.Create(ADocument: TNyxDocument; ARoot: TNyxNode;
  const AEvents: INyxEvents; const AParent: INyxApplicationContext;
  const AResolver: INyxLogicResolver);
var
  LResolver: INyxLogicResolver;
begin
  inherited Create;
  FEvents := AEvents;
  FParent := AParent;
  LResolver := AResolver;
  FResolver := LResolver;

  if (FEvents = nil) or (FParent = nil) or not FParent.Active then
  begin
    raise ENyxApplicationContext.Create('Embedded behavior requires active parent authority and its own event router');
  end;
  FScheduler := FParent.Scheduler;
  FScheduler.RequireUI;
  FLease := NewNyxApplicationContext(FScheduler, ResolveState, ResolveCollections, ResolveGeneration);
  FContext := TEmbeddedContext.Create(FLease.Context, FParent, FScheduler);
  FGraphPort := TGraphContext.Create;
  FGraphContext := FGraphPort;
  FGraphPort.Base := FContext;
  FGraphPort.Enabled := True;
  FCallbacks := BindNyxViewCallbacks(ADocument, ARoot, FEvents, nil, FContext);

  if not FParent.Active then
  begin
    raise ENyxApplicationContext.Create('Embedded owner retired while attaching callbacks');
  end;
  FRoutes := BindNyxViewFlowRoutes(ADocument, ARoot, FEvents, FGraphContext, LResolver);
end;

destructor TViewBehavior.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TViewBehavior.Close;
var
  LIndex: Integer;
begin

  if FClosed then
  begin
    Exit;
  end;

  if FScheduler <> nil then
  begin
    FScheduler.RequireUI;
  end;
  FClosed := True;

  if FGraphPort <> nil then
  begin
    FGraphPort.Closed := True;

    if FGraphPort.Scope <> nil then
    begin
      FGraphPort.Scope.Cancel;
    end;
  end;

  if FLease <> nil then
  begin
    FLease.Close;
  end;

  if FGeneration <> nil then
  begin
    FGeneration.Cancel;
  end;

  if FRoutes <> nil then
  begin
    FRoutes.Close;
  end;
  for LIndex := 0 to High(FCallbacks) do
  begin
    FCallbacks[LIndex].Cancel;
  end;
  FCallbacks := nil;
  FResolver := nil;
end;

function TViewBehavior.ResolveState: TNyxState;
begin

  if not FParent.Active then
  begin
    raise ENyxApplicationContext.Create('Embedded parent is retired');
  end;
  Result := FParent.State;
end;

function TViewBehavior.ResolveCollections: INyxCollections;
begin

  if not FParent.Active then
  begin
    raise ENyxApplicationContext.Create('Embedded parent is retired');
  end;
  Result := FParent.Collections;
end;

function TViewBehavior.ResolveGeneration: INyxCancellationScope;
var
  LParent: INyxCancellationScope;
  LView: INyxCancellationScope;
begin

  if not FParent.Active then
  begin
    raise ENyxApplicationContext.Create('Embedded parent is retired');
  end;
  LParent := FParent.Generation;
  LView := NyxEventGeneration(FEvents);

  if (LParent <> FParentGeneration) or (LView <> FViewGeneration) then
  begin

    if FGeneration <> nil then
    begin
      FGeneration.Cancel;
    end;
    FParentGeneration := LParent;
    FViewGeneration := LView;
    FGeneration := Linked(LParent, LView);
  end;
  Result := FGeneration;
end;

function TViewBehavior.GetContext: INyxApplicationContext;
begin
  FScheduler.RequireUI;
  Result := FContext;
end;

function TViewBehavior.GetRoutes: INyxFlowRouteBinding;
begin
  FScheduler.RequireUI;
  Result := FRoutes;
end;

procedure TViewBehavior.EnableGraphs(AValue: Boolean);
var
  LIndex: Integer;
begin
  FScheduler.RequireUI;

  if FClosed or (FRoutePreparation <> nil) or (FRouteRevision = High(Integer)) then
  begin
    raise ENyxApplicationContext.Create('Embedded graph policy is retired or reserved');
  end;
  FGraphPort.Configure(AValue);
  Inc(FRouteRevision);

  if not AValue then
  begin
    for LIndex := 0 to FRoutes.Count - 1 do
    begin
      FRoutes.Cancel(LIndex);
    end;
  end;
end;

function TViewBehavior.GetGraphsEnabled: Boolean;
begin
  FScheduler.RequireUI;
  Result := not FClosed and FGraphPort.Enabled;
end;

function NyxViewRequiresBehavior(ADocument: TNyxDocument; ARoot: TNyxNode): Boolean;
var
  LRoot: TNyxNode;

  function Visit(ANode: TNyxNode): Boolean;
  var
    LEvents: TNyxAuthoredEventInfos;
    LIndex: Integer;
  begin
    Result := False;

    if ANode.Extensions.Has(NyxExtension(NyxFlowRoutesKey)) and
      (NyxFlowRoutesFromData(ANode.Extensions.Value(NyxExtension(NyxFlowRoutesKey))).Count > 0) then
    begin
      Exit(True);
    end;
    LEvents := NyxAuthoredEvents(ANode);
    for LIndex := 0 to High(LEvents) do
    begin

      if Length(LEvents[LIndex].Callbacks) > 0 then
      begin
        Exit(True);
      end;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin

      if Visit(ANode.Children[LIndex]) then
      begin
        Exit(True);
      end;
    end;
  end;

begin
  LRoot := RealizeNyxView(ADocument, ARoot);
  try
    Result := Visit(LRoot);
  finally
    ReleaseNyxNode(LRoot);
  end;
end;

function BindNyxViewBehavior(ADocument: TNyxDocument; ARoot: TNyxNode;
  const AEvents: INyxEvents; const AParent: INyxApplicationContext;
  const AResolver: INyxLogicResolver): INyxViewBehavior;
begin
  Result := TViewBehavior.Create(ADocument, ARoot, AEvents, AParent, AResolver);
end;

end.
