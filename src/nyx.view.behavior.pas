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
  nyx.application.context, nyx.collections.registry, nyx.flow, nyx.flow.routing;

type
  { One independently mounted authored fragment. The attachment owns only its
    subscriptions, scheduler, weak parent capability and revocable context port;
    no document, tree, renderer, widget or parent application is retained.
    Close BEFORE its physical renderer/store retires. All methods/final release
    belong to UI. Retained callback contexts safely refuse after Close.

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

  TViewBehavior = class(TInterfacedObject, INyxViewBehavior)
  private
    FEvents: INyxEvents;
    FParent: INyxApplicationContext;
    FScheduler: INyxScheduler;
    FLease: INyxApplicationContextLease;
    FGraphPort: TGraphContext;
    FGraphContext: INyxApplicationContext;
    FRoutes: INyxFlowRouteBinding;
    FCallbacks: TNyxCallbackSubscriptions;
    FParentGeneration: INyxCancellationScope;
    FViewGeneration: INyxCancellationScope;
    FGeneration: INyxCancellationScope;
    FClosed: Boolean;
    function ResolveState: TNyxState;
    function ResolveCollections: INyxCollections;
    function ResolveGeneration: INyxCancellationScope;
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
  end;

procedure TLinkedScope.Cancel;
begin
  Local.Cancel;
end;

function TLinkedScope.GetCancelled: Boolean;
begin
  Result := Local.Cancelled or Parent.Cancelled or View.Cancelled;
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

  if (FEvents = nil) or (FParent = nil) or not FParent.Active then
  begin
    raise ENyxApplicationContext.Create('Embedded behavior requires active parent authority and its own event router');
  end;
  FScheduler := FParent.Scheduler;
  FScheduler.RequireUI;
  FLease := NewNyxApplicationContext(FScheduler, ResolveState, ResolveCollections, ResolveGeneration);
  FGraphPort := TGraphContext.Create;
  FGraphContext := FGraphPort;
  FGraphPort.Base := FLease.Context;
  FGraphPort.Enabled := True;
  FCallbacks := BindNyxViewCallbacks(ADocument, ARoot, FEvents, nil, FLease.Context);

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
  Result := FLease.Context;
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

  if FClosed then
  begin
    raise ENyxApplicationContext.Create('Embedded behavior is retired');
  end;
  FGraphPort.Configure(AValue);

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
