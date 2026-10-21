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
unit nyx.studio.previewruntime;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.root.types, nyx.state,
  nyx.scheduler, nyx.application.state, nyx.application.context,
  nyx.collections.registry, nyx.collections.bindings, nyx.events,
  nyx.publication.types, nyx.view.publication;

type
  { One Studio Interact mount owns independent application stores and a scoped
    capability for embedded Nyx sandboxes. Saved defaults/source never change.
    It consumes the public application-state/context contracts, not another UI
    runtime. Root Pascal callback implementations still require compiled hosts.

    The renderer borrows State/Bindings until unmount. Close revokes context and
    pending generations before widget teardown; Free follows renderer teardown.
    Private source/prototypes outlive all raw state validators. All operations
    and final disposal belong to the renderer's application UI thread. }
  TNyxStudioPreviewRuntime = class
  private
    FDocument: TNyxDocument;
    FRuntime: TNyxApplicationState;
    FRoot: TNyxRootRef;
    FScheduler: INyxScheduler;
    FGeneration: INyxCancellationScope;
    FLease: INyxApplicationContextLease;
    FManagedContext: INyxApplicationContext;
    FClosed: Boolean;
    FCleaned: Boolean;
    function ResolveState: TNyxState;
    function ResolveCollections: INyxCollections;
    function ResolveGeneration: INyxCancellationScope;
    function GetContext: INyxApplicationContext;
    function GetBindings: INyxCollectionBindings;
  public
    { Capture an independent source/default frame. A focused reusable gets the
      same typed stores and hidden-page validation as an ordinary page. No file,
      HTTP or arbitrary native compiler authority is inferred by this host. }
    constructor Create(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
      APlatform: TNyxPlatform; const AScheduler: INyxScheduler);
    destructor Destroy; override;
    procedure Close;
    property State: TNyxState read ResolveState;
    property Context: INyxApplicationContext read GetContext;
    property Bindings: INyxCollectionBindings read GetBindings;
  end;

  { UI-only managed wrapper preserves the raw runtime's Create/Close/Free API.
    Runtime is borrowed while this interface is retained. Revoke changes owned
    flags only; Close performs fallible cancellation and context cleanup later. }
  INyxStudioPreviewOwner = interface(IInterface)
    ['{9AA641EB-2E25-4871-AA27-5006CB023369}']
    function GetRuntime: TNyxStudioPreviewRuntime;
    procedure Revoke;
    procedure Close;
    property Runtime: TNyxStudioPreviewRuntime read GetRuntime;
  end;

  { Studio owns the slot, never the preparation source. Sources retain a weak
    port which is revoked before slot disposal. Current is a managed snapshot;
    its runtime stays borrowed until that snapshot is released. Revoke precedes
    renderer teardown; Close and destruction follow renderer teardown. }
  TNyxStudioPreviewSlot = class
  private
    FScheduler: INyxScheduler;
    FPort: TObject;
    FPortLease: IInterface;
    FCurrent: INyxStudioPreviewOwner;
    FCurrentObject: TObject;
    FMount: TObject;
    FMountLease: IInterface;
    FReservation: TObject;
    FRevision: Integer;
    FClosed: Boolean;
    function GetCurrent: INyxStudioPreviewOwner;
  public
    constructor Create(const AScheduler: INyxScheduler);
    destructor Destroy; override;
    procedure Revoke;
    procedure Close;
    property Current: INyxStudioPreviewOwner read GetCurrent;
  end;

function NewNyxStudioPreviewOwner(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; APlatform: TNyxPlatform;
  const AScheduler: INyxScheduler): INyxStudioPreviewOwner;
{ ANext=nil explicitly retires Interact during the accepted design publication.
  The caller pairs AEvents with the renderer's supplied prospective event view.
  No executable creator resolver or external service authority is supplied. }
function NewNyxStudioPreviewSource(ASlot: TNyxStudioPreviewSlot;
  const ANext: INyxStudioPreviewOwner;
  const AEvents: INyxEvents): INyxViewPublicationSource;

implementation

uses nyx.flow.routing, nyx.publication;

type
  { Forward managed wrapper declaration used by exact private ownership checks. }
  TPreviewOwner = class;
  { Forward source declaration used by its single outstanding reservation. }
  TPreviewSource = class;
  { Forward per-view mount declaration shared by slot and preparation leases. }
  TPreviewMount = class;

  { Private access is observed only before callbacks, then checked against the
    exact owned class and the public interface identity, never trusted alone. }
  IPreviewOwnerAccess = interface(IInterface)
    ['{A4FE3BE7-F823-4573-8FC1-E02E1F1EBFE6}']
    function OwnedObject: TObject;
  end;
  { A retained context can outlive its runtime but cannot revive its receiver. }
  TPreviewOwnerPort = class(TInterfacedObject)
  public
    Owner: TPreviewOwner;
    Revoked: Boolean;
  end;
  { This port retains no Studio or slot. Revoke makes every pending local fence
    fail before the raw slot is freed. }
  TPreviewSlotPort = class(TInterfacedObject)
  public
    Owner: TNyxStudioPreviewSlot;
  end;
  { Exact fixed-generation context delegates existing stores, not another state
    engine. The embedded bridge is weak; route contexts also retain the store
    owner until their detached executions and route bindings release it. }
  TPreviewContext = class(TInterfacedObject, INyxApplicationContext)
  public
    Port: TPreviewOwnerPort;
    PortLease: IInterface;
    OwnerLease: INyxStudioPreviewOwner;
    Generation: INyxCancellationScope;
    Revoked: Boolean;
    function Acquire: INyxStudioPreviewOwner;
    function GetActive: Boolean;
    function GetState: TNyxState;
    function GetCollections: INyxCollections;
    function GetScheduler: INyxScheduler;
    function GetGeneration: INyxCancellationScope;
  end;
  { Owns the unchanged raw runtime and the weak embedded-context receiver. }
  TPreviewOwner = class(TInterfacedObject, INyxStudioPreviewOwner, IPreviewOwnerAccess)
  private
    FRuntime: TNyxStudioPreviewRuntime;
    FPort: TPreviewOwnerPort;
    FPortLease: IInterface;
    FRevoked: Boolean;
    FCleaned: Boolean;
    procedure DoClose;
  public
    constructor Create(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
      APlatform: TNyxPlatform; const AScheduler: INyxScheduler);
    destructor Destroy; override;
    function OwnedObject: TObject;
    function GetRuntime: TNyxStudioPreviewRuntime;
    procedure Revoke;
    procedure Close;
  end;
  { One view's fixed context/routes. Revocation is flags-only; cleanup attempts
    every route, preparation and token after controls have stopped borrowing it. }
  TPreviewMount = class(TInterfacedObject)
  public
    Owner: TPreviewOwner;
    OwnerLease: INyxStudioPreviewOwner;
    Context: TPreviewContext;
    ContextLease: INyxApplicationContext;
    Generation: INyxCancellationScope;
    Routes: INyxFlowRouteBinding;
    Preparation: INyxPreparedPublication;
    Fence: INyxPublicationReadiness;
    Revoked: Boolean;
    Closed: Boolean;
    destructor Destroy; override;
    procedure Revoke;
    procedure Close;
    procedure RetirePreparation;
  end;
  { Renderer-retained source owns stores and its last mount, but only a weak
    reference to the Studio-owned adoption slot. }
  TPreviewSource = class(TInterfacedObject, INyxViewPublicationSource)
  public
    Port: TPreviewSlotPort;
    PortLease: IInterface;
    NextOwner: TPreviewOwner;
    NextLease: INyxStudioPreviewOwner;
    Events: INyxEvents;
    Scheduler: INyxScheduler;
    Pending: TObject;
    Mount: TPreviewMount;
    MountLease: IInterface;
    destructor Destroy; override;
    function Prepare(ADocument: TNyxDocument; ARoot: TNyxNode;
      const AView: INyxPreparedEventView): INyxPreparedPublication;
  end;
  { Preallocated slot adoption retains both sides before any assignment, so
    Install cannot finalize a runtime or enter arbitrary cleanup. }
  TPreviewPublication = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FSource: TPreviewSource;
    FSourceLease: INyxViewPublicationSource;
    FSourceFormerMountLease: IInterface;
    FPort: TPreviewSlotPort;
    FPortLease: IInterface;
    FFormerOwner: TPreviewOwner;
    FFormerLease: INyxStudioPreviewOwner;
    FFormerMount: TPreviewMount;
    FFormerMountLease: IInterface;
    FNextOwner: TPreviewOwner;
    FNextLease: INyxStudioPreviewOwner;
    FNextMount: TPreviewMount;
    FNextMountLease: IInterface;
    FView: INyxPreparedEventView;
    FViewFence: INyxPublicationReadiness;
    FRevision: Integer;
    FValidated: Boolean;
    FValidating: Boolean;
    FInstalled: Boolean;
    FNotified: Boolean;
    FRetired: Boolean;
    procedure CheckLocal;
    procedure DoNotify;
    procedure DoRetire;
  public
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

procedure RememberFailure(AError: Exception; var AFailure: TNyxText);
begin

  if AFailure = '' then
  begin
    AFailure := TNyxText(AError.Message);
  end;
end;

constructor TNyxStudioPreviewRuntime.Create(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; APlatform: TNyxPlatform; const AScheduler: INyxScheduler);
begin
  inherited Create;

  if (ADocument = nil) or (AScheduler = nil) or (APlatform = npfAny) then
  begin
    raise ENyxModel.Create('Interact runtime requires a source, concrete target and UI scheduler');
  end;
  FScheduler := AScheduler;
  FScheduler.RequireUI;
  FRoot := ARoot;
  FDocument := ADocument.Clone;
  FRuntime := TNyxApplicationState.CreateForRoot(FDocument, FRoot, APlatform);
  FGeneration := NewNyxCancellationScope;
  FLease := NewNyxApplicationContext(FScheduler,
    {$ifdef PAS2JS}@{$endif}ResolveState, {$ifdef PAS2JS}@{$endif}ResolveCollections,
    {$ifdef PAS2JS}@{$endif}ResolveGeneration);
end;

destructor TNyxStudioPreviewRuntime.Destroy;
begin
  try
    Close;
  except
    { Explicit Close reports cleanup failure; destruction still frees stores. }
  end;
  FManagedContext := nil;
  try
    FLease := nil;
  except
    { A hostile scheduler cannot skip store/source retirement in destruction. }
  end;
  try
    FRuntime.Free;
  finally
    FDocument.Free;
  end;
  inherited Destroy;
end;

procedure TNyxStudioPreviewRuntime.Close;
var
  LFailure: TNyxText;
begin

  if FCleaned then
  begin
    Exit;
  end;
  FClosed := True;
  FCleaned := True;
  LFailure := '';
  try

    if FGeneration <> nil then
    begin
      FGeneration.Cancel;
    end;
  except
    on E: Exception do RememberFailure(E, LFailure);
  end;
  try

    if FLease <> nil then
    begin
      FLease.Close;
    end;
  except
    on E: Exception do RememberFailure(E, LFailure);
  end;

  if LFailure <> '' then
  begin
    raise ENyxApplicationContext.Create(LFailure);
  end;
end;

function TNyxStudioPreviewRuntime.ResolveState: TNyxState;
begin
  FScheduler.RequireUI;

  if FClosed then
  begin
    raise ENyxApplicationContext.Create('Studio Interact runtime is retired');
  end;
  Result := FRuntime.State;
end;

function TNyxStudioPreviewRuntime.ResolveCollections: INyxCollections;
begin
  ResolveState;
  Result := FRuntime.Collections;
end;

function TNyxStudioPreviewRuntime.ResolveGeneration: INyxCancellationScope;
begin
  ResolveState;
  Result := FGeneration;
end;

function TNyxStudioPreviewRuntime.GetContext: INyxApplicationContext;
begin

  if FManagedContext <> nil then
  begin
    Exit(FManagedContext);
  end;
  Result := FLease.Context;
end;

function TNyxStudioPreviewRuntime.GetBindings: INyxCollectionBindings;
begin
  ResolveState;
  Result := FRuntime.RootCollections(FRoot);
end;

function TPreviewContext.Acquire: INyxStudioPreviewOwner;
var
  LOwner: TPreviewOwner;
begin
  Result := nil;

  if Revoked or Port.Revoked then
  begin
    Exit;
  end;
  LOwner := Port.Owner;

  if (LOwner <> nil) and not LOwner.FRevoked and not LOwner.FRuntime.FClosed then
  begin
    Result := LOwner;
  end;
end;

function TPreviewContext.GetActive: Boolean;
var
  LContext: INyxApplicationContext;
  LOwner: INyxStudioPreviewOwner;
begin
  LContext := Self;
  LOwner := Acquire;
  Result := (LOwner <> nil) and (Generation <> nil);

  if Result then
  begin
    Result := not Generation.Cancelled and not Revoked and not Port.Revoked;

    if Result then
    begin
      LOwner := Acquire;
      Result := (LOwner <> nil) and not Revoked and not Port.Revoked;
    end;
  end;
end;

function TPreviewContext.GetState: TNyxState;
var
  LContext: INyxApplicationContext;
  LOwner: INyxStudioPreviewOwner;
begin
  LContext := Self;
  LOwner := Acquire;

  if (LOwner = nil) or not GetActive then
  begin
    raise ENyxApplicationContext.Create('Studio preview view context is retired');
  end;
  Result := Port.Owner.FRuntime.ResolveState;
end;

function TPreviewContext.GetCollections: INyxCollections;
var
  LContext: INyxApplicationContext;
  LOwner: INyxStudioPreviewOwner;
begin
  LContext := Self;
  LOwner := Acquire;
  GetState;
  Result := Port.Owner.FRuntime.FRuntime.Collections;
end;

function TPreviewContext.GetScheduler: INyxScheduler;
var
  LContext: INyxApplicationContext;
  LOwner: INyxStudioPreviewOwner;
begin
  LContext := Self;
  LOwner := Acquire;
  GetState;
  Result := Port.Owner.FRuntime.FScheduler;
end;

function TPreviewContext.GetGeneration: INyxCancellationScope;
var
  LContext: INyxApplicationContext;
  LOwner: INyxStudioPreviewOwner;
begin
  LContext := Self;
  LOwner := Acquire;
  GetState;
  Result := Generation;
end;

constructor TPreviewOwner.Create(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; APlatform: TNyxPlatform;
  const AScheduler: INyxScheduler);
var
  LContext: TPreviewContext;
begin
  inherited Create;
  FPort := TPreviewOwnerPort.Create;
  FPortLease := FPort;
  FPort.Owner := Self;
  FRuntime := TNyxStudioPreviewRuntime.Create(ADocument, ARoot, APlatform, AScheduler);
  LContext := TPreviewContext.Create;
  FRuntime.FManagedContext := LContext;
  LContext.Port := FPort;
  LContext.PortLease := FPortLease;
  LContext.Generation := FRuntime.FGeneration;
end;

destructor TPreviewOwner.Destroy;
begin
  Revoke;

  if FPort <> nil then
  begin
    FPort.Owner := nil;
  end;
  try
    DoClose;
  except
    { Explicit cleanup reports failure without skipping raw runtime retirement. }
  end;
  FRuntime.Free;
  inherited Destroy;
end;

function TPreviewOwner.OwnedObject: TObject;
begin
  Result := Self;
end;

function TPreviewOwner.GetRuntime: TNyxStudioPreviewRuntime;
begin

  if FRevoked or FRuntime.FClosed then
  begin
    raise ENyxApplicationContext.Create('Studio preview owner is revoked');
  end;
  Result := FRuntime;
end;

procedure TPreviewOwner.Revoke;
begin
  FRevoked := True;

  if FPort <> nil then
  begin
    FPort.Revoked := True;
  end;

  if FRuntime <> nil then
  begin
    FRuntime.FClosed := True;
  end;
end;

procedure TPreviewOwner.DoClose;
begin
  Revoke;

  if FCleaned then
  begin
    Exit;
  end;
  FCleaned := True;

  if FRuntime <> nil then
  begin
    FRuntime.Close;
  end;
end;

procedure TPreviewOwner.Close;
var
  LOwner: INyxStudioPreviewOwner;
begin
  LOwner := Self;
  DoClose;
end;

destructor TPreviewMount.Destroy;
begin
  try
    Close;
  except
    { Every cleanup was attempted by Close; destruction cannot revive a view. }
  end;
  inherited Destroy;
end;

procedure TPreviewMount.Revoke;
begin
  Revoked := True;

  if Context <> nil then
  begin
    Context.Revoked := True;
  end;
end;

procedure TPreviewMount.RetirePreparation;
var
  LPreparation: INyxPreparedPublication;
begin
  LPreparation := Preparation;
  Preparation := nil;
  Fence := nil;

  if LPreparation <> nil then
  begin
    LPreparation.Retire;
  end;
end;

procedure TPreviewMount.Close;
var
  LFailure: TNyxText;
begin
  Revoke;

  if Closed then
  begin
    Exit;
  end;
  Closed := True;
  LFailure := '';
  try
    RetirePreparation;
  except
    on E: Exception do RememberFailure(E, LFailure);
  end;
  try

    if Routes <> nil then
    begin
      Routes.Close;
    end;
  except
    on E: Exception do RememberFailure(E, LFailure);
  end;
  try

    if Generation <> nil then
    begin
      Generation.Cancel;
    end;
  except
    on E: Exception do RememberFailure(E, LFailure);
  end;
  try
    Routes := nil;
  except
    on E: Exception do RememberFailure(E, LFailure);
  end;
  Context := nil;
  try
    ContextLease := nil;
  except
    on E: Exception do RememberFailure(E, LFailure);
  end;

  if LFailure <> '' then
  begin
    raise ENyxApplicationContext.Create(LFailure);
  end;
end;

constructor TNyxStudioPreviewSlot.Create(const AScheduler: INyxScheduler);
var
  LPort: TPreviewSlotPort;
begin
  inherited Create;

  if AScheduler = nil then
  begin
    raise ENyxApplicationContext.Create('Preview slot requires its canvas scheduler');
  end;
  FScheduler := AScheduler;
  FScheduler.RequireUI;
  LPort := TPreviewSlotPort.Create;
  FPort := LPort;
  FPortLease := LPort;
  LPort.Owner := Self;
end;

destructor TNyxStudioPreviewSlot.Destroy;
begin
  try
    Close;
  except
    { Explicit Close reports failures after attempting every owned cleanup. }
  end;
  inherited Destroy;
end;

function TNyxStudioPreviewSlot.GetCurrent: INyxStudioPreviewOwner;
begin
  Result := FCurrent;
end;

procedure TNyxStudioPreviewSlot.Revoke;
begin

  if FPort <> nil then
  begin
    TPreviewSlotPort(FPort).Owner := nil;
  end;

  if FMount <> nil then
  begin
    TPreviewMount(FMount).Revoke;
  end;

  if FCurrentObject <> nil then
  begin
    TPreviewOwner(FCurrentObject).Revoke;
  end;
end;

procedure TNyxStudioPreviewSlot.Close;
var
  LOwner: TPreviewOwner;
  LOwnerLease: INyxStudioPreviewOwner;
  LMount: TPreviewMount;
  LMountLease: IInterface;
  LFailure: TNyxText;
begin
  Revoke;

  if FClosed then
  begin
    Exit;
  end;
  FClosed := True;
  LOwner := TPreviewOwner(FCurrentObject);
  LOwnerLease := FCurrent;
  LMount := TPreviewMount(FMount);
  LMountLease := FMountLease;
  FCurrent := nil;
  FCurrentObject := nil;
  FMount := nil;
  FMountLease := nil;
  LFailure := '';
  try

    if LMount <> nil then
    begin
      LMount.Close;
    end;
  except
    on E: Exception do RememberFailure(E, LFailure);
  end;
  try

    if LOwner <> nil then
    begin
      LOwner.Close;
    end;
  except
    on E: Exception do RememberFailure(E, LFailure);
  end;

  if LFailure <> '' then
  begin
    raise ENyxApplicationContext.Create(LFailure);
  end;
end;

destructor TPreviewSource.Destroy;
begin

  if Mount <> nil then
  begin
    try
      Mount.Close;
    except
      { Renderer/slot cleanup owns explicit reporting; this is a final fallback. }
    end;
  end;
  inherited Destroy;
end;

function TPreviewSource.Prepare(ADocument: TNyxDocument; ARoot: TNyxNode;
  const AView: INyxPreparedEventView): INyxPreparedPublication;
var
  LPlan: TPreviewPublication;
  LLease: INyxPreparedPublication;
  LSlot: TNyxStudioPreviewSlot;
  LMount: TPreviewMount;
  LContext: TPreviewContext;
begin
  LSlot := Port.Owner;

  if (LSlot = nil) or (Pending <> nil) or (LSlot.FReservation <> nil) or
    (LSlot.FRevision = High(Integer)) or (ADocument = nil) or
    (ARoot = nil) or not ARoot.IsRealized or (AView = nil) then
  begin
    raise ENyxApplicationContext.Create('Preview source requires an unreserved realized view');
  end;
  LPlan := TPreviewPublication.Create;
  LLease := LPlan;
  LPlan.FSource := Self;
  LPlan.FSourceLease := Self;
  LPlan.FSourceFormerMountLease := MountLease;
  LPlan.FPort := Port;
  LPlan.FPortLease := PortLease;
  LPlan.FRevision := LSlot.FRevision;
  LPlan.FFormerOwner := TPreviewOwner(LSlot.FCurrentObject);
  LPlan.FFormerLease := LSlot.FCurrent;
  LPlan.FFormerMount := TPreviewMount(LSlot.FMount);
  LPlan.FFormerMountLease := LSlot.FMountLease;
  LPlan.FNextOwner := NextOwner;
  LPlan.FNextLease := NextLease;
  LPlan.FView := AView;
  Pending := LPlan;
  LSlot.FReservation := LPlan;

  if not Supports(LPlan.FView, INyxPublicationReadiness, LPlan.FViewFence) then
  begin
    raise ENyxApplicationContext.Create('Preview source requires final event readiness');
  end;
  Scheduler.RequireUI;
  LPlan.CheckLocal;

  if Events.Scheduler <> Scheduler then
  begin
    raise ENyxApplicationContext.Create('Preview router scheduler changed during preparation');
  end;
  LPlan.CheckLocal;

  if NextOwner <> nil then
  begin
    LMount := TPreviewMount.Create;
    LPlan.FNextMount := LMount;
    LPlan.FNextMountLease := LMount;
    LMount.Owner := NextOwner;
    LMount.OwnerLease := NextLease;
    LMount.Generation := LPlan.FView.Generation;
    LPlan.CheckLocal;
    LContext := TPreviewContext.Create;
    LMount.Context := LContext;
    LMount.ContextLease := LContext;
    LContext.Port := NextOwner.FPort;
    LContext.PortLease := NextOwner.FPortLease;
    LContext.OwnerLease := NextLease;
    LContext.Generation := LMount.Generation;
    LMount.Preparation := PrepareNyxViewFlowRoutes(ADocument, ARoot, Events,
      LPlan.FView, LMount.ContextLease, nil, LMount.Routes);

    if not Supports(LMount.Preparation, INyxPublicationReadiness, LMount.Fence) then
    begin
      raise ENyxApplicationContext.Create('Preview routes require final readiness');
    end;
    LPlan.CheckLocal;
  end;
  Result := LLease;
end;

destructor TPreviewPublication.Destroy;
begin
  DoRetire;
  inherited Destroy;
end;

procedure TPreviewPublication.CheckLocal;
var
  LSlot: TNyxStudioPreviewSlot;
begin

  if FRetired or FInstalled or (FPort = nil) then
  begin
    raise ENyxApplicationContext.Create('Preview adoption is no longer preparing');
  end;
  LSlot := FPort.Owner;

  if (LSlot = nil) or
    (LSlot.FRevision <> FRevision) or (LSlot.FCurrent <> FFormerLease) or
    (LSlot.FMount <> FFormerMount) or (LSlot.FReservation <> Self) or
    (FSource.Pending <> Self) then
  begin
    raise ENyxApplicationContext.Create('Preview adoption lost its exact slot reservation');
  end;

  if (FNextOwner <> nil) and
    (FNextOwner.FRevoked or FNextOwner.FRuntime.FClosed) then
  begin
    raise ENyxApplicationContext.Create('Preview candidate owner was revoked');
  end;

  if (FFormerOwner <> nil) and
    (FFormerOwner.FRevoked or FFormerOwner.FRuntime.FClosed) then
  begin
    raise ENyxApplicationContext.Create('Preview former owner changed during admission');
  end;

  if (FNextMount <> nil) and (FNextMount.Revoked or
    ((FNextMount.Context <> nil) and FNextMount.Context.Revoked)) then
  begin
    raise ENyxApplicationContext.Create('Preview candidate context was revoked');
  end;
end;

procedure TPreviewPublication.Validate;
var
  LLease: INyxPreparedPublication;
begin
  LLease := Self;

  if FValidated or FValidating then
  begin
    raise ENyxApplicationContext.Create('Preview adoption validates once');
  end;
  FValidating := True;
  try
    CheckLocal;
    FSource.Scheduler.RequireUI;
    CheckLocal;

    if FSource.Events.Scheduler <> FSource.Scheduler then
    begin
      raise ENyxApplicationContext.Create('Preview router scheduler changed during validation');
    end;
    CheckLocal;

    if FNextMount <> nil then
    begin
      FNextMount.Preparation.Validate;
      CheckLocal;
    end;
    FValidated := True;
  finally
    FValidating := False;
  end;
end;

procedure TPreviewPublication.CheckReady;
var
  LLease: INyxPreparedPublication;
begin
  LLease := Self;

  if not FValidated then
  begin
    raise ENyxApplicationContext.Create('Preview adoption requires validation');
  end;
  CheckLocal;

  if FNextMount <> nil then
  begin
    FNextMount.Fence.CheckReady;
    CheckLocal;
  end;
  FViewFence.CheckReady;
  CheckLocal;
end;

procedure TPreviewPublication.Install;
var
  LSlot: TNyxStudioPreviewSlot;
begin

  if not FValidated or FInstalled or FRetired then
  begin
    Exit;
  end;
  LSlot := FPort.Owner;

  if FFormerMount <> nil then
  begin
    FFormerMount.Revoke;
  end;

  if (FFormerOwner <> nil) and (FFormerOwner <> FNextOwner) then
  begin
    FFormerOwner.Revoke;
  end;

  if FNextMount <> nil then
  begin
    FNextMount.Preparation.Install;
  end;
  LSlot.FCurrent := FNextLease;
  LSlot.FCurrentObject := FNextOwner;
  LSlot.FMount := FNextMount;
  LSlot.FMountLease := FNextMountLease;
  Inc(LSlot.FRevision);
  FSource.Mount := FNextMount;
  FSource.MountLease := FNextMountLease;
  FInstalled := True;
end;

procedure TPreviewPublication.DoNotify;
var
  LFormerMount: TPreviewMount;
  LFormerMountLease: IInterface;
  LFormerOwner: TPreviewOwner;
  LFormerLease: INyxStudioPreviewOwner;
  LCloseOwner: Boolean;
  LFailure: TNyxText;
begin

  if not FInstalled or FNotified then
  begin
    Exit;
  end;
  FNotified := True;
  { Public Retire may reenter from route/context cleanup and release every
    preparation field. Keep both receivers and the independent cleanup decision
    in local leases until all old cleanup has been attempted. }
  LFormerMount := FFormerMount;
  LFormerMountLease := FFormerMountLease;
  LFormerOwner := FFormerOwner;
  LFormerLease := FFormerLease;
  LCloseOwner := (LFormerOwner <> nil) and (LFormerOwner <> FNextOwner);
  LFailure := '';
  try

    if LFormerMount <> nil then
    begin
      LFormerMount.Close;
    end;
  except
    on E: Exception do RememberFailure(E, LFailure);
  end;
  try

    if LCloseOwner then
    begin
      LFormerOwner.Close;
    end;
  except
    on E: Exception do RememberFailure(E, LFailure);
  end;

  if LFailure <> '' then
  begin
    raise ENyxPublicationNotification.CreateReceiverFailure(LFailure);
  end;
end;

procedure TPreviewPublication.Notify;
var
  LLease: INyxPreparedPublication;
begin
  LLease := Self;
  DoNotify;
end;

procedure TPreviewPublication.DoRetire;
var
  LSlot: TNyxStudioPreviewSlot;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;
  try

    if FInstalled then
    begin
      DoNotify;
    end
    else if FNextMount <> nil then
    begin
      FNextMount.Close;
    end;
  except
    { Retirement is a fallback which never skips reservation release below. }
  end;
  try

    if FInstalled and (FNextMount <> nil) then
    begin
      FNextMount.RetirePreparation;
    end;
  except
    { The installed route owner remains usable; every cleanup got an attempt. }
  end;
  LSlot := FPort.Owner;

  if (LSlot <> nil) and (LSlot.FReservation = Self) then
  begin
    LSlot.FReservation := nil;
  end;

  if (FSource <> nil) and (FSource.Pending = Self) then
  begin
    FSource.Pending := nil;
  end;
  FSource := nil;
  FFormerOwner := nil;
  FFormerMount := nil;
  FNextOwner := nil;
  FNextMount := nil;
  FPort := nil;
  { Each independent lease gets its release attempt. Installed mounts remain
    owned by the slot/source; a retained retired plan owns no preparation frame.
    Unexpected extension finalizers cannot skip the remaining releases. }
  try
    FSourceLease := nil;
  except
  end;
  try
    FViewFence := nil;
  except
  end;
  try
    FView := nil;
  except
  end;
  try
    FSourceFormerMountLease := nil;
  except
  end;
  try
    FFormerMountLease := nil;
  except
  end;
  try
    FNextMountLease := nil;
  except
  end;
  try
    FFormerLease := nil;
  except
  end;
  try
    FNextLease := nil;
  except
  end;
  try
    FPortLease := nil;
  except
  end;
end;

procedure TPreviewPublication.Retire;
var
  LLease: INyxPreparedPublication;
begin
  LLease := Self;
  DoRetire;
end;

function NewNyxStudioPreviewOwner(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; APlatform: TNyxPlatform;
  const AScheduler: INyxScheduler): INyxStudioPreviewOwner;
begin
  Result := TPreviewOwner.Create(ADocument, ARoot, APlatform, AScheduler);
end;

function NewNyxStudioPreviewSource(ASlot: TNyxStudioPreviewSlot;
  const ANext: INyxStudioPreviewOwner;
  const AEvents: INyxEvents): INyxViewPublicationSource;
var
  LPort: TPreviewSlotPort;
  LPortLease: IInterface;
  LOwnerLease: INyxStudioPreviewOwner;
  LOwner: TPreviewOwner;
  LAccess: IPreviewOwnerAccess;
  LObject: TObject;
  LSource: TPreviewSource;
  LEvents: INyxEvents;
  LScheduler: INyxScheduler;
begin
  LOwnerLease := ANext;
  LEvents := AEvents;

  if (ASlot = nil) or (LEvents = nil) then
  begin
    raise ENyxApplicationContext.Create('Preview source requires its slot and event router');
  end;
  LPort := TPreviewSlotPort(ASlot.FPort);
  LPortLease := ASlot.FPortLease;
  LScheduler := ASlot.FScheduler;
  LOwner := nil;

  if LOwnerLease <> nil then
  begin

    if not Supports(LOwnerLease, IPreviewOwnerAccess, LAccess) then
    begin
      raise ENyxApplicationContext.Create('Preview source requires an owned runtime wrapper');
    end;
    LObject := LAccess.OwnedObject;

    if (LObject = nil) or (LObject.ClassType <> TPreviewOwner) then
    begin
      raise ENyxApplicationContext.Create('Preview source requires the exact runtime wrapper');
    end;
    LOwner := TPreviewOwner(LObject);

    if INyxStudioPreviewOwner(LOwner) <> LOwnerLease then
    begin
      raise ENyxApplicationContext.Create('Preview runtime wrapper identity differs');
    end;
  end;
  LScheduler.RequireUI;

  if (LPort.Owner = nil) or (LEvents.Scheduler <> LScheduler) then
  begin
    raise ENyxApplicationContext.Create('Preview source requires the live canvas scheduler');
  end;

  if (LPort.Owner = nil) or ((LOwner <> nil) and
    (LOwner.FRevoked or LOwner.FRuntime.FClosed or
      (LOwner.FRuntime.FScheduler <> LScheduler))) then
  begin
    raise ENyxApplicationContext.Create('Preview owner or slot changed during admission');
  end;
  LSource := TPreviewSource.Create;
  Result := LSource;
  LSource.Port := LPort;
  LSource.PortLease := LPortLease;
  LSource.NextOwner := LOwner;
  LSource.NextLease := LOwnerLease;
  LSource.Events := LEvents;
  LSource.Scheduler := LScheduler;
end;

end.
