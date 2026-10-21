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
unit nyx.kanban;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Authoring and runtime operations in this unit are UI-thread confined, including
  Configure, Refresh, CaptureMove, Commit, Cancel and Disconnect. Copied options
  carry values only; the move's plain Cancel flag is not a cross-thread token. }

uses
  SysUtils, nyx.text, nyx.controls, nyx.collections, nyx.collections.view.types,
  nyx.collections.bindings, nyx.events, nyx.scheduler, nyx.model,
  nyx.publication.types, nyx.coordinators;

type
  { Kanban admission/refusal preserves the existing collection exception family. }
  ENyxKanban = class(ENyxCollection);
  { A disconnected/replaced attachment or cancelled move never grants a new
    publication. Cancellation after installation does not undo committed data. }
  ENyxKanbanCancelled = class(ENyxKanban);

  { Open, copied lane identity. It is a field value, not a row number, widget ID,
    collection capability or closed workflow enumeration. }
  TNyxKanbanLaneRef = record
  private
    FName: TNyxText;
    FDefined: Boolean;
    function GetName: TNyxText;
  public
    property Name: TNyxText read GetName;
    property Defined: Boolean read FDefined;
  end;

  { Copied authoring options for the existing three-lane board. Builders detach
    their private value vectors; no interface, store or document is embedded. }
  TNyxKanbanOptions = record
  private
    FCollection: TNyxCollectionRef;
    FTitle: TNyxTextFieldRef;
    FField: TNyxTextFieldRef;
    FLanes: array of TNyxKanbanLaneRef;
    FCaptions: array of TNyxText;
    FScope: TNyxCollectionScope;
    FDefined: Boolean;
    function GetCount: Integer;
  public
    function Lane(const ARef: TNyxKanbanLaneRef;
      const ACaption: TNyxText): TNyxKanbanOptions;
    function Scoped(AScope: TNyxCollectionScope): TNyxKanbanOptions;
    function Copy: TNyxKanbanOptions;
    function LaneAt(AIndex: Integer): TNyxKanbanLaneRef;
    function CaptionAt(AIndex: Integer): TNyxText;
    procedure Validate;
    property Collection: TNyxCollectionRef read FCollection;
    property TitleField: TNyxTextFieldRef read FTitle;
    property LaneField: TNyxTextFieldRef read FField;
    property Scope: TNyxCollectionScope read FScope;
    property Count: Integer read GetCount;
  end;

  { Borrowed presentation callback, valid only while the attachment is connected.
    It runs last, so it may navigate, disconnect or dispose its renderer. }
  TNyxKanbanRefresh = procedure of object;

  { One-shot move captured from one exact live attachment/store revision. It
    retains copied intent and a revocable port, never a renderer or document.
    Committed becomes true at installation, including when Notify later raises
    ENyxCollectionNotification. Never retry a committed operation. }
  INyxKanbanMove = interface(IInterface)
    ['{D632DF5E-DAB5-4939-9388-5F4D9C79BCE4}']
    procedure Commit;
    procedure Cancel;
    function GetCommitted: Boolean;
    property Committed: Boolean read GetCommitted;
  end;

  { Explicit code-first runtime attachment to an actual realized board. Its
    three views share one atomic store. Keyboard Move and local selected-card
    drag/drop use the same captured move. Disconnect before retiring the page;
    subscription callbacks keep only a weak, revocable port to this owner.
    Move is a synchronous before-callback default via INyxEventObservation;
    application Click policy is preserved. Drag negotiation requires sequential
    physical response authority; queued/threaded gestures cannot negotiate. }
  INyxKanbanBinding = interface(IInterface)
    ['{C23B66EE-6271-48A1-9768-E1BC268F3BF6}']
    procedure Disconnect;
    procedure Refresh;
    function GetConnected: Boolean;
    function Items(const ALane: TNyxKanbanLaneRef): INyxList;
    function MoveButton(const ALane: TNyxKanbanLaneRef): INyxButton;
    function CaptureMove(const AItem: TNyxItemRef;
      const AFromLane, AToLane: TNyxKanbanLaneRef;
      AExpectedRevision: Integer): INyxKanbanMove;
    property Connected: Boolean read GetConnected;
  end;

  { One staged automatic attachment, retained by the renderer for its view
    lifetime. Binding is nil for a disabled availability mask. Revoke only
    closes weak input authority; Disconnect later attempts all cleanup. Retire
    abandons an uninstalled preparation, but retains installed behavior until
    Disconnect/destruction. Coordinate Install after the event plan installs and
    the accepted renderer adopts its nodes. Install never calls the host. }
  INyxPreparedKanbanBinding = interface(INyxPreparedPublication)
    ['{6F84A65A-5B49-4825-BC28-0BD052209CF7}']
    function GetBinding: INyxKanbanBinding;
    procedure Revoke;
    procedure Disconnect;
    property Binding: INyxKanbanBinding read GetBinding;
  end;

function NyxKanbanLane(const AName: TNyxText): TNyxKanbanLaneRef;
function NyxKanbanOptions(const ACollection: TNyxCollectionRef;
  const ATitleField, ALaneField: TNyxTextFieldRef): TNyxKanbanOptions;

{ Configure an authored board before mounting, using ordinary persisted list
  bindings and buttons. Existing Add actions/default recipe remain unchanged.
  Exactly three lanes are this recipe's current compatibility boundary. }
procedure ConfigureNyxKanbanBoard(const ABoard: INyxKanbanBoard;
  const AOptions: TNyxKanbanOptions);

{ Attach after mounting, using retained actual runtime board, bindings and event
  generation. Missing/retyped parts, mismatched views/stores, unsupported atomic
  publication and stale generations refuse. Configuration is not automatically
  installed by generated Pascal: an application explicitly owns this attachment. }
function NewNyxKanbanBinding(const ABoard: INyxKanbanBoard;
  const AOptions: TNyxKanbanOptions;
  const AViews: INyxCollectionBindings;
  const AEvents: INyxEvents;
  const AScheduler: INyxScheduler;
  ARefresh: TNyxKanbanRefresh): INyxKanbanBinding;

{ Shared behavioral declaration admission. It preserves independently authored
  captions, styling and payloads, while requiring the exact three list roles. }
procedure ValidateNyxKanbanBoardOptions(ANode: TNyxNode;
  const AOptions: TNyxKanbanOptions);

{ Prepare before Build/Sync against the accepted router's prospective event
  plan. ARefresh borrows the final accepted receiver, never a temporary renderer.
  Views must expose their existing publication-readiness capability. Validate
  performs outward admission; its final readiness fence calls no view getters. }
function PrepareNyxKanbanBinding(const ABoard: INyxKanbanBoard;
  const AOptions: TNyxKanbanOptions; const AViews: INyxCollectionBindings;
  const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  ARefresh: TNyxKanbanRefresh): INyxPreparedKanbanBinding;

{ Disabled intent masks only recognized existing Move buttons. It requires no
  collection and never reconstructs or removes customized parts. }
function PrepareNyxKanbanDisabled(const ABoard: INyxKanbanBoard;
  const AEventView: INyxPreparedEventView;
  const AScheduler: INyxScheduler): INyxPreparedKanbanBinding;

{ The common driver supplies the canonical realized board to this built-in
  adapter. Manual attachment and the existing specialized binding stay intact. }
function NewNyxKanbanCoordinatorFactory: INyxCoordinatorFactory;

implementation

uses
  nyx.types, nyx.data, nyx.state, nyx.behavior, nyx.collections.view,
  nyx.collections.query, nyx.collections.selection, nyx.interaction.policy,
  nyx.gestures, nyx.publication, nyx.kanban.intent, nyx.coordinators.internal;

const
  CKanbanLaneCount = 3;
  CMovePart: TNyxText = 'move';
  CMoveText: TNyxText = 'Move selected card here';
  CTransferFormat: TNyxText = 'application/x-nyx-kanban-selection';

type
  { Forward declaration of the operation-local owner used by its weak port. }
  TKanbanBinding = class;
  { Forward declaration of a captured one-shot move owned by its caller. }
  TKanbanMove = class;

  { Shared revocable indirection. Callbacks, policies and captured commands retain
    this port, which borrows the attachment and owns no store or control. }
  IKanbanPort = interface(IInterface)
    function Acquire: INyxKanbanBinding;
    function Owner: TKanbanBinding;
    procedure Revoke;
  end;

  { Private attachment marker prevents two command owners on the same mounted
    Move face. Retired projection copies may drop their obsolete constraint. }
  IKanbanEnabled = interface(INyxRuntimeEnabledPolicy)
    ['{258D6AB9-7994-470C-82A1-FAED2BF46BE8}']
    function GetConnected: Boolean;
  end;

  { Router registrations own this callback independently of the page. Revocation
    makes already snapshotted callbacks inert without retaining the page owner. }
  TKanbanPort = class(TNyxEventCallback, IKanbanPort)
  private
    FOwner: TKanbanBinding;
  public
    constructor Create(AOwner: TKanbanBinding);
    function Acquire: INyxKanbanBinding;
    function Owner: TKanbanBinding;
    procedure Revoke;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;

  { Default Move and drag-end cleanup run synchronously through the existing
    before-callback observation boundary, independent of application scheduling.
    This receiver owns only the same revocable port as ordinary subscriptions. }
  TKanbanObservation = class(TNyxEventObserver)
  private
    FPort: IKanbanPort;
  public
    constructor Create(const APort: IKanbanPort);
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
  end;

  { Runtime availability is an additional constraint, never a replacement for
    authored Enabled/read-only configuration. A detached policy stays inert. }
  TKanbanEnabled = class(TInterfacedObject, INyxRuntimeEnabledPolicy, IKanbanEnabled)
  private
    FPort: IKanbanPort;
    FDestination: Integer;
  public
    constructor Create(const APort: IKanbanPort; ADestination: Integer);
    function GetAllowed: Boolean;
    function GetConnected: Boolean;
  end;

  { A captured command owns only copied intent and revocable lifetime handles.
    A local owner lease during Commit protects reentrant caller-slot release. }
  TKanbanMove = class(TInterfacedObject, INyxKanbanMove)
  private
    FPort: IKanbanPort;
    FItem: TNyxItemRef;
    FFrom: Integer;
    FTo: Integer;
    FRevision: Integer;
    FAttempted: Boolean;
    FCancelled: Boolean;
    FCommitted: Boolean;
    FExecution: INyxExecution;
  public
    constructor Create(const APort: IKanbanPort; const AItem: TNyxItemRef;
      AFrom, ATo, ARevision: Integer; const AExecution: INyxExecution);
    procedure Commit;
    procedure Cancel;
    function GetCommitted: Boolean;
  end;

  { One ordinary board attachment. Arrays have three admitted entries and are
    never exposed. The only raw pointer is its private, revocable callback port;
    no renderer, document, widget or platform transfer is retained. }
  TKanbanBinding = class(TInterfacedObject, INyxKanbanBinding,
    INyxCoordinatorBinding)
  private
    FBoard: INyxKanbanBoard;
    FOptions: TNyxKanbanOptions;
    FViews: array of INyxCollectionView;
    FItems: array of INyxList;
    FMoves: array of INyxButton;
    FPolicies: array of INyxRuntimeEnabledPolicy;
    FSubscriptions: array of INyxEventSubscription;
    FViewTokens: array of INyxCollectionViewSubscription;
    FStore: INyxCollection;
    FAtomic: INyxAtomicCollection;
    FGeneration: INyxCancellationScope;
    FScheduler: INyxScheduler;
    FRefresh: TNyxKanbanRefresh;
    FScopedRefresh: INyxCoordinatorRefresh;
    FProjectionLifetime: INyxCoordinatorProjectionLifetime;
    FPort: IKanbanPort;
    FCallback: TKanbanPort;
    FConnected: Boolean;
    FPrepared: Boolean;
    FDisabled: Boolean;
    FRevoked: Boolean;
    FDirty: Boolean;
    FCleaned: Boolean;
    FBoardNode: TNyxNode;
    FBoardParent: TNyxNode;
    FBoardProps: TNyxText;
    FBoardExtensions: TNyxText;
    FItemNodes: array[0..2] of TNyxNode;
    FMoveNodes: array[0..2] of TNyxNode;
    FItemProps: array[0..2] of TNyxText;
    FMoveProps: array[0..2] of TNyxText;
    FReadOnly: array[0..2] of Boolean;
    FAllowed: array[0..2] of Boolean;
    FQueries: array[0..2] of TNyxText;
    FReadiness: array of INyxCollectionViewPublication;
    FActive: Integer;
    FDragItem: TNyxItemRef;
    FDragLane: Integer;
    FDragRevision: Integer;
    function IndexOf(const ALane: TNyxKanbanLaneRef): Integer;
    procedure RequireLive;
    procedure RequireContract;
    procedure CheckMove(const AItem: TNyxItemRef; AFrom, ATo,
      ARevision: Integer);
    function NewMove(const AItem: TNyxItemRef; AFrom, ATo,
      ARevision: Integer; const AExecution: INyxExecution): INyxKanbanMove;
    procedure CommitMove(AMove: TKanbanMove);
    function SelectedItem(AFrom: Integer; out AItem: TNyxItemRef): Boolean;
    function CanMove(ATo: Integer): Boolean;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure Handle(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution);
    procedure Observe(const AEvent: TNyxEventInfo);
    procedure Subscribe(const AEvents: INyxEvents; ATrigger: TNyxTrigger;
      const AID: TNyxText);
    procedure ClearDrag;
    procedure Retire;
    procedure CheckPrepared;
    procedure CheckPreparedNodes;
    procedure AdmitPreparedContract;
    procedure CacheAvailability;
    procedure RevokePrepared;
    procedure DisconnectPrepared;
  public
    constructor Create(const ABoard: INyxKanbanBoard;
      const AOptions: TNyxKanbanOptions;
      const AViews: INyxCollectionBindings; const AEvents: INyxEvents;
      const AScheduler: INyxScheduler; ARefresh: TNyxKanbanRefresh);
    destructor Destroy; override;
    constructor CreatePrepared(const ABoard: INyxKanbanBoard;
      const AOptions: TNyxKanbanOptions; const AViews: INyxCollectionBindings;
      const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      ARefresh: TNyxKanbanRefresh; ADisabled: Boolean;
      const AScopedRefresh: INyxCoordinatorRefresh = nil;
      const AProjectionLifetime: INyxCoordinatorProjectionLifetime = nil);
    procedure Activate(const AEvents: INyxEvents);
    procedure Disconnect;
    procedure Refresh;
    function GetConnected: Boolean;
    function Items(const ALane: TNyxKanbanLaneRef): INyxList;
    function MoveButton(const ALane: TNyxKanbanLaneRef): INyxButton;
    function CaptureMove(const AItem: TNyxItemRef;
      const AFromLane, AToLane: TNyxKanbanLaneRef;
      AExpectedRevision: Integer): INyxKanbanMove;
  end;

  { Preparation owns the future event lease until retirement and the installed
    attachment until explicit disconnection. It contains no renderer reference;
    the borrowed final Refresh method is protected by the binding's weak port. }
  TPreparedKanban = class(TInterfacedObject, INyxPreparedPublication,
    INyxPreparedKanbanBinding, INyxPublicationReadiness)
  private
    FEventView: INyxPreparedEventView;
    FBindingLease: INyxKanbanBinding;
    FOwner: TKanbanBinding;
    FValidated: Boolean;
    FInstalled: Boolean;
    FRetired: Boolean;
    procedure DoDisconnect;
    procedure DoRetire;
  public
    constructor Create(const ABoard: INyxKanbanBoard;
      const AOptions: TNyxKanbanOptions; const AViews: INyxCollectionBindings;
      const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      ARefresh: TNyxKanbanRefresh; ADisabled: Boolean;
      const AScopedRefresh: INyxCoordinatorRefresh = nil;
      const AProjectionLifetime: INyxCoordinatorProjectionLifetime = nil);
    destructor Destroy; override;
    function GetBinding: INyxKanbanBinding;
    procedure Revoke;
    procedure Disconnect;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { The common lifetime delegates to the existing prepared engine. Configured
    lookup returns its exact specialized binding; a disabled mask has only this
    base lifetime, preserving the old nil specialized lookup contract. }
  TCoordinatorKanbanPlan = class(TInterfacedObject, INyxPreparedCoordinator,
    INyxPreparedPublication, INyxPublicationReadiness, INyxCoordinatorBinding)
  private
    FOwner: TPreparedKanban;
    FPrepared: INyxPreparedKanbanBinding;
    FReadiness: INyxPublicationReadiness;
    FBinding: INyxCoordinatorBinding;
  public
    constructor Create(AOwner: TPreparedKanban;
      const APrepared: INyxPreparedKanbanBinding);
    function GetBinding: INyxCoordinatorBinding;
    function GetConnected: Boolean;
    procedure Revoke;
    procedure Disconnect;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { Stateless library adapter receives the exact canonical control only through
    the internal driver seam. Ordinary creator projections gain no node access;
    every candidate reuses the same movement, gesture and publication engine. }
  TKanbanCoordinatorFactory = class(TInterfacedObject, INyxCoordinatorFactory,
    INyxCanonicalCoordinatorFactory)
  public
    function Prepare(const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
    function PrepareCanonical(const ACanonical: INyxControl;
      const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
  end;

function TNyxKanbanLaneRef.GetName: TNyxText;
begin

  if not FDefined then
  begin
    raise ENyxKanban.Create('Kanban lane is undefined');
  end;
  Result := FName;
end;

procedure CheckText(const AText: TNyxText; AMaximum: Integer);
var
  LIndex: Integer;
  LCount: Integer;
  LScalar: Integer;
begin
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(AText) do
  begin

    if (LCount >= AMaximum) or not NyxNextScalar(AText, LIndex, LScalar) or
      (LScalar = 0) then
    begin
      raise ENyxKanban.Create('Kanban text is malformed or exceeds its scalar limit');
    end;
    Inc(LCount);
  end;

  if LCount = 0 then
  begin
    raise ENyxKanban.Create('Kanban text cannot be empty');
  end;
end;

function NyxKanbanLane(const AName: TNyxText): TNyxKanbanLaneRef;
begin
  CheckText(AName, 128);
  Result.FName := AName;
  Result.FDefined := True;
end;

function NyxKanbanOptions(const ACollection: TNyxCollectionRef;
  const ATitleField, ALaneField: TNyxTextFieldRef): TNyxKanbanOptions;
begin
  { Typed reference getters validate default/uninitialized records. }

  if (ACollection.Name = '') or (ATitleField.Name = '') or
    (ALaneField.Name = '') or (ATitleField.Name = ALaneField.Name) then
  begin
    raise ENyxKanban.Create('Kanban needs distinct title and lane text fields');
  end;
  Result := Default(TNyxKanbanOptions);
  Result.FCollection := ACollection;
  Result.FTitle := ATitleField;
  Result.FField := ALaneField;
  Result.FScope := csInstance;
  Result.FDefined := True;
end;

function TNyxKanbanOptions.Copy: TNyxKanbanOptions;
var
  LIndex: Integer;
begin
  Result := Default(TNyxKanbanOptions);
  Result.FCollection := FCollection;
  Result.FTitle := FTitle;
  Result.FField := FField;
  Result.FScope := FScope;
  Result.FDefined := FDefined;
  SetLength(Result.FLanes, Length(FLanes));
  SetLength(Result.FCaptions, Length(FCaptions));
  for LIndex := 0 to High(FLanes) do
  begin
    Result.FLanes[LIndex] := FLanes[LIndex];
    Result.FCaptions[LIndex] := FCaptions[LIndex];
  end;
end;

function TNyxKanbanOptions.Lane(const ARef: TNyxKanbanLaneRef;
  const ACaption: TNyxText): TNyxKanbanOptions;
var
  LIndex: Integer;
begin

  if not FDefined or not ARef.Defined or (Length(FLanes) >= CKanbanLaneCount) then
  begin
    raise ENyxKanban.Create('Kanban needs three named lanes with bounded captions');
  end;
  CheckText(ACaption, 256);
  for LIndex := 0 to High(FLanes) do
  begin

    if FLanes[LIndex].Name = ARef.Name then
    begin
      raise ENyxKanban.Create('Kanban lane is duplicated');
    end;
  end;
  Result := Copy;
  SetLength(Result.FLanes, Length(FLanes) + 1);
  SetLength(Result.FCaptions, Length(FCaptions) + 1);
  Result.FLanes[High(Result.FLanes)] := ARef;
  Result.FCaptions[High(Result.FCaptions)] := ACaption;
end;

function TNyxKanbanOptions.Scoped(AScope: TNyxCollectionScope): TNyxKanbanOptions;
begin

  if not (AScope in [csApplication, csInstance]) then
  begin
    raise ENyxKanban.Create('Unknown Kanban collection scope');
  end;
  Result := Copy;
  Result.FScope := AScope;
end;

function TNyxKanbanOptions.GetCount: Integer;
begin
  Result := Length(FLanes);
end;

function TNyxKanbanOptions.LaneAt(AIndex: Integer): TNyxKanbanLaneRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FLanes)) then
  begin
    raise ENyxKanban.Create('Kanban lane index is outside the configured board');
  end;
  Result := FLanes[AIndex];
end;

function TNyxKanbanOptions.CaptionAt(AIndex: Integer): TNyxText;
begin
  LaneAt(AIndex);
  Result := FCaptions[AIndex];
end;

procedure TNyxKanbanOptions.Validate;
var
  LIndex: Integer;
  LOther: Integer;
begin

  if not FDefined or (Count <> CKanbanLaneCount) or
    (Length(FCaptions) <> Count) then
  begin
    raise ENyxKanban.Create('Existing Kanban board requires exactly three lanes');
  end;

  if (FCollection.Name = '') or (FTitle.Name = '') or (FField.Name = '') or
    (FTitle.Name = FField.Name) or not (FScope in [csApplication, csInstance]) then
  begin
    raise ENyxKanban.Create('Invalid Kanban field/scope options');
  end;
  for LIndex := 0 to Count - 1 do
  begin

    if not FLanes[LIndex].Defined or (FCaptions[LIndex] = '') then
    begin
      raise ENyxKanban.Create('Kanban lane/caption is undefined');
    end;
    for LOther := 0 to LIndex - 1 do
    begin

      if FLanes[LOther].Name = FLanes[LIndex].Name then
      begin
        raise ENyxKanban.Create('Kanban lane is duplicated');
      end;
    end;
  end;
end;

function LanePath(AIndex: Integer; const AChild: TNyxText): TNyxPartRef;
var
  LName: TNyxText;
begin
  LName := 'lane-' + IntToStr(AIndex);

  if AChild <> '' then
  begin
    LName := LName + '/' + AChild;
  end;
  Result := NyxPart(LName);
end;

function LaneSpec(const AOptions: TNyxKanbanOptions;
  AIndex: Integer): TNyxCollectionViewSpec;
var
  LQuery: TNyxCollectionQuery;
begin
  LQuery := NyxCollectionQuery.Where(NyxWhere(AOptions.LaneField).EqualTo(
    AOptions.LaneAt(AIndex).Name, nqtExact));
  Result := NyxCollectionView(AOptions.Collection).Column(AOptions.TitleField,
    'Card').Scoped(AOptions.Scope).Selection(nsmSingle).Query(LQuery);
end;

procedure CheckLaneSpec(const ASpec: TNyxCollectionViewSpec;
  const AOptions: TNyxKanbanOptions; const AQuery: TNyxText);
var
  LColumn: TNyxCollectionColumn;
begin

  if not ASpec.Defined or (ASpec.Key.Name <> AOptions.Collection.Name) or
    (ASpec.Scope <> AOptions.Scope) or (ASpec.SelectionMode <> nsmSingle) or
    (ASpec.ParentField <> '') or (ASpec.Count <> 1) or
    (ASpec.QueryPolicy.ToData.ToJSON <> AQuery) then
  begin
    raise ENyxKanban.Create('Automatic Kanban list binding disagrees with its typed intent');
  end;
  LColumn := ASpec.ColumnAt(0);

  if (LColumn.FieldName <> AOptions.TitleField.Name) or
    (LColumn.Kind <> nskText) or (LColumn.Mode <> cmReadOnly) then
  begin
    raise ENyxKanban.Create('Automatic Kanban requires its typed card-title field');
  end;
end;

procedure ValidateNyxKanbanBoardOptions(ANode: TNyxNode;
  const AOptions: TNyxKanbanOptions);
var
  LOptions: TNyxKanbanOptions;
  LItems: TNyxNode;
  LMove: TNyxNode;
  LIndex: Integer;
begin
  LOptions := AOptions.Copy;
  LOptions.Validate;

  if (ANode = nil) or (ANode.Kind <> NyxKindName(nkKanbanBoard)) then
  begin
    raise ENyxKanban.Create('Automatic Kanban intent requires its specialized board');
  end;
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin
    LItems := ANode.Part(LanePath(LIndex, 'items'));
    LMove := ANode.Part(LanePath(LIndex, CMovePart));

    if (LItems.ProjectionKind <> NyxKindName(nkList)) or
      (LMove.ProjectionKind <> NyxKindName(nkButton)) or
      not LItems.HasCollectionView then
    begin
      raise ENyxKanban.Create('Automatic Kanban requires bound lists and Move buttons');
    end;
    CheckLaneSpec(LItems.CollectionView, LOptions,
      LaneSpec(LOptions, LIndex).QueryPolicy.ToData.ToJSON);
  end;
end;

procedure ConfigureNyxKanbanBoard(const ABoard: INyxKanbanBoard;
  const AOptions: TNyxKanbanOptions);
var
  LBoard: INyxKanbanBoard;
  LOptions: TNyxKanbanOptions;
  LColumns: array of INyxControl;
  LItems: array of INyxControl;
  LTitles: array of INyxHeading;
  LButtons: array of INyxButton;
  LControl: INyxControl;
  LIndex: Integer;
  LChild: Integer;
begin
  LBoard := ABoard;
  LOptions := AOptions.Copy;
  LOptions.Validate;

  if (LBoard = nil) or LBoard.Node.IsRealized then
  begin
    raise ENyxKanban.Create('Configure an authored Kanban board before mounting');
  end;
  SetLength(LColumns, CKanbanLaneCount);
  SetLength(LItems, CKanbanLaneCount);
  SetLength(LTitles, CKanbanLaneCount);
  SetLength(LButtons, CKanbanLaneCount);
  { Check every required part and name collision before editing authored values. }
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin
    LColumns[LIndex] := LBoard.Part(LanePath(LIndex, ''));
    LItems[LIndex] := LBoard.Part(LanePath(LIndex, 'items'));
    LControl := LBoard.Part(LanePath(LIndex, 'title'));

    if not Supports(LColumns[LIndex], INyxColumn) or
      not Supports(LItems[LIndex], INyxList) or
      not Supports(LControl, INyxHeading, LTitles[LIndex]) then
    begin
      raise ENyxKanban.Create('Kanban requires its three column/list/title parts');
    end;
    for LChild := 0 to LColumns[LIndex].Count - 1 do
    begin

      if LColumns[LIndex].Children[LChild].Node.Prop(
        NyxAttributeName(atPart), '') = CMovePart then
      begin
        raise ENyxKanban.Create('Kanban move part already exists');
      end;
    end;
    LButtons[LIndex] := NewNyxButton(LColumns[LIndex].ID + '-move').WithText(CMoveText);
    LButtons[LIndex].Configure.PartName(NyxPart(CMovePart)).AccessibleName(
      'Move selected card to ' + LOptions.CaptionAt(LIndex)).Done;
  end;
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin
    LTitles[LIndex].Text := LOptions.CaptionAt(LIndex);
    LItems[LIndex].Binds.Collection(LaneSpec(LOptions, LIndex));
    LItems[LIndex].Configure.DragSource(True).DropTarget(True).Done;
    LColumns[LIndex].Add(LButtons[LIndex]);
  end;
end;

constructor TKanbanPort.Create(AOwner: TKanbanBinding);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TKanbanPort.Acquire: INyxKanbanBinding;
begin
  Result := FOwner;
end;

function TKanbanPort.Owner: TKanbanBinding;
begin
  Result := FOwner;
end;

procedure TKanbanPort.Revoke;
begin
  FOwner := nil;
end;

procedure TKanbanPort.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LOwner: INyxKanbanBinding;
  LExecution: INyxExecution;
  LEvent: TNyxEventInfo;
begin
  LOwner := Acquire;
  LExecution := AExecution;
  LEvent := AEvent.Copy;

  if (LOwner <> nil) and LOwner.Connected then
  begin
    Owner.Handle(LEvent, LExecution);
  end;
end;

procedure TKanbanPort.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
var
  LOwner: INyxKanbanBinding;
  LView: INyxCollectionView;
  LChanges: INyxCollectionChanges;
  LBinding: TKanbanBinding;
begin
  LOwner := Acquire;
  LBinding := Owner;
  LView := AView;
  LChanges := AChanges;

  if (LOwner <> nil) and (LBinding <> nil) and LBinding.FPrepared and
    not LBinding.FConnected then
  begin
    LBinding.FDirty := True;
    Exit;
  end;

  if (LOwner <> nil) and LOwner.Connected then
  begin
    Owner.Changed(LView, LChanges);
  end;
end;

constructor TKanbanObservation.Create(const APort: IKanbanPort);
begin
  inherited Create;
  FPort := APort;
end;

procedure TKanbanObservation.Observe(const AEvent: TNyxEventInfo;
  ACallbackCount: Integer);
var
  LOwner: INyxKanbanBinding;
  LEvent: TNyxEventInfo;
begin
  { Counts describe ordinary callbacks; a malformed alternate observation
    producer cannot silently supply a negative dispatch envelope. }

  if ACallbackCount < 0 then
  begin
    raise ENyxKanban.Create('Invalid Kanban observation callback count');
  end;
  LOwner := FPort.Acquire;
  LEvent := AEvent.Copy;

  if (LOwner <> nil) and LOwner.Connected then
  begin
    FPort.Owner.Observe(LEvent);
  end;
end;

constructor TKanbanEnabled.Create(const APort: IKanbanPort; ADestination: Integer);
begin
  inherited Create;
  FPort := APort;
  FDestination := ADestination;
end;

function TKanbanEnabled.GetAllowed: Boolean;
var
  LOwner: INyxKanbanBinding;
  LBinding: TKanbanBinding;
  LCancelled: Boolean;
begin
  LOwner := FPort.Acquire;
  LBinding := FPort.Owner;

  if (LOwner <> nil) and (LBinding <> nil) and LBinding.FPrepared then
  begin
    Result := False;

    if LBinding.FRevoked or (LBinding.FGeneration = nil) then
    begin
      Exit;
    end;
    LCancelled := LBinding.FGeneration.Cancelled;
    Result := not LCancelled and not LBinding.FRevoked and
      LBinding.FAllowed[FDestination];
    Exit;
  end;
  Result := (LOwner <> nil) and LOwner.Connected;

  if Result then
  begin
    Result := FPort.Owner.CanMove(FDestination);
  end;
end;

function TKanbanEnabled.GetConnected: Boolean;
var
  LOwner: INyxKanbanBinding;
begin
  LOwner := FPort.Acquire;

  if (LOwner <> nil) and FPort.Owner.FPrepared then
  begin
    { Dormant automatic attachment reserves its faces against another owner. }
    Result := not FPort.Owner.FRevoked;
    Exit;
  end;
  Result := (LOwner <> nil) and LOwner.Connected;
end;

constructor TKanbanMove.Create(const APort: IKanbanPort;
  const AItem: TNyxItemRef; AFrom, ATo, ARevision: Integer;
  const AExecution: INyxExecution);
begin
  inherited Create;
  FPort := APort;
  FItem := AItem;
  FFrom := AFrom;
  FTo := ATo;
  FRevision := ARevision;
  FExecution := AExecution;
end;

procedure TKanbanMove.Commit;
var
  LOwner: INyxKanbanBinding;
  LKeepAlive: INyxKanbanMove;
  LBinding: TKanbanBinding;
begin
  LKeepAlive := Self;

  if FAttempted or FCancelled then
  begin
    raise ENyxKanbanCancelled.Create('Kanban move is cancelled or already attempted');
  end;
  FAttempted := True;
  LOwner := FPort.Acquire;
  LBinding := FPort.Owner;

  if (LOwner = nil) or (LBinding = nil) then
  begin
    raise ENyxKanbanCancelled.Create('Kanban move attachment has retired');
  end;
  try
    LBinding.CommitMove(Self);
  finally
    FExecution := nil;
  end;
end;

procedure TKanbanMove.Cancel;
begin
  FCancelled := True;
end;

function TKanbanMove.GetCommitted: Boolean;
begin
  Result := FCommitted;
end;

constructor TKanbanBinding.Create(const ABoard: INyxKanbanBoard;
  const AOptions: TNyxKanbanOptions; const AViews: INyxCollectionBindings;
  const AEvents: INyxEvents; const AScheduler: INyxScheduler;
  ARefresh: TNyxKanbanRefresh);
var
  LViews: INyxCollectionBindings;
  LEvents: INyxEvents;
  LIndex: Integer;
  LPolicyIndex: Integer;
  LControl: INyxControl;
  LPolicy: IKanbanEnabled;
  LObservation: INyxEventObservation;
begin
  inherited Create;
  FBoard := ABoard;
  FOptions := AOptions.Copy;
  LViews := AViews;
  LEvents := AEvents;
  FScheduler := AScheduler;
  FRefresh := ARefresh;
  FActive := -1;
  FDragLane := -1;
  FOptions.Validate;

  if (FBoard = nil) or (LViews = nil) or (LEvents = nil) or
    (FScheduler = nil) then
  begin
    raise ENyxKanban.Create('Kanban requires board, bindings, events and scheduler');
  end;
  FScheduler.RequireUI;

  if not FBoard.Node.IsRealized or (LEvents.Scheduler <> FScheduler) then
  begin
    raise ENyxKanban.Create('Kanban requires a realized board and its UI scheduler');
  end;
  FGeneration := NyxEventGeneration(LEvents);

  if (FGeneration = nil) or FGeneration.Cancelled then
  begin
    raise ENyxKanbanCancelled.Create('Kanban requires a live mounted event generation');
  end;

  if not Supports(LEvents, INyxEventObservation, LObservation) then
  begin
    raise ENyxKanban.Create('Kanban requires synchronous event observation');
  end;
  SetLength(FViews, CKanbanLaneCount);
  SetLength(FItems, CKanbanLaneCount);
  SetLength(FMoves, CKanbanLaneCount);
  SetLength(FPolicies, CKanbanLaneCount);
  SetLength(FViewTokens, CKanbanLaneCount);
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin
    LControl := FBoard.Part(LanePath(LIndex, 'items'));

    if not Supports(LControl, INyxList, FItems[LIndex]) then
    begin
      raise ENyxKanban.Create('Kanban items part is not a list');
    end;
    LControl := FBoard.Part(LanePath(LIndex, CMovePart));

    if not Supports(LControl, INyxButton, FMoves[LIndex]) then
    begin
      raise ENyxKanban.Create('Kanban move part is not a button');
    end;
    for LPolicyIndex := 0 to FMoves[LIndex].Node.EnabledPolicyCount - 1 do
    begin

      if Supports(FMoves[LIndex].Node.EnabledPolicyAt(LPolicyIndex),
        IKanbanEnabled, LPolicy) and LPolicy.GetConnected then
      begin
        raise ENyxKanban.Create('Kanban board already has a live move attachment');
      end;
    end;
    FViews[LIndex] := LViews.ViewFor(FItems[LIndex].ID);

    if FViews[LIndex] = nil then
    begin
      raise ENyxKanban.Create('Kanban items part has no mounted collection view');
    end;

    if LIndex = 0 then
    begin
      FStore := FViews[LIndex].Store;
    end
    else
    begin

      if FViews[LIndex].Store <> FStore then
      begin
        raise ENyxKanban.Create('Kanban lanes must share one exact runtime store');
      end;
    end;
  end;

  if (FStore = nil) or not Supports(FStore, INyxAtomicCollection, FAtomic) then
  begin
    raise ENyxKanban.Create('Kanban store must support prepared atomic publication');
  end;
  FStore.Snapshot.Schema.Definition(FOptions.TitleField);
  FStore.Snapshot.Schema.Definition(FOptions.LaneField);
  FConnected := True;
  RequireContract;
  FConnected := False;
end;

procedure TKanbanBinding.RequireLive;
var
  LKeepAlive: INyxKanbanBinding;
  LGeneration: INyxCancellationScope;
  LScheduler: INyxScheduler;
begin
  LKeepAlive := Self;
  LGeneration := FGeneration;
  LScheduler := FScheduler;

  if not FConnected or (LGeneration = nil) or (LScheduler = nil) then
  begin
    raise ENyxKanbanCancelled.Create('Kanban attachment has disconnected or been replaced');
  end;
  LScheduler.RequireUI;

  if LGeneration.Cancelled or not FConnected or
    (FGeneration <> LGeneration) then
  begin
    raise ENyxKanbanCancelled.Create('Kanban attachment has disconnected or been replaced');
  end;
end;

procedure TKanbanBinding.RequireContract;
var
  LIndex: Integer;
  LExpected: TNyxCollectionViewSpec;
  LNode: TNyxNode;
begin

  if FPrepared then
  begin
    RequireLive;
    AdmitPreparedContract;
    Exit;
  end;
  RequireLive;
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin
    LExpected := LaneSpec(FOptions, LIndex);
    LNode := FBoard.Part(LanePath(LIndex, 'items')).Node;

    if (LNode <> FItems[LIndex].Node) or not LNode.IsRealized or
      (FBoard.Part(LanePath(LIndex, CMovePart)).Node <> FMoves[LIndex].Node) or
      (FViews[LIndex].Store <> FStore) or
      (FViews[LIndex].Projection <> cpList) or
      (FViews[LIndex].Spec.ToData.ToJSON <> LExpected.ToData.ToJSON) or
      (FViews[LIndex].QueryPolicy.ToData.ToJSON <>
        LExpected.QueryPolicy.ToData.ToJSON) then
    begin
      raise ENyxKanban.Create('Kanban mounted parts or lane projections have changed');
    end;
    RequireLive;
  end;
end;

procedure TKanbanBinding.Subscribe(const AEvents: INyxEvents;
  ATrigger: TNyxTrigger; const AID: TNyxText);
var
  LToken: INyxEventSubscription;
begin
  LToken := AEvents.On(NyxControlEvents(AID, niRuntime), ATrigger).Subscribe(FCallback);
  SetLength(FSubscriptions, Length(FSubscriptions) + 1);
  FSubscriptions[High(FSubscriptions)] := LToken;
end;

procedure TKanbanBinding.Activate(const AEvents: INyxEvents);
var
  LIndex: Integer;
  LPolicyIndex: Integer;
  LPolicy: INyxRuntimeEnabledPolicy;
  LObservation: INyxEventObservation;
  LObserver: INyxEventObserver;
  LToken: INyxEventSubscription;
begin
  FConnected := True;
  FCallback := TKanbanPort.Create(Self);
  FPort := FCallback;
  try

    if not Supports(AEvents, INyxEventObservation, LObservation) then
    begin
      raise ENyxKanban.Create('Kanban requires synchronous event observation');
    end;
    LObserver := TKanbanObservation.Create(FPort);
    for LIndex := 0 to CKanbanLaneCount - 1 do
    begin
      for LPolicyIndex := FMoves[LIndex].Node.EnabledPolicyCount - 1 downto 0 do
      begin
        LPolicy := FMoves[LIndex].Node.EnabledPolicyAt(LPolicyIndex);

        if Supports(LPolicy, IKanbanEnabled) then
        begin
          FMoves[LIndex].Node.DetachEnabledPolicy(LPolicy);
        end;
      end;
      FPolicies[LIndex] := TKanbanEnabled.Create(FPort, LIndex);
      FMoves[LIndex].Node.AttachEnabledPolicy(FPolicies[LIndex]);
      FViewTokens[LIndex] := FViews[LIndex].Subscribe(FCallback.Changed);
      LToken := LObservation.Observe(NyxControlEvents(FMoves[LIndex].ID, niRuntime),
        ntClick, LObserver, nopBeforeCallbacks, NyxEvent(''));
      SetLength(FSubscriptions, Length(FSubscriptions) + 1);
      FSubscriptions[High(FSubscriptions)] := LToken;
      LToken := LObservation.Observe(NyxControlEvents(FItems[LIndex].ID, niRuntime),
        ntDragEnd, LObserver, nopBeforeCallbacks, NyxEvent(''));
      SetLength(FSubscriptions, Length(FSubscriptions) + 1);
      FSubscriptions[High(FSubscriptions)] := LToken;
      Subscribe(AEvents, ntDragStart, FItems[LIndex].ID);
      Subscribe(AEvents, ntDragOver, FItems[LIndex].ID);
      Subscribe(AEvents, ntDragEnter, FItems[LIndex].ID);
      Subscribe(AEvents, ntDrop, FItems[LIndex].ID);
    end;
    RequireContract;
    { The physical controls were mounted before attaching these constraints. }
    Refresh;
  except
    Disconnect;
    raise;
  end;
end;

destructor TKanbanBinding.Destroy;
begin

  if FPrepared then
  begin
    try
      DisconnectPrepared;
    except
      { Destruction follows irrevocable weak-port revocation. }
    end;
  end
  else
  begin
    Retire;
  end;
  inherited Destroy;
end;

procedure TKanbanBinding.Disconnect;
var
  LKeepAlive: INyxKanbanBinding;
begin
  LKeepAlive := Self;
  Retire;
end;

procedure TKanbanBinding.Retire;
var
  LIndex: Integer;
begin

  if FPrepared then
  begin
    DisconnectPrepared;
    Exit;
  end;
  FConnected := False;
  FRefresh := nil;

  if FPort <> nil then
  begin
    FPort.Revoke;
  end;
  for LIndex := 0 to High(FSubscriptions) do
  begin
    FSubscriptions[LIndex].Cancel;
  end;
  FSubscriptions := nil;
  for LIndex := 0 to High(FViewTokens) do
  begin

    if FViewTokens[LIndex] <> nil then
    begin
      FViewTokens[LIndex].Disconnect;
    end;
  end;
  FViewTokens := nil;
  for LIndex := 0 to High(FPolicies) do
  begin

    if (FPolicies[LIndex] <> nil) and (FMoves[LIndex] <> nil) then
    begin
      FMoves[LIndex].Node.DetachEnabledPolicy(FPolicies[LIndex]);
    end;
  end;
  FPolicies := nil;
  ClearDrag;
  FItems := nil;
  FMoves := nil;
  FViews := nil;
  FAtomic := nil;
  FStore := nil;
  FBoard := nil;
  FGeneration := nil;
  FScheduler := nil;
  FCallback := nil;
  FPort := nil;
end;

function TKanbanBinding.GetConnected: Boolean;
var
  LGeneration: INyxCancellationScope;
  LKeepAlive: INyxKanbanBinding;
begin
  LKeepAlive := Self;
  LGeneration := FGeneration;
  Result := FConnected and (LGeneration <> nil);

  if Result then
  begin
    Result := not LGeneration.Cancelled and FConnected;
  end;
end;

procedure TKanbanBinding.Refresh;
var
  LKeepAlive: INyxKanbanBinding;
  LRefresh: TNyxKanbanRefresh;
  LScopedRefresh: INyxCoordinatorRefresh;
begin
  LKeepAlive := Self;
  RequireLive;
  LRefresh := FRefresh;
  LScopedRefresh := FScopedRefresh;

  if LScopedRefresh <> nil then
  begin
    { The guarded port belongs to the accepted scope, not a borrowed renderer.
      It may dispose the complete view; no attachment access follows. }
    LScopedRefresh.Request;
    Exit;
  end;

  if Assigned(LRefresh) then
  begin
    { Sole last borrowed call. Do not inspect attachment fields afterward. }
    LRefresh;
  end;
end;

function TKanbanBinding.IndexOf(const ALane: TNyxKanbanLaneRef): Integer;
var
  LIndex: Integer;
begin
  Result := -1;

  if ALane.Defined then
  begin
    for LIndex := 0 to FOptions.Count - 1 do
    begin

      if FOptions.LaneAt(LIndex).Name = ALane.Name then
      begin
        Exit(LIndex);
      end;
    end;
  end;
  raise ENyxKanban.Create('Lane does not belong to this Kanban board');
end;

function TKanbanBinding.Items(const ALane: TNyxKanbanLaneRef): INyxList;
var
  LKeepAlive: INyxKanbanBinding;
begin
  LKeepAlive := Self;
  RequireLive;
  Result := FItems[IndexOf(ALane)];
end;

function TKanbanBinding.MoveButton(const ALane: TNyxKanbanLaneRef): INyxButton;
var
  LKeepAlive: INyxKanbanBinding;
begin
  LKeepAlive := Self;
  RequireLive;
  Result := FMoves[IndexOf(ALane)];
end;

procedure TKanbanBinding.CheckMove(const AItem: TNyxItemRef;
  AFrom, ATo, ARevision: Integer);
var
  LSnapshot: INyxCollectionSnapshot;
  LItem: TNyxCollectionItem;
begin
  RequireContract;

  if not AItem.Defined or (AFrom < 0) or (AFrom >= CKanbanLaneCount) or
    (ATo < 0) or (ATo >= CKanbanLaneCount) or (AFrom = ATo) or
    (ARevision < 0) then
  begin
    raise ENyxKanban.Create('Kanban move requires an item, distinct lanes and revision');
  end;
  LSnapshot := FStore.Snapshot;

  if (LSnapshot.Revision <> ARevision) or
    (AItem.Collection.Name <> FOptions.Collection.Name) or
    not FViews[AFrom].Snapshot.Has(AItem) then
  begin
    raise ENyxKanban.Create('Kanban move source is foreign, stale or no longer visible');
  end;
  LItem := LSnapshot.Item(AItem);

  if LItem.GetValue(FOptions.LaneField) <> FOptions.LaneAt(AFrom).Name then
  begin
    raise ENyxKanban.Create('Kanban card no longer belongs to the captured source lane');
  end;

  if (FItems[AFrom].Node.Prop(NyxAttributeName(atReadOnly), 'false') = 'true') or
    (FItems[ATo].Node.Prop(NyxAttributeName(atReadOnly), 'false') = 'true') then
  begin
    raise ENyxKanban.Create('Read-only Kanban lanes cannot move cards');
  end;
  RequireLive;
end;

function TKanbanBinding.NewMove(const AItem: TNyxItemRef;
  AFrom, ATo, ARevision: Integer;
  const AExecution: INyxExecution): INyxKanbanMove;
var
  LKeepAlive: INyxKanbanBinding;
  LItem: TNyxItemRef;
  LExecution: INyxExecution;
begin
  LKeepAlive := Self;
  LItem := AItem;
  LExecution := AExecution;
  CheckMove(LItem, AFrom, ATo, ARevision);
  Result := TKanbanMove.Create(FPort, LItem, AFrom, ATo, ARevision, LExecution);
end;

function TKanbanBinding.CaptureMove(const AItem: TNyxItemRef;
  const AFromLane, AToLane: TNyxKanbanLaneRef;
  AExpectedRevision: Integer): INyxKanbanMove;
begin
  Result := NewMove(AItem, IndexOf(AFromLane), IndexOf(AToLane),
    AExpectedRevision, nil);
end;

procedure TKanbanBinding.CommitMove(AMove: TKanbanMove);
var
  LKeepAlive: INyxKanbanBinding;
  LStore: INyxCollection;
  LAtomic: INyxAtomicCollection;
  LCandidate: INyxCollection;
  LPrepared: INyxPreparedPublication;
  LItem: TNyxCollectionItem;
  procedure CheckCurrent;
  begin

    if AMove.FCancelled or ((AMove.FExecution <> nil) and
      AMove.FExecution.Cancelled) or (AMove.FPort <> FPort) then
    begin
      raise ENyxKanbanCancelled.Create('Kanban move cancelled before installation');
    end;
    CheckMove(AMove.FItem, AMove.FFrom, AMove.FTo, AMove.FRevision);
  end;
begin
  LKeepAlive := Self;
  CheckCurrent;
  LStore := FStore;
  LAtomic := FAtomic;
  LItem := LStore.Snapshot.Item(AMove.FItem).WithValue(FOptions.LaneField,
    FOptions.LaneAt(AMove.FTo).Name);
  LCandidate := LStore.Clone;
  CheckCurrent;
  LCandidate.Apply([NyxUpdate(LItem)], AMove.FRevision);
  CheckCurrent;
  { Assignment-style receipts preserve the complete final stable item dataset.
    Existing prepared projections install before any observer is notified. }
  LPrepared := LAtomic.PrepareAssign(LCandidate.Snapshot, AMove.FRevision);
  try
    LPrepared.Validate;
    CheckCurrent;
    LPrepared.Install;
    AMove.FCommitted := True;
    LPrepared.Notify;
  finally
    LPrepared.Retire;
  end;
end;

function TKanbanBinding.SelectedItem(AFrom: Integer;
  out AItem: TNyxItemRef): Boolean;
var
  LSelection: INyxCollectionSelection;
begin
  AItem := Default(TNyxItemRef);
  Result := False;

  if not GetConnected or (AFrom < 0) or (AFrom >= Length(FViews)) then
  begin
    Exit;
  end;
  LSelection := FViews[AFrom].Selection;

  if LSelection.Count <> 1 then
  begin
    Exit;
  end;
  AItem := LSelection.ItemAt(0);
  Result := FViews[AFrom].Snapshot.Has(AItem);
end;

function TKanbanBinding.CanMove(ATo: Integer): Boolean;
var
  LItem: TNyxItemRef;
begin
  Result := (FActive <> ATo) and SelectedItem(FActive, LItem);

  if Result then
  begin
    Result := (FItems[FActive].Node.Prop(NyxAttributeName(atReadOnly), 'false') <>
      'true') and (FItems[ATo].Node.Prop(NyxAttributeName(atReadOnly), 'false') <>
      'true');
  end;
end;

procedure TKanbanBinding.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
var
  LIndex: Integer;
  LItem: TNyxItemRef;
begin

  if not GetConnected then
  begin
    Exit;
  end;

  if AChanges = nil then
  begin
    for LIndex := 0 to High(FViews) do
    begin

      if (FViews[LIndex] = AView) and SelectedItem(LIndex, LItem) then
      begin
        FActive := LIndex;
      end;
    end;
  end;

  if not SelectedItem(FActive, LItem) then
  begin
    FActive := -1;
  end;

  if FPrepared then
  begin
    CacheAvailability;
  end;
  { No selection mutation from a view notification; hidden members stay owned
    by the view but cannot become move input after leaving their source lane. }
  Refresh;
end;

procedure TKanbanBinding.ClearDrag;
begin
  FDragItem := Default(TNyxItemRef);
  FDragLane := -1;
  FDragRevision := -1;
end;

procedure TKanbanBinding.Observe(const AEvent: TNyxEventInfo);
var
  LIndex: Integer;
  LItem: TNyxItemRef;
  LMove: INyxKanbanMove;
begin
  RequireLive;
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin

    if FPrepared and (AEvent.Trigger in [ntClick, ntAfterEnter]) and
      (AEvent.OriginID = FItemNodes[LIndex].ID) then
    begin
      FActive := -1;

      if SelectedItem(LIndex, LItem) then
      begin
        FActive := LIndex;
      end;
      CacheAvailability;
      Refresh;
      Exit;
    end;

    if (AEvent.Trigger = ntDragEnd) and AEvent.HasDrag and
      (AEvent.Drag.Phase = ndpEnd) and
      (AEvent.OriginID = FItems[LIndex].ID) and (FDragLane = LIndex) and
      (AEvent.Drag.SourceID = FItems[LIndex].ID) then
    begin
      ClearDrag;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and (AEvent.OriginID = FMoves[LIndex].ID) then
    begin

      if (FMoves[LIndex].Node.Prop(NyxAttributeName(atEnabled), 'true') = 'true') and
        CanMove(LIndex) and SelectedItem(FActive, LItem) then
      begin
        LMove := NewMove(LItem, FActive, LIndex, FStore.Snapshot.Revision, nil);
        LMove.Commit;
      end;
      Exit;
    end;
  end;
end;

procedure TKanbanBinding.Handle(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LIndex: Integer;
  LItem: TNyxItemRef;
  LMove: INyxKanbanMove;
  LResponse: INyxGestureResponse;
  LRevision: Integer;
  LSource: TNyxText;
begin
  RequireLive;

  if (AExecution = nil) or AExecution.Cancelled then
  begin
    Exit;
  end;
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin

    if AEvent.OriginID <> FItems[LIndex].ID then
    begin
      Continue;
    end;

    if not AEvent.HasDrag then
    begin
      Exit;
    end;
    LResponse := NyxGestureResponse(AExecution);

    if AEvent.Trigger = ntDragStart then
    begin

      if not LResponse.CanRequest(ngcOfferDrag) or
        not SelectedItem(LIndex, LItem) or
        (FItems[LIndex].Node.Prop(NyxAttributeName(atReadOnly), 'false') = 'true') then
      begin
        Exit;
      end;
      ClearDrag;
      FDragItem := LItem;
      FDragLane := LIndex;
      FDragRevision := FStore.Snapshot.Revision;
      LResponse.OfferDrag(NyxTransferCustom(NyxTransferFormat(CTransferFormat),
        FDragItem.ID), [ndoMove]);
      Exit;
    end;

    if not FDragItem.Defined or (FDragLane = LIndex) or
      not LResponse.CanRequest(ngcAcceptDrop) or
      not (ndoMove in AEvent.Drag.Allowed) or
      not AEvent.Drag.Transfer.HasFormat(NyxTransferFormat(CTransferFormat)) then
    begin
      Exit;
    end;
    LSource := FItems[FDragLane].ID;

    if AEvent.Drag.SourceID <> LSource then
    begin
      Exit;
    end;
    CheckMove(FDragItem, FDragLane, LIndex, FDragRevision);

    if AEvent.Trigger = ntDrop then
    begin

      if not AEvent.Drag.Transfer.Readable or
        (AEvent.Drag.Transfer.TextFor(NyxTransferFormat(CTransferFormat)) <>
          FDragItem.ID) then
      begin
        Exit;
      end;
      LItem := FDragItem;
      LRevision := FDragRevision;
      LMove := NewMove(LItem, FDragLane, LIndex, LRevision, AExecution);
      ClearDrag;
      LMove.Commit;
      { A notification may replace the view. A revoked execution cannot respond
        to the old platform gesture even though its data is already committed. }

      if GetConnected and not AExecution.Cancelled and
        LResponse.CanRequest(ngcAcceptDrop) then
      begin
        LResponse.AcceptDrop(ndoMove);
      end;
      Exit;
    end;
    LResponse.AcceptDrop(ndoMove);
    Exit;
  end;
end;

function OptionalMoveNode(ABoard: TNyxNode; AIndex: Integer): TNyxNode;
var
  LLane: TNyxNode;
  LIndex: Integer;
  LName: TNyxText;
begin
  Result := nil;
  LLane := nil;
  LName := 'lane-' + IntToStr(AIndex);
  for LIndex := 0 to ABoard.Count - 1 do
  begin

    if ABoard.Children[LIndex].StoredProp(NyxAttributeName(atPart)) = LName then
    begin

      if LLane <> nil then
      begin
        raise ENyxKanban.Create('Automatic Kanban lane part is ambiguous');
      end;
      LLane := ABoard.Children[LIndex];
    end;
  end;

  if LLane = nil then
  begin
    Exit;
  end;
  for LIndex := 0 to LLane.Count - 1 do
  begin

    if LLane.Children[LIndex].StoredProp(NyxAttributeName(atPart)) = CMovePart then
    begin

      if Result <> nil then
      begin
        raise ENyxKanban.Create('Automatic Kanban Move part is ambiguous');
      end;
      Result := LLane.Children[LIndex];
    end;
  end;

  if (Result <> nil) and (Result.ProjectionKind <> NyxKindName(nkButton)) then
  begin
    Result := nil;
  end;
end;

constructor TKanbanBinding.CreatePrepared(const ABoard: INyxKanbanBoard;
  const AOptions: TNyxKanbanOptions; const AViews: INyxCollectionBindings;
  const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  ARefresh: TNyxKanbanRefresh; ADisabled: Boolean;
  const AScopedRefresh: INyxCoordinatorRefresh;
  const AProjectionLifetime: INyxCoordinatorProjectionLifetime);
var
  LViews: INyxCollectionBindings;
  LEventView: INyxPreparedEventView;
  LControl: INyxControl;
  LPolicy: IKanbanEnabled;
  LObserver: INyxEventObserver;
  LIndex: Integer;
  LPolicyIndex: Integer;
  LNext: Integer;

  procedure RememberEvent(const AToken: INyxEventSubscription);
  begin
    LNext := Length(FSubscriptions);
    SetLength(FSubscriptions, LNext + 1);
    FSubscriptions[LNext] := AToken;
    CheckPrepared;
  end;

begin
  inherited Create;
  FPrepared := True;
  FDisabled := ADisabled;
  FBoard := ABoard;
  FOptions := AOptions.Copy;
  LViews := AViews;
  LEventView := AEventView;
  FScheduler := AScheduler;
  FRefresh := ARefresh;
  FScopedRefresh := AScopedRefresh;
  FProjectionLifetime := AProjectionLifetime;
  FActive := -1;
  FDragLane := -1;

  if (FBoard = nil) or (LEventView = nil) or (FScheduler = nil) then
  begin
    raise ENyxKanban.Create('Prepared Kanban requires board, event plan and UI scheduler');
  end;
  FBoardNode := FBoard.Node;
  FGeneration := LEventView.Generation;
  FScheduler.RequireUI;
  CheckPrepared;

  if (FBoardNode = nil) or not FBoardNode.IsRealized or
    (FBoardNode.Kind <> NyxKindName(nkKanbanBoard)) then
  begin
    raise ENyxKanban.Create('Prepared Kanban requires its realized specialized board');
  end;
  FBoardProps := FBoardNode.StoredProp('projection-kind');
  FBoardParent := FBoardNode.Parent;
  FBoardExtensions := FBoardNode.Extensions.ToJSON;
  SetLength(FMoves, CKanbanLaneCount);
  SetLength(FPolicies, CKanbanLaneCount);
  SetLength(FReadiness, CKanbanLaneCount);
  FCallback := TKanbanPort.Create(Self);
  FPort := FCallback;

  if not FDisabled then
  begin
    FOptions.Validate;

    if LViews = nil then
    begin
      raise ENyxKanban.Create('Prepared Kanban requires its mounted collection bindings');
    end;
    ValidateNyxKanbanBoardOptions(FBoardNode, FOptions);
    SetLength(FItems, CKanbanLaneCount);
    SetLength(FViews, CKanbanLaneCount);
    SetLength(FViewTokens, CKanbanLaneCount);
  end;
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin
    FMoveNodes[LIndex] := OptionalMoveNode(FBoardNode, LIndex);

    if FMoveNodes[LIndex] <> nil then
    begin
      LControl := RetainNyxControl(FMoveNodes[LIndex]);

      if not Supports(LControl, INyxButton, FMoves[LIndex]) then
      begin
        raise ENyxKanban.Create('Prepared Kanban Move face has no button capability');
      end;
      for LPolicyIndex := 0 to FMoveNodes[LIndex].EnabledPolicyCount - 1 do
      begin

        if Supports(FMoveNodes[LIndex].EnabledPolicyAt(LPolicyIndex),
          IKanbanEnabled, LPolicy) and LPolicy.GetConnected then
        begin
          raise ENyxKanban.Create('Kanban board already reserves its Move faces');
        end;
        CheckPrepared;
      end;
      FMoveProps[LIndex] := FMoveNodes[LIndex].StoredProp(
        NyxAttributeName(atEnabled), 'true');
    end;

    if not FDisabled then
    begin
      FItemNodes[LIndex] := FBoardNode.Part(LanePath(LIndex, 'items'));
      LControl := RetainNyxControl(FItemNodes[LIndex]);

      if not Supports(LControl, INyxList, FItems[LIndex]) then
      begin
        raise ENyxKanban.Create('Prepared Kanban items role has no list capability');
      end;
      FItemProps[LIndex] := FItemNodes[LIndex].StoredProp(
        NyxAttributeName(atReadOnly), 'false');
      FQueries[LIndex] := LaneSpec(FOptions, LIndex).QueryPolicy.ToData.ToJSON;
      FViews[LIndex] := LViews.ViewFor(FItemNodes[LIndex].ID);
      CheckPrepared;

      if (FViews[LIndex] = nil) or not Supports(FViews[LIndex],
        INyxCollectionViewPublication, FReadiness[LIndex]) then
      begin
        raise ENyxKanban.Create('Prepared Kanban views require publication readiness');
      end;
      CheckPrepared;

      if LIndex = 0 then
      begin
        FStore := FViews[LIndex].Store;
        CheckPrepared;
      end;
    end;
  end;

  if not FDisabled then
  begin

    if (FStore = nil) or not Supports(FStore, INyxAtomicCollection, FAtomic) then
    begin
      raise ENyxKanban.Create('Prepared Kanban requires atomic collection publication');
    end;
    CheckPrepared;
    for LIndex := 0 to CKanbanLaneCount - 1 do
    begin
      FViewTokens[LIndex] := FViews[LIndex].Subscribe(FCallback.Changed);
      CheckPrepared;
    end;
    AdmitPreparedContract;
    CacheAvailability;
  end;
  LObserver := TKanbanObservation.Create(FPort);
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin

    if FMoveNodes[LIndex] <> nil then
    begin
      FPolicies[LIndex] := TKanbanEnabled.Create(FPort, LIndex);
      FMoveNodes[LIndex].AttachEnabledPolicy(FPolicies[LIndex]);
    end;

    if not FDisabled then
    begin
      RememberEvent(LEventView.PrepareObservation(
        NyxControlEvents(FMoveNodes[LIndex].ID, niRuntime), ntClick,
        LObserver, nopBeforeCallbacks, NyxEvent('')));
      RememberEvent(LEventView.PrepareObservation(
        NyxControlEvents(FItemNodes[LIndex].ID, niRuntime), ntDragEnd,
        LObserver, nopBeforeCallbacks, NyxEvent('')));
      RememberEvent(LEventView.PrepareObservation(
        NyxControlEvents(FItemNodes[LIndex].ID, niRuntime), ntClick,
        LObserver, nopBeforeCallbacks, NyxEvent('')));
      RememberEvent(LEventView.PrepareObservation(
        NyxControlEvents(FItemNodes[LIndex].ID, niRuntime), ntAfterEnter,
        LObserver, nopBeforeCallbacks, NyxEvent('')));
      RememberEvent(LEventView.PrepareCallback(
        NyxControlEvents(FItemNodes[LIndex].ID, niRuntime), ntDragStart,
        FCallback, NyxEvent('')));
      RememberEvent(LEventView.PrepareCallback(
        NyxControlEvents(FItemNodes[LIndex].ID, niRuntime), ntDragOver,
        FCallback, NyxEvent('')));
      RememberEvent(LEventView.PrepareCallback(
        NyxControlEvents(FItemNodes[LIndex].ID, niRuntime), ntDragEnter,
        FCallback, NyxEvent('')));
      RememberEvent(LEventView.PrepareCallback(
        NyxControlEvents(FItemNodes[LIndex].ID, niRuntime), ntDrop,
        FCallback, NyxEvent('')));
    end;
  end;
  CheckPreparedNodes;
end;

procedure TKanbanBinding.CheckPrepared;
var
  LGeneration: INyxCancellationScope;
  LCancelled: Boolean;
begin
  LGeneration := FGeneration;

  if not FPrepared or FRevoked or FCleaned or FDirty or
    (LGeneration = nil) then
  begin
    raise ENyxKanbanCancelled.Create('Prepared Kanban changed or retired before adoption');
  end;
  LCancelled := LGeneration.Cancelled;

  if LCancelled or FRevoked or FCleaned or FDirty or
    (FGeneration <> LGeneration) then
  begin
    raise ENyxKanbanCancelled.Create('Prepared Kanban changed or retired before adoption');
  end;
end;

procedure TKanbanBinding.CheckPreparedNodes;
var
  LIndex: Integer;
  LPolicyIndex: Integer;
  LFound: Boolean;
begin
  CheckPrepared;

  if not FBoardNode.IsRealized or
    (FBoardNode.Parent <> FBoardParent) or
    (FBoardNode.Kind <> NyxKindName(nkKanbanBoard)) or
    (FBoardNode.StoredProp('projection-kind') <> FBoardProps) or
    (FBoardNode.Extensions.ToJSON <> FBoardExtensions) then
  begin
    raise ENyxKanban.Create('Prepared Kanban board identity or intent changed');
  end;
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin

    if OptionalMoveNode(FBoardNode, LIndex) <> FMoveNodes[LIndex] then
    begin
      raise ENyxKanban.Create('Prepared Kanban Move face was replaced');
    end;

    if FMoveNodes[LIndex] <> nil then
    begin

      if FMoveNodes[LIndex].StoredProp(NyxAttributeName(atEnabled), 'true') <>
        FMoveProps[LIndex] then
      begin
        raise ENyxKanban.Create('Prepared Kanban Move availability changed');
      end;
      LFound := False;
      for LPolicyIndex := 0 to FMoveNodes[LIndex].EnabledPolicyCount - 1 do
      begin
        LFound := LFound or
          (FMoveNodes[LIndex].EnabledPolicyAt(LPolicyIndex) = FPolicies[LIndex]);
      end;

      if not LFound then
      begin
        raise ENyxKanban.Create('Prepared Kanban availability policy was detached');
      end;
    end;

    if not FDisabled then
    begin

      if (FBoardNode.Part(LanePath(LIndex, 'items')) <> FItemNodes[LIndex]) or
        not FItemNodes[LIndex].HasCollectionView or
        (FItemNodes[LIndex].ProjectionKind <> NyxKindName(nkList)) or
        (FItemNodes[LIndex].StoredProp(NyxAttributeName(atReadOnly), 'false') <>
          FItemProps[LIndex]) then
      begin
        raise ENyxKanban.Create('Prepared Kanban list role changed');
      end;
      CheckLaneSpec(FItemNodes[LIndex].CollectionView, FOptions, FQueries[LIndex]);
    end;
  end;
  CheckPrepared;
end;

procedure TKanbanBinding.AdmitPreparedContract;
var
  LIndex: Integer;
  LSnapshot: INyxCollectionSnapshot;
begin
  CheckPrepared;
  ValidateNyxKanbanBoardOptions(FBoardNode, FOptions);
  LSnapshot := FStore.Snapshot;
  CheckPrepared;
  LSnapshot.Schema.Definition(FOptions.TitleField);
  CheckPrepared;
  LSnapshot.Schema.Definition(FOptions.LaneField);
  CheckPrepared;
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin

    if FViews[LIndex].Store <> FStore then
    begin
      raise ENyxKanban.Create('Prepared Kanban lanes do not share one exact store');
    end;
    CheckPrepared;

    if FViews[LIndex].Projection <> cpList then
    begin
      raise ENyxKanban.Create('Prepared Kanban requires list projections');
    end;
    CheckPrepared;
    CheckLaneSpec(FViews[LIndex].Spec, FOptions, FQueries[LIndex]);
    CheckPrepared;

    if FViews[LIndex].QueryPolicy.ToData.ToJSON <> FQueries[LIndex] then
    begin
      raise ENyxKanban.Create('Prepared Kanban runtime query changed');
    end;
    CheckPrepared;
  end;
end;

procedure TKanbanBinding.CacheAvailability;
var
  LIndex: Integer;
  LSource: Integer;
  LSelection: INyxCollectionSelection;
  LItem: TNyxItemRef;
  LVisible: Boolean;
begin
  CheckPrepared;
  LSource := FActive;

  if LSource < 0 then
  begin
    for LIndex := 0 to CKanbanLaneCount - 1 do
    begin
      LSelection := FViews[LIndex].Selection;
      CheckPrepared;

      if LSelection.Count = 1 then
      begin
        LItem := LSelection.ItemAt(0);
        LVisible := FViews[LIndex].Snapshot.Has(LItem);
        CheckPrepared;

        if LVisible then
        begin

          if LSource >= 0 then
          begin
            LSource := -1;
            Break;
          end;
          LSource := LIndex;
        end;
      end;
    end;
  end;
  FActive := LSource;
  LVisible := False;

  if LSource >= 0 then
  begin
    LSelection := FViews[LSource].Selection;
    CheckPrepared;

    if LSelection.Count = 1 then
    begin
      LVisible := FViews[LSource].Snapshot.Has(LSelection.ItemAt(0));
      CheckPrepared;
    end;
  end;
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin
    FReadOnly[LIndex] := FItemNodes[LIndex].Prop(
      NyxAttributeName(atReadOnly), 'false') = 'true';
    CheckPrepared;
  end;
  for LIndex := 0 to CKanbanLaneCount - 1 do
  begin
    FAllowed[LIndex] := LVisible and (LIndex <> LSource) and
      not FReadOnly[LIndex];

    if LSource >= 0 then
    begin
      FAllowed[LIndex] := FAllowed[LIndex] and not FReadOnly[LSource];
    end;
  end;
end;

procedure TKanbanBinding.RevokePrepared;
var
  LLifetime: INyxCoordinatorProjectionLifetime;
begin
  LLifetime := FProjectionLifetime;
  FRevoked := True;
  FConnected := False;
  FRefresh := nil;

  if FPort <> nil then
  begin
    FPort.Revoke;
  end;

  if LLifetime <> nil then
  begin
    LLifetime.Revoke;
  end;
end;

procedure TKanbanBinding.DisconnectPrepared;
var
  LLifetime: INyxCoordinatorProjectionLifetime;
  LIndex: Integer;
  LFailure: TNyxText;
  LFailed: Boolean;

  procedure Failed(const AMessage: string);
  begin

    if not LFailed then
    begin
      LFailure := TNyxText(AMessage);
    end;
    LFailed := True;
  end;

begin
  LLifetime := FProjectionLifetime;
  RevokePrepared;

  if FCleaned then
  begin
    Exit;
  end;
  FCleaned := True;
  LFailure := '';
  LFailed := False;
  for LIndex := 0 to High(FSubscriptions) do
  begin
    try
      FSubscriptions[LIndex].Cancel;
    except
      on E: Exception do
      begin
        Failed(E.Message);
      end;
    end;
  end;
  for LIndex := 0 to High(FViewTokens) do
  begin

    if FViewTokens[LIndex] <> nil then
    begin
      try
        FViewTokens[LIndex].Disconnect;
      except
        on E: Exception do
        begin
          Failed(E.Message);
        end;
      end;
    end;
  end;
  for LIndex := 0 to High(FPolicies) do
  begin

    if (FPolicies[LIndex] <> nil) and (FMoveNodes[LIndex] <> nil) then
    begin
      try
        FMoveNodes[LIndex].DetachEnabledPolicy(FPolicies[LIndex]);
      except
        on E: Exception do
        begin
          Failed(E.Message);
        end;
      end;
    end;
    FReadiness[LIndex] := nil;
    FItemNodes[LIndex] := nil;
    FMoveNodes[LIndex] := nil;
  end;

  if LLifetime <> nil then
  begin
    try
      { Explicit typed Disconnect retires the same common scope immediately.
        Its cleanup never calls the child or the enclosing mount back. }
      LLifetime.Disconnect;
    except
      on E: Exception do
      begin
        Failed(E.Message);
      end;
    end;
  end;
  FSubscriptions := nil;
  FViewTokens := nil;
  FPolicies := nil;
  ClearDrag;
  FItems := nil;
  FMoves := nil;
  FViews := nil;
  FAtomic := nil;
  FStore := nil;
  FBoard := nil;
  FBoardNode := nil;
  FGeneration := nil;
  FScheduler := nil;
  FCallback := nil;
  FPort := nil;
  FScopedRefresh := nil;
  FProjectionLifetime := nil;

  if LFailed then
  begin
    raise ENyxKanban.Create('Kanban disconnected; cleanup receiver failed: ' + LFailure);
  end;
end;

constructor TPreparedKanban.Create(const ABoard: INyxKanbanBoard;
  const AOptions: TNyxKanbanOptions; const AViews: INyxCollectionBindings;
  const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  ARefresh: TNyxKanbanRefresh; ADisabled: Boolean;
  const AScopedRefresh: INyxCoordinatorRefresh;
  const AProjectionLifetime: INyxCoordinatorProjectionLifetime);
begin
  inherited Create;
  FEventView := AEventView;
  FOwner := TKanbanBinding.CreatePrepared(ABoard, AOptions, AViews,
    FEventView, AScheduler, ARefresh, ADisabled, AScopedRefresh, AProjectionLifetime);
  FBindingLease := FOwner;
end;

destructor TPreparedKanban.Destroy;
begin
  try
    DoDisconnect;
  except
    { All tokens/policies were attempted after revocation; destructors do not
      turn a cleanup diagnostic into rollback or a dangling weak receiver. }
  end;
  inherited Destroy;
end;

function TPreparedKanban.GetBinding: INyxKanbanBinding;
begin
  Result := nil;

  if (FOwner <> nil) and not FOwner.FDisabled then
  begin
    Result := FBindingLease;
  end;
end;

procedure TPreparedKanban.Revoke;
begin

  if FOwner <> nil then
  begin
    FOwner.RevokePrepared;
  end;
end;

procedure TPreparedKanban.DoDisconnect;
begin

  if FOwner <> nil then
  begin
    FOwner.DisconnectPrepared;
  end;
end;

procedure TPreparedKanban.Disconnect;
var
  LKeepAlive: INyxPreparedKanbanBinding;
begin
  LKeepAlive := Self;
  DoDisconnect;
end;

procedure TPreparedKanban.Validate;
var
  LKeepAlive: INyxPreparedKanbanBinding;
  LScheduler: INyxScheduler;
  LIndex: Integer;
begin
  LKeepAlive := Self;

  if FValidated or FInstalled or FRetired then
  begin
    raise ENyxKanban.Create('Prepared Kanban admission is single use');
  end;
  FOwner.CheckPrepared;
  LScheduler := FOwner.FScheduler;
  LScheduler.RequireUI;
  FOwner.CheckPrepared;

  if not FOwner.FDisabled then
  begin
    FOwner.AdmitPreparedContract;
    for LIndex := 0 to CKanbanLaneCount - 1 do
    begin

      if not FOwner.FReadiness[LIndex].PublicationReady then
      begin
        raise ENyxKanban.Create('Prepared Kanban collection view is busy');
      end;
      FOwner.CheckPrepared;

      if (FOwner.FItemNodes[LIndex].Prop(NyxAttributeName(atReadOnly), 'false') =
        'true') <> FOwner.FReadOnly[LIndex] then
      begin
        raise ENyxKanban.Create('Prepared Kanban read-only availability changed');
      end;
      FOwner.CheckPrepared;
    end;
  end;
  FValidated := True;
  CheckReady;
end;

procedure TPreparedKanban.CheckReady;
var
  LKeepAlive: INyxPreparedKanbanBinding;
begin
  LKeepAlive := Self;

  if not FValidated or FInstalled or FRetired then
  begin
    raise ENyxKanban.Create('Prepared Kanban readiness requires admitted nodes');
  end;
  FOwner.CheckPreparedNodes;
end;

procedure TPreparedKanban.Install;
begin

  if not FValidated or FInstalled or FRetired or FOwner.FRevoked then
  begin
    Exit;
  end;
  FOwner.FConnected := True;
  FInstalled := True;
end;

procedure TPreparedKanban.Notify;
begin
  { Candidate Sync already rendered availability. Activation makes no borrowed
    host call; ordinary live view notifications request subsequent refreshes. }
end;

procedure TPreparedKanban.DoRetire;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if not FInstalled then
  begin
    try
      DoDisconnect;
    except
      { Cleanup has already revoked every weak receiver and attempted all rows. }
    end;
  end;
  FEventView := nil;
end;

procedure TPreparedKanban.Retire;
var
  LKeepAlive: INyxPreparedKanbanBinding;
begin
  LKeepAlive := Self;
  DoRetire;
end;

function PrepareNyxKanbanBinding(const ABoard: INyxKanbanBoard;
  const AOptions: TNyxKanbanOptions; const AViews: INyxCollectionBindings;
  const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  ARefresh: TNyxKanbanRefresh): INyxPreparedKanbanBinding;
var
  LBoard: INyxKanbanBoard;
  LOptions: TNyxKanbanOptions;
  LViews: INyxCollectionBindings;
  LEventView: INyxPreparedEventView;
  LScheduler: INyxScheduler;
begin
  LBoard := ABoard;
  LOptions := AOptions.Copy;
  LViews := AViews;
  LEventView := AEventView;
  LScheduler := AScheduler;
  Result := TPreparedKanban.Create(LBoard, LOptions, LViews, LEventView,
    LScheduler, ARefresh, False);
end;

function PrepareNyxKanbanDisabled(const ABoard: INyxKanbanBoard;
  const AEventView: INyxPreparedEventView;
  const AScheduler: INyxScheduler): INyxPreparedKanbanBinding;
var
  LBoard: INyxKanbanBoard;
  LEventView: INyxPreparedEventView;
  LScheduler: INyxScheduler;
begin
  LBoard := ABoard;
  LEventView := AEventView;
  LScheduler := AScheduler;
  Result := TPreparedKanban.Create(LBoard, Default(TNyxKanbanOptions), nil,
    LEventView, LScheduler, nil, True);
end;

function NewNyxKanbanBinding(const ABoard: INyxKanbanBoard;
  const AOptions: TNyxKanbanOptions; const AViews: INyxCollectionBindings;
  const AEvents: INyxEvents; const AScheduler: INyxScheduler;
  ARefresh: TNyxKanbanRefresh): INyxKanbanBinding;
var
  LBoard: INyxKanbanBoard;
  LOptions: TNyxKanbanOptions;
  LViews: INyxCollectionBindings;
  LEvents: INyxEvents;
  LScheduler: INyxScheduler;
  LOwner: TKanbanBinding;
  LBinding: INyxKanbanBinding;
begin
  LBoard := ABoard;
  LOptions := AOptions.Copy;
  LViews := AViews;
  LEvents := AEvents;
  LScheduler := AScheduler;
  LOwner := TKanbanBinding.Create(LBoard, LOptions, LViews, LEvents,
    LScheduler, ARefresh);
  LBinding := LOwner;
  LOwner.Activate(LEvents);
  Result := LBinding;
end;

constructor TCoordinatorKanbanPlan.Create(AOwner: TPreparedKanban;
  const APrepared: INyxPreparedKanbanBinding);
begin
  inherited Create;
  FOwner := AOwner;
  FPrepared := APrepared;

  if (FOwner = nil) or (FPrepared = nil) or
    not Supports(FPrepared, INyxPublicationReadiness, FReadiness) then
  begin
    raise ENyxKanban.Create('Common Kanban requires prepared publication readiness');
  end;

  if not FOwner.FOwner.FDisabled and
    not Supports(FOwner.FBindingLease, INyxCoordinatorBinding, FBinding) then
  begin
    raise ENyxKanban.Create('Common Kanban must retain its exact specialized binding');
  end;
end;

function TCoordinatorKanbanPlan.GetBinding: INyxCoordinatorBinding;
begin
  Result := FBinding;

  if Result = nil then
  begin
    Result := Self;
  end;
end;

function TCoordinatorKanbanPlan.GetConnected: Boolean;
var
  LLease: INyxPreparedCoordinator;
begin
  LLease := Self;
  Result := FOwner.FOwner.GetConnected;
end;

procedure TCoordinatorKanbanPlan.Revoke;
begin
  FPrepared.Revoke;
end;

procedure TCoordinatorKanbanPlan.Disconnect;
var
  LLease: INyxPreparedCoordinator;
  LPrepared: INyxPreparedKanbanBinding;
begin
  LLease := Self;
  LPrepared := FPrepared;
  LPrepared.Disconnect;
end;

procedure TCoordinatorKanbanPlan.Validate;
var
  LLease: INyxPreparedCoordinator;
  LPrepared: INyxPreparedKanbanBinding;
begin
  LLease := Self;
  LPrepared := FPrepared;
  LPrepared.Validate;
end;

procedure TCoordinatorKanbanPlan.CheckReady;
var
  LLease: INyxPreparedCoordinator;
  LReadiness: INyxPublicationReadiness;
begin
  LLease := Self;
  LReadiness := FReadiness;
  LReadiness.CheckReady;
end;

procedure TCoordinatorKanbanPlan.Install;
begin
  FPrepared.Install;
end;

procedure TCoordinatorKanbanPlan.Notify;
begin
  FPrepared.Notify;
end;

procedure TCoordinatorKanbanPlan.Retire;
begin
  FPrepared.Retire;
end;

function TKanbanCoordinatorFactory.Prepare(const AProjection: INyxCoordinatorProjection;
  const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
  const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
begin
  Result := nil;
  raise ENyxKanban.Create('Kanban preparation requires the canonical common driver');
end;

function TKanbanCoordinatorFactory.PrepareCanonical(const ACanonical: INyxControl;
  const AProjection: INyxCoordinatorProjection;
  const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
  const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
var
  LCanonical: INyxControl;
  LControl: INyxControl;
  LBoard: INyxKanbanBoard;
  LProjection: INyxCoordinatorProjection;
  LLifetime: INyxCoordinatorProjectionLifetime;
  LViews: INyxCollectionBindings;
  LView: INyxPreparedEventView;
  LScheduler: INyxScheduler;
  LRefresh: INyxCoordinatorRefresh;
  LRoot: TNyxControlRef;
  LIntent: TNyxKanbanIntent;
  LOptions: TNyxKanbanOptions;
  LOwner: TPreparedKanban;
  LPrepared: INyxPreparedKanbanBinding;
begin
  LCanonical := ACanonical;
  LProjection := AProjection;
  LViews := AViews;
  LView := AView;
  LScheduler := AScheduler;
  LRefresh := ARefresh;
  LIntent := TNyxKanbanIntent.FromData(AIntent);

  if (LCanonical = nil) or (LProjection = nil) or
    not Supports(LProjection, INyxCoordinatorProjectionLifetime, LLifetime) then
  begin
    raise ENyxKanban.Create('Common Kanban requires canonical control and scoped retirement');
  end;
  LRoot := LProjection.Root;

  if (LCanonical.ID <> LRoot.ID) or not LCanonical.Node.IsRealized or
    (LCanonical.Kind <> NyxKindName(nkKanbanBoard)) then
  begin
    raise ENyxKanban.Create('Common Kanban canonical control differs from its scoped target');
  end;
  { The driver deliberately retains a generic canonical facade. Recover the
    actual specialized implementation from that same descriptor, never clone it. }
  LControl := RetainNyxControl(LCanonical.Node);

  if not Supports(LControl, INyxKanbanBoard, LBoard) then
  begin
    raise ENyxKanban.Create('Common Kanban requires its real specialized board');
  end;
  LOptions := Default(TNyxKanbanOptions);

  if LIntent.Mode = nkiConfigured then
  begin
    LOptions := LIntent.Options;
  end;
  LOwner := TPreparedKanban.Create(LBoard, LOptions, LViews, LView,
    LScheduler, nil, LIntent.Mode = nkiDisabled, LRefresh, LLifetime);
  LPrepared := LOwner;
  Result := TCoordinatorKanbanPlan.Create(LOwner, LPrepared);
end;

function NewNyxKanbanCoordinatorFactory: INyxCoordinatorFactory;
begin
  Result := TKanbanCoordinatorFactory.Create;
end;

end.
