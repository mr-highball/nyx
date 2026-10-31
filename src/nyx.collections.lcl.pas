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
unit nyx.collections.lcl;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  Controls,
  Types,
  nyx.gestures.lcl,
  nyx.events, nyx.text, nyx.observation,
  nyx.collections.view,
  nyx.collections.presentation,
  nyx.collections.alignment,
  nyx.collections.mount,
  nyx.collections.position,
  nyx.collections.recovery,
  nyx.collections.clipboard.actions, nyx.collections.draft,
  nyx.collections.lcl.grid, nyx.collections.range.stroke,
  nyx.collections.range.pointer, nyx.gestures, nyx.scheduler,
  nyx.content.projection.lcl;

type
  { Borrowed synchronous canonical Click port. False covers blocked/rejected,
    retired and committed-notification-failure outcomes; True also permits an
    accepted control with no authored action. The mount clears it on detach. }
  TNyxLCLColumnClick = function(Sender: TObject): Boolean of object;
  { Native compound-editor context uses the grid's borrowed prepared dispatch
    port. The renderer keeps canonical events single-owned; no event is replayed. }
  TNyxLCLCellContextDispatch = nyx.collections.lcl.grid.TNyxGridCellContextDispatch;
  { Native pointer preparation preserves the grid's weak synchronous dispatch
    contract. A renderer supplies its existing original-event receiver; a direct
    host omitting it retains ordinary native row/editor input behavior. }
  TNyxLCLRangePointerDispatch = nyx.collections.lcl.grid.TNyxGridRangePointerDispatch;

  { Optional native hint composition. The mount borrows its connected control
    and combines current authored text with its own runtime guidance. Updating
    the text never refreshes data or dismisses an editor; detach restores the
    latest authored grid hint. Retired mounts ignore subsequent updates. }
  INyxLCLCollectionHints = interface(IInterface)
    ['{1A31E921-E305-44E4-A13C-8E4D8DBB16FD}']
    procedure SetAuthoredHint(const AHint: TNyxText);
  end;

  { Optional native sustained-pointer route. Each call reserves one synchronous
    envelope before hit/focus reads. Original event flags and detached pending
    data survive messages; no entry, event router or renderer is retained by a
    stroke. Down keeps the existing pre-focus grid port; this facet admits Move
    and Release only. Non-primary buttons and unrelated surfaces keep old input. }
  INyxLCLCollectionCellRangeStrokeInput = interface(IInterface)
    ['{F32588E0-F27B-4425-B2F9-6A6DB5B06406}']
    function BeginRangeStroke(ASender: TObject; APhase: TNyxCollectionRangeStrokePhase;
      AShift: TShiftState; AX, AY: Integer;
      const AGeneration: INyxCancellationScope): INyxCollectionCellRangePointerEntry;
  end;

  { Reports the sealed original canonical capture directive before its platform
    effect. Explicit caller capture/release outranks the optional range default;
    this facet belongs only to the current synchronous pointer envelope. }
  INyxLCLRangeCaptureDecision = interface(IInterface)
    ['{C040929A-0674-4943-BD7B-4D725246077E}']
    procedure ObservePointerRequest(ARequest: TNyxPointerRequest);
  end;

  { Optional native observation publication. The returned aggregate owns only
    observer/input leases and copied context, never its mount/view/store. Prepare
    it before retiring a renderer and publish the actual accepted router revision
    before releasing input. Manual opted-in hosts supply that same real context. }
  INyxLCLCollectionObservation = interface(IInterface)
    ['{29756C6D-2E2B-4963-B46E-616BA21C95DA}']
    function GetEditingPublication: INyxObservationPublication;
    function EditingDispatching: Boolean;
  end;

  { Optional pre-canonical context entry resolves only actual owned grid cells.
    Editors retain their native path unless focused-draft actions explicitly
    opt in. A stale reserved envelope remains claimed instead of replaying an
    old native menu. A negative point uses the currently owned logical cell
    for ordinary keyboard context requests. }
  INyxLCLCollectionCellContextInput = interface(IInterface)
    ['{A29C10EB-1B0E-4565-B363-C8BA0BDA676F}']
    function BeginCellContext(Sender: TObject; const APosition: TPoint):
      INyxCollectionClipboardCommand;
  end;

{ Flags-only bridge for native observers. It owns the canonical input lifetime,
  never a mount or widget; epoch/Ready reads remain safe after target retirement. }
function NyxLCLCollectionCaptureInput(
  const ALifetime: INyxCollectionInputLifetime): INyxLCLCaptureInput;

{ Native adapter boundary for Nyx's real TListBox/TStringGrid/TTreeView controls.
  Renderer retains/disconnects the mount before destroying its borrowed control.
  No dataset is serialized into newline/tab-separated items. Lists select rows;
  grids and tree labels edit permitted columns through atomic typed admission.
  Opted-in arrangement or text/density requires the caller's real frame/router/runtime
  origin before any hook/handle is installed. Ordinary Apps supply them; direct
  hosts use the same context and publication port. Legacy non-layout calls keep
  their optional-argument defaults and unchanged native editing path. The weak
  capture handler reports canonical pane observations and is revoked before
  its binding or any borrowed native control is destroyed. }
function MountNyxLCLCollection(AControl: TControl;
  const AView: INyxCollectionView;
  AInputPhase: TNyxCollectionInputPhase = ncipReady;
  const AInputGate: INyxCollectionInputGate = nil;
  const AInputFrame: INyxNativeGestureFrame = nil;
  const AInputEvents: INyxEvents = nil;
  const AOriginID: TNyxText = '';
  AColumnClick: TNyxLCLColumnClick = nil;
  ACellContext: TNyxLCLCellContextDispatch = nil;
  ARangePointer: TNyxLCLRangePointerDispatch = nil;
  ACaptureHandler: TNyxLCLCaptureHandler = nil): INyxCollectionMount;

{ Runtime measurement limits are copied before activation, independently of
  authored policies. The physical frame/router requirements remain unchanged. }
function MountNyxLCLCollectionWithTextLimits(AControl: TControl;
  const AView: INyxCollectionView; const ALimits: TNyxTableTextWorkLimits;
  AInputPhase: TNyxCollectionInputPhase = ncipReady;
  const AInputGate: INyxCollectionInputGate = nil;
  const AInputFrame: INyxNativeGestureFrame = nil;
  const AInputEvents: INyxEvents = nil;
  const AOriginID: TNyxText = '';
  AColumnClick: TNyxLCLColumnClick = nil;
  ACellContext: TNyxLCLCellContextDispatch = nil;
  ARangePointer: TNyxLCLRangePointerDispatch = nil;
  ACaptureHandler: TNyxLCLCaptureHandler = nil): INyxCollectionMount;

implementation

uses
  SysUtils,
  nyx.types,
  Math,
  Forms,
  nyx.collections.refresh,
  StdCtrls,
  ComCtrls,
  Grids,
  Graphics,
  LCLType,
  nyx.state,
  nyx.formatting,
  nyx.editing,
  nyx.editing.lcl,
  nyx.data,
  nyx.collections,
  nyx.collections.grid,
  nyx.collections.columns,
  nyx.collections.arrangement,
  nyx.collections.arrangement.controls,
  nyx.collections.arrangement.presenter,
  nyx.collections.arrangement.lcl,
  nyx.layout.flow,
  nyx.layout.constraints,
  nyx.publication,
  nyx.collections.view.types,
  nyx.collections.query,
  nyx.collections.query.controls,
  nyx.typeahead,
  nyx.collections.selection,
  nyx.collections.cell.actions,
  nyx.collections.representation,
  nyx.collections.text.layout,
  nyx.collections.clipboard,
  nyx.collections.range,
  nyx.collections.range.actions,
  nyx.collections.range.commands,
  nyx.collections.range.navigation,
  nyx.collections.tree.rows;

type
  { Forward mount is borrowed by native ports only while a local specialized
    lease and the original input frame protect its entered callback. }
  TNativeMount = class;
  { Detached adapter forwards only atomic incarnation facts. It keeps neither
    native controls nor their collection/view owners alive. }
  TCollectionCaptureInput = class(TInterfacedObject, INyxLCLCaptureInput)
  private
    FLifetime: INyxCollectionInputLifetime;
  public
    constructor Create(const ALifetime: INyxCollectionInputLifetime);
    function GetCurrent: Boolean;
    function GetReady: Boolean;
    function GetEpoch: Integer;
  end;
  { Synchronous native envelope borrows its mount only while the wrapped entry
    owns it. Captured generation flags fence canonical callbacks and original
    focus. Neither the mount nor a pending stroke retains this wrapper. }
  TNativeRangePointerCommand = class(TInterfacedObject,
    INyxCollectionCellRangePointerEntry, INyxLCLRangeCaptureDecision)
  private
    FCommand: INyxCollectionCellRangePointerEntry;
    FOwner: TNativeMount;
    FGeneration: INyxCancellationScope;
    FSubject: INyxCollectionRangeStroke;
    FEndpoint: TNyxCollectionCellIdentity;
    FControl: TNyxCollectionStringGrid;
    FPhase: TNyxCollectionRangeStrokePhase;
    FArm: Boolean;
    FCaptureRequested: Boolean;
    FCompleted: Boolean;
    FCompleting: Boolean;
  public
    constructor Create(AOwner: TNativeMount;
      const ACommand: INyxCollectionCellRangePointerEntry;
      const AGeneration: INyxCancellationScope);
    destructor Destroy; override;
    procedure ResolveCells(const AOrigin, ATarget: TNyxCollectionCellIdentity);
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure ObservePointerRequest(ARequest: TNyxPointerRequest);
    procedure Retire;
  end;
  { Entered native focus shield retains the exact mount and uses the existing
    counted grid transition. No session/controller owns this temporary guard. }
  TNativeCellPresentation = class(TInterfacedObject, INyxCollectionCellPresentation)
  private
    FOwner: TNativeMount;
    FLease: INyxCollectionMount;
    FInput: INyxCollectionInputLifetime;
    FGrid: TNyxCollectionStringGrid;
    FActive: Boolean;
  public
    constructor Create(AOwner: TNativeMount);
    destructor Destroy; override;
    procedure Leave;
  end;
  { One cell observer's private, managed control port. Its callback retains a
    local lease; the weak wake only queues the mount's next UI turn. The outer
    input surface covers the observer's WindowProc and is removed first. }
  ICellEditing = interface(IInterface)
    ['{F7825EF5-2EBC-4D1A-B35D-1C9536224517}']
    function Ready: Boolean;
    function Composing: Boolean;
    function Dispatching: Boolean;
    procedure PublishRevision(ARevision: Integer);
    procedure Revoke;
    procedure Disconnect;
  end;
  TCellEditing = class(TInterfacedObject, ICellEditing)
  private
    FLifetime: INyxCollectionInputLifetime;
    FSurface: INyxGridInputSurface;
    FContinuity: INyxGridInputContinuity;
    FObserver: INyxLCLEditingObserver;
    FPublication: INyxObservationPublication;
    FWake: TNotifyEvent;
    FConnected, FOuterAttached: Boolean;
    procedure Editing(const AOriginID: TNyxText;
      const AEditing: TNyxEditingSnapshot);
  public
    constructor Create(AGrid: TNyxCollectionStringGrid;
      const ALifetime: INyxCollectionInputLifetime; const AEvents: INyxEvents;
      const AOriginID: TNyxText; AWake: TNotifyEvent);
    destructor Destroy; override;
    function Ready: Boolean;
    function Composing: Boolean;
    function Dispatching: Boolean;
    procedure PublishRevision(ARevision: Integer);
    procedure Revoke;
    procedure Disconnect;
  end;

  { Fixed-small observation aggregate: one center and at most two edge editors.
    It retains the real router and cached ADMITTED revision for lazy panes.
    Revoke changes only flags/weak receivers; publication only assigns already
    prepared observer epochs. Neither operation touches a widget or model. }
  IColumnEditing = interface(INyxObservationPublication)
    ['{49893923-9033-4621-BAEC-0375E452A36C}']
    procedure Add(AGrid: TNyxCollectionStringGrid; AObserveCapture: Boolean = False);
    function Continuity: INyxGridInputContinuity;
    function Composing: Boolean;
    function Dispatching: Boolean;
    procedure Revoke;
    procedure Disconnect;
  end;
  { Owns cell observers and optional OUTER capture hooks for lazy panes. Revoke
    clears the weak canonical handler; Disconnect restores capture before editor
    hooks. Publication changes only already-prepared observer revision flags. }
  TColumnEditing = class(TInterfacedObject, INyxObservationPublication,
    IColumnEditing)
  private
    FCells: array[0..2] of ICellEditing;
    FCaptures: array[0..2] of INyxLCLCaptureObserver;
    FCapturePublications: array[0..2] of INyxObservationPublication;
    FCaptureHandler: TNyxLCLCaptureHandler;
    FCaptureInput: INyxLCLCaptureInput;
    FFrame: INyxNativeGestureFrame;
    FCount: Integer;
    FRevision: Integer;
    FLifetime: INyxCollectionInputLifetime;
    FEvents: INyxEvents;
    FOriginID: TNyxText;
    FWake: TNotifyEvent;
    FRevoked: Boolean;
    FContinuity: INyxGridInputContinuity;
    procedure CaptureChanged(const AOriginID: TNyxText; ATrigger: TNyxTrigger);
  public
    constructor Create(const ALifetime: INyxCollectionInputLifetime;
      const AEvents: INyxEvents; const AOriginID: TNyxText; AWake: TNotifyEvent;
      ACaptureHandler: TNyxLCLCaptureHandler; const AFrame: INyxNativeGestureFrame);
    destructor Destroy; override;
    procedure Add(AGrid: TNyxCollectionStringGrid; AObserveCapture: Boolean = False);
    function Continuity: INyxGridInputContinuity;
    function GetReady: Boolean;
    procedure PublishRevision(ARevision: Integer);
    function Composing: Boolean;
    function Dispatching: Boolean;
    procedure Revoke;
    procedure Disconnect;
  end;
  { A borrowed editable projection of one logical edge. Only a canonical index
    vector is duplicated: data, query, selection and row geometry belong to the
    same view/outer table. Revoke ends every reader/hook before Grid is freed. }
  TColumnPane = class
  public
    Owner: TNativeMount;
    Grid: TNyxCollectionStringGrid;
    { The exact renderer dispatch owner; no mount/view backreference. It may
      retire an entered pane independently of the outer panel's retirement. }
    Frame: INyxNativeGestureFrame;
    InputLifetime: INyxCollectionInputLifetime;
    Columns: TNyxCollectionColumnOrder;
    constructor Create(AOwner: TNativeMount);
    destructor Destroy; override;
    function Text(AColumn, ARow: Integer): TNyxText;
    function Display(AColumn, ARow: Integer;
      out APresentation: TNyxValuePresentation): TNyxText;
    function TextLayout(AColumn, ARow: Integer;
      out ALayout: TNyxTableTextLayout): Boolean;
    function Alignment(AColumn, ARow: Integer;
      out APolicy: TNyxColumnAlignmentPolicy): Boolean;
    function Editable(AColumn, ARow: Integer): Boolean;
    function RangeSelected(AColumn, ARow: Integer): Boolean;
    procedure Revoke;
  end;

  { The single active logical native edit. Capturing suspends validation under
    the mount's update guard; it never commits draft text to a different field
    after ordering or pane changes. No widget address is part of this lease. }
  TColumnEditLease = record
    Active, HadFocus: Boolean;
    Item: TNyxItemRef;
    Field: TNyxColumnIdentity;
    Accepted: TNyxStateValue;
    Text: String;
    Start, Length: Integer;
    HasNativeRange: Boolean;
    Selection: TNyxTextSelection;
  end;

  { Managed logical font copies outlive every outward sample getter. Typography
    is copied without modifying live controls, including native height rounding
    under canvas zoom. This owner retains no grid, mount or renderer. }
  TNativeColumnFitFonts = class(TInterfacedObject)
  public
    Header: TFont;
    Body: TFont;
    constructor Create(AGrid: TNyxCollectionStringGrid);
    destructor Destroy; override;
  end;

  { Drawing facts share a managed font-copy lease across record copies. Font
    pointers borrow that lease, never native controls. The metric then owns its
    separate witness; acquisition/Matches still belong to the UI thread. }
  TNativeColumnFitSurface = record
    Fonts: IInterface;
    HeaderFont: TFont;
    BodyFont: TFont;
    HeaderText: TNyxText;
    HeaderStyle: TTextStyle;
    BodyStyle: TTextStyle;
    PPI: Integer;
    HeaderInset: Integer;
    BodyInset: Integer;
  end;

  { Adapter-local actions keep Fit permission independent of manual Resize. }
  TNativeColumnLayoutAction = (nclaResize, nclaFit);

  { One physical column intention. Waiting work retains immutable model/input
    leases and copied map facts, never its mount. The active call/final participant
    separately retains the mount. OnHeaderSized supplies a Resize width; Fit
    measures accepted text later. Monitor DPI and canvas projection are captured
    independently; a zoom epoch change retires an unfinished physical resize.
    No callback may recapture its launch proof. }
  TNativeColumnLayoutRequest = class(TInterfacedObject)
  public
    View: INyxCollectionView;
    Columns: INyxCollectionColumns;
    Capture: INyxCollectionColumnsCapture;
    Input: INyxCollectionInputLifetime;
    Editing: IColumnEditing;
    Activities: array[0..2] of INyxGridValidationActivity;
    Field: TNyxColumnIdentity;
    Grid, Target: TStringGrid;
    Serial, Epoch, Column, Physical, Revision, LayoutSerial, Mutation, PPI: Integer;
    ProjectionEpoch: Integer;
    ContentScale: Double;
    Width, Minimum, Maximum, ReleaseStamp: Integer;
    Action: TNativeColumnLayoutAction;
    DoubleStamp: Integer;
    Allowed, Pending, Publishing, Retired, Installed: Boolean;
    constructor Create(AOwner: TNativeMount; ATarget: TStringGrid;
      AColumn, APhysical, ASerial: Integer; AAction: TNativeColumnLayoutAction);
  end;

  { One native button's original command, shared only through its managed
    interface. Entered calls retain the mount; the physical button is never
    retained. Disconnect retires its port before component disposal. }
  TNativeColumnButtonCommand = class(TInterfacedObject, INyxGridColumnButtonCommand)
  private
    FOwner: TNativeMount;
    FRequest: TNativeColumnLayoutRequest;
    FRequestLease: IInterface;
    FInvoked: Boolean;
    FTransferred: Boolean;
    FRetired: Boolean;
    function LifetimeCurrent: Boolean;
  public
    constructor Create(AOwner: TNativeMount; ARequest: TNativeColumnLayoutRequest;
      const ARequestLease: IInterface);
    destructor Destroy; override;
    function DispatchCurrent: Boolean;
    procedure Invoke;
    procedure Retire;
  end;

  { One persistent manager launch retains its original input incarnation while
    a real native button is pressed. Canonical callbacks keep ordinary lifetime
    continuity, but opening the default form requires the original epoch/map.
    Recovery selects the same entered command route with data-only review. }
  TNativeColumnManagerCommand = class(TInterfacedObject, INyxGridColumnButtonCommand)
  private
    FOwner: TNativeMount;
    FOwnerLease: INyxCollectionMount;
    FManager: INyxColumnArrangementManager;
    FCells: INyxCollectionCellActions;
    FRecovery: Boolean;
    FView: INyxCollectionView;
    FInput: INyxCollectionInputLifetime;
    FEpoch: Integer;
    FSerial: Integer;
    FMutation: Integer;
    FRevision: Integer;
    FLayoutSerial: Integer;
    FInvoked: Boolean;
    FRetired: Boolean;
    function LifetimeCurrent: Boolean;
  public
    constructor Create(AOwner: TNativeMount; ASerial: Integer; ARecovery: Boolean = False);
    function DispatchCurrent: Boolean;
    procedure Invoke;
    procedure Retire;
  end;

  { Synchronous final participant for the SAME queued request. Outward native
    observation readiness finishes in Validate. All final checks use retained
    canonical activity/composition/input facts, with no native widget getter. }
  TNativeColumnLayoutFence = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FOwner: TNativeMount;
    FOwnerLease: INyxCollectionMount;
    FRequest: TNativeColumnLayoutRequest;
    FRequestLease: IInterface;
    FStarted, FValidated, FRetired: Boolean;
  public
    constructor Create(AOwner: TNativeMount; ARequest: TNativeColumnLayoutRequest;
      const ARequestLease: IInterface);
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { The original mounted attempt owns model/input and canonical native activity
    leases. It never retains a queued callback. UI metric checks occur only in
    Validate; CheckReady uses inert map/epoch/continuity/activity facts only. }
  TNativeColumnFitFence = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FOwner: TNativeMount;
    FOwnerLease: INyxCollectionMount;
    FView: INyxCollectionView;
    FLaunch: INyxCollectionColumnsCapture;
    FInput: INyxCollectionInputLifetime;
    FEditing: IColumnEditing;
    FActivities: array[0..2] of INyxGridValidationActivity;
    FField: TNyxColumnIdentity;
    FGrid: TStringGrid;
    FMetric: INyxLCLColumnFitMeasurement;
    FColumn, FEpoch, FMutation, FRevision, FLayoutSerial, FCommandSerial: Integer;
    FValidationStarted, FValidated, FInstalled, FRetired: Boolean;
  public
    constructor Create(AOwner: TNativeMount;
      const ALaunch: INyxCollectionColumnsCapture; const AField: TNyxColumnIdentity);
    destructor Destroy; override;
    procedure RequireCurrent;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { Exact native collection presentation. Borrowed controls are retired through
    the shared physical frame; copied drafts never retain widgets or authority
    to edit, and input/layout commands retain their original admission facts. }
  TNativeMount = class(TNyxCollectionMountBase, INyxCollectionEditMount,
    INyxLCLCollectionObservation, INyxLCLCollectionCellContextInput,
    INyxCollectionFocusedDraftInput, INyxLCLCollectionCellRangeStrokeInput,
    INyxLCLCollectionHints)
  private
    FList: TListBox;
    FGrid: TStringGrid;
    FGridReaderAttached: Boolean;
    { A viewport-sized measured cache owns lines only. The outer grid owns row
      geometry for all three panes; painting performs no source or font work. }
    FTextLayouts: array of array of TNyxTableTextLayout;
    FTextLayoutFirst: Integer;
    FTextLayoutTotal: Integer;
    FTextLayoutRows: array of Integer;
    FTextLayoutApplying: Boolean;
    { The exact paint/top-left hooks are admitted once, including a first
      runtime-only activation after the original table has already mounted. }
    FTextLayoutAttached: Boolean;
    FPreviousDefaultRowHeight: Integer;
    FTextMeasureCanvas: TCanvas;
    function MeasurePresentationText(const AText: TNyxText): Double;
    procedure PrepareTextRows(const AData: INyxCollectionSnapshot);
    function CanonicalTextLayout(AColumn, ARow: Integer;
      out ALayout: TNyxTableTextLayout): Boolean;
    function GridTextLayout(AColumn, ARow: Integer;
      out ALayout: TNyxTableTextLayout): Boolean;
    function CanonicalAlignment(AColumn, ARow: Integer;
      out APolicy: TNyxColumnAlignmentPolicy): Boolean;
    function GridAlignment(AColumn, ARow: Integer;
      out APolicy: TNyxColumnAlignmentPolicy): Boolean;
  private
    { Exact copied page bounds/field bits drive paint without querying a live
      store/view or a snapshot row from the native DrawCell callback. }
    FRangePaint: INyxCollectionCellRangeIntent;
    { Preview readiness is a detached flags/data owner, not a live view. The
      copied row/field cache remains the only native paint membership source. }
    FRangePaintStroke: INyxCollectionRangeStroke;
    FRangeFirstRow: Integer;
    FRangeLastRow: Integer;
    FRangePaintFields: array of Boolean;
    FInputGrid: TNyxCollectionStringGrid;
    FInputGridAttached: Boolean;
    FColumnFrame: INyxNativeGestureFrame;
    { Exact immutable field faces, owned as components by the outer grid. }
    FFitButtons: array of TNyxCollectionFitButton;
    FColumnsButton: TNyxCollectionColumnButton;
    FRecoveryButton: TNyxCollectionColumnButton;
    { Grid-owned readonly status surface has no binding/host authority. Its
      stable caption band is allocated at service admission; text updates never
      refresh data or move an existing editor. Retirement uses the same frame. }
    FClipboardStatus: TMemo;
    FClipboardStatusPending: Boolean;
    FClipboardStatusSyncing: Boolean;
    FRecoverySyncing: Boolean;
    FRecoveryCountPending: Boolean;
    FColumnsButtonSerial: Integer;
    FColumnToolbarApplied: Boolean;
    FColumnToolbarHeight: Integer;
    FColumnClick: TNyxLCLColumnClick;
    FCellContextDispatch: TNyxLCLCellContextDispatch;
    FRangePointerDispatch: TNyxLCLRangePointerDispatch;
    { The mounted adapter borrows its physical surfaces and owns the ordinary
      event source only while connected. Pending gestures retain flags alone;
      retirement clears these ports before any control/frame can be released. }
    FRangeEvents: INyxEvents;
    FRangeGesture: INyxCollectionRangeStroke;
    FRangeGeneration: INyxCancellationScope;
    FRangeControl: TNyxCollectionStringGrid;
    FRangeCaptured: Boolean;
    procedure FinishRangeGesture(const ASubject: INyxCollectionRangeStroke);
    procedure CancelRangeSurface(Sender: TObject);
    procedure ArmRangeGesture(AControl: TNyxCollectionStringGrid;
      const AGeneration: INyxCancellationScope;
      const AEndpoint: TNyxCollectionCellIdentity);
    function PrepareRangePointer(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer): INyxCollectionCellRangePointerEntry;
    function GridRangeSelected(AColumn, ARow: Integer): Boolean;
    function CanonicalRangeSelected(AColumn, ARow: Integer): Boolean;
    function PrepareFitButton(Sender: TObject): INyxGridColumnButtonCommand;
    function PrepareColumnsButton(Sender: TObject): INyxGridColumnButtonCommand;
    function PrepareColumnButtonDraft(Sender: TObject): Boolean;
    procedure SynchronizeColumnsButton;
    procedure SynchronizeRecovery;
    procedure SynchronizeClipboardFeedback;
    function ClipboardStatusHeight: Integer;
    function RequiredToolbarHeight(ARecoveryCount: Integer): Integer;
    procedure ApplyToolbarHeight(AHeight: Integer);
    function RecoveryPresentationReady: Boolean;
    function PrepareRecoveryButton(Sender: TObject): INyxGridColumnButtonCommand;
    procedure RetireColumnsButton;
    procedure PrepareFitButtonFocus(Sender: TObject);
    procedure FitButtonKeyDown(Sender: TObject; var AKey: Word; AShift: TShiftState);
    procedure FitButtonKeyUp(Sender: TObject; var AKey: Word; AShift: TShiftState);
    procedure SynchronizeFitButtons;
    procedure RetireFitButtons;
    function FocusedFitButton: TNyxCollectionColumnButton;
    function FitButtonWidth(AGrid: TStringGrid): Integer;
  private
    FColumnEditing: IColumnEditing;
    FCellRefreshPending, FCellRefreshQueued, FCellIdleArmed: Boolean;
    FStartPane, FEndPane: TColumnPane;
    { Negative entries are inert reservation spacers, never data columns. }
    FMainColumns: TNyxCollectionColumnOrder;
    FColumnPresentation: TNyxColumnPresentation;
    FArrangedSerial: Integer;
    FFocusedCanonical: Integer;
    FActiveGrid: TStringGrid;
    FColumnEdit: TColumnEditLease;
    { Dirty values without editor/focus authority survive hiding. Only exact
      row/typed-field/accepted-scalar matches can join a later explicit edit. }
    FDormantColumnEdits: TNyxCollectionCellEdits;
    FOpeningDormantEdit: Boolean;
    { Internal copied-draft restoration may create an editor before admission;
      physical input still uses the shared pending gate. Never expose this flag
      as user interaction policy or hold it beyond the guarded restore call. }
    FImportingCellEdit: Boolean;
    { Full-render physical preview is separate from the transient update guard.
      Model notifications revoke cached focus proof while physical Refresh,
      blur validation and deferred editor work wait for exact lease Resume. }
    FEditSuspension: INyxCollectionEditsSnapshot;
    { Borrowed physical coordinates qualify the copied logical focus proof.
      A reused editor, changed map or a new caret/draft never grants rollback
      permission. Suspension permits only its exact recorded grid fallback. }
    FCellFocusGrid: TStringGrid;
    FCellFocusInput: TWinControl;
    FCellFocusRow, FCellFocusColumn, FCellFocusCanonical: Integer;
    FCellFocusRevision: Integer;
    FPaneSync, FLayoutGesture, FPointerInside: Boolean;
    FArrangedRevision: Integer;
    FPreviousTopLeft, FPreviousEnter, FPreviousExit: TNotifyEvent;
    FPreviousClick, FPreviousDoubleClick: TNotifyEvent;
    FPreviousMouseUp: TMouseEvent;
    FPreviousMouseMove: TMouseMoveEvent;
    FPreviousMouseEnter, FPreviousMouseLeave: TNotifyEvent;
    FPreviousKeyUp: TKeyEvent;
    FPreviousHeaderSized: THdrEvent;
    FResizeSerial: Integer;
    FResizeRequest: TNativeColumnLayoutRequest;
    FResizeRequestLease: IInterface;
    FResizeCapture: INyxCollectionColumnsCapture;
    FResizeField: TNyxColumnIdentity;
    FResizeWidth: Integer;
    FResizePending: Boolean;
    FResizeReleased, FResizeQueued: Boolean;
    FResumeQueued: Boolean;
    { Native editor reveal can resize the grid before its Show tail finishes.
      Coalesce only geometry here: the current map remains valid until the
      existing idle owner reflows it. This flag never blocks draft promotion. }
    FColumnGeometryPending: Boolean;
    procedure RetireColumnResize(ARequest: TNativeColumnLayoutRequest);
    function ColumnResizeInterrupted(ARequest: TNativeColumnLayoutRequest): Boolean;
    function ColumnResizeCurrent(ARequest: TNativeColumnLayoutRequest;
      AFinal: Boolean): Boolean;
    { A managed return protects the raw request across displacement/destructor
      callouts. Both actions share one slot and the original capture/epoch. }
    function BeginColumnLayout(AGrid: TStringGrid; AColumn, APhysical: Integer;
      AAction: TNativeColumnLayoutAction;
      out ARequest: TNativeColumnLayoutRequest): IInterface;
    function CaptureColumnLayout(ARequest: TNativeColumnLayoutRequest): Boolean;
    procedure ColumnDoubleClick(Sender: TObject; AStamp: Integer);
    function ColumnFitSurface(AColumn: Integer): TNativeColumnFitSurface;
    procedure CellCompositionEnded(Sender: TObject);
    procedure RefreshAfterComposition(AData: PtrInt);
    procedure CellRefreshIdle(Sender: TObject; var ADone: Boolean);
    { One existing ordinary idle registration serves deferred cell refresh,
      count presentation and resize retirement. No nested pump or second queue. }
    procedure ArmCellRefreshIdle;
    function ColumnInputBusy: Boolean;
    function InputGrid(ASender: TObject): TStringGrid;
    function CanonicalColumn(AGrid: TStringGrid; APhysical: Integer): Integer;
    function GridForColumn(ACanonical: Integer; out APhysical: Integer): TStringGrid;
    function CanonicalText(AColumn, ARow: Integer): TNyxText;
    function CanonicalDisplay(AColumn, ARow: Integer;
      out APresentation: TNyxValuePresentation): TNyxText;
    function CanonicalEditable(AColumn, ARow: Integer): Boolean;
    function MainEditable(AColumn, ARow: Integer): Boolean;
    function ReadColumnEditor: TColumnEditLease;
    { Optional displayed destination preflights recovery BEFORE closing a
      physical editor. Capacity refusal must leave its exact text/caret visible. }
    procedure CaptureColumnEditor(const ARecoveryData: INyxCollectionSnapshot = nil);
    procedure RestoreColumnEditor;
    function ColumnValueDisplay(const AValue: TNyxStateValue): TNyxText;
    procedure RememberDormantColumnEdit(const ALease: TColumnEditLease);
    procedure RecoverColumnDrafts(const AData: INyxCollectionSnapshot);
    procedure PruneDormantColumnEdits;
    procedure ColumnEditorShown(Sender: TObject);
    procedure ResumeColumnEditor(AData: PtrInt);
    procedure ColumnValidationIdle(Sender: TObject);
    procedure RenderArrangedGrid(const AData: INyxCollectionSnapshot);
    procedure SynchronizePaneRows;
    procedure SynchronizeLogicalPaneRows;
    procedure SynchronizeViewportMetrics;
    procedure GridTopLeft(Sender: TObject);
    procedure GridHeaderSized(Sender: TObject; AIsColumn: Boolean; AIndex: Integer);
    procedure PublishColumnLayout(AData: PtrInt);
    procedure GridMouseUp(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
    procedure PaneMouseMove(Sender: TObject; AShift: TShiftState; AX, AY: Integer);
    procedure PaneMouseEnter(Sender: TObject);
    procedure PaneMouseLeave(Sender: TObject);
    procedure PaneKeyUp(Sender: TObject; var AKey: Word; AShift: TShiftState);
    procedure PaneEnter(Sender: TObject);
    procedure PaneExit(Sender: TObject);
    procedure PaneClick(Sender: TObject);
    procedure PaneDoubleClick(Sender: TObject);
    procedure ArrangedKeyDown(Sender: TObject; var AKey: Word; AShift: TShiftState);
    procedure ArrangedMouseDown(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
  private
    FTree: TTreeView;
    FNodes: array of TTreeNode;
    FRendered: INyxCollectionSnapshot;
    { Non-nil only during an admitted map build. Native readers use this
      prepared immutable snapshot, or the last fully rendered one while idle. }
    FColumnRendering: INyxCollectionSnapshot;
    { The native mount owns the view association. Cached row metadata retains
      no view backreference and is revoked with the borrowed tree receiver. }
    FTreeRows: INyxTreeRows;
    FPreviousSelection: TSelectionChangeEvent;
    FPreviousCell: TOnSelectCellEvent;
    FPreviousValidate: TValidateEntryEvent;
    FPreviousTreeChange: TTVChangedEvent;
    FPreviousTreeEdit: TTVEditedEvent;
    FPreviousTreeExpanded: TTVExpandedEvent;
    FPreviousTreeCollapsed: TTVExpandedEvent;
    { Host disclosure publishes after LCL has finished its node call stack.
      Only exact copied identities/values cross that queue turn. Queue callbacks
      borrow this mount; DetachTarget revokes them before the receiver dies. }
    FDisclosureItems: TNyxItemRefs;
    FDisclosureValues: array of Boolean;
    FDisclosureSerial: Integer;
    FPreviousOptions: TGridOptions;
    { Borrowed grids recover their original native wheel policy at disconnect.
      A live bound table scrolls its viewport without moving the active cell. }
    FPreviousMouseWheelOption: TMouseWheelOption;
    FPreviousReadOnly: Boolean;
    FPreviousKey: TKeyEvent;
    FPreviousUTF8: TUTF8KeyPressEvent;
    FRejectTextKey: Boolean;
    FConsumedTextKey: Boolean;
    FPreviousMouse: TMouseEvent;
    FPreviousHeaderClick: THdrEvent;
    FQueryPressActive: Boolean;
    FQueryPressColumn: Integer;
    FQueryPressFilter: Boolean;
    FQueryPressAppend: Boolean;
    FQueryPressRevision: Integer;
    FPreviousPrepare: TOnPrepareCanvasEvent;
    FPreviousTreeSelection: TNotifyEvent;
    FPreviousMultiSelect: Boolean;
    FPreviousExtendedSelect: Boolean;
    FPreviousMultiStyle: TMultiSelectStyle;
    FGridShift: TShiftState;
    { Native pixels are cached separately from portable column intentions.
      User overrides remain runtime presentation, never document mutations. }
    FColumnNatural: TNyxFlowSizes;
    FNaturalFontName: String;
    FNaturalFontHeight, FNaturalFontPPI: Integer;
    FNaturalFontStyle: TFontStyles;
    FColumnApplied: TNyxFlowSizes;
    FColumnOverrides: TNyxFlowSizes;
    FPreviousGridResize: TNotifyEvent;
    FColumnApplying: Boolean;
    FPreviousHeaderHeight: Integer;
    FQueryHeaderAdjusted: Boolean;
    { Captions reuse the body pass's target font measurement, never body density.
      Flags/scalars own no native object; legacy headers keep their old path. }
    FHeaderLinePixels: Integer;
    FCustomHeaderApplied: Boolean;
    FPreviousGridHint: String;
    FPreviousGridShowHint: Boolean;
    FPreviousCellHint: TGetCellHintEvent;
    { Copied adapter guidance is separate from the latest authored/restoration
      hint. Renderer synchronization must not erase resize/query help. }
    FGridHelp: TNyxText;
    function GridHint: String;
    procedure GridCellHint(Sender: TObject; AColumn, ARow: Integer;
      var AHint: String);
    procedure ApplyGridHint;
    function TreeRows: INyxTreeRows;
    procedure SizeGridColumns;
    procedure ReserveHeader;
    procedure GridResized(Sender: TObject);
    procedure GridProjectionChanged(Sender: TObject);
    procedure SizeLogicalGridColumns;
    procedure RenderLogicalDataset;
    procedure PrepareLogicalTextRows(const AData: INyxCollectionSnapshot);
    procedure ListSelected(Sender: TObject; User: Boolean);
    procedure GridSelected(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure GridEdited(Sender: TObject; ACol, ARow: Integer;
      const OldValue: String; var NewValue: String);
    procedure TreeSelected(Sender: TObject; ANode: TTreeNode);
    procedure TreeExpanded(Sender: TObject; ANode: TTreeNode);
    procedure TreeCollapsed(Sender: TObject; ANode: TTreeNode);
    procedure TreeDisclosure(ANode: TTreeNode; AExpanded: Boolean);
    procedure PublishDisclosure(AData: PtrInt);
    procedure TreeSelectionChanged(Sender: TObject);
    procedure KeyDown(Sender: TObject; var AKey: Word; AShift: TShiftState);
    function ClipboardKeyDown(Sender: TObject; var AKey: Word;
      AShift: TShiftState): Boolean;
    function CellRangeKeyDown(Sender: TObject; var AKey: Word;
      AShift: TShiftState): Boolean;
    procedure UTF8KeyPress(Sender: TObject; var AKey: TUTF8Char);
    procedure GridMouseDown(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
    procedure GridHeaderClicked(Sender: TObject; AIsColumn: Boolean; AIndex: Integer);
    procedure PrepareGrid(Sender: TObject; AColumn, ARow: Integer; AState: TGridDrawState);
    function VisibleOrder: TNyxItemRefs;
    procedure TreeEdited(Sender: TObject; ANode: TTreeNode; var AText: String);
    function NodeIndex(ANode: TTreeNode): Integer;
    function DisplayText(const AValue: TNyxText): TNyxText;
    { Pure UI-thread read, clipped during native geometry/teardown transitions.
      It borrows the view only while this mount owns the grid reader. }
    function GridText(AColumn, ARow: Integer): TNyxText;
    function GridDisplay(AColumn, ARow: Integer;
      out APresentation: TNyxValuePresentation): TNyxText;
    procedure QueryColumn(AColumn: Integer; AFilter, AAppend: Boolean);
    function EditText(const AItem: TNyxItemRef; AColumn: Integer;
      const AValue: TNyxText): TNyxText;
  protected
    procedure PrepareTablePresentation; override;
    function ReadRecoveryDraft(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity; out ACell: TNyxCollectionCellEdit): Boolean; override;
    function DiscardRecoveryDraft(const ACell: TNyxCollectionCellEdit): Boolean; override;
    procedure RenderCellRange; override;
    procedure RetireCellRangeStroke(const AStroke: INyxCollectionRangeStroke); override;
    function FocusCellRange(ARow, AColumn: Integer): Boolean; override;
    function CellRangeAvailable: Boolean; override;
    function ClipboardInputAvailable: Boolean; override;
    function BeginCellPresentation: INyxCollectionCellPresentation; override;
    function PrepareColumnFit(const ALaunch: INyxCollectionColumnsCapture;
      const AField: TNyxColumnIdentity;
      var AMeasurement: INyxColumnFitMeasurement): INyxPreparedPublication; override;
    function DeferTargetRefresh: Boolean; override;
    function DeferComposingRefresh: Boolean; override;
    procedure RefreshTargetIdle; override;
    procedure RecoveryMembershipChanged(const AObservation: TNyxDraftRecoveryObservation); override;
    procedure ClipboardFeedbackChanged; override;
    procedure RetireTargetInput; override;
    procedure RenderDataset; override;
    procedure DetachTarget; override;
  public
    procedure SetAuthoredHint(const AHint: TNyxText);
    function BeginRangeStroke(ASender: TObject; APhase: TNyxCollectionRangeStrokePhase;
      AShift: TShiftState; AX, AY: Integer;
      const AGeneration: INyxCancellationScope): INyxCollectionCellRangePointerEntry;
    constructor Create(AControl: TControl; const AView: INyxCollectionView;
      const ALimits: TNyxTableTextWorkLimits;
      AInputPhase: TNyxCollectionInputPhase = ncipReady;
      const AInputGate: INyxCollectionInputGate = nil;
      const AInputFrame: INyxNativeGestureFrame = nil;
      const AInputEvents: INyxEvents = nil;
      const AOriginID: TNyxText = '';
      AColumnClick: TNyxLCLColumnClick = nil;
      ACellContext: TNyxLCLCellContextDispatch = nil;
      ARangePointer: TNyxLCLRangePointerDispatch = nil;
      ACaptureHandler: TNyxLCLCaptureHandler = nil);
    function GetEditingPublication: INyxObservationPublication;
    function EditingDispatching: Boolean;
    function BeginCellContext(Sender: TObject; const APosition: TPoint):
      INyxCollectionClipboardCommand;
    function CaptureFocusedDraft: INyxCollectionFocusedDraftCapture;
    function SuspendCellEdits: INyxCollectionEditsSnapshot;
    procedure ResumeCellEdits(const ASnapshot: INyxCollectionEditsSnapshot;
      AFocus: Boolean);
    function RestoreCellEdits(const ASnapshot: INyxCollectionEditsSnapshot;
      AFocus: Boolean): Boolean;
    function CellEditFocusMatches(const AStore: INyxCollection;
      const ACell: TNyxCollectionCellEdit): Boolean;
  end;

{$include nyx.collections.lcl.selection.inc}

constructor TCollectionCaptureInput.Create(const ALifetime: INyxCollectionInputLifetime);
begin
  inherited Create;
  FLifetime := ALifetime;
end;

function TCollectionCaptureInput.GetCurrent: Boolean;
begin
  Result := (FLifetime <> nil) and FLifetime.Current;
end;

function TCollectionCaptureInput.GetReady: Boolean;
begin
  Result := (FLifetime <> nil) and FLifetime.Ready;
end;

function TCollectionCaptureInput.GetEpoch: Integer;
begin
  Result := -1;

  if FLifetime <> nil then
  begin
    Result := FLifetime.Epoch;
  end;
end;

function NyxLCLCollectionCaptureInput(
  const ALifetime: INyxCollectionInputLifetime): INyxLCLCaptureInput;
begin
  Result := TCollectionCaptureInput.Create(ALifetime);
end;

function TNativeMount.GridHint: String;
begin
  Result := FPreviousGridHint;

  if (Result <> '') and (FGridHelp <> '') then
  begin
    Result := Result + ' ';
  end;
  Result := Result + DisplayText(FGridHelp);
end;

procedure TNativeMount.GridCellHint(Sender: TObject; AColumn, ARow: Integer;
  var AHint: String);
var
  LPrevious: TGetCellHintEvent;
begin

  if not GetConnected then
  begin
    Exit;
  end;
  AHint := GridHint;
  LPrevious := FPreviousCellHint;
  { LCL temporarily rewrites Hint during hover and restores its entry-time
    copy on leave. Its ordinary cell-hint port must read our current owned
    metadata instead. The prior host callback may customize it; touch no mount
    or borrowed widget after that external call. }

  if Assigned(LPrevious) then
  begin
    LPrevious(Sender, AColumn, ARow, AHint);
  end;
end;

procedure TNativeMount.ApplyGridHint;

  { Only the mount's own two edge panes share this copied guidance. Other
    child controls retain their independent metadata and callbacks. }
  procedure ApplyPaneHint(APane: TColumnPane);
  begin

    if APane = nil then
    begin
      Exit;
    end;
    APane.Grid.Hint := FGrid.Hint;
    APane.Grid.ShowHint := FGrid.ShowHint;
    APane.Grid.Options := APane.Grid.Options + [goCellHints];
    APane.Grid.OnGetCellHint := GridCellHint;
  end;
begin

  if not GetConnected or (FGrid = nil) then
  begin
    Exit;
  end;
  FGrid.Hint := GridHint;
  FGrid.ShowHint := FPreviousGridShowHint or (FGridHelp <> '');
  FGrid.Options := FGrid.Options + [goCellHints];
  FGrid.OnGetCellHint := GridCellHint;
  ApplyPaneHint(FStartPane);
  ApplyPaneHint(FEndPane);
end;

procedure TNativeMount.SetAuthoredHint(const AHint: TNyxText);
var
  LControl: TControl;
begin

  if not GetConnected then
  begin
    Exit;
  end;

  if FGrid <> nil then
  begin
    FPreviousGridHint := DisplayText(AHint);
    FPreviousGridShowHint := AHint <> '';
    ApplyGridHint;
    Exit;
  end;
  LControl := FList;

  if LControl = nil then
  begin
    LControl := FTree;
  end;

  if LControl <> nil then
  begin
    LControl.Hint := DisplayText(AHint);
    LControl.ShowHint := AHint <> '';
  end;
end;

function TNativeMount.CaptureFocusedDraft: INyxCollectionFocusedDraftCapture;
var
  LLease: INyxCollectionMount;
  LCommands: INyxCollectionClipboardCommands;
  LEntry: INyxCollectionClipboardCellInput;
  LInspection: INyxCollectionClipboardInspectionInput;
  LCommand: INyxCollectionClipboardCellCommand;
  LPrepared: INyxCollectionClipboardPreparedScope;
  LReady: INyxCancellationReadiness;
  LFrame: INyxNativeGestureFrame;
  LInput: INyxCollectionInputLifetime;
  LGrid: TStringGrid;
  LEditor: TCustomEdit;
  LFocus: TControl;
  LData: INyxCollectionSnapshot;
  LView: INyxCollectionView;
  LSpec: TNyxCollectionViewSpec;
  LField: TNyxColumnIdentity;
  LItem: TNyxItemRef;
  LValue: INyxCollectionCellText;
  LSchema: TNyxCollectionSchema;
  LText: TNyxText;
  LSelection: TNyxTextSelection;
  LEpoch: Integer;
  LMutation: Integer;
  LRevision: Integer;
  LRow: Integer;
  LPhysical: Integer;
  LColumn: Integer;
  LCount: Integer;

  function Original: Boolean;
  var
    LParent: TControl;
    LDepth: Integer;
  begin
    Result := GetConnected and GetInputReady and not FUpdating and
      not FReadOnly and not FEditsSuspended and ClipboardInputAvailable and
      (FView = LView) and (FRendered = LData) and (FCellMutationSerial = LMutation) and
      (FArrangedRevision = LRevision) and (FColumnFrame = LFrame) and
      LInput.Ready and (LInput.Epoch = LEpoch);

    if not Result then
    begin
      Exit;
    end;
    Result := (Screen <> nil) and (Screen.ActiveControl = LFocus) and
      (InputGrid(LFocus) = LGrid) and LGrid.EditorMode and
      (LGrid.Editor = LEditor) and (LGrid.Row = LRow) and
      (LGrid.Col = LPhysical) and (CanonicalColumn(LGrid, LPhysical) = LColumn) and
      (not FHasColumnLayout or ((LColumn >= 0) and (LColumn < FColumnLayout.Count) and
        FColumnLayout.ColumnAt(LColumn).IsVisible));

    if not Result then
    begin
      Exit;
    end;
    { A toolbar/grid focus is not editor authority. Retain only the actual edit
      or a bounded descendant belonging to that same edit implementation. }
    LParent := LFocus;
    LDepth := 0;
    while (LParent <> nil) and (LParent <> LEditor) and (LDepth < 32) do
    begin
      LParent := LParent.Parent;
      Inc(LDepth);
    end;
    Result := (LParent = LEditor) or LEditor.Focused;
  end;

begin
  Result := nil;
  LLease := Self as INyxCollectionMount;
  LCommands := FClipboardCommands;
  LFrame := FColumnFrame;
  LInput := GetInputLifetime;

  if (LCommands = nil) or (LFrame = nil) or (Screen = nil) or
    not GetInputReady or FUpdating or FReadOnly or not ClipboardInputAvailable then
  begin
    Exit;
  end;
  LEpoch := LInput.Epoch;
  LMutation := FCellMutationSerial;
  LRevision := FArrangedRevision;
  LData := FRendered;
  LView := FView;
  LFrame.Enter;
  try

    if not GetInputReady or (FColumnFrame <> LFrame) then
    begin
      Exit;
    end;
    { Reserve readonly inspection before source/native getters. Proving the
      accepted counterpart of this draft grants no cell-copy authority, and
      therefore also works when the authored copy permission set is empty. }
    if Supports(LCommands, INyxCollectionClipboardInspectionInput, LInspection) then
    begin
      LCommand := LInspection.BeginCellInspection;
    end
    else
    begin

      if not Supports(LCommands, INyxCollectionClipboardCellInput, LEntry) then
      begin
        Exit;
      end;
      LCommand := LEntry.BeginCellInput;
    end;

    if LCommand = nil then
    begin
      Exit;
    end;
    try
      LFocus := Screen.ActiveControl;
      LGrid := InputGrid(LFocus);

      if (LGrid = nil) or not LGrid.EditorMode or not (LGrid.Editor is TCustomEdit) then
      begin
        Exit;
      end;
      LEditor := TCustomEdit(LGrid.Editor);
      LRow := LGrid.Row;
      LPhysical := LGrid.Col;
      LColumn := CanonicalColumn(LGrid, LPhysical);

      if not LCommand.Current or not Original or (LData = nil) or (LColumn < 0) then
      begin
        Exit;
      end;
      LSpec := LView.Spec;
      LCount := LData.Count;

      if not LCommand.Current or not Original or (LRow < 1) or
        (LRow > LCount) or (LColumn >= LSpec.Count) or
        (LSpec.ColumnAt(LColumn).Mode <> cmEditable) then
      begin
        Exit;
      end;
      LField := LSpec.ColumnAt(LColumn).Identity;
      LItem := LData.ItemAt(LRow - 1).Ref;

      if not LCommand.Current or not Original then
      begin
        Exit;
      end;
      LCommand.ResolveCell(LItem, LField);
      LCommand.Prepare;

      if not Supports(LCommand, INyxCollectionClipboardPreparedScope, LPrepared) or
        not LCommand.Current or not Original then
      begin
        Exit;
      end;
      LReady := LPrepared.Readiness;
      LSchema := LData.Schema;

      if not LCommand.Current or not Original then
      begin
        Exit;
      end;
      LText := NyxLCLInputText(LEditor);

      if not LCommand.Current or not Original then
      begin
        Exit;
      end;
      LSelection := CaptureNyxLCLSelection(LEditor);

      if not LCommand.Current or not Original then
      begin
        Exit;
      end;
      LValue := NewNyxCollectionCellDraft(LCommand.Payload, LText,
        NyxCollectionDraftDomain(LSchema, LField));

      if LCommand.Current and Original then
      begin
        Result := NewNyxCollectionFocusedDraftCapture(LValue, LEpoch, LSelection, LReady);
      end;

      if not LCommand.Current or not Original then
      begin
        Result := nil;
      end;
    finally
      LCommand.Retire;
    end;
  finally
    { Leave the exact renderer frame last, from locals. Native text/selection
      messages may retire the mount; this frame keeps its borrowed editor alive. }
    LFrame.Leave;
  end;
end;

constructor TNativeColumnButtonCommand.Create(AOwner: TNativeMount;
  ARequest: TNativeColumnLayoutRequest; const ARequestLease: IInterface);
begin
  inherited Create;
  FOwner := AOwner;
  FRequest := ARequest;
  FRequestLease := ARequestLease;
end;

destructor TNativeColumnButtonCommand.Destroy;
begin
  Retire;
  inherited Destroy;
end;

function TNativeColumnButtonCommand.LifetimeCurrent: Boolean;
begin
  Result := (FRequest <> nil) and (FRequest.Input <> nil) and
    FRequest.Input.Current;

  if Result then
  begin
    Result := (FOwner.FView = FRequest.View) and
      (FOwner.GetInputLifetime = FRequest.Input) and FRequest.Input.Current;
  end;
end;

function TNativeColumnButtonCommand.DispatchCurrent: Boolean;
begin
  Result := not FRetired and not FInvoked and LifetimeCurrent;

  if Result then
  begin
    { A held physical press cannot regain canonical Click authority after an
      explicit disable/re-enable or edit suspension/resume. Check its original
      epoch at dispatch ENTRY only: admitted callbacks retain ordinary lifetime
      continuity, while ColumnResizeCurrent separately guards the Fit default. }
    Result := (FRequest.Epoch > 0) and (FRequest.Epoch < High(Integer)) and
      FRequest.Input.Ready and (FRequest.Input.Epoch = FRequest.Epoch);
  end;
end;

procedure TNativeColumnButtonCommand.Invoke;
var
  LClick: TNyxLCLColumnClick;
  LLegacyClick: TNotifyEvent;
  LAccepted: Boolean;
  LKeepAlive: INyxCollectionMount;
begin

  if not DispatchCurrent then
  begin
    Exit;
  end;
  LKeepAlive := FOwner as INyxCollectionMount;
  FInvoked := True;
  LClick := FOwner.FColumnClick;
  LLegacyClick := FOwner.FPreviousClick;
  LAccepted := True;
  try

    if Assigned(LClick) then
    begin
      LAccepted := LClick(FRequest.Grid);
    end
    else if Assigned(LLegacyClick) then
    begin
      { Direct native hosts keep their own ordinary callback. Renderer hosts
        supply the explicit outcome port instead, never invoking both paths. }
      LLegacyClick(FRequest.Grid);
    end;

    if not LAccepted or not LifetimeCurrent or
      not FOwner.ColumnResizeCurrent(FRequest, False) or
      not FRequest.Allowed or (FRequest.Capture = nil) then
    begin
      Exit;
    end;
    FRequest.Pending := True;
    FOwner.FResizePending := True;
    FOwner.FResizeReleased := True;
    FTransferred := True;
    FOwner.ArmCellRefreshIdle;
  finally
    Retire;
  end;
end;

procedure TNativeColumnButtonCommand.Retire;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if not FTransferred then
  begin
    FRequest.Retired := True;

    if LifetimeCurrent then
    begin
      FOwner.RetireColumnResize(FRequest);
    end;
  end;
end;

constructor TNativeColumnManagerCommand.Create(AOwner: TNativeMount;
  ASerial: Integer; ARecovery: Boolean);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerLease := AOwner as INyxCollectionMount;
  FManager := AOwner.FColumnManager;
  FCells := AOwner.FCellActions;
  FRecovery := ARecovery;
  FView := AOwner.FView;
  FInput := AOwner.GetInputLifetime;
  FEpoch := FInput.Epoch;
  FSerial := ASerial;
  FMutation := AOwner.FCellMutationSerial;
  FRevision := AOwner.FArrangedRevision;
  if not ARecovery then
  begin
    FLayoutSerial := AOwner.FColumnLayout.Serial;
  end;
end;

function TNativeColumnManagerCommand.LifetimeCurrent: Boolean;
begin
  Result := not FRetired and (FInput <> nil) and FInput.Current and
    (FOwner.FView = FView) and (FOwner.GetInputLifetime = FInput) and
    (((not FRecovery) and (FOwner.FColumnManager = FManager) and (FManager <> nil)) or
     (FRecovery and (FOwner.FCellActions = FCells) and (FCells <> nil))) and FInput.Current;
end;

function TNativeColumnManagerCommand.DispatchCurrent: Boolean;
begin
  Result := not FInvoked and LifetimeCurrent and (FEpoch > 0) and
    (FEpoch < High(Integer)) and (FInput.Epoch = FEpoch) and FInput.Ready and
    (FSerial > 0) and (FSerial < High(Integer)) and
    (FOwner.FColumnsButtonSerial = FSerial);
end;

procedure TNativeColumnManagerCommand.Invoke;
var
  LClick: TNyxLCLColumnClick;
  LLegacyClick: TNotifyEvent;
  LAccepted: Boolean;
  LGrid: TStringGrid;
begin

  if not DispatchCurrent then
  begin
    Exit;
  end;
  FInvoked := True;
  LClick := FOwner.FColumnClick;
  LLegacyClick := FOwner.FPreviousClick;
  LGrid := FOwner.FGrid;
  LAccepted := True;
  try

    if Assigned(LClick) then
    begin
      LAccepted := LClick(LGrid);
    end
    else if Assigned(LLegacyClick) then
    begin
      LLegacyClick(LGrid);
    end;

    if not LAccepted or not LifetimeCurrent or
      (FOwner.FColumnsButtonSerial <> FSerial) or
      (FOwner.FCellMutationSerial <> FMutation) or
      (FOwner.FArrangedRevision <> FRevision) or
      (not FRecovery and (FOwner.FColumnLayout.Serial <> FLayoutSerial)) or
      FOwner.FUpdating or FOwner.ColumnInputBusy or
      (FInput.Epoch <> FEpoch) or not FInput.Ready then
    begin
      Exit;
    end;
    { Last outward operation. The retained exact manager owns its presentation;
      callback retirement cannot redirect this default into a successor table. }
    if FRecovery then
    begin
      FOwner.OpenRecoveredEdits;
    end
    else
    begin
      FManager.Open;
    end;
  finally
    Retire;
  end;
end;

procedure TNativeColumnManagerCommand.Retire;
begin
  FRetired := True;
end;

function TNativeMount.PrepareColumnsButton(Sender: TObject): INyxGridColumnButtonCommand;
var
  LKeepAlive: INyxCollectionMount;
begin
  Result := nil;
  LKeepAlive := Self as INyxCollectionMount;

  if not GetInputReady or FUpdating or ColumnInputBusy or
    (FColumnManager = nil) or (Sender <> FColumnsButton) or
    (FColumnsButton = nil) or not FColumnsButton.Visible or
    not FColumnsButton.Enabled or (FColumnsButtonSerial = High(Integer)) then
  begin
    Exit;
  end;

  if not PrepareColumnButtonDraft(Sender) then
  begin
    Exit;
  end;
  Inc(FColumnsButtonSerial);
  Result := TNativeColumnManagerCommand.Create(Self, FColumnsButtonSerial);
end;

function TNativeMount.PrepareColumnButtonDraft(Sender: TObject): Boolean;
var
  LInput: INyxCollectionInputLifetime;
  LView: INyxCollectionView;
  LGrid: TStringGrid;
  LEpoch: Integer;
  LMutation: Integer;
  LRevision: Integer;
  LLayoutSerial: Integer;
  LManagerSerial: Integer;
  LResizeSerial: Integer;
begin
  Result := False;

  if not GetInputReady or FUpdating or ColumnInputBusy then
  begin
    Exit;
  end;
  LInput := GetInputLifetime;
  LView := FView;
  LGrid := FGrid;
  LEpoch := LInput.Epoch;
  LMutation := FCellMutationSerial;
  LRevision := FArrangedRevision;
  LLayoutSerial := FColumnLayout.Serial;
  LManagerSerial := FColumnsButtonSerial;
  LResizeSerial := FResizeSerial;
  { OS pointer activation and programmatic Click need not call the Pascal
    SetFocus override. Park the editor BEFORE constructing their command too.
    This is still the original input: a nested intention, rebind, suspension,
    map change or model mutation during capture refuses it rather than gaining
    fresh authority from whatever target exists when capture returns. }
  PrepareFitButtonFocus(Sender);
  Result := LInput.Current and LInput.Ready and (LInput.Epoch = LEpoch) and
    (FView = LView) and (FGrid = LGrid) and
    (FCellMutationSerial = LMutation) and (FArrangedRevision = LRevision) and
    (FColumnLayout.Serial = LLayoutSerial) and
    (FColumnsButtonSerial = LManagerSerial) and (FResizeSerial = LResizeSerial) and
    not FUpdating and not ColumnInputBusy;
end;

procedure TNativeMount.RetireColumnsButton;
var
  LButton: TNyxCollectionColumnButton;
  LRecovery: TNyxCollectionColumnButton;
  LStatus: TMemo;
  LFrame: INyxNativeGestureFrame;
begin
  LButton := FColumnsButton;
  LRecovery := FRecoveryButton;
  LStatus := FClipboardStatus;
  FColumnsButton := nil;
  FRecoveryButton := nil;
  FClipboardStatus := nil;
  FClipboardStatusPending := False;
  LFrame := FColumnFrame;

  if (LButton <> nil) or (LRecovery <> nil) or (LStatus <> nil) then
  begin
    { Keep both component destructions deferred until all borrowed button/grid
      accesses finish, including any callback-bearing destruction tail. }
    LFrame.Enter;
    try

      if LStatus <> nil then
      begin
        LFrame.Retire(LStatus);
      end;

      if LRecovery <> nil then
      begin
        LRecovery.RetireColumnAction;
        TNyxCollectionStringGrid(FGrid).UnregisterColumnButton(LRecovery);
        LFrame.Retire(LRecovery);
      end;

      if LButton <> nil then
      begin
        LButton.RetireColumnAction;
        TNyxCollectionStringGrid(FGrid).UnregisterColumnButton(LButton);
        LFrame.Retire(LButton);
      end;
    finally
      LFrame.Leave;
    end;
  end;
end;

{$include nyx.collections.recovery.lcl.inc}
{$include nyx.collections.clipboard.feedback.lcl.inc}

procedure TNativeMount.SynchronizeColumnsButton;
var
  LKeepAlive: INyxCollectionMount;
  LInput: INyxCollectionInputLifetime;
  LManager: INyxColumnArrangementManager;
  LFrame: INyxNativeGestureFrame;
  LGrid: TStringGrid;
  LSurface: INyxGridInputSurface;
  LButton: TNyxCollectionColumnButton;
  LScale: Double;
  LWidth: Integer;
  LHeight: Integer;
  LLeft: Integer;

  function Current: Boolean;
  begin
    Result := LInput.Current and (FGrid = LGrid) and
      (FColumnManager = LManager) and (FColumnsButton = LButton);
  end;
begin
  LKeepAlive := Self as INyxCollectionMount;

  if not InputLifetimeCurrent or (FGrid = nil) or ColumnInputBusy then
  begin
    Exit;
  end;

  if FColumnManager = nil then
  begin

    if FColumnsButton <> nil then
    begin
      FColumnsButton.Visible := False;
    end;
    SynchronizeRecovery;
    Exit;
  end;
  LInput := GetInputLifetime;
  LGrid := FGrid;
  LManager := FColumnManager;
  LFrame := FColumnFrame;
  LFrame.Enter;
  try
    LButton := FColumnsButton;

    if LButton = nil then
    begin
      LSurface := TNyxCollectionStringGrid(FGrid).ColumnEditorInputSurface;

      if LSurface = nil then
      begin
        raise ENyxCollection.Create('Column launcher requires its exact native input surface');
      end;
      LButton := TNyxCollectionColumnButton.Create(FGrid);
      FColumnsButton := LButton;
      LButton.Visible := False;
      LButton.Name := 'NyxColumnManagerButton';
      LButton.ConfigureColumnAction(GetInputLifetime, LSurface.Frame,
        LSurface.Continuity, PrepareColumnsButton, PrepareFitButtonFocus);

      if not Current then
      begin
        Exit;
      end;
      LButton.OnKeyDown := FitButtonKeyDown;
      LButton.OnKeyUp := FitButtonKeyUp;
      TNyxCollectionStringGrid(FGrid).RegisterColumnButton(LButton);
    end;
    LScale := Max(1, FGrid.Font.PixelsPerInch) / 96;
    LButton.Parent := FGrid;

    if not Current then
    begin
      Exit;
    end;
    LButton.Caption := 'Columns';

    if not Current then
    begin
      Exit;
    end;
    LButton.AccessibleName := 'Manage table columns';
    LButton.Hint := 'Choose, reorder, show, hide, size and pin columns';
    LButton.ShowHint := True;
    LButton.Font.Assign(FGrid.TitleFont);

    if not Current then
    begin
      Exit;
    end;
    LButton.TabStop := True;
    LButton.Enabled := FEnabled;

    if not Current then
    begin
      Exit;
    end;
    LWidth := Min(Max(1, FGrid.ClientWidth - Round(8 * LScale)), Round(112 * LScale));
    LHeight := Max(1, FColumnToolbarHeight - ClipboardStatusHeight - Round(8 * LScale));
    LLeft := Round(4 * LScale);

    if FGrid.UseRightToLeftAlignment then
    begin
      LLeft := Max(0, FGrid.ClientWidth - LWidth - LLeft);
    end;
    LButton.SetBounds(LLeft, Round(4 * LScale), LWidth, LHeight);

    if not Current then
    begin
      Exit;
    end;
    LButton.Visible := True;

    if not Current then
    begin
      Exit;
    end;
    LButton.BringToFront;
    SynchronizeRecovery;
  finally
    { Exact frame retained before native setters; deferred component retirement
      and its destructor tails run only after every borrowed button use ends. }
    LFrame.Leave;
  end;
end;

function TNativeMount.PrepareFitButton(Sender: TObject): INyxGridColumnButtonCommand;
var
  LKeepAlive: INyxCollectionMount;
  LRequest: TNativeColumnLayoutRequest;
  LLease: IInterface;
  LColumn: Integer;
  LPhysical: Integer;
  LGrid: TStringGrid;
  LButton: TNyxCollectionFitButton;
begin
  Result := nil;
  LKeepAlive := Self as INyxCollectionMount;

  if not GetInputReady or FUpdating or ColumnInputBusy or
    not (Sender is TNyxCollectionFitButton) then
  begin
    Exit;
  end;
  LButton := TNyxCollectionFitButton(Sender);
  LColumn := FColumnLayout.Spec.ColumnIndex(LButton.Field);

  if (LColumn < 0) or (LColumn >= Length(FFitButtons)) or
    (FFitButtons[LColumn] <> LButton) or not LButton.Visible or
    not LButton.Enabled then
  begin
    Exit;
  end;

  if not PrepareColumnButtonDraft(Sender) then
  begin
    Exit;
  end;
  LGrid := GridForColumn(LColumn, LPhysical);

  if LGrid = nil then
  begin
    Exit;
  end;
  LLease := BeginColumnLayout(LGrid, LColumn, LPhysical, nclaFit, LRequest);

  if LRequest = nil then
  begin
    Exit;
  end;
  Result := TNativeColumnButtonCommand.Create(Self, LRequest, LLease);
  try

    if LRequest.Allowed and ColumnResizeCurrent(LRequest, False) then
    begin
      CaptureColumnLayout(LRequest);
    end;
  except
    Result.Retire;
    raise;
  end;
end;

procedure TNativeMount.PrepareFitButtonFocus(Sender: TObject);
var
  LKeepAlive: INyxCollectionMount;
  LOuter: TNyxCollectionStringGrid;
begin
  LKeepAlive := Self as INyxCollectionMount;

  if not GetInputReady or FUpdating or ColumnInputBusy then
  begin
    Exit;
  end;
  LOuter := TNyxCollectionStringGrid(FGrid);
  LOuter.BeginColumnTransition;
  try
    CaptureColumnEditor;

    if InputLifetimeCurrent and FColumnEdit.Active then
    begin
      { Intentional focus on a management/Fit button withdraws editing authority,
        not the draft. A scalar Click can synchronously refresh this same table
        and an older native idle wake can still run afterward. Neither may
        recreate EditorMode behind the popup: a later Col/Row setter would then
        validate/commit that background text. Only a fresh EditorShow consumes
        this detached typed draft and resumes normal native editing. }
      RememberDormantColumnEdit(FColumnEdit);
      FColumnEdit := Default(TColumnEditLease);
    end;
  finally
    LOuter.EndColumnTransition;
  end;
end;

procedure TNativeMount.FitButtonKeyDown(Sender: TObject;
  var AKey: Word; AShift: TShiftState);
var
  LKeepAlive: INyxCollectionMount;
  LReply: TKeyEvent;
begin
  LKeepAlive := Self as INyxCollectionMount;

  if not GetInputReady or FUpdating or ColumnInputBusy then
  begin
    AKey := 0;
    Exit;
  end;
  LReply := FPreviousKey;

  if Assigned(LReply) then
  begin
    LReply(FGrid, AKey, AShift);
  end;
end;

procedure TNativeMount.FitButtonKeyUp(Sender: TObject;
  var AKey: Word; AShift: TShiftState);
var
  LKeepAlive: INyxCollectionMount;
  LReply: TKeyEvent;
begin
  LKeepAlive := Self as INyxCollectionMount;

  if not GetInputReady or FUpdating or ColumnInputBusy then
  begin
    AKey := 0;
    Exit;
  end;
  LReply := FPreviousKeyUp;

  if Assigned(LReply) then
  begin
    LReply(FGrid, AKey, AShift);
  end;
end;

function TNativeMount.FocusedFitButton: TNyxCollectionColumnButton;
var
  LIndex: Integer;
begin
  Result := nil;

  if (FColumnsButton <> nil) and FColumnsButton.Focused then
  begin
    Exit(FColumnsButton);
  end;
  for LIndex := 0 to High(FFitButtons) do
  begin

    if (FFitButtons[LIndex] <> nil) and FFitButtons[LIndex].Focused then
    begin
      Exit(FFitButtons[LIndex]);
    end;
  end;
end;

function TNativeMount.FitButtonWidth(AGrid: TStringGrid): Integer;
begin
  Result := Max(28, Round(40 * Max(1, AGrid.Font.PixelsPerInch) / 96));
end;

procedure TNativeMount.RetireFitButtons;
var
  LButton: TNyxCollectionFitButton;
  LIndex: Integer;
  LFrame: INyxNativeGestureFrame;
begin
  LFrame := FColumnFrame;
  for LIndex := 0 to High(FFitButtons) do
  begin
    LButton := FFitButtons[LIndex];
    FFitButtons[LIndex] := nil;

    if LButton <> nil then
    begin
      LButton.RetireFitAction;
      TNyxCollectionStringGrid(FGrid).UnregisterColumnButton(LButton);
      LFrame.Retire(LButton);
    end;
  end;
  FFitButtons := nil;
end;

procedure TNativeMount.SynchronizeFitButtons;
var
  LIndex: Integer;
  LPhysical: Integer;
  LWidth: Integer;
  LHeight: Integer;
  LGrid: TStringGrid;
  LButton: TNyxCollectionFitButton;
  LEntry: TNyxColumnLayoutEntry;
  LBounds: TRect;
  LClip: TRect;
  LSurface: INyxGridInputSurface;
  LUpdating: Boolean;

  procedure ExcludePane(APane: TColumnPane);
  begin

    if (APane = nil) or not APane.Grid.Visible then
    begin
      Exit;
    end;

    if APane.Grid.Left <= 0 then
    begin
      LClip.Left := Max(LClip.Left, APane.Grid.Left + APane.Grid.Width);
    end
    else
    begin
      LClip.Right := Min(LClip.Right, APane.Grid.Left);
    end;
  end;

begin

  if not InputLifetimeCurrent or (FGrid = nil) or ColumnInputBusy then
  begin
    Exit;
  end;
  LUpdating := FUpdating;
  FUpdating := True;
  try
    { Schema changes never retarget an old button or its entered token. }
    for LIndex := 0 to High(FFitButtons) do
    begin
      LButton := FFitButtons[LIndex];

      if (LButton <> nil) and ((LIndex >= FColumnLayout.Count) or
        not LButton.Field.Same(FColumnLayout.ColumnAt(LIndex).Column.Identity)) then
      begin
        FFitButtons[LIndex] := nil;
        LButton.RetireFitAction;
        TNyxCollectionStringGrid(FGrid).UnregisterColumnButton(LButton);
        FColumnFrame.Retire(LButton);
      end;
    end;
    SetLength(FFitButtons, FColumnLayout.Count);
    LSurface := TNyxCollectionStringGrid(FGrid).ColumnEditorInputSurface;

    if LSurface = nil then
    begin
      raise ENyxCollection.Create('Native Fit buttons require their exact table input surface');
    end;
    for LIndex := 0 to FColumnLayout.Count - 1 do
    begin
      LEntry := FColumnLayout.ColumnAt(LIndex);
      LButton := FFitButtons[LIndex];

      if not LEntry.IsVisible or not (nclfAutoSize in LEntry.Features) or
        (nclfResize in LEntry.Features) then
      begin

        if LButton <> nil then
        begin
          LButton.Visible := False;
        end;
        Continue;
      end;
      LGrid := GridForColumn(LIndex, LPhysical);

      if LGrid = nil then
      begin
        Continue;
      end;

      if LButton = nil then
      begin
        LButton := TNyxCollectionFitButton.Create(FGrid);
        FFitButtons[LIndex] := LButton;
        LButton.Visible := False;
        LButton.ConfigureFitAction(LEntry.Column.Identity, GetInputLifetime,
          LSurface.Frame, LSurface.Continuity, PrepareFitButton, PrepareFitButtonFocus);
        LButton.OnKeyDown := FitButtonKeyDown;
        LButton.OnKeyUp := FitButtonKeyUp;
        TNyxCollectionStringGrid(FGrid).RegisterColumnButton(LButton);
      end;
      LButton.Parent := LGrid;
      LButton.Caption := 'Fit';
      LButton.AccessibleName := 'Fit ' + DisplayText(LEntry.Column.Title);
      LButton.Hint := LButton.AccessibleName;
      LButton.ShowHint := True;
      LButton.Font.Assign(LGrid.TitleFont);
      LButton.TabStop := True;
      LButton.Enabled := FEnabled;
      LBounds := TNyxCollectionStringGrid(LGrid).ColumnHeaderRect(LPhysical);
      LClip := LGrid.ClientRect;

      if LGrid = FGrid then
      begin
        ExcludePane(FStartPane);
        ExcludePane(FEndPane);
      end;
      LWidth := Min(FitButtonWidth(LGrid), LBounds.Right - LBounds.Left - 2);
      LHeight := Min(24, LBounds.Bottom - LBounds.Top - 2);

      if (LWidth < 20) or (LHeight < 12) or
        (LBounds.Left < LClip.Left) or (LBounds.Right > LClip.Right) then
      begin
        LButton.Visible := False;
        Continue;
      end;

      if LGrid.UseRightToLeftAlignment then
      begin
        LBounds.Right := LBounds.Left + LWidth + 1;
      end
      else
      begin
        LBounds.Left := LBounds.Right - LWidth - 1;
      end;
      LButton.SetBounds(LBounds.Left + 1, LBounds.Top + 1, LWidth, LHeight);
      LButton.Visible := True;
      LButton.BringToFront;
    end;
  finally
    FUpdating := LUpdating;
  end;
end;

constructor TNativeColumnLayoutRequest.Create(AOwner: TNativeMount;
  ATarget: TStringGrid; AColumn, APhysical, ASerial: Integer;
  AAction: TNativeColumnLayoutAction);
var
  LEntry: TNyxColumnLayoutEntry;
  LSizing: TNyxColumnSizing;
begin
  inherited Create;
  Input := AOwner.GetInputLifetime;

  if Input <> nil then
  begin
    Epoch := Input.Epoch;
  end;
  View := AOwner.FView;
  Grid := AOwner.FGrid;
  Target := ATarget;
  Column := AColumn;
  Physical := APhysical;
  Serial := ASerial;
  Action := AAction;
  Revision := AOwner.FArrangedRevision;
  LayoutSerial := AOwner.FColumnLayout.Serial;
  Mutation := AOwner.FCellMutationSerial;
  Editing := AOwner.FColumnEditing;

  if (Column >= 0) and (Column < AOwner.FColumnLayout.Count) then
  begin
    LEntry := AOwner.FColumnLayout.ColumnAt(Column);
    Field := LEntry.Column.Identity;
    Allowed := AOwner.FEnabled and LEntry.IsVisible and
      (((Action = nclaResize) and (nclfResize in LEntry.Features)) or
        ((Action = nclaFit) and (nclfAutoSize in LEntry.Features)));
    LSizing := LEntry.Column.Sizing;

    if not LSizing.Defined then
    begin
      LSizing := NyxColumnSizing;
    end;
    LSizing.Validate;
    Minimum := LSizing.Bounds.MinimumValue;
    Maximum := LSizing.Bounds.MaximumValue;
  end;

  ContentScale := 1;

  if Target <> nil then
  begin
    PPI := Target.Font.PixelsPerInch;

    if Target is TNyxCollectionStringGrid then
    begin
      ProjectionEpoch := TNyxCollectionStringGrid(Target).ProjectionEpoch;
      ContentScale := TNyxCollectionStringGrid(Target).ContentProjectionScale;
    end;
  end;

  if Grid is TNyxCollectionStringGrid then
  begin
    Activities[0] := TNyxCollectionStringGrid(Grid).ValidationActivityLease;
  end;

  if AOwner.FStartPane <> nil then
  begin
    Activities[1] := AOwner.FStartPane.Grid.ValidationActivityLease;
  end;

  if AOwner.FEndPane <> nil then
  begin
    Activities[2] := AOwner.FEndPane.Grid.ValidationActivityLease;
  end;
end;

function TNativeMount.BeginColumnLayout(AGrid: TStringGrid;
  AColumn, APhysical: Integer; AAction: TNativeColumnLayoutAction;
  out ARequest: TNativeColumnLayoutRequest): IInterface;
var
  LRequest: TNativeColumnLayoutRequest;
  LLease, LDisplaced: IInterface;
  LSerial: Integer;
begin
  Result := nil;
  ARequest := nil;

  if FResizeSerial = High(Integer) then
  begin
    raise ENyxCollection.Create('Native column command serial is exhausted');
  end;
  Inc(FResizeSerial);
  LSerial := FResizeSerial;
  LDisplaced := FResizeRequestLease;
  RetireColumnResize(FResizeRequest);
  LRequest := TNativeColumnLayoutRequest.Create(Self, AGrid,
    AColumn, APhysical, LSerial, AAction);
  LLease := LRequest;

  if FResizeSerial <> LSerial then
  begin
    LRequest.Retired := True;
    Exit;
  end;
  FResizeRequest := LRequest;
  FResizeRequestLease := LLease;
  FLayoutGesture := True;
  FResizePending := False;
  FResizeReleased := False;
  FResizeQueued := False;
  FResizeCapture := nil;
  ARequest := LRequest;
  Result := LLease;
end;

function TNativeMount.CaptureColumnLayout(
  ARequest: TNativeColumnLayoutRequest): Boolean;
var
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
begin
  Result := False;

  if not ColumnResizeCurrent(ARequest, False) or not ARequest.Allowed then
  begin
    Exit;
  end;
  LColumns := NyxCollectionColumns(ARequest.View);

  if not ColumnResizeCurrent(ARequest, False) then
  begin
    Exit;
  end;
  LCapture := LColumns.CaptureColumns;

  if not ColumnResizeCurrent(ARequest, False) then
  begin
    Exit;
  end;
  ARequest.Columns := LColumns;
  ARequest.Capture := LCapture;
  FResizeCapture := LCapture;
  FResizeField := ARequest.Field;
  Result := LCapture <> nil;
end;

function TNativeMount.ColumnResizeCurrent(ARequest: TNativeColumnLayoutRequest;
  AFinal: Boolean): Boolean;
var
  LIndex: Integer;
begin
  Result := (ARequest <> nil) and not ARequest.Retired and not ARequest.Installed and
    (FResizeRequest = ARequest) and (FResizeSerial = ARequest.Serial) and
    (ARequest.Serial > 0) and (ARequest.Serial < High(Integer)) and
    (FView = ARequest.View) and (FView <> nil) and
    (FGrid = ARequest.Grid) and (FGrid <> nil) and
    FHasColumnLayout and not FUpdating and not FColumnApplying and
    not FPaneSync and not FEditsSuspended and FEnabled and
    (FColumnEditing = ARequest.Editing) and (ARequest.Editing <> nil) and
    not FCellRefreshPending and (FCellMutationSerial = ARequest.Mutation) and
    (ARequest.Mutation < High(Integer)) and
    (FArrangedRevision = ARequest.Revision) and (ARequest.Revision < High(Integer)) and
    (FColumnLayout.Serial = ARequest.LayoutSerial);

  if Result and ARequest.Allowed then
  begin
    Result := (ARequest.Column >= 0) and (ARequest.Column < FColumnLayout.Count);

    if Result then
    begin
      Result := FColumnLayout.ColumnAt(ARequest.Column).IsVisible and
        FColumnLayout.ColumnAt(ARequest.Column).Column.Identity.Same(ARequest.Field) and
        (((ARequest.Action = nclaResize) and
          (nclfResize in FColumnLayout.ColumnAt(ARequest.Column).Features)) or
        ((ARequest.Action = nclaFit) and
          (nclfAutoSize in FColumnLayout.ColumnAt(ARequest.Column).Features)));
    end;
  end;

  if Result and AFinal then
  begin
    Result := ARequest.Allowed and (ARequest.Capture <> nil) and
      (ARequest.Columns <> nil) and ARequest.Pending and ARequest.Publishing and
      (ARequest.Activities[0] <> nil);
    for LIndex := Low(ARequest.Activities) to High(ARequest.Activities) do
    begin

      if ARequest.Activities[LIndex] <> nil then
      begin
        Result := Result and not ARequest.Activities[LIndex].GetActive;
      end;
    end;
  end;

  if Result then
  begin
    { LAST: only cached composition and the ORIGINAL canonical input epoch. }
    Result := not ARequest.Editing.Composing and (ARequest.Input <> nil) and
      (ARequest.Epoch > 0) and (ARequest.Epoch < High(Integer)) and
      ARequest.Input.Ready and (ARequest.Input.Epoch = ARequest.Epoch);
  end;
end;

procedure TNativeMount.RetireColumnResize(ARequest: TNativeColumnLayoutRequest);
begin

  if ARequest = nil then
  begin
    Exit;
  end;
  { Flags first. Keep all managed owners and LCL sizing timing until its native
    tail has unwound; capture release does not prove the widget stopped sizing. }
  ARequest.Retired := True;
  ARequest.Pending := False;

  if FResizeRequest = ARequest then
  begin
    FResizePending := False;
    FQueryPressActive := False;
  end;
end;

function TNativeMount.ColumnResizeInterrupted(
  ARequest: TNativeColumnLayoutRequest): Boolean;
begin
  Result := False;

  if ARequest = nil then
  begin
    Exit;
  end;
  { Temporary whole-refresh shielding deliberately leaves Epoch unchanged.
    Ready=False alone must NOT terminally cancel a physical request. }
  Result := ARequest.Retired or (FResizeSerial <> ARequest.Serial) or
    (FView <> ARequest.View) or
    (FGrid <> ARequest.Grid) or not FEnabled or FEditsSuspended or
    (FColumnEditing <> ARequest.Editing) or (ARequest.Editing = nil) or
    (ARequest.Input = nil) or (ARequest.Epoch <= 0) or
    (ARequest.Epoch = High(Integer));

  if not Result then
  begin
    Result := not ARequest.Input.Current or
      (ARequest.Input.Epoch <> ARequest.Epoch) or ARequest.Editing.Composing;
  end;

  if not Result and not (ARequest.Publishing and ARequest.Installed) then
  begin
    { Its own committed notification may advance the physical map. That is
      committed cleanup, never permission to replay or undo the width. }
    Result := (FCellMutationSerial <> ARequest.Mutation) or
      (FArrangedRevision <> ARequest.Revision) or
      (FColumnLayout.Serial <> ARequest.LayoutSerial);
  end;
end;

constructor TNativeColumnLayoutFence.Create(AOwner: TNativeMount;
  ARequest: TNativeColumnLayoutRequest; const ARequestLease: IInterface);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerLease := AOwner as INyxCollectionMount;
  FRequest := ARequest;
  FRequestLease := ARequestLease;
end;

destructor TNativeColumnLayoutFence.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TNativeColumnLayoutFence.Validate;
begin

  if FStarted or FRetired then
  begin
    raise ENyxCollection.Create('Native column resize validation is single-use');
  end;
  FStarted := True;

  if not FOwner.ColumnResizeCurrent(FRequest, True) or
    not FRequest.Editing.GetReady then
  begin
    raise ENyxCollection.Create('Native column resize observation is no longer ready');
  end;
  { GetReady can enter the real router. Only cached facts follow its return. }
  FValidated := True;
  CheckReady;
end;

procedure TNativeColumnLayoutFence.CheckReady;
begin

  if FRetired or not FValidated or not FOwner.ColumnResizeCurrent(FRequest, True) then
  begin
    raise ENyxCollection.Create('Native column resize was superseded or its input retired');
  end;
end;

procedure TNativeColumnLayoutFence.Install;
begin
  FRequest.Installed := True;
end;

procedure TNativeColumnLayoutFence.Notify;
begin
  { The existing columns plan owns its one committed layout notification. }
end;

procedure TNativeColumnLayoutFence.Retire;
begin
  FRetired := True;
end;

constructor TNativeColumnFitFence.Create(AOwner: TNativeMount;
  const ALaunch: INyxCollectionColumnsCapture; const AField: TNyxColumnIdentity);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerLease := AOwner as INyxCollectionMount;
  FView := AOwner.FView;
  FLaunch := ALaunch;
  FField := AField;
  FInput := AOwner.GetInputLifetime;

  if FInput <> nil then
  begin
    FEpoch := FInput.Epoch;
  end;
  FGrid := AOwner.FGrid;
  FEditing := AOwner.FColumnEditing;
  FMutation := AOwner.FCellMutationSerial;
  FRevision := AOwner.FArrangedRevision;
  FLayoutSerial := AOwner.FColumnLayout.Serial;
  FCommandSerial := AOwner.FResizeSerial;
  FColumn := -1;

  if AOwner.FHasColumnLayout then
  begin
    FColumn := AOwner.FColumnLayout.Spec.ColumnIndex(FField);
  end;

  if FGrid is TNyxCollectionStringGrid then
  begin
    FActivities[0] := TNyxCollectionStringGrid(FGrid).ValidationActivityLease;
  end;

  if AOwner.FStartPane <> nil then
  begin
    FActivities[1] := AOwner.FStartPane.Grid.ValidationActivityLease;
  end;

  if AOwner.FEndPane <> nil then
  begin
    FActivities[2] := AOwner.FEndPane.Grid.ValidationActivityLease;
  end;
  RequireCurrent;
end;

destructor TNativeColumnFitFence.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TNativeColumnFitFence.RequireCurrent;
var
  LIndex: Integer;
begin

  if FRetired or FInstalled or (FLaunch = nil) or (FView = nil) or
    not FOwner.FHasColumnLayout or (FOwner.FView <> FView) or
    (FGrid = nil) or (FOwner.FGrid <> FGrid) or FOwner.FUpdating or
    FOwner.FEditsSuspended or not FOwner.FEnabled or FOwner.FColumnApplying or
    FOwner.FPaneSync or FOwner.FLayoutGesture or FOwner.FResizePending or
    (FOwner.FCellMutationSerial <> FMutation) or (FMutation = High(Integer)) or
    (FOwner.FArrangedRevision <> FRevision) or (FRevision = High(Integer)) or
    (FOwner.FColumnLayout.Serial <> FLayoutSerial) or
    (FOwner.FResizeSerial <> FCommandSerial) or (FCommandSerial = High(Integer)) or
    (FEditing = nil) or (FOwner.FColumnEditing <> FEditing) or
    FOwner.FCellRefreshPending or (FActivities[0] = nil) or
    (FColumn < 0) or (FColumn >= FOwner.FColumnLayout.Count) then
  begin
    raise ENyxCollection.Create('Column fit native target or input was replaced');
  end;

  if not FOwner.FColumnLayout.ColumnAt(FColumn).Column.Identity.Same(FField) or
    not (nclfAutoSize in FOwner.FColumnLayout.ColumnAt(FColumn).Features) then
  begin
    raise ENyxCollection.Create('Column fit field or permission changed');
  end;
  { Retained canonical activity flags own no widget and cannot call a router.
    No raw grid getter belongs in final prepared readiness. }
  for LIndex := Low(FActivities) to High(FActivities) do
  begin

    if (FActivities[LIndex] <> nil) and FActivities[LIndex].GetActive then
    begin
      raise ENyxCollection.Create('Column fit waits for native input and validation to finish');
    end;
  end;
  { LAST: cached composition, then the original canonical Ready/Epoch pair. }

  if FEditing.Composing or (FInput = nil) or (FEpoch <= 0) or
    (FEpoch = High(Integer)) or not FInput.Ready or (FInput.Epoch <> FEpoch) then
  begin
    raise ENyxCollection.Create('Column fit native input is no longer admitted');
  end;
end;

procedure TNativeColumnFitFence.Validate;
var
  LSurface: TNativeColumnFitSurface;
begin

  if FValidationStarted or FRetired then
  begin
    raise ENyxCollection.Create('Column fit validation is single-use');
  end;
  FValidationStarted := True;
  RequireCurrent;
  { This readiness getter may consult the actual event router. It must finish
    BEFORE borrowing current font/style pointers and the last cached checks. }

  if not FEditing.GetReady then
  begin
    raise ENyxCollection.Create('Column fit native observation is not ready');
  end;
  RequireCurrent;

  if FMetric = nil then
  begin
    raise ENyxCollection.Create('Column fit requires its original measured candidate');
  end;
  LSurface := FOwner.ColumnFitSurface(FColumn);

  if not FMetric.Matches(LSurface.HeaderFont, LSurface.BodyFont, LSurface.HeaderText,
    LSurface.HeaderStyle, LSurface.BodyStyle, LSurface.PPI,
    LSurface.HeaderInset, LSurface.BodyInset) then
  begin
    raise ENyxCollection.Create('Column fit native font or drawing facts changed');
  end;
  RequireCurrent;
  FValidated := True;
  CheckReady;
end;

procedure TNativeColumnFitFence.CheckReady;
begin

  if not FValidated then
  begin
    raise ENyxCollection.Create('Column fit is not validated');
  end;
  RequireCurrent;
end;

procedure TNativeColumnFitFence.Install;
begin
  { Flag only, before ordinary view notifications. Committed notification
    failure must not be interpreted as permission to retry or roll back. }
  FInstalled := True;
end;

procedure TNativeColumnFitFence.Notify;
begin
  { The existing model owns its one layout notification and draft restoration. }
end;

procedure TNativeColumnFitFence.Retire;
begin
  FRetired := True;
end;

constructor TNativeColumnFitFonts.Create(AGrid: TNyxCollectionStringGrid);
var
  LHeights: TNyxLCLLogicalFontHeights;
begin
  inherited Create;
  LHeights := AGrid.LogicalFontHeights;
  Header := TFont.Create;
  Body := TFont.Create;
  Header.PixelsPerInch := AGrid.TitleFont.PixelsPerInch;
  Header.Assign(AGrid.TitleFont);
  Body.PixelsPerInch := AGrid.Font.PixelsPerInch;
  Body.Assign(AGrid.Font);
  Header.Height := LHeights.TitleHeight;
  Body.Height := LHeights.BodyHeight;
end;

destructor TNativeColumnFitFonts.Destroy;
begin
  Header.Free;
  Body.Free;
  inherited Destroy;
end;

function TNativeMount.ColumnFitSurface(AColumn: Integer): TNativeColumnFitSurface;
var
  LGrid: TStringGrid;
  LPhysical: Integer;
  LCurrent, LExpected: TOnPrepareCanvasEvent;
  LAlignment: TNyxColumnAlignmentPolicy;
  LFonts: TNativeColumnFitFonts;

  procedure ResolveStyle(var AStyle: TTextStyle);
  begin
    AStyle := LGrid.DefaultTextStyle;
    AStyle.SingleLine := True;
    AStyle.ShowPrefix := False;
    AStyle.RightToLeft := LGrid.UseRightToLeftReading;

    if LGrid.UseRightToLeftAlignment then
    begin
      case AStyle.Alignment of
        taLeftJustify: AStyle.Alignment := taRightJustify;
        taRightJustify: AStyle.Alignment := taLeftJustify;
        taCenter:
          begin
            { Centered text is unchanged by logical alignment mirroring. }
          end;
      end;
    end;
  end;

  function TextInsets(const AStyle: TTextStyle): Integer;
  begin
    { Mirrors LCL DrawCellText. varCellPadding is ALREADY physically scaled;
      right alignment has the extra trailing pixel, center only one padding. }
    case AStyle.Alignment of
      taLeftJustify: Result := 2 * varCellPadding;
      taRightJustify: Result := varCellPadding + 1;
      taCenter: Result := varCellPadding;
    end;
  end;

begin
  Result := Default(TNativeColumnFitSurface);
  LGrid := GridForColumn(AColumn, LPhysical);

  if LGrid = nil then
  begin
    { Hidden fields have no physical column. Under this explicit uniform
      default-grid contract the outer table supplies their retained typography;
      do not reveal a field or create an edge pane merely to measure it. }
    LGrid := FGrid;
  end;

  if (LGrid = nil) or (LGrid.ClassType <> TNyxCollectionStringGrid) or
    (LGrid.Columns.Count <> 0) or not LGrid.DefaultDrawing or
    Assigned(LGrid.OnDrawCell) or Assigned(FPreviousPrepare) then
  begin
    raise ENyxCollection.Create('Custom native column drawing needs its own resolved fit metric');
  end;
  LCurrent := LGrid.OnPrepareCanvas;
  LExpected := PrepareGrid;

  if (TMethod(LCurrent).Code <> TMethod(LExpected).Code) or
    (TMethod(LCurrent).Data <> TMethod(LExpected).Data) then
  begin
    raise ENyxCollection.Create('Native column drawing no longer belongs to its mount');
  end;
  LFonts := TNativeColumnFitFonts.Create(TNyxCollectionStringGrid(LGrid));
  Result.Fonts := LFonts;
  Result.HeaderFont := LFonts.Header;
  Result.BodyFont := LFonts.Body;
  Result.HeaderText := CanonicalText(AColumn, 0);
  ResolveStyle(Result.HeaderStyle);
  ResolveStyle(Result.BodyStyle);

  if FHasTablePresentation and (AColumn >= 0) and
    (AColumn < FTablePresentation.Count) then
  begin
    LAlignment := FTablePresentation.AlignmentAt(AColumn);

    if LAlignment.HasHorizontal then
    begin
      case ResolveNyxColumnHorizontalAlignment(LAlignment.HorizontalAlignment,
        LGrid.UseRightToLeftAlignment) of
        ncpaLeft: Result.BodyStyle.Alignment := taLeftJustify;
        ncpaCenter: Result.BodyStyle.Alignment := taCenter;
        ncpaRight: Result.BodyStyle.Alignment := taRightJustify;
      end;
    end;

    if LAlignment.HasVertical then
    begin
      case LAlignment.VerticalAlignment of
        ncvaTop: Result.BodyStyle.Layout := tlTop;
        ncvaMiddle: Result.BodyStyle.Layout := tlCenter;
        ncvaBottom: Result.BodyStyle.Layout := tlBottom;
      end;
    end;
  end;

  if FQueries <> nil then
  begin
    Result.HeaderStyle.SingleLine := False;
    Result.HeaderStyle.WordBreak := True;
  end;
  Result.PPI := LGrid.Font.PixelsPerInch;

  if Result.PPI <= 0 then
  begin
    Result.PPI := Screen.PixelsPerInch;
  end;
  Result.HeaderInset := TextInsets(Result.HeaderStyle);

  if (AColumn >= 0) and (AColumn < FColumnLayout.Count) and
    (nclfAutoSize in FColumnLayout.ColumnAt(AColumn).Features) and
    not (nclfResize in FColumnLayout.ColumnAt(AColumn).Features) then
  begin
    Inc(Result.HeaderInset, FitButtonWidth(LGrid) + 2);
  end;
  Result.BodyInset := TextInsets(Result.BodyStyle);

  if FHasTablePresentation and FTablePresentation.TextAt(AColumn).Defined then
  begin
    { The measured multiline drawing path uses symmetric content insets. }
    Result.BodyInset := 2 * varCellPadding;
  end;
end;

function TNativeMount.PrepareColumnFit(const ALaunch: INyxCollectionColumnsCapture;
  const AField: TNyxColumnIdentity;
  var AMeasurement: INyxColumnFitMeasurement): INyxPreparedPublication;
var
  LFenceObject: TNativeColumnFitFence;
  LFence, LModel: INyxPreparedPublication;
  LSampling: INyxCollectionColumnSampling;
  LSample: INyxCollectionColumnSample;
  LSurface: TNativeColumnFitSurface;
  LTextPolicy: TNyxColumnTextPolicy;
begin
  LFenceObject := TNativeColumnFitFence.Create(Self, ALaunch, AField);
  LFence := LFenceObject;
  { Retain the mount BEFORE releasing a caller's former managed output value. }
  AMeasurement := nil;
  Result := nil;
  LFenceObject.RequireCurrent;

  if not LFenceObject.FEditing.GetReady then
  begin
    raise ENyxCollection.Create('Column fit native observation is not ready');
  end;
  LFenceObject.RequireCurrent;

  if not Supports(LFenceObject.FView, INyxCollectionColumnSampling, LSampling) then
  begin
    raise ENyxCollection.Create('Column fit requires the current-page sampling facet');
  end;
  LFenceObject.RequireCurrent;
  LSample := LSampling.CaptureColumnSample(LFenceObject.FLaunch, LFenceObject.FField);
  LFenceObject.RequireCurrent;

  if LSample = nil then
  begin
    raise ENyxCollection.Create('Column fit sampling returned no accepted sample');
  end;
  LSurface := ColumnFitSurface(LFenceObject.FColumn);
  LTextPolicy := Default(TNyxColumnTextPolicy);

  if FHasTablePresentation then
  begin
    LTextPolicy := FTablePresentation.TextAt(LFenceObject.FColumn);
  end;
  LFenceObject.FMetric := MeasureNyxLCLColumnFitWithTextPolicy(LSample, LSurface.HeaderFont,
    LSurface.BodyFont, LSurface.HeaderText, LSurface.HeaderStyle, LSurface.BodyStyle,
    LSurface.PPI, LSurface.HeaderInset, LSurface.BodyInset, LTextPolicy, FTextLimits);
  LFenceObject.RequireCurrent;
  LModel := LSampling.PrepareSampleAutoSize(LSample, LFenceObject.FMetric.Measurement.Width);
  LFenceObject.RequireCurrent;
  Result := PrepareNyxGroup([LModel, LFence]);
  LFenceObject.RequireCurrent;
  AMeasurement := LFenceObject.FMetric.Measurement;
end;

constructor TCellEditing.Create(AGrid: TNyxCollectionStringGrid;
  const ALifetime: INyxCollectionInputLifetime; const AEvents: INyxEvents;
  const AOriginID: TNyxText; AWake: TNotifyEvent);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  inherited Create;
  FLifetime := ALifetime;
  FWake := AWake;
  FSurface := AGrid.ColumnEditorInputSurface;

  if (FSurface = nil) or (AEvents = nil) then
  begin
    raise ENyxCollection.Create('Cell composition requires its owned input and real event router');
  end;
  LSurface := FSurface;
  FContinuity := LSurface.Continuity;

  if FContinuity = nil then
  begin
    raise ENyxCollection.Create('Cell composition requires the logical table continuity lease');
  end;
  LFrame := LSurface.Frame;
  { Observer activation reads the real router/scheduler and native caret.
    Hold the SAME physical frame through hook preparation AND rollback so a
    reentrant disconnect cannot adopt this borrowed editor into a newer chain. }
  LFrame.Enter;
  try
    try
      FObserver := NewNyxLCLEditingObserver;
      FObserver.Add(AOriginID, AGrid.EditorByStyle(cbsAuto));
      FConnected := True;
      FObserver.Activate(AEvents, Editing);

      if not Supports(FObserver, INyxObservationPublication, FPublication) or
        not FPublication.Ready or LSurface.Retired or ALifetime.Retired then
      begin
        raise ENyxCollection.Create('Cell composition observation cannot publish its prepared revision');
      end;
      LSurface.CoverWindowProc;
      FOuterAttached := True;
    except
      Disconnect;
      raise;
    end;
  finally
    LFrame.Leave;
  end;
end;

destructor TCellEditing.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TCellEditing.Ready: Boolean;
begin
  Result := FConnected and (FPublication <> nil) and FPublication.Ready;
end;

function TCellEditing.Composing: Boolean;
begin
  Result := FConnected and (FSurface <> nil) and FSurface.Composing;
end;

function TCellEditing.Dispatching: Boolean;
begin
  Result := (FObserver <> nil) and FObserver.Dispatching;
end;

procedure TCellEditing.PublishRevision(ARevision: Integer);
begin
  FPublication.PublishRevision(ARevision);
end;

procedure TCellEditing.Revoke;
begin
  FConnected := False;
  FWake := nil;

  if FSurface <> nil then
  begin
    FSurface.EndComposition;
  end;
end;

procedure TCellEditing.Disconnect;
begin
  Revoke;
  try

    if FOuterAttached and (FSurface <> nil) then
    begin
      FOuterAttached := False;
      FSurface.UncoverWindowProc;
    end;
  finally
    try

      if FObserver <> nil then
      begin
        FObserver.Disconnect;
      end;
    finally
      FObserver := nil;
      FPublication := nil;
      FSurface := nil;
    end;
  end;
end;

procedure TCellEditing.Editing(const AOriginID: TNyxText;
  const AEditing: TNyxEditingSnapshot);
var
  LKeepAlive: ICellEditing;
  LWake: TNotifyEvent;
  LWasComposing: Boolean;
begin
  LKeepAlive := Self;

  if not FConnected or not FLifetime.Current then
  begin
    Exit;
  end;
  case AEditing.Phase of
    nepCompositionStart, nepCompositionUpdate:
      begin
        FSurface.BeginComposition;
      end;
    nepCompositionEnd:
      begin
        { The existing observer emits END only from idle, after the OS's final
          committed characters. Its raw END message never clears this flag. }
        LWasComposing := FSurface.Composing;
        FSurface.EndComposition;
        LWake := FWake;

        if LWasComposing and Assigned(LWake) then
        begin
          LWake(nil);
        end;
      end;
  else
    begin
      { Ordinary text/selection phases do not alter the physical IME lease. }
    end;
  end;
end;

constructor TColumnEditing.Create(const ALifetime: INyxCollectionInputLifetime;
  const AEvents: INyxEvents; const AOriginID: TNyxText; AWake: TNotifyEvent;
  ACaptureHandler: TNyxLCLCaptureHandler; const AFrame: INyxNativeGestureFrame);
begin
  inherited Create;
  FLifetime := ALifetime;
  FEvents := AEvents;
  FOriginID := AOriginID;
  FWake := AWake;
  FCaptureHandler := ACaptureHandler;
  FFrame := AFrame;
  FCaptureInput := NyxLCLCollectionCaptureInput(ALifetime);
  FContinuity := NewNyxGridInputContinuity;
  FRevision := AEvents.ViewRevision;
end;

destructor TColumnEditing.Destroy;
begin
  try
    Disconnect;
  except
    { Explicit disconnect reports failure; final release must free all fields. }
  end;
  inherited Destroy;
end;

function TColumnEditing.Continuity: INyxGridInputContinuity;
begin
  Result := FContinuity;
end;

procedure TColumnEditing.CaptureChanged(const AOriginID: TNyxText; ATrigger: TNyxTrigger);
var
  LKeepAlive: IColumnEditing;
  LHandler: TNyxLCLCaptureHandler;
begin
  LKeepAlive := Self;

  if FRevoked or not FLifetime.Ready or not Assigned(FCaptureHandler) then
  begin
    Exit;
  end;
  LHandler := FCaptureHandler;
  LHandler(AOriginID, ATrigger);
end;

procedure TColumnEditing.Add(AGrid: TNyxCollectionStringGrid; AObserveCapture: Boolean);
var
  LKeepAlive: IColumnEditing;
  LCell: ICellEditing;
  LEvents: INyxEvents;
  LLifetime: INyxCollectionInputLifetime;
  LOriginID: TNyxText;
  LWake: TNotifyEvent;
  LCapture: INyxLCLCaptureObserver;
  LScoped: INyxLCLScopedCaptureObserver;
  LPublication: INyxObservationPublication;
begin
  LKeepAlive := Self;

  if FRevoked or (FCount = Length(FCells)) then
  begin
    raise ENyxCollection.Create('Native table observation is retired or has all three editors');
  end;
  { Copy context before the first outward scheduler/router read. The mount may
    retire during activation; this entered aggregate still owns its own frame. }
  LEvents := FEvents;
  LLifetime := FLifetime;
  LOriginID := FOriginID;
  LWake := FWake;
  LCell := TCellEditing.Create(AGrid, LLifetime, LEvents, LOriginID, LWake);
  try

    if AObserveCapture and Assigned(FCaptureHandler) then
    begin
      LCapture := NewNyxLCLCaptureObserver(FFrame);

      if not Supports(LCapture, INyxLCLScopedCaptureObserver, LScoped) or
        not Supports(LCapture, INyxObservationPublication, LPublication) then
      begin
        raise ENyxCollection.Create('Native pane capture requires scoped publication');
      end;
      LScoped.AddScoped(LOriginID, AGrid, FCaptureInput);
      LCapture.Activate(LEvents, CaptureChanged);
    end;

    if FRevoked or FLifetime.Retired or (FCount = Length(FCells)) then
    begin
      raise ENyxCollection.Create('Native table observation retired while preparing its editor');
    end;
    { Lazy panes use the aggregate's ACTUAL admitted revision while still hidden.
      Allocation/activation occurs here, never in pure renderer adoption. }
    LCell.PublishRevision(FRevision);

    if LPublication <> nil then
    begin
      LPublication.PublishRevision(FRevision);
    end;
    FCells[FCount] := LCell;
    FCaptures[FCount] := LCapture;
    FCapturePublications[FCount] := LPublication;
    Inc(FCount);
  except
    try

      if LCapture <> nil then
      begin
        LCapture.Disconnect;
      end;
    except
      { Preserve the original failed pane admission. }
    end;
    try
      LCell.Disconnect;
    except
      { Independent inner cleanup cannot replace the admission diagnostic. }
    end;
    raise;
  end;
end;

function TColumnEditing.GetReady: Boolean;
var
  LKeepAlive: IColumnEditing;
  LEvents: INyxEvents;
  LIndex, LRevision: Integer;
begin
  LKeepAlive := Self;
  Result := False;
  LEvents := FEvents;

  if LEvents = nil then
  begin
    Exit;
  end;
  LRevision := LEvents.ViewRevision;
  { External getter first; known local lifetime/ownership flags LAST. A getter
    may retire this aggregate, empty its slots or begin cell composition. }

  if FRevoked or (FCount = 0) or not FLifetime.Current or
    (FEvents <> LEvents) or (LRevision <> FRevision) then
  begin
    Exit;
  end;
  for LIndex := 0 to FCount - 1 do
  begin

    if not FCells[LIndex].Ready then
    begin
      Exit;
    end;

    if (FCapturePublications[LIndex] <> nil) and not FCapturePublications[LIndex].Ready then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

procedure TColumnEditing.PublishRevision(ARevision: Integer);
var
  LIndex: Integer;
begin
  FRevision := ARevision;
  for LIndex := 0 to FCount - 1 do
  begin
    FCells[LIndex].PublishRevision(ARevision);

    if FCapturePublications[LIndex] <> nil then
    begin
      FCapturePublications[LIndex].PublishRevision(ARevision);
    end;
  end;
end;

function TColumnEditing.Composing: Boolean;
begin
  Result := (FContinuity <> nil) and FContinuity.Composing;
end;

function TColumnEditing.Dispatching: Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to FCount - 1 do
  begin
    Result := Result or FCells[LIndex].Dispatching;
  end;
end;

procedure TColumnEditing.Revoke;
var
  LIndex: Integer;
begin
  FRevoked := True;
  FWake := nil;
  FCaptureHandler := nil;

  if FContinuity <> nil then
  begin
    FContinuity.Retire;
  end;
  for LIndex := 0 to FCount - 1 do
  begin
    FCells[LIndex].Revoke;
  end;
end;

procedure TColumnEditing.Disconnect;
var
  LIndex: Integer;
  LError: String;
begin
  Revoke;
  LError := '';
  for LIndex := 0 to FCount - 1 do
  begin
    try

      if FCaptures[LIndex] <> nil then
      begin
        FCaptures[LIndex].Disconnect;
      end;
    except
      on E: Exception do
      begin

        if LError = '' then
        begin
          LError := E.Message;
        end;
      end;
    end;
    FCapturePublications[LIndex] := nil;
    FCaptures[LIndex] := nil;
    try
      FCells[LIndex].Disconnect;
    except
      on E: Exception do
      begin

        if LError = '' then
        begin
          LError := E.Message;
        end;
      end;
    end;
    FCells[LIndex] := nil;
  end;
  FCount := 0;
  FEvents := nil;

  if LError <> '' then
  begin
    raise ENyxCollection.Create(LError);
  end;
end;

constructor TColumnPane.Create(AOwner: TNativeMount);
begin
  inherited Create;
  Owner := AOwner;
  Frame := AOwner.FColumnFrame;
  InputLifetime := AOwner.GetInputLifetime;
  Grid := TNyxCollectionStringGrid.Create(nil);
  Grid.Visible := False;
  Grid.AttachColumnInput(InputLifetime, Frame, AOwner.FColumnEditing.Continuity);
  Grid.ConfigureCellContext(InputLifetime, AOwner.BeginCellContext, AOwner.FCellContextDispatch);
  Grid.ConfigureRangePointer(InputLifetime, AOwner.PrepareRangePointer,
    AOwner.FRangePointerDispatch, AOwner.CancelRangeSurface);
  Grid.Parent := AOwner.FGrid;
  Grid.BorderStyle := bsNone;
  Grid.ScrollBars := ssNone;
  Grid.MouseWheelOption := AOwner.FGrid.MouseWheelOption;
  Grid.TabStop := False;
  Grid.FixedCols := 0;
  Grid.ColCount := 1;
  Grid.RowCount := 2;
  Grid.FixedRows := 1;
  Grid.AttachReader(Text);

  if AOwner.FHasTablePresentation then
  begin
    Grid.ObserveTextLayout(TextLayout);
    Grid.ObserveCellAlignment(Alignment);
  end;

  if AOwner.FView.Spec.HasColumnFormat then
  begin
    Grid.AttachDisplayReader(Display);
  end;
  Grid.AttachEditReader(Editable);

  if AOwner.FCellRangeEnabled then
  begin
    Grid.ObserveCellRange(RangeSelected);
  end;
  Grid.ObserveValidationIdle(AOwner.ColumnValidationIdle);
  Grid.ObserveColumnDoubleClick(AOwner.ColumnDoubleClick);
  Grid.ObserveColumnEditor(AOwner.ColumnEditorShown);
  TNyxCollectionStringGrid(AOwner.FGrid).RegisterColumnPane(Grid);
  Grid.OnSelectCell := AOwner.GridSelected;
  Grid.OnValidateEntry := AOwner.GridEdited;
  Grid.OnKeyDown := AOwner.KeyDown;
  Grid.OnKeyUp := AOwner.PaneKeyUp;
  Grid.OnMouseDown := AOwner.GridMouseDown;
  Grid.OnMouseUp := AOwner.GridMouseUp;
  Grid.OnMouseMove := AOwner.PaneMouseMove;
  Grid.OnMouseEnter := AOwner.PaneMouseEnter;
  Grid.OnMouseLeave := AOwner.PaneMouseLeave;
  Grid.OnEnter := AOwner.PaneEnter;
  Grid.OnExit := AOwner.PaneExit;
  Grid.OnClick := AOwner.PaneClick;
  Grid.OnDblClick := AOwner.PaneDoubleClick;
  Grid.OnHeaderClick := AOwner.GridHeaderClicked;
  Grid.OnHeaderSized := AOwner.GridHeaderSized;
  Grid.OnPrepareCanvas := AOwner.PrepareGrid;
  Grid.OnTopLeftChanged := AOwner.GridTopLeft;
  { Join observation only after the whole hidden pane is constructed. A failed
    earlier property/handle setup must not leave an aggregate borrowing a freed
    editor. Add is the final operation before exposing the completed pane. }
  AOwner.FColumnEditing.Add(Grid, True);
end;

procedure TColumnPane.Revoke;
begin
  Owner := nil;
  Columns := nil;

  if Grid = nil then
  begin
    Exit;
  end;
  Grid.RetireColumnInput(InputLifetime);
  Grid.DetachReader;
  Grid.DetachEditReader;
  Grid.OnSelectCell := nil;
  Grid.OnValidateEntry := nil;
  Grid.OnKeyDown := nil;
  Grid.OnKeyUp := nil;
  Grid.OnMouseDown := nil;
  Grid.OnMouseUp := nil;
  Grid.OnMouseMove := nil;
  Grid.OnMouseEnter := nil;
  Grid.OnMouseLeave := nil;
  Grid.OnEnter := nil;
  Grid.OnExit := nil;
  Grid.OnClick := nil;
  Grid.OnDblClick := nil;
  Grid.OnHeaderClick := nil;
  Grid.OnHeaderSized := nil;
  Grid.OnPrepareCanvas := nil;
  Grid.OnTopLeftChanged := nil;

  if Grid.Parent is TNyxCollectionStringGrid then
  begin
    TNyxCollectionStringGrid(Grid.Parent).UnregisterColumnPane(Grid);
  end;
end;

destructor TColumnPane.Destroy;
var
  LGrid: TNyxCollectionStringGrid;
begin
  Revoke;
  LGrid := Grid;
  Grid := nil;

  if (LGrid <> nil) and (Frame <> nil) and Frame.Dispatching then
  begin
    { An entered key/editor/default tail can still borrow this pane. Transfer
      it out of its old parent before that panel is destroyed; no weak mount
      receiver, native guard or reader remains on the queued component. }
    Frame.Retire(LGrid);
  end
  else
  begin
    LGrid.Free;
  end;
  Frame := nil;
  InputLifetime := nil;
  inherited Destroy;
end;

function TColumnPane.Text(AColumn, ARow: Integer): TNyxText;
begin
  Result := '';

  if (Owner <> nil) and (AColumn >= 0) and (AColumn < Length(Columns)) then
  begin
    Result := Owner.CanonicalText(Columns[AColumn], ARow);
  end;
end;

function TColumnPane.Display(AColumn, ARow: Integer;
  out APresentation: TNyxValuePresentation): TNyxText;
begin
  Result := '';
  APresentation := Default(TNyxValuePresentation);

  if (Owner <> nil) and (AColumn >= 0) and (AColumn < Length(Columns)) then
  begin
    Result := Owner.CanonicalDisplay(Columns[AColumn], ARow, APresentation);
  end;
end;

function TColumnPane.TextLayout(AColumn, ARow: Integer;
  out ALayout: TNyxTableTextLayout): Boolean;
begin
  Result := False;

  if (Owner <> nil) and (AColumn >= 0) and (AColumn < Length(Columns)) then
  begin
    Result := Owner.CanonicalTextLayout(Columns[AColumn], ARow, ALayout);
  end;
end;

function TColumnPane.Alignment(AColumn, ARow: Integer;
  out APolicy: TNyxColumnAlignmentPolicy): Boolean;
begin
  APolicy := NyxColumnAlignment;
  Result := (Owner <> nil) and (AColumn >= 0) and (AColumn < Length(Columns));

  if Result then
  begin
    Result := Owner.CanonicalAlignment(Columns[AColumn], ARow, APolicy);
  end;
end;

function TColumnPane.Editable(AColumn, ARow: Integer): Boolean;
begin
  Result := False;

  if (Owner <> nil) and (AColumn >= 0) and (AColumn < Length(Columns)) then
  begin
    Result := Owner.CanonicalEditable(Columns[AColumn], ARow);
  end;
end;

function TColumnPane.RangeSelected(AColumn, ARow: Integer): Boolean;
begin
  Result := False;

  if (Owner <> nil) and (AColumn >= 0) and (AColumn < Length(Columns)) then
  begin
    Result := Owner.CanonicalRangeSelected(Columns[AColumn], ARow);
  end;
end;

function TNativeMount.InputGrid(ASender: TObject): TStringGrid;
var
  LControl: TControl;
begin
  Result := nil;

  if not GetConnected or (ASender = nil) then
  begin
    Exit;
  end;

  if (FStartPane <> nil) and (ASender = FStartPane.Grid) then
  begin
    Exit(FStartPane.Grid);
  end;

  if (FEndPane <> nil) and (ASender = FEndPane.Grid) then
  begin
    Exit(FEndPane.Grid);
  end;

  if ASender = FGrid then
  begin
    Exit(FGrid);
  end;

  if ASender is TControl then
  begin
    LControl := TControl(ASender);
    { Query the specific panes first: the outer grid also recognizes them as
      part of its logical control. Names/Tags never participate in identity. }

    if (FStartPane <> nil) and FStartPane.Grid.OwnsColumnInput(LControl) then
    begin
      Exit(FStartPane.Grid);
    end;

    if (FEndPane <> nil) and FEndPane.Grid.OwnsColumnInput(LControl) then
    begin
      Exit(FEndPane.Grid);
    end;

    if (FGrid is TNyxCollectionStringGrid) and
      TNyxCollectionStringGrid(FGrid).OwnsColumnInput(LControl) then
    begin
      Result := FGrid;
    end;
  end;
end;

function TNativeMount.CanonicalColumn(AGrid: TStringGrid; APhysical: Integer): Integer;
var
  LMap: TNyxCollectionColumnOrder;
begin
  Result := -1;

  if APhysical < 0 then
  begin
    Exit;
  end;

  if not FHasColumnLayout then
  begin

    if AGrid = FGrid then
    begin
      Result := APhysical;
    end;
    Exit;
  end;
  LMap := nil;

  if AGrid = FGrid then
  begin
    LMap := FMainColumns;
  end
  else if (FStartPane <> nil) and (AGrid = FStartPane.Grid) then
  begin
    LMap := FStartPane.Columns;
  end
  else if (FEndPane <> nil) and (AGrid = FEndPane.Grid) then
  begin
    LMap := FEndPane.Columns;
  end;

  if APhysical < Length(LMap) then
  begin
    Result := LMap[APhysical];
  end;
end;

function TNativeMount.GridForColumn(ACanonical: Integer;
  out APhysical: Integer): TStringGrid;
var
  LIndex: Integer;
begin
  Result := nil;
  APhysical := -1;

  if ACanonical < 0 then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FMainColumns) do
  begin

    if FMainColumns[LIndex] = ACanonical then
    begin
      APhysical := LIndex;
      Exit(FGrid);
    end;
  end;

  if FStartPane <> nil then
  begin
    for LIndex := 0 to High(FStartPane.Columns) do
    begin

      if FStartPane.Columns[LIndex] = ACanonical then
      begin
        APhysical := LIndex;
        Exit(FStartPane.Grid);
      end;
    end;
  end;

  if FEndPane <> nil then
  begin
    for LIndex := 0 to High(FEndPane.Columns) do
    begin

      if FEndPane.Columns[LIndex] = ACanonical then
      begin
        APhysical := LIndex;
        Exit(FEndPane.Grid);
      end;
    end;
  end;
end;

function TNativeMount.CanonicalText(AColumn, ARow: Integer): TNyxText;
var
  LData: INyxCollectionSnapshot;
  LColumn: TNyxCollectionColumn;
  LValue: TNyxStateValue;
begin
  Result := '';

  if (AColumn < 0) or (AColumn >= FColumnLayout.Count) then
  begin
    Exit;
  end;
  LColumn := FColumnLayout.ColumnAt(AColumn).Column;

  if ARow = 0 then
  begin
    Result := LColumn.Title;

    if AColumn < Length(FQueryHeaders) then
    begin
      Result := FQueryHeaders[AColumn];
    end;

    if (FQueries <> nil) and
      ((FColumnLayout.Spec.TableQueryPolicy.FilterEnabled and LColumn.QueryPolicy.FilterEnabled) or
        (FColumnLayout.Spec.TableQueryPolicy.SortEnabled and LColumn.QueryPolicy.SortEnabled)) then
    begin
      Result := Result + TNyxText('  ▾');
    end;
    { Header display and Fit share the native NUL-safe conversion, just like
      body cells. A legal title must not paint only its prefix on Win32. }
    Result := DisplayText(Result);
    Exit;
  end;
  LData := FColumnRendering;

  if LData = nil then
  begin
    LData := FRendered;
  end;

  if (LData <> nil) and (ARow > 0) and (ARow <= LData.Count) then
  begin
    { The physical row belongs to this exact rendered record, including while
      the model/query has advanced during IME. CellText reads the CURRENT store
      and therefore cannot supply a retained native row's accepted display. }
    LValue := LColumn.Read(LData.ItemAt(ARow - 1));
    case LValue.Kind of
      nskText: Result := LValue.TextValue;
      nskBoolean:
        begin
          Result := 'false';

          if LValue.BooleanValue then
          begin
            Result := 'true';
          end;
        end;
      nskInteger: Result := IntToStr(LValue.IntegerValue);
      nskNumber: Result := LValue.NumberText;
    end;
    Result := DisplayText(Result);
  end;
end;

function TNativeMount.CanonicalDisplay(AColumn, ARow: Integer;
  out APresentation: TNyxValuePresentation): TNyxText;
var
  LData: INyxCollectionSnapshot;
begin
  Result := CanonicalText(AColumn, ARow);
  APresentation := NyxCanonicalPresentation(TNyxStateValue.FromText(Result));

  if (ARow <= 0) or (AColumn < 0) or (AColumn >= FColumnLayout.Count) then
  begin
    Exit;
  end;
  LData := FColumnRendering;

  if LData = nil then
  begin
    LData := FRendered;
  end;

  if (LData <> nil) and (ARow <= LData.Count) then
  begin
    APresentation := FColumnLayout.ColumnAt(AColumn).Column
      .Present(LData.ItemAt(ARow - 1), LData.Schema);
    Result := DisplayText(APresentation.Text);
  end;
end;

function TNativeMount.CanonicalEditable(AColumn, ARow: Integer): Boolean;
var
  LData: INyxCollectionSnapshot;
begin
  LData := FColumnRendering;

  if LData = nil then
  begin
    LData := FRendered;
  end;
  Result := InputLifetimeCurrent and not FEditsSuspended and
    (GetInputReady or FUpdating) and
    (not ColumnInputBusy or (FUpdating and FImportingCellEdit)) and
    FEnabled and not FReadOnly and (LData <> nil) and
    (AColumn >= 0) and (AColumn < FColumnLayout.Count) and
    (ARow > 0) and (ARow <= LData.Count);

  if Result then
  begin
    Result := ColumnVisible(AColumn) and
      (FColumnLayout.ColumnAt(AColumn).Column.Mode = cmEditable);
  end;
end;

function TNativeMount.MainEditable(AColumn, ARow: Integer): Boolean;
begin
  Result := CanonicalEditable(CanonicalColumn(FGrid, AColumn), ARow);
end;

function TNativeMount.ColumnValueDisplay(const AValue: TNyxStateValue): TNyxText;
begin
  case AValue.Kind of
    nskText: Result := AValue.TextValue;
    nskBoolean:
      begin
        Result := 'false';

        if AValue.BooleanValue then
        begin
          Result := 'true';
        end;
      end;
    nskInteger: Result := IntToStr(AValue.IntegerValue);
    nskNumber: Result := AValue.NumberText;
  end;
  Result := DisplayText(Result);
end;

procedure TNativeMount.RememberDormantColumnEdit(const ALease: TColumnEditLease);
var
  LIndex: Integer;
  LPosition: Integer;
  LMove: Integer;
  LDirty: Boolean;
begin
  LPosition := -1;
  for LIndex := 0 to High(FDormantColumnEdits) do
  begin

    if (FDormantColumnEdits[LIndex].Item.Collection.Name = ALease.Item.Collection.Name) and
      (FDormantColumnEdits[LIndex].Item.ID = ALease.Item.ID) and
      FDormantColumnEdits[LIndex].Field.Same(ALease.Field) then
    begin
      LPosition := LIndex;
      Break;
    end;
  end;
  LDirty := ALease.Active and
    (TNyxText(ALease.Text) <> ColumnValueDisplay(ALease.Accepted));

  if not LDirty then
  begin

    if LPosition >= 0 then
    begin
      for LMove := LPosition to High(FDormantColumnEdits) - 1 do
      begin
        FDormantColumnEdits[LMove] := FDormantColumnEdits[LMove + 1];
      end;
      SetLength(FDormantColumnEdits, Length(FDormantColumnEdits) - 1);
    end;
    Exit;
  end;

  if LPosition < 0 then
  begin
    LPosition := Length(FDormantColumnEdits);
    SetLength(FDormantColumnEdits, LPosition + 1);
  end;
  FDormantColumnEdits[LPosition] := NyxCollectionCellEdit(ALease.Item,
    ALease.Field, ALease.Accepted, TNyxText(ALease.Text), ALease.Selection,
    False, False);
end;

procedure TNativeMount.RecoverColumnDrafts(const AData: INyxCollectionSnapshot);
var
  LBefore: INyxCollectionSnapshot;
  LFormer: TNyxCollectionCellEdits;
  LNext: TNyxCollectionCellEdits;
  LCell: TNyxCollectionCellEdit;
  LReason: TNyxCollectionDraftLoss;
  LIndex: Integer;
  LCount: Integer;
  LDirty: Boolean;
begin
  LBefore := FRendered;
  LFormer := FDormantColumnEdits;
  SetLength(LNext, Length(LFormer));
  LCount := 0;
  for LIndex := 0 to High(LFormer) do
  begin

    if not RecoverCell(LFormer[LIndex], LBefore, AData, LReason) then
    begin
      LNext[LCount] := LFormer[LIndex].Copy;
      Inc(LCount);
    end;

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;
  end;
  SetLength(LNext, LCount);
  FDormantColumnEdits := LNext;

  if FColumnEdit.Active then
  begin
    LCell := NyxCollectionCellEdit(FColumnEdit.Item, FColumnEdit.Field,
      FColumnEdit.Accepted, TNyxText(FColumnEdit.Text), FColumnEdit.Selection, False, False);
    LDirty := TNyxText(FColumnEdit.Text) <> ColumnValueDisplay(FColumnEdit.Accepted);

    if RecoverCell(LCell, LBefore, AData, LReason, LDirty) and InputLifetimeCurrent then
    begin
      { A saved value is inert recovery data. Never let its old edit/focus lease
        attach to a replaced ID or overwrite a changed accepted scalar. }
      FColumnEdit := Default(TColumnEditLease);
    end;
  end;
end;

procedure TNativeMount.PruneDormantColumnEdits;
var
  LView: INyxCollectionView;
  LData: INyxCollectionSnapshot;
  LCurrent: INyxCollectionSnapshot;
  LSpec: TNyxCollectionViewSpec;
  LFormer: TNyxCollectionCellEdits;
  LNext: TNyxCollectionCellEdits;
  LCell: TNyxCollectionCellEdit;
  LIndex: Integer;
  LColumn: Integer;
  LCount: Integer;
  LRevision: Integer;
  LMutation: Integer;
begin

  if (Length(FDormantColumnEdits) = 0) or not InputLifetimeCurrent then
  begin
    Exit;
  end;
  LView := FView;
  LData := FRendered;
  LSpec := FColumnLayout.Spec;
  LRevision := FArrangedRevision;
  LMutation := FCellMutationSerial;

  if LMutation = High(Integer) then
  begin
    Exit;
  end;
  LFormer := FDormantColumnEdits;
  SetLength(LNext, Length(LFormer));
  LCount := 0;
  for LIndex := 0 to High(LFormer) do
  begin
    LCell := LFormer[LIndex];
    LColumn := LSpec.ColumnIndex(LCell.Field);

    if (LColumn >= 0) and (LData <> nil) and LData.Has(LCell.Item) and
      (LSpec.ColumnAt(LColumn).Mode = cmEditable) and
      LSpec.ColumnAt(LColumn).Read(LData.Item(LCell.Item)).SameValue(LCell.Accepted) then
    begin
      LNext[LCount] := LCell.Copy;
      Inc(LCount);
    end;
  end;
  LCurrent := LView.Snapshot;

  if not InputLifetimeCurrent or (FView <> LView) or (FRendered <> LData) or
    (LCurrent <> LData) or (FArrangedRevision <> LRevision) or
    (FCellMutationSerial <> LMutation) then
  begin
    Exit;
  end;
  SetLength(LNext, LCount);
  FDormantColumnEdits := LNext;
end;

procedure TNativeMount.ColumnEditorShown(Sender: TObject);
var
  LKeepAlive: INyxCollectionMount;
  LGrid: TStringGrid;
  LData: INyxCollectionSnapshot;
  LCurrent: INyxCollectionSnapshot;
  LView: INyxCollectionView;
  LColumn: Integer;
  LIndex: Integer;
  LMove: Integer;
  LRevision: Integer;
  LMutation: Integer;
  LItem: TNyxItemRef;
  LCell: TNyxCollectionCellEdit;
  LLease: TColumnEditLease;
  LOpening: Boolean;
begin
  LKeepAlive := Self as INyxCollectionMount;

  if not GetInputReady or FUpdating or FEditsSuspended or ColumnInputBusy or
    (FCellMutationSerial = High(Integer)) or
    (Length(FDormantColumnEdits) = 0) then
  begin
    Exit;
  end;
  LGrid := InputGrid(Sender);
  LData := FRendered;

  if (LGrid = nil) or (LData = nil) or not LGrid.EditorMode or
    (LGrid.Row < 1) or (LGrid.Row > LData.Count) then
  begin
    Exit;
  end;
  LView := FView;
  LRevision := FArrangedRevision;
  LMutation := FCellMutationSerial;
  LColumn := CanonicalColumn(LGrid, LGrid.Col);

  if not CanonicalEditable(LColumn, LGrid.Row) then
  begin
    Exit;
  end;
  LItem := LData.ItemAt(LGrid.Row - 1).Ref;
  for LIndex := 0 to High(FDormantColumnEdits) do
  begin
    LCell := FDormantColumnEdits[LIndex];

    if (LCell.Item.Collection.Name <> LItem.Collection.Name) or
      (LCell.Item.ID <> LItem.ID) or
      not LCell.Field.Same(FColumnLayout.ColumnAt(LColumn).Column.Identity) then
    begin
      Continue;
    end;
    LCurrent := LView.Snapshot;

    if not GetInputReady or FUpdating or FEditsSuspended or ColumnInputBusy or
      (FView <> LView) or (FRendered <> LData) or (LCurrent <> LData) or
      (FArrangedRevision <> LRevision) or (FCellMutationSerial <> LMutation) or
      (InputGrid(Sender) <> LGrid) or not LGrid.EditorMode or
      (CanonicalColumn(LGrid, LGrid.Col) <> LColumn) then
    begin
      Exit;
    end;
    { A NEW explicit editing entry consumes only this exact dormant value.
      Pure reveal/refresh never invokes this path while FUpdating is true. }
    LLease := Default(TColumnEditLease);
    LLease.Active := True;
    LLease.Item := LCell.Item;
    LLease.Field := LCell.Field;
    LLease.Accepted := LCell.Accepted;
    LLease.Text := String(LCell.Text);
    LLease.Selection := LCell.Selection;
    LLease.HadFocus := False;
    for LMove := LIndex to High(FDormantColumnEdits) - 1 do
    begin
      FDormantColumnEdits[LMove] := FDormantColumnEdits[LMove + 1];
    end;
    SetLength(FDormantColumnEdits, Length(FDormantColumnEdits) - 1);

    if FColumnEdit.Active then
    begin
      RememberDormantColumnEdit(FColumnEdit);
    end;
    FColumnEdit := LLease;
    LOpening := FOpeningDormantEdit;
    FOpeningDormantEdit := True;
    try
      { The exact fresh EditorShow is still entered. Restore before a typed
        character's remaining LCL tail, retaining its normal insertion behavior. }
      RestoreColumnEditor;
    finally
      FOpeningDormantEdit := LOpening;
    end;
    Exit;
  end;
end;

function TNativeMount.ReadColumnEditor: TColumnEditLease;
var
  LGrid: TStringGrid;
  LColumn: Integer;
  LEditor: TCustomEdit;
begin
  Result := FColumnEdit;

  if not InputLifetimeCurrent or Result.Active or (FRendered = nil) then
  begin
    Exit;
  end;
  LGrid := FActiveGrid;

  if (LGrid = nil) or not LGrid.EditorMode then
  begin
    LGrid := FGrid;

    if (FStartPane <> nil) and FStartPane.Grid.EditorMode then
    begin
      LGrid := FStartPane.Grid;
    end
    else if (FEndPane <> nil) and FEndPane.Grid.EditorMode then
    begin
      LGrid := FEndPane.Grid;
    end;
  end;

  if (LGrid = nil) or not LGrid.EditorMode or not (LGrid.Editor is TCustomEdit) or
    (LGrid.Row < 1) or (LGrid.Row > FRendered.Count) then
  begin
    Exit;
  end;
  LColumn := CanonicalColumn(LGrid, LGrid.Col);

  if (LColumn < 0) or (LColumn >= FView.Spec.Count) then
  begin
    Exit;
  end;
  Result.Item := FRendered.ItemAt(LGrid.Row - 1).Ref;
  Result.Field := FView.Spec.ColumnAt(LColumn).Identity;
  Result.Accepted := FView.Spec.ColumnAt(LColumn).Read(FRendered.Item(Result.Item));
  LEditor := TCustomEdit(LGrid.Editor);
  Result.Text := LEditor.Text;
  Result.Start := LEditor.SelStart;
  Result.Length := LEditor.SelLength;
  Result.HasNativeRange := True;
  Result.Selection := CaptureNyxLCLSelection(LEditor);
  Result.HadFocus := LGrid.Focused or LEditor.Focused;

  if (Screen <> nil) and (Screen.ActiveControl <> nil) and Screen.ActiveControl.Focused then
  begin
    Result.HadFocus := Result.HadFocus or
      (InputGrid(Screen.ActiveControl) = LGrid);
  end;
  Result.Active := True;
end;

function TNativeMount.ReadRecoveryDraft(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity; out ACell: TNyxCollectionCellEdit): Boolean;
var
  LLease: TColumnEditLease;
  LIndex: Integer;
  LMutation: Integer;
begin
  Result := False;
  ACell := Default(TNyxCollectionCellEdit);

  if not InputLifetimeCurrent or FEditsSuspended or (FColumnEditing = nil) or
    FColumnEditing.Composing then
  begin
    Exit;
  end;
  LMutation := FCellMutationSerial;
  LLease := ReadColumnEditor;

  if not InputLifetimeCurrent or (FCellMutationSerial <> LMutation) then
  begin
    Exit;
  end;

  if LLease.Active and (LLease.Item.Collection.Name = AItem.Collection.Name) and
    (LLease.Item.ID = AItem.ID) and LLease.Field.Same(AField) then
  begin
    ACell := NyxCollectionCellEdit(LLease.Item, LLease.Field, LLease.Accepted,
      TNyxText(LLease.Text), LLease.Selection, False, False);
    Result := ACell.Text <> ColumnValueDisplay(ACell.Accepted);
    Exit;
  end;
  for LIndex := 0 to High(FDormantColumnEdits) do
  begin

    if (FDormantColumnEdits[LIndex].Item.Collection.Name = AItem.Collection.Name) and
      (FDormantColumnEdits[LIndex].Item.ID = AItem.ID) and
      FDormantColumnEdits[LIndex].Field.Same(AField) then
    begin
      ACell := FDormantColumnEdits[LIndex].Copy;
      Result := ACell.Text <> ColumnValueDisplay(ACell.Accepted);
      Exit;
    end;
  end;
end;

function TNativeMount.DiscardRecoveryDraft(const ACell: TNyxCollectionCellEdit): Boolean;
var
  LCurrent: TNyxCollectionCellEdit;
  LLease: TColumnEditLease;
  LGrid: TStringGrid;
  LPhysical: Integer;
  LIndex: Integer;
  LMove: Integer;
  LUpdating: Boolean;
begin
  Result := False;

  if not ReadRecoveryDraft(ACell.Item, ACell.Field, LCurrent) or
    not NyxSameCollectionDraft(LCurrent, ACell) or not GetDraftRecoveryHeld then
  begin
    Exit;
  end;

  LLease := ReadColumnEditor;

  if not InputLifetimeCurrent or not GetDraftRecoveryHeld then
  begin
    Exit;
  end;

  if LLease.Active and (LLease.Item.ID = ACell.Item.ID) and
    LLease.Field.Same(ACell.Field) then
  begin
    LCurrent := NyxCollectionCellEdit(LLease.Item, LLease.Field, LLease.Accepted,
      TNyxText(LLease.Text), LLease.Selection, False, False);

    if not NyxSameCollectionDraft(LCurrent, ACell) then
    begin
      Exit;
    end;

    if FColumnEdit.Active then
    begin
      FColumnEdit := Default(TColumnEditLease);
      Exit(True);
    end;
    LGrid := GridForColumn(FColumnLayout.Spec.ColumnIndex(ACell.Field), LPhysical);

    if (LGrid = nil) or not LGrid.EditorMode or not (LGrid.Editor is TCustomEdit) then
    begin
      Exit;
    end;
    { Revert only the reviewed physical value. The ordinary explicit Refresh
      subsequently installs latest source data; no EditCell or validation runs.
      Use the cached physical spec, with no new outward getter after comparison. }
    LUpdating := FUpdating;
    FUpdating := True;
    try
      TCustomEdit(LGrid.Editor).Text := String(ColumnValueDisplay(ACell.Accepted));
      Result := True;
    finally
      FUpdating := LUpdating;
    end;
    Exit;
  end;
  for LIndex := 0 to High(FDormantColumnEdits) do
  begin

    if NyxSameCollectionDraft(FDormantColumnEdits[LIndex], ACell) then
    begin
      for LMove := LIndex to High(FDormantColumnEdits) - 1 do
      begin
        FDormantColumnEdits[LMove] := FDormantColumnEdits[LMove + 1];
      end;
      SetLength(FDormantColumnEdits, Length(FDormantColumnEdits) - 1);
      Exit(True);
    end;
  end;
end;

function TNativeMount.SuspendCellEdits: INyxCollectionEditsSnapshot;
var
  LKeepAlive: INyxCollectionMount;
  LLease: TColumnEditLease;
  LCells: TNyxCollectionCellEdits;
  LSnapshot: INyxCollectionEditsSnapshot;
  LView: INyxCollectionView;
  LData: INyxCollectionSnapshot;
  LOuter: TNyxCollectionStringGrid;
  LUpdating: Boolean;
  LRevision: Integer;
  LPhysical: Integer;
  LBodyFailed: Boolean;
  LIndex: Integer;
  LCount: Integer;

  procedure Suspend(AGrid: TNyxCollectionStringGrid);
  begin
    AGrid.AdvanceColumnMap;
    AGrid.EditorMode := False;

    if GetConnected then
    begin
      AGrid.ClearOverrides;
    end;
  end;

begin
  LKeepAlive := Self as INyxCollectionMount;

  if not InputLifetimeCurrent or not (FHasColumnLayout or FHasTablePresentation) or FEditsSuspended then
  begin
    raise ENyxCollection.Create('Cell edit suspension requires one live opted-in table lease');
  end;

  if FCellMutationSerial = High(Integer) then
  begin
    raise ENyxCollection.Create('Cell edit handoff generations are exhausted; remount the table');
  end;
  { An absent draft makes this provisional marker unqueryable. Any model/refresh
    callback during capture revokes it before the first editor can be hidden. }
  FCellFocusDraft := Default(TNyxCollectionCellEdit);
  FCellFocusValid := True;

  if (FColumnEditing = nil) or not FColumnEditing.GetReady or
    FColumnEditing.Composing then
  begin
    raise ENyxCollection.Create('Finish cell text composition before replacing this table');
  end;
  PruneDormantColumnEdits;

  if not InputLifetimeCurrent or FEditsSuspended or not FCellFocusValid then
  begin
    raise ENyxCollection.Create('Cell table changed while dormant drafts were captured');
  end;
  LView := FView;
  LData := FRendered;
  LRevision := FArrangedRevision;
  LLease := ReadColumnEditor;
  LCount := Length(FDormantColumnEdits);
  SetLength(LCells, LCount + Ord(LLease.Active));
  for LIndex := 0 to LCount - 1 do
  begin
    LCells[LIndex] := FDormantColumnEdits[LIndex].Copy;
  end;

  if LLease.Active then
  begin
    LCells[LCount] := NyxCollectionCellEdit(LLease.Item, LLease.Field,
      LLease.Accepted, TNyxText(LLease.Text), LLease.Selection, True,
      TNyxCollectionStringGrid(FGrid).HasOwnedColumnFocus);
  end;
  { Allocation and scalar/range validation precede any physical suspension.
    An alternate store getter may call out; no old editor has been hidden yet. }
  LSnapshot := NewNyxCollectionEditsSnapshot(LView.Store, LCells);

  if not GetConnected or (FView <> LView) or (FRendered <> LData) or
    (FArrangedRevision <> LRevision) or FEditsSuspended or not FCellFocusValid then
  begin
    raise ENyxCollection.Create('Cell editing changed while its handoff was captured');
  end;

  if not FColumnEditing.GetReady or FColumnEditing.Composing then
  begin
    raise ENyxCollection.Create('Cell text composition began while its handoff was captured');
  end;
  RetireColumnResize(FResizeRequest);
  { Suspension may be called directly, without an enclosing grid activity or
    base Refresh finally. Register only an ordinary idle wake, before callouts. }
  ArmCellRefreshIdle;
  FEditSuspension := LSnapshot;
  try
    SetCellEditsSuspended(True);
  except

    if GetConnected and (FView = LView) and (FEditSuspension = LSnapshot) then
    begin
      FEditSuspension := nil;
      SetCellEditsSuspended(False);
    end;
    raise;
  end;

  if not GetConnected or (FView <> LView) or (FRendered <> LData) or
    (FArrangedRevision <> LRevision) or (FEditSuspension <> LSnapshot) then
  begin

    if GetConnected and (FEditSuspension = LSnapshot) then
    begin
      FEditSuspension := nil;
      SetCellEditsSuspended(False);
    end;
    raise ENyxCollection.Create('Cell editing changed during suspension');
  end;
  FColumnEdit := LLease;
  FCellFocusValid := False;

  if LLease.Active and LCells[LCount].Focused then
  begin
    FCellFocusStore := LSnapshot.Store;
    FCellFocusDraft := LCells[LCount].Copy;
    FCellFocusCanonical := FColumnLayout.Spec.ColumnIndex(LLease.Field);
    FCellFocusGrid := GridForColumn(FCellFocusCanonical, LPhysical);
    FCellFocusColumn := LPhysical;
    FCellFocusRow := LData.IndexOf(LLease.Item) + 1;
    FCellFocusRevision := FArrangedRevision;
    FCellFocusValid := FCellFocusGrid <> nil;
  end;
  LUpdating := FUpdating;
  FUpdating := True;
  LOuter := TNyxCollectionStringGrid(FGrid);
  LOuter.BeginColumnTransition;
  LBodyFailed := False;
  try
    try
      try
        Suspend(LOuter);

        if GetConnected and (FStartPane <> nil) then
        begin
          Suspend(FStartPane.Grid);
        end;

        if GetConnected and (FEndPane <> nil) then
        begin
          Suspend(FEndPane.Grid);
        end;

        if not GetConnected then
        begin
          raise ENyxCollection.Create('Cell table retired during edit suspension');
        end;
        FCellFocusInput := Screen.ActiveControl;
        Result := LSnapshot;
      except
        LBodyFailed := True;
        raise;
      end;
    finally
      FUpdating := LUpdating;

      if GetConnected then
      begin

        if LBodyFailed then
        begin
          try
            LOuter.EndColumnTransition;
          except
            on E: Exception do Failed(E);
          end;
        end
        else
        begin
          LOuter.EndColumnTransition;
        end;
      end;
    end;
  except
    { A caller cannot resume a suspension that never returned. Undo this
      adapter's own lease before propagating the original suspension failure.
      Earlier tables are separately unwound by the renderer. }
    SetCellEditsSuspended(False);
    FEditSuspension := nil;
    FCellFocusValid := False;

    if GetConnected then
    begin
      try
        RestoreColumnEditor;
      except
        on E: Exception do Failed(E);
      end;
    end;
    raise;
  end;
end;

procedure TNativeMount.ResumeCellEdits(
  const ASnapshot: INyxCollectionEditsSnapshot; AFocus: Boolean);
var
  LKeepAlive: INyxCollectionMount;
  LSnapshot: INyxCollectionEditsSnapshot;
begin
  LKeepAlive := Self as INyxCollectionMount;
  LSnapshot := ASnapshot;

  if not GetConnected then
  begin
    Exit;
  end;

  if not FEditsSuspended or (LSnapshot = nil) or
    (LSnapshot <> FEditSuspension) then
  begin
    raise ENyxCollection.Create('Cell edit resume requires its exact current suspension');
  end;
  SetCellEditsSuspended(False);
  FEditSuspension := nil;
  Refresh;

  if not GetConnected then
  begin
    Exit;
  end;
  RestoreCellEdits(LSnapshot, AFocus);

  if GetConnected then
  begin
    ColumnValidationIdle(nil);
  end;
end;

function TNativeMount.RestoreCellEdits(
  const ASnapshot: INyxCollectionEditsSnapshot; AFocus: Boolean): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LSnapshot: INyxCollectionEditsSnapshot;
  LCells: TNyxCollectionCellEdits;
  LStore: INyxCollection;
  LTargetStore: INyxCollection;
  LView: INyxCollectionView;
  LData: INyxCollectionSnapshot;
  LSpec: TNyxCollectionViewSpec;
  LColumns: INyxCollectionColumns;
  LCell: TNyxCollectionCellEdit;
  LGrid: TStringGrid;
  LOuter: TNyxCollectionStringGrid;
  LExpectedFocus: TWinControl;
  LColumn, LPhysical, LIndex, LCount, LRevision, LLayoutSerial: Integer;
  LMutationSerial: Integer;
  LUpdating, LImporting: Boolean;
  LDormant: TNyxCollectionCellEdits;
  LDormantCount: Integer;

  procedure RequireTarget;
  var
    LCurrentData: INyxCollectionSnapshot;
    LCurrentSerial: Integer;
  begin
    { Alternate getters may call application code. Read them first, then fence
      the CURRENT managed target after the last outward getter returns. }
    LCurrentData := LView.Snapshot;
    LCurrentSerial := LColumns.CaptureColumns.Serial;

    if not InputLifetimeCurrent or FEditsSuspended or (FView <> LView) or
      (FColumnEditing = nil) or FColumnEditing.Composing or
      (FCellMutationSerial <> LMutationSerial) or
      (LMutationSerial = High(Integer)) or
      (LCurrentData <> LData) or (FRendered <> LData) or
      (FArrangedRevision <> LRevision) or
      (FColumnLayout.Serial <> LLayoutSerial) or
      (LCurrentSerial <> LLayoutSerial) then
    begin
      raise ENyxCollection.Create('Cell edit target changed during restoration');
    end;
  end;
begin
  Result := False;
  LKeepAlive := Self as INyxCollectionMount;
  FCellFocusValid := False;
  LSnapshot := ASnapshot;

  if not InputLifetimeCurrent or not (FHasColumnLayout or FHasTablePresentation) or FEditsSuspended or
    (FColumnEditing = nil) or FColumnEditing.Composing or (LSnapshot = nil) then
  begin
    raise ENyxCollection.Create('Cell edit restoration requires a live opted-in table');
  end;
  LMutationSerial := FCellMutationSerial;
  { Copy arbitrary snapshot readers before any physical mutation. The canonical
    constructor validates the complete vector, including duplicate/focus claims. }
  LStore := LSnapshot.Store;
  LCount := LSnapshot.Count;

  if LCount < 0 then
  begin
    raise ENyxCollection.Create('Cell edit snapshot has a negative count');
  end;
  SetLength(LCells, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LCells[LIndex] := LSnapshot.CellAt(LIndex).Copy;
  end;
  LSnapshot := NewNyxCollectionEditsSnapshot(LStore, LCells);

  if not InputLifetimeCurrent or FEditsSuspended then
  begin
    Exit;
  end;
  LView := FView;
  LRevision := FArrangedRevision;
  LLayoutSerial := FColumnLayout.Serial;
  LColumns := NyxCollectionColumns(LView);
  LData := LView.Snapshot;
  LTargetStore := LView.Store;
  LSpec := LView.Spec;

  if LStore <> LTargetStore then
  begin
    Exit;
  end;
  RequireTarget;
  SetLength(LDormant, LCount);
  LDormantCount := 0;
  for LIndex := 0 to LCount - 1 do
  begin
    LCell := LCells[LIndex];
    LColumn := LSpec.ColumnIndex(LCell.Field);

    if (LColumn >= 0) and LData.Has(LCell.Item) and
      (LSpec.ColumnAt(LColumn).Mode = cmEditable) and
      LSpec.ColumnAt(LColumn).Read(LData.Item(LCell.Item)).SameValue(LCell.Accepted) and
      (LCell.Text <> ColumnValueDisplay(LCell.Accepted)) and
      (not LCell.Editing or not FEnabled or FReadOnly or not ColumnVisible(LColumn)) then
    begin
      LDormant[LDormantCount] := NyxCollectionCellEdit(LCell.Item, LCell.Field,
        LCell.Accepted, LCell.Text, LCell.Selection, False, False);
      Inc(LDormantCount);
    end;
  end;
  RequireTarget;
  SetLength(LDormant, LDormantCount);
  FDormantColumnEdits := LDormant;
  FColumnEdit := Default(TColumnEditLease);
  for LIndex := 0 to LSnapshot.Count - 1 do
  begin
    LCell := LSnapshot.CellAt(LIndex);
    LColumn := LSpec.ColumnIndex(LCell.Field);

    if not LCell.Editing or (LColumn < 0) or not LData.Has(LCell.Item) or
      not FEnabled or FReadOnly or not ColumnVisible(LColumn) or
      (LSpec.ColumnAt(LColumn).Mode <> cmEditable) or
      not LSpec.ColumnAt(LColumn).Read(LData.Item(LCell.Item)).SameValue(LCell.Accepted) then
    begin
      Continue;
    end;
    FColumnEdit.Active := True;
    FColumnEdit.Item := LCell.Item;
    FColumnEdit.Field := LCell.Field;
    FColumnEdit.Accepted := LCell.Accepted;
    FColumnEdit.Text := String(LCell.Text);
    FColumnEdit.Selection := LCell.Selection;
    { Keep a compatible unfocused draft inert until its table regains focus.
      Opening an LCL editor can focus it even without an explicit SetFocus. }
    FColumnEdit.HadFocus := True;
    LOuter := TNyxCollectionStringGrid(FGrid);

    if not (AFocus and LCell.Focused) and not LOuter.HasOwnedColumnFocus then
    begin
      Exit;
    end;
    LGrid := GridForColumn(LColumn, LPhysical);

    if LGrid = nil then
    begin
      Exit;
    end;
    LUpdating := FUpdating;
    FUpdating := True;
    FCellFocusStore := LStore;
    FCellFocusDraft := LCell.Copy;
    FCellFocusGrid := LGrid;
    FCellFocusColumn := LPhysical;
    FCellFocusRow := LData.IndexOf(LCell.Item) + 1;
    FCellFocusCanonical := LColumn;
    FCellFocusRevision := LRevision;
    FCellFocusValid := LCell.Focused and AFocus;
    LOuter.BeginColumnTransition;
    try

      if AFocus and LCell.Focused and LGrid.CanFocus then
      begin
        LGrid.SetFocus;
      end;

      RequireTarget;
      LImporting := FImportingCellEdit;
      FImportingCellEdit := True;
      try
        RestoreColumnEditor;
      finally
        FImportingCellEdit := LImporting;
      end;
      RequireTarget;
      LExpectedFocus := Screen.ActiveControl;
      FCellFocusInput := LExpectedFocus;
      Result := True;
    finally
      FUpdating := LUpdating;

      if GetConnected then
      begin
        LOuter.EndColumnTransition;
      end;
    end;
    { EndColumnTransition may reuse the SAME native editor for another cell.
      Revalidate the logical/physical proof after that final outward call. }
    Result := Result and CellEditFocusMatches(LStore, LCell);
    Exit;
  end;
end;

function TNativeMount.CellEditFocusMatches(const AStore: INyxCollection;
  const ACell: TNyxCollectionCellEdit): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LGrid: TStringGrid;
  LEditor: TCustomEdit;
  LFocus: TWinControl;
  LText: String;
  LSelection: TNyxTextSelection;

  function CoordinatesCurrent: Boolean;
  begin
    Result := MatchesCellFocusProof(AStore, ACell) and
      (FCellFocusRevision = FArrangedRevision) and
      (FCellFocusGrid = LGrid) and (InputGrid(LGrid) = LGrid) and
      (LGrid.Row = FCellFocusRow) and (LGrid.Col = FCellFocusColumn) and
      (CanonicalColumn(LGrid, LGrid.Col) = FCellFocusCanonical) and
      (FCellFocusCanonical >= 0) and
      (FCellFocusCanonical < FColumnLayout.Count);

    if Result then
    begin
      Result := FColumnLayout.ColumnAt(FCellFocusCanonical).IsVisible and
        FColumnLayout.ColumnAt(FCellFocusCanonical).Column.Identity.Same(ACell.Field);
    end;
  end;
begin
  Result := False;
  LKeepAlive := Self as INyxCollectionMount;
  LGrid := FCellFocusGrid;

  if (LGrid = nil) or not CoordinatesCurrent or (Screen = nil) then
  begin
    Exit;
  end;
  LFocus := Screen.ActiveControl;

  if (LFocus = nil) or (LFocus <> FCellFocusInput) or
    (InputGrid(LFocus) <> LGrid) or not LFocus.Focused then
  begin
    Exit;
  end;

  if not CoordinatesCurrent or (Screen.ActiveControl <> LFocus) then
  begin
    Exit;
  end;

  if FEditsSuspended then
  begin
    { Hiding a pinned editor may focus an unrelated outer/spacer cell. Such a
      fallback deliberately fails the exact canonical coordinates above. }
    Result := (FEditSuspension <> nil) and not LGrid.EditorMode and
      FColumnEdit.Active and (FColumnEdit.Item.ID = ACell.Item.ID) and
      FColumnEdit.Field.Same(ACell.Field) and
      FColumnEdit.Accepted.SameValue(ACell.Accepted) and
      (TNyxText(FColumnEdit.Text) = ACell.Text);
    Exit;
  end;

  if not LGrid.EditorMode or not (LGrid.Editor is TCustomEdit) then
  begin
    Exit;
  end;
  LEditor := TCustomEdit(LGrid.Editor);
  { Text/selection reads can send native messages. Retain Self and reacquire
    the current editor/map after EACH read before touching borrowed input again. }
  LText := LEditor.Text;

  if not CoordinatesCurrent or (Screen.ActiveControl <> LFocus) or
    not LGrid.EditorMode or (LGrid.Editor <> LEditor) or
    (TNyxText(LText) <> ACell.Text) then
  begin
    Exit;
  end;

  if ACell.Selection.Defined then
  begin
    LSelection := CaptureNyxLCLSelection(LEditor);

    if not CoordinatesCurrent or (Screen.ActiveControl <> LFocus) or
      not LGrid.EditorMode or (LGrid.Editor <> LEditor) or
      not LSelection.Defined or not LSelection.SameRange(ACell.Selection) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

procedure TNativeMount.CaptureColumnEditor(const ARecoveryData: INyxCollectionSnapshot);
var
  LGrid: TStringGrid;
  LColumn: Integer;
  LPhysical: Integer;
  LRow: Integer;
  LRevision: Integer;
  LUpdating: Boolean;
  LLease: TColumnEditLease;
  LCell: TNyxCollectionCellEdit;
  LReason: TNyxCollectionDraftLoss;
begin

  if not InputLifetimeCurrent or FEditsSuspended or FColumnEdit.Active then
  begin
    Exit;
  end;

  if DeferTargetRefresh then
  begin
    Exit;
  end;
  LLease := ReadColumnEditor;

  if DeferTargetRefresh or not InputLifetimeCurrent then
  begin
    Exit;
  end;

  if (ARecoveryData <> nil) and LLease.Active and (FRendered <> nil) then
  begin
    LCell := NyxCollectionCellEdit(LLease.Item, LLease.Field, LLease.Accepted,
      TNyxText(LLease.Text), LLease.Selection, False, False);
    RecoverCell(LCell, FRendered, ARecoveryData, LReason,
      TNyxText(LLease.Text) <> ColumnValueDisplay(LLease.Accepted));

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;
  end;
  FColumnEdit := LLease;

  if not FColumnEdit.Active then
  begin
    Exit;
  end;
  LGrid := GridForColumn(FView.Spec.ColumnIndex(FColumnEdit.Field), LPhysical);

  if LGrid = nil then
  begin
    Exit;
  end;
  LColumn := FView.Spec.ColumnIndex(FColumnEdit.Field);
  LPhysical := LGrid.Col;
  LRow := LGrid.Row;
  LRevision := FArrangedRevision;
  LUpdating := FUpdating;

  if DeferTargetRefresh then
  begin
    FColumnEdit := Default(TColumnEditLease);
    Exit;
  end;
  FUpdating := True;
  try
    LGrid.EditorMode := False;

    if InputLifetimeCurrent and (LRevision = FArrangedRevision) and
      (InputGrid(LGrid) = LGrid) and
      (CanonicalColumn(LGrid, LPhysical) = LColumn) and
      (LGrid is TNyxCollectionStringGrid) then
    begin
      TNyxCollectionStringGrid(LGrid).ClearOverride(LPhysical, LRow);
    end;
  finally
    FUpdating := LUpdating;
  end;
end;

procedure TNativeMount.RestoreColumnEditor;
var
  LLease: TColumnEditLease;
  LGrid: TStringGrid;
  LEditor: TCustomEdit;
  LData: INyxCollectionSnapshot;
  LView: INyxCollectionView;
  LSpec: TNyxCollectionViewSpec;
  LColumn, LPhysical, LRow, LRevision, LMutation: Integer;
  LUpdating, LCompatible, LHasSelection: Boolean;

  function ReadyToWrite: Boolean;
  var
    LCurrent: INyxCollectionSnapshot;
  begin
    LCurrent := LView.Snapshot;

    if DeferComposingRefresh then
    begin
      { A NEW physical composition takes priority over the old dormant lease.
        The queued post-END refresh will capture that live editor's own text. }
      FColumnEdit := Default(TColumnEditLease);
      Exit(False);
    end;
    Result := InputLifetimeCurrent and not FEditsSuspended and
      (FView = LView) and (FRendered = LData) and (LCurrent = LData) and
      (LRevision = FArrangedRevision) and (LMutation = FCellMutationSerial) and
      (LMutation <> High(Integer)) and not ColumnInputBusy;
  end;
begin

  if not InputLifetimeCurrent or FEditsSuspended or FLayoutGesture then
  begin
    Exit;
  end;

  if DeferComposingRefresh then
  begin
    FColumnEdit := Default(TColumnEditLease);
    Exit;
  end;
  PruneDormantColumnEdits;

  if not InputLifetimeCurrent or not FColumnEdit.Active then
  begin
    Exit;
  end;

  if not FOpeningDormantEdit and
    (TNyxCollectionStringGrid(FGrid).ValidationActive or
    ((FStartPane <> nil) and FStartPane.Grid.ValidationActive) or
    ((FEndPane <> nil) and FEndPane.Grid.ValidationActive)) then
  begin
    { Enter/Tab's older LCL tail can still call ResetEditor after a validator
      refused a retired positional map. Keep this logical draft dormant until
      that native input frame has unwound; never validate a restored draft twice. }

    { The last validator queues one wake when it becomes idle. Do not repost
      from here: an application validator may itself be pumping native messages. }
    Exit;
  end;
  LLease := FColumnEdit;
  LView := FView;
  LSpec := FColumnLayout.Spec;
  LRevision := FArrangedRevision;
  LMutation := FCellMutationSerial;
  LColumn := LSpec.ColumnIndex(LLease.Field);
  LData := LView.Snapshot;

  LCompatible := (LColumn >= 0) and LData.Has(LLease.Item) and
    (LSpec.ColumnAt(LColumn).Mode = cmEditable) and
    LSpec.ColumnAt(LColumn).Read(LData.Item(LLease.Item)).SameValue(LLease.Accepted) and not
    (FNormalizeValues and (FNormalizeItem.ID = LLease.Item.ID) and
      (FNormalizeColumn = LColumn));

  if not ReadyToWrite then
  begin
    Exit;
  end;

  if not LCompatible then
  begin
    FColumnEdit := Default(TColumnEditLease);
    Exit;
  end;

  if not ColumnVisible(LColumn) or not FEnabled or FReadOnly then
  begin
    { Withdrawal of physical authority never admits an edit. Keep only dirty
      detached text/caret with its accepted scalar; reveal alone cannot revive
      EditorMode, focus or native validation of the old coordinate. }
    RememberDormantColumnEdit(LLease);
    FColumnEdit := Default(TColumnEditLease);
    Exit;
  end;

  if LLease.HadFocus and (FGrid is TNyxCollectionStringGrid) and
    not TNyxCollectionStringGrid(FGrid).HasOwnedColumnFocus and
    not TNyxCollectionStringGrid(FGrid).ColumnTransitionHadFocus then
  begin
    { Keep the inert draft while a layout form owns focus. A later management
      close/accepted focus restoration may resume it; never steal external focus. }
    Exit;
  end;
  LGrid := GridForColumn(LColumn, LPhysical);

  if LGrid = nil then
  begin
    FColumnEdit := Default(TColumnEditLease);
    Exit;
  end;
  FColumnEdit := Default(TColumnEditLease);
  LRevision := FArrangedRevision;
  LRow := LData.IndexOf(LLease.Item) + 1;
  LUpdating := FUpdating;
  FUpdating := True;
  try

    if not ReadyToWrite then
    begin
      Exit;
    end;
    LGrid.Col := LPhysical;

    if not ReadyToWrite then
    begin
      Exit;
    end;
    LGrid.Row := LRow;

    if not ReadyToWrite then
    begin
      Exit;
    end;
    FActiveGrid := LGrid;
    FFocusedCanonical := LColumn;

    if LLease.HadFocus and LGrid.CanFocus then
    begin
      LGrid.SetFocus;
    end;

    if not ReadyToWrite then
    begin
      Exit;
    end;
    LGrid.EditorMode := True;

    if not ReadyToWrite then
    begin
      Exit;
    end;

    if LGrid.Editor is TCustomEdit then
    begin
      LEditor := TCustomEdit(LGrid.Editor);
      LEditor.Text := LLease.Text;

      if not ReadyToWrite or (FActiveGrid <> LGrid) then
      begin
        Exit;
      end;
      { Text can invoke a native edit observer. Reacquire the editor after that
        boundary instead of continuing through a borrowed former edit object. }

      if not LGrid.EditorMode or (LGrid.Col <> LPhysical) or (LGrid.Row <> LRow) or
        (CanonicalColumn(LGrid, LPhysical) <> LColumn) or
        not (LGrid.Editor is TCustomEdit) then
      begin
        Exit;
      end;
      LEditor := TCustomEdit(LGrid.Editor);

      LHasSelection := LLease.Selection.Defined and CaptureNyxLCLSelection(LEditor).Defined;

      if not ReadyToWrite then
      begin
        Exit;
      end;

      if LHasSelection then
      begin
        SelectNyxLCLText(LEditor, LLease.Selection);
      end
      else if LLease.HasNativeRange then
      begin
        { Host-local fallback only. No raw native offset crosses the portable
          snapshot when that platform cannot describe a scalar selection. }
        LEditor.SelStart := LLease.Start;

        if ReadyToWrite then
        begin
          LEditor.SelLength := LLease.Length;
        end;
      end;
    end;
  finally
    FUpdating := LUpdating;
  end;
end;

function TNativeMount.GetEditingPublication: INyxObservationPublication;
begin
  Result := FColumnEditing;
end;

function TNativeMount.EditingDispatching: Boolean;
begin
  Result := (FColumnEditing <> nil) and FColumnEditing.Dispatching;
end;

function TNativeMount.DeferTargetRefresh: Boolean;
var
  LEditing: IColumnEditing;
  LReady: Boolean;
begin
  Result := False;

  if DeferComposingRefresh then
  begin
    Exit(True);
  end;

  if not FHasColumnLayout and not (FObservesTablePresentation and (FColumnEditing <> nil)) then
  begin
    Exit;
  end;
  LEditing := FColumnEditing;
  LReady := (LEditing <> nil) and LEditing.GetReady;
  Result := not InputLifetimeCurrent or not LReady or
    (FColumnEditing <> LEditing) or LEditing.Composing;

  if Result then
  begin
    FTargetRefreshDeferred := True;
    FCellRefreshPending := InputLifetimeCurrent;

    if LEditing <> nil then
    begin
      LEditing.Continuity.DeferPositions(True);
    end;
  end;
end;

function TNativeMount.DeferComposingRefresh: Boolean;
begin
  { This first check runs before the base suppresses Ready. Never query the
    event router here: an existing composition must keep its END/text delivery. }
  Result := (FHasColumnLayout or FObservesTablePresentation) and (FColumnEditing <> nil) and
    FColumnEditing.Composing;

  if ColumnResizeInterrupted(FResizeRequest) then
  begin
    RetireColumnResize(FResizeRequest);
  end;

  if Result then
  begin
    FTargetRefreshDeferred := True;
    FCellRefreshPending := InputLifetimeCurrent;
    FColumnEditing.Continuity.DeferPositions(True);
  end;
end;

procedure TNativeMount.RefreshTargetIdle;
begin
  { Base Refresh has cleared BOTH shields, including on its composition-deferred
    early return. Only register an ordinary wake; never drain/repaint in this tail. }

  SynchronizeRecovery;
  SynchronizeClipboardFeedback;

  if ColumnResizeInterrupted(FResizeRequest) then
  begin
    RetireColumnResize(FResizeRequest);
  end;
  ArmCellRefreshIdle;
end;

function TNativeMount.ColumnInputBusy: Boolean;
begin
  { No model/router getter belongs in this physical-position check. Accepted
    text can continue in the existing editor; switching/committing a positional
    cell waits for the deferred physical map to catch up after composition. }
  Result := FCellRefreshPending or (FColumnEditing = nil) or
    FColumnEditing.Composing;
end;

procedure TNativeMount.RetireTargetInput;
var
  LIndex: Integer;
begin
  FinishRangeGesture(FRangeGesture);
  FColumnGeometryPending := False;
  FTextLayoutTotal := 0;
  FColumnClick := nil;
  FCellContextDispatch := nil;
  FRangePointerDispatch := nil;
  FRangeEvents := nil;
  FRangePaint := nil;
  FRangePaintStroke := nil;
  FRangePaintFields := nil;

  if FGrid is TNyxCollectionStringGrid then
  begin
    TNyxCollectionStringGrid(FGrid).ObserveCellRange(nil);
  end;

  if FInputGrid <> nil then
  begin

    if FInputGridAttached then
    begin
      FInputGrid.ConfigureCellContext(GetInputLifetime, nil, nil);
      FInputGrid.ConfigureRangePointer(GetInputLifetime, nil, nil);
    end;
    FInputGrid.ObserveColumnEditor(nil);
  end;

  if FStartPane <> nil then
  begin
    FStartPane.Grid.ObserveCellRange(nil);
    FStartPane.Grid.ConfigureCellContext(GetInputLifetime, nil, nil);
    FStartPane.Grid.ConfigureRangePointer(GetInputLifetime, nil, nil);
    FStartPane.Grid.ObserveColumnEditor(nil);
  end;

  if FEndPane <> nil then
  begin
    FEndPane.Grid.ObserveCellRange(nil);
    FEndPane.Grid.ConfigureCellContext(GetInputLifetime, nil, nil);
    FEndPane.Grid.ConfigureRangePointer(GetInputLifetime, nil, nil);
    FEndPane.Grid.ObserveColumnEditor(nil);
  end;

  if FColumnsButton <> nil then
  begin
    FColumnsButton.RetireColumnAction;
  end;
  for LIndex := 0 to High(FFitButtons) do
  begin

    if FFitButtons[LIndex] <> nil then
    begin
      FFitButtons[LIndex].RetireFitAction;
    end;
  end;
  RetireColumnResize(FResizeRequest);
  FLayoutGesture := False;
  FResizeQueued := False;
  FResizeReleased := False;
  FCellRefreshPending := False;
  FCellRefreshQueued := False;

  if FColumnEditing <> nil then
  begin
    FColumnEditing.Revoke;
  end;
end;

procedure TNativeMount.CellCompositionEnded(Sender: TObject);
begin

  if not InputLifetimeCurrent or FEditsSuspended or FDraftRecoveryHeld or
    not (FCellRefreshPending or FColumnGeometryPending) or
    FCellRefreshQueued or (FColumnEditing = nil) or FColumnEditing.Composing then
  begin
    Exit;
  end;
  FCellRefreshQueued := True;
  Application.QueueAsyncCall(RefreshAfterComposition, 0);
end;

procedure TNativeMount.ArmCellRefreshIdle;
begin

  if not InputLifetimeCurrent or FCellIdleArmed then
  begin
    Exit;
  end;

  if not FRecoveryCountPending and not FClipboardStatusPending and
    (FDraftRecoveryHeld or not (FCellRefreshPending or FColumnGeometryPending)) and ((FResizeRequest = nil) or
    FResizeRequest.Publishing or FResizeQueued or
    not (FResizeRequest.Retired or FResizeReleased)) then
  begin
    Exit;
  end;
  FCellIdleArmed := True;
  Application.AddOnIdleHandler(CellRefreshIdle);
end;

procedure TNativeMount.CellRefreshIdle(Sender: TObject; var ADone: Boolean);
var
  LKeepAlive: INyxCollectionMount;
  LRequest: TNativeColumnLayoutRequest;
  LRequestLease: IInterface;
  LIndex: Integer;
begin
  LKeepAlive := Self as INyxCollectionMount;

  if not InputLifetimeCurrent then
  begin
    Application.RemoveOnIdleHandler(CellRefreshIdle);
    FCellIdleArmed := False;
    Exit;
  end;
  LRequest := FResizeRequest;
  LRequestLease := FResizeRequestLease;

  if ColumnResizeInterrupted(LRequest) then
  begin
    RetireColumnResize(LRequest);
  end;

  if FRecoveryCountPending then
  begin

    if RecoveryPresentationReady then
    begin
      SynchronizeRecovery;

      if not InputLifetimeCurrent then
      begin
        Exit;
      end;
      { Presentation setters may enter callbacks which replace the current
        resize request. Recapture that exact slot before its ordinary drain. }
      LRequest := FResizeRequest;
      LRequestLease := FResizeRequestLease;
    end;
  end;

  if FClipboardStatusPending and RecoveryPresentationReady then
  begin
    SynchronizeClipboardFeedback;

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;
    LRequest := FResizeRequest;
    LRequestLease := FResizeRequestLease;
  end;

  if (LRequest <> nil) and not LRequest.Publishing and not FResizeQueued and
    (LRequest.Retired or FResizeReleased) then
  begin
    { A retired request needs no Ready, END or Resume admission. Wait for the
      complete entered native tail, then queue the SAME one-shot publisher.
      FCellRefreshPending remains exclusively a real deferred cell-map flag. }

    if FUpdating then
    begin
      Exit;
    end;
    for LIndex := Low(LRequest.Activities) to High(LRequest.Activities) do
    begin

      if (LRequest.Activities[LIndex] <> nil) and LRequest.Activities[LIndex].GetActive then
      begin
        Exit;
      end;
    end;
    FResizeQueued := True;
    Application.QueueAsyncCall(PublishColumnLayout, LRequest.Serial);
  end;

  if FDraftRecoveryHeld or not (FCellRefreshPending or FColumnGeometryPending) or FCellRefreshQueued then
  begin
    { A count-only wake must survive composition/entered input without turning
      into a row refresh. One registration waits for the next ordinary idle. }

    if (FRecoveryCountPending or FClipboardStatusPending) and not FDraftRecoveryHeld then
    begin
      Exit;
    end;
    { Queued work now owns its retry. A busy publisher explicitly rearms this
      same registration, even when there will be no later native Activity.Leave. }
    Application.RemoveOnIdleHandler(CellRefreshIdle);
    FCellIdleArmed := False;
    Exit;
  end;

  if FEditsSuspended or FUpdating or FLayoutGesture or (FColumnEditing = nil) or
    FColumnEditing.Composing or FColumnFrame.Dispatching or
    TNyxCollectionStringGrid(FGrid).ValidationActive or
    ((FStartPane <> nil) and FStartPane.Grid.ValidationActive) or
    ((FEndPane <> nil) and FEndPane.Grid.ValidationActive) then
  begin
    { Do not request another idle turn or repeatedly repost into a nested pump.
      The next ordinary idle boundary is the next opportunity to queue work. }
    Exit;
  end;
  Application.RemoveOnIdleHandler(CellRefreshIdle);
  FCellIdleArmed := False;
  CellCompositionEnded(nil);
end;

procedure TNativeMount.RefreshAfterComposition(AData: PtrInt);
var
  LKeepAlive: INyxCollectionMount;
begin

  if not InputLifetimeCurrent then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  FCellRefreshQueued := False;

  if FDraftRecoveryHeld or not (FCellRefreshPending or FColumnGeometryPending) or FEditsSuspended or
    (FColumnEditing = nil) or FColumnEditing.Composing then
  begin
    Exit;
  end;

  if FUpdating or FLayoutGesture or FColumnFrame.Dispatching or
    TNyxCollectionStringGrid(FGrid).ValidationActive or
    ((FStartPane <> nil) and FStartPane.Grid.ValidationActive) or
    ((FEndPane <> nil) and FEndPane.Grid.ValidationActive) then
  begin

    ArmCellRefreshIdle;
    Exit;
  end;
  Refresh;
end;

procedure TNativeMount.ColumnValidationIdle(Sender: TObject);
var
  LRequest: TNativeColumnLayoutRequest;
  LLease: IInterface;
  LIndex: Integer;
begin
  CellCompositionEnded(nil);

  if not GetConnected then
  begin
    Exit;
  end;
  LRequest := FResizeRequest;
  LLease := FResizeRequestLease;

  if ColumnResizeInterrupted(LRequest) then
  begin
    RetireColumnResize(LRequest);
  end;

  if (LRequest <> nil) and not LRequest.Publishing then
  begin
    for LIndex := Low(LRequest.Activities) to High(LRequest.Activities) do
    begin

      if (LRequest.Activities[LIndex] <> nil) and LRequest.Activities[LIndex].GetActive then
      begin
        Exit;
      end;
    end;

    if (LRequest.Retired or FResizeReleased) and not FResizeQueued then
    begin
      FResizeQueued := True;
      Application.QueueAsyncCall(PublishColumnLayout, LRequest.Serial);
    end;
  end;

  if FEditsSuspended or
    TNyxCollectionStringGrid(FGrid).ValidationActive or
    ((FStartPane <> nil) and FStartPane.Grid.ValidationActive) or
    ((FEndPane <> nil) and FEndPane.Grid.ValidationActive) then
  begin
    Exit;
  end;

  if FLayoutGesture then
  begin

    Exit;
  end;

  if not FColumnEdit.Active or FResumeQueued or
    not TNyxCollectionStringGrid(FGrid).HasOwnedColumnFocus then
  begin
    Exit;
  end;
  FResumeQueued := True;
  Application.QueueAsyncCall(ResumeColumnEditor, 0);
end;

procedure TNativeMount.ResumeColumnEditor(AData: PtrInt);
var
  LKeepAlive: INyxCollectionMount;
  LOuter: TNyxCollectionStringGrid;
begin
  LKeepAlive := Self as INyxCollectionMount;
  FResumeQueued := False;

  if not GetConnected or FEditsSuspended or not FColumnEdit.Active then
  begin
    Exit;
  end;
  LOuter := TNyxCollectionStringGrid(FGrid);
  LOuter.BeginColumnTransition;
  try
    RestoreColumnEditor;
  finally

    if GetConnected then
    begin
      LOuter.EndColumnTransition;
    end;
  end;
end;

procedure TNativeMount.RenderArrangedGrid(const AData: INyxCollectionSnapshot);
var
  LPosition: TNyxCollectionPositionRecovery;
  LOuter: TNyxCollectionStringGrid;
  LStart, LEnd, LMain: TNyxCollectionColumnOrder;
  LIndex, LColumn, LRow, LWidth, LStartWidth, LEndWidth: Integer;
  LSelected, LPhysical, LTop, LSample, LPass, LClipWidth: Integer;
  LScale: Double;
  LHadFocus, LUpdating, LEditable, LResizable: Boolean;
  LFocusGrid: TStringGrid;
  LFocus: TNyxItemRef;
  LButtonFocus: TNyxCollectionColumnButton;

  procedure Append(var AMap: TNyxCollectionColumnOrder; AColumn: Integer);
  var
    LCount: Integer;
  begin
    LCount := Length(AMap);
    SetLength(AMap, LCount + 1);
    AMap[LCount] := AColumn;
  end;

  procedure ConfigurePane(var APane: TColumnPane;
    const AMap: TNyxCollectionColumnOrder; ALeft, AWidth: Integer);
  var
    LPaneIndex: Integer;
  begin

    if Length(AMap) = 0 then
    begin

      if APane <> nil then
      begin
        APane.Grid.Visible := False;
        APane.Columns := nil;
        APane.Grid.ClearOverrides;
      end;
      Exit;
    end;

    if APane = nil then
    begin
      APane := TColumnPane.Create(Self);
    end;
    APane.Columns := System.Copy(AMap, 0, Length(AMap));
    APane.Grid.ClearOverrides;
    APane.Grid.Options := FGrid.Options - [goColMoving, goSmoothScroll];
    APane.Grid.Font.Assign(FGrid.Font);
    { Edge panes are the same logical table. Keep title/body drawing defaults
      together, including a custom outer title font. A later unhidden field
      must not switch typography merely because its pin creates a new pane. }
    APane.Grid.TitleFont.Assign(FGrid.TitleFont);
    APane.Grid.DefaultTextStyle := FGrid.DefaultTextStyle;
    APane.Grid.Color := FGrid.Color;
    APane.Grid.FixedColor := FGrid.FixedColor;
    APane.Grid.GridLineWidth := FGrid.GridLineWidth;
    APane.Grid.DefaultRowHeight := FGrid.DefaultRowHeight;
    APane.Grid.ColCount := Length(AMap);
    APane.Grid.RowCount := Max(2, AData.Count + 1);
    APane.Grid.FixedCols := 0;
    APane.Grid.FixedRows := 1;
    APane.Grid.RowHeights[0] := FGrid.RowHeights[0];
    APane.Grid.ColumnToolbarHeight := FColumnToolbarHeight;
    for LPaneIndex := 0 to High(AMap) do
    begin
      APane.Grid.ColWidths[LPaneIndex] := FColumnApplied[AMap[LPaneIndex]];
    end;
    { Child bounds use the outer CLIENT, excluding its scrollbars and border.
      LCL mirrors column coordinates for RTL; the panes follow that same fact. }
    APane.Grid.SetBounds(ALeft, 0, AWidth, FGrid.ClientHeight);
    APane.Grid.Visible := True;
    APane.Grid.BringToFront;
  end;

begin

  if not InputLifetimeCurrent or FColumnApplying or DeferTargetRefresh then
  begin
    Exit;
  end;
  { Admission must precede even column-transition/toolbar invalidation. A held
    dirty editor is still a live LCL control; repeatedly invalidating its map
    on refused refresh can continuously enqueue editor/paint work. }
  CaptureColumnEditor(AData);
  RecoverColumnDrafts(AData);

  if not InputLifetimeCurrent or FTargetRefreshDeferred or DeferTargetRefresh then
  begin
    Exit;
  end;
  FCellFocusValid := False;
  LOuter := TNyxCollectionStringGrid(FGrid);
  LHadFocus := LOuter.HasOwnedColumnFocus;
  LButtonFocus := FocusedFitButton;
  LUpdating := FUpdating;
  FUpdating := True;
  FColumnApplying := True;
  LOuter.BeginColumnTransition;
  try
    { Withdraw only our previous physical band before the existing query/header
      policy computes its base height. Reapplying must never accumulate pixels
      or alter body row zero/one indexing, including late query configuration. }

    if FColumnToolbarApplied then
    begin
      FGrid.RowHeights[0] := Max(1, FGrid.RowHeights[0] - FColumnToolbarHeight);
      FColumnToolbarApplied := False;
    end;
    FColumnToolbarHeight := 0;
    LOuter.ColumnToolbarHeight := 0;
    { Retire every entered validator before editor/overlay/geometry changes,
      including a no-op edit's explicit normalization on an equal snapshot. }

    if (FResizeRequest <> nil) and
      not (FResizeRequest.Publishing and FResizeRequest.Installed) then
    begin
      RetireColumnResize(FResizeRequest);
    end;
    LOuter.AdvanceColumnMap;

    if FStartPane <> nil then
    begin
      FStartPane.Grid.AdvanceColumnMap;
    end;

    if FEndPane <> nil then
    begin
      FEndPane.Grid.AdvanceColumnMap;
    end;

    if FArrangedRevision = High(Integer) then
    begin
      raise ENyxCollection.Create('Native column presentation revision is exhausted');
    end;
    Inc(FArrangedRevision);
    FColumnRendering := AData;
    LTop := FGrid.TopRow;
    LPosition := ResolvePosition(FRendered, AData, LTop - 1);

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;

    if LPosition.Index >= 0 then
    begin
      LTop := LPosition.Index + 1;
    end
    else if (FRendered <> nil) and (LTop > 0) and (LTop <= FRendered.Count) then
    begin
      LTop := AData.IndexOf(FRendered.ItemAt(LTop - 1).Ref) + 1;
    end;
    LScale := Max(1, FGrid.Font.PixelsPerInch) / 96;

    if FGrid.Font.PixelsPerInch <= 0 then
    begin
      LScale := 1;
    end;

    if (Length(FColumnNatural) <> FView.Spec.Count) or (FRendered <> AData) or
      (FArrangedSerial <> FColumnLayout.Serial) or
      (FNaturalFontName <> FGrid.Font.Name) or
      (FNaturalFontHeight <> FGrid.Font.Height) or
      (FNaturalFontPPI <> FGrid.Font.PixelsPerInch) or
      (FNaturalFontStyle <> FGrid.Font.Style) then
    begin
      { Measure a declared, bounded sample. Arrangement never asks native
        AutoSizeColumns to walk the entire store or materialize every cell. }
      SetLength(FColumnNatural, FView.Spec.Count);
      FGrid.Canvas.Font.Assign(FGrid.Font);
      LSample := Min(AData.Count, FView.Spec.TableLayoutPolicy.AutoSizeRows);
      for LColumn := 0 to FView.Spec.Count - 1 do
      begin
        LWidth := FGrid.Canvas.TextWidth(FView.Spec.ColumnAt(LColumn).Title) + 24;
        for LRow := 0 to LSample - 1 do
        begin
          LWidth := Max(LWidth, FGrid.Canvas.TextWidth(DisplayText(
            FView.Spec.ColumnAt(LColumn).Display(AData.ItemAt(LRow), AData.Schema))) + 16);
        end;
        FColumnNatural[LColumn] := Min(MaximumNyxLayoutBound,
          Max(1, Round(LWidth / LScale)));
      end;
      FNaturalFontName := FGrid.Font.Name;
      FNaturalFontHeight := FGrid.Font.Height;
      FNaturalFontPPI := FGrid.Font.PixelsPerInch;
      FNaturalFontStyle := FGrid.Font.Style;
    end;
    { Establish vertical geometry before observing the horizontal client clip.
      Width installation can itself reveal a horizontal native scrollbar and
      in turn a vertical one. Reallocate at most three times, using the smaller
      observed clip, so native scrollbar settlement cannot consume pin space. }
    FGrid.RowCount := Max(2, AData.Count + 1);
    FGrid.FixedRows := 1;

    ReserveHeader;

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;
    { The launcher belongs to the logical table, never a visible field/pin
      group. Its row-zero band survives all-hidden and narrow presentations. }

    if (FColumnManager <> nil) or HasClipboardFeedback then
    begin
      FColumnToolbarHeight := RequiredToolbarHeight(GetRecoveredEditCount);
      FGrid.RowHeights[0] := FGrid.RowHeights[0] + FColumnToolbarHeight;
      FColumnToolbarApplied := True;
      LOuter.ColumnToolbarHeight := FColumnToolbarHeight;
    end;
    LClipWidth := FGrid.ClientWidth;
    for LPass := 0 to 2 do
    begin
      FColumnPresentation := NyxColumnPresentation(FColumnLayout,
        Max(0, Round((LClipWidth - FGrid.GridLineWidth *
          FColumnLayout.VisibleCount) / LScale)), FColumnNatural);
      LEditable := False;
      LResizable := False;
      for LColumn := 0 to FColumnLayout.Count - 1 do
      begin

        if FColumnLayout.ColumnAt(LColumn).IsVisible then
        begin
          LEditable := LEditable or (FEnabled and not FReadOnly and
            (FView.Spec.ColumnAt(LColumn).Mode = cmEditable));
          LResizable := LResizable or (FEnabled and
            (nclfResize in FColumnLayout.ColumnAt(LColumn).Features));
        end;
      end;

      if LEditable then
      begin
        FGrid.Options := FGrid.Options + [goEditing];
      end
      else
      begin
        FGrid.Options := FGrid.Options - [goEditing];
      end;

      if LResizable then
      begin
        FGrid.Options := FGrid.Options + [goColSizing];
      end
      else
      begin
        FGrid.Options := FGrid.Options - [goColSizing];
      end;
      SetLength(FColumnApplied, FView.Spec.Count);
      LStart := nil;
      LEnd := nil;
      LMain := nil;
      LStartWidth := 0;
      LEndWidth := 0;
      for LIndex := 0 to FColumnLayout.VisibleCount - 1 do
      begin
        LColumn := FColumnLayout.VisibleAt(LIndex);
        FColumnApplied[LColumn] := Max(1,
          Round(FColumnPresentation.Width(LColumn) * LScale));
        case FColumnPresentation.EffectivePin(LColumn) of
          ncpStart:
            begin
              Append(LStart, LColumn);
              Inc(LStartWidth, FColumnApplied[LColumn] + FGrid.GridLineWidth);
            end;
          ncpEnd:
            begin
              Append(LEnd, LColumn);
              Inc(LEndWidth, FColumnApplied[LColumn] + FGrid.GridLineWidth);
            end;
          ncpNone: Append(LMain, LColumn);
        end;
      end;

      if Length(LStart) > 0 then
      begin
        SetLength(FMainColumns, Length(LMain) + 1);
        FMainColumns[0] := -1;
        for LIndex := 0 to High(LMain) do
        begin
          FMainColumns[LIndex + 1] := LMain[LIndex];
        end;
      end
      else
      begin
        FMainColumns := System.Copy(LMain, 0, Length(LMain));
      end;

      if Length(LMain) = 0 then
      begin
        Append(FMainColumns, -1);
      end;

      if Length(LEnd) > 0 then
      begin
        Append(FMainColumns, -1);
      end;
      LOuter.ClearOverrides;
      FGrid.FixedCols := 0;
      FGrid.ColCount := Length(FMainColumns);
      FGrid.RowCount := Max(2, AData.Count + 1);
      FGrid.FixedRows := 1;

      ReserveHeader;

      if not InputLifetimeCurrent then
      begin
        Exit;
      end;
      for LIndex := 0 to High(FMainColumns) do
      begin
        LColumn := FMainColumns[LIndex];

        if LColumn >= 0 then
        begin
          FGrid.ColWidths[LIndex] := FColumnApplied[LColumn];
        end
        else if (LIndex = 0) and (LStartWidth > 0) then
        begin
          FGrid.ColWidths[LIndex] := Max(1, LStartWidth - FGrid.GridLineWidth);
        end
        else if (LIndex = High(FMainColumns)) and (LEndWidth > 0) then
        begin
          FGrid.ColWidths[LIndex] := Max(1, LEndWidth - FGrid.GridLineWidth);
        end
        else
        begin
          FGrid.ColWidths[LIndex] := Max(1, FGrid.ClientWidth - LStartWidth - LEndWidth);
        end;
      end;

      if LStartWidth > 0 then
      begin
        FGrid.FixedCols := 1;
      end;
      { The composite exposes one explicit editor. Always-shown native editors
        would reopen inside ResetEditor's older callback tail on another pane. }
      FGrid.Options := FGrid.Options - [goColMoving, goAlwaysShowEditor];

      if (LStartWidth > 0) or (LEndWidth > 0) then
      begin
        FGrid.Options := FGrid.Options - [goSmoothScroll];
      end
      else if goSmoothScroll in FPreviousOptions then
      begin
        FGrid.Options := FGrid.Options + [goSmoothScroll];
      end;

      if FGrid.UseRightToLeftAlignment then
      begin
        ConfigurePane(FStartPane, LStart, FGrid.ClientWidth - LStartWidth, LStartWidth);
        ConfigurePane(FEndPane, LEnd, 0, LEndWidth);
      end
      else
      begin
        ConfigurePane(FStartPane, LStart, 0, LStartWidth);
        ConfigurePane(FEndPane, LEnd, FGrid.ClientWidth - LEndWidth, LEndWidth);
      end;

      if FGrid.ClientWidth >= LClipWidth then
      begin
        Break;
      end;
      LClipWidth := FGrid.ClientWidth;
    end;
    FArrangedSerial := FColumnLayout.Serial;
    FRendered := AData;
    FCellRefreshPending := False;
    FColumnEditing.Continuity.DeferPositions(False);
    FGrid.TopRow := Min(Max(1, LTop), FGrid.RowCount - 1);
    LSelected := -1;
    LFocus := FView.Selection.Focus;

    if LFocus.Defined then
    begin
      LSelected := AData.IndexOf(LFocus);
    end;

    if LSelected >= 0 then
    begin
      FGrid.Row := LSelected + 1;
    end;
    LFocusGrid := GridForColumn(FFocusedCanonical, LPhysical);

    if (LFocusGrid = nil) and (DisplayColumnCount > 0) then
    begin
      FFocusedCanonical := DisplayColumn(0);
      LFocusGrid := GridForColumn(FFocusedCanonical, LPhysical);
    end;

    if LFocusGrid <> nil then
    begin
      LFocusGrid.Col := LPhysical;
      LFocusGrid.Row := FGrid.Row;
      FActiveGrid := LFocusGrid;

      if LHadFocus and (LButtonFocus = nil) and not FEditsSuspended and
        not FColumnEdit.Active and LFocusGrid.CanFocus then
      begin
        LFocusGrid.SetFocus;
      end;
    end;

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;
    SynchronizePaneRows;
    FGridHelp := FQuerySummary;

    if FColumnPresentation.PinsSuspended then
    begin
      FGridHelp := FGridHelp + ' Pinned columns are scrolling at this width.';
    end;
    for LIndex := 0 to FColumnLayout.Count - 1 do
    begin

      if FColumnLayout.ColumnAt(LIndex).IsVisible and
        (nclfAutoSize in FColumnLayout.ColumnAt(LIndex).Features) then
      begin
        FGridHelp := FGridHelp +
          ' Fit a column: double-click its header edge, or press Ctrl+Shift+F in its cell.';
        Break;
      end;
    end;
    ApplyGridHint;
    SynchronizeFitButtons;
    SynchronizeColumnsButton;
    PrepareTextRows(AData);
    SynchronizePaneRows;
    RestoreColumnEditor;
    { Row/editor selection can scroll LCL after the first TopRow assignment.
      Opted-in source recovery settles last, then mirrors the SAME outer row
      into the pinned panes. Ordinary selection-only reveals remain unchanged. }

    if InputLifetimeCurrent and (LPosition.Index >= 0) then
    begin
      FGrid.TopRow := Min(Max(1, LTop), FGrid.RowCount - 1);
      SynchronizePaneRows;
    end;
    { Editor/source recovery can scroll after the first bounded measurement.
      Its guarded native notification does not run GridTopLeft, so settle the
      actual final row window before exposing wrapped cells again. }

    if InputLifetimeCurrent and FHasTablePresentation and
      (FTextLayoutFirst <> Max(1, FGrid.TopRow)) then
    begin
      PrepareTextRows(AData);
      SynchronizePaneRows;
    end;
    { Consume geometry only after the admitted layout and copied editor have
      settled. A deferred/refused refresh retains the pending request. }
    FColumnGeometryPending := False;
  finally
    FColumnRendering := nil;
    FColumnApplying := False;
    FUpdating := LUpdating;

    if InputLifetimeCurrent then
    begin
      FGrid.Invalidate;
      ColumnValidationIdle(nil);
      { Last outward call: a logical focus observer can retire this renderer. }
      LOuter.EndColumnTransition;
    end;
  end;
end;

{ The title band is independent of management controls. Capture the caller's
  native minimum once, and include an already installed launcher exactly once.
  Legacy mounts remain unchanged after their original reservation; an explicit
  header can be refreshed from current copied presentation/font observations. }
procedure TNativeMount.ReserveHeader;
var
  LHeader: TNyxTableHeaderPolicy;
  LHeight: Integer;
  LLines: Integer;
begin
  LHeader := Default(TNyxTableHeaderPolicy);

  if FHasTablePresentation then
  begin
    LHeader := FTablePresentation.TablePolicy.HeaderPolicy;
  end;

  if not FQueryHeaderAdjusted then
  begin

    if (FQueries = nil) and not LHeader.Defined and (FColumnToolbarHeight = 0) then
    begin
      Exit;
    end;
    FPreviousHeaderHeight := FGrid.RowHeights[0];
    FQueryHeaderAdjusted := True;
  end
  else if not LHeader.Defined and not FCustomHeaderApplied then
  begin
    Exit;
  end;
  LHeight := FPreviousHeaderHeight;

  if LHeader.Defined then
  begin
    LLines := FHeaderLinePixels;

    if LLines < 1 then
    begin
      { Provisional construction precedes the shared font measurement pass. }
      LLines := Max(1, FPreviousDefaultRowHeight);
    end;
    LHeight := Max(LHeight, LHeader.HeightFor(LLines,
      Max(1, FGrid.Font.PixelsPerInch) / 96));
  end
  else if FQueries <> nil then
  begin
    LHeight := Max(LHeight, FPreviousDefaultRowHeight * 3);
  end;

  if FColumnToolbarApplied then
  begin

    if FColumnToolbarHeight > MaximumNyxLayoutBound - LHeight then
    begin
      LHeight := MaximumNyxLayoutBound;
    end
    else
    begin
      Inc(LHeight, FColumnToolbarHeight);
    end;
  end;
  FCustomHeaderApplied := LHeader.Defined;
  FGrid.RowHeights[0] := LHeight;
end;

procedure TNativeMount.SynchronizePaneRows;
var
  LUpdate: INyxLCLLogicalUpdate;
begin

  if (FStartPane = nil) and (FEndPane = nil) then
  begin
    Exit;
  end;

  if FGrid is TNyxCollectionStringGrid then
  begin
    LUpdate := TNyxCollectionStringGrid(FGrid).BeginLogicalUpdate;
  end;
  try
    SynchronizeLogicalPaneRows;
  finally

    if LUpdate <> nil then
    begin
      LUpdate.Close;
    end;
  end;
end;

procedure TNativeMount.SynchronizeLogicalPaneRows;
var
  LIndex: Integer;
  LFirst: Integer;
  LLast: Integer;
  LUpdating: Boolean;

  procedure Synchronize(APane: TColumnPane);
  var
    LRow: Integer;
    LIndex: Integer;
    LCount: Integer;
    LRows: array of Integer;
  begin

    if (APane = nil) or not APane.Grid.Visible then
    begin
      Exit;
    end;
    APane.Grid.DefaultRowHeight := FGrid.DefaultRowHeight;
    APane.Grid.RowHeights[0] := FGrid.RowHeights[0];
    { Retire only the previous explicit window. Default-height assignment may
      be a no-op, so it cannot be relied on to erase old wrapped overrides. }
    for LIndex := 0 to APane.Grid.ProjectionRowCount - 1 do
    begin
      LRow := APane.Grid.ProjectionRow(LIndex);

      if (LRow > 0) and (LRow < APane.Grid.RowCount) then
      begin
        APane.Grid.RowHeights[LRow] := FGrid.DefaultRowHeight;
      end;
    end;
    { Only currently visible rows need exact nondefault heights. The outer grid
      remains the source of row geometry; panes never infer independent pages. }
    SetLength(LRows, Max(0, LLast - LFirst + 1));
    LCount := 0;
    for LRow := LFirst to LLast do
    begin
      APane.Grid.RowHeights[LRow] := FGrid.RowHeights[LRow];

      if (LRow > 0) and (FGrid.RowHeights[LRow] <> FGrid.DefaultRowHeight) then
      begin
        LRows[LCount] := LRow;
        Inc(LCount);
      end;
    end;
    SetLength(LRows, LCount);
    APane.Grid.ConfigureProjectionRows(LRows);
    APane.Grid.TopRow := FGrid.TopRow;
    APane.Grid.Invalidate;
  end;

begin

  if not GetConnected or FPaneSync or (FGrid = nil) then
  begin
    Exit;
  end;
  FPaneSync := True;
  LUpdating := FUpdating;
  FUpdating := True;
  try
    LIndex := Max(1, FGrid.TopRow);
    LFirst := Min(LIndex, FGrid.RowCount - 1);
    LLast := Min(FGrid.RowCount - 1, LFirst + FGrid.VisibleRowCount + 1);
    Synchronize(FStartPane);
    Synchronize(FEndPane);
  finally
    FUpdating := LUpdating;
    FPaneSync := False;
  end;
end;

procedure TNativeMount.GridHeaderSized(Sender: TObject; AIsColumn: Boolean;
  AIndex: Integer);
var
  LKeepAlive: INyxCollectionMount;
  LGrid: TStringGrid;
  LColumn, LSerial: Integer;
  LScale, LWidth: Double;
  LRequest: TNativeColumnLayoutRequest;
  LRequestLease: IInterface;
begin
  LKeepAlive := Self as INyxCollectionMount;
  LRequest := FResizeRequest;
  LRequestLease := FResizeRequestLease;

  if ColumnResizeInterrupted(LRequest) then
  begin
    RetireColumnResize(LRequest);
  end;

  if (FHasColumnLayout or FHasTablePresentation) and not GetInputReady then
  begin
    Exit;
  end;

  if not GetConnected or FUpdating or FColumnApplying then
  begin
    Exit;
  end;
  LGrid := InputGrid(Sender);

  if (LGrid = nil) or not AIsColumn then
  begin
    Exit;
  end;
  LColumn := CanonicalColumn(LGrid, AIndex);
  LSerial := FArrangedRevision;

  if FLayoutGesture and ColumnResizeCurrent(LRequest, False) and
    (LRequest.Action = nclaResize) and LRequest.Allowed and (LRequest.Capture <> nil) and
    (LRequest.Target = LGrid) and (LRequest.Column = LColumn) and
    (LRequest.Physical = AIndex) and (AIndex >= 0) and (AIndex < LGrid.ColCount) and
    (LRequest.ReleaseStamp > 0) and (LRequest.ReleaseStamp < High(Integer)) and
    (TNyxCollectionStringGrid(LGrid).ColumnReleaseStamp = LRequest.ReleaseStamp) then
  begin

    if (LGrid.Font.PixelsPerInch <> LRequest.PPI) or
      ((LGrid is TNyxCollectionStringGrid) and
      (TNyxCollectionStringGrid(LGrid).ProjectionEpoch <> LRequest.ProjectionEpoch)) then
    begin
      RetireColumnResize(LRequest);
    end
    else
    begin
      LScale := 1;

      if LRequest.PPI > 0 then
      begin
        LScale := LRequest.PPI / 96;
      end;
      LScale := LScale * LRequest.ContentScale;
      LWidth := Double(LGrid.ColWidths[AIndex]) / LScale;
      LRequest.Width := Round(Max(LRequest.Minimum, Min(LRequest.Maximum, LWidth)));
      LRequest.Pending := True;
      FResizeWidth := LRequest.Width;
      FResizePending := True;
    end;
  end;

  if Assigned(FPreviousHeaderSized) then
  begin
    FPreviousHeaderSized(FGrid, True, LColumn);
  end;

  if GetConnected then
  begin
    { A prior header receiver may replace geometry while keeping this mount.
      Retire the copied measurement rather than reading its new physical map. }

    if LSerial <> FArrangedRevision then
    begin
      RetireColumnResize(LRequest);
    end;
    { LCL reports final width here, but the subclass's after-inherited input
      activity fence owns the queue turn, including nested message pumping. }
    ColumnValidationIdle(nil);
  end;
end;

procedure TNativeMount.PublishColumnLayout(AData: PtrInt);
var
  LKeepAlive: INyxCollectionMount;
  LRequest: TNativeColumnLayoutRequest;
  LRequestLease: IInterface;
  LPreferences: TNyxColumnPreferences;
  LPreference: TNyxColumnPreference;
  LPrepared, LFence: INyxPreparedPublication;
  LMeasurement: INyxColumnFitMeasurement;
  LIndex: Integer;
  LReconcile: Boolean;

  procedure RequireCurrent;
  begin

    if not ColumnResizeCurrent(LRequest, True) then
    begin
      raise ENyxCollection.Create('Native resize no longer owns its original input and column');
    end;
  end;

begin
  LKeepAlive := Self as INyxCollectionMount;
  LRequest := FResizeRequest;
  LRequestLease := FResizeRequestLease;

  if not GetConnected or (LRequest = nil) or LRequest.Publishing or
    (AData <> LRequest.Serial) then
  begin
    Exit;
  end;
  FResizeQueued := False;

  if FUpdating then
  begin
    { This queued call may have been pumped inside a programmatic Refresh or
      editor callback. No later native Leave is guaranteed to wake it again. }
    ArmCellRefreshIdle;
    Exit;
  end;
  for LIndex := Low(LRequest.Activities) to High(LRequest.Activities) do
  begin

    if (LRequest.Activities[LIndex] <> nil) and LRequest.Activities[LIndex].GetActive then
    begin
      ArmCellRefreshIdle;
      Exit;
    end;
  end;

  if ColumnResizeInterrupted(LRequest) then
  begin
    RetireColumnResize(LRequest);
  end;
  LReconcile := False;
  LRequest.Publishing := True;
  try
    { Retired cleanup is admitted BEFORE Ready: disable/suspend may have kept
      MouseUp out of the target. Never revive it by recapturing current input. }

    if LRequest.Retired or not LRequest.Allowed or not LRequest.Pending or
      (LRequest.Capture = nil) or (LRequest.Columns = nil) then
    begin
      Exit;
    end;
    FLayoutGesture := False;
    FResizePending := False;
    FResizeReleased := False;
    try
      RequireCurrent;
      LFence := TNativeColumnLayoutFence.Create(Self, LRequest, LRequestLease);

      if LRequest.Action = nclaFit then
      begin
        LPrepared := PrepareColumnFit(LRequest.Capture, LRequest.Field, LMeasurement);
        RequireCurrent;
        { Known outward command observation precedes the nested Fit metric's
          last UI validation. Both final fences retain ORIGINAL launch facts. }
        PublishNyxGroup([LFence, LPrepared]);
      end
      else
      begin
        LPreferences := LRequest.Capture.Preferences;
        RequireCurrent;
        LIndex := LPreferences.IndexOf(LRequest.Field);
        LPreference := NyxColumnPreference(LRequest.Field);

        if LIndex >= 0 then
        begin
          LPreference := LPreferences.ColumnAt(LIndex);
        end;
        LPreferences := LPreferences.WithColumn(LPreference.Width(LRequest.Width));
        LPrepared := LRequest.Columns.PrepareColumns(LRequest.Capture, LPreferences);
        RequireCurrent;
        PublishNyxGroup([LPrepared, LFence]);
      end;
    except
      on LException: Exception do
      begin
        { Group notification errors describe committed width; Failed preserves
          that phase. The request is never replayed after any attempted drain. }
        Failed(LException);
      end;
    end;
  finally
    LRequest.Retired := True;

    if FResizeRequest = LRequest then
    begin
      { Revoke the exact slot/timing BEFORE any ordinary repaint. A notification
        may have started a newer gesture, whose flags and lease stay untouched. }
      FResizeRequest := nil;
      FResizeRequestLease := nil;
      FResizeCapture := nil;
      FResizePending := False;
      FLayoutGesture := False;
      FResizeReleased := False;
      FResizeQueued := False;
      LReconcile := GetConnected;
    end;

    if LReconcile and (FResizeRequest = nil) and GetConnected then
    begin
      { Accepted geometry only, without data edits or manufactured native input.
        A suspended target keeps its dormant draft and resumes through Resume. }
      Refresh;
    end;
  end;
end;

procedure TNativeMount.GridMouseUp(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LKeepAlive: INyxCollectionMount;
  LGrid: TStringGrid;
  LPoint: TPoint;
  LRequest: TNativeColumnLayoutRequest;
  LRequestLease: IInterface;
begin
  LKeepAlive := Self as INyxCollectionMount;
  LRequest := FResizeRequest;
  LRequestLease := FResizeRequestLease;

  if ColumnResizeInterrupted(LRequest) then
  begin
    RetireColumnResize(LRequest);
  end;

  if (FHasColumnLayout or FHasTablePresentation) and not GetInputReady then
  begin
    Exit;
  end;

  if not GetConnected then
  begin
    Exit;
  end;
  LGrid := InputGrid(Sender);

  if LGrid = nil then
  begin
    Exit;
  end;
  LPoint := FGrid.ScreenToClient(LGrid.ClientToScreen(Point(AX, AY)));

  if FLayoutGesture and (LRequest <> nil) and (FResizeRequest = LRequest) then
  begin
    { Intent only: the subclass's AFTER-inherited idle wake owns queueing.
      The retained release stamp distinguishes a later HeaderSized in THIS
      LCL tail from a nested newer same-column gesture in a prior callback. }
    LRequest.ReleaseStamp := TNyxCollectionStringGrid(LGrid).ColumnReleaseStamp;
    FResizeReleased := True;
  end;

  if Assigned(FPreviousMouseUp) then
  begin
    FPreviousMouseUp(FGrid, AButton, AShift, LPoint.X, LPoint.Y);
  end;
end;

procedure TNativeMount.PaneMouseMove(Sender: TObject; AShift: TShiftState;
  AX, AY: Integer);
var
  LGrid: TStringGrid;
  LReply: TMouseMoveEvent;
  LPoint: TPoint;
begin

  if (FHasColumnLayout or FHasTablePresentation) and not GetInputReady then
  begin
    Exit;
  end;
  LGrid := InputGrid(Sender);

  if LGrid = nil then
  begin
    Exit;
  end;
  LPoint := FGrid.ScreenToClient(LGrid.ClientToScreen(Point(AX, AY)));
  LReply := FPreviousMouseMove;

  if Assigned(LReply) then
  begin
    LReply(FGrid, AShift, LPoint.X, LPoint.Y);
  end;
end;

procedure TNativeMount.PaneMouseEnter(Sender: TObject);
var
  LReply: TNotifyEvent;
begin

  if (FHasColumnLayout or FHasTablePresentation) and not GetInputReady then
  begin
    Exit;
  end;

  if not GetConnected or FPointerInside then
  begin
    Exit;
  end;
  FPointerInside := True;
  LReply := FPreviousMouseEnter;

  if Assigned(LReply) then
  begin
    LReply(FGrid);
  end;
end;

procedure TNativeMount.PaneMouseLeave(Sender: TObject);
var
  LReply: TNotifyEvent;
  LPoint: TPoint;
begin

  if (FHasColumnLayout or FHasTablePresentation) and not GetInputReady then
  begin
    Exit;
  end;

  if not GetConnected then
  begin
    Exit;
  end;
  LPoint := FGrid.ScreenToClient(Mouse.CursorPos);

  if PtInRect(FGrid.ClientRect, LPoint) then
  begin
    { Crossing a child pane boundary remains inside the same logical table. }
    Exit;
  end;
  FPointerInside := False;
  LReply := FPreviousMouseLeave;

  if Assigned(LReply) then
  begin
    LReply(FGrid);
  end;
end;

procedure TNativeMount.PaneKeyUp(Sender: TObject; var AKey: Word;
  AShift: TShiftState);
var
  LReply: TKeyEvent;
begin

  if (FHasColumnLayout or FHasTablePresentation) and not GetInputReady then
  begin
    AKey := 0;
    Exit;
  end;

  if not GetConnected then
  begin
    AKey := 0;
    Exit;
  end;
  LReply := FPreviousKeyUp;

  if Assigned(LReply) then
  begin
    LReply(FGrid, AKey, AShift);
  end;
end;

procedure TNativeMount.PaneEnter(Sender: TObject);
var
  LReply: TNotifyEvent;
begin

  if (FHasColumnLayout or FHasTablePresentation) and not GetInputReady then
  begin
    Exit;
  end;

  if not GetConnected then
  begin
    Exit;
  end;
  FActiveGrid := InputGrid(Sender);
  LReply := FPreviousEnter;

  if Assigned(LReply) then
  begin
    LReply(FGrid);
  end;
end;

procedure TNativeMount.PaneExit(Sender: TObject);
var
  LReply: TNotifyEvent;
begin

  if (FHasColumnLayout or FHasTablePresentation) and not GetInputReady then
  begin
    Exit;
  end;

  if not GetConnected then
  begin
    Exit;
  end;
  LReply := FPreviousExit;

  if Assigned(LReply) then
  begin
    LReply(FGrid);
  end;
end;

procedure TNativeMount.PaneClick(Sender: TObject);
var
  LReply: TNotifyEvent;
begin

  if FUpdating then
  begin
    { LCL Col/Row setters can invoke Click during internal cursor restoration.
      They are part of this admitted update, not another application command. }
    Exit;
  end;

  if (FHasColumnLayout or FHasTablePresentation) and not GetInputReady then
  begin
    Exit;
  end;

  if not GetConnected then
  begin
    Exit;
  end;
  LReply := FPreviousClick;

  if Assigned(LReply) then
  begin
    LReply(FGrid);
  end;
end;

procedure TNativeMount.ColumnDoubleClick(Sender: TObject; AStamp: Integer);
var
  LKeepAlive: INyxCollectionMount;
  LRequest: TNativeColumnLayoutRequest;
  LLease: IInterface;
  LReply: TNotifyEvent;
begin
  LKeepAlive := Self as INyxCollectionMount;
  LRequest := FResizeRequest;
  LLease := FResizeRequestLease;

  if not ((LRequest <> nil) and (LRequest.Action = nclaFit) and
    (LRequest.Target = Sender) and (LRequest.DoubleStamp = AStamp) and
    (AStamp > 0) and (AStamp < High(Integer))) then
  begin
    Exit;
  end;

  if not ColumnResizeCurrent(LRequest, False) then
  begin
    RetireColumnResize(LRequest);
    Exit;
  end;

  if LRequest.Allowed and (LRequest.Capture <> nil) then
  begin
    LRequest.Pending := True;
    { This is still a physical button-down. GridMouseUp owns release; the
      native tail/idle will publish only after that actual pointer release. }
  end
  else
  begin
    RetireColumnResize(LRequest);
  end;
  LReply := FPreviousDoubleClick;
  try

    if GetInputReady and Assigned(LReply) then
    begin
      { The canonical outer callback occurs exactly once. It can retire/reenter;
        the request keeps its original proof and no widget is borrowed afterward. }
      LReply(FGrid);
    end;
  except
    { A later physical MouseUp must not accept an intention whose canonical
      callback failed. A nested newer request keeps its own flags and lease. }
    RetireColumnResize(LRequest);
    ArmCellRefreshIdle;
    raise;
  end;
end;

procedure TNativeMount.PaneDoubleClick(Sender: TObject);
var
  LReply: TNotifyEvent;
begin

  if (FHasColumnLayout or FHasTablePresentation) and not GetInputReady then
  begin
    Exit;
  end;

  if not GetConnected then
  begin
    Exit;
  end;
  LReply := FPreviousDoubleClick;

  if Assigned(LReply) then
  begin
    LReply(FGrid);
  end;
end;

procedure TNativeMount.SynchronizeViewportMetrics;
var
  LUpdate: INyxLCLLogicalUpdate;
begin

  if FGrid is TNyxCollectionStringGrid then
  begin
    LUpdate := TNyxCollectionStringGrid(FGrid).BeginLogicalUpdate;
  end;
  try
    { One world frame owns the text window, matching pinned heights and header
      actions. Closing the row helper first would let button setters overwrite
      an already projected face. Nested helpers reuse this same section. }
    PrepareTextRows(FRendered);
    SynchronizePaneRows;
    SynchronizeFitButtons;
  finally

    if LUpdate <> nil then
    begin
      LUpdate.Close;
    end;
  end;
end;

procedure TNativeMount.GridTopLeft(Sender: TObject);
var
  LKeepAlive: INyxCollectionMount;
  LGrid: TStringGrid;
begin

  if (FGrid is TNyxCollectionStringGrid) and
    TNyxCollectionStringGrid(FGrid).ContentProjectionChanging then
  begin
    Exit;
  end;

  if not GetConnected or FPaneSync or FUpdating or FTextLayoutApplying then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LGrid := InputGrid(Sender);

  if LGrid = nil then
  begin
    Exit;
  end;

  if LGrid <> FGrid then
  begin
    { A pane's cursor reveal is a request to the one vertical authority. Its
      resulting outer callback owns notification and synchronization. }
    FGrid.TopRow := LGrid.TopRow;

    if GetConnected then
    begin
      SynchronizeViewportMetrics;
    end;
    Exit;
  end;

  if Assigned(FPreviousTopLeft) then
  begin
    FPreviousTopLeft(FGrid);
  end;

  if GetConnected then
  begin
    { The previous native observer above still sees the physical frame. Only
      Nyx's subsequent metric work opens a temporary logical section. }
    SynchronizeViewportMetrics;
  end;
end;

function TNativeMount.TreeRows: INyxTreeRows;
var
  LHierarchy: INyxTreeHierarchy;
begin
  LHierarchy := NyxTreeHierarchy(FView);

  if (FTreeRows = nil) or (FTreeRows.Snapshot <> FView.Snapshot) or
    (FTreeRows.CommandSerial <> LHierarchy.CommandSerial) then
  begin
    FTreeRows := NyxTreeRows(FView);
  end;
  Result := FTreeRows;
end;

constructor TNativeMount.Create(AControl: TControl; const AView: INyxCollectionView;
  const ALimits: TNyxTableTextWorkLimits;
  AInputPhase: TNyxCollectionInputPhase;
  const AInputGate: INyxCollectionInputGate;
  const AInputFrame: INyxNativeGestureFrame;
  const AInputEvents: INyxEvents; const AOriginID: TNyxText;
  AColumnClick: TNyxLCLColumnClick; ACellContext: TNyxLCLCellContextDispatch;
  ARangePointer: TNyxLCLRangePointerDispatch; ACaptureHandler: TNyxLCLCaptureHandler);
var
  LIndex: Integer;
  {$ifdef NYX_STUDIO_PROFILE}LHandleStarted: QWord;{$endif}
begin
  ALimits.Validate;
  inherited Create(AView, AInputPhase, AInputGate);
  FTextLimits := ALimits;

  if FHasTablePresentation and not (AControl is TNyxCollectionStringGrid) then
  begin
    raise ENyxCollection.Create('Table presentation requires the Nyx native grid');
  end;

  if FHasColumnLayout or FHasTablePresentation then
  begin

    if (AInputFrame = nil) or (AInputEvents = nil) or (AOriginID = '') then
    begin
      raise ENyxCollection.Create('Native table layout requires its real event router, physical frame and runtime origin');
    end;

    if not NyxLCLTextHistorySupported then
    begin
      { The existing capability identifies the admitted Win32 editing/IME
        bridge. Observer.Ready alone also reports its unobserved fallback on
        other widgetsets, which cannot promise structural draft continuity. }
      raise ENyxCollection.Create('Column arrangement requires the supported native editing bridge');
    end;

    if not (AControl is TNyxCollectionStringGrid) then
    begin
      raise ENyxCollection.Create('Column arrangement requires the Nyx native table grid');
    end;
  end;

  if FHasColumnLayout or ((AControl is TNyxCollectionStringGrid) and
    (AInputFrame <> nil) and (AInputEvents <> nil) and (AOriginID <> '') and
    NyxLCLTextHistorySupported) then
  begin
    { Observation belongs to renderer-managed editable tables independently of
      arrangement authoring. Attach before an editor can begin composition; a
      later copy request must not guess an unobserved IME history. Raw caller
      mounts lacking this supplied context retain their original behavior. }
    FColumnFrame := AInputFrame;
    FColumnClick := AColumnClick;
    FCellContextDispatch := ACellContext;
    FRangePointerDispatch := ARangePointer;
    FRangeEvents := AInputEvents;

    FColumnEditing := TColumnEditing.Create(GetInputLifetime, AInputEvents,
      AOriginID, CellCompositionEnded, ACaptureHandler, AInputFrame);
    FInputGrid := TNyxCollectionStringGrid(AControl);
    FInputGrid.AttachColumnInput(GetInputLifetime, FColumnFrame,
      FColumnEditing.Continuity);
    FInputGrid.ConfigureCellContext(GetInputLifetime, BeginCellContext, FCellContextDispatch);
    FInputGrid.ConfigureRangePointer(GetInputLifetime, PrepareRangePointer,
      FRangePointerDispatch, CancelRangeSurface);
    FInputGridAttached := True;
    FColumnEditing.Add(FInputGrid);
  end;
  { LCL may establish its initial cell/cursor while creating a widget handle.
    Admit that native initialization before installing selection observers; it
    must not turn an empty portable selection into a user selection. The mounted
    control already belongs to its host, including a hidden renderer candidate. }

  {$ifdef NYX_STUDIO_PROFILE}LHandleStarted := GetTickCount64;{$endif}

  if AControl is TWinControl then
  begin
    TWinControl(AControl).HandleNeeded;
  end;
  {$ifdef NYX_STUDIO_PROFILE}
  { HandleNeeded may initialize the surrounding native tree as well as this
    collection. This is inclusive elapsed time, not one widget's CPU cost.
    Report fixed adapter phases only, without control identity or user data. }
  WriteLn('native-mount,handle,', GetTickCount64 - LHandleStarted);
  LHandleStarted := GetTickCount64;
  {$endif}
  case AView.Projection of
    cpList:
      begin

        if not (AControl is TListBox) then
        begin
          raise ENyxCollection.Create('List binding requires a native list box');
        end;
        FList := TListBox(AControl);
        FPreviousSelection := FList.OnSelectionChange;
        FList.OnSelectionChange := ListSelected;
        FPreviousKey := FList.OnKeyDown;
        FList.OnKeyDown := KeyDown;
        FPreviousUTF8 := FList.OnUTF8KeyPress;
        FList.OnUTF8KeyPress := UTF8KeyPress;
        FPreviousMultiSelect := FList.MultiSelect;
        FPreviousExtendedSelect := FList.ExtendedSelect;
        FList.MultiSelect := AView.Spec.SelectionMode = nsmMultiple;
        FList.ExtendedSelect := True;
      end;
    cpTable:
      begin

        if not (AControl is TStringGrid) then
        begin
          raise ENyxCollection.Create('Table binding requires a native string grid');
        end;
        FGrid := TStringGrid(AControl);
        FPreviousMouseWheelOption := FGrid.MouseWheelOption;

        if AView.Spec.HasColumnFormat and not (FGrid is TNyxCollectionStringGrid) then
        begin
          raise ENyxCollection.Create('Display formatting requires the native Nyx collection grid');
        end;

        if FHasColumnLayout and not (FGrid is TNyxCollectionStringGrid) then
        begin
          raise ENyxCollection.Create('Column arrangement requires the Nyx native table grid');
        end;
        FFocusedCanonical := -1;
        FPreviousGridHint := FGrid.Hint;
        FPreviousGridShowHint := FGrid.ShowHint;
        FPreviousCellHint := FGrid.OnGetCellHint;
        FPreviousDefaultRowHeight := FGrid.DefaultRowHeight;
        FPreviousCell := FGrid.OnSelectCell;
        FPreviousValidate := FGrid.OnValidateEntry;
        FPreviousOptions := FGrid.Options;
        { LCL defaults to moving the active cell on wheel input. Bound tables
          follow browser table semantics instead: wheel browsing scrolls the
          viewport and leaves selection/unfinished edits to explicit commands.
          Keep LCL's ordinary event/default path so callbacks can still consume
          the request. This setting is restored when the borrowed grid detaches. }
        FGrid.MouseWheelOption := mwGrid;
        FGrid.OnSelectCell := GridSelected;
        FGrid.OnValidateEntry := GridEdited;
        FPreviousKey := FGrid.OnKeyDown;
        FPreviousMouse := FGrid.OnMouseDown;
        FPreviousHeaderClick := FGrid.OnHeaderClick;
        FPreviousPrepare := FGrid.OnPrepareCanvas;
        FPreviousGridResize := FGrid.OnResize;
        FGrid.OnResize := GridResized;

        if FGrid is TNyxCollectionStringGrid then
        begin
          TNyxCollectionStringGrid(FGrid).ObserveContentProjection(GridProjectionChanged);
        end;
        FGrid.OnKeyDown := KeyDown;
        FGrid.OnMouseDown := GridMouseDown;
        FGrid.OnHeaderClick := GridHeaderClicked;
        FGrid.OnPrepareCanvas := PrepareGrid;

        if FHasColumnLayout or FHasTablePresentation then
        begin
          FPreviousTopLeft := FGrid.OnTopLeftChanged;
          FGrid.OnTopLeftChanged := GridTopLeft;
        end;

        if FHasColumnLayout then
        begin
          FPreviousHeaderSized := FGrid.OnHeaderSized;
          FPreviousMouseUp := FGrid.OnMouseUp;
          FPreviousMouseMove := FGrid.OnMouseMove;
          FPreviousMouseEnter := FGrid.OnMouseEnter;
          FPreviousMouseLeave := FGrid.OnMouseLeave;
          FPreviousKeyUp := FGrid.OnKeyUp;
          FPreviousEnter := FGrid.OnEnter;
          FPreviousExit := FGrid.OnExit;
          FPreviousClick := FGrid.OnClick;
          FPreviousDoubleClick := FGrid.OnDblClick;
          FGrid.OnTopLeftChanged := GridTopLeft;
          FGrid.OnHeaderSized := GridHeaderSized;
          FGrid.OnMouseUp := GridMouseUp;
          FGrid.OnMouseMove := PaneMouseMove;
          FGrid.OnMouseEnter := PaneMouseEnter;
          FGrid.OnMouseLeave := PaneMouseLeave;
          FGrid.OnKeyUp := PaneKeyUp;
          FGrid.OnEnter := PaneEnter;
          FGrid.OnExit := PaneExit;
          FGrid.OnClick := PaneClick;
          FGrid.OnDblClick := PaneDoubleClick;
        end;

        if FGrid is TNyxCollectionStringGrid then
        begin
          TNyxCollectionStringGrid(FGrid).AttachReader(GridText);

          if AView.Spec.HasColumnFormat then
          begin
            TNyxCollectionStringGrid(FGrid).AttachDisplayReader(GridDisplay);
          end;
          FGridReaderAttached := True;

          if FHasTablePresentation then
          begin
            TNyxCollectionStringGrid(FGrid).ObserveTextLayout(GridTextLayout);
            TNyxCollectionStringGrid(FGrid).ObserveCellAlignment(GridAlignment);
            FTextLayoutAttached := True;
          end;

          if FHasColumnLayout then
          begin
            TNyxCollectionStringGrid(FGrid).AttachEditReader(MainEditable);
            TNyxCollectionStringGrid(FGrid).ObserveValidationIdle(ColumnValidationIdle);
            TNyxCollectionStringGrid(FGrid).ObserveColumnDoubleClick(ColumnDoubleClick);
            TNyxCollectionStringGrid(FGrid).ObserveColumnEditor(ColumnEditorShown);
          end;
        end;
        for LIndex := 0 to AView.Spec.Count - 1 do
        begin

          if AView.Spec.ColumnAt(LIndex).Mode = cmEditable then
          begin
            FGrid.Options := FGrid.Options + [goEditing];
          end;
        end;
      end;
    cpTree:
      begin

        if not (AControl is TTreeView) then
        begin
          raise ENyxCollection.Create('Tree binding requires a native tree view');
        end;
        FTree := TTreeView(AControl);
        FPreviousTreeChange := FTree.OnChange;
        FPreviousTreeEdit := FTree.OnEdited;
        FPreviousTreeExpanded := FTree.OnExpanded;
        FPreviousTreeCollapsed := FTree.OnCollapsed;
        FPreviousReadOnly := FTree.ReadOnly;
        FTree.OnChange := TreeSelected;
        FTree.OnEdited := TreeEdited;
        FTree.OnExpanded := TreeExpanded;
        FTree.OnCollapsed := TreeCollapsed;
        FTree.ReadOnly := AView.Spec.ColumnAt(0).Mode <> cmEditable;
        FPreviousKey := FTree.OnKeyDown;
        FTree.OnKeyDown := KeyDown;
        FPreviousUTF8 := FTree.OnUTF8KeyPress;
        FTree.OnUTF8KeyPress := UTF8KeyPress;
        FPreviousTreeSelection := FTree.OnSelectionChanged;
        FTree.OnSelectionChanged := TreeSelectionChanged;
        FPreviousMultiSelect := FTree.MultiSelect;
        FPreviousMultiStyle := FTree.MultiSelectStyle;
        FTree.MultiSelect := AView.Spec.SelectionMode = nsmMultiple;
        FTree.MultiSelectStyle := [msControlSelect, msShiftSelect, msVisibleOnly];
      end;
  end;
  {$ifdef NYX_STUDIO_PROFILE}
  WriteLn('native-mount,configure,', GetTickCount64 - LHandleStarted);
  {$endif}
end;

function TNativeMount.DisplayText(const AValue: TNyxText): TNyxText;
begin
  Result := NyxLCLColumnDisplayText(AValue);
end;

procedure TNativeMount.PrepareTablePresentation;
begin

  if FTextLayoutAttached then
  begin
    Exit;
  end;

  if not (FGrid is TNyxCollectionStringGrid) or not FInputGridAttached or
    (FColumnFrame = nil) or (FColumnEditing = nil) or
    not NyxLCLTextHistorySupported then
  begin
    raise ENyxCollection.Create('Runtime table presentation requires the observed Nyx native grid and its real input frame');
  end;

  if not FHasColumnLayout then
  begin
    FPreviousTopLeft := FGrid.OnTopLeftChanged;
    FGrid.OnTopLeftChanged := GridTopLeft;
  end;
  TNyxCollectionStringGrid(FGrid).ObserveTextLayout(GridTextLayout);
  TNyxCollectionStringGrid(FGrid).ObserveCellAlignment(GridAlignment);
  FTextLayoutAttached := True;
end;

function TNativeMount.EditText(const AItem: TNyxItemRef; AColumn: Integer;
  const AValue: TNyxText): TNyxText;
begin
  Result := AValue;

  if (FView.Spec.ColumnAt(AColumn).Kind = nskText) and
    (Pos(#0, FView.CellText(AItem, AColumn)) > 0) then
  begin
    Result := TNyxDataValue.ParseJSON(AValue).AsText;
  end;
end;

procedure TNativeMount.ListSelected(Sender: TObject; User: Boolean);
var
  LKeepAlive: INyxCollectionMount;
  LItems: TNyxItemRefs;
  LFocus: TNyxItemRef;
  LAnchor: TNyxItemRef;
  LIndex: Integer;
  LCount: Integer;
begin

  if FUpdating or FEditsSuspended or not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if not FEnabled then
  begin
    Refresh;
    Exit;
  end;

  if FView.Spec.SelectionMode = nsmMultiple then
  begin
    SetLength(LItems, FView.Snapshot.Count);
    LCount := 0;
    LFocus := Default(TNyxItemRef);
    for LIndex := 0 to FView.Snapshot.Count - 1 do
    begin

      if FList.Selected[LIndex] then
      begin
        LItems[LCount] := FView.Snapshot.ItemAt(LIndex).Ref;
        Inc(LCount);
      end;
    end;
    SetLength(LItems, LCount);

    if (FList.ItemIndex >= 0) and (FList.ItemIndex < FView.Snapshot.Count) then
    begin
      LFocus := FView.Snapshot.ItemAt(FList.ItemIndex).Ref;
    end;
    LAnchor := FView.Selection.Anchor;

    if not (ssShift in GetKeyShiftState) then
    begin
      LAnchor := LFocus;
    end;
    SetSelection(LItems, LFocus, LAnchor);
  end
  else if (FList.ItemIndex >= 0) and (FList.ItemIndex < FView.Snapshot.Count) then
  begin
    Select(FView.Snapshot.ItemAt(FList.ItemIndex).Ref);
  end
  else
  begin
    FView.ClearSelection;
  end;

  if GetConnected and Assigned(FPreviousSelection) then
  begin
    FPreviousSelection(Sender, User);
  end;
end;

procedure TNativeMount.GridSelected(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var
  LKeepAlive: INyxCollectionMount;
  LGrid: TStringGrid;
  LView: INyxCollectionView;
  LData, LCurrent: INyxCollectionSnapshot;
  LItem: TNyxItemRef;
  LColumn, LRevision, LCount: Integer;
  LShift: TShiftState;
begin

  if (Sender is TNyxCollectionStringGrid) and
    TNyxCollectionStringGrid(Sender).InitializingHandle then
  begin
    { LCL CheckPosition runs while a lazily shown/recreated grid acquires its
      handle. Accept its local cursor adjustment without publishing a row
      selection or replacing the remembered canonical column with a pane's
      physical column zero. Ordinary later pointer/key selection stays intact. }
    Exit;
  end;

  if (FGrid is TNyxCollectionStringGrid) and
    TNyxCollectionStringGrid(FGrid).ContentProjectionChanging then
  begin
    { Geometry normalization may move the native cursor, but it is not a new
      portable selection command and must not reenter the collection refresh. }
    Exit;
  end;

  if (FHasColumnLayout or FHasTablePresentation) and not GetInputReady and
    not (FUpdating and not FEditsSuspended) then
  begin
    CanSelect := False;
    Exit;
  end;

  if FEditsSuspended then
  begin
    CanSelect := False;
    Exit;
  end;

  if FUpdating or FEditsSuspended or not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if FHasColumnLayout then
  begin
    LGrid := InputGrid(Sender);
    LColumn := CanonicalColumn(LGrid, ACol);
    LView := FView;
    LData := FRendered;
    LRevision := FArrangedRevision;
    LCount := 0;

    if LData <> nil then
    begin
      LCount := LData.Count;
    end;
    LCurrent := LView.Snapshot;
    LShift := FGridShift;
    FGridShift := [];

    if not GetInputReady or ColumnInputBusy or (FView <> LView) or
      (FArrangedRevision <> LRevision) or (LData = nil) or
      (FRendered <> LData) or (LCurrent <> LData) or
      not FEnabled or (LGrid = nil) or (LColumn < 0) or
      (ARow < 1) or (ARow > LCount) then
    begin
      CanSelect := False;
      Exit;
    end;
    LItem := LData.ItemAt(ARow - 1).Ref;
    LCurrent := LView.Snapshot;

    if not GetInputReady or ColumnInputBusy or (FView <> LView) or
      (LRevision <> FArrangedRevision) or (FRendered <> LData) or
      (LCurrent <> LData) then
    begin
      CanSelect := False;
      Exit;
    end;

    if Assigned(FPreviousCell) then
    begin
      FPreviousCell(FGrid, LColumn, ARow, CanSelect);
    end;
    LCurrent := LView.Snapshot;

    if not GetInputReady or ColumnInputBusy or not CanSelect or
      (LRevision <> FArrangedRevision) or (FView <> LView) or
      (FRendered <> LData) or (LCurrent <> LData) or not FEnabled then
    begin
      CanSelect := False;
      Exit;
    end;
    FActiveGrid := LGrid;
    FFocusedCanonical := LColumn;

    if ssShift in LShift then
    begin
      SelectRange(LItem, VisibleOrder, ssCtrl in LShift);
    end
    else if ssCtrl in LShift then
    begin
      Select(LItem, nsaToggle);
    end
    else
    begin
      Select(LItem);
    end;
    { Portable selection refresh installs its own current cursor. A retired
      positional native selection must not continue over that replacement map. }
    CanSelect := GetInputReady and not ColumnInputBusy and
      (LRevision = FArrangedRevision);
    Exit;
  end;

  if not FEnabled then
  begin
    CanSelect := False;
    Refresh;
    Exit;
  end;

  if Assigned(FPreviousCell) then
  begin
    FPreviousCell(Sender, ACol, ARow, CanSelect);
  end;

  if GetConnected and CanSelect and (ARow > 0) and (ARow <= FView.Snapshot.Count) then
  begin
    { Shared row membership is independent of the grid's cell/editor cursor. }

    if ssShift in FGridShift then
    begin
      SelectRange(FView.Snapshot.ItemAt(ARow - 1).Ref, VisibleOrder,
        ssCtrl in FGridShift);
    end
    else if ssCtrl in FGridShift then
    begin
      Select(FView.Snapshot.ItemAt(ARow - 1).Ref, nsaToggle);
    end
    else
    begin
      Select(FView.Snapshot.ItemAt(ARow - 1).Ref);
    end;
    FGridShift := [];
  end;
end;

procedure TNativeMount.GridEdited(Sender: TObject; ACol, ARow: Integer;
  const OldValue: String; var NewValue: String);
var
  LItem: TNyxItemRef;
  LWire: TNyxText;
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
  LData, LCurrent: INyxCollectionSnapshot;
  LColumn, LRevision: Integer;
  LGrid: TStringGrid;
  LEditable: Boolean;
begin

  if (FHasColumnLayout or FHasTablePresentation) and not GetInputReady then
  begin
    NewValue := OldValue;
    Exit;
  end;

  if FEditsSuspended then
  begin
    NewValue := OldValue;
    Exit;
  end;

  if FUpdating or not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if FHasColumnLayout then
  begin
    LGrid := InputGrid(Sender);
    LColumn := CanonicalColumn(LGrid, ACol);
    LView := FView;
    LData := FRendered;
    LRevision := FArrangedRevision;
    LEditable := CanonicalEditable(LColumn, ARow);
    LCurrent := LView.Snapshot;

    if not GetInputReady or ColumnInputBusy or (FView <> LView) or
      (FArrangedRevision <> LRevision) or (LData = nil) or
      (FRendered <> LData) or (LCurrent <> LData) or
      (LGrid = nil) or not LEditable then
    begin
      NewValue := OldValue;
      Exit;
    end;
    LItem := LData.ItemAt(ARow - 1).Ref;
    LCurrent := LView.Snapshot;

    if not GetInputReady or ColumnInputBusy or (FView <> LView) or
      (LRevision <> FArrangedRevision) or (FRendered <> LData) or
      (LCurrent <> LData) then
    begin
      NewValue := OldValue;
      Exit;
    end;

    if Assigned(FPreviousValidate) then
    begin
      FPreviousValidate(FGrid, LColumn, ARow, OldValue, NewValue);
    end;
    LEditable := CanonicalEditable(LColumn, ARow);
    LCurrent := LView.Snapshot;

    if not GetInputReady or ColumnInputBusy or (LRevision <> FArrangedRevision) or
      (FView <> LView) or (FRendered <> LData) or (LCurrent <> LData) or
      not LEditable then
    begin
      { The grid's managed validation epoch neutralizes the native positional
        tail too. Never reinterpret ACol/ARow after an outward validator. }
      NewValue := OldValue;
      Exit;
    end;
    try
      LWire := EditText(LItem, LColumn, TNyxText(NewValue));

      if not GetInputReady or ColumnInputBusy or
        (LRevision <> FArrangedRevision) or (FRendered <> LData) then
      begin
        NewValue := OldValue;
        Exit;
      end;
      EditCell(LItem, LColumn, LWire);
    except
      on LException: Exception do
      begin
        Failed(LException);
      end;
    end;
    { The retained model is safe after publication retires the physical host.
      No widget or old physical coordinate is read after that command. }

    if LView.Snapshot.Has(LItem) then
    begin
      NewValue := DisplayText(LView.CellText(LItem, LColumn));
    end
    else
    begin
      { Removal/filtering has already retired the native epoch. No accepted
        physical output is needed for that vanished visible item. }
      NewValue := OldValue;
    end;
    Exit;
  end;

  if (ARow < 1) or (ARow > FView.Snapshot.Count) or
    (ACol < 0) or (ACol >= FView.Spec.Count) then
  begin
    NewValue := OldValue;
    Exit;
  end;

  if Assigned(FPreviousValidate) then
  begin
    FPreviousValidate(Sender, ACol, ARow, OldValue, NewValue);
  end;

  if not GetConnected or (ARow > FView.Snapshot.Count) then
  begin
    NewValue := OldValue;
    Exit;
  end;
  { An admitted edit may notify an observer that unmounts the control. Keep its
    view independently for the returned accepted text; never borrow the widget
    again after publication. The callback's value belongs to its caller. }
  LView := FView;
  LItem := LView.Snapshot.ItemAt(ARow - 1).Ref;
  try
    LWire := EditText(LItem, ACol, TNyxText(NewValue));
    EditCell(LItem, ACol, LWire);
  except
    { Malformed escaped text is rejected before reaching the shared wire editor.
      Restore the full accepted display, including supplementary text and NUL. }
    on LException: Exception do
    begin
      Failed(LException);
      NewValue := DisplayText(LView.CellText(LItem, ACol));
      Exit;
    end;
  end;
  NewValue := DisplayText(LView.CellText(LItem, ACol));
end;

function TNativeMount.NodeIndex(ANode: TTreeNode): Integer;
var
  LIndex: Integer;
begin
  Result := -1;

  if (ANode = nil) or (PtrUInt(ANode.Data) < 1) or
    (PtrUInt(ANode.Data) > PtrUInt(Length(FNodes))) then
  begin
    Exit;
  end;
  { Mount-owned nodes carry a checked ordinal, never an object pointer or a
    dataset row reference. Validate the pointer too, so a foreign native node
    cannot impersonate a portable item. Visible traversal is therefore linear. }
  LIndex := Integer(PtrUInt(ANode.Data)) - 1;

  if FNodes[LIndex] = ANode then
  begin
    Result := LIndex;
  end;
end;

procedure TNativeMount.TreeSelected(Sender: TObject; ANode: TTreeNode);
var
  LIndex: Integer;
  LKeepAlive: INyxCollectionMount;
begin

  if FUpdating or not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if FView.Spec.SelectionMode = nsmMultiple then
  begin

    if Assigned(FPreviousTreeChange) then
    begin
      FPreviousTreeChange(Sender, ANode);
    end;
    Exit;
  end;

  if not FEnabled then
  begin
    Refresh;
    Exit;
  end;
  LIndex := NodeIndex(ANode);

  if LIndex >= 0 then
  begin
    Select(FView.Snapshot.ItemAt(LIndex).Ref);
  end
  else
  begin
    FView.ClearSelection;
  end;

  if GetConnected and Assigned(FPreviousTreeChange) then
  begin
    FPreviousTreeChange(Sender, ANode);
  end;
end;

procedure TNativeMount.TreeDisclosure(ANode: TTreeNode; AExpanded: Boolean);
var
  LIndex: Integer;
  LPending: Integer;
  LItem: TNyxItemRef;
  LKeepAlive: INyxCollectionMount;
begin

  if FUpdating or not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if not FEnabled then
  begin
    Refresh;
    Exit;
  end;
  LIndex := NodeIndex(ANode);

  if LIndex >= 0 then
  begin
    LItem := FView.Snapshot.ItemAt(LIndex).Ref;

    if (Length(FDisclosureItems) > 0) and
      (FDisclosureSerial <> NyxTreeHierarchy(FView).CommandSerial) then
    begin
      Application.RemoveAsyncCalls(Self);
      FDisclosureItems := nil;
      FDisclosureValues := nil;
    end;
    for LPending := 0 to Length(FDisclosureItems) - 1 do
    begin

      if FDisclosureItems[LPending].ID = LItem.ID then
      begin
        FDisclosureValues[LPending] := AExpanded;
        Exit;
      end;
    end;
    LPending := Length(FDisclosureItems);
    SetLength(FDisclosureItems, LPending + 1);
    SetLength(FDisclosureValues, LPending + 1);
    FDisclosureItems[LPending] := LItem;
    FDisclosureValues[LPending] := AExpanded;

    if LPending = 0 then
    begin
      FDisclosureSerial := NyxTreeHierarchy(FView).CommandSerial;
      Application.QueueAsyncCall(PublishDisclosure, 0);
    end;
  end;
end;

procedure TNativeMount.PublishDisclosure(AData: PtrInt);
var
  LKeepAlive: INyxCollectionMount;
  LItems: TNyxItemRefs;
  LValues: array of Boolean;
  LIndex: Integer;
begin
  LKeepAlive := Self as INyxCollectionMount;
  LItems := FDisclosureItems;
  LValues := FDisclosureValues;
  FDisclosureItems := nil;
  FDisclosureValues := nil;

  if not GetConnected then
  begin
    Exit;
  end;

  if FDisclosureSerial <> NyxTreeHierarchy(FView).CommandSerial then
  begin
    { Even a silent CollapseAll/SetExpanded/explicit focus supersedes a queued
      host transition. Reconcile physical disclosure without replaying it. }
    Refresh;
    Exit;
  end;
  for LIndex := 0 to Length(LItems) - 1 do
  begin

    if not GetConnected then
    begin
      Exit;
    end;

    if FEnabled and FView.Snapshot.Has(LItems[LIndex]) then
    begin
      { Publication can retire the whole host. No node or tree pointer crosses
        this boundary, and the current mount call frame remains managed. }
      NyxTreeHierarchy(FView).SetExpanded(LItems[LIndex], LValues[LIndex]);
    end;
  end;

  if GetConnected then
  begin
    Refresh;
  end;
end;

procedure TNativeMount.TreeExpanded(Sender: TObject; ANode: TTreeNode);
var
  LKeepAlive: INyxCollectionMount;
begin
  LKeepAlive := Self as INyxCollectionMount;
  { Preserve an existing host callback before publication can retire its node. }

  if not FUpdating and Assigned(FPreviousTreeExpanded) then
  begin
    FPreviousTreeExpanded(Sender, ANode);
  end;

  if GetConnected then
  begin
    TreeDisclosure(ANode, True);
  end;
end;

procedure TNativeMount.TreeCollapsed(Sender: TObject; ANode: TTreeNode);
var
  LKeepAlive: INyxCollectionMount;
begin
  LKeepAlive := Self as INyxCollectionMount;

  if not FUpdating and Assigned(FPreviousTreeCollapsed) then
  begin
    FPreviousTreeCollapsed(Sender, ANode);
  end;

  if GetConnected then
  begin
    TreeDisclosure(ANode, False);
  end;
end;

procedure TNativeMount.TreeEdited(Sender: TObject; ANode: TTreeNode;
  var AText: String);
var
  LIndex: Integer;
  LItem: TNyxItemRef;
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if FUpdating or not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LIndex := NodeIndex(ANode);

  if LIndex < 0 then
  begin
    Exit;
  end;

  if Assigned(FPreviousTreeEdit) then
  begin
    FPreviousTreeEdit(Sender, ANode, AText);
  end;

  if not GetConnected then
  begin
    Exit;
  end;
  LIndex := NodeIndex(ANode);

  if LIndex < 0 then
  begin
    Exit;
  end;
  LView := FView;
  LItem := LView.Snapshot.ItemAt(LIndex).Ref;
  try
    EditCell(LItem, 0, EditText(LItem, 0, TNyxText(AText)));
  except
    on LException: Exception do
    begin
      Failed(LException);
      AText := DisplayText(LView.CellText(LItem, 0));
      Exit;
    end;
  end;
  AText := DisplayText(LView.CellText(LItem, 0));
end;

function TNativeMount.GridText(AColumn, ARow: Integer): TNyxText;
var
  LData: INyxCollectionSnapshot;
begin

  if FHasColumnLayout then
  begin
    Exit(CanonicalText(CanonicalColumn(FGrid, AColumn), ARow));
  end;
  Result := '';

  if (FView = nil) or (AColumn < 0) or (AColumn >= FView.Spec.Count) then
  begin
    Exit;
  end;

  if ARow = 0 then
  begin
    Result := QueryHeader(AColumn);

    if (FQueries <> nil) and
      ((FView.Spec.TableQueryPolicy.FilterEnabled and FView.Spec.ColumnAt(AColumn).QueryPolicy.FilterEnabled) or
       (FView.Spec.TableQueryPolicy.SortEnabled and FView.Spec.ColumnAt(AColumn).QueryPolicy.SortEnabled)) then
    begin
      Result := Result + TNyxText('  ▾');
    end;
    Exit;
  end;
  LData := FView.Snapshot;

  if (ARow > 0) and (ARow <= LData.Count) then
  begin
    Result := DisplayText(FView.CellText(LData.ItemAt(ARow - 1).Ref, AColumn));
  end;
end;

function TNativeMount.MeasurePresentationText(const AText: TNyxText): Double;
begin
  Result := FTextMeasureCanvas.TextWidth(AText);
end;

function TNativeMount.CanonicalTextLayout(AColumn, ARow: Integer;
  out ALayout: TNyxTableTextLayout): Boolean;
var
  LRow: Integer;
begin
  Result := False;
  LRow := ARow - FTextLayoutFirst;

  if (LRow >= 0) and (LRow < Length(FTextLayouts)) and (AColumn >= 0) and
    (AColumn < Length(FTextLayouts[LRow])) and (FTextLayouts[LRow][AColumn].Count > 0) then
  begin
    ALayout := FTextLayouts[LRow][AColumn].Copy;
    Result := True;
  end;

  if not Result and FHasTablePresentation and (ARow > 0) and
    (ARow <= FTextLayoutTotal) and (AColumn >= 0) and
    (AColumn < FTablePresentation.Count) and FTablePresentation.TextAt(AColumn).Defined then
  begin
    ALayout := UnavailableNyxTableTextLayout(nttaCellLimit,
      'Visible cells exceed this host measurement pass budget');
    Result := True;
  end;
end;

function TNativeMount.GridTextLayout(AColumn, ARow: Integer;
  out ALayout: TNyxTableTextLayout): Boolean;
begin
  Result := CanonicalTextLayout(CanonicalColumn(FGrid, AColumn), ARow, ALayout);
end;

function TNativeMount.CanonicalAlignment(AColumn, ARow: Integer;
  out APolicy: TNyxColumnAlignmentPolicy): Boolean;
begin
  APolicy := NyxColumnAlignment;
  Result := InputLifetimeCurrent and FHasTablePresentation and (ARow > 0) and
    (AColumn >= 0) and (AColumn < FTablePresentation.Count);

  if Result then
  begin
    APolicy := FTablePresentation.AlignmentAt(AColumn);
    Result := APolicy.Defined;
  end;
end;

function TNativeMount.GridAlignment(AColumn, ARow: Integer;
  out APolicy: TNyxColumnAlignmentPolicy): Boolean;
begin
  Result := CanonicalAlignment(CanonicalColumn(FGrid, AColumn), ARow, APolicy);
end;

procedure TNativeMount.PrepareTextRows(const AData: INyxCollectionSnapshot);
var
  LUpdate: INyxLCLLogicalUpdate;
begin

  if FGrid is TNyxCollectionStringGrid then
  begin
    LUpdate := TNyxCollectionStringGrid(FGrid).BeginLogicalUpdate;
  end;
  try
    PrepareLogicalTextRows(AData);
  finally

    if LUpdate <> nil then
    begin
      LUpdate.Close;
    end;
  end;
end;

procedure TNativeMount.PrepareLogicalTextRows(const AData: INyxCollectionSnapshot);
var
  LBitmap: TBitmap;
  LLayouts: array of array of TNyxTableTextLayout;
  LHeights: array of Integer;
  LRows: array of Integer;
  LSpec: TNyxCollectionViewSpec;
  LDensity: TNyxTablePresentationPolicy;
  LPolicy: TNyxColumnTextPolicy;
  LPresentation: TNyxValuePresentation;
  LText: TNyxText;
  LFirst: Integer;
  LLast: Integer;
  LRow: Integer;
  LColumn: Integer;
  LPhysical: Integer;
  LLineHeight: Integer;
  LDefault: Integer;
  LPadding: Integer;
  LHeader: Integer;
  LIndex: Integer;
  LGrid: TStringGrid;
  LUpdating: Boolean;
  LFrame: INyxNativeGestureFrame;
  LMeasuredCells: Integer;
  LMetricSupported: Boolean;
  LCount: Integer;
begin

  if not FHasTablePresentation or (FGrid = nil) or (AData = nil) or
    FTextLayoutApplying or not InputLifetimeCurrent then
  begin
    Exit;
  end;
  LFrame := FColumnFrame;

  if LFrame <> nil then
  begin
    LFrame.Enter;
  end;
  LBitmap := nil;
  LUpdating := FUpdating;
  FUpdating := True;
  FTextLayoutApplying := True;
  try

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;
    LSpec := FView.Spec;
    LCount := AData.Count;

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;
    LBitmap := TBitmap.Create;
    LBitmap.SetSize(1, 1);
    { Header fonts may differ from body fonts. Borrow the same measurement
      canvas for the caption metric before restoring it for body text layout. }
    LBitmap.Canvas.Font.Assign(FGrid.TitleFont);
    FHeaderLinePixels := Max(1, LBitmap.Canvas.TextHeight('Ag'));
    LBitmap.Canvas.Font.Assign(FGrid.Font);
    LMetricSupported := (FGrid.Font.Orientation = 0) and
      not FGrid.DefaultTextStyle.SystemFont and not Assigned(FPreviousPrepare) and
      not Assigned(FGrid.OnDrawCell) and FGrid.DefaultDrawing and (FGrid.Columns.Count = 0);
    FTextMeasureCanvas := LBitmap.Canvas;
    LLineHeight := Max(1, LBitmap.Canvas.TextHeight('Ag'));
    LDensity := FTablePresentation.TablePolicy;

    if LDensity.HasHeader or FCustomHeaderApplied then
    begin
      ReserveHeader;

      if not InputLifetimeCurrent then
      begin
        Exit;
      end;
    end;
    LDefault := FPreviousDefaultRowHeight;
    LPadding := Max(0, (LDefault - LLineHeight) div 2);

    if LDensity.Defined then
    begin
      LPadding := Max(0, Round(NyxTableRowPadding(LDensity.RowDensity) *
        Max(1, FGrid.Font.PixelsPerInch) / 96));
      LDefault := LLineHeight + 2 * LPadding;
    end;

    if FGrid is TNyxCollectionStringGrid then
    begin

      if LDensity.Defined then
      begin
        TNyxCollectionStringGrid(FGrid).ConfigureAlignmentVerticalInset(LPadding);
      end
      else
      begin
        TNyxCollectionStringGrid(FGrid).ConfigureAlignmentVerticalInset(varCellPadding);
      end;
    end;

    if FStartPane <> nil then
    begin

      if LDensity.Defined then
      begin
        FStartPane.Grid.ConfigureAlignmentVerticalInset(LPadding);
      end
      else
      begin
        FStartPane.Grid.ConfigureAlignmentVerticalInset(varCellPadding);
      end;
    end;

    if FEndPane <> nil then
    begin

      if LDensity.Defined then
      begin
        FEndPane.Grid.ConfigureAlignmentVerticalInset(LPadding);
      end
      else
      begin
        FEndPane.Grid.ConfigureAlignmentVerticalInset(varCellPadding);
      end;
    end;
    LHeader := FGrid.RowHeights[0];
    FGrid.DefaultRowHeight := Max(1, LDefault);

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;
    FGrid.RowHeights[0] := LHeader;

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;
    for LIndex := 0 to High(FTextLayoutRows) do
    begin

      if (FTextLayoutRows[LIndex] > 0) and (FTextLayoutRows[LIndex] < FGrid.RowCount) then
      begin
        FGrid.RowHeights[FTextLayoutRows[LIndex]] := Max(1, LDefault);

        if not InputLifetimeCurrent then
        begin
          Exit;
        end;
      end;
    end;
    LFirst := Max(1, FGrid.TopRow);
    LLast := Min(LCount, LFirst + Min(FGrid.VisibleRowCount + 1,
      Max(1, FTextLimits.MaximumCells div Max(1, LSpec.Count))) - 1);
    LMeasuredCells := 0;
    SetLength(LLayouts, Max(0, LLast - LFirst + 1));
    SetLength(LHeights, Length(LLayouts));
    SetLength(LRows, Length(LLayouts));
    for LRow := LFirst to LLast do
    begin
      LIndex := LRow - LFirst;
      LRows[LIndex] := LRow;
      LHeights[LIndex] := Max(1, LDefault);
      SetLength(LLayouts[LIndex], LSpec.Count);
      for LColumn := 0 to LSpec.Count - 1 do
      begin
        LPolicy := FTablePresentation.TextAt(LColumn);

        if not LPolicy.Defined or
          (FHasColumnLayout and not FColumnLayout.ColumnAt(LColumn).IsVisible) then
        begin
          Continue;
        end;

        if LMeasuredCells >= FTextLimits.MaximumCells then
        begin
          LLayouts[LIndex][LColumn] := UnavailableNyxTableTextLayout(nttaCellLimit,
            'Visible cells exceed this host measurement pass budget');
          Continue;
        end;
        Inc(LMeasuredCells);

        if not LMetricSupported then
        begin
          LLayouts[LIndex][LColumn] := UnavailableNyxTableTextLayout(nttaHostMetrics,
            'This native font or custom drawing requires its own resolved text metric');
          Continue;
        end;
        LGrid := FGrid;
        LPhysical := LColumn;

        if FHasColumnLayout then
        begin
          LGrid := GridForColumn(LColumn, LPhysical);
        end;

        if LGrid = nil then
        begin
          Continue;
        end;
        LPresentation := LSpec.ColumnAt(LColumn).Present(AData.ItemAt(LRow - 1), AData.Schema);

        if not InputLifetimeCurrent then
        begin
          Exit;
        end;
        LText := DisplayText(LPresentation.Text);
        LLayouts[LIndex][LColumn] := LayoutNyxTableText(LText, LPolicy,
          Max(0, LGrid.ColWidths[LPhysical] - 2 * varCellPadding),
          FTextLimits, MeasurePresentationText);

        if not InputLifetimeCurrent then
        begin
          Exit;
        end;
        LHeights[LIndex] := Max(LHeights[LIndex],
          LLayouts[LIndex][LColumn].Count * LLineHeight + 2 * LPadding);
      end;
    end;
    FTextLayouts := LLayouts;
    FTextLayoutRows := LRows;

    if FGrid is TNyxCollectionStringGrid then
    begin
      TNyxCollectionStringGrid(FGrid).ConfigureProjectionRows(LRows);
    end;
    FTextLayoutFirst := LFirst;
    FTextLayoutTotal := LCount;
    for LIndex := 0 to High(LHeights) do
    begin
      FGrid.RowHeights[LRows[LIndex]] := LHeights[LIndex];

      if not InputLifetimeCurrent then
      begin
        Exit;
      end;
    end;
  finally
    FTextMeasureCanvas := nil;
    LBitmap.Free;
    FTextLayoutApplying := False;
    FUpdating := LUpdating;

    if LFrame <> nil then
    begin
      LFrame.Leave;
    end;
  end;
end;

function TNativeMount.GridDisplay(AColumn, ARow: Integer;
  out APresentation: TNyxValuePresentation): TNyxText;
var
  LData: INyxCollectionSnapshot;
begin

  if FHasColumnLayout then
  begin
    Exit(CanonicalDisplay(CanonicalColumn(FGrid, AColumn), ARow, APresentation));
  end;
  Result := GridText(AColumn, ARow);
  APresentation := NyxCanonicalPresentation(TNyxStateValue.FromText(Result));

  if (FView = nil) or (ARow <= 0) or (AColumn < 0) or
    (AColumn >= FView.Spec.Count) then
  begin
    Exit;
  end;
  LData := FRendered;

  if (LData <> nil) and (ARow <= LData.Count) then
  begin
    APresentation := FView.Spec.ColumnAt(AColumn)
      .Present(LData.ItemAt(ARow - 1), LData.Schema);
    Result := DisplayText(APresentation.Text);
  end;
end;

procedure TNativeMount.QueryColumn(AColumn: Integer; AFilter, AAppend: Boolean);
var
  LQueries: INyxCollectionQueryWorkbench;
  LSort: TNyxCollectionSort;
  LObservation: TNyxQueryWorkbenchObservation;
  LField: TNyxText;
  LIndex: Integer;
  LAscending: Boolean;
  LReset: Boolean;
begin

  if FHasColumnLayout and (not GetInputReady or ColumnInputBusy) then
  begin
    Exit;
  end;

  if not GetConnected or not FEnabled or (FQueries = nil) or
    (AColumn < 0) or (AColumn >= FView.Spec.Count) then
  begin
    Exit;
  end;
  LQueries := FQueries;
  try

    if not ((FView.Spec.TableQueryPolicy.FilterEnabled and FView.Spec.ColumnAt(AColumn).QueryPolicy.FilterEnabled) or
      (FView.Spec.TableQueryPolicy.SortEnabled and FView.Spec.ColumnAt(AColumn).QueryPolicy.SortEnabled)) then
    begin
      Exit;
    end;

    if AFilter or not FView.Spec.TableQueryPolicy.SortEnabled or
      not FView.Spec.ColumnAt(AColumn).QueryPolicy.SortEnabled then
    begin

      if FView.Spec.TableQueryPolicy.FilterEnabled and
        FView.Spec.ColumnAt(AColumn).QueryPolicy.FilterEnabled then
      begin
        LQueries.OpenColumn(AColumn);
      end
      else
      begin
        LQueries.OpenSort;
      end;
      Exit;
    end;
    LAscending := True;
    LReset := False;
    LField := FView.Spec.ColumnAt(AColumn).FieldName;
    LObservation := LQueries.Observation;

    if not GetInputReady or (FHasColumnLayout and ColumnInputBusy) or
      not FEnabled or (FQueries <> LQueries) then
    begin
      Exit;
    end;
    { An authored descending baseline is not a user cycle already at its last
      step. The first header activation deliberately starts an ascending key. }
    for LIndex := 0 to LObservation.Query.SortCount - 1 do
    begin
      LSort := LObservation.Query.SortAt(LIndex);

      if LObservation.HasUserSort and (LSort.FieldName = LField) then
      begin
        LAscending := False;
        LReset := LSort.Direction = nsdDescending;
        Break;
      end;
    end;

    if LReset then
    begin
      LQueries.ResetSort;
    end
    else if LAscending then
    begin
      LQueries.SortColumn(AColumn, nsdAscending, AAppend);
    end
    else
    begin
      LQueries.SortColumn(AColumn, nsdDescending, AAppend);
    end;
  except
    on LException: Exception do
    begin
      { The same diagnostic phase distinguishes a refused command from a
        committed observer failure. Never undo committed rows in an adapter. }
      Failed(LException);
    end;
  end;
end;

procedure TNativeMount.GridResized(Sender: TObject);
var
  LKeepAlive: INyxCollectionMount;
begin
  LKeepAlive := Self as INyxCollectionMount;

  if (FGrid is TNyxCollectionStringGrid) and
    TNyxCollectionStringGrid(FGrid).ContentProjectionChanging then
  begin
    Exit;
  end;

  if FColumnApplying or (FHasColumnLayout and FUpdating) then
  begin
    { Internal pane/scrollbar geometry must settle before an external observer
      can see it; the originating outer resize retains its ordinary callback. }
    Exit;
  end;

  if Assigned(FPreviousGridResize) then
  begin
    FPreviousGridResize(Sender);
  end;

  if GetConnected and not FUpdating and not FColumnApplying and
    (Length(FColumnNatural) > 0) then
  begin
    SizeGridColumns;
  end;
end;

procedure TNativeMount.GridProjectionChanged(Sender: TObject);
var
  LIndex: Integer;
  LPhysical: Integer;
  LGrid: TStringGrid;
begin

  if not GetConnected or (FGrid <> Sender) then
  begin
    Exit;
  end;
  { These are physical observation caches, never authored policy. Refreshing
    them after both logical restoration and visual projection prevents the
    ordinary resize detector from mistaking zoom for a user column override. }
  for LIndex := 0 to High(FColumnApplied) do
  begin

    if FHasColumnLayout then
    begin
      LGrid := GridForColumn(LIndex, LPhysical);
    end
    else
    begin
      LGrid := FGrid;
      LPhysical := LIndex;
    end;

    if (LGrid <> nil) and (LPhysical >= 0) and (LPhysical < LGrid.ColCount) then
    begin
      FColumnApplied[LIndex] := LGrid.ColWidths[LPhysical];
    end;
  end;
  FColumnToolbarHeight := TNyxCollectionStringGrid(FGrid).ColumnToolbarHeight;
end;

procedure TNativeMount.SizeGridColumns;
var
  LUpdate: INyxLCLLogicalUpdate;
begin

  if FGrid is TNyxCollectionStringGrid then
  begin
    LUpdate := TNyxCollectionStringGrid(FGrid).BeginLogicalUpdate;
  end;
  try
    SizeLogicalGridColumns;
  finally

    if LUpdate <> nil then
    begin
      LUpdate.Close;
    end;
  end;
end;

procedure TNativeMount.SizeLogicalGridColumns;
var
  LPolicies: TNyxColumnSizings;
  LWidths: TNyxFlowSizes;
  LIndex: Integer;
  LScale: Double;
  LUpdating: Boolean;
begin

  if not GetConnected or (FGrid = nil) or FColumnApplying then
  begin
    Exit;
  end;

  if FHasColumnLayout or FHasTablePresentation then
  begin
    { Route external resizing through the same whole-refresh composition
      fence and latest captured column policy, rather than a second map path. }

    if not FUpdating then
    begin
      { ScrollToCell/editor positioning can synchronously notify OnResize in
        the middle of LCL EditorShow. Refresh would capture/hide the accepted
        text before the original fresh-show observer restores its dormant draft.
        Keep the current map usable, let that exact input finish, and use the
        SAME bounded idle/refresh queue as composition and resize retirement. }

      if ((FColumnFrame <> nil) and FColumnFrame.Dispatching) or
        TNyxCollectionStringGrid(FGrid).ValidationActive or
        ((FStartPane <> nil) and FStartPane.Grid.ValidationActive) or
        ((FEndPane <> nil) and FEndPane.Grid.ValidationActive) then
      begin
        FColumnGeometryPending := True;
        ArmCellRefreshIdle;
        Exit;
      end;
      Refresh;
      Exit;
    end;

    if FHasColumnLayout then
    begin
      Exit;
    end;
    { A presentation-only table has no arranged renderer to allocate widths.
      During its admitted refresh, retain the ordinary sizing/override path
      below before measuring wrapped rows. External resizes still enter the
      same composition and native-frame fence above. }
  end;
  LScale := Max(1, FGrid.Font.PixelsPerInch) / 96;

  if FGrid.Font.PixelsPerInch <= 0 then
  begin
    LScale := 1;
  end;
  SetLength(LPolicies, FView.Spec.Count);
  SetLength(FColumnOverrides, FView.Spec.Count);
  for LIndex := 0 to High(LPolicies) do
  begin
    LPolicies[LIndex] := FView.Spec.ColumnAt(LIndex).Sizing;

    if (Length(FColumnApplied) = Length(LPolicies)) and
      (FGrid.ColWidths[LIndex] <> FColumnApplied[LIndex]) then
    begin
      FColumnOverrides[LIndex] := Max(1, Round(FGrid.ColWidths[LIndex] / LScale));
    end;

    if FColumnOverrides[LIndex] > 0 then
    begin
      LPolicies[LIndex] := NyxColumnSizing.Fixed(FColumnOverrides[LIndex]);
    end;
  end;
  LWidths := NyxColumnWidths(Max(0, Round((FGrid.ClientWidth -
    FGrid.GridLineWidth * FView.Spec.Count) / LScale)), LPolicies, FColumnNatural);
  SetLength(FColumnApplied, Length(LWidths));
  LUpdating := FUpdating;
  FUpdating := True;
  FColumnApplying := True;
  try
    for LIndex := 0 to High(LWidths) do
    begin
      FColumnApplied[LIndex] := Max(1, Round(LWidths[LIndex] * LScale));
      FGrid.ColWidths[LIndex] := FColumnApplied[LIndex];

      if not GetConnected then
      begin
        Exit;
      end;
    end;
  finally
    FColumnApplying := False;
    FUpdating := LUpdating;
  end;
end;

procedure TNativeMount.RenderDataset;
var
  LUpdate: INyxLCLLogicalUpdate;
begin

  if FGrid is TNyxCollectionStringGrid then
  begin
    LUpdate := TNyxCollectionStringGrid(FGrid).BeginLogicalUpdate;
  end;
  try
    RenderLogicalDataset;
  finally

    if LUpdate <> nil then
    begin
      LUpdate.Close;
    end;
  end;
end;

procedure TNativeMount.RenderLogicalDataset;
var
  LPosition: TNyxCollectionPositionRecovery;
  LReveal: TNyxTreeRevealRequest;
  LData: INyxCollectionSnapshot;
  LHierarchy: INyxTreeHierarchy;
  LIndex: Integer;
  LColumn: Integer;
  LSelected: Integer;
  LPrevious: Integer;
  LParent: Integer;
  LTop: Integer;
  LNext: array of TTreeNode;
  LStructureChanged: Boolean;
  LEditable: Boolean;
  LApplyValues: Boolean;
  LColumnSpec: TNyxCollectionColumn;
  LText: TNyxText;
  LEditingItem: TNyxItemRef;
  LEditingColumn: Integer;
  LEditingText: String;
  LEditingStart: Integer;
  LEditingLength: Integer;
  LRestoreEditor: Boolean;
  LEditor: TCustomEdit;
  LCell: TNyxCollectionCellEdit;
  LReason: TNyxCollectionDraftLoss;
begin
  LData := FView.Snapshot;

  if DeferTargetRefresh then
  begin
    Exit;
  end;
  if FHasColumnLayout and (FGrid <> nil) then
  begin
    RenderArrangedGrid(LData);

    if not FTargetRefreshDeferred then
    begin
      FCellRefreshPending := False;
    end;
    Exit;
  end;
  LPosition := Default(TNyxCollectionPositionRecovery);
  LPosition.Index := -1;

  if FGrid <> nil then
  begin
    LPosition := ResolvePosition(FRendered, LData, FGrid.TopRow - 1);
  end
  else if FList <> nil then
  begin
    LPosition := ResolvePosition(FRendered, LData, FList.TopIndex);
  end;

  if not InputLifetimeCurrent then
  begin
    Exit;
  end;

  { An authoritative view publication supersedes any host proposal queued before
    it. Never replay that older physical state over a later application command. }

  if Length(FDisclosureItems) > 0 then
  begin
    Application.RemoveAsyncCalls(Self);
    FDisclosureItems := nil;
    FDisclosureValues := nil;
  end;
  LRestoreEditor := False;
  LEditingItem := Default(TNyxItemRef);
  LEditingColumn := -1;

  if (FGrid <> nil) and (FRendered <> nil) and (FRendered <> LData) and
    FGrid.EditorMode and (FGrid.Editor is TCustomEdit) and
    (FGrid.Row > 0) and (FGrid.Row <= FRendered.Count) and
    (FGrid.Col >= 0) and (FGrid.Col < FView.Spec.Count) then
  begin
    { End the positional editor while Refresh has FUpdating set, so it cannot
      admit its draft against a different row after sorting/filtering. Retain
      the draft/caret only when the same item/column accepted scalar survives. }
    LEditingItem := FRendered.ItemAt(FGrid.Row - 1).Ref;
    LEditingColumn := FGrid.Col;
    LEditor := TCustomEdit(FGrid.Editor);
    LEditingText := LEditor.Text;
    LEditingStart := LEditor.SelStart;
    LEditingLength := LEditor.SelLength;
    LColumnSpec := FView.Spec.ColumnAt(LEditingColumn);
    LRestoreEditor := LData.Has(LEditingItem) and
      LColumnSpec.Read(FRendered.Item(LEditingItem)).SameValue(
        LColumnSpec.Read(LData.Item(LEditingItem))) and
      not (FNormalizeValues and (FNormalizeItem.ID = LEditingItem.ID) and
        (FNormalizeColumn = LEditingColumn));
    LCell := NyxCollectionCellEdit(LEditingItem, LColumnSpec.Identity,
      LColumnSpec.Read(FRendered.Item(LEditingItem)), TNyxText(LEditingText),
      CaptureNyxLCLSelection(LEditor), False, False);

    if RecoverCell(LCell, FRendered, LData, LReason,
      TNyxText(LEditingText) <> ColumnValueDisplay(LCell.Accepted)) then
    begin
      LRestoreEditor := False;
    end;

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;
    FGrid.EditorMode := False;
  end;

  if FGrid <> nil then
  begin
    LEditable := False;
    for LColumn := 0 to FView.Spec.Count - 1 do
    begin

      if FEnabled and not FReadOnly and (FView.Spec.ColumnAt(LColumn).Mode = cmEditable) then
      begin
        LEditable := True;
      end;
    end;

    if LEditable <> (goEditing in FGrid.Options) then
    begin

      if LEditable then
      begin
        FGrid.Options := FGrid.Options + [goEditing];
      end
      else
      begin
        FGrid.Options := FGrid.Options - [goEditing];
      end;
    end;
  end;

  if FTree <> nil then
  begin
    FTree.ReadOnly := not FEnabled or FReadOnly or (FView.Spec.ColumnAt(0).Mode <> cmEditable);
  end;
  LSelected := -1;

  if FView.Selection.Focus.Defined then
  begin
    LSelected := LData.IndexOf(FView.Selection.Focus);
  end;

  if FList <> nil then
  begin
    LTop := FList.TopIndex;

    if LPosition.Index >= 0 then
    begin
      LTop := LPosition.Index;
    end
    else if (FRendered <> nil) and (LTop >= 0) and (LTop < FRendered.Count) then
    begin
      LTop := LData.IndexOf(FRendered.ItemAt(LTop).Ref);
    end;
    FList.Items.BeginUpdate;
    try

      if FRefreshPlan.Kind = ncrFull then
      begin
        { Rebuild only for a changed shape or unavailable publication context.
          Scalar/selection refreshes retain entry objects, scroll and selection. }
        FList.Items.Clear;
        for LIndex := 0 to LData.Count - 1 do
        begin
          FList.Items.Add(DisplayText(FView.CellText(LData.ItemAt(LIndex).Ref, 0)));
        end;
      end
      else
      begin
        for LIndex := 0 to LData.Count - 1 do
        begin

          if FRefreshPlan.ValuesChanged(LIndex) or
            (FNormalizeValues and (FNormalizeItem.ID = LData.ItemAt(LIndex).Ref.ID)) then
          begin
            LText := DisplayText(FView.CellText(LData.ItemAt(LIndex).Ref, 0));

            if TNyxText(FList.Items[LIndex]) <> LText then
            begin
              FList.Items[LIndex] := LText;
            end;
          end;
        end;
      end;

      if FList.ItemIndex <> LSelected then
      begin
        FList.ItemIndex := LSelected;
      end;

      if FList.MultiSelect then
      begin
        for LIndex := 0 to LData.Count - 1 do
        begin

          if FList.Selected[LIndex] <> FView.Selection.Contains(LData.ItemAt(LIndex).Ref) then
          begin
            FList.Selected[LIndex] := FView.Selection.Contains(LData.ItemAt(LIndex).Ref);
          end;
        end;
      end;

      if LTop >= 0 then
      begin
        FList.TopIndex := LTop;
      end;
    finally
      FList.Items.EndUpdate;
    end;

    if InputLifetimeCurrent and (LPosition.Index >= 0) then
    begin
      FList.TopIndex := LPosition.Index;
    end;
  end
  else if FGrid <> nil then
  begin
    FGrid.BeginUpdate;
    try
      { Enlarge the existing fixed header, retaining every existing row index.
        Counts/markers wrap within it; detailed removable chips live in the
        shared Nyx form. This is not an overlaid toolbar hiding data rows. }

      ReserveHeader;

      if not InputLifetimeCurrent then
      begin
        Exit;
      end;

      if FQueries <> nil then
      begin
        FGridHelp := FQuerySummary + TNyxText('. Header arrow or Alt+Down: filter. ') +
          TNyxText('Header title: sort; Shift: add a sort key.');
        ApplyGridHint;
      end;
      { Preserve drafts when their accepted scalar is unchanged. Native grid
        cells are positional, so moved/new rows must receive their new content;
        unchanged rows survive unrelated field publications and normalization. }

      if FNormalizeValues or (FRendered <> LData) then
      begin

        if FGrid.ColCount <> FView.Spec.Count then
        begin
          FGrid.ColCount := FView.Spec.Count;
        end;
        FGrid.FixedCols := 0;

        if FGrid.RowCount <> Max(2, LData.Count + 1) then
        begin
          FGrid.RowCount := Max(2, LData.Count + 1);
        end;
        FGrid.FixedRows := 1;

        if FGridReaderAttached then
        begin
          TNyxCollectionStringGrid(FGrid).TrimOverrides(FView.Spec.Count, LData.Count);
        end;
        for LColumn := 0 to FView.Spec.Count - 1 do
        begin
          FGrid.Cells[LColumn, 0] := GridText(LColumn, 0);

          if LData.Count = 0 then
          begin
            FGrid.Cells[LColumn, 1] := '';
          end;
        end;
        for LIndex := 0 to LData.Count - 1 do
        begin
          { An unchanged row needs no scalar reads. Closing a positional editor
            can have written its draft into the physical cell, so that row still
            receives the ordinary per-cell normalization/restoration below. }

          if not FRefreshPlan.ValuesChanged(LIndex) and
            not (FNormalizeValues and (FNormalizeItem.ID = LData.ItemAt(LIndex).Ref.ID)) and
            (not LEditingItem.Defined or (LData.ItemAt(LIndex).Ref.ID <> LEditingItem.ID)) then
          begin
            Continue;
          end;
          LPrevious := -1;

          if FRendered <> nil then
          begin
            LPrevious := FRendered.IndexOf(LData.ItemAt(LIndex).Ref);
          end;
          for LColumn := 0 to FView.Spec.Count - 1 do
          begin
            LApplyValues := LPrevious <> LIndex;

            if (LColumn = LEditingColumn) and
              (LData.ItemAt(LIndex).Ref.ID = LEditingItem.ID) then
            begin
              { Closing the old editor may write its text into the physical
                cell. Normalize it from accepted data before restoring a draft. }
              LApplyValues := True;
            end;

            if not LApplyValues and (FRendered.Revision <> LData.Revision) then
            begin
              LColumnSpec := FView.Spec.ColumnAt(LColumn);
              LApplyValues := not LColumnSpec.Read(LData.ItemAt(LIndex)).SameValue(
                LColumnSpec.Read(FRendered.ItemAt(LPrevious)));
            end;

            if FNormalizeValues and
              (FNormalizeItem.ID = LData.ItemAt(LIndex).Ref.ID) and
              (FNormalizeColumn = LColumn) then
            begin
              LApplyValues := True;
            end;

            if LApplyValues then
            begin

              if FGridReaderAttached then
              begin
                TNyxCollectionStringGrid(FGrid).ClearOverride(LColumn, LIndex + 1);
              end
              else
              begin
                FGrid.Cells[LColumn, LIndex + 1] := DisplayText(
                  FView.CellText(LData.ItemAt(LIndex).Ref, LColumn));
              end;
            end;
          end;
        end;
        { The widget's fixed default width clips ordinary task captions. Fit
          admitted headers/content once through LCL's own font measurement;
          later publications retain user column widths and active editor drafts.
          This remains adapter presentation, never an authored schema mutation. }

        if FRendered = nil then
        begin
          FGrid.AutoSizeColumns;
          SetLength(FColumnNatural, FView.Spec.Count);
          for LColumn := 0 to FView.Spec.Count - 1 do
          begin
            { Natural text measurement remains the widget's existing initial
              admission. Reflow subsequently reuses metrics instead of reading
              the whole dataset again on every resize/publication. }
            FColumnNatural[LColumn] := Max(1, FGrid.ColWidths[LColumn]);

            if FGrid.Font.PixelsPerInch > 0 then
            begin
              FColumnNatural[LColumn] := Max(1, Round(FGrid.ColWidths[LColumn] * 96 /
                FGrid.Font.PixelsPerInch));
            end;
          end;
          SizeGridColumns;
        end;
      end;
      { Query metadata may change without row membership changing. Refresh
        headers independently of data-cell normalization/draft ownership. }
      for LColumn := 0 to FView.Spec.Count - 1 do
      begin

        if not FGridReaderAttached then
        begin
          FGrid.Cells[LColumn, 0] := GridText(LColumn, 0);
        end;
      end;

      if FHasTablePresentation then
      begin
        SizeGridColumns;
      end;
      PrepareTextRows(LData);
      FColumnGeometryPending := False;
      FGrid.Invalidate;

      if (LSelected >= 0) and (FGrid.Row <> LSelected + 1) then
      begin
        FGrid.Row := LSelected + 1;
      end;

      if LRestoreEditor then
      begin
        FGrid.Col := LEditingColumn;
        FGrid.Row := LData.IndexOf(LEditingItem) + 1;
        FGrid.EditorMode := True;

        if FGrid.Editor is TCustomEdit then
        begin
          LEditor := TCustomEdit(FGrid.Editor);
          LEditor.Text := LEditingText;
          LEditor.SelStart := LEditingStart;
          LEditor.SelLength := LEditingLength;
        end;
      end;
    finally
      FGrid.EndUpdate;
    end;
    FGrid.Invalidate;
  end
  else
  begin
    LHierarchy := NyxTreeHierarchy(FView);
    FTree.Items.BeginUpdate;
    try

      if FRendered = nil then
      begin
        FTree.Items.Clear;
      end;
      SetLength(LNext, LData.Count);
      LStructureChanged := (FRendered = nil);

      if FRendered <> nil then
      begin
        LStructureChanged := FRendered.Count <> LData.Count;
      end;
      for LIndex := 0 to LData.Count - 1 do
      begin
        LPrevious := -1;

        if FRendered <> nil then
        begin
          LPrevious := FRendered.IndexOf(LData.ItemAt(LIndex).Ref);
        end;

        if LPrevious >= 0 then
        begin
          LNext[LIndex] := FNodes[LPrevious];

          if LPrevious <> LIndex then
          begin
            LStructureChanged := True;
          end;

          if (FView.Spec.ParentField <> '') and
            (LData.ItemAt(LIndex).GetValue(NyxTextField(FView.Spec.ParentField)) <>
            FRendered.ItemAt(LPrevious).GetValue(NyxTextField(FView.Spec.ParentField))) then
          begin
            LStructureChanged := True;
          end;
        end
        else
        begin
          LNext[LIndex] := FTree.Items.Add(nil, '');
          LStructureChanged := True;
        end;
        LNext[LIndex].Data := Pointer(PtrUInt(LIndex + 1));

        if FRefreshPlan.ValuesChanged(LIndex) or
          (FNormalizeValues and (FNormalizeItem.ID = LData.ItemAt(LIndex).Ref.ID)) then
        begin
          LText := DisplayText(FView.CellText(LData.ItemAt(LIndex).Ref, 0));

          if TNyxText(LNext[LIndex].Text) <> LText then
          begin
            LNext[LIndex].Text := LText;
          end;
        end;
      end;

      if LStructureChanged and (FRendered <> nil) then
      begin
        { Scalar/selection changes preserve structure and expansion completely.
          Flatten only for an admitted reorder/reparent/removal, before deleting
          old ancestors or applying new links that could temporarily form cycles. }
        for LIndex := 0 to Length(LNext) - 1 do
        begin
          LNext[LIndex].MoveTo(nil, naAdd);
        end;
      end;
      for LIndex := 0 to Length(FNodes) - 1 do
      begin

        if not LData.Has(FRendered.ItemAt(LIndex).Ref) then
        begin
          FNodes[LIndex].Delete;
        end;
      end;
      FNodes := LNext;

      if LStructureChanged then
      begin
        for LIndex := 0 to Length(FNodes) - 1 do
        begin
          LParent := FView.ParentIndex(LIndex);

          if LParent >= 0 then
          begin
            FNodes[LIndex].MoveTo(FNodes[LParent], naAddChild);
          end;
        end;
      end;

      if LSelected >= 0 then
      begin
        FTree.Selected := FNodes[LSelected];
      end
      else
      begin
        FTree.Selected := nil;
      end;

      if FTree.MultiSelect then
      begin
        for LIndex := 0 to Length(FNodes) - 1 do
        begin
          FNodes[LIndex].MultiSelected := FView.Selection.Contains(LData.ItemAt(LIndex).Ref);
        end;
      end;
      { LCL may open ancestors while setting Selected or moving nodes. Restore
        public disclosure after those host operations, under FUpdating so host
        notifications cannot feed back into a view currently publishing. }
      for LIndex := 0 to Length(FNodes) - 1 do
      begin
        FNodes[LIndex].Expanded := LHierarchy.IsExpanded(LData.ItemAt(LIndex).Ref);
      end;
    finally
      FTree.Items.EndUpdate;
    end;
  end;
  { Settling source geometry/selection must not steal a reader's stable row.
    This does not run for ordinary focus/selection-only notifications. }

  if InputLifetimeCurrent and (FGrid <> nil) and (LPosition.Index >= 0) then
  begin
    FGrid.TopRow := Min(LPosition.Index + 1, FGrid.RowCount - 1);
  end;

  if InputLifetimeCurrent and (FGrid <> nil) and FHasTablePresentation and
    (FTextLayoutFirst <> Max(1, FGrid.TopRow)) then
  begin
    { Selection and source recovery run under the update guard. Re-admit only
      their final bounded viewport; no dataset-wide text layout is introduced. }
    PrepareTextRows(LData);
  end;
  FRendered := LData;
  FCellRefreshPending := False;
  LReveal := PendingTreeReveal;

  if InputLifetimeCurrent and (FTree <> nil) and LReveal.Item.Defined then
  begin
    LIndex := LData.IndexOf(LReveal.Item);

    if (LIndex >= 0) and (LIndex < Length(FNodes)) then
    begin
      { Setting the same Selected node is a host no-op. MakeVisible is the
        explicit viewport effect, after disclosure and EndUpdate have settled. }
      FNodes[LIndex].MakeVisible;

      if not InputLifetimeCurrent then
      begin
        Exit;
      end;
      TreeRevealApplied(LReveal.Serial);

      if (LReveal.Focus = ntrfTake) and FTree.CanFocus then
      begin
        { Last borrowed target call: OnEnter may retire this renderer. }
        FTree.SetFocus;
      end;
    end;
  end;
end;

procedure TNativeMount.DetachTarget;
var
  LHeader: Integer;

  procedure Suspend(AGrid: TStringGrid);
  begin

    if AGrid = nil then
    begin
      Exit;
    end;
    TNyxCollectionStringGrid(AGrid).AdvanceColumnMap;
    AGrid.EditorMode := False;
    TNyxCollectionStringGrid(AGrid).DetachEditReader;
  end;

begin
  FinishRangeGesture(FRangeGesture);
  FColumnGeometryPending := False;
  FTextLayouts := nil;
  FTextLayoutRows := nil;
  try
    Application.RemoveAsyncCalls(Self);

    if FCellIdleArmed then
    begin
      FCellIdleArmed := False;
      Application.RemoveOnIdleHandler(CellRefreshIdle);
    end;
    { Weak wakes were revoked before any query cleanup. Restore the outer
      input chain before removing its inner editing observer, then touch grids. }

    try

      if FColumnEditing <> nil then
      begin
        FColumnEditing.Disconnect;
      end;
    finally
      { Observer cleanup attempts every editor before reporting an error. Even
        that error cannot skip revocation of the mount's borrowed grid hooks. }
      Application.RemoveAsyncCalls(Self);
      FDisclosureItems := nil;
      FDisclosureValues := nil;
      RetireColumnResize(FResizeRequest);
      FResizeRequest := nil;
      FResizeRequestLease := nil;
      FResizeCapture := nil;
      FResizePending := False;
      FLayoutGesture := False;
      FColumnEdit := Default(TColumnEditLease);
      FDormantColumnEdits := nil;
      FResumeQueued := False;
      FColumnGeometryPending := False;
      FResizeQueued := False;
      FResizeReleased := False;
      SetCellEditsSuspended(False);
      FEditSuspension := nil;
      FCellFocusValid := False;
      FCellFocusGrid := nil;
      FCellFocusInput := nil;

      if FList <> nil then
      begin
        FList.OnSelectionChange := FPreviousSelection;
        FList.OnKeyDown := FPreviousKey;
        FList.OnUTF8KeyPress := FPreviousUTF8;
        FList.MultiSelect := FPreviousMultiSelect;
        FList.ExtendedSelect := FPreviousExtendedSelect;
        FList := nil;
      end;

      if FGrid <> nil then
      begin

        if FGrid is TNyxCollectionStringGrid then
        begin
          TNyxCollectionStringGrid(FGrid).ObserveContentProjection(nil);
          TNyxCollectionStringGrid(FGrid).ConfigureContentProjection(nil);
          TNyxCollectionStringGrid(FGrid).ColumnToolbarHeight := 0;
        end;
        RetireColumnsButton;

        if FHasColumnLayout then
        begin
          { Connected is already False. Revoke queued work and entered validators
            while maps/readers still exist, suppress ordinary edit admission, then
            retire every weak pane callback before any child control is freed. }
          FUpdating := True;
          Suspend(FGrid);

          if FStartPane <> nil then
          begin
            Suspend(FStartPane.Grid);
            FStartPane.Revoke;
          end;

          if FEndPane <> nil then
          begin
            Suspend(FEndPane.Grid);
            FEndPane.Revoke;
          end;
          RetireFitButtons;
          TNyxCollectionStringGrid(FGrid).ColumnToolbarHeight := 0;

          if FColumnToolbarApplied then
          begin
            FGrid.RowHeights[0] := Max(1, FGrid.RowHeights[0] - FColumnToolbarHeight);
            FColumnToolbarApplied := False;
          end;
          FColumnToolbarHeight := 0;
          FreeAndNil(FStartPane);
          FreeAndNil(FEndPane);
          FActiveGrid := nil;
          FMainColumns := nil;
          FGrid.OnTopLeftChanged := FPreviousTopLeft;
          FGrid.OnHeaderSized := FPreviousHeaderSized;
          FGrid.OnMouseUp := FPreviousMouseUp;
          FGrid.OnMouseMove := FPreviousMouseMove;
          FGrid.OnMouseEnter := FPreviousMouseEnter;
          FGrid.OnMouseLeave := FPreviousMouseLeave;
          FGrid.OnKeyUp := FPreviousKeyUp;
          FGrid.OnEnter := FPreviousEnter;
          FGrid.OnExit := FPreviousExit;
          FGrid.OnClick := FPreviousClick;
          FGrid.OnDblClick := FPreviousDoubleClick;
        end;

        if FGridReaderAttached then
        begin
          TNyxCollectionStringGrid(FGrid).DetachReader;
          FGridReaderAttached := False;
          FTextLayoutAttached := False;
        end;

        if FHasTablePresentation then
        begin
          FGrid.OnTopLeftChanged := FPreviousTopLeft;
          LHeader := FGrid.RowHeights[0];
          FGrid.DefaultRowHeight := FPreviousDefaultRowHeight;
          FGrid.RowHeights[0] := LHeader;
        end;
        FGrid.OnSelectCell := FPreviousCell;
        FGrid.OnValidateEntry := FPreviousValidate;
        FGrid.OnKeyDown := FPreviousKey;
        FGrid.OnMouseDown := FPreviousMouse;
        FGrid.OnHeaderClick := FPreviousHeaderClick;
        FQueryPressActive := False;
        FGrid.OnPrepareCanvas := FPreviousPrepare;
        FGrid.OnResize := FPreviousGridResize;
        FGrid.Options := FPreviousOptions;
        FGrid.MouseWheelOption := FPreviousMouseWheelOption;
        FGrid.Hint := FPreviousGridHint;
        FGrid.ShowHint := FPreviousGridShowHint;
        FGrid.OnGetCellHint := FPreviousCellHint;

        if FQueryHeaderAdjusted then
        begin
          FGrid.RowHeights[0] := FPreviousHeaderHeight;
          FQueryHeaderAdjusted := False;
        end;
        FGrid := nil;
      end;

      if FTree <> nil then
      begin
        FTree.OnChange := FPreviousTreeChange;
        FTree.OnEdited := FPreviousTreeEdit;
        FTree.OnExpanded := FPreviousTreeExpanded;
        FTree.OnCollapsed := FPreviousTreeCollapsed;
        FTree.ReadOnly := FPreviousReadOnly;
        FTree.OnKeyDown := FPreviousKey;
        FTree.OnUTF8KeyPress := FPreviousUTF8;
        FTree.OnSelectionChanged := FPreviousTreeSelection;
        FTree.MultiSelect := FPreviousMultiSelect;
        FTree.MultiSelectStyle := FPreviousMultiStyle;
        FTree := nil;
      end;
      FNodes := nil;
      FRendered := nil;
      FColumnRendering := nil;
      FTreeRows := nil;
    end;
  finally

    if FInputGridAttached and (FInputGrid <> nil) then
    begin
      FInputGridAttached := False;
      FInputGrid.RetireColumnInput(GetInputLifetime);
    end;
    FInputGrid := nil;
    FColumnFrame := nil;
    FColumnEditing := nil;
  end;
end;

function MountNyxLCLCollection(AControl: TControl;
  const AView: INyxCollectionView;
  AInputPhase: TNyxCollectionInputPhase;
  const AInputGate: INyxCollectionInputGate;
  const AInputFrame: INyxNativeGestureFrame;
  const AInputEvents: INyxEvents; const AOriginID: TNyxText;
  AColumnClick: TNyxLCLColumnClick; ACellContext: TNyxLCLCellContextDispatch;
  ARangePointer: TNyxLCLRangePointerDispatch;
  ACaptureHandler: TNyxLCLCaptureHandler): INyxCollectionMount;
begin
  Result := MountNyxLCLCollectionWithTextLimits(AControl, AView,
    TNyxTableTextWorkLimits.Defaults, AInputPhase, AInputGate, AInputFrame,
    AInputEvents, AOriginID, AColumnClick, ACellContext, ARangePointer, ACaptureHandler);
end;

function MountNyxLCLCollectionWithTextLimits(AControl: TControl;
  const AView: INyxCollectionView; const ALimits: TNyxTableTextWorkLimits;
  AInputPhase: TNyxCollectionInputPhase;
  const AInputGate: INyxCollectionInputGate;
  const AInputFrame: INyxNativeGestureFrame;
  const AInputEvents: INyxEvents; const AOriginID: TNyxText;
  AColumnClick: TNyxLCLColumnClick; ACellContext: TNyxLCLCellContextDispatch;
  ARangePointer: TNyxLCLRangePointerDispatch;
  ACaptureHandler: TNyxLCLCaptureHandler): INyxCollectionMount;
var
  LMount: TNativeMount;
  {$ifdef NYX_STUDIO_PROFILE}LActivateStarted: QWord;{$endif}
begin
  LMount := TNativeMount.Create(AControl, AView, ALimits, AInputPhase, AInputGate,
    AInputFrame, AInputEvents, AOriginID, AColumnClick, ACellContext, ARangePointer, ACaptureHandler);
  Result := LMount;
  {$ifdef NYX_STUDIO_PROFILE}LActivateStarted := GetTickCount64;{$endif}
  LMount.Activate;
  {$ifdef NYX_STUDIO_PROFILE}
  WriteLn('native-mount,activate,', GetTickCount64 - LActivateStarted);
  {$endif}
end;

end.
