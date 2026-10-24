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
unit nyx.collections.lcl.grid;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, Types, Controls, Grids, LMessages, LCLType, nyx.text,
  nyx.collections.mount, nyx.gestures.lcl, nyx.widgets.lcl,
  nyx.collections.arrangement, nyx.formatting, nyx.state,
  nyx.collections.text.layout,
  nyx.collections.clipboard.actions;

type
  { Native-only pure text reader. The grid borrows its receiver; its mount must
    detach before releasing the receiver/view or destroying the widget. Reads
    run on the UI thread and must not publish commands or dispose the widget. }
  TNyxGridTextReader = function(AColumn, ARow: Integer): TNyxText of object;
  { Paint-only borrowed receiver returns target-safe text together with exact
    portable outcome/RAW/diagnostic facts from the same pure presentation read. }
  TNyxGridDisplayReader = function(AColumn, ARow: Integer;
    out APresentation: TNyxValuePresentation): TNyxText of object;
  { Paint-only borrowed cache receiver. Measurement happens during guarded
    synchronization, never from DrawCellText; returned lines own no widget.
    False preserves the ordinary grid drawing path for an absent text policy. }
  TNyxGridTextLayoutReader = function(AColumn, ARow: Integer;
    out ALayout: TNyxTableTextLayout): Boolean of object;
  { Borrowed pure range-paint receiver. It reads copied page bounds/typed field
    membership only, with no dataset/widget getter, validation or publication.
    Its mount detaches it before retiring either receiver or physical grid. }
  TNyxGridCellRangeReader = function(AColumn, ARow: Integer): Boolean of object;
  { Pure per-cell edit admission for arranged data surfaces. Like the text
    reader this borrows its receiver and must neither publish nor dispose it. }
  TNyxGridEditReader = function(AColumn, ARow: Integer): Boolean of object;
  { Weak adapter-only double-click dispatch. The stamp identifies THIS entered
    native message, including nested callbacks. The grid already owns/consumes
    that header tail; the receiver only marks a previously captured command. }
  TNyxGridColumnDoubleClick = procedure(Sender: TObject; AStamp: Integer) of object;

  { Borrowed synchronous mount entry reserves the exact input/context and
    optional focused draft before any previous editor context callback. Nil
    keeps the ordinary editor menu only for unsupported/default-off entry;
    stale managed admission returns its inert envelope and remains claimed.
    No physical event is synthesized. }
  TNyxGridCellContextPrepare = function(Sender: TObject; const APosition: TPoint):
    INyxCollectionClipboardCommand of object;
  { Borrowed renderer dispatch consumes that SAME original envelope. It invokes
    the supplied editor callback before canonical/default delivery and never
    recaptures a later cell. Both weak ports retire before widget cleanup. }
  TNyxGridCellContextDispatch = procedure(Sender: TObject; APosition: TPoint;
    var AHandled: Boolean; const ACommand: INyxCollectionClipboardCommand;
    APrevious: TContextPopupEvent) of object;

  { UI-thread continuity shared by the three surfaces of ONE logical table.
    It owns only composition/deferred-map flags, never a mount, model or widget.
    Text delivery remains independent: PositionsBlocked stops the grid protocol,
    while the already-visible native edit still receives its IME/default text. }
  INyxGridInputContinuity = interface(IInterface)
    ['{D37C9186-6305-45FD-A910-F5826DD22E6F}']
    function Composing: Boolean;
    function PositionsBlocked: Boolean;
    procedure BeginComposition;
    procedure EndComposition;
    procedure DeferPositions(ADeferred: Boolean);
    procedure Retire;
  end;

  { Native implementation lease, exposed here only so the grid retains its
    specialized port without hot-path interface queries. A surface borrows one
    component and owns only the readonly input lifetime and physical frame.
    InitializeWnd/FinalizeWnd manage the borrowed handle; reads never create it. }
  INyxGridInputSurface = interface(IInterface)
    ['{6753C975-53DC-4D98-A580-B710142DC543}']
    function Current: Boolean;
    function Ready: Boolean;
    function Frame: INyxNativeGestureFrame;
    function Continuity: INyxGridInputContinuity;
    function PositionReady: Boolean;
    function Composing: Boolean;
    procedure BeginComposition;
    procedure EndComposition;
    function Retired: Boolean;
    function MatchesLifetime(const ALifetime: INyxCollectionInputLifetime): Boolean;
    procedure Retire;
    { An observer replaces WindowProc. Cover it after activation so pending
      direct-LCL composition cannot alter even that observer's private state;
      uncover in reverse order before disconnecting the borrowed observer. }
    procedure CoverWindowProc;
    procedure UncoverWindowProc;
    procedure AttachHandle;
    procedure DropHandle;
    procedure Disconnect;
  end;

  { Local callback validity, with no grid/mount backreference. A validator keeps
    this managed flag across application callbacks. Map replacement/teardown
    revokes it before touching positional native state. It protects stale cell
    writes; physical component retirement still follows the renderer's policy. }
  INyxGridValidationEpoch = interface(IInterface)
    ['{7C08EAFB-D1B7-4E51-BB9D-08FDD227D5E8}']
    function GetCurrent: Boolean;
    procedure Revoke;
    property Current: Boolean read GetCurrent;
  end;

  { Independent managed call-depth flag shared across replacement epochs.
    Entered validators and native input frames leave through a local interface,
    even when their grid has detached. It owns no widget; its optional weak
    idle receiver may only queue a later turn, never run application code. }
  INyxGridValidationActivity = interface(IInterface)
    ['{D651F987-F30D-4CEC-A3FA-F0F55B2F2D36}']
    procedure Enter;
    procedure Leave;
    function GetActive: Boolean;
    procedure ObserveIdle(AReply: TNotifyEvent);
  end;

  { One original Fit-button activation. Keyboard and pointer callbacks may
    invalidate its default, while the original live incarnation can still
    deliver canonical Click. The terminal token owns no physical button. }
  INyxGridColumnButtonCommand = interface(IInterface)
    ['{12498496-7400-47E6-8058-131D34C14CB9}']
    function DispatchCurrent: Boolean;
    procedure Invoke;
    procedure Retire;
  end;

  { Weak UI-only receiver, cleared before the native mount retires. Returning a
    managed token protects its original capture through key-up and nested input. }
  TNyxGridColumnButtonPrepare = function(Sender: TObject):
    INyxGridColumnButtonCommand of object;

  { An ordinary Nyx column-action button. Its exact readonly input
    surface and the table's existing physical frame cover real HWND and direct
    LCL input. No view/store/renderer is retained by the physical face. }
  TNyxCollectionColumnButton = class(TNyxLCLButton)
  private
    FSurface: INyxGridInputSurface;
    FField: TNyxColumnIdentity;
    FCommand: INyxGridColumnButtonCommand;
    FCommandKey: Word;
    { Stack-restored release provenance survives capture-loss and key callbacks
      which synchronously start a newer input on this same physical button. }
    FReleaseCommand: INyxGridColumnButtonCommand;
    FInRelease: Boolean;
    FPrepare: TNyxGridColumnButtonPrepare;
    FBeforeFocus: TNotifyEvent;
    function Ready: Boolean;
    procedure CancelCommand;
    procedure PrepareCommand(AKey: Word);
    function GetField: TNyxColumnIdentity;
  protected
    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;
    procedure DestroyHandle; override;
    procedure WndProc(var AMessage: TLMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DoExit; override;
  public
    destructor Destroy; override;
    procedure ConfigureColumnAction(
      const ALifetime: INyxCollectionInputLifetime;
      const AFrame: INyxNativeGestureFrame;
      const AContinuity: INyxGridInputContinuity;
      APrepare: TNyxGridColumnButtonPrepare; ABeforeFocus: TNotifyEvent);
    procedure ConfigureFitAction(const AField: TNyxColumnIdentity;
      const ALifetime: INyxCollectionInputLifetime;
      const AFrame: INyxNativeGestureFrame;
      const AContinuity: INyxGridInputContinuity;
      APrepare: TNyxGridColumnButtonPrepare; ABeforeFocus: TNotifyEvent);
    { Logical retirement comes first; the ordinary frame later destroys the
      component. Stale proxies and key-up tails cannot activate its old owner. }
    procedure RetireColumnAction;
    { Compatibility spelling for existing specialized Fit faces. }
    procedure RetireFitAction;
    procedure SetFocus; override;
    procedure Click; override;
    property Field: TNyxColumnIdentity read GetField;
  end;

  { Existing specialized Fit face keeps its public name and typed-field setup;
    the shared physical envelope also serves the persistent management launcher. }
  TNyxCollectionFitButton = class(TNyxCollectionColumnButton);

  { Ordinary LCL grid with on-demand bound values. Without a reader it retains
    TStringGrid behavior. With one, accepted values are queried rather than
    copied into every native cell; explicit widget drafts remain sparse overlays.
    Inherited input/validation still owns edit admission. Provider/model data is
    saved through Nyx, never by native widget streaming of its physical cache.
    Grid geometry and model snapshots remain dataset-sized; this class alone
    establishes neither browser row windowing nor complete memory/frame budgets.
    Counted internal column transitions and a withdrawn attached input lifetime
    suppress setter-generated Click. Admitted physical/canonical user Click
    outside those transitions and raw grids without input retain LCL behavior. }
  TNyxCollectionStringGrid = class(TStringGrid)
  private
    FReader: TNyxGridTextReader;
    { Paint-only accepted display. Cells and LCL's edit/validation path retain
      their canonical reader; formatted labels can never become editor input. }
    FDisplayReader: TNyxGridDisplayReader;
    FTextLayoutReader: TNyxGridTextLayoutReader;
    FCellRangeReader: TNyxGridCellRangeReader;
    FEditReader: TNyxGridEditReader;
    FValidationEpoch: INyxGridValidationEpoch;
    FValidationActivity: INyxGridValidationActivity;
    FValidationActivityObject: TObject;
    FColumnDoubleClick: TNyxGridColumnDoubleClick;
    FColumnEditorShown: TNotifyEvent;
    { The private input surface owns only a readonly lifetime/frame and borrowed
      native hook. Its concrete type stays out of the portable collection API. }
    FInputSurface: INyxGridInputSurface;
    FColumnEditor: TWinControl;
    FCellContextPrepare: TNyxGridCellContextPrepare;
    FCellContextDispatch: TNyxGridCellContextDispatch;
    FReads: Int64;
    { Weak, bounded presentation ownership. A table has at most its two edge
      panes; neither side retains the other's component lifetime. The mount
      unregisters before freeing a pane and disconnects before the outer grid. }
    FColumnOwner: TNyxCollectionStringGrid;
    FColumnPanes: array of TNyxCollectionStringGrid;
    { Exact weak button identities, owned as components by this outer grid. }
    FColumnButtons: array of TControl;
    FColumnTransition: Integer;
    FColumnTransitionFocused: Boolean;
    { A renderer-owned visibility transaction keeps native DoExit/ResetEditor
      from validating a draft while its presentation plane is being hidden. }
    FPresentationChanges: Integer;
    FColumnToolbarHeight: Integer;
    FOverrides: array of record
      Column: Integer;
      Row: Integer;
      Text: TNyxText;
    end;
    function OverrideIndex(AColumn, ARow: Integer): Integer;
    procedure RemoveOverride(AIndex: Integer);
    function GetReaderAttached: Boolean;
    function GetOverrideCount: Integer;
    function GetHasColumnPanes: Boolean;
    function GetHasColumnComposite: Boolean;
    function GetColumnTransitionActive: Boolean;
    function GetValidationActive: Boolean;
    procedure SyncColumnEditorStyle;
    function ColumnEditorBounds(const ACell: TRect; AColumn: Integer): TRect;
    procedure SetColumnToolbarHeight(AHeight: Integer);
  protected
    procedure CreateWnd; override;
    procedure DestroyHandle; override;
    procedure DestroyWnd; override;
    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;
    procedure WndProc(var AMessage: TLMessage); override;
    procedure SelectEditor; override;
    procedure EditorShow(const SelAll: Boolean); override;
    procedure DoEditorShow; override;
    procedure DoPrepareCanvas(AColumn, ARow: Integer;
      AState: TGridDrawState); override;
    procedure DrawCellText(AColumn, ARow: Integer; ARect: TRect;
      AState: TGridDrawState; AText: String); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Click; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    function CanEditShow: Boolean; override;
    function MouseButtonAllowed(Button: TMouseButton): Boolean; override;
    function CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function DoMouseWheelHorz(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function GetCells(ACol, ARow: Integer): String; override;
    procedure SetCells(ACol, ARow: Integer; const AValue: String); override;
    function ValidateEntry(const ACol, ARow: Integer; const OldValue: String;
      var NewValue: String): Boolean; override;
  public
    destructor Destroy; override;
    { Opt-in native input ownership. Install before Activate/handle exposure;
      the renderer supplies its exact existing frame. No mount, view or model
      is retained by either surface. Detach precedes destruction/reader release. }
    procedure AttachColumnInput(const ALifetime: INyxCollectionInputLifetime;
      const AFrame: INyxNativeGestureFrame;
      const AContinuity: INyxGridInputContinuity = nil);
    { Terminal flags only for the exact former mount. Keep its blocking native
      guard until component destruction or explicit adoption by a NEW mount;
      cleanup must not expose a retired HWND or detach a newer owner. }
    procedure RetireColumnInput(const ALifetime: INyxCollectionInputLifetime);
    function ColumnEditorInputSurface: INyxGridInputSurface;
    { Install weak ports on the exact live observed incarnation, including its
      pre-Activate construction. This grants no input authority: the editor
      entry still requires Ready before invoking either borrowed receiver. }
    procedure ConfigureCellContext(const ALifetime: INyxCollectionInputLifetime;
      APrepare: TNyxGridCellContextPrepare; ADispatch: TNyxGridCellContextDispatch);
    procedure DetachColumnInput;
    function EditorByStyle(Style: TColumnButtonStyle): TWinControl; override;
    procedure RegisterColumnPane(APane: TNyxCollectionStringGrid);
    procedure UnregisterColumnPane(APane: TNyxCollectionStringGrid);
    procedure RegisterColumnButton(AButton: TNyxCollectionColumnButton);
    procedure UnregisterColumnButton(AButton: TNyxCollectionColumnButton);
    { Physical band inside row zero, never another logical row or data field.
      All panes share its height; normal header text starts beneath the band. }
    property ColumnToolbarHeight: Integer read FColumnToolbarHeight
      write SetColumnToolbarHeight;
    function ColumnHeaderRect(AColumn: Integer): TRect;
    { Identity and focus queries recognize only this grid, its known panes and
      their actual editor subtrees (bounded four parents). Arbitrary descendants
      cannot impersonate a table merely by carrying a name or parent control. }
    function OwnsColumnInput(AControl: TControl): Boolean;
    function HasOwnedColumnFocus: Boolean;
    property HasColumnPanes: Boolean read GetHasColumnPanes;
    { Focus mapping also recognizes exact table-owned header buttons when no
      pinned pane exists. Legacy tables with neither keep their old path. }
    property HasColumnComposite: Boolean read GetHasColumnComposite;
    procedure BeginColumnTransition;
    procedure EndColumnTransition;
    property ColumnTransitionActive: Boolean read GetColumnTransitionActive;
    { A transition may restore focus only when this logical table owned focus
      on entry. Merely opening a transition cannot authorize stealing it back
      from a management form or another application control. }
    function ColumnTransitionHadFocus: Boolean;
    { Nil or a second active reader refuses. Caller retains/disconnects the
      exclusive receiver; no managed backreference or ownership transfer occurs. }
    procedure AttachReader(AReader: TNyxGridTextReader);
    procedure AttachDisplayReader(AReader: TNyxGridDisplayReader);
    procedure ObserveTextLayout(AReader: TNyxGridTextLayoutReader);
    function CellTextLayout(AColumn, ARow: Integer;
      out ALayout: TNyxTableTextLayout): Boolean;
    procedure ObserveCellRange(AReader: TNyxGridCellRangeReader);
    function CellRangeSelected(AColumn, ARow: Integer): Boolean;
    { The same paint-only projection is observable by native target consumers.
      It borrows the exclusive mount receiver until DetachReader retires it. }
    function DisplayCellText(AColumn, ARow: Integer): TNyxText;
    function DisplayCellPresentation(AColumn, ARow: Integer): TNyxValuePresentation;
    procedure AttachEditReader(AReader: TNyxGridEditReader);
    { Ends borrowing and clears overlays. Ordinary physical LCL cells remain;
      disconnect does not materialize the former entire bound dataset. }
    procedure DetachReader;
    procedure DetachEditReader;
    { Advance before any arranged row/map/editor normalization, even when the
      view snapshot is unchanged. Old native validator tails then refuse their
      positional write instead of targeting the replacement physical cell. }
    procedure AdvanceColumnMap;
    { Borrowed, queue-only mount wake. Detach clears it before receiver release;
      it must never run application callbacks or restore an editor inline. }
    procedure ObserveValidationIdle(AReply: TNotifyEvent);
    { Adapter-only retained observation of this exact edit reader's canonical
      call-depth owner. Use GetActive only: never drive Enter/Leave/ObserveIdle
      through this lease. It contains no widget backreference, so final prepared
      readiness need not dereference a possibly retired native component. }
    function ValidationActivityLease: INyxGridValidationActivity;
    { UI callback provenance only, never a final publication getter. Nested
      MouseUp restores its caller's stamp; zero/High cannot identify a release. }
    function ColumnReleaseStamp: Integer;
    { The native double-click sends MouseDown(ssDouble) before DblClick. Its
      message stamp lets those two callbacks share exact, nested-safe provenance. }
    function ColumnDoubleClickStamp: Integer;
    procedure ClaimColumnDoubleClick(AStamp: Integer);
    procedure ObserveColumnDoubleClick(AReply: TNyxGridColumnDoubleClick);
    { Weak fresh-editor notification. The receiver may import a compatible
      dormant value before the user's remaining key/text tail; detach revokes it. }
    procedure ObserveColumnEditor(AReply: TNotifyEvent);
    { Includes the enclosing key/focus/mouse-release native frame as well as
      nested validators. A restored editor waits until ALL have unwound. }
    property ValidationActive: Boolean read GetValidationActive;
    { Explicit native validation/commit before logical navigation. Setting
      EditorMode=False only hides LCL's editor and is deliberately used for
      draft suspension instead. This method uses LCL's existing ResetEditor. }
    procedure CommitColumnEditor;
    { Counted UI-thread guard for a visibility-only presentation change. It
      preserves the live editor/draft and never disables ordinary user editing
      after the balanced End. Caller retains the enclosing physical frame. }
    procedure BeginPresentationChange;
    procedure EndPresentationChange;
    { Withdraw only native sizing owned by this original input incarnation.
      No release is synthesized and no cell/editor is changed. Capture release
      may call out, so sizing/click flags are cleared first and the retained
      native frame protects the borrowed component until cleanup returns. }
    function CancelColumnSizing(const AInput: INyxCollectionInputLifetime;
      AEpoch: Integer): Boolean;
    { Adapter normalization clears exactly one override without creating a
      native cell or invoking edit validation. Unknown cells are a no-op. }
    procedure ClearOverride(AColumn, ARow: Integer);
    { Pure reset used after a logical editor is captured and before a new
      physical column map is installed. Invalidating/painting belongs to caller. }
    procedure ClearOverrides;
    { Discard overlays outside the admitted source geometry, including the spare
      empty row required by LCL when the source has no rows. Header row is zero. }
    procedure TrimOverrides(AColumns, ASourceRows: Integer);
    { Diagnostic counts source requests, including headers/measurement. It
      saturates rather than making rendering fail on integer overflow. }
    procedure ResetReadCount;
    property ReaderAttached: Boolean read GetReaderAttached;
    property SourceReadCount: Int64 read FReads;
    { Own overlay count; excludes LCL metadata/cache and model resident memory. }
    property OverrideCount: Integer read GetOverrideCount;
  end;

function NewNyxGridInputContinuity: INyxGridInputContinuity;

implementation

uses
  Forms, SysUtils, Math, Graphics, StdCtrls, WSGrids, InterfaceBase, LCLPlatformDef, nyx.collections
  {$ifdef WINDOWS}, Windows, CommCtrl{$endif};

type
  { Bounded by the center and two edge observers. Each observer enters once per
    START and leaves once on settled END. Retirement is terminal and flags-only. }
  TInputContinuity = class(TInterfacedObject, INyxGridInputContinuity)
  private
    FComposing: Integer;
    FDeferred, FRetired: Boolean;
  public
    function Composing: Boolean;
    function PositionsBlocked: Boolean;
    procedure BeginComposition;
    procedure EndComposition;
    procedure DeferPositions(ADeferred: Boolean);
    procedure Retire;
  end;

  { A surface is a managed message lease, never a component owner. The grid and
    its private LCL editor each retain one; entered dispatches keep local leases
    after Disconnect has revoked every borrowed address. Native hook removal is
    separate from the readonly lifetime's earlier flags-only retirement. }
  TNativeInputSurface = class(TInterfacedObject, INyxGridInputSurface)
  private
    FControl: TWinControl;
    FLifetime: INyxCollectionInputLifetime;
    FFrame: INyxNativeGestureFrame;
    FContinuity: INyxGridInputContinuity;
    FConnected, FComposing: Boolean;
    FPrevious: TWndMethod;
    FHooked: Boolean;
    {$ifdef WINDOWS}
    FWindow: HWND;
    {$endif}
    procedure WindowMessage(var AMessage: TLMessage);
    {$ifdef WINDOWS}
    function NativeMessage(AWindow: HWND; AMessage: UINT;
      AWParam: WPARAM; ALParam: LPARAM): LRESULT;
    {$endif}
  public
    constructor Create(AControl: TWinControl;
      const ALifetime: INyxCollectionInputLifetime;
      const AFrame: INyxNativeGestureFrame;
      const AContinuity: INyxGridInputContinuity);
    destructor Destroy; override;
    function Current: Boolean;
    function Ready: Boolean;
    function Frame: INyxNativeGestureFrame;
    function Continuity: INyxGridInputContinuity;
    function PositionReady: Boolean;
    function Composing: Boolean;
    procedure BeginComposition;
    procedure EndComposition;
    function Retired: Boolean;
    function MatchesLifetime(const ALifetime: INyxCollectionInputLifetime): Boolean;
    procedure Retire;
    procedure CoverWindowProc;
    procedure UncoverWindowProc;
    procedure AttachHandle;
    procedure DropHandle;
    procedure Disconnect;
  end;

  { Reuses LCL's complete string-cell protocol (GM_SETGRID/value/position and
    validation), adding only input/lifetime boundaries. InitializeWnd runs with
    the NEW HWND assigned, before inherited property initialization or visibility;
    unlike idle rebinding it also covers independent editor handle recreation. }
  { Access only to inherited LCL properties; no extra state or ownership. The
    original private standard editor remains the authority for border options
    set through a TStringGrid-typed reference. }
  TCellEditorAccess = class(TStringCellEditor)
  public
    property BorderStyle;
  end;
  { Actual LCL string editor retains its original grid protocol. The borrowed
    observed surface frames input and optional managed context dispatch; it
    owns no mount/renderer and falls back to the native editor menu by default. }
  TColumnCellEditor = class(TCellEditorAccess)
  private
    FLayoutGrid: TNyxCollectionStringGrid;
  public
    InputSurface: INyxGridInputSurface;
    property BorderStyle;
    property Alignment;
    property Text;
    property ParentFont;
    property Color;
  protected
    procedure CreateWnd; override;
    procedure DestroyHandle; override;
    procedure DestroyWnd; override;
    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;
    procedure WndProc(var AMessage: TLMessage); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure SetGrid(var AMessage: TGridMessage); message GM_SETGRID;
    procedure SetCellBounds(var AMessage: TGridMessage); message GM_SETBOUNDS;
  end;

  { A managed activity lease outlives a grid when an entered callback does.
    Its weak queue-only idle wake prevents a deferred resume creating a new
    editor inside the old native tail. Revocation clears that receiver first. }
  TValidationActivity = class(TInterfacedObject, INyxGridValidationActivity)
  private
    FDepth: Integer;
    FIdle: TNotifyEvent;
    FReleaseClock, FReleaseStamp: Integer;
    FDownClock, FDownStamp, FDoubleStamp: Integer;
    FDownIsDouble: Boolean;
    FDoubleClaimed: Boolean;
    function GetDoubleIsCurrentDown: Boolean;
    function GetDoubleOwnsDown: Boolean;
  public
    procedure Enter;
    procedure Leave;
    function GetActive: Boolean;
    procedure ObserveIdle(AReply: TNotifyEvent);
    function BeginRelease: Integer;
    procedure EndRelease(APrevious: Integer);
    function BeginDown(ADouble: Boolean; out APreviousDouble: Boolean): Integer;
    procedure EndDown(APrevious: Integer; APreviousDouble: Boolean);
    function BeginDouble(out APreviousClaimed: Boolean): Integer;
    procedure EndDouble(APrevious: Integer; APreviousClaimed: Boolean);
    procedure ClaimDouble(AStamp: Integer);
    property ReleaseStamp: Integer read FReleaseStamp;
    property DoubleStamp: Integer read FDoubleStamp;
    property DoubleClaimed: Boolean read FDoubleClaimed;
    property DoubleIsCurrentDown: Boolean read GetDoubleIsCurrentDown;
    property DoubleOwnsDown: Boolean read GetDoubleOwnsDown;
  end;
  { Joins the SAME canonical activity to every existing grid/editor/OS frame.
    The renderer's original frame still owns physical retirement/drag handling;
    this transparent private wrapper adds no dispatcher, queue or widget owner.
    Final readiness reads the activity directly and never calls this wrapper. }
  TColumnActivityFrame = class(TInterfacedObject, INyxNativeGestureFrame)
  private
    FInner: INyxNativeGestureFrame;
    FActivity: INyxGridValidationActivity;
  public
    constructor Create(const AInner: INyxNativeGestureFrame;
      const AActivity: INyxGridValidationActivity);
    procedure Enter;
    procedure Leave;
    function Dispatching: Boolean;
    procedure Retire(AComponent: TComponent);
    procedure AbortDrag(AControl: TControl);
  end;
  { One inert epoch is shared only by the grid and entered validation frames.
    Revoke is flags-only; it performs no widget access or application callback. }
  TValidationEpoch = class(TInterfacedObject, INyxGridValidationEpoch)
  private
    FRevoked: Boolean;
  public
    function GetCurrent: Boolean;
    procedure Revoke;
  end;

{ Portable LCL messages and the Win32 compatibility family share these numeric
  IDs. Keep the few missing SDK/LCL declarations named here. Do not broaden this
  list to lifecycle, focus, text-import or caret-import messages: those remain
  necessary while a candidate is staged or a former editor is suspended. }
const
  NyxWMUniChar = $0109;
  NyxUnicodeNoChar = $FFFF;
  NyxWMUndo = $0304;
  NyxEMReplaceSel = $00C2;
  NyxEMUndo = $00C7;
  NyxWMIMEStart = $010D;
  NyxWMIMEEnd = $010E;
  NyxWMIMEComposition = $010F;
  NyxWMIMESetContext = $0281;
  NyxWMIMENotify = $0282;
  NyxWMIMEControl = $0283;
  NyxWMIMEFull = $0284;
  NyxWMIMESelect = $0285;
  NyxWMIMEChar = $0286;
  NyxWMIMERequest = $0288;
  NyxWMIMEKeyDown = $0290;
  NyxWMIMEKeyUp = $0291;
  NyxWMMouseHover = $02A1;
  NyxWMMouseLeave = $02A3;
  NyxWMNCMouseHover = $02A0;
  NyxWMNCMouseLeave = $02A2;
  NyxWMNCMouseLast = $00AD;

function PhysicalInputMessage(AMessage: Cardinal; AWParam: PtrUInt): Boolean;
begin
  { UNICODE_NOCHAR is a capability query, not inserted text. Let the ordinary
    control answer it without falsely advertising an unsupported Unicode path. }

  if (AMessage = NyxWMUniChar) and (AWParam = NyxUnicodeNoChar) then
  begin
    Exit(False);
  end;
  Result := ((AMessage >= LM_KEYFIRST) and (AMessage <= LM_KEYLAST)) or
    (AMessage = NyxWMUniChar) or
    ((AMessage >= LM_MOUSEFIRST) and (AMessage <= LM_MOUSELAST)) or
    ((AMessage >= LM_MOUSEFIRST2) and (AMessage <= LM_MOUSELAST2)) or
    ((AMessage >= LM_NCMOUSEMOVE) and (AMessage <= NyxWMNCMouseLast));

  if Result then
  begin
    Exit;
  end;
  case AMessage of
    CN_KEYDOWN, CN_KEYUP, CN_CHAR, CN_SYSKEYDOWN, CN_SYSKEYUP, CN_SYSCHAR,
    LM_CONTEXTMENU, LM_HSCROLL, LM_VSCROLL, CN_HSCROLL, CN_VSCROLL,
    LM_CUT, LM_COPY, LM_PASTE, LM_CLEAR, NyxWMUndo, NyxEMUndo, NyxEMReplaceSel,
    NyxWMIMEStart, NyxWMIMEEnd, NyxWMIMEComposition, NyxWMIMESetContext,
    NyxWMIMENotify, NyxWMIMEControl, NyxWMIMEFull, NyxWMIMESelect,
    NyxWMIMEChar, NyxWMIMERequest, NyxWMIMEKeyDown, NyxWMIMEKeyUp,
    NyxWMMouseHover, NyxWMMouseLeave, NyxWMNCMouseHover, NyxWMNCMouseLeave:
      Result := True;
  end;
end;

function NewNyxGridInputContinuity: INyxGridInputContinuity;
begin
  Result := TInputContinuity.Create;
end;

function NewValidationActivity(out AObject: TObject): INyxGridValidationActivity;
var
  LActivity: TValidationActivity;
begin
  LActivity := TValidationActivity.Create;
  Result := LActivity;
  AObject := LActivity;
end;

constructor TColumnActivityFrame.Create(const AInner: INyxNativeGestureFrame;
  const AActivity: INyxGridValidationActivity);
begin
  inherited Create;

  if (AInner = nil) or (AActivity = nil) then
  begin
    raise ENyxCollection.Create('Native column activity requires its actual frame and depth owner');
  end;
  FInner := AInner;
  FActivity := AActivity;
end;

procedure TColumnActivityFrame.Enter;
var
  LInner: INyxNativeGestureFrame;
  LActivity: INyxGridValidationActivity;
begin
  LInner := FInner;
  LActivity := FActivity;
  { Activity precedes even an outward frame Enter. Undo it if admission fails. }
  LActivity.Enter;
  try
    LInner.Enter;
  except
    LActivity.Leave;
    raise;
  end;
end;

procedure TColumnActivityFrame.Leave;
var
  LInner: INyxNativeGestureFrame;
  LActivity: INyxGridValidationActivity;
begin
  LInner := FInner;
  LActivity := FActivity;
  try
    LInner.Leave;
  finally
    { Remain active throughout retirement callbacks in the actual frame tail.
      Only after they finish may the existing weak idle wake observe zero. }
    LActivity.Leave;
  end;
end;

function TColumnActivityFrame.Dispatching: Boolean;
var
  LInner: INyxNativeGestureFrame;
begin
  LInner := FInner;
  Result := LInner.Dispatching;
end;

procedure TColumnActivityFrame.Retire(AComponent: TComponent);
var
  LInner: INyxNativeGestureFrame;
begin
  LInner := FInner;
  LInner.Retire(AComponent);
end;

procedure TColumnActivityFrame.AbortDrag(AControl: TControl);
var
  LInner: INyxNativeGestureFrame;
begin
  LInner := FInner;
  LInner.AbortDrag(AControl);
end;

function TInputContinuity.Composing: Boolean;
begin
  Result := FComposing > 0;
end;

function TInputContinuity.PositionsBlocked: Boolean;
begin
  Result := FRetired or FDeferred or (FComposing > 0);
end;

procedure TInputContinuity.BeginComposition;
begin

  if not FRetired and (FComposing < 3) then
  begin
    Inc(FComposing);
  end;
end;

procedure TInputContinuity.EndComposition;
begin

  if not FRetired and (FComposing > 0) then
  begin
    Dec(FComposing);
  end;
end;

procedure TInputContinuity.DeferPositions(ADeferred: Boolean);
begin

  if not FRetired then
  begin
    FDeferred := ADeferred;
  end;
end;

procedure TInputContinuity.Retire;
begin
  FRetired := True;
end;

{$ifdef WINDOWS}
{ SUBCLASSPROC intercepts WM_CHAR before Win32's direct IntfUTF8KeyPress route
  and EM_REPLACESEL before the edit default. Managed leases span exactly one
  forwarding call. No exception, including an exception handler's own failure,
  may cross the external callback boundary. }
function InputSubclass(AWindow: HWND; AMessage: UINT; AWParam: WPARAM;
  ALParam: LPARAM; ASubclassID: UINT_PTR; AData: DWORD_PTR): LRESULT; stdcall;
var
  LSurface: TNativeInputSurface;
  LKeepAlive: INyxGridInputSurface;
begin
  Result := 1;
  try
    LSurface := TNativeInputSurface(Pointer(AData));
    LKeepAlive := LSurface;
    Result := LSurface.NativeMessage(AWindow, AMessage, AWParam, ALParam);
  except
    try
      Application.HandleException(Application);
    except
      { An application error sink cannot unwind an OS window procedure. }
    end;
  end;
end;

function TNativeInputSurface.NativeMessage(AWindow: HWND; AMessage: UINT;
  AWParam: WPARAM; ALParam: LPARAM): LRESULT;
var
  LFrame: INyxNativeGestureFrame;
begin
  LFrame := FFrame;
  LFrame.Enter;
  try

    if (AMessage = WM_NCDESTROY) and (AWindow = FWindow) then
    begin
      DropHandle;
    end
    else if (not Ready or ((FControl is TNyxCollectionStringGrid) and
      not PositionReady)) and PhysicalInputMessage(AMessage, PtrUInt(AWParam)) then
    begin
      Exit(1);
    end;

    if AMessage = NyxWMIMEStart then
    begin
      { Latch BEFORE the observer's router/caret getters. Its outward revision
        getter can request Refresh before the observer emits its START packet. }
      BeginComposition;
    end;
    { Forward lifecycle exactly once, even after revocation. Do not touch the
      borrowed control after native/default code has entered application code. }
    Result := CommCtrl.DefSubclassProc(AWindow, AMessage, AWParam, ALParam);
  finally
    LFrame.Leave;
  end;
end;
{$endif}

constructor TNativeInputSurface.Create(AControl: TWinControl;
  const ALifetime: INyxCollectionInputLifetime;
  const AFrame: INyxNativeGestureFrame;
  const AContinuity: INyxGridInputContinuity);
begin
  inherited Create;

  if (AControl = nil) or (ALifetime = nil) or (AFrame = nil) then
  begin
    raise ENyxCollection.Create('Native table input requires a control, lifetime and frame');
  end;
  FControl := AControl;
  FLifetime := ALifetime;
  FFrame := AFrame;
  FContinuity := AContinuity;
  FConnected := True;
end;

destructor TNativeInputSurface.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TNativeInputSurface.Current: Boolean;
begin
  Result := FConnected and FLifetime.Current;
end;

function TNativeInputSurface.Ready: Boolean;
begin
  Result := FConnected and FLifetime.Ready;
end;

function TNativeInputSurface.Frame: INyxNativeGestureFrame;
begin
  Result := FFrame;
end;

function TNativeInputSurface.Continuity: INyxGridInputContinuity;
begin
  Result := FContinuity;
end;

function TNativeInputSurface.PositionReady: Boolean;
begin
  Result := (FContinuity = nil) or not FContinuity.PositionsBlocked;
end;

function TNativeInputSurface.Composing: Boolean;
begin
  Result := FComposing;
end;

procedure TNativeInputSurface.BeginComposition;
begin

  if Ready and (FControl is TColumnCellEditor) and not FComposing and
    (FContinuity <> nil) then
  begin
    FComposing := True;
    FContinuity.BeginComposition;
  end;
end;

procedure TNativeInputSurface.EndComposition;
begin

  if FComposing then
  begin
    FComposing := False;
    FContinuity.EndComposition;
  end;
end;

function TNativeInputSurface.Retired: Boolean;
begin
  Result := not FConnected;
end;

function TNativeInputSurface.MatchesLifetime(
  const ALifetime: INyxCollectionInputLifetime): Boolean;
begin
  Result := (ALifetime <> nil) and (FLifetime = ALifetime);
end;

procedure TNativeInputSurface.Retire;
begin
  { Leave the borrowed HWND guarded while entered callbacks/queued retirement
    finish. The surviving lease owns no mount, widget or model. }
  FConnected := False;
end;

procedure TNativeInputSurface.CoverWindowProc;
begin

  if not FConnected or FLifetime.Retired or FHooked or (FControl = nil) then
  begin
    raise ENyxCollection.Create('Native input observation requires one live outer guard');
  end;
  FPrevious := FControl.WindowProc;
  FControl.WindowProc := WindowMessage;
  FHooked := True;
end;

procedure TNativeInputSurface.UncoverWindowProc;
var
  LMethod: TWndMethod;
begin

  if FHooked and (FControl <> nil) then
  begin
    LMethod := WindowMessage;

    if (TMethod(FControl.WindowProc).Code = TMethod(LMethod).Code) and
      (TMethod(FControl.WindowProc).Data = TMethod(LMethod).Data) then
    begin
      FControl.WindowProc := FPrevious;
    end;
  end;
  FHooked := False;
  FPrevious := nil;
end;

procedure TNativeInputSurface.WindowMessage(var AMessage: TLMessage);
var
  LKeepAlive: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LPrevious: TWndMethod;
begin
  LKeepAlive := Self;
  LFrame := FFrame;
  LPrevious := FPrevious;
  LFrame.Enter;
  try

    if (not Ready or ((FControl is TNyxCollectionStringGrid) and
      not PositionReady)) and PhysicalInputMessage(AMessage.Msg,
      PtrUInt(AMessage.WParam)) then
    begin
      AMessage.Result := 1;
      Exit;
    end;

    if AMessage.Msg = NyxWMIMEStart then
    begin
      BeginComposition;
    end;

    if Assigned(LPrevious) then
    begin
      LPrevious(AMessage);
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNativeInputSurface.AttachHandle;
begin
  {$ifdef WINDOWS}

  if (FControl = nil) or not FControl.HandleAllocated or
    (WidgetSet = nil) or (WidgetSet.LCLPlatform <> lpWin32) then
  begin
    DropHandle;
    Exit;
  end;

  if FWindow = FControl.Handle then
  begin
    Exit;
  end;
  DropHandle;

  if not CommCtrl.SetWindowSubclass(FControl.Handle, @InputSubclass,
    UINT_PTR(Self), DWORD_PTR(Self)) then
  begin
    raise ENyxCollection.Create('The native table input could not install its message guard');
  end;
  FWindow := FControl.Handle;
  {$endif}
end;

procedure TNativeInputSurface.DropHandle;
{$ifdef WINDOWS}
var
  LWindow: HWND;
{$endif}
begin
  {$ifdef WINDOWS}
  LWindow := FWindow;
  FWindow := 0;

  if LWindow <> 0 then
  begin
    CommCtrl.RemoveWindowSubclass(LWindow, @InputSubclass, UINT_PTR(Self));
  end;
  {$endif}
end;

procedure TNativeInputSurface.Disconnect;
begin
  FConnected := False;
  EndComposition;
  UncoverWindowProc;
  DropHandle;
  FControl := nil;
  { Entered callbacks retain the frame/lifetime until their managed lease ends.
    Neither interface has a backreference to this surface or its component. }
end;

procedure TColumnCellEditor.CreateWnd;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := InputSurface;

  if LSurface = nil then
  begin
    inherited CreateWnd;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    inherited CreateWnd;
  finally
    LFrame.Leave;
  end;
end;

procedure TColumnCellEditor.DestroyHandle;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := InputSurface;

  if LSurface = nil then
  begin
    inherited DestroyHandle;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    inherited DestroyHandle;
  finally
    LFrame.Leave;
  end;
end;

procedure TColumnCellEditor.DestroyWnd;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := InputSurface;

  if LSurface = nil then
  begin
    inherited DestroyWnd;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    inherited DestroyWnd;
  finally
    LFrame.Leave;
  end;
end;

procedure TColumnCellEditor.InitializeWnd;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := InputSurface;

  if LSurface = nil then
  begin
    inherited InitializeWnd;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    LSurface.AttachHandle;
    inherited InitializeWnd;
  finally
    LFrame.Leave;
  end;
end;

procedure TColumnCellEditor.FinalizeWnd;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := InputSurface;

  if LSurface = nil then
  begin
    inherited FinalizeWnd;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    LSurface.DropHandle;
    inherited FinalizeWnd;
  finally
    LFrame.Leave;
  end;
end;

procedure TColumnCellEditor.WndProc(var AMessage: TLMessage);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := InputSurface;

  if LSurface = nil then
  begin
    inherited WndProc(AMessage);
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try

    if not LSurface.Ready and PhysicalInputMessage(AMessage.Msg,
      PtrUInt(AMessage.WParam)) then
    begin
      AMessage.Result := 1;
      Exit;
    end;

    if AMessage.Msg = NyxWMIMEStart then
    begin
      LSurface.BeginComposition;
    end;
    inherited WndProc(AMessage);
  finally
    LFrame.Leave;
  end;
end;

procedure TColumnCellEditor.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LPrepare: TNyxGridCellContextPrepare;
  LDispatch: TNyxGridCellContextDispatch;
  LPrevious: TContextPopupEvent;
  LCommand: INyxCollectionClipboardCommand;
begin
  LSurface := InputSurface;

  if (LSurface = nil) or (FLayoutGrid = nil) then
  begin
    inherited DoContextPopup(MousePos, Handled);
    Exit;
  end;

  if not LSurface.Ready then
  begin
    Handled := True;
    Exit;
  end;
  LPrepare := FLayoutGrid.FCellContextPrepare;
  LDispatch := FLayoutGrid.FCellContextDispatch;
  LPrevious := OnContextPopup;

  if not Assigned(LPrepare) or not Assigned(LDispatch) or
    LSurface.Composing or not LSurface.PositionReady then
  begin
    inherited DoContextPopup(MousePos, Handled);
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    LCommand := LPrepare(Self, MousePos);

    if LCommand = nil then
    begin

      if LSurface.Current then
      begin
        inherited DoContextPopup(MousePos, Handled);
      end
      else
      begin
        Handled := True;
      end;
      Exit;
    end;

    if not LCommand.DispatchCurrent or not LSurface.Current then
    begin
      { Preparation may retire the renderer which supplied this borrowed
        method pointer. Check only retained flags before invoking it. }
      Handled := True;
      Exit;
    end;
    LDispatch(Self, MousePos, Handled, LCommand, LPrevious);
  finally
    try

      if LCommand <> nil then
      begin
        LCommand.Retire;
      end;
    finally
      { LAST: a nested callback can retire this exact editor/grid/mount. }
      LFrame.Leave;
    end;
  end;
end;

procedure TColumnCellEditor.KeyDown(var Key: Word; Shift: TShiftState);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := InputSurface;

  if LSurface = nil then
  begin
    inherited KeyDown(Key, Shift);
    Exit;
  end;

  if not LSurface.Ready then
  begin
    Key := 0;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try

    if (LSurface.Continuity <> nil) and LSurface.Continuity.PositionsBlocked then
    begin
      { Leave Key unchanged for the OS edit/IME. TStringCellEditor's inherited
        method forwards arrows/Escape/Tab to grid navigation and can hide this
        editor; that protocol resumes only after the physical map catches up. }
      Exit;
    end;
    inherited KeyDown(Key, Shift);
  finally
    LFrame.Leave;
  end;
end;

procedure TColumnCellEditor.KeyPress(var Key: Char);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := InputSurface;

  if LSurface = nil then
  begin
    inherited KeyPress(Key);
    Exit;
  end;

  if not LSurface.Ready then
  begin
    Key := #0;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try

    if (LSurface.Continuity <> nil) and LSurface.Continuity.PositionsBlocked then
    begin
      { EO_HOOKKEYPRESS installs the grid's Enter-to-ResetEditor handler. Skip
        that callback while busy; the unmodified character belongs to the IME. }
      Exit;
    end;
    inherited KeyPress(Key);
  finally
    LFrame.Leave;
  end;
end;

procedure TColumnCellEditor.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := InputSurface;

  if LSurface = nil then
  begin
    inherited UTF8KeyPress(UTF8Key);
    Exit;
  end;

  if not LSurface.Ready then
  begin
    UTF8Key := '';
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try

    if (LSurface.Continuity <> nil) and LSurface.Continuity.PositionsBlocked then
    begin
      Exit;
    end;
    inherited UTF8KeyPress(UTF8Key);
  finally
    LFrame.Leave;
  end;
end;

procedure TColumnCellEditor.SetGrid(var AMessage: TGridMessage);
begin
  inherited msg_SetGrid(AMessage);
  FLayoutGrid := nil;

  if AMessage.Grid is TNyxCollectionStringGrid then
  begin
    FLayoutGrid := TNyxCollectionStringGrid(AMessage.Grid);
  end;
  { LCL's private FStringEditor identity controls its EO_AUTOSIZE branch. Use
    the existing custom-editor GM_SETBOUNDS protocol instead, receiving the raw
    clipped cell rectangle and applying the SAME widgetset geometry below. }
  AMessage.Options := AMessage.Options and not EO_AUTOSIZE;
end;

procedure TColumnCellEditor.SetCellBounds(var AMessage: TGridMessage);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LGrid: TNyxCollectionStringGrid;
  LBounds: TRect;
begin
  LSurface := InputSurface;
  LGrid := FLayoutGrid;

  if (LSurface = nil) or not LSurface.Current or (LGrid = nil) then
  begin
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    LGrid.SyncColumnEditorStyle;

    if not LSurface.Current then
    begin
      Exit;
    end;
    LBounds := LGrid.ColumnEditorBounds(AMessage.CellRect, AMessage.Col);
    BoundsRect := LBounds;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.SyncColumnEditorStyle;
var
  LOriginal: TWinControl;
begin

  if (FInputSurface = nil) or (FColumnEditor = nil) then
  begin
    Exit;
  end;
  LOriginal := inherited EditorByStyle(cbsAuto);
  { The inherited EditorBorderStyle getter reports bsSingle for an unknown
    editor. Read the actual standard editor that its nonvirtual setter updates;
    mirror on show, position and painting, including later base-typed changes. }
  TColumnCellEditor(FColumnEditor).BorderStyle :=
    TCellEditorAccess(LOriginal).BorderStyle;
  TColumnCellEditor(FColumnEditor).Alignment := GetColumnAlignment(Col, False);
end;

function TNyxCollectionStringGrid.ColumnEditorBounds(const ACell: TRect;
  AColumn: Integer): TRect;
begin
  Result := ACell;

  if TColumnCellEditor(FColumnEditor).BorderStyle = bsNone then
  begin
    Result := TWSCustomGridClass(WidgetSetClass).GetEditorBoundsFromCellRect(
      Canvas, ACell, GetColumnLayout(AColumn, False));
  end
  else
  begin
    AdjustInnerCellRect(Result);
  end;
end;

procedure TNyxCollectionStringGrid.DoPrepareCanvas(AColumn, ARow: Integer;
  AState: TGridDrawState);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LSelected: Boolean;
begin
  LSurface := FInputSurface;

  if LSurface = nil then
  begin

    if CellRangeSelected(AColumn, ARow) then
    begin
      Include(AState, gdSelected);
    end;
    inherited DoPrepareCanvas(AColumn, ARow, AState);
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    SyncColumnEditorStyle;

    if not LSurface.Current then
    begin
      Exit;
    end;

    if CellRangeSelected(AColumn, ARow) then
    begin
      Include(AState, gdSelected);
    end;
    GetSelectedState(AState, LSelected);

    if LSelected and DefaultDrawing and EditorMode and (AColumn = Col) and
      (Editor = FColumnEditor) and
      (TColumnCellEditor(FColumnEditor).BorderStyle = bsNone) then
    begin
      { Match the private standard-editor fill BEFORE the existing application
        OnPrepareCanvas chain, so custom painting remains the final authority. }
      Canvas.Brush.Color := TColumnCellEditor(FColumnEditor).Color;
    end;
    inherited DoPrepareCanvas(AColumn, ARow, AState);
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.AttachColumnInput(
  const ALifetime: INyxCollectionInputLifetime;
  const AFrame: INyxNativeGestureFrame;
  const AContinuity: INyxGridInputContinuity);
var
  LSurface, LEditorSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin

  if FInputSurface <> nil then
  begin

    if not FInputSurface.Retired then
    begin
      raise ENyxCollection.Create('Native table input already has an owner');
    end;

    if FInputSurface.Frame.Dispatching then
    begin
      { An old LCL editor method still carries private grid/cell state after
        its outward callback returns. Never retarget that SAME component to a
        new mount inside this native tail; ordinary UI-queue adoption can retry
        after it unwinds. Fresh renderer candidates use different controls. }
      raise ENyxCollection.Create('Reattach this native table after its entered input calls return');
    end;
    DetachColumnInput;
  end;

  if Assigned(OnSelectEditor) then
  begin
    raise ENyxCollection.Create('Arranged table input requires its owned string-cell editor');
  end;

  if EditorMode or ((Editor <> nil) and Editor.Visible) then
  begin
    raise ENyxCollection.Create('Attach arranged table input before opening a native editor');
  end;

  if FValidationActivity = nil then
  begin
    FValidationActivity := NewValidationActivity(FValidationActivityObject);
  end;
  LFrame := TColumnActivityFrame.Create(AFrame, FValidationActivity);
  LSurface := TNativeInputSurface.Create(Self, ALifetime, LFrame, AContinuity);

  if FColumnEditor = nil then
  begin
    FColumnEditor := TColumnCellEditor.Create(Self);
    TColumnCellEditor(FColumnEditor).Text := '';
    TColumnCellEditor(FColumnEditor).ParentFont := False;
    TColumnCellEditor(FColumnEditor).BorderStyle := bsNone;
    FColumnEditor.Visible := False;
  end;
  LEditorSurface := TNativeInputSurface.Create(FColumnEditor, ALifetime, LFrame, AContinuity);
  try
    LSurface.AttachHandle;
    LEditorSurface.AttachHandle;
  except
    LEditorSurface.Disconnect;
    LSurface.Disconnect;
    raise;
  end;
  TColumnCellEditor(FColumnEditor).InputSurface := LEditorSurface;
  FInputSurface := LSurface;
end;

procedure TNyxCollectionStringGrid.RetireColumnInput(
  const ALifetime: INyxCollectionInputLifetime);
var
  LSurface: INyxGridInputSurface;
begin
  LSurface := FInputSurface;

  if (LSurface <> nil) and LSurface.MatchesLifetime(ALifetime) then
  begin
    FCellContextPrepare := nil;
    FCellContextDispatch := nil;
    LSurface.Retire;
  end;

  if FColumnEditor <> nil then
  begin
    LSurface := TColumnCellEditor(FColumnEditor).InputSurface;

    if (LSurface <> nil) and LSurface.MatchesLifetime(ALifetime) then
    begin
      LSurface.Retire;
    end;
  end;
end;

function TNyxCollectionStringGrid.ColumnEditorInputSurface: INyxGridInputSurface;
begin
  Result := nil;

  if FColumnEditor <> nil then
  begin
    Result := TColumnCellEditor(FColumnEditor).InputSurface;
  end;
end;

procedure TNyxCollectionStringGrid.ConfigureCellContext(
  const ALifetime: INyxCollectionInputLifetime;
  APrepare: TNyxGridCellContextPrepare; ADispatch: TNyxGridCellContextDispatch);
begin

  if (FInputSurface = nil) or not FInputSurface.MatchesLifetime(ALifetime) then
  begin
    raise ENyxCollection.Create('Editor context requires its exact observed grid incarnation');
  end;

  if (Assigned(APrepare) or Assigned(ADispatch)) and
    (FInputSurface.Retired or ALifetime.Retired) then
  begin
    raise ENyxCollection.Create('Retired editor context cannot acquire new dispatch ports');
  end;
  FCellContextPrepare := APrepare;
  FCellContextDispatch := ADispatch;
end;

procedure TNyxCollectionStringGrid.DetachColumnInput;
var
  LSurface: INyxGridInputSurface;
begin
  FCellContextPrepare := nil;
  FCellContextDispatch := nil;

  if FInputSurface <> nil then
  begin
    LSurface := FInputSurface;
    FInputSurface := nil;
    LSurface.Disconnect;
  end;

  if FColumnEditor <> nil then
  begin
    LSurface := TColumnCellEditor(FColumnEditor).InputSurface;
    TColumnCellEditor(FColumnEditor).InputSurface := nil;

    if LSurface <> nil then
    begin
      LSurface.Disconnect;
    end;
  end;
end;

function TNyxCollectionStringGrid.EditorByStyle(
  Style: TColumnButtonStyle): TWinControl;
begin

  if FInputSurface <> nil then
  begin

    if Style <> cbsAuto then
    begin
      raise ENyxCollection.Create('Arranged table input requires its owned string-cell editor');
    end;
    Exit(FColumnEditor);
  end;
  Result := inherited EditorByStyle(Style);
end;

procedure TNyxCollectionStringGrid.CreateWnd;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := FInputSurface;

  if LSurface = nil then
  begin
    inherited CreateWnd;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    inherited CreateWnd;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.DestroyHandle;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := FInputSurface;

  if LSurface = nil then
  begin
    inherited DestroyHandle;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    { TWinControl destroys child handles before DestroyWnd. TCustomGrid also
      reads/hides its editor AFTER inherited DestroyHandle. Keep the exact
      frame across that complete operation, not merely the HWND destructor. }
    inherited DestroyHandle;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.DestroyWnd;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := FInputSurface;

  if LSurface = nil then
  begin
    inherited DestroyWnd;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    inherited DestroyWnd;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.InitializeWnd;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := FInputSurface;

  if LSurface = nil then
  begin
    inherited InitializeWnd;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    LSurface.AttachHandle;
    inherited InitializeWnd;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.FinalizeWnd;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := FInputSurface;

  if LSurface = nil then
  begin
    inherited FinalizeWnd;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    LSurface.DropHandle;
    inherited FinalizeWnd;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.WndProc(var AMessage: TLMessage);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LActivity: INyxGridValidationActivity;
  LDouble: TValidationActivity;
  LPrevious: Integer;
  LDoubleMessage: Boolean;
  LPreviousClaimed: Boolean;
  LDownMessage: Boolean;
  LPreviousDown: Integer;
  LPreviousDownDouble: Boolean;
begin

  if FInputSurface = nil then
  begin
    inherited WndProc(AMessage);
    Exit;
  end;
  LSurface := FInputSurface;
  LActivity := FValidationActivity;
  LDouble := nil;

  if LActivity <> nil then
  begin
    LDouble := TValidationActivity(FValidationActivityObject);
  end;
  LDoubleMessage := AMessage.Msg = LM_LBUTTONDBLCLK;
  case AMessage.Msg of
    LM_LBUTTONDOWN, LM_LBUTTONDBLCLK, LM_LBUTTONTRIPLECLK, LM_LBUTTONQUADCLK,
    LM_RBUTTONDOWN, LM_RBUTTONDBLCLK, LM_RBUTTONTRIPLECLK, LM_RBUTTONQUADCLK,
    LM_MBUTTONDOWN, LM_MBUTTONDBLCLK, LM_MBUTTONTRIPLECLK, LM_MBUTTONQUADCLK,
    LM_XBUTTONDOWN, LM_XBUTTONDBLCLK, LM_XBUTTONTRIPLECLK, LM_XBUTTONQUADCLK:
      LDownMessage := True;
  else
    LDownMessage := False;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try

    if (not LSurface.Ready or not LSurface.PositionReady) and PhysicalInputMessage(AMessage.Msg,
      PtrUInt(AMessage.WParam)) then
    begin
      AMessage.Result := 1;
      Exit;
    end;

    if LDownMessage and (FColumnToolbarHeight > 0) and
      (SmallInt((PtrUInt(AMessage.LParam) shr 16) and $FFFF) >= 0) and
      (SmallInt((PtrUInt(AMessage.LParam) shr 16) and $FFFF) < FColumnToolbarHeight) then
    begin
      { Toolbar children receive their own HWND input. Blank band space is not
        a sortable header, resizing boundary or positional data-cell command. }
      AMessage.Result := 1;
      Exit;
    end;
    LPrevious := 0;
    LPreviousClaimed := False;
    LPreviousDown := 0;
    LPreviousDownDouble := False;

    if LDownMessage and (LDouble <> nil) then
    begin
      LPreviousDown := LDouble.BeginDown(LDoubleMessage, LPreviousDownDouble);
    end;

    if LDoubleMessage and (LDouble <> nil) then
    begin
      LPrevious := LDouble.BeginDouble(LPreviousClaimed);
    end;
    try
      inherited WndProc(AMessage);
    finally

      if LDoubleMessage and (LDouble <> nil) then
      begin
        { LActivity owns this independent state even after a callback retires
          the widget. Restore through locals only, before the native frame ends. }
        LDouble.EndDouble(LPrevious, LPreviousClaimed);
      end;

      if LDownMessage and (LDouble <> nil) then
      begin
        LDouble.EndDown(LPreviousDown, LPreviousDownDouble);
      end;
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.SetColumnToolbarHeight(AHeight: Integer);
begin

  if (AHeight < 0) or (AHeight > 4096) then
  begin
    raise ENyxCollection.Create('Native column toolbar height is outside physical bounds');
  end;
  FColumnToolbarHeight := AHeight;
end;

function TNyxCollectionStringGrid.ColumnHeaderRect(AColumn: Integer): TRect;
begin
  Result := CellRect(AColumn, 0);
  Result.Top := Min(Result.Bottom, Result.Top + FColumnToolbarHeight);
end;

procedure TNyxCollectionStringGrid.DrawCellText(AColumn, ARow: Integer;
  ARect: TRect; AState: TGridDrawState; AText: String);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LLayout: TNyxTableTextLayout;
  LStyle: TTextStyle;
  LLine: Integer;
  LHeight: Integer;
  LRect: TRect;
  LHasLayout: Boolean;
begin
  LSurface := FInputSurface;
  LFrame := nil;

  if LSurface <> nil then
  begin
    LFrame := LSurface.Frame;
    LFrame.Enter;
  end;
  try

    if (LSurface <> nil) and not LSurface.Current then
    begin
      Exit;
    end;

    LHasLayout := (ARow > 0) and CellTextLayout(AColumn, ARow, LLayout);

    if not LHasLayout and Assigned(FDisplayReader) then
    begin
      AText := DisplayCellText(AColumn, ARow);
    end;

    if (LSurface <> nil) and not LSurface.Current then
    begin
      Exit;
    end;

    if (ARow = 0) and (FColumnToolbarHeight > 0) then
    begin
      ARect.Top := Min(ARect.Bottom, ARect.Top + FColumnToolbarHeight);
    end;
    { Lines have already been admitted against this content clip and copied
      font. The physical rectangle still clips an overwide grapheme sequence.
      Accepted editor text and the canonical Cells reader remain independent. }

    if LHasLayout then
    begin
      LStyle := Canvas.TextStyle;
      LStyle.SingleLine := True;
      LStyle.Wordbreak := False;
      LStyle.EndEllipsis := False;
      LStyle.Clipping := True;
      LStyle.Layout := tlTop;
      LHeight := Max(1, Canvas.TextHeight('Ag'));
      LRect := ARect;
      Inc(LRect.Left, varCellPadding);
      Dec(LRect.Right, varCellPadding);
      Inc(LRect.Top, Max(0, (ARect.Height - LHeight * LLayout.Count) div 2));
      for LLine := 0 to LLayout.Count - 1 do
      begin
        LRect.Bottom := Min(ARect.Bottom, LRect.Top + LHeight);
        Canvas.TextRect(LRect, LRect.Left, LRect.Top,
          LLayout.LineAt(LLine), LStyle);
        Inc(LRect.Top, LHeight);
      end;
      Exit;
    end;
    inherited DrawCellText(AColumn, ARow, ARect, AState, AText);
  finally

    if LFrame <> nil then
    begin
      LFrame.Leave;
    end;
  end;
end;

procedure TNyxCollectionStringGrid.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin

  if (FColumnToolbarHeight > 0) and (Y >= 0) and (Y < FColumnToolbarHeight) then
  begin
    Exit;
  end;
  LSurface := FInputSurface;

  if LSurface = nil then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    inherited MouseDown(Button, Shift, X, Y);
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.SelectEditor;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LEditor: TWinControl;
begin

  if FInputSurface = nil then
  begin
    inherited SelectEditor;
    Exit;
  end;
  LSurface := FInputSurface;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try

    if not LSurface.Current then
    begin
      Exit;
    end;

    if Assigned(OnSelectEditor) then
    begin
      raise ENyxCollection.Create('Arranged table input refuses an unguarded replacement editor');
    end;
    LEditor := GetDefaultEditor(Col);

    if (LEditor <> nil) and (LEditor <> FColumnEditor) then
    begin
      raise ENyxCollection.Create('Arranged table input refuses an unguarded replacement editor');
    end;
    inherited SelectEditor;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.EditorShow(const SelAll: Boolean);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LShown: TNotifyEvent;
begin
  LSurface := FInputSurface;

  if LSurface = nil then
  begin
    inherited EditorShow(SelAll);
    Exit;
  end;
  LFrame := LSurface.Frame;
  LShown := FColumnEditorShown;
  LFrame.Enter;
  try

    if not LSurface.Current then
    begin
      Exit;
    end;
    { Covers the complete inherited mode/show/select-all tail. Controlled draft
      import needs Current; CanEditShow's existing pure reader admits that case.
      Physical messages still require Ready in the earlier OS/LCL guards. }
    inherited EditorShow(SelAll);

    if LSurface.Ready and LSurface.PositionReady and Assigned(LShown) then
    begin
      { Last receiver call. The retained original frame owns all remaining
        component/validator tails even when the callback retires this grid. }
      LShown(Self);
    end;
  finally
    LFrame.Leave;
  end;
end;


procedure TNyxCollectionStringGrid.DoEditorShow;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin

  if FInputSurface = nil then
  begin
    inherited DoEditorShow;
    Exit;
  end;
  LSurface := FInputSurface;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try

    if not LSurface.Current then
    begin
      Exit;
    end;

    if Editor <> FColumnEditor then
    begin
      raise ENyxCollection.Create('Arranged table input refuses an unguarded active editor');
    end;
    SyncColumnEditorStyle;

    if not LSurface.Current then
    begin
      Exit;
    end;
    inherited DoEditorShow;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := FInputSurface;

  if LSurface = nil then
  begin
    inherited UTF8KeyPress(UTF8Key);
    Exit;
  end;

  if not LSurface.Ready or not LSurface.PositionReady then
  begin
    UTF8Key := '';
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    inherited UTF8KeyPress(UTF8Key);
  finally
    LFrame.Leave;
  end;
end;

procedure TValidationActivity.Enter;
begin

  if FDepth = High(Integer) then
  begin
    raise ENyxCollection.Create('Native validation depth is exhausted');
  end;
  Inc(FDepth);
end;

procedure TValidationActivity.Leave;
var
  LReply: TNotifyEvent;
begin

  if FDepth > 0 then
  begin
    Dec(FDepth);

    if FDepth = 0 then
    begin
      LReply := FIdle;

      if Assigned(LReply) then
      begin
        { Queue-only internal wake is the final call; no widget is retained or
          borrowed by this activity lease. Teardown clears the weak receiver. }
        LReply(nil);
      end;
    end;
  end;
end;

procedure TValidationActivity.ObserveIdle(AReply: TNotifyEvent);
begin
  FIdle := AReply;
end;

function TValidationActivity.BeginRelease: Integer;
begin
  Result := FReleaseStamp;

  if FReleaseClock < High(Integer) then
  begin
    Inc(FReleaseClock);
  end;
  { Exhaustion never wraps. High stamps refuse width admission while inherited
    MouseUp still finishes its native sizing cleanup exactly once. }
  FReleaseStamp := FReleaseClock;
end;

procedure TValidationActivity.EndRelease(APrevious: Integer);
begin
  FReleaseStamp := APrevious;
end;

function TValidationActivity.BeginDown(ADouble: Boolean;
  out APreviousDouble: Boolean): Integer;
begin
  Result := FDownStamp;
  APreviousDouble := FDownIsDouble;
  FDownIsDouble := ADouble;

  if FDownClock < High(Integer) then
  begin
    Inc(FDownClock);
  end;
  FDownStamp := FDownClock;
end;

procedure TValidationActivity.EndDown(APrevious: Integer; APreviousDouble: Boolean);
begin
  FDownStamp := APrevious;
  FDownIsDouble := APreviousDouble;
end;

function TValidationActivity.GetDoubleIsCurrentDown: Boolean;
begin
  { Message kind remains explicit at saturated stamps: ordinary/right/middle/X
    input must not inherit an outer left-double claim merely by equal numbers. }
  Result := FDownIsDouble and (FDoubleStamp > 0) and (FDoubleStamp = FDownStamp);
end;

function TValidationActivity.GetDoubleOwnsDown: Boolean;
begin
  Result := GetDoubleIsCurrentDown and (FDoubleStamp < High(Integer)) and
    (FDoubleStamp = FDownClock);
end;

function TValidationActivity.BeginDouble(out APreviousClaimed: Boolean): Integer;
begin
  Result := FDoubleStamp;
  APreviousClaimed := FDoubleClaimed;
  FDoubleClaimed := False;

  FDoubleStamp := FDownStamp;
end;

procedure TValidationActivity.EndDouble(APrevious: Integer; APreviousClaimed: Boolean);
begin
  FDoubleStamp := APrevious;
  FDoubleClaimed := APreviousClaimed;
end;

procedure TValidationActivity.ClaimDouble(AStamp: Integer);
begin

  if (AStamp > 0) and (AStamp = FDoubleStamp) then
  begin
    FDoubleClaimed := True;
  end;
end;

function TValidationActivity.GetActive: Boolean;
begin
  Result := FDepth > 0;
end;

function TNyxCollectionStringGrid.GetValidationActive: Boolean;
begin
  Result := (FValidationActivity <> nil) and FValidationActivity.GetActive;
end;

function TNyxCollectionStringGrid.ValidationActivityLease: INyxGridValidationActivity;
begin
  Result := FValidationActivity;
end;

function TNyxCollectionStringGrid.ColumnReleaseStamp: Integer;
begin
  Result := 0;

  if FValidationActivityObject <> nil then
  begin
    Result := TValidationActivity(FValidationActivityObject).ReleaseStamp;
  end;
end;

function TNyxCollectionStringGrid.ColumnDoubleClickStamp: Integer;
begin
  Result := 0;

  if FValidationActivityObject <> nil then
  begin
    Result := TValidationActivity(FValidationActivityObject).DoubleStamp;
  end;
end;

procedure TNyxCollectionStringGrid.ObserveColumnEditor(AReply: TNotifyEvent);
begin
  FColumnEditorShown := AReply;
end;

procedure TNyxCollectionStringGrid.ObserveColumnDoubleClick(
  AReply: TNyxGridColumnDoubleClick);
begin
  FColumnDoubleClick := AReply;
end;

procedure TNyxCollectionStringGrid.ClaimColumnDoubleClick(AStamp: Integer);
begin

  if FValidationActivityObject <> nil then
  begin
    TValidationActivity(FValidationActivityObject).ClaimDouble(AStamp);
  end;
end;

procedure TNyxCollectionStringGrid.ObserveValidationIdle(AReply: TNotifyEvent);
begin

  if FValidationActivity <> nil then
  begin
    FValidationActivity.ObserveIdle(AReply);
  end;
end;

procedure TNyxCollectionStringGrid.CommitColumnEditor;
begin
  ResetEditor;
end;

procedure TNyxCollectionStringGrid.BeginPresentationChange;
begin

  if FPresentationChanges = High(Integer) then
  begin
    raise EInvalidOperation.Create('Native grid presentation nesting is exhausted');
  end;
  Inc(FPresentationChanges);
end;

procedure TNyxCollectionStringGrid.EndPresentationChange;
begin

  if FPresentationChanges = 0 then
  begin
    raise EInvalidOperation.Create('Native grid presentation guard is not entered');
  end;
  Dec(FPresentationChanges);
end;

function TNyxCollectionStringGrid.CancelColumnSizing(
  const AInput: INyxCollectionInputLifetime; AEpoch: Integer): Boolean;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  Result := False;
  LSurface := FInputSurface;

  if (LSurface = nil) or (AInput = nil) or
    not LSurface.MatchesLifetime(AInput) or not LSurface.Current or
    not LSurface.Ready or not LSurface.PositionReady or
    (AEpoch <= 0) or (AEpoch = High(Integer)) or (AInput.Epoch <> AEpoch) then
  begin
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try

    if (FInputSurface <> LSurface) or not LSurface.Current or
      not LSurface.Ready or not LSurface.PositionReady or
      (AInput.Epoch <> AEpoch) or (fGridState <> gsColSizing) then
    begin
      Exit;
    end;
    { LCL's later real MouseUp must see neither sizing nor a pending Click.
      SelectActive=False short-circuits the inherited edit-permission query;
      cancellation deliberately leaves the dormant cell draft untouched. }
    fGridState := gsNormal;
    GridFlags := GridFlags - [gfSizingStarted, gfNeedsSelectActive, gfAutoEditPending];
    SelectActive := False;
    ControlState := ControlState - [csClicked];
    Result := True;
    { Queue the ordinary repaint without a descendant Invalidate override.
      Releasing only this control's actual capture is the final physical call:
      a capture callback may retire this component or start another gesture. }
    inherited Invalidate;
    ReleaseNyxLCLPointer(Self);
  finally
    LFrame.Leave;
  end;
end;

{ These entered native frames share the validation activity. A queue-only idle
  wake is emitted AFTER inherited returns, not from OnMouseUp before LCL's
  final ResizeColumn, nor from an application callback pumping native messages.
  Each finally uses a managed local only; detach revokes its weak idle receiver. }
procedure TNyxCollectionStringGrid.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LActivity: INyxGridValidationActivity;
  LRelease: TValidationActivity;
  LPreviousStamp: Integer;
begin
  LActivity := FValidationActivity;
  LRelease := TValidationActivity(FValidationActivityObject);

  if LActivity <> nil then
  begin
    LActivity.Enter;
  end;
  try
    LPreviousStamp := 0;

    if LRelease <> nil then
    begin
      LPreviousStamp := LRelease.BeginRelease;
    end;
    try
      inherited MouseUp(Button, Shift, X, Y);
    finally
      { LActivity retains this independent owner even when inherited callbacks
        retire the grid. Restore before the queue-only idle wake; no Self read. }

      if LRelease <> nil then
      begin
        LRelease.EndRelease(LPreviousStamp);
      end;
    end;
  finally

    if LActivity <> nil then
    begin
      LActivity.Leave;
    end;
  end;
end;

procedure TNyxCollectionStringGrid.DblClick;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LReply: TNyxGridColumnDoubleClick;
  LStamp: Integer;
  LClaimed: Boolean;
  LActivity: INyxGridValidationActivity;
  LDouble: TValidationActivity;
begin
  LSurface := FInputSurface;
  LActivity := FValidationActivity;
  LDouble := nil;

  if LActivity <> nil then
  begin
    LDouble := TValidationActivity(FValidationActivityObject);
  end;

  if LSurface = nil then
  begin
    inherited DblClick;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try

    LReply := FColumnDoubleClick;
    LStamp := 0;
    LClaimed := (LDouble <> nil) and LDouble.DoubleClaimed and
      LDouble.DoubleIsCurrentDown;

    if LClaimed then
    begin

      if not LDouble.DoubleOwnsDown or not LSurface.Ready or
        not LSurface.PositionReady then
      begin
        { Consumption survives retirement/supersession, but an old tail owns
          neither the newer grid state nor a canonical callback. High refuses. }
        Exit;
      end;
      LStamp := LDouble.DoubleStamp;
      { Preserve LCL's bounded double-click state reset BEFORE a callback.
        Its inherited branch may swallow header edges or call unbounded native
        AutoAdjustColumn; an owned sampled Fit command consumes that branch. }
      SelectActive := False;
      fGridState := gsNormal;

      if Assigned(LReply) then
      begin
        LReply(Self, LStamp);
      end;
      { A superseded old request still owns this native tail. Never fall back
        to unbounded LCL sizing on a newer map merely because its slot changed. }
      Exit;
    end;

    if LSurface.Ready and LSurface.PositionReady then
    begin
      inherited DblClick;
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  LActivity: INyxGridValidationActivity;
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := FInputSurface;

  if LSurface <> nil then
  begin

    if not LSurface.Ready or not LSurface.PositionReady then
    begin
      Key := 0;
      Exit;
    end;
    LFrame := LSurface.Frame;
    LFrame.Enter;
  end;
  try
    LActivity := FValidationActivity;

    if LActivity <> nil then
    begin
      LActivity.Enter;
    end;
    try
      inherited KeyDown(Key, Shift);
    finally

      if LActivity <> nil then
      begin
        LActivity.Leave;
      end;
    end;
  finally

    if LFrame <> nil then
    begin
      LFrame.Leave;
    end;
  end;
end;

procedure TNyxCollectionStringGrid.Click;
begin

  if ColumnTransitionActive or ((FInputSurface <> nil) and not FInputSurface.Ready) then
  begin
    { LCL Col/Row setters synthesize Click after moving the cursor. Internal
      restoration/navigation and a refreshing/retired original input must not
      dispatch another application action. The surface reads canonical flags. }
    Exit;
  end;
  inherited Click;
end;

procedure TNyxCollectionStringGrid.DoEnter;
var
  LActivity: INyxGridValidationActivity;
begin
  LActivity := FValidationActivity;

  if LActivity <> nil then
  begin
    LActivity.Enter;
  end;
  try
    inherited DoEnter;
  finally

    if LActivity <> nil then
    begin
      LActivity.Leave;
    end;
  end;
end;

procedure TNyxCollectionStringGrid.DoExit;
var
  LActivity: INyxGridValidationActivity;
begin
  LActivity := FValidationActivity;

  if LActivity <> nil then
  begin
    LActivity.Enter;
  end;
  try
    inherited DoExit;
  finally

    if LActivity <> nil then
    begin
      LActivity.Leave;
    end;
  end;
end;

function TValidationEpoch.GetCurrent: Boolean;
begin
  Result := not FRevoked;
end;

procedure TValidationEpoch.Revoke;
begin
  FRevoked := True;
end;

procedure TNyxCollectionStringGrid.AdvanceColumnMap;
var
  LNext: INyxGridValidationEpoch;
begin
  LNext := TValidationEpoch.Create;

  if FValidationEpoch <> nil then
  begin
    FValidationEpoch.Revoke;
  end;
  FValidationEpoch := LNext;
end;

function TNyxCollectionStringGrid.ValidateEntry(const ACol, ARow: Integer;
  const OldValue: String; var NewValue: String): Boolean;
var
  LEpoch: INyxGridValidationEpoch;
  LActivity: INyxGridValidationActivity;
  LEntry: String;
begin

  if FPresentationChanges <> 0 then
  begin
    { EditorGetValue initialized NewValue from its current entry immediately
      before calling ValidateEntry. Leave that exact argument untouched: False
      then suppresses BOTH its write and its EditorHide/reset tail, including
      legacy grids. This refusal performs no reader or application callout. }
    Exit(False);
  end;
  LEpoch := FValidationEpoch;

  if LEpoch = nil then
  begin
    Exit(inherited ValidateEntry(ACol, ARow, OldValue, NewValue));
  end;
  LEntry := GetCells(ACol, ARow);
  LActivity := FValidationActivity;
  LActivity.Enter;
  try
    Result := inherited ValidateEntry(ACol, ARow, OldValue, NewValue);
  finally
    LActivity.Leave;
  end;

  if not LEpoch.Current then
  begin
    { LCL read the same CurValue immediately before ValidateEntry. Returning
      that exact text AND False suppresses its SetEditText, EditorDoGetValue
      and EditorHide tails. No grid field is borrowed after the retired call. }
    NewValue := LEntry;
    Result := False;
  end;
end;

destructor TNyxCollectionStringGrid.Destroy;
var
  LIndex: Integer;
begin
  DetachColumnInput;

  if FValidationActivity <> nil then
  begin
    FValidationActivity.ObserveIdle(nil);
  end;

  if FValidationEpoch <> nil then
  begin
    FValidationEpoch.Revoke;
    FValidationEpoch := nil;
  end;
  FReader := nil;
  FDisplayReader := nil;
  FEditReader := nil;
  { The extra editor has exactly one component owner. Deselect it while the
    grid protocol is intact, then free it before inherited frees its OWN
    standard/composite editors. Never leave FEditor borrowing a freed object. }

  if FColumnEditor <> nil then
  begin

    if Editor = FColumnEditor then
    begin
      EditorMode := False;
      Editor := nil;
    end;
    FreeAndNil(FColumnEditor);
  end;
  { Revoke raw peer links before inherited component destruction can release
    child controls. This cleanup invokes no reader or application event. }

  if FColumnOwner <> nil then
  begin
    FColumnOwner.UnregisterColumnPane(Self);
    FColumnOwner := nil;
  end;
  for LIndex := 0 to High(FColumnPanes) do
  begin
    FColumnPanes[LIndex].FColumnOwner := nil;
  end;
  FColumnPanes := nil;
  inherited Destroy;
end;

function TNyxCollectionStringGrid.CanEditShow: Boolean;
begin

  if Assigned(FEditReader) and not FEditReader(Col, Row) then
  begin
    Exit(False);
  end;
  Result := inherited CanEditShow;
end;

function TNyxCollectionStringGrid.MouseButtonAllowed(Button: TMouseButton): Boolean;
var
  LDouble: TValidationActivity;
begin
  { LCL asks after the outward OnMouseDown callback and before its grid default.
    A callback-started composition must not be followed by sizing/navigation. }
  Result := (FInputSurface = nil) or
    (FInputSurface.Ready and FInputSurface.PositionReady);
  LDouble := TValidationActivity(FValidationActivityObject);

  if Result and (LDouble <> nil) and LDouble.DoubleClaimed and
    LDouble.DoubleIsCurrentDown then
  begin
    { A nested NEW down remains eligible in its own frame. After it returns,
      the old stamp restores but the clock does not: refuse OLD defaults. }
    Result := LDouble.DoubleOwnsDown;
  end;

  if Result then
  begin
    Result := inherited MouseButtonAllowed(Button);
  end;
end;

function TNyxCollectionStringGrid.CanGridAcceptKey(Key: Word;
  Shift: TShiftState): Boolean;
begin
  { This existing final LCL seam follows OnKeyDown. It covers a composition
    started inside the callback, in addition to the earlier OS/LCL entry guard. }
  Result := (FInputSurface = nil) or
    (FInputSurface.Ready and FInputSurface.PositionReady);

  if Result then
  begin
    Result := inherited CanGridAcceptKey(Key, Shift);
  end;
end;

function TNyxCollectionStringGrid.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  LOwner: TNyxCollectionStringGrid;
begin
  LOwner := FColumnOwner;

  if LOwner <> nil then
  begin
    { The ordinary outer wheel event chain/default runs exactly once. Never
      replay a pane's independent scroll
      or bubble an unhandled event back through this same parent afterward.
      No pane/owner field is touched after the outward event chain. }
    MousePos := LOwner.ScreenToClient(ClientToScreen(MousePos));
    LOwner.DoMouseWheel(Shift, WheelDelta, MousePos);
    Exit(True);
  end;
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TNyxCollectionStringGrid.DoMouseWheelHorz(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  LOwner: TNyxCollectionStringGrid;
begin
  LOwner := FColumnOwner;

  if LOwner <> nil then
  begin
    MousePos := LOwner.ScreenToClient(ClientToScreen(MousePos));
    LOwner.DoMouseWheelHorz(Shift, WheelDelta, MousePos);
    Exit(True);
  end;
  Result := inherited DoMouseWheelHorz(Shift, WheelDelta, MousePos);
end;

procedure TNyxCollectionStringGrid.ClearOverrides;
begin
  FOverrides := nil;
end;

procedure TNyxCollectionStringGrid.AttachEditReader(AReader: TNyxGridEditReader);
begin

  if not Assigned(AReader) or Assigned(FEditReader) then
  begin
    raise ENyxCollection.Create('Grid requires one exclusive live edit reader');
  end;
  FEditReader := AReader;

  if FValidationActivity = nil then
  begin
    { Arranged input created this owner before its native surfaces. Preserve
      that identity; replacing it here would lose entered OS/editor frames. }
    FValidationActivity := NewValidationActivity(FValidationActivityObject);
  end;
  AdvanceColumnMap;
end;

procedure TNyxCollectionStringGrid.DetachEditReader;
begin
  FColumnDoubleClick := nil;
  FColumnEditorShown := nil;

  if FValidationActivity <> nil then
  begin
    FValidationActivity.ObserveIdle(nil);
  end;

  if FValidationEpoch <> nil then
  begin
    FValidationEpoch.Revoke;
    FValidationEpoch := nil;
  end;
  FEditReader := nil;
  FValidationActivityObject := nil;
  FValidationActivity := nil;
end;

function TNyxCollectionStringGrid.GetHasColumnPanes: Boolean;
begin
  Result := Length(FColumnPanes) > 0;
end;

function TNyxCollectionStringGrid.GetHasColumnComposite: Boolean;
begin
  Result := (Length(FColumnPanes) > 0) or (Length(FColumnButtons) > 0);
end;

function TNyxCollectionStringGrid.GetColumnTransitionActive: Boolean;
begin
  Result := FColumnTransition > 0;
end;

function TNyxCollectionStringGrid.ColumnTransitionHadFocus: Boolean;
begin
  Result := (FColumnTransition > 0) and FColumnTransitionFocused;
end;

procedure TNyxCollectionStringGrid.BeginColumnTransition;
begin

  if FColumnTransition = High(Integer) then
  begin
    raise ENyxCollection.Create('Column transition depth is exhausted');
  end;

  if FColumnTransition = 0 then
  begin
    FColumnTransitionFocused := HasOwnedColumnFocus;
  end;
  Inc(FColumnTransition);
end;

procedure TNyxCollectionStringGrid.EndColumnTransition;
var
  LFocused: Boolean;
  LReply: TNotifyEvent;
begin

  if FColumnTransition = 0 then
  begin
    Exit;
  end;
  Dec(FColumnTransition);

  if FColumnTransition > 0 then
  begin
    Exit;
  end;
  LFocused := HasOwnedColumnFocus;

  if LFocused = FColumnTransitionFocused then
  begin
    Exit;
  end;
  LReply := OnExit;

  if LFocused then
  begin
    LReply := OnEnter;
  end;

  if Assigned(LReply) then
  begin
    { Final outward call. A receiver can retire the table; no widget tail or
      pointer access follows it in this owned transition helper. }
    LReply(Self);
  end;
end;

procedure TNyxCollectionStringGrid.RegisterColumnPane(APane: TNyxCollectionStringGrid);
var
  LCount: Integer;
begin
  LCount := Length(FColumnPanes);

  if (APane = nil) or (APane = Self) or (APane.Parent <> Self) or
    (APane.FColumnOwner <> nil) or (LCount >= 2) then
  begin
    raise ENyxCollection.Create('A table edge pane requires exclusive bounded outer-grid ownership');
  end;
  SetLength(FColumnPanes, LCount + 1);
  FColumnPanes[LCount] := APane;
  APane.FColumnOwner := Self;
end;

procedure TNyxCollectionStringGrid.UnregisterColumnPane(APane: TNyxCollectionStringGrid);
var
  LIndex, LMove: Integer;
begin
  for LIndex := 0 to High(FColumnPanes) do
  begin

    if FColumnPanes[LIndex] = APane then
    begin
      APane.FColumnOwner := nil;
      for LMove := LIndex to High(FColumnPanes) - 1 do
      begin
        FColumnPanes[LMove] := FColumnPanes[LMove + 1];
      end;
      SetLength(FColumnPanes, Length(FColumnPanes) - 1);
      Exit;
    end;
  end;
end;

function TNyxCollectionStringGrid.OwnsColumnInput(AControl: TControl): Boolean;
var
  LIndex: Integer;
  LPaneIndex: Integer;

  function OwnsSurface(AGrid: TNyxCollectionStringGrid): Boolean;
  var
    LCurrent: TControl;
    LDepth: Integer;
  begin
    Result := AControl = AGrid;

    if Result or (AGrid.Editor = nil) then
    begin
      Exit;
    end;
    LCurrent := AControl;
    for LDepth := 0 to 4 do
    begin

      if LCurrent = AGrid.Editor then
      begin
        Exit(True);
      end;

      if (LCurrent = nil) or (LCurrent = AGrid) then
      begin
        Break;
      end;
      LCurrent := LCurrent.Parent;
    end;
  end;

begin
  Result := False;

  if AControl = nil then
  begin
    Exit;
  end;

  for LIndex := 0 to High(FColumnButtons) do
  begin

    if FColumnButtons[LIndex] = AControl then
    begin

      if AControl.Parent = Self then
      begin
        Exit(True);
      end;
      for LPaneIndex := 0 to High(FColumnPanes) do
      begin

        if AControl.Parent = FColumnPanes[LPaneIndex] then
        begin
          Exit(True);
        end;
      end;
      Exit(False);
    end;
  end;

  if OwnsSurface(Self) then
  begin
    Exit(True);
  end;
  for LIndex := 0 to High(FColumnPanes) do
  begin

    if OwnsSurface(FColumnPanes[LIndex]) then
    begin
      Exit(True);
    end;
  end;
end;

function TNyxCollectionStringGrid.HasOwnedColumnFocus: Boolean;
var
  LIndex: Integer;
  LActive: TWinControl;
begin
  Result := Focused or ((Editor <> nil) and Editor.Focused);
  for LIndex := 0 to High(FColumnPanes) do
  begin
    Result := Result or FColumnPanes[LIndex].Focused or
      ((FColumnPanes[LIndex].Editor <> nil) and FColumnPanes[LIndex].Editor.Focused);
  end;
  LActive := nil;

  if Screen <> nil then
  begin
    LActive := Screen.ActiveControl;
  end;
  { A composed editor can focus a real LCL child. Screen records that child,
    while Editor.Focused tests exact native ownership. Require actual focus as
    well as our bounded identity check: Screen's remembered control alone can
    survive focus moving outside this application. }

  if (LActive <> nil) and LActive.Focused and OwnsColumnInput(LActive) then
  begin
    Result := True;
  end;
end;

function TNyxCollectionStringGrid.OverrideIndex(AColumn, ARow: Integer): Integer;
begin
  for Result := 0 to Length(FOverrides) - 1 do
  begin

    if (FOverrides[Result].Column = AColumn) and (FOverrides[Result].Row = ARow) then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TNyxCollectionStringGrid.RemoveOverride(AIndex: Integer);
var
  LIndex: Integer;
begin
  for LIndex := AIndex to Length(FOverrides) - 2 do
  begin
    FOverrides[LIndex] := FOverrides[LIndex + 1];
  end;
  SetLength(FOverrides, Length(FOverrides) - 1);
end;

function TNyxCollectionStringGrid.GetReaderAttached: Boolean;
begin
  Result := Assigned(FReader);
end;

function TNyxCollectionStringGrid.GetOverrideCount: Integer;
begin
  Result := Length(FOverrides);
end;

procedure TNyxCollectionStringGrid.AttachReader(AReader: TNyxGridTextReader);
begin

  if not Assigned(AReader) or ReaderAttached then
  begin
    raise ENyxCollection.Create('Grid requires one exclusive live text reader');
  end;
  FOverrides := nil;
  FReads := 0;
  FReader := AReader;
end;

procedure TNyxCollectionStringGrid.DetachReader;
begin
  FReader := nil;
  FDisplayReader := nil;
  FTextLayoutReader := nil;
  FCellRangeReader := nil;
  FOverrides := nil;
end;

procedure TNyxCollectionStringGrid.ObserveTextLayout(AReader: TNyxGridTextLayoutReader);
begin
  FTextLayoutReader := AReader;
end;

function TNyxCollectionStringGrid.CellTextLayout(AColumn, ARow: Integer;
  out ALayout: TNyxTableTextLayout): Boolean;
begin
  Result := False;

  if Assigned(FTextLayoutReader) then
  begin
    Result := FTextLayoutReader(AColumn, ARow, ALayout);
  end;
end;

procedure TNyxCollectionStringGrid.ObserveCellRange(AReader: TNyxGridCellRangeReader);
begin
  FCellRangeReader := AReader;
end;

function TNyxCollectionStringGrid.CellRangeSelected(AColumn, ARow: Integer): Boolean;
begin
  Result := False;

  if Assigned(FCellRangeReader) then
  begin
    Result := FCellRangeReader(AColumn, ARow);
  end;
end;

procedure TNyxCollectionStringGrid.AttachDisplayReader(AReader: TNyxGridDisplayReader);
begin

  if not Assigned(AReader) or Assigned(FDisplayReader) then
  begin
    raise ENyxCollection.Create('Grid requires one exclusive display reader');
  end;
  FDisplayReader := AReader;
end;

function TNyxCollectionStringGrid.DisplayCellText(AColumn, ARow: Integer): TNyxText;
var
  LPresentation: TNyxValuePresentation;
begin

  if Assigned(FDisplayReader) then
  begin
    Exit(FDisplayReader(AColumn, ARow, LPresentation));
  end;
  Result := Cells[AColumn, ARow];
end;

function TNyxCollectionStringGrid.DisplayCellPresentation(AColumn, ARow: Integer): TNyxValuePresentation;
begin

  if Assigned(FDisplayReader) then
  begin
    FDisplayReader(AColumn, ARow, Result);
    Exit;
  end;
  Result := NyxCanonicalPresentation(TNyxStateValue.FromText(Cells[AColumn, ARow]));
end;

procedure TNyxCollectionStringGrid.ResetReadCount;
begin
  FReads := 0;
end;

procedure TNyxCollectionStringGrid.ClearOverride(AColumn, ARow: Integer);
var
  LIndex: Integer;
begin
  LIndex := OverrideIndex(AColumn, ARow);

  if LIndex >= 0 then
  begin
    RemoveOverride(LIndex);

    if (AColumn >= 0) and (AColumn < ColCount) and (ARow >= 0) and (ARow < RowCount) then
    begin
      InvalidateCell(AColumn, ARow);
    end;
  end;
end;

procedure TNyxCollectionStringGrid.TrimOverrides(AColumns, ASourceRows: Integer);
var
  LIndex: Integer;
begin
  for LIndex := Length(FOverrides) - 1 downto 0 do
  begin

    if (FOverrides[LIndex].Column < 0) or (FOverrides[LIndex].Column >= AColumns) or
      (FOverrides[LIndex].Row < 0) or (FOverrides[LIndex].Row > ASourceRows) then
    begin
      RemoveOverride(LIndex);
    end;
  end;
end;

function TNyxCollectionStringGrid.GetCells(ACol, ARow: Integer): String;
var
  LIndex: Integer;
begin

  if not ReaderAttached then
  begin
    Exit(inherited GetCells(ACol, ARow));
  end;
  LIndex := OverrideIndex(ACol, ARow);

  if LIndex >= 0 then
  begin
    Exit(FOverrides[LIndex].Text);
  end;

  if FReads < High(Int64) then
  begin
    Inc(FReads);
  end;
  Result := FReader(ACol, ARow);
end;

procedure TNyxCollectionStringGrid.SetCells(ACol, ARow: Integer; const AValue: String);
var
  LIndex: Integer;
  LText: TNyxText;
  LEpoch: INyxGridValidationEpoch;
begin

  if not ReaderAttached then
  begin
    inherited SetCells(ACol, ARow, AValue);
    Exit;
  end;

  if TNyxText(AValue) = FReader(ACol, ARow) then
  begin
    ClearOverride(ACol, ARow);
    Exit;
  end;
  { Preserve LCL's input/validation path for an actual draft. It may normalize
    the text or publish a store update before returning. Record its final value,
    not the original unvalidated argument. Accepted adapter writes use the
    dedicated ClearOverride path and create no eager physical cell values. }
  LEpoch := FValidationEpoch;
  inherited SetCells(ACol, ARow, AValue);

  if (LEpoch <> nil) and not LEpoch.Current then
  begin
    Exit;
  end;

  if not ReaderAttached then
  begin
    Exit;
  end;
  LText := inherited GetCells(ACol, ARow);

  if LText = FReader(ACol, ARow) then
  begin
    ClearOverride(ACol, ARow);
    Exit;
  end;
  LIndex := OverrideIndex(ACol, ARow);

  if LIndex < 0 then
  begin
    LIndex := Length(FOverrides);
    SetLength(FOverrides, LIndex + 1);
    FOverrides[LIndex].Column := ACol;
    FOverrides[LIndex].Row := ARow;
  end;
  FOverrides[LIndex].Text := LText;

  if (ACol >= 0) and (ACol < ColCount) and (ARow >= 0) and (ARow < RowCount) then
  begin
    InvalidateCell(ACol, ARow);
  end;
end;


function TNyxCollectionColumnButton.Ready: Boolean;
begin
  Result := (FSurface <> nil) and FSurface.Ready and FSurface.PositionReady;
end;

function TNyxCollectionColumnButton.GetField: TNyxColumnIdentity;
begin
  Result := FField;
end;

procedure TNyxCollectionColumnButton.ConfigureFitAction(
  const AField: TNyxColumnIdentity;
  const ALifetime: INyxCollectionInputLifetime;
  const AFrame: INyxNativeGestureFrame;
  const AContinuity: INyxGridInputContinuity;
  APrepare: TNyxGridColumnButtonPrepare; ABeforeFocus: TNotifyEvent);
begin
  AField.Validate;
  ConfigureColumnAction(ALifetime, AFrame, AContinuity, APrepare, ABeforeFocus);
  FField := AField;
end;

procedure TNyxCollectionColumnButton.ConfigureColumnAction(
  const ALifetime: INyxCollectionInputLifetime;
  const AFrame: INyxNativeGestureFrame;
  const AContinuity: INyxGridInputContinuity;
  APrepare: TNyxGridColumnButtonPrepare; ABeforeFocus: TNotifyEvent);
begin

  if FSurface <> nil then
  begin
    raise ENyxCollection.Create('A column button has one native input owner');
  end;
  FPrepare := APrepare;
  FBeforeFocus := ABeforeFocus;
  FSurface := TNativeInputSurface.Create(Self, ALifetime, AFrame, AContinuity);
  FSurface.CoverWindowProc;
end;

procedure TNyxCollectionColumnButton.RetireFitAction;
begin
  RetireColumnAction;
end;

procedure TNyxCollectionColumnButton.RetireColumnAction;
begin
  FPrepare := nil;
  FBeforeFocus := nil;
  OnKeyDown := nil;
  OnKeyUp := nil;

  if FSurface <> nil then
  begin
    FSurface.Retire;
  end;
  CancelCommand;
end;

procedure TNyxCollectionColumnButton.CancelCommand;
var
  LCommand: INyxGridColumnButtonCommand;
begin
  LCommand := FCommand;
  FCommand := nil;
  FCommandKey := 0;

  if LCommand <> nil then
  begin
    LCommand.Retire;
  end;
end;

procedure TNyxCollectionColumnButton.PrepareCommand(AKey: Word);
var
  LPrepare: TNyxGridColumnButtonPrepare;
  LCommand: INyxGridColumnButtonCommand;
begin

  if (FCommand <> nil) or not Ready then
  begin
    Exit;
  end;
  LPrepare := FPrepare;

  if Assigned(LPrepare) then
  begin
    LCommand := LPrepare(Self);

    if Ready and (FCommand = nil) then
    begin
      FCommand := LCommand;
      FCommandKey := AKey;
    end
    else if LCommand <> nil then
    begin
      LCommand.Retire;
    end;
  end;
end;

procedure TNyxCollectionColumnButton.WndProc(var AMessage: TLMessage);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LPrevious: INyxGridColumnButtonCommand;
  LRelease: INyxGridColumnButtonCommand;
  LWasRelease: Boolean;
  LIsRelease: Boolean;
begin
  LSurface := FSurface;

  if LSurface = nil then
  begin
    inherited WndProc(AMessage);
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    { Win32 delivers the virtual KeyUp from CN_KEYUP before its remaining
      LM_KEYUP phase. Reserve the original press before either phase can call
      key-preview/application code; neither may recapture a resumed command. }
    LIsRelease := (AMessage.Msg = LM_LBUTTONUP) or
      (((AMessage.Msg = CN_KEYUP) or (AMessage.Msg = CN_SYSKEYUP) or
        (AMessage.Msg = LM_KEYUP) or (AMessage.Msg = LM_SYSKEYUP)) and
        (AMessage.WParam in [VK_RETURN, VK_SPACE]));
    LPrevious := FReleaseCommand;
    LWasRelease := FInRelease;
    LRelease := FCommand;

    if LIsRelease then
    begin
      FReleaseCommand := LRelease;
      FInRelease := True;
    end;
    try
      inherited WndProc(AMessage);
    finally

      if LIsRelease then
      begin
        FReleaseCommand := LPrevious;
        FInRelease := LWasRelease;

        if FCommand = LRelease then
        begin
          CancelCommand;
        end;
      end;
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionColumnButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  { TControl.WMLButtonUp performs its Click before this virtual tail. The
    complete WndProc release fence retires an outside/cancelled press afterward,
    without recapturing or clearing any nested newer command. }
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TNyxCollectionColumnButton.InitializeWnd;
begin
  inherited InitializeWnd;

  if FSurface <> nil then
  begin
    FSurface.AttachHandle;
  end;
end;

procedure TNyxCollectionColumnButton.FinalizeWnd;
begin

  if FSurface <> nil then
  begin
    FSurface.DropHandle;
  end;
  inherited FinalizeWnd;
end;

procedure TNyxCollectionColumnButton.DestroyHandle;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := FSurface;

  if LSurface = nil then
  begin
    inherited DestroyHandle;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    inherited DestroyHandle;
  finally
    LFrame.Leave;
  end;
end;

destructor TNyxCollectionColumnButton.Destroy;
begin
  RetireColumnAction;

  if FSurface <> nil then
  begin
    FSurface.Disconnect;
  end;
  FSurface := nil;
  inherited Destroy;
end;

procedure TNyxCollectionColumnButton.SetFocus;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LPrepareFocus: TNotifyEvent;
begin
  LSurface := FSurface;

  if (LSurface = nil) or not LSurface.Ready or not LSurface.PositionReady then
  begin
    Exit;
  end;
  LFrame := LSurface.Frame;
  LPrepareFocus := FBeforeFocus;
  LFrame.Enter;
  try

    if Assigned(LPrepareFocus) then
    begin
      LPrepareFocus(Self);
    end;

    if LSurface.Ready and LSurface.PositionReady then
    begin
      inherited SetFocus;
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionColumnButton.KeyDown(var Key: Word; Shift: TShiftState);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := FSurface;

  if (LSurface = nil) or not LSurface.Ready or not LSurface.PositionReady then
  begin
    Key := 0;
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    try

      if Key in [VK_RETURN, VK_SPACE] then
      begin

        if (FCommand <> nil) and (FCommandKey <> Key) then
        begin
          CancelCommand;
        end;
        PrepareCommand(Key);
      end;
      inherited KeyDown(Key, Shift);

      if (Key = 0) or not LSurface.Ready or not LSurface.PositionReady then
      begin
        CancelCommand;
        Key := 0;
      end;
    except
      CancelCommand;
      raise;
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionColumnButton.KeyUp(var Key: Word; Shift: TShiftState);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LCommand: INyxGridColumnButtonCommand;
begin
  LSurface := FSurface;

  if (LSurface = nil) or not LSurface.Ready or not LSurface.PositionReady then
  begin
    Key := 0;
    Exit;
  end;
  LCommand := FCommand;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    try
      inherited KeyUp(Key, Shift);
    finally

      if (Key in [0, VK_RETURN, VK_SPACE]) and (FCommand = LCommand) then
      begin
        CancelCommand;
      end;
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionColumnButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
begin
  LSurface := FSurface;

  if (LSurface = nil) or not LSurface.Ready or not LSurface.PositionReady then
  begin
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try
    try

      if Button = mbLeft then
      begin
        CancelCommand;
        PrepareCommand(0);
      end;
      inherited MouseDown(Button, Shift, X, Y);
    except
      CancelCommand;
      raise;
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionColumnButton.DoExit;
begin
  CancelCommand;
  inherited DoExit;
end;

procedure TNyxCollectionColumnButton.Click;
var
  LSurface: INyxGridInputSurface;
  LFrame: INyxNativeGestureFrame;
  LCommand: INyxGridColumnButtonCommand;
begin
  LSurface := FSurface;

  if (LSurface = nil) or not LSurface.Ready or not LSurface.PositionReady then
  begin
    Exit;
  end;
  LFrame := LSurface.Frame;
  LFrame.Enter;
  try

    if FInRelease then
    begin
      LCommand := FReleaseCommand;
    end
    else
    begin
      { Programmatic/automation activation is a fresh physical command, never
        permission to complete another still-held keyboard or pointer press. }
      CancelCommand;
      PrepareCommand(0);
      LCommand := FCommand;
    end;

    if FCommand = LCommand then
    begin
      FCommand := nil;
      FCommandKey := 0;
    end;
    try

      if (LCommand <> nil) and LCommand.DispatchCurrent then
      begin
        LCommand.Invoke;
      end;
    finally

      if LCommand <> nil then
      begin
        LCommand.Retire;
      end;
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxCollectionStringGrid.RegisterColumnButton(AButton: TNyxCollectionColumnButton);
var
  LIndex: Integer;
begin

  if (AButton = nil) or (AButton.Owner <> Self) then
  begin
    raise ENyxCollection.Create('Column button belongs to its exact outer grid');
  end;
  for LIndex := 0 to High(FColumnButtons) do
  begin

    if FColumnButtons[LIndex] = AButton then
    begin
      raise ENyxCollection.Create('Column button is already registered');
    end;
  end;

  if Length(FColumnButtons) >= NyxMaximumColumnLayoutFields + 1 then
  begin
    raise ENyxCollection.Create('Too many native column buttons');
  end;
  LIndex := Length(FColumnButtons);
  SetLength(FColumnButtons, LIndex + 1);
  FColumnButtons[LIndex] := AButton;
end;

procedure TNyxCollectionStringGrid.UnregisterColumnButton(AButton: TNyxCollectionColumnButton);
var
  LIndex: Integer;
  LMove: Integer;
begin
  for LIndex := 0 to High(FColumnButtons) do
  begin

    if FColumnButtons[LIndex] = AButton then
    begin
      for LMove := LIndex to High(FColumnButtons) - 1 do
      begin
        FColumnButtons[LMove] := FColumnButtons[LMove + 1];
      end;
      SetLength(FColumnButtons, Length(FColumnButtons) - 1);
      Exit;
    end;
  end;
end;

end.
