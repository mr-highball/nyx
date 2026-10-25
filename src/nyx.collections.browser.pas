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
unit nyx.collections.browser;

{$mode delphi}{$H+}
{$modeswitch externalclass}
{$codepage utf8}

interface

uses
  JS,
  Web,
  nyx.collections.view,
  nyx.collections.presentation,
  nyx.collections.alignment,
  nyx.collections.mount, nyx.collections.draft, nyx.scheduler,
  nyx.collections.range.stroke, nyx.gestures;

type
  { Synchronous physical-input envelope. Prepare captures the original model
    authority before canonical callbacks; Complete queues that same intention
    only when default handling remains admitted. True means the scoped stroke
    is claimed even when its default is denied, preventing a bubbling recapture.
    Retire is terminal/flags-only and never revokes an already-transferred plan. }
  INyxBrowserColumnCommand = interface(IInterface)
    ['{6AD6ADFD-464E-4A00-8DCE-6012183A4232}']
    procedure Prepare;
    function GetCurrent: Boolean;
    { Canonical callback delivery needs only the original live incarnation.
      Benign model edits may cancel Fit without deleting remaining key phases. }
    function GetDispatchCurrent: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
    property Current: Boolean read GetCurrent;
    property DispatchCurrent: Boolean read GetDispatchCurrent;
  end;
  { Optional browser host port, preserving every existing mount GUID/vtable.
    A host with canonical callbacks brackets them with this exact owned token.
    Ordinary direct mounts use the same port in their existing local listeners;
    no document-wide keyboard listener or event metadata registry is needed. }
  INyxBrowserCollectionColumnInput = interface(IInterface)
    ['{3673530D-00CA-4251-B993-52D3AC5E64F2}']
    function BeginColumnKey(AEvent: TJSKeyboardEvent): INyxBrowserColumnCommand;
    function BeginColumnDoubleClick(AEvent: TJSMouseEvent): INyxBrowserColumnCommand;
  end;

  { Optional raw-copy key entry reuses the existing pre-canonical envelope.
    Only an exact resident cell owns Ctrl/Meta+C; active editors retain their
    platform copy behavior. Hosts prepare/complete around their real key route. }
  INyxBrowserCollectionClipboardInput = interface(IInterface)
    ['{AE3D9D17-99D2-4BA2-BEC9-0F885E5D0A69}']
    function BeginClipboardKey(AEvent: TJSKeyboardEvent): INyxBrowserColumnCommand;
  end;

  { Optional explicitly enabled cell-range key route. Only exact resident cells
    qualify; editors retain their own selection/caret and default-off tables keep
    ordinary row navigation. It uses the existing canonical keyboard envelope. }
  INyxBrowserCollectionCellRangeInput = interface(IInterface)
    ['{C86619CD-DDBD-4A10-856A-877C3D08F2D0}']
    function BeginRangeKey(AEvent: TJSKeyboardEvent): INyxBrowserColumnCommand;
  end;

  { Optional Shift-primary endpoint route. The mount reserves original input
    before DOM hit/focus or source getters. Only resident owned data cells and
    an exact pre-down focused cell qualify; editors/custom controls and headers
    keep their ordinary defaults. The same envelope spans canonical pointer
    dispatch, and its matching click tail never selects an ordinary row. }
  INyxBrowserCollectionCellRangePointerInput = interface(IInterface)
    ['{035C3457-C726-4571-B694-FEAEFBC3F2EC}']
    function BeginRangePointer(AEvent: TJSPointerEvent): INyxBrowserColumnCommand;
  end;

  { Optional sustained range route. The renderer supplies its ORIGINAL event
    generation before physical hit testing. Only flags and detached stroke data
    survive messages; each returned command is retired synchronously. Begin
    preserves the existing Shift-click publication before arming movement. }
  INyxBrowserCollectionCellRangeStrokeInput = interface(IInterface)
    ['{517485F6-732B-4D53-B818-51BA7E835FD1}']
    function BeginRangeStroke(AEvent: TJSPointerEvent;
      APhase: TNyxCollectionRangeStrokePhase;
      const AGeneration: INyxCancellationScope): INyxBrowserColumnCommand;
  end;

  { Canonical capture directives outrank an adapter's optional gesture default.
    The renderer reports the sealed decision on the same synchronous command,
    before applying the platform effect. No event/router lease is retained. }
  INyxBrowserRangeCaptureDecision = interface(IInterface)
    ['{5C317A84-F35E-4768-A555-A53691D3FEED}']
    procedure ObservePointerRequest(ARequest: TNyxPointerRequest);
  end;

  { Optional exact resident-cell context entry. Editors retain their native text
    menu unless focused-draft actions explicitly opt in. The renderer reserves
    this proof before canonical context callbacks and completes its managed
    default only if unconsumed. A stale nonnil envelope stays claimed; nil is
    reserved for unsupported/default-off entry, never a newer managed intent. }
  INyxBrowserCollectionCellContextInput = interface(IInterface)
    ['{3187C630-5F36-46B1-BD40-E2F359DBCECC}']
    function BeginCellContext(AEvent: TJSMouseEvent): INyxBrowserColumnCommand;
  end;

  { Optional single-activation facet keeps the existing keyboard/double-click
    port compatible. Exact Fit and persistent Columns buttons are claimed. Even a
    pre-cancelled click returns an envelope, so it cannot fall through to an
    ordinary control action or be recaptured by a later mount listener. }
  INyxBrowserCollectionColumnActivation = interface(IInterface)
    ['{8D20FD23-AE33-41D3-A931-5ECBDF7CCCF6}']
    function BeginColumnClick(AEvent: TJSMouseEvent): INyxBrowserColumnCommand;
  end;

  { Captured button intent accompanies the original command through keyboard
    callbacks. The host must not re-inspect a possibly replaced header or
    recapture a second command when Enter/Space proceeds to canonical Click. }
  INyxBrowserColumnButtonActivation = interface(IInterface)
    ['{571150C4-7A7E-4C57-A196-8A58C745C5E7}']
    function GetActivatesButton: Boolean;
    property ActivatesButton: Boolean read GetActivatesButton;
  end;

  { Space has a press/release default, unlike Enter. Holding transfers only
    owner-free captured facts into the mount, never this mount-retaining command.
    Retire remains terminal and noncalling, including a cancelled repeat. }
  INyxBrowserColumnButtonRelease = interface(IInterface)
    ['{F1A16D98-7F20-4907-B12E-C17C7A85847B}']
    function GetAwaitsRelease: Boolean;
    function GetEntryAdmitted: Boolean;
    function HoldUntilRelease: Boolean;
    property AwaitsRelease: Boolean read GetAwaitsRelease;
    property EntryAdmitted: Boolean read GetEntryAdmitted;
  end;

  { Retrieves the exact prepared Space press, without recapturing current model
    authority on keyup. A stale or cancelled press retires and returns nil;
    ordinary KeyUp callbacks may still run through the host's general route. }
  INyxBrowserCollectionColumnRelease = interface(IInterface)
    ['{A53357C1-4B48-4C4B-8334-BD8623CE2CAA}']
    function BeginColumnRelease(AEvent: TJSKeyboardEvent): INyxBrowserColumnCommand;
  end;

{ Adapter boundary. Host must be the list/table/tree element projected by Nyx.
  The returned mount borrows it; its renderer disconnects before removing DOM.
  Rows are reused by stable item identity, including during reorder. Lists expose
  selection; tables/tree captions offer column editing where cmEditable permits.
  Lists/tables realize a measured viewport window, retaining focused editors and exact
  offscreen drafts. Same-source refresh visits owned rows and the incoming window;
  source changes still remap logical slots. Trees still materialize rows, and
  full update/frame/memory budgets remain open. }
function MountNyxBrowserCollection(AHost: TJSHTMLElement;
  const AView: INyxCollectionView;
  AInputPhase: TNyxCollectionInputPhase = ncipReady;
  const AInputGate: INyxCollectionInputGate = nil): INyxCollectionMount;

{ Explicit runtime measurement limits are copied before activation. They affect
  only opted-in text layout, never persisted policies or accepted cell values. }
function MountNyxBrowserCollectionWithTextLimits(AHost: TJSHTMLElement;
  const AView: INyxCollectionView; const ALimits: TNyxTableTextWorkLimits;
  AInputPhase: TNyxCollectionInputPhase = ncipReady;
  const AInputGate: INyxCollectionInputGate = nil): INyxCollectionMount;

implementation

uses
  SysUtils,
  Math,
  nyx.text,
  nyx.state,
  nyx.formatting,
  nyx.editing,
  nyx.editing.browser,
  nyx.collections,
  nyx.collections.view.types,
  nyx.collections.arrangement.presenter,
  nyx.typeahead,
  nyx.collections.grid,
  nyx.collections.columns,
  nyx.collections.arrangement,
  nyx.collections.arrangement.controls,
  nyx.collections.arrangement.browser,
  nyx.collections.query,
  nyx.collections.query.controls,
  nyx.publication,
  nyx.gestures.browser,
  nyx.layout.flow,
  nyx.collections.window,
  nyx.collections.selection,
  nyx.collections.clipboard.actions,
  nyx.collections.range.actions,
  nyx.collections.range,
  nyx.collections.range.commands,
  nyx.collections.range.navigation,
  nyx.collections.range.pointer,
  nyx.collections.cell.actions,
  nyx.collections.representation,
  nyx.collections.text.layout,
  nyx.collections.tree.rows;

{ The resize preview uses the same ordinary DOM factory as mounted rows. }
function NewElement(const ATag: TNyxText): TJSHTMLElement; forward;

type
  { Standard Document hit testing accepts CSS pixel doubles. The installed Web
    unit narrows these arguments; this bridge preserves fractional coordinates.
    The global document and returned resident elements remain borrowed hosts. }
  TRangeHitDocument = class external name 'Document' (TJSDocument)
    function elementFromPoint(AX, AY: Double): TJSElement; reintroduce;
  end;
  { Typed bridge to standard DOM focus options, missing from older Web units. }
  TFocusOptions = class external name 'Object' (TJSObject)
    preventScroll: Boolean;
  end;
  TFocusable = class external name 'HTMLElement' (TJSHTMLElement)
    procedure focus(AOptions: TFocusOptions); reintroduce;
  end;
  { CSSOM scroll coordinates are doubles. Older Web declarations narrow the
    window method to integers; this bridge preserves fractional host offsets. }
  TScrollableWindow = class external name 'Window' (TJSWindow)
    scrollXPixels: Double; external name 'scrollX';
    scrollYPixels: Double; external name 'scrollY';
    procedure scrollTo(AX, AY: Double); reintroduce;
  end;
  TScrollableElement = class external name 'HTMLElement' (TJSHTMLElement)
    scrollTopPixels: Double; external name 'scrollTop';
    scrollLeftPixels: Double; external name 'scrollLeft';
  end;
  { Listener capture is part of DOM callback identity. Some Web versions omit
    the remove overload; teardown must use the same capture flag as admission. }
  TCapturedDocument = class external name 'Document' (TJSDocument)
    procedure removeEventListener(AType: String; AListener: JSValue;
      ACapture: Boolean); reintroduce;
  end;
  TCapturedElement = class external name 'HTMLElement' (TJSHTMLElement)
    procedure removeEventListener(AType: String; AListener: JSValue;
      ACapture: Boolean); reintroduce;
  end;
  TNyxDetails = class external name 'HTMLDetailsElement' (TJSHTMLElement)
    open: Boolean;
  end;

  TBrowserMount = class;
  { A synchronous focus shield retains only the entered mount. Its count
    suppresses blur/change noise and never becomes stored session authority. }
  TBrowserCellPresentation = class(TInterfacedObject, INyxCollectionCellPresentation)
  private
    FOwner: TBrowserMount;
    FLease: INyxCollectionMount;
  public
    constructor Create(AOwner: TBrowserMount);
    destructor Destroy; override;
    procedure Leave;
  end;
  TBrowserRow = class;
  { Adapt a portable original-copy proof to the already-owned renderer key
    dispatcher. No button activation, synthesized Click or second key route
    belongs here. Retire only withdraws this synchronous envelope. }
  TBrowserCopyCommand = class(TInterfacedObject, INyxBrowserColumnCommand)
  private
    FCommand: INyxCollectionClipboardCommand;
  public
    constructor Create(const ACommand: INyxCollectionClipboardCommand);
    destructor Destroy; override;
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
  end;
  { Adapts one prepared original range key to the same canonical host envelope;
    no replay, button action, host write or second event route is introduced. }
  TBrowserRangeKeyCommand = class(TInterfacedObject, INyxBrowserColumnCommand)
  private
    FCommand: INyxCollectionCellRangeKeyCommand;
  public
    constructor Create(const ACommand: INyxCollectionCellRangeKeyCommand);
    destructor Destroy; override;
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
  end;
  { Adapts one original endpoint or pending-stroke message to canonical browser
    dispatch. Its mount is borrowed only through that message's owning entry;
    original event flags and exact subject prevent stale cleanup or capture.
    No event is replayed and no mount retains this synchronous wrapper. }
  TBrowserRangePointerCommand = class(TInterfacedObject, INyxBrowserColumnCommand,
    INyxBrowserRangeCaptureDecision)
  private
    FCommand: INyxCollectionCellRangePointerEntry;
    { Borrowed only while FCommand owns the exact mount synchronously. }
    FOwner: TBrowserMount;
    FGeneration: INyxCancellationScope;
    FPointer: TNyxCollectionRangeStrokePointer;
    FEndpoint: TNyxCollectionCellIdentity;
    FSubject: INyxCollectionRangeStroke;
    FPhase: TNyxCollectionRangeStrokePhase;
    FArm: Boolean;
    FCaptureRequested: Boolean;
    FCompleted: Boolean;
    FCompleting: Boolean;
  public
    constructor Create(const ACommand: INyxCollectionCellRangePointerEntry);
    destructor Destroy; override;
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure ObservePointerRequest(ARequest: TNyxPointerRequest);
    procedure Retire;
  end;
  { Host-local action and physical origin are typed independently. Fit does not
    require manual Resize; an editor origin is never inferred from attributes. }
  TColumnLayoutAction = (claResize, claFit);
  TColumnInputOrigin = (cioHeader, cioCell, cioEditor);
  TColumnLayoutProposal = class;
  { One entered Fit call owns its exact mount, launch/model and metric leases.
    It retains no delayed callback. Validate may inspect current DOM/fonts;
    final readiness reads only cached map/input facts. Install records commitment
    before any model notification can fail, without touching a widget. }
  TBrowserColumnFitFence = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FOwner: TBrowserMount;
    FOwnerLease: INyxCollectionMount;
    FView: INyxCollectionView;
    FLaunch: INyxCollectionColumnsCapture;
    FInput: INyxCollectionInputLifetime;
    FField: TNyxColumnIdentity;
    FHost: TJSHTMLElement;
    FMetric: INyxBrowserColumnFitMeasurement;
    FCommand: TColumnLayoutProposal;
    FCommandLease: IInterface;
    FColumn, FEpoch, FMutation, FLayoutSerial, FResizeSerial: Integer;
    FValidationStarted, FValidated, FInstalled, FRetired: Boolean;
  public
    constructor Create(AOwner: TBrowserMount;
      const ALaunch: INyxCollectionColumnsCapture; const AField: TNyxColumnIdentity);
    destructor Destroy; override;
    procedure RequireCurrent;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { One host-local proposal. It retains only exact model/capture/input leases
    and its own DOM surfaces, never a mount or application. Active movement
    changes a guide/value only. The SAME proposal becomes pending on release;
    the existing animation frame publishes once after the physical event tail. }
  TColumnLayoutProposal = class(TInterfacedObject)
  public
    View: INyxCollectionView;
    Columns: INyxCollectionColumns;
    Capture: INyxCollectionColumnsCapture;
    Input: INyxCollectionInputLifetime;
    Field: TNyxColumnIdentity;
    Action: TColumnLayoutAction;
    Origin: TColumnInputOrigin;
    OriginRow: TBrowserRow;
    OriginSlot: Integer;
    OriginElement: TJSHTMLElement;
    OriginItem: TNyxItemRef;
    Rendered: INyxCollectionSnapshot;
    Mutation: Integer;
    Host, Handle, Guide: TJSHTMLElement;
    { Same-slot actions have their own saturating clock. A nested Arrow/Move
      which returns to the same width must still supersede an older Fit token. }
    Column, Serial, Epoch, LayoutSerial, Activity: Integer;
    InitialWidth, Width, Minimum, Maximum, Available: Integer;
    StartX, Scale, PhysicalWidth: Double;
    Font: String;
    Pointer: NativeInt;
    RightToLeft, Keyboard, Captured, Retired, Released, Publishing, Installed: Boolean;
  end;
  { Forward for an owner-free prepared press transported between physical keys. }
  TBrowserButtonPress = class;

  { Holds the mount only through synchronous canonical dispatch. Its candidate
    has no mount backreference and enters the existing pending slot only after
    callbacks accept the ORIGINAL serial/field/epoch/model proof. }
  TBrowserColumnCommand = class(TInterfacedObject, INyxBrowserColumnCommand,
    INyxBrowserColumnButtonActivation, INyxBrowserColumnButtonRelease)
  private
    FOwner: TBrowserMount;
    FOwnerLease: INyxCollectionMount;
    FProposal: TColumnLayoutProposal;
    FProposalLease: IInterface;
    FEvent: TEventListenerEvent;
    FActive, FPending: TColumnLayoutProposal;
    FActiveLease, FPendingLease: IInterface;
    FActiveActivity, FPendingActivity: Integer;
    FEligible, FPrepared, FCompleteStarted, FSuperseding, FTransferred, FRetired: Boolean;
    FActivatesButton: Boolean;
    FEntryAdmitted: Boolean;
    FSpaceAtEntry: TBrowserButtonPress;
    FSpaceAtEntryLease: IInterface;
    FManagerSerial: Integer;
    function CurrentOriginal: Boolean;
  public
    constructor Create(AOwner: TBrowserMount; AProposal: TColumnLayoutProposal;
      const ALease: IInterface; AEvent: TEventListenerEvent; AEligible: Boolean;
      AActivatesButton: Boolean);
    destructor Destroy; override;
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function GetActivatesButton: Boolean;
    function GetAwaitsRelease: Boolean;
    function GetEntryAdmitted: Boolean;
    function HoldUntilRelease: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
  end;

  { Owns the original prepared proposal and arbitration witnesses, with no mount,
    renderer, binding or event backreference. Mount ownership therefore creates
    no reference cycle. Blur/disable/disconnect terminally revoke the proposal. }
  TBrowserButtonPress = class(TInterfacedObject)
  public
    Proposal: TColumnLayoutProposal;
    ProposalLease: IInterface;
    Active: TColumnLayoutProposal;
    Pending: TColumnLayoutProposal;
    ActiveLease: IInterface;
    PendingLease: IInterface;
    ActiveActivity: Integer;
    PendingActivity: Integer;
    Cancelled: Boolean;
    constructor Create(ACommand: TBrowserColumnCommand);
  end;

  { A Columns intention and optional held Space press retain value/identity
    witnesses only. There is no mount, renderer, event or widget-owner reference
    back to this lease, so retaining it on the mount cannot create a cycle. }
  TBrowserManagerPress = class(TInterfacedObject)
  public
    View: INyxCollectionView;
    Manager: INyxColumnArrangementManager;
    Input: INyxCollectionInputLifetime;
    Capture: INyxCollectionColumnsCapture;
    Rendered: INyxCollectionSnapshot;
    Host: TJSHTMLElement;
    Launcher: TJSHTMLElement;
    Epoch: Integer;
    Mutation: Integer;
    LayoutSerial: Integer;
    ResizeSerial: Integer;
    ManagerSerial: Integer;
    Active: TColumnLayoutProposal;
    Pending: TColumnLayoutProposal;
    ActiveLease: IInterface;
    PendingLease: IInterface;
    ActiveActivity: Integer;
    PendingActivity: Integer;
    FitPress: TBrowserButtonPress;
    FitPressLease: IInterface;
    Cancelled: Boolean;
  end;

  { Canonical Click/keyboard delivery owns the mount only while entered. The
    separately retained intention may cross Space down/up without owning it.
    Full default freshness is distinct from the live callback incarnation. }
  TBrowserManagerCommand = class(TInterfacedObject, INyxBrowserColumnCommand,
    INyxBrowserColumnButtonActivation, INyxBrowserColumnButtonRelease)
  private
    FOwner: TBrowserMount;
    FOwnerLease: INyxCollectionMount;
    FPress: TBrowserManagerPress;
    FPressLease: IInterface;
    FPrevious: TBrowserManagerPress;
    FPreviousLease: IInterface;
    FEvent: TEventListenerEvent;
    FAllowOpen: Boolean;
    FEntryAdmitted: Boolean;
    FPrepared: Boolean;
    FCompleteStarted: Boolean;
    FTransferred: Boolean;
    FSuperseding: Boolean;
    FRetired: Boolean;
    function CurrentOriginal: Boolean;
  public
    constructor Create(AOwner: TBrowserMount; APress: TBrowserManagerPress;
      const ALease: IInterface; AEvent: TEventListenerEvent; AAllowOpen: Boolean);
    destructor Destroy; override;
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function GetActivatesButton: Boolean;
    function GetAwaitsRelease: Boolean;
    function GetEntryAdmitted: Boolean;
    function HoldUntilRelease: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
  end;
  { UI-only final participant: after outward model validators it reads known
    cached mount/proposal facts and the canonical readonly input state only.
    It owns its borrowed receiver through the synchronous group, not through
    queued work; no DOM/source/router getter enters CheckReady or Install. }
  TColumnLayoutFence = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FOwner: TBrowserMount;
    FOwnerLease: INyxCollectionMount;
    FProposal: TColumnLayoutProposal;
    FProposalLease: IInterface;
    FValidated, FRetired: Boolean;
  public
    constructor Create(AOwner: TBrowserMount; AProposal: TColumnLayoutProposal;
      const AProposalLease: IInterface);
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { A genuine DOM change, delayed while its exact input is composing. Only
    identity/accepted scalar are copied now; the settled DOM text is read at the
    existing next frame. END alone never invents a model edit. }
  TCompositionChange = record
    Pending: Boolean;
    Input: TJSHTMLInputElement;
    Item: TNyxItemRef;
    Field: TNyxColumnIdentity;
    Accepted: TNyxStateValue;
  end;
  { One realized stable row owns its cells/editors. Range paint uses cached
    immutable membership and never changes accepted data or draft focus. }
  TBrowserRow = class
  private
    FOwner: TBrowserMount;
    FRef: TNyxItemRef;
    FElement: TJSHTMLElement;
    FChildren: TJSHTMLElement;
    { Exact owned data-cell surfaces. Editors remain children with a separate
      editing mode; row membership is independent of this physical cursor. }
    FCells: array of TJSHTMLElement;
    FLabels: array of TJSHTMLElement;
    { Complete accepted display is independent of the measured visible lines.
      Reflow never reads or changes an input's unfinished native text. }
    FDisplayTexts: array of TNyxText;
    FDisplayDiagnostics: array of TNyxText;
    FInputs: array of TJSHTMLInputElement;
    FInitialized: Boolean;
    FHadCellRange: Boolean;
    procedure SyncCellRange;
    procedure SyncTextLayout(var AMeasuredCells: Integer);
    function Click(AEvent: TJSMouseEvent): Boolean;
    function DoubleClick(AEvent: TJSMouseEvent): Boolean;
    function Change(AEvent: TEventListenerEvent): Boolean;
    function Toggle(AEvent: TEventListenerEvent): Boolean;
  public
    constructor Create(AOwner: TBrowserMount; const ARef: TNyxItemRef);
    destructor Destroy; override;
    procedure Sync(AValuesChanged: Boolean);
    { Pure comparison with accepted scalar display. Detached draft rows retain
      their owned controls until normalization, removal or teardown. }
    function HasDraft: Boolean;
  end;

  { Owns projected collection rows and pending layout commands while borrowing
    its host DOM. Input envelopes retain this exact mount through synchronous
    callbacks; terminal detachment revokes their authority before DOM removal. }
  TBrowserMount = class(TNyxCollectionMountBase, INyxCollectionEditMount,
    INyxBrowserCollectionColumnInput, INyxBrowserCollectionColumnActivation,
    INyxBrowserCollectionColumnRelease, INyxBrowserCollectionClipboardInput,
    INyxBrowserCollectionCellContextInput, INyxCollectionFocusedDraftInput,
    INyxBrowserCollectionCellRangeInput, INyxBrowserCollectionCellRangePointerInput,
    INyxBrowserCollectionCellRangeStrokeInput)
  private
    FHost: TJSHTMLElement;
    { Borrowed exact down target, owned by this mount's row DOM. This scoped
      stamp suppresses only its ensuing primary click's row/focus default.
      Another pointer down, the matching click or retirement clears it; it
      retains no command, view or selection owner and uses no time heuristic. }
    FRangePointerCell: TJSHTMLElement;
    { Adapter ownership holds detached data and original event flags, never a
      synchronous input envelope. The borrowed host is this mount's own DOM. }
    FRangeGesture: INyxCollectionRangeStroke;
    FRangeGeneration: INyxCancellationScope;
    FRangePointer: TNyxCollectionRangeStrokePointer;
    FRangeCaptured: Boolean;
    function RangePointerCommand(AEvent: TJSPointerEvent;
      const AGeneration: INyxCancellationScope): INyxBrowserColumnCommand;
    procedure FinishRangeGesture(const ASubject: INyxCollectionRangeStroke);
    procedure ArmRangeGesture(const APointer: TNyxCollectionRangeStrokePointer;
      const AGeneration: INyxCancellationScope;
      const AEndpoint: TNyxCollectionCellIdentity);
    function RangeGestureCancel(AEvent: TJSEvent): Boolean;
    { A permanent caption survives an empty visible-column map. Opening focus
      suppresses only its own synchronous blur/change noise, not later edits. }
    FColumnLauncher: TJSHTMLElement;
    FOpeningColumnManager: Boolean;
    FCellPresentationDepth: Integer;
    FColumnManagerSerial: Integer;
    FColumnManagerPress: TBrowserManagerPress;
    FColumnManagerPressLease: IInterface;
    function ColumnLauncherPointer(AEvent: TJSMouseEvent): Boolean;
    function NewColumnManagerCommand(AEvent: TEventListenerEvent;
      AAllowOpen: Boolean = True): INyxBrowserColumnCommand;
    FBody: TJSHTMLElement;
    FRows: array of TBrowserRow;
    FRendered: INyxCollectionSnapshot;
    { Distinct from FUpdating: physical blur/change and queued row work stay
      suspended for the whole reversible renderer preview, even if model
      observers request Refresh. The exact snapshot is the single-use lease. }
    FEditSuspension: INyxCollectionEditsSnapshot;
    { Exact owned editor, retained through compositionend dispatch until the
      next existing window frame. No source lookup/DOM rebuild runs in this
      capture listener. A new composition cancels a pending end for that input. }
    FComposingInput: TJSHTMLInputElement;
    FCompositionEnding: Boolean;
    FCompositionChange: TCompositionChange;
    FCompositionFocusPending: Boolean;
    { Physical focus proof never owns a row or DOM subtree. Exact current row,
      canonical input, text and supported caret qualify the copied logical cell;
      notifications/window reconstruction revoke it before any outward work. }
    FCellFocusInput: TJSHTMLInputElement;
    FCellFocusRow, FCellFocusColumn, FCellFocusSerial: Integer;
    { Immutable hierarchy reads retain only snapshot/metadata. The mount owns
      the view association, so cache invalidation compares its snapshot/stamp
      without a projection-to-view retention cycle. }
    FTreeRows: INyxTreeRows;
    FTreeGeometryRows: INyxTreeRows;
    FFocusedColumn: Integer;
    { Stable header controls survive row refresh and own no per-row closures.
      The delegated handlers run after the renderer's canonical host events. }
    FHeaderCells: array of TJSHTMLElement;
    FHeaderLabels: array of TJSHTMLElement;
    FHeaderSorts: array of TJSHTMLElement;
    FHeaderFilters: array of TJSHTMLElement;
    FHeaderResizers: array of TJSHTMLElement;
    { Revocation precedes capture release/guide cleanup. A fresh intention or
      composition/geometry invalidation advances this non-wrapping serial,
      covering even a proposal still being constructed across an outward call. }
    FColumnResizeSerial: Integer;
    FColumnResize, FColumnResizePending: TColumnLayoutProposal;
    FColumnResizeLease, FColumnResizePendingLease: IInterface;
    FColumnSpacePress: TBrowserButtonPress;
    FColumnSpacePressLease: IInterface;
    procedure CancelColumnButtonPress;
    function ColumnFitSurface(AColumn: Integer;
      AHasBody: Boolean): TNyxBrowserColumnFitSurface;
    procedure InvalidateColumnResizes;
    procedure CancelColumnResizes(ACancelButton: Boolean = True);
    procedure ReconcileColumnResizeCapture(AProposal: TColumnLayoutProposal);
    function ColumnResizeContext(AProposal: TColumnLayoutProposal;
      ARequirePermission: Boolean = True): Boolean;
    function ColumnResizeGeometry(AProposal: TColumnLayoutProposal): Boolean;
    function ColumnResizeHandle(AEvent: TEventListenerEvent;
      out AColumn: Integer): TJSHTMLElement;
    function BeginColumnResize(AHandle: TJSHTMLElement; AColumn: Integer;
      AKeyboard: Boolean; AX: Double; APointer: NativeInt): TColumnLayoutProposal;
    procedure PreviewColumnResize(AProposal: TColumnLayoutProposal);
    procedure FinishColumnResize(AProposal: TColumnLayoutProposal);
    procedure PublishColumnResize;
    procedure RefreshColumnResizers;
    function ColumnResizeDown(AEvent: TJSPointerEvent): Boolean;
    function ColumnResizeMove(AEvent: TJSPointerEvent): Boolean;
    function ColumnResizeUp(AEvent: TJSPointerEvent): Boolean;
    function ColumnResizeCancel(AEvent: TJSPointerEvent): Boolean;
    function ColumnResizeKey(AEvent: TJSKeyboardEvent): Boolean;
    function ColumnResizeFocus(AEvent: TEventListenerEvent): Boolean;
    function ColumnButtonBlur(AEvent: TEventListenerEvent): Boolean;
    function ColumnButtonRelease(AEvent: TJSKeyboardEvent): Boolean;
    function IsColumnFitButton(AEvent: TEventListenerEvent): Boolean;
    function NewColumnFitCommand(AEvent: TEventListenerEvent;
      AKeyboard: Boolean; AAllowFit: Boolean = True;
      AActivatesButton: Boolean = False): INyxBrowserColumnCommand;
    function ColumnFitInput(AEvent: TJSMouseEvent): Boolean;
    procedure RefreshQueryHeaders;
    procedure ApplyHeaderPresentation;
    procedure QueryColumn(AColumn: Integer; AFilter, AAppend: Boolean);
    function QueryClick(AEvent: TEventListenerEvent): Boolean;
    function QueryKey(AEvent: TJSKeyboardEvent): Boolean;
    function BlockPendingInput(AEvent: TEventListenerEvent): Boolean;
    function CellComposition(AEvent: TEventListenerEvent): Boolean;
    procedure DrainCompositionChange(const AChange: TCompositionChange);
    procedure ReconcileCompositionFocus;
  private
    { Edit ownership is runtime-only and uses a stable item/column tuple.
      Hidden editors retain draft/caret identity without becoming extra Tab or
      accessibility entries. No document, source or row object is retained. }
    FEditingItem: TNyxItemRef;
    FEditingColumn: Integer;
    function EnterCellEditor(const AItem: TNyxItemRef; AColumn: Integer): Boolean;
    function CellEditing(const AItem: TNyxItemRef; AColumn: Integer): Boolean;
    function EditorFocusChanged(AEvent: TEventListenerEvent): Boolean;
  private
    FColumnNatural: TNyxFlowSizes;
    FColumnWidths: TNyxFlowSizes;
    FColumnFont: String;
    { Borrowed only during synchronous line layout; cleared before return. }
    FTextMetric: TJSCanvasRenderingContext2D;
    function MeasurePresentationText(const AText: TNyxText): Double;
  private
    FColumnAvailable: Integer;
    FColumnLayoutSerial: Integer;
    FColumnPresentation: TNyxColumnPresentation;
    function ColumnClipWidth: Integer;
    procedure OrderColumnChildren(AParent: TJSHTMLElement;
      const ACells: array of TJSHTMLElement);
    procedure ApplyColumnFace(ACell: TJSHTMLElement; AColumn: Integer;
      AHeader, ASelected: Boolean);
    procedure SizeTableColumns;
  private
    { Numerical geometry is owned; row slots are logical indices and may be nil.
      Only realized rows and explicit unfinished drafts own controls. }
    FGeometry: TNyxCollectionRowGeometry;
    { Sorted logical indices of owned flat rows, including detached table drafts.
      Scroll/selection refresh visits these rows and the new viewport only;
      the dataset-sized slot map remains private for direct keyboard lookup. }
    FResidentRows: array of Integer;
    FSpacers: array of TJSHTMLElement;
    FForcedItem: TNyxItemRef;
    { Keep a keyboard destination revealed while new neighboring rows acquire
      measured heights. Manual host scrolling cancels this short-lived intent;
      stable geometry consumes it. Values retain no DOM or document owner. }
    FRevealItem: TNyxItemRef;
    FRevealScrollTop: Double;
    FWindowFrame: Integer;
    FWindowQueued: Boolean;
    FWindowObserver: TJSHTMLResizeObserver;
    FWindowAncestors: array of TJSHTMLElement;
    function ScrollChanged(AEvent: TEventListenerEvent): Boolean;
    function WindowChanged(AEvent: TEventListenerEvent): Boolean;
    procedure RowBoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
      AObserver: TJSHTMLResizeObserver);
    procedure QueueWindow;
    procedure WindowFrame(ATime: Double);
    procedure ObserveWindowAncestors;
    procedure RowViewport(out AOffset, AExtent: Double);
    procedure RenderFlatWindow;
    { Disclosed rows use viewport geometry; retained ancestors own their real
      child groups. Hidden drafts retain controls without becoming tree entries. }
    procedure RenderTreeWindow;
    function TreeRows: INyxTreeRows;
    { Realizes a logical destination before navigation publishes selection.
      It does not scroll or rewrite source/history. Normal focus reveals it. }
    procedure EnsureRow(AIndex: Integer);
    { Reveal within the list's own scroll plane before focus. An already-focused
      retained option may have moved during a query; DOM focus alone is a no-op.
      The last borrowed focus call may retire this mount through application UI. }
    procedure FocusListOption(AOption: TJSHTMLElement);
    procedure RevealListOption(AOption: TJSHTMLElement);
    function GridKey(AEvent: TJSKeyboardEvent; const AOrder: TNyxItemRefs;
      ARow: Integer): Boolean;
    function Key(AEvent: TJSKeyboardEvent): Boolean;
    function CellContext(AEvent: TJSMouseEvent): Boolean;
    function VisibleOrder: TNyxItemRefs;
    procedure Gesture(const AItem: TNyxItemRef; AShift, AControl: Boolean;
      AFocusOnly: Boolean = False);
  protected
    procedure RenderCellRange; override;
    procedure RetireCellRangeStroke(const AStroke: INyxCollectionRangeStroke); override;
    function FocusCellRange(ARow, AColumn: Integer): Boolean; override;
    function ClipboardInputAvailable: Boolean; override;
    function BeginCellPresentation: INyxCollectionCellPresentation; override;
    function PrepareColumnFit(const ALaunch: INyxCollectionColumnsCapture;
      const AField: TNyxColumnIdentity;
      var AMeasurement: INyxColumnFitMeasurement): INyxPreparedPublication; override;
    function DeferTargetRefresh: Boolean; override;
    function DeferComposingRefresh: Boolean; override;
    procedure RetireTargetInput; override;
    procedure RenderDataset; override;
    procedure DetachTarget; override;
  public
    constructor Create(AHost: TJSHTMLElement; const AView: INyxCollectionView;
      const ALimits: TNyxTableTextWorkLimits;
      AInputPhase: TNyxCollectionInputPhase = ncipReady;
      const AInputGate: INyxCollectionInputGate = nil);
    function BeginColumnKey(AEvent: TJSKeyboardEvent): INyxBrowserColumnCommand;
    function BeginClipboardKey(AEvent: TJSKeyboardEvent): INyxBrowserColumnCommand;
    function BeginRangeKey(AEvent: TJSKeyboardEvent): INyxBrowserColumnCommand;
    function BeginRangePointer(AEvent: TJSPointerEvent): INyxBrowserColumnCommand;
    function BeginRangeStroke(AEvent: TJSPointerEvent;
      APhase: TNyxCollectionRangeStrokePhase;
      const AGeneration: INyxCancellationScope): INyxBrowserColumnCommand;
    function BeginCellContext(AEvent: TJSMouseEvent): INyxBrowserColumnCommand;
    function CaptureFocusedDraft: INyxCollectionFocusedDraftCapture;
    function BeginColumnDoubleClick(AEvent: TJSMouseEvent): INyxBrowserColumnCommand;
    function BeginColumnClick(AEvent: TJSMouseEvent): INyxBrowserColumnCommand;
    function BeginColumnRelease(AEvent: TJSKeyboardEvent): INyxBrowserColumnCommand;
    function SuspendCellEdits: INyxCollectionEditsSnapshot;
    procedure ResumeCellEdits(const ASnapshot: INyxCollectionEditsSnapshot;
      AFocus: Boolean);
    function RestoreCellEdits(const ASnapshot: INyxCollectionEditsSnapshot;
      AFocus: Boolean): Boolean;
    function CellEditFocusMatches(const AStore: INyxCollection;
      const ACell: TNyxCollectionCellEdit): Boolean;
  end;

const
  ColumnResizeAttribute = 'data-nyx-column-resize';
  ColumnResizeSelector = '[data-nyx-column-resize]';
  ColumnResizeInstructions = 'Resize column. Left and Right adjust; Shift uses larger steps. ' +
    'Home and End choose bounds; Enter accepts; Escape cancels. Tab leaves without applying.';
  ColumnResizeFitInstructions = ColumnResizeInstructions +
    ' Ctrl+Shift+F or double-click fits accepted text.';
  ColumnFitInstructions = 'Fit column to accepted text. Enter, Space or Ctrl+Shift+F fits. ' +
    'Double-click also fits.';
  ColumnFitDoubleEvent = 'dblclick';
  ColumnFitClickEvent = 'click';
  ColumnCompositionStart = 'compositionstart';
  ColumnCompositionEnd = 'compositionend';
  { DOM protocol names stay private constants. Capture precedes row/default and
    bubbling renderer handlers. beforeinput/paste/drop prevent text changes;
    input/change also stop uncancellable late notifications reaching defaults. }
  PendingInputEvents: array[0..26] of String = (
    'keydown', 'keypress', 'keyup', 'beforeinput', 'input', 'change',
    'paste', 'cut', 'drop', 'dragstart', 'contextmenu',
    'pointerdown', 'pointerup', 'pointermove', 'mousedown', 'mouseup',
    'mousemove', 'touchstart', 'touchmove', 'touchend', 'wheel',
    'dblclick', 'click', ColumnCompositionStart, 'compositionupdate',
    ColumnCompositionEnd, 'selectstart');

constructor TBrowserRangePointerCommand.Create(
  const ACommand: INyxCollectionCellRangePointerEntry);
begin
  inherited Create;
  FCommand := ACommand;
end;

destructor TBrowserRangePointerCommand.Destroy;
begin
  try
    Retire;
  except
    { Explicit retirement reports failure; final release still frees fields. }
  end;
  inherited Destroy;
end;

procedure TBrowserRangePointerCommand.Prepare;
begin
  try
    FCommand.Prepare;
  except
    on E: ENyxCollectionRangeStale do
    begin
      { A nested newer range wins; this original physical stroke stays claimed. }
    end;
  end;
end;

function TBrowserRangePointerCommand.GetCurrent: Boolean;
begin
  Result := (FCommand <> nil) and
    ((FGeneration = nil) or not FGeneration.Cancelled) and FCommand.Current;
end;

function TBrowserRangePointerCommand.GetDispatchCurrent: Boolean;
begin
  Result := (FCommand <> nil) and
    ((FGeneration = nil) or not FGeneration.Cancelled) and FCommand.DispatchCurrent;
end;

function TBrowserRangePointerCommand.Complete(AAllowDefault: Boolean): Boolean;
var
  LCommand: INyxCollectionCellRangePointerEntry;
  LReceipt: INyxCollectionCellRangePointerReceipt;
  LSubject: INyxCollectionRangeStroke;
  LOwner: TBrowserMount;
  LArm: Boolean;
begin
  LCommand := FCommand;
  LOwner := FOwner;
  LSubject := FSubject;
  LArm := FArm;
  FArm := False;
  Result := False;

  if LCommand = nil then
  begin
    Exit;
  end;
  FCompleted := True;
  FCompleting := True;
  try
    try
      Result := LCommand.Complete(AAllowDefault and GetCurrent);

      if (LOwner = nil) or (FCommand <> LCommand) then
      begin
        Exit;
      end;

      if LArm and AAllowDefault and GetCurrent and not FCaptureRequested and
        Supports(LCommand, INyxCollectionCellRangePointerReceipt, LReceipt) and
        LReceipt.Installed then
      begin
        LOwner.ArmRangeGesture(FPointer, FGeneration, FEndpoint);
      end
      else if (LSubject <> nil) and ((FPhase = ncrspRelease) or
        not AAllowDefault or FCaptureRequested or not GetCurrent or not LSubject.Current) then
      begin
        LOwner.FinishRangeGesture(LSubject);
      end;
    except
      { Cancellation is still attempted, but a secondary paint/capture cleanup
        failure must not replace the original canonical/focus diagnostic. }
      try

        if (LOwner <> nil) and (FCommand = LCommand) and (LSubject <> nil) then
        begin
          LOwner.FinishRangeGesture(LSubject);
        end;
      except
      end;
      raise;
    end;
  finally
    FCompleting := False;
  end;
end;

procedure TBrowserRangePointerCommand.ObservePointerRequest(ARequest: TNyxPointerRequest);
begin

  if ARequest <> nprUnchanged then
  begin
    FCaptureRequested := True;

    if (FOwner <> nil) and (FSubject <> nil) and
      (FOwner.FRangeGesture = FSubject) then
    begin
      { Ownership transfers to the explicit application directive. Cleanup of
        this pending range must not undo the capture it has just requested. }
      FOwner.FRangeCaptured := False;
    end;
  end;
end;

procedure TBrowserRangePointerCommand.Retire;
var
  LCommand: INyxCollectionCellRangePointerEntry;
  LOwner: TBrowserMount;
  LSubject: INyxCollectionRangeStroke;
  LAbandon: Boolean;
  LMountLease: INyxCollectionMount;
begin
  LCommand := FCommand;
  LOwner := FOwner;
  LSubject := FSubject;
  LAbandon := not FCompleted or FCompleting;

  if LOwner <> nil then
  begin
    LMountLease := LOwner;
  end;
  FCommand := nil;
  FOwner := nil;
  FGeneration := nil;
  FSubject := nil;

  try

    if LCommand <> nil then
    begin
      LCommand.Retire;
    end;
  finally

    if LAbandon and (LOwner <> nil) and (LSubject <> nil) then
    begin
      LOwner.FinishRangeGesture(LSubject);
    end;
  end;
end;

constructor TBrowserRangeKeyCommand.Create(const ACommand: INyxCollectionCellRangeKeyCommand);
begin
  inherited Create;
  FCommand := ACommand;
end;

destructor TBrowserRangeKeyCommand.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TBrowserRangeKeyCommand.Prepare;
begin
  try
    FCommand.Prepare;
  except
    on E: ENyxCollectionRangeStale do
    begin
      { A nested newer range keeps authority; the original key remains claimed. }
    end;
  end;
end;

function TBrowserRangeKeyCommand.GetCurrent: Boolean;
begin
  Result := (FCommand <> nil) and FCommand.Current;
end;

function TBrowserRangeKeyCommand.GetDispatchCurrent: Boolean;
begin
  Result := (FCommand <> nil) and FCommand.DispatchCurrent;
end;

function TBrowserRangeKeyCommand.Complete(AAllowDefault: Boolean): Boolean;
begin
  Result := FCommand.Complete(AAllowDefault);
end;

procedure TBrowserRangeKeyCommand.Retire;
var
  LCommand: INyxCollectionCellRangeKeyCommand;
begin
  LCommand := FCommand;
  FCommand := nil;

  if LCommand <> nil then
  begin
    LCommand.Retire;
  end;
end;

constructor TBrowserCopyCommand.Create(const ACommand: INyxCollectionClipboardCommand);
begin
  inherited Create;
  FCommand := ACommand;
end;

constructor TBrowserCellPresentation.Create(AOwner: TBrowserMount);
begin
  inherited Create;

  if AOwner.FCellPresentationDepth = High(Integer) then
  begin
    raise ENyxCollection.Create('Cell presentation nesting is exhausted');
  end;
  FOwner := AOwner;
  FLease := AOwner as INyxCollectionMount;
  Inc(AOwner.FCellPresentationDepth);
end;

destructor TBrowserCellPresentation.Destroy;
begin
  Leave;
  inherited Destroy;
end;

procedure TBrowserCellPresentation.Leave;
var
  LOwner: TBrowserMount;
  LLease: INyxCollectionMount;
begin
  LOwner := FOwner;
  LLease := FLease;
  FOwner := nil;
  FLease := nil;

  if LOwner <> nil then
  begin
    Dec(LOwner.FCellPresentationDepth);
  end;
end;

function TBrowserMount.BeginCellPresentation: INyxCollectionCellPresentation;
begin
  Result := nil;

  if GetInputReady and not FUpdating then
  begin
    Result := TBrowserCellPresentation.Create(Self);
  end;
end;

function TBrowserMount.CaptureFocusedDraft: INyxCollectionFocusedDraftCapture;
var
  LLease: INyxCollectionMount;
  LCommands: INyxCollectionClipboardCommands;
  LEntry: INyxCollectionClipboardCellInput;
  LInspection: INyxCollectionClipboardInspectionInput;
  LCommand: INyxCollectionClipboardCellCommand;
  LPrepared: INyxCollectionClipboardPreparedScope;
  LReady: INyxCancellationReadiness;
  LLifetime: INyxCollectionInputLifetime;
  LHost: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LCell: TJSHTMLElement;
  LRowElement: TJSHTMLElement;
  LRow: TBrowserRow;
  LView: INyxCollectionView;
  LData: INyxCollectionSnapshot;
  LSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
  LItem: TNyxItemRef;
  LField: TNyxColumnIdentity;
  LText: TNyxText;
  LSelection: TNyxTextSelection;
  LValue: INyxCollectionCellText;
  LEpoch: Integer;
  LMutation: Integer;
  LIndex: Integer;
  LColumn: Integer;

  function FlagsCurrent: Boolean;
  begin
    Result := GetConnected and GetInputReady and not FUpdating and
      not FReadOnly and not FEditsSuspended and ClipboardInputAvailable and
      (FView = LView) and (FRendered = LData) and (FHost = LHost) and
      (FCellMutationSerial = LMutation) and LLifetime.Ready and
      (LLifetime.Epoch = LEpoch);
  end;

  function Original: Boolean;
  begin
    Result := FlagsCurrent and (document.activeElement = LInput) and
      (LIndex >= 0) and (LIndex < Length(FRows)) and (FRows[LIndex] = LRow) and
      (LColumn >= 0) and (LColumn < Length(LRow.FInputs)) and
      (LRow.FInputs[LColumn] = LInput) and (LRow.FElement = LRowElement) and
      (LRow.FRef.ID = LItem.ID) and
      (LRow.FRef.Collection.Name = LItem.Collection.Name) and
      (not FHasColumnLayout or ((LColumn < FColumnLayout.Count) and
        FColumnLayout.ColumnAt(LColumn).IsVisible));

    if Result then
    begin
      Result := not LInput.disabled and not LInput.readOnly and
        (LInput._type = 'text') and not LInput.hasAttribute('hidden') and
        LHost.contains(LInput);
    end;
  end;

begin
  Result := nil;
  LLease := Self as INyxCollectionMount;
  LCommands := FClipboardCommands;
  LLifetime := GetInputLifetime;
  LHost := FHost;
  LView := FView;
  LData := FRendered;

  if (LCommands = nil) or (LHost = nil) or (LData = nil) or
    not GetInputReady or FReadOnly or not ClipboardInputAvailable then
  begin
    Exit;
  end;
  LEpoch := LLifetime.Epoch;
  LMutation := FCellMutationSerial;

  { This internal reservation proves the accepted counterpart of an explicitly
    inspected draft. It never authorizes copying: a no-cell-copy policy must
    still permit readonly inspection. Physical Ctrl+C keeps its separate gate. }
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

    if not LCommand.Current or not FlagsCurrent or (document.activeElement = nil) or
      (document.activeElement.tagName <> 'INPUT') then
    begin
      Exit;
    end;
    LInput := TJSHTMLInputElement(document.activeElement);
    LCell := TJSHTMLElement(LInput.parentElement);

    if (LCell = nil) or not LHost.contains(LInput) or
      not TryStrToInt(LInput.getAttribute('data-nyx-column'), LColumn) then
    begin
      Exit;
    end;
    LRowElement := TJSHTMLElement(LCell.closest('[data-nyx-item]'));

    if LRowElement = nil then
    begin
      Exit;
    end;
    LSpec := LView.Spec;
    LItem := NyxItem(LSpec.Key, LRowElement.getAttribute('data-nyx-item'));
    LIndex := LData.IndexOf(LItem);

    if not LCommand.Current or not FlagsCurrent or (LIndex < 0) or
      (LIndex >= Length(FRows)) or (FRows[LIndex] = nil) or
      (LColumn < 0) or (LColumn >= LSpec.Count) then
    begin
      Exit;
    end;
    LRow := FRows[LIndex];

    if not Original or (LSpec.ColumnAt(LColumn).Mode <> cmEditable) then
    begin
      Exit;
    end;
    { Parsed DOM hints only locate a candidate. Exact private row/input object
      identity above, plus the copied typed spec, supplies the actual proof. }
    LField := LSpec.ColumnAt(LColumn).Identity;
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
    LText := LInput.value;

    if not LCommand.Current or not Original then
    begin
      Exit;
    end;
    LSelection := CaptureNyxBrowserSelection(LInput);

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
end;

function TBrowserMount.BeginCellContext(AEvent: TJSMouseEvent): INyxBrowserColumnCommand;
var
  LLease: INyxCollectionMount;
  LCommands: INyxCollectionClipboardCommands;
  LActions: INyxCollectionCellActions;
  LEntry: INyxCollectionCellActionInput;
  LCommand: INyxCollectionClipboardCellCommand;
  LRange: INyxCollectionCellRangeIntent;
  LRangeContext: INyxCollectionClipboardCellRangeContext;
  LRow: TBrowserRow;
  LDraft: INyxCollectionCellActionDraftCommand;
  LDraftInput: INyxCollectionFocusedDraftInput;
  LDraftActions: INyxCollectionCellDraftActions;
  LField: TNyxColumnIdentity;
  LItem: TNyxItemRef;
  LIndex: Integer;
  LColumn: Integer;
  LEditorTarget: Boolean;
begin
  Result := nil;
  LLease := Self as INyxCollectionMount;
  LCommands := FClipboardCommands;
  LActions := FCellActions;

  if (LCommands = nil) or (LActions = nil) or AEvent.defaultPrevented then
  begin
    Exit;
  end;

  if not Supports(LActions, INyxCollectionCellActionInput, LEntry) then
  begin
    Exit;
  end;

  if (AEvent.target is TJSHTMLInputElement) and
    (not Supports(LActions, INyxCollectionCellDraftActions, LDraftActions) or
      not LDraftActions.FocusedDraftEnabled) then
  begin
    Exit;
  end;
  { Intent is captured before the RAW input factory or any physical/source
    identity getter. The same envelope is returned through canonical dispatch. }

  if FCellRangeEnabled and (FCellRanges <> nil) then
  begin
    { This owned canonical getter reads flags only. Capture BEFORE the outward
      input factory: its callback may replace even an identical rectangle. }
    LRange := FCellRanges.Selection;
  end;
  LCommand := LEntry.BeginCellInput(LCommands);

  if LCommand = nil then
  begin
    Exit;
  end;

  if not LCommand.Current then
  begin
    { A stale managed reservation remains claimed. Nil would let the outer
      binding dispatch/default this old context after a nested newer menu. }
    LCommand.Retire;
    Exit(TBrowserCopyCommand.Create(LCommand));
  end;

  if (LRange <> nil) and Supports(LCommand,
    INyxCollectionClipboardCellRangeContext, LRangeContext) then
  begin
    LRangeContext.AttachRange(LRange);
  end;

  if not LCommand.Current then
  begin
    LCommand.Retire;
    Exit(TBrowserCopyCommand.Create(LCommand));
  end;

  if Supports(LCommand, INyxCollectionCellActionDraftCommand, LDraft) and
    Supports(LLease, INyxCollectionFocusedDraftInput, LDraftInput) then
  begin
    LDraft.CaptureFocusedDraft(LDraftInput);
  end;

  if not LCommand.Current then
  begin
    LCommand.Retire;
    Exit(TBrowserCopyCommand.Create(LCommand));
  end;
  for LIndex := 0 to Length(FResidentRows) - 1 do
  begin
    LRow := FRows[FResidentRows[LIndex]];

    if LRow = nil then
    begin
      Continue;
    end;
    for LColumn := 0 to Length(LRow.FCells) - 1 do
    begin
      LEditorTarget := (LColumn < Length(LRow.FInputs)) and
        (AEvent.target = LRow.FInputs[LColumn]);

      if (AEvent.target = LRow.FCells[LColumn]) or
        ((LColumn < Length(LRow.FLabels)) and (AEvent.target = LRow.FLabels[LColumn])) or
        (LEditorTarget and (LDraft <> nil) and LDraft.HasDraft) then
      begin
        LItem := LRow.FRef;
        LField := ClipboardField(LColumn);

        if not LCommand.Current then
        begin
          LCommand.Retire;
          Exit(TBrowserCopyCommand.Create(LCommand));
        end;

        if LEditorTarget and not LDraft.MatchesDraftCell(LItem, LField) then
        begin
          Exit;
        end;
        LCommand.ResolveCell(LItem, LField);

        if not LCommand.Current then
        begin
          LCommand.Retire;
        end;
        Result := TBrowserCopyCommand.Create(LCommand);
        Exit;
      end;
    end;
  end;
end;

function TBrowserMount.CellContext(AEvent: TJSMouseEvent): Boolean;
var
  LCommand: INyxBrowserColumnCommand;
begin
  Result := True;
  LCommand := BeginCellContext(AEvent);

  if LCommand = nil then
  begin
    Exit;
  end;
  try

    if LCommand.Current and not AEvent.defaultPrevented then
    begin
      LCommand.Prepare;
    end;
    LCommand.Complete(not AEvent.defaultPrevented and LCommand.DispatchCurrent);
  finally
    AEvent.preventDefault;
    AEvent.stopPropagation;
    LCommand.Retire;
  end;
end;

destructor TBrowserCopyCommand.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TBrowserCopyCommand.Prepare;
begin
  FCommand.Prepare;
end;

function TBrowserCopyCommand.GetCurrent: Boolean;
begin
  Result := FCommand.Current;
end;

function TBrowserCopyCommand.GetDispatchCurrent: Boolean;
begin
  Result := FCommand.DispatchCurrent;
end;

function TBrowserCopyCommand.Complete(AAllowDefault: Boolean): Boolean;
begin
  Result := FCommand.Complete(AAllowDefault);
end;

procedure TBrowserCopyCommand.Retire;
begin

  if FCommand <> nil then
  begin
    FCommand.Retire;
  end;
end;

function TBrowserMount.ClipboardInputAvailable: Boolean;
begin
  Result := (FHost <> nil) and (FComposingInput = nil) and not FCompositionEnding and
    (FColumnResize = nil) and (FColumnResizePending = nil) and not FOpeningColumnManager;
end;

procedure TBrowserMount.RetireCellRangeStroke(const AStroke: INyxCollectionRangeStroke);
begin
  FinishRangeGesture(AStroke);

  if InputLifetimeCurrent then
  begin
    RenderCellRange;
  end;
end;

procedure TBrowserMount.RenderCellRange;
var
  LRow: TBrowserRow;
  LResident: Integer;
  LColumn: Integer;
  LFocus: TJSHTMLElement;
begin

  if (FRangeGesture <> nil) and (not FRangeGesture.Current or
    ((FRangeGeneration <> nil) and FRangeGeneration.Cancelled)) then
  begin
    FinishRangeGesture(FRangeGesture);
  end;

  if FHost = nil then
  begin
    Exit;
  end;
  LFocus := TJSHTMLElement(document.activeElement);

  if (LFocus = nil) or not FHost.contains(LFocus) or
    (LFocus.getAttribute('role') <> 'gridcell') then
  begin
    LFocus := nil;
  end;
  for LResident := 0 to High(FResidentRows) do
  begin
    LRow := FRows[FResidentRows[LResident]];

    if LRow = nil then
    begin
      Continue;
    end;
    LRow.SyncCellRange;

    if FCellRangeEnabled and (LFocus <> nil) then
    begin
      for LColumn := 0 to High(LRow.FCells) do
      begin

        if LRow.FCells[LColumn] = LFocus then
        begin
          LRow.FCells[LColumn].setAttribute('tabindex', '0');
        end
        else
        begin
          LRow.FCells[LColumn].setAttribute('tabindex', '-1');
        end;
      end;
    end;
  end;
end;

procedure TBrowserRow.SyncCellRange;
var
  LCell: TJSHTMLElement;
  LColumn: Integer;
  LSelected: Boolean;
begin

  if not FOwner.FCellRangeEnabled and not FHadCellRange then
  begin
    Exit;
  end;
  FHadCellRange := FOwner.FCellRangeEnabled;
  for LColumn := 0 to High(FCells) do
  begin
    LCell := FCells[LColumn];
    LSelected := FOwner.CellRangeContains(FRef, FOwner.ClipboardField(LColumn));

    if LSelected then
    begin

      if LCell.getAttribute('data-nyx-range-selected') <> 'true' then
      begin
        LCell.setAttribute('data-nyx-range-selected', 'true');
        LCell.setAttribute('aria-selected', 'true');
      end;
    end
    else if LCell.hasAttribute('data-nyx-range-selected') then
    begin
      LCell.removeAttribute('data-nyx-range-selected');
      LCell.removeAttribute('aria-selected');
    end;
  end;
end;

function TBrowserMount.FocusCellRange(ARow, AColumn: Integer): Boolean;
var
  LInput: INyxCollectionInputLifetime;
  LFocus: TJSHTMLElement;
  LUpdating: Boolean;
begin
  Result := False;
  LInput := GetInputLifetime;

  if not GetInputReady or (ARow < 0) or (AColumn < 0) then
  begin
    Exit;
  end;
  EnsureRow(ARow);

  if not GetInputReady or (ARow >= Length(FRows)) or (FRows[ARow] = nil) or
    (AColumn >= Length(FRows[ARow].FCells)) then
  begin
    Exit;
  end;
  LFocus := FRows[ARow].FCells[AColumn];
  LUpdating := FUpdating;
  FUpdating := True;
  try
    FFocusedColumn := AColumn;
    { Exact TD focus leaves every unfinished editor/draft inert. No Gesture or
      live View.Selection call belongs to independent cell-range navigation. }
    LFocus.focus;
    Result := LInput.Current and (document.activeElement = LFocus);
  finally
    FUpdating := LUpdating;
  end;
end;

function TBrowserMount.BeginRangePointer(AEvent: TJSPointerEvent): INyxBrowserColumnCommand;
begin
  Result := RangePointerCommand(AEvent, nil);
end;

function TBrowserMount.RangePointerCommand(AEvent: TJSPointerEvent;
  const AGeneration: INyxCancellationScope): INyxBrowserColumnCommand;
const
  CInteractive = 'input,textarea,select,button,a[href],[contenteditable="true"]';
var
  LCommand: INyxCollectionCellRangePointerEntry;
  LTarget: TJSHTMLElement;
  LFocus: TJSElement;
  LRow: TBrowserRow;
  LIndex: Integer;
  LColumn: Integer;
  LOrigin: TNyxCollectionCellIdentity;
  LEndpoint: TNyxCollectionCellIdentity;
  LWrapper: TBrowserRangePointerCommand;
begin
  Result := nil;
  FRangePointerCell := nil;

  if not FCellRangeEnabled or AEvent.defaultPrevented or not AEvent.isPrimary or
    (AEvent.button <> 0) or not AEvent.shiftKey or AEvent.ctrlKey or
    AEvent.metaKey or AEvent.altKey then
  begin
    Exit;
  end;
  { Reservation precedes the first hit/field/source/focus getter. Unsupported
    physical targets retire it without reinterpreting ordinary row/editor input. }
  LCommand := BeginCellRangePointer(ncrpExtend);

  if LCommand = nil then
  begin
    Exit;
  end;
  try
    FinishRangeGesture(FRangeGesture);

    if not LCommand.Current then
    begin
      Exit;
    end;
    LTarget := TJSHTMLElement(AEvent.target);

    if (LTarget = nil) or (LTarget.closest(CInteractive) <> nil) then
    begin
      Exit;
    end;
    LTarget := TJSHTMLElement(LTarget.closest('[role=gridcell]'));
    LFocus := document.activeElement;
    LOrigin := Default(TNyxCollectionCellIdentity);
    LEndpoint := Default(TNyxCollectionCellIdentity);
    for LIndex := 0 to High(FResidentRows) do
    begin
      LRow := FRows[FResidentRows[LIndex]];

      if LRow = nil then
      begin
        Continue;
      end;
      for LColumn := 0 to High(LRow.FCells) do
      begin

        if LRow.FCells[LColumn] = LFocus then
        begin
          LOrigin := NyxCollectionCellIdentity(LRow.FRef, ClipboardField(LColumn));
        end;

        if LRow.FCells[LColumn] = LTarget then
        begin
          LEndpoint := NyxCollectionCellIdentity(LRow.FRef, ClipboardField(LColumn));
        end;
      end;
    end;

    if not LOrigin.Defined or not LEndpoint.Defined then
    begin
      Exit;
    end;
    FRangePointerCell := LTarget;
    LCommand.ResolveCells(LOrigin, LEndpoint);
    LWrapper := TBrowserRangePointerCommand.Create(LCommand);
    Result := LWrapper;

    if (AGeneration <> nil) and (AEvent.pointerId >= 0) and
      (AEvent.pointerId < High(Integer)) then
    begin
      LWrapper.FOwner := Self;
      LWrapper.FGeneration := AGeneration;
      LWrapper.FPointer := NyxCollectionRangeStrokePointer(ncrsdMouse, AEvent.pointerId);
      LWrapper.FEndpoint := LEndpoint;
      LWrapper.FArm := AEvent.pointerType = 'mouse';
    end;
  finally

    if Result = nil then
    begin
      LCommand.Retire;
    end;
  end;
end;

procedure TBrowserMount.FinishRangeGesture(const ASubject: INyxCollectionRangeStroke);
var
  LPointer: TNyxCollectionRangeStrokePointer;
  LHost: TJSHTMLElement;
  LCaptured: Boolean;
begin

  if (ASubject = nil) or (FRangeGesture <> ASubject) then
  begin
    Exit;
  end;
  LPointer := FRangePointer;
  LHost := FHost;
  LCaptured := FRangeCaptured;
  { Withdraw before a physical release can synchronously dispatch capture loss.
    A nested new stroke owns independent flags and must survive this tail. }
  FRangeGesture := nil;
  FRangeGeneration := nil;
  FRangeCaptured := False;
  try

    if LCaptured and (LHost <> nil) and
      TNyxGestureElement(LHost).hasPointerCapture(LPointer.ID) and
      not ((FRangeGesture <> nil) and FRangeCaptured and
        FRangePointer.Same(LPointer)) then
    begin
      LHost.releasePointerCapture(LPointer.ID);
    end;
  finally
    AbandonCellRangeStroke(LPointer, ASubject);
  end;
end;

procedure TBrowserMount.ArmRangeGesture(
  const APointer: TNyxCollectionRangeStrokePointer;
  const AGeneration: INyxCancellationScope;
  const AEndpoint: TNyxCollectionCellIdentity);
var
  LEntry: INyxCollectionCellRangeStrokeEntry;
  LStroke: INyxCollectionRangeStroke;
  LHost: TJSHTMLElement;
  LAttemptedCapture: Boolean;

  procedure ReleaseSupersededCapture;
  begin
    { Acquisition itself can dispatch retirement. Reconcile the entered pair
      after return, leaving a newer stroke's same host/pointer ownership intact. }
    if LAttemptedCapture and (FRangeGesture <> LStroke) and
      TNyxGestureElement(LHost).hasPointerCapture(APointer.ID) and
      not ((FRangeGesture <> nil) and FRangeCaptured and
        FRangePointer.Same(APointer)) then
    begin
      LHost.releasePointerCapture(APointer.ID);
    end;
  end;

begin

  if (AGeneration = nil) or AGeneration.Cancelled or not InputLifetimeCurrent then
  begin
    Exit;
  end;
  LEntry := BeginCellRangeStroke(ncrspBegin, APointer, ncrpExtend);

  if LEntry = nil then
  begin
    Exit;
  end;
  try
    LEntry.ResolveCells(AEndpoint, AEndpoint);
    LEntry.Prepare;
    LEntry.Complete(not AGeneration.Cancelled);
    LStroke := LEntry.PendingStroke;

    if (LStroke = nil) or not LEntry.Current or not LStroke.Current or
      AGeneration.Cancelled then
    begin
      AbandonCellRangeStroke(APointer, LStroke);
      Exit;
    end;
    LHost := FHost;
    FRangePointer := APointer;
    FRangeGeneration := AGeneration;
    FRangeGesture := LStroke;
    LAttemptedCapture := False;
    try
      { Real capture is required; synthetic tests may inject this platform seam
        explicitly, but an inactive DOM pointer never masquerades as captured. }
      if TNyxGestureElement(LHost).hasPointerCapture(APointer.ID) then
      begin
        FinishRangeGesture(LStroke);
        Exit;
      end;

      if (FRangeGesture <> LStroke) or AGeneration.Cancelled or not LEntry.Current then
      begin
        FinishRangeGesture(LStroke);
        Exit;
      end;
      FRangeCaptured := True;
      LAttemptedCapture := True;
      LHost.setPointerCapture(APointer.ID);
      ReleaseSupersededCapture;

      if (FRangeGesture <> LStroke) or AGeneration.Cancelled or
        not LEntry.Current or not TNyxGestureElement(LHost).hasPointerCapture(APointer.ID) then
      begin
        FinishRangeGesture(LStroke);
      end;
    except
      { The accepted Shift-click is already complete. Refused optional capture
        withdraws its preview rather than replacing that successful endpoint. }
      try
        FinishRangeGesture(LStroke);
      finally
        ReleaseSupersededCapture;
      end;
    end;
  finally
    LEntry.Retire;
  end;
end;

function TBrowserMount.BeginRangeStroke(AEvent: TJSPointerEvent;
  APhase: TNyxCollectionRangeStrokePhase;
  const AGeneration: INyxCancellationScope): INyxBrowserColumnCommand;
var
  LEntry: INyxCollectionCellRangeStrokeEntry;
  LWrapper: TBrowserRangePointerCommand;
  LSubject: INyxCollectionRangeStroke;
  LTarget: TJSHTMLElement;
  LFocus: TJSElement;
  LRow: TBrowserRow;
  LIndex: Integer;
  LColumn: Integer;
  LOrigin: TNyxCollectionCellIdentity;
  LEndpoint: TNyxCollectionCellIdentity;
begin
  Result := nil;

  if APhase = ncrspBegin then
  begin
    Exit(RangePointerCommand(AEvent, AGeneration));
  end;
  LSubject := FRangeGesture;

  if (LSubject = nil) or (AEvent.pointerId <> FRangePointer.ID) or
    (AEvent.pointerType <> 'mouse') then
  begin
    Exit;
  end;

  if (APhase = ncrspRelease) and ((AEvent.button <> 0) or
    ((AEvent.buttons and 1) <> 0)) then
  begin
    Exit;
  end;

  if (AGeneration = nil) or (FRangeGeneration <> AGeneration) or
    AGeneration.Cancelled or not LSubject.Current or
    ((APhase = ncrspMove) and ((AEvent.buttons and 1) = 0)) then
  begin
    FinishRangeGesture(LSubject);
    Exit;
  end;
  LEntry := BeginCellRangeStroke(APhase, FRangePointer, ncrpExtend, LSubject);

  if LEntry = nil then
  begin
    FinishRangeGesture(LSubject);
    Exit;
  end;
  try
    { Capture retargets event.target to the host. Hit-test the physical point,
      then accept only exact resident cells belonging to this mount. }
    LTarget := TJSHTMLElement(TRangeHitDocument(document).elementFromPoint(
      AEvent.clientX, AEvent.clientY));
    LFocus := document.activeElement;
    LOrigin := Default(TNyxCollectionCellIdentity);
    LEndpoint := Default(TNyxCollectionCellIdentity);

    if (LTarget <> nil) and
      (LTarget.closest('input,textarea,select,button,a[href],[contenteditable="true"]') = nil) then
    begin
      LTarget := TJSHTMLElement(LTarget.closest('[role=gridcell]'));
      for LIndex := 0 to High(FResidentRows) do
      begin
        LRow := FRows[FResidentRows[LIndex]];

        if LRow = nil then
        begin
          Continue;
        end;
        for LColumn := 0 to High(LRow.FCells) do
        begin

          if LRow.FCells[LColumn] = LFocus then
          begin
            LOrigin := NyxCollectionCellIdentity(LRow.FRef, ClipboardField(LColumn));
          end;

          if LRow.FCells[LColumn] = LTarget then
          begin
            LEndpoint := NyxCollectionCellIdentity(LRow.FRef, ClipboardField(LColumn));
          end;
        end;
      end;
    end;
    { A nonnil original envelope remains claimed even outside the grid. The
      unresolved candidate then cancels; it never falls into ordinary row input. }
    if LOrigin.Defined and LEndpoint.Defined then
    begin
      LEntry.ResolveCells(LOrigin, LEndpoint);
    end;
    LWrapper := TBrowserRangePointerCommand.Create(LEntry);
    Result := LWrapper;
    LWrapper.FOwner := Self;
    LWrapper.FGeneration := AGeneration;
    LWrapper.FPointer := FRangePointer;
    LWrapper.FSubject := LSubject;
    LWrapper.FPhase := APhase;
  finally

    if Result = nil then
    begin
      LEntry.Retire;
      FinishRangeGesture(LSubject);
    end;
  end;
end;

function TBrowserMount.RangeGestureCancel(AEvent: TJSEvent): Boolean;
var
  LSubject: INyxCollectionRangeStroke;
begin
  Result := True;
  LSubject := FRangeGesture;

  if LSubject = nil then
  begin
    Exit;
  end;

  if AEvent._type = 'keydown' then
  begin

    if (TJSKeyboardEvent(AEvent).key <> 'Escape') or AEvent.defaultPrevented then
    begin
      Exit;
    end;
  end
  else if AEvent is TJSPointerEvent then
  begin

    if TJSPointerEvent(AEvent).pointerId <> FRangePointer.ID then
    begin
      Exit;
    end;

    if (AEvent._type = 'lostpointercapture') and FRangeCaptured and
      TNyxGestureElement(FHost).hasPointerCapture(FRangePointer.ID) then
    begin
      Exit;
    end;
  end;
  FinishRangeGesture(LSubject);
end;

function TBrowserMount.BeginRangeKey(AEvent: TJSKeyboardEvent): INyxBrowserColumnCommand;
var
  LKey: TNyxCollectionRangeKey;
  LCommand: INyxCollectionCellRangeKeyCommand;
  LPageCommand: INyxCollectionCellRangePageKeyCommand;
  LRow: TBrowserRow;
  LIndex: Integer;
  LColumn: Integer;
  LOffset: Double;
  LExtent: Double;
begin
  Result := nil;

  if not FCellRangeEnabled or AEvent.defaultPrevented or AEvent.isComposing or
    AEvent.altKey or AEvent.getModifierState('AltGraph') or
    (AEvent.ctrlKey and AEvent.metaKey) then
  begin
    Exit;
  end;

  if (AEvent.key = 'Escape') and not AEvent.shiftKey and
    not AEvent.ctrlKey and not AEvent.metaKey and (GetCellRange <> nil) then
  begin
    LKey := ncrkClear;
  end
  else
  begin

    if not AEvent.shiftKey then
    begin
      Exit;
    end;
    case AEvent.key of
      'ArrowLeft':
        begin
          LKey := ncrkExtendLeft;
        end;
      'ArrowRight':
        begin
          LKey := ncrkExtendRight;
        end;
      'ArrowUp':
        begin
          LKey := ncrkExtendUp;
        end;
      'ArrowDown':
        begin
          LKey := ncrkExtendDown;
        end;
      'Home':
        begin
          LKey := ncrkExtendRowStart;
        end;
      'End':
        begin
          LKey := ncrkExtendRowEnd;
        end;
      'PageUp':
        begin
          LKey := ncrkExtendPageUp;
        end;
      'PageDown':
        begin
          LKey := ncrkExtendPageDown;
        end;
      else
      begin
        Exit;
      end;
    end;

    if AEvent.ctrlKey or AEvent.metaKey then
    begin

      if AEvent.key = 'Home' then
      begin
        LKey := ncrkExtendFirstCell;
      end
      else if AEvent.key = 'End' then
      begin
        LKey := ncrkExtendLastCell;
      end
      else
      begin
        Exit;
      end;
    end;

    if FHasColumnLayout and
      (window.getComputedStyle(FHost).getPropertyValue('direction') = 'rtl') then
    begin

      if LKey = ncrkExtendLeft then
      begin
        LKey := ncrkExtendRight;
      end
      else if LKey = ncrkExtendRight then
      begin
        LKey := ncrkExtendLeft;
      end;
    end;
  end;
  { Resident cell identity is private and callback-free. Reserve BEFORE any
    immutable page/item getter. Inputs/custom descendants keep native behavior. }
  for LIndex := 0 to High(FResidentRows) do
  begin
    LRow := FRows[FResidentRows[LIndex]];

    if LRow = nil then
    begin
      Continue;
    end;
    for LColumn := 0 to High(LRow.FCells) do
    begin

      if AEvent.target = LRow.FCells[LColumn] then
      begin
        LCommand := BeginCellRangeKey(LKey);

        if LCommand <> nil then
        begin

          if (LKey in [ncrkExtendPageUp, ncrkExtendPageDown]) and LCommand.Current and
            Supports(LCommand, INyxCollectionCellRangePageKeyCommand, LPageCommand) then
          begin
            { Use the clipped data plane, without virtual overscan or retained
              offscreen drafts. Geometry is observed only after input reservation. }
            RowViewport(LOffset, LExtent);

            if LCommand.Current then
            begin
              LPageCommand.ResolvePageRows(Max(1,
                FGeometry.Window(LOffset, LExtent, 0).Count - 1));
            end;
          end;

          if (LKey <> ncrkClear) and LCommand.Current then
          begin
            LCommand.ResolveOrigin(NyxCollectionCellIdentity(LRow.FRef,
              ClipboardField(LColumn)));
          end;
          Result := TBrowserRangeKeyCommand.Create(LCommand);
        end;
        Exit;
      end;
    end;
  end;
end;

function TBrowserMount.BeginClipboardKey(AEvent: TJSKeyboardEvent): INyxBrowserColumnCommand;
var
  LCommands: INyxCollectionClipboardCommands;
  LCommand: INyxCollectionClipboardCommand;
  LRow: TBrowserRow;
  LIndex: Integer;
  LColumn: Integer;
  LIntent: INyxCollectionCellRangeIntent;
  LRangeInput: INyxCollectionClipboardRangeInput;
  LRangeCommand: INyxCollectionClipboardRangeCommand;
begin
  Result := nil;
  LCommands := FClipboardCommands;
  LIntent := GetCellRange;

  if (LCommands = nil) or AEvent.defaultPrevented or AEvent.isComposing or
    AEvent.altKey or AEvent.shiftKey or AEvent.getModifierState('AltGraph') or
    not (AEvent.ctrlKey xor AEvent.metaKey) or (LowerCase(AEvent.key) <> 'c') then
  begin
    Exit;
  end;
  { Pure identity matching precedes the first source/router callback. Keyboard
    focus belongs to the owned cell itself. An input, textarea, select, custom
    descendant or editor can never be mistaken for a data-copy command. }
  for LIndex := 0 to Length(FResidentRows) - 1 do
  begin
    LRow := FRows[FResidentRows[LIndex]];

    if LRow = nil then
    begin
      Continue;
    end;
    for LColumn := 0 to Length(LRow.FCells) - 1 do
    begin

      if AEvent.target = LRow.FCells[LColumn] then
      begin

        if LIntent <> nil then
        begin

          if not Supports(LCommands, INyxCollectionClipboardRangeInput, LRangeInput) then
          begin
            raise ENyxCollection.Create('Collection clipboard cannot reserve original cell range input');
          end;
          LRangeCommand := LRangeInput.BeginRangeInput;
          LRangeCommand.ResolveRange(LIntent);
          LCommand := LRangeCommand;
        end
        else
        begin
          LCommand := LCommands.BeginCell(LRow.FRef, ClipboardField(LColumn));
        end;
        Exit(TBrowserCopyCommand.Create(LCommand));
      end;
    end;
  end;
end;

{ Pure, nonthrowing arbitration step. Saturation permanently retires this
  proposal; the existing exact-slot cleanup/frame owns physical retirement.
  No source/widget getter, counter reset or fallible Install step belongs here. }
function AdvanceColumnLayoutActivity(AProposal: TColumnLayoutProposal): Boolean;
begin
  Result := (AProposal <> nil) and not AProposal.Retired and
    (AProposal.Activity > 0) and (AProposal.Activity < High(Integer) - 1);

  if Result then
  begin
    Inc(AProposal.Activity);
  end
  else if AProposal <> nil then
  begin
    AProposal.Activity := High(Integer);
    AProposal.Retired := True;
  end;
end;

constructor TBrowserManagerCommand.Create(AOwner: TBrowserMount;
  APress: TBrowserManagerPress; const ALease: IInterface;
  AEvent: TEventListenerEvent; AAllowOpen: Boolean);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerLease := AOwner as INyxCollectionMount;
  FPress := APress;
  FPressLease := ALease;
  FPrevious := AOwner.FColumnManagerPress;
  FPreviousLease := AOwner.FColumnManagerPressLease;
  FEvent := AEvent;
  FAllowOpen := AAllowOpen;
  FEntryAdmitted := AOwner.GetInputReady and AOwner.FEnabled and
    not AOwner.FUpdating and not AOwner.FEditsSuspended and
    not AOwner.FOpeningColumnManager and (AOwner.FComposingInput = nil);
end;

destructor TBrowserManagerCommand.Destroy;
begin
  Retire;
  inherited Destroy;
end;

function TBrowserManagerCommand.GetDispatchCurrent: Boolean;
begin
  { Only original lifetime qualifies canonical callbacks. Changes made by a
    legitimate action may reject Open without deleting semantic delivery. }
  Result := not FRetired and not FCompleteStarted and (FPress <> nil) and
    (FOwner.FView = FPress.View) and (FOwner.FHost = FPress.Host) and
    (FOwner.GetInputLifetime = FPress.Input) and (FPress.Input <> nil) and
    FPress.Input.Current;
end;

function TBrowserManagerCommand.CurrentOriginal: Boolean;
begin
  Result := not FRetired and (FPress <> nil) and not FPress.Cancelled and
    (FOwner.FView = FPress.View) and (FOwner.FHost = FPress.Host) and
    (FOwner.FColumnLauncher = FPress.Launcher) and
    (FOwner.FColumnManager = FPress.Manager) and (FPress.Manager <> nil) and
    (FOwner.FColumnManagerSerial = FPress.ManagerSerial) and
    (FPress.ManagerSerial < High(Integer)) and
    (FOwner.FColumnResizeSerial = FPress.ResizeSerial) and
    (FPress.ResizeSerial < High(Integer)) and
    (FOwner.FCellMutationSerial = FPress.Mutation) and
    (FPress.Mutation < High(Integer)) and
    (FOwner.FColumnLayout.Serial = FPress.LayoutSerial) and
    (FOwner.FRendered = FPress.Rendered) and FOwner.FEnabled and
    not FOwner.FUpdating and not FOwner.FEditsSuspended and
    not FOwner.FOpeningColumnManager and (FOwner.FComposingInput = nil);

  if Result and FSuperseding then
  begin
    Result := (FOwner.FColumnResize = nil) and
      (FOwner.FColumnResizePending = nil) and
      (FOwner.FColumnSpacePress = nil) and
      (FOwner.FColumnManagerPress = nil);
  end
  else if Result then
  begin
    Result := (FOwner.FColumnResize = FPress.Active) and
      (FOwner.FColumnResizePending = FPress.Pending) and
      (FOwner.FColumnSpacePress = FPress.FitPress) and
      (FOwner.FColumnManagerPress = FPrevious);

    if Result and (FPress.Active <> nil) then
    begin
      Result := not FPress.Active.Retired and
        (FPress.ActiveActivity > 0) and (FPress.ActiveActivity < High(Integer)) and
        (FPress.Active.Activity = FPress.ActiveActivity);
    end;

    if Result and (FPress.Pending <> nil) then
    begin
      Result := not FPress.Pending.Retired and
        (FPress.PendingActivity > 0) and (FPress.PendingActivity < High(Integer)) and
        (FPress.Pending.Activity = FPress.PendingActivity);
    end;
  end;

  if Result then
  begin
    { Final checks read only the canonical independent input state. Epoch is
      essential: disable/re-enable and suspend/resume must never revive a press. }
    Result := (FOwner.GetInputLifetime = FPress.Input) and
      (FPress.Input <> nil) and (FPress.Epoch > 0) and
      (FPress.Epoch < High(Integer)) and FPress.Input.Ready and
      (FPress.Input.Epoch = FPress.Epoch);
  end;
end;

function TBrowserManagerCommand.GetCurrent: Boolean;
begin
  Result := not FCompleteStarted and CurrentOriginal;
end;

function TBrowserManagerCommand.GetActivatesButton: Boolean;
begin
  Result := True;
end;

function TBrowserManagerCommand.GetAwaitsRelease: Boolean;
begin
  Result := (FEvent is TJSKeyboardEvent) and (FEvent._type = 'keydown') and
    (TJSKeyboardEvent(FEvent).key = ' ');
end;

function TBrowserManagerCommand.GetEntryAdmitted: Boolean;
begin
  Result := FEntryAdmitted;
end;

procedure TBrowserManagerCommand.Prepare;
var
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LLayout: TNyxColumnLayoutObservation;
  LMatches: Boolean;
begin

  if FPrepared or FCompleteStarted or FRetired then
  begin
    raise ENyxCollection.Create('Browser Columns preparation is single-use');
  end;
  FPrepared := True;

  if not FAllowOpen or not CurrentOriginal then
  begin
    Exit;
  end;
  LColumns := NyxCollectionColumns(FPress.View);

  if not CurrentOriginal then
  begin
    Exit;
  end;
  LCapture := LColumns.CaptureColumns;

  if (LCapture = nil) or not CurrentOriginal then
  begin
    Exit;
  end;
  LLayout := TNyxColumnLayoutObservation.Read(LCapture);
  LMatches := (LLayout.Serial = FPress.LayoutSerial) and
    (LLayout.Spec.ToData.ToJSON = FOwner.FColumnLayout.Spec.ToData.ToJSON) and
    (LLayout.Preferences.ToData.ToJSON = FOwner.FColumnLayout.Preferences.ToData.ToJSON);

  if LMatches and CurrentOriginal then
  begin
    FPress.Capture := LCapture;
  end;
end;

function TBrowserManagerCommand.HoldUntilRelease: Boolean;
var
  LFitLease: IInterface;
begin
  Result := True;

  if FCompleteStarted or FRetired then
  begin
    Exit;
  end;

  if not GetAwaitsRelease or not FEntryAdmitted or not FPrepared or
    not FAllowOpen or FEvent.defaultPrevented or not GetDispatchCurrent or
    (FOwner.FColumnManagerPress <> FPrevious) or
    (FOwner.FColumnSpacePress <> FPress.FitPress) or
    (FOwner.FColumnManagerSerial <> FPress.ManagerSerial) or
    (FPress.ManagerSerial >= High(Integer) - 1) then
  begin
    FCompleteStarted := True;
    Retire;
    Exit;
  end;
  FCompleteStarted := True;
  try

    if TJSKeyboardEvent(FEvent)._repeat then
    begin
      { A repeat cannot replace the first prepared press. Retiring this command
        must not cancel that original unless the canonical repeat was consumed. }
      FTransferred := (FPrevious <> nil) and not FPrevious.Cancelled;
      Exit;
    end;
    Inc(FOwner.FColumnManagerSerial);
    FPress.ManagerSerial := FOwner.FColumnManagerSerial;
    LFitLease := FOwner.FColumnSpacePressLease;

    if FOwner.FColumnSpacePress <> nil then
    begin
      FOwner.FColumnSpacePress.Cancelled := True;
      FOwner.FColumnSpacePress.Proposal.Retired := True;
    end;
    FOwner.FColumnSpacePress := nil;
    FOwner.FColumnSpacePressLease := nil;
    FPress.FitPress := nil;
    FPress.FitPressLease := nil;

    if FPrevious <> nil then
    begin
      FPrevious.Cancelled := True;
    end;
    FOwner.FColumnManagerPress := FPress;
    FOwner.FColumnManagerPressLease := FPressLease;
    FTransferred := True;
  finally
    Retire;
  end;
end;

function TBrowserManagerCommand.Complete(AAllowDefault: Boolean): Boolean;
begin
  Result := True;

  if FCompleteStarted or FRetired then
  begin
    Exit;
  end;
  FCompleteStarted := True;
  try

    if not AAllowDefault or not FAllowOpen or not FPrepared or
      FEvent.defaultPrevented or (FPress.Capture = nil) or
      not CurrentOriginal or (FPress.ManagerSerial >= High(Integer) - 1) or
      (FPress.ResizeSerial >= High(Integer) - 1) then
    begin
      Exit;
    end;
    { Reserve the accepted intention before releasing old pointer capture can
      call application code. A nested accepted intention advances this clock;
      the original command can never reopen over it after that callback. }
    Inc(FOwner.FColumnManagerSerial);
    FPress.ManagerSerial := FOwner.FColumnManagerSerial;
    Inc(FPress.ResizeSerial);
    FSuperseding := True;
    FOwner.CancelColumnResizes;

    if FEvent.defaultPrevented or not CurrentOriginal then
    begin
      Exit;
    end;
    FOwner.FOpeningColumnManager := True;
    try
      FPress.Manager.Open;
    finally
      { The entered command keeps this original mount alive. No controller or
        borrowed DOM is read after Open, including its exception/retirement tail. }
      FOwner.FOpeningColumnManager := False;
    end;
  finally
    Retire;
  end;
end;

procedure TBrowserManagerCommand.Retire;
begin
  FRetired := True;

  if not FTransferred and (FPress <> nil) then
  begin
    FPress.Cancelled := True;

    if GetAwaitsRelease and (FPrevious <> nil) then
    begin
      FPrevious.Cancelled := True;
    end;
  end;
end;

constructor TBrowserColumnCommand.Create(AOwner: TBrowserMount;
  AProposal: TColumnLayoutProposal; const ALease: IInterface;
  AEvent: TEventListenerEvent; AEligible: Boolean; AActivatesButton: Boolean);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerLease := AOwner as INyxCollectionMount;
  FProposal := AProposal;
  FProposalLease := ALease;
  FEvent := AEvent;
  FEligible := AEligible;
  FActivatesButton := AActivatesButton;
  FSpaceAtEntry := AOwner.FColumnSpacePress;
  FSpaceAtEntryLease := AOwner.FColumnSpacePressLease;
  FManagerSerial := AOwner.FColumnManagerSerial;
  { Initial physical admission is distinct from later Fit freshness. A benign
    model edit can invalidate the default without deleting canonical key phases. }
  FEntryAdmitted := AOwner.GetInputReady and not AOwner.FEditsSuspended and
    not AOwner.FUpdating and AOwner.FEnabled and (AOwner.FComposingInput = nil);
  FActive := AOwner.FColumnResize;
  FPending := AOwner.FColumnResizePending;
  FActiveLease := AOwner.FColumnResizeLease;
  FPendingLease := AOwner.FColumnResizePendingLease;

  if FActive <> nil then
  begin
    FActiveActivity := FActive.Activity;
  end;

  if FPending <> nil then
  begin
    FPendingActivity := FPending.Activity;
  end;
end;

destructor TBrowserColumnCommand.Destroy;
begin
  Retire;
  inherited Destroy;
end;

function TBrowserColumnCommand.CurrentOriginal: Boolean;
begin
  Result := not FRetired and (FProposal <> nil) and
    (FOwner.FColumnManagerSerial = FManagerSerial) and
    (FManagerSerial < High(Integer)) and
    not FProposal.Retired;

  if Result then
  begin

    if FSuperseding then
    begin
      Result := (FOwner.FColumnResize = nil) and (FOwner.FColumnResizePending = nil) and
        (FOwner.FColumnSpacePress = nil);
    end
    else
    begin
      Result := (FOwner.FColumnResize = FActive) and (FOwner.FColumnResizePending = FPending) and
        (FOwner.FColumnSpacePress = FSpaceAtEntry);

      if Result and (FActive <> nil) then
      begin
        Result := not FActive.Retired and (FActiveActivity > 0) and
          (FActiveActivity < High(Integer)) and (FActive.Activity = FActiveActivity);
      end;

      if Result and (FPending <> nil) then
      begin
        Result := not FPending.Retired and (FPendingActivity > 0) and
          (FPendingActivity < High(Integer)) and (FPending.Activity = FPendingActivity);
      end;
    end;
  end;

  if Result then
  begin
    { Finish on the original canonical input epoch. Slots/activity above are
      pure retained/cached facts, so no later borrowed observation follows it. }
    Result := FOwner.ColumnResizeContext(FProposal, False);
  end;
end;

function TBrowserColumnCommand.GetCurrent: Boolean;
begin
  Result := not FCompleteStarted and CurrentOriginal;
end;

function TBrowserColumnCommand.GetActivatesButton: Boolean;
begin
  Result := FActivatesButton;
end;

function TBrowserColumnCommand.GetAwaitsRelease: Boolean;
begin
  Result := FActivatesButton and (FEvent is TJSKeyboardEvent) and
    (FEvent._type = 'keydown') and (TJSKeyboardEvent(FEvent).key = ' ');
end;

function TBrowserColumnCommand.GetEntryAdmitted: Boolean;
begin
  Result := FEntryAdmitted;
end;

constructor TBrowserButtonPress.Create(ACommand: TBrowserColumnCommand);
begin
  inherited Create;
  Proposal := ACommand.FProposal;
  ProposalLease := ACommand.FProposalLease;
  Active := ACommand.FActive;
  Pending := ACommand.FPending;
  ActiveLease := ACommand.FActiveLease;
  PendingLease := ACommand.FPendingLease;
  ActiveActivity := ACommand.FActiveActivity;
  PendingActivity := ACommand.FPendingActivity;
end;

function TBrowserColumnCommand.HoldUntilRelease: Boolean;
var
  LPress: TBrowserButtonPress;
  LLease: IInterface;
  LManagerLease: IInterface;
  LAdmitted: Boolean;
begin
  Result := True;

  if FCompleteStarted or FRetired then
  begin
    Exit;
  end;
  LAdmitted := GetAwaitsRelease and GetDispatchCurrent and FEntryAdmitted and
    FPrepared and FEligible and not FEvent.defaultPrevented and
    (FOwner.FColumnManagerSerial = FManagerSerial) and
    (FManagerSerial < High(Integer)) and
    (FOwner.FColumnSpacePress = FSpaceAtEntry);
  FCompleteStarted := True;
  try

    if not LAdmitted then
    begin
      Exit;
    end;

    if TJSKeyboardEvent(FEvent)._repeat then
    begin
      { Auto-repeat neither activates nor replaces the first prepared press.
        Its own candidate has no future owner and is retired independently. }

      if (FSpaceAtEntry <> nil) and not FSpaceAtEntry.Cancelled then
      begin
        FProposal.Retired := True;
        FTransferred := True;
      end;
      Exit;
    end;
    LPress := TBrowserButtonPress.Create(Self);
    LLease := LPress;

    if FSpaceAtEntry <> nil then
    begin
      FSpaceAtEntry.Cancelled := True;
      FSpaceAtEntry.Proposal.Retired := True;
    end;
    FOwner.FColumnSpacePress := LPress;
    FOwner.FColumnSpacePressLease := LLease;
    LManagerLease := FOwner.FColumnManagerPressLease;

    if FOwner.FColumnManagerPress <> nil then
    begin
      FOwner.FColumnManagerPress.Cancelled := True;
    end;
    FOwner.FColumnManagerPress := nil;
    FOwner.FColumnManagerPressLease := nil;
    FTransferred := True;
  finally
    Retire;
  end;
end;

function TBrowserColumnCommand.GetDispatchCurrent: Boolean;
begin
  { This is lifetime, NOT default-command permission or freshness. The original
    canonical input incarnation never revives after mount/ancestor retirement.
    Same-owner refresh, disable, nested input or model edits may still deliver
    the ordinary remaining callbacks; Complete separately refuses their stale Fit. }
  Result := not FRetired and not FCompleteStarted and (FProposal <> nil) and
    (FOwner.FView = FProposal.View) and (FOwner.FHost = FProposal.Host) and
    (FOwner.GetInputLifetime = FProposal.Input) and (FProposal.Input <> nil) and
    FProposal.Input.Current;
end;

procedure TBrowserColumnCommand.Prepare;
var
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LLayout: TNyxColumnLayoutObservation;
begin

  if FPrepared or FCompleteStarted or FRetired then
  begin
    raise ENyxCollection.Create('Browser column input preparation is single-use');
  end;
  FPrepared := True;

  if not FEligible or not CurrentOriginal then
  begin
    Exit;
  end;
  LColumns := NyxCollectionColumns(FProposal.View);

  if not CurrentOriginal then
  begin
    Exit;
  end;
  LCapture := LColumns.CaptureColumns;

  if (LCapture = nil) or not CurrentOriginal then
  begin
    Exit;
  end;
  LLayout := TNyxColumnLayoutObservation.Read(LCapture);

  if not CurrentOriginal or (LLayout.Serial <> FProposal.LayoutSerial) or
    (LLayout.Spec.ToData.ToJSON <> FOwner.FColumnLayout.Spec.ToData.ToJSON) or
    (LLayout.Preferences.ToData.ToJSON <> FOwner.FColumnLayout.Preferences.ToData.ToJSON) then
  begin
    Exit;
  end;
  FProposal.Columns := LColumns;
  FProposal.Capture := LCapture;
end;

function TBrowserColumnCommand.Complete(AAllowDefault: Boolean): Boolean;
begin
  { Even an originally denied scoped action stays claimed. A later bubbling
    listener may not capture a newly enabled field after canonical callbacks. }
  Result := True;

  if FCompleteStarted or FRetired then
  begin
    Exit;
  end;
  FCompleteStarted := True;
  try

    if not AAllowDefault or not FPrepared or not FEligible or
      FEvent.defaultPrevented or not CurrentOriginal or
      not FOwner.ColumnResizeContext(FProposal) or
      (FProposal.Capture = nil) or (FProposal.Columns = nil) or
      (FProposal.Serial >= High(Integer) - 1) then
    begin
      Exit;
    end;
    { The old slots are still exactly the observed ones. Cancel flags/clears
      them before capture-release callouts; local leases retain both displaced
      owners. Only this accepted default advances arbitration, never a consumed key. }
    Inc(FProposal.Serial);
    FSuperseding := True;
    FOwner.CancelColumnResizes;

    if FEvent.defaultPrevented or not CurrentOriginal or
      not FOwner.ColumnResizeContext(FProposal) then
    begin
      Exit;
    end;
    { Admit the EXISTING frame before transferring authority. A failed queue
      leaves no accepted pending command; a reentrant newer owner still wins. }
    FOwner.QueueWindow;

    if not FOwner.FWindowQueued or not CurrentOriginal then
    begin
      Exit;
    end;
    FProposal.Released := True;
    FOwner.FColumnResizePending := FProposal;
    FOwner.FColumnResizePendingLease := FProposalLease;
    FTransferred := True;
  finally
    Retire;
  end;
end;

procedure TBrowserColumnCommand.Retire;
begin
  FRetired := True;

  if not FTransferred and GetAwaitsRelease and (FSpaceAtEntry <> nil) then
  begin
    { A consumed/failed repeat cancels only its original held press. This is
      flags-only: no foreign capture/owner destructor runs during revocation. }
    FSpaceAtEntry.Cancelled := True;
    FSpaceAtEntry.Proposal.Retired := True;
  end;

  if not FTransferred and (FProposal <> nil) then
  begin
    FProposal.Retired := True;
  end;
end;

constructor TBrowserColumnFitFence.Create(AOwner: TBrowserMount;
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
  FHost := AOwner.FHost;
  FMutation := AOwner.FCellMutationSerial;
  FLayoutSerial := AOwner.FColumnLayout.Serial;
  FResizeSerial := AOwner.FColumnResizeSerial;
  { Only the exact publishing Fit command may occupy the shared pending slot.
    Generic/manual Fit callers still require both resize slots to be empty. }

  if (AOwner.FColumnResizePending <> nil) and
    (AOwner.FColumnResizePending.Action = claFit) and
    AOwner.FColumnResizePending.Publishing and
    AOwner.FColumnResizePending.Field.Same(AField) and
    (AOwner.FColumnResizePending.Capture = ALaunch) then
  begin
    FCommand := AOwner.FColumnResizePending;
    FCommandLease := AOwner.FColumnResizePendingLease;
  end;
  FColumn := -1;

  if AOwner.FHasColumnLayout then
  begin
    FColumn := AOwner.FColumnLayout.Spec.ColumnIndex(FField);
  end;
  RequireCurrent;
end;

destructor TBrowserColumnFitFence.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TBrowserColumnFitFence.RequireCurrent;
begin

  if FRetired or FInstalled or (FLaunch = nil) or (FView = nil) or
    not FOwner.FHasColumnLayout or (FOwner.FView <> FView) or
    (FHost = nil) or (FOwner.FHost <> FHost) or FOwner.FUpdating or
    FOwner.FEditsSuspended or not FOwner.FEnabled or
    (FOwner.FCellMutationSerial <> FMutation) or (FMutation = High(Integer)) or
    (FOwner.FColumnLayout.Serial <> FLayoutSerial) or
    (FOwner.FColumnResizeSerial <> FResizeSerial) or (FResizeSerial = High(Integer)) or
    (FOwner.FColumnResize <> nil) or (FOwner.FColumnResizePending <> FCommand) or
    (FColumn < 0) or (FColumn >= FOwner.FColumnLayout.Count) then
  begin
    raise ENyxCollection.Create('Column fit target was replaced or its input changed');
  end;

  if (FCommand <> nil) and ((FCommand.Action <> claFit) or
    not FCommand.Publishing or not FOwner.ColumnResizeContext(FCommand)) then
  begin
    raise ENyxCollection.Create('Column fit lost its original pending command');
  end;

  if not FOwner.FColumnLayout.ColumnAt(FColumn).Column.Identity.Same(FField) or
    not (nclfAutoSize in FOwner.FColumnLayout.ColumnAt(FColumn).Features) then
  begin
    raise ENyxCollection.Create('Column fit field or permission changed');
  end;
  { LAST: known local composition and canonical readonly input facts only.
    Disable/enable or suspend/resume cannot revive the captured epoch. }

  if (FOwner.FComposingInput <> nil) or FOwner.FCompositionEnding or
    (FInput = nil) or (FEpoch <= 0) or (FEpoch = High(Integer)) or
    not FInput.Ready or (FInput.Epoch <> FEpoch) then
  begin
    raise ENyxCollection.Create('Column fit input is no longer the admitted input');
  end;
end;

procedure TBrowserColumnFitFence.Validate;
var
  LSurface: TNyxBrowserColumnFitSurface;
begin

  if FValidationStarted or FRetired then
  begin
    raise ENyxCollection.Create('Column fit validation is single-use');
  end;
  FValidationStarted := True;

  if FMetric = nil then
  begin
    raise ENyxCollection.Create('Column fit requires a fresh measured candidate');
  end;
  RequireCurrent;
  LSurface := FOwner.ColumnFitSurface(FColumn, FMetric.Measurement.SampleRows > 0);

  if not FMetric.Matches(LSurface) then
  begin
    raise ENyxCollection.Create('Column fit fonts or surface changed; choose Fit again');
  end;
  RequireCurrent;
  FValidated := True;
  CheckReady;
end;

procedure TBrowserColumnFitFence.CheckReady;
begin

  if not FValidated then
  begin
    raise ENyxCollection.Create('Column fit is not validated');
  end;
  RequireCurrent;
end;

procedure TBrowserColumnFitFence.Install;
begin
  { The sample model plan installs width. This marker precedes all notifications;
    a later notification failure never turns the accepted plan into a refusal. }
  FInstalled := True;
end;

procedure TBrowserColumnFitFence.Notify;
begin
  { The original view refresh owns physical layout and compatible draft restore. }
end;

procedure TBrowserColumnFitFence.Retire;
begin
  FRetired := True;
end;

function TBrowserMount.ColumnFitSurface(AColumn: Integer;
  AHasBody: Boolean): TNyxBrowserColumnFitSurface;
var
  LResident, LRowIndex: Integer;
  LRow: TBrowserRow;
begin
  Result := Default(TNyxBrowserColumnFitSurface);

  if (AColumn < 0) or (AColumn >= Length(FHeaderCells)) or
    (AColumn >= Length(FHeaderLabels)) or (AColumn >= Length(FHeaderSorts)) or
    (AColumn >= Length(FHeaderFilters)) then
  begin
    raise ENyxCollection.Create('Column fit has no canonical header surface');
  end;
  Result.HeaderCell := FHeaderCells[AColumn];
  Result.HeaderLabel := FHeaderLabels[AColumn];
  Result.SortButton := FHeaderSorts[AColumn];
  Result.FilterButton := FHeaderFilters[AColumn];

  if FHasTablePresentation then
  begin
    Result.TextPolicy := FTablePresentation.TextAt(AColumn);
    Result.TextLimits := FTextLimits;
  end;

  if not AHasBody then
  begin
    Exit;
  end;
  { Use only an already resident actual row, including a hidden canonical field.
    Never realize/scroll a row or hide/blur an editor just to obtain a metric. }
  for LResident := 0 to High(FResidentRows) do
  begin
    LRowIndex := FResidentRows[LResident];

    if (LRowIndex < 0) or (LRowIndex >= Length(FRows)) then
    begin
      Continue;
    end;
    LRow := FRows[LRowIndex];

    if (LRow = nil) or (LRow.FElement.parentElement <> FBody) or
      (AColumn >= Length(LRow.FCells)) or (AColumn >= Length(LRow.FLabels)) or
      (AColumn >= Length(LRow.FInputs)) then
    begin
      Continue;
    end;
    Result.BodyCell := LRow.FCells[AColumn];
    Result.BodyLabel := LRow.FLabels[AColumn];
    Result.BodyEditor := LRow.FInputs[AColumn];
    Exit;
  end;
  raise ENyxCollection.Create('Column fit needs a resident standard body-text surface');
end;

function TBrowserMount.PrepareColumnFit(const ALaunch: INyxCollectionColumnsCapture;
  const AField: TNyxColumnIdentity;
  var AMeasurement: INyxColumnFitMeasurement): INyxPreparedPublication;
var
  LFenceObject: TBrowserColumnFitFence;
  LFence, LModel: INyxPreparedPublication;
  LSampling: INyxCollectionColumnSampling;
  LSample: INyxCollectionColumnSample;
  LSurface: TNyxBrowserColumnFitSurface;
  LCount: Integer;
begin
  LFenceObject := TBrowserColumnFitFence.Create(Self, ALaunch, AField);
  LFence := LFenceObject;
  { var avoids automatic release of a caller's previous managed sample before
    this exact mount call frame is retained. Clear it only under the lease. }
  AMeasurement := nil;
  Result := nil;
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
  LCount := LSample.Count;
  LFenceObject.RequireCurrent;
  LSurface := ColumnFitSurface(LFenceObject.FColumn, LCount > 0);
  LFenceObject.FMetric := MeasureNyxBrowserColumnFit(LSample, LSurface);
  LFenceObject.RequireCurrent;
  LModel := LSampling.PrepareSampleAutoSize(LSample, LFenceObject.FMetric.Measurement.Width);
  LFenceObject.RequireCurrent;
  Result := PrepareNyxGroup([LModel, LFence]);
  LFenceObject.RequireCurrent;
  AMeasurement := LFenceObject.FMetric.Measurement;
end;

constructor TColumnLayoutFence.Create(AOwner: TBrowserMount;
  AProposal: TColumnLayoutProposal; const AProposalLease: IInterface);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerLease := AOwner as INyxCollectionMount;
  FProposal := AProposal;
  FProposalLease := AProposalLease;
end;

procedure TColumnLayoutFence.Validate;
begin

  if FValidated or FRetired then
  begin
    raise ENyxCollection.Create('Column resize fence is no longer fresh');
  end;
  FValidated := True;
  CheckReady;
end;

procedure TColumnLayoutFence.CheckReady;
begin

  if FRetired or not FValidated or (FProposal = nil) or
    (FOwner.FColumnResizePending <> FProposal) or not FProposal.Publishing or
    not FOwner.ColumnResizeContext(FProposal) then
  begin
    raise ENyxCollection.Create('Column resize was superseded or its input retired');
  end;
end;

procedure TColumnLayoutFence.Install;
begin
  { Flag only; ordinary committed refresh must not invalidate its own request. }
  FProposal.Installed := True;
end;

procedure TColumnLayoutFence.Notify;
begin
  { The same view owns its ordinary notifications. }
end;

procedure TColumnLayoutFence.Retire;
begin
  FRetired := True;
end;

procedure ReleaseColumnResizeVisuals(AProposal: TColumnLayoutProposal);
var
  LHandle, LGuide: TJSHTMLElement;
  LPointer: NativeInt;
  LCaptured: Boolean;
begin

  if AProposal = nil then
  begin
    Exit;
  end;
  LHandle := AProposal.Handle;
  LGuide := AProposal.Guide;
  LPointer := AProposal.Pointer;
  LCaptured := AProposal.Captured;
  { Clear ownership before removing DOM/releasing capture. A physical lost-
    capture callback must not release again or resurrect the retired proposal. }
  AProposal.Guide := nil;
  AProposal.Captured := False;
  try

    if LGuide <> nil then
    begin
      LGuide.remove;
    end;
  finally

    if LCaptured and (LHandle <> nil) and
      TNyxGestureElement(LHandle).hasPointerCapture(LPointer) then
    begin
      LHandle.releasePointerCapture(LPointer);
    end;
  end;
end;

procedure TBrowserMount.InvalidateColumnResizes;
begin

  if not FHasColumnLayout then
  begin
    Exit;
  end;

  if FColumnResizeSerial < High(Integer) then
  begin
    Inc(FColumnResizeSerial);
  end;

  if FColumnResize <> nil then
  begin
    FColumnResize.Retired := True;
  end;

  if FColumnResizePending <> nil then
  begin
    FColumnResizePending.Retired := True;
  end;

  if FColumnSpacePress <> nil then
  begin
    FColumnSpacePress.Proposal.Retired := True;
  end;
end;

procedure TBrowserMount.CancelColumnButtonPress;
var
  LPress: TBrowserButtonPress;
  LLease: IInterface;
  LManagerPress: TBrowserManagerPress;
  LManagerLease: IInterface;
begin
  LPress := FColumnSpacePress;
  LLease := FColumnSpacePressLease;
  LManagerPress := FColumnManagerPress;
  LManagerLease := FColumnManagerPressLease;
  FColumnSpacePress := nil;
  FColumnSpacePressLease := nil;
  FColumnManagerPress := nil;
  FColumnManagerPressLease := nil;

  if LPress <> nil then
  begin
    LPress.Cancelled := True;
    LPress.Proposal.Retired := True;
  end;

  if LManagerPress <> nil then
  begin
    LManagerPress.Cancelled := True;
  end;
  { Local ownership ends after all receiver fields are detached. }
end;

procedure TBrowserMount.CancelColumnResizes(ACancelButton: Boolean);
var
  LActive, LPending: TColumnLayoutProposal;
  LActiveLease, LPendingLease: IInterface;
  LActiveWidth, LPendingWidth: Integer;

  function AcceptedWidth(AProposal: TColumnLayoutProposal): Integer;
  begin
    Result := 0;

    if (AProposal <> nil) and (AProposal.Action = claResize) and (AProposal.Host = FHost) and
      (AProposal.Column >= 0) and
      (AProposal.Column < Length(FHeaderResizers)) and
      (AProposal.Column < Length(FColumnWidths)) and
      (FHeaderResizers[AProposal.Column] = AProposal.Handle) then
    begin
      Result := FColumnWidths[AProposal.Column];
    end;
  end;
begin
  InvalidateColumnResizes;
  LActive := FColumnResize;
  LPending := FColumnResizePending;
  LActiveLease := FColumnResizeLease;
  LPendingLease := FColumnResizePendingLease;

  if ACancelButton then
  begin
    CancelColumnButtonPress;
  end;
  LActiveWidth := AcceptedWidth(LActive);
  LPendingWidth := AcceptedWidth(LPending);
  FColumnResize := nil;
  FColumnResizePending := nil;
  FColumnResizeLease := nil;
  FColumnResizePendingLease := nil;
  { Both authorities are already revoked. Restore the exact owned handles from
    copied accepted widths before releasing capture can enter application code.
    A cancelled draft must not remain announced until some later refresh. }
  try
    try

      if LActiveWidth > 0 then
      begin
        LActive.Handle.setAttribute('aria-valuenow', IntToStr(LActiveWidth));
      end;
    finally

      if LPendingWidth > 0 then
      begin
        LPending.Handle.setAttribute('aria-valuenow', IntToStr(LPendingWidth));
      end;
    end;
  finally
    { Independent physical cleanup is all-attempt. No mount field is consulted
      after the first DOM operation, including a reentrant capture release. }
    try
      ReleaseColumnResizeVisuals(LActive);
    finally
      ReleaseColumnResizeVisuals(LPending);
    end;
  end;
end;

procedure TBrowserMount.ReconcileColumnResizeCapture(
  AProposal: TColumnLayoutProposal);
var
  LCurrent: TColumnLayoutProposal;
begin
  { setPointerCapture can return after cancellation cleared the optimistic bit.
    Reconcile the real handle/pointer even on its exception tail. A distinct
    live proposal already claiming that same pair owns the physical capture;
    old cleanup must leave it intact. This uses cached ownership, never a model
    or router callback, and the caller retains both receiver and old proposal. }
  AProposal.Captured :=
    TNyxGestureElement(AProposal.Handle).hasPointerCapture(AProposal.Pointer);
  LCurrent := FColumnResize;

  if AProposal.Captured and (LCurrent <> nil) and (LCurrent <> AProposal) and
    not LCurrent.Retired and LCurrent.Captured and
    (LCurrent.Handle = AProposal.Handle) and
    (LCurrent.Pointer = AProposal.Pointer) then
  begin
    AProposal.Captured := False;
  end;
end;

function TBrowserMount.ColumnResizeContext(
  AProposal: TColumnLayoutProposal; ARequirePermission: Boolean): Boolean;
begin
  { This is the final publication fence as well as the callback-tail predicate.
    Only known cached fields/canonical input flags are read. Geometry is checked
    before release; an admitted pending proposal is a copied logical width,
    not a promise about arbitrary later direct DOM styling by an application. }
  Result := (AProposal <> nil) and not AProposal.Retired and
    InputLifetimeCurrent and not FEditsSuspended and not FUpdating and
    FEnabled and (FComposingInput = nil) and
    (FColumnResizeSerial = AProposal.Serial) and
    (FView = AProposal.View) and (FHost = AProposal.Host) and
    (AProposal.Column >= 0) and (AProposal.Column < FColumnLayout.Count) and
    (AProposal.Column < Length(FHeaderResizers)) and
    (AProposal.Column < Length(FColumnWidths));

  if Result then
  begin
    Result := (FHeaderResizers[AProposal.Column] = AProposal.Handle) and
      (FColumnLayout.Serial = AProposal.LayoutSerial) and
      FColumnLayout.ColumnAt(AProposal.Column).Column.Identity.Same(AProposal.Field) and
      FColumnLayout.ColumnAt(AProposal.Column).IsVisible and
      (FColumnWidths[AProposal.Column] = AProposal.InitialWidth) and
      (FColumnFont = AProposal.Font) and (FColumnAvailable = AProposal.Available);
  end;

  if Result and ARequirePermission then
  begin
    Result := ((AProposal.Action = claResize) and
      (nclfResize in FColumnLayout.ColumnAt(AProposal.Column).Features)) or
      ((AProposal.Action = claFit) and
      (nclfAutoSize in FColumnLayout.ColumnAt(AProposal.Column).Features));
  end;

  if Result and (AProposal.Action = claFit) then
  begin
    Result := not FCompositionEnding and (FRendered = AProposal.Rendered) and
      (FCellMutationSerial = AProposal.Mutation) and (AProposal.Mutation < High(Integer));

    if Result and (AProposal.Origin <> cioHeader) then
    begin
      Result := (AProposal.OriginSlot >= 0) and (AProposal.OriginSlot < Length(FRows)) and
        (FRows[AProposal.OriginSlot] = AProposal.OriginRow) and (AProposal.OriginRow <> nil);

      if Result then
      begin
        Result := (AProposal.OriginRow.FOwner = Self) and
          (AProposal.OriginRow.FRef.ID = AProposal.OriginItem.ID) and
          (AProposal.OriginRow.FRef.Collection.Name = AProposal.OriginItem.Collection.Name) and
          (AProposal.Column < Length(AProposal.OriginRow.FCells)) and
          (AProposal.Column < Length(AProposal.OriginRow.FInputs));
      end;

      if Result then
      begin

        if AProposal.Origin = cioEditor then
        begin
          Result := AProposal.OriginRow.FInputs[AProposal.Column] = AProposal.OriginElement;
        end
        else
        begin
          Result := AProposal.OriginRow.FCells[AProposal.Column] = AProposal.OriginElement;
        end;
      end;
    end;
  end;

  if Result then
  begin
    { LAST: no source, router, widget or application getter follows these
      canonical facts. Explicit suspend/disable cannot revive the old epoch. }
    Result := (AProposal.Input <> nil) and AProposal.Input.Ready and
      (AProposal.Epoch > 0) and (AProposal.Epoch < High(Integer)) and
      (AProposal.Input.Epoch = AProposal.Epoch);
  end;
end;

function TBrowserMount.ColumnResizeHandle(AEvent: TEventListenerEvent;
  out AColumn: Integer): TJSHTMLElement;
var
  LCandidate: TJSHTMLElement;
  LIndex: Integer;
begin
  Result := nil;
  AColumn := -1;

  if not FHasColumnLayout or (FHost = nil) or
    not (AEvent.target is TJSHTMLElement) then
  begin
    Exit;
  end;
  LCandidate := TJSHTMLElement(TJSHTMLElement(AEvent.target).closest(ColumnResizeSelector));

  if (LCandidate = nil) or not FHost.contains(LCandidate) then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FHeaderResizers) do
  begin

    if FHeaderResizers[LIndex] = LCandidate then
    begin
      AColumn := LIndex;
      Exit(LCandidate);
    end;
  end;
end;

function TBrowserMount.ColumnResizeGeometry(
  AProposal: TColumnLayoutProposal): Boolean;
var
  LScale, LPhysicalWidth: Double;
  LFont, LDirection: String;
  LStyle: TJSCSSStyleDeclaration;
  LAvailable: Integer;
begin
  Result := False;

  if not ColumnResizeContext(AProposal) then
  begin
    Exit;
  end;

  if not AProposal.Host.contains(AProposal.Handle) then
  begin
    Exit;
  end;
  LScale := 1;

  if AProposal.Host.offsetWidth > 0 then
  begin
    LScale := AProposal.Host.getBoundingClientRect.width / AProposal.Host.offsetWidth;
  end;
  LPhysicalWidth := FHeaderCells[AProposal.Column].getBoundingClientRect.width;
  LStyle := window.getComputedStyle(AProposal.Host);
  LFont := LStyle.getPropertyValue('font');
  LDirection := LStyle.getPropertyValue('direction');

  if LFont = '' then
  begin
    LFont := '13px sans-serif';
  end;
  LAvailable := ColumnClipWidth;
  Result := not IsNan(LScale) and not IsInfinite(LScale) and (LScale > 0) and
    not IsNan(LPhysicalWidth) and not IsInfinite(LPhysicalWidth) and
    (Abs(LScale - AProposal.Scale) < 0.000001) and
    (Abs(LPhysicalWidth - AProposal.PhysicalWidth) < 0.5) and
    ((LDirection = 'rtl') = AProposal.RightToLeft) and
    (LFont = AProposal.Font) and (LAvailable = AProposal.Available) and
    ColumnResizeContext(AProposal);
end;

function TBrowserMount.BeginColumnResize(AHandle: TJSHTMLElement;
  AColumn: Integer; AKeyboard: Boolean; AX: Double;
  APointer: NativeInt): TColumnLayoutProposal;
var
  LKeepAlive: INyxCollectionMount;
  LProposal: TColumnLayoutProposal;
  LLease: IInterface;
  LLayout: TNyxColumnLayoutObservation;
  LSizing: TNyxColumnSizing;
  LRect: TJSDOMRect;
  LSerial: Integer;
begin
  Result := nil;
  LKeepAlive := Self as INyxCollectionMount;

  if not FHasColumnLayout or not GetInputReady or FUpdating or
    (FComposingInput <> nil) or (AColumn < 0) or
    (AColumn >= FColumnLayout.Count) or
    (AColumn >= Length(FColumnWidths)) or
    (AColumn >= Length(FHeaderResizers)) or
    (FHeaderResizers[AColumn] <> AHandle) or
    not FColumnLayout.ColumnAt(AColumn).IsVisible or
    not (nclfResize in FColumnLayout.ColumnAt(AColumn).Features) or
    IsNan(AX) or IsInfinite(AX) then
  begin
    Exit;
  end;
  LSerial := FColumnResizeSerial;

  if LSerial = High(Integer) then
  begin
    Exit;
  end;
  Inc(LSerial);
  CancelColumnResizes;

  if not GetInputReady or (FHost = nil) or FUpdating or
    (FComposingInput <> nil) or (FColumnResizeSerial <> LSerial) or
    (LSerial = High(Integer)) or (FColumnResize <> nil) or
    (FColumnResizePending <> nil) or (AColumn >= FColumnLayout.Count) or
    (AColumn >= Length(FColumnWidths)) or (AColumn >= Length(FHeaderResizers)) or
    (FHeaderResizers[AColumn] <> AHandle) or
    not FColumnLayout.ColumnAt(AColumn).IsVisible or
    not (nclfResize in FColumnLayout.ColumnAt(AColumn).Features) then
  begin
    Exit;
  end;
  LProposal := TColumnLayoutProposal.Create;
  LLease := LProposal;
  LProposal.Activity := 1;
  LProposal.View := FView;
  LProposal.Input := GetInputLifetime;
  LProposal.Epoch := LProposal.Input.Epoch;
  LProposal.Serial := FColumnResizeSerial;
  LProposal.LayoutSerial := FColumnLayout.Serial;
  LProposal.Host := FHost;
  LProposal.Handle := AHandle;
  LProposal.Column := AColumn;
  LProposal.Field := FColumnLayout.ColumnAt(AColumn).Column.Identity;
  LProposal.InitialWidth := FColumnWidths[AColumn];
  LProposal.Width := LProposal.InitialWidth;
  LProposal.Available := FColumnAvailable;
  LProposal.Font := FColumnFont;
  LProposal.Keyboard := AKeyboard;
  LProposal.Pointer := APointer;
  LProposal.StartX := AX;
  LSizing := FColumnLayout.ColumnAt(AColumn).Column.Sizing;

  if not LSizing.Defined then
  begin
    LSizing := NyxColumnSizing;
  end;
  LProposal.Minimum := LSizing.Bounds.MinimumValue;
  LProposal.Maximum := LSizing.Bounds.MaximumValue;
  LProposal.Columns := NyxCollectionColumns(LProposal.View);
  LProposal.Capture := LProposal.Columns.CaptureColumns;
  LLayout := TNyxColumnLayoutObservation.Read(LProposal.Capture);

  if not ColumnResizeContext(LProposal) or
    (LLayout.Serial <> LProposal.LayoutSerial) or
    (LLayout.Spec.ToData.ToJSON <> FColumnLayout.Spec.ToData.ToJSON) or
    (LLayout.Preferences.ToData.ToJSON <> FColumnLayout.Preferences.ToData.ToJSON) then
  begin
    Exit;
  end;
  LProposal.RightToLeft :=
    window.getComputedStyle(LProposal.Host).getPropertyValue('direction') = 'rtl';
  LProposal.PhysicalWidth := FHeaderCells[AColumn].getBoundingClientRect.width;
  LRect := LProposal.Host.getBoundingClientRect;
  LProposal.Scale := 1;

  if LProposal.Host.offsetWidth > 0 then
  begin
    LProposal.Scale := LRect.width / LProposal.Host.offsetWidth;
  end;

  if IsNan(LProposal.Scale) or IsInfinite(LProposal.Scale) or
    (LProposal.Scale <= 0) or not ColumnResizeGeometry(LProposal) then
  begin
    Exit;
  end;
  FColumnResize := LProposal;
  FColumnResizeLease := LLease;
  Result := LProposal;
end;

procedure TBrowserMount.PreviewColumnResize(AProposal: TColumnLayoutProposal);
var
  LGuide: TJSHTMLElement;
begin

  if (FColumnResize <> AProposal) or not ColumnResizeContext(AProposal) then
  begin
    Exit;
  end;
  LGuide := AProposal.Guide;

  if LGuide = nil then
  begin
    LGuide := NewElement('span');
    LGuide.setAttribute('aria-hidden', 'true');
    LGuide.style.cssText := 'position:absolute;top:0;bottom:0;pointer-events:none;' +
      'z-index:7;padding:2px 4px;white-space:nowrap;' +
      'border-inline-end:2px solid var(--nyx-accent,#6953df);' +
      'background:var(--nyx-accent-soft,#eeeafd);color:var(--nyx-text,#202331);';
    AProposal.Guide := LGuide;
    FHeaderCells[AProposal.Column].appendChild(LGuide);
  end;
  LGuide.style.setProperty('inset-inline-end',
    IntToStr(AProposal.InitialWidth - AProposal.Width) + 'px');
  LGuide.textContent := IntToStr(AProposal.Width) + ' px';
  AProposal.Handle.setAttribute('aria-valuenow', IntToStr(AProposal.Width));
end;

procedure TBrowserMount.FinishColumnResize(AProposal: TColumnLayoutProposal);
var
  LLease: IInterface;
begin

  if FColumnResize <> AProposal then
  begin
    Exit;
  end;

  if not ColumnResizeContext(AProposal) then
  begin
    CancelColumnResizes;
    Exit;
  end;
  LLease := FColumnResizeLease;

  if not AdvanceColumnLayoutActivity(AProposal) then
  begin
    QueueWindow;
    Exit;
  end;
  AProposal.Released := True;
  FColumnResizePending := AProposal;
  FColumnResizePendingLease := LLease;
  FColumnResize := nil;
  FColumnResizeLease := nil;
  try
    ReleaseColumnResizeVisuals(AProposal);
  except
    AProposal.Retired := True;

    if FColumnResizePending = AProposal then
    begin
      FColumnResizePending := nil;
      FColumnResizePendingLease := nil;
    end;
    raise;
  end;
  { Capture release can call application code, retire the view or start another
    gesture. Only this SAME pending lease may queue its copied intention. }

  if (FColumnResizePending = AProposal) and ColumnResizeContext(AProposal) then
  begin
    QueueWindow;
  end;
end;

procedure TBrowserMount.PublishColumnResize;
var
  LProposal: TColumnLayoutProposal;
  LLease: IInterface;
  LPreferences: TNyxColumnPreferences;
  LPreference: TNyxColumnPreference;
  LPrepared, LFence: INyxPreparedPublication;
  LMeasurement: INyxColumnFitMeasurement;
  LIndex: Integer;
begin
  LProposal := FColumnResizePending;
  LLease := FColumnResizePendingLease;

  if (LProposal = nil) or LProposal.Publishing then
  begin
    Exit;
  end;
  LProposal.Publishing := True;
  try

    if not AdvanceColumnLayoutActivity(LProposal) then
    begin
      Exit;
    end;

    if not LProposal.Released or not ColumnResizeContext(LProposal) then
    begin
      Exit;
    end;
    try
      LFence := TColumnLayoutFence.Create(Self, LProposal, LLease);

      if LProposal.Action = claFit then
      begin
        LPrepared := PrepareColumnFit(LProposal.Capture, LProposal.Field, LMeasurement);
        { Original command validation precedes the nested Fit model/metric;
          metric Matches is the last known outward UI validator. }
        PublishNyxGroup([LFence, LPrepared]);
      end
      else
      begin
        LPreferences := LProposal.Capture.Preferences;
        LPreference := NyxColumnPreference(LProposal.Field);
        LIndex := LPreferences.IndexOf(LProposal.Field);

        if LIndex >= 0 then
        begin
          LPreference := LPreferences.ColumnAt(LIndex);
        end;
        LPreferences := LPreferences.WithColumn(LPreference.Width(LProposal.Width));
        LPrepared := LProposal.Columns.PrepareColumns(LProposal.Capture, LPreferences);
        PublishNyxGroup([LPrepared, LFence]);
      end;
    except
      on LException: Exception do
      begin
        Failed(LException);
      end;
    end;
  finally
    LProposal.Retired := True;

    if FColumnResizePending = LProposal then
    begin
      FColumnResizePending := nil;
      FColumnResizePendingLease := nil;
    end;
  end;
end;

function TBrowserMount.ColumnResizeDown(AEvent: TJSPointerEvent): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LHandle: TJSHTMLElement;
  LProposal: TColumnLayoutProposal;
  LLease: IInterface;
  LColumn: Integer;
begin
  Result := True;

  if not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if AEvent.defaultPrevented or not AEvent.isPrimary or (AEvent.button <> 0) then
  begin
    Exit;
  end;
  LHandle := ColumnResizeHandle(AEvent, LColumn);

  if (LHandle = nil) or (FHost = nil) or
    TNyxGestureElement(FHost).hasPointerCapture(AEvent.pointerId) then
  begin
    Exit;
  end;
  { A Fit-only grip must not focus/blur the live editor on its first physical
    down. Suppress only that exact enabled face's focus default; do not capture
    a pointer, read/commit a draft or borrow manual Resize permission. }

  if (LColumn < FColumnLayout.Count) and
    FColumnLayout.ColumnAt(LColumn).IsVisible and
    (nclfAutoSize in FColumnLayout.ColumnAt(LColumn).Features) and
    not (nclfResize in FColumnLayout.ColumnAt(LColumn).Features) and GetInputReady then
  begin
    AEvent.preventDefault;
    Exit;
  end;
  LProposal := BeginColumnResize(LHandle, LColumn, False,
    AEvent.clientX, AEvent.pointerId);

  if LProposal = nil then
  begin
    Exit;
  end;
  LLease := FColumnResizeLease;

  if AEvent.defaultPrevented or not ColumnResizeContext(LProposal) then
  begin

    if FColumnResize = LProposal then
    begin
      CancelColumnResizes;
    end;
    Exit;
  end;
  AEvent.preventDefault;
  try
    { A capture getter could have given an authored gesture the host pointer
      since the first entry check. Recheck before taking this exact handle. }

    if TNyxGestureElement(LProposal.Host).hasPointerCapture(AEvent.pointerId) then
    begin
      LProposal.Retired := True;
      CancelColumnResizes;
      Exit;
    end;
    { Trusted input uses actual pointer capture; synthetic fixture events have
      no active platform pointer to capture. Never take authored host capture. }

    if AEvent.isTrusted then
    begin
      LProposal.Captured := True;
      try
        LHandle.setPointerCapture(AEvent.pointerId);
      except
        try
          ReconcileColumnResizeCapture(LProposal);
        except
          { Preserve the acquisition failure if platform reconciliation also
            fails. The outer handler still attempts old-lease cleanup. }
        end;
        raise;
      end;
      ReconcileColumnResizeCapture(LProposal);
    end;

    if (FColumnResize <> LProposal) or not ColumnResizeContext(LProposal) or
      (AEvent.isTrusted and not LProposal.Captured) then
    begin
      LProposal.Retired := True;

      if FColumnResize = LProposal then
      begin
        CancelColumnResizes;
      end
      else
      begin
        ReleaseColumnResizeVisuals(LProposal);
      end;
      Exit;
    end;
    PreviewColumnResize(LProposal);
  except
    LProposal.Retired := True;
    try

      if FColumnResize = LProposal then
      begin
        CancelColumnResizes;
      end
      else
      begin
        ReleaseColumnResizeVisuals(LProposal);
      end;
    except
      { A cleanup failure must not replace the original acquisition/preview
        exception. Slots and flags were already revoked before its callout. }
    end;
    raise;
  end;
end;

function TBrowserMount.ColumnResizeMove(AEvent: TJSPointerEvent): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LProposal: TColumnLayoutProposal;
  LLease: IInterface;
  LWidth, LDelta: Double;
begin
  Result := True;

  if not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LProposal := FColumnResize;
  LLease := FColumnResizeLease;

  if (LProposal = nil) or LProposal.Keyboard or
    (LProposal.Pointer <> AEvent.pointerId) then
  begin
    Exit;
  end;

  if AEvent.defaultPrevented or not ColumnResizeGeometry(LProposal) or
    (LProposal.Captured and
      not TNyxGestureElement(LProposal.Handle).hasPointerCapture(LProposal.Pointer)) or
    IsNan(AEvent.clientX) or IsInfinite(AEvent.clientX) then
  begin
    CancelColumnResizes;
    Exit;
  end;
  LDelta := (AEvent.clientX - LProposal.StartX) / LProposal.Scale;

  if LProposal.RightToLeft then
  begin
    LDelta := -LDelta;
  end;
  LWidth := LProposal.InitialWidth + LDelta;

  if IsNan(LWidth) or IsInfinite(LWidth) then
  begin
    CancelColumnResizes;
    Exit;
  end;
  AEvent.preventDefault;

  if not AdvanceColumnLayoutActivity(LProposal) then
  begin
    QueueWindow;
    Exit;
  end;
  LProposal.Width := Round(Max(LProposal.Minimum, Min(LProposal.Maximum, LWidth)));
  PreviewColumnResize(LProposal);
end;

function TBrowserMount.ColumnResizeUp(AEvent: TJSPointerEvent): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LProposal: TColumnLayoutProposal;
  LLease: IInterface;
begin
  Result := True;

  if not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LProposal := FColumnResize;
  LLease := FColumnResizeLease;

  if (LProposal = nil) or LProposal.Keyboard or
    (LProposal.Pointer <> AEvent.pointerId) then
  begin
    Exit;
  end;
  ColumnResizeMove(AEvent);

  if (FColumnResize = LProposal) and ColumnResizeContext(LProposal) then
  begin
    AEvent.preventDefault;
    FinishColumnResize(LProposal);
  end;
end;

function TBrowserMount.ColumnResizeCancel(AEvent: TJSPointerEvent): Boolean;
var
  LKeepAlive: INyxCollectionMount;
begin
  Result := True;

  if not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if (FColumnResize <> nil) and not FColumnResize.Keyboard and
    (FColumnResize.Pointer = AEvent.pointerId) and
    (AEvent.target = FColumnResize.Handle) then
  begin

    if (AEvent._type = 'lostpointercapture') and FColumnResize.Captured and
      TNyxGestureElement(FColumnResize.Handle).hasPointerCapture(AEvent.pointerId) then
    begin
      { A queued loss from an earlier capture cannot cancel the newer owner
        which already holds this same physical handle/pointer pair. }
      Exit;
    end;
    CancelColumnResizes;
    QueueWindow;
  end;
end;

function TBrowserMount.IsColumnFitButton(AEvent: TEventListenerEvent): Boolean;
var
  LColumn: Integer;
begin
  Result := False;

  if not FHasColumnLayout or (FHost = nil) or (FView = nil) then
  begin
    Exit;
  end;
  for LColumn := 0 to High(FHeaderResizers) do
  begin

    if (AEvent.target = FHeaderResizers[LColumn]) and
      (LColumn < FColumnLayout.Count) then
    begin
      Result := (nclfAutoSize in FColumnLayout.ColumnAt(LColumn).Features) and
        not (nclfResize in FColumnLayout.ColumnAt(LColumn).Features);
      Exit;
    end;
  end;
end;

function TBrowserMount.NewColumnFitCommand(AEvent: TEventListenerEvent;
  AKeyboard: Boolean; AAllowFit: Boolean;
  AActivatesButton: Boolean): INyxBrowserColumnCommand;
var
  LProposal: TColumnLayoutProposal;
  LLease: IInterface;
  LTarget: TJSHTMLElement;
  LRow: TBrowserRow;
  LColumn, LIndex, LSlot, LWork: Integer;
  LOrigin: TColumnInputOrigin;
  LEligible: Boolean;
  LKeepAlive: INyxCollectionMount;
begin
  LKeepAlive := Self as INyxCollectionMount;
  Result := nil;

  if not FHasColumnLayout or (FView = nil) or (FHost = nil) or
    not (AEvent.target is TJSHTMLElement) then
  begin
    Exit;
  end;
  LTarget := TJSHTMLElement(AEvent.target);
  LColumn := -1;
  LSlot := -1;
  LRow := nil;
  LOrigin := cioHeader;
  for LIndex := 0 to High(FHeaderResizers) do
  begin

    if FHeaderResizers[LIndex] = LTarget then
    begin
      LColumn := LIndex;
      Break;
    end;
  end;

  if (LColumn < 0) and AKeyboard then
  begin
    for LWork := 0 to High(FResidentRows) do
    begin
      LSlot := FResidentRows[LWork];

      if (LSlot < 0) or (LSlot >= Length(FRows)) then
      begin
        Continue;
      end;
      LRow := FRows[LSlot];

      if LRow = nil then
      begin
        Continue;
      end;
      for LIndex := 0 to High(LRow.FCells) do
      begin

        if LRow.FCells[LIndex] = LTarget then
        begin
          LColumn := LIndex;
          LOrigin := cioCell;
          Break;
        end;

        if (LIndex < Length(LRow.FInputs)) and (LRow.FInputs[LIndex] = LTarget) then
        begin
          LColumn := LIndex;
          LOrigin := cioEditor;
          Break;
        end;
      end;

      if LColumn >= 0 then
      begin
        Break;
      end;
    end;
  end;

  if (LColumn < 0) or (LColumn >= FColumnLayout.Count) or
    (LColumn >= Length(FColumnWidths)) or (LColumn >= Length(FHeaderResizers)) then
  begin
    Exit;
  end;
  LProposal := TColumnLayoutProposal.Create;
  LLease := LProposal;
  LProposal.Activity := 1;
  LProposal.Action := claFit;
  LProposal.View := FView;
  LProposal.Input := GetInputLifetime;

  if LProposal.Input <> nil then
  begin
    LProposal.Epoch := LProposal.Input.Epoch;
  end;
  LProposal.Serial := FColumnResizeSerial;
  LProposal.LayoutSerial := FColumnLayout.Serial;
  LProposal.Mutation := FCellMutationSerial;
  LProposal.Rendered := FRendered;
  LProposal.Host := FHost;
  LProposal.Handle := FHeaderResizers[LColumn];
  LProposal.Column := LColumn;
  LProposal.Field := FColumnLayout.ColumnAt(LColumn).Column.Identity;
  LProposal.InitialWidth := FColumnWidths[LColumn];
  LProposal.Width := LProposal.InitialWidth;
  LProposal.Available := FColumnAvailable;
  LProposal.Font := FColumnFont;
  LProposal.Keyboard := AKeyboard;
  LProposal.Pointer := -1;
  LProposal.Origin := LOrigin;
  LProposal.OriginElement := LTarget;
  LProposal.OriginSlot := LSlot;

  if LOrigin <> cioHeader then
  begin
    LProposal.OriginRow := LRow;
    LProposal.OriginItem := LRow.FRef;
  end;
  LEligible := AAllowFit and FColumnLayout.ColumnAt(LColumn).IsVisible and
    (nclfAutoSize in FColumnLayout.ColumnAt(LColumn).Features);

  if LOrigin = cioEditor then
  begin
    { Entry proves the real edit identity. Subsequent focus navigation does not
      retract an accepted logical command; row/input/map mutation still does. }
    LEligible := LEligible and (FEditingItem.ID = LProposal.OriginItem.ID) and
      (FEditingItem.Collection.Name = LProposal.OriginItem.Collection.Name) and
      (FEditingColumn = LColumn);
  end;
  { DOM identity/focus observations finish only after all original scalar/model
    leases are copied. No draft text, caret or arbitrary data attribute is read. }

  if not FHost.contains(LTarget) or
    (AKeyboard and (document.activeElement <> LTarget)) then
  begin
    Exit;
  end;
  Result := TBrowserColumnCommand.Create(Self, LProposal, LLease, AEvent, LEligible,
    AActivatesButton);
end;

function TBrowserMount.BeginColumnKey(AEvent: TJSKeyboardEvent): INyxBrowserColumnCommand;
var
  LColumn: Integer;
begin
  Result := nil;

  if AEvent.defaultPrevented or AEvent.isComposing or AEvent.altKey or AEvent.metaKey or
    AEvent.getModifierState('AltGraph') then
  begin
    Exit;
  end;

  if AEvent.ctrlKey and AEvent.shiftKey and (LowerCase(AEvent.key) = 'f') then
  begin
    Exit(NewColumnFitCommand(AEvent, True));
  end;

  if AEvent.ctrlKey or AEvent.shiftKey or
    not ((AEvent.key = 'Enter') or (AEvent.key = ' ')) then
  begin
    Exit;
  end;

  if AEvent.target = FColumnLauncher then
  begin
    Exit(NewColumnManagerCommand(AEvent));
  end;
  for LColumn := 0 to High(FHeaderResizers) do
  begin

    if (AEvent.target = FHeaderResizers[LColumn]) and
      (LColumn < FColumnLayout.Count) and
      (nclfAutoSize in FColumnLayout.ColumnAt(LColumn).Features) and
      not (nclfResize in FColumnLayout.ColumnAt(LColumn).Features) then
    begin
      Exit(NewColumnFitCommand(AEvent, True, True, True));
    end;
  end;
end;

function TBrowserMount.BeginColumnDoubleClick(
  AEvent: TJSMouseEvent): INyxBrowserColumnCommand;
begin
  Result := nil;

  if not AEvent.defaultPrevented and (AEvent._type = ColumnFitDoubleEvent) and
    (AEvent.button = 0) then
  begin

    if AEvent.target = FColumnLauncher then
    begin
      { The two Clicks already own button activation. Canonical DoubleClick is
        still delivered, but its claimed default must not open a third time. }
      Exit(NewColumnManagerCommand(AEvent, False));
    end;
    { A button already activates on each click. Claim the trailing dblclick for
      canonical callbacks without a third Fit; resize separators retain their
      established double-click default. No timer or event metadata is needed. }
    Result := NewColumnFitCommand(AEvent, False, not IsColumnFitButton(AEvent));
  end;
end;

function TBrowserMount.BeginColumnClick(AEvent: TJSMouseEvent): INyxBrowserColumnCommand;
begin
  Result := nil;

  if (AEvent._type = ColumnFitClickEvent) and (AEvent.button = 0) and
    (AEvent.target = FColumnLauncher) then
  begin
    { Claim even an already-prevented same-host event. The renderer's canonical
      route or direct fallback must not recapture a fresh default later. }
    Exit(NewColumnManagerCommand(AEvent));
  end;

  if (AEvent._type = ColumnFitClickEvent) and (AEvent.button = 0) and
    IsColumnFitButton(AEvent) then
  begin
    Result := NewColumnFitCommand(AEvent, False, True, True);
  end;
end;

function TBrowserMount.BeginColumnRelease(AEvent: TJSKeyboardEvent): INyxBrowserColumnCommand;
var
  LPress: TBrowserButtonPress;
  LPressLease: IInterface;
  LCommand: TBrowserColumnCommand;
  LCommandLease: INyxBrowserColumnCommand;
  LKeepAlive: INyxCollectionMount;
  LManagerPress: TBrowserManagerPress;
  LManagerLease: IInterface;
  LManagerCommand: TBrowserManagerCommand;
begin
  Result := nil;

  if (AEvent._type <> 'keyup') or (AEvent.key <> ' ') then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if FColumnManagerPress <> nil then
  begin
    LManagerPress := FColumnManagerPress;
    LManagerLease := FColumnManagerPressLease;
    FColumnManagerPress := nil;
    FColumnManagerPressLease := nil;

    if LManagerPress.Cancelled or AEvent.isComposing or
      (LManagerPress.Input = nil) or
      (LManagerPress.Epoch <= 0) or (LManagerPress.Epoch >= High(Integer)) or
      (LManagerPress.Input.Epoch <> LManagerPress.Epoch) or
      not LManagerPress.Input.Ready or
      (LManagerPress.ManagerSerial <> FColumnManagerSerial) or
      (AEvent.target <> LManagerPress.Launcher) or
      (document.activeElement <> LManagerPress.Launcher) then
    begin
      LManagerPress.Cancelled := True;
      Exit;
    end;
    LManagerCommand := TBrowserManagerCommand.Create(Self, LManagerPress,
      LManagerLease, AEvent, True);
    LCommandLease := LManagerCommand;
    LManagerCommand.FPrepared := True;

    if not LManagerCommand.GetDispatchCurrent then
    begin
      LManagerCommand.Retire;
      Exit;
    end;
    Exit(LCommandLease);
  end;

  if FColumnSpacePress = nil then
  begin
    Exit;
  end;
  LPress := FColumnSpacePress;
  LPressLease := FColumnSpacePressLease;
  FColumnSpacePress := nil;
  FColumnSpacePressLease := nil;

  if LPress.Cancelled or AEvent.isComposing or
    (LPress.Proposal.Input = nil) or
    (LPress.Proposal.Epoch <= 0) or (LPress.Proposal.Epoch >= High(Integer)) or
    (LPress.Proposal.Input.Epoch <> LPress.Proposal.Epoch) or
    (AEvent.target <> LPress.Proposal.OriginElement) or
    (document.activeElement <> LPress.Proposal.OriginElement) then
  begin
    LPress.Proposal.Retired := True;
    Exit;
  end;
  LCommand := TBrowserColumnCommand.Create(Self, LPress.Proposal,
    LPress.ProposalLease, AEvent, True, True);
  LCommandLease := LCommand;
  LCommand.FActive := LPress.Active;
  LCommand.FPending := LPress.Pending;
  LCommand.FActiveLease := LPress.ActiveLease;
  LCommand.FPendingLease := LPress.PendingLease;
  LCommand.FActiveActivity := LPress.ActiveActivity;
  LCommand.FPendingActivity := LPress.PendingActivity;
  LCommand.FPrepared := True;

  if not LCommand.GetDispatchCurrent then
  begin
    LCommand.Retire;
    Exit;
  end;
  Result := LCommandLease;
end;

function TBrowserMount.ColumnButtonRelease(AEvent: TJSKeyboardEvent): Boolean;
var
  LCommand: INyxBrowserColumnCommand;
  LKeepAlive: INyxCollectionMount;
begin
  Result := True;
  LKeepAlive := Self as INyxCollectionMount;
  LCommand := BeginColumnRelease(AEvent);

  if LCommand = nil then
  begin
    Exit;
  end;
  try
    LCommand.Complete(not AEvent.defaultPrevented);
  finally
    AEvent.preventDefault;
    AEvent.stopPropagation;
    LCommand.Retire;
  end;
end;

function TBrowserMount.ColumnButtonBlur(AEvent: TEventListenerEvent): Boolean;
var
  LKeepAlive: INyxCollectionMount;
begin
  Result := True;
  LKeepAlive := Self as INyxCollectionMount;

  if ((FColumnSpacePress <> nil) and
    ((AEvent._type = 'blur') or
    (AEvent.target = FColumnSpacePress.Proposal.OriginElement))) or
    ((FColumnManagerPress <> nil) and
    ((AEvent._type = 'blur') or
    (AEvent.target = FColumnManagerPress.Launcher))) then
  begin
    { The host capture listener runs before canonical bubbling focus callbacks.
      A callback may focus back and start a distinct press after this retirement. }
    CancelColumnButtonPress;
  end;
end;

function TBrowserMount.ColumnFitInput(AEvent: TJSMouseEvent): Boolean;
var
  LCommand: INyxBrowserColumnCommand;
begin
  Result := True;

  if AEvent._type = ColumnFitClickEvent then
  begin
    LCommand := BeginColumnClick(AEvent);
  end
  else
  begin
    LCommand := BeginColumnDoubleClick(AEvent);
  end;

  if LCommand = nil then
  begin
    Exit;
  end;
  try
    try

      if AEvent.defaultPrevented then
      begin
        { The renderer or an earlier same-host listener already consumed this
          input. In particular do not call a model capture getter merely to
          discover again that no default is allowed. }
        AEvent.stopPropagation;
        Exit;
      end;
      LCommand.Prepare;

      if LCommand.Complete(not AEvent.defaultPrevented) then
      begin
        AEvent.preventDefault;
      end;
    except
      AEvent.preventDefault;
      AEvent.stopPropagation;
      raise;
    end;
  finally
    LCommand.Retire;
  end;
end;

function TBrowserMount.ColumnResizeFocus(AEvent: TEventListenerEvent): Boolean;
var
  LKeepAlive: INyxCollectionMount;
begin
  Result := True;

  if not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if (FColumnResize <> nil) and FColumnResize.Keyboard and
    (AEvent.target = FColumnResize.Handle) then
  begin
    CancelColumnResizes;
    QueueWindow;
  end;
end;

function TBrowserMount.ColumnResizeKey(AEvent: TJSKeyboardEvent): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LHandle: TJSHTMLElement;
  LProposal: TColumnLayoutProposal;
  LLease: IInterface;
  LColumn, LStep: Integer;
  LCommand: INyxBrowserColumnCommand;
  LRelease: INyxBrowserColumnButtonRelease;
begin
  Result := True;

  if not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if AEvent.defaultPrevented or AEvent.isComposing then
  begin
    Exit;
  end;
  LCommand := BeginColumnKey(AEvent);

  if LCommand <> nil then
  begin
    try
      try
        LCommand.Prepare;

        if not AEvent.defaultPrevented and
          Supports(LCommand, INyxBrowserColumnButtonRelease, LRelease) and
          LRelease.AwaitsRelease then
        begin
          LRelease.HoldUntilRelease;
          AEvent.preventDefault;
        end
        else if LCommand.Complete(not AEvent.defaultPrevented) then
        begin
          AEvent.preventDefault;
        end;
      except
        AEvent.preventDefault;
        AEvent.stopPropagation;
        raise;
      end;
    finally
      LCommand.Retire;
    end;
    Exit;
  end;

  if (AEvent.key = 'Escape') and
    ((FColumnResize <> nil) or (FColumnResizePending <> nil) or
    (FColumnSpacePress <> nil) or (FColumnManagerPress <> nil)) then
  begin
    AEvent.preventDefault;
    CancelColumnResizes;
    QueueWindow;
    Exit;
  end;
  LHandle := ColumnResizeHandle(AEvent, LColumn);

  if LHandle = nil then
  begin
    Exit;
  end;

  if AEvent.key = 'Tab' then
  begin

    if (FColumnResize <> nil) and FColumnResize.Keyboard and
      (FColumnResize.Handle = LHandle) then
    begin
      CancelColumnResizes;
      QueueWindow;
    end;
    { Navigation does not retract a request already accepted with Enter. }
    Exit;
  end;

  if AEvent.altKey or AEvent.ctrlKey or AEvent.metaKey or
    not ((AEvent.key = 'ArrowLeft') or (AEvent.key = 'ArrowRight') or
      (AEvent.key = 'Home') or (AEvent.key = 'End') or (AEvent.key = 'Enter')) then
  begin
    Exit;
  end;
  LProposal := FColumnResize;
  LLease := FColumnResizeLease;

  if (LProposal = nil) and (AEvent.key = 'Enter') then
  begin
    Exit;
  end;

  if LProposal = nil then
  begin
    LProposal := BeginColumnResize(LHandle, LColumn, True, 0, -1);
    LLease := FColumnResizeLease;
  end;

  if AEvent.defaultPrevented or (LProposal = nil) or not LProposal.Keyboard or
    (LProposal.Handle <> LHandle) or not ColumnResizeGeometry(LProposal) then
  begin
    Exit;
  end;
  AEvent.preventDefault;

  if AEvent.key = 'Enter' then
  begin
    FinishColumnResize(LProposal);
    Exit;
  end;

  if not AdvanceColumnLayoutActivity(LProposal) then
  begin
    QueueWindow;
    Exit;
  end;
  LStep := 1;

  if AEvent.shiftKey then
  begin
    LStep := 10;
  end;

  if ((AEvent.key = 'ArrowLeft') and not LProposal.RightToLeft) or
    ((AEvent.key = 'ArrowRight') and LProposal.RightToLeft) then
  begin
    LStep := -LStep;
  end;
  case AEvent.key of
    'Home': LProposal.Width := LProposal.Minimum;
    'End': LProposal.Width := LProposal.Maximum;
  else
    LProposal.Width := Max(LProposal.Minimum,
      Min(LProposal.Maximum, LProposal.Width + LStep));
  end;
  PreviewColumnResize(LProposal);
end;

function TBrowserMount.DeferTargetRefresh: Boolean;
begin
  Result := DeferComposingRefresh;
end;

function TBrowserMount.DeferComposingRefresh: Boolean;
begin
  Result := (FHasColumnLayout or FObservesTablePresentation) and (FComposingInput <> nil);

  if Result then
  begin
    FTargetRefreshDeferred := True;
  end;
end;

procedure TBrowserMount.RetireTargetInput;
begin
  FRangePointerCell := nil;
  try
    FinishRangeGesture(FRangeGesture);
  finally
    CancelColumnButtonPress;

    if FColumnLauncher <> nil then
    begin
      FColumnLauncher.removeEventListener('pointerdown', @ColumnLauncherPointer);
      FColumnLauncher.setAttribute('disabled', '');
    end;
    InvalidateColumnResizes;
    FComposingInput := nil;
    FCompositionEnding := False;
    FCompositionChange := Default(TCompositionChange);
    FCompositionFocusPending := False;
  end;
end;

function TBrowserMount.CellComposition(AEvent: TEventListenerEvent): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LInput: TJSHTMLInputElement;
  LRow: TBrowserRow;
  LWork, LColumn: Integer;
  LOwned: Boolean;
begin
  Result := True;

  if not InputLifetimeCurrent or not GetInputReady or
    not (AEvent.target is TJSHTMLInputElement) then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LInput := TJSHTMLInputElement(AEvent.target);
  LOwned := False;
  for LWork := 0 to High(FResidentRows) do
  begin
    LRow := FRows[FResidentRows[LWork]];

    if LRow <> nil then
    begin
      for LColumn := 0 to High(LRow.FInputs) do
      begin
        LOwned := LOwned or (LRow.FInputs[LColumn] = LInput);
      end;
    end;
  end;

  if not LOwned then
  begin
    Exit;
  end;

  if AEvent._type = ColumnCompositionStart then
  begin
    InvalidateColumnResizes;

    if (FComposingInput <> nil) and (FComposingInput <> LInput) then
    begin
      { One physical IME owns one input. An overlapping synthetic start cannot
        retire the first editor's guard or impersonate an unowned descendant. }
      AEvent.preventDefault;
      AEvent.stopImmediatePropagation;
      Exit;
    end;
    FComposingInput := LInput;
    FCompositionEnding := False;
    { The raw composition latch precedes capture-release callbacks. This is
      the LAST operation on the START path; those callbacks may retire Self. }
    CancelColumnResizes;
  end
  else if (AEvent._type = ColumnCompositionEnd) and (FComposingInput = LInput) then
  begin
    FCompositionEnding := True;
    { Keep busy for every remaining synchronous end/input/change listener.
      Only the next owned window/UI turn may clear it and refresh the table. }
    QueueWindow;
  end;
end;

function TBrowserMount.BlockPendingInput(AEvent: TEventListenerEvent): Boolean;
var
  LKeepAlive: INyxCollectionMount;
begin
  Result := True;
  LKeepAlive := Self as INyxCollectionMount;

  if (FHasColumnLayout or FObservesTablePresentation) and not GetInputReady then
  begin
    AEvent.preventDefault;
    AEvent.stopImmediatePropagation;
    Result := False;
  end;
end;

function TBrowserMount.SuspendCellEdits: INyxCollectionEditsSnapshot;
var
  LKeepAlive: INyxCollectionMount;
  LCells: TNyxCollectionCellEdits;
  LSnapshot: INyxCollectionEditsSnapshot;
  LView: INyxCollectionView;
  LData: INyxCollectionSnapshot;
  LInput: TJSHTMLInputElement;
  LRow: TBrowserRow;
  LWork, LColumn, LCount, LCapacity: Integer;
  LText, LAccepted: TNyxText;
  LFocusCell: TNyxCollectionCellEdit;
  LEditing, LFocused: Boolean;
begin
  LKeepAlive := Self as INyxCollectionMount;
  FCellFocusValid := False;

  if not InputLifetimeCurrent or not (FHasColumnLayout or FObservesTablePresentation) or FEditsSuspended then
  begin
    raise ENyxCollection.Create('Cell edit suspension requires one live opted-in table lease');
  end;

  if FCellMutationSerial = High(Integer) then
  begin
    raise ENyxCollection.Create('Cell edit handoff generations are exhausted; remount the table');
  end;

  if FComposingInput <> nil then
  begin
    raise ENyxCollection.Create('Finish cell text composition before replacing this table');
  end;
  FCellFocusDraft := Default(TNyxCollectionCellEdit);
  FCellFocusValid := True;
  LView := FView;
  LData := FRendered;
  LCount := 0;
  LCells := nil;
  LFocusCell := Default(TNyxCollectionCellEdit);
  { Resident rows include offscreen dirty editors. Only realized/retained rows
    are visited; no full dataset materialization is needed for this handoff. }
  for LWork := 0 to High(FResidentRows) do
  begin
    LRow := FRows[FResidentRows[LWork]];

    if (LRow = nil) or (LData = nil) or not LData.Has(LRow.FRef) then
    begin
      Continue;
    end;
    for LColumn := 0 to High(LRow.FInputs) do
    begin
      LInput := LRow.FInputs[LColumn];

      if LInput = nil then
      begin
        Continue;
      end;
      LText := LInput.value;
      LAccepted := LView.CellText(LRow.FRef, LColumn);

      if LInput._type = 'checkbox' then
      begin
        LText := 'false';

        if LInput.checked then
        begin
          LText := 'true';
        end;
      end;
      LFocused := ColumnVisible(LColumn) and (document.activeElement = LInput);
      LEditing := ColumnVisible(LColumn) and
        (CellEditing(LRow.FRef, LColumn) or LFocused);

      if (LText = LAccepted) and not LEditing then
      begin
        Continue;
      end;

      if LCount = Length(LCells) then
      begin
        LCapacity := Max(16, LCount);

        if LCapacity > High(Integer) - LCount then
        begin
          raise ENyxCollection.Create('Too many retained cell drafts');
        end;
        SetLength(LCells, LCount + LCapacity);
      end;
      LCells[LCount] := NyxCollectionCellEdit(LRow.FRef,
        LView.Spec.ColumnAt(LColumn).Identity,
        LView.Spec.ColumnAt(LColumn).Read(LData.Item(LRow.FRef)),
        LText, CaptureNyxBrowserSelection(LInput), LEditing, LFocused);

      if LFocused then
      begin
        LFocusCell := LCells[LCount].Copy;
        FCellFocusInput := LInput;
        FCellFocusRow := FResidentRows[LWork];
        FCellFocusColumn := LColumn;
      end;
      Inc(LCount);
    end;
  end;
  SetLength(LCells, LCount);
  LSnapshot := NewNyxCollectionEditsSnapshot(LView.Store, LCells);

  if not GetConnected or (FView <> LView) or (FRendered <> LData) or
    FEditsSuspended or not FCellFocusValid then
  begin
    raise ENyxCollection.Create('Cell editing changed while its handoff was captured');
  end;

  if FComposingInput <> nil then
  begin
    raise ENyxCollection.Create('Cell text composition began while its handoff was captured');
  end;
  { No hide/blur is necessary yet. Install the complete snapshot and suppression
    flag BEFORE the renderer moves old DOM, so native change/focusout cannot
    accidentally publish unfinished text while that DOM is being detached. }
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
    (FEditSuspension <> LSnapshot) then
  begin

    if GetConnected and (FEditSuspension = LSnapshot) then
    begin
      FEditSuspension := nil;
      SetCellEditsSuspended(False);
    end;
    raise ENyxCollection.Create('Cell editing changed during suspension');
  end;
  FCellFocusStore := LSnapshot.Store;
  FCellFocusDraft := LFocusCell;
  FCellFocusSerial := FColumnLayout.Serial;
  FCellFocusValid := LFocusCell.Defined;
  Result := LSnapshot;
end;

procedure TBrowserMount.ResumeCellEdits(
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

  if not FEditsSuspended or (LSnapshot = nil) or (LSnapshot <> FEditSuspension) then
  begin
    raise ENyxCollection.Create('Cell edit resume requires its exact current suspension');
  end;
  SetCellEditsSuspended(False);
  FEditSuspension := nil;
  Refresh;

  if GetConnected then
  begin
    RestoreCellEdits(LSnapshot, AFocus);
  end;
end;

function TBrowserMount.RestoreCellEdits(
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
  LInput, LFocus: TJSHTMLInputElement;
  LFocusOptions: TFocusOptions;
  LFocusCell: TNyxCollectionCellEdit;
  LIndex, LRow, LColumn, LCount, LPass, LLayoutSerial: Integer;
  LMutationSerial: Integer;
  LUpdating: Boolean;

  procedure RequireTarget;
  var
    LCurrentData: INyxCollectionSnapshot;
    LCurrentSerial: Integer;
  begin
    LCurrentData := LView.Snapshot;
    LCurrentSerial := LColumns.CaptureColumns.Serial;

    if not InputLifetimeCurrent or FEditsSuspended or (FView <> LView) or
      (FComposingInput <> nil) or
      (FCellMutationSerial <> LMutationSerial) or
      (LMutationSerial = High(Integer)) or
      (LCurrentData <> LData) or (FRendered <> LData) or
      (FColumnLayout.Serial <> LLayoutSerial) or
      (LCurrentSerial <> LLayoutSerial) then
    begin
      { Refuse the whole candidate instead of accepting a partially imported
        draft vector after an application callback changes its target. }
      raise ENyxCollection.Create('Cell edit target changed during restoration');
    end;
  end;
begin
  Result := False;
  LKeepAlive := Self as INyxCollectionMount;
  FCellFocusValid := False;
  LSnapshot := ASnapshot;

  if not InputLifetimeCurrent or not (FHasColumnLayout or FObservesTablePresentation) or FEditsSuspended or
    (FComposingInput <> nil) or (LSnapshot = nil) then
  begin
    raise ENyxCollection.Create('Cell edit restoration requires a live opted-in table');
  end;
  LMutationSerial := FCellMutationSerial;
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
  LFocus := nil;
  LFocusCell := Default(TNyxCollectionCellEdit);
  { Restore dormant dirty inputs first. The unique active editor is realized
    LAST, so another offscreen EnsureRow pass cannot detach/free its equal-text
    input before focus/caret restoration. No earlier DOM input is retained. }
  for LPass := 0 to 1 do
  begin
    for LIndex := 0 to LSnapshot.Count - 1 do
    begin
      LCell := LSnapshot.CellAt(LIndex);

      if LCell.Editing <> (LPass = 1) then
      begin
        Continue;
      end;
      LColumn := LSpec.ColumnIndex(LCell.Field);
      LRow := LData.IndexOf(LCell.Item);

      if not FEnabled or FReadOnly or (LColumn < 0) or (LRow < 0) or
        (LSpec.ColumnAt(LColumn).Mode <> cmEditable) or
        not LSpec.ColumnAt(LColumn).Read(LData.Item(LCell.Item)).SameValue(LCell.Accepted) then
      begin
        Continue;
      end;
      { Force just this logical row through the existing window, without selecting
        it. Writing a dirty value then gives its existing HasDraft rule ownership. }
      EnsureRow(LRow);

      RequireTarget;

      if (LRow >= Length(FRows)) or (FRows[LRow] = nil) or
        (LColumn >= Length(FRows[LRow].FInputs)) then
      begin
        Continue;
      end;
      LInput := FRows[LRow].FInputs[LColumn];

      if LInput = nil then
      begin
        Continue;
      end;
      LUpdating := FUpdating;
      FUpdating := True;
      try

        if LInput._type = 'checkbox' then
        begin
          LInput.checked := LCell.Text = 'true';
        end
        else
        begin
          LInput.value := LCell.Text;
        end;

        if LCell.Editing and ColumnVisible(LColumn) then
        begin
          FEditingItem := LCell.Item;
          FEditingColumn := LColumn;
          FFocusedColumn := LColumn;
          LInput.removeAttribute('hidden');
          FRows[LRow].FLabels[LColumn].setAttribute('hidden', '');
        end;

        if LCell.Selection.Defined and CaptureNyxBrowserSelection(LInput).Defined then
        begin
          SelectNyxBrowserText(LInput, LCell.Selection);
        end;
      finally
        FUpdating := LUpdating;
      end;

      if LCell.Focused and AFocus and ColumnVisible(LColumn) then
      begin
        LFocus := LInput;
        LFocusCell := LCell.Copy;
      end;
    end;
  end;

  if (LFocus <> nil) and GetConnected and FHost.contains(LFocus) then
  begin
    { Focus can run application callbacks. No borrowed editor/caret tail follows
      until its exact current logical row/field/input has been reacquired. }
    LUpdating := FUpdating;
    FUpdating := True;
    FCellFocusStore := LStore;
    FCellFocusDraft := LFocusCell.Copy;
    FCellFocusInput := LFocus;
    FCellFocusRow := LData.IndexOf(LFocusCell.Item);
    FCellFocusColumn := LSpec.ColumnIndex(LFocusCell.Field);
    FCellFocusSerial := LLayoutSerial;
    FCellFocusValid := True;
    try
      LFocusOptions := TFocusOptions.new;
      LFocusOptions.preventScroll := True;
      TFocusable(LFocus).focus(LFocusOptions);

      RequireTarget;
      LColumn := LSpec.ColumnIndex(LFocusCell.Field);
      LRow := LData.IndexOf(LFocusCell.Item);

      if (LRow < 0) or (LColumn < 0) or not ColumnVisible(LColumn) or
        (FRows[LRow] = nil) or (FRows[LRow].FInputs[LColumn] <> LFocus) or
        not FEnabled or FReadOnly or not FHost.contains(LFocus) then
      begin
        Exit;
      end;

      if LFocusCell.Selection.Defined and
        CaptureNyxBrowserSelection(FRows[LRow].FInputs[LColumn]).Defined and
        (FRows[LRow].FInputs[LColumn].value = LFocusCell.Text) then
      begin
        SelectNyxBrowserText(FRows[LRow].FInputs[LColumn], LFocusCell.Selection);
      end;
      Result := CellEditFocusMatches(LStore, LFocusCell);
    finally
      FUpdating := LUpdating;
    end;
  end;
  QueueWindow;
end;

function TBrowserMount.CellEditFocusMatches(const AStore: INyxCollection;
  const ACell: TNyxCollectionCellEdit): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LInput: TJSHTMLInputElement;
  LText: TNyxText;
  LSelection: TNyxTextSelection;

  function CoordinatesCurrent: Boolean;
  begin
    Result := MatchesCellFocusProof(AStore, ACell) and
      (FCellFocusSerial = FColumnLayout.Serial) and
      (FCellFocusRow >= 0) and (FCellFocusRow < Length(FRows)) and
      (FRows[FCellFocusRow] <> nil) and
      (FCellFocusColumn >= 0) and
      (FCellFocusColumn < Length(FRows[FCellFocusRow].FInputs)) and
      (FCellFocusColumn < FColumnLayout.Count) and
      (FRows[FCellFocusRow].FInputs[FCellFocusColumn] = LInput) and
      (FRows[FCellFocusRow].FRef.ID = ACell.Item.ID) and
      (FRows[FCellFocusRow].FRef.Collection.Name = ACell.Item.Collection.Name);

    if Result then
    begin
      Result := FColumnLayout.ColumnAt(FCellFocusColumn).IsVisible and
        FColumnLayout.ColumnAt(FCellFocusColumn).Column.Identity.Same(ACell.Field);
    end;
  end;
begin
  Result := False;
  LKeepAlive := Self as INyxCollectionMount;
  LInput := FCellFocusInput;

  if (LInput = nil) or not CoordinatesCurrent or
    (document.activeElement <> LInput) or not FHost.contains(LInput) then
  begin
    Exit;
  end;

  if FEditsSuspended and (FEditSuspension = nil) then
  begin
    Exit;
  end;
  LText := LInput.value;

  if LInput._type = 'checkbox' then
  begin
    LText := 'false';

    if LInput.checked then
    begin
      LText := 'true';
    end;
  end;

  if not CoordinatesCurrent or (document.activeElement <> LInput) or
    (LText <> ACell.Text) then
  begin
    Exit;
  end;

  if ACell.Selection.Defined then
  begin
    LSelection := CaptureNyxBrowserSelection(LInput);

    if not CoordinatesCurrent or (document.activeElement <> LInput) or
      not LSelection.Defined or not LSelection.SameRange(ACell.Selection) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function NewElement(const ATag: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.createElement(ATag));
end;

function TBrowserMount.TreeRows: INyxTreeRows;
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

function TBrowserMount.ColumnLauncherPointer(AEvent: TJSMouseEvent): Boolean;
begin
  Result := True;
  { Keep the current cell's unfinished text in its real control until the
    manager has captured its own original launch. Click still opens the dialog;
    only the browser's premature button-focus default is suppressed. }
  AEvent.preventDefault;
end;

function TBrowserMount.NewColumnManagerCommand(AEvent: TEventListenerEvent;
  AAllowOpen: Boolean): INyxBrowserColumnCommand;
var
  LKeepAlive: INyxCollectionMount;
  LPress: TBrowserManagerPress;
  LLease: IInterface;
begin
  Result := nil;
  LKeepAlive := Self as INyxCollectionMount;

  if not FHasColumnLayout or (FView = nil) or (FHost = nil) or
    (FColumnLauncher = nil) or (AEvent.target <> FColumnLauncher) then
  begin
    Exit;
  end;
  LPress := TBrowserManagerPress.Create;
  LLease := LPress;
  LPress.View := FView;
  LPress.Manager := FColumnManager;
  LPress.Input := GetInputLifetime;

  if LPress.Input <> nil then
  begin
    LPress.Epoch := LPress.Input.Epoch;
  end;
  LPress.Host := FHost;
  LPress.Launcher := FColumnLauncher;
  LPress.Rendered := FRendered;
  LPress.Mutation := FCellMutationSerial;
  LPress.LayoutSerial := FColumnLayout.Serial;
  LPress.ResizeSerial := FColumnResizeSerial;
  LPress.ManagerSerial := FColumnManagerSerial;
  LPress.Active := FColumnResize;
  LPress.Pending := FColumnResizePending;
  LPress.ActiveLease := FColumnResizeLease;
  LPress.PendingLease := FColumnResizePendingLease;
  LPress.FitPress := FColumnSpacePress;
  LPress.FitPressLease := FColumnSpacePressLease;

  if LPress.Active <> nil then
  begin
    LPress.ActiveActivity := LPress.Active.Activity;
  end;

  if LPress.Pending <> nil then
  begin
    LPress.PendingActivity := LPress.Pending.Activity;
  end;

  if not FHost.contains(FColumnLauncher) or
    ((AEvent is TJSKeyboardEvent) and
    (document.activeElement <> LPress.Launcher)) then
  begin
    Exit;
  end;
  Result := TBrowserManagerCommand.Create(Self, LPress, LLease, AEvent, AAllowOpen);
end;

constructor TBrowserMount.Create(AHost: TJSHTMLElement;
  const AView: INyxCollectionView; const ALimits: TNyxTableTextWorkLimits;
  AInputPhase: TNyxCollectionInputPhase;
  const AInputGate: INyxCollectionInputGate);
var
  LHeader: TJSHTMLElement;
  LRow: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LCaption: TJSHTMLElement;
  LIndex: Integer;
begin
  ALimits.Validate;
  inherited Create(AView, AInputPhase, AInputGate);
  FTextLimits := ALimits;

  if AHost = nil then
  begin
    raise ENyxCollection.Create('Browser collection requires a host');
  end;

  if ((AView.Projection = cpList) and (LowerCase(AHost.tagName) <> 'ul')) or
    ((AView.Projection = cpTable) and (LowerCase(AHost.tagName) <> 'table')) or
    ((AView.Projection = cpTree) and (LowerCase(AHost.tagName) <> 'div')) then
  begin
    raise ENyxCollection.Create('Browser collection host differs from its projection');
  end;
  FHost := AHost;

  if FHasColumnLayout or FObservesTablePresentation then
  begin
    for LIndex := Low(PendingInputEvents) to High(PendingInputEvents) do
    begin
      FHost.addEventListener(PendingInputEvents[LIndex], @BlockPendingInput, True);
    end;
    { The pending-input capture guard was registered first. A staged synthetic
      composition cannot dirty this observer before accepted input admission. }
    FHost.addEventListener(ColumnCompositionStart, @CellComposition, True);
    FHost.addEventListener(ColumnCompositionEnd, @CellComposition, True);
  end;
  FHost.textContent := '';

  if AView.Projection = cpTable then
  begin
    FHost.setAttribute('role', 'grid');

    if FHasColumnLayout then
    begin
      { Separate borders give overlapping sticky cells their own opaque edge;
        the row window, data model and renderer's outer element stay the same. }
      FHost.style.setProperty('border-collapse', 'separate');
      FHost.style.setProperty('border-spacing', '0');
      LCaption := NewElement('caption');
      LCaption.style.setProperty('text-align', 'end');
      LCaption.style.setProperty('padding', '6px');
      FColumnLauncher := NewElement('button');
      FColumnLauncher.setAttribute('type', 'button');
      FColumnLauncher.className := 'nyx-button';
      FColumnLauncher.setAttribute('data-nyx-columns-button', '');
      FColumnLauncher.setAttribute('aria-haspopup', 'dialog');
      FColumnLauncher.setAttribute('aria-label', 'Manage table columns');
      FColumnLauncher.textContent := 'Columns';
      FColumnLauncher.style.setProperty('font', 'inherit');
      FColumnLauncher.style.setProperty('padding', '6px 12px');
      FColumnLauncher.style.setProperty('border-radius', '6px');
      FColumnLauncher.style.setProperty('cursor', 'pointer');
      FColumnLauncher.addEventListener('pointerdown', @ColumnLauncherPointer);
      LCaption.appendChild(FColumnLauncher);
      FHost.appendChild(LCaption);
    end;
    LHeader := NewElement('thead');
    LRow := NewElement('tr');
    LRow.setAttribute('role', 'row');
    LRow.setAttribute('aria-rowindex', '1');
    LHeader.appendChild(LRow);
    SetLength(FHeaderCells, AView.Spec.Count);
    SetLength(FHeaderLabels, AView.Spec.Count);
    SetLength(FHeaderSorts, AView.Spec.Count);
    SetLength(FHeaderFilters, AView.Spec.Count);

    if FHasColumnLayout then
    begin
      SetLength(FHeaderResizers, Length(FHeaderCells));
    end;
    for LIndex := 0 to AView.Spec.Count - 1 do
    begin
      LCell := NewElement('th');
      LCell.setAttribute('role', 'columnheader');
      LCell.setAttribute('aria-colindex', IntToStr(LIndex + 1));
      LCell.setAttribute('scope', 'col');
      FHeaderCells[LIndex] := LCell;
      FHeaderLabels[LIndex] := NewElement('span');
      FHeaderLabels[LIndex].textContent := AView.Spec.ColumnAt(LIndex).Title;
      LCell.appendChild(FHeaderLabels[LIndex]);
      FHeaderSorts[LIndex] := NewElement('button');
      FHeaderSorts[LIndex].className := 'nyx-button';
      FHeaderSorts[LIndex].setAttribute('type', 'button');
      FHeaderSorts[LIndex].style.setProperty('padding', '4px 6px');
      FHeaderSorts[LIndex].style.setProperty('white-space', 'normal');
      FHeaderSorts[LIndex].setAttribute('data-nyx-query-sort', IntToStr(LIndex));
      FHeaderSorts[LIndex].style.setProperty('display', 'none');
      LCell.appendChild(FHeaderSorts[LIndex]);
      FHeaderFilters[LIndex] := NewElement('button');
      FHeaderFilters[LIndex].className := 'nyx-button';
      FHeaderFilters[LIndex].textContent := 'Filter';
      FHeaderFilters[LIndex].setAttribute('type', 'button');
      FHeaderFilters[LIndex].style.setProperty('padding', '4px 6px');
      FHeaderFilters[LIndex].setAttribute('data-nyx-query-column', IntToStr(LIndex));
      FHeaderFilters[LIndex].setAttribute('aria-label', 'Filter ' + AView.Spec.ColumnAt(LIndex).Title);
      FHeaderFilters[LIndex].setAttribute('aria-haspopup', 'dialog');
      FHeaderFilters[LIndex].style.setProperty('display', 'none');
      LCell.appendChild(FHeaderFilters[LIndex]);

      if FHasColumnLayout then
      begin
        LCell.style.setProperty('padding-inline-end', '18px');
        FHeaderResizers[LIndex] := NewElement('div');
        FHeaderResizers[LIndex].setAttribute(ColumnResizeAttribute, '');
        FHeaderResizers[LIndex].setAttribute('role', 'separator');
        FHeaderResizers[LIndex].setAttribute('aria-orientation', 'vertical');
        FHeaderResizers[LIndex].setAttribute('title', ColumnResizeInstructions);
        FHeaderResizers[LIndex].tabIndex := 0;
        FHeaderResizers[LIndex].style.cssText :=
          'position:absolute;inset-inline-end:0;top:0;bottom:0;width:12px;' +
          'cursor:col-resize;touch-action:none;user-select:none;z-index:5;' +
          'border-inline-end:2px solid var(--nyx-border,#dddce8);';
        LCell.appendChild(FHeaderResizers[LIndex]);
      end;
      LRow.appendChild(LCell);
    end;
    FHost.appendChild(LHeader);
    FBody := NewElement('tbody');
    FHost.appendChild(FBody);
  end
  else
  begin
    FBody := FHost;
    FHost.setAttribute('role', 'listbox');

    if AView.Projection = cpTree then
    begin
      FHost.setAttribute('role', 'tree');
      FHost.style.setProperty('overflow', 'auto');
    end;
  end;
  FHost.setAttribute('aria-multiselectable', 'false');

  if AView.Spec.SelectionMode = nsmMultiple then
  begin
    FHost.setAttribute('aria-multiselectable', 'true');
  end;
  { Nyx's canonical keyboard listener was installed by the renderer first.
    Delegating row selection at this host lets its callbacks consume the key
    before the collection applies the default action. }

  if FHasColumnLayout then
  begin
    FHost.addEventListener('pointerdown', @ColumnResizeDown);
    FHost.addEventListener('pointermove', @ColumnResizeMove);
    FHost.addEventListener('pointerup', @ColumnResizeUp);
    FHost.addEventListener('pointercancel', @ColumnResizeCancel);
    FHost.addEventListener('lostpointercapture', @ColumnResizeCancel);
    FHost.addEventListener('keydown', @ColumnResizeKey);
    FHost.addEventListener('keyup', @ColumnButtonRelease);
    FHost.addEventListener('focusout', @ColumnButtonBlur, True);
    window.addEventListener('blur', @ColumnButtonBlur);
    FHost.addEventListener('focusout', @ColumnResizeFocus);
    FHost.addEventListener(ColumnFitClickEvent, @ColumnFitInput);
    FHost.addEventListener(ColumnFitDoubleEvent, @ColumnFitInput);
  end;
  FHost.addEventListener('keydown', @QueryKey);
  FHost.addEventListener('click', @QueryClick);
  FHost.addEventListener('keydown', @Key);
  FHost.addEventListener('contextmenu', @CellContext);
  FHost.addEventListener('pointercancel', @RangeGestureCancel);
  FHost.addEventListener('lostpointercapture', @RangeGestureCancel);
  FHost.addEventListener('keydown', @RangeGestureCancel);
  window.addEventListener('blur', @RangeGestureCancel);

  if AView.Projection in [cpList, cpTable, cpTree] then
  begin
    FGeometry := TNyxCollectionRowGeometry.Create(0, 32);
    document.addEventListener('scroll', @ScrollChanged, True);
    window.addEventListener('resize', @WindowChanged);
    FHost.addEventListener('focusin', @WindowChanged);

    if AView.Projection in [cpTable, cpTree] then
    begin
      FHost.addEventListener('focusin', @EditorFocusChanged);
      FHost.addEventListener('focusout', @EditorFocusChanged);
    end;

    if Assigned(window['ResizeObserver']) then
    begin
      FWindowObserver := TJSHTMLResizeObserver.new(@RowBoundsChanged);
      FWindowObserver.observe(FHost);
    end;
    QueueWindow;
  end;
end;

constructor TBrowserRow.Create(AOwner: TBrowserMount; const ARef: TNyxItemRef);
var
  LCell: TJSHTMLElement;
  LCaption: TJSHTMLElement;
  LColumn: TNyxCollectionColumn;
  LColumns: Integer;
  LIndex: Integer;
begin
  inherited Create;
  FOwner := AOwner;
  FRef := ARef;
  LColumns := 1;

  if FOwner.FView.Projection = cpTable then
  begin
    FElement := NewElement('tr');
    FElement.setAttribute('role', 'row');
    LColumns := FOwner.FView.Spec.Count;
  end
  else if FOwner.FView.Projection = cpTree then
  begin
    FElement := NewElement('details');
    FElement.setAttribute('role', 'treeitem');
    LCaption := NewElement('summary');
    LCaption.setAttribute('tabindex', '-1');
    FElement.addEventListener('toggle', @Toggle);
    FElement.appendChild(LCaption);
    FChildren := NewElement('div');
    FChildren.setAttribute('role', 'group');
    FChildren.style.setProperty('padding-left', '16px');
    FElement.appendChild(FChildren);
  end
  else
  begin
    FElement := NewElement('li');
    FElement.setAttribute('role', 'option');
  end;
  FElement.setAttribute('data-nyx-item', FRef.ID);
  FElement.setAttribute('tabindex', '0');
  FElement.onclick := Click;

  if FOwner.FView.Projection in [cpTable, cpTree] then
  begin
    FElement.ondblclick := DoubleClick;
  end;
  SetLength(FLabels, LColumns);
  SetLength(FDisplayTexts, LColumns);
  SetLength(FDisplayDiagnostics, LColumns);
  SetLength(FInputs, LColumns);
  SetLength(FCells, LColumns);
  for LIndex := 0 to LColumns - 1 do
  begin
    LCell := FElement;

    if FOwner.FView.Projection = cpTable then
    begin
      LCell := NewElement('td');
      LCell.setAttribute('role', 'gridcell');
      LCell.setAttribute('data-nyx-column', IntToStr(LIndex));
      LCell.setAttribute('aria-colindex', IntToStr(LIndex + 1));
      LCell.setAttribute('tabindex', '-1');
      FElement.appendChild(LCell);
    end
    else if FOwner.FView.Projection = cpTree then
    begin
      LCell := LCaption;
      LCell.setAttribute('data-nyx-column', '0');
    end;
    LColumn := FOwner.FView.Spec.ColumnAt(LIndex);
    FCells[LIndex] := LCell;

    if FOwner.FView.Projection in [cpTable, cpTree] then
    begin
      { A data cell exposes its actual value in navigation mode. An inactive
        textbox's value is not equivalent cell text for assistive technology. }
      FLabels[LIndex] := NewElement('span');
      FLabels[LIndex].classList.add('nyx-cell-value');
      LCell.appendChild(FLabels[LIndex]);
    end;

    if (LColumn.Mode = cmEditable) and (FOwner.FView.Projection <> cpList) then
    begin
      FInputs[LIndex] := TJSHTMLInputElement(document.createElement('input'));

      if FOwner.FView.Projection in [cpTable, cpTree] then
      begin
        FInputs[LIndex].classList.add('nyx-cell-editor');
        FInputs[LIndex].setAttribute('hidden', '');
      end;
      { The composite has one Tab entry in navigation mode. F2/Enter enters an
        owned editor; its Tab/Escape handling then preserves keyboard access to
        columns without putting every field in the surrounding page's Tab order. }
      FInputs[LIndex].setAttribute('tabindex', '-1');
      FInputs[LIndex].setAttribute('aria-label', LColumn.Title);
      FInputs[LIndex].setAttribute('data-nyx-column', IntToStr(LIndex));
      FInputs[LIndex].onchange := Change;
      FInputs[LIndex]._type := 'text';
      FInputs[LIndex].style.setProperty('min-width', '0');
      FInputs[LIndex].style.setProperty('width', '100%');
      FInputs[LIndex].style.setProperty('box-sizing', 'border-box');

      if LColumn.Kind = nskBoolean then
      begin
        FInputs[LIndex]._type := 'checkbox';
        FInputs[LIndex].style.removeProperty('width');
      end
      else if LColumn.Kind in [nskInteger, nskNumber] then
      begin
        FInputs[LIndex].setAttribute('inputmode', 'decimal');
      end;
      LCell.appendChild(FInputs[LIndex]);
    end
    else
    begin

      if FLabels[LIndex] = nil then
      begin
        FLabels[LIndex] := NewElement('span');
        LCell.appendChild(FLabels[LIndex]);
      end;
    end;
  end;

  if (FOwner.FView.Projection = cpTable) and
    (Length(FOwner.FColumnWidths) = LColumns) then
  begin
    for LIndex := 0 to LColumns - 1 do
    begin
      FCells[LIndex].style.setProperty('width', IntToStr(FOwner.FColumnWidths[LIndex]) + 'px');
      FCells[LIndex].style.setProperty('min-width', IntToStr(FOwner.FColumnWidths[LIndex]) + 'px');
      FCells[LIndex].style.setProperty('max-width', IntToStr(FOwner.FColumnWidths[LIndex]) + 'px');
      FCells[LIndex].style.setProperty('box-sizing', 'border-box');
    end;
  end;

  if FOwner.FWindowObserver <> nil then
  begin
    { Font loading, wrapped text and creator styling can change a visible row
      without changing its fixed-height scroll host. Observe owned row faces. }
    FOwner.FWindowObserver.observe(FElement);
  end;
end;

destructor TBrowserRow.Destroy;
var
  LIndex: Integer;
begin

  if (FOwner <> nil) and (FOwner.FWindowObserver <> nil) and (FElement <> nil) then
  begin
    FOwner.FWindowObserver.unobserve(FElement);
  end;

  if FElement <> nil then
  begin
    FElement.onclick := nil;
    FElement.ondblclick := nil;
    FElement.onkeydown := nil;
    FElement.removeEventListener('toggle', @Toggle);
  end;
  for LIndex := 0 to Length(FInputs) - 1 do
  begin

    if FInputs[LIndex] <> nil then
    begin
      FInputs[LIndex].onchange := nil;
    end;
  end;

  if (FElement <> nil) and (FElement.parentNode <> nil) then
  begin
    FElement.parentNode.removeChild(FElement);
  end;
  inherited Destroy;
end;

function TBrowserMount.MeasurePresentationText(const AText: TNyxText): Double;
begin
  Result := FTextMetric.measureText(AText).width;
end;

procedure TBrowserRow.SyncTextLayout(var AMeasuredCells: Integer);
const
  COutcomes: array[TNyxTableTextLayoutOutcome] of String =
    ('complete', 'clipped', 'ellipsized', 'unavailable');
  CAdmissions: array[TNyxTableTextAdmission] of String =
    ('ready', 'line-limit', 'text-limit', 'measurement-limit', 'cell-limit', 'host-metrics');
var
  LIndex: Integer;
  LLine: Integer;
  LText: TNyxText;
  LPolicy: TNyxColumnTextPolicy;
  LDensity: TNyxTablePresentationPolicy;
  LLayout: TNyxTableTextLayout;
  LCanvas: TJSHTMLCanvasElement;
  LContext: TJSCanvasRenderingContext2D;
  LStyle: TJSCSSStyleDeclaration;
  LWidth: Double;
  LFont: String;
  LMetricSupported: Boolean;
  LAlignment: TNyxColumnAlignmentPolicy;

  function Pixels(const AName: String): Double;
  begin
    Result := JS.parseFloat(LStyle.getPropertyValue(AName));

    if IsNan(Result) or IsInfinite(Result) then
    begin
      Result := 0;
    end;
  end;

begin

  if not FOwner.FHasTablePresentation then
  begin
    Exit;
  end;
  LDensity := FOwner.FTablePresentation.TablePolicy;
  LCanvas := TJSHTMLCanvasElement(document.createElement('canvas'));
  LContext := TJSCanvasRenderingContext2D(LCanvas.getContext('2d'));
  for LIndex := 0 to High(FLabels) do
  begin
    LAlignment := FOwner.FTablePresentation.AlignmentAt(LIndex);

    if LAlignment.HasHorizontal then
    begin
      LText := NyxColumnHorizontalAlignmentName(LAlignment.HorizontalAlignment);
      FCells[LIndex].style.setProperty('text-align', LText);
    end
    else
    begin
      FCells[LIndex].style.removeProperty('text-align');
    end;

    if (LIndex < Length(FInputs)) and (FInputs[LIndex] <> nil) then
    begin

      if LAlignment.HasHorizontal then
      begin
        FInputs[LIndex].style.setProperty('text-align',
          NyxColumnHorizontalAlignmentName(LAlignment.HorizontalAlignment));
      end
      else
      begin
        FInputs[LIndex].style.removeProperty('text-align');
      end;
    end;

    if LAlignment.HasVertical then
    begin
      FCells[LIndex].style.setProperty('vertical-align',
        NyxColumnVerticalAlignmentName(LAlignment.VerticalAlignment));
    end
    else
    begin
      FCells[LIndex].style.removeProperty('vertical-align');
    end;

    if LDensity.Defined then
    begin
      LText := IntToStr(NyxTableRowPadding(LDensity.RowDensity)) + 'px';
      FCells[LIndex].style.setProperty('padding-top', LText);
      FCells[LIndex].style.setProperty('padding-bottom', LText);
    end
    else
    begin
      FCells[LIndex].style.removeProperty('padding-top');
      FCells[LIndex].style.removeProperty('padding-bottom');
    end;
    LPolicy := FOwner.FTablePresentation.TextAt(LIndex);

    if not LPolicy.Defined then
    begin
      FLabels[LIndex].textContent := FDisplayTexts[LIndex];
      FLabels[LIndex].style.removeProperty('white-space');
      FLabels[LIndex].style.removeProperty('overflow');
      FLabels[LIndex].style.removeProperty('line-height');
      FLabels[LIndex].removeAttribute('data-nyx-text-layout');
      FLabels[LIndex].removeAttribute('data-nyx-text-layout-diagnostic');
      FLabels[LIndex].removeAttribute('data-nyx-text-layout-admission');
      FCells[LIndex].removeAttribute('aria-label');

      if FDisplayDiagnostics[LIndex] <> '' then
      begin
        FLabels[LIndex].setAttribute('title', FDisplayDiagnostics[LIndex]);
      end
      else
      begin
        FLabels[LIndex].removeAttribute('title');
      end;
      Continue;
    end;
    { Read actual typography after insertion too. A newly constructed row may
      not inherit its final face until it belongs to the ordinary table. }
    LStyle := window.getComputedStyle(FLabels[LIndex]);
    LMetricSupported := NyxBrowserColumnTextMetricSupported(LStyle);
    LFont := LStyle.getPropertyValue('font');

    { A hidden renderer candidate can still have unresolved inherited font
      properties. Do not invent a line height or poison later computed styles
      with an inline fallback. The next admitted DOM pass measures its actual
      font; CSS itself owns the line boxes and their readable body allocation. }
    LMetricSupported := LMetricSupported and (LFont <> '');
    LContext.font := LFont;
    LStyle := window.getComputedStyle(FCells[LIndex]);
    LWidth := 0;

    if LIndex < Length(FOwner.FColumnWidths) then
    begin
      LWidth := FOwner.FColumnWidths[LIndex];
    end;

    if FCells[LIndex].clientWidth > 0 then
    begin
      LWidth := FCells[LIndex].clientWidth;
    end;
    LWidth := Max(0, LWidth - Pixels('padding-left') - Pixels('padding-right'));

    if AMeasuredCells >= FOwner.FTextLimits.MaximumCells then
    begin
      LLayout := UnavailableNyxTableTextLayout(nttaCellLimit,
        'Visible cells exceed this host measurement pass budget');
    end
    else
    begin
      Inc(AMeasuredCells);

      if not LMetricSupported then
      begin
        LLayout := UnavailableNyxTableTextLayout(nttaHostMetrics,
          'This browser text style requires its own resolved layout metric');
      end
      else
      begin
        FOwner.FTextMetric := LContext;
        try
          LLayout := LayoutNyxTableText(FDisplayTexts[LIndex], LPolicy,
            LWidth, FOwner.FTextLimits, @FOwner.MeasurePresentationText);
        finally
          FOwner.FTextMetric := nil;
        end;
      end;
    end;
    LText := '';
    for LLine := 0 to LLayout.Count - 1 do
    begin

      if LLine > 0 then
      begin
        LText := LText + #10;
      end;
      LText := LText + LLayout.LineAt(LLine);
    end;
    FLabels[LIndex].textContent := LText;
    FLabels[LIndex].style.setProperty('white-space', 'pre');
    FLabels[LIndex].style.setProperty('overflow', 'hidden');
    FLabels[LIndex].setAttribute('data-nyx-text-layout', COutcomes[LLayout.Outcome]);
    FLabels[LIndex].setAttribute('data-nyx-text-layout-diagnostic', LLayout.Diagnostic);
    FLabels[LIndex].setAttribute('data-nyx-text-layout-admission', CAdmissions[LLayout.Admission]);

    if LLayout.Outcome = ntloUnavailable then
    begin
      FLabels[LIndex].setAttribute('title', LLayout.Diagnostic);
    end
    else

    if FDisplayDiagnostics[LIndex] <> '' then
    begin
      FLabels[LIndex].setAttribute('title', FDisplayDiagnostics[LIndex]);
    end
    else
    begin
      FLabels[LIndex].removeAttribute('title');
    end;
    FCells[LIndex].setAttribute('aria-label', FDisplayTexts[LIndex]);
  end;
end;

procedure TBrowserRow.Sync(AValuesChanged: Boolean);
const
  CPresentationNames: array[TNyxValuePresentationOutcome] of String =
    ('canonical', 'formatted', 'unavailable');
var
  LIndex: Integer;
  LText: TNyxText;
  LApplyValues: Boolean;
  LData: INyxCollectionSnapshot;
  LPrevious: Integer;
  LColumn: TNyxCollectionColumn;
  LEditing: Boolean;
  LPresentation: TNyxValuePresentation;

  procedure ApplyLabel;
  begin
    LPresentation := FOwner.FView.Spec.ColumnAt(LIndex)
      .Present(LData.Item(FRef), LData.Schema);
    FLabels[LIndex].textContent := LPresentation.Text;
    FDisplayTexts[LIndex] := LPresentation.Text;
    FDisplayDiagnostics[LIndex] := LPresentation.Diagnostic;
    { Status is observable independently of text. A refusal marker and tooltip
      never re-label canonical RAW as a successful formatted representation. }
    FLabels[LIndex].setAttribute('data-nyx-display-outcome',
      CPresentationNames[LPresentation.Outcome]);

    if LPresentation.Outcome = nvpoUnavailable then
    begin
      FLabels[LIndex].setAttribute('title', LPresentation.Diagnostic);
    end
    else
    begin
      FLabels[LIndex].removeAttribute('title');
    end;
  end;
begin
  { Compare accepted scalar values per cell, rather than treating every dataset
    publication as permission to replace every draft. Stable row identities keep
    their DOM editors across selection, reorder and unrelated item/field edits. }
  LData := FOwner.FView.Snapshot;
  LPrevious := -1;

  if FOwner.FRendered <> nil then
  begin
    LPrevious := FOwner.FRendered.IndexOf(FRef);
  end;
  for LIndex := 0 to Length(FLabels) - 1 do
  begin

    if FOwner.FView.Projection = cpTable then
    begin
      FCells[LIndex].setAttribute('tabindex', '-1');
      FCells[LIndex].setAttribute('aria-readonly', LowerCase(BoolToStr(
        FOwner.FReadOnly or (FOwner.FView.Spec.ColumnAt(LIndex).Mode <> cmEditable), True)));
      FOwner.ApplyColumnFace(FCells[LIndex], LIndex, False,
        FOwner.FView.Selection.Contains(FRef));
    end;
    LApplyValues := not FInitialized or (AValuesChanged and (LPrevious < 0));

    { Hiding withdraws editing/focus authority, not the user's unfinished text.
      Keep its existing input dormant while the accepted scalar/schema still
      matches. The ordinary accepted-value comparison below invalidates stale
      text; revealing the field never commits or reactivates it implicitly. }

    if AValuesChanged and not LApplyValues and (FOwner.FRendered.Revision <> LData.Revision) then
    begin
      LColumn := FOwner.FView.Spec.ColumnAt(LIndex);
      LApplyValues := not LColumn.Read(LData.Item(FRef)).SameValue(
        LColumn.Read(FOwner.FRendered.ItemAt(LPrevious)));
    end;

    if FOwner.FNormalizeValues and (FOwner.FNormalizeItem.ID = FRef.ID) and
      (FOwner.FNormalizeColumn = LIndex) then
    begin
      LApplyValues := True;
    end;

    if FInputs[LIndex] <> nil then
    begin
      { Read-only text remains inspectable, selectable and copyable. Checkboxes
        have no native readOnly behavior, so their editing authority is disabled
        explicitly while row selection stays available. }
      FInputs[LIndex].disabled := not FOwner.FEnabled or
        (FOwner.FReadOnly and (FInputs[LIndex]._type = 'checkbox'));
      FInputs[LIndex].readOnly := FOwner.FReadOnly;

      if FOwner.FView.Projection in [cpTable, cpTree] then
      begin
        LEditing := FOwner.CellEditing(FRef, LIndex);

        if LEditing then
        begin
          FInputs[LIndex].removeAttribute('hidden');
          FLabels[LIndex].setAttribute('hidden', '');
        end
        else
        begin
          FInputs[LIndex].setAttribute('hidden', '');
          FLabels[LIndex].removeAttribute('hidden');
        end;
      end;

      if LApplyValues then
      begin
        LText := FOwner.FView.CellText(FRef, LIndex);

        if FInputs[LIndex]._type = 'checkbox' then
        begin
          FInputs[LIndex].checked := LText = 'true';
        end
        else if FInputs[LIndex].value <> LText then
        begin
          FInputs[LIndex].value := LText;
        end;

        if FLabels[LIndex] <> nil then
        begin
          ApplyLabel;
        end;
      end;
    end
    else if LApplyValues then
    begin
      { Unchanged label nodes are retained just like editors. Reassigning
        textContent also destroys their DOM text node and needlessly repaints. }
      ApplyLabel;
    end;
  end;

  if (FOwner.FView.Projection = cpTable) and FOwner.FHasColumnLayout then
  begin
    FOwner.OrderColumnChildren(FElement, FCells);
  end;
  FElement.setAttribute('aria-selected', 'false');
  FElement.classList.remove('nyx-selected');

  if FOwner.FView.Selection.Contains(FRef) then
  begin
    FElement.setAttribute('aria-selected', 'true');
    FElement.classList.add('nyx-selected');
  end;
  FElement.setAttribute('tabindex', '-1');
  FInitialized := True;

  if FOwner.FView.Projection = cpTree then
  begin
    Toggle(nil);
  end;
  SyncCellRange;
end;

function TBrowserRow.HasDraft: Boolean;
var
  LIndex: Integer;
  LText: TNyxText;
begin
  Result := False;
  for LIndex := 0 to Length(FInputs) - 1 do
  begin

    if FInputs[LIndex] <> nil then
    begin
      LText := FOwner.FView.CellText(FRef, LIndex);

      if FInputs[LIndex]._type = 'checkbox' then
      begin
        Result := FInputs[LIndex].checked <> (LText = 'true');
      end
      else
      begin
        Result := FInputs[LIndex].value <> LText;
      end;

      if Result then
      begin
        Exit;
      end;
    end;
  end;
end;

function TBrowserMount.ScrollChanged(AEvent: TEventListenerEvent): Boolean;
var
  LKeepAlive: INyxCollectionMount;
begin
  Result := True;

  if not GetConnected or (FHost = nil) or ((AEvent.target is TJSNode) and
    not TJSNode(AEvent.target).contains(FHost)) then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  { Descendant editor scrolling and unrelated panes do not move this viewport. }

  if FRevealItem.Defined and (AEvent.target = FHost) and
    (Abs(TScrollableElement(FHost).scrollTopPixels - FRevealScrollTop) > 0.5) then
  begin
    FRevealItem := Default(TNyxItemRef);
  end;
  QueueWindow;
  { Geometry revokes Fit freshness, not an admitted button's later key phases. }
  CancelColumnResizes(False);
end;

function TBrowserMount.WindowChanged(AEvent: TEventListenerEvent): Boolean;
var
  LKeepAlive: INyxCollectionMount;
begin
  Result := True;

  if not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  QueueWindow;
  { Focus navigation ends an active draft, but Enter already released its
    logical request. Tab must not discard that pending next-frame publication. }

  if (AEvent._type <> 'focusin') or (FColumnResize <> nil) then
  begin
    CancelColumnResizes(False);
  end;
end;

procedure TBrowserMount.RowBoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
  AObserver: TJSHTMLResizeObserver);
var
  LKeepAlive: INyxCollectionMount;
  LActive: TColumnLayoutProposal;
  LPending: TColumnLayoutProposal;
  LActiveLease: IInterface;
  LPendingLease: IInterface;
  LChanged: Boolean;

  function CurrentGeometry(AProposal: TColumnLayoutProposal): Boolean;
  begin

    if AProposal.Action = claFit then
    begin
      { Fit has no drag-scale witness. Its original context and detached
        measurement/final fence validate freshness when the queue prepares it. }
      Result := ColumnResizeContext(AProposal);
    end
    else
    begin
      Result := ColumnResizeGeometry(AProposal);
    end;
  end;

begin

  if not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  QueueWindow;
  LActive := FColumnResize;
  LPending := FColumnResizePending;
  LActiveLease := FColumnResizeLease;
  LPendingLease := FColumnResizePendingLease;
  { ResizeObserver delivery may describe the preceding accepted width even
    after a new key/drag has captured the already-current geometry. Cancel an
    actual mismatch, not the mere arrival of that delayed notification. Keep
    the original owners through DOM reads and refuse to clear a newer slot. }
  LChanged := ((LActive <> nil) and not CurrentGeometry(LActive)) or
    ((LPending <> nil) and not CurrentGeometry(LPending));

  if LChanged and (FColumnResize = LActive) and (FColumnResizePending = LPending) then
  begin
    CancelColumnResizes(False);
  end;
end;

procedure TBrowserMount.QueueWindow;
begin

  if (FHost = nil) or FWindowQueued or FEditsSuspended then
  begin
    Exit;
  end;
  FWindowQueued := True;
  try
    FWindowFrame := window.requestAnimationFrame(@WindowFrame);
  except
    FWindowQueued := False;
    FWindowFrame := 0;
    raise;
  end;
end;

procedure TBrowserMount.WindowFrame(ATime: Double);
var
  LKeepAlive: INyxCollectionMount;
  LChange: TCompositionChange;
  LFocusPending: Boolean;
begin
  LKeepAlive := Self as INyxCollectionMount;
  FWindowQueued := False;

  if InputLifetimeCurrent and FCompositionEnding then
  begin
    { Clear the owned queue markers before any model call can reenter. A new
      START then owns its own pending state and cannot be erased by this frame. }
    FCompositionEnding := False;
    FComposingInput := nil;
    LChange := FCompositionChange;
    FCompositionChange := Default(TCompositionChange);
    LFocusPending := FCompositionFocusPending;
    FCompositionFocusPending := False;

    if LFocusPending then
    begin
      ReconcileCompositionFocus;
    end;

    if LChange.Pending then
    begin
      DrainCompositionChange(LChange);
    end;
  end;

  if GetConnected then
  begin
    { A released command is drained only after the physical dispatch tail.
      Composition above can revoke it; a recursive frame cannot publish twice. }
    PublishColumnResize;
  end;

  if (FColumnResize <> nil) and FColumnResize.Retired then
  begin
    CancelColumnResizes;
  end;

  if GetConnected then
  begin
    { Refresh retains its managed call frame through observer/focus callbacks.
      The following cleanup reads only retained proposal flags, never a borrowed
      row/header from before a callback; capture release is its final callout. }
    Refresh;

    if GetConnected and (FColumnResize <> nil) and FColumnResize.Retired then
    begin
      CancelColumnResizes;
    end;
  end;
end;

procedure TBrowserMount.ReconcileCompositionFocus;
var
  LActive: TJSHTMLElement;
  LRow: TBrowserRow;
  LWork, LColumn: Integer;
begin
  LActive := TJSHTMLElement(document.activeElement);
  for LWork := 0 to High(FResidentRows) do
  begin
    LRow := FRows[FResidentRows[LWork]];

    if LRow <> nil then
    begin
      for LColumn := 0 to High(LRow.FInputs) do
      begin

        if (LRow.FInputs[LColumn] <> nil) and (LRow.FInputs[LColumn] = LActive) then
        begin
          FEditingItem := LRow.FRef;
          FEditingColumn := LColumn;
          FFocusedColumn := LColumn;
          Exit;
        end;
      end;
    end;
  end;
  FEditingItem := Default(TNyxItemRef);
end;

procedure TBrowserMount.DrainCompositionChange(const AChange: TCompositionChange);
var
  LView: INyxCollectionView;
  LData, LCurrent, LRendered: INyxCollectionSnapshot;
  LLayout: TNyxColumnLayoutObservation;
  LSpec: TNyxCollectionViewSpec;
  LPhysical, LColumn, LRow, LLayoutSerial, LMutation: Integer;
  LText: TNyxText;
  LAccepted: Boolean;
begin

  if not GetInputReady or FEditsSuspended or not FEnabled or FReadOnly or
    (FCellMutationSerial = High(Integer)) or not AChange.Pending or
    (AChange.Input = nil) or (FRendered = nil) then
  begin
    Exit;
  end;
  LMutation := FCellMutationSerial;
  { Resolve the borrowed input against its retained physical identity first.
    The later CURRENT projection/accepted scalar decides whether its delayed
    change still has authority. No row position is reused after an outward call. }
  LRendered := FRendered;
  LLayoutSerial := FColumnLayout.Serial;
  LRow := LRendered.IndexOf(AChange.Item);
  LPhysical := FColumnLayout.Spec.ColumnIndex(AChange.Field);

  if (LRow < 0) or (LRow >= Length(FRows)) or (FRows[LRow] = nil) or
    (LPhysical < 0) or (LPhysical >= Length(FRows[LRow].FInputs)) or
    (FRows[LRow].FInputs[LPhysical] <> AChange.Input) then
  begin
    Exit;
  end;
  LView := FView;
  LData := LView.Snapshot;
  LSpec := LView.Spec;
  LLayout := TNyxColumnLayoutObservation.Read(NyxCollectionColumns(LView).CaptureColumns);
  LColumn := LSpec.ColumnIndex(AChange.Field);
  LAccepted := (LColumn >= 0) and LData.Has(AChange.Item) and
    (LSpec.ColumnAt(LColumn).Mode = cmEditable) and
    LLayout.ColumnAt(LColumn).IsVisible;

  if LAccepted then
  begin
    LAccepted := LSpec.ColumnAt(LColumn).Read(LData.Item(AChange.Item))
      .SameValue(AChange.Accepted);
  end;
  LCurrent := LView.Snapshot;

  if not GetInputReady or (FView <> LView) or FEditsSuspended or
    (FCellMutationSerial <> LMutation) or
    (FRendered <> LRendered) or (FColumnLayout.Serial <> LLayoutSerial) or
    (LCurrent <> LData) or not FEnabled or FReadOnly then
  begin
    Exit;
  end;

  if FComposingInput <> nil then
  begin
    { A getter may start another composition on the same retained editor.
      Preserve the genuine change for its eventual END, without replacing a
      newer change that already owns the pending slot. }

    if (FComposingInput = AChange.Input) and not FCompositionChange.Pending then
    begin
      FCompositionChange := AChange;
    end;
    Exit;
  end;

  if not LAccepted or not FHost.contains(AChange.Input) then
  begin
    Exit;
  end;
  LText := AChange.Input.value;

  if AChange.Input._type = 'checkbox' then
  begin
    LText := 'false';

    if AChange.Input.checked then
    begin
      LText := 'true';
    end;
  end;
  { Final outward call: accepted editing may rebuild rows or retire this mount.
    The frame retains Self and uses no borrowed row/input after admission. }
  EditCell(AChange.Item, LColumn, LText);
end;

procedure TBrowserMount.ObserveWindowAncestors;
var
  LNext: array of TJSHTMLElement;
  LAncestor: TJSHTMLElement;
  LIndex: Integer;
  LNextIndex: Integer;
  LFound: Boolean;
begin

  if FWindowObserver = nil then
  begin
    Exit;
  end;
  LAncestor := FHost;
  while LAncestor <> nil do
  begin
    SetLength(LNext, Length(LNext) + 1);
    LNext[High(LNext)] := LAncestor;
    FWindowObserver.observe(LAncestor);
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;
  for LIndex := 0 to Length(FWindowAncestors) - 1 do
  begin
    LFound := False;
    for LNextIndex := 0 to Length(LNext) - 1 do
    begin
      LFound := LFound or (LNext[LNextIndex] = FWindowAncestors[LIndex]);
    end;

    if not LFound then
    begin
      { A retained view can move/park without unmounting. Its old scroll/layout
        owners must not remain observer targets or retain obsolete DOM roots. }
      FWindowObserver.unobserve(FWindowAncestors[LIndex]);
    end;
  end;
  FWindowAncestors := LNext;
end;

procedure TBrowserMount.RowViewport(out AOffset, AExtent: Double);
var
  LClipTop: Double;
  LClipBottom: Double;
  LBodyTop: Double;
  LAncestor: TJSHTMLElement;
  LRect: TJSDOMRect;
  LStyle: TJSCSSStyleDeclaration;
  LOverflow: String;
  LScale: Double;
  LAncestorScale: Double;
  LHeaderCell: TJSHTMLElement;
  LHeaderHeight: String;
  LPadding: Double;
begin
  ObserveWindowAncestors;
  AOffset := 0;
  AExtent := 320;

  if not document.documentElement.contains(FHost) or
    (Length(FHost.getClientRects) = 0) then
  begin
    { A detached candidate realizes a small prefetch window. Its first layout
      observation recomputes actual host/ancestor/window intersections. }
    Exit;
  end;
  LClipTop := 0;
  LClipBottom := window.innerHeight;
  LScale := 1;

  if FHost.offsetHeight > 0 then
  begin
    LScale := FHost.getBoundingClientRect.height / FHost.offsetHeight;
  end;

  if IsNan(LScale) or IsInfinite(LScale) or (LScale <= 0) then
  begin
    LScale := 1;
  end;
  LBodyTop := FBody.getBoundingClientRect.top;

  if (FView.Projection = cpTable) and (FBody.children.length = 0) then
  begin
    { An empty tbody can report the zero rectangle on an already attached host.
      The actual header end establishes its prospective first data-row origin. }
    LBodyTop := TJSHTMLElement(FHost.querySelector('thead')).getBoundingClientRect.bottom;
  end;

  if FView.Projection in [cpList, cpTree] then
  begin
    { Lists use their own scrolling content rather than a separate tbody.
      Keep the origin in that logical plane, including an empty attached host. }
    LPadding := parseFloat(window.getComputedStyle(FHost).getPropertyValue('padding-top'));

    if IsNan(LPadding) or IsInfinite(LPadding) then
    begin
      LPadding := 0;
    end;
    LBodyTop := FHost.getBoundingClientRect.top +
      (FHost.clientTop + LPadding - TScrollableElement(FHost).scrollTopPixels) * LScale;
  end;
  LAncestor := FHost;
  while LAncestor <> nil do
  begin
    LStyle := window.getComputedStyle(LAncestor);
    LOverflow := LStyle.getPropertyValue('overflow-y');

    if (LOverflow = 'auto') or (LOverflow = 'scroll') or
      (LOverflow = 'hidden') or (LOverflow = 'clip') then
    begin
      LRect := LAncestor.getBoundingClientRect;
      LAncestorScale := 1;

      if LAncestor.offsetHeight > 0 then
      begin
        LAncestorScale := LRect.height / LAncestor.offsetHeight;
      end;
      LClipTop := Max(LClipTop, LRect.top + LAncestor.clientTop * LAncestorScale);
      LClipBottom := Min(LClipBottom,
        LRect.top + (LAncestor.clientTop + LAncestor.clientHeight) * LAncestorScale);
    end;
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;
  LHeaderCell := TJSHTMLElement(FHost.querySelector('thead th'));

  if (LHeaderCell <> nil) and
    (window.getComputedStyle(LHeaderCell).getPropertyValue('position') = 'sticky') then
  begin
    { Sticky headings occlude part of the scroll plane. Exclude that measured
      area from realization/paging and use logical height for nearest scrolling,
      including scaled Studio previews. }
    LClipTop := Max(LClipTop, Min(LClipBottom, LHeaderCell.getBoundingClientRect.bottom));
    LHeaderHeight := IntToStr(Max(1, Ceil(LHeaderCell.offsetHeight))) + 'px';

    if FHost.style.getPropertyValue('--nyx-grid-header-height') <> LHeaderHeight then
    begin
      FHost.style.setProperty('--nyx-grid-header-height', LHeaderHeight);
    end;
  end;
  { Rectangles are viewport coordinates; row geometry is the logical CSS plane.
    Compensate axis-aligned scaling rather than shrinking authored text. }
  AOffset := Max(0, (LClipTop - LBodyTop) / LScale);
  AExtent := Max(0, (LClipBottom - Max(LClipTop, LBodyTop)) / LScale);

  if AExtent = 0 then
  begin
    { A wholly clipped table retains a bounded prefetch, never a full source. }
    AExtent := 320;
  end;
end;

procedure TBrowserMount.EnsureRow(AIndex: Integer);
begin

  if not InputLifetimeCurrent or (AIndex < 0) or (AIndex >= FView.Snapshot.Count) then
  begin
    Exit;
  end;

  if (AIndex >= Length(FRows)) or (FRows[AIndex] = nil) or
    not FHost.contains(FRows[AIndex].FElement) then
  begin
    FForcedItem := FView.Snapshot.ItemAt(AIndex).Ref;
    Refresh;
  end;
end;

procedure TBrowserMount.FocusListOption(AOption: TJSHTMLElement);
var
  LOptions: TFocusOptions;
  LFace: TJSHTMLElement;
begin

  if not GetConnected or (AOption = nil) or not FHost.contains(AOption) then
  begin
    Exit;
  end;
  FRevealItem := NyxItem(FView.Spec.Key, AOption.getAttribute('data-nyx-item'));
  LFace := AOption;

  if FView.Projection = cpTree then
  begin
    { A branch box includes its descendant group. Its own summary, rather than
      the complete subtree, is the keyboard destination to reveal. }
    LFace := TJSHTMLElement(AOption.querySelector('summary'));
  end;
  RevealListOption(LFace);
  LOptions := TFocusOptions.new;
  LOptions.preventScroll := True;
  TFocusable(AOption).focus(LOptions);
end;

procedure TBrowserMount.RevealListOption(AOption: TJSHTMLElement);
var
  LHostBounds: TJSDOMRect;
  LOptionBounds: TJSDOMRect;
  LScale: Double;
  LTop: Double;
  LBottom: Double;
  LDelta: Double;
begin

  if not GetConnected or (AOption = nil) or not FHost.contains(AOption) then
  begin
    Exit;
  end;
  LHostBounds := FHost.getBoundingClientRect;
  LOptionBounds := AOption.getBoundingClientRect;
  LScale := 1;

  if FHost.offsetHeight > 0 then
  begin
    LScale := LHostBounds.height / FHost.offsetHeight;
  end;

  if IsNan(LScale) or IsInfinite(LScale) or (LScale <= 0) then
  begin
    LScale := 1;
  end;
  LTop := LHostBounds.top + FHost.clientTop * LScale;
  LBottom := LTop + FHost.clientHeight * LScale;
  LDelta := 0;

  if LOptionBounds.top < LTop then
  begin
    LDelta := LOptionBounds.top - LTop;
  end
  else if LOptionBounds.bottom > LBottom then
  begin
    LDelta := LOptionBounds.bottom - LBottom;
  end;
  TScrollableElement(FHost).scrollTopPixels :=
    TScrollableElement(FHost).scrollTopPixels + LDelta / LScale;
  FRevealScrollTop := TScrollableElement(FHost).scrollTopPixels;
end;

function TBrowserMount.ColumnClipWidth: Integer;
var
  LAncestor: TJSHTMLElement;
  LRect: TJSDOMRect;
  LStyle: TJSCSSStyleDeclaration;
  LOverflow: String;
  LLeft, LRight, LScale, LAncestorScale: Double;
begin
  LLeft := 0;
  LRight := window.innerWidth;
  LScale := 1;

  if FHost.offsetWidth > 0 then
  begin
    LScale := FHost.getBoundingClientRect.width / FHost.offsetWidth;
  end;

  if IsNan(LScale) or IsInfinite(LScale) or (LScale <= 0) then
  begin
    LScale := 1;
  end;
  LAncestor := TJSHTMLElement(FHost.parentElement);
  while LAncestor <> nil do
  begin
    LStyle := window.getComputedStyle(LAncestor);
    LOverflow := LStyle.getPropertyValue('overflow-x');

    if (LOverflow = 'auto') or (LOverflow = 'scroll') or
      (LOverflow = 'hidden') or (LOverflow = 'clip') then
    begin
      LRect := LAncestor.getBoundingClientRect;
      LAncestorScale := 1;

      if LAncestor.offsetWidth > 0 then
      begin
        LAncestorScale := LRect.width / LAncestor.offsetWidth;
      end;

      if IsNan(LAncestorScale) or IsInfinite(LAncestorScale) or (LAncestorScale <= 0) then
      begin
        LAncestorScale := 1;
      end;
      LLeft := Max(LLeft, LRect.left + LAncestor.clientLeft * LAncestorScale);
      LRight := Min(LRight,
        LRect.left + (LAncestor.clientLeft + LAncestor.clientWidth) * LAncestorScale);
    end;
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;
  Result := Min(Max(0, FHost.clientWidth), Max(0, Floor((LRight - LLeft) / LScale)));
end;

procedure TBrowserMount.OrderColumnChildren(AParent: TJSHTMLElement;
  const ACells: array of TJSHTMLElement);
var
  LDesired: array of Integer;
  LIndex, LCanonical, LCount, LActive: Integer;
  LCursor: TJSNode;
begin

  if not FHasColumnLayout then
  begin
    Exit;
  end;
  SetLength(LDesired, Length(ACells));
  LCount := 0;
  for LIndex := 0 to DisplayColumnCount - 1 do
  begin
    LDesired[LCount] := DisplayColumn(LIndex);
    Inc(LCount);
  end;
  for LIndex := 0 to FColumnLayout.Count - 1 do
  begin
    LCanonical := FColumnLayout.OrderedAt(LIndex);

    if not ColumnVisible(LCanonical) then
    begin
      LDesired[LCount] := LCanonical;
      Inc(LCount);
    end;
  end;
  LActive := -1;
  for LIndex := 0 to High(LDesired) do
  begin

    if ACells[LDesired[LIndex]].contains(document.activeElement) then
    begin
      LActive := LIndex;
      Break;
    end;
  end;

  if LActive >= 0 then
  begin
    { Reorder the siblings AROUND the active cell. Moving that ancestor would
      blur a live editor on older browsers, even if its input object survives.
      Its text, focus and caret never leave their current DOM incarnation here. }
    for LIndex := 0 to LActive - 1 do
    begin
      AParent.insertBefore(ACells[LDesired[LIndex]], ACells[LDesired[LActive]]);
    end;
    for LIndex := LActive + 1 to High(LDesired) do
    begin
      AParent.appendChild(ACells[LDesired[LIndex]]);
    end;
  end
  else
  begin
    LCursor := AParent.firstChild;
    for LIndex := 0 to High(LDesired) do
    begin

      if ACells[LDesired[LIndex]] <> LCursor then
      begin
        AParent.insertBefore(ACells[LDesired[LIndex]], LCursor);
      end;
      LCursor := ACells[LDesired[LIndex]].nextSibling;
    end;
  end;
end;

procedure TBrowserMount.ApplyColumnFace(ACell: TJSHTMLElement; AColumn: Integer;
  AHeader, ASelected: Boolean);
var
  LPin: TNyxColumnPin;
  LIndex, LCanonical, LOffset, LPosition: Integer;
begin

  if not FHasColumnLayout then
  begin
    Exit;
  end;
  ACell.style.removeProperty('inset-inline-start');
  ACell.style.removeProperty('inset-inline-end');
  ACell.style.removeProperty('position');
  ACell.style.removeProperty('z-index');
  ACell.style.removeProperty('background-color');

  if not ColumnVisible(AColumn) then
  begin
    ACell.style.setProperty('display', 'none');
    ACell.setAttribute('aria-hidden', 'true');
    ACell.removeAttribute('aria-colindex');
    Exit;
  end;
  LPosition := ColumnDisplayIndex(AColumn);
  ACell.style.removeProperty('display');
  ACell.removeAttribute('aria-hidden');
  ACell.setAttribute('aria-colindex', IntToStr(LPosition + 1));
  LPin := FColumnPresentation.EffectivePin(AColumn);

  if LPin = ncpNone then
  begin

    if AHeader then
    begin
      ACell.style.setProperty('position', 'relative');
    end;
    Exit;
  end;
  LOffset := 0;
  for LIndex := 0 to DisplayColumnCount - 1 do
  begin
    LCanonical := DisplayColumn(LIndex);

    if (FColumnPresentation.EffectivePin(LCanonical) = LPin) and
      (((LPin = ncpStart) and (LIndex < LPosition)) or
        ((LPin = ncpEnd) and (LIndex > LPosition))) then
    begin
      Inc(LOffset, FColumnPresentation.Width(LCanonical));
    end;
  end;
  ACell.style.setProperty('position', 'sticky');
  ACell.style.setProperty('z-index', '2');
  ACell.style.setProperty('background-color', 'var(--nyx-surface,#fff)');

  if ASelected then
  begin
    ACell.style.setProperty('background-color', 'var(--nyx-accent-soft,var(--nyx-surface,#fff))');
  end;

  if AHeader then
  begin
    ACell.style.setProperty('z-index', '4');
  end;

  if LPin = ncpStart then
  begin
    ACell.style.setProperty('inset-inline-start', IntToStr(LOffset) + 'px');
  end
  else
  begin
    ACell.style.setProperty('inset-inline-end', IntToStr(LOffset) + 'px');
  end;
end;

procedure TBrowserMount.RefreshColumnResizers;
var
  LColumn: Integer;
  LEntry: TNyxColumnLayoutEntry;
  LSizing: TNyxColumnSizing;
  LHandle: TJSHTMLElement;
  LAllowed, LResize, LFit: Boolean;

  function CacheMatches(AProposal: TColumnLayoutProposal): Boolean;
  begin
    { Installation already committed before its notification updates geometry.
      Keep its ownership through that refresh; it is not an uninstalled draft. }

    if AProposal.Publishing and AProposal.Installed then
    begin
      Exit(True);
    end;
    Result := FEnabled and not FEditsSuspended and (FComposingInput = nil) and
      AProposal.Input.Current and (AProposal.Input.Epoch = AProposal.Epoch) and
      (AProposal.Column >= 0) and
      (AProposal.Column < Length(FColumnWidths)) and
      (AProposal.Column < FColumnLayout.Count) and
      (AProposal.LayoutSerial = FColumnLayout.Serial) and
      (AProposal.Font = FColumnFont) and
      (AProposal.Available = FColumnAvailable);

    if Result then
    begin
      Result := (AProposal.InitialWidth = FColumnWidths[AProposal.Column]) and
        FColumnLayout.ColumnAt(AProposal.Column).IsVisible and
        (((AProposal.Action = claResize) and
          (nclfResize in FColumnLayout.ColumnAt(AProposal.Column).Features)) or
        ((AProposal.Action = claFit) and
          (nclfAutoSize in FColumnLayout.ColumnAt(AProposal.Column).Features))) and
        FColumnLayout.ColumnAt(AProposal.Column).Column.Identity.Same(AProposal.Field);
    end;
  end;

  procedure RemoveGuide(AProposal: TColumnLayoutProposal);
  var
    LGuide: TJSHTMLElement;
  begin

    if AProposal <> nil then
    begin
      LGuide := AProposal.Guide;
      AProposal.Guide := nil;

      if LGuide <> nil then
      begin
        { This owned span has no input/focus/capture or custom element hooks.
          Drop its obsolete visual during geometry sync; physical capture
          release stays outside the protected refresh/default-edit frame. }
        LGuide.remove;
      end;
    end;
  end;

begin

  if not FHasColumnLayout then
  begin
    Exit;
  end;
  { Refresh deliberately shields new physical input. Inspect only geometry here,
    not temporary Ready=False, so an unchanged refresh preserves a live draft. }

  if ((FColumnResize <> nil) and not FColumnResize.Retired and
      not CacheMatches(FColumnResize)) or
    ((FColumnResizePending <> nil) and not FColumnResizePending.Retired and
      not CacheMatches(FColumnResizePending)) then
  begin
    InvalidateColumnResizes;
    RemoveGuide(FColumnResize);
    RemoveGuide(FColumnResizePending);
    QueueWindow;
  end;
  for LColumn := 0 to High(FHeaderResizers) do
  begin
    LHandle := FHeaderResizers[LColumn];
    LEntry := FColumnLayout.ColumnAt(LColumn);
    LSizing := LEntry.Column.Sizing;

    if not LSizing.Defined then
    begin
      LSizing := NyxColumnSizing;
    end;
    LResize := nclfResize in LEntry.Features;
    LFit := nclfAutoSize in LEntry.Features;
    LAllowed := FEnabled and LEntry.IsVisible and (LResize or LFit);

    if LResize then
    begin
      LHandle.setAttribute('role', 'separator');
      LHandle.setAttribute('aria-orientation', 'vertical');
      LHandle.setAttribute('aria-label', 'Resize ' + LEntry.Column.Title);

      if LFit then
      begin
        LHandle.setAttribute('title', ColumnResizeFitInstructions);
      end
      else
      begin
        LHandle.setAttribute('title', ColumnResizeInstructions);
      end;
      LHandle.style.setProperty('cursor', 'col-resize');
      LHandle.setAttribute('aria-valuemin', IntToStr(LSizing.Bounds.MinimumValue));
      LHandle.setAttribute('aria-valuemax', IntToStr(LSizing.Bounds.MaximumValue));
    end
    else
    begin
      LHandle.setAttribute('role', 'button');
      LHandle.setAttribute('aria-label', 'Fit ' + LEntry.Column.Title);
      LHandle.setAttribute('title', ColumnFitInstructions);
      LHandle.style.setProperty('cursor', 'pointer');
      LHandle.removeAttribute('aria-orientation');
      LHandle.removeAttribute('aria-valuemin');
      LHandle.removeAttribute('aria-valuemax');
      LHandle.removeAttribute('aria-valuenow');
    end;

    if LResize and (LColumn < Length(FColumnWidths)) then
    begin
      LHandle.setAttribute('aria-valuenow', IntToStr(FColumnWidths[LColumn]));
    end;

    if LResize and (FColumnResize <> nil) and not FColumnResize.Retired and
      (FColumnResize.Handle = LHandle) then
    begin
      LHandle.setAttribute('aria-valuenow', IntToStr(FColumnResize.Width));
    end;

    if LAllowed then
    begin
      LHandle.style.removeProperty('display');
      LHandle.removeAttribute('aria-hidden');
      LHandle.setAttribute('aria-disabled', 'false');
      LHandle.tabIndex := 0;
    end
    else
    begin
      LHandle.style.setProperty('display', 'none');
      LHandle.setAttribute('aria-hidden', 'true');
      LHandle.setAttribute('aria-disabled', 'true');
      LHandle.tabIndex := -1;
    end;
  end;
end;

procedure TBrowserMount.SizeTableColumns;
var
  LCanvas: TJSHTMLCanvasElement;
  LContext: TJSCanvasRenderingContext2D;
  LFont: String;
  LAvailable: Integer;
  LPolicies: TNyxColumnSizings;
  LWidths: TNyxFlowSizes;
  LColumn: Integer;
  LRow: Integer;
  LResidentIndex: Integer;
  LNatural: Double;
  LWidth: String;
  LData: INyxCollectionSnapshot;
begin
  LAvailable := Max(0, FHost.clientWidth);

  if FHasColumnLayout then
  begin
    LAvailable := ColumnClipWidth;
  end;
  LFont := window.getComputedStyle(FHost).getPropertyValue('font');

  if LFont = '' then
  begin
    LFont := '13px sans-serif';
  end;
  LData := FView.Snapshot;

  if (Length(FColumnNatural) <> FView.Spec.Count) or (FColumnFont <> LFont) then
  begin
    { Bounded initial sample avoids materializing/measuring an entire virtual
      dataset. Width metrics then remain stable across scrolling and edits.
      Font changes invalidate measurement; resize only reallocates the widths. }
    LCanvas := TJSHTMLCanvasElement(document.createElement('canvas'));
    LContext := TJSCanvasRenderingContext2D(LCanvas.getContext('2d'));
    LContext.font := LFont;
    SetLength(FColumnNatural, FView.Spec.Count);
    for LColumn := 0 to FView.Spec.Count - 1 do
    begin
      LNatural := LContext.measureText(FView.Spec.ColumnAt(LColumn).Title).width;
      for LRow := 0 to Min(31, LData.Count - 1) do
      begin
        LNatural := Max(LNatural, LContext.measureText(
          FView.Spec.ColumnAt(LColumn).Display(LData.ItemAt(LRow), LData.Schema)).width);
      end;
      FColumnNatural[LColumn] := Max(48, Ceil(LNatural + 24));
    end;
    FColumnFont := LFont;
    FColumnAvailable := -1;
  end;

  if (FColumnAvailable = LAvailable) and (Length(FColumnWidths) = FView.Spec.Count) and
    (not FHasColumnLayout or (FColumnLayoutSerial = FColumnLayout.Serial)) then
  begin
    RefreshColumnResizers;
    Exit;
  end;
  SetLength(LPolicies, FView.Spec.Count);
  for LColumn := 0 to High(LPolicies) do
  begin
    LPolicies[LColumn] := FView.Spec.ColumnAt(LColumn).Sizing;
  end;

  if FHasColumnLayout then
  begin
    FColumnPresentation := NyxColumnPresentation(FColumnLayout,
      LAvailable, FColumnNatural);
    SetLength(LWidths, FView.Spec.Count);
    for LColumn := 0 to High(LWidths) do
    begin
      LWidths[LColumn] := FColumnPresentation.Width(LColumn);
    end;
    FColumnLayoutSerial := FColumnLayout.Serial;
    FHost.setAttribute('data-nyx-pins-suspended',
      LowerCase(BoolToStr(FColumnPresentation.PinsSuspended, True)));
  end
  else
  begin
    LWidths := NyxColumnWidths(LAvailable, LPolicies, FColumnNatural);
  end;
  FColumnWidths := LWidths;
  FColumnAvailable := LAvailable;
  for LColumn := 0 to High(LWidths) do
  begin
    LWidth := IntToStr(LWidths[LColumn]) + 'px';
    { These vectors retain canonical identity after the actual TH order moves. }
    FHeaderCells[LColumn].style.setProperty('width', LWidth);
    FHeaderCells[LColumn].style.setProperty('min-width', LWidth);
    FHeaderCells[LColumn].style.setProperty('max-width', LWidth);
    FHeaderCells[LColumn].style.setProperty('box-sizing', 'border-box');
    ApplyColumnFace(FHeaderCells[LColumn], LColumn, True, False);
    for LResidentIndex := 0 to High(FResidentRows) do
    begin
      LRow := FResidentRows[LResidentIndex];

      if FRows[LRow] <> nil then
      begin
        FRows[LRow].FCells[LColumn].style.setProperty('width', LWidth);
        FRows[LRow].FCells[LColumn].style.setProperty('min-width', LWidth);
        FRows[LRow].FCells[LColumn].style.setProperty('max-width', LWidth);
        FRows[LRow].FCells[LColumn].style.setProperty('box-sizing', 'border-box');
      end;
    end;
  end;

  if FHasColumnLayout and (Length(FHeaderCells) > 0) then
  begin
    OrderColumnChildren(TJSHTMLElement(FHeaderCells[0].parentElement), FHeaderCells);
  end;
  RefreshColumnResizers;
end;

procedure TBrowserMount.RenderFlatWindow;
const
  CEstimate = 32;
  COverscan = 6;
type
  TScrollPosition = record
    Element: TJSHTMLElement;
    Top: Double;
    Left: Double;
  end;
var
  LData: INyxCollectionSnapshot;
  LNext: array of TBrowserRow;
  LUsed: array of Boolean;
  LWork: array of Integer;
  LResident: array of Integer;
  LScroll: array of TScrollPosition;
  LWindow: TNyxCollectionRowWindow;
  LFocused: TJSHTMLElement;
  LFocusRow: TJSHTMLElement;
  LFocusID: TNyxText;
  LTabStop: TJSHTMLElement;
  LNextFocus: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LOptions: TFocusOptions;
  LIndex: Integer;
  LPrevious: Integer;
  LCursor: Integer;
  LPosition: Integer;
  LWorkIndex: Integer;
  LListEntry: Integer;
  LSameOrder: Boolean;
  LSameSnapshot: Boolean;
  LRealize: Boolean;
  LMeasured: Boolean;
  LTextWork: Integer;
  LOffset: Double;
  LExtent: Double;
  LHeight: Double;
  LRowGap: Double;
  LSpacing: String;
  LAnchor: TJSNode;
  LAncestor: TJSHTMLElement;
  LWindowTop: Double;
  LWindowLeft: Double;
  LCaretStart: NativeInt;
  LCaretEnd: NativeInt;
  LCaretDirection: String;

  { This private work vector admits only current logical indices. It combines
    existing owned faces/drafts, the viewport and pinned navigation destinations.
    Sorting once avoids dataset-sized marker allocation on every scroll. }
  procedure AddWork(AIndex: Integer);
  begin

    if (AIndex < 0) or (AIndex >= LData.Count) then
    begin
      Exit;
    end;
    SetLength(LWork, Length(LWork) + 1);
    LWork[High(LWork)] := AIndex;
  end;

  procedure SortWork(AFirst, ALast: Integer);
  var
    LLeft: Integer;
    LRight: Integer;
    LPivot: Integer;
    LSwap: Integer;
  begin
    LLeft := AFirst;
    LRight := ALast;
    LPivot := LWork[AFirst + (ALast - AFirst) div 2];
    repeat
      while LWork[LLeft] < LPivot do
      begin
        Inc(LLeft);
      end;
      while LWork[LRight] > LPivot do
      begin
        Dec(LRight);
      end;

      if LLeft <= LRight then
      begin
        LSwap := LWork[LLeft];
        LWork[LLeft] := LWork[LRight];
        LWork[LRight] := LSwap;
        Inc(LLeft);
        Dec(LRight);
      end;
    until LLeft > LRight;

    if AFirst < LRight then
    begin
      SortWork(AFirst, LRight);
    end;

    if LLeft < ALast then
    begin
      SortWork(LLeft, ALast);
    end;
  end;

  { Spacers represent exact missing logical intervals. They carry no item/cell
    identity and are excluded from accessibility and all keyboard/range orders.
    Owned Nyx theme uses collapsed borders; separate spacing is compensated. }
  procedure AddGap(AFirst, AAfter: Integer);
  var
    LRow: TJSHTMLElement;
    LCell: TJSHTMLElement;
    LPixels: Double;
    LFormat: TFormatSettings;
  begin

    if AAfter <= AFirst then
    begin
      Exit;
    end;
    LPixels := Max(0, FGeometry.OffsetAt(AAfter) - FGeometry.OffsetAt(AFirst) - LRowGap);

    if FView.Projection = cpTable then
    begin
      LRow := NewElement('tr');
    end
    else
    begin
      LRow := NewElement('li');
    end;
    LRow.setAttribute('aria-hidden', 'true');
    LRow.setAttribute('data-nyx-row-spacer', 'true');
    LCell := LRow;

    if FView.Projection = cpTable then
    begin
      LCell := NewElement('td');
      LCell.setAttribute('colspan', IntToStr(Max(1, DisplayColumnCount)));
      LRow.appendChild(LCell);
    end;
    { CSS uses a decimal point independently of application/user RTL locale. }
    LFormat := FormatSettings;
    LFormat.DecimalSeparator := '.';
    LCell.style.setProperty('height', FloatToStr(LPixels, LFormat) + 'px');
    LCell.style.setProperty('padding', '0');
    LCell.style.setProperty('border', '0');
    LCell.style.setProperty('line-height', '0');
    SetLength(FSpacers, Length(FSpacers) + 1);
    FSpacers[High(FSpacers)] := LRow;
    LAnchor := nil;

    if LPosition < FBody.children.length then
    begin
      LAnchor := FBody.children[LPosition];
    end;
    FBody.insertBefore(LRow, LAnchor);
    Inc(LPosition);
  end;

begin
  FCellFocusValid := False;

  if DeferTargetRefresh then
  begin
    Exit;
  end;
  LData := FView.Snapshot;

  if DeferTargetRefresh then
  begin
    Exit;
  end;

  if not FEnabled then
  begin
    FRevealItem := Default(TNyxItemRef);
  end;

  if FEditingItem.Defined and
    (not FEnabled or FReadOnly or (LData.IndexOf(FEditingItem) < 0) or
      ((FView.Projection = cpTable) and not ColumnVisible(FEditingColumn))) then
  begin
    { Revoked authority or filtered/removed membership ends runtime editing.
      A later reappearance of the same open item name must not resurrect an
      editor that no longer owns focus. Authored values remain untouched. }
    FEditingItem := Default(TNyxItemRef);
  end;
  LFocused := TJSHTMLElement(document.activeElement);

  if FView.Projection = cpTable then
  begin

    if (DisplayColumnCount > 0) and not ColumnVisible(FFocusedColumn) then
    begin
      FFocusedColumn := DisplayColumn(0);
    end;
    SizeTableColumns;
  end;
  LFocusID := '';
  LInput := nil;

  if FHost.contains(LFocused) then
  begin
    LFocusRow := TJSHTMLElement(LFocused.closest('[data-nyx-item]'));

    if LFocusRow <> nil then
    begin
      LFocusID := LFocusRow.getAttribute('data-nyx-item');
    end;

    if (LFocused is TJSHTMLInputElement) and
      (TJSHTMLInputElement(LFocused)._type <> 'checkbox') then
    begin
      LInput := TJSHTMLInputElement(LFocused);
      LCaretStart := LInput.selectionStart;
      LCaretEnd := LInput.selectionEnd;
      LCaretDirection := LInput.selectionDirection;
    end;
  end
  else
  begin
    LFocused := nil;
  end;
  LSameSnapshot := FRendered = LData;
  LSameOrder := (FRendered <> nil) and (FRendered.Count = LData.Count);

  if LSameSnapshot then
  begin
    { The mount exclusively owns these mutable row slots. Same-source refresh
      needs no copy/remapping; retained runtime views never borrow this array. }
    LNext := FRows;
    LWork := Copy(FResidentRows, 0, Length(FResidentRows));
  end
  else
  begin
    SetLength(LNext, LData.Count);
    SetLength(LUsed, Length(FRows));
    for LIndex := 0 to LData.Count - 1 do
    begin
      LPrevious := -1;

      if FRendered <> nil then
      begin
        LPrevious := FRendered.IndexOf(LData.ItemAt(LIndex).Ref);
      end;

      if LPrevious <> LIndex then
      begin
        LSameOrder := False;
      end;

      if (LPrevious >= 0) and (LPrevious < Length(FRows)) then
      begin
        LNext[LIndex] := FRows[LPrevious];
        LUsed[LPrevious] := True;

        if LNext[LIndex] <> nil then
        begin
          AddWork(LIndex);
        end;
      end;
    end;
  end;

  if not LSameOrder then
  begin
    FGeometry.Reset(LData.Count, CEstimate);
  end;
  RowViewport(LOffset, LExtent);
  LWindow := FGeometry.Window(LOffset, LExtent, COverscan);
  LListEntry := -1;

  if (FView.Projection = cpList) and (LData.Count > 0) then
  begin
    { A list with no selected/focused option retains the first logical Tab
      entry, even after manual scrolling. DOM membership does not own order. }
    LListEntry := 0;

    if FView.Selection.Focus.Defined then
    begin
      LListEntry := LData.IndexOf(FView.Selection.Focus);
    end;
    AddWork(LListEntry);
  end;
  for LIndex := LWindow.First to LWindow.AfterLast - 1 do
  begin
    AddWork(LIndex);
  end;

  if LFocusID <> '' then
  begin
    AddWork(LData.IndexOf(NyxItem(LData.Key, LFocusID)));
  end;

  if FView.Selection.Focus.Defined then
  begin
    AddWork(LData.IndexOf(FView.Selection.Focus));
  end;

  if FForcedItem.Defined then
  begin
    AddWork(LData.IndexOf(FForcedItem));
  end;

  if FHasColumnLayout and FEditingItem.Defined then
  begin
    { An imported equal-text editor still owns its caret/editing session before
      final focus. Keep that one logical row through later window passes. }
    AddWork(LData.IndexOf(FEditingItem));
  end;

  if Length(LWork) > 1 then
  begin
    SortWork(0, High(LWork));
  end;
  LRowGap := 0;

  if (FView.Projection = cpTable) and
    (window.getComputedStyle(FHost).getPropertyValue('border-collapse') <> 'collapse') then
  begin
    LSpacing := Trim(window.getComputedStyle(FHost).getPropertyValue('border-spacing'));
    LPrevious := Pos(' ', LSpacing);

    if LPrevious > 0 then
    begin
      LSpacing := Copy(LSpacing, LPrevious + 1, Length(LSpacing));
    end;
    LRowGap := parseFloat(LSpacing);

    if IsNan(LRowGap) or IsInfinite(LRowGap) or (LRowGap < 0) then
    begin
      LRowGap := 0;
    end;
  end;
  { Capture before even an offscreen draft/row is detached: DOM removal may
    synchronously clamp a scroll owner. Keep the surrounding policy untouched. }
  LWindowTop := TScrollableWindow(window).scrollYPixels;
  LWindowLeft := TScrollableWindow(window).scrollXPixels;
  LAncestor := FHost;
  while LAncestor <> nil do
  begin
    SetLength(LScroll, Length(LScroll) + 1);
    LScroll[High(LScroll)].Element := LAncestor;
    LScroll[High(LScroll)].Top := TScrollableElement(LAncestor).scrollTopPixels;
    LScroll[High(LScroll)].Left := TScrollableElement(LAncestor).scrollLeftPixels;
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;
  FBody.style.setProperty('overflow-anchor', 'none');
  LTabStop := nil;
  for LWorkIndex := 0 to High(LWork) do
  begin
    LIndex := LWork[LWorkIndex];

    if (LWorkIndex > 0) and (LIndex = LWork[LWorkIndex - 1]) then
    begin
      Continue;
    end;
    LRealize := ((LIndex >= LWindow.First) and (LIndex < LWindow.AfterLast)) or
      (LIndex = LListEntry) or
      (LData.ItemAt(LIndex).Ref.ID = LFocusID) or
      (FView.Selection.Focus.Defined and
        (LData.ItemAt(LIndex).Ref.ID = FView.Selection.Focus.ID)) or
      (FForcedItem.Defined and (LData.ItemAt(LIndex).Ref.ID = FForcedItem.ID)) or
      (FHasColumnLayout and FEditingItem.Defined and
        (LData.ItemAt(LIndex).Ref.ID = FEditingItem.ID));

    if LRealize and (LNext[LIndex] = nil) then
    begin
      LNext[LIndex] := TBrowserRow.Create(Self, LData.ItemAt(LIndex).Ref);
    end;

    if LNext[LIndex] <> nil then
    begin
      LNext[LIndex].Sync(FRefreshPlan.ValuesChanged(LIndex));

      if not LRealize and not LNext[LIndex].HasDraft then
      begin
        LNext[LIndex].Free;
        LNext[LIndex] := nil;
      end
      else if not LRealize then
      begin

        if LNext[LIndex].FElement.parentNode <> nil then
        begin
          LNext[LIndex].FElement.remove;
        end;
      end
      else
      begin

        if FView.Projection = cpTable then
        begin
          LNext[LIndex].FElement.setAttribute('aria-rowindex', IntToStr(LIndex + 2));
        end
        else
        begin
          LNext[LIndex].FElement.setAttribute('aria-posinset', IntToStr(LIndex + 1));
          LNext[LIndex].FElement.setAttribute('aria-setsize', IntToStr(LData.Count));
        end;

        if ((FView.Projection <> cpTable) or (DisplayColumnCount > 0)) and
          ((LTabStop = nil) or (FView.Selection.Focus.Defined and
          (LData.ItemAt(LIndex).Ref.ID = FView.Selection.Focus.ID))) then
        begin
          LTabStop := LNext[LIndex].FCells[FFocusedColumn];
        end;
      end;

      if LNext[LIndex] <> nil then
      begin
        SetLength(LResident, Length(LResident) + 1);
        LResident[High(LResident)] := LIndex;
      end;
    end;
  end;

  if not LSameSnapshot then
  begin
    for LIndex := 0 to Length(FRows) - 1 do
    begin

      if not LUsed[LIndex] then
      begin
        FRows[LIndex].Free;
      end;
    end;
  end;
  FRows := LNext;
  FResidentRows := LResident;
  for LIndex := 0 to Length(FSpacers) - 1 do
  begin
    FSpacers[LIndex].remove;
  end;
  FSpacers := nil;
  LCursor := 0;
  LPosition := 0;
  for LWorkIndex := 0 to High(FResidentRows) do
  begin
    LIndex := FResidentRows[LWorkIndex];

    if (FRows[LIndex] <> nil) and
      (((LIndex >= LWindow.First) and (LIndex < LWindow.AfterLast)) or
        (LIndex = LListEntry) or
        (FRows[LIndex].FRef.ID = LFocusID) or
        (FView.Selection.Focus.Defined and
          (FRows[LIndex].FRef.ID = FView.Selection.Focus.ID)) or
        (FForcedItem.Defined and (FRows[LIndex].FRef.ID = FForcedItem.ID))) then
    begin
      AddGap(LCursor, LIndex);
      LAnchor := nil;

      if LPosition < FBody.children.length then
      begin
        LAnchor := FBody.children[LPosition];
      end;

      if FRows[LIndex].FElement <> LAnchor then
      begin
        FBody.insertBefore(FRows[LIndex].FElement, LAnchor);
      end;
      Inc(LPosition);
      LCursor := LIndex + 1;
    end;
  end;
  AddGap(LCursor, LData.Count);
  for LIndex := 0 to Length(LScroll) - 1 do
  begin
    TScrollableElement(LScroll[LIndex].Element).scrollTopPixels := LScroll[LIndex].Top;
    TScrollableElement(LScroll[LIndex].Element).scrollLeftPixels := LScroll[LIndex].Left;
  end;

  if (TScrollableWindow(window).scrollXPixels <> LWindowLeft) or
    (TScrollableWindow(window).scrollYPixels <> LWindowTop) then
  begin
    TScrollableWindow(window).scrollTo(LWindowLeft, LWindowTop);
  end;
  LMeasured := False;
  LTextWork := 0;
  for LWorkIndex := 0 to High(FResidentRows) do
  begin
    LIndex := FResidentRows[LWorkIndex];

    if (FRows[LIndex] <> nil) and FHost.contains(FRows[LIndex].FElement) then
    begin
      FRows[LIndex].SyncTextLayout(LTextWork);
      { offsetHeight is the logical outer allocation, excluding CSS transforms;
        screen rectangles would feed scaled heights back into logical spacers. }
      LHeight := FRows[LIndex].FElement.offsetHeight + LRowGap;

      if (LHeight > 0) and not IsNan(LHeight) and not IsInfinite(LHeight) and
        (Abs(LHeight - FGeometry.HeightAt(LIndex)) > 0.25) then
      begin
        LMeasured := FGeometry.Measure(LIndex, LHeight) or LMeasured;
      end;
    end;
  end;

  if FRevealItem.Defined then
  begin
    LIndex := LData.IndexOf(FRevealItem);

    if (LIndex >= 0) and (FRows[LIndex] <> nil) and FHost.contains(FRows[LIndex].FElement) then
    begin
      { Newly realized rows above the destination can increase its physical
        offset after the first key. Reapply nearest reveal through that measured
        pass, without refocusing or changing the containing page scroll. }
      RevealListOption(FRows[LIndex].FElement);
    end;

    if (LIndex < 0) or not LMeasured then
    begin
      FRevealItem := Default(TNyxItemRef);
    end;
  end;
  FRendered := LData;
  FForcedItem := Default(TNyxItemRef);
  { Cell ranges deliberately do not publish ordinary row focus. A measured
    window reconciliation must therefore retain the actual focused range cell
    as the sole Tab entry instead of returning to View.Selection.Focus. }

  if FCellRangeEnabled and (LFocused <> nil) and FHost.contains(LFocused) and
    (LFocused.getAttribute('role') = 'gridcell') and
    (LFocused.closest('[aria-hidden="true"]') = nil) then
  begin
    LTabStop := LFocused;
  end;
  FHost.setAttribute('tabindex', '-1');

  if FEnabled then
  begin

    if LTabStop <> nil then
    begin
      LTabStop.setAttribute('tabindex', '0');
    end
    else
    begin
      FHost.setAttribute('tabindex', '0');
    end;
  end;

  if LMeasured then
  begin
    QueueWindow;
  end;
  LNextFocus := nil;

  if (LFocused <> nil) and FEnabled then
  begin
    LNextFocus := LTabStop;

    if LNextFocus = nil then
    begin
      LNextFocus := FHost;
    end;

    if FHost.contains(LFocused) and
      (LFocused.closest('[aria-hidden="true"]') = nil) and
      not ((LFocused is TJSHTMLInputElement) and TJSHTMLInputElement(LFocused).disabled) then
    begin
      LNextFocus := LFocused;
    end;
  end;

  if (LNextFocus <> nil) and (document.activeElement <> LNextFocus) then
  begin
    LOptions := TFocusOptions.new;
    LOptions.preventScroll := True;
    TFocusable(LNextFocus).focus(LOptions);

    if GetConnected and (LInput <> nil) and (document.activeElement = LInput) then
    begin
      LInput.setSelectionRange(LCaretStart, LCaretEnd, LCaretDirection);
    end;
  end;
end;

function TBrowserRow.Toggle(AEvent: TEventListenerEvent): Boolean;
var
  LMount: INyxCollectionMount;
  LHierarchy: INyxTreeHierarchy;
  LExpanded: Boolean;
begin

  if (FOwner.FHasColumnLayout or FOwner.FObservesTablePresentation) and not FOwner.GetInputReady then
  begin
    AEvent.preventDefault;
    Exit(True);
  end;
  Result := True;
  LMount := FOwner as INyxCollectionMount;

  if not LMount.Connected then
  begin
    Exit;
  end;
  LHierarchy := NyxTreeHierarchy(FOwner.FView);
  { Real disclosure proposes runtime state. A nil event synchronizes and never
    publishes during Notify. DOM toggle events coalesce; current final state is
    authoritative instead of an obsolete event transition. }
  LExpanded := TNyxDetails(FElement).open;

  if (AEvent <> nil) and FOwner.FEnabled and LHierarchy.HasChildren(FRef) and
    (LExpanded <> LHierarchy.IsExpanded(FRef)) then
  begin
    LHierarchy.SetExpanded(FRef, LExpanded);
    { An observer can free this row. Only managed locals may be used afterward. }
    Exit;
  end;
  TNyxDetails(FElement).open := LHierarchy.IsExpanded(FRef);

  if not LHierarchy.HasChildren(FRef) then
  begin
    { A leaf is not a collapsed parent. Exposing aria-expanded on it would
      advertise disclosure that has no corresponding tree children. }
    FElement.removeAttribute('aria-expanded');
    Exit;
  end;
  FElement.setAttribute('aria-expanded', 'false');

  if TNyxDetails(FElement).open then
  begin
    FElement.setAttribute('aria-expanded', 'true');
  end;
end;

function TBrowserRow.Click(AEvent: TJSMouseEvent): Boolean;
const
  CEditorSelector = 'input,textarea,select,button,a[href],[contenteditable="true"]';
var
  LMount: INyxCollectionMount;
  LInputAdmission: INyxCollectionInputAdmission;
  LHost: TJSHTMLElement;
  LFocus: TJSHTMLElement;
  LEditor: TJSElement;
  LOptions: TFocusOptions;
  LCell: TJSHTMLElement;
  LColumn: Integer;
begin

  { A managed down already owns focus and range default. Only its exact ensuing
    physical primary click bypasses row selection; forward the ORIGINAL click
    to the projected host so canonical Click remains single-owned. Keyboard
    activation (detail=0), other cells and ordinary clicks keep the old path. }
  LCell := TJSHTMLElement(TJSHTMLElement(AEvent.target).closest('[role=gridcell]'));

  if (FOwner.FRangePointerCell <> nil) and (AEvent.detail > 0) and
    (AEvent.button = 0) and (LCell = FOwner.FRangePointerCell) then
  begin
    FOwner.FRangePointerCell := nil;
    LMount := FOwner as INyxCollectionMount;
    LInputAdmission := FOwner as INyxCollectionInputAdmission;
    LHost := FOwner.FHost;
    AEvent.stopPropagation;

    if LInputAdmission.InputReady and Assigned(LHost.onclick) then
    begin
      LHost.onclick(AEvent);
    end;
    AEvent.preventDefault;
    Exit(True);
  end;

  if (FOwner.FHasColumnLayout or FOwner.FObservesTablePresentation) and not FOwner.GetInputReady then
  begin
    AEvent.preventDefault;
    Exit(True);
  end;
  { A nested tree item owns its selection; bubbling must not select its parent. }
  AEvent.stopPropagation;

  if AEvent.defaultPrevented then
  begin
    Exit(True);
  end;

  if FOwner.FEditsSuspended then
  begin
    AEvent.preventDefault;
    Exit(True);
  end;

  if not FOwner.FEnabled then
  begin
    { aria-disabled alone does not remove a custom row's mouse default. Refuse
      disclosure, selection and focus before consulting the admitted view. }
    AEvent.preventDefault;
    Exit(True);
  end;
  { Selecting can notify an application observer that unmounts this whole view.
    Retain the attachment and capture the host before any row is detached. Never
    touch this row after publication; Disconnect may have destroyed it. }
  LMount := FOwner as INyxCollectionMount;
  LInputAdmission := FOwner as INyxCollectionInputAdmission;
  LHost := FOwner.FHost;
  LFocus := FElement;

  if FOwner.FView.Projection = cpTable then
  begin
    LCell := TJSHTMLElement(TJSHTMLElement(AEvent.target).closest('[role=gridcell]'));

    if LCell <> nil then
    begin
      LColumn := StrToIntDef(LCell.getAttribute('data-nyx-column'), -1);

      if (LColumn >= 0) and (LColumn < Length(FCells)) and (FCells[LColumn] = LCell) then
      begin
        FOwner.FFocusedColumn := LColumn;
        LFocus := LCell;
      end;
    end;
  end;
  { Selecting an editable row must not steal the caret from the actual editor.
    Capture that managed DOM element before publication can detach this row. }

  LEditor := TJSHTMLElement(AEvent.target).closest(CEditorSelector);

  if LEditor <> nil then
  begin
    LFocus := TJSHTMLElement(LEditor);
  end;
  FOwner.Gesture(FRef, AEvent.shiftKey, AEvent.ctrlKey or AEvent.metaKey);

  if LInputAdmission.InputReady then
  begin
    { A same-row click may change only the column, so no selection publication
      is required. Refresh the single Tab entry while preserving all drafts. }
    LMount.Refresh;
  end;

  if LInputAdmission.InputReady then
  begin
    LOptions := TFocusOptions.new;
    LOptions.preventScroll := True;
    TFocusable(LFocus).focus(LOptions);
  end;
  { Preserve Nyx's existing control callback/scheduler boundary while stopping
    ancestor row selection. Forward the original event to the projected host's
    handler, with its modifiers and default-consumption state intact. }

  if LInputAdmission.InputReady and Assigned(LHost.onclick) then
  begin
    LHost.onclick(AEvent);
  end;
  Result := True;
end;

{ Text belongs to the cell until its exact eligible editor is entered. Retained
  editor objects keep unfinished values and Unicode selection across unrelated
  publications; only visibility and runtime edit ownership change here. }
function TBrowserMount.CellEditing(const AItem: TNyxItemRef; AColumn: Integer): Boolean;
begin
  Result := FEnabled and not FReadOnly and FEditingItem.Defined and
    (FEditingItem.ID = AItem.ID) and (FEditingColumn = AColumn) and
    ((FView.Projection <> cpTable) or ColumnVisible(AColumn));
end;

function TBrowserMount.EnterCellEditor(const AItem: TNyxItemRef; AColumn: Integer): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LRow: Integer;
  LInput: TJSHTMLInputElement;
  LOptions: TFocusOptions;
begin

  if (FHasColumnLayout or FObservesTablePresentation) and (not GetInputReady or (FComposingInput <> nil)) then
  begin
    Exit(False);
  end;
  Result := False;

  if not GetConnected or FEditsSuspended or not FEnabled or FReadOnly or
    not (FView.Projection in [cpTable, cpTree]) then
  begin
    Exit;
  end;

  if (FView.Projection = cpTable) and not ColumnVisible(AColumn) then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LRow := FView.Snapshot.IndexOf(AItem);

  if not GetInputReady or ((FHasColumnLayout or FObservesTablePresentation) and (FComposingInput <> nil)) or
    (LRow < 0) or (LRow >= Length(FRows)) or (FRows[LRow] = nil) or
    (FRows[LRow].FRef.ID <> AItem.ID) or
    (AColumn < 0) or (AColumn >= Length(FRows[LRow].FInputs)) then
  begin
    Exit;
  end;
  LInput := FRows[LRow].FInputs[AColumn];

  if (LInput = nil) or LInput.disabled then
  begin
    Exit;
  end;
  { Selection may enter application/model code. Publish physical editor mode
    only after that returns and the cached composition/end-pending fence is
    still clear; a retained input must never replace the composing control. }
  Select(AItem, nsaFocus);

  if GetInputReady and not ((FHasColumnLayout or FObservesTablePresentation) and (FComposingInput <> nil)) and
    FHost.contains(LInput) then
  begin
    FEditingItem := AItem;
    FEditingColumn := AColumn;
    FFocusedColumn := AColumn;
    Refresh;

    if GetInputReady and not ((FHasColumnLayout or FObservesTablePresentation) and (FComposingInput <> nil)) and
      FHost.contains(LInput) then
    begin
      LOptions := TFocusOptions.new;
      LOptions.preventScroll := True;
      TFocusable(LInput).focus(LOptions);
      Result := True;
    end;
  end;
end;

function TBrowserMount.EditorFocusChanged(AEvent: TEventListenerEvent): Boolean;
var
  LCell: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LRow: Integer;
  LColumn: Integer;
  LItem: TNyxItemRef;
  LRelated: TJSElement;
begin

  if FOpeningColumnManager or (FCellPresentationDepth > 0) then
  begin
    Exit(True);
  end;

  if (FHasColumnLayout or FObservesTablePresentation) and not GetInputReady then
  begin
    AEvent.preventDefault;
    Exit(True);
  end;
  Result := True;

  if (FHasColumnLayout or FObservesTablePresentation) and (FComposingInput <> nil) then
  begin
    FCompositionFocusPending := True;
    Exit;
  end;

  if not GetConnected or FEditsSuspended or not (FView.Projection in [cpTable, cpTree]) or
    not (AEvent.target is TJSHTMLInputElement) then
  begin
    Exit;
  end;
  LInput := TJSHTMLInputElement(AEvent.target);
  LCell := TJSHTMLElement(LInput.closest('[role=gridcell]'));

  if FView.Projection = cpTree then
  begin
    LCell := TJSHTMLElement(LInput.closest('summary'));
  end;

  if (LCell = nil) or not FHost.contains(LCell) then
  begin
    Exit;
  end;
  LItem := NyxItem(FView.Spec.Key, LCell.parentElement.getAttribute('data-nyx-item'));
  LRow := FView.Snapshot.IndexOf(LItem);
  LColumn := StrToIntDef(LCell.getAttribute('data-nyx-column'), -1);

  if (LRow < 0) or (LRow >= Length(FRows)) or (FRows[LRow] = nil) or
    (LColumn < 0) or (LColumn >= Length(FRows[LRow].FInputs)) or
    (FRows[LRow].FInputs[LColumn] <> LInput) then
  begin
    Exit;
  end;

  if AEvent._type = 'focusin' then
  begin
    FEditingItem := LItem;
    FEditingColumn := LColumn;
  end
  else if CellEditing(LItem, LColumn) then
  begin
    LRelated := TJSElement(TJSFocusEvent(AEvent).relatedTarget);

    if (LRelated = nil) or not FHost.contains(LRelated) or
      (LowerCase(LRelated.tagName) <> 'input') then
    begin
      FEditingItem := Default(TNyxItemRef);
    end;
  end;
  { Focus events run inside the host's focusing algorithm. Defer presentation
    rather than hide or refocus the element while its blur/change is draining. }
  QueueWindow;
end;

function TBrowserRow.DoubleClick(AEvent: TJSMouseEvent): Boolean;
var
  LMount: INyxCollectionMount;
  LInputAdmission: INyxCollectionInputAdmission;
  LOwner: TBrowserMount;
  LHost: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LItem: TNyxItemRef;
  LColumn: Integer;
begin

  if (FOwner.FHasColumnLayout or FOwner.FObservesTablePresentation) and not FOwner.GetInputReady then
  begin
    AEvent.preventDefault;
    Exit(True);
  end;
  Result := True;
  AEvent.stopPropagation;
  LMount := FOwner as INyxCollectionMount;
  LInputAdmission := FOwner as INyxCollectionInputAdmission;

  if not LMount.Connected or AEvent.defaultPrevented then
  begin
    Exit;
  end;
  LCell := TJSHTMLElement(TJSHTMLElement(AEvent.target).closest('[role=gridcell]'));

  if FOwner.FView.Projection = cpTree then
  begin
    LCell := TJSHTMLElement(TJSHTMLElement(AEvent.target).closest('summary'));
  end;

  if LCell = nil then
  begin
    Exit;
  end;
  LColumn := StrToIntDef(LCell.getAttribute('data-nyx-column'), -1);

  if FOwner.FView.Projection = cpTree then
  begin
    LColumn := 0;
  end;

  if (LColumn < 0) or (LColumn >= Length(FCells)) or (FCells[LColumn] <> LCell) then
  begin
    Exit;
  end;
  LItem := FRef;
  LHost := FOwner.FHost;
  LOwner := FOwner;

  if LInputAdmission.InputReady and Assigned(LHost.ondblclick) then
  begin
    LHost.ondblclick(AEvent);
  end;

  if LInputAdmission.InputReady and not AEvent.defaultPrevented then
  begin
    LOwner.EnterCellEditor(LItem, LColumn);
  end;
end;

{$include nyx.collections.browser.selection.inc}

function TBrowserRow.Change(AEvent: TEventListenerEvent): Boolean;
var
  LIndex: Integer;
  LValue: TNyxText;
  LOwner: TBrowserMount;
  LKeepAlive: INyxCollectionMount;
begin

  if (FOwner.FHasColumnLayout or FOwner.FObservesTablePresentation) and not FOwner.GetInputReady then
  begin
    AEvent.preventDefault;
    Exit(True);
  end;
  Result := True;
  AEvent.stopPropagation;
  LOwner := FOwner;
  LKeepAlive := LOwner as INyxCollectionMount;

  if FOwner.FUpdating or FOwner.FEditsSuspended or FOwner.FOpeningColumnManager or
    (FOwner.FCellPresentationDepth > 0) then
  begin
    { Relocating retained rows can blur an editor on older DOM engines. That
      publication noise must never admit an unfinished draft as a user edit. }
    Exit;
  end;
  for LIndex := 0 to Length(FInputs) - 1 do
  begin

    if AEvent.target = FInputs[LIndex] then
    begin

      if not LOwner.ColumnVisible(LIndex) then
      begin
        { A retained hidden draft owns text only. Synthetic/native change must
          not grant hidden controls editing authority or write the collection. }
        Exit;
      end;

      if (LOwner.FHasColumnLayout or LOwner.FObservesTablePresentation) and (LOwner.FComposingInput = FInputs[LIndex]) then
      begin
        { Capture from the immutable physical snapshot, not the newer model.
          Repeated changes coalesce; final value is intentionally read at drain. }
        LOwner.FCompositionChange.Pending := True;
        LOwner.FCompositionChange.Input := FInputs[LIndex];
        LOwner.FCompositionChange.Item := FRef;
        LOwner.FCompositionChange.Field := LOwner.FColumnLayout.ColumnAt(LIndex).Column.Identity;
        LOwner.FCompositionChange.Accepted := LOwner.FColumnLayout.ColumnAt(LIndex).Column
          .Read(LOwner.FRendered.Item(FRef));
        Exit;
      end;
      LValue := FInputs[LIndex].value;

      if FInputs[LIndex]._type = 'checkbox' then
      begin
        LValue := 'false';

        if FInputs[LIndex].checked then
        begin
          LValue := 'true';
        end;
      end;
      FOwner.EditCell(FRef, LIndex, LValue);
      Exit;
    end;
  end;
end;

{$I nyx.collections.browser.tree.inc}

procedure TBrowserMount.RenderDataset;
begin

  if DeferTargetRefresh then
  begin
    Exit;
  end;
  FHost.setAttribute('aria-disabled', LowerCase(BoolToStr(not FEnabled, True)));

  if FColumnLauncher <> nil then
  begin

    if FEnabled and (FColumnManager <> nil) then
    begin
      FColumnLauncher.removeAttribute('disabled');
    end
    else
    begin
      FColumnLauncher.setAttribute('disabled', '');
    end;
  end;
  RefreshQueryHeaders;
  case FView.Projection of
    cpTable:
      begin
        FHost.setAttribute('aria-readonly', LowerCase(BoolToStr(FReadOnly, True)));
        FHost.setAttribute('aria-rowcount', IntToStr(FView.Snapshot.Count + 1));

        if DisplayColumnCount > 0 then
        begin
          FHost.setAttribute('aria-colcount', IntToStr(DisplayColumnCount));
        end
        else
        begin
          FHost.removeAttribute('aria-colcount');
        end;
        RenderFlatWindow;
      end;
    cpList:
      begin
        RenderFlatWindow;
      end;
    cpTree:
      begin
        RenderTreeWindow;
      end;
  end;
end;

procedure TBrowserMount.RefreshQueryHeaders;
var
  LColumn, LIndex: Integer;
  LSortEnabled, LFilterEnabled: Boolean;
  LSort: TNyxCollectionSort;
begin
  for LColumn := 0 to High(FHeaderCells) do
  begin
    FHeaderLabels[LColumn].textContent := QueryHeader(LColumn);
    FHeaderSorts[LColumn].textContent := QueryHeader(LColumn);
    FHeaderSorts[LColumn].setAttribute('aria-label', 'Sort ' + QueryHeader(LColumn) +
      '. Activate for ascending, descending, then default order. Shift adds a key.');
    LSortEnabled := (FQueries <> nil) and FView.Spec.TableQueryPolicy.SortEnabled and
      FView.Spec.ColumnAt(LColumn).QueryPolicy.SortEnabled;
    LFilterEnabled := (FQueries <> nil) and FView.Spec.TableQueryPolicy.FilterEnabled and
      FView.Spec.ColumnAt(LColumn).QueryPolicy.FilterEnabled;
    FHeaderLabels[LColumn].style.setProperty('display', '');
    FHeaderSorts[LColumn].style.setProperty('display', 'none');
    FHeaderFilters[LColumn].style.setProperty('display', 'none');

    if LSortEnabled then
    begin
      FHeaderLabels[LColumn].style.setProperty('display', 'none');
      FHeaderSorts[LColumn].style.setProperty('display', '');
    end;

    if LFilterEnabled then
    begin
      FHeaderFilters[LColumn].style.setProperty('display', '');
    end;

    if FEnabled then
    begin
      FHeaderSorts[LColumn].removeAttribute('disabled');
      FHeaderFilters[LColumn].removeAttribute('disabled');
    end
    else
    begin
      FHeaderSorts[LColumn].setAttribute('disabled', '');
      FHeaderFilters[LColumn].setAttribute('disabled', '');
    end;
    FHeaderCells[LColumn].setAttribute('aria-sort', 'none');
    for LIndex := 0 to FView.QueryPolicy.SortCount - 1 do
    begin
      LSort := FView.QueryPolicy.SortAt(LIndex);

      if LSort.FieldName = FView.Spec.ColumnAt(LColumn).FieldName then
      begin

        if LIndex > 0 then
        begin
          FHeaderCells[LColumn].setAttribute('aria-sort', 'other');
        end
        else if LSort.Direction = nsdAscending then
        begin
          FHeaderCells[LColumn].setAttribute('aria-sort', 'ascending');
        end
        else
        begin
          FHeaderCells[LColumn].setAttribute('aria-sort', 'descending');
        end;
      end;
    end;
  end;
  ApplyHeaderPresentation;
end;

{ Caption clamping owns only private header labels. Sort/filter buttons retain
  their complete accessible captions and natural interactive allocation; the
  minimum is not a maximum for the entire header or its management toolbar. }
procedure TBrowserMount.ApplyHeaderPresentation;
var
  LHeader: TNyxTableHeaderPolicy;
  LColumn: Integer;
  LIndex: Integer;
  LLabel: TJSHTMLElement;
begin
  LHeader := Default(TNyxTableHeaderPolicy);

  if FHasTablePresentation then
  begin
    LHeader := FTablePresentation.TablePolicy.HeaderPolicy;
  end;
  for LColumn := 0 to High(FHeaderCells) do
  begin

    if LHeader.Defined then
    begin
      FHeaderCells[LColumn].style.setProperty('height',
        IntToStr(LHeader.MinimumLogicalHeight) + 'px');
      FHeaderCells[LColumn].setAttribute('data-nyx-header-lines',
        IntToStr(LHeader.ReservedLines));
    end
    else
    begin
      FHeaderCells[LColumn].style.removeProperty('height');
      FHeaderCells[LColumn].removeAttribute('data-nyx-header-lines');
    end;
    for LIndex := 0 to 1 do
    begin

      if LIndex = 0 then
      begin
        LLabel := FHeaderLabels[LColumn];
      end
      else
      begin
        LLabel := FHeaderSorts[LColumn];
      end;

      if LHeader.Defined then
      begin
        LLabel.setAttribute('title', QueryHeader(LColumn));
        LLabel.style.setProperty('overflow', 'hidden');
        LLabel.style.setProperty('max-width', '100%');
        LLabel.style.setProperty('min-height', 'calc(1lh * ' +
          IntToStr(LHeader.ReservedLines) + ')');
        LLabel.style.setProperty('-webkit-line-clamp', IntToStr(LHeader.ReservedLines));
        LLabel.style.setProperty('-webkit-box-orient', 'vertical');

        if LLabel.style.getPropertyValue('display') <> 'none' then
        begin
          LLabel.style.setProperty('display', '-webkit-box');
        end;
      end
      else
      begin
        LLabel.removeAttribute('title');
        LLabel.style.removeProperty('overflow');
        LLabel.style.removeProperty('max-width');
        LLabel.style.removeProperty('min-height');
        LLabel.style.removeProperty('-webkit-line-clamp');
        LLabel.style.removeProperty('-webkit-box-orient');
      end;
    end;
  end;
end;

procedure TBrowserMount.QueryColumn(AColumn: Integer; AFilter, AAppend: Boolean);
var
  LQueries: INyxCollectionQueryWorkbench;
  LIndex: Integer;
  LSort: TNyxCollectionSort;
  LObservation: TNyxQueryWorkbenchObservation;
  LField: TNyxText;
  LAscending, LReset: Boolean;
begin

  if not GetInputReady or ((FHasColumnLayout or FObservesTablePresentation) and (FComposingInput <> nil)) or
    not FEnabled or FUpdating or (FQueries = nil) or
    (AColumn < 0) or (AColumn >= FView.Spec.Count) then
  begin
    Exit;
  end;
  LQueries := FQueries;
  try

    if AFilter then
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

    if not GetInputReady or ((FHasColumnLayout or FObservesTablePresentation) and (FComposingInput <> nil)) or
      not FEnabled or (FQueries <> LQueries) then
    begin
      Exit;
    end;
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
      Failed(LException);
    end;
  end;
end;

function TBrowserMount.QueryClick(AEvent: TEventListenerEvent): Boolean;
var
  LKeep: INyxCollectionMount;
  LButton: TJSHTMLElement;
  LColumn: Integer;
  LFilter: Boolean;
begin

  if (FHasColumnLayout or FObservesTablePresentation) and (not GetInputReady or (FComposingInput <> nil)) then
  begin
    AEvent.preventDefault;
    Exit(True);
  end;
  Result := True;
  LKeep := Self as INyxCollectionMount;

  if not GetConnected or FEditsSuspended or not FEnabled or AEvent.defaultPrevented or
    not (AEvent.target is TJSHTMLElement) then
  begin
    Exit;
  end;
  LButton := TJSHTMLElement(TJSHTMLElement(AEvent.target).closest(
    '[data-nyx-query-column],[data-nyx-query-sort]'));

  if (LButton = nil) or not FHost.contains(LButton) then
  begin
    Exit;
  end;
  LFilter := LButton.hasAttribute('data-nyx-query-column');
  { A filter affordance has no sort attribute. DOM absence is JavaScript null,
    not a Pascal string; choose the present role before parsing its index. }

  if LFilter then
  begin
    LColumn := StrToIntDef(LButton.getAttribute('data-nyx-query-column'), -1);
  end
  else
  begin
    LColumn := StrToIntDef(LButton.getAttribute('data-nyx-query-sort'), -1);
  end;
  AEvent.preventDefault;
  QueryColumn(LColumn, LFilter, TJSMouseEvent(AEvent).shiftKey);
end;

function TBrowserMount.QueryKey(AEvent: TJSKeyboardEvent): Boolean;
var
  LKeep: INyxCollectionMount;
  LElement, LHeader: TJSHTMLElement;
  LColumn: Integer;
  LFilter: Boolean;
begin

  if (FHasColumnLayout or FObservesTablePresentation) and not GetInputReady then
  begin
    AEvent.preventDefault;
    Exit(True);
  end;
  Result := True;
  LKeep := Self as INyxCollectionMount;

  if not GetConnected or FEditsSuspended or not FEnabled or (FQueries = nil) or
    AEvent.defaultPrevented or AEvent.isComposing or
    not (AEvent.target is TJSHTMLElement) then
  begin
    Exit;
  end;
  LElement := TJSHTMLElement(AEvent.target);

  if LElement.closest(ColumnResizeSelector) <> nil then
  begin
    Exit;
  end;

  if (LowerCase(LElement.tagName) = 'input') or
    (LowerCase(LElement.tagName) = 'textarea') or
    (LowerCase(LElement.tagName) = 'select') then
  begin
    Exit;
  end;
  LColumn := FFocusedColumn;
  LHeader := TJSHTMLElement(LElement.closest('[data-nyx-query-column],[data-nyx-query-sort]'));
  LFilter := False;

  if LHeader <> nil then
  begin
    LFilter := LHeader.hasAttribute('data-nyx-query-column');
    { Keyboard activation uses the same exclusive DOM roles as pointer input. }

    if LFilter then
    begin
      LColumn := StrToIntDef(LHeader.getAttribute('data-nyx-query-column'), -1);
    end
    else
    begin
      LColumn := StrToIntDef(LHeader.getAttribute('data-nyx-query-sort'), -1);
    end;
  end;

  if (AEvent.altKey and (AEvent.key = 'ArrowDown')) or
    ((LHeader <> nil) and not AEvent.ctrlKey and not AEvent.metaKey and
      ((AEvent.key = 'Enter') or (AEvent.key = ' '))) then
  begin
    AEvent.preventDefault;
    QueryColumn(LColumn, LFilter or AEvent.altKey, AEvent.shiftKey);
  end;
end;

procedure TBrowserMount.DetachTarget;
var
  LIndex: Integer;
begin
  try
    try
      FinishRangeGesture(FRangeGesture);
    finally
      CancelColumnResizes;
    end;
  finally
    SetCellEditsSuspended(False);
    FEditSuspension := nil;
    FCellFocusValid := False;
    FCellFocusInput := nil;

    if FHost <> nil then
    begin
      FHost.removeEventListener('pointercancel', @RangeGestureCancel);
      FHost.removeEventListener('lostpointercapture', @RangeGestureCancel);
      FHost.removeEventListener('keydown', @RangeGestureCancel);
      window.removeEventListener('blur', @RangeGestureCancel);

      if FHasColumnLayout then
      begin
        FHost.removeEventListener('pointerdown', @ColumnResizeDown);
        FHost.removeEventListener('pointermove', @ColumnResizeMove);
        FHost.removeEventListener('pointerup', @ColumnResizeUp);
        FHost.removeEventListener('pointercancel', @ColumnResizeCancel);
        FHost.removeEventListener('lostpointercapture', @ColumnResizeCancel);
        FHost.removeEventListener('keydown', @ColumnResizeKey);
        FHost.removeEventListener('keyup', @ColumnButtonRelease);
        TCapturedElement(FHost).removeEventListener('focusout', @ColumnButtonBlur, True);
        window.removeEventListener('blur', @ColumnButtonBlur);
        FHost.removeEventListener('focusout', @ColumnResizeFocus);
        FHost.removeEventListener(ColumnFitClickEvent, @ColumnFitInput);
        FHost.removeEventListener(ColumnFitDoubleEvent, @ColumnFitInput);
      end;

      if FHasColumnLayout or FObservesTablePresentation then
      begin
        TCapturedElement(FHost).removeEventListener(ColumnCompositionStart,
          @CellComposition, True);
        TCapturedElement(FHost).removeEventListener(ColumnCompositionEnd,
          @CellComposition, True);
        for LIndex := Low(PendingInputEvents) to High(PendingInputEvents) do
        begin
          TCapturedElement(FHost).removeEventListener(PendingInputEvents[LIndex],
            @BlockPendingInput, True);
        end;
      end;
    FHost.removeEventListener('keydown', @Key);
    FHost.removeEventListener('contextmenu', @CellContext);
      FHost.removeEventListener('keydown', @QueryKey);
      FHost.removeEventListener('click', @QueryClick);
      FHost.removeEventListener('focusin', @WindowChanged);
      FHost.removeEventListener('focusin', @EditorFocusChanged);
      FHost.removeEventListener('focusout', @EditorFocusChanged);
    end;
    TCapturedDocument(document).removeEventListener('scroll', @ScrollChanged, True);
    window.removeEventListener('resize', @WindowChanged);

    if FWindowQueued then
    begin
      window.cancelAnimationFrame(FWindowFrame);
      FWindowQueued := False;
    end;

    if FWindowObserver <> nil then
    begin
      FWindowObserver.disconnect;
      FWindowObserver := nil;
    end;
    FWindowAncestors := nil;
    FGeometry.Free;
    FGeometry := nil;
    for LIndex := 0 to Length(FRows) - 1 do
    begin
      FRows[LIndex].Free;
    end;
    FRows := nil;
    FTreeRows := nil;
    FTreeGeometryRows := nil;
    FResidentRows := nil;
    FRevealItem := Default(TNyxItemRef);
    for LIndex := 0 to Length(FSpacers) - 1 do
    begin
      FSpacers[LIndex].remove;
    end;
    FSpacers := nil;
    FRendered := nil;
    FHeaderCells := nil;
    FHeaderLabels := nil;
    FHeaderSorts := nil;
    FHeaderFilters := nil;
    FHeaderResizers := nil;
    FBody := nil;
    FHost := nil;
  end;
end;

function MountNyxBrowserCollection(AHost: TJSHTMLElement;
  const AView: INyxCollectionView;
  AInputPhase: TNyxCollectionInputPhase;
  const AInputGate: INyxCollectionInputGate): INyxCollectionMount;
begin
  Result := MountNyxBrowserCollectionWithTextLimits(AHost, AView,
    TNyxTableTextWorkLimits.Defaults, AInputPhase, AInputGate);
end;

function MountNyxBrowserCollectionWithTextLimits(AHost: TJSHTMLElement;
  const AView: INyxCollectionView; const ALimits: TNyxTableTextWorkLimits;
  AInputPhase: TNyxCollectionInputPhase;
  const AInputGate: INyxCollectionInputGate): INyxCollectionMount;
var
  LMount: TBrowserMount;
begin
  LMount := TBrowserMount.Create(AHost, AView, ALimits, AInputPhase, AInputGate);
  Result := LMount;
  LMount.Activate;
end;

end.
