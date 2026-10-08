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
unit nyx.events;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.behavior,
  nyx.editing,
  nyx.gestures,
  nyx.scheduler;

type
  TNyxRegistrationID = type Integer;
  TNyxExecutions = array of INyxExecution;
  TNyxEventRole = (nerOrigin, nerSource);
  { A short-lived adapter callback, borrowed only within a guarded mounted-view
    dispatch. Every invocation returns an owned phase-specific payload. A view
    revision change prevents all later calls to a disposed supplier. }
  TNyxSignalSnapshot = function(ANode: TNyxNode;
    ATrigger: TNyxTrigger): TNyxEventInfo of object;

  { Immutable exact identity selector. Origin addresses the physical control;
    Source addresses its semantic compound. Explicit design/runtime modes resolve
    the same identity distinction as the adapters. Automatic accepts either.
    Open identity text is a name, never an execution or behavior keyword. }
  TNyxEventTarget = record
  private
    FID: TNyxText;
    FIdentity: TNyxIdentityKind;
    FRole: TNyxEventRole;
  public
    property ID: TNyxText read FID;
    property Identity: TNyxIdentityKind read FIdentity;
    property Role: TNyxEventRole read FRole;
  end;

  { Snapshot-only callbacks do not borrow a node/widget. They can survive
    navigation/disposal and execute on native workers. Model/UI updates must be
    submitted to neUIQueue. Callback implementations own their dependencies and
    remain alive until each scheduled invocation is complete. }
  INyxEventCallback = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001002000001}']
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
  end;
  TNyxEventCallback = class(TInterfacedObject, INyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); virtual; abstract;
  end;

  { A physical input's synchronous response, available through its execution.
    Consume suppresses the platform default, not sibling registrations. Only an
    active sequential callback on the UI thread may consume; retained, queued,
    threaded, cancelled and non-input contexts reject Consume explicitly.
    The interface owns no callback, router, node or widget. Consumed is monotonic
    and safely readable after dispatch, including by asynchronous callbacks. }
  INyxEventResponse = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001002000006}']
    procedure Consume;
    function GetCanConsume: Boolean;
    function GetConsumed: Boolean;
    property CanConsume: Boolean read GetCanConsume;
    property Consumed: Boolean read GetConsumed;
  end;

  { A typed physical gesture response, borrowed through the invocation execution.
    Requests are limited to active sequential UI callbacks. They are recorded
    without retaining controls and applied by the adapter after dispatch, only
    if the original view is still mounted. Last valid ordered request wins. }
  INyxGestureResponse = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001008000002}']
    function CanRequest(ACapability: TNyxGestureCapability): Boolean;
    procedure CapturePointer;
    procedure ReleasePointer;
    procedure OfferDrag(const ATransfer: TNyxTransferSnapshot; AAllowed: TNyxDropOperations);
    procedure AcceptDrop(AOperation: TNyxDropOperation);
  end;

  { A subscription is retained independently of its router. Cancel is a UI-thread
    operation; it removes the registration and cancels pending invocations. Work
    already running observes cancellation cooperatively. Dropping this interface
    does not remove a registration: call Cancel explicitly. No back-reference
    to the router or a component forms a retention cycle. }
  INyxEventSubscription = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001002000002}']
    function GetID: TNyxRegistrationID;
    function GetActive: Boolean;
    function GetLastExecution: INyxExecution;
    procedure Cancel;
    property ID: TNyxRegistrationID read GetID;
    property Active: Boolean read GetActive;
    { Latest execution exposes synchronous/asynchronous failures to a harness.
      It survives cancellation; no UI message box or swallowed failure is needed. }
    property LastExecution: INyxExecution read GetLastExecution;
  end;

  { One event has one execution policy and any number of ordered registrations.
    Sequential callbacks finish in registration order; async callbacks are
    submitted in that order, but completion order is deliberately unspecified.
    A failed callback records its own failure and does not suppress siblings.
    Unsupported policy changes fail before replacing the accepted policy. }
  INyxEventStream = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001002000003}']
    function Policy(AValue: TNyxExecutionPolicy): INyxEventStream;
    function GetPolicy: TNyxExecutionPolicy;
    function Subscribe(const ACallback: INyxEventCallback): INyxEventSubscription;
    function GetCount: Integer;
    function GetSubscription(AIndex: Integer): INyxEventSubscription;
    property ExecutionPolicy: TNyxExecutionPolicy read GetPolicy;
    property Count: Integer read GetCount;
    property Registrations[AIndex: Integer]: INyxEventSubscription read GetSubscription;
  end;

  { Runtime router, owned by the application/view, never a model child. Registration,
    policy and dispatch are UI-thread operations. Dispatch snapshots membership
    and policy before invoking anything; additions start with the next dispatch,
    cancellation suppresses not-yet-started calls and nested dispatch is supported.
    CancelPending invalidates a view generation without removing registrations.
    Close also removes registrations and prevents further authoring/dispatch. }
  INyxEvents = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001002000004}']
    function On(const ATarget: TNyxEventTarget; ATrigger: TNyxTrigger): INyxEventStream;
    { A semantic named stream matches the exact owned event reference, whether
      a standard physical route or a declared custom producer emitted it. Named
      streams share scheduling/cancellation with physical streams. They do not
      grant permission to emit an undeclared custom payload. }
    function OnNamed(const ATarget: TNyxEventTarget;
      const AName: TNyxEventRef): INyxEventStream;
    function OnAfterEnter(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterExit(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnKeyDown(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnKeyUp(const ATarget: TNyxEventTarget): INyxEventStream;
    { Logical key actuation and its phases use typed shortcut snapshots. Text
      proposals cover all edit origins; pointer hooks retain control-relative
      coordinates. Only synchronous input hooks can consume a default. }
    function OnBeforeKeyDown(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterKeyDown(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnKeyPress(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeKeyPress(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterKeyPress(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeKeyUp(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterKeyUp(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeTextInput(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnTextInput(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterTextInput(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeEdit(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnCompositionStart(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnCompositionUpdate(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnCompositionEnd(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnTextSelectionChange(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerCancel(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerCapture(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerCaptureLost(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragStart(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDrag(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragEnter(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragOver(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragExit(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDrop(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragEnd(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDoubleClick(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerDown(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerUp(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerMove(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerEnter(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerExit(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnContextMenu(const ATarget: TNyxEventTarget): INyxEventStream;
    { Wheel phases carry original units and cancelability. Scroll/ScrollEnd
      observe actual movement/completion and never permit Consume. }
    function OnBeforeWheel(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnWheel(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterWheel(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnScroll(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnScrollEnd(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnSelectionChange(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnImageLoading(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnImageReady(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnImageError(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnImageCleared(const ATarget: TNyxEventTarget): INyxEventStream;
    { Fast empty-router path lets focus bridges avoid creating unused payloads. }
    function HasSubscribers(ATrigger: TNyxTrigger): Boolean;
    function Dispatch(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText): TNyxExecutions;
    { Producer generation cancellation is linked before the first callback and
      inherited by PostUI alongside view/subscription cancellation. }
    function DispatchGuarded(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText;
      const ADelivery: INyxCancellationScope): TNyxExecutions;
    { Adapter-only keyboard boundary. The owned decision is sealed before return;
      asynchronous callbacks cannot retroactively prevent the native default. }
    function DispatchInput(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText;
      out AConsumed: Boolean): TNyxExecutions;
    function DispatchGesture(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText;
      const ADecision: INyxGestureDecision): TNyxExecutions;
    procedure CancelPending;
    procedure Close;
    function GetScheduler: INyxScheduler;
    function GetViewRevision: Integer;
    property Scheduler: INyxScheduler read GetScheduler;
    { Callback bridges can detect that a chained custom handler replaced or
      disposed its view before touching borrowed platform bindings again. }
    property ViewRevision: Integer read GetViewRevision;
  end;

function NyxControlEvents(const AID: TNyxText;
  AIdentity: TNyxIdentityKind = niDesign): TNyxEventTarget;
function NyxCompoundEvents(const AID: TNyxText;
  AIdentity: TNyxIdentityKind = niDesign): TNyxEventTarget;
function NewNyxEvents(const AScheduler: INyxScheduler = nil): INyxEvents;
{ Query an event invocation's typed response. Ordinary scheduler/child work is
  not a physical input invocation and raises ENyxSchedule on this query. }
function NyxEventResponse(const AExecution: INyxExecution): INyxEventResponse;
function NyxGestureResponse(const AExecution: INyxExecution): INyxGestureResponse;
{ Adapter-only physical negotiation. The decision is sealed in every outcome,
  including failure or navigation. It owns no platform or model references. }
procedure DispatchNyxGesture(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ADispatch: TNyxDispatch; const ADecision: INyxGestureDecision);
{ Shared adapter bridge. Retains both borrowed nodes across callbacks that may
  replace the mounted view; only immutable snapshots enter scheduled work. }
function DispatchNyxInput(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ADispatch: TNyxDispatch): Boolean;
{ Shared keyboard ordering: before, main, after; a key-down then has its key-
  press cycle. Press means logical key actuation, including repeat, rather than
  character insertion. A consumed before hook skips the main hook; after hooks
  observe DefaultPrevented and cannot consume. Navigation ends the cycle.
  The helper retains nodes only for the synchronous bridge, never scheduled work. }
function DispatchNyxKeyboard(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ADispatch: TNyxDispatch; ASnapshot: TNyxSignalSnapshot): Boolean;
function NyxHasKeyboardSubscribers(const AEvents: INyxEvents;
  ATrigger: TNyxTrigger): Boolean;
{ Wheel phases bracket dispatch before the platform default. Uncancelable
  requests have read-only responses; Scroll separately observes real movement. }
function DispatchNyxWheel(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ADispatch: TNyxDispatch; ASnapshot: TNyxSignalSnapshot): Boolean;
{ Complete a text admission attempt using owned before/after strings. Accepted
  changes emit TextInput then AfterTextInput; rejected proposals emit only the
  latter with DefaultPrevented=True. Navigation suppresses remaining callbacks. }
procedure DispatchNyxTextResult(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ADispatch: TNyxDispatch; ASnapshot: TNyxSignalSnapshot);

implementation

{$IFNDEF PAS2JS}
uses
  Classes;
{$ENDIF}

type
  TNyxEvents = class;

  INyxEventScope = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001002000005}']
    procedure Cancel;
    function Cancelled: Boolean;
  end;

  TNyxEventScope = class(TInterfacedObject, INyxEventScope)
  private
    FCancelled: LongInt;
    FParent: INyxEventScope;
    FDelivery: INyxCancellationScope;
  public
    constructor Create(const AParent: INyxEventScope = nil;
      const ADelivery: INyxCancellationScope = nil);
    procedure Cancel;
    function Cancelled: Boolean;
  end;

  INyxInputDecision = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001002000007}']
    procedure Consume;
    function Consumed: Boolean;
  end;
  TNyxInputDecision = class(TInterfacedObject, INyxInputDecision)
  private
    FConsumed: LongInt;
  public
    procedure Consume;
    function Consumed: Boolean;
  end;

  { Cancellation linked before entry, including a synchronous callback that
    cancels itself or navigates before Submit has returned its execution token. }
  TNyxEventExecution = class(TInterfacedObject, INyxExecution, INyxEventResponse,
    INyxGestureResponse)
  private
    FExecution: INyxExecution;
    FScope: INyxEventScope;
    FSubscriptionScope: INyxEventScope;
    FDecision: INyxInputDecision;
    FAllowsConsume: Boolean;
    FActive: LongInt;
    FGesture: INyxGestureDecision;
    FAllowsGesture: Boolean;
  public
    constructor Create(const AExecution: INyxExecution; const AScope: INyxEventScope;
      const ASubscriptionScope: INyxEventScope; const ADecision: INyxInputDecision;
      AAllowsConsume: Boolean; const AGesture: INyxGestureDecision; AAllowsGesture: Boolean);
    procedure Cancel;
    function GetCancelled: Boolean;
    function GetStatus: TNyxExecutionStatus;
    function GetFailure: TNyxText;
    procedure Consume;
    function GetCanConsume: Boolean;
    function GetConsumed: Boolean;
    procedure SealResponse;
    function CanRequest(ACapability: TNyxGestureCapability): Boolean;
    procedure CapturePointer;
    procedure ReleasePointer;
    procedure OfferDrag(const ATransfer: TNyxTransferSnapshot; AAllowed: TNyxDropOperations);
    procedure AcceptDrop(AOperation: TNyxDropOperation);
  end;

  TNyxSubscription = class(TInterfacedObject, INyxEventSubscription)
  private
    FID: TNyxRegistrationID;
    FScope: INyxEventScope;
    FCallback: INyxEventCallback;
    FExecutions: TNyxExecutions;
    FLastExecution: INyxExecution;
  public
    constructor Create(AID: TNyxRegistrationID; const ACallback: INyxEventCallback);
    function GetID: TNyxRegistrationID;
    function GetActive: Boolean;
    function GetLastExecution: INyxExecution;
    procedure Cancel;
    procedure Track(const AExecution: INyxExecution);
    property Callback: INyxEventCallback read FCallback;
    property Scope: INyxEventScope read FScope;
  end;

  TNyxEventGroup = class
  public
    Target: TNyxEventTarget;
    Trigger: TNyxTrigger;
    Name: TNyxEventRef;
    Policy: TNyxExecutionPolicy;
    Registrations: array of INyxEventSubscription;
    Entries: array of TNyxSubscription;
    procedure Prune;
  end;

  TNyxEventStream = class(TInterfacedObject, INyxEventStream)
  private
    FOwner: INyxEvents;
    FRouter: TNyxEvents;
    FGroup: TNyxEventGroup;
  public
    constructor Create(ARouter: TNyxEvents; AGroup: TNyxEventGroup);
    function Policy(AValue: TNyxExecutionPolicy): INyxEventStream;
    function GetPolicy: TNyxExecutionPolicy;
    function Subscribe(const ACallback: INyxEventCallback): INyxEventSubscription;
    function GetCount: Integer;
    function GetSubscription(AIndex: Integer): INyxEventSubscription;
  end;

  TNyxEventWork = class(TInterfacedObject, INyxWork)
  private
    FEvent: TNyxEventInfo;
    FCallback: INyxEventCallback;
    FScope: INyxEventScope;
    FSubscriptionScope: INyxEventScope;
    FDecision: INyxInputDecision;
    FAllowsConsume: Boolean;
    FGesture: INyxGestureDecision;
    FAllowsGesture: Boolean;
  public
    constructor Create(const AEvent: TNyxEventInfo;
      const ACallback: INyxEventCallback; const AScope, ASubscriptionScope: INyxEventScope;
      const ADecision: INyxInputDecision; AAllowsConsume: Boolean;
      const AGesture: INyxGestureDecision; AAllowsGesture: Boolean);
    procedure Execute(const AExecution: INyxExecution);
  end;

  TNyxEvents = class(TInterfacedObject, INyxEvents)
  private
    FClosed: Boolean;
    FViewRevision: Integer;
    FScope: INyxEventScope;
    FSerial: Integer;
    FScheduler: INyxScheduler;
    FGroups: array of TNyxEventGroup;
    FExecutions: TNyxExecutions;
    procedure Admit;
    procedure PruneExecutions;
    function Stream(const ATarget: TNyxEventTarget; ATrigger: TNyxTrigger;
      const AName: TNyxEventRef): INyxEventStream;
    function DispatchCore(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText;
      const ADecision: INyxInputDecision; const AGesture: INyxGestureDecision = nil;
      const ADelivery: INyxCancellationScope = nil): TNyxExecutions;
  public
    constructor Create(const AScheduler: INyxScheduler);
    destructor Destroy; override;
    function On(const ATarget: TNyxEventTarget; ATrigger: TNyxTrigger): INyxEventStream;
    function OnNamed(const ATarget: TNyxEventTarget;
      const AName: TNyxEventRef): INyxEventStream;
    function OnAfterEnter(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterExit(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnKeyDown(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnKeyUp(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeKeyDown(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterKeyDown(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnKeyPress(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeKeyPress(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterKeyPress(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeKeyUp(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterKeyUp(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeTextInput(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnTextInput(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterTextInput(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeEdit(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnCompositionStart(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnCompositionUpdate(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnCompositionEnd(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnTextSelectionChange(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerCancel(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerCapture(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerCaptureLost(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragStart(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDrag(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragEnter(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragOver(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragExit(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDrop(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragEnd(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDoubleClick(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerDown(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerUp(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerMove(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerEnter(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerExit(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnContextMenu(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeWheel(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnWheel(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterWheel(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnScroll(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnScrollEnd(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnSelectionChange(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnImageLoading(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnImageReady(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnImageError(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnImageCleared(const ATarget: TNyxEventTarget): INyxEventStream;
    function HasSubscribers(ATrigger: TNyxTrigger): Boolean;
    function Dispatch(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText): TNyxExecutions; reintroduce;
    function DispatchGuarded(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText;
      const ADelivery: INyxCancellationScope): TNyxExecutions;
    function DispatchInput(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText;
      out AConsumed: Boolean): TNyxExecutions;
    function DispatchGesture(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText;
      const ADecision: INyxGestureDecision): TNyxExecutions;
    procedure CancelPending;
    procedure Close;
    function GetScheduler: INyxScheduler;
    function GetViewRevision: Integer;
  end;

  { pas2js does not support COM interfaces in records. Owned invocation objects
    retain an independent snapshot without relying on shared dynamic records. }
  TNyxInvocation = class
  public
    Lease: INyxEventSubscription;
    Entry: TNyxSubscription;
    Callback: INyxEventCallback;
    Policy: TNyxExecutionPolicy;
  end;

function DispatchNyxInput(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ADispatch: TNyxDispatch): Boolean;
begin
  Result := False;

  if (AEvents = nil) or (AOrigin = nil) or (ADispatch.Source = nil) then
  begin
    raise ENyxSchedule.Create('Input dispatch requires its mounted event source');
  end;

  if ADispatch.EventName = '' then
  begin
    Exit;
  end;
  AOrigin.AcquireReference;
  ADispatch.Source.AcquireReference;
  try
    AEvents.DispatchInput(ADispatch.Info, AOrigin.DesignID,
      ADispatch.Source.DesignID, Result);
  finally
    ADispatch.Source.ReleaseReference;
    AOrigin.ReleaseReference;
  end;
end;

function NyxHasKeyboardSubscribers(const AEvents: INyxEvents;
  ATrigger: TNyxTrigger): Boolean;
begin
  Result := False;

  if AEvents = nil then
  begin
    Exit;
  end;

  if ATrigger = ntKeyDown then
  begin
    Result := AEvents.HasSubscribers(ntBeforeKeyDown) or
      AEvents.HasSubscribers(ntKeyDown) or AEvents.HasSubscribers(ntAfterKeyDown) or
      AEvents.HasSubscribers(ntBeforeKeyPress) or AEvents.HasSubscribers(ntKeyPress) or
      AEvents.HasSubscribers(ntAfterKeyPress);
  end
  else if ATrigger = ntKeyUp then
  begin
    Result := AEvents.HasSubscribers(ntBeforeKeyUp) or
      AEvents.HasSubscribers(ntKeyUp) or AEvents.HasSubscribers(ntAfterKeyUp);
  end;
end;

function DispatchNyxKeyboard(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ADispatch: TNyxDispatch; ASnapshot: TNyxSignalSnapshot): Boolean;
var
  LRevision: Integer;
  LDispatch: TNyxDispatch;
  LConsumed: Boolean;

  procedure Capture(ATrigger: TNyxTrigger);
  begin
    LDispatch.Info := ASnapshot(AOrigin, ATrigger);
    LDispatch.Info.HasKeyboard := True;
    LDispatch.Info.Keyboard := ADispatch.Info.Keyboard;
  end;

  procedure Cycle(ABefore, AMain, AAfter: TNyxTrigger);
  begin
    LConsumed := False;

    if AEvents.HasSubscribers(ABefore) then
    begin
      Capture(ABefore);
      LConsumed := DispatchNyxInput(AEvents, AOrigin, LDispatch);
    end;

    if AEvents.ViewRevision <> LRevision then
    begin
      Exit;
    end;

    if not LConsumed and AEvents.HasSubscribers(AMain) then
    begin
      Capture(AMain);
      LConsumed := DispatchNyxInput(AEvents, AOrigin, LDispatch);
    end;
    Result := Result or LConsumed;

    if AEvents.ViewRevision <> LRevision then
    begin
      Exit;
    end;

    if AEvents.HasSubscribers(AAfter) then
    begin
      Capture(AAfter);
      LDispatch.Info.DefaultPrevented := LConsumed;
      AEvents.Dispatch(LDispatch.Info, AOrigin.DesignID, LDispatch.Source.DesignID);
    end;
  end;

begin
  Result := False;

  if (AEvents = nil) or (AOrigin = nil) or (ADispatch.Source = nil) or
    not Assigned(ASnapshot) or not ADispatch.Info.HasKeyboard or
    not (ADispatch.Info.Trigger in [ntKeyDown, ntKeyUp]) then
  begin
    raise ENyxSchedule.Create('Keyboard cycles require a mounted typed key-down/up');
  end;

  if ADispatch.EventName = '' then
  begin
    Exit;
  end;
  LRevision := AEvents.ViewRevision;
  LDispatch := ADispatch;
  LDispatch.Info := ADispatch.Info.Copy;
  AOrigin.AcquireReference;
  LDispatch.Source.AcquireReference;
  try

    if ADispatch.Info.Trigger = ntKeyUp then
    begin
      Cycle(ntBeforeKeyUp, ntKeyUp, ntAfterKeyUp);
    end
    else
    begin
      Cycle(ntBeforeKeyDown, ntKeyDown, ntAfterKeyDown);

      if not Result and (AEvents.ViewRevision = LRevision) and
        not (ADispatch.Info.Keyboard.Key in [nkShiftKey, nkControlKey, nkAltKey, nkMetaKey]) then
      begin
        Cycle(ntBeforeKeyPress, ntKeyPress, ntAfterKeyPress);
      end;
    end;
    Result := Result or (AEvents.ViewRevision <> LRevision);
  finally
    LDispatch.Source.ReleaseReference;
    AOrigin.ReleaseReference;
  end;
end;

function DispatchNyxWheel(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ADispatch: TNyxDispatch; ASnapshot: TNyxSignalSnapshot): Boolean;
var
  LRevision: Integer;
  LDispatch: TNyxDispatch;

  procedure Capture(ATrigger: TNyxTrigger);
  begin
    LDispatch.Info := ASnapshot(AOrigin, ATrigger);
    LDispatch.Info.HasWheel := True;
    LDispatch.Info.Wheel := ADispatch.Info.Wheel;
  end;

begin
  Result := False;

  if (AEvents = nil) or (AOrigin = nil) or (ADispatch.Source = nil) or
    not Assigned(ASnapshot) or not ADispatch.Info.HasWheel or
    not ADispatch.Info.Wheel.Defined or (ADispatch.Info.Trigger <> ntWheel) then
  begin
    raise ENyxSchedule.Create('Wheel dispatch requires a mounted typed request');
  end;

  if ADispatch.EventName = '' then
  begin
    Exit;
  end;
  LRevision := AEvents.ViewRevision;
  LDispatch := ADispatch;
  AOrigin.AcquireReference;
  LDispatch.Source.AcquireReference;
  try

    if AEvents.HasSubscribers(ntBeforeWheel) then
    begin
      Capture(ntBeforeWheel);
      Result := DispatchNyxInput(AEvents, AOrigin, LDispatch);
    end;

    if (AEvents.ViewRevision = LRevision) and not Result and
      AEvents.HasSubscribers(ntWheel) then
    begin
      Capture(ntWheel);
      Result := DispatchNyxInput(AEvents, AOrigin, LDispatch);
    end;

    if (AEvents.ViewRevision = LRevision) and AEvents.HasSubscribers(ntAfterWheel) then
    begin
      Capture(ntAfterWheel);
      LDispatch.Info.DefaultPrevented := Result;
      AEvents.Dispatch(LDispatch.Info, AOrigin.DesignID, LDispatch.Source.DesignID);
    end;
    Result := Result or (AEvents.ViewRevision <> LRevision);
  finally
    LDispatch.Source.ReleaseReference;
    AOrigin.ReleaseReference;
  end;
end;

procedure DispatchNyxTextResult(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ADispatch: TNyxDispatch; ASnapshot: TNyxSignalSnapshot);
var
  LInfo: TNyxEventInfo;
  LRevision: Integer;
begin

  if not ADispatch.Info.HasTextEdit or (AEvents = nil) or (AOrigin = nil) or
    (ADispatch.Source = nil) or not Assigned(ASnapshot) then
  begin
    Exit;
  end;

  if not AEvents.HasSubscribers(ntTextInput) and
    not AEvents.HasSubscribers(ntAfterTextInput) then
  begin
    Exit;
  end;
  LInfo := ADispatch.Info.Copy;
  LRevision := AEvents.ViewRevision;
  AOrigin.AcquireReference;
  ADispatch.Source.AcquireReference;
  try

    if not LInfo.DefaultPrevented and AEvents.HasSubscribers(ntTextInput) then
    begin
      LInfo := ASnapshot(AOrigin, ntTextInput);
      LInfo.HasTextEdit := True;
      LInfo.TextEdit := ADispatch.Info.TextEdit;
      LInfo.HasEditing := ADispatch.Info.HasEditing;
      LInfo.Editing := ADispatch.Info.Editing;
      AEvents.Dispatch(LInfo, AOrigin.DesignID, ADispatch.Source.DesignID);
    end;

    if (AEvents.ViewRevision = LRevision) and AEvents.HasSubscribers(ntAfterTextInput) then
    begin
      LInfo := ASnapshot(AOrigin, ntAfterTextInput);
      LInfo.HasTextEdit := True;
      LInfo.TextEdit := ADispatch.Info.TextEdit;
      LInfo.HasEditing := ADispatch.Info.HasEditing;
      LInfo.Editing := ADispatch.Info.Editing;
      LInfo.DefaultPrevented := ADispatch.Info.DefaultPrevented;
      AEvents.Dispatch(LInfo, AOrigin.DesignID, ADispatch.Source.DesignID);
    end;
  finally
    ADispatch.Source.ReleaseReference;
    AOrigin.ReleaseReference;
  end;
end;

function NyxEventResponse(const AExecution: INyxExecution): INyxEventResponse;
begin

  if (AExecution = nil) or not Supports(AExecution, INyxEventResponse, Result) then
  begin
    raise ENyxSchedule.Create('This execution has no physical event response');
  end;
end;

function NyxGestureResponse(const AExecution: INyxExecution): INyxGestureResponse;
begin

  if (AExecution = nil) or not Supports(AExecution, INyxGestureResponse, Result) then
  begin
    raise ENyxSchedule.Create('This execution has no physical gesture response');
  end;
end;

procedure DispatchNyxGesture(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ADispatch: TNyxDispatch; const ADecision: INyxGestureDecision);
begin

  if (AEvents = nil) or (AOrigin = nil) or (ADispatch.Source = nil) or
    (ADecision = nil) then
  begin
    raise ENyxSchedule.Create('Physical gesture dispatch requires an owned decision and live origin');
  end;
  AOrigin.AcquireReference;
  ADispatch.Source.AcquireReference;
  try
    AEvents.DispatchGesture(ADispatch.Info, AOrigin.DesignID,
      ADispatch.Source.DesignID, ADecision);
  finally
    ADecision.Seal;
    ADispatch.Source.ReleaseReference;
    AOrigin.ReleaseReference;
  end;
end;

function TNyxEvents.DispatchGesture(const AEvent: TNyxEventInfo;
  const AOriginDesignID, ASourceDesignID: TNyxText;
  const ADecision: INyxGestureDecision): TNyxExecutions;
begin
  try

    if (ADecision = nil) or
      ((not AEvent.HasPointer) and (not AEvent.HasDrag or not AEvent.Drag.Defined)) then
    begin
      raise ENyxSchedule.Create('Gesture dispatch requires typed physical pointer or drag context');
    end;
    Result := DispatchCore(AEvent, AOriginDesignID, ASourceDesignID, nil, ADecision);
  finally

    if ADecision <> nil then
    begin
      ADecision.Seal;
    end;
  end;
end;

procedure TNyxInputDecision.Consume;
begin
  {$IFDEF PAS2JS}
  FConsumed := 1;
  {$ELSE}
  InterlockedExchange(FConsumed, 1);
  {$ENDIF}
end;

function TNyxInputDecision.Consumed: Boolean;
begin
  {$IFDEF PAS2JS}
  Result := FConsumed <> 0;
  {$ELSE}
  Result := InterlockedCompareExchange(FConsumed, 0, 0) <> 0;
  {$ENDIF}
end;

constructor TNyxEventScope.Create(const AParent: INyxEventScope;
  const ADelivery: INyxCancellationScope);
begin
  inherited Create;
  FParent := AParent;
  FDelivery := ADelivery;
end;

procedure TNyxEventScope.Cancel;
begin
  {$IFDEF PAS2JS}
  FCancelled := 1;
  {$ELSE}
  InterlockedExchange(FCancelled, 1);
  {$ENDIF}
end;

function TNyxEventScope.Cancelled: Boolean;
begin
  {$IFDEF PAS2JS}
  Result := FCancelled <> 0;
  {$ELSE}
  Result := InterlockedCompareExchange(FCancelled, 0, 0) <> 0;
  {$ENDIF}
  Result := Result or ((FParent <> nil) and FParent.Cancelled) or
    ((FDelivery <> nil) and FDelivery.Cancelled);
end;

constructor TNyxEventExecution.Create(const AExecution: INyxExecution;
  const AScope: INyxEventScope; const ASubscriptionScope: INyxEventScope;
  const ADecision: INyxInputDecision; AAllowsConsume: Boolean;
  const AGesture: INyxGestureDecision; AAllowsGesture: Boolean);
begin
  inherited Create;
  FExecution := AExecution;
  FScope := AScope;
  FSubscriptionScope := ASubscriptionScope;
  FDecision := ADecision;
  FAllowsConsume := AAllowsConsume;
  FActive := 1;
  FGesture := AGesture;
  FAllowsGesture := AAllowsGesture;
end;

procedure TNyxEventExecution.SealResponse;
begin
  {$IFDEF PAS2JS}
  FActive := 0;
  {$ELSE}
  InterlockedExchange(FActive, 0);
  {$ENDIF}
end;

function TNyxEventExecution.GetCanConsume: Boolean;
begin
  Result := False;

  if not FAllowsConsume or (FDecision = nil) then
  begin
    Exit;
  end;
  {$IFDEF PAS2JS}
  Result := FActive <> 0;
  {$ELSE}
  Result := (GetCurrentThreadID = MainThreadID) and
    (InterlockedCompareExchange(FActive, 0, 0) <> 0);
  {$ENDIF}
  Result := Result and not GetCancelled;
end;

procedure TNyxEventExecution.Consume;
begin

  if not GetCanConsume then
  begin
    raise ENyxSchedule.Create('Only an active sequential input callback may consume its event');
  end;
  FDecision.Consume;
end;

function TNyxEventExecution.CanRequest(ACapability: TNyxGestureCapability): Boolean;
begin
  Result := False;

  if not FAllowsGesture or (FGesture = nil) then
  begin
    Exit;
  end;
  {$IFDEF PAS2JS}
  Result := FActive <> 0;
  {$ELSE}
  Result := (GetCurrentThreadID = MainThreadID) and
    (InterlockedCompareExchange(FActive, 0, 0) <> 0);
  {$ENDIF}
  Result := Result and not GetCancelled and FGesture.CanRequest(ACapability);
end;

procedure TNyxEventExecution.CapturePointer;
begin

  if not CanRequest(ngcCapturePointer) then
  begin
    raise ENyxSchedule.Create('Pointer capture requires an active sequential physical callback');
  end;
  FGesture.CapturePointer;
end;

procedure TNyxEventExecution.ReleasePointer;
begin

  if not CanRequest(ngcReleasePointer) then
  begin
    raise ENyxSchedule.Create('Pointer release requires an active sequential physical callback');
  end;
  FGesture.ReleasePointer;
end;

procedure TNyxEventExecution.OfferDrag(const ATransfer: TNyxTransferSnapshot;
  AAllowed: TNyxDropOperations);
begin

  if not CanRequest(ngcOfferDrag) then
  begin
    raise ENyxSchedule.Create('Drag offers require an active sequential drag-start callback');
  end;
  FGesture.OfferDrag(ATransfer, AAllowed);
end;

procedure TNyxEventExecution.AcceptDrop(AOperation: TNyxDropOperation);
begin

  if not CanRequest(ngcAcceptDrop) then
  begin
    raise ENyxSchedule.Create('Drop negotiation requires an active sequential physical callback');
  end;
  FGesture.AcceptDrop(AOperation);
end;

function TNyxEventExecution.GetConsumed: Boolean;
begin
  Result := (FDecision <> nil) and FDecision.Consumed;
end;

procedure TNyxEventExecution.Cancel;
begin
  FExecution.Cancel;
end;

function TNyxEventExecution.GetCancelled: Boolean;
begin
  { Retained callback context carries its view/subscription lifetime even after
    the callback returns. PostUI links that context so a later navigation cannot
    publish a stale worker result into another view. Terminal ticket status stays
    immutable; the scope's cancellation request is separately observable. }
  Result := FExecution.Cancelled or FScope.Cancelled or FSubscriptionScope.Cancelled;
end;

function TNyxEventExecution.GetStatus: TNyxExecutionStatus;
begin
  Result := FExecution.Status;

  if (Result = nesPending) and GetCancelled then
  begin
    Result := nesCancelled;
  end;
end;

function TNyxEventExecution.GetFailure: TNyxText;
begin
  Result := FExecution.Failure;
end;

function PendingTickets(const AExecutions: TNyxExecutions): TNyxExecutions;
var
  LIndex: Integer;
  LCount: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AExecutions));
  LCount := 0;
  for LIndex := 0 to Length(AExecutions) - 1 do
  begin

    if AExecutions[LIndex].Status in [nesPending, nesRunning] then
    begin
      Result[LCount] := AExecutions[LIndex];
      Inc(LCount);
    end;
  end;
  SetLength(Result, LCount);
end;

constructor TNyxSubscription.Create(AID: TNyxRegistrationID;
  const ACallback: INyxEventCallback);
begin
  inherited Create;
  FID := AID;
  FScope := TNyxEventScope.Create;
  FCallback := ACallback;
end;

function TNyxSubscription.GetID: TNyxRegistrationID;
begin
  Result := FID;
end;

function TNyxSubscription.GetActive: Boolean;
begin
  Result := not FScope.Cancelled;
end;

procedure TNyxSubscription.Cancel;
var
  LIndex: Integer;
begin
  FScope.Cancel;
  for LIndex := 0 to Length(FExecutions) - 1 do
  begin
    FExecutions[LIndex].Cancel;
  end;
  SetLength(FExecutions, 0);
  FCallback := nil;
end;

procedure TNyxSubscription.Track(const AExecution: INyxExecution);
begin
  FLastExecution := AExecution;

  if not GetActive then
  begin
    AExecution.Cancel;
    Exit;
  end;
  FExecutions := PendingTickets(FExecutions);
  SetLength(FExecutions, Length(FExecutions) + 1);
  FExecutions[High(FExecutions)] := AExecution;
end;

function TNyxSubscription.GetLastExecution: INyxExecution;
begin
  Result := FLastExecution;
end;

procedure TNyxEventGroup.Prune;
var
  LIndex: Integer;
  LCount: Integer;
begin
  LCount := 0;
  for LIndex := 0 to Length(Registrations) - 1 do
  begin

    if Registrations[LIndex].Active then
    begin
      Registrations[LCount] := Registrations[LIndex];
      Entries[LCount] := Entries[LIndex];
      Inc(LCount);
    end;
  end;
  SetLength(Registrations, LCount);
  SetLength(Entries, LCount);
end;

constructor TNyxEventStream.Create(ARouter: TNyxEvents; AGroup: TNyxEventGroup);
begin
  inherited Create;
  FRouter := ARouter;
  FOwner := ARouter;
  FGroup := AGroup;
end;

function TNyxEventStream.Policy(AValue: TNyxExecutionPolicy): INyxEventStream;
begin
  FRouter.Admit;
  FRouter.FScheduler.Admit(AValue);
  FGroup.Policy := AValue;
  Result := Self;
end;

function TNyxEventStream.GetPolicy: TNyxExecutionPolicy;
begin
  FRouter.Admit;
  Result := FGroup.Policy;
end;

function TNyxEventStream.Subscribe(const ACallback: INyxEventCallback): INyxEventSubscription;
var
  LEntry: TNyxSubscription;
begin
  FRouter.Admit;

  if ACallback = nil then
  begin
    raise ENyxSchedule.Create('An event callback is required');
  end;

  if FRouter.FSerial = High(Integer) then
  begin
    raise ENyxSchedule.Create('Event registration identity limit reached');
  end;
  FGroup.Prune;
  Inc(FRouter.FSerial);
  LEntry := TNyxSubscription.Create(TNyxRegistrationID(FRouter.FSerial), ACallback);
  Result := LEntry;
  SetLength(FGroup.Registrations, Length(FGroup.Registrations) + 1);
  SetLength(FGroup.Entries, Length(FGroup.Entries) + 1);
  FGroup.Registrations[High(FGroup.Registrations)] := Result;
  FGroup.Entries[High(FGroup.Entries)] := LEntry;
end;

function TNyxEventStream.GetCount: Integer;
var
  LIndex: Integer;
begin
  FRouter.Admit;
  Result := 0;
  for LIndex := 0 to Length(FGroup.Registrations) - 1 do
  begin

    if FGroup.Registrations[LIndex].Active then
    begin
      Inc(Result);
    end;
  end;
end;

function TNyxEventStream.GetSubscription(AIndex: Integer): INyxEventSubscription;
var
  LIndex: Integer;
  LCount: Integer;
begin
  FRouter.Admit;
  LCount := 0;
  for LIndex := 0 to Length(FGroup.Registrations) - 1 do
  begin

    if FGroup.Registrations[LIndex].Active then
    begin

      if LCount = AIndex then
      begin
        Exit(FGroup.Registrations[LIndex]);
      end;
      Inc(LCount);
    end;
  end;
  raise ENyxSchedule.Create('Event registration index is outside the active list');
end;

constructor TNyxEventWork.Create(const AEvent: TNyxEventInfo;
  const ACallback: INyxEventCallback; const AScope, ASubscriptionScope: INyxEventScope;
  const ADecision: INyxInputDecision; AAllowsConsume: Boolean;
  const AGesture: INyxGestureDecision; AAllowsGesture: Boolean);
begin
  inherited Create;
  FEvent := AEvent.Copy;
  FCallback := ACallback;
  FScope := AScope;
  FSubscriptionScope := ASubscriptionScope;
  FDecision := ADecision;
  FAllowsConsume := AAllowsConsume;
  FGesture := AGesture;
  FAllowsGesture := AAllowsGesture;
end;

procedure TNyxEventWork.Execute(const AExecution: INyxExecution);
var
  LExecution: INyxExecution;
  LContext: TNyxEventExecution;
begin
  LContext := TNyxEventExecution.Create(AExecution, FScope, FSubscriptionScope,
    FDecision, FAllowsConsume, FGesture, FAllowsGesture);
  LExecution := LContext;

  if LExecution.Cancelled then
  begin
    LContext.SealResponse;
    AExecution.Cancel;
    Exit;
  end;
  try
    FCallback.Invoke(FEvent, LExecution);
  finally
    LContext.SealResponse;

    if LExecution.Cancelled then
    begin
      AExecution.Cancel;
    end;
  end;
end;

constructor TNyxEvents.Create(const AScheduler: INyxScheduler);
begin
  inherited Create;
  FScope := TNyxEventScope.Create;
  FScheduler := AScheduler;

  if FScheduler = nil then
  begin
    FScheduler := NewNyxScheduler;
  end;
end;

destructor TNyxEvents.Destroy;
var
  LIndex: Integer;
begin
  Close;
  for LIndex := 0 to Length(FGroups) - 1 do
  begin
    FGroups[LIndex].Free;
  end;
  inherited Destroy;
end;

procedure TNyxEvents.Admit;
begin
  FScheduler.RequireUI;

  if FClosed then
  begin
    raise ENyxSchedule.Create('Event router is closed');
  end;
end;

function SameTarget(const ALeft, ARight: TNyxEventTarget): Boolean;
begin
  Result := (ALeft.ID = ARight.ID) and (ALeft.Identity = ARight.Identity) and
    (ALeft.Role = ARight.Role);
end;

function TNyxEvents.On(const ATarget: TNyxEventTarget;
  ATrigger: TNyxTrigger): INyxEventStream;
begin

  if not NyxIsRuntimeTrigger(ATrigger) then
  begin
    raise ENyxSchedule.Create('Physical registrations require a physical runtime trigger');
  end;
  Result := Stream(ATarget, ATrigger, NyxEvent(''));
end;

function TNyxEvents.OnNamed(const ATarget: TNyxEventTarget;
  const AName: TNyxEventRef): INyxEventStream;
begin

  if Trim(AName.Name) = '' then
  begin
    raise ENyxSchedule.Create('A named event reference is required');
  end;
  Result := Stream(ATarget, ntNamed, NyxNamedEvent(AName.Name));
end;

function TNyxEvents.Stream(const ATarget: TNyxEventTarget;
  ATrigger: TNyxTrigger; const AName: TNyxEventRef): INyxEventStream;
var
  LIndex: Integer;
  LGroup: TNyxEventGroup;
begin
  Admit;

  if ATarget.ID = '' then
  begin
    raise ENyxSchedule.Create('An event target identity is required');
  end;

  if ATrigger in [ntDesignSelect, ntDesignValue] then
  begin
    raise ENyxSchedule.Create('Application registrations require an application trigger');
  end;
  for LIndex := 0 to Length(FGroups) - 1 do
  begin

    if SameTarget(FGroups[LIndex].Target, ATarget) and
      (FGroups[LIndex].Trigger = ATrigger) and
      (FGroups[LIndex].Name.Name = AName.Name) then
    begin
      Exit(TNyxEventStream.Create(Self, FGroups[LIndex]));
    end;
  end;
  LGroup := TNyxEventGroup.Create;
  LGroup.Target := ATarget;
  LGroup.Trigger := ATrigger;
  LGroup.Name := NyxEvent(AName.Name);
  LGroup.Policy := neSequential;
  SetLength(FGroups, Length(FGroups) + 1);
  FGroups[High(FGroups)] := LGroup;
  Result := TNyxEventStream.Create(Self, LGroup);
end;

function TNyxEvents.OnAfterEnter(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntAfterEnter);
end;

function TNyxEvents.OnAfterExit(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntAfterExit);
end;

function TNyxEvents.OnKeyDown(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntKeyDown);
end;

function TNyxEvents.OnBeforeKeyDown(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntBeforeKeyDown);
end;

function TNyxEvents.OnAfterKeyDown(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntAfterKeyDown);
end;

function TNyxEvents.OnKeyPress(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntKeyPress);
end;

function TNyxEvents.OnBeforeKeyPress(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntBeforeKeyPress);
end;

function TNyxEvents.OnAfterKeyPress(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntAfterKeyPress);
end;

function TNyxEvents.OnBeforeKeyUp(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntBeforeKeyUp);
end;

function TNyxEvents.OnAfterKeyUp(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntAfterKeyUp);
end;

function TNyxEvents.OnBeforeTextInput(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntBeforeTextInput);
end;

function TNyxEvents.OnTextInput(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntTextInput);
end;

function TNyxEvents.OnBeforeEdit(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntBeforeEdit);
end;

function TNyxEvents.OnCompositionStart(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntCompositionStart);
end;

function TNyxEvents.OnCompositionUpdate(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntCompositionUpdate);
end;

function TNyxEvents.OnCompositionEnd(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntCompositionEnd);
end;

function TNyxEvents.OnTextSelectionChange(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntTextSelectionChange);
end;

function TNyxEvents.OnAfterTextInput(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntAfterTextInput);
end;

function TNyxEvents.OnDoubleClick(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntDoubleClick);
end;

function TNyxEvents.OnPointerDown(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntPointerDown);
end;

function TNyxEvents.OnPointerUp(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntPointerUp);
end;

function TNyxEvents.OnPointerMove(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntPointerMove);
end;

function TNyxEvents.OnPointerEnter(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntPointerEnter);
end;

function TNyxEvents.OnPointerExit(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntPointerExit);
end;

function TNyxEvents.OnContextMenu(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntContextMenu);
end;

function TNyxEvents.OnPointerCancel(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntPointerCancel);
end;

function TNyxEvents.OnPointerCapture(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntPointerCapture);
end;

function TNyxEvents.OnPointerCaptureLost(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntPointerCaptureLost);
end;

function TNyxEvents.OnDragStart(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntDragStart);
end;

function TNyxEvents.OnDrag(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntDrag);
end;

function TNyxEvents.OnDragEnter(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntDragEnter);
end;

function TNyxEvents.OnDragOver(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntDragOver);
end;

function TNyxEvents.OnDragExit(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntDragExit);
end;

function TNyxEvents.OnDrop(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntDrop);
end;

function TNyxEvents.OnDragEnd(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntDragEnd);
end;

function TNyxEvents.OnBeforeWheel(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntBeforeWheel);
end;

function TNyxEvents.OnWheel(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntWheel);
end;

function TNyxEvents.OnAfterWheel(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntAfterWheel);
end;

function TNyxEvents.OnScroll(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntScroll);
end;

function TNyxEvents.OnScrollEnd(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntScrollEnd);
end;

function TNyxEvents.OnImageLoading(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntImageLoading);
end;

function TNyxEvents.OnImageReady(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntImageReady);
end;

function TNyxEvents.OnImageError(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntImageError);
end;

function TNyxEvents.OnImageCleared(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntImageCleared);
end;

function TNyxEvents.OnSelectionChange(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntSelectionChange);
end;

function TNyxEvents.OnKeyUp(const ATarget: TNyxEventTarget): INyxEventStream;
begin
  Result := On(ATarget, ntKeyUp);
end;

function TNyxEvents.HasSubscribers(ATrigger: TNyxTrigger): Boolean;
var
  LIndex: Integer;
  LEntry: Integer;
begin
  Result := False;

  if FClosed then
  begin
    Exit;
  end;
  for LIndex := 0 to Length(FGroups) - 1 do
  begin

    if (FGroups[LIndex].Trigger = ATrigger) or
      (FGroups[LIndex].Trigger = ntNamed) then
    begin
      for LEntry := 0 to Length(FGroups[LIndex].Registrations) - 1 do
      begin

        if FGroups[LIndex].Registrations[LEntry].Active then
        begin
          Exit(True);
        end;
      end;
    end;
  end;
end;

function Matches(const ATarget: TNyxEventTarget; const AEvent: TNyxEventInfo;
  const AOriginDesignID, ASourceDesignID: TNyxText): Boolean;
var
  LRuntimeID: TNyxText;
  LDesignID: TNyxText;
begin

  if ATarget.Role = nerOrigin then
  begin
    LRuntimeID := AEvent.OriginID;
    LDesignID := AOriginDesignID;
  end
  else
  begin
    LRuntimeID := AEvent.SourceID;
    LDesignID := ASourceDesignID;
  end;
  Result := ((ATarget.Identity <> niDesign) and (ATarget.ID = LRuntimeID)) or
    ((ATarget.Identity <> niRuntime) and (ATarget.ID = LDesignID));
end;

procedure TNyxEvents.PruneExecutions;
begin
  FExecutions := PendingTickets(FExecutions);
end;

function TNyxEvents.DispatchGuarded(const AEvent: TNyxEventInfo;
  const AOriginDesignID, ASourceDesignID: TNyxText;
  const ADelivery: INyxCancellationScope): TNyxExecutions;
begin

  if ADelivery = nil then
  begin
    raise ENyxSchedule.Create('Guarded dispatch requires a cancellation generation');
  end;
  Result := DispatchCore(AEvent, AOriginDesignID, ASourceDesignID, nil, nil, ADelivery);
end;

function TNyxEvents.Dispatch(const AEvent: TNyxEventInfo;
  const AOriginDesignID, ASourceDesignID: TNyxText): TNyxExecutions;
begin
  Result := DispatchCore(AEvent, AOriginDesignID, ASourceDesignID, nil);
end;

function TNyxEvents.DispatchInput(const AEvent: TNyxEventInfo;
  const AOriginDesignID, ASourceDesignID: TNyxText;
  out AConsumed: Boolean): TNyxExecutions;
var
  LDecision: INyxInputDecision;
begin
  AConsumed := False;

  if not NyxIsInputTrigger(AEvent.Trigger) or
    (NyxIsKeyboardTrigger(AEvent.Trigger) and not AEvent.HasKeyboard) or
    ((AEvent.Trigger = ntBeforeTextInput) and not AEvent.HasTextEdit) or
    ((AEvent.Trigger = ntBeforeEdit) and
      (not AEvent.HasEditing or not AEvent.Editing.Defined or
        (AEvent.Editing.Phase <> nepBeforeEdit))) or
    ((AEvent.Trigger in [ntBeforeWheel, ntWheel]) and
      (not AEvent.HasWheel or not AEvent.Wheel.Defined)) then
  begin
    raise ENyxSchedule.Create('Input dispatch requires a supported typed input proposal');
  end;
  LDecision := nil;

  if ((AEvent.Trigger = ntBeforeEdit) and AEvent.Editing.CanCancel) or
    ((AEvent.Trigger in [ntBeforeWheel, ntWheel]) and AEvent.Wheel.CanCancel) or
    not (AEvent.Trigger in [ntBeforeEdit, ntBeforeWheel, ntWheel]) then
  begin
    LDecision := TNyxInputDecision.Create;
  end;
  Result := DispatchCore(AEvent, AOriginDesignID, ASourceDesignID, LDecision);

  if LDecision <> nil then
  begin
    AConsumed := LDecision.Consumed;
  end;
end;

function TNyxEvents.DispatchCore(const AEvent: TNyxEventInfo;
  const AOriginDesignID, ASourceDesignID: TNyxText;
  const ADecision: INyxInputDecision; const AGesture: INyxGestureDecision;
  const ADelivery: INyxCancellationScope): TNyxExecutions;
var
  LKeepAlive: INyxEvents;
  LSnapshot: array of TNyxInvocation;
  LGroup: TNyxEventGroup;
  LIndex: Integer;
  LEntryIndex: Integer;
  LCount: Integer;
  LWork: INyxWork;
  LEvent: TNyxEventInfo;
  LScope: INyxEventScope;
begin
  Admit;
  LKeepAlive := Self;
  LSnapshot := nil;
  Result := nil;

  if AEvent.Name.Name = '' then
  begin
    Exit;
  end;
  LEvent := AEvent.Copy;
  LScope := FScope;

  if ADelivery <> nil then
  begin
    { Link before the first synchronous invocation, not after Submit returns. }
    LScope := TNyxEventScope.Create(FScope, ADelivery);
  end;
  try
    for LIndex := 0 to Length(FGroups) - 1 do
    begin
      LGroup := FGroups[LIndex];

      if (((LGroup.Trigger <> ntNamed) and (LGroup.Trigger = AEvent.Trigger)) or
        ((LGroup.Trigger = ntNamed) and (LGroup.Name.Name = AEvent.Name.Name))) and
        Matches(LGroup.Target, AEvent, AOriginDesignID, ASourceDesignID) then
      begin
        LGroup.Prune;
        for LEntryIndex := 0 to Length(LGroup.Registrations) - 1 do
        begin
          LCount := Length(LSnapshot);
          SetLength(LSnapshot, LCount + 1);
          LSnapshot[LCount] := TNyxInvocation.Create;
          LSnapshot[LCount].Lease := LGroup.Registrations[LEntryIndex];
          LSnapshot[LCount].Entry := LGroup.Entries[LEntryIndex];
          LSnapshot[LCount].Callback := LGroup.Entries[LEntryIndex].Callback;
          LSnapshot[LCount].Policy := LGroup.Policy;
        end;
      end;
    end;
    SetLength(Result, Length(LSnapshot));
    for LIndex := 0 to Length(LSnapshot) - 1 do
    begin
      LWork := TNyxEventWork.Create(LEvent, LSnapshot[LIndex].Callback,
        LScope, LSnapshot[LIndex].Entry.Scope, ADecision,
        (ADecision <> nil) and (LSnapshot[LIndex].Policy = neSequential), AGesture,
        (AGesture <> nil) and (LSnapshot[LIndex].Policy = neSequential));
      try
        Result[LIndex] := FScheduler.Submit(LWork, LSnapshot[LIndex].Policy);
      except
        on LException: ENyxScheduleCapacity do
        begin
          { Native backpressure belongs to this invocation's diagnostic. Retain
            LastExecution and continue independent siblings, including UI-only
            registrations. Neither run overload on UI nor hide it as success. }
          {$IFDEF PAS2JS}
          Result[LIndex] := NewNyxFailedExecution(LException.Message);
          {$ELSE}
          Result[LIndex] := NewNyxFailedExecution(UTF8Encode(UnicodeString(LException.Message)));
          {$ENDIF}
        end;
      end;
      LSnapshot[LIndex].Entry.Track(Result[LIndex]);

      if FClosed then
      begin
        Result[LIndex].Cancel;
      end
      else
      begin
        PruneExecutions;
        SetLength(FExecutions, Length(FExecutions) + 1);
        FExecutions[High(FExecutions)] := Result[LIndex];
      end;
    end;
  finally
    for LIndex := 0 to Length(LSnapshot) - 1 do
    begin
      LSnapshot[LIndex].Free;
    end;
  end;
end;

procedure TNyxEvents.CancelPending;
var
  LIndex: Integer;
begin
  FScope.Cancel;
  FScope := TNyxEventScope.Create;

  if FViewRevision = High(Integer) then
  begin
    FViewRevision := 0;
  end
  else
  begin
    Inc(FViewRevision);
  end;
  for LIndex := 0 to Length(FExecutions) - 1 do
  begin
    FExecutions[LIndex].Cancel;
  end;
  SetLength(FExecutions, 0);
end;

procedure TNyxEvents.Close;
var
  LIndex: Integer;
  LEntryIndex: Integer;
begin

  if FClosed then
  begin
    Exit;
  end;
  FClosed := True;
  CancelPending;
  for LIndex := 0 to Length(FGroups) - 1 do
  begin
    for LEntryIndex := 0 to Length(FGroups[LIndex].Registrations) - 1 do
    begin
      FGroups[LIndex].Registrations[LEntryIndex].Cancel;
    end;
    FGroups[LIndex].Prune;
  end;
end;

function TNyxEvents.GetScheduler: INyxScheduler;
begin
  Result := FScheduler;
end;

function TNyxEvents.GetViewRevision: Integer;
begin
  Result := FViewRevision;
end;

function NyxControlEvents(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TNyxEventTarget;
begin

  if AID = '' then
  begin
    raise ENyxSchedule.Create('An event target identity is required');
  end;
  Result.FID := AID;
  Result.FIdentity := AIdentity;
  Result.FRole := nerOrigin;
end;

function NyxCompoundEvents(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TNyxEventTarget;
begin
  Result := NyxControlEvents(AID, AIdentity);
  Result.FRole := nerSource;
end;

function NewNyxEvents(const AScheduler: INyxScheduler): INyxEvents;
begin
  Result := TNyxEvents.Create(AScheduler);
end;

end.
