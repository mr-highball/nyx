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
unit nyx.gestures.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Classes, SysUtils, Controls, LMessages, nyx.text, nyx.types, nyx.events,
  nyx.gestures, nyx.observation;

type
  { Hooks and direct drag slots retain this frame on their stack. A retiring view
    transfers revoked controls into its idle release queue; they retain no
    renderer/model callback links. Native drags are canceled after construction
    and the current message finish, before any retired control is freed. }
  INyxNativeGestureFrame = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001008000003}']
    procedure Enter;
    procedure Leave;
    function Dispatching: Boolean;
    procedure Retire(AComponent: TComponent);
    { LCL cannot cancel from OnStartDrag while its performer is constructing.
      Queue a weak source reference, checked at the next idle boundary. }
    procedure AbortDrag(AControl: TControl);
  end;
  TNyxLCLCaptureHandler = procedure(const AOriginID: TNyxText;
    ATrigger: TNyxTrigger) of object;

  { Borrowed pointer-capture and physical-focus observer. Notification sinks are
    explicitly revoked before controls or their model are freed. It chains the
    widget's original procedure. Attach after editing/viewport hooks and detach
    before them so nested native procedure chains preserve their lifetimes. }
  INyxLCLCaptureObserver = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001008000004}']
    { Observe focus only on the binding's real keyboard surface, never both its
      grouped frame and editor. Logical LCL Enter/Exit alone miss transitions
      between native top-level windows that retain the same active control. }
    procedure Add(const AOriginID: TNyxText; AControl: TControl;
      AObserveFocus: Boolean = False);
    procedure Activate(const AEvents: INyxEvents; AHandler: TNyxLCLCaptureHandler);
    procedure Observe(AControl: TControl);
    procedure Disconnect;
  end;

type
  { Flags-only native incarnation, without a widget/model backreference. Epoch
    changes revoke an old button stream across Ready ABA. Getters must not call
    application code; the source adapter owns and retires these flags. }
  INyxLCLCaptureInput = interface(IInterface)
    ['{CBF44153-7D91-437D-AD48-CBEBD659F3AB}']
    function GetCurrent: Boolean;
    function GetReady: Boolean;
    function GetEpoch: Integer;
    property Current: Boolean read GetCurrent;
    property Ready: Boolean read GetReady;
    property Epoch: Integer read GetEpoch;
  end;
  { Optional source-scoped admission preserves the original observer ABI. Add
    before activation and after inner editing hooks. Only flags are retained. }
  INyxLCLScopedCaptureObserver = interface(IInterface)
    ['{9544AB34-9808-4DDE-8109-368EDC77855E}']
    procedure AddScoped(const AOriginID: TNyxText; AControl: TControl;
      const AInput: INyxLCLCaptureInput; AObserveFocus: Boolean = False);
  end;

function NewNyxNativeGestureFrame: INyxNativeGestureFrame;
function NewNyxLCLCaptureObserver(const AFrame: INyxNativeGestureFrame): INyxLCLCaptureObserver;
function NyxLCLHasPointerCapture(AControl: TControl): Boolean;
procedure CaptureNyxLCLPointer(AControl: TControl);
procedure ReleaseNyxLCLPointer(AControl: TControl);

implementation

uses Forms{$ifdef NYX_FOCUS_TRACE}, LCLIntf{$endif};

type
  TNyxControlAccess = class(TControl);
  { FreeNotification protects a deferred cancellation when the application
    destroys the source before idle. This component never owns the control. }
  TNyxDragAbort = class(TComponent)
  private
    FControl: TControl;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor CreateFor(AControl: TControl);
    destructor Destroy; override;
    procedure Cancel;
  end;
  TNyxNativeGestureFrame = class(TInterfacedObject, INyxNativeGestureFrame)
  private
    FDepth: Integer;
    FRetired: array of TComponent;
    FAborts: array of TNyxDragAbort;
    FRetirementHost: TForm;
    FRetirementLease: INyxNativeGestureFrame;
    FAsyncScheduled: Boolean;
    procedure QueueRelease;
    procedure ReleaseDeferred(AData: PtrInt);
    procedure ReleaseIdle(ASender: TObject; var ADone: Boolean);
  public
    procedure Enter;
    procedure Leave;
    function Dispatching: Boolean;
    procedure Retire(AComponent: TComponent);
    procedure AbortDrag(AControl: TControl);
  end;
  INyxLCLCaptureHook = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001008000005}']
    procedure Connect(const AEvents: INyxEvents; AHandler: TNyxLCLCaptureHandler);
    procedure Disconnect;
    procedure Observe;
    procedure PublishRevision(ARevision: Integer);
    function Matches(AControl: TControl): Boolean;
  end;
  { One revocable native window-procedure chain borrows its control and retains
    the physical frame/router only while connected. Nested capture observations
    during button-up/cancellation wait for their native defaults. Cancellation
    retains its original button epoch/revision through callbacks and can never
    borrow a newly published view or cancel a callback's new pointer stream. }
  TNyxLCLCaptureHook = class(TInterfacedObject, INyxLCLCaptureHook)
  private
    FControl: TControl;
    FOriginID: TNyxText;
    FPrevious: TWndMethod;
    FFrame: INyxNativeGestureFrame;
    FEvents: INyxEvents;
    FRevision: Integer;
    FHandler: TNyxLCLCaptureHandler;
    FConnected: Boolean;
    FCaptured: Boolean;
    FObserveFocus: Boolean;
    FButtons: TNyxPointerButtons;
    FInput: INyxLCLCaptureInput;
    FButtonEpoch: Integer;
    FButtonRevision: Integer;
    FReleaseDepth: Integer;
    procedure Handle(var AMessage: TLMessage);
    procedure Notify(ATrigger: TNyxTrigger; AInputEpoch: Integer = -1;
      ARevision: Integer = -1);
    function InputCurrent(AInputEpoch: Integer = -1): Boolean;
  public
    constructor Create(const AOriginID: TNyxText; AControl: TControl;
      const AFrame: INyxNativeGestureFrame; AObserveFocus: Boolean;
      const AInput: INyxLCLCaptureInput = nil);
    destructor Destroy; override;
    procedure Connect(const AEvents: INyxEvents; AHandler: TNyxLCLCaptureHandler);
    procedure Disconnect;
    procedure Observe;
    procedure PublishRevision(ARevision: Integer);
    function Matches(AControl: TControl): Boolean;
  end;
  { Owns a bounded control set and its idle observation. Scoped flags add input
    authority without retaining a model. Disconnect precedes inner-hook release. }
  TNyxLCLCaptureObserver = class(TInterfacedObject, INyxLCLCaptureObserver,
    INyxObservationPublication, INyxLCLScopedCaptureObserver)
  private
    FHooks: array of INyxLCLCaptureHook;
    FFrame: INyxNativeGestureFrame;
    FActive: Boolean;
    procedure Idle(ASender: TObject; var ADone: Boolean);
  public
    constructor Create(const AFrame: INyxNativeGestureFrame);
    destructor Destroy; override;
    procedure Add(const AOriginID: TNyxText; AControl: TControl;
      AObserveFocus: Boolean = False);
    procedure AddScoped(const AOriginID: TNyxText; AControl: TControl;
      const AInput: INyxLCLCaptureInput; AObserveFocus: Boolean = False);
    procedure Activate(const AEvents: INyxEvents; AHandler: TNyxLCLCaptureHandler);
    procedure Observe(AControl: TControl);
    procedure Disconnect;
    function GetReady: Boolean;
    procedure PublishRevision(ARevision: Integer);
  end;

constructor TNyxDragAbort.CreateFor(AControl: TControl);
begin
  inherited Create(nil);
  FControl := AControl;
  FControl.FreeNotification(Self);
end;

destructor TNyxDragAbort.Destroy;
begin

  if FControl <> nil then
  begin
    FControl.RemoveFreeNotification(Self);
  end;
  inherited Destroy;
end;

procedure TNyxDragAbort.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);

  if (AOperation = opRemove) and (AComponent = FControl) then
  begin
    FControl := nil;
  end;
end;

procedure TNyxDragAbort.Cancel;
begin

  if (FControl <> nil) and FControl.Dragging then
  begin
    FControl.EndDrag(False);
  end;
end;

procedure TNyxNativeGestureFrame.Enter;
begin

  if GetCurrentThreadID <> MainThreadID then
  begin
    raise ENyxGesture.Create('Native physical frames require UI-thread access');
  end;
  Inc(FDepth);
end;

procedure TNyxNativeGestureFrame.Leave;
begin

  if FDepth <= 0 then
  begin
    raise ENyxGesture.Create('Native physical frame depth is unbalanced');
  end;
  Dec(FDepth);

  if (FDepth = 0) and (FRetirementLease <> nil) then
  begin
    QueueRelease;
  end;
end;

procedure TNyxNativeGestureFrame.QueueRelease;
begin

  if not FAsyncScheduled then
  begin
    FAsyncScheduled := True;
    Application.QueueAsyncCall(ReleaseDeferred, 0);
  end;
end;

procedure TNyxNativeGestureFrame.ReleaseDeferred(AData: PtrInt);
var
  LDone: Boolean;
  LKeepAlive: INyxNativeGestureFrame;
begin
  LKeepAlive := Self;
  FAsyncScheduled := False;

  if Dispatching then
  begin
    { Leave queues the next attempt after the outermost frame unwinds. Avoid
      spinning inside a callback that pumps native messages recursively. }
    Exit;
  end;
  LDone := True;
  ReleaseIdle(nil, LDone);
end;

function TNyxNativeGestureFrame.Dispatching: Boolean;
begin
  Result := FDepth > 0;
end;

function NewNyxNativeGestureFrame: INyxNativeGestureFrame;
begin
  Result := TNyxNativeGestureFrame.Create;
end;

procedure CancelRetiredDrag(AControl: TControl);
var
  LIndex: Integer;
begin

  if AControl is TWinControl then
  begin
    for LIndex := 0 to TWinControl(AControl).ControlCount - 1 do
    begin
      CancelRetiredDrag(TWinControl(AControl).Controls[LIndex]);
    end;
  end;

  if AControl.Dragging then
  begin
    AControl.EndDrag(False);
  end;
end;

procedure TNyxNativeGestureFrame.Retire(AComponent: TComponent);
begin

  if AComponent = nil then
  begin
    Exit;
  end;
  { Transfer lifetime out of the borrowed host. A caller may close its form
    after unmounting; it must not free this queued control behind the frame. }

  if AComponent.Owner <> nil then
  begin
    AComponent.Owner.RemoveComponent(AComponent);
  end;

  if AComponent is TControl then
  begin
    { A pending drag constructor still acquires a native source handle after
      OnStartDrag returns. Keep a valid, hidden parent independently of the
      caller's form; a parentless panel would make that native step fail. }

    if FRetirementHost = nil then
    begin
      FRetirementHost := TForm.CreateNew(nil);
      FRetirementHost.HandleNeeded;
    end;
    TControl(AComponent).Parent := FRetirementHost;
  end;
  SetLength(FRetired, Length(FRetired) + 1);
  FRetired[High(FRetired)] := AComponent;

  if FRetirementLease = nil then
  begin
    FRetirementLease := Self;
    Application.AddOnIdleHandler(ReleaseIdle);
    QueueRelease;
  end;
end;

procedure TNyxNativeGestureFrame.AbortDrag(AControl: TControl);
var
  LIndex: Integer;
begin

  if AControl = nil then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FAborts) do
  begin

    if FAborts[LIndex].FControl = AControl then
    begin
      Exit;
    end;
  end;
  SetLength(FAborts, Length(FAborts) + 1);
  FAborts[High(FAborts)] := TNyxDragAbort.CreateFor(AControl);

  if FRetirementLease = nil then
  begin
    FRetirementLease := Self;
    Application.AddOnIdleHandler(ReleaseIdle);
    QueueRelease;
  end;
end;

procedure TNyxNativeGestureFrame.ReleaseIdle(ASender: TObject; var ADone: Boolean);
var
  LKeepAlive: INyxNativeGestureFrame;
  LRetired: array of TComponent;
  LAborts: array of TNyxDragAbort;
  LIndex: Integer;
begin
  LKeepAlive := Self;

  if Dispatching then
  begin
    Exit;
  end;
  Application.RemoveOnIdleHandler(ReleaseIdle);
  Application.RemoveAsyncCalls(Self);
  FAsyncScheduled := False;
  LRetired := FRetired;
  FRetired := nil;
  LAborts := FAborts;
  FAborts := nil;
  try
    for LIndex := 0 to High(LAborts) do
    begin
      LAborts[LIndex].Cancel;
    end;
    for LIndex := 0 to High(LRetired) do
    begin

      if LRetired[LIndex] is TControl then
      begin
        CancelRetiredDrag(TControl(LRetired[LIndex]));
      end;
    end;
  finally
    { Every queued owner is released even when an external native handler fails.
      Reentrant callbacks may enqueue a new batch; keep its separate idle lease. }
    for LIndex := 0 to High(LAborts) do
    begin
      LAborts[LIndex].Free;
    end;
    for LIndex := 0 to High(LRetired) do
    begin
      LRetired[LIndex].Free;
    end;

    if (Length(FRetired) = 0) and (Length(FAborts) = 0) then
    begin
      FreeAndNil(FRetirementHost);
      FRetirementLease := nil;
    end
    else
    begin
      Application.AddOnIdleHandler(ReleaseIdle);
      QueueRelease;
    end;
  end;
end;

function NyxLCLHasPointerCapture(AControl: TControl): Boolean;
begin
  Result := (AControl <> nil) and TNyxControlAccess(AControl).MouseCapture;
end;

procedure CaptureNyxLCLPointer(AControl: TControl);
begin

  if AControl = nil then
  begin
    raise ENyxGesture.Create('Native capture requires a mounted control');
  end;
  TNyxControlAccess(AControl).MouseCapture := True;

  if not NyxLCLHasPointerCapture(AControl) then
  begin
    raise ENyxGesture.Create('The native widget refused mouse capture');
  end;
end;

procedure ReleaseNyxLCLPointer(AControl: TControl);
begin

  if NyxLCLHasPointerCapture(AControl) then
  begin
    TNyxControlAccess(AControl).MouseCapture := False;
  end;
end;

constructor TNyxLCLCaptureHook.Create(const AOriginID: TNyxText;
  AControl: TControl; const AFrame: INyxNativeGestureFrame; AObserveFocus: Boolean;
  const AInput: INyxLCLCaptureInput);
begin
  inherited Create;

  if (AControl = nil) or (AFrame = nil) then
  begin
    raise ENyxGesture.Create('Native capture hook requires a control and frame');
  end;
  FControl := AControl;
  FOriginID := AOriginID;
  FObserveFocus := AObserveFocus;
  FInput := AInput;
  FFrame := AFrame;
  FPrevious := AControl.WindowProc;
  FCaptured := NyxLCLHasPointerCapture(AControl);
  AControl.WindowProc := Handle;
end;

destructor TNyxLCLCaptureHook.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TNyxLCLCaptureHook.Connect(const AEvents: INyxEvents;
  AHandler: TNyxLCLCaptureHandler);
begin
  FEvents := AEvents;
  FRevision := AEvents.ViewRevision;
  FHandler := AHandler;
  FConnected := True;
end;

procedure TNyxLCLCaptureHook.PublishRevision(ARevision: Integer);
begin
  FRevision := ARevision;
end;

procedure TNyxLCLCaptureHook.Disconnect;
var
  LCurrent: TWndMethod;
  LOwn: TWndMethod;
begin
  FConnected := False;
  FHandler := nil;
  FEvents := nil;
  FInput := nil;
  FButtons := [];

  if FControl <> nil then
  begin
    LCurrent := FControl.WindowProc;
    LOwn := Handle;

    if (TMethod(LCurrent).Code = TMethod(LOwn).Code) and
      (TMethod(LCurrent).Data = TMethod(LOwn).Data) then
    begin
      FControl.WindowProc := FPrevious;
    end;
  end;
  FControl := nil;
  FPrevious := nil;
end;

function TNyxLCLCaptureHook.Matches(AControl: TControl): Boolean;
begin
  Result := FControl = AControl;
end;

function TNyxLCLCaptureHook.InputCurrent(AInputEpoch: Integer): Boolean;
begin
  Result := (FInput = nil) or (FInput.Current and FInput.Ready and
    ((AInputEpoch < 0) or (FInput.Epoch = AInputEpoch)));
end;

procedure TNyxLCLCaptureHook.Notify(ATrigger: TNyxTrigger; AInputEpoch: Integer;
  ARevision: Integer);
var
  LHandler: TNyxLCLCaptureHandler;
  LOriginID: TNyxText;
  LEvents: INyxEvents;
  LRevision: Integer;
begin
  LEvents := FEvents;
  LRevision := FRevision;
  LHandler := FHandler;
  LOriginID := FOriginID;

  if not FConnected or (LEvents = nil) or not Assigned(LHandler) or
    not InputCurrent(AInputEpoch) or ((ARevision >= 0) and (LRevision <> ARevision)) then
  begin
    Exit;
  end;

  if (LEvents.ViewRevision <> LRevision) or not FConnected or
    (FEvents <> LEvents) or (FRevision <> LRevision) or not InputCurrent(AInputEpoch) then
  begin
    Exit;
  end;
  LHandler(LOriginID, ATrigger);
end;

procedure TNyxLCLCaptureHook.Observe;
var
  LCaptured: Boolean;
  LKeepAlive: INyxLCLCaptureHook;
  LFrame: INyxNativeGestureFrame;
begin
  LKeepAlive := Self;
  LFrame := FFrame;

  if not FConnected or (FControl = nil) then
  begin
    Exit;
  end;
  LCaptured := NyxLCLHasPointerCapture(FControl);

  if not LCaptured and (FReleaseDepth > 0) then
  begin
    { LCL releases automatic mouse capture before calling OnMouseUp. Preserve
      portable Up -> capture-loss order across its nested native messages. }
    Exit;
  end;

  if LCaptured = FCaptured then
  begin
    Exit;
  end;
  FCaptured := LCaptured;
  LFrame.Enter;
  try

    if LCaptured then
    begin
      Notify(ntPointerCapture);
    end
    else
    begin
      Notify(ntPointerCaptureLost);
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxLCLCaptureHook.Handle(var AMessage: TLMessage);
var
  LKeepAlive: INyxLCLCaptureHook;
  LFrame: INyxNativeGestureFrame;
  LPrevious: TWndMethod;
  LMessageID: Cardinal;
  LReleasing: Boolean;
  LCancel: Boolean;
  LCancelEpoch: Integer;
  LCancelRevision: Integer;
  LAcceptDown: Boolean;
begin
  LKeepAlive := Self;
  LFrame := FFrame;
  LPrevious := FPrevious;
  LMessageID := AMessage.Msg;
  LCancel := (LMessageID = LM_CANCELMODE) and (FButtons <> []) and
    InputCurrent(FButtonEpoch) and (FButtonRevision = FRevision);
  LCancelEpoch := FButtonEpoch;
  LCancelRevision := FButtonRevision;
  LReleasing := (LMessageID = LM_LBUTTONUP) or
    (LMessageID = LM_RBUTTONUP) or (LMessageID = LM_MBUTTONUP) or
    (LMessageID = LM_CANCELMODE);

  if LReleasing then
  begin
    Inc(FReleaseDepth);
  end;
  { Optional diagnostics distinguish native messages from logical slot delivery.
    They sample handles only; no widget allocation or focus assignment occurs. }
  {$ifdef NYX_FOCUS_TRACE}

  if FObserveFocus and ((LMessageID = LM_SETFOCUS) or (LMessageID = LM_KILLFOCUS)) then
  begin
    WriteLn('FOCUS begin ', FOriginID, ' message=', LMessageID,
      ' other=', AMessage.WParam, ' native=', LCLIntf.GetFocus);
  end;
  {$endif}
  LFrame.Enter;
  try
    LAcceptDown := InputCurrent;

    if (LMessageID = LM_LBUTTONDOWN) or (LMessageID = LM_RBUTTONDOWN) or
      (LMessageID = LM_MBUTTONDOWN) then
    begin

      if not LAcceptDown or (FButtonRevision <> FRevision) or
        ((FInput <> nil) and (FButtonEpoch <> FInput.Epoch)) then
      begin
        FButtons := [];
      end;

      if LAcceptDown then
      begin
        FButtonRevision := FRevision;

        if FInput <> nil then
        begin
          FButtonEpoch := FInput.Epoch;
        end;
      end;
    end;
    case LMessageID of
      LM_LBUTTONDOWN:
        begin

          if LAcceptDown then
          begin
            Include(FButtons, npbPrimary);
          end;
        end;
      LM_RBUTTONDOWN:
        begin

          if LAcceptDown then
          begin
            Include(FButtons, npbSecondary);
          end;
        end;
      LM_MBUTTONDOWN:
        begin

          if LAcceptDown then
          begin
            Include(FButtons, npbAuxiliary);
          end;
        end;
      LM_LBUTTONUP:
        begin
          Exclude(FButtons, npbPrimary);
        end;
      LM_RBUTTONUP:
        begin
          Exclude(FButtons, npbSecondary);
        end;
      LM_MBUTTONUP:
        begin
          Exclude(FButtons, npbAuxiliary);
        end;
    end;
    { Native defaults withdraw the OLD stream before application observation.
      Its capture-loss observation waits so Cancel still precedes Lost. A new
      press from the cancel callback must survive the completed native default. }

    if LMessageID = LM_CANCELMODE then
    begin
      FButtons := [];
    end;

    if Assigned(LPrevious) then
    begin
      LPrevious(AMessage);
    end;

    if LReleasing then
    begin
      Dec(FReleaseDepth);
      LReleasing := False;
    end;

    if LCancel then
    begin
      Notify(ntPointerCancel, LCancelEpoch, LCancelRevision);
    end;
    { Observe after the native/default handler and its editing completion. The
      router epoch refuses a retired view; the retained physical frame protects
      controls through a callback that unmounts that same view. Renderer-side
      transition state deduplicates these messages against logical LCL slots. }

    if FObserveFocus then
    begin
      {$ifdef NYX_FOCUS_TRACE}

      if (LMessageID = LM_SETFOCUS) or (LMessageID = LM_KILLFOCUS) then
      begin
        WriteLn('FOCUS end ', FOriginID, ' message=', LMessageID,
          ' native=', LCLIntf.GetFocus);
      end;
      {$endif}
      case LMessageID of
        LM_SETFOCUS:
          begin
            Notify(ntAfterEnter);
          end;
        LM_KILLFOCUS:
          begin
            Notify(ntAfterExit);
          end;
      end;
    end;
    Observe;
  finally

    if LReleasing then
    begin
      Dec(FReleaseDepth);
    end;
    LFrame.Leave;
  end;
end;

constructor TNyxLCLCaptureObserver.Create(const AFrame: INyxNativeGestureFrame);
begin
  inherited Create;
  FFrame := AFrame;
end;

destructor TNyxLCLCaptureObserver.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TNyxLCLCaptureObserver.Add(const AOriginID: TNyxText; AControl: TControl;
  AObserveFocus: Boolean);
begin
  AddScoped(AOriginID, AControl, nil, AObserveFocus);
end;

procedure TNyxLCLCaptureObserver.AddScoped(const AOriginID: TNyxText;
  AControl: TControl; const AInput: INyxLCLCaptureInput; AObserveFocus: Boolean);
var
  LIndex: Integer;
begin

  if FActive then
  begin
    raise ENyxGesture.Create('Capture controls must be admitted before observer activation');
  end;
  for LIndex := 0 to High(FHooks) do
  begin

    if FHooks[LIndex].Matches(AControl) then
    begin
      Exit;
    end;
  end;
  SetLength(FHooks, Length(FHooks) + 1);
  FHooks[High(FHooks)] := TNyxLCLCaptureHook.Create(AOriginID, AControl, FFrame,
    AObserveFocus, AInput);
end;

procedure TNyxLCLCaptureObserver.Activate(const AEvents: INyxEvents;
  AHandler: TNyxLCLCaptureHandler);
var
  LIndex: Integer;
begin

  if FActive or (AEvents = nil) then
  begin
    raise ENyxGesture.Create('Capture observer requires an inactive view and event router');
  end;
  for LIndex := 0 to High(FHooks) do
  begin
    FHooks[LIndex].Connect(AEvents, AHandler);
  end;
  FActive := True;
  Application.AddOnIdleHandler(Idle);
end;

procedure TNyxLCLCaptureObserver.Observe(AControl: TControl);
var
  LHook: INyxLCLCaptureHook;
  LIndex: Integer;
begin
  for LIndex := 0 to High(FHooks) do
  begin

    if FHooks[LIndex].Matches(AControl) then
    begin
      LHook := FHooks[LIndex];
      LHook.Observe;
      Exit;
    end;
  end;
end;

function TNyxLCLCaptureObserver.GetReady: Boolean;
begin
  Result := FActive;
end;

procedure TNyxLCLCaptureObserver.PublishRevision(ARevision: Integer);
var
  LIndex: Integer;
begin
  { No reconnection or target callback belongs in the commit phase. The installed
    outermost capture hooks simply adopt the now-accepted event-router epoch. }
  for LIndex := 0 to High(FHooks) do
  begin
    FHooks[LIndex].PublishRevision(ARevision);
  end;
end;

procedure TNyxLCLCaptureObserver.Disconnect;
var
  LIndex: Integer;
begin

  if FActive then
  begin
    Application.RemoveOnIdleHandler(Idle);
  end;
  FActive := False;
  for LIndex := High(FHooks) downto 0 do
  begin
    FHooks[LIndex].Disconnect;
  end;
  FHooks := nil;
end;

procedure TNyxLCLCaptureObserver.Idle(ASender: TObject; var ADone: Boolean);
var
  LKeepAlive: INyxLCLCaptureObserver;
  LHooks: array of INyxLCLCaptureHook;
  LIndex: Integer;
begin
  LKeepAlive := Self;
  LHooks := Copy(FHooks);
  for LIndex := 0 to High(LHooks) do
  begin

    if not FActive then
    begin
      Exit;
    end;
    LHooks[LIndex].Observe;
  end;
end;

function NewNyxLCLCaptureObserver(const AFrame: INyxNativeGestureFrame): INyxLCLCaptureObserver;
begin
  Result := TNyxLCLCaptureObserver.Create(AFrame);
end;

end.
