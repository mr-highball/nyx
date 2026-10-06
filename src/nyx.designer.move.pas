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
unit nyx.designer.move;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.behavior, nyx.events, nyx.scheduler,
  nyx.gestures, nyx.designer.guides, nyx.designer.resize, nyx.layout.constraints;

type
  TNyxMovePhase = (nmpPreview, nmpCommit, nmpCancel);
  TNyxPositionSnap = (npsUnsnapped, npsGrid);

  { Exact authored absolute origin in logical pixels. Default is absent;
    NyxMovePosition admits explicit zero and the portable 0..100000 domain.
    Guides explain transient feedback only and never participate in equality. }
  TNyxMovePosition = record
  private
    FDefined: Boolean;
    FLeft, FTop: Integer;
    FHorizontalGuide, FVerticalGuide: TNyxAlignmentGuide;
  public
    function SamePosition(const AOther: TNyxMovePosition): Boolean;
    property Defined: Boolean read FDefined;
    property Left: Integer read FLeft;
    property Top: Integer read FTop;
    property HorizontalGuide: TNyxAlignmentGuide read FHorizontalGuide;
    property VerticalGuide: TNyxAlignmentGuide read FVerticalGuide;
  end;

  { Copied fluent policy, independent of widgets and documents. Nearest edge/
    center guides precede the grid; bounds clamp the resulting origin. Alt
    bypasses both snapping sources. A zero delta preserves that exact axis;
    keyboard movement bypasses grid/guides so an exact step never traps navigation.
    Builders validate a copy before returning and never modify retained policy. }
  TNyxMovePolicy = record
  private
    FSnap: TNyxPositionSnap;
    FGrid, FKeyboardStep: Integer;
    FHorizontalBounds, FVerticalBounds: TNyxSizeRange;
    FGuides: TNyxAlignmentContext;
  public
    function Snap(AValue: TNyxPositionSnap): TNyxMovePolicy;
    function Grid(AValue: Integer): TNyxMovePolicy;
    function KeyboardStep(AValue: Integer): TNyxMovePolicy;
    function Bounds(const AHorizontal, AVertical: TNyxSizeRange): TNyxMovePolicy;
    function Guides(const AValue: TNyxAlignmentContext): TNyxMovePolicy;
    procedure Validate;
    function Adjust(const AStart: TNyxMovePosition; AX, AY: Double;
      ABypassSnap: Boolean = False; ABypassGuides: Boolean = False): TNyxMovePosition;
    property KeyStep: Integer read FKeyboardStep;
  end;

  { Synchronous borrowed receivers, alive until Disconnect/Destroy. Capture
    must return copied accepted position/policy; false refuses a gesture without
    mutation. Feedback previews, commits once or cancels. Neither receiver may
    destroy this executing handle. Hosts own admission/history and geometry. }
  TNyxMoveCapture = function(out APosition: TNyxMovePosition;
    out APolicy: TNyxMovePolicy): Boolean of object;
  TNyxMoveFeedback = procedure(APhase: TNyxMovePhase;
    const APosition: TNyxMovePosition) of object;
  { Map copied grip-local input into one stable logical plane for the entire
    gesture, including when the grip/viewport moves. Nil means local coordinates
    and is suitable only for a stationary unscaled grip. Undefined points refuse. }
  TNyxMovePointerMap = function(const APointer: TNyxPointerSnapshot): TNyxResizePoint of object;
  TNyxMoveRetired = procedure of object;

  { Reusable public behavior for a specialized Nyx button. Pointer capture,
    matching identity, cancellation, Escape and arrow keys use sequential Nyx
    streams. It retains scopes/subscriptions, never widgets or a design tree.
    Disconnect detaches borrowed receivers and caller-retained callbacks before
    subscription retirement. An external Cancel preserves a capture lease until
    its adapter delivers release/lost capture; it does not invent a host event. }
  TNyxMoveHandle = class
  private
    FEvents: INyxEvents;
    FRevision: Integer;
    FCallback: INyxEventCallback;
    FSubscriptions: array of INyxEventSubscription;
    FCapture: TNyxMoveCapture;
    FFeedback: TNyxMoveFeedback;
    FPointerMap: TNyxMovePointerMap;
    FPolicy: TNyxMovePolicy;
    FStart, FCurrent: TNyxMovePosition;
    FStartPoint: TNyxResizePoint;
    FPointer: TNyxPointerSnapshot;
    FDragging, FCaptured: Boolean;
    function BeginChange: Boolean;
    function Point(const APointer: TNyxPointerSnapshot): TNyxResizePoint;
    procedure Finish;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
  public
    constructor Create(const AEvents: INyxEvents; const AGrip: TNyxControlRef;
      ACapture: TNyxMoveCapture; AFeedback: TNyxMoveFeedback;
      APointerMap: TNyxMovePointerMap = nil);
    destructor Destroy; override;
    procedure Cancel;
    procedure Disconnect;
    property Dragging: Boolean read FDragging;
  end;

  { Managed canvas adornment owns its independent one-button Nyx document.
    Adapters retain it while borrowing that tree. Unbind silently retires input
    before view destruction; Disconnect also detaches borrowed editor receivers.
    Retirement must revoke leases without painting or remounting the editor. }
  INyxCanvasMoveGrip = interface(IInterface)
    ['{F55A9DB5-02CD-49F4-B60C-706CB9CE4C20}']
    function GetOwner: TNyxControlRef;
    function GetDocument: TNyxDocument;
    function GetRoot: TNyxNode;
    procedure Bind(const AEvents: INyxEvents; APointerMap: TNyxMovePointerMap);
    procedure Unbind;
    procedure Cancel;
    procedure Disconnect;
    property Owner: TNyxControlRef read GetOwner;
    property Document: TNyxDocument read GetDocument;
    property Root: TNyxNode read GetRoot;
  end;

function NyxMovePosition(ALeft, ATop: Integer): TNyxMovePosition;
{ Defaults to an 8-pixel grid and keyboard step, with portable origin bounds. }
function NyxMovePolicy: TNyxMovePolicy;
{ Managed public button with a 44-pixel touch face and English accessible help.
  Its borrowed descriptor can be adopted by a caller-owned Nyx layout. }
function NewNyxMoveGrip(const AID: TNyxText): INyxButton;
{ Stable ID in a separate adornment scope; application IDs cannot intercept it. }
function NyxCanvasMoveGripID: TNyxText;
function NewNyxCanvasMoveGrip(const AOwner: TNyxControlRef;
  ACapture: TNyxMoveCapture; AFeedback: TNyxMoveFeedback;
  ARetired: TNyxMoveRetired = nil): INyxCanvasMoveGrip;

implementation

uses SysUtils, Math;

type
  IMoveCallback = interface(INyxEventCallback)
    ['{617D4F2E-F7B4-454B-A941-F151B9763504}']
    procedure Detach;
  end;
  TMoveCallback = class(TNyxEventCallback, IMoveCallback)
  private
    FOwner: TNyxMoveHandle;
  public
    constructor Create(AOwner: TNyxMoveHandle);
    procedure Detach;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  TCanvasMoveGrip = class(TInterfacedObject, INyxCanvasMoveGrip)
  private
    FOwner: TNyxControlRef;
    FDocument: TNyxDocument;
    FHandle: TNyxMoveHandle;
    FCapture: TNyxMoveCapture;
    FFeedback: TNyxMoveFeedback;
    FRetired: TNyxMoveRetired;
    FMuting: Boolean;
    function Capture(out APosition: TNyxMovePosition; out APolicy: TNyxMovePolicy): Boolean;
    procedure Feedback(APhase: TNyxMovePhase; const APosition: TNyxMovePosition);
  public
    constructor Create(const AOwner: TNyxControlRef; ACapture: TNyxMoveCapture;
      AFeedback: TNyxMoveFeedback; ARetired: TNyxMoveRetired);
    destructor Destroy; override;
    function GetOwner: TNyxControlRef;
    function GetDocument: TNyxDocument;
    function GetRoot: TNyxNode;
    procedure Bind(const AEvents: INyxEvents; APointerMap: TNyxMovePointerMap);
    procedure Unbind;
    procedure Cancel;
    procedure Disconnect;
  end;

function NyxCanvasMoveGripID: TNyxText;
begin
  Result := 'nyx-canvas-move';
end;

function NewNyxCanvasMoveGrip(const AOwner: TNyxControlRef;
  ACapture: TNyxMoveCapture; AFeedback: TNyxMoveFeedback;
  ARetired: TNyxMoveRetired): INyxCanvasMoveGrip;
begin
  Result := TCanvasMoveGrip.Create(AOwner, ACapture, AFeedback, ARetired);
end;

constructor TCanvasMoveGrip.Create(const AOwner: TNyxControlRef;
  ACapture: TNyxMoveCapture; AFeedback: TNyxMoveFeedback; ARetired: TNyxMoveRetired);
var
  LPage: INyxPage;
  LButton: INyxButton;
begin
  inherited Create;

  if (AOwner.ID = '') or not Assigned(ACapture) or not Assigned(AFeedback) then
  begin
    raise EArgumentException.Create('Canvas move grip requires exact owner and live receivers');
  end;
  FOwner := AOwner;
  FCapture := ACapture;
  FFeedback := AFeedback;
  FRetired := ARetired;
  FDocument := TNyxDocument.Create;
  LPage := NewNyxPage(NyxCanvasMoveGripID + '-face');
  LPage.Configure.Padding(0).Gap(0).Width(44).Height(44).Done;
  LButton := NewNyxMoveGrip(NyxCanvasMoveGripID).WithText('↔');
  LButton.Configure.Width(44).Height(44).Padding(0).Variant(nvSecondary).Done;
  LPage.Add(LButton);
  FDocument.AddPage(LPage);
end;

destructor TCanvasMoveGrip.Destroy;
begin
  Disconnect;
  FDocument.Free;
  inherited Destroy;
end;

function TCanvasMoveGrip.GetOwner: TNyxControlRef;
begin
  Result := FOwner;
end;

function TCanvasMoveGrip.GetDocument: TNyxDocument;
begin
  Result := FDocument;
end;

function TCanvasMoveGrip.GetRoot: TNyxNode;
begin
  Result := FDocument.Pages[0];
end;

function TCanvasMoveGrip.Capture(out APosition: TNyxMovePosition; out APolicy: TNyxMovePolicy): Boolean;
begin
  Result := False;

  if not FMuting and Assigned(FCapture) then
  begin
    Result := FCapture(APosition, APolicy);
  end;
end;

procedure TCanvasMoveGrip.Feedback(APhase: TNyxMovePhase; const APosition: TNyxMovePosition);
begin

  if not FMuting and Assigned(FFeedback) then
  begin
    FFeedback(APhase, APosition);
  end;
end;

procedure TCanvasMoveGrip.Bind(const AEvents: INyxEvents; APointerMap: TNyxMovePointerMap);
begin

  if (AEvents = nil) or not Assigned(APointerMap) or not Assigned(FCapture) then
  begin
    raise EArgumentException.Create('Canvas movement requires live scope, mapping and receivers');
  end;
  FMuting := True;
  try
    FreeAndNil(FHandle);
    FHandle := TNyxMoveHandle.Create(AEvents, NyxControl(NyxCanvasMoveGripID), Capture, Feedback, APointerMap);
  finally
    FMuting := False;
  end;
end;

procedure TCanvasMoveGrip.Unbind;
begin
  FMuting := True;
  try
    FreeAndNil(FHandle);
  finally
    FMuting := False;
  end;

  if Assigned(FRetired) then
  begin
    FRetired;
  end;
end;

procedure TCanvasMoveGrip.Cancel;
begin

  if FHandle <> nil then
  begin
    FHandle.Cancel;
  end;
end;

procedure TCanvasMoveGrip.Disconnect;
begin
  FCapture := nil;
  FFeedback := nil;
  FRetired := nil;
  Unbind;
end;

function NyxMovePosition(ALeft, ATop: Integer): TNyxMovePosition;
begin

  if (ALeft < 0) or (ATop < 0) or (ALeft > MaximumNyxLayoutBound) or
    (ATop > MaximumNyxLayoutBound) then
  begin
    raise EArgumentException.Create('Move origin exceeds the portable layout domain');
  end;
  Result := Default(TNyxMovePosition);
  Result.FDefined := True;
  Result.FLeft := ALeft;
  Result.FTop := ATop;
end;

function TNyxMovePosition.SamePosition(const AOther: TNyxMovePosition): Boolean;
begin
  Result := (Defined = AOther.Defined) and (Left = AOther.Left) and (Top = AOther.Top);
end;

function NyxMovePolicy: TNyxMovePolicy;
begin
  Result := Default(TNyxMovePolicy);
  Result.FSnap := npsGrid;
  Result.FGrid := 8;
  Result.FKeyboardStep := 8;
end;

procedure TNyxMovePolicy.Validate;
begin

  if (Ord(FSnap) < Ord(Low(TNyxPositionSnap))) or
    (Ord(FSnap) > Ord(High(TNyxPositionSnap))) or (FGrid < 1) or
    (FGrid > MaximumNyxLayoutBound) or (FKeyboardStep < 1) or
    (FKeyboardStep > MaximumNyxLayoutBound) then
  begin
    raise EArgumentException.Create('Move policy requires typed snapping and positive steps');
  end;
  FHorizontalBounds.Validate;
  FVerticalBounds.Validate;
end;

function TNyxMovePolicy.Snap(AValue: TNyxPositionSnap): TNyxMovePolicy;
begin
  Result := Self;
  Result.FSnap := AValue;
  Result.Validate;
end;

function TNyxMovePolicy.Grid(AValue: Integer): TNyxMovePolicy;
begin
  Result := Self;
  Result.FGrid := AValue;
  Result.Validate;
end;

function TNyxMovePolicy.KeyboardStep(AValue: Integer): TNyxMovePolicy;
begin
  Result := Self;
  Result.FKeyboardStep := AValue;
  Result.Validate;
end;

function TNyxMovePolicy.Bounds(const AHorizontal, AVertical: TNyxSizeRange): TNyxMovePolicy;
begin
  Result := Self;
  Result.FHorizontalBounds := AHorizontal;
  Result.FVerticalBounds := AVertical;
  Result.Validate;
end;

function TNyxMovePolicy.Guides(const AValue: TNyxAlignmentContext): TNyxMovePolicy;
begin
  Result := Self;
  Result.FGuides := AValue;
end;

function TNyxMovePolicy.Adjust(const AStart: TNyxMovePosition; AX, AY: Double;
  ABypassSnap, ABypassGuides: Boolean): TNyxMovePosition;

  function Coordinate(AInitial: Integer; ADelta: Double; const ARange: TNyxSizeRange;
    AAxis: TNyxGuideAxis; out AGuide: TNyxAlignmentGuide): Integer;
  var
    LValue: Double;
  begin
    AGuide := Default(TNyxAlignmentGuide);

    if ADelta = 0 then
    begin
      Exit(AInitial);
    end;
    LValue := EnsureRange(AInitial + ADelta, 0.0, MaximumNyxLayoutBound * 1.0);

    if not ABypassSnap and not ABypassGuides and FGuides.Defined then
    begin
      Result := FGuides.SnapPosition(AAxis, LValue, ARange.Clamp(0),
        ARange.Clamp(MaximumNyxLayoutBound), AGuide);

      if AGuide.Kind <> ngkNone then
      begin
        Exit;
      end;
    end;

    if (FSnap = npsGrid) and not ABypassSnap then
    begin
      LValue := Floor(LValue / FGrid + 0.5) * FGrid;
    end;
    Result := ARange.Clamp(Integer(Floor(EnsureRange(LValue, 0.0,
      MaximumNyxLayoutBound * 1.0) + 0.5)));
  end;

var
  LLeft, LTop: Integer;
  LHorizontal, LVertical: TNyxAlignmentGuide;
begin
  Validate;

  if not AStart.Defined or IsNan(AX) or IsInfinite(AX) or IsNan(AY) or IsInfinite(AY) then
  begin
    raise EArgumentException.Create('Moving requires defined origin and finite deltas');
  end;
  LLeft := Coordinate(AStart.Left, AX, FHorizontalBounds, ngaWidth, LHorizontal);
  LTop := Coordinate(AStart.Top, AY, FVerticalBounds, ngaHeight, LVertical);
  Result := NyxMovePosition(LLeft, LTop);
  Result.FHorizontalGuide := LHorizontal.Moved(LLeft, LTop);
  Result.FVerticalGuide := LVertical.Moved(LLeft, LTop);
end;

function NewNyxMoveGrip(const AID: TNyxText): INyxButton;
begin
  Result := NewNyxButton(AID).WithText('Move');
  Result.Configure.Width(116).Height(44).TouchBehavior(ntbNone)
    .AccessibleName('Move selected control in its absolute layout')
    .Hint('Drag to move. Arrow keys adjust; Shift takes larger steps. Escape cancels; Alt bypasses snapping.').Done;
end;

constructor TMoveCallback.Create(AOwner: TNyxMoveHandle);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TMoveCallback.Detach;
begin
  FOwner := nil;
end;

procedure TMoveCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin

  if (FOwner <> nil) and not AExecution.Cancelled then
  begin
    FOwner.Invoke(AEvent, AExecution);
  end;
end;

constructor TNyxMoveHandle.Create(const AEvents: INyxEvents; const AGrip: TNyxControlRef;
  ACapture: TNyxMoveCapture; AFeedback: TNyxMoveFeedback; APointerMap: TNyxMovePointerMap);
const
  CTriggers: array[0..6] of TNyxTrigger = (ntPointerDown, ntPointerMove, ntPointerUp,
    ntPointerCancel, ntPointerCaptureLost, ntKeyDown, ntAfterExit);
var
  LIndex: Integer;
begin
  inherited Create;

  if (AEvents = nil) or (AGrip.ID = '') or not Assigned(ACapture) or not Assigned(AFeedback) then
  begin
    raise EArgumentException.Create('Move handle requires live scope, identity and borrowed receivers');
  end;
  FEvents := AEvents;
  FRevision := AEvents.ViewRevision;
  FCapture := ACapture;
  FFeedback := AFeedback;
  FPointerMap := APointerMap;
  FCallback := TMoveCallback.Create(Self);
  SetLength(FSubscriptions, Length(CTriggers));
  for LIndex := 0 to High(CTriggers) do
  begin
    FSubscriptions[LIndex] := AEvents.On(NyxControlEvents(AGrip.ID, niRuntime),
      CTriggers[LIndex]).Policy(neSequential).Subscribe(FCallback);
  end;
end;

destructor TNyxMoveHandle.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TNyxMoveHandle.Disconnect;
var
  LIndex: Integer;
begin
  Cancel;
  FCapture := nil;
  FFeedback := nil;
  FPointerMap := nil;

  if FCallback <> nil then
  begin
    (FCallback as IMoveCallback).Detach;
  end;
  for LIndex := 0 to High(FSubscriptions) do
  begin

    if FSubscriptions[LIndex] <> nil then
    begin
      FSubscriptions[LIndex].Cancel;
    end;
  end;
  FSubscriptions := nil;
  FCallback := nil;
  FEvents := nil;
end;

procedure TNyxMoveHandle.Cancel;
begin

  if FEvents <> nil then
  begin
    FEvents.Scheduler.RequireUI;
  end;

  if not FDragging then
  begin
    Exit;
  end;
  FDragging := False;

  if Assigned(FFeedback) then
  begin
    FFeedback(nmpCancel, FStart);
  end;
end;

function TNyxMoveHandle.Point(const APointer: TNyxPointerSnapshot): TNyxResizePoint;
begin

  if Assigned(FPointerMap) then
  begin
    Result := FPointerMap(APointer);
  end
  else
  begin
    Result := NyxResizePoint(APointer.X, APointer.Y);
  end;

  if not Result.Defined then
  begin
    raise EArgumentException.Create('Move pointer mapping requires a defined logical point');
  end;
end;

function TNyxMoveHandle.BeginChange: Boolean;
begin
  Result := FCapture(FStart, FPolicy);

  if Result then
  begin
    FPolicy.Validate;

    if not FStart.Defined then
    begin
      raise EArgumentException.Create('Move capture returned an undefined origin');
    end;
    FCurrent := FStart;
    FDragging := True;
  end;
end;

procedure TNyxMoveHandle.Finish;
begin

  if not FDragging then
  begin
    Exit;
  end;
  FDragging := False;

  if FCurrent.SamePosition(FStart) then
  begin
    FFeedback(nmpCancel, FStart);
  end
  else
  begin
    FFeedback(nmpCommit, FCurrent);
  end;
end;

procedure TNyxMoveHandle.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LResponse: INyxGestureResponse;
  LInput: INyxEventResponse;
  LX, LY: Double;
  LStep: Integer;
  LPoint: TNyxResizePoint;
begin

  if FEvents <> nil then
  begin
    FEvents.Scheduler.RequireUI;
  end;

  if (FEvents = nil) or (FEvents.ViewRevision <> FRevision) then
  begin
    Cancel;
    Exit;
  end;

  if AEvent.Trigger in [ntPointerCancel, ntPointerCaptureLost, ntAfterExit] then
  begin

    if AEvent.HasPointer and FCaptured and (AEvent.Pointer.ID <> FPointer.ID) then
    begin
      Exit;
    end;
    Cancel;

    if AEvent.Trigger <> ntAfterExit then
    begin
      FCaptured := False;
    end;
    Exit;
  end;

  if (AEvent.Trigger = ntKeyDown) and AEvent.HasKeyboard then
  begin
    LInput := NyxEventResponse(AExecution);

    if (AEvent.Keyboard.Key = nkEscapeKey) and FDragging then
    begin
      Cancel;
      LInput.Consume;
      Exit;
    end;

    if FDragging or (AEvent.Keyboard.Modifiers - [nmShift] <> []) then
    begin
      Exit;
    end;
    LX := 0;
    LY := 0;
    case AEvent.Keyboard.Key of
      nkLeftKey:
        begin
          LX := -1;
        end;
      nkRightKey:
        begin
          LX := 1;
        end;
      nkUpKey:
        begin
          LY := -1;
        end;
      nkDownKey:
        begin
          LY := 1;
        end;
      else
      begin
        Exit;
      end;
    end;
    LInput.Consume;

    if not BeginChange then
    begin
      Exit;
    end;
    LStep := FPolicy.KeyStep;

    if nmShift in AEvent.Keyboard.Modifiers then
    begin
      LStep := LStep * 10;
    end;
    FCurrent := FPolicy.Adjust(FStart, LX * LStep, LY * LStep, True, True);
    Finish;
    Exit;
  end;

  if not AEvent.HasPointer then
  begin
    Exit;
  end;

  if (AEvent.Trigger = ntPointerDown) and not FDragging then
  begin

    if FCaptured or not AEvent.Pointer.Primary or not AEvent.Pointer.HasPosition or
      (AEvent.Pointer.Button <> npbPrimary) then
    begin
      Exit;
    end;
    LResponse := NyxGestureResponse(AExecution);

    if not LResponse.CanRequest(ngcCapturePointer) then
    begin
      Exit;
    end;
    LPoint := Point(AEvent.Pointer);

    if not BeginChange then
    begin
      Exit;
    end;
    FStartPoint := LPoint;
    FPointer := AEvent.Pointer;
    LResponse.CapturePointer;
    FCaptured := True;
    FFeedback(nmpPreview, FCurrent);
    Exit;
  end;

  if AEvent.Pointer.ID <> FPointer.ID then
  begin
    Exit;
  end;

  if (AEvent.Trigger = ntPointerUp) and (AEvent.Pointer.Button <> npbPrimary) then
  begin
    Exit;
  end;

  if (AEvent.Trigger = ntPointerMove) and FDragging and
    not (npbPrimary in AEvent.Pointer.Buttons) then
  begin
    Cancel;
    Exit;
  end;

  if (AEvent.Trigger = ntPointerUp) and FCaptured then
  begin
    LResponse := NyxGestureResponse(AExecution);

    if LResponse.CanRequest(ngcReleasePointer) then
    begin
      LResponse.ReleasePointer;
    end;
    FCaptured := False;
  end;

  if not FDragging or not AEvent.Pointer.HasPosition then
  begin
    Exit;
  end;

  if AEvent.Trigger in [ntPointerMove, ntPointerUp] then
  begin
    LPoint := Point(AEvent.Pointer);
    FCurrent := FPolicy.Adjust(FStart, LPoint.X - FStartPoint.X,
      LPoint.Y - FStartPoint.Y, nmAlt in AEvent.Pointer.Modifiers);

    if AEvent.Trigger = ntPointerUp then
    begin
      Finish;
    end
    else
    begin
      FFeedback(nmpPreview, FCurrent);
    end;
  end;
end;

end.
