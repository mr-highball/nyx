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

unit nyx.designer.resize;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.controls, nyx.behavior, nyx.events,
  nyx.scheduler, nyx.layout.constraints, nyx.model, nyx.gestures;

type
  { Positive edges resize flow controls without inventing absolute positioning.
    Both changes dimensions together; it never implies aspect-ratio locking. }
  TNyxResizeAxis = (nraWidth, nraHeight, nraBoth);
  TNyxResizePhase = (nrpPreview, nrpCommit, nrpCancel);
  TNyxSizeSnap = (nssUnsnapped, nssGrid);

  { Copied outer-face dimensions in logical pixels. Default is uninitialized;
    the factory admits explicit zero and rejects sizes outside the layout domain. }
  TNyxResizeSize = record
  private
    FDefined: Boolean;
    FWidth: Integer;
    FHeight: Integer;
  public
    function SameSize(const AOther: TNyxResizeSize): Boolean;
    property Defined: Boolean read FDefined;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

  { Copied designer presentation, independent of the accepted document and
    target widgets. Default clears the proposal. A defined value names an exact
    authored control and its proposed outer size in logical pixels. Renderers
    resolve the current mounted origin; no tree, input or ownership is retained. }
  TNyxResizePreview = record
  private
    FControl: TNyxControlRef;
    FSize: TNyxResizeSize;
    function GetActive: Boolean;
  public
    property Active: Boolean read GetActive;
    property Control: TNyxControlRef read FControl;
    property Size: TNyxResizeSize read FSize;
  end;

  { Value-only policy. Grid snapping rounds final dimensions to the nearest
    multiple (ties toward increasing size), then clamps to explicit bounds.
    Thus a non-grid minimum/maximum remains exact. Alt bypasses the grid during
    pointer input. Bounds never constrain the unchanged axis. Factories supply
    defaults; all copied builders validate before returning a new value. }
  TNyxResizePolicy = record
  private
    FSnap: TNyxSizeSnap;
    FGrid: Integer;
    FKeyboardStep: Integer;
    FBounds: TNyxSizeConstraints;
  public
    function Snap(AValue: TNyxSizeSnap): TNyxResizePolicy;
    function Grid(AValue: Integer): TNyxResizePolicy;
    function KeyboardStep(AValue: Integer): TNyxResizePolicy;
    function Bounds(const AValue: TNyxSizeConstraints): TNyxResizePolicy;
    procedure Validate;
    function Adjust(const AStart: TNyxResizeSize; AAxis: TNyxResizeAxis;
      AX, AY: Double; ABypassGrid: Boolean = False): TNyxResizeSize;
    property SnapMode: TNyxSizeSnap read FSnap;
    property GridSize: Integer read FGrid;
    property KeyStep: Integer read FKeyboardStep;
    property SizeBounds: TNyxSizeConstraints read FBounds;
  end;

  { Synchronous UI-thread receivers, borrowed until Disconnect/Destroy. Capture
    returns copied accepted geometry/policy and may refuse a stale owner/draft.
    Preview is presentation only; Commit is one semantic operation owned by the
    host. Receivers must not destroy this handle during their invocation. }
  TNyxResizeCapture = function(AAxis: TNyxResizeAxis; out ASize: TNyxResizeSize;
    out APolicy: TNyxResizePolicy): Boolean of object;
  TNyxResizeFeedback = procedure(AAxis: TNyxResizeAxis; APhase: TNyxResizePhase;
    const ASize: TNyxResizeSize) of object;

  { Reusable public behavior for an ordinary Nyx button grip. Sequential public
    pointer/key streams implement capture, matching-pointer deltas, Escape,
    cancellation/lost capture/focus exit, and arrow-key one-step transactions.
    No model/widget/DOM/LCL handle is retained. Disconnect detaches borrowed
    callbacks before canceling subscriptions, including caller-retained streams. }
  TNyxResizeHandle = class
  private
    FEvents: INyxEvents;
    FRevision: Integer;
    FCallback: INyxEventCallback;
    FSubscriptions: array of INyxEventSubscription;
    FCapture: TNyxResizeCapture;
    FFeedback: TNyxResizeFeedback;
    FAxis: TNyxResizeAxis;
    FPolicy: TNyxResizePolicy;
    FStart: TNyxResizeSize;
    FCurrent: TNyxResizeSize;
    FPointer: TNyxPointerSnapshot;
    FDragging: Boolean;
    FCaptured: Boolean;
    function BeginChange: Boolean;
    procedure Finish;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
  public
    constructor Create(const AEvents: INyxEvents; const AGrip: TNyxControlRef;
      AAxis: TNyxResizeAxis; ACapture: TNyxResizeCapture; AFeedback: TNyxResizeFeedback);
    destructor Destroy; override;
    procedure Cancel;
    procedure Disconnect;
    property Dragging: Boolean read FDragging;
  end;

function NyxResizeSize(AWidth, AHeight: Integer): TNyxResizeSize;
{ Rejects missing identity or undefined dimensions before any presentation
  changes. Default(TNyxResizePreview) is the explicit clear operation. }
function NyxResizePreview(const AControl: TNyxControlRef;
  const ASize: TNyxResizeSize): TNyxResizePreview;
function NyxResizePolicy: TNyxResizePolicy;
{ Managed specialized button, stable 116x44 logical-pixel face, explicit touch
  negotiation and an English accessible name/hint. Its descriptor is borrowed
  through Node as with every public Nyx control; a parent can adopt it safely. }
function NewNyxResizeGrip(const AID: TNyxText; AAxis: TNyxResizeAxis): INyxButton;

implementation

uses
  Math;

function TNyxResizePreview.GetActive: Boolean;
begin
  Result := FControl.ID <> '';
end;

function NyxResizePreview(const AControl: TNyxControlRef;
  const ASize: TNyxResizeSize): TNyxResizePreview;
begin

  if (AControl.ID = '') or not ASize.Defined then
  begin
    raise EArgumentException.Create('Resize preview requires an exact control and defined size');
  end;
  Result := Default(TNyxResizePreview);
  Result.FControl := AControl;
  Result.FSize := ASize;
end;

type
  IResizeCallback = interface(INyxEventCallback)
    ['{54AE046B-5CE1-435A-A109-2FCBD0A2FDAD}']
    procedure Detach;
  end;
  TResizeCallback = class(TNyxEventCallback, IResizeCallback)
  private
    FOwner: TNyxResizeHandle;
  public
    constructor Create(AOwner: TNyxResizeHandle);
    procedure Detach;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

procedure RequireAxis(AAxis: TNyxResizeAxis);
begin

  if (Ord(AAxis) < Ord(Low(TNyxResizeAxis))) or
    (Ord(AAxis) > Ord(High(TNyxResizeAxis))) then
  begin
    raise EArgumentException.Create('Resize requires width, height or both');
  end;
end;

function NyxResizeSize(AWidth, AHeight: Integer): TNyxResizeSize;
begin

  if (AWidth < 0) or (AHeight < 0) or
    (AWidth > MaximumNyxLayoutBound) or (AHeight > MaximumNyxLayoutBound) then
  begin
    raise EArgumentException.Create('Resize dimensions exceed the portable layout domain');
  end;
  Result := Default(TNyxResizeSize);
  Result.FDefined := True;
  Result.FWidth := AWidth;
  Result.FHeight := AHeight;
end;

function TNyxResizeSize.SameSize(const AOther: TNyxResizeSize): Boolean;
begin
  Result := (Defined = AOther.Defined) and (Width = AOther.Width) and (Height = AOther.Height);
end;

function NyxResizePolicy: TNyxResizePolicy;
begin
  Result := Default(TNyxResizePolicy);
  Result.FSnap := nssGrid;
  Result.FGrid := 8;
  Result.FKeyboardStep := 8;
end;

procedure TNyxResizePolicy.Validate;
begin

  if (Ord(FSnap) < Ord(Low(TNyxSizeSnap))) or (Ord(FSnap) > Ord(High(TNyxSizeSnap))) or
    (FGrid < 1) or (FGrid > MaximumNyxLayoutBound) or
    (FKeyboardStep < 1) or (FKeyboardStep > MaximumNyxLayoutBound) then
  begin
    raise EArgumentException.Create('Resize policy requires valid snapping and positive steps');
  end;
  FBounds.Validate;
end;

function TNyxResizePolicy.Snap(AValue: TNyxSizeSnap): TNyxResizePolicy;
begin
  Result := Self;
  Result.FSnap := AValue;
  Result.Validate;
end;

function TNyxResizePolicy.Grid(AValue: Integer): TNyxResizePolicy;
begin
  Result := Self;
  Result.FGrid := AValue;
  Result.Validate;
end;

function TNyxResizePolicy.KeyboardStep(AValue: Integer): TNyxResizePolicy;
begin
  Result := Self;
  Result.FKeyboardStep := AValue;
  Result.Validate;
end;

function TNyxResizePolicy.Bounds(const AValue: TNyxSizeConstraints): TNyxResizePolicy;
begin
  Result := Self;
  Result.FBounds := AValue;
  Result.Validate;
end;

function TNyxResizePolicy.Adjust(const AStart: TNyxResizeSize; AAxis: TNyxResizeAxis;
  AX, AY: Double; ABypassGrid: Boolean): TNyxResizeSize;

  function Dimension(AInitial: Integer; ADelta: Double; const ARange: TNyxSizeRange): Integer;
  var
    LValue: Double;
  begin
    { A tap/release or movement only along the other axis is a true no-op,
      including an allocated face that does not start on the chosen grid. }

    if ADelta = 0 then
    begin
      Exit(AInitial);
    end;
    LValue := AInitial;
    LValue := EnsureRange(LValue + ADelta, 0.0, MaximumNyxLayoutBound * 1.0);

    if (FSnap = nssGrid) and not ABypassGrid then
    begin
      LValue := Floor(LValue / FGrid + 0.5) * FGrid;
    end;
    Result := ARange.Clamp(Integer(Floor(EnsureRange(LValue, 0.0,
      MaximumNyxLayoutBound * 1.0) + 0.5)));
  end;

var
  LWidth: Integer;
  LHeight: Integer;
begin
  Validate;
  RequireAxis(AAxis);

  if not AStart.Defined or IsNan(AX) or IsInfinite(AX) or IsNan(AY) or IsInfinite(AY) then
  begin
    raise EArgumentException.Create('Resize requires initialized geometry and finite deltas');
  end;
  LWidth := AStart.Width;
  LHeight := AStart.Height;

  if AAxis in [nraWidth, nraBoth] then
  begin
    LWidth := Dimension(LWidth, AX, FBounds.WidthRange);
  end;

  if AAxis in [nraHeight, nraBoth] then
  begin
    LHeight := Dimension(LHeight, AY, FBounds.HeightRange);
  end;
  Result := NyxResizeSize(LWidth, LHeight);
end;

function NewNyxResizeGrip(const AID: TNyxText; AAxis: TNyxResizeAxis): INyxButton;
const
  CTitles: array[TNyxResizeAxis] of TNyxText = ('Width', 'Height', 'Both');
begin
  RequireAxis(AAxis);
  Result := NewNyxButton(AID).WithText('Resize ' + CTitles[AAxis]);
  Result.Configure.Width(116).Height(44).TouchBehavior(ntbNone)
    .AccessibleName('Resize selected control ' + CTitles[AAxis])
    .Hint('Drag to resize. Arrow keys adjust; Shift takes larger steps. Escape cancels; Alt bypasses snapping.')
    .Done;
end;

constructor TResizeCallback.Create(AOwner: TNyxResizeHandle);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TResizeCallback.Detach;
begin
  FOwner := nil;
end;

procedure TResizeCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin

  if (FOwner <> nil) and not AExecution.Cancelled then
  begin
    FOwner.Invoke(AEvent, AExecution);
  end;
end;

constructor TNyxResizeHandle.Create(const AEvents: INyxEvents; const AGrip: TNyxControlRef;
  AAxis: TNyxResizeAxis; ACapture: TNyxResizeCapture; AFeedback: TNyxResizeFeedback);
const
  CTriggers: array[0..6] of TNyxTrigger = (ntPointerDown, ntPointerMove, ntPointerUp,
    ntPointerCancel, ntPointerCaptureLost, ntKeyDown, ntAfterExit);
var
  LIndex: Integer;
begin
  inherited Create;
  RequireAxis(AAxis);

  if (AEvents = nil) or (AGrip.ID = '') or not Assigned(ACapture) or not Assigned(AFeedback) then
  begin
    raise EArgumentException.Create('Resize handle requires live events, identity and host receivers');
  end;
  FEvents := AEvents;
  FRevision := AEvents.ViewRevision;
  FAxis := AAxis;
  FCapture := ACapture;
  FFeedback := AFeedback;
  FCallback := TResizeCallback.Create(Self);
  SetLength(FSubscriptions, Length(CTriggers));
  for LIndex := 0 to High(CTriggers) do
  begin
    FSubscriptions[LIndex] := AEvents.On(NyxControlEvents(AGrip.ID, niRuntime),
      CTriggers[LIndex]).Policy(neSequential).Subscribe(FCallback);
  end;
end;

destructor TNyxResizeHandle.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TNyxResizeHandle.Disconnect;
var
  LIndex: Integer;
begin
  Cancel;
  FCapture := nil;
  FFeedback := nil;

  if FCallback <> nil then
  begin
    (FCallback as IResizeCallback).Detach;
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

procedure TNyxResizeHandle.Cancel;
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
    FFeedback(FAxis, nrpCancel, FStart);
  end;
end;

function TNyxResizeHandle.BeginChange: Boolean;
begin
  Result := FCapture(FAxis, FStart, FPolicy);

  if Result then
  begin
    FPolicy.Validate;

    if not FStart.Defined then
    begin
      raise EArgumentException.Create('Resize capture returned undefined geometry');
    end;
    FCurrent := FStart;
    FDragging := True;
  end;
end;

procedure TNyxResizeHandle.Finish;
begin

  if not FDragging then
  begin
    Exit;
  end;
  FDragging := False;

  if FCurrent.SameSize(FStart) then
  begin
    FFeedback(FAxis, nrpCancel, FStart);
  end
  else
  begin
    FFeedback(FAxis, nrpCommit, FCurrent);
  end;
end;

procedure TNyxResizeHandle.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LResponse: INyxGestureResponse;
  LInput: INyxEventResponse;
  LDeltaX: Double;
  LDeltaY: Double;
  LStep: Integer;
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
    LDeltaX := 0;
    LDeltaY := 0;
    case AEvent.Keyboard.Key of
      nkLeftKey:
        begin
          LDeltaX := -1;
        end;
      nkRightKey:
        begin
          LDeltaX := 1;
        end;
      nkUpKey:
        begin
          LDeltaY := -1;
        end;
      nkDownKey:
        begin
          LDeltaY := 1;
        end;
      else
      begin
        Exit;
      end;
    end;

    if ((FAxis = nraWidth) and (LDeltaX = 0)) or
      ((FAxis = nraHeight) and (LDeltaY = 0)) then
    begin
      Exit;
    end;

    if not BeginChange then
    begin
      { A recognized grip shortcut remains handled while its editor is busy or
        protecting a draft. Falling through would scroll/navigate unexpectedly. }
      LInput.Consume;
      Exit;
    end;
    LStep := FPolicy.KeyStep;

    if nmShift in AEvent.Keyboard.Modifiers then
    begin
      LStep := LStep * 10;
    end;
    FCurrent := FPolicy.Adjust(FStart, FAxis, LDeltaX * LStep, LDeltaY * LStep);
    LInput.Consume;
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

    if not LResponse.CanRequest(ngcCapturePointer) or not BeginChange then
    begin
      Exit;
    end;
    FPointer := AEvent.Pointer;
    LResponse.CapturePointer;
    FCaptured := True;
    FFeedback(FAxis, nrpPreview, FCurrent);
    Exit;
  end;

  if (AEvent.Pointer.ID <> FPointer.ID) then
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
    FCurrent := FPolicy.Adjust(FStart, FAxis, AEvent.Pointer.X - FPointer.X,
      AEvent.Pointer.Y - FPointer.Y, nmAlt in AEvent.Pointer.Modifiers);

    if AEvent.Trigger = ntPointerUp then
    begin
      Finish;
    end
    else
    begin
      FFeedback(FAxis, nrpPreview, FCurrent);
    end;
  end;
end;

end.
