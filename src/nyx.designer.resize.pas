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
  nyx.scheduler, nyx.layout.constraints, nyx.model, nyx.gestures, nyx.designer.guides;

type
  { Positive edges resize flow controls without inventing absolute positioning.
    Both changes dimensions together; it never implies aspect-ratio locking. }
  TNyxResizeAxis = (nraWidth, nraHeight, nraBoth);
  TNyxResizePhase = (nrpPreview, nrpCommit, nrpCancel);

  { Copied coordinates in one stable logical plane. Target adapters map the
    moving grip's local pointer position into this plane; differences remain
    exact when a canvas handle follows its preview or its viewport scrolls. }
  TNyxResizePoint = record
  private
    FX: Double;
    FY: Double;
    FDefined: Boolean;
  public
    property X: Double read FX;
    property Y: Double read FY;
    property Defined: Boolean read FDefined;
  end;
  TNyxSizeSnap = (nssUnsnapped, nssGrid);

  { Copied outer-face dimensions in logical pixels. Default is uninitialized;
    the factory admits explicit zero and rejects sizes outside the layout domain. }
  TNyxResizeSize = record
  private
    FDefined: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FWidthGuide: TNyxAlignmentGuide;
    FHeightGuide: TNyxAlignmentGuide;
  public
    function SameSize(const AOther: TNyxResizeSize): Boolean;
    property Defined: Boolean read FDefined;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    { Transient explanation only: equality/persistence use dimensions, never
      renderer geometry. A factory-created ordinary size has no guides. }
    property WidthGuide: TNyxAlignmentGuide read FWidthGuide;
    property HeightGuide: TNyxAlignmentGuide read FHeightGuide;
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
    FGuides: TNyxAlignmentContext;
  public
    function Snap(AValue: TNyxSizeSnap): TNyxResizePolicy;
    function Grid(AValue: Integer): TNyxResizePolicy;
    function KeyboardStep(AValue: Integer): TNyxResizePolicy;
    function Bounds(const AValue: TNyxSizeConstraints): TNyxResizePolicy;
    { Copied gesture snapshot. Eligible guides take precedence over the grid.
      Alt bypasses both; keyboard callers bypass guides to avoid sticky steps. }
    function Guides(const AValue: TNyxAlignmentContext): TNyxResizePolicy;
    procedure Validate;
    function Adjust(const AStart: TNyxResizeSize; AAxis: TNyxResizeAxis;
      AX, AY: Double; ABypassGrid: Boolean = False;
      ABypassGuides: Boolean = False): TNyxResizeSize;
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
  { Borrowed UI-thread mapping. Return a defined finite point in the same
    logical plane for the whole gesture. Nil retains ordinary local behavior.
    The receiver must outlive the connected handle and must not destroy it. }
  TNyxResizePointerMap = function(AAxis: TNyxResizeAxis;
    const APointer: TNyxPointerSnapshot): TNyxResizePoint of object;
  { Scope retirement only: revoke any shared host lease. The borrowed receiver
    must not paint, reenter mounting or destroy an executing grip from here. }
  TNyxResizeRetired = procedure of object;

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
    FPointerMap: TNyxResizePointerMap;
    FStartPoint: TNyxResizePoint;
    FAxis: TNyxResizeAxis;
    FPolicy: TNyxResizePolicy;
    FStart: TNyxResizeSize;
    FCurrent: TNyxResizeSize;
    FPointer: TNyxPointerSnapshot;
    FDragging: Boolean;
    FCaptured: Boolean;
    function BeginChange: Boolean;
    function PointerPoint(const APointer: TNyxPointerSnapshot): TNyxResizePoint;
    procedure Finish;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
  public
    constructor Create(const AEvents: INyxEvents; const AGrip: TNyxControlRef;
      AAxis: TNyxResizeAxis; ACapture: TNyxResizeCapture; AFeedback: TNyxResizeFeedback;
      APointerMap: TNyxResizePointerMap = nil);
    destructor Destroy; override;
    procedure Cancel;
    procedure Disconnect;
    property Dragging: Boolean read FDragging;
  end;

  { Managed reusable canvas adornment. It owns an independent tiny document
    with three specialized Nyx buttons, never the edited application. Adapters
    retain this interface while mounting its borrowed roots and bind their own
    event scopes/mapping. Unbind silently retires target subscriptions while
    allowing a later remount; Disconnect permanently retires borrowed editor
    receivers. Both must precede destruction of their respective receivers. }
  INyxCanvasResizeGrips = interface(IInterface)
    ['{82536A1A-1D2D-4727-B4D6-8E6B9CC1C749}']
    function GetOwner: TNyxControlRef;
    function GetDocument: TNyxDocument;
    function Root(AAxis: TNyxResizeAxis): TNyxNode;
    procedure Bind(AAxis: TNyxResizeAxis; const AEvents: INyxEvents;
      APointerMap: TNyxResizePointerMap);
    procedure Unbind;
    procedure Disconnect;
    function Dragging(AAxis: TNyxResizeAxis): Boolean;
    property Owner: TNyxControlRef read GetOwner;
    property Document: TNyxDocument read GetDocument;
  end;

function NyxResizePoint(AX, AY: Double): TNyxResizePoint;
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
{ Closed axis maps to one stable adornment ID. IDs are scoped to its separate
  renderer, so identical application names cannot intercept these callbacks. }
function NyxCanvasResizeGripID(AAxis: TNyxResizeAxis): TNyxText;
{ The returned interface owns its document and connected behaviors. Caller must
  Disconnect before its borrowed capture/feedback objects are destroyed. }
function NewNyxCanvasResizeGrips(const AOwner: TNyxControlRef;
  ACapture: TNyxResizeCapture; AFeedback: TNyxResizeFeedback;
  ARetired: TNyxResizeRetired = nil): INyxCanvasResizeGrips;

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

  TCanvasResizeGrips = class(TInterfacedObject, INyxCanvasResizeGrips)
  private
    FOwner: TNyxControlRef;
    FDocument: TNyxDocument;
    FHandles: array[TNyxResizeAxis] of TNyxResizeHandle;
    FCapture: TNyxResizeCapture;
    FFeedback: TNyxResizeFeedback;
    FMuting: Boolean;
    FRetired: TNyxResizeRetired;
    function Capture(AAxis: TNyxResizeAxis; out ASize: TNyxResizeSize;
      out APolicy: TNyxResizePolicy): Boolean;
    procedure Feedback(AAxis: TNyxResizeAxis; APhase: TNyxResizePhase;
      const ASize: TNyxResizeSize);
  public
    constructor Create(const AOwner: TNyxControlRef;
      ACapture: TNyxResizeCapture; AFeedback: TNyxResizeFeedback; ARetired: TNyxResizeRetired);
    destructor Destroy; override;
    function GetOwner: TNyxControlRef;
    function GetDocument: TNyxDocument;
    function Root(AAxis: TNyxResizeAxis): TNyxNode;
    procedure Bind(AAxis: TNyxResizeAxis; const AEvents: INyxEvents;
      APointerMap: TNyxResizePointerMap);
    procedure Unbind;
    procedure Disconnect;
    function Dragging(AAxis: TNyxResizeAxis): Boolean;
  end;

procedure RequireAxis(AAxis: TNyxResizeAxis);
begin

  if (Ord(AAxis) < Ord(Low(TNyxResizeAxis))) or
    (Ord(AAxis) > Ord(High(TNyxResizeAxis))) then
  begin
    raise EArgumentException.Create('Resize requires width, height or both');
  end;
end;

function NyxResizePoint(AX, AY: Double): TNyxResizePoint;
begin

  if IsNan(AX) or IsInfinite(AX) or IsNan(AY) or IsInfinite(AY) then
  begin
    raise EArgumentException.Create('Resize pointer coordinates must be finite');
  end;
  Result := Default(TNyxResizePoint);
  Result.FX := AX;
  Result.FY := AY;
  Result.FDefined := True;
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

function TNyxResizePolicy.Guides(const AValue: TNyxAlignmentContext): TNyxResizePolicy;
begin
  Result := Self;
  Result.FGuides := AValue;
end;

function TNyxResizePolicy.Adjust(const AStart: TNyxResizeSize; AAxis: TNyxResizeAxis;
  AX, AY: Double; ABypassGrid, ABypassGuides: Boolean): TNyxResizeSize;

  function Dimension(AInitial: Integer; ADelta: Double; const ARange: TNyxSizeRange;
    AGuideAxis: TNyxGuideAxis; out AGuide: TNyxAlignmentGuide): Integer;
  var
    LValue: Double;
    LMinimum: Integer;
    LMaximum: Integer;
  begin
    AGuide := Default(TNyxAlignmentGuide);
    { A tap/release or movement only along the other axis is a true no-op,
      including an allocated face that does not start on the chosen grid. }

    if ADelta = 0 then
    begin
      Exit(AInitial);
    end;
    LValue := AInitial;
    LValue := EnsureRange(LValue + ADelta, 0.0, MaximumNyxLayoutBound * 1.0);

    if FGuides.Defined and not ABypassGrid and not ABypassGuides then
    begin
      LMinimum := ARange.Clamp(0);
      LMaximum := ARange.Clamp(MaximumNyxLayoutBound);
      Result := FGuides.Snap(AGuideAxis, LValue, LMinimum, LMaximum, AGuide);

      if AGuide.Kind <> ngkNone then
      begin
        Exit;
      end;
    end;

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
  LWidthGuide: TNyxAlignmentGuide;
  LHeightGuide: TNyxAlignmentGuide;
begin
  Validate;
  RequireAxis(AAxis);

  if not AStart.Defined or IsNan(AX) or IsInfinite(AX) or IsNan(AY) or IsInfinite(AY) then
  begin
    raise EArgumentException.Create('Resize requires initialized geometry and finite deltas');
  end;
  LWidth := AStart.Width;
  LHeight := AStart.Height;
  LWidthGuide := Default(TNyxAlignmentGuide);
  LHeightGuide := Default(TNyxAlignmentGuide);

  if AAxis in [nraWidth, nraBoth] then
  begin
    LWidth := Dimension(LWidth, AX, FBounds.WidthRange, ngaWidth, LWidthGuide);
  end;

  if AAxis in [nraHeight, nraBoth] then
  begin
    LHeight := Dimension(LHeight, AY, FBounds.HeightRange, ngaHeight, LHeightGuide);
  end;
  Result := NyxResizeSize(LWidth, LHeight);
  Result.FWidthGuide := LWidthGuide;
  Result.FHeightGuide := LHeightGuide;
end;

function NewNyxResizeGrip(const AID: TNyxText; AAxis: TNyxResizeAxis): INyxButton;
const
  CTitles: array[TNyxResizeAxis] of TNyxText = ('Width', 'Height', 'Both');
begin
  RequireAxis(AAxis);
  { The containing tools explain the action; short visual captions fit the
    guaranteed 44-pixel face. The accessible name retains the complete action. }
  Result := NewNyxButton(AID).WithText(CTitles[AAxis]);
  Result.Configure.Width(116).Height(44).TouchBehavior(ntbNone)
    .AccessibleName('Resize selected control ' + CTitles[AAxis])
    .Hint('Drag to resize. Arrow keys adjust; Shift takes larger steps. Escape cancels; Alt bypasses snapping.')
    .Done;
end;

function NyxCanvasResizeGripID(AAxis: TNyxResizeAxis): TNyxText;
const
  CIDs: array[TNyxResizeAxis] of TNyxText =
    ('nyx-canvas-resize-width', 'nyx-canvas-resize-height', 'nyx-canvas-resize-both');
begin
  RequireAxis(AAxis);
  Result := CIDs[AAxis];
end;

function NewNyxCanvasResizeGrips(const AOwner: TNyxControlRef;
  ACapture: TNyxResizeCapture; AFeedback: TNyxResizeFeedback;
  ARetired: TNyxResizeRetired): INyxCanvasResizeGrips;
begin
  Result := TCanvasResizeGrips.Create(AOwner, ACapture, AFeedback, ARetired);
end;

constructor TCanvasResizeGrips.Create(const AOwner: TNyxControlRef;
  ACapture: TNyxResizeCapture; AFeedback: TNyxResizeFeedback; ARetired: TNyxResizeRetired);
const
  CGlyphs: array[TNyxResizeAxis] of TNyxText = ('↔', '↕', '↘');
var
  LAxis: TNyxResizeAxis;
  LPage: INyxPage;
  LButton: INyxButton;
begin
  inherited Create;

  if (AOwner.ID = '') or not Assigned(ACapture) or not Assigned(AFeedback) then
  begin
    raise EArgumentException.Create('Canvas resize grips require an owner and live receivers');
  end;
  FOwner := AOwner;
  FCapture := ACapture;
  FFeedback := AFeedback;
  FRetired := ARetired;
  FDocument := TNyxDocument.Create;
  for LAxis := Low(TNyxResizeAxis) to High(TNyxResizeAxis) do
  begin
    LPage := NewNyxPage(NyxCanvasResizeGripID(LAxis) + '-face');
    LPage.Configure.Padding(0).Gap(0).Width(44).Height(44).Done;
    LButton := NewNyxResizeGrip(NyxCanvasResizeGripID(LAxis), LAxis);
    LButton.WithText(CGlyphs[LAxis]);
    LButton.Configure.Width(44).Height(44).Padding(0).Variant(nvSecondary).Done;
    LPage.Add(LButton);
    FDocument.AddPage(LPage);
  end;
end;

destructor TCanvasResizeGrips.Destroy;
begin
  Disconnect;
  FDocument.Free;
  inherited Destroy;
end;

function TCanvasResizeGrips.GetOwner: TNyxControlRef;
begin
  Result := FOwner;
end;

function TCanvasResizeGrips.GetDocument: TNyxDocument;
begin
  Result := FDocument;
end;

function TCanvasResizeGrips.Root(AAxis: TNyxResizeAxis): TNyxNode;
begin
  RequireAxis(AAxis);
  Result := FDocument.Pages[Ord(AAxis)];
end;

function TCanvasResizeGrips.Capture(AAxis: TNyxResizeAxis; out ASize: TNyxResizeSize;
  out APolicy: TNyxResizePolicy): Boolean;
begin
  Result := False;

  if not FMuting and Assigned(FCapture) then
  begin
    Result := FCapture(AAxis, ASize, APolicy);
  end;
end;

procedure TCanvasResizeGrips.Feedback(AAxis: TNyxResizeAxis; APhase: TNyxResizePhase;
  const ASize: TNyxResizeSize);
begin

  if not FMuting and Assigned(FFeedback) then
  begin
    FFeedback(AAxis, APhase, ASize);
  end;
end;

procedure TCanvasResizeGrips.Bind(AAxis: TNyxResizeAxis; const AEvents: INyxEvents;
  APointerMap: TNyxResizePointerMap);
begin
  RequireAxis(AAxis);

  if (AEvents = nil) or not Assigned(APointerMap) or not Assigned(FCapture) then
  begin
    raise EArgumentException.Create('Canvas resize scope requires live events, mapping and receivers');
  end;
  FMuting := True;
  try
    FreeAndNil(FHandles[AAxis]);
    FHandles[AAxis] := TNyxResizeHandle.Create(AEvents,
      NyxControl(NyxCanvasResizeGripID(AAxis)), AAxis, Capture, Feedback, APointerMap);
  finally
    FMuting := False;
  end;

end;

procedure TCanvasResizeGrips.Unbind;
var
  LAxis: TNyxResizeAxis;
begin
  { Target retirement must not reenter editor painting while hosts are being
    removed. Its gesture lease loses the old scope; it cannot publish afterward. }
  FMuting := True;
  try
    for LAxis := Low(TNyxResizeAxis) to High(TNyxResizeAxis) do
    begin
      FreeAndNil(FHandles[LAxis]);
    end;
  finally
    FMuting := False;
  end;

  if Assigned(FRetired) then
  begin
    FRetired;
  end;
end;

procedure TCanvasResizeGrips.Disconnect;
begin
  FCapture := nil;
  FFeedback := nil;
  FRetired := nil;
  Unbind;
end;

function TCanvasResizeGrips.Dragging(AAxis: TNyxResizeAxis): Boolean;
begin
  RequireAxis(AAxis);
  Result := (FHandles[AAxis] <> nil) and FHandles[AAxis].Dragging;
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
  AAxis: TNyxResizeAxis; ACapture: TNyxResizeCapture; AFeedback: TNyxResizeFeedback;
  APointerMap: TNyxResizePointerMap);
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
  FPointerMap := APointerMap;
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
  FPointerMap := nil;

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

function TNyxResizeHandle.PointerPoint(const APointer: TNyxPointerSnapshot): TNyxResizePoint;
begin

  if Assigned(FPointerMap) then
  begin
    Result := FPointerMap(FAxis, APointer);
  end
  else
  begin
    Result := NyxResizePoint(APointer.X, APointer.Y);
  end;

  if not Result.Defined then
  begin
    raise EArgumentException.Create('Resize pointer mapping returned undefined coordinates');
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
    FCurrent := FPolicy.Adjust(FStart, FAxis, LDeltaX * LStep, LDeltaY * LStep, False, True);
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

    if not LResponse.CanRequest(ngcCapturePointer) then
    begin
      Exit;
    end;
    LPoint := PointerPoint(AEvent.Pointer);

    if not BeginChange then
    begin
      Exit;
    end;
    FStartPoint := LPoint;
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
    LPoint := PointerPoint(AEvent.Pointer);
    FCurrent := FPolicy.Adjust(FStart, FAxis, LPoint.X - FStartPoint.X,
      LPoint.Y - FStartPoint.Y, nmAlt in AEvent.Pointer.Modifiers);

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
