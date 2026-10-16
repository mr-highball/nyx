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
unit nyx.canvas.geometry;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.text;

const
  { An unbounded scene has no page edges; its arithmetic still has finite
    precision. These portable admission limits avoid integer widget extents and
    unpredictable browser/native overflow. They are not a rendered scene size.
    Camera limits below select narrower application-specific zoom ranges. }
  MaximumNyxCanvasCoordinate = 1.0e12;
  MinimumNyxCanvasZoom = 1.0e-6;
  MaximumNyxCanvasZoom = 1.0e6;
  MinimumNyxCanvasGridSpacing = 1.0e-6;

type
  ENyxCanvasGeometry = class(Exception);

  { Immutable logical 2D coordinate. Points are used in world and viewport
    space; method parameter names identify which space is required. Viewport
    coordinates are logical pixels, not hardware/device pixels. Default records
    are absent and refuse use rather than silently supplying (0, 0). }
  TNyxCanvasPoint = record
  private
    FDefined: Boolean;
    FX: Double;
    FY: Double;
    procedure RequireDefined;
  public
    function Offset(AX, AY: Double): TNyxCanvasPoint;
    property Defined: Boolean read FDefined;
    property X: Double read FX;
    property Y: Double read FY;
  end;

  { Nonnegative extents permit a point/line bounds for Fit. Edges are closed for
    geometric queries. Actual rendering/hit slop must choose its own semantics. }
  TNyxCanvasBounds = record
  private
    FDefined: Boolean;
    FLeft: Double;
    FTop: Double;
    FWidth: Double;
    FHeight: Double;
    procedure RequireDefined;
  public
    function Center: TNyxCanvasPoint;
    function Contains(const AWorldPoint: TNyxCanvasPoint): Boolean;
    function Intersects(const AOther: TNyxCanvasBounds): Boolean;
    property Defined: Boolean read FDefined;
    property Left: Double read FLeft;
    property Top: Double read FTop;
    property Width: Double read FWidth;
    property Height: Double read FHeight;
  end;

  { Positive zoom is viewport logical pixels per world unit. Limits contain the
    initial zoom; requests clamp to this range. Invalid/nonfinite values refuse
    instead of being clamped. Records own no scene, view, adapter or callbacks. }
  TNyxCanvasZoomLimits = record
  private
    FDefined: Boolean;
    FMinimum: Double;
    FMaximum: Double;
    procedure RequireDefined;
  public
    function Clamp(AZoom: Double): Double;
    property Defined: Boolean read FDefined;
    property Minimum: Double read FMinimum;
    property Maximum: Double read FMaximum;
  end;

  { Unbounded retains infinite navigation within portable numerical limits.
    Center constrains only the camera center; Viewport keeps each visible axis
    inside the region when it fits, otherwise centers that oversized axis. }
  TNyxCanvasBoundaryMode = (ncbUnbounded, ncbCenter, ncbViewport);

  { The world center maps to the viewport center. All fluent methods return
    independently copied values; failed operations preserve the original camera.
    Adapters normalize input gestures and publish only an admitted result.
    Optional world bounds follow every derived camera, including resize/Fit and
    scripted delivery. Boundary clamping may move a requested zoom anchor.
    No device-pixel ratio, DOM transform or LCL control size belongs here. }
  TNyxCanvasCamera = record
  private
    FDefined: Boolean;
    FCenter: TNyxCanvasPoint;
    FWidth: Double;
    FHeight: Double;
    FZoom: Double;
    FLimits: TNyxCanvasZoomLimits;
    FBounds: TNyxCanvasBounds;
    FBoundaryMode: TNyxCanvasBoundaryMode;
    procedure RequireDefined;
    function Reframe(AWidth, AHeight, ACenterX, ACenterY, AZoom: Double;
      const ALimits: TNyxCanvasZoomLimits): TNyxCanvasCamera;
  public
    function WorldToViewport(const AWorldPoint: TNyxCanvasPoint): TNyxCanvasPoint;
    function ViewportToWorld(const AViewportPoint: TNyxCanvasPoint): TNyxCanvasPoint;
    function VisibleWorld: TNyxCanvasBounds;
    { Resizing keeps zoom and the proposed world center, then applies optional
      bounds. The host chooses whether a changed size should also fit content. }
    function WithViewport(AWidth, AHeight: Double): TNyxCanvasCamera;
    function AtCenter(const AWorldCenter: TNyxCanvasPoint): TNyxCanvasCamera; overload;
    { Atomically derive center and requested zoom before boundary clamping.
      Useful for Fit/navigation; avoids an invalid intermediate zoomed frame. }
    function AtCenter(const AWorldCenter: TNyxCanvasPoint; AZoom: Double): TNyxCanvasCamera; overload;
    function WithLimits(const ALimits: TNyxCanvasZoomLimits): TNyxCanvasCamera;
    { Constrain this independent camera now and on every later derived value.
      Point/line regions lock their corresponding center axes. Unbounded is
      explicit through WithoutBounds; invalid modes/absent regions refuse.
      Numerical admission still applies to the complete visible world. }
    function Within(const AWorldBounds: TNyxCanvasBounds;
      AMode: TNyxCanvasBoundaryMode = ncbViewport): TNyxCanvasCamera;
    function WithoutBounds: TNyxCanvasCamera;
    { Exact defined frame/limits/policy equality for revision-guarded captures.
      False when either record is absent; no host objects or epsilon matching. }
    function SameFrame(const AOther: TNyxCanvasCamera): Boolean;
    { Positive screen deltas move content right/down (a hand-tool drag). }
    function PanBy(AScreenX, AScreenY: Double): TNyxCanvasCamera;
    { Keep the world point beneath the supplied viewport anchor stationary,
      including when zoom clamps at a configured limit. Optional world bounds
      take precedence when that anchor would require leaving the region. }
    function ZoomAt(AZoom: Double;
      const AViewportAnchor: TNyxCanvasPoint): TNyxCanvasCamera;
    { Preserve the world point beneath the old finger midpoint beneath the new
      midpoint, with scale from their distances. Rotation is deliberately absent
      in this initial camera contract; rotated scene items are a separate layer.
      Zero-distance contacts refuse; adapters can treat these as a pan instead.
      Optional world bounds constrain the combined final scale/translation. }
    function Pinch(const AOldFirst, AOldSecond, ANewFirst,
      ANewSecond: TNyxCanvasPoint): TNyxCanvasCamera;
    { Fit centers nonnegative bounds with a logical-pixel inset on all sides.
      Point bounds keep current zoom; line bounds use their nonzero extent.
      Zoom limits and world bounds may prevent complete fitting and remain
      caller-controlled; no policy is dropped to make a requested fit succeed. }
    function Fit(const AWorldBounds: TNyxCanvasBounds;
      AInset: Double = 24): TNyxCanvasCamera;
    property Defined: Boolean read FDefined;
    property WorldCenter: TNyxCanvasPoint read FCenter;
    property ViewportWidth: Double read FWidth;
    property ViewportHeight: Double read FHeight;
    property Zoom: Double read FZoom;
    property Limits: TNyxCanvasZoomLimits read FLimits;
    property NavigationBounds: TNyxCanvasBounds read FBounds;
    property BoundaryMode: TNyxCanvasBoundaryMode read FBoundaryMode;
  end;

  { Visual grid treatment, independent of whether placement snaps to its points. }
  TNyxCanvasGridStyle = (ncgsNone, ncgsDots, ncgsLines);
  { Placement quantization; grid snapping remains available with hidden chrome. }
  TNyxCanvasSnapMode = (ncsmFree, ncsmGrid);

  { Grid visibility and snapping are independent. Hidden grids can snap; visible
    grids can permit free placement. Spacing/origin are world values. Adaptive
    visible spacing doubles to avoid subpixel overdraw but never changes the
    authored snapping interval. Appearance is supplied later by theme tokens. }
  TNyxCanvasGrid = record
  private
    FDefined: Boolean;
    FStyle: TNyxCanvasGridStyle;
    FSnap: TNyxCanvasSnapMode;
    FSpacing: Double;
    FOrigin: TNyxCanvasPoint;
    procedure RequireDefined;
  public
    function Styled(AStyle: TNyxCanvasGridStyle): TNyxCanvasGrid;
    function Snapping(AMode: TNyxCanvasSnapMode): TNyxCanvasGrid;
    function AtOrigin(const AWorldOrigin: TNyxCanvasPoint): TNyxCanvasGrid;
    { Ties round away from the grid origin, consistently on both targets. }
    function Snap(const AWorldPoint: TNyxCanvasPoint): TNyxCanvasPoint;
    { Return a world interval whose projected spacing is at least the requested
      logical-pixel distance. This does not materialize off-screen grid points. }
    function VisibleSpacing(const ACamera: TNyxCanvasCamera;
      AMinimumPixels: Double = 12): Double;
    property Defined: Boolean read FDefined;
    property Style: TNyxCanvasGridStyle read FStyle;
    property SnapMode: TNyxCanvasSnapMode read FSnap;
    property Spacing: Double read FSpacing;
    property Origin: TNyxCanvasPoint read FOrigin;
  end;

{ Constructors validate before publication. No global options or mutable shared
  state are used; snapshots may be evaluated on workers without touching UI. }
function NyxCanvasPoint(AX, AY: Double): TNyxCanvasPoint;
function NyxCanvasBounds(ALeft, ATop, AWidth, AHeight: Double): TNyxCanvasBounds;
function NyxCanvasZoomLimits(AMinimum: Double = 0.05;
  AMaximum: Double = 16): TNyxCanvasZoomLimits;
function NyxCanvasCamera(AViewportWidth, AViewportHeight: Double;
  const AWorldCenter: TNyxCanvasPoint; AZoom: Double = 1): TNyxCanvasCamera; overload;
function NyxCanvasCamera(AViewportWidth, AViewportHeight: Double;
  const AWorldCenter: TNyxCanvasPoint; AZoom: Double;
  const ALimits: TNyxCanvasZoomLimits): TNyxCanvasCamera; overload;
function NyxCanvasGrid(AWorldSpacing: Double = 20;
  AStyle: TNyxCanvasGridStyle = ncgsDots;
  ASnap: TNyxCanvasSnapMode = ncsmFree): TNyxCanvasGrid;

implementation

procedure RequireFinite(AValue: Double; const AName: TNyxText);
begin

  if IsNan(AValue) or IsInfinite(AValue) then
  begin
    raise ENyxCanvasGeometry.Create(AName + ' must be finite');
  end;
end;

procedure RequireCoordinate(AValue: Double);
begin
  RequireFinite(AValue, 'Canvas coordinate');

  if Abs(AValue) > MaximumNyxCanvasCoordinate then
  begin
    raise ENyxCanvasGeometry.Create('Canvas coordinate exceeds portable precision limits');
  end;
end;

procedure RequireExtent(AValue: Double; APositive: Boolean);
begin
  RequireCoordinate(AValue);

  if (AValue < 0) or (APositive and (AValue = 0)) then
  begin
    raise ENyxCanvasGeometry.Create('Canvas extent is invalid');
  end;
end;

procedure RequireZoom(AValue: Double);
begin
  RequireFinite(AValue, 'Canvas zoom');

  if AValue <= 0 then
  begin
    raise ENyxCanvasGeometry.Create('Canvas zoom must be positive');
  end;
end;

procedure TNyxCanvasPoint.RequireDefined;
begin

  if not Defined then
  begin
    raise ENyxCanvasGeometry.Create('Canvas point is absent');
  end;
end;

function NyxCanvasPoint(AX, AY: Double): TNyxCanvasPoint;
begin
  RequireCoordinate(AX);
  RequireCoordinate(AY);
  Result.FDefined := True;
  Result.FX := AX;
  Result.FY := AY;
end;

function TNyxCanvasPoint.Offset(AX, AY: Double): TNyxCanvasPoint;
begin
  RequireDefined;
  RequireCoordinate(AX);
  RequireCoordinate(AY);
  Result := NyxCanvasPoint(X + AX, Y + AY);
end;

procedure TNyxCanvasBounds.RequireDefined;
begin

  if not Defined then
  begin
    raise ENyxCanvasGeometry.Create('Canvas bounds are absent');
  end;
end;

function NyxCanvasBounds(ALeft, ATop, AWidth, AHeight: Double): TNyxCanvasBounds;
begin
  RequireCoordinate(ALeft);
  RequireCoordinate(ATop);
  RequireExtent(AWidth, False);
  RequireExtent(AHeight, False);
  RequireCoordinate(ALeft + AWidth);
  RequireCoordinate(ATop + AHeight);
  Result.FDefined := True;
  Result.FLeft := ALeft;
  Result.FTop := ATop;
  Result.FWidth := AWidth;
  Result.FHeight := AHeight;
end;

function TNyxCanvasBounds.Center: TNyxCanvasPoint;
begin
  RequireDefined;
  Result := NyxCanvasPoint(Left + Width / 2, Top + Height / 2);
end;

function TNyxCanvasBounds.Contains(const AWorldPoint: TNyxCanvasPoint): Boolean;
begin
  RequireDefined;
  AWorldPoint.RequireDefined;
  Result := (AWorldPoint.X >= Left) and (AWorldPoint.X <= Left + Width) and
    (AWorldPoint.Y >= Top) and (AWorldPoint.Y <= Top + Height);
end;

function TNyxCanvasBounds.Intersects(const AOther: TNyxCanvasBounds): Boolean;
begin
  RequireDefined;
  AOther.RequireDefined;
  Result := (Left <= AOther.Left + AOther.Width) and
    (Left + Width >= AOther.Left) and (Top <= AOther.Top + AOther.Height) and
    (Top + Height >= AOther.Top);
end;

procedure TNyxCanvasZoomLimits.RequireDefined;
begin

  if not Defined then
  begin
    raise ENyxCanvasGeometry.Create('Canvas zoom limits are absent');
  end;
end;

function NyxCanvasZoomLimits(AMinimum, AMaximum: Double): TNyxCanvasZoomLimits;
begin
  RequireZoom(AMinimum);
  RequireZoom(AMaximum);

  if (AMinimum < MinimumNyxCanvasZoom) or (AMaximum > MaximumNyxCanvasZoom) or
    (AMinimum > AMaximum) then
  begin
    raise ENyxCanvasGeometry.Create('Canvas zoom limits are invalid');
  end;
  Result.FDefined := True;
  Result.FMinimum := AMinimum;
  Result.FMaximum := AMaximum;
end;

function TNyxCanvasZoomLimits.Clamp(AZoom: Double): Double;
begin
  RequireDefined;
  RequireZoom(AZoom);
  Result := Max(Minimum, Min(Maximum, AZoom));
end;

procedure TNyxCanvasCamera.RequireDefined;
begin

  if not Defined then
  begin
    raise ENyxCanvasGeometry.Create('Canvas camera is absent');
  end;
end;

function NyxCanvasCamera(AViewportWidth, AViewportHeight: Double;
  const AWorldCenter: TNyxCanvasPoint; AZoom: Double): TNyxCanvasCamera;
begin
  Result := NyxCanvasCamera(AViewportWidth, AViewportHeight, AWorldCenter,
    AZoom, NyxCanvasZoomLimits);
end;

function NyxCanvasCamera(AViewportWidth, AViewportHeight: Double;
  const AWorldCenter: TNyxCanvasPoint; AZoom: Double;
  const ALimits: TNyxCanvasZoomLimits): TNyxCanvasCamera;
begin
  Result := Default(TNyxCanvasCamera);
  RequireExtent(AViewportWidth, True);
  RequireExtent(AViewportHeight, True);
  AWorldCenter.RequireDefined;
  ALimits.RequireDefined;
  RequireZoom(AZoom);

  if (AZoom < ALimits.Minimum) or (AZoom > ALimits.Maximum) then
  begin
    raise ENyxCanvasGeometry.Create('Initial canvas zoom is outside its limits');
  end;
  { Admit the entire visible world, not just its center. Otherwise an accepted
    camera could fail even when converting its viewport corners. }
  NyxCanvasBounds(AWorldCenter.X - AViewportWidth / (2 * AZoom),
    AWorldCenter.Y - AViewportHeight / (2 * AZoom),
    AViewportWidth / AZoom, AViewportHeight / AZoom);
  Result.FDefined := True;
  Result.FCenter := AWorldCenter;
  Result.FWidth := AViewportWidth;
  Result.FHeight := AViewportHeight;
  Result.FZoom := AZoom;
  Result.FLimits := ALimits;
end;

function TNyxCanvasCamera.WorldToViewport(
  const AWorldPoint: TNyxCanvasPoint): TNyxCanvasPoint;
begin
  RequireDefined;
  AWorldPoint.RequireDefined;
  Result := NyxCanvasPoint((AWorldPoint.X - FCenter.X) * Zoom + FWidth / 2,
    (AWorldPoint.Y - FCenter.Y) * Zoom + FHeight / 2);
end;

function TNyxCanvasCamera.ViewportToWorld(
  const AViewportPoint: TNyxCanvasPoint): TNyxCanvasPoint;
begin
  RequireDefined;
  AViewportPoint.RequireDefined;
  Result := NyxCanvasPoint(FCenter.X + (AViewportPoint.X - FWidth / 2) / Zoom,
    FCenter.Y + (AViewportPoint.Y - FHeight / 2) / Zoom);
end;

function TNyxCanvasCamera.VisibleWorld: TNyxCanvasBounds;
begin
  RequireDefined;
  Result := NyxCanvasBounds(FCenter.X - FWidth / (2 * Zoom),
    FCenter.Y - FHeight / (2 * Zoom), FWidth / Zoom, FHeight / Zoom);
end;

function TNyxCanvasCamera.Reframe(AWidth, AHeight, ACenterX, ACenterY,
  AZoom: Double; const ALimits: TNyxCanvasZoomLimits): TNyxCanvasCamera;

  function ConstrainAxis(AProposed, AStart, AExtent, AVisibleExtent: Double): Double;
  var
    LInset: Double;
  begin
    LInset := 0;

    if FBoundaryMode = ncbViewport then
    begin

      if AVisibleExtent >= AExtent then
      begin
        Exit(AStart + AExtent / 2);
      end;
      LInset := AVisibleExtent / 2;
    end;
    Result := Max(AStart + LInset, Min(AProposed, AStart + AExtent - LInset));
  end;

begin
  RequireDefined;
  RequireExtent(AWidth, True);
  RequireExtent(AHeight, True);
  RequireZoom(AZoom);

  if IsNan(ACenterX) or IsInfinite(ACenterX) or
    IsNan(ACenterY) or IsInfinite(ACenterY) then
  begin
    raise ENyxCanvasGeometry.Create('Canvas center must remain finite');
  end;

  if FBoundaryMode <> ncbUnbounded then
  begin
    ACenterX := ConstrainAxis(ACenterX, FBounds.Left, FBounds.Width, AWidth / AZoom);
    ACenterY := ConstrainAxis(ACenterY, FBounds.Top, FBounds.Height, AHeight / AZoom);
  end;
  Result := NyxCanvasCamera(AWidth, AHeight, NyxCanvasPoint(ACenterX, ACenterY), AZoom, ALimits);
  Result.FBounds := FBounds;
  Result.FBoundaryMode := FBoundaryMode;
end;

function TNyxCanvasCamera.Within(const AWorldBounds: TNyxCanvasBounds;
  AMode: TNyxCanvasBoundaryMode): TNyxCanvasCamera;
begin
  RequireDefined;
  AWorldBounds.RequireDefined;

  if not (AMode in [ncbCenter, ncbViewport]) then
  begin
    raise ENyxCanvasGeometry.Create('Choose center or viewport bounds, or explicitly remove bounds');
  end;
  Result := Self;
  Result.FBounds := AWorldBounds;
  Result.FBoundaryMode := AMode;
  Result := Result.Reframe(FWidth, FHeight, FCenter.X, FCenter.Y, Zoom, Limits);
end;

function TNyxCanvasCamera.WithoutBounds: TNyxCanvasCamera;
begin
  RequireDefined;
  Result := Self;
  Result.FBounds := Default(TNyxCanvasBounds);
  Result.FBoundaryMode := ncbUnbounded;
end;

function TNyxCanvasCamera.WithViewport(AWidth, AHeight: Double): TNyxCanvasCamera;
begin
  RequireDefined;
  Result := Reframe(AWidth, AHeight, FCenter.X, FCenter.Y, Zoom, Limits);
end;

function TNyxCanvasCamera.AtCenter(const AWorldCenter: TNyxCanvasPoint): TNyxCanvasCamera;
begin
  RequireDefined;
  AWorldCenter.RequireDefined;
  Result := Reframe(FWidth, FHeight, AWorldCenter.X, AWorldCenter.Y, Zoom, Limits);
end;

function TNyxCanvasCamera.AtCenter(const AWorldCenter: TNyxCanvasPoint;
  AZoom: Double): TNyxCanvasCamera;
begin
  RequireDefined;
  AWorldCenter.RequireDefined;
  Result := Reframe(FWidth, FHeight, AWorldCenter.X, AWorldCenter.Y, Limits.Clamp(AZoom), Limits);
end;

function TNyxCanvasCamera.SameFrame(const AOther: TNyxCanvasCamera): Boolean;
begin
  Result := Defined and AOther.Defined;

  if not Result then
  begin
    Exit;
  end;
  Result := (FCenter.X = AOther.FCenter.X) and (FCenter.Y = AOther.FCenter.Y) and
    (Zoom = AOther.Zoom) and (FWidth = AOther.FWidth) and (FHeight = AOther.FHeight) and
    (Limits.Minimum = AOther.Limits.Minimum) and (Limits.Maximum = AOther.Limits.Maximum) and
    (FBoundaryMode = AOther.FBoundaryMode);

  if Result and (FBoundaryMode <> ncbUnbounded) then
  begin
    Result := (FBounds.Left = AOther.FBounds.Left) and (FBounds.Top = AOther.FBounds.Top) and
      (FBounds.Width = AOther.FBounds.Width) and (FBounds.Height = AOther.FBounds.Height);
  end;
end;

function TNyxCanvasCamera.WithLimits(const ALimits: TNyxCanvasZoomLimits): TNyxCanvasCamera;
begin
  RequireDefined;
  Result := Reframe(FWidth, FHeight, FCenter.X, FCenter.Y, ALimits.Clamp(Zoom), ALimits);
end;

function TNyxCanvasCamera.PanBy(AScreenX, AScreenY: Double): TNyxCanvasCamera;
begin
  RequireDefined;
  RequireCoordinate(AScreenX);
  RequireCoordinate(AScreenY);
  Result := Reframe(FWidth, FHeight, FCenter.X - AScreenX / Zoom,
    FCenter.Y - AScreenY / Zoom, Zoom, Limits);
end;

function TNyxCanvasCamera.ZoomAt(AZoom: Double;
  const AViewportAnchor: TNyxCanvasPoint): TNyxCanvasCamera;
var
  LWorldAnchor: TNyxCanvasPoint;
  LZoom: Double;
  LCenter: TNyxCanvasPoint;
begin
  RequireDefined;
  LZoom := Limits.Clamp(AZoom);
  AViewportAnchor.RequireDefined;

  if FBoundaryMode <> ncbUnbounded then
  begin
    { Clamp the mathematical candidate before admitting a portable point. A
      large finite overshoot must reach a configured edge, not overflow merely
      because an intermediate unbounded center leaves the world domain. }
    Exit(Reframe(FWidth, FHeight,
      FCenter.X + (AViewportAnchor.X - FWidth / 2) * (1 / Zoom - 1 / LZoom),
      FCenter.Y + (AViewportAnchor.Y - FHeight / 2) * (1 / Zoom - 1 / LZoom),
      LZoom, Limits));
  end;
  LWorldAnchor := ViewportToWorld(AViewportAnchor);
  LCenter := NyxCanvasPoint(LWorldAnchor.X - (AViewportAnchor.X - FWidth / 2) / LZoom,
    LWorldAnchor.Y - (AViewportAnchor.Y - FHeight / 2) / LZoom);
  Result := NyxCanvasCamera(FWidth, FHeight, LCenter, LZoom, Limits);
end;

function Midpoint(const AFirst, ASecond: TNyxCanvasPoint): TNyxCanvasPoint;
begin
  AFirst.RequireDefined;
  ASecond.RequireDefined;
  Result := NyxCanvasPoint(AFirst.X / 2 + ASecond.X / 2,
    AFirst.Y / 2 + ASecond.Y / 2);
end;

function Distance(const AFirst, ASecond: TNyxCanvasPoint): Double;
begin
  AFirst.RequireDefined;
  ASecond.RequireDefined;
  Result := Hypot(AFirst.X - ASecond.X, AFirst.Y - ASecond.Y);

  if Result = 0 then
  begin
    raise ENyxCanvasGeometry.Create('Pinch contacts must have distinct positions');
  end;
end;

function TNyxCanvasCamera.Pinch(const AOldFirst, AOldSecond, ANewFirst,
  ANewSecond: TNyxCanvasPoint): TNyxCanvasCamera;
var
  LOldMidpoint: TNyxCanvasPoint;
  LNewMidpoint: TNyxCanvasPoint;
  LRatio: Double;
  LZoom: Double;
begin
  RequireDefined;
  LOldMidpoint := Midpoint(AOldFirst, AOldSecond);
  LNewMidpoint := Midpoint(ANewFirst, ANewSecond);
  LRatio := Distance(ANewFirst, ANewSecond) / Distance(AOldFirst, AOldSecond);

  if FBoundaryMode <> ncbUnbounded then
  begin
    { A pinch is one proposal. Clamping an intermediate zoom before applying
      its translation would discard overshoot that the final midpoint cancels. }
    LZoom := Limits.Clamp(Zoom * LRatio);
    Exit(Reframe(FWidth, FHeight,
      FCenter.X + (LOldMidpoint.X - FWidth / 2) / Zoom -
        (LNewMidpoint.X - FWidth / 2) / LZoom,
      FCenter.Y + (LOldMidpoint.Y - FHeight / 2) / Zoom -
        (LNewMidpoint.Y - FHeight / 2) / LZoom,
      LZoom, Limits));
  end;
  Result := ZoomAt(Zoom * LRatio, LOldMidpoint);
  Result := Result.PanBy(LNewMidpoint.X - LOldMidpoint.X,
    LNewMidpoint.Y - LOldMidpoint.Y);
end;

function TNyxCanvasCamera.Fit(const AWorldBounds: TNyxCanvasBounds;
  AInset: Double): TNyxCanvasCamera;
var
  LZoom: Double;
begin
  RequireDefined;
  AWorldBounds.RequireDefined;
  RequireExtent(AInset, False);

  if (2 * AInset >= FWidth) or (2 * AInset >= FHeight) then
  begin
    raise ENyxCanvasGeometry.Create('Canvas fit inset leaves no viewport area');
  end;
  LZoom := Zoom;

  if (AWorldBounds.Width > 0) and (AWorldBounds.Height > 0) then
  begin
    LZoom := Min((FWidth - 2 * AInset) / AWorldBounds.Width,
      (FHeight - 2 * AInset) / AWorldBounds.Height);
  end
  else
  begin

    if AWorldBounds.Width > 0 then
    begin
      LZoom := (FWidth - 2 * AInset) / AWorldBounds.Width;
    end;

    if AWorldBounds.Height > 0 then
    begin
      LZoom := (FHeight - 2 * AInset) / AWorldBounds.Height;
    end;
  end;
  Result := Reframe(FWidth, FHeight, AWorldBounds.Center.X, AWorldBounds.Center.Y,
    Limits.Clamp(LZoom), Limits);
end;

procedure TNyxCanvasGrid.RequireDefined;
begin

  if not Defined then
  begin
    raise ENyxCanvasGeometry.Create('Canvas grid is absent');
  end;
end;

procedure RequireStyle(AStyle: TNyxCanvasGridStyle);
begin

  if (Ord(AStyle) < Ord(Low(TNyxCanvasGridStyle))) or
    (Ord(AStyle) > Ord(High(TNyxCanvasGridStyle))) then
  begin
    raise ENyxCanvasGeometry.Create('Canvas grid style is invalid');
  end;
end;

procedure RequireSnap(AMode: TNyxCanvasSnapMode);
begin

  if (Ord(AMode) < Ord(Low(TNyxCanvasSnapMode))) or
    (Ord(AMode) > Ord(High(TNyxCanvasSnapMode))) then
  begin
    raise ENyxCanvasGeometry.Create('Canvas snap mode is invalid');
  end;
end;

function NyxCanvasGrid(AWorldSpacing: Double; AStyle: TNyxCanvasGridStyle;
  ASnap: TNyxCanvasSnapMode): TNyxCanvasGrid;
begin
  RequireExtent(AWorldSpacing, True);

  if AWorldSpacing < MinimumNyxCanvasGridSpacing then
  begin
    raise ENyxCanvasGeometry.Create('Canvas grid spacing is below portable precision limits');
  end;
  RequireStyle(AStyle);
  RequireSnap(ASnap);
  Result.FDefined := True;
  Result.FStyle := AStyle;
  Result.FSnap := ASnap;
  Result.FSpacing := AWorldSpacing;
  Result.FOrigin := NyxCanvasPoint(0, 0);
end;

function TNyxCanvasGrid.Styled(AStyle: TNyxCanvasGridStyle): TNyxCanvasGrid;
begin
  RequireDefined;
  RequireStyle(AStyle);
  Result := Self;
  Result.FStyle := AStyle;
end;

function TNyxCanvasGrid.Snapping(AMode: TNyxCanvasSnapMode): TNyxCanvasGrid;
begin
  RequireDefined;
  RequireSnap(AMode);
  Result := Self;
  Result.FSnap := AMode;
end;

function TNyxCanvasGrid.AtOrigin(const AWorldOrigin: TNyxCanvasPoint): TNyxCanvasGrid;
begin
  RequireDefined;
  AWorldOrigin.RequireDefined;
  Result := Self;
  Result.FOrigin := AWorldOrigin;
end;

function SnapAxis(AValue, AOrigin, ASpacing: Double): Double;
var
  LUnits: Double;
  LRounded: Double;
begin
  LUnits := (AValue - AOrigin) / ASpacing;
  { Trunc/Floor return target-dependent integer types; the rounding arithmetic
    stays within JavaScript's exact integer range before any conversion. }

  if Abs(LUnits) > 4503599627370495.0 then
  begin
    raise ENyxCanvasGeometry.Create('Canvas snap exceeds exact portable grid indexing');
  end;
  LRounded := Int(Abs(LUnits) + 0.5);

  if LUnits < 0 then
  begin
    LRounded := -LRounded;
  end;
  Result := AOrigin + LRounded * ASpacing;
end;

function TNyxCanvasGrid.Snap(const AWorldPoint: TNyxCanvasPoint): TNyxCanvasPoint;
begin
  RequireDefined;
  AWorldPoint.RequireDefined;

  if SnapMode = ncsmFree then
  begin
    Exit(AWorldPoint);
  end;
  Result := NyxCanvasPoint(SnapAxis(AWorldPoint.X, Origin.X, Spacing),
    SnapAxis(AWorldPoint.Y, Origin.Y, Spacing));
end;

function TNyxCanvasGrid.VisibleSpacing(const ACamera: TNyxCanvasCamera;
  AMinimumPixels: Double): Double;
begin
  RequireDefined;
  ACamera.RequireDefined;
  RequireExtent(AMinimumPixels, True);
  Result := Spacing;
  while Result * ACamera.Zoom < AMinimumPixels do
  begin
    Result := Result * 2;

    if Result > MaximumNyxCanvasCoordinate then
    begin
      raise ENyxCanvasGeometry.Create('Visible canvas grid interval exceeds precision limits');
    end;
  end;
end;

end.
