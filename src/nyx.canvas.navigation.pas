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

unit nyx.canvas.navigation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, Math, nyx.canvas.geometry, nyx.canvas.types;

const
  MaximumNyxCanvasNavigationItems = 16384;
  MaximumNyxCanvasNavigationOccluders = 16;

type
  { One inert navigation extent. Left/top are world anchors; width/height are
    world units for Reflow or fixed logical pixels for LogicalSize. Values own
    no content, scene, control or adapter. Independent extents must not occlude
    other requested content to report Complete. Default bounds refuse admission. }
  TNyxCanvasNavigationItem = record
    Bounds: TNyxCanvasBounds;
    Scaling: TNyxCanvasControlScaling;
    Independent: Boolean;
  end;

  { Caller-owned extent window. Navigation reads it without retaining/mutating
    array storage; live adapters copy creator contributions before publication. }
  TNyxCanvasNavigationItems = array of TNyxCanvasNavigationItem;

  { Viewport-logical rectangles occupied by floating chrome. They affect camera
    planning only, never authored scene geometry or hit-testing authority. }
  TNyxCanvasNavigationOccluders = array of TNyxCanvasBounds;

  { Caller choice for floating chrome. Avoid searches unobstructed rectangles;
    Ignore deliberately treats the whole viewport as usable. }
  TNyxCanvasNavigationChrome = (ncncAvoid, ncncIgnore);

  { Independent fluent camera preferences. Inset is logical pixels and can be
    reduced when the minimum admitted zoom needs that space. ZoomIn permits
    magnification beyond current zoom during Fit; Reveal preserves zoom. }
  TNyxCanvasNavigationOptions = record
  private
    FDefined: Boolean;
    FInset: Double;
    FZoomIn: Boolean;
    FChrome: TNyxCanvasNavigationChrome;
    FDistinct: Boolean;
  public
    function Inset(AValue: Double): TNyxCanvasNavigationOptions;
    function ZoomIn(AAllowed: Boolean): TNyxCanvasNavigationOptions;
    function Chrome(APolicy: TNyxCanvasNavigationChrome): TNyxCanvasNavigationOptions;
    function Distinct(ARequired: Boolean): TNyxCanvasNavigationOptions;
    procedure Validate;
    property Margin: Double read FInset;
    property MayZoomIn: Boolean read FZoomIn;
    property ChromePolicy: TNyxCanvasNavigationChrome read FChrome;
    property CheckDistinct: Boolean read FDistinct;
  end;

  { Empty preserves the original camera. Complete means the requested extents
    fit one unobstructed region; Limited reports zoom/size/occlusion constraints
    without pretending full visibility. No status grants execution authority. }
  TNyxCanvasNavigationStatus = (ncnsEmpty, ncnsComplete, ncnsLimited);

  { Copied camera proposal plus its chosen viewport region. Undefined Region
    means no positive unobstructed allocation exists. A caller publishes Camera
    through its normal guarded view; computing this record has no side effects. }
  TNyxCanvasNavigationResult = record
    Camera: TNyxCanvasCamera;
    Region: TNyxCanvasBounds;
    Status: TNyxCanvasNavigationStatus;
  end;

function NyxCanvasNavigationOptions: TNyxCanvasNavigationOptions;
function NyxCanvasNavigationItem(const ABounds: TNyxCanvasBounds;
  AScaling: TNyxCanvasControlScaling; AIndependent: Boolean = False): TNyxCanvasNavigationItem;
{ Fit mixed world/fixed-size extents without scaling readable logical controls.
  Zoom obeys caller limits. Huge/unrepresentable camera geometry refuses before
  publication. Bounded region search never silently discards an occluder. }
function FitNyxCanvasNavigation(const ACamera: TNyxCanvasCamera;
  const AItems: array of TNyxCanvasNavigationItem;
  const AOccluders: array of TNyxCanvasBounds;
  const AOptions: TNyxCanvasNavigationOptions): TNyxCanvasNavigationResult;
{ Pan minimally to reveal one extent at current zoom. An oversized extent aligns
  its leading edge for useful partial access and returns Limited. }
function RevealNyxCanvasNavigation(const ACamera: TNyxCanvasCamera;
  const AItem: TNyxCanvasNavigationItem;
  const AOccluders: array of TNyxCanvasBounds;
  const AOptions: TNyxCanvasNavigationOptions): TNyxCanvasNavigationResult;

implementation

type
  { Private maximal-region candidates contain only value geometry. Overlapping
    candidates are intentional alternatives; containment pruning bounds work. }
  TRegions = array of TNyxCanvasBounds;

procedure RequireNonnegative(AValue: Double);
begin

  if IsNan(AValue) or IsInfinite(AValue) or (AValue < 0) or
    (AValue > MaximumNyxCanvasCoordinate) then
  begin
    raise ENyxCanvasGeometry.Create('Navigation inset must be finite and nonnegative');
  end;
end;

function NyxCanvasNavigationOptions: TNyxCanvasNavigationOptions;
begin
  Result := Default(TNyxCanvasNavigationOptions);
  Result.FDefined := True;
  Result.FInset := 24;
  Result.FChrome := ncncAvoid;
  Result.FDistinct := True;
end;

procedure TNyxCanvasNavigationOptions.Validate;
begin

  if not FDefined or not (FChrome in [ncncAvoid, ncncIgnore]) then
  begin
    raise ENyxCanvasGeometry.Create('Use defined typed canvas navigation options');
  end;
  RequireNonnegative(FInset);
end;

function TNyxCanvasNavigationOptions.Inset(AValue: Double): TNyxCanvasNavigationOptions;
begin
  Validate;
  RequireNonnegative(AValue);
  Result := Self;
  Result.FInset := AValue;
end;

function TNyxCanvasNavigationOptions.ZoomIn(AAllowed: Boolean): TNyxCanvasNavigationOptions;
begin
  Validate;
  Result := Self;
  Result.FZoomIn := AAllowed;
end;

function TNyxCanvasNavigationOptions.Chrome(
  APolicy: TNyxCanvasNavigationChrome): TNyxCanvasNavigationOptions;
begin
  Result := Self;
  Result.FChrome := APolicy;
  Result.Validate;
end;

function NyxCanvasNavigationItem(const ABounds: TNyxCanvasBounds;
  AScaling: TNyxCanvasControlScaling; AIndependent: Boolean): TNyxCanvasNavigationItem;
begin

  if not ABounds.Defined or not (AScaling in [nccsReflow, nccsLogicalSize]) then
  begin
    raise ENyxCanvasGeometry.Create('Navigation requires admitted bounds and scaling policy');
  end;
  Result.Bounds := NyxCanvasBounds(ABounds.Left, ABounds.Top, ABounds.Width, ABounds.Height);
  Result.Scaling := AScaling;
  Result.Independent := AIndependent;
end;

function TNyxCanvasNavigationOptions.Distinct(ARequired: Boolean): TNyxCanvasNavigationOptions;
begin
  Validate;
  Result := Self;
  Result.FDistinct := ARequired;
end;

function ContainsRect(const AOuter, AInner: TNyxCanvasBounds): Boolean;
begin
  Result := (AInner.Left >= AOuter.Left) and (AInner.Top >= AOuter.Top) and
    (AInner.Left + AInner.Width <= AOuter.Left + AOuter.Width) and
    (AInner.Top + AInner.Height <= AOuter.Top + AOuter.Height);
end;

procedure AddRegion(var ARegions: TRegions; const ABounds: TNyxCanvasBounds);
var
  LIndex: Integer;
  LNext: Integer;
begin

  if (ABounds.Width <= 0) or (ABounds.Height <= 0) then
  begin
    Exit;
  end;
  LIndex := 0;
  while LIndex < Length(ARegions) do
  begin

    if ContainsRect(ARegions[LIndex], ABounds) then
    begin
      Exit;
    end;

    if ContainsRect(ABounds, ARegions[LIndex]) then
    begin
      for LNext := LIndex to High(ARegions) - 1 do
      begin
        ARegions[LNext] := ARegions[LNext + 1];
      end;
      SetLength(ARegions, Length(ARegions) - 1);
    end
    else
    begin
      Inc(LIndex);
    end;
  end;

  if Length(ARegions) >= 128 then
  begin
    raise ENyxCanvasGeometry.Create('Navigation region search exceeds its bounded capacity');
  end;
  SetLength(ARegions, Length(ARegions) + 1);
  ARegions[High(ARegions)] := ABounds;
end;

function Regions(const ACamera: TNyxCanvasCamera;
  const AOccluders: array of TNyxCanvasBounds;
  const AOptions: TNyxCanvasNavigationOptions): TRegions;
var
  LNext: TRegions;
  LRect: TNyxCanvasBounds;
  LBlock: TNyxCanvasBounds;
  LIndex: Integer;
  LRegion: Integer;
  LLeft: Double;
  LTop: Double;
  LRight: Double;
  LBottom: Double;
begin
  AOptions.Validate;

  if not ACamera.Defined or (Length(AOccluders) > MaximumNyxCanvasNavigationOccluders) then
  begin
    raise ENyxCanvasGeometry.Create('Navigation requires a camera and bounded occluder window');
  end;
  for LIndex := 0 to High(AOccluders) do
  begin

    if not AOccluders[LIndex].Defined then
    begin
      raise ENyxCanvasGeometry.Create('Navigation occluders must be defined viewport rectangles');
    end;
  end;
  Result := nil;
  AddRegion(Result, NyxCanvasBounds(0, 0, ACamera.ViewportWidth, ACamera.ViewportHeight));

  if AOptions.ChromePolicy = ncncIgnore then
  begin
    Exit;
  end;
  for LIndex := 0 to High(AOccluders) do
  begin
    LNext := nil;
    LBlock := AOccluders[LIndex];
    for LRegion := 0 to High(Result) do
    begin
      LRect := Result[LRegion];
      LLeft := Max(LRect.Left, LBlock.Left);
      LTop := Max(LRect.Top, LBlock.Top);
      LRight := Min(LRect.Left + LRect.Width, LBlock.Left + LBlock.Width);
      LBottom := Min(LRect.Top + LRect.Height, LBlock.Top + LBlock.Height);

      if (LRight <= LLeft) or (LBottom <= LTop) then
      begin
        AddRegion(LNext, LRect);
        Continue;
      end;
      AddRegion(LNext, NyxCanvasBounds(LRect.Left, LRect.Top, LLeft - LRect.Left, LRect.Height));
      AddRegion(LNext, NyxCanvasBounds(LRight, LRect.Top, LRect.Left + LRect.Width - LRight, LRect.Height));
      AddRegion(LNext, NyxCanvasBounds(LRect.Left, LRect.Top, LRect.Width, LTop - LRect.Top));
      AddRegion(LNext, NyxCanvasBounds(LRect.Left, LBottom, LRect.Width, LRect.Top + LRect.Height - LBottom));
    end;
    Result := LNext;
  end;
end;

function FitsNavigationRegion(const ACamera: TNyxCanvasCamera;
  const AItem: TNyxCanvasNavigationItem; const ARegion: TNyxCanvasBounds;
  AInsetX, AInsetY: Double): Boolean;
var
  LX: Double;
  LY: Double;
  LScale: Double;
begin
  { This is a post-constraint visibility test, not admission of an off-screen
    point as a portable coordinate. Relative projection can exceed the viewport
    without forcing an otherwise valid far-away item into an integer widget. }
  LX := (AItem.Bounds.Left - ACamera.WorldCenter.X) * ACamera.Zoom + ACamera.ViewportWidth / 2;
  LY := (AItem.Bounds.Top - ACamera.WorldCenter.Y) * ACamera.Zoom + ACamera.ViewportHeight / 2;
  LScale := 1;

  if AItem.Scaling = nccsReflow then
  begin
    LScale := ACamera.Zoom;
  end;
  { Permit only subpixel arithmetic noise at an admitted region edge. }
  Result := (LX >= ARegion.Left + AInsetX - 1.0e-7) and
    (LY >= ARegion.Top + AInsetY - 1.0e-7) and
    (LX + AItem.Bounds.Width * LScale <= ARegion.Left + ARegion.Width - AInsetX + 1.0e-7) and
    (LY + AItem.Bounds.Height * LScale <= ARegion.Top + ARegion.Height - AInsetY + 1.0e-7);
end;

function FitNyxCanvasNavigation(const ACamera: TNyxCanvasCamera;
  const AItems: array of TNyxCanvasNavigationItem;
  const AOccluders: array of TNyxCanvasBounds;
  const AOptions: TNyxCanvasNavigationOptions): TNyxCanvasNavigationResult;
var
  LRegions: TRegions;
  LIndex: Integer;
  LRegion: Integer;
  LIteration: Integer;
  LMinX: Double;
  LMinY: Double;
  LWidth: Double;
  LHeight: Double;
  LInsetX: Double;
  LInsetY: Double;
  LAvailableW: Double;
  LAvailableH: Double;
  LLow: Double;
  LHigh: Double;
  LZoom: Double;
  LBestZoom: Double;
  LBestArea: Double;
  LArea: Double;
  LComplete: Boolean;
  LCandidate: TNyxCanvasCamera;

  { Relative world anchors avoid multiplying enormous absolute positions by
    zoom. Every admitted size/offset is monotone in positive zoom. }
  procedure Measure(AZoom: Double);
  var
    LItem: Integer;
    LScale: Double;
  begin
    LWidth := 0;
    LHeight := 0;
    for LItem := 0 to High(AItems) do
    begin
      LScale := 1;

      if AItems[LItem].Scaling = nccsReflow then
      begin
        LScale := AZoom;
      end;
      LWidth := Max(LWidth, (AItems[LItem].Bounds.Left - LMinX) * AZoom +
        AItems[LItem].Bounds.Width * LScale);
      LHeight := Max(LHeight, (AItems[LItem].Bounds.Top - LMinY) * AZoom +
        AItems[LItem].Bounds.Height * LScale);
    end;
  end;

  function Occluded(AZoom: Double): Boolean;
  var
    LFirst: Integer;
    LSecond: Integer;
    LFirstScale: Double;
    LSecondScale: Double;
    LFirstX: Double;
    LFirstY: Double;
    LSecondX: Double;
    LSecondY: Double;
  begin
    Result := False;
    for LFirst := 0 to High(AItems) do
    begin

      if not AItems[LFirst].Independent then
      begin
        Continue;
      end;
      LFirstScale := 1;

      if AItems[LFirst].Scaling = nccsReflow then
      begin
        LFirstScale := AZoom;
      end;
      LFirstX := (AItems[LFirst].Bounds.Left - LMinX) * AZoom;
      LFirstY := (AItems[LFirst].Bounds.Top - LMinY) * AZoom;
      for LSecond := 0 to High(AItems) do
      begin

        if (LFirst = LSecond) or (AItems[LSecond].Independent and (LSecond < LFirst)) then
        begin
          Continue;
        end;
        LSecondScale := 1;

        if AItems[LSecond].Scaling = nccsReflow then
        begin
          LSecondScale := AZoom;
        end;
        LSecondX := (AItems[LSecond].Bounds.Left - LMinX) * AZoom;
        LSecondY := (AItems[LSecond].Bounds.Top - LMinY) * AZoom;

        if (LFirstX < LSecondX + AItems[LSecond].Bounds.Width * LSecondScale) and
          (LSecondX < LFirstX + AItems[LFirst].Bounds.Width * LFirstScale) and
          (LFirstY < LSecondY + AItems[LSecond].Bounds.Height * LSecondScale) and
          (LSecondY < LFirstY + AItems[LFirst].Bounds.Height * LFirstScale) then
        begin
          Exit(True);
        end;
      end;
    end;
  end;

begin
  LRegions := Regions(ACamera, AOccluders, AOptions);

  if Length(AItems) > MaximumNyxCanvasNavigationItems then
  begin
    raise ENyxCanvasGeometry.Create('Navigation extent window exceeds its declared capacity');
  end;
  Result := Default(TNyxCanvasNavigationResult);
  Result.Camera := ACamera;
  Result.Status := ncnsEmpty;

  if Length(AItems) = 0 then
  begin
    Exit;
  end;
  LMinX := 0;
  LMinY := 0;
  for LIndex := 0 to High(AItems) do
  begin
    NyxCanvasNavigationItem(AItems[LIndex].Bounds, AItems[LIndex].Scaling, AItems[LIndex].Independent);

    if LIndex = 0 then
    begin
      LMinX := AItems[LIndex].Bounds.Left;
      LMinY := AItems[LIndex].Bounds.Top;
    end
    else
    begin
      LMinX := Min(LMinX, AItems[LIndex].Bounds.Left);
      LMinY := Min(LMinY, AItems[LIndex].Bounds.Top);
    end;
  end;
  Result.Status := ncnsLimited;
  LBestZoom := -1;
  LBestArea := -1;
  for LRegion := 0 to High(LRegions) do
  begin
    Measure(ACamera.Limits.Minimum);
    LInsetX := Min(AOptions.Margin, Max(0, (LRegions[LRegion].Width - LWidth) / 2));
    LInsetY := Min(AOptions.Margin, Max(0, (LRegions[LRegion].Height - LHeight) / 2));
    LAvailableW := LRegions[LRegion].Width - 2 * LInsetX;
    LAvailableH := LRegions[LRegion].Height - 2 * LInsetY;
    LComplete := (LWidth <= LAvailableW) and (LHeight <= LAvailableH);
    LLow := ACamera.Limits.Minimum;
    LHigh := ACamera.Zoom;

    if AOptions.MayZoomIn then
    begin
      LHigh := ACamera.Limits.Maximum;
    end;

    if LComplete then
    begin
      for LIteration := 1 to 48 do
      begin
        LZoom := LLow + (LHigh - LLow) / 2;
        Measure(LZoom);

        if (LWidth <= LAvailableW) and (LHeight <= LAvailableH) then
        begin
          LLow := LZoom;
        end
        else
        begin
          LHigh := LZoom;
        end;
      end;
    end;
    LZoom := LLow;
    Measure(LZoom);

    if LComplete and AOptions.CheckDistinct and Occluded(LZoom) then
    begin
      LComplete := False;
    end;
    LArea := LRegions[LRegion].Width * LRegions[LRegion].Height;

    if (ACamera.BoundaryMode = ncbUnbounded) and
      (((Result.Status = ncnsComplete) and not LComplete) or
      ((LComplete = (Result.Status = ncnsComplete)) and
        ((LZoom < LBestZoom) or ((LZoom = LBestZoom) and (LArea <= LBestArea))))) then
    begin
      Continue;
    end;
    LCandidate := ACamera.AtCenter(
      NyxCanvasPoint(LMinX + (LWidth / 2 + ACamera.ViewportWidth / 2 -
        LRegions[LRegion].Left - LRegions[LRegion].Width / 2) / LZoom,
        LMinY + (LHeight / 2 + ACamera.ViewportHeight / 2 -
        LRegions[LRegion].Top - LRegions[LRegion].Height / 2) / LZoom),
      LZoom);

    if ACamera.BoundaryMode <> ncbUnbounded then
    begin
      for LIndex := 0 to High(AItems) do
      begin
        LComplete := LComplete and FitsNavigationRegion(LCandidate, AItems[LIndex],
          LRegions[LRegion], AOptions.Margin, AOptions.Margin);
      end;

      if ((Result.Status = ncnsComplete) and not LComplete) or
        ((LComplete = (Result.Status = ncnsComplete)) and
          ((LZoom < LBestZoom) or ((LZoom = LBestZoom) and (LArea <= LBestArea)))) then
      begin
        Continue;
      end;
    end;
    Result.Camera := LCandidate;
    Result.Region := LRegions[LRegion];
    Result.Status := ncnsLimited;

    if LComplete then
    begin
      Result.Status := ncnsComplete;
    end;
    LBestZoom := LZoom;
    LBestArea := LArea;
  end;
end;

function RevealNyxCanvasNavigation(const ACamera: TNyxCanvasCamera;
  const AItem: TNyxCanvasNavigationItem;
  const AOccluders: array of TNyxCanvasBounds;
  const AOptions: TNyxCanvasNavigationOptions): TNyxCanvasNavigationResult;
var
  LRegions: TRegions;
  LIndex: Integer;
  LAnchor: TNyxCanvasPoint;
  LScale: Double;
  LWidth: Double;
  LHeight: Double;
  LInsetX: Double;
  LInsetY: Double;
  LLeft: Double;
  LTop: Double;
  LRight: Double;
  LBottom: Double;
  LDX: Double;
  LDY: Double;
  LDistance: Double;
  LBestDistance: Double;
  LComplete: Boolean;
  LCandidate: TNyxCanvasCamera;
begin
  LRegions := Regions(ACamera, AOccluders, AOptions);
  NyxCanvasNavigationItem(AItem.Bounds, AItem.Scaling);
  Result := Default(TNyxCanvasNavigationResult);
  Result.Camera := ACamera;
  Result.Status := ncnsLimited;
  LAnchor := ACamera.WorldToViewport(NyxCanvasPoint(AItem.Bounds.Left, AItem.Bounds.Top));
  LScale := 1;

  if AItem.Scaling = nccsReflow then
  begin
    LScale := ACamera.Zoom;
  end;
  LWidth := AItem.Bounds.Width * LScale;
  LHeight := AItem.Bounds.Height * LScale;
  LBestDistance := Infinity;
  for LIndex := 0 to High(LRegions) do
  begin
    LInsetX := Min(AOptions.Margin, Max(0, (LRegions[LIndex].Width - LWidth) / 2));
    LInsetY := Min(AOptions.Margin, Max(0, (LRegions[LIndex].Height - LHeight) / 2));
    LLeft := LRegions[LIndex].Left + LInsetX;
    LTop := LRegions[LIndex].Top + LInsetY;
    LRight := LRegions[LIndex].Left + LRegions[LIndex].Width - LInsetX;
    LBottom := LRegions[LIndex].Top + LRegions[LIndex].Height - LInsetY;
    LComplete := (LWidth <= LRight - LLeft) and (LHeight <= LBottom - LTop);
    LDX := Max(LLeft - LAnchor.X, Min(0, LRight - LAnchor.X - LWidth));
    LDY := Max(LTop - LAnchor.Y, Min(0, LBottom - LAnchor.Y - LHeight));
    LDistance := LDX * LDX + LDY * LDY;

    if ACamera.BoundaryMode <> ncbUnbounded then
    begin
      LCandidate := ACamera.PanBy(LDX, LDY);
      LComplete := LComplete and FitsNavigationRegion(LCandidate, AItem, LRegions[LIndex],
        LInsetX, LInsetY);
      LDistance := Sqr((LCandidate.WorldCenter.X - ACamera.WorldCenter.X) * ACamera.Zoom) +
        Sqr((LCandidate.WorldCenter.Y - ACamera.WorldCenter.Y) * ACamera.Zoom);
    end;

    if ((Result.Status = ncnsComplete) and not LComplete) or
      ((LComplete = (Result.Status = ncnsComplete)) and (LDistance >= LBestDistance)) then
    begin
      Continue;
    end;
    if ACamera.BoundaryMode = ncbUnbounded then
    begin
      Result.Camera := ACamera.PanBy(LDX, LDY);
    end
    else
    begin
      Result.Camera := LCandidate;
    end;
    Result.Region := LRegions[LIndex];
    Result.Status := ncnsLimited;

    if LComplete then
    begin
      Result.Status := ncnsComplete;
    end;
    LBestDistance := LDistance;
  end;
end;

end.
