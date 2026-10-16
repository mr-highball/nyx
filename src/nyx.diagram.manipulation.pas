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
unit nyx.diagram.manipulation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Math, nyx.diagram, nyx.canvas.geometry;

type
  { Closed boundary grips in clockwise order. None is explicit absence; grip
    coordinates are signed world Doubles, independent of widgets or pixel sizes. }
  TNyxDiagramGrip = (ndgNone, ndgNorthWest, ndgNorth, ndgNorthEast, ndgEast,
    ndgSouthEast, ndgSouth, ndgSouthWest, ndgWest);

  { A copied proposal has no document/history or interface ownership. Absence
    cannot be updated; move and resize always derive from their original capture. }
  TNyxDiagramManipulationKind = (ndmkAbsent, ndmkMove, ndmkResize);

  { One endpoint of an existing visual edge. Reconnection retains its identity,
    opposite endpoint, routing, arrows and style; graph membership is separate. }
  TNyxDiagramConnectionEnd = (ndceStart, ndceFinish);

  { Immutable geometry proposal retaining one copied vertex and pointer origin.
    Updating never accumulates rounding and does not mutate its baseline.
    Resizing pins the opposite sides and clamps to the graph's 8..1,000,000 extent
    domain. Invalid coordinates refuse the copy. Optional grid snapping retains
    zero-delta axes and can be bypassed explicitly; no UI or graph is retained. }
  TNyxDiagramManipulation = record
  private
    FKind: TNyxDiagramManipulationKind;
    FGrip: TNyxDiagramGrip;
    FVertex: TNyxDiagramVertex;
    FOrigin: TNyxCanvasPoint;
    FBounds: TNyxCanvasBounds;
    function GetVertex: TNyxDiagramVertex;
    function GetChanged: Boolean;
  public
    function At(const AWorldPoint: TNyxCanvasPoint): TNyxDiagramManipulation; overload;
    function At(const AWorldPoint: TNyxCanvasPoint; const AGrid: TNyxCanvasGrid;
      ABypassGrid: Boolean = False): TNyxDiagramManipulation; overload;
    property Kind: TNyxDiagramManipulationKind read FKind;
    property Grip: TNyxDiagramGrip read FGrip;
    property Vertex: TNyxDiagramVertex read GetVertex;
    property Bounds: TNyxCanvasBounds read FBounds;
    property Changed: Boolean read GetChanged;
  end;

{ Capture validates a copied vertex and a defined origin before returning. }
function NyxDiagramMove(const AVertex: TNyxDiagramVertex;
  const AOrigin: TNyxCanvasPoint): TNyxDiagramManipulation;
function NyxDiagramResize(const AVertex: TNyxDiagramVertex; AGrip: TNyxDiagramGrip;
  const AOrigin: TNyxCanvasPoint): TNyxDiagramManipulation;
{ Closest grip wins within an explicit world tolerance; ties use declared order.
  Hosts choose a logical-pixel tolerance divided by their current camera zoom. }
function NyxDiagramGripPoint(const ABounds: TNyxCanvasBounds;
  AGrip: TNyxDiagramGrip): TNyxCanvasPoint;
function HitNyxDiagramGrip(const ABounds: TNyxCanvasBounds;
  const APoint: TNyxCanvasPoint; ATolerance: Double): TNyxDiagramGrip;
{ Pure edge reconstruction; attached target membership is validated by its graph. }
function ReconnectNyxDiagramEdge(const AEdge: TNyxDiagramEdge;
  AEnd: TNyxDiagramConnectionEnd; const AEndpoint: TNyxDiagramEndpoint): TNyxDiagramEdge;

implementation

{ Integer validation keeps the external enum boundary checked without an
  unreachable exhaustive-case else branch under FPC's range analysis. }
procedure RequireConnectionEnd(AValue: Integer);
begin

  if (AValue < Ord(Low(TNyxDiagramConnectionEnd))) or
    (AValue > Ord(High(TNyxDiagramConnectionEnd))) then
  begin
    raise ENyxDiagram.Create('Choose a known connection endpoint');
  end;
end;

function NyxDiagramGripPoint(const ABounds: TNyxCanvasBounds;
  AGrip: TNyxDiagramGrip): TNyxCanvasPoint;
var
  LX: Double;
  LY: Double;
begin

  if not ABounds.Defined then
  begin
    raise ENyxDiagram.Create('A diagram grip requires defined bounds');
  end;
  LX := ABounds.Left;
  LY := ABounds.Top;
  case AGrip of
    ndgNorthWest: Result := NyxCanvasPoint(LX, LY);
    ndgNorth: Result := NyxCanvasPoint(LX + ABounds.Width / 2, LY);
    ndgNorthEast: Result := NyxCanvasPoint(LX + ABounds.Width, LY);
    ndgEast: Result := NyxCanvasPoint(LX + ABounds.Width, LY + ABounds.Height / 2);
    ndgSouthEast: Result := NyxCanvasPoint(LX + ABounds.Width, LY + ABounds.Height);
    ndgSouth: Result := NyxCanvasPoint(LX + ABounds.Width / 2, LY + ABounds.Height);
    ndgSouthWest: Result := NyxCanvasPoint(LX, LY + ABounds.Height);
    ndgWest: Result := NyxCanvasPoint(LX, LY + ABounds.Height / 2);
  else
    raise ENyxDiagram.Create('Choose a defined diagram resize grip');
  end;
end;

function HitNyxDiagramGrip(const ABounds: TNyxCanvasBounds;
  const APoint: TNyxCanvasPoint; ATolerance: Double): TNyxDiagramGrip;
var
  LGrip: TNyxDiagramGrip;
  LPoint: TNyxCanvasPoint;
  LDistance: Double;
  LBest: Double;
begin

  if not APoint.Defined or IsNan(ATolerance) or IsInfinite(ATolerance) or (ATolerance < 0) then
  begin
    raise ENyxDiagram.Create('Grip hit testing requires a defined point and finite nonnegative tolerance');
  end;
  Result := ndgNone;
  LBest := ATolerance * ATolerance;
  for LGrip := ndgNorthWest to ndgWest do
  begin
    LPoint := NyxDiagramGripPoint(ABounds, LGrip);
    LDistance := Sqr(APoint.X - LPoint.X) + Sqr(APoint.Y - LPoint.Y);

    if (LDistance <= LBest) and ((Result = ndgNone) or (LDistance < LBest)) then
    begin
      Result := LGrip;
      LBest := LDistance;
    end;
  end;
end;

function NyxDiagramMove(const AVertex: TNyxDiagramVertex;
  const AOrigin: TNyxCanvasPoint): TNyxDiagramManipulation;
begin

  if not AOrigin.Defined then
  begin
    raise ENyxDiagram.Create('Manipulation requires a defined world origin');
  end;
  Result := Default(TNyxDiagramManipulation);
  Result.FVertex := TNyxDiagramVertex.FromData(AVertex.ToData);
  Result.FBounds := Result.FVertex.Bounds;
  Result.FOrigin := AOrigin;
  Result.FKind := ndmkMove;
end;

function NyxDiagramResize(const AVertex: TNyxDiagramVertex; AGrip: TNyxDiagramGrip;
  const AOrigin: TNyxCanvasPoint): TNyxDiagramManipulation;
begin
  NyxDiagramGripPoint(AVertex.Bounds, AGrip);
  Result := NyxDiagramMove(AVertex, AOrigin);
  Result.FGrip := AGrip;
  Result.FKind := ndmkResize;
end;

function TNyxDiagramManipulation.GetVertex: TNyxDiagramVertex;
begin

  if FKind = ndmkAbsent then
  begin
    raise ENyxDiagram.Create('An absent manipulation has no vertex');
  end;
  Result := FVertex.AtBounds(FBounds);
end;

function TNyxDiagramManipulation.GetChanged: Boolean;
begin
  Result := (FKind <> ndmkAbsent) and
    ((FBounds.Left <> FVertex.Bounds.Left) or (FBounds.Top <> FVertex.Bounds.Top) or
      (FBounds.Width <> FVertex.Bounds.Width) or (FBounds.Height <> FVertex.Bounds.Height));
end;

function TNyxDiagramManipulation.At(const AWorldPoint: TNyxCanvasPoint): TNyxDiagramManipulation;
begin
  Result := At(AWorldPoint, Default(TNyxCanvasGrid), True);
end;

function TNyxDiagramManipulation.At(const AWorldPoint: TNyxCanvasPoint;
  const AGrid: TNyxCanvasGrid; ABypassGrid: Boolean): TNyxDiagramManipulation;
var
  LDX: Double;
  LDY: Double;
  LLeft: Double;
  LTop: Double;
  LWidth: Double;
  LHeight: Double;
  LAnchor: TNyxCanvasPoint;
  LSnapped: TNyxCanvasPoint;
begin

  if (FKind = ndmkAbsent) or not AWorldPoint.Defined then
  begin
    raise ENyxDiagram.Create('Update a captured manipulation with a defined point');
  end;
  LDX := AWorldPoint.X - FOrigin.X;
  LDY := AWorldPoint.Y - FOrigin.Y;
  LLeft := FVertex.Bounds.Left;
  LTop := FVertex.Bounds.Top;
  LWidth := FVertex.Bounds.Width;
  LHeight := FVertex.Bounds.Height;

  if FKind = ndmkMove then
  begin
    LAnchor := NyxCanvasPoint(LLeft, LTop);
  end
  else
  begin
    LAnchor := NyxDiagramGripPoint(FVertex.Bounds, FGrip);
  end;

  if AGrid.Defined and not ABypassGrid then
  begin
    LSnapped := AGrid.Snap(LAnchor.Offset(LDX, LDY));

    if LDX <> 0 then
    begin
      LDX := LSnapped.X - LAnchor.X;
    end;

    if LDY <> 0 then
    begin
      LDY := LSnapped.Y - LAnchor.Y;
    end;
  end;

  if FKind = ndmkMove then
  begin
    LLeft := LLeft + LDX;
    LTop := LTop + LDY;
  end
  else
  begin

    if FGrip in [ndgNorthWest, ndgWest, ndgSouthWest] then
    begin
      LWidth := Min(1000000.0, Max(8.0, LWidth - LDX));
      LLeft := FVertex.Bounds.Left + FVertex.Bounds.Width - LWidth;
    end
    else if FGrip in [ndgNorthEast, ndgEast, ndgSouthEast] then
    begin
      LWidth := Min(1000000.0, Max(8.0, LWidth + LDX));
    end;

    if FGrip in [ndgNorthWest, ndgNorth, ndgNorthEast] then
    begin
      LHeight := Min(1000000.0, Max(8.0, LHeight - LDY));
      LTop := FVertex.Bounds.Top + FVertex.Bounds.Height - LHeight;
    end
    else if FGrip in [ndgSouthWest, ndgSouth, ndgSouthEast] then
    begin
      LHeight := Min(1000000.0, Max(8.0, LHeight + LDY));
    end;
  end;
  Result := Self;
  Result.FBounds := FVertex.AtBounds(NyxCanvasBounds(LLeft, LTop, LWidth, LHeight)).Bounds;
end;

function ReconnectNyxDiagramEdge(const AEdge: TNyxDiagramEdge;
  AEnd: TNyxDiagramConnectionEnd; const AEndpoint: TNyxDiagramEndpoint): TNyxDiagramEdge;
var
  LStart: TNyxDiagramEndpoint;
  LFinish: TNyxDiagramEndpoint;
begin
  RequireConnectionEnd(Ord(AEnd));
  LStart := AEdge.Start;
  LFinish := AEdge.Finish;
  case AEnd of
    ndceStart: LStart := AEndpoint;
    ndceFinish: LFinish := AEndpoint;
  end;
  Result := NyxDiagramEdge(AEdge.Reference, LStart, LFinish).Routed(AEdge.Routing)
    .Arrows(AEdge.StartArrow, AEdge.FinishArrow).Styled(AEdge.Style);
end;

end.
