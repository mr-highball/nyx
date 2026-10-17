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
unit nyx.diagram;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.text, nyx.bytes, nyx.data, nyx.canvas.geometry, nyx.canvas.types,
  nyx.drawing, nyx.design.tokens.core, nyx.diagram.content;

const
  MaximumNyxDiagramVertices = 256;
  MaximumNyxDiagramEdges = 512;
  MaximumNyxDiagramPortsPerVertex = 64;
  MaximumNyxDiagramBytes = 1048576;
  MaximumNyxDiagramCoordinate = 1.0e9;

type
  { Refusal of a visual graph candidate; immutable accepted plans stay usable.
    This category conveys no executable behavior or application capability. }
  ENyxDiagram = class(EArgumentException);

  { Stable case-sensitive visual shape identity, independent of scene items,
    controls and executable graph steps. A default reference is absent. Names
    follow the canvas name contract; removing/reusing one is explicit intent. }
  TNyxDiagramVertexRef = record
  private
    FName: TNyxText;
  public
    function Defined: Boolean;
    property Name: TNyxText read FName;
  end;

  { Stable visual edge identity in its own namespace. Parallel edges may have
    identical endpoints but distinct references; the reference retains no plan. }
  TNyxDiagramEdgeRef = record
  private
    FName: TNyxText;
  public
    function Defined: Boolean;
    property Name: TNyxText read FName;
  end;

  { Closed outline recipes. Reusable/image content and named physical ports
    attach independently; rotation and group transforms are not modeled here. }
  TNyxDiagramShape = (ndsRectangle, ndsEllipse, ndsDiamond);

  { Automatic uses the boundary toward the other end; explicit sides remain at
    their boundary midpoint after geometry changes. Named ports use exact ratios. }
  TNyxDiagramAttachment = (ndaAutomatic, ndaLeft, ndaTop, ndaRight, ndaBottom);

  { Absence refuses admission. Free points own world coordinates; an attached
    end owns only an exact shape name and attachment, never a shape interface.
    A port end additionally names a physical connection local to that shape. }
  TNyxDiagramEndpointKind = (ndekAbsent, ndekFree, ndekAttached, ndekPort);

  { Initial deterministic routing policies. Orthogonal routes honor outward
    side stubs but do not promise obstacle avoidance or optimal routing. }
  TNyxDiagramRouting = (ndrStraight, ndrOrthogonal);

  { Independent end decorations. Open arrows are presentation only; they never
    give a visual edge executable direction, a value type or data authority. }
  TNyxDiagramArrow = (ndaNone, ndaOpen);

  { Retiring a connected vertex refuses by default. Explicit incident removal
    retires exactly those edges in the returned copy and preserves other order. }
  TNyxDiagramRemoval = (ndrmRequireDisconnected, ndrmIncidentEdges);

  { Stable case-sensitive physical connection identity, local to one vertex.
    It owns only text and retains no shape, graph, control or executable route. }
  TNyxDiagramPortRef = record
  private
    FName: TNyxText;
  public
    function Defined: Boolean;
    property Name: TNyxText read FName;
  end;

  { Immutable named physical attachment. Position contains width/height ratios
    relative to the vertex's top-left; outside ratios are allowed, but resolved
    world coordinates must remain finite and inside the diagram budget. Direction
    controls the orthogonal exit, with Automatic choosing an outward side.
    Caption is single-line Unicode help, never executable event/data authority. }
  TNyxDiagramPort = record
  private
    FReference: TNyxDiagramPortRef;
    FPosition: TNyxCanvasPoint;
    FDirection: TNyxDiagramAttachment;
    FCaption: TNyxText;
  public
    function Directed(AValue: TNyxDiagramAttachment): TNyxDiagramPort;
    function Captioned(const AText: TNyxText): TNyxDiagramPort;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDiagramPort; static;
    property Reference: TNyxDiagramPortRef read FReference;
    property Position: TNyxCanvasPoint read FPosition;
    property Direction: TNyxDiagramAttachment read FDirection;
    property Caption: TNyxText read FCaption;
  end;

  { Immutable shape specification. Geometry has positive 8..1,000,000 world-unit
    extents inside +/-1e9; captions are single-line Unicode up to 256 scalars.
    Styles are copied typed drawing values, retaining semantic theme roles.
    Optional content retains typed resource/reusable references only; the graph
    never owns live control roots or a document. Content remains attached through
    immutable geometry/caption/style changes. }
  TNyxDiagramVertex = record
  private
    FReference: TNyxDiagramVertexRef;
    FShape: TNyxDiagramShape;
    FBounds: TNyxCanvasBounds;
    FCaption: TNyxText;
    FStyle: TNyxDrawingStyle;
    FContent: TNyxDiagramContent;
    FPorts: array of TNyxDiagramPort;
    function GetPortCount: Integer;
  public
    { Strict standalone persistence boundary; graph membership remains separate. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDiagramVertex; static;
    function AtBounds(const ABounds: TNyxCanvasBounds): TNyxDiagramVertex;
    function Captioned(const AText: TNyxText): TNyxDiagramVertex;
    function Styled(const AStyle: TNyxDrawingStyle): TNyxDiagramVertex;
    { Capture a validated value only; resource/reusable availability is checked
      against the owning document, and physical construction happens at mount. }
    function WithContent(const AContent: TNyxDiagramContent): TNyxDiagramVertex;
    { Clear only content intent, retaining the shape's identity and other data. }
    function WithoutContent: TNyxDiagramVertex;
    { Upsert preserves port order; new references append. Edits detach storage.
      Removing a still-connected port is refused by whole-graph admission. }
    function WithPort(const APort: TNyxDiagramPort): TNyxDiagramVertex;
    function WithoutPort(const AReference: TNyxDiagramPortRef): TNyxDiagramVertex;
    function Port(AIndex: Integer): TNyxDiagramPort;
    function FindPort(const AReference: TNyxDiagramPortRef): TNyxDiagramPort;
    property Reference: TNyxDiagramVertexRef read FReference;
    property Shape: TNyxDiagramShape read FShape;
    property Bounds: TNyxCanvasBounds read FBounds;
    property Caption: TNyxText read FCaption;
    property Style: TNyxDrawingStyle read FStyle;
    property Content: TNyxDiagramContent read FContent;
    property PortCount: Integer read GetPortCount;
  end;

  { Copied endpoint intent. Free uses Point; Attached uses Vertex/Attachment;
    Port uses Vertex/Port. No endpoint retains a shape or graph interface.
    Resolution reads the current immutable plan and refuses missing membership. }
  TNyxDiagramEndpoint = record
  private
    FKind: TNyxDiagramEndpointKind;
    FVertex: TNyxDiagramVertexRef;
    FPoint: TNyxCanvasPoint;
    FAttachment: TNyxDiagramAttachment;
    FPort: TNyxDiagramPortRef;
  public
    function UsesVertex: Boolean;
    property Kind: TNyxDiagramEndpointKind read FKind;
    property Vertex: TNyxDiagramVertexRef read FVertex;
    property Point: TNyxCanvasPoint read FPoint;
    property Attachment: TNyxDiagramAttachment read FAttachment;
    property Port: TNyxDiagramPortRef read FPort;
  end;

  { Detached nearest port hit; reverse paint order breaks exact distance ties.
    An absent pair means no match;
    point/distance are values and no view or graph owner is retained. }
  TNyxDiagramPortHit = record
    Vertex: TNyxDiagramVertexRef;
    Port: TNyxDiagramPortRef;
    Point: TNyxCanvasPoint;
    Distance: Double;
    function Defined: Boolean;
  end;

  { Immutable visual connection. Endpoints resolve only against its admitted
    plan. Self-loops require Orthogonal and distinct resolved attachment points;
    automatic ends select right/top.
    Stroke fill is ignored in edge presentation; stroke/width remain typed. }
  TNyxDiagramEdge = record
  private
    FReference: TNyxDiagramEdgeRef;
    FStart: TNyxDiagramEndpoint;
    FFinish: TNyxDiagramEndpoint;
    FRouting: TNyxDiagramRouting;
    FStartArrow: TNyxDiagramArrow;
    FFinishArrow: TNyxDiagramArrow;
    FStyle: TNyxDrawingStyle;
  public
    { Standalone typed edge intent. Attached names resolve only when the owning
      graph admits the edge; this descriptor grants no executable connection. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDiagramEdge; static;
    function Routed(AValue: TNyxDiagramRouting): TNyxDiagramEdge;
    function Arrows(AStart, AFinish: TNyxDiagramArrow): TNyxDiagramEdge;
    function Styled(const AStyle: TNyxDrawingStyle): TNyxDiagramEdge;
    property Reference: TNyxDiagramEdgeRef read FReference;
    property Start: TNyxDiagramEndpoint read FStart;
    property Finish: TNyxDiagramEndpoint read FFinish;
    property Routing: TNyxDiagramRouting read FRouting;
    property StartArrow: TNyxDiagramArrow read FStartArrow;
    property FinishArrow: TNyxDiagramArrow read FFinishArrow;
    property Style: TNyxDrawingStyle read FStyle;
  end;

  { Detached immutable polyline in world units. Indexed reads return values;
    no caller array, plan or renderer is retained. Consecutive duplicates are
    removed during resolution, retaining a point for a degenerate free edge. }
  TNyxDiagramRoute = record
  private
    FPoints: array of TNyxCanvasPoint;
    procedure Add(const APoint: TNyxCanvasPoint);
    function GetCount: Integer;
  public
    function Point(AIndex: Integer): TNyxCanvasPoint;
    property Count: Integer read GetCount;
  end;

  { Immutable visual graph, safe to publish after construction by ordinary
    worker handoff. Edits return independent plans and refuse before publication.
    Upserts preserve existing order; new names append. Indexed reads are bounded
    values. Every attached endpoint must exist, including after decoding.
    Pick returns the last matching shape/edge in paint order, independently:
    callers choose shape-vs-edge priority. No control, application, callbacks,
    history owner, source compiler, scheduler or executable graph is retained. }
  INyxDiagram = interface(IInterface)
    ['{E398E619-547D-4F77-9329-5A1AB7082C01}']
    function GetVertexCount: Integer;
    function GetEdgeCount: Integer;
    function Vertex(AIndex: Integer): TNyxDiagramVertex;
    function Edge(AIndex: Integer): TNyxDiagramEdge;
    function FindVertex(const AReference: TNyxDiagramVertexRef): TNyxDiagramVertex;
    function FindEdge(const AReference: TNyxDiagramEdgeRef): TNyxDiagramEdge;
    function WithVertex(const AVertex: TNyxDiagramVertex): INyxDiagram;
    function WithEdge(const AEdge: TNyxDiagramEdge): INyxDiagram;
    function WithoutVertex(const AReference: TNyxDiagramVertexRef;
      APolicy: TNyxDiagramRemoval = ndrmRequireDisconnected): INyxDiagram;
    function WithoutEdge(const AReference: TNyxDiagramEdgeRef): INyxDiagram;
    function Route(const AReference: TNyxDiagramEdgeRef): TNyxDiagramRoute;
    function PickVertex(const APoint: TNyxCanvasPoint): TNyxDiagramVertexRef;
    function PickEdge(const APoint: TNyxCanvasPoint;
      ATolerance: Double = 6): TNyxDiagramEdgeRef;
    { Complete canonical descriptor with its UTF-8 byte refusal. Built-ins may
      atomically record that success for live capture, without changing intent. }
    function ToData: TNyxDataValue;
    property VertexCount: Integer read GetVertexCount;
    property EdgeCount: Integer read GetEdgeCount;
  end;

function NyxDiagramVertexRef(const AName: TNyxText): TNyxDiagramVertexRef;
function NyxDiagramEdgeRef(const AName: TNyxText): TNyxDiagramEdgeRef;
function NyxDiagramPortRef(const AName: TNyxText): TNyxDiagramPortRef;
function NyxDiagramPort(const AReference: TNyxDiagramPortRef;
  AX, AY: Double): TNyxDiagramPort;
function NyxDiagramPortPoint(const AVertex: TNyxDiagramVertex;
  const AReference: TNyxDiagramPortRef): TNyxCanvasPoint;
function PickNyxDiagramPort(const ADiagram: INyxDiagram;
  const APoint: TNyxCanvasPoint; ATolerance: Double): TNyxDiagramPortHit;
{ Remove one exact physical port. Default refuses connected membership; the
  explicit incident policy removes only edges attached to that vertex/port pair.
  The source graph/order remain exact on failure and independent on success. }
function RemoveNyxDiagramPort(const ADiagram: INyxDiagram;
  const AVertex: TNyxDiagramVertexRef; const APort: TNyxDiagramPortRef;
  APolicy: TNyxDiagramRemoval = ndrmRequireDisconnected): INyxDiagram;
function NyxDiagramVertex(const AReference: TNyxDiagramVertexRef;
  AShape: TNyxDiagramShape; const ABounds: TNyxCanvasBounds): TNyxDiagramVertex;
function NyxDiagramAttached(const AVertex: TNyxDiagramVertexRef;
  AAttachment: TNyxDiagramAttachment = ndaAutomatic): TNyxDiagramEndpoint;
function NyxDiagramFreeEnd(const APoint: TNyxCanvasPoint): TNyxDiagramEndpoint;
function NyxDiagramPortEnd(const AVertex: TNyxDiagramVertexRef;
  const APort: TNyxDiagramPortRef): TNyxDiagramEndpoint;
function NyxDiagramEdge(const AReference: TNyxDiagramEdgeRef;
  const AStart, AFinish: TNyxDiagramEndpoint): TNyxDiagramEdge;
function NewNyxDiagram: INyxDiagram;
{ Capture complete immutable intent for live read-only use. Exact built-in plans
  are retained after their canonical byte budget has passed. An uncertified
  capture serializes once; concurrent initial readers may duplicate that pure
  work. Its monotonic thread-safe certificate retains no wire text and never
  changes graph intent. Other creator
  implementations are captured once through their complete ToData/strict decoder;
  no live creator object is retained. Nil refuses before returning a candidate.
  Sharing an immutable plan never shares subsequent fluent edits or UI owners. }
function CaptureNyxDiagram(const ADiagram: INyxDiagram): INyxDiagram;
{ Strict version-one/two/three persistence boundary: exact closed fields, numeric enum
  ordinals, finite geometry and bounded membership. Unknown/duplicate/dangling
  members refuse the whole candidate; this is not draw.io compatibility. Version
  one retains its exact no-content bytes; richer vertex content requires two,
  and named ports/endpoints require three. No older descriptor is reinterpreted. }
function NyxDiagramFromData(const AData: TNyxDataValue): INyxDiagram;

implementation

type
  { Private immutable graph storage. Fork copies both arrays before an edit;
    records contain no interface fields. A failed fork is freed by its local
    interface owner. Linear name lookup is bounded by the declared capacities. }
  TDiagram = class(TInterfacedObject, INyxDiagram)
  private
    FVertices: array of TNyxDiagramVertex;
    FEdges: array of TNyxDiagramEdge;
    { Only complete canonical byte admission sets this monotonic certificate.
      Fork leaves a fresh zero; native readers/writers use atomic operations.
      No serialized payload, caller or UI owner is retained by the certificate. }
    FWireAdmitted: LongInt;
    function CaptureReady: Boolean;
    function VertexIndex(const AReference: TNyxDiagramVertexRef): Integer;
    function EdgeIndex(const AReference: TNyxDiagramEdgeRef): Integer;
    function Fork: TDiagram;
    procedure ValidateRoutes;
  public
    function GetVertexCount: Integer;
    function GetEdgeCount: Integer;
    function Vertex(AIndex: Integer): TNyxDiagramVertex;
    function Edge(AIndex: Integer): TNyxDiagramEdge;
    function FindVertex(const AReference: TNyxDiagramVertexRef): TNyxDiagramVertex;
    function FindEdge(const AReference: TNyxDiagramEdgeRef): TNyxDiagramEdge;
    function WithVertex(const AVertex: TNyxDiagramVertex): INyxDiagram;
    function WithEdge(const AEdge: TNyxDiagramEdge): INyxDiagram;
    function WithoutVertex(const AReference: TNyxDiagramVertexRef;
      APolicy: TNyxDiagramRemoval): INyxDiagram;
    function WithoutEdge(const AReference: TNyxDiagramEdgeRef): INyxDiagram;
    function Route(const AReference: TNyxDiagramEdgeRef): TNyxDiagramRoute;
    function PickVertex(const APoint: TNyxCanvasPoint): TNyxDiagramVertexRef;
    function PickEdge(const APoint: TNyxCanvasPoint; ATolerance: Double): TNyxDiagramEdgeRef;
    function ToData: TNyxDataValue;
  end;

procedure RequirePoint(const APoint: TNyxCanvasPoint);
begin

  if not APoint.Defined or (Abs(APoint.X) > MaximumNyxDiagramCoordinate) or
    (Abs(APoint.Y) > MaximumNyxDiagramCoordinate) then
  begin
    raise ENyxDiagram.Create('Diagram points require finite coordinates inside +/-1e9');
  end;
end;

procedure RequireBounds(const ABounds: TNyxCanvasBounds);
begin

  if not ABounds.Defined or (ABounds.Width < 8) or (ABounds.Height < 8) or
    (ABounds.Width > 1000000) or (ABounds.Height > 1000000) then
  begin
    raise ENyxDiagram.Create('Diagram shapes require 8..1000000 world-unit extents');
  end;
  RequirePoint(NyxCanvasPoint(ABounds.Left, ABounds.Top));
  RequirePoint(NyxCanvasPoint(ABounds.Left + ABounds.Width, ABounds.Top + ABounds.Height));
end;

procedure RequireOrdinal(AValue, AMaximum: Integer);
begin

  if (AValue < 0) or (AValue > AMaximum) then
  begin
    raise ENyxDiagram.Create('Unknown diagram choice');
  end;
end;

{ Shared caption admission preserves exact Unicode without normalization. }
procedure RequireCaption(const AText: TNyxText);
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(AText) do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) or (LScalar < 32) or
      ((LScalar >= 127) and (LScalar <= 159)) then
    begin
      raise ENyxDiagram.Create('Diagram captions require single-line Unicode');
    end;
    Inc(LCount);

    if LCount > 256 then
    begin
      raise ENyxDiagram.Create('Diagram captions admit at most 256 Unicode scalars');
    end;
  end;
end;

function TNyxDiagramVertexRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function TNyxDiagramEdgeRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function NyxDiagramVertexRef(const AName: TNyxText): TNyxDiagramVertexRef;
begin
  Result.FName := NyxCanvasItem(AName).Name;
end;

function NyxDiagramEdgeRef(const AName: TNyxText): TNyxDiagramEdgeRef;
begin
  Result.FName := NyxCanvasItem(AName).Name;
end;

function NyxDiagramVertex(const AReference: TNyxDiagramVertexRef;
  AShape: TNyxDiagramShape; const ABounds: TNyxCanvasBounds): TNyxDiagramVertex;
begin
  Result := Default(TNyxDiagramVertex);
  Result.FReference := NyxDiagramVertexRef(AReference.Name);
  RequireOrdinal(Ord(AShape), Ord(High(TNyxDiagramShape)));
  RequireBounds(ABounds);
  Result.FShape := AShape;
  Result.FBounds := ABounds;
  Result.FStyle := NyxDrawingStyle.Fill(NyxThemePaint(ntcSurface))
    .Stroke(NyxThemePaint(ntcAccent), 2);
end;

function TNyxDiagramVertex.AtBounds(const ABounds: TNyxCanvasBounds): TNyxDiagramVertex;
var
  LIndex: Integer;
begin
  RequireBounds(ABounds);
  Result := Self;
  Result.FBounds := ABounds;
  for LIndex := 0 to Result.PortCount - 1 do
  begin
    NyxDiagramPortPoint(Result, Result.Port(LIndex).Reference);
  end;
end;

function TNyxDiagramVertex.Captioned(const AText: TNyxText): TNyxDiagramVertex;
begin
  RequireCaption(AText);
  Result := Self;
  Result.FCaption := AText;
end;

function TNyxDiagramVertex.Styled(const AStyle: TNyxDrawingStyle): TNyxDiagramVertex;
begin
  Result := Self;
  Result.FStyle := TNyxDrawingStyle.FromData(AStyle.ToData);
end;

function TNyxDiagramVertex.WithContent(const AContent: TNyxDiagramContent): TNyxDiagramVertex;
begin
  Result := Self;
  Result.FContent := TNyxDiagramContent.FromData(AContent.ToData);
end;

function TNyxDiagramVertex.WithoutContent: TNyxDiagramVertex;
begin
  Result := Self;
  Result.FContent := NyxNoDiagramContent;
end;

function NyxDiagramAttached(const AVertex: TNyxDiagramVertexRef;
  AAttachment: TNyxDiagramAttachment): TNyxDiagramEndpoint;
begin
  Result := Default(TNyxDiagramEndpoint);
  Result.FVertex := NyxDiagramVertexRef(AVertex.Name);
  RequireOrdinal(Ord(AAttachment), Ord(High(TNyxDiagramAttachment)));
  Result.FKind := ndekAttached;
  Result.FAttachment := AAttachment;
end;

function NyxDiagramFreeEnd(const APoint: TNyxCanvasPoint): TNyxDiagramEndpoint;
begin
  RequirePoint(APoint);
  Result := Default(TNyxDiagramEndpoint);
  Result.FKind := ndekFree;
  Result.FPoint := APoint;
end;

procedure RequireEndpoint(const AValue: TNyxDiagramEndpoint);
begin
  case AValue.Kind of
    ndekFree: NyxDiagramFreeEnd(AValue.Point);
    ndekAttached: NyxDiagramAttached(AValue.Vertex, AValue.Attachment);
    ndekPort: NyxDiagramPortEnd(AValue.Vertex, AValue.Port);
  else
    raise ENyxDiagram.Create('Diagram edge endpoint is absent');
  end;
end;

function NyxDiagramEdge(const AReference: TNyxDiagramEdgeRef;
  const AStart, AFinish: TNyxDiagramEndpoint): TNyxDiagramEdge;
begin
  RequireEndpoint(AStart);
  RequireEndpoint(AFinish);
  Result := Default(TNyxDiagramEdge);
  Result.FReference := NyxDiagramEdgeRef(AReference.Name);
  Result.FStart := AStart;
  Result.FFinish := AFinish;
  Result.FFinishArrow := ndaOpen;
  Result.FStyle := NyxDrawingStyle.Stroke(NyxThemePaint(ntcMuted), 2);
end;

function TNyxDiagramEdge.Routed(AValue: TNyxDiagramRouting): TNyxDiagramEdge;
begin
  RequireOrdinal(Ord(AValue), Ord(High(TNyxDiagramRouting)));
  Result := Self;
  Result.FRouting := AValue;
end;

function TNyxDiagramEdge.Arrows(AStart, AFinish: TNyxDiagramArrow): TNyxDiagramEdge;
begin
  RequireOrdinal(Ord(AStart), Ord(High(TNyxDiagramArrow)));
  RequireOrdinal(Ord(AFinish), Ord(High(TNyxDiagramArrow)));
  Result := Self;
  Result.FStartArrow := AStart;
  Result.FFinishArrow := AFinish;
end;

function TNyxDiagramEdge.Styled(const AStyle: TNyxDrawingStyle): TNyxDiagramEdge;
begin
  Result := Self;
  Result.FStyle := TNyxDrawingStyle.FromData(AStyle.ToData);
end;

procedure TNyxDiagramRoute.Add(const APoint: TNyxCanvasPoint);
var
  LCount: Integer;
begin
  LCount := Length(FPoints);

  if (LCount > 0) and (FPoints[LCount - 1].X = APoint.X) and
    (FPoints[LCount - 1].Y = APoint.Y) then
  begin
    Exit;
  end;
  SetLength(FPoints, LCount + 1);
  FPoints[LCount] := APoint;
end;

function TNyxDiagramRoute.GetCount: Integer;
begin
  Result := Length(FPoints);
end;

function TNyxDiagramRoute.Point(AIndex: Integer): TNyxCanvasPoint;
begin

  if (AIndex < 0) or (AIndex >= Length(FPoints)) then
  begin
    raise ENyxDiagram.Create('Diagram route point is outside its range');
  end;
  Result := FPoints[AIndex];
end;

function NewNyxDiagram: INyxDiagram;
begin
  Result := TDiagram.Create;
end;

function CaptureNyxDiagram(const ADiagram: INyxDiagram): INyxDiagram;
var
  LStorage: TDiagram;
  LIdentity: INyxDiagram;
begin

  if ADiagram = nil then
  begin
    raise ENyxDiagram.Create('Diagram capture requires a complete plan');
  end;

  if ADiagram is TDiagram then
  begin
    { Aggregating creators can forward the object-instance query. Require the
      private object's own interface too, before trusting its immutable storage. }
    LStorage := ADiagram as TDiagram;
    LIdentity := LStorage;

    if (LStorage.ClassType = TDiagram) and (LIdentity = ADiagram) then
    begin

      if not LStorage.CaptureReady then
      begin
        { Fluent membership can outgrow the complete persistence byte budget.
          Preserve the old ToData refusal before retaining that owned plan. }
        ADiagram.ToData;
      end;
      Result := ADiagram;
      Exit;
    end;
  end;
  Result := NyxDiagramFromData(ADiagram.ToData);
  { Decoder normalization may change numeric spellings. Admit the complete
    canonical descriptor too, without invoking the creator a second time. }
  Result.ToData;
end;

function TDiagram.CaptureReady: Boolean;
begin
  {$ifdef PAS2JS}
  Result := FWireAdmitted = 1;
  {$else}
  Result := InterlockedCompareExchange(FWireAdmitted, 0, 0) = 1;
  {$endif}
end;

function TDiagram.GetVertexCount: Integer;
begin
  Result := Length(FVertices);
end;

function TDiagram.GetEdgeCount: Integer;
begin
  Result := Length(FEdges);
end;

function TDiagram.VertexIndex(const AReference: TNyxDiagramVertexRef): Integer;
var
  LIndex: Integer;
begin
  NyxDiagramVertexRef(AReference.Name);
  for LIndex := 0 to High(FVertices) do
  begin

    if FVertices[LIndex].Reference.Name = AReference.Name then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TDiagram.EdgeIndex(const AReference: TNyxDiagramEdgeRef): Integer;
var
  LIndex: Integer;
begin
  NyxDiagramEdgeRef(AReference.Name);
  for LIndex := 0 to High(FEdges) do
  begin

    if FEdges[LIndex].Reference.Name = AReference.Name then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TDiagram.Vertex(AIndex: Integer): TNyxDiagramVertex;
begin

  if (AIndex < 0) or (AIndex >= Length(FVertices)) then
  begin
    raise ENyxDiagram.Create('Diagram vertex is outside its range');
  end;
  Result := FVertices[AIndex];
end;

function TDiagram.Edge(AIndex: Integer): TNyxDiagramEdge;
begin

  if (AIndex < 0) or (AIndex >= Length(FEdges)) then
  begin
    raise ENyxDiagram.Create('Diagram edge is outside its range');
  end;
  Result := FEdges[AIndex];
end;

function TDiagram.FindVertex(const AReference: TNyxDiagramVertexRef): TNyxDiagramVertex;
var
  LIndex: Integer;
begin
  LIndex := VertexIndex(AReference);

  if LIndex < 0 then
  begin
    raise ENyxDiagram.Create('Diagram vertex does not exist');
  end;
  Result := FVertices[LIndex];
end;

function TDiagram.FindEdge(const AReference: TNyxDiagramEdgeRef): TNyxDiagramEdge;
var
  LIndex: Integer;
begin
  LIndex := EdgeIndex(AReference);

  if LIndex < 0 then
  begin
    raise ENyxDiagram.Create('Diagram edge does not exist');
  end;
  Result := FEdges[LIndex];
end;

function TDiagram.Fork: TDiagram;
begin
  Result := TDiagram.Create;
  Result.FVertices := Copy(FVertices, 0, Length(FVertices));
  Result.FEdges := Copy(FEdges, 0, Length(FEdges));
end;

procedure TDiagram.ValidateRoutes;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FEdges) do
  begin
    Route(FEdges[LIndex].Reference);
  end;
end;

function TDiagram.WithVertex(const AVertex: TNyxDiagramVertex): INyxDiagram;
var
  LCandidate: TDiagram;
  LIndex: Integer;
  LVertex: TNyxDiagramVertex;
  LPortIndex: Integer;
begin
  LVertex := NyxDiagramVertex(AVertex.Reference, AVertex.Shape, AVertex.Bounds)
    .Captioned(AVertex.Caption).Styled(AVertex.Style).WithContent(AVertex.Content);
  for LPortIndex := 0 to AVertex.PortCount - 1 do
  begin
    LVertex := LVertex.WithPort(AVertex.Port(LPortIndex));
  end;
  LIndex := VertexIndex(LVertex.Reference);

  if (LIndex < 0) and (Length(FVertices) >= MaximumNyxDiagramVertices) then
  begin
    raise ENyxDiagram.Create('Diagram vertex capacity exceeded');
  end;
  LCandidate := Fork;
  Result := LCandidate;

  if LIndex < 0 then
  begin
    LIndex := Length(LCandidate.FVertices);
    SetLength(LCandidate.FVertices, LIndex + 1);
  end;
  LCandidate.FVertices[LIndex] := LVertex;
  LCandidate.ValidateRoutes;
end;

function TDiagram.WithEdge(const AEdge: TNyxDiagramEdge): INyxDiagram;
var
  LCandidate: TDiagram;
  LIndex: Integer;
  LEdge: TNyxDiagramEdge;
begin
  LEdge := NyxDiagramEdge(AEdge.Reference, AEdge.Start, AEdge.Finish)
    .Routed(AEdge.Routing).Arrows(AEdge.StartArrow, AEdge.FinishArrow).Styled(AEdge.Style);
  LIndex := EdgeIndex(LEdge.Reference);

  if (LIndex < 0) and (Length(FEdges) >= MaximumNyxDiagramEdges) then
  begin
    raise ENyxDiagram.Create('Diagram edge capacity exceeded');
  end;
  LCandidate := Fork;
  Result := LCandidate;

  if LIndex < 0 then
  begin
    LIndex := Length(LCandidate.FEdges);
    SetLength(LCandidate.FEdges, LIndex + 1);
  end;
  LCandidate.FEdges[LIndex] := LEdge;
  LCandidate.Route(LEdge.Reference);
end;

function Incident(const AEdge: TNyxDiagramEdge; const AName: TNyxText): Boolean;
begin
  Result := (AEdge.Start.UsesVertex and (AEdge.Start.Vertex.Name = AName)) or
    (AEdge.Finish.UsesVertex and (AEdge.Finish.Vertex.Name = AName));
end;

function TDiagram.WithoutVertex(const AReference: TNyxDiagramVertexRef;
  APolicy: TNyxDiagramRemoval): INyxDiagram;
var
  LCandidate: TDiagram;
  LIndex: Integer;
  LRemove: Integer;
  LWrite: Integer;
begin
  RequireOrdinal(Ord(APolicy), Ord(High(TNyxDiagramRemoval)));
  LRemove := VertexIndex(AReference);

  if LRemove < 0 then
  begin
    raise ENyxDiagram.Create('Cannot remove an absent diagram vertex');
  end;
  for LIndex := 0 to High(FEdges) do
  begin

    if Incident(FEdges[LIndex], AReference.Name) and (APolicy = ndrmRequireDisconnected) then
    begin
      raise ENyxDiagram.Create('Disconnect incident edges or choose explicit incident removal');
    end;
  end;
  LCandidate := Fork;
  Result := LCandidate;
  for LIndex := LRemove to High(LCandidate.FVertices) - 1 do
  begin
    LCandidate.FVertices[LIndex] := LCandidate.FVertices[LIndex + 1];
  end;
  SetLength(LCandidate.FVertices, Length(LCandidate.FVertices) - 1);
  LWrite := 0;
  for LIndex := 0 to High(FEdges) do
  begin

    if not Incident(FEdges[LIndex], AReference.Name) then
    begin
      LCandidate.FEdges[LWrite] := FEdges[LIndex];
      Inc(LWrite);
    end;
  end;
  SetLength(LCandidate.FEdges, LWrite);
end;

function TDiagram.WithoutEdge(const AReference: TNyxDiagramEdgeRef): INyxDiagram;
var
  LCandidate: TDiagram;
  LIndex: Integer;
  LRemove: Integer;
begin
  LRemove := EdgeIndex(AReference);

  if LRemove < 0 then
  begin
    raise ENyxDiagram.Create('Cannot remove an absent diagram edge');
  end;
  LCandidate := Fork;
  Result := LCandidate;
  for LIndex := LRemove to High(LCandidate.FEdges) - 1 do
  begin
    LCandidate.FEdges[LIndex] := LCandidate.FEdges[LIndex + 1];
  end;
  SetLength(LCandidate.FEdges, Length(LCandidate.FEdges) - 1);
end;

function EndpointCenter(const ADiagram: INyxDiagram;
  const AEnd: TNyxDiagramEndpoint): TNyxCanvasPoint;
begin

  if AEnd.Kind = ndekPort then
  begin
    Result := NyxDiagramPortPoint(ADiagram.FindVertex(AEnd.Vertex), AEnd.Port);
  end
  else if AEnd.Kind = ndekFree then
  begin
    Result := AEnd.Point;
  end
  else
  begin
    Result := ADiagram.FindVertex(AEnd.Vertex).Bounds.Center;
  end;
end;

function Attach(const ADiagram: INyxDiagram; const AEnd: TNyxDiagramEndpoint;
  const AToward: TNyxCanvasPoint; AOrthogonal: Boolean;
  ALoopSide: TNyxDiagramAttachment; out ANormal: TNyxCanvasPoint): TNyxCanvasPoint;
var
  LVertex: TNyxDiagramVertex;
  LCenter: TNyxCanvasPoint;
  LSide: TNyxDiagramAttachment;
  LDX: Double;
  LDY: Double;
  LScale: Double;
  LPort: TNyxDiagramPort;
begin
  ANormal := NyxCanvasPoint(0, 0);

  if AEnd.Kind = ndekFree then
  begin
    Exit(AEnd.Point);
  end;
  LVertex := ADiagram.FindVertex(AEnd.Vertex);
  LCenter := LVertex.Bounds.Center;
  LSide := AEnd.Attachment;

  if AEnd.Kind = ndekPort then
  begin
    LPort := LVertex.FindPort(AEnd.Port);
    Result := NyxDiagramPortPoint(LVertex, AEnd.Port);
    LSide := LPort.Direction;

    if LSide = ndaAutomatic then
    begin
      LDX := LPort.Position.X - 0.5;
      LDY := LPort.Position.Y - 0.5;

      if (LDX = 0) and (LDY = 0) then
      begin
        LDX := AToward.X - Result.X;
        LDY := AToward.Y - Result.Y;
      end;

      if (LDX = 0) and (LDY = 0) and (ALoopSide <> ndaAutomatic) then
      begin
        LSide := ALoopSide;
      end
      else if Abs(LDX) >= Abs(LDY) then
      begin

        if LDX < 0 then
        begin
          LSide := ndaLeft;
        end
        else
        begin
          LSide := ndaRight;
        end;
      end
      else if LDY < 0 then
      begin
        LSide := ndaTop;
      end
      else
      begin
        LSide := ndaBottom;
      end;
    end;
    case LSide of
      ndaLeft: ANormal := NyxCanvasPoint(-1, 0);
      ndaTop: ANormal := NyxCanvasPoint(0, -1);
      ndaRight: ANormal := NyxCanvasPoint(1, 0);
      ndaBottom: ANormal := NyxCanvasPoint(0, 1);
    else
      raise ENyxDiagram.Create('A named port requires a resolved exit direction');
    end;
    Exit;
  end;
  LDX := (AToward.X - LCenter.X) / (LVertex.Bounds.Width / 2);
  LDY := (AToward.Y - LCenter.Y) / (LVertex.Bounds.Height / 2);

  if (LSide = ndaAutomatic) and (ALoopSide <> ndaAutomatic) then
  begin
    LSide := ALoopSide;
  end;

  if (LSide = ndaAutomatic) and AOrthogonal then
  begin

    if Abs(LDX) >= Abs(LDY) then
    begin

      if LDX < 0 then
      begin
        LSide := ndaLeft;
      end
      else
      begin
        LSide := ndaRight;
      end;
    end
    else if LDY < 0 then
    begin
      LSide := ndaTop;
    end
    else
    begin
      LSide := ndaBottom;
    end;
  end;
  case LSide of
    ndaLeft:
      begin
        ANormal := NyxCanvasPoint(-1, 0);
        Result := NyxCanvasPoint(LVertex.Bounds.Left, LCenter.Y);
      end;
    ndaTop:
      begin
        ANormal := NyxCanvasPoint(0, -1);
        Result := NyxCanvasPoint(LCenter.X, LVertex.Bounds.Top);
      end;
    ndaRight:
      begin
        ANormal := NyxCanvasPoint(1, 0);
        Result := NyxCanvasPoint(LVertex.Bounds.Left + LVertex.Bounds.Width, LCenter.Y);
      end;
    ndaBottom:
      begin
        ANormal := NyxCanvasPoint(0, 1);
        Result := NyxCanvasPoint(LCenter.X, LVertex.Bounds.Top + LVertex.Bounds.Height);
      end;
  else
    begin

      if (LDX = 0) and (LDY = 0) then
      begin
        LDX := 1;
      end;
      case LVertex.Shape of
        ndsEllipse: LScale := Sqrt(LDX * LDX + LDY * LDY);
        ndsDiamond: LScale := Abs(LDX) + Abs(LDY);
      else
        LScale := Max(Abs(LDX), Abs(LDY));
      end;
      Result := NyxCanvasPoint(LCenter.X + LDX / LScale * LVertex.Bounds.Width / 2,
        LCenter.Y + LDY / LScale * LVertex.Bounds.Height / 2);
    end;
  end;
end;

{ Named interior ports must leave the whole shape before an orthogonal bend.
  Legacy midpoint/free endpoints retain their exact existing 24-unit stubs. }
function PortExit(const ADiagram: INyxDiagram; const AEnd: TNyxDiagramEndpoint;
  const APoint, ANormal: TNyxCanvasPoint): TNyxCanvasPoint;
var
  LBounds: TNyxCanvasBounds;
  LDistance: Double;
begin
  LDistance := 24;

  if AEnd.Kind = ndekPort then
  begin
    LBounds := ADiagram.FindVertex(AEnd.Vertex).Bounds;

    if ANormal.X < 0 then
    begin
      LDistance := LDistance + Max(0, APoint.X - LBounds.Left);
    end
    else if ANormal.X > 0 then
    begin
      LDistance := LDistance + Max(0, LBounds.Left + LBounds.Width - APoint.X);
    end
    else if ANormal.Y < 0 then
    begin
      LDistance := LDistance + Max(0, APoint.Y - LBounds.Top);
    end
    else if ANormal.Y > 0 then
    begin
      LDistance := LDistance + Max(0, LBounds.Top + LBounds.Height - APoint.Y);
    end;
  end;
  Result := APoint.Offset(ANormal.X * LDistance, ANormal.Y * LDistance);
end;

function TDiagram.Route(const AReference: TNyxDiagramEdgeRef): TNyxDiagramRoute;
var
  LEdge: TNyxDiagramEdge;
  LStart: TNyxCanvasPoint;
  LFinish: TNyxCanvasPoint;
  LNormalStart: TNyxCanvasPoint;
  LNormalFinish: TNyxCanvasPoint;
  LExitStart: TNyxCanvasPoint;
  LExitFinish: TNyxCanvasPoint;
  LLoop: Boolean;
  LStartSide: TNyxDiagramAttachment;
  LFinishSide: TNyxDiagramAttachment;
  LMiddle: Double;
begin
  Result := Default(TNyxDiagramRoute);
  LEdge := FindEdge(AReference);
  LLoop := LEdge.Start.UsesVertex and LEdge.Finish.UsesVertex and
    (LEdge.Start.Vertex.Name = LEdge.Finish.Vertex.Name);

  if LLoop and (LEdge.Routing <> ndrOrthogonal) then
  begin
    raise ENyxDiagram.Create('Self-loops require orthogonal routing');
  end;
  LStartSide := ndaAutomatic;
  LFinishSide := ndaAutomatic;

  if LLoop then
  begin
    LStartSide := ndaRight;
    LFinishSide := ndaTop;
  end;
  LStart := Attach(Self, LEdge.Start, EndpointCenter(Self, LEdge.Finish),
    LEdge.Routing = ndrOrthogonal, LStartSide, LNormalStart);
  LFinish := Attach(Self, LEdge.Finish, EndpointCenter(Self, LEdge.Start),
    LEdge.Routing = ndrOrthogonal, LFinishSide, LNormalFinish);

  if LLoop and (LStart.X = LFinish.X) and (LStart.Y = LFinish.Y) then
  begin
    raise ENyxDiagram.Create('Self-loop endpoints require distinct boundary attachments');
  end;
  Result.Add(LStart);

  if LEdge.Routing = ndrOrthogonal then
  begin
    LExitStart := PortExit(Self, LEdge.Start, LStart, LNormalStart);
    LExitFinish := PortExit(Self, LEdge.Finish, LFinish, LNormalFinish);
    Result.Add(LExitStart);

    if (LNormalStart.X <> 0) and (LNormalFinish.X <> 0) then
    begin

      if LLoop then
      begin

        if LNormalStart.X = LNormalFinish.X then
        begin

          if LNormalStart.X > 0 then
          begin
            LMiddle := Max(LExitStart.X, LExitFinish.X) + 24;
          end
          else
          begin
            LMiddle := Min(LExitStart.X, LExitFinish.X) - 24;
          end;
          Result.Add(NyxCanvasPoint(LMiddle, LExitStart.Y));
          Result.Add(NyxCanvasPoint(LMiddle, LExitFinish.Y));
        end
        else
        begin
          LMiddle := FindVertex(LEdge.Start.Vertex).Bounds.Top - 24;
          Result.Add(NyxCanvasPoint(LExitStart.X, LMiddle));
          Result.Add(NyxCanvasPoint(LExitFinish.X, LMiddle));
        end;
      end
      else
      begin
        LMiddle := (LExitStart.X + LExitFinish.X) / 2;
        Result.Add(NyxCanvasPoint(LMiddle, LExitStart.Y));
        Result.Add(NyxCanvasPoint(LMiddle, LExitFinish.Y));
      end;
    end
    else if (LNormalStart.Y <> 0) and (LNormalFinish.Y <> 0) then
    begin

      if LLoop then
      begin

        if LNormalStart.Y = LNormalFinish.Y then
        begin

          if LNormalStart.Y > 0 then
          begin
            LMiddle := Max(LExitStart.Y, LExitFinish.Y) + 24;
          end
          else
          begin
            LMiddle := Min(LExitStart.Y, LExitFinish.Y) - 24;
          end;
          Result.Add(NyxCanvasPoint(LExitStart.X, LMiddle));
          Result.Add(NyxCanvasPoint(LExitFinish.X, LMiddle));
        end
        else
        begin
          LMiddle := FindVertex(LEdge.Start.Vertex).Bounds.Left +
            FindVertex(LEdge.Start.Vertex).Bounds.Width + 24;
          Result.Add(NyxCanvasPoint(LMiddle, LExitStart.Y));
          Result.Add(NyxCanvasPoint(LMiddle, LExitFinish.Y));
        end;
      end
      else
      begin
        LMiddle := (LExitStart.Y + LExitFinish.Y) / 2;
        Result.Add(NyxCanvasPoint(LExitStart.X, LMiddle));
        Result.Add(NyxCanvasPoint(LExitFinish.X, LMiddle));
      end;
    end
    else if LNormalStart.X <> 0 then
    begin
      Result.Add(NyxCanvasPoint(LExitStart.X, LExitFinish.Y));
    end
    else
    begin
      Result.Add(NyxCanvasPoint(LExitFinish.X, LExitStart.Y));
    end;
    Result.Add(LExitFinish);
  end;
  Result.Add(LFinish);
end;

function TDiagram.PickVertex(const APoint: TNyxCanvasPoint): TNyxDiagramVertexRef;
var
  LIndex: Integer;
  LVertex: TNyxDiagramVertex;
  LDX: Double;
  LDY: Double;
  LHit: Boolean;
begin
  RequirePoint(APoint);
  Result := Default(TNyxDiagramVertexRef);
  for LIndex := High(FVertices) downto 0 do
  begin
    LVertex := FVertices[LIndex];

    if not LVertex.Bounds.Contains(APoint) then
    begin
      Continue;
    end;
    LDX := (APoint.X - LVertex.Bounds.Center.X) / (LVertex.Bounds.Width / 2);
    LDY := (APoint.Y - LVertex.Bounds.Center.Y) / (LVertex.Bounds.Height / 2);
    case LVertex.Shape of
      ndsEllipse: LHit := LDX * LDX + LDY * LDY <= 1;
      ndsDiamond: LHit := Abs(LDX) + Abs(LDY) <= 1;
    else
      LHit := True;
    end;

    if LHit then
    begin
      Exit(LVertex.Reference);
    end;
  end;
end;

function SegmentDistance(const APoint, AStart, AFinish: TNyxCanvasPoint): Double;
var
  LDX: Double;
  LDY: Double;
  LLength: Double;
  LFraction: Double;
begin
  LDX := AFinish.X - AStart.X;
  LDY := AFinish.Y - AStart.Y;
  LLength := LDX * LDX + LDY * LDY;
  LFraction := 0;

  if LLength > 0 then
  begin
    LFraction := Max(0, Min(1, ((APoint.X - AStart.X) * LDX +
      (APoint.Y - AStart.Y) * LDY) / LLength));
  end;
  LDX := APoint.X - (AStart.X + LFraction * LDX);
  LDY := APoint.Y - (AStart.Y + LFraction * LDY);
  Result := Sqrt(LDX * LDX + LDY * LDY);
end;

function TDiagram.PickEdge(const APoint: TNyxCanvasPoint;
  ATolerance: Double): TNyxDiagramEdgeRef;
var
  LIndex: Integer;
  LPoint: Integer;
  LRoute: TNyxDiagramRoute;
begin
  RequirePoint(APoint);

  if IsNan(ATolerance) or IsInfinite(ATolerance) or (ATolerance < 0) or (ATolerance > 64) then
  begin
    raise ENyxDiagram.Create('Diagram edge hit tolerance requires finite 0..64 world units');
  end;
  Result := Default(TNyxDiagramEdgeRef);
  for LIndex := High(FEdges) downto 0 do
  begin
    LRoute := Route(FEdges[LIndex].Reference);

    if (LRoute.Count = 1) and
      (SegmentDistance(APoint, LRoute.Point(0), LRoute.Point(0)) <= ATolerance) then
    begin
      Exit(FEdges[LIndex].Reference);
    end;
    for LPoint := 1 to LRoute.Count - 1 do
    begin

      if SegmentDistance(APoint, LRoute.Point(LPoint - 1), LRoute.Point(LPoint)) <= ATolerance then
      begin
        Exit(FEdges[LIndex].Reference);
      end;
    end;
  end;
end;

function PointData(const APoint: TNyxCanvasPoint): TNyxDataValue;
begin
  Result := NyxArray([NyxData(APoint.X), NyxData(APoint.Y)]);
end;

function EndpointData(const AEnd: TNyxDiagramEndpoint): TNyxDataValue;
begin

  if AEnd.Kind = ndekPort then
  begin
    Result := NyxObject([NyxField('vertex', NyxData(AEnd.Vertex.Name)),
      NyxField('port', NyxData(AEnd.Port.Name))]);
  end
  else if AEnd.Kind = ndekFree then
  begin
    Result := NyxObject([NyxField('point', PointData(AEnd.Point))]);
  end
  else
  begin
    Result := NyxObject([NyxField('vertex', NyxData(AEnd.Vertex.Name)),
      NyxField('attachment', NyxData(Ord(AEnd.Attachment)))]);
  end;
end;

function TNyxDiagramVertex.ToData: TNyxDataValue;
var
  LValue: TNyxDiagramVertex;
  LFields: array of TNyxDataField;
  LPorts: array of TNyxDataValue;
  LIndex: Integer;
begin
  LValue := NyxDiagramVertex(Reference, Shape, Bounds).Captioned(Caption).Styled(Style)
    .WithContent(Content);
  for LIndex := 0 to PortCount - 1 do
  begin
    LValue := LValue.WithPort(Port(LIndex));
  end;
  SetLength(LFields, 5);
  LFields[0] := NyxField('name', NyxData(LValue.Reference.Name));
  LFields[1] := NyxField('shape', NyxData(Ord(LValue.Shape)));
  LFields[2] := NyxField('bounds', NyxArray([NyxData(LValue.Bounds.Left), NyxData(LValue.Bounds.Top),
    NyxData(LValue.Bounds.Width), NyxData(LValue.Bounds.Height)]));
  LFields[3] := NyxField('caption', NyxData(LValue.Caption));
  LFields[4] := NyxField('style', LValue.Style.ToData);

  if LValue.Content.Kind <> ndckNone then
  begin
    SetLength(LFields, 6);
    LFields[5] := NyxField('content', LValue.Content.ToData);
  end;

  if LValue.PortCount > 0 then
  begin
    SetLength(LPorts, LValue.PortCount);
    for LIndex := 0 to High(LPorts) do
    begin
      LPorts[LIndex] := LValue.Port(LIndex).ToData;
    end;
    LIndex := Length(LFields);
    SetLength(LFields, LIndex + 1);
    LFields[LIndex] := NyxField('ports', NyxArray(LPorts));
  end;
  Result := NyxObject(LFields);
end;

function TNyxDiagramEdge.ToData: TNyxDataValue;
var
  LValue: TNyxDiagramEdge;
begin
  LValue := NyxDiagramEdge(Reference, Start, Finish).Routed(Routing)
    .Arrows(StartArrow, FinishArrow).Styled(Style);
  Result := NyxObject([NyxField('name', NyxData(LValue.Reference.Name)),
    NyxField('start', EndpointData(LValue.Start)), NyxField('finish', EndpointData(LValue.Finish)),
    NyxField('routing', NyxData(Ord(LValue.Routing))),
    NyxField('startArrow', NyxData(Ord(LValue.StartArrow))),
    NyxField('finishArrow', NyxData(Ord(LValue.FinishArrow))),
    NyxField('style', LValue.Style.ToData)]);
end;

function TDiagram.ToData: TNyxDataValue;
var
  LVertices: array of TNyxDataValue;
  LEdges: array of TNyxDataValue;
  LIndex: Integer;
  LVersion: Integer;
begin
  LVersion := 1;
  SetLength(LVertices, Length(FVertices));
  SetLength(LEdges, Length(FEdges));
  for LIndex := 0 to High(FVertices) do
  begin
    LVertices[LIndex] := FVertices[LIndex].ToData;

    if FVertices[LIndex].Content.Kind <> ndckNone then
    begin
      LVersion := Max(LVersion, 2);
    end;

    if FVertices[LIndex].PortCount > 0 then
    begin
      LVersion := 3;
    end;
  end;
  for LIndex := 0 to High(FEdges) do
  begin
    LEdges[LIndex] := FEdges[LIndex].ToData;
  end;
  Result := NyxObject([NyxField('version', NyxData(LVersion)),
    NyxField('vertices', NyxArray(LVertices)), NyxField('edges', NyxArray(LEdges))]);

  if NyxUTF8ByteCount(Result.ToJSON) > MaximumNyxDiagramBytes then
  begin
    raise ENyxDiagram.Create('Diagram descriptor exceeds its persistence byte boundary');
  end;
  {$ifdef PAS2JS}
  FWireAdmitted := 1;
  {$else}
  InterlockedExchange(FWireAdmitted, 1);
  {$endif}
end;

procedure Fields(const AData: TNyxDataValue; const ANames: array of TNyxText);
var
  LIndex: Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> Length(ANames)) then
  begin
    raise ENyxDiagram.Create('Diagram values require their exact closed fields');
  end;
  for LIndex := 0 to High(ANames) do
  begin
    AData.Field(ANames[LIndex]);
  end;
end;

{ Optional closed-record members are queried without catching unrelated type or
  numeric errors. Nonobjects remain false and are rejected by Fields afterward. }
function DiagramFieldPresent(const AData: TNyxDataValue; const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if AData.Kind <> ndObject then
  begin
    Exit;
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
end;

function ReadPoint(const AData: TNyxDataValue): TNyxCanvasPoint;
begin

  if (AData.Kind <> ndArray) or (AData.Count <> 2) then
  begin
    raise ENyxDiagram.Create('Diagram point requires two numeric coordinates');
  end;
  Result := NyxCanvasPoint(AData.Item(0).AsNumber, AData.Item(1).AsNumber);
end;

{$I nyx.diagram.ports.inc}

function ReadEndpoint(const AData: TNyxDataValue): TNyxDiagramEndpoint;
var
  LChoice: Integer;
begin

  if (AData.Kind = ndObject) and (AData.Count = 1) then
  begin
    Fields(AData, ['point']);
    Result := NyxDiagramFreeEnd(ReadPoint(AData.Field('point')));
  end
  else if DiagramFieldPresent(AData, 'port') then
  begin
    Fields(AData, ['vertex', 'port']);
    Result := NyxDiagramPortEnd(NyxDiagramVertexRef(AData.Field('vertex').AsText),
      NyxDiagramPortRef(AData.Field('port').AsText));
  end
  else
  begin
    Fields(AData, ['vertex', 'attachment']);
    LChoice := AData.Field('attachment').AsInteger;
    RequireOrdinal(LChoice, Ord(High(TNyxDiagramAttachment)));
    Result := NyxDiagramAttached(NyxDiagramVertexRef(AData.Field('vertex').AsText),
      TNyxDiagramAttachment(LChoice));
  end;
end;

class function TNyxDiagramVertex.FromData(const AData: TNyxDataValue): TNyxDiagramVertex;
var
  LBounds: TNyxDataValue;
  LChoice: Integer;
  LPort: TNyxDiagramPort;
  LPorts: TNyxDataValue;
  LIndex: Integer;
  LPrevious: Integer;
begin

  if DiagramFieldPresent(AData, 'ports') then
  begin

    if DiagramFieldPresent(AData, 'content') then
    begin
      Fields(AData, ['name', 'shape', 'bounds', 'caption', 'style', 'content', 'ports']);
    end
    else
    begin
      Fields(AData, ['name', 'shape', 'bounds', 'caption', 'style', 'ports']);
    end;
  end
  else if (AData.Kind = ndObject) and (AData.Count = 6) then
  begin
    Fields(AData, ['name', 'shape', 'bounds', 'caption', 'style', 'content']);
  end
  else
  begin
    Fields(AData, ['name', 'shape', 'bounds', 'caption', 'style']);
  end;
  LChoice := AData.Field('shape').AsInteger;
  RequireOrdinal(LChoice, Ord(High(TNyxDiagramShape)));
  LBounds := AData.Field('bounds');

  if (LBounds.Kind <> ndArray) or (LBounds.Count <> 4) then
  begin
    raise ENyxDiagram.Create('Diagram shape requires four numeric bounds');
  end;
  Result := NyxDiagramVertex(NyxDiagramVertexRef(AData.Field('name').AsText),
    TNyxDiagramShape(LChoice), NyxCanvasBounds(LBounds.Item(0).AsNumber,
      LBounds.Item(1).AsNumber, LBounds.Item(2).AsNumber, LBounds.Item(3).AsNumber))
    .Captioned(AData.Field('caption').AsText)
    .Styled(TNyxDrawingStyle.FromData(AData.Field('style')));

  if DiagramFieldPresent(AData, 'content') then
  begin
    Result := Result.WithContent(TNyxDiagramContent.FromData(AData.Field('content')));
  end;

  if DiagramFieldPresent(AData, 'ports') then
  begin
    LPorts := AData.Field('ports');

    if (LPorts.Kind <> ndArray) or (LPorts.Count = 0) or
      (LPorts.Count > MaximumNyxDiagramPortsPerVertex) then
    begin
      raise ENyxDiagram.Create('Diagram port arrays require bounded nonempty membership');
    end;
    for LIndex := 0 to LPorts.Count - 1 do
    begin
      LPort := TNyxDiagramPort.FromData(LPorts.Item(LIndex));
      for LPrevious := 0 to Result.PortCount - 1 do
      begin

        if Result.Port(LPrevious).Reference.Name = LPort.Reference.Name then
        begin
          raise ENyxDiagram.Create('Duplicate named diagram port');
        end;
      end;
      Result := Result.WithPort(LPort);
    end;
  end;
end;

class function TNyxDiagramEdge.FromData(const AData: TNyxDataValue): TNyxDiagramEdge;
var
  LChoice: Integer;
  LStartArrow: Integer;
  LFinishArrow: Integer;
begin
  Fields(AData, ['name', 'start', 'finish', 'routing', 'startArrow', 'finishArrow', 'style']);
  LChoice := AData.Field('routing').AsInteger;
  RequireOrdinal(LChoice, Ord(High(TNyxDiagramRouting)));
  LStartArrow := AData.Field('startArrow').AsInteger;
  LFinishArrow := AData.Field('finishArrow').AsInteger;
  RequireOrdinal(LStartArrow, Ord(High(TNyxDiagramArrow)));
  RequireOrdinal(LFinishArrow, Ord(High(TNyxDiagramArrow)));
  Result := NyxDiagramEdge(NyxDiagramEdgeRef(AData.Field('name').AsText),
    ReadEndpoint(AData.Field('start')), ReadEndpoint(AData.Field('finish')))
    .Routed(TNyxDiagramRouting(LChoice))
    .Arrows(TNyxDiagramArrow(LStartArrow), TNyxDiagramArrow(LFinishArrow))
    .Styled(TNyxDrawingStyle.FromData(AData.Field('style')));
end;

function NyxDiagramFromData(const AData: TNyxDataValue): INyxDiagram;
var
  LPlan: TDiagram;
  LVertices: TNyxDataValue;
  LEdges: TNyxDataValue;
  LIndex: Integer;
  LVertex: TNyxDiagramVertex;
  LEdge: TNyxDiagramEdge;
  LVersion: Integer;
begin
  Fields(AData, ['version', 'vertices', 'edges']);
  LVersion := AData.Field('version').AsInteger;

  if not (LVersion in [1, 2, 3]) or
    (NyxUTF8ByteCount(AData.ToJSON) > MaximumNyxDiagramBytes) then
  begin
    raise ENyxDiagram.Create('Unsupported or oversized diagram descriptor');
  end;
  LVertices := AData.Field('vertices');
  LEdges := AData.Field('edges');

  if (LVertices.Kind <> ndArray) or (LEdges.Kind <> ndArray) or
    (LVertices.Count > MaximumNyxDiagramVertices) or (LEdges.Count > MaximumNyxDiagramEdges) then
  begin
    raise ENyxDiagram.Create('Diagram membership exceeds its bounded arrays');
  end;
  { Decode once into private storage, then resolve every route. Repeated fluent
    upserts would copy all preceding members and reparse their styles. Nothing
    can observe this candidate before the complete function returns. }
  LPlan := TDiagram.Create;
  Result := LPlan;
  SetLength(LPlan.FVertices, LVertices.Count);
  SetLength(LPlan.FEdges, LEdges.Count);
  for LIndex := 0 to LVertices.Count - 1 do
  begin

    if (LVersion = 1) and (LVertices.Item(LIndex).Count <> 5) then
    begin
      raise ENyxDiagram.Create('Rich diagram content requires descriptor version two');
    end;
    LVertex := TNyxDiagramVertex.FromData(LVertices.Item(LIndex));

    if (LVersion < 3) and (LVertex.PortCount > 0) then
    begin
      raise ENyxDiagram.Create('Named ports require diagram descriptor version three');
    end;

    if LPlan.VertexIndex(LVertex.Reference) >= 0 then
    begin
      raise ENyxDiagram.Create('Duplicate diagram vertex');
    end;
    LPlan.FVertices[LIndex] := LVertex;
  end;
  for LIndex := 0 to LEdges.Count - 1 do
  begin
    LEdge := TNyxDiagramEdge.FromData(LEdges.Item(LIndex));

    if (LVersion < 3) and ((LEdge.Start.Kind = ndekPort) or (LEdge.Finish.Kind = ndekPort)) then
    begin
      raise ENyxDiagram.Create('Named endpoints require diagram descriptor version three');
    end;

    if LPlan.EdgeIndex(LEdge.Reference) >= 0 then
    begin
      raise ENyxDiagram.Create('Duplicate diagram edge');
    end;
    LPlan.FEdges[LIndex] := LEdge;
  end;
  LPlan.ValidateRoutes;
end;

end.
