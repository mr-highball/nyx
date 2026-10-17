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
unit nyx.diagram.selection;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, Math, nyx.text, nyx.diagram, nyx.canvas.geometry;

type
  { Immutable ordered set of exact shape references. Last is the primary shape;
    order follows explicit inclusion, not sorting or Unicode normalization.
    No graph, view, control or callback is retained. Empty is a valid selection;
    absent/duplicate references and more than the graph's vertex limit refuse.
    Including an existing name and excluding a missing name are exact no-ops. }
  INyxDiagramVertexSelection = interface(IInterface)
    ['{44B2CD44-78AB-4BD3-BECA-FDDBEAA338A4}']
    function GetCount: Integer;
    function Item(AIndex: Integer): TNyxDiagramVertexRef;
    function Contains(const AReference: TNyxDiagramVertexRef): Boolean;
    function Including(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
    function Excluding(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
    function Toggling(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
    property Count: Integer read GetCount;
  end;

  { Align shape bounds to the selected union's corresponding edge/center.
    Extents, content, ports and paint order remain authored values. }
  TNyxDiagramAlignment = (ndalLeft, ndalHorizontalCenter, ndalRight,
    ndalTop, ndalVerticalCenter, ndalBottom);

  { Distribute along one axis, preserving the two spatially outer members.
    Centers use center order; gaps use left/top order and may be negative when
    shapes overlap. Ties retain original diagram paint order. }
  TNyxDiagramDistribution = (nddiHorizontalCenters, nddiVerticalCenters,
    nddiHorizontalGaps, nddiVerticalGaps);

  { Region membership uses bounds, independent of mathematical shape outline.
    This is data selection, not a claim of an installed pointer-marquee gesture. }
  TNyxDiagramMarqueeMode = (ndmmContained, ndmmIntersecting);

{ Construct an independently owned ordered set. Empty is deliberate; duplicate
  and undefined members refuse rather than silently changing the caller's order. }
function NewNyxDiagramVertexSelection(
  const AReferences: array of TNyxDiagramVertexRef): INyxDiagramVertexSelection;
{ Copy an alternate immutable creator's bounded values once. Private admitted
  built-ins can share their immutable storage; caller methods may run during
  alternate capture, so UI owners must retain their usual revision/draft guards. }
function CaptureNyxDiagramVertexSelection(
  const ASelection: INyxDiagramVertexSelection): INyxDiagramVertexSelection;
{ Validate every selected reference against the captured graph. No missing member
  is silently dropped from an authored movement or arrangement operation. }
procedure ValidateNyxDiagramVertexSelection(const ADiagram: INyxDiagram;
  const ASelection: INyxDiagramVertexSelection);
{ Presentation reconciliation after accepted removal/replacement: return only
  surviving selected names, preserving their order, without selecting a fallback. }
function RetainNyxDiagramVertexSelection(const ADiagram: INyxDiagram;
  const ASelection: INyxDiagramVertexSelection): INyxDiagramVertexSelection;
{ Query a defined world-space bounds region in graph paint order. Intersecting
  includes touching boundaries; neither policy captures pointer input. }
function SelectNyxDiagramVertices(const ADiagram: INyxDiagram;
  const ARegion: TNyxCanvasBounds;
  AMode: TNyxDiagramMarqueeMode = ndmmContained): INyxDiagramVertexSelection;
{ Batch data edits fork once and admit all routes after all geometry is ready.
  Sources, nonmembers, edges/styles/ports/content and stable paint order remain
  unchanged. Failures leave the source usable. Zero movement, an already aligned
  set, or too few distribution members returns the captured original graph. }
function MoveNyxDiagramVertices(const ADiagram: INyxDiagram;
  const ASelection: INyxDiagramVertexSelection; AX, AY: Double): INyxDiagram;
{ Use the complete selected bounds union as the alignment reference. No shape
  resizes, no nonmember moves, and an already aligned group retains identity. }
function AlignNyxDiagramVertices(const ADiagram: INyxDiagram;
  const ASelection: INyxDiagramVertexSelection; AAlignment: TNyxDiagramAlignment): INyxDiagram;
{ Preserve spatially outer members and stable paint-order ties. Equal centers
  and equal gaps are separate choices; negative gaps retain intentional overlap. }
function DistributeNyxDiagramVertices(const ADiagram: INyxDiagram;
  const ASelection: INyxDiagramVertexSelection;
  ADistribution: TNyxDiagramDistribution): INyxDiagram;

implementation

type
  { Private value vector; individual reference records contain owned text only.
    Published vectors never change, so independent interface holders stay safe. }
  TVertexReferences = array of TNyxDiagramVertexRef;

  { Data-only immutable selection. Class/interface identity is checked before
    the capture shortcut; aggregation cannot lend another object's authority. }
  TVertexSelection = class(TInterfacedObject, INyxDiagramVertexSelection)
  private
    FValues: TVertexReferences;
    function IndexOf(const AReference: TNyxDiagramVertexRef): Integer;
  public
    constructor Create(const AReferences: array of TNyxDiagramVertexRef);
    function GetCount: Integer;
    function Item(AIndex: Integer): TNyxDiagramVertexRef;
    function Contains(const AReference: TNyxDiagramVertexRef): Boolean;
    function Including(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
    function Excluding(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
    function Toggling(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
  end;

  { Detached geometry proposals in graph paint order. Only a complete admitted
    batch can become a returned graph; no caller sees partially moved members. }
  TVertexValues = array of TNyxDiagramVertex;

function BoundsRight(const AValue: TNyxCanvasBounds): Double;
begin
  Result := AValue.Left + AValue.Width;
end;

function BoundsBottom(const AValue: TNyxCanvasBounds): Double;
begin
  Result := AValue.Top + AValue.Height;
end;

constructor TVertexSelection.Create(const AReferences: array of TNyxDiagramVertexRef);
var
  LIndex: Integer;
begin
  inherited Create;

  if Length(AReferences) > MaximumNyxDiagramVertices then
  begin
    raise ENyxDiagram.Create('Diagram selection exceeds vertex capacity');
  end;
  SetLength(FValues, Length(AReferences));
  for LIndex := 0 to High(AReferences) do
  begin

    if not AReferences[LIndex].Defined then
    begin
      raise ENyxDiagram.Create('Diagram selection requires defined shape references');
    end;

    if IndexOf(AReferences[LIndex]) >= 0 then
    begin
      raise ENyxDiagram.Create('Diagram selection contains a duplicate shape');
    end;
    FValues[LIndex] := NyxDiagramVertexRef(AReferences[LIndex].Name);
  end;
end;

function TVertexSelection.IndexOf(const AReference: TNyxDiagramVertexRef): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FValues) do
  begin

    if FValues[LIndex].Name = AReference.Name then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TVertexSelection.GetCount: Integer;
begin
  Result := Length(FValues);
end;

function TVertexSelection.Item(AIndex: Integer): TNyxDiagramVertexRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FValues)) then
  begin
    raise ENyxDiagram.Create('Diagram selection index is outside its bounds');
  end;
  Result := FValues[AIndex];
end;

function TVertexSelection.Contains(const AReference: TNyxDiagramVertexRef): Boolean;
begin
  Result := AReference.Defined and (IndexOf(AReference) >= 0);
end;

function TVertexSelection.Including(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
var
  LValues: TVertexReferences;
begin

  if Contains(AReference) then
  begin
    Exit(Self as INyxDiagramVertexSelection);
  end;
  LValues := Copy(FValues, 0, Length(FValues));
  SetLength(LValues, Length(LValues) + 1);
  LValues[High(LValues)] := AReference;
  Result := NewNyxDiagramVertexSelection(LValues);
end;

function TVertexSelection.Excluding(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
var
  LValues: TVertexReferences;
  LIndex: Integer;
  LPosition: Integer;
begin

  if not Contains(AReference) then
  begin
    Exit(Self as INyxDiagramVertexSelection);
  end;
  SetLength(LValues, Length(FValues) - 1);
  LPosition := 0;
  for LIndex := 0 to High(FValues) do
  begin

    if FValues[LIndex].Name <> AReference.Name then
    begin
      LValues[LPosition] := FValues[LIndex];
      Inc(LPosition);
    end;
  end;
  Result := NewNyxDiagramVertexSelection(LValues);
end;

function TVertexSelection.Toggling(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
begin

  if Contains(AReference) then
  begin
    Result := Excluding(AReference);
  end
  else
  begin
    Result := Including(AReference);
  end;
end;

function NewNyxDiagramVertexSelection(
  const AReferences: array of TNyxDiagramVertexRef): INyxDiagramVertexSelection;
begin
  Result := TVertexSelection.Create(AReferences);
end;

function CaptureNyxDiagramVertexSelection(
  const ASelection: INyxDiagramVertexSelection): INyxDiagramVertexSelection;
var
  LStorage: TVertexSelection;
  LIdentity: INyxDiagramVertexSelection;
  LCount: Integer;
  LIndex: Integer;
  LValues: TVertexReferences;
begin

  if ASelection = nil then
  begin
    raise ENyxDiagram.Create('Diagram vertex selection is absent');
  end;

  if ASelection is TVertexSelection then
  begin
    LStorage := ASelection as TVertexSelection;
    LIdentity := LStorage;

    if (LStorage.ClassType = TVertexSelection) and (LIdentity = ASelection) then
    begin
      Exit(ASelection);
    end;
  end;
  LCount := ASelection.Count;

  if (LCount < 0) or (LCount > MaximumNyxDiagramVertices) then
  begin
    raise ENyxDiagram.Create('Diagram selection exceeds vertex capacity');
  end;
  SetLength(LValues, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LValues[LIndex] := ASelection.Item(LIndex);
  end;
  Result := NewNyxDiagramVertexSelection(LValues);
end;

procedure ValidateNyxDiagramVertexSelection(const ADiagram: INyxDiagram;
  const ASelection: INyxDiagramVertexSelection);
var
  LGraph: INyxDiagram;
  LSelection: INyxDiagramVertexSelection;
  LIndex: Integer;
begin
  LGraph := CaptureNyxDiagram(ADiagram);
  LSelection := CaptureNyxDiagramVertexSelection(ASelection);
  for LIndex := 0 to LSelection.Count - 1 do
  begin
    LGraph.FindVertex(LSelection.Item(LIndex));
  end;
end;

function RetainNyxDiagramVertexSelection(const ADiagram: INyxDiagram;
  const ASelection: INyxDiagramVertexSelection): INyxDiagramVertexSelection;
var
  LGraph: INyxDiagram;
  LSelection: INyxDiagramVertexSelection;
  LIndex: Integer;
  LVertex: Integer;
begin
  LGraph := CaptureNyxDiagram(ADiagram);
  LSelection := CaptureNyxDiagramVertexSelection(ASelection);
  Result := NewNyxDiagramVertexSelection([]);
  for LIndex := 0 to LSelection.Count - 1 do
  begin
    for LVertex := 0 to LGraph.VertexCount - 1 do
    begin

      if LGraph.Vertex(LVertex).Reference.Name = LSelection.Item(LIndex).Name then
      begin
        Result := Result.Including(LSelection.Item(LIndex));
        Break;
      end;
    end;
  end;
end;

function SelectNyxDiagramVertices(const ADiagram: INyxDiagram;
  const ARegion: TNyxCanvasBounds; AMode: TNyxDiagramMarqueeMode): INyxDiagramVertexSelection;
var
  LGraph: INyxDiagram;
  LBounds: TNyxCanvasBounds;
  LIndex: Integer;
  LMatch: Boolean;
begin

  if not ARegion.Defined or (Ord(AMode) < Ord(Low(TNyxDiagramMarqueeMode))) or
    (Ord(AMode) > Ord(High(TNyxDiagramMarqueeMode))) then
  begin
    raise ENyxDiagram.Create('Diagram selection requires a region and known containment policy');
  end;
  LGraph := CaptureNyxDiagram(ADiagram);
  Result := NewNyxDiagramVertexSelection([]);
  for LIndex := 0 to LGraph.VertexCount - 1 do
  begin
    LBounds := LGraph.Vertex(LIndex).Bounds;

    if AMode = ndmmContained then
    begin
      LMatch := (LBounds.Left >= ARegion.Left) and (LBounds.Top >= ARegion.Top) and
        (BoundsRight(LBounds) <= BoundsRight(ARegion)) and
        (BoundsBottom(LBounds) <= BoundsBottom(ARegion));
    end
    else
    begin
      LMatch := (BoundsRight(LBounds) >= ARegion.Left) and (BoundsBottom(LBounds) >= ARegion.Top) and
        (LBounds.Left <= BoundsRight(ARegion)) and (LBounds.Top <= BoundsBottom(ARegion));
    end;

    if LMatch then
    begin
      Result := Result.Including(LGraph.Vertex(LIndex).Reference);
    end;
  end;
end;

function SelectedValues(const ADiagram: INyxDiagram;
  const ASelection: INyxDiagramVertexSelection): TVertexValues;
var
  LSelection: INyxDiagramVertexSelection;
  LIndex: Integer;
  LCount: Integer;
begin
  LSelection := CaptureNyxDiagramVertexSelection(ASelection);
  ValidateNyxDiagramVertexSelection(ADiagram, LSelection);
  Result := nil;
  SetLength(Result, LSelection.Count);
  LCount := 0;
  for LIndex := 0 to ADiagram.VertexCount - 1 do
  begin

    if LSelection.Contains(ADiagram.Vertex(LIndex).Reference) then
    begin
      Result[LCount] := ADiagram.Vertex(LIndex);
      Inc(LCount);
    end;
  end;
end;

function MoveNyxDiagramVertices(const ADiagram: INyxDiagram;
  const ASelection: INyxDiagramVertexSelection; AX, AY: Double): INyxDiagram;
var
  LGraph: INyxDiagram;
  LValues: TVertexValues;
  LIndex: Integer;
  LBounds: TNyxCanvasBounds;
begin

  if IsNan(AX) or IsInfinite(AX) or IsNan(AY) or IsInfinite(AY) then
  begin
    raise ENyxDiagram.Create('Diagram movement requires finite deltas');
  end;
  LGraph := CaptureNyxDiagram(ADiagram);
  LValues := SelectedValues(LGraph, ASelection);

  if (Length(LValues) = 0) or ((AX = 0) and (AY = 0)) then
  begin
    Exit(LGraph);
  end;
  for LIndex := 0 to High(LValues) do
  begin
    LBounds := LValues[LIndex].Bounds;
    LValues[LIndex] := LValues[LIndex].AtBounds(NyxCanvasBounds(
      LBounds.Left + AX, LBounds.Top + AY, LBounds.Width, LBounds.Height));
  end;
  Result := WithNyxDiagramVertices(LGraph, LValues);
end;

function AlignNyxDiagramVertices(const ADiagram: INyxDiagram;
  const ASelection: INyxDiagramVertexSelection; AAlignment: TNyxDiagramAlignment): INyxDiagram;
var
  LGraph: INyxDiagram;
  LValues: TVertexValues;
  LBounds: TNyxCanvasBounds;
  LIndex: Integer;
  LLeft: Double;
  LRight: Double;
  LTop: Double;
  LBottom: Double;
  LX: Double;
  LY: Double;
  LChanged: Boolean;
begin

  if (Ord(AAlignment) < Ord(Low(TNyxDiagramAlignment))) or
    (Ord(AAlignment) > Ord(High(TNyxDiagramAlignment))) then
  begin
    raise ENyxDiagram.Create('Unknown diagram alignment');
  end;
  LGraph := CaptureNyxDiagram(ADiagram);
  LValues := SelectedValues(LGraph, ASelection);

  if Length(LValues) < 2 then
  begin
    Exit(LGraph);
  end;
  LLeft := LValues[0].Bounds.Left;
  LRight := BoundsRight(LValues[0].Bounds);
  LTop := LValues[0].Bounds.Top;
  LBottom := BoundsBottom(LValues[0].Bounds);
  for LIndex := 1 to High(LValues) do
  begin
    LLeft := Min(LLeft, LValues[LIndex].Bounds.Left);
    LRight := Max(LRight, BoundsRight(LValues[LIndex].Bounds));
    LTop := Min(LTop, LValues[LIndex].Bounds.Top);
    LBottom := Max(LBottom, BoundsBottom(LValues[LIndex].Bounds));
  end;
  LChanged := False;
  for LIndex := 0 to High(LValues) do
  begin
    LBounds := LValues[LIndex].Bounds;
    LX := LBounds.Left;
    LY := LBounds.Top;
    case AAlignment of
      ndalLeft: LX := LLeft;
      ndalHorizontalCenter: LX := (LLeft + LRight - LBounds.Width) / 2;
      ndalRight: LX := LRight - LBounds.Width;
      ndalTop: LY := LTop;
      ndalVerticalCenter: LY := (LTop + LBottom - LBounds.Height) / 2;
      ndalBottom: LY := LBottom - LBounds.Height;
    end;
    LChanged := LChanged or (LX <> LBounds.Left) or (LY <> LBounds.Top);
    LValues[LIndex] := LValues[LIndex].AtBounds(NyxCanvasBounds(LX, LY, LBounds.Width, LBounds.Height));
  end;

  if not LChanged then
  begin
    Exit(LGraph);
  end;
  Result := WithNyxDiagramVertices(LGraph, LValues);
end;

function DistributeNyxDiagramVertices(const ADiagram: INyxDiagram;
  const ASelection: INyxDiagramVertexSelection;
  ADistribution: TNyxDiagramDistribution): INyxDiagram;
var
  LGraph: INyxDiagram;
  LValues: TVertexValues;
  LValue: TNyxDiagramVertex;
  LIndex: Integer;
  LPosition: Integer;
  LHorizontal: Boolean;
  LGaps: Boolean;
  LChanged: Boolean;
  LFirst: Double;
  LLast: Double;
  LSize: Double;
  LStep: Double;
  LCursor: Double;
  LX: Double;
  LY: Double;

  function Position(const AValue: TNyxDiagramVertex): Double;
  begin

    if LHorizontal then
    begin
      Result := AValue.Bounds.Left;

      if not LGaps then
      begin
        Result := Result + AValue.Bounds.Width / 2;
      end;
    end
    else
    begin
      Result := AValue.Bounds.Top;

      if not LGaps then
      begin
        Result := Result + AValue.Bounds.Height / 2;
      end;
    end;
  end;

  function Size(const AValue: TNyxDiagramVertex): Double;
  begin

    if LHorizontal then
    begin
      Result := AValue.Bounds.Width;
    end
    else
    begin
      Result := AValue.Bounds.Height;
    end;
  end;

begin

  if (Ord(ADistribution) < Ord(Low(TNyxDiagramDistribution))) or
    (Ord(ADistribution) > Ord(High(TNyxDiagramDistribution))) then
  begin
    raise ENyxDiagram.Create('Unknown diagram distribution');
  end;
  LGraph := CaptureNyxDiagram(ADiagram);
  LValues := SelectedValues(LGraph, ASelection);

  if Length(LValues) < 3 then
  begin
    Exit(LGraph);
  end;
  LHorizontal := ADistribution in [nddiHorizontalCenters, nddiHorizontalGaps];
  LGaps := ADistribution in [nddiHorizontalGaps, nddiVerticalGaps];
  { Stable insertion sort preserves the diagram's paint order for equal keys. }
  for LIndex := 1 to High(LValues) do
  begin
    LValue := LValues[LIndex];
    LPosition := LIndex;
    while (LPosition > 0) and (Position(LValues[LPosition - 1]) > Position(LValue)) do
    begin
      LValues[LPosition] := LValues[LPosition - 1];
      Dec(LPosition);
    end;
    LValues[LPosition] := LValue;
  end;
  LFirst := Position(LValues[0]);
  LLast := Position(LValues[High(LValues)]);
  LSize := 0;

  if LGaps then
  begin
    LLast := LLast + Size(LValues[High(LValues)]);
    for LIndex := 0 to High(LValues) do
    begin
      LSize := LSize + Size(LValues[LIndex]);
    end;
  end;
  LStep := (LLast - LFirst - LSize) / (Length(LValues) - 1);
  LCursor := LFirst;
  LChanged := False;
  for LIndex := 1 to High(LValues) - 1 do
  begin

    if LGaps then
    begin
      LCursor := LCursor + Size(LValues[LIndex - 1]) + LStep;
    end
    else
    begin
      LCursor := LFirst + LIndex * LStep;
    end;
    LX := LValues[LIndex].Bounds.Left;
    LY := LValues[LIndex].Bounds.Top;

    if LHorizontal then
    begin
      LX := LCursor;

      if not LGaps then
      begin
        LX := LX - Size(LValues[LIndex]) / 2;
      end;
    end
    else
    begin
      LY := LCursor;

      if not LGaps then
      begin
        LY := LY - Size(LValues[LIndex]) / 2;
      end;
    end;
    LChanged := LChanged or (LX <> LValues[LIndex].Bounds.Left) or (LY <> LValues[LIndex].Bounds.Top);
    LValues[LIndex] := LValues[LIndex].AtBounds(NyxCanvasBounds(LX, LY,
      LValues[LIndex].Bounds.Width, LValues[LIndex].Bounds.Height));
  end;

  if not LChanged then
  begin
    Exit(LGraph);
  end;
  Result := WithNyxDiagramVertices(LGraph, LValues);
end;

end.
