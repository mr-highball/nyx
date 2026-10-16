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
{ Shared immutable physical-port behavior; no target or executable graph types
  enter this contract. Included after the ordinary closed data readers. }

function TNyxDiagramPortRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function NyxDiagramPortRef(const AName: TNyxText): TNyxDiagramPortRef;
begin
  Result.FName := NyxCanvasItem(AName).Name;
end;

function NyxDiagramPort(const AReference: TNyxDiagramPortRef;
  AX, AY: Double): TNyxDiagramPort;
begin
  Result := Default(TNyxDiagramPort);
  Result.FReference := NyxDiagramPortRef(AReference.Name);
  Result.FPosition := NyxCanvasPoint(AX, AY);
  RequirePoint(Result.FPosition);
end;

function TNyxDiagramPort.Directed(AValue: TNyxDiagramAttachment): TNyxDiagramPort;
begin
  RequireOrdinal(Ord(AValue), Ord(High(TNyxDiagramAttachment)));
  Result := Self;
  Result.FDirection := AValue;
end;

function TNyxDiagramPort.Captioned(const AText: TNyxText): TNyxDiagramPort;
begin
  RequireCaption(AText);
  Result := Self;
  Result.FCaption := AText;
end;

function TNyxDiagramPort.ToData: TNyxDataValue;
var
  LPort: TNyxDiagramPort;
begin
  LPort := NyxDiagramPort(Reference, Position.X, Position.Y).Directed(Direction).Captioned(Caption);
  Result := NyxObject([NyxField('name', NyxData(LPort.Reference.Name)),
    NyxField('position', PointData(LPort.Position)),
    NyxField('direction', NyxData(Ord(LPort.Direction))),
    NyxField('caption', NyxData(LPort.Caption))]);
end;

class function TNyxDiagramPort.FromData(const AData: TNyxDataValue): TNyxDiagramPort;
var
  LPoint: TNyxCanvasPoint;
  LChoice: Integer;
begin
  Fields(AData, ['name', 'position', 'direction', 'caption']);
  LPoint := ReadPoint(AData.Field('position'));
  LChoice := AData.Field('direction').AsInteger;
  RequireOrdinal(LChoice, Ord(High(TNyxDiagramAttachment)));
  Result := NyxDiagramPort(NyxDiagramPortRef(AData.Field('name').AsText), LPoint.X, LPoint.Y)
    .Directed(TNyxDiagramAttachment(LChoice)).Captioned(AData.Field('caption').AsText);
end;

function TNyxDiagramVertex.GetPortCount: Integer;
begin
  Result := Length(FPorts);
end;

function TNyxDiagramVertex.Port(AIndex: Integer): TNyxDiagramPort;
begin

  if (AIndex < 0) or (AIndex >= Length(FPorts)) then
  begin
    raise ENyxDiagram.Create('Diagram port is outside its vertex range');
  end;
  Result := FPorts[AIndex];
end;

function TNyxDiagramVertex.FindPort(const AReference: TNyxDiagramPortRef): TNyxDiagramPort;
var
  LIndex: Integer;
begin
  NyxDiagramPortRef(AReference.Name);
  for LIndex := 0 to High(FPorts) do
  begin

    if FPorts[LIndex].Reference.Name = AReference.Name then
    begin
      Exit(FPorts[LIndex]);
    end;
  end;
  raise ENyxDiagram.Create('Named diagram port does not exist on its vertex');
end;

function NyxDiagramPortPoint(const AVertex: TNyxDiagramVertex;
  const AReference: TNyxDiagramPortRef): TNyxCanvasPoint;
var
  LPort: TNyxDiagramPort;
begin
  LPort := AVertex.FindPort(AReference);
  Result := NyxCanvasPoint(AVertex.Bounds.Left + AVertex.Bounds.Width * LPort.Position.X,
    AVertex.Bounds.Top + AVertex.Bounds.Height * LPort.Position.Y);
  RequirePoint(Result);
end;

function TNyxDiagramVertex.WithPort(const APort: TNyxDiagramPort): TNyxDiagramVertex;
var
  LPort: TNyxDiagramPort;
  LIndex: Integer;
  LFound: Integer;
begin
  LPort := NyxDiagramPort(APort.Reference, APort.Position.X, APort.Position.Y)
    .Directed(APort.Direction).Captioned(APort.Caption);
  LFound := -1;
  for LIndex := 0 to High(FPorts) do
  begin

    if FPorts[LIndex].Reference.Name = LPort.Reference.Name then
    begin
      LFound := LIndex;
      Break;
    end;
  end;

  if (LFound < 0) and (Length(FPorts) >= MaximumNyxDiagramPortsPerVertex) then
  begin
    raise ENyxDiagram.Create('Diagram vertex port capacity exceeded');
  end;
  Result := Self;
  Result.FPorts := Copy(FPorts, 0, Length(FPorts));

  if LFound < 0 then
  begin
    LFound := Length(Result.FPorts);
    SetLength(Result.FPorts, LFound + 1);
  end;
  Result.FPorts[LFound] := LPort;
  NyxDiagramPortPoint(Result, LPort.Reference);
end;

function TNyxDiagramVertex.WithoutPort(const AReference: TNyxDiagramPortRef): TNyxDiagramVertex;
var
  LIndex: Integer;
  LCount: Integer;
begin
  FindPort(AReference);
  Result := Self;
  Result.FPorts := nil;
  SetLength(Result.FPorts, Length(FPorts) - 1);
  LCount := 0;
  for LIndex := 0 to High(FPorts) do
  begin

    if FPorts[LIndex].Reference.Name <> AReference.Name then
    begin
      Result.FPorts[LCount] := FPorts[LIndex];
      Inc(LCount);
    end;
  end;
end;

function TNyxDiagramEndpoint.UsesVertex: Boolean;
begin
  Result := FKind in [ndekAttached, ndekPort];
end;

function NyxDiagramPortEnd(const AVertex: TNyxDiagramVertexRef;
  const APort: TNyxDiagramPortRef): TNyxDiagramEndpoint;
begin
  Result := Default(TNyxDiagramEndpoint);
  Result.FKind := ndekPort;
  Result.FVertex := NyxDiagramVertexRef(AVertex.Name);
  Result.FPort := NyxDiagramPortRef(APort.Name);
end;

function TNyxDiagramPortHit.Defined: Boolean;
begin
  Result := Vertex.Defined and Port.Defined;
end;

function PickNyxDiagramPort(const ADiagram: INyxDiagram;
  const APoint: TNyxCanvasPoint; ATolerance: Double): TNyxDiagramPortHit;
var
  LVertexIndex: Integer;
  LPortIndex: Integer;
  LVertex: TNyxDiagramVertex;
  LPoint: TNyxCanvasPoint;
  LDistance: Double;
begin
  RequirePoint(APoint);

  if (ADiagram = nil) or IsNan(ATolerance) or IsInfinite(ATolerance) or
    (ATolerance < 0) or (ATolerance > MaximumNyxDiagramCoordinate) then
  begin
    raise ENyxDiagram.Create('Port picking requires a graph and finite nonnegative world tolerance');
  end;
  Result := Default(TNyxDiagramPortHit);
  Result.Distance := ATolerance;
  for LVertexIndex := ADiagram.VertexCount - 1 downto 0 do
  begin
    LVertex := ADiagram.Vertex(LVertexIndex);
    for LPortIndex := LVertex.PortCount - 1 downto 0 do
    begin
      LPoint := NyxDiagramPortPoint(LVertex, LVertex.Port(LPortIndex).Reference);
      LDistance := Hypot(LPoint.X - APoint.X, LPoint.Y - APoint.Y);

      if (LDistance <= ATolerance) and
        (not Result.Defined or (LDistance < Result.Distance)) then
      begin
        Result.Vertex := LVertex.Reference;
        Result.Port := LVertex.Port(LPortIndex).Reference;
        Result.Point := LPoint;
        Result.Distance := LDistance;
      end;
    end;
  end;
end;

function RemoveNyxDiagramPort(const ADiagram: INyxDiagram;
  const AVertex: TNyxDiagramVertexRef; const APort: TNyxDiagramPortRef;
  APolicy: TNyxDiagramRemoval): INyxDiagram;
var
  LCandidate: INyxDiagram;
  LVertex: TNyxDiagramVertex;
  LEdge: TNyxDiagramEdge;
  LIndex: Integer;
  LIncident: Boolean;
begin

  if ADiagram = nil then
  begin
    raise ENyxDiagram.Create('Port removal requires its source graph');
  end;
  RequireOrdinal(Ord(APolicy), Ord(High(TNyxDiagramRemoval)));
  LVertex := ADiagram.FindVertex(AVertex);
  LVertex.FindPort(APort);
  LCandidate := ADiagram;
  for LIndex := 0 to ADiagram.EdgeCount - 1 do
  begin
    LEdge := ADiagram.Edge(LIndex);
    LIncident := ((LEdge.Start.Kind = ndekPort) and
      (LEdge.Start.Vertex.Name = AVertex.Name) and (LEdge.Start.Port.Name = APort.Name)) or
      ((LEdge.Finish.Kind = ndekPort) and (LEdge.Finish.Vertex.Name = AVertex.Name) and
      (LEdge.Finish.Port.Name = APort.Name));

    if LIncident then
    begin

      if APolicy = ndrmRequireDisconnected then
      begin
        raise ENyxDiagram.Create('This port has connected lines; explicitly include them in removal');
      end;
      LCandidate := LCandidate.WithoutEdge(LEdge.Reference);
    end;
  end;
  Result := LCandidate.WithVertex(LVertex.WithoutPort(APort));
end;
