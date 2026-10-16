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
program nyx_diagram_port_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Math, nyx.text, nyx.data, nyx.diagram, nyx.canvas.geometry,
  nyx.diagram.drawing, nyx.drawing
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Run;
var
  LVertex: TNyxDiagramVertex;
  LChanged: TNyxDiagramVertex;
  LOther: TNyxDiagramVertex;
  LPort: TNyxDiagramPort;
  LPlan: INyxDiagram;
  LCopy: INyxDiagram;
  LRoute: TNyxDiagramRoute;
  LHit: TNyxDiagramPortHit;
  LBefore: TNyxText;
  LFailed: Boolean;
  LIndex: Integer;
  LValue: TNyxDataValue;
  LPorts: TNyxDataValue;
  LHidden: INyxDrawingPlan;
  LVisible: INyxDrawingPlan;
begin
  LPort := NyxDiagramPort(NyxDiagramPortRef('Result 🌙'), 1, 0.25)
    .Directed(ndaRight).Captioned('Result output 🌙');
  LVertex := NyxDiagramVertex(NyxDiagramVertexRef('Form'), ndsRectangle,
    NyxCanvasBounds(10, 20, 100, 80)).WithPort(LPort);
  Check((LVertex.PortCount = 1) and
    (LVertex.Port(0).Caption = TNyxText('Result output 🌙')), 'named ports preserve supplementary help text');
  Check((NyxDiagramPortPoint(LVertex, LPort.Reference).X = 110) and
    (NyxDiagramPortPoint(LVertex, LPort.Reference).Y = 40), 'relative ports resolve exact world coordinates');
  LChanged := LVertex.WithPort(NyxDiagramPort(LPort.Reference, 0.5, 0.5).Directed(ndaLeft));
  Check((LVertex.Port(0).Position.X = 1) and (LChanged.Port(0).Position.X = 0.5),
    'port upsert detaches value storage without changing the accepted vertex');
  LChanged := LVertex.WithPort(NyxDiagramPort(NyxDiagramPortRef('Extra'), 1, 0.75));
  LChanged := LChanged.WithoutPort(LPort.Reference);
  Check((LVertex.PortCount = 1) and (LChanged.PortCount = 1) and
    (LChanged.Port(0).Reference.Name = 'Extra'), 'port removal preserves independent immutable arrays');
  LOther := NyxDiagramVertex(NyxDiagramVertexRef('Destination'), ndsEllipse,
    NyxCanvasBounds(300, 20, 100, 80)).WithPort(
      NyxDiagramPort(NyxDiagramPortRef('Input'), 0, 0.75).Directed(ndaLeft));
  LPlan := NewNyxDiagram.WithVertex(LVertex).WithVertex(LOther)
    .WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('Delivery'),
      NyxDiagramPortEnd(LVertex.Reference, LPort.Reference),
      NyxDiagramPortEnd(LOther.Reference, NyxDiagramPortRef('Input'))));
  LRoute := LPlan.Route(NyxDiagramEdgeRef('Delivery'));
  Check((LRoute.Point(0).X = 110) and (LRoute.Point(0).Y = 40) and
    (LRoute.Point(LRoute.Count - 1).X = 300) and
    (LRoute.Point(LRoute.Count - 1).Y = 80), 'straight named endpoints retain their exact anchored positions');
  LBefore := LPlan.ToData.ToJSON;
  LCopy := NyxDiagramFromData(LPlan.ToData);
  Check((LCopy.ToData.ToJSON = LBefore) and
    (LCopy.ToData.Field('version').AsInteger = 3), 'complete named port graphs round trip exact version-three intent');
  LChanged := LVertex.AtBounds(NyxCanvasBounds(20, 30, 200, 160));
  LCopy := LPlan.WithVertex(LChanged);
  LRoute := LCopy.Route(NyxDiagramEdgeRef('Delivery'));
  Check((LRoute.Point(0).X = 220) and (LRoute.Point(0).Y = 70) and
    (LPlan.ToData.ToJSON = LBefore), 'moving/resizing changes derived anchors without touching the accepted plan');
  LFailed := False;
  try
    LPlan.WithVertex(LVertex.WithoutPort(LPort.Reference));
  except
    on ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LPlan.ToData.ToJSON = LBefore),
    'removing a connected port refuses complete graph admission atomically');
  LFailed := False;
  try
    LPlan.WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('Missing'),
      NyxDiagramPortEnd(LVertex.Reference, NyxDiagramPortRef('Unknown')),
      NyxDiagramFreeEnd(NyxCanvasPoint(500, 200))));
  except
    on ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LPlan.ToData.ToJSON = LBefore), 'unknown ports never degrade to an automatic attachment');
  LHit := PickNyxDiagramPort(LPlan, NyxCanvasPoint(112, 42), 4);
  Check(LHit.Defined and (LHit.Vertex.Name = 'Form') and
    (LHit.Port.Name = TNyxText('Result 🌙')), 'port picking returns an exact detached nearest reference pair');
  Check(not PickNyxDiagramPort(LPlan, NyxCanvasPoint(112, 42), 1).Defined,
    'logical world tolerance excludes points outside the hit radius');
  LChanged := LVertex.WithPort(NyxDiagramPort(NyxDiagramPortRef('Lower'), 1, 0.75)
    .Directed(ndaRight));
  LCopy := NewNyxDiagram.WithVertex(LChanged).WithEdge(
    NyxDiagramEdge(NyxDiagramEdgeRef('Loop'),
      NyxDiagramPortEnd(LChanged.Reference, LPort.Reference),
      NyxDiagramPortEnd(LChanged.Reference, NyxDiagramPortRef('Lower'))).Routed(ndrOrthogonal));
  LRoute := LCopy.Route(NyxDiagramEdgeRef('Loop'));
  Check((LRoute.Point(2).X > LRoute.Point(1).X) and
    (LRoute.Point(2).Y = LRoute.Point(1).Y), 'same-side self loops route outward without retracing their side stub');
  LCopy := LPlan.WithEdge(LPlan.FindEdge(NyxDiagramEdgeRef('Delivery')).Routed(ndrOrthogonal));
  LChanged := LVertex.WithPort(NyxDiagramPort(LPort.Reference, 0.5, 0.5).Directed(ndaRight));
  LCopy := LCopy.WithVertex(LChanged);
  LRoute := LCopy.Route(NyxDiagramEdgeRef('Delivery'));
  Check(LRoute.Point(1).X > LChanged.Bounds.Left + LChanged.Bounds.Width,
    'interior named ports leave the entire shape before an orthogonal bend');
  LCopy := NewNyxDiagram.WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('Legacy'),
    ndsRectangle, NyxCanvasBounds(0, 0, 80, 40)));
  Check(LCopy.ToData.Field('version').AsInteger = 1, 'no-port legacy graphs preserve their original version');
  LChanged := LVertex;
  for LIndex := 1 to MaximumNyxDiagramPortsPerVertex - 1 do
  begin
    LChanged := LChanged.WithPort(NyxDiagramPort(
      NyxDiagramPortRef('Port ' + TNyxText(IntToStr(LIndex))), 0, 0));
  end;
  LFailed := False;
  try
    LChanged.WithPort(NyxDiagramPort(NyxDiagramPortRef('Overflow'), 0, 0));
  except
    on ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LChanged.PortCount = MaximumNyxDiagramPortsPerVertex),
    'port capacity refuses before replacing the accepted value');

  LFailed := False;
  try
    RemoveNyxDiagramPort(LPlan, LVertex.Reference, LPort.Reference);
  except
    on ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LPlan.ToData.ToJSON = LBefore), 'default helper removal refuses exact connected port membership');
  LCopy := RemoveNyxDiagramPort(LPlan, LVertex.Reference, LPort.Reference, ndrmIncidentEdges);
  Check((LCopy.EdgeCount = 0) and (LCopy.Vertex(0).PortCount = 0) and
    (LCopy.Vertex(1).PortCount = 1) and (LPlan.ToData.ToJSON = LBefore),
    'explicit incident removal preserves independent peer ports and original graph');
  LValue := NyxObject([NyxField('version', NyxData(2)),
    NyxField('vertices', LPlan.ToData.Field('vertices')), NyxField('edges', LPlan.ToData.Field('edges'))]);
  LFailed := False;
  try
    NyxDiagramFromData(LValue);
  except
    on ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'older descriptors cannot smuggle named port or endpoint meaning');
  LPorts := NyxArray([LPort.ToData, LPort.ToData]);
  LValue := NyxObject([NyxField('name', NyxData(LVertex.Reference.Name)),
    NyxField('shape', NyxData(Ord(LVertex.Shape))), NyxField('bounds', LVertex.ToData.Field('bounds')),
    NyxField('caption', NyxData(LVertex.Caption)), NyxField('style', LVertex.Style.ToData),
    NyxField('ports', LPorts)]);
  LFailed := False;
  try
    TNyxDiagramVertex.FromData(LValue);
  except
    on ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'duplicate wire references refuse instead of silently upserting');
  LHidden := DrawNyxDiagramEditor(LPlan, LVertex.Reference, Default(TNyxDiagramEdgeRef), 8);
  LVisible := DrawNyxDiagramEditor(LPlan, LVertex.Reference, Default(TNyxDiagramEdgeRef), 8, 10,
    Default(TNyxCanvasBounds));
  Check(LVisible.Count = LHidden.Count + LVertex.PortCount,
    'optional projection adds only the selected vertex named-port glyphs');
  Check(LPlan.ToData.ToJSON = LBefore, 'port presentation leaves saved graph intent exact');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-diagram-ports', 'passed');
    document.body.setAttribute('data-diagram-port-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' named diagram port checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-ports', 'failed');
      document.body.setAttribute('data-diagram-port-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
