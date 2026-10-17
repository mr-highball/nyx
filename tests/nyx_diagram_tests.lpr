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
program nyx_diagram_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, fpjson, nyx.text, nyx.bytes, nyx.data, nyx.diagram, nyx.diagram.drawing,
  nyx.drawing, nyx.canvas.geometry, nyx.test.diagram
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

{$ifndef PAS2JS}
type
  { Joined worker reads a published immutable plan and makes private edits.
    The main thread retains that plan until both joins; no live UI/model or
    shared mutable result is touched. Failure is captured for the joining owner. }
  TSnapshotWorker = class(TThread)
  public
    Plan: INyxDiagram;
    Failed: Boolean;
    procedure Execute; override;
  end;
{$endif}

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Diagram: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Near(AActual, AExpected: Double; const AReason: TNyxText);
begin
  Check(Abs(AActual - AExpected) < 0.000001, AReason);
end;

function Replacing(const AObject: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AObject.Count);
  for LIndex := 0 to AObject.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AObject.Key(LIndex), AObject.Field(AObject.Key(LIndex)));

    if AObject.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
    end;
  end;
  Result := NyxObject(LFields);
end;

{$ifndef PAS2JS}
procedure TSnapshotWorker.Execute;
var
  LIndex: Integer;
  LPrivate: INyxDiagram;
  LRoute: TNyxDiagramRoute;
begin
  try
    for LIndex := 1 to 50 do
    begin
      LPrivate := CaptureNyxDiagram(Plan).WithVertex(Plan.Vertex(0).AtBounds(NyxCanvasBounds(LIndex, 0, 120, 80)));
      LRoute := LPrivate.Route(NyxDiagramEdgeRef('review'));

      if (LRoute.Point(0).X <> LIndex + 120) or (Plan.Vertex(0).Bounds.Left <> 0) then
      begin
        Failed := True;
      end;
    end;
  except
    Failed := True;
  end;
end;
{$endif}

{ Whole-byte admission remains necessary for fluent owned graphs too. A fresh
  fork must not inherit its parent's certificate; port captions exercise UTF-8
  bytes independently of scalar limits. No oversized plan is published to UI. }
procedure CaptureBudget;
var
  LPlan: INyxDiagram;
  LCaptured: INyxDiagram;
  LReduced: INyxDiagram;
  LVertex: TNyxDiagramVertex;
  LNext: TNyxDiagramVertex;
  LCaption: TNyxText;
  LIndex: Integer;
  LPort: Integer;
  LCount: Integer;
  LFailed: Boolean;
begin
  LPlan := NewNyxDiagram;
  LCaptured := CaptureNyxDiagram(LPlan);
  LCaption := '';
  for LIndex := 1 to 256 do
  begin
    LCaption := LCaption + TNyxText('𝄞');
  end;
  LVertex := NyxDiagramVertex(NyxDiagramVertexRef('Large 000'), ndsRectangle,
    NyxCanvasBounds(0, 0, 96, 64));
  for LPort := 0 to MaximumNyxDiagramPortsPerVertex - 1 do
  begin
    LVertex := LVertex.WithPort(NyxDiagramPort(NyxDiagramPortRef('Port ' + TNyxText(IntToStr(LPort))),
      0.5, 0.5).Captioned(LCaption));
  end;
  LCount := MaximumNyxDiagramBytes div NyxUTF8ByteCount(LVertex.ToData.ToJSON) + 1;
  Check((LCount > 2) and (LCount <= MaximumNyxDiagramVertices),
    'byte qualification crosses its own budget within typed member limits');
  for LIndex := 0 to LCount - 1 do
  begin
    LNext := NyxDiagramVertex(NyxDiagramVertexRef(
      'Large ' + TNyxText(Format('%.3d', [LIndex]))), ndsRectangle, LVertex.Bounds);
    for LPort := 0 to LVertex.PortCount - 1 do
    begin
      { Prepare one complete record locally; appending ports must not repeatedly
        fork the whole graph or accidentally certify an intermediate plan. }
      LNext := LNext.WithPort(LVertex.Port(LPort));
    end;
    LPlan := LPlan.WithVertex(LNext);
  end;
  LFailed := False;
  try
    LCaptured := CaptureNyxDiagram(LPlan);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LCaptured.VertexCount = 0),
    'fresh fluent forks cannot inherit a successful whole-byte certificate');
  LReduced := LPlan.WithoutVertex(LPlan.Vertex(LPlan.VertexCount - 1).Reference);
  LReduced := LReduced.WithoutVertex(LReduced.Vertex(LReduced.VertexCount - 1).Reference);
  LCaptured := CaptureNyxDiagram(LReduced);
  Check((LCaptured = LReduced) and (NyxUTF8ByteCount(LCaptured.ToData.ToJSON) <= MaximumNyxDiagramBytes),
    'a reduced independent graph can admit its own canonical bytes after earlier refusal');
  LFailed := False;
  try
    CaptureNyxDiagram(LPlan);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'capturing a reduced graph never certifies the still-oversized original');
end;

{ JSON's existing slash preference may change between capture and interchange.
  A whole-plan bound must cover either spelling without mutating accepted intent.
  This single-threaded fixture restores the preference before joined readers. }
procedure CaptureEscaping;
var
  LStrict: Boolean;
  LPlan: INyxDiagram;
  LCaptured: INyxDiagram;
  LReduced: INyxDiagram;
  LVertex: TNyxDiagramVertex;
  LName: TNyxText;
  LBefore: TNyxText;
begin
  LStrict := TJSONString.StrictEscaping;
  try
    TJSONString.StrictEscaping := False;
    LName := 'Shape / "quoted" \ 🌙';
    LVertex := NyxDiagramVertex(NyxDiagramVertexRef(LName), ndsDiamond,
      NyxCanvasBounds(-1.0e9, -1.0e9, 1000000, 1000000))
      .Captioned('Caption / "quoted" \ 𝄞')
      .WithPort(NyxDiagramPort(NyxDiagramPortRef('Output / 🌙'), 0.5, 0.5)
        .Captioned('Port / "quoted" \ 𝄞'));
    LPlan := NewNyxDiagram.WithVertex(LVertex)
      .WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('Edge / "quoted" \ 🌙'),
        NyxDiagramPortEnd(LVertex.Reference, LVertex.Port(0).Reference),
        NyxDiagramFreeEnd(NyxCanvasPoint(-0.00000123456789012345, -1.0e-300))));
    LCaptured := CaptureNyxDiagram(LPlan);
    LBefore := LCaptured.ToData.ToJSON;
    Check((LCaptured = LPlan) and (NyxUTF8ByteCount(LBefore) < MaximumNyxDiagramBytes),
      'whole capture retains escaped Unicode names, ports, captions and extreme finite values');
    TJSONString.StrictEscaping := True;
    Check(NyxDiagramFromData(LCaptured.ToData).ToData.ToJSON = LCaptured.ToData.ToJSON,
      'changing slash policy preserves complete canonical round-trip admission');
    LReduced := LPlan.WithVertex(LVertex.Captioned('A fresh / caption'));
    Check((CaptureNyxDiagram(LReduced) = LReduced) and
      (LPlan.Vertex(0).Caption = LVertex.Caption),
      'fresh edited capture remains independent under strict slash encoding');
    TJSONString.StrictEscaping := False;
    Check(LCaptured.ToData.ToJSON = LBefore,
      'restoring wire formatting retains the exact original complete snapshot');
  finally
    TJSONString.StrictEscaping := LStrict;
  end;
end;

procedure Run;
var
  LPlan: INyxDiagram;
  LCopy: INyxDiagram;
  LOther: INyxDiagram;
  LBefore: TNyxText;
  LRoute: TNyxDiagramRoute;
  LOldRoute: TNyxDiagramRoute;
  LEdge: TNyxDiagramEdge;
  LData: TNyxDataValue;
  LBad: TNyxDataValue;
  LItem: TNyxDataValue;
  LDrawing: INyxDrawingPlan;
  LIndex: Integer;
  LCase: Integer;
  LSide: Integer;
  LOtherSide: Integer;
  LMidpoint: TNyxCanvasPoint;
  LFailed: Boolean;
  LName: TNyxText;
  LUnicode: TNyxText;
  LExpected: Double;
  {$ifndef PAS2JS}
  LWorkers: array[0..1] of TSnapshotWorker;
  {$endif}
begin
  LPlan := NewNyxDiagram;
  Check((LPlan.VertexCount = 0) and (LPlan.EdgeCount = 0), 'independent empty plan');
  Check(DrawNyxDiagram(LPlan).Count = 0, 'empty drawing is explicit');
  LPlan := DiagramFixture;
  LBefore := LPlan.ToData.ToJSON;
  LData := LPlan.ToData;
  Check(NyxDiagramFromData(Replacing(LData, 'version', NyxData(2))).ToData.ToJSON = LBefore,
    'version two accepts plain vertices and canonicalizes their unchanged legacy bytes');
  Check(NyxDiagramFromData(Replacing(LData, 'version', NyxData(3))).ToData.ToJSON = LBefore,
    'version three also canonicalizes plain legacy graphs without inventing ports');
  Check((LPlan.VertexCount = 3) and (LPlan.EdgeCount = 4), 'fixture membership');
  LCopy := NyxDiagramFromData(TNyxDataValue.ParseJSON(LBefore));
  Check(LCopy.ToData.ToJSON = LBefore, 'complete canonical codec equality');
  Check(DrawNyxDiagram(LCopy).ToData.ToJSON = DrawNyxDiagram(LPlan).ToData.ToJSON,
    'complete independent drawing equality');
  LOldRoute := LPlan.Route(NyxDiagramEdgeRef('review'));
  Near(LOldRoute.Point(0).X, 120, 'right midpoint starts on rectangle');
  Near(LOldRoute.Point(LOldRoute.Count - 1).X, 240, 'left midpoint ends on diamond');
  LCopy := LPlan.WithVertex(LPlan.Vertex(1).AtBounds(NyxCanvasBounds(300, 180, 180, 120)));
  LRoute := LCopy.Route(NyxDiagramEdgeRef('review'));
  Near(LRoute.Point(LRoute.Count - 1).X, 300, 'move reroutes destination');
  Near(LRoute.Point(LRoute.Count - 1).Y, 240, 'resize updates boundary midpoint');
  Check((LPlan.ToData.ToJSON = LBefore) and (LOldRoute.Point(LOldRoute.Count - 1).X = 240),
    'retained plan and route stay independent');
  for LIndex := 1 to LRoute.Count - 1 do
  begin
    Check((LRoute.Point(LIndex - 1).X = LRoute.Point(LIndex).X) or
      (LRoute.Point(LIndex - 1).Y = LRoute.Point(LIndex).Y), 'orthogonal segment');
  end;
  Check(LCopy.Vertex(1).Reference.Name = 'decision', 'upsert retains paint order');
  Check(LPlan.PickVertex(NyxCanvasPoint(20, 20)).Name = 'start', 'rectangle hit');
  Check(not LPlan.PickVertex(NyxCanvasPoint(241, -9)).Defined, 'diamond corner misses');
  Check(LPlan.PickVertex(NyxCanvasPoint(300, -10)).Name = 'decision', 'diamond closed tip hits');
  Check(not LPlan.PickVertex(NyxCanvasPoint(481, 1)).Defined, 'ellipse corner misses');
  Check(LPlan.PickVertex(NyxCanvasPoint(540, 40)).Name = 'finish', 'ellipse center hits');
  LCopy := LPlan.WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('front'), ndsRectangle,
    NyxCanvasBounds(0, 0, 120, 80)));
  Check(LCopy.PickVertex(NyxCanvasPoint(20, 20)).Name = 'front', 'last painted shape wins');
  Check(LPlan.PickEdge(NyxCanvasPoint(180, 40), 0).Name = 'review', 'exact edge hit');
  Check(not LPlan.PickEdge(NyxCanvasPoint(180, 47), 6).Defined, 'edge tolerance excludes distant point');
  Check(LPlan.PickEdge(NyxCanvasPoint(180, 46), 6).Name = 'review', 'closed hit tolerance');
  LCopy := LPlan.WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('parallel'),
    LPlan.Edge(0).Start, LPlan.Edge(0).Finish).Routed(ndrOrthogonal));
  Check(LCopy.PickEdge(NyxCanvasPoint(180, 40), 0).Name = 'parallel', 'parallel edges retain distinct order');
  LRoute := LPlan.Route(NyxDiagramEdgeRef('retry'));
  Check(LRoute.Count >= 4, 'self-loop has an external orthogonal route');
  Near(LRoute.Point(0).X, 600, 'loop automatic start uses right boundary');
  Near(LRoute.Point(LRoute.Count - 1).Y, 0, 'loop automatic finish uses top boundary');

  { Independent geometric oracle: every resolved loop segment midpoint stays
    outside the source rectangle's open interior for all twelve side pairs.
    This catches visually covered routes even when endpoint/count checks pass. }
  for LSide := Ord(ndaLeft) to Ord(ndaBottom) do
  begin
    for LOtherSide := Ord(ndaLeft) to Ord(ndaBottom) do
    begin

      if LSide = LOtherSide then
      begin
        Continue;
      end;
      LOther := NewNyxDiagram
        .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('loop-shape'), ndsRectangle,
          NyxCanvasBounds(0, 0, 120, 80)))
        .WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('loop'),
          NyxDiagramAttached(NyxDiagramVertexRef('loop-shape'), TNyxDiagramAttachment(LSide)),
          NyxDiagramAttached(NyxDiagramVertexRef('loop-shape'), TNyxDiagramAttachment(LOtherSide)))
          .Routed(ndrOrthogonal));
      LRoute := LOther.Route(NyxDiagramEdgeRef('loop'));
      for LIndex := 1 to LRoute.Count - 1 do
      begin
        LMidpoint := NyxCanvasPoint((LRoute.Point(LIndex - 1).X + LRoute.Point(LIndex).X) / 2,
          (LRoute.Point(LIndex - 1).Y + LRoute.Point(LIndex).Y) / 2);
        Check(not ((LMidpoint.X > 0) and (LMidpoint.X < 120) and
          (LMidpoint.Y > 0) and (LMidpoint.Y < 80)), 'loop segment stays outside source');
      end;
    end;
  end;

  { Independent mathematical oracle for floating attachments along a diagonal. }
  for LCase := 0 to 2 do
  begin
    LOther := NewNyxDiagram.WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('shape'),
      TNyxDiagramShape(LCase), NyxCanvasBounds(-50, -50, 100, 100)))
      .WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('diagonal'),
        NyxDiagramAttached(NyxDiagramVertexRef('shape')),
        NyxDiagramFreeEnd(NyxCanvasPoint(100, 100))));
    LRoute := LOther.Route(NyxDiagramEdgeRef('diagonal'));
    case LCase of
      0: LExpected := 50;
      1: LExpected := 50 / Sqrt(2);
    else
      LExpected := 25;
    end;
    Near(LRoute.Point(0).X, LExpected, 'floating shape boundary x');
    Near(LRoute.Point(0).Y, LExpected, 'floating shape boundary y');
    Near(LRoute.Point(1).X, 100, 'free end remains exact');
  end;

  LUnicode := 'Idea ' + NyxScalarText($1F680);
  LCopy := LPlan.WithVertex(NyxDiagramVertex(NyxDiagramVertexRef(LUnicode), ndsEllipse,
    NyxCanvasBounds(-200.125, 100.25, 120.5, 80.75)).Captioned('Review ' + NyxScalarText($1F9ED)));
  LOther := NyxDiagramFromData(LCopy.ToData);
  Check(LOther.FindVertex(NyxDiagramVertexRef(LUnicode)).Caption =
    'Review ' + NyxScalarText($1F9ED), 'supplementary identity and caption remain exact');
  Near(LOther.Vertex(3).Bounds.Left, -200.125, 'signed fractional geometry remains exact');
  Check(not LOther.PickVertex(NyxCanvasPoint(-500, -500)).Defined, 'absent hit is typed');
  LOther := LPlan.WithoutVertex(NyxDiagramVertexRef('decision'), ndrmIncidentEdges);
  Check((LOther.VertexCount = 2) and (LOther.EdgeCount = 1) and
    (LOther.Edge(0).Reference.Name = 'retry'), 'explicit incident removal retains unrelated loop');
  LOther := LPlan.WithoutEdge(NyxDiagramEdgeRef('annotation'));
  Check((LOther.EdgeCount = 3) and (LPlan.EdgeCount = 4), 'edge removal preserves retained history');

  for LCase := 0 to 19 do
  begin
    LFailed := False;
    try
      case LCase of
        0: LOther := LPlan.WithoutVertex(NyxDiagramVertexRef('decision'));
        1: LOther := LPlan.WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('dangling'),
          NyxDiagramAttached(NyxDiagramVertexRef('missing')), LPlan.Edge(0).Finish));
        2: LOther := LPlan.WithEdge(LPlan.Edge(2).Routed(ndrStraight));
        3: LOther := LPlan.WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('same-port'),
          NyxDiagramAttached(NyxDiagramVertexRef('start'), ndaRight),
          NyxDiagramAttached(NyxDiagramVertexRef('start'), ndaRight)).Routed(ndrOrthogonal));
        4: LPlan.Vertex(0).AtBounds(NyxCanvasBounds(0, 0, 7, 80));
        5: LPlan.Vertex(0).AtBounds(NyxCanvasBounds(1.0e9, 0, 120, 80));
        6: LPlan.Vertex(0).Captioned('two' + #10 + 'lines');
        7: LPlan.Vertex(0).Captioned(StringOfChar('a', 257));
        8: LRoute.Point(-1);
        9: LPlan.Vertex(3);
        10: LPlan.Edge(4);
        11: LPlan.PickEdge(NyxCanvasPoint(0, 0), -1);
        12: LOther := LPlan.WithoutEdge(NyxDiagramEdgeRef('absent'));
        13: LOther := LPlan.WithVertex(Default(TNyxDiagramVertex));
        14: LOther := LPlan.WithEdge(Default(TNyxDiagramEdge));
        15: LOther := NyxDiagramFromData(Replacing(LData, 'version', NyxData(4)));
        16:
          begin
            LBad := NyxArray([LData.Field('vertices').Item(0), LData.Field('vertices').Item(0)]);
            LOther := NyxDiagramFromData(Replacing(LData, 'vertices', LBad));
          end;
        17:
          begin
            LBad := NyxArray([LData.Field('edges').Item(0), LData.Field('edges').Item(0)]);
            LOther := NyxDiagramFromData(Replacing(LData, 'edges', LBad));
          end;
        18:
          begin
            LItem := Replacing(LData.Field('vertices').Item(0), 'shape', NyxData(99));
            LOther := NyxDiagramFromData(Replacing(LData, 'vertices', NyxArray([LItem])));
          end;
        19: DrawNyxDiagram(nil);
      end;
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'invalid candidate refuses ' + IntToStr(LCase));
    Check(LPlan.ToData.ToJSON = LBefore, 'refusal retains entire accepted plan ' + IntToStr(LCase));
  end;

  { Verify strict wire types rather than string/rounded behavioral coercion. }
  for LCase := 0 to 5 do
  begin
    case LCase of
      0: LBad := Replacing(LData, 'version', NyxData('1'));
      1: LBad := Replacing(LData, 'version', TNyxDataValue.ParseJSON('1.0'));
      2: LBad := Replacing(LData, 'edges', NyxNull);
      3:
        begin
          LItem := Replacing(LData.Field('edges').Item(0), 'routing', NyxData('orthogonal'));
          LBad := Replacing(LData, 'edges', NyxArray([LItem]));
        end;
      4:
        begin
          LItem := Replacing(LData.Field('edges').Item(0), 'finish',
            NyxObject([NyxField('point', NyxArray([NyxData('2'), NyxData(3)]))]));
          LBad := Replacing(LData, 'edges', NyxArray([LItem]));
        end;
      5: LBad := NyxObject([NyxField('version', NyxData(1)),
        NyxField('vertices', NyxArray([])), NyxField('edges', NyxArray([])),
        NyxField('unknown', NyxData(True))]);
    end;
    LFailed := False;
    try
      LOther := NyxDiagramFromData(LBad);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LPlan.ToData.ToJSON = LBefore), 'strict wire refusal ' + IntToStr(LCase));
  end;

  LOther := NewNyxDiagram;
  for LIndex := 0 to MaximumNyxDiagramVertices - 1 do
  begin
    LName := 'Shape ' + IntToStr(LIndex);
    LOther := LOther.WithVertex(NyxDiagramVertex(NyxDiagramVertexRef(LName),
      TNyxDiagramShape(LIndex mod 3), NyxCanvasBounds(0, 0, 100, 80)).Captioned('Item'));
  end;
  LFailed := False;
  try
    LOther.WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('overflow'), ndsRectangle,
      NyxCanvasBounds(0, 0, 100, 80)));
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LOther.VertexCount = MaximumNyxDiagramVertices), 'vertex capacity preserves full baseline');
  for LIndex := 0 to MaximumNyxDiagramEdges - 1 do
  begin
    LEdge := NyxDiagramEdge(NyxDiagramEdgeRef('Line ' + IntToStr(LIndex)),
      NyxDiagramFreeEnd(NyxCanvasPoint(0, 0)),
      NyxDiagramFreeEnd(NyxCanvasPoint(100, 80))).Arrows(ndaOpen, ndaOpen);
    LOther := LOther.WithEdge(LEdge);
  end;
  LFailed := False;
  try
    LOther.WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('overflow'), LEdge.Start, LEdge.Finish));
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LOther.EdgeCount = MaximumNyxDiagramEdges), 'edge capacity preserves full baseline');
  LDrawing := DrawNyxDiagram(LOther);
  Check(LDrawing.Count = MaximumNyxDrawingCommands, 'maximum membership including two arrows fits command capacity');
  Check(NyxDiagramFromData(LOther.ToData).ToData.ToJSON = LOther.ToData.ToJSON,
    'maximum membership complete codec equality');

  {$ifndef PAS2JS}
  { Identical fresh intent has no inherited byte certificate. Joined readers
    may perform the first pure byte check concurrently, then share admission. }
  LPlan := LPlan.WithVertex(LPlan.Vertex(0));
  for LIndex := 0 to High(LWorkers) do
  begin
    LWorkers[LIndex] := TSnapshotWorker.Create(True);
    LWorkers[LIndex].Plan := LPlan;
    LWorkers[LIndex].Start;
  end;
  for LIndex := 0 to High(LWorkers) do
  begin
    LWorkers[LIndex].WaitFor;
    Check(not LWorkers[LIndex].Failed, 'joined immutable snapshot worker');
    LWorkers[LIndex].Free;
  end;
  Check(LPlan.ToData.ToJSON = LBefore, 'worker edits leave accepted snapshot exact');
  {$endif}
end;

begin
  try
    CaptureEscaping;
    CaptureBudget;
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-diagram', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' diagram checks';
    {$else}
    WriteLn('PASS ', GChecks, ' diagram checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram', 'failed');
      document.body.setAttribute('data-diagram-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
