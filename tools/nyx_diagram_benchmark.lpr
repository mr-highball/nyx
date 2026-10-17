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
program nyx_diagram_benchmark;

{$mode delphi}{$H+}{$codepage utf8}

{ Measure complete portable graph phases at 16/64/256 vertices and twice as many
  edges. Named ports, parallel edges, three outlines and orthogonal/self routes
  exercise ordinary admission/projection; no abbreviated validator is installed.
  Fixture construction, output serialization and assertions are outside timing.
  --snapshot emits whole graph/drawing/route/moved results instead of timings for
  exact comparison between builds. This program neither mounts UI nor measures
  paint, input latency, browser execution, peak memory or production budgets. }

uses
  SysUtils,
  {$ifdef PAS2JS}Web,{$else}{$ifdef MSWINDOWS}Windows,{$endif}{$endif}
  nyx.text, nyx.data, nyx.canvas.geometry, nyx.diagram, nyx.diagram.drawing,
  nyx.drawing;

type
  { Fixture-owned value vectors used only for complete output/sampling packets.
    NyxArray captures them by value; no caller storage escapes the benchmark. }
  TBenchmarkValues = array of TNyxDataValue;

  { Closed measured operations. Each consumes the same immutable original plan;
    MoveProjection includes ordinary edit validation and complete drawing capture. }
  TDiagramPhase = (dpCapture, dpAdmission, dpRoutes, dpProjection, dpMoveProjection);

{$if not defined(PAS2JS) and defined(MSWINDOWS)}
var
  GFrequency: Int64;
{$endif}

function Clock: Double;
{$if not defined(PAS2JS) and defined(MSWINDOWS)}
var
  LCounter: Int64;
{$endif}
begin
  {$ifdef PAS2JS}
  Result := window.performance.now;
  {$else}
  {$ifdef MSWINDOWS}
  QueryPerformanceCounter(LCounter);
  Result := LCounter / GFrequency * 1000;
  {$else}
  Result := GetTickCount64;
  {$endif}
  {$endif}
end;

procedure Require(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxDiagram.Create('Diagram benchmark: ' + AMessage);
  end;
end;

function VertexRef(AIndex: Integer): TNyxDiagramVertexRef;
begin
  Result := NyxDiagramVertexRef('Step ' + TNyxText(IntToStr(AIndex)));
end;

function Fixture(AVertices: Integer): INyxDiagram;
var
  LIndex: Integer;
  LVertex: TNyxDiagramVertex;
  LEdge: TNyxDiagramEdge;
  LNext: Integer;
begin
  Result := NewNyxDiagram;
  for LIndex := 0 to AVertices - 1 do
  begin
    LVertex := NyxDiagramVertex(VertexRef(LIndex), TNyxDiagramShape(LIndex mod 3),
      NyxCanvasBounds((LIndex mod 16) * 160 - 640, (LIndex div 16) * 120 - 240, 96, 64))
      .Captioned('Step ' + TNyxText(IntToStr(LIndex)))
      .WithPort(NyxDiagramPort(NyxDiagramPortRef('Input'), 0, 0.5).Directed(ndaLeft))
      .WithPort(NyxDiagramPort(NyxDiagramPortRef('Output'), 1, 0.5).Directed(ndaRight));
    Result := Result.WithVertex(LVertex);
  end;
  for LIndex := 0 to AVertices * 2 - 1 do
  begin
    LNext := (LIndex + 1) mod AVertices;

    if LIndex mod 7 = 0 then
    begin
      LNext := LIndex mod AVertices;
    end;
    LEdge := NyxDiagramEdge(NyxDiagramEdgeRef('Connection ' + TNyxText(IntToStr(LIndex))),
      NyxDiagramPortEnd(VertexRef(LIndex mod AVertices), NyxDiagramPortRef('Output')),
      NyxDiagramPortEnd(VertexRef(LNext), NyxDiagramPortRef('Input')))
      .Routed(ndrOrthogonal).Arrows(ndaNone, ndaOpen);
    Result := Result.WithEdge(LEdge);
  end;
end;

function Routes(const APlan: INyxDiagram): TNyxDataValue;
var
  LEdges: TBenchmarkValues;
  LPoints: TBenchmarkValues;
  LRoute: TNyxDiagramRoute;
  LIndex: Integer;
  LPoint: Integer;
begin
  SetLength(LEdges, APlan.EdgeCount);
  for LIndex := 0 to APlan.EdgeCount - 1 do
  begin
    LRoute := APlan.Route(APlan.Edge(LIndex).Reference);
    SetLength(LPoints, LRoute.Count);
    for LPoint := 0 to LRoute.Count - 1 do
    begin
      LPoints[LPoint] := NyxArray([NyxData(LRoute.Point(LPoint).X), NyxData(LRoute.Point(LPoint).Y)]);
    end;
    LEdges[LIndex] := NyxArray(LPoints);
  end;
  Result := NyxArray(LEdges);
end;

function PhaseName(APhase: TDiagramPhase): TNyxText;
begin
  case APhase of
    dpCapture: Result := 'capture';
    dpAdmission: Result := 'admission';
    dpRoutes: Result := 'routes';
    dpProjection: Result := 'projection';
    dpMoveProjection: Result := 'move-projection';
  end;
end;

procedure Measure(AVertices: Integer; ASnapshot: Boolean);
var
  LOriginal: INyxDiagram;
  LCandidate: INyxDiagram;
  LDrawing: INyxDrawingPlan;
  LMovedDrawing: INyxDrawingPlan;
  LData: TNyxDataValue;
  LCaptured: TNyxDataValue;
  LBefore: TNyxText;
  LMoved: TNyxDiagramVertex;
  LRoute: TNyxDiagramRoute;
  LPhase: TDiagramPhase;
  LSample: Integer;
  LIndex: Integer;
  LPoint: Integer;
  LClock: Double;
  LChecksum: Double;
  LTimes: array[0..2] of Double;
  LValues: TBenchmarkValues;
  LSwap: Double;

  { One complete operation, including replacement/release of its previous result.
    Every plan stays independently owned; output checks occur outside its clock. }
  procedure Run;
  var
    LEdgeIndex: Integer;
    LPointIndex: Integer;
  begin
    case LPhase of
      dpCapture: LCaptured := LOriginal.ToData;
      dpAdmission: LCandidate := NyxDiagramFromData(LData);
      dpRoutes:
        begin
          LChecksum := 0;
          for LEdgeIndex := 0 to LOriginal.EdgeCount - 1 do
          begin
            LRoute := LOriginal.Route(LOriginal.Edge(LEdgeIndex).Reference);
            for LPointIndex := 0 to LRoute.Count - 1 do
            begin
              LChecksum := LChecksum + LRoute.Point(LPointIndex).X + LRoute.Point(LPointIndex).Y;
            end;
          end;
        end;
      dpProjection: LDrawing := DrawNyxDiagramEditor(LOriginal, VertexRef(0),
        Default(TNyxDiagramEdgeRef), 8, 8, Default(TNyxCanvasBounds));
      dpMoveProjection:
        begin
          LCandidate := LOriginal.WithVertex(LMoved);
          LMovedDrawing := DrawNyxDiagramEditor(LCandidate, VertexRef(0),
            Default(TNyxDiagramEdgeRef), 8, 8, Default(TNyxCanvasBounds));
        end;
    end;
  end;

begin
  LOriginal := Fixture(AVertices);
  LData := LOriginal.ToData;
  LBefore := LData.ToJSON;
  LMoved := LOriginal.FindVertex(VertexRef(0));
  LMoved := LMoved.AtBounds(NyxCanvasBounds(LMoved.Bounds.Left + 11.5,
    LMoved.Bounds.Top - 7.25, LMoved.Bounds.Width, LMoved.Bounds.Height));
  LChecksum := 0;
  for LPhase := Low(TDiagramPhase) to High(TDiagramPhase) do
  begin
    Run;

    if not ASnapshot then
    begin
      SetLength(LValues, Length(LTimes));
      for LSample := 0 to High(LTimes) do
      begin
        LClock := Clock;
        Run;
        LTimes[LSample] := Clock - LClock;
        LValues[LSample] := NyxData(LTimes[LSample]);
      end;
      for LIndex := 0 to High(LTimes) - 1 do
      begin
        for LPoint := LIndex + 1 to High(LTimes) do
        begin

          if LTimes[LIndex] > LTimes[LPoint] then
          begin
            LSwap := LTimes[LIndex];
            LTimes[LIndex] := LTimes[LPoint];
            LTimes[LPoint] := LSwap;
          end;
        end;
      end;
      WriteLn(NyxObject([NyxField('vertices', NyxData(AVertices)),
        NyxField('edges', NyxData(AVertices * 2)), NyxField('phase', NyxData(PhaseName(LPhase))),
        NyxField('samplesMs', NyxArray(LValues)), NyxField('medianMs', NyxData(LTimes[1]))]).ToJSON);
    end;
  end;
  Require((LOriginal.VertexCount = AVertices) and (LOriginal.EdgeCount = AVertices * 2),
    'the complete workload must remain admitted');
  Require(LOriginal.ToData.ToJSON = LBefore, 'measured reads/edits changed the original graph');
  Require((LDrawing <> nil) and (LMovedDrawing <> nil) and (LCandidate <> nil),
    'complete projection and edit outputs must exist');
  Require(LCaptured.ToJSON = LBefore, 'capture lost graph intent');
  Require(LCandidate.FindVertex(VertexRef(0)).Bounds.Left = LMoved.Bounds.Left,
    'move-projection skipped the complete graph edit');

  if ASnapshot then
  begin
    WriteLn(NyxObject([NyxField('vertices', NyxData(AVertices)),
      NyxField('graph', LData), NyxField('routes', Routes(LOriginal)),
      NyxField('drawing', LDrawing.ToData), NyxField('moved', LCandidate.ToData),
      NyxField('movedDrawing', LMovedDrawing.ToData), NyxField('routeChecksum', NyxData(LChecksum))]).ToJSON);
  end;
end;

begin
  {$if not defined(PAS2JS) and defined(MSWINDOWS)}
  Require(QueryPerformanceFrequency(GFrequency) and (GFrequency > 0), 'high-resolution clock unavailable');
  {$endif}
  Measure(16, (ParamCount > 0) and (ParamStr(1) = '--snapshot'));
  Measure(64, (ParamCount > 0) and (ParamStr(1) = '--snapshot'));
  Measure(256, (ParamCount > 0) and (ParamStr(1) = '--snapshot'));
end.
