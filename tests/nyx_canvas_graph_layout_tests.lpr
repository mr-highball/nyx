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

program nyx_canvas_graph_layout_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.codegen, nyx.controls,
  nyx.source, nyx.types, nyx.root.types, nyx.canvas.types, nyx.canvas.geometry,
  nyx.canvas.declarations, nyx.canvas.editing, nyx.flow, nyx.flow.types,
  nyx.flow.declarations, nyx.flow.routes, nyx.logic, nyx.studio.session,
  nyx.studio.projects, nyx.studio.edits, nyx.schema
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Graph layout: ' + AMessage);
  end;
  Inc(GChecks);
end;

function ReplaceField(const AObject: TNyxDataValue; const AName: TNyxText;
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

procedure Run;
var
  LBase: INyxCanvasDefinition;
  LConnections: INyxCanvasDefinition;
  LLayout: INyxCanvasGraphLayout;
  LSaved: INyxCanvasDefinition;
  LBranch: INyxCanvasDefinition;
  LNodes: TNyxCanvasGraphNodes;
  LOriginal: TNyxText;
  LSource: TNyxText;
  LData: TNyxDataValue;
  LGraphName: TNyxText;
  LFailed: Boolean;
  LDocument: TNyxDocument;
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LFlowBefore: TNyxText;
  LIndex: Integer;
  LLongName: TNyxText;
begin
  LDocument := nil;
  LRead := nil;
  LWorkspace := nil;
  LSession := nil;
  LGraphName := 'Data ' + NyxScalarText($1F680);
  try
    LBase := NewNyxCanvasDefinition.WithCamera(NyxCanvasCamera(800, 600,
      NyxCanvasPoint(25, -50), 1.25)).WithLayer(NyxCanvasLayer('Workspace'))
      .WithItem(NyxCanvasItem('Content'), NyxReusableRoot('content'),
        NyxCanvasPlacement(NyxCanvasLayer('Workspace'), NyxCanvasBounds(0, 0, 100, 60)));
    LOriginal := LBase.ToData.ToJSON;
    Check((LBase.ToData.Field('version').AsInteger = 1) and
      (NyxCanvasDefinitionFromData(LBase.ToData).ToData.ToJSON = LOriginal),
      'legacy version one retains exact bytes and no authored positions');
    Check(Length(NyxCanvasGraphNodes(LBase)) = 0, 'legacy definitions have no explicit layout');
    LConnections := NyxCanvasConnections(LBase)
      .ForControl(NyxCanvasItem('Content'), NyxControl('content'), NewNyxFlowRoutes).Done;
    Check((LConnections.ToData.Field('version').AsInteger = 2) and
      (NyxCanvasDefinitionFromData(LConnections.ToData).ToData.ToJSON = LConnections.ToData.ToJSON),
      'legacy version two route masks retain exact bytes');
    LLayout := NyxCanvasGraphLayout(LConnections);
    LSaved := LLayout.Place(NyxFlowRef(LGraphName), NyxFlowStep('parse'),
      NyxCanvasPoint(-125.5, 64.25)).Place(NyxFlowRef(LGraphName), NyxFlowStep('format'),
      NyxCanvasPoint(320.75, -8.5)).Done;
    Check((LSaved.ToData.Field('version').AsInteger = 3) and
      (LBase.ToData.ToJSON = LOriginal) and (Length(NyxCanvasGraphNodes(LLayout.Done)) = 0),
      'immutable layout branches retain the original geometry and builder');
    Check(Length(NyxCanvasConnectionChanges(LSaved)) = 1, 'layout retains unrelated route masks');
    Check(NyxCanvasDefinitionFromData(LSaved.ToData).ToData.ToJSON = LSaved.ToData.ToJSON,
      'version three round-trips exact Unicode and fractional signed positions');
    LNodes := NyxCanvasGraphNodes(LSaved);
    Check((Length(LNodes) = 2) and (LNodes[0].Graph.Name = LGraphName) and
      (LNodes[0].Position.X = -125.5) and (LNodes[1].Position.Y = -8.5),
      'typed inspection retains exact graph/step/world values');
    LNodes[0].Position := NyxCanvasPoint(999, 999);
    Check(NyxCanvasGraphNodes(LSaved)[0].Position.X = -125.5,
      'caller mutation of a returned array cannot mutate its immutable definition');
    LBranch := NyxCanvasGraphLayout(LSaved).Place(NyxFlowRef(LGraphName),
      NyxFlowStep('parse'), NyxCanvasPoint(-1, -2)).Done;
    Check((Length(NyxCanvasGraphNodes(LBranch)) = 2) and
      (NyxCanvasGraphNodes(LBranch)[1].Step.Name = 'format') and
      (NyxCanvasGraphNodes(LSaved)[0].Position.X = -125.5),
      'replacing a position preserves key order and independent prior branches');
    LBranch := NyxCanvasGraphLayout(LBranch).Automatic(NyxFlowRef(LGraphName), NyxFlowStep('parse'))
      .Automatic(NyxFlowRef(LGraphName), NyxFlowStep('format')).Done;
    Check(LBranch.ToData.ToJSON = LConnections.ToData.ToJSON,
      'removing all explicit layout restores exact legacy connection bytes');
    LBranch := NyxCanvasConnections(LSaved).Inherit(NyxCanvasItem('Content'), NyxControl('content')).Done;
    Check((Length(NyxCanvasGraphNodes(LBranch)) = 2) and
      (Length(NyxCanvasConnectionChanges(LBranch)) = 0), 'connection edits preserve graph-node layout');
    LBranch := LSaved.WithCamera(NyxCanvasCamera(600, 400, NyxCanvasPoint(0, 0), 2));
    Check((NyxCanvasGraphNodes(LBranch)[0].Position.X = -125.5) and
      (LSaved.Camera.Zoom = 1.25), 'geometry branches retain layout and independent camera intent');

    LFailed := False;
    try
      LLayout.Nodes([NyxCanvasGraphNode(NyxFlowRef(LGraphName), NyxFlowStep('parse'), NyxCanvasPoint(1, 2)),
        Default(TNyxCanvasGraphNode)]);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LLayout.Done.ToData.ToJSON = LConnections.ToData.ToJSON),
      'a late invalid group member preserves the complete earlier plan');
    LNodes := NyxCanvasGraphNodes(LSaved);
    LFailed := False;
    try
      LLayout.Nodes([LNodes[0], LNodes[0]]);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'one grouped edit refuses duplicate graph/step keys');
    LFailed := False;
    try
      LLayout.Automatic(NyxFlowRef(LGraphName), NyxFlowStep('missing'));
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'removing an absent explicit position refuses');
    LData := ReplaceField(LSaved.ToData, 'graphNodes',
      NyxArray([LSaved.ToData.Field('graphNodes').Item(0), LSaved.ToData.Field('graphNodes').Item(0)]));
    LFailed := False;
    try
      NyxCanvasDefinitionFromData(LData);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'strict descriptor admission refuses duplicate positions');
    LFailed := False;
    try
      NyxCanvasDefinitionFromData(ReplaceField(LSaved.ToData, 'version', NyxData(2)));
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'legacy version cannot silently admit graph-layout fields');
    SetLength(LNodes, MaximumNyxCanvasGraphNodes + 1);
    LFailed := False;
    try
      LLayout.Nodes(LNodes);
    except
      on E: Exception do
      begin
        LFailed := Pos('1..4096', E.Message) > 0;
      end;
    end;
    Check(LFailed, 'oversized groups refuse at the explicit count budget before reading their entries');
    LLongName := '';
    for LIndex := 1 to 120 do
    begin
      LLongName := LLongName + NyxScalarText($1F680);
    end;
    SetLength(LNodes, 2048);
    for LIndex := 0 to High(LNodes) do
    begin
      LNodes[LIndex] := NyxCanvasGraphNode(NyxFlowRef(LLongName),
        NyxFlowStep(TNyxText(StringOfChar('a', 120)) + TNyxText(IntToStr(LIndex))), NyxCanvasPoint(1, 2));
    end;
    LFailed := False;
    try
      LLayout.Nodes(LNodes);
    except
      on E: Exception do
      begin
        LFailed := Pos('byte budget', E.Message) > 0;
      end;
    end;
    Check(LFailed and (LLayout.Done.ToData.ToJSON = LConnections.ToData.ToJSON),
      'supplementary Unicode counts toward the full UTF-8 byte budget and failed admission preserves the branch');
    LNodes := nil;

    LDocument := TNyxDocument.Create;
    LDocument.AddPage(NewNyxPage('home'));
    LDocument.AddComponent(NewNyxLabel('content').WithText('Content'));
    LDocument.Flows.Define(NyxFlowRef(LGraphName), NewNyxFlowGraph
      .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
      .WithStep(NyxFlowStep('format'), NewNyxJSONFormat)
      .Connect(NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)),
        NyxFlowEndpoint(NyxFlowStep('format'), NyxLogicPort(nlpJSON))).Build);
    LFlowBefore := LDocument.Flows.ToData.ToJSON;
    LDocument.Canvases.Define(NyxCanvasRef('Tools'), LSaved);
    LDocument.Canvases.Define(NyxCanvasRef('Other tools'), NyxCanvasGraphLayout(LSaved)
      .Place(NyxFlowRef(LGraphName), NyxFlowStep('parse'), NyxCanvasPoint(888, -123)).Done);
    LDocument.Validate;
    Check((NyxCanvasGraphNodes(LDocument.Canvases.Definition(NyxCanvasRef('Tools')))[0].Position.X = -125.5) and
      (NyxCanvasGraphNodes(LDocument.Canvases.Definition(NyxCanvasRef('Other tools')))[0].Position.X = 888),
      'independent canvases retain different positions for the same shared graph and step');
    LOriginal := TNyxCodec.Encode(LDocument);
    LRead := TNyxCodec.Decode(LOriginal);
    Check(TNyxCodec.Encode(LRead) = LOriginal, 'complete document codec retains layout with all graph/root meaning');
    FreeAndNil(LRead);
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('NyxCanvasGraphLayout(', LSource) > 0) and (Pos('.Place(NyxFlowRef(', LSource) > 0),
      'generated authoring uses typed fluent graph/step/world constructs');
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LOriginal, 'generated Pascal reader retains the exact complete document');
    Check(LWorkspace.Render(LRead) = LSource, 'unchanged accepted source remains byte-exact');
    FreeAndNil(LRead);
    FreeAndNil(LWorkspace);
    LBefore := NyxProjectPair(LOriginal, LSource);
    LSession := TNyxStudioSession.Create(LBefore);
    LBranch := NyxCanvasGraphLayout(LSaved).Place(NyxFlowRef(LGraphName),
      NyxFlowStep('format'), NyxCanvasPoint(600, 500)).Done;
    LSession.ApplyPatch(NyxCanvasPatch([NyxDefineCanvas(NyxCanvasRef('Tools'), LBranch)]));
    LAfter := LSession.ProjectSnapshot;
    Check((LAfter.Design <> LBefore.Design) and (LAfter.Source <> LBefore.Source) and
      (LSession.Document.Flows.ToData.ToJSON = LFlowBefore),
      'one ordinary typed layout edit updates the pair without changing graph execution declarations');
    Check(NyxCanvasGraphNodes(LSession.Document.Canvases.Definition(NyxCanvasRef('Other tools')))[0].Position.X = 888,
      'editing one canvas leaves its sibling layout exact');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'one Undo restores the exact complete design/Pascal pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'one Redo restores the exact complete saved layout pair');

    LBranch := NyxCanvasGraphLayout(LSaved).Place(NyxFlowRef(LGraphName),
      NyxFlowStep('missing'), NyxCanvasPoint(1, 2)).Done;
    LDocument.Canvases.Define(NyxCanvasRef('Tools'), LBranch);
    LDocument.Validate;
    Check(LDocument.Flows.ToData.ToJSON = LFlowBefore,
      'a dormant position grants no graph or operation authority and does not block topology editing');
    while LDocument.Flows.Count > 0 do
    begin
      LDocument.Flows.Remove(LDocument.Flows.Reference(0));
    end;
    LDocument.Validate;
    LRead := TNyxSourceWorkspace.PrepareDraft(TNyxCodegen.Generate(LDocument), LWorkspace);
    Check((LRead.Flows.Count = 0) and (TNyxCodec.Encode(LRead) = TNyxCodec.Encode(LDocument)),
      'dormant layout survives generated admission even after its complete graph registry is removed');
  finally
    LSession.Free;
    LWorkspace.Free;
    LRead.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-graph-layout', 'passed');
    document.body.setAttribute('data-graph-layout-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable graph-layout contracts');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-graph-layout', 'failed');
      document.body.setAttribute('data-graph-layout-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
