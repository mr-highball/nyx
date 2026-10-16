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

program nyx_canvas_graph_semantics_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec,
  nyx.codegen, nyx.source, nyx.canvas.types, nyx.canvas.geometry,
  nyx.canvas.declarations, nyx.canvas.editing, nyx.flow.types, nyx.flow.declarations,
  nyx.studio.projects, nyx.studio.agents, nyx.test.canvas.graph.layout
  {$ifdef PAS2JS}, Web{$else}, Classes, nyx.studio.mcp{$endif};

var
  GChecks: Integer;
  GAgent: TNyxAgentSession;
  GCanvasID: TNyxText;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Semantic graph layout: ' + AReason);
  end;
  Inc(GChecks);
end;

function Has(const AValue: TNyxDataValue; const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if AValue.Kind = ndObject then
  begin
    for LIndex := 0 to AValue.Count - 1 do
    begin

      if AValue.Key(LIndex) = AName then
      begin
        Exit(True);
      end;
    end;
  end;
end;

function Pair: TNyxProjectPair;
begin
  Result := GAgent.PreviewPair(GAgent.Revision, 'home');
end;

function Position(const AStep: TNyxText; AX, AY: Double): TNyxDataValue;
begin
  Result := NyxPositionCanvasGraphNode(NyxCanvasRef('Layout board'),
    NyxFlowRef(GraphLayoutFlowName), NyxFlowStep(AStep), NyxCanvasPoint(AX, AY)).ToData;
end;

function Automatic(const AGraph, AStep: TNyxText): TNyxDataValue;
begin
  Result := NyxAutomaticCanvasGraphNode(NyxCanvasRef('Layout board'),
    NyxFlowRef(AGraph), NyxFlowStep(AStep)).ToData;
end;

function Request(const AID: TNyxText; const AOperations: TNyxDataValue;
  ARevision: Integer): TNyxDataValue;
begin
  Result := NyxObject([NyxField('expectedRevision', NyxData(ARevision)),
    NyxField('operationId', NyxData(AID)), NyxField('operations', AOperations)]);
end;

function Query(AOffset, ALimit: Integer; const AGraph, AStep: TNyxText): TNyxDataValue;
begin
  Result := GAgent.Call('nyx_node', 'Scooty layout qualification', NyxObject([
    NyxField('id', NyxData(GCanvasID)), NyxField('expectedRevision', NyxData(GAgent.Revision)),
    NyxField('canvas', NyxData(True)), NyxField('canvasGraphNodes', NyxData(True)),
    NyxField('canvasGraphNodeOffset', NyxData(AOffset)), NyxField('canvasGraphNodeLimit', NyxData(ALimit)),
    NyxField('canvasGraphNodeGraph', NyxData(AGraph)), NyxField('canvasGraphNodeStep', NyxData(AStep))]));
end;

procedure Refuse(const ATool: TNyxText; const AArguments: TNyxDataValue);
var
  LBefore: TNyxText;
  LRevision: Integer;
  LFailed: Boolean;
begin
  LBefore := EncodeNyxProject(Pair);
  LRevision := GAgent.Revision;
  LFailed := False;
  try
    GAgent.Call(ATool, 'Scooty layout qualification', AArguments);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GAgent.Revision = LRevision) and
    (EncodeNyxProject(Pair) = LBefore), 'refused operation preserves complete pair/draft/base and revision');
end;

{$ifndef PAS2JS}
{ Export exact accepted constructor companions only to the caller's fresh owned
  evidence root. Native text is already UTF-8; no ANSI string collection mediates
  these bytes. Independent compilation remains separate from source admission. }
procedure ExportPair(const AStage: TNyxText; const APair: TNyxProjectPair);
var
  LDirectory: TNyxText;

  procedure Save(const AName, AValue: TNyxText);
  var
    LFile: TFileStream;
  begin
    LFile := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if AValue <> '' then
      begin
        LFile.WriteBuffer(AValue[1], Length(AValue));
      end;
    finally
      LFile.Free;
    end;
  end;

begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + AStage + PathDelim;
  ForceDirectories(LDirectory);
  Save('nyx.generated.view.pas', APair.Source);
  Save('design.nyx', APair.Design);
end;

{ Walk finite schema data rather than assuming inventory array offsets or oneOf
  nesting. No server, socket, transport credential or listener is constructed. }
function OperationSchema(const AValue: TNyxDataValue; const AOperation: TNyxText): TNyxDataValue;
var
  LProperties: TNyxDataValue;
  LEnums: TNyxDataValue;
  LChild: TNyxDataValue;
  LIndex: Integer;
begin
  Result := NyxNull;

  if Has(AValue, 'properties') then
  begin
    LProperties := AValue.Field('properties');

    if Has(LProperties, 'op') and Has(LProperties.Field('op'), 'enum') then
    begin
      LEnums := LProperties.Field('op').Field('enum');
      for LIndex := 0 to LEnums.Count - 1 do
      begin

        if LEnums.Item(LIndex).AsText = AOperation then
        begin
          Exit(AValue);
        end;
      end;
    end;
  end;

  if AValue.Kind in [ndObject, ndArray] then
  begin
    for LIndex := 0 to AValue.Count - 1 do
    begin

      if AValue.Kind = ndObject then
      begin
        LChild := AValue.Field(AValue.Key(LIndex));
      end
      else
      begin
        LChild := AValue.Item(LIndex);
      end;
      Result := OperationSchema(LChild, AOperation);

      if Result.Kind <> ndNull then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure SchemaChecks;
var
  LTools: TNyxDataValue;
  LSchema: TNyxDataValue;
  LVersions: TNyxDataValue;
  LIndex: Integer;
  LFound: Boolean;
begin
  LTools := NyxStudioMCPTools.Field('tools');
  Check(LTools.Count = 24, 'current inventory adds capabilities to existing tools only');
  LFound := False;
  for LIndex := 0 to LTools.Count - 1 do
  begin

    if LTools.Item(LIndex).Field('name').AsText = 'nyx_node' then
    begin
      LSchema := LTools.Item(LIndex).Field('inputSchema').Field('properties');
      Check(LSchema.Field('canvasGraphNodeLimit').Field('maximum').AsInteger = 50,
        'discoverable node query advertises its bounded window');
      Check(Has(LSchema, 'expectedRevision'), 'node query advertises optional revision pinning');
      LFound := True;
    end;
  end;
  Check(LFound, 'existing node tool remains discoverable');
  LSchema := OperationSchema(LTools, 'canvas-graph-position');
  Check(LSchema.Field('required').Count = 6, 'placement schema has exact native numeric coordinate fields');
  LSchema := OperationSchema(LTools, 'canvas-graph-automatic');
  Check(LSchema.Field('required').Count = 4, 'automatic reset schema retains exact typed identity');
  LSchema := OperationSchema(LTools, 'canvas-define');
  LVersions := LSchema.Field('properties').Field('definition').Field('oneOf');
  Check((LVersions.Count = 3) and
    (LVersions.Item(2).Field('properties').Field('version').Field('const').AsInteger = 3),
    'existing full-definition schema includes saved graph layout version three');
  Check(LVersions.Item(2).Field('properties').Field('graphNodes').Field('maxItems').AsInteger =
    MaximumNyxCanvasGraphNodes, 'version-three schema retains the public saved-layout bound');
end;
{$endif}

procedure Run;
var
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LClear: TNyxProjectPair;
  LPending: TNyxProjectPair;
  LOther: TNyxText;
  LConnections: TNyxText;
  LFlows: TNyxText;
  LRequest: TNyxDataValue;
  LReply: TNyxDataValue;
  LQuery: TNyxDataValue;
  LOldWindow: TNyxDataValue;
  LChange: TNyxCanvasChange;
  LRevision: Integer;
  LSavedAgent: TNyxAgentSession;
  LMany: array of TNyxDataValue;
  LIndex: Integer;
begin
  LDocument := GraphLayoutDocument;
  LWorkspace := nil;
  LRead := nil;
  LDecoded := nil;
  try
    GCanvasID := LDocument.Find('layout-tools').Part('workspace').ID;
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LOther := LDocument.Canvases.Definition(NyxCanvasRef('Other board')).ToData.ToJSON;
    LConnections := LDocument.Canvases.Definition(NyxCanvasRef('Layout board')).ToData.Field('connections').ToJSON;
    LFlows := LDocument.Flows.ToData.ToJSON;
    GAgent := TNyxAgentSession.Create(LBefore);
    LRevision := GAgent.Revision;
    LQuery := GAgent.Call('nyx_node', 'Scooty layout qualification', NyxObject([
      NyxField('id', NyxData(GCanvasID)), NyxField('canvas', NyxData(True))]));
    Check((LQuery.Field('canvas').Field('graphNodeTotal').AsInteger = 2) and
      not Has(LQuery.Field('canvas'), 'graphNodes'), 'default context returns a count rather than all saved positions');
    LQuery := Query(0, 1, '', '');
    LOldWindow := LQuery;
    Check((LQuery.Field('canvas').Field('graphNodes').Field('items').Count = 1) and
      (LQuery.Field('canvas').Field('graphNodes').Field('total').AsInteger = 2),
      'one-row semantic query retains the whole filtered count');
    Check(LQuery.Field('canvas').Field('graphNodes').Field('items').Item(0).Field('step').AsText =
      GraphLayoutStepName, 'supplementary Unicode identities survive bounded semantic query');
    LQuery := Query(1, 1, '', '');
    Check(LQuery.Field('canvas').Field('graphNodes').Field('items').Item(0).Field('graph').AsText =
      'Future graph', 'paging includes dormant saved intent without inventing runtime nodes');
    LQuery := Query(0, 1, 'Future graph', 'Arrange');
    Check((LQuery.Field('canvas').Field('graphNodes').Field('total').AsInteger = 1) and
      LQuery.Field('canvas').Field('graphNodes').Field('savedOnly').AsBoolean and
      LQuery.Field('canvas').Field('graphNodes').Field('explicitOnly').AsBoolean,
      'exact filters and provenance distinguish saved layout from runtime geometry');
    Check(EncodeNyxProject(Pair) = EncodeNyxProject(LBefore), 'paged inspection authors no pair/history changes');

    LChange := TNyxCanvasChange.FromData(Position(GraphLayoutStepName, -456.125, 912.5));
    Check(LChange.ToData.ToJSON = Position(GraphLayoutStepName, -456.125, 912.5).ToJSON,
      'typed placement survives strict wire round trip with exact fractions');
    LRequest := Request('arrange', NyxArray([LChange.ToData, Position('Format', 300.75, -99),
      Automatic('Future graph', 'Arrange')]), LRevision);
    LReply := GAgent.Call('nyx_transaction', 'Scooty layout qualification', LRequest);
    LAfter := Pair;
    Check(GAgent.Revision = LRevision + 1, 'related placement and automatic reset publish once');
    Check(GAgent.Call('nyx_transaction', 'Scooty layout qualification', LRequest).ToJSON = LReply.ToJSON,
      'the exact old-revision retry returns its original mutation receipt');
    Refuse('nyx_transaction', Request('arrange', NyxArray([Position('Format', 1, 2)]), LRevision));
    LDecoded := TNyxCodec.Decode(LAfter.Design);
    Check(LDecoded.Canvases.Definition(NyxCanvasRef('Other board')).ToData.ToJSON = LOther,
      'another canvas sharing the graph retains every exact saved field');
    Check((LDecoded.Flows.ToData.ToJSON = LFlows) and
      (LDecoded.Canvases.Definition(NyxCanvasRef('Layout board')).ToData.Field('connections').ToJSON = LConnections),
      'layout leaves execution and per-control masks exact');
    LRead := TNyxSourceWorkspace.PrepareDraft(LAfter.Source, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LAfter.Design, 'accepted Pascal reconstructs the complete admitted design');
    Check(Pos('NyxCanvasGraphLayout(', LAfter.Source) > 0, 'source uses typed fluent layout construction');
    Check(LOldWindow.Field('canvas').Field('graphNodes').Field('items').Item(0).Field('x').AsNumber = 64.25,
      'a retained query result is independent of later edits');
    Refuse('nyx_transaction', Request('stale', NyxArray([Position('Format', 1, 2)]), LRevision));
    Refuse('nyx_node', NyxObject([NyxField('id', NyxData(GCanvasID)),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('canvas', NyxData(True)),
      NyxField('canvasGraphNodes', NyxData(True))]));
    Refuse('nyx_transaction', Request('atomic-refusal', NyxArray([Position('Format', 1, 2),
      Automatic(GraphLayoutFlowName, 'Missing')]), GAgent.Revision));
    Refuse('nyx_transaction', Request('wrong-number', NyxArray([NyxObject([
      NyxField('op', NyxData('canvas-graph-position')), NyxField('scene', NyxData('Layout board')),
      NyxField('graph', NyxData(GraphLayoutFlowName)), NyxField('step', NyxData('Format')),
      NyxField('x', NyxData('12')), NyxField('y', NyxData(2))])]), GAgent.Revision));
    Refuse('nyx_node', NyxObject([NyxField('id', NyxData(GCanvasID)), NyxField('canvas', NyxData(True)),
      NyxField('canvasGraphNodes', NyxData(True)), NyxField('canvasGraphNodeLimit', NyxData(51))]));
    Refuse('nyx_transaction', Request('missing-scene', NyxArray([Position('Format', 1, 2),
      NyxPositionCanvasGraphNode(NyxCanvasRef('Missing board'), NyxFlowRef(GraphLayoutFlowName),
        NyxFlowStep('Format'), NyxCanvasPoint(3, 4)).ToData]), GAgent.Revision));
    Refuse('nyx_transaction', Request('unknown-field', NyxArray([NyxObject([
      NyxField('op', NyxData('canvas-graph-automatic')), NyxField('scene', NyxData('Layout board')),
      NyxField('graph', NyxData(GraphLayoutFlowName)), NyxField('step', NyxData('Format')),
      NyxField('runtime', NyxData(True))])]), GAgent.Revision));
    Refuse('nyx_transaction', Request('coordinate-budget', NyxArray([NyxObject([
      NyxField('op', NyxData('canvas-graph-position')), NyxField('scene', NyxData('Layout board')),
      NyxField('graph', NyxData(GraphLayoutFlowName)), NyxField('step', NyxData('Format')),
      NyxField('x', NyxData(MaximumNyxCanvasCoordinate * 2)), NyxField('y', NyxData(2))])]), GAgent.Revision));
    SetLength(LMany, 65);
    for LIndex := 0 to High(LMany) do
    begin
      LMany[LIndex] := Position('Format', LIndex, LIndex);
    end;
    Refuse('nyx_transaction', Request('group-budget', NyxArray(LMany), GAgent.Revision));
    LQuery := Query(100000, 1, '', '');
    Check((LQuery.Field('canvas').Field('graphNodes').Field('items').Count = 0) and
      (LQuery.Field('canvas').Field('graphNodes').Field('total').AsInteger = 2),
      'a distant legal offset returns an empty bounded page and the exact total');
    LQuery := Query(0, 1, 'Missing graph', '');
    Check(LQuery.Field('canvas').Field('graphNodes').Field('total').AsInteger = 0,
      'an absent exact graph filter never falls back to other names');
    GAgent.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('readOnly'))]));
    LQuery := Query(0, 1, '', '');
    Check(LQuery.Field('canvas').Field('graphNodes').Field('items').Count = 1,
      'read-only agent policy permits bounded saved-layout inspection');
    Refuse('nyx_transaction', Request('read-only', NyxArray([Position('Format', 1, 2)]), GAgent.Revision));
    GAgent.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('edit'))]));
    GAgent.Call('nyx_history', 'Scooty layout qualification', NyxObject([
      NyxField('expectedRevision', NyxData(GAgent.Revision)), NyxField('operationId', NyxData('undo')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(Pair) = EncodeNyxProject(LBefore), 'one semantic Undo restores the exact complete original pair');
    GAgent.Call('nyx_history', 'Scooty layout qualification', NyxObject([
      NyxField('expectedRevision', NyxData(GAgent.Revision)), NyxField('operationId', NyxData('redo')),
      NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(Pair) = EncodeNyxProject(LAfter), 'one semantic Redo restores the exact arranged pair');
    GAgent.Call('nyx_transaction', 'Scooty layout qualification', Request('clear', NyxArray([
      Automatic(GraphLayoutFlowName, GraphLayoutStepName), Automatic(GraphLayoutFlowName, 'Format')]), GAgent.Revision));
    LClear := Pair;
    LQuery := Query(0, 1, '', '');
    Check((LQuery.Field('canvas').Field('graphNodeTotal').AsInteger = 0) and
      (LQuery.Field('canvas').Field('graphNodes').Field('items').Count = 0),
      'removing all explicit positions restores an empty bounded layout window');
    GAgent.Call('nyx_transaction', 'Scooty layout qualification', Request('ordered', NyxArray([
      Position(GraphLayoutStepName, 1, 1), Position(GraphLayoutStepName, 2, 2)]), GAgent.Revision));
    LQuery := Query(0, 1, GraphLayoutFlowName, GraphLayoutStepName);
    Check(LQuery.Field('canvas').Field('graphNodes').Field('items').Item(0).Field('x').AsNumber = 2,
      'one grouped transaction retains chronological same-key edits');
    LPending := Pair;
    LPending.Pending := True;
    LPending.DraftBase := LPending.Source;
    LPending.Draft := LPending.Source + #10 + '// unfinished layout code';
    LSavedAgent := GAgent;
    GAgent := TNyxAgentSession.Create(LPending);
    try
      Refuse('nyx_transaction', Request('pending', NyxArray([Position('Format', 1, 2)]), GAgent.Revision));
    finally
      GAgent.Free;
      GAgent := LSavedAgent;
    end;
    {$ifndef PAS2JS}
    SchemaChecks;
    ExportPair('before', LBefore);
    ExportPair('arranged', LAfter);
    ExportPair('cleared', LClear);
    {$endif}
  finally
    FreeAndNil(GAgent);
    LWorkspace.Free;
    LRead.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-graph-semantics', 'passed');
    document.body.setAttribute('data-graph-semantics-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' production semantic graph layout checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-graph-semantics', 'failed');
      document.body.setAttribute('data-graph-semantics-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.

