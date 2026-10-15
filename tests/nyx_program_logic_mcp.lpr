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

program nyx_program_logic_mcp;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.state,
  nyx.model, nyx.codec, nyx.controls, nyx.flow.controls.editor, nyx.flow.routes,
  nyx.binding.types, nyx.programs.declarations, nyx.programs.editing,
  nyx.flow.editing, nyx.flow.declarations, nyx.flow.route.model, nyx.root.types,
  nyx.canvas.types, nyx.canvas.editing,
  nyx.studio.projects, nyx.studio.stateedits, nyx.studio.edits,
  nyx.studio.transactions, nyx.studio.workspaces, nyx.studio.sourcejobs,
  nyx.studio.buildjobs,
  nyx.test.mcp.client, nyx.test.programs.logic;

var
  GClient: TNyxMCPTestClient;
  GWorkspace: TNyxWorkspaceRef;
  GRevision: Integer;
  GChecks: Integer;
  GSerial: Integer;
  GPrefix: TNyxText;
  GEvidence: TNyxText;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Semantic program composition: ' + AReason);
  end;
  Inc(GChecks);
end;

function Read(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBytes, LStream.Size);

    if Length(LBytes) > 0 then
    begin
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
  Result := NyxDecodeUTF8(LBytes);
end;

procedure Save(const AName, AValue: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AValue);
  LStream := TFileStream.Create(GEvidence + AName, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
end;

function Operation: TNyxText;
begin
  Inc(GSerial);
  Result := GPrefix + '-' + TNyxText(IntToStr(GSerial));
end;

function Tool(const AName: TNyxText; const AArguments: TNyxDataValue;
  APrimary: Boolean = False): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin

  if APrimary then
  begin
    LReply := GClient.Tool(AName, AArguments);
  end
  else
  begin
    Check(GWorkspace.ID <> '', 'every design request names the owned ordinary project');
    LReply := GClient.Tool(AName, NyxWithWorkspace(AArguments, GWorkspace));
  end;

  if LReply.Field('isError').AsBoolean then
  begin
    Save('refusal-private.json', LReply.ToJSON);
    raise Exception.Create('Owned semantic tool refused ' + AName);
  end;
  Result := LReply.Field('structuredContent');
end;

function ExportPair(APrimary: Boolean; ARevision: Integer): TNyxText;
var
  LValue: TNyxDataValue;
  LOffset: Integer;
begin
  Result := '';
  LOffset := 0;
  repeat
    LValue := Tool('nyx_project', NyxObject([
      NyxField('mode', NyxData('export')), NyxField('expectedRevision', NyxData(ARevision)),
      NyxField('part', NyxData('project')), NyxField('offset', NyxData(LOffset)),
      NyxField('count', NyxData(2048))]), APrimary);
    Result := Result + LValue.Field('text').AsText;
    LOffset := LValue.Field('nextOffset').AsInteger;
  until LOffset = LValue.Field('total').AsInteger;
end;

function Pair: TNyxText;
begin
  Result := ExportPair(False, GRevision);
end;

function Node(const AKind, AID, AParent, AText: TNyxText): TNyxDataValue;
begin

  if AParent = '' then
  begin
    Exit(NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData(AKind)),
      NyxField('id', NyxData(AID)), NyxField('root', NyxData('page'))]));
  end;
  Result := NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData(AKind)),
    NyxField('id', NyxData(AID)), NyxField('parent', NyxData(AParent)),
    NyxField('properties', NyxObject([NyxField('text', NyxData(AText))]))]);
end;

{ One public typed transaction feeds the actual semantic wire. Node dictionaries
  are the explicit MCP creation boundary; program/graph/state/binding/route data
  comes from the same typed recipes and patches used by ordinary Studio. }
function Compose: INyxProjectTransaction;
var
  LFixture: TNyxDocument;
begin
  LFixture := ProgramLogicDocument;
  try
    Result := NyxProjectTransaction([
      NyxStateStep(NyxStateBindingPatch([
        NyxCreateDefault(NyxStateValue(NyxTextState('name'), 'Ada')),
        NyxCreateDefault(NyxStateValue(NyxTextState('caption'), 'Ready for a greeting'))])),
      NyxDesignStep(ReadNyxDesignPatch(NyxArray([
        NyxObject([NyxField('op', NyxData('title')),
          NyxField('value', NyxData('Pascal greeting workshop'))]),
        Node('page', 'home', '', ''),
        Node('heading', 'title', 'home', 'Make a personal greeting'),
        Node('input', 'name-input', 'home', 'Your name'),
        Node('button', 'greet-button', 'home', 'Greet'),
        Node('label', 'caption-label', 'home', ''),
        Node('page', 'other', '', ''),
        Node('heading', 'other-title', 'other', 'A fresh page generation'),
        NyxDefineProgram(NyxProgramRef('Greeting'),
          LFixture.Programs.Definition(NyxProgramRef('Greeting'))).ToData,
        NyxDefineFlow(NyxFlowRef('Greeting flow'),
          LFixture.Flows.Definition(NyxFlowRef('Greeting flow'))).ToData,
        NyxDesignChanges(NyxFlowRoutesPatch(NyxControl('greet-button'),
          NyxControlFlowRoutes(LFixture.Find('greet-button')))).ToData.Item(0)]))),
      NyxStateStep(NyxStateBindingPatch([
        NyxBindControl(NyxBindingOwner('name-input'), bpValue, NyxTextState('name'), bdTwoWay),
        NyxBindControl(NyxBindingOwner('caption-label'), bpText, NyxTextState('caption'), bdFromState)]))]);
  finally
    LFixture.Free;
  end;
end;

{ The same controls move into a reusable item, retaining their exact bindings
  and saved event routes. This is one ordinary semantic ownership transaction. }
function ComposeCanvas: INyxProjectTransaction;
var
  LFixture: TNyxDocument;
begin
  LFixture := ProgramToolCanvasDocument;
  try
    Result := NyxProjectTransaction([NyxDesignStep(ReadNyxDesignPatch(NyxArray([
      NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData('card')),
        NyxField('root', NyxData('component')), NyxField('id', NyxData('greeting-template'))]),
      NyxObject([NyxField('op', NyxData('move')), NyxField('id', NyxData('name-input')),
        NyxField('parent', NyxData('greeting-template')), NyxField('index', NyxData(0))]),
      NyxObject([NyxField('op', NyxData('move')), NyxField('id', NyxData('greet-button')),
        NyxField('parent', NyxData('greeting-template')), NyxField('index', NyxData(1))]),
      NyxObject([NyxField('op', NyxData('move')), NyxField('id', NyxData('caption-label')),
        NyxField('parent', NyxData('greeting-template')), NyxField('index', NyxData(2))]),
      Node('tool-canvas', 'program-tools', 'home', ''),
      NyxDefineCanvas(NyxCanvasRef('Greeting canvas'), LFixture.Canvases.Definition(
        NyxCanvasRef('Greeting canvas'))).ToData,
      NyxAttachCanvas(NyxControl(LFixture.Find('program-tools').Part(NyxPart('workspace')).ID),
        NyxCanvasRef('Greeting canvas')).ToData])))]);
  finally
    LFixture.Free;
  end;
end;

procedure Build(const ATarget, ASource, AStage: TNyxText);
var
  LOutput: TNyxDataValue;
  LReply: TNyxDataValue;
  LStatus: TNyxDataValue;
  LStarted: QWord;
begin
  LOutput := Tool('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))]));
  LReply := Tool('nyx_build', NyxObject([
    NyxField('mode', NyxData('request')), NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(Operation)), NyxField('outputID', LOutput.Field('outputID')),
    NyxField('target', NyxData(ATarget)), NyxField('scope', NyxData('application'))]));
  LStarted := GetTickCount64;
  repeat
    LStatus := Tool('nyx_build', NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', LReply.Field('job')), NyxField('severity', NyxData('error')),
      NyxField('limit', NyxData(3))]));

    if (LStatus.Field('state').AsText = 'succeeded') or
      (LStatus.Field('state').AsText = 'failed') or
      (LStatus.Field('state').AsText = 'cancelled') then
    begin
      Break;
    end;
    Sleep(50);
  until GetTickCount64 - LStarted > 180000;
  Save(AStage + PathDelim + 'build-' + ATarget + '.json', LStatus.ToJSON);
  Check((LStatus.Field('state').AsText = 'succeeded') and
    LStatus.Field('currentSource').AsBoolean and
    (LStatus.Field('sourceFingerprint').AsText = NyxBuildFingerprint(ASource)),
    'actual ' + ATarget + ' application compiler owns the exact MCP-authored source');
end;

{ Compose with the same public Nyx route form and patch as ordinary Studio.
  Only the existing authenticated transaction wire publishes the grouped pair.
  This explicit mode operates solely in the marked isolated owned workspace. }
procedure ControlRoutes;
var
  LDocument: TNyxDocument;
  LForm: INyxControlRouteEditor;
  LChange: TNyxControlRouteEditorChange;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LPair: TNyxProjectPair;
  LArguments: TNyxDataValue;
  LResult: TNyxDataValue;
  LQuery: TNyxDataValue;
  LOldRevision: Integer;

  function Field(const AName: TNyxText): TNyxNode;
  begin
    Result := LForm.Node.Find('semantic-control-routes-' + AName);
  end;

  procedure Choose(const AName, ACaption: TNyxText);
  var
    LLabels: TNyxStrings;
    LIndex: Integer;
  begin
    LLabels := TNyxStrings.Create;
    try
      LLabels.Text := Field(AName).Prop('items');
      for LIndex := 0 to LLabels.Count - 1 do
      begin

        if Copy(LLabels[LIndex], Length(LLabels[LIndex]) - Length(ACaption) + 1,
          Length(ACaption)) = ACaption then
        begin
          Field(AName).Configure.Value(LLabels[LIndex]).Done;
          Exit;
        end;
      end;
    finally
      LLabels.Free;
    end;
    raise Exception.Create('Missing public form choice: ' + AName);
  end;

begin
  LOldRevision := GRevision;
  LBefore := Pair;
  LPair := DecodeNyxProject(LBefore);
  LDocument := TNyxCodec.Decode(LPair.Design);
  try
    LForm := NewNyxControlRouteEditor('semantic-control-routes', LDocument,
      NyxControl('name-input'), GRevision);
    Field('name').Configure.Value('Live greeting').Done;
    Choose('event', 'OnChange');
    Choose('input', 'greeting / Name / Text');
    Choose('output', 'greeting / Caption / Text');
    Choose('destination', 'caption-label / Text');
    Check(HandleNyxControlRouteEditor(Field('map-input'), LForm.Node, LDocument, GRevision),
      'semantic consumer stages an owned event-value input through the public form');
    Check(HandleNyxControlRouteEditor(Field('map-output'), LForm.Node, LDocument, GRevision),
      'semantic consumer stages an actual direct control binding');
    Check(CaptureNyxControlRouteEditor(Field('apply'), LForm.Node, LDocument, GRevision, LChange),
      'the complete public proposal admits against the revision-pinned document');
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)), NyxField('operations', NyxProjectTransaction([
        NyxDesignStep(NyxFlowRoutesPatch(NyxControl('greet-button'), NewNyxFlowRoutes)),
        NyxDesignStep(NyxFlowRoutesPatch(LChange.Owner,
          NyxFlowRoutesFromData(LChange.RoutesData)))]).ToData)]);
  finally
    LForm := nil;
    LDocument.Free;
  end;
  LResult := Tool('nyx_transaction', LArguments);
  GRevision := LResult.Field('revision').AsInteger;
  Check(Tool('nyx_transaction', LArguments).ToJSON = LResult.ToJSON,
    'an exact grouped control-route retry returns its original receipt');
  LAfter := Pair;
  LQuery := Tool('nyx_logic', NyxObject([NyxField('scope', NyxData('routes')),
    NyxField('id', NyxData('name-input')), NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('limit', NyxData(1))]));
  Check((LQuery.Field('total').AsInteger = 1) and (LQuery.Field('items').Count = 1) and
    (LQuery.Field('items').Item(0).Field('route').AsText = 'Live greeting'),
    'one bounded semantic response identifies the exact authored control route');
  LQuery := GClient.Tool('nyx_transaction', NyxWithWorkspace(NyxObject([
    NyxField('expectedRevision', NyxData(LOldRevision)), NyxField('operationId', NyxData(Operation)),
    NyxField('operations', NyxProjectTransaction([
      NyxDesignStep(NyxFlowRoutesPatch(NyxControl('name-input'), NewNyxFlowRoutes))]).ToData)]), GWorkspace));
  Check(LQuery.Field('isError').AsBoolean and (Pair = LAfter),
    'a stale authenticated route mutation preserves the exact accepted pair');
  LResult := Tool('nyx_history', NyxObject([NyxField('direction', NyxData('undo')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation))]));
  GRevision := LResult.Field('revision').AsInteger;
  Check(Pair = LBefore, 'one semantic Undo restores both grouped route owners and exact source');
  LResult := Tool('nyx_history', NyxObject([NyxField('direction', NyxData('redo')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation))]));
  GRevision := LResult.Field('revision').AsInteger;
  Check(Pair = LAfter, 'one semantic Redo restores the entire control/script/control pair');
  LPair := DecodeNyxProject(LAfter);
  ForceDirectories(GEvidence + 'control-routes');
  Save('control-routes' + PathDelim + 'nyx.generated.view.pas', LPair.Source);
  Save('control-routes' + PathDelim + 'design.nyx', LPair.Design);
  Save('control-routes' + PathDelim + 'accepted.nyxpair', LAfter);
  Build('browser', LPair.Source, 'control-routes');
  Build('lcl', LPair.Source, 'control-routes');
end;

var
  LMarker: TNyxDataValue;
  LParent: TNyxDataValue;
  LResult: TNyxDataValue;
  LArguments: TNyxDataValue;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LProject: TNyxProjectPair;
  LTransaction: INyxProjectTransaction;
  LID: TGUID;
  LRuntime: TNyxText;
  LExpectedEndpoint: TNyxText;
  LPort: Integer;
  LPrimaryPair: TNyxText;
  LField: Integer;
begin
  GClient := nil;
  try
    Check((ParamCount = 4) or ((ParamCount = 5) and (ParamStr(5) = '--control-routes')),
      'supply marked origin, config, fresh evidence, exact runtime marker and optional --control-routes');
    Check(Pos('http://127.0.0.1:', ParamStr(1)) = 1, 'this consumer never targets LAN or a remote editor');
    LPort := StrToInt(Copy(ParamStr(1), Length('http://127.0.0.1:') + 1, MaxInt));
    LExpectedEndpoint := 'http://127.0.0.1:' + TNyxText(IntToStr(LPort + 1)) + '/mcp/';
    LRuntime := ExtractFileDir(ExtractFileDir(ExpandFileName(ParamStr(2))));
    Check(ExpandFileName(ParamStr(4)) = IncludeTrailingPathDelimiter(LRuntime) +
      '.local' + PathDelim + 'runtime-qualification.json',
      'runtime marker is tied to the explicit enrollment configuration');
    LMarker := TNyxDataValue.ParseJSON(Read(ParamStr(4)));
    Check((LMarker.Field('origin').AsText = TNyxText(ParamStr(1))) and
      (LMarker.Field('service').AsText = 'nyx-resource-runtime-qualification'),
      'explicit marker admits only this owned qualification host');
    GEvidence := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));
    Check(not DirectoryExists(GEvidence), 'semantic evidence directory must be new');
    ForceDirectories(GEvidence);
    CreateGUID(LID);
    GPrefix := TNyxText(GUIDToString(LID));
    GClient := TNyxMCPTestClient.Create(ParamStr(2), 'Scooty program runtime composition');
    Check(Pos(LExpectedEndpoint, GClient.Endpoint) = 1,
      'authenticated endpoint belongs to the marked isolated HTTP/MCP pair');
    LParent := Tool('nyx_session', NyxObject([]), True);
    LPrimaryPair := ExportPair(True, LParent.Field('revision').AsInteger);
    Save('primary-before-private.json', LParent.ToJSON);
    LResult := Tool('nyx_workspaces', NyxObject([
      NyxField('mode', NyxData('create')), NyxField('base', NyxData('empty')),
      NyxField('label', NyxData('Program runtime workshop')),
      NyxField('expectedRevision', LParent.Field('revision')),
      NyxField('operationId', NyxData(Operation))]), True);
    GWorkspace := NyxWorkspace(LResult.Field('workspace').AsText);
    GRevision := Tool('nyx_session', NyxObject([])).Field('revision').AsInteger;
    LBefore := Pair;
    LTransaction := Compose;
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)), NyxField('operations', LTransaction.ToData)]);
    LResult := Tool('nyx_transaction', LArguments);
    GRevision := LResult.Field('revision').AsInteger;
    Check(Tool('nyx_transaction', LArguments).ToJSON = LResult.ToJSON,
      'completed grouped composition retry retains its exact receipt');
    LAfter := Pair;
    LProject := DecodeNyxProject(LAfter);
    Check((Pos('NewNyxProgramLogic(', LProject.Source) > 0) and
      (Pos('INyxInput', LProject.Source) > 0), 'semantic composition emits crafted specialized Pascal');
    LResult := Tool('nyx_logic', NyxObject([NyxField('scope', NyxData('catalog')),
      NyxField('query', NyxData('pascal')), NyxField('limit', NyxData(1)),
      NyxField('expectedRevision', NyxData(GRevision))]));
    Check((LResult.Field('total').AsInteger = 1) and (LResult.Field('items').Count = 1),
      'authenticated bounded discovery exposes the document program template');
    LResult := Tool('nyx_history', NyxObject([NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation))]));
    GRevision := LResult.Field('revision').AsInteger;
    Check(Pair = LBefore, 'one semantic Undo restores the exact pre-composition pair');
    LResult := Tool('nyx_history', NyxObject([NyxField('direction', NyxData('redo')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation))]));
    GRevision := LResult.Field('revision').AsInteger;
    Check(Pair = LAfter, 'one semantic Redo restores the complete routed-program pair');
    ForceDirectories(GEvidence + 'plain');
    Save('plain' + PathDelim + 'nyx.generated.view.pas', LProject.Source);
    Save('plain' + PathDelim + 'design.nyx', LProject.Design);
    Save('plain' + PathDelim + 'accepted.nyxpair', LAfter);
    Build('browser', LProject.Source, 'plain');
    Build('lcl', LProject.Source, 'plain');
    LBefore := LAfter;
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)), NyxField('operations', ComposeCanvas.ToData)]);
    LResult := Tool('nyx_transaction', LArguments);
    GRevision := LResult.Field('revision').AsInteger;
    Check(Tool('nyx_transaction', LArguments).ToJSON = LResult.ToJSON,
      'exact reusable/canvas composition retry retains one grouped result');
    LAfter := Pair;
    LResult := Tool('nyx_history', NyxObject([NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation))]));
    GRevision := LResult.Field('revision').AsInteger;
    Check(Pair = LBefore, 'one Undo restores exact ordinary control ownership and source');
    LResult := Tool('nyx_history', NyxObject([NyxField('direction', NyxData('redo')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation))]));
    GRevision := LResult.Field('revision').AsInteger;
    Check(Pair = LAfter, 'one Redo restores exact embedded ownership, programs and source');
    LProject := DecodeNyxProject(LAfter);
    ForceDirectories(GEvidence + 'canvas');
    Save('canvas' + PathDelim + 'nyx.generated.view.pas', LProject.Source);
    Save('canvas' + PathDelim + 'design.nyx', LProject.Design);
    Save('canvas' + PathDelim + 'accepted.nyxpair', LAfter);
    Save('workspace-private.json', NyxObject([NyxField('workspace', NyxData(GWorkspace.ID)),
      NyxField('revision', NyxData(GRevision))]).ToJSON);
    Build('browser', LProject.Source, 'canvas');
    Build('lcl', LProject.Source, 'canvas');

    if ParamCount = 5 then
    begin
      ControlRoutes;
    end;
    LResult := Tool('nyx_session', NyxObject([]), True);
    Save('primary-after-private.json', LResult.ToJSON);
    Check(ExportPair(True, LParent.Field('revision').AsInteger) = LPrimaryPair,
      'isolated primary retains its exact complete accepted/draft pair');
    for LField := 0 to LParent.Count - 1 do
    begin

      if LParent.Key(LField) <> 'activitySequence' then
      begin
        Check(LResult.Field(LParent.Key(LField)).ToJSON =
          LParent.Field(LParent.Key(LField)).ToJSON,
          'primary navigation, revision, history/draft facts remain exact');
      end;
    end;
    WriteLn('PASS ', GChecks, ' authenticated semantic program composition checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;

  if GClient <> nil then
  begin
    GClient.Close;
  end;
  GClient.Free;
end.
