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

program nyx_canvas_saved_routes_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec, nyx.state, nyx.scheduler,
  nyx.codegen, nyx.source,
  nyx.flow.types, nyx.flow.routes, nyx.flow.route.model, nyx.flow.declarations,
  nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.declarations,
  nyx.canvas.route.types, nyx.canvas.routes, nyx.canvas.workspace,
  nyx.canvas.editing, nyx.root.types, nyx.test.programs.logic,
  nyx.studio.projects, nyx.studio.session, nyx.studio.edits, nyx.studio.agents
  {$ifdef PAS2JS}, Web{$else}, Classes, nyx.studio.mcp{$endif};

type
  { Qualification creator retires the caller's builder/route aliases and changes
    its reference variables during serialization. It owns no document or target;
    the public capture must already retain its receiver, input and exact names. }
  TRetiringRoutes = class(TInterfacedObject, INyxFlowRoutes)
  public
    destructor Destroy; override;
    function GetCount: Integer;
    function Route(AIndex: Integer): TNyxFlowRoute;
    function WithRoute(const ARoute: TNyxFlowRoute): INyxFlowRoutes;
    function WithoutRoute(const AID: TNyxFlowRouteRef): INyxFlowRoutes;
    function ToData: TNyxDataValue;
  end;

var
  GChecks: Integer;
  GBuilder: INyxCanvasConnections;
  GRoutes: INyxFlowRoutes;
  GItem: TNyxCanvasItemRef;
  GControl: TNyxControlRef;
  GDisposed: Integer;

destructor TRetiringRoutes.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

function TRetiringRoutes.GetCount: Integer;
begin
  Result := 0;
end;

function TRetiringRoutes.Route(AIndex: Integer): TNyxFlowRoute;
begin
  Result := NewNyxFlowRoutes.Route(AIndex);
end;

function TRetiringRoutes.WithRoute(const ARoute: TNyxFlowRoute): INyxFlowRoutes;
begin
  Result := NewNyxFlowRoutes.WithRoute(ARoute);
end;

function TRetiringRoutes.WithoutRoute(const AID: TNyxFlowRouteRef): INyxFlowRoutes;
begin
  Result := NewNyxFlowRoutes.WithoutRoute(AID);
end;

function TRetiringRoutes.ToData: TNyxDataValue;
begin
  GRoutes := nil;
  GBuilder := nil;
  GItem := NyxCanvasItem('Changed caller');
  GControl := NyxControl('changed-caller');
  Result := NewNyxFlowRoutes.ToData;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Saved canvas routes: ' + AReason);
  end;
  Inc(GChecks);
end;

function GreetingRoutes: INyxFlowRoutes;
begin
  Result := NewNyxFlowRoutes.WithRoute(
    NyxFlowRoute(NyxFlowRouteID('Live greeting'), NyxFlowRef('Greeting flow'),
      ntChange, nfcmAfterDispatch)
      .InputEventValue(ProgramLogicEndpoint('Name'), nftText)
      .OutputState(ProgramLogicEndpoint('Caption'), NyxTextState('caption')));
end;

{$ifndef PAS2JS}
{ Write qualification companions only when the caller supplies an output folder.
  Native execution verifies their exact full design bytes in a separate process;
  exporting or browser compilation alone never establishes constructor execution. }
procedure ExportStage(const AStage, ASource, ADesign: TNyxText);
var
  LDirectory: TNyxText;

  procedure WriteBytes(const AName, AValue: TNyxText);
  var
    LFile: TFileStream;
  begin
    LFile := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if Length(AValue) > 0 then
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
  WriteBytes('nyx.generated.view.pas', ASource);
  WriteBytes('design.nyx', ADesign);
end;

procedure ExportRuntime;
var
  LDocument: TNyxDocument;
  LRoutes: INyxFlowRoutes;
begin
  LDocument := ProgramToolCanvasDocument;
  try
    LRoutes := NewNyxFlowRoutes.WithRoute(GreetingRoutes.Route(0)
      .Using(neUIQueue).Concurrent(nfrLatest).MaximumRuns(8));
    LDocument.Canvases.Define(NyxCanvasRef('Greeting canvas'),
      NyxCanvasConnections(LDocument.Canvases.Definition(NyxCanvasRef('Greeting canvas')))
        .ForControl(NyxCanvasItem('Greeting'), NyxControl('name-input'), LRoutes)
        .ForControl(NyxCanvasItem('Greeting'), NyxControl('greet-button'), NewNyxFlowRoutes).Done);
    LDocument.Validate;
    ExportStage('runtime', TNyxCodegen.Generate(LDocument), TNyxCodec.Encode(LDocument));
  finally
    LDocument.Free;
  end;
end;
{$endif}

procedure RunEditing;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LPending: TNyxText;
  LCanvasID: TNyxText;
  LChange: TNyxCanvasChange;
  LRead: TNyxCanvasChange;
  LRequest: TNyxDataValue;
  LReply: TNyxDataValue;
  LQuery: TNyxDataValue;
  LRevision: Integer;
  LFailed: Boolean;
begin
  LDocument := ProgramToolCanvasDocument;
  LSession := nil;
  LAgent := nil;
  try
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LCanvasID := LDocument.Find('program-tools').Part('workspace').ID;
    LChange := NyxSetCanvasConnections(NyxCanvasRef('Greeting canvas'), [
      NyxCanvasRoutes(NyxCanvasItem('Greeting'), NyxControl('name-input'), GreetingRoutes),
      NyxCanvasRoutes(NyxCanvasItem('Greeting'), NyxControl('greet-button'), NewNyxFlowRoutes)]);
    LRead := TNyxCanvasChange.FromData(TNyxDataValue.ParseJSON(LChange.ToData.ToJSON));
    Check(LRead.ToData.ToJSON = LChange.ToData.ToJSON,
      'typed grouped intent retains exact scene/item/control masks through strict wire admission');
    LSession := TNyxStudioSession.Create(LBefore);
    LSession.ApplyPatch(NyxCanvasPatch([LRead]));
    LAfter := LSession.ProjectSnapshot;
    Check((LAfter.Design <> LBefore.Design) and (Pos('NyxCanvasConnections(', LAfter.Source) > 0),
      'ordinary typed command publishes saved masks and crafted Pascal together');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'one Undo restores the exact original complete pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'one Redo restores the exact connection pair');
    LSession.ApplyPatch(NyxCanvasPatch([NyxInheritCanvasConnections(NyxCanvasRef('Greeting canvas'),
      NyxCanvasItem('Greeting'), NyxControl('greet-button'))]));
    Check(Length(NyxCanvasConnectionChanges(LSession.Document.Canvases.Definition(
      NyxCanvasRef('Greeting canvas')))) = 1, 'typed inheritance removes exactly its own saved mask');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'inheritance Undo restores both masks and complete accepted source');
    LSession.SetSourceDraft(LSession.Source + #10 + '// unfinished canvas wiring');
    LPending := EncodeNyxProject(LSession.ProjectSnapshot);
    LFailed := False;
    try
      LSession.ApplyPatch(NyxCanvasPatch([NyxInheritCanvasConnections(NyxCanvasRef('Greeting canvas'),
        NyxCanvasItem('Greeting'), NyxControl('name-input'))]));
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = LPending),
      'unfinished Pascal refuses grouped mutation with its pair, buffer and base exact');
    LAgent := TNyxAgentSession.Create(LBefore);
    LRevision := LAgent.Revision;
    LRequest := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('saved-canvas-masks')),
      NyxField('operations', NyxArray([LRead.ToData]))]);
    LReply := LAgent.Call('nyx_transaction', 'Scooty saved canvas qualification', LRequest);
    Check((LAgent.Revision = LRevision + 1) and
      (LAgent.Call('nyx_transaction', 'Scooty saved canvas qualification', LRequest).ToJSON = LReply.ToJSON),
      'existing semantic transaction publishes once and retains its exact retry receipt');
    LAfter := LAgent.PreviewPair(LAgent.Revision, 'home');
    LQuery := LAgent.Call('nyx_node', 'Scooty saved canvas qualification', NyxObject([
      NyxField('id', NyxData(LCanvasID)), NyxField('canvas', NyxData(True))]));
    Check((LQuery.Field('canvas').Field('connectionTotal').AsInteger = 2) and
      (LQuery.Field('canvas').Field('items').Item(0).Field('connectionTotal').AsInteger = 2),
      'ordinary scene context exposes bounded total and per-item mask counts');
    LQuery := LAgent.Call('nyx_node', 'Scooty saved canvas qualification', NyxObject([
      NyxField('id', NyxData(LCanvasID)), NyxField('canvas', NyxData(True)),
      NyxField('canvasConnections', NyxData(True)), NyxField('canvasConnectionLimit', NyxData(1)),
      NyxField('canvasConnectionControl', NyxData('name-input')), NyxField('canvasRouteLimit', NyxData(1))]));
    Check((LQuery.Field('canvas').Field('connections').Field('total').AsInteger = 1) and
      (LQuery.Field('canvas').Field('connections').Field('items').Item(0).Field('routes').Count = 1),
      'exact saved-control query pages masks and routes without returning the document');
    LFailed := False;
    try
      LAgent.Call('nyx_transaction', 'Scooty saved canvas qualification', NyxObject([
        NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('stale-masks')),
        NyxField('operations', NyxArray([LRead.ToData]))]));
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LAfter)),
      'stale semantic admission preserves every accepted source/default byte');
    LFailed := False;
    try
      LAgent.Call('nyx_transaction', 'Scooty saved canvas qualification', NyxObject([
        NyxField('expectedRevision', NyxData(LAgent.Revision)),
        NyxField('operationId', NyxData('later-mask-refusal')), NyxField('operations', NyxArray([
          NyxInheritCanvasConnections(NyxCanvasRef('Greeting canvas'),
            NyxCanvasItem('Greeting'), NyxControl('name-input')).ToData,
          NyxSetCanvasConnections(NyxCanvasRef('Greeting canvas'), [NyxCanvasRoutes(
            NyxCanvasItem('Missing item'), NyxControl('name-input'), GreetingRoutes)]).ToData]))]));
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LAfter)),
      'a later invalid replacement refuses the complete semantic group');
    LAgent.Call('nyx_history', 'Scooty saved canvas qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('undo-masks')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LBefore),
      'semantic history restores the exact original pair through the existing owner');
    {$ifndef PAS2JS}
    Check(Pos('canvas-connections-set', NyxStudioMCPTools.ToJSON) > 0,
      'the existing tool schema advertises scoped masks without adding another tool');
    {$endif}
  finally
    LAgent.Free;
    LSession.Free;
    LDocument.Free;
  end;
end;

procedure RunContinuation;
var
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
  LDefinition: INyxCanvasDefinition;
  LSource: TNyxText;
  LNext: TNyxText;
  LHelper: TNyxText;
  LPosition: Integer;
  LStage: Integer;
  LFailed: Boolean;

  function ReplaceFirst(const AText, AExpected, AReplacement: TNyxText): TNyxText;
  var
    LOffset: Integer;
  begin
    LOffset := Pos(AExpected, AText);
    Check(LOffset > 0, 'qualification source addresses exact expected text');
    Result := Copy(AText, 1, LOffset - 1) + AReplacement +
      Copy(AText, LOffset + Length(AExpected), MaxInt);
  end;

begin
  LBefore := ProgramToolCanvasDocument;
  LAfter := nil;
  try
    LBefore.Canvases.Define(NyxCanvasRef('Greeting canvas'),
      LBefore.Canvases.Definition(NyxCanvasRef('Greeting canvas')).WithGrid(NyxCanvasGrid(32)));
    LSource := TNyxCodegen.Generate(LBefore);
    LHelper := 'function SceneSpacing: Double;' + #10 + 'begin' + #10 +
      '  Result := 16 * 2;' + #10 + 'end;' + #10;
    LSource := ReplaceFirst(LSource, 'implementation', 'implementation' + #10 + #10 + LHelper);
    LSource := ReplaceFirst(LSource, 'NyxCanvasGrid(32,', 'NyxCanvasGrid(SceneSpacing,');
    {$ifndef PAS2JS}ExportStage('stage-0', LSource, TNyxCodec.Encode(LBefore));{$endif}
    for LStage := 1 to 5 do
    begin
      LAfter := LBefore.Clone;
      LDefinition := LAfter.Canvases.Definition(NyxCanvasRef('Greeting canvas'));
      case LStage of
        1, 4:
          begin
            LDefinition := NyxCanvasConnections(LDefinition).ForControl(NyxCanvasItem('Greeting'),
              NyxControl('name-input'), GreetingRoutes).Done;
          end;
        2:
          begin
            LDefinition := NyxCanvasConnections(LDefinition).ForControl(NyxCanvasItem('Greeting'),
              NyxControl('greet-button'), NewNyxFlowRoutes).Done;
          end;
        3:
          begin
            LDefinition := NyxCanvasConnections(LDefinition).Inherit(NyxCanvasItem('Greeting'),
              NyxControl('name-input')).Done;
          end;
        5:
          begin
            LDefinition := NyxCanvasConnections(LDefinition).Inherit(NyxCanvasItem('Greeting'),
              NyxControl('greet-button')).Done;
          end;
      end;
      LAfter.Canvases.Define(NyxCanvasRef('Greeting canvas'), LDefinition);
      LAfter.Validate;
      LNext := CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
      Check((Pos(LHelper, LNext) > 0) and (Pos('NyxCanvasGrid(SceneSpacing,', LNext) > 0),
        'targeted connection changes preserve exact handwritten geometry helpers');
      LPosition := Pos('NyxCanvasConnections(ADocument.Canvases.Definition(', LNext);
      Check((LPosition > 0) and (Pos('NyxCanvasGrid(', Copy(LNext, LPosition, MaxInt)) = 0),
        'connection continuation never captures helper-computed scene geometry');
      Check(Pos('// nyx:continuation ''' + TNyxText(IntToStr(LStage)) + '''', LNext) > 0,
        'repeated removal and recreation retain their chronological source stages');
      {$ifndef PAS2JS}ExportStage('stage-' + TNyxText(IntToStr(LStage)), LNext, TNyxCodec.Encode(LAfter));{$endif}
      LBefore.Free;
      LBefore := LAfter;
      LAfter := nil;
      LSource := LNext;
    end;
    LAfter := LBefore.Clone;
    LAfter.Canvases.Define(NyxCanvasRef('Greeting canvas'),
      LAfter.Canvases.Definition(NyxCanvasRef('Greeting canvas')).WithGrid(NyxCanvasGrid(34)));
    LNext := ReplaceFirst(LSource, 'Result := 16 * 2;', 'Result := 17 * 2;');
    {$ifndef PAS2JS}ExportStage('stage-6', LNext, TNyxCodec.Encode(LAfter));{$endif}
    Check(Length(NyxCanvasConnectionChanges(LAfter.Canvases.Definition(NyxCanvasRef('Greeting canvas')))) = 1,
      'a future helper edit preserves independently saved route intent');
    FreeAndNil(LAfter);
    LAfter := LBefore.Clone;
    LDefinition := NyxCanvasConnections(LAfter.Canvases.Definition(NyxCanvasRef('Greeting canvas')))
      .ForControl(NyxCanvasItem('Greeting'), NyxControl('greet-button'), NewNyxFlowRoutes).Done;
    LAfter.Canvases.Define(NyxCanvasRef('Greeting canvas'), LDefinition);
    LNext := CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
    LBefore.Free;
    LBefore := LAfter;
    LAfter := LBefore.Clone;
    LSource := LNext;
    LAfter.Canvases.Define(NyxCanvasRef('Greeting canvas'), NyxCanvasConnections(LDefinition)
      .Inherit(NyxCanvasItem('Greeting'), NyxControl('name-input'))
      .ForControl(NyxCanvasItem('Greeting'), NyxControl('name-input'), GreetingRoutes).Done);
    LFailed := False;
    try
      CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'unexpressed mask ordering refuses rather than freezing the full computed definition');
  finally
    LAfter.Free;
    LBefore.Free;
  end;
end;

procedure Run;
const
  CScene: TNyxText = 'Greeting canvas';
  CSecond: TNyxText = 'Second 🌙';
var
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LCandidate: TNyxDocument;
  LParsed: TNyxDocument;
  LSourceWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LBase: INyxCanvasDefinition;
  LSaved: INyxCanvasDefinition;
  LChanged: INyxCanvasDefinition;
  LBuilder: INyxCanvasConnections;
  LEmptyBuilder: INyxCanvasConnections;
  LAccess: INyxCanvasDefinitionConnections;
  LSnapshot: TNyxCanvasRouteChanges;
  LOriginal: TNyxCanvasRouteChanges;
  LTooMany: TNyxCanvasRouteChanges;
  LWorkspace: INyxCanvasWorkspace;
  LBefore: TNyxText;
  LBaseBytes: TNyxText;
  LSavedBytes: TNyxText;
  LWire: TNyxDataValue;
  LFields: array of TNyxDataField;
  LFailed: Boolean;
  LIndex: Integer;
begin
  LDocument := ProgramToolCanvasDocument;
  LDecoded := nil;
  LCandidate := nil;
  LParsed := nil;
  LSourceWorkspace := nil;
  try
    LBase := LDocument.Canvases.Definition(NyxCanvasRef(CScene))
      .WithItem(NyxCanvasItem(CSecond), NyxReusableRoot('greeting-template'),
        NyxCanvasPlacement(NyxCanvasLayer('Controls'), NyxCanvasBounds(540, 20, 300, 260)));
    LBaseBytes := LBase.ToData.ToJSON;
    Check((LBase.ToData.Field('version').AsInteger = 1) and (LBase.ToData.Count = 6),
      'untouched plans retain their complete version-one descriptor');
    LEmptyBuilder := NyxCanvasConnections(LBase);
    Check(LEmptyBuilder.Done.ToData.ToJSON = LBaseBytes,
      'opening connection configuration preserves every legacy byte');
    GDisposed := 0;
    GItem := NyxCanvasItem('Greeting');
    GControl := NyxControl('name-input');
    GRoutes := TRetiringRoutes.Create;
    GBuilder := NyxCanvasConnections(LBase);
    LBuilder := GBuilder.ForControl(GItem, GControl, GRoutes);
    LSnapshot := NyxCanvasConnectionChanges(LBuilder.Done);
    Check((GBuilder = nil) and (GRoutes = nil) and (GDisposed = 1) and
      (LSnapshot[0].Item.Name = 'Greeting') and (LSnapshot[0].Control.ID = 'name-input'),
      'creator callouts cannot retire the builder/input or replace captured item/control identities');
    LBuilder := LEmptyBuilder.ForControl(NyxCanvasItem('Greeting'),
      NyxControl('name-input'), GreetingRoutes);
    LSaved := LBuilder.ForControl(NyxCanvasItem('Greeting'), NyxControl('greet-button'), NewNyxFlowRoutes)
      .ForControl(NyxCanvasItem(CSecond), NyxControl('greet-button'), NewNyxFlowRoutes).Done;
    LSavedBytes := LSaved.ToData.ToJSON;
    Check((LBase.ToData.ToJSON = LBaseBytes) and (LEmptyBuilder.Done.ToData.ToJSON = LBaseBytes),
      'fluent edits preserve the source definition and earlier builder branch');
    Check(LBuilder.Done.ToData.Field('connections').Count = 1,
      'later edits preserve the earlier nonempty builder branch');
    Check((LSaved.ToData.Field('version').AsInteger = 2) and
      Supports(LSaved, INyxCanvasDefinitionConnections, LAccess) and (LAccess.ConnectionCount = 3),
      'saved masks use an optional typed capability and version-two descriptor');
    Check((LAccess.Connection(0).Item.Name = 'Greeting') and
      (LAccess.Connection(1).Routes.Count = 0) and
      (LAccess.Connection(2).Item.Name = CSecond),
      'exact item/control order and explicit empty masks survive');
    LSnapshot := NyxCanvasConnectionChanges(LSaved);
    LSnapshot[0] := NyxCanvasRoutes(NyxCanvasItem(CSecond), NyxControl('caption-label'), NewNyxFlowRoutes);
    LOriginal := NyxCanvasConnectionChanges(LSaved);
    Check(LOriginal[0].ToData.ToJSON = LAccess.Connection(0).ToData.ToJSON,
      'returned intent vectors cannot mutate saved definitions');
    LChanged := NyxCanvasDefinitionFromData(LSaved.ToData);
    Check(LChanged.ToData.ToJSON = LSavedBytes,
      'complete version-two route, geometry and Unicode metadata round-trip');
    LChanged := LSaved.WithCamera(NyxCanvasCamera(600, 400, NyxCanvasPoint(-17, 28), 0.9));
    Check(Length(NyxCanvasConnectionChanges(LChanged)) = 3,
      'ordinary geometry derivation retains its independent route masks');
    LChanged := NyxCanvasConnections(LSaved)
      .Inherit(NyxCanvasItem('Greeting'), NyxControl('name-input'))
      .Inherit(NyxCanvasItem('Greeting'), NyxControl('greet-button'))
      .Inherit(NyxCanvasItem(CSecond), NyxControl('greet-button')).Done;
    Check((LChanged.ToData.ToJSON = LBaseBytes) and (LSaved.ToData.ToJSON = LSavedBytes),
      'explicit inheritance removes masks and restores exact legacy bytes without changing its source');
    LFailed := False;
    try
      LBuilder.Controls([
        NyxCanvasRoutes(NyxCanvasItem('Greeting'), NyxControl('caption-label'), NewNyxFlowRoutes),
        NyxCanvasRoutes(NyxCanvasItem('Missing item'), NyxControl('name-input'), NewNyxFlowRoutes)]);
    except
      on E: ENyxCanvasScene do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LBuilder.Done.ToData.Field('connections').Count = 1),
      'a later missing item refuses the whole new group and preserves the old builder');
    LFailed := False;
    try
      LBuilder.Controls([
        NyxCanvasRoutes(NyxCanvasItem('Greeting'), NyxControl('name-input'), NewNyxFlowRoutes),
        NyxCanvasRoutes(NyxCanvasItem('Greeting'), NyxControl('name-input'), GreetingRoutes)]);
    except
      on E: ENyxCanvasScene do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'duplicate item/control replacements refuse instead of hiding an intermediate edit');
    SetLength(LTooMany, MaximumNyxCanvasConnections + 1);
    LFailed := False;
    try
      LBuilder.Controls(LTooMany);
    except
      on E: ENyxCanvasScene do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'oversized groups refuse before interpreting their members');
    SetLength(LFields, 7);
    LWire := LBase.ToData;
    for LIndex := 0 to 5 do
    begin
      LFields[LIndex] := NyxField(LWire.Key(LIndex), LWire.Field(LWire.Key(LIndex)));
    end;
    LFields[6] := NyxField('connections', NyxArray([]));
    LFailed := False;
    try
      NyxCanvasDefinitionFromData(NyxObject(LFields));
    except
      on E: ENyxCanvasScene do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'version one never silently accepts a connection field');
    LDocument.Canvases.Define(NyxCanvasRef(CScene), LSaved);
    LDocument.Validate;
    LBefore := TNyxCodec.Encode(LDocument);
    LDecoded := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LDecoded) = LBefore,
      'the complete document codec retains per-item masks and unchanged shared roots');
    Check(NyxControlFlowRoutes(LDocument.Find('name-input')).Count = 0,
      'saved per-item routes do not decorate the shared authored control');
    LWorkspace := NewNyxCanvasWorkspace('saved-connection-proof', LDecoded, NyxCanvasRef(CScene));
    Check(NyxControlFlowRoutes(LWorkspace.Scene.Control(NyxCanvasItem('Greeting')).Node.Find('name-input')).Count = 1,
      'workspace construction applies the saved route only to its independently owned first item');
    Check((NyxControlFlowRoutes(LWorkspace.Scene.Control(NyxCanvasItem('Greeting')).Node.Find('greet-button')).Count = 0) and
      (NyxControlFlowRoutes(LWorkspace.Scene.Control(NyxCanvasItem(CSecond)).Node.Find('greet-button')).Count = 0),
      'empty saved masks disable inherited root routes in each exact item');
    Check(NyxControlFlowRoutes(LWorkspace.Scene.Control(NyxCanvasItem(CSecond)).Node.Find('name-input')).Count = 0,
      'a sibling item does not inherit another item private connection');
    Check(TNyxCodec.Encode(LDecoded) = LBefore,
      'constructing the runtime scene preserves every saved source/default byte');
    LSource := TNyxCodegen.Generate(LDocument);
    {$ifndef PAS2JS}ExportStage('literal', LSource, LBefore);{$endif}
    Check((Pos('NyxCanvasConnections(', LSource) > 0) and (Pos('.ForControl(', LSource) > 0),
      'crafted source uses public typed scene connection configuration');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LSourceWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LBefore,
      'the full generated Pascal reader preserves every route, program, scene, default and root byte');
    Check(LSourceWorkspace.Render(LParsed) = LSource,
      'unchanged saved-connection source preserves its exact accepted frame');
    LCandidate := LDocument.Clone;
    LCandidate.Canvases.Define(NyxCanvasRef(CScene), NyxCanvasConnections(LSaved)
      .ForControl(NyxCanvasItem('Greeting'), NyxControl('title'), NewNyxFlowRoutes).Done);
    LFailed := False;
    try
      LCandidate.Validate;
    except
      on E: ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore),
      'a real control in another root cannot become this item route owner');
    FreeAndNil(LCandidate);
    LCandidate := LDocument.Clone;
    LCandidate.Canvases.Define(NyxCanvasRef(CScene), NyxCanvasConnections(LSaved)
      .ForControl(NyxCanvasItem(CSecond), NyxControl('name-input'),
        NewNyxFlowRoutes.WithRoute(NyxFlowRoute(NyxFlowRouteID('Missing'), NyxFlowRef('Missing flow'),
          ntClick, nfcmAfterDispatch))).Done);
    LFailed := False;
    try
      LCandidate.Validate;
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore),
      'missing graph/default dependencies refuse candidate admission and preserve the original document');
  finally
    LSourceWorkspace.Free;
    LParsed.Free;
    LCandidate.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    RunEditing;
    RunContinuation;
    {$ifndef PAS2JS}ExportRuntime;{$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-saved-canvas-routes', 'passed');
    document.body.setAttribute('data-saved-canvas-routes-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' saved canvas connection checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-saved-canvas-routes', 'failed');
      document.body.setAttribute('data-saved-canvas-routes-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
