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
program nyx_canvas_reuse_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.source, nyx.composition, nyx.data, nyx.root.types,
  nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.declarations,
  nyx.canvas.editing, nyx.canvas.editor, nyx.studio.projects,
  nyx.studio.session, nyx.studio.edits, nyx.studio.agents
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Canvas reuse: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Saved content remains ordinary reusable Nyx controls. A scene never retains
  this document; each mounted instance independently materializes its template. }
function Fixture: TNyxDocument;
var
  LScene: INyxCanvasDefinition;
  LLayer: TNyxCanvasLayerRef;
  LCanvas: INyxCanvas;
  LWrapper: INyxCard;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Reusable canvas workshop';
    Result.AddPage(NewNyxPage('home'));
    Result.AddComponent(NewNyxMemo('first-content').Configure.Value('First scene').Done);
    Result.AddComponent(NewNyxMemo('second-content').Configure.Value('Second scene').Done);
    LLayer := NyxCanvasLayer('Controls');
    LScene := NewNyxCanvasDefinition
      .WithCamera(NyxCanvasCamera(400, 300, NyxCanvasPoint(0, 0)))
      .WithLayer(LLayer)
      .WithItem(NyxCanvasItem('Editor'), NyxReusableRoot('first-content'),
        NyxCanvasPlacement(LLayer, NyxCanvasBounds(-150, -100, 300, 200)));
    Result.Canvases.Define(NyxCanvasRef('First'), LScene);
    Result.Canvases.Define(NyxCanvasRef('Second'),
      NewNyxCanvasDefinition.WithLayer(LLayer)
        .WithItem(NyxCanvasItem('Editor'), NyxReusableRoot('second-content'),
          NyxCanvasPlacement(LLayer, NyxCanvasBounds(-150, -100, 300, 200))));
    LCanvas := NewNyxCanvas('canvas-template').WithScene(NyxCanvasRef('First'));
    LCanvas.Configure.Height(350).MinimumHeight(300).PartName(NyxPart('board')).Done;
    Result.AddComponent(LCanvas);
    Result.Pages[0].Add(NewNyxComponent('inherited')
      .Configure.Component(NyxComponent('canvas-template')).Done);
    Result.Pages[0].Add(NewNyxComponent('alternate')
      .Configure.Component(NyxComponent('canvas-template'))
        .CanvasScene(NyxCanvasRef('Second')).Done);
    Result.Pages[0].Add(NewNyxComponent('empty')
      .Configure.Component(NyxComponent('canvas-template')).NoCanvasScene.Done);
    LWrapper := NewNyxCard('wrapper-template');
    LWrapper.Add(NewNyxCanvas('wrapped-board').WithScene(NyxCanvasRef('First'))
      .Configure.PartName(NyxPart('board')).Height(350).Done);
    Result.AddComponent(LWrapper);
    Result.Pages[0].Add(NewNyxComponent('wrapped')
      .Configure.Component(NyxComponent('wrapper-template')).Done);
    Result.Find('wrapped').OverridePart(NyxPart('board')).Named('wrapped-scene')
      .Configure.CanvasScene(NyxCanvasRef('Second')).Done;
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LRuntime: TNyxNode;
  LSource: TNyxText;
  LBefore: TNyxText;
  LWorkspace: TNyxSourceWorkspace;
  LParsed: TNyxDocument;
  LPair: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LSession: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LReply: TNyxDataValue;
  LForm: INyxCard;
  LChange: TNyxCanvasEditorChange;
  LRefused: Boolean;
  LEmptyDocument: TNyxDocument;
  LDirectory: TNyxText;
  {$ifndef PAS2JS}LStream: TFileStream;{$endif}
begin
  {$ifdef PAS2JS}document.body.setAttribute('data-fixture-checkpoint', 'constructing');{$endif}
  LDocument := Fixture;
  LDecoded := nil;
  LRuntime := nil;
  LWorkspace := nil;
  LParsed := nil;
  LSession := nil;
  LAgent := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    {$ifdef PAS2JS}document.body.setAttribute('data-fixture-checkpoint', 'wire');{$endif}
    LDecoded := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LDecoded) = LBefore, 'full string/null/absent wire round trip');
    Check(not LDecoded.Find('inherited').HasCanvasScene and
      LDecoded.Find('empty').HasCanvasScene and not LDecoded.Find('empty').CanvasScene.Defined,
      'wire distinguishes inherited scene from explicit empty mask');
    LRuntime := RealizeNyxView(LDocument, LDocument.Pages[0]);
    Check(LRuntime.Find(NyxQualifiedID('inherited', 'canvas-template')).CanvasScene.Name = 'First',
      'root instance inherits template scene');
    Check(LRuntime.Find(NyxQualifiedID('alternate', 'canvas-template')).CanvasScene.Name = 'Second',
      'root instance independently overrides scene');
    Check(LRuntime.Find(NyxQualifiedID('empty', 'canvas-template')).HasCanvasScene and
      not LRuntime.Find(NyxQualifiedID('empty', 'canvas-template')).CanvasScene.Defined,
      'empty instance masks template content');
    Check(LRuntime.Find(NyxQualifiedID('wrapped', 'wrapper-template')).Part(NyxPart('board')).CanvasScene.Name = 'Second',
      'named-part descriptor overrides nested canvas');
    LSource := TNyxCodegen.Generate(LDocument);
    {$ifdef PAS2JS}document.body.setAttribute('data-fixture-checkpoint', 'source');{$endif}
    Check((Pos('.CanvasScene(NyxCanvasRef(', LSource) > 0) and
      (Pos('.NoCanvasScene', LSource) > 0), 'crafted source uses typed mask/override configuration');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LBefore, 'complete emitted source reconstructs exact reusable meaning');
    Check(LWorkspace.Render(LParsed) = LSource, 'unchanged source bytes remain exact');
    LPair := NyxProjectPair(LBefore, LSource);
    LSession := TNyxStudioSession.Create(LPair);
    {$ifdef PAS2JS}document.body.setAttribute('data-fixture-checkpoint', 'history');{$endif}
    LSession.Select('alternate');
    LSession.ApplyPatch(NyxCanvasPatch([NyxDetachCanvas(NyxControl('alternate'))]));
    Check(not LSession.Document.Find('alternate').HasCanvasScene, 'detach restores inheritance');
    LAfter := LSession.ProjectSnapshot;
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair), 'override Undo restores complete pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter), 'override Redo restores inherited pair');
    LForm := NewNyxCanvasSceneEditor('reuse-picker', NyxControl('alternate'), LDocument);
    LForm.Node.Find('reuse-picker-choice').Configure.Value('Use inherited scene').Done;
    Check(CaptureNyxCanvasSceneEditor(LForm.Node.Find('reuse-picker-apply'), LForm.Node, LChange) and
      (LChange.Change.Kind = nccDetachScene), 'public picker captures explicit inheritance');
    LAgent := TNyxAgentSession.Create(LPair);
    LReply := LAgent.Call('nyx_node', 'Scooty reusable canvas', NyxObject([
      NyxField('id', NyxData('inherited')), NyxField('canvas', NyxData(True))]));
    Check(LReply.Field('canvas').Field('defined').AsBoolean and
      not LReply.Field('canvas').Field('localDeclared').AsBoolean,
      'bounded effective semantic context distinguishes inherited declaration');
    LReply := LAgent.Call('nyx_node', 'Scooty reusable canvas', NyxObject([
      NyxField('id', NyxData('inherited')), NyxField('canvas', NyxData(True)),
      NyxField('canvasScope', NyxData('local'))]));
    Check(not LReply.Field('canvas').Field('defined').AsBoolean, 'bounded local context omits inherited scene');
    LAgent.Call('nyx_transaction', 'Scooty reusable canvas', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('empty-instance')),
      NyxField('operations', NyxArray([NyxEmptyCanvas(NyxControl('alternate')).ToData]))]));
    LDecoded.Free;
    LDecoded := TNyxCodec.Decode(LAgent.PreviewPair(LAgent.Revision, 'home').Design);
    Check(LDecoded.Find('alternate').HasCanvasScene and not LDecoded.Find('alternate').CanvasScene.Defined and
      not LDecoded.Find('inherited').HasCanvasScene, 'semantic empty changes only exact instance');
    LRefused := False;
    try
      LSession.ApplyPatch(NyxCanvasPatch([NyxAttachCanvas(NyxControl('wrapped'), NyxCanvasRef('First'))]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter)),
      'scene declaration on non-canvas reusable root refuses entire pair');
    LEmptyDocument := TNyxDocument.Create;
    try
      LEmptyDocument.AddPage(NewNyxPage('empty-home'));
      LEmptyDocument.Pages[0].Add(NewNyxCanvas('empty-board').Configure.NoCanvasScene.Done);
      Check(TNyxDataValue.ParseJSON(TNyxCodec.Encode(LEmptyDocument)).Field('version').AsInteger = 13,
        'empty-only declaration promotes wire even without a saved registry');
      FreeAndNil(LDecoded);
      LDecoded := TNyxCodec.Decode(TNyxCodec.Encode(LEmptyDocument));
      Check(LDecoded.Find('empty-board').HasCanvasScene and
        not LDecoded.Find('empty-board').CanvasScene.Defined,
        'empty-only wire retains explicit mask');
      LEmptyDocument.Find('empty-board').Configure.InheritCanvasScene.Done;
      Check(not LEmptyDocument.Find('empty-board').HasCanvasScene,
        'typed inheritance removes only the local declaration');
      LRefused := False;
      try
        LEmptyDocument.Find('empty-board').Configure.ForPlatform(npfBrowser).NoCanvasScene.Done;
      except
        on Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and not LEmptyDocument.Find('empty-board').HasCanvasScene,
        'structural scene policies refuse platform-only configuration');
    finally
      LEmptyDocument.Free;
    end;
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      LDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
      ForceDirectories(LDirectory);
      LStream := TFileStream.Create(LDirectory + 'nyx.generated.view.pas', fmCreate);
      try
        LStream.WriteBuffer(LSource[1], Length(LSource));
      finally
        LStream.Free;
      end;
      LStream := TFileStream.Create(LDirectory + 'design.nyx', fmCreate);
      try
        LStream.WriteBuffer(LBefore[1], Length(LBefore));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LAgent.Free;
    LSession.Free;
    LParsed.Free;
    LWorkspace.Free;
    LRuntime.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end;

procedure ExecuteQualification;
begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-reuse', 'passed');
    document.body.setAttribute('data-canvas-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' canvas reuse/source/history checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-reuse', 'failed');
      document.body.setAttribute('data-canvas-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  {$ifdef PAS2JS}
  { Return from page startup before the substantial source/history journey.
    The host still observes its ordinary real-clock readiness deadline. }
  if window.location.search = '?startup-only' then
  begin
    { Diagnostic readiness is separate from the actual contract marker. }
    document.body.setAttribute('data-canvas-module', 'passed');
  end
  else
  begin
    window.setTimeout(@ExecuteQualification, 0);
  end;
  {$else}
  ExecuteQualification;
  {$endif}
end.
