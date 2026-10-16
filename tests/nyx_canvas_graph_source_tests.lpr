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

program nyx_canvas_graph_source_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, Interfaces, Forms, nyx.text, nyx.types, nyx.data, nyx.model,
  nyx.codec, nyx.codegen, nyx.controls, nyx.source, nyx.state, nyx.scheduler,
  nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.flow.editing, nyx.flow.routes,
  nyx.logic, nyx.logic.programs, nyx.programs.declarations, nyx.programs.editing,
  nyx.programs.contract,
  nyx.canvas.types, nyx.canvas.workspace, nyx.canvas.routes,
  nyx.canvas.declarations, nyx.canvas.geometry, nyx.canvas.editing,
  nyx.tool.canvas, nyx.canvas.adoption, nyx.application.lcl, nyx.canvas.lcl,
  nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs, nyx.studio.transactions,
  nyx.studio.directories, nyx.studio.outputs, nyx.studio.builds,
  nyx.studio.buildexecutor, nyx.studio.sourceprojection,
  nyx.studio.sourcecompilation, nyx.studio.sourcecompilation.native,
  nyx.test.programs.logic;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Compiled graph source: ' + AReason);
  end;
  Inc(GChecks);
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, LFile.Size);

    if Length(Result) > 0 then
    begin
      LFile.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LFile.Free;
  end;
end;

{ Exact source-only splice. UTF-8 text outside the ASCII marker is copied without
  passing through an ANSI RTL collection or untyped replacement operation. }
function ReplaceOnce(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LAt: Integer;
begin
  LAt := Pos(ABefore, ASource);

  if (LAt = 0) or (Pos(ABefore, Copy(ASource, LAt + Length(ABefore), MaxInt)) > 0) then
  begin
    raise Exception.Create('Expected one exact fixture source marker');
  end;
  Result := Copy(ASource, 1, LAt - 1) + AAfter +
    Copy(ASource, LAt + Length(ABefore), MaxInt);
end;

procedure SaveText(const APath, AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

procedure Run;
var
  LDirectories: TNyxStudioDirectories;
  LProfile: TNyxOutputConfiguration;
  LTools: TNyxDataValue;
  LExecutor: TNyxBuildExecutor;
  LBuild: INyxSourceProjectionBuild;
  LCompiler: INyxSourceCompiler;
  LSession: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LDocument: TNyxDocument;
  LExpected: TNyxDocument;
  LApp: TNyxLCLApplication;
  LCanvas: INyxLCLCanvasWorkspaceView;
  LRuntime: INyxCanvasRuntimeView;
  LEditing: INyxCanvasFlowEditing;
  LPrograms: INyxCanvasProgramEditing;
  LRoutes: INyxCanvasRouteEditing;
  LPersistence: INyxToolCanvasRoutePersistence;
  LPlacement: INyxToolCanvasGraphPlacement;
  LLayout: Boolean;
  LSemanticLayout: Boolean;
  LEdit: TNyxStudioDesignEdit;
  LGraphNodes: TNyxCanvasGraphNodes;
  LFlow: INyxFlowGraph;
  LSource: TNyxText;
  LChanged: TNyxText;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LLayoutBefore: TNyxText;
  LLayoutFlows: TNyxText;
  LStarted: QWord;
  LTurn: Integer;
begin

  if not (ParamCount in [3, 4]) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
  begin
    raise Exception.Create('Supply repository, toolchain JSON and a NEW owned compiler runtime');
  end;
  LLayout := ParamCount = 4;
  LSemanticLayout := LLayout and (ParamStr(4) = '--semantic-layout');

  if LLayout and (ParamStr(4) <> '--layout') and not LSemanticLayout then
  begin
    raise Exception.Create('The optional fourth argument is --layout or --semantic-layout');
  end;
  LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
    .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
  LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
  LProfile := TNyxOutputConfiguration.Create;
  LExecutor := nil;
  LSession := nil;
  LCommands := nil;
  LDocument := nil;
  LExpected := nil;
  LApp := nil;
  try
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LExecutor := TNyxBuildExecutor.Create(LDirectories, LProfile.Encode);
    LExpected := ProgramToolCanvasDocument;
    LExpected.Programs.Define(NyxProgramRef('Computed program'),
      NewNyxProgramDefinition('begin end.', NyxProgramContract));
    LExpected.Flows.Define(NyxFlowRef('Computed helper'), NewNyxFlowGraph
      .WithStep(NyxFlowStep('join'), NewNyxTextJoin('Computed value')).Build);

    if LLayout then
    begin
      LExpected.Canvases.Define(NyxCanvasRef('Greeting canvas'),
        NyxCanvasGraphLayout(LExpected.Canvases.Definition(NyxCanvasRef('Greeting canvas')))
          .Place(NyxFlowRef('Computed helper'), NyxFlowStep('join'), NyxCanvasPoint(17, 23)).Done);
    end;
    LSource := TNyxCodegen.Generate(LExpected);
    LSource := ReplaceOnce(LSource, '''begin end.''', 'ComputedProgramSource');
    LSource := ReplaceOnce(LSource, 'implementation', 'implementation' + #10 +
      'function ComputedProgramSource: TNyxText;' + #10 +
      'begin' + #10 + '  Result := ''begin end.'';' + #10 + 'end;' + #10 +
      'function ComputedSeparator: TNyxText;' + #10 +
      'begin' + #10 + '  Result := ''Computed value'';' + #10 + 'end;' + #10);
    LSource := ReplaceOnce(LSource, 'NewNyxTextJoin(''Computed value'')',
      'NewNyxTextJoin(ComputedSeparator)');

    if LLayout then
    begin
      LSource := ReplaceOnce(LSource, 'NyxCanvasPoint(17, 23)', 'ComputedPosition');
      LSource := ReplaceOnce(LSource, 'implementation', 'implementation' + #10 +
        'function ComputedPosition: TNyxCanvasPoint;' + #10 +
        'begin' + #10 + '  Result := NyxCanvasPoint(17, 23);' + #10 + 'end;' + #10);
    end;
    LBuild := LExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.generated.view'),
      btNativeLCL, spcChecked);
    Check((LBuild.Projection.State = spsExecuted) and
      (LBuild.Projection.Design = TNyxCodec.Encode(LExpected)),
      'actual FPC constructor establishes the exact independent computed-graph source baseline');
    LDocument := TNyxCodec.Decode(LBuild.Projection.Design);
    LSession := TNyxStudioSession.Create;
    LSession.AdoptProjectedProject(NyxProjectPair(LBuild.Projection.Design, LSource), LBuild.Projection);
    LSession.Activate('home');
    LCommands := TNyxSourceCommands.Create(LSession, nil);
    LCompiler := NewNyxNativeSourceCompiler(LDirectories, LProfile.Encode, TNyxCompilerLimits.Default);
    LCommands.UseCompiler(LCompiler);
    LApp := TNyxLCLApplication.Create;
    LApp.Mount(LDocument);
    LApp.Window.Show;
    LCanvas := LApp.View.CanvasView(LDocument.Find('program-tools').Part(NyxPart('workspace')).ID)
      as INyxLCLCanvasWorkspaceView;
    Check(Supports(LCanvas, INyxCanvasRuntimeView, LRuntime) and
      Supports(LCanvas, INyxCanvasFlowEditing, LEditing) and
      Supports(LCanvas, INyxCanvasProgramEditing, LPrograms) and
      Supports(LCanvas, INyxCanvasRouteEditing, LRoutes),
      'actual runtime supplies owned scene and typed graph/route publication');
    LCanvas.Refresh;
    LCanvas.Execute(nccFit);
    LPrograms.ApplyProgramChanges([
      NyxDefineProgram(NyxProgramRef('Greeting'), NewNyxProgramDefinition(
        'begin Caption := ''Hello again, '' + Name + ''!'' end.', GreetingProgramContract)),
      NyxDefineProgram(NyxProgramRef('Friendly greeting'), NewNyxProgramDefinition(
        'begin Caption := ''Welcome, '' + Name + ''!'' end.', GreetingProgramContract))],
      LRuntime.Scene.Revision, LPrograms.ProgramRevision);
    LFlow := NewNyxFlowGraph
      .WithStep(NyxFlowStep('greeting'), NewNyxProgramLogic(
        NyxProgramRef('Friendly greeting'), GreetingProgramContract))
      .WithStep(NyxFlowStep('echo'), NewNyxTextJoin(' / '))
      .Connect(ProgramLogicEndpoint('Caption'), NyxFlowEndpoint(NyxFlowStep('echo'), NyxLogicPort(nlpLeft)))
      .Connect(ProgramLogicEndpoint('Caption'), NyxFlowEndpoint(NyxFlowStep('echo'), NyxLogicPort(nlpRight))).Build;
    LEditing.ApplyFlowChanges([NyxDefineFlow(NyxFlowRef('Echo greeting'),
      NyxFlowDefinitionFromData(LFlow.ToData))], LRuntime.Scene.Revision, LEditing.FlowRevision);
    LRoutes.ApplyRouteChanges([
      NyxCanvasRoutes(NyxCanvasItem('Greeting'), NyxControl('name-input'),
        NewNyxFlowRoutes.WithRoute(NyxFlowRoute(NyxFlowRouteID('Live greeting'),
          NyxFlowRef('Echo greeting'), ntChange, nfcmAfterDispatch)
          .InputEventValue(ProgramLogicEndpoint('Name'), nftText)
          .OutputState(NyxFlowEndpoint(NyxFlowStep('echo'), NyxLogicPort(nlpText)), NyxTextState('caption'))))],
      LRuntime.Scene.Revision, LRoutes.RouteRevision);
    Check(Supports(LApp.View.ToolCanvasView('program-tools').Inspector,
      INyxToolCanvasRoutePersistence, LPersistence), 'ordinary public inspector supplies source adoption');
    LPersistence.ConfigureAdoption(NyxControl(LDocument.Find('program-tools').Part(NyxPart('workspace')).ID),
      LCommands.CanvasAdoptionHost(LSession.CommandContext, NyxControl('home')), nil);

    if LLayout then
    begin
      Check(Supports(LApp.View.ToolCanvasView('program-tools').Inspector,
        INyxToolCanvasGraphPlacement, LPlacement), 'source owner supplies the same typed node placement proposal');
      LPlacement.StageGraphNodePosition(NyxFlowRef('Echo greeting'), NyxFlowStep('echo'),
        NyxCanvasPoint(360, 220));
    end;
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LPersistence.SaveConnections;
    LStarted := GetTickCount64;
    while LCommands.Busy do
    begin
      CheckSynchronize;
      Application.ProcessMessages;

      if GetTickCount64 - LStarted > 240000 then
      begin
        raise Exception.Create('Compiler-backed source adoption deadline');
      end;
      Sleep(1);
    end;
    Check((LCommands.State = nssApplied) and not LPersistence.AdoptionPending,
      'owning source queue admits real compiler-backed graph and route publication: ' + LCommands.Message);
    Check(Pos('NewNyxTextJoin(ComputedSeparator)', LSession.Source) > 0,
      'unchanged helper-defined graph remains computed in accepted source');
    Check(Pos('ComputedProgramSource', LSession.Source) > 0,
      'unchanged helper-defined Pascal program remains computed after other programs change');

    if LLayout then
    begin
      Check((Length(NyxCanvasGraphNodes(LSession.Document.Canvases.Definition(NyxCanvasRef('Greeting canvas')))) = 2) and
        (Pos('ComputedPosition', LSession.Source) > 0),
        'one combined graph/mask/layout save retains its independent computed position');
    end;
    Check((LSession.Document.Programs.Definition(NyxProgramRef('Friendly greeting')).Source =
      'begin Caption := ''Welcome, '' + Name + ''!'' end.') and
      (LSession.Document.Programs.Definition(NyxProgramRef('Greeting')).Source =
      'begin Caption := ''Hello again, '' + Name + ''!'' end.'),
      'new and replaced runtime programs publish with their referencing graph and mask');
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore,
      'one semantic Undo restores exact handwritten source and original design');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter,
      'one semantic Redo restores exact compiler-accepted graph/source checkpoint');
    LPersistence.SaveConnections;
    Check(not LPersistence.AdoptionPending and not LCommands.Busy,
      'same compiler-adopted applied registry is an ordinary no-op Save');

    if LLayout then
    begin
      LLayoutBefore := EncodeNyxProject(LSession.ProjectSnapshot);
      LLayoutFlows := LSession.Document.Flows.ToData.ToJSON;

      if LSemanticLayout then
      begin
        { Decode the same semantic leaf operations into the ordinary owning
          compiler queue. Chronological remove/recreation shares one paired
          publication and leaves the independent computed helper untouched. }
        LEdit := Default(TNyxStudioDesignEdit);
        LEdit.Action := sdaTransaction;
        LEdit.Selection := 'program-tools';
        LEdit.View := 'home';
        LEdit.TransactionDesignBaseline := LSession.Save;
        LEdit.Transaction := CaptureNyxProjectTransaction(ReadNyxProjectTransaction(NyxArray([
          NyxPositionCanvasGraphNode(NyxCanvasRef('Greeting canvas'), NyxFlowRef('Echo greeting'),
            NyxFlowStep('echo'), NyxCanvasPoint(400, 250)).ToData,
          NyxAutomaticCanvasGraphNode(NyxCanvasRef('Greeting canvas'), NyxFlowRef('Echo greeting'),
            NyxFlowStep('echo')).ToData,
          NyxPositionCanvasGraphNode(NyxCanvasRef('Greeting canvas'), NyxFlowRef('Echo greeting'),
            NyxFlowStep('echo'), NyxCanvasPoint(444, 260)).ToData])));
        LCommands.Edit(LEdit);
      end
      else
      begin
        LPlacement.StageGraphNodePosition(NyxFlowRef('Echo greeting'), NyxFlowStep('echo'),
          NyxCanvasPoint(444, 260));
        LPersistence.SaveConnections;
      end;
      LStarted := GetTickCount64;
      while LCommands.Busy do
      begin
        CheckSynchronize;
        Application.ProcessMessages;

        if GetTickCount64 - LStarted > 240000 then
        begin
          raise Exception.Create('Compiler-backed layout-only adoption deadline');
        end;
        Sleep(1);
      end;
      Check((LCommands.State = nssApplied) and not LPersistence.AdoptionPending and
        (LSession.Document.Flows.ToData.ToJSON = LLayoutFlows),
        'a layout-only save uses real compiler admission without changing graph execution declarations');
      LGraphNodes := NyxCanvasGraphNodes(LSession.Document.Canvases.Definition(
        NyxCanvasRef('Greeting canvas')));
      Check((Length(LGraphNodes) = 2) and (LGraphNodes[1].Step.Name = 'echo') and
        (LGraphNodes[1].Position.X = 444) and (LGraphNodes[1].Position.Y = 260),
        'the complete admitted layout retains final chronological intent and the independent helper');
      LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
      Check(LAfter <> LLayoutBefore, 'the explicit layout-only change publishes a new exact pair');
      LSession.Undo;
      Check(EncodeNyxProject(LSession.ProjectSnapshot) = LLayoutBefore,
        'one layout-only Undo restores the exact prior compiled pair');
      LSession.Redo;
      Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter,
        'one layout-only Redo restores the exact later compiled pair');
      LPersistence.SaveConnections;
      Check(not LPersistence.AdoptionPending and not LCommands.Busy,
        'the acknowledged layout-only save is then an ordinary no-op');
    end;
    SaveText(LDirectories.RuntimeRoot + 'accepted.pas', LSession.Source);
    SaveText(LDirectories.RuntimeRoot + 'nyx.generated.view.pas', LSession.Source);
    SaveText(LDirectories.RuntimeRoot + 'accepted.nyx', LSession.Save);
    LChanged := ReplaceOnce(LSession.Source, 'Result := ''Computed value'';',
      'Result := ''Changed helper value'';');
    LChanged := ReplaceOnce(LChanged, 'Result := ''begin end.'';',
      'Result := ''var Ignored: Integer; begin Ignored := 7 end.'';');

    if LLayout then
    begin
      LChanged := ReplaceOnce(LChanged, 'Result := NyxCanvasPoint(17, 23);',
        'Result := NyxCanvasPoint(-31, 47);');
    end;
    FreeAndNil(LExpected);
    LExpected := LSession.Document.Clone;
    LExpected.Programs.Define(NyxProgramRef('Computed program'), NewNyxProgramDefinition(
      'var Ignored: Integer; begin Ignored := 7 end.', NyxProgramContract));
    LExpected.Flows.Define(NyxFlowRef('Computed helper'), NewNyxFlowGraph
      .WithStep(NyxFlowStep('join'), NewNyxTextJoin('Changed helper value')).Build);

    if LLayout then
    begin
      LExpected.Canvases.Define(NyxCanvasRef('Greeting canvas'),
        NyxCanvasGraphLayout(LExpected.Canvases.Definition(NyxCanvasRef('Greeting canvas')))
          .Place(NyxFlowRef('Computed helper'), NyxFlowStep('join'), NyxCanvasPoint(-31, 47)).Done);
    end;
    LBuild := LExecutor.ProjectSource(LChanged, NyxPascalUnit('nyx.generated.view'),
      btNativeLCL, spcChecked);
    Check((LBuild.Projection.State = spsExecuted) and
      (LBuild.Projection.Design = TNyxCodec.Encode(LExpected)),
      'actual changed helper still determines untouched graph meaning after source adoption');
    LBuild := LExecutor.ProjectSource(LSession.Source, NyxPascalUnit('nyx.generated.view'), btBrowser);
    Check(LBuild.Projection.State = spsCompiled,
      'exact accepted handwritten companion also compiles for the browser without execution provenance');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter,
      'independent qualification jobs never change accepted source/history');
  finally
    LPlacement := nil;
    LPersistence := nil;
    LRoutes := nil;
    LEditing := nil;
    LPrograms := nil;
    LRuntime := nil;
    LCanvas := nil;
    LApp.Free;
    LCommands.Free;
    LCompiler := nil;
    LSession.Free;
    LExpected.Free;
    LDocument.Free;
    LExecutor.Free;
    LProfile.Free;
    for LTurn := 1 to 12 do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
      Sleep(1);
    end;
  end;
end;

begin
  try
    Application.Initialize;
    Run;
    WriteLn('PASS ', GChecks, ' actual compiler-backed graph source checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
