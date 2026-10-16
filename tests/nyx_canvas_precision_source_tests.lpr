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
program nyx_canvas_precision_source_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, nyx.text, nyx.bytes, nyx.types, nyx.model, nyx.controls,
  nyx.data, nyx.codec, nyx.codegen, nyx.source, nyx.root.types,
  nyx.canvas.geometry, nyx.canvas.types, nyx.canvas.precision,
  nyx.canvas.declarations, nyx.canvas.editing,
  nyx.studio.projects, nyx.studio.session, nyx.studio.agents
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

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

{$ifndef PAS2JS}
{ Write exact owned fixture bytes for independent Pascal constructor validation;
  no protected workspace, installed artifact or source file is addressed. }
procedure ExportPair(const AStage: String; const APair: TNyxProjectPair);
var
  LDirectory: String;

  procedure Save(const AName: String; const AText: TNyxText);
  var
    LBytes: TNyxBytes;
    LFile: TFileStream;
  begin
    LBytes := NyxEncodeUTF8(AText);
    LFile := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if Length(LBytes) > 0 then
      begin
        LFile.WriteBuffer(LBytes[0], Length(LBytes));
      end;
    finally
      LFile.Free;
    end;
  end;
begin

  if ParamStr(1) = '' then
  begin
    Exit;
  end;
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + AStage + PathDelim;
  ForceDirectories(LDirectory);
  Save('nyx.generated.view.pas', APair.Source);
  Save('expected.nyx', APair.Design);
end;
{$endif}

procedure Run;
var
  LDocument: TNyxDocument;
  LExpected: TNyxDocument;
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPage: INyxPage;
  LCard: INyxCard;
  LDefinition: INyxCanvasDefinition;
  LOptions: TNyxCanvasPrecisionOptions;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LChanged: TNyxText;
  LSession: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LReply: TNyxDataValue;
begin
  LDocument := TNyxDocument.Create;
  LExpected := nil;
  LRead := nil;
  LWorkspace := nil;
  LSession := nil;
  LAgent := nil;
  try
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LPage.Add(NewNyxCanvas('board').WithScene(NyxCanvasRef('Board')));
    LCard := NewNyxCard('notes-card');
    LCard.Add(NewNyxMemo('notes-memo').WithText('Notes'));
    LDocument.AddComponent(LCard);
    LOptions := NyxCanvasPrecisionOptions
      .WithRulers(NyxCanvasRulers.Axes([ncaxX, ncaxY])
        .FromOrigin(NyxCanvasPoint(10.5, -20.25)).Units(2.5, 'cm').Interval(0.25, 72, 5).Thickness(24))
      .WithGuide(NyxCanvasGuide(NyxCanvasGuideRef('Left margin'), ncaxX, -120.5))
      .WithGuide(NyxCanvasGuide(NyxCanvasGuideRef('Top margin'), ncaxY, 60.25).Locked(True))
      .Snapping(ncgspVisible, 8);
    LDefinition := NyxCanvasPrecision(NewNyxCanvasDefinition.WithCamera(
      NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0)).Within(
        NyxCanvasBounds(-1200, -1000, 2400, 2000)))
      .WithLayer(NyxCanvasLayer('Content'))
      .WithItem(NyxCanvasItem('Notes'), NyxReusableRoot('notes-card'),
        NyxCanvasPlacement(NyxCanvasLayer('Content'), NyxCanvasBounds(-130, -60, 260, 120))))
      .Options(LOptions).Done;
    LDocument.Canvases.Define(NyxCanvasRef('Board'), LDefinition);
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    Check((Pos('NyxCanvasPrecision(', LBefore.Source) > 0) and
      (Pos('.Rulers(NyxCanvasRulers.Axes([ncaxX, ncaxY])', LBefore.Source) > 0) and
      (Pos('ncgspVisible', LBefore.Source) > 0), 'crafted source uses fluent typed precision construction');
    LRead := TNyxSourceWorkspace.PrepareDraft(LBefore.Source, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LBefore.Design, 'literal source admission reproduces the complete precision design');
    FreeAndNil(LRead);
    FreeAndNil(LWorkspace);
    LExpected := LDocument.Clone;
    LExpected.Canvases.Define(NyxCanvasRef('Board'), NyxCanvasPrecision(LDefinition)
      .Guide(LOptions.Guide(0).AtPosition(-100.25)).Rulers(LOptions.Rulers.Axes([ncaxY]))
      .Snapping(ncgspAll, 12).Done);
    LChanged := TNyxCodegen.Generate(LExpected);
    LSession := TNyxStudioSession.Create(LBefore);
    LSession.SetSourceDraft(LChanged);
    LSession.ApplySourceDraft;
    LAfter := LSession.ProjectSnapshot;
    Check((LAfter.Design = TNyxCodec.Encode(LExpected)) and (LAfter.Source = LChanged),
      'ordinary Apply publishes the whole expected precision design and Pascal pair');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'ordinary Undo restores the entire previous pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'ordinary Redo restores the entire applied precision pair');
    LAgent := TNyxAgentSession.Create(LBefore);
    LReply := LAgent.Call('nyx_node', 'Precision qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('id', NyxData('board')),
      NyxField('canvas', NyxData(True)), NyxField('canvasPrecision', NyxData(True)),
      NyxField('canvasGuideOffset', NyxData(1)), NyxField('canvasGuideLimit', NyxData(1))]));
    LReply := LReply.Field('canvas').Field('precision');
    Check((LReply.Field('guideTotal').AsInteger = 2) and
      (LReply.Field('guides').Count = 1) and
      (LReply.Field('guides').Item(0).ToJSON = LOptions.Guide(1).ToData.ToJSON),
      'semantic precision independently pages exact saved guides');
    Check(LReply.Field('rulers').ToJSON = LOptions.Rulers.ToData.ToJSON,
      'semantic precision retains bounded typed ruler units and intervals');
    LAgent.Call('nyx_transaction', 'Precision qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('precision-apply')),
      NyxField('operations', NyxArray([NyxDefineCanvas(NyxCanvasRef('Board'),
        LExpected.Canvases.Definition(NyxCanvasRef('Board'))).ToData]))]));
    LAfter := LAgent.PreviewPair(LAgent.Revision, 'home');
    Check(LAfter.Design = TNyxCodec.Encode(LExpected), 'existing semantic scene transaction admits complete typed precision intent');
    LAgent.Call('nyx_history', 'Precision qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('precision-undo')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LBefore),
      'semantic Undo restores the complete previous accepted pair');
    LAgent.Call('nyx_history', 'Precision qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('precision-redo')),
      NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LAfter),
      'semantic Redo restores the complete precision pair');
    {$ifndef PAS2JS}
    ExportPair('original', LBefore);
    ExportPair('changed', LAfter);
    {$endif}
  finally
    LAgent.Free;
    LSession.Free;
    LWorkspace.Free;
    LRead.Free;
    LExpected.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-precision-source', 'passed');
    document.body.setAttribute('data-canvas-precision-source-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' exact precision source/history checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-precision-source', 'failed');
      document.body.setAttribute('data-canvas-precision-source-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
