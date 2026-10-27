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
unit nyx.json.workshop;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.state, nyx.canvas.types,
  nyx.flow.types, nyx.flow.declarations, nyx.tool.canvas,
  nyx.programs.types, nyx.programs.contract, nyx.programs.declarations;

const
  JSONWorkshopSample: TNyxText =
    '{"project":"Weekend workshop","owner":"Alex","tasks":["Sketch","Build","Share"]}';
  { This is authored interpreted Pascal, independently editable in the canvas's
    Pascal programs tab. Only a complete newline-delimited Markdown fence is
    removed. Incomplete or differently labelled fences remain parser input,
    so preparation cannot silently repair or truncate malformed JSON. }
  JSONWorkshopPreparationSource: TNyxText =
    '{ Accept JSON copied from a Markdown example while preserving its values. }' + #10 +
    'function WithoutFence(const Value: String): String;' + #10 +
    'var' + #10 +
    '  First: Integer;' + #10 +
    '  Size: Integer;' + #10 +
    'begin' + #10 +
    '  Result := Trim(Value);' + #10 +
    '  Size := Length(Result);' + #10 +
    '  First := 0;' + #10 +
    '' + #10 +
    '  if (Size >= 7) and ((Copy(Result, 1, 7) = ''```json'') or' + #10 +
    '    (Copy(Result, 1, 7) = ''```JSON'')) then' + #10 +
    '  begin' + #10 +
    '    First := 8;' + #10 +
    '  end' + #10 +
    '  else if (Size >= 4) and (Copy(Result, 1, 3) = ''```'') then' + #10 +
    '  begin' + #10 +
    '    First := 4;' + #10 +
    '  end;' + #10 +
    '' + #10 +
    '  if (First > 0) and (Size >= First + 3) and' + #10 +
    '    ((Copy(Result, First, 1) = #10) or (Copy(Result, First, 1) = #13)) and' + #10 +
    '    (Copy(Result, Size - 2, 3) = ''```'') then' + #10 +
    '  begin' + #10 +
    '    Result := Trim(Copy(Result, First, Size - First - 2));' + #10 +
    '  end;' + #10 +
    'end;' + #10 +
    '' + #10 +
    'begin' + #10 +
    '  Prepared := WithoutFence(Source);' + #10 +
    'end.';

type
  { Closed identities let applications and input consumers address this example
    without duplicating its persisted control names. No reference owns a view. }
  TJSONWorkshopControl = (jwcSource, jwcFormat, jwcSave, jwcPreview, jwcSavedName);
  { Public stage identities keep graph wiring, saved placement and consumers
    independent of insertion order. The program stage has only text ports. }
  TJSONWorkshopStep = (jwsPrepare, jwsParse, jwsFormat, jwsEncode, jwsSave);

function JSONWorkshopControl(AControl: TJSONWorkshopControl): TNyxControlRef;
function JSONWorkshopCanvas: TNyxControlRef;
function JSONWorkshopItem: TNyxCanvasItemRef;
function JSONWorkshopSaveFlow: TNyxFlowRef;
function JSONWorkshopStep(AStep: TJSONWorkshopStep): TNyxFlowStepRef;
function JSONWorkshopPreparation: TNyxProgramRef;
function JSONWorkshopPreparationContract: TNyxProgramContract;
function JSONWorkshopSource: TNyxTextStateRef;
function JSONWorkshopPreview: TNyxTextStateRef;

{ An ordinary independently owned document with saved reusable controls, graphs,
  routes and node placement. It contains no executable file host. Applications
  explicitly configure saving; Studio may inspect/edit the same portable design. }
function BuildJSONWorkshopDocument: TNyxDocument;
{ Optional initial editor presentation through public Tool Canvas interfaces.
  It authors no source/history and retains no controller. Hosts call it after
  mounting; physical cards publish on the view's queued UI turn. Observers wait
  for that publication rather than treating this request as a rendered frame.
  Saved documents remain useful without this initial presentation. }
procedure ShowJSONWorkshopGraph(const ATools: INyxToolCanvasView);

implementation

uses
  SysUtils, nyx.controls, nyx.root.types, nyx.logic, nyx.logic.files,
  nyx.files.save, nyx.flow, nyx.flow.routes, nyx.flow.route.model,
  nyx.canvas.geometry, nyx.canvas.workspace, nyx.canvas.declarations,
  nyx.logic.programs;

const
  CCanvas = 'json-workshop';
  CTemplate = 'json-notebook';
  CControls: array[TJSONWorkshopControl] of TNyxText =
    ('json-source', 'format-json', 'save-json', 'json-preview', 'saved-name');

function JSONWorkshopControl(AControl: TJSONWorkshopControl): TNyxControlRef;
begin
  Result := NyxControl(CControls[AControl]);
end;

function JSONWorkshopCanvas: TNyxControlRef;
begin
  Result := NyxControl(CCanvas);
end;

function JSONWorkshopItem: TNyxCanvasItemRef;
begin
  Result := NyxCanvasItem('Notebook');
end;

function JSONWorkshopSaveFlow: TNyxFlowRef;
begin
  Result := NyxFlowRef('Save JSON');
end;

function JSONWorkshopSource: TNyxTextStateRef;
begin
  Result := NyxTextState('json-source');
end;

function JSONWorkshopPreview: TNyxTextStateRef;
begin
  Result := NyxTextState('json-preview');
end;

function JSONWorkshopPreparation: TNyxProgramRef;
begin
  Result := NyxProgramRef('Prepare JSON');
end;

function JSONWorkshopPreparationContract: TNyxProgramContract;
begin
  Result := NyxProgramContract
    .Input(NyxProgramSymbol('Source'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Prepared'), NyxProgramType(npkText));
end;

function JSONWorkshopStep(AStep: TJSONWorkshopStep): TNyxFlowStepRef;
const
  CNames: array[TJSONWorkshopStep] of TNyxText =
    ('Prepare JSON', 'Parse JSON', 'Format JSON', 'Encode UTF-8', 'Save file');
begin
  Result := NyxFlowStep(CNames[AStep]);
end;

function LogicPort(AValue: TJSONWorkshopStep; APort: TNyxLogicPort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(JSONWorkshopStep(AValue), NyxLogicPort(APort));
end;

function FilePort(AValue: TJSONWorkshopStep; APort: TNyxFilePort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(JSONWorkshopStep(AValue), NyxFilePort(APort));
end;

function BuildJSONWorkshopDocument: TNyxDocument;
var
  LPage: INyxPage;
  LNotebook: INyxCard;
  LSource: INyxMemo;
  LPreview: INyxCode;
  LButtons: INyxRow;
  LLabel: INyxLabel;
  LGraph: INyxFlowGraph;
  LScene: INyxCanvasDefinition;
  LCanvas: INyxToolCanvas;
  LFormatFlow: TNyxFlowRef;
  LPreparation: INyxProgramDefinition;
  LSourcePort: TNyxFlowEndpoint;
  LPreparedPort: TNyxFlowEndpoint;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'JSON Workshop';
    Result.State.SetValue(JSONWorkshopSource, JSONWorkshopSample);
    Result.State.SetValue(JSONWorkshopPreview, 'Your formatted JSON appears here.');
    Result.State.SetValue(NyxTextState('saved-name'), '');
    Result.State.SetValue(NyxTextState('save-error'), '');
    Result.State.SetValue(NyxBooleanState('save-written'), False);
    Result.State.SetValue(NyxBooleanState('save-delivered'), False);
    Result.State.SetValue(NyxBooleanState('save-cancelled'), False);
    LNotebook := NewNyxCard(CTemplate);
    LNotebook.Configure.Layout(nlColumn).Gap(12).Padding(20).Done;
    LNotebook.Add(NewNyxHeading('notebook-title').WithText('Your JSON'));
    LNotebook.Add(NewNyxLabel('notebook-help').WithText(
      'Paste JSON or a fenced JSON example, format it, then keep a copy. ' +
      'Open Pascal programs / Prepare JSON to customize how input is prepared.'));
    LSource := NewNyxMemo(JSONWorkshopControl(jwcSource).ID).WithText('Source');
    LSource.Configure.Height(170).AccessibleName('JSON source').Done;
    LSource.Binds.Value(JSONWorkshopSource).Done;
    LNotebook.Add(LSource);
    LButtons := NewNyxRow('notebook-actions');
    LButtons.Configure.Gap(8).Done;
    LButtons.Add(NewNyxButton(JSONWorkshopControl(jwcFormat).ID).WithText('Format JSON'));
    LButtons.Add(NewNyxButton(JSONWorkshopControl(jwcSave).ID).WithText('Save JSON'));
    LNotebook.Add(LButtons);
    LNotebook.Add(NewNyxLabel('preview-caption').WithText('Last formatted result'));
    LPreview := NewNyxCode(JSONWorkshopControl(jwcPreview).ID);
    LPreview.Configure.Height(170).AccessibleName('Formatted JSON').Done;
    LPreview.Binds.Text(JSONWorkshopPreview).Done;
    LNotebook.Add(LPreview);
    LNotebook.Add(NewNyxLabel('save-caption').WithText('Last save'));
    LLabel := NewNyxLabel(JSONWorkshopControl(jwcSavedName).ID);
    LLabel.Binds.Text(NyxTextState('saved-name')).Done;
    LNotebook.Add(LLabel);
    LLabel := NewNyxLabel('save-written').WithText('The file was written.');
    LLabel.Binds.Visible(NyxBooleanState('save-written')).Done;
    LNotebook.Add(LLabel);
    LLabel := NewNyxLabel('save-delivered').WithText('Save request accepted.');
    LLabel.Binds.Visible(NyxBooleanState('save-delivered')).Done;
    LNotebook.Add(LLabel);
    LLabel := NewNyxLabel('save-cancelled').WithText('Save cancelled. Your JSON is still here.');
    LLabel.Binds.Visible(NyxBooleanState('save-cancelled')).Done;
    LNotebook.Add(LLabel);
    LLabel := NewNyxLabel('save-error');
    LLabel.Binds.Text(NyxTextState('save-error')).Done;
    LNotebook.Add(LLabel);
    Result.AddComponent(LNotebook);

    LPreparation := NewNyxProgramDefinition(JSONWorkshopPreparationSource,
      JSONWorkshopPreparationContract, NyxProgramLimits.Steps(10000));
    Result.Programs.Define(JSONWorkshopPreparation, LPreparation);
    LSourcePort := NyxFlowEndpoint(JSONWorkshopStep(jwsPrepare), NyxFlowPort('Source'));
    LPreparedPort := NyxFlowEndpoint(JSONWorkshopStep(jwsPrepare), NyxFlowPort('Prepared'));
    LFormatFlow := NyxFlowRef('Format JSON');
    LGraph := NewNyxFlowGraph
      .WithStep(JSONWorkshopStep(jwsPrepare), NewNyxProgramLogic(JSONWorkshopPreparation, LPreparation))
      .WithStep(JSONWorkshopStep(jwsParse), NewNyxJSONParse)
      .WithStep(JSONWorkshopStep(jwsFormat), NewNyxJSONFormat(njlIndented, 2))
      .Connect(LPreparedPort, LogicPort(jwsParse, nlpText))
      .Connect(LogicPort(jwsParse, nlpJSON), LogicPort(jwsFormat, nlpJSON)).Build;
    Result.Flows.Define(LFormatFlow, LGraph);
    SetNyxFlowRoutes(Result.Find(JSONWorkshopControl(jwcFormat).ID), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Format notebook'), LFormatFlow, ntClick, nfcmExclusive)
        .InputState(LSourcePort, JSONWorkshopSource)
        .OutputState(LogicPort(jwsFormat, nlpText), JSONWorkshopPreview)));

    LGraph := NewNyxFlowGraph
      .WithStep(JSONWorkshopStep(jwsPrepare), NewNyxProgramLogic(JSONWorkshopPreparation, LPreparation))
      .WithStep(JSONWorkshopStep(jwsParse), NewNyxJSONParse)
      .WithStep(JSONWorkshopStep(jwsFormat), NewNyxJSONFormat(njlIndented, 2))
      .WithStep(JSONWorkshopStep(jwsEncode), NewNyxUTF8Encode)
      .WithStep(JSONWorkshopStep(jwsSave), NewNyxSaveFile(NyxFileSaveOptions('workshop.json')
        .Title('Save your JSON').MediaType(NyxMediaType(nfmtJSON)).Allow('json').Mode(nfsmPreferPicker)))
      .Connect(LPreparedPort, LogicPort(jwsParse, nlpText))
      .Connect(LogicPort(jwsParse, nlpJSON), LogicPort(jwsFormat, nlpJSON))
      .Connect(LogicPort(jwsFormat, nlpText), LogicPort(jwsEncode, nlpText))
      .Connect(FilePort(jwsEncode, nfpBytes), FilePort(jwsSave, nfpBytes)).Build;
    Result.Flows.Define(JSONWorkshopSaveFlow, LGraph);
    SetNyxFlowRoutes(Result.Find(JSONWorkshopControl(jwcSave).ID), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Save notebook'), JSONWorkshopSaveFlow, ntClick, nfcmExclusive)
        .InputState(LSourcePort, JSONWorkshopSource)
        .InputSignal(FilePort(jwsSave, nfpTrigger))
        .OutputState(LogicPort(jwsFormat, nlpText), JSONWorkshopPreview)
        .OutputState(FilePort(jwsSave, nfpName), NyxTextState('saved-name'))
        .OutputState(FilePort(jwsSave, nfpWritten), NyxBooleanState('save-written'))
        .OutputState(FilePort(jwsSave, nfpDelivered), NyxBooleanState('save-delivered'))
        .OutputState(FilePort(jwsSave, nfpCancelled), NyxBooleanState('save-cancelled'))
        .OutputState(FilePort(jwsSave, nfpError), NyxTextState('save-error'))));

    LScene := NewNyxCanvasDefinition
      .WithCamera(NyxCanvasCamera(1200, 760, NyxCanvasPoint(0, 0), 1))
      .WithLayer(NyxCanvasLayer('Work'))
      .WithItem(JSONWorkshopItem, NyxReusableRoot(CTemplate),
        NyxCanvasPlacement(NyxCanvasLayer('Work'), NyxCanvasBounds(20, 20, 450, 700)));
    LScene := NyxCanvasGraphLayout(LScene)
      .Place(JSONWorkshopSaveFlow, JSONWorkshopStep(jwsPrepare), NyxCanvasPoint(520, 30))
      .Place(JSONWorkshopSaveFlow, JSONWorkshopStep(jwsParse), NyxCanvasPoint(820, 30))
      .Place(JSONWorkshopSaveFlow, JSONWorkshopStep(jwsFormat), NyxCanvasPoint(1120, 30))
      .Place(JSONWorkshopSaveFlow, JSONWorkshopStep(jwsEncode), NyxCanvasPoint(820, 340))
      .Place(JSONWorkshopSaveFlow, JSONWorkshopStep(jwsSave), NyxCanvasPoint(1120, 340)).Done;
    Result.Canvases.Define(NyxCanvasRef('JSON workbench'), LScene);
    LCanvas := NewNyxToolCanvas(JSONWorkshopCanvas.ID).WithScene(NyxCanvasRef('JSON workbench'));
    LCanvas.WorkspaceCanvas.Configure.Height(720).MinimumHeight(360).Done;
    LPage := NewNyxPage('home');
    LPage.Configure.Gap(12).Padding(16).Done;
    LPage.Add(NewNyxHeading('workshop-title').WithText('JSON Workshop'));
    LPage.Add(NewNyxLabel('workshop-help').WithText(
      'An editable tool built from Nyx controls and connected logic. Pan the workspace to explore.'));
    LPage.Add(LCanvas);
    Result.AddPage(LPage);
    Result.AddPage(NewNyxPage('away').Add(
      NewNyxHeading('away-title').WithText('Your workshop is waiting')));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure ShowJSONWorkshopGraph(const ATools: INyxToolCanvasView);
var
  LGraphs: INyxToolCanvasGraphConnections;
  LNodes: INyxToolCanvasPipelineNodes;
begin

  if ATools = nil then
  begin
    raise ENyxModel.Create('JSON Workshop needs its mounted Tool Canvas');
  end;

  if Supports(ATools.Inspector, INyxToolCanvasGraphConnections, LGraphs) then
  begin
    LGraphs.ShowGraphConnections(JSONWorkshopSaveFlow);
  end;

  if Supports(ATools, INyxToolCanvasPipelineNodes, LNodes) then
  begin
    LNodes.ShowPipelineNodes;
  end;
end;

end.
