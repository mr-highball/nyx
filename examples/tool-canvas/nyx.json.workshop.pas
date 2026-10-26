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
  nyx.flow.declarations, nyx.tool.canvas;

const
  JSONWorkshopSample: TNyxText =
    '{"project":"Weekend workshop","owner":"Alex","tasks":["Sketch","Build","Share"]}';

type
  { Closed identities let applications and input consumers address this example
    without duplicating its persisted control names. No reference owns a view. }
  TJSONWorkshopControl = (jwcSource, jwcFormat, jwcSave, jwcPreview, jwcSavedName);

function JSONWorkshopControl(AControl: TJSONWorkshopControl): TNyxControlRef;
function JSONWorkshopCanvas: TNyxControlRef;
function JSONWorkshopItem: TNyxCanvasItemRef;
function JSONWorkshopSaveFlow: TNyxFlowRef;
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
  nyx.files.save, nyx.flow, nyx.flow.types, nyx.flow.routes, nyx.flow.route.model,
  nyx.canvas.geometry, nyx.canvas.workspace, nyx.canvas.declarations;

const
  CCanvas = 'json-workshop';
  CTemplate = 'json-notebook';
  CControls: array[TJSONWorkshopControl] of TNyxText =
    ('json-source', 'format-json', 'save-json', 'json-preview', 'saved-name');

type
  { One shared stage vocabulary keeps graph construction and route mappings
    aligned. Names remain open persisted references at the public boundary. }
  TWorkshopStep = (wsParse, wsFormat, wsEncode, wsSave);

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

function Step(AValue: TWorkshopStep): TNyxFlowStepRef;
const
  CNames: array[TWorkshopStep] of TNyxText = ('Parse JSON', 'Format JSON', 'Encode UTF-8', 'Save file');
begin
  Result := NyxFlowStep(CNames[AValue]);
end;

function LogicPort(AValue: TWorkshopStep; APort: TNyxLogicPort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(Step(AValue), NyxLogicPort(APort));
end;

function FilePort(AValue: TWorkshopStep; APort: TNyxFilePort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(Step(AValue), NyxFilePort(APort));
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
      'Paste some JSON, format it, then keep a copy. The logic behind this tool is yours to reshape.'));
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

    LFormatFlow := NyxFlowRef('Format JSON');
    LGraph := NewNyxFlowGraph
      .WithStep(Step(wsParse), NewNyxJSONParse)
      .WithStep(Step(wsFormat), NewNyxJSONFormat(njlIndented, 2))
      .Connect(LogicPort(wsParse, nlpJSON), LogicPort(wsFormat, nlpJSON)).Build;
    Result.Flows.Define(LFormatFlow, LGraph);
    SetNyxFlowRoutes(Result.Find(JSONWorkshopControl(jwcFormat).ID), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Format notebook'), LFormatFlow, ntClick, nfcmExclusive)
        .InputState(LogicPort(wsParse, nlpText), JSONWorkshopSource)
        .OutputState(LogicPort(wsFormat, nlpText), JSONWorkshopPreview)));

    LGraph := NewNyxFlowGraph
      .WithStep(Step(wsParse), NewNyxJSONParse)
      .WithStep(Step(wsFormat), NewNyxJSONFormat(njlIndented, 2))
      .WithStep(Step(wsEncode), NewNyxUTF8Encode)
      .WithStep(Step(wsSave), NewNyxSaveFile(NyxFileSaveOptions('workshop.json')
        .Title('Save your JSON').MediaType(NyxMediaType(nfmtJSON)).Allow('json').Mode(nfsmPreferPicker)))
      .Connect(LogicPort(wsParse, nlpJSON), LogicPort(wsFormat, nlpJSON))
      .Connect(LogicPort(wsFormat, nlpText), LogicPort(wsEncode, nlpText))
      .Connect(FilePort(wsEncode, nfpBytes), FilePort(wsSave, nfpBytes)).Build;
    Result.Flows.Define(JSONWorkshopSaveFlow, LGraph);
    SetNyxFlowRoutes(Result.Find(JSONWorkshopControl(jwcSave).ID), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Save notebook'), JSONWorkshopSaveFlow, ntClick, nfcmExclusive)
        .InputState(LogicPort(wsParse, nlpText), JSONWorkshopSource)
        .InputSignal(FilePort(wsSave, nfpTrigger))
        .OutputState(LogicPort(wsFormat, nlpText), JSONWorkshopPreview)
        .OutputState(FilePort(wsSave, nfpName), NyxTextState('saved-name'))
        .OutputState(FilePort(wsSave, nfpWritten), NyxBooleanState('save-written'))
        .OutputState(FilePort(wsSave, nfpDelivered), NyxBooleanState('save-delivered'))
        .OutputState(FilePort(wsSave, nfpCancelled), NyxBooleanState('save-cancelled'))
        .OutputState(FilePort(wsSave, nfpError), NyxTextState('save-error'))));

    LScene := NewNyxCanvasDefinition
      .WithCamera(NyxCanvasCamera(1200, 760, NyxCanvasPoint(0, 0), 1))
      .WithLayer(NyxCanvasLayer('Work'))
      .WithItem(JSONWorkshopItem, NyxReusableRoot(CTemplate),
        NyxCanvasPlacement(NyxCanvasLayer('Work'), NyxCanvasBounds(20, 20, 450, 700)));
    LScene := NyxCanvasGraphLayout(LScene)
      .Place(JSONWorkshopSaveFlow, Step(wsParse), NyxCanvasPoint(520, 30))
      .Place(JSONWorkshopSaveFlow, Step(wsFormat), NyxCanvasPoint(820, 30))
      .Place(JSONWorkshopSaveFlow, Step(wsEncode), NyxCanvasPoint(520, 340))
      .Place(JSONWorkshopSaveFlow, Step(wsSave), NyxCanvasPoint(820, 340)).Done;
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
