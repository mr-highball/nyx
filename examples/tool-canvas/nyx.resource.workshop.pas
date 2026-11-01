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
unit nyx.resource.workshop;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.model, nyx.types, nyx.canvas.types, nyx.flow.types, nyx.flow.declarations,
  nyx.resource.sources, nyx.data.rows;

const
  ResourceWorkshopSample: TNyxText =
    '{"items":[{"id":"grace","name":"Grace","active":true,"rank":9007199254740993},' +
    '{"id":"hidden","name":"Hidden","active":false,"rank":1},' +
    '{"id":"ada","name":"Ada","active":true,"rank":9007199254740992}]}';

type
  { Closed public identities address controls inside the reusable canvas item,
    independently of their position or the renderer's physical widget names. }
  TResourceWorkshopControl = (rwcEmbedded, rwcHosted, rwcTable, rwcSummary,
    rwcError, rwcCacheWarning);

function ResourceWorkshopControl(AControl: TResourceWorkshopControl): TNyxControlRef;
function ResourceWorkshopCanvas: TNyxControlRef;
function ResourceWorkshopItem: TNyxCanvasItemRef;
function ResourceWorkshopFlow: TNyxFlowRef;
function ResourceWorkshopRows: TNyxDataRows;
{ One portable tool embeds ordinary controls in a Tool Canvas. Both buttons use
  the same typed graph; their reference inputs choose admitted embedded/hosted
  declarations. Loading policy/authority stays with the running application.
  The default hosted URL is illustrative; pass a public JSON URL or edit the
  resource in Studio. Failed loading retains accepted rows and exposes errors. }
function BuildResourceWorkshopDocument: TNyxDocument; overload;
function BuildResourceWorkshopDocument(
  const AHostedURL: TNyxResourceURL): TNyxDocument; overload;

implementation

uses
  SysUtils, nyx.controls, nyx.root.types, nyx.state, nyx.collections,
  nyx.collections.view.types, nyx.resources, nyx.data, nyx.flow,
  nyx.flow.routes, nyx.flow.route.model, nyx.logic,
  nyx.logic.resources, nyx.logic.collections, nyx.tool.canvas,
  nyx.canvas.declarations, nyx.canvas.geometry;

function ResourceWorkshopControl(AControl: TResourceWorkshopControl): TNyxControlRef;
const
  CNames: array[TResourceWorkshopControl] of TNyxText =
    ('load-embedded-team', 'load-hosted-team', 'team-table', 'team-summary',
     'team-error', 'team-cache-warning');
begin

  if (Ord(AControl) < Ord(Low(TResourceWorkshopControl))) or
    (Ord(AControl) > Ord(High(TResourceWorkshopControl))) then
  begin
    raise ENyxModel.Create('Unknown resource workshop control');
  end;
  Result := NyxControl(CNames[AControl]);
end;

function ResourceWorkshopCanvas: TNyxControlRef;
begin
  Result := NyxControl('resource-workshop');
end;

function ResourceWorkshopItem: TNyxCanvasItemRef;
begin
  Result := NyxCanvasItem('Team');
end;

function ResourceWorkshopFlow: TNyxFlowRef;
begin
  Result := NyxFlowRef('Load resource team');
end;

function ResourceWorkshopRows: TNyxDataRows;
begin
  Result := NyxDataRows.Identity(NyxResourcePath.Field('id'))
    .Text(NyxTextField('name')).DecimalText(NyxTextField('rank'));
end;

function LogicPort(const AStep: TNyxText; APort: TNyxLogicPort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), NyxLogicPort(APort));
end;

function ReadPort(APort: TNyxJSONResourcePort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep('resource'), NyxJSONResourcePort(APort));
end;

function TeamRoute(const AID: TNyxFlowRouteRef;
  const AReference: TNyxResourceRef): TNyxFlowRoute;
begin
  Result := NyxFlowRoute(AID, ResourceWorkshopFlow, ntClick, nfcmExclusive)
    .InputSignal(ReadPort(njrpTrigger))
    .InputConstant(ReadPort(njrpReference), NyxFlowText(AReference.Name))
    .OutputRows(LogicPort('sort', nlpValue), NyxCollection('team'), ResourceWorkshopRows)
    .OutputState(LogicPort('format', nlpText), NyxTextState('team-summary'))
    .OutputState(ReadPort(njrpError), NyxTextState('team-error'))
    .OutputState(ReadPort(njrpCacheWarning), NyxTextState('team-cache-warning'));
end;

function BuildResourceWorkshopDocument: TNyxDocument;
begin
  Result := BuildResourceWorkshopDocument(NyxResourceURL('https://example.invalid/team.json'));
end;

function BuildResourceWorkshopDocument(const AHostedURL: TNyxResourceURL): TNyxDocument;
var
  LPanel: INyxPanel;
  LTable: INyxTable;
  LPage: INyxPage;
  LCanvas: INyxToolCanvas;
  LGraph: INyxFlowGraph;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Resource Workshop';
    Result.Resources.Define(NyxResourceRef('sample-team'),
      NyxJSONResource(ResourceWorkshopSample)
        .Describe('Sample team', 'Embedded team records for an offline-first starter.'));
    Result.Resources.Define(NyxResourceRef('hosted-team'),
      NyxHostedResource(nrkJSON, AHostedURL)
        .Cache(NyxResourceCache.Memory.FreshFor(60).StaleFor(300).MaximumBytes(1024 * 1024))
        .Describe('Hosted team', 'Public JSON with an items array, cached using caller policy.'));
    Result.State.SetValue(NyxTextState('team-summary'), 'Choose a resource to load your team.');
    Result.State.SetValue(NyxTextState('team-error'), '');
    Result.State.SetValue(NyxTextState('team-cache-warning'), '');
    Result.Collections.Define(NyxCollection('team'), ResourceWorkshopRows.Schema, []);
    LPanel := NewNyxPanel('resource-team');
    LPanel.Configure.Padding(20).Gap(12).Surface(True).Done;
    LPanel.Add(NewNyxHeading('team-heading').WithText('People, from your resources'));
    LPanel.Add(NewNyxLabel('team-help').WithText(
      'Load JSON, keep active people, sort exact ranks, then publish one complete Table.'));
    LPanel.Add(NewNyxButton(ResourceWorkshopControl(rwcEmbedded).ID).WithText('Load embedded sample'));
    LPanel.Add(NewNyxButton(ResourceWorkshopControl(rwcHosted).ID).WithText('Load hosted team'));
    LTable := NewNyxTable(ResourceWorkshopControl(rwcTable).ID);
    LTable.Configure.Height(240).Done;
    LTable.Binds.Collection(NyxCollectionView(NyxCollection('team'))
      .Column(NyxTextField('name'), 'Name').Column(NyxTextField('rank'), 'Exact rank')).Done;
    LPanel.Add(LTable);
    LPanel.Add(NewNyxCode(ResourceWorkshopControl(rwcSummary).ID)
      .Binds.Text(NyxTextState('team-summary')).Done);
    LPanel.Add(NewNyxLabel(ResourceWorkshopControl(rwcError).ID)
      .Binds.Text(NyxTextState('team-error')).Done);
    LPanel.Add(NewNyxLabel(ResourceWorkshopControl(rwcCacheWarning).ID)
      .Binds.Text(NyxTextState('team-cache-warning')).Done);
    Result.AddComponent(LPanel);
    LGraph := NewNyxFlowGraph
      .WithStep(NyxFlowStep('resource'), NewNyxJSONResource(
        NyxJSONResourceOptions(NyxResourceRef('sample-team')).WholeRequest(15000)))
      .WithStep(NyxFlowStep('items'), NewNyxJSONSelect(NyxDataPath.Field('items')))
      .WithStep(NyxFlowStep('active'), NewNyxJSONFilter(
        NyxDataPath.Field('active'), njcEqual, NyxData(True)))
      .WithStep(NyxFlowStep('sort'), NewNyxJSONSort(NyxDataPath.Field('rank'), njkDecimal))
      .WithStep(NyxFlowStep('format'), NewNyxJSONFormat)
      .Connect(ReadPort(njrpJSON), LogicPort('items', nlpJSON))
      .Connect(LogicPort('items', nlpValue), LogicPort('active', nlpJSON))
      .Connect(LogicPort('active', nlpValue), LogicPort('sort', nlpJSON))
      .Connect(LogicPort('sort', nlpValue), LogicPort('format', nlpJSON)).Build;
    LGraph := NyxFlowConditions(LGraph).When(NyxFlowStep('items'), ReadPort(njrpSucceeded)).Build;
    Result.Flows.Define(ResourceWorkshopFlow, LGraph);
    SetNyxFlowRoutes(Result.Find(ResourceWorkshopControl(rwcEmbedded).ID),
      NewNyxFlowRoutes.WithRoute(TeamRoute(NyxFlowRouteID('Load sample team'),
        NyxResourceRef('sample-team'))));
    SetNyxFlowRoutes(Result.Find(ResourceWorkshopControl(rwcHosted).ID),
      NewNyxFlowRoutes.WithRoute(TeamRoute(NyxFlowRouteID('Load hosted team'),
        NyxResourceRef('hosted-team'))));
    Result.Canvases.Define(NyxCanvasRef('Resource workbench'), NewNyxCanvasDefinition
      .WithCamera(NyxCanvasCamera(600, 720, NyxCanvasPoint(160, 300), 1))
      .WithLayer(NyxCanvasLayer('Work'))
      .WithItem(ResourceWorkshopItem, NyxReusableRoot('resource-team'),
        NyxCanvasPlacement(NyxCanvasLayer('Work'), NyxCanvasBounds(0, 0, 320, 1100))));
    LCanvas := NewNyxToolCanvas(ResourceWorkshopCanvas.ID).WithScene(NyxCanvasRef('Resource workbench'));
    LCanvas.WorkspaceCanvas.Configure.Height(720).MinimumHeight(360).Done;
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(16).Gap(12).Done;
    LPage.Add(NewNyxHeading('workshop-heading').WithText('Resource Workshop'));
    LPage.Add(LCanvas);
    Result.AddPage(LPage);
    Result.AddPage(NewNyxPage('away').Add(NewNyxHeading('away-heading').WithText('Your team is waiting')));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
