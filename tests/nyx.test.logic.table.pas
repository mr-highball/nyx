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

unit nyx.test.logic.table;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.model, nyx.data.rows, nyx.flow, nyx.scheduler,
  nyx.http.types, nyx.resources.loader;

{ English, complete document and immutable row recipe. The saved graph has no
  transport authority. The consumer supplies its own provider and application. }
function LogicTableDocument: TNyxDocument;
function LogicTableRoutedDocument: TNyxDocument;
{ Expected companion after the ordinary row editor maps Name to the ID field. }
function LogicTableEditedDocument: TNyxDocument;
function LogicTableRows: TNyxDataRows;

type
  { Explicit asynchronous substitute, not an HTTP server. Each request captures
    its complete response before delivery; later edits cannot change that run.
    The request's ordinary cancellation gate suppresses an already queued reply. }
  TLogicTableTransport = class(TInterfacedObject, INyxHTTPTransport)
  private
    FScheduler: INyxScheduler;
  public
    Payload: TNyxText;
    Calls: Integer;
    constructor Create(const AScheduler: INyxScheduler);
    function Send(const ARequest: TNyxHTTPRequest;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
  end;

const
  LogicTablePayload: TNyxText =
    '{"items":[{"active":true,"profile":{"id":"grace","name":"Grace","rank":9007199254740993,' +
    '"ready":true,"count":3,"ratio":1.25,"literal.🌟\u0000":{"value":"English note"}}},' +
    '{"active":false,"profile":{"id":"hidden","name":"Hidden","rank":1}},' +
    '{"active":true,"profile":{"id":"ada","name":"Ada","rank":9007199254740992,' +
    '"ready":true,"count":3,"ratio":1.25,"literal.🌟\u0000":{"value":"English note"}}}]}';

implementation

uses
  nyx.bytes, nyx.controls, nyx.types, nyx.state, nyx.collections,
  nyx.resources, nyx.resource.sources, nyx.collections.view.types, nyx.flow.types, nyx.flow.declarations,
  nyx.logic, nyx.logic.http, nyx.logic.collections, nyx.flow.routes, nyx.flow.route.model;

type
  TReply = class(TNyxResourceRequest)
  public
    Payload: TNyxText;
    procedure Deliver;
  end;
  TDelivery = class(TInterfacedObject, INyxWork)
  public
    Reply: TReply;
    Lease: INyxResourceRequest;
    procedure Execute(const AExecution: INyxExecution);
  end;

function TableRowsForName(const ANamePath: TNyxResourcePath): TNyxDataRows;
begin
  Result := NyxDataRows.Identity(NyxResourcePath.Field('id'))
    .Text(NyxTextField('name'), ANamePath).DecimalText(NyxTextField('rank'))
    .Boolean(NyxBooleanField('ready')).Integer(NyxIntegerField('count'))
    .Number(NyxNumberField('ratio'))
    .Text(NyxTextField('note'), NyxResourcePath.Field(TNyxText('literal.🌟') + #0).Field('value'));
end;

function LogicTableRows: TNyxDataRows;
begin
  Result := TableRowsForName(NyxResourcePath.Field('name'));
end;

function Endpoint(const AStep: TNyxText; APort: TNyxLogicPort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), NyxLogicPort(APort));
end;

function LogicTableDocument: TNyxDocument;
var
  LPage: INyxPage;
  LTable: INyxTable;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Connected team data';
    Result.State.SetValue(NyxTextState('summary'), 'Ready to load');
    Result.State.SetValue(NyxIntegerState('status'), 0);
    Result.Collections.Define(NyxCollection('people'), LogicTableRows.Schema, []);
    Result.Flows.Define(NyxFlowRef('Team'), NewNyxFlowGraph
      .WithStep(NyxFlowStep('request'), NewNyxHTTPCall(
        NyxHTTPRequest(NyxResourceURL('https://example.invalid/team')).MaximumBytes(65536)))
      .WithStep(NyxFlowStep('decode'), NewNyxUTF8Decode)
      .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
      .WithStep(NyxFlowStep('items'), NewNyxJSONSelect(NyxDataPath.Field('items')))
      .WithStep(NyxFlowStep('active'), NewNyxJSONFilter(
        NyxDataPath.Field('active'), njcEqual, NyxData(True)))
      .WithStep(NyxFlowStep('sort'), NewNyxJSONSort(
        NyxDataPath.Field('profile').Field('rank'), njkDecimal))
      .WithStep(NyxFlowStep('profile'), NewNyxJSONMap(NyxDataPath.Field('profile')))
      .WithStep(NyxFlowStep('format'), NewNyxJSONFormat)
      .Connect(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpBody)),
        NyxFlowEndpoint(NyxFlowStep('decode'), NyxHTTPPort(nhpBody)))
      .Connect(Endpoint('decode', nlpText), Endpoint('parse', nlpText))
      .Connect(Endpoint('parse', nlpJSON), Endpoint('items', nlpJSON))
      .Connect(Endpoint('items', nlpValue), Endpoint('active', nlpJSON))
      .Connect(Endpoint('active', nlpValue), Endpoint('sort', nlpJSON))
      .Connect(Endpoint('sort', nlpValue), Endpoint('profile', nlpJSON))
      .Connect(Endpoint('profile', nlpValue), Endpoint('format', nlpJSON)).Build);
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(20).Gap(12).Done;
    LPage.Add(NewNyxHeading('title').WithText('From request to useful rows'));
    LPage.Add(NewNyxLabel('intro').WithText(
      'Decode, filter, sort and map a response into an ordinary Nyx Table.'));
    LPage.Add(NewNyxButton('load').WithText('Load team'));
    LTable := NewNyxTable('people-table');
    LTable.Configure.Height(240).Done;
    LTable.Binds.Collection(NyxCollectionView(NyxCollection('people'))
      .Column(NyxTextField('name'), 'Name').Column(NyxTextField('rank'), 'Exact rank')).Done;
    LPage.Add(LTable);
    LPage.Add(NewNyxCode('summary').Binds.Text(NyxTextState('summary')).Done);
    Result.AddPage(LPage);
    Result.AddPage(NewNyxPage('other').Add(NewNyxHeading('away').WithText('Work retired')));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function LogicTableRoutedDocument: TNyxDocument;
begin
  Result := LogicTableDocument;
  try
    SetNyxFlowRoutes(Result.Find('load'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Load team'), NyxFlowRef('Team'), ntClick, nfcmExclusive)
        .InputSignal(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpTrigger)))
        .OutputState(Endpoint('format', nlpText), NyxTextState('summary'))
        .OutputState(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpStatus)),
          NyxIntegerState('status'))
        .OutputRows(Endpoint('profile', nlpValue), NyxCollection('people'), LogicTableRows)
        .Using(neAsynchronous)));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function LogicTableEditedDocument: TNyxDocument;
var
  LRoutes: INyxFlowRoutes;
  LRoute: TNyxFlowRoute;
begin
  Result := LogicTableRoutedDocument;
  try
    LRoutes := NyxControlFlowRoutes(Result.Find('load'));
    LRoute := LRoutes.Route(0).WithRows(Endpoint('profile', nlpValue),
      NyxCollection('people'), TableRowsForName(NyxResourcePath.Field('id')).Numbers(nrnpApproximate));
    SetNyxFlowRoutes(Result.Find('load'), LRoutes.WithRoute(LRoute));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

constructor TLogicTableTransport.Create(const AScheduler: INyxScheduler);
begin
  inherited Create;
  FScheduler := AScheduler;
  Payload := LogicTablePayload;
end;

procedure TReply.Deliver;
var
  LResult: TNyxResourceHTTPResult;
begin
  LResult := Default(TNyxResourceHTTPResult);
  LResult.Status := 200;
  LResult.Bytes := NyxEncodeUTF8(Payload);
  LResult.Headers := NyxHTTPHeaders.Append('content-type', 'application/json');
  LResult.HeaderVisibility := nhvBrowserFiltered;
  Complete(LResult);
end;

procedure TDelivery.Execute(const AExecution: INyxExecution);
begin
  Reply.Deliver;
end;

function TLogicTableTransport.Send(const ARequest: TNyxHTTPRequest;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
var
  LReply: TReply;
  LDelivery: TDelivery;
  LWork: INyxWork;
begin
  FScheduler.RequireUI;
  ARequest.Validate;
  Inc(Calls);
  LReply := TReply.Create(AReply);
  Result := LReply;
  LReply.Payload := Payload;
  LDelivery := TDelivery.Create;
  LWork := LDelivery;
  LDelivery.Reply := LReply;
  LDelivery.Lease := Result;
  FScheduler.PostUI(LWork);
end;

end.
