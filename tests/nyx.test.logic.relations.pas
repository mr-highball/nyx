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
unit nyx.test.logic.relations;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.model, nyx.flow, nyx.flow.types, nyx.logic, nyx.logic.relations,
  nyx.numbers.decimal, nyx.data.rows;

const
  RelationOrders: TNyxText =
    '[{"id":"order-1","customer":"ada","amount":0.1},' +
    '{"id":"order-2","customer":"grace","amount":0.2},' +
    '{"id":"order-3","customer":"ada","amount":9007199254740993},' +
    '{"id":"order-4","customer":"unknown","amount":5}]';
  RelationCustomers: TNyxText =
    '[{"id":"ada","name":"Ada","region":"West"},' +
    '{"id":"grace","name":"Grace","region":"East"},' +
    '{"id":"ada","name":"Duplicate customer","region":"Ignored"}]';

function RelationEndpoint(const AStep: TNyxText; APort: TNyxLogicPort): TNyxFlowEndpoint;
function RelationGraph(ARounding: TNyxDecimalRounding = ndrToEven;
  ANulls: TNyxJSONNullKeys = njnMatch): INyxFlowGraph;
function RelationDocument(ARounding: TNyxDecimalRounding = ndrToEven;
  ANulls: TNyxJSONNullKeys = njnMatch): TNyxDocument;
function RelationOrderRows: TNyxDataRows;
function RelationGroupRows: TNyxDataRows;

implementation

uses
  nyx.types, nyx.state, nyx.controls, nyx.data, nyx.collections,
  nyx.collections.view.types, nyx.logic.collections,
  nyx.flow.declarations, nyx.flow.routes, nyx.flow.route.model;

function RelationEndpoint(const AStep: TNyxText; APort: TNyxLogicPort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), NyxLogicPort(APort));
end;

function RelationGraph(ARounding: TNyxDecimalRounding; ANulls: TNyxJSONNullKeys): INyxFlowGraph;
begin
  Result := NewNyxFlowGraph
    .WithStep(NyxFlowStep('orders'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('customers'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('unique'), NewNyxJSONDistinct(NyxDataPath.Field('id'), njkText, ANulls))
    .WithStep(NyxFlowStep('join'), NewNyxJSONJoin(
      NyxDataPath.Field('customer'), NyxDataPath.Field('id'), njkText, njjInner, ANulls))
    .WithStep(NyxFlowStep('groups'), NewNyxJSONGroup(NyxDataPath.Field('right').Field('region'), njkText, ANulls))
    .WithStep(NyxFlowStep('total'), NewNyxJSONAggregate(NyxDataPath.Field('left').Field('amount'), njaSum, ARounding))
    .WithStep(NyxFlowStep('format-total'), NewNyxJSONFormat(njlCompact))
    .Connect(RelationEndpoint('customers', nlpJSON), RelationEndpoint('unique', nlpJSON))
    .Connect(RelationEndpoint('unique', nlpValue), RelationEndpoint('join', nlpRight))
    .Connect(RelationEndpoint('orders', nlpJSON), RelationEndpoint('join', nlpLeft))
    .Connect(RelationEndpoint('join', nlpValue), RelationEndpoint('groups', nlpJSON))
    .Connect(RelationEndpoint('join', nlpValue), RelationEndpoint('total', nlpJSON))
    .Connect(RelationEndpoint('total', nlpValue), RelationEndpoint('format-total', nlpJSON))
    .Build;
end;

function RelationOrderRows: TNyxDataRows;
begin
  Result := NyxDataRows.Identity(NyxDataPath.Field('left').Field('id'))
    .Text(NyxTextField('order'), NyxDataPath.Field('left').Field('id'))
    .Text(NyxTextField('customer'), NyxDataPath.Field('right').Field('name'))
    .DecimalText(NyxTextField('amount'), NyxDataPath.Field('left').Field('amount'));
end;

function RelationGroupRows: TNyxDataRows;
begin
  Result := NyxDataRows.Identity(NyxDataPath.Field('key'))
    .Text(NyxTextField('region'), NyxDataPath.Field('key'))
    .Integer(NyxIntegerField('count'), NyxDataPath.Field('count'));
end;

function RelationDocument(ARounding: TNyxDecimalRounding; ANulls: TNyxJSONNullKeys): TNyxDocument;
var
  LPage: INyxPage;
  LCard: INyxCard;
  LMemo: INyxMemo;
  LTable: INyxTable;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Sales data workbench';
    Result.State.SetValue(NyxTextState('orders'), RelationOrders);
    Result.State.SetValue(NyxTextState('customers'), RelationCustomers);
    Result.State.SetValue(NyxTextState('total'), 'Build the report to calculate an exact total.');
    Result.Collections.Define(NyxCollection('orders'), RelationOrderRows.Schema, []);
    Result.Collections.Define(NyxCollection('regions'), RelationGroupRows.Schema, []);
    Result.Flows.Define(NyxFlowRef('Sales'), RelationGraph(ARounding, ANulls));
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(20).Gap(12).Done;
    LPage.Add(NewNyxHeading('title').WithText('Connect your data'));
    LPage.Add(NewNyxLabel('intro').WithText(
      'Keep the first customer record, join orders, group by region and calculate an exact total.'));
    LCard := NewNyxCard('inputs');
    LMemo := NewNyxMemo('orders-memo').WithText('Orders JSON');
    LMemo.Configure.Height(100).Done;
    LMemo.Binds.Value(NyxTextState('orders')).Done;
    LCard.Add(LMemo);
    LMemo := NewNyxMemo('customers-memo').WithText('Customers JSON');
    LMemo.Configure.Height(100).Done;
    LMemo.Binds.Value(NyxTextState('customers')).Done;
    LCard.Add(LMemo);
    LCard.Add(NewNyxButton('run-button').WithText('Build sales report'));
    LPage.Add(LCard);
    LPage.Add(NewNyxHeading('result-title').WithText('Matched orders'));
    LTable := NewNyxTable('orders-table');
    LTable.Configure.Height(180).Done;
    LTable.Binds.Collection(NyxCollectionView(NyxCollection('orders'))
      .Column(NyxTextField('order'), 'Order')
      .Column(NyxTextField('customer'), 'Customer')
      .Column(NyxTextField('amount'), 'Exact amount')).Done;
    LPage.Add(LTable);
    LPage.Add(NewNyxHeading('groups-title').WithText('Orders by region'));
    LTable := NewNyxTable('regions-table');
    LTable.Configure.Height(140).Done;
    LTable.Binds.Collection(NyxCollectionView(NyxCollection('regions'))
      .Column(NyxTextField('region'), 'Region').Column(NyxIntegerField('count'), 'Orders')).Done;
    LPage.Add(LTable);
    LPage.Add(NewNyxLabel('total-caption').WithText('Exact matched-order total'));
    LPage.Add(NewNyxCode('total-code').Binds.Text(NyxTextState('total')).Done);
    Result.AddPage(LPage);
    Result.AddPage(NewNyxPage('other').Add(NewNyxHeading('away').WithText('Another view')));
    SetNyxFlowRoutes(Result.Find('run-button'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Build sales report'), NyxFlowRef('Sales'), ntClick, nfcmExclusive)
        .InputState(RelationEndpoint('orders', nlpText), NyxTextState('orders'))
        .InputState(RelationEndpoint('customers', nlpText), NyxTextState('customers'))
        .OutputRows(RelationEndpoint('join', nlpValue), NyxCollection('orders'), RelationOrderRows)
        .OutputRows(RelationEndpoint('groups', nlpValue), NyxCollection('regions'), RelationGroupRows)
        .OutputState(RelationEndpoint('format-total', nlpText), NyxTextState('total'))));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
