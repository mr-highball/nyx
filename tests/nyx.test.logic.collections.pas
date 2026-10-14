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


unit nyx.test.logic.collections;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.flow;

function CollectionLogicGraph: INyxFlowGraph;
function CollectionLogicDocument: TNyxDocument;
function CollectionLogicSource: TNyxText;

implementation

uses
  nyx.types, nyx.data, nyx.state, nyx.logic, nyx.logic.collections,
  nyx.flow.types, nyx.flow.declarations, nyx.test.tool.canvas, nyx.test.logic.fixture;

function CollectionLogicSource: TNyxText;
begin
  Result := '[{"active":true,"priority":9007199254740993,"profile":{"name":"Ada"}},' +
    '{"active":false,"priority":999,"profile":{"name":"Hidden"}},' +
    '{"active":true,"priority":9007199254740992,"profile":{"name":"Grace"}},' +
    '{"active":true,"priority":1,"profile":{"name":"Linus"}}]';
end;

function CollectionLogicGraph: INyxFlowGraph;
begin
  Result := NewNyxFlowGraph
    .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('filter'), NewNyxJSONFilter(NyxDataPath.Field('active'), njcEqual, NyxData(True)))
    .WithStep(NyxFlowStep('sort'), NewNyxJSONSort(NyxDataPath.Field('priority'), njkDecimal, njdDescending))
    .WithStep(NyxFlowStep('page'), NewNyxJSONSlice(0, 2))
    .WithStep(NyxFlowStep('map'), NewNyxJSONMap(NyxDataPath.Field('profile')))
    .WithStep(NyxFlowStep('pretty'), NewNyxJSONFormat(njlIndented, 2))
    .WithStep(NyxFlowStep('compact'), NewNyxJSONFormat(njlCompact))
    .WithStep(NyxFlowStep('summary'), NewNyxTextJoin(#10))
    .Connect(LogicEndpoint('parse', nlpJSON), LogicEndpoint('filter', nlpJSON))
    .Connect(LogicEndpoint('filter', nlpValue), LogicEndpoint('sort', nlpJSON))
    .Connect(LogicEndpoint('sort', nlpValue), LogicEndpoint('page', nlpJSON))
    .Connect(LogicEndpoint('page', nlpValue), LogicEndpoint('map', nlpJSON))
    .Connect(LogicEndpoint('map', nlpValue), LogicEndpoint('pretty', nlpJSON))
    .Connect(LogicEndpoint('map', nlpValue), LogicEndpoint('compact', nlpJSON))
    .Connect(LogicEndpoint('pretty', nlpText), LogicEndpoint('summary', nlpRight))
    .Build;
end;

function CollectionLogicDocument: TNyxDocument;
begin
  Result := ToolCanvasDocument;
  try
    Result.Title := 'JSON collection workshop';
    Result.State.SetValue(NyxTextState('source'), CollectionLogicSource);
    Result.Find('title').Configure.Text('Shape your data').Done;
    Result.Find('intro').Configure.Text(
      'Filter active people, sort exact priorities, take a page and select their profiles.').Done;
    Result.Flows.Define(NyxFlowRef('Profile'), CollectionLogicGraph);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
