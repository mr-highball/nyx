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
unit nyx.test.logic.fixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.flow.types, nyx.flow, nyx.logic;

function LogicFixture: INyxFlowGraph;
function LogicEndpoint(const AStep: TNyxText; APort: TNyxLogicPort): TNyxFlowEndpoint;

implementation

function LogicEndpoint(const AStep: TNyxText; APort: TNyxLogicPort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), NyxLogicPort(APort));
end;

function LogicFixture: INyxFlowGraph;
begin
  Result := NewNyxFlowGraph
    .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('select'), NewNyxJSONSelect(NyxDataPath.Field('profile')))
    .WithStep(NyxFlowStep('pretty'), NewNyxJSONFormat(njlIndented, 2))
    .WithStep(NyxFlowStep('compact'), NewNyxJSONFormat(njlCompact))
    .WithStep(NyxFlowStep('summary'), NewNyxTextJoin(#10))
    .Connect(LogicEndpoint('parse', nlpJSON), LogicEndpoint('select', nlpJSON))
    .Connect(LogicEndpoint('select', nlpValue), LogicEndpoint('pretty', nlpJSON))
    .Connect(LogicEndpoint('select', nlpValue), LogicEndpoint('compact', nlpJSON))
    .Connect(LogicEndpoint('pretty', nlpText), LogicEndpoint('summary', nlpRight))
    .Build;
end;

end.
