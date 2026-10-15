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
unit nyx.test.logic.json.editing;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model, nyx.flow, nyx.flow.types, nyx.text, nyx.logic;

const
  JSONEditingInput: TNyxText =
    '{"profile":{"name":"Ada","obsolete":"retire"},"debug":true,"items":[1,2],"exact":9007199254740993}';
  JSONEditingPatch: TNyxText = '{"profile":{"obsolete":null},"items":[3]}';
  JSONEditingExpected: TNyxText =
    '{"profile":{"name":"Grace"},"items":[3],"exact":9007199254740993}';

function JSONEditingEndpoint(const AStep: TNyxText; APort: TNyxLogicPort): TNyxFlowEndpoint;
function JSONEditingGraph: INyxFlowGraph;
function JSONEditingDocument: TNyxDocument;

implementation

uses
  nyx.types, nyx.controls, nyx.state, nyx.logic.json.editing,
  nyx.flow.declarations, nyx.flow.routes, nyx.flow.route.model;

function JSONEditingEndpoint(const AStep: TNyxText; APort: TNyxLogicPort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), NyxLogicPort(APort));
end;

function JSONEditingGraph: INyxFlowGraph;
begin
  Result := NewNyxFlowGraph
    .WithStep(NyxFlowStep('source'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('patch'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('name'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('merge'), NewNyxJSONMerge(njmpMergePatch))
    .WithStep(NyxFlowStep('rename'), NewNyxJSONSet(NyxDataPath.Field('profile').Field('name'), njsReplace))
    .WithStep(NyxFlowStep('remove'), NewNyxJSONRemove(NyxDataPath.Field('debug'), njmRequire))
    .WithStep(NyxFlowStep('format'), NewNyxJSONFormat(njlCompact))
    .Connect(JSONEditingEndpoint('source', nlpJSON), JSONEditingEndpoint('merge', nlpLeft))
    .Connect(JSONEditingEndpoint('patch', nlpJSON), JSONEditingEndpoint('merge', nlpRight))
    .Connect(JSONEditingEndpoint('merge', nlpValue), JSONEditingEndpoint('rename', nlpJSON))
    .Connect(JSONEditingEndpoint('name', nlpJSON), JSONEditingEndpoint('rename', nlpRight))
    .Connect(JSONEditingEndpoint('rename', nlpValue), JSONEditingEndpoint('remove', nlpJSON))
    .Connect(JSONEditingEndpoint('remove', nlpValue), JSONEditingEndpoint('format', nlpJSON))
    .Build;
end;

function JSONEditingDocument: TNyxDocument;
var
  LPage: INyxPage;
  LCard: INyxCard;
  LMemo: INyxMemo;
  LCode: INyxCode;

  procedure Input(const AID, ACaption, AState: TNyxText);
  begin
    LMemo := NewNyxMemo(AID).WithText(ACaption);
    LMemo.Configure.Height(100).Done;
    LMemo.Binds.Value(NyxTextState(AState)).Done;
    LCard.Add(LMemo);
  end;

begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'JSON request builder';
    Result.State.SetValue(NyxTextState('source'), JSONEditingInput);
    Result.State.SetValue(NyxTextState('patch'), JSONEditingPatch);
    Result.State.SetValue(NyxTextState('name'), '"Grace"');
    Result.State.SetValue(NyxTextState('result'), 'Build your request to see the result.');
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxHeading('title').WithText('Shape a request'));
    LPage.Add(NewNyxLabel('intro').WithText(
      'Merge an override, replace a nested value, and remove private debug data through typed connections.'));
    LCard := NewNyxCard('inputs');
    Input('source-memo', 'Original JSON', 'source');
    Input('patch-memo', 'Merge Patch (null deletes an object member)', 'patch');
    Input('name-memo', 'Replacement name as JSON', 'name');
    LCard.Add(NewNyxButton('run-button').WithText('Build request'));
    LPage.Add(LCard);
    LCode := NewNyxCode('result-code');
    LCode.Configure.Height(180).Done;
    LCode.Binds.Text(NyxTextState('result')).Done;
    LPage.Add(LCode);
    Result.AddPage(LPage);
    Result.AddPage(NewNyxPage('other').Add(NewNyxHeading('other-title').WithText('Another view')));
    Result.Flows.Define(NyxFlowRef('Request'), JSONEditingGraph);
    SetNyxFlowRoutes(Result.Find('run-button'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Build request'), NyxFlowRef('Request'), ntClick, nfcmExclusive)
        .InputState(JSONEditingEndpoint('source', nlpText), NyxTextState('source'))
        .InputState(JSONEditingEndpoint('patch', nlpText), NyxTextState('patch'))
        .InputState(JSONEditingEndpoint('name', nlpText), NyxTextState('name'))
        .OutputState(JSONEditingEndpoint('format', nlpText), NyxTextState('result'))));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
