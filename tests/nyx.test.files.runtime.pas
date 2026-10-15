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

unit nyx.test.files.runtime;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model;

const
  RuntimeReportJSON: TNyxText =
    '{"title":"Evening notes","count":9007199254740993,"detail":"Hello 🌙\u0000world"}';
  RuntimeReportLeaf: TNyxText = 'evening-notes.json';

{ An independently owned ordinary application document. Saved routes/configuration
  contain no host authority; the caller configures its application separately. }
function FileRuntimeDocument: TNyxDocument;

implementation

uses nyx.controls, nyx.state, nyx.types, nyx.logic, nyx.logic.files,
  nyx.files.save, nyx.flow, nyx.flow.types, nyx.flow.declarations,
  nyx.flow.routes, nyx.flow.route.model;

function Endpoint(const AStep: TNyxText; APort: TNyxLogicPort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), NyxLogicPort(APort));
end;

function SavePort(APort: TNyxFilePort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep('save'), NyxFilePort(APort));
end;

function FileRuntimeDocument: TNyxDocument;
var
  LPage: INyxPage;
  LMemo: INyxMemo;
  LName: INyxLabel;
  LPretty: INyxCode;
  LGraph: INyxFlowGraph;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Keep a report';
    Result.State.SetValue(NyxTextState('source'), RuntimeReportJSON);
    Result.State.SetValue(NyxTextState('pretty'), '');
    Result.State.SetValue(NyxTextState('saved-name'), '');
    Result.State.SetValue(NyxTextState('save-error'), '');
    Result.State.SetValue(NyxIntegerState('save-outcome'), -1);
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxHeading('title').WithText('A report worth keeping'));
    LMemo := NewNyxMemo('report-source').WithText('JSON source');
    LMemo.Configure.Height(180).Done;
    LMemo.Binds.Value(NyxTextState('source')).Done;
    LPage.Add(LMemo);
    LPage.Add(NewNyxButton('save-report').WithText('Save formatted report'));
    LName := NewNyxLabel('saved-name');
    LName.Binds.Text(NyxTextState('saved-name')).Done;
    LPage.Add(LName);
    LPretty := NewNyxCode('pretty-report');
    LPretty.Configure.Height(180).Done;
    LPretty.Binds.Text(NyxTextState('pretty')).Done;
    LPage.Add(LPretty);
    Result.AddPage(LPage);
    Result.AddPage(NewNyxPage('other').Add(
      NewNyxHeading('other-title').WithText('Your report stays with this application')));
    LGraph := NewNyxFlowGraph
      .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
      .WithStep(NyxFlowStep('pretty'), NewNyxJSONFormat(njlIndented, 2))
      .WithStep(NyxFlowStep('encode'), NewNyxUTF8Encode)
      .WithStep(NyxFlowStep('save'), NewNyxSaveFile(
        NyxFileSaveOptions(RuntimeReportLeaf).Title('Save a Nyx report')
          .MediaType(NyxMediaType(nfmtJSON)).Allow('json').Mode(nfsmPreferPicker)))
      .Connect(Endpoint('parse', nlpJSON), Endpoint('pretty', nlpJSON))
      .Connect(Endpoint('pretty', nlpText), Endpoint('encode', nlpText))
      .Connect(NyxFlowEndpoint(NyxFlowStep('encode'), NyxFilePort(nfpBytes)), SavePort(nfpBytes))
      .Build;
    Result.Flows.Define(NyxFlowRef('Save report'), LGraph);
    SetNyxFlowRoutes(Result.Find('save-report'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Save formatted report'), NyxFlowRef('Save report'),
        ntClick, nfcmExclusive)
        .InputState(Endpoint('parse', nlpText), NyxTextState('source'))
        .InputSignal(SavePort(nfpTrigger))
        .OutputState(Endpoint('pretty', nlpText), NyxTextState('pretty'))
        .OutputState(SavePort(nfpName), NyxTextState('saved-name'))
        .OutputState(SavePort(nfpError), NyxTextState('save-error'))
        .OutputState(SavePort(nfpOutcome), NyxIntegerState('save-outcome'))));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
