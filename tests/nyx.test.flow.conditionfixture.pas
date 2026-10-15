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

unit nyx.test.flow.conditionfixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model;

{ An ordinary saved event route performs opening, decoding, parsing, formatting
  and grouped publication. The constructor supplies no runtime file host and no
  handwritten event/publication callback. Each caller owns its independent tree. }
function ConditionalFileDocument: TNyxDocument;

implementation

uses nyx.text, nyx.types, nyx.controls, nyx.state, nyx.expressions,
  nyx.flow, nyx.flow.types, nyx.flow.routes, nyx.flow.route.model,
  nyx.flow.declarations, nyx.logic, nyx.logic.http, nyx.http.types,
  nyx.logic.files.open, nyx.files.open;

function Endpoint(const AStep: TNyxText; const APort: TNyxFlowPortRef): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), APort);
end;

function ConditionalFileDocument: TNyxDocument;
var
  LPage: INyxPage;
  LLabel: INyxLabel;
  LCode: INyxCode;
  LGraph: INyxFlowGraph;
  LRoute: TNyxFlowRoute;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'A report worth keeping';
    Result.State.SetValue(NyxTextState('pretty'), 'Choose a report to begin.')
      .SetValue(NyxTextState('read-name'), '')
      .SetValue(NyxTextState('read-status'), 'Ready')
      .SetValue(NyxTextState('metadata'), '')
      .SetValue(NyxBooleanState('read-complete'), False)
      .SetValue(NyxBooleanState('selection-cancelled'), False);
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxHeading('title').WithText('Your report, ready to explore'));
    LPage.Add(NewNyxLabel('intro').WithText(
      'Open a JSON report. Cancelling keeps your last report exactly where it was.'));
    LPage.Add(NewNyxButton('open-report').WithText('Open report'));
    LLabel := NewNyxLabel('read-status');
    LLabel.Binds.Text(NyxChoose(NyxBooleanExpression(NyxBooleanState('read-complete')),
      NyxTextExpression('Read complete'),
      NyxChoose(NyxBooleanExpression(NyxBooleanState('selection-cancelled')),
        NyxTextExpression('Selection cancelled'),
        NyxTextExpression(NyxTextState('read-status'))))).Done;
    LPage.Add(LLabel);
    LLabel := NewNyxLabel('read-name');
    LLabel.Binds.Text(NyxTextState('read-name')).Done;
    LPage.Add(LLabel);
    LCode := NewNyxCode('read-output');
    LCode.Configure.Height(230).Done;
    LCode.Binds.Text(NyxTextState('pretty')).Done;
    LPage.Add(LCode);
    LCode := NewNyxCode('read-metadata');
    LCode.Configure.Height(170).Done;
    LCode.Binds.Text(NyxTextState('metadata')).Done;
    LPage.Add(LCode);
    Result.AddPage(LPage);
    Result.AddPage(NewNyxPage('other').Add(NewNyxHeading('other-title')
      .WithText('The previous read belongs to its retired view')));

    LGraph := NewNyxFlowGraph
      .WithStep(NyxFlowStep('choose'), NewNyxOpenFile(
        NyxFileOpenOptions.Title('Choose a report').Allow('json').MaximumBytes(4096)))
      .WithStep(NyxFlowStep('decode'), NewNyxUTF8Decode)
      .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
      .WithStep(NyxFlowStep('pretty'), NewNyxJSONFormat)
      .WithStep(NyxFlowStep('metadata'), NewNyxJSONFormat)
      .WithStep(NyxFlowStep('name'), NewNyxTextJoin)
      .Connect(Endpoint('choose', NyxOpenFilePort(nofpBytes)),
        Endpoint('decode', NyxHTTPPort(nhpBody)))
      .Connect(Endpoint('decode', NyxLogicPort(nlpText)), Endpoint('parse', NyxLogicPort(nlpText)))
      .Connect(Endpoint('parse', NyxLogicPort(nlpJSON)), Endpoint('pretty', NyxLogicPort(nlpJSON)))
      .Connect(Endpoint('choose', NyxOpenFilePort(nofpMetadata)), Endpoint('metadata', NyxLogicPort(nlpJSON)))
      .Connect(Endpoint('choose', NyxOpenFilePort(nofpName)), Endpoint('name', NyxLogicPort(nlpLeft))).Build;
    LGraph := NyxFlowConditions(LGraph)
      .When(NyxFlowStep('decode'), Endpoint('choose', NyxOpenFilePort(nofpRead)))
      .When(NyxFlowStep('metadata'), Endpoint('choose', NyxOpenFilePort(nofpRead)))
      .When(NyxFlowStep('name'), Endpoint('choose', NyxOpenFilePort(nofpRead))).Build;
    Result.Flows.Define(NyxFlowRef('Read report'), LGraph);
    LRoute := NyxFlowRoute(NyxFlowRouteID('open-report'), NyxFlowRef('Read report'),
      ntClick, nfcmExclusive)
      .InputSignal(Endpoint('choose', NyxOpenFilePort(nofpTrigger)))
      .InputConstant(Endpoint('name', NyxLogicPort(nlpRight)), NyxFlowText(''))
      .OutputState(Endpoint('pretty', NyxLogicPort(nlpText)), NyxTextState('pretty'))
      .OutputState(Endpoint('metadata', NyxLogicPort(nlpText)), NyxTextState('metadata'))
      .OutputState(Endpoint('name', NyxLogicPort(nlpText)), NyxTextState('read-name'))
      .OutputState(Endpoint('choose', NyxOpenFilePort(nofpError)), NyxTextState('read-status'))
      .OutputState(Endpoint('choose', NyxOpenFilePort(nofpRead)), NyxBooleanState('read-complete'))
      .OutputState(Endpoint('choose', NyxOpenFilePort(nofpCancelled)),
        NyxBooleanState('selection-cancelled'));
    SetNyxFlowRoutes(Result.Find('open-report'), NewNyxFlowRoutes.WithRoute(LRoute));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
