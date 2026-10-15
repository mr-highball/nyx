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

unit nyx.test.files.openfixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model;

const
  OpenFixtureJSON: TNyxText =
    '{"title":"Evening notes","count":9007199254740993,"detail":"Hello 🌙\u0000world"}';
  OpenFixtureLeaf: TNyxText = 'report-🌙.json';
  OpenFixtureModified: Double = 1792036800123.0;

{ Independent ordinary UI and inert saved graph. Only a runtime resolver can
  supply read authority. Excluding the graph qualifies exact optional imports. }
function FileOpenDocument(AWithGraph: Boolean = True): TNyxDocument;

implementation

uses nyx.controls, nyx.state, nyx.types, nyx.flow, nyx.flow.types,
  nyx.flow.declarations, nyx.logic, nyx.logic.http, nyx.files.open,
  nyx.logic.files.open;

function FileOpenDocument(AWithGraph: Boolean): TNyxDocument;
var
  LPage: INyxPage;
  LCode: INyxCode;
  LLabel: INyxLabel;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Read a report';
    Result.State.SetValue(NyxTextState('pretty'), 'Choose a report to begin.');
    Result.State.SetValue(NyxTextState('read-name'), '');
    Result.State.SetValue(NyxTextState('read-status'), 'Ready');
    Result.State.SetValue(NyxTextState('metadata'), '');
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxHeading('title').WithText('Bring your data along'));
    LPage.Add(NewNyxLabel('intro').WithText(
      'Choose a file, read its exact bytes and inspect its portable metadata.'));
    LPage.Add(NewNyxButton('open-report').WithText('Open report'));
    LLabel := NewNyxLabel('read-status');
    LLabel.Binds.Text(NyxTextState('read-status')).Done;
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

    if AWithGraph then
    begin
      Result.Flows.Define(NyxFlowRef('Read report'), NewNyxFlowGraph
        .WithStep(NyxFlowStep('choose'), NewNyxOpenFile(
          NyxFileOpenOptions.Title('Choose a report').Allow('json').Allow('txt')
            .MaximumBytes(4096)))
        .WithStep(NyxFlowStep('decode'), NewNyxUTF8Decode)
        .Connect(NyxFlowEndpoint(NyxFlowStep('choose'), NyxOpenFilePort(nofpBytes)),
          NyxFlowEndpoint(NyxFlowStep('decode'), NyxHTTPPort(nhpBody)))
        .Build);
    end;
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
