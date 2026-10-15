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


unit nyx.test.logic.regex;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.regex.types, nyx.regex.replacement, nyx.flow,
  nyx.flow.types, nyx.data.rows;

const
  RegexPeople: TNyxText = 'Ada Lovelace' + #10 + 'Grace Hopper';
  RegexNamePattern: TNyxText = '^(?<first>[\p{L}\p{M}]+)[ \t]+(?<last>[\p{L}\p{M}''-]+)$';

{ Independent English practical application recipes. Each document is owned by
  its caller. Runtime stores/collections never belong to these saved defaults. }
function RegexEndpoint(const AStep: TNyxText; const APort: TNyxFlowPortRef): TNyxFlowEndpoint;
function RegexOptions(AComparison: TNyxRegexCase = nrcUnicode): TNyxRegexOptions;
function RegexNameReplacement: TNyxRegexReplacement;
function RegexRows: TNyxDataRows;
function RegexGraph(AComparison: TNyxRegexCase = nrcUnicode): INyxFlowGraph;
function RegexDocument(AComparison: TNyxRegexCase = nrcUnicode): TNyxDocument;

implementation

uses nyx.types, nyx.state, nyx.controls, nyx.collections, nyx.collections.view.types,
  nyx.logic, nyx.logic.regex, nyx.flow.declarations, nyx.flow.routes, nyx.flow.route.model;

function RegexEndpoint(const AStep: TNyxText; const APort: TNyxFlowPortRef): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), APort);
end;

function RegexOptions(AComparison: TNyxRegexCase): TNyxRegexOptions;
begin
  Result := NyxRegex(RegexNamePattern).CaseMode(AComparison).Multiline(True);
end;

function RegexNameReplacement: TNyxRegexReplacement;
begin
  Result := NyxRegexReplacement.Capture(NyxRegexGroup('last')).Literal(', ')
    .Capture(NyxRegexGroup('first'));
end;

function RegexRows: TNyxDataRows;
begin
  Result := NyxDataRows.Identity(NyxDataPath.Field('id'))
    .Text(NyxTextField('name'), NyxDataPath.Field('value'))
    .Integer(NyxIntegerField('start'), NyxDataPath.Field('start'))
    .Integer(NyxIntegerField('length'), NyxDataPath.Field('length'));
end;

function RegexGraph(AComparison: TNyxRegexCase): INyxFlowGraph;
var
  LOptions: TNyxRegexOptions;
begin
  LOptions := RegexOptions(AComparison);
  Result := NewNyxFlowGraph
    .WithStep(NyxFlowStep('match'), NewNyxRegexMatch(LOptions))
    .WithStep(NyxFlowStep('replace'), NewNyxRegexReplace(LOptions, RegexNameReplacement, nruError))
    .WithStep(NyxFlowStep('extract'), NewNyxRegexExtract(LOptions,
      NyxRegexCapture(NyxRegexGroup('last')), nrxSkip))
    .WithStep(NyxFlowStep('format'), NewNyxJSONFormat(njlCompact))
    .Connect(RegexEndpoint('match', NyxRegexPort(nrpValue)),
      RegexEndpoint('format', NyxLogicPort(nlpJSON))).Build;
end;

function RegexDocument(AComparison: TNyxRegexCase): TNyxDocument;
var
  LPage: INyxPage;
  LMemo: INyxMemo;
  LTable: INyxTable;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Text transformation workbench';
    Result.State.SetValue(NyxTextState('people'), RegexPeople);
    Result.State.SetValue(NyxTextState('formatted'), 'Run the transformation to format the names.');
    Result.State.SetValue(NyxTextState('captures'), '[]');
    Result.Collections.Define(NyxCollection('names'), RegexRows.Schema, []);
    Result.Flows.Define(NyxFlowRef('Names'), RegexGraph(AComparison));
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(20).Gap(12).Done;
    LPage.Add(NewNyxHeading('title').WithText('Make text useful'));
    LPage.Add(NewNyxLabel('intro').WithText(
      'Find names, reuse named captures, format a mailing list and extract rows for a Table.'));
    LMemo := NewNyxMemo('people-memo').WithText('Names, one person per line');
    LMemo.Configure.Height(100).Done;
    LMemo.Binds.Value(NyxTextState('people')).Done;
    LPage.Add(LMemo);
    LPage.Add(NewNyxButton('run-button').WithText('Transform names'));
    LPage.Add(NewNyxHeading('formatted-title').WithText('Mailing list'));
    LPage.Add(NewNyxCode('formatted-code').Binds.Text(NyxTextState('formatted')).Done);
    LPage.Add(NewNyxHeading('rows-title').WithText('Extracted surnames'));
    LTable := NewNyxTable('names-table');
    LTable.Configure.Height(180).Done;
    LTable.Binds.Collection(NyxCollectionView(NyxCollection('names'))
      .Column(NyxTextField('name'), 'Surname')
      .Column(NyxIntegerField('start'), 'Position')
      .Column(NyxIntegerField('length'), 'Length')).Done;
    LPage.Add(LTable);
    Result.AddPage(LPage);
    Result.AddPage(NewNyxPage('other').Add(NewNyxHeading('away').WithText('Another view')));
    SetNyxFlowRoutes(Result.Find('run-button'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Transform names'), NyxFlowRef('Names'), ntClick, nfcmExclusive)
        .InputState(RegexEndpoint('match', NyxRegexPort(nrpInput)), NyxTextState('people'))
        .InputState(RegexEndpoint('replace', NyxRegexPort(nrpInput)), NyxTextState('people'))
        .InputState(RegexEndpoint('extract', NyxRegexPort(nrpInput)), NyxTextState('people'))
        .OutputState(RegexEndpoint('replace', NyxRegexPort(nrpValue)), NyxTextState('formatted'))
        .OutputState(RegexEndpoint('format', NyxLogicPort(nlpText)), NyxTextState('captures'))
        .OutputRows(RegexEndpoint('extract', NyxRegexPort(nrpValue)), NyxCollection('names'), RegexRows)));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
