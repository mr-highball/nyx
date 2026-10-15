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


unit nyx.test.flow.joins;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.flow, nyx.flow.types, nyx.logic.joins;

{ Shared English application recipe, independent of runtime/UI state. The caller
  owns each document; graph recipes retain only immutable operation options. }
function JoinEndpoint(const AStep, APort: TNyxText): TNyxFlowEndpoint;
function JoinGraph(ASelection: TNyxFlowJoinSelection = nfjExactlyOne;
  AEmpty: TNyxFlowJoinEmpty = nfjeSkip): INyxFlowGraph;
function JoinDocument(ASelection: TNyxFlowJoinSelection = nfjExactlyOne;
  AEmpty: TNyxFlowJoinEmpty = nfjeSkip): TNyxDocument;

implementation

uses nyx.types, nyx.state, nyx.controls, nyx.logic, nyx.flow.declarations,
  nyx.flow.routes, nyx.flow.route.model;

function JoinEndpoint(const AStep, APort: TNyxText): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), NyxFlowPort(APort));
end;

function JoinGraph(ASelection: TNyxFlowJoinSelection;
  AEmpty: TNyxFlowJoinEmpty): INyxFlowGraph;
var
  LBase: INyxFlowGraph;
begin
  { Register fallback before primary deliberately. Selection must follow the
    join's declared alternatives, independently of step/wire completion order. }
  LBase := NewNyxFlowGraph
    .WithStep(NyxFlowStep('primary-gate'), NewNyxFlowJoin(
      NyxFlowJoinOptions(nftBoolean).Input(NyxFlowPort('enabled'))))
    .WithStep(NyxFlowStep('fallback-gate'), NewNyxFlowJoin(
      NyxFlowJoinOptions(nftBoolean).Input(NyxFlowPort('enabled'))))
    .WithStep(NyxFlowStep('fallback'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('primary'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('choice'), NewNyxFlowJoin(NyxFlowJoinOptions(nftJSON)
      .Input(NyxFlowPort('primary')).Input(NyxFlowPort('fallback'))
      .Select(ASelection).WhenEmpty(AEmpty)))
    .WithStep(NyxFlowStep('format'), NewNyxJSONFormat(njlCompact))
    .Connect(JoinEndpoint('fallback', 'json'), JoinEndpoint('choice', 'fallback'))
    .Connect(JoinEndpoint('primary', 'json'), JoinEndpoint('choice', 'primary'))
    .Connect(JoinEndpoint('choice', 'value'), JoinEndpoint('format', 'json')).Build;
  Result := NyxFlowConditions(LBase)
    .When(NyxFlowStep('primary'), JoinEndpoint('primary-gate', 'value'))
    .When(NyxFlowStep('fallback'), JoinEndpoint('fallback-gate', 'value')).Build;
end;

function JoinDocument(ASelection: TNyxFlowJoinSelection;
  AEmpty: TNyxFlowJoinEmpty): TNyxDocument;
var
  LPage: INyxPage;
  LMemo: INyxMemo;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Branch workbench';
    Result.State.SetValue(NyxBooleanState('use-primary'), True);
    Result.State.SetValue(NyxBooleanState('use-fallback'), False);
    Result.State.SetValue(NyxTextState('primary'), '{"message":"Primary"}');
    Result.State.SetValue(NyxTextState('fallback'), '{"message":"Fallback"}');
    Result.State.SetValue(NyxTextState('result'), 'Run a branch to see its exact output.');
    Result.State.SetValue(NyxTextState('source'), 'No branch selected yet.');
    Result.Flows.Define(NyxFlowRef('Branches'), JoinGraph(ASelection, AEmpty));
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(20).Gap(12).Done;
    LPage.Add(NewNyxHeading('title').WithText('Choose your path'));
    LPage.Add(NewNyxLabel('help').WithText(
      'Enable one branch. Missing branches preserve the last result; conflicting branches report a problem.'));
    LPage.Add(NewNyxCheckbox('primary-toggle').WithText('Use primary branch')
      .Binds.Value(NyxBooleanState('use-primary')).Done);
    LMemo := NewNyxMemo('primary-memo').WithText('Primary JSON');
    LMemo.Configure.Height(80).Done;
    LMemo.Binds.Value(NyxTextState('primary')).Done;
    LPage.Add(LMemo);
    LPage.Add(NewNyxCheckbox('fallback-toggle').WithText('Use fallback branch')
      .Binds.Value(NyxBooleanState('use-fallback')).Done);
    LMemo := NewNyxMemo('fallback-memo').WithText('Fallback JSON');
    LMemo.Configure.Height(80).Done;
    LMemo.Binds.Value(NyxTextState('fallback')).Done;
    LPage.Add(LMemo);
    LPage.Add(NewNyxButton('run-button').WithText('Run selected branch'));
    LPage.Add(NewNyxHeading('result-title').WithText('Selected result'));
    LPage.Add(NewNyxCode('result-code').Binds.Text(NyxTextState('result')).Done);
    LPage.Add(NewNyxLabel('source-label').Binds.Text(NyxTextState('source')).Done);
    Result.AddPage(LPage);
    Result.AddPage(NewNyxPage('other').Add(NewNyxHeading('away').WithText('Another view')));
    SetNyxFlowRoutes(Result.Find('run-button'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Choose branch'), NyxFlowRef('Branches'), ntClick, nfcmExclusive)
        .InputState(JoinEndpoint('primary-gate', 'enabled'), NyxBooleanState('use-primary'))
        .InputState(JoinEndpoint('fallback-gate', 'enabled'), NyxBooleanState('use-fallback'))
        .InputState(JoinEndpoint('primary', 'text'), NyxTextState('primary'))
        .InputState(JoinEndpoint('fallback', 'text'), NyxTextState('fallback'))
        .OutputState(JoinEndpoint('format', 'text'), NyxTextState('result'))
        .OutputState(JoinEndpoint('choice', 'source'), NyxTextState('source'))));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
