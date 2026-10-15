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

unit nyx.test.programs.logic;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.programs.contract, nyx.flow.types;

{ Independently owned English application recipe. Ordinary event routes select a
  saved program; only runtime hosts resolve it. All controls use public Nyx APIs. }
function ProgramLogicDocument: TNyxDocument;
{ Reuse the same saved program and state routes inside a Tool Canvas item. }
function ProgramToolCanvasDocument: TNyxDocument;
{ Independent two-port schema shared by semantic authoring and actual consumers. }
function GreetingProgramContract: TNyxProgramContract;
{ Name one public port on the recipe's exact greeting step without wire strings. }
function ProgramLogicEndpoint(const APort: TNyxText): TNyxFlowEndpoint;

implementation

uses nyx.types, nyx.state, nyx.controls, nyx.flow, nyx.flow.routes,
  nyx.flow.route.model, nyx.flow.declarations, nyx.logic.programs,
  nyx.programs.types, nyx.programs.declarations, nyx.root.types,
  nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.declarations;

function GreetingProgramContract: TNyxProgramContract;
begin
  Result := NyxProgramContract
    .Input(NyxProgramSymbol('Name'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText));
end;

function ProgramLogicEndpoint(const APort: TNyxText): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep('greeting'), NyxFlowPort(APort));
end;

function ProgramLogicDocument: TNyxDocument;
var
  LPage: INyxPage;
  LInput: INyxInput;
  LCaption: INyxLabel;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Pascal greeting workshop';
    Result.State.SetValue(NyxTextState('name'), 'Ada');
    Result.State.SetValue(NyxTextState('caption'), 'Ready for a greeting');
    Result.Programs.Define(NyxProgramRef('Greeting'), NewNyxProgramDefinition(
      'begin Caption := ''Hello, '' + Name + ''!'' end.', GreetingProgramContract));
    Result.Flows.Define(NyxFlowRef('Greeting flow'), NewNyxFlowGraph
      .WithStep(NyxFlowStep('greeting'), NewNyxProgramLogic(
        NyxProgramRef('Greeting'), GreetingProgramContract)).Build);
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxHeading('title').WithText('Make a personal greeting'));
    LInput := NewNyxInput('name-input').WithText('Your name');
    LInput.Binds.Value(NyxTextState('name')).Done;
    LPage.Add(LInput);
    LPage.Add(NewNyxButton('greet-button').WithText('Greet'));
    LCaption := NewNyxLabel('caption-label');
    LCaption.Binds.Text(NyxTextState('caption')).Done;
    LPage.Add(LCaption);
    Result.AddPage(LPage);
    Result.AddPage(NewNyxPage('other').Add(
      NewNyxHeading('other-title').WithText('A fresh page generation')));
    SetNyxFlowRoutes(Result.Find('greet-button'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Greet'), NyxFlowRef('Greeting flow'),
        ntClick, nfcmAfterDispatch)
        .InputState(ProgramLogicEndpoint('Name'), NyxTextState('name'))
        .OutputState(ProgramLogicEndpoint('Caption'), NyxTextState('caption'))));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function ProgramToolCanvasDocument: TNyxDocument;
var
  LContent: INyxCard;
  LCanvas: INyxToolCanvas;
begin
  Result := ProgramLogicDocument;
  try
    LContent := NewNyxCard('greeting-template');
    LContent.Add(ExtractNyxControl(Result, NyxControl('name-input')));
    LContent.Add(ExtractNyxControl(Result, NyxControl('greet-button')));
    LContent.Add(ExtractNyxControl(Result, NyxControl('caption-label')));
    Result.AddComponent(LContent);
    Result.Canvases.Define(NyxCanvasRef('Greeting canvas'), NewNyxCanvasDefinition
      .WithCamera(NyxCanvasCamera(900, 600, NyxCanvasPoint(0, 0), 1))
      .WithLayer(NyxCanvasLayer('Controls'))
      .WithItem(NyxCanvasItem('Greeting'), NyxReusableRoot('greeting-template'),
        NyxCanvasPlacement(NyxCanvasLayer('Controls'), NyxCanvasBounds(20, 20, 480, 300))));
    LCanvas := NewNyxToolCanvas('program-tools').WithScene(NyxCanvasRef('Greeting canvas'));
    LCanvas.WorkspaceCanvas.Configure.Height(500).MinimumHeight(360).Done;
    Result.Pages[0].Add(LCanvas);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
