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
unit nyx.test.compiled.runtime;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model;

const
  CompiledRuntimeTitle: TNyxText = 'A thoughtful plan';
  CompiledRuntimeMemo: TNyxText = 'Describe the next step';
  CompiledRuntimeStatus: TNyxText = 'Ready to record';

{ Independently owned English application with a reusable editor and two pages.
  The companion contains actual typed callback classes outside the managed views.
  It uses no hosted resources, external data or runtime reporting authority. }
function CompiledRuntimeDocument(const ADefault: TNyxText;
  out ASource: TNyxText): TNyxDocument;

implementation

uses nyx.types, nyx.controls, nyx.state, nyx.events, nyx.scheduler,
  nyx.callbacks, nyx.codegen, nyx.source, nyx.flow, nyx.flow.types,
  nyx.flow.declarations, nyx.flow.routes, nyx.flow.route.model, nyx.logic,
  nyx.diagram, nyx.diagram.content, nyx.diagram.declarations, nyx.canvas.geometry;

{ A single typed operation makes saved route execution observable independently
  of callback implementation and preserves the ordinary Logic port contract. }
function JoinEndpoint(APort: TNyxLogicPort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep('join'), NyxLogicPort(APort));
end;

function CompiledRuntimeDocument(const ADefault: TNyxText;
  out ASource: TNyxText): TNyxDocument;
var
  LEditor: INyxColumn;
  LInput: INyxInput;
  LMemo: INyxMemo;
  LButton: INyxButton;
  LHome: INyxPage;
  LReview: INyxPage;
  LLine: Integer;
  LRegion: TNyxHandlerSource;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := CompiledRuntimeTitle;
    Result.State.SetValue(NyxTextState('plan'), ADefault)
      .SetValue(NyxTextState('detail'), CompiledRuntimeMemo)
      .SetValue(NyxTextState('receipt'), CompiledRuntimeStatus)
      .SetValue(NyxTextState('joined'), 'No route yet')
      .SetValue(NyxIntegerState('records'), 0);
    Result.Flows.Define(NyxFlowRef('Join plan'), NewNyxFlowGraph
      .WithStep(NyxFlowStep('join'), NewNyxTextJoin(' ~ ')).Build);
    LEditor := NewNyxColumn('PlanEditor');
    LEditor.Configure.Padding(16).Gap(12).Done;
    LInput := NewNyxInput('plan-title').WithText('Plan title');
    LInput.Binds.Value(NyxTextState('plan'));
    LEditor.Add(LInput);
    LMemo := NewNyxMemo('plan-detail').WithText('Next step');
    LMemo.Configure.Height(80).MinimumHeight(0).Done;
    LMemo.Binds.Value(NyxTextState('detail'));
    LEditor.Add(LMemo);
    LButton := NewNyxButton('record-plan').WithText('Record plan');
    NyxCallbacks(LButton).OnClick.Policy(neSequential)
      .Add(NyxHandler('TRecordPlanCallback'), NyxCallbackID('record'))
      .Add(NyxHandler('TStampPlanCallback'), NyxCallbackID('stamp'));
    LEditor.Add(LButton);
    LButton := NewNyxButton('plan-receipt').WithText(CompiledRuntimeStatus);
    LButton.Configure.Enabled(False).Done;
    LButton.Binds.Text(NyxTextState('receipt'));
    LEditor.Add(LButton);
    LButton := NewNyxButton('join-plan').WithText('Join plan');
    SetNyxFlowRoutes(LButton.Node, NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Join editor values'), NyxFlowRef('Join plan'),
        ntClick, nfcmExclusive)
        .InputState(JoinEndpoint(nlpLeft), NyxTextState('plan'))
        .InputState(JoinEndpoint(nlpRight), NyxTextState('detail'))
        .OutputState(JoinEndpoint(nlpText), NyxTextState('joined'))));
    LEditor.Add(LButton);
    LButton := NewNyxButton('joined-plan').WithText('No route yet');
    LButton.Configure.Enabled(False).Done;
    LButton.Binds.Text(NyxTextState('joined'));
    LEditor.Add(LButton);
    Result.AddComponent(LEditor);
    { A saved diagram retains the original reusable partition during focus.
      This common dependency must not require an invented page merely to bind
      the reusable's ordinary callbacks and saved Logic routes. }
    Result.Diagrams.Define(NyxDiagramRef('Plan board'), NewNyxDiagram
      .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('Editor'), ndsRectangle,
        NyxCanvasBounds(0, 0, 360, 480))
        .WithContent(NyxDiagramComponent(NyxComponent('PlanEditor')))));
    LHome := NewNyxPage('compose').WithText('Compose');
    LHome.Add(NewNyxComponent('active-editor')
      .Configure.Component(NyxComponent('PlanEditor')).Done);
    Result.AddPage(LHome);
    LReview := NewNyxPage('review').WithText('Review');
    LButton := NewNyxButton('review-plan').WithText('Plan');
    LButton.Configure.Enabled(False).Done;
    LButton.Binds.Text(NyxTextState('plan'));
    LReview.Add(LButton);
    LButton := NewNyxButton('review-detail').WithText('Detail');
    LButton.Configure.Enabled(False).Done;
    LButton.Binds.Text(NyxTextState('detail'));
    LReview.Add(LButton);
    LButton := NewNyxButton('review-receipt').WithText(CompiledRuntimeStatus);
    LButton.Configure.Enabled(False).Done;
    LButton.Binds.Text(NyxTextState('receipt'));
    LReview.Add(LButton);
    Result.AddPage(LReview);
    Result.Validate;
    ASource := TNyxCodegen.Generate(Result);
    ASource := AddNyxHandlerStub(ASource, NyxHandler('TRecordPlanCallback'), ntClick, LLine);
    ASource := AddNyxHandlerStub(ASource, NyxHandler('TStampPlanCallback'), ntClick, LLine);
    ASource := EditNyxImport(ASource, nisImplementation, niaAdd, NyxPascalUnit('SysUtils'));
    LRegion := ReadNyxHandlerSource(ASource, NyxHandler('TRecordPlanCallback'));
    ASource := ReplaceNyxHandlerImplementation(ASource, LRegion.Handler, LRegion.Code,
      #10 + 'begin' + #10 +
      '  Context.State.SetValue(NyxIntegerState(''records''),' + #10 +
      '    Context.State.GetValue(NyxIntegerState(''records'')) + 1);' + #10 +
      '  Context.State.SetValue(NyxTextState(''receipt''),' + #10 +
      '    TNyxText(IntToStr(Context.State.GetValue(NyxIntegerState(''records'')))) +' + #10 +
      '    TNyxText('' | '') + Context.State.GetValue(NyxTextState(''plan'')) +' + #10 +
      '    TNyxText('' | '') + Context.State.GetValue(NyxTextState(''detail'')));' + #10 +
      'end;');
    LRegion := ReadNyxHandlerSource(ASource, NyxHandler('TStampPlanCallback'));
    ASource := ReplaceNyxHandlerImplementation(ASource, LRegion.Handler, LRegion.Code,
      #10 + 'begin' + #10 +
      '  Context.State.SetValue(NyxTextState(''receipt''),' + #10 +
      '    Context.State.GetValue(NyxTextState(''receipt'')) + TNyxText('' | stamped''));' + #10 +
      'end;');
  except
    Result.Free;
    raise;
  end;
end;

end.
