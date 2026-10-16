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
unit nyx.test.focused.context;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model;

const
  { Dedicated supplementary-text qualification, not a starter document. }
  FocusedUnicodeID = 'Focused 🌙';
  FocusedUnicodeCaption = 'A focused moon 🌙';

{ Return an independently owned English document. Retained diagrams require a
  reusable that owns a nested canvas, bound memo and menu. Its private graph can
  exercise page/reusable/descendant focus without HTTP or application factories. }
function FocusedContextDocument: TNyxDocument;

implementation

uses nyx.types, nyx.controls, nyx.state, nyx.diagram, nyx.diagram.content,
  nyx.diagram.declarations, nyx.canvas.types, nyx.canvas.geometry,
  nyx.canvas.declarations, nyx.root.types, nyx.menu.types, nyx.menu.declarations;

function FocusedContextDocument: TNyxDocument;
var
  LWrapper: INyxColumn;
  LMemo: INyxMemo;
  LAction: INyxButton;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Focused workspace';
    Result.State.SetValue(NyxTextState('reply'), 'Write a focused reply');
    Result.AddPage(NewNyxPage('home').Add(NewNyxLabel('home-title').WithText('Project home')));
    Result.AddPage(NewNyxPage('other-page').Add(NewNyxLabel('other-title').WithText('Another page')));
    Result.AddComponent(NewNyxLabel('Leaf').WithText('Nested canvas content'));
    Result.AddComponent(NewNyxLabel(FocusedUnicodeID).WithText(FocusedUnicodeCaption));
    LWrapper := NewNyxColumn('Wrapper');
    LWrapper.Configure.Gap(12).Padding(16).Done;
    LAction := NewNyxButton('wrapper-menu').WithText('Actions');
    LAction.Configure.Menu(NyxMenuRef('Wrapper actions')).Done;
    LWrapper.Add(LAction);
    LWrapper.Add(NewNyxLabel('wrapper-title').WithText('Focused form'));
    LMemo := NewNyxMemo('focused-memo').WithText('Reply');
    LMemo.Configure.Height(80).MinimumHeight(0).Done;
    LMemo.Binds.Value(NyxTextState('reply'));
    LWrapper.Add(LMemo);
    LAction := NewNyxButton('wrapper-action').WithText('Continue');
    LAction.Configure.PartName(NyxPart('action')).Done;
    LWrapper.Add(LAction);
    LWrapper.Add(NewNyxCanvas('nested-canvas').WithScene(NyxCanvasRef('Nested board')));
    Result.AddComponent(LWrapper);
    Result.AddComponent(NewNyxLabel('Unrelated').WithText('Unreferenced component'));
    Result.Menus.Define(NyxMenuRef('Wrapper actions'),
      NewNyxMenuDefinition(NyxReusableRoot('Wrapper'), NyxMenu('Wrapper actions'))
        .Action(NyxPart('action'), NyxMenuCommand('continue')));
    Result.Canvases.Define(NyxCanvasRef('Page library'), NewNyxCanvasDefinition
      .WithLayer(NyxCanvasLayer('Content')).WithItem(NyxCanvasItem('Retained page'),
        NyxPageRoot('other-page'), NyxCanvasPlacement(NyxCanvasLayer('Content'),
          NyxCanvasBounds(0, 0, 200, 80))));
    Result.Canvases.Define(NyxCanvasRef('Nested board'), NewNyxCanvasDefinition
      .WithLayer(NyxCanvasLayer('Content')).WithItem(NyxCanvasItem('Nested leaf'),
        NyxReusableRoot('Leaf'), NyxCanvasPlacement(NyxCanvasLayer('Content'),
          NyxCanvasBounds(0, 0, 180, 60))));
    Result.Diagrams.Define(NyxDiagramRef('Board'), NewNyxDiagram
      .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('Form'), ndsRectangle,
        NyxCanvasBounds(0, 0, 320, 300))
        .WithContent(NyxDiagramComponent(NyxComponent('Wrapper'))))
      .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('Moon'), ndsRectangle,
        NyxCanvasBounds(350, 0, 180, 80))
        .WithContent(NyxDiagramComponent(NyxComponent(FocusedUnicodeID)))));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
