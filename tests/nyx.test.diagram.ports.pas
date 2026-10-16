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
unit nyx.test.diagram.ports;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model;

{ Return an independently owned English source document with reusable input and
  named visual ports. State defaults remain authored; every runtime uses copies.
  No transport, file capability, target factory or UI receiver is installed. }
function DiagramPortsDocument: TNyxDocument;

implementation

uses nyx.types, nyx.controls, nyx.state, nyx.diagram, nyx.diagram.content,
  nyx.diagram.declarations, nyx.canvas.geometry;

function DiagramPortsDocument: TNyxDocument;
var
  LCard: INyxColumn;
  LMemo: INyxMemo;
  LPlan: INyxDiagram;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Connected workspace';
    Result.AddPage(NewNyxPage('home'));
    Result.State.SetValue(NyxTextState('draft'), 'Write something worth sharing');
    LCard := NewNyxColumn('Input card');
    LCard.Configure.Padding(12).Gap(10).Done;
    LCard.Add(NewNyxLabel('card-heading').WithText('A reusable form'));
    LMemo := NewNyxMemo('card-draft').WithText('Message');
    LMemo.Binds.Value(NyxTextState('draft'));
    LMemo.Configure.Height(90).MinimumHeight(0).Done;
    LCard.Add(LMemo);
    LCard.Add(NewNyxButton('card-send').WithText('Send'));
    Result.AddComponent(LCard);
    LPlan := NewNyxDiagram
      .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('Form'), ndsRectangle,
        NyxCanvasBounds(0, 0, 320, 260)).Captioned('Compose')
        .WithContent(NyxDiagramComponent(NyxComponent('Input card')).Padded(12).CaptionSpace(30))
        .WithPort(NyxDiagramPort(NyxDiagramPortRef('Message'), 1, 0.5)
          .Directed(ndaRight).Captioned('Message output')))
      .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('Destination'), ndsRectangle,
        NyxCanvasBounds(430, 0, 260, 180)).Captioned('Publish')
        .WithPort(NyxDiagramPort(NyxDiagramPortRef('Input'), 0, 0.5)
          .Directed(ndaLeft).Captioned('Message input')))
      .WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('Delivery'),
        NyxDiagramPortEnd(NyxDiagramVertexRef('Form'), NyxDiagramPortRef('Message')),
        NyxDiagramPortEnd(NyxDiagramVertexRef('Destination'), NyxDiagramPortRef('Input')))
        .Routed(ndrOrthogonal));
    Result.Diagrams.Define(NyxDiagramRef('Workspace'), LPlan);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
