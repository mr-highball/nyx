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
{ Closed visual graph grammar invokes only public immutable constructors.
  Diagram plans cross the reader as snapshots; records contain no interfaces.
  This grants no renderer, script or project-source execution authority. }

function TConfigurationReader.DiagramConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
  LDiagram: INyxDiagram;
  LAttachment: TNyxDiagramAttachment;
  LRemoval: TNyxDiagramRemoval;

  procedure Count(AMinimum, AMaximum: Integer);
  begin

    if (Length(LArgs) < AMinimum) or (Length(LArgs) > AMaximum) then
    begin
      Fail('Wrong diagram argument count');
    end;
  end;

  procedure Kind(AIndex: Integer; AKind: TValueKind);
  begin

    if (AIndex >= Length(LArgs)) or (LArgs[AIndex].Kind <> AKind) then
    begin
      Fail('Wrong typed diagram argument family');
    end;
  end;

begin
  Result := Default(TValue);
  LArgs := nil;

  if At('(') then
  begin
    LArgs := Arguments(3);
  end;

  if AName = 'newnyxdiagram' then
  begin
    Count(0, 0);
    Result.Kind := vkDiagram;
    LDiagram := NewNyxDiagram;
  end
  else if (AName = 'nyxdiagramref') or (AName = 'nyxdiagramvertexref') or
    (AName = 'nyxdiagramedgeref') then
  begin
    Count(1, 1);
    Kind(0, vkText);
    Result.Text := LArgs[0].Text;

    if AName = 'nyxdiagramref' then
    begin
      Result.Kind := vkDiagramRef;
      Result.Text := NyxDiagramRef(Result.Text).Name;
    end
    else if AName = 'nyxdiagramvertexref' then
    begin
      Result.Kind := vkDiagramVertexRef;
      Result.Text := NyxDiagramVertexRef(Result.Text).Name;
    end
    else
    begin
      Result.Kind := vkDiagramEdgeRef;
      Result.Text := NyxDiagramEdgeRef(Result.Text).Name;
    end;
  end
  else if AName = 'nyxdiagramvertex' then
  begin
    Count(3, 3);
    Kind(0, vkDiagramVertexRef);
    Kind(1, vkDiagramShape);
    Kind(2, vkCanvasBounds);
    Result.Kind := vkDiagramVertex;
    Result.DiagramVertex := NyxDiagramVertex(NyxDiagramVertexRef(LArgs[0].Text),
      TNyxDiagramShape(LArgs[1].Ordinal), LArgs[2].CanvasBounds);
  end
  else if AName = 'nyxdiagramattached' then
  begin
    Count(1, 2);
    Kind(0, vkDiagramVertexRef);
    LAttachment := ndaAutomatic;

    if Length(LArgs) = 2 then
    begin
      Kind(1, vkDiagramAttachment);
      LAttachment := TNyxDiagramAttachment(LArgs[1].Ordinal);
    end;
    Result.Kind := vkDiagramEndpoint;
    Result.DiagramEndpoint := NyxDiagramAttached(NyxDiagramVertexRef(LArgs[0].Text),
      LAttachment);
  end
  else if AName = 'nyxdiagramfreeend' then
  begin
    Count(1, 1);
    Kind(0, vkCanvasPoint);
    Result.Kind := vkDiagramEndpoint;
    Result.DiagramEndpoint := NyxDiagramFreeEnd(LArgs[0].CanvasPoint);
  end
  else if AName = 'nyxdiagramedge' then
  begin
    Count(3, 3);
    Kind(0, vkDiagramEdgeRef);
    Kind(1, vkDiagramEndpoint);
    Kind(2, vkDiagramEndpoint);
    Result.Kind := vkDiagramEdge;
    Result.DiagramEdge := NyxDiagramEdge(NyxDiagramEdgeRef(LArgs[0].Text),
      LArgs[1].DiagramEndpoint, LArgs[2].DiagramEndpoint);
  end
  else
  begin
    Fail('Unsupported diagram constructor');
  end;

  while At('.') do
  begin
    Inc(FCursor);
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := nil;

    if At('(') then
    begin
      LArgs := Arguments(3);
    end;
    case Result.Kind of
      vkDiagramVertex:
        begin
          Count(1, 1);

          if LMethod = 'captioned' then
          begin
            Kind(0, vkText);
            Result.DiagramVertex := Result.DiagramVertex.Captioned(LArgs[0].Text);
          end
          else if LMethod = 'atbounds' then
          begin
            Kind(0, vkCanvasBounds);
            Result.DiagramVertex := Result.DiagramVertex.AtBounds(LArgs[0].CanvasBounds);
          end
          else if LMethod = 'styled' then
          begin
            Kind(0, vkDrawingStyle);
            Result.DiagramVertex := Result.DiagramVertex.Styled(
              TNyxDrawingStyle.FromData(LArgs[0].Data));
          end
          else
          begin
            Fail('Unsupported diagram vertex method');
          end;
        end;
      vkDiagramEdge:
        begin

          if LMethod = 'routed' then
          begin
            Count(1, 1);
            Kind(0, vkDiagramRouting);
            Result.DiagramEdge := Result.DiagramEdge.Routed(
              TNyxDiagramRouting(LArgs[0].Ordinal));
          end
          else if LMethod = 'arrows' then
          begin
            Count(2, 2);
            Kind(0, vkDiagramArrow);
            Kind(1, vkDiagramArrow);
            Result.DiagramEdge := Result.DiagramEdge.Arrows(
              TNyxDiagramArrow(LArgs[0].Ordinal), TNyxDiagramArrow(LArgs[1].Ordinal));
          end
          else if LMethod = 'styled' then
          begin
            Count(1, 1);
            Kind(0, vkDrawingStyle);
            Result.DiagramEdge := Result.DiagramEdge.Styled(
              TNyxDrawingStyle.FromData(LArgs[0].Data));
          end
          else
          begin
            Fail('Unsupported diagram edge method');
          end;
        end;
      vkDiagram:
        begin

          if LMethod = 'withvertex' then
          begin
            Count(1, 1);
            Kind(0, vkDiagramVertex);
            LDiagram := LDiagram.WithVertex(LArgs[0].DiagramVertex);
          end
          else if LMethod = 'withedge' then
          begin
            Count(1, 1);
            Kind(0, vkDiagramEdge);
            LDiagram := LDiagram.WithEdge(LArgs[0].DiagramEdge);
          end
          else if LMethod = 'withoutvertex' then
          begin
            Count(1, 2);
            Kind(0, vkDiagramVertexRef);
            LRemoval := ndrmRequireDisconnected;

            if Length(LArgs) = 2 then
            begin
              Kind(1, vkDiagramRemoval);
              LRemoval := TNyxDiagramRemoval(LArgs[1].Ordinal);
            end;
            LDiagram := LDiagram.WithoutVertex(NyxDiagramVertexRef(LArgs[0].Text), LRemoval);
          end
          else if LMethod = 'withoutedge' then
          begin
            Count(1, 1);
            Kind(0, vkDiagramEdgeRef);
            LDiagram := LDiagram.WithoutEdge(NyxDiagramEdgeRef(LArgs[0].Text));
          end
          else
          begin
            Fail('Unsupported diagram plan method');
          end;
        end;
    else
      Fail('Diagram reference and endpoint values have no fluent methods');
    end;
  end;

  if Result.Kind = vkDiagram then
  begin
    Result.Data := LDiagram.ToData;
  end;
end;

procedure TConfigurationReader.DiagramDefaults;
var
  LArgs: TValues;
  LMethod: TNyxText;
begin
  Inc(FCursor, 3);
  repeat
    Expect('.');
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := Arguments(2);

    if LMethod = 'define' then
    begin

      if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkDiagramRef) or
        (LArgs[1].Kind <> vkDiagram) then
      begin
        Fail('Diagrams.Define requires a distinct name and immutable visual graph');
      end;

      if FApply then
      begin
        FDocument.Diagrams.Define(NyxDiagramRef(LArgs[0].Text),
          NyxDiagramFromData(LArgs[1].Data));
      end;
    end
    else if LMethod = 'remove' then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkDiagramRef) then
      begin
        Fail('Diagrams.Remove requires a distinct diagram reference');
      end;

      if FApply then
      begin
        FDocument.Diagrams.Remove(NyxDiagramRef(LArgs[0].Text));
      end;
    end
    else
    begin
      Fail('Unsupported diagram registry method');
    end;
  until not At('.');
  Expect(';');
end;
