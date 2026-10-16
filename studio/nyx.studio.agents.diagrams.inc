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
{ Bounded saved visual intent shares the ordinary outline/revision authority.
  Queries do not mount an editor, return a complete graph, invoke source or mutate
  history. Rendered previews and runtime state remain separate capabilities. }
function TNyxAgentSession.DiagramQuery(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LScope: TNyxText;
  LName: TNyxText;
  LMember: TNyxText;
  LQuery: TNyxText;
  LSearch: TNyxText;
  LOffset: Integer;
  LLimit: Integer;
  LIndex: Integer;
  LTotal: Integer;
  LCount: Integer;
  LAvailable: Integer;
  LReference: TNyxDiagramRef;
  LPlan: INyxDiagram;
  LValue: TNyxDataValue;
  LItems: array of TNyxDataValue;
begin
  NyxAgentFields(AArguments, '|scope|name|member|query|offset|limit|expectedRevision|');

  if NyxAgentHas(AArguments, 'expectedRevision') then
  begin
    RequireRevision(AArguments);
  end;
  LScope := TextArgument(AArguments, 'scope');
  LName := TextArgument(AArguments, 'name');
  LMember := TextArgument(AArguments, 'member');
  LQuery := TextArgument(AArguments, 'query');
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 100000);
  LLimit := IntegerArgument(AArguments, 'limit', 8, 1, 32);
  LCount := 0;
  LTotal := 0;
  LPlan := nil;
  SetLength(LItems, LLimit);

  if LScope = 'diagrams' then
  begin

    if LMember <> '' then
    begin
      raise ENyxModel.Create('A diagram member filter requires vertex or edge scope');
    end;
    LAvailable := FSession.Document.Diagrams.Count;
  end
  else
  begin
    LReference := NyxDiagramRef(LName);
    LPlan := FSession.Document.Diagrams.Definition(LReference);

    if LScope = 'diagram-vertices' then
    begin
      LAvailable := LPlan.VertexCount;
    end
    else if LScope = 'diagram-edges' then
    begin
      LAvailable := LPlan.EdgeCount;
    end
    else
    begin
      raise ENyxModel.Create('Unknown saved diagram outline scope');
    end;
  end;
  for LIndex := 0 to LAvailable - 1 do
  begin

    if LScope = 'diagrams' then
    begin
      LReference := FSession.Document.Diagrams.Reference(LIndex);

      if (LName <> '') and (LReference.Name <> LName) then
      begin
        Continue;
      end;
      LSearch := LReference.Name;

      if (LQuery <> '') and (Pos(LQuery, LSearch) = 0) then
      begin
        Continue;
      end;
      LPlan := FSession.Document.Diagrams.Definition(LReference);
      LValue := NyxObject([NyxField('name', NyxData(LReference.Name)),
        NyxField('vertices', NyxData(LPlan.VertexCount)),
        NyxField('edges', NyxData(LPlan.EdgeCount))]);
    end
    else
    begin

      if LScope = 'diagram-vertices' then
      begin
        LValue := LPlan.Vertex(LIndex).ToData;
        LSearch := LValue.Field('name').AsText + #10 + LValue.Field('caption').AsText;
      end
      else
      begin
        LValue := LPlan.Edge(LIndex).ToData;
        LSearch := LValue.Field('name').AsText;
      end;

      if (LMember <> '') and (LValue.Field('name').AsText <> LMember) then
      begin
        Continue;
      end;

      if (LQuery <> '') and (Pos(LQuery, LSearch) = 0) then
      begin
        Continue;
      end;
    end;

    if (LTotal >= LOffset) and (LCount < LLimit) then
    begin
      LItems[LCount] := LValue;
      Inc(LCount);
    end;
    Inc(LTotal);
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('scope', NyxData(LScope)), NyxField('name', NyxData(LName)),
    NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
    NyxField('items', NyxArray(LItems)), NyxField('savedOnly', NyxData(True))]);
end;
