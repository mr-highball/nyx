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

{ Included by nyx.studio.agents.collections. Pages retain value-only child paths
  and bounded scalar previews, not recursive subtrees or unrelated binding/data.
  The accepted immutable policy is borrowed only for this call. No authority
  ticket is needed: mutation uses the ordinary exact-revision grouped boundary. }
type
  TNyxAgentQueryPreviewSection = (aqpSummary, aqpRows, aqpPascal);

{ Read-only candidate evaluation shares the public bounded preview engine.
  Responses contain only the requested section. No source/tree/store survives
  this call; the surrounding owner route supplies revision and exact context.
  Repeated pages re-evaluate immutable defaults at that same required revision. }
function CollectionQueryPreview(const ASource: INyxCollectionSnapshot;
  const ASpec: TNyxCollectionViewSpec; AProjection: TNyxCollectionProjection;
  const AArguments: TNyxDataValue): TNyxDataValue;
const
  CSections: array[TNyxAgentQueryPreviewSection] of TNyxText = ('summary', 'rows', 'pascal');
var
  LSection: TNyxAgentQueryPreviewSection;
  LFound: Boolean;
  LQuery: TNyxCollectionQuery;
  LVisible: INyxCollectionSnapshot;
  LFields: array of TNyxCollectionField;
  LRows: array of TNyxDataValue;
  LCells: array of TNyxDataValue;
  LFieldData: TNyxDataValue;
  LRow: TNyxCollectionItem;
  LIndex: Integer;
  LOther: Integer;
  LOffset: Integer;
  LLimit: Integer;
  LTotal: Integer;
  LText: TNyxText;
begin
  LFound := False;
  for LSection := Low(TNyxAgentQueryPreviewSection) to High(TNyxAgentQueryPreviewSection) do
  begin

    if CSections[LSection] = AArguments.Field('section').AsText then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxCollection.Create('Preview section must be summary, rows or pascal');
  end;
  case LSection of
    aqpSummary:
      NyxAgentFields(AArguments, '|mode|owner|source|expectedRevision|query|section|');
    aqpRows:
      NyxAgentFields(AArguments, '|mode|owner|source|expectedRevision|query|section|offset|limit|fields|');
    aqpPascal:
      NyxAgentFields(AArguments, '|mode|owner|source|expectedRevision|query|section|offset|count|');
  end;
  LQuery := TNyxCollectionQuery.FromData(AArguments.Field('query'));
  LVisible := EvaluateNyxCollectionQueryPreview(ASource, ASpec, AProjection, LQuery,
    TNyxQueryPreviewOptions.Defaults);
  Result := NyxObject([NyxField('key', NyxData(ASource.Key.Name)),
    NyxField('origin', NyxData('project-defaults')),
    NyxField('sourceRevision', NyxData(ASource.Revision)),
    NyxField('section', NyxData(CSections[LSection])),
    NyxField('sourceCount', NyxData(ASource.Count)),
    NyxField('visibleCount', NyxData(LVisible.Count)),
    NyxField('tree', NyxData(AProjection = cpTree))]);

  if LSection = aqpSummary then
  begin
    Exit;
  end;

  if LSection = aqpPascal then
  begin
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumCollectionBytes);
    LLimit := IntegerArgument(AArguments, 'count', 2048, 1, 4096);
    LText := TextSpan(TNyxCodegen.QueryExpression(LQuery), LOffset, LLimit, LTotal);

    if LOffset > LTotal then
    begin
      raise ENyxCollection.Create('Pascal offset is beyond the query expression');
    end;
    Result := WithResults(Result, NyxObject([NyxField('offset', NyxData(LOffset)),
      NyxField('nextOffset', NyxData(Min(LOffset + LLimit, LTotal))),
      NyxField('characters', NyxData(LTotal)), NyxField('text', NyxData(LText))]), 'pascal');
    Exit;
  end;
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 5000);
  LLimit := IntegerArgument(AArguments, 'limit', 8, 1, 20);
  LFields := nil;

  if NyxAgentHas(AArguments, 'fields') then
  begin
    LFieldData := AArguments.Field('fields');

    if (LFieldData.Kind <> ndArray) or (LFieldData.Count < 1) or (LFieldData.Count > 16) then
    begin
      raise ENyxCollection.Create('Preview rows select 1..16 exact fields');
    end;
    SetLength(LFields, LFieldData.Count);
    for LIndex := 0 to High(LFields) do
    begin
      LFields[LIndex] := CollectionField(ASource.Schema, LFieldData.Item(LIndex).AsText);
      for LOther := 0 to LIndex - 1 do
      begin

        if LFields[LOther].Name = LFields[LIndex].Name then
        begin
          raise ENyxCollection.Create('Duplicate preview row field');
        end;
      end;
    end;
  end;
  SetLength(LRows, Max(0, Min(LLimit, LVisible.Count - LOffset)));
  for LIndex := 0 to High(LRows) do
  begin
    LRow := LVisible.ItemAt(LOffset + LIndex);
    SetLength(LCells, Length(LFields));
    for LOther := 0 to High(LFields) do
    begin
      LCells[LOther] := NyxObject([NyxField('field', NyxData(LFields[LOther].Name)),
        NyxField('value', CollectionScalarPreview(CollectionCell(LRow, LFields[LOther])))]);
    end;
    LRows[LIndex] := NyxObject([NyxField('item', NyxData(LRow.Ref.ID)),
      NyxField('values', NyxArray(LCells))]);
  end;
  Result := WithResults(Result, NyxArray(LRows), 'rows');
  Result := WithResults(Result, NyxData(LOffset), 'offset');
  Result := WithResults(Result, NyxData(Min(LOffset + Length(LRows), LVisible.Count)), 'nextOffset');
end;

function CollectionQueryPage(const ASpec: TNyxCollectionViewSpec;
  const AArguments: TNyxDataValue): TNyxDataValue;
const
  COperators: array[TNyxQueryPredicateKind] of TNyxText = ('field', 'all', 'any', 'not');
  CComparisons: array[TNyxQueryComparison] of TNyxText =
    ('equal', 'notEqual', 'less', 'atMost', 'greater', 'atLeast',
     'contains', 'startsWith', 'endsWith');
  CMatches: array[TNyxQueryTextComparison] of TNyxText = ('exact', 'asciiInsensitive');
var
  LQuery: TNyxCollectionQuery;
  LFilter: INyxCollectionPredicate;
  LNodes: array of TNyxDataValue;
  LSorts: array of TNyxDataValue;
  LOffset: Integer;
  LLimit: Integer;
  LTotal: Integer;
  LIndex: Integer;
  LKey: TNyxText;

  procedure Visit(const APredicate: INyxCollectionPredicate;
    const APath: array of TNyxDataValue);
  var
    LNode: TNyxDataValue;
    LChildren: array of TNyxDataValue;
    LChildIndex: Integer;
    LPathIndex: Integer;
  begin

    if (LTotal >= LOffset) and (Length(LNodes) < LLimit) then
    begin
      LNode := NyxObject([NyxField('path', NyxArray(APath)),
        NyxField('op', NyxData(COperators[APredicate.Kind])),
        NyxField('children', NyxData(APredicate.ChildCount))]);

      if APredicate.Kind = nqpField then
      begin
        LNode := NyxObject([NyxField('path', NyxArray(APath)),
          NyxField('op', NyxData('field')), NyxField('children', NyxData(0)),
          NyxField('field', NyxData(APredicate.FieldName)),
          NyxField('kind', NyxData(NyxStateKindName(APredicate.FieldKind))),
          NyxField('comparison', NyxData(CComparisons[APredicate.Comparison])),
          NyxField('textComparison', NyxData(CMatches[APredicate.TextComparison])),
          NyxField('expected', CollectionScalarPreview(APredicate.Expected))]);
      end;
      SetLength(LNodes, Length(LNodes) + 1);
      LNodes[High(LNodes)] := LNode;
    end;
    Inc(LTotal);
    SetLength(LChildren, Length(APath) + 1);
    for LPathIndex := 0 to High(APath) do
    begin
      LChildren[LPathIndex] := APath[LPathIndex];
    end;
    for LChildIndex := 0 to APredicate.ChildCount - 1 do
    begin
      LChildren[High(LChildren)] := NyxData(LChildIndex);
      Visit(APredicate.Child(LChildIndex), LChildren);
    end;
  end;

begin
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumQueryPredicates);
  LLimit := IntegerArgument(AArguments, 'limit', 8, 1, 20);
  LQuery := ASpec.QueryPolicy;
  LKey := '';

  if ASpec.Defined then
  begin
    LKey := ASpec.Key.Name;
  end;
  LNodes := nil;
  LTotal := 0;
  LFilter := LQuery.Filter;

  if LFilter <> nil then
  begin
    Visit(LFilter, []);
  end;
  SetLength(LSorts, LQuery.SortCount);
  for LIndex := 0 to High(LSorts) do
  begin
    LSorts[LIndex] := LQuery.SortAt(LIndex).ToData;
  end;
  Result := NyxObject([NyxField('bound', NyxData(ASpec.Defined)),
    NyxField('key', NyxData(LKey)), NyxField('defined', NyxData(LQuery.Defined)),
    NyxField('offset', NyxData(LOffset)),
    NyxField('nextOffset', NyxData(Min(Min(LOffset, LTotal) + Length(LNodes), LTotal))),
    NyxField('total', NyxData(LTotal)), NyxField('nodes', NyxArray(LNodes)),
    NyxField('order', NyxArray(LSorts))]);
end;

{ Resolve one exact predicate child path without dumping its siblings. Unicode
  text uses the existing scalar-window contract; numeric/Boolean leaves remain
  exact primitives and refuse irrelevant offset/count arguments. }
function CollectionQueryValue(const ASpec: TNyxCollectionViewSpec;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LPredicate: INyxCollectionPredicate;
  LPath: TNyxDataValue;
  LIndex: Integer;
  LChild: Integer;
begin
  LPath := AArguments.Field('path');

  if (LPath.Kind <> ndArray) or (LPath.Count >= NyxMaximumQueryDepth) then
  begin
    raise ENyxCollection.Create('Query value requires a bounded predicate child path');
  end;
  LPredicate := ASpec.QueryPolicy.Filter;
  for LIndex := 0 to LPath.Count - 1 do
  begin
    LChild := LPath.Item(LIndex).AsInteger;

    if (LPredicate = nil) or (LChild < 0) or (LChild >= LPredicate.ChildCount) then
    begin
      raise ENyxCollection.Create('Exact predicate child path is missing');
    end;
    LPredicate := LPredicate.Child(LChild);
  end;

  if (LPredicate = nil) or (LPredicate.Kind <> nqpField) then
  begin
    raise ENyxCollection.Create('Query value requires an existing field predicate');
  end;
  Result := CollectionScalarWindow(LPredicate.Expected, AArguments);
end;
