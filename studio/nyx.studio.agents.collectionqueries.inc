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
