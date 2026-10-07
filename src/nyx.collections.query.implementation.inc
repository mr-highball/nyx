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

const
  CPredicateNames: array[TNyxQueryPredicateKind] of TNyxText =
    ('field', 'all', 'any', 'not');
  CComparisonNames: array[TNyxQueryComparison] of TNyxText =
    ('equal', 'notEqual', 'less', 'atMost', 'greater', 'atLeast',
     'contains', 'startsWith', 'endsWith');
  CTextComparisonNames: array[TNyxQueryTextComparison] of TNyxText =
    ('exact', 'asciiInsensitive');
  CDirectionNames: array[TNyxSortDirection] of TNyxText = ('ascending', 'descending');

type
  TQueryPredicate = class(TInterfacedObject, INyxCollectionPredicate)
  private
    FKind: TNyxQueryPredicateKind;
    FField: TNyxText;
    FFamily: TNyxStateKind;
    FComparison: TNyxQueryComparison;
    FTextComparison: TNyxQueryTextComparison;
    FExpected: TNyxStateValue;
    FChildren: array of INyxCollectionPredicate;
    FNodes: Integer;
    FDepth: Integer;
    { A scalar KMP needle is prepared once. Searches stream the row text without
      repeated string folding/allocation, and have linear worst-case cost. }
    FNeedle: array of Integer;
    FPrefix: array of Integer;
    procedure LeafOnly;
    procedure PrepareNeedle;
    function Search(const AText: TNyxText): Boolean;
  public
    constructor CreateLeaf(const AField: TNyxText; AFamily: TNyxStateKind;
      AComparison: TNyxQueryComparison; ATextComparison: TNyxQueryTextComparison;
      const AExpected: TNyxStateValue);
    constructor CreateBranch(AKind: TNyxQueryPredicateKind;
      const AChildren: array of INyxCollectionPredicate);
    function GetKind: TNyxQueryPredicateKind;
    function GetFieldName: TNyxText;
    function GetFieldKind: TNyxStateKind;
    function GetComparison: TNyxQueryComparison;
    function GetTextComparison: TNyxQueryTextComparison;
    function GetExpected: TNyxStateValue;
    function GetChildCount: Integer;
    function Child(AIndex: Integer): INyxCollectionPredicate;
    function GetNodeCount: Integer;
    function GetDepth: Integer;
    function AndAlso(const AOther: INyxCollectionPredicate): INyxCollectionPredicate;
    function OrElse(const AOther: INyxCollectionPredicate): INyxCollectionPredicate;
    function Negated: INyxCollectionPredicate;
    procedure Validate(const ASchema: TNyxCollectionSchema);
    function Matches(const AItem: TNyxCollectionItem): Boolean;
    function ToData: TNyxDataValue;
  end;

procedure QueryFailure(const AReason: TNyxText);
begin
  raise ENyxCollection.Create(AReason);
end;

procedure CheckQueryBytes(const AData: TNyxDataValue);
var
  LText: TNyxText;
  LIndex: Integer;
  LBytes: Integer;
  LScalar: Integer;
begin
  { Count the canonical UTF-8 wire representation on either target. Native
    Length counts UTF-8 bytes; browser Length counts UTF-16 code units. }
  LText := AData.ToJSON;
  LIndex := 1;
  LBytes := 0;
  while NyxNextScalar(LText, LIndex, LScalar) do
  begin
    case LScalar of
      0..$7F: Inc(LBytes);
      $80..$7FF: Inc(LBytes, 2);
      $800..$FFFF: Inc(LBytes, 3);
    else
      Inc(LBytes, 4);
    end;

    if LBytes > NyxMaximumQueryBytes then
    begin
      QueryFailure('Collection query exceeds its 65536-byte wire budget');
    end;
  end;
end;

procedure QueryFields(const AData: TNyxDataValue; const ANames: array of TNyxText);
var
  LIndex: Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> Length(ANames)) then
  begin
    QueryFailure('Invalid collection query descriptor members');
  end;
  for LIndex := 0 to High(ANames) do
  begin
    { Field refuses a missing member. Equal count then also excludes extras. }
    AData.Field(ANames[LIndex]);
  end;
end;

function ReadQueryKind(const AText: TNyxText): TNyxStateKind;
var
  LKind: TNyxStateKind;
begin
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin

    if NyxStateKindName(LKind) = AText then
    begin
      Exit(LKind);
    end;
  end;
  QueryFailure('Unknown collection query scalar family');
  Result := nskText;
end;

function ReadTextComparison(const AText: TNyxText): TNyxQueryTextComparison;
var
  LMode: TNyxQueryTextComparison;
begin
  for LMode := Low(TNyxQueryTextComparison) to High(TNyxQueryTextComparison) do
  begin

    if CTextComparisonNames[LMode] = AText then
    begin
      Exit(LMode);
    end;
  end;
  QueryFailure('Unknown collection query text comparison');
  Result := nqtExact;
end;

procedure CheckQueryFamily(AKind: TNyxStateKind;
  ATextComparison: TNyxQueryTextComparison);
begin

  if (Ord(AKind) < Ord(Low(TNyxStateKind))) or
    (Ord(AKind) > Ord(High(TNyxStateKind))) or
    (Ord(ATextComparison) < Ord(Low(TNyxQueryTextComparison))) or
    (Ord(ATextComparison) > Ord(High(TNyxQueryTextComparison))) then
  begin
    QueryFailure('Unknown collection query scalar choice');
  end;

  if (AKind <> nskText) and (ATextComparison <> nqtExact) then
  begin
    QueryFailure('Text comparison applies only to a text field');
  end;
end;

procedure ValidateQueryField(const ASchema: TNyxCollectionSchema;
  const AName: TNyxText; AKind: TNyxStateKind);
var
  LIndex: Integer;
  LField: TNyxCollectionField;
begin
  for LIndex := 0 to ASchema.Count - 1 do
  begin
    LField := ASchema.FieldAt(LIndex);

    if LField.Name = AName then
    begin

      if LField.Kind <> AKind then
      begin
        QueryFailure('Collection query field has a different scalar family: ' + AName);
      end;
      Exit;
    end;
  end;
  QueryFailure('Collection query field is not in the schema: ' + AName);
end;

function ReadQueryField(const AItem: TNyxCollectionItem;
  const AName: TNyxText; AKind: TNyxStateKind): TNyxStateValue;
var
  LIndex: Integer;
begin
  for LIndex := 0 to AItem.Count - 1 do
  begin

    if AItem.FieldName(LIndex) = AName then
    begin
      Result := AItem.FieldValue(LIndex);

      if Result.Kind <> AKind then
      begin
        QueryFailure('Collection query item has a different scalar family');
      end;
      Exit;
    end;
  end;
  QueryFailure('Collection query item is missing a field: ' + AName);
  Result := Default(TNyxStateValue);
end;

function FoldQueryScalar(AScalar: Integer;
  AMode: TNyxQueryTextComparison): Integer;
begin
  Result := AScalar;

  if (AMode = nqtAsciiInsensitive) and (AScalar >= Ord('A')) and
    (AScalar <= Ord('Z')) then
  begin
    Inc(Result, 32);
  end;
end;

function CompareQueryText(const ALeft, ARight: TNyxText;
  AMode: TNyxQueryTextComparison): Integer;
var
  LLeftIndex: Integer;
  LRightIndex: Integer;
  LLeft: Integer;
  LRight: Integer;
  LHasLeft: Boolean;
  LHasRight: Boolean;
begin
  LLeftIndex := 1;
  LRightIndex := 1;
  repeat
    LHasLeft := NyxNextScalar(ALeft, LLeftIndex, LLeft);
    LHasRight := NyxNextScalar(ARight, LRightIndex, LRight);

    if not LHasLeft or not LHasRight then
    begin
      Exit(Ord(LHasLeft) - Ord(LHasRight));
    end;
    LLeft := FoldQueryScalar(LLeft, AMode);
    LRight := FoldQueryScalar(LRight, AMode);

    if LLeft < LRight then
    begin
      Exit(-1);
    end;

    if LLeft > LRight then
    begin
      Exit(1);
    end;
  until False;
end;

function CompareQueryValues(const ALeft, ARight: TNyxStateValue;
  AKind: TNyxStateKind; AMode: TNyxQueryTextComparison): Integer;
var
  LLess: Boolean;
  LMore: Boolean;
begin

  if (ALeft.Kind <> AKind) or (ARight.Kind <> AKind) then
  begin
    QueryFailure('Collection query comparison requires its exact scalar family');
  end;
  case Ord(AKind) of
    Ord(nskText):
      begin
        Exit(CompareQueryText(ALeft.TextValue, ARight.TextValue, AMode));
      end;
    Ord(nskBoolean):
      begin
        Exit(Ord(ALeft.BooleanValue) - Ord(ARight.BooleanValue));
      end;
    Ord(nskInteger):
      begin
        LLess := ALeft.IntegerValue < ARight.IntegerValue;
        LMore := ALeft.IntegerValue > ARight.IntegerValue;
      end;
    Ord(nskNumber):
      begin
        LLess := ALeft.NumberValue < ARight.NumberValue;
        LMore := ALeft.NumberValue > ARight.NumberValue;
      end;
  else
    QueryFailure('Unknown collection query scalar family');
    Exit(0);
  end;
  Result := Ord(LMore) - Ord(LLess);
end;

constructor TQueryPredicate.CreateLeaf(const AField: TNyxText; AFamily: TNyxStateKind;
  AComparison: TNyxQueryComparison; ATextComparison: TNyxQueryTextComparison;
  const AExpected: TNyxStateValue);
begin
  inherited Create;
  CheckQueryFamily(AFamily, ATextComparison);
  AExpected.Validate;

  if (AExpected.Kind <> AFamily) or
    (Ord(AComparison) < Ord(Low(TNyxQueryComparison))) or
    (Ord(AComparison) > Ord(High(TNyxQueryComparison))) then
  begin
    QueryFailure('Invalid collection predicate comparison');
  end;

  if ((AFamily = nskText) and not (AComparison in
    [nqcEqual, nqcNotEqual, nqcContains, nqcStartsWith, nqcEndsWith])) or
    ((AFamily = nskBoolean) and not (AComparison in [nqcEqual, nqcNotEqual])) or
    ((AFamily in [nskInteger, nskNumber]) and (AComparison > nqcAtLeast)) then
  begin
    QueryFailure('Collection predicate comparison is not valid for its field family');
  end;
  { The open name still uses the public typed reference admission. }
  FField := NyxTextField(AField).Name;
  FKind := nqpField;
  FFamily := AFamily;
  FComparison := AComparison;
  FTextComparison := ATextComparison;
  FExpected := AExpected.Copy;
  FNodes := 1;
  FDepth := 1;
  CheckQueryBytes(ToData);

  if AComparison in [nqcContains, nqcStartsWith, nqcEndsWith] then
  begin
    PrepareNeedle;
  end;
end;

constructor TQueryPredicate.CreateBranch(AKind: TNyxQueryPredicateKind;
  const AChildren: array of INyxCollectionPredicate);
var
  LIndex: Integer;
begin
  inherited Create;

  if not (AKind in [nqpAll, nqpAny, nqpNot]) or
    ((AKind = nqpNot) and (Length(AChildren) <> 1)) or
    ((AKind <> nqpNot) and (Length(AChildren) <> 2)) then
  begin
    QueryFailure('Invalid collection predicate branch');
  end;
  FKind := AKind;
  FNodes := 1;
  FDepth := 1;
  SetLength(FChildren, Length(AChildren));
  for LIndex := 0 to High(AChildren) do
  begin

    if AChildren[LIndex] = nil then
    begin
      QueryFailure('A collection predicate branch requires a predicate');
    end;
    FChildren[LIndex] := AChildren[LIndex];
    Inc(FNodes, AChildren[LIndex].NodeCount);

    if AChildren[LIndex].Depth + 1 > FDepth then
    begin
      FDepth := AChildren[LIndex].Depth + 1;
    end;
  end;

  if (FNodes > NyxMaximumQueryPredicates) or (FDepth > NyxMaximumQueryDepth) then
  begin
    QueryFailure('Collection predicate exceeds its node or depth budget');
  end;
  CheckQueryBytes(ToData);
end;

procedure TQueryPredicate.PrepareNeedle;
var
  LIndex: Integer;
  LCount: Integer;
  LScalar: Integer;
  LMatched: Integer;
begin
  { Storage length is a safe upper bound for scalar count on both targets. }
  SetLength(FNeedle, Length(FExpected.TextValue));
  LIndex := 1;
  LCount := 0;
  while NyxNextScalar(FExpected.TextValue, LIndex, LScalar) do
  begin
    FNeedle[LCount] := FoldQueryScalar(LScalar, FTextComparison);
    Inc(LCount);
  end;
  SetLength(FNeedle, LCount);
  SetLength(FPrefix, LCount);
  LMatched := 0;
  for LIndex := 1 to LCount - 1 do
  begin
    while (LMatched > 0) and (FNeedle[LMatched] <> FNeedle[LIndex]) do
    begin
      LMatched := FPrefix[LMatched - 1];
    end;

    if FNeedle[LMatched] = FNeedle[LIndex] then
    begin
      Inc(LMatched);
    end;
    FPrefix[LIndex] := LMatched;
  end;
end;

function TQueryPredicate.Search(const AText: TNyxText): Boolean;
var
  LIndex: Integer;
  LMatched: Integer;
  LScalar: Integer;
  LAtEnd: Boolean;
begin

  if Length(FNeedle) = 0 then
  begin
    Exit(True);
  end;
  LIndex := 1;
  LMatched := 0;
  LAtEnd := False;
  while NyxNextScalar(AText, LIndex, LScalar) do
  begin
    LScalar := FoldQueryScalar(LScalar, FTextComparison);
    LAtEnd := False;

    if FComparison = nqcStartsWith then
    begin

      if LScalar <> FNeedle[LMatched] then
      begin
        Exit(False);
      end;
      Inc(LMatched);

      if LMatched = Length(FNeedle) then
      begin
        Exit(True);
      end;
    end
    else
    begin
      while (LMatched > 0) and (FNeedle[LMatched] <> LScalar) do
      begin
        LMatched := FPrefix[LMatched - 1];
      end;

      if FNeedle[LMatched] = LScalar then
      begin
        Inc(LMatched);
      end;

      if LMatched = Length(FNeedle) then
      begin

        if FComparison = nqcContains then
        begin
          Exit(True);
        end;
        LAtEnd := True;
        LMatched := FPrefix[LMatched - 1];
      end;
    end;
  end;
  Result := (FComparison = nqcEndsWith) and LAtEnd;
end;

procedure TQueryPredicate.LeafOnly;
begin

  if FKind <> nqpField then
  begin
    QueryFailure('Collection predicate branch has no field comparison');
  end;
end;

function TQueryPredicate.GetKind: TNyxQueryPredicateKind;
begin
  Result := FKind;
end;

function TQueryPredicate.GetFieldName: TNyxText;
begin
  LeafOnly;
  Result := FField;
end;

function TQueryPredicate.GetFieldKind: TNyxStateKind;
begin
  LeafOnly;
  Result := FFamily;
end;

function TQueryPredicate.GetComparison: TNyxQueryComparison;
begin
  LeafOnly;
  Result := FComparison;
end;

function TQueryPredicate.GetTextComparison: TNyxQueryTextComparison;
begin
  LeafOnly;
  Result := FTextComparison;
end;

function TQueryPredicate.GetExpected: TNyxStateValue;
begin
  LeafOnly;
  Result := FExpected.Copy;
end;

function TQueryPredicate.GetChildCount: Integer;
begin
  Result := Length(FChildren);
end;

function TQueryPredicate.Child(AIndex: Integer): INyxCollectionPredicate;
begin

  if (AIndex < 0) or (AIndex >= Length(FChildren)) then
  begin
    QueryFailure('Collection predicate child index is outside its range');
  end;
  Result := FChildren[AIndex];
end;

function TQueryPredicate.GetNodeCount: Integer;
begin
  Result := FNodes;
end;

function TQueryPredicate.GetDepth: Integer;
begin
  Result := FDepth;
end;

function TQueryPredicate.AndAlso(const AOther: INyxCollectionPredicate): INyxCollectionPredicate;
begin

  if AOther = nil then
  begin
    QueryFailure('AndAlso requires a collection predicate');
  end;
  Result := TQueryPredicate.CreateBranch(nqpAll,
    [Self as INyxCollectionPredicate, NyxPredicateFromData(AOther.ToData)]);
end;

function TQueryPredicate.OrElse(const AOther: INyxCollectionPredicate): INyxCollectionPredicate;
begin

  if AOther = nil then
  begin
    QueryFailure('OrElse requires a collection predicate');
  end;
  Result := TQueryPredicate.CreateBranch(nqpAny,
    [Self as INyxCollectionPredicate, NyxPredicateFromData(AOther.ToData)]);
end;

function TQueryPredicate.Negated: INyxCollectionPredicate;
begin
  Result := TQueryPredicate.CreateBranch(nqpNot, [Self as INyxCollectionPredicate]);
end;

procedure TQueryPredicate.Validate(const ASchema: TNyxCollectionSchema);
var
  LIndex: Integer;
begin

  if FKind = nqpField then
  begin
    ValidateQueryField(ASchema, FField, FFamily);
  end
  else
  begin
    for LIndex := 0 to High(FChildren) do
    begin
      FChildren[LIndex].Validate(ASchema);
    end;
  end;
end;

function TQueryPredicate.Matches(const AItem: TNyxCollectionItem): Boolean;
var
  LValue: TNyxStateValue;
  LOrder: Integer;
begin
  case FKind of
    nqpField:
      begin
        { Continue with the admitted leaf comparison below. }
      end;
    nqpAll: Exit(FChildren[0].Matches(AItem) and FChildren[1].Matches(AItem));
    nqpAny: Exit(FChildren[0].Matches(AItem) or FChildren[1].Matches(AItem));
    nqpNot: Exit(not FChildren[0].Matches(AItem));
  end;
  LValue := ReadQueryField(AItem, FField, FFamily);

  if FComparison in [nqcContains, nqcStartsWith, nqcEndsWith] then
  begin
    Exit(Search(LValue.TextValue));
  end;
  LOrder := CompareQueryValues(LValue, FExpected, FFamily, FTextComparison);
  case FComparison of
    nqcEqual: Result := LOrder = 0;
    nqcNotEqual: Result := LOrder <> 0;
    nqcLess: Result := LOrder < 0;
    nqcAtMost: Result := LOrder <= 0;
    nqcGreater: Result := LOrder > 0;
    nqcAtLeast: Result := LOrder >= 0;
  else
    QueryFailure('Unsupported collection predicate comparison');
    Result := False;
  end;
end;

function TQueryPredicate.ToData: TNyxDataValue;
var
  LValue: TNyxDataValue;
  LChildren: array of TNyxDataValue;
  LIndex: Integer;
begin

  if FKind <> nqpField then
  begin
    SetLength(LChildren, Length(FChildren));
    for LIndex := 0 to High(FChildren) do
    begin
      LChildren[LIndex] := FChildren[LIndex].ToData;
    end;
    Exit(NyxObject([NyxField('op', NyxData(CPredicateNames[FKind])),
      NyxField('children', NyxArray(LChildren))]));
  end;
  case Ord(FFamily) of
    Ord(nskText): LValue := NyxData(FExpected.TextValue);
    Ord(nskBoolean): LValue := NyxData(FExpected.BooleanValue);
    Ord(nskInteger): LValue := NyxData(FExpected.IntegerValue);
    Ord(nskNumber): LValue := NyxData(FExpected.NumberValue);
  else
    QueryFailure('Unknown collection predicate scalar family');
    LValue := NyxNull;
  end;
  Result := NyxObject([NyxField('op', NyxData('field')),
    NyxField('field', NyxData(FField)), NyxField('kind', NyxData(NyxStateKindName(FFamily))),
    NyxField('comparison', NyxData(CComparisonNames[FComparison])),
    NyxField('textComparison', NyxData(CTextComparisonNames[FTextComparison])),
    NyxField('value', LValue)]);
end;

function ParseQueryPredicate(const AData: TNyxDataValue; ADepth: Integer;
  var ACount: Integer): INyxCollectionPredicate;
var
  LKind: TNyxQueryPredicateKind;
  LFound: Boolean;
  LFamily: TNyxStateKind;
  LComparison: TNyxQueryComparison;
  LTextComparison: TNyxQueryTextComparison;
  LValue: TNyxStateValue;
  LChildren: array of INyxCollectionPredicate;
  LChildData: TNyxDataValue;
  LIndex: Integer;
begin
  Inc(ACount);

  if (ADepth > NyxMaximumQueryDepth) or (ACount > NyxMaximumQueryPredicates) then
  begin
    QueryFailure('Collection predicate exceeds its node or depth budget');
  end;
  LFound := False;
  LKind := nqpField;
  for LKind := Low(TNyxQueryPredicateKind) to High(TNyxQueryPredicateKind) do
  begin

    if AData.Field('op').AsText = CPredicateNames[LKind] then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    QueryFailure('Unknown collection predicate operator');
  end;

  if LKind <> nqpField then
  begin
    QueryFields(AData, ['op', 'children']);
    LChildData := AData.Field('children');

    if (LChildData.Kind <> ndArray) or
      ((LKind = nqpNot) and (LChildData.Count <> 1)) or
      ((LKind <> nqpNot) and (LChildData.Count <> 2)) then
    begin
      QueryFailure('Invalid collection predicate branch children');
    end;
    SetLength(LChildren, LChildData.Count);
    for LIndex := 0 to High(LChildren) do
    begin
      LChildren[LIndex] := ParseQueryPredicate(LChildData.Item(LIndex), ADepth + 1, ACount);
    end;
    Exit(TQueryPredicate.CreateBranch(LKind, LChildren));
  end;
  QueryFields(AData, ['op', 'field', 'kind', 'comparison', 'textComparison', 'value']);
  LFamily := ReadQueryKind(AData.Field('kind').AsText);
  LTextComparison := ReadTextComparison(AData.Field('textComparison').AsText);
  LFound := False;
  LComparison := nqcEqual;
  for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
  begin

    if AData.Field('comparison').AsText = CComparisonNames[LComparison] then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    QueryFailure('Unknown collection predicate comparison');
  end;
  case LFamily of
    nskText: LValue := TNyxStateValue.FromText(AData.Field('value').AsText);
    nskBoolean: LValue := TNyxStateValue.FromBoolean(AData.Field('value').AsBoolean);
    nskInteger: LValue := TNyxStateValue.FromInteger(AData.Field('value').AsInteger);
    nskNumber: LValue := TNyxStateValue.FromNumber(AData.Field('value').AsNumber);
  end;
  Result := TQueryPredicate.CreateLeaf(AData.Field('field').AsText,
    LFamily, LComparison, LTextComparison, LValue);
end;

function NyxPredicateFromData(const AData: TNyxDataValue): INyxCollectionPredicate;
var
  LCount: Integer;
begin
  CheckQueryBytes(AData);
  LCount := 0;
  Result := ParseQueryPredicate(AData, 1, LCount);
end;

function TNyxTextWhere.Compare(const AValue: TNyxText;
  AComparison: TNyxQueryComparison;
  ATextComparison: TNyxQueryTextComparison): INyxCollectionPredicate;
begin
  Result := TQueryPredicate.CreateLeaf(FField.Name, nskText, AComparison,
    ATextComparison, TNyxStateValue.FromText(AValue));
end;

function TNyxTextWhere.EqualTo(const AValue: TNyxText;
  AComparison: TNyxQueryTextComparison): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcEqual, AComparison);
end;

function TNyxTextWhere.NotEqualTo(const AValue: TNyxText;
  AComparison: TNyxQueryTextComparison): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcNotEqual, AComparison);
end;

function TNyxTextWhere.Contains(const AValue: TNyxText;
  AComparison: TNyxQueryTextComparison): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcContains, AComparison);
end;

function TNyxTextWhere.StartsWith(const AValue: TNyxText;
  AComparison: TNyxQueryTextComparison): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcStartsWith, AComparison);
end;

function TNyxTextWhere.EndsWith(const AValue: TNyxText;
  AComparison: TNyxQueryTextComparison): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcEndsWith, AComparison);
end;

function TNyxBooleanWhere.EqualTo(AValue: Boolean): INyxCollectionPredicate;
begin
  Result := TQueryPredicate.CreateLeaf(FField.Name, nskBoolean, nqcEqual,
    nqtExact, TNyxStateValue.FromBoolean(AValue));
end;

function TNyxBooleanWhere.NotEqualTo(AValue: Boolean): INyxCollectionPredicate;
begin
  Result := TQueryPredicate.CreateLeaf(FField.Name, nskBoolean, nqcNotEqual,
    nqtExact, TNyxStateValue.FromBoolean(AValue));
end;

function TNyxIntegerWhere.Compare(AValue: Integer;
  AComparison: TNyxQueryComparison): INyxCollectionPredicate;
begin
  Result := TQueryPredicate.CreateLeaf(FField.Name, nskInteger, AComparison,
    nqtExact, TNyxStateValue.FromInteger(AValue));
end;

function TNyxIntegerWhere.EqualTo(AValue: Integer): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcEqual);
end;

function TNyxIntegerWhere.NotEqualTo(AValue: Integer): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcNotEqual);
end;

function TNyxIntegerWhere.LessThan(AValue: Integer): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcLess);
end;

function TNyxIntegerWhere.AtMost(AValue: Integer): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcAtMost);
end;

function TNyxIntegerWhere.GreaterThan(AValue: Integer): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcGreater);
end;

function TNyxIntegerWhere.AtLeast(AValue: Integer): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcAtLeast);
end;

function TNyxNumberWhere.Compare(AValue: Double;
  AComparison: TNyxQueryComparison): INyxCollectionPredicate;
begin
  Result := TQueryPredicate.CreateLeaf(FField.Name, nskNumber, AComparison,
    nqtExact, TNyxStateValue.FromNumber(AValue));
end;

function TNyxNumberWhere.EqualTo(AValue: Double): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcEqual);
end;

function TNyxNumberWhere.NotEqualTo(AValue: Double): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcNotEqual);
end;

function TNyxNumberWhere.LessThan(AValue: Double): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcLess);
end;

function TNyxNumberWhere.AtMost(AValue: Double): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcAtMost);
end;

function TNyxNumberWhere.GreaterThan(AValue: Double): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcGreater);
end;

function TNyxNumberWhere.AtLeast(AValue: Double): INyxCollectionPredicate;
begin
  Result := Compare(AValue, nqcAtLeast);
end;

function NyxWhere(const AField: TNyxTextFieldRef): TNyxTextWhere;
begin
  Result.FField := NyxTextField(AField.Name);
end;

function NyxWhere(const AField: TNyxBooleanFieldRef): TNyxBooleanWhere;
begin
  Result.FField := NyxBooleanField(AField.Name);
end;

function NyxWhere(const AField: TNyxIntegerFieldRef): TNyxIntegerWhere;
begin
  Result.FField := NyxIntegerField(AField.Name);
end;

function NyxWhere(const AField: TNyxNumberFieldRef): TNyxNumberWhere;
begin
  Result.FField := NyxNumberField(AField.Name);
end;

function MakeQuerySort(const AField: TNyxText; AKind: TNyxStateKind;
  ADirection: TNyxSortDirection;
  ATextComparison: TNyxQueryTextComparison): TNyxCollectionSort;
begin
  Result := Default(TNyxCollectionSort);
  Result.FField := NyxTextField(AField).Name;
  Result.FKind := AKind;
  Result.FDirection := ADirection;
  Result.FTextComparison := ATextComparison;
  Result.FInitialized := True;
  Result.Check;
end;

procedure TNyxCollectionSort.Check;
begin

  if not FInitialized then
  begin
    QueryFailure('Collection sort requires a typed field');
  end;
  CheckQueryFamily(FKind, FTextComparison);

  if (Ord(FDirection) < Ord(Low(TNyxSortDirection))) or
    (Ord(FDirection) > Ord(High(TNyxSortDirection))) then
  begin
    QueryFailure('Unknown collection sort direction');
  end;
end;

function TNyxCollectionSort.Copy: TNyxCollectionSort;
begin
  Check;
  Result := MakeQuerySort(FField, FKind, FDirection, FTextComparison);
end;

procedure TNyxCollectionSort.Validate(const ASchema: TNyxCollectionSchema);
begin
  Check;
  ValidateQueryField(ASchema, FField, FKind);
end;

function TNyxCollectionSort.Read(const AItem: TNyxCollectionItem): TNyxStateValue;
begin
  Check;
  Result := ReadQueryField(AItem, FField, FKind);
end;

function TNyxCollectionSort.Compare(const ALeft, ARight: TNyxStateValue): Integer;
begin
  Check;
  Result := CompareQueryValues(ALeft, ARight, FKind, FTextComparison);

  if FDirection = nsdDescending then
  begin
    Result := -Result;
  end;
end;

function TNyxCollectionSort.ToData: TNyxDataValue;
begin
  Check;
  Result := NyxObject([NyxField('field', NyxData(FField)),
    NyxField('kind', NyxData(NyxStateKindName(FKind))),
    NyxField('direction', NyxData(CDirectionNames[FDirection])),
    NyxField('textComparison', NyxData(CTextComparisonNames[FTextComparison]))]);
end;

function NyxSort(const AField: TNyxTextFieldRef; ADirection: TNyxSortDirection;
  AComparison: TNyxQueryTextComparison): TNyxCollectionSort;
begin
  Result := MakeQuerySort(AField.Name, nskText, ADirection, AComparison);
end;

function NyxSort(const AField: TNyxBooleanFieldRef;
  ADirection: TNyxSortDirection): TNyxCollectionSort;
begin
  Result := MakeQuerySort(AField.Name, nskBoolean, ADirection, nqtExact);
end;

function NyxSort(const AField: TNyxIntegerFieldRef;
  ADirection: TNyxSortDirection): TNyxCollectionSort;
begin
  Result := MakeQuerySort(AField.Name, nskInteger, ADirection, nqtExact);
end;

function NyxSort(const AField: TNyxNumberFieldRef;
  ADirection: TNyxSortDirection): TNyxCollectionSort;
begin
  Result := MakeQuerySort(AField.Name, nskNumber, ADirection, nqtExact);
end;

function NyxCollectionQuery: TNyxCollectionQuery;
begin
  Result := Default(TNyxCollectionQuery);
end;

function TNyxCollectionQuery.GetDefined: Boolean;
begin
  Result := FFilterData.Defined or (Length(FSorts) > 0);
end;

function TNyxCollectionQuery.GetFilter: INyxCollectionPredicate;
begin
  Result := nil;

  if FFilterData.Defined then
  begin
    Result := NyxPredicateFromData(FFilterData);
  end;
end;

function TNyxCollectionQuery.GetSortCount: Integer;
begin
  Result := Length(FSorts);
end;

function TNyxCollectionQuery.Copy: TNyxCollectionQuery;
var
  LIndex: Integer;
begin
  Result := Default(TNyxCollectionQuery);

  if FFilterData.Defined then
  begin
    Result.FFilterData := FFilterData.Copy;
  end;
  SetLength(Result.FSorts, Length(FSorts));
  for LIndex := 0 to High(FSorts) do
  begin
    Result.FSorts[LIndex] := FSorts[LIndex].Copy;
  end;
end;

function TNyxCollectionQuery.Where(const APredicate: INyxCollectionPredicate): TNyxCollectionQuery;
begin

  if APredicate = nil then
  begin
    QueryFailure('Where requires a collection predicate; use WithoutFilter to clear');
  end;
  Result := Copy;
  Result.FFilterData := NyxPredicateFromData(APredicate.ToData).ToData;
  CheckQueryBytes(Result.ToData);
end;

function TNyxCollectionQuery.WithoutFilter: TNyxCollectionQuery;
begin
  Result := Copy;
  Result.FFilterData := Default(TNyxDataValue);
end;

function TNyxCollectionQuery.Unsorted: TNyxCollectionQuery;
begin
  Result := Copy;
  Result.FSorts := nil;
end;

function TNyxCollectionQuery.AddSort(const ASort: TNyxCollectionSort;
  APrimary: Boolean): TNyxCollectionQuery;
var
  LIndex: Integer;
begin
  ASort.Check;
  Result := Copy;

  if APrimary then
  begin
    Result.FSorts := nil;
  end
  else
  begin

    if Result.SortCount = 0 then
    begin
      QueryFailure('ThenBy requires a primary OrderBy');
    end;
  end;

  if Result.SortCount >= NyxMaximumQuerySorts then
  begin
    QueryFailure('Collection query supports at most eight sort fields');
  end;
  for LIndex := 0 to Result.SortCount - 1 do
  begin

    if Result.FSorts[LIndex].FieldName = ASort.FieldName then
    begin
      QueryFailure('Collection query sort field is already defined');
    end;
  end;
  LIndex := Result.SortCount;
  SetLength(Result.FSorts, LIndex + 1);
  Result.FSorts[LIndex] := ASort.Copy;
  CheckQueryBytes(Result.ToData);
end;

function TNyxCollectionQuery.OrderBy(const ASort: TNyxCollectionSort): TNyxCollectionQuery;
begin
  Result := AddSort(ASort, True);
end;

function TNyxCollectionQuery.OrderBy(const AField: TNyxTextFieldRef;
  ADirection: TNyxSortDirection; AComparison: TNyxQueryTextComparison): TNyxCollectionQuery;
begin
  Result := OrderBy(NyxSort(AField, ADirection, AComparison));
end;

function TNyxCollectionQuery.OrderBy(const AField: TNyxBooleanFieldRef;
  ADirection: TNyxSortDirection): TNyxCollectionQuery;
begin
  Result := OrderBy(NyxSort(AField, ADirection));
end;

function TNyxCollectionQuery.OrderBy(const AField: TNyxIntegerFieldRef;
  ADirection: TNyxSortDirection): TNyxCollectionQuery;
begin
  Result := OrderBy(NyxSort(AField, ADirection));
end;

function TNyxCollectionQuery.OrderBy(const AField: TNyxNumberFieldRef;
  ADirection: TNyxSortDirection): TNyxCollectionQuery;
begin
  Result := OrderBy(NyxSort(AField, ADirection));
end;

function TNyxCollectionQuery.ThenBy(const ASort: TNyxCollectionSort): TNyxCollectionQuery;
begin
  Result := AddSort(ASort, False);
end;

function TNyxCollectionQuery.ThenBy(const AField: TNyxTextFieldRef;
  ADirection: TNyxSortDirection; AComparison: TNyxQueryTextComparison): TNyxCollectionQuery;
begin
  Result := ThenBy(NyxSort(AField, ADirection, AComparison));
end;

function TNyxCollectionQuery.ThenBy(const AField: TNyxBooleanFieldRef;
  ADirection: TNyxSortDirection): TNyxCollectionQuery;
begin
  Result := ThenBy(NyxSort(AField, ADirection));
end;

function TNyxCollectionQuery.ThenBy(const AField: TNyxIntegerFieldRef;
  ADirection: TNyxSortDirection): TNyxCollectionQuery;
begin
  Result := ThenBy(NyxSort(AField, ADirection));
end;

function TNyxCollectionQuery.ThenBy(const AField: TNyxNumberFieldRef;
  ADirection: TNyxSortDirection): TNyxCollectionQuery;
begin
  Result := ThenBy(NyxSort(AField, ADirection));
end;

function TNyxCollectionQuery.SortAt(AIndex: Integer): TNyxCollectionSort;
begin

  if (AIndex < 0) or (AIndex >= SortCount) then
  begin
    QueryFailure('Collection query sort index is outside its range');
  end;
  Result := FSorts[AIndex].Copy;
end;

procedure TNyxCollectionQuery.Validate(const ASchema: TNyxCollectionSchema);
var
  LIndex: Integer;
begin

  if FFilterData.Defined then
  begin
    GetFilter.Validate(ASchema);
  end;
  for LIndex := 0 to High(FSorts) do
  begin
    FSorts[LIndex].Validate(ASchema);
  end;
end;

function TNyxCollectionQuery.ToData: TNyxDataValue;
var
  LFilter: TNyxDataValue;
  LSorts: array of TNyxDataValue;
  LIndex: Integer;
begin

  if not Defined then
  begin
    Exit(NyxNull);
  end;
  LFilter := NyxNull;

  if FFilterData.Defined then
  begin
    LFilter := FFilterData.Copy;
  end;
  SetLength(LSorts, SortCount);
  for LIndex := 0 to High(FSorts) do
  begin
    LSorts[LIndex] := FSorts[LIndex].ToData;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('filter', LFilter), NyxField('order', NyxArray(LSorts))]);
end;

class function TNyxCollectionQuery.FromData(const AData: TNyxDataValue): TNyxCollectionQuery;
var
  LOrder: TNyxDataValue;
  LSort: TNyxDataValue;
  LDirection: TNyxSortDirection;
  LIndex: Integer;
  LFound: Boolean;
  LValue: TNyxCollectionSort;
begin
  Result := NyxCollectionQuery;

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  CheckQueryBytes(AData);
  QueryFields(AData, ['version', 'filter', 'order']);

  if AData.Field('version').AsInteger <> 1 then
  begin
    QueryFailure('Unsupported collection query version');
  end;

  if AData.Field('filter').Kind <> ndNull then
  begin
    Result.FFilterData := NyxPredicateFromData(AData.Field('filter')).ToData;
  end;
  LOrder := AData.Field('order');

  if (LOrder.Kind <> ndArray) or (LOrder.Count > NyxMaximumQuerySorts) then
  begin
    QueryFailure('Collection query order requires at most eight sort fields');
  end;
  for LIndex := 0 to LOrder.Count - 1 do
  begin
    LSort := LOrder.Item(LIndex);
    QueryFields(LSort, ['field', 'kind', 'direction', 'textComparison']);
    LFound := False;
    LDirection := nsdAscending;
    for LDirection := Low(TNyxSortDirection) to High(TNyxSortDirection) do
    begin

      if LSort.Field('direction').AsText = CDirectionNames[LDirection] then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      QueryFailure('Unknown collection sort direction');
    end;
    LValue := MakeQuerySort(LSort.Field('field').AsText,
      ReadQueryKind(LSort.Field('kind').AsText), LDirection,
      ReadTextComparison(LSort.Field('textComparison').AsText));
    Result := Result.AddSort(LValue, LIndex = 0);
  end;

  if not Result.Defined then
  begin
    QueryFailure('An empty collection query must use null');
  end;
end;
