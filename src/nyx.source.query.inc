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

{ Query grammar invokes the same family-specific public contract as a compiled
  Pascal application. Lexical names remain at this explicit source boundary.
  No callback, expression evaluation or untyped property invocation is introduced. }
function TConfigurationReader.QueryConstructor(const AName: TNyxText): TValue;
const
  CComparisons: array[TNyxQueryComparison] of TNyxText =
    ('EqualTo', 'NotEqualTo', 'LessThan', 'AtMost', 'GreaterThan', 'AtLeast',
     'Contains', 'StartsWith', 'EndsWith');
var
  LArgs: TValues;
  LMethod: TNyxText;
  LSort: TNyxCollectionSort;

  function SortArgument(const AArgs: TValues): TNyxCollectionSort;
  var
    LDirection: TNyxSortDirection;
    LTextMode: TNyxQueryTextComparison;
  begin

    if (Length(AArgs) = 1) and (AArgs[0].Kind = vkCollectionSort) then
    begin
      Exit(AArgs[0].CollectionSort.Copy);
    end;

    if not (Length(AArgs) in [1, 2, 3]) or (AArgs[0].Kind <> vkCollectionField) then
    begin
      Fail('Ordering requires a typed field or NyxSort');
    end;
    LDirection := nsdAscending;
    LTextMode := nqtExact;

    if Length(AArgs) >= 2 then
    begin

      if AArgs[1].Kind <> vkSortDirection then
      begin
        Fail('Ordering direction requires a TNyxSortDirection');
      end;
      LDirection := TNyxSortDirection(AArgs[1].Ordinal);
    end;

    if Length(AArgs) = 3 then
    begin

      if (AArgs[0].Ordinal <> Ord(nskText)) or
        (AArgs[2].Kind <> vkQueryTextComparison) then
      begin
        Fail('Text ordering requires a TNyxQueryTextComparison');
      end;
      LTextMode := TNyxQueryTextComparison(AArgs[2].Ordinal);
    end;
    case TNyxStateKind(AArgs[0].Ordinal) of
      nskText: Result := NyxSort(NyxTextField(AArgs[0].Text), LDirection, LTextMode);
      nskBoolean: Result := NyxSort(NyxBooleanField(AArgs[0].Text), LDirection);
      nskInteger: Result := NyxSort(NyxIntegerField(AArgs[0].Text), LDirection);
      nskNumber: Result := NyxSort(NyxNumberField(AArgs[0].Text), LDirection);
    end;
  end;

  function PredicateArgument(const AField: TValue): INyxCollectionPredicate;
  var
    LFamily: TNyxStateKind;
    LComparison: TNyxQueryComparison;
    LMode: TNyxQueryTextComparison;
    LScalar: TNyxStateValue;
    LFound: Boolean;
    LText: TNyxTextWhere;
    LBoolean: TNyxBooleanWhere;
    LInteger: TNyxIntegerWhere;
    LNumber: TNyxNumberWhere;
  begin
    LFound := False;
    for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
    begin

      if SameText(LMethod, CComparisons[LComparison]) then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound or not (Length(LArgs) in [1, 2]) then
    begin
      Fail('NyxWhere requires a family-specific comparison');
    end;
    LFamily := TNyxStateKind(AField.Ordinal);
    LMode := nqtExact;

    if Length(LArgs) = 2 then
    begin

      if (LFamily <> nskText) or (LArgs[1].Kind <> vkQueryTextComparison) then
      begin
        Fail('Text predicates require a TNyxQueryTextComparison');
      end;
      LMode := TNyxQueryTextComparison(LArgs[1].Ordinal);
    end;
    LScalar := CollectionScalar(LArgs[0], LFamily);
    case LFamily of
      nskText:
        begin
          LText := NyxWhere(NyxTextField(AField.Text));
          case LComparison of
            nqcEqual: Exit(LText.EqualTo(LScalar.TextValue, LMode));
            nqcNotEqual: Exit(LText.NotEqualTo(LScalar.TextValue, LMode));
            nqcContains: Exit(LText.Contains(LScalar.TextValue, LMode));
            nqcStartsWith: Exit(LText.StartsWith(LScalar.TextValue, LMode));
            nqcEndsWith: Exit(LText.EndsWith(LScalar.TextValue, LMode));
          else
            Fail('Text fields require a text comparison');
          end;
        end;
      nskBoolean:
        begin
          LBoolean := NyxWhere(NyxBooleanField(AField.Text));
          case LComparison of
            nqcEqual: Exit(LBoolean.EqualTo(LScalar.BooleanValue));
            nqcNotEqual: Exit(LBoolean.NotEqualTo(LScalar.BooleanValue));
          else
            Fail('Boolean fields support EqualTo and NotEqualTo');
          end;
        end;
      nskInteger:
        begin
          LInteger := NyxWhere(NyxIntegerField(AField.Text));
          case LComparison of
            nqcEqual: Exit(LInteger.EqualTo(LScalar.IntegerValue));
            nqcNotEqual: Exit(LInteger.NotEqualTo(LScalar.IntegerValue));
            nqcLess: Exit(LInteger.LessThan(LScalar.IntegerValue));
            nqcAtMost: Exit(LInteger.AtMost(LScalar.IntegerValue));
            nqcGreater: Exit(LInteger.GreaterThan(LScalar.IntegerValue));
            nqcAtLeast: Exit(LInteger.AtLeast(LScalar.IntegerValue));
          else
            Fail('Integer fields require a numeric comparison');
          end;
        end;
      nskNumber:
        begin
          LNumber := NyxWhere(NyxNumberField(AField.Text));
          case LComparison of
            nqcEqual: Exit(LNumber.EqualTo(LScalar.NumberValue));
            nqcNotEqual: Exit(LNumber.NotEqualTo(LScalar.NumberValue));
            nqcLess: Exit(LNumber.LessThan(LScalar.NumberValue));
            nqcAtMost: Exit(LNumber.AtMost(LScalar.NumberValue));
            nqcGreater: Exit(LNumber.GreaterThan(LScalar.NumberValue));
            nqcAtLeast: Exit(LNumber.AtLeast(LScalar.NumberValue));
          else
            Fail('Number fields require a numeric comparison');
          end;
        end;
    end;
    Result := nil;
  end;
begin
  Result := Default(TValue);

  if SameText(AName, 'NyxCollectionQuery') then
  begin
    Result.Kind := vkCollectionQuery;
    Result.CollectionQuery := NyxCollectionQuery;
  end
  else
  begin
    LArgs := Arguments;

    if SameText(AName, 'NyxSort') then
    begin
      Result.Kind := vkCollectionSort;
      Result.CollectionSort := SortArgument(LArgs);
      Exit;
    end;

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkCollectionField) then
    begin
      Fail('NyxWhere requires a typed collection field');
    end;
    Result.Kind := vkQueryField;
    Result.Text := LArgs[0].Text;
    Result.Ordinal := LArgs[0].Ordinal;
  end;
  while At('.') do
  begin
    Expect('.');

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected a typed collection query method');
    end;
    LMethod := FTokens[FCursor].Text;
    Inc(FCursor);
    LArgs := nil;

    if At('(') then
    begin
      LArgs := Arguments;
    end;
    case Result.Kind of
      vkQueryField:
        begin
          Result.CollectionPredicateData := PredicateArgument(Result).ToData;
          Result.Kind := vkCollectionPredicate;
        end;
      vkCollectionPredicate:
        begin

          if SameText(LMethod, 'Negated') and (Length(LArgs) = 0) then
          begin
            Result.CollectionPredicateData := NyxPredicateFromData(
              Result.CollectionPredicateData).Negated.ToData;
          end
          else
          begin

            if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkCollectionPredicate) then
            begin
              Fail('AndAlso/OrElse require a typed predicate');
            end;

            if SameText(LMethod, 'AndAlso') then
            begin
              Result.CollectionPredicateData := NyxPredicateFromData(
                Result.CollectionPredicateData).AndAlso(NyxPredicateFromData(
                  LArgs[0].CollectionPredicateData)).ToData;
            end
            else if SameText(LMethod, 'OrElse') then
            begin
              Result.CollectionPredicateData := NyxPredicateFromData(
                Result.CollectionPredicateData).OrElse(NyxPredicateFromData(
                  LArgs[0].CollectionPredicateData)).ToData;
            end
            else
            begin
              Fail('Unsupported collection predicate method');
            end;
          end;
        end;
      vkCollectionQuery:
        begin

          if SameText(LMethod, 'Where') then
          begin

            if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkCollectionPredicate) then
            begin
              Fail('Where requires a typed collection predicate');
            end;
            Result.CollectionQuery := Result.CollectionQuery.Where(
              NyxPredicateFromData(LArgs[0].CollectionPredicateData));
          end
          else if SameText(LMethod, 'WithoutFilter') and (Length(LArgs) = 0) then
          begin
            Result.CollectionQuery := Result.CollectionQuery.WithoutFilter;
          end
          else if SameText(LMethod, 'Unsorted') and (Length(LArgs) = 0) then
          begin
            Result.CollectionQuery := Result.CollectionQuery.Unsorted;
          end
          else if SameText(LMethod, 'OrderBy') or SameText(LMethod, 'ThenBy') then
          begin
            LSort := SortArgument(LArgs);

            if SameText(LMethod, 'OrderBy') then
            begin
              Result.CollectionQuery := Result.CollectionQuery.OrderBy(LSort);
            end
            else
            begin
              Result.CollectionQuery := Result.CollectionQuery.ThenBy(LSort);
            end;
          end
          else
          begin
            Fail('Unsupported collection query method');
          end;
        end;
    else
      Fail('Unsupported collection query expression');
    end;
  end;

  if Result.Kind = vkQueryField then
  begin
    Fail('NyxWhere requires a comparison');
  end;
end;
