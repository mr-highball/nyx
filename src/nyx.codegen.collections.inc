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

  { Immutable predicates are emitted as family-specific fluent expressions.
    Branch indentation follows the logical structure rather than serializing
    the wire descriptor into an opaque generated string. }
  function QueryPredicateExpression(const APredicate: INyxCollectionPredicate;
    const AIndent: TNyxText): TNyxText;
  const
    CFields: array[TNyxStateKind] of TNyxText =
      ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
    CMethods: array[TNyxQueryComparison] of TNyxText =
      ('EqualTo', 'NotEqualTo', 'LessThan', 'AtMost', 'GreaterThan', 'AtLeast',
       'Contains', 'StartsWith', 'EndsWith');
  var
    LValue: TNyxStateValue;
    LArgument: TNyxText;
    LMethod: TNyxText;
  begin

    if APredicate.Kind = nqpNot then
    begin
      Exit(QueryPredicateExpression(APredicate.Child(0), AIndent) + '.Negated');
    end;

    if APredicate.Kind in [nqpAll, nqpAny] then
    begin
      LMethod := 'AndAlso';

      if APredicate.Kind = nqpAny then
      begin
        LMethod := 'OrElse';
      end;
      Exit(QueryPredicateExpression(APredicate.Child(0), AIndent) + #10 + AIndent +
        '  .' + LMethod + '(' + #10 + AIndent + '    ' +
        QueryPredicateExpression(APredicate.Child(1), AIndent + '    ') + ')');
    end;
    LValue := APredicate.Expected;
    case LValue.Kind of
      nskText: LArgument := PascalString(LValue.TextValue);
      nskBoolean:
        begin
          LArgument := 'False';

          if LValue.BooleanValue then
          begin
            LArgument := 'True';
          end;
        end;
      nskInteger: LArgument := PascalInteger(LValue.IntegerValue);
      nskNumber:
        begin
          LArgument := LValue.NumberText;

          if (Pos('.', LArgument) = 0) and (Pos('e', LArgument) = 0) then
          begin
            LArgument := LArgument + '.0';
          end;
        end;
    end;

    if APredicate.TextComparison = nqtAsciiInsensitive then
    begin
      LArgument := LArgument + ', nqtAsciiInsensitive';
    end;
    Result := 'NyxWhere(' + CFields[APredicate.FieldKind] + '(' +
      PascalString(APredicate.FieldName) + ')).' +
      CMethods[APredicate.Comparison] + '(' + LArgument + ')';
  end;

  { Bindings use the public immutable fluent view object, with exact field
    families and enum choices. No wire property strings become authoring API. }
  procedure EmitCollectionBinding(ANode: TNyxNode; const AVariable: TNyxText);
  const
    CFields: array[TNyxStateKind] of TNyxText =
      ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
    CMatches: array[TNyxTypeAheadMatch] of TNyxText = ('ntmFolded', 'ntmExact');
  var
    LView: TNyxCollectionViewSpec;
    LColumn: TNyxCollectionColumn;
    LIndex: Integer;
    LLine: TNyxText;
    LQuery: TNyxCollectionQuery;
    LSort: TNyxCollectionSort;
  begin

    if not ANode.HasCollectionView then
    begin
      Exit;
    end;
    LView := ANode.CollectionView;

    if not LView.Defined then
    begin
      LLines.Add('      .ClearCollection');
      Exit;
    end;
    LLines.Add('      .Collection(');
    LLines.Add('        NyxCollectionView(NyxCollection(' + PascalString(LView.Key.Name) + '))');
    for LIndex := 0 to LView.Count - 1 do
    begin
      LColumn := LView.ColumnAt(LIndex);
      LLine := '          .Column(' + CFields[LColumn.Kind] + '(' +
        PascalString(LColumn.FieldName) + '), ' + PascalString(LColumn.Title);

      if LColumn.Mode = cmEditable then
      begin
        LLine := LLine + ', cmEditable';
      end;
      LLines.Add(LLine + ')');
    end;

    if LView.ParentField <> '' then
    begin
      LLines.Add('          .Parent(NyxTextField(' + PascalString(LView.ParentField) + '))');
    end;

    if LView.Scope = csInstance then
    begin
      LLines.Add('          .Scoped(csInstance)');
    end;

    if LView.SelectionMode = nsmMultiple then
    begin
      LLines.Add('          .Selection(nsmMultiple)');
    end;

    if LView.HasTypeAhead then
    begin
      { Saved behavior uses the same public fluent value as runtime authoring.
        No wire text, search prefix or timer state enters generated Pascal. }
      LLine := 'True';

      if not LView.TypeAheadPolicy.IsEnabled then
      begin
        LLine := 'False';
      end;
      LLines.Add('          .TypeAhead(NyxTypeAhead');
      LLines.Add('            .Enabled(' + LLine + ')');
      LLines.Add('            .WindowMilliseconds(' +
        IntToStr(LView.TypeAheadPolicy.WindowMS) + ')');
      LLines.Add('            .Match(' + CMatches[LView.TypeAheadPolicy.MatchMode] + '))');
    end;
    LQuery := LView.QueryPolicy;

    if LQuery.Defined then
    begin
      LLines.Add('          .Query(');
      LLines.Add('            NyxCollectionQuery');

      if LQuery.Filter <> nil then
      begin
        LLines.Add('              .Where(');
        LLines.Add('                ' + QueryPredicateExpression(LQuery.Filter, '                ') + ')');
      end;
      for LIndex := 0 to LQuery.SortCount - 1 do
      begin
        LSort := LQuery.SortAt(LIndex);
        LLine := '              .ThenBy(';

        if LIndex = 0 then
        begin
          LLine := '              .OrderBy(';
        end;
        LLine := LLine + CFields[LSort.Kind] + '(' + PascalString(LSort.FieldName) + ')';

        if (LSort.Direction = nsdDescending) or (LSort.TextComparison = nqtAsciiInsensitive) then
        begin

          if LSort.Direction = nsdDescending then
          begin
            LLine := LLine + ', nsdDescending';
          end
          else
          begin
            LLine := LLine + ', nsdAscending';
          end;
        end;

        if LSort.TextComparison = nqtAsciiInsensitive then
        begin
          LLine := LLine + ', nqtAsciiInsensitive';
        end;
        LLines.Add(LLine + ')');
      end;
      LLines[LLines.Count - 1] := LLines[LLines.Count - 1] + ')';
    end;
    LLines[LLines.Count - 1] := LLines[LLines.Count - 1] + ')';
  end;

  { Emit typed collection construction rather than opaque JSON or property keys.
    Defaults remain one admitted ordered packet. Only values differing from the
    schema default are spelled again in rows; normalization materializes the
    rest in schema order. Existing typed domain expressions retain their exact
    family/choices/range, including unconstrained family domains. }

  function ResourcePathExpression(const APath: TNyxResourcePath;
    const AHead: TNyxText): TNyxText;
  var
    LSteps: TNyxDataValue;
    LStep: TNyxDataValue;
    LIndex: Integer;
  begin
    Result := AHead;
    LSteps := APath.ToData;
    for LIndex := 0 to LSteps.Count - 1 do
    begin
      LStep := LSteps.Item(LIndex);

      if LStep.Kind = ndText then
      begin
        Result := Result + '.Field(' + PascalString(LStep.AsText) + ')';
      end
      else
      begin
        Result := Result + '.Item(' + IntToStr(LStep.AsInteger) + ')';
      end;
    end;
  end;

  procedure EmitResourceCollection(const AKey: TNyxCollectionRef;
    const ASource: TNyxResourceRows);
  const
    CMethods: array[TNyxStateKind] of TNyxText = ('Text', 'Boolean', 'Integer', 'Number');
    CFields: array[TNyxStateKind] of TNyxText =
      ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
  var
    LIndex: Integer;
    LField: TNyxCollectionField;
    LLine: TNyxText;
  begin
    LLines.Add('    Result.ResourceCollections.Define(');
    LLines.Add('      NyxCollection(' + PascalString(AKey.Name) + '),');
    LLines.Add('      ' + ResourcePathExpression(ASource.Path,
      'NyxResourceRows(NyxResourceRef(' + PascalString(ASource.Reference.Name) + '))'));
    LLines.Add('        .Identity(' + ResourcePathExpression(ASource.IdentityPath, 'NyxResourcePath') + ')');
    for LIndex := 0 to ASource.Schema.Count - 1 do
    begin
      LField := ASource.Schema.FieldAt(LIndex);
      LLine := '        .' + CMethods[LField.Kind] + '(' + CFields[LField.Kind] + '(' +
        PascalString(LField.Name) + ')';
      { The public shorthand selects the exact field name. Spell a structural
        path only when it contributes meaning to the crafted source. }

      if ASource.FieldPath(LIndex).ToData.ToJSON <>
        NyxResourcePath.Field(LField.Name).ToData.ToJSON then
      begin
        LLine := LLine + ', ' + ResourcePathExpression(ASource.FieldPath(LIndex), 'NyxResourcePath');
      end;
      LLines.Add(LLine + ')');
    end;
    LLines.Add('    );');
  end;

  procedure EmitCollections;
  const
    CMethods: array[TNyxStateKind] of TNyxText =
      ('Text', 'Boolean', 'Integer', 'Number');
    CFields: array[TNyxStateKind] of TNyxText =
      ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
  var
    LCollectionIndex: Integer;
    LFieldIndex: Integer;
    LRowIndex: Integer;
    LDefinition: INyxCollectionSnapshot;
    LSchema: TNyxCollectionSchema;
    LField: TNyxCollectionField;
    LRow: TNyxCollectionItem;
    LLine: TNyxText;
    LKey: TNyxText;
    LSource: TNyxResourceRows;

    function Literal(const AValue: TNyxStateValue): TNyxText;
    begin
      case AValue.Kind of
        nskText:
          begin
            Result := PascalString(AValue.TextValue);
          end;
        nskBoolean:
          begin
            Result := 'False';

            if AValue.BooleanValue then
            begin
              Result := 'True';
            end;
          end;
        nskInteger:
          begin
            Result := PascalInteger(AValue.IntegerValue);
          end;
        nskNumber:
          begin
            Result := AValue.NumberText;

            if (Pos('.', Result) = 0) and (Pos('e', Result) = 0) then
            begin
              Result := Result + '.0';
            end;
          end;
      end;
    end;

  begin

    if ADocument.Collections.Count = 0 then
    begin
      Exit;
    end;
    LLines.Add('');
    LLines.Add('    // Authored collection defaults; each application owns independent runtime stores.');
    for LCollectionIndex := 0 to ADocument.Collections.Count - 1 do
    begin
      LDefinition := ADocument.Collections.Snapshot(ADocument.Collections.Key(LCollectionIndex));

      if NyxCollectionResourceSource(ADocument.Collections, LDefinition.Key, LSource) then
      begin
        EmitResourceCollection(LDefinition.Key, LSource);
        Continue;
      end;
      LSchema := LDefinition.Schema;
      LKey := 'NyxCollection(' + PascalString(LDefinition.Key.Name) + ')';
      LLines.Add('    Result.Collections.Define(');
      LLines.Add('      ' + LKey + ',');
      LLine := '      NyxCollectionSchema';

      if LSchema.Count = 0 then
      begin
        LLine := LLine + ',';
      end;
      LLines.Add(LLine);
      for LFieldIndex := 0 to LSchema.Count - 1 do
      begin
        LField := LSchema.FieldAt(LFieldIndex);

        if LField.Domain.Defined then
        begin
          LLine := '        .' + CMethods[LField.Kind] + '(' + CFields[LField.Kind] +
            '(' + PascalString(LField.Name) + '), ' + Literal(LField.DefaultValue) +
            ', ' + DomainExpression(LField.Domain) + ')';
        end
        else
        begin
          { Explicit descriptor absence has a typed default and no constraint;
            it cannot become an unconstrained but Defined domain silently. }
          LLine := '        .Field(' + PascalString(LField.Name) +
            ', TNyxStateValue.From' + CMethods[LField.Kind] +
            '(' + Literal(LField.DefaultValue) + '), NyxNoDomain)';
        end;

        if LFieldIndex = LSchema.Count - 1 then
        begin
          LLine := LLine + ',';
        end;
        LLines.Add(LLine);
      end;
      LLines.Add('      [');
      for LRowIndex := 0 to LDefinition.Count - 1 do
      begin
        LRow := LDefinition.ItemAt(LRowIndex);
        LLine := '        NyxCollectionItem(NyxItem(' + LKey + ', ' + PascalString(LRow.Ref.ID) + '))';
        for LFieldIndex := 0 to LSchema.Count - 1 do
        begin
          LField := LSchema.FieldAt(LFieldIndex);

          if not LRow.FieldValue(LFieldIndex).SameValue(LField.DefaultValue) then
          begin
            LLines.Add(LLine);
            LLine := '          .WithValue(' + CFields[LField.Kind] +
              '(' + PascalString(LField.Name) + '), ' + Literal(LRow.FieldValue(LFieldIndex)) + ')';
          end;
        end;

        if LRowIndex < LDefinition.Count - 1 then
        begin
          LLine := LLine + ',';
        end;
        LLines.Add(LLine);
      end;
      LLines.Add('      ]);');
      LLines.Add('');
    end;
  end;
