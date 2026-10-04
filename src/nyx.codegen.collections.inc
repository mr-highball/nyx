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

  { Bindings use the public immutable fluent view object, with exact field
    families and enum choices. No wire property strings become authoring API. }
  procedure EmitCollectionBinding(ANode: TNyxNode; const AVariable: TNyxText);
  const
    CFields: array[TNyxStateKind] of TNyxText =
      ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
  var
    LView: TNyxCollectionViewSpec;
    LColumn: TNyxCollectionColumn;
    LIndex: Integer;
    LLine: TNyxText;
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
    LLines[LLines.Count - 1] := LLines[LLines.Count - 1] + ')';
  end;

  { Emit typed collection construction rather than opaque JSON or property keys.
    Defaults remain one admitted ordered packet. Only values differing from the
    schema default are spelled again in rows; normalization materializes the
    rest in schema order. Existing typed domain expressions retain their exact
    family/choices/range, including unconstrained family domains. }
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
