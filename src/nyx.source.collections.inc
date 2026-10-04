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

{ Typed declarative collection grammar. This calls the public immutable builders;
  it does not execute Pascal callbacks or evaluate arbitrary identifiers. The
  existing expression depth, array count and complete document admission gates
  apply to schema fields, references and every nested row. }

function TConfigurationReader.CollectionScalar(const AValue: TValue;
  AKind: TNyxStateKind): TNyxStateValue;
var
  LInteger: Integer;
begin
  case AKind of
    nskText:
      begin

        if AValue.Kind <> vkText then
        begin
          Fail('Collection Text requires a text value');
        end;
        Result := TNyxStateValue.FromText(AValue.Text);
      end;
    nskBoolean:
      begin

        if AValue.Kind <> vkBoolean then
        begin
          Fail('Collection Boolean requires a Boolean value');
        end;
        Result := TNyxStateValue.FromBoolean(AValue.Ordinal <> 0);
      end;
    nskInteger:
      begin

        if (AValue.Kind <> vkInteger) or not TryNyxStateInteger(AValue.Text, LInteger) then
        begin
          Fail('Collection Integer requires a portable Integer value');
        end;
        Result := TNyxStateValue.FromInteger(LInteger);
      end;
    nskNumber:
      begin
        Result := TNyxStateValue.FromNumber(Numeric(AValue));
      end;
  end;
end;

function TConfigurationReader.CollectionConstructor(const AName: TNyxText): TValue;
const
  CFields: array[TNyxStateKind] of TNyxText =
    ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
  CMethods: array[TNyxStateKind] of TNyxText =
    ('Text', 'Boolean', 'Integer', 'Number');
var
  LArgs: TValues;
  LKind: TNyxStateKind;
  LFound: Boolean;
  LMethod: TNyxText;
  LValue: TNyxStateValue;
  LDomain: TNyxValueDomain;
  LMode: TNyxCollectionCellMode;
begin
  Result := Default(TValue);

  if SameText(AName, 'NyxCollectionView') then
  begin
    LArgs := Arguments;

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkCollectionKey) then
    begin
      Fail('NyxCollectionView requires a typed collection reference');
    end;
    Result.Kind := vkCollectionView;
    Result.CollectionView := NyxCollectionView(NyxCollection(LArgs[0].Text));
    while At('.') do
    begin
      Expect('.');

      if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
      begin
        Fail('Expected a typed collection view method');
      end;
      LMethod := FTokens[FCursor].Text;
      Inc(FCursor);
      LArgs := Arguments;

      if SameText(LMethod, 'Selection') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkSelectionMode) then
        begin
          Fail('Selection requires a TNyxSelectionMode');
        end;
        Result.CollectionView := Result.CollectionView.Selection(
          TNyxSelectionMode(LArgs[0].Ordinal));
        Continue;
      end;

      if SameText(LMethod, 'Scoped') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkCollectionScope) then
        begin
          Fail('Scoped requires a TNyxCollectionScope');
        end;
        Result.CollectionView := Result.CollectionView.Scoped(
          TNyxCollectionScope(LArgs[0].Ordinal));
        Continue;
      end;

      if SameText(LMethod, 'Parent') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkCollectionField) or
          (LArgs[0].Ordinal <> Ord(nskText)) then
        begin
          Fail('Parent requires a typed text field');
        end;
        Result.CollectionView := Result.CollectionView.Parent(NyxTextField(LArgs[0].Text));
        Continue;
      end;

      if not SameText(LMethod, 'Column') or not (Length(LArgs) in [2, 3]) or
        (LArgs[0].Kind <> vkCollectionField) or (LArgs[1].Kind <> vkText) then
      begin
        Fail('Column requires a typed field, title and optional cell mode');
      end;
      LMode := cmReadOnly;

      if Length(LArgs) = 3 then
      begin

        if LArgs[2].Kind <> vkCollectionCellMode then
        begin
          Fail('Column editability requires a TNyxCollectionCellMode');
        end;
        LMode := TNyxCollectionCellMode(LArgs[2].Ordinal);
      end;
      case TNyxStateKind(LArgs[0].Ordinal) of
        nskText: Result.CollectionView := Result.CollectionView.Column(
          NyxTextField(LArgs[0].Text), LArgs[1].Text, LMode);
        nskBoolean: Result.CollectionView := Result.CollectionView.Column(
          NyxBooleanField(LArgs[0].Text), LArgs[1].Text, LMode);
        nskInteger: Result.CollectionView := Result.CollectionView.Column(
          NyxIntegerField(LArgs[0].Text), LArgs[1].Text, LMode);
        nskNumber: Result.CollectionView := Result.CollectionView.Column(
          NyxNumberField(LArgs[0].Text), LArgs[1].Text, LMode);
      end;
    end;
    Result.CollectionView.Validate;
    Exit;
  end;

  if SameText(AName, 'NyxNoDomain') then
  begin
    Result.Kind := vkNoDomain;
    Exit;
  end;

  if SameText(AName, 'TNyxStateValue') then
  begin
    Expect('.');

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected a typed scalar factory method');
    end;
    LMethod := FTokens[FCursor].Text;
    Inc(FCursor);
    LFound := False;
    for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
    begin

      if SameText(LMethod, 'From' + CMethods[LKind]) then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      Fail('Use a typed scalar value factory');
    end;
    LArgs := Arguments;

    if Length(LArgs) <> 1 then
    begin
      Fail('A typed collection default requires one scalar');
    end;
    Result.Kind := vkScalarValue;
    Result.ScalarValue := CollectionScalar(LArgs[0], LKind);
    Exit;
  end;

  if SameText(AName, 'NyxCollectionSchema') then
  begin
    Result.Kind := vkCollectionSchema;
    Result.CollectionSchema := NyxCollectionSchema;
    while At('.') do
    begin
      Expect('.');

      if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
      begin
        Fail('Expected a typed collection schema method');
      end;
      LMethod := FTokens[FCursor].Text;
      Inc(FCursor);
      LArgs := Arguments;

      if SameText(LMethod, 'Field') then
      begin

        if (Length(LArgs) <> 3) or (LArgs[0].Kind <> vkText) or
          (LArgs[1].Kind <> vkScalarValue) or (LArgs[2].Kind <> vkNoDomain) then
        begin
          Fail('Descriptor Field requires a name, typed scalar default and NyxNoDomain');
        end;
        Result.CollectionSchema := Result.CollectionSchema.Field(
          LArgs[0].Text, LArgs[1].ScalarValue, NyxNoDomain);
        Continue;
      end;
      LFound := False;
      for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
      begin

        if SameText(LMethod, CMethods[LKind]) then
        begin
          LFound := True;
          Break;
        end;
      end;

      if not LFound or not (Length(LArgs) in [2, 3]) or
        (LArgs[0].Kind <> vkCollectionField) or (LArgs[0].Ordinal <> Ord(LKind)) then
      begin
        Fail('Schema methods require their matching typed field and scalar default');
      end;
      LValue := CollectionScalar(LArgs[1], LKind);
      case LKind of
        nskText:
          begin
            LDomain := NyxTextDomain.Definition;
          end;
        nskBoolean:
          begin
            LDomain := NyxBooleanDomain.Definition;
          end;
        nskInteger:
          begin
            LDomain := NyxIntegerDomain.Definition;
          end;
        nskNumber:
          begin
            LDomain := NyxNumberDomain.Definition;
          end;
      end;

      if Length(LArgs) = 3 then
      begin

        if (LArgs[2].Kind <> vkDomain) or (LArgs[2].Ordinal <> Ord(LKind)) then
        begin
          Fail('Collection field/default/domain families must agree');
        end;
        case LKind of
          nskText:
            begin
              LDomain := LArgs[2].TextDomain.Definition;
            end;
          nskBoolean:
            begin
              LDomain := LArgs[2].BooleanDomain.Definition;
            end;
          nskInteger:
            begin
              LDomain := LArgs[2].IntegerDomain.Definition;
            end;
          nskNumber:
            begin
              LDomain := LArgs[2].NumberDomain.Definition;
            end;
        end;
      end;
      Result.CollectionSchema := Result.CollectionSchema.Field(LArgs[0].Text, LValue, LDomain);
    end;
    Exit;
  end;

  LArgs := Arguments;
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin

    if SameText(AName, CFields[LKind]) then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkText) then
      begin
        Fail('A typed collection field requires one name');
      end;
      { Reference factories reject default/invalid names before schema admission. }
      NyxTextField(LArgs[0].Text);
      Result.Kind := vkCollectionField;
      Result.Ordinal := Ord(LKind);
      Result.Text := LArgs[0].Text;
      Exit;
    end;
  end;

  if SameText(AName, 'NyxCollection') then
  begin

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkText) then
    begin
      Fail('NyxCollection requires one name');
    end;
    Result.Kind := vkCollectionKey;
    Result.Text := NyxCollection(LArgs[0].Text).Name;
    Exit;
  end;

  if SameText(AName, 'NyxItem') then
  begin

    if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkCollectionKey) or
      (LArgs[1].Kind <> vkText) then
    begin
      Fail('NyxItem requires a collection reference and item ID');
    end;
    Result.Kind := vkCollectionItemRef;
    Result.CollectionItemRef := NyxItem(NyxCollection(LArgs[0].Text), LArgs[1].Text);
    Exit;
  end;

  if not SameText(AName, 'NyxCollectionItem') or (Length(LArgs) <> 1) or
    (LArgs[0].Kind <> vkCollectionItemRef) then
  begin
    Fail('NyxCollectionItem requires a typed item reference');
  end;
  Result.Kind := vkCollectionItem;
  Result.CollectionItem := NyxCollectionItem(LArgs[0].CollectionItemRef);
  while At('.') do
  begin
    Expect('.');
    Expect('WithValue');
    LArgs := Arguments;

    if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkCollectionField) then
    begin
      Fail('Collection WithValue requires a typed field and matching scalar');
    end;
    LValue := CollectionScalar(LArgs[1], TNyxStateKind(LArgs[0].Ordinal));
    case LValue.Kind of
      nskText:
        begin
          Result.CollectionItem := Result.CollectionItem.WithValue(NyxTextField(LArgs[0].Text), LValue.TextValue);
        end;
      nskBoolean:
        begin
          Result.CollectionItem := Result.CollectionItem.WithValue(NyxBooleanField(LArgs[0].Text), LValue.BooleanValue);
        end;
      nskInteger:
        begin
          Result.CollectionItem := Result.CollectionItem.WithValue(NyxIntegerField(LArgs[0].Text), LValue.IntegerValue);
        end;
      nskNumber:
        begin
          Result.CollectionItem := Result.CollectionItem.WithValue(NyxNumberField(LArgs[0].Text), LValue.NumberValue);
        end;
    end;
  end;
end;

procedure TConfigurationReader.CollectionDefaults;
var
  LKey: TValue;
  LSchema: TValue;
  LValues: TValues;
  LRows: array of TNyxCollectionItem;
  LIndex: Integer;
begin
  Inc(FCursor, 3);
  Expect('.');
  Expect('Define');
  Expect('(');
  LKey := Expression;
  Expect(',');
  LSchema := Expression;
  Expect(',');
  LValues := ArrayArguments(NyxMaximumCollectionItems);
  Expect(')');
  Expect(';');

  if (LKey.Kind <> vkCollectionKey) or (LSchema.Kind <> vkCollectionSchema) then
  begin
    Fail('Collections.Define requires a typed collection key and schema');
  end;
  SetLength(LRows, Length(LValues));
  for LIndex := 0 to High(LValues) do
  begin

    if LValues[LIndex].Kind <> vkCollectionItem then
    begin
      Fail('Collection defaults require typed item values');
    end;
    LRows[LIndex] := LValues[LIndex].CollectionItem;
  end;

  if FApply then
  begin

    if FDocument.Collections.Has(NyxCollection(LKey.Text)) then
    begin
      Fail('Define each collection key once in the declarative builder');
    end;
    FDocument.Collections.Define(NyxCollection(LKey.Text), LSchema.CollectionSchema, LRows);
  end;
end;
