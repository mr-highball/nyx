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
  LFeatures: TNyxColumnLayoutFeatures;
  LCopyActions: TNyxWorkbenchCopyActions;
  LFeature: TValue;
  LInteger: Integer;
begin
  Result := Default(TValue);

  if SameText(AName, 'NyxTablePresentation') or SameText(AName, 'NyxColumnText') then
  begin

    if At('(') then
    begin
      LArgs := Arguments;

      if Length(LArgs) <> 0 then
      begin
        Fail('Table presentation factories take no arguments');
      end;
    end;

    if SameText(AName, 'NyxTablePresentation') then
    begin
      Result.Kind := vkTablePresentationPolicy;
      Result.TablePresentationPolicy := NyxTablePresentation;
    end
    else
    begin
      Result.Kind := vkColumnTextPolicy;
      Result.ColumnTextPolicy := NyxColumnText;
    end;
    while At('.') do
    begin
      Expect('.');

      if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
      begin
        Fail('Expected a typed table presentation method');
      end;
      LMethod := FTokens[FCursor].Text;
      Inc(FCursor);

      if (Result.Kind = vkColumnTextPolicy) and not At('(') and
        (SameText(LMethod, 'Clip') or SameText(LMethod, 'Ellipsis') or
          SameText(LMethod, 'Wrap')) then
      begin
        LArgs := nil;
      end
      else
      begin
        LArgs := Arguments;
      end;

      if Result.Kind = vkTablePresentationPolicy then
      begin

        if not SameText(LMethod, 'Density') or (Length(LArgs) <> 1) or
          (LArgs[0].Kind <> vkTableRowDensity) then
        begin
          Fail('Density requires one typed table row density');
        end;
        Result.TablePresentationPolicy := Result.TablePresentationPolicy.Density(
          TNyxTableRowDensity(LArgs[0].Ordinal));
        Continue;
      end;

      if SameText(LMethod, 'Wrap') then
      begin
        LInteger := 3;

        if Length(LArgs) = 1 then
        begin

          if (LArgs[0].Kind <> vkInteger) or
            not TryNyxStateInteger(LArgs[0].Text, LInteger) then
          begin
            Fail('Wrap requires a positive portable Integer line count');
          end;
        end
        else if Length(LArgs) <> 0 then
        begin
          Fail('Wrap accepts only its optional line count');
        end;

        if LInteger < 1 then
        begin
          Fail('Wrap requires a positive line count');
        end;
        Result.ColumnTextPolicy := Result.ColumnTextPolicy.Wrap(LInteger);
      end
      else
      begin

        if Length(LArgs) <> 0 then
        begin
          Fail('Single-line text presentation takes no arguments');
        end;

        if SameText(LMethod, 'Clip') then
        begin
          Result.ColumnTextPolicy := Result.ColumnTextPolicy.Clip;
        end
        else if SameText(LMethod, 'Ellipsis') then
        begin
          Result.ColumnTextPolicy := Result.ColumnTextPolicy.Ellipsis;
        end
        else
        begin
          Fail('Unknown column text presentation method');
        end;
      end;
    end;
    Exit;
  end;

  if SameText(AName, 'NyxTableWorkbench') then
  begin

    if At('(') then
    begin
      LArgs := Arguments;

      if Length(LArgs) <> 0 then
      begin
        Fail('NyxTableWorkbench takes no factory arguments');
      end;
    end;
    Result.Kind := vkTableWorkbenchPolicy;
    Result.TableWorkbenchPolicy := NyxTableWorkbench;
    while At('.') do
    begin
      Expect('.');

      if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
      begin
        Fail('Expected a typed table workbench method');
      end;
      LMethod := FTokens[FCursor].Text;
      Inc(FCursor);

      if SameText(LMethod, 'AllowCopy') then
      begin
        Expect('(');
        Expect('[');
        LCopyActions := [];
        while not At(']') do
        begin
          LFeature := Expression;

          if (LFeature.Kind <> vkWorkbenchCopyAction) or
            (LFeature.Ordinal < Ord(Low(TNyxWorkbenchCopyAction))) or
            (LFeature.Ordinal > Ord(High(TNyxWorkbenchCopyAction))) then
          begin
            Fail('AllowCopy requires typed workbench-copy action symbols');
          end;

          if TNyxWorkbenchCopyAction(LFeature.Ordinal) in LCopyActions then
          begin
            Fail('Duplicate workbench copy action');
          end;
          Include(LCopyActions, TNyxWorkbenchCopyAction(LFeature.Ordinal));

          if not At(']') then
          begin
            Expect(',');

            if At(']') then
            begin
              Fail('A workbench copy action must follow the comma');
            end;
          end;
        end;
        Expect(']');
        Expect(')');
        Result.TableWorkbenchPolicy := Result.TableWorkbenchPolicy.AllowCopy(LCopyActions);
        Continue;
      end;

      if SameText(LMethod, 'Copy') and not At('(') then
      begin
        LArgs := nil;
      end
      else
      begin
        LArgs := Arguments;
      end;

      if SameText(LMethod, 'Copy') and (Length(LArgs) = 0) then
      begin
        Continue;
      end;

      if (Length(LArgs) = 1) and (LArgs[0].Kind = vkBoolean) then
      begin

        if SameText(LMethod, 'RangeSelection') then
        begin
          Result.TableWorkbenchPolicy := Result.TableWorkbenchPolicy.RangeSelection(LArgs[0].Ordinal <> 0);
          Continue;
        end;

        if SameText(LMethod, 'Headers') then
        begin
          Result.TableWorkbenchPolicy := Result.TableWorkbenchPolicy.Headers(LArgs[0].Ordinal <> 0);
          Continue;
        end;

        if SameText(LMethod, 'FocusedDraft') then
        begin
          Result.TableWorkbenchPolicy := Result.TableWorkbenchPolicy.FocusedDraft(LArgs[0].Ordinal <> 0);
          Continue;
        end;
      end;

      if SameText(LMethod, 'CopyFields') and (Length(LArgs) = 1) and
        (LArgs[0].Kind = vkWorkbenchCopyFields) then
      begin
        Result.TableWorkbenchPolicy := Result.TableWorkbenchPolicy.CopyFields(
          TNyxWorkbenchCopyFields(LArgs[0].Ordinal));
        Continue;
      end;

      if SameText(LMethod, 'LineEnding') and (Length(LArgs) = 1) and
        (LArgs[0].Kind = vkWorkbenchLineEnding) then
      begin
        Result.TableWorkbenchPolicy := Result.TableWorkbenchPolicy.LineEnding(
          TNyxWorkbenchLineEnding(LArgs[0].Ordinal));
        Continue;
      end;
      Fail('Unsupported or ill-typed table workbench method');
    end;
    Exit;
  end;

  if SameText(AName, 'NyxColumnLayout') or SameText(AName, 'NyxTableLayout') then
  begin
    { The source reader reconstructs the public typed policy. It never executes
      saved Pascal or decodes an enum/set from an arbitrary text expression. }

    if At('(') then
    begin
      LArgs := Arguments;

      if Length(LArgs) <> 0 then
      begin
        Fail('Column/table layout factories take no arguments');
      end;
    end;
    Result.Kind := vkColumnLayoutPolicy;
    Result.ColumnLayoutPolicy := NyxColumnLayout;

    if SameText(AName, 'NyxTableLayout') then
    begin
      Result.Kind := vkTableLayoutPolicy;
      Result.TableLayoutPolicy := NyxTableLayout;
    end;
    while At('.') do
    begin
      Expect('.');

      if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
      begin
        Fail('Expected a typed column layout method');
      end;
      LMethod := FTokens[FCursor].Text;
      Inc(FCursor);

      if SameText(LMethod, 'AllowOnly') then
      begin
        Expect('(');
        Expect('[');
        LFeatures := [];
        while not At(']') do
        begin
          LFeature := Expression;

          if (LFeature.Kind <> vkColumnLayoutFeature) or
            (LFeature.Ordinal < Ord(Low(TNyxColumnLayoutFeature))) or
            (LFeature.Ordinal > Ord(High(TNyxColumnLayoutFeature))) then
          begin
            Fail('AllowOnly requires typed column-layout feature symbols');
          end;

          if TNyxColumnLayoutFeature(LFeature.Ordinal) in LFeatures then
          begin
            Fail('Duplicate column-layout feature');
          end;
          Include(LFeatures, TNyxColumnLayoutFeature(LFeature.Ordinal));

          if not At(']') then
          begin
            Expect(',');

            if At(']') then
            begin
              Fail('A column-layout feature must follow the comma');
            end;
          end;
        end;
        Expect(']');
        Expect(')');

        if Result.Kind = vkColumnLayoutPolicy then
        begin
          Result.ColumnLayoutPolicy := Result.ColumnLayoutPolicy.AllowOnly(LFeatures);
        end
        else
        begin
          Result.TableLayoutPolicy := Result.TableLayoutPolicy.AllowOnly(LFeatures);
        end;
        Continue;
      end;

      if (SameText(LMethod, 'PinStart') or SameText(LMethod, 'PinEnd') or
        SameText(LMethod, 'Unpinned') or SameText(LMethod, 'Copy')) and not At('(') then
      begin
        LArgs := nil;
      end
      else
      begin
        LArgs := Arguments;
      end;

      if SameText(LMethod, 'Copy') and (Length(LArgs) = 0) then
      begin
        Continue;
      end;

      if Result.Kind = vkColumnLayoutPolicy then
      begin

        if SameText(LMethod, 'Visible') and (Length(LArgs) = 1) and
          (LArgs[0].Kind = vkBoolean) then
        begin
          Result.ColumnLayoutPolicy := Result.ColumnLayoutPolicy.Visible(LArgs[0].Ordinal <> 0);
          Continue;
        end;

        if SameText(LMethod, 'Pinned') and (Length(LArgs) = 1) and
          (LArgs[0].Kind = vkColumnPin) then
        begin
          Result.ColumnLayoutPolicy := Result.ColumnLayoutPolicy.Pinned(TNyxColumnPin(LArgs[0].Ordinal));
          Continue;
        end;

        if (Length(LArgs) = 0) and SameText(LMethod, 'PinStart') then
        begin
          Result.ColumnLayoutPolicy := Result.ColumnLayoutPolicy.PinStart;
          Continue;
        end;

        if (Length(LArgs) = 0) and SameText(LMethod, 'PinEnd') then
        begin
          Result.ColumnLayoutPolicy := Result.ColumnLayoutPolicy.PinEnd;
          Continue;
        end;

        if (Length(LArgs) = 0) and SameText(LMethod, 'Unpinned') then
        begin
          Result.ColumnLayoutPolicy := Result.ColumnLayoutPolicy.Unpinned;
          Continue;
        end;
      end
      else
      begin

        if SameText(LMethod, 'Enabled') and (Length(LArgs) = 1) and
          (LArgs[0].Kind = vkBoolean) then
        begin
          Result.TableLayoutPolicy := Result.TableLayoutPolicy.Enabled(LArgs[0].Ordinal <> 0);
          Continue;
        end;

        if (Length(LArgs) = 1) and (LArgs[0].Kind = vkInteger) and
          TryNyxStateInteger(LArgs[0].Text, LInteger) then
        begin

          if SameText(LMethod, 'MinimumCenter') then
          begin
            Result.TableLayoutPolicy := Result.TableLayoutPolicy.MinimumCenter(LInteger);
            Continue;
          end;

          if SameText(LMethod, 'AutoSizeSample') then
          begin
            Result.TableLayoutPolicy := Result.TableLayoutPolicy.AutoSizeSample(LInteger);
            Continue;
          end;
        end;
      end;
      Fail('Invalid typed column/table layout method ' + LMethod);
    end;
    Exit;
  end;

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
      { Pascal permits this parameterless reset with or without parentheses.
        Other methods keep their ordinary typed argument grammar. }

      if (SameText(LMethod, 'UseDefaultTypeAhead') or
        SameText(LMethod, 'UseDefaultColumnSizing') or
        SameText(LMethod, 'UseDefaultColumnQuery') or
        SameText(LMethod, 'UseDefaultTableQuery') or
        SameText(LMethod, 'UseDefaultColumnLayout') or
        SameText(LMethod, 'UseDefaultColumnFormat') or
        SameText(LMethod, 'UseDefaultTableLayout') or
        SameText(LMethod, 'UseDefaultTableWorkbench') or
        SameText(LMethod, 'UseDefaultTablePresentation') or
        SameText(LMethod, 'UseDefaultColumnText') or
        SameText(LMethod, 'UseDefaultPaging')) and not At('(') then
      begin
        LArgs := nil;
      end
      else
      begin
        LArgs := Arguments;
      end;

      if SameText(LMethod, 'TypeAhead') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkTypeAheadOptions) then
        begin
          Fail('TypeAhead requires a typed NyxTypeAhead policy');
        end;
        Result.CollectionView := Result.CollectionView.TypeAhead(LArgs[0].TypeAheadOptions);
        Continue;
      end;

      if SameText(LMethod, 'Paged') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkCollectionPaging) then
        begin
          Fail('Paged requires typed NyxCollectionPaging options');
        end;
        Result.CollectionView := Result.CollectionView.Paged(LArgs[0].CollectionPaging);
        Continue;
      end;

      if SameText(LMethod, 'UseDefaultPaging') then
      begin

        if Length(LArgs) <> 0 then
        begin
          Fail('UseDefaultPaging takes no arguments');
        end;
        Result.CollectionView := Result.CollectionView.UseDefaultPaging;
        Continue;
      end;

      if SameText(LMethod, 'ColumnSizing') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkColumnSizing) then
        begin
          Fail('ColumnSizing requires a typed NyxColumnSizing policy');
        end;
        Result.CollectionView := Result.CollectionView.ColumnSizing(LArgs[0].ColumnSizing);
        Continue;
      end;

      if SameText(LMethod, 'ColumnQuery') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkColumnQueryPolicy) then
        begin
          Fail('ColumnQuery requires a typed NyxColumnQuery policy');
        end;
        Result.CollectionView := Result.CollectionView.ColumnQuery(LArgs[0].ColumnQueryPolicy);
        Continue;
      end;

      if SameText(LMethod, 'ColumnLayout') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkColumnLayoutPolicy) then
        begin
          Fail('ColumnLayout requires a typed NyxColumnLayout policy');
        end;
        Result.CollectionView := Result.CollectionView.ColumnLayout(LArgs[0].ColumnLayoutPolicy);
        Continue;
      end;

      if SameText(LMethod, 'ColumnFormat') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkValueFormat) then
        begin
          Fail('ColumnFormat requires a typed NyxValueFormat policy');
        end;
        Result.CollectionView := Result.CollectionView.ColumnFormat(LArgs[0].ValueFormat);
        Continue;
      end;

      if SameText(LMethod, 'UseDefaultColumnFormat') then
      begin

        if Length(LArgs) <> 0 then
        begin
          Fail('UseDefaultColumnFormat takes no arguments');
        end;
        Result.CollectionView := Result.CollectionView.UseDefaultColumnFormat;
        Continue;
      end;

      if SameText(LMethod, 'TablePresentation') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkTablePresentationPolicy) then
        begin
          Fail('TablePresentation requires a typed NyxTablePresentation policy');
        end;
        Result.CollectionView := Result.CollectionView.TablePresentation(
          LArgs[0].TablePresentationPolicy);
        Continue;
      end;

      if SameText(LMethod, 'ColumnText') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkColumnTextPolicy) then
        begin
          Fail('ColumnText requires a typed NyxColumnText policy');
        end;
        Result.CollectionView := Result.CollectionView.ColumnText(LArgs[0].ColumnTextPolicy);
        Continue;
      end;

      if SameText(LMethod, 'UseDefaultTablePresentation') or
        SameText(LMethod, 'UseDefaultColumnText') then
      begin

        if Length(LArgs) <> 0 then
        begin
          Fail('Restoring text presentation defaults takes no arguments');
        end;

        if SameText(LMethod, 'UseDefaultTablePresentation') then
        begin
          Result.CollectionView := Result.CollectionView.UseDefaultTablePresentation;
        end
        else
        begin
          Result.CollectionView := Result.CollectionView.UseDefaultColumnText;
        end;
        Continue;
      end;

      if SameText(LMethod, 'TableWorkbench') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkTableWorkbenchPolicy) then
        begin
          Fail('TableWorkbench requires a typed NyxTableWorkbench policy');
        end;
        Result.CollectionView := Result.CollectionView.TableWorkbench(LArgs[0].TableWorkbenchPolicy);
        Continue;
      end;

      if SameText(LMethod, 'UseDefaultTableWorkbench') then
      begin

        if Length(LArgs) <> 0 then
        begin
          Fail('Restoring table workbench defaults takes no arguments');
        end;
        Result.CollectionView := Result.CollectionView.UseDefaultTableWorkbench;
        Continue;
      end;

      if SameText(LMethod, 'TableLayout') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkTableLayoutPolicy) then
        begin
          Fail('TableLayout requires a typed NyxTableLayout policy');
        end;
        Result.CollectionView := Result.CollectionView.TableLayout(LArgs[0].TableLayoutPolicy);
        Continue;
      end;

      if SameText(LMethod, 'UseDefaultColumnLayout') or SameText(LMethod, 'UseDefaultTableLayout') then
      begin

        if Length(LArgs) <> 0 then
        begin
          Fail('Restoring column layout choices takes no arguments');
        end;

        if SameText(LMethod, 'UseDefaultColumnLayout') then
        begin
          Result.CollectionView := Result.CollectionView.UseDefaultColumnLayout;
        end
        else
        begin
          Result.CollectionView := Result.CollectionView.UseDefaultTableLayout;
        end;
        Continue;
      end;

      if SameText(LMethod, 'TableQuery') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkTableQueryPolicy) then
        begin
          Fail('TableQuery requires a typed NyxTableQuery policy');
        end;
        Result.CollectionView := Result.CollectionView.TableQuery(LArgs[0].TableQueryPolicy);
        Continue;
      end;

      if SameText(LMethod, 'UseDefaultColumnQuery') or SameText(LMethod, 'UseDefaultTableQuery') then
      begin

        if Length(LArgs) <> 0 then
        begin
          Fail('Restoring query controls takes no arguments');
        end;

        if SameText(LMethod, 'UseDefaultColumnQuery') then
        begin
          Result.CollectionView := Result.CollectionView.UseDefaultColumnQuery;
        end
        else
        begin
          Result.CollectionView := Result.CollectionView.UseDefaultTableQuery;
        end;
        Continue;
      end;

      if SameText(LMethod, 'UseDefaultColumnSizing') then
      begin

        if Length(LArgs) <> 0 then
        begin
          Fail('UseDefaultColumnSizing takes no arguments');
        end;
        Result.CollectionView := Result.CollectionView.UseDefaultColumnSizing;
        Continue;
      end;

      if SameText(LMethod, 'UseDefaultTypeAhead') then
      begin

        if Length(LArgs) <> 0 then
        begin
          Fail('UseDefaultTypeAhead takes no arguments');
        end;
        Result.CollectionView := Result.CollectionView.UseDefaultTypeAhead;
        Continue;
      end;

      if SameText(LMethod, 'Query') then
      begin

        if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkCollectionQuery) then
        begin
          Fail('Query requires a typed NyxCollectionQuery');
        end;
        Result.CollectionView := Result.CollectionView.Query(LArgs[0].CollectionQuery);
        Continue;
      end;

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

        if (LKind = nskText) and (LArgs[2].Kind = vkClockDomain) then
        begin
          Result.CollectionSchema := Result.CollectionSchema.Text(NyxTextField(LArgs[0].Text),
            LValue.TextValue, LArgs[2].TimeDomain);
          Continue;
        end;

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
