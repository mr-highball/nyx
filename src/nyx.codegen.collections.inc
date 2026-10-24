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

class function TNyxCodegen.QueryExpression(const AQuery: TNyxCollectionQuery;
  const AIndent: TNyxText): TNyxText;
const
  CFields: array[TNyxStateKind] of TNyxText =
    ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
var
  LLines: TNyxStrings;
  LIndex: Integer;
  LSort: TNyxCollectionSort;
  LLine: TNyxText;
begin
  for LIndex := 1 to Length(AIndent) do
  begin

    if not (AIndent[LIndex] in [' ', #9]) then
    begin
      raise ENyxModel.Create('Query indentation accepts only spaces and tabs');
    end;
  end;
  AQuery.ToData;
  LLines := TNyxStrings.Create;
  try
    LLines.Add(AIndent + 'NyxCollectionQuery');

    if AQuery.Filter <> nil then
    begin
      LLines.Add(AIndent + '  .Where(');
      LLines.Add(AIndent + '    ' + QueryPredicateExpression(AQuery.Filter, AIndent + '    ') + ')');
    end;
    for LIndex := 0 to AQuery.SortCount - 1 do
    begin
      LSort := AQuery.SortAt(LIndex);
      LLine := AIndent + '  .ThenBy(';

      if LIndex = 0 then
      begin
        LLine := AIndent + '  .OrderBy(';
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
    Result := LLines.Text;
    { The expression has no trailing newline; its host owns terminators. }
    SetLength(Result, Length(Result) - 1);
  finally
    LLines.Free;
  end;
end;

{ Keep the new namespace conditional so an unchanged legacy document produces
  its former import list exactly. Definitions are authored roots in their own
  right, independently of whether an application currently instantiates them. }
function NeedsColumnLayoutUnits(ADocument: TNyxDocument): Boolean;
var
  LIndex: Integer;

  function ContainsLayout(ANode: TNyxNode): Boolean;
  var
    LChild: Integer;
  begin
    Result := ANode.HasCollectionView and ANode.CollectionView.HasLayoutControls;
    for LChild := 0 to ANode.Count - 1 do
    begin

      if Result then
      begin
        Exit;
      end;
      Result := ContainsLayout(ANode.Children[LChild]);
    end;
  end;

begin
  for LIndex := 0 to ADocument.Count - 1 do
  begin

    if ContainsLayout(ADocument.Pages[LIndex]) then
    begin
      Exit(True);
    end;
  end;
  for LIndex := 0 to ADocument.ComponentCount - 1 do
  begin

    if ContainsLayout(ADocument.Components[LIndex]) then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function NeedsColumnFormatUnits(ADocument: TNyxDocument): Boolean;
var
  LIndex: Integer;

  function ContainsFormat(ANode: TNyxNode): Boolean;
  var
    LChild: Integer;
  begin
    Result := ANode.HasCollectionView and ANode.CollectionView.HasColumnFormat;
    for LChild := 0 to ANode.Count - 1 do
    begin

      if Result then
      begin
        Exit;
      end;
      Result := ContainsFormat(ANode.Children[LChild]);
    end;
  end;

begin
  for LIndex := 0 to ADocument.Count - 1 do
  begin

    if ContainsFormat(ADocument.Pages[LIndex]) then
    begin
      Exit(True);
    end;
  end;
  for LIndex := 0 to ADocument.ComponentCount - 1 do
  begin

    if ContainsFormat(ADocument.Components[LIndex]) then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

{ Emit actual enum symbols, in a stable order, rather than making generated
  callers reconstruct a policy from interchange strings or ordinal masks. }
function ColumnLayoutFeatureExpression(const AFeatures: TNyxColumnLayoutFeatures): TNyxText;
const
  CFeatures: array[TNyxColumnLayoutFeature] of TNyxText =
    ('nclfResize', 'nclfAutoSize', 'nclfReorder', 'nclfVisibility', 'nclfPinning');
var
  LFeature: TNyxColumnLayoutFeature;
begin
  Result := '[';
  for LFeature := Low(TNyxColumnLayoutFeature) to High(TNyxColumnLayoutFeature) do
  begin

    if LFeature in AFeatures then
    begin

      if Length(Result) > 1 then
      begin
        Result := Result + ', ';
      end;
      Result := Result + CFeatures[LFeature];
    end;
  end;
  Result := Result + ']';
end;

procedure EmitCollectionBinding(ANode: TNyxNode; ALines: TNyxStrings;
  const AIndent: TNyxText);
const
  CFields: array[TNyxStateKind] of TNyxText =
    ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
  CMatches: array[TNyxTypeAheadMatch] of TNyxText = ('ntmFolded', 'ntmExact');
  CFormatRounding: array[TNyxDecimalRounding] of TNyxText =
    ('ndrExact', 'ndrToEven', 'ndrAwayFromZero', 'ndrTowardZero', 'ndrFloor', 'ndrCeiling');
  CFormatDateOrder: array[TNyxFormatDateOrder] of TNyxText =
    ('nfdoYearMonthDay', 'nfdoDayMonthYear', 'nfdoMonthDayYear');
  CComparisons: array[TNyxQueryComparison] of TNyxText =
    ('nqcEqual', 'nqcNotEqual', 'nqcLess', 'nqcAtMost', 'nqcGreater', 'nqcAtLeast',
     'nqcContains', 'nqcStartsWith', 'nqcEndsWith');
var
  LView: TNyxCollectionViewSpec;
  LColumn: TNyxCollectionColumn;
  LIndex: Integer;
  LOperator: Integer;
  LLine: TNyxText;
  LQuery: TNyxCollectionQuery;
  LColumnPolicy: TNyxColumnQueryPolicy;
  LTablePolicy: TNyxTableQueryPolicy;
  LColumnLayout: TNyxColumnLayoutPolicy;
  LTableLayout: TNyxTableLayoutPolicy;
  LFormat: TNyxValueFormat;
begin

  if not ANode.HasCollectionView then
  begin
    Exit;
  end;
  LView := ANode.CollectionView;

  if not LView.Defined then
  begin
    ALines.Add(AIndent + '.ClearCollection');
    Exit;
  end;
  ALines.Add(AIndent + '.Collection(');
  ALines.Add(AIndent + '  NyxCollectionView(NyxCollection(' + PascalString(LView.Key.Name) + '))');
  for LIndex := 0 to LView.Count - 1 do
  begin
    LColumn := LView.ColumnAt(LIndex);
    LLine := AIndent + '    .Column(' + CFields[LColumn.Kind] + '(' +
      PascalString(LColumn.FieldName) + '), ' + PascalString(LColumn.Title);

    if LColumn.Mode = cmEditable then
    begin
      LLine := LLine + ', cmEditable';
    end;
    ALines.Add(LLine + ')');

    if LColumn.Sizing.Defined then
    begin
      LLine := AIndent + '    .ColumnSizing(NyxColumnSizing';
      case LColumn.Sizing.Mode of
        ncsContent:
          begin
            LLine := LLine + '.Content()';
          end;
        ncsFixed:
          begin
            LLine := LLine + '.Fixed(' + TNyxText(IntToStr(LColumn.Sizing.Value)) + ')';
          end;
        ncsFlex:
          begin
            LLine := LLine + '.Flex(' + TNyxText(IntToStr(LColumn.Sizing.Value)) + ')';
          end;
      end;

      if LColumn.Sizing.Bounds.MinimumValue <> 1 then
      begin
        LLine := LLine + '.Minimum(' + TNyxText(IntToStr(LColumn.Sizing.Bounds.MinimumValue)) + ')';
      end;

      if LColumn.Sizing.Bounds.MaximumValue <> MaximumNyxLayoutBound then
      begin
        LLine := LLine + '.Maximum(' + TNyxText(IntToStr(LColumn.Sizing.Bounds.MaximumValue)) + ')';
      end;
      ALines.Add(LLine + ')');
    end;

    if LColumn.HasQueryPolicy then
    begin
      LColumnPolicy := LColumn.QueryPolicy;
      LLine := AIndent + '    .ColumnQuery(NyxColumnQuery';

      if not LColumnPolicy.FilterEnabled then
      begin
        LLine := LLine + '.Filtering(False)';
      end;

      if not LColumnPolicy.SortEnabled then
      begin
        LLine := LLine + '.Sorting(False)';
      end;

      if LColumnPolicy.TextMode = nqtAsciiInsensitive then
      begin
        LLine := LLine + '.TextComparison(nqtAsciiInsensitive)';
      end;

      if LColumnPolicy.HasOperators then
      begin
        LLine := LLine + '.Operators([';
        for LOperator := 0 to LColumnPolicy.ComparisonCount - 1 do
        begin

          if LOperator > 0 then
          begin
            LLine := LLine + ', ';
          end;
          LLine := LLine + CComparisons[LColumnPolicy.ComparisonAt(LOperator)];
        end;
        LLine := LLine + '])';
      end;
      ALines.Add(LLine + ')');
    end;

    if LColumn.HasLayoutPolicy then
    begin
      LColumnLayout := LColumn.LayoutPolicy;
      LLine := AIndent + '    .ColumnLayout(NyxColumnLayout';

      if not LColumnLayout.IsVisible then
      begin
        LLine := LLine + '.Visible(False)';
      end;
      case LColumnLayout.Pin of
        ncpNone: begin end;
        ncpStart: LLine := LLine + '.PinStart()';
        ncpEnd: LLine := LLine + '.PinEnd()';
      end;

      if LColumnLayout.Features <> NyxColumnLayout.Features then
      begin
        LLine := LLine + '.AllowOnly(' + ColumnLayoutFeatureExpression(LColumnLayout.Features) + ')';
      end;
      ALines.Add(LLine + ')');
    end;

    if LColumn.HasFormatPolicy then
    begin
      LFormat := LColumn.FormatPolicy;
      LLine := AIndent + '    .ColumnFormat(NyxValueFormat';
      case LFormat.Style of
        nvfsCanonical: begin end;
        nvfsDecimal: LLine := LLine + '.Decimal(' + TNyxText(IntToStr(LFormat.Scale)) +
          ', ' + CFormatRounding[LFormat.Rounding] + ')';
        nvfsCalendarDate: LLine := LLine + '.CalendarDate(' +
          CFormatDateOrder[LFormat.DateOrder] + ', ' + PascalString(LFormat.DateSeparator) + ')';
        nvfsClockTime: LLine := LLine + '.ClockTime(' + NyxTimePrecisionPascal(LFormat.TimePrecision) + ')';
        nvfsBooleanLabels: LLine := LLine + '.BooleanLabels(' +
          PascalString(LFormat.FirstText) + ', ' + PascalString(LFormat.SecondText) + ')';
        nvfsTextAffixes: LLine := LLine + '.TextAffixes(' +
          PascalString(LFormat.FirstText) + ', ' + PascalString(LFormat.SecondText) + ')';
      end;
      LLine := LLine + '.Identified(NyxFormatter(' + PascalString(LFormat.Identity.Name) + '), ' +
        TNyxText(IntToStr(LFormat.Revision)) + ').Localized(NyxFormatLocale';

      if LFormat.Locale.Locale.Defined then
      begin
        LLine := LLine + '.Named(NyxLocale(' + PascalString(LFormat.Locale.Locale.Name) + '))';
      end;
      ALines.Add(LLine + '.NumberSeparators(' + PascalString(LFormat.Locale.DecimalSeparator) +
        ', ' + PascalString(LFormat.Locale.GroupSeparator) + ')))');
    end;
  end;

  if LView.HasTableLayoutPolicy then
  begin
    LTableLayout := LView.TableLayoutPolicy;
    LLine := AIndent + '    .TableLayout(NyxTableLayout';

    if not LTableLayout.IsEnabled then
    begin
      LLine := LLine + '.Enabled(False)';
    end;

    if LTableLayout.Features <> NyxTableLayout.Features then
    begin
      LLine := LLine + '.AllowOnly(' + ColumnLayoutFeatureExpression(LTableLayout.Features) + ')';
    end;

    if LTableLayout.MinimumScrollableWidth <> NyxDefaultColumnScrollableWidth then
    begin
      LLine := LLine + '.MinimumCenter(' + TNyxText(IntToStr(LTableLayout.MinimumScrollableWidth)) + ')';
    end;

    if LTableLayout.AutoSizeRows <> NyxDefaultColumnAutoSizeRows then
    begin
      LLine := LLine + '.AutoSizeSample(' + TNyxText(IntToStr(LTableLayout.AutoSizeRows)) + ')';
    end;
    ALines.Add(LLine + ')');
  end;

  if LView.HasTableQueryPolicy then
  begin
    LTablePolicy := LView.TableQueryPolicy;
    LLine := AIndent + '    .TableQuery(NyxTableQuery';

    if not LTablePolicy.IsEnabled then
    begin
      LLine := LLine + '.Enabled(False)';
    end;

    if not LTablePolicy.FilterEnabled then
    begin
      LLine := LLine + '.Filtering(False)';
    end;

    if not LTablePolicy.SortEnabled then
    begin
      LLine := LLine + '.Sorting(False)';
    end;

    if LTablePolicy.Mode = nqsmSingle then
    begin
      LLine := LLine + '.SortMode(nqsmSingle)';
    end;
    ALines.Add(LLine + ')');
  end;

  if LView.ParentField <> '' then
  begin
    ALines.Add(AIndent + '    .Parent(NyxTextField(' + PascalString(LView.ParentField) + '))');
  end;

  if LView.Scope = csInstance then
  begin
    ALines.Add(AIndent + '    .Scoped(csInstance)');
  end;

  if LView.SelectionMode = nsmMultiple then
  begin
    ALines.Add(AIndent + '    .Selection(nsmMultiple)');
  end;

  if LView.PagingPolicy.Defined then
  begin
    LLine := AIndent + '    .Paged(NyxCollectionPaging.PageSize(' +
      IntToStr(LView.PagingPolicy.ItemsPerPage) + ')';

    if LView.PagingPolicy.FirstPage <> 1 then
    begin
      LLine := LLine + '.InitialPage(' + IntToStr(LView.PagingPolicy.FirstPage) + ')';
    end;
    ALines.Add(LLine + ')');
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
    ALines.Add(AIndent + '    .TypeAhead(NyxTypeAhead');
    ALines.Add(AIndent + '      .Enabled(' + LLine + ')');
    ALines.Add(AIndent + '      .WindowMilliseconds(' +
      IntToStr(LView.TypeAheadPolicy.WindowMS) + ')');
    ALines.Add(AIndent + '      .Match(' + CMatches[LView.TypeAheadPolicy.MatchMode] + '))');
  end;
  LQuery := LView.QueryPolicy;

  if LQuery.Defined then
  begin
    ALines.Add(AIndent + '    .Query(');
    ALines.Add(TNyxCodegen.QueryExpression(LQuery, AIndent + '      ') + ')');
  end;
  ALines[ALines.Count - 1] := ALines[ALines.Count - 1] + ')';
end;

function PascalCollectionValue(const AValue: TNyxStateValue): TNyxText;
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

{ Keep row recipes readable and strongly typed, sharing the structural selector
  formatter with resource rows. The public default number policy needs no noise. }
function DataRowsExpression(const ARows: TNyxDataRows): TNyxText;
const
  CMethods: array[TNyxStateKind] of TNyxText = ('Text', 'Boolean', 'Integer', 'Number');
  CFields: array[TNyxStateKind] of TNyxText =
    ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
var
  LIndex: Integer;
  LField: TNyxCollectionField;
  LMethod: TNyxText;
begin
  ARows.Validate;
  Result := ResourcePathExpression(ARows.Path, 'NyxDataRows') +
    #10 + '            .Identity(' + ResourcePathExpression(ARows.IdentityPath, 'NyxDataPath') + ')';

  if ARows.NumberPolicy = nrnpApproximate then
  begin
    Result := Result + '.Numbers(nrnpApproximate)';
  end;
  for LIndex := 0 to ARows.Schema.Count - 1 do
  begin
    LField := ARows.Schema.FieldAt(LIndex);
    LMethod := CMethods[LField.Kind];

    if ARows.Projection(LIndex) = nrpDecimalText then
    begin
      LMethod := 'DecimalText';
    end;
    Result := Result + #10 + '            .' + LMethod + '(' +
      CFields[LField.Kind] + '(' + PascalString(LField.Name) + ')';

    if ARows.FieldPath(LIndex).ToData.ToJSON <>
      NyxResourcePath.Field(LField.Name).ToData.ToJSON then
    begin
      Result := Result + ', ' + ResourcePathExpression(ARows.FieldPath(LIndex), 'NyxDataPath');
    end;
    Result := Result + ')';
  end;
end;

procedure EmitResourceCollection(const AKey: TNyxCollectionRef;
  const ASource: TNyxResourceRows; ALines: TNyxStrings;
  const AReceiver, AIndent: TNyxText);
const
  CMethods: array[TNyxStateKind] of TNyxText = ('Text', 'Boolean', 'Integer', 'Number');
  CFields: array[TNyxStateKind] of TNyxText =
    ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
var
  LIndex: Integer;
  LField: TNyxCollectionField;
  LLine: TNyxText;
begin
  ALines.Add(AIndent + AReceiver + '.ResourceCollections.Define(');
  ALines.Add(AIndent + '  NyxCollection(' + PascalString(AKey.Name) + '),');
  ALines.Add(AIndent + '  ' + ResourcePathExpression(ASource.Path,
    'NyxResourceRows(NyxResourceRef(' + PascalString(ASource.Reference.Name) + '))'));
  ALines.Add(AIndent + '    .Identity(' + ResourcePathExpression(ASource.IdentityPath, 'NyxResourcePath') + ')');
  for LIndex := 0 to ASource.Schema.Count - 1 do
  begin
    LField := ASource.Schema.FieldAt(LIndex);
    LLine := AIndent + '    .' + CMethods[LField.Kind] + '(' + CFields[LField.Kind] + '(' +
      PascalString(LField.Name) + ')';
    { The public shorthand selects the exact field name. Spell a structural
      path only when it contributes meaning to the crafted source. }

    if ASource.FieldPath(LIndex).ToData.ToJSON <>
      NyxResourcePath.Field(LField.Name).ToData.ToJSON then
    begin
      LLine := LLine + ', ' + ResourcePathExpression(ASource.FieldPath(LIndex), 'NyxResourcePath');
    end;
    ALines.Add(LLine + ')');
  end;
  ALines.Add(AIndent + ');');
end;

{ Emit one authored definition through the same public types for full generation
  and handwritten continuation. Borrowed immutable snapshots/recipes outlive no
  caller; resource declarations do not resolve or fetch their source. }
procedure EmitCollectionDefinition(const ADefaults: INyxCollectionDefaults;
  const AKey: TNyxCollectionRef; ALines: TNyxStrings;
  const AReceiver, AIndent: TNyxText);
const
  CMethods: array[TNyxStateKind] of TNyxText =
    ('Text', 'Boolean', 'Integer', 'Number');
  CFields: array[TNyxStateKind] of TNyxText =
    ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
var
  LFieldIndex: Integer;
  LRowIndex: Integer;
  LDefinition: INyxCollectionSnapshot;
  LSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
  LRow: TNyxCollectionItem;
  LLine: TNyxText;
  LKey: TNyxText;
  LSource: TNyxResourceRows;
begin
  LDefinition := ADefaults.Snapshot(AKey);

  if NyxCollectionResourceSource(ADefaults, AKey, LSource) then
  begin
    EmitResourceCollection(AKey, LSource, ALines, AReceiver, AIndent);
    Exit;
  end;
  LSchema := LDefinition.Schema;
  LKey := 'NyxCollection(' + PascalString(LDefinition.Key.Name) + ')';
  ALines.Add(AIndent + AReceiver + '.Collections.Define(');
  ALines.Add(AIndent + '  ' + LKey + ',');
  LLine := AIndent + '  NyxCollectionSchema';

  if LSchema.Count = 0 then
  begin
    LLine := LLine + ',';
  end;
  ALines.Add(LLine);
  for LFieldIndex := 0 to LSchema.Count - 1 do
  begin
    LField := LSchema.FieldAt(LFieldIndex);

    if LField.Domain.Defined then
    begin
      LLine := AIndent + '    .' + CMethods[LField.Kind] + '(' + CFields[LField.Kind] +
        '(' + PascalString(LField.Name) + '), ' + PascalCollectionValue(LField.DefaultValue) +
        ', ' + DomainExpression(LField.Domain) + ')';
    end
    else
    begin
      { Explicit descriptor absence has a typed default and no constraint;
        it cannot become an unconstrained but Defined domain silently. }
      LLine := AIndent + '    .Field(' + PascalString(LField.Name) +
        ', TNyxStateValue.From' + CMethods[LField.Kind] +
        '(' + PascalCollectionValue(LField.DefaultValue) + '), NyxNoDomain)';
    end;

    if LFieldIndex = LSchema.Count - 1 then
    begin
      LLine := LLine + ',';
    end;
    ALines.Add(LLine);
  end;
  ALines.Add(AIndent + '  [');
  for LRowIndex := 0 to LDefinition.Count - 1 do
  begin
    LRow := LDefinition.ItemAt(LRowIndex);
    LLine := AIndent + '    NyxCollectionItem(NyxItem(' + LKey + ', ' + PascalString(LRow.Ref.ID) + '))';
    for LFieldIndex := 0 to LSchema.Count - 1 do
    begin
      LField := LSchema.FieldAt(LFieldIndex);

      if not LRow.FieldValue(LFieldIndex).SameValue(LField.DefaultValue) then
      begin
        ALines.Add(LLine);
        LLine := AIndent + '      .WithValue(' + CFields[LField.Kind] +
          '(' + PascalString(LField.Name) + '), ' + PascalCollectionValue(LRow.FieldValue(LFieldIndex)) + ')';
      end;
    end;

    if LRowIndex < LDefinition.Count - 1 then
    begin
      LLine := LLine + ',';
    end;
    ALines.Add(LLine);
  end;
  ALines.Add(AIndent + '  ]);');
  ALines.Add('');
end;

{ Default construction retains the established ordered, readable source. Each
  application materializes independent runtime stores from these declarations. }
procedure EmitCollections(ADocument: TNyxDocument; ALines: TNyxStrings);
var
  LIndex: Integer;
begin

  if ADocument.Collections.Count = 0 then
  begin
    Exit;
  end;
  ALines.Add('');
  ALines.Add('    // Authored collection defaults; each application owns independent runtime stores.');
  for LIndex := 0 to ADocument.Collections.Count - 1 do
  begin
    EmitCollectionDefinition(ADocument.Collections, ADocument.Collections.Key(LIndex),
      ALines, 'Result', '    ');
  end;
end;
