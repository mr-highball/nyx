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

unit nyx.collections.query.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.collections,
  nyx.collections.query, nyx.collections.view.types, nyx.menu.editor;

type
  { Each action captures a complete independent policy. Hosts decide whether to
    publish it through an undoable candidate or configure a live runtime view.
    Filter actions target an exact predicate path; sort actions target an index. }
  TNyxQueryEditorAction = (nqeSave, nqeAnd, nqeOr, nqeNegate, nqeRemove,
    nqeAddSort, nqeSortEarlier, nqeSortLater, nqeRemoveSort,
    nqeClearFilter, nqeClearSort);
  TNyxQueryEditorField = (nqfField, nqfComparison, nqfValue,
    nqfTextMatch, nqfDirection);
  TNyxQueryEditorChange = record
    Owner: TNyxControlRef;
    Baseline: TNyxText;
    Query: TNyxCollectionQuery;
  end;
  { Reuse the owned scalar-form draft protocol already shared by Nyx editors.
    The query compound supplies its own exact owner/baseline and no registry
    reference. Invalid partial text survives unrelated shell repaint, but never
    crosses a changed binding/schema or project. No document/widget is retained. }
  TNyxQueryEditorDraft = TNyxMenuEditorDraft;

{ Exact binding plus schema/default/domain baseline, independent of row contents.
  A row publication does not invalidate input; a field/binding change does. }
function NyxQueryEditorBaseline(const ASchema: TNyxCollectionSchema;
  const ASpec: TNyxCollectionViewSpec): TNyxText;
{ Public compound built from specialized Nyx controls and named parts. Borrows
  schema/specification only during construction, and owns copied metadata/children.
  Every predicate family, nested All/Any/Not and ordered sort key is editable.
  Adding uses the chosen typed field's default; remove/re-add changes its family.
  Text matching is explicitly scalar/exact or ASCII-insensitive, never locale
  coercion. Save and structural actions capture the whole form as one policy. }
function NewNyxQueryEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  const ASchema: TNyxCollectionSchema; const ASpec: TNyxCollectionViewSpec): INyxCard;
{ Stable typed identities for host navigation and physical-input consumers.
  Predicate children append their ordinal to their parent's ID. }
function NyxQueryEditorFieldID(const APrefix: TNyxText;
  AField: TNyxQueryEditorField): TNyxText;
function NyxQueryEditorActionID(const APrefix: TNyxText;
  AAction: TNyxQueryEditorAction): TNyxText;
function NyxQueryEditorPredicateID(const AID: TNyxText): TNyxText;
function NyxQueryEditorSortID(const AID: TNyxText; AIndex: Integer): TNyxText;
{ False denotes an unrelated button. Mounted identity, closed choices, exact
  scalar notation, bounded tree/order and duplicate sort fields are validated
  before returning a value. Capture mutates neither tree. Clearing/removing an
  invalid subtree can recover it; unrelated invalid fields still refuse atomically.
  The host must compare Baseline against its current schema/binding and retain
  the captured Owner when applying, rather than using a newer selection. }
function CaptureNyxQueryEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxQueryEditorChange): Boolean;

implementation

uses
  SysUtils, nyx.data, nyx.state, nyx.contract, nyx.responsive;

const
  CEditor = 'nyx.query-editor';
  CAction = 'nyx.query-editor.action';
  CTarget = 'nyx.query-editor.target';
  CPolicy = 'nyx.query-editor.policy';
  CFields = 'nyx.query-editor.fields';
  CFieldNames: array[TNyxQueryEditorField] of TNyxText =
    ('field', 'comparison', 'value', 'text-match', 'direction');
  CActions: array[TNyxQueryEditorAction] of TNyxText =
    ('save', 'and', 'or', 'negate', 'remove', 'add-sort', 'sort-earlier',
     'sort-later', 'remove-sort', 'clear-filter', 'clear-sort');
  CComparisons: array[TNyxQueryComparison] of TNyxText =
    ('Equals', 'Does not equal', 'Less than', 'At most', 'Greater than',
     'At least', 'Contains', 'Starts with', 'Ends with');
  CComparisonWire: array[TNyxQueryComparison] of TNyxText =
    ('equal', 'notEqual', 'less', 'atMost', 'greater', 'atLeast',
     'contains', 'startsWith', 'endsWith');
  CMatches: array[TNyxQueryTextComparison] of TNyxText =
    ('Exact Unicode scalars', 'ASCII case insensitive');
  CMatchWire: array[TNyxQueryTextComparison] of TNyxText =
    ('exact', 'asciiInsensitive');
  CDirections: array[TNyxSortDirection] of TNyxText = ('Ascending', 'Descending');
  CDirectionWire: array[TNyxSortDirection] of TNyxText = ('ascending', 'descending');

function NyxQueryEditorFieldID(const APrefix: TNyxText;
  AField: TNyxQueryEditorField): TNyxText;
begin
  Result := APrefix + TNyxText('-') + CFieldNames[AField];
end;

function NyxQueryEditorActionID(const APrefix: TNyxText;
  AAction: TNyxQueryEditorAction): TNyxText;
begin
  Result := APrefix + TNyxText('-') + CActions[AAction];
end;

function NyxQueryEditorPredicateID(const AID: TNyxText): TNyxText;
begin
  Result := AID + TNyxText('-filter');
end;

function NyxQueryEditorSortID(const AID: TNyxText; AIndex: Integer): TNyxText;
begin
  Result := AID + TNyxText('-sort-') + TNyxText(IntToStr(AIndex));
end;

function ScalarData(const AValue: TNyxStateValue): TNyxDataValue;
begin
  AValue.Validate;
  case Ord(AValue.Kind) of
    Ord(nskText):
      begin
        Result := NyxData(AValue.TextValue);
      end;
    Ord(nskBoolean):
      begin
        Result := NyxData(AValue.BooleanValue);
      end;
    Ord(nskInteger):
      begin
        Result := NyxData(AValue.IntegerValue);
      end;
    Ord(nskNumber):
      begin
        Result := NyxData(AValue.NumberValue);
      end;
  else
    begin
      raise ENyxCollection.Create('Unknown query editor scalar family');
    end;
  end;
end;

function SchemaFields(const ASchema: TNyxCollectionSchema): TNyxDataValue;
var
  LFields: array of TNyxDataValue;
  LIndex: Integer;
  LField: TNyxCollectionField;
begin
  ASchema.Validate;
  SetLength(LFields, ASchema.Count);
  for LIndex := 0 to ASchema.Count - 1 do
  begin
    LField := ASchema.FieldAt(LIndex);
    LFields[LIndex] := NyxObject([
      NyxField('name', NyxData(LField.Name)),
      NyxField('kind', NyxData(NyxStateKindName(LField.Kind))),
      NyxField('default', ScalarData(LField.DefaultValue)),
      NyxField('domain', LField.Domain.ToData)]);
  end;
  Result := NyxArray(LFields);
end;

function NyxQueryEditorBaseline(const ASchema: TNyxCollectionSchema;
  const ASpec: TNyxCollectionViewSpec): TNyxText;
begin
  ASpec.Validate;

  if not ASpec.Defined then
  begin
    raise ENyxCollection.Create('A query editor requires a bound collection view');
  end;
  ASpec.QueryPolicy.Validate(ASchema);
  Result := NyxObject([NyxField('binding', ASpec.ToData),
    NyxField('fields', SchemaFields(ASchema))]).ToJSON;
end;

function NewNyxQueryEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  const ASchema: TNyxCollectionSchema; const ASpec: TNyxCollectionViewSpec): INyxCard;
var
  LBaseline: TNyxText;
  LFields: TNyxDataValue;
  LOptions: TNyxText;
  LIndex: Integer;
  LSort: TNyxCollectionSort;
  LCard: INyxCard;
  LPolicy: TNyxCollectionQuery;
  LFilter: INyxCollectionPredicate;

  procedure Button(const AParent: INyxControl; const APrefix: TNyxText;
    AAction: TNyxQueryEditorAction; const ACaption: TNyxText;
    AEnabled: Boolean = True);
  begin
    AParent.Add(NewNyxButton(NyxQueryEditorActionID(APrefix, AAction)).Configure
      .PartName(NyxPart(CActions[AAction])).Text(ACaption).Enabled(AEnabled)
      .Extension(CEditor, AID).Extension(CAction, CActions[AAction])
      .Extension(CTarget, APrefix).Done);
  end;

  procedure Choice(const AParent: INyxControl; const APrefix: TNyxText;
    AField: TNyxQueryEditorField; const ATitle, AOptions, AValue: TNyxText);
  begin
    AParent.Add(NewNyxSelect(NyxQueryEditorFieldID(APrefix, AField)).Configure
      .PartName(NyxPart(CFieldNames[AField])).Text(ATitle)
      .Items(AOptions).Value(AValue).Done);
  end;

  procedure Predicate(const AParent: INyxControl;
    const AValue: INyxCollectionPredicate; const APrefix, APart: TNyxText);
  var
    LRow: INyxCard;
    LComparison: TNyxQueryComparison;
    LChoices: TNyxText;
    LValue: TNyxStateValue;
    LText: TNyxText;
    LChild: Integer;
  begin
    LRow := NewNyxCard(APrefix);
    LRow.Configure.Layout(nlColumn).Gap(6).Padding(8).Compound(True)
      .PartName(NyxPart(APart)).WhenViewport(TNyxViewportWidth.Below(640))
      .Padding(3).Done;

    if AValue.Kind = nqpField then
    begin
      LRow.Add(NewNyxLabel(APrefix + TNyxText('-caption')).Configure
        .PartName(NyxPart('caption')).Text(AValue.FieldName + TNyxText(' / ') +
          NyxStateKindName(AValue.FieldKind)).Done);
      LChoices := '';
      for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
      begin

        if ((AValue.FieldKind = nskText) and
          (LComparison in [nqcEqual, nqcNotEqual, nqcContains, nqcStartsWith, nqcEndsWith])) or
          ((AValue.FieldKind = nskBoolean) and
          (LComparison in [nqcEqual, nqcNotEqual])) or
          ((AValue.FieldKind in [nskInteger, nskNumber]) and
          (LComparison in [nqcEqual, nqcNotEqual, nqcLess, nqcAtMost, nqcGreater, nqcAtLeast])) then
        begin

          if LChoices <> '' then
          begin
            LChoices := LChoices + #10;
          end;
          LChoices := LChoices + CComparisons[LComparison];
        end;
      end;
      Choice(LRow, APrefix, nqfComparison, 'Comparison', LChoices,
        CComparisons[AValue.Comparison]);
      LValue := AValue.Expected;

      if AValue.FieldKind = nskBoolean then
      begin
        LRow.Add(NewNyxCheckbox(NyxQueryEditorFieldID(APrefix, nqfValue)).Configure
          .PartName(NyxPart('value')).Text('Expected value').Value(LValue.BooleanValue).Done);
      end
      else
      begin
        case AValue.FieldKind of
          nskText:
            begin
              LText := LValue.TextValue;
            end;
          nskInteger:
            begin
              LText := TNyxText(IntToStr(LValue.IntegerValue));
            end;
          nskNumber:
            begin
              LText := LValue.NumberText;
            end;
        else
          begin
            raise ENyxCollection.Create('Unknown query editor scalar family');
          end;
        end;
        LRow.Add(NewNyxInput(NyxQueryEditorFieldID(APrefix, nqfValue)).Configure
          .PartName(NyxPart('value')).Text('Expected ' + NyxStateKindName(AValue.FieldKind))
          .Value(LText).Done);
      end;

      if AValue.FieldKind = nskText then
      begin
        Choice(LRow, APrefix, nqfTextMatch, 'Text matching',
          CMatches[nqtExact] + #10 + CMatches[nqtAsciiInsensitive],
          CMatches[AValue.TextComparison]);
      end;
    end
    else
    begin
      case AValue.Kind of
        nqpAll:
          begin
            LText := 'All conditions';
          end;
        nqpAny:
          begin
            LText := 'Any condition';
          end;
        nqpNot:
          begin
            LText := 'Not';
          end;
      else
        begin
          raise ENyxCollection.Create('Unknown query editor predicate kind');
        end;
      end;
      LRow.Add(NewNyxLabel(APrefix + TNyxText('-caption')).Configure
        .PartName(NyxPart('caption')).Text(LText).Done);
      for LChild := 0 to AValue.ChildCount - 1 do
      begin
        Predicate(LRow, AValue.Child(LChild),
          APrefix + TNyxText('-') + TNyxText(IntToStr(LChild)),
          TNyxText('condition-') + TNyxText(IntToStr(LChild)));
      end;
    end;
    Button(LRow, APrefix, nqeAnd, 'AND chosen field');
    Button(LRow, APrefix, nqeOr, 'OR chosen field');
    Button(LRow, APrefix, nqeNegate, 'Toggle NOT');
    Button(LRow, APrefix, nqeRemove, 'Remove condition');
    AParent.Add(LRow);
  end;

  function FieldCaption(const AName: TNyxText): TNyxText;
  var
    LFieldIndex: Integer;
  begin
    for LFieldIndex := 0 to LFields.Count - 1 do
    begin

      if LFields.Item(LFieldIndex).Field('name').AsText = AName then
      begin
        Exit(NyxMenuEditorChoiceCaption(LFieldIndex, AName));
      end;
    end;
    raise ENyxCollection.Create('Query field is no longer in this schema');
  end;

begin
  LBaseline := NyxQueryEditorBaseline(ASchema, ASpec);
  LFields := SchemaFields(ASchema);
  LPolicy := ASpec.QueryPolicy;
  LOptions := '';
  for LIndex := 0 to LFields.Count - 1 do
  begin

    if LIndex > 0 then
    begin
      LOptions := LOptions + #10;
    end;
    LOptions := LOptions + NyxMenuEditorChoiceCaption(LIndex,
      LFields.Item(LIndex).Field('name').AsText);
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Layout(nlColumn).Gap(8).Padding(12).Compound(True)
    .WhenViewport(TNyxViewportWidth.Below(640)).Padding(6).Done;
  Result.Node.SetProp(NyxMenuFormOwnerKey, AOwner.ID)
    .SetProp(NyxMenuFormBaselineKey, LBaseline)
    .SetProp(NyxMenuFormReferenceKey, '')
    .SetProp(CPolicy, LPolicy.ToData.ToJSON).SetProp(CFields, LFields.ToJSON);
  Result.Add(NewNyxHeading(AID + TNyxText('-title')).Configure
    .PartName(NyxPart('title')).Text('Filter and sort').Done);
  Result.Add(NewNyxLabel(AID + TNyxText('-help')).Configure
    .PartName(NyxPart('help')).Text(
    'Choose a field to add a condition or sort key. Edit values, then Apply. ' +
    'Each action saves the complete query as one Undo step; source rows remain intact.').Done);
  Choice(Result, AID, nqfField, 'Field to add', LOptions,
    NyxMenuEditorChoiceCaption(0, LFields.Item(0).Field('name').AsText));
  LFilter := LPolicy.Filter;

  if LFilter <> nil then
  begin
    Predicate(Result, LFilter, NyxQueryEditorPredicateID(AID), 'filter');
  end
  else
  begin
    Result.Add(NewNyxLabel(AID + TNyxText('-no-filter')).Configure
      .PartName(NyxPart('no-filter')).Text('All source rows are visible').Done);
    Button(Result, NyxQueryEditorPredicateID(AID), nqeAnd, 'Add condition');
  end;
  for LIndex := 0 to LPolicy.SortCount - 1 do
  begin
    LSort := LPolicy.SortAt(LIndex);
    LCard := NewNyxCard(NyxQueryEditorSortID(AID, LIndex));
    LCard.Configure.Layout(nlColumn).Gap(6).Padding(8).Compound(True)
      .PartName(NyxPart(TNyxText('sort-') + TNyxText(IntToStr(LIndex)))).Done;
    Choice(LCard, LCard.ID, nqfField, 'Sort key ' + TNyxText(IntToStr(LIndex + 1)),
      LOptions, FieldCaption(LSort.FieldName));
    Choice(LCard, LCard.ID, nqfDirection, 'Direction',
      CDirections[nsdAscending] + #10 + CDirections[nsdDescending], CDirections[LSort.Direction]);
    Choice(LCard, LCard.ID, nqfTextMatch, 'Text matching (text fields)',
      CMatches[nqtExact] + #10 + CMatches[nqtAsciiInsensitive], CMatches[LSort.TextComparison]);
    Button(LCard, LCard.ID, nqeSortEarlier, 'Move earlier', LIndex > 0);
    Button(LCard, LCard.ID, nqeSortLater, 'Move later', LIndex < LPolicy.SortCount - 1);
    Button(LCard, LCard.ID, nqeRemoveSort, 'Remove sort key');
    Result.Add(LCard);
  end;
  Button(Result, AID, nqeAddSort, 'Add chosen sort key', LPolicy.SortCount < NyxMaximumQuerySorts);
  Button(Result, AID, nqeSave, 'Apply query');
  Button(Result, AID, nqeClearFilter, 'Clear filter', LFilter <> nil);
  Button(Result, AID, nqeClearSort, 'Clear sorting', LPolicy.SortCount > 0);
end;

function CaptureNyxQueryEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxQueryEditorChange): Boolean;
var
  LEditor: TNyxNode;
  LEditorID: TNyxText;
  LTarget: TNyxText;
  LAction: TNyxQueryEditorAction;
  LActionIndex: Integer;
  LOriginal: TNyxCollectionQuery;
  LFields: TNyxDataValue;
  LFilter: INyxCollectionPredicate;
  LNew: INyxCollectionPredicate;
  LSorts: array of TNyxDataValue;
  LTemporary: TNyxDataValue;
  LSelected: Integer;
  LOther: Integer;
  LIndex: Integer;
  LTargetFound: Boolean;

  function Value(const APrefix: TNyxText; AField: TNyxQueryEditorField): TNyxText;
  var
    LControl: TNyxNode;
  begin
    LControl := LEditor.Find(NyxQueryEditorFieldID(APrefix, AField));

    if LControl = nil then
    begin
      raise ENyxCollection.Create('Query editor field is no longer mounted');
    end;
    Result := LControl.Prop('value');
  end;

  function Choice(const AValue: TNyxText; const AChoices: array of TNyxText): Integer;
  var
    LChoice: Integer;
  begin
    for LChoice := 0 to High(AChoices) do
    begin

      if AValue = AChoices[LChoice] then
      begin
        Exit(LChoice);
      end;
    end;
    raise ENyxCollection.Create('Choose a displayed query option');
  end;

  function Field(const APrefix: TNyxText): TNyxDataValue;
  var
    LChoice: Integer;
    LValue: TNyxText;
  begin
    LValue := Value(APrefix, nqfField);
    for LChoice := 0 to LFields.Count - 1 do
    begin

      if LValue = NyxMenuEditorChoiceCaption(LChoice,
        LFields.Item(LChoice).Field('name').AsText) then
      begin
        Exit(LFields.Item(LChoice));
      end;
    end;
    raise ENyxCollection.Create('Choose a field from this exact schema');
  end;

  function NewPredicate: INyxCollectionPredicate;
  var
    LField: TNyxDataValue;
  begin
    LField := Field(LEditorID);
    Result := NyxPredicateFromData(NyxObject([
      NyxField('op', NyxData('field')), NyxField('field', LField.Field('name')),
      NyxField('kind', LField.Field('kind')), NyxField('comparison', NyxData('equal')),
      NyxField('textComparison', NyxData('exact')), NyxField('value', LField.Field('default'))]));
  end;

  function Predicate(const AOriginal: INyxCollectionPredicate;
    const APrefix: TNyxText): INyxCollectionPredicate;
  var
    LChild: INyxCollectionPredicate;
    LChildIndex: Integer;
    LValue: TNyxDataValue;
    LInteger: Integer;
    LNumber: Double;
    LText: TNyxText;
    LComparison: Integer;
    LMatch: Integer;
  begin
    Result := nil;

    if (APrefix = LTarget) and (LAction = nqeRemove) then
    begin
      LTargetFound := True;
      Exit;
    end;

    if AOriginal.Kind = nqpField then
    begin
      LText := Value(APrefix, nqfValue);
      case Ord(AOriginal.FieldKind) of
        Ord(nskText):
          begin
            LValue := NyxData(LText);
          end;
        Ord(nskBoolean):
          begin

            if (LText <> 'true') and (LText <> 'false') then
            begin
              raise ENyxCollection.Create('Query value requires an explicit Boolean');
            end;
            LValue := NyxData(LText = 'true');
          end;
        Ord(nskInteger):
          begin

            if not TryNyxStateInteger(LText, LInteger) then
            begin
              raise ENyxCollection.Create('Query value requires an exact signed Integer');
            end;
            LValue := NyxData(LInteger);
          end;
        Ord(nskNumber):
          begin

            if not TryNyxStateNumber(LText, LNumber) then
            begin
              raise ENyxCollection.Create('Query value requires a finite decimal Number');
            end;
            LValue := NyxData(LNumber);
          end;
      else
        begin
          raise ENyxCollection.Create('Unknown query editor scalar family');
        end;
      end;
      LComparison := Choice(Value(APrefix, nqfComparison), CComparisons);
      LMatch := Ord(nqtExact);

      if AOriginal.FieldKind = nskText then
      begin
        LMatch := Choice(Value(APrefix, nqfTextMatch), CMatches);
      end;
      Result := NyxPredicateFromData(NyxObject([
        NyxField('op', NyxData('field')), NyxField('field', NyxData(AOriginal.FieldName)),
        NyxField('kind', NyxData(NyxStateKindName(AOriginal.FieldKind))),
        NyxField('comparison', NyxData(CComparisonWire[TNyxQueryComparison(LComparison)])),
        NyxField('textComparison', NyxData(CMatchWire[TNyxQueryTextComparison(LMatch)])),
        NyxField('value', LValue)]));
    end
    else
    begin
      for LChildIndex := 0 to AOriginal.ChildCount - 1 do
      begin
        LChild := Predicate(AOriginal.Child(LChildIndex),
          APrefix + TNyxText('-') + TNyxText(IntToStr(LChildIndex)));

        if LChild = nil then
        begin
          Continue;
        end;

        if Result = nil then
        begin
          Result := LChild;
        end
        else if AOriginal.Kind = nqpAll then
        begin
          Result := Result.AndAlso(LChild);
        end
        else
        begin
          Result := Result.OrElse(LChild);
        end;
      end;

      if (AOriginal.Kind = nqpNot) and (Result <> nil) then
      begin
        Result := Result.Negated;
      end;
    end;

    if APrefix = LTarget then
    begin
      LTargetFound := True;
      case LAction of
        nqeAnd:
          begin
            Result := Result.AndAlso(LNew);
          end;
        nqeOr:
          begin
            Result := Result.OrElse(LNew);
          end;
        nqeNegate:
          begin

            if Result.Kind = nqpNot then
            begin
              Result := Result.Child(0);
            end
            else
            begin
              Result := Result.Negated;
            end;
          end;
      else
        begin
          { Save/sort/clear actions keep this already captured predicate. }
        end;
      end;
    end;
  end;

  function Sort(const APrefix: TNyxText): TNyxDataValue;
  var
    LField: TNyxDataValue;
    LDirection: Integer;
    LMatch: Integer;
  begin
    LField := Field(APrefix);
    LDirection := Choice(Value(APrefix, nqfDirection), CDirections);
    LMatch := Choice(Value(APrefix, nqfTextMatch), CMatches);
    Result := NyxObject([
      NyxField('field', LField.Field('name')), NyxField('kind', LField.Field('kind')),
      NyxField('direction', NyxData(CDirectionWire[TNyxSortDirection(LDirection)])),
      NyxField('textComparison', NyxData(CMatchWire[TNyxQueryTextComparison(LMatch)]))]);
  end;

begin
  Result := False;
  AChange := Default(TNyxQueryEditorChange);

  if (AButton = nil) or (AShellRoot = nil) or (AButton.Prop(CEditor) = '') then
  begin
    Exit;
  end;
  LEditorID := AButton.Prop(CEditor);
  LEditor := AShellRoot.Find(LEditorID);

  if (LEditor = nil) or (LEditor.Find(AButton.ID) <> AButton) or
    (AButton.ProjectionKind <> NyxKindName(nkButton)) or
    (AButton.Prop('enabled', 'true') <> 'true') then
  begin
    raise ENyxCollection.Create('Query editor action is no longer mounted or enabled');
  end;
  LActionIndex := Choice(AButton.Prop(CAction), CActions);
  LAction := TNyxQueryEditorAction(LActionIndex);
  LTarget := AButton.Prop(CTarget);
  AChange.Owner := NyxControl(LEditor.Prop(NyxMenuFormOwnerKey));
  AChange.Baseline := LEditor.Prop(NyxMenuFormBaselineKey);

  if AChange.Baseline = '' then
  begin
    raise ENyxCollection.Create('Query editor requires its exact baseline');
  end;
  LOriginal := TNyxCollectionQuery.FromData(TNyxDataValue.ParseJSON(LEditor.Prop(CPolicy)));
  LFields := TNyxDataValue.ParseJSON(LEditor.Prop(CFields));
  LTargetFound := False;
  LNew := nil;

  if LAction in [nqeAnd, nqeOr] then
  begin
    LNew := NewPredicate;
  end;
  LFilter := nil;

  if LAction <> nqeClearFilter then
  begin
    LFilter := LOriginal.Filter;

    if LFilter <> nil then
    begin
      LFilter := Predicate(LFilter, NyxQueryEditorPredicateID(LEditorID));
    end
    else if (LAction = nqeAnd) and (LTarget = NyxQueryEditorPredicateID(LEditorID)) then
    begin
      LFilter := LNew;
      LTargetFound := True;
    end;
  end;

  if (LAction in [nqeAnd, nqeOr, nqeNegate, nqeRemove]) and not LTargetFound then
  begin
    raise ENyxCollection.Create('Query predicate path is no longer present');
  end;
  LSorts := nil;
  LSelected := -1;

  if LAction <> nqeClearSort then
  begin
    SetLength(LSorts, LOriginal.SortCount);
    for LIndex := 0 to LOriginal.SortCount - 1 do
    begin

      if LTarget = NyxQueryEditorSortID(LEditorID, LIndex) then
      begin
        LSelected := LIndex;
      end;

      if (LAction = nqeRemoveSort) and (LSelected = LIndex) then
      begin
        Continue;
      end;
      LSorts[LIndex] := Sort(NyxQueryEditorSortID(LEditorID, LIndex));
    end;
  end;

  if LAction = nqeAddSort then
  begin
    LTemporary := Field(LEditorID);
    SetLength(LSorts, Length(LSorts) + 1);
    LSorts[High(LSorts)] := NyxObject([
      NyxField('field', LTemporary.Field('name')), NyxField('kind', LTemporary.Field('kind')),
      NyxField('direction', NyxData('ascending')), NyxField('textComparison', NyxData('exact'))]);
  end;

  if LAction in [nqeSortEarlier, nqeSortLater, nqeRemoveSort] then
  begin

    if LSelected < 0 then
    begin
      raise ENyxCollection.Create('Query sort key is no longer present');
    end;

    if LAction = nqeRemoveSort then
    begin
      for LIndex := LSelected to High(LSorts) - 1 do
      begin
        LSorts[LIndex] := LSorts[LIndex + 1];
      end;
      SetLength(LSorts, Length(LSorts) - 1);
    end
    else
    begin
      LOther := LSelected - 1;

      if LAction = nqeSortLater then
      begin
        LOther := LSelected + 1;
      end;

      if (LOther < 0) or (LOther >= Length(LSorts)) then
      begin
        raise ENyxCollection.Create('Query sort order has reached its boundary');
      end;
      LTemporary := LSorts[LSelected];
      LSorts[LSelected] := LSorts[LOther];
      LSorts[LOther] := LTemporary;
    end;
  end;
  LTemporary := NyxNull;

  if LFilter <> nil then
  begin
    LTemporary := LFilter.ToData;
  end;
  AChange.Query := NyxCollectionQuery;

  if (LFilter <> nil) or (Length(LSorts) > 0) then
  begin
    AChange.Query := TNyxCollectionQuery.FromData(NyxObject([
      NyxField('version', NyxData(1)), NyxField('filter', LTemporary),
      NyxField('order', NyxArray(LSorts))]));
  end;
  Result := True;
end;

end.
