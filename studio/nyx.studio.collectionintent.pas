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


unit nyx.studio.collectionintent;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.data, nyx.state, nyx.collections,
  nyx.collections.view.types, nyx.collections.selection, nyx.collections.query,
  nyx.studio.authoring, nyx.collections.columns, nyx.collections.paging,
  nyx.collections.query.policy, nyx.collections.arrangement, nyx.formatting,
  nyx.collections.workbench.policy, nyx.collections.presentation, nyx.collections.alignment,
  nyx.collections.inspection, nyx.collections.recovery.policy;

type
  { Closed ordinary editor operations. These describe intent, not a saved
    collection replacement or an executable closure. Append new choices rather
    than changing existing private-wire ordinals. }
  TNyxStudioCollectionAction = (scaCreate, scaRemove, scaAddField, scaDefault,
    scaAddRow, scaRemoveRow, scaCell, scaBind, scaScope, scaTitle, scaMode,
    scaParent, scaRemoveColumn, scaAddColumn, scaClear, scaInherit, scaSelection, scaQuery,
    scaColumnContent, scaColumnFixed, scaColumnFlex, scaColumnMinimum,
    scaColumnMaximum, scaColumnDefault, scaPageSize, scaInitialPage, scaDefaultPaging,
    scaTableQuery, scaColumnQuery, scaDefaultTableQuery, scaDefaultColumnQuery,
    scaTableLayout, scaColumnLayout, scaDefaultTableLayout, scaDefaultColumnLayout,
    scaColumnFormat, scaDefaultColumnFormat, scaTableWorkbench, scaDefaultTableWorkbench,
    scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment, scaInspection, scaDefaultInspection,
    scaDraftRecovery, scaDefaultDraftRecovery);

  { Optional family-qualified field reference. Normal construction requires
    one of the four distinct public field types. The undefined value denotes
    no field (for example removing a tree's parent field), never an empty name.
    All copies own immutable text; there are no schema/store/target pointers. }
  TNyxStudioCollectionFieldRef = record
  private
    FDefined: Boolean;
    FName: TNyxText;
    FKind: TNyxStateKind;
    function GetName: TNyxText;
    function GetKind: TNyxStateKind;
  public
    class function Text(const AField: TNyxTextFieldRef): TNyxStudioCollectionFieldRef; static;
    class function Boolean(const AField: TNyxBooleanFieldRef): TNyxStudioCollectionFieldRef; static;
    class function Integer(const AField: TNyxIntegerFieldRef): TNyxStudioCollectionFieldRef; static;
    class function Number(const AField: TNyxNumberFieldRef): TNyxStudioCollectionFieldRef; static;
    { Explicit inspector/descriptor boundary: schema metadata supplies its
      admitted family. Author code uses the typed constructors above. }
    class function FromMetadata(const AField: TNyxCollectionField):
      TNyxStudioCollectionFieldRef; static;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue):
      TNyxStudioCollectionFieldRef; static;
    function SameReference(const AOther: TNyxStudioCollectionFieldRef): Boolean;
    property Defined: Boolean read FDefined;
    { Reading an undefined reference raises before any editor mutation. }
    property Name: TNyxText read GetName;
    property Kind: TNyxStateKind read GetKind;
  end;

  { Value-only editor proposal. Key is required for every operation, including
    a freshly allocated create name. Item is collection-scoped and used only by
    row addition/removal/cell edits. Field retains its family through replay.
    Scalar notation stays text until isolated parsing so partial/newer input can
    remain visible without entering the accepted document. Value is also the
    open caption for a column-title operation. Projection qualifies view edits;
    Scope/Mode/SelectionMode and Kind are closed choices for their own actions.
    Unused fields must keep their documented default value, enforced by Validate. }
  TNyxStudioCollectionIntent = record
    Action: TNyxStudioCollectionAction;
    Key: TNyxCollectionRef;
    Item: TNyxItemRef;
    Field: TNyxStudioCollectionFieldRef;
    Input: TNyxStudioStateInput;
    Value: TNyxText;
    Kind: TNyxStateKind;
    Mode: TNyxCollectionCellMode;
    Scope: TNyxCollectionScope;
    SelectionMode: TNyxSelectionMode;
    Projection: TNyxCollectionProjection;
    { Authored predicates and command capabilities are separate choices. Only
      scaQuery uses Query; its undefined value clears the authored query. The
      two policy setters require a defined matching policy; the default actions
      carry no policy and clear only that raw declaration, never inheritance.
      Every query/policy action carries the full effective-spec/schema baseline
      so an old form cannot replace a newer binding's independent choices. }
    Query: TNyxCollectionQuery;
    TableQuery: TNyxTableQueryPolicy;
    ColumnQuery: TNyxColumnQueryPolicy;
    QueryBaseline: TNyxText;
    { Complete authored layout choices have their own exact schema/binding
      baseline. Absence differs from an explicitly applied default policy;
      runtime user preferences never enter this source/history proposal. }
    TableLayout: TNyxTableLayoutPolicy;
    ColumnLayout: TNyxColumnLayoutPolicy;
    { Formatting is presentation-only and carries complete immutable locale,
      formatter and rule facts. It shares the full layout/display form baseline. }
    ColumnFormat: TNyxValueFormat;
    { Complete copied action choices share the same exact binding/schema
      baseline. Hosts, runtime bounds and live selection never enter an intent. }
    TableWorkbench: TNyxTableWorkbenchPolicy;
    { Authored text and spacing have separate presence/reset choices. Runtime
      preferences, host metric limits and rendered truncation never enter them. }
    TablePresentation: TNyxTablePresentationPolicy;
    ColumnText: TNyxColumnTextPolicy;
    { Logical axes are separate from text overflow and retain their own reset.
      Empty is reserved for the explicit default action, never a setter. }
    ColumnAlignment: TNyxColumnAlignmentPolicy;
    LayoutBaseline: TNyxText;
    { Saved initial follow/retention choices are detached scalars. The exact
      effective binding/schema baseline prevents an old form from replacing a
      newer declaration. Runtime pause, position and drafts are never authored. }
    Inspection: TNyxCollectionInspectionPolicy;
    InspectionBaseline: TNyxText;
    DraftRecovery: TNyxCollectionDraftRecoveryPolicy;
    DraftRecoveryBaseline: TNyxText;
    procedure Validate;
    function SameIntent(const AOther: TNyxStudioCollectionIntent): Boolean;
    { Strict private descriptor. Null is not an intent. Unknown/extra members,
      wrong scalar types, undefined identities and unused payload reject. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue):
      TNyxStudioCollectionIntent; static;
  end;

{ View operations belong to the captured authored control; data operations belong
  to the exact document-owned collection. No positional chrome identity is used. }
function NyxStudioCollectionViewAction(AAction: TNyxStudioCollectionAction): Boolean;
{ Structural proposals lock their exact collection or authored view while
  pending, preventing pre-paint removal/recreation or column retargeting. }
function NyxStudioCollectionStructuralAction(AAction: TNyxStudioCollectionAction): Boolean;
{ Apply one copied view proposal to an immutable specification. Exact schema
  families and existing column identities are required; later changes to other
  columns/scope remain intact. Bind starts from the current schema. Inherit is
  session-owned because it needs the reusable recipe's effective projection. }
function ApplyNyxStudioCollectionViewIntent(const AIntent: TNyxStudioCollectionIntent;
  const ASpec: TNyxCollectionViewSpec; const ASchema: TNyxCollectionSchema):
  TNyxCollectionViewSpec;

implementation

uses nyx.collections.query.editor;

function TNyxStudioCollectionFieldRef.GetName: TNyxText;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Collection intent field reference is undefined');
  end;
  Result := FName;
end;

function TNyxStudioCollectionFieldRef.GetKind: TNyxStateKind;
begin
  GetName;
  Result := FKind;
end;

class function TNyxStudioCollectionFieldRef.Text(
  const AField: TNyxTextFieldRef): TNyxStudioCollectionFieldRef;
begin
  Result := Default(TNyxStudioCollectionFieldRef);
  Result.FDefined := True;
  Result.FName := AField.Name;
  Result.FKind := nskText;
end;

class function TNyxStudioCollectionFieldRef.Boolean(
  const AField: TNyxBooleanFieldRef): TNyxStudioCollectionFieldRef;
begin
  Result := Default(TNyxStudioCollectionFieldRef);
  Result.FDefined := True;
  Result.FName := AField.Name;
  Result.FKind := nskBoolean;
end;

class function TNyxStudioCollectionFieldRef.Integer(
  const AField: TNyxIntegerFieldRef): TNyxStudioCollectionFieldRef;
begin
  Result := Default(TNyxStudioCollectionFieldRef);
  Result.FDefined := True;
  Result.FName := AField.Name;
  Result.FKind := nskInteger;
end;

class function TNyxStudioCollectionFieldRef.Number(
  const AField: TNyxNumberFieldRef): TNyxStudioCollectionFieldRef;
begin
  Result := Default(TNyxStudioCollectionFieldRef);
  Result.FDefined := True;
  Result.FName := AField.Name;
  Result.FKind := nskNumber;
end;

class function TNyxStudioCollectionFieldRef.FromMetadata(
  const AField: TNyxCollectionField): TNyxStudioCollectionFieldRef;
begin
  case Ord(AField.Kind) of
    Ord(nskText):
      begin
        Result := Text(NyxTextField(AField.Name));
      end;
    Ord(nskBoolean):
      begin
        Result := Boolean(NyxBooleanField(AField.Name));
      end;
    Ord(nskInteger):
      begin
        Result := Integer(NyxIntegerField(AField.Name));
      end;
    Ord(nskNumber):
      begin
        Result := Number(NyxNumberField(AField.Name));
      end;
  else
    begin
      raise ENyxCollection.Create('Collection intent field has an unknown family');
    end;
  end;
end;

function TNyxStudioCollectionFieldRef.ToData: TNyxDataValue;
begin
  Result := NyxNull;

  if FDefined then
  begin
    Result := NyxObject([NyxField('name', NyxData(Name)),
      NyxField('kind', NyxData(Ord(Kind)))]);
  end;
end;

class function TNyxStudioCollectionFieldRef.FromData(const AData: TNyxDataValue):
  TNyxStudioCollectionFieldRef;
var
  LKind: System.Integer;
  LName: TNyxText;
begin
  Result := Default(TNyxStudioCollectionFieldRef);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if (AData.Kind <> ndObject) or (AData.Count <> 2) then
  begin
    raise ENyxCollection.Create('Collection intent field requires its exact descriptor');
  end;
  LKind := AData.Field('kind').AsInteger;
  LName := AData.Field('name').AsText;
  case LKind of
    Ord(nskText):
      begin
        Result := Text(NyxTextField(LName));
      end;
    Ord(nskBoolean):
      begin
        Result := Boolean(NyxBooleanField(LName));
      end;
    Ord(nskInteger):
      begin
        Result := Integer(NyxIntegerField(LName));
      end;
    Ord(nskNumber):
      begin
        Result := Number(NyxNumberField(LName));
      end;
  else
    begin
      raise ENyxCollection.Create('Collection intent field has an unknown family');
    end;
  end;
end;

function TNyxStudioCollectionFieldRef.SameReference(
  const AOther: TNyxStudioCollectionFieldRef): Boolean;
begin
  Result := (FDefined = AOther.FDefined) and (FName = AOther.FName) and
    (FKind = AOther.FKind);
end;

function NyxStudioCollectionViewAction(AAction: TNyxStudioCollectionAction): Boolean;
begin
  Result := AAction in [scaBind, scaScope, scaTitle, scaMode, scaParent,
    scaRemoveColumn, scaAddColumn, scaClear, scaInherit, scaSelection, scaQuery,
    scaColumnContent, scaColumnFixed, scaColumnFlex, scaColumnMinimum,
    scaColumnMaximum, scaColumnDefault, scaPageSize, scaInitialPage, scaDefaultPaging,
    scaTableQuery, scaColumnQuery, scaDefaultTableQuery, scaDefaultColumnQuery,
    scaTableLayout, scaColumnLayout, scaDefaultTableLayout, scaDefaultColumnLayout,
    scaColumnFormat, scaDefaultColumnFormat, scaTableWorkbench, scaDefaultTableWorkbench,
    scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment, scaInspection, scaDefaultInspection,
    scaDraftRecovery, scaDefaultDraftRecovery];
end;

function NyxStudioCollectionStructuralAction(AAction: TNyxStudioCollectionAction): Boolean;
begin
  Result := AAction in [scaCreate, scaRemove, scaAddField, scaAddRow, scaRemoveRow,
    scaBind, scaRemoveColumn, scaAddColumn, scaClear, scaInherit, scaQuery,
    scaTableQuery, scaColumnQuery, scaDefaultTableQuery, scaDefaultColumnQuery,
    scaTableLayout, scaColumnLayout, scaDefaultTableLayout, scaDefaultColumnLayout,
    scaColumnFormat, scaDefaultColumnFormat, scaTableWorkbench, scaDefaultTableWorkbench,
    scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment, scaInspection, scaDefaultInspection,
    scaDraftRecovery, scaDefaultDraftRecovery];
end;

procedure TNyxStudioCollectionIntent.Validate;
var
  LKey: TNyxText;
begin
  LKey := Key.Name;

  if ((Action in [scaDraftRecovery, scaDefaultDraftRecovery]) <> (DraftRecoveryBaseline <> '')) or
    ((Action = scaDraftRecovery) <> DraftRecovery.Defined) then
  begin
    raise ENyxCollection.Create('Recovery intent requires its own exact baseline and policy');
  end;

  if Action = scaDraftRecovery then
  begin
    DraftRecovery.Validate;
  end;

  if ((Action in [scaInspection, scaDefaultInspection]) <> (InspectionBaseline <> '')) or
    ((Action = scaInspection) <> Inspection.Defined) then
  begin
    raise ENyxCollection.Create('Inspection intent requires its own exact baseline and policy');
  end;

  if Action = scaInspection then
  begin
    Inspection.Validate;

    if (Projection = cpTree) and (Inspection.DefaultFollow = ncfLastVisible) then
    begin
      raise ENyxCollection.Create('Tree inspection supports keep-position, not follow-last');
    end;
  end;

  if ((Action in [scaQuery, scaTableQuery, scaColumnQuery,
      scaDefaultTableQuery, scaDefaultColumnQuery]) and (QueryBaseline = '')) or
    (not (Action in [scaQuery, scaTableQuery, scaColumnQuery,
      scaDefaultTableQuery, scaDefaultColumnQuery]) and (QueryBaseline <> '')) or
    ((Action <> scaQuery) and Query.Defined) or
    ((Action = scaTableQuery) <> TableQuery.Defined) or
    ((Action = scaColumnQuery) <> ColumnQuery.Defined) then
  begin
    raise ENyxCollection.Create('Query intent requires its own exact baseline and policy');
  end;

  if ((Action in [scaTableLayout, scaColumnLayout,
      scaDefaultTableLayout, scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat,
      scaTableWorkbench, scaDefaultTableWorkbench,
      scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
      scaColumnAlignment, scaDefaultColumnAlignment]) and (LayoutBaseline = '')) or
    (not (Action in [scaTableLayout, scaColumnLayout,
      scaDefaultTableLayout, scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat,
      scaTableWorkbench, scaDefaultTableWorkbench,
      scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
      scaColumnAlignment, scaDefaultColumnAlignment]) and (LayoutBaseline <> '')) or
    ((Action = scaTableLayout) <> TableLayout.Defined) or
    ((Action = scaColumnLayout) <> ColumnLayout.Defined) or
    ((Action = scaColumnFormat) <> ColumnFormat.Defined) or
    ((Action = scaTableWorkbench) <> TableWorkbench.Defined) or
    ((Action = scaTablePresentation) <> TablePresentation.Defined) or
    ((Action = scaColumnText) <> ColumnText.Defined) or
    ((Action = scaColumnAlignment) <> ColumnAlignment.Defined) then
  begin
    raise ENyxCollection.Create('Layout intent requires its own exact baseline and policy');
  end;

  if Action = scaQuery then
  begin
    Query.ToData;
  end;

  if (Ord(Action) < Ord(Low(TNyxStudioCollectionAction))) or
    (Ord(Action) > Ord(High(TNyxStudioCollectionAction))) or
    (Ord(Input) < Ord(Low(TNyxStudioStateInput))) or
    (Ord(Input) > Ord(High(TNyxStudioStateInput))) or
    (Ord(Kind) < Ord(Low(TNyxStateKind))) or (Ord(Kind) > Ord(High(TNyxStateKind))) or
    (Ord(Mode) < Ord(Low(TNyxCollectionCellMode))) or
    (Ord(Mode) > Ord(High(TNyxCollectionCellMode))) or
    (Ord(Scope) < Ord(Low(TNyxCollectionScope))) or
    (Ord(Scope) > Ord(High(TNyxCollectionScope))) or
    (Ord(SelectionMode) < Ord(Low(TNyxSelectionMode))) or
    (Ord(SelectionMode) > Ord(High(TNyxSelectionMode))) or
    (Ord(Projection) < Ord(Low(TNyxCollectionProjection))) or
    (Ord(Projection) > Ord(High(TNyxCollectionProjection))) then
  begin
    raise ENyxCollection.Create('Collection intent has an unknown closed choice');
  end;

  if Item.Defined <> (Action in [scaAddRow, scaRemoveRow, scaCell]) then
  begin
    raise ENyxCollection.Create('Only row/cell intent requires an exact item reference');
  end;

  if Item.Defined and (Item.Collection.Name <> LKey) then
  begin
    raise ENyxCollection.Create('Collection intent item belongs to another collection');
  end;

  if (Action in [scaDefault, scaCell, scaTitle, scaMode, scaRemoveColumn,
    scaAddColumn, scaColumnContent, scaColumnFixed, scaColumnFlex,
    scaColumnMinimum, scaColumnMaximum, scaColumnDefault,
    scaColumnQuery, scaDefaultColumnQuery, scaColumnLayout,
    scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat,
    scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment]) and not Field.Defined then
  begin
    raise ENyxCollection.Create('Collection intent requires its exact typed field');
  end;

  if Field.Defined and not (Action in [scaDefault, scaCell, scaTitle, scaMode,
    scaParent, scaRemoveColumn, scaAddColumn, scaColumnContent, scaColumnFixed,
    scaColumnFlex, scaColumnMinimum, scaColumnMaximum, scaColumnDefault,
    scaColumnQuery, scaDefaultColumnQuery, scaColumnLayout,
    scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat,
    scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment]) then
  begin
    raise ENyxCollection.Create('This collection intent does not use a field');
  end;

  if (Action = scaParent) and Field.Defined and (Field.Kind <> nskText) then
  begin
    raise ENyxCollection.Create('A parent reference requires a text field');
  end;

  if (Action in [scaColumnContent, scaColumnFixed, scaColumnFlex,
    scaColumnMinimum, scaColumnMaximum, scaColumnDefault,
    scaTableQuery, scaColumnQuery, scaDefaultTableQuery, scaDefaultColumnQuery,
    scaTableLayout, scaColumnLayout, scaDefaultTableLayout, scaDefaultColumnLayout,
    scaColumnFormat, scaDefaultColumnFormat, scaTableWorkbench, scaDefaultTableWorkbench,
    scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment]) and
    (Projection <> cpTable) then
  begin
    raise ENyxCollection.Create('Column sizing/query/layout controls require the table projection');
  end;

  if Action = scaTableQuery then
  begin
    TableQuery.Validate;
  end;

  if Action = scaColumnQuery then
  begin
    ColumnQuery.Validate(Field.Kind);
  end;

  if Action = scaTableLayout then
  begin
    TableLayout.Validate;
  end;

  if Action = scaColumnLayout then
  begin
    ColumnLayout.Validate;
  end;

  if Action = scaColumnFormat then
  begin
    ColumnFormat.ValidateKind(Field.Kind);
  end;

  if Action = scaTableWorkbench then
  begin
    TableWorkbench.Validate;
  end;

  if Action = scaTablePresentation then
  begin
    TablePresentation.Validate;
  end;

  if Action = scaColumnText then
  begin
    ColumnText.Validate;
  end;

  if Action = scaColumnAlignment then
  begin
    ColumnAlignment.Validate;
  end;

  if (Action in [scaPageSize, scaInitialPage, scaDefaultPaging]) and (Projection = cpTree) then
  begin
    raise ENyxCollection.Create('Paging requires a list or table projection');
  end;

  if (Action in [scaDefault, scaCell]) and
    (NyxStudioStateInputKind(Input) <> Field.Kind) then
  begin
    raise ENyxCollection.Create('Collection intent notation does not match its field family');
  end;

  if ((Action <> scaAddField) and (Kind <> nskText)) or
    ((Action <> scaMode) and (Mode <> cmReadOnly)) or
    ((Action <> scaScope) and (Scope <> csApplication)) or
    ((Action <> scaSelection) and (SelectionMode <> nsmSingle)) or
    (not (Action in [scaDefault, scaCell]) and (Input <> ssiText)) or
    (not (Action in [scaDefault, scaCell, scaTitle, scaColumnFixed, scaColumnFlex,
      scaColumnMinimum, scaColumnMaximum, scaPageSize, scaInitialPage]) and (Value <> '')) or
    (not NyxStudioCollectionViewAction(Action) and (Projection <> cpList)) then
  begin
    raise ENyxCollection.Create('Collection intent contains unrelated payload');
  end;
end;

function TNyxStudioCollectionIntent.SameIntent(
  const AOther: TNyxStudioCollectionIntent): Boolean;
begin
  Validate;
  AOther.Validate;
  Result := (Action = AOther.Action) and (Key.Name = AOther.Key.Name) and
    (Item.Defined = AOther.Item.Defined) and Field.SameReference(AOther.Field) and
    (Input = AOther.Input) and (Value = AOther.Value) and (Kind = AOther.Kind) and
    (Mode = AOther.Mode) and (Scope = AOther.Scope) and
    (SelectionMode = AOther.SelectionMode) and (Projection = AOther.Projection) and
    (QueryBaseline = AOther.QueryBaseline) and
    (Query.ToData.ToJSON = AOther.Query.ToData.ToJSON) and
    (TableQuery.ToData.ToJSON = AOther.TableQuery.ToData.ToJSON) and
    (ColumnQuery.ToData.ToJSON = AOther.ColumnQuery.ToData.ToJSON) and
    (LayoutBaseline = AOther.LayoutBaseline) and
    (TableLayout.ToData.ToJSON = AOther.TableLayout.ToData.ToJSON) and
    (ColumnLayout.ToData.ToJSON = AOther.ColumnLayout.ToData.ToJSON) and
    (ColumnFormat.ToData.ToJSON = AOther.ColumnFormat.ToData.ToJSON) and
    (TableWorkbench.ToData.ToJSON = AOther.TableWorkbench.ToData.ToJSON) and
    (TablePresentation.ToData.ToJSON = AOther.TablePresentation.ToData.ToJSON) and
    (ColumnText.ToData.ToJSON = AOther.ColumnText.ToData.ToJSON) and
    (ColumnAlignment.ToData.ToJSON = AOther.ColumnAlignment.ToData.ToJSON) and
    (InspectionBaseline = AOther.InspectionBaseline) and
    (Inspection.ToData.ToJSON = AOther.Inspection.ToData.ToJSON) and
    (DraftRecoveryBaseline = AOther.DraftRecoveryBaseline) and
    (DraftRecovery.ToData.ToJSON = AOther.DraftRecovery.ToData.ToJSON);

  if Result and Item.Defined then
  begin
    Result := (Item.Collection.Name = AOther.Item.Collection.Name) and
      (Item.ID = AOther.Item.ID);
  end;
end;

function TNyxStudioCollectionIntent.ToData: TNyxDataValue;
var
  LItem: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  Validate;
  LItem := NyxNull;

  if Item.Defined then
  begin
    LItem := NyxObject([NyxField('collection', NyxData(Item.Collection.Name)),
      NyxField('id', NyxData(Item.ID))]);
  end;
  Result := NyxObject([NyxField('action', NyxData(Ord(Action))),
    NyxField('key', NyxData(Key.Name)), NyxField('item', LItem),
    NyxField('field', Field.ToData), NyxField('input', NyxData(Ord(Input))),
    NyxField('value', NyxData(Value)), NyxField('kind', NyxData(Ord(Kind))),
    NyxField('mode', NyxData(Ord(Mode))), NyxField('scope', NyxData(Ord(Scope))),
    NyxField('selection', NyxData(Ord(SelectionMode))),
    NyxField('projection', NyxData(Ord(Projection)))]);

  if Action in [scaDraftRecovery, scaDefaultDraftRecovery] then
  begin
    SetLength(LFields, 12);
    for LIndex := 0 to 10 do
    begin
      LFields[LIndex] := NyxField(Result.Key(LIndex), Result.Field(Result.Key(LIndex)));
    end;
    LFields[11] := NyxField('baseline', NyxData(DraftRecoveryBaseline));

    if Action = scaDraftRecovery then
    begin
      SetLength(LFields, 13);
      LFields[12] := NyxField('draftRecovery', DraftRecovery.ToData);
    end;
    Result := NyxObject(LFields);
  end;

  if Action in [scaInspection, scaDefaultInspection] then
  begin
    SetLength(LFields, 12);
    for LIndex := 0 to 10 do
    begin
      LFields[LIndex] := NyxField(Result.Key(LIndex), Result.Field(Result.Key(LIndex)));
    end;
    LFields[11] := NyxField('baseline', NyxData(InspectionBaseline));

    if Action = scaInspection then
    begin
      SetLength(LFields, 13);
      LFields[12] := NyxField('inspection', Inspection.ToData);
    end;
    Result := NyxObject(LFields);
  end;

  if Action in [scaQuery, scaTableQuery, scaColumnQuery,
    scaDefaultTableQuery, scaDefaultColumnQuery] then
  begin
    { Former actions retain their exact eleven-member encoding, and scaQuery
      retains its former thirteen-member encoding and names. Default-policy
      actions add only a baseline; setters add their own closed policy object. }
    SetLength(LFields, 12);
    for LIndex := 0 to 10 do
    begin
      LFields[LIndex] := NyxField(Result.Key(LIndex), Result.Field(Result.Key(LIndex)));
    end;
    LFields[11] := NyxField('baseline', NyxData(QueryBaseline));

    if Action in [scaQuery, scaTableQuery, scaColumnQuery] then
    begin
      SetLength(LFields, 13);
      LFields[12] := LFields[11];
      case Action of
        scaQuery: LFields[11] := NyxField('query', Query.ToData);
        scaTableQuery: LFields[11] := NyxField('tableQuery', TableQuery.ToData);
        scaColumnQuery: LFields[11] := NyxField('columnQuery', ColumnQuery.ToData);
      else
        begin
          raise ENyxCollection.Create('This intent has no query policy payload');
        end;
      end;
    end;
    Result := NyxObject(LFields);
  end;

  if Action in [scaTableLayout, scaColumnLayout,
    scaDefaultTableLayout, scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat,
    scaTableWorkbench, scaDefaultTableWorkbench,
    scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment] then
  begin
    SetLength(LFields, 12);
    for LIndex := 0 to 10 do
    begin
      LFields[LIndex] := NyxField(Result.Key(LIndex), Result.Field(Result.Key(LIndex)));
    end;
    LFields[11] := NyxField('baseline', NyxData(LayoutBaseline));

    if Action in [scaTableLayout, scaColumnLayout, scaColumnFormat, scaTableWorkbench,
      scaTablePresentation, scaColumnText, scaColumnAlignment] then
    begin
      SetLength(LFields, 13);
      LFields[12] := LFields[11];

      if Action = scaTableLayout then
      begin
        LFields[11] := NyxField('tableLayout', TableLayout.ToData);
      end
      else if Action = scaColumnLayout then
      begin
        LFields[11] := NyxField('columnLayout', ColumnLayout.ToData);
      end
      else if Action = scaColumnFormat then
      begin
        LFields[11] := NyxField('columnFormat', ColumnFormat.ToData);
      end
      else if Action = scaTablePresentation then
      begin
        LFields[11] := NyxField('tablePresentation', TablePresentation.ToData);
      end
      else if Action = scaColumnText then
      begin
        LFields[11] := NyxField('columnText', ColumnText.ToData);
      end
      else if Action = scaColumnAlignment then
      begin
        LFields[11] := NyxField('columnAlignment', ColumnAlignment.ToData);
      end
      else
      begin
        LFields[11] := NyxField('tableWorkbench', TableWorkbench.ToData);
      end;
    end;
    Result := NyxObject(LFields);
  end;
end;

function AddTypedColumn(const ASpec: TNyxCollectionViewSpec;
  const AField: TNyxStudioCollectionFieldRef; const ATitle: TNyxText;
  AMode: TNyxCollectionCellMode): TNyxCollectionViewSpec;
begin
  case Ord(AField.Kind) of
    Ord(nskText):
      begin
        Result := ASpec.Column(NyxTextField(AField.Name), ATitle, AMode);
      end;
    Ord(nskBoolean):
      begin
        Result := ASpec.Column(NyxBooleanField(AField.Name), ATitle, AMode);
      end;
    Ord(nskInteger):
      begin
        Result := ASpec.Column(NyxIntegerField(AField.Name), ATitle, AMode);
      end;
    Ord(nskNumber):
      begin
        Result := ASpec.Column(NyxNumberField(AField.Name), ATitle, AMode);
      end;
  else
    begin
      raise ENyxCollection.Create('Collection view column has an unknown family');
    end;
  end;
end;

function ApplyNyxStudioCollectionViewIntent(const AIntent: TNyxStudioCollectionIntent;
  const ASpec: TNyxCollectionViewSpec; const ASchema: TNyxCollectionSchema):
  TNyxCollectionViewSpec;
var
  LIndex: Integer;
  LFieldIndex: Integer;
  LField: TNyxCollectionField;
  LReference: TNyxStudioCollectionFieldRef;
  LColumn: TNyxCollectionColumn;
  LTitle: TNyxText;
  LMode: TNyxCollectionCellMode;
  LFound: Boolean;
  LColumnFound: Boolean;
  LSizing: TNyxColumnSizing;
  LSizeValue: Integer;
  LPaging: TNyxCollectionPaging;
begin
  AIntent.Validate;
  ASchema.Validate;

  if not NyxStudioCollectionViewAction(AIntent.Action) or
    (AIntent.Action = scaInherit) then
  begin
    raise ENyxCollection.Create('This operation requires its collection/session command');
  end;

  if AIntent.Action = scaBind then
  begin
    Result := NyxCollectionView(AIntent.Key);
    for LIndex := 0 to ASchema.Count - 1 do
    begin
      LField := ASchema.FieldAt(LIndex);
      Result := AddTypedColumn(Result, TNyxStudioCollectionFieldRef.FromMetadata(LField),
        LField.Name, cmReadOnly);
    end;
    Exit;
  end;

  if not ASpec.Defined or (ASpec.Key.Name <> AIntent.Key.Name) then
  begin
    raise ENyxCollection.Create('Collection binding changed; use its current inspector');
  end;

  if AIntent.Action in [scaQuery, scaTableQuery, scaColumnQuery,
    scaDefaultTableQuery, scaDefaultColumnQuery] then
  begin

    if AIntent.QueryBaseline <> NyxQueryEditorBaseline(ASchema, ASpec) then
    begin
      raise ENyxCollection.Create('Query binding/schema changed; use its current inspector');
    end;

    if AIntent.Action = scaQuery then
    begin
      AIntent.Query.Validate(ASchema);
      Exit(ASpec.Query(AIntent.Query));
    end;

    if AIntent.Action = scaTableQuery then
    begin
      Exit(ASpec.TableQuery(AIntent.TableQuery));
    end;

    if AIntent.Action = scaDefaultTableQuery then
    begin
      Exit(ASpec.UseDefaultTableQuery);
    end;
    { Column policies need the existing family-qualified column loop below.
      Never append a column merely to apply a captured policy to its name. }
  end;

  if AIntent.Action in [scaTableLayout, scaColumnLayout,
    scaDefaultTableLayout, scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat,
    scaTableWorkbench, scaDefaultTableWorkbench,
    scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment] then
  begin
    { This existing proof covers the complete binding and schema families; it
      performs no runtime query or preference operation. }

    if AIntent.LayoutBaseline <> NyxQueryEditorBaseline(ASchema, ASpec) then
    begin
      raise ENyxCollection.Create('Layout binding/schema changed; use its current inspector');
    end;

    if AIntent.Action = scaTableLayout then
    begin
      Exit(ASpec.TableLayout(AIntent.TableLayout));
    end;

    if AIntent.Action = scaDefaultTableLayout then
    begin
      Exit(ASpec.UseDefaultTableLayout);
    end;

    if AIntent.Action = scaTableWorkbench then
    begin
      Exit(ASpec.TableWorkbench(AIntent.TableWorkbench));
    end;

    if AIntent.Action = scaDefaultTableWorkbench then
    begin
      Exit(ASpec.UseDefaultTableWorkbench);
    end;

    if AIntent.Action = scaTablePresentation then
    begin
      Exit(ASpec.TablePresentation(AIntent.TablePresentation));
    end;

    if AIntent.Action = scaDefaultTablePresentation then
    begin
      Exit(ASpec.UseDefaultTablePresentation);
    end;
  end;

  if AIntent.Action in [scaDraftRecovery, scaDefaultDraftRecovery] then
  begin

    if AIntent.DraftRecoveryBaseline <> NyxQueryEditorBaseline(ASchema, ASpec) then
    begin
      raise ENyxCollection.Create('Recovery binding/schema changed; reopen its inspector');
    end;

    if AIntent.Action = scaDraftRecovery then
    begin
      Exit(ASpec.DraftRecovery(AIntent.DraftRecovery));
    end;
    Exit(ASpec.UseDefaultDraftRecovery);
  end;

  if AIntent.Action in [scaInspection, scaDefaultInspection] then
  begin

    if AIntent.InspectionBaseline <> NyxQueryEditorBaseline(ASchema, ASpec) then
    begin
      raise ENyxCollection.Create('Inspection binding/schema changed; reopen its inspector');
    end;

    if AIntent.Action = scaInspection then
    begin
      Exit(ASpec.Inspection(AIntent.Inspection));
    end;
    Exit(ASpec.UseDefaultInspection);
  end;

  if AIntent.Action = scaClear then
  begin
    Exit(Default(TNyxCollectionViewSpec));
  end;

  if AIntent.Action = scaDefaultPaging then
  begin
    Exit(ASpec.UseDefaultPaging);
  end;

  if AIntent.Action in [scaPageSize, scaInitialPage] then
  begin

    if not TryNyxStateInteger(AIntent.Value, LSizeValue) then
    begin
      raise ENyxCollection.Create('Paging requires a positive Integer');
    end;
    LPaging := ASpec.PagingPolicy;

    if not LPaging.Defined then
    begin
      LPaging := NyxCollectionPaging;
    end;

    if AIntent.Action = scaPageSize then
    begin
      LPaging := LPaging.PageSize(LSizeValue);
    end
    else
    begin
      LPaging := LPaging.InitialPage(LSizeValue);
    end;
    Exit(ASpec.Paged(LPaging));
  end;

  if AIntent.Field.Defined then
  begin
    LFound := False;
    for LIndex := 0 to ASchema.Count - 1 do
    begin
      LField := ASchema.FieldAt(LIndex);
      LFound := LFound or ((LField.Name = AIntent.Field.Name) and
        (LField.Kind = AIntent.Field.Kind));
    end;

    if not LFound then
    begin
      raise ENyxCollection.Create('Collection field disappeared or changed its family');
    end;
  end;

  if (AIntent.Action = scaParent) and (AIntent.Projection <> cpTree) then
  begin
    raise ENyxCollection.Create('Parent-field authoring requires the captured tree');
  end;
  Result := NyxCollectionView(ASpec.Key).Scoped(ASpec.Scope)
    .Selection(ASpec.SelectionMode).Query(ASpec.QueryPolicy);
  { Column/parent/scope edits reconstruct the ordered binding. Retain an exact
    saved search choice as well; an unrelated Inspector action must not silently
    restore the library default or lose an explicitly disabled policy. }

  if ASpec.HasTypeAhead then
  begin
    Result := Result.TypeAhead(ASpec.TypeAheadPolicy);
  end;

  if ASpec.PagingPolicy.Defined then
  begin
    Result := Result.Paged(ASpec.PagingPolicy);
  end;

  if ASpec.InspectionPolicy.Defined then
  begin
    Result := Result.Inspection(ASpec.InspectionPolicy);
  end;

  if ASpec.DraftRecoveryPolicy.Defined then
  begin
    Result := Result.DraftRecovery(ASpec.DraftRecoveryPolicy);
  end;

  if ASpec.HasTableQueryPolicy then
  begin
    { Copy raw presence, not the resolved default: absence must remain absent
      through unrelated edits and continue to encode as the older view version. }
    Result := Result.TableQuery(ASpec.TableQueryPolicy);
  end;

  if ASpec.HasTableLayoutPolicy then
  begin
    { Layout defaults are authored values too. An unrelated schema/title edit
      must preserve their explicit presence instead of resolving then saving a
      library default which the author never chose. }
    Result := Result.TableLayout(ASpec.TableLayoutPolicy);
  end;

  if ASpec.HasTableWorkbenchPolicy then
  begin
    { Rebuilding column/title/scope intent preserves raw presence and all
      independent action choices; only an explicit policy reset clears them. }
    Result := Result.TableWorkbench(ASpec.TableWorkbenchPolicy);
  end;

  if ASpec.HasTablePresentationPolicy then
  begin
    Result := Result.TablePresentation(ASpec.TablePresentationPolicy);
  end;

  if (AIntent.Action <> scaParent) and (ASpec.ParentField <> '') then
  begin
    Result := Result.Parent(NyxTextField(ASpec.ParentField));
  end;
  LColumnFound := False;
  for LIndex := 0 to ASpec.Count - 1 do
  begin
    LColumn := ASpec.ColumnAt(LIndex);
    LTitle := LColumn.Title;
    LMode := LColumn.Mode;
    LSizing := LColumn.Sizing;
    { Schema and view order can differ. Resolve this column's exact name/family
      without borrowing a positional schema field. }
    LFound := False;
    LReference := Default(TNyxStudioCollectionFieldRef);
    for LFieldIndex := 0 to ASchema.Count - 1 do
    begin
      LField := ASchema.FieldAt(LFieldIndex);

      if (LField.Name = LColumn.FieldName) and (LField.Kind = LColumn.Kind) then
      begin
        LReference := TNyxStudioCollectionFieldRef.FromMetadata(LField);
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxCollection.Create('Existing view column no longer matches its schema');
    end;

    if AIntent.Field.Defined and (LColumn.FieldName = AIntent.Field.Name) then
    begin
      LColumnFound := True;

      if LColumn.Kind <> AIntent.Field.Kind then
      begin
        raise ENyxCollection.Create('Collection column changed its scalar family');
      end;

      if AIntent.Action = scaRemoveColumn then
      begin
        Continue;
      end;

      if AIntent.Action = scaTitle then
      begin
        LTitle := AIntent.Value;
      end
      else if AIntent.Action = scaMode then
      begin
        LMode := AIntent.Mode;
      end;

      if AIntent.Action in [scaColumnContent, scaColumnFixed, scaColumnFlex,
        scaColumnMinimum, scaColumnMaximum, scaColumnDefault] then
      begin

        if AIntent.Projection <> cpTable then
        begin
          raise ENyxCollection.Create('Column sizing requires this captured table');
        end;

        if not LSizing.Defined then
        begin
          LSizing := NyxColumnSizing;
        end;

        if AIntent.Action in [scaColumnFixed, scaColumnFlex, scaColumnMinimum, scaColumnMaximum] then
        begin

          if not TryNyxStateInteger(AIntent.Value, LSizeValue) then
          begin
            raise ENyxCollection.Create('Column sizing requires an exact Integer');
          end;
        end;
        case AIntent.Action of
          scaColumnContent:
            begin
              LSizing := LSizing.Content;
            end;
          scaColumnFixed:
            begin
              LSizing := LSizing.Fixed(LSizeValue);
            end;
          scaColumnFlex:
            begin
              LSizing := LSizing.Flex(LSizeValue);
            end;
          scaColumnMinimum:
            begin
              LSizing := LSizing.Minimum(LSizeValue);
            end;
          scaColumnMaximum:
            begin
              LSizing := LSizing.Maximum(LSizeValue);
            end;
          scaColumnDefault:
            begin
              LSizing := Default(TNyxColumnSizing);
            end;
          else
            begin
              raise ENyxCollection.Create('Unknown column sizing action');
            end;
        end;
      end;
    end;
    Result := AddTypedColumn(Result, LReference, LTitle, LMode);

    if AIntent.Field.Defined and (LColumn.FieldName = AIntent.Field.Name) and
      (AIntent.Action = scaColumnQuery) then
    begin
      Result := Result.ColumnQuery(AIntent.ColumnQuery);
    end
    else if LColumn.HasQueryPolicy and not
      (AIntent.Field.Defined and (LColumn.FieldName = AIntent.Field.Name) and
        (AIntent.Action = scaDefaultColumnQuery)) then
    begin
      { The selected policy's reset affects only that raw declaration. Every
        surviving peer retains its independently declared operators and mode. }
      Result := Result.ColumnQuery(LColumn.QueryPolicy);
    end;

    if LSizing.Defined then
    begin
      { An unrelated title/edit-mode/scope action must retain the exact authored
        width policy. Rebuilding the ordered descriptor cannot reset styling. }
      Result := Result.ColumnSizing(LSizing);
    end;

    if AIntent.Field.Defined and (LColumn.FieldName = AIntent.Field.Name) and
      (AIntent.Action = scaColumnLayout) then
    begin
      Result := Result.ColumnLayout(AIntent.ColumnLayout);
    end
    else if LColumn.HasLayoutPolicy and not
      (AIntent.Field.Defined and (LColumn.FieldName = AIntent.Field.Name) and
        (AIntent.Action = scaDefaultColumnLayout)) then
    begin
      Result := Result.ColumnLayout(LColumn.LayoutPolicy);
    end;

    if AIntent.Field.Defined and (LColumn.FieldName = AIntent.Field.Name) and
      (AIntent.Action = scaColumnText) then
    begin
      Result := Result.ColumnText(AIntent.ColumnText);
    end
    else if LColumn.HasTextPolicy and not
      (AIntent.Field.Defined and (LColumn.FieldName = AIntent.Field.Name) and
        (AIntent.Action = scaDefaultColumnText)) then
    begin
      Result := Result.ColumnText(LColumn.TextPolicy);
    end;

    if AIntent.Field.Defined and (LColumn.FieldName = AIntent.Field.Name) and
      (AIntent.Action = scaColumnAlignment) then
    begin
      Result := Result.ColumnAlignment(AIntent.ColumnAlignment);
    end
    else if LColumn.HasAlignmentPolicy and not
      (AIntent.Field.Defined and (LColumn.FieldName = AIntent.Field.Name) and
        (AIntent.Action = scaDefaultColumnAlignment)) then
    begin
      Result := Result.ColumnAlignment(LColumn.AlignmentPolicy);
    end;

    if AIntent.Field.Defined and (LColumn.FieldName = AIntent.Field.Name) and
      (AIntent.Action = scaColumnFormat) then
    begin
      AIntent.ColumnFormat.ValidateDomain(LColumn.Kind, LField.Domain);
      Result := Result.ColumnFormat(AIntent.ColumnFormat);
    end
    else if LColumn.HasFormatPolicy and not
      (AIntent.Field.Defined and (LColumn.FieldName = AIntent.Field.Name) and
        (AIntent.Action = scaDefaultColumnFormat)) then
    begin
      Result := Result.ColumnFormat(LColumn.FormatPolicy);
    end;
  end;

  if (AIntent.Action in [scaTitle, scaMode, scaRemoveColumn, scaColumnContent,
    scaColumnFixed, scaColumnFlex, scaColumnMinimum, scaColumnMaximum,
    scaColumnDefault, scaColumnQuery, scaDefaultColumnQuery,
    scaColumnLayout, scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat,
    scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment]) and not LColumnFound then
  begin
    raise ENyxCollection.Create('Collection column no longer exists');
  end;
  case AIntent.Action of
    scaScope:
      begin
        Result := Result.Scoped(AIntent.Scope);
      end;
    scaSelection:
      begin
        Result := Result.Selection(AIntent.SelectionMode);
      end;
    scaParent:
      begin

        if AIntent.Field.Defined then
        begin
          Result := Result.Parent(NyxTextField(AIntent.Field.Name));
        end;
      end;
    scaAddColumn:
      begin
        Result := AddTypedColumn(Result, AIntent.Field, AIntent.Field.Name, cmReadOnly);
      end;
  else
    begin
      { Other admitted operations already modified the copied column loop. }
    end;
  end;

  if Result.Count = 0 then
  begin
    Result := Default(TNyxCollectionViewSpec);
  end;
end;

class function TNyxStudioCollectionIntent.FromData(const AData: TNyxDataValue):
  TNyxStudioCollectionIntent;
var
  LItem: TNyxDataValue;

  function Choice(const AName: TNyxText; ALow, AHigh: System.Integer): System.Integer;
  begin
    Result := AData.Field(AName).AsInteger;

    if (Result < ALow) or (Result > AHigh) then
    begin
      raise ENyxCollection.Create('Collection intent has an unknown closed choice / ' + AName);
    end;
  end;

begin
  Result := Default(TNyxStudioCollectionIntent);

  if (AData.Kind <> ndObject) or not (AData.Count in [11, 12, 13]) then
  begin
    raise ENyxCollection.Create('Collection intent requires its exact descriptor');
  end;
  Result.Action := TNyxStudioCollectionAction(Choice('action',
    Ord(Low(TNyxStudioCollectionAction)), Ord(High(TNyxStudioCollectionAction))));

  if ((Result.Action in [scaQuery, scaTableQuery, scaColumnQuery,
      scaTableLayout, scaColumnLayout, scaColumnFormat, scaTableWorkbench,
      scaTablePresentation, scaColumnText, scaColumnAlignment, scaInspection, scaDraftRecovery]) and
      (AData.Count <> 13)) or
    ((Result.Action in [scaDefaultTableQuery, scaDefaultColumnQuery,
      scaDefaultTableLayout, scaDefaultColumnLayout, scaDefaultColumnFormat,
      scaDefaultTableWorkbench, scaDefaultTablePresentation, scaDefaultColumnText,
      scaDefaultColumnAlignment, scaDefaultInspection, scaDefaultDraftRecovery]) and
      (AData.Count <> 12)) or
    (not (Result.Action in [scaQuery, scaTableQuery, scaColumnQuery,
      scaDefaultTableQuery, scaDefaultColumnQuery, scaTableLayout,
      scaColumnLayout, scaDefaultTableLayout, scaDefaultColumnLayout, scaColumnFormat,
      scaDefaultColumnFormat, scaTableWorkbench, scaDefaultTableWorkbench,
      scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
      scaColumnAlignment, scaDefaultColumnAlignment, scaInspection, scaDefaultInspection,
      scaDraftRecovery, scaDefaultDraftRecovery]) and
      (AData.Count <> 11)) then
  begin
    raise ENyxCollection.Create('Policy intent requires its exact extended descriptor');
  end;

  if Result.Action = scaQuery then
  begin
    Result.Query := TNyxCollectionQuery.FromData(AData.Field('query'));
  end;

  if Result.Action in [scaDraftRecovery, scaDefaultDraftRecovery] then
  begin
    Result.DraftRecoveryBaseline := AData.Field('baseline').AsText;

    if Result.Action = scaDraftRecovery then
    begin
      Result.DraftRecovery := TNyxCollectionDraftRecoveryPolicy.FromData(AData.Field('draftRecovery'));
    end;
  end;

  if Result.Action in [scaInspection, scaDefaultInspection] then
  begin
    Result.InspectionBaseline := AData.Field('baseline').AsText;

    if Result.Action = scaInspection then
    begin
      Result.Inspection := TNyxCollectionInspectionPolicy.FromData(AData.Field('inspection'));
    end;
  end;

  if Result.Action = scaTableQuery then
  begin
    Result.TableQuery := TNyxTableQueryPolicy.FromData(AData.Field('tableQuery'));
  end;

  if Result.Action = scaColumnQuery then
  begin
    Result.ColumnQuery := TNyxColumnQueryPolicy.FromData(AData.Field('columnQuery'));
  end;

  if Result.Action in [scaQuery, scaTableQuery, scaColumnQuery,
    scaDefaultTableQuery, scaDefaultColumnQuery] then
  begin
    Result.QueryBaseline := AData.Field('baseline').AsText;
  end;

  if Result.Action = scaTableLayout then
  begin
    Result.TableLayout := TNyxTableLayoutPolicy.FromData(AData.Field('tableLayout'));
  end;

  if Result.Action = scaColumnLayout then
  begin
    Result.ColumnLayout := TNyxColumnLayoutPolicy.FromData(AData.Field('columnLayout'));
  end;

  if Result.Action = scaColumnFormat then
  begin
    Result.ColumnFormat := TNyxValueFormat.FromData(AData.Field('columnFormat'));
  end;

  if Result.Action = scaTableWorkbench then
  begin
    Result.TableWorkbench := TNyxTableWorkbenchPolicy.FromData(AData.Field('tableWorkbench'));
  end;

  if Result.Action = scaTablePresentation then
  begin
    Result.TablePresentation := TNyxTablePresentationPolicy.FromData(AData.Field('tablePresentation'));
  end;

  if Result.Action = scaColumnText then
  begin
    Result.ColumnText := TNyxColumnTextPolicy.FromData(AData.Field('columnText'));
  end;

  if Result.Action = scaColumnAlignment then
  begin
    Result.ColumnAlignment := TNyxColumnAlignmentPolicy.FromData(AData.Field('columnAlignment'));
  end;

  if Result.Action in [scaTableLayout, scaColumnLayout,
    scaDefaultTableLayout, scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat,
    scaTableWorkbench, scaDefaultTableWorkbench,
    scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment] then
  begin
    Result.LayoutBaseline := AData.Field('baseline').AsText;
  end;
  Result.Key := NyxCollection(AData.Field('key').AsText);
  LItem := AData.Field('item');

  if LItem.Kind <> ndNull then
  begin

    if (LItem.Kind <> ndObject) or (LItem.Count <> 2) then
    begin
      raise ENyxCollection.Create('Collection intent item requires its exact descriptor');
    end;
    Result.Item := NyxItem(NyxCollection(LItem.Field('collection').AsText),
      LItem.Field('id').AsText);
  end;
  Result.Field := TNyxStudioCollectionFieldRef.FromData(AData.Field('field'));
  Result.Input := TNyxStudioStateInput(Choice('input',
    Ord(Low(TNyxStudioStateInput)), Ord(High(TNyxStudioStateInput))));
  Result.Value := AData.Field('value').AsText;
  Result.Kind := TNyxStateKind(Choice('kind', Ord(Low(TNyxStateKind)),
    Ord(High(TNyxStateKind))));
  Result.Mode := TNyxCollectionCellMode(Choice('mode', Ord(Low(TNyxCollectionCellMode)),
    Ord(High(TNyxCollectionCellMode))));
  Result.Scope := TNyxCollectionScope(Choice('scope', Ord(Low(TNyxCollectionScope)),
    Ord(High(TNyxCollectionScope))));
  Result.SelectionMode := TNyxSelectionMode(Choice('selection',
    Ord(Low(TNyxSelectionMode)), Ord(High(TNyxSelectionMode))));
  Result.Projection := TNyxCollectionProjection(Choice('projection',
    Ord(Low(TNyxCollectionProjection)), Ord(High(TNyxCollectionProjection))));
  Result.Validate;
end;

end.
