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
  nyx.studio.authoring;

type
  { Closed ordinary editor operations. These describe intent, not a saved
    collection replacement or an executable closure. Append new choices rather
    than changing existing private-wire ordinals. }
  TNyxStudioCollectionAction = (scaCreate, scaRemove, scaAddField, scaDefault,
    scaAddRow, scaRemoveRow, scaCell, scaBind, scaScope, scaTitle, scaMode,
    scaParent, scaRemoveColumn, scaAddColumn, scaClear, scaInherit, scaSelection, scaQuery);

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
    { A complete typed replacement, qualified by the form's exact schema/binding
      baseline. Only scaQuery uses these fields; an empty policy clears defaults. }
    Query: TNyxCollectionQuery;
    QueryBaseline: TNyxText;
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
    scaRemoveColumn, scaAddColumn, scaClear, scaInherit, scaSelection, scaQuery];
end;

function NyxStudioCollectionStructuralAction(AAction: TNyxStudioCollectionAction): Boolean;
begin
  Result := AAction in [scaCreate, scaRemove, scaAddField, scaAddRow, scaRemoveRow,
    scaBind, scaRemoveColumn, scaAddColumn, scaClear, scaInherit, scaQuery];
end;

procedure TNyxStudioCollectionIntent.Validate;
var
  LKey: TNyxText;
begin
  LKey := Key.Name;

  if ((Action = scaQuery) and (QueryBaseline = '')) or
    ((Action <> scaQuery) and (Query.Defined or (QueryBaseline <> ''))) then
  begin
    raise ENyxCollection.Create('Query intent requires its own exact baseline and policy');
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
    scaAddColumn]) and not Field.Defined then
  begin
    raise ENyxCollection.Create('Collection intent requires its exact typed field');
  end;

  if Field.Defined and not (Action in [scaDefault, scaCell, scaTitle, scaMode,
    scaParent, scaRemoveColumn, scaAddColumn]) then
  begin
    raise ENyxCollection.Create('This collection intent does not use a field');
  end;

  if (Action = scaParent) and Field.Defined and (Field.Kind <> nskText) then
  begin
    raise ENyxCollection.Create('A parent reference requires a text field');
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
    (not (Action in [scaDefault, scaCell, scaTitle]) and (Value <> '')) or
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
    (Query.ToData.ToJSON = AOther.Query.ToData.ToJSON);

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

  if Action = scaQuery then
  begin
    SetLength(LFields, 13);
    for LIndex := 0 to 10 do
    begin
      LFields[LIndex] := NyxField(Result.Key(LIndex), Result.Field(Result.Key(LIndex)));
    end;
    LFields[11] := NyxField('query', Query.ToData);
    LFields[12] := NyxField('baseline', NyxData(QueryBaseline));
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

  if AIntent.Action = scaQuery then
  begin

    if AIntent.QueryBaseline <> NyxQueryEditorBaseline(ASchema, ASpec) then
    begin
      raise ENyxCollection.Create('Query binding/schema changed; use its current inspector');
    end;
    AIntent.Query.Validate(ASchema);
    Exit(ASpec.Query(AIntent.Query));
  end;

  if AIntent.Action = scaClear then
  begin
    Exit(Default(TNyxCollectionViewSpec));
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
    end;
    Result := AddTypedColumn(Result, LReference, LTitle, LMode);
  end;

  if (AIntent.Action in [scaTitle, scaMode, scaRemoveColumn]) and not LColumnFound then
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

  if (AData.Kind <> ndObject) or not (AData.Count in [11, 13]) then
  begin
    raise ENyxCollection.Create('Collection intent requires its exact descriptor');
  end;
  Result.Action := TNyxStudioCollectionAction(Choice('action',
    Ord(Low(TNyxStudioCollectionAction)), Ord(High(TNyxStudioCollectionAction))));

  if (Result.Action = scaQuery) <> (AData.Count = 13) then
  begin
    raise ENyxCollection.Create('Query intent requires its exact extended descriptor');
  end;

  if Result.Action = scaQuery then
  begin
    Result.Query := TNyxCollectionQuery.FromData(AData.Field('query'));
    Result.QueryBaseline := AData.Field('baseline').AsText;
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
