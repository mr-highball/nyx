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


unit nyx.studio.collectionedits;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.state, nyx.contract, nyx.collections,
  nyx.collections.view.types, nyx.collections.selection, nyx.studio.authoring,
  nyx.studio.collectionintent, nyx.studio.stateedits, nyx.studio.projects;

const
  NyxMaximumCollectionChanges = 32;

type
  TNyxCollectionChangeKind = (ccDefine, ccField, ccRemoveField, ccAppend,
    ccUpdateRow, ccMoveRow, ccIntent, ccBind);

  { Immutable typed proposals own copied values, never a document/store/control.
    A one-field schema supplies named field authoring with its typed default and
    domain. Scoped items keep row identity distinct from open collection names.
    The ordinary intent overload covers the editor's seventeen existing actions. }
  TNyxCollectionChange = record
  private
    FKind: TNyxCollectionChangeKind;
    FKey: TNyxCollectionRef;
    FSchema: TNyxCollectionSchema;
    FItems: array of TNyxCollectionItem;
    FField: TNyxStudioCollectionFieldRef;
    FItem: TNyxCollectionItem;
    FIndex: Integer;
    FOwner: TNyxStudioBindingOwner;
    FIntent: TNyxStudioCollectionIntent;
    FSpec: TNyxCollectionViewSpec;
    FProjection: TNyxCollectionProjection;
  public
    property Kind: TNyxCollectionChangeKind read FKind;
  end;

  { Candidate replays ordinary Studio commands on one independent owned session.
    Each ordered intermediate must admit: clear dependencies before removing a
    field/collection. Failure discards the complete group and leaves APair intact.
    The caller publishes the final pair once, as one paired Undo step. Pending
    Pascal drafts refuse; queries may still inspect accepted document defaults.
    Runtime stores and user-owned navigation are outside this mutation contract. }
  INyxCollectionPatch = interface(IInterface)
    ['{2D83A76B-9649-4382-AB29-7E39C917EA32}']
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
    function ToData: TNyxDataValue;
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

{ New named definitions refuse existing keys; omitted rows cannot erase data. }
function NyxDefineCollection(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema; const AItems: array of TNyxCollectionItem):
  TNyxCollectionChange;
{ Add/update exactly one named field. Existing family must agree; unspecified
  fields/rows retain order and values. Changed domains revalidate every row/view. }
function NyxSetCollectionField(const AKey: TNyxCollectionRef;
  const ADefinition: TNyxCollectionSchema): TNyxCollectionChange;
{ Remove one exact family-qualified field and its row cells; dependent authored
  columns/parent mappings must be cleared earlier in the admitted group. }
function NyxRemoveCollectionField(const AKey: TNyxCollectionRef;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;
{ Append materializes omitted schema defaults; update changes only supplied
  fields of an existing scoped row. Unknown/duplicate identities refuse. }
function NyxAppendCollectionRow(const AItem: TNyxCollectionItem): TNyxCollectionChange;
function NyxUpdateCollectionRow(const AItem: TNyxCollectionItem): TNyxCollectionChange;
{ Final index is in the sequence after removing this existing row (0..Count-1). }
function NyxMoveCollectionRow(const AItem: TNyxItemRef;
  AIndex: Integer): TNyxCollectionChange;
{ Data intents have no owner; view intents require an exact authored owner. }
function NyxCollectionIntentChange(const AIntent: TNyxStudioCollectionIntent):
  TNyxCollectionChange; overload;
function NyxCollectionIntentChange(const AOwner: TNyxStudioBindingOwner;
  const AIntent: TNyxStudioCollectionIntent): TNyxCollectionChange; overload;
{ Full fluent view specification, with explicit expected list/table/tree family.
  Clearing/inheritance remain distinct ordinary intents, rather than null binds. }
function NyxBindCollection(const AOwner: TNyxStudioBindingOwner;
  AProjection: TNyxCollectionProjection; const ASpec: TNyxCollectionViewSpec):
  TNyxCollectionChange;
function NyxCollectionPatch(const AChanges: array of TNyxCollectionChange):
  INyxCollectionPatch;

{ Explicit MCP/descriptor boundary. Friendly closed choices decode to the typed
  API above. Unknown/extra members, wrong primitives, duplicates and unused
  payload refuse. Numeric values retain their exact accepted Pascal family. }
function ReadNyxCollectionPatch(const AData: TNyxDataValue): INyxCollectionPatch;
function NyxCollectionAgentSchema: TNyxDataValue;

implementation

uses
  nyx.model, nyx.studio.session;

type
  TNyxCollectionPatch = class(TInterfacedObject, INyxCollectionPatch)
  private
    FChanges: array of TNyxCollectionChange;
  public
    constructor Create(const AChanges: array of TNyxCollectionChange);
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
    function ToData: TNyxDataValue;
    function GetCount: Integer;
  end;

const
  CCollectionActions: array[TNyxStudioCollectionAction] of TNyxText =
    ('create', 'remove', 'add-field', 'default', 'add-row', 'remove-row', 'cell',
     'bind', 'scope', 'column-title', 'column-mode', 'parent', 'remove-column',
     'add-column', 'clear', 'inherit', 'selection');
  CCollectionProjections: array[TNyxCollectionProjection] of TNyxText =
    ('list', 'table', 'tree');

function PutValue(const AItem: TNyxCollectionItem; const AName: TNyxText;
  const AValue: TNyxStateValue): TNyxCollectionItem;
begin
  case Ord(AValue.Kind) of
    Ord(nskText):
      begin
        Result := AItem.WithValue(NyxTextField(AName), AValue.TextValue);
      end;
    Ord(nskBoolean):
      begin
        Result := AItem.WithValue(NyxBooleanField(AName), AValue.BooleanValue);
      end;
    Ord(nskInteger):
      begin
        Result := AItem.WithValue(NyxIntegerField(AName), AValue.IntegerValue);
      end;
    Ord(nskNumber):
      begin
        Result := AItem.WithValue(NyxNumberField(AName), AValue.NumberValue);
      end;
  else
    begin
      raise ENyxCollection.Create('Unknown collection scalar family');
    end;
  end;
end;

function NyxDefineCollection(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema; const AItems: array of TNyxCollectionItem):
  TNyxCollectionChange;
var
  LIndex: Integer;
begin
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccDefine;
  Result.FKey := NyxCollection(AKey.Name);
  Result.FSchema := ASchema.Copy;
  Result.FSchema.Validate;

  if Length(AItems) > NyxMaximumCollectionItems then
  begin
    raise ENyxCollection.Create('Too many initial collection rows');
  end;
  SetLength(Result.FItems, Length(AItems));
  for LIndex := 0 to High(AItems) do
  begin
    Result.FItems[LIndex] := AItems[LIndex].Copy;

    if Result.FItems[LIndex].Ref.Collection.Name <> AKey.Name then
    begin
      raise ENyxCollection.Create('Initial row belongs to another collection');
    end;
  end;
end;

function NyxSetCollectionField(const AKey: TNyxCollectionRef;
  const ADefinition: TNyxCollectionSchema): TNyxCollectionChange;
begin
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccField;
  Result.FKey := NyxCollection(AKey.Name);
  Result.FSchema := ADefinition.Copy;
  Result.FSchema.Validate;

  if Result.FSchema.Count <> 1 then
  begin
    raise ENyxCollection.Create('Field authoring requires exactly one typed definition');
  end;
end;

function NyxRemoveCollectionField(const AKey: TNyxCollectionRef;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;
begin
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccRemoveField;
  Result.FKey := NyxCollection(AKey.Name);

  if not AField.Defined then
  begin
    raise ENyxCollection.Create('Field removal requires a typed field reference');
  end;
  Result.FField := AField;
end;

function RowChange(const AItem: TNyxCollectionItem;
  AKind: TNyxCollectionChangeKind): TNyxCollectionChange;
begin
  Result := Default(TNyxCollectionChange);
  Result.FKind := AKind;
  Result.FKey := NyxCollection(AItem.Ref.Collection.Name);
  Result.FItem := AItem.Copy;
end;

function NyxAppendCollectionRow(const AItem: TNyxCollectionItem): TNyxCollectionChange;
begin
  Result := RowChange(AItem, ccAppend);
end;

function NyxUpdateCollectionRow(const AItem: TNyxCollectionItem): TNyxCollectionChange;
begin
  Result := RowChange(AItem, ccUpdateRow);
end;

function NyxMoveCollectionRow(const AItem: TNyxItemRef;
  AIndex: Integer): TNyxCollectionChange;
begin

  if (AIndex < 0) or (AIndex >= NyxMaximumCollectionItems) then
  begin
    raise ENyxCollection.Create('Collection row index is outside its budget');
  end;
  Result := RowChange(NyxCollectionItem(AItem), ccMoveRow);
  Result.FIndex := AIndex;
end;

function NyxCollectionIntentChange(const AIntent: TNyxStudioCollectionIntent):
  TNyxCollectionChange;
begin
  AIntent.Validate;

  if NyxStudioCollectionViewAction(AIntent.Action) then
  begin
    raise ENyxCollection.Create('View intent requires its exact authored owner');
  end;
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccIntent;
  Result.FKey := NyxCollection(AIntent.Key.Name);
  Result.FIntent := AIntent;
end;

function NyxCollectionIntentChange(const AOwner: TNyxStudioBindingOwner;
  const AIntent: TNyxStudioCollectionIntent): TNyxCollectionChange;
begin
  AIntent.Validate;

  if not NyxStudioCollectionViewAction(AIntent.Action) then
  begin
    raise ENyxCollection.Create('Data intent cannot carry an authored view owner');
  end;
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccIntent;
  Result.FKey := NyxCollection(AIntent.Key.Name);
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FIntent := AIntent;
end;

function NyxBindCollection(const AOwner: TNyxStudioBindingOwner;
  AProjection: TNyxCollectionProjection; const ASpec: TNyxCollectionViewSpec):
  TNyxCollectionChange;
begin
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccBind;
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FSpec := ASpec.Copy;
  Result.FSpec.Validate;

  if not Result.FSpec.Defined or (Ord(AProjection) < Ord(Low(TNyxCollectionProjection))) or
    (Ord(AProjection) > Ord(High(TNyxCollectionProjection))) then
  begin
    raise ENyxCollection.Create('Bind requires a defined view and a valid projection');
  end;
  Result.FKey := NyxCollection(ASpec.Key.Name);
  Result.FProjection := AProjection;
end;

constructor TNyxCollectionPatch.Create(const AChanges: array of TNyxCollectionChange);
var
  LIndex: Integer;
  LRow: Integer;
begin
  inherited Create;

  if (Length(AChanges) < 1) or (Length(AChanges) > NyxMaximumCollectionChanges) then
  begin
    raise ENyxCollection.Create('Collection group requires 1..32 changes');
  end;
  SetLength(FChanges, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin
    FChanges[LIndex] := AChanges[LIndex];
    { Immutable public records are copied explicitly at the owned group boundary;
      caller array replacement cannot mutate a patch on either compiler. }
    FChanges[LIndex].FItems := nil;
    SetLength(FChanges[LIndex].FItems, Length(AChanges[LIndex].FItems));
    for LRow := 0 to High(AChanges[LIndex].FItems) do
    begin
      FChanges[LIndex].FItems[LRow] := AChanges[LIndex].FItems[LRow].Copy;
    end;
  end;
end;

function TNyxCollectionPatch.GetCount: Integer;
begin
  Result := Length(FChanges);
end;

procedure SelectCollectionOwner(ASession: TNyxStudioSession;
  const AOwner: TNyxStudioBindingOwner; AProjection: TNyxCollectionProjection);
var
  LNode: TNyxNode;
  LProjected: TNyxNode;
begin
  LNode := ASession.Document.Find(AOwner.ID);

  if LNode = nil then
  begin
    raise ENyxCollection.Create('The exact authored collection owner is missing');
  end;
  while LNode.Parent <> nil do
  begin
    LNode := LNode.Parent;
  end;
  ASession.Activate(LNode.ID);
  ASession.Select(AOwner.ID);
  LProjected := ASession.SelectedProjection;
  try

    if (LProjected = nil) or
      (LProjected.ProjectionKind <> CCollectionProjections[AProjection]) then
    begin
      raise ENyxCollection.Create('The authored collection projection changed');
    end;
  finally
    LProjected.Free;
  end;
end;

function TNyxCollectionPatch.Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
var
  LSession: TNyxStudioSession;
  LChange: TNyxCollectionChange;
  LData: INyxCollectionSnapshot;
  LSchema: TNyxCollectionSchema;
  LNextSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
  LDefinition: TNyxCollectionField;
  LItems: array of TNyxCollectionItem;
  LRow: TNyxCollectionItem;
  LIndex: Integer;
  LColumn: Integer;
  LItemIndex: Integer;
  LOther: Integer;
  LFound: Boolean;
  LValue: TNyxStateValue;
begin
  LSession := TNyxStudioSession.Create(APair);
  try

    if LSession.DraftSource <> LSession.Source then
    begin
      raise ENyxCollection.Create('Resolve the pending Pascal draft before collection edits');
    end;
    for LIndex := 0 to High(FChanges) do
    begin
      LChange := FChanges[LIndex];
      case LChange.FKind of
        ccIntent:
          begin

            if NyxStudioCollectionViewAction(LChange.FIntent.Action) then
            begin
              SelectCollectionOwner(LSession, LChange.FOwner, LChange.FIntent.Projection);
            end;
            LSession.ApplyCollectionIntent(LChange.FIntent);
          end;
        ccBind:
          begin
            SelectCollectionOwner(LSession, LChange.FOwner, LChange.FProjection);
            LSession.SetCollectionView(LChange.FSpec);
          end;
        ccDefine:
          begin

            if LSession.Document.Collections.Has(LChange.FKey) then
            begin
              raise ENyxCollection.Create('Named collection already exists');
            end;
            LSession.DefineCollection(LChange.FKey, LChange.FSchema, LChange.FItems);
          end;
      else
        begin
          LData := LSession.Document.Collections.Snapshot(LChange.FKey);
          LSchema := LData.Schema;
          SetLength(LItems, LData.Count);
          for LOther := 0 to LData.Count - 1 do
          begin
            LItems[LOther] := LData.ItemAt(LOther).Copy;
          end;
          case LChange.FKind of
            ccField, ccRemoveField:
              begin
                LNextSchema := NyxCollectionSchema;
                LFound := False;

                if LChange.FKind = ccField then
                begin
                  LDefinition := LChange.FSchema.FieldAt(0);
                end;
                for LColumn := 0 to LSchema.Count - 1 do
                begin
                  LField := LSchema.FieldAt(LColumn);

                  if ((LChange.FKind = ccField) and (LField.Name = LDefinition.Name)) or
                    ((LChange.FKind = ccRemoveField) and
                     (LField.Name = LChange.FField.Name)) then
                  begin
                    LFound := True;

                    if LChange.FKind = ccField then
                    begin

                      if LField.Kind <> LDefinition.Kind then
                      begin
                        raise ENyxCollection.Create('Field update cannot change scalar family');
                      end;
                      LNextSchema := LNextSchema.Field(LDefinition.Name,
                        LDefinition.DefaultValue, LDefinition.Domain);
                    end
                    else if LField.Kind <> LChange.FField.Kind then
                    begin
                      raise ENyxCollection.Create('Field removal family changed');
                    end;
                  end
                  else
                  begin
                    LNextSchema := LNextSchema.Field(LField.Name,
                      LField.DefaultValue, LField.Domain);
                  end;
                end;

                if not LFound then
                begin

                  if LChange.FKind = ccRemoveField then
                  begin
                    raise ENyxCollection.Create('Removed field no longer exists');
                  end;
                  LNextSchema := LNextSchema.Field(LDefinition.Name,
                    LDefinition.DefaultValue, LDefinition.Domain);
                end;

                if LChange.FKind = ccRemoveField then
                begin
                  for LOther := 0 to High(LItems) do
                  begin
                    LRow := NyxCollectionItem(LItems[LOther].Ref);
                    for LColumn := 0 to LItems[LOther].Count - 1 do
                    begin

                      if LItems[LOther].FieldName(LColumn) <> LChange.FField.Name then
                      begin
                        LRow := PutValue(LRow, LItems[LOther].FieldName(LColumn),
                          LItems[LOther].FieldValue(LColumn));
                      end;
                    end;
                    LItems[LOther] := LRow;
                  end;
                end;
                LSchema := LNextSchema;
              end;
            ccAppend:
              begin

                if LData.IndexOf(LChange.FItem.Ref) >= 0 then
                begin
                  raise ENyxCollection.Create('Appended row already exists');
                end;
                SetLength(LItems, Length(LItems) + 1);
                LItems[High(LItems)] := LChange.FItem.Copy;
              end;
            ccUpdateRow, ccMoveRow:
              begin
                LItemIndex := LData.IndexOf(LChange.FItem.Ref);

                if LItemIndex < 0 then
                begin
                  raise ENyxCollection.Create('The exact scoped row is missing');
                end;

                if LChange.FKind = ccMoveRow then
                begin

                  if LChange.FIndex >= LData.Count then
                  begin
                    raise ENyxCollection.Create('Final row index exceeds this collection');
                  end;
                  LRow := LItems[LItemIndex];

                  if LItemIndex < LChange.FIndex then
                  begin
                    for LOther := LItemIndex to LChange.FIndex - 1 do
                    begin
                      LItems[LOther] := LItems[LOther + 1];
                    end;
                  end
                  else
                  begin
                    for LOther := LItemIndex downto LChange.FIndex + 1 do
                    begin
                      LItems[LOther] := LItems[LOther - 1];
                    end;
                  end;
                  LItems[LChange.FIndex] := LRow;
                end
                else
                begin
                  LRow := LItems[LItemIndex];
                  for LColumn := 0 to LChange.FItem.Count - 1 do
                  begin
                    LValue := LChange.FItem.FieldValue(LColumn);
                    LFound := False;
                    for LOther := 0 to LSchema.Count - 1 do
                    begin
                      LField := LSchema.FieldAt(LOther);

                      if LField.Name = LChange.FItem.FieldName(LColumn) then
                      begin

                        if LField.Kind <> LValue.Kind then
                        begin
                          raise ENyxCollection.Create('Updated cell family changed');
                        end;
                        LFound := True;
                        Break;
                      end;
                    end;

                    if not LFound then
                    begin
                      raise ENyxCollection.Create('Updated cell field does not exist');
                    end;
                    LRow := PutValue(LRow, LChange.FItem.FieldName(LColumn), LValue);
                  end;
                  LItems[LItemIndex] := LRow;
                end;
              end;
          else
            begin
              raise ENyxCollection.Create('Unknown collection data change');
            end;
          end;
          LSession.DefineCollection(LChange.FKey, LSchema, LItems);
        end;
      end;
    end;
    Result := LSession.ProjectSnapshot;
  finally
    LSession.Free;
  end;
end;

{ All raw member names and choices below are confined to this explicit protocol
  boundary. The ordinary authoring and candidate commands above use Pascal types. }
procedure Fields(const AData: TNyxDataValue; const AAllowed: TNyxText;
  ACount: Integer = -1);
var
  LIndex: Integer;
begin
  AData.Validate;

  if (AData.Kind <> ndObject) or ((ACount >= 0) and (AData.Count <> ACount)) then
  begin
    raise ENyxCollection.Create('Malformed collection operation object');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if (Pos('|', AData.Key(LIndex)) > 0) or
      (Pos('|' + AData.Key(LIndex) + '|', AAllowed) = 0) then
    begin
      raise ENyxCollection.Create('Unknown collection operation member: ' + AData.Key(LIndex));
    end;
  end;
end;

function ReadKind(const AData: TNyxDataValue): TNyxStateKind;
var
  LKind: TNyxStateKind;
begin
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin

    if AData.AsText = NyxStateKindName(LKind) then
    begin
      Exit(LKind);
    end;
  end;
  raise ENyxCollection.Create('Unknown collection scalar family');
end;

function ReadValue(AKind: TNyxStateKind; const AData: TNyxDataValue): TNyxStateValue;
begin
  case Ord(AKind) of
    Ord(nskText):
      begin
        Result := TNyxStateValue.FromText(AData.AsText);
      end;
    Ord(nskBoolean):
      begin
        Result := TNyxStateValue.FromBoolean(AData.AsBoolean);
      end;
    Ord(nskInteger):
      begin
        Result := TNyxStateValue.FromInteger(AData.AsInteger);
      end;
    Ord(nskNumber):
      begin
        Result := TNyxStateValue.FromNumber(AData.AsNumber);
      end;
  else
    begin
      raise ENyxCollection.Create('Unknown collection scalar family');
    end;
  end;
end;

function ReadField(const AName: TNyxText; AKind: TNyxStateKind):
  TNyxStudioCollectionFieldRef;
begin
  case Ord(AKind) of
    Ord(nskText):
      begin
        Result := TNyxStudioCollectionFieldRef.Text(NyxTextField(AName));
      end;
    Ord(nskBoolean):
      begin
        Result := TNyxStudioCollectionFieldRef.Boolean(NyxBooleanField(AName));
      end;
    Ord(nskInteger):
      begin
        Result := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField(AName));
      end;
    Ord(nskNumber):
      begin
        Result := TNyxStudioCollectionFieldRef.Number(NyxNumberField(AName));
      end;
  else
    begin
      raise ENyxCollection.Create('Unknown collection scalar family');
    end;
  end;
end;

function ReadSchema(const AData: TNyxDataValue): TNyxCollectionSchema;
var
  LIndex: Integer;
  LField: TNyxDataValue;
  LDomain: TNyxValueDomain;
  LValue: TNyxStateValue;
  LMember: Integer;
begin

  if (AData.Kind <> ndArray) or (AData.Count < 1) or
    (AData.Count > NyxMaximumCollectionFields) then
  begin
    raise ENyxCollection.Create('Schema requires 1..64 named fields');
  end;
  Result := NyxCollectionSchema;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LField := AData.Item(LIndex);
    Fields(LField, '|name|kind|default|domain|');
    LValue := ReadValue(ReadKind(LField.Field('kind')), LField.Field('default'));
    LDomain := NyxNoDomain;

    for LMember := 0 to LField.Count - 1 do
    begin

      if LField.Key(LMember) = 'domain' then
      begin
        LDomain := TNyxValueDomain.FromData(LField.Field('domain'));
      end;
    end;
    Result := Result.Field(LField.Field('name').AsText, LValue, LDomain);
  end;
end;

function ReadRow(const AKey: TNyxCollectionRef; const AID: TNyxText;
  const AValues: TNyxDataValue): TNyxCollectionItem;
var
  LIndex: Integer;
  LOther: Integer;
  LCell: TNyxDataValue;
  LName: TNyxText;
begin

  if (AValues.Kind <> ndArray) or (AValues.Count > NyxMaximumCollectionFields) then
  begin
    raise ENyxCollection.Create('Row values require an array of at most 64 typed cells');
  end;
  Result := NyxCollectionItem(NyxItem(AKey, AID));
  for LIndex := 0 to AValues.Count - 1 do
  begin
    LCell := AValues.Item(LIndex);
    Fields(LCell, '|field|kind|value|', 3);
    LName := LCell.Field('field').AsText;
    for LOther := 0 to Result.Count - 1 do
    begin

      if Result.FieldName(LOther) = LName then
      begin
        raise ENyxCollection.Create('Duplicate row value field');
      end;
    end;
    Result := PutValue(Result, LName,
      ReadValue(ReadKind(LCell.Field('kind')), LCell.Field('value')));
  end;
end;

function ReadProjection(const AData: TNyxDataValue): TNyxCollectionProjection;
var
  LProjection: TNyxCollectionProjection;
begin
  for LProjection := Low(TNyxCollectionProjection) to High(TNyxCollectionProjection) do
  begin

    if AData.AsText = CCollectionProjections[LProjection] then
    begin
      Exit(LProjection);
    end;
  end;
  raise ENyxCollection.Create('Projection must be list, table or tree');
end;

function IntentData(const AChange: TNyxCollectionChange): TNyxDataValue;
var
  LIntent: TNyxStudioCollectionIntent;
  LFields: array of TNyxDataField;
  LIndex: Integer;

  procedure Add(const AName: TNyxText; const AValue: TNyxDataValue);
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField(AName, AValue);
  end;

begin
  LIntent := AChange.FIntent;
  LFields := nil;
  Add('op', NyxData('intent'));
  Add('action', NyxData(CCollectionActions[LIntent.Action]));
  Add('key', NyxData(LIntent.Key.Name));

  if NyxStudioCollectionViewAction(LIntent.Action) then
  begin
    Add('owner', NyxData(AChange.FOwner.ID));
    Add('projection', NyxData(CCollectionProjections[LIntent.Projection]));
  end;

  if LIntent.Action in [scaAddRow, scaRemoveRow, scaCell] then
  begin
    Add('item', NyxData(LIntent.Item.ID));
  end;

  if LIntent.Action in [scaDefault, scaCell, scaTitle, scaMode,
    scaRemoveColumn, scaAddColumn] then
  begin
    Add('field', NyxData(LIntent.Field.Name));
    Add('kind', NyxData(NyxStateKindName(LIntent.Field.Kind)));
  end;

  if LIntent.Action = scaAddField then
  begin
    Add('kind', NyxData(NyxStateKindName(LIntent.Kind)));
  end;

  if LIntent.Action in [scaDefault, scaCell] then
  begin
    Add('value', NyxStateValueData(ParseNyxStudioStateInput(LIntent.Input, LIntent.Value)));
  end;

  if LIntent.Action = scaTitle then
  begin
    Add('title', NyxData(LIntent.Value));
  end;

  if LIntent.Action = scaMode then
  begin
    Add('editable', NyxData(LIntent.Mode = cmEditable));
  end;

  if LIntent.Action = scaParent then
  begin
    Add('parent', NyxNull);

    if LIntent.Field.Defined then
    begin
      LIndex := High(LFields);
      LFields[LIndex] := NyxField('parent', NyxData(LIntent.Field.Name));
    end;
  end;

  if LIntent.Action = scaScope then
  begin

    if LIntent.Scope = csInstance then
    begin
      Add('scope', NyxData('instance'));
    end
    else
    begin
      Add('scope', NyxData('application'));
    end;
  end;

  if LIntent.Action = scaSelection then
  begin

    if LIntent.SelectionMode = nsmMultiple then
    begin
      Add('selection', NyxData('multiple'));
    end
    else
    begin
      Add('selection', NyxData('single'));
    end;
  end;
  Result := NyxObject(LFields);
end;

function ReadIntent(const AData: TNyxDataValue): TNyxCollectionChange;
var
  LIntent: TNyxStudioCollectionIntent;
  LAction: TNyxStudioCollectionAction;
  LFound: Boolean;
  LAllowed: TNyxText;
  LRequired: Integer;
  LValue: TNyxStateValue;
begin
  LIntent := Default(TNyxStudioCollectionIntent);
  LFound := False;
  for LAction := Low(TNyxStudioCollectionAction) to High(TNyxStudioCollectionAction) do
  begin

    if AData.Field('action').AsText = CCollectionActions[LAction] then
    begin
      LIntent.Action := LAction;
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxCollection.Create('Unknown ordinary collection action');
  end;
  LIntent.Key := NyxCollection(AData.Field('key').AsText);
  LAllowed := '|op|action|key|';
  LRequired := 3;

  if NyxStudioCollectionViewAction(LIntent.Action) then
  begin
    LAllowed := LAllowed + 'owner|projection|';
    Inc(LRequired, 2);
    LIntent.Projection := ReadProjection(AData.Field('projection'));
  end;

  if LIntent.Action in [scaAddRow, scaRemoveRow, scaCell] then
  begin
    LAllowed := LAllowed + 'item|';
    Inc(LRequired);
    LIntent.Item := NyxItem(LIntent.Key, AData.Field('item').AsText);
  end;

  if LIntent.Action in [scaDefault, scaCell, scaTitle, scaMode,
    scaRemoveColumn, scaAddColumn] then
  begin
    LAllowed := LAllowed + 'field|kind|';
    Inc(LRequired, 2);
    LIntent.Field := ReadField(AData.Field('field').AsText, ReadKind(AData.Field('kind')));
  end;

  if LIntent.Action = scaAddField then
  begin
    LAllowed := LAllowed + 'kind|';
    Inc(LRequired);
    LIntent.Kind := ReadKind(AData.Field('kind'));
  end;

  if LIntent.Action in [scaDefault, scaCell] then
  begin
    LAllowed := LAllowed + 'value|';
    Inc(LRequired);
    LValue := ReadValue(LIntent.Field.Kind, AData.Field('value'));
    LIntent.Input := NyxStudioStateInputFor(LValue);
    LIntent.Value := NyxStudioStateEditorText(LValue);
  end;

  if LIntent.Action = scaTitle then
  begin
    LAllowed := LAllowed + 'title|';
    Inc(LRequired);
    LIntent.Value := AData.Field('title').AsText;
  end;

  if LIntent.Action = scaMode then
  begin
    LAllowed := LAllowed + 'editable|';
    Inc(LRequired);

    if AData.Field('editable').AsBoolean then
    begin
      LIntent.Mode := cmEditable;
    end;
  end;

  if LIntent.Action = scaParent then
  begin
    LAllowed := LAllowed + 'parent|';
    Inc(LRequired);

    if AData.Field('parent').Kind <> ndNull then
    begin
      LIntent.Field := TNyxStudioCollectionFieldRef.Text(
        NyxTextField(AData.Field('parent').AsText));
    end;
  end;

  if LIntent.Action = scaScope then
  begin
    LAllowed := LAllowed + 'scope|';
    Inc(LRequired);

    if AData.Field('scope').AsText = 'instance' then
    begin
      LIntent.Scope := csInstance;
    end
    else if AData.Field('scope').AsText <> 'application' then
    begin
      raise ENyxCollection.Create('Unknown collection scope');
    end;
  end;

  if LIntent.Action = scaSelection then
  begin
    LAllowed := LAllowed + 'selection|';
    Inc(LRequired);

    if AData.Field('selection').AsText = 'multiple' then
    begin
      LIntent.SelectionMode := nsmMultiple;
    end
    else if AData.Field('selection').AsText <> 'single' then
    begin
      raise ENyxCollection.Create('Unknown collection selection mode');
    end;
  end;
  Fields(AData, LAllowed, LRequired);

  if NyxStudioCollectionViewAction(LIntent.Action) then
  begin
    Result := NyxCollectionIntentChange(NyxBindingOwner(AData.Field('owner').AsText), LIntent);
  end
  else
  begin
    Result := NyxCollectionIntentChange(LIntent);
  end;
end;

function RowData(const AItem: TNyxCollectionItem): TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LIndex: Integer;
  LValue: TNyxStateValue;
begin
  SetLength(LValues, AItem.Count);
  for LIndex := 0 to AItem.Count - 1 do
  begin
    LValue := AItem.FieldValue(LIndex);
    LValues[LIndex] := NyxObject([
      NyxField('field', NyxData(AItem.FieldName(LIndex))),
      NyxField('kind', NyxData(NyxStateKindName(LValue.Kind))),
      NyxField('value', NyxStateValueData(LValue))]);
  end;
  Result := NyxObject([NyxField('item', NyxData(AItem.Ref.ID)),
    NyxField('values', NyxArray(LValues))]);
end;

function SchemaData(const ASchema: TNyxCollectionSchema): TNyxDataValue;
var
  LFields: array of TNyxDataValue;
  LIndex: Integer;
  LField: TNyxCollectionField;
begin
  SetLength(LFields, ASchema.Count);
  for LIndex := 0 to ASchema.Count - 1 do
  begin
    LField := ASchema.FieldAt(LIndex);
    LFields[LIndex] := NyxObject([
      NyxField('name', NyxData(LField.Name)),
      NyxField('kind', NyxData(NyxStateKindName(LField.Kind))),
      NyxField('default', NyxStateValueData(LField.DefaultValue)),
      NyxField('domain', LField.Domain.ToData)]);
  end;
  Result := NyxArray(LFields);
end;

function TNyxCollectionPatch.ToData: TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LRows: array of TNyxDataValue;
  LChange: TNyxCollectionChange;
  LIndex: Integer;
  LRow: Integer;
  LOp: TNyxText;
  LRowData: TNyxDataValue;
begin
  SetLength(LValues, GetCount);
  for LIndex := 0 to High(FChanges) do
  begin
    LChange := FChanges[LIndex];
    case LChange.FKind of
      ccDefine:
        begin
          SetLength(LRows, Length(LChange.FItems));
          for LRow := 0 to High(LChange.FItems) do
          begin
            LRows[LRow] := RowData(LChange.FItems[LRow]);
          end;
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('define')),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('fields', SchemaData(LChange.FSchema)),
            NyxField('rows', NyxArray(LRows))]);
        end;
      ccField:
        begin
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('field')),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('definition', SchemaData(LChange.FSchema).Item(0))]);
        end;
      ccRemoveField:
        begin
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('remove-field')),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('field', NyxData(LChange.FField.Name)),
            NyxField('kind', NyxData(NyxStateKindName(LChange.FField.Kind)))]);
        end;
      ccAppend, ccUpdateRow:
        begin
          LOp := 'append';

          if LChange.FKind = ccUpdateRow then
          begin
            LOp := 'update-row';
          end;
          LRowData := RowData(LChange.FItem);
          LValues[LIndex] := NyxObject([NyxField('op', NyxData(LOp)),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('item', LRowData.Field('item')),
            NyxField('values', LRowData.Field('values'))]);
        end;
      ccMoveRow:
        begin
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('move-row')),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('item', NyxData(LChange.FItem.Ref.ID)),
            NyxField('index', NyxData(LChange.FIndex))]);
        end;
      ccIntent:
        begin
          LValues[LIndex] := IntentData(LChange);
        end;
      ccBind:
        begin
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('bind')),
            NyxField('owner', NyxData(LChange.FOwner.ID)),
            NyxField('projection', NyxData(CCollectionProjections[LChange.FProjection])),
            NyxField('spec', LChange.FSpec.ToData)]);
        end;
    end;
  end;
  Result := NyxArray(LValues);
end;

function NyxCollectionPatch(const AChanges: array of TNyxCollectionChange): INyxCollectionPatch;
begin
  Result := TNyxCollectionPatch.Create(AChanges);
end;

function ReadNyxCollectionPatch(const AData: TNyxDataValue): INyxCollectionPatch;
var
  LChanges: array of TNyxCollectionChange;
  LRows: array of TNyxCollectionItem;
  LData: TNyxDataValue;
  LRow: TNyxDataValue;
  LKey: TNyxCollectionRef;
  LIndex: Integer;
  LRowIndex: Integer;
  LOp: TNyxText;
begin
  AData.Validate;

  if (AData.Kind <> ndArray) or (AData.Count < 1) or
    (AData.Count > NyxMaximumCollectionChanges) then
  begin
    raise ENyxCollection.Create('Collection group requires 1..32 changes');
  end;
  SetLength(LChanges, AData.Count);
  for LIndex := 0 to AData.Count - 1 do
  begin
    LData := AData.Item(LIndex);
    LOp := LData.Field('op').AsText;

    if LOp = 'intent' then
    begin
      LChanges[LIndex] := ReadIntent(LData);
      Continue;
    end;

    if LOp = 'bind' then
    begin
      Fields(LData, '|op|owner|projection|spec|', 4);
      LChanges[LIndex] := NyxBindCollection(NyxBindingOwner(LData.Field('owner').AsText),
        ReadProjection(LData.Field('projection')),
        TNyxCollectionViewSpec.FromData(LData.Field('spec')));
      Continue;
    end;
    LKey := NyxCollection(LData.Field('key').AsText);

    if LOp = 'define' then
    begin
      Fields(LData, '|op|key|fields|rows|', 4);
      LRow := LData.Field('rows');

      if (LRow.Kind <> ndArray) or (LRow.Count > NyxMaximumCollectionItems) then
      begin
        raise ENyxCollection.Create('Initial rows require a bounded array');
      end;
      SetLength(LRows, LRow.Count);
      for LRowIndex := 0 to LRow.Count - 1 do
      begin
        Fields(LRow.Item(LRowIndex), '|item|values|', 2);
        LRows[LRowIndex] := ReadRow(LKey, LRow.Item(LRowIndex).Field('item').AsText,
          LRow.Item(LRowIndex).Field('values'));
      end;
      LChanges[LIndex] := NyxDefineCollection(LKey, ReadSchema(LData.Field('fields')), LRows);
    end
    else if LOp = 'field' then
    begin
      Fields(LData, '|op|key|definition|', 3);
      LChanges[LIndex] := NyxSetCollectionField(LKey,
        ReadSchema(NyxArray([LData.Field('definition')])));
    end
    else if LOp = 'remove-field' then
    begin
      Fields(LData, '|op|key|field|kind|', 4);
      LChanges[LIndex] := NyxRemoveCollectionField(LKey,
        ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))));
    end
    else if (LOp = 'append') or (LOp = 'update-row') then
    begin
      Fields(LData, '|op|key|item|values|', 4);
      LRows := nil;
      SetLength(LRows, 1);
      LRows[0] := ReadRow(LKey, LData.Field('item').AsText, LData.Field('values'));

      if LOp = 'append' then
      begin
        LChanges[LIndex] := NyxAppendCollectionRow(LRows[0]);
      end
      else
      begin
        LChanges[LIndex] := NyxUpdateCollectionRow(LRows[0]);
      end;
    end
    else if LOp = 'move-row' then
    begin
      Fields(LData, '|op|key|item|index|', 4);
      LChanges[LIndex] := NyxMoveCollectionRow(NyxItem(LKey, LData.Field('item').AsText),
        LData.Field('index').AsInteger);
    end
    else
    begin
      raise ENyxCollection.Create('Unknown collection change');
    end;
  end;
  Result := NyxCollectionPatch(LChanges);
end;

{$I nyx.studio.collectionedits.schema.inc}

end.
