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

{ Private implementation of nyx.collections. Published row records are immutable:
  all item builders allocate new arrays and all scalar copies are explicit.
  Structural sharing keeps snapshots/independent clones from copying every cell
  on every edit. Only a detached candidate's outer array is ever shifted. }

type
  TCollectionItems = array of TNyxCollectionItem;
  TCollectionStep = record
    Kind: TNyxCollectionEditKind;
    Ref: TNyxItemRef;
    BeforeIndex: Integer;
    AfterIndex: Integer;
    BeforeItem: TNyxCollectionItem;
    AfterItem: TNyxCollectionItem;
  end;
  TCollectionSteps = array of TCollectionStep;
  TCollectionStore = class;

  TCollectionSnapshot = class(TInterfacedObject, INyxCollectionSnapshot)
  private
    FKey: TNyxCollectionRef;
    FSchema: TNyxCollectionSchema;
    FItems: TCollectionItems;
    FIndex: array of Integer;
    FRevision: Integer;
    FBytes: Integer;
    function Bucket(const AID: TNyxText): Integer;
  public
    constructor Create(const AKey: TNyxCollectionRef; const ASchema: TNyxCollectionSchema;
      const AItems: TCollectionItems; ARevision, ABytes: Integer);
    function GetKey: TNyxCollectionRef;
    function GetSchema: TNyxCollectionSchema;
    function GetCount: Integer;
    function GetRevision: Integer;
    function GetDataBytes: Integer;
    function ItemAt(AIndex: Integer): TNyxCollectionItem;
    function Item(const ARef: TNyxItemRef): TNyxCollectionItem;
    function IndexOf(const ARef: TNyxItemRef): Integer;
    function Has(const ARef: TNyxItemRef): Boolean;
  end;

  TCollectionChanges = class(TInterfacedObject, INyxCollectionChanges)
  private
    FBefore: INyxCollectionSnapshot;
    FAfter: INyxCollectionSnapshot;
    FSteps: TCollectionSteps;
    procedure CheckIndex(AIndex: Integer);
  public
    constructor Create(const ABefore, AAfter: INyxCollectionSnapshot;
      const ASteps: TCollectionSteps);
    function GetCount: Integer;
    function GetBefore: INyxCollectionSnapshot;
    function GetAfter: INyxCollectionSnapshot;
    function Kind(AIndex: Integer): TNyxCollectionEditKind;
    function ItemRef(AIndex: Integer): TNyxItemRef;
    function BeforeIndex(AIndex: Integer): Integer;
    function AfterIndex(AIndex: Integer): Integer;
    function BeforeItem(AIndex: Integer): TNyxCollectionItem;
    function AfterItem(AIndex: Integer): TNyxCollectionItem;
  end;

  TCollectionSubscription = class(TInterfacedObject, INyxCollectionSubscription)
  public
    Owner: TCollectionStore;
    Serial: Integer;
    Validator: TNyxCollectionValidator;
    Observer: TNyxCollectionObserver;
    destructor Destroy; override;
    function GetConnected: Boolean;
    procedure Disconnect;
  end;

  TCollectionStore = class(TInterfacedObject, INyxCollection)
  private
    FKey: TNyxCollectionRef;
    FSchema: TNyxCollectionSchema;
    FItems: TCollectionItems;
    FCurrent: INyxCollectionSnapshot;
    FSubscriptions: array of TCollectionSubscription;
    FSerial: Integer;
    FBusy: Boolean;
    procedure CheckWritable(AExpectedRevision: Integer);
    function Normalize(const AItem: TNyxCollectionItem;
      const ABase: TNyxCollectionItem; AUpdate: Boolean): TNyxCollectionItem;
    function DatasetBytes(const AItems: TCollectionItems): Integer;
    function Connection(ASerial: Integer): TCollectionSubscription;
    procedure Disconnect(AToken: TCollectionSubscription);
    procedure Publish(const AItems: TCollectionItems; const ASteps: TCollectionSteps);
  public
    constructor Create(const AKey: TNyxCollectionRef; const ASchema: TNyxCollectionSchema;
      const AItems: array of TNyxCollectionItem);
    destructor Destroy; override;
    function Snapshot: INyxCollectionSnapshot;
    function Append(const AItem: TNyxCollectionItem): INyxCollection;
    function Insert(AIndex: Integer; const AItem: TNyxCollectionItem): INyxCollection;
    function Remove(const ARef: TNyxItemRef): INyxCollection;
    function Move(const ARef: TNyxItemRef; AIndex: Integer): INyxCollection;
    function Update(const AItem: TNyxCollectionItem): INyxCollection;
    function Replace(const AItem: TNyxCollectionItem): INyxCollection;
    procedure Apply(const AEdits: array of TNyxCollectionEdit; AExpectedRevision: Integer = -1);
    procedure Assign(const ASource: INyxCollectionSnapshot; AExpectedRevision: Integer = -1);
    function Clone: INyxCollection;
    function Subscribe(AObserver: TNyxCollectionObserver;
      AValidator: TNyxCollectionValidator = nil): INyxCollectionSubscription;
  end;

function NameBytes(const AName: TNyxText; AReference: Boolean): Integer;
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
  LContent: Boolean;
begin
  Result := 0;
  LIndex := 1;
  LCount := 0;
  LContent := False;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) then
    begin
      raise ENyxCollection.Create('Collection text contains malformed Unicode');
    end;
    Inc(LCount);
    LContent := LContent or not NyxScalarWhitespace(LScalar);

    if AReference and ((LCount > 128) or (LScalar < 32) or
      ((LScalar >= $7f) and (LScalar <= $9f)) or (LScalar = $2028) or (LScalar = $2029)) then
    begin
      raise ENyxCollection.Create('Collection references require 1..128 Unicode scalars without controls');
    end;

    if LScalar <= $7f then
    begin
      Inc(Result);
    end
    else if LScalar <= $7ff then
    begin
      Inc(Result, 2);
    end
    else if LScalar <= $ffff then
    begin
      Inc(Result, 3);
    end
    else
    begin
      Inc(Result, 4);
    end;

    if Result > NyxMaximumCollectionBytes then
    begin
      raise ENyxCollection.Create('Collection text exceeds its UTF-8 byte budget');
    end;
  end;

  if AReference and not LContent then
  begin
    raise ENyxCollection.Create('Collection references require non-whitespace content');
  end;
end;

function NyxCollection(const AName: TNyxText): TNyxCollectionRef;
begin
  NameBytes(AName, True);
  Result.FName := AName;
  Result.FInitialized := True;
end;

function TNyxCollectionRef.GetName: TNyxText;
begin

  if not FInitialized then
  begin
    raise ENyxCollection.Create('Construct a collection reference');
  end;
  Result := FName;
end;

function NyxItem(const ACollection: TNyxCollectionRef; const AID: TNyxText): TNyxItemRef;
begin
  ACollection.GetName;
  NameBytes(AID, True);
  Result.FCollection.FName := ACollection.FName;
  Result.FCollection.FInitialized := True;
  Result.FID := AID;
  Result.FInitialized := True;
end;

function TNyxItemRef.GetID: TNyxText;
begin

  if not FInitialized then
  begin
    raise ENyxCollection.Create('Construct a scoped item reference');
  end;
  Result := FID;
end;

function TNyxItemRef.GetCollection: TNyxCollectionRef;
begin
  GetID;
  Result.FName := FCollection.GetName;
  Result.FInitialized := True;
end;

procedure RequireItem(const AKey: TNyxCollectionRef; const ARef: TNyxItemRef);
begin
  ARef.GetID;

  if ARef.FCollection.GetName <> AKey.GetName then
  begin
    raise ENyxCollection.Create('Item reference belongs to a different collection');
  end;
end;


function NyxTextField(const AName: TNyxText): TNyxTextFieldRef;
begin
  NameBytes(AName, True);
  Result.FName := AName;
  Result.FInitialized := True;
end;

function TNyxTextFieldRef.GetName: TNyxText;
begin

  if not FInitialized then
  begin
    raise ENyxCollection.Create('Construct a typed text field reference');
  end;
  Result := FName;
end;

function NyxBooleanField(const AName: TNyxText): TNyxBooleanFieldRef;
begin
  NameBytes(AName, True);
  Result.FName := AName;
  Result.FInitialized := True;
end;

function TNyxBooleanFieldRef.GetName: TNyxText;
begin

  if not FInitialized then
  begin
    raise ENyxCollection.Create('Construct a typed boolean field reference');
  end;
  Result := FName;
end;

function NyxIntegerField(const AName: TNyxText): TNyxIntegerFieldRef;
begin
  NameBytes(AName, True);
  Result.FName := AName;
  Result.FInitialized := True;
end;

function TNyxIntegerFieldRef.GetName: TNyxText;
begin

  if not FInitialized then
  begin
    raise ENyxCollection.Create('Construct a typed integer field reference');
  end;
  Result := FName;
end;

function NyxNumberField(const AName: TNyxText): TNyxNumberFieldRef;
begin
  NameBytes(AName, True);
  Result.FName := AName;
  Result.FInitialized := True;
end;

function TNyxNumberFieldRef.GetName: TNyxText;
begin

  if not FInitialized then
  begin
    raise ENyxCollection.Create('Construct a typed number field reference');
  end;
  Result := FName;
end;

function ScalarData(const AValue: TNyxStateValue): TNyxDataValue;
begin
  case AValue.Kind of
    nskText: Result := NyxData(AValue.TextValue);
    nskBoolean: Result := NyxData(AValue.BooleanValue);
    nskInteger: Result := NyxData(AValue.IntegerValue);
    nskNumber: Result := NyxData(AValue.NumberValue);
  end;
end;

function ScalarBytes(const AValue: TNyxStateValue): Integer;
begin
  case AValue.Kind of
    nskText: Result := NameBytes(AValue.TextValue, False);
    nskBoolean: Result := 1;
    nskInteger: Result := 4;
    nskNumber: Result := 8;
  end;
end;

function TNyxCollectionField.GetDefault: TNyxStateValue;
begin
  Result := FDefault.Copy;
end;

function TNyxCollectionField.GetDomain: TNyxValueDomain;
begin
  Result := FDomain.Copy;
end;

function TNyxCollectionField.GetKind: TNyxStateKind;
begin
  Result := FDefault.Kind;
end;

function NyxCollectionSchema: TNyxCollectionSchema;
begin
  Result.FFields := nil;
  Result.FInitialized := True;
  Result.FMetadataBytes := 0;
end;

function TNyxCollectionSchema.GetCount: Integer;
begin

  if not FInitialized then
  begin
    raise ENyxCollection.Create('Construct a collection schema');
  end;
  Result := Length(FFields);
end;

function TNyxCollectionSchema.GetDataBytes: Integer;
begin
  GetCount;
  Result := FMetadataBytes;
end;

function TNyxCollectionSchema.IndexOf(const AName: TNyxText): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Count - 1 do
  begin

    if FFields[LIndex].FName = AName then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TNyxCollectionSchema.Field(const AName: TNyxText;
  const ADefault: TNyxStateValue; const ADomain: TNyxValueDomain): TNyxCollectionSchema;
begin
  NameBytes(AName, True);
  Result := AddField(AName, ADefault, ADomain);
end;

function TNyxCollectionSchema.FieldAt(AIndex: Integer): TNyxCollectionField;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxCollection.Create('Collection field index is outside its range');
  end;
  Result.FName := FFields[AIndex].FName;
  Result.FDefault := FFields[AIndex].FDefault.Copy;
  Result.FDomain := FFields[AIndex].FDomain.Copy;
  Result.FDefaultBytes := FFields[AIndex].FDefaultBytes;
end;

function TNyxCollectionSchema.Copy: TNyxCollectionSchema;
var
  LIndex: Integer;
begin
  Result.FInitialized := True;
  Result.FMetadataBytes := FMetadataBytes;
  SetLength(Result.FFields, Count);
  for LIndex := 0 to Count - 1 do
  begin
    Result.FFields[LIndex] := FieldAt(LIndex);
  end;
end;

procedure TNyxCollectionSchema.Validate;
var
  LIndex: Integer;
  LOther: Integer;
begin

  if Count > NyxMaximumCollectionFields then
  begin
    raise ENyxCollection.Create('Collection schema exceeds its 64-field budget');
  end;
  for LIndex := 0 to Count - 1 do
  begin
    NameBytes(FFields[LIndex].FName, True);
    FFields[LIndex].FDefault.Validate;
    FFields[LIndex].FDomain.Validate;

    if FFields[LIndex].FDomain.Defined and
      (FFields[LIndex].FDomain.Kind <> FFields[LIndex].FDefault.Kind) then
    begin
      raise ENyxCollection.Create('Collection domain/default families must agree');
    end;

    if FFields[LIndex].FDomain.Defined then
    begin
      FFields[LIndex].FDomain.Admit(ScalarData(FFields[LIndex].FDefault));
    end;
    for LOther := 0 to LIndex - 1 do
    begin

      if FFields[LIndex].FName = FFields[LOther].FName then
      begin
        raise ENyxCollection.Create('Duplicate collection field: ' + FFields[LIndex].FName);
      end;
    end;
  end;
end;

function TNyxCollectionSchema.AddField(const AName: TNyxText;
  const ADefault: TNyxStateValue; const ADomain: TNyxValueDomain): TNyxCollectionSchema;
var
  LIndex: Integer;
  LBytes: Integer;
begin
  NameBytes(AName, True);

  if (Count >= NyxMaximumCollectionFields) or (IndexOf(AName) >= 0) then
  begin
    raise ENyxCollection.Create('Duplicate or excessive collection field: ' + AName);
  end;
  ADefault.Validate;
  ADomain.Validate;
  { A descriptor may explicitly omit an additional domain. Its default still
    declares and validates the scalar family; typed authoring factories supply
    their unconstrained family domain automatically. }

  if ADomain.Defined then
  begin
    ADomain.Admit(ScalarData(ADefault));
  end;
  LBytes := NameBytes(AName, True) + ScalarBytes(ADefault) +
    NameBytes(ADomain.ToData.ToJSON, False);

  if LBytes > NyxMaximumCollectionBytes - FMetadataBytes then
  begin
    raise ENyxCollection.Create('Collection schema exceeds its 8 MiB metadata budget');
  end;
  Result := Copy;
  Inc(Result.FMetadataBytes, LBytes);
  LIndex := Result.Count;
  SetLength(Result.FFields, LIndex + 1);
  Result.FFields[LIndex].FName := AName;
  Result.FFields[LIndex].FDefault := ADefault.Copy;
  Result.FFields[LIndex].FDomain := ADomain.Copy;
  Result.FFields[LIndex].FDefaultBytes := ScalarBytes(ADefault);
end;


function TNyxCollectionSchema.Text(const AField: TNyxTextFieldRef;
  const ADefault: TNyxText): TNyxCollectionSchema;
begin
  Result := Text(AField, ADefault, NyxTextDomain);
end;

function TNyxCollectionSchema.Text(const AField: TNyxTextFieldRef;
  const ADefault: TNyxText; const ADomain: TNyxTextDomain): TNyxCollectionSchema;
begin
  Result := AddField(AField.Name, TNyxStateValue.FromText(ADefault), ADomain.Definition);
end;

function TNyxCollectionSchema.Boolean(const AField: TNyxBooleanFieldRef;
  const ADefault: Boolean): TNyxCollectionSchema;
begin
  Result := Boolean(AField, ADefault, NyxBooleanDomain);
end;

function TNyxCollectionSchema.Boolean(const AField: TNyxBooleanFieldRef;
  const ADefault: Boolean; const ADomain: TNyxBooleanDomain): TNyxCollectionSchema;
begin
  Result := AddField(AField.Name, TNyxStateValue.FromBoolean(ADefault), ADomain.Definition);
end;

function TNyxCollectionSchema.Integer(const AField: TNyxIntegerFieldRef;
  const ADefault: Integer): TNyxCollectionSchema;
begin
  Result := Integer(AField, ADefault, NyxIntegerDomain);
end;

function TNyxCollectionSchema.Integer(const AField: TNyxIntegerFieldRef;
  const ADefault: Integer; const ADomain: TNyxIntegerDomain): TNyxCollectionSchema;
begin
  Result := AddField(AField.Name, TNyxStateValue.FromInteger(ADefault), ADomain.Definition);
end;

function TNyxCollectionSchema.Number(const AField: TNyxNumberFieldRef;
  const ADefault: Double): TNyxCollectionSchema;
begin
  Result := Number(AField, ADefault, NyxNumberDomain);
end;

function TNyxCollectionSchema.Number(const AField: TNyxNumberFieldRef;
  const ADefault: Double; const ADomain: TNyxNumberDomain): TNyxCollectionSchema;
begin
  Result := AddField(AField.Name, TNyxStateValue.FromNumber(ADefault), ADomain.Definition);
end;

function TNyxCollectionSchema.SameSchema(const AOther: TNyxCollectionSchema): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if Count <> AOther.Count then
  begin
    Exit;
  end;
  for LIndex := 0 to Count - 1 do
  begin

    if (FFields[LIndex].FName <> AOther.FFields[LIndex].FName) or
      not FFields[LIndex].FDefault.SameValue(AOther.FFields[LIndex].FDefault) or
      (FFields[LIndex].FDomain.ToData.ToJSON <> AOther.FFields[LIndex].FDomain.ToData.ToJSON) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function NyxCollectionItem(const ARef: TNyxItemRef): TNyxCollectionItem;
begin
  Result.FRef := NyxItem(ARef.Collection, ARef.ID);
  Result.FNames := nil;
  Result.FValues := nil;
  Result.FValueBytes := nil;
  Result.FPayloadBytes := -1;
end;

function TNyxCollectionItem.GetRef: TNyxItemRef;
begin
  Result := NyxItem(FRef.Collection, FRef.ID);
end;

function TNyxCollectionItem.GetCount: Integer;
begin
  FRef.GetID;
  Result := Length(FNames);
end;

function TNyxCollectionItem.IndexOf(const AName: TNyxText): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Count - 1 do
  begin

    if FNames[LIndex] = AName then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TNyxCollectionItem.FieldName(AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxCollection.Create('Item field index is outside its range');
  end;
  Result := FNames[AIndex];
end;

function TNyxCollectionItem.FieldValue(AIndex: Integer): TNyxStateValue;
begin
  FieldName(AIndex);
  Result := FValues[AIndex].Copy;
end;

function TNyxCollectionItem.Copy: TNyxCollectionItem;
var
  LIndex: Integer;
begin
  Result.FRef := GetRef;
  Result.FNames := System.Copy(FNames, 0, Count);
  Result.FValueBytes := System.Copy(FValueBytes, 0, Count);
  Result.FPayloadBytes := FPayloadBytes;
  SetLength(Result.FValues, Count);
  for LIndex := 0 to Count - 1 do
  begin
    Result.FValues[LIndex] := FValues[LIndex].Copy;
  end;
end;

function TNyxCollectionItem.Put(const AName: TNyxText;
  const AValue: TNyxStateValue): TNyxCollectionItem;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AName);

  if (LIndex >= 0) and (FValues[LIndex].Kind <> AValue.Kind) then
  begin
    raise ENyxCollection.Create('An item field cannot change scalar family');
  end;

  if (LIndex < 0) and (Count >= NyxMaximumCollectionFields) then
  begin
    raise ENyxCollection.Create('Item exceeds its 64-field budget');
  end;
  Result := Copy;

  if LIndex < 0 then
  begin
    LIndex := Result.Count;
    SetLength(Result.FNames, LIndex + 1);
    SetLength(Result.FValues, LIndex + 1);
    SetLength(Result.FValueBytes, LIndex + 1);
    Result.FNames[LIndex] := AName;
  end;
  Result.FValues[LIndex] := AValue.Copy;
  Result.FValueBytes[LIndex] := ScalarBytes(AValue);
  Result.FPayloadBytes := -1;
end;

function TNyxCollectionItem.ReadValue(const AName: TNyxText;
  AKind: TNyxStateKind): TNyxStateValue;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AName);

  if (LIndex < 0) or (FValues[LIndex].Kind <> AKind) then
  begin
    raise ENyxCollection.Create('Missing field or different scalar family: ' + AName);
  end;
  Result := FValues[LIndex].Copy;
end;


function TNyxCollectionItem.WithValue(const AField: TNyxTextFieldRef;
  const AValue: TNyxText): TNyxCollectionItem;
begin
  Result := Put(AField.Name, TNyxStateValue.FromText(AValue));
end;

function TNyxCollectionItem.GetValue(const AField: TNyxTextFieldRef): TNyxText;
begin
  Result := ReadValue(AField.Name, nskText).TextValue;
end;

function TNyxCollectionItem.Has(const AField: TNyxTextFieldRef): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AField.Name);
  Result := LIndex >= 0;

  if Result and (FValues[LIndex].Kind <> nskText) then
  begin
    raise ENyxCollection.Create('Collection field family differs: ' + AField.Name);
  end;
end;

function TNyxCollectionItem.WithValue(const AField: TNyxBooleanFieldRef;
  const AValue: Boolean): TNyxCollectionItem;
begin
  Result := Put(AField.Name, TNyxStateValue.FromBoolean(AValue));
end;

function TNyxCollectionItem.GetValue(const AField: TNyxBooleanFieldRef): Boolean;
begin
  Result := ReadValue(AField.Name, nskBoolean).BooleanValue;
end;

function TNyxCollectionItem.Has(const AField: TNyxBooleanFieldRef): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AField.Name);
  Result := LIndex >= 0;

  if Result and (FValues[LIndex].Kind <> nskBoolean) then
  begin
    raise ENyxCollection.Create('Collection field family differs: ' + AField.Name);
  end;
end;

function TNyxCollectionItem.WithValue(const AField: TNyxIntegerFieldRef;
  const AValue: Integer): TNyxCollectionItem;
begin
  Result := Put(AField.Name, TNyxStateValue.FromInteger(AValue));
end;

function TNyxCollectionItem.GetValue(const AField: TNyxIntegerFieldRef): Integer;
begin
  Result := ReadValue(AField.Name, nskInteger).IntegerValue;
end;

function TNyxCollectionItem.Has(const AField: TNyxIntegerFieldRef): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AField.Name);
  Result := LIndex >= 0;

  if Result and (FValues[LIndex].Kind <> nskInteger) then
  begin
    raise ENyxCollection.Create('Collection field family differs: ' + AField.Name);
  end;
end;

function TNyxCollectionItem.WithValue(const AField: TNyxNumberFieldRef;
  const AValue: Double): TNyxCollectionItem;
begin
  Result := Put(AField.Name, TNyxStateValue.FromNumber(AValue));
end;

function TNyxCollectionItem.GetValue(const AField: TNyxNumberFieldRef): Double;
begin
  Result := ReadValue(AField.Name, nskNumber).NumberValue;
end;

function TNyxCollectionItem.Has(const AField: TNyxNumberFieldRef): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AField.Name);
  Result := LIndex >= 0;

  if Result and (FValues[LIndex].Kind <> nskNumber) then
  begin
    raise ENyxCollection.Create('Collection field family differs: ' + AField.Name);
  end;
end;

function TNyxCollectionItem.SameItem(const AOther: TNyxCollectionItem): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if (GetRef.ID <> AOther.GetRef.ID) or
    (FRef.FCollection.GetName <> AOther.FRef.FCollection.GetName) or (Count <> AOther.Count) then
  begin
    Exit;
  end;
  for LIndex := 0 to Count - 1 do
  begin

    if (FNames[LIndex] <> AOther.FNames[LIndex]) or
      not FValues[LIndex].SameValue(AOther.FValues[LIndex]) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function MakeEdit(AKind: TNyxCollectionEditKind;
  const ARef: TNyxItemRef; const AItem: TNyxCollectionItem; AIndex: Integer): TNyxCollectionEdit;
begin
  Result.FKind := AKind;
  Result.FRef := NyxItem(ARef.Collection, ARef.ID);
  Result.FItem := AItem;
  Result.FIndex := AIndex;
  Result.FInitialized := True;
end;

function NyxInsert(AIndex: Integer; const AItem: TNyxCollectionItem): TNyxCollectionEdit;
begin
  Result := MakeEdit(nceInsert, AItem.Ref, AItem.Copy, AIndex);
end;

function NyxRemove(const ARef: TNyxItemRef): TNyxCollectionEdit;
var
  LEmpty: TNyxCollectionItem;
begin
  LEmpty := NyxCollectionItem(ARef);
  Result := MakeEdit(nceRemove, ARef, LEmpty, -1);
end;

function NyxMove(const ARef: TNyxItemRef; AIndex: Integer): TNyxCollectionEdit;
var
  LEmpty: TNyxCollectionItem;
begin
  LEmpty := NyxCollectionItem(ARef);
  Result := MakeEdit(nceMove, ARef, LEmpty, AIndex);
end;

function NyxUpdate(const AItem: TNyxCollectionItem): TNyxCollectionEdit;
begin
  Result := MakeEdit(nceUpdate, AItem.Ref, AItem.Copy, -1);
end;

function NyxReplace(const AItem: TNyxCollectionItem): TNyxCollectionEdit;
begin
  Result := MakeEdit(nceReplace, AItem.Ref, AItem.Copy, -1);
end;

constructor TCollectionSnapshot.Create(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema; const AItems: TCollectionItems;
  ARevision, ABytes: Integer);
var
  LSize: Integer;
  LIndex: Integer;
  LBucket: Integer;
begin
  inherited Create;
  FKey := NyxCollection(AKey.Name);
  FSchema := ASchema.Copy;
  { Copy only the outer vector. Every shared item is private/immutable; getters
    copy row records and builders always fork their field arrays. }
  FItems := Copy(AItems, 0, Length(AItems));
  FRevision := ARevision;
  FBytes := ABytes;
  LSize := 16;
  while LSize < Length(FItems) * 2 do
  begin
    LSize := LSize * 2;
  end;
  SetLength(FIndex, LSize);
  for LIndex := 0 to LSize - 1 do
  begin
    FIndex[LIndex] := -1;
  end;
  for LIndex := 0 to Length(FItems) - 1 do
  begin
    LBucket := Bucket(FItems[LIndex].FRef.FID);
    while FIndex[LBucket] >= 0 do
    begin

      if FItems[FIndex[LBucket]].FRef.FID = FItems[LIndex].FRef.FID then
      begin
        raise ENyxCollection.Create('Duplicate collection item identity');
      end;
      LBucket := (LBucket + 1) mod LSize;
    end;
    FIndex[LBucket] := LIndex;
  end;
end;

function TCollectionSnapshot.Bucket(const AID: TNyxText): Integer;
var
  LIndex: Integer;
  LScalar: Integer;
begin
  { Hash Unicode scalars on both targets. The bounded multiplier never overflows
    a checked 32-bit Integer and avoids JavaScript bitwise/sign differences. }
  Result := 0;
  LIndex := 1;
  while LIndex <= Length(AID) do
  begin

    if not NyxNextScalar(AID, LIndex, LScalar) then
    begin
      raise ENyxCollection.Create('Malformed item identity');
    end;
    Result := ((Result * 31) + LScalar) mod Length(FIndex);
  end;
end;

function TCollectionSnapshot.GetKey: TNyxCollectionRef;
begin
  Result := NyxCollection(FKey.Name);
end;

function TCollectionSnapshot.GetSchema: TNyxCollectionSchema;
begin
  Result := FSchema.Copy;
end;

function TCollectionSnapshot.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TCollectionSnapshot.GetRevision: Integer;
begin
  Result := FRevision;
end;

function TCollectionSnapshot.GetDataBytes: Integer;
begin
  Result := FBytes;
end;

function TCollectionSnapshot.ItemAt(AIndex: Integer): TNyxCollectionItem;
begin

  if (AIndex < 0) or (AIndex >= GetCount) then
  begin
    raise ENyxCollection.Create('Collection item index is outside its range');
  end;
  Result := FItems[AIndex].Copy;
end;

function TCollectionSnapshot.IndexOf(const ARef: TNyxItemRef): Integer;
var
  LBucket: Integer;
begin
  RequireItem(FKey, ARef);
  LBucket := Bucket(ARef.ID);
  while FIndex[LBucket] >= 0 do
  begin
    Result := FIndex[LBucket];

    if FItems[Result].FRef.FID = ARef.FID then
    begin
      Exit;
    end;
    LBucket := (LBucket + 1) mod Length(FIndex);
  end;
  Result := -1;
end;

function TCollectionSnapshot.Has(const ARef: TNyxItemRef): Boolean;
begin
  Result := IndexOf(ARef) >= 0;
end;

function TCollectionSnapshot.Item(const ARef: TNyxItemRef): TNyxCollectionItem;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ARef);

  if LIndex < 0 then
  begin
    raise ENyxCollection.Create('Collection item is missing: ' + ARef.ID);
  end;
  Result := ItemAt(LIndex);
end;

constructor TCollectionChanges.Create(const ABefore, AAfter: INyxCollectionSnapshot;
  const ASteps: TCollectionSteps);
begin
  inherited Create;
  FBefore := ABefore;
  FAfter := AAfter;
  FSteps := Copy(ASteps, 0, Length(ASteps));
end;

procedure TCollectionChanges.CheckIndex(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= Length(FSteps)) then
  begin
    raise ENyxCollection.Create('Collection change index is outside its range');
  end;
end;

function TCollectionChanges.GetCount: Integer;
begin
  Result := Length(FSteps);
end;

function TCollectionChanges.GetBefore: INyxCollectionSnapshot;
begin
  Result := FBefore;
end;

function TCollectionChanges.GetAfter: INyxCollectionSnapshot;
begin
  Result := FAfter;
end;

function TCollectionChanges.Kind(AIndex: Integer): TNyxCollectionEditKind;
begin
  CheckIndex(AIndex);
  Result := FSteps[AIndex].Kind;
end;

function TCollectionChanges.ItemRef(AIndex: Integer): TNyxItemRef;
begin
  CheckIndex(AIndex);
  Result := NyxItem(FSteps[AIndex].Ref.Collection, FSteps[AIndex].Ref.ID);
end;

function TCollectionChanges.BeforeIndex(AIndex: Integer): Integer;
begin
  CheckIndex(AIndex);
  Result := FSteps[AIndex].BeforeIndex;
end;

function TCollectionChanges.AfterIndex(AIndex: Integer): Integer;
begin
  CheckIndex(AIndex);
  Result := FSteps[AIndex].AfterIndex;
end;

function TCollectionChanges.BeforeItem(AIndex: Integer): TNyxCollectionItem;
begin
  CheckIndex(AIndex);

  if FSteps[AIndex].BeforeIndex < 0 then
  begin
    raise ENyxCollection.Create('Inserted item has no before value');
  end;
  Result := FSteps[AIndex].BeforeItem.Copy;
end;

function TCollectionChanges.AfterItem(AIndex: Integer): TNyxCollectionItem;
begin
  CheckIndex(AIndex);

  if FSteps[AIndex].AfterIndex < 0 then
  begin
    raise ENyxCollection.Create('Removed item has no after value');
  end;
  Result := FSteps[AIndex].AfterItem.Copy;
end;

constructor TCollectionStore.Create(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema; const AItems: array of TNyxCollectionItem);
var
  LIndex: Integer;
begin
  inherited Create;
  FKey := NyxCollection(AKey.Name);
  ASchema.Validate;
  FSchema := ASchema.Copy;

  if Length(AItems) > NyxMaximumCollectionItems then
  begin
    raise ENyxCollection.Create('Collection defaults exceed the item budget');
  end;
  SetLength(FItems, Length(AItems));
  for LIndex := 0 to High(AItems) do
  begin
    FItems[LIndex] := Normalize(AItems[LIndex], AItems[LIndex], False);
  end;
  FCurrent := TCollectionSnapshot.Create(FKey, FSchema, FItems, 0, DatasetBytes(FItems));
end;

function NewNyxCollection(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema): INyxCollection;
begin
  Result := TCollectionStore.Create(AKey, ASchema, []);
end;

function NewNyxCollection(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema;
  const AItems: array of TNyxCollectionItem): INyxCollection;
begin
  Result := TCollectionStore.Create(AKey, ASchema, AItems);
end;

function NewNyxCollection(const ADefaults: INyxCollectionSnapshot): INyxCollection;
var
  LCount: Integer;
  LIndex: Integer;
  LItems: TCollectionItems;
begin

  if ADefaults = nil then
  begin
    raise ENyxCollection.Create('Collection defaults are required');
  end;
  LCount := ADefaults.Count;

  if (LCount < 0) or (LCount > NyxMaximumCollectionItems) then
  begin
    raise ENyxCollection.Create('Invalid collection defaults item count');
  end;
  SetLength(LItems, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LItems[LIndex] := ADefaults.ItemAt(LIndex);
  end;
  Result := TCollectionStore.Create(ADefaults.Key, ADefaults.Schema, LItems);
end;

destructor TCollectionStore.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(FSubscriptions) - 1 do
  begin
    FSubscriptions[LIndex].Owner := nil;
  end;
  FSubscriptions := nil;
  FCurrent := nil;
  inherited Destroy;
end;

function TCollectionStore.Snapshot: INyxCollectionSnapshot;
begin
  Result := FCurrent;
end;

procedure TCollectionStore.CheckWritable(AExpectedRevision: Integer);
begin

  if (AExpectedRevision < -1) or
    ((AExpectedRevision >= 0) and (AExpectedRevision <> FCurrent.Revision)) then
  begin
    raise ENyxCollection.Create('Collection update uses a stale or invalid revision');
  end;

  if FBusy then
  begin
    raise ENyxCollection.Create('Collection callbacks cannot mutate or add subscriptions');
  end;
end;

function TCollectionStore.Normalize(const AItem, ABase: TNyxCollectionItem;
  AUpdate: Boolean): TNyxCollectionItem;
var
  LIndex: Integer;
  LField: Integer;
  LValue: TNyxStateValue;
begin
  RequireItem(FKey, AItem.Ref);
  for LIndex := 0 to AItem.Count - 1 do
  begin
    LField := FSchema.IndexOf(AItem.FNames[LIndex]);

    if (LField < 0) or (FSchema.FFields[LField].Kind <> AItem.FValues[LIndex].Kind) then
    begin
      raise ENyxCollection.Create('Unknown field or different schema family: ' + AItem.FNames[LIndex]);
    end;
  end;
  Result := NyxCollectionItem(AItem.Ref);
  SetLength(Result.FNames, FSchema.Count);
  SetLength(Result.FValues, FSchema.Count);
  SetLength(Result.FValueBytes, FSchema.Count);
  Result.FPayloadBytes := NameBytes(AItem.Ref.ID, True);
  for LField := 0 to FSchema.Count - 1 do
  begin
    Result.FNames[LField] := FSchema.FFields[LField].FName;
    LIndex := AItem.IndexOf(Result.FNames[LField]);

    if LIndex >= 0 then
    begin
      LValue := AItem.FValues[LIndex].Copy;
      Result.FValueBytes[LField] := AItem.FValueBytes[LIndex];

      if FSchema.FFields[LField].FDomain.Defined then
      begin
        FSchema.FFields[LField].FDomain.Admit(ScalarData(LValue));
      end;
    end
    else if AUpdate then
    begin
      LValue := ABase.FValues[LField].Copy;
      Result.FValueBytes[LField] := ABase.FValueBytes[LField];
    end
    else
    begin
      LValue := FSchema.FFields[LField].FDefault.Copy;
      Result.FValueBytes[LField] := FSchema.FFields[LField].FDefaultBytes;
    end;
    Result.FValues[LField] := LValue.Copy;
    Inc(Result.FPayloadBytes, Result.FValueBytes[LField]);

    if Result.FPayloadBytes > NyxMaximumCollectionBytes then
    begin
      raise ENyxCollection.Create('Collection item exceeds its payload budget');
    end;
  end;
end;

function TCollectionStore.DatasetBytes(const AItems: TCollectionItems): Integer;
var
  LItem: Integer;
begin
  Result := NameBytes(FKey.Name, True);
  Inc(Result, FSchema.DataBytes);

  if Result > NyxMaximumCollectionBytes then
  begin
    raise ENyxCollection.Create('Collection key/schema exceeds its payload budget');
  end;

  if Length(AItems) > NyxMaximumCollectionItems then
  begin
    raise ENyxCollection.Create('Collection exceeds its 16384-item budget');
  end;
  for LItem := 0 to Length(AItems) - 1 do
  begin
    { A row's private payload count is calculated on normalization from cached
      immutable scalar sizes. Updating one cell does not rescan every large
      text value in an otherwise unchanged dataset. }

    if AItems[LItem].FPayloadBytes < 0 then
    begin
      raise ENyxCollection.Create('Normalize collection items before publishing');
    end;
    Inc(Result, AItems[LItem].FPayloadBytes);

    if Result > NyxMaximumCollectionBytes then
    begin
      raise ENyxCollection.Create('Collection exceeds its 8 MiB payload budget');
    end;
  end;
end;

destructor TCollectionSubscription.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TCollectionSubscription.GetConnected: Boolean;
begin
  Result := Owner <> nil;
end;

procedure TCollectionSubscription.Disconnect;
begin

  if Owner <> nil then
  begin
    Owner.Disconnect(Self);
  end;
end;

procedure TCollectionStore.Disconnect(AToken: TCollectionSubscription);
var
  LIndex: Integer;
  LNext: Integer;
begin
  for LIndex := 0 to Length(FSubscriptions) - 1 do
  begin

    if FSubscriptions[LIndex] = AToken then
    begin
      AToken.Owner := nil;
      for LNext := LIndex to Length(FSubscriptions) - 2 do
      begin
        FSubscriptions[LNext] := FSubscriptions[LNext + 1];
      end;
      SetLength(FSubscriptions, Length(FSubscriptions) - 1);
      Exit;
    end;
  end;
end;

function TCollectionStore.Connection(ASerial: Integer): TCollectionSubscription;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to Length(FSubscriptions) - 1 do
  begin

    if FSubscriptions[LIndex].Serial = ASerial then
    begin
      Exit(FSubscriptions[LIndex]);
    end;
  end;
end;

function TCollectionStore.Subscribe(AObserver: TNyxCollectionObserver;
  AValidator: TNyxCollectionValidator): INyxCollectionSubscription;
var
  LToken: TCollectionSubscription;
begin
  CheckWritable(-1);

  if not Assigned(AObserver) and not Assigned(AValidator) then
  begin
    raise ENyxCollection.Create('A collection subscription requires a callback');
  end;

  if (Length(FSubscriptions) >= NyxMaximumCollectionSubscriptions) or (FSerial = High(Integer)) then
  begin
    raise ENyxCollection.Create('Collection subscription budget is exhausted');
  end;
  LToken := TCollectionSubscription.Create;
  { Retain before array allocation: even an allocation failure releases the token
    safely. Store registration follows initialization; no token owns its store. }
  Result := LToken;
  Inc(FSerial);
  LToken.Serial := FSerial;
  LToken.Validator := AValidator;
  LToken.Observer := AObserver;
  SetLength(FSubscriptions, Length(FSubscriptions) + 1);
  FSubscriptions[High(FSubscriptions)] := LToken;
  LToken.Owner := Self;
end;

function ExceptionText(AException: Exception): TNyxText;
begin
  {$IFDEF PAS2JS}
  Result := AException.Message;
  {$ELSE}

  if AException is ENyxState then
  begin
    { Nyx state errors explicitly put UTF-8 bytes into the older ANSI RTL field.
      Label them without conversion; ordinary foreign exceptions use their RTL
      codepage and retain normal conversion into the portable text contract. }
    Result := RawByteString(AException.Message);
    SetCodePage(RawByteString(Result), CP_UTF8, False);
  end
  else
  begin
    Result := TNyxText(AException.Message);
  end;
  {$ENDIF}
end;

procedure TCollectionStore.Publish(const AItems: TCollectionItems;
  const ASteps: TCollectionSteps);
var
  LAfter: INyxCollectionSnapshot;
  LChanges: INyxCollectionChanges;
  LSerials: array of Integer;
  LIndex: Integer;
  LToken: TCollectionSubscription;
  LError: TNyxText;
  LFailed: Boolean;
begin

  if FCurrent.Revision = High(Integer) then
  begin
    raise ENyxCollection.Create('Collection revision budget is exhausted');
  end;
  LAfter := TCollectionSnapshot.Create(FKey, FSchema, AItems,
    FCurrent.Revision + 1, DatasetBytes(AItems));
  LChanges := TCollectionChanges.Create(FCurrent, LAfter, ASteps);
  SetLength(LSerials, Length(FSubscriptions));
  for LIndex := 0 to Length(FSubscriptions) - 1 do
  begin
    LSerials[LIndex] := FSubscriptions[LIndex].Serial;
  end;
  FBusy := True;
  try
    { Serial snapshots survive receivers releasing their own/other managed token
      during a callback. Never retain a dangling concrete token for the next call. }
    for LIndex := 0 to Length(LSerials) - 1 do
    begin
      LToken := Connection(LSerials[LIndex]);

      if (LToken <> nil) and Assigned(LToken.Validator) then
      begin
        LToken.Validator(LAfter, LChanges);
      end;
    end;
    FItems := Copy(AItems, 0, Length(AItems));
    FCurrent := LAfter;
    LFailed := False;
    LError := '';
    for LIndex := 0 to Length(LSerials) - 1 do
    begin
      LToken := Connection(LSerials[LIndex]);

      if (LToken <> nil) and Assigned(LToken.Observer) then
      begin
        try
          LToken.Observer(Self, LChanges);
        except
          on LException: Exception do
          begin

            if not LFailed then
            begin
              LFailed := True;
              LError := ExceptionText(LException);
            end;
          end;
        end;
      end;
    end;

    if LFailed then
    begin
      raise ENyxCollectionNotification.Create('Collection committed; observer failed: ' + LError);
    end;
  finally
    FBusy := False;
  end;
end;

function ItemIndex(const AItems: TCollectionItems; const ARef: TNyxItemRef): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(AItems) - 1 do
  begin

    if AItems[LIndex].FRef.FID = ARef.FID then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

procedure TCollectionStore.Apply(const AEdits: array of TNyxCollectionEdit;
  AExpectedRevision: Integer);
var
  LItems: TCollectionItems;
  LSteps: TCollectionSteps;
  LEdit: Integer;
  LIndex: Integer;
  LNext: Integer;
  LStep: Integer;
  LTarget: Integer;
  LItem: TNyxCollectionItem;
  LSame: Boolean;
  LKeepAlive: INyxCollection;
begin
  CheckWritable(AExpectedRevision);

  if Length(AEdits) > NyxMaximumCollectionEdits then
  begin
    raise ENyxCollection.Create('Collection Apply exceeds its 256-edit budget');
  end;
  { Hold this implementation throughout callbacks even if a receiver releases
    the application's last store reference. Data/changes do not retain the store. }
  LKeepAlive := Self;
  LItems := Copy(FItems, 0, Length(FItems));
  LSteps := nil;
  for LEdit := 0 to Length(AEdits) - 1 do
  begin

    if not AEdits[LEdit].FInitialized then
    begin
      raise ENyxCollection.Create('Construct a typed collection edit');
    end;
    RequireItem(FKey, AEdits[LEdit].FRef);
    LIndex := ItemIndex(LItems, AEdits[LEdit].FRef);
    LTarget := AEdits[LEdit].FIndex;

    if AEdits[LEdit].FKind = nceInsert then
    begin

      if (LIndex >= 0) or (LTarget < 0) or (LTarget > Length(LItems)) or
        (Length(LItems) >= NyxMaximumCollectionItems) then
      begin
        raise ENyxCollection.Create('Duplicate item or invalid insertion index/item budget');
      end;
      LItem := Normalize(AEdits[LEdit].FItem, AEdits[LEdit].FItem, False);
      LStep := Length(LSteps);
      SetLength(LSteps, LStep + 1);
      LSteps[LStep].BeforeIndex := -1;
      LSteps[LStep].AfterIndex := LTarget;
      LSteps[LStep].AfterItem := LItem;
      SetLength(LItems, Length(LItems) + 1);
      for LNext := High(LItems) downto LTarget + 1 do
      begin
        LItems[LNext] := LItems[LNext - 1];
      end;
      LItems[LTarget] := LItem;
    end
    else
    begin

      if LIndex < 0 then
      begin
        raise ENyxCollection.Create('Collection edit refers to a missing item: ' + AEdits[LEdit].FRef.ID);
      end;

      if AEdits[LEdit].FKind in [nceUpdate, nceReplace] then
      begin
        LItem := Normalize(AEdits[LEdit].FItem, LItems[LIndex],
          AEdits[LEdit].FKind = nceUpdate);

        if LItems[LIndex].SameItem(LItem) then
        begin
          Continue;
        end;
      end
      else
      begin
        LItem := LItems[LIndex];
      end;

      if AEdits[LEdit].FKind = nceMove then
      begin

        if (LTarget < 0) or (LTarget >= Length(LItems)) then
        begin
          raise ENyxCollection.Create('Move index is outside its final range');
        end;

        if LIndex = LTarget then
        begin
          Continue;
        end;
      end;
      LStep := Length(LSteps);
      SetLength(LSteps, LStep + 1);
      LSteps[LStep].BeforeIndex := LIndex;
      LSteps[LStep].BeforeItem := LItems[LIndex];
      LSteps[LStep].AfterIndex := LIndex;
      LSteps[LStep].AfterItem := LItem;
      case AEdits[LEdit].FKind of
        nceRemove:
          begin
            LSteps[LStep].AfterIndex := -1;
            for LNext := LIndex to High(LItems) - 1 do
            begin
              LItems[LNext] := LItems[LNext + 1];
            end;
            SetLength(LItems, Length(LItems) - 1);
          end;
        nceMove:
          begin
            LSteps[LStep].AfterIndex := LTarget;

            if LIndex < LTarget then
            begin
              for LNext := LIndex to LTarget - 1 do
              begin
                LItems[LNext] := LItems[LNext + 1];
              end;
            end
            else
            begin
              for LNext := LIndex downto LTarget + 1 do
              begin
                LItems[LNext] := LItems[LNext - 1];
              end;
            end;
            LItems[LTarget] := LItem;
          end;
        nceUpdate, nceReplace: LItems[LIndex] := LItem;
      end;
    end;
    LSteps[LStep].Kind := AEdits[LEdit].FKind;
    LSteps[LStep].Ref := AEdits[LEdit].FRef;
  end;

  if Length(LSteps) = 0 then
  begin
    Exit;
  end;
  { A batch may insert/remove or move away/back. Suppress the complete net no-op,
    keeping selection snapshots/revision stable rather than notifying churn. }
  LSame := Length(LItems) = Length(FItems);

  if LSame then
  begin
    for LIndex := 0 to Length(LItems) - 1 do
    begin

      if not LItems[LIndex].SameItem(FItems[LIndex]) then
      begin
        LSame := False;
        Break;
      end;
    end;
  end;

  if not LSame then
  begin
    Publish(LItems, LSteps);
  end;
end;

function TCollectionStore.Append(const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := Self;
  Apply([NyxInsert(Length(FItems), AItem)]);
end;

function TCollectionStore.Insert(AIndex: Integer; const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := Self;
  Apply([NyxInsert(AIndex, AItem)]);
end;

function TCollectionStore.Remove(const ARef: TNyxItemRef): INyxCollection;
begin
  Result := Self;
  Apply([NyxRemove(ARef)]);
end;

function TCollectionStore.Move(const ARef: TNyxItemRef; AIndex: Integer): INyxCollection;
begin
  Result := Self;
  Apply([NyxMove(ARef, AIndex)]);
end;

function TCollectionStore.Update(const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := Self;
  Apply([NyxUpdate(AItem)]);
end;

function TCollectionStore.Replace(const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := Self;
  Apply([NyxReplace(AItem)]);
end;

procedure TCollectionStore.Assign(const ASource: INyxCollectionSnapshot; AExpectedRevision: Integer);
var
  LItems: TCollectionItems;
  LSteps: TCollectionSteps;
  LIndex: Integer;
  LStep: Integer;
  LSame: Boolean;
  LKeepAlive: INyxCollection;
begin
  CheckWritable(AExpectedRevision);

  if (ASource = nil) or (ASource.Key.Name <> FKey.Name) or
    not FSchema.SameSchema(ASource.Schema) then
  begin
    raise ENyxCollection.Create('Assign requires the same collection key and schema');
  end;

  if (ASource.Count < 0) or (ASource.Count > NyxMaximumCollectionItems) then
  begin
    raise ENyxCollection.Create('Assigned collection exceeds its item budget');
  end;
  LKeepAlive := Self;
  SetLength(LItems, ASource.Count);
  for LIndex := 0 to Length(LItems) - 1 do
  begin
    LItems[LIndex] := Normalize(ASource.ItemAt(LIndex), ASource.ItemAt(LIndex), False);
  end;
  LSame := Length(LItems) = Length(FItems);

  if LSame then
  begin
    for LIndex := 0 to Length(LItems) - 1 do
    begin

      if not LItems[LIndex].SameItem(FItems[LIndex]) then
      begin
        LSame := False;
        Break;
      end;
    end;
  end;

  if LSame then
  begin
    Exit;
  end;
  { Full dataset replacement has an explicit incremental replay: remove old rows
    from the end, then insert the admitted new order. Ordinary updates use Apply
    and retain their smaller precise operation logs. }
  SetLength(LSteps, Length(FItems) + Length(LItems));
  LStep := 0;
  for LIndex := High(FItems) downto 0 do
  begin
    LSteps[LStep].Kind := nceRemove;
    LSteps[LStep].Ref := FItems[LIndex].Ref;
    LSteps[LStep].BeforeIndex := LIndex;
    LSteps[LStep].AfterIndex := -1;
    LSteps[LStep].BeforeItem := FItems[LIndex];
    Inc(LStep);
  end;
  for LIndex := 0 to Length(LItems) - 1 do
  begin
    LSteps[LStep].Kind := nceInsert;
    LSteps[LStep].Ref := LItems[LIndex].Ref;
    LSteps[LStep].BeforeIndex := -1;
    LSteps[LStep].AfterIndex := LIndex;
    LSteps[LStep].AfterItem := LItems[LIndex];
    Inc(LStep);
  end;
  Publish(LItems, LSteps);
end;

function TCollectionStore.Clone: INyxCollection;
var
  LStore: TCollectionStore;
begin
  LStore := TCollectionStore.Create(FKey, FSchema, []);
  Result := LStore;
  { Private immutable rows/snapshot are shared safely; the first edit forks the
    outer vector. No callbacks/tokens are copied or reference-linked. }
  LStore.FItems := Copy(FItems, 0, Length(FItems));
  LStore.FCurrent := FCurrent;
end;
