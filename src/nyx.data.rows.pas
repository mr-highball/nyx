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

unit nyx.data.rows;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.state, nyx.resources, nyx.collections, nyx.contract;

type
  TNyxRowNumberPolicy = (nrnpCanonical, nrnpApproximate);
  TNyxRowProjection = (nrpDirect, nrpDecimalText);
  { Immutable JSON-to-typed-rows recipe, independent of resources, UI and stores.
    Identity must select exact text; absent/wrong/null values and duplicate IDs
    refuse the whole result. DecimalText explicitly preserves a JSON number's
    original spelling in a text field. Number uses finite IEEE state storage:
    Canonical requires its exact canonical round-trip spelling; Approximate opts
    into the ordinary explicit AsNumber conversion. No implicit type coercion.
    Paths/mappings are owned snapshots; every fluent call preserves its baseline. }
  TNyxDataRows = record
  private
    FPath: TNyxResourcePath;
    FIdentity: TNyxResourcePath;
    FHasIdentity: Boolean;
    FSchema: TNyxCollectionSchema;
    FFields: array of TNyxResourcePath;
    FProjections: array of TNyxRowProjection;
    FNumbers: TNyxRowNumberPolicy;
    function Add(const AName: TNyxText; const ADefault: TNyxStateValue;
      const APath: TNyxResourcePath; AProjection: TNyxRowProjection): TNyxDataRows;
    function GetSchema: TNyxCollectionSchema;
  public
    function Copy: TNyxDataRows;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDataRows; static;
    function FieldPath(AIndex: System.Integer): TNyxResourcePath;
    function Projection(AIndex: System.Integer): TNyxRowProjection;
    function Field(const AName: TNyxText): TNyxDataRows;
    function Item(AIndex: System.Integer): TNyxDataRows;
    function Identity(const APath: TNyxResourcePath): TNyxDataRows;
    function Numbers(APolicy: TNyxRowNumberPolicy): TNyxDataRows;
    function Text(const AField: TNyxTextFieldRef): TNyxDataRows; overload;
    function Text(const AField: TNyxTextFieldRef; const APath: TNyxResourcePath): TNyxDataRows; overload;
    function DecimalText(const AField: TNyxTextFieldRef): TNyxDataRows; overload;
    function DecimalText(const AField: TNyxTextFieldRef; const APath: TNyxResourcePath): TNyxDataRows; overload;
    function Boolean(const AField: TNyxBooleanFieldRef): TNyxDataRows; overload;
    function Boolean(const AField: TNyxBooleanFieldRef; const APath: TNyxResourcePath): TNyxDataRows; overload;
    function Integer(const AField: TNyxIntegerFieldRef): TNyxDataRows; overload;
    function Integer(const AField: TNyxIntegerFieldRef; const APath: TNyxResourcePath): TNyxDataRows; overload;
    function Number(const AField: TNyxNumberFieldRef): TNyxDataRows; overload;
    function Number(const AField: TNyxNumberFieldRef; const APath: TNyxResourcePath): TNyxDataRows; overload;
    { Mapping covers every destination field once, by exact name/family. A runtime
      schema may supply different defaults/domains; all actual values still pass
      that schema's ordinary validators before any result escapes. }
    procedure MatchSchema(const ASchema: TNyxCollectionSchema);
    function Read(const AData: TNyxDataValue; const AKey: TNyxCollectionRef): INyxCollectionSnapshot; overload;
    function Read(const AData: TNyxDataValue; const AKey: TNyxCollectionRef;
      const ASchema: TNyxCollectionSchema): INyxCollectionSnapshot; overload;
    property Path: TNyxResourcePath read FPath;
    property IdentityPath: TNyxResourcePath read FIdentity;
    property Schema: TNyxCollectionSchema read GetSchema;
    property NumberPolicy: TNyxRowNumberPolicy read FNumbers;
  end;

function NyxDataRows: TNyxDataRows;

implementation

function NyxDataRows: TNyxDataRows;
begin
  Result := Default(TNyxDataRows);
  Result.FSchema := NyxCollectionSchema;
end;

function TNyxDataRows.Copy: TNyxDataRows;
var
  LIndex: System.Integer;
begin
  Result := Self;
  Result.FPath := FPath.Copy;
  Result.FIdentity := FIdentity.Copy;
  Result.FSchema := FSchema.Copy;
  Result.FFields := nil;
  Result.FProjections := nil;
  SetLength(Result.FFields, Length(FFields));
  SetLength(Result.FProjections, Length(FProjections));
  for LIndex := 0 to High(FFields) do
  begin
    Result.FFields[LIndex] := FFields[LIndex].Copy;
    Result.FProjections[LIndex] := FProjections[LIndex];
  end;
end;

procedure TNyxDataRows.Validate;
var
  LStore: INyxCollection;
begin

  if not FHasIdentity or (FSchema.Count = 0) or
    (Length(FFields) <> FSchema.Count) or (Length(FProjections) <> FSchema.Count) or
    (Ord(FNumbers) < Ord(Low(TNyxRowNumberPolicy))) or
    (Ord(FNumbers) > Ord(High(TNyxRowNumberPolicy))) then
  begin
    raise ENyxResource.Create('Data rows require identity, ordered typed fields and numeric policy');
  end;
  LStore := NewNyxCollection(NyxCollection('row-schema'), FSchema, []);
end;

function TNyxDataRows.GetSchema: TNyxCollectionSchema;
begin
  Result := FSchema.Copy;
end;

function TNyxDataRows.FieldPath(AIndex: System.Integer): TNyxResourcePath;
begin

  if (AIndex < 0) or (AIndex >= Length(FFields)) then
  begin
    raise ENyxResource.Create('Data row field index is outside its range');
  end;
  Result := FFields[AIndex].Copy;
end;

function TNyxDataRows.Projection(AIndex: System.Integer): TNyxRowProjection;
begin
  FieldPath(AIndex);
  Result := FProjections[AIndex];
end;

function TNyxDataRows.Field(const AName: TNyxText): TNyxDataRows;
begin
  Result := Copy;
  Result.FPath := FPath.Field(AName);
end;

function TNyxDataRows.Item(AIndex: System.Integer): TNyxDataRows;
begin
  Result := Copy;
  Result.FPath := FPath.Item(AIndex);
end;

function TNyxDataRows.Identity(const APath: TNyxResourcePath): TNyxDataRows;
begin
  Result := Copy;
  Result.FIdentity := APath.Copy;
  Result.FHasIdentity := True;
end;

function TNyxDataRows.Numbers(APolicy: TNyxRowNumberPolicy): TNyxDataRows;
begin

  if (Ord(APolicy) < Ord(Low(TNyxRowNumberPolicy))) or
    (Ord(APolicy) > Ord(High(TNyxRowNumberPolicy))) then
  begin
    raise ENyxResource.Create('Unknown data row number policy');
  end;
  Result := Copy;
  Result.FNumbers := APolicy;
end;

function TNyxDataRows.Add(const AName: TNyxText; const ADefault: TNyxStateValue;
  const APath: TNyxResourcePath; AProjection: TNyxRowProjection): TNyxDataRows;
begin
  Result := Copy;
  Result.FSchema := FSchema.Field(AName, ADefault, NyxNoDomain);
  SetLength(Result.FFields, Length(FFields) + 1);
  SetLength(Result.FProjections, Length(FProjections) + 1);
  Result.FFields[High(Result.FFields)] := APath.Copy;
  Result.FProjections[High(Result.FProjections)] := AProjection;
end;

function TNyxDataRows.Text(const AField: TNyxTextFieldRef): TNyxDataRows;
begin
  Result := Text(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxDataRows.Text(const AField: TNyxTextFieldRef; const APath: TNyxResourcePath): TNyxDataRows;
begin
  Result := Add(AField.Name, TNyxStateValue.FromText(''), APath, nrpDirect);
end;

function TNyxDataRows.DecimalText(const AField: TNyxTextFieldRef): TNyxDataRows;
begin
  Result := DecimalText(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxDataRows.DecimalText(const AField: TNyxTextFieldRef; const APath: TNyxResourcePath): TNyxDataRows;
begin
  Result := Add(AField.Name, TNyxStateValue.FromText(''), APath, nrpDecimalText);
end;

function TNyxDataRows.Boolean(const AField: TNyxBooleanFieldRef): TNyxDataRows;
begin
  Result := Boolean(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxDataRows.Boolean(const AField: TNyxBooleanFieldRef; const APath: TNyxResourcePath): TNyxDataRows;
begin
  Result := Add(AField.Name, TNyxStateValue.FromBoolean(False), APath, nrpDirect);
end;

function TNyxDataRows.Integer(const AField: TNyxIntegerFieldRef): TNyxDataRows;
begin
  Result := Integer(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxDataRows.Integer(const AField: TNyxIntegerFieldRef; const APath: TNyxResourcePath): TNyxDataRows;
begin
  Result := Add(AField.Name, TNyxStateValue.FromInteger(0), APath, nrpDirect);
end;

function TNyxDataRows.Number(const AField: TNyxNumberFieldRef): TNyxDataRows;
begin
  Result := Number(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxDataRows.Number(const AField: TNyxNumberFieldRef; const APath: TNyxResourcePath): TNyxDataRows;
begin
  Result := Add(AField.Name, TNyxStateValue.FromNumber(0), APath, nrpDirect);
end;

function TNyxDataRows.ToData: TNyxDataValue;
var
  LFields: array of TNyxDataValue;
  LIndex: System.Integer;
  LField: TNyxCollectionField;
begin
  Validate;
  SetLength(LFields, FSchema.Count);
  for LIndex := 0 to High(LFields) do
  begin
    LField := FSchema.FieldAt(LIndex);
    LFields[LIndex] := NyxObject([NyxField('name', NyxData(LField.Name)),
      NyxField('type', NyxData(NyxStateKindName(LField.Kind))),
      NyxField('path', FFields[LIndex].ToData),
      NyxField('projection', NyxData(Ord(FProjections[LIndex])))]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('path', FPath.ToData),
    NyxField('identity', FIdentity.ToData), NyxField('fields', NyxArray(LFields)),
    NyxField('numbers', NyxData(Ord(FNumbers)))]);
end;

class function TNyxDataRows.FromData(const AData: TNyxDataValue): TNyxDataRows;
var
  LFields: TNyxDataValue;
  LField: TNyxDataValue;
  LPath: TNyxResourcePath;
  LName: TNyxText;
  LKind: TNyxText;
  LIndex: System.Integer;
  LProjection: System.Integer;
  LNumbers: System.Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 5) or
    (AData.Field('version').ToJSON <> '1') then
  begin
    raise ENyxResource.Create('Data row recipe requires a closed version-one descriptor');
  end;
  LNumbers := AData.Field('numbers').AsInteger;

  if (LNumbers < Ord(Low(TNyxRowNumberPolicy))) or
    (LNumbers > Ord(High(TNyxRowNumberPolicy))) then
  begin
    raise ENyxResource.Create('Unknown data row number policy');
  end;
  Result := NyxDataRows.Numbers(TNyxRowNumberPolicy(LNumbers));
  Result.FPath := TNyxResourcePath.FromData(AData.Field('path'));
  Result := Result.Identity(TNyxResourcePath.FromData(AData.Field('identity')));
  LFields := AData.Field('fields');

  if (LFields.Kind <> ndArray) or (LFields.Count > NyxMaximumCollectionFields) then
  begin
    raise ENyxResource.Create('Data row fields require a bounded ordered array');
  end;
  for LIndex := 0 to LFields.Count - 1 do
  begin
    LField := LFields.Item(LIndex);

    if (LField.Kind <> ndObject) or (LField.Count <> 4) then
    begin
      raise ENyxResource.Create('Data row field requires name, type, path and projection');
    end;
    LName := LField.Field('name').AsText;
    LKind := LField.Field('type').AsText;
    LPath := TNyxResourcePath.FromData(LField.Field('path'));
    LProjection := LField.Field('projection').AsInteger;

    if LProjection = Ord(nrpDecimalText) then
    begin

      if LKind <> NyxStateKindName(nskText) then
      begin
        raise ENyxResource.Create('Decimal text projection requires a text field');
      end;
      Result := Result.DecimalText(NyxTextField(LName), LPath);
    end
    else if LProjection <> Ord(nrpDirect) then
    begin
      raise ENyxResource.Create('Unknown data row projection');
    end
    else if LKind = NyxStateKindName(nskText) then
    begin
      Result := Result.Text(NyxTextField(LName), LPath);
    end
    else if LKind = NyxStateKindName(nskBoolean) then
    begin
      Result := Result.Boolean(NyxBooleanField(LName), LPath);
    end
    else if LKind = NyxStateKindName(nskInteger) then
    begin
      Result := Result.Integer(NyxIntegerField(LName), LPath);
    end
    else if LKind = NyxStateKindName(nskNumber) then
    begin
      Result := Result.Number(NyxNumberField(LName), LPath);
    end
    else
    begin
      raise ENyxResource.Create('Unknown data row field family');
    end;
  end;
  Result.Validate;
end;

procedure TNyxDataRows.MatchSchema(const ASchema: TNyxCollectionSchema);
var
  LIndex: System.Integer;
  LTarget: System.Integer;
  LField: TNyxCollectionField;
  LFound: System.Boolean;
begin
  Validate;

  if ASchema.Count <> FSchema.Count then
  begin
    raise ENyxResource.Create('Data row mapping must cover the complete destination schema');
  end;
  for LIndex := 0 to FSchema.Count - 1 do
  begin
    LField := FSchema.FieldAt(LIndex);

    LFound := False;
    for LTarget := 0 to ASchema.Count - 1 do
    begin

      if ASchema.FieldAt(LTarget).Name = LField.Name then
      begin
        LFound := ASchema.FieldAt(LTarget).Kind = LField.Kind;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxResource.Create('Data row field/destination families differ');
    end;
  end;
end;

function TNyxDataRows.Read(const AData: TNyxDataValue;
  const AKey: TNyxCollectionRef): INyxCollectionSnapshot;
begin
  Result := Read(AData, AKey, FSchema);
end;

function TNyxDataRows.Read(const AData: TNyxDataValue; const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema): INyxCollectionSnapshot;
var
  LRows: TNyxDataValue;
  LRow: TNyxDataValue;
  LValue: TNyxDataValue;
  LField: TNyxCollectionField;
  LItems: array of TNyxCollectionItem;
  LIndex: System.Integer;
  LFieldIndex: System.Integer;
  LNumber: TNyxStateValue;
  LStore: INyxCollection;
begin
  MatchSchema(ASchema);
  LRows := FPath.Select(AData);

  if (LRows.Kind <> ndArray) or (LRows.Count > NyxMaximumCollectionItems) then
  begin
    raise ENyxResource.Create('Data rows require a bounded JSON array');
  end;
  SetLength(LItems, LRows.Count);
  for LIndex := 0 to LRows.Count - 1 do
  begin
    LRow := LRows.Item(LIndex);
    LItems[LIndex] := NyxCollectionItem(NyxItem(AKey, FIdentity.Select(LRow).AsText));
    for LFieldIndex := 0 to FSchema.Count - 1 do
    begin
      LField := FSchema.FieldAt(LFieldIndex);
      LValue := FFields[LFieldIndex].Select(LRow);

      if FProjections[LFieldIndex] = nrpDecimalText then
      begin
        LItems[LIndex] := LItems[LIndex].WithValue(NyxTextField(LField.Name), LValue.AsDecimal.Text);
        Continue;
      end;
      case LField.Kind of
        nskText: LItems[LIndex] := LItems[LIndex].WithValue(NyxTextField(LField.Name), LValue.AsText);
        nskBoolean: LItems[LIndex] := LItems[LIndex].WithValue(NyxBooleanField(LField.Name), LValue.AsBoolean);
        nskInteger: LItems[LIndex] := LItems[LIndex].WithValue(NyxIntegerField(LField.Name), LValue.AsInteger);
        nskNumber:
        begin
          LNumber := TNyxStateValue.FromNumber(LValue.AsNumber);

          if (FNumbers = nrnpCanonical) and (LNumber.NumberText <> LValue.ToJSON) then
          begin
            raise ENyxResource.Create('Data number requires canonical IEEE spelling, DecimalText or explicit approximate projection');
          end;
          LItems[LIndex] := LItems[LIndex].WithValue(NyxNumberField(LField.Name), LNumber.NumberValue);
        end;
      end;
    end;
  end;
  LStore := NewNyxCollection(AKey, ASchema, LItems);
  Result := LStore.Snapshot;
end;

end.
