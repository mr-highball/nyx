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

unit nyx.collections.codec;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.collections.registry;

{ Explicit versioned descriptor boundary. Scalars remain typed in authoring;
  decimal strings preserve finite Double values through older JSON writers.
  Decode builds a detached registry, admits every schema/row/domain, and returns
  only a complete candidate. Neither function mutates its caller's defaults.
  JSON byte/depth/member limits remain the existing shared admission rules. }
function EncodeNyxCollectionDefaults(const ADefaults: INyxCollectionDefaults): TNyxText;
function DecodeNyxCollectionDefaults(const ASource: TNyxText;
  AResourceSources: Boolean = False): INyxCollectionDefaults;

implementation

uses
  fpjson,
  nyx.json,
  nyx.data,
  nyx.state,
  nyx.contract,
  nyx.resources.rows,
  nyx.collections;

function RequireObject(AData: TJSONData; AFields: Integer): TJSONObject;
begin

  if (AData = nil) or (AData.JSONType <> jtObject) then
  begin
    raise ENyxCollection.Create('Collection descriptor requires an object');
  end;
  Result := TJSONObject(AData);

  if Result.Count <> AFields then
  begin
    raise ENyxCollection.Create('Collection descriptor has missing or unknown fields');
  end;
end;

function RequireField(AObject: TJSONObject; const AName: TNyxText;
  AKind: TJSONType): TJSONData;
begin
  Result := AObject.Find(AName);

  if (Result = nil) or (Result.JSONType <> AKind) then
  begin
    raise ENyxCollection.Create('Invalid collection descriptor field: ' + AName);
  end;
end;

function KindOf(const AName: TNyxText): TNyxStateKind;
var
  LKind: TNyxStateKind;
begin
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin

    if NyxStateKindName(LKind) = AName then
    begin
      Exit(LKind);
    end;
  end;
  raise ENyxCollection.Create('Unknown collection scalar family');
end;

function ScalarJSON(const AValue: TNyxStateValue): TJSONData;
begin
  case AValue.Kind of
    nskText:
      begin
        Result := TJSONString.Create(AValue.TextValue);
      end;
    nskBoolean:
      begin
        Result := TJSONBoolean.Create(AValue.BooleanValue);
      end;
    nskInteger:
      begin
        Result := TJSONIntegerNumber.Create(AValue.IntegerValue);
      end;
    nskNumber:
      begin
        Result := TJSONString.Create(AValue.NumberText);
      end;
  end;
end;

function ReadScalar(AData: TJSONData; AKind: TNyxStateKind): TNyxStateValue;
var
  LNumber: Double;
begin

  if AData = nil then
  begin
    raise ENyxCollection.Create('Collection scalar is missing');
  end;
  case AKind of
    nskText:
      begin

        if AData.JSONType <> jtString then
        begin
          raise ENyxCollection.Create('Collection text must be a JSON string');
        end;
        Result := TNyxStateValue.FromText(AData.AsString);
      end;
    nskBoolean:
      begin

        if AData.JSONType <> jtBoolean then
        begin
          raise ENyxCollection.Create('Collection Boolean must be a JSON Boolean');
        end;
        Result := TNyxStateValue.FromBoolean(AData.AsBoolean);
      end;
    nskInteger:
      begin

        if AData.JSONType <> jtNumber then
        begin
          raise ENyxCollection.Create('Collection Integer must be a JSON integer');
        end;
        Result := TNyxStateValue.FromInteger(TNyxDataValue.ParseJSON(AData.AsJSON).AsInteger);
      end;
    nskNumber:
      begin

        if (AData.JSONType <> jtString) or
          not TryNyxStateNumber(AData.AsString, LNumber) then
        begin
          raise ENyxCollection.Create('Collection Number must be a finite decimal string');
        end;
        Result := TNyxStateValue.FromNumber(LNumber);
      end;
  end;
end;

function EncodeNyxCollectionDefaults(const ADefaults: INyxCollectionDefaults): TNyxText;
var
  LRuntime: INyxCollections;
  LDefinition: INyxCollectionSnapshot;
  LSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
  LRow: TNyxCollectionItem;
  LRoot: TJSONObject;
  LDefinitions: TJSONArray;
  LDefinitionJSON: TJSONObject;
  LFields: TJSONArray;
  LItems: TJSONArray;
  LFieldJSON: TJSONObject;
  LDefaultJSON: TJSONObject;
  LItemJSON: TJSONObject;
  LValues: TJSONArray;
  LCollection: Integer;
  LFieldIndex: Integer;
  LItemIndex: Integer;
  LAdmitted: TJSONData;
  LHasSources: Boolean;
  LSource: TNyxResourceRows;
begin
  { Materialization re-admits a foreign registry before encoding. It also
    prevents borrowed alternative arrays/index/byte claims crossing the wire. }
  LRuntime := NewNyxCollections(ADefaults);
  LHasSources := NyxHasResourceCollections(ADefaults);
  LRoot := TJSONObject.Create;
  try

    if LHasSources then
    begin
      LRoot.Add('version', 2);
    end
    else
    begin
      LRoot.Add('version', 1);
    end;
    LDefinitions := TJSONArray.Create;
    LRoot.Add('definitions', LDefinitions);
    for LCollection := 0 to LRuntime.Count - 1 do
    begin
      LDefinition := LRuntime.Collection(LRuntime.Key(LCollection)).Snapshot;
      LSchema := LDefinition.Schema;
      LDefinitionJSON := TJSONObject.Create;
      LDefinitions.Add(LDefinitionJSON);
      LDefinitionJSON.Add('key', LDefinition.Key.Name);

      if LHasSources then
      begin

        if NyxCollectionResourceSource(ADefaults, LDefinition.Key, LSource) then
        begin

          if (LDefinition.Count <> 0) or not LDefinition.Schema.SameSchema(LSource.Schema) then
          begin
            raise ENyxCollection.Create('Resource recipe conflicts with its empty schema seed');
          end;
          LDefinitionJSON.Add('source', DecodeNyxJSON(LSource.ToData.ToJSON));
        end
        else
        begin
          LDefinitionJSON.Add('source', TJSONNull.Create);
        end;
      end;
      LFields := TJSONArray.Create;
      LDefinitionJSON.Add('schema', LFields);
      for LFieldIndex := 0 to LSchema.Count - 1 do
      begin
        LField := LSchema.FieldAt(LFieldIndex);
        LFieldJSON := TJSONObject.Create;
        LFields.Add(LFieldJSON);
        LFieldJSON.Add('name', LField.Name);
        LDefaultJSON := TJSONObject.Create;
        LFieldJSON.Add('default', LDefaultJSON);
        LDefaultJSON.Add('type', NyxStateKindName(LField.Kind));
        LDefaultJSON.Add('value', ScalarJSON(LField.DefaultValue));
        LFieldJSON.Add('domain', DecodeNyxJSON(LField.Domain.ToData.ToJSON));
      end;
      LItems := TJSONArray.Create;
      LDefinitionJSON.Add('items', LItems);
      for LItemIndex := 0 to LDefinition.Count - 1 do
      begin
        LRow := LDefinition.ItemAt(LItemIndex);
        LItemJSON := TJSONObject.Create;
        LItems.Add(LItemJSON);
        LItemJSON.Add('id', LRow.Ref.ID);
        LValues := TJSONArray.Create;
        LItemJSON.Add('values', LValues);
        for LFieldIndex := 0 to LRow.Count - 1 do
        begin
          LValues.Add(ScalarJSON(LRow.FieldValue(LFieldIndex)));
        end;
      end;
    end;
    Result := LRoot.AsJSON;
    { The existing shared JSON limit applies to the entire packet, even when
      the logical runtime/defaults payload is below its independent 8-MiB cap. }
    LAdmitted := DecodeNyxJSON(Result);
    LAdmitted.Free;
  finally
    LRoot.Free;
  end;
end;

function DecodeNyxCollectionDefaults(const ASource: TNyxText;
  AResourceSources: Boolean): INyxCollectionDefaults;
var
  LData: TJSONData;
  LRoot: TJSONObject;
  LDefinitions: TJSONArray;
  LDefinition: TJSONObject;
  LFields: TJSONArray;
  LField: TJSONObject;
  LDefault: TJSONObject;
  LItems: TJSONArray;
  LItem: TJSONObject;
  LValues: TJSONArray;
  LDefaults: INyxCollectionDefaults;
  LKey: TNyxCollectionRef;
  LSchema: TNyxCollectionSchema;
  LDomain: TNyxValueDomain;
  LValue: TNyxStateValue;
  LRows: array of TNyxCollectionItem;
  LKind: TNyxStateKind;
  LCollection: Integer;
  LFieldIndex: Integer;
  LItemIndex: Integer;
  LName: TNyxText;
  LVersion: TNyxText;
  LSource: TNyxResourceRows;
  LSourceData: TJSONData;
begin
  LData := DecodeNyxJSON(ASource);
  try
    LRoot := RequireObject(LData, 2);

    LVersion := RequireField(LRoot, 'version', jtNumber).AsJSON;

    if (LVersion <> '1') and ((LVersion <> '2') or not AResourceSources) then
    begin
      raise ENyxCollection.Create('Unsupported collection descriptor version');
    end;
    LDefinitions := TJSONArray(RequireField(LRoot, 'definitions', jtArray));

    if LDefinitions.Count > NyxMaximumCollections then
    begin
      raise ENyxCollection.Create('Collection descriptor exceeds the definition count budget');
    end;
    LDefaults := NewNyxCollectionDefaults;
    for LCollection := 0 to LDefinitions.Count - 1 do
    begin

      if LVersion = '2' then
      begin
        LDefinition := RequireObject(LDefinitions.Items[LCollection], 4);
      end
      else
      begin
        LDefinition := RequireObject(LDefinitions.Items[LCollection], 3);
      end;
      LKey := NyxCollection(RequireField(LDefinition, 'key', jtString).AsString);

      if LDefaults.Has(LKey) then
      begin
        raise ENyxCollection.Create('Duplicate collection definition');
      end;
      LFields := TJSONArray(RequireField(LDefinition, 'schema', jtArray));

      if LFields.Count > NyxMaximumCollectionFields then
      begin
        raise ENyxCollection.Create('Collection descriptor exceeds the field count budget');
      end;
      LSchema := NyxCollectionSchema;
      for LFieldIndex := 0 to LFields.Count - 1 do
      begin
        LField := RequireObject(LFields.Items[LFieldIndex], 3);
        LName := RequireField(LField, 'name', jtString).AsString;
        LDefault := RequireObject(RequireField(LField, 'default', jtObject), 2);
        LKind := KindOf(RequireField(LDefault, 'type', jtString).AsString);
        LValue := ReadScalar(LDefault.Find('value'), LKind);

        if LField.Find('domain') = nil then
        begin
          raise ENyxCollection.Create('Collection field domain descriptor is required');
        end;
        LDomain := TNyxValueDomain.FromData(TNyxDataValue.ParseJSON(LField.Find('domain').AsJSON));
        LSchema := LSchema.Field(LName, LValue, LDomain);
      end;
      LItems := TJSONArray(RequireField(LDefinition, 'items', jtArray));

      if LItems.Count > NyxMaximumCollectionItems then
      begin
        raise ENyxCollection.Create('Collection descriptor exceeds the item count budget');
      end;
      SetLength(LRows, LItems.Count);
      for LItemIndex := 0 to LItems.Count - 1 do
      begin
        LItem := RequireObject(LItems.Items[LItemIndex], 2);
        LRows[LItemIndex] := NyxCollectionItem(NyxItem(LKey,
          RequireField(LItem, 'id', jtString).AsString));
        LValues := TJSONArray(RequireField(LItem, 'values', jtArray));

        if LValues.Count <> LSchema.Count then
        begin
          raise ENyxCollection.Create('Collection row does not match its ordered schema');
        end;
        for LFieldIndex := 0 to LValues.Count - 1 do
        begin
          LValue := ReadScalar(LValues.Items[LFieldIndex], LSchema.FieldAt(LFieldIndex).Kind);
          LName := LSchema.FieldAt(LFieldIndex).Name;
          case LValue.Kind of
            nskText:
              begin
                LRows[LItemIndex] := LRows[LItemIndex].WithValue(NyxTextField(LName), LValue.TextValue);
              end;
            nskBoolean:
              begin
                LRows[LItemIndex] := LRows[LItemIndex].WithValue(NyxBooleanField(LName), LValue.BooleanValue);
              end;
            nskInteger:
              begin
                LRows[LItemIndex] := LRows[LItemIndex].WithValue(NyxIntegerField(LName), LValue.IntegerValue);
              end;
            nskNumber:
              begin
                LRows[LItemIndex] := LRows[LItemIndex].WithValue(NyxNumberField(LName), LValue.NumberValue);
              end;
          end;
        end;
      end;
      LSourceData := LDefinition.Find('source');

      if (LVersion = '2') and (LSourceData = nil) then
      begin
        raise ENyxCollection.Create('Resource-capable collection requires explicit source or null');
      end;

      if (LVersion = '2') and (LSourceData.JSONType <> jtNull) then
      begin
        LSource := TNyxResourceRows.FromData(TNyxDataValue.ParseJSON(LSourceData.AsJSON));

        if (Length(LRows) <> 0) or not LSchema.SameSchema(LSource.Schema) then
        begin
          raise ENyxCollection.Create('Resource source must match its empty typed schema seed');
        end;
        NyxResourceCollections(LDefaults).Define(LKey, LSource);
      end
      else
      begin
        LDefaults.Define(LKey, LSchema, LRows);
      end;
    end;
    Result := LDefaults;
  finally
    LData.Free;
  end;
end;

end.
