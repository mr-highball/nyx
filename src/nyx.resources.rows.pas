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
unit nyx.resources.rows;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.state, nyx.resources, nyx.collections, nyx.contract,
  nyx.publication, nyx.data.rows;

type
  { Immutable JSON-to-collection recipe. An explicit text identity path supplies
    stable row IDs. Typed fields select their own structural paths; omitted
    source paths use the exact field name. Missing/null/wrong-kind data refuses,
    including duplicate IDs, rather than silently applying schema defaults.
    Read builds a complete independent snapshot. Reload uses the ordinary
    collection's validator/notification contract and cannot mutate authored
    defaults, a sibling store or this recipe. No control/document is retained. }
  TNyxResourceRows = record
  private
    FReference: TNyxResourceRef;
    FRows: TNyxDataRows;
    function GetPath: TNyxResourcePath;
    function GetIdentity: TNyxResourcePath;
    function GetSchema: TNyxCollectionSchema;
  public
    function Copy: TNyxResourceRows;
    { Structural admission does not resolve a resource or fetch a URL. Returned
      paths/schema are independent immutable values; field order is preserved.
      ToData/FromData are the closed version-one persistence boundary. }
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceRows; static;
    function FieldPath(AIndex: System.Integer): TNyxResourcePath;
    property Reference: TNyxResourceRef read FReference;
    property Path: TNyxResourcePath read GetPath;
    property IdentityPath: TNyxResourcePath read GetIdentity;
    property Schema: TNyxCollectionSchema read GetSchema;
    function Field(const AName: TNyxText): TNyxResourceRows;
    function Item(AIndex: System.Integer): TNyxResourceRows;
    function Identity(const APath: TNyxResourcePath): TNyxResourceRows;
    function Read(const AResources: INyxResources; const AKey: TNyxCollectionRef;
      const ALocale, AFallback: TNyxLocaleRef): INyxCollectionSnapshot;
    { One store commit; a stale revision or invalid row preserves accepted rows.
      Notification failure follows commit, matching INyxCollection.Assign. }
    procedure Reload(const ACollection: INyxCollection; const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef; AExpectedRevision: System.Integer = -1);
    { Reads a detached dataset and reserves a coordinated-capable runtime store.
      No receiver runs until publication. Combine independent preparations with
      PublishNyxGroup; release/Retire on abandonment. Unsupported custom stores,
      busy/stale revisions and malformed data refuse without changing any rows.
      Selection/query admission also happens before every group participant
      installs. This explicit operation does not register a saved/live binding. }
    function PrepareReload(const ACollection: INyxCollection; const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef;
      AExpectedRevision: System.Integer = -1): INyxPreparedPublication;
    function Text(const AField: TNyxTextFieldRef): TNyxResourceRows; overload;
    function Text(const AField: TNyxTextFieldRef;
      const APath: TNyxResourcePath): TNyxResourceRows; overload;
    function Boolean(const AField: TNyxBooleanFieldRef): TNyxResourceRows; overload;
    function Boolean(const AField: TNyxBooleanFieldRef;
      const APath: TNyxResourcePath): TNyxResourceRows; overload;
    function Integer(const AField: TNyxIntegerFieldRef): TNyxResourceRows; overload;
    function Integer(const AField: TNyxIntegerFieldRef;
      const APath: TNyxResourcePath): TNyxResourceRows; overload;
    function Number(const AField: TNyxNumberFieldRef): TNyxResourceRows; overload;
    function Number(const AField: TNyxNumberFieldRef;
      const APath: TNyxResourcePath): TNyxResourceRows; overload;
  end;

function NyxResourceRows(const AReference: TNyxResourceRef): TNyxResourceRows;

implementation

procedure TNyxResourceRows.Validate;
begin
  NyxResourceRef(FReference.Name);
  FRows.Validate;
end;

function TNyxResourceRows.GetSchema: TNyxCollectionSchema;
begin
  Result := FRows.Schema;
end;

function TNyxResourceRows.GetPath: TNyxResourcePath;
begin
  Result := FRows.Path;
end;

function TNyxResourceRows.GetIdentity: TNyxResourcePath;
begin
  Result := FRows.IdentityPath;
end;

function TNyxResourceRows.FieldPath(AIndex: System.Integer): TNyxResourcePath;
begin
  Result := FRows.FieldPath(AIndex);
end;

function TNyxResourceRows.ToData: TNyxDataValue;
var
  LFields: array of TNyxDataValue;
  LField: TNyxCollectionField;
  LIndex: System.Integer;
begin
  Validate;
  SetLength(LFields, FRows.Schema.Count);
  for LIndex := 0 to High(LFields) do
  begin
    LField := FRows.Schema.FieldAt(LIndex);
    LFields[LIndex] := NyxObject([NyxField('name', NyxData(LField.Name)),
      NyxField('type', NyxData(NyxStateKindName(LField.Kind))),
      NyxField('path', FRows.FieldPath(LIndex).ToData)]);
  end;
  { Keep the complete original resource-row version-one wire contract. The data
    mapper's explicit numeric policy preserves the existing AsNumber behavior. }
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('resource', NyxData(FReference.Name)), NyxField('path', FRows.Path.ToData),
    NyxField('identity', FRows.IdentityPath.ToData), NyxField('fields', NyxArray(LFields))]);
end;

class function TNyxResourceRows.FromData(const AData: TNyxDataValue): TNyxResourceRows;
var
  LFields: array of TNyxDataValue;
  LField: TNyxDataValue;
  LOriginal: TNyxDataValue;
  LIndex: System.Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 5) or
    (AData.Field('version').ToJSON <> '1') then
  begin
    raise ENyxResource.Create('Resource row recipe requires a closed version-one descriptor');
  end;
  Result := NyxResourceRows(NyxResourceRef(AData.Field('resource').AsText));
  LOriginal := AData.Field('fields');

  if (LOriginal.Kind <> ndArray) or (LOriginal.Count > NyxMaximumCollectionFields) then
  begin
    raise ENyxResource.Create('Resource row fields require a bounded ordered array');
  end;
  SetLength(LFields, LOriginal.Count);
  for LIndex := 0 to High(LFields) do
  begin
    LField := LOriginal.Item(LIndex);

    if (LField.Kind <> ndObject) or (LField.Count <> 3) then
    begin
      raise ENyxResource.Create('Resource row field requires name, type and path');
    end;
    LFields[LIndex] := NyxObject([NyxField('name', LField.Field('name')),
      NyxField('type', LField.Field('type')), NyxField('path', LField.Field('path')),
      NyxField('projection', NyxData(Ord(nrpDirect)))]);
  end;
  Result.FRows := TNyxDataRows.FromData(NyxObject([
    NyxField('version', NyxData(1)), NyxField('path', AData.Field('path')),
    NyxField('identity', AData.Field('identity')), NyxField('fields', NyxArray(LFields)),
    NyxField('numbers', NyxData(Ord(nrnpApproximate)))]));
end;

function NyxResourceRows(const AReference: TNyxResourceRef): TNyxResourceRows;
begin
  Result := Default(TNyxResourceRows);
  Result.FReference := NyxResourceRef(AReference.Name);
  Result.FRows := NyxDataRows.Numbers(nrnpApproximate);
end;

function TNyxResourceRows.Copy: TNyxResourceRows;
begin
  Result.FReference := FReference;
  Result.FRows := FRows.Copy;
end;

function TNyxResourceRows.Field(const AName: TNyxText): TNyxResourceRows;
begin
  Result := Copy;
  Result.FRows := FRows.Field(AName);
end;

function TNyxResourceRows.Item(AIndex: System.Integer): TNyxResourceRows;
begin
  Result := Copy;
  Result.FRows := FRows.Item(AIndex);
end;

function TNyxResourceRows.Identity(const APath: TNyxResourcePath): TNyxResourceRows;
begin
  Result := Copy;
  Result.FRows := FRows.Identity(APath);
end;

function TNyxResourceRows.Text(const AField: TNyxTextFieldRef): TNyxResourceRows;
begin
  Result := Text(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxResourceRows.Text(const AField: TNyxTextFieldRef;
  const APath: TNyxResourcePath): TNyxResourceRows;
begin
  Result := Copy;
  Result.FRows := FRows.Text(AField, APath);
end;

function TNyxResourceRows.Boolean(const AField: TNyxBooleanFieldRef): TNyxResourceRows;
begin
  Result := Boolean(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxResourceRows.Boolean(const AField: TNyxBooleanFieldRef;
  const APath: TNyxResourcePath): TNyxResourceRows;
begin
  Result := Copy;
  Result.FRows := FRows.Boolean(AField, APath);
end;

function TNyxResourceRows.Integer(const AField: TNyxIntegerFieldRef): TNyxResourceRows;
begin
  Result := Integer(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxResourceRows.Integer(const AField: TNyxIntegerFieldRef;
  const APath: TNyxResourcePath): TNyxResourceRows;
begin
  Result := Copy;
  Result.FRows := FRows.Integer(AField, APath);
end;

function TNyxResourceRows.Number(const AField: TNyxNumberFieldRef): TNyxResourceRows;
begin
  Result := Number(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxResourceRows.Number(const AField: TNyxNumberFieldRef;
  const APath: TNyxResourcePath): TNyxResourceRows;
begin
  Result := Copy;
  Result.FRows := FRows.Number(AField, APath);
end;

function TNyxResourceRows.Read(const AResources: INyxResources;
  const AKey: TNyxCollectionRef; const ALocale, AFallback: TNyxLocaleRef): INyxCollectionSnapshot;
begin

  if AResources = nil then
  begin
    raise ENyxResource.Create('Resource rows require a catalog');
  end;
  Validate;
  Result := FRows.Read(AResources.Resolve(FReference, ALocale, AFallback).Data, AKey);
end;

procedure TNyxResourceRows.Reload(const ACollection: INyxCollection;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef;
  AExpectedRevision: System.Integer);
var
  LCandidate: INyxCollectionSnapshot;
begin

  if ACollection = nil then
  begin
    raise ENyxResource.Create('Resource reload requires a runtime collection');
  end;
  LCandidate := Read(AResources, ACollection.Snapshot.Key, ALocale, AFallback);
  ACollection.Assign(LCandidate, AExpectedRevision);
end;

function TNyxResourceRows.PrepareReload(const ACollection: INyxCollection;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef;
  AExpectedRevision: System.Integer): INyxPreparedPublication;
var
  LAtomic: INyxAtomicCollection;
  LCandidate: INyxCollectionSnapshot;
  LRevision: System.Integer;
begin

  if (ACollection = nil) or not Supports(ACollection, INyxAtomicCollection, LAtomic) then
  begin
    raise ENyxResource.Create('Grouped resource reload requires a coordinated collection');
  end;
  LRevision := ACollection.Snapshot.Revision;

  if LAtomic.Busy or ((AExpectedRevision >= 0) and (AExpectedRevision <> LRevision)) then
  begin
    raise ENyxResource.Create('Grouped resource reload requires an idle, current collection');
  end;
  LCandidate := Read(AResources, ACollection.Snapshot.Key, ALocale, AFallback);
  { Recheck the captured revision after reads from a possibly foreign catalog.
    Omitting an expected revision still cannot admit an intervening mutation. }
  Result := LAtomic.PrepareAssign(LCandidate, LRevision);
end;

end.
