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
  nyx.publication;

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
    FPath: TNyxResourcePath;
    FIdentity: TNyxResourcePath;
    FHasIdentity: Boolean;
    FSchema: TNyxCollectionSchema;
    FFields: array of TNyxResourcePath;
    function Add(const AName: TNyxText; const ADefault: TNyxStateValue;
      const APath: TNyxResourcePath): TNyxResourceRows;
  public
    function Copy: TNyxResourceRows;
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

function NyxResourceRows(const AReference: TNyxResourceRef): TNyxResourceRows;
begin
  Result := Default(TNyxResourceRows);
  Result.FReference := NyxResourceRef(AReference.Name);
  Result.FSchema := NyxCollectionSchema;
end;

function TNyxResourceRows.Copy: TNyxResourceRows;
var
  LIndex: System.Integer;
begin
  Result.FReference := FReference;
  Result.FPath := FPath.Copy;
  Result.FIdentity := FIdentity.Copy;
  Result.FHasIdentity := FHasIdentity;
  Result.FSchema := FSchema.Copy;
  SetLength(Result.FFields, Length(FFields));
  for LIndex := 0 to High(FFields) do
  begin
    Result.FFields[LIndex] := FFields[LIndex].Copy;
  end;
end;

function TNyxResourceRows.Field(const AName: TNyxText): TNyxResourceRows;
begin
  Result := Copy;
  Result.FPath := FPath.Field(AName);
end;

function TNyxResourceRows.Item(AIndex: System.Integer): TNyxResourceRows;
begin
  Result := Copy;
  Result.FPath := FPath.Item(AIndex);
end;

function TNyxResourceRows.Identity(const APath: TNyxResourcePath): TNyxResourceRows;
begin
  Result := Copy;
  Result.FIdentity := APath.Copy;
  Result.FHasIdentity := True;
end;

function TNyxResourceRows.Add(const AName: TNyxText; const ADefault: TNyxStateValue;
  const APath: TNyxResourcePath): TNyxResourceRows;
var
  LCandidate: TNyxResourceRows;
begin
  LCandidate := Copy;
  LCandidate.FSchema := FSchema.Field(AName, ADefault, NyxNoDomain);
  SetLength(LCandidate.FFields, Length(FFields) + 1);
  LCandidate.FFields[High(LCandidate.FFields)] := APath.Copy;
  Result := LCandidate;
end;

function TNyxResourceRows.Text(const AField: TNyxTextFieldRef): TNyxResourceRows;
begin
  Result := Text(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxResourceRows.Text(const AField: TNyxTextFieldRef;
  const APath: TNyxResourcePath): TNyxResourceRows;
begin
  Result := Add(AField.Name, TNyxStateValue.FromText(''), APath);
end;

function TNyxResourceRows.Boolean(const AField: TNyxBooleanFieldRef): TNyxResourceRows;
begin
  Result := Boolean(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxResourceRows.Boolean(const AField: TNyxBooleanFieldRef;
  const APath: TNyxResourcePath): TNyxResourceRows;
begin
  Result := Add(AField.Name, TNyxStateValue.FromBoolean(False), APath);
end;

function TNyxResourceRows.Integer(const AField: TNyxIntegerFieldRef): TNyxResourceRows;
begin
  Result := Integer(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxResourceRows.Integer(const AField: TNyxIntegerFieldRef;
  const APath: TNyxResourcePath): TNyxResourceRows;
begin
  Result := Add(AField.Name, TNyxStateValue.FromInteger(0), APath);
end;

function TNyxResourceRows.Number(const AField: TNyxNumberFieldRef): TNyxResourceRows;
begin
  Result := Number(AField, NyxResourcePath.Field(AField.Name));
end;

function TNyxResourceRows.Number(const AField: TNyxNumberFieldRef;
  const APath: TNyxResourcePath): TNyxResourceRows;
begin
  Result := Add(AField.Name, TNyxStateValue.FromNumber(0), APath);
end;

function TNyxResourceRows.Read(const AResources: INyxResources;
  const AKey: TNyxCollectionRef; const ALocale, AFallback: TNyxLocaleRef): INyxCollectionSnapshot;
var
  LRows: TNyxDataValue;
  LRow: TNyxDataValue;
  LValue: TNyxDataValue;
  LField: TNyxCollectionField;
  LItems: array of TNyxCollectionItem;
  LIndex: System.Integer;
  LFieldIndex: System.Integer;
  LStore: INyxCollection;
begin

  if (AResources = nil) or not FHasIdentity or (FSchema.Count = 0) then
  begin
    raise ENyxResource.Create('Resource rows require a catalog, identity and typed fields');
  end;
  LRows := FPath.Select(AResources.Resolve(FReference, ALocale, AFallback).Data);

  if (LRows.Kind <> ndArray) or (LRows.Count > NyxMaximumCollectionItems) then
  begin
    raise ENyxResource.Create('Resource rows require a bounded JSON array');
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
      case LField.Kind of
        nskText:
          begin
            LItems[LIndex] := LItems[LIndex].WithValue(NyxTextField(LField.Name), LValue.AsText);
          end;
        nskBoolean:
          begin
            LItems[LIndex] := LItems[LIndex].WithValue(NyxBooleanField(LField.Name), LValue.AsBoolean);
          end;
        nskInteger:
          begin
            LItems[LIndex] := LItems[LIndex].WithValue(NyxIntegerField(LField.Name), LValue.AsInteger);
          end;
        nskNumber:
          begin
            LItems[LIndex] := LItems[LIndex].WithValue(NyxNumberField(LField.Name), LValue.AsNumber);
          end;
      end;
    end;
  end;
  LStore := NewNyxCollection(AKey, FSchema, LItems);
  Result := LStore.Snapshot;
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
