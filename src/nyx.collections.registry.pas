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

unit nyx.collections.registry;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.resources,
  nyx.resources.rows,
  nyx.collections;

const
  { Recognized only by version-2 design persistence. In version 1 this name
    remains ordinary extension data, preserved without reinterpretation. }
  NyxCollectionsWireField: TNyxText = 'collections';
  NyxMaximumCollections = 64;
  { Aggregate logical payload, independent of the smaller JSON document limit.
    Runtime data may grow beyond export budgets; saving must still reject an
    oversized complete design rather than truncate it. }
  NyxMaximumCollectionDefaultsBytes = 8 * 1024 * 1024;

type
  { Managed authored definitions. Snapshot reads are immutable and remain valid
    after replacement/removal/disposal. Define admits a complete independent
    dataset at revision zero, replaces an existing key in place, and publishes
    only after aggregate admission. No mutation of a returned snapshot can
    change defaults. Clone creates a new registry, sharing only immutable data. }
  INyxCollectionDefaults = interface
    ['{0C9B0C2B-44DB-4AA6-B677-A277A998CF46}']
    function Define(const AKey: TNyxCollectionRef;
      const ASchema: TNyxCollectionSchema;
      const AItems: array of TNyxCollectionItem): INyxCollectionDefaults; overload;
    function Define(const ADefinition: INyxCollectionSnapshot): INyxCollectionDefaults; overload;
    function Remove(const AKey: TNyxCollectionRef): INyxCollectionDefaults;
    function Snapshot(const AKey: TNyxCollectionRef): INyxCollectionSnapshot;
    function Key(AIndex: Integer): TNyxCollectionRef;
    function Has(const AKey: TNyxCollectionRef): Boolean;
    function GetCount: Integer;
    function GetDataBytes: Integer;
    function Clone: INyxCollectionDefaults;
    procedure Validate;
    property Count: Integer read GetCount;
    property DataBytes: Integer read GetDataBytes;
  end;

  { Optional source capability on authored defaults. Define owns a copied recipe
    and an empty schema seed, never a resource catalog or document. Static Define
    on the original interface explicitly replaces/removes a source relationship.
    Snapshot remains the empty authored seed; materialization resolves rows from
    a supplied runtime catalog without changing this registry. Clone preserves
    recipes; Remove removes seed and recipe. No constructor fetches URLs. }
  INyxResourceCollectionDefaults = interface
    ['{95A9556F-6A88-4AA5-B5B1-C4D2A4F08E09}']
    function Define(const AKey: TNyxCollectionRef;
      const ASource: TNyxResourceRows): INyxResourceCollectionDefaults;
    function HasSource(const AKey: TNyxCollectionRef): Boolean;
    function Source(const AKey: TNyxCollectionRef): TNyxResourceRows;
    function GetSourceCount: Integer;
    property Count: Integer read GetSourceCount;
  end;

  { Managed runtime registry. Keys are fixed from admitted authored definitions;
    each returned specialized collection is independently mutable. Registries
    and returned stores can outlive their application owner without borrowing its
    document, controls or subscriptions. Clone retains current data/revisions in
    fresh stores with no observers, including a custom store implementation.
    Navigation must reuse this registry, rather than rematerialize defaults. }
  INyxCollections = interface
    ['{5F97F6F4-F7E9-44C0-B9A8-0470129E8B68}']
    function Collection(const AKey: TNyxCollectionRef): INyxCollection;
    function Key(AIndex: Integer): TNyxCollectionRef;
    function Has(const AKey: TNyxCollectionRef): Boolean;
    function GetCount: Integer;
    function Clone: INyxCollections;
    property Count: Integer read GetCount;
  end;

function NewNyxCollectionDefaults: INyxCollectionDefaults;
{ Copies authored snapshots, including empty resource schema seeds. This raw
  snapshot boundary has no catalog/locale to resolve sources. Application hosts
  use NewNyxCollectionContext or MaterializeNyxCollectionDefaults first. }
function NewNyxCollections(const ADefaults: INyxCollectionDefaults): INyxCollections;
{ The typed authoring facade refuses unsupported alternative registries. Source
  queries treat absence of the optional capability as ordinary static data. }
function NyxResourceCollections(const ADefaults: INyxCollectionDefaults): INyxResourceCollectionDefaults;
function NyxCollectionResourceSource(const ADefaults: INyxCollectionDefaults;
  const AKey: TNyxCollectionRef; out ASource: TNyxResourceRows): Boolean;
function NyxHasResourceCollections(const ADefaults: INyxCollectionDefaults): Boolean;
{ Produces independent static definitions from all source/static defaults at an
  explicit locale/fallback. Source rows must match their empty schema seed.
  Missing/malformed rows refuse the entire detached result; no URL is fetched. }
function MaterializeNyxCollectionDefaults(const ADefaults: INyxCollectionDefaults;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef): INyxCollectionDefaults;

implementation

uses nyx.bytes;

type
  TCollectionDefaultArray = array of INyxCollectionSnapshot;
  TCollectionStoreArray = array of INyxCollection;
  TResourceRecipeArray = array of TNyxResourceRows;
  TSourceFlagArray = array of Boolean;

  TCollectionDefaults = class(TInterfacedObject, INyxCollectionDefaults, INyxResourceCollectionDefaults)
  private
    FDefinitions: TCollectionDefaultArray;
    FSources: TResourceRecipeArray;
    FHasSources: TSourceFlagArray;
    FDataBytes: Integer;
    function IndexOf(const AKey: TNyxCollectionRef): Integer;
    function Admit(const ADefinition: INyxCollectionSnapshot; AHasSource: Boolean;
      const ASource: TNyxResourceRows): INyxCollectionDefaults;
  public
    function Define(const AKey: TNyxCollectionRef;
      const ASchema: TNyxCollectionSchema;
      const AItems: array of TNyxCollectionItem): INyxCollectionDefaults; overload;
    function Define(const ADefinition: INyxCollectionSnapshot): INyxCollectionDefaults; overload;
    function Define(const AKey: TNyxCollectionRef;
      const ASource: TNyxResourceRows): INyxResourceCollectionDefaults; overload;
    function HasSource(const AKey: TNyxCollectionRef): Boolean;
    function Source(const AKey: TNyxCollectionRef): TNyxResourceRows;
    function GetSourceCount: Integer;
    function Remove(const AKey: TNyxCollectionRef): INyxCollectionDefaults;
    function Snapshot(const AKey: TNyxCollectionRef): INyxCollectionSnapshot;
    function Key(AIndex: Integer): TNyxCollectionRef;
    function Has(const AKey: TNyxCollectionRef): Boolean;
    function GetCount: Integer;
    function GetDataBytes: Integer;
    function Clone: INyxCollectionDefaults;
    procedure Validate;
  end;

  TCollections = class(TInterfacedObject, INyxCollections)
  private
    FStores: TCollectionStoreArray;
    function IndexOf(const AKey: TNyxCollectionRef): Integer;
  public
    constructor Create(const ADefaults: INyxCollectionDefaults);
    function Collection(const AKey: TNyxCollectionRef): INyxCollection;
    function Key(AIndex: Integer): TNyxCollectionRef;
    function Has(const AKey: TNyxCollectionRef): Boolean;
    function GetCount: Integer;
    function Clone: INyxCollections;
  end;

{ This explicit interface boundary does not trust an alternative snapshot's
  reported byte count or row/schema admission. The normalizing store checks every
  value and builds its own identity index before returning an owned definition. }
function CopyDefinition(const ADefinition: INyxCollectionSnapshot): INyxCollectionSnapshot;
var
  LStore: INyxCollection;
begin
  LStore := NewNyxCollection(ADefinition);
  Result := LStore.Snapshot;
end;

function NewNyxCollectionDefaults: INyxCollectionDefaults;
begin
  Result := TCollectionDefaults.Create;
end;

function NewNyxCollections(const ADefaults: INyxCollectionDefaults): INyxCollections;
begin
  Result := TCollections.Create(ADefaults);
end;

function NyxResourceCollections(const ADefaults: INyxCollectionDefaults): INyxResourceCollectionDefaults;
begin

  if (ADefaults = nil) or not Supports(ADefaults, INyxResourceCollectionDefaults, Result) then
  begin
    raise ENyxCollection.Create('Collection defaults do not support typed resource sources');
  end;
end;

function NyxCollectionResourceSource(const ADefaults: INyxCollectionDefaults;
  const AKey: TNyxCollectionRef; out ASource: TNyxResourceRows): Boolean;
var
  LSources: INyxResourceCollectionDefaults;
begin
  ASource := Default(TNyxResourceRows);
  Result := (ADefaults <> nil) and Supports(ADefaults, INyxResourceCollectionDefaults, LSources);

  if Result then
  begin
    Result := LSources.HasSource(AKey);

    if Result then
    begin
      ASource := TNyxResourceRows.FromData(LSources.Source(AKey).ToData);
    end;
  end;
end;

function NyxHasResourceCollections(const ADefaults: INyxCollectionDefaults): Boolean;
var
  LSources: INyxResourceCollectionDefaults;
  LCount: Integer;
  LFound: Integer;
  LIndex: Integer;
  LKeys: Integer;
begin
  Result := (ADefaults <> nil) and Supports(ADefaults, INyxResourceCollectionDefaults, LSources);

  if Result then
  begin
    LCount := LSources.Count;
    LKeys := ADefaults.Count;

    if (LKeys < 0) or (LKeys > NyxMaximumCollections) or (LCount < 0) or (LCount > LKeys) then
    begin
      raise ENyxCollection.Create('Invalid resource collection source count');
    end;
    { A foreign optional facade cannot hide recipes by reporting zero. Check
      membership before choosing persistence/materialization versions. }
    LFound := 0;
    for LIndex := 0 to LKeys - 1 do
    begin

      if LSources.HasSource(ADefaults.Key(LIndex)) then
      begin
        Inc(LFound);
      end;
    end;

    if LFound <> LCount then
    begin
      raise ENyxCollection.Create('Resource source count differs from registry membership');
    end;
    Result := LCount > 0;
  end;
end;

function TCollectionDefaults.Define(const AKey: TNyxCollectionRef;
  const ASource: TNyxResourceRows): INyxResourceCollectionDefaults;
var
  LSource: TNyxResourceRows;
  LSeed: INyxCollection;
  LAdmitted: INyxCollectionDefaults;
begin
  LSource := TNyxResourceRows.FromData(ASource.ToData);
  LSeed := NewNyxCollection(AKey, LSource.Schema, []);
  LAdmitted := Admit(LSeed.Snapshot, True, LSource);
  Result := Self;
end;

function TCollectionDefaults.HasSource(const AKey: TNyxCollectionRef): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AKey);
  Result := (LIndex >= 0) and FHasSources[LIndex];
end;

function TCollectionDefaults.Source(const AKey: TNyxCollectionRef): TNyxResourceRows;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AKey);

  if (LIndex < 0) or not FHasSources[LIndex] then
  begin
    raise ENyxCollection.Create('Collection has no resource row source: ' + AKey.Name);
  end;
  Result := FSources[LIndex].Copy;
end;

function TCollectionDefaults.GetSourceCount: Integer;
var
  LIndex: Integer;
begin
  Result := 0;
  for LIndex := 0 to High(FHasSources) do
  begin

    if FHasSources[LIndex] then
    begin
      Inc(Result);
    end;
  end;
end;

function MaterializeNyxCollectionDefaults(const ADefaults: INyxCollectionDefaults;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef): INyxCollectionDefaults;
var
  LIndex: Integer;
  LDefinition: INyxCollectionSnapshot;
  LSource: TNyxResourceRows;
  LNormalized: INyxCollections;
  LResult: INyxCollectionDefaults;
begin
  { Re-admit foreign registry/snapshot values before resolving any source. The
    resulting registry owns only immutable materialized rows, never recipes or
    catalogs. Runtime contexts preserve their separate original source defaults. }
  LNormalized := NewNyxCollections(ADefaults);
  LResult := NewNyxCollectionDefaults;
  for LIndex := 0 to LNormalized.Count - 1 do
  begin
    LDefinition := LNormalized.Collection(LNormalized.Key(LIndex)).Snapshot;

    if NyxCollectionResourceSource(ADefaults, LDefinition.Key, LSource) then
    begin

      if (LDefinition.Count <> 0) or not LDefinition.Schema.SameSchema(LSource.Schema) then
      begin
        raise ENyxCollection.Create('Resource row source must match its empty schema seed');
      end;
      LDefinition := LSource.Read(AResources, LDefinition.Key, ALocale, AFallback);
    end;
    LResult.Define(LDefinition);
  end;
  Result := LResult;
end;

function TCollectionDefaults.IndexOf(const AKey: TNyxCollectionRef): Integer;
var
  LIndex: Integer;
  LName: TNyxText;
begin
  LName := AKey.Name;
  for LIndex := 0 to Length(FDefinitions) - 1 do
  begin

    if FDefinitions[LIndex].Key.Name = LName then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TCollectionDefaults.Admit(const ADefinition: INyxCollectionSnapshot;
  AHasSource: Boolean; const ASource: TNyxResourceRows): INyxCollectionDefaults;
var
  LIndex: Integer;
  LBytes: Integer;
  LCount: Integer;
  LDefinitions: TCollectionDefaultArray;
  LSources: TResourceRecipeArray;
  LFlags: TSourceFlagArray;
begin
  LIndex := IndexOf(ADefinition.Key);
  LBytes := FDataBytes + ADefinition.DataBytes;

  if AHasSource then
  begin
    Inc(LBytes, NyxUTF8ByteCount(ASource.ToData.ToJSON));
  end;

  if LIndex >= 0 then
  begin
    Dec(LBytes, FDefinitions[LIndex].DataBytes);

    if FHasSources[LIndex] then
    begin
      Dec(LBytes, NyxUTF8ByteCount(FSources[LIndex].ToData.ToJSON));
    end;
  end;

  if (LBytes > NyxMaximumCollectionDefaultsBytes) or
    ((LIndex < 0) and (Length(FDefinitions) >= NyxMaximumCollections)) then
  begin
    raise ENyxCollection.Create('Collection defaults exceed their aggregate payload/count budget');
  end;
  { Allocate every parallel vector before replacing accepted members. The
    bounded registry shares only immutable rows/recipe payloads; no callbacks. }
  LCount := Length(FDefinitions);

  if LIndex < 0 then
  begin
    LIndex := LCount;
    Inc(LCount);
  end;
  LDefinitions := Copy(FDefinitions, 0, Length(FDefinitions));
  LSources := Copy(FSources, 0, Length(FSources));
  LFlags := Copy(FHasSources, 0, Length(FHasSources));
  SetLength(LDefinitions, LCount);
  SetLength(LSources, LCount);
  SetLength(LFlags, LCount);
  LDefinitions[LIndex] := ADefinition;
  LSources[LIndex] := ASource;
  LFlags[LIndex] := AHasSource;
  FDefinitions := LDefinitions;
  FSources := LSources;
  FHasSources := LFlags;
  FDataBytes := LBytes;
  Result := Self;
end;

function TCollectionDefaults.Define(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema;
  const AItems: array of TNyxCollectionItem): INyxCollectionDefaults;
var
  LStore: INyxCollection;
begin
  LStore := NewNyxCollection(AKey, ASchema, AItems);
  Result := Admit(LStore.Snapshot, False, Default(TNyxResourceRows));
end;

function TCollectionDefaults.Define(const ADefinition: INyxCollectionSnapshot): INyxCollectionDefaults;
begin
  Result := Admit(CopyDefinition(ADefinition), False, Default(TNyxResourceRows));
end;

function TCollectionDefaults.Remove(const AKey: TNyxCollectionRef): INyxCollectionDefaults;
var
  LIndex: Integer;
  LNext: Integer;
  LBytes: Integer;
begin
  LIndex := IndexOf(AKey);

  if LIndex < 0 then
  begin
    raise ENyxCollection.Create('Cannot remove an unknown collection definition');
  end;
  { Compute the immutable recipe charge before touching accepted vectors. }
  LBytes := FDefinitions[LIndex].DataBytes;

  if FHasSources[LIndex] then
  begin
    Inc(LBytes, NyxUTF8ByteCount(FSources[LIndex].ToData.ToJSON));
  end;
  Dec(FDataBytes, LBytes);
  for LNext := LIndex to High(FDefinitions) - 1 do
  begin
    FDefinitions[LNext] := FDefinitions[LNext + 1];
    FSources[LNext] := FSources[LNext + 1];
    FHasSources[LNext] := FHasSources[LNext + 1];
  end;
  SetLength(FDefinitions, Length(FDefinitions) - 1);
  SetLength(FSources, Length(FDefinitions));
  SetLength(FHasSources, Length(FDefinitions));
  Result := Self;
end;

function TCollectionDefaults.Snapshot(const AKey: TNyxCollectionRef): INyxCollectionSnapshot;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AKey);

  if LIndex < 0 then
  begin
    raise ENyxCollection.Create('Unknown collection definition: ' + AKey.Name);
  end;
  Result := FDefinitions[LIndex];
end;

function TCollectionDefaults.Key(AIndex: Integer): TNyxCollectionRef;
begin

  if (AIndex < 0) or (AIndex >= GetCount) then
  begin
    raise ENyxCollection.Create('Collection definition index is outside its range');
  end;
  Result := FDefinitions[AIndex].Key;
end;

function TCollectionDefaults.Has(const AKey: TNyxCollectionRef): Boolean;
begin
  Result := IndexOf(AKey) >= 0;
end;

function TCollectionDefaults.GetCount: Integer;
begin
  Result := Length(FDefinitions);
end;

function TCollectionDefaults.GetDataBytes: Integer;
begin
  Result := FDataBytes;
end;

function TCollectionDefaults.Clone: INyxCollectionDefaults;
var
  LCopy: TCollectionDefaults;
begin
  LCopy := TCollectionDefaults.Create;
  Result := LCopy;
  { Fork the mutable registry vector; immutable admitted snapshots can share. }
  LCopy.FDefinitions := Copy(FDefinitions, 0, Length(FDefinitions));
  LCopy.FSources := Copy(FSources, 0, Length(FSources));
  LCopy.FHasSources := Copy(FHasSources, 0, Length(FHasSources));
  LCopy.FDataBytes := FDataBytes;
end;

procedure TCollectionDefaults.Validate;
begin
  { Define is the only insertion/replacement path; no writable data or arrays
    escape it. Admission already owns and validates every field/value/index. }

  if (GetCount > NyxMaximumCollections) or
    (FDataBytes > NyxMaximumCollectionDefaultsBytes) then
  begin
    raise ENyxCollection.Create('Invalid collection defaults budget');
  end;
end;

constructor TCollections.Create(const ADefaults: INyxCollectionDefaults);
var
  LIndex: Integer;
  LPrevious: Integer;
  LCount: Integer;
  LBytes: Integer;
  LDefinition: INyxCollectionSnapshot;
begin
  inherited Create;

  if ADefaults = nil then
  begin
    raise ENyxCollection.Create('Runtime collections require authored defaults');
  end;
  ADefaults.Validate;
  LCount := ADefaults.Count;

  if (LCount < 0) or (LCount > NyxMaximumCollections) then
  begin
    raise ENyxCollection.Create('Invalid runtime collection count');
  end;
  SetLength(FStores, LCount);
  LBytes := 0;
  for LIndex := 0 to LCount - 1 do
  begin
    LDefinition := ADefaults.Snapshot(ADefaults.Key(LIndex));
    FStores[LIndex] := NewNyxCollection(LDefinition);
    LDefinition := FStores[LIndex].Snapshot;

    if LDefinition.Key.Name <> ADefaults.Key(LIndex).Name then
    begin
      raise ENyxCollection.Create('Collection registry key/definition mismatch');
    end;
    Inc(LBytes, LDefinition.DataBytes);

    if LBytes > NyxMaximumCollectionDefaultsBytes then
    begin
      raise ENyxCollection.Create('Runtime collection defaults exceed the aggregate budget');
    end;
    for LPrevious := 0 to LIndex - 1 do
    begin

      if FStores[LPrevious].Snapshot.Key.Name = LDefinition.Key.Name then
      begin
        raise ENyxCollection.Create('Duplicate runtime collection definition');
      end;
    end;
  end;
end;

function TCollections.IndexOf(const AKey: TNyxCollectionRef): Integer;
var
  LIndex: Integer;
  LName: TNyxText;
begin
  LName := AKey.Name;
  for LIndex := 0 to Length(FStores) - 1 do
  begin

    if FStores[LIndex].Snapshot.Key.Name = LName then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TCollections.Collection(const AKey: TNyxCollectionRef): INyxCollection;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AKey);

  if LIndex < 0 then
  begin
    raise ENyxCollection.Create('Unknown runtime collection: ' + AKey.Name);
  end;
  Result := FStores[LIndex];
end;

function TCollections.Key(AIndex: Integer): TNyxCollectionRef;
begin

  if (AIndex < 0) or (AIndex >= GetCount) then
  begin
    raise ENyxCollection.Create('Runtime collection index is outside its range');
  end;
  Result := FStores[AIndex].Snapshot.Key;
end;

function TCollections.Has(const AKey: TNyxCollectionRef): Boolean;
begin
  Result := IndexOf(AKey) >= 0;
end;

function TCollections.GetCount: Integer;
begin
  Result := Length(FStores);
end;

function TCollections.Clone: INyxCollections;
var
  LCopy: TCollections;
  LIndex: Integer;
begin
  LCopy := TCollections.Create(NewNyxCollectionDefaults);
  Result := LCopy;
  SetLength(LCopy.FStores, Length(FStores));
  for LIndex := 0 to High(FStores) do
  begin
    LCopy.FStores[LIndex] := FStores[LIndex].Clone;
  end;
end;

end.
