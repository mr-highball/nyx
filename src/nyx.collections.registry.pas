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
  nyx.text,
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
function NewNyxCollections(const ADefaults: INyxCollectionDefaults): INyxCollections;

implementation

type
  TCollectionDefaultArray = array of INyxCollectionSnapshot;
  TCollectionStoreArray = array of INyxCollection;

  TCollectionDefaults = class(TInterfacedObject, INyxCollectionDefaults)
  private
    FDefinitions: TCollectionDefaultArray;
    FDataBytes: Integer;
    function IndexOf(const AKey: TNyxCollectionRef): Integer;
    function Admit(const ADefinition: INyxCollectionSnapshot): INyxCollectionDefaults;
  public
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

function TCollectionDefaults.Admit(const ADefinition: INyxCollectionSnapshot): INyxCollectionDefaults;
var
  LIndex: Integer;
  LBytes: Integer;
begin
  LIndex := IndexOf(ADefinition.Key);
  LBytes := FDataBytes + ADefinition.DataBytes;

  if LIndex >= 0 then
  begin
    Dec(LBytes, FDefinitions[LIndex].DataBytes);
  end;

  if (LBytes > NyxMaximumCollectionDefaultsBytes) or
    ((LIndex < 0) and (Length(FDefinitions) >= NyxMaximumCollections)) then
  begin
    raise ENyxCollection.Create('Collection defaults exceed their aggregate payload/count budget');
  end;
  { Allocation is the final possible failure before replacement. Retained
    snapshots are immutable; no caller or receiver can observe a partial row. }

  if LIndex < 0 then
  begin
    LIndex := Length(FDefinitions);
    SetLength(FDefinitions, LIndex + 1);
  end;
  FDefinitions[LIndex] := ADefinition;
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
  Result := Admit(LStore.Snapshot);
end;

function TCollectionDefaults.Define(const ADefinition: INyxCollectionSnapshot): INyxCollectionDefaults;
begin
  Result := Admit(CopyDefinition(ADefinition));
end;

function TCollectionDefaults.Remove(const AKey: TNyxCollectionRef): INyxCollectionDefaults;
var
  LIndex: Integer;
  LNext: Integer;
begin
  LIndex := IndexOf(AKey);

  if LIndex < 0 then
  begin
    raise ENyxCollection.Create('Cannot remove an unknown collection definition');
  end;
  Dec(FDataBytes, FDefinitions[LIndex].DataBytes);
  for LNext := LIndex to High(FDefinitions) - 1 do
  begin
    FDefinitions[LNext] := FDefinitions[LNext + 1];
  end;
  SetLength(FDefinitions, Length(FDefinitions) - 1);
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
