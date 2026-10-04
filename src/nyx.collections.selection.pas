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
unit nyx.collections.selection;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses nyx.collections, nyx.text;

type
  { Membership and keyboard focus are independent. Single is the compatible
    default; Multiple admits discontiguous items. Selection is runtime state,
    never a persisted row index or a mutable array borrowed from a control. }
  TNyxSelectionMode = (nsmSingle, nsmMultiple);
  TNyxSelectionAction = (nsaReplace, nsaToggle, nsaRange, nsaAddRange, nsaFocus);
  TNyxItemRefs = array of TNyxItemRef;

  { Event records use a value snapshot because older pas2js cannot embed a COM
    interface in a record. Private arrays are immutable after construction and
    never exposed to writers; copied records therefore retain owned values on
    both targets. The managed view API below can use alternative implementations. }
  TNyxCollectionSelectionSnapshot = record
  private
    FMarker: TNyxText;
    FKey: TNyxCollectionRef;
    FIDs: array of TNyxText;
    FIndex: array of Integer;
    FFocus: TNyxItemRef;
    FAnchor: TNyxItemRef;
    FDataRevision: Integer;
    function GetDefined: Boolean;
    function GetCount: Integer;
  public
    function Contains(const AItem: TNyxItemRef): Boolean;
    function ItemAt(AIndex: Integer): TNyxItemRef;
    function SameState(const AOther: TNyxCollectionSelectionSnapshot): Boolean;
    property Defined: Boolean read GetDefined;
    property Key: TNyxCollectionRef read FKey;
    property Count: Integer read GetCount;
    property DataRevision: Integer read FDataRevision;
    property Focus: TNyxItemRef read FFocus;
    property Anchor: TNyxItemRef read FAnchor;
  end;

  { Immutable managed snapshot. Items are in admitted dataset order. Focus may
    identify an unselected item; Anchor is the origin of range gestures. Readers
    return typed values, and the snapshot retains no dataset, widget or observer.
    Contains is indexed; painting N selected rows does not perform N squared
    membership scans. DataRevision identifies the dataset used for admission. }
  INyxCollectionSelection = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001002000025}']
    function GetKey: TNyxCollectionRef;
    function GetCount: Integer;
    function GetDataRevision: Integer;
    function GetFocus: TNyxItemRef;
    function GetAnchor: TNyxItemRef;
    function GetSnapshot: TNyxCollectionSelectionSnapshot;
    function Contains(const AItem: TNyxItemRef): Boolean;
    function ItemAt(AIndex: Integer): TNyxItemRef;
    function SameState(const AOther: INyxCollectionSelection): Boolean;
    property Key: TNyxCollectionRef read GetKey;
    property Count: Integer read GetCount;
    property DataRevision: Integer read GetDataRevision;
    property Focus: TNyxItemRef read GetFocus;
    property Anchor: TNyxItemRef read GetAnchor;
    property Snapshot: TNyxCollectionSelectionSnapshot read GetSnapshot;
  end;

{ Validate the complete candidate before publication: scope, existence, duplicate
  membership, focus and anchor. Undefined focus/anchor references mean absent.
  The snapshot copies identities only; retaining it cannot retain dataset rows.
  The caller supplies canonical dataset order, as the live view does. }
function NewNyxCollectionSelection(const AData: INyxCollectionSnapshot;
  const AItems: array of TNyxItemRef; const AFocus, AAnchor: TNyxItemRef): INyxCollectionSelection;

implementation

uses SysUtils;

type
  TSelection = class(TInterfacedObject, INyxCollectionSelection)
  private
    FKey: TNyxCollectionRef;
    FIDs: array of TNyxText;
    FIndex: array of Integer;
    FFocus: TNyxItemRef;
    FAnchor: TNyxItemRef;
    FDataRevision: Integer;
    function Slot(const AID: TNyxText): Integer;
  public
    constructor Create(const AData: INyxCollectionSnapshot;
      const AItems: array of TNyxItemRef; const AFocus, AAnchor: TNyxItemRef);
    function GetKey: TNyxCollectionRef;
    function GetCount: Integer;
    function GetDataRevision: Integer;
    function GetFocus: TNyxItemRef;
    function GetAnchor: TNyxItemRef;
    function GetSnapshot: TNyxCollectionSelectionSnapshot;
    function Contains(const AItem: TNyxItemRef): Boolean;
    function ItemAt(AIndex: Integer): TNyxItemRef;
    function SameState(const AOther: INyxCollectionSelection): Boolean;
  end;

function SelectionSlot(const AID: TNyxText; const AIDs: array of TNyxText;
  const AIndex: array of Integer): Integer;
var
  LHash: Integer;
  LIndex: Integer;
begin
  LHash := 0;
  for LIndex := 1 to Length(AID) do
  begin
    { Bounded arithmetic works under checked FPC and JavaScript exact integers.
      Each target indexes its own text representation; equality remains exact. }
    LHash := (LHash * 31 + Ord(AID[LIndex])) mod 1048573;
  end;
  Result := LHash mod Length(AIndex);
  while (AIndex[Result] >= 0) and (AIDs[AIndex[Result]] <> AID) do
  begin
    Result := (Result + 1) mod Length(AIndex);
  end;
end;

function TSelection.Slot(const AID: TNyxText): Integer;
begin
  Result := SelectionSlot(AID, FIDs, FIndex);
end;

constructor TSelection.Create(const AData: INyxCollectionSnapshot;
  const AItems: array of TNyxItemRef; const AFocus, AAnchor: TNyxItemRef);
var
  LIndex: Integer;
  LSlot: Integer;
begin
  inherited Create;

  if AData = nil then
  begin
    raise ENyxCollection.Create('Selection requires an admitted dataset');
  end;

  if Length(AItems) > AData.Count then
  begin
    raise ENyxCollection.Create('Selection exceeds the admitted dataset');
  end;
  FKey := AData.Key;
  FDataRevision := AData.Revision;
  SetLength(FIDs, Length(AItems));
  SetLength(FIndex, Length(AItems) * 2 + 1);
  for LIndex := 0 to Length(FIndex) - 1 do
  begin
    FIndex[LIndex] := -1;
  end;
  for LIndex := 0 to Length(AItems) - 1 do
  begin

    if not AData.Has(AItems[LIndex]) then
    begin
      raise ENyxCollection.Create('Selected item is outside the admitted dataset');
    end;
    LSlot := Slot(AItems[LIndex].ID);

    if FIndex[LSlot] >= 0 then
    begin
      raise ENyxCollection.Create('Selection contains a duplicate item');
    end;
    FIDs[LIndex] := AItems[LIndex].ID;
    FIndex[LSlot] := LIndex;
  end;

  if AFocus.Defined then
  begin

    if not AData.Has(AFocus) then
    begin
      raise ENyxCollection.Create('Selection focus is outside the admitted dataset');
    end;
    FFocus := NyxItem(FKey, AFocus.ID);
  end;

  if AAnchor.Defined then
  begin

    if not AData.Has(AAnchor) then
    begin
      raise ENyxCollection.Create('Selection anchor is outside the admitted dataset');
    end;
    FAnchor := NyxItem(FKey, AAnchor.ID);
  end;
end;

function TSelection.GetKey: TNyxCollectionRef;
begin
  Result := FKey;
end;

function TSelection.GetCount: Integer;
begin
  Result := Length(FIDs);
end;

function TSelection.GetDataRevision: Integer;
begin
  Result := FDataRevision;
end;

function TSelection.GetFocus: TNyxItemRef;
begin
  Result := FFocus;
end;

function TSelection.GetAnchor: TNyxItemRef;
begin
  Result := FAnchor;
end;

function TSelection.GetSnapshot: TNyxCollectionSelectionSnapshot;
begin
  Result.FMarker := 'collection-selection-1';
  Result.FKey := FKey;
  Result.FIDs := FIDs;
  Result.FIndex := FIndex;
  Result.FFocus := FFocus;
  Result.FAnchor := FAnchor;
  Result.FDataRevision := FDataRevision;
end;

function TNyxCollectionSelectionSnapshot.GetDefined: Boolean;
begin
  Result := FMarker = 'collection-selection-1';
end;

function TNyxCollectionSelectionSnapshot.GetCount: Integer;
begin
  Result := Length(FIDs);
end;

function TNyxCollectionSelectionSnapshot.Contains(const AItem: TNyxItemRef): Boolean;
begin

  if not Defined or (AItem.Collection.Name <> FKey.Name) then
  begin
    raise ENyxCollection.Create('Selection item belongs to an absent or different scope');
  end;
  Result := FIndex[SelectionSlot(AItem.ID, FIDs, FIndex)] >= 0;
end;

function TNyxCollectionSelectionSnapshot.ItemAt(AIndex: Integer): TNyxItemRef;
begin

  if not Defined or (AIndex < 0) or (AIndex >= Length(FIDs)) then
  begin
    raise ENyxCollection.Create('Selection item index is out of range');
  end;
  Result := NyxItem(FKey, FIDs[AIndex]);
end;

function TSelection.Contains(const AItem: TNyxItemRef): Boolean;
begin

  if AItem.Collection.Name <> FKey.Name then
  begin
    raise ENyxCollection.Create('Selection item belongs to another collection');
  end;
  Result := FIndex[Slot(AItem.ID)] >= 0;
end;

function TSelection.ItemAt(AIndex: Integer): TNyxItemRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FIDs)) then
  begin
    raise ENyxCollection.Create('Selection item index is out of range');
  end;
  Result := NyxItem(FKey, FIDs[AIndex]);
end;

function SameReference(const ALeft, ARight: TNyxItemRef): Boolean;
begin
  Result := ALeft.Defined = ARight.Defined;

  if Result and ALeft.Defined then
  begin
    Result := (ALeft.Collection.Name = ARight.Collection.Name) and
      (ALeft.ID = ARight.ID);
  end;
end;

function TNyxCollectionSelectionSnapshot.SameState(
  const AOther: TNyxCollectionSelectionSnapshot): Boolean;
var
  LIndex: Integer;
begin
  Result := Defined and AOther.Defined and (AOther.Key.Name = FKey.Name) and
    (AOther.Count = Length(FIDs));

  if not Result then
  begin
    Exit;
  end;
  Result := SameReference(FFocus, AOther.Focus) and SameReference(FAnchor, AOther.Anchor);
  for LIndex := 0 to Length(FIDs) - 1 do
  begin

    if not Result then
    begin
      Exit;
    end;
    Result := AOther.Contains(NyxItem(FKey, FIDs[LIndex]));
  end;
end;

function TSelection.SameState(const AOther: INyxCollectionSelection): Boolean;
begin
  Result := (AOther <> nil) and GetSnapshot.SameState(AOther.Snapshot);
end;

function NewNyxCollectionSelection(const AData: INyxCollectionSnapshot;
  const AItems: array of TNyxItemRef; const AFocus, AAnchor: TNyxItemRef): INyxCollectionSelection;
begin
  Result := TSelection.Create(AData, AItems, AFocus, AAnchor);
end;

end.
