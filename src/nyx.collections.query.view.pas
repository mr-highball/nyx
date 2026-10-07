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
unit nyx.collections.query.view;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.collections, nyx.collections.query;

type
  TNyxQueryIndexes = array of Integer;

{ Build an immutable read projection. Source identity/order/revision never change.
  Empty parents means a flat projection; otherwise one parent index per source
  row, with -1 for roots. Invalid indexes/cycles refuse before publication. Tree
  filters include required ancestors; sorting compares siblings by the same
  stable multi-key ordering used for flat rows. Returned parents index the result.
  The result retains the source and forwards its retained logical DataBytes;
  hiding rows does not discount retained memory. Lookup uses the source index and
  an inverse map. Sorting uses cached scalar keys and a stable O(n log n) merge.
  An empty policy returns the original source, preserving compatible identity. }
function NyxQuerySnapshot(const ASource: INyxCollectionSnapshot;
  const AQuery: TNyxCollectionQuery; const AParents: TNyxQueryIndexes;
  out AResultParents: TNyxQueryIndexes): INyxCollectionSnapshot;

implementation

uses nyx.state;

type
  TQuerySnapshot = class(TInterfacedObject, INyxCollectionSnapshot)
  private
    FSource: INyxCollectionSnapshot;
    FRows: TNyxQueryIndexes;
    FInverse: TNyxQueryIndexes;
  public
    constructor Create(const ASource: INyxCollectionSnapshot;
      const ARows: TNyxQueryIndexes);
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

constructor TQuerySnapshot.Create(const ASource: INyxCollectionSnapshot;
  const ARows: TNyxQueryIndexes);
var
  LIndex: Integer;
begin
  inherited Create;
  FSource := ASource;
  SetLength(FRows, Length(ARows));
  SetLength(FInverse, ASource.Count);
  for LIndex := 0 to High(FInverse) do
  begin
    FInverse[LIndex] := -1;
  end;
  for LIndex := 0 to High(ARows) do
  begin
    FRows[LIndex] := ARows[LIndex];
    FInverse[ARows[LIndex]] := LIndex;
  end;
end;

function TQuerySnapshot.GetKey: TNyxCollectionRef;
begin
  Result := FSource.Key;
end;

function TQuerySnapshot.GetSchema: TNyxCollectionSchema;
begin
  Result := FSource.Schema;
end;

function TQuerySnapshot.GetCount: Integer;
begin
  Result := Length(FRows);
end;

function TQuerySnapshot.GetRevision: Integer;
begin
  Result := FSource.Revision;
end;

function TQuerySnapshot.GetDataBytes: Integer;
begin
  Result := FSource.DataBytes;
end;

function TQuerySnapshot.ItemAt(AIndex: Integer): TNyxCollectionItem;
begin

  if (AIndex < 0) or (AIndex >= Length(FRows)) then
  begin
    raise ENyxCollection.Create('Collection query row index is outside its range');
  end;
  Result := FSource.ItemAt(FRows[AIndex]);
end;

function TQuerySnapshot.Item(const ARef: TNyxItemRef): TNyxCollectionItem;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ARef);

  if LIndex < 0 then
  begin
    raise ENyxCollection.Create('Item is not in this collection query result');
  end;
  Result := ItemAt(LIndex);
end;

function TQuerySnapshot.IndexOf(const ARef: TNyxItemRef): Integer;
begin
  Result := FSource.IndexOf(ARef);

  if Result >= 0 then
  begin
    Result := FInverse[Result];
  end;
end;

function TQuerySnapshot.Has(const ARef: TNyxItemRef): Boolean;
begin
  Result := IndexOf(ARef) >= 0;
end;

procedure CheckQueryParents(const AParents: TNyxQueryIndexes; ACount: Integer);
var
  LColors: TNyxQueryIndexes;
  LIndex: Integer;
  LCursor: Integer;
begin

  if Length(AParents) = 0 then
  begin
    Exit;
  end;

  if Length(AParents) <> ACount then
  begin
    raise ENyxCollection.Create('Collection query requires one parent index per source row');
  end;
  SetLength(LColors, ACount);
  for LIndex := 0 to ACount - 1 do
  begin

    if (AParents[LIndex] < -1) or (AParents[LIndex] >= ACount) then
    begin
      raise ENyxCollection.Create('Collection query parent is outside the source');
    end;
  end;
  { Three colors qualify every chain once, including child-before-parent input. }
  for LIndex := 0 to ACount - 1 do
  begin
    LCursor := LIndex;
    while (LCursor >= 0) and (LColors[LCursor] = 0) do
    begin
      LColors[LCursor] := 1;
      LCursor := AParents[LCursor];
    end;

    if (LCursor >= 0) and (LColors[LCursor] = 1) then
    begin
      raise ENyxCollection.Create('Collection query hierarchy contains a cycle');
    end;
    LCursor := LIndex;
    while (LCursor >= 0) and (LColors[LCursor] = 1) do
    begin
      LColors[LCursor] := 2;
      LCursor := AParents[LCursor];
    end;
  end;
end;

function NyxQuerySnapshot(const ASource: INyxCollectionSnapshot;
  const AQuery: TNyxCollectionQuery; const AParents: TNyxQueryIndexes;
  out AResultParents: TNyxQueryIndexes): INyxCollectionSnapshot;
var
  LIncluded: array of Boolean;
  LRows: TNyxQueryIndexes;
  LTemporary: TNyxQueryIndexes;
  LInverse: TNyxQueryIndexes;
  LKeys: array of array of TNyxStateValue;
  LSorts: array of TNyxCollectionSort;
  LFilter: INyxCollectionPredicate;
  LIndex: Integer;
  LKey: Integer;
  LCount: Integer;
  LCursor: Integer;
  LWidth: Integer;
  LStart: Integer;
  LMiddle: Integer;
  LStop: Integer;
  LLeft: Integer;
  LRight: Integer;
  LWrite: Integer;

  function CompareRows(ALeft, ARight: Integer): Integer;
  var
    LSort: Integer;
  begin
    Result := 0;
    for LSort := 0 to High(LSorts) do
    begin
      Result := LSorts[LSort].Compare(LKeys[LSort][ALeft], LKeys[LSort][ARight]);

      if Result <> 0 then
      begin
        Exit;
      end;
    end;
  end;
begin

  if (ASource = nil) or (ASource.Count < 0) or
    (ASource.Count > NyxMaximumCollectionItems) then
  begin
    raise ENyxCollection.Create('Collection query requires a bounded source snapshot');
  end;
  AQuery.Validate(ASource.Schema);
  CheckQueryParents(AParents, ASource.Count);
  SetLength(AResultParents, Length(AParents));

  if not AQuery.Defined then
  begin
    for LIndex := 0 to High(AParents) do
    begin
      AResultParents[LIndex] := AParents[LIndex];
    end;
    Exit(ASource);
  end;
  SetLength(LIncluded, ASource.Count);
  LFilter := AQuery.Filter;
  for LIndex := 0 to ASource.Count - 1 do
  begin
    LIncluded[LIndex] := (LFilter = nil) or LFilter.Matches(ASource.ItemAt(LIndex));
  end;

  if Length(AParents) > 0 then
  begin
    for LIndex := 0 to ASource.Count - 1 do
    begin

      if LIncluded[LIndex] then
      begin
        LCursor := AParents[LIndex];
        while (LCursor >= 0) and not LIncluded[LCursor] do
        begin
          LIncluded[LCursor] := True;
          LCursor := AParents[LCursor];
        end;
      end;
    end;
  end;
  SetLength(LRows, ASource.Count);
  LCount := 0;
  for LIndex := 0 to ASource.Count - 1 do
  begin

    if LIncluded[LIndex] then
    begin
      LRows[LCount] := LIndex;
      Inc(LCount);
    end;
  end;
  SetLength(LRows, LCount);
  SetLength(LSorts, AQuery.SortCount);
  SetLength(LKeys, AQuery.SortCount);
  for LKey := 0 to AQuery.SortCount - 1 do
  begin
    LSorts[LKey] := AQuery.SortAt(LKey);
    SetLength(LKeys[LKey], ASource.Count);
    for LIndex := 0 to LCount - 1 do
    begin
      LKeys[LKey][LRows[LIndex]] := LSorts[LKey].Read(ASource.ItemAt(LRows[LIndex]));
    end;
  end;
  { Choose the left run on equal keys. Source-order ties survive every merge;
    descending ordering reverses comparisons, never reverses tied row groups. }
  SetLength(LTemporary, LCount);
  LWidth := 1;
  while (Length(LSorts) > 0) and (LWidth < LCount) do
  begin
    LStart := 0;
    while LStart < LCount do
    begin
      LMiddle := LStart + LWidth;

      if LMiddle > LCount then
      begin
        LMiddle := LCount;
      end;
      LStop := LMiddle + LWidth;

      if LStop > LCount then
      begin
        LStop := LCount;
      end;
      LLeft := LStart;
      LRight := LMiddle;
      for LWrite := LStart to LStop - 1 do
      begin

        if (LRight >= LStop) or ((LLeft < LMiddle) and
          (CompareRows(LRows[LLeft], LRows[LRight]) <= 0)) then
        begin
          LTemporary[LWrite] := LRows[LLeft];
          Inc(LLeft);
        end
        else
        begin
          LTemporary[LWrite] := LRows[LRight];
          Inc(LRight);
        end;
      end;
      LStart := LStop;
    end;
    for LIndex := 0 to LCount - 1 do
    begin
      LRows[LIndex] := LTemporary[LIndex];
    end;
    LWidth := LWidth * 2;
  end;
  SetLength(LInverse, ASource.Count);
  for LIndex := 0 to High(LInverse) do
  begin
    LInverse[LIndex] := -1;
  end;
  for LIndex := 0 to LCount - 1 do
  begin
    LInverse[LRows[LIndex]] := LIndex;
  end;
  SetLength(AResultParents, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    AResultParents[LIndex] := -1;

    if (Length(AParents) > 0) and (AParents[LRows[LIndex]] >= 0) then
    begin
      AResultParents[LIndex] := LInverse[AParents[LRows[LIndex]]];
    end;
  end;
  Result := TQuerySnapshot.Create(ASource, LRows);
end;

end.
