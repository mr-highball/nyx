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
unit nyx.collections.tree.rows;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.collections, nyx.collections.view;

type
  { Copied disclosed row, independent of adapter layout and DOM/LCL types.
    Level and sibling Position/Size are one-based logical hierarchy metadata,
    never viewport counts. Parent is undefined for a root. SourceIndex addresses
    the retained query snapshot; Item retains its exact scoped identity. Leaf
    Expanded is always False. The default record is undefined, with zero fields. }
  TNyxTreeRow = record
  private
    FDefined: Boolean;
    FItem: TNyxItemRef;
    FParent: TNyxItemRef;
    FSourceIndex: Integer;
    FLevel: Integer;
    FPosition: Integer;
    FSize: Integer;
    FHasChildren: Boolean;
    FExpanded: Boolean;
  public
    property Defined: Boolean read FDefined;
    property Item: TNyxItemRef read FItem;
    property Parent: TNyxItemRef read FParent;
    property SourceIndex: Integer read FSourceIndex;
    property Level: Integer read FLevel;
    property Position: Integer read FPosition;
    property Size: Integer read FSize;
    property HasChildren: Boolean read FHasChildren;
    property Expanded: Boolean read FExpanded;
  end;

  { Immutable managed projection of one accepted disclosed preorder. Retains
    only its query snapshot and copied metadata; never a view/store/document,
    renderer, widget or subscription. Survives disclosure/query changes and
    view retirement. Snapshot.DataBytes retains full logical source accounting.

    CommandSerial is copied evidence, not a view identity. A consumer caches this
    projection only with its independently owned view and checks both snapshot
    identity and command serial there. Different views may have equal stamps. }
  INyxTreeRows = interface(IInterface)
    ['{611B0293-100D-4A2D-A2D8-482126536198}']
    function GetCount: Integer;
    function GetSnapshot: INyxCollectionSnapshot;
    function GetCommandSerial: Integer;
    { Zero-based disclosed ordinal. Invalid indices raise ENyxCollection. }
    function RowAt(AIndex: Integer): TNyxTreeRow;
    { Exact source-scoped lookup. Hidden, removed or foreign items return -1. }
    function IndexOf(const AItem: TNyxItemRef): Integer;
    property Count: Integer read GetCount;
    property Snapshot: INyxCollectionSnapshot read GetSnapshot;
    property CommandSerial: Integer read GetCommandSerial;
  end;

{ Read synchronously on the view's UI thread. Validate parent graph, disclosure
  flags and exact visible preorder before returning an independent projection.
  Alternative providers must expose the same coherent portable contract. Cycles,
  invalid parents/identities, inconsistent disclosure/order or a changed snapshot/
  command stamp raise ENyxCollection; no view mutation or publication occurs.
  Depth traversal is iterative. Construction is O(query rows + disclosed rows). }
function NyxTreeRows(const AView: INyxCollectionView): INyxTreeRows;

implementation

uses
  nyx.collections.selection;

type
  TTreeRows = class(TInterfacedObject, INyxTreeRows)
  private
    FSnapshot: INyxCollectionSnapshot;
    FCommandSerial: Integer;
    FRows: array of TNyxTreeRow;
    FInverse: array of Integer;
  public
    constructor Create(const AView: INyxCollectionView);
    function GetCount: Integer;
    function GetSnapshot: INyxCollectionSnapshot;
    function GetCommandSerial: Integer;
    function RowAt(AIndex: Integer): TNyxTreeRow;
    function IndexOf(const AItem: TNyxItemRef): Integer;
  end;

constructor TTreeRows.Create(const AView: INyxCollectionView);
var
  LHierarchy: INyxTreeHierarchy;
  LVisible: TNyxItemRefs;
  LParents: array of Integer;
  LLevels: array of Integer;
  LPositions: array of Integer;
  LSizes: array of Integer;
  LFirst: array of Integer;
  LNext: array of Integer;
  LPath: array of Integer;
  LColors: array of Byte;
  LExpanded: array of Boolean;
  LCount: Integer;
  LIndex: Integer;
  LParent: Integer;
  LCursor: Integer;
  LDepth: Integer;
  LPathCount: Integer;
  LOrdinal: Integer;
  LItem: TNyxItemRef;
begin
  inherited Create;
  LHierarchy := NyxTreeHierarchy(AView);
  FSnapshot := AView.Snapshot;
  FCommandSerial := LHierarchy.CommandSerial;

  if (FSnapshot = nil) or (FSnapshot.Count < 0) or
    (FSnapshot.Count = MaxInt) or (FCommandSerial < 0) then
  begin
    raise ENyxCollection.Create('Tree row projection requires an admitted snapshot and stamp');
  end;
  LCount := FSnapshot.Count;
  SetLength(LParents, LCount);
  SetLength(LLevels, LCount);
  SetLength(LPositions, LCount);
  SetLength(LSizes, LCount + 1);
  SetLength(LFirst, LCount + 1);
  SetLength(LNext, LCount);
  SetLength(LPath, LCount);
  SetLength(LColors, LCount);
  SetLength(LExpanded, LCount);
  SetLength(FInverse, LCount);
  for LIndex := 0 to LCount do
  begin
    LFirst[LIndex] := -1;
  end;
  for LIndex := 0 to LCount - 1 do
  begin
    LItem := FSnapshot.ItemAt(LIndex).Ref;

    if FSnapshot.IndexOf(LItem) <> LIndex then
    begin
      raise ENyxCollection.Create('Tree row snapshot identity/index is inconsistent');
    end;
    LParent := AView.ParentIndex(LIndex);

    if (LParent < -1) or (LParent >= LCount) or (LParent = LIndex) then
    begin
      raise ENyxCollection.Create('Tree row parent is outside the admitted graph');
    end;
    LParents[LIndex] := LParent;
    FInverse[LIndex] := -1;

    if LParent < 0 then
    begin
      LParent := LCount;
    end;
    Inc(LSizes[LParent]);
    LPositions[LIndex] := LSizes[LParent];
  end;
  for LIndex := LCount - 1 downto 0 do
  begin
    LParent := LParents[LIndex];

    if LParent < 0 then
    begin
      LParent := LCount;
    end;
    LNext[LIndex] := LFirst[LParent];
    LFirst[LParent] := LIndex;
  end;
  { A source may place children before parents. Cache resolved depth and color
    each path once, without recursion or repeated ancestor walks. }
  for LIndex := 0 to LCount - 1 do
  begin
    LCursor := LIndex;
    LPathCount := 0;
    while (LCursor >= 0) and (LColors[LCursor] = 0) do
    begin
      LColors[LCursor] := 1;
      LPath[LPathCount] := LCursor;
      Inc(LPathCount);
      LCursor := LParents[LCursor];
    end;

    if (LCursor >= 0) and (LColors[LCursor] = 1) then
    begin
      raise ENyxCollection.Create('Tree row parent graph contains a cycle');
    end;
    LDepth := 0;

    if LCursor >= 0 then
    begin
      LDepth := LLevels[LCursor];
    end;
    while LPathCount > 0 do
    begin
      Dec(LPathCount);
      LCursor := LPath[LPathCount];
      Inc(LDepth);
      LLevels[LCursor] := LDepth;
      LColors[LCursor] := 2;
    end;
    LItem := FSnapshot.ItemAt(LIndex).Ref;

    if LHierarchy.HasChildren(LItem) <> (LSizes[LIndex] > 0) then
    begin
      raise ENyxCollection.Create('Tree row disclosure differs from its parent graph');
    end;
    LExpanded[LIndex] := LHierarchy.IsExpanded(LItem);

    if LExpanded[LIndex] and (LSizes[LIndex] = 0) then
    begin
      raise ENyxCollection.Create('Tree row leaf cannot expose expanded state');
    end;
  end;
  LVisible := LHierarchy.VisibleItems;
  SetLength(FRows, Length(LVisible));
  LOrdinal := 0;
  LCursor := LFirst[LCount];
  while LCursor >= 0 do
  begin

    if (LOrdinal >= Length(LVisible)) or
      (FSnapshot.IndexOf(LVisible[LOrdinal]) <> LCursor) then
    begin
      raise ENyxCollection.Create('Tree rows require the exact disclosed preorder');
    end;
    FRows[LOrdinal].FDefined := True;
    FRows[LOrdinal].FItem := LVisible[LOrdinal];
    FRows[LOrdinal].FSourceIndex := LCursor;
    FRows[LOrdinal].FLevel := LLevels[LCursor];
    FRows[LOrdinal].FPosition := LPositions[LCursor];
    FRows[LOrdinal].FHasChildren := LSizes[LCursor] > 0;
    FRows[LOrdinal].FExpanded := LExpanded[LCursor];
    LParent := LParents[LCursor];

    if LParent < 0 then
    begin
      FRows[LOrdinal].FSize := LSizes[LCount];
    end
    else
    begin
      FRows[LOrdinal].FSize := LSizes[LParent];
      FRows[LOrdinal].FParent := FSnapshot.ItemAt(LParent).Ref;
    end;
    FInverse[LCursor] := LOrdinal;
    Inc(LOrdinal);

    if LExpanded[LCursor] and (LFirst[LCursor] >= 0) then
    begin
      LCursor := LFirst[LCursor];
    end
    else
    begin
      while (LCursor >= 0) and (LNext[LCursor] < 0) do
      begin
        LCursor := LParents[LCursor];
      end;

      if LCursor >= 0 then
      begin
        LCursor := LNext[LCursor];
      end;
    end;
  end;

  if (LOrdinal <> Length(LVisible)) or (AView.Snapshot <> FSnapshot) or
    (LHierarchy.CommandSerial <> FCommandSerial) then
  begin
    raise ENyxCollection.Create('Tree row read changed before projection admission');
  end;
end;

function TTreeRows.GetCount: Integer;
begin
  Result := Length(FRows);
end;

function TTreeRows.GetSnapshot: INyxCollectionSnapshot;
begin
  Result := FSnapshot;
end;

function TTreeRows.GetCommandSerial: Integer;
begin
  Result := FCommandSerial;
end;

function TTreeRows.RowAt(AIndex: Integer): TNyxTreeRow;
begin

  if (AIndex < 0) or (AIndex >= Length(FRows)) then
  begin
    raise ENyxCollection.Create('Tree row ordinal is outside the disclosed projection');
  end;
  Result := FRows[AIndex];
end;

function TTreeRows.IndexOf(const AItem: TNyxItemRef): Integer;
begin
  Result := -1;

  if not AItem.Defined then
  begin
    Exit;
  end;

  if AItem.Collection.Name <> FSnapshot.Key.Name then
  begin
    Exit;
  end;
  Result := FSnapshot.IndexOf(AItem);

  if Result >= 0 then
  begin
    Result := FInverse[Result];
  end;
end;

function NyxTreeRows(const AView: INyxCollectionView): INyxTreeRows;
begin
  Result := TTreeRows.Create(AView);
end;

end.
