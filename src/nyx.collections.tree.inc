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

{ Implementation belongs to nyx.collections.view. Tree disclosure is runtime
  view state, like selection, rather than persisted authored store content. }

function NyxTreeHierarchy(const AView: INyxCollectionView): INyxTreeHierarchy;
begin

  if (AView = nil) or (AView.Projection <> cpTree) or
    not Supports(AView, INyxTreeHierarchy, Result) then
  begin
    raise ENyxCollection.Create('Collection view has no tree hierarchy capability');
  end;
end;

procedure TView.BuildHierarchy;
var
  LIndex: Integer;
  LParent: Integer;
begin
  FFirstChild := nil;
  FNextSibling := nil;

  if FProjection <> cpTree then
  begin
    Exit;
  end;
  { A sentinel at Count owns roots. Reverse insertion preserves result order
    among siblings even when a dataset places its child before its parent. }
  SetLength(FFirstChild, FSnapshot.Count + 1);
  SetLength(FNextSibling, FSnapshot.Count);
  for LIndex := 0 to FSnapshot.Count do
  begin
    FFirstChild[LIndex] := -1;
  end;
  for LIndex := FSnapshot.Count - 1 downto 0 do
  begin
    LParent := FParents[LIndex];

    if LParent < 0 then
    begin
      LParent := FSnapshot.Count;
    end;
    FNextSibling[LIndex] := FFirstChild[LParent];
    FFirstChild[LParent] := LIndex;
  end;
end;

function TView.TreeIndex(const AItem: TNyxItemRef): Integer;
begin

  if FProjection <> cpTree then
  begin
    raise ENyxCollection.Create('Tree disclosure requires a tree projection');
  end;
  Result := FSnapshot.IndexOf(AItem);

  if Result < 0 then
  begin
    raise ENyxCollection.Create('Tree disclosure requires an item in the query result');
  end;
end;

function TView.HasChildren(const AItem: TNyxItemRef): Boolean;
begin
  Result := FFirstChild[TreeIndex(AItem)] >= 0;
end;

function TView.GetCommandSerial: Integer;
begin
  Result := FTreeCommandSerial;
end;

procedure TView.AdvanceTreeCommand;
begin

  if FProjection <> cpTree then
  begin
    Exit;
  end;

  if FTreeCommandSerial = MaxInt then
  begin
    raise ENyxCollection.Create('Tree command serial is exhausted');
  end;
  Inc(FTreeCommandSerial);
end;

function TView.IsExpanded(const AItem: TNyxItemRef): Boolean;
begin
  Result := HasChildren(AItem) and FExpanded[FSource.IndexOf(AItem)];
end;

function TView.RevealAncestors(const AFocus: TNyxItemRef): Boolean;
var
  LParent: Integer;
  LSource: Integer;
begin
  Result := False;

  if (FProjection <> cpTree) or not AFocus.Defined then
  begin
    Exit;
  end;
  LParent := FParents[TreeIndex(AFocus)];
  while LParent >= 0 do
  begin
    LSource := FSource.IndexOf(FSnapshot.ItemAt(LParent).Ref);

    if not FExpanded[LSource] then
    begin
      FExpanded[LSource] := True;
      Result := True;
    end;
    LParent := FParents[LParent];
  end;
end;

procedure TView.ReconcileTreeFocus;
var
  LIndex: Integer;
  LVisible: Integer;
  LItems: TNyxItemRefs;
begin

  if (FProjection <> cpTree) or not FSelection.Focus.Defined then
  begin
    Exit;
  end;
  LIndex := FSnapshot.IndexOf(FSelection.Focus);
  LVisible := LIndex;
  { Choose the highest closed ancestor: returning to an intermediate closed
    parent would still park the sole keyboard cursor in an invisible subtree. }
  LIndex := FParents[LIndex];
  while LIndex >= 0 do
  begin

    if not FExpanded[FSource.IndexOf(FSnapshot.ItemAt(LIndex).Ref)] then
    begin
      LVisible := LIndex;
    end;
    LIndex := FParents[LIndex];
  end;

  if LVisible = FSnapshot.IndexOf(FSelection.Focus) then
  begin
    Exit;
  end;
  SetLength(LItems, FSelection.Count);
  for LIndex := 0 to FSelection.Count - 1 do
  begin
    LItems[LIndex] := FSelection.ItemAt(LIndex);
  end;
  FSelection := NewNyxCollectionSelection(FSource, LItems,
    FSnapshot.ItemAt(LVisible).Ref, FSelection.Anchor);
end;

function TView.SetExpanded(const AItem: TNyxItemRef;
  AExpanded: Boolean): INyxTreeHierarchy;
var
  LIndex: Integer;
  LHasChildren: Boolean;
begin
  Writable;
  Result := Self as INyxTreeHierarchy;
  LHasChildren := HasChildren(AItem);
  AdvanceTreeCommand;

  if not LHasChildren then
  begin
    Exit;
  end;
  LIndex := FSource.IndexOf(AItem);

  if FExpanded[LIndex] = AExpanded then
  begin
    Exit;
  end;
  FExpanded[LIndex] := AExpanded;
  ReconcileTreeFocus;
  Notify(nil);
end;

function TView.ExpandAll: INyxTreeHierarchy;
var
  LIndex: Integer;
  LSource: Integer;
  LChanged: Boolean;
begin
  Writable;
  Result := Self as INyxTreeHierarchy;

  if FProjection <> cpTree then
  begin
    raise ENyxCollection.Create('Tree disclosure requires a tree projection');
  end;
  AdvanceTreeCommand;
  LChanged := False;
  for LIndex := 0 to FSnapshot.Count - 1 do
  begin

    if FFirstChild[LIndex] >= 0 then
    begin
      LSource := FSource.IndexOf(FSnapshot.ItemAt(LIndex).Ref);
      LChanged := LChanged or not FExpanded[LSource];
      FExpanded[LSource] := True;
    end;
  end;

  if LChanged then
  begin
    Notify(nil);
  end;
end;

function TView.CollapseAll: INyxTreeHierarchy;
var
  LIndex: Integer;
  LChanged: Boolean;
begin
  Writable;
  Result := Self as INyxTreeHierarchy;

  if FProjection <> cpTree then
  begin
    raise ENyxCollection.Create('Tree disclosure requires a tree projection');
  end;
  AdvanceTreeCommand;
  LChanged := False;
  { Also close parked branches, so removing a query cannot resurrect disclosure
    that the application's CollapseAll command deliberately cleared. }
  for LIndex := 0 to Length(FExpanded) - 1 do
  begin
    LChanged := LChanged or FExpanded[LIndex];
    FExpanded[LIndex] := False;
  end;

  if LChanged then
  begin
    ReconcileTreeFocus;
    Notify(nil);
  end;
end;

function TView.VisibleItems: TNyxItemRefs;
var
  LIndex: Integer;
  LCount: Integer;
begin
  Result := nil;

  if FProjection <> cpTree then
  begin
    raise ENyxCollection.Create('Tree disclosure requires a tree projection');
  end;
  SetLength(Result, FSnapshot.Count);
  LCount := 0;
  LIndex := FFirstChild[FSnapshot.Count];
  while LIndex >= 0 do
  begin
    Result[LCount] := FSnapshot.ItemAt(LIndex).Ref;
    Inc(LCount);

    if (FFirstChild[LIndex] >= 0) and
      FExpanded[FSource.IndexOf(Result[LCount - 1])] then
    begin
      LIndex := FFirstChild[LIndex];
    end
    else
    begin
      { Climb each exhausted branch once. Depth never consumes call-stack space,
        and closed descendants are never scanned to determine visibility. }
      while (LIndex >= 0) and (FNextSibling[LIndex] < 0) do
      begin
        LIndex := FParents[LIndex];
      end;

      if LIndex >= 0 then
      begin
        LIndex := FNextSibling[LIndex];
      end;
    end;
  end;
  SetLength(Result, LCount);
end;
