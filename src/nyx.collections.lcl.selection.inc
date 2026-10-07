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

{ Native selection gestures remain behind the LCL adapter. Canonical Nyx key
  callbacks execute first and may consume the key or dispose this attachment.
  Every widget callback retains the mount call frame and rechecks connection.
  Row membership is owned by the view, not by native grid rectangles. }

procedure TNativeMount.GridMouseDown(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LKeepAlive: INyxCollectionMount;
begin
  LKeepAlive := Self as INyxCollectionMount;
  FGridShift := AShift;

  if Assigned(FPreviousMouse) then
  begin
    FPreviousMouse(Sender, AButton, AShift, AX, AY);
  end;
end;

procedure TNativeMount.PrepareGrid(Sender: TObject; AColumn, ARow: Integer;
  AState: TGridDrawState);
var
  LKeepAlive: INyxCollectionMount;
begin
  LKeepAlive := Self as INyxCollectionMount;

  if Assigned(FPreviousPrepare) then
  begin
    FPreviousPrepare(Sender, AColumn, ARow, AState);
  end;

  if not GetConnected or (FView.Spec.SelectionMode <> nsmMultiple) or
    (ARow < 1) or (ARow > FView.Snapshot.Count) then
  begin
    Exit;
  end;

  if FView.Selection.Contains(FView.Snapshot.ItemAt(ARow - 1).Ref) then
  begin
    FGrid.Canvas.Brush.Color := clHighlight;
    FGrid.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    FGrid.Canvas.Brush.Color := FGrid.Color;
    FGrid.Canvas.Font.Color := FGrid.Font.Color;
  end;
end;

function TNativeMount.VisibleOrder: TNyxItemRefs;
var
  LIndex: Integer;
begin

  if FTree <> nil then
  begin
    Exit(NyxTreeHierarchy(FView).VisibleItems);
  end;
  SetLength(Result, FView.Snapshot.Count);
  for LIndex := 0 to Length(Result) - 1 do
  begin
    Result[LIndex] := FView.Snapshot.ItemAt(LIndex).Ref;
  end;
end;

procedure TNativeMount.KeyDown(Sender: TObject; var AKey: Word; AShift: TShiftState);
var
  LKeepAlive: INyxCollectionMount;
  LOrder: TNyxItemRefs;
  LIndex: Integer;
  LTarget: Integer;
  LFocus: TNyxItemRef;
  LNode: TTreeNode;
  LMove: TNyxGridMove;
  LDestination: TNyxGridCell;
  LGridMove: Boolean;
  LColumn: Integer;
begin
  LKeepAlive := Self as INyxCollectionMount;

  { Only later UTF-8 text for this admitted key may drive search. Virtual-key
    codes are not text; AltGr/IME processing and consumed callbacks stay owned
    by the host. Shift still permits ordinary uppercase characters. }
  FRejectTextKey := (AShift * [ssAlt, ssCtrl, ssMeta, ssSuper, ssAltGr] <> []) or
    (AKey = $E5) or (AKey = $E7);

  if Assigned(FPreviousKey) then
  begin
    FPreviousKey(Sender, AKey, AShift);
  end;
  FConsumedTextKey := AKey = 0;
  FRejectTextKey := FRejectTextKey or FConsumedTextKey;

  if FRejectTextKey or (AKey in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT,
    VK_HOME, VK_END, VK_SPACE, VK_RETURN, VK_TAB, VK_ESCAPE]) then
  begin
    ResetTypeAhead;
  end;

  if not GetConnected then
  begin
    AKey := 0;
    Exit;
  end;

  if FUpdating or not FEnabled or (AKey = 0) or (ssAlt in AShift) then
  begin
    Exit;
  end;
  { An active cell editor owns its text arrows. }

  if (FGrid <> nil) and FGrid.EditorMode then
  begin

    if AKey = VK_F2 then
    begin
      { Ending editing uses the widget's ordinary typed validation callback.
        Escape remains the native discard path and keeps its cell position. }
      AKey := 0;
      FGrid.EditorMode := False;
    end;
    Exit;
  end;

  if (AKey = VK_A) and (ssCtrl in AShift) and
    (FView.Spec.SelectionMode = nsmMultiple) then
  begin
    AKey := 0;
    SelectAll;
    Exit;
  end;
  LOrder := VisibleOrder;

  if Length(LOrder) = 0 then
  begin
    Exit;
  end;
  LFocus := FView.Selection.Focus;
  LTarget := -1;
  for LIndex := 0 to Length(LOrder) - 1 do
  begin

    if LFocus.Defined and (LOrder[LIndex].ID = LFocus.ID) then
    begin
      LTarget := LIndex;
      Break;
    end;
  end;

  if FGrid <> nil then
  begin

    if (AKey = VK_F2) or (AKey = VK_RETURN) then
    begin
      { Read-only columns have no editing authority. Enter/F2 addresses this
        exact cell, never scans for a different editable column in the row. }
      AKey := 0;

      if not FReadOnly and (FGrid.Row > 0) and
        (FView.Spec.ColumnAt(FGrid.Col).Mode = cmEditable) then
      begin
        FGrid.EditorMode := True;
      end;
      Exit;
    end;
    LGridMove := True;
    case AKey of
      VK_LEFT:
        begin
          LMove := ngmLeft;
        end;
      VK_RIGHT:
        begin
          LMove := ngmRight;
        end;
      VK_UP:
        begin
          LMove := ngmUp;
        end;
      VK_DOWN:
        begin
          LMove := ngmDown;
        end;
      VK_HOME:
        begin
          LMove := ngmRowStart;

          if ssCtrl in AShift then
          begin
            LMove := ngmFirstCell;
          end;
        end;
      VK_END:
        begin
          LMove := ngmRowEnd;

          if ssCtrl in AShift then
          begin
            LMove := ngmLastCell;
          end;
        end;
      else
        begin
          LGridMove := False;
        end;
    end;

    if LGridMove then
    begin

      if LTarget < 0 then
      begin
        LTarget := 0;
      end;
      LColumn := FGrid.Col;
      LDestination := MoveNyxGridCell(LTarget, LColumn, Length(LOrder), FView.Spec.Count, LMove);
      AKey := 0;
      FUpdating := True;
      try
        { The physical column and row membership are separate. Prevent the
          widget's selection slot from interpreting a horizontal move as a
          mouse gesture or publishing a second row-selection notification. }
        FGrid.Col := LDestination.Column;
      finally
        FUpdating := False;
      end;

      if LMove in [ngmLeft, ngmRight, ngmRowStart, ngmRowEnd] then
      begin
        Select(LOrder[LDestination.Row], nsaFocus);
      end
      else if ssShift in AShift then
      begin
        SelectRange(LOrder[LDestination.Row], LOrder, ssCtrl in AShift);
      end
      else if ssCtrl in AShift then
      begin
        Select(LOrder[LDestination.Row], nsaFocus);
      end
      else
      begin
        Select(LOrder[LDestination.Row]);
      end;
      Exit;
    end;
  end;

  if (FTree <> nil) and LFocus.Defined and (AKey in [VK_LEFT, VK_RIGHT]) then
  begin
    LNode := FNodes[FView.Snapshot.IndexOf(LFocus)];

    if (AKey = VK_RIGHT) and LNode.HasChildren and not LNode.Expanded then
    begin
      AKey := 0;
      NyxTreeHierarchy(FView).SetExpanded(LFocus, True);
      Exit;
    end;

    if (AKey = VK_LEFT) and LNode.Expanded then
    begin
      AKey := 0;
      NyxTreeHierarchy(FView).SetExpanded(LFocus, False);
      Exit;
    end;

    if (AKey = VK_LEFT) and (LNode.Parent <> nil) then
    begin
      LFocus := FView.Snapshot.ItemAt(NodeIndex(LNode.Parent)).Ref;
      AKey := 0;
      Select(LFocus);
      Exit;
    end;

    if (AKey = VK_RIGHT) and LNode.HasChildren then
    begin
      AKey := VK_DOWN;
    end
    else
    begin
      AKey := 0;
      Exit;
    end;
  end;
  case AKey of
    VK_UP:
      begin

        if LTarget > 0 then
        begin
          Dec(LTarget);
        end;
      end;
    VK_DOWN:
      begin

        if LTarget < Length(LOrder) - 1 then
        begin
          Inc(LTarget);
        end;
      end;
    VK_HOME:
      begin
        LTarget := 0;
      end;
    VK_END:
      begin
        LTarget := Length(LOrder) - 1;
      end;
    VK_SPACE:
      begin

        if LTarget < 0 then
        begin
          LTarget := 0;
        end;
        AKey := 0;

        if ssShift in AShift then
        begin
          SelectRange(LOrder[LTarget], LOrder, ssCtrl in AShift);
        end
        else
        begin
          Select(LOrder[LTarget], nsaToggle);
        end;
        Exit;
      end;
    else
      begin
        Exit;
      end;
  end;

  if LTarget < 0 then
  begin
    LTarget := 0;
  end;
  AKey := 0;

  if ssShift in AShift then
  begin
    SelectRange(LOrder[LTarget], LOrder, ssCtrl in AShift);
  end
  else if ssCtrl in AShift then
  begin
    Select(LOrder[LTarget], nsaFocus);
  end
  else
  begin
    Select(LOrder[LTarget]);
  end;
end;

procedure TNativeMount.UTF8KeyPress(Sender: TObject; var AKey: TUTF8Char);
var
  LKeepAlive: INyxCollectionMount;
  LOrder: TNyxItemRefs;
  LFocus: TNyxItemRef;
  LIndex: Integer;
  LTarget: Integer;
  LText: TNyxText;
begin
  LKeepAlive := Self as INyxCollectionMount;

  if Assigned(FPreviousUTF8) then
  begin
    FPreviousUTF8(Sender, AKey);
  end;

  if not GetConnected then
  begin
    AKey := '';
    Exit;
  end;

  if FConsumedTextKey then
  begin
    { A queued native character may follow a consumed virtual key. Suppress
      that text as well, before the widgetset can perform its own search. }
    AKey := '';
    ResetTypeAhead;
    Exit;
  end;

  if FUpdating or FRejectTextKey or ((FTree <> nil) and FTree.IsEditing) then
  begin
    ResetTypeAhead;
    Exit;
  end;
  LText := TNyxText(AKey);

  if not NyxTypeAheadCharacter(LText) then
  begin
    ResetTypeAhead;
    Exit;
  end;
  { Suppress the widgetset's separate locale-dependent search, even without a
    match. One portable default owns this admitted text gesture. }
  AKey := '';

  if not FEnabled then
  begin
    ResetTypeAhead;
    Exit;
  end;
  LOrder := VisibleOrder;
  LFocus := FView.Selection.Focus;
  LTarget := -1;
  for LIndex := 0 to Length(LOrder) - 1 do
  begin

    if LFocus.Defined and (LOrder[LIndex].ID = LFocus.ID) then
    begin
      LTarget := LIndex;
      Break;
    end;
  end;
  LTarget := FindTypeAhead(LText, Double(GetTickCount64), LOrder, LTarget);

  if LTarget >= 0 then
  begin
    { Selection refresh reveals the native item. The callback may detach this
      mount: selection is the last borrowed operation in this retained frame. }
    Select(LOrder[LTarget]);
  end;
end;

procedure TNativeMount.TreeSelectionChanged(Sender: TObject);
var
  LKeepAlive: INyxCollectionMount;
  LItems: TNyxItemRefs;
  LFocus: TNyxItemRef;
  LAnchor: TNyxItemRef;
  LIndex: Integer;
  LCount: Integer;
begin

  if FUpdating or not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if FEnabled and (FView.Spec.SelectionMode = nsmMultiple) then
  begin
    SetLength(LItems, FView.Snapshot.Count);
    LCount := 0;
    for LIndex := 0 to Length(FNodes) - 1 do
    begin

      if FNodes[LIndex].MultiSelected then
      begin
        LItems[LCount] := FView.Snapshot.ItemAt(LIndex).Ref;
        Inc(LCount);
      end;
    end;
    SetLength(LItems, LCount);
    LFocus := Default(TNyxItemRef);
    LIndex := NodeIndex(FTree.Selected);

    if LIndex >= 0 then
    begin
      LFocus := FView.Snapshot.ItemAt(LIndex).Ref;
    end;
    LAnchor := FView.Selection.Anchor;

    if not (ssShift in GetKeyShiftState) then
    begin
      LAnchor := LFocus;
    end;
    SetSelection(LItems, LFocus, LAnchor);
  end
  else if not FEnabled then
  begin
    Refresh;
  end;

  if GetConnected and Assigned(FPreviousTreeSelection) then
  begin
    FPreviousTreeSelection(Sender);
  end;
end;
