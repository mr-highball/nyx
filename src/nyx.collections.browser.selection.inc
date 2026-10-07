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

{ The browser consumes only its admitted collection gestures. Exact key
  callbacks run before this delegated listener. Focus and membership stay
  separate, and collapsed tree descendants are excluded from range navigation. }

function TBrowserMount.VisibleOrder: TNyxItemRefs;
var
  LRows: TJSNodeList;
  LElement: TJSHTMLElement;
  LIndex: Integer;
  LCount: Integer;
begin
  { Every admitted table row participates in navigation/ranges, including rows
    outside its realized viewport. Trees use the same portable disclosed order
    as LCL; layout boxes and queued DOM toggle events do not own navigation. }

  if FView.Projection = cpTree then
  begin
    Exit(NyxTreeHierarchy(FView).VisibleItems);
  end;

  if FView.Projection = cpTable then
  begin
    SetLength(Result, FView.Snapshot.Count);
    for LIndex := 0 to Length(Result) - 1 do
    begin
      Result[LIndex] := FView.Snapshot.ItemAt(LIndex).Ref;
    end;
    Exit;
  end;
  LRows := FHost.querySelectorAll('[data-nyx-item]');
  SetLength(Result, LRows.length);
  LCount := 0;
  for LIndex := 0 to LRows.length - 1 do
  begin
    LElement := TJSHTMLElement(LRows[LIndex]);

    { Chromium may retain layout boxes beneath a closed details element. The
      disclosure state is authoritative for tree navigation/ranges; rectangles
      alone would admit visually hidden descendants after a synchronous collapse. }

    if ((Length(LElement.getClientRects) > 0) or
      not document.documentElement.contains(FHost)) and
      ((FView.Projection <> cpTree) or
        (LElement.parentElement.closest('details[data-nyx-item]:not([open])') = nil)) then
    begin
      Result[LCount] := NyxItem(FView.Spec.Key, LElement.getAttribute('data-nyx-item'));
      Inc(LCount);
    end;
  end;
  SetLength(Result, LCount);
end;

procedure TBrowserMount.Gesture(const AItem: TNyxItemRef; AShift, AControl: Boolean;
  AFocusOnly: Boolean);
begin

  if AShift then
  begin
    SelectRange(AItem, VisibleOrder, AControl);
  end
  else if AFocusOnly then
  begin
    Select(AItem, nsaFocus);
  end
  else if AControl then
  begin
    Select(AItem, nsaToggle);
  end
  else
  begin
    Select(AItem);
  end;
end;

{ Grid navigation addresses the current data cell, not the first editor in a
  row. Capture physical objects before selection callbacks can retire this mount.
  Column position is runtime presentation; document defaults are never rewritten. }
function TBrowserMount.GridKey(AEvent: TJSKeyboardEvent;
  const AOrder: TNyxItemRefs; ARow: Integer): Boolean;
var
  LRow: Integer;
  LColumn: Integer;
  LCell: TJSHTMLElement;
  LFocus: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LMove: TNyxGridMove;
  LDestination: TNyxGridCell;
  LOptions: TFocusOptions;
  LControl: Boolean;
begin
  Result := True;
  LRow := FView.Snapshot.IndexOf(AOrder[ARow]);

  if (LRow < 0) or (FRows[LRow] = nil) then
  begin
    Exit;
  end;
  LColumn := FFocusedColumn;
  LCell := TJSHTMLElement(TJSHTMLElement(AEvent.target).closest('[role=gridcell]'));

  if LCell <> nil then
  begin
    LColumn := StrToIntDef(LCell.getAttribute('data-nyx-column'), -1);

    if (LColumn < 0) or (LColumn >= Length(FRows[LRow].FCells)) or
      (FRows[LRow].FCells[LColumn] <> LCell) then
    begin
      Exit;
    end;
  end;
  FFocusedColumn := LColumn;
  LControl := AEvent.ctrlKey or AEvent.metaKey;

  if (AEvent.key = 'Enter') or (AEvent.key = 'F2') then
  begin
    AEvent.preventDefault;
    LInput := FRows[LRow].FInputs[LColumn];

    if (LInput = nil) or LInput.disabled then
    begin
      Exit;
    end;
    Select(AOrder[ARow], nsaFocus);

    if GetConnected and FHost.contains(LInput) then
    begin
      Refresh;

      if GetConnected and FHost.contains(LInput) then
      begin
        LOptions := TFocusOptions.new;
        LOptions.preventScroll := True;
        TFocusable(LInput).focus(LOptions);
      end;
    end;
    Exit;
  end;
  case AEvent.key of
    'ArrowLeft':
      begin
        LMove := ngmLeft;
      end;
    'ArrowRight':
      begin
        LMove := ngmRight;
      end;
    'ArrowUp':
      begin
        LMove := ngmUp;
      end;
    'ArrowDown':
      begin
        LMove := ngmDown;
      end;
    'Home':
      begin
        LMove := ngmRowStart;

        if LControl then
        begin
          LMove := ngmFirstCell;
        end;
      end;
    'End':
      begin
        LMove := ngmRowEnd;

        if LControl then
        begin
          LMove := ngmLastCell;
        end;
      end;
    else
      begin
        Exit(False);
      end;
  end;
  AEvent.preventDefault;
  LDestination := MoveNyxGridCell(ARow, LColumn, Length(AOrder), FView.Spec.Count, LMove);
  FFocusedColumn := LDestination.Column;
  LRow := FView.Snapshot.IndexOf(AOrder[LDestination.Row]);
  EnsureRow(LRow);

  if not GetConnected then
  begin
    Exit;
  end;
  LRow := FView.Snapshot.IndexOf(AOrder[LDestination.Row]);

  if not GetConnected or (LRow < 0) or (LRow >= Length(FRows)) or
    (FRows[LRow] = nil) or (FRows[LRow].FRef.ID <> AOrder[LDestination.Row].ID) then
  begin
    Exit;
  end;
  LFocus := FRows[LRow].FCells[LDestination.Column];

  if LMove in [ngmLeft, ngmRight, ngmRowStart, ngmRowEnd] then
  begin
    Select(AOrder[LDestination.Row], nsaFocus);
  end
  else
  begin
    Gesture(AOrder[LDestination.Row], AEvent.shiftKey, LControl,
      LControl and not AEvent.shiftKey);
  end;

  if GetConnected and FHost.contains(LFocus) then
  begin
    Refresh;
    { Navigation reveals the cell using normal nearest scrolling. A surviving
      editor owns caret movement instead and takes the separate path below. }

    if GetConnected and FHost.contains(LFocus) then
    begin
      LFocus.focus;
    end;
  end;
end;

function TBrowserMount.Key(AEvent: TJSKeyboardEvent): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LOrder: TNyxItemRefs;
  LTarget: Integer;
  LIndex: Integer;
  LRow: Integer;
  LElement: TJSHTMLElement;
  LFocus: TJSHTMLElement;
  LOptions: TFocusOptions;
  LControl: Boolean;
  LKey: String;
  LColumn: Integer;
  LNextColumn: Integer;
  LInput: TJSHTMLInputElement;
  LItem: TNyxItemRef;
  LRowElement: TJSHTMLElement;
begin
  Result := True;

  if not GetConnected or not FEnabled or AEvent.defaultPrevented or
    AEvent.isComposing or AEvent.altKey then
  begin
    ResetTypeAhead;
    Exit;
  end;
  LElement := TJSHTMLElement(AEvent.target);
  LKeepAlive := Self as INyxCollectionMount;
  { A cell editor owns its text navigation and selection. A list option cannot
    contain interactive children; tables use grid/row/gridcell semantics. }

  if (LowerCase(LElement.tagName) = 'input') or
    (LowerCase(LElement.tagName) = 'textarea') or
    (LowerCase(LElement.tagName) = 'select') then
  begin
    { Editors own caret navigation. Escape discards only their uncommitted
      physical draft, then returns to the cell; Tab traverses eligible columns
      within that row. Native LCL retains its standard cell-editor behavior. }
    ResetTypeAhead;

    if (AEvent.key = 'Escape') or (AEvent.key = 'F2') or (AEvent.key = 'Tab') then
    begin
      LRowElement := TJSHTMLElement(LElement.closest('[data-nyx-item]'));

      if LRowElement = nil then
      begin
        Exit;
      end;
      LItem := NyxItem(FView.Spec.Key, LRowElement.getAttribute('data-nyx-item'));
      LRow := FView.Snapshot.IndexOf(LItem);
      LColumn := StrToIntDef(LElement.getAttribute('data-nyx-column'), -1);

      if (LRow < 0) or (FRows[LRow] = nil) or (LColumn < 0) or
        (LColumn >= Length(FRows[LRow].FInputs)) or
        (FRows[LRow].FInputs[LColumn] <> LElement) then
      begin
        Exit;
      end;
      LInput := FRows[LRow].FInputs[LColumn];

      if FView.Projection = cpTable then
      begin
        FFocusedColumn := LColumn;
      end;

      if (AEvent.key = 'Escape') or (AEvent.key = 'F2') then
      begin
        AEvent.preventDefault;

        if AEvent.key = 'Escape' then
        begin

          if LInput._type = 'checkbox' then
          begin
            LInput.checked := FView.CellText(LItem, LColumn) = 'true';
          end
          else
          begin
            LInput.value := FView.CellText(LItem, LColumn);
          end;
        end;

        if FView.Projection = cpTable then
        begin
          LRowElement := FRows[LRow].FCells[LColumn];
        end;
        Refresh;

        if GetConnected and FHost.contains(LRowElement) then
        begin
          LOptions := TFocusOptions.new;
          LOptions.preventScroll := True;
          TFocusable(LRowElement).focus(LOptions);
        end;
        Exit;
      end;
      LNextColumn := LColumn + 1;

      if AEvent.shiftKey then
      begin
        LNextColumn := LColumn - 1;
      end;
      while (LNextColumn >= 0) and (LNextColumn < Length(FRows[LRow].FInputs)) do
      begin
        LInput := FRows[LRow].FInputs[LNextColumn];

        if (LInput <> nil) and not LInput.disabled then
        begin
          AEvent.preventDefault;

          if FView.Projection = cpTable then
          begin
            FFocusedColumn := LNextColumn;
            Refresh;
          end;

          if GetConnected and FHost.contains(LInput) then
          begin
            LOptions := TFocusOptions.new;
            LOptions.preventScroll := True;
            TFocusable(LInput).focus(LOptions);
          end;
          Exit;
        end;

        if AEvent.shiftKey then
        begin
          Dec(LNextColumn);
        end
        else
        begin
          Inc(LNextColumn);
        end;
      end;
      { At either boundary, ordinary browser Tab leaves editing rather than
        creating a keyboard trap. It must retain its native default action. }
    end;
    Exit;
  end;
  LControl := AEvent.ctrlKey or AEvent.metaKey;

  if LControl and (LowerCase(AEvent.key) = 'a') and
    (FView.Spec.SelectionMode = nsmMultiple) then
  begin
    AEvent.preventDefault;
    SelectAll;
    Exit;
  end;
  LOrder := VisibleOrder;

  if Length(LOrder) = 0 then
  begin
    Exit;
  end;
  LTarget := -1;
  for LIndex := 0 to Length(LOrder) - 1 do
  begin
    LRow := FView.Snapshot.IndexOf(LOrder[LIndex]);

    if (FRows[LRow] <> nil) and ((LElement = FRows[LRow].FElement) or
      FRows[LRow].FElement.contains(LElement)) then
    begin
      { For a nested tree, the deepest actual row wins. }
      LTarget := LIndex;
    end;
  end;

  if LTarget < 0 then
  begin
    Exit;
  end;
  LKey := AEvent.key;

  if (FView.Projection in [cpList, cpTree]) and not LControl and
    NyxTypeAheadCharacter(LKey) then
  begin
    AEvent.preventDefault;
    LTarget := FindTypeAhead(LKey, window.performance.now, LOrder, LTarget);

    if LTarget >= 0 then
    begin
      LFocus := FRows[FView.Snapshot.IndexOf(LOrder[LTarget])].FElement;
      Select(LOrder[LTarget]);

      if GetConnected and FHost.contains(LFocus) then
      begin
        { Focus reveals the matching row using the host's normal nearest-scroll
          behavior. Capturing the managed DOM object before publication avoids
          touching a row released by a selection callback. }
        LFocus.focus;
      end;
    end;
    Exit;
  end;
  ResetTypeAhead;

  if (FView.Projection = cpTable) and GridKey(AEvent, LOrder, LTarget) then
  begin
    Exit;
  end;

  if (LKey = 'F2') or (LKey = 'Enter') then
  begin
    LRow := FView.Snapshot.IndexOf(LOrder[LTarget]);
    for LColumn := 0 to Length(FRows[LRow].FInputs) - 1 do
    begin
      LInput := FRows[LRow].FInputs[LColumn];

      if (LInput <> nil) and not LInput.disabled then
      begin
        AEvent.preventDefault;
        LItem := LOrder[LTarget];
        Select(LItem, nsaFocus);

        if GetConnected and FHost.contains(LInput) then
        begin
          { Selection callbacks can remove a row or navigate away. Capture the
            DOM editor first and recheck ownership before changing its focus. }
          LOptions := TFocusOptions.new;
          LOptions.preventScroll := True;
          TFocusable(LInput).focus(LOptions);
        end;
        Exit;
      end;
    end;
  end;

  if (FView.Projection = cpTree) and
    ((LKey = 'ArrowLeft') or (LKey = 'ArrowRight')) then
  begin
    LRow := FView.Snapshot.IndexOf(LOrder[LTarget]);
    LElement := FRows[LRow].FElement;
    AEvent.preventDefault;

    if (LKey = 'ArrowRight') and (FRows[LRow].FChildren.children.length > 0) and
      not TNyxDetails(LElement).open then
    begin
      NyxTreeHierarchy(FView).SetExpanded(FView.Snapshot.ItemAt(LRow).Ref, True);
      Exit;
    end;

    if (LKey = 'ArrowLeft') and TNyxDetails(LElement).open then
    begin
      NyxTreeHierarchy(FView).SetExpanded(FView.Snapshot.ItemAt(LRow).Ref, False);
      Exit;
    end;

    if (LKey = 'ArrowLeft') and (FView.ParentIndex(LRow) >= 0) then
    begin
      LRow := FView.ParentIndex(LRow);
      LFocus := FRows[LRow].FElement;
      Gesture(FView.Snapshot.ItemAt(LRow).Ref, False, False);

      if GetConnected then
      begin
        LOptions := TFocusOptions.new;
        LOptions.preventScroll := True;
        TFocusable(LFocus).focus(LOptions);
      end;
      Exit;
    end;

    if (LKey = 'ArrowRight') and (FRows[LRow].FChildren.children.length > 0) then
    begin
      LKey := 'ArrowDown';
    end
    else
    begin
      Exit;
    end;
  end;
  case LKey of
    'ArrowUp':
      begin

        if LTarget > 0 then
        begin
          Dec(LTarget);
        end;
      end;
    'ArrowDown':
      begin

        if LTarget < Length(LOrder) - 1 then
        begin
          Inc(LTarget);
        end;
      end;
    'Home':
      begin
        LTarget := 0;
      end;
    'End':
      begin
        LTarget := Length(LOrder) - 1;
      end;
    ' ', 'Enter':
      begin
        { The focused item remains the target. Space toggles in Multiple;
          Enter retains the established activate/select behavior. }
      end;
    else
      begin
        Exit;
      end;
  end;
  AEvent.preventDefault;
  LRow := FView.Snapshot.IndexOf(LOrder[LTarget]);
  EnsureRow(LRow);

  if not GetConnected then
  begin
    Exit;
  end;
  LRow := FView.Snapshot.IndexOf(LOrder[LTarget]);

  if not GetConnected or (LRow < 0) or (LRow >= Length(FRows)) or
    (FRows[LRow] = nil) or (FRows[LRow].FRef.ID <> LOrder[LTarget].ID) then
  begin
    Exit;
  end;
  LFocus := FRows[LRow].FElement;

  if FView.Projection = cpTable then
  begin
    LFocus := FRows[LRow].FCells[FFocusedColumn];
  end;

  if (LKey = ' ') and not AEvent.shiftKey then
  begin
    Select(LOrder[LTarget], nsaToggle);
  end
  else
  begin
    Gesture(LOrder[LTarget], AEvent.shiftKey,
      LControl, LControl and not AEvent.shiftKey and (LKey <> 'Enter'));
  end;

  if GetConnected then
  begin
    { Publication may have disposed every row; only the captured managed DOM
      object and retained attachment are consulted after the callback. }
    LOptions := TFocusOptions.new;
    LOptions.preventScroll := True;
    TFocusable(LFocus).focus(LOptions);
  end;
end;
