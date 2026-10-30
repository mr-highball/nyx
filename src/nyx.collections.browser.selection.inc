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
  LTreeRows: INyxTreeRows;
begin
  { Every admitted table row participates in navigation/ranges, including rows
    outside its realized viewport. Trees use the same portable disclosed order
    as LCL; layout boxes and queued DOM toggle events do not own navigation. }

  if FView.Projection = cpTree then
  begin
    LTreeRows := TreeRows;
    SetLength(Result, LTreeRows.Count);
    for LIndex := 0 to Length(Result) - 1 do
    begin
      Result[LIndex] := LTreeRows.RowAt(LIndex).Item;
    end;
    Exit;
  end;

  if FView.Projection in [cpList, cpTable] then
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

  if not GetInputReady then
  begin
    Exit;
  end;

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
  LMove: TNyxGridMove;
  LDestination: TNyxGridCell;
  LControl: Boolean;
  LPageRows: Integer;
  LDisplayColumn: Integer;
  LOffset: Double;
  LExtent: Double;
begin
  Result := True;

  if DisplayColumnCount = 0 then
  begin
    Exit;
  end;
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
  LDisplayColumn := ColumnDisplayIndex(LColumn);

  if LDisplayColumn < 0 then
  begin
    LDisplayColumn := 0;
    LColumn := DisplayColumn(0);
  end;
  FFocusedColumn := LColumn;
  LControl := AEvent.ctrlKey or AEvent.metaKey;

  if (AEvent.key = 'Enter') or (AEvent.key = 'F2') then
  begin
    AEvent.preventDefault;
    EnterCellEditor(AOrder[ARow], LColumn);
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
    'PageUp':
      begin
        LMove := ngmPageUp;
      end;
    'PageDown':
      begin
        LMove := ngmPageDown;
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
  { Arrow keys are physical. Logical start/end pins and the complete display
    map stay direction-neutral; only this host key mapping observes RTL. }

  if FHasColumnLayout and
    (window.getComputedStyle(FHost).getPropertyValue('direction') = 'rtl') then
  begin

    if LMove = ngmLeft then
    begin
      LMove := ngmRight;
    end
    else if LMove = ngmRight then
    begin
      LMove := ngmLeft;
    end;
  end;
  LPageRows := 1;

  if LMove in [ngmPageUp, ngmPageDown] then
  begin
    { Use the same clipped, scaled data plane as virtualization. Overscan and
      retained offscreen drafts must not enlarge the page. One row overlaps
      consecutive pages; a viewport shorter than two rows still advances one. }
    RowViewport(LOffset, LExtent);
    LPageRows := Max(1, FGeometry.Window(LOffset, LExtent, 0).Count - 1);
  end;
  AEvent.preventDefault;
  LDestination := MoveNyxGridCell(ARow, LDisplayColumn, Length(AOrder), DisplayColumnCount,
    LMove, LPageRows);
  LDestination.Column := DisplayColumn(LDestination.Column);
  FFocusedColumn := LDestination.Column;
  LRow := FView.Snapshot.IndexOf(AOrder[LDestination.Row]);
  EnsureRow(LRow);

  if not GetInputReady then
  begin
    Exit;
  end;
  LRow := FView.Snapshot.IndexOf(AOrder[LDestination.Row]);

  if not GetInputReady or (LRow < 0) or (LRow >= Length(FRows)) or
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

  if GetInputReady and FHost.contains(LFocus) then
  begin
    Refresh;
    { Navigation reveals the cell using normal nearest scrolling. A surviving
      editor owns caret movement instead and takes the separate path below. }

    if GetInputReady and FHost.contains(LFocus) then
    begin
      LFocus.focus;
    end;
  end;
end;

function TBrowserMount.CommitCellEditor(AInput: TJSHTMLInputElement;
  const AItem: TNyxItemRef; AColumn: Integer): Boolean;
var
  LView: INyxCollectionView;
  LLifetime: INyxCollectionInputLifetime;
  LSource: INyxCollectionSnapshot;
  LColumn: TNyxCollectionColumn;
  LSelection: TNyxTextSelection;
  LText: TNyxText;
  LEpoch: Integer;
  LAccepted: Boolean;

  { All physical references must still belong to this exact mounted input,
    including after callbacks disable/re-enable or replace its enclosing view. }
  function OriginalEditor: Boolean;
  var
    LRow: Integer;
  begin
    Result := False;

    if not LLifetime.Ready or (LLifetime.Epoch <> LEpoch) or
      not GetInputReady or (FView <> LView) or FReadOnly or FEditsSuspended or
      FUpdating or FOpeningColumnManager or (FCellPresentationDepth > 0) or
      (LView.Spec.ColumnIndex(LColumn.Identity) <> AColumn) or
      not ColumnVisible(AColumn) then
    begin
      Exit;
    end;
    LRow := LView.Snapshot.IndexOf(AItem);

    if not LLifetime.Ready or (LLifetime.Epoch <> LEpoch) or
      (FView <> LView) or (LRow < 0) or (LRow > High(FRows)) or
      (FRows[LRow] = nil) or (AColumn > High(FRows[LRow].FInputs)) then
    begin
      Exit;
    end;
    Result := (FRows[LRow].FInputs[AColumn] = AInput) and
      FHost.contains(AInput) and not AInput.disabled and not AInput.readOnly;
  end;

begin
  Result := False;

  if not GetInputReady or (AInput = nil) or (AColumn < 0) or
    (AColumn >= FView.Spec.Count) then
  begin
    Exit;
  end;
  LView := FView;
  LLifetime := GetInputLifetime;
  LEpoch := LLifetime.Epoch;
  LColumn := LView.Spec.ColumnAt(AColumn);

  if not OriginalEditor or (LColumn.Mode <> cmEditable) then
  begin
    Exit;
  end;
  LSource := LView.Store.Snapshot;
  LText := AInput.value;
  LSelection := CaptureNyxBrowserSelection(AInput);

  if AInput._type = 'checkbox' then
  begin
    LText := 'false';

    if AInput.checked then
    begin
      LText := 'true';
    end;
  end;
  { Hiding a retained editor can synchronously dispatch change while Refresh's
    presentation shield is active. That shield correctly ignores layout noise;
    an intentional keyboard exit must instead use the ordinary typed edit path
    before hiding. Its callbacks may retire every row borrowed above. }
  LAccepted := EditCell(AItem, AColumn, LText);

  if not OriginalEditor then
  begin
    Exit;
  end;

  if not LAccepted then
  begin

    if (LView.Store.Snapshot <> LSource) or not OriginalEditor then
    begin
      Exit;
    end;
    { The ordinary edit path normalizes rejected display values. Keep this
      still-focused keyboard draft correctable, without restoring over a newer
      accepted source or a replacement control. Selection uses scalar bounds. }
    AInput.value := LText;

    if AInput._type = 'checkbox' then
    begin
      AInput.checked := LText = 'true';
    end;

    if LSelection.Defined then
    begin
      SelectNyxBrowserText(AInput, LSelection);
    end;
    AInput.setAttribute('aria-invalid', 'true');
    AInput.setCustomValidity(GetError);
    Exit;
  end;
  AInput.removeAttribute('aria-invalid');
  AInput.setCustomValidity('');
  Result := True;
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
  LNextDisplay, LColumnCount: Integer;
  LInput: TJSHTMLInputElement;
  LEditor: TJSHTMLInputElement;
  LPreviousDisplay: Integer;
  LItem: TNyxItemRef;
  LRowElement: TJSHTMLElement;
  LCopy: INyxBrowserColumnCommand;
begin
  Result := True;
  { Readonly caption feedback owns its own keyboard scrolling/text selection;
    it is never a data-cell navigation or Copy source. }

  if (FClipboardStatus <> nil) and (AEvent.target = FClipboardStatus) then
  begin
    Exit;
  end;
  LCopy := BeginRangeKey(AEvent);

  if LCopy = nil then
  begin
    LCopy := BeginClipboardKey(AEvent);
  end;

  if LCopy <> nil then
  begin
    try

      if LCopy.Current then
      begin
        LCopy.Prepare;
        LCopy.Complete(not AEvent.defaultPrevented);
      end;
    finally
      AEvent.preventDefault;
      AEvent.stopPropagation;
      LCopy.Retire;
    end;
    Exit;
  end;

  if not GetInputReady then
  begin
    AEvent.preventDefault;
    Exit;
  end;

  if not GetInputReady or not FEnabled or AEvent.defaultPrevented or
    AEvent.isComposing or AEvent.altKey or
    ((FHasColumnLayout or FObservesTablePresentation) and (FComposingInput <> nil)) then
  begin
    ResetTypeAhead;
    Exit;
  end;
  LElement := TJSHTMLElement(AEvent.target);

  if LElement.closest(ColumnResizeSelector) <> nil then
  begin
    Exit;
  end;
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

    if (AEvent.key = 'Escape') or (AEvent.key = 'F2') or
      (AEvent.key = 'Enter') or (AEvent.key = 'Tab') then
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
      LEditor := LInput;

      if FView.Projection = cpTable then
      begin
        FFocusedColumn := LColumn;
      end;

      if (AEvent.key = 'Escape') or (AEvent.key = 'F2') or (AEvent.key = 'Enter') then
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
          LInput.removeAttribute('aria-invalid');
          LInput.setCustomValidity('');
        end
        else
        begin

          if not CommitCellEditor(LEditor, LItem, LColumn) then
          begin
            Exit;
          end;
          LRow := FView.Snapshot.IndexOf(LItem);
        end;

        if FView.Projection in [cpTable, cpTree] then
        begin

          if FView.Projection = cpTable then
          begin
            LRowElement := FRows[LRow].FCells[LColumn];
          end;
          FEditingItem := Default(TNyxItemRef);
        end;
        Refresh;

        if GetInputReady and FHost.contains(LRowElement) then
        begin
          LOptions := TFocusOptions.new;
          LOptions.preventScroll := True;
          TFocusable(LRowElement).focus(LOptions);
        end;
        Exit;
      end;
      LNextDisplay := LColumn;
      LColumnCount := Length(FRows[LRow].FInputs);

      if FView.Projection = cpTable then
      begin
        LNextDisplay := ColumnDisplayIndex(LColumn);
        LColumnCount := DisplayColumnCount;
      end;
      LPreviousDisplay := LNextDisplay;

      if AEvent.shiftKey then
      begin
        Dec(LNextDisplay);
      end
      else
      begin
        Inc(LNextDisplay);
      end;
      while (LNextDisplay >= 0) and (LNextDisplay < LColumnCount) do
      begin
        LNextColumn := LNextDisplay;

        if FView.Projection = cpTable then
        begin
          LNextColumn := DisplayColumn(LNextDisplay);
        end;
        LInput := FRows[LRow].FInputs[LNextColumn];

        if (LInput <> nil) and not LInput.disabled and not LInput.readOnly then
        begin
          AEvent.preventDefault;

          if not CommitCellEditor(LEditor, LItem, LColumn) then
          begin
            Exit;
          end;
          LRow := FView.Snapshot.IndexOf(LItem);

          if (LRow < 0) or (LRow > High(FRows)) or (FRows[LRow] = nil) or
            (LNextColumn > High(FRows[LRow].FInputs)) or
            (FRows[LRow].FInputs[LNextColumn] <> LInput) or
            LInput.disabled or LInput.readOnly or
            ((FView.Projection = cpTable) and
              (not ColumnVisible(LNextColumn) or
                (ColumnDisplayIndex(LColumn) <> LPreviousDisplay) or
                (ColumnDisplayIndex(LNextColumn) <> LNextDisplay))) then
          begin
            Exit;
          end;

          if FView.Projection = cpTable then
          begin
            FFocusedColumn := LNextColumn;
            FEditingItem := LItem;
            FEditingColumn := LNextColumn;
            Refresh;
          end;

          if GetInputReady and FHost.contains(LInput) then
          begin
            LOptions := TFocusOptions.new;
            LOptions.preventScroll := True;
            TFocusable(LInput).focus(LOptions);
          end;
          Exit;
        end;

        if AEvent.shiftKey then
        begin
          Dec(LNextDisplay);
        end
        else
        begin
          Inc(LNextDisplay);
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
      { A logical type-ahead match can be outside the realized window. Prepare
        it before selection callbacks can retire the mount or its row objects. }
      LRow := FView.Snapshot.IndexOf(LOrder[LTarget]);
      EnsureRow(LRow);

      if not GetInputReady or (LRow < 0) or (FRows[LRow] = nil) then
      begin
        Exit;
      end;
      LFocus := FRows[FView.Snapshot.IndexOf(LOrder[LTarget])].FElement;
      Select(LOrder[LTarget]);

      if GetInputReady and FHost.contains(LFocus) then
      begin
        { Focus reveals the matching row using the host's normal nearest-scroll
          behavior. Capturing the managed DOM object before publication avoids
          touching a row released by a selection callback. }

        if FView.Projection in [cpList, cpTree] then
        begin
          FocusListOption(LFocus);
        end
        else
        begin
          LFocus.focus;
        end;
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

        if FView.Projection = cpTree then
        begin
          EnterCellEditor(LItem, LColumn);
          Exit;
        end;
        Select(LItem, nsaFocus);

        if GetInputReady and FHost.contains(LInput) then
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

    if (LKey = 'ArrowRight') and NyxTreeHierarchy(FView).HasChildren(LOrder[LTarget]) and
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

    if (LKey = 'ArrowRight') and NyxTreeHierarchy(FView).HasChildren(LOrder[LTarget]) then
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

  if not GetInputReady then
  begin
    Exit;
  end;
  LRow := FView.Snapshot.IndexOf(LOrder[LTarget]);

  if not GetInputReady or (LRow < 0) or (LRow >= Length(FRows)) or
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

    if FView.Projection in [cpList, cpTree] then
    begin
      { The realized logical destination must become visible, matching native
        list selection and normal nearest scrolling. Source refresh itself
        still restores its containing scroll offsets without moving focus. }
      FocusListOption(LFocus);
    end
    else
    begin
      LOptions := TFocusOptions.new;
      LOptions.preventScroll := True;
      TFocusable(LFocus).focus(LOptions);
    end;
  end;
end;
