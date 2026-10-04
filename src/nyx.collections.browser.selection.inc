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
      physical draft, then returns to the row; Tab traverses eligible columns
      within that row. Native LCL retains its standard cell-editor behavior. }

    if (AEvent.key = 'Escape') or (AEvent.key = 'Tab') then
    begin
      LRowElement := TJSHTMLElement(LElement.closest('[data-nyx-item]'));

      if LRowElement = nil then
      begin
        Exit;
      end;
      LItem := NyxItem(FView.Spec.Key, LRowElement.getAttribute('data-nyx-item'));
      LRow := FView.Snapshot.IndexOf(LItem);
      LColumn := StrToIntDef(LElement.getAttribute('data-nyx-column'), -1);

      if (LRow < 0) or (LColumn < 0) or (LColumn >= Length(FRows[LRow].FInputs)) or
        (FRows[LRow].FInputs[LColumn] <> LElement) then
      begin
        Exit;
      end;
      LInput := FRows[LRow].FInputs[LColumn];

      if AEvent.key = 'Escape' then
      begin
        AEvent.preventDefault;

        if LInput._type = 'checkbox' then
        begin
          LInput.checked := FView.CellText(LItem, LColumn) = 'true';
        end
        else
        begin
          LInput.value := FView.CellText(LItem, LColumn);
        end;
        LOptions := TFocusOptions.new;
        LOptions.preventScroll := True;
        TFocusable(LRowElement).focus(LOptions);
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
          LOptions := TFocusOptions.new;
          LOptions.preventScroll := True;
          TFocusable(LInput).focus(LOptions);
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

    if (LElement = FRows[LRow].FElement) or
      FRows[LRow].FElement.contains(LElement) then
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
      TNyxDetails(LElement).open := True;
      FRows[LRow].Toggle(nil);
      Exit;
    end;

    if (LKey = 'ArrowLeft') and TNyxDetails(LElement).open then
    begin
      TNyxDetails(LElement).open := False;
      FRows[LRow].Toggle(nil);
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
  LFocus := FRows[LRow].FElement;

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
