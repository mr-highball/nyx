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

{ The viewport owns physical rows, not hierarchy or selection. Realized branches
  retain their ordinary details/summary and nested ARIA groups. Missing disclosed
  intervals contribute numerical, accessibility-hidden spacers; closed branches
  contribute neither spacers nor interactive descendants. This preserves native
  browser disclosure and editing without flattening the semantic tree. }
procedure TBrowserMount.RenderTreeWindow;
type
  TScrollPosition = record
    Element: TJSHTMLElement;
    Top: Double;
    Left: Double;
  end;
var
  LData: INyxCollectionSnapshot;
  LRows: INyxTreeRows;
  LNext: array of TBrowserRow;
  LNeeded: array of Integer;
  LWork: array of Integer;
  LResident: array of Integer;
  LPositions: array of Integer;
  LScroll: array of TScrollPosition;
  LWindow: TNyxCollectionRowWindow;
  LFocused: TJSHTMLElement;
  LFocusRow: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LNextFocus: TJSHTMLElement;
  LTabStop: TJSHTMLElement;
  LDestination: TJSHTMLElement;
  LAncestor: TJSHTMLElement;
  LAnchor: TJSNode;
  LOptions: TFocusOptions;
  LViewportScroll: TTreeViewportScroll;
  LRow: TNyxTreeRow;
  LIndex: Integer;
  LSource: Integer;
  LOrdinal: Integer;
  LParent: Integer;
  LParentPosition: Integer;
  LPosition: Integer;
  LCursor: Integer;
  LPrevious: Integer;
  LRootPosition: Integer;
  LEntry: Integer;
  LRealize: Boolean;
  LMeasured: Boolean;
  LChanged: Boolean;
  LReveal: TNyxTreeRevealRequest;
  LCurrentReveal: TNyxTreeRevealRequest;
  LOffset: Double;
  LExtent: Double;
  LHeight: Double;
  LWindowTop: Double;
  LWindowLeft: Double;
  LCaretStart: NativeInt;
  LCaretEnd: NativeInt;
  LCaretDirection: String;

  function Pixels(AValue: Double): String;
  var
    LFormat: TFormatSettings;
  begin
    LFormat := FormatSettings;
    LFormat.DecimalSeparator := '.';
    Result := FloatToStr(AValue, LFormat) + 'px';
  end;

  { Sorted ordinals allow a bounded union of viewport/focus rows and their
    ancestors. Never allocate a dataset-sized mark array on every host scroll. }
  function NeededPosition(AOrdinal: Integer): Integer;
  var
    LLow: Integer;
    LHigh: Integer;
    LMiddle: Integer;
  begin
    LLow := 0;
    LHigh := Length(LNeeded);
    while LLow < LHigh do
    begin
      LMiddle := LLow + (LHigh - LLow) div 2;

      if LNeeded[LMiddle] < AOrdinal then
      begin
        LLow := LMiddle + 1;
      end
      else
      begin
        LHigh := LMiddle;
      end;
    end;
    Result := LLow;
  end;

  procedure NeedBranch(AOrdinal: Integer);
  var
    LInsert: Integer;
    LMove: Integer;
    LCurrent: TNyxTreeRow;
  begin
    while AOrdinal >= 0 do
    begin
      LInsert := NeededPosition(AOrdinal);

      if (LInsert < Length(LNeeded)) and (LNeeded[LInsert] = AOrdinal) then
      begin
        Exit;
      end;
      SetLength(LNeeded, Length(LNeeded) + 1);
      for LMove := High(LNeeded) downto LInsert + 1 do
      begin
        LNeeded[LMove] := LNeeded[LMove - 1];
      end;
      LNeeded[LInsert] := AOrdinal;
      LCurrent := LRows.RowAt(AOrdinal);
      AOrdinal := LRows.IndexOf(LCurrent.Parent);
    end;
  end;

  procedure AddWork(AIndex: Integer);
  begin

    if AIndex >= 0 then
    begin
      SetLength(LWork, Length(LWork) + 1);
      LWork[High(LWork)] := AIndex;
    end;
  end;

  procedure SortWork(AFirst, ALast: Integer);
  var
    LLeft: Integer;
    LRight: Integer;
    LPivot: Integer;
    LSwap: Integer;
  begin
    LLeft := AFirst;
    LRight := ALast;
    LPivot := LWork[AFirst + (ALast - AFirst) div 2];
    repeat
      while LWork[LLeft] < LPivot do
      begin
        Inc(LLeft);
      end;
      while LWork[LRight] > LPivot do
      begin
        Dec(LRight);
      end;

      if LLeft <= LRight then
      begin
        LSwap := LWork[LLeft];
        LWork[LLeft] := LWork[LRight];
        LWork[LRight] := LSwap;
        Inc(LLeft);
        Dec(LRight);
      end;
    until LLeft > LRight;

    if AFirst < LRight then
    begin
      SortWork(AFirst, LRight);
    end;

    if LLeft < ALast then
    begin
      SortWork(LLeft, ALast);
    end;
  end;

  procedure InsertAt(AElement: TJSHTMLElement);
  begin
    LAnchor := nil;

    if LPosition < LDestination.children.length then
    begin
      LAnchor := LDestination.children[LPosition];
    end;

    if AElement <> LAnchor then
    begin
      LDestination.insertBefore(AElement, LAnchor);
    end;
    Inc(LPosition);
  end;

  procedure AddGap(AFirst, AAfter: Integer);
  var
    LSpacer: TJSHTMLElement;
  begin

    if AAfter <= AFirst then
    begin
      Exit;
    end;
    LSpacer := NewElement('div');
    LSpacer.setAttribute('aria-hidden', 'true');
    LSpacer.setAttribute('data-nyx-row-spacer', 'true');
    LSpacer.style.setProperty('height', Pixels(
      FGeometry.OffsetAt(AAfter) - FGeometry.OffsetAt(AFirst)));
    LSpacer.style.setProperty('pointer-events', 'none');
    SetLength(FSpacers, Length(FSpacers) + 1);
    FSpacers[High(FSpacers)] := LSpacer;
    InsertAt(LSpacer);
  end;

begin
  LData := FView.Snapshot;

  if FRendered <> LData then
  begin
    RecoverRowDrafts(LData);
  end;

  if not InputLifetimeCurrent then
  begin
    Exit;
  end;
  LRows := TreeRows;
  LCurrentReveal := CurrentTreeReveal;

  if (FTreeRevealCorrectionSerial > 0) and (LCurrentReveal.Serial <> FTreeRevealCorrectionSerial) then
  begin
    FRevealItem := Default(TNyxItemRef);
    FTreeRevealCorrectionSerial := 0;
  end;
  LReveal := PendingTreeReveal;

  if LReveal.Item.Defined and (LRows.IndexOf(LReveal.Item) >= 0) then
  begin
    FRevealItem := LReveal.Item;
    FTreeRevealCorrectionSerial := LReveal.Serial;
  end;
  LChanged := FTreeGeometryRows <> LRows;

  if FEditingItem.Defined and
    (not FEnabled or FReadOnly or (LRows.IndexOf(FEditingItem) < 0)) then
  begin
    { Closing/filtering a branch revokes active editing, while an unfinished
      physical value may still retain its independent row control offscreen. }
    FEditingItem := Default(TNyxItemRef);
  end;

  if LChanged then
  begin
    FGeometry.Reset(LRows.Count, 32);
    FTreeGeometryRows := LRows;
  end;
  LFocused := TJSHTMLElement(document.activeElement);
  LFocusRow := nil;
  LInput := nil;

  if FHost.contains(LFocused) then
  begin
    LFocusRow := TJSHTMLElement(LFocused.closest('[data-nyx-item]'));

    if (LFocused is TJSHTMLInputElement) and
      (TJSHTMLInputElement(LFocused)._type <> 'checkbox') then
    begin
      LInput := TJSHTMLInputElement(LFocused);
      LCaretStart := LInput.selectionStart;
      LCaretEnd := LInput.selectionEnd;
      LCaretDirection := LInput.selectionDirection;
    end;
  end
  else
  begin
    LFocused := nil;
  end;
  RowViewport(LOffset, LExtent);
  LWindow := FGeometry.Window(LOffset, LExtent, 6);
  for LIndex := LWindow.First to LWindow.AfterLast - 1 do
  begin
    NeedBranch(LIndex);
  end;
  LEntry := LRows.IndexOf(FView.Selection.Focus);

  if (LEntry < 0) and (LRows.Count > 0) then
  begin
    LEntry := 0;
  end;
  NeedBranch(LEntry);
  NeedBranch(LRows.IndexOf(FForcedItem));

  if LFocusRow <> nil then
  begin
    NeedBranch(LRows.IndexOf(NyxItem(LData.Key,
      LFocusRow.getAttribute('data-nyx-item'))));
  end;
  { Preserve every scroll plane before hierarchy relocation or spacer removal
    can synchronously clamp it. Focus restoration below never scrolls the page. }
  LWindowTop := TScrollableWindow(window).scrollYPixels;
  LWindowLeft := TScrollableWindow(window).scrollXPixels;
  LAncestor := FHost;
  while LAncestor <> nil do
  begin
    SetLength(LScroll, Length(LScroll) + 1);
    LScroll[High(LScroll)].Element := LAncestor;
    LScroll[High(LScroll)].Top := TScrollableElement(LAncestor).scrollTopPixels;
    LScroll[High(LScroll)].Left := TScrollableElement(LAncestor).scrollLeftPixels;
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;
  FBody.style.setProperty('overflow-anchor', 'none');
  for LIndex := 0 to High(FSpacers) do
  begin
    FSpacers[LIndex].remove;
  end;
  FSpacers := nil;

  if FRendered = LData then
  begin
    LNext := FRows;
    LWork := Copy(FResidentRows, 0, Length(FResidentRows));
  end
  else
  begin
    SetLength(LNext, LData.Count);
    for LIndex := 0 to High(FResidentRows) do
    begin
      LPrevious := FResidentRows[LIndex];
      LSource := LData.IndexOf(FRows[LPrevious].FRef);
      { Flatten the old graph before any parent is freed or swapped. Surviving
        descendant controls/listeners remain independently owned by row slots. }
      FBody.appendChild(FRows[LPrevious].FElement);

      if (LSource >= 0) and not FRows[LPrevious].FIdentityRetired then
      begin
        LNext[LSource] := FRows[LPrevious];
        AddWork(LSource);
      end
      else
      begin
        FRows[LPrevious].Free;
      end;
    end;
  end;
  for LIndex := 0 to High(LNeeded) do
  begin
    AddWork(LRows.RowAt(LNeeded[LIndex]).SourceIndex);
  end;

  if Length(LWork) > 1 then
  begin
    SortWork(0, High(LWork));
  end;
  { Detach all departing faces before freeing any branch. A hidden child draft
    must not remain physically owned by a departing ancestor's DOM group. }
  for LIndex := 0 to High(LWork) do
  begin
    LSource := LWork[LIndex];
    LOrdinal := LRows.IndexOf(LData.ItemAt(LSource).Ref);
    LPosition := NeededPosition(LOrdinal);
    LRealize := (LOrdinal >= 0) and (LPosition < Length(LNeeded)) and
      (LNeeded[LPosition] = LOrdinal);

    if not LRealize and (LNext[LSource] <> nil) then
    begin
      LNext[LSource].FElement.remove;
    end;
  end;
  for LIndex := 0 to High(LWork) do
  begin
    LSource := LWork[LIndex];

    if (LIndex > 0) and (LSource = LWork[LIndex - 1]) then
    begin
      Continue;
    end;
    LOrdinal := LRows.IndexOf(LData.ItemAt(LSource).Ref);
    LPosition := NeededPosition(LOrdinal);
    LRealize := (LOrdinal >= 0) and (LPosition < Length(LNeeded)) and
      (LNeeded[LPosition] = LOrdinal);

    if LRealize and (LNext[LSource] = nil) then
    begin
      LNext[LSource] := TBrowserRow.Create(Self, LData.ItemAt(LSource).Ref);
    end;

    if LNext[LSource] <> nil then
    begin
      LNext[LSource].Sync(FRefreshPlan.ValuesChanged(LSource));

      if not LRealize and not LNext[LSource].HasDraft then
      begin
        LNext[LSource].Free;
        LNext[LSource] := nil;
      end
      else
      begin
        SetLength(LResident, Length(LResident) + 1);
        LResident[High(LResident)] := LSource;
      end;
    end;
  end;
  FRows := LNext;
  FResidentRows := LResident;
  FRendered := LData;
  FForcedItem := Default(TNyxItemRef);
  LTabStop := nil;
  LCursor := 0;
  LRootPosition := 0;
  SetLength(LPositions, Length(LNeeded));
  for LIndex := 0 to High(LNeeded) do
  begin
    LOrdinal := LNeeded[LIndex];
    LRow := LRows.RowAt(LOrdinal);
    LSource := LRow.SourceIndex;
    LParent := LRows.IndexOf(LRow.Parent);
    LParentPosition := -1;
    LDestination := FBody;
    LPosition := LRootPosition;

    if LParent >= 0 then
    begin
      LParentPosition := NeededPosition(LParent);
      LDestination := FRows[LRows.RowAt(LParent).SourceIndex].FChildren;
      LPosition := LPositions[LParentPosition];
    end;
    AddGap(LCursor, LOrdinal);
    InsertAt(FRows[LSource].FElement);

    if LParentPosition >= 0 then
    begin
      LPositions[LParentPosition] := LPosition;
    end
    else
    begin
      LRootPosition := LPosition;
    end;
    LCursor := LOrdinal + 1;
    FRows[LSource].FElement.setAttribute('aria-level', IntToStr(LRow.Level));
    FRows[LSource].FElement.setAttribute('aria-posinset', IntToStr(LRow.Position));
    FRows[LSource].FElement.setAttribute('aria-setsize', IntToStr(LRow.Size));

    if LOrdinal = LEntry then
    begin
      LTabStop := FRows[LSource].FElement;
    end;
  end;
  LDestination := FBody;
  LPosition := LRootPosition;
  AddGap(LCursor, LRows.Count);
  LMeasured := False;
  for LIndex := 0 to High(LNeeded) do
  begin
    LOrdinal := LNeeded[LIndex];
    LSource := LRows.RowAt(LOrdinal).SourceIndex;
    { A branch's outer box includes descendants. Measure only its summary,
      otherwise nested child/spacer heights would be counted twice. }
    LHeight := FRows[LSource].FCells[0].offsetHeight;

    if (LHeight > 0) and (Abs(LHeight - FGeometry.HeightAt(LOrdinal)) > 0.25) then
    begin
      LMeasured := FGeometry.Measure(LOrdinal, LHeight) or LMeasured;
    end;
  end;
  for LIndex := 0 to High(LScroll) do
  begin
    TScrollableElement(LScroll[LIndex].Element).scrollTopPixels := LScroll[LIndex].Top;
    TScrollableElement(LScroll[LIndex].Element).scrollLeftPixels := LScroll[LIndex].Left;
  end;

  if (TScrollableWindow(window).scrollXPixels <> LWindowLeft) or
    (TScrollableWindow(window).scrollYPixels <> LWindowTop) then
  begin
    TScrollableWindow(window).scrollTo(LWindowLeft, LWindowTop);
  end;
  FHost.setAttribute('tabindex', '-1');

  if FEnabled then
  begin

    if LTabStop <> nil then
    begin
      LTabStop.setAttribute('tabindex', '0');
    end
    else
    begin
      FHost.setAttribute('tabindex', '0');
    end;
  end;

  if not FEnabled then
  begin
    FRevealItem := Default(TNyxItemRef);
  end;

  if FRevealItem.Defined then
  begin
    LSource := LData.IndexOf(FRevealItem);

    if (LSource >= 0) and (FRows[LSource] <> nil) and
      FHost.contains(FRows[LSource].FElement) then
    begin
      RevealListOption(FRows[LSource].FCells[0]);

      if FTreeRevealCorrectionSerial > 0 then
      begin
        { Explicit navigation may reveal through surrounding scroll containers
          as well as the Tree itself. Instant nearest movement has no later
          smooth-scroll animation which could outlive cancellation/navigation. }
        LViewportScroll := TTreeViewportScroll.new;
        LViewportScroll.behavior := 'instant';
        LViewportScroll.block := 'nearest';
        LViewportScroll.inline_ := 'nearest';
        FRows[LSource].FCells[0].scrollIntoView(LViewportScroll);

        if not InputLifetimeCurrent then
        begin
          Exit;
        end;
        FRevealScrollTop := TScrollableElement(FHost).scrollTopPixels;
        TreeRevealApplied(FTreeRevealCorrectionSerial);
      end;
    end;

    if (LRows.IndexOf(FRevealItem) < 0) or not LMeasured then
    begin
      FRevealItem := Default(TNyxItemRef);
      FTreeRevealCorrectionSerial := 0;
    end;
  end;

  if LMeasured then
  begin
    QueueWindow;
  end;
  LNextFocus := nil;

  if (LFocused <> nil) and FEnabled then
  begin
    LNextFocus := LTabStop;

    if FHost.contains(LFocused) and
      not ((LFocused is TJSHTMLInputElement) and TJSHTMLInputElement(LFocused).disabled) then
    begin
      LNextFocus := LFocused;
    end;

    if LNextFocus = nil then
    begin
      LNextFocus := FHost;
    end;
  end;

  if LReveal.Item.Defined and (LReveal.Focus = ntrfTake) and
    (LReveal.Serial = CurrentTreeReveal.Serial) then
  begin
    LSource := LData.IndexOf(LReveal.Item);

    if (LSource >= 0) and (FRows[LSource] <> nil) then
    begin
      LNextFocus := FRows[LSource].FCells[0];
    end;
  end;

  if (LNextFocus <> nil) and (document.activeElement <> LNextFocus) then
  begin
    LOptions := TFocusOptions.new;
    LOptions.preventScroll := True;
    TFocusable(LNextFocus).focus(LOptions);

    if GetConnected and (LInput <> nil) and (document.activeElement = LInput) then
    begin
      LInput.setSelectionRange(LCaretStart, LCaretEnd, LCaretDirection);
    end;
  end;
end;
