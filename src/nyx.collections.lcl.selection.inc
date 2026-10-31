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

procedure TNativeMount.ArrangedMouseDown(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LKeepAlive: INyxCollectionMount;
  LGrid: TStringGrid;
  LColumn, LPhysical, LRow, LRevision: Integer;
  LResize: TNativeColumnLayoutRequest;
  LResizeLease: IInterface;
  LAction: TNativeColumnLayoutAction;
  LDoubleStamp: Integer;
  LBounds: TRect;
  LPoint: TPoint;
  LBoundary: Boolean;
begin

  if FHasColumnLayout and (not GetInputReady or ColumnInputBusy) then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LResize := nil;

  if FEditsSuspended then
  begin
    Exit;
  end;
  LGrid := InputGrid(Sender);

  if LGrid = nil then
  begin
    Exit;
  end;
  LRevision := FArrangedRevision;
  LGrid.MouseToCell(AX, AY, LPhysical, LRow);
  LColumn := CanonicalColumn(LGrid, LPhysical);
  FGridShift := AShift;
  FQueryPressActive := False;
  LBoundary := False;

  if (AButton = mbLeft) and (LRow = 0) and
    (LPhysical >= 0) and (LPhysical < LGrid.ColCount) then
  begin
    LBounds := LGrid.CellRect(LPhysical, 0);
    LBoundary := (AX - LBounds.Left <= 5) or (LBounds.Right - AX <= 5);

    if LBoundary and (AX - LBounds.Left <= 5) and
      not LGrid.UseRightToLeftAlignment then
    begin
      Dec(LPhysical);
      LColumn := CanonicalColumn(LGrid, LPhysical);
    end;

    if LBoundary and (LBounds.Right - AX <= 5) and
      LGrid.UseRightToLeftAlignment then
    begin
      Dec(LPhysical);
      LColumn := CanonicalColumn(LGrid, LPhysical);
    end;

    if LBoundary or (LColumn >= 0) then
    begin
      { Reserve the ORIGINAL input/map/field before editor suspension can call
        out. Caller-held leases protect both this request and displaced owners
        across callbacks and interface-release tails. }

      if LBoundary then
      begin

        LAction := nclaResize;
        LDoubleStamp := 0;

        if ssDouble in AShift then
        begin
          LAction := nclaFit;
          LDoubleStamp := TNyxCollectionStringGrid(LGrid).ColumnDoubleClickStamp;
          TNyxCollectionStringGrid(LGrid).ClaimColumnDoubleClick(LDoubleStamp);
        end;
        LResizeLease := BeginColumnLayout(LGrid, LColumn, LPhysical, LAction, LResize);

        if LResize = nil then
        begin
          Exit;
        end;

        if LAction = nclaFit then
        begin
          LResize.DoubleStamp := LDoubleStamp;
        end;

        if not ColumnResizeCurrent(LResize, False) then
        begin
          RetireColumnResize(LResize);
          ColumnValidationIdle(nil);
          Exit;
        end;
      end;
      { Suspend before the prior mouse/focus callbacks can open a form or move
        native focus. Query/header activation and resizing share this lease. }

      if (LResize = nil) or (LResize.Action = nclaResize) then
      begin
        TNyxCollectionStringGrid(FGrid).BeginColumnTransition;
        try
          CaptureColumnEditor;
        finally

          if GetConnected then
          begin
            TNyxCollectionStringGrid(FGrid).EndColumnTransition;
          end;
        end;
      end;

      if not GetInputReady or ColumnInputBusy or (LRevision <> FArrangedRevision) then
      begin
        RetireColumnResize(LResize);
        ColumnValidationIdle(nil);
        Exit;
      end;

      if (LResize <> nil) and not ColumnResizeCurrent(LResize, False) then
      begin
        RetireColumnResize(LResize);
        ColumnValidationIdle(nil);
        Exit;
      end;
    end;

    if LBoundary then
    begin

      if LResize.Allowed then
      begin
        try

          if not CaptureColumnLayout(LResize) then
          begin
            RetireColumnResize(LResize);
            Exit;
          end;
        except
          RetireColumnResize(LResize);
          raise;
        end;
      end
      else if LResize.Action = nclaResize then
      begin
        { The LCL sizing switch is table-wide. Suppress this one disallowed
          boundary before its default MouseDown starts; the queued reconciliation
          restores the accepted table policy after release. }
        LGrid.Options := LGrid.Options - [goColSizing];
      end;
    end
    else if LColumn >= 0 then
    begin
      FQueryPressColumn := LColumn;
      FQueryPressFilter := (LBounds.Right - AX <= 28) or (ssAlt in AShift);
      FQueryPressAppend := ssShift in AShift;
      FQueryPressRevision := LRevision;
      FQueryPressActive := FQueries <> nil;
    end;
  end;
  LPoint := FGrid.ScreenToClient(LGrid.ClientToScreen(Point(AX, AY)));

  if Assigned(FPreviousMouse) then
  begin
    { Exactly one canonical outer event, with coordinates in that outer client.
      The renderer's structured pointer callback never sees a child identity. }
    try
      FPreviousMouse(FGrid, AButton, AShift, LPoint.X, LPoint.Y);
    except
      { A failed down never reaches LCL's DblClick/default/release admission.
        Retire only this retained intention, including after nested replacement. }
      RetireColumnResize(LResize);
      ArmCellRefreshIdle;
      raise;
    end;
  end;
end;

procedure TNativeMount.ArrangedKeyDown(Sender: TObject; var AKey: Word;
  AShift: TShiftState);
var
  LKeepAlive: INyxCollectionMount;
  LGrid: TStringGrid;
  LNextGrid: TStringGrid;
  LView: INyxCollectionView;
  LData: INyxCollectionSnapshot;
  LCurrent: INyxCollectionSnapshot;
  LOrder: TNyxItemRefs;
  LMove: TNyxGridMove;
  LDestination: TNyxGridCell;
  LColumn: Integer;
  LDisplay: Integer;
  LRow: Integer;
  LPhysical: Integer;
  LRevision: Integer;
  LNavigation: Boolean;
  LUpdating: Boolean;
  LItem: TNyxItemRef;
  LField: TNyxColumnIdentity;
  LForm: TCustomForm;
  LFit: TNativeColumnLayoutRequest;
  LFitLease: IInterface;
  LFitKey: Boolean;
  LCancel: TNativeColumnLayoutRequest;
  LCancelLease: IInterface;
begin

  if FHasColumnLayout and (not GetInputReady or ColumnInputBusy) then
  begin
    AKey := 0;
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if FEditsSuspended then
  begin
    AKey := 0;
    Exit;
  end;
  LGrid := InputGrid(Sender);

  if LGrid = nil then
  begin
    AKey := 0;
    Exit;
  end;
  LView := FView;
  LData := FRendered;
  LRevision := FArrangedRevision;
  LColumn := CanonicalColumn(LGrid, LGrid.Col);
  LFit := nil;
  LCancel := nil;

  if (AKey = VK_ESCAPE) and FLayoutGesture and (FResizeRequest <> nil) and
    (FResizeRequest.Action = nclaResize) and (FResizeRequest.Target = LGrid) then
  begin
    { Keep the original request BEFORE the canonical key callback. A nested
      callback may consume Escape, replace the mount or start another drag. }
    LCancel := FResizeRequest;
    LCancelLease := FResizeRequestLease;
  end;
  LFitKey := (AKey = VK_F) and
    (AShift * [ssCtrl, ssShift, ssAlt, ssMeta, ssSuper, ssAltGr] = [ssCtrl, ssShift]);

  if LFitKey and (LColumn >= 0) and
    (nclfAutoSize in FColumnLayout.ColumnAt(LColumn).Features) then
  begin
    { Reserve before the canonical key callback. Disable/enable in that callback
      cannot lend this old physical key a fresh epoch. No editor state is read. }
    LFitLease := BeginColumnLayout(LGrid, LColumn, LGrid.Col, nclaFit, LFit);
  end;

  try

    if Assigned(FPreviousKey) then
    begin
      FPreviousKey(FGrid, AKey, AShift);
    end;
    LCurrent := LView.Snapshot;
  except
    RetireColumnResize(LFit);
    RetireColumnResize(LCancel);
    ArmCellRefreshIdle;
    raise;
  end;

  if (LCancel <> nil) and (AKey = VK_ESCAPE) then
  begin
    AKey := 0;

    if FLayoutGesture and (LCancel.Target = LGrid) and
      ColumnResizeCurrent(LCancel, False) then
    begin
      { Logical withdrawal precedes capture-release callbacks. The existing
        same-slot idle publisher owns eventual gesture cleanup/reconciliation;
        do not refresh, recapture input or clear deferred geometry here. }
      RetireColumnResize(LCancel);
      ArmCellRefreshIdle;
      TNyxCollectionStringGrid(LGrid).CancelColumnSizing(LCancel.Input, LCancel.Epoch);
    end
    else
    begin
      { A stale Escape cannot reset a replacement editor or cancel a newer
        request. Retiring the retained original never changes its successor. }
      RetireColumnResize(LCancel);
      ArmCellRefreshIdle;
    end;
    Exit;
  end;

  if LFit <> nil then
  begin
    try

      if (AKey = VK_F) and ColumnResizeCurrent(LFit, False) and
        CaptureColumnLayout(LFit) then
      begin
        { Fit waits until this key's complete native tail has returned.
          Unlike Enter/F2 this neither commits nor hides a physical editor. }
        AKey := 0;
        LFit.Pending := True;
        FResizeReleased := True;
      end
      else
      begin
        RetireColumnResize(LFit);
      end;
    except
      RetireColumnResize(LFit);
      ArmCellRefreshIdle;
      raise;
    end;
    ColumnValidationIdle(nil);
    Exit;
  end;

  if not GetInputReady or ColumnInputBusy or (LRevision <> FArrangedRevision) or
    (FView <> LView) or (LData = nil) or (FRendered <> LData) or
    (LCurrent <> LData) then
  begin
    AKey := 0;
    Exit;
  end;

  if FUpdating or not FEnabled or (AKey = 0) then
  begin
    Exit;
  end;

  if (AKey = VK_DOWN) and (ssAlt in AShift) and (LColumn >= 0) and
    (FQueries <> nil) then
  begin
    AKey := 0;
    TNyxCollectionStringGrid(FGrid).BeginColumnTransition;
    try
      CaptureColumnEditor;
    finally

      if GetConnected then
      begin
        TNyxCollectionStringGrid(FGrid).EndColumnTransition;
      end;
    end;

    if GetInputReady and not ColumnInputBusy and (LRevision = FArrangedRevision) then
    begin
      QueryColumn(LColumn, True, False);
    end;
    Exit;
  end;

  if ssAlt in AShift then
  begin
    Exit;
  end;

  if AKey = VK_TAB then
  begin
    { Native physical Tab cannot cross separate edge panes or inert geometry
      spacers. Keep the same logical row and move between eligible visible
      fields. At either outer edge, ordinary parent traversal leaves the table. }
    AKey := 0;

    if (LColumn < 0) or (LGrid.Row < 1) or (LGrid.Row > LData.Count) then
    begin
      LForm := GetParentForm(FGrid);

      if LForm <> nil then
      begin
        LForm.SelectNext(FGrid, not (ssShift in AShift), True);
      end;
      Exit;
    end;
    LItem := LData.ItemAt(LGrid.Row - 1).Ref;
    LField := FView.Spec.ColumnAt(LColumn).Identity;

    if LGrid.EditorMode then
    begin
      TNyxCollectionStringGrid(LGrid).CommitColumnEditor;

      if not GetInputReady or ColumnInputBusy or FColumnEdit.Active then
      begin
        Exit;
      end;
    end;
    LData := FView.Snapshot;
    LColumn := FView.Spec.ColumnIndex(LField);

    if (LColumn < 0) or not LData.Has(LItem) then
    begin
      Exit;
    end;
    LDisplay := ColumnDisplayIndex(LColumn);

    if LDisplay < 0 then
    begin
      Exit;
    end;
    LRow := LData.IndexOf(LItem) + 1;
    repeat

      if ssShift in AShift then
      begin
        Dec(LDisplay);
      end
      else
      begin
        Inc(LDisplay);
      end;

      if (LDisplay < 0) or (LDisplay >= DisplayColumnCount) then
      begin

        LForm := GetParentForm(FGrid);

        if LForm <> nil then
        begin
          LForm.SelectNext(FGrid, not (ssShift in AShift), True);
        end;
        Exit;
      end;
      LColumn := DisplayColumn(LDisplay);
    until CanonicalEditable(LColumn, LRow);
    LNextGrid := GridForColumn(LColumn, LPhysical);

    if LNextGrid = nil then
    begin
      Exit;
    end;
    LUpdating := FUpdating;
    FUpdating := True;
    try
      LNextGrid.Col := LPhysical;
      LNextGrid.Row := LRow;
      FActiveGrid := LNextGrid;
      FFocusedCanonical := LColumn;
    finally
      FUpdating := LUpdating;
    end;

    if LNextGrid.CanFocus then
    begin
      LNextGrid.SetFocus;
    end;

    if GetInputReady and (FView.Snapshot = LData) and
      (GridForColumn(LColumn, LPhysical) = LNextGrid) then
    begin
      LNextGrid.EditorMode := True;
    end;
    Exit;
  end;

  if LGrid.EditorMode then
  begin

    if AKey in [VK_F2, VK_RETURN] then
    begin
      AKey := 0;
      TNyxCollectionStringGrid(LGrid).CommitColumnEditor;
    end
    else if AKey in [VK_PRIOR, VK_NEXT] then
    begin
      AKey := 0;
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

  if AKey in [VK_F2, VK_RETURN] then
  begin
    AKey := 0;

    if CanonicalEditable(LColumn, LGrid.Row) then
    begin
      FActiveGrid := LGrid;
      FFocusedCanonical := LColumn;
      LGrid.EditorMode := True;
    end;
    Exit;
  end;

  if (DisplayColumnCount = 0) or (LData.Count = 0) then
  begin
    Exit;
  end;
  LDisplay := ColumnDisplayIndex(LColumn);

  if LDisplay < 0 then
  begin
    LDisplay := 0;
  end;
  { An actual focused grid need not have a semantic selection yet. Navigation
    falls back to its physical row; an absent focus is not a scoped item. }
  LItem := LView.Selection.Focus;
  LRow := -1;

  if LItem.Defined then
  begin
    LRow := LData.IndexOf(LItem);
  end;

  if LRow < 0 then
  begin
    LRow := Max(0, Min(LGrid.Row - 1, LData.Count - 1));
  end;
  LNavigation := True;
  case AKey of
    VK_LEFT: LMove := ngmLeft;
    VK_RIGHT: LMove := ngmRight;
    VK_UP: LMove := ngmUp;
    VK_DOWN: LMove := ngmDown;
    VK_PRIOR: LMove := ngmPageUp;
    VK_NEXT: LMove := ngmPageDown;
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
    else LNavigation := False;
  end;

  if not LNavigation then
  begin

    if AKey = VK_SPACE then
    begin
      AKey := 0;
      Select(LData.ItemAt(LRow).Ref, nsaToggle);
    end;
    Exit;
  end;

  if FGrid.UseRightToLeftAlignment then
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
  AKey := 0;
  LDestination := MoveNyxGridCell(LRow, LDisplay, LData.Count,
    DisplayColumnCount, LMove, Max(1, FGrid.VisibleRowCount - 1));
  LColumn := DisplayColumn(LDestination.Column);
  LOrder := VisibleOrder;
  FFocusedCanonical := LColumn;
  LNextGrid := GridForColumn(LColumn, LPhysical);

  if LNextGrid = nil then
  begin
    Exit;
  end;
  LUpdating := FUpdating;
  FUpdating := True;
  try
    LNextGrid.Col := LPhysical;
    LNextGrid.Row := LDestination.Row + 1;
    FGrid.Row := LDestination.Row + 1;
    FActiveGrid := LNextGrid;
  finally
    FUpdating := LUpdating;
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

  if GetInputReady and (FView.Snapshot = LData) then
  begin
    { Selection refresh may have changed physical maps. Reacquire the current
      surface by stable field identity before assigning actual native focus. }
    LNextGrid := GridForColumn(LColumn, LPhysical);

    if (LNextGrid <> nil) and LNextGrid.CanFocus then
    begin
      LNextGrid.SetFocus;
    end;
  end;
end;

procedure TNativeMount.GridMouseDown(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LKeepAlive: INyxCollectionMount;
  LColumn, LRow: Integer;
  LBounds: TRect;
begin

  if FHasColumnLayout and not GetInputReady then
  begin
    Exit;
  end;

  if FHasColumnLayout then
  begin
    ArrangedMouseDown(Sender, AButton, AShift, AX, AY);
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  FGridShift := AShift;
  FQueryPressActive := False;

  if Assigned(FPreviousMouse) then
  begin
    FPreviousMouse(Sender, AButton, AShift, AX, AY);
  end;

  if not GetInputReady or not FEnabled or FUpdating or (FGrid = nil) or
    (FQueries = nil) or (AButton <> mbLeft) or FGrid.EditorMode then
  begin
    Exit;
  end;
  FGrid.MouseToCell(AX, AY, LColumn, LRow);

  if (LRow <> 0) or (LColumn < 0) or (LColumn >= FView.Spec.Count) then
  begin
    Exit;
  end;
  LBounds := FGrid.CellRect(LColumn, 0);
  { Leave native column resizing to the widget's existing boundary gesture. }

  if (AX - LBounds.Left <= 5) or (LBounds.Right - AX <= 5) then
  begin
    Exit;
  end;
  { MouseDown runs before LCL focuses the grid. Only record intent here;
    HeaderClick runs on release after the widget admits a genuine header click,
    so opening the form does not immediately lose focus to the grid default. }
  FQueryPressColumn := LColumn;
  FQueryPressFilter := (LBounds.Right - AX <= 28) or (ssAlt in AShift);
  FQueryPressAppend := ssShift in AShift;
  FQueryPressActive := True;
end;

procedure TNativeMount.GridHeaderClicked(Sender: TObject; AIsColumn: Boolean; AIndex: Integer);
var
  LKeepAlive: INyxCollectionMount;
  LFilter, LAppend: Boolean;
  LGrid: TStringGrid;
  LColumn, LRevision: Integer;
begin

  if FHasColumnLayout and not GetInputReady then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if FHasColumnLayout then
  begin
    LGrid := InputGrid(Sender);
    LColumn := CanonicalColumn(LGrid, AIndex);
    LRevision := FArrangedRevision;
    LFilter := FQueryPressActive and (FQueryPressColumn = LColumn) and
      (FQueryPressRevision = LRevision) and FQueryPressFilter;
    LAppend := FQueryPressActive and (FQueryPressColumn = LColumn) and
      (FQueryPressRevision = LRevision) and FQueryPressAppend;
    FQueryPressActive := False;

    if (LGrid = nil) or (LColumn < 0) or not AIsColumn or FLayoutGesture then
    begin
      Exit;
    end;

    if Assigned(FPreviousHeaderClick) then
    begin
      FPreviousHeaderClick(FGrid, True, LColumn);
    end;

    if GetInputReady and (LRevision = FArrangedRevision) and FEnabled and
      not FUpdating and (FQueries <> nil) then
    begin
      QueryColumn(LColumn, LFilter, LAppend);
    end;
    Exit;
  end;
  LFilter := FQueryPressActive and (FQueryPressColumn = AIndex) and FQueryPressFilter;
  LAppend := FQueryPressActive and (FQueryPressColumn = AIndex) and FQueryPressAppend;
  FQueryPressActive := False;

  if Assigned(FPreviousHeaderClick) then
  begin
    FPreviousHeaderClick(Sender, AIsColumn, AIndex);
  end;

  if not GetInputReady or not FEnabled or FUpdating or not AIsColumn or
    (FGrid = nil) or (FQueries = nil) or FGrid.EditorMode then
  begin
    Exit;
  end;
  QueryColumn(AIndex, LFilter, LAppend);
end;

procedure TNativeMount.PrepareGrid(Sender: TObject; AColumn, ARow: Integer;
  AState: TGridDrawState);
var
  LKeepAlive: INyxCollectionMount;
  LStyle: TTextStyle;
  LGrid: TStringGrid;
  LData, LPaintData: INyxCollectionSnapshot;
  LView: INyxCollectionView;
  LColumn, LRevision: Integer;
  LMultiple, LSelected, LDataRow: Boolean;
begin
  LKeepAlive := Self as INyxCollectionMount;

  if FHasColumnLayout then
  begin
    LGrid := InputGrid(Sender);
    LColumn := CanonicalColumn(LGrid, AColumn);

    if (LGrid = nil) or (LColumn < 0) then
    begin
      Exit;
    end;
    LView := FView;
    LData := FColumnRendering;

    if LData = nil then
    begin
      LData := FRendered;
    end;
    LRevision := FArrangedRevision;
    LMultiple := FColumnLayout.Spec.SelectionMode = nsmMultiple;

    if Assigned(FPreviousPrepare) then
    begin
      FPreviousPrepare(FGrid, LColumn, ARow, AState);
    end;
    { Membership is current, but its row identity belongs to the physical
      immutable snapshot. A deferred query/reorder cannot recolor a different
      visible item merely because it now occupies this numeric source row. }
    LSelected := False;
    LDataRow := LMultiple and (LData <> nil) and (ARow > 0) and (ARow <= LData.Count);

    if LDataRow then
    begin
      LSelected := LView.Selection.Contains(LData.ItemAt(ARow - 1).Ref);
    end;
    LPaintData := FColumnRendering;

    if LPaintData = nil then
    begin
      LPaintData := FRendered;
    end;

    if not InputLifetimeCurrent or (FView <> LView) or
      (LRevision <> FArrangedRevision) or (LPaintData <> LData) or
      (InputGrid(Sender) <> LGrid) or
      (CanonicalColumn(LGrid, AColumn) <> LColumn) then
    begin
      Exit;
    end;

    if (ARow = 0) and (FQueries <> nil) then
    begin
      LStyle := LGrid.Canvas.TextStyle;
      LStyle.SingleLine := False;
      LStyle.WordBreak := True;
      LGrid.Canvas.TextStyle := LStyle;
    end
    else if LDataRow then
    begin

      if LSelected then
      begin
        LGrid.Canvas.Brush.Color := clHighlight;
        LGrid.Canvas.Font.Color := clHighlightText;
      end
      else
      begin
        LGrid.Canvas.Brush.Color := LGrid.Color;
        LGrid.Canvas.Font.Color := LGrid.Font.Color;
      end;
    end;
    Exit;
  end;

  if Assigned(FPreviousPrepare) then
  begin
    FPreviousPrepare(Sender, AColumn, ARow, AState);
  end;

  if not GetInputReady or (FGrid = nil) then
  begin
    Exit;
  end;

  if (ARow = 0) and (FQueries <> nil) then
  begin
    LStyle := FGrid.Canvas.TextStyle;
    LStyle.SingleLine := False;
    LStyle.WordBreak := True;
    FGrid.Canvas.TextStyle := LStyle;
    Exit;
  end;

  if not GetInputReady or (FView.Spec.SelectionMode <> nsmMultiple) or
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
  LTreeRows: INyxTreeRows;
begin
  Result := nil;

  if FTree <> nil then
  begin
    LTreeRows := TreeRows;
    SetLength(Result, LTreeRows.Count);
    for LIndex := 0 to Length(Result) - 1 do
    begin
      Result[LIndex] := LTreeRows.RowAt(LIndex).Item;
    end;
    Exit;
  end;
  SetLength(Result, FView.Snapshot.Count);
  for LIndex := 0 to Length(Result) - 1 do
  begin
    Result[LIndex] := FView.Snapshot.ItemAt(LIndex).Ref;
  end;
end;

function TNativeMount.ClipboardInputAvailable: Boolean;
begin
  Result := (FGrid <> nil) and not FPaneSync and not FLayoutGesture and not FResizePending;

  if Result and (FColumnEditing <> nil) then
  begin
    Result := not ColumnInputBusy;
  end;
end;

function TNativeMount.CellRangeAvailable: Boolean;
begin
  Result := FGrid is TNyxCollectionStringGrid;
end;

function TNativeMount.CanonicalRangeSelected(AColumn, ARow: Integer): Boolean;
begin
  Result := FCellRangeEnabled and InputLifetimeCurrent and
    (((FRangePaint <> nil) and FRangePaint.Current) or
      ((FRangePaintStroke <> nil) and FRangePaintStroke.Current)) and
    (ARow >= FRangeFirstRow) and (ARow <= FRangeLastRow) and
    (AColumn >= 0) and (AColumn < Length(FRangePaintFields));

  if Result then
  begin
    Result := FRangePaintFields[AColumn];
  end;
end;

function TNativeMount.GridRangeSelected(AColumn, ARow: Integer): Boolean;
begin
  Result := CanonicalRangeSelected(CanonicalColumn(FGrid, AColumn), ARow);
end;

procedure TNativeMount.RetireCellRangeStroke(const AStroke: INyxCollectionRangeStroke);
begin
  FinishRangeGesture(AStroke);

  if InputLifetimeCurrent then
  begin
    RenderCellRange;
  end;
end;

procedure TNativeMount.RenderCellRange;
var
  LLease: INyxCollectionMount;
  LIntent: INyxCollectionCellRangeIntent;
  LPrevious: INyxCollectionCellRangeIntent;
  LPreviousStroke: INyxCollectionRangeStroke;
  LStroke: INyxCollectionRangeStroke;
  LCapture: INyxCollectionCellRangeCapture;
  LBounds: INyxCollectionCellRangeBounds;
  LPayload: INyxCollectionClipboardText;
  LFrame: INyxNativeGestureFrame;
  LCanonical: Integer;
  LField: Integer;
begin
  LLease := Self as INyxCollectionMount;

  if (FRangeGesture <> nil) and (not FRangeGesture.Current or
    ((FRangeGeneration <> nil) and FRangeGeneration.Cancelled)) then
  begin
    FinishRangeGesture(FRangeGesture);
  end;
  LPrevious := FRangePaint;
  LPreviousStroke := FRangePaintStroke;
  FRangePaint := nil;
  FRangePaintStroke := nil;
  FRangePaintFields := nil;

  if FCellRangeEnabled and (FCellRanges <> nil) then
  begin

    if (FCellStroke <> nil) and FCellStroke.Current and (FCellStrokePreview <> nil) then
    begin
      LStroke := FCellStroke;
      LCapture := FCellStrokePreview;
    end
    else
    begin
      LIntent := FCellRanges.Selection;

      if (LIntent <> nil) and LIntent.Current and (LIntent.Page = FRendered) then
      begin
        LCapture := LIntent.Capture;
      end;
    end;

    if LCapture <> nil then
    begin

      if Supports(LCapture, INyxCollectionCellRangeBounds, LBounds) then
      begin
        LPayload := LCapture.Payload;
        SetLength(FRangePaintFields, Length(FClipboardFields));
        for LCanonical := 0 to High(FRangePaintFields) do
        begin
          for LField := 0 to LPayload.FieldCount - 1 do
          begin

            if LPayload.FieldAt(LField).Same(FClipboardFields[LCanonical]) then
            begin
              FRangePaintFields[LCanonical] := True;
              Break;
            end;
          end;
        end;
        FRangeFirstRow := LBounds.FirstRow + 1;
        FRangeLastRow := LBounds.LastRow + 1;
        FRangePaint := LIntent;
        FRangePaintStroke := LStroke;
      end;
    end;
  end;
  LFrame := FColumnFrame;

  if LFrame <> nil then
  begin
    LFrame.Enter;
  end;
  try

    if FGrid is TNyxCollectionStringGrid then
    begin

      if FCellRangeEnabled then
      begin
        TNyxCollectionStringGrid(FGrid).ObserveCellRange(GridRangeSelected);
      end
      else
      begin
        TNyxCollectionStringGrid(FGrid).ObserveCellRange(nil);
      end;
      FGrid.Invalidate;
    end;

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;

    if FStartPane <> nil then
    begin

      if FCellRangeEnabled then
      begin
        FStartPane.Grid.ObserveCellRange(FStartPane.RangeSelected);
      end
      else
      begin
        FStartPane.Grid.ObserveCellRange(nil);
      end;
      FStartPane.Grid.Invalidate;
    end;

    if not InputLifetimeCurrent then
    begin
      Exit;
    end;

    if FEndPane <> nil then
    begin

      if FCellRangeEnabled then
      begin
        FEndPane.Grid.ObserveCellRange(FEndPane.RangeSelected);
      end
      else
      begin
        FEndPane.Grid.ObserveCellRange(nil);
      end;
      FEndPane.Grid.Invalidate;
    end;
  finally

    if LFrame <> nil then
    begin
      LFrame.Leave;
    end;
  end;
end;

constructor TNativeCellPresentation.Create(AOwner: TNativeMount);
begin
  inherited Create;
  FOwner := AOwner;
  FLease := AOwner as INyxCollectionMount;
  FInput := AOwner.GetInputLifetime;
  FGrid := TNyxCollectionStringGrid(AOwner.FGrid);
  FGrid.BeginColumnTransition;
  FActive := True;
end;

destructor TNativeCellPresentation.Destroy;
begin
  try
    Leave;
  except
    { Explicit Leave reports errors; final managed release never throws. }
  end;
  inherited Destroy;
end;

procedure TNativeCellPresentation.Leave;
var
  LGrid: TNyxCollectionStringGrid;
  LInput: INyxCollectionInputLifetime;
  LOwner: TNativeMount;
  LLease: INyxCollectionMount;
begin

  if not FActive then
  begin
    Exit;
  end;
  FActive := False;
  LGrid := FGrid;
  LInput := FInput;
  LOwner := FOwner;
  LLease := FLease;
  FGrid := nil;
  FOwner := nil;
  FLease := nil;
  FInput := nil;

  if LInput.Current and (LOwner.FGrid = LGrid) then
  begin
    LGrid.EndColumnTransition;
  end;
end;

function TNativeMount.BeginCellPresentation: INyxCollectionCellPresentation;
begin
  Result := nil;

  if not GetInputReady or FUpdating or not (FGrid is TNyxCollectionStringGrid) then
  begin
    Exit;
  end;

  if FHasColumnLayout and not PrepareColumnButtonDraft(FGrid) then
  begin
    raise ENyxCollection.Create('Original cell draft cannot safely leave its editor');
  end;
  Result := TNativeCellPresentation.Create(Self);
end;

constructor TNativeRangePointerCommand.Create(AOwner: TNativeMount;
  const ACommand: INyxCollectionCellRangePointerEntry;
  const AGeneration: INyxCancellationScope);
begin
  inherited Create;
  FOwner := AOwner;
  FCommand := ACommand;
  FGeneration := AGeneration;
end;

destructor TNativeRangePointerCommand.Destroy;
begin
  try
    Retire;
  except
    { Explicit retirement reports failure; final release still frees fields. }
  end;
  inherited Destroy;
end;

procedure TNativeRangePointerCommand.ResolveCells(
  const AOrigin, ATarget: TNyxCollectionCellIdentity);
begin

  if GetCurrent then
  begin
    FCommand.ResolveCells(AOrigin, ATarget);
  end;
end;

procedure TNativeRangePointerCommand.Prepare;
begin

  if GetCurrent then
  begin
    FCommand.Prepare;
  end;
end;

function TNativeRangePointerCommand.GetCurrent: Boolean;
begin
  Result := (FCommand <> nil) and (FGeneration <> nil) and
    not FGeneration.Cancelled and FCommand.Current;
end;

function TNativeRangePointerCommand.GetDispatchCurrent: Boolean;
begin
  Result := (FCommand <> nil) and (FGeneration <> nil) and
    not FGeneration.Cancelled and FCommand.DispatchCurrent;
end;

function TNativeRangePointerCommand.Complete(AAllowDefault: Boolean): Boolean;
var
  LCommand: INyxCollectionCellRangePointerEntry;
  LReceipt: INyxCollectionCellRangePointerReceipt;
  LSubject: INyxCollectionRangeStroke;
  LOwner: TNativeMount;
  LArm: Boolean;
begin
  LCommand := FCommand;
  LOwner := FOwner;
  LSubject := FSubject;
  LArm := FArm;
  FArm := False;
  Result := False;

  if (LCommand = nil) or FCompleted then
  begin
    Exit;
  end;
  FCompleted := True;
  FCompleting := True;
  try
    try
      Result := LCommand.Complete(AAllowDefault and GetCurrent);

      if (FOwner <> LOwner) or (FCommand <> LCommand) then
      begin
        Exit;
      end;

      if LArm and AAllowDefault and GetCurrent and not FCaptureRequested and
        Supports(LCommand, INyxCollectionCellRangePointerReceipt, LReceipt) and
        LReceipt.Installed then
      begin
        LOwner.ArmRangeGesture(FControl, FGeneration, FEndpoint);
      end
      else if (LSubject <> nil) and ((FPhase = ncrspRelease) or
        not AAllowDefault or FCaptureRequested or not GetCurrent or not LSubject.Current) then
      begin
        LOwner.FinishRangeGesture(LSubject);
      end;
    except
      try

        if (FCommand = LCommand) and (LSubject <> nil) then
        begin
          LOwner.FinishRangeGesture(LSubject);
        end;
      except
        { Preserve the original callback/focus diagnostic over cleanup failure. }
      end;
      raise;
    end;
  finally
    FCompleting := False;
  end;
end;

procedure TNativeRangePointerCommand.ObservePointerRequest(ARequest: TNyxPointerRequest);
begin

  if ARequest <> nprUnchanged then
  begin
    FCaptureRequested := True;

    if (FOwner <> nil) and (FSubject <> nil) and
      (FOwner.FRangeGesture = FSubject) then
    begin
      { An explicit canonical directive now owns capture. Cancelling this range
        must not release the capture the application has chosen to keep. }
      FOwner.FRangeCaptured := False;
    end;
  end;
end;

procedure TNativeRangePointerCommand.Retire;
var
  LCommand: INyxCollectionCellRangePointerEntry;
  LMountLease: INyxCollectionMount;
  LOwner: TNativeMount;
  LSubject: INyxCollectionRangeStroke;
  LAbandon: Boolean;
begin
  LCommand := FCommand;
  LOwner := FOwner;
  LSubject := FSubject;
  LAbandon := not FCompleted or FCompleting;

  if LOwner <> nil then
  begin
    LMountLease := LOwner;
  end;
  FCommand := nil;
  FOwner := nil;
  FGeneration := nil;
  FSubject := nil;
  FControl := nil;
  try

    if LCommand <> nil then
    begin
      LCommand.Retire;
    end;
  finally

    if LAbandon and (LOwner <> nil) and (LSubject <> nil) then
    begin
      LOwner.FinishRangeGesture(LSubject);
    end;
  end;
end;

procedure TNativeMount.FinishRangeGesture(const ASubject: INyxCollectionRangeStroke);
var
  LControl: TNyxCollectionStringGrid;
  LCaptured: Boolean;
begin

  if (ASubject = nil) or (FRangeGesture <> ASubject) then
  begin
    Exit;
  end;
  LControl := FRangeControl;
  LCaptured := FRangeCaptured;
  FRangeGesture := nil;
  FRangeGeneration := nil;
  FRangeControl := nil;
  FRangeCaptured := False;
  try

    if LCaptured and (LControl <> nil) and NyxLCLHasPointerCapture(LControl) and
      not ((FRangeGesture <> nil) and FRangeCaptured and (FRangeControl = LControl)) then
    begin
      ReleaseNyxLCLPointer(LControl);
    end;
  finally
    AbandonCellRangeStroke(NyxCollectionRangeStrokePointer(ncrsdMouse, 0), ASubject);
  end;
end;

procedure TNativeMount.CancelRangeSurface(Sender: TObject);
var
  LLease: INyxCollectionMount;
begin
  LLease := Self;

  if Sender = FRangeControl then
  begin
    FinishRangeGesture(FRangeGesture);
  end;
end;

procedure TNativeMount.ArmRangeGesture(AControl: TNyxCollectionStringGrid;
  const AGeneration: INyxCancellationScope;
  const AEndpoint: TNyxCollectionCellIdentity);
var
  LEntry: INyxCollectionCellRangeStrokeEntry;
  LStroke: INyxCollectionRangeStroke;
  LPointer: TNyxCollectionRangeStrokePointer;
  LAttempted: Boolean;
  LExisting: TControl;

  procedure ReconcileCapture;
  begin

    if LAttempted and (FRangeGesture <> LStroke) and
      NyxLCLHasPointerCapture(AControl) and
      not ((FRangeGesture <> nil) and FRangeCaptured and (FRangeControl = AControl)) then
    begin
      ReleaseNyxLCLPointer(AControl);
    end;
  end;

begin

  if (AGeneration = nil) or AGeneration.Cancelled or
    not InputLifetimeCurrent or (AControl = nil) then
  begin
    Exit;
  end;
  LPointer := NyxCollectionRangeStrokePointer(ncrsdMouse, 0);
  LEntry := BeginCellRangeStroke(ncrspBegin, LPointer, ncrpExtend);

  if LEntry = nil then
  begin
    Exit;
  end;
  try
    LEntry.ResolveCells(AEndpoint, AEndpoint);
    LEntry.Prepare;
    LEntry.Complete(not AGeneration.Cancelled);
    LStroke := LEntry.PendingStroke;

    if (LStroke = nil) or not LEntry.Current or not LStroke.Current or
      AGeneration.Cancelled then
    begin
      AbandonCellRangeStroke(LPointer, LStroke);
      Exit;
    end;
    FRangeGesture := LStroke;
    FRangeGeneration := AGeneration;
    FRangeControl := AControl;
    LAttempted := False;
    try
      { LCL's real WMLButtonDown captures its own source BEFORE MouseDown. Adopt
        that same default pair; virtual/direct input needs explicit acquisition.
        Explicit canonical capture was already excluded by the wrapper. Never
        take a different control's capture to implement this optional default. }
      LExisting := GetCaptureControl;

      if (LExisting <> nil) and (LExisting <> AControl) then
      begin
        FinishRangeGesture(LStroke);
        Exit;
      end;

      if not LEntry.Current or AGeneration.Cancelled or (FRangeGesture <> LStroke) then
      begin
        FinishRangeGesture(LStroke);
        Exit;
      end;
      FRangeCaptured := True;

      if LExisting <> AControl then
      begin
        LAttempted := True;
        CaptureNyxLCLPointer(AControl);
      end;
      ReconcileCapture;

      if not LEntry.Current or AGeneration.Cancelled or (FRangeGesture <> LStroke) or
        not NyxLCLHasPointerCapture(AControl) then
      begin
        FinishRangeGesture(LStroke);
      end;
    except
      try
        FinishRangeGesture(LStroke);
      finally
        ReconcileCapture;
      end;
    end;
  finally
    LEntry.Retire;
  end;
end;

function TNativeMount.BeginRangeStroke(ASender: TObject;
  APhase: TNyxCollectionRangeStrokePhase; AShift: TShiftState; AX, AY: Integer;
  const AGeneration: INyxCancellationScope): INyxCollectionCellRangePointerEntry;
var
  LEntry: INyxCollectionCellRangeStrokeEntry;
  LWrapper: TNativeRangePointerCommand;
  LSubject: INyxCollectionRangeStroke;
  LHit: TWinControl;
  LGrid: TStringGrid;
  LOriginGrid: TStringGrid;
  LScreen: TPoint;
  LPoint: TPoint;
  LData: INyxCollectionSnapshot;
  LOrigin: TNyxCollectionCellIdentity;
  LTarget: TNyxCollectionCellIdentity;
  LPhysical: Integer;
  LRow: Integer;
  LColumn: Integer;
  LOriginColumn: Integer;
  LOriginRow: Integer;
  LCount: Integer;
begin
  Result := nil;
  LSubject := FRangeGesture;

  if (LSubject = nil) or
    ((ASender <> FRangeControl) and (ASender <> FGrid)) or (APhase = ncrspBegin) then
  begin
    Exit;
  end;

  if (AGeneration = nil) or (AGeneration <> FRangeGeneration) or
    AGeneration.Cancelled or not LSubject.Current or
    ((APhase = ncrspMove) and not (ssLeft in AShift)) then
  begin
    FinishRangeGesture(LSubject);
    Exit;
  end;
  LEntry := BeginCellRangeStroke(APhase,
    NyxCollectionRangeStrokePointer(ncrsdMouse, 0), ncrpExtend, LSubject);

  if LEntry = nil then
  begin
    FinishRangeGesture(LSubject);
    Exit;
  end;
  LWrapper := TNativeRangePointerCommand.Create(Self, LEntry, AGeneration);
  Result := LWrapper;
  LWrapper.FSubject := LSubject;
  LWrapper.FPhase := APhase;
  { A nonnil but unresolved envelope still claims this original input. A hit
    outside owned data cells cancels, never borrowing a new row/editor default. }
  { Existing arranged-pane forwarding reports logical outer-grid coordinates.
    Preserve that public callback behavior and translate from the ACTUAL sender
    coordinate system; the pending capture still belongs to its original pane. }
  LScreen := TControl(ASender).ClientToScreen(Point(AX, AY));

  if not LWrapper.GetCurrent then
  begin
    Exit;
  end;
  LHit := FindLCLWindow(LScreen);
  LGrid := InputGrid(LHit);
  LOriginGrid := InputGrid(Screen.ActiveControl);

  if not LWrapper.GetCurrent or (LGrid = nil) or (LHit <> LGrid) or
    (LOriginGrid = nil) or (Screen.ActiveControl <> LOriginGrid) or LOriginGrid.EditorMode then
  begin
    Exit;
  end;
  LPoint := LGrid.ScreenToClient(LScreen);
  LGrid.MouseToCell(LPoint.X, LPoint.Y, LPhysical, LRow);
  LColumn := CanonicalColumn(LGrid, LPhysical);
  LOriginRow := LOriginGrid.Row;
  LOriginColumn := CanonicalColumn(LOriginGrid, LOriginGrid.Col);
  LData := FRendered;

  if not LWrapper.GetCurrent or (LData = nil) or (LColumn < 0) or
    (LOriginColumn < 0) or (LRow <= 0) or (LOriginRow <= 0) or
    not PtInRect(LGrid.CellRect(LPhysical, LRow), LPoint) then
  begin
    Exit;
  end;
  LCount := LData.Count;

  if not LWrapper.GetCurrent or (LRow > LCount) or (LOriginRow > LCount) then
  begin
    Exit;
  end;
  LOrigin := NyxCollectionCellIdentity(LData.ItemAt(LOriginRow - 1).Ref,
    ClipboardField(LOriginColumn));

  if not LWrapper.GetCurrent then
  begin
    Exit;
  end;
  LTarget := NyxCollectionCellIdentity(LData.ItemAt(LRow - 1).Ref,
    ClipboardField(LColumn));

  if LWrapper.GetCurrent then
  begin
    LWrapper.ResolveCells(LOrigin, LTarget);
  end;
end;

function TNativeMount.PrepareRangePointer(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer): INyxCollectionCellRangePointerEntry;
var
  LCommand: INyxCollectionCellRangePointerEntry;
  LGrid: TStringGrid;
  LOriginGrid: TStringGrid;
  LData: INyxCollectionSnapshot;
  LOrigin: TNyxCollectionCellIdentity;
  LTarget: TNyxCollectionCellIdentity;
  LPhysical: Integer;
  LRow: Integer;
  LOriginColumn: Integer;
  LOriginRow: Integer;
  LColumn: Integer;
  LCount: Integer;
  LGeneration: INyxCancellationScope;
  LWrapper: TNativeRangePointerCommand;
begin
  Result := nil;

  if not FCellRangeEnabled or not Assigned(FRangePointerDispatch) or
    (AButton <> mbLeft) or not (ssShift in AShift) or
    not (Sender is TNyxCollectionStringGrid) or (FRangeEvents = nil) then
  begin
    Exit;
  end;
  { Reserve BEFORE physical hit/focus/field and immutable page getters. The
    grid's existing native frame protects all borrowed controls during entry. }
  LGeneration := NyxEventGeneration(FRangeEvents);
  LCommand := BeginCellRangePointer(ncrpExtend);

  if LCommand = nil then
  begin
    Exit;
  end;
  LWrapper := TNativeRangePointerCommand.Create(Self, LCommand, LGeneration);
  LCommand := LWrapper;
  try
    FinishRangeGesture(FRangeGesture);

    if not LCommand.Current then
    begin
      Result := LCommand;
      Exit;
    end;
    LGrid := InputGrid(Sender);
    LOriginGrid := InputGrid(Screen.ActiveControl);

    if (LGrid = nil) or (LOriginGrid = nil) or
      (Screen.ActiveControl <> LOriginGrid) or LOriginGrid.EditorMode then
    begin
      Exit;
    end;
    { Captured native presses may carry coordinates outside their source.
      MouseToCell can still return a clamped data position there. Keep this
      original range input claimed but unresolved: callbacks are delivered,
      while neither a range nor ordinary row/editor defaults may borrow it. }

    if not PtInRect(LGrid.ClientRect, Point(AX, AY)) then
    begin
      Result := LCommand;
      Exit;
    end;
    LData := FRendered;
    LGrid.MouseToCell(AX, AY, LPhysical, LRow);
    LColumn := CanonicalColumn(LGrid, LPhysical);
    LOriginRow := LOriginGrid.Row;
    LOriginColumn := CanonicalColumn(LOriginGrid, LOriginGrid.Col);

    if (LData = nil) or (LRow <= 0) or (LOriginRow <= 0) or
      (LColumn < 0) or (LOriginColumn < 0) then
    begin
      Exit;
    end;

    if not PtInRect(LGrid.CellRect(LPhysical, LRow), Point(AX, AY)) then
    begin
      Result := LCommand;
      Exit;
    end;

    if not LCommand.Current then
    begin
      Result := LCommand;
      Exit;
    end;
    LCount := LData.Count;

    if not LCommand.Current then
    begin
      Result := LCommand;
      Exit;
    end;

    if (LRow > LCount) or (LOriginRow > LCount) then
    begin
      Exit;
    end;
    LOrigin := NyxCollectionCellIdentity(LData.ItemAt(LOriginRow - 1).Ref,
      ClipboardField(LOriginColumn));

    if not LCommand.Current then
    begin
      Result := LCommand;
      Exit;
    end;
    LTarget := NyxCollectionCellIdentity(LData.ItemAt(LRow - 1).Ref,
      ClipboardField(LColumn));

    if LCommand.Current then
    begin
      LCommand.ResolveCells(LOrigin, LTarget);
      LWrapper.FEndpoint := LTarget;
      LWrapper.FControl := TNyxCollectionStringGrid(LGrid);
      LWrapper.FArm := True;
    end;
    Result := LCommand;
  finally

    if Result = nil then
    begin
      LCommand.Retire;
    end;
  end;
end;

function TNativeMount.BeginCellContext(Sender: TObject; const APosition: TPoint):
  INyxCollectionClipboardCommand;
var
  LLease: INyxCollectionMount;
  LCommands: INyxCollectionClipboardCommands;
  LActions: INyxCollectionCellActions;
  LEntry: INyxCollectionCellActionInput;
  LCommand: INyxCollectionClipboardCellCommand;
  LRange: INyxCollectionCellRangeIntent;
  LRangeContext: INyxCollectionClipboardCellRangeContext;
  LDraft: INyxCollectionCellActionDraftCommand;
  LDraftInput: INyxCollectionFocusedDraftInput;
  LDraftActions: INyxCollectionCellDraftActions;
  LPosition: TPoint;
  LGrid: TStringGrid;
  LData: INyxCollectionSnapshot;
  LColumn: Integer;
  LPhysical: Integer;
  LRow: Integer;
  LCount: Integer;
  LField: TNyxColumnIdentity;
  LItem: TNyxItemRef;
begin
  Result := nil;
  LCommands := FClipboardCommands;
  LActions := FCellActions;

  if (LCommands = nil) or (LActions = nil) then
  begin
    Exit;
  end;
  LLease := Self as INyxCollectionMount;
  LData := FRendered;

  if (Sender is TCustomEdit) and
    (not Supports(LActions, INyxCollectionCellDraftActions, LDraftActions) or
      not LDraftActions.FocusedDraftEnabled) then
  begin
    { Default-off editors retain their ordinary native context path without
      even entering a managed action's outward input admission. }
    Exit;
  end;

  if not Supports(LActions, INyxCollectionCellActionInput, LEntry) then
  begin
    Exit;
  end;
  { Reserve both the original RAW input and the actions intent before physical
    coordinates or snapshot getters. A nested newer menu cannot lend the
    returning outer frame its intent, even when every cell byte is unchanged. }

  if FCellRangeEnabled and (FCellRanges <> nil) then
  begin
    { Canonical flags-only selection precedes even the outward input factory.
      A new same-cell rectangle admitted there cannot lend this old menu proof. }
    LRange := FCellRanges.Selection;
  end;
  LCommand := LEntry.BeginCellInput(LCommands);

  if LCommand = nil then
  begin
    Exit;
  end;

  if not LCommand.Current then
  begin
    { Nil means unsupported. A reserved but stale managed event stays claimed,
      so the editor cannot replay a native menu after a newer context wins. }
    LCommand.Retire;
    Result := LCommand;
    Exit;
  end;

  if (LRange <> nil) and Supports(LCommand,
    INyxCollectionClipboardCellRangeContext, LRangeContext) then
  begin
    LRangeContext.AttachRange(LRange);
  end;

  if not LCommand.Current then
  begin
    LCommand.Retire;
    Result := LCommand;
    Exit;
  end;

  if Supports(LCommand, INyxCollectionCellActionDraftCommand, LDraft) and
    Supports(LLease, INyxCollectionFocusedDraftInput, LDraftInput) then
  begin
    LDraft.CaptureFocusedDraft(LDraftInput);
  end;

  if not LCommand.Current then
  begin
    LCommand.Retire;
    Result := LCommand;
    Exit;
  end;

  if (Sender is TCustomEdit) and ((LDraft = nil) or not LDraft.HasDraft) then
  begin
    Exit;
  end;
  LGrid := InputGrid(Sender);

  if LGrid = nil then
  begin
    Exit;
  end;
  LPhysical := LGrid.Col;
  LRow := LGrid.Row;

  if (APosition.X >= 0) and (APosition.Y >= 0) then
  begin
    LPosition := APosition;

    if (Sender is TControl) and (Sender <> LGrid) then
    begin
      LPosition := LGrid.ScreenToClient(TControl(Sender).ClientToScreen(LPosition));
    end;

    if not LCommand.Current then
    begin
      LCommand.Retire;
      Result := LCommand;
      Exit;
    end;
    LGrid.MouseToCell(LPosition.X, LPosition.Y, LPhysical, LRow);
  end;
  Dec(LRow);
  LColumn := CanonicalColumn(LGrid, LPhysical);
  LField := ClipboardField(LColumn);

  if not LCommand.Current then
  begin
    LCommand.Retire;
    Result := LCommand;
    Exit;
  end;

  if (LData = nil) or (LRow < 0) or (LColumn < 0) or not LField.Defined then
  begin
    Exit;
  end;
  LCount := LData.Count;

  if not LCommand.Current then
  begin
    LCommand.Retire;
    Result := LCommand;
    Exit;
  end;

  if LRow < LCount then
  begin
    LItem := LData.ItemAt(LRow).Ref;

    if not LCommand.Current then
    begin
      LCommand.Retire;
      Result := LCommand;
      Exit;
    end;

    if (Sender is TCustomEdit) and not LDraft.MatchesDraftCell(LItem, LField) then
    begin
      Exit;
    end;
    LCommand.ResolveCell(LItem, LField);

    if not LCommand.Current then
    begin
      LCommand.Retire;
    end;
    Result := LCommand;
  end;
end;

function TNativeMount.FocusCellRange(ARow, AColumn: Integer): Boolean;
var
  LLease: INyxCollectionMount;
  LGrid: TStringGrid;
  LOuter: TNyxCollectionStringGrid;
  LInput: INyxCollectionInputLifetime;
  LFrame: INyxNativeGestureFrame;
  LEpoch: Integer;
  LPhysical: Integer;
  LUpdating: Boolean;

  function Original: Boolean;
  begin
    Result := LInput.Current and LInput.Ready and (LInput.Epoch = LEpoch) and
      (GetInputLifetime = LInput) and (FGrid = LOuter) and GetConnected;
  end;

begin
  LLease := Self as INyxCollectionMount;
  Result := False;

  if FHasColumnLayout then
  begin
    LGrid := GridForColumn(AColumn, LPhysical);
  end
  else
  begin
    LGrid := FGrid;
    LPhysical := AColumn;
  end;
  LInput := GetInputLifetime;

  if (LGrid = nil) or (ARow < 0) or (ARow + 1 >= LGrid.RowCount) or
    not GetInputReady or not (FGrid is TNyxCollectionStringGrid) then
  begin
    Exit;
  end;
  LOuter := TNyxCollectionStringGrid(FGrid);
  LEpoch := LInput.Epoch;
  LFrame := FColumnFrame;
  LUpdating := FUpdating;

  if LFrame <> nil then
  begin
    { Public prepared input need not arrive inside a native key frame. Retain
      the original controls through custom LCL selection/focus callbacks too. }
    LFrame.Enter;
  end;
  try
    FUpdating := True;
    LOuter.BeginColumnTransition;
    try
      LGrid.Col := LPhysical;

      if not Original then
      begin
        Exit;
      end;
      LGrid.Row := ARow + 1;

      if not Original then
      begin
        Exit;
      end;
      FActiveGrid := LGrid;
      FFocusedCanonical := AColumn;

      if LGrid.CanFocus and Original then
      begin
        LGrid.SetFocus;
      end;

      if Original then
      begin
        Result := LGrid.Focused;
      end;
    finally

      if LInput.Current then
      begin
        LOuter.EndColumnTransition;
      end;
    end;
  finally
    FUpdating := LUpdating;

    if LFrame <> nil then
    begin
      LFrame.Leave;
    end;
  end;
end;

function TNativeMount.CellRangeKeyDown(Sender: TObject; var AKey: Word;
  AShift: TShiftState): Boolean;
var
  LLease: INyxCollectionMount;
  LCommand: INyxCollectionCellRangeKeyCommand;
  LPageCommand: INyxCollectionCellRangePageKeyCommand;
  LKey: TNyxCollectionRangeKey;
  LOriginal: Word;
  LGrid: TStringGrid;
  LData: INyxCollectionSnapshot;
  LItem: TNyxItemRef;
  LField: TNyxColumnIdentity;
  LPrevious: TKeyEvent;
  LSender: TObject;
  LRow: Integer;
  LColumn: Integer;
  LCount: Integer;
begin
  Result := False;

  if not FCellRangeEnabled or
    (AShift * [ssAlt, ssSuper, ssAltGr] <> []) or
    ((ssCtrl in AShift) and (ssMeta in AShift)) then
  begin
    Exit;
  end;
  LGrid := InputGrid(Sender);

  if (LGrid = nil) or LGrid.EditorMode then
  begin
    Exit;
  end;

  if (AKey = VK_ESCAPE) and (AShift = []) and (GetCellRange <> nil) then
  begin
    LKey := ncrkClear;
  end
  else
  begin

    if not (ssShift in AShift) then
    begin
      Exit;
    end;
    case AKey of
      VK_LEFT:
        begin
          LKey := ncrkExtendLeft;
        end;
      VK_RIGHT:
        begin
          LKey := ncrkExtendRight;
        end;
      VK_UP:
        begin
          LKey := ncrkExtendUp;
        end;
      VK_DOWN:
        begin
          LKey := ncrkExtendDown;
        end;
      VK_HOME:
        begin
          LKey := ncrkExtendRowStart;
        end;
      VK_END:
        begin
          LKey := ncrkExtendRowEnd;
        end;
      VK_PRIOR:
        begin
          LKey := ncrkExtendPageUp;
        end;
      VK_NEXT:
        begin
          LKey := ncrkExtendPageDown;
        end;
      else
      begin
        Exit;
      end;
    end;

    if AShift * [ssCtrl, ssMeta] <> [] then
    begin

      if AKey = VK_HOME then
      begin
        LKey := ncrkExtendFirstCell;
      end
      else if AKey = VK_END then
      begin
        LKey := ncrkExtendLastCell;
      end
      else
      begin
        Exit;
      end;
    end;

    if FHasColumnLayout and FGrid.UseRightToLeftAlignment then
    begin

      if LKey = ncrkExtendLeft then
      begin
        LKey := ncrkExtendRight;
      end
      else if LKey = ncrkExtendRight then
      begin
        LKey := ncrkExtendLeft;
      end;
    end;
  end;
  LLease := Self as INyxCollectionMount;
  LCommand := BeginCellRangeKey(LKey);

  if LCommand = nil then
  begin
    Exit;
  end;
  Result := True;
  LOriginal := AKey;
  LPrevious := FPreviousKey;
  LSender := Sender;

  if FHasColumnLayout then
  begin
    LSender := FGrid;
  end;
  LData := FRendered;
  FRejectTextKey := True;
  FConsumedTextKey := True;
  try
    try

      if (LKey in [ncrkExtendPageUp, ncrkExtendPageDown]) and LCommand.Current then
      begin
        { Reserve the original input before observing the same native page
          geometry used by ordinary navigation. One row overlaps each step. }

        if Supports(LCommand, INyxCollectionCellRangePageKeyCommand, LPageCommand) then
        begin
          LPageCommand.ResolvePageRows(Max(1, LGrid.VisibleRowCount - 1));
        end;
      end;

      if (LKey <> ncrkClear) and LCommand.Current then
      begin
        LRow := LGrid.Row - 1;
        LColumn := CanonicalColumn(LGrid, LGrid.Col);
        LField := ClipboardField(LColumn);

        if (LData <> nil) and (LRow >= 0) and LField.Defined then
        begin
          LCount := LData.Count;

          if LCommand.Current and (LRow < LCount) then
          begin
            LItem := LData.ItemAt(LRow).Ref;
            LCommand.ResolveOrigin(NyxCollectionCellIdentity(LItem, LField));
          end;
        end;
      end;
      LCommand.Prepare;
    except
      on E: ENyxCollectionRangeStale do
      begin
        { Original input remains claimed; a nested newer selection survives. }
      end;
    end;

    if LCommand.DispatchCurrent and Assigned(LPrevious) then
    begin
      LPrevious(LSender, AKey, AShift);
    end;
    LCommand.Complete((AKey = LOriginal) and LCommand.DispatchCurrent);
  finally
    AKey := 0;
    LCommand.Retire;
  end;
end;

function TNativeMount.ClipboardKeyDown(Sender: TObject; var AKey: Word;
  AShift: TShiftState): Boolean;
var
  LLease: INyxCollectionMount;
  LCommands: INyxCollectionClipboardCommands;
  LEntry: INyxCollectionClipboardCellInput;
  LCommand: INyxCollectionClipboardCommand;
  LCellCommand: INyxCollectionClipboardCellCommand;
  LRangeInput: INyxCollectionClipboardRangeInput;
  LRangeCommand: INyxCollectionClipboardRangeCommand;
  LRangeIntent: INyxCollectionCellRangeIntent;
  LGrid: TStringGrid;
  LData: INyxCollectionSnapshot;
  LField: TNyxColumnIdentity;
  LItem: TNyxItemRef;
  LPrevious: TKeyEvent;
  LSender: TObject;
  LRow: Integer;
  LColumn: Integer;
  LCount: Integer;
begin
  Result := False;
  LCommands := FClipboardCommands;
  LRangeIntent := GetCellRange;

  if (LCommands = nil) or (AKey <> VK_C) or
    not ((AShift * [ssCtrl, ssShift, ssAlt, ssMeta, ssSuper, ssAltGr] = [ssCtrl]) or
      (AShift * [ssCtrl, ssShift, ssAlt, ssMeta, ssSuper, ssAltGr] = [ssMeta])) then
  begin
    Exit;
  end;
  LLease := Self as INyxCollectionMount;
  LData := FRendered;
  LPrevious := FPreviousKey;
  LSender := Sender;

  if FHasColumnLayout then
  begin
    LSender := FGrid;
  end;

  if LRangeIntent <> nil then
  begin

    if not Supports(LCommands, INyxCollectionClipboardRangeInput, LRangeInput) then
    begin
      raise ENyxCollectionRange.Create('Collection clipboard cannot reserve original cell range input');
    end;
    LRangeCommand := LRangeInput.BeginRangeInput;
    LCommand := LRangeCommand;
  end
  else
  begin

    if not Supports(LCommands, INyxCollectionClipboardCellInput, LEntry) then
    begin
      raise ENyxCollection.Create('Collection clipboard cannot reserve original cell input');
    end;
    LCellCommand := LEntry.BeginCellInput;
    LCommand := LCellCommand;
  end;
  { Reserve the original input/epoch/intent/final fence BEFORE even an immutable
    source item getter. Resolution below cannot lend this key newer authority. }
  try
    LGrid := InputGrid(Sender);

    if (LGrid = nil) or LGrid.EditorMode then
    begin
      { Native editors retain their ordinary Ctrl+C selection/clipboard route. }
      Exit;
    end;
    Result := True;
    FRejectTextKey := True;
    FConsumedTextKey := True;

    if not LCommand.Current then
    begin
      Exit;
    end;

    if LRangeCommand <> nil then
    begin
      LRangeCommand.ResolveRange(LRangeIntent);
      LCommand.Prepare;
    end
    else
    begin
      LRow := LGrid.Row - 1;
      LColumn := CanonicalColumn(LGrid, LGrid.Col);
      LField := ClipboardField(LColumn);

      if (LData <> nil) and (LRow >= 0) and (LColumn >= 0) and LField.Defined then
      begin
        LCount := LData.Count;

        if LCommand.Current and (LRow < LCount) then
        begin
          LItem := LData.ItemAt(LRow).Ref;
          LCellCommand.ResolveCell(LItem, LField);

          if LCommand.Current then
          begin
            LCommand.Prepare;
          end;
        end;
      end;
    end;

    if LCommand.DispatchCurrent and Assigned(LPrevious) then
    begin
      LPrevious(LSender, AKey, AShift);
    end;
    LCommand.Complete((AKey = VK_C) and LCommand.DispatchCurrent);
  finally

    if Result then
    begin
      AKey := 0;
    end;
    LCommand.Retire;
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
  LPageRows: Integer;
begin

  if CellRangeKeyDown(Sender, AKey, AShift) then
  begin
    Exit;
  end;

  if ClipboardKeyDown(Sender, AKey, AShift) then
  begin
    Exit;
  end;

  if FHasColumnLayout and not GetInputReady then
  begin
    AKey := 0;
    Exit;
  end;

  if FHasColumnLayout then
  begin
    ArrangedKeyDown(Sender, AKey, AShift);
    Exit;
  end;
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
    VK_HOME, VK_END, VK_PRIOR, VK_NEXT, VK_SPACE, VK_RETURN, VK_TAB, VK_ESCAPE]) then
  begin
    ResetTypeAhead;
  end;

  if not GetInputReady then
  begin
    AKey := 0;
    Exit;
  end;

  if not FUpdating and FEnabled and (AKey <> 0) and (FGrid <> nil) and
    not FGrid.EditorMode and (FQueries <> nil) and (ssAlt in AShift) and
    (AKey = VK_DOWN) then
  begin
    AKey := 0;
    QueryColumn(FGrid.Col, True, False);
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
    end
    else if AKey in [VK_PRIOR, VK_NEXT] then
    begin
      { LCL forwards these editor keys to its grid default, which would leave
        the cell and commit its draft. A single-line editor has no text page;
        retain its draft/caret until the explicit editing session ends. }
      AKey := 0;
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
      VK_PRIOR:
        begin
          LMove := ngmPageUp;
        end;
      VK_NEXT:
        begin
          LMove := ngmPageDown;
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
      LPageRows := 1;

      if LMove in [ngmPageUp, ngmPageDown] then
      begin
        { LCL's visible-row cache excludes fixed headers and follows actual
          row geometry/resize. Match the shared one-row overlap policy without
          falling through to a second widget-owned selection/default. }
        LPageRows := Max(1, FGrid.VisibleRowCount - 1);
      end;
      LDestination := MoveNyxGridCell(LTarget, LColumn, Length(LOrder),
        FView.Spec.Count, LMove, LPageRows);
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

  if FHasColumnLayout and not GetInputReady then
  begin
    AKey := '';
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if Assigned(FPreviousUTF8) then
  begin
    FPreviousUTF8(Sender, AKey);
  end;

  if not GetInputReady then
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

  if FUpdating or FTreeCommitQueued or FTreeComposing or not GetInputReady then
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

  if GetInputReady and Assigned(FPreviousTreeSelection) then
  begin
    FPreviousTreeSelection(Sender);
  end;
end;
