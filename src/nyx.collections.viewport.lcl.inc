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

{ Intersect ancestor client boxes in this control's coordinate plane. Showing
  rejects hidden/unparented windows; no parent is scrolled, shown or focused.
  The entered input frame guards borrowed controls across native handle calls.
  This is software clipping, not a claim about other windows covering this one. }
function NativeViewportClip(AControl: TWinControl;
  const AInput: INyxCollectionInputLifetime): TRect;
var
  LOrigin: TPoint;
  LParentOrigin: TPoint;
  LParent: TWinControl;
begin
  Result := Rect(0, 0, 0, 0);

  if not AInput.Current or not AControl.Showing then
  begin
    Exit;
  end;
  LOrigin := AControl.ClientToScreen(Point(0, 0));

  if not AInput.Current then
  begin
    Exit;
  end;
  Result := AControl.ClientRect;
  LParent := AControl.Parent;
  while (LParent <> nil) and (Result.Right > Result.Left) and
    (Result.Bottom > Result.Top) do
  begin
    LParentOrigin := LParent.ClientToScreen(Point(0, 0));

    if not AInput.Current then
    begin
      Exit(Rect(0, 0, 0, 0));
    end;
    Result.Left := Max(Result.Left, LParentOrigin.X - LOrigin.X);
    Result.Top := Max(Result.Top, LParentOrigin.Y - LOrigin.Y);
    Result.Right := Min(Result.Right,
      LParentOrigin.X - LOrigin.X + LParent.ClientWidth);
    Result.Bottom := Min(Result.Bottom,
      LParentOrigin.Y - LOrigin.Y + LParent.ClientHeight);
    LParent := LParent.Parent;
  end;
end;

{ Public native viewport properties do not require a private LCL scroll entry.
  The caller retains its original frame/input through setters. Alignment uses
  actual row/column extents, with native boundary clamping and no selection. }
procedure PlaceNativeViewportCell(AGrid: TStringGrid; AColumn, ARow: Integer;
  const AClip: TRect; const AOptions: TNyxCollectionRevealOptions;
  AHorizontal: Boolean; const AInput: INyxCollectionInputLifetime);
var
  LOptions: TGridOptions;
  LBefore: Integer;
  LFirst: Integer;
  LSize: Integer;
  LAvailable: Integer;
  LInset: Integer;
  LUsed: Integer;
  LRoom: Integer;
  LRect: TRect;
  LStartOutside: Boolean;
begin
  LOptions := AGrid.Options;
  try
    { This native option otherwise moves selection as a scroll side effect.
      Nyx's explicit viewport command owns no row-membership authority. }

    if goScrollKeepVisible in LOptions then
    begin
      AGrid.Options := LOptions - [goScrollKeepVisible];
    end;

    if not AInput.Current then
    begin
      Exit;
    end;

    LRect := AGrid.CellRect(AColumn, ARow);
    LAvailable := Max(0, AClip.Bottom - AClip.Top - AGrid.RowHeights[ARow]);
    LBefore := -1;
    case AOptions.VerticalAlignment of
      ncvaNearest:
      begin

        if (LRect.Top < AClip.Top) and (LRect.Bottom < AClip.Bottom) then
        begin
          LBefore := 0;
        end
        else if (LRect.Bottom > AClip.Bottom) and (LRect.Top > AClip.Top) then
        begin
          LBefore := LAvailable;
        end;
      end;
      ncvaStart: LBefore := 0;
      ncvaCenter: LBefore := LAvailable div 2;
      ncvaEnd: LBefore := LAvailable;
    end;

    if LBefore >= 0 then
    begin
      { Pinned panes and partially clipped parents can cover the native first
        cell. Leave enough preceding content to place this cell beyond that
        leading inset. Prefer a fully visible legal boundary when one fits. }
      LInset := Max(0, AClip.Top - AGrid.CellRect(AGrid.LeftCol, AGrid.TopRow).Top);
      LRoom := LInset + LAvailable;
      Inc(LBefore, LInset);
      LUsed := 0;
      LFirst := ARow;
      while LFirst > AGrid.FixedRows do
      begin
        LSize := Max(0, AGrid.RowHeights[LFirst - 1]);

        if LSize > LBefore then
        begin
          Break;
        end;
        Dec(LBefore, LSize);
        Inc(LUsed, LSize);
        Dec(LFirst);
      end;

      if (LUsed < LInset) and (LFirst > AGrid.FixedRows) then
      begin
        LSize := Max(0, AGrid.RowHeights[LFirst - 1]);

        if LUsed + LSize <= LRoom then
        begin
          Dec(LFirst);
        end;
      end;
      AGrid.TopRow := LFirst;
    end;

    if not AInput.Current or not AHorizontal then
    begin
      Exit;
    end;

    LRect := AGrid.CellRect(AColumn, ARow);
    LAvailable := Max(0, AClip.Right - AClip.Left - AGrid.ColWidths[AColumn]);
    LBefore := -1;
    case AOptions.HorizontalAlignment of
      ncvaNearest:
      begin

        if (LRect.Left > AClip.Left) or (LRect.Right < AClip.Right) then
        begin
          LStartOutside := LRect.Left < AClip.Left;

          if AGrid.UseRightToLeftAlignment then
          begin
            LStartOutside := LRect.Right > AClip.Right;
          end;

          if LStartOutside then
          begin
            LBefore := 0;
          end
          else if (LRect.Left < AClip.Left) or (LRect.Right > AClip.Right) then
          begin
            LBefore := LAvailable;
          end;
        end;
      end;
      ncvaStart: LBefore := 0;
      ncvaCenter: LBefore := LAvailable div 2;
      ncvaEnd: LBefore := LAvailable;
    end;

    if LBefore >= 0 then
    begin
      LRect := AGrid.CellRect(AGrid.LeftCol, AGrid.TopRow);
      LInset := Max(0, AClip.Left - LRect.Left);

      if AGrid.UseRightToLeftAlignment then
      begin
        LInset := Max(0, LRect.Right - AClip.Right);
      end;
      LRoom := LInset + LAvailable;
      Inc(LBefore, LInset);
      LUsed := 0;
      LFirst := AColumn;
      while LFirst > AGrid.FixedCols do
      begin
        LSize := Max(0, AGrid.ColWidths[LFirst - 1]);

        if LSize > LBefore then
        begin
          Break;
        end;
        Dec(LBefore, LSize);
        Inc(LUsed, LSize);
        Dec(LFirst);
      end;

      if (LUsed < LInset) and (LFirst > AGrid.FixedCols) then
      begin
        LSize := Max(0, AGrid.ColWidths[LFirst - 1]);

        if LUsed + LSize <= LRoom then
        begin
          Dec(LFirst);
        end;
      end;
      AGrid.LeftCol := LFirst;
    end;
  finally

    if AInput.Current and (AGrid.Options <> LOptions) then
    begin
      AGrid.Options := LOptions;
    end;
  end;
end;

function TNativeMount.SupportsViewportReveal: Boolean;
begin
  Result := (FList <> nil) or (FGrid is TNyxCollectionStringGrid);
end;

procedure TNativeMount.QueueViewportReveal;
begin
  ArmCellRefreshIdle;
end;

function TNativeMount.RevealViewportTarget(const AOptions: TNyxCollectionRevealOptions;
  ARow, AColumn: Integer; ATakeFocus: Boolean; out AFullyVisible: Boolean;
  out AReason: TNyxCollectionRevealReason): TNyxCollectionRevealStatus;
var
  LRequest: INyxCollectionReveal;
  LInput: INyxCollectionInputLifetime;
  LFrame: INyxNativeGestureFrame;
  LOuter: TNyxCollectionStringGrid;
  LGrid: TStringGrid;
  LList: TListBox;
  LPhysical: Integer;
  LClip: TRect;
  LRect: TRect;
  LUpdating: Boolean;
  LPass: Integer;
  LTop: Integer;
  LLeft: Integer;
  LFirst: Integer;
  LDelta: Double;

  function Current: Boolean;
  begin
    Result := ViewportRequestCurrent(LRequest) and LInput.Current;
  end;

  procedure ReadClip;
  var
    LPane: TColumnPane;
    LIndex: Integer;
  begin
    LClip := NativeViewportClip(LGrid, LInput);

    if not Current then
    begin
      Exit;
    end;
    LClip.Top := Max(LClip.Top, LGrid.CellRect(LPhysical, 0).Bottom);

    if LGrid = FGrid then
    begin
      for LIndex := 0 to 1 do
      begin
        LPane := FStartPane;

        if LIndex = 1 then
        begin
          LPane := FEndPane;
        end;

        if (LPane <> nil) and LPane.Grid.Visible then
        begin

          if LPane.Grid.Left <= 0 then
          begin
            LClip.Left := Max(LClip.Left, LPane.Grid.Left + LPane.Grid.Width);
          end
          else
          begin
            LClip.Right := Min(LClip.Right, LPane.Grid.Left);
          end;
        end;
      end;
    end;
  end;

begin
  Result := ncvsPending;
  AFullyVisible := False;
  AReason := ncvrNone;
  LRequest := CurrentViewportRequest;
  LInput := GetInputLifetime;

  if (FColumnEditing <> nil) and FColumnEditing.Composing then
  begin
    AReason := ncvrInputUnavailable;
    Exit(ncvsRefused);
  end;

  if FCellRefreshPending or FLayoutGesture or
    ((FColumnFrame <> nil) and FColumnFrame.Dispatching) then
  begin
    Exit;
  end;
  LList := FList;

  if LList <> nil then
  begin

    if (AColumn <> 0) or (LList.ItemHeight <= 0) or not LList.Showing or
      (LList.ClientHeight <= 0) then
    begin
      AReason := ncvrNoViewport;
      Exit(ncvsRefused);
    end;
    LUpdating := FUpdating;
    FUpdating := True;
    try
      LClip := NativeViewportClip(LList, LInput);

      if not Current then
      begin
        Exit;
      end;

      if (LClip.Bottom <= LClip.Top) or (LClip.Right <= LClip.Left) then
      begin
        AReason := ncvrNoViewport;
        Exit(ncvsRefused);
      end;
      LDelta := NyxCollectionRevealDelta((ARow - LList.TopIndex) * LList.ItemHeight,
        LList.ItemHeight, LClip.Top, LClip.Bottom - LClip.Top, AOptions.VerticalAlignment);
      LFirst := LList.TopIndex;

      if LDelta > 0 then
      begin
        Inc(LFirst, Ceil(LDelta / LList.ItemHeight));
      end
      else if LDelta < 0 then
      begin
        Inc(LFirst, Floor(LDelta / LList.ItemHeight));
      end;
      LList.TopIndex := Max(0, LFirst);

      if not Current or (FList <> LList) then
      begin
        Exit;
      end;
      LRect.Top := (ARow - LList.TopIndex) * LList.ItemHeight;
      LRect.Bottom := LRect.Top + LList.ItemHeight;

      if (LRect.Bottom <= LClip.Top) or (LRect.Top >= LClip.Bottom) then
      begin
        AReason := ncvrNoViewport;
        Exit(ncvsRefused);
      end;
      AFullyVisible := (LRect.Top >= LClip.Top) and (LRect.Bottom <= LClip.Bottom);

      if ATakeFocus and LList.CanFocus then
      begin
        { Final borrowed target call. OnEnter may retire this mount. }
        LList.SetFocus;
      end;
      Result := ncvsCompleted;
    finally
      FUpdating := LUpdating;
    end;
    Exit;
  end;
  LOuter := TNyxCollectionStringGrid(FGrid);
  LGrid := FGrid;
  LPhysical := AColumn;

  if FHasColumnLayout then
  begin
    LGrid := GridForColumn(AColumn, LPhysical);
  end;

  if (LGrid = nil) or not LGrid.Showing or (LPhysical < 0) or
    (ARow + 1 >= LGrid.RowCount) or (LGrid.ClientHeight <= 0) then
  begin
    AReason := ncvrNoViewport;
    Exit(ncvsRefused);
  end;
  LFrame := FColumnFrame;

  if LFrame <> nil then
  begin
    LFrame.Enter;
  end;
  LUpdating := FUpdating;
  FUpdating := True;
  LOuter.BeginColumnTransition;
  try
    for LPass := 0 to 2 do
    begin
      ReadClip;

      if not Current then
      begin
        Exit;
      end;

      if (LClip.Bottom <= LClip.Top) or (LClip.Right <= LClip.Left) then
      begin
        AReason := ncvrNoViewport;
        Exit(ncvsRefused);
      end;
      LTop := LGrid.TopRow;
      LLeft := LGrid.LeftCol;
      PlaceNativeViewportCell(LGrid, LPhysical, ARow + 1, LClip,
        AOptions, AOptions.Field.Defined, LInput);

      if not Current or (FGrid <> LOuter) then
      begin
        Exit;
      end;

      if LGrid <> FGrid then
      begin
        FGrid.TopRow := LGrid.TopRow;
      end;
      SynchronizeViewportMetrics;

      if not Current or (FGrid <> LOuter) then
      begin
        Exit;
      end;

      if (LTop = LGrid.TopRow) and (LLeft = LGrid.LeftCol) then
      begin
        Break;
      end;
    end;
    ReadClip;

    if not Current then
    begin
      Exit;
    end;
    LRect := LGrid.CellRect(LPhysical, ARow + 1);

    if (LRect.Bottom <= LClip.Top) or (LRect.Top >= LClip.Bottom) or
      (AOptions.Field.Defined and ((LRect.Right <= LClip.Left) or (LRect.Left >= LClip.Right))) then
    begin
      AReason := ncvrNoViewport;
      Exit(ncvsRefused);
    end;
    AFullyVisible := (LRect.Top >= LClip.Top) and (LRect.Bottom <= LClip.Bottom) and
      (not AOptions.Field.Defined or
        ((LRect.Left >= LClip.Left) and (LRect.Right <= LClip.Right)));

    if ATakeFocus then
    begin
      LGrid.Col := LPhysical;

      if not Current then
      begin
        Exit;
      end;
      LGrid.Row := ARow + 1;

      if not Current then
      begin
        Exit;
      end;
      FActiveGrid := LGrid;
      FFocusedCanonical := AColumn;

      if LGrid.CanFocus then
      begin
        LGrid.SetFocus;
      end;
    end;
    Result := ncvsCompleted;
  finally

    if LInput.Current then
    begin
      LOuter.EndColumnTransition;
    end;
    FUpdating := LUpdating;

    if LFrame <> nil then
    begin
      LFrame.Leave;
    end;
  end;
end;
