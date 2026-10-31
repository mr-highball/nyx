{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE.
  Readonly clipboard status shares the table's existing header and retirement
  frame. Its width-aware band is admitted with services, not resized per outcome.
  Raw diagnostics stay in the copied feedback record; visible control characters
  use the same reversible notation as ordinary cell inspection. }

function TNativeMount.ToolbarLayout(ARecoveryCount: Integer): TNativeTableToolbarLayout;
var
  LScale: Double;
  LMargin: Integer;
  LGap: Integer;
  LLeft: Integer;
  LRight: Integer;
  LWidth: Integer;
  LRowHeight: Integer;
  LButtonHeight: Integer;

  procedure Mirror(var ABounds: TRect);
  var
    LPreviousLeft: Integer;
  begin

    if FGrid.UseRightToLeftAlignment and not IsRectEmpty(ABounds) then
    begin
      LPreviousLeft := ABounds.Left;
      ABounds.Left := FGrid.ClientWidth - ABounds.Right;
      ABounds.Right := FGrid.ClientWidth - LPreviousLeft;
    end;
  end;
begin
  Result := Default(TNativeTableToolbarLayout);

  if FGrid = nil then
  begin
    Exit;
  end;
  LScale := Max(1, FGrid.Font.PixelsPerInch) / 96;
  LMargin := Max(1, Round(4 * LScale));
  LGap := Max(1, Round(8 * LScale));
  LRowHeight := Max(1, Round(40 * LScale));
  LButtonHeight := Max(1, LRowHeight - 2 * LMargin);
  LLeft := LMargin;
  LRight := Max(LLeft, FGrid.ClientWidth - LMargin);

  if FColumnManager <> nil then
  begin
    LWidth := Max(0, Min(Round(112 * LScale), LRight - LLeft));
    Result.Columns := Rect(LLeft, LMargin, LLeft + LWidth, LMargin + LButtonHeight);
    Inc(LLeft, LWidth + LGap);
    Result.Height := LRowHeight;
  end;

  if ARecoveryCount > 0 then
  begin
    LWidth := Max(0, Min(Round(160 * LScale), LRight - LLeft));
    Result.Recovery := Rect(LLeft, LMargin, LLeft + LWidth, LMargin + LButtonHeight);
    Inc(LLeft, LWidth + LGap);
    Result.Height := LRowHeight;
  end;

  if HasClipboardFeedback then
  begin
    { Outcomes never change this decision. Keep the complete readonly memo and
      its own scrolling; do not shrink fonts or replace text with a tooltip. }

    if (Result.Height > 0) and (LRight - LLeft >= Round(160 * LScale)) then
    begin
      Result.Status := Rect(LLeft, LMargin, LRight, LMargin + LButtonHeight);
    end
    else
    begin
      Result.Status := Rect(LMargin, Result.Height + Max(1, Round(2 * LScale)),
        LRight, Result.Height + Max(1, Round(34 * LScale)));
      Inc(Result.Height, Max(1, Round(36 * LScale)));
    end;
  end;
  Mirror(Result.Columns);
  Mirror(Result.Recovery);
  Mirror(Result.Status);
end;

function TNativeMount.RequiredToolbarHeight(ARecoveryCount: Integer): Integer;
var
  LLayout: TNativeTableToolbarLayout;
begin
  LLayout := ToolbarLayout(ARecoveryCount);
  Result := LLayout.Height;
end;

procedure TNativeMount.ApplyToolbarHeight(AHeight: Integer);
begin
  FColumnToolbarHeight := AHeight;
  FColumnToolbarApplied := AHeight > 0;
  FCustomHeaderApplied := True;
  TNyxCollectionStringGrid(FGrid).ColumnToolbarHeight := AHeight;
  ReserveHeader;
end;

procedure TNativeMount.ClipboardFeedbackChanged;
begin
  FClipboardStatusPending := True;

  if RecoveryPresentationReady then
  begin
    SynchronizeClipboardFeedback;
  end;
  ArmCellRefreshIdle;
end;

procedure TNativeMount.SynchronizeClipboardFeedback;
var
  LLease: INyxCollectionMount;
  LInput: INyxCollectionInputLifetime;
  LFrame: INyxNativeGestureFrame;
  LGrid: TStringGrid;
  LStatus: TMemo;
  LFeedback: TNyxCollectionClipboardFeedback;
  LText: TNyxText;
  LToolbar: TNativeTableToolbarLayout;

  function Current: Boolean;
  begin
    Result := LInput.Current and (GetInputLifetime = LInput) and
      (FGrid = LGrid) and (FColumnFrame = LFrame) and (FClipboardStatus = LStatus) and
      (GetClipboardFeedback.Serial = LFeedback.Serial) and
      (GetClipboardFeedback.Phase = LFeedback.Phase);
  end;

begin
  LLease := Self as INyxCollectionMount;

  if FClipboardStatusSyncing or not InputLifetimeCurrent or (FGrid = nil) or
    not (FGrid is TNyxCollectionStringGrid) or (FColumnFrame = nil) or
    ColumnInputBusy or FUpdating or FEditsSuspended or FDraftRecoveryHeld then
  begin
    Exit;
  end;

  if not HasClipboardFeedback then
  begin
    FClipboardStatusPending := False;
    Exit;
  end;
  LInput := GetInputLifetime;
  LFrame := FColumnFrame;
  LGrid := FGrid;
  LStatus := FClipboardStatus;
  LFeedback := GetClipboardFeedback;
  LText := NyxCollectionCellInspectionText(NyxCollectionClipboardFeedbackText(LFeedback));
  FClipboardStatusSyncing := True;
  FClipboardStatusPending := False;
  LFrame.Enter;
  try
    LToolbar := ToolbarLayout(GetRecoveredEditCount);

    if FColumnToolbarHeight <> LToolbar.Height then
    begin
      ApplyToolbarHeight(LToolbar.Height);

      if not Current then
      begin
        Exit;
      end;
    end;

    if LStatus = nil then
    begin
      LStatus := TMemo.Create(LGrid);
      FClipboardStatus := LStatus;
      LStatus.Name := 'NyxClipboardStatus';
      LStatus.ReadOnly := True;
      LStatus.TabStop := True;
      LStatus.WantTabs := False;
      LStatus.WordWrap := True;
      LStatus.ScrollBars := ssAutoVertical;
      LStatus.BorderStyle := bsNone;
      LStatus.AccessibleName := 'Clipboard result';
      LStatus.Parent := LGrid;

      if not Current then
      begin
        Exit;
      end;
    end;
    { Width, recovery membership and native direction must move the complete
      composition together. In particular, releasing the last recovered edit
      must not put the newly inline status over an older action position. }

    if (FColumnsButton <> nil) and FColumnsButton.Visible then
    begin
      FColumnsButton.SetBounds(LToolbar.Columns.Left, LToolbar.Columns.Top,
        LToolbar.Columns.Width, LToolbar.Columns.Height);

      if not Current then
      begin
        Exit;
      end;
    end;

    if (FRecoveryButton <> nil) and FRecoveryButton.Visible then
    begin
      FRecoveryButton.SetBounds(LToolbar.Recovery.Left, LToolbar.Recovery.Top,
        LToolbar.Recovery.Width, LToolbar.Recovery.Height);

      if not Current then
      begin
        Exit;
      end;
    end;
    LStatus.SetBounds(LToolbar.Status.Left, LToolbar.Status.Top,
      LToolbar.Status.Width, LToolbar.Status.Height);

    if not Current then
    begin
      Exit;
    end;
    LStatus.Visible := GetClipboardFeedbackEnabled;

    if not Current or not LStatus.Visible then
    begin
      Exit;
    end;
    LStatus.Font.Assign(LGrid.TitleFont);

    if not Current or (LStatus.Handle = 0) then
    begin
      Exit;
    end;

    if not Current then
    begin
      Exit;
    end;

    if NyxLCLInputText(LStatus) <> LText then
    begin
      { Use the existing Unicode host boundary, including supplementary text.
        No editor draft/selection or row/source notification is consulted. }
      RestoreNyxLCLInputText(LStatus, LText);
    end;

    if not Current then
    begin
      Exit;
    end;
    LStatus.Hint := LText;
    LStatus.ShowHint := True;
    LStatus.AccessibleDescription := LText;
  finally
    FClipboardStatusSyncing := False;

    if FClipboardStatusPending then
    begin
      ArmCellRefreshIdle;
    end;
    { Target retirement may destroy controls; it is the last outward action. }
    LFrame.Leave;
  end;
end;
