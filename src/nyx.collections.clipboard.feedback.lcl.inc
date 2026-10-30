{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE.
  Readonly clipboard status shares the table's existing header and retirement
  frame. Its fixed band is admitted with services, not resized per outcome.
  Raw diagnostics stay in the copied feedback record; visible control characters
  use the same reversible notation as ordinary cell inspection. }

function TNativeMount.ClipboardStatusHeight: Integer;
begin
  Result := 0;

  if HasClipboardFeedback and (FGrid <> nil) then
  begin
    Result := Round(36 * Max(1, FGrid.Font.PixelsPerInch) / 96);
  end;
end;

function TNativeMount.RequiredToolbarHeight(ARecoveryCount: Integer): Integer;
begin
  Result := ClipboardStatusHeight;

  if (FGrid <> nil) and ((FColumnManager <> nil) or (ARecoveryCount > 0)) then
  begin
    Inc(Result, Round(40 * Max(1, FGrid.Font.PixelsPerInch) / 96));
  end;
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
  LScale: Double;
  LToolbar: Integer;
  LHeight: Integer;

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
    LScale := Max(1, LGrid.Font.PixelsPerInch) / 96;
    LToolbar := RequiredToolbarHeight(GetRecoveredEditCount);

    if FColumnToolbarHeight <> LToolbar then
    begin
      ApplyToolbarHeight(LToolbar);

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
    LHeight := ClipboardStatusHeight;
    LStatus.SetBounds(Round(4 * LScale),
      FColumnToolbarHeight - LHeight + Round(2 * LScale),
      Max(1, LGrid.ClientWidth - Round(8 * LScale)),
      Max(1, LHeight - Round(4 * LScale)));

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
