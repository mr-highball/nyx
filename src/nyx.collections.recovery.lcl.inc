{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE.
  Native recovery launcher shares the existing canonical table-toolbar command
  and deferred gesture frame. No native data store or recovery dialog is added. }

function TNativeMount.RecoveryPresentationReady: Boolean;
begin
  Result := InputLifetimeCurrent and not FUpdating and not FEditsSuspended and
    not FDraftRecoveryHeld and not FLayoutGesture and (FColumnFrame <> nil) and
    not FColumnFrame.Dispatching and not ColumnInputBusy and
    (FGrid is TNyxCollectionStringGrid) and
    not TNyxCollectionStringGrid(FGrid).ValidationActive and
    ((FStartPane = nil) or not FStartPane.Grid.ValidationActive) and
    ((FEndPane = nil) or not FEndPane.Grid.ValidationActive);
end;

procedure TNativeMount.RecoveryMembershipChanged(
  const AObservation: TNyxDraftRecoveryObservation);
var
  LLease: INyxCollectionMount;
begin
  LLease := Self as INyxCollectionMount;

  if not InputLifetimeCurrent then
  begin
    Exit;
  end;
  FRecoveryCountPending := True;

  if not FUpdating and not FEditsSuspended and not FDraftRecoveryHeld then
  begin

    if RecoveryPresentationReady then
    begin
      SynchronizeRecovery;
    end;
    ArmCellRefreshIdle;
  end;
end;

function TNativeMount.PrepareRecoveryButton(Sender: TObject): INyxGridColumnButtonCommand;
var
  LLease: INyxCollectionMount;
begin
  Result := nil;
  LLease := Self as INyxCollectionMount;

  if not GetInputReady or FUpdating or ColumnInputBusy or
    (FCellActions = nil) or (FRecoveryButton = nil) or
    (Sender <> FRecoveryButton) or not FRecoveryButton.Visible or
    not FRecoveryButton.Enabled or (FColumnsButtonSerial = High(Integer)) then
  begin
    Exit;
  end;

  if not PrepareColumnButtonDraft(Sender) then
  begin
    Exit;
  end;
  Inc(FColumnsButtonSerial);
  Result := TNativeColumnManagerCommand.Create(Self, FColumnsButtonSerial, True);
end;

procedure TNativeMount.SynchronizeRecovery;
var
  LLease: INyxCollectionMount;
  LInput: INyxCollectionInputLifetime;
  LFrame: INyxNativeGestureFrame;
  LGrid: TStringGrid;
  LButton: TNyxCollectionColumnButton;
  LSurface: INyxGridInputSurface;
  LCount: Integer;
  LToolbar: TNativeTableToolbarLayout;

  function Current: Boolean;
  begin
    Result := LInput.Current and (GetInputLifetime = LInput) and
      (FGrid = LGrid) and (FRecoveryButton = LButton) and
      (FColumnFrame = LFrame);
  end;

begin
  LLease := Self as INyxCollectionMount;

  if FRecoverySyncing or not InputLifetimeCurrent or (FCellActions = nil) or
    (FGrid = nil) or not (FGrid is TNyxCollectionStringGrid) or
    (FColumnFrame = nil) or ColumnInputBusy then
  begin
    Exit;
  end;
  FRecoveryCountPending := False;
  LCount := GetRecoveredEditCount;

  if not InputLifetimeCurrent or ((LCount = 0) and (FRecoveryButton = nil)) then
  begin
    Exit;
  end;
  LInput := GetInputLifetime;
  LGrid := FGrid;
  LFrame := FColumnFrame;
  LButton := FRecoveryButton;
  FRecoverySyncing := True;
  LFrame.Enter;
  try
    LToolbar := ToolbarLayout(LCount);

    if FColumnToolbarHeight <> LToolbar.Height then
    begin
      ApplyToolbarHeight(LToolbar.Height);

      if not Current then
      begin
        Exit;
      end;
    end;

    if LButton = nil then
    begin
      LSurface := TNyxCollectionStringGrid(LGrid).ColumnEditorInputSurface;

      if not Current or (LSurface = nil) then
      begin
        Exit;
      end;
      LButton := TNyxCollectionColumnButton.Create(LGrid);
      FRecoveryButton := LButton;
      LButton.Visible := False;
      LButton.Name := 'NyxRecoveredEditsButton';
      LButton.ConfigureColumnAction(LInput, LSurface.Frame, LSurface.Continuity,
        PrepareRecoveryButton, PrepareFitButtonFocus);

      if not Current then
      begin
        Exit;
      end;
      LButton.OnKeyDown := FitButtonKeyDown;
      LButton.OnKeyUp := FitButtonKeyUp;
      TNyxCollectionStringGrid(LGrid).RegisterColumnButton(LButton);
    end;
    LButton.Parent := LGrid;

    if not Current then
    begin
      Exit;
    end;
    LButton.Caption := 'Recovered (' + IntToStr(LCount) + ')';
    LButton.AccessibleName := 'Review recovered edits';
    LButton.Hint := 'Review unfinished edits preserved when their original cell became unavailable';
    LButton.ShowHint := True;
    LButton.Font.Assign(LGrid.TitleFont);

    if not Current then
    begin
      Exit;
    end;
    LButton.TabStop := True;
    LButton.Enabled := FEnabled and GetInputReady;
    LButton.SetBounds(LToolbar.Recovery.Left, LToolbar.Recovery.Top,
      LToolbar.Recovery.Width, LToolbar.Recovery.Height);

    if not Current then
    begin
      Exit;
    end;
    LButton.Visible := LCount > 0;

    if Current then
    begin
      LButton.BringToFront;
    end;

    if Current then
    begin
      { A new/released recovered edit can wrap or unwrap the feedback row.
        Reposition that same memo without publishing data or replacing input. }
      SynchronizeClipboardFeedback;
    end;
  finally
    FRecoverySyncing := False;

    if FRecoveryCountPending then
    begin
      ArmCellRefreshIdle;
    end;
    { Deferred native retirement/destruction is the last outward action. }
    LFrame.Leave;
  end;
end;
