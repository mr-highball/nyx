{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE.
  Included in the existing mount owner; recovery retains copied values only. }

function NyxCollectionDraftRecovery(const AOwner: IInterface): INyxCollectionDraftRecovery;
var
  LMount: INyxCollectionDraftRecoveryMount;
begin
  Result := nil;

  if (AOwner <> nil) and Supports(AOwner, INyxCollectionDraftRecoveryMount, LMount) then
  begin
    Result := LMount.GetDraftRecovery;
  end;
end;

function TNyxCollectionMountBase.GetDraftRecovery: INyxCollectionDraftRecovery;
var
  LWriter: INyxCollectionDraftRecoveryWriter;
  LBudget: INyxCollectionDraftRecoveryBudget;
  LChanges: INyxCollectionDraftRecoveryChanges;
begin

  if FRecoveryWriter = nil then
  begin
    LWriter := NewNyxCollectionDraftRecovery;

    if FRecoveryPolicy.Defined then
    begin
      LWriter.Recovery.ConfigureWarnings(NyxDraftRecoveryWarnings
        .Entries(FRecoveryPolicy.WarningEntries).Bytes(FRecoveryPolicy.WarningTextBytes));
      LBudget := NyxCollectionDraftRecoveryBudget(LWriter.Recovery);
      LBudget.ConfigureLimits(NyxDraftRecoveryLimits
        .Entries(FRecoveryPolicy.MaximumEntries).PayloadBytes(FRecoveryPolicy.MaximumPayloadBytes));
    end;
    FRecoveryWriter := LWriter;

    if (FView = nil) or FDisconnecting then
    begin
      FRecoveryWriter.Retire;
    end
    else if Supports(LWriter, INyxCollectionDraftRecoveryChanges, LChanges) then
    begin
      { The writer borrows this receiver only while its mount-owned token is
        connected. Disconnect revokes it before any target/receiver teardown. }
      FRecoverySubscription := LChanges.Subscribe(
        {$ifdef PAS2JS}@{$endif}RecoveryJournalChanged);
    end;
  end;
  Result := FRecoveryWriter.Recovery;
end;

procedure TRecoveryReviewReadiness.CheckReady;
begin

  if (Input = nil) or not Input.Ready or (Epoch <= 0) or
    (Epoch >= High(Integer)) or (Input.Epoch <> Epoch) then
  begin
    raise ENyxSchedule.Create('The original recovery review input is no longer ready');
  end;
end;

function TNyxCollectionMountBase.GetRecoveredEditCount: Integer;
var
  LJournal: INyxCollectionDraftRecovery;
begin
  Result := 0;

  if not InputLifetimeCurrent or (FRecoveryWriter = nil) then
  begin
    Exit;
  end;
  LJournal := FRecoveryWriter.Recovery;
  Result := LJournal.Capture.Count;
end;

function TNyxCollectionMountBase.OpenRecoveredEdits: Boolean;
var
  LLease: INyxCollectionMount;
  LActions: INyxCollectionCellActions;
  LRecovery: INyxCollectionCellRecoveryActions;
  LReady: TRecoveryReviewReadiness;
  LReadyLease: INyxCancellationReadiness;
  LAllowCopy: Boolean;
begin
  Result := False;
  LLease := Self as INyxCollectionMount;
  LActions := FCellActions;

  if not GetInputReady or (LActions = nil) or not FEnabled or
    not Supports(LActions, INyxCollectionCellRecoveryActions, LRecovery) then
  begin
    Exit;
  end;
  LReady := TRecoveryReviewReadiness.Create;
  LReadyLease := LReady;
  LReady.Input := GetInputLifetime;
  LReady.Epoch := LReady.Input.Epoch;
  LAllowCopy := (FClipboardCommands <> nil) and (nccaCell in FClipboardOptions.Actions);

  if GetInputReady and (FCellActions = LActions) then
  begin
    Result := LRecovery.OpenRecovery(LReadyLease, LAllowCopy);
  end;
end;

function NyxCollectionDraftRecoveryHeld(const AOwner: IInterface): Boolean;
var
  LStatus: INyxCollectionDraftRecoveryStatus;
begin
  Result := (AOwner <> nil) and Supports(AOwner, INyxCollectionDraftRecoveryStatus, LStatus);

  if Result then
  begin
    Result := LStatus.DraftRecoveryHeld;
  end;
end;

procedure TNyxCollectionMountBase.CaptureRecoveryContinuity(
  const AChanges: INyxCollectionChanges);
var
  LView: INyxCollectionView;
  LBefore: INyxCollectionSnapshot;
  LAfter: INyxCollectionSnapshot;
  LFrame: INyxCollectionSnapshot;
  LContinuity: INyxCollectionDisplayContinuity;
  LNext: TNyxItemRefs;
  LSeen: TNyxTextIndex;
  LItem: TNyxItemRef;
  LMutation: Integer;
  LFrom: Integer;
  LTo: Integer;
  LCount: Integer;
  LIndex: Integer;
  LReset: Boolean;

  function Current: Boolean;
  begin
    Result := InputLifetimeCurrent and (FView = LView) and
      (FLastSnapshot = LFrame) and (FCellMutationSerial = LMutation);
  end;

begin
  LView := FView;
  LFrame := FLastSnapshot;
  LMutation := FCellMutationSerial;

  if (AChanges = nil) or (LFrame = nil) or not Current then
  begin
    Exit;
  end;
  LBefore := AChanges.Before;
  LAfter := AChanges.After;

  if not Current or (LBefore = nil) or (LAfter = nil) or
    (LBefore.Key.Name <> LFrame.Key.Name) or (LAfter.Key.Name <> LFrame.Key.Name) then
  begin
    Exit;
  end;
  LFrom := LFrame.Revision;
  LTo := LFrom;
  LReset := False;
  LNext := nil;
  LContinuity := FRecoveryContinuity;

  if (LContinuity <> nil) and (LContinuity.Collection.Name = LFrame.Key.Name) and
    (LContinuity.FromRevision = LFrom) then
  begin
    LTo := LContinuity.ToRevision;
    LReset := LContinuity.Reset;
    LNext := Copy(FRecoveryRemoved, 0, Length(FRecoveryRemoved));
  end;

  if LAfter.Revision <= LTo then
  begin
    Exit;
  end;
  LReset := LReset or (LBefore.Revision <> LTo);
  LTo := LAfter.Revision;
  LSeen := TNyxTextIndex.Create;
  try
    for LIndex := 0 to High(LNext) do
    begin
      LSeen.AddFirst(LNext[LIndex].ID, LIndex);
    end;

    if not LReset then
    begin
      for LIndex := 0 to AChanges.Count - 1 do
      begin

        if AChanges.Kind(LIndex) <> nceRemove then
        begin
          Continue;
        end;
        LItem := AChanges.ItemRef(LIndex);

        if not LFrame.Has(LItem) or (LSeen.IndexOf(LItem.ID) >= 0) then
        begin
          Continue;
        end;
        LCount := Length(LNext);

        if LCount >= FRecoveryIdentityLimit then
        begin
          LReset := True;
          LNext := nil;
          Break;
        end;
        SetLength(LNext, LCount + 1);
        LNext[LCount] := LItem;
        LSeen.AddFirst(LItem.ID, LCount);
      end;
    end;
    LContinuity := NewNyxCollectionDisplayContinuity(LFrame.Key,
      LFrom, LTo, LNext, LReset);

    if Current then
    begin
      FRecoveryContinuity := LContinuity;
      FRecoveryRemoved := LNext;
    end;
  finally
    LSeen.Free;
  end;
end;

function TNyxCollectionMountBase.GetDraftRecoveryHeld: Boolean;
begin
  Result := InputLifetimeCurrent and FDraftRecoveryHeld;
end;

procedure TNyxCollectionMountBase.RecoveryJournalChanged(
  const AObservation: TNyxDraftRecoveryObservation);
var
  LLease: INyxCollectionMount;
begin
  LLease := Self as INyxCollectionMount;
  try
    RecoveryMembershipChanged(AObservation);
  finally
    { A capture inside Refresh is followed by its completed tail. Explicit
      journal discards outside that tail still update an observing panel now. }

    if not FUpdating then
    begin
      PublishRecoveryStatus;
    end;
  end;
end;

function TNyxCollectionMountBase.CaptureRecoveryStatus: TNyxCollectionRecoveryStatus;
var
  LLease: INyxCollectionMount;
  LJournal: INyxCollectionDraftRecovery;
begin
  LLease := Self as INyxCollectionMount;
  Result := Default(TNyxCollectionRecoveryStatus);

  if FRecoveryWriter <> nil then
  begin
    LJournal := FRecoveryWriter.Recovery;
    Result.Journal := LJournal.Capture;
  end;
  { A foreign journal reader can retire the mount. Flags are sampled AFTER
    that callout; no borrowed target or outward reader follows these facts. }
  Result.Active := InputLifetimeCurrent;
  Result.Held := Result.Active and FDraftRecoveryHeld;
  Result.InputReady := GetInputReady;

  if not Result.Active then
  begin
    Result.Journal.Active := False;
  end;
end;

function TNyxCollectionMountBase.SubscribeRecoveryStatus(
  AObserver: TNyxCollectionRecoveryObserver): INyxCollectionSubscription;
var
  LLease: INyxCollectionMount;
  LStatus: INyxCollectionRecoveryStatusWriter;
begin
  LLease := Self as INyxCollectionMount;

  if not InputLifetimeCurrent then
  begin
    raise ENyxCollection.Create('Recovery status requires a current mount');
  end;

  if FRecoveryStatus = nil then
  begin
    LStatus := NewNyxCollectionRecoveryStatus;
    LStatus.Publish(CaptureRecoveryStatus);

    if not InputLifetimeCurrent then
    begin
      LStatus.Retire;
      raise ENyxCollection.Create('Recovery status mount retired while connecting');
    end;
    FRecoveryStatus := LStatus;
  end;
  Result := FRecoveryStatus.SubscribeRecoveryStatus(AObserver);
end;

procedure TNyxCollectionMountBase.PublishRecoveryStatus;
var
  LStatus: INyxCollectionRecoveryStatusWriter;
  LObservation: TNyxCollectionRecoveryStatus;
begin
  LStatus := FRecoveryStatus;

  if (LStatus = nil) or not InputLifetimeCurrent then
  begin
    Exit;
  end;
  LObservation := CaptureRecoveryStatus;

  if InputLifetimeCurrent and (FRecoveryStatus = LStatus) then
  begin
    LStatus.Publish(LObservation);
  end;
end;

function TNyxCollectionMountBase.ReadRecoveryDraft(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity; out ACell: TNyxCollectionCellEdit): Boolean;
begin
  ACell := Default(TNyxCollectionCellEdit);
  Result := False;
end;

function TNyxCollectionMountBase.DiscardRecoveryDraft(
  const ACell: TNyxCollectionCellEdit): Boolean;
begin
  Result := False;
end;

function TNyxCollectionMountBase.CapturePendingDraft: INyxPendingCollectionDraft;
var
  LKeepAlive: INyxCollectionMount;
  LCell: TNyxCollectionCellEdit;
  LFrame: TNyxDraftRecoveryFrame;
begin
  Result := nil;
  LKeepAlive := Self as INyxCollectionMount;

  if not GetDraftRecoveryHeld or FUpdating or FEditsSuspended or
    not FRecoveryPendingFrame.Defined then
  begin
    Exit;
  end;
  LFrame := FRecoveryPendingFrame;

  if ReadRecoveryDraft(FRecoveryPendingItem, FRecoveryPendingField, LCell) and
    GetDraftRecoveryHeld and not FUpdating and not FEditsSuspended and
    LFrame.Same(FRecoveryPendingFrame) then
  begin
    Result := NewNyxPendingCollectionDraft(LCell, LFrame);
  end;
end;

function TNyxCollectionMountBase.DiscardPendingDraft(
  const AExpected: INyxPendingCollectionDraft): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LCurrent: INyxPendingCollectionDraft;
  LFrame: TNyxDraftRecoveryFrame;
  LCell: TNyxCollectionCellEdit;
begin
  Result := False;
  LKeepAlive := Self as INyxCollectionMount;

  if (AExpected = nil) or not GetDraftRecoveryHeld or FUpdating or FEditsSuspended then
  begin
    Exit;
  end;
  { Alternative receipt getters may call out. Copy both values before checking
    the current original mount/frame again; no borrowed target is held here. }
  LFrame := AExpected.Frame;
  LCell := AExpected.Cell;
  LCurrent := CapturePendingDraft;

  if (LCurrent = nil) or not LFrame.Same(FRecoveryPendingFrame) or
    not NyxSameCollectionDraft(LCell, LCurrent.Cell) or not GetDraftRecoveryHeld then
  begin
    Exit;
  end;
  Result := DiscardRecoveryDraft(LCell);

  if Result then
  begin
    FRecoveryPendingFrame := Default(TNyxDraftRecoveryFrame);
    FRecoveryPendingItem := Default(TNyxItemRef);
  end;
end;

function TNyxCollectionMountBase.RecoverCell(const ACell: TNyxCollectionCellEdit;
  const ABefore, AAfter: INyxCollectionSnapshot;
  out ALoss: TNyxCollectionDraftLoss; AHasDraft: Boolean): Boolean;
var
  LView: INyxCollectionView;
  LSource: INyxCollectionSnapshot;
  LInspection: INyxCollectionInspection;
  LPosition: INyxCollectionPositionView;
  LContinuity: INyxCollectionDisplayContinuity;
  LSpec: TNyxCollectionViewSpec;
  LReason: TNyxCollectionDraftLoss;
  LMutation: Integer;
  LColumn: Integer;
  LIndex: Integer;
  LKnown: Boolean;
  LRetired: Boolean;

  function Current: Boolean;
  begin
    Result := InputLifetimeCurrent and (FView = LView);

    if Result and (FCellMutationSerial <> LMutation) then
    begin
      raise ENyxCollection.Create('Collection changed during draft recovery');
    end;
  end;

begin
  Result := False;
  LView := FView;
  LMutation := FCellMutationSerial;

  if not Current or not ACell.Defined or (ABefore = nil) or (AAfter = nil) then
  begin
    Exit;
  end;
  ACell.Validate;
  LSpec := LView.Spec;

  if not Current then
  begin
    Exit;
  end;
  LColumn := LSpec.ColumnIndex(ACell.Field);

  if FNormalizeValues and (FNormalizeItem.ID = ACell.Item.ID) and
    (FNormalizeColumn = LColumn) then
  begin
    { The user's own successful edit/cancel normalization is deliberate, not
      an external producer overwriting unfinished work. }
    Exit;
  end;
  LInspection := NyxCollectionInspection(LView);

  if not Current then
  begin
    Exit;
  end;

  if LInspection <> nil then
  begin
    LSource := LInspection.GetDisplayedSource;
  end
  else
  begin
    LSource := LView.Store.Snapshot;
  end;

  if not Current then
  begin
    Exit;
  end;

  if (LSource = nil) or (LSource.Key.Name <> ACell.Item.Collection.Name) or
    (LSource.Revision <> AAfter.Revision) then
  begin
    raise ENyxCollection.Create('Draft recovery requires one current displayed source');
  end;
  LKnown := ABefore.Revision = LSource.Revision;
  LRetired := False;

  if not LKnown and Supports(LView, INyxCollectionPositionView, LPosition) then
  begin

    if not Current then
    begin
      Exit;
    end;
    LContinuity := LPosition.CaptureDisplayContinuity;

    if not Current then
    begin
      Exit;
    end;

    if LContinuity <> nil then
    begin
      LKnown := (LContinuity.Collection.Name = LSource.Key.Name) and
        (LContinuity.FromRevision = ABefore.Revision) and
        (LContinuity.ToRevision = LSource.Revision) and not LContinuity.Reset;

      if LKnown then
      begin
        LRetired := not LContinuity.Retains(ACell.Item);
      end;
    end;
  end;

  if not LKnown and (FRecoveryContinuity <> nil) then
  begin
    { View continuity remains an opt-in inspection capability. This private
      proof covers only this mount's uninstalled physical frame and survives a
      composition/pressure retry without retaining source change packets. }
    LContinuity := FRecoveryContinuity;
    LKnown := (LContinuity.Collection.Name = LSource.Key.Name) and
      (LContinuity.FromRevision = ABefore.Revision) and
      (LContinuity.ToRevision = LSource.Revision) and not LContinuity.Reset;

    if LKnown then
    begin
      LRetired := not LContinuity.Retains(ACell.Item);
    end;
  end;

  if not LKnown and (FChanges <> nil) and
    (FChanges.Before.Revision = ABefore.Revision) and
    (FChanges.After.Revision = LSource.Revision) then
  begin
    LKnown := True;
    for LIndex := 0 to FChanges.Count - 1 do
    begin

      if (FChanges.Kind(LIndex) = nceRemove) and
        (FChanges.ItemRef(LIndex).ID = ACell.Item.ID) then
      begin
        LRetired := True;
        Break;
      end;
    end;
  end;

  if not LSource.Has(ACell.Item) then
  begin
    LReason := ncdlRowRemoved;
  end
  else if LRetired then
  begin
    LReason := ncdlIdentityReplaced;
  end
  else if not LKnown then
  begin
    LReason := ncdlUnknownIdentity;
  end
  else if LColumn < 0 then
  begin
    LReason := ncdlFieldUnavailable;
  end
  else if not LSpec.ColumnAt(LColumn).Read(LSource.Item(ACell.Item)).SameValue(ACell.Accepted) then
  begin
    LReason := ncdlValueChanged;
  end
  else if not AAfter.Has(ACell.Item) then
  begin
    LReason := ncdlHidden;
  end
  else
  begin
    Current;
    Exit;
  end;

  if not Current then
  begin
    Exit;
  end;

  if AHasDraft then
  begin
    PreserveCell(ACell, LReason, ABefore.Revision, LSource.Revision);
  end;
  ALoss := LReason;
  Result := True;
end;

procedure TNyxCollectionMountBase.PreserveCell(const ACell: TNyxCollectionCellEdit;
  AReason: TNyxCollectionDraftLoss; ABeforeRevision, AAfterRevision: Integer);
var
  LJournal: INyxCollectionDraftRecovery;
  LReceipt: INyxRecoveredCollectionDraft;
  LScoped: INyxCollectionDraftRecoveryScopedWriter;
begin
  { Admission and scoped deduplication are shared with ordinary RecoverCell.
    A refused target restoration must not invent a second recovery store. }
  LJournal := GetDraftRecovery;

  if not FRecoveryFrame.Defined then
  begin
    FRecoveryFrame := NewNyxDraftRecoveryFrame;
  end;
  try

    if Supports(FRecoveryWriter, INyxCollectionDraftRecoveryScopedWriter, LScoped) then
    begin
      LReceipt := LScoped.PreserveInFrame(ACell, AReason, ABeforeRevision,
        AAfterRevision, FRecoveryFrame);
    end
    else
    begin
      LReceipt := FRecoveryWriter.Preserve(ACell, AReason, ABeforeRevision, AAfterRevision);
    end;
  except
    on LPressure: ENyxDraftRecoveryPressure do
    begin

      if not FRecoveryPendingFrame.Defined or
        (FRecoveryPendingItem.Collection.Name <> ACell.Item.Collection.Name) or
        (FRecoveryPendingItem.ID <> ACell.Item.ID) or
        not FRecoveryPendingField.Same(ACell.Field) then
      begin
        FRecoveryPendingFrame := NewNyxDraftRecoveryFrame;
      end;
      FRecoveryPendingItem := ACell.Item;
      FRecoveryPendingField := ACell.Field;
      raise;
    end;
  end;
end;
