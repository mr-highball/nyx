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
begin

  if FRecoveryWriter = nil then
  begin
    FRecoveryWriter := NewNyxCollectionDraftRecovery;

    if (FView = nil) or FDisconnecting then
    begin
      FRecoveryWriter.Retire;
    end;
  end;
  Result := FRecoveryWriter.Recovery;
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
  LJournal: INyxCollectionDraftRecovery;
  LReceipt: INyxRecoveredCollectionDraft;
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
    LJournal := GetDraftRecovery;
    LReceipt := FRecoveryWriter.Preserve(ACell, LReason, ABefore.Revision, LSource.Revision);
  end;
  ALoss := LReason;
  Result := True;
end;
