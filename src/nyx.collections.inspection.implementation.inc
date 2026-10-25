{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE.
  Included in nyx.collections.view: inspection shares that view's accepted
  projection, atomic reservation and weak subscriptions, never a second store. }

function NyxCollectionInspection(const AView: INyxCollectionView): INyxCollectionInspection;
var
  LInspection: INyxCollectionInspection;
begin
  Result := nil;

  if (AView <> nil) and Supports(AView, INyxCollectionInspection, LInspection) then
  begin
    Result := LInspection;
  end;
end;

function TView.GetInspectionPolicy: TNyxCollectionInspectionPolicy;
begin
  Result := FInspectionPolicy.Copy;
end;

function TView.CaptureInspection: TNyxCollectionInspectionObservation;
begin
  { Cached scalar facts cannot call a provider, acquire authority or keep this
    owner alive after the returned value has been copied. }
  Result := FInspectionStatus;
end;

function TView.GetDisplayedSource: INyxCollectionSnapshot;
begin
  Result := FSource;
end;

procedure TView.ConfigureInspection(const APolicy: TNyxCollectionInspectionPolicy);
var
  LPolicy: TNyxCollectionInspectionPolicy;
  LLease: INyxCollectionView;
begin
  LLease := Self as INyxCollectionView;
  LPolicy := APolicy.Copy;

  if LPolicy.Defined then
  begin
    LPolicy.Validate;
  end;
  Writable;

  if LPolicy.Defined and (FAtomic = nil) then
  begin
    raise ENyxCollectionInspection.Create('Inspection requires a source with prepared publication');
  end;

  if FInspectionStatus.Mode = ncimPaused then
  begin

    if not LPolicy.Defined or
      (LPolicy.MaximumPausedBytes < FInspectionStatus.RetainedDataBytes) then
    begin
      raise ENyxCollectionInspection.Create('Resume before releasing the retained inspection frame');
    end;

    if Length(FInspectionRemoved) > LPolicy.IdentityLimit then
    begin
      { Reducing the budget is allowed, but forgotten removals can no longer
        prove that surviving IDs refer to their original runtime incarnations. }
      FInspectionRemoved := nil;
      FreeAndNil(FInspectionIndex);
      FInspectionStatus.RetainedIdentities := 0;
      FInspectionStatus.IdentityTrackingOverflow := True;
    end;
  end;
  FInspectionPolicy := LPolicy;
  FInspectionStatus.Configured := LPolicy.Defined;
  Notify(nil, True, ncicPolicyChanged);
end;

procedure TView.PauseInspection;
var
  LLease: INyxCollectionView;
  LBytes: Integer;
begin
  LLease := Self as INyxCollectionView;
  Writable;

  if not FInspectionPolicy.Defined or (FAtomic = nil) then
  begin
    raise ENyxCollectionInspection.Create('Configure an inspection policy on an atomic source first');
  end;

  if FInspectionStatus.Mode = ncimPaused then
  begin
    Exit;
  end;
  FInspectionTransition := True;
  try
    LBytes := FSource.DataBytes;

    if (LBytes < 0) or (LBytes > FInspectionPolicy.MaximumPausedBytes) then
    begin
      raise ENyxCollectionInspection.Create('The displayed source exceeds the paused payload budget');
    end;
    FInspectionRemoved := nil;
    FreeAndNil(FInspectionIndex);
    FInspectionStatus.Mode := ncimPaused;
    FInspectionStatus.PendingPublications := 0;
    FInspectionStatus.PendingCountSaturated := False;
    FInspectionStatus.RetainedDataBytes := LBytes;
    FInspectionStatus.RetainedIdentities := 0;
    FInspectionStatus.IdentityTrackingOverflow := False;
    FInspectionStatus.MissingSelectedRows := 0;
    FInspectionStatus.FocusChangedOnResume := False;
    FInspectionStatus.ContextResetOnResume := False;
  finally
    FInspectionTransition := False;
  end;
  Notify(nil, True, ncicPaused);
end;

procedure TView.CopyProjectionTo(ATarget: TView);
begin
  ATarget.FQuery := FQuery;
  ATarget.FSource := FSource;
  ATarget.FSnapshot := FSnapshot;
  ATarget.FUnpaged := FUnpaged;
  ATarget.FPaging := FPaging;
  ATarget.FPage := FPage;
  ATarget.FSelection := FSelection;
  ATarget.FExpanded := Copy(FExpanded, 0, Length(FExpanded));
  ATarget.FInspectionPolicy := FInspectionPolicy.Copy;
  ATarget.FInspectionStatus := FInspectionStatus;
  ATarget.FInspectionRemoved := Copy(FInspectionRemoved, 0, Length(FInspectionRemoved));

  if FInspectionIndex <> nil then
  begin
    ATarget.FInspectionIndex := FInspectionIndex.Clone;
  end;
end;

procedure TView.PrepareInspectionUpdate(ATarget: TView;
  const AData: INyxCollectionSnapshot; const AChanges: INyxCollectionChanges);
var
  LParents: TParentIndexes;
  LIndex: Integer;
  LCount: Integer;
  LItem: TNyxItemRef;
begin
  { Schema/hierarchy contributions still validate the incoming source. Query
    evaluation and display publication wait for deliberate Resume. No incoming
    snapshot or operation log is retained after this prepared scalar state. }
  ValidateDataset(AData, LParents);
  ATarget.FInspectionPendingOnly := True;
  ATarget.FInspectionStatus.IncomingRevision := AData.Revision;

  if ATarget.FInspectionStatus.PendingPublications < High(Integer) then
  begin
    Inc(ATarget.FInspectionStatus.PendingPublications);
  end
  else
  begin
    ATarget.FInspectionStatus.PendingCountSaturated := True;
  end;

  if ATarget.FInspectionStatus.IdentityTrackingOverflow then
  begin
    Exit;
  end;
  for LIndex := 0 to AChanges.Count - 1 do
  begin

    if AChanges.Kind(LIndex) <> nceRemove then
    begin
      Continue;
    end;
    LItem := AChanges.ItemRef(LIndex);

    if not FSource.Has(LItem) then
    begin
      Continue;
    end;

    if (ATarget.FInspectionIndex <> nil) and
      (ATarget.FInspectionIndex.IndexOf(LItem.ID) >= 0) then
    begin
      Continue;
    end;
    LCount := Length(ATarget.FInspectionRemoved);

    if LCount >= FInspectionPolicy.IdentityLimit then
    begin
      ATarget.FInspectionStatus.IdentityTrackingOverflow := True;
      Break;
    end;

    if ATarget.FInspectionIndex = nil then
    begin
      ATarget.FInspectionIndex := TNyxTextIndex.Create;
    end;
    SetLength(ATarget.FInspectionRemoved, LCount + 1);
    ATarget.FInspectionRemoved[LCount] := LItem;
    ATarget.FInspectionIndex.AddFirst(LItem.ID, LCount);
    ATarget.FInspectionStatus.RetainedIdentities := LCount + 1;
  end;
end;

procedure TView.ResumeInspection;
var
  LLease: INyxCollectionView;
  LSource: INyxCollectionSnapshot;
  LKeepSource: INyxCollectionSnapshot;
  LSelected: TNyxCollectionSelectionSnapshot;
  LItems: TNyxItemRefs;
  LFocus: TNyxItemRef;
  LAnchor: TNyxItemRef;
  LNextFocus: TNyxItemRef;
  LIndex: Integer;
  LCount: Integer;
  LReset: Boolean;

  function Removed(const AItem: TNyxItemRef): Boolean;
  begin
    Result := AItem.Defined and (LReset or ((FInspectionIndex <> nil) and
      (FInspectionIndex.IndexOf(AItem.ID) >= 0)));
  end;

begin
  LLease := Self as INyxCollectionView;
  Writable;

  if FInspectionStatus.Mode <> ncimPaused then
  begin
    Exit;
  end;
  FInspectionTransition := True;
  try
    LKeepSource := FSource;
    LSource := FStore.Snapshot;
    LSelected := FSelection.Snapshot;
    LReset := FInspectionStatus.IdentityTrackingOverflow;
    FPrepared := TView.CreateValidator(FSpec, FProjection);
    CopyProjectionTo(FPrepared);
    LFocus := LSelected.Focus;
    LAnchor := LSelected.Anchor;
    SetLength(LItems, LSelected.Count);
    LCount := 0;
    for LIndex := 0 to LSelected.Count - 1 do
    begin

      if not Removed(LSelected.ItemAt(LIndex)) then
      begin
        LItems[LCount] := LSelected.ItemAt(LIndex);
        Inc(LCount);
      end;
    end;
    SetLength(LItems, LCount);

    if Removed(LFocus) then
    begin
      LFocus := Default(TNyxItemRef);
    end;

    if Removed(LAnchor) then
    begin
      LAnchor := Default(TNyxItemRef);
    end;
    FPrepared.FSelection := NewNyxCollectionSelection(FSource, LItems, LFocus, LAnchor);
    for LIndex := 0 to High(FPrepared.FExpanded) do
    begin

      if Removed(FSource.ItemAt(LIndex).Ref) then
      begin
        FPrepared.FExpanded[LIndex] := False;
      end;
    end;
    FPrepared.ProjectSnapshot(LSource, nil);
    FPrepared.FInspectionStatus.Mode := ncimFollowing;
    FPrepared.FInspectionStatus.DisplayRevision := LSource.Revision;
    FPrepared.FInspectionStatus.IncomingRevision := LSource.Revision;
    FPrepared.FInspectionStatus.PendingPublications := 0;
    FPrepared.FInspectionStatus.PendingCountSaturated := False;
    FPrepared.FInspectionStatus.RetainedDataBytes := 0;
    FPrepared.FInspectionStatus.RetainedIdentities := 0;
    FPrepared.FInspectionStatus.IdentityTrackingOverflow := False;
    FPrepared.FInspectionStatus.MissingSelectedRows :=
      Max(0, LSelected.Count - FPrepared.FSelection.Count);
    LNextFocus := FPrepared.FSelection.Focus;
    FPrepared.FInspectionStatus.FocusChangedOnResume :=
      LSelected.Focus.Defined <> LNextFocus.Defined;

    if LSelected.Focus.Defined and LNextFocus.Defined then
    begin
      FPrepared.FInspectionStatus.FocusChangedOnResume :=
        (LSelected.Focus.Collection.Name <> LNextFocus.Collection.Name) or
        (LSelected.Focus.ID <> LNextFocus.ID);
    end;
    FPrepared.FInspectionStatus.ContextResetOnResume := LReset;
    FPrepared.FInspectionRemoved := nil;
    FreeAndNil(FPrepared.FInspectionIndex);
    InstallCandidate(LSource, nil);
  finally
    RetireCandidate;
    FInspectionTransition := False;
  end;
  { A nil ordered log correctly requests a full target refresh across the
    coalesced source gap. The old frame remains retained through notification. }
  Notify(nil, False, ncicResumed);
end;

function TView.SubscribeInspection(AObserver: TNyxCollectionInspectionObserver):
  INyxCollectionViewSubscription;
var
  LToken: TViewSubscription;
begin
  Writable;

  if not Assigned(AObserver) or
    (Length(FTokens) >= NyxMaximumCollectionSubscriptions) or
    (FNextSerial = High(Integer)) then
  begin
    raise ENyxCollectionInspection.Create('Inspection subscription is invalid or exceeds its budget');
  end;
  LToken := TViewSubscription.Create;
  Result := LToken;
  Inc(FNextSerial);
  LToken.Serial := FNextSerial;
  LToken.InspectionObserver := AObserver;
  SetLength(FTokens, Length(FTokens) + 1);
  FTokens[Length(FTokens) - 1] := LToken;
  LToken.Owner := Self;
end;
