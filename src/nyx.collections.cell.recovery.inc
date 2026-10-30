{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE.
  Shared managed recovery presentation; included by nyx.collections.cell.actions.
  The journal is data-only. Original input flags and weak serials own execution. }

procedure TRecoveryOpenWork.Execute(const AExecution: INyxExecution);
var
  LOwner: TCellActions;
  LLease: INyxCollectionCellActions;
begin
  LOwner := Port.Owner;

  if (LOwner = nil) or not Port.Active or (Port.Serial <> Serial) then
  begin
    Exit;
  end;
  LLease := LOwner;
  Ready.CheckReady;

  if Port.Active and (Port.Serial = Serial) then
  begin
    LOwner.ShowRecovery(Serial, Ready, AllowCopy, Index, Revision);
  end;
end;

procedure TCellActions.AttachRecovery(const AJournal: INyxCollectionDraftRecovery);
begin

  if (GetIntent = 0) or (FRecovery <> nil) or (AJournal = nil) then
  begin
    raise ENyxCollection.Create('Recovery review requires one journal on a live actions owner');
  end;
  FRecovery := AJournal;
end;

function TCellActions.GetRecoveryCount: Integer;
var
  LJournal: INyxCollectionDraftRecovery;
  LObservation: TNyxDraftRecoveryObservation;
  LIntent: Integer;
begin
  Result := 0;
  LIntent := GetIntent;
  LJournal := FRecovery;

  if (LIntent = 0) or (LJournal = nil) then
  begin
    Exit;
  end;
  LObservation := LJournal.Capture;

  if (GetIntent = LIntent) and (FRecovery = LJournal) then
  begin

    if LObservation.Count < 0 then
    begin
      raise ENyxCollection.Create('Recovery count must be nonnegative');
    end;
    Result := LObservation.Count;
  end;
end;

function TCellActions.OpenRecovery(const AReadiness: INyxCancellationReadiness;
  AAllowCopy: Boolean): Boolean;
begin
  Result := QueueRecovery(AReadiness, AAllowCopy, 0, 0);
end;

function TCellActions.QueueRecovery(const AReadiness: INyxCancellationReadiness;
  AAllowCopy: Boolean; AIndex, ARevision: Integer): Boolean;
var
  LLease: INyxCollectionCellActions;
  LScheduler: INyxScheduler;
  LWork: TRecoveryOpenWork;
  LWorkLease: INyxWork;
  LExecution: INyxExecution;
  LIntent: Integer;
begin
  Result := False;
  LLease := Self;
  LIntent := GetIntent;
  LScheduler := FScheduler;

  if (LIntent = 0) or (FRecovery = nil) or (AReadiness = nil) or
    (LScheduler = nil) or (AIndex < 0) or (ARevision < 0) then
  begin
    Exit;
  end;
  LScheduler.RequireUI;
  AReadiness.CheckReady;

  if GetIntent <> LIntent then
  begin
    Exit;
  end;
  LWork := TRecoveryOpenWork.Create;
  LWorkLease := LWork;
  LWork.Port := FPort;
  LWork.PortLease := FPortLease;
  LWork.Ready := AReadiness;
  LWork.AllowCopy := AAllowCopy;
  LWork.Index := AIndex;
  LWork.Revision := ARevision;
  LWork.Serial := Advance;

  if FScope <> nil then
  begin
    FScope.Cancel;
  end;
  LExecution := LScheduler.PostUI(LWorkLease);

  if GetIntent = LWork.Serial then
  begin
    FLastExecution := LExecution;
  end;
  Result := True;
end;

procedure TCellActions.ShowRecovery(ASerial: Integer;
  const AReadiness: INyxCancellationReadiness; AAllowCopy: Boolean;
  AIndex, ARevision: Integer);
var
  LLease: INyxCollectionCellActions;
  LJournal: INyxCollectionDraftRecovery;
  LObservation: TNyxDraftRecoveryObservation;
  LAfter: TNyxDraftRecoveryObservation;
  LEntry: INyxRecoveredCollectionDraft;
  LReview: INyxRecoveredDraftReview;
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LStatus: INyxLabel;
  LInspector: INyxPopover;
  LFactory: INyxCollectionCellPresenterFactory;
  LEvents: INyxEvents;
  LAdmission: INyxPopoverOpenAdmission;
  LOpen: INyxPopoverOpenLifetime;
  LGuard: INyxCollectionCellPresentation;
  LEnter: TNyxCollectionCellPresentation;
  LChanged: Boolean;
  LCaption: TNyxText;
begin
  LLease := Self;

  if (GetIntent <> ASerial) or (FRecovery = nil) then
  begin
    Exit;
  end;
  AReadiness.CheckReady;
  Cleanup;

  if GetIntent <> ASerial then
  begin
    Exit;
  end;
  AReadiness.CheckReady;
  FScope := TCellScope.Create(AReadiness);
  FScopeLease := FScope;
  FSessionReady := AReadiness;
  FRecoveryCopyAllowed := AAllowCopy;
  FCellCopyAllowed := AAllowCopy and (FRepresentationWriter <> nil);
  LJournal := FRecovery;
  LFactory := FFactory;
  LObservation := LJournal.Capture;

  if not Current(ASerial) or (FRecovery <> LJournal) then
  begin
    Exit;
  end;
  LChanged := (ARevision <> 0) and (ARevision <> LObservation.Revision);

  if not LChanged and (AIndex < LObservation.Count) then
  begin
    LEntry := LJournal.EntryAt(AIndex);

    if not Current(ASerial) or (FRecovery <> LJournal) then
    begin
      Exit;
    end;
    LAfter := LJournal.Capture;

    if not Current(ASerial) or (FRecovery <> LJournal) then
    begin
      Exit;
    end;
    LChanged := LAfter.Revision <> LObservation.Revision;

    if LChanged then
    begin
      LEntry := nil;
    end;
  end;
  FRecoveryIndex := AIndex;
  FRecoveryRevision := LObservation.Revision;
  LDocument := TNyxDocument.Create;
  try
    LPage := NewNyxPage('nyx-recovered-edits-page');
    LPage.Configure.Padding(12).Gap(8).Done;

    if LEntry <> nil then
    begin
      LReview := NewNyxRecoveredDraftReview('nyx-recovered-edit', LEntry);

      if not Current(ASerial) then
      begin
        Exit;
      end;

      { Review retains its frozen, detached cell. Preparing a represented Copy
        is a separate bounded operation; its budget cannot hide the review. }
      FRecoveryReview := LReview;
      LCaption := 'Recovered edit ' + TNyxText(IntToStr(AIndex + 1)) + ' of ' +
        TNyxText(IntToStr(LObservation.Count));
      LPage.Add(NewNyxHeading('nyx-recovered-edits-count').WithText(LCaption));
      LPage.Add(LReview);
    end
    else
    begin
      LCaption := 'No recovered edits';

      if LChanged then
      begin
        LCaption := 'The recovery list changed. Reload to review the latest entries.';
      end;
      LPage.Add(NewNyxHeading('nyx-recovered-edits-count').WithText(LCaption));
    end;
    LPage.Add(NewNyxButton(NyxRecoveredEditsPreviousID).Configure
      .Text('Previous edit').Enabled((LEntry <> nil) and (AIndex > 0)).Done);
    LPage.Add(NewNyxButton(NyxRecoveredEditsNextID).Configure
      .Text('Next edit').Enabled((LEntry <> nil) and (AIndex < LObservation.Count - 1)).Done);
    LPage.Add(NewNyxButton(NyxRecoveredEditsReloadID).WithText('Reload recovered edits'));

    if FCellCopyAllowed and (LEntry <> nil) then
    begin
      LPage.Add(NewNyxButton(NyxRecoveredEditsCopyID).WithText('Copy recovered text'));
    end
    else if LEntry <> nil then
    begin
      LPage.Add(NewNyxLabel('nyx-recovered-copy-disabled').WithText(
        'Copy recovered text is not enabled for this table.'));
    end;
    LPage.Add(NewNyxButton(NyxCellInspectorCloseID).Configure
      .PartName(NyxPart(NyxCellInspectorCloseID)).Text('Close').Done);
    LDocument.State.SetValue(NyxTextState(NyxCellInspectorStatusState), '');
    LStatus := NewNyxLabel('nyx-cell-inspector-status');
    LStatus.Binds.Text(NyxTextState(NyxCellInspectorStatusState)).Done;
    LPage.Add(LStatus);
    LDocument.AddPage(LPage);

    if not Current(ASerial) then
    begin
      Exit;
    end;
    LInspector := LFactory.CreateInspector(LDocument, NyxPageRoot(LPage.ID));
  finally
    LDocument.Free;
  end;

  if not Current(ASerial) then
  begin
    Exit;
  end;
  FInspector := LInspector;
  LEvents := LInspector.Events;

  if not Current(ASerial) or (FInspector <> LInspector) then
  begin
    Exit;
  end;
  ObserveButton(LEvents, NyxRecoveredEditsPreviousID, 5, ASerial);
  ObserveButton(LEvents, NyxRecoveredEditsNextID, 6, ASerial);
  ObserveButton(LEvents, NyxRecoveredEditsReloadID, 7, ASerial);

  if FCellCopyAllowed and (LEntry <> nil) then
  begin
    ObserveButton(LEvents, NyxRecoveredEditsCopyID, 8, ASerial);
  end;
  ObserveButton(LEvents, NyxCellInspectorCloseID, 2, ASerial);

  if not Current(ASerial) then
  begin
    Exit;
  end;
  LEnter := FPresentation;

  if Assigned(LEnter) then
  begin
    LGuard := LEnter();
  end;
  try

    if Current(ASerial) then
    begin
      LInspector.Open(NyxPopover('Recovered edits').Size(560, 560)
        .Focus(NyxPart(NyxCellInspectorCloseID)));
    end;
  finally

    if LGuard <> nil then
    begin
      LGuard.Leave;
    end;
  end;

  if not Current(ASerial) or not Supports(LInspector, INyxPopoverOpenAdmission, LAdmission) then
  begin
    LInspector.Close;
    Exit;
  end;
  LOpen := LAdmission.GetOpenLifetime;

  if Current(ASerial) and (FInspector = LInspector) and
    (LOpen <> nil) and LOpen.Current then
  begin
    FOpen := LOpen;
  end
  else
  begin
    LInspector.Close;
  end;
end;

procedure TCellActions.RecoveryDefault(AKind, ASerial: Integer);
var
  LLease: INyxCollectionCellActions;
  LJournal: INyxCollectionDraftRecovery;
  LObservation: TNyxDraftRecoveryObservation;
  LIndex: Integer;
  LValue: INyxCollectionCellText;
  LReview: INyxRecoveredDraftReview;
begin
  LLease := Self;

  if not Current(ASerial) or (FRecovery = nil) then
  begin
    Exit;
  end;

  if AKind = 8 then
  begin
    LReview := FRecoveryReview;

    if LReview <> nil then
    begin
      try
        LValue := NewNyxCollectionRecoveredDraftText(LReview.Cell, LReview.BeforeRevision);

        if Current(ASerial) and (FRecoveryReview = LReview) then
        begin
          FRecoveryValue := LValue;
          CopyValue(ASerial, LValue);
        end;
      except
        on E: ENyxCollectionRepresentation do
        begin
          Status('Recovered text cannot be copied within the represented-text byte limit.', ASerial);
          Exit;
        end;
      end;

    end;
    Exit;
  end;

  if AKind = 7 then
  begin
    OpenRecovery(FSessionReady, FRecoveryCopyAllowed);
    Exit;
  end;
  LJournal := FRecovery;
  LObservation := LJournal.Capture;

  if not Current(ASerial) or (FRecovery <> LJournal) then
  begin
    Exit;
  end;

  if LObservation.Revision <> FRecoveryRevision then
  begin
    Status('The recovery list changed. Reload before choosing another edit.', ASerial);
    Exit;
  end;
  LIndex := FRecoveryIndex - 1;

  if AKind = 6 then
  begin
    LIndex := FRecoveryIndex + 1;
  end;

  if (LIndex >= 0) and (LIndex < LObservation.Count) then
  begin
    QueueRecovery(FSessionReady, FRecoveryCopyAllowed, LIndex, LObservation.Revision);
  end;
end;
