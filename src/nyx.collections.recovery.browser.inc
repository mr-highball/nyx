{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE.
  Browser affordance for the shared data-only journal review. Input still uses
  the original toolbar command, canonical callbacks and managed queued default. }

procedure TBrowserMount.RefreshTargetIdle;
begin
  SynchronizeRecovery;
end;

procedure TBrowserMount.RecoveryMembershipChanged(
  const AObservation: TNyxDraftRecoveryObservation);
var
  LLease: INyxCollectionMount;
begin
  LLease := Self as INyxCollectionMount;

  if InputLifetimeCurrent and not FUpdating then
  begin
    { A capture inside RenderDataset is published by RefreshTargetIdle instead.
      External discard needs only its caption; live row/editor DOM stays put. }
    SynchronizeRecovery;
  end;
end;

procedure TBrowserMount.SynchronizeRecovery;
var
  LCount: Integer;
  LIndex: Integer;
  LCaption: TJSHTMLElement;
  LWidth: Double;
  LAvailable: Integer;
begin

  if not InputLifetimeCurrent or (FHost = nil) or (FView = nil) or
    (FView.Projection <> cpTable) or (FCellActions = nil) then
  begin
    Exit;
  end;
  LCount := GetRecoveredEditCount;

  if not InputLifetimeCurrent then
  begin
    Exit;
  end;

  if (LCount = 0) and (FRecoveryLauncher = nil) then
  begin
    Exit;
  end;

  if FColumnBar = nil then
  begin
    LCaption := NewElement('caption');
    LCaption.style.setProperty('text-align', 'end');
    LCaption.style.setProperty('padding', '6px');
    FColumnBar := NewElement('span');
    FColumnBar.style.setProperty('display', 'inline-flex');
    FColumnBar.style.setProperty('align-items', 'center');
    FColumnBar.style.setProperty('justify-content', 'flex-end');
    FColumnBar.style.setProperty('gap', '8px');
    FColumnBar.style.setProperty('max-width', '100%');
    FColumnBar.style.setProperty('position', 'sticky');
    FColumnBar.style.setProperty('inset-inline-end', '6px');
    LCaption.appendChild(FColumnBar);
    FHost.insertBefore(LCaption, FHost.firstChild);
  end;

  if FRecoveryLauncher = nil then
  begin
    FRecoveryLauncher := NewElement('button');
    FRecoveryLauncher.setAttribute('type', 'button');
    FRecoveryLauncher.setAttribute('aria-haspopup', 'dialog');
    FRecoveryLauncher.setAttribute('aria-label', 'Review recovered edits');
    FRecoveryLauncher.setAttribute('data-nyx-recovery-button', '');
    FRecoveryLauncher.className := 'nyx-button';
    FRecoveryLauncher.style.setProperty('font', 'inherit');
    FRecoveryLauncher.style.setProperty('padding', '6px 12px');
    FRecoveryLauncher.style.setProperty('border-radius', '6px');
    FRecoveryLauncher.style.setProperty('cursor', 'pointer');
    FRecoveryLauncher.style.setProperty('min-width', '0');
    FRecoveryLauncher.style.setProperty('max-width', '100%');
    FRecoveryLauncher.style.setProperty('white-space', 'normal');
    FRecoveryLauncher.style.setProperty('overflow-wrap', 'anywhere');
    FRecoveryLauncher.addEventListener('pointerdown', @ColumnLauncherPointer);
    FColumnBar.appendChild(FRecoveryLauncher);
  end;

  if not FHasColumnLayout and not FRecoveryInputBound then
  begin
    { Renderer-managed and direct mounts share the same consumed/default route.
      Add only the missing toolbar listeners; ordinary cell input is unchanged. }
    FRecoveryInputBound := True;
    for LIndex := Low(PendingInputEvents) to High(PendingInputEvents) do
    begin
      FHost.addEventListener(PendingInputEvents[LIndex], @BlockPendingInput, True);
    end;
    FHost.addEventListener(ColumnFitClickEvent, @ColumnFitInput);
    FHost.addEventListener(ColumnFitDoubleEvent, @ColumnFitInput);
    FHost.addEventListener('keyup', @ColumnButtonRelease);
    FHost.addEventListener('focusout', @ColumnButtonBlur, True);
    window.addEventListener('blur', @ColumnButtonBlur);
  end;
  FRecoveryLauncher.textContent := 'Recovered edits (' + IntToStr(LCount) + ')';

  if LCount > 0 then
  begin
    FRecoveryLauncher.style.removeProperty('display');
    FColumnBar.style.setProperty('flex-wrap', 'wrap');
    LAvailable := ColumnClipWidth;
    LWidth := 0;
    for LIndex := 0 to High(FColumnWidths) do
    begin
      LWidth := LWidth + FColumnWidths[LIndex];
    end;

    if LWidth > 0 then
    begin
      LAvailable := Min(LAvailable, Floor(LWidth));
    end;
    { Stay inside both the actual clip and the table's intrinsic column widths.
      A caption must not stretch a caller's deliberately fixed-width cells. }
    FColumnBar.style.setProperty('width', IntToStr(Max(1, LAvailable - 12)) + 'px');

    if FColumnStatus <> nil then
    begin
      FColumnStatus.style.setProperty('flex', '1 0 100%');
    end;
  end
  else
  begin
    FRecoveryLauncher.style.setProperty('display', 'none');
    FColumnBar.style.removeProperty('flex-wrap');

    if FColumnStatus <> nil then
    begin
      FColumnStatus.style.setProperty('flex', '1 1 0');
    end;

    if FHasColumnLayout and FColumnPresentation.PinsSuspended then
    begin
      FColumnBar.style.setProperty('width', IntToStr(Max(1, ColumnClipWidth - 12)) + 'px');
    end
    else
    begin
      FColumnBar.style.removeProperty('width');
    end;
  end;

  if FEnabled and GetInputReady then
  begin
    FRecoveryLauncher.removeAttribute('disabled');
  end
  else
  begin
    FRecoveryLauncher.setAttribute('disabled', '');
  end;
end;
