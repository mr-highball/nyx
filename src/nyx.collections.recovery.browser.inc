{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE.
  Browser affordance for the shared data-only journal review. Input still uses
  the original toolbar command, canonical callbacks and managed queued default. }

procedure TBrowserMount.RefreshTargetIdle;
begin
  SynchronizeRecovery;
  SynchronizeClipboardFeedback;
end;

procedure TBrowserMount.ClipboardFeedbackChanged;
begin
  SynchronizeClipboardFeedback;
end;

procedure TBrowserMount.SynchronizeClipboardFeedback;
var
  LLease: INyxCollectionMount;
  LHost: TJSHTMLElement;
  LCaption: TJSHTMLElement;
  LFeedback: TNyxCollectionClipboardFeedback;
  LText: TNyxText;
  LWidth: Integer;
  LTotal: Double;
  LIndex: Integer;
begin
  LLease := Self as INyxCollectionMount;
  LHost := FHost;

  if not InputLifetimeCurrent or (LHost = nil) or (FView = nil) or
    (FView.Projection <> cpTable) then
  begin
    Exit;
  end;

  if not HasClipboardFeedback or not GetClipboardFeedbackEnabled then
  begin

    if FClipboardStatus <> nil then
    begin
      FClipboardStatus.style.setProperty('visibility', 'hidden');
      FClipboardStatus.setAttribute('aria-hidden', 'true');
      FClipboardStatus.removeAttribute('tabindex');
    end;
    Exit;
  end;
  LFeedback := GetClipboardFeedback;
  LText := NyxCollectionCellInspectionText(NyxCollectionClipboardFeedbackText(LFeedback));

  if FClipboardStatus = nil then
  begin
    LCaption := TJSHTMLElement(LHost.querySelector('caption'));

    if LCaption = nil then
    begin
      LCaption := NewElement('caption');
      LCaption.style.setProperty('padding', '6px');
      LHost.insertBefore(LCaption, LHost.firstChild);
    end;
    FClipboardStatus := NewElement('output');
    FClipboardStatus.setAttribute('data-nyx-clipboard-status', '');
    FClipboardStatus.setAttribute('role', 'status');
    FClipboardStatus.setAttribute('aria-label', 'Clipboard result');
    FClipboardStatus.setAttribute('aria-live', 'polite');
    FClipboardStatus.setAttribute('aria-atomic', 'true');
    FClipboardStatus.setAttribute('tabindex', '0');
    FClipboardStatus.style.setProperty('position', 'sticky');
    FClipboardStatus.style.setProperty('inset-inline-start', '6px');
    FClipboardStatus.style.setProperty('box-sizing', 'border-box');
    FClipboardStatus.style.setProperty('max-width', '100%');
    FClipboardStatus.style.setProperty('min-height', '1.5em');
    FClipboardStatus.style.setProperty('height', '2.5em');
    FClipboardStatus.style.setProperty('max-height', '2.5em');
    FClipboardStatus.style.setProperty('overflow-y', 'auto');
    FClipboardStatus.style.setProperty('white-space', 'normal');
    FClipboardStatus.style.setProperty('overflow-wrap', 'anywhere');
    FClipboardStatus.style.setProperty('text-align', 'start');
    FClipboardStatus.style.setProperty('font-size', '0.85em');
    LCaption.appendChild(FClipboardStatus);
  end;
  FClipboardStatus.style.setProperty('display', 'block');
  FClipboardStatus.style.setProperty('visibility', 'visible');
  FClipboardStatus.removeAttribute('aria-hidden');
  FClipboardStatus.setAttribute('tabindex', '0');
  { Clamp to both the real clip and the intrinsic data-column budget, as for
    recovery access. Long diagnostics scroll inside this bounded output rather
    than stretching fixed columns or rebuilding the data face. }
  LWidth := ColumnClipWidth;
  LTotal := 0;
  for LIndex := 0 to High(FColumnWidths) do
  begin
    LTotal := LTotal + FColumnWidths[LIndex];
  end;

  if LTotal > 0 then
  begin
    LWidth := Min(LWidth, Floor(LTotal));
  end;

  if not InputLifetimeCurrent or (FHost <> LHost) or
    (GetClipboardFeedback.Serial <> LFeedback.Serial) or
    (GetClipboardFeedback.Phase <> LFeedback.Phase) then
  begin
    Exit;
  end;

  if LWidth > 12 then
  begin
    FClipboardStatus.style.setProperty('width', IntToStr(LWidth - 12) + 'px');
  end;

  if FClipboardStatus.textContent <> LText then
  begin
    FClipboardStatus.textContent := LText;
  end;
  FClipboardStatus.setAttribute('data-copy-phase', IntToStr(Ord(LFeedback.Phase)));
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
