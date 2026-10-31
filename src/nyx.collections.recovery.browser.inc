{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE.
  Browser affordance for the shared data-only journal review. Input still uses
  the original toolbar command, canonical callbacks and managed queued default. }

procedure TBrowserMount.EnsureCaptionBar;
var
  LCaption: TJSHTMLElement;
begin

  if FColumnBar <> nil then
  begin
    Exit;
  end;
  LCaption := NewElement('caption');
  LCaption.style.setProperty('text-align', 'end');
  LCaption.style.setProperty('padding', '6px');
  FColumnBar := NewElement('span');
  FColumnBar.style.setProperty('display', 'inline-flex');
  FColumnBar.style.setProperty('align-items', 'center');
  FColumnBar.style.setProperty('gap', '8px');
  FColumnBar.style.setProperty('max-width', '100%');
  FColumnBar.style.setProperty('overflow-x', 'auto');
  FColumnBar.style.setProperty('overflow-y', 'hidden');
  FColumnBar.style.setProperty('position', 'sticky');
  FColumnBar.style.setProperty('inset-inline-end', '6px');
  LCaption.appendChild(FColumnBar);
  FHost.insertBefore(LCaption, FHost.firstChild);
end;

procedure TBrowserMount.RefreshCaptionWidth;
var
  LWidth: Integer;
  LTotal: Double;
  LIndex: Integer;
begin

  if (FColumnBar = nil) or not InputLifetimeCurrent then
  begin
    Exit;
  end;
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
  { A single caption row shares the real horizontal clip. Its fixed width never
    exceeds the data-column budget, so long diagnostics cannot stretch caller
    columns. Very narrow allocations scroll the toolbar instead of stacking
    controls over the entire data viewport. Existing DOM/focus stays in place. }
  FColumnBar.style.setProperty('width', IntToStr(Max(1, LWidth - 12)) + 'px');
end;

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
  LFeedback: TNyxCollectionClipboardFeedback;
  LText: TNyxText;
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
    EnsureCaptionBar;
    FClipboardStatus := NewElement('output');
    FClipboardStatus.setAttribute('data-nyx-clipboard-status', '');
    FClipboardStatus.setAttribute('role', 'status');
    FClipboardStatus.setAttribute('aria-label', 'Clipboard result');
    FClipboardStatus.setAttribute('aria-live', 'polite');
    FClipboardStatus.setAttribute('aria-atomic', 'true');
    FClipboardStatus.setAttribute('tabindex', '0');
    FClipboardStatus.style.setProperty('box-sizing', 'border-box');
    FClipboardStatus.style.setProperty('max-width', '100%');
    FClipboardStatus.style.setProperty('min-width', '0');
    FClipboardStatus.style.setProperty('flex', '1 1 0');
    FClipboardStatus.style.setProperty('min-height', '1.5em');
    FClipboardStatus.style.setProperty('height', '2.5em');
    FClipboardStatus.style.setProperty('max-height', '2.5em');
    FClipboardStatus.style.setProperty('overflow-y', 'auto');
    FClipboardStatus.style.setProperty('white-space', 'normal');
    FClipboardStatus.style.setProperty('overflow-wrap', 'anywhere');
    FClipboardStatus.style.setProperty('text-align', 'start');
    FClipboardStatus.style.setProperty('font-size', '0.85em');
    { Status and commands share a row; this exact owning element still carries
      live announcements, focus and its internally scrollable full diagnostic. }
    FColumnBar.insertBefore(FClipboardStatus, FColumnBar.firstChild);
  end;
  FClipboardStatus.style.setProperty('display', 'block');
  FClipboardStatus.style.setProperty('visibility', 'visible');
  FClipboardStatus.removeAttribute('aria-hidden');
  FClipboardStatus.setAttribute('tabindex', '0');
  if not InputLifetimeCurrent or (FHost <> LHost) or
    (GetClipboardFeedback.Serial <> LFeedback.Serial) or
    (GetClipboardFeedback.Phase <> LFeedback.Phase) then
  begin
    Exit;
  end;

  RefreshCaptionWidth;

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

  EnsureCaptionBar;

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
    FRecoveryLauncher.style.setProperty('flex', '0 0 auto');
    FRecoveryLauncher.style.setProperty('max-width', '100%');
    FRecoveryLauncher.style.setProperty('white-space', 'nowrap');
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
  end
  else
  begin
    FRecoveryLauncher.style.setProperty('display', 'none');
  end;
  RefreshCaptionWidth;

  if FEnabled and GetInputReady then
  begin
    FRecoveryLauncher.removeAttribute('disabled');
  end
  else
  begin
    FRecoveryLauncher.setAttribute('disabled', '');
  end;
end;
