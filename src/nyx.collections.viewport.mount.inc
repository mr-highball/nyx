{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

{ Mount-owned request admission. Only exact already-rendered immutable data and
  original input flags may move a target. Retained ports/receipts own no mount.
  Terminal writes have no callbacks; arbitrary model reads remain fenced. }

function TNyxCollectionMountBase.CaptureViewport: INyxCollectionViewport;
var
  LPort: TNyxCollectionViewportPort;
begin

  if not FConnected then
  begin
    LPort := TNyxCollectionViewportPort.Create(nil);
    Exit(LPort);
  end;

  if FViewport = nil then
  begin
    LPort := TNyxCollectionViewportPort.Create({$ifdef PAS2JS}@{$endif}RevealViewport);
    FViewport := LPort;
    FViewportPort := LPort;
  end;
  Result := FViewport;
end;

function TNyxCollectionMountBase.ViewportRequestCurrent(
  const ARequest: INyxCollectionReveal; ACheckSelection: Boolean): Boolean;
begin
  Result := (ARequest <> nil) and (FViewportRequest = ARequest) and
    ARequest.Pending and FConnected and FEnabled and GetInputReady and
    (FViewportEpoch > 0) and (FViewportEpoch < High(Integer)) and
    (FInputLifetime.Epoch = FViewportEpoch) and
    (not ACheckSelection or (FSelection = FViewportSelection));
end;

function TNyxCollectionMountBase.ViewportRevealPending: Boolean;
begin
  Result := (FViewportRequest <> nil) and FViewportRequest.Pending;
end;

function TNyxCollectionMountBase.CurrentViewportRequest: INyxCollectionReveal;
begin
  Result := FViewportRequest;
end;

procedure TNyxCollectionMountBase.CancelViewportReveal(AReason: TNyxCollectionRevealReason);
var
  LRequest: INyxCollectionReveal;
  LData: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
begin
  LRequest := FViewportRequest;
  LData := FViewportSnapshot;
  LSelection := FViewportSelection;

  if FViewportState <> nil then
  begin
    FViewportState.Finish(ncvsCancelled, AReason);
  end;
  FViewportState := nil;
  FViewportRequest := nil;
  FViewportSnapshot := nil;
  FViewportSelection := nil;
end;

function TNyxCollectionMountBase.RevealViewport(
  const AOptions: TNyxCollectionRevealOptions): INyxCollectionReveal;
var
  LLease: INyxCollectionMount;
  LPrevious: INyxCollectionReveal;
  LPreviousData: INyxCollectionSnapshot;
  LPreviousSelection: INyxCollectionSelection;
  LState: TNyxCollectionRevealState;
  LData: INyxCollectionSnapshot;
  LView: INyxCollectionView;
begin
  LLease := Self as INyxCollectionMount;
  LPrevious := FViewportRequest;
  LPreviousData := FViewportSnapshot;
  LPreviousSelection := FViewportSelection;

  if FViewportState <> nil then
  begin
    FViewportState.Finish(ncvsCancelled, ncvrSuperseded);
  end;

  if FViewportSerial < High(Integer) then
  begin
    Inc(FViewportSerial);
  end;
  LState := TNyxCollectionRevealState.Create(AOptions, FViewportSerial);
  Result := LState;
  FViewportRequest := Result;
  FViewportState := LState;
  FViewportSnapshot := nil;
  FViewportSelection := nil;
  try

    if FViewportSerial = High(Integer) then
    begin
      LState.Finish(ncvsRefused, ncvrExhausted);
      Exit;
    end;

    if not AOptions.Valid then
    begin
      LState.Finish(ncvsRefused, ncvrInvalidItem);
      Exit;
    end;

    if not FConnected then
    begin
      LState.Finish(ncvsRefused, ncvrDisconnected);
      Exit;
    end;
    PrepareInputTracking;

    if not GetInputReady or not FEnabled then
    begin
      LState.Finish(ncvsRefused, ncvrInputUnavailable);
      Exit;
    end;

    if not SupportsViewportReveal then
    begin
      LState.Finish(ncvsRefused, ncvrUnsupported);
      Exit;
    end;
    FViewportEpoch := FInputLifetime.Epoch;
    FViewportSelection := FSelection;
    LView := FView;
    LData := LView.Snapshot;

    if not ViewportRequestCurrent(Result) or (FView <> LView) then
    begin
      LState.Finish(ncvsCancelled, ncvrChanged);
      Exit;
    end;

    if (LData = nil) or (LData <> FLastSnapshot) then
    begin
      LState.Finish(ncvsRefused, ncvrInputUnavailable);
      Exit;
    end;
    FViewportSnapshot := LData;
    ProcessViewportReveal;
  except
    on LError: Exception do
    begin
      LState.Finish(ncvsFailed, ncvrTargetFailure);
      Failed(LError);
    end;
  end;

  if (FViewportRequest = Result) and Result.Pending then
  begin
    QueueViewportReveal;
  end;
  { Release of the former foreign snapshot is the final callout. An observer
    may issue a new request there; it cannot overwrite an already newer slot. }
end;

function TNyxCollectionMountBase.SupportsViewportReveal: Boolean;
begin
  Result := False;
end;

function TNyxCollectionMountBase.RevealViewportTarget(
  const AOptions: TNyxCollectionRevealOptions; ARow, AColumn: Integer;
  ATakeFocus: Boolean; out AFullyVisible: Boolean;
  out AReason: TNyxCollectionRevealReason): TNyxCollectionRevealStatus;
begin
  AFullyVisible := False;
  AReason := ncvrUnsupported;
  Result := ncvsRefused;
end;

procedure TNyxCollectionMountBase.QueueViewportReveal;
begin
  { Ordinary synchronous alternatives need no private scheduler. A target with
    deferred geometry overrides this using its existing owned UI wake. }
end;

procedure TNyxCollectionMountBase.ProcessViewportReveal;
var
  LLease: INyxCollectionMount;
  LRequest: INyxCollectionReveal;
  LState: TNyxCollectionRevealState;
  LView: INyxCollectionView;
  LData: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
  LSpec: TNyxCollectionViewSpec;
  LOptions: TNyxCollectionRevealOptions;
  LRow: Integer;
  LColumn: Integer;
  LStatus: TNyxCollectionRevealStatus;
  LReason: TNyxCollectionRevealReason;
  LFullyVisible: Boolean;

  function Current(ACheckSelection: Boolean = True): Boolean;
  begin
    Result := ViewportRequestCurrent(LRequest, ACheckSelection) and (FView = LView) and
      (FViewportSnapshot = FLastSnapshot);
  end;

begin

  if FViewportProcessing or FUpdating or (FViewportRequest = nil) then
  begin
    Exit;
  end;
  LLease := Self as INyxCollectionMount;
  LRequest := FViewportRequest;
  LState := FViewportState;
  LView := FView;

  if not LRequest.Pending then
  begin
    CancelViewportReveal(LRequest.Capture.Reason);
    Exit;
  end;
  FViewportProcessing := True;
  try
    try

      if not Current then
      begin
        LState.Finish(ncvsCancelled, ncvrChanged);
        Exit;
      end;
      LData := LView.Snapshot;

      if not Current or (LData <> FViewportSnapshot) or (LData <> FLastSnapshot) then
      begin
        LState.Finish(ncvsCancelled, ncvrChanged);
        Exit;
      end;
      LOptions := LRequest.Capture.Options;

      if LOptions.Item.Collection.Name <> LData.Key.Name then
      begin
        LState.Finish(ncvsRefused, ncvrInvalidItem);
        Exit;
      end;
      LRow := LData.IndexOf(LOptions.Item);
      LSpec := LView.Spec;

      if not Current then
      begin
        LState.Finish(ncvsCancelled, ncvrChanged);
        Exit;
      end;

      if LRow < 0 then
      begin
        LState.Finish(ncvsRefused, ncvrNotDisplayed);
        Exit;
      end;
      LColumn := 0;

      if LOptions.Field.Defined then
      begin
        LColumn := LSpec.ColumnIndex(LOptions.Field);
      end
      else if DisplayColumnCount > 0 then
      begin
        LColumn := DisplayColumn(0);
      end;

      if LColumn < 0 then
      begin
        LState.Finish(ncvsRefused, ncvrUnknownField);
        Exit;
      end;

      if not ColumnVisible(LColumn) then
      begin
        LState.Finish(ncvsRefused, ncvrHiddenField);
        Exit;
      end;

      if not Current then
      begin
        LState.Finish(ncvsCancelled, ncvrChanged);
        Exit;
      end;
      LStatus := RevealViewportTarget(LOptions, LRow, LColumn, False, LFullyVisible, LReason);

      if not Current then
      begin
        LState.Finish(ncvsCancelled, ncvrChanged);
        Exit;
      end;

      if (LStatus = ncvsCompleted) and (LOptions.FocusPolicy = ncvfTake) then
      begin
        { Only this explicit choice moves the canonical cursor. Selection
          membership is untouched. Reentrant observers can supersede/retire the
          request, change its data or choose a different cursor; none is undone. }
        LView.Select(LOptions.Item, nsaFocus);
        LData := LView.Snapshot;
        LSelection := LView.Selection;

        if not Current(False) or (LData <> FViewportSnapshot) or
          not LSelection.Focus.Defined or
          (LSelection.Focus.Collection.Name <> LOptions.Item.Collection.Name) or
          (LSelection.Focus.ID <> LOptions.Item.ID) then
        begin
          LState.Finish(ncvsCancelled, ncvrChanged);
          Exit;
        end;
        FViewportSelection := FSelection;
        LStatus := RevealViewportTarget(LOptions, LRow, LColumn, True, LFullyVisible, LReason);

        if not Current then
        begin
          LState.Finish(ncvsCancelled, ncvrChanged);
          Exit;
        end;
      end;

      if LStatus <> ncvsPending then
      begin
        LState.Finish(LStatus, LReason, LFullyVisible);
      end;
    except
      on LError: Exception do
      begin
        LState.Finish(ncvsFailed, ncvrTargetFailure);
        Failed(LError);
      end;
    end;
  finally
    FViewportProcessing := False;

    if (FViewportRequest = LRequest) and not LRequest.Pending then
    begin
      CancelViewportReveal(LRequest.Capture.Reason);
    end;

    if ViewportRevealPending then
    begin
      QueueViewportReveal;
    end;
  end;
end;
