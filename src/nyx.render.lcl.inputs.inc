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

constructor TLCLInputProof.Create(AOwner: TNyxLCLRenderer;
  const AControls: array of TNyxControlRef);
var
  LIndex: Integer;
begin
  inherited Create;
  FLease := AOwner.FCoordinatorRefreshTarget;
  FReceiver := TNyxLCLCoordinatorRefresh(AOwner.FCoordinatorRefreshObject);
  FCommands := AOwner.GetInputs;
  FDrafts := TNyxStrings.Create;
  SetLength(FControls, Length(AControls));
  for LIndex := 0 to High(AControls) do
  begin
    FControls[LIndex].ID := AControls[LIndex].ID;
    FDrafts.Add(AOwner.InputStamp(AControls[LIndex].ID));
  end;
  CheckReady;
end;

destructor TLCLInputProof.Destroy;
begin
  FDrafts.Free;
  inherited Destroy;
end;

procedure TLCLInputProof.CheckReady;
var
  LOwner: TNyxLCLRenderer;
  LIndex: Integer;
begin
  LOwner := FReceiver.Owner;

  if (LOwner = nil) or (FCommands = nil) or not FCommands.Connected or
    (LOwner.GetInputs <> FCommands) then
  begin
    raise ENyxState.Create('Physical input capture belongs to a retired view');
  end;
  for LIndex := 0 to High(FControls) do
  begin

    if LOwner.InputStamp(FControls[LIndex].ID) <> FDrafts[LIndex] then
    begin
      raise ENyxState.Create('Physical input draft changed after capture');
    end;
  end;
end;

function TNyxLCLRenderer.GetInputs: INyxInputCommands;
begin
  Result := nil;

  if (FLiveBindings <> nil) and not FDesignMode then
  begin
    Result := FLiveBindings.Inputs;
  end;
end;

function TNyxLCLRenderer.CaptureInputTargets(
  const AControls: array of TNyxControlRef): INyxPublicationReadiness;
begin
  FEvents.Scheduler.RequireUI;
  Result := TLCLInputProof.Create(Self, AControls);
end;

function TNyxLCLRenderer.InputStamp(const AID: TNyxText): TNyxText;
var
  LBinding: TNyxLCLBinding;
  LInput: TControl;
begin

  if FUpdating or FDesignMode or (FLiveBindings = nil) then
  begin
    raise ENyxState.Create('Physical input requires an idle runtime view');
  end;
  LBinding := IdentityBinding(AID, niRuntime);
  LInput := LBinding.FInput;

  if LBinding.FComposing or NyxLCLHasPointerCapture(LInput) then
  begin
    raise ENyxState.Create('Physical input is composing or captured');
  end;

  if (LInput is TWinControl) and not TWinControl(LInput).HandleAllocated then
  begin
    { Reading a virtual native Text must not create a window and invoke user
      creation callbacks inside final publication readiness. }
    raise ENyxState.Create('Physical input has not acquired its native handle');
  end;

  if LInput is TCustomEdit then
  begin
    Exit(TNyxText(TCustomEdit(LInput).Text));
  end;

  if LInput is TComboBox then
  begin
    Exit(TNyxText(TComboBox(LInput).Text) + #0 + IntToStr(TComboBox(LInput).ItemIndex));
  end;

  if LInput is TCheckBox then
  begin
    Exit(BoolToStr(TCheckBox(LInput).Checked));
  end;

  if LInput is TRadioButton then
  begin
    Exit(BoolToStr(TRadioButton(LInput).Checked));
  end;

  if LInput is TTrackBar then
  begin
    Exit(IntToStr(TTrackBar(LInput).Position));
  end;
  raise ENyxState.Create('The component has no supported physical scalar input');
end;

procedure TNyxLCLRenderer.SyncInputTargets(
  const AControls: array of TNyxControlRef);
var
  LIndex: Integer;
  LRole: Integer;
begin
  { The complete model/query group is committed. Invalidate only the requested
    physical baselines, including a same-value reset of an unfinished draft.
    Normal Sync preserves all other drafts, composition, selection and lifetime
    rules. Sync is LAST: a custom updater may retire or destroy this renderer. }
  for LIndex := 0 to High(FBindings) do
  begin
    for LRole := 0 to High(AControls) do
    begin

      if FBindings[LIndex].FNode.ID = AControls[LRole].ID then
      begin
        FBindings[LIndex].FHasValueBaseline := False;
        Break;
      end;
    end;
  end;
  Sync;
end;
