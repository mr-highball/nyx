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

constructor TBrowserInputProof.Create(AOwner: TNyxBrowserRenderer;
  const AControls: array of TNyxControlRef);
var
  LIndex: Integer;
begin
  inherited Create;
  FLease := AOwner.FCoordinatorRefreshTarget;
  FReceiver := TNyxBrowserCoordinatorRefresh(AOwner.FCoordinatorRefreshObject);
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

destructor TBrowserInputProof.Destroy;
begin
  FDrafts.Free;
  inherited Destroy;
end;

procedure TBrowserInputProof.CheckReady;
var
  LOwner: TNyxBrowserRenderer;
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

function TNyxBrowserRenderer.GetInputs: INyxInputCommands;
begin
  Result := nil;

  if (FLiveBindings <> nil) and not FDesignMode then
  begin
    Result := FLiveBindings.Inputs;
  end;
end;

function TNyxBrowserRenderer.CaptureInputTargets(
  const AControls: array of TNyxControlRef): INyxPublicationReadiness;
begin
  FEvents.Scheduler.RequireUI;
  Result := TBrowserInputProof.Create(Self, AControls);
end;

function TNyxBrowserRenderer.InputStamp(const AID: TNyxText): TNyxText;
var
  LIndex: Integer;
  LBinding: TNyxBrowserBinding;
  LInput: TJSHTMLElement;
begin

  if FUpdating or FDesignMode or (FLiveBindings = nil) then
  begin
    raise ENyxState.Create('Physical input requires an idle runtime view');
  end;
  for LIndex := 0 to High(FBindings) do
  begin
    LBinding := FBindings[LIndex];

    if LBinding.FNode.ID <> AID then
    begin
      Continue;
    end;

    if LBinding.FDisconnected or LBinding.FComposing or
      (Length(LBinding.FCapturedPointers) > 0) then
    begin
      raise ENyxState.Create('Physical input is composing, captured or disconnected');
    end;
    LInput := LBinding.FInput;

    if LInput is TJSHTMLTextAreaElement then
    begin
      Exit(TJSHTMLTextAreaElement(LInput).value);
    end;

    if LInput is TJSHTMLSelectElement then
    begin
      Exit(TJSHTMLSelectElement(LInput).value + #0 +
        IntToStr(TJSHTMLSelectElement(LInput).selectedIndex));
    end;

    if LInput is TJSHTMLInputElement then
    begin
      Exit(TJSHTMLInputElement(LInput).value + #0 +
        BoolToStr(TJSHTMLInputElement(LInput).checked));
    end;
    raise ENyxState.Create('The component has no supported physical scalar input');
  end;
  raise ENyxState.Create('Physical input role is no longer mounted');
end;

procedure TNyxBrowserRenderer.SyncInputTargets(
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
