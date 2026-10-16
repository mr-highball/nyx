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

{ Shared target navigation uses only each adapter's allocated logical viewport
  and normal guarded Present. Contributions own no platform or document handles. }

function TCanvasView.ObserveNavigation(
  const ASource: INyxCanvasNavigationSource): INyxCancellationScope;
begin
  RequireConnected;
  Result := FNavigationSources.Subscribe(ASource);
end;

function TCanvasView.FitNavigation(
  const AOptions: TNyxCanvasNavigationOptions): TNyxCanvasNavigationResult;
begin
  Result := DoFitNavigation(AOptions, False);
end;

function TCanvasView.DoFitNavigation(const AOptions: TNyxCanvasNavigationOptions;
  ALegacyFallback: Boolean): TNyxCanvasNavigationResult;
var
  LLease: INyxCanvasWorkspaceView;
  LCamera: TNyxCanvasCamera;
  LFrame: TNyxCanvasNavigationFrame;
  LItems: TNyxCanvasNavigationItems;
  LInfo: TNyxCanvasItemInfo;
  LSnapshot: INyxCanvasSnapshot;
  LIndex: Integer;
  LCount: Integer;
  LExpected: Integer;
  LExpectedScene: Integer;
  LWidth: Double;
  LHeight: Double;
begin
  RequireConnected;
  AOptions.Validate;

  if FNavigating then
  begin
    raise ENyxSchedule.Create('Canvas navigation cannot recursively capture its sources');
  end;
  LLease := Self;
  Result := Default(TNyxCanvasNavigationResult);
  Result.Camera := FCamera;
  Result.Status := ncnsLimited;
  LWidth := FSurface.{$ifdef PAS2JS}clientWidth{$else}ClientWidth{$endif};
  LHeight := FSurface.{$ifdef PAS2JS}clientHeight{$else}ClientHeight{$endif};

  if (LWidth <= 0) or (LHeight <= 0) then
  begin
    Exit;
  end;
  LCamera := FCamera.WithViewport(LWidth, LHeight);
  LExpected := FPresentationRevision;
  LExpectedScene := FWorkspace.Scene.Revision;
  LSnapshot := FSnapshot;

  if (LSnapshot = nil) or (LSnapshot.Revision <> LExpectedScene) then
  begin
    { A mounted overlay may precede the first allocated scene presentation.
      Capture current scene meaning when the allocated snapshot is absent or
      older; callbacks below must not replace that meaning before publication. }
    LSnapshot := FWorkspace.Scene.Snapshot;
  end;
  FNavigating := True;
  try
    LFrame := FNavigationSources.Capture(LCamera);

    if not FConnected or (FPresentationRevision <> LExpected) or
      (FWorkspace.Scene.Revision <> LExpectedScene) then
    begin
      raise ENyxSchedule.Create('Canvas changed during navigation contribution capture');
    end;

    if ALegacyFallback and (Length(LFrame.Items) = 0) and (Length(LFrame.Occluders) = 0) then
    begin
      Result.Camera := FitNyxCanvasScene(LCamera, LSnapshot, FScaling);
      Result.Region := NyxCanvasBounds(0, 0, LWidth, LHeight);
      Result.Status := ncnsEmpty;

      if LSnapshot.Count > 0 then
      begin
        { The legacy command retains its existing zoom/oversize contract.
          Direct typed callers use the fully classified mixed-fit calculation. }
        Result.Status := ncnsLimited;
      end;
    end
    else
    begin
      LItems := nil;
      LCount := 0;
      for LIndex := 0 to LSnapshot.Count - 1 do
      begin

        if LSnapshot.Items[LIndex].Placement.IsVisible then
        begin
          Inc(LCount);
        end;
      end;

      if (LCount > MaximumNyxCanvasNavigationItems) or
        (Length(LFrame.Items) > MaximumNyxCanvasNavigationItems - LCount) then
      begin
        raise ENyxSchedule.Create('Scene and overlay navigation exceed their shared extent budget');
      end;
      { Admit the visible combined window before allocating its array. Large
        scenes do not allocate their hidden membership merely to fit overlays. }
      SetLength(LItems, LCount + Length(LFrame.Items));
      LCount := 0;
      for LIndex := 0 to LSnapshot.Count - 1 do
      begin
        LInfo := LSnapshot.Items[LIndex];

        if LInfo.Placement.IsVisible then
        begin
          LItems[LCount] := NyxCanvasNavigationItem(LInfo.Placement.Bounds, FScaling);
          Inc(LCount);
        end;
      end;
      for LIndex := 0 to High(LFrame.Items) do
      begin
        LItems[LCount] := NyxCanvasNavigationItem(LFrame.Items[LIndex].Bounds,
          LFrame.Items[LIndex].Scaling, LFrame.Items[LIndex].Independent);
        Inc(LCount);
      end;
      SetLength(LItems, LCount);
      Result := FitNyxCanvasNavigation(LCamera, LItems, LFrame.Occluders, AOptions);
    end;

    if not FConnected or (FPresentationRevision <> LExpected) or
      (FWorkspace.Scene.Revision <> LExpectedScene) then
    begin
      raise ENyxSchedule.Create('Canvas changed before navigation presentation');
    end;
    SetCamera(Result.Camera);
  finally
    FNavigating := False;
  end;
end;
