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
{ Included by both canvas adapters. Precision values and occlusion use the same
  portable contract; physical painting/capture remains with each target. }

function TCanvasView.GetPrecision: TNyxCanvasPrecisionOptions;
begin
  RequireConnected;
  Result := FGuideInput.Accepted;
end;

function TCanvasView.GetSelectedGuide: TNyxCanvasGuideRef;
begin
  RequireConnected;
  Result := FGuideInput.Selection;

  if not FGuideInput.Accepted.Contains(Result) then
  begin
    Result := Default(TNyxCanvasGuideRef);
  end;
end;

procedure TCanvasView.RequirePrecisionRevision;
begin
  RequireConnected;

  if FUpdating or (FPresentationRevision = High(Integer)) then
  begin
    raise ENyxCanvasPrecision.Create('Precision cannot publish during another update or exhausted revision');
  end;
end;

procedure TCanvasView.NotifyPrecision;
begin
  Inc(FPresentationRevision);
  PaintPrecision;
  {$ifndef PAS2JS}
  FSurface.Invalidate;
  {$endif}
  FPresentationObservers.Notify(FPresentationRevision);

  if FConnected and (FResources <> nil) then
  begin
    FResources.Wake;
  end;
end;

procedure TCanvasView.SetPrecision(const AOptions: TNyxCanvasPrecisionOptions);
var
  LOptions: TNyxCanvasPrecisionOptions;
  LTicks: TNyxCanvasRulerTicks;
  LLease: INyxCanvasWorkspaceView;
begin
  RequirePrecisionRevision;
  LOptions := TNyxCanvasPrecisionOptions.FromData(AOptions.ToData);
  LTicks := NyxCanvasRulerTicks(LOptions.Rulers, FCamera);

  if Length(LTicks) > MaximumNyxCanvasRulerTicks then
  begin
    raise ENyxCanvasPrecision.Create('Precision tick budget exceeded');
  end;
  LLease := Self;

  if LOptions.ToData.ToJSON = FGuideInput.Accepted.ToData.ToJSON then
  begin
    CancelGuideGesture;
    PaintPrecision;
    Exit;
  end;
  CancelGuideGesture;
  FGuideInput.Reset(LOptions);
  NotifyPrecision;
end;

procedure TCanvasView.SelectGuide(const AReference: TNyxCanvasGuideRef);
var
  LLease: INyxCanvasWorkspaceView;
begin
  RequirePrecisionRevision;

  if AReference.Name = FGuideInput.Selection.Name then
  begin
    Exit;
  end;
  LLease := Self;
  FGuideInput.Select(AReference);
  NotifyPrecision;
end;

function TCanvasView.SnapGuidePoint(const APoint: TNyxCanvasPoint;
  const AExclude: TNyxCanvasGuideRef): TNyxCanvasGuideSnap;
begin
  RequireConnected;
  Result := SnapNyxCanvasGuides(FGuideInput.Accepted, APoint, FCamera, AExclude);
end;

procedure TCanvasView.AppendPrecisionOccluders(const ACamera: TNyxCanvasCamera;
  var AFrame: TNyxCanvasNavigationFrame);
var
  LRulers: TNyxCanvasRulers;
  LCount: Integer;
begin

  if FGuideInput = nil then
  begin
    Exit;
  end;
  LRulers := FGuideInput.Accepted.Rulers;

  if not LRulers.Defined then
  begin
    Exit;
  end;
  AFrame.Occluders := Copy(AFrame.Occluders, 0, Length(AFrame.Occluders));
  LCount := Length(AFrame.Occluders);
  SetLength(AFrame.Occluders, LCount + Ord(ncaxX in LRulers.VisibleAxes) +
    Ord(ncaxY in LRulers.VisibleAxes));

  if ncaxX in LRulers.VisibleAxes then
  begin
    AFrame.Occluders[LCount] := NyxCanvasBounds(0, 0, ACamera.ViewportWidth,
      Min(LRulers.StripSize, ACamera.ViewportHeight));
    Inc(LCount);
  end;

  if ncaxY in LRulers.VisibleAxes then
  begin
    AFrame.Occluders[LCount] := NyxCanvasBounds(0, 0,
      Min(LRulers.StripSize, ACamera.ViewportWidth), ACamera.ViewportHeight);
  end;
end;
