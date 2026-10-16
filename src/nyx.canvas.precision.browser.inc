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
{ Included by nyx.canvas.browser. SVG nodes are built from admitted numeric
  geometry and textContent only. No markup, script or caller CSS is interpreted.
  Ruler pointer move/up/cancel/capture-loss bubble to the owning surface once. }

const
  CPrecisionSVG = 'http://www.w3.org/2000/svg';

function PrecisionSVG(const AName: String): TJSElement;
begin
  Result := document.createElementNS(CPrecisionSVG, AName);
end;

procedure PrecisionLine(AParent: TJSElement; AX1, AY1, AX2, AY2: Double;
  const AColor: TNyxText; AWidth: Double = 1);
var
  LLine: TJSElement;
begin
  LLine := PrecisionSVG('line');
  LLine.setAttribute('x1', NyxData(AX1).ToJSON);
  LLine.setAttribute('y1', NyxData(AY1).ToJSON);
  LLine.setAttribute('x2', NyxData(AX2).ToJSON);
  LLine.setAttribute('y2', NyxData(AY2).ToJSON);
  LLine.setAttribute('stroke', AColor);
  LLine.setAttribute('stroke-width', NyxData(AWidth).ToJSON);
  AParent.appendChild(LLine);
end;

function TCanvasView.RulerFor(AAxis: TNyxCanvasAxis): TJSHTMLElement;
var
  LRulers: TNyxCanvasRulers;
begin
  RequireConnected;
  Result := nil;

  if not (AAxis in [ncaxX, ncaxY]) then
  begin
    raise ENyxCanvasPrecision.Create('Unknown ruler axis');
  end;
  LRulers := FGuideInput.Accepted.Rulers;

  if not LRulers.Defined or not (AAxis in LRulers.VisibleAxes) then
  begin
    Exit;
  end;

  if AAxis = ncaxX then
  begin
    Result := FTopRuler;
  end
  else
  begin
    Result := FLeftRuler;
  end;
end;

function TCanvasView.BuildRulerSVG(AAxis: TNyxCanvasAxis): TJSElement;
var
  LRulers: TNyxCanvasRulers;
  LTicks: TNyxCanvasRulerTicks;
  LText: TJSElement;
  LUnit: TJSElement;
  LIndex: Integer;
  LPixel: Double;
  LLength: Double;
  LStart: Double;
  LWidth: Double;
  LHeight: Double;

  procedure TextAt(const ACaption: TNyxText; AX, AY: Double; AVertical: Boolean);
  begin
    LText := PrecisionSVG('text');
    LText.textContent := ACaption;
    LText.setAttribute('x', NyxData(AX).ToJSON);
    LText.setAttribute('y', NyxData(AY).ToJSON);
    LText.setAttribute('fill', FTheme.Muted);
    LText.setAttribute('font-family', 'Segoe UI, sans-serif');
    LText.setAttribute('font-size', IntToStr(Max(9, Min(12, FTheme.FontSize - 3))));
    LText.setAttribute('dominant-baseline', 'text-before-edge');

    if AVertical then
    begin
      LText.setAttribute('transform', 'rotate(-90 ' + NyxData(AX).ToJSON + ' ' +
        NyxData(AY).ToJSON + ')');
    end;
    Result.appendChild(LText);
  end;
begin
  LRulers := FGuideInput.Display.Rulers;
  LTicks := NyxCanvasRulerTicks(LRulers, FCamera);
  LStart := 0;

  if ncaxX in LRulers.VisibleAxes then
  begin
    LStart := LRulers.StripSize;
  end;
  LWidth := FCamera.ViewportWidth;
  LHeight := LRulers.StripSize;

  if AAxis = ncaxY then
  begin
    LWidth := LRulers.StripSize;
    LHeight := Max(1, FCamera.ViewportHeight - LStart);
  end;
  Result := PrecisionSVG('svg');
  Result.setAttribute('width', '100%');
  Result.setAttribute('height', '100%');
  Result.setAttribute('viewBox', '0 0 ' + NyxData(LWidth).ToJSON + ' ' + NyxData(LHeight).ToJSON);
  Result.setAttribute('pointer-events', 'none');
  Result.setAttribute('aria-hidden', 'true');

  if AAxis = ncaxX then
  begin
    PrecisionLine(Result, 0, LHeight - 1, LWidth, LHeight - 1, FTheme.Border);

    if (ncaxY in LRulers.VisibleAxes) and (LRulers.UnitCaption <> '') then
    begin
      { Nested SVG clips the complete Unicode unit caption inside the corner. }
      LUnit := PrecisionSVG('svg');
      LUnit.setAttribute('width', NyxData(LRulers.StripSize).ToJSON);
      LUnit.setAttribute('height', NyxData(LRulers.StripSize).ToJSON);
      TextAt(LRulers.UnitCaption, 3, 3, False);
      LUnit.appendChild(LText);
      Result.appendChild(LUnit);
    end;
  end
  else
  begin
    PrecisionLine(Result, LWidth - 1, 0, LWidth - 1, LHeight, FTheme.Border);
  end;
  for LIndex := 0 to High(LTicks) do
  begin

    if LTicks[LIndex].Axis <> AAxis then
    begin
      Continue;
    end;
    LPixel := LTicks[LIndex].Pixel;
    LLength := 5;

    if LTicks[LIndex].Major then
    begin
      LLength := 10;
    end;

    if AAxis = ncaxX then
    begin

      if (ncaxY in LRulers.VisibleAxes) and (LPixel < LRulers.StripSize) then
      begin
        Continue;
      end;
      PrecisionLine(Result, LPixel, LHeight - LLength, LPixel, LHeight, FTheme.Border);

      if LTicks[LIndex].Major then
      begin
        TextAt(LTicks[LIndex].Caption, LPixel + 3, 2, False);
      end;
    end
    else
    begin
      LPixel := LPixel - LStart;

      if LPixel < 0 then
      begin
        Continue;
      end;
      PrecisionLine(Result, LWidth - LLength, LPixel, LWidth, LPixel, FTheme.Border);

      if LTicks[LIndex].Major then
      begin
        TextAt(LTicks[LIndex].Caption, 2, LPixel - 3, True);
      end;
    end;
  end;
end;

procedure TCanvasView.PaintPrecision;
var
  LOptions: TNyxCanvasPrecisionOptions;
  LRulers: TNyxCanvasRulers;
  LKey: TNyxText;
  LGuide: TNyxCanvasGuide;
  LSVG: TJSElement;
  LLine: TJSElement;
  LAxis: TNyxCanvasAxis;
  LFace: TJSHTMLElement;
  LIndex: Integer;
  LPixel: Double;
  LStart: Double;
  LEnabled: Boolean;
begin

  if (FSurface = nil) or (FGuideInput = nil) then
  begin
    Exit;
  end;
  LOptions := FGuideInput.Display;
  LRulers := LOptions.Rulers;
  LKey := LRulers.ToData.ToJSON;
  LStart := 0;
  LEnabled := FGate.EnabledPolicy.Allowed and not FGate.ReadOnlyPolicy.ReadOnly and
    not FDesignMode and (LOptions.Count < MaximumNyxCanvasGuides);

  if LRulers.Defined and (ncaxX in LRulers.VisibleAxes) then
  begin
    LStart := LRulers.StripSize;
  end;
  for LAxis := Low(TNyxCanvasAxis) to High(TNyxCanvasAxis) do
  begin
    LFace := FTopRuler;

    if LAxis = ncaxY then
    begin
      LFace := FLeftRuler;
    end;
    LFace.style.setProperty('display', 'none');

    if not LRulers.Defined or not (LAxis in LRulers.VisibleAxes) then
    begin
      Continue;
    end;
    LFace.style.setProperty('display', 'block');
    LFace.style.setProperty('position', 'absolute');
    LFace.style.setProperty('left', '0');
    LFace.style.setProperty('background-color', FTheme.Surface);
    LFace.style.setProperty('z-index', '2147483646');
    LFace.style.setProperty('touch-action', 'none');
    LFace.style.setProperty('user-select', 'none');
    LFace.style.setProperty('overflow', 'hidden');
    LFace.style.setProperty('cursor', 'crosshair');
    LFace.setAttribute('role', 'button');
    LFace.tabIndex := 0;

    if LEnabled then
    begin
      LFace.setAttribute('aria-disabled', 'false');
    end
    else
    begin
      LFace.setAttribute('aria-disabled', 'true');
    end;

    if LAxis = ncaxX then
    begin
      LFace.style.setProperty('top', '0');
      LFace.style.setProperty('width', '100%');
      LFace.style.setProperty('height', Metric(LRulers.StripSize));
      LFace.setAttribute('aria-label', 'Horizontal ruler. Drag or press Enter to add a horizontal guide. Units: ' +
        LRulers.UnitCaption);
    end
    else
    begin
      LFace.style.setProperty('top', Metric(LStart));
      LFace.style.setProperty('bottom', '0');
      LFace.style.setProperty('width', Metric(LRulers.StripSize));
      LFace.setAttribute('aria-label', 'Vertical ruler. Drag or press Enter to add a vertical guide. Units: ' +
        LRulers.UnitCaption);
    end;

    if not FLastRulerCamera.SameFrame(FCamera) or (FLastRulerData <> LKey) then
    begin
      LSVG := BuildRulerSVG(LAxis);

      if LFace.firstChild = nil then
      begin
        LFace.appendChild(LSVG);
      end
      else
      begin
        LFace.replaceChild(LSVG, LFace.firstChild);
      end;
    end;
  end;
  FLastRulerCamera := FCamera;
  FLastRulerData := LKey;
  LSVG := PrecisionSVG('svg');
  LSVG.setAttribute('width', '100%');
  LSVG.setAttribute('height', '100%');
  LSVG.setAttribute('viewBox', '0 0 ' + NyxData(FCamera.ViewportWidth).ToJSON + ' ' +
    NyxData(FCamera.ViewportHeight).ToJSON);
  LSVG.setAttribute('pointer-events', 'none');
  LSVG.setAttribute('aria-hidden', 'true');
  for LIndex := 0 to LOptions.Count - 1 do
  begin
    LGuide := LOptions.Guide(LIndex);

    if not LGuide.IsVisible then
    begin
      Continue;
    end;
    LLine := PrecisionSVG('line');

    if LGuide.Axis = ncaxX then
    begin
      LPixel := (LGuide.Position - FCamera.WorldCenter.X) * FCamera.Zoom +
        FCamera.ViewportWidth / 2;

      if (LPixel < 0) or (LPixel >= FCamera.ViewportWidth) then
      begin
        Continue;
      end;
      LLine.setAttribute('x1', NyxData(LPixel).ToJSON);
      LLine.setAttribute('x2', NyxData(LPixel).ToJSON);
      LLine.setAttribute('y1', '0');
      LLine.setAttribute('y2', NyxData(FCamera.ViewportHeight).ToJSON);
    end
    else
    begin
      LPixel := (LGuide.Position - FCamera.WorldCenter.Y) * FCamera.Zoom +
        FCamera.ViewportHeight / 2;

      if (LPixel < 0) or (LPixel >= FCamera.ViewportHeight) then
      begin
        Continue;
      end;
      LLine.setAttribute('y1', NyxData(LPixel).ToJSON);
      LLine.setAttribute('y2', NyxData(LPixel).ToJSON);
      LLine.setAttribute('x1', '0');
      LLine.setAttribute('x2', NyxData(FCamera.ViewportWidth).ToJSON);
    end;
    LLine.setAttribute('data-nyx-guide', LGuide.Reference.Name);
    LLine.setAttribute('stroke', FTheme.Accent);
    LLine.setAttribute('stroke-dasharray', '4 4');
    LLine.setAttribute('stroke-width', '1');

    if LGuide.Reference.Name = FGuideInput.Selection.Name then
    begin
      LLine.setAttribute('stroke-width', '2');
    end;
    LSVG.appendChild(LLine);
  end;

  if FGuideSVG = nil then
  begin
    FBackground.appendChild(LSVG);
  end
  else
  begin
    FBackground.replaceChild(LSVG, FGuideSVG);
  end;
  FGuideSVG := LSVG;
end;

function TCanvasView.GuidePoint(AX, AY: Double): TNyxCanvasPoint;
var
  LPoint: TNyxCanvasPoint;
  LX: Double;
  LY: Double;
begin
  LPoint := LocalPoint(AX, AY);
  LX := FCamera.WorldCenter.X + (LPoint.X - FCamera.ViewportWidth / 2) / FCamera.Zoom;
  LY := FCamera.WorldCenter.Y + (LPoint.Y - FCamera.ViewportHeight / 2) / FCamera.Zoom;
  Result := NyxCanvasPoint(Max(-MaximumNyxCanvasCoordinate, Min(MaximumNyxCanvasCoordinate, LX)),
    Max(-MaximumNyxCanvasCoordinate, Min(MaximumNyxCanvasCoordinate, LY)));
end;

procedure TCanvasView.CancelGuideGesture;
var
  LCapture: TJSHTMLElement;
  LPointer: NativeInt;
begin

  if (FGuideInput = nil) or (not FGuideInput.Active and not FGuideCapturing) then
  begin
    Exit;
  end;
  FGuideInput.Cancel;
  LCapture := FGuideCapture;
  LPointer := FGuidePointer;
  FGuideCapture := nil;
  FGuideCapturing := False;

  if (LCapture <> nil) and TNyxGestureElement(LCapture).hasPointerCapture(LPointer) then
  begin
    LCapture.releasePointerCapture(LPointer);
  end;

  if FConnected then
  begin
    PaintPrecision;

    if FResources <> nil then
    begin
      FResources.Wake;
    end;
  end;
end;

function TCanvasView.RulerDown(AEvent: TJSPointerEvent): Boolean;
var
  LAxis: TNyxCanvasAxis;
  LLease: INyxCanvasWorkspaceView;
begin
  Result := True;

  if not FConnected or FDesignMode or not FGate.EnabledPolicy.Allowed or
    FGate.ReadOnlyPolicy.ReadOnly or (AEvent.button <> 0) or FGuideCapturing or
    (FGuideInput.Accepted.Count >= MaximumNyxCanvasGuides) then
  begin
    Exit;
  end;
  RequirePrecisionRevision;
  LLease := Self;
  CancelPan;

  if not FConnected then
  begin
    Exit;
  end;
  LAxis := ncaxX;

  if AEvent.currentTarget = FTopRuler then
  begin
    LAxis := ncaxY;
  end;
  FGuideInput.BeginCreate(LAxis, GuidePoint(AEvent.clientX, AEvent.clientY));
  FGuideCapture := TJSHTMLElement(AEvent.currentTarget);
  FGuidePointer := AEvent.pointerId;
  FGuideCapturing := True;
  FGuideCapture.focus;

  if not FConnected or not FGuideInput.Active then
  begin
    Exit;
  end;

  if AEvent.isTrusted then
  begin
    FGuideCapture.setPointerCapture(FGuidePointer);
  end;
  AEvent.preventDefault;
  NotifyPrecision;
end;

function TCanvasView.RulerKey(AEvent: TJSKeyboardEvent): Boolean;
var
  LAxis: TNyxCanvasAxis;
  LLease: INyxCanvasWorkspaceView;
begin
  Result := True;
  AEvent.stopPropagation;

  if not FConnected or FDesignMode or not FGate.EnabledPolicy.Allowed or
    FGate.ReadOnlyPolicy.ReadOnly then
  begin
    Exit;
  end;

  if (AEvent.key = 'Enter') or (AEvent.key = ' ') then
  begin

    if FGuideInput.Active or (FGuideInput.Accepted.Count >= MaximumNyxCanvasGuides) then
    begin
      AEvent.preventDefault;
      Exit;
    end;
    RequirePrecisionRevision;
    LLease := Self;
    LAxis := ncaxX;

    if AEvent.currentTarget = FTopRuler then
    begin
      LAxis := ncaxY;
    end;
    FGuideInput.BeginCreate(LAxis, FCamera.WorldCenter);
    FGuideInput.Commit;
    AEvent.preventDefault;
    NotifyPrecision;
  end
  else
  begin
    Keyboard(AEvent);
  end;
end;

function TCanvasView.GuideKey(AEvent: TJSKeyboardEvent): Boolean;
var
  LGuide: TNyxCanvasGuide;
  LStep: Double;
  LChanged: Boolean;
begin
  Result := False;

  if FDesignMode or (FGuideInput = nil) then
  begin
    Exit;
  end;

  if FGuideInput.Active then
  begin

    if AEvent.key = 'Escape' then
    begin
      CancelGuideGesture;
      AEvent.preventDefault;
      Exit(True);
    end;

    if (AEvent.key = 'ArrowLeft') or (AEvent.key = 'ArrowRight') or
      (AEvent.key = 'ArrowUp') or (AEvent.key = 'ArrowDown') or
      (AEvent.key = 'Home') or (AEvent.key = '+') or (AEvent.key = '-') or
      (AEvent.key = '=') then
    begin
      AEvent.preventDefault;
      Exit(True);
    end;
    Exit;
  end;

  if FGate.ReadOnlyPolicy.ReadOnly or not FGuideInput.Selection.Defined then
  begin
    Exit;
  end;
  LGuide := FGuideInput.Accepted.Named(FGuideInput.Selection);

  if (AEvent.key <> 'ArrowLeft') and (AEvent.key <> 'ArrowRight') and
    (AEvent.key <> 'ArrowUp') and (AEvent.key <> 'ArrowDown') and
    (AEvent.key <> 'Delete') and (AEvent.key <> 'Escape') then
  begin
    Exit;
  end;
  RequirePrecisionRevision;
  Result := True;
  AEvent.preventDefault;

  if AEvent.key = 'Escape' then
  begin
    FGuideInput.Select(Default(TNyxCanvasGuideRef));
    NotifyPrecision;
    Exit;
  end;

  if LGuide.IsLocked then
  begin
    Exit;
  end;
  LStep := 1 / FCamera.Zoom;

  if AEvent.shiftKey then
  begin
    LStep := LStep * 10;
  end;
  LChanged := False;

  if AEvent.key = 'Delete' then
  begin
    LChanged := FGuideInput.RemoveSelected;
  end
  else if ((LGuide.Axis = ncaxX) and ((AEvent.key = 'ArrowLeft') or (AEvent.key = 'ArrowRight'))) or
    ((LGuide.Axis = ncaxY) and ((AEvent.key = 'ArrowUp') or (AEvent.key = 'ArrowDown'))) then
  begin

    if (AEvent.key = 'ArrowLeft') or (AEvent.key = 'ArrowUp') then
    begin
      LStep := -LStep;
    end;
    LChanged := FGuideInput.Nudge(LStep);
  end;

  if LChanged then
  begin
    NotifyPrecision;
  end;
end;
