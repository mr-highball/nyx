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
{ Included by nyx.canvas.lcl. Rulers are small native siblings above retained
  items; guide lines paint behind those items. No full-surface input overlay
  intercepts embedded editors. All world/tick/gesture meaning is portable. }

constructor TCanvasRuler.CreateFor(AParent: TWinControl; AView: TCanvasView;
  AAxis: TNyxCanvasAxis);
begin
  inherited Create(AParent);
  View := AView;
  Axis := AAxis;
  Parent := AParent;
  Visible := False;
  TabStop := True;
  DoubleBuffered := True;
  ShowHint := True;
  OnMouseDown := AView.RulerDown;
  OnMouseMove := AView.Move;
  OnMouseUp := AView.Up;
  OnKeyDown := AView.RulerKey;
end;

procedure TCanvasRuler.Paint;
begin

  if View <> nil then
  begin
    View.PaintRuler(Self);
  end;
end;

procedure TCanvasRuler.WndProc(var AMessage: TLMessage);
begin

  if (AMessage.Msg = LM_CAPTURECHANGED) and (View <> nil) and
    (View.FGuideInput <> nil) and View.FGuideInput.Active then
  begin
    View.CancelGuideGesture;
  end;
  inherited WndProc(AMessage);
end;

function TCanvasView.RulerFor(AAxis: TNyxCanvasAxis): TWinControl;
begin
  RequireConnected;
  Result := nil;

  if not (AAxis in [ncaxX, ncaxY]) then
  begin
    raise ENyxCanvasPrecision.Create('Unknown ruler axis');
  end;

  if (AAxis = ncaxX) and FTopRuler.Visible then
  begin
    Result := FTopRuler;
  end
  else if (AAxis = ncaxY) and FLeftRuler.Visible then
  begin
    Result := FLeftRuler;
  end;
end;

procedure TCanvasView.PaintPrecision;
var
  LRulers: TNyxCanvasRulers;
  LSize: Integer;
  LTop: Integer;
begin

  if (FSurface = nil) or (FGuideInput = nil) then
  begin
    Exit;
  end;
  LRulers := FGuideInput.Display.Rulers;
  LSize := 0;

  if LRulers.Defined then
  begin
    LSize := Round(LRulers.StripSize);
  end;
  FTopRuler.Visible := LRulers.Defined and (ncaxX in LRulers.VisibleAxes);
  FLeftRuler.Visible := LRulers.Defined and (ncaxY in LRulers.VisibleAxes);
  FTopRuler.Enabled := FGate.EnabledPolicy.Allowed and not FGate.ReadOnlyPolicy.ReadOnly and
    not FDesignMode and (FGuideInput.Accepted.Count < MaximumNyxCanvasGuides);
  FLeftRuler.Enabled := FTopRuler.Enabled;
  LTop := 0;

  if FTopRuler.Visible then
  begin
    LTop := LSize;
    FTopRuler.SetBounds(0, 0, FSurface.ClientWidth, LSize);
    FTopRuler.Caption := 'Horizontal ruler';
    FTopRuler.Hint := 'Drag to add a horizontal guide. Enter adds one at the center. Units: ' +
      LRulers.UnitCaption;
    FTopRuler.BringToFront;
    FTopRuler.Invalidate;
  end;

  if FLeftRuler.Visible then
  begin
    FLeftRuler.SetBounds(0, LTop, LSize, Max(0, FSurface.ClientHeight - LTop));
    FLeftRuler.Caption := 'Vertical ruler';
    FLeftRuler.Hint := 'Drag to add a vertical guide. Enter adds one at the center. Units: ' +
      LRulers.UnitCaption;
    FLeftRuler.BringToFront;
    FLeftRuler.Invalidate;
  end;
end;

procedure TCanvasView.PaintRuler(ARuler: TCanvasRuler);
var
  LRulers: TNyxCanvasRulers;
  LTicks: TNyxCanvasRulerTicks;
  LIndex: Integer;
  LPixel: Integer;
  LLength: Integer;
  LStart: Integer;
  LCanvas: TCanvas;
begin

  if not FConnected or (FGuideInput = nil) then
  begin
    Exit;
  end;
  LRulers := FGuideInput.Display.Rulers;
  LTicks := NyxCanvasRulerTicks(LRulers, FCamera);
  LCanvas := ARuler.Canvas;
  LCanvas.Brush.Color := NyxLCLColor(FTheme.Surface);
  LCanvas.FillRect(ARuler.ClientRect);
  LCanvas.Pen.Color := NyxLCLColor(FTheme.Border);
  LCanvas.Pen.Width := 1;
  LCanvas.Pen.Style := psSolid;
  LCanvas.Font.Name := 'Segoe UI';
  LCanvas.Font.Height := -Max(9, Min(12, FTheme.FontSize - 3));
  LCanvas.Font.Color := NyxLCLColor(FTheme.Muted);
  LCanvas.Font.Orientation := 0;
  LCanvas.Brush.Style := bsClear;
  LStart := 0;

  if ncaxX in LRulers.VisibleAxes then
  begin
    LStart := Round(LRulers.StripSize);
  end;

  if ARuler.Axis = ncaxX then
  begin
    LCanvas.Line(0, ARuler.Height - 1, ARuler.Width, ARuler.Height - 1);

    if ncaxY in LRulers.VisibleAxes then
    begin
      LCanvas.TextRect(Rect(0, 0, Round(LRulers.StripSize), ARuler.Height), 3, 3,
        LRulers.UnitCaption);
    end;
  end
  else
  begin
    LCanvas.Line(ARuler.Width - 1, 0, ARuler.Width - 1, ARuler.Height);
    { LCL expresses font rotation in tenths of a degree. The matched Win32
      widgetset rotates its owned drawing font; no child input is transformed. }
    LCanvas.Font.Orientation := 900;
  end;
  for LIndex := 0 to High(LTicks) do
  begin

    if LTicks[LIndex].Axis <> ARuler.Axis then
    begin
      Continue;
    end;
    LPixel := Round(LTicks[LIndex].Pixel);
    LLength := 5;

    if LTicks[LIndex].Major then
    begin
      LLength := 10;
    end;

    if ARuler.Axis = ncaxX then
    begin

      if (ncaxY in LRulers.VisibleAxes) and (LPixel < Round(LRulers.StripSize)) then
      begin
        Continue;
      end;
      LCanvas.Line(LPixel, ARuler.Height - LLength, LPixel, ARuler.Height);

      if LTicks[LIndex].Major then
      begin
        LCanvas.TextOut(LPixel + 3, 2, LTicks[LIndex].Caption);
      end;
    end
    else
    begin
      Dec(LPixel, LStart);

      if LPixel < 0 then
      begin
        Continue;
      end;
      LCanvas.Line(ARuler.Width - LLength, LPixel, ARuler.Width, LPixel);

      if LTicks[LIndex].Major then
      begin
        LCanvas.TextOut(2, LPixel - 3, LTicks[LIndex].Caption);
      end;
    end;
  end;
  LCanvas.Font.Orientation := 0;
  LCanvas.Brush.Style := bsSolid;
end;

procedure TCanvasView.PaintGuides(ACanvas: TCanvas);
var
  LOptions: TNyxCanvasPrecisionOptions;
  LGuide: TNyxCanvasGuide;
  LIndex: Integer;
  LPixel: Double;
begin

  if FGuideInput = nil then
  begin
    Exit;
  end;
  LOptions := FGuideInput.Display;
  ACanvas.Pen.Color := NyxLCLColor(FTheme.Accent);
  ACanvas.Pen.Style := psDash;
  for LIndex := 0 to LOptions.Count - 1 do
  begin
    LGuide := LOptions.Guide(LIndex);

    if not LGuide.IsVisible then
    begin
      Continue;
    end;
    ACanvas.Pen.Width := 1;

    if LGuide.Reference.Name = FGuideInput.Selection.Name then
    begin
      ACanvas.Pen.Width := 2;
    end;

    if LGuide.Axis = ncaxX then
    begin
      LPixel := (LGuide.Position - FCamera.WorldCenter.X) * FCamera.Zoom +
        FCamera.ViewportWidth / 2;

      if (LPixel >= 0) and (LPixel < FSurface.ClientWidth) then
      begin
        ACanvas.Line(Round(LPixel), 0, Round(LPixel), FSurface.ClientHeight);
      end;
    end
    else
    begin
      LPixel := (LGuide.Position - FCamera.WorldCenter.Y) * FCamera.Zoom +
        FCamera.ViewportHeight / 2;

      if (LPixel >= 0) and (LPixel < FSurface.ClientHeight) then
      begin
        ACanvas.Line(0, Round(LPixel), FSurface.ClientWidth, Round(LPixel));
      end;
    end;
  end;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;
end;

function TCanvasView.GuidePoint(ASender: TObject; AX, AY: Integer): TNyxCanvasPoint;
var
  LPoint: TPoint;
  LX: Double;
  LY: Double;
begin
  LPoint := FSurface.ScreenToClient(TControl(ASender).ClientToScreen(Point(AX, AY)));
  LX := FCamera.WorldCenter.X + (LPoint.X - FCamera.ViewportWidth / 2) / FCamera.Zoom;
  LY := FCamera.WorldCenter.Y + (LPoint.Y - FCamera.ViewportHeight / 2) / FCamera.Zoom;
  Result := NyxCanvasPoint(Max(-MaximumNyxCanvasCoordinate, Min(MaximumNyxCanvasCoordinate, LX)),
    Max(-MaximumNyxCanvasCoordinate, Min(MaximumNyxCanvasCoordinate, LY)));
end;

procedure TCanvasView.CancelGuideGesture;
var
  LCapture: TWinControl;
begin

  if FGuideInput = nil then
  begin
    Exit;
  end;

  if not FGuideInput.Active and (FGuideCapture = nil) then
  begin
    Exit;
  end;
  FGuideInput.Cancel;
  LCapture := FGuideCapture;
  FGuideCapture := nil;

  if LCapture <> nil then
  begin
    TCanvasInputAccess(LCapture).MouseCapture := False;
  end;

  if FConnected then
  begin
    PaintPrecision;
    FSurface.Invalidate;

    if FResources <> nil then
    begin
      FResources.Wake;
    end;
  end;
end;

procedure TCanvasView.RulerDown(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LAxis: TNyxCanvasAxis;
  LLease: INyxCanvasWorkspaceView;
begin

  if not FConnected or FDesignMode or not FGate.EnabledPolicy.Allowed or
    FGate.ReadOnlyPolicy.ReadOnly or FGuideInput.Active or (AButton <> mbLeft) or
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

  if TCanvasRuler(ASender).Axis = ncaxX then
  begin
    LAxis := ncaxY;
  end;
  FGuideInput.BeginCreate(LAxis, GuidePoint(ASender, AX, AY));
  TWinControl(ASender).SetFocus;

  if not FConnected or not FGuideInput.Active then
  begin
    Exit;
  end;
  FGuideCapture := TWinControl(ASender);
  TCanvasInputAccess(FGuideCapture).MouseCapture := True;
  NotifyPrecision;
end;

procedure TCanvasView.RulerKey(ASender: TObject; var AKey: Word; AShift: TShiftState);
var
  LAxis: TNyxCanvasAxis;
  LLease: INyxCanvasWorkspaceView;
begin

  if not FConnected or FDesignMode or not FGate.EnabledPolicy.Allowed or
    FGate.ReadOnlyPolicy.ReadOnly then
  begin
    Exit;
  end;

  if AKey in [VK_RETURN, VK_SPACE] then
  begin

    if FGuideInput.Active or (FGuideInput.Accepted.Count >= MaximumNyxCanvasGuides) then
    begin
      AKey := 0;
      Exit;
    end;
    RequirePrecisionRevision;
    LLease := Self;
    LAxis := ncaxX;

    if TCanvasRuler(ASender).Axis = ncaxX then
    begin
      LAxis := ncaxY;
    end;
    FGuideInput.BeginCreate(LAxis, FCamera.WorldCenter);
    FGuideInput.Commit;
    AKey := 0;
    NotifyPrecision;
  end
  else
  begin
    Keyboard(ASender, AKey, AShift);
  end;
end;

function TCanvasView.GuideKey(var AKey: Word; AShift: TShiftState): Boolean;
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

  if (AKey = VK_ESCAPE) and FGuideInput.Active then
  begin
    CancelGuideGesture;
    AKey := 0;
    Exit(True);
  end;

  if FGuideInput.Active and (AKey in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
    VK_HOME, VK_ADD, VK_OEM_PLUS, VK_SUBTRACT, VK_OEM_MINUS]) then
  begin
    AKey := 0;
    Exit(True);
  end;

  if FGate.ReadOnlyPolicy.ReadOnly or not FGuideInput.Selection.Defined or FGuideInput.Active then
  begin
    Exit;
  end;
  LGuide := FGuideInput.Accepted.Named(FGuideInput.Selection);

  if not (AKey in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_DELETE, VK_ESCAPE]) then
  begin
    Exit;
  end;
  RequirePrecisionRevision;
  Result := True;

  if AKey = VK_ESCAPE then
  begin
    FGuideInput.Select(Default(TNyxCanvasGuideRef));
    AKey := 0;
    NotifyPrecision;
    Exit;
  end;

  if LGuide.IsLocked then
  begin
    AKey := 0;
    Exit;
  end;
  LStep := 1 / FCamera.Zoom;

  if ssShift in AShift then
  begin
    LStep := LStep * 10;
  end;
  LChanged := False;

  if AKey = VK_DELETE then
  begin
    LChanged := FGuideInput.RemoveSelected;
  end
  else if ((LGuide.Axis = ncaxX) and (AKey in [VK_LEFT, VK_RIGHT])) or
    ((LGuide.Axis = ncaxY) and (AKey in [VK_UP, VK_DOWN])) then
  begin

    if AKey in [VK_LEFT, VK_UP] then
    begin
      LStep := -LStep;
    end;
    LChanged := FGuideInput.Nudge(LStep);
  end;
  AKey := 0;

  if LChanged then
  begin
    NotifyPrecision;
  end;
end;
