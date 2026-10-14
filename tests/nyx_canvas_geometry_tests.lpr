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
program nyx_canvas_geometry_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.canvas.geometry
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Canvas geometry: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Near(AActual, AExpected: Double; const AReason: TNyxText);
begin
  Check(Abs(AActual - AExpected) <= 1e-8 * Max(1, Abs(AExpected)), AReason);
end;

procedure Run;
var
  LCamera: TNyxCanvasCamera;
  LChanged: TNyxCanvasCamera;
  LDefaultCamera: TNyxCanvasCamera;
  LPoint: TNyxCanvasPoint;
  LAnchor: TNyxCanvasPoint;
  LWorld: TNyxCanvasPoint;
  LBounds: TNyxCanvasBounds;
  LGrid: TNyxCanvasGrid;
  LChangedGrid: TNyxCanvasGrid;
  LDefaultGrid: TNyxCanvasGrid;
  LDefaultPoint: TNyxCanvasPoint;
  LDefaultBounds: TNyxCanvasBounds;
  LDefaultLimits: TNyxCanvasZoomLimits;
  LIndex: Integer;
  LZoomIndex: Integer;
  LCase: Integer;
  LRefused: Boolean;
  LZoom: Double;
begin
  LDefaultCamera := Default(TNyxCanvasCamera);
  LDefaultPoint := Default(TNyxCanvasPoint);
  LDefaultBounds := Default(TNyxCanvasBounds);
  LDefaultLimits := Default(TNyxCanvasZoomLimits);
  LDefaultGrid := Default(TNyxCanvasGrid);
  Check(not LDefaultCamera.Defined and not LDefaultPoint.Defined and
    not LDefaultBounds.Defined and not LDefaultLimits.Defined and
    not LDefaultGrid.Defined, 'default records are absent');

  LCamera := NyxCanvasCamera(800, 600, NyxCanvasPoint(-120, 80), 2);
  LPoint := LCamera.WorldToViewport(NyxCanvasPoint(-110, 60));
  Near(LPoint.X, 420, 'known horizontal world projection');
  Near(LPoint.Y, 260, 'known vertical world projection');
  LBounds := LCamera.VisibleWorld;
  Near(LBounds.Left, -320, 'signed visible world left');
  Near(LBounds.Top, -70, 'signed visible world top');
  Near(LBounds.Width, 400, 'visible world width');
  Check(LBounds.Contains(NyxCanvasPoint(-320, -70)) and
    LBounds.Intersects(NyxCanvasBounds(80, 0, 30, 20)) and
    not LBounds.Intersects(NyxCanvasBounds(81, 0, 30, 20)),
    'closed query edges include touching shapes');

  { Independent projections and anchor invariants over negative/positive world
    positions and several scales. Formula expectations are not generated from
    the implementation's inverse function. }
  for LZoomIndex := 0 to 4 do
  begin
    LZoom := 0.125 * Power(2, LZoomIndex);
    LCamera := NyxCanvasCamera(1000, 700, NyxCanvasPoint(-300, 170), LZoom);
    for LIndex := -20 to 20 do
    begin
      LWorld := NyxCanvasPoint(LIndex * 17.5, LIndex * -13.25);
      LPoint := LCamera.WorldToViewport(LWorld);
      Near(LPoint.X, (LWorld.X + 300) * LZoom + 500, 'horizontal projection oracle');
      Near(LPoint.Y, (LWorld.Y - 170) * LZoom + 350, 'vertical projection oracle');
      LPoint := LCamera.ViewportToWorld(LPoint);
      Near(LPoint.X, LWorld.X, 'horizontal transform round trip');
      Near(LPoint.Y, LWorld.Y, 'vertical transform round trip');
      LAnchor := NyxCanvasPoint(100 + LIndex * 2, 500 - LIndex * 3);
      LWorld := LCamera.ViewportToWorld(LAnchor);
      LChanged := LCamera.ZoomAt(LZoom * 3, LAnchor);
      LPoint := LChanged.WorldToViewport(LWorld);
      Near(LPoint.X, LAnchor.X, 'anchored zoom horizontal invariant');
      Near(LPoint.Y, LAnchor.Y, 'anchored zoom vertical invariant');
    end;
  end;

  LCamera := NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0), 2);
  LChanged := LCamera.PanBy(40, -60);
  Near(LChanged.WorldCenter.X, -20, 'hand drag changes world center inversely');
  Near(LChanged.WorldCenter.Y, 30, 'hand drag vertical center');
  LPoint := LChanged.WorldToViewport(NyxCanvasPoint(0, 0));
  Near(LPoint.X, 440, 'drag moves content right');
  Near(LPoint.Y, 240, 'drag moves content up');
  Near(LCamera.WorldCenter.X, 0, 'original camera remains immutable');
  LChanged := LCamera.WithViewport(390, 640);
  Near(LChanged.WorldCenter.Y, 0, 'viewport resize retains center');
  Near(LChanged.Zoom, 2, 'viewport resize retains zoom');
  LAnchor := NyxCanvasPoint(80, 120);
  LWorld := LCamera.ViewportToWorld(LAnchor);
  LChanged := LCamera.ZoomAt(1000, LAnchor);
  Near(LChanged.Zoom, 16, 'requested zoom clamps at caller maximum');
  LPoint := LChanged.WorldToViewport(LWorld);
  Near(LPoint.X, 80, 'clamped zoom retains anchor X');
  Near(LPoint.Y, 120, 'clamped zoom retains anchor Y');
  LChanged := LCamera.WithLimits(NyxCanvasZoomLimits(3, 4));
  Near(LChanged.Zoom, 3, 'new caller limits reconcile current zoom');

  LWorld := LCamera.ViewportToWorld(NyxCanvasPoint(200, 200));
  LChanged := LCamera.Pinch(NyxCanvasPoint(150, 200), NyxCanvasPoint(250, 200),
    NyxCanvasPoint(210, 250), NyxCanvasPoint(410, 250));
  Near(LChanged.Zoom, 4, 'two-contact pinch doubles scale');
  LPoint := LChanged.WorldToViewport(LWorld);
  Near(LPoint.X, 310, 'pinch follows moving midpoint horizontally');
  Near(LPoint.Y, 250, 'pinch follows moving midpoint vertically');
  LChanged := LCamera.Pinch(NyxCanvasPoint(100, 200), NyxCanvasPoint(300, 200),
    NyxCanvasPoint(310, 250), NyxCanvasPoint(310, 450));
  Near(LChanged.Zoom, 2, 'rotated contacts do not implicitly rotate the camera');

  LBounds := NyxCanvasBounds(-200, -100, 400, 200);
  LChanged := LCamera.Fit(LBounds, 40);
  Near(LChanged.Zoom, 1.8, 'fit uses limiting horizontal dimension');
  Near(LChanged.WorldCenter.X, 0, 'fit centers authored bounds');
  Check(LChanged.VisibleWorld.Contains(NyxCanvasPoint(-200, -100)) and
    LChanged.VisibleWorld.Contains(NyxCanvasPoint(200, 100)), 'fit includes content');
  LChanged := LCamera.Fit(NyxCanvasBounds(50, 60, 0, 0));
  Near(LChanged.Zoom, 2, 'point bounds retain zoom');
  Near(LChanged.WorldCenter.X, 50, 'point bounds center correctly');
  LChanged := LCamera.Fit(NyxCanvasBounds(-5, -100, 0, 200), 40);
  Near(LChanged.Zoom, 2.6, 'line bounds fit their nonzero dimension');

  LGrid := NyxCanvasGrid(20, ncgsNone, ncsmGrid).AtOrigin(NyxCanvasPoint(5, -5));
  LPoint := LGrid.Snap(NyxCanvasPoint(15, -15));
  Near(LPoint.X, 25, 'positive grid tie rounds away from origin');
  Near(LPoint.Y, -25, 'negative grid tie rounds away from origin');
  Check(LGrid.Style = ncgsNone, 'hidden grid still snaps');
  LChangedGrid := LGrid.Styled(ncgsLines).Snapping(ncsmFree);
  LPoint := LChangedGrid.Snap(NyxCanvasPoint(17.25, -12.75));
  Near(LPoint.X, 17.25, 'visible grid permits free horizontal placement');
  Near(LPoint.Y, -12.75, 'visible grid permits free vertical placement');
  Check((LGrid.Style = ncgsNone) and (LGrid.SnapMode = ncsmGrid),
    'changed grid retains independent original settings');
  LChanged := LCamera.ZoomAt(0.05, NyxCanvasPoint(400, 300));
  Near(LGrid.VisibleSpacing(LChanged), 320, 'visible grid coarsens at low zoom');
  Near(LGrid.Spacing, 20, 'adaptive visible grid never changes snap spacing');

  { Admission cases exercise no published partial camera/grid changes. }
  for LCase := 0 to 16 do
  begin
    LRefused := False;
    try
      case LCase of
        0:
          begin
            LPoint := NyxCanvasPoint(NaN, 0);
          end;
        1:
          begin
            LPoint := NyxCanvasPoint(0, Infinity);
          end;
        2:
          begin
            LPoint := NyxCanvasPoint(MaximumNyxCanvasCoordinate + 1, 0);
          end;
        3:
          begin
            LChanged := NyxCanvasCamera(0, 500, NyxCanvasPoint(0, 0));
          end;
        4:
          begin
            LChanged := LCamera.ZoomAt(0, NyxCanvasPoint(0, 0));
          end;
        5:
          begin
            LChanged := LCamera.ZoomAt(NaN, NyxCanvasPoint(0, 0));
          end;
        6:
          begin
            LChanged := LCamera.Fit(LBounds, 400);
          end;
        7:
          begin
            LChanged := LCamera.Pinch(NyxCanvasPoint(0, 0), NyxCanvasPoint(0, 0),
              NyxCanvasPoint(0, 0), NyxCanvasPoint(20, 0));
          end;
        8:
          begin
            LChanged := LCamera.AtCenter(NyxCanvasPoint(MaximumNyxCanvasCoordinate, 0));
          end;
        9:
          begin
            LChanged := LDefaultCamera.PanBy(1, 1);
          end;
        10:
          begin
            LPoint := LDefaultPoint.Offset(1, 1);
          end;
        11:
          begin
            LChanged := LCamera.Fit(LDefaultBounds);
          end;
        12:
          begin
            LChanged := LCamera.WithLimits(LDefaultLimits);
          end;
        13:
          begin
            LPoint := LDefaultGrid.Snap(NyxCanvasPoint(0, 0));
          end;
        14:
          begin
            LChangedGrid := NyxCanvasGrid(0);
          end;
        15:
          begin
            LBounds := NyxCanvasBounds(0, 0, -1, 2);
          end;
        16:
          begin
            LChangedGrid := NyxCanvasGrid(0.000001, ncgsDots, ncsmGrid);
          end;
      end;

      if LCase = 16 then
      begin
        LPoint := LChangedGrid.Snap(NyxCanvasPoint(1000000000000.0, 0));
      end;
    except
      on ENyxCanvasGeometry do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'invalid or absent geometry refuses case ' + IntToStr(LCase));
    Near(LCamera.Zoom, 2, 'refusal retains original admitted camera');
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-geometry', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' canvas geometry checks';
    {$else}
    WriteLn('PASS ', GChecks, ' canvas geometry checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-geometry', 'failed');
      document.body.setAttribute('data-canvas-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
