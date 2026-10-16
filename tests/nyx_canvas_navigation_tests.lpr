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

program nyx_canvas_navigation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Math, nyx.canvas.geometry, nyx.canvas.types, nyx.canvas.navigation,
  nyx.canvas.navigation.sources, nyx.scheduler
  {$ifdef PAS2JS}, Web{$endif};

type
  { Mutable creator fixture returns its own arrays deliberately. The hub must
    detach them before another callback or later caller changes their contents.
    Hub and Other are borrowed only while Run owns both; retained scopes and
    the optional next source are revoked before those test owners are released. }
  TSource = class(TInterfacedObject, INyxCanvasNavigationSource)
  public
    Frame: TNyxCanvasNavigationFrame;
    OwnScope: INyxCancellationScope;
    CancelDuringCapture: Boolean;
    Hub: TNyxCanvasNavigationSources;
    Other: TSource;
    NextSource: INyxCanvasNavigationSource;
    AddedScope: INyxCancellationScope;
    Reenter: Boolean;
    CloseDuringCapture: Boolean;
    function Capture(const ACamera: TNyxCanvasCamera): TNyxCanvasNavigationFrame;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    raise Exception.Create('Navigation: ' + AReason);
  end;
  Inc(GChecks);
end;

function TSource.Capture(const ACamera: TNyxCanvasCamera): TNyxCanvasNavigationFrame;
begin
  Result := Frame;

  if Other <> nil then
  begin
    Other.Frame.Items[0].Bounds := NyxCanvasBounds(101, 102, 103, 104);
  end;

  if NextSource <> nil then
  begin
    AddedScope := Hub.Subscribe(NextSource);
    NextSource := nil;
  end;

  if Reenter then
  begin
    Hub.Capture(ACamera);
  end;

  if CloseDuringCapture then
  begin
    Hub.Close;
  end;

  if CancelDuringCapture then
  begin
    OwnScope.Cancel;
  end;
end;

function ScreenBounds(const ACamera: TNyxCanvasCamera;
  const AItem: TNyxCanvasNavigationItem): TNyxCanvasBounds;
var
  LPoint: TNyxCanvasPoint;
  LScale: Double;
begin
  LPoint := ACamera.WorldToViewport(NyxCanvasPoint(AItem.Bounds.Left, AItem.Bounds.Top));
  LScale := 1;

  if AItem.Scaling = nccsReflow then
  begin
    LScale := ACamera.Zoom;
  end;
  Result := NyxCanvasBounds(LPoint.X, LPoint.Y, AItem.Bounds.Width * LScale, AItem.Bounds.Height * LScale);
end;

procedure CheckInside(const AResult: TNyxCanvasNavigationResult;
  const AItem: TNyxCanvasNavigationItem);
var
  LBounds: TNyxCanvasBounds;
begin
  LBounds := ScreenBounds(AResult.Camera, AItem);
  Check(AResult.Region.Defined and (LBounds.Left >= AResult.Region.Left - 0.000001) and
    (LBounds.Top >= AResult.Region.Top - 0.000001) and
    (LBounds.Left + LBounds.Width <= AResult.Region.Left + AResult.Region.Width + 0.000001) and
    (LBounds.Top + LBounds.Height <= AResult.Region.Top + AResult.Region.Height + 0.000001),
    'actual projected extent fits the returned usable rectangle');
end;

procedure Run;
var
  LCamera: TNyxCanvasCamera;
  LCompact: TNyxCanvasCamera;
  LResult: TNyxCanvasNavigationResult;
  LItems: TNyxCanvasNavigationItems;
  LBlock: TNyxCanvasBounds;
  LBefore: TNyxCanvasPoint;
  LFailed: Boolean;
  LScheduler: INyxScheduler;
  LHub: TNyxCanvasNavigationSources;
  LSource: TSource;
  LSecond: TSource;
  LLease: INyxCanvasNavigationSource;
  LSecondLease: INyxCanvasNavigationSource;
  LScope: INyxCancellationScope;
  LSecondScope: INyxCancellationScope;
  LScopes: array of INyxCancellationScope;
  LFrame: TNyxCanvasNavigationFrame;
  LIndex: Integer;
begin
  LCamera := NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0), 1);
  LResult := FitNyxCanvasNavigation(LCamera, [], [], NyxCanvasNavigationOptions);
  Check((LResult.Status = ncnsEmpty) and (LResult.Camera.WorldCenter.X = 0) and
    (LResult.Camera.Zoom = 1), 'empty navigation preserves the camera');
  SetLength(LItems, 2);
  LItems[0] := NyxCanvasNavigationItem(NyxCanvasBounds(-10000, 5000, 280, 200), nccsLogicalSize);
  LItems[1] := NyxCanvasNavigationItem(NyxCanvasBounds(-7000, 6000, 280, 200), nccsLogicalSize);
  LBefore := NyxCanvasPoint(LItems[0].Bounds.Left, LItems[0].Bounds.Top);
  LResult := FitNyxCanvasNavigation(LCamera, LItems, [], NyxCanvasNavigationOptions);
  Check((LResult.Status = ncnsComplete) and (LResult.Camera.Zoom < 1), 'widely separated fixed cards fit by changing anchor scale');
  CheckInside(LResult, LItems[0]);
  CheckInside(LResult, LItems[1]);
  Check((ScreenBounds(LResult.Camera, LItems[0]).Width = 280) and
    (LItems[0].Bounds.Left = LBefore.X), 'logical card size and input world geometry remain exact');
  LItems[0] := NyxCanvasNavigationItem(NyxCanvasBounds(-2000, -2000, 600, 400), nccsReflow);
  LResult := FitNyxCanvasNavigation(LCamera, LItems, [], NyxCanvasNavigationOptions);
  Check(LResult.Status = ncnsComplete, 'mixed scaled scene and fixed overlay fit together');
  CheckInside(LResult, LItems[0]);
  CheckInside(LResult, LItems[1]);

  LBlock := NyxCanvasBounds(432, 352, 360, 240);
  LResult := FitNyxCanvasNavigation(LCamera, LItems, [LBlock], NyxCanvasNavigationOptions);
  Check(LResult.Status = ncnsComplete, 'floating tools leave a valid fit alternative');
  CheckInside(LResult, LItems[0]);
  CheckInside(LResult, LItems[1]);
  Check(not ((LResult.Region.Left < LBlock.Left + LBlock.Width) and
    (LResult.Region.Left + LResult.Region.Width > LBlock.Left) and
    (LResult.Region.Top < LBlock.Top + LBlock.Height) and
    (LResult.Region.Top + LResult.Region.Height > LBlock.Top)),
    'the selected region does not overlap floating tools');

  LCompact := NyxCanvasCamera(390, 360, NyxCanvasPoint(0, 0), 1);
  LItems[0] := NyxCanvasNavigationItem(NyxCanvasBounds(9000, -8000, 280, 320), nccsLogicalSize);
  LBlock := NyxCanvasBounds(302, 308, 80, 44);
  LResult := RevealNyxCanvasNavigation(LCompact, LItems[0], [LBlock], NyxCanvasNavigationOptions);
  Check((LResult.Status = ncnsComplete) and (LResult.Camera.Zoom = 1),
    'compact reveal preserves readable zoom and avoids a small corner tool');
  CheckInside(LResult, LItems[0]);
  LResult := RevealNyxCanvasNavigation(LResult.Camera, LItems[0], [LBlock], NyxCanvasNavigationOptions);
  Check(LResult.Status = ncnsComplete, 'reveal of an already visible card remains complete');
  CheckInside(LResult, LItems[0]);
  LItems[0] := NyxCanvasNavigationItem(NyxCanvasBounds(0, 0, 500, 500), nccsLogicalSize);
  LResult := RevealNyxCanvasNavigation(LCompact, LItems[0], [], NyxCanvasNavigationOptions);
  Check(LResult.Status = ncnsLimited, 'an oversized logical card reports incomplete visibility');
  Check(Abs(ScreenBounds(LResult.Camera, LItems[0]).Left) < 0.000001,
    'oversized reveal retains useful leading-edge access');

  LItems[0] := NyxCanvasNavigationItem(NyxCanvasBounds(0, 0, 280, 200), nccsLogicalSize);
  LItems[1] := NyxCanvasNavigationItem(NyxCanvasBounds(10000, 0, 280, 200), nccsLogicalSize);
  LResult := FitNyxCanvasNavigation(LCamera.WithLimits(NyxCanvasZoomLimits(0.5, 2)),
    LItems, [], NyxCanvasNavigationOptions);
  Check((LResult.Status = ncnsLimited) and (LResult.Camera.Zoom = 0.5),
    'caller minimum zoom remains authoritative when it prevents complete fit');
  LResult := FitNyxCanvasNavigation(LCamera, LItems,
    [NyxCanvasBounds(0, 0, 800, 600)], NyxCanvasNavigationOptions);
  Check((LResult.Status = ncnsLimited) and not LResult.Region.Defined and
    (LResult.Camera.WorldCenter.X = 0), 'fully occluded view retains its camera and reports no usable region');
  LResult := FitNyxCanvasNavigation(LCamera, LItems,
    [NyxCanvasBounds(0, 0, 800, 600)], NyxCanvasNavigationOptions.Chrome(ncncIgnore));
  Check(LResult.Status = ncnsComplete, 'explicit caller override can ignore floating chrome');
  LFailed := False;
  try
    FitNyxCanvasNavigation(LCamera, [Default(TNyxCanvasNavigationItem)], [], NyxCanvasNavigationOptions);
  except
    on E: ENyxCanvasGeometry do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LCamera.Zoom = 1), 'undefined extent refuses without mutating the original camera');
  LItems[0] := NyxCanvasNavigationItem(NyxCanvasBounds(0, 0, 280, 200), nccsLogicalSize, True);
  LItems[1] := NyxCanvasNavigationItem(NyxCanvasBounds(30, 0, 280, 200), nccsLogicalSize, True);
  LResult := FitNyxCanvasNavigation(LCompact, LItems, [], NyxCanvasNavigationOptions);
  Check(LResult.Status = ncnsLimited, 'readable cards that overlap do not claim complete visibility');
  LResult := FitNyxCanvasNavigation(LCompact, LItems, [], NyxCanvasNavigationOptions.Distinct(False));
  Check(LResult.Status = ncnsComplete, 'callers can explicitly allow overlapping presentation');

  LItems[0] := NyxCanvasNavigationItem(NyxCanvasBounds(0, 0, 10, 10), nccsReflow);
  LResult := FitNyxCanvasNavigation(LCamera.ZoomAt(0.25, NyxCanvasPoint(400, 300)), [LItems[0]], [],
    NyxCanvasNavigationOptions.Inset(0));
  Check(LResult.Camera.Zoom <= 0.25, 'default Fit never magnifies beyond the current camera');
  LResult := FitNyxCanvasNavigation(LCamera.ZoomAt(0.25, NyxCanvasPoint(400, 300)), [LItems[0]], [],
    NyxCanvasNavigationOptions.Inset(0).ZoomIn(True));
  Check((LResult.Camera.Zoom > 0.25) and
    (LResult.Camera.Zoom <= LCamera.Limits.Maximum), 'explicit magnification still obeys caller zoom limits');

  LScheduler := NewNyxScheduler;
  LHub := TNyxCanvasNavigationSources.Create(LScheduler);
  LSource := TSource.Create;
  LLease := LSource;
  try
    SetLength(LSource.Frame.Items, 1);
    LSource.Frame.Items[0] := LItems[0];
    LScope := LHub.Subscribe(LLease);
    LFrame := LHub.Capture(LCamera);
    LSource.Frame.Items[0].Bounds := NyxCanvasBounds(77, 88, 99, 100);
    Check((Length(LFrame.Items) = 1) and (LFrame.Items[0].Bounds.Left = 0),
      'source array mutation cannot rewrite an already captured navigation window');
    LSource.OwnScope := LScope;
    LSource.CancelDuringCapture := True;
    LFrame := LHub.Capture(LCamera);
    Check((Length(LFrame.Items) = 0) and LScope.Cancelled,
      'cancellation during capture retires that contribution before publication');
    LSource.CancelDuringCapture := False;
    LSource.OwnScope := nil;
    LScope := LHub.Subscribe(LLease);
    LSource.Hub := LHub;
    LSecond := TSource.Create;
    LSecondLease := LSecond;
    SetLength(LSecond.Frame.Items, 1);
    LSecond.Frame.Items[0] := LItems[1];
    LSource.NextSource := LSecondLease;
    LFrame := LHub.Capture(LCamera);
    Check(Length(LFrame.Items) = 1, 'a registration inside a callback starts on the next capture');
    LSecondScope := LSource.AddedScope;
    LSecond.Other := LSource;
    LFrame := LHub.Capture(LCamera);
    Check((Length(LFrame.Items) = 2) and (LFrame.Items[0].Bounds.Left = 77),
      'a later receiver cannot rewrite an earlier admitted contribution');
    LSecond.Other := nil;
    LSource.Reenter := True;
    LFailed := False;
    try
      LHub.Capture(LCamera);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    LSource.Reenter := False;
    LFrame := LHub.Capture(LCamera);
    Check(LFailed and (Length(LFrame.Items) = 2),
      'recursive capture refuses and its guard retires for the next ordinary call');
    SetLength(LSecond.Frame.Items, MaximumNyxCanvasNavigationItems);
    LFailed := False;
    try
      LHub.Capture(LCamera);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'combined contribution budget refuses before examining unadmitted tail geometry');
    SetLength(LSecond.Frame.Items, 1);
    SetLength(LScopes, 30);
    for LIndex := 0 to High(LScopes) do
    begin
      LScopes[LIndex] := LHub.Subscribe(LSecondLease);
    end;
    LFailed := False;
    try
      LHub.Subscribe(LSecondLease);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'source count is bounded rather than silently truncating contributors');
    LScopes[0].Cancel;
    LScopes[0] := LHub.Subscribe(LSecondLease);
    Check(not LScopes[0].Cancelled, 'revoked registration capacity is reusable');
    LSource.CloseDuringCapture := True;
    LFailed := False;
    try
      LHub.Capture(LCamera);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LScope.Cancelled and LSecondScope.Cancelled,
      'closing during capture refuses the incomplete frame and revokes every source');
  finally
    LSource.NextSource := nil;
    LSource.AddedScope := nil;
    LSource.Hub := nil;
    LSecondLease := nil;
    LScopes := nil;
    LSecondScope := nil;
    LHub.Free;
    LLease := nil;
  end;
  Check(LScope.Cancelled, 'a retained cancellation scope safely outlives its hub');
  LScope := nil;
  LScheduler := nil;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-navigation', 'passed');
    document.body.setAttribute('data-canvas-navigation-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable canvas navigation checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-navigation', 'failed');
      document.body.setAttribute('data-canvas-navigation-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
