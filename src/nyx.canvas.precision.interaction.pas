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
unit nyx.canvas.precision.interaction;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.canvas.geometry, nyx.canvas.precision;

type
  { One owner-thread interaction state machine, independent of DOM/LCL and any
    document/history. Accepted options stay unchanged during captured previews.
    Commit installs once; Cancel restores the prior selection and exact options.
    Reset validates before retiring a gesture. All returned values are immutable;
    no camera, view, receiver or external capability is retained. }
  INyxCanvasGuideInteraction = interface(IInterface)
    ['{2CAB13D6-C881-49DD-AC66-10160A100005}']
    function GetAccepted: TNyxCanvasPrecisionOptions;
    function GetDisplay: TNyxCanvasPrecisionOptions;
    function GetSelection: TNyxCanvasGuideRef;
    function GetActive: Boolean;
    procedure Reset(const AOptions: TNyxCanvasPrecisionOptions);
    procedure Select(const AReference: TNyxCanvasGuideRef);
    function Hit(const APoint: TNyxCanvasPoint; const ACamera: TNyxCanvasCamera;
      ATolerancePixels: Double = 5): TNyxCanvasGuideRef;
    procedure BeginCreate(AAxis: TNyxCanvasAxis; const APoint: TNyxCanvasPoint);
    procedure BeginMove(const AReference: TNyxCanvasGuideRef);
    procedure Move(const APoint: TNyxCanvasPoint; const ACamera: TNyxCanvasCamera;
      const AGrid: TNyxCanvasGrid);
    function Commit: Boolean;
    procedure Cancel;
    function Nudge(AWorldDelta: Double): Boolean;
    function RemoveSelected: Boolean;
    property Accepted: TNyxCanvasPrecisionOptions read GetAccepted;
    property Display: TNyxCanvasPrecisionOptions read GetDisplay;
    property Selection: TNyxCanvasGuideRef read GetSelection;
    property Active: Boolean read GetActive;
  end;

{ Independent reference-counted state. Explicit options and every subsequent
  reset are normalized through the same strict public precision descriptor. }
function NewNyxCanvasGuideInteraction(
  const AOptions: TNyxCanvasPrecisionOptions): INyxCanvasGuideInteraction;

implementation

uses Math;

type
  { Sole mutable owner of one local gesture. Arrays inside its value snapshots
    are immutable and copy before writing; reference counting owns no UI cycle. }
  TGuideInteraction = class(TInterfacedObject, INyxCanvasGuideInteraction)
  private
    FAccepted: TNyxCanvasPrecisionOptions;
    FDisplay: TNyxCanvasPrecisionOptions;
    FSelection: TNyxCanvasGuideRef;
    FBeforeSelection: TNyxCanvasGuideRef;
    FActive: Boolean;
    FGuide: TNyxCanvasGuide;
    procedure RequireIdle;
  public
    constructor Create;
    function GetAccepted: TNyxCanvasPrecisionOptions;
    function GetDisplay: TNyxCanvasPrecisionOptions;
    function GetSelection: TNyxCanvasGuideRef;
    function GetActive: Boolean;
    procedure Reset(const AOptions: TNyxCanvasPrecisionOptions);
    procedure Select(const AReference: TNyxCanvasGuideRef);
    function Hit(const APoint: TNyxCanvasPoint; const ACamera: TNyxCanvasCamera;
      ATolerancePixels: Double): TNyxCanvasGuideRef;
    procedure BeginCreate(AAxis: TNyxCanvasAxis; const APoint: TNyxCanvasPoint);
    procedure BeginMove(const AReference: TNyxCanvasGuideRef);
    procedure Move(const APoint: TNyxCanvasPoint; const ACamera: TNyxCanvasCamera;
      const AGrid: TNyxCanvasGrid);
    function Commit: Boolean;
    procedure Cancel;
    function Nudge(AWorldDelta: Double): Boolean;
    function RemoveSelected: Boolean;
  end;

function NewNyxCanvasGuideInteraction(
  const AOptions: TNyxCanvasPrecisionOptions): INyxCanvasGuideInteraction;
begin
  Result := TGuideInteraction.Create;
  Result.Reset(AOptions);
end;

constructor TGuideInteraction.Create;
begin
  inherited Create;
  FAccepted := Default(TNyxCanvasPrecisionOptions);
  FDisplay := Default(TNyxCanvasPrecisionOptions);
  FSelection := Default(TNyxCanvasGuideRef);
  FBeforeSelection := Default(TNyxCanvasGuideRef);
  FGuide := Default(TNyxCanvasGuide);
end;

procedure TGuideInteraction.RequireIdle;
begin

  if FActive then
  begin
    raise ENyxCanvasPrecision.Create('Finish or cancel the current guide gesture first');
  end;
end;

function TGuideInteraction.GetAccepted: TNyxCanvasPrecisionOptions;
begin
  Result := FAccepted;
end;

function TGuideInteraction.GetDisplay: TNyxCanvasPrecisionOptions;
begin
  Result := FAccepted;

  if FActive then
  begin
    Result := FDisplay;
  end;
end;

function TGuideInteraction.GetSelection: TNyxCanvasGuideRef;
begin
  Result := FSelection;
end;

function TGuideInteraction.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TGuideInteraction.Reset(const AOptions: TNyxCanvasPrecisionOptions);
var
  LOptions: TNyxCanvasPrecisionOptions;
begin
  LOptions := TNyxCanvasPrecisionOptions.FromData(AOptions.ToData);
  Cancel;
  FAccepted := LOptions;
  FDisplay := Default(TNyxCanvasPrecisionOptions);

  if not FAccepted.Contains(FSelection) then
  begin
    FSelection := Default(TNyxCanvasGuideRef);
  end;
end;

procedure TGuideInteraction.Select(const AReference: TNyxCanvasGuideRef);
begin
  RequireIdle;

  if AReference.Defined and not FAccepted.Contains(AReference) then
  begin
    raise ENyxCanvasPrecision.Create('Select a guide in this accepted precision snapshot');
  end;
  FSelection := AReference;
end;

function TGuideInteraction.Hit(const APoint: TNyxCanvasPoint;
  const ACamera: TNyxCanvasCamera; ATolerancePixels: Double): TNyxCanvasGuideRef;
var
  LIndex: Integer;
  LGuide: TNyxCanvasGuide;
  LDistance: Double;
  LBest: Double;
begin

  if not APoint.Defined or not ACamera.Defined or IsNan(ATolerancePixels) or
    IsInfinite(ATolerancePixels) or (ATolerancePixels < 0) or (ATolerancePixels > 64) then
  begin
    raise ENyxCanvasPrecision.Create('Guide hit testing requires admitted point, camera and tolerance');
  end;
  Result := Default(TNyxCanvasGuideRef);
  LBest := ATolerancePixels + 1;
  for LIndex := 0 to FAccepted.Count - 1 do
  begin
    LGuide := FAccepted.Guide(LIndex);

    if not LGuide.IsVisible then
    begin
      Continue;
    end;

    if LGuide.Axis = ncaxX then
    begin
      LDistance := Abs(LGuide.Position - APoint.X) * ACamera.Zoom;
    end
    else
    begin
      LDistance := Abs(LGuide.Position - APoint.Y) * ACamera.Zoom;
    end;

    if (LDistance <= ATolerancePixels) and (LDistance < LBest) then
    begin
      Result := LGuide.Reference;
      LBest := LDistance;
    end;
  end;
end;

procedure TGuideInteraction.BeginCreate(AAxis: TNyxCanvasAxis; const APoint: TNyxCanvasPoint);
var
  LOptions: TNyxCanvasPrecisionOptions;
  LGuide: TNyxCanvasGuide;
  LPosition: Double;
begin
  RequireIdle;

  if not APoint.Defined then
  begin
    raise ENyxCanvasPrecision.Create('A new guide requires an admitted world point');
  end;
  LOptions := FAccepted;

  if not LOptions.Defined then
  begin
    LOptions := NyxCanvasPrecisionOptions;
  end;
  LPosition := APoint.X;

  if AAxis = ncaxY then
  begin
    LPosition := APoint.Y;
  end;
  LGuide := NyxCanvasGuide(LOptions.NextGuide, AAxis, LPosition);
  LOptions := LOptions.WithGuide(LGuide);
  { Validate the whole preview before recording any capture/selection state. }
  LOptions := TNyxCanvasPrecisionOptions.FromData(LOptions.ToData);
  FBeforeSelection := FSelection;
  FGuide := LGuide;
  FSelection := LGuide.Reference;
  FDisplay := LOptions;
  FActive := True;
end;

procedure TGuideInteraction.BeginMove(const AReference: TNyxCanvasGuideRef);
var
  LGuide: TNyxCanvasGuide;
begin
  RequireIdle;
  LGuide := FAccepted.Named(AReference);

  if LGuide.IsLocked then
  begin
    raise ENyxCanvasPrecision.Create('Unlock this guide before moving it');
  end;
  FBeforeSelection := FSelection;
  FGuide := LGuide;
  FSelection := AReference;
  FDisplay := FAccepted;
  FActive := True;
end;

procedure TGuideInteraction.Move(const APoint: TNyxCanvasPoint;
  const ACamera: TNyxCanvasCamera; const AGrid: TNyxCanvasGrid);
var
  LPoint: TNyxCanvasPoint;
  LSnap: TNyxCanvasGuideSnap;
  LGuide: TNyxCanvasGuide;
  LPosition: Double;
begin

  if not FActive then
  begin
    raise ENyxCanvasPrecision.Create('Start a guide gesture before moving its preview');
  end;
  LPoint := APoint;

  if AGrid.Defined then
  begin
    LPoint := AGrid.Snap(LPoint);
  end;
  LSnap := SnapNyxCanvasGuides(FAccepted, APoint, ACamera, FGuide.Reference);

  if LSnap.XGuide.Defined then
  begin
    LPoint := NyxCanvasPoint(LSnap.Point.X, LPoint.Y);
  end;

  if LSnap.YGuide.Defined then
  begin
    LPoint := NyxCanvasPoint(LPoint.X, LSnap.Point.Y);
  end;
  LPosition := LPoint.X;

  if FGuide.Axis = ncaxY then
  begin
    LPosition := LPoint.Y;
  end;
  LGuide := FGuide.AtPosition(LPosition);
  FDisplay := FDisplay.WithGuide(LGuide);
end;

function TGuideInteraction.Commit: Boolean;
begin

  if not FActive then
  begin
    Exit(False);
  end;
  Result := FAccepted.ToData.ToJSON <> FDisplay.ToData.ToJSON;
  FAccepted := FDisplay;
  FActive := False;
  FDisplay := Default(TNyxCanvasPrecisionOptions);
end;

procedure TGuideInteraction.Cancel;
begin

  if FActive then
  begin
    FSelection := FBeforeSelection;
  end;
  FActive := False;
  FDisplay := Default(TNyxCanvasPrecisionOptions);
end;

function TGuideInteraction.Nudge(AWorldDelta: Double): Boolean;
var
  LGuide: TNyxCanvasGuide;
begin
  RequireIdle;
  Result := False;

  if not FSelection.Defined then
  begin
    Exit;
  end;
  LGuide := FAccepted.Named(FSelection);

  if LGuide.IsLocked then
  begin
    raise ENyxCanvasPrecision.Create('Unlock this guide before moving it');
  end;
  LGuide := LGuide.AtPosition(LGuide.Position + AWorldDelta);
  Result := LGuide.Position <> FAccepted.Named(FSelection).Position;
  FAccepted := FAccepted.WithGuide(LGuide);
end;

function TGuideInteraction.RemoveSelected: Boolean;
var
  LGuide: TNyxCanvasGuide;
begin
  RequireIdle;
  Result := False;

  if not FSelection.Defined then
  begin
    Exit;
  end;
  LGuide := FAccepted.Named(FSelection);

  if LGuide.IsLocked then
  begin
    raise ENyxCanvasPrecision.Create('Unlock this guide before removing it');
  end;
  FAccepted := FAccepted.WithoutGuide(FSelection);
  FSelection := Default(TNyxCanvasGuideRef);
  Result := True;
end;

end.
