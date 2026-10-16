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
{ Included by nyx.canvas.declarations; builders own detached saved plans. }

function TCanvasDefinition.GetPrecision: TNyxCanvasPrecisionOptions;
begin
  Result := FPrecision;
end;

function NyxCanvasPrecisionOptionsOf(
  const ADefinition: INyxCanvasDefinition): TNyxCanvasPrecisionOptions;
var
  LPrecision: INyxCanvasDefinitionPrecision;
begin

  if ADefinition = nil then
  begin
    raise ENyxCanvasScene.Create('Precision inspection requires a saved definition');
  end;
  Result := Default(TNyxCanvasPrecisionOptions);

  if Supports(ADefinition, INyxCanvasDefinitionPrecision, LPrecision) then
  begin
    Result := LPrecision.Precision;
  end;
end;

constructor TCanvasPrecision.CreateOwned(APlan: TCanvasDefinition);
begin
  inherited Create;
  FPlan := APlan;
end;

destructor TCanvasPrecision.Destroy;
begin
  FPlan.Free;
  inherited Destroy;
end;

function TCanvasPrecision.Current: TNyxCanvasPrecisionOptions;
begin
  Result := FPlan.FPrecision;

  if not Result.Defined then
  begin
    Result := NyxCanvasPrecisionOptions;
  end;
end;

function TCanvasPrecision.Options(const AValue: TNyxCanvasPrecisionOptions): INyxCanvasPrecision;
var
  LOptions: TNyxCanvasPrecisionOptions;
  LCopy: TCanvasDefinition;
begin
  LOptions := TNyxCanvasPrecisionOptions.FromData(AValue.ToData);
  LCopy := FPlan.CopyPlan;
  try
    LCopy.FPrecision := LOptions;

    if NyxUTF8ByteCount(LCopy.ToData.ToJSON) > MaximumNyxCanvasDefinitionBytes then
    begin
      raise ENyxCanvasScene.Create('Precision exceeds the complete canvas definition budget');
    end;
    Result := TCanvasPrecision.CreateOwned(LCopy);
    LCopy := nil;
  finally
    LCopy.Free;
  end;
end;

function TCanvasPrecision.Rulers(const AValue: TNyxCanvasRulers): INyxCanvasPrecision;
begin
  Result := Options(Current.WithRulers(AValue));
end;

function TCanvasPrecision.Guide(const AValue: TNyxCanvasGuide): INyxCanvasPrecision;
begin
  Result := Options(Current.WithGuide(AValue));
end;

function TCanvasPrecision.RemoveGuide(
  const AReference: TNyxCanvasGuideRef): INyxCanvasPrecision;
begin
  Result := Options(Current.WithoutGuide(AReference));
end;

function TCanvasPrecision.Snapping(AMode: TNyxCanvasGuideSnapping;
  ATolerancePixels: Double): INyxCanvasPrecision;
begin
  Result := Options(Current.Snapping(AMode, ATolerancePixels));
end;

function TCanvasPrecision.Done: INyxCanvasDefinition;
begin
  Result := FPlan.CopyPlan;
end;

function NyxCanvasPrecision(const ADefinition: INyxCanvasDefinition): INyxCanvasPrecision;
var
  LPlan: TCanvasDefinition;
begin
  LPlan := CopyAdmittedCanvasPlan(ADefinition);
  try
    Result := TCanvasPrecision.CreateOwned(LPlan);
    LPlan := nil;
  finally
    LPlan.Free;
  end;
end;

function TCanvasDefinition.WithPrecision(
  const AValue: TNyxCanvasPrecisionOptions): INyxCanvasDefinition;
var
  LOptions: TNyxCanvasPrecisionOptions;
  LPlan: TCanvasDefinition;
begin
  LOptions := TNyxCanvasPrecisionOptions.FromData(AValue.ToData);
  LPlan := CopyPlan;
  try
    LPlan.FPrecision := LOptions;

    if NyxUTF8ByteCount(LPlan.ToData.ToJSON) > MaximumNyxCanvasDefinitionBytes then
    begin
      raise ENyxCanvasScene.Create('Precision exceeds the complete canvas definition budget');
    end;
    Result := LPlan;
    LPlan := nil;
  finally
    LPlan.Free;
  end;
end;
