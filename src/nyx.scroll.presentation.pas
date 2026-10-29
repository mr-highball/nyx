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
unit nyx.scroll.presentation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data;

const
  NyxMaximumScrollThickness = 256;

type
  { Invalid presentation input refuses before changing a model or native face. }
  ENyxScrollPresentation = class(Exception);

  { Only chrome changes size. Content offsets, typography and layout remain
    owned by the viewport/canvas. A physical floor is in final displayed pixels,
    independent of the current content zoom, and cannot invent missing space. }
  TNyxScrollSizing = (nssFollowZoom, nssKeepSize, nssMinimumThickness);

  { Immutable value authoring, with no widget, renderer or callback references.
    The default record is the same valid Follow zoom value as the factory.
    Absence is represented by the containing descriptor, not by an invalid value.
    Adapters must either honor the requested allocation or report unavailable;
    platform native input/accessibility remain with the original scrolling face. }
  TNyxScrollPresentation = record
  private
    FInitialized: Boolean;
    FSizing: TNyxScrollSizing;
    FMinimumThickness: Integer;
    function Normalized: TNyxScrollPresentation;
    function GetSizing: TNyxScrollSizing;
    function GetMinimumThickness: Integer;
  public
    function Copy: TNyxScrollPresentation;
    procedure Validate;
    function Sizing(AValue: TNyxScrollSizing): TNyxScrollPresentation;
    { Selecting a floor also selects its sizing mode. The stored floor survives
      switching modes so an editor can change that choice without losing it. }
    function MinimumThickness(AValue: Integer): TNyxScrollPresentation;
    { Convert a copied logical host thickness into the chrome's absolute factor.
      Never feed a previously projected thickness back as the logical baseline. }
    function EffectiveScale(AContentScale, ALogicalThickness: Double): Double;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxScrollPresentation; static;
    property SizingMode: TNyxScrollSizing read GetSizing;
    property PhysicalMinimum: Integer read GetMinimumThickness;
  end;

function NyxScrollPresentation: TNyxScrollPresentation;
function NyxScrollSizingName(AValue: TNyxScrollSizing): TNyxText;
function ReadNyxScrollSizing(const AValue: TNyxText): TNyxScrollSizing;

implementation

uses Math;

const
  CSizingNames: array[TNyxScrollSizing] of TNyxText =
    ('follow-zoom', 'keep-size', 'minimum-thickness');

function NyxScrollSizingName(AValue: TNyxScrollSizing): TNyxText;
begin

  if not (AValue in [nssFollowZoom, nssKeepSize, nssMinimumThickness]) then
  begin
    raise ENyxScrollPresentation.Create('Unknown scroll sizing choice');
  end;
  Result := CSizingNames[AValue];
end;

function ReadNyxScrollSizing(const AValue: TNyxText): TNyxScrollSizing;
var
  LValue: TNyxScrollSizing;
begin
  for LValue := Low(TNyxScrollSizing) to High(TNyxScrollSizing) do
  begin

    if AValue = CSizingNames[LValue] then
    begin
      Exit(LValue);
    end;
  end;
  raise ENyxScrollPresentation.Create('Unknown scroll sizing choice: ' + AValue);
end;

function NyxScrollPresentation: TNyxScrollPresentation;
begin
  Result := Default(TNyxScrollPresentation);
  Result.FInitialized := True;
  Result.FSizing := nssFollowZoom;
  Result.FMinimumThickness := 14;
end;

function TNyxScrollPresentation.Normalized: TNyxScrollPresentation;
begin
  Result := Self;

  if not FInitialized then
  begin
    Result := NyxScrollPresentation;
  end;
end;

function TNyxScrollPresentation.Copy: TNyxScrollPresentation;
begin
  Validate;
  Result := Normalized;
end;

procedure TNyxScrollPresentation.Validate;
var
  LValue: TNyxScrollPresentation;
begin
  LValue := Normalized;
  NyxScrollSizingName(LValue.FSizing);

  if (LValue.FMinimumThickness < 1) or
    (LValue.FMinimumThickness > NyxMaximumScrollThickness) then
  begin
    raise ENyxScrollPresentation.Create('Scroll thickness must be between 1 and 256 pixels');
  end;
end;

function TNyxScrollPresentation.GetSizing: TNyxScrollSizing;
begin
  Result := Normalized.FSizing;
end;

function TNyxScrollPresentation.GetMinimumThickness: Integer;
begin
  Result := Normalized.FMinimumThickness;
end;

function TNyxScrollPresentation.Sizing(AValue: TNyxScrollSizing): TNyxScrollPresentation;
begin
  Result := Copy;
  Result.FSizing := AValue;
  Result.Validate;
end;

function TNyxScrollPresentation.MinimumThickness(AValue: Integer): TNyxScrollPresentation;
begin
  Result := Copy;
  Result.FSizing := nssMinimumThickness;
  Result.FMinimumThickness := AValue;
  Result.Validate;
end;

function TNyxScrollPresentation.EffectiveScale(AContentScale,
  ALogicalThickness: Double): Double;
begin
  Validate;

  if IsNan(AContentScale) or IsInfinite(AContentScale) or (AContentScale <= 0) or
    IsNan(ALogicalThickness) or IsInfinite(ALogicalThickness) or (ALogicalThickness <= 0) then
  begin
    raise ENyxScrollPresentation.Create('Scroll projection requires finite positive scale and thickness');
  end;
  Result := AContentScale;
  case SizingMode of
    nssFollowZoom:
      begin
        Result := AContentScale;
      end;
    nssKeepSize:
      begin
        Result := 1;
      end;
    nssMinimumThickness:
      begin
        Result := Max(Result, PhysicalMinimum / ALogicalThickness);
      end;
  end;

  if IsInfinite(Result * ALogicalThickness) or
    (Result * ALogicalThickness > High(Integer) - 1.0) then
  begin
    raise ENyxScrollPresentation.Create('Scroll thickness exceeds its physical integer domain');
  end;
end;

function TNyxScrollPresentation.ToData: TNyxDataValue;
var
  LValue: TNyxScrollPresentation;
begin
  LValue := Copy;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('sizing', NyxData(NyxScrollSizingName(LValue.FSizing))),
    NyxField('minimumThickness', NyxData(LValue.FMinimumThickness))]);
end;

class function TNyxScrollPresentation.FromData(
  const AData: TNyxDataValue): TNyxScrollPresentation;
var
  LValue: TNyxScrollPresentation;
  LIndex: Integer;
  LKey: TNyxText;
  LMinimum: Integer;
begin

  if not AData.Defined or (AData.Kind <> ndObject) or (AData.Count <> 3) then
  begin
    raise ENyxScrollPresentation.Create('Scroll presentation requires its exact object shape');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LKey := AData.Key(LIndex);

    if (LKey <> 'version') and (LKey <> 'sizing') and (LKey <> 'minimumThickness') then
    begin
      raise ENyxScrollPresentation.Create('Unknown scroll presentation member: ' + LKey);
    end;
  end;

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxScrollPresentation.Create('Unsupported scroll presentation version');
  end;
  LValue := NyxScrollPresentation;
  LValue.FSizing := ReadNyxScrollSizing(AData.Field('sizing').AsText);
  LMinimum := AData.Field('minimumThickness').AsInteger;

  if (LMinimum < 1) or (LMinimum > NyxMaximumScrollThickness) then
  begin
    { AsInteger first admits an exact signed 32-bit spelling on both targets.
      Apply the narrower pixel domain explicitly even without range checking. }
    raise ENyxScrollPresentation.Create('Scroll thickness must be between 1 and 256 pixels');
  end;
  LValue.FMinimumThickness := LMinimum;
  LValue.Validate;
  Result := LValue;
end;

end.
