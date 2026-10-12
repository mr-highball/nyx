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
unit nyx.collections.columns;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.layout.constraints, nyx.layout.flow;

type
  { Closed, portable column intentions. Content shares available space in
    proportion to host-measured natural widths, retaining their bounded minimum.
    Fixed uses pixels; Flex uses a positive integer proportion. Bounds apply to
    all modes. Explicit minimum overflow is reachable by horizontal scrolling. }
  TNyxColumnSizingMode = (ncsContent, ncsFixed, ncsFlex);

  { Immutable-by-value authoring policy. No widget, callback or dataset is held.
    The default record is absent; the factory creates Content with bounds
    1..MaximumNyxLayoutBound. Fluent calls validate a copy before returning, so
    failure cannot modify a previously retained policy. Pixel units are the
    same logical plane as Nyx's ordinary layout constraints. }
  TNyxColumnSizing = record
  private
    FDefined: Boolean;
    FMode: TNyxColumnSizingMode;
    FValue: Integer;
    FRange: TNyxSizeRange;
  public
    function Content: TNyxColumnSizing;
    function Fixed(APixels: Integer): TNyxColumnSizing;
    function Flex(AWeight: Integer): TNyxColumnSizing;
    function Minimum(APixels: Integer): TNyxColumnSizing;
    function Maximum(APixels: Integer): TNyxColumnSizing;
    procedure Validate;
    { Null is absent. Closed version-one persistence refuses extra/missing
      fields, wrong scalar families and invalid modes/values/bounds. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxColumnSizing; static;
    property Defined: Boolean read FDefined;
    property Mode: TNyxColumnSizingMode read FMode;
    property Value: Integer read FValue;
    property Bounds: TNyxSizeRange read FRange;
  end;
  TNyxColumnSizings = array of TNyxColumnSizing;

function NyxColumnSizing: TNyxColumnSizing;
{ Detached widths for 1..64 columns. All policies and positive host-measured
  natural widths are admitted before allocation. Absent policies mean Content.
  Native/browser adapters share the existing constrained Nyx flow implementation;
  input arrays and previous allocations remain untouched. Available may be zero,
  in which case the positive column minima overflow rather than hiding cells. }
function NyxColumnWidths(AAvailable: Integer; const APolicies: TNyxColumnSizings;
  const ANatural: TNyxFlowSizes): TNyxFlowSizes;

implementation

uses
  Math;

function NyxColumnSizing: TNyxColumnSizing;
begin
  Result := Default(TNyxColumnSizing);
  Result.FDefined := True;
  Result.FMode := ncsContent;
  Result.FRange := NyxSizeRange.Minimum(1).Maximum(MaximumNyxLayoutBound);
end;

procedure TNyxColumnSizing.Validate;
begin

  if not FDefined then
  begin
    raise EArgumentException.Create('Create a column sizing policy before configuring it');
  end;
  FRange.Validate;

  if (Ord(FMode) < Ord(Low(TNyxColumnSizingMode))) or
    (Ord(FMode) > Ord(High(TNyxColumnSizingMode))) or
    not FRange.HasMinimum or not FRange.HasMaximum or
    (FRange.MinimumValue < 1) or (FRange.MaximumValue > MaximumNyxLayoutBound) then
  begin
    raise EArgumentException.Create('Column sizing requires an admitted mode and positive bounded range');
  end;

  if ((FMode = ncsContent) and (FValue <> 0)) or
    ((FMode <> ncsContent) and ((FValue < 1) or (FValue > MaximumNyxLayoutBound))) then
  begin
    raise EArgumentException.Create('Column extent or proportion is outside its admitted range');
  end;
end;

function TNyxColumnSizing.Content: TNyxColumnSizing;
begin
  Result := Self;
  Result.FMode := ncsContent;
  Result.FValue := 0;
  Result.Validate;
end;

function TNyxColumnSizing.Fixed(APixels: Integer): TNyxColumnSizing;
begin
  Result := Self;
  Result.FMode := ncsFixed;
  Result.FValue := APixels;
  Result.Validate;
end;

function TNyxColumnSizing.Flex(AWeight: Integer): TNyxColumnSizing;
begin
  Result := Self;
  Result.FMode := ncsFlex;
  Result.FValue := AWeight;
  Result.Validate;
end;

function TNyxColumnSizing.Minimum(APixels: Integer): TNyxColumnSizing;
begin
  Validate;
  Result := Self;
  Result.FRange := FRange.Minimum(APixels);
  Result.Validate;
end;

function TNyxColumnSizing.Maximum(APixels: Integer): TNyxColumnSizing;
begin
  Validate;
  Result := Self;
  Result.FRange := FRange.Maximum(APixels);
  Result.Validate;
end;

function TNyxColumnSizing.ToData: TNyxDataValue;
const
  CNames: array[TNyxColumnSizingMode] of TNyxText = ('content', 'fixed', 'flex');
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('mode', NyxData(CNames[FMode])), NyxField('value', NyxData(FValue)),
    NyxField('minimum', NyxData(FRange.MinimumValue)),
    NyxField('maximum', NyxData(FRange.MaximumValue))]);
end;

class function TNyxColumnSizing.FromData(const AData: TNyxDataValue): TNyxColumnSizing;
var
  LIndex: Integer;
  LMode: TNyxText;
begin
  Result := Default(TNyxColumnSizing);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if (AData.Kind <> ndObject) or (AData.Count <> 5) then
  begin
    raise EArgumentException.Create('Column sizing requires its complete version-one object');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if Pos('|' + AData.Key(LIndex) + '|', '|version|mode|value|minimum|maximum|') = 0 then
    begin
      raise EArgumentException.Create('Unknown column sizing field');
    end;
  end;

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise EArgumentException.Create('Unknown column sizing version');
  end;
  Result := NyxColumnSizing;
  LMode := AData.Field('mode').AsText;

  if LMode = 'content' then
  begin
    Result.FMode := ncsContent;
  end
  else if LMode = 'fixed' then
  begin
    Result.FMode := ncsFixed;
  end
  else if LMode = 'flex' then
  begin
    Result.FMode := ncsFlex;
  end
  else
  begin
    raise EArgumentException.Create('Unknown column sizing mode');
  end;
  Result.FValue := AData.Field('value').AsInteger;
  Result.FRange := NyxSizeRange.Minimum(AData.Field('minimum').AsInteger)
    .Maximum(AData.Field('maximum').AsInteger);
  Result.Validate;
end;

function NyxColumnWidths(AAvailable: Integer; const APolicies: TNyxColumnSizings;
  const ANatural: TNyxFlowSizes): TNyxFlowSizes;
var
  LItems: TNyxFlowItems;
  LRanges: TNyxSizeRanges;
  LPolicy: TNyxColumnSizing;
  LIndex: Integer;
begin

  if (AAvailable < 0) or (Length(APolicies) < 1) or (Length(APolicies) > 64) or
    (Length(APolicies) <> Length(ANatural)) then
  begin
    raise EArgumentException.Create('Column sizing requires an admitted host and parallel column metrics');
  end;
  SetLength(LItems, Length(APolicies));
  SetLength(LRanges, Length(APolicies));
  for LIndex := 0 to High(APolicies) do
  begin

    if ANatural[LIndex] < 1 then
    begin
      raise EArgumentException.Create('Natural column widths must be positive');
    end;
    LPolicy := APolicies[LIndex];

    if not LPolicy.Defined then
    begin
      LPolicy := NyxColumnSizing;
    end;
    LPolicy.Validate;
    LItems[LIndex].Visible := True;
    LRanges[LIndex] := LPolicy.Bounds;
    case LPolicy.Mode of
      ncsContent:
        begin
          LItems[LIndex].Weight := Min(ANatural[LIndex], MaximumNyxLayoutBound);
          LRanges[LIndex] := LRanges[LIndex].Minimum(LRanges[LIndex].Clamp(ANatural[LIndex]));
        end;
      ncsFixed:
        begin
          LItems[LIndex].NaturalSize := LPolicy.Value;
        end;
      ncsFlex:
        begin
          LItems[LIndex].Weight := LPolicy.Value;
        end;
    end;
  end;
  Result := NyxFlowSizes(AAvailable, 0, LItems, LRanges);
end;

end.
