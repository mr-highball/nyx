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
unit nyx.collections.window;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils;

type
  { Copied logical row interval [First, AfterLast). Spacer sizes are logical
    pixels, independent of DOM/LCL, selection, source ownership and frame timing.
    Empty sources produce an empty interval. No mutable arrays escape. }
  TNyxCollectionRowWindow = record
  private
    FFirst: Integer;
    FAfterLast: Integer;
    FBeforePixels: Double;
    FAfterPixels: Double;
    function GetCount: Integer;
  public
    property First: Integer read FFirst;
    property AfterLast: Integer read FAfterLast;
    property Count: Integer read GetCount;
    property BeforePixels: Double read FBeforePixels;
    property AfterPixels: Double read FAfterPixels;
  end;

  { Exclusively owned measured-row geometry. Unknown rows use an estimate; real
    positive measured heights replace that estimate in O(log Count). Prefix
    offsets and offset-to-row lookup also cost O(log Count). Reset is O(Count).
    This owns numerical metadata only; it neither reads source values nor retains
    a document, control or callback. Adapters reset when logical order changes.
    Heights/offsets must be finite; heights positive, offsets/extents nonnegative.
    Invalid input refuses before mutation. Counts exclude a table's header. }
  TNyxCollectionRowGeometry = class
  private
    FHeights: array of Double;
    FTree: array of Double;
    function GetCount: Integer;
    function GetTotal: Double;
    procedure CheckIndex(AIndex: Integer; AAllowEnd: Boolean);
  public
    constructor Create(ACount: Integer; AEstimate: Double);
    { Publishes detached numerical arrays only after all arguments are admitted.
      Failed admission preserves previous geometry. No user text is stored. }
    procedure Reset(ACount: Integer; AEstimate: Double);
    { True when the admitted height changed. Same-height measurement is a no-op.
      The total must stay finite before any prefix entry is changed. }
    function Measure(AIndex: Integer; AHeight: Double): Boolean;
    function HeightAt(AIndex: Integer): Double;
    { Start offset of a zero-based row; Count returns the total end offset. }
    function OffsetAt(AIndex: Integer): Double;
    { Zero-based row containing offset; Total or beyond returns Count. }
    function IndexAt(AOffset: Double): Integer;
    { Exact viewport intersection plus independently bounded overscan rows.
      A zero extent realizes no rows; a caller chooses its own hidden-host
      prefetch policy. Extents beyond the source are clipped without overflow. }
    function Window(AOffset, AExtent: Double;
      AOverscan: Integer): TNyxCollectionRowWindow;
    property Count: Integer read GetCount;
    property Total: Double read GetTotal;
  end;

implementation

uses
  Math;

procedure RequireFinite(AValue: Double; APositive: Boolean);
begin

  if IsNan(AValue) or IsInfinite(AValue) or (AValue < 0) or
    (APositive and (AValue = 0)) then
  begin
    raise EArgumentException.Create('Row geometry requires finite admitted pixels');
  end;
end;

function TNyxCollectionRowWindow.GetCount: Integer;
begin
  Result := FAfterLast - FFirst;
end;

constructor TNyxCollectionRowGeometry.Create(ACount: Integer; AEstimate: Double);
begin
  inherited Create;
  Reset(ACount, AEstimate);
end;

procedure TNyxCollectionRowGeometry.Reset(ACount: Integer; AEstimate: Double);
var
  LHeights: array of Double;
  LTree: array of Double;
  LIndex: Integer;
begin

  if (ACount < 0) or (ACount = High(Integer)) then
  begin
    raise EArgumentException.Create('Row geometry count is outside the signed range');
  end;
  RequireFinite(AEstimate, True);
  RequireFinite(ACount * AEstimate, False);
  SetLength(LHeights, ACount);
  SetLength(LTree, ACount + 1);
  LTree[0] := 0;
  for LIndex := 1 to ACount do
  begin
    LHeights[LIndex - 1] := AEstimate;
    LTree[LIndex] := (LIndex and -LIndex) * AEstimate;
  end;
  FHeights := LHeights;
  FTree := LTree;
end;

function TNyxCollectionRowGeometry.GetCount: Integer;
begin
  Result := Length(FHeights);
end;

function TNyxCollectionRowGeometry.GetTotal: Double;
begin
  Result := OffsetAt(Count);
end;

procedure TNyxCollectionRowGeometry.CheckIndex(AIndex: Integer; AAllowEnd: Boolean);
begin

  if (AIndex < 0) or (AIndex > Count) or
    ((AIndex = Count) and not AAllowEnd) then
  begin
    raise EArgumentOutOfRangeException.Create('Row geometry index is outside the source');
  end;
end;

function TNyxCollectionRowGeometry.Measure(AIndex: Integer; AHeight: Double): Boolean;
var
  LIndex: Integer;
  LDelta: Double;
begin
  CheckIndex(AIndex, False);
  RequireFinite(AHeight, True);
  Result := FHeights[AIndex] <> AHeight;

  if not Result then
  begin
    Exit;
  end;
  LDelta := AHeight - FHeights[AIndex];
  RequireFinite(Total + LDelta, False);
  FHeights[AIndex] := AHeight;
  LIndex := AIndex + 1;
  while LIndex <= Count do
  begin
    FTree[LIndex] := FTree[LIndex] + LDelta;

    if (LIndex and -LIndex) > Count - LIndex then
    begin
      Break;
    end;
    LIndex := LIndex + (LIndex and -LIndex);
  end;
end;

function TNyxCollectionRowGeometry.HeightAt(AIndex: Integer): Double;
begin
  CheckIndex(AIndex, False);
  Result := FHeights[AIndex];
end;

function TNyxCollectionRowGeometry.OffsetAt(AIndex: Integer): Double;
begin
  CheckIndex(AIndex, True);
  Result := 0;
  while AIndex > 0 do
  begin
    Result := Result + FTree[AIndex];
    AIndex := AIndex - (AIndex and -AIndex);
  end;
end;

function TNyxCollectionRowGeometry.IndexAt(AOffset: Double): Integer;
var
  LBit: Integer;
  LNext: Integer;
  LPrefix: Double;
begin
  RequireFinite(AOffset, False);
  Result := 0;
  LPrefix := 0;
  LBit := 1;
  while LBit <= Count div 2 do
  begin
    LBit := LBit * 2;
  end;
  while LBit > 0 do
  begin

    if LBit <= Count - Result then
    begin
      LNext := Result + LBit;

      if LPrefix + FTree[LNext] <= AOffset then
      begin
        Result := LNext;
        LPrefix := LPrefix + FTree[LNext];
      end;
    end;
    LBit := LBit div 2;
  end;
end;

function TNyxCollectionRowGeometry.Window(AOffset, AExtent: Double;
  AOverscan: Integer): TNyxCollectionRowWindow;
var
  LEnd: Double;
  LTotal: Double;
begin
  RequireFinite(AOffset, False);
  RequireFinite(AExtent, False);

  if AOverscan < 0 then
  begin
    raise EArgumentException.Create('Row overscan cannot be negative');
  end;
  Result := Default(TNyxCollectionRowWindow);
  LTotal := Total;
  Result.FFirst := IndexAt(AOffset);
  Result.FAfterLast := Result.FFirst;

  if (AExtent > 0) and (AOffset < LTotal) then
  begin
    LEnd := LTotal;

    if AExtent < LTotal - AOffset then
    begin
      LEnd := AOffset + AExtent;
    end;
    Result.FAfterLast := IndexAt(LEnd);

    if (Result.FAfterLast < Count) and (OffsetAt(Result.FAfterLast) < LEnd) then
    begin
      Inc(Result.FAfterLast);
    end;

    if AOverscan >= Result.FFirst then
    begin
      Result.FFirst := 0;
    end
    else
    begin
      Result.FFirst := Result.FFirst - AOverscan;
    end;

    if AOverscan >= Count - Result.FAfterLast then
    begin
      Result.FAfterLast := Count;
    end
    else
    begin
      Result.FAfterLast := Result.FAfterLast + AOverscan;
    end;
  end;
  Result.FBeforePixels := OffsetAt(Result.FFirst);
  Result.FAfterPixels := LTotal - OffsetAt(Result.FAfterLast);
end;

end.
