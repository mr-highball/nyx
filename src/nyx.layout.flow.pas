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
unit nyx.layout.flow;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.types, nyx.layout.constraints;

type
  { Adapter-independent main-axis inputs, in logical pixels. Hidden entries
    keep their identity/index but consume neither space, weight nor a gap.
    Positive Weight replaces NaturalSize on a definite main axis. Zero weight
    preserves that fixed/natural size, including deliberately overflowing sizes. }
  TNyxFlowItem = record
    Visible: Boolean;
    NaturalSize: Integer;
    Weight: Integer;
  end;
  TNyxFlowItems = array of TNyxFlowItem;
  TNyxFlowSizes = array of Integer;
  { Inclusive source-index windows. Hidden entries can occur inside a window;
    no window starts/ends on one. Empty visible input returns no lines. }
  TNyxFlowLine = record
    First: Integer;
    Last: Integer;
  end;
  TNyxFlowLines = array of TNyxFlowLine;

{ Returns a fresh allocation array; inputs remain borrowed and unchanged.
  Negative available space/gaps/sizes/weights are clamped to zero at this adapter
  boundary. Fixed children and visible gaps are reserved before proportional
  allocation. Cumulative rounding assigns every available pixel, in source order,
  without overflowing Integer products. It does not implement text measurement,
  CSS minimum-content sizing, wrapping or platform widget metrics. }
function NyxFlowSizes(AAvailable, AGap: Integer;
  const AItems: TNyxFlowItems): TNyxFlowSizes; overload;

{ Constrained zero-basis weight allocation. Bounds are parallel copied values;
  old callers need not initialize additional fields in TNyxFlowItem. Fixed
  extents are clamped first; weighted min/max violations freeze according to
  their total adjustment, then remaining siblings share remaining space.
  Explicit minima may overflow; maxima may leave space for justification.
  A count mismatch or invalid range refuses before producing allocations. }
function NyxFlowSizes(AAvailable, AGap: Integer; const AItems: TNyxFlowItems;
  const ARanges: TNyxSizeRanges): TNyxFlowSizes; overload;

{ Build source-ordered row lines before allocating weights. A weighted item has
  zero basis, matching Nyx's positive-weight contract. An oversized first item
  occupies one line; neither overflow nor hidden entries create empty lines. }
function NyxFlowLines(AAvailable, AGap: Integer; const AItems: TNyxFlowItems;
  AWrap: Boolean): TNyxFlowLines; overload;
{ Weighted hypothetical sizes include their explicit minimum before wrapping;
  each resulting line subsequently redistributes weights within its bounds. }
function NyxFlowLines(AAvailable, AGap: Integer; const AItems: TNyxFlowItems;
  const ARanges: TNyxSizeRanges; AWrap: Boolean): TNyxFlowLines; overload;
{ Fresh logical positions for one line. Extra spacing uses cumulative rounding;
  gaps are minima. Overflow retains start alignment, keeping the leading content
  reachable. Hidden indices stay zero. Inputs and prior results remain unchanged.
  A size-array length mismatch raises EArgumentException before allocation. }
function NyxFlowPositions(AAvailable, AGap: Integer; const AItems: TNyxFlowItems;
  const ASizes: TNyxFlowSizes; AJustification: TNyxJustification): TNyxFlowSizes;

implementation

uses
  Math, SysUtils;

{ Assignment conversion is portable to FPC 3.2 and pas2js. FPC 3.2 does not
  accept the newer explicit Integer-to-Double cast used by some compilers. }
function FlowReal(AValue: Integer): Double;
begin
  Result := AValue;
end;

function NyxFlowSizes(AAvailable, AGap: Integer;
  const AItems: TNyxFlowItems): TNyxFlowSizes;
var
  LIndex: Integer;
  LVisible: Integer;
  LFixed: Double;
  LWeight: Double;
  LThrough: Double;
  LAvailable: Integer;
  LAllocated: Integer;
  LEnd: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AItems));
  LVisible := 0;
  LFixed := 0;
  LWeight := 0;
  for LIndex := 0 to High(AItems) do
  begin
    Result[LIndex] := 0;

    if AItems[LIndex].Visible then
    begin
      Inc(LVisible);

      if AItems[LIndex].Weight > 0 then
      begin
        LWeight := LWeight + AItems[LIndex].Weight;
      end
      else
      begin
        Result[LIndex] := Max(0, AItems[LIndex].NaturalSize);
        LFixed := LFixed + Result[LIndex];
      end;
    end;
  end;
  LFixed := LFixed + FlowReal(Max(0, LVisible - 1)) * Max(0, AGap);
  LAvailable := Trunc(Max(0.0, FlowReal(Max(0, AAvailable)) - LFixed));
  LThrough := 0;
  LAllocated := 0;
  for LIndex := 0 to High(AItems) do
  begin

    if AItems[LIndex].Visible and (AItems[LIndex].Weight > 0) then
    begin
      LThrough := LThrough + AItems[LIndex].Weight;
      LEnd := Trunc(FlowReal(LAvailable) * (LThrough / LWeight));
      Result[LIndex] := LEnd - LAllocated;
      LAllocated := LEnd;
    end;
  end;
end;

function NyxFlowSizes(AAvailable, AGap: Integer; const AItems: TNyxFlowItems;
  const ARanges: TNyxSizeRanges): TNyxFlowSizes;
var
  LFrozen: array of Boolean;
  LTargets, LViolations: array of Double;
  LIndex, LVisible, LRemaining: Integer;
  LFixed, LWeight, LAvailable, LProposed, LViolation: Double;
  LThrough: Double;
  LAllocated, LEnd: Integer;
  LConstrained: Boolean;
begin

  if Length(ARanges) <> Length(AItems) then
  begin
    raise EArgumentException.Create('Flow bounds require one range per item');
  end;
  LConstrained := False;
  for LIndex := 0 to High(ARanges) do
  begin
    ARanges[LIndex].Validate;
    LConstrained := LConstrained or ARanges[LIndex].HasMinimum or
      ARanges[LIndex].HasMaximum;
  end;

  if not LConstrained then
  begin
    Exit(NyxFlowSizes(AAvailable, AGap, AItems));
  end;
  Result := nil;
  SetLength(Result, Length(AItems));
  SetLength(LFrozen, Length(AItems));
  SetLength(LTargets, Length(AItems));
  SetLength(LViolations, Length(AItems));
  LVisible := 0;
  LRemaining := 0;
  LFixed := 0;
  for LIndex := 0 to High(AItems) do
  begin
    LFrozen[LIndex] := not AItems[LIndex].Visible or (AItems[LIndex].Weight <= 0);
    LTargets[LIndex] := 0;
    Result[LIndex] := 0;

    if AItems[LIndex].Visible then
    begin
      Inc(LVisible);

      if LFrozen[LIndex] then
      begin
        Result[LIndex] := ARanges[LIndex].Clamp(AItems[LIndex].NaturalSize);
        LFixed := LFixed + Result[LIndex];
      end
      else
      begin
        Inc(LRemaining);
      end;
    end;
  end;
  LFixed := LFixed + FlowReal(Max(0, LVisible - 1)) * Max(0, AGap);
  while LRemaining > 0 do
  begin
    LWeight := 0;
    LAvailable := FlowReal(Max(0, AAvailable)) - LFixed;
    for LIndex := 0 to High(AItems) do
    begin

      if not LFrozen[LIndex] then
      begin
        LWeight := LWeight + AItems[LIndex].Weight;
      end;
    end;
    LAvailable := Max(0.0, LAvailable);
    LViolation := 0;
    for LIndex := 0 to High(AItems) do
    begin

      if LFrozen[LIndex] then
      begin
        Continue;
      end;
      LProposed := LAvailable * (AItems[LIndex].Weight / LWeight);
      LTargets[LIndex] := LProposed;

      if ARanges[LIndex].HasMaximum then
      begin
        LTargets[LIndex] := Min(LTargets[LIndex], ARanges[LIndex].MaximumValue);
      end;

      if ARanges[LIndex].HasMinimum then
      begin
        LTargets[LIndex] := Max(LTargets[LIndex], ARanges[LIndex].MinimumValue);
      end;
      LViolations[LIndex] := LTargets[LIndex] - LProposed;
      LViolation := LViolation + LViolations[LIndex];
    end;
    { Freeze the appropriate side, rather than both at once. For 100 pixels,
      min(60) and max(20) first propose 50/50; freezing both would leave unused
      space. The negative total freezes max(20), then its sibling receives 80.
      This is the min/max freezing rule in CSS Flexbox section 9.7, restricted
      to Nyx's integer positive grow weights and zero main-axis basis. }

    if Abs(LViolation) < 0.0000001 then
    begin
      Break;
    end;
    for LIndex := 0 to High(AItems) do
    begin

      if not LFrozen[LIndex] and
        (((LViolation > 0) and (LViolations[LIndex] > 0)) or
        ((LViolation < 0) and (LViolations[LIndex] < 0))) then
      begin
        LFrozen[LIndex] := True;
        Result[LIndex] := ARanges[LIndex].Clamp(Round(LTargets[LIndex]));
        LFixed := LFixed + Result[LIndex];
        Dec(LRemaining);
      end;
    end;
  end;
  LThrough := 0;
  LAllocated := 0;
  for LIndex := 0 to High(AItems) do
  begin

    if not LFrozen[LIndex] then
    begin
      LThrough := LThrough + LTargets[LIndex];
      LEnd := Trunc(Min(FlowReal(High(Integer)), LThrough + 0.0000001));
      Result[LIndex] := ARanges[LIndex].Clamp(LEnd - LAllocated);
      LAllocated := LEnd;
    end;
  end;
end;

function NyxFlowLines(AAvailable, AGap: Integer; const AItems: TNyxFlowItems;
  const ARanges: TNyxSizeRanges; AWrap: Boolean): TNyxFlowLines;
var
  LHypothetical: TNyxFlowItems;
  LIndex: Integer;
begin

  if Length(ARanges) <> Length(AItems) then
  begin
    raise EArgumentException.Create('Flow lines require one range per item');
  end;
  SetLength(LHypothetical, Length(AItems));
  for LIndex := 0 to High(AItems) do
  begin
    ARanges[LIndex].Validate;
    LHypothetical[LIndex] := AItems[LIndex];
    LHypothetical[LIndex].Weight := 0;

    if AItems[LIndex].Weight > 0 then
    begin
      LHypothetical[LIndex].NaturalSize := ARanges[LIndex].Clamp(0);
    end
    else
    begin
      LHypothetical[LIndex].NaturalSize := ARanges[LIndex].Clamp(AItems[LIndex].NaturalSize);
    end;
  end;
  Result := NyxFlowLines(AAvailable, AGap, LHypothetical, AWrap);
end;

function NyxFlowLines(AAvailable, AGap: Integer; const AItems: TNyxFlowItems;
  AWrap: Boolean): TNyxFlowLines;
var
  LIndex: Integer;
  LLine: Integer;
  LSize: Integer;
  LUsed: Double;
begin
  Result := nil;
  LUsed := 0;
  LLine := -1;
  for LIndex := 0 to High(AItems) do
  begin

    if not AItems[LIndex].Visible then
    begin
      Continue;
    end;
    LSize := Max(0, AItems[LIndex].NaturalSize);

    if AItems[LIndex].Weight > 0 then
    begin
      LSize := 0;
    end;

    if (LLine < 0) or (AWrap and
      (LUsed + Max(0, AGap) + LSize > Max(0, AAvailable))) then
    begin
      Inc(LLine);
      SetLength(Result, LLine + 1);
      Result[LLine].First := LIndex;
      LUsed := 0;
    end
    else
    begin
      LUsed := LUsed + Max(0, AGap);
    end;
    Result[LLine].Last := LIndex;
    LUsed := LUsed + LSize;
  end;
end;

function NyxFlowPositions(AAvailable, AGap: Integer; const AItems: TNyxFlowItems;
  const ASizes: TNyxFlowSizes; AJustification: TNyxJustification): TNyxFlowSizes;
var
  LIndex: Integer;
  LCount: Integer;
  LPosition: Integer;
  LUsed: Double;
  LFree: Double;
  LOffset: Double;
  LSpacing: Double;
  LThrough: Double;
begin
  Result := nil;

  if Length(ASizes) <> Length(AItems) then
  begin
    raise EArgumentException.Create('Flow positions require one size per item');
  end;
  SetLength(Result, Length(AItems));
  LCount := 0;
  LUsed := 0;
  for LIndex := 0 to High(AItems) do
  begin
    Result[LIndex] := 0;

    if AItems[LIndex].Visible then
    begin
      Inc(LCount);
      LUsed := LUsed + Max(0, ASizes[LIndex]);
    end;
  end;
  LFree := Max(0.0, AAvailable - LUsed - FlowReal(Max(0, LCount - 1)) * Max(0, AGap));
  LOffset := 0;
  LSpacing := 0;
  case AJustification of
    njStart:
      begin
        { Defaults above already place the line at its leading edge. }
      end;
    njCenter: LOffset := LFree / 2;
    njEnd: LOffset := LFree;
    njSpaceBetween:
      begin

        if LCount > 1 then
        begin
          LSpacing := LFree / (LCount - 1);
        end;
      end;
    njSpaceAround:
      begin

        if LCount > 0 then
        begin
          LSpacing := LFree / LCount;
          LOffset := LSpacing / 2;
        end;
      end;
    njSpaceEvenly:
      begin
        LSpacing := LFree / (LCount + 1);
        LOffset := LSpacing;
      end;
  end;
  LPosition := 0;
  LThrough := 0;
  for LIndex := 0 to High(AItems) do
  begin

    if AItems[LIndex].Visible then
    begin
      { Bound conversion at the widgetset's signed Integer limit. Double keeps
        intermediate sums defined even for deliberately oversized input. }
      Result[LIndex] := Trunc(Min(FlowReal(High(Integer)), LOffset + LThrough +
        FlowReal(LPosition) * (Max(0, AGap) + LSpacing)));
      LThrough := LThrough + Max(0, ASizes[LIndex]);
      Inc(LPosition);
    end;
  end;
end;

end.
