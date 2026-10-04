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

{ Returns a fresh allocation array; inputs remain borrowed and unchanged.
  Negative available space/gaps/sizes/weights are clamped to zero at this adapter
  boundary. Fixed children and visible gaps are reserved before proportional
  allocation. Cumulative rounding assigns every available pixel, in source order,
  without overflowing Integer products. It does not implement text measurement,
  CSS minimum-content sizing, wrapping or platform widget metrics. }
function NyxFlowSizes(AAvailable, AGap: Integer;
  const AItems: TNyxFlowItems): TNyxFlowSizes;

implementation

uses
  Math;

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
  LFixed := LFixed + Double(Max(0, LVisible - 1)) * Max(0, AGap);
  LAvailable := Trunc(Max(0.0, Double(Max(0, AAvailable)) - LFixed));
  LThrough := 0;
  LAllocated := 0;
  for LIndex := 0 to High(AItems) do
  begin

    if AItems[LIndex].Visible and (AItems[LIndex].Weight > 0) then
    begin
      LThrough := LThrough + AItems[LIndex].Weight;
      LEnd := Trunc(Double(LAvailable) * (LThrough / LWeight));
      Result[LIndex] := LEnd - LAllocated;
      LAllocated := LEnd;
    end;
  end;
end;

end.
