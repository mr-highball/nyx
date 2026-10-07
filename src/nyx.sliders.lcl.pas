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

unit nyx.sliders.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  ComCtrls, nyx.text, nyx.sliders;

type
  { Real LCL trackbar with an owned portable value cache. Renderer Sync guards
    OnChange while publishing accepted scale/value. No node or renderer link is
    retained here; the ordinary renderer owns this control and its event slots. }
  TNyxLCLSlider = class(TTrackBar)
  private
    FValue: TNyxSliderValue;
  public
    { Complete admission happens before any physical field changes. }
    procedure Accept(const AScale: TNyxSliderScale; const AValue: TNyxText);
    { Return numeric wire value, never the internal ordinal thumb position. }
    function NumericValue: TNyxText;
  end;

implementation

procedure TNyxLCLSlider.Accept(const AScale: TNyxSliderScale;
  const AValue: TNyxText);
var
  LTickFrequency: Integer;
begin
  FValue.Accept(AScale, AValue);
  { Visible markers are independent of the one-position keyboard step.
    A million-interval policy must not request a million native tick marks. }
  LTickFrequency := (AScale.MaximumPosition + 9) div 10;

  if LTickFrequency < 1 then
  begin
    LTickFrequency := 1;
  end;
  { Grow marker spacing before increasing the range; shrink the range before
    decreasing spacing. Avoid a transient million-marker native allocation. }

  if LTickFrequency > Frequency then
  begin
    Frequency := LTickFrequency;
  end;
  Min := 0;
  Max := AScale.MaximumPosition;
  Frequency := LTickFrequency;
  Position := FValue.Position;
end;

function TNyxLCLSlider.NumericValue: TNyxText;
begin
  Result := FValue.ReadPosition(Position);
end;

end.
