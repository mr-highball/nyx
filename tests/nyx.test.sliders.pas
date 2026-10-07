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

unit nyx.test.sliders;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.model;

{ Public typed augmentation of a semantic demo seed. This is a new local
  candidate; it does not claim the frozen HTTP service admits Number sliders.
  Caller owns the document; no store/view/interface is retained by this helper. }
procedure ConfigureNyxSliderCompanion(ADocument: TNyxDocument);
function NewNyxSliderCompanion: TNyxDocument;

implementation

uses
  nyx.controls, nyx.contract, nyx.state, nyx.types;

procedure ConfigureNyxSliderCompanion(ADocument: TNyxDocument);
begin
  ADocument.Find('fractional-slider').Contract.Value(NyxNumberDomain.Range(-1, 1));
  ADocument.Find('fractional-slider').Configure.Value(0.123456789012345)
    .SliderIntervals(400).AccessibleName('Audio gain').Height(48);
  ADocument.State.SetValue(NyxNumberState('gain'), 0.123456789012345);
  ADocument.Find('fractional-slider').Binds.Value(NyxNumberState('gain'));
  ADocument.Find('choice-slider').Contract.Value(NyxIntegerDomain.Choices([30, -12, 0]));
  ADocument.Find('choice-slider').Configure.Value(0).AccessibleName('Preset level').Height(48);
  ADocument.Find('number-choice-slider').Contract.Value(NyxNumberDomain.Choices([2.0, -0.5, 0.0, 0.125]));
  ADocument.Find('number-choice-slider').Configure.Value(0.0).AccessibleName('Blend preset').Height(48);
  ADocument.State.SetValue(NyxNumberState('blend'), 0.0);
  ADocument.Find('number-choice-slider').Binds.Value(NyxNumberState('blend'));
  ADocument.Validate;
end;

function NewNyxSliderCompanion: TNyxDocument;
var
  LPage: INyxColumn;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Mixing desk';
    LPage := NewNyxColumn('slider-review');
    LPage.Configure.Padding(20).Gap(12);
    LPage.Add(NewNyxHeading('slider-title').WithText('Mixing desk'));
    LPage.Add(NewNyxLabel('slider-help').WithText('Adjust gain smoothly or choose an exact preset.'));
    LPage.Add(NewNyxSlider('fractional-slider'));
    LPage.Add(NewNyxSlider('choice-slider'));
    LPage.Add(NewNyxSlider('number-choice-slider'));
    Result.AddPage(LPage);
    Result.State.SetValue(NyxIntegerState('level'), 0);
    Result.Find('choice-slider').Binds.Value(NyxIntegerState('level'));
    ConfigureNyxSliderCompanion(Result);
  except
    Result.Free;
    raise;
  end;
end;

end.
