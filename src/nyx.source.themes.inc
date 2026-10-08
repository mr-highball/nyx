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

{ Private declarative theme reader. Fluent roles retain their public Pascal
  argument families; raw color strings and floating point metrics cannot enter
  authoring by implicit conversion. Only the wire RGB constructor accepts text. }
function TConfigurationReader.ThemeTokens(const AName: TNyxText): TValue;
var
  LMethod: TNyxText;
  LArgs: TValues;
  LColor: TNyxThemeColor;
  LMetric: TNyxThemeMetric;
  LFound: Boolean;
begin
  Result := Default(TValue);
  Result.Kind := vkThemeTokens;
  Result.ThemeTokens := NyxThemeTokens;

  if SameText(AName, 'NyxThemePreset') then
  begin
    LArgs := Arguments(1);

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkThemePreset) then
    begin
      Fail('NyxThemePreset requires a closed light/dark choice');
    end;
    Result.ThemeTokens := NyxThemePreset(TNyxThemePreset(LArgs[0].Ordinal));
  end
  else if At('(') then
  begin
    LArgs := Arguments;

    if Length(LArgs) <> 0 then
    begin
      Fail('NyxThemeTokens takes no arguments');
    end;
  end;
  while At('.') do
  begin
    Expect('.');
    LMethod := FTokens[FCursor].Text;
    Inc(FCursor);
    LArgs := Arguments(1);

    if Length(LArgs) <> 1 then
    begin
      Fail('Theme roles require one typed argument');
    end;
    LFound := False;
    for LColor := Low(TNyxThemeColor) to High(TNyxThemeColor) do
    begin

      if SameText(LMethod, NyxThemeColorName(LColor)) then
      begin

        if LArgs[0].Kind <> vkRGBColor then
        begin
          Fail('Theme colors require TNyxRGBColor');
        end;
        Result.ThemeTokens := Result.ThemeTokens.Color(LColor, LArgs[0].RGBColor);
        LFound := True;
        Break;
      end;
    end;
    for LMetric := Low(TNyxThemeMetric) to High(TNyxThemeMetric) do
    begin

      if SameText(LMethod, NyxThemeMetricName(LMetric)) then
      begin

        if LArgs[0].Kind <> vkInteger then
        begin
          Fail('Theme metrics require Integer logical pixels');
        end;
        Result.ThemeTokens := Result.ThemeTokens.Metric(LMetric, StrToInt(LArgs[0].Text));
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      Fail('Unknown semantic theme role');
    end;
  end;
end;
