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

{ Typed display grammar calls immutable public builders only. Closed enum
  arguments retain their Pascal families; no saved expression invokes code. }
function TConfigurationReader.FormattingConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
  LRevision: Integer;
  LScale: Integer;

  function IntegerAt(AIndex: Integer): Integer;
  begin

    if (AIndex >= Length(LArgs)) or (LArgs[AIndex].Kind <> vkInteger) or
      not TryNyxStateInteger(LArgs[AIndex].Text, Result) then
    begin
      Fail('Display policy requires a portable Integer');
    end;
  end;

begin
  Result := Default(TValue);
  LArgs := nil;

  if At('(') then
  begin
    LArgs := Arguments;
  end;

  if SameText(AName, 'NyxFormatter') then
  begin

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkText) then
    begin
      Fail('NyxFormatter requires an exact text identity');
    end;
    Result.Kind := vkFormatterRef;
    Result.Text := NyxFormatter(LArgs[0].Text).Name;
    Exit;
  end;

  if Length(LArgs) <> 0 then
  begin
    Fail('Display policy starting points take no arguments');
  end;
  Result.Kind := vkValueFormat;
  Result.ValueFormat := NyxValueFormat;

  if SameText(AName, 'NyxFormatLocale') then
  begin
    Result.Kind := vkFormatLocale;
    Result.FormatLocale := NyxFormatLocale;
  end;
  while At('.') do
  begin
    Expect('.');

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected an immutable display policy method');
    end;
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := Arguments;

    if Result.Kind = vkFormatLocale then
    begin

      if (LMethod = 'named') and (Length(LArgs) = 1) and
        (LArgs[0].Kind = vkResourceLocale) then
      begin
        Result.FormatLocale := Result.FormatLocale.Named(NyxDefaultLocale);

        if LArgs[0].Text <> '' then
        begin
          Result.FormatLocale := Result.FormatLocale.Named(NyxLocale(LArgs[0].Text));
        end;
        Continue;
      end;

      if (LMethod = 'numberseparators') and (Length(LArgs) = 2) and
        (LArgs[0].Kind = vkText) and (LArgs[1].Kind = vkText) then
      begin
        Result.FormatLocale := Result.FormatLocale.NumberSeparators(LArgs[0].Text, LArgs[1].Text);
        Continue;
      end;
    end
    else
    begin

      if (LMethod = 'identified') and (Length(LArgs) = 2) and
        (LArgs[0].Kind = vkFormatterRef) then
      begin
        LRevision := IntegerAt(1);
        Result.ValueFormat := Result.ValueFormat.Identified(NyxFormatter(LArgs[0].Text), LRevision);
        Continue;
      end;

      if (LMethod = 'localized') and (Length(LArgs) = 1) and
        (LArgs[0].Kind = vkFormatLocale) then
      begin
        Result.ValueFormat := Result.ValueFormat.Localized(LArgs[0].FormatLocale);
        Continue;
      end;

      if (LMethod = 'decimal') and (Length(LArgs) in [1, 2]) then
      begin
        LScale := IntegerAt(0);

        if Length(LArgs) = 1 then
        begin
          Result.ValueFormat := Result.ValueFormat.Decimal(LScale);
          Continue;
        end;

        if LArgs[1].Kind = vkDecimalRounding then
        begin
          Result.ValueFormat := Result.ValueFormat.Decimal(LScale,
            TNyxDecimalRounding(LArgs[1].Ordinal));
          Continue;
        end;
      end;

      if (LMethod = 'calendardate') and (Length(LArgs) in [1, 2]) and
        (LArgs[0].Kind = vkFormatDateOrder) then
      begin

        if Length(LArgs) = 1 then
        begin
          Result.ValueFormat := Result.ValueFormat.CalendarDate(TNyxFormatDateOrder(LArgs[0].Ordinal));
          Continue;
        end;

        if LArgs[1].Kind = vkText then
        begin
          Result.ValueFormat := Result.ValueFormat.CalendarDate(
            TNyxFormatDateOrder(LArgs[0].Ordinal), LArgs[1].Text);
          Continue;
        end;
      end;

      if (LMethod = 'clocktime') and (Length(LArgs) = 1) and
        (LArgs[0].Kind = vkTimePrecision) then
      begin
        Result.ValueFormat := Result.ValueFormat.ClockTime(TNyxTimePrecision(LArgs[0].Ordinal));
        Continue;
      end;

      if (Length(LArgs) = 2) and (LArgs[0].Kind = vkText) and (LArgs[1].Kind = vkText) then
      begin

        if LMethod = 'booleanlabels' then
        begin
          Result.ValueFormat := Result.ValueFormat.BooleanLabels(LArgs[0].Text, LArgs[1].Text);
          Continue;
        end;

        if LMethod = 'textaffixes' then
        begin
          Result.ValueFormat := Result.ValueFormat.TextAffixes(LArgs[0].Text, LArgs[1].Text);
          Continue;
        end;
      end;
    end;
    Fail('Invalid typed display policy method ' + LMethod);
  end;
end;