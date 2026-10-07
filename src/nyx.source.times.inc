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

{ Private implementation of the declarative source reader. Tagged immutable
  values invoke the same public Pascal overloads as handwritten source. This
  include owns no document, renderer, interface, callback or mutable collection. }
function TConfigurationReader.ClockValue(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LParts: array[0..3] of Integer;
  LIndex: Integer;
begin
  Result := Default(TValue);
  Result.Kind := vkClockTime;
  Result.ClockTime := NyxNoTime;

  if SameText(AName, 'NyxTime') then
  begin
    LArgs := Arguments(4);

    if (Length(LArgs) < 2) or (Length(LArgs) > 4) then
    begin
      Fail('NyxTime requires Integer hour/minute and optional second/millisecond');
    end;
    for LIndex := 0 to High(LParts) do
    begin
      LParts[LIndex] := 0;
    end;
    for LIndex := 0 to High(LArgs) do
    begin

      if LArgs[LIndex].Kind <> vkInteger then
      begin
        Fail('NyxTime parts require Integer arguments');
      end;
      TryNyxStateInteger(LArgs[LIndex].Text, LParts[LIndex]);
    end;
    Result.ClockTime := NyxTime(LParts[0], LParts[1], LParts[2], LParts[3]);
  end
  else if At('(') then
  begin
    LArgs := Arguments;

    if Length(LArgs) <> 0 then
    begin
      Fail('NyxNoTime takes no arguments');
    end;
  end;
  while At('.') do
  begin
    Expect('.');
    Expect('WithPrecision');
    LArgs := Arguments;

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkTimePrecision) then
    begin
      Fail('WithPrecision requires TNyxTimePrecision');
    end;
    Result.ClockTime := Result.ClockTime.WithPrecision(TNyxTimePrecision(LArgs[0].Ordinal));
  end;
end;

function TConfigurationReader.ClockDomain: TValue;
var
  LMethod: TNyxText;
  LArgs: TValues;
  LTimes: array of TNyxClockTime;
  LIndex: Integer;
  LStep: Integer;
begin
  Result := Default(TValue);
  Result.Kind := vkClockDomain;
  Result.Ordinal := Ord(nskText);
  Result.TimeDomain := NyxTimeDomain;

  if At('(') then
  begin
    LArgs := Arguments;

    if Length(LArgs) <> 0 then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkValueDomain) then
      begin
        Fail('NyxTimeDomain takes no arguments or one explicit domain Definition');
      end;
      Result.TimeDomain := NyxTimeDomain(LArgs[0].ValueDomain);
    end;
  end;
  while At('.') do
  begin
    Expect('.');

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected a typed clock domain method');
    end;
    LMethod := FTokens[FCursor].Text;
    Inc(FCursor);

    if SameText(LMethod, 'Choices') then
    begin
      Expect('(');
      LArgs := ArrayArguments(NyxMaximumDomainChoices);
      Expect(')');
      SetLength(LTimes, Length(LArgs));
      for LIndex := 0 to High(LArgs) do
      begin

        if LArgs[LIndex].Kind <> vkClockTime then
        begin
          Fail('Clock Choices requires homogeneous typed clock values');
        end;
        LTimes[LIndex] := LArgs[LIndex].ClockTime;
      end;
      Result.TimeDomain := Result.TimeDomain.Choices(LTimes);
    end
    else if SameText(LMethod, 'AnyStep') or SameText(LMethod, 'Definition') then
    begin

      if At('(') then
      begin
        LArgs := Arguments;

        if Length(LArgs) <> 0 then
        begin
          Fail('AnyStep/Definition takes no arguments');
        end;
      end;

      if SameText(LMethod, 'Definition') then
      begin
        Result.ValueDomain := Result.TimeDomain.Definition;
        Result.Kind := vkValueDomain;
        Exit;
      end;
      Result.TimeDomain := Result.TimeDomain.AnyStep;
    end
    else if SameText(LMethod, 'StepMilliseconds') or SameText(LMethod, 'StepSeconds') then
    begin
      LArgs := Arguments;

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkInteger) then
      begin
        Fail('Clock step requires one Integer argument');
      end;
      TryNyxStateInteger(LArgs[0].Text, LStep);

      if SameText(LMethod, 'StepSeconds') then
      begin
        Result.TimeDomain := Result.TimeDomain.StepSeconds(LStep);
      end
      else
      begin
        Result.TimeDomain := Result.TimeDomain.StepMilliseconds(LStep);
      end;
    end
    else if SameText(LMethod, 'Range') or SameText(LMethod, 'Minimum') or
      SameText(LMethod, 'Maximum') then
    begin
      LArgs := Arguments;

      if ((SameText(LMethod, 'Range')) and (Length(LArgs) <> 2)) or
        ((not SameText(LMethod, 'Range')) and (Length(LArgs) <> 1)) then
      begin
        Fail('Clock bounds require their declared number of typed clock values');
      end;
      for LIndex := 0 to High(LArgs) do
      begin

        if LArgs[LIndex].Kind <> vkClockTime then
        begin
          Fail('Clock bounds require typed clock values');
        end;
      end;

      if SameText(LMethod, 'Range') then
      begin
        Result.TimeDomain := Result.TimeDomain.Range(LArgs[0].ClockTime, LArgs[1].ClockTime);
      end
      else if SameText(LMethod, 'Minimum') then
      begin
        Result.TimeDomain := Result.TimeDomain.Minimum(LArgs[0].ClockTime);
      end
      else
      begin
        Result.TimeDomain := Result.TimeDomain.Maximum(LArgs[0].ClockTime);
      end;
    end
    else
    begin
      Fail('Unsupported clock domain method ' + LMethod);
    end;
  end;
end;
