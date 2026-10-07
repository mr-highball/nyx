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

unit nyx.times;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text;

const
  NyxMillisecondsPerDay = 86400000;

type
  { Invalid clock parts, wire spelling and lossy precision are argument failures. }
  ENyxTimeValue = class(EArgumentException);

  { Precision describes retained spelling, independently of the clock reading.
    Thus 08:30, 08:30:00 and 08:30:00.000 compare equal without losing their
    distinct wire text. Closed choices also give generated Pascal a typed way
    to preserve an explicitly authored zero second/fraction. }
  TNyxTimePrecision = (ntpMinute, ntpSecond, ntpTenth, ntpHundredth, ntpMillisecond);

  { Immutable local time of day. Integer milliseconds avoid floating-point,
    timezone, locale and timestamp assumptions. Default records mean no time;
    defined midnight is a different value. Copies contain no borrowed objects
    or mutable storage. Valid hours are 0..23; leap seconds are not clock values.
    Parts/precision/comparison require Defined; ToText returns empty for no time. }
  TNyxClockTime = record
  private
    FDefined: Boolean;
    FMilliseconds: Integer;
    FPrecision: TNyxTimePrecision;
    procedure RequireDefined;
    function GetHour: Integer;
    function GetMinute: Integer;
    function GetSecond: Integer;
    function GetMillisecond: Integer;
    function GetMilliseconds: Integer;
    function GetPrecision: TNyxTimePrecision;
  public
    { Parts are validated before constructing the result. Zero seconds/fraction
      use minute precision; nonzero seconds use second precision; nonzero
      milliseconds use three fractional digits. WithPrecision changes spelling. }
    class function FromParts(AHour, AMinute: Integer; ASecond: Integer = 0;
      AMillisecond: Integer = 0): TNyxClockTime; static;
    { Persistence/control boundary: empty, HH:MM, HH:MM:SS, or HH:MM:SS.f with
      one to three ASCII fractional digits. Preserve spelling exactly; do not
      trim, infer an AM/PM convention or admit dates/timezone offsets. }
    class function FromText(const AText: TNyxText): TNyxClockTime; static;
    function ToText: TNyxText;
    { Return an independent value. Refuse a precision that would discard any
      nonzero part; changing an empty value or an invalid enum also refuses. }
    function WithPrecision(APrecision: TNyxTimePrecision): TNyxClockTime;
    { Compare clock readings only, returning -1, 0 or 1. Spelling is independent. }
    function Compare(const AOther: TNyxClockTime): Integer;
    property Defined: Boolean read FDefined;
    property Hour: Integer read GetHour;
    property Minute: Integer read GetMinute;
    property Second: Integer read GetSecond;
    property Millisecond: Integer read GetMillisecond;
    property MillisecondsSinceMidnight: Integer read GetMilliseconds;
    property Precision: TNyxTimePrecision read GetPrecision;
  end;

{ Normal authoring uses typed integer parts. NoTime never means now or midnight. }
function NyxTime(AHour, AMinute: Integer; ASecond: Integer = 0;
  AMillisecond: Integer = 0): TNyxClockTime;
function NyxNoTime: TNyxClockTime;
{ Malformed input returns False and an empty result, without partial values or
  exceptions. Empty succeeds with no time; non-ASCII digits are never accepted. }
function TryNyxTime(const AText: TNyxText; out ATime: TNyxClockTime): Boolean;
{ Closed source vocabulary shared by generation and declarative admission.
  Invalid enum ordinals refuse before indexing the vocabulary. }
function NyxTimePrecisionPascal(APrecision: TNyxTimePrecision): TNyxText;

implementation

function NyxTimePrecisionPascal(APrecision: TNyxTimePrecision): TNyxText;
const
  CSymbols: array[TNyxTimePrecision] of TNyxText =
    ('ntpMinute', 'ntpSecond', 'ntpTenth', 'ntpHundredth', 'ntpMillisecond');
begin

  if (Ord(APrecision) < Ord(Low(TNyxTimePrecision))) or
    (Ord(APrecision) > Ord(High(TNyxTimePrecision))) then
  begin
    raise ENyxTimeValue.Create('Unknown clock precision');
  end;
  Result := CSymbols[APrecision];
end;

function NyxNoTime: TNyxClockTime;
begin
  Result := Default(TNyxClockTime);
end;

function NyxTime(AHour, AMinute: Integer; ASecond: Integer;
  AMillisecond: Integer): TNyxClockTime;
begin
  Result := TNyxClockTime.FromParts(AHour, AMinute, ASecond, AMillisecond);
end;

class function TNyxClockTime.FromParts(AHour, AMinute: Integer; ASecond: Integer;
  AMillisecond: Integer): TNyxClockTime;
begin

  if (AHour < 0) or (AHour > 23) or (AMinute < 0) or (AMinute > 59) or
    (ASecond < 0) or (ASecond > 59) or (AMillisecond < 0) or (AMillisecond > 999) then
  begin
    raise ENyxTimeValue.Create('Clock parts require hour 0..23, minute/second 0..59, millisecond 0..999');
  end;
  Result := NyxNoTime;
  Result.FDefined := True;
  Result.FMilliseconds := ((AHour * 60 + AMinute) * 60 + ASecond) * 1000 + AMillisecond;
  Result.FPrecision := ntpMinute;

  if ASecond <> 0 then
  begin
    Result.FPrecision := ntpSecond;
  end;

  if AMillisecond <> 0 then
  begin
    Result.FPrecision := ntpMillisecond;
  end;
end;

function TryNyxTime(const AText: TNyxText; out ATime: TNyxClockTime): Boolean;
var
  LHour: Integer;
  LMinute: Integer;
  LSecond: Integer;
  LMillisecond: Integer;
  LFractionDigits: Integer;
  LPrecision: TNyxTimePrecision;

  function Digits(AStart, ACount: Integer; out AValue: Integer): Boolean;
  var
    LIndex: Integer;
  begin
    AValue := 0;
    for LIndex := AStart to AStart + ACount - 1 do
    begin

      if (AText[LIndex] < '0') or (AText[LIndex] > '9') then
      begin
        Exit(False);
      end;
      AValue := AValue * 10 + Ord(AText[LIndex]) - Ord('0');
    end;
    Result := True;
  end;

begin
  ATime := NyxNoTime;
  Result := False;

  if AText = '' then
  begin
    Exit(True);
  end;

  if not (Length(AText) in [5, 8, 10, 11, 12]) or (AText[3] <> ':') then
  begin
    Exit;
  end;

  if not Digits(1, 2, LHour) or not Digits(4, 2, LMinute) or
    (LHour > 23) or (LMinute > 59) then
  begin
    Exit;
  end;
  LSecond := 0;
  LMillisecond := 0;
  LPrecision := ntpMinute;

  if Length(AText) >= 8 then
  begin

    if (AText[6] <> ':') or not Digits(7, 2, LSecond) or (LSecond > 59) then
    begin
      Exit;
    end;
    LPrecision := ntpSecond;
  end;

  if Length(AText) > 8 then
  begin
    LFractionDigits := Length(AText) - 9;

    if (AText[9] <> '.') or not Digits(10, LFractionDigits, LMillisecond) then
    begin
      Exit;
    end;
    LPrecision := TNyxTimePrecision(Ord(ntpTenth) + LFractionDigits - 1);

    if LFractionDigits = 1 then
    begin
      LMillisecond := LMillisecond * 100;
    end
    else if LFractionDigits = 2 then
    begin
      LMillisecond := LMillisecond * 10;
    end;
  end;
  ATime := NyxTime(LHour, LMinute, LSecond, LMillisecond).WithPrecision(LPrecision);
  Result := True;
end;

class function TNyxClockTime.FromText(const AText: TNyxText): TNyxClockTime;
begin

  if not TryNyxTime(AText, Result) then
  begin
    raise ENyxTimeValue.Create('Clock wire requires HH:MM with optional seconds and 1..3 fractional digits');
  end;
end;

procedure TNyxClockTime.RequireDefined;
begin

  if not FDefined then
  begin
    raise ENyxTimeValue.Create('No clock time is defined');
  end;
end;

function TNyxClockTime.GetHour: Integer;
begin
  RequireDefined;
  Result := FMilliseconds div 3600000;
end;

function TNyxClockTime.GetMinute: Integer;
begin
  RequireDefined;
  Result := FMilliseconds div 60000 mod 60;
end;

function TNyxClockTime.GetSecond: Integer;
begin
  RequireDefined;
  Result := FMilliseconds div 1000 mod 60;
end;

function TNyxClockTime.GetMillisecond: Integer;
begin
  RequireDefined;
  Result := FMilliseconds mod 1000;
end;

function TNyxClockTime.GetMilliseconds: Integer;
begin
  RequireDefined;
  Result := FMilliseconds;
end;

function TNyxClockTime.GetPrecision: TNyxTimePrecision;
begin
  RequireDefined;
  Result := FPrecision;
end;

function TNyxClockTime.WithPrecision(APrecision: TNyxTimePrecision): TNyxClockTime;
var
  LDivisor: Integer;
begin
  RequireDefined;
  { Interpret the ordinal at this checked boundary so an unchecked external
    enum cast also refuses, without an unreachable enum-case fallback. }
  case Ord(APrecision) of
    Ord(ntpMinute):
      begin
        LDivisor := 60000;
      end;
    Ord(ntpSecond):
      begin
        LDivisor := 1000;
      end;
    Ord(ntpTenth):
      begin
        LDivisor := 100;
      end;
    Ord(ntpHundredth):
      begin
        LDivisor := 10;
      end;
    Ord(ntpMillisecond):
      begin
        LDivisor := 1;
      end;
  else
    raise ENyxTimeValue.Create('Unknown clock precision');
  end;

  if FMilliseconds mod LDivisor <> 0 then
  begin
    raise ENyxTimeValue.Create('Clock precision would discard a nonzero value');
  end;
  Result := Self;
  Result.FPrecision := APrecision;
end;

function TNyxClockTime.Compare(const AOther: TNyxClockTime): Integer;
begin
  RequireDefined;
  AOther.RequireDefined;
  Result := 0;

  if FMilliseconds < AOther.FMilliseconds then
  begin
    Result := -1;
  end
  else if FMilliseconds > AOther.FMilliseconds then
  begin
    Result := 1;
  end;
end;

function TNyxClockTime.ToText: TNyxText;

  function Padded(AValue, AWidth: Integer): TNyxText;
  begin
    Result := TNyxText(IntToStr(AValue));
    while Length(Result) < AWidth do
    begin
      Result := '0' + Result;
    end;
  end;

begin
  Result := '';

  if not FDefined then
  begin
    Exit;
  end;
  Result := Padded(Hour, 2) + ':' + Padded(Minute, 2);

  if FPrecision <> ntpMinute then
  begin
    Result := Result + ':' + Padded(Second, 2);
  end;
  case FPrecision of
    ntpMinute, ntpSecond:
      begin
        { These spellings have no fractional part. }
      end;
    ntpTenth:
      begin
        Result := Result + '.' + Padded(Millisecond div 100, 1);
      end;
    ntpHundredth:
      begin
        Result := Result + '.' + Padded(Millisecond div 10, 2);
      end;
    ntpMillisecond:
      begin
        Result := Result + '.' + Padded(Millisecond, 3);
      end;
  end;
end;

end.
