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

unit nyx.dates;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text;

type
  { Invalid calendar values are argument failures, never locale parse failures. }
  ENyxDateValue = class(EArgumentException);

  { Immutable Gregorian calendar date, independent of time zones, DOM, LCL and
    floating-point timestamps. Years are 1..9999. A default/empty record means
    no date, rather than today or an epoch. Text belongs to the canonical
    persistence/control boundary; normal authoring uses NyxDate's integer parts. }
  TNyxCalendarDate = record
  private
    FYear: Integer;
    FMonth: Integer;
    FDay: Integer;
    function GetDefined: Boolean;
  public
    { Reject an impossible date before returning a value. }
    class function FromParts(AYear, AMonth, ADay: Integer): TNyxCalendarDate; static;
    { Empty text is no date. Otherwise accept exactly ASCII YYYY-MM-DD with a
      valid Gregorian day. Never trim, normalize or use machine date formats. }
    class function FromText(const AText: TNyxText): TNyxCalendarDate; static;
    function ToText: TNyxText;
    { Both operands must be defined. Result is -1, 0 or 1 in calendar order. }
    function Compare(const AOther: TNyxCalendarDate): Integer;
    property Defined: Boolean read GetDefined;
    property Year: Integer read FYear;
    property Month: Integer read FMonth;
    property Day: Integer read FDay;
  end;

{ Typed authoring convenience; the result owns only immutable integer parts. }
function NyxDate(AYear, AMonth, ADay: Integer): TNyxCalendarDate;
function NyxNoDate: TNyxCalendarDate;
{ A failed parse returns False and an empty result; malformed text never raises.
  Empty text succeeds with an empty result. Supplementary/non-ASCII digits refuse. }
function TryNyxDate(const AText: TNyxText; out ADate: TNyxCalendarDate): Boolean;
{ Validate year/month before applying the Gregorian 4/100/400 leap-year rule. }
function NyxDaysInMonth(AYear, AMonth: Integer): Integer;

implementation

function NyxDaysInMonth(AYear, AMonth: Integer): Integer;
const
  CDays: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin

  if (AYear < 1) or (AYear > 9999) or (AMonth < 1) or (AMonth > 12) then
  begin
    raise ENyxDateValue.Create('Calendar year/month are outside 1..9999 / 1..12');
  end;
  Result := CDays[AMonth];

  if (AMonth = 2) and (AYear mod 4 = 0) and
    ((AYear mod 100 <> 0) or (AYear mod 400 = 0)) then
  begin
    Inc(Result);
  end;
end;

function NyxNoDate: TNyxCalendarDate;
begin
  Result := Default(TNyxCalendarDate);
end;

function NyxDate(AYear, AMonth, ADay: Integer): TNyxCalendarDate;
begin

  if (ADay < 1) or (ADay > NyxDaysInMonth(AYear, AMonth)) then
  begin
    raise ENyxDateValue.Create('Calendar day does not exist in the selected month');
  end;
  Result.FYear := AYear;
  Result.FMonth := AMonth;
  Result.FDay := ADay;
end;

function TryNyxDate(const AText: TNyxText; out ADate: TNyxCalendarDate): Boolean;
var
  LIndex: Integer;
  LYear: Integer;
  LMonth: Integer;
  LDay: Integer;
begin
  ADate := NyxNoDate;
  Result := False;

  if AText = '' then
  begin
    Exit(True);
  end;

  if (Length(AText) <> 10) or (AText[5] <> '-') or (AText[8] <> '-') then
  begin
    Exit;
  end;
  LYear := 0;
  LMonth := 0;
  LDay := 0;
  for LIndex := 1 to 10 do
  begin

    if LIndex in [5, 8] then
    begin
      Continue;
    end;

    if (AText[LIndex] < '0') or (AText[LIndex] > '9') then
    begin
      Exit;
    end;

    if LIndex < 5 then
    begin
      LYear := LYear * 10 + Ord(AText[LIndex]) - Ord('0');
    end
    else if LIndex < 8 then
    begin
      LMonth := LMonth * 10 + Ord(AText[LIndex]) - Ord('0');
    end
    else
    begin
      LDay := LDay * 10 + Ord(AText[LIndex]) - Ord('0');
    end;
  end;

  if (LYear < 1) or (LYear > 9999) or (LMonth < 1) or (LMonth > 12) then
  begin
    Exit;
  end;

  if (LDay < 1) or (LDay > NyxDaysInMonth(LYear, LMonth)) then
  begin
    Exit;
  end;
  ADate := NyxDate(LYear, LMonth, LDay);
  Result := True;
end;

function TNyxCalendarDate.GetDefined: Boolean;
begin
  Result := FYear <> 0;
end;

class function TNyxCalendarDate.FromParts(AYear, AMonth, ADay: Integer): TNyxCalendarDate;
begin
  Result := NyxDate(AYear, AMonth, ADay);
end;

class function TNyxCalendarDate.FromText(const AText: TNyxText): TNyxCalendarDate;
begin

  if not TryNyxDate(AText, Result) then
  begin
    raise ENyxDateValue.Create('Calendar date requires a valid YYYY-MM-DD value');
  end;
end;

function TNyxCalendarDate.ToText: TNyxText;
var
  LYear: TNyxText;
  LMonth: TNyxText;
  LDay: TNyxText;
begin

  if not Defined then
  begin
    Exit('');
  end;
  LYear := '0000' + TNyxText(IntToStr(FYear));
  LMonth := '00' + TNyxText(IntToStr(FMonth));
  LDay := '00' + TNyxText(IntToStr(FDay));
  Result := Copy(LYear, Length(LYear) - 3, 4) + '-' +
    Copy(LMonth, Length(LMonth) - 1, 2) + '-' + Copy(LDay, Length(LDay) - 1, 2);
end;

function TNyxCalendarDate.Compare(const AOther: TNyxCalendarDate): Integer;
var
  LLeft: Integer;
  LRight: Integer;
begin

  if not Defined or not AOther.Defined then
  begin
    raise ENyxDateValue.Create('Calendar comparison requires two defined dates');
  end;
  { The integer ordering key is exact on FPC and pas2js, including year 9999. }
  LLeft := FYear * 10000 + FMonth * 100 + FDay;
  LRight := AOther.FYear * 10000 + AOther.FMonth * 100 + AOther.FDay;
  Result := 0;

  if LLeft < LRight then
  begin
    Result := -1;
  end
  else if LLeft > LRight then
  begin
    Result := 1;
  end;
end;

end.
