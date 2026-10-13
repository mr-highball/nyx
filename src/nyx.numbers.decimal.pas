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
unit nyx.numbers.decimal;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data;

type
  ENyxDecimalNumber = class(Exception);
  ENyxDecimalOverflow = class(ENyxDecimalNumber);
  ENyxDecimalPrecision = class(ENyxDecimalNumber);
  { Exact is the default for operations that may otherwise lose digits. Nearest
    modes differ at ties; directed modes apply to the signed value. No global
    locale, FPU masks or rounding mode changes occur. }
  TNyxDecimalRounding = (ndrExact, ndrToEven, ndrAwayFromZero,
    ndrTowardZero, ndrFloor, ndrCeiling);
  { Portable 96-bit decimal coefficient and scale 0..28. Immutable owned text
    stores exact digits on both targets; no binary floating arithmetic is used.
    Default and negative zero are canonical zero. Magnitude never exceeds
    79228162514264337593543950335 at scale zero. Factories/operations refuse
    overflow and honor explicit rounding before admitting a result. }
  TNyxDecimalNumber = record
  private
    FDigits: TNyxText;
    FScale: Integer;
    FNegative: Boolean;
    function Digits: TNyxText;
    function Combine(const AOther: TNyxDecimalNumber; ASubtract: Boolean;
      ARounding: TNyxDecimalRounding): TNyxDecimalNumber;
  public
    class function FromInteger(AValue: Integer): TNyxDecimalNumber; static;
    { Typed code-first constructor: unsigned coefficient limbs in high/mid/low
      order, with scale and sign. Every bit of the 96-bit coefficient is exact. }
    class function FromParts(AHigh, AMiddle, ALow: Cardinal; AScale: Integer = 0;
      ANegative: Boolean = False): TNyxDecimalNumber; static;
    { Interchange entry from the existing admitted exact Nyx decimal reference.
      Fraction/exponent spelling is interpreted exactly, never via AsNumber. }
    class function FromDecimal(const AValue: TNyxDecimal;
      ARounding: TNyxDecimalRounding = ndrExact): TNyxDecimalNumber; static;
    function Add(const AOther: TNyxDecimalNumber;
      ARounding: TNyxDecimalRounding = ndrExact): TNyxDecimalNumber;
    function Subtract(const AOther: TNyxDecimalNumber;
      ARounding: TNyxDecimalRounding = ndrExact): TNyxDecimalNumber;
    function CompareTo(const AOther: TNyxDecimalNumber): Integer;
    { Round to at most the requested fractional scale, 0..28; canonical output
      may have fewer places after trailing zero normalization. }
    function Quantize(AScale: Integer;
      ARounding: TNyxDecimalRounding = ndrExact): TNyxDecimalNumber;
    function ToText: TNyxText;
    function ToDecimal: TNyxDecimal;
    property Scale: Integer read FScale;
    property Negative: Boolean read FNegative;
  end;
  { Detached immutable wide accumulation. Only this unit constructs its private
    digits/count; Default is the empty identity. Merge retains all intermediate
    precision, including totals outside the final 96-bit decimal domain. Text
    storage uses ordinary copy-on-write value ownership, never a builder link. }
  TNyxDecimalAccumulation = record
  private
    FDigits: TNyxText;
    FScale: Integer;
    FNegative: Boolean;
    FCount: Integer;
  public
    class function Empty: TNyxDecimalAccumulation; static;
    function Added(const AValue: TNyxDecimalNumber): TNyxDecimalAccumulation;
    function Merged(const AOther: TNyxDecimalAccumulation): TNyxDecimalAccumulation;
    function Total(ARounding: TNyxDecimalRounding = ndrExact): TNyxDecimalNumber;
    function Average(ARounding: TNyxDecimalRounding = ndrToEven): TNyxDecimalNumber;
    property Count: Integer read FCount;
  end;
  { Caller-owned exact decimal accumulator. Push accepts at most High(Integer)
    values and atomically updates its own state. Wider intermediate digits let
    Average succeed for large values whose total exceeds the final 96-bit range.
    Total rounds/adopts once; Average refuses empty. No source/UI references. }
  TNyxDecimalAccumulator = class
  private
    FDigits: TNyxText;
    FScale: Integer;
    FNegative: Boolean;
    FCount: Integer;
  public
    procedure Push(const AValue: TNyxDecimalNumber);
    function Total(ARounding: TNyxDecimalRounding = ndrExact): TNyxDecimalNumber;
    function Average(ARounding: TNyxDecimalRounding = ndrToEven): TNyxDecimalNumber;
    { Copy exact wide state for independent worker publication. Merge is atomic
      on failure and never rounds intermediate digits or retains another owner. }
    function Snapshot: TNyxDecimalAccumulation;
    procedure Merge(const AOther: TNyxDecimalAccumulation);
    property Count: Integer read FCount;
  end;

implementation

const
  CDecimalMaximum: TNyxText = '79228162514264337593543950335';

procedure RequireRounding(ARounding: TNyxDecimalRounding);
begin

  if (Ord(ARounding) < Ord(Low(TNyxDecimalRounding))) or
    (Ord(ARounding) > Ord(High(TNyxDecimalRounding))) then
  begin
    raise ENyxDecimalNumber.Create('Unknown decimal rounding policy');
  end;
end;

function TrimDigits(const AValue: TNyxText): TNyxText;
var
  LStart: Integer;
begin
  LStart := 1;
  while (LStart < Length(AValue)) and (AValue[LStart] = '0') do
  begin
    Inc(LStart);
  end;
  Result := Copy(AValue, LStart, Length(AValue));

  if Result = '' then
  begin
    Result := '0';
  end;
end;

function CompareDigits(const ALeft, ARight: TNyxText): Integer;
begin
  Result := 0;

  if Length(ALeft) < Length(ARight) then
  begin
    Exit(-1);
  end;

  if Length(ALeft) > Length(ARight) then
  begin
    Exit(1);
  end;

  if ALeft < ARight then
  begin
    Result := -1;
  end
  else
  begin

    if ALeft > ARight then
    begin
      Result := 1;
    end;
  end;
end;

function AppendZeros(const AValue: TNyxText; ACount: Integer): TNyxText;
var
  LIndex: Integer;
  LLength: Integer;
begin
  Result := AValue;
  LLength := Length(Result);
  SetLength(Result, LLength + ACount);
  for LIndex := LLength + 1 to Length(Result) do
  begin
    Result[LIndex] := '0';
  end;
end;

function AddDigits(const ALeft, ARight: TNyxText): TNyxText;
var
  LLeft: Integer;
  LRight: Integer;
  LOut: Integer;
  LCarry: Integer;
  LSum: Integer;
begin
  LLeft := Length(ALeft);
  LRight := Length(ARight);
  LOut := LLeft;

  if LRight > LOut then
  begin
    LOut := LRight;
  end;
  SetLength(Result, LOut + 1);
  Inc(LOut);
  LCarry := 0;
  while LOut > 0 do
  begin
    LSum := LCarry;

    if LLeft > 0 then
    begin
      Inc(LSum, Ord(ALeft[LLeft]) - 48);
      Dec(LLeft);
    end;

    if LRight > 0 then
    begin
      Inc(LSum, Ord(ARight[LRight]) - 48);
      Dec(LRight);
    end;
    Result[LOut] := Chr(48 + LSum mod 10);
    LCarry := LSum div 10;
    Dec(LOut);
  end;
  Result := TrimDigits(Result);
end;

function SubtractDigits(const ALeft, ARight: TNyxText): TNyxText;
var
  LLeft: Integer;
  LRight: Integer;
  LBorrow: Integer;
  LValue: Integer;
begin
  { Caller establishes magnitude Left >= Right; each subtraction stays small. }
  SetLength(Result, Length(ALeft));
  LRight := Length(ARight);
  LBorrow := 0;
  for LLeft := Length(ALeft) downto 1 do
  begin
    LValue := Ord(ALeft[LLeft]) - 48 - LBorrow;

    if LRight > 0 then
    begin
      Dec(LValue, Ord(ARight[LRight]) - 48);
      Dec(LRight);
    end;
    LBorrow := 0;

    if LValue < 0 then
    begin
      Inc(LValue, 10);
      LBorrow := 1;
    end;
    Result[LLeft] := Chr(48 + LValue);
  end;
  Result := TrimDigits(Result);
end;

procedure Normalize(var ADigits: TNyxText; var AScale: Integer; var ANegative: Boolean);
begin
  ADigits := TrimDigits(ADigits);

  if ADigits = '0' then
  begin
    AScale := 0;
    ANegative := False;
    Exit;
  end;
  while (AScale > 0) and (ADigits[Length(ADigits)] = '0') do
  begin
    SetLength(ADigits, Length(ADigits) - 1);
    Dec(AScale);
  end;
end;

procedure CombineDigits(const ALeft, ARight: TNyxText;
  ALeftScale, ARightScale: Integer; ALeftNegative, ARightNegative: Boolean;
  out ADigits: TNyxText; out AScale: Integer; out ANegative: Boolean);
var
  LLeft: TNyxText;
  LRight: TNyxText;
  LCompare: Integer;
begin
  AScale := ALeftScale;

  if ARightScale > AScale then
  begin
    AScale := ARightScale;
  end;
  LLeft := AppendZeros(ALeft, AScale - ALeftScale);
  LRight := AppendZeros(ARight, AScale - ARightScale);
  LLeft := TrimDigits(LLeft);
  LRight := TrimDigits(LRight);

  if ALeftNegative = ARightNegative then
  begin
    ADigits := AddDigits(LLeft, LRight);
    ANegative := ALeftNegative;
  end
  else
  begin
    LCompare := CompareDigits(LLeft, LRight);

    if LCompare >= 0 then
    begin
      ADigits := SubtractDigits(LLeft, LRight);
      ANegative := ALeftNegative;
    end
    else
    begin
      ADigits := SubtractDigits(LRight, LLeft);
      ANegative := ARightNegative;
    end;
  end;
  Normalize(ADigits, AScale, ANegative);
end;

function RoundDigits(const ADigits: TNyxText; ADrop: Integer; ANegative: Boolean;
  ARounding: TNyxDecimalRounding; AExtra: Boolean; AExtraHalf: Integer): TNyxText;
var
  LKeep: Integer;
  LIndex: Integer;
  LHalf: Integer;
  LDiscarded: Boolean;
  LIncrement: Boolean;
begin
  RequireRounding(ARounding);
  LKeep := Length(ADigits) - ADrop;
  Result := '0';

  if LKeep > 0 then
  begin
    Result := Copy(ADigits, 1, LKeep);
  end;
  LDiscarded := AExtra;
  LHalf := AExtraHalf;

  if ADrop > 0 then
  begin
    LHalf := -1;

    if LKeep >= 0 then
    begin
      LHalf := Ord(ADigits[LKeep + 1]) - Ord('5');

      if LHalf > 0 then
      begin
        LHalf := 1;
      end
      else
      begin

        if LHalf < 0 then
        begin
          LHalf := -1;
        end;
      end;
    end;
    LIndex := LKeep + 1;

    if LIndex < 1 then
    begin
      LIndex := 1;
    end;
    while LIndex <= Length(ADigits) do
    begin

      if ADigits[LIndex] <> '0' then
      begin
        LDiscarded := True;
      end;

      if (LHalf = 0) and (LIndex > LKeep + 1) and (ADigits[LIndex] <> '0') then
      begin
        LHalf := 1;
      end;
      Inc(LIndex);
    end;

    if (LHalf = 0) and AExtra then
    begin
      LHalf := 1;
    end;
  end
  else
  begin
    Result := ADigits;
  end;
  LIncrement := False;
  case ARounding of
    ndrExact:
    begin

      if LDiscarded then
      begin
        raise ENyxDecimalPrecision.Create('Exact decimal operation would lose digits');
      end;
    end;
    ndrToEven:
    begin
      LIncrement := (LHalf > 0) or ((LHalf = 0) and
        ((Ord(Result[Length(Result)]) - 48) mod 2 <> 0));
    end;
    ndrAwayFromZero:
    begin
      LIncrement := LHalf >= 0;
    end;
    ndrFloor:
    begin
      LIncrement := ANegative and LDiscarded;
    end;
    ndrCeiling:
    begin
      LIncrement := not ANegative and LDiscarded;
    end;
    ndrTowardZero:
    begin
      LIncrement := False;
    end;
  end;

  if LIncrement and LDiscarded then
  begin
    Result := AddDigits(Result, '1');
  end;
end;

function Adopt(ADigits: TNyxText; AScale: Integer; ANegative: Boolean;
  ARounding: TNyxDecimalRounding; AExtra: Boolean = False; AExtraHalf: Integer = -1):
  TNyxDecimalNumber;
var
  LDrop: Integer;
  LRounded: TNyxText;
  LScale: Integer;
  LNegative: Boolean;
begin
  RequireRounding(ARounding);

  if not AExtra then
  begin
    Normalize(ADigits, AScale, ANegative);
  end;
  LDrop := 0;

  if AScale > 28 then
  begin
    LDrop := AScale - 28;
  end;
  { Retry from ORIGINAL digits, never from an already rounded intermediate.
    This avoids double rounding when both scale and coefficient need reduction. }
  repeat
    LRounded := RoundDigits(ADigits, LDrop, ANegative, ARounding, AExtra, AExtraHalf);
    LScale := AScale - LDrop;

    if CompareDigits(LRounded, CDecimalMaximum) <= 0 then
    begin
      Break;
    end;

    if LScale = 0 then
    begin
      raise ENyxDecimalOverflow.Create('Decimal exceeds its 96-bit coefficient domain');
    end;
    Inc(LDrop);
  until False;
  LNegative := ANegative;
  Normalize(LRounded, LScale, LNegative);
  Result.FDigits := LRounded;
  Result.FScale := LScale;
  Result.FNegative := LNegative;
end;

function TNyxDecimalNumber.Digits: TNyxText;
begin
  Result := FDigits;

  if Result = '' then
  begin
    Result := '0';
  end;
end;

class function TNyxDecimalNumber.FromInteger(AValue: Integer): TNyxDecimalNumber;
begin
  Result := FromDecimal(NyxDecimal(TNyxText(IntToStr(AValue))));
end;

class function TNyxDecimalNumber.FromParts(AHigh, AMiddle, ALow: Cardinal;
  AScale: Integer; ANegative: Boolean): TNyxDecimalNumber;
var
  LLimbs: array[0..5] of Cardinal;
  LIndex: Integer;
  LCarry: Cardinal;
  LCurrent: Cardinal;
  LNonzero: Boolean;
  LDigits: TNyxText;
begin

  if (AScale < 0) or (AScale > 28) then
  begin
    raise ENyxDecimalNumber.Create('Decimal scale must be between zero and 28');
  end;
  LLimbs[0] := AHigh shr 16;
  LLimbs[1] := AHigh and $ffff;
  LLimbs[2] := AMiddle shr 16;
  LLimbs[3] := AMiddle and $ffff;
  LLimbs[4] := ALow shr 16;
  LLimbs[5] := ALow and $ffff;
  LDigits := '';
  repeat
    LCarry := 0;
    LNonzero := False;
    for LIndex := 0 to 5 do
    begin
      LCurrent := LCarry * 65536 + LLimbs[LIndex];
      LLimbs[LIndex] := LCurrent div 10;
      LCarry := LCurrent mod 10;
      LNonzero := LNonzero or (LLimbs[LIndex] <> 0);
    end;
    LDigits := TNyxText(Chr(48 + LCarry)) + LDigits;
  until not LNonzero;
  Result := Adopt(LDigits, AScale, ANegative, ndrExact);
end;

class function TNyxDecimalNumber.FromDecimal(const AValue: TNyxDecimal;
  ARounding: TNyxDecimalRounding): TNyxDecimalNumber;
var
  LText: TNyxText;
  LDigits: TNyxText;
  LIndex: Integer;
  LLength: Integer;
  LFraction: Integer;
  LExponent: Integer;
  LScale: Integer;
  LNegative: Boolean;
  LAfterPoint: Boolean;
  LExponentNegative: Boolean;
  LOverflowExponent: Boolean;
begin
  RequireRounding(ARounding);
  LText := AValue.Text;

  if LText = '' then
  begin
    raise ENyxDecimalNumber.Create('Decimal reference is not initialized');
  end;
  LNegative := LText[1] = '-';
  LIndex := 1;

  if LNegative then
  begin
    Inc(LIndex);
  end;
  SetLength(LDigits, Length(LText));
  LLength := 0;
  LFraction := 0;
  LAfterPoint := False;
  while (LIndex <= Length(LText)) and (LText[LIndex] <> 'e') and (LText[LIndex] <> 'E') do
  begin

    if LText[LIndex] = '.' then
    begin
      LAfterPoint := True;
    end
    else
    begin
      Inc(LLength);
      LDigits[LLength] := LText[LIndex];

      if LAfterPoint then
      begin
        Inc(LFraction);
      end;
    end;
    Inc(LIndex);
  end;
  SetLength(LDigits, LLength);
  LDigits := TrimDigits(LDigits);
  LExponent := 0;
  LExponentNegative := False;
  LOverflowExponent := False;

  if LIndex <= Length(LText) then
  begin
    Inc(LIndex);

    if LText[LIndex] in ['-', '+'] then
    begin
      LExponentNegative := LText[LIndex] = '-';
      Inc(LIndex);
    end;
    while LIndex <= Length(LText) do
    begin

      if LExponent > 100000000 then
      begin
        LOverflowExponent := True;
      end
      else
      begin
        LExponent := LExponent * 10 + Ord(LText[LIndex]) - 48;
      end;
      Inc(LIndex);
    end;
  end;

  if LDigits = '0' then
  begin
    Exit(Adopt('0', 0, False, ARounding));
  end;

  if LOverflowExponent then
  begin

    if not LExponentNegative then
    begin
      raise ENyxDecimalOverflow.Create('Decimal positive exponent exceeds its domain');
    end;
    { Nonzero values this tiny are below any representable rounding quantum.
      A large scale works without allocating exponent-sized padding. }
    Exit(Adopt(LDigits, High(Integer) div 2, LNegative, ARounding));
  end;

  if LExponentNegative then
  begin
    LExponent := -LExponent;
  end;

  if LExponent > LFraction then
  begin

    if LExponent - LFraction > 29 then
    begin
      raise ENyxDecimalOverflow.Create('Decimal positive exponent exceeds its domain');
    end;
    LDigits := AppendZeros(LDigits, LExponent - LFraction);
    LScale := 0;
  end
  else
  begin
    LScale := LFraction - LExponent;
  end;
  Result := Adopt(LDigits, LScale, LNegative, ARounding);
end;

function TNyxDecimalNumber.Combine(const AOther: TNyxDecimalNumber; ASubtract: Boolean;
  ARounding: TNyxDecimalRounding): TNyxDecimalNumber;
var
  LDigits: TNyxText;
  LScale: Integer;
  LNegative: Boolean;
begin
  CombineDigits(Digits, AOther.Digits, FScale, AOther.FScale,
    FNegative, AOther.FNegative <> ASubtract, LDigits, LScale, LNegative);
  Result := Adopt(LDigits, LScale, LNegative, ARounding);
end;

function TNyxDecimalNumber.Add(const AOther: TNyxDecimalNumber;
  ARounding: TNyxDecimalRounding): TNyxDecimalNumber;
begin
  Result := Combine(AOther, False, ARounding);
end;

function TNyxDecimalNumber.Subtract(const AOther: TNyxDecimalNumber;
  ARounding: TNyxDecimalRounding): TNyxDecimalNumber;
begin
  Result := Combine(AOther, True, ARounding);
end;

function TNyxDecimalNumber.CompareTo(const AOther: TNyxDecimalNumber): Integer;
var
  LScale: Integer;
  LLeft: TNyxText;
  LRight: TNyxText;
begin

  if FNegative <> AOther.FNegative then
  begin

    if FNegative then
    begin
      Exit(-1);
    end;
    Exit(1);
  end;
  LScale := FScale;

  if AOther.FScale > LScale then
  begin
    LScale := AOther.FScale;
  end;
  LLeft := TrimDigits(AppendZeros(Digits, LScale - FScale));
  LRight := TrimDigits(AppendZeros(AOther.Digits, LScale - AOther.FScale));
  Result := CompareDigits(LLeft, LRight);

  if FNegative then
  begin
    Result := -Result;
  end;
end;

function TNyxDecimalNumber.Quantize(AScale: Integer;
  ARounding: TNyxDecimalRounding): TNyxDecimalNumber;
var
  LRounded: TNyxText;
begin
  RequireRounding(ARounding);

  if (AScale < 0) or (AScale > 28) then
  begin
    raise ENyxDecimalNumber.Create('Decimal scale must be between zero and 28');
  end;

  if AScale >= FScale then
  begin
    Exit(Self);
  end;
  LRounded := RoundDigits(Digits, FScale - AScale, FNegative, ARounding, False, -1);
  Result := Adopt(LRounded, AScale, FNegative, ndrExact);
end;

function TNyxDecimalNumber.ToText: TNyxText;
var
  LDigits: TNyxText;
  LIndex: Integer;
  LPrefix: TNyxText;
begin
  LDigits := Digits;

  if FScale = 0 then
  begin
    Result := LDigits;
  end
  else
  begin

    if FScale >= Length(LDigits) then
    begin
      SetLength(LPrefix, FScale - Length(LDigits));
      for LIndex := 1 to Length(LPrefix) do
      begin
        LPrefix[LIndex] := '0';
      end;
      Result := TNyxText('0.') + LPrefix + LDigits;
    end
    else
    begin
      Result := Copy(LDigits, 1, Length(LDigits) - FScale) + TNyxText('.') +
        Copy(LDigits, Length(LDigits) - FScale + 1, FScale);
    end;
  end;

  if FNegative then
  begin
    Result := '-' + Result;
  end;
end;

function TNyxDecimalNumber.ToDecimal: TNyxDecimal;
begin
  Result := NyxDecimal(ToText);
end;

procedure TNyxDecimalAccumulator.Push(const AValue: TNyxDecimalNumber);
var
  LDigits: TNyxText;
  LScale: Integer;
  LNegative: Boolean;
  LExisting: TNyxText;
begin

  if FCount = High(Integer) then
  begin
    raise ENyxDecimalOverflow.Create('Decimal accumulator count exceeds Integer');
  end;
  LExisting := FDigits;

  if LExisting = '' then
  begin
    LExisting := '0';
  end;
  CombineDigits(LExisting, AValue.Digits, FScale, AValue.FScale, FNegative,
    AValue.FNegative, LDigits, LScale, LNegative);
  Inc(FCount);
  FDigits := LDigits;
  FScale := LScale;
  FNegative := LNegative;
end;

function TNyxDecimalAccumulator.Total(ARounding: TNyxDecimalRounding): TNyxDecimalNumber;
begin
  Result := Adopt(FDigits, FScale, FNegative, ARounding);
end;

function TNyxDecimalAccumulation.Average(ARounding: TNyxDecimalRounding): TNyxDecimalNumber;
var
  LNumerator: TNyxText;
  LQuotient: TNyxText;
  LIndex: Integer;
  LCarry: {$ifdef PAS2JS}NativeInt{$else}Int64{$endif};
  LValue: {$ifdef PAS2JS}NativeInt{$else}Int64{$endif};
  LHalf: Integer;
begin
  RequireRounding(ARounding);

  if FCount = 0 then
  begin
    raise ENyxDecimalNumber.Create('Decimal average requires at least one value');
  end;
  { Base-ten division carry remains below 10*High(Integer), exact in native
    Int64 and browser safe integers. No 96-bit value is projected through Double. }
  LNumerator := AppendZeros(FDigits, 28 - FScale);
  SetLength(LQuotient, Length(LNumerator));
  LCarry := 0;
  for LIndex := 1 to Length(LNumerator) do
  begin
    LValue := LCarry * 10 + Ord(LNumerator[LIndex]) - 48;
    LQuotient[LIndex] := Chr(48 + LValue div FCount);
    LCarry := LValue mod FCount;
  end;
  LQuotient := TrimDigits(LQuotient);
  LHalf := -1;

  if LCarry * 2 = FCount then
  begin
    LHalf := 0;
  end
  else
  begin

    if LCarry * 2 > FCount then
    begin
      LHalf := 1;
    end;
  end;
  Result := Adopt(LQuotient, 28, FNegative, ARounding, LCarry <> 0, LHalf);
end;

function TNyxDecimalAccumulator.Snapshot: TNyxDecimalAccumulation;
begin
  Result.FDigits := FDigits;
  Result.FScale := FScale;
  Result.FNegative := FNegative;
  Result.FCount := FCount;
end;

procedure TNyxDecimalAccumulator.Merge(const AOther: TNyxDecimalAccumulation);
var
  LCandidate: TNyxDecimalAccumulation;
begin
  LCandidate := Snapshot.Merged(AOther);
  FDigits := LCandidate.FDigits;
  FScale := LCandidate.FScale;
  FNegative := LCandidate.FNegative;
  FCount := LCandidate.FCount;
end;

function TNyxDecimalAccumulator.Average(ARounding: TNyxDecimalRounding): TNyxDecimalNumber;
begin
  Result := Snapshot.Average(ARounding);
end;

class function TNyxDecimalAccumulation.Empty: TNyxDecimalAccumulation;
begin
  Result.FDigits := '0';
  Result.FScale := 0;
  Result.FNegative := False;
  Result.FCount := 0;
end;

function TNyxDecimalAccumulation.Added(const AValue: TNyxDecimalNumber): TNyxDecimalAccumulation;
var
  LSingle: TNyxDecimalAccumulation;
begin
  LSingle.FDigits := AValue.Digits;
  LSingle.FScale := AValue.FScale;
  LSingle.FNegative := AValue.FNegative;
  LSingle.FCount := 1;
  Result := Merged(LSingle);
end;

function TNyxDecimalAccumulation.Merged(const AOther: TNyxDecimalAccumulation): TNyxDecimalAccumulation;
var
  LDigits: TNyxText;
  LScale: Integer;
  LNegative: Boolean;
  LExisting: TNyxText;
begin

  if AOther.FCount > High(Integer) - FCount then
  begin
    raise ENyxDecimalOverflow.Create('Decimal accumulator count exceeds Integer');
  end;

  if AOther.FCount = 0 then
  begin
    Exit(Self);
  end;
  LExisting := FDigits;

  if LExisting = '' then
  begin
    LExisting := '0';
  end;
  CombineDigits(LExisting, AOther.FDigits, FScale, AOther.FScale, FNegative,
    AOther.FNegative, LDigits, LScale, LNegative);
  Result.FDigits := LDigits;
  Result.FScale := LScale;
  Result.FNegative := LNegative;
  Result.FCount := FCount + AOther.FCount;
end;

function TNyxDecimalAccumulation.Total(ARounding: TNyxDecimalRounding): TNyxDecimalNumber;
begin
  Result := Adopt(FDigits, FScale, FNegative, ARounding);
end;

end.
