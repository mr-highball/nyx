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
unit nyx.pascal.reals;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler;

type
  { Exact mathematical decimal evidence, separate from any compiler-rounded
    float. Coefficient is an unsigned normalized decimal integer; Exponent is
    an arbitrary-length signed decimal integer. Value = sign * coefficient *
    10^exponent. Neither exponent expansion nor a host floating conversion occurs.
    Spelling retains the complete unsigned Pascal token; Negated records a
    surrounding unary sign separately, including a negative zero. Unknown is
    the default. Strings are immutable value copies; no query/owner is retained. }
  TNyxPascalRealValue = record
  private
    FKnown: Boolean;
    FNegative: Boolean;
    FSpelling, FCoefficient, FExponent: TNyxText;
  public
    class function Unknown: TNyxPascalRealValue; static;
    function Negated: TNyxPascalRealValue;
    function IsZero: Boolean;
    { Mathematical equality ignores zero's sign. Unknown supplies no equality
      proof, even against Unknown; source identity instead uses Spelling. }
    function SameValue(const AOther: TNyxPascalRealValue;
      const ACancellation: INyxCancellationScope = nil): Boolean;
    { Optional checked projection for consumers whose own algorithms require
      a signed 32-bit exponent. False leaves the out value zero; the exact
      decimal exponent remains available, including beyond browser integer range. }
    function TryExponent(out AValue: Integer;
      const ACancellation: INyxCancellationScope = nil): Boolean;
    property Known: Boolean read FKnown;
    property Negative: Boolean read FNegative;
    property Spelling: TNyxText read FSpelling;
    property Coefficient: TNyxText read FCoefficient;
    property Exponent: TNyxText read FExponent;
  end;

{ One complete unsigned decimal real token in the current Pascal lexical
  contract: fraction and/or exponent, with decimal digits on both sides of a
  written point. Unary signs, radix prefixes, separators, whitespace, NaN and
  infinity are outside this token entry. Malformed spelling raises the existing
  ENyxPascalValue family. Syntax/value proof grants no compiler range, rounded
  value, representation, literal type or generic-call eligibility. All work is
  charged to the caller's existing budget; cancellation/refusal returns no value. }
function NyxPascalRealToken(const ASpelling: TNyxText;
  const ACancellation: INyxCancellationScope = nil): TNyxPascalRealValue;

implementation

uses SysUtils, nyx.pascal.values, nyx.pascal.work, nyx.pascal.lexing;

{ Work admission dispatches optional budgets. Ordinary cancellation scopes do
  not implement that capability, so each actual decimal step checks both. }
procedure Admit(const AScope: INyxCancellationScope; APhase: TNyxPascalAnalysisPhase;
  AUnits: Integer = 1);
begin

  if (AScope <> nil) and AScope.Cancelled then
    raise ENyxPascalAnalysisCancelled.Create('Pascal real-value analysis was cancelled');
  NyxPascalAdmitAnalysis(AScope,APhase,AUnits);
end;

class function TNyxPascalRealValue.Unknown: TNyxPascalRealValue;
begin
  Result := Default(TNyxPascalRealValue);
end;

function TNyxPascalRealValue.Negated: TNyxPascalRealValue;
begin
  Result := Self;

  if Result.FKnown then Result.FNegative := not Result.FNegative;
end;

function TNyxPascalRealValue.IsZero: Boolean;
begin
  Result := FKnown and (FCoefficient = '0');
end;

function TNyxPascalRealValue.SameValue(const AOther: TNyxPascalRealValue;
  const ACancellation: INyxCancellationScope): Boolean;
begin
  Admit(ACancellation,papConstantStep);

  if not FKnown or not AOther.FKnown then Exit(False);
  Admit(ACancellation,papSourceProof,Length(FCoefficient));
  Admit(ACancellation,papSourceProof,Length(FExponent));
  Result := FKnown and AOther.FKnown and (FCoefficient = AOther.FCoefficient) and
    (FExponent = AOther.FExponent) and ((FNegative = AOther.FNegative) or IsZero);
end;

function TNyxPascalRealValue.TryExponent(out AValue: Integer;
  const ACancellation: INyxCancellationScope): Boolean;
var LAt, LLimit, LNumber, LDigit: Integer; LNegative: Boolean;
begin
  Result := False; AValue := 0;
  Admit(ACancellation,papConstantStep);

  if not FKnown then Exit;
  LAt := 1; LNegative := FExponent[1] = '-';

  if LNegative then Inc(LAt);
  LLimit := -High(Integer);

  if LNegative then LLimit := Low(Integer);
  LNumber := 0;
  while LAt <= Length(FExponent) do
  begin
    Admit(ACancellation,papConstantStep);
    LDigit := Ord(FExponent[LAt])-Ord('0');

    if LNumber < LLimit div 10 then Exit;
    LNumber := LNumber*10;

    if LNumber < LLimit+LDigit then Exit;
    Dec(LNumber,LDigit); Inc(LAt);
  end;

  if not LNegative then LNumber := -LNumber;
  AValue := LNumber; Result := True;
end;

{ The exact exponent needs only addition/subtraction of decimal digit strings.
  Carry/borrow values remain below twenty on both native and browser targets. }
function AddExponent(const ALeft: TNyxText; ADelta: Integer;
  const ACancel: INyxCancellationScope): TNyxText;
var LLeft, LRight, LBuffer, LSwap: TNyxText;
  LLeftNegative, LRightNegative, LNegative, LAdd: Boolean;
  LAt, LStart, LWrite, LLength, LLeftAt, LRightAt, LDigit, LCarry, LCompare: Integer;
begin
  Result := ALeft;

  if ADelta = 0 then Exit;
  LRight := TNyxText(IntToStr(ADelta)); LLeft := ALeft;
  LLeftNegative := LLeft[1] = '-'; LRightNegative := LRight[1] = '-';

  if LLeftNegative then Delete(LLeft,1,1);
  if LRightNegative then Delete(LRight,1,1);
  LAdd := LLeftNegative = LRightNegative;
  LNegative := LLeftNegative;
  LCompare := 0;

  if not LAdd then
  begin

    if Length(LLeft) < Length(LRight) then LCompare := -1
    else if Length(LLeft) > Length(LRight) then LCompare := 1
    else
      for LAt := 1 to Length(LLeft) do
      begin
        Admit(ACancel,papConstantStep);

        if LLeft[LAt] < LRight[LAt] then LCompare := -1
        else if LLeft[LAt] > LRight[LAt] then LCompare := 1;

        if LCompare <> 0 then Break;
      end;

    if LCompare = 0 then Exit('0');

    if LCompare < 0 then
    begin
      LSwap := LLeft; LLeft := LRight; LRight := LSwap;
      LNegative := LRightNegative;
    end;
  end;
  LLength := Length(LLeft);

  if LLength < Length(LRight) then LLength := Length(LRight);
  { Both source strings already occupy representable storage. Admit the one
    possible carry before allocating, including when the caller budget refuses. }
  Admit(ACancel,papSourceProof,LLength);

  if LLength = High(Integer) then
    raise ENyxPascalValue.Create('A real exponent cannot allocate its carry digit');
  SetLength(LBuffer,LLength+1);
  LLeftAt := Length(LLeft); LRightAt := Length(LRight);
  LWrite := LLength+1; LCarry := 0;
  while LWrite > 1 do
  begin
    Admit(ACancel,papConstantStep);
    LDigit := 0;

    if LLeftAt > 0 then LDigit := Ord(LLeft[LLeftAt])-Ord('0');

    if LAdd then
    begin
      Inc(LDigit,LCarry);

      if LRightAt > 0 then Inc(LDigit,Ord(LRight[LRightAt])-Ord('0'));
      LCarry := LDigit div 10; LDigit := LDigit mod 10;
    end
    else
    begin
      Dec(LDigit,LCarry);

      if LRightAt > 0 then Dec(LDigit,Ord(LRight[LRightAt])-Ord('0'));
      LCarry := 0;

      if LDigit < 0 then begin Inc(LDigit,10); LCarry := 1; end;
    end;
    LBuffer[LWrite] := Char(Ord('0')+LDigit);
    Dec(LLeftAt); Dec(LRightAt); Dec(LWrite);
  end;
  LBuffer[1] := Char(Ord('0')+LCarry); LStart := 1;
  while (LStart < Length(LBuffer)) and (LBuffer[LStart] = '0') do
  begin
    Admit(ACancel,papConstantStep); Inc(LStart);
  end;
  Result := Copy(LBuffer,LStart,Length(LBuffer)-LStart+1);

  if LNegative and (Result <> '0') then Result := '-'+Result;
end;

function NyxPascalRealToken(const ASpelling: TNyxText;
  const ACancellation: INyxCancellationScope): TNyxPascalRealValue;
var LValue: TNyxPascalRealValue; LDigits, LExponent: TNyxText;
  LAt, LCount, LWrite, LFraction, LFirst, LLast, LExponentStart, LStart: Integer;
  LReal, LNegativeExponent: Boolean;

  procedure Digits;
  var LBefore: Integer;
  begin
    LBefore := LWrite;
    while (LAt <= LCount) and (ASpelling[LAt] in ['0'..'9']) do
    begin
      Admit(ACancellation,papConstantStep);
      Inc(LWrite); LDigits[LWrite] := ASpelling[LAt]; Inc(LAt);
    end;

    if LWrite = LBefore then raise ENyxPascalValue.Create('A real token needs decimal digits');
  end;
begin
  Result := TNyxPascalRealValue.Unknown;
  LCount := Length(ASpelling);
  Admit(ACancellation,papConstantStep);

  if LCount = 0 then raise ENyxPascalValue.Create('A real token cannot be empty');
  Admit(ACancellation,papSourceProof,LCount);
  LValue := TNyxPascalRealValue.Unknown;
  SetLength(LDigits,LCount); LAt := 1; LWrite := 0; LFraction := 0; LReal := False;
  Digits;

  if (LAt <= LCount) and (ASpelling[LAt] = '.') then
  begin
    LReal := True; Inc(LAt); LFraction := LWrite; Digits;
    LFraction := LWrite-LFraction;
  end;
  LExponent := '0';

  if (LAt <= LCount) and (ASpelling[LAt] in ['e','E']) then
  begin
    LReal := True; Inc(LAt); LNegativeExponent := False;

    if (LAt <= LCount) and (ASpelling[LAt] in ['+','-']) then
    begin
      LNegativeExponent := ASpelling[LAt] = '-'; Inc(LAt);
    end;
    LExponentStart := LAt;
    while (LAt <= LCount) and (ASpelling[LAt] in ['0'..'9']) do
    begin
      Admit(ACancellation,papConstantStep); Inc(LAt);
    end;

    if LAt = LExponentStart then raise ENyxPascalValue.Create('A real token needs a complete exponent');
    LStart := LExponentStart;
    while (LStart < LAt-1) and (ASpelling[LStart] = '0') do
    begin
      Admit(ACancellation,papConstantStep); Inc(LStart);
    end;
    LExponent := Copy(ASpelling,LStart,LAt-LStart);

    if LNegativeExponent and (LExponent <> '0') then LExponent := '-'+LExponent;
  end;

  if not LReal or (LAt <= LCount) then
    raise ENyxPascalValue.Create('The spelling is not one complete unsigned Pascal real token');
  LFirst := 1; LLast := LWrite;
  while (LFirst < LLast) and (LDigits[LFirst] = '0') do
  begin
    Admit(ACancellation,papConstantStep); Inc(LFirst);
  end;

  if LDigits[LFirst] = '0' then
  begin
    LValue.FCoefficient := '0'; LValue.FExponent := '0';
  end
  else
  begin
    while LDigits[LLast] = '0' do
    begin
      Admit(ACancellation,papConstantStep); Dec(LLast);
    end;
    LValue.FCoefficient := Copy(LDigits,LFirst,LLast-LFirst+1);
    LValue.FExponent := AddExponent(LExponent,LWrite-LLast-LFraction,ACancellation);
  end;
  LValue.FSpelling := ASpelling; LValue.FKnown := True;
  Result := LValue;
end;

end.
