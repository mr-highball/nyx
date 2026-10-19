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
unit nyx.pascal.numeric;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.values, nyx.pascal.syntax;

type
  { Canonical fixed integer ranges, independently of host register/pointer size.
    Distinct types, enums, subranges and native aliases need their own binding
    proof before entering this closed arithmetic rule owner. }
  TNyxPascalIntegerKind = (pikNone, pikShortInt, pikByte, pikSmallInt, pikWord,
    pikLongInt, pikLongWord, pikInt64, pikQWord);
  { An operand retains an optional exact source constant. Unknown means runtime
    data; a routine result must never carry its argument's constant proof. }
  TNyxPascalIntegerOperand = record
    Kind: TNyxPascalIntegerKind;
    Constant: TNyxPascalConstantValue;
  end;
  { Some FPC rules preserve an operand's actual declared identity. A canonical
    result otherwise names a fresh intrinsic; equal ranges alone grant no alias
    equivalence or source declaration identity. }
  TNyxPascalIntegerReuse = (pirNone, pirLeft, pirRight);
  { Detached result with no world, callback or interface lease. Kind=None and
    BooleanResult=False refuse. A known constant is a checked mathematical/
    fixed-pattern proof, not permission for a runtime conversion or execution. }
  TNyxPascalIntegerResult = record
    Kind: TNyxPascalIntegerKind;
    BooleanResult: Boolean;
    Constant: TNyxPascalConstantValue;
    Reuse: TNyxPascalIntegerReuse;
  end;

function NyxPascalIntegerName(AKind: TNyxPascalIntegerKind): TNyxText;
{ FPC 3.3.1 Delphi/DelphiUnicode rules, qualified by the binding caller. ALUBits
  must be captured CPUINT8/16/32/64, never the analyzer's own SizeOf(Integer).
  No general conversion, floating-point, wrapping or invalid-shift rule is
  inferred. Existing exact-value operations own the bounded 64-bit domain. }
function NyxPascalIntegerOperation(AOperation: TNyxPascalOperatorKind;
  const ALeft, ARight: TNyxPascalIntegerOperand; AUnary: Boolean; AALUBits: Integer;
  const ACancellation: INyxCancellationScope = nil): TNyxPascalIntegerResult;

implementation

uses nyx.pascal.work;

const
  CBits: array[TNyxPascalIntegerKind] of Integer = (0,8,8,16,16,32,32,64,64);
  CNames: array[TNyxPascalIntegerKind] of TNyxText = ('','shortint','byte',
    'smallint','word','longint','longword','int64','qword');
  CUpper: array[TNyxPascalIntegerKind] of TNyxText = ('0','127','255','32767',
    '65535','2147483647','4294967295','9223372036854775807','18446744073709551615');
  CLowerMagnitude: array[TNyxPascalIntegerKind] of TNyxText = ('0','128','0',
    '32768','0','2147483648','0','9223372036854775808','0');

function NyxPascalIntegerName(AKind: TNyxPascalIntegerKind): TNyxText;
begin
  Result := CNames[AKind];
end;

function Signed(AKind: TNyxPascalIntegerKind): Boolean;
begin
  Result := AKind in [pikShortInt,pikSmallInt,pikLongInt,pikInt64];
end;

function AtWidth(ABits: Integer; ASigned: Boolean): TNyxPascalIntegerKind;
begin
  Result := pikNone;
  case ABits of
    8: Result := pikShortInt;
    16: Result := pikSmallInt;
    32: Result := pikLongInt;
    64: Result := pikInt64;
  end;

  if not ASigned and (Result <> pikNone) then Result := Succ(Result);
end;

function Fits(const AValue: TNyxPascalConstantValue; AKind: TNyxPascalIntegerKind): Boolean;
var
  LLow, LHigh: TNyxPascalConstantValue;
begin
  Result := False;

  if (AValue.Kind <> pvvInteger) or (AKind = pikNone) then Exit;
  LLow := NyxPascalIntegerToken(CLowerMagnitude[AKind],10).Negated;
  LHigh := NyxPascalIntegerToken(CUpper[AKind],10);
  Result := (AValue.Number.CompareTo(LLow.Number) >= 0) and
    (AValue.Number.CompareTo(LHigh.Number) <= 0);
end;

{ The caller charges each bounded range candidate. No machine integer ever
  stores these bounds, including QWord's maximum on a pas2js target. }
function Narrow(const AValue: TNyxPascalConstantValue;
  const ACancel: INyxCancellationScope): TNyxPascalIntegerKind;
var
  LKind: TNyxPascalIntegerKind;
begin
  Result := pikNone;
  for LKind := pikShortInt to pikQWord do
  begin
    NyxPascalAdmitAnalysis(ACancel,papConstantStep);

    if Fits(AValue,LKind) then Exit(LKind);
  end;
end;

{ Union the complete type ranges, preserving FPC's signed-first selection.
  Keeping equal operand signs is a separate bitwise rule, not general numeric
  promotion. A signed/unsigned 64-bit union has no representable signed range. }
function Common(ALeft, ARight: TNyxPascalIntegerKind; AKeepSign: Boolean;
  const ACancel: INyxCancellationScope): TNyxPascalIntegerKind;
var
  LKind: TNyxPascalIntegerKind;
begin
  Result := pikNone;
  for LKind := pikShortInt to pikQWord do
  begin
    NyxPascalAdmitAnalysis(ACancel,papConstantStep);

    if (CBits[LKind] < CBits[ALeft]) or (CBits[LKind] < CBits[ARight]) then Continue;

    if not Signed(LKind) and (Signed(ALeft) or Signed(ARight)) then Continue;

    if Signed(LKind) and (((not Signed(ALeft)) and (CBits[ALeft] >= CBits[LKind])) or
      ((not Signed(ARight)) and (CBits[ARight] >= CBits[LKind]))) then Continue;
    Result := LKind;
    Break;
  end;

  if AKeepSign and (Result <> pikNone) and (Signed(ALeft) = Signed(ARight)) and
    (Signed(Result) <> Signed(ALeft)) then
  begin

    if Result = pikQWord then Exit(pikNone);
    Result := Succ(Result);
  end;
end;

function NyxPascalIntegerOperation(AOperation: TNyxPascalOperatorKind;
  const ALeft, ARight: TNyxPascalIntegerOperand; AUnary: Boolean; AALUBits: Integer;
  const ACancellation: INyxCancellationScope): TNyxPascalIntegerResult;
var
  LSInt, LUInt, LKind: TNyxPascalIntegerKind;
  LValue, LMax: TNyxPascalConstantValue;
  LOperation: TNyxPascalValueOperation;
  LConstant, LUnsignedLeft, LUnsignedRight: Boolean;

  function PositiveFits(const AValue: TNyxPascalConstantValue;
    AKind: TNyxPascalIntegerKind): Boolean;
  begin
    Result := (AValue.Kind = pvvInteger) and not AValue.Number.Negative and Fits(AValue,AKind);
  end;

begin
  Result := Default(TNyxPascalIntegerResult);
  NyxPascalAdmitAnalysis(ACancellation,papConstantStep);
  LSInt := AtWidth(AALUBits,True);
  LUInt := AtWidth(AALUBits,False);

  if (LSInt = pikNone) or (ALeft.Kind = pikNone) or
    (not AUnary and (ARight.Kind = pikNone)) then Exit;
  LConstant := (ALeft.Constant.Kind = pvvInteger) and
    (AUnary or (ARight.Constant.Kind = pvvInteger));
  LKind := pikNone;
  LValue := TNyxPascalConstantValue.Unknown;

  if AUnary then
  begin
    case AOperation of
      pokAdd: begin LKind := ALeft.Kind; LValue := ALeft.Constant; Result.Reuse := pirLeft; end;
      pokSubtract:
        begin
          LKind := AtWidth(CBits[ALeft.Kind],True);

          if CBits[ALeft.Kind] <= AALUBits then LKind := LSInt;

          if LConstant then
          begin
            LValue := ALeft.Constant.Negated;
            LKind := Narrow(LValue,ACancellation);
          end;
        end;
      pokNot:
        begin
          LKind := ALeft.Kind;
          Result.Reuse := pirLeft;

          if LConstant then
          begin
            Result.Reuse := pirNone;

            if not Signed(ALeft.Kind) and (CBits[ALeft.Kind] >= AALUBits) then
            begin
              LMax := NyxPascalIntegerToken(CUpper[ALeft.Kind],10);
              LValue := LMax.Operate(pvoSubtract,ALeft.Constant);
            end
            else
            begin
              LValue := ALeft.Constant.Complemented64;
              LKind := Common(ALeft.Kind,LSInt,False,ACancellation);
            end;
          end;
        end;
    else Exit;
    end;
  end
  else if AOperation in [pokEqual,pokNotEqual,pokLess,pokLessEqual,pokGreater,pokGreaterEqual] then
  begin
    { These integer comparisons always yield Boolean. No conversion-based
      overload admission or runtime truth is asserted by that result contract. }
    Result.BooleanResult := True;
    Exit;
  end
  else
  begin
    LUnsignedLeft := not Signed(ALeft.Kind);
    LUnsignedRight := not Signed(ARight.Kind);
    case AOperation of
      pokShiftLeft,pokShiftRight:
        begin
          LKind := ALeft.Kind;

          if (CBits[LKind] <> 64) and (LKind <> pikLongWord) then
          begin

            if AALUBits >= 32 then LKind := AtWidth(32,Signed(LKind))
            else LKind := Common(LKind,AtWidth(AALUBits,Signed(LKind)),True,ACancellation);
          end
          else Result.Reuse := pirLeft;
        end;
      pokIntDivide,pokModulus:
        begin

          if LUnsignedRight and ((LUnsignedLeft and (CBits[ARight.Kind] >= CBits[ALeft.Kind])) or
            PositiveFits(ALeft.Constant,ARight.Kind)) then
          begin
            LKind := ARight.Kind;
            Result.Reuse := pirRight;
          end
          else if LUnsignedLeft and ((LUnsignedRight and (CBits[ALeft.Kind] >= CBits[ARight.Kind])) or
            PositiveFits(ARight.Constant,ALeft.Kind)) then
          begin
            LKind := ALeft.Kind;
            Result.Reuse := pirLeft;
          end
          else if (CBits[ALeft.Kind] = 64) or (CBits[ARight.Kind] = 64) then
            LKind := AtWidth(64,not (LUnsignedLeft and LUnsignedRight))
          else if (CBits[ALeft.Kind] > AALUBits) or (CBits[ARight.Kind] > AALUBits) then
            LKind := Common(ALeft.Kind,ARight.Kind,False,ACancellation)
          else if ((ALeft.Kind = LUInt) and not LUnsignedRight) or
            ((ARight.Kind = LUInt) and not LUnsignedLeft) then
            LKind := Common(LSInt,LUInt,False,ACancellation)
          else LKind := LSInt;

          if (LKind <> pikNone) and (CBits[LKind] < AALUBits) then
          begin
            LKind := LUInt;
            Result.Reuse := pirNone;
          end;
        end;
      pokAdd,pokSubtract,pokMultiply,pokAnd,pokOr,pokXor:
        begin

          if (AOperation = pokAnd) or ((AOperation in [pokOr,pokXor]) and
            (CBits[ALeft.Kind] <> 64) and (CBits[ARight.Kind] <> 64) and
            (LUnsignedLeft = LUnsignedRight)) then
          begin

            if LUnsignedRight and not LUnsignedLeft and
              ((CBits[ARight.Kind] >= AALUBits) or (CBits[ARight.Kind] = 32)) and
              (CBits[ARight.Kind] >= CBits[ALeft.Kind]) then
            begin LKind := ARight.Kind; Result.Reuse := pirRight; end
            else if LUnsignedLeft and not LUnsignedRight and
              ((CBits[ALeft.Kind] >= AALUBits) or (CBits[ALeft.Kind] = 32)) and
              (CBits[ALeft.Kind] >= CBits[ARight.Kind]) then
            begin LKind := ALeft.Kind; Result.Reuse := pirLeft; end
            else LKind := Common(ALeft.Kind,ARight.Kind,True,ACancellation);
          end
          else if (ALeft.Kind = pikInt64) or (ARight.Kind = pikInt64) then LKind := pikInt64
          else if (ALeft.Kind = pikQWord) or (ARight.Kind = pikQWord) then LKind := pikQWord
          else if (CBits[ALeft.Kind] > AALUBits) or (CBits[ARight.Kind] > AALUBits) then
            LKind := Common(ALeft.Kind,ARight.Kind,False,ACancellation)
          else if (ALeft.Kind = LUInt) or (ARight.Kind = LUInt) then
          begin
            LUnsignedLeft := LUnsignedLeft or PositiveFits(ALeft.Constant,LUInt);
            LUnsignedRight := LUnsignedRight or PositiveFits(ARight.Constant,LUInt);

            if not LUnsignedLeft or not LUnsignedRight or (AOperation = pokSubtract) then
              LKind := Common(LSInt,LUInt,False,ACancellation)
            else
            begin
              LKind := LUInt;

              if ALeft.Kind = LUInt then Result.Reuse := pirLeft else Result.Reuse := pirRight;
            end;
          end
          else if not LUnsignedLeft or not LUnsignedRight or (AOperation = pokSubtract) then LKind := LSInt
          else LKind := LUInt;
        end;
    else Exit;
    end;

    if LKind = pikNone then Exit;

    if LConstant then
    begin
      { Refuse conversions that would change either constant's mathematical
        value. Source casts/wrap and compiler overflow diagnostics have separate
        owners; evaluating an unconverted value would invent a false proof. }
      if not Fits(ALeft.Constant,LKind) or
        (not (AOperation in [pokShiftLeft,pokShiftRight]) and not Fits(ARight.Constant,LKind)) then Exit;
      case AOperation of
        pokAdd: LOperation := pvoAdd;
        pokSubtract: LOperation := pvoSubtract;
        pokMultiply: LOperation := pvoMultiply;
        pokIntDivide: LOperation := pvoDivide;
        pokModulus: LOperation := pvoRemainder;
        pokAnd: LOperation := pvoAnd;
        pokOr: LOperation := pvoOr;
        pokXor: LOperation := pvoXor;
        pokShiftLeft: LOperation := pvoShiftLeft64;
        pokShiftRight: LOperation := pvoShiftRight64;
      else Exit;
      end;
      NyxPascalAdmitAnalysis(ACancellation,papConstantStep);
      LValue := ALeft.Constant.Operate(LOperation,ARight.Constant);
      LKind := Narrow(LValue,ACancellation);
      Result.Reuse := pirNone;
    end;
  end;

  if LConstant and (LValue.Kind <> pvvInteger) then Exit;
  Result.Kind := LKind;
  Result.Constant := LValue;
end;

end.
