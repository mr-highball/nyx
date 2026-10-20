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
unit nyx.pascal.ieee;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.pascal.reals, nyx.scheduler;

type
  { Abstract IEEE values, independent of any machine bit layout. NaN records
    unordered meaning only; its sign, payload and signaling state are unproved. }
  TNyxPascalIEEEKind = (pifUnknown, pifFinite, pifInfinity, pifNaN);
  { Unknown supplies no relation. Unordered is a known NaN relation and must not
    be confused with equality or a missing compiler fact. }
  TNyxPascalIEEEOrder = (pieoUnknown, pieoLess, pieoEqual, pieoGreater, pieoUnordered);
  { Detached mathematical IEEE value. Finite data uses the existing exact binary
    owner; infinity has a meaningful sign and explicit format. NaN has neither
    finite data nor a projectable sign/payload. No mutable workspace, source,
    compiler policy, exception mask or execution scope survives publication. }
  TNyxPascalIEEEValue = record
  private
    FKind: TNyxPascalIEEEKind;
    FPrecision: TNyxPascalBinaryPrecision;
    FNegative: Boolean;
    FFinite: TNyxPascalBinaryReal;
  public
    class function Unknown: TNyxPascalIEEEValue; static;
    class function Finite(const AValue: TNyxPascalBinaryReal): TNyxPascalIEEEValue; static;
    class function Infinity(APrecision: TNyxPascalBinaryPrecision;
      ANegative: Boolean = False): TNyxPascalIEEEValue; static;
    class function NotANumber(APrecision: TNyxPascalBinaryPrecision): TNyxPascalIEEEValue; static;
    function FiniteValue: TNyxPascalBinaryReal;
    function Negative: Boolean;
    function IsZero: Boolean;
    function Negated: TNyxPascalIEEEValue;
    { Explicit permissive IEEE rounding turns a finite-format overflow into an
      infinity. This is not proof that a compiler admits the conversion. }
    function RoundToEven(APrecision: TNyxPascalBinaryPrecision;
      const ACancellation: INyxCancellationScope = nil): TNyxPascalIEEEValue;
    function Compare(const AOther: TNyxPascalIEEEValue;
      const ACancellation: INyxCancellationScope = nil): TNyxPascalIEEEOrder;
    { One permissive IEEE operation. Finite arithmetic delegates to exact
      rational rounding; special values use abstract IEEE rules. No host float,
      guessed NaN bits or compiler diagnostic/trap admission is involved. }
    function Operate(AOperation: TNyxPascalRealOperation;
      const AOther: TNyxPascalIEEEValue; APrecision: TNyxPascalBinaryPrecision;
      const ACancellation: INyxCancellationScope = nil): TNyxPascalIEEEValue;
    property Kind: TNyxPascalIEEEKind read FKind;
    property Precision: TNyxPascalBinaryPrecision read FPrecision;
  end;

implementation

uses nyx.pascal.values, nyx.pascal.work, nyx.pascal.lexing;

procedure Admit(const AScope: INyxCancellationScope);
begin

  if (AScope <> nil) and AScope.Cancelled then
    raise ENyxPascalAnalysisCancelled.Create('Pascal IEEE-value analysis was cancelled');
  NyxPascalAdmitAnalysis(AScope,papConstantStep);
end;

procedure ValidatePrecision(APrecision: TNyxPascalBinaryPrecision);
begin

  if (Ord(APrecision) < Ord(Low(TNyxPascalBinaryPrecision))) or
    (Ord(APrecision) > Ord(High(TNyxPascalBinaryPrecision))) then
    raise ENyxPascalValue.Create('An IEEE precision is outside its closed domain');
end;

class function TNyxPascalIEEEValue.Unknown: TNyxPascalIEEEValue;
begin
  Result := Default(TNyxPascalIEEEValue);
end;

class function TNyxPascalIEEEValue.Finite(const AValue: TNyxPascalBinaryReal): TNyxPascalIEEEValue;
begin
  Result := Unknown;

  if AValue.Status <> rrsFinite then Exit;
  Result.FKind := pifFinite; Result.FPrecision := AValue.Precision; Result.FFinite := AValue;
end;

class function TNyxPascalIEEEValue.Infinity(APrecision: TNyxPascalBinaryPrecision;
  ANegative: Boolean): TNyxPascalIEEEValue;
begin
  ValidatePrecision(APrecision); Result := Unknown;

  if APrecision = pbpUnknown then Exit;
  Result.FKind := pifInfinity; Result.FPrecision := APrecision; Result.FNegative := ANegative;
end;

class function TNyxPascalIEEEValue.NotANumber(APrecision: TNyxPascalBinaryPrecision): TNyxPascalIEEEValue;
begin
  ValidatePrecision(APrecision); Result := Unknown;

  if APrecision = pbpUnknown then Exit;
  Result.FKind := pifNaN; Result.FPrecision := APrecision;
end;

function TNyxPascalIEEEValue.FiniteValue: TNyxPascalBinaryReal;
begin

  if FKind <> pifFinite then raise ENyxPascalValue.Create('This IEEE value has no finite representation');
  Result := FFinite;
end;

function TNyxPascalIEEEValue.Negative: Boolean;
begin
  case FKind of
    pifFinite: Result := FFinite.Negative;
    pifInfinity: Result := FNegative;
  else raise ENyxPascalValue.Create('This IEEE value supplies no sign proof');
  end;
end;

function TNyxPascalIEEEValue.IsZero: Boolean;
begin
  Result := (FKind = pifFinite) and FFinite.IsZero;
end;

function TNyxPascalIEEEValue.Negated: TNyxPascalIEEEValue;
begin
  Result := Self;
  case FKind of
    pifFinite: Result.FFinite := FFinite.Negated;
    pifInfinity: Result.FNegative := not FNegative;
  else begin end;
  end;
end;

{ Failed finite operations become IEEE values only at this explicit boundary.
  Wrapping a binary error with Finite above never grants that authority. }
function IEEEOutcome(const AValue: TNyxPascalBinaryReal): TNyxPascalIEEEValue;
begin
  case AValue.Status of
    rrsFinite: Result := TNyxPascalIEEEValue.Finite(AValue);
    rrsOverflow, rrsDivisionByZero:
      Result := TNyxPascalIEEEValue.Infinity(AValue.Precision,AValue.Negative);
    rrsInvalidOperation: Result := TNyxPascalIEEEValue.NotANumber(AValue.Precision);
  else Result := TNyxPascalIEEEValue.Unknown;
  end;
end;

function TNyxPascalIEEEValue.RoundToEven(APrecision: TNyxPascalBinaryPrecision;
  const ACancellation: INyxCancellationScope): TNyxPascalIEEEValue;
begin
  Admit(ACancellation); ValidatePrecision(APrecision); Result := Unknown;

  if APrecision = pbpUnknown then Exit;
  case FKind of
    pifFinite: Result := IEEEOutcome(FFinite.RoundToEven(APrecision,ACancellation));
    pifInfinity: Result := Infinity(APrecision,FNegative);
    pifNaN: Result := NotANumber(APrecision);
  else begin end;
  end;
end;

function TNyxPascalIEEEValue.Compare(const AOther: TNyxPascalIEEEValue;
  const ACancellation: INyxCancellationScope): TNyxPascalIEEEOrder;
var LOrder: Integer;
begin
  Admit(ACancellation); Result := pieoUnknown;

  if (FKind = pifUnknown) or (AOther.FKind = pifUnknown) then Exit;

  if (FKind = pifNaN) or (AOther.FKind = pifNaN) then Exit(pieoUnordered);

  if (FKind = pifInfinity) or (AOther.FKind = pifInfinity) then
  begin

    if (FKind = pifInfinity) and (AOther.FKind = pifInfinity) then
    begin

      if FNegative = AOther.FNegative then Exit(pieoEqual);

      if FNegative then Exit(pieoLess) else Exit(pieoGreater);
    end;

    if FKind = pifInfinity then
    begin

      if FNegative then Exit(pieoLess) else Exit(pieoGreater);
    end;

    if AOther.FNegative then Exit(pieoGreater) else Exit(pieoLess);
  end;

  if not FFinite.TryCompare(AOther.FFinite,LOrder,ACancellation) then Exit;

  if LOrder < 0 then Result := pieoLess
  else if LOrder > 0 then Result := pieoGreater
  else Result := pieoEqual;
end;

function TNyxPascalIEEEValue.Operate(AOperation: TNyxPascalRealOperation;
  const AOther: TNyxPascalIEEEValue; APrecision: TNyxPascalBinaryPrecision;
  const ACancellation: INyxCancellationScope): TNyxPascalIEEEValue;
var LRight: TNyxPascalIEEEValue; LZero: TNyxPascalBinaryReal;
begin
  Admit(ACancellation); ValidatePrecision(APrecision); Result := Unknown;

  if (Ord(AOperation) < Ord(Low(TNyxPascalRealOperation))) or
    (Ord(AOperation) > Ord(High(TNyxPascalRealOperation))) then
    raise ENyxPascalValue.Create('An IEEE operation is outside its closed domain');

  if (APrecision = pbpUnknown) or (FKind = pifUnknown) or (AOther.FKind = pifUnknown) then Exit;

  if (FKind = pifNaN) or (AOther.FKind = pifNaN) then Exit(NotANumber(APrecision));

  if (FKind = pifFinite) and (AOther.FKind = pifFinite) then
    Exit(IEEEOutcome(NyxPascalCalculateRealToEven(AOperation,FFinite,AOther.FFinite,APrecision,ACancellation)));
  case AOperation of
    proAdd, proSubtract:
      begin
        LRight := AOther;

        if AOperation = proSubtract then LRight := LRight.Negated;

        if (FKind = pifInfinity) and (LRight.FKind = pifInfinity) and
          (FNegative <> LRight.FNegative) then Exit(NotANumber(APrecision));

        if FKind = pifInfinity then Exit(Infinity(APrecision,FNegative));
        Result := Infinity(APrecision,LRight.FNegative);
      end;
    proMultiply:
      begin

        if IsZero or AOther.IsZero then Exit(NotANumber(APrecision));
        Result := Infinity(APrecision,Negative <> AOther.Negative);
      end;
    proDivide:
      begin

        if (FKind = pifInfinity) and (AOther.FKind = pifInfinity) then Exit(NotANumber(APrecision));

        if FKind = pifInfinity then Exit(Infinity(APrecision,Negative <> AOther.Negative));
        LZero := NyxPascalRoundRealToEven(NyxPascalRealToken('0.0',ACancellation),APrecision,ACancellation);

        if Negative <> AOther.Negative then LZero := LZero.Negated;
        Result := Finite(LZero);
      end;
  end;
end;

end.
