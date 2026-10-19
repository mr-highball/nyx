{ Whole handwritten operator and address expressions. Native execution and
  source-only result queries share these declarations without sharing execution. }
unit operator.expressions.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
interface

type
  { Named fluent result exposes its real field to completion/navigation. }
  TOperatorBadge = class
  public
    Value: LongInt;
  end;
  { A different overload's result must not contaminate an exact operand match. }
  TOtherOperatorBadge = class
  public
    Value: LongInt;
  end;
  { Operator declarations belong to this exact record. Multiplication and
    unary negation retain the record; addition deliberately returns a class. }
  TOperatorValue = record
  public
    Badge: TOperatorBadge;
    class operator +(const ALeft, ARight: TOperatorValue): TOperatorBadge;
    class operator +(const ALeft: TOperatorValue; ARight: LongInt): TOtherOperatorBadge;
    class operator *(const ALeft, ARight: TOperatorValue): TOperatorValue;
    class operator -(const AValue: TOperatorValue): TOperatorValue;
    class operator =(const ALeft, ARight: TOperatorValue): Boolean;
  end;

{ Generic templates and source inspection use only exported declarations; the
  private counter is never referenced by a caller's semantic query. }
procedure RecordOperatorInvocation;
function OperatorInvocations: LongInt;
function NativeOperatorValue: LongInt;
function KeepOperator<T>(const AValue: T): T;

implementation

var
  GInvocations: LongInt;

procedure RecordOperatorInvocation;
begin
  Inc(GInvocations);
end;

function OperatorInvocations: LongInt;
begin
  Result := GInvocations;
end;

function KeepOperator<T>(const AValue: T): T;
begin
  RecordOperatorInvocation;
  Result := AValue;
end;

class operator TOperatorValue.+(const ALeft, ARight: TOperatorValue): TOperatorBadge;
begin
  RecordOperatorInvocation;
  Result := ALeft.Badge;
end;

class operator TOperatorValue.+(const ALeft: TOperatorValue; ARight: LongInt): TOtherOperatorBadge;
begin
  RecordOperatorInvocation;
  Result := TOtherOperatorBadge.Create;
  Result.Value := ALeft.Badge.Value;
end;

class operator TOperatorValue.*(const ALeft, ARight: TOperatorValue): TOperatorValue;
begin
  RecordOperatorInvocation;
  Result := ALeft;
end;

class operator TOperatorValue.-(const AValue: TOperatorValue): TOperatorValue;
begin
  RecordOperatorInvocation;
  Result := AValue;
end;

class operator TOperatorValue.=(const ALeft, ARight: TOperatorValue): Boolean;
begin
  RecordOperatorInvocation;
  Result := ALeft.Badge = ARight.Badge;
end;

function NativeOperatorValue: LongInt;
var
  LBadge: TOperatorBadge;
  LLeft, LRight: TOperatorValue;
  LAddress: ^TOperatorValue;
  LNumber: LongInt;
  LOther: TOtherOperatorBadge;
  LEnabled, LReady: Boolean;
begin
  LBadge := TOperatorBadge.Create;
  try
    LBadge.Value := 42;
    LLeft.Badge := LBadge;
    LRight := LLeft;
    Result := (LLeft + LRight).Value;
    Result := (LLeft * LRight + LRight).Value;
    Result := (-LLeft + LRight).Value;

    if LLeft = LRight then Result := LBadge.Value;
    LNumber := 1;
    LOther := LLeft + LNumber;
    try
      Result := LOther.Value;
    finally
      LOther.Free;
    end;
    LEnabled := True;
    LReady := False;

    if (LEnabled and not LReady) or (LReady xor LEnabled) then Result := LBadge.Value;
    Result := KeepOperator(LLeft + LRight).Value;
    {$T+}
    LAddress := @LLeft;
    Result := LAddress^.Badge.Value;
    Result := (@LLeft)^.Badge.Value;
    {$T-}
  finally
    LBadge.Free;
  end;
end;

end.
