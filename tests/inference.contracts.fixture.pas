{ Whole native compiler witness for implicit generic calls. The same source
  has an independent expected-failure build proving PUSH/POP mode lifetime. }
unit inference.contracts.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
{$push}
{$modeswitch implicitfunctionspecialization-}
{$pop}
{$ifndef NYX_INFERENCE_AFTER_POP}
{$ifndef NYX_INFERENCE_INTERFACE_LOOKAHEAD}
{$modeswitch implicitfunctionspecialization+}
{$endif}
{$endif}
interface
{$ifdef NYX_INFERENCE_INTERFACE_LOOKAHEAD}
{$modeswitch implicitfunctionspecialization+}
{$endif}

type
  { Named class identity provides an observable fluent result after deduction. }
  TInferredBadge = class
    Value: LongInt;
  end;
  { A constructed result retains its explicit/inferred argument in a field. }
  TInferredBox<T> = class
    Value: T;
  end;
  { A generic callback alias supplies a type argument without invoking it. }
  TInferredStep<T> = function(AValue: T): T;
  { The receiver is nongeneric; generic methods inside a generic class remain
    the separately recorded unsupported compiler case. }
  TInferredMethods = class
    class function Keep<T>(const AValue: T): T; static;
    function Box<T>(const AValue: T): TInferredBox<T>;
  end;

function KeepValue<T>(const AValue: T): T;
function BoxValue<T>(const AValue: T): TInferredBox<T>;
function TakeValue<T>(const ABox: TInferredBox<T>): T;
function PairValue<T, U>(const AValue: T; const AOther: U): TInferredBox<U>;
function ConstrainedValue<T: TInferredBadge>(const AValue: T): T;
function OptionalValue<T>(const AValue: T; AMarker: LongInt = 0): T;
function CommonValue<T>(const AValue: T; AMarker: LongInt): T; overload;
function CommonValue<T>(const AValue: T; AMarker: Boolean): T; overload;
function CallbackValue<T>(const AStep: TInferredStep<T>): TInferredStep<T>;
function RepeatedValue<T>(const ALeft, ARight: T): T;
{ Exported bridge keeps generic templates independent of private storage. }
procedure RecordInferenceInvocation;
function InferenceInvocations: LongInt;
function NativeInferenceValue: LongInt;

implementation

{$ifdef NYX_INFERENCE_LATE_SWITCH}
{$modeswitch implicitfunctionspecialization-}
{$endif}

var
  GInvocations: LongInt;

procedure RecordInferenceInvocation;
begin
  Inc(GInvocations);
end;

function InferenceInvocations: LongInt;
begin
  Result := GInvocations;
end;

function KeepValue<T>(const AValue: T): T;
begin
  RecordInferenceInvocation;
  Result := AValue;
end;

function BoxValue<T>(const AValue: T): TInferredBox<T>;
begin
  RecordInferenceInvocation;
  Result := TInferredBox<T>.Create;
  Result.Value := AValue;
end;

function TakeValue<T>(const ABox: TInferredBox<T>): T;
begin
  RecordInferenceInvocation;
  Result := ABox.Value;
end;

function PairValue<T, U>(const AValue: T; const AOther: U): TInferredBox<U>;
begin
  RecordInferenceInvocation;
  Result := TInferredBox<U>.Create;
  Result.Value := AOther;
end;

function ConstrainedValue<T>(const AValue: T): T;
begin
  RecordInferenceInvocation;
  Result := AValue;
end;

function OptionalValue<T>(const AValue: T; AMarker: LongInt): T;
begin
  RecordInferenceInvocation;
  Result := AValue;
end;

function CommonValue<T>(const AValue: T; AMarker: LongInt): T;
begin
  RecordInferenceInvocation;
  Result := AValue;
end;

function CommonValue<T>(const AValue: T; AMarker: Boolean): T;
begin
  RecordInferenceInvocation;
  Result := AValue;
end;

function CallbackValue<T>(const AStep: TInferredStep<T>): TInferredStep<T>;
begin
  RecordInferenceInvocation;
  Result := AStep;
end;

function RepeatedValue<T>(const ALeft, ARight: T): T;
begin
  RecordInferenceInvocation;
  Result := ALeft;
end;

class function TInferredMethods.Keep<T>(const AValue: T): T;
begin
  RecordInferenceInvocation;
  Result := AValue;
end;

function TInferredMethods.Box<T>(const AValue: T): TInferredBox<T>;
begin
  RecordInferenceInvocation;
  Result := TInferredBox<T>.Create;
  Result.Value := AValue;
end;

function AfterPop(AValue: TInferredBadge): TInferredBadge;
begin
  Result := KeepValue<TInferredBadge>(AValue);
end;

function EchoBadge(AValue: TInferredBadge): TInferredBadge;
begin
  Result := AValue;
end;

function NativeInferenceValue: LongInt;
var
  LBadge, LResult: TInferredBadge;
  LBox, LPair, LMethodBox: TInferredBox<TInferredBadge>;
  LMethods: TInferredMethods;
  LStep, LRetained: TInferredStep<TInferredBadge>;
  LNumber: LongInt;
begin
  LBadge := TInferredBadge.Create;
  LMethods := TInferredMethods.Create;
  LBox := nil;
  LPair := nil;
  LMethodBox := nil;
  try
    LBadge.Value := 42;
    LNumber := 42;
    LResult := KeepValue(LBadge);
    LBox := BoxValue(LResult);
    LResult := TakeValue(LBox);
    LPair := PairValue(LNumber, LResult);
    LResult := TInferredMethods.Keep(LPair.Value);
    LMethodBox := LMethods.Box(LResult);
    LResult := ConstrainedValue(LMethodBox.Value);
    LResult := OptionalValue(LResult);
    LResult := CommonValue(LResult, True);
    LStep := @EchoBadge;
    LRetained := CallbackValue(LStep);
    LResult := LRetained(LResult);
    LResult := RepeatedValue(LResult, LBadge);
    LResult := AfterPop(LResult);
    Result := LResult.Value;
  finally
    LMethodBox.Free;
    LPair.Free;
    LBox.Free;
    LMethods.Free;
    LBadge.Free;
  end;
end;

end.
