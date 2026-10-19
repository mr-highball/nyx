{ Whole handwritten generic application fixture. The native compiler establishes
  legal calls independently; semantic inspection never executes these bodies. }
unit application.generics.fixture;
{$mode delphi}{$H+}{$codepage utf8}
interface

type
  { Fluent class result provides an ordinary member after generic application. }
  TAppliedBadge = class
  public
    Value: LongInt;
    function WithValue(AValue: LongInt): TAppliedBadge;
    function Choose<T>(const AValue: T): T;
  end;
  { Constructed return values retain the explicit routine argument as their own
    class frame. The caller owns the box and its separately owned class value. }
  TAppliedBox<T> = class
  public
    Value: T;
  end;
  { Nongeneric receiver still permits explicitly generic static methods when
    accepted by the configured compiler; no outer generic frame is inferred. }
  TAppliedCalls = class
  public
    class function Keep<T>(const AValue: T): T; static;
  end;
  { An interface result exercises compiler-managed lifetime through a generic
    function without coercing that result to a concrete implementing class. }
  IAppliedValue = interface
    function ReadValue: LongInt;
  end;

function KeepValue<T>(const AValue: T): T;
function BoxValue<T>(const AValue: T): TAppliedBox<T>;
function KeepBadge<T: TAppliedBadge>(const AValue: T): T;
function RetypeValue<T, U>(const AValue: T; const AOther: U): TAppliedBox<U>;
function CommonValue<T>(const AValue: T; AMarker: LongInt): T; overload;
function CommonValue<T>(const AValue: T; AMarker: Boolean): T; overload;
function DifferentValue<T>(const AValue: T; AMarker: LongInt): T; overload;
function DifferentValue<T>(const AValue: T; AMarker: Boolean): TAppliedBox<T>; overload;
{ Exported bridge keeps generic templates independent of private unit storage. }
procedure RecordApplicationInvocation;
function ApplicationInvocations: LongInt;
function CompilerApplicationValue: LongInt;

implementation

type
  { Concrete reference-counted implementation is hidden behind IAppliedValue;
    its implementation identity must never become the generic result contract. }
  TAppliedValue = class(TInterfacedObject, IAppliedValue)
  public
    function ReadValue: LongInt;
  end;

var
  GInvocations: LongInt;

procedure RecordApplicationInvocation;
begin
  Inc(GInvocations);
end;

function ApplicationInvocations: LongInt;
begin
  Result := GInvocations;
end;

function KeepValue<T>(const AValue: T): T;
begin
  RecordApplicationInvocation;
  Result := AValue;
end;

function BoxValue<T>(const AValue: T): TAppliedBox<T>;
begin
  RecordApplicationInvocation;
  Result := TAppliedBox<T>.Create;
  Result.Value := AValue;
end;

function KeepBadge<T>(const AValue: T): T;
begin
  RecordApplicationInvocation;
  Result := AValue;
end;

function RetypeValue<T, U>(const AValue: T; const AOther: U): TAppliedBox<U>;
begin
  RecordApplicationInvocation;
  Result := TAppliedBox<U>.Create;
  Result.Value := AOther;
end;

function CommonValue<T>(const AValue: T; AMarker: LongInt): T;
begin
  RecordApplicationInvocation;
  Result := AValue;
end;

function CommonValue<T>(const AValue: T; AMarker: Boolean): T;
begin
  RecordApplicationInvocation;
  Result := AValue;
end;

function DifferentValue<T>(const AValue: T; AMarker: LongInt): T;
begin
  RecordApplicationInvocation;
  Result := AValue;
end;

function DifferentValue<T>(const AValue: T; AMarker: Boolean): TAppliedBox<T>;
begin
  RecordApplicationInvocation;
  Result := BoxValue<T>(AValue);
end;

class function TAppliedCalls.Keep<T>(const AValue: T): T;
begin
  RecordApplicationInvocation;
  Result := AValue;
end;

function TAppliedBadge.Choose<T>(const AValue: T): T;
begin
  RecordApplicationInvocation;
  Result := AValue;
end;

function TAppliedBadge.WithValue(AValue: LongInt): TAppliedBadge;
begin
  Value := AValue;
  Result := Self;
end;

function TAppliedValue.ReadValue: LongInt;
begin
  Result := 42;
end;

function CompilerApplicationValue: LongInt;
var
  LBadge, LResult: TAppliedBadge;
  LBox: TAppliedBox<TAppliedBadge>;
  LRetyped: TAppliedBox<TAppliedBadge>;
  LDifferent: TAppliedBox<TAppliedBadge>;
  LInterface, LRetained: IAppliedValue;
begin
  LBadge := TAppliedBadge.Create;
  LBox := nil;
  LRetyped := nil;
  LDifferent := nil;
  try
    LBadge.Value := 42;
    LResult := KeepValue<TAppliedBadge>(LBadge);
    LResult := KeepBadge<TAppliedBadge>(LResult);
    LBox := BoxValue<TAppliedBadge>(LResult);
    LResult := TAppliedCalls.Keep<TAppliedBadge>(LBox.Value);
    LResult := LBadge.Choose<TAppliedBadge>(LResult);
    LResult := CommonValue<TAppliedBadge>(LResult, 0);
    LResult := CommonValue<TAppliedBadge>(LResult, True);
    LRetyped := RetypeValue<LongInt, TAppliedBadge>(0, LResult);
    LResult := DifferentValue<TAppliedBadge>(LResult, 0);
    LDifferent := DifferentValue<TAppliedBadge>(LResult, True);
    LInterface := TAppliedValue.Create;
    LRetained := KeepValue<IAppliedValue>(LInterface);
    LInterface := nil;

    if LRetained.ReadValue <> 42 then Exit(0);
    Result := LRetyped.Value.WithValue(42).Value;
  finally
    LBox.Free;
    LRetyped.Free;
    LDifferent.Free;
    LBadge.Free;
  end;
end;

end.
