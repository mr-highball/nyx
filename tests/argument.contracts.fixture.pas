{ Whole handwritten calls establish the compiler's source contracts independently
  of Nyx's result-type queries. Non-deduction arguments never decide T. }
unit argument.contracts.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
interface

type
  { Named result identity makes the recovered fluent member independently
    observable through native execution and exact source navigation. }
  TArgumentBadge = class
  public
    Value: LongInt;
  end;
  { A named generic result exposes the deduced argument through its real field. }
  TArgumentBox<T> = class
  public
    Value: T;
  end;

function WithText<T>(const AValue: T; const AText: UnicodeString): TArgumentBox<T>;
function WithNumbers<T>(const AValue: T; AReal: Double; APointer: Pointer): TArgumentBox<T>;
function WithFlag<T>(const AValue: T; AFlag: Boolean): TArgumentBox<T>;
function NeedBoth<T, U>(const AValue: T; const AOther: U): TArgumentBox<U>;
{ Exported invocation bridge keeps generic templates independent of private
  storage. Reading source must not invoke any of these application functions. }
procedure RecordArgumentInvocation;
function ArgumentInvocations: LongInt;
function NativeArgumentValue: LongInt;

implementation

var
  GInvocations: LongInt;

procedure RecordArgumentInvocation;
begin
  Inc(GInvocations);
end;

function ArgumentInvocations: LongInt;
begin
  Result := GInvocations;
end;

function WithText<T>(const AValue: T; const AText: UnicodeString): TArgumentBox<T>;
begin
  RecordArgumentInvocation;
  Result := TArgumentBox<T>.Create;
  Result.Value := AValue;
end;

function WithNumbers<T>(const AValue: T; AReal: Double; APointer: Pointer): TArgumentBox<T>;
begin
  RecordArgumentInvocation;
  Result := TArgumentBox<T>.Create;
  Result.Value := AValue;
end;

function WithFlag<T>(const AValue: T; AFlag: Boolean): TArgumentBox<T>;
begin
  RecordArgumentInvocation;
  Result := TArgumentBox<T>.Create;
  Result.Value := AValue;
end;

function NeedBoth<T, U>(const AValue: T; const AOther: U): TArgumentBox<U>;
begin
  RecordArgumentInvocation;
  Result := TArgumentBox<U>.Create;
  Result.Value := AOther;
end;

function NativeArgumentValue: LongInt;
var
  LBadge: TArgumentBadge;
  LBox: TArgumentBox<TArgumentBadge>;
begin
  LBadge := TArgumentBadge.Create;
  try
    LBadge.Value := 42;
    LBox := WithText(LBadge, 'λ');
    try
      Result := LBox.Value.Value;
    finally
      LBox.Free;
    end;
    LBox := WithText(LBadge, (('a' + #0 + 'b')));
    try
      Result := LBox.Value.Value;
    finally
      LBox.Free;
    end;
    LBox := WithNumbers(LBadge, -(1.5 + 2.5), nil);
    try
      Result := LBox.Value.Value;
    finally
      LBox.Free;
    end;
    LBox := WithFlag(LBadge, (1 < 2) and not False);
    try
      Result := LBox.Value.Value;
    finally
      LBox.Free;
    end;
  finally
    LBadge.Free;
  end;
end;

end.
