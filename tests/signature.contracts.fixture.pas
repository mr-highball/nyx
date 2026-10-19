{ Whole handwritten compiler witness for source-owned routine signatures.
  Semantic readers inspect this same unit without invoking any of its bodies. }
unit signature.contracts.fixture;
{$mode delphi}{$H+}{$codepage utf8}
interface

type
  { An ordinary callback retains its own signature behind a named alias. }
  TSignatureStep = function(AValue: LongInt): LongInt;
  { Methods exercise grouped names, defaults, callback aliases and every
    authored parameter mode, including explicitly untyped storage arguments. }
  TSignatureBadge = class
  public
    constructor Create(AValue: LongInt = 42);
    function Combine(const ALeft, ARight: LongInt; AExtra: LongInt = 0): LongInt;
    function Call(AValue: LongInt; AStep: TSignatureStep = nil): LongInt;
    class procedure Touch(var AValue; const AInput; out AResult: LongInt;
      constref AOffset: LongInt); static;
  end;
  { A nongeneric method in a generic class retains its outer type parameter.
    The optional Boolean has a separate source default, not a value for T. }
  TSignatureBox<T> = class
  public
    function Replace(const AValue: T; ACommit: Boolean = True): T;
  end;

function Grouped(AFirst, ASecond: LongInt): LongInt;
function Describe(const AText: AnsiString = 'A;B)'; AMarker: LongInt = 7): LongInt;
function Identity<T>(const AValue: T): T;
{ Generic templates refer to this public bridge rather than private storage. }
procedure RecordSignatureInvocation;
function SignatureInvocations: LongInt;
function SignatureFixtureValue: LongInt;

implementation

var
  GInvocations: LongInt;

procedure RecordSignatureInvocation;
begin
  Inc(GInvocations);
end;

function SignatureInvocations: LongInt;
begin
  Result := GInvocations;
end;

constructor TSignatureBadge.Create(AValue: LongInt);
begin
  inherited Create;
  RecordSignatureInvocation;
end;

function TSignatureBadge.Combine(const ALeft, ARight: LongInt; AExtra: LongInt): LongInt;
begin
  RecordSignatureInvocation;
  Result := ALeft + ARight + AExtra;
end;

function TSignatureBadge.Call(AValue: LongInt; AStep: TSignatureStep): LongInt;
begin
  RecordSignatureInvocation;
  Result := AValue;

  if Assigned(AStep) then Result := AStep(Result);
end;

class procedure TSignatureBadge.Touch(var AValue; const AInput; out AResult: LongInt;
  constref AOffset: LongInt);
begin
  RecordSignatureInvocation;
  AResult := 42 + AOffset;
end;

function TSignatureBox<T>.Replace(const AValue: T; ACommit: Boolean): T;
begin
  RecordSignatureInvocation;
  Result := AValue;
end;

function Grouped(AFirst, ASecond: LongInt): LongInt;
begin
  RecordSignatureInvocation;
  Result := AFirst + ASecond;
end;

function Describe(const AText: AnsiString; AMarker: LongInt): LongInt;
begin
  RecordSignatureInvocation;
  Result := 42;
end;

function Identity<T>(const AValue: T): T;
begin
  RecordSignatureInvocation;
  Result := AValue;
end;

function SignatureFixtureValue: LongInt;
var
  LBadge: TSignatureBadge;
  LBox: TSignatureBox<TSignatureBadge>;
  LValue, LOffset: LongInt;
begin
  LBadge := TSignatureBadge.Create;
  LBox := TSignatureBox<TSignatureBadge>.Create;
  try
    LValue := Grouped(20, 22);
    LValue := LBadge.Combine(40, 2);
    LValue := LBadge.Call(LValue);
    LValue := Describe;
    LOffset := 0;
    TSignatureBadge.Touch(LValue, LValue, LValue, LOffset);
    LBadge := Identity<TSignatureBadge>(LBadge);

    if LBox.Replace(LBadge) = LBadge then Result := LValue else Result := 0;
  finally
    LBox.Free;
    LBadge.Free;
  end;
end;

end.
