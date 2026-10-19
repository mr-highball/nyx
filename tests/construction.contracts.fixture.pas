{ Handwritten compiler fixture: source inspection never invokes these bodies.
  All created instances are manually owned by CompilerConstructorValue. }
unit construction.contracts.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Ordinary constructor has no authored function result. Both overloads still
    construct the receiver's class, independently of argument selection. }
  TReceiverBadge = class
  public
    Value: LongInt;
    constructor Create; reintroduce; overload;
    constructor Create(AValue: LongInt); reintroduce; overload;
    constructor Create(AValue: Boolean); reintroduce; overload;
    function WithValue(AValue: LongInt): TReceiverBadge;
  end;
  { Inherited construction must retain this descendant's additional members. }
  TReceiverSpecial = class(TReceiverBadge)
  public
    Accent: LongInt;
  end;
  { A transparent spelling must not erase the constructed descendant. }
  TReceiverAlias = TReceiverSpecial;
  { The static result of a metaclass call is its declared instance contract. }
  TReceiverClass = class of TReceiverBadge;
  { Constructor-owned fields retain the concrete generic receiver's frame. }
  TReceiverBox<T> = class
  public
    Value: T;
    constructor Create(const AValue: T); reintroduce;
  end;
  { The inherited member has a base frame while the result remains descendant. }
  TReceiverBadgeBox = class(TReceiverBox<TReceiverBadge>)
  public
    Extra: LongInt;
  end;
  { Advanced-record construction produces a value rather than a class instance. }
  TReceiverPoint = record
    Value: LongInt;
    constructor Create(AValue: LongInt);
  end;
  { Legacy object initialization remains a statement, not a class factory. }
  TReceiverLegacy = object
    Value: LongInt;
    constructor Init(AValue: LongInt);
  end;
  { Automatic class initialization must never become an ordinary value factory. }
  TReceiverStartup = class
    class constructor Start;
    class destructor Stop;
  end;

{ A function returning a metaclass also exercises grouped/postfix receivers. }
function ReceiverClass: TReceiverClass;
{ Native invocation count makes accidental execution during analysis visible. }
function ConstructorInvocations: LongInt;
{ Exported bridge keeps generic templates independent of private unit storage. }
procedure RecordConstructorInvocation;
{ Real compiler execution independently validates the fixture's result types. }
function CompilerConstructorValue: LongInt;

implementation

var
  GInvocations: LongInt;

constructor TReceiverBadge.Create;
begin
  inherited Create;
  Inc(GInvocations);
  Value := 42;
end;

constructor TReceiverBadge.Create(AValue: LongInt);
begin
  inherited Create;
  Inc(GInvocations);
  Value := AValue;
end;

constructor TReceiverBadge.Create(AValue: Boolean);
begin
  inherited Create;
  Inc(GInvocations);
  Value := Ord(AValue);
end;

function TReceiverBadge.WithValue(AValue: LongInt): TReceiverBadge;
begin
  Value := AValue;
  Result := Self;
end;

constructor TReceiverBox<T>.Create(const AValue: T);
begin
  inherited Create;
  RecordConstructorInvocation;
  Value := AValue;
end;

constructor TReceiverPoint.Create(AValue: LongInt);
begin
  Inc(GInvocations);
  Value := AValue;
end;

constructor TReceiverLegacy.Init(AValue: LongInt);
begin
  Inc(GInvocations);
  Value := AValue;
end;

class constructor TReceiverStartup.Start;
begin
end;

class destructor TReceiverStartup.Stop;
begin
end;

function ReceiverClass: TReceiverClass;
begin
  Result := TReceiverSpecial;
end;

function ConstructorInvocations: LongInt;
begin
  Result := GInvocations;
end;

procedure RecordConstructorInvocation;
begin
  Inc(GInvocations);
end;

function CompilerConstructorValue: LongInt;
var
  LSpecial: TReceiverSpecial;
  LImplicit: TReceiverSpecial;
  LBadge: TReceiverBadge;
  LBox: TReceiverBox<TReceiverBadge>;
  LChild: TReceiverBadgeBox;
  LMeta: TReceiverClass;
  LPoint: TReceiverPoint;
  LLegacy: TReceiverLegacy;
begin
  Result := 0;
  LSpecial := TReceiverAlias.Create(42);
  LImplicit := nil;
  LBadge := nil;
  LBox := nil;
  LChild := nil;
  try
    LSpecial.Accent := LSpecial.Value;
    LImplicit := TReceiverSpecial.Create;
    LMeta := ReceiverClass;
    LBadge := LMeta.Create(42);
    LBox := TReceiverBox<TReceiverBadge>.Create(LBadge);
    LChild := TReceiverBadgeBox.Create(LBadge);
    LChild.Extra := LChild.Value.Value;
    LPoint := TReceiverPoint.Create(42);
    LLegacy.Init(42);

    if (LSpecial.Accent = 42) and (LImplicit.Value = 42) and (LBox.Value.Value = 42) and
      (LChild.Extra = 42) and (LPoint.Value = 42) and (LLegacy.Value = 42) then
      Result := LBadge.WithValue(42).Value;
  finally
    LChild.Free;
    LBox.Free;
    LBadge.Free;
    LSpecial.Free;
    LImplicit.Free;
  end;
end;

end.
