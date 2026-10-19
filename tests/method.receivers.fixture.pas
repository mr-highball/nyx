{ Independent Delphi-mode compiler witness for implicit routine storage.
  This unit does not depend on Nyx: the maintained resolver tests also inspect
  equivalent source, so their expectations cannot substitute for compilation. }
unit method.receivers.fixture;

{$mode delphi}{$H+}

interface

type
  { A mutable instance exercises constructor, destructor and fluent function
    receivers, including lexical capture through nested routine bodies. }
  TMethodBadge = class
  public
    Value: LongInt;
    class var Shared: LongInt;
    constructor Create(AValue: LongInt);
    destructor Destroy; override;
    function Adjust: TMethodBadge;
    class function ReadShared: LongInt;
    class function Detached: LongInt; static;
  end;
  { The generic receiver and its function result keep the enclosing parameter;
    neither is a concrete specialization while the method is being declared. }
  TMethodBox<T> = class
  public
    Value: T;
    function Echo: TMethodBox<T>;
    function ReadValue: T;
  end;
  { Value-type methods have an instance Self without class-reference semantics. }
  TMethodRecord = record
    Value: LongInt;
    function Echo: TMethodRecord;
  end;
  { Old-style objects also own an implicit instance receiver. }
  TMethodObject = object
    Value: LongInt;
    function Echo: TMethodObject;
  end;

function MethodReceiverProof: LongInt;

implementation

constructor TMethodBadge.Create(AValue: LongInt);
begin
  inherited Create;
  Self.Value := AValue;
end;

destructor TMethodBadge.Destroy;
begin
  Self.Value := 0;
  inherited Destroy;
end;

function TMethodBadge.Adjust: TMethodBadge;

  procedure CaptureOuter;
  begin
    Result.Value := Self.Value + 1;
  end;

  function OwnResult: LongInt;
  begin
    Result := Self.Value + 1;
  end;

begin
  Result := Self;
  CaptureOuter;
  Result.Value := OwnResult;
end;

class function TMethodBadge.ReadShared: LongInt;
begin
  Result := Self.Shared;
end;

class function TMethodBadge.Detached: LongInt;
begin
  Result := 42;
end;

function TMethodBox<T>.Echo: TMethodBox<T>;
begin
  Result := Self;
end;

function TMethodBox<T>.ReadValue: T;
begin
  Result := Self.Value;
end;

function TMethodRecord.Echo: TMethodRecord;
begin
  Result := Self;
end;

function TMethodObject.Echo: TMethodObject;
begin
  Result := Self;
end;

function MethodReceiverProof: LongInt;
var
  LBadge: TMethodBadge;
  LBox: TMethodBox<TMethodBadge>;
  LRecord: TMethodRecord;
  LObject: TMethodObject;
begin
  LBadge := TMethodBadge.Create(3);
  LBox := TMethodBox<TMethodBadge>.Create;
  try
    LBox.Value := LBadge;
    LRecord.Value := 7;
    LObject.Value := 11;
    TMethodBadge.Shared := 19;
    Result := LBox.Echo.ReadValue.Adjust.Value + LRecord.Echo.Value +
      LObject.Echo.Value + TMethodBadge.ReadShared;

    if TMethodBadge.Detached <> 42 then
    begin
      Result := -1;
    end;
  finally
    LBox.Free;
    LBadge.Free;
  end;
end;

end.
