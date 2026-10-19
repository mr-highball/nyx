{ Independent native witness: static cast contracts never replace compiler or
  runtime checks. The source reader must not execute this unit's operands. }
unit cast.contracts.fixture;
{$mode delphi}{$H+}{$interfaces corba}
interface

type
  { A specialized interface exposes a typed result after a checked assertion. }
  ICastValue = interface
    ['{DD4A1EC2-63C6-4269-B829-E0734102DE80}']
    function ReadValue: LongInt;
  end;
  { Runtime storage provides both explicit class and checked interface views. }
  TCastValue = class(TObject, ICastValue)
    Value: LongInt;
    function ReadValue: LongInt;
  end;
  { A generic explicit cast retains the class argument in its member result. }
  TCastBox<T> = class
    Item: T;
    function ReadItem: T;
  end;
  { A declared pointer cast retains its target without granting memory access. }
  PCastValue = ^TCastValue;

function CastOperandCalls: LongInt;
function CastOperand: TObject;
function CompilerCastValue: LongInt;

implementation

var
  GValue: TCastValue;
  GCalls: LongInt;

function TCastValue.ReadValue: LongInt;
begin
  Result := Value;
end;

function TCastBox<T>.ReadItem: T;
begin
  Result := Item;
end;

function CastOperandCalls: LongInt;
begin
  Result := GCalls;
end;

function CastOperand: TObject;
begin
  Inc(GCalls);
  Result := GValue;
end;

function CompilerCastValue: LongInt;
var
  LBox: TCastBox<TCastValue>;
  LBase: TObject;
  LAddress: Pointer;
begin
  GValue := TCastValue.Create;
  LBox := TCastBox<TCastValue>.Create;
  try
    GValue.Value := 7;
    LBox.Item := GValue;
    LBase := LBox;
    LAddress := @GValue;
    Result := TCastValue(CastOperand).ReadValue +
      (CastOperand as TCastValue).ReadValue +
      (CastOperand as ICastValue).ReadValue +
      (TCastBox<TCastValue>(LBase)).ReadItem.ReadValue +
      (LBase as TCastBox<TCastValue>).ReadItem.ReadValue +
      PCastValue(LAddress)^.ReadValue;
  finally
    LBox.Free;
    GValue.Free;
    GValue := nil;
  end;
end;

end.
