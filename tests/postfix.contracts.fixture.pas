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
unit postfix.contracts.fixture;
{$mode delphi}{$H+}{$codepage utf8}
interface

type
  { Real fluent result shared by both overloads. Instances are manually owned. }
  TPostfixBadge = class
  public
    Value: LongInt;
    function WithValue(AValue: LongInt): TPostfixBadge;
    function Choose(AValue: LongInt): TPostfixBadge; overload;
    function Choose(AValue: Boolean): TPostfixBadge; overload;
    class function Build: TPostfixBadge; static;
  end;
  { A different result deliberately prevents a common overload-result contract. }
  TPostfixOther = class
    OtherValue: LongInt;
  end;
  { Returned T must retain the concrete declaring receiver's argument frame. }
  TPostfixBox<T> = class
    Value: T;
    function ReadValue: T;
  end;
  { Pointer access is structural; the resolver never reads its target memory. }
  PPostfixBadge = ^TPostfixBadge;
  { Dynamic elements have a declared result independent of index evaluation. }
  TPostfixRows = array of TPostfixBadge;
  { Two authored dimensions must be consumed before the element is available. }
  TPostfixGrid = array[0..1, 0..1] of TPostfixBadge;
  { Nested arrays qualify both repeated and combined index syntax. }
  TPostfixNested = array of TPostfixRows;
  { Invoking a function value follows its result rather than executing it. }
  TPostfixFactory = function: TPostfixBadge;

function NewPostfixBadge: TPostfixBadge;
function VaryPostfix(AValue: LongInt): TPostfixBadge; overload;
function VaryPostfix(AValue: Boolean): TPostfixOther; overload;
{ The counter distinguishes independent runtime execution from source queries. }
function PostfixInvocations: LongInt;
{ An exported template may call this bridge without naming private storage. }
procedure RecordPostfixInvocation;
{ Compile and execute actual chained calls, generic returns, arrays and pointers. }
function CompilerPostfixValue: LongInt;

implementation

var
  GInvocations: LongInt;

function TPostfixBadge.WithValue(AValue: LongInt): TPostfixBadge;
begin
  Inc(GInvocations);
  Value := AValue;
  Result := Self;
end;

function TPostfixBadge.Choose(AValue: LongInt): TPostfixBadge;
begin
  Inc(GInvocations);
  Inc(Value, AValue);
  Result := Self;
end;

function TPostfixBadge.Choose(AValue: Boolean): TPostfixBadge;
begin
  Inc(GInvocations);

  if AValue then Inc(Value);
  Result := Self;
end;

class function TPostfixBadge.Build: TPostfixBadge;
begin
  Result := NewPostfixBadge;
end;

function TPostfixBox<T>.ReadValue: T;
begin
  RecordPostfixInvocation;
  Result := Value;
end;

function NewPostfixBadge: TPostfixBadge;
begin
  Inc(GInvocations);
  Result := TPostfixBadge.Create;
  Result.Value := 42;
end;

function VaryPostfix(AValue: LongInt): TPostfixBadge;
begin
  Result := NewPostfixBadge.WithValue(AValue);
end;

function VaryPostfix(AValue: Boolean): TPostfixOther;
begin
  Result := TPostfixOther.Create;
  Result.OtherValue := Ord(AValue);
end;

function PostfixInvocations: LongInt;
begin
  Result := GInvocations;
end;

procedure RecordPostfixInvocation;
begin
  Inc(GInvocations);
end;

function CompilerPostfixValue: LongInt;
var
  LBadge, LCreated: TPostfixBadge;
  LBox: TPostfixBox<TPostfixBadge>;
  LAddress: PPostfixBadge;
  LRows: TPostfixRows;
  LGrid: TPostfixGrid;
  LNested: TPostfixNested;
  LFactory: TPostfixFactory;
begin
  Result := -1;
  LBadge := NewPostfixBadge;
  LBox := TPostfixBox<TPostfixBadge>.Create;
  try
    LBox.Value := LBadge;
    LAddress := @LBadge;
    SetLength(LRows, 1);
    LRows[0] := LBadge;
    LGrid[0, 1] := LBadge;
    SetLength(LNested, 1);
    LNested[0] := LRows;

    if (LBadge.WithValue(40).Choose(1).Choose(True).Value <> 42) or
      ((LBox.ReadValue()).WithValue(Ord(True) + 41).Value <> 42) or
      ((LAddress^).Value <> 42) or (LRows[0].Value <> 42) or
      (LGrid[0, 1].Value <> 42) or (LNested[0, 0].Value <> 42) then Exit;
    LFactory := @NewPostfixBadge;
    LCreated := LFactory();
    try

      if LCreated.Value <> 42 then Exit;
    finally
      LCreated.Free;
    end;
    Result := LNested[0][0].Value;
  finally
    LBox.Free;
    LBadge.Free;
  end;
end;

end.
