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
unit operator.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses operator.fixture.global;

{$i operator.fixture.inc}

{ Invoke independently compiled symbolic/named/unary/conversion operators. }
function RunOperatorForms: LongInt;

implementation

function TOperatorVector.Add(AValue: LongInt): LongInt;
begin
  Result := Value + AValue;
end;

class operator TOperatorVector.+(const A, B: TOperatorVector): TOperatorVector;
begin
  Result.Value := A.Value + B.Value;
end;

class operator TOperatorVector.+(const A: TOperatorVector): TOperatorVector;
begin
  Result := A;
end;

class operator TOperatorVector.Add(const A: TOperatorVector; B: LongInt): TOperatorVector;
begin
  Result.Value := A.Value + B;
end;

class operator TOperatorVector.=(const A, B: TOperatorVector): Boolean;
begin
  Result := A.Value = B.Value;
end;

class operator TOperatorVector.Implicit(A: LongInt): TOperatorVector;
begin
  Result.Value := A;
end;

class operator TOperatorVector.Explicit(const A: TOperatorVector): LongInt;
begin
  Result := A.Value;
end;

function RunOperatorForms: LongInt;
var
  LFirst: TOperatorVector;
  LSecond: TOperatorVector;
  LSum: TOperatorVector;
  LUnary: TOperatorVector;
  LNamed: TOperatorVector;
begin
  LFirst := 2;
  LSecond := 3;
  LSum := LFirst + LSecond;
  LUnary := +LFirst;
  LNamed := LFirst + 4;
  Result := LongInt(LSum) + LUnary.Value + LNamed.Value + LFirst.Add(8);

  if LFirst = LFirst then
  begin
    Inc(Result);
  end;
  Inc(Result, RunGlobalOperator);
end;

end.
