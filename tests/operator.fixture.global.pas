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
unit operator.fixture.global;

{ This independent compiler input deliberately selects ObjFPC: Delphi mode
  accepts record operators but does not admit this global declaration syntax. }
{$mode objfpc}{$H+}{$codepage utf8}

interface

type
  { Value-only operands for the compiler's named result-identifier grammar. }
  TGlobalProduct = record
    Value: LongInt;
  end;

operator *(const A, B: TGlobalProduct) AResult: TGlobalProduct;

{ Invoke the whole independently compiled global operator. }
function RunGlobalOperator: LongInt;

implementation

operator *(const A, B: TGlobalProduct) AResult: TGlobalProduct;
begin
  AResult.Value := A.Value * B.Value;
end;

function RunGlobalOperator: LongInt;
var
  LLeft: TGlobalProduct;
  LRight: TGlobalProduct;
  LProduct: TGlobalProduct;
begin
  LLeft.Value := 2;
  LRight.Value := 3;
  LProduct := LLeft * LRight;
  Result := LProduct.Value;
end;

end.
