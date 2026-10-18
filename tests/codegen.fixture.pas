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
unit codegen.fixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Whole compiler-backed input: native execution proves both assembler readers
  and POP restoration were admitted. Register directives request code generation;
  the numerical result does not prove a particular effective register allocation. }
function CompilerPolicyValue: Integer;

implementation

{$ifndef PAS2JS}
{$maxfpuregisters 0}
{$asmmode att}
{$push}{$asmmode intel}
{ This body requires Intel syntax while the saved outer reader remains AT&T. }
function AddOne(AValue: Integer): Integer; assembler;
asm
  mov eax, AValue
  add eax, 1
end;
{$pop}

{ The compiler must have restored AT&T before reading these percent operands. }
function AddTwo(AValue: Integer): Integer; assembler;
asm
  movl AValue, %eax
  addl $2, %eax
end;

{ Request a per-routine register limit without changing floating type meaning. }
function AddHalf(AValue: Double): Double;
{$maxfpuregisters 7}
begin
  Result := AValue + 0.5;
end;
{$maxfpuregisters normal}
{ Qualification uses the explicitly configured 3.3.1 compiler, whose reader
  admits eight. This fixture does not claim that older compilers admit it. }
{$maxfpuregisters 8}
{$endif}

function CompilerPolicyValue: Integer;
begin
  {$ifndef PAS2JS}
  Result := AddOne(10) + AddTwo(20);

  if AddHalf(2.5) <> 3.0 then
  begin
    Result := -1;
  end;
  {$else}
  Result := 33;
  {$endif}
end;

end.
