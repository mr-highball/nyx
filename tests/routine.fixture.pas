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
unit routine.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

{$i routine.fixture.inc}

type
  { Value-only callback slot, borrowing the static routine assigned by the test.
    Its inline callback type retains an independent explicit convention. }
  THandlers = record
    Compute: function(AValue: LongInt): LongInt; cdecl;
    After: LongInt;
  end;

{ Invoke the compiler's alias, inline record-field and local-variable forms. }
function RunCallbackForms: LongInt;

implementation

function Twice(AValue: LongInt): LongInt; cdecl;
begin
  Result := AValue * 2;
end;

function Bump(AValue: LongInt): LongInt; stdcall;
begin
  Result := AValue + 1;
end;

function RunCallbackForms: LongInt;
var
  LCallback: TCompute;
  LHandlers: THandlers;
  LDirect: function(AValue: LongInt): LongInt; stdcall;
  LAfter: LongInt;
begin
  LCallback := @Twice;
  LHandlers.Compute := @Twice;
  LHandlers.After := 0;
  LDirect := @Bump;
  LAfter := LHandlers.After;
  Result := LCallback(10) + LHandlers.Compute(5) + LDirect(3) + LAfter;
end;

end.
