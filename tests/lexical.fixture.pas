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
unit lexical.fixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

{$push}{$I-,Q-,H-,R-,V-}
const
  CGroupedIO = {$ifopt I-}True{$else}False{$endif};
  CGroupedOverflow = {$ifopt Q-}True{$else}False{$endif};
  CGroupedStrings = {$ifopt H-}True{$else}False{$endif};
  CGroupedRange = {$ifopt R-}True{$else}False{$endif};
  CGroupedVar = {$ifopt V-}True{$else}False{$endif};
{$pop}

{ A real compiled function qualifies the register/operand lexical envelope. Its
  i386 assembly is executed only by the configured native qualification target;
  the browser consumer compiles the portable branch without an execution claim. }
function TwiceLexicalValue(AValue: Integer): Integer;

implementation

function TwiceLexicalValue(AValue: Integer): Integer;
begin
  {$if defined(CPUI386) and not defined(PAS2JS)}
  {$asmmode att}
  asm
    movl AValue, %eax
    addl %eax, %eax
    movl %eax, Result
  end;
  {$else}
  Result := AValue * 2;
  {$endif}
end;

end.
