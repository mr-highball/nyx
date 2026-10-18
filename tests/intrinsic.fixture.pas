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
unit intrinsic.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{ This compiler-specific fixture uses the direct Delphi-mode suffix spelling.
  The original RTL's bracket spelling is qualified separately as source syntax. }

interface

const
  { Exact configured FPC innr.inc operation, qualified with that source pin.
    Nyx retains this operand reference; its reader never evaluates the number. }
  CWordSwapIntrinsic = 104;

{ Declared compile-time role does not remove the ordinary implementation body. }
function SwapFixtureWord(AValue: Word): Word; internconst: CWordSwapIntrinsic;
{ Observe runtime body calls independently of compiler-folded constant use. }
function IntrinsicBodyCalls: Integer;

implementation

var
  GBodyCalls: Integer;

function SwapFixtureWord(AValue: Word): Word;
begin
  Inc(GBodyCalls);
  Result := Word((AValue shl 8) or (AValue shr 8));
end;

function IntrinsicBodyCalls: Integer;
begin
  Result := GBodyCalls;
end;

end.
