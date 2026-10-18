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
program nyx_pascal_integer_oracle;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, shift.meaning.fixture;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

begin
  try
    Check(NativeShiftMeaning, 'Whole object/record source conditions and values');
    Check((1 shl 5) = 32, 'Authored quantum');
    Check((1 << 5) = 32, 'Ordinary symbolic left shift');
    Check((128 >> 2) = 32, 'Ordinary symbolic right shift');
    Check((128 shr 2 div 2) = 16, 'Multiplicative left association');
    Check((1 shl 5 + 1) = 33, 'Shift binds more tightly than addition');
    Check((9007199254740993 shl 1) = 18014398509481986, 'Beyond browser exact-number range');
    Check((1 shl 31) = 2147483648, 'Thirty-first bit');
    Check((1 shl 32) = 4294967296, 'Thirty-second bit');
    Check((1 shl 63) = Low(Int64), 'Signed high-bit interpretation');
    Check(((1 shl 63) shl 1) = 0, 'Discarded high bits');
    Check(((-1) shr 1) = High(Int64), 'Logical zero fill on a negative constant');
    Check(((-1) shr 63) = 1, 'Final bit of a negative constant');
    Check((High(QWord) shr 63) = 1, 'Unsigned high bit');
    Check((High(QWord) shl 1) = High(QWord) - 1, 'Unsigned shifted pattern');
    Check((High(Word) - 65536) = -1, 'Narrow unsigned bounds use signed constant arithmetic');
    Check((High(Word) shl 48) = 18446462598732840960, 'Unsigned narrow bound in the constant-folding domain');
    Check((High(Byte) shl 8) = 65280, 'A constant fold does not truncate to the declared narrow operand');
    WriteLn('PASS ', GChecks, ' actual compiler shift and narrow-bound checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
