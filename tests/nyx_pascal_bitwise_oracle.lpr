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
program nyx_pascal_bitwise_oracle;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, mask.meaning.fixture;

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
    Check(NativeMaskMeaning, 'Whole object/record source masks and radix values');
    Check(((416 + (1 shl 10 - 1) shl 10) and -32) = 1047968, 'Shifted negative mask');
    Check((63 and 42) = 42, 'Integer AND');
    Check((32 or 10) = 42, 'Integer OR');
    Check((63 xor 21) = 42, 'Integer XOR');
    Check(((-1) and 42) = 42, 'Negative AND pattern');
    Check(((-64) or 42) = -22, 'Negative OR pattern');
    Check(((-1) xor 42) = -43, 'Negative XOR pattern');
    Check((High(QWord) and High(QWord)) = High(QWord), 'Unsigned all-ones AND');
    Check((High(QWord) or 0) = High(QWord), 'Unsigned OR with zero');
    Check((High(QWord) xor High(QWord)) = 0, 'Unsigned XOR with self');
    Check($8000000000000001 = -9223372036854775807, 'Ordinary FPC radix sign');
    Check(($8000000000000001 and $f000000000000003) = -9223372036854775807,
      'High-bit radix operands retain signed interpretation');
    Check((9223372036854775809 and 17293822569102704643) = 9223372036854775809,
      'Decimal unsigned operands retain their distinct value');
    Check((9007199254740993 xor 1) = 9007199254740992, 'Exact bits above browser number precision');
    WriteLn('PASS ', GChecks, ' actual compiler integer masking and radix checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
