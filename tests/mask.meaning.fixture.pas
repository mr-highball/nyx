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
unit mask.meaning.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Local constants model a shifted allocation mask before the object closes.
    Ordinary source interpretation must retain that exact declaration owner. }
  TMaskedObject = object
  const
    Step = 5;
    Quantum = 1 shl Step;
    Maximum = (416 + (1 shl 10 - 1) shl 10) and -Quantum;
    {$if Maximum <> 1047968}{$fatal Object mask lost its authored arithmetic}{$endif}
    Flags = 32 or 10;
    {$if Flags xor 21 <> 63}{$fatal Object flag combination changed}{$endif}
  end;
  { An independent record reuses names while keeping its own smaller mask. }
  TMaskedRecord = record
  const
    Quantum = 8;
    Maximum = 55 and -Quantum;
    {$if Maximum <> 48}{$fatal Record mask borrowed another owner}{$endif}
  end;

const
  Wide = 9007199254740993 xor 1;
  {$if Wide <> 9007199254740992}{$fatal Wide mask lost exact integer bits}{$endif}
  Pattern = $8000000000000001;
  {$if Pattern >= 0}{$fatal FPC source radix pattern lost its signed interpretation}{$endif}
  UnsignedMask = High(QWord) and High(QWord);
  {$if UnsignedMask <> High(QWord)}{$fatal Unsigned mask changed its interpretation}{$endif}
  DecimalPattern = 9223372036854775809 and 17293822569102704643;

{ This whole compiled companion supplies independent source/operator evidence. }
function NativeMaskMeaning: Boolean;

implementation

function NativeMaskMeaning: Boolean;
begin
  Result := (TMaskedObject.Maximum = 1047968) and
    (TMaskedRecord.Maximum = 48) and (TMaskedObject.Flags = 42) and
    (Wide = 9007199254740992) and (Pattern = -9223372036854775807) and
    (UnsignedMask = High(QWord)) and (DecimalPattern = 9223372036854775809);
end;

end.
