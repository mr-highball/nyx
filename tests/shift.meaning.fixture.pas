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
unit shift.meaning.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { An unfinished object scope must expose its already declared constants to
    the following source condition, independently of later members. }
  TShiftedObject = object
  const
    Step = 5;
    Quantum = 1 shl Step;
    {$if Quantum <> 32}{$fatal Object quantum differs from its authored shift}{$endif}
    Wide = 9007199254740993 shl 1;
    {$if Wide <> 18014398509481986}{$fatal Exact wide shift lost precision}{$endif}
    Combined = Quantum << 2;
    {$if Combined shr 2 <> Quantum}{$fatal Symbolic shift precedence changed}{$endif}
  end;
  { A record provides a separate declaration owner with the same local spelling.
    Its smaller quantum cannot borrow the object's value or future scope. }
  TShiftedRecord = record
  const
    Step = 3;
    Quantum = 1 shl Step;
    {$if Quantum <> 8}{$fatal Record quantum lost its independent scope}{$endif}
  end;

const
  Wrapped = 1 shl 63;
  ZeroFilled = (-1) shr 1;
  UnsignedResult = High(QWord) shl 1;
  {$if Wrapped >= 0}{$fatal Signed constant shift did not retain its pattern}{$endif}
  {$if ZeroFilled <> 9223372036854775807}{$fatal Negative SHR was not logical}{$endif}

{ Compiled whole-unit proof, separate from Nyx's source interpretation. }
function NativeShiftMeaning: Boolean;

implementation

function NativeShiftMeaning: Boolean;
begin
  Result := (TShiftedObject.Quantum = 32) and
    (TShiftedRecord.Quantum = 8) and
    (TShiftedObject.Wide = 18014398509481986) and
    (UnsignedResult = High(QWord) - 1) and
    (High(Word) - 65536 = -1) and
    (High(Byte) shl 8 = 65280);
end;

end.
