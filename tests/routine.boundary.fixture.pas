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
unit routine.boundary.fixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Detached routine result used for declared-type and navigation checks. }
  TRoutineValue = record
    Count: LongInt;
  end;
  { Escaped contextual spelling remains a type name, never a convention clause. }
  &cdecl = LongInt;

{$i routine.boundary.fixture.inc}

{ Execute ordinary routines; compiler-helper declaration admission is separate. }
function RoutineBoundaryValue: LongInt;

implementation

{ Adjacent calling and forward clauses preserve the later body's signature. }
function ForwardValue(AValue: LongInt): LongInt cdecl forward;

procedure CopyChars(out AResult: array of AnsiChar; const ASource: RawByteString); compilerproc;
begin
  { This unused helper is a compiler-syntax oracle, not an ordinary callable API. }
end;

function IncrementValue(AValue: LongInt): LongInt;
begin
  Result := AValue + 1;
end;

function MakeState: TRoutineValue;
begin
  Result.Count := 40;
end;

function EscapedValue: &cdecl;
begin
  Result := 1;
end;

function ForwardValue(AValue: LongInt): LongInt;
begin
  Result := AValue;
end;

function RoutineBoundaryValue: LongInt;
var
  LState: TRoutineValue;
begin
  LState := MakeState;
  Result := ForwardValue(IncrementValue(LState.Count)) + EscapedValue;
end;

end.
