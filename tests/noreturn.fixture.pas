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
unit noreturn.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

{ Terminates only its explicitly launched qualification process. The marker is
  compiler-checked declaration intent, not a substitute for examining this body. }
procedure StopFixture(AExitCode: LongInt); noreturn;
{ An ordinary following routine retains its independent signature and body. }
function FollowingFixtureValue: LongInt;

implementation

procedure StopFixture(AExitCode: LongInt); noreturn;
begin
  WriteLn('Entered the authored non-returning routine');
  Halt(AExitCode);
end;

function FollowingFixtureValue: LongInt;
begin
  Result := 42;
end;

end.
