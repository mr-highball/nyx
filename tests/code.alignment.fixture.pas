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
unit code.alignment.fixture;
{$H+}{$codepage utf8}

{ Whole compiler syntax oracle; none of these requests asserts a target layout. }
{$codealign proc=16,jump=4,loop=8,constmin=16,constmax=32,varmin=8,varmax=16,localmin=4,localmax=16,recordmin=1,recordmax=16}
{$push}
{$i code.alignment.fixture.inc}
{$mode delphi}
{$pop}

{ Explicit negative compiler oracles; ordinary builds never select these. }
{$ifdef NYX_ALIGNMENT_REJECT_POWER}
{$codealign proc=3}
{$endif}
{$ifdef NYX_ALIGNMENT_REJECT_SELECTOR}
{$codealign maxcrecord=8}
{$endif}

interface

{ Execute an ordinary function after the original requested settings. }
function AlignmentFixtureValue(AValue: Integer): Integer;

implementation

{$codealign proc=0,jump=0,loop=0}
{$codealign constmin=-1,constmax=0}

function AlignmentFixtureValue(AValue: Integer): Integer;
var
  LValue: Integer;
begin
  LValue := AValue * 2;
  Result := LValue + 2;
end;

end.
