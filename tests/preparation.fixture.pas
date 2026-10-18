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
unit preparation.fixture;
{$mode delphi}{$H+}{$codepage utf8}
interface

{$ifdef NYX_PREPARATION_UNDEFINED_ALPHA}
const SelectedValue = 0;
{$else}
{$i preparation.fixture.inc}
{$endif}

type
  { Whole compiled source exposes a concrete member chosen by real preprocessing. }
  TPreparedValue = record
    Value: LongInt;
  end;

{ Run the compiler-selected original branch through a real record field. }
function RunPreparedValue: LongInt;

implementation

function RunPreparedValue: LongInt;
var
  LValue: TPreparedValue;
begin
  LValue.Value := SelectedValue;
  Result := LValue.Value;
end;

end.
