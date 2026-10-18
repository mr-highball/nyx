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
unit scope.labels.fixture;

{$mode delphi}{$H+}{$codepage utf8}{$goto on}

interface

{ Actual native GOTO execution is separate from structural label metadata. }
function LabelledValue: LongInt;
{ Exercise initialized procedural variables with and without a convention. }
function CallbackValue: LongInt;
{ A module switch remains changed across local settings PUSH/POP. }
function ModuleSwitchValue: LongInt;

implementation

function OffsetValue(AValue: LongInt): LongInt; cdecl;
begin
  Result := AValue + 2;
end;

function PlainValue(AValue: LongInt): LongInt;
begin
  Result := AValue;
end;

var
  ConventionCallback: function(AValue: LongInt): LongInt; cdecl = @OffsetValue;
  PlainCallback: function(AValue: LongInt): LongInt = @PlainValue;

{$i scope.labels.fixture.inc}

{$goto off}{$push}{$goto on}{$pop}
function ModuleSwitchValue: LongInt;
label
  Ready;
begin
  Result := 40;

  if Result = 40 then
  begin
    goto Ready;
  end;
  Inc(Result, 100);
Ready:
  Inc(Result, 2);
end;

function CallbackValue: LongInt;
begin
  Result := ConventionCallback(PlainCallback(40));
end;

end.
