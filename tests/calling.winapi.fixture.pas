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
unit calling.winapi.fixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Compiler-admitted named request, not an analyzer-computed native ABI. }
  TApiCallback = function(AValue: LongInt): LongInt; winapi;
  { The anonymous callback owns its modifier; its following scalar stays a
    separate record field. Values retain no managed object or UI references. }
  TApiSlots = record
    Call: function(AValue: LongInt): LongInt; winapi;
    Marker: LongInt;
  end;

{ Exercise named, anonymous-variable and record callbacks on the pinned native
  compiler. No other target's convention mapping is inferred from this call. }
function CompilerWinapiValue: LongInt;

implementation

{$include calling.winapi.fixture.inc}

function CompilerWinapiValue: LongInt;
var
  LNamed: TApiCallback;
  LSlots: TApiSlots;
begin
  LNamed := IncrementApi;
  Direct := IncrementApi;
  LSlots.Call := IncrementApi;
  AfterValue := 4;
  LSlots.Marker := AfterValue;
  Result := LNamed(35) + Direct(0) + LSlots.Call(LSlots.Marker);
end;

end.
