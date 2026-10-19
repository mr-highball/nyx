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
unit packing.mode.fixture;

{ PACKRECORDS is deliberately pending when MODE is scanned. The compiler must
  flush that request and preserve it while changing the language-mode defaults. }
{$packrecords 1}{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Ordered instance data inherits the pre-MODE request. No platform byte size
    is prescribed; the independent packed declaration supplies the comparison. }
  TInheritedPacking = record
    Lead: Byte;
    Value: Longint;
  end;
  { Explicit packing supplies a compiler-owned comparison independent of the
    retained global request. This is data only, with no methods or initialization. }
  TExplicitPacking = packed record
    Lead: Byte;
    Value: Longint;
  end;

{ Native proof uses actual sizeof for both independently declared types. It
  establishes this mode transition, not an initial target packing/default ABI. }
function PackingPreserved: Boolean;

implementation

{$if sizeof(TInheritedPacking) <> sizeof(TExplicitPacking)}
  {$fatal The source MODE discarded its preceding record packing request}
{$endif}

function PackingPreserved: Boolean;
begin
  Result := SizeOf(TInheritedPacking) = SizeOf(TExplicitPacking);
end;

end.
