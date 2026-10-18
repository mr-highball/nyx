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
program nyx_pascal_scope_oracle;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.scope.first, nyx.scope.last;

const
  CImported = SizeOf(Integer);
  CSystem = SizeOf(System.Integer);
  CObjPas = SizeOf(ObjPas.Integer);

type
  { A local declaration overrides the explicitly imported name. }
  Integer = Word;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

procedure Nested;
type
  { A nested declaration shadows the unit's own local alias. }
  Integer = Byte;
begin
  Check(SizeOf(Integer) = SizeOf(nyx.scope.first.Integer), 'Nested lexical scope must win');
end;

begin
  WriteLn('scope oracle / imported=', CImported, ' system=', CSystem,
    ' local=', SizeOf(Integer), ' first=', SizeOf(nyx.scope.first.Integer),
    ' last=', SizeOf(nyx.scope.last.Integer));
  Check(CImported = SizeOf(nyx.scope.last.Integer), 'Last explicit import must precede compiler implicit units');
  Check(SizeOf(Integer) = SizeOf(Word), 'Source-local alias must precede explicit imports');
  Check(CSystem = SizeOf(SmallInt), 'Explicit System qualification must retain its own declaration');
  Check(CObjPas = SizeOf(LongInt), 'Implicit ObjPas remains available by its explicit namespace');
  Nested;
  WriteLn('PASS 5 native source/import/implicit scope checks');
end.
