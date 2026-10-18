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
program nyx_pascal_size_oracle;
{$mode delphi}{$H+}{$codepage utf8}

{ Independent compiler evidence separates a source-local alias from an explicitly
  qualified intrinsic. No Nyx-generated size expression is used by this oracle. }
uses SysUtils;

const
  CBefore: System.Integer = SizeOf(Integer);
  CQualified: System.Integer = SizeOf(System.Integer);
type
  { Intentionally shadows the imported intrinsic in this source scope. }
  Integer = Byte;
begin
  WriteLn('compiler sizes / before=', CBefore, ' local=', SizeOf(Integer),
    ' qualified=', CQualified, ' current-qualified=', SizeOf(System.Integer));

  if (SizeOf(Integer) <> 1) or (CBefore < 1) or (CQualified < 1) or
    (SizeOf(System.Integer) <> CQualified) then
  begin
    raise Exception.Create('Compiler type identity does not match the recorded scope contract');
  end;
  WriteLn('PASS compiler size identity / before=', CBefore,
    ' local=', SizeOf(Integer), ' qualified=', CQualified, ' pointer=', SizeOf(Pointer));
end.
