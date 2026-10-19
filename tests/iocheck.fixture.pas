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
unit iocheck.fixture;

{ ObjFPC admits legacy bracket procedure directives. Delphi mode enables
  prefixed attributes on the pinned compiler and rejects this suffix spelling.
  The consumer and product remain in Delphi mode; this is a compiler oracle. }
{$mode objfpc}{$H+}{$codepage utf8}

interface

{ Execute actual caller-side I/O checks under the native compiler. The parser
  inspects this same complete unit/include without predicting runtime effects. }
function CompilerIOCheckValue: Integer;

implementation

uses SysUtils;

{$include iocheck.fixture.inc}

function CompilerIOCheckValue: Integer;
begin
  Result := 0;
  {$push}{$I+}
  try
    BareIOCheck;
  except
    on LError: EInOutError do
    begin

      if LError.ErrorCode = 123 then
      begin
        Inc(Result);
      end;
    end;
  end;
  try
    BracketIOCheck;
  except
    on LError: EInOutError do
    begin

      if LError.ErrorCode = 124 then
      begin
        Inc(Result);
      end;
    end;
  end;
  { The same marked routine leaves its result for the caller under I-. }
  {$I-}
  BareIOCheck;

  if IOResult = 123 then
  begin
    Inc(Result);
  end;
  { An unmarked routine does not gain an implicit check from the caller's I+. }
  {$I+}
  OrdinaryIOResult;

  if IOResult = 125 then
  begin
    Inc(Result);
  end;
  {$pop}

  if Result <> 4 then
  begin
    raise Exception.Create('The native IOCheck caller-policy oracle disagrees');
  end;
  Result := 42;
end;

end.
