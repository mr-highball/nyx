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
program nyx_pascal_initializer_oracle;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, hint.initializer.fixture;

var
  GValue: LongInt;

begin
  { This native compiler oracle intentionally references marked symbols inside
    its unit, preserving their expected diagnostics without warning suppression.
    Portable analysis/product builds do not import this compiler-only fixture. }
  GValue := RunHintInitializerFixture;

  if GValue <> 42 then
  begin
    raise Exception.Create('The exact compiled hinted initializer behavior changed');
  end;
  WriteLn('PASS native hinted initializer compiler oracle / value=', GValue);
end.
