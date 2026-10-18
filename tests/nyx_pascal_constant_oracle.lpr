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
program nyx_pascal_constant_oracle;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, constant.meaning.fixture;

{ Independent configured-compiler evidence. These expressions compile in the
  actual conditional language; the resolver's own arithmetic cannot prove them. }
{$if High(TAuthoredAlias)<>Answer}
  {$fatal Source range and constant do not agree}
{$endif}
{$ifdef NYX_TEST_UNSUPPORTED_LOW}
  {$if Low(TAuthoredAlias)<>-2}
    {$fatal Source lower bound changed}
  {$endif}
{$endif}
{$if Ord(High(TAuthoredChoice))<>9}
  {$fatal Explicit enumeration changed}
{$endif}
{$if LargeExact+2<>9007199254740995}
  {$fatal Wide exact arithmetic changed}
{$endif}
{$if High(QWord)<>18446744073709551615}
  {$fatal Unsigned primitive bound changed}
{$endif}
{$if $ffffffffffffffff<>-1}
  {$fatal Signed radix interpretation changed}
{$endif}
{$if Ord(High(QWord))<>-1}
  {$fatal Signed ordinal projection changed}
{$endif}
{$if (-19 div 3)<>-6}
  {$fatal Signed quotient changed}
{$endif}
{$if (-19 mod 3)<>-1}
  {$fatal Signed remainder changed}
{$endif}

begin

  if NativeConstantMeaning(StrToInt('45')) <> 42 then
  begin
    raise Exception.Create('The whole native constant companion returned the wrong value');
  end;
  WriteLn('PASS actual compiler constant conditions / value=42');
end.
