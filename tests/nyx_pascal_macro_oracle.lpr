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
program nyx_pascal_macro_oracle;
{$mode delphi}{$H+}{$codepage utf8}

{ Independent handwritten compiler oracle. No resolver-generated projection is
  compiled here; the actual compiler owns every replacement and conditional. }
uses SysUtils;

const
  Value = 7;
{$macro on}
{$define FIELD_MEMBER:=AlphaField:Integer;}
type
  { Actual compiler-created record member, independently of Nyx's projection. }
  TData = record
    FIELD_MEMBER
  end;
var
  LChecks: Integer;
  LData: TData;

procedure Check(AValue: Boolean);
begin
  Inc(LChecks);

  if not AValue then
  begin
    raise Exception.Create('Actual compiler macro behavior differs at check ' + IntToStr(LChecks));
  end;
end;

begin
  {$macro on}
  LData.AlphaField := 23;
  Check(LData.AlphaField = 23);
  {$define Value:=11}
  Check(Value = 11);
  Check(&Value = 7);
  Check('Value' = 'Value');
  {$define AliasValue:=Value}
  Check(AliasValue = 11);
  {$define Value:=13}
  Check(AliasValue = 13);
  {$if AliasValue <> 13}{$fatal Recursive condition replacement failed}{$endif}
  {$macro off}
  Check(Value = 7);
  {$macro on}
  Check(Value = 13);
  {$define Value:=19}
  {$define Value}
  Check(Value True);
  {$undef Value}
  Check(Value = 7);
  {$define EmptyValue:=}
  Check(EmptyValue True);
  WriteLn('PASS ', LChecks, ' independent compiler macro checks');
end.
