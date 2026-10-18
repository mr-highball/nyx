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
unit member.generics.fixture;

{$mode objfpc}{$H+}{$codepage utf8}

interface

type
  { Stateless native oracle for FPC's generic-class method prefix order. }
  TGenericFactory = class
  public
    { Return the specialized input without retaining caller state. }
    generic class function Echo<T>(const AValue: T): T; static;
    { Copy using the compiler's assignment rules for the actual specialization. }
    generic class procedure CopyValue<T>(const AValue: T; out AResult: T); static;
  end;

{ Execute actual specializations instead of inferring runtime behavior from parsing. }
function GenericMemberValue: LongInt;

implementation

generic class function TGenericFactory.Echo<T>(const AValue: T): T;
begin
  Result := AValue;
end;

generic class procedure TGenericFactory.CopyValue<T>(const AValue: T; out AResult: T);
begin
  AResult := AValue;
end;

function GenericMemberValue: LongInt;
begin
  TGenericFactory.specialize CopyValue<LongInt>(40, Result);
  Result := TGenericFactory.specialize Echo<LongInt>(Result + 2);
end;

end.
