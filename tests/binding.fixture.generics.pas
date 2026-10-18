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
unit binding.fixture.generics;
{$mode delphi}{$H+}{$codepage utf8}
interface
type
  { Actual compiler oracle with a concrete field, property and fluent self type.
    This class owns its value; no resolver handles or runtime UI are involved. }
  TBindingBox<T> = class
  public type
    { Nested alias resolves its element through the enclosing generic frame. }
    TItems = array of T;
    { Nested callable must bind parameter and result through that same frame. }
    TMapper = function(const AValue: T): T of object;
  private
    FValue: T;
  public
    property Value: T read FValue write FValue;
    procedure Put(const AValue: T);
    function ReadValue: T;
    function SelfType: TBindingBox<T>;
  end;
  { A generic ancestor is specialized with the descendant's concrete argument. }
  TBindingDerived<T> = class(TBindingBox<T>)
  public
    Extra: T;
  end;
  { Multi-parameter record exercises independent names rather than replacement
    of one hardcoded placeholder spelling. }
  TBindingPair<TLeft, TRight> = record
    Left: TLeft;
    Right: TRight;
  end;
  { Generic aliases own their parameter scope even without a nested member scope. }
  TBindingSequence<TElement> = array of TElement;
  { An ordinary nested record captures its enclosing concrete type. FPC rejects
    introducing a second generic parameter list inside this generic declaration. }
  TBindingOuter<TOuter> = class
  public type
    { The nested value uses the outer argument without owning another template. }
    TInner = record
      OuterValue: TOuter;
    end;
  end;
implementation
procedure TBindingBox<T>.Put(const AValue: T);
begin
  FValue := AValue;
end;
function TBindingBox<T>.ReadValue: T;
begin
  Result := FValue;
end;
function TBindingBox<T>.SelfType: TBindingBox<T>;
begin
  Result := Self;
end;
end.
