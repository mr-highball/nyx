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
unit binding.fixture.base;
{$mode delphi}{$H+}{$codepage utf8}
interface
type
  { Compiled visibility oracle. Private is unit-scoped; strict/private/protected
    fields are observed only through language-permitted ordinary method calls. }
  TBindingBase = class
  private
    FUnitSecret: Integer;
  strict private
    FSecret: Integer;
  protected
    FProtected: Integer;
  strict protected
    FStrictProtected: Integer;
  public
    Value: Integer;
    constructor Create;
    function WithValue(AValue: Integer): TBindingBase;
    function RevealSecret: Integer;
  end;
  { Transparent alias keeps the same constructed class identity. }
  TBindingAlias = TBindingBase;
  { Distinct ordinal alias must retain its authored declaration identity. }
  TBindingDistinct = type Integer;
  { Forward identity is available before its later member completion. }
  TBindingForward = class;
  { Alias references the same forward identity, not a new incomplete class. }
  TBindingForwardAlias = TBindingForward;
  { Full completion of the preceding forward type. }
  TBindingForward = class
  public
    Data: Integer;
  end;
function BindingSameUnitSecret(AValue: TBindingBase): Integer;
implementation
constructor TBindingBase.Create;
begin
  inherited Create;
  FUnitSecret := 1;
  FSecret := 2;
  FProtected := 4;
  FStrictProtected := 8;
  Value := 16;
end;
function TBindingBase.WithValue(AValue: Integer): TBindingBase;
begin
  Value := AValue;
  Result := Self;
end;
function TBindingBase.RevealSecret: Integer;
begin
  Result := FSecret;
end;
function BindingSameUnitSecret(AValue: TBindingBase): Integer;
begin
  Result := AValue.FUnitSecret;
end;
end.
