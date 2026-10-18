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
unit open.scope.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Outer value visible before a later class-local declaration of the same name. }
  TOrderedValue = Byte;
  { Complete ancestor supplies its nested type to an unfinished descendant. }
  TScopeAncestor = class
  public
    type
      { Independent value passed through the descendant's inherited alias. }
      TInheritedValue = record
        Number: Integer;
      end;
  end;
  { This whole authored class is also inspected at earlier incomplete prefixes.
    Its later nested alias must never change the earlier field's resolved type. }
  TScopeDescendant = class(TScopeAncestor)
  public
    { Uses the earlier outer alias even after a same-named nested type is added. }
    Earlier: TOrderedValue;
    type
      { Independent later name proves declaration order rather than spelling. }
      TOrderedValue = Word;
      { Inherited nested type remains available while this body is being declared. }
      TLocalValue = TInheritedValue;

    { The native compiler's declared() directive is an additional compiler oracle.
      pas2js still compiles the inherited alias and runtime behavior above/below;
      Nyx's own conditional reader is exercised separately on both source targets. }
    {$ifndef PAS2JS}
      {$if not declared(TInheritedValue)}
        {$fatal The compiler must see the captured inherited nested type}
      {$endif}
    {$endif}

    function Evaluate(const AValue: TLocalValue): Integer;
  end;

{ Returns authored inherited/member behavior; the caller owns the temporary object. }
function RunOpenScopeFixture: Integer;
{$ifndef PAS2JS}
{ Returns actual compiler layout of the field bound before the nested alias. }
function EarlierScopeSize: Integer;
{ Returns actual compiler layout of the later nested alias independently. }
function LaterScopeSize: Integer;
{$endif}

implementation

function TScopeDescendant.Evaluate(const AValue: TLocalValue): Integer;
begin
  Result := AValue.Number + Earlier;
end;

function RunOpenScopeFixture: Integer;
var
  LObject: TScopeDescendant;
  LValue: TScopeDescendant.TLocalValue;
begin
  LObject := TScopeDescendant.Create;
  try
    LObject.Earlier := 2;
    LValue.Number := 40;
    Result := LObject.Evaluate(LValue);
  finally
    LObject.Free;
  end;
end;

{$ifndef PAS2JS}
function EarlierScopeSize: Integer;
var
  LObject: TScopeDescendant;
begin
  Result := SizeOf(LObject.Earlier);
end;

function LaterScopeSize: Integer;
begin
  Result := SizeOf(TScopeDescendant.TOrderedValue);
end;
{$endif}

end.
