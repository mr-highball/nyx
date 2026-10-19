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
unit constraints.ancestry.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Empty nominal contracts exercise source identity without a UI or callback. }
  IConstraintMark = interface(IInterface)
    ['{86E21E45-5AA3-42F6-BD61-879EAAED9209}']
  end;
  { A distinct second contract qualifies an intersection of requirements. }
  IConstraintOther = interface(IInterface)
    ['{659C814B-55A2-423D-B0E4-A391D5338702}']
  end;
  { Interface ancestry is separate from a class's explicit implementation list. }
  IConstraintChild = interface(IConstraintMark)
    ['{213DEAA6-A3D7-4099-BB6B-55616DC46B0F}']
  end;
  { A real class ancestor supplies a field through the ordinary compiler. }
  TConstraintAncestor = class(TInterfacedObject)
    Value: LongInt;
  end;
  { Explicit contracts and the concrete parent must all survive specialization. }
  TConstraintTagged = class(TConstraintAncestor, IConstraintMark, IConstraintOther)
  end;
  { Implementing a child alone does not author the parent's nominal entry. }
  TConstraintChildOnly = class(TInterfacedObject, IConstraintChild)
  end;
  { This deliberately has no parameterless Create. A constructor constraint's
    admission is not evidence that a particular constructor call will bind. }
  TConstraintNeedsArgument = class
    Value: LongInt;
    constructor Create(AValue: LongInt);
  end;
  { The concrete class requirement admits descendants, not unrelated names. }
  TAncestryBox<T: TConstraintAncestor> = class
    Item: T;
  end;
  { Its constructed identity includes the actual argument, not only its name. }
  TGenericAncestor<T> = class
    Item: T;
  end;
  { Inherited generic arguments remain attached to their declaring ancestor. }
  TConstraintGenericChild = class(TGenericAncestor<LongInt>)
  end;
  { Adjacent closing angles intentionally share one original lexical token. }
  TExactAncestryBox<T: TGenericAncestor<LongInt>> = class
    Item: T;
  end;
  { A single interface requirement also admits related interface arguments. }
  TInterfaceBox<T: IConstraintMark> = class
    Item: T;
  end;
  { Several interfaces require one implementing class in the inspected FPC. }
  TIntersectionBox<T: IConstraintMark, IConstraintOther> = class
    Item: T;
  end;
  { Concrete ancestry, interface implementation and constructor intent combine. }
  TCombinedBox<T: TConstraintAncestor, IConstraintMark, constructor> = class
    Item: T;
  end;
  { FPC 3.3.1 checks this as class eligibility at specialization, independently
    of whether generic code later attempts a particular constructor call. }
  TConstructorBox<T: class, constructor> = class
    Item: T;
  end;
  { Whole source aliases force the compiler to check every accepted family. }
  TIntersectionProof = TIntersectionBox<TConstraintTagged>;
  { A child interface is a valid actual type for one ancestor requirement. }
  TInterfaceProof = TInterfaceBox<IConstraintChild>;
  { The combined requirement keeps the same concrete descendant. }
  TCombinedProof = TCombinedBox<TConstraintTagged>;

  {$ifdef NYX_BAD_CONSTRAINT_LINEAGE}
  { Independent negative oracle: an unrelated class cannot satisfy ancestry. }
  TBadLineage = TAncestryBox<TObject>;
  {$endif}
  {$ifdef NYX_BAD_CONSTRAINT_ARGUMENT}
  { Independent negative oracle: the same generic name with another argument. }
  TBadArgument = TExactAncestryBox<TGenericAncestor<Byte>>;
  {$endif}
  {$ifdef NYX_BAD_CONSTRAINT_INTERFACE}
  { Independent negative oracle: this class never implements the contract. }
  TBadInterface = TInterfaceBox<TObject>;
  {$endif}
  {$ifdef NYX_BAD_CONSTRAINT_CONSTRUCTOR}
  { Independent negative oracle: constructor intent still requires a class. }
  TBadConstructor = TConstructorBox<LongInt>;
  {$endif}
  {$ifdef NYX_BAD_CONSTRAINT_INTERFACE_PARENT}
  { Independent negative oracle: a class's child-interface entry is not a
    separately authored implementation of the parent's nominal contract. }
  TBadInterfaceParent = TInterfaceBox<TConstraintChildOnly>;
  {$endif}

{ Retire every allocated object and return a value through three actual generic
  fields. This unit is also consumed unchanged by the semantic reader. }
function CompilerAncestryValue: LongInt;

implementation

constructor TConstraintNeedsArgument.Create(AValue: LongInt);
begin
  inherited Create;
  Value := AValue;
end;

function CompilerAncestryValue: LongInt;
var
  LAncestry: TAncestryBox<TConstraintTagged>;
  LExact: TExactAncestryBox<TConstraintGenericChild>;
  LConstructor: TConstructorBox<TConstraintNeedsArgument>;
begin
  LAncestry := TAncestryBox<TConstraintTagged>.Create;
  try
    LAncestry.Item := TConstraintTagged.Create;
    try
      LExact := TExactAncestryBox<TConstraintGenericChild>.Create;
      try
        LExact.Item := TConstraintGenericChild.Create;
        try
          LConstructor := TConstructorBox<TConstraintNeedsArgument>.Create;
          try
            LConstructor.Item := TConstraintNeedsArgument.Create(1);
            try
              LAncestry.Item.Value := 40;
              LExact.Item.Item := 1;
              Result := LAncestry.Item.Value + LExact.Item.Item + LConstructor.Item.Value;
            finally
              LConstructor.Item.Free;
            end;
          finally
            LConstructor.Free;
          end;
        finally
          LExact.Item.Free;
        end;
      finally
        LExact.Free;
      end;
    finally
      LAncestry.Item.Free;
    end;
  finally
    LAncestry.Free;
  end;
end;

end.
