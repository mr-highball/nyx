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
unit constraints.formal.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { A nominal member contract, independent of any supplied implementation. }
  IFormalValue = interface(IInterface)
    ['{51F8C5F3-1D17-4A05-BB15-EC8B63376F2B}']
    function ReadValue: LongInt;
  end;
  { This class guarantees members available to an unapplied formal argument. }
  TFormalBase = class(TInterfacedObject)
    Value: LongInt;
    function ReadValue: LongInt;
  end;
  { Additional members are not part of a generic parameter's base contract. }
  TFormalItem = class(TFormalBase, IFormalValue)
    Extra: LongInt;
  end;
  { Its method body can use the declared base without a concrete T. }
  TFormalClassBox<T: TFormalBase> = class
    Item: T;
    function Read(const AValue: T): LongInt;
  end;
  { The interface contract guarantees its own declared member. }
  TFormalInterfaceBox<T: IFormalValue> = class
    function Read(const AValue: T): LongInt;
  end;
  { An unconstrained parameter retains identity without inventing members. }
  TFormalAny<T> = class
    function Keep(const AValue: T): T;
  end;
  { A value requirement does not manufacture a particular record's fields. }
  TFormalRecord<T: record> = class
    function Keep(const AValue: T): T;
  end;
  { Class eligibility guarantees the compiler's resolved root contract. }
  TFormalClassOnly<T: class> = class
    Item: T;
  end;
  { A separate formal declaration cannot borrow the first box's parameter. }
  TFormalOther<T: TFormalBase> = class
    Item: T;
  end;
  { The compiler validates passing a bounded formal to another generic type. }
  TFormalRelay<T: TFormalBase> = class
    Item: TFormalClassBox<T>;
  end;
  { Interface and value guarantees also survive a nested constructed field. }
  TFormalInterfaceRelay<T: IFormalValue> = class
    Item: TFormalInterfaceBox<T>;
  end;
  { A value bound remains a formal value, without selecting a concrete record. }
  TFormalRecordRelay<T: record> = class
    Item: TFormalRecord<T>;
  end;
  { The independent compiler validates a real value specialization as well. }
  TFormalRecordProof = TFormalRecord<LongInt>;

{ The compiler checks generic method bodies and executes three real calls.
  All objects are retired; no UI, source rewriting or external effect occurs. }
function CompilerFormalValue: LongInt;

implementation

function TFormalBase.ReadValue: LongInt;
begin
  Result := Value;
end;

function TFormalClassBox<T>.Read(const AValue: T): LongInt;
begin
  Result := AValue.Value;
end;

function TFormalInterfaceBox<T>.Read(const AValue: T): LongInt;
begin
  Result := AValue.ReadValue;
end;

function TFormalAny<T>.Keep(const AValue: T): T;
begin
  Result := AValue;
end;

function TFormalRecord<T>.Keep(const AValue: T): T;
begin
  Result := AValue;
end;

function CompilerFormalValue: LongInt;
var
  LItem: TFormalItem;
  LClass: TFormalClassBox<TFormalItem>;
  LInterface: TFormalInterfaceBox<TFormalItem>;
  LAny: TFormalAny<LongInt>;
begin
  LItem := TFormalItem.Create;
  try
    LClass := TFormalClassBox<TFormalItem>.Create;
    try
      LInterface := TFormalInterfaceBox<TFormalItem>.Create;
      try
        LAny := TFormalAny<LongInt>.Create;
        try
          LItem.Value := 40;
          Result := LClass.Read(LItem);
          LItem.Value := 1;
          Result := Result + LInterface.Read(LItem) + LAny.Keep(1);
        finally
          LAny.Free;
        end;
      finally
        LInterface.Free;
      end;
    finally
      LClass.Free;
    end;
  finally
    LItem.Free;
  end;
end;

end.
