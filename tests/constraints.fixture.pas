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
unit constraints.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { A real class argument, independent of a resolver's classification. }
  TConstraintItem = class
    Value: LongInt;
  end;
  { A real record argument owns only a scalar; no interface occurs in a record. }
  TConstraintValue = record
    Value: LongInt;
  end;
  { The compiler enforces the reference-type requirement on this owned field. }
  TClassBox<T: class> = class
    Item: T;
  end;
  { Delphi-mode FPC also admits scalar value arguments for RECORD. }
  TValueBox<T: record> = class
    Item: T;
  end;

{ Exercise accepted class/record/scalar specializations with real field access.
  The boxes borrow no application/UI; the function retires every owned object. }
function CompilerConstraintValue: LongInt;

implementation

uses constraints.objfpc.fixture;

function CompilerConstraintValue: LongInt;
var
  LClass: TClassBox<TConstraintItem>;
  LRecord: TValueBox<TConstraintValue>;
  LScalar: TValueBox<LongInt>;
  LForeign: TForeignValueBox<LongInt>;
begin
  LClass := TClassBox<TConstraintItem>.Create;
  try
    LClass.Item := TConstraintItem.Create;
    try
      LRecord := TValueBox<TConstraintValue>.Create;
      try
        LScalar := TValueBox<LongInt>.Create;
        try
          LForeign := TForeignValueBox<LongInt>.Create;
          try
            LClass.Item.Value := 39;
            LRecord.Item.Value := 1;
            LScalar.Item := 1;
            LForeign.Item := 1;
            Result := LClass.Item.Value + LRecord.Item.Value + LScalar.Item + LForeign.Item;
          finally
            LForeign.Free;
          end;
        finally
          LScalar.Free;
        end;
      finally
        LRecord.Free;
      end;
    finally
      LClass.Item.Free;
    end;
  finally
    LClass.Free;
  end;
end;

end.
