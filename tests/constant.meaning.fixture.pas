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
unit constant.meaning.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

const
  Seed = 7;
  Answer = Seed * 6;
  LargeExact = 9007199254740993;
type
  { A source-defined range qualifies bounds without assuming a host ABI. }
  TAuthoredBand = -2..42;
  { A distinct alias must retain the underlying source ordinal range. }
  TAuthoredAlias = type TAuthoredBand;
  { Explicit enum values remain ordinal values, requiring Ord for arithmetic. }
  TAuthoredChoice = (acFirst = 3, acMiddle, acLast = 9);
const
  RangeWidth = High(TAuthoredAlias) - Low(TAuthoredAlias) + 1;
  LastOrdinal = Ord(High(TAuthoredChoice));
  NegativeQuotient = -19 div 3;
  NegativeRemainder = -19 mod 3;

{ Compiled whole-source proof, separate from the resolver's interpretation. }
function NativeConstantMeaning(AExpectedWidth: Integer): Integer;

implementation

function NativeConstantMeaning(AExpectedWidth: Integer): Integer;
begin
  Result := Answer;

  if (RangeWidth <> AExpectedWidth) or (LastOrdinal <> 9) or
    (NegativeQuotient <> -6) or (NegativeRemainder <> -1) or
    (LargeExact <> 9007199254740993) then
  begin
    Result := -1;
  end;
end;

end.
