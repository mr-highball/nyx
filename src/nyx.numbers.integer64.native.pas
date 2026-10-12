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
unit nyx.numbers.integer64.native;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch typehelpers}

interface

uses
  nyx.numbers.integer64
  {$ifndef PAS2JS}, nyx.linq{$endif};

{$ifndef PAS2JS}
type
  { Native Int64 bridge never converts through Double. Owned capture preserves
    exact values after the caller changes/releases its array; helpers apply to
    this exported alias, matching the qualified native helper lookup contract. }
  TNyxNativeIntegers64 = TArray<Int64>;
  TNyxNativeIntegers64Helper = type helper for TNyxNativeIntegers64
    function AsNyx: INyxSequence<TNyxInteger64>;
  end;

function NyxInteger64FromNative(AValue: Int64): TNyxInteger64;
function NyxInteger64ToNative(const AValue: TNyxInteger64): Int64;
{$endif}

implementation

{$ifndef PAS2JS}
function NyxInteger64FromNative(AValue: Int64): TNyxInteger64;
var
  LMagnitude: QWord;
begin

  if AValue < 0 then
  begin
    LMagnitude := QWord(-(AValue + 1)) + 1;
  end
  else
  begin
    LMagnitude := QWord(AValue);
  end;
  Result := TNyxInteger64.FromParts(Cardinal(LMagnitude shr 32),
    Cardinal(LMagnitude and $ffffffff), AValue < 0);
end;

function NyxInteger64ToNative(const AValue: TNyxInteger64): Int64;
var
  LMagnitude: Int64;
begin

  if AValue.Negative and (AValue.HighPart = $80000000) then
  begin
    Exit(Low(Int64));
  end;
  LMagnitude := Int64(AValue.HighPart) * Int64(4294967296) + Int64(AValue.LowPart);

  if AValue.Negative then
  begin
    Result := -LMagnitude;
  end
  else
  begin
    Result := LMagnitude;
  end;
end;

function TNyxNativeIntegers64Helper.AsNyx: INyxSequence<TNyxInteger64>;
var
  LValues: TNyxSequenceArray<TNyxInteger64>;
  LIndex: Integer;
begin
  SetLength(LValues, Length(Self));
  for LIndex := 0 to High(Self) do
  begin
    LValues[LIndex] := NyxInteger64FromNative(Self[LIndex]);
  end;
  Result := TNyxSequence<TNyxInteger64>.From(LValues);
end;
{$endif}

end.
