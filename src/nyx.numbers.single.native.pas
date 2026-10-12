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
unit nyx.numbers.single.native;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch typehelpers}

interface

uses
  nyx.numbers.single, nyx.linq.numeric
  {$ifndef PAS2JS}, nyx.linq{$endif};

{$ifndef PAS2JS}
type
  { Native array capture owns a detached vector of portable binary32 values.
    Caller changes/disposal cannot alter the query. Policy explicitly admits or
    refuses non-finite values; helper lookup uses this exported native alias. }
  TNyxNativeSingles = TArray<Single>;
  TNyxNativeSinglesHelper = type helper for TNyxNativeSingles
    function AsNyx(APolicy: TNyxNumberPolicy = nnpFinite): INyxSequence<TNyxSingleNumber>;
  end;

{ Lossless native binary32 bridges. Native finite values are already exactly
  representable; the portable factory still validates the caller's number policy. }
function NyxSingleFromNative(AValue: Single;
  APolicy: TNyxNumberPolicy = nnpFinite): TNyxSingleNumber;
function NyxSingleToNative(const AValue: TNyxSingleNumber): Single;
{$endif}

implementation

{$ifndef PAS2JS}
function NyxSingleFromNative(AValue: Single; APolicy: TNyxNumberPolicy): TNyxSingleNumber;
begin
  Result := TNyxSingleNumber.FromNumber(AValue, APolicy);
end;

function NyxSingleToNative(const AValue: TNyxSingleNumber): Single;
begin
  Result := AValue.Value;
end;

function TNyxNativeSinglesHelper.AsNyx(APolicy: TNyxNumberPolicy):
  INyxSequence<TNyxSingleNumber>;
var
  LValues: TNyxSequenceArray<TNyxSingleNumber>;
  LIndex: Integer;
begin
  SetLength(LValues, Length(Self));
  for LIndex := 0 to High(Self) do
  begin
    LValues[LIndex] := NyxSingleFromNative(Self[LIndex], APolicy);
  end;
  Result := TNyxSequence<TNyxSingleNumber>.From(LValues);
end;
{$endif}

end.
