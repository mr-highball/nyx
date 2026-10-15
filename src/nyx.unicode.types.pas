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


unit nyx.unicode.types;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text;

type
  { Unicode General_Category values, pinned to the generated Unicode database.
    Names/ordinals describe character properties, never native codepages. Cn
    includes unassigned scalars; surrogate code points are not portable scalars. }
  TNyxUnicodeCategory = (nucUnassigned, nucUppercaseLetter, nucLowercaseLetter,
    nucTitlecaseLetter, nucModifierLetter, nucOtherLetter, nucNonspacingMark,
    nucSpacingMark, nucEnclosingMark, nucDecimalNumber, nucLetterNumber,
    nucOtherNumber, nucConnectorPunctuation, nucDashPunctuation, nucOpenPunctuation,
    nucClosePunctuation, nucInitialPunctuation, nucFinalPunctuation,
    nucOtherPunctuation, nucMathSymbol, nucCurrencySymbol, nucModifierSymbol,
    nucOtherSymbol, nucSpaceSeparator, nucLineSeparator, nucParagraphSeparator,
    nucControl, nucFormat, nucSurrogate, nucPrivateUse);

{ Stable UCD two-letter property value. Invalid cast enum values raise. }
function NyxUnicodeCategoryName(AValue: TNyxUnicodeCategory): TNyxText;
{ Exact UCD abbreviation; this low-level data boundary refuses unknown names. }
function NyxUnicodeCategoryFromName(const AName: TNyxText): TNyxUnicodeCategory;

implementation

const
  CNames: array[TNyxUnicodeCategory] of TNyxText =
    ('Cn', 'Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Mn', 'Mc', 'Me', 'Nd', 'Nl', 'No',
     'Pc', 'Pd', 'Ps', 'Pe', 'Pi', 'Pf', 'Po', 'Sm', 'Sc', 'Sk', 'So', 'Zs',
     'Zl', 'Zp', 'Cc', 'Cf', 'Cs', 'Co');

function NyxUnicodeCategoryName(AValue: TNyxUnicodeCategory): TNyxText;
var
  LIndex: Integer;
begin
  LIndex := Ord(AValue);

  if (LIndex < Ord(Low(TNyxUnicodeCategory))) or
    (LIndex > Ord(High(TNyxUnicodeCategory))) then
  begin
    raise EArgumentException.Create('Unknown Unicode general category');
  end;
  Result := CNames[AValue];
end;

function NyxUnicodeCategoryFromName(const AName: TNyxText): TNyxUnicodeCategory;
var
  LValue: TNyxUnicodeCategory;
begin
  for LValue := Low(TNyxUnicodeCategory) to High(TNyxUnicodeCategory) do
  begin

    if CNames[LValue] = AName then
    begin
      Exit(LValue);
    end;
  end;
  raise EArgumentException.Create('Unknown Unicode general category name');
end;

end.
