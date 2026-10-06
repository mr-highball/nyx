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

unit nyx.text.search;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text;

{ Unicode 17 default full case folding, independent of locale/ANSI RTL tables.
  Owns its result, preserves source text, and refuses malformed encoding.
  Folding expands some scalars and does not normalize or remove accents. }
function NyxFoldText(const AText: TNyxText): TNyxText;
{ Compare only the necessary prefix of a label against an already folded key.
  Both arguments are ordinary owned text values; no data is retained. }
function NyxStartsWithFolded(const AText, AFoldedPrefix: TNyxText): Boolean;

implementation

uses SysUtils;

type
  TNyxCaseFoldMapping = record
    Scalar: Integer;
    Count: Integer;
    First: Integer;
    Second: Integer;
    Third: Integer;
  end;

{$include nyx.text.casefold.inc}

function FoldScalar(AScalar: Integer): TNyxText;
var
  LLow: Integer;
  LHigh: Integer;
  LMiddle: Integer;
begin
  LLow := 0;
  LHigh := High(CCaseFold);
  while LLow <= LHigh do
  begin
    LMiddle := LLow + (LHigh - LLow) div 2;

    if CCaseFold[LMiddle].Scalar < AScalar then
    begin
      LLow := LMiddle + 1;
    end
    else if CCaseFold[LMiddle].Scalar > AScalar then
    begin
      LHigh := LMiddle - 1;
    end
    else
    begin
      Result := NyxScalarText(CCaseFold[LMiddle].First);

      if CCaseFold[LMiddle].Count > 1 then
      begin
        Result := Result + NyxScalarText(CCaseFold[LMiddle].Second);
      end;

      if CCaseFold[LMiddle].Count > 2 then
      begin
        Result := Result + NyxScalarText(CCaseFold[LMiddle].Third);
      end;
      Exit;
    end;
  end;
  Result := NyxScalarText(AScalar);
end;

function NyxFoldText(const AText: TNyxText): TNyxText;
var
  LIndex: Integer;
  LScalar: Integer;
  LParts: TNyxStrings;
begin
  LParts := TNyxStrings.Create;
  try
    LIndex := 1;
    while LIndex <= Length(AText) do
    begin

      if not NyxNextScalar(AText, LIndex, LScalar) then
      begin
        raise EArgumentException.Create('Search text has malformed Unicode');
      end;
      LParts.Add(FoldScalar(LScalar));
    end;
    { Join sizes the native result once; repeated concatenation would copy the
      growing label at every scalar. Browser uses its standard array join. }
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function NyxStartsWithFolded(const AText, AFoldedPrefix: TNyxText): Boolean;
var
  LIndex: Integer;
  LScalar: Integer;
  LPosition: Integer;
  LPart: TNyxText;
  LCount: Integer;
begin
  LIndex := 1;
  LPosition := 1;
  while LPosition <= Length(AFoldedPrefix) do
  begin

    if LIndex > Length(AText) then
    begin
      Exit(False);
    end;

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise EArgumentException.Create('Search label has malformed Unicode');
    end;
    LPart := FoldScalar(LScalar);
    LCount := Length(AFoldedPrefix) - LPosition + 1;

    if Length(LPart) < LCount then
    begin
      LCount := Length(LPart);
    end;

    if Copy(LPart, 1, LCount) <> Copy(AFoldedPrefix, LPosition, LCount) then
    begin
      Exit(False);
    end;
    Inc(LPosition, LCount);
  end;
  Result := True;
end;

end.
