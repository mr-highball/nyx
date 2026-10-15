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


unit nyx.unicode;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.unicode.types;

{ Unicode 17 General_Category for a scalar, including unassigned/noncharacters.
  Invalid/surrogate code points raise; locale and machine codepage never enter. }
function NyxScalarCategory(AScalar: Integer): TNyxUnicodeCategory;
{ Default simple Unicode 17 folding (C/S), exactly one scalar per scalar.
  Distinct from NyxFoldText's full folding: sharp-s does not expand to ss here. }
function NyxSimpleFold(AScalar: Integer): Integer;
{ Simple-fold equivalence closure in deterministic order. Index zero is the
  original scalar, one its canonical fold, then reverse mappings. Duplicates
  are permitted; false marks the end. No iterator or mutable storage is retained. }
function NyxScalarCaseEquivalent(AScalar, AIndex: Integer; out AValue: Integer): Boolean;
{ Unicode word membership used by the portable regex contract: letters, marks,
  decimal numbers, connector punctuation and the two join controls. }
function NyxScalarWord(AScalar: Integer): Boolean;

implementation

type
  { Immutable compressed Unicode category interval. Generated ranges cover all
    code points; public admission excludes surrogate values before lookup. }
  TUnicodeRange = record
    First: Integer;
    Last: Integer;
    Category: TNyxUnicodeCategory;
  end;
  { Immutable binary-search mapping. Forward keys map scalars to simple folds;
    reverse keys map folds to their sorted equivalent source scalars. }
  TUnicodeMapping = record
    Key: Integer;
    Value: Integer;
  end;

{$include nyx.unicode.regex.inc}

procedure RequireScalar(AScalar: Integer);
begin

  if (AScalar < 0) or (AScalar > $10FFFF) or
    ((AScalar >= $D800) and (AScalar <= $DFFF)) then
  begin
    raise EArgumentException.Create('Unicode property lookup requires a scalar');
  end;
end;

function NyxScalarCategory(AScalar: Integer): TNyxUnicodeCategory;
var
  LLow: Integer;
  LHigh: Integer;
  LMiddle: Integer;
begin
  RequireScalar(AScalar);
  LLow := 0;
  LHigh := High(CUnicodeRanges);
  while LLow <= LHigh do
  begin
    LMiddle := LLow + (LHigh - LLow) div 2;

    if AScalar < CUnicodeRanges[LMiddle].First then
    begin
      LHigh := LMiddle - 1;
    end
    else if AScalar > CUnicodeRanges[LMiddle].Last then
    begin
      LLow := LMiddle + 1;
    end
    else
    begin
      Exit(CUnicodeRanges[LMiddle].Category);
    end;
  end;
  raise Exception.Create('Unicode range database is incomplete');
end;

function NyxSimpleFold(AScalar: Integer): Integer;
var
  LLow: Integer;
  LHigh: Integer;
  LMiddle: Integer;
begin
  RequireScalar(AScalar);
  LLow := 0;
  LHigh := High(CUnicodeFold);
  while LLow <= LHigh do
  begin
    LMiddle := LLow + (LHigh - LLow) div 2;

    if CUnicodeFold[LMiddle].Key < AScalar then
    begin
      LLow := LMiddle + 1;
    end
    else if CUnicodeFold[LMiddle].Key > AScalar then
    begin
      LHigh := LMiddle - 1;
    end
    else
    begin
      Exit(CUnicodeFold[LMiddle].Value);
    end;
  end;
  Result := AScalar;
end;

function NyxScalarCaseEquivalent(AScalar, AIndex: Integer; out AValue: Integer): Boolean;
var
  LFold: Integer;
  LLow: Integer;
  LHigh: Integer;
  LMiddle: Integer;
begin
  RequireScalar(AScalar);

  if AIndex < 0 then
  begin
    raise EArgumentException.Create('Case closure index must be nonnegative');
  end;
  AValue := AScalar;

  if AIndex = 0 then
  begin
    Exit(True);
  end;
  LFold := NyxSimpleFold(AScalar);
  AValue := LFold;

  if AIndex = 1 then
  begin
    Exit(True);
  end;
  LLow := 0;
  LHigh := Length(CUnicodeReverseFold);
  while LLow < LHigh do
  begin
    LMiddle := LLow + (LHigh - LLow) div 2;

    if CUnicodeReverseFold[LMiddle].Key < LFold then
    begin
      LLow := LMiddle + 1;
    end
    else
    begin
      LHigh := LMiddle;
    end;
  end;
  if AIndex - 2 >= Length(CUnicodeReverseFold) - LLow then
  begin
    Exit(False);
  end;
  LMiddle := LLow + AIndex - 2;
  Result := (LMiddle < Length(CUnicodeReverseFold)) and
    (CUnicodeReverseFold[LMiddle].Key = LFold);

  if Result then
  begin
    AValue := CUnicodeReverseFold[LMiddle].Value;
  end;
end;

function NyxScalarWord(AScalar: Integer): Boolean;
begin
  Result := (AScalar = $200C) or (AScalar = $200D) or
    (NyxScalarCategory(AScalar) in [nucUppercaseLetter, nucLowercaseLetter,
      nucTitlecaseLetter, nucModifierLetter, nucOtherLetter, nucNonspacingMark,
      nucSpacingMark, nucEnclosingMark, nucDecimalNumber, nucConnectorPunctuation]);
end;

end.
