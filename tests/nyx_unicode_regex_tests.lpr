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


program nyx_unicode_regex_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, MD5, nyx.text, nyx.data, nyx.unicode, nyx.unicode.types;

type
  { Qualification-only dense source oracle, independently read from pinned UCD
    input. Product tables remain compressed binary-search intervals/mappings. }
  TCategoryOracle = array[0..$10FFFF] of TNyxUnicodeCategory;
  { Qualification-only scalar map initialized to identity before C/S mappings. }
  TFoldOracle = array[0..$10FFFF] of Integer;
  { Portable bounded Adler components over four little-endian bytes per value.
    This transport fingerprint supplements exact native scalar comparisons; it
    is not a cryptographic authentication claim or the sole UCD admission guard. }
  TDigest = record
    A: Integer;
    B: Integer;
  end;

var
  GCategories: TCategoryOracle;
  GFolds: TFoldOracle;
  GChecks: Integer;

function ReadBytes(const APath: String): TNyxText;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if LFile.Size > 8 * 1024 * 1024 then
    begin
      raise Exception.Create('UCD qualification input exceeds eight MiB');
    end;
    SetLength(Result, LFile.Size);
    LFile.ReadBuffer(Result[1], Length(Result));
  finally
    LFile.Free;
  end;
end;

function Take(var ALine: TNyxText): TNyxText;
var
  LEnd: Integer;
begin
  LEnd := Pos(';', ALine);
  Result := Trim(Copy(ALine, 1, LEnd - 1));
  ALine := Copy(ALine, LEnd + 1, MaxInt);
end;

procedure Add(var ADigest: TDigest; AValue: Integer);
var
  LByte: Integer;
begin
  for LByte := 0 to 3 do
  begin
    ADigest.A := (ADigest.A + (AValue and 255)) mod 65521;
    ADigest.B := (ADigest.B + ADigest.A) mod 65521;
    AValue := AValue shr 8;
  end;
end;

function Text(const ADigest: TDigest): TNyxText;
begin
  Result := TNyxText(IntToStr(ADigest.A)) + ':' + TNyxText(IntToStr(ADigest.B));
end;

var
  LRows: TNyxStrings;
  LSource: TNyxText;
  LLine: TNyxText;
  LName: TNyxText;
  LStatus: TNyxText;
  LScalar: Integer;
  LStart: Integer;
  LEnd: Integer;
  LIndex: Integer;
  LCategory: TNyxUnicodeCategory;
  LCategoryDigest: TDigest;
  LFoldDigest: TDigest;
  LReply: TNyxDataValue;
  LFile: TFileStream;
begin
  GChecks := 0;
  LRows := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply pinned UnicodeData.txt, CaseFolding.txt and output JSON');
    end;
    LRows := TNyxStrings.Create;
    LSource := ReadBytes(ParamStr(1));

    if MD5Print(MD5String(RawByteString(LSource))) <> '959cb19fcda0240caef8c02953e3d771' then
    begin
      raise Exception.Create('Unexpected Unicode 17 category input');
    end;
    LRows.Text := LSource;
    LStart := -1;
    for LIndex := 0 to LRows.Count - 1 do
    begin
      LLine := LRows[LIndex];

      if LLine = '' then
      begin
        Continue;
      end;
      LScalar := StrToInt('$' + Take(LLine));
      LName := Take(LLine);
      LCategory := NyxUnicodeCategoryFromName(Take(LLine));

      if Pos(', First>', LName) > 0 then
      begin
        LStart := LScalar;
      end
      else if Pos(', Last>', LName) > 0 then
      begin
        for LEnd := LStart to LScalar do
        begin
          GCategories[LEnd] := LCategory;
        end;
        LStart := -1;
      end
      else
      begin
        GCategories[LScalar] := LCategory;
      end;
    end;
    for LScalar := 0 to $10FFFF do
    begin
      GFolds[LScalar] := LScalar;
    end;
    LSource := ReadBytes(ParamStr(2));

    if MD5Print(MD5String(RawByteString(LSource))) <> '6b7075ab2647623eab0e52d7dfa4ffbb' then
    begin
      raise Exception.Create('Unexpected Unicode 17 folding input');
    end;
    LRows.Text := LSource;
    for LIndex := 0 to LRows.Count - 1 do
    begin
      LLine := LRows[LIndex];

      if (LLine = '') or (LLine[1] = '#') then
      begin
        Continue;
      end;
      LScalar := StrToInt('$' + Take(LLine));
      LStatus := Take(LLine);

      if (LStatus = 'C') or (LStatus = 'S') then
      begin
        GFolds[LScalar] := StrToInt('$' + Take(LLine));
      end;
    end;
    LCategoryDigest := Default(TDigest);
    LFoldDigest := Default(TDigest);
    LCategoryDigest.A := 1;
    LFoldDigest.A := 1;
    for LScalar := 0 to $10FFFF do
    begin

      if (LScalar >= $D800) and (LScalar <= $DFFF) then
      begin
        Continue;
      end;

      if NyxScalarCategory(LScalar) <> GCategories[LScalar] then
      begin
        raise Exception.Create('Category differs at U+' + IntToHex(LScalar, 4));
      end;
      Inc(GChecks);

      if NyxSimpleFold(LScalar) <> GFolds[LScalar] then
      begin
        raise Exception.Create('Simple folding differs at U+' + IntToHex(LScalar, 4));
      end;
      Inc(GChecks);
      Add(LCategoryDigest, Ord(GCategories[LScalar]));
      Add(LFoldDigest, GFolds[LScalar]);
    end;
    LReply := NyxObject([NyxField('unicode', NyxData('17.0.0')),
      NyxField('checks', NyxData(GChecks)), NyxField('category', NyxData(Text(LCategoryDigest))),
      NyxField('fold', NyxData(Text(LFoldDigest)))]);
    LSource := LReply.ToJSON;
    LFile := TFileStream.Create(ParamStr(3), fmCreate);
    try
      LFile.WriteBuffer(LSource[1], Length(LSource));
    finally
      LFile.Free;
    end;
    WriteLn('PASS ', GChecks, ' exact pinned-UCD scalar checks / ', LSource);
  finally
    LRows.Free;
  end;
end.
