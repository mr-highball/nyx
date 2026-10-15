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


program nyx_unicode_regex;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, TypInfo, MD5, nyx.text, nyx.unicode.types;

const
  CUnicodeMD5 = '959cb19fcda0240caef8c02953e3d771';
  CFoldingMD5 = '6b7075ab2647623eab0e52d7dfa4ffbb';

type
  { Generator-only complete scalar property map. It owns no text; compressed
    immutable ranges, not this temporary 0x110000-entry map, enter the product. }
  TCategories = array[0..$10FFFF] of TNyxUnicodeCategory;

function ReadBytes(const APath: String): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if LStream.Size > 8 * 1024 * 1024 then
    begin
      raise Exception.Create('Unicode generator input exceeds eight MiB');
    end;
    SetLength(Result, LStream.Size);

    if Length(Result) > 0 then
    begin
      LStream.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LStream.Free;
  end;
end;

function Take(var AText: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  LIndex := Pos(';', AText);

  if LIndex = 0 then
  begin
    LIndex := Length(AText) + 1;
  end;
  Result := Trim(Copy(AText, 1, LIndex - 1));
  AText := Copy(AText, LIndex + 1, MaxInt);
end;

function Hex(const AText: TNyxText): Integer;
begin
  Result := StrToInt('$' + String(AText));

  if (Result < 0) or (Result > $10FFFF) then
  begin
    raise Exception.Create('Unicode code point is out of range');
  end;
end;

var
  LCategories: TCategories;
  LRows: TNyxStrings;
  LOutput: TNyxStrings;
  LRanges: TNyxStrings;
  LFolds: TNyxStrings;
  LReverse: TStringList;
  LLicense: TNyxStrings;
  LText: TNyxText;
  LLine: TNyxText;
  LName: TNyxText;
  LStatus: TNyxText;
  LFirst: Integer;
  LLast: Integer;
  LScalar: Integer;
  LPrevious: Integer;
  LPending: Integer;
  LIndex: Integer;
  LPart: Integer;
  LFold: Integer;
  LCategory: TNyxUnicodeCategory;
  LPendingCategory: TNyxUnicodeCategory;
  LStream: TFileStream;

  procedure Emit(const AName, AType: TNyxText; AValues: TNyxStrings);
  var
    LItem: Integer;
    LValue: TNyxText;
  begin
    LOutput.Add('  ' + AName + ': array[0..' +
      TNyxText(IntToStr(AValues.Count - 1)) + '] of ' + AType + ' = (');
    for LItem := 0 to AValues.Count - 1 do
    begin
      LValue := AValues[LItem];

      if LItem < AValues.Count - 1 then
      begin
        LValue := LValue + ',';
      end;
      LOutput.Add('    ' + LValue);
    end;
    LOutput.Add('  );');
  end;

begin
  LRows := nil;
  LOutput := nil;
  LRanges := nil;
  LFolds := nil;
  LReverse := nil;
  LLicense := nil;
  try

    if ParamCount <> 4 then
    begin
      raise Exception.Create('Supply UnicodeData.txt, CaseFolding.txt, Unicode license and output include');
    end;
    LRows := TNyxStrings.Create;
    LOutput := TNyxStrings.Create;
    LRanges := TNyxStrings.Create;
    LFolds := TNyxStrings.Create;
    { ASCII-only hexadecimal sort keys, not owned Unicode application text. }
    LReverse := TStringList.Create;
    LReverse.Sorted := True;
    LReverse.Duplicates := dupError;
    LLicense := TNyxStrings.Create;
    LLicense.Text := ReadBytes(ParamStr(3));
    LText := ReadBytes(ParamStr(1));

    if MD5Print(MD5String(RawByteString(LText))) <> CUnicodeMD5 then
    begin
      raise Exception.Create('Unicode 17.0.0 UnicodeData byte identity differs');
    end;
    LRows.Text := LText;
    LPrevious := -1;
    LPending := -1;
    FillChar(LCategories, SizeOf(LCategories), 0);
    for LIndex := 0 to LRows.Count - 1 do
    begin
      LLine := LRows[LIndex];

      if LLine = '' then
      begin
        Continue;
      end;
      LScalar := Hex(Take(LLine));
      LName := Take(LLine);
      LCategory := NyxUnicodeCategoryFromName(Take(LLine));

      if LScalar <= LPrevious then
      begin
        raise Exception.Create('Unicode data is not strictly ordered');
      end;
      LPrevious := LScalar;

      if Pos(', First>', LName) > 0 then
      begin

        if LPending >= 0 then
        begin
          raise Exception.Create('Nested Unicode ranges');
        end;
        LPending := LScalar;
        LPendingCategory := LCategory;
        Continue;
      end;

      if Pos(', Last>', LName) > 0 then
      begin

        if (LPending < 0) or (LPendingCategory <> LCategory) then
        begin
          raise Exception.Create('Unpaired Unicode range');
        end;
        for LPart := LPending to LScalar do
        begin
          LCategories[LPart] := LCategory;
        end;
        LPending := -1;
        Continue;
      end;

      if LPending >= 0 then
      begin
        raise Exception.Create('Interrupted Unicode range');
      end;
      LCategories[LScalar] := LCategory;
    end;

    if LPending >= 0 then
    begin
      raise Exception.Create('Unclosed Unicode range');
    end;
    LFirst := 0;
    while LFirst <= High(LCategories) do
    begin
      LLast := LFirst;
      while (LLast < High(LCategories)) and
        (LCategories[LLast + 1] = LCategories[LFirst]) do
      begin
        Inc(LLast);
      end;
      LRanges.Add('(First: $' + TNyxText(IntToHex(LFirst, 4)) +
        '; Last: $' + TNyxText(IntToHex(LLast, 4)) + '; Category: ' +
        TNyxText(GetEnumName(TypeInfo(TNyxUnicodeCategory), Ord(LCategories[LFirst]))) + ')');
      LFirst := LLast + 1;
    end;
    LText := ReadBytes(ParamStr(2));

    if MD5Print(MD5String(RawByteString(LText))) <> CFoldingMD5 then
    begin
      raise Exception.Create('Unicode 17.0.0 CaseFolding byte identity differs');
    end;
    LRows.Text := LText;
    LPrevious := -1;
    for LIndex := 0 to LRows.Count - 1 do
    begin
      LLine := LRows[LIndex];
      LPart := Pos('#', LLine);

      if LPart > 0 then
      begin
        LLine := Copy(LLine, 1, LPart - 1);
      end;

      if Trim(LLine) = '' then
      begin
        Continue;
      end;
      LScalar := Hex(Take(LLine));
      LStatus := Take(LLine);
      LName := Take(LLine);

      if (LStatus = 'F') or (LStatus = 'T') then
      begin
        Continue;
      end;

      if ((LStatus <> 'C') and (LStatus <> 'S')) or (Pos(' ', LName) > 0) or
        (LScalar <= LPrevious) then
      begin
        raise Exception.Create('Simple folding requires ordered unique C/S scalar mappings');
      end;
      LPrevious := LScalar;
      LFold := Hex(LName);
      LFolds.Add('(Key: $' + TNyxText(IntToHex(LScalar, 4)) +
        '; Value: $' + TNyxText(IntToHex(LFold, 4)) + ')');
      LReverse.Add(IntToHex(LFold, 6) + ';' + IntToHex(LScalar, 6));
    end;
    LOutput.Add('{ Generated by tools/nyx_unicode_regex.lpr from Unicode 17.0.0.');
    LOutput.Add('  Sources: https://www.unicode.org/Public/17.0.0/ucd/UnicodeData.txt');
    LOutput.Add('           https://www.unicode.org/Public/17.0.0/ucd/CaseFolding.txt');
    LOutput.Add('  UnicodeData SHA-256: 2e1efc1dcb59c575eedf5ccae60f95229f706ee6d031835247d843c11d96470c');
    LOutput.Add('  CaseFolding SHA-256: ff8d8fefbf123574205085d6714c36149eb946d717a0c585c27f0f4ef58c4183');
    LOutput.Add('  General categories and default simple C/S folding; no normalization/Turkic tailoring.');
    LOutput.Add('');
    for LIndex := 0 to LLicense.Count - 1 do
    begin
      { Preserve the full notice while keeping its blank lines free of trailing
        indentation, so regenerated Pascal also satisfies the source style. }

      if LLicense[LIndex] = '' then
      begin
        LOutput.Add('');
      end
      else
      begin
        LOutput.Add('  ' + LLicense[LIndex]);
      end;
    end;
    LOutput.Add('}');
    LOutput.Add('const');
    Emit('CUnicodeRanges', 'TUnicodeRange', LRanges);
    Emit('CUnicodeFold', 'TUnicodeMapping', LFolds);
    LFolds.Clear;
    for LIndex := 0 to LReverse.Count - 1 do
    begin
      LLine := TNyxText(LReverse[LIndex]);
      LScalar := Hex(Take(LLine));
      LFold := Hex(LLine);
      LFolds.Add('(Key: $' + TNyxText(IntToHex(LScalar, 4)) +
        '; Value: $' + TNyxText(IntToHex(LFold, 4)) + ')');
    end;
    Emit('CUnicodeReverseFold', 'TUnicodeMapping', LFolds);
    LText := LOutput.Join(#10) + #10;
    LStream := TFileStream.Create(ParamStr(4), fmCreate);
    try
      LStream.WriteBuffer(LText[1], Length(LText));
    finally
      LStream.Free;
    end;
    WriteLn('Generated ', LRanges.Count, ' category ranges and ', LFolds.Count, ' simple fold mappings');
  finally
    LLicense.Free;
    LReverse.Free;
    LFolds.Free;
    LRanges.Free;
    LOutput.Free;
    LRows.Free;
  end;
end.
