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

program nyx_unicode_casefold;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, MD5, nyx.text;

const
  { The official Unicode 17.0.0 input is pinned independently of a moving URL.
    This byte identity checks reproducibility, not malicious-input authentication. }
  CInputMD5 = '6b7075ab2647623eab0e52d7dfa4ffbb';

function ReadBytes(const APath: String): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if LStream.Size > 1024 * 1024 then
    begin
      raise Exception.Create('Unicode generator input exceeds one MiB');
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

function HexScalar(const AText: TNyxText): Integer;
begin
  Result := StrToInt('$' + String(AText));

  if (Result < 0) or (Result > $10FFFF) or
    ((Result >= $D800) and (Result <= $DFFF)) then
  begin
    raise Exception.Create('Unicode mapping requires a scalar value');
  end;
end;

var
  LSource: TNyxText;
  LLine: TNyxText;
  LFields: TNyxText;
  LMapping: TNyxText;
  LStatus: TNyxText;
  LRows: TNyxStrings;
  LLicense: TNyxStrings;
  LOutput: TNyxStrings;
  LEntries: TNyxStrings;
  LStream: TFileStream;
  LIndex: Integer;
  LPosition: Integer;
  LScalar: Integer;
  LPrevious: Integer;
  LCount: Integer;
  LValues: array[0..2] of Integer;
  LBytes: TNyxText;
begin
  LRows := nil;
  LLicense := nil;
  LOutput := nil;
  LEntries := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply pinned CaseFolding.txt, Unicode license and output include');
    end;
    LSource := ReadBytes(ParamStr(1));

    if MD5Print(MD5String(RawByteString(LSource))) <> CInputMD5 then
    begin
      raise Exception.Create('Unicode 17.0.0 source fingerprint differs');
    end;
    LRows := TNyxStrings.Create;
    LLicense := TNyxStrings.Create;
    LOutput := TNyxStrings.Create;
    LEntries := TNyxStrings.Create;
    LRows.Text := LSource;
    LLicense.Text := ReadBytes(ParamStr(2));
    LPrevious := -1;
    for LIndex := 0 to LRows.Count - 1 do
    begin
      LLine := LRows[LIndex];
      LPosition := Pos('#', LLine);

      if LPosition > 0 then
      begin
        LLine := Copy(LLine, 1, LPosition - 1);
      end;

      if Trim(String(LLine)) = '' then
      begin
        Continue;
      end;
      LPosition := Pos(';', LLine);
      LScalar := HexScalar(Trim(Copy(LLine, 1, LPosition - 1)));
      LFields := Copy(LLine, LPosition + 1, MaxInt);
      LPosition := Pos(';', LFields);
      LStatus := Trim(Copy(LFields, 1, LPosition - 1));

      if (LStatus = 'S') or (LStatus = 'T') then
      begin
        Continue;
      end;

      if (LStatus <> 'C') and (LStatus <> 'F') then
      begin
        raise Exception.Create('Unknown Unicode case-folding status');
      end;

      if LScalar <= LPrevious then
      begin
        raise Exception.Create('Case-fold mappings must have unique ascending scalars');
      end;
      LPrevious := LScalar;
      LFields := Copy(LFields, LPosition + 1, MaxInt);
      LMapping := Trim(Copy(LFields, 1, Pos(';', LFields) - 1));
      LCount := 0;
      FillChar(LValues, SizeOf(LValues), 0);
      while LMapping <> '' do
      begin

        if LCount = Length(LValues) then
        begin
          raise Exception.Create('Unicode full mapping exceeds three scalars');
        end;
        LPosition := Pos(' ', LMapping);

        if LPosition = 0 then
        begin
          LPosition := Length(LMapping) + 1;
        end;
        LValues[LCount] := HexScalar(Copy(LMapping, 1, LPosition - 1));
        Inc(LCount);
        LMapping := Trim(Copy(LMapping, LPosition + 1, MaxInt));
      end;
      LEntries.Add('    (Scalar: $' + TNyxText(IntToHex(LScalar, 4)) +
        '; Count: ' + TNyxText(IntToStr(LCount)) +
        '; First: $' + TNyxText(IntToHex(LValues[0], 4)) +
        '; Second: $' + TNyxText(IntToHex(LValues[1], 4)) +
        '; Third: $' + TNyxText(IntToHex(LValues[2], 4)) + ')');
    end;
    LOutput.Add('{ Generated by tools/nyx_unicode_casefold.lpr from Unicode 17.0.0.');
    LOutput.Add('  Source: https://www.unicode.org/Public/17.0.0/ucd/CaseFolding.txt');
    LOutput.Add('  SHA-256: ff8d8fefbf123574205085d6714c36149eb946d717a0c585c27f0f4ef58c4183');
    LOutput.Add('  Default full folding uses C/F mappings; no Turkic tailoring or normalization.');
    LOutput.Add('');

    for LIndex := 0 to LLicense.Count - 1 do
    begin

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
    LOutput.Add('  CCaseFold: array[0..' + TNyxText(IntToStr(LEntries.Count - 1)) +
      '] of TNyxCaseFoldMapping = (');
    for LIndex := 0 to LEntries.Count - 1 do
    begin
      LLine := LEntries[LIndex];

      if LIndex < LEntries.Count - 1 then
      begin
        LLine := LLine + ',';
      end;
      LOutput.Add(LLine);
    end;
    LOutput.Add('  );');
    LBytes := LOutput.Join(#10) + #10;
    LStream := TFileStream.Create(ParamStr(3), fmCreate);
    try
      LStream.WriteBuffer(LBytes[1], Length(LBytes));
    finally
      LStream.Free;
    end;
    WriteLn('Generated ', LEntries.Count, ' Unicode full case-fold mappings');
  finally
    LEntries.Free;
    LOutput.Free;
    LLicense.Free;
    LRows.Free;
  end;
end.
