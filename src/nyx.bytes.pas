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

unit nyx.bytes;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text;

type
  { Owned bytes at persistence/import boundaries. Every public decoder returns
    an independent array and publishes only after complete admission. }
  TNyxBytes = array of Byte;
  ENyxBytes = class(Exception);

const
  NyxMaximumPackedBytes = 1048576;

{ Compact canonical Base64, including empty data. Wrong alphabet, padding,
  unused pad bits and byte budgets refuse; no caller result is modified. }
function NyxDecodeBase64(const AText: TNyxText;
  AMaximumBytes: Integer = NyxMaximumPackedBytes): TNyxBytes;
{ Borrowed input; bounded chunks avoid quadratic whole-string writes in pas2js. }
function NyxEncodeBase64(const ABytes: TNyxBytes;
  AMaximumBytes: Integer = NyxMaximumPackedBytes): TNyxText;
{ Exact UTF-8 interchange; malformed encodings refuse, supplementary scalars and
  embedded NUL survive. Native ANSI conversion/global codepages are never used. }
function NyxDecodeUTF8(const ABytes: TNyxBytes): TNyxText;
function NyxEncodeUTF8(const AText: TNyxText): TNyxBytes;
function NyxUTF8ByteCount(const AText: TNyxText): Integer;

implementation

const
  CAlphabet: TNyxText = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function NyxDecodeBase64(const AText: TNyxText; AMaximumBytes: Integer): TNyxBytes;
var
  LPadding: Integer;
  LIndex: Integer;
  LOutput: Integer;
  LPart: Integer;
  LValues: array[0..3] of Integer;
  LLength: Integer;
  LBytes: TNyxBytes;
begin
  LBytes := nil;
  LLength := Length(AText);

  if (AMaximumBytes < 0) or (AMaximumBytes > NyxMaximumPackedBytes) then
  begin
    raise ENyxBytes.Create('Invalid byte budget');
  end;

  if LLength = 0 then
  begin
    Result := LBytes;
    Exit;
  end;

  if (LLength = 0) or (LLength mod 4 <> 0) or
    (LLength > ((AMaximumBytes + 2) div 3) * 4) then
  begin
    raise ENyxBytes.Create('Packed data exceeds the base64 byte budget or has invalid length');
  end;
  LPadding := 0;

  if AText[LLength] = '=' then
  begin
    Inc(LPadding);

    if AText[LLength - 1] = '=' then
    begin
      Inc(LPadding);
    end;
  end;
  LOutput := (LLength div 4) * 3 - LPadding;

  if LOutput > AMaximumBytes then
  begin
    raise ENyxBytes.Create('Packed data exceeds the decoded byte budget');
  end;
  SetLength(LBytes, LOutput);
  LOutput := 0;
  LIndex := 1;
  while LIndex <= LLength do
  begin
    for LPart := 0 to 3 do
    begin
      LValues[LPart] := Pos(AText[LIndex + LPart], CAlphabet) - 1;

      if (LValues[LPart] < 0) and (AText[LIndex + LPart] = '=') and
        (LIndex + LPart > LLength - LPadding) then
      begin
        LValues[LPart] := 0;
      end
      else if LValues[LPart] < 0 then
      begin
        raise ENyxBytes.Create('Packed data requires compact canonical base64');
      end;
    end;

    if (LIndex + 3 = LLength) and
      (((LPadding = 2) and (LValues[1] and 15 <> 0)) or
      ((LPadding = 1) and (LValues[2] and 3 <> 0))) then
    begin
      raise ENyxBytes.Create('Packed data has noncanonical base64 pad bits');
    end;
    LBytes[LOutput] := (LValues[0] shl 2) or (LValues[1] shr 4);
    Inc(LOutput);

    if LOutput < Length(LBytes) then
    begin
      LBytes[LOutput] := ((LValues[1] and 15) shl 4) or (LValues[2] shr 2);
      Inc(LOutput);
    end;

    if LOutput < Length(LBytes) then
    begin
      LBytes[LOutput] := ((LValues[2] and 3) shl 6) or LValues[3];
      Inc(LOutput);
    end;
    Inc(LIndex, 4);
  end;
  Result := LBytes;
end;

function NyxEncodeBase64(const ABytes: TNyxBytes; AMaximumBytes: Integer): TNyxText;
var
  LIndex: Integer;
  LFirst: Integer;
  LSecond: Integer;
  LThird: Integer;
  LGroup: TNyxText;
  LChunk: TNyxText;
  LParts: TNyxStrings;
begin

  if (AMaximumBytes < 0) or (AMaximumBytes > NyxMaximumPackedBytes) or
    (Length(ABytes) > AMaximumBytes) then
  begin
    raise ENyxBytes.Create('Packed data bytes must fit the declared budget');
  end;
  { Bounded chunks avoid whole-string indexed mutation on pas2js. The portable
    text join performs one complete output allocation on either target. }
  LParts := TNyxStrings.Create;
  try
    LChunk := '';
    LIndex := 0;
    while LIndex < Length(ABytes) do
    begin
      LFirst := ABytes[LIndex];
      LSecond := 0;
      LThird := 0;

      if LIndex + 1 < Length(ABytes) then
      begin
        LSecond := ABytes[LIndex + 1];
      end;

      if LIndex + 2 < Length(ABytes) then
      begin
        LThird := ABytes[LIndex + 2];
      end;
      LGroup := TNyxText(CAlphabet[(LFirst shr 2) + 1]) +
        CAlphabet[((LFirst and 3) shl 4) + (LSecond shr 4) + 1] + '==';

      if LIndex + 1 < Length(ABytes) then
      begin
        LGroup[3] := CAlphabet[((LSecond and 15) shl 2) + (LThird shr 6) + 1];
      end;

      if LIndex + 2 < Length(ABytes) then
      begin
        LGroup[4] := CAlphabet[(LThird and 63) + 1];
      end;
      LChunk := LChunk + LGroup;

      if Length(LChunk) >= 4096 then
      begin
        LParts.Add(LChunk);
        LChunk := '';
      end;
      Inc(LIndex, 3);
    end;

    if LChunk <> '' then
    begin
      LParts.Add(LChunk);
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;


function NyxUTF8ByteCount(const AText: TNyxText): Integer;
var
  LIndex: Integer;
  LScalar: Integer;
begin
  Result := 0;
  LIndex := 1;
  while LIndex <= Length(AText) do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxBytes.Create('Resource text contains malformed Unicode');
    end;

    if LScalar < $80 then
    begin
      Inc(Result);
    end
    else if LScalar < $800 then
    begin
      Inc(Result, 2);
    end
    else if LScalar < $10000 then
    begin
      Inc(Result, 3);
    end
    else
    begin
      Inc(Result, 4);
    end;
  end;
end;

function NyxEncodeUTF8(const AText: TNyxText): TNyxBytes;
var
  LBytes: TNyxBytes;
  LIndex: Integer;
  LOutput: Integer;
  LScalar: Integer;
begin
  SetLength(LBytes, NyxUTF8ByteCount(AText));
  LIndex := 1;
  LOutput := 0;
  while LIndex <= Length(AText) do
  begin
    NyxNextScalar(AText, LIndex, LScalar);

    if LScalar < $80 then
    begin
      LBytes[LOutput] := LScalar;
      Inc(LOutput);
    end
    else if LScalar < $800 then
    begin
      LBytes[LOutput] := $C0 or (LScalar shr 6);
      LBytes[LOutput + 1] := $80 or (LScalar and $3F);
      Inc(LOutput, 2);
    end
    else if LScalar < $10000 then
    begin
      LBytes[LOutput] := $E0 or (LScalar shr 12);
      LBytes[LOutput + 1] := $80 or ((LScalar shr 6) and $3F);
      LBytes[LOutput + 2] := $80 or (LScalar and $3F);
      Inc(LOutput, 3);
    end
    else
    begin
      LBytes[LOutput] := $F0 or (LScalar shr 18);
      LBytes[LOutput + 1] := $80 or ((LScalar shr 12) and $3F);
      LBytes[LOutput + 2] := $80 or ((LScalar shr 6) and $3F);
      LBytes[LOutput + 3] := $80 or (LScalar and $3F);
      Inc(LOutput, 4);
    end;
  end;
  Result := LBytes;
end;

function NyxDecodeUTF8(const ABytes: TNyxBytes): TNyxText;
var
  LIndex: Integer;
  LPart: Integer;
  LCount: Integer;
  LScalar: Integer;
  LMinimum: Integer;
  LLead: Byte;
  LText: TNyxText;
  LChunk: TNyxText;
  LParts: TNyxStrings;
begin
  LParts := TNyxStrings.Create;
  try
    LChunk := '';
    LIndex := 0;
    while LIndex < Length(ABytes) do
    begin
      LLead := ABytes[LIndex];
      LCount := 1;
      LMinimum := 0;
      LScalar := LLead;

      if LLead >= $F0 then
      begin
        LCount := 4;
        LMinimum := $10000;
        LScalar := LLead and 7;

        if LLead > $F4 then
        begin
          raise ENyxBytes.Create('Malformed resource UTF-8');
        end;
      end
      else if LLead >= $E0 then
      begin
        LCount := 3;
        LMinimum := $800;
        LScalar := LLead and 15;
      end
      else if LLead >= $C2 then
      begin
        LCount := 2;
        LMinimum := $80;
        LScalar := LLead and 31;
      end
      else if LLead >= $80 then
      begin
        raise ENyxBytes.Create('Malformed resource UTF-8');
      end;

      if LIndex + LCount > Length(ABytes) then
      begin
        raise ENyxBytes.Create('Truncated resource UTF-8');
      end;
      for LPart := 1 to LCount - 1 do
      begin

        if (ABytes[LIndex + LPart] and $C0) <> $80 then
        begin
          raise ENyxBytes.Create('Malformed resource UTF-8 continuation');
        end;
        LScalar := (LScalar shl 6) or (ABytes[LIndex + LPart] and $3F);
      end;

      if (LScalar < LMinimum) or (LScalar > $10FFFF) or
        ((LScalar >= $D800) and (LScalar <= $DFFF)) then
      begin
        raise ENyxBytes.Create('Resource UTF-8 requires Unicode scalars');
      end;
      LChunk := LChunk + NyxScalarText(LScalar);
      Inc(LIndex, LCount);

      if Length(LChunk) >= 4096 then
      begin
        LParts.Add(LChunk);
        LChunk := '';
      end;
    end;
    LParts.Add(LChunk);
    LText := LParts.Join;
  finally
    LParts.Free;
  end;
  Result := LText;
end;

end.
