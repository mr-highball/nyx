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
program nyx_resource_source_evidence;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes;

{ This is an artifact consumer, not an HTML/editor automation driver. The
  maintained browser journey publishes its accepted source with encodeURIComponent.
  Read only that bounded ASCII attribute, decode exact UTF-8 bytes, then compare
  against the native controller's file. No newline/codepage normalization or
  source regeneration is allowed to hide different emitted Pascal. }
function ReadText(const APath: String): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if LFile.Size > 16777216 then
    begin
      raise Exception.Create('Source evidence exceeds its 16 MiB artifact bound');
    end;
    SetLength(LBytes, Integer(LFile.Size));

    if Length(LBytes) > 0 then
    begin
      LFile.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;

function HexDigit(AValue: Char): Byte;
begin
  case AValue of
    '0'..'9':
      begin
        Result := Ord(AValue) - Ord('0');
      end;
    'a'..'f':
      begin
        Result := Ord(AValue) - Ord('a') + 10;
      end;
    'A'..'F':
      begin
        Result := Ord(AValue) - Ord('A') + 10;
      end;
    else
    begin
      raise Exception.Create('Source evidence contains malformed percent encoding');
    end;
  end;
end;

function BrowserSource(const APath: String): TNyxText;
const
  CMarker: TNyxText = 'data-workbench-source="';
var
  LHTML: TNyxText;
  LEncoded: TNyxText;
  LBytes: TNyxBytes;
  LStart: Integer;
  LEnd: Integer;
  LIndex: Integer;
  LCount: Integer;
begin
  LHTML := ReadText(APath);
  LStart := Pos(CMarker, LHTML);

  if LStart = 0 then
  begin
    raise Exception.Create('Browser did not publish accepted source evidence');
  end;
  LHTML := Copy(LHTML, LStart + Length(CMarker), Length(LHTML));
  LEnd := Pos('"', LHTML);

  if (LEnd = 0) or (Pos(CMarker, LHTML) > 0) then
  begin
    raise Exception.Create('Browser source evidence is incomplete or ambiguous');
  end;
  LEncoded := Copy(LHTML, 1, LEnd - 1);
  SetLength(LBytes, Length(LEncoded));
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(LEncoded) do
  begin

    if LEncoded[LIndex] = '%' then
    begin

      if LIndex + 2 > Length(LEncoded) then
      begin
        raise Exception.Create('Browser source percent encoding is truncated');
      end;
      LBytes[LCount] := HexDigit(LEncoded[LIndex + 1]) * 16 + HexDigit(LEncoded[LIndex + 2]);
      Inc(LIndex, 3);
    end
    else
    begin

      if Ord(LEncoded[LIndex]) > 127 then
      begin
        raise Exception.Create('Browser source attribute must contain ASCII encoding');
      end;
      LBytes[LCount] := Ord(LEncoded[LIndex]);
      Inc(LIndex);
    end;
    Inc(LCount);
  end;
  SetLength(LBytes, LCount);
  Result := NyxDecodeUTF8(LBytes);
end;

var
  LSource: TNyxText;
  LIndex: Integer;
begin
  try

    if (ParamCount < 2) or (ParamCount > 3) then
    begin
      raise Exception.Create('Supply native Pascal and one or two browser DOM artifacts');
    end;
    LSource := ReadText(ParamStr(1));
    for LIndex := 2 to ParamCount do
    begin

      if BrowserSource(ParamStr(LIndex)) <> LSource then
      begin
        raise Exception.Create('Native and browser consumers emitted different Pascal');
      end;
    end;
    WriteLn('PASS / identical native and supplied HTTP Pascal artifacts');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
