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
program nyx_glyph_pack;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, base64;

var
  LInput: TFileStream;
  LBytes: RawByteString;
  LEncoded: String;
  LOutput: TStringList;
  LOffset: Integer;
  LLine: String;
begin
  { This asset build tool encodes original bytes without resampling or changing
    alpha. Source/destination/constant identity are explicit orchestration input. }

  if ParamCount <> 3 then
  begin
    raise Exception.Create('Usage: nyx_glyph_pack source.png destination.inc PascalConstantName');
  end;

  if FileExists(ParamStr(2)) then
  begin
    raise Exception.Create('Refusing to overwrite an existing generated include');
  end;
  LInput := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
  try

    if (LInput.Size < 24) or (LInput.Size > 8 * 1024 * 1024) then
    begin
      raise Exception.Create('Glyph PNG must contain 24 bytes..8 MiB');
    end;
    SetLength(LBytes, LInput.Size);
    LInput.ReadBuffer(LBytes[1], Length(LBytes));
  finally
    LInput.Free;
  end;

  if (Ord(LBytes[1]) <> 137) or (Copy(LBytes, 2, 7) <> 'PNG'#13#10#26#10) then
  begin
    raise Exception.Create('Glyph source must be a PNG');
  end;
  LEncoded := EncodeStringBase64(LBytes);
  LOutput := TStringList.Create;
  try
    LOutput.LineBreak := #10;
    LOutput.Add('{ Original PNG bytes; layout belongs to the typed glyph selector.');
    LOutput.Add('  Provenance, dimensions and SHA256: README.md beside this include. }');
    LOutput.Add('const');
    LOutput.Add('  ' + ParamStr(3) + ' =');
    LOffset := 1;
    while LOffset <= Length(LEncoded) do
    begin
      LLine := '    ''' + Copy(LEncoded, LOffset, 100) + '''';
      Inc(LOffset, 100);

      if LOffset <= Length(LEncoded) then
      begin
        LLine := LLine + ' +';
      end
      else
      begin
        LLine := LLine + ';';
      end;
      LOutput.Add(LLine);
    end;
    LOutput.SaveToFile(ParamStr(2));
  finally
    LOutput.Free;
  end;
end.
