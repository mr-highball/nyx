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

program nyx_image_fixtures;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, FPImage, FPWritePNG, FPWriteJPEG, nyx.text, nyx.images;

procedure WriteText(const APath: String; const AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmCreate);
  try

    if AText <> '' then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

function Raster(AFormat: TNyxImageFormat): TNyxImageSource;
var
  LImage: TFPMemoryImage;
  LWriter: TFPCustomImageWriter;
  LStream: TMemoryStream;
  LBytes: TNyxImageBytes;
  LColor: TFPColor;
  LX: Integer;
  LY: Integer;
begin
  LImage := TFPMemoryImage.Create(100, 50);
  LWriter := nil;
  LStream := TMemoryStream.Create;
  try
    for LY := 0 to LImage.Height - 1 do
    begin
      for LX := 0 to LImage.Width - 1 do
      begin
        LColor := colRed;

        if LX >= LImage.Width div 2 then
        begin
          LColor := colBlue;
        end;
        LImage.Colors[LX, LY] := LColor;
      end;
    end;

    if AFormat = nimPNG then
    begin
      LWriter := TFPWriterPNG.Create;
    end
    else
    begin
      LWriter := TFPWriterJPEG.Create;
    end;
    LImage.SaveToStream(LStream, LWriter);
    SetLength(LBytes, LStream.Size);
    LStream.Position := 0;
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxEmbeddedImageBytes(AFormat, LBytes);
  finally
    LStream.Free;
    LWriter.Free;
    LImage.Free;
  end;
end;

var
  LDirectory: String;
  LPNG: TNyxImageSource;
  LJPEG: TNyxImageSource;
  LSource: TNyxText;

begin
  try

    if ParamCount <> 1 then
    begin
      raise Exception.Create('Supply the owned fixture output directory');
    end;
    LDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
    ForceDirectories(LDirectory);
    LPNG := Raster(nimPNG);
    LJPEG := Raster(nimJPEG);
    WriteText(LDirectory + 'png-source.txt', LPNG.ToWire);
    LSource := '{ Pascal-created image resources for both ordinary target consumers. }' + #10 +
      'unit nyx.image.fixtures;' + #10 + '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
      'interface' + #10 + 'uses nyx.text;' + #10 + 'const' + #10 +
      '  ImagePNG: TNyxText = ''' + LPNG.Encoded + ''';' + #10 +
      '  ImageJPEG: TNyxText = ''' + LJPEG.Encoded + ''';' + #10 +
      'implementation' + #10 + 'end.' + #10;
    WriteText(LDirectory + 'nyx.image.fixtures.pas', LSource);
    WriteLn('PASS / fixture raster / PNG ', Length(LPNG.Bytes), ' / JPEG ', Length(LJPEG.Bytes));
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
