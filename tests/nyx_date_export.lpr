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

program nyx_date_export;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.model, nyx.codec, nyx.codegen,
  nyx.test.dates, nyx.generated.view;

{ Write exact UTF-8 bytes. The owned output directory is supplied by build
  orchestration; this tool never connects to or changes an editor project. }
procedure Save(const APath: String; const AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

var
  LDocument: TNyxDocument;
  LDirectory: String;

begin
  LDocument := nil;
  try

    if ParamCount <> 1 then
    begin
      raise EArgumentException.Create('Supply an existing owned date-generation directory');
    end;
    LDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
    LDocument := BuildNyxDocument;
    ConfigureNyxDateReview(LDocument);
    Save(LDirectory + 'nyx.generated.date.pas',
      TNyxCodegen.Generate(LDocument, 'nyx.generated.date'));
    Save(LDirectory + 'dates.nyx', TNyxCodec.Encode(LDocument));
    WriteLn('PASS current typed date companion source exported');
  finally
    LDocument.Free;
  end;
end.
