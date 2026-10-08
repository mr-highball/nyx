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
program nyx_color_values_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.codec, nyx.codegen, nyx.test.colors
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

{$ifndef PAS2JS}
procedure ExportFixture(const ADirectory: TNyxText);
var
  LDoc: TNyxDocument;

  procedure Save(const AName, AText: TNyxText);
  var
    LStream: TFileStream;
  begin
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ADirectory) + AName, fmCreate);
    try

      if AText <> '' then
      begin
        LStream.WriteBuffer(AText[1], Length(AText));
      end;
    finally
      LStream.Free;
    end;
  end;

begin
  LDoc := CreateNyxColorFixture;
  try
    Save('nyx.generated.colors.pas', TNyxCodegen.Generate(LDoc, 'nyx.generated.colors'));
    Save('color-fixture.nyx', TNyxCodec.Encode(LDoc));
  finally
    LDoc.Free;
  end;
end;
{$endif}

var
  LChecks: Integer;
begin
  try
    LChecks := RunNyxColorChecks;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-color-values', 'passed');
    document.body.setAttribute('data-color-checks', IntToStr(LChecks));
    {$else}

    if ParamCount > 0 then
    begin
      ExportFixture(ParamStr(1));
    end;
    WriteLn('PASS ', LChecks, ' portable RGB value/source/history checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-color-values', 'failed');
      document.body.setAttribute('data-color-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
