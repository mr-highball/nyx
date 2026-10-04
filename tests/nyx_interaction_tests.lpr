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
program nyx_interaction_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.codegen, nyx.test.interactions
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  LCount: Integer;
  {$ifndef PAS2JS}
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LFile: TFileStream;
  {$endif}
begin
  try
    LCount := RunNyxInteractionTests;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(LCount) + ' interaction contracts';
    document.body.setAttribute('data-interaction-tests', 'passed');
    {$else}

    if ParamCount > 0 then
    begin
      LDocument := CreateNyxInteractionFixture;
      try
        LSource := TNyxCodegen.Generate(LDocument, 'nyx.interaction.generated');
        LFile := TFileStream.Create(ParamStr(1), fmCreate);
        try
          LFile.WriteBuffer(LSource[1], Length(LSource));
        finally
          LFile.Free;
        end;
      finally
        LDocument.Free;
      end;
    end;
    WriteLn('PASS ', LCount, ' interaction contracts');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-interaction-tests', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
