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
program nyx_split_generated_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils, nyx.model, nyx.codec, nyx.schema, nyx.test.split,
  nyx.split.generated
  {$ifdef PAS2JS}, Web{$endif};

var
  LExpected: TNyxDocument;
  LActual: TNyxDocument;
begin
  try
    LExpected := CreateNyxSplitFixture;
    LActual := BuildNyxDocument;
    try
      ValidateNyxDocumentProperties(LActual);

      if TNyxCodec.Encode(LExpected) <> TNyxCodec.Encode(LActual) then
      begin
        raise ENyxModel.Create('Compiled split source changed the design or platform rules');
      end;
    finally
      LExpected.Free;
      LActual.Free;
    end;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS compiled specialized split and platform reconstruction';
    document.body.setAttribute('data-split-generated', 'passed');
    {$else}
    WriteLn('PASS compiled specialized split and platform reconstruction');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-split-generated', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.

