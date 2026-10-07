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
program nyx_typeahead_policy_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.model, nyx.codec, nyx.test.typeahead.policy,
  nyx.generated.typeahead
  {$ifdef PAS2JS}, JS, Web{$endif};

var
  LDocument: TNyxDocument;
  LExpected: TNyxDocument;
  LChecks: Integer;
begin
  LDocument := nil;
  LExpected := nil;
  try
    try
      { This call executes the exact emitted unit, independently of Studio's
        bounded reader. A successful source admission is not compiler proof. }
      LDocument := nyx.generated.typeahead.BuildNyxDocument;
      LChecks := VerifyNyxSavedTypeAheadFixture(LDocument);
      LExpected := CreateNyxSavedTypeAheadFixture;

      if TNyxCodec.Encode(LDocument) <> TNyxCodec.Encode(LExpected) then
      begin
        raise Exception.Create('Compiled builder differs from the complete accepted design');
      end;
      Inc(LChecks);
      {$ifdef PAS2JS}
      document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' compiled saved-policy checks';
      document.body.setAttribute('data-typeahead-generated-tests', 'passed');
      {$else}
      WriteLn('PASS ', LChecks, ' compiled saved-policy checks');
      {$endif}
    finally
      LExpected.Free;
      LDocument.Free;
    end;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-typeahead-generated-tests', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
