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
program nyx_source_diagnostic_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  {$ifdef PAS2JS}
  Web,
  {$endif}
  nyx.test.source.diagnostics,
  nyx.test.source.indexed,
  nyx.test.source.context,
  nyx.test.source.history,
  nyx.test.compiler,
  nyx.test.schema.admission;

var
  LCount: Integer;
begin
  try
    LCount := RunNyxSourceDiagnosticTests + RunNyxIndexedSourceTests +
      RunNyxSourceContextTests + RunNyxSourceHistoryTests +
      RunNyxSourceAdmissionTests + RunNyxCompilerDiagnosticTests +
      RunNyxSchemaAdmissionTests;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(LCount) + ' source diagnostic checks';
    document.body.setAttribute('data-source-diagnostics', 'passed');
    {$else}
    WriteLn('PASS ', LCount, ' source diagnostic checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-source-diagnostics', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      Halt(1);
      {$endif}
    end;
  end;
end.
