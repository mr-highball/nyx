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

program nyx_controls_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  {$IFDEF PAS2JS}
  Web,
  {$ENDIF}
  nyx.test.controls;

var
  LChecks: Integer;
begin
  try
    LChecks := RunNyxControlTests;
    {$IFDEF PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' managed component checks';
    document.body.setAttribute('data-tests', 'passed');
    {$ELSE}
    WriteLn('PASS ', LChecks, ' managed component checks');
    {$ENDIF}
  except
    on LException: Exception do
    begin
      {$IFDEF PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-tests', 'failed');
      {$ELSE}
      WriteLn(LException.ClassName, ': ', LException.Message);
      Halt(1);
      {$ENDIF}
    end;
  end;
end.

