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

program nyx_callback_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  {$IFDEF PAS2JS}
  Web,
  {$ELSE}
  Classes,
  {$ENDIF}
  nyx.text,
  nyx.model,
  nyx.test.callbacks;

var
  LChecks: Integer;
  {$IFNDEF PAS2JS}
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LFile: TFileStream;
  {$ENDIF}
begin
  try
    LChecks := RunNyxCallbackAuthoringTests;
    {$IFDEF PAS2JS}
    document.body.setAttribute('data-nyx-callbacks', 'passed');
    document.body.setAttribute('data-nyx-callback-checks', IntToStr(LChecks));
    {$ELSE}
    WriteLn('PASS ', LChecks, ' callback descriptor/inspector/source checks');

    if ParamCount > 0 then
    begin
      LDocument := CreateNyxCallbackFixture(LSource);
      try
        LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
          'nyx.callback.fixture.pas', fmCreate);
        try
          LFile.WriteBuffer(LSource[1], Length(LSource));
        finally
          LFile.Free;
        end;
      finally
        LDocument.Free;
      end;
    end;
    {$ENDIF}
  except
    on LException: Exception do
    begin
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-nyx-callbacks', 'failed');
      document.body.setAttribute('data-nyx-callback-error', LException.Message);
      {$ELSE}
      WriteLn(LException.Message);
      Halt(1);
      {$ENDIF}
    end;
  end;
end.

