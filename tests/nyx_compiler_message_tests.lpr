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

program nyx_compiler_message_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.test.compiler.stream
  {$ifndef PAS2JS}, nyx.test.compiler.messages.native{$endif};

var
  LChecks: Integer;

begin
  try
    LChecks := RunNyxCompilerStreamTests;
    WriteLn('PASS incremental compiler messages ', LChecks);
    {$ifndef PAS2JS}

    if ParamCount = 3 then
    begin
      LChecks := RunNyxLiveCompilerMessages(ParamStr(1), ParamStr(2), ParamStr(3));
      WriteLn('PASS native live semantic messages ', LChecks);
    end
    else if ParamCount <> 0 then
    begin
      raise Exception.Create('Supply a fresh owned runtime, fixture compiler and matching RTL');
    end;
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
