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

program nyx_event_queue_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.studio.projects, nyx.test.event.queue
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  LPair: TNyxProjectPair;
  LChecks: Integer;
  {$ifndef PAS2JS}LStream: TFileStream;{$endif}

begin
  try
    LChecks := RunNyxEventQueueJourney(LPair);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-event-queue', 'passed');
    document.body.setAttribute('data-event-checks', IntToStr(LChecks));
    {$else}

    if ParamStr(1) <> '' then
    begin
      ForceDirectories(ParamStr(1));
      LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
        'nyx.generated.view.pas', fmCreate);
      try
        LStream.WriteBuffer(PAnsiChar(LPair.Source)^, Length(LPair.Source));
      finally
        LStream.Free;
      end;
    end;
    WriteLn('PASS ', LChecks, ' typed callback queue/admission checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-queue', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
