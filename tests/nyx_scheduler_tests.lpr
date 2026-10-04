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
program nyx_scheduler_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  nyx.text,
  nyx.test.scheduler
  {$IFDEF PAS2JS}
  , Web
  {$ENDIF}
  ;

type
  TTestHost = class
  public
    Count: Integer;
    Failure: TNyxText;
    procedure Done(ACount: Integer; const AFailure: TNyxText);
  end;

procedure TTestHost.Done(ACount: Integer; const AFailure: TNyxText);
begin
  Count := ACount + Count;
  Failure := AFailure;
  {$IFDEF PAS2JS}

  if Failure = '' then
  begin
    document.body.setAttribute('data-scheduler-tests', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(Count) + ' scheduler/registration checks';
  end
  else
  begin
    document.body.setAttribute('data-scheduler-tests', 'failed');
    document.body.textContent := Failure;
  end;
  {$ELSE}

  if Failure = '' then
  begin
    WriteLn('PASS ', Count, ' scheduler/registration checks');
  end
  else
  begin
    WriteLn(Failure);
    ExitCode := 1;
  end;
  {$ENDIF}
end;

var
  GHost: TTestHost;
  GJourney: TNyxScheduleJourney;
begin
  GHost := TTestHost.Create;
  try
    GHost.Count := RunNyxSchedulerTests;
    GJourney := TNyxScheduleJourney.Create(GHost.Done);
    GJourney.Start;
  except
    on LException: Exception do
    begin
      GHost.Done(0, LException.Message);
      {$IFNDEF PAS2JS}
      ExitCode := 1;
      {$ENDIF}
    end;
  end;
  {$IFNDEF PAS2JS}
  GJourney.Free;
  GHost.Free;
  {$ENDIF}
end.
