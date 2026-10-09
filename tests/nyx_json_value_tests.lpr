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

program nyx_json_value_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils,
  nyx.text,
  {$ifdef PAS2JS}
  Web,
  {$endif}
  nyx.test.json,
  nyx.test.data;

var
  GPhase: Integer;
  GChecks: Integer;

{ Run the existing complete JSON and structured-value suites without substituting
  smaller budgets or samples. Browser startup yields before qualification, and
  acknowledged whole-suite checkpoints retain the exact last completed boundary.
  This focused gate does not replace the broader composition/designer suite. }
procedure Run;
{$ifdef PAS2JS}
const
  CCheckpoints: array[0..1] of String = ('strict-json', 'structured-values');
{$endif}
begin
  try
    {$ifdef PAS2JS}
    document.body.setAttribute('data-test-checks', IntToStr(GChecks));
    document.body.setAttribute('data-capture-checkpoint', CCheckpoints[GPhase]);

    if document.body.getAttribute('data-capture-observed') <> CCheckpoints[GPhase] then
    begin
      window.setTimeout(@Run, 20);
      Exit;
    end;
    {$endif}
    case GPhase of
      0:
        begin
          Inc(GChecks, RunNyxJSONTests);
        end;
      1:
        begin
          Inc(GChecks, RunNyxDataTests);
        end;
    end;
    Inc(GPhase);
    {$ifdef PAS2JS}

    if GPhase < 2 then
    begin
      window.setTimeout(@Run, 20);
      Exit;
    end;
    document.body.textContent := 'PASS / strict JSON and structured values / ' +
      IntToStr(GChecks) + ' checks';
    document.body.setAttribute('data-test-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
    {$endif}

    if GPhase = 2 then
    begin
      WriteLn('PASS / strict JSON and structured values / ', GChecks, ' checks');
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
      {$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  {$ifdef PAS2JS}
  window.setTimeout(@Run, 20);
  {$else}
  while (GPhase < 2) and (ExitCode = 0) do
  begin
    Run;
  end;
  {$endif}
end.
