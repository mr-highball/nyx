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

program nyx_resource_persistence_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.test.browser.pipe, nyx.test.resource.failures;

const
  CPhases: array[0..6] of String =
    ('store', 'restore', 'quota', 'corrupt', 'respect', 'deadline', 'failures');

var
  LProfile: TNyxBrowserProfile;
  LHost: TNyxBrowserPipe;
  LDirectory: String;
  LMarker: TNyxText;
  LCheckpoint: TNyxText;
  LObserved: TNyxText;
  LStarted: QWord;
  LPhase: Integer;
  LWidth: Integer;
  LFailureFile: TNyxResourceFailureFile;
  LFailure: TNyxResourceFailure;
  LFailureName: TNyxText;
  LFound: Boolean;
begin
  LProfile := nil;
  LHost := nil;
  LFailureFile := nil;
  try

    if (ParamCount < 2) or (ParamCount > 3) then
    begin
      raise Exception.Create('Supply owned loopback fixture URL, fresh evidence directory and optional marked failure directory');
    end;
    LDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));

    if DirectoryExists(LDirectory) then
    begin
      raise Exception.Create('Persistence qualification requires a fresh evidence directory');
    end;
    LProfile := TNyxBrowserProfile.Create(LDirectory);
    try

      if ParamCount = 3 then
      begin
        LFailureFile := TNyxResourceFailureFile.Create(ParamStr(3),
          TNyxText(Copy(ParamStr(1), 1, LastDelimiter('/', ParamStr(1)))) + 'copy.json');
      end;
      for LPhase := Low(CPhases) to High(CPhases) do
      begin

        if (CPhases[LPhase] = 'failures') and (LFailureFile = nil) then
        begin
          Continue;
        end;
        LWidth := 1280;

        if LPhase = 1 then
        begin
          LWidth := 390;
        end;
        LHost := TNyxBrowserPipe.Create(ParamStr(1) + '?phase=' + CPhases[LPhase],
          LDirectory + CPhases[LPhase], LWidth, 844, LProfile);
        try
          LStarted := GetTickCount64;
          LObserved := '';
          repeat
            LMarker := LHost.Attribute('data-test-result');

            if LHost.RuntimeError <> '' then
            begin
              raise Exception.Create('Actual browser runtime failure; see private debugger receipt');
            end;

            if LMarker = 'failed' then
            begin
              LHost.Capture('failure');
              raise Exception.Create(LHost.Attribute('data-event-error'));
            end;
            LCheckpoint := LHost.Attribute('data-capture-checkpoint');

            if (LCheckpoint <> '') and (LCheckpoint <> LObserved) then
            begin

              if CPhases[LPhase] = 'failures' then
              begin
                { Change one compared, origin-marked fixture file. Requests use
                  the real server/fetch path; no transport reply is fabricated. }
                LFound := False;
                for LFailure := Low(TNyxResourceFailure) to High(TNyxResourceFailure) do
                begin
                  LFailureName := 'failure-' + NyxResourceFailureName(LFailure);

                  if LCheckpoint = LFailureName then
                  begin
                    LFound := True;
                    LHost.Capture(String(LFailureName));

                    if LFailure = nrfCorrected then
                    begin
                      LFailureFile.Apply(nrfHealthy);
                    end
                    else
                    begin
                      LFailureFile.Apply(Succ(LFailure));
                    end;
                    Break;
                  end;
                end;

                if not LFound then
                begin
                  raise Exception.Create('Unknown hosted failure checkpoint');
                end;
              end
              else if CPhases[LPhase] = 'deadline' then
              begin
                { Delay real fetch only after the application mounts. Timeout
                  and cancellation use the actual AbortController adapter;
                  healthy recovery restores ordinary networking first. }
                if (LCheckpoint = 'deadline-ready') or
                  (LCheckpoint = 'deadline-recovered') then
                begin
                  LHost.NetworkLatency(2000);
                end
                else if LCheckpoint = 'deadline-expired' then
                begin
                  LHost.NetworkLatency(0);
                end
                else
                begin
                  raise Exception.Create('Unknown deadline qualification checkpoint');
                end;
                LHost.Capture(String(LCheckpoint));
              end
              else
              begin
                LHost.Capture('loaded');
              end;
              LHost.SetAttribute('data-capture-observed', LCheckpoint);
              LObserved := LCheckpoint;
            end;

            if GetTickCount64 - LStarted > 30000 then
            begin
              LHost.Capture('timeout');
              raise Exception.Create('Application persistence did not finish within 30 seconds');
            end;
            Sleep(20);
          until LMarker = 'passed';

          if (LObserved = '') or LHost.Exists('[data-node="caption"]') then
          begin
            raise Exception.Create('Actual mounted control capture or application retirement is missing');
          end;
          LHost.Capture('retired');
          WriteLn('PASS / fresh browser process / ', CPhases[LPhase], ' / ',
            LHost.Attribute('data-persistence-checks'), ' checks');
        finally
          { Retire this exact process and its debugger handles before the next
            pipe may lease the same fresh private profile. No existing browser,
            user project, LAN listener or enrollment is involved. }
          FreeAndNil(LHost);
        end;
      end;
    finally
      FreeAndNil(LFailureFile);
      FreeAndNil(LProfile);
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
