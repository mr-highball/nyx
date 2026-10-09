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

program nyx_resource_policy_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.resource.sources, nyx.resources.loader,
  nyx.test.browser.pipe, nyx.test.resource.stream, nyx.test.resource.policy;

var
  LServer: TNyxResourceStreamFixture;
  LHost: TNyxBrowserPipe;
  LURL: TNyxText;
  LOrigin: TNyxText;
  LMarker: TNyxText;
  LCheckpoint: TNyxText;
  LObserved: TNyxText;
  LMoment: TNyxText;
  LStarted: QWord;
  LPathAt: Integer;
  LSchemeAt: Integer;
  LCase: Integer;
  LBaseRequests: Integer;
  LExpectedRequests: Integer;
  LCompleted: Integer;
  LPlan: TNyxTestPolicyPlan;

begin
  LServer := nil;
  LHost := nil;
  LCompleted := 0;
  LBaseRequests := 0;
  try
    try

      if ParamCount <> 2 then
      begin
        raise Exception.Create('Supply admitted static page URL and fresh owned evidence directory');
      end;
      LURL := NyxResourceURL(TNyxText(ParamStr(1))).Address;
      LSchemeAt := Pos('://', LURL);
      LPathAt := Pos('/', Copy(LURL, LSchemeAt + 3, MaxInt));

      if LPathAt = 0 then
      begin
        raise Exception.Create('Policy page requires its admitted fixture path');
      end;
      LOrigin := Copy(LURL, 1, LPathAt + LSchemeAt + 1);
      LServer := TNyxResourceStreamFixture.Create(LOrigin);
      LHost := TNyxBrowserPipe.Create(String(LURL + '?resource=' + LServer.URL),
        ParamStr(2), 1100, 800);
      LStarted := GetTickCount64;
      LObserved := '';
      repeat
        LMarker := LHost.Attribute('data-test-result');

        if (LMarker = 'failed') or (LHost.RuntimeError <> '') then
        begin
          LHost.Capture('failure');
          raise Exception.Create('Actual policy application refused: ' +
            LHost.Attribute('data-event-error') + ' / origin ' +
            LHost.Attribute('data-policy-origin') + ' / expected ' +
            LHost.Attribute('data-policy-expected'));
        end;

        if LServer.Error <> '' then
        begin
          raise Exception.Create(String(LServer.Error));
        end;
        LCheckpoint := LHost.Attribute('data-capture-checkpoint');

        if (LCheckpoint <> '') and (LCheckpoint <> LObserved) then
        begin

          if not TryStrToInt(String(LHost.Attribute('data-policy-case')), LCase) or
            (LCase < Ord(Low(TNyxTestPolicyCase))) or (LCase > Ord(High(TNyxTestPolicyCase))) then
          begin
            raise Exception.Create('Policy checkpoint requires an exact known case');
          end;
          LPlan := NyxTestPolicyPlan(TNyxTestPolicyCase(LCase));
          LMoment := LHost.Attribute('data-policy-moment');

          if LCheckpoint <> TNyxText(IntToStr(LCase)) + '-' + LMoment then
          begin
            raise Exception.Create('Policy checkpoint identity differs from its case/moment');
          end;
          LExpectedRequests := 0;

          if LMoment = 'start' then
          begin

            if LCase <> LCompleted then
            begin
              raise Exception.Create('Policy case is repeated or skips an unfinished consumer');
            end;
            LBaseRequests := LServer.Requests;
            LServer.ArmPolicy(LPlan.Reply);
          end
          else if LMoment = 'warm' then
          begin
            LExpectedRequests := 1;
            LServer.ArmPolicy(ntrUnavailable);
          end
          else if LMoment = 'decision' then
          begin
            LExpectedRequests := 2;

            if LPlan.Expected = rloFreshCache then
            begin
              LExpectedRequests := 1;
              Inc(LCompleted);
            end
            else
            begin
              LServer.ArmPolicy(LPlan.Reply, True);
            end;
          end
          else if LMoment = 'recovered' then
          begin
            LExpectedRequests := 3;
            Inc(LCompleted);
          end
          else
          begin
            raise Exception.Create('Unknown policy checkpoint moment');
          end;

          if LServer.Requests - LBaseRequests <> LExpectedRequests then
          begin
            raise Exception.Create(String(LPlan.Title) + ': actual producer request count differs from policy');
          end;

          if (TNyxTestPolicyCase(LCase) in [npcStale, npcExpired, npcOverrideNoStore]) and
            ((LMoment = 'decision') or (LMoment = 'recovered')) then
          begin
            LHost.Capture(String(LCheckpoint));
          end;
          { Acknowledge only fixture coordination, after independent network
            receipts qualify the decision. No document or editor operation is
            driven through DOM; controls receive actual public application data. }
          LHost.SetAttribute('data-capture-observed', LCheckpoint);
          LObserved := LCheckpoint;
          WriteLn('Observed policy / ', LPlan.Title, ' / ', LMoment,
            ' / actual requests ', LServer.Requests - LBaseRequests);
        end;

        if GetTickCount64 - LStarted > 30000 then
        begin
          LHost.Capture('timeout');
          raise Exception.Create('Actual policy application did not finish within thirty seconds');
        end;
        Sleep(20);
      until LMarker = 'passed';

      if (LCompleted <> Ord(High(TNyxTestPolicyCase)) + 1) or
        LHost.Exists('[data-node="caption"]') or (LServer.Requests <> 27) then
      begin
        raise Exception.Create('Complete policy consumers, actual network receipts or retirement are missing');
      end;
      WriteLn('PASS / actual browser policy application / ',
        LHost.Attribute('data-policy-checks'), ' checks / requests ', LServer.Requests);
    finally
      { Retire the exact browser/debugger before joining this owned read-only
        producer. Existing browsers, Studio services and user pairs stay owned. }
      LHost.Free;
      LServer.Free;
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
