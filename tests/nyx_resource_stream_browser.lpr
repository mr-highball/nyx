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

program nyx_resource_stream_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.resource.sources, nyx.test.browser.pipe,
  nyx.test.resource.stream;

var
  LServer: TNyxResourceStreamFixture;
  LHost: TNyxBrowserPipe;
  LURL: TNyxText;
  LOrigin: TNyxText;
  LMarker: TNyxText;
  LCheckpoint: TNyxText;
  LObserved: TNyxText;
  LStarted: QWord;
  LClosing: QWord;
  LPathAt: Integer;
  LSchemeAt: Integer;
  LClosed: Integer;

begin
  LServer := nil;
  LHost := nil;
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
        raise Exception.Create('Stream page URL requires its admitted fixture path');
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
          raise Exception.Create('Actual stream application refused: ' +
            LHost.Attribute('data-event-error'));
        end;

        if LServer.Error <> '' then
        begin
          raise Exception.Create(String(LServer.Error));
        end;
        LCheckpoint := LHost.Attribute('data-capture-checkpoint');

        if (LCheckpoint <> '') and (LCheckpoint <> LObserved) then
        begin
          LHost.Capture(String(LCheckpoint));
          LClosed := 0;

          if LCheckpoint = 'initial' then
          begin
            LServer.ArmNext(True, False);
          end
          else if LCheckpoint = 'cancelled' then
          begin
            LClosed := 1;
            LServer.ArmNext(False, True);
          end
          else if LCheckpoint = 'recovered' then
          begin
            LServer.ArmNext(True, False);
          end
          else if LCheckpoint = 'retired' then
          begin
            LClosed := 2;
          end
          else
          begin
            raise Exception.Create('Unknown stream checkpoint');
          end;
          LClosing := GetTickCount64;
          while LServer.ClosedBodies < LClosed do
          begin

            if (LServer.Error <> '') or (GetTickCount64 - LClosing > 3000) then
            begin
              raise Exception.Create('Cancelled actual body did not close its peer within three seconds');
            end;
            Sleep(10);
          end;
          { The producer observes real socket closure; the application supplies
            the actual positive received prefix and typed terminal snapshot.
            Only test coordination is acknowledged, never an editor command. }
          LHost.SetAttribute('data-capture-observed', LCheckpoint);
          LObserved := LCheckpoint;
          WriteLn('Observed actual stream / ', LCheckpoint, ' / closed bodies ', LServer.ClosedBodies);
        end;

        if GetTickCount64 - LStarted > 30000 then
        begin
          LHost.Capture('timeout');
          raise Exception.Create('Actual stream application did not finish within thirty seconds');
        end;
        Sleep(20);
      until LMarker = 'passed';

      if (LObserved <> 'retired') or LHost.Exists('[data-node="caption"]') or
        (LServer.ClosedBodies <> 2) then
      begin
        raise Exception.Create('Actual stream retirement or peer-close evidence is missing');
      end;
      WriteLn('PASS / actual browser stream application / ',
        LHost.Attribute('data-stream-checks'), ' checks / closed bodies ', LServer.ClosedBodies);
    finally
      { Exact process/debugger retirement precedes disposal of its owned producer.
        Neither a pre-existing browser nor a Studio backend is involved. }
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
