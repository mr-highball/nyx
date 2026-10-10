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

uses SysUtils, FPHTTPClient, nyx.text, nyx.resource.sources, nyx.test.browser.pipe,
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
  LDelivery: TNyxTestDeliveryCase;
  LDeliveryFound: Boolean;
  LSecureURL: TNyxText;
  LCaseURL: TNyxText;
  LNetwork: TNyxBrowserNetworkResult;
  LProducerBefore: Integer;

begin
  LServer := nil;
  LHost := nil;
  try
    try

      if not (ParamCount in [2, 3]) then
      begin
        raise Exception.Create('Supply admitted static page URL, fresh owned evidence directory and optional immutable HTTPS JSON URL');
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
      LSecureURL := TNyxText(ParamStr(3));

      if LSecureURL <> '' then
      begin
        LURL := LURL + '?resource=' + LServer.URL + '&secure=' +
          TNyxText(EncodeURLElement(AnsiString(LSecureURL)));
      end
      else
      begin
        LURL := LURL + '?resource=' + LServer.URL;
      end;
      LHost := TNyxBrowserPipe.Create(String(LURL),
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
          LDeliveryFound := False;
          for LDelivery := Low(TNyxTestDeliveryCase) to High(TNyxTestDeliveryCase) do
          begin

            if LCheckpoint = 'transport-' + NyxTestDeliveryName(LDelivery) then
            begin
              LDeliveryFound := True;
              LCaseURL := LServer.URL;

              if LDelivery = ndcHTTPS then
              begin
                LCaseURL := LSecureURL;
              end
              else if LDelivery = ndcInvalidTLS then
              begin
                LCaseURL := 'https://expired.badssl.com/';
              end
              else
              begin
                LServer.ArmPolicy(NyxTestDeliveryReply(LDelivery));
              end;
              LProducerBefore := LServer.Requests;
              LHost.ObserveResource(LCaseURL);
              Break;
            end
            else if LCheckpoint = 'observed-' + NyxTestDeliveryName(LDelivery) then
            begin
              LDeliveryFound := True;
              LNetwork := LHost.NetworkResult;

              if LNetwork.Requests <> 1 then
              begin
                raise Exception.Create('Actual transport case requires exactly one observed request start');
              end;

              if LDelivery = ndcHTTPS then
              begin

                if (LNetwork.Status <> 200) or not LNetwork.Secure or
                  (LNetwork.Failure <> nnfNone) then
                begin
                  raise Exception.Create('Positive HTTPS requires actual secure response evidence');
                end;
              end
              else if LDelivery = ndcInvalidTLS then
              begin

                if (LNetwork.Failure <> nnfCertificate) or
                  (LNetwork.Error <> 'net::ERR_CERT_DATE_INVALID') then
                begin
                  raise Exception.Create('Expired TLS requires the actual certificate-date refusal');
                end;
              end
              else
              begin

                if LServer.Requests <> LProducerBefore + 1 then
                begin
                  raise Exception.Create('Local refusal must not retry or follow its owned redirect');
                end;

                if (LDelivery = ndcCorsDenied) and (LNetwork.Failure <> nnfCORS) then
                begin
                  raise Exception.Create('Missing CORS permission requires the actual CORS refusal');
                end;
              end;
              WriteLn('Observed wire / ', NyxTestDeliveryName(LDelivery), ' / status ',
                LNetwork.Status, ' / secure ', LNetwork.Secure, ' / ', LNetwork.Error);
              Break;
            end;
          end;

          if LDeliveryFound then
          begin
            { The actual application and bounded network observation own this
              case. Only its fixture checkpoint is acknowledged below. }
          end
          else if LCheckpoint = 'initial' then
          begin
            { Additional transport cases precede the original held-body path;
              the driver arms its first held body at the last observed case. }
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

          if (LCheckpoint = 'observed-invalid-tls') or
            ((LSecureURL = '') and (LCheckpoint = 'observed-http-recovered')) then
          begin
            LServer.ArmNext(True, False);
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
