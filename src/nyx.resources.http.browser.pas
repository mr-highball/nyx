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


unit nyx.resources.http.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.resources.loader;

{ Abortable, bounded public-file fetch. Cookies/ambient credentials and redirects
  are omitted/refused. CORS and mixed-content policy remain browser capabilities,
  reported as loading failures rather than invented local success. All callbacks
  run on the UI event loop; cancellation retires the receiver immediately. }
function NewNyxBrowserResourceTransport: INyxResourceTransport;

implementation

uses SysUtils, JS, Web, WebOrWorker, nyx.text, nyx.bytes,
  nyx.resource.sources, nyx.resource.cache;

type
  TBrowserRequest = class(TNyxResourceRequest)
  private
    FURL: TNyxResourceURL;
    FOptions: TNyxResourceLoadOptions;
    FMaximum: Integer;
    FController: TJSAbortController;
    FTimer: NativeInt;
    FTimedOut: Boolean;
    FLease: INyxResourceRequest;
    function Run: JSValue; async;
    procedure Expired;
  public
    procedure Start;
    procedure Cancel; override;
  end;
  TBrowserTransport = class(TInterfacedObject, INyxResourceTransport)
    function Request(const AURL: TNyxResourceURL;
      const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
  end;

function Header(AResponse: TJSResponse; const AName: String): TNyxText;
var
  LValue: JSValue;
begin
  LValue := AResponse.headers.get(AName);
  Result := '';

  if isString(LValue) then
  begin
    Result := String(LValue);
  end;
end;

procedure TBrowserRequest.Expired;
begin
  FTimedOut := True;
  FController.abort;
end;

procedure TBrowserRequest.Cancel;
begin
  inherited Cancel;

  if FController <> nil then
  begin
    FController.abort;
  end;
end;

procedure TBrowserRequest.Start;
begin
  FLease := Self;
  FController := TJSAbortController.new;
  FTimer := window.setTimeout(@Expired, FOptions.DeadlineMS);
  Run;
end;

function TBrowserRequest.Run: JSValue; async;
var
  LOptions: TJSObject;
  LResponse: TJSResponse;
  LReader: TJSReadableStreamDefaultReader;
  LPacket: TJSObject;
  LChunk: TJSUint8Array;
  LBytes: TNyxBytes;
  LCount: Integer;
  LIndex: Integer;
  LResult: TNyxResourceHTTPResult;
  LCacheControl: TNyxText;
begin
  Result := Undefined;
  LReader := nil;
  LResult := Default(TNyxResourceHTTPResult);
  LBytes := nil;
  LCount := 0;
  try
    try
      LOptions := TJSObject.new;
      LOptions.Properties['signal'] := FController.signal;
      LOptions.Properties['credentials'] := 'omit';
      LOptions.Properties['redirect'] := 'error';
      { Nyx owns private cache policy. Do not use an independent browser HTTP
        cache hit whose response age/validation cannot be qualified here. }
      LOptions.Properties['cache'] := 'no-store';
      LResponse := await(fetchAsync(FURL.Address, LOptions));

      if not Active then
      begin
        Exit;
      end;
      LResult.Status := LResponse.status;
      LCacheControl := Header(LResponse, 'cache-control');

      if (LResponse.type_ = 'cors') and (Header(LResponse, 'age') = '') then
      begin
        { CORS can hide Age. Without exposure the Respect policy must fetch
          again instead of inventing zero age; explicit Override still uses
          caller freshness. Same-origin responses retain ordinary semantics. }
        LCacheControl := LCacheControl + TNyxText(',no-cache');
      end;
      LResult.Hints := NyxResourceCacheHeaders(LCacheControl, Header(LResponse, 'age'));

      if LResponse.body <> nil then
      begin
        LReader := LResponse.body.getReader();
        SetLength(LBytes, FMaximum);
        repeat
          LPacket := TJSObject(await(LReader.read()));

          if not Active then
          begin
            Exit;
          end;

          if Boolean(LPacket.Properties['done']) then
          begin
            Break;
          end;
          LChunk := TJSUint8Array(LPacket.Properties['value']);

          if LChunk.length > FMaximum - LCount then
          begin
            FController.abort;
            raise ENyxBytes.Create('Hosted reply exceeds the caller byte budget');
          end;
          for LIndex := 0 to LChunk.length - 1 do
          begin
            LBytes[LCount + LIndex] := LChunk[LIndex];
          end;
          Inc(LCount, LChunk.length);
        until False;
      end;
      SetLength(LBytes, LCount);
      LResult.Bytes := LBytes;
    except
      on LException: Exception do
      begin
        LResult.Error := 'Browser resource request failed; check URL, CORS and network access';

        if FTimedOut then
        begin
          LResult.Error := 'Whole resource request deadline expired';
        end
        else if LException is ENyxBytes then
        begin
          LResult.Error := LException.Message;
        end;
      end;
      else
      begin
        { Native DOM/Promise errors are not SysUtils.Exception instances in
          pas2js. They must still produce exactly one bounded loading result. }
        LResult.Error := 'Browser resource request failed; check URL, CORS and network access';

        if FTimedOut then
        begin
          LResult.Error := 'Whole resource request deadline expired';
        end;
      end;
    end;
  finally
    window.clearTimeout(FTimer);

    try

      if LReader <> nil then
      begin
        LReader.releaseLock();
      end;
    finally
      { Keep the pending lease through callback execution, including receiver
        exceptions or platform reader cleanup failure. }
      try
        Complete(LResult);
      finally
        FLease := nil;
      end;
    end;
  end;
end;

function TBrowserTransport.Request(const AURL: TNyxResourceURL;
  const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
var
  LURL: TNyxResourceURL;
  LRequest: TBrowserRequest;
begin
  AOptions.Validate;
  LURL := NyxResourceURL(AURL.Address);

  if (AMaximumBytes < 1) or (AMaximumBytes > NyxMaximumPackedBytes) then
  begin
    raise ENyxBytes.Create('Hosted transport requires 1..1 MiB of payload budget');
  end;
  LRequest := TBrowserRequest.Create(AReply);
  Result := LRequest;
  LRequest.FURL := LURL;
  LRequest.FOptions := AOptions;
  LRequest.FMaximum := AMaximumBytes;
  LRequest.Start;
end;

function NewNyxBrowserResourceTransport: INyxResourceTransport;
begin
  Result := TBrowserTransport.Create;
end;

end.
