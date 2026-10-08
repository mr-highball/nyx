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
unit nyx.resource.cache.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.resources, nyx.resource.sources, nyx.resource.cache;

{ Native browser Cache Storage owns a versioned private cache. It needs an
  available secure-context API; unavailable/quota/corrupt storage reports errors
  through the same callback contract so a resolver can select memory fallback.
  Cancel retires the receiver. An already issued Cache.put may still finish;
  this storage operation never publishes data into a document or mounted view.
  Browser counterparts must execute before this adapter earns parity evidence. }
function NewNyxBrowserResourceCache(AEntryLimit: Integer = 128;
  AByteLimit: Integer = 16777216): INyxResourceCacheStorage;

implementation

uses SysUtils, JS, Web, nyx.text, nyx.bytes, nyx.data, nyx.state;

const
  CCacheName = 'nyx-resource-cache-v1';
  CSizeHeader = 'x-nyx-cache-bytes';

type
  TBrowserCacheJob = class(TNyxResourceCacheJob)
  private
    FURL: TNyxResourceURL;
    FKind: TNyxResourceKind;
    FEntry: TNyxResourceCacheEntry;
    FWriting: Boolean;
    FEntryLimit: Integer;
    FByteLimit: Integer;
    FPendingLease: INyxResourceCacheJob;
    function Run: JSValue; async;
  public
    procedure Start;
  end;
  TBrowserCache = class(TInterfacedObject, INyxResourceCacheStorage)
  private
    FEntryLimit: Integer;
    FByteLimit: Integer;
  public
    constructor Create(AEntryLimit, AByteLimit: Integer);
    function Read(const AURL: TNyxResourceURL; AKind: TNyxResourceKind;
      AReply: TNyxResourceCacheRead): INyxResourceCacheJob;
    function Write(const AEntry: TNyxResourceCacheEntry;
      AReply: TNyxResourceCacheWrite): INyxResourceCacheJob;
  end;

var
  { All providers share one named cache on one browser UI event loop. Refusing
    a concurrent write avoids racing quota checks; reads remain independent. }
  GWriting: Boolean;

function CacheSize(AResponse: TJSResponse; out ASize: Integer): Boolean;
var
  LValue: JSValue;
begin
  LValue := AResponse.headers.get(CSizeHeader);
  Result := isString(LValue);

  if Result then
  begin
    Result := TryNyxStateInteger(String(LValue), ASize);
  end;
end;

function TBrowserCacheJob.Run: JSValue; async;
var
  LStorage: TJSCacheStorage;
  LCache: TJSCache;
  LResponse: TJSResponse;
  LKeys: TJSArray;
  LRequest: TJSRequest;
  LKey: String;
  LText: TNyxText;
  LError: TNyxText;
  LCount: Integer;
  LBytes: Integer;
  LSize: Integer;
  LIndex: Integer;
  LFound: Boolean;
  LWriteLease: Boolean;
begin
  Result := Undefined;
  LError := '';
  LFound := False;
  LWriteLease := False;
  try
    try
      LStorage := TJSCacheStorage(TJSObject(window).Properties['caches']);

      if (LStorage = nil) or isUndefined(LStorage) then
      begin
        raise ENyxResource.Create('Browser Cache Storage is unavailable; use a memory cache');
      end;

      if FWriting then
      begin

        if GWriting then
        begin
          raise ENyxResource.Create('Browser resource cache is writing; retry after completion');
        end;
        GWriting := True;
        LWriteLease := True;
      end;
      LCache := TJSCache(await(LStorage.open(CCacheName)));

      if not Active then
      begin
        Exit;
      end;
      LKey := window.location.origin + '/.nyx/resource-cache?kind=' + IntToStr(Ord(FKind)) +
        '&url=' + encodeURIComponent(FURL.Address);

      if FWriting then
      begin
        LText := FEntry.ToData.ToJSON;
        LBytes := NyxUTF8ByteCount(LText);
        LCount := 1;
        LKeys := TJSArray(await(LCache.keys));
        for LIndex := 0 to LKeys.length - 1 do
        begin
          LRequest := TJSRequest(LKeys[LIndex]);

          if LRequest.url = LKey then
          begin
            Continue;
          end;
          LResponse := TJSResponse(await(LCache.match(LRequest)));

          if (LResponse = nil) or isUndefined(LResponse) or not CacheSize(LResponse, LSize) or
            (LSize < 1) or (LSize > 2 * NyxMaximumPackedBytes + 16384) then
          begin
            raise ENyxResource.Create('Browser cache contains an invalid size envelope');
          end;
          Inc(LCount);
          Inc(LBytes, LSize);
        end;

        if (LCount > FEntryLimit) or (LBytes > FByteLimit) then
        begin
          raise ENyxResource.Create('Browser resource cache storage budget is full');
        end;

        if not Active then
        begin
          Exit;
        end;
        LResponse := TJSResponse.new(LText);
        LResponse.headers.set_(CSizeHeader, IntToStr(NyxUTF8ByteCount(LText)));
        await(LCache.put(LKey, LResponse));
      end
      else
      begin
        LResponse := TJSResponse(await(LCache.match(LKey)));

        if (LResponse <> nil) and not isUndefined(LResponse) then
        begin

          if not CacheSize(LResponse, LSize) or
            (LSize < 1) or (LSize > 2 * NyxMaximumPackedBytes + 16384) then
          begin
            raise ENyxResource.Create('Browser cache envelope exceeds its byte budget');
          end;
          LText := await(LResponse.text());

          if NyxUTF8ByteCount(LText) <> LSize then
          begin
            raise ENyxResource.Create('Browser cache envelope has changed size');
          end;
          FEntry := TNyxResourceCacheEntry.FromData(TNyxDataValue.ParseJSON(LText));

          if (FEntry.URL.Address <> FURL.Address) or (FEntry.Kind <> FKind) then
          begin
            raise ENyxResource.Create('Browser cache entry belongs to another resource');
          end;
          LFound := True;
        end;
      end;
    except
      on LException: Exception do
      begin
        LError := LException.Message;
      end;
      else
      begin
        { Quota/security/unavailable API rejections are ordinary JS errors,
          not Pascal exceptions. Retire the job and let the resolver select
          memory fallback instead of leaving its borrowed receiver waiting. }
        LError := 'Browser resource storage failed; use memory or check storage access';
      end;
    end;

    if LWriteLease then
    begin
      GWriting := False;
      LWriteLease := False;
    end;

    if FWriting then
    begin
      CompleteWrite(LError = '', LError);
    end
    else
    begin
      CompleteRead(LFound, FEntry, LError);
    end;
  finally

    if LWriteLease then
    begin
      GWriting := False;
    end;
    FPendingLease := nil;
  end;
end;

procedure TBrowserCacheJob.Start;
begin
  FPendingLease := Self;
  Run;
end;

constructor TBrowserCache.Create(AEntryLimit, AByteLimit: Integer);
begin
  inherited Create;

  if (AEntryLimit < 1) or (AEntryLimit > 128) or
    (AByteLimit < 1) or (AByteLimit > 16777216) then
  begin
    raise ENyxResource.Create('Browser cache requires 1..128 entries and 1..16 MiB');
  end;
  FEntryLimit := AEntryLimit;
  FByteLimit := AByteLimit;
end;

function TBrowserCache.Read(const AURL: TNyxResourceURL; AKind: TNyxResourceKind;
  AReply: TNyxResourceCacheRead): INyxResourceCacheJob;
var
  LJob: TBrowserCacheJob;
begin
  NyxResourceKindName(AKind);
  LJob := TBrowserCacheJob.Create(AReply);
  Result := LJob;
  LJob.FURL := NyxResourceURL(AURL.Address);
  LJob.FKind := AKind;
  LJob.Start;
end;

function TBrowserCache.Write(const AEntry: TNyxResourceCacheEntry;
  AReply: TNyxResourceCacheWrite): INyxResourceCacheJob;
var
  LJob: TBrowserCacheJob;
begin
  LJob := TBrowserCacheJob.Create(AReply);
  Result := LJob;
  LJob.FEntry := TNyxResourceCacheEntry.FromData(AEntry.ToData);
  LJob.FURL := LJob.FEntry.URL;
  LJob.FKind := LJob.FEntry.Kind;
  LJob.FWriting := True;
  LJob.FEntryLimit := FEntryLimit;
  LJob.FByteLimit := FByteLimit;
  LJob.Start;
end;

function NewNyxBrowserResourceCache(AEntryLimit, AByteLimit: Integer): INyxResourceCacheStorage;
begin
  Result := TBrowserCache.Create(AEntryLimit, AByteLimit);
end;

end.
