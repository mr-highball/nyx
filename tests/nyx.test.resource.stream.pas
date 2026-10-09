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

unit nyx.test.resource.stream;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.bytes
  {$ifndef PAS2JS}, Classes, SysUtils, Windows, WinSock2{$endif};

const
  { Exceeds the native 8 KiB read buffer, ensuring a real completed read while
    the meaningful JSON tail remains withheld. Browser consumes the same bytes. }
  NyxStreamPrefixBytes = 32792;

type
  { Fixed qualification replies. These are wire fixtures, not product cache
    policies; no caller-supplied header or body is reflected over this socket. }
  TNyxTestResourceReply = (ntrNoStore, ntrFresh, ntrValidate, ntrRevalidate,
    ntrStale, ntrExpired, ntrConflict, ntrUnavailable);

{$ifndef PAS2JS}
type
  { Read-only qualification producer, never a Studio server. One OS-assigned
    loopback socket serves only a fresh capability path and fixed JSON bytes.
    A held reply sends a real prefix and withholds its tail until peer closure.
    ArmNext is explicit; no disk, compiler, editor/configuration API or user
    project is accessible. The owning driver must retire this exact thread. }
  TNyxResourceStreamFixture = class(TThread)
  private
    FListener: TSocket;
    FLock: TRTLCriticalSection;
    FLockReady: Boolean;
    FSocketReady: Boolean;
    FPath: TNyxText;
    FURL: TNyxText;
    FOrigin: TNyxText;
    FNextHold: Boolean;
    FNextRecovered: Boolean;
    FNextReply: TNyxTestResourceReply;
    FRequests: Integer;
    FClosedBodies: Integer;
    FError: TNyxText;
    function Readable(ASocket: TSocket; AMilliseconds: Integer): Boolean;
    function ReadHeaders(ASocket: TSocket): TNyxText;
    procedure SendBytes(ASocket: TSocket; const ABytes: TNyxBytes);
    procedure Serve(ASocket: TSocket);
  protected
    procedure Execute; override;
  public
    constructor Create(const AOrigin: TNyxText);
    { Driver-only coordination; producer fields are copied under its lock.
      A held request uses a different unadmitted caption, so partial publication
      is observable. Recovered selects the complete subsequent healthy value. }
    procedure ArmNext(AHold, ARecovered: Boolean);
    { Select fixed cache headers/status for the next policy journey. Each valid
      capability GET increments Requests, independently of application reports. }
    procedure ArmPolicy(AReply: TNyxTestResourceReply; ARecovered: Boolean = False);
    function Requests: Integer;
    function ClosedBodies: Integer;
    function Error: TNyxText;
    property URL: TNyxText read FURL;
    { Qualification may join its own bounded thread. Product resource disposal
      never joins network work. Terminate interrupts held/read/accept polling. }
    destructor Destroy; override;
  end;
{$endif}

implementation

{$ifndef PAS2JS}
uses nyx.resource.sources;

constructor TNyxResourceStreamFixture.Create(const AOrigin: TNyxText);
var
  LData: TWSAData;
  LAddress: TSockAddrIn;
  LLength: LongInt;
  LGuid: TGuid;
begin
  inherited Create(True);
  FListener := INVALID_SOCKET;
  InitCriticalSection(FLock);
  FLockReady := True;
  FOrigin := AOrigin;

  if FOrigin <> '' then
  begin
    { Typed URL validation rejects control/header injection. Origin is supplied
      by the owned browser driver, not reflected from an incoming request. }
    FOrigin := NyxResourceURL(FOrigin).Address;
  end;

  if WSAStartup($0202, LData) <> 0 then
  begin
    raise Exception.Create('Stream qualification cannot initialize system sockets');
  end;
  FSocketReady := True;
  FListener := WinSock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

  if FListener = INVALID_SOCKET then
  begin
    raise Exception.Create('Stream qualification cannot create its loopback socket');
  end;
  LAddress := Default(TSockAddrIn);
  LAddress.sin_family := AF_INET;
  LAddress.sin_addr.S_addr := inet_addr('127.0.0.1');
  LAddress.sin_port := 0;

  if WinSock2.bind(FListener, LAddress, SizeOf(LAddress)) <> 0 then
  begin
    raise Exception.Create('Stream qualification cannot bind its ephemeral loopback socket');
  end;

  if WinSock2.listen(FListener, 4) <> 0 then
  begin
    raise Exception.Create('Stream qualification cannot listen on its owned socket');
  end;
  LLength := SizeOf(LAddress);

  if getsockname(FListener, LAddress, LLength) <> 0 then
  begin
    raise Exception.Create('Stream qualification cannot inspect its assigned port');
  end;
  CreateGuid(LGuid);
  FPath := TNyxText('/' + StringReplace(StringReplace(StringReplace(
    GuidToString(LGuid), '{', '', [rfReplaceAll]), '}', '', [rfReplaceAll]),
    '-', '', [rfReplaceAll]));
  FURL := TNyxText('http://127.0.0.1:') + TNyxText(IntToStr(ntohs(LAddress.sin_port))) + FPath;
  Start;
end;

function TNyxResourceStreamFixture.Readable(ASocket: TSocket;
  AMilliseconds: Integer): Boolean;
var
  LRead: TFDSet;
  LWait: TTimeVal;
  LResult: Integer;
begin
  LRead := Default(TFDSet);
  LRead.fd_count := 1;
  LRead.fd_array[0] := ASocket;
  LWait.tv_sec := AMilliseconds div 1000;
  LWait.tv_usec := (AMilliseconds mod 1000) * 1000;
  LResult := WinSock2.select(0, @LRead, nil, nil, @LWait);

  if LResult = SOCKET_ERROR then
  begin
    raise Exception.Create('Stream qualification socket readiness failed');
  end;
  Result := LResult > 0;
end;

function TNyxResourceStreamFixture.ReadHeaders(ASocket: TSocket): TNyxText;
var
  LBuffer: array[0..2047] of Byte;
  LBytes: TNyxBytes;
  LCount: Integer;
  LRead: Integer;
  LStarted: QWord;
begin
  Result := '';
  LCount := 0;
  LStarted := GetTickCount64;
  while not Terminated and (GetTickCount64 - LStarted < 3000) do
  begin

    if not Readable(ASocket, 50) then
    begin
      Continue;
    end;
    LRead := WinSock2.recv(ASocket, @LBuffer[LCount], SizeOf(LBuffer) - LCount, 0);

    if LRead <= 0 then
    begin
      Exit;
    end;
    Inc(LCount, LRead);
    SetLength(LBytes, LCount);
    Move(LBuffer[0], LBytes[0], LCount);
    Result := NyxDecodeUTF8(LBytes);

    if Pos(#13#10#13#10, Result) > 0 then
    begin
      Exit;
    end;

    if LCount = SizeOf(LBuffer) then
    begin
      Exit('');
    end;
  end;
  Result := '';
end;

procedure TNyxResourceStreamFixture.SendBytes(ASocket: TSocket;
  const ABytes: TNyxBytes);
var
  LAt: Integer;
  LSent: Integer;
begin
  LAt := 0;
  while LAt < Length(ABytes) do
  begin
    LSent := WinSock2.send(ASocket, @ABytes[LAt], Length(ABytes) - LAt, 0);

    if LSent <= 0 then
    begin
      raise Exception.Create('Stream qualification cannot send its bounded reply');
    end;
    Inc(LAt, LSent);
  end;
end;

procedure TNyxResourceStreamFixture.Serve(ASocket: TSocket);
var
  LHeaders: TNyxText;
  LBody: TNyxText;
  LBytes: TNyxBytes;
  LPrefix: TNyxBytes;
  LHold: Boolean;
  LRecovered: Boolean;
  LReply: TNyxTestResourceReply;
  LCacheHeaders: TNyxText;
  LStatus: TNyxText;
  LStarted: QWord;
  LByte: Byte;
  LRead: Integer;
begin
  LHeaders := ReadHeaders(ASocket);

  if Copy(LHeaders, 1, Length(FPath) + 15) <> TNyxText('GET ') + FPath + ' HTTP/1.1' + #13#10 then
  begin
    { Empty speculative browser connections and other capability paths cannot
      change the next scripted reply. Close them without exposing any data. }
    Exit;
  end;
  EnterCriticalSection(FLock);
  try
    LHold := FNextHold;
    LRecovered := FNextRecovered;
    LReply := FNextReply;
    Inc(FRequests);
    FNextHold := False;
  finally
    LeaveCriticalSection(FLock);
  end;
  LBody := '{"headline":"Keep creating 🌙","prompt":"Project name 🌙"}';

  if LRecovered then
  begin
    LBody := '{"headline":"Back to creating 🌙","prompt":"A refreshed project 🌙"}';
  end;

  if LHold then
  begin
    LBody := TNyxText(StringOfChar(' ', NyxStreamPrefixBytes - 24)) +
      TNyxText('{"headline":"Partial text must stay hidden","prompt":"Unadmitted prompt"}');
  end;
  LBytes := NyxEncodeUTF8(LBody);
  LStatus := 'HTTP/1.1 200 OK';
  case LReply of
    ntrNoStore:
      begin
        LCacheHeaders := 'Cache-Control: no-store';
      end;
    ntrFresh:
      begin
        LCacheHeaders := TNyxText('Cache-Control: max-age=600') + #13#10 + 'Age: 1';
      end;
    ntrValidate:
      begin
        LCacheHeaders := TNyxText('Cache-Control: no-cache, max-age=600') + #13#10 + 'Age: 0';
      end;
    ntrRevalidate:
      begin
        LCacheHeaders := TNyxText('Cache-Control: max-age=0, must-revalidate') + #13#10 + 'Age: 1';
      end;
    ntrStale:
      begin
        LCacheHeaders := TNyxText('Cache-Control: max-age=60') + #13#10 + 'Age: 61';
      end;
    ntrExpired:
      begin
        LCacheHeaders := TNyxText('Cache-Control: max-age=60') + #13#10 + 'Age: 3600';
      end;
    ntrConflict:
      begin
        LCacheHeaders := TNyxText('Cache-Control: max-age=600, max-age=0') + #13#10 + 'Age: 0';
      end;
    ntrUnavailable:
      begin
        LStatus := 'HTTP/1.1 503 Service Unavailable';
        LCacheHeaders := 'Cache-Control: no-store';
      end;
  end;
  LHeaders := LStatus + #13#10 +
    'Content-Type: application/json' + #13#10 + LCacheHeaders + #13#10 +
    'Connection: close' + #13#10 + 'Content-Length: ' +
    TNyxText(IntToStr(Length(LBytes))) + #13#10;

  if FOrigin <> '' then
  begin
    LHeaders := LHeaders + TNyxText('Access-Control-Allow-Origin: ') + FOrigin + #13#10 +
      'Access-Control-Expose-Headers: Age' + #13#10;
  end;
  SendBytes(ASocket, NyxEncodeUTF8(LHeaders + #13#10));

  if not LHold then
  begin
    SendBytes(ASocket, LBytes);
    Exit;
  end;
  SetLength(LPrefix, NyxStreamPrefixBytes);
  Move(LBytes[0], LPrefix[0], Length(LPrefix));
  SendBytes(ASocket, LPrefix);
  LStarted := GetTickCount64;
  while not Terminated and (GetTickCount64 - LStarted < 10000) do
  begin

    if not Readable(ASocket, 50) then
    begin
      Continue;
    end;
    LRead := WinSock2.recv(ASocket, @LByte, 1, MSG_PEEK);

    if (LRead = 0) or ((LRead = SOCKET_ERROR) and (WSAGetLastError = WSAECONNRESET)) then
    begin
      EnterCriticalSection(FLock);
      try
        Inc(FClosedBodies);
      finally
        LeaveCriticalSection(FLock);
      end;
      Exit;
    end;
    raise Exception.Create('Stream qualification received unexpected held-body input');
  end;

  if not Terminated then
  begin
    raise Exception.Create('Stream qualification held body did not retire within ten seconds');
  end;
end;

procedure TNyxResourceStreamFixture.Execute;
var
  LPeer: TSocket;
  LSendTimeout: LongInt;
begin
  try
    try
      while not Terminated do
      begin

        if not Readable(FListener, 50) then
        begin
          Continue;
        end;
        LPeer := WinSock2.accept(FListener, nil, PLongInt(nil));

        if LPeer = INVALID_SOCKET then
        begin
          raise Exception.Create('Stream qualification cannot accept its request');
        end;
        try
          { Tiny replies normally fit the socket buffer. An unresponsive peer
            still cannot make qualification teardown join an unbounded send. }
          LSendTimeout := 3000;

          if setsockopt(LPeer, SOL_SOCKET, SO_SNDTIMEO, PAnsiChar(@LSendTimeout),
            SizeOf(LSendTimeout)) <> 0 then
          begin
            raise Exception.Create('Stream qualification cannot bound its socket send');
          end;
          Serve(LPeer);
        finally
          closesocket(LPeer);
        end;
      end;
    except
      on LException: Exception do
      begin
        EnterCriticalSection(FLock);
        try
          FError := TNyxText(LException.Message);
        finally
          LeaveCriticalSection(FLock);
        end;
      end;
    end;
  finally
    closesocket(FListener);
    FListener := INVALID_SOCKET;
  end;
end;

procedure TNyxResourceStreamFixture.ArmNext(AHold, ARecovered: Boolean);
begin
  EnterCriticalSection(FLock);
  try
    FNextHold := AHold;
    FNextRecovered := ARecovered;
    FNextReply := ntrNoStore;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TNyxResourceStreamFixture.ArmPolicy(AReply: TNyxTestResourceReply;
  ARecovered: Boolean);
begin
  EnterCriticalSection(FLock);
  try
    FNextHold := False;
    FNextRecovered := ARecovered;
    FNextReply := AReply;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TNyxResourceStreamFixture.Requests: Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result := FRequests;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TNyxResourceStreamFixture.ClosedBodies: Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result := FClosedBodies;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TNyxResourceStreamFixture.Error: TNyxText;
begin
  EnterCriticalSection(FLock);
  try
    Result := FError;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

destructor TNyxResourceStreamFixture.Destroy;
begin
  Terminate;
  { TThread.Destroy joins the exact owned thread, including construction refusal
    before Start. Its bounded select/read/held-body loops observe Terminated. }
  inherited Destroy;

  if FListener <> INVALID_SOCKET then
  begin
    closesocket(FListener);
  end;

  if FSocketReady then
  begin
    WSACleanup;
  end;

  if FLockReady then
  begin
    DoneCriticalSection(FLock);
  end;
end;

{$endif}

end.
