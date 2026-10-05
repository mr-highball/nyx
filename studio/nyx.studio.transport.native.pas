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

unit nyx.studio.transport.native;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, SyncObjs, fphttpclient, ssockets, nyx.studio.transport;

type
  { Distinct local stop conditions. Transport consumers translate these into
    bounded wire/help failures; neither exception revokes server admission. }
  ENyxTransportCanceled = class(Exception);
  ENyxTransportDeadline = class(Exception);

  { Worker owns this lifetime before Start and frees it only after join. UI
    cancellation sets a thread-safe event; only the worker checks elapsed time.
    It borrows no widget, source tree or callback receiver. Monotonic elapsed
    time starts at creation and is never extended by network progress. }
  TNyxHTTPRequestLifetime = class
  private
    FStarted: QWord;
    FLimits: TNyxTransportLimits;
    FCanceled: TEvent;
  public
    { AStartedTick is an optional host monotonic anchor, captured when a request
      queues behind retiring work. Zero starts now; future anchors refuse. }
    constructor Create(const ALimits: TNyxTransportLimits; AStartedTick: QWord = 0);
    destructor Destroy; override;
    procedure Cancel;
    procedure Check;
    function RemainingMS: Integer;
  end;

  { Byte-only loopback HTTP adapter. Borrows a worker-owned lifetime. Its socket
    handler uses nonblocking reads/writes and short readiness waits, covering
    stalled headers, body trickles and upload backpressure on both FPC versions.
    No concurrent thread frees/closes its socket. Numeric loopback connection
    waits use the remaining deadline, capped at five seconds; TLS/DNS refuse.
    HTTP framing remains FPC-owned and redirects remain disabled. }
  TNyxDeadlineHTTPClient = class(TFPHTTPClient)
  private
    FLifetime: TNyxHTTPRequestLifetime;
  protected
    function GetSocketHandler(const UseSSL: Boolean): TSocketHandler; override;
    procedure ConnectToServer(const AHost: String; APort: Integer;
      UseSSL: Boolean = False); override;
  public
    constructor CreateFor(ALifetime: TNyxHTTPRequestLifetime);
  end;

implementation

uses
  {$ifdef windows}Winsock2{$else}BaseUnix{$endif};

type
  TNativeReadiness = (nrRead, nrWrite);
  TDeadlineSocketHandler = class(TSocketHandler)
  private
    FLifetime: TNyxHTTPRequestLifetime;
    procedure WaitReady(AState: TNativeReadiness);
    function WouldBlock: Boolean;
  public
    constructor CreateFor(ALifetime: TNyxHTTPRequestLifetime);
    function Connect: Boolean; override;
    function Recv(const Buffer; Count: Integer): Integer; override;
    function Send(const Buffer; Count: Integer): Integer; override;
  end;

constructor TNyxHTTPRequestLifetime.Create(const ALimits: TNyxTransportLimits;
  AStartedTick: QWord);
begin
  inherited Create;

  ValidateNyxTransportLimits(ALimits);
  FLimits := ALimits;
  FStarted := GetTickCount64;

  if AStartedTick > FStarted then
  begin
    raise ENyxTransportDeadline.Create('Transport start must use the host monotonic clock');
  end;

  if AStartedTick <> 0 then
  begin
    FStarted := AStartedTick;
  end;
  FCanceled := TEvent.Create(nil, True, False, '');
end;

destructor TNyxHTTPRequestLifetime.Destroy;
begin
  FCanceled.Free;
  inherited Destroy;
end;

procedure TNyxHTTPRequestLifetime.Cancel;
begin
  FCanceled.SetEvent;
end;

procedure TNyxHTTPRequestLifetime.Check;
begin

  if FCanceled.WaitFor(0) = wrSignaled then
  begin
    raise ENyxTransportCanceled.Create('Transport request canceled');
  end;

  if GetTickCount64 - FStarted >= QWord(FLimits.DeadlineMS) then
  begin
    raise ENyxTransportDeadline.Create('Whole-request transport deadline expired');
  end;
end;

function TNyxHTTPRequestLifetime.RemainingMS: Integer;
begin
  Check;
  Result := FLimits.DeadlineMS - Integer(GetTickCount64 - FStarted);

  if Result < 1 then
  begin
    raise ENyxTransportDeadline.Create('Whole-request transport deadline expired');
  end;
end;

constructor TDeadlineSocketHandler.CreateFor(ALifetime: TNyxHTTPRequestLifetime);
begin
  inherited Create;
  FLifetime := ALifetime;
end;

function TDeadlineSocketHandler.Connect: Boolean;
var
  {$ifdef windows}
  LMode: LongWord;
  {$else}
  LFlags: Integer;
  {$endif}
begin
  Result := inherited Connect;
  FLifetime.Check;
  { Switch only after the numeric loopback connect succeeds. The ordinary FPC
    handler keeps ownership and closes this exact socket on every exit. }
  {$ifdef windows}
  LMode := 1;

  if ioctlsocket(Socket.Handle, LongInt(FIONBIO), @LMode) <> 0 then
  {$else}
  LFlags := fpfcntl(Socket.Handle, F_GETFL, 0);

  if (LFlags < 0) or (fpfcntl(Socket.Handle, F_SETFL, LFlags or O_NONBLOCK) < 0) then
  {$endif}
  begin
    raise Exception.Create('Transport could not admit nonblocking socket work');
  end;
end;

procedure TDeadlineSocketHandler.WaitReady(AState: TNativeReadiness);
var
  LWait: Integer;
  LReady: TFDSet;
  LExceptions: TFDSet;
  LTimeout: TTimeVal;
  LResult: Integer;
  LException: Boolean;
begin
  repeat
    LWait := FLifetime.RemainingMS;

    if LWait > 50 then
    begin
      LWait := 50;
    end;
    { Stable FPC predates TSocketHandler.Select. Use the native readiness API
      behind this adapter, preserving the same nonblocking handler on both
      compiler versions instead of depending on a newer FCL method. }
    LReady := Default(TFDSet);
    LExceptions := Default(TFDSet);
    LTimeout.tv_sec := 0;
    LTimeout.tv_usec := LWait * 1000;
    {$ifdef windows}
    FD_Set(Socket.Handle, LReady);
    FD_Set(Socket.Handle, LExceptions);

    if AState = nrRead then
    begin
      LResult := Winsock2.select(0, @LReady, nil, @LExceptions, @LTimeout);
    end
    else
    begin
      LResult := Winsock2.select(0, nil, @LReady, @LExceptions, @LTimeout);
    end;
    LException := FD_IsSet(Socket.Handle, LExceptions);
    {$else}
    fpFD_Set(Socket.Handle, LReady);
    fpFD_Set(Socket.Handle, LExceptions);

    if AState = nrRead then
    begin
      LResult := fpSelect(Socket.Handle + 1, @LReady, nil, @LExceptions, @LTimeout);
    end
    else
    begin
      LResult := fpSelect(Socket.Handle + 1, nil, @LReady, @LExceptions, @LTimeout);
    end;
    LException := fpFD_IsSet(Socket.Handle, LExceptions) <> 0;
    {$endif}
    FLifetime.Check;

    if (LResult < 0) or ((LResult > 0) and LException) then
    begin
      raise Exception.Create('Transport socket readiness failed');
    end;
  until LResult > 0;
end;

function TDeadlineSocketHandler.WouldBlock: Boolean;
begin
  {$ifdef windows}
  Result := LastError = WSAEWOULDBLOCK;
  {$else}
  Result := LastError = ESysEAGAIN;
  {$endif}
end;

function TDeadlineSocketHandler.Recv(const Buffer; Count: Integer): Integer;
begin
  repeat
    WaitReady(nrRead);
    Result := inherited Recv(Buffer, Count);
    FLifetime.Check;
  until (Result >= 0) or not WouldBlock;
end;

function TDeadlineSocketHandler.Send(const Buffer; Count: Integer): Integer;
var
  LOffset: Integer;
  LSent: Integer;
begin
  LOffset := 0;
  while LOffset < Count do
  begin
    WaitReady(nrWrite);
    LSent := inherited Send(PByte(@Buffer)[LOffset], Count - LOffset);
    FLifetime.Check;

    if LSent > 0 then
    begin
      Inc(LOffset, LSent);
    end
    else if (LSent = 0) or not WouldBlock then
    begin
      Exit(-1);
    end;
  end;
  { Stable FPC's WriteBuffer requires a complete write; newer HTTP framing also
    accepts it. Partial native sends never masquerade as an admitted request. }
  Result := LOffset;
end;

constructor TNyxDeadlineHTTPClient.CreateFor(ALifetime: TNyxHTTPRequestLifetime);
begin
  inherited Create(nil);

  if ALifetime = nil then
  begin
    raise Exception.Create('HTTP transport requires its owned request lifetime');
  end;
  FLifetime := ALifetime;
  AllowRedirect := False;
end;

function TNyxDeadlineHTTPClient.GetSocketHandler(const UseSSL: Boolean): TSocketHandler;
begin

  if UseSSL then
  begin
    raise Exception.Create('Private transport requires the local HTTP origin');
  end;
  Result := TDeadlineSocketHandler.CreateFor(FLifetime);
end;

procedure TNyxDeadlineHTTPClient.ConnectToServer(const AHost: String;
  APort: Integer; UseSSL: Boolean);
var
  LWait: Integer;
begin

  if (AHost <> '127.0.0.1') or UseSSL or (APort < 1) or (APort > 65535) then
  begin
    raise Exception.Create('Private transport requires the local HTTP origin');
  end;
  LWait := FLifetime.RemainingMS;

  if LWait > 5000 then
  begin
    LWait := 5000;
  end;
  ConnectTimeout := LWait;
  inherited ConnectToServer(AHost, APort, UseSSL);
  FLifetime.Check;
end;

end.
