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

program nyx_transport_deadline_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Forms, SyncObjs, Winsock2, md5,
  nyx.text, nyx.data, nyx.studio.transport, nyx.studio.exchange.lcl,
  nyx.studio.preview, nyx.studio.preview.lcl, nyx.test.browser.host;

const
  CReply: TNyxText = '{"ok":true}';

type
  { Alternative implementations still cross the closed snapshot admission.
    This intentionally returns an unset record to qualify early refusal. }
  TEmptyPolicy = class(TInterfacedObject, INyxTransportPolicy)
    function WholeRequest(AMilliseconds: Integer): INyxTransportPolicy;
    function Snapshot: TNyxTransportLimits;
  end;

  { Qualification-only raw peer, never a Studio/MCP server. Its listener binds
    an OS-selected loopback port, handles only bounded test packets/files and
    retires its own handles. No editor capability, configuration or project is
    admitted. Scripted bytes test actual client socket waits and cancellation. }
  TPeerMode = (pmReply, pmSilent, pmHeaderTrickle, pmBodyTrickle, pmUploadBlocked);
  TProbePeer = class;
  { The peer may serve an abandoned trickle while the browser starts its next
    request. Each bounded connection owns its socket; the peer joins all of
    these workers before releasing the borrowed stop event or file root. }
  TProbeClient = class(TThread)
  private
    FPeer: TProbePeer;
    FSocket: TSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(APeer: TProbePeer; ASocket: TSocket);
    destructor Destroy; override;
  end;

  TProbePeer = class(TThread)
  private
    FListen: TSocket;
    FStop: TEvent;
    FPort: Integer;
    FMode: TPeerMode;
    FWeb: TNyxText;
    FClients: array of TProbeClient;
    function Ready(ASocket: TSocket; AWrite: Boolean): Boolean;
    function SendBytes(ASocket: TSocket; const ABytes: TNyxText): Boolean;
    function ReadByte(ASocket: TSocket; out AByte: AnsiChar): Boolean;
    procedure Serve(ASocket: TSocket);
  protected
    procedure Execute; override;
  public
    Accepted: TEvent;
    Connections: LongInt;
    constructor Create(AMode: TPeerMode; const AWeb: TNyxText = '');
    destructor Destroy; override;
    procedure Stop;
    function Origin: TNyxText;
    property Port: Integer read FPort;
  end;

  { Borrowed public adapter receiver. All callbacks must run on the fixture's
    main/UI thread and exactly once. Global notification count survives deletion
    of a receiver to detect incorrectly retained canceled/timer deliveries. }
  TReceiver = class
    Count: Integer;
    Status: Integer;
    Text: TNyxText;
    Prepared: Boolean;
    procedure Reply(AStatus: Integer; const AText: TNyxText);
    procedure Preview(ASucceeded: Boolean; const AError: TNyxText);
    procedure Tick;
  end;

  { Real-clock browser qualification. Read only bounded fixture attributes
    through the maintained Pascal CDP host; do not inject editor automation or
    use virtual-time screenshot capture for elapsed network assertions. }
  TTransportBrowser = class(TNyxBrowserHost)
  public
    procedure Verify;
  end;

var
  GChecks: Integer;
  GNotifications: Integer;
  GMainThread: TThreadID;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function TEmptyPolicy.WholeRequest(AMilliseconds: Integer): INyxTransportPolicy;
begin
  Result := Self;
end;

function TEmptyPolicy.Snapshot: TNyxTransportLimits;
begin
  Result := Default(TNyxTransportLimits);
end;

constructor TProbePeer.Create(AMode: TPeerMode; const AWeb: TNyxText);
var
  LAddress: TSockAddrIn;
  LSize: Integer;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FListen := INVALID_SOCKET;
  FStop := TEvent.Create(nil, True, False, '');
  Accepted := TEvent.Create(nil, True, False, '');
  FMode := AMode;
  FWeb := AWeb;
  FListen := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

  if FListen = INVALID_SOCKET then
  begin
    raise Exception.Create('Cannot create qualification socket');
  end;
  LAddress := Default(TSockAddrIn);
  LAddress.sin_family := AF_INET;
  LAddress.sin_addr.S_addr := inet_addr('127.0.0.1');
  LAddress.sin_port := 0;

  if (bind(FListen, LAddress, SizeOf(LAddress)) <> 0) or (listen(FListen, 8) <> 0) then
  begin
    raise Exception.Create('Cannot bind qualification peer');
  end;
  LSize := SizeOf(LAddress);

  if getsockname(FListen, LAddress, LSize) <> 0 then
  begin
    raise Exception.Create('Cannot inspect qualification port');
  end;
  FPort := ntohs(LAddress.sin_port);
  Start;
end;

procedure TProbePeer.Stop;
begin
  FStop.SetEvent;
end;

destructor TProbePeer.Destroy;
begin

  if FStop <> nil then
  begin
    Stop;
  end;
  inherited Destroy;

  if FListen <> INVALID_SOCKET then
  begin
    closesocket(FListen);
  end;
  Accepted.Free;
  FStop.Free;
end;

function TProbePeer.Origin: TNyxText;
begin
  Result := 'http://127.0.0.1:' + IntToStr(FPort);
end;

function TProbePeer.Ready(ASocket: TSocket; AWrite: Boolean): Boolean;
var
  LSet: TFDSet;
  LWait: TTimeVal;
  LSelected: Integer;
begin
  Result := False;
  LSet := Default(TFDSet);
  FD_Set(ASocket, LSet);
  LWait.tv_sec := 0;
  LWait.tv_usec := 50000;

  if AWrite then
  begin
    LSelected := Winsock2.select(0, nil, @LSet, nil, @LWait);
  end
  else
  begin
    LSelected := Winsock2.select(0, @LSet, nil, nil, @LWait);
  end;
  Result := (FStop.WaitFor(0) <> wrSignaled) and (LSelected > 0);
end;

function TProbePeer.SendBytes(ASocket: TSocket; const ABytes: TNyxText): Boolean;
var
  LOffset: Integer;
  LSent: Integer;
  LStarted: QWord;
begin
  LOffset := 0;
  LStarted := GetTickCount64;
  Result := False;
  while (LOffset < Length(ABytes)) and (FStop.WaitFor(0) <> wrSignaled) and
    (GetTickCount64 - LStarted < 3000) do
  begin

    if not Ready(ASocket, True) then
    begin
      Continue;
    end;
    LSent := send(ASocket, ABytes[LOffset + 1], Length(ABytes) - LOffset, 0);

    if LSent > 0 then
    begin
      Inc(LOffset, LSent);
    end
    else if (LSent = 0) or (WSAGetLastError <> WSAEWOULDBLOCK) then
    begin
      Exit;
    end;
  end;
  Result := LOffset = Length(ABytes);
end;

function TProbePeer.ReadByte(ASocket: TSocket; out AByte: AnsiChar): Boolean;
var
  LReceived: Integer;
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  Result := False;
  while (FStop.WaitFor(0) <> wrSignaled) and (GetTickCount64 - LStarted < 3000) do
  begin

    if not Ready(ASocket, False) then
    begin
      Continue;
    end;
    LReceived := recv(ASocket, AByte, 1, 0);

    if LReceived = 1 then
    begin
      Exit(True);
    end;

    if (LReceived = 0) or (WSAGetLastError <> WSAEWOULDBLOCK) then
    begin
      Exit;
    end;
  end;
end;

procedure TProbePeer.Serve(ASocket: TSocket);
var
  LHead: TNyxText;
  LBody: TNyxText;
  LResponse: TNyxText;
  LPath: TNyxText;
  LByte: AnsiChar;
  LIndex: Integer;
  LLength: Integer;
  LAt: Integer;
  LMode: TPeerMode;
  LFile: TFileStream;
begin
  LHead := '';
  while (Length(LHead) < 4096) and (Copy(LHead, Length(LHead) - 3, 4) <> #13#10#13#10) do
  begin

    if not ReadByte(ASocket, LByte) then
    begin
      Exit;
    end;
    LHead := LHead + LByte;
  end;

  if Copy(LHead, Length(LHead) - 3, 4) <> #13#10#13#10 then
  begin
    Exit;
  end;
  Accepted.SetEvent;
  LMode := FMode;
  LLength := 0;
  LAt := Pos('content-length:', LowerCase(LHead));

  if LAt > 0 then
  begin
    LPath := Trim(Copy(LHead, LAt + 15, Pos(#13#10, Copy(LHead, LAt + 15, MaxInt)) - 1));

    if not TryStrToInt(LPath, LLength) or (LLength < 0) then
    begin
      Exit;
    end;
  end;

  if LMode <> pmUploadBlocked then
  begin

    if LLength > 4096 then
    begin
      Exit;
    end;
    LBody := '';
    for LIndex := 1 to LLength do
    begin

      if not ReadByte(ASocket, LByte) then
      begin
        Exit;
      end;
      LBody := LBody + LByte;
    end;

    if LBody <> '' then
    begin
      LIndex := TNyxDataValue.ParseJSON(LBody).Field('case').AsInteger;

      if (LIndex < Ord(Low(TPeerMode))) or (LIndex > Ord(High(TPeerMode))) then
      begin
        Exit;
      end;
      LMode := TPeerMode(LIndex);
    end;
  end;
  LResponse := CReply;
  LPath := '';

  if Copy(LHead, 1, 4) = 'GET ' then
  begin
    LPath := Copy(LHead, 5, Pos(' HTTP/', LHead) - 5);
  end;

  if (FWeb <> '') and (LPath <> '') then
  begin

    if LPath = '/done' then
    begin
      SendBytes(ASocket, 'HTTP/1.1 200 OK'#13#10'Content-Length: 2'#13#10'Connection: close'#13#10#13#10'OK');
      Stop;
      Exit;
    end;
    { Closed names prevent arbitrary file browsing through this test listener. }

    if (LPath <> '/transport-tests.html') and (LPath <> '/transport-tests.js') and
      (LPath <> '/rtl.js') then
    begin
      SendBytes(ASocket, 'HTTP/1.1 404 Not Found'#13#10'Content-Length: 0'#13#10#13#10);
      Exit;
    end;
    LFile := TFileStream.Create(FWeb + PathDelim + Copy(LPath, 2, MaxInt), fmOpenRead or fmShareDenyNone);
    try
      SetLength(LResponse, LFile.Size);

      if LResponse <> '' then
      begin
        LFile.ReadBuffer(LResponse[1], Length(LResponse));
      end;
    finally
      LFile.Free;
    end;
    LMode := pmReply;
  end;
  LHead := 'HTTP/1.1 200 OK'#13#10'Content-Type: ';

  if LPath = '/transport-tests.html' then
  begin
    LHead := LHead + 'text/html; charset=utf-8';
  end
  else if (LPath = '/rtl.js') or (LPath = '/transport-tests.js') then
  begin
    LHead := LHead + 'application/javascript';
  end
  else
  begin
    LHead := LHead + 'application/json';
  end;
  LHead := LHead + #13#10'Content-Length: ' + IntToStr(Length(LResponse)) +
    #13#10'Connection: close'#13#10#13#10;
  case LMode of
    pmReply:
      begin
        SendBytes(ASocket, LHead + LResponse);
      end;
    pmSilent, pmUploadBlocked:
      begin
        FStop.WaitFor(2000);
      end;
    pmHeaderTrickle:
      begin
        for LIndex := 1 to Length(LHead) do
        begin

          if not SendBytes(ASocket, LHead[LIndex]) or (FStop.WaitFor(20) = wrSignaled) then
          begin
            Break;
          end;
        end;
      end;
    pmBodyTrickle:
      begin

        if SendBytes(ASocket, LHead) then
        begin
          for LIndex := 1 to Length(LResponse) do
          begin

            if not SendBytes(ASocket, LResponse[LIndex]) or (FStop.WaitFor(80) = wrSignaled) then
            begin
              Break;
            end;
          end;
        end;
      end;
  end;
end;

procedure TProbePeer.Execute;
var
  LClient: TSocket;
  LMode: LongWord;
  LIndex: Integer;
begin
  try
    while (FStop.WaitFor(0) <> wrSignaled) and (Length(FClients) < 64) do
    begin

      if not Ready(FListen, False) then
      begin
        Continue;
      end;
      LClient := accept(FListen, nil, nil);

      if LClient = INVALID_SOCKET then
      begin
        Continue;
      end;
      LMode := 1;
      ioctlsocket(LClient, LongInt(FIONBIO), @LMode);
      InterlockedIncrement(Connections);
      SetLength(FClients, Length(FClients) + 1);
      FClients[High(FClients)] := TProbeClient.Create(Self, LClient);
    end;
  finally
    Stop;
    for LIndex := 0 to High(FClients) do
    begin
      FClients[LIndex].Free;
    end;
  end;
end;

procedure TTransportBrowser.Verify;
var
  LStarted: QWord;
  LError: TNyxText;
  LCount: TNyxText;
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    LError := Attribute('data-nyx-transport-error');

    if LError <> '' then
    begin
      raise Exception.Create(LError);
    end;

    if GetTickCount64 - LStarted > 10000 then
    begin
      raise Exception.Create('Real-clock browser transport journey did not finish');
    end;
  until Attribute('data-nyx-transport-ready') = 'passed';
  LCount := Attribute('data-nyx-transport-checks');
  SetLength(LFields, 5);
  LFields[0] := NyxField('checks', NyxData(StrToInt(LCount)));
  for LIndex := 0 to 3 do
  begin
    LFields[LIndex + 1] := NyxField('elapsedMS' + IntToStr(LIndex),
      NyxData(Attribute('data-nyx-transport-elapsed-' + IntToStr(LIndex))));
  end;
  Save('browser-status.json', RawByteString(NyxObject(LFields).ToJSON));
  WriteLn('PASS ', LCount, ' actual real-clock browser transport checks');
end;

constructor TProbeClient.Create(APeer: TProbePeer; ASocket: TSocket);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FPeer := APeer;
  FSocket := ASocket;
  Start;
end;

destructor TProbeClient.Destroy;
begin
  inherited Destroy;
  closesocket(FSocket);
end;

procedure TProbeClient.Execute;
begin
  try
    FPeer.Serve(FSocket);
  except
    { A deliberately abandoned peer must not stop the next test request.
      The consumer asserts its own bounded status, bytes and callback count. }
  end;
end;

procedure TReceiver.Reply(AStatus: Integer; const AText: TNyxText);
begin
  Check(GetCurrentThreadID = GMainThread, 'Reply must run on UI thread');
  Inc(Count);
  Inc(GNotifications);
  Status := AStatus;
  Text := AText;
end;

procedure TReceiver.Preview(ASucceeded: Boolean; const AError: TNyxText);
begin
  Check(GetCurrentThreadID = GMainThread, 'Preview must run on UI thread');
  Inc(Count);
  Inc(GNotifications);
  Prepared := ASucceeded;
  Text := AError;
end;

procedure TReceiver.Tick;
begin
  Inc(GNotifications);
end;

procedure Pump(AMilliseconds: Integer);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Application.ProcessMessages;
    Sleep(2);
  until GetTickCount64 - LStarted >= QWord(AMilliseconds);
end;

procedure AwaitReply(AReceiver: TReceiver; AStarted: QWord);
begin
  while (AReceiver.Count = 0) and (GetTickCount64 - AStarted < 1800) do
  begin
    Pump(5);
  end;
  Check(AReceiver.Count = 1, 'Request must finish exactly once inside elapsed qualification bound');
end;

function Packet(AMode: TPeerMode): TNyxText;
begin
  Result := NyxObject([NyxField('case', NyxData(Ord(AMode)))]).ToJSON;
end;

function Artifact: TNyxCompiledArtifact;
const
  CPath = 'builds/job-637c834d-93a1-40a5-b718-7b631d3338ee/nyx_native.exe';
var
  LEntry: TNyxDataValue;
begin
  { Synthetic byte manifest tests preparation only; this is never an executable
    proof or a current compiler job and must never be launched. }
  LEntry := NyxObject([NyxField('path', NyxData(CPath)),
    NyxField('bytes', NyxData(Length(CReply))),
    NyxField('md5', NyxData(MD5Print(MD5Buffer(CReply[1], Length(CReply)))))]);
  Result := AdmitNyxCompiledArtifact(NyxObject([
    NyxField('state', NyxData('succeeded')), NyxField('target', NyxData('lcl')),
    NyxField('job', NyxData('transport-fixture')),
    NyxField('currentSource', NyxData(True)), NyxField('currentOutput', NyxData(True)),
    NyxField('artifact', NyxData(CPath)), NyxField('manifest', NyxArray([LEntry]))]));
end;

procedure NativeChecks(const ADirectory: TNyxText);
var
  LPolicy: INyxTransportPolicy;
  LSnapshot: TNyxTransportLimits;
  LPeer: TProbePeer;
  LExchange: TNyxLCLEditorExchange;
  LReceiver: TReceiver;
  LPreview: TNyxLCLCompiledPreview;
  LStarted: QWord;
  LElapsed: QWord;
  LCount: Integer;
  LMode: TPeerMode;
  LRefused: Boolean;
  LLarge: TNyxText;
begin
  LPolicy := NewNyxTransportPolicy.WholeRequest(220);
  LSnapshot := LPolicy.Snapshot;
  LPolicy.WholeRequest(1000);
  Check(LSnapshot.DeadlineMS = 220, 'Captured deadline cannot change through fluent policy mutation');
  LRefused := False;
  try
    LPolicy.WholeRequest(0);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Unbounded deadline must refuse');
  LRefused := False;
  try
    LExchange := TNyxLCLEditorExchange.Create('http://127.0.0.1:1', TEmptyPolicy.Create);
    LExchange.Free;
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Alternative unset policy refuses before native transport work');
  LPolicy.WholeRequest(220);
  for LMode := pmReply to pmBodyTrickle do
  begin
    LPeer := TProbePeer.Create(LMode);
    LReceiver := TReceiver.Create;
    LExchange := TNyxLCLEditorExchange.Create(LPeer.Origin, LPolicy);
    try
      LStarted := GetTickCount64;
      LExchange.Post(True, '', Packet(LMode), LReceiver.Reply);
      Check(LReceiver.Count = 0, 'Post never delivers inline');
      AwaitReply(LReceiver, LStarted);
      LElapsed := GetTickCount64 - LStarted;

      if LMode = pmReply then
      begin
        Check((LReceiver.Status = 200) and (LReceiver.Text = CReply), 'Normal request preserves exact reply bytes');
      end
      else
      begin
        Check((LReceiver.Status = 0) and (Pos('local work is retained', LReceiver.Text) > 0),
          'Stalled/trickling request refuses success and retains local work');
        Check((LElapsed >= 180) and (LElapsed < 1200), 'Progress must not reset the whole-request deadline');
      end;
      WriteLn('Native mode ', Ord(LMode), ' elapsed ', LElapsed, 'ms');
      Pump(80);
      Check(LReceiver.Count = 1, 'No duplicate terminal notification');
    finally
      LExchange.Free;
      LReceiver.Free;
      LPeer.Free;
    end;
  end;

  LPeer := TProbePeer.Create(pmSilent);
  LReceiver := TReceiver.Create;
  LExchange := TNyxLCLEditorExchange.Create(LPeer.Origin, NewNyxTransportPolicy.WholeRequest(15000));
  LExchange.Post(True, '', Packet(pmSilent), LReceiver.Reply);
  Check(LPeer.Accepted.WaitFor(1000) = wrSignaled, 'Canceled request really entered socket wait');
  LCount := GNotifications;
  LStarted := GetTickCount64;
  LExchange.CancelRequest;
  LReceiver.Free;
  LExchange.Free;
  LElapsed := GetTickCount64 - LStarted;
  Check(LElapsed < 1200, 'Canceled stalled socket retires before the default deadline');
  Pump(80);
  Check(GNotifications = LCount, 'Destroyed canceled receiver receives no notification');
  WriteLn('Canceled editor retirement ', LElapsed, 'ms');
  LPeer.Free;

  LPeer := TProbePeer.Create(pmReply);
  LReceiver := TReceiver.Create;
  LExchange := TNyxLCLEditorExchange.Create(LPeer.Origin,
    NewNyxTransportPolicy.WholeRequest(15000));
  try
    LExchange.Post(True, '', Packet(pmSilent), LReceiver.Reply);
    Check(LPeer.Accepted.WaitFor(1000) = wrSignaled, 'Deferred request follows an actual stalled socket');
    LExchange.CancelRequest;
    LStarted := GetTickCount64;
    LExchange.Post(True, '', Packet(pmReply), LReceiver.Reply);
    AwaitReply(LReceiver, LStarted);
    Check((LReceiver.Status = 200) and (LReceiver.Text = CReply),
      'Deferred request completes after canceled worker retirement');
    LCount := GNotifications;
    LExchange.Schedule(30, LReceiver.Tick);
    LExchange.CancelTick;
    LExchange.Free;
    LExchange := nil;
    LReceiver.Free;
    LReceiver := nil;
    Pump(80);
    Check(GNotifications = LCount, 'Canceled timer and former request never notify destroyed receiver');
  finally
    LExchange.Free;
    LReceiver.Free;
    LPeer.Free;
  end;

  LPeer := TProbePeer.Create(pmUploadBlocked);
  LReceiver := TReceiver.Create;
  LExchange := TNyxLCLEditorExchange.Create(LPeer.Origin, LPolicy);
  try
    SetLength(LLarge, 16 * 1024 * 1024);
    FillChar(LLarge[1], Length(LLarge), Ord('x'));
    LStarted := GetTickCount64;
    LExchange.Post(True, '', LLarge, LReceiver.Reply);
    AwaitReply(LReceiver, LStarted);
    LElapsed := GetTickCount64 - LStarted;
    Check(LPeer.Accepted.WaitFor(0) = wrSignaled, 'Upload fixture accepted an actual socket');
    Check((LReceiver.Status = 0) and (LElapsed < 1200), 'Upload backpressure cannot exceed whole deadline');
    WriteLn('Blocked upload elapsed ', LElapsed, 'ms');
  finally
    LExchange.Free;
    LReceiver.Free;
    LPeer.Free;
  end;
  LLarge := '';

  LPeer := TProbePeer.Create(pmReply);
  LReceiver := TReceiver.Create;
  LExchange := TNyxLCLEditorExchange.Create(LPeer.Origin,
    NewNyxTransportPolicy.WholeRequest(120));
  try
    LExchange.Post(True, '', Packet(pmSilent), LReceiver.Reply);
    Check(LPeer.Accepted.WaitFor(1000) = wrSignaled, 'Queued expiry starts behind an actual request');
    LExchange.CancelRequest;
    LStarted := GetTickCount64;
    LExchange.Post(True, '', Packet(pmReply), LReceiver.Reply);
    { Deliberately pause this fixture's UI pump until the queued deadline has
      elapsed. The adapter must not start a new budget when retiring the old
      worker, nor send the expired packet to the peer. Product code never sleeps. }
    Sleep(220);
    AwaitReply(LReceiver, LStarted);
    Check(LReceiver.Status = 0, 'Queued request retains its original deadline');
    Check(InterlockedCompareExchange(LPeer.Connections, 0, 0) = 1,
      'Expired queued request performs no new network admission');
  finally
    LExchange.Free;
    LReceiver.Free;
    LPeer.Free;
  end;

  for LMode := pmReply to pmBodyTrickle do
  begin
    LPeer := TProbePeer.Create(LMode);
    LReceiver := TReceiver.Create;
    LPreview := TNyxLCLCompiledPreview.Create(LPeer.Origin, ADirectory, LPolicy);
    try
      LStarted := GetTickCount64;
      LPreview.Prepare(Artifact, LReceiver.Preview);
      AwaitReply(LReceiver, LStarted);

      if LMode = pmReply then
      begin
        Check(LReceiver.Prepared and LPreview.Ready, 'Verified preparation still succeeds inside deadline');
      end
      else
      begin
        Check(not LReceiver.Prepared and not LPreview.Ready, 'Stalled artifact never becomes runnable');
        Check(GetTickCount64 - LStarted < 1200, 'Artifact trickles cannot reset whole deadline');
      end;
      Check(LPreview.ProcessID = 0, 'Preparation never launches synthetic bytes');
    finally
      LPreview.Free;
      LReceiver.Free;
      LPeer.Free;
    end;
  end;
  LPeer := TProbePeer.Create(pmSilent);
  LReceiver := TReceiver.Create;
  LPreview := TNyxLCLCompiledPreview.Create(LPeer.Origin, ADirectory);
  LPreview.Prepare(Artifact, LReceiver.Preview);
  Check(LPeer.Accepted.WaitFor(1000) = wrSignaled, 'Canceled artifact really entered socket wait');
  LStarted := GetTickCount64;
  LCount := GNotifications;
  LPreview.Cancel;
  LReceiver.Free;
  LPreview.Free;
  LElapsed := GetTickCount64 - LStarted;
  Check(LElapsed < 1200, 'Canceled artifact worker retires before default deadline');
  Pump(80);
  Check(GNotifications = LCount, 'Canceled preview never notifies a destroyed receiver');
  WriteLn('Canceled preview retirement ', LElapsed, 'ms');
  LPeer.Free;
  Check(not DirectoryExists(ADirectory) or RemoveDir(ADirectory), 'Owned preview directory retires empty');
end;

var
  LPeer: TProbePeer;
  LReady: TFileStream;
  LOrigin: TNyxText;
  LStarted: QWord;
  LBrowser: TTransportBrowser;
  LHTML: TNyxText;
begin
  Application.Initialize;
  GMainThread := GetCurrentThreadID;
  try

    if (ParamCount = 2) and (ParamStr(1) = '--prepare-browser') then
    begin
      { Pascal owns the fixture host artifact; shell orchestration only invokes
        compilers, copies their matched runtime and calls these closed modes. }
      LHTML := '<!doctype html><html lang="en"><meta charset="utf-8">' +
        '<title>Transport deadline review</title><body><h1>Transport deadline review</h1>' +
        '<script src="rtl.js"></script><script src="transport-tests.js"></script>' +
        '<script>rtl.run();</script></body></html>';
      LReady := TFileStream.Create(IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2))) +
        'transport-tests.html', fmCreate);
      try
        LReady.WriteBuffer(LHTML[1], Length(LHTML));
      finally
        LReady.Free;
      end;
    end
    else if (ParamCount = 3) and (ParamStr(1) = '--browser') then
    begin
      LPeer := TProbePeer.Create(pmReply, ExpandFileName(ParamStr(2)));
      try
        LBrowser := TTransportBrowser.Create(LPeer.Origin + '/transport-tests.html',
          ExpandFileName(ParamStr(3)));
        try
          LBrowser.Verify;
        finally
          LBrowser.Free;
        end;
      finally
        LPeer.Free;
      end;
    end
    else if (ParamCount = 3) and (ParamStr(1) = '--serve') then
    begin
      LPeer := TProbePeer.Create(pmReply, ExpandFileName(ParamStr(2)));
      try
        LOrigin := LPeer.Origin;
        LReady := TFileStream.Create(ParamStr(3), fmCreate);
        try
          LReady.WriteBuffer(LOrigin[1], Length(LOrigin));
        finally
          LReady.Free;
        end;
        LStarted := GetTickCount64;
        while (LPeer.FStop.WaitFor(0) <> wrSignaled) and (GetTickCount64 - LStarted < 60000) do
        begin
          Sleep(20);
        end;
      finally
        LPeer.Free;
      end;
      WriteLn('Qualification peer retired');
    end
    else
    begin

      if ParamCount <> 1 then
      begin
        raise Exception.Create('Provide an explicitly owned preview directory');
      end;
      NativeChecks(ExpandFileName(ParamStr(1)));
      WriteLn('PASS ', GChecks, ' actual native transport deadline checks');
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      Halt(1);
    end;
  end;
end.
