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


unit nyx.resources.http.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.resources.loader, nyx.scheduler;

{ Current Win32 LCL adapter: system-validated HTTP(S) through asynchronous
  WinHTTP on Nyx's bounded worker pool. Queued time counts toward the deadline.
  An independent LCL timer reports expiry on the serviced UI loop even when all
  workers remain occupied; it cancels work without joining native handles.
  UI callbacks are posted with the parent cancellation token. No worker touches
  a control, resource catalog or borrowed receiver. Declared Content-Length is
  verified before bytes leave the worker, including compressed representations.
  Compressed-length verification uses request statistics supported by Windows
  10 version 1903 / Windows Server 2019. An unavailable capability reports a
  normal load failure; it never admits an unverifiable prefix or disables TLS.
  Other native systems require another INyxResourceTransport implementation;
  they are not claimed qualified. }
function NewNyxNativeResourceTransport(const AScheduler: INyxScheduler): INyxResourceTransport;

implementation

uses SysUtils, nyx.text, nyx.bytes, nyx.resource.sources, nyx.resource.cache
  {$ifdef MSWINDOWS}, Windows, WinHTTP, SyncObjs, ExtCtrls{$endif};

{$ifdef MSWINDOWS}
const
  { Older FPC headers omit the newer Windows TLS 1.3 flag. Unsupported hosts
    fall back to TLS 1.2, retaining ordinary system certificate verification. }
  CProtocolTLS13 = $2000;
  { SDK additions absent from the matched FPC headers. Request statistics keep
    encoded body size distinct from WinHTTP's automatically decoded reads.
    https://learn.microsoft.com/windows/win32/api/winhttp/ns-winhttp-winhttp_request_stats }
  COptionRequestStats = 146;
  CRequestStatCount = 16;
  CRequestStatCapacity = 32;
  CResponseCompressedSize = 11;

type
  { Matches the SDK layout on both Windows pointer widths: 8+4+4 bytes followed
    by thirty-two 64-bit counters. No handle, pointer or managed field is owned. }
  TNativeRequestStats = packed record
    Flags: UInt64;
    Index: DWORD;
    Count: DWORD;
    Values: array[0..CRequestStatCapacity - 1] of UInt64;
  end;
  TNativeRequest = class;
  { Only the worker starts/closes handles. WinHTTP's status callback owns a
    temporary managed lease until HANDLE_CLOSING, its last notification. This
    protects the async read buffer and native context after timeout/cancellation,
    without joining native I/O from the UI or freeing an in-flight callback. }
  THTTPBridge = class(TInterfacedObject)
  private
    FSession: HINTERNET;
    FConnection: HINTERNET;
    FRequest: HINTERNET;
    FNotify: TEvent;
    FStatus: DWORD;
    FCount: DWORD;
    FError: DWORD;
    FLock: TRTLCriticalSection;
    FLease: IInterface;
    FBuffer: array[0..8191] of Byte;
    procedure Await(AExpected: DWORD; const AExecution: INyxExecution;
      AStarted: QWord; ADeadline: Integer);
    procedure Option(AOption, AValue: DWORD);
    function Header(const AName: UnicodeString): TNyxText;
    { A parsable prefix is not a completed HTTP representation. Verify declared
      encoded length before assigning result bytes; errors remain in the worker
      callback boundary. Compressed counters require Windows 10 version 1903+. }
    procedure VerifyFraming(ADecodedBytes: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close;
    function Fetch(const AURL: TNyxResourceURL; AMaximum, ADeadline: Integer;
      AStarted: QWord; const AExecution: INyxExecution;
      AProgress: TNativeRequest): TNyxResourceHTTPResult;
  end;
  TNativeRequest = class(TNyxResourceRequest, INyxResourceRequestProgress)
  private
    FProgressLock: TRTLCriticalSection;
    FProgressLockReady: Boolean;
    FProgress: TNyxResourceTransferProgress;
    FScheduler: INyxScheduler;
    FExecution: INyxExecution;
    FDeadlineTimer: TTimer;
    FDeadlineLease: INyxResourceRequest;
    FStarted: QWord;
    FDeadlineMS: Integer;
    procedure StopDeadline;
    procedure Deadline(Sender: TObject);
    { Worker writes only scalar evidence; terminal UI publication wins over
      late read/completion notifications. No UI work or allocation per chunk. }
    procedure RecordProgress(APhase: TNyxResourceTransferPhase; ABytes: Integer);
  public
    constructor Create(AReply: TNyxResourceHTTPReply); reintroduce;
    function Progress: TNyxResourceTransferProgress;
    { The temporary timer lease outlives dropping the caller's token. Completion,
      cancellation and expiry retire it on the UI thread. Worker work retains a
      separate lease until WinHTTP closing notifications have finished. }
    procedure StartDeadline(AStarted: QWord; AMilliseconds: Integer);
    destructor Destroy; override;
    procedure Cancel; override;
    procedure Deliver(const AResult: TNyxResourceHTTPResult);
  end;
  TResourceHTTPWork = class(TInterfacedObject, INyxWork)
  private
    FScheduler: INyxScheduler;
    FRequest: TNativeRequest;
    FLease: INyxResourceRequest;
    FURL: TNyxResourceURL;
    FOptions: TNyxResourceLoadOptions;
    FMaximum: Integer;
    FStarted: QWord;
  public
    procedure Execute(const AExecution: INyxExecution);
  end;
  TResourceHTTPDelivery = class(TInterfacedObject, INyxWork)
  private
    FRequest: TNativeRequest;
    FLease: INyxResourceRequest;
    FResult: TNyxResourceHTTPResult;
  public
    procedure Execute(const AExecution: INyxExecution);
  end;
  TNativeTransport = class(TInterfacedObject, INyxResourceTransport)
  private
    FScheduler: INyxScheduler;
  public
    constructor Create(const AScheduler: INyxScheduler);
    function Request(const AURL: TNyxResourceURL;
      const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
  end;

procedure HTTPStatus(AHandle: HINTERNET; AContext: DWORD_PTR;
  AStatus: DWORD; AInformation: Pointer; ALength: DWORD); stdcall;
var
  LBridge: THTTPBridge;
  LLease: IInterface;
begin

  if AContext = 0 then
  begin
    Exit;
  end;
  LBridge := THTTPBridge(Pointer(AContext));
  LLease := LBridge;

  if AStatus = WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING then
  begin
    LBridge.FLease := nil;
    Exit;
  end;
  EnterCriticalSection(LBridge.FLock);
  try
    LBridge.FStatus := AStatus;
    LBridge.FCount := ALength;

    if (AStatus = WINHTTP_CALLBACK_STATUS_REQUEST_ERROR) and
      (ALength >= SizeOf(WINHTTP_ASYNC_RESULT)) then
    begin
      LBridge.FError := LPWINHTTP_ASYNC_RESULT(AInformation)^.dwError;
    end;
  finally
    LeaveCriticalSection(LBridge.FLock);
  end;
  LBridge.FNotify.SetEvent;
  LLease := nil;
end;

constructor THTTPBridge.Create;
begin
  inherited Create;
  InitCriticalSection(FLock);
  FNotify := TEvent.Create(nil, False, False, '');
end;

destructor THTTPBridge.Destroy;
begin
  { Fetch always closes the request. Its pending callback lease prevents this
    destructor until the async handle has sent its final closing notification. }
  Close;

  if FConnection <> nil then
  begin
    WinHttpCloseHandle(FConnection);
  end;

  if FSession <> nil then
  begin
    WinHttpCloseHandle(FSession);
  end;
  FNotify.Free;
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure THTTPBridge.Close;
var
  LRequest: HINTERNET;
begin
  LRequest := FRequest;
  FRequest := nil;

  if LRequest <> nil then
  begin
    WinHttpCloseHandle(LRequest);
  end;
end;

procedure THTTPBridge.Option(AOption, AValue: DWORD);
begin

  if not WinHttpSetOption(FRequest, AOption, @AValue, SizeOf(AValue)) then
  begin
    raise ENyxBytes.Create('Cannot admit the native resource request policy');
  end;
end;

procedure THTTPBridge.Await(AExpected: DWORD; const AExecution: INyxExecution;
  AStarted: QWord; ADeadline: Integer);
var
  LStatus: DWORD;
  LError: DWORD;
begin
  repeat

    if AExecution.Cancelled then
    begin
      raise ENyxBytes.Create('Resource request cancelled');
    end;

    if GetTickCount64 - AStarted >= QWord(ADeadline) then
    begin
      raise ENyxBytes.Create('Whole resource request deadline expired');
    end;

    if FNotify.WaitFor(20) = wrSignaled then
    begin
      EnterCriticalSection(FLock);
      try
        LStatus := FStatus;
        LError := FError;
      finally
        LeaveCriticalSection(FLock);
      end;

      if LStatus = WINHTTP_CALLBACK_STATUS_REQUEST_ERROR then
      begin
        raise ENyxBytes.Create('Native resource request failed, code ' + IntToStr(LError));
      end;

      if LStatus = AExpected then
      begin
        Exit;
      end;
    end;
  until False;
end;

function THTTPBridge.Header(const AName: UnicodeString): TNyxText;
var
  LBuffer: array[0..4095] of WideChar;
  LSize: DWORD;
  LIndex: DWORD;
  LText: UnicodeString;
  LPart: TNyxText;
begin
  Result := '';
  LIndex := 0;
  repeat
    LSize := SizeOf(LBuffer);

    if not WinHttpQueryHeaders(FRequest, WINHTTP_QUERY_CUSTOM, PWideChar(AName),
      @LBuffer[0], @LSize, @LIndex) then
    begin

      if GetLastError <> ERROR_WINHTTP_HEADER_NOT_FOUND then
      begin
        raise ENyxBytes.Create('Resource response header exceeds its metadata budget');
      end;
      Exit;
    end;
    SetString(LText, PWideChar(@LBuffer[0]), LSize div SizeOf(WideChar));
    LPart := UTF8Encode(LText);

    if Result <> '' then
    begin
      Result := Result + TNyxText(',');
    end;
    Result := Result + LPart;

    if Length(Result) > 8192 then
    begin
      raise ENyxBytes.Create('Resource response headers exceed their metadata budget');
    end;
  until False;
end;

procedure THTTPBridge.VerifyFraming(ADecodedBytes: Integer);
var
  LHeader: TNyxText;
  LEncoding: TNyxText;
  LPart: TNyxText;
  LIndex: Integer;
  LStart: Integer;
  LPartIndex: Integer;
  LDigit: UInt64;
  LValue: UInt64;
  LExpected: UInt64;
  LActual: UInt64;
  LHasExpected: Boolean;
  LStats: TNativeRequestStats;
  LSize: DWORD;
begin
  LHeader := Header('content-length');

  if LHeader = '' then
  begin
    Exit;
  end;

  if Header('transfer-encoding') <> '' then
  begin
    raise ENyxBytes.Create('Hosted response has conflicting framing headers');
  end;
  LExpected := 0;
  LHasExpected := False;
  LStart := 1;
  { RFC 9112 section 6.3 permits repeated identical decimal lengths. Refuse signs,
    exponents, empty members, overflow and conflicting values without parsing
    through floating point or accepting the prefix of a malformed field. }
  for LIndex := 1 to Length(LHeader) + 1 do
  begin

    if (LIndex > Length(LHeader)) or (LHeader[LIndex] = ',') then
    begin
      LPart := Trim(Copy(LHeader, LStart, LIndex - LStart));

      if LPart = '' then
      begin
        raise ENyxBytes.Create('Hosted response has an invalid content length');
      end;
      LValue := 0;
      for LPartIndex := 1 to Length(LPart) do
      begin

        if not (LPart[LPartIndex] in ['0'..'9']) then
        begin
          raise ENyxBytes.Create('Hosted response has an invalid content length');
        end;
        LDigit := Ord(LPart[LPartIndex]) - Ord('0');

        if LValue > (High(UInt64) - LDigit) div 10 then
        begin
          raise ENyxBytes.Create('Hosted response content length exceeds the numeric budget');
        end;
        LValue := LValue * 10 + LDigit;
      end;

      if LHasExpected and (LValue <> LExpected) then
      begin
        raise ENyxBytes.Create('Hosted response has conflicting content lengths');
      end;
      LExpected := LValue;
      LHasExpected := True;
      LStart := LIndex + 1;
    end;
  end;
  LActual := ADecodedBytes;
  LEncoding := LowerCase(Trim(Header('content-encoding')));

  if (LEncoding <> '') and (LEncoding <> 'identity') then
  begin
    { Content-Length measures the encoded representation. Decoded reads cannot
      be compared with it. The OS counter avoids copying/decompressing again
      and catches premature EOF after a complete compressed member as well. }
    LStats := Default(TNativeRequestStats);
    LStats.Count := CRequestStatCount;
    LSize := SizeOf(LStats);

    if not WinHttpQueryOption(FRequest, COptionRequestStats, @LStats, @LSize) or
      (LSize <> SizeOf(LStats)) or (LStats.Count <= CResponseCompressedSize) then
    begin
      raise ENyxBytes.Create('Native system cannot verify the encoded response length');
    end;
    LActual := LStats.Values[CResponseCompressedSize];
  end;

  if LActual <> LExpected then
  begin
    raise ENyxBytes.Create('Hosted response did not complete its declared content length');
  end;
end;

function THTTPBridge.Fetch(const AURL: TNyxResourceURL;
  AMaximum, ADeadline: Integer; AStarted: QWord;
  const AExecution: INyxExecution; AProgress: TNativeRequest): TNyxResourceHTTPResult;
var
  LURL: UnicodeString;
  LHost: UnicodeString;
  LPath: UnicodeString;
  LExtra: UnicodeString;
  LParts: URL_COMPONENTS;
  LFlags: DWORD;
  LSize: DWORD;
  LStatus: DWORD;
  LProtocols: DWORD;
  LCount: Integer;
  LBytes: TNyxBytes;
  LContext: DWORD_PTR;
  LCallback: WINHTTP_STATUS_CALLBACK;
  LFragment: Integer;
  LResult: TNyxResourceHTTPResult;
begin
  LResult := Default(TNyxResourceHTTPResult);

  if AExecution.Cancelled then
  begin
    raise ENyxBytes.Create('Resource request cancelled');
  end;

  if GetTickCount64 - AStarted >= QWord(ADeadline) then
  begin
    raise ENyxBytes.Create('Whole resource request deadline expired before admission');
  end;
  LURL := UTF8Decode(AURL.Address);
  LParts := Default(URL_COMPONENTS);
  LParts.dwStructSize := SizeOf(LParts);
  LParts.dwHostNameLength := DWORD(-1);
  LParts.dwUrlPathLength := DWORD(-1);
  LParts.dwExtraInfoLength := DWORD(-1);

  if not WinHttpCrackUrl(PWideChar(LURL), Length(LURL), 0, @LParts) then
  begin
    raise ENyxBytes.Create('Native transport could not parse the hosted URL');
  end;
  SetString(LHost, LParts.lpszHostName, LParts.dwHostNameLength);
  SetString(LPath, LParts.lpszUrlPath, LParts.dwUrlPathLength);
  SetString(LExtra, LParts.lpszExtraInfo, LParts.dwExtraInfoLength);
  LPath := LPath + LExtra;
  LFragment := Pos('#', LPath);

  if LFragment > 0 then
  begin
    Delete(LPath, LFragment, MaxInt);
  end;

  if LPath = '' then
  begin
    LPath := '/';
  end;
  FSession := WinHttpOpen('Nyx resources', WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
    nil, nil, WINHTTP_FLAG_ASYNC);

  if FSession = nil then
  begin
    raise ENyxBytes.Create('Native resource session could not start');
  end;
  LProtocols := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 or CProtocolTLS13;

  if not WinHttpSetOption(FSession, WINHTTP_OPTION_SECURE_PROTOCOLS,
    @LProtocols, SizeOf(LProtocols)) then
  begin
    LProtocols := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;

    if not WinHttpSetOption(FSession, WINHTTP_OPTION_SECURE_PROTOCOLS,
      @LProtocols, SizeOf(LProtocols)) then
    begin
      raise ENyxBytes.Create('Native system cannot admit TLS 1.2 resource requests');
    end;
  end;
  WinHttpSetTimeouts(FSession, ADeadline, ADeadline, ADeadline, ADeadline);
  FConnection := WinHttpConnect(FSession, PWideChar(LHost), LParts.nPort, 0);

  if FConnection = nil then
  begin
    raise ENyxBytes.Create('Native resource connection could not start');
  end;
  LFlags := WINHTTP_FLAG_ESCAPE_DISABLE or WINHTTP_FLAG_ESCAPE_DISABLE_QUERY;

  if LParts.nScheme = INTERNET_SCHEME_HTTPS then
  begin
    LFlags := LFlags or WINHTTP_FLAG_SECURE;
  end;
  FRequest := WinHttpOpenRequest(FConnection, 'GET', PWideChar(LPath),
    nil, nil, nil, LFlags);

  if FRequest = nil then
  begin
    raise ENyxBytes.Create('Native resource request could not start');
  end;
  Option(WINHTTP_OPTION_REDIRECT_POLICY, WINHTTP_OPTION_REDIRECT_POLICY_NEVER);
  Option(WINHTTP_OPTION_DISABLE_FEATURE, WINHTTP_DISABLE_COOKIES or WINHTTP_DISABLE_AUTHENTICATION);
  Option(WINHTTP_OPTION_MAX_RESPONSE_HEADER_SIZE, 16384);
  Option(WINHTTP_OPTION_DECOMPRESSION, WINHTTP_DECOMPRESSION_FLAG_ALL);
  LContext := DWORD_PTR(Pointer(Self));

  if not WinHttpSetOption(FRequest, WINHTTP_OPTION_CONTEXT_VALUE, @LContext, SizeOf(LContext)) then
  begin
    raise ENyxBytes.Create('Native resource callback context could not start');
  end;
  LCallback := WinHttpSetStatusCallback(FRequest, @HTTPStatus,
    WINHTTP_CALLBACK_FLAG_SENDREQUEST_COMPLETE or WINHTTP_CALLBACK_FLAG_HEADERS_AVAILABLE or
    WINHTTP_CALLBACK_FLAG_READ_COMPLETE or WINHTTP_CALLBACK_FLAG_REQUEST_ERROR or
    WINHTTP_CALLBACK_FLAG_HANDLES, 0);

  if Pointer(@LCallback) = Pointer(-1) then
  begin
    raise ENyxBytes.Create('Native resource callback could not start');
  end;
  FLease := Self;

  if not WinHttpSendRequest(FRequest, nil, 0, nil, 0, 0, LContext) then
  begin
    raise ENyxBytes.Create('Native resource request could not send');
  end;
  Await(WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE, AExecution, AStarted, ADeadline);

  if not WinHttpReceiveResponse(FRequest, nil) then
  begin
    raise ENyxBytes.Create('Native resource response could not start');
  end;
  Await(WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE, AExecution, AStarted, ADeadline);
  LSize := SizeOf(LStatus);

  if not WinHttpQueryHeaders(FRequest, WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER,
    nil, @LStatus, @LSize, nil) then
  begin
    raise ENyxBytes.Create('Native resource response has no HTTP status');
  end;
  LResult.Status := LStatus;
  LResult.Hints := NyxResourceCacheHeaders(Header('cache-control'), Header('age'));
  SetLength(LBytes, AMaximum);
  LCount := 0;
  AProgress.RecordProgress(nrtReceiving, 0);
  repeat

    if not WinHttpReadData(FRequest, @FBuffer[0], SizeOf(FBuffer), nil) then
    begin
      raise ENyxBytes.Create('Native resource body could not read');
    end;
    Await(WINHTTP_CALLBACK_STATUS_READ_COMPLETE, AExecution, AStarted, ADeadline);

    if FCount = 0 then
    begin
      Break;
    end;

    if FCount > DWORD(AMaximum - LCount) then
    begin
      raise ENyxBytes.Create('Hosted reply exceeds the caller byte budget');
    end;
    Move(FBuffer[0], LBytes[LCount], FCount);
    Inc(LCount, FCount);
    AProgress.RecordProgress(nrtReceiving, LCount);
  until False;
  VerifyFraming(LCount);
  SetLength(LBytes, LCount);
  LResult.Bytes := LBytes;
  Result := LResult;
end;

constructor TNativeRequest.Create(AReply: TNyxResourceHTTPReply);
begin
  inherited Create(AReply);
  InitCriticalSection(FProgressLock);
  FProgressLockReady := True;
  FProgress := Default(TNyxResourceTransferProgress);
end;

function TNativeRequest.Progress: TNyxResourceTransferProgress;
begin
  FScheduler.RequireUI;
  EnterCriticalSection(FProgressLock);
  try
    Result := FProgress;
  finally
    LeaveCriticalSection(FProgressLock);
  end;
end;

procedure TNativeRequest.RecordProgress(APhase: TNyxResourceTransferPhase;
  ABytes: Integer);
begin
  EnterCriticalSection(FProgressLock);
  try

    if FProgress.Phase in [nrtDelivered, nrtCancelled] then
    begin
      Exit;
    end;
    FProgress.Phase := APhase;

    if ABytes >= 0 then
    begin
      FProgress.BytesReceived := ABytes;
    end;
  finally
    LeaveCriticalSection(FProgressLock);
  end;
end;

procedure TNativeRequest.StartDeadline(AStarted: QWord; AMilliseconds: Integer);
begin
  FScheduler.RequireUI;
  FStarted := AStarted;
  FDeadlineMS := AMilliseconds;
  FDeadlineLease := Self;
  FDeadlineTimer := TTimer.Create(nil);
  FDeadlineTimer.Enabled := False;
  FDeadlineTimer.Interval := AMilliseconds;
  FDeadlineTimer.OnTimer := Deadline;
  FDeadlineTimer.Enabled := True;
end;

procedure TNativeRequest.StopDeadline;
begin

  if FDeadlineTimer <> nil then
  begin
    FDeadlineTimer.Enabled := False;
    FDeadlineTimer.OnTimer := nil;
    FreeAndNil(FDeadlineTimer);
  end;
  FDeadlineLease := nil;
end;

destructor TNativeRequest.Destroy;
begin
  { Every normal terminal path already retired the timer on the UI thread.
    Construction refusal also disconnects it before releasing pending work. }
  StopDeadline;

  if FProgressLockReady then
  begin
    DoneCriticalSection(FProgressLock);
  end;
  inherited Destroy;
end;

procedure TNativeRequest.Deadline(Sender: TObject);
var
  LLease: INyxResourceRequest;
  LElapsed: QWord;
  LResult: TNyxResourceHTTPResult;
begin
  FScheduler.RequireUI;
  LLease := Self;

  if not Active or ((FExecution <> nil) and FExecution.Cancelled) then
  begin
    { Scheduler shutdown revokes callbacks too. Expiry must not turn a cancelled
      parent into a fresh delivery, even if its workers have already retired. }
    inherited Cancel;
    RecordProgress(nrtCancelled, -1);
    StopDeadline;
    Exit;
  end;
  LElapsed := GetTickCount64 - FStarted;

  if LElapsed < QWord(FDeadlineMS) then
  begin
    FDeadlineTimer.Interval := FDeadlineMS - Integer(LElapsed);
    Exit;
  end;

  if FExecution <> nil then
  begin
    FExecution.Cancel;
  end;
  StopDeadline;
  LResult := Default(TNyxResourceHTTPResult);
  LResult.Error := 'Whole resource request deadline expired';
  RecordProgress(nrtDelivered, -1);
  { Complete retires the borrowed receiver before invoking it. The local lease
    protects this operation if the receiver releases or cancels its own token. }
  Complete(LResult);
end;

procedure TNativeRequest.Cancel;
var
  LLease: INyxResourceRequest;
begin
  FScheduler.RequireUI;
  LLease := Self;
  inherited Cancel;
  RecordProgress(nrtCancelled, -1);

  if FExecution <> nil then
  begin
    FExecution.Cancel;
  end;
  StopDeadline;
end;

procedure TNativeRequest.Deliver(const AResult: TNyxResourceHTTPResult);
var
  LLease: INyxResourceRequest;
begin
  FScheduler.RequireUI;
  LLease := Self;

  if (GetTickCount64 - FStarted >= QWord(FDeadlineMS)) or
    ((FExecution <> nil) and FExecution.Cancelled) then
  begin
    { A completed HTTP reply may wait in the UI queue past the deadline.
      Arbitration uses the same monotonic origin as the worker and timer. }
    Deadline(nil);
    Exit;
  end;
  StopDeadline;
  RecordProgress(nrtDelivered, -1);
  Complete(AResult);
end;

procedure TResourceHTTPDelivery.Execute(const AExecution: INyxExecution);
begin

  if not AExecution.Cancelled then
  begin
    FRequest.Deliver(FResult);
  end;
end;

procedure TResourceHTTPWork.Execute(const AExecution: INyxExecution);
var
  LBridge: THTTPBridge;
  LGuard: IInterface;
  LDelivery: TResourceHTTPDelivery;
  LWork: INyxWork;
  LResult: TNyxResourceHTTPResult;
begin
  LBridge := THTTPBridge.Create;
  LGuard := LBridge;
  LResult := Default(TNyxResourceHTTPResult);
  try
    try
      LResult := LBridge.Fetch(FURL, FMaximum, FOptions.DeadlineMS, FStarted,
        AExecution, FRequest);
    except
      on LException: Exception do
      begin
        LResult.Error := LException.Message;
      end;
    end;
  finally
    LBridge.Close;
    LGuard := nil;
  end;
  FRequest.RecordProgress(nrtAwaitingReply, -1);

  if not AExecution.Cancelled then
  begin
    LDelivery := TResourceHTTPDelivery.Create;
    LWork := LDelivery;
    LDelivery.FRequest := FRequest;
    LDelivery.FLease := FLease;
    LDelivery.FResult := LResult;
    FScheduler.PostUI(LWork, AExecution);
  end;
end;

constructor TNativeTransport.Create(const AScheduler: INyxScheduler);
begin
  inherited Create;

  if AScheduler = nil then
  begin
    raise ENyxSchedule.Create('Native resource transport requires a scheduler');
  end;
  AScheduler.RequireUI;
  AScheduler.Admit(neThreaded);
  FScheduler := AScheduler;
end;

function TNativeTransport.Request(const AURL: TNyxResourceURL;
  const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
var
  LURL: TNyxResourceURL;
  LRequest: TNativeRequest;
  LWork: TResourceHTTPWork;
  LLease: INyxWork;
begin
  FScheduler.RequireUI;
  AOptions.Validate;
  LURL := NyxResourceURL(AURL.Address);

  if (AMaximumBytes < 1) or (AMaximumBytes > NyxMaximumPackedBytes) then
  begin
    raise ENyxBytes.Create('Hosted transport requires 1..1 MiB of payload budget');
  end;
  LRequest := TNativeRequest.Create(AReply);
  Result := LRequest;
  LRequest.FScheduler := FScheduler;
  LWork := TResourceHTTPWork.Create;
  LLease := LWork;
  LWork.FScheduler := FScheduler;
  LWork.FRequest := LRequest;
  LWork.FLease := Result;
  LWork.FURL := LURL;
  LWork.FOptions := AOptions;
  LWork.FMaximum := AMaximumBytes;
  LWork.FStarted := GetTickCount64;
  try
    LRequest.StartDeadline(LWork.FStarted, AOptions.DeadlineMS);
    LRequest.FExecution := FScheduler.Submit(LLease, neThreaded);
  except
    { Capacity/admission failure cannot leave a self-retaining timer armed. }
    LRequest.Cancel;
    raise;
  end;
end;
{$endif}

function NewNyxNativeResourceTransport(const AScheduler: INyxScheduler): INyxResourceTransport;
begin
  {$ifdef MSWINDOWS}
  Result := TNativeTransport.Create(AScheduler);
  {$else}
  raise ENyxSchedule.Create('This native target needs an INyxResourceTransport adapter');
  {$endif}
end;

end.
