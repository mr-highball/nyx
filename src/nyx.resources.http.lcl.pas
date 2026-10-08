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
  UI callbacks are posted with the parent cancellation token. No worker touches
  a control, resource catalog or borrowed receiver. Other native systems require
  another INyxResourceTransport implementation; they are not claimed qualified. }
function NewNyxNativeResourceTransport(const AScheduler: INyxScheduler): INyxResourceTransport;

implementation

uses SysUtils, nyx.text, nyx.bytes, nyx.resource.sources, nyx.resource.cache
  {$ifdef MSWINDOWS}, Windows, WinHTTP, SyncObjs{$endif};

{$ifdef MSWINDOWS}
const
  { Older FPC headers omit the newer Windows TLS 1.3 flag. Unsupported hosts
    fall back to TLS 1.2, retaining ordinary system certificate verification. }
  CProtocolTLS13 = $2000;

type
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
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close;
    function Fetch(const AURL: TNyxResourceURL; AMaximum, ADeadline: Integer;
      AStarted: QWord; const AExecution: INyxExecution): TNyxResourceHTTPResult;
  end;
  TNativeRequest = class(TNyxResourceRequest)
  private
    FScheduler: INyxScheduler;
    FExecution: INyxExecution;
  public
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

function THTTPBridge.Fetch(const AURL: TNyxResourceURL;
  AMaximum, ADeadline: Integer; AStarted: QWord;
  const AExecution: INyxExecution): TNyxResourceHTTPResult;
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
  until False;
  SetLength(LBytes, LCount);
  LResult.Bytes := LBytes;
  Result := LResult;
end;

procedure TNativeRequest.Cancel;
begin
  inherited Cancel;

  if FExecution <> nil then
  begin
    FExecution.Cancel;
  end;
end;

procedure TNativeRequest.Deliver(const AResult: TNyxResourceHTTPResult);
begin
  FScheduler.RequireUI;
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
      LResult := LBridge.Fetch(FURL, FMaximum, FOptions.DeadlineMS, FStarted, AExecution);
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
  LRequest.FExecution := FScheduler.Submit(LLease, neThreaded);
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
