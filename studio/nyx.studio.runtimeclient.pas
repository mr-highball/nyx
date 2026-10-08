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


unit nyx.studio.runtimeclient;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.editing, nyx.application.resources
  {$ifdef PAS2JS}, JS, Web{$else}, Classes, ExtCtrls{$endif};

type
  { Studio-only diagnostic producer. Captures resource state on the UI thread;
    transports immutable bytes with one in-flight report. Credentials are private
    launch context, never authored properties or generated source literals.
    Owner frees this before its application. Native disposal cancels/joins work;
    browser disposal aborts delivery. Server expiry covers abrupt process exits. }
  TNyxStudioRuntimeReporter = class
  private
    FResources: INyxApplicationResources;
    FEndpoint: TNyxText;
    FToken: TNyxText;
    FSequence: Integer;
    FPending: TNyxText;
    FPendingSnapshot: TNyxText;
    FLastSnapshot: TNyxText;
    FError: TNyxText;
    FStopped: Boolean;
    {$ifdef PAS2JS}
    FTimer: NativeInt;
    FRequest: TJSXMLHttpRequest;
    FUnload: TJSEventHandler;
    procedure Tick;
    procedure Received;
    {$else}
    FTimer: TTimer;
    FWorker: TThread;
    procedure Tick(ASender: TObject);
    {$endif}
    function Message(const AOperation: TNyxText): TNyxText;
    procedure Accept(AStatus: Integer; const AReply: TNyxText);
  public
    constructor Create(const AResources: INyxApplicationResources;
      const AConfiguration: TNyxDataValue);
    destructor Destroy; override;
    { Idempotent. Does not stop application loading. A best-effort retirement
      request is bounded separately; expiry retires an unreachable producer. }
    procedure Stop;
    { Generic bounded delivery diagnostic; empty after an accepted receipt.
      Does not expose credentials/paths or cancel application resource loads. }
    property LastError: TNyxText read FError;
  end;

{ No context means nil and no I/O/timer. Studio supplies a fragment in browsers
  or one child-process environment value natively. Ordinary exported builds do
  not carry reporting credentials. Invalid supplied context refuses explicitly. }
function ObserveNyxStudioResources(const AResources: INyxApplicationResources): TNyxStudioRuntimeReporter;

implementation

{$ifndef PAS2JS}
uses nyx.studio.transport, nyx.studio.transport.native, nyx.studio.exchange.lcl;

type
  TReportResponse = class(TMemoryStream)
  public
    function Write(const ABuffer; ACount: Longint): Longint; override;
  end;
  { Byte-only worker. Does not retain resources, reporter, document or widgets.
    Cancellation only revokes transport delivery, never server admission. }
  TReportWorker = class(TThread)
  private
    FEndpoint: TNyxText;
    FToken: TNyxText;
    FBody: TNyxText;
    FLifetime: TNyxHTTPRequestLifetime;
  protected
    procedure Execute; override;
  public
    Status: Integer;
    Reply: TNyxText;
    constructor Create(const AEndpoint, AToken, ABody: TNyxText; ADeadline: Integer = 5000);
    destructor Destroy; override;
    procedure Cancel;
  end;

function TReportResponse.Write(const ABuffer; ACount: Longint): Longint;
begin

  if (ACount < 0) or (Position > 1024 - ACount) then
  begin
    raise Exception.Create('Runtime acknowledgement exceeds one KiB');
  end;
  Result := inherited Write(ABuffer, ACount);
end;

constructor TReportWorker.Create(const AEndpoint, AToken, ABody: TNyxText; ADeadline: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FEndpoint := AEndpoint;
  FToken := AToken;
  FBody := ABody;
  FLifetime := TNyxHTTPRequestLifetime.Create(
    NewNyxTransportPolicy.WholeRequest(ADeadline).Snapshot);
end;

destructor TReportWorker.Destroy;
begin
  inherited Destroy;
  FLifetime.Free;
end;

procedure TReportWorker.Cancel;
begin
  FLifetime.Cancel;
  Terminate;
end;

procedure TReportWorker.Execute;
var
  LClient: TNyxDeadlineHTTPClient;
  LBody: TMemoryStream;
  LResponse: TReportResponse;
begin
  LClient := nil;
  LBody := nil;
  LResponse := nil;
  try
    try
      LClient := TNyxDeadlineHTTPClient.CreateFor(FLifetime);
      LBody := TMemoryStream.Create;
      LResponse := TReportResponse.Create;
      LBody.WriteBuffer(FBody[1], Length(FBody));
      LBody.Position := 0;
      LClient.RequestBody := LBody;
      LClient.AddHeader('Content-Type', 'application/json; charset=utf-8');
      LClient.AddHeader('X-Nyx-Runtime', FToken);
      LClient.HTTPMethod('POST', FEndpoint, LResponse, [200, 400, 403, 404, 409, 413, 500]);
      Status := LClient.ResponseStatusCode;
      SetLength(Reply, LResponse.Size);
      LResponse.Position := 0;

      if Reply <> '' then
      begin
        LResponse.ReadBuffer(Reply[1], Length(Reply));
      end;
    except
      on LException: Exception do
      begin
        Status := 0;
        Reply := '';
      end;
    end;
  finally
    LClient.Free;
    LResponse.Free;
    LBody.Free;
  end;
end;
{$endif}

function ObserveNyxStudioResources(const AResources: INyxApplicationResources): TNyxStudioRuntimeReporter;
var
  LText: TNyxText;
begin
  Result := nil;
  {$ifdef PAS2JS}
  LText := window.location.hash;

  if Copy(LText, 1, 13) <> '#nyx-runtime=' then
  begin
    Exit;
  end;
  LText := decodeURIComponent(Copy(LText, 14, Length(LText)));
  {$else}
  { Context contains only admitted ASCII machine origin and JSON capability.
    Read bytes without assigning arbitrary user resource text through ANSI RTL. }
  LText := TNyxText(RawByteString(GetEnvironmentVariable('NYX_STUDIO_RESOURCE_REPORT')));
  {$endif}

  if LText <> '' then
  begin
    Result := TNyxStudioRuntimeReporter.Create(AResources, TNyxDataValue.ParseJSON(LText));
  end;
end;

constructor TNyxStudioRuntimeReporter.Create(const AResources: INyxApplicationResources;
  const AConfiguration: TNyxDataValue);
var
  LIndex: Integer;
  LInterval: Integer;
  {$ifndef PAS2JS}
  LOrigin: TNyxText;
  {$endif}
begin
  inherited Create;

  if (AResources = nil) or (AConfiguration.Kind <> ndObject) or
    (AConfiguration.Field('version').AsInteger <> 1) or
    (AConfiguration.Field('endpoint').AsText <> '/api/resource-runtime') then
  begin
    raise Exception.Create('Runtime reporter requires exact private launch context');
  end;
  FToken := AConfiguration.Field('token').AsText;

  if Length(FToken) <> 76 then
  begin
    raise Exception.Create('Runtime reporter capability is invalid');
  end;
  for LIndex := 1 to Length(FToken) do
  begin

    if not (FToken[LIndex] in ['0'..'9', 'a'..'f', 'A'..'F', '{', '}', '-']) then
    begin
      raise Exception.Create('Runtime reporter capability is invalid');
    end;
  end;
  LInterval := AConfiguration.Field('intervalMilliseconds').AsInteger;

  if (NyxTextScalarCount(AConfiguration.Field('run').AsText) < 1) or
    (NyxTextScalarCount(AConfiguration.Field('run').AsText) > 80) then
  begin
    raise Exception.Create('Runtime reporter requires its exact run identity');
  end;

  if (LInterval < 250) or (LInterval > 15000) then
  begin
    raise Exception.Create('Runtime reporter interval requires 250..15000 milliseconds');
  end;
  {$ifdef PAS2JS}

  if AConfiguration.Count <> 5 then
  begin
    raise Exception.Create('Browser runtime context has unexpected fields');
  end;
  FEndpoint := AConfiguration.Field('endpoint').AsText;
  {$else}

  if AConfiguration.Count <> 6 then
  begin
    raise Exception.Create('Native runtime context requires its local service origin');
  end;
  LOrigin := AConfiguration.Field('origin').AsText;
  ValidateNyxLocalStudioOrigin(LOrigin);
  FEndpoint := LOrigin + AConfiguration.Field('endpoint').AsText;
  {$endif}
  { Optional diagnostics admission precedes timer/transport allocation. }
  NyxApplicationResourceDiagnostics(AResources);
  FResources := AResources;
  {$ifdef PAS2JS}
  FUnload := function(AEvent: TJSEvent): Boolean
    begin
      Stop;
      Result := True;
    end;
  window.addEventListener('pagehide', FUnload);
  FTimer := window.setInterval(@Tick, LInterval);
  Tick;
  {$else}
  FTimer := TTimer.Create(nil);
  FTimer.Interval := LInterval;
  FTimer.OnTimer := Tick;
  FTimer.Enabled := True;
  Tick(nil);
  {$endif}
end;

function TNyxStudioRuntimeReporter.Message(const AOperation: TNyxText): TNyxText;
var
  LFields: array of TNyxDataField;
  LSnapshot: TNyxDataValue;
begin

  if FSequence = High(Integer) then
  begin
    raise Exception.Create('Runtime reporter sequence exhausted');
  end;
  SetLength(LFields, 3);
  LFields[0] := NyxField('version', NyxData(1));
  LFields[1] := NyxField('operation', NyxData(AOperation));
  LFields[2] := NyxField('sequence', NyxData(FSequence + 1));

  if AOperation = 'publish' then
  begin
    LSnapshot := EncodeNyxResourceRuntime(
      NyxApplicationResourceDiagnostics(FResources).CaptureRuntime);
    FPendingSnapshot := LSnapshot.ToJSON;

    if (FSequence > 0) and (FPendingSnapshot = FLastSnapshot) then
    begin
      LFields[1] := NyxField('operation', NyxData('heartbeat'));
    end
    else
    begin
      SetLength(LFields, 4);
      LFields[3] := NyxField('snapshot', LSnapshot);
    end;
  end;
  Result := NyxObject(LFields).ToJSON;
end;

procedure TNyxStudioRuntimeReporter.Accept(AStatus: Integer; const AReply: TNyxText);
var
  LReply: TNyxDataValue;
begin

  if AStatus = 200 then
  begin
    LReply := TNyxDataValue.ParseJSON(AReply);

    if (LReply.Kind <> ndObject) or (LReply.Count <> 2) or
      (LReply.Field('sequence').AsInteger <> FSequence + 1) then
    begin
      raise Exception.Create('Runtime acknowledgement does not match the pending sequence');
    end;
    Inc(FSequence);
    FLastSnapshot := FPendingSnapshot;
    FPendingSnapshot := '';
    FPending := '';
    FError := '';

    if not LReply.Field('active').AsBoolean then
    begin
      FStopped := True;
    end;
  end
  else
  begin
    FError := 'Runtime reporting is unavailable; the application remains usable';

    if (AStatus >= 400) and (AStatus < 500) then
    begin
      FStopped := True;
    end;
  end;
end;

{$ifdef PAS2JS}
procedure TNyxStudioRuntimeReporter.Received;
var
  LRequest: TJSXMLHttpRequest;
begin

  if (FRequest = nil) or (FRequest.readyState <> 4) then
  begin
    Exit;
  end;
  LRequest := FRequest;
  FRequest := nil;
  try
    Accept(LRequest.status, LRequest.responseText);
  except
    on LException: Exception do
    begin
      FError := 'Runtime reporting response was invalid; the application remains usable';
      FStopped := True;
    end;
  end;
end;

procedure TNyxStudioRuntimeReporter.Tick;
begin

  if FStopped or (FRequest <> nil) then
  begin
    Exit;
  end;
  try

    if FPending = '' then
    begin
      FPending := Message('publish');
    end;
    FRequest := TJSXMLHttpRequest.new;
    FRequest.open('POST', FEndpoint, True);
    FRequest.timeout := 5000;
    FRequest.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
    FRequest.setRequestHeader('X-Nyx-Runtime', FToken);
    FRequest.onreadystatechange := @Received;
    FRequest.send(FPending);
  except
    on LException: Exception do
    begin
      FError := 'Runtime reporting failed; the application remains usable';
      FStopped := True;
    end;
  end;
end;
{$else}
procedure TNyxStudioRuntimeReporter.Tick(ASender: TObject);
var
  LWorker: TReportWorker;
begin

  if FWorker <> nil then
  begin

    if not FWorker.Finished then
    begin
      Exit;
    end;
    LWorker := TReportWorker(FWorker);
    FWorker := nil;
    try
      LWorker.WaitFor;
      try
        Accept(LWorker.Status, LWorker.Reply);
      except
        on LException: Exception do
        begin
          FError := 'Runtime reporting response was invalid; the application remains usable';
          FStopped := True;
        end;
      end;
    finally
      LWorker.Free;
    end;
  end;

  if FStopped then
  begin
    Exit;
  end;
  try

    if FPending = '' then
    begin
      FPending := Message('publish');
    end;
    FWorker := TReportWorker.Create(FEndpoint, FToken, FPending);
    FWorker.Start;
  except
    on LException: Exception do
    begin
      FError := 'Runtime reporting failed; the application remains usable';
      FStopped := True;
    end;
  end;
end;
{$endif}

procedure TNyxStudioRuntimeReporter.Stop;
var
  {$ifdef PAS2JS}
  LRetirement: TJSXMLHttpRequest;
  {$else}
  LRetirement: TReportWorker;
  {$endif}
begin
  {$ifdef PAS2JS}
  window.clearInterval(FTimer);

  if FRequest <> nil then
  begin
    FRequest.onreadystatechange := nil;
    FRequest.abort;
    FRequest := nil;
  end;
  { Pagehide delivery is best effort. The unretained request has no callback or
    owner; abrupt browser exits rely on broker expiry. }

  if not FStopped and (FResources <> nil) and (FSequence > 0) then
  begin
    try
      LRetirement := TJSXMLHttpRequest.new;
      LRetirement.open('POST', FEndpoint, True);
      LRetirement.timeout := 1000;
      LRetirement.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
      LRetirement.setRequestHeader('X-Nyx-Runtime', FToken);
      LRetirement.send(Message('retire'));
    except
      on LException: Exception do
      begin
        { Retirement remains bounded by server expiry after unavailable delivery. }
      end;
    end;
  end;
  {$else}

  if FTimer <> nil then
  begin
    FTimer.Enabled := False;
  end;

  if FWorker <> nil then
  begin
    TReportWorker(FWorker).Cancel;
    FWorker.WaitFor;
    FreeAndNil(FWorker);
  end;

  if not FStopped and (FResources <> nil) and (FSequence > 0) then
  begin
    LRetirement := TReportWorker.Create(FEndpoint, FToken, Message('retire'), 1000);
    try
      LRetirement.Start;
      LRetirement.WaitFor;
    finally
      LRetirement.Free;
    end;
  end;
  {$endif}
  FStopped := True;
end;

destructor TNyxStudioRuntimeReporter.Destroy;
begin
  Stop;
  {$ifdef PAS2JS}

  if FUnload <> nil then
  begin
    window.removeEventListener('pagehide', FUnload);
  end;
  {$else}
  FTimer.Free;
  {$endif}
  FResources := nil;
  inherited Destroy;
end;

end.
