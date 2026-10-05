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

unit nyx.studio.exchange.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, ExtCtrls, nyx.text, nyx.studio.exchange, nyx.studio.transport;

type
  { Native private editor adapter. One HTTP worker owns bytes only; UI timers
    deliver completion after joining a finished worker. No widget, session or
    callback receiver is accessed by that worker. Cancellation detaches delivery
    immediately; a canceled worker may finish server admission independently.
    A subsequent request waits behind that canceled worker without blocking UI.
    Destruction joins in-flight work before releasing timers. A monotonic whole-
    request deadline includes partial headers/body and upload. Socket readiness
    polls cancellation without borrowing the UI or extending that deadline.
    The explicit loopback HTTP origin remains machine configuration, outside
    portable documents. No redirect, TLS credential or MCP token is introduced. }
  TNyxLCLEditorExchange = class(TNyxStudioEditorExchange)
  private
    FBaseURL: TNyxText;
    FLimits: TNyxTransportLimits;
    FWorker: TThread;
    FReply: TNyxEditorReply;
    FPoll: TTimer;
    FTickTimer: TTimer;
    FTick: TNyxEditorTick;
    FDeferred: Boolean;
    FDeferredConnect: Boolean;
    FDeferredToken: TNyxText;
    FDeferredBody: TNyxText;
    FDeferredReply: TNyxEditorReply;
    FDeferredStartedTick: QWord;
    procedure PostQueued(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply; AStartedTick: QWord);
    procedure Poll(ASender: TObject);
    procedure Tick(ASender: TObject);
  public
    { Accept an explicit http://127.0.0.1:port origin. A remote/relative URL,
      credentials, path, query or fragment refuses before creating network work. }
    constructor Create(const ABaseURL: TNyxText;
      const APolicy: INyxTransportPolicy = nil);
    destructor Destroy; override;
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
  end;

{ Shared native service-origin admission for private exchanges and compiler
  artifacts. No credentials, redirects or paths enter this machine reference. }
procedure ValidateNyxLocalStudioOrigin(const ABaseURL: TNyxText);

implementation

uses
  SysUtils, nyx.data, nyx.studio.transport.native;

const
  CEditorByteBudget = 16 * 1024 * 1024;

type
  { Bounded response bytes. Avoid unbounded memory growth before JSON admission. }
  TEditorResponseBytes = class(TMemoryStream)
  public
    function Write(const ABuffer; ACount: Longint): Longint; override;
  end;

  TEditorHTTPWorker = class(TThread)
  private
    FBase: TNyxText;
    FToken: TNyxText;
    FBody: TNyxText;
    FConnect: Boolean;
    FLifetime: TNyxHTTPRequestLifetime;
  protected
    procedure Execute; override;
  public
    Status: Integer;
    Text: TNyxText;
    constructor Create(const ABase, AToken, ABody: TNyxText; AConnect: Boolean;
      const ALimits: TNyxTransportLimits; AStartedTick: QWord);
    destructor Destroy; override;
    procedure Cancel;
  end;

function TEditorResponseBytes.Write(const ABuffer; ACount: Longint): Longint;
begin

  if (ACount < 0) or (Position > CEditorByteBudget - ACount) then
  begin
    raise Exception.Create('Editor response exceeds the byte budget');
  end;
  Result := inherited Write(ABuffer, ACount);
end;

constructor TEditorHTTPWorker.Create(const ABase, AToken, ABody: TNyxText;
  AConnect: Boolean; const ALimits: TNyxTransportLimits; AStartedTick: QWord);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FBase := ABase;
  FToken := AToken;
  FBody := ABody;
  FConnect := AConnect;
  FLifetime := TNyxHTTPRequestLifetime.Create(ALimits, AStartedTick);
end;

destructor TEditorHTTPWorker.Destroy;
begin
  { Owner joins before destruction; no socket/client borrows this after Execute. }
  inherited Destroy;
  FLifetime.Free;
end;

procedure TEditorHTTPWorker.Cancel;
begin
  FLifetime.Cancel;
  Terminate;
end;

procedure TEditorHTTPWorker.Execute;
var
  LClient: TNyxDeadlineHTTPClient;
  LBody: TMemoryStream;
  LReply: TEditorResponseBytes;
  LPath: TNyxText;
begin
  LClient := nil;
  LBody := nil;
  LReply := nil;
  try
    try
      FLifetime.Check;
      LClient := TNyxDeadlineHTTPClient.CreateFor(FLifetime);
      LBody := TMemoryStream.Create;
      LReply := TEditorResponseBytes.Create;
      LClient.AddHeader('Content-Type', 'application/json; charset=utf-8');
      LClient.AddHeader('Origin', FBase);

      if not FConnect then
      begin
        LClient.AddHeader('X-Nyx-Editor', FToken);
      end;

      if FBody <> '' then
      begin
        LBody.WriteBuffer(FBody[1], Length(FBody));
      end;
      LBody.Position := 0;
      LClient.RequestBody := LBody;
      LPath := '/api/agents';

      if FConnect then
      begin
        LPath := '/api/agents/connect';
      end;
      LClient.HTTPMethod('POST', FBase + LPath, LReply, []);
      FLifetime.Check;
      Status := LClient.ResponseStatusCode;
      SetLength(Text, LReply.Size);
      LReply.Position := 0;

      if Text <> '' then
      begin
        LReply.ReadBuffer(Text[1], Length(Text));
      end;
    except
      on LException: Exception do
      begin
        Status := 0;
        { Do not publish URLs, capabilities or raw HTTP exceptions in chrome. }
        Text := NyxObject([NyxField('error',
          NyxData('Native editor connection failed; local work is retained'))]).ToJSON;
      end;
    end;
  finally

    if LClient <> nil then
    begin
      LClient.RequestBody := nil;
    end;
    LClient.Free;
    LBody.Free;
    LReply.Free;
  end;
end;

procedure ValidateNyxLocalStudioOrigin(const ABaseURL: TNyxText);
var
  LPort: Integer;
begin

  if (Copy(ABaseURL, 1, 17) <> 'http://127.0.0.1:') or
    not TryStrToInt(Copy(ABaseURL, 18, MaxInt), LPort) or
    (LPort < 1) or (LPort > 65535) or
    (ABaseURL <> 'http://127.0.0.1:' + IntToStr(LPort)) then
  begin
    raise Exception.Create('Native editor connection requires an explicit loopback HTTP origin');
  end;
end;

constructor TNyxLCLEditorExchange.Create(const ABaseURL: TNyxText;
  const APolicy: INyxTransportPolicy);
begin
  inherited Create;
  ValidateNyxLocalStudioOrigin(ABaseURL);
  FBaseURL := ABaseURL;
  FLimits := NewNyxTransportPolicy.Snapshot;

  if APolicy <> nil then
  begin
    FLimits := APolicy.Snapshot;
  end;
  ValidateNyxTransportLimits(FLimits);
  FPoll := TTimer.Create(nil);
  FPoll.Enabled := False;
  FPoll.Interval := 20;
  FPoll.OnTimer := Poll;
  FTickTimer := TTimer.Create(nil);
  FTickTimer.Enabled := False;
  FTickTimer.OnTimer := Tick;
end;

destructor TNyxLCLEditorExchange.Destroy;
begin
  CancelTick;
  CancelRequest;
  FPoll.Free;

  if FWorker <> nil then
  begin
    FWorker.WaitFor;
    FWorker.Free;
  end;
  FTickTimer.Free;
  inherited Destroy;
end;

procedure TNyxLCLEditorExchange.CancelRequest;
begin
  FReply := nil;
  FDeferred := False;
  FDeferredReply := nil;
  FDeferredToken := '';
  FDeferredBody := '';
  FDeferredStartedTick := 0;

  if FWorker <> nil then
  begin
    TEditorHTTPWorker(FWorker).Cancel;
  end;
end;

procedure TNyxLCLEditorExchange.Post(AConnect: Boolean;
  const AToken, ABody: TNyxText; AReply: TNyxEditorReply);
begin
  PostQueued(AConnect, AToken, ABody, AReply, GetTickCount64);
end;

procedure TNyxLCLEditorExchange.PostQueued(AConnect: Boolean;
  const AToken, ABody: TNyxText; AReply: TNyxEditorReply; AStartedTick: QWord);
begin

  if Length(ABody) > CEditorByteBudget then
  begin
    raise Exception.Create('Editor request exceeds the byte budget');
  end;

  if FWorker <> nil then
  begin

    if Assigned(FReply) or FDeferred then
    begin
      raise Exception.Create('An editor request is already pending');
    end;
    FDeferred := True;
    FDeferredConnect := AConnect;
    FDeferredToken := AToken;
    FDeferredBody := ABody;
    FDeferredReply := AReply;
    FDeferredStartedTick := AStartedTick;
    Exit;
  end;
  FReply := AReply;
  FWorker := TEditorHTTPWorker.Create(FBaseURL, AToken, ABody, AConnect, FLimits, AStartedTick);
  try
    FWorker.Start;
  except
    FReply := nil;
    FreeAndNil(FWorker);
    raise;
  end;
  FPoll.Enabled := True;
end;

procedure TNyxLCLEditorExchange.Poll(ASender: TObject);
var
  LWorker: TEditorHTTPWorker;
  LReply: TNyxEditorReply;
  LStatus: Integer;
  LText: TNyxText;
  LConnect: Boolean;
  LToken: TNyxText;
  LBody: TNyxText;
  LStartedTick: QWord;
begin

  if (FWorker = nil) or not FWorker.Finished then
  begin
    Exit;
  end;
  FPoll.Enabled := False;
  LWorker := TEditorHTTPWorker(FWorker);
  LWorker.WaitFor;
  FWorker := nil;
  LStatus := LWorker.Status;
  LText := LWorker.Text;
  LWorker.Free;
  LReply := FReply;
  FReply := nil;

  if FDeferred then
  begin
    LConnect := FDeferredConnect;
    LToken := FDeferredToken;
    LBody := FDeferredBody;
    LReply := FDeferredReply;
    LStartedTick := FDeferredStartedTick;
    FDeferred := False;
    FDeferredReply := nil;
    FDeferredToken := '';
    FDeferredBody := '';
    FDeferredStartedTick := 0;
    PostQueued(LConnect, LToken, LBody, LReply, LStartedTick);
  end
  else if Assigned(LReply) then
  begin
    { There is no access to Self after delivery: an owner can retire the adapter
      from its callback without leaving a second notification or borrowed worker. }
    LReply(LStatus, LText);
  end;
end;

procedure TNyxLCLEditorExchange.CancelTick;
begin

  if FTickTimer <> nil then
  begin
    FTickTimer.Enabled := False;
  end;
  FTick := nil;
end;

procedure TNyxLCLEditorExchange.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin
  CancelTick;
  FTick := ATick;
  FTickTimer.Interval := ADelayMS;
  FTickTimer.Enabled := True;
end;

procedure TNyxLCLEditorExchange.Tick(ASender: TObject);
var
  LTick: TNyxEditorTick;
begin
  FTickTimer.Enabled := False;
  LTick := FTick;
  FTick := nil;

  if Assigned(LTick) then
  begin
    LTick;
  end;
end;

end.
