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

program nyx_http_logic_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.resources.loader,
  nyx.resource.sources, nyx.http.types, nyx.logic.http, nyx.logic.collections,
  nyx.logic, nyx.flow, nyx.flow.types, nyx.flow.execution, nyx.scheduler,
  nyx.linq, nyx.linq.execution
  {$ifdef PAS2JS}, JS, Web, nyx.resources.http.browser
  {$else}, Interfaces, Forms, nyx.resources.http.lcl{$endif};

type
  TPort = class(TInterfacedObject, INyxQueryResultPort<INyxFlowResult>)
  public
    Calls: Integer;
    Last: INyxFlowResult;
    function Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
  end;
  { One fixture owner services real UI turns. Its borrowed reply is disconnected
    before disposal; no test callback outlives its request token or scheduler. }
  TJourney = class
  private
    FScheduler: INyxScheduler;
    FTransport: INyxHTTPTransport;
    FToken: INyxResourceRequest;
    FReply: TNyxResourceHTTPResult;
    FCalls: Integer;
    FStage: Integer;
    FWaiting: Boolean;
    FStageAt: Double;
    FRun: INyxQueryRun;
    FPort: TPort;
    FPortLease: INyxQueryResultPort<INyxFlowResult>;
    FGraph: INyxFlowGraph;
    FScope: INyxCancellationScope;
    FStarted: Double;
    FChecks: Integer;
    FDone: Boolean;
    FBase: TNyxText;
    procedure Check(ACondition: Boolean; const AMessage: TNyxText);
    procedure Shared;
    procedure Loaded(const AResult: TNyxResourceHTTPResult);
    procedure BeginGraph(const APath: TNyxText; AAuthority: Boolean = True);
    procedure Step;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Drive;
    property Done: Boolean read FDone;
  end;

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

function TPort.Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
begin

  if Length(AValues) <> 1 then
  begin
    raise Exception.Create('Expected one complete graph result');
  end;
  Inc(Calls);
  Last := AValues[0];
  Result := True;
end;

procedure TJourney.Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(FChecks);
end;

procedure TJourney.Shared;
var
  LRequest: TNyxHTTPRequest;
  LCopy: TNyxHTTPRequest;
  LBytes: TNyxBytes;
  LHeaders: TNyxHTTPHeaders;
  LFailed: Boolean;
  LIndex: Integer;
begin
  LBytes := NyxEncodeUTF8('Hello 🌟' + #0 + ' exact');
  LRequest := NyxHTTPRequest(NyxResourceURL(FBase + '/echo')).Method(nhmPost)
    .WithBody(LBytes).Header('X-Nyx-Input', 'first');
  LCopy := LRequest.Header('X-Nyx-Input', 'second');
  LBytes[0] := 0;
  Check((LRequest.Headers.First('x-nyx-input') = 'first') and
    (LCopy.Headers.First('x-nyx-input') = 'second'), 'request headers are immutable');
  Check(NyxDecodeUTF8(LRequest.Body) = TNyxText('Hello 🌟') + #0 +
    TNyxText(' exact'), 'request body is detached');
  Check(TNyxHTTPRequest.FromData(LRequest.ToData).ToData.ToJSON = LRequest.ToData.ToJSON,
    'request persistence is exact');
  LHeaders := NyxHTTPHeaders.Append('X-Value', 'a').Append('x-value', 'b');
  Check((LHeaders.Count = 2) and (LHeaders.SetValue('X-Value', 'c').Count = 1),
    'response duplicate fields retain order; replacement removes every occurrence');
  { Latin-1 uses one protocol byte per scalar on both targets, independently of
    UTF-8 native storage. Exactly 8192 admitted value bytes pass. }
  LHeaders := NyxHTTPHeaders.Append('x', TNyxText(StringOfChar('a', 8191)) + TNyxText('é'));
  Check(LHeaders.Count = 1, 'portable header limit counts Latin-1 bytes');
  for LIndex := 0 to 7 do
  begin
    LFailed := False;
    try
      case LIndex of
        0: LCopy := NyxHTTPRequest(NyxResourceURL(FBase + '/echo')).WithBody(LRequest.Body);
        1: LCopy := LRequest.Method(nhmHead);
        2: LCopy := LRequest.Header('Host', 'elsewhere.invalid');
        3: LCopy := LRequest.Header('x', 'value' + #13#10 + 'Injected: yes');
        4: LCopy := LRequest.Method(NyxHTTPMethod('TRACE'));
        5: LCopy := LRequest.Header('x', '🌟');
        6: LCopy := LRequest.WholeRequest(0);
        7: NewNyxUTF8Decode.Decode(NyxDecodeBase64('/w=='));
      end;
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'invalid request/encoding refuses before host entry ' + IntToStr(LIndex));
  end;
end;

procedure TJourney.Loaded(const AResult: TNyxResourceHTTPResult);
begin
  FScheduler.RequireUI;
  Inc(FCalls);
  FReply := AResult;
  FWaiting := False;
end;

constructor TJourney.Create;
begin
  inherited Create;
  {$ifdef PAS2JS}
  FBase := window.location.origin;
  {$else}

  if ParamCount <> 1 then
  begin
    raise Exception.Create('Supply the owned loopback fixture origin');
  end;
  FBase := TNyxText(ParamStr(1));
  {$endif}
  FStarted := Clock;
  FScheduler := NewNyxScheduler;
  {$ifdef PAS2JS}
  FTransport := NewNyxBrowserHTTPTransport;
  {$else}
  FTransport := NewNyxNativeHTTPTransport(FScheduler);
  {$endif}
  Shared;
end;

procedure TJourney.BeginGraph(const APath: TNyxText; AAuthority: Boolean);
var
  LTransport: INyxHTTPTransport;
begin
  FPort := TPort.Create;
  FPortLease := FPort;
  FScope := NewNyxCancellationScope;
  LTransport := nil;

  if AAuthority then
  begin
    LTransport := FTransport;
  end;
  FGraph := NewNyxFlowGraph
    .WithStep(NyxFlowStep('request'), NewNyxHTTPCall(
      NyxHTTPRequest(NyxResourceURL(FBase + APath)), LTransport))
    .WithStep(NyxFlowStep('decode'), NewNyxUTF8Decode)
    .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
    .Connect(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpBody)),
      NyxFlowEndpoint(NyxFlowStep('decode'), NyxHTTPPort(nhpBody)))
    .Connect(NyxFlowEndpoint(NyxFlowStep('decode'), NyxLogicPort(nlpText)),
      NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpText))).Build;

  if (APath = '/json') and AAuthority then
  begin
    FGraph := NyxFlowGraphFromData(FGraph.ToData, NewNyxHTTPLogicResolver(FTransport));
    Check(FGraph.ToData.ToJSON = NyxFlowGraphFromData(FGraph.ToData,
      NewNyxHTTPLogicResolver(nil)).ToData.ToJSON, 'saved graph carries no injected authority');
  end;
  FRun := TNyxFlowExecution.Submit(FGraph,
    [NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpTrigger)),
      NyxFlowSignal)], FScheduler, FPortLease, neAsynchronous, FScope);
end;

procedure TJourney.Step;
var
  LRequest: TNyxHTTPRequest;
  LMethod: TNyxHTTPMethodRef;
  LExpected: TNyxText;
  LBytes: TNyxBytes;
  LJSON: TNyxDataValue;
begin

  if Clock - FStarted > 45000 then
  begin
    raise Exception.Create('HTTP Logic deadline at stage ' + IntToStr(FStage));
  end;

  if FStage >= 15 then
  begin

    if not FWaiting then
    begin
      case FStage of
        15: BeginGraph('/json');
        16: BeginGraph('/json', False);
        17: BeginGraph('/slow');
        18:
        begin
          FDone := True;
          Exit;
        end;
      end;
      FWaiting := True;
      FStageAt := Clock;
      Exit;
    end;

    if (FStage = 17) and (Clock - FStageAt >= 30) then
    begin
      FScope.Cancel;
    end;

    if FRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
    begin
      Exit;
    end;

    if FStage = 15 then
    begin
      Check((FRun.Phase = nqpSucceeded) and (FPort.Calls = 1),
        'real HTTP graph resumes pure native/browser operations exactly once: ' + FRun.Failure);
      LJSON := FPort.Last.Value(NyxFlowEndpoint(NyxFlowStep('parse'),
        NyxLogicPort(nlpJSON))).AsJSON;
      Check((LJSON.Field('caption').AsText = 'Hello from HTTP') and
        (LJSON.Field('items').Item(0).Field('rank').ToJSON = '9007199254740993'),
        'HTTP bytes decode and parse with exact numbers');
    end;

    if FStage = 16 then
    begin
      Check((FRun.Phase = nqpFailed) and (FPort.Calls = 0) and
        (Pos('explicit transport', FRun.Failure) > 0), 'data-only HTTP declaration grants no authority');
    end;

    if FStage = 17 then
    begin

      if Clock - FStageAt < 650 then
      begin
        Exit;
      end;
      Check((FRun.Phase = nqpCancelled) and (FPort.Calls = 0),
        'generation retirement cancels real HTTP without late publication');
    end;
    FRun := nil;
    FGraph := nil;
    FPortLease := nil;
    FScope := nil;
    FWaiting := False;
    Inc(FStage);
    Exit;
  end;

  if not FWaiting and (FCalls = 0) then
  begin
    LRequest := NyxHTTPRequest(NyxResourceURL(FBase + '/echo')).Header('X-Nyx-Input', 'owned');
    case FStage of
      0..6: LRequest := LRequest.Method(TNyxHTTPMethod(FStage));
      7: LRequest := LRequest.Method(NyxHTTPMethod('REPORT'));
      8: LRequest := NyxHTTPRequest(NyxResourceURL(FBase + '/status'));
      9: LRequest := NyxHTTPRequest(NyxResourceURL(FBase + '/head')).Method(nhmHead);
      10: LRequest := NyxHTTPRequest(NyxResourceURL(FBase + '/slow')).WholeRequest(40);
      11: LRequest := NyxHTTPRequest(NyxResourceURL(FBase + '/json')).MaximumBytes(8);
      12: LRequest := NyxHTTPRequest(NyxResourceURL(FBase + '/slow'));
      13: LRequest := NyxHTTPRequest(NyxResourceURL(FBase + '/json'));
      14: LRequest := NyxHTTPRequest(NyxResourceURL(FBase + '/status'));
    end;

    if FStage in [2, 3, 4, 5, 6, 7] then
    begin
      LRequest := LRequest.WithBody(NyxEncodeUTF8('Exact 🌟' + #0 + ' bytes'));
    end;
    FWaiting := True;
    FStageAt := Clock;
    FToken := FTransport.Send(LRequest, Loaded);
    Exit;
  end;

  if FStage = 12 then
  begin

    if Clock - FStageAt >= 30 then
    begin
      FToken.Cancel;
    end;

    if Clock - FStageAt < 650 then
    begin
      Exit;
    end;
    Check(FCalls = 0, 'cancelled transport cannot call retired receiver');
    FWaiting := False;
  end;

  if FWaiting then
  begin
    Exit;
  end;

  if FStage <> 12 then
  begin
    Check(FCalls = 1, 'transport delivers exactly once');

    if FStage <= 7 then
    begin

      if FStage <= 6 then
      begin
        LMethod := NyxHTTPMethod(TNyxHTTPMethod(FStage));
      end
      else
      begin
        LMethod := NyxHTTPMethod('REPORT');
      end;
      LExpected := LMethod.Name + #10 + 'owned' + #10;

      if FStage in [2, 3, 4, 5, 6, 7] then
      begin
        LExpected := LExpected + TNyxText('Exact 🌟') + #0 + TNyxText(' bytes');
      end;

      if FStage = 1 then
      begin
        LExpected := '';
      end;
      LBytes := FReply.Bytes;
      Check((FReply.Error = '') and (FReply.Status = 200) and
        (NyxDecodeUTF8(LBytes) = LExpected), 'real method/header/body transport ' + LMethod.Name +
        ': ' + FReply.Error);
      Check(FReply.Headers.First('x-nyx-reply') = 'owned-http-fixture',
        'observable response headers survive');

      {$ifdef PAS2JS}Check(FReply.HeaderVisibility = nhvBrowserFiltered, 'browser header visibility');
      {$else}Check(FReply.HeaderVisibility = nhvNative, 'native header visibility');{$endif}
    end;

    if FStage in [8, 14] then
    begin
      Check((FReply.Error = '') and (FReply.Status = 422) and
        (NyxDecodeUTF8(FReply.Bytes) = 'Please check the input.'),
        'non-2xx response retains status and useful body');
    end;

    if FStage = 9 then
    begin
      Check((FReply.Error = '') and (FReply.Status = 200) and (Length(FReply.Bytes) = 0),
        'HEAD has no body despite representation Content-Length');
    end;

    if FStage in [10, 11] then
    begin
      Check((FReply.Error <> '') and (Length(FReply.Bytes) = 0),
        'deadline/overflow publishes no partial bytes');
    end;

    if FStage = 13 then
    begin
      Check((FReply.Error = '') and
        (TNyxDataValue.ParseJSON(NyxDecodeUTF8(FReply.Bytes)).Field('caption').AsText =
          'Hello from HTTP'), 'a fresh request recovers after cancellation/overflow');
    end;
  end;
  FToken := nil;
  FReply := Default(TNyxResourceHTTPResult);
  FCalls := 0;
  FWaiting := False;
  Inc(FStage);
end;

procedure TJourney.Drive;
begin
  Step;

  {$ifdef PAS2JS}
  if FDone then
  begin
    FScheduler.Shutdown;
    document.body.setAttribute('data-http-logic', 'passed');
    document.body.setAttribute('data-http-logic-checks', IntToStr(FChecks));
  end;
  {$else}
  if FDone then
  begin
    WriteLn('PASS ', FChecks, ' real HTTP Logic checks');
  end;
  {$endif}
end;

destructor TJourney.Destroy;
{$ifndef PAS2JS}
var
  LMonitor: INyxSchedulerMonitor;
  LUntil: Double;
{$endif}
begin

  if FToken <> nil then
  begin
    FToken.Cancel;
  end;

  if FRun <> nil then
  begin
    FRun.Cancel;
  end;

  if FScheduler <> nil then
  begin
    FScheduler.Shutdown;
    {$ifndef PAS2JS}
    LMonitor := FScheduler as INyxSchedulerMonitor;
    LUntil := Clock + 100;
    { This checked fixture owns its workers and must service their UI couriers
      through retirement before the RTL/heap tracer finalizes. Product request
      cancellation itself remains nonblocking and never joins network work. }
    while (Clock < LUntil) or (LMonitor.WorkerLoad.ActiveWorkers > 0) do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
      Sleep(1);

      if Clock > LUntil + 5000 then
      begin
        raise Exception.Create('Owned HTTP fixture workers did not retire');
      end;
    end;
    {$endif}
  end;
  inherited Destroy;
end;

var
  GJourney: TJourney;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    GJourney.Drive;

    if GJourney.Done then
    begin
      FreeAndNil(GJourney);
    end
    else
    begin
      window.setTimeout(@Drive, 5);
    end;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-http-logic', 'failed');
      document.body.setAttribute('data-http-logic-error', E.Message);
      FreeAndNil(GJourney);
    end;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    GJourney := TJourney.Create;
    {$ifdef PAS2JS}Drive;{$else}
    try
      while not GJourney.Done do
      begin
        Application.ProcessMessages;
        CheckSynchronize;
        GJourney.Drive;
        Sleep(1);
      end;
    finally
      FreeAndNil(GJourney);
    end;
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-http-logic', 'failed');
      document.body.setAttribute('data-http-logic-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
