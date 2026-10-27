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

program nyx_http_logic_contract_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.http.types,
  nyx.resource.sources, nyx.resources.loader, nyx.logic.http, nyx.logic,
  nyx.flow, nyx.flow.types, nyx.flow.execution, nyx.scheduler, nyx.linq,
  nyx.linq.execution
  {$ifdef PAS2JS}, JS, Web{$endif};

type
  { Closed transport/target scenarios keep legacy configured calls beside
    connected-target admission, including refusal before any external effect. }
  TMode = (hmInline, hmDeferred, hmError, hmStatus, hmNoAuthority, hmNoToken,
    hmThrow, hmCancel, hmGeneration, hmInlineNoToken, hmConnectedURL,
    hmEmptyURL, hmBadURL, hmWrongURLType);
  TRequest = class(TNyxResourceRequest)
  public
    procedure Send(const AResult: TNyxResourceHTTPResult);
  end;
  TDelivery = class(TInterfacedObject, INyxWork)
  public
    Request: TRequest;
    Lease: INyxResourceRequest;
    Reply: TNyxResourceHTTPResult;
    procedure Execute(const AExecution: INyxExecution);
  end;
  { Deliberately substituted transport. These tests qualify component/frame and
    ownership semantics; they make no claim about HTTP sockets, TLS or Fetch. }
  TTransport = class(TInterfacedObject, INyxHTTPTransport)
  public
    Mode: TMode;
    Scheduler: INyxScheduler;
    Last: TNyxHTTPRequest;
    Pending: TRequest;
    Lease: INyxResourceRequest;
    Calls: Integer;
    function Send(const ARequest: TNyxHTTPRequest;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
  end;
  TPort = class(TInterfacedObject, INyxQueryResultPort<INyxFlowResult>)
  public
    Calls: Integer;
    Last: INyxFlowResult;
    function Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
  end;

var
  GChecks: Integer;
  GCase: Integer;
  GWaiting: Boolean;
  GDone: Boolean;
  GScheduler: INyxScheduler;
  GRun: INyxQueryRun;
  GPort: TPort;
  GPortLease: INyxQueryResultPort<INyxFlowResult>;
  GTransport: TTransport;
  GTransportLease: INyxHTTPTransport;
  GGraph: INyxFlowGraph;
  GScope: INyxCancellationScope;
  GStarted: Double;
  GCaseAt: Double;
  {$ifndef PAS2JS}
  GMonitor: INyxSchedulerMonitor;
  GDrainUntil: Double;
  {$endif}

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure RequestBoundaries;
var
  LRequest: TNyxHTTPRequest;
  LCandidate: TNyxHTTPRequest;
  LHeaders: TNyxHTTPHeaders;
  LIndex: Integer;
  LFailed: Boolean;
begin
  LRequest := NyxHTTPRequest(NyxResourceURL('https://example.invalid/api'))
    .Method(nhmPost).WithBody(NyxEncodeUTF8('owned')).Header('x-kept', 'original').
      WholeRequest(3210).MaximumBytes(1024);
  LCandidate := LRequest.WithURL(NyxResourceURL('https://example.invalid/derived'));
  Check((LRequest.URL.Address = 'https://example.invalid/api') and
    (LCandidate.URL.Address = 'https://example.invalid/derived') and
    (LCandidate.RequestMethod.Name = 'POST') and
    (LCandidate.Headers.First('x-kept') = 'original') and
    (NyxDecodeUTF8(LCandidate.Body) = 'owned') and
    (LCandidate.DeadlineMS = 3210) and (LCandidate.ByteLimit = 1024),
    'derived target preserves its independent configured request and every non-target option');
  LHeaders := NyxHTTPHeaders.Append('x', TNyxText(StringOfChar('a', 8191)) + TNyxText('é'));
  Check(LHeaders.Count = 1, 'Latin-1 header limit is equal on UTF-8/native and browser strings');
  for LIndex := 0 to 8 do
  begin
    LFailed := False;
    try
      case LIndex of
        0: LCandidate := NyxHTTPRequest(LRequest.URL).WithBody(LRequest.Body);
        1: LCandidate := LRequest.Method(nhmHead);
        2: LCandidate := LRequest.Method(NyxHTTPMethod('TRACE'));
        3: LCandidate := LRequest.Header('cookie', 'private');
        4: LCandidate := LRequest.Header('x', '🌟');
        5: LCandidate := LRequest.WholeRequest(0);
        6: LCandidate := LRequest.MaximumBytes(NyxMaximumPackedBytes + 1);
        7: LCandidate := LRequest.Header('x-http-method-override', 'TRACE');
        8: NewNyxUTF8Decode.Decode(NyxDecodeBase64('/w=='));
      end;
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'invalid portable request/encoding refuses ' + IntToStr(LIndex));
  end;
  LHeaders := NyxHTTPHeaders;
  for LIndex := 0 to 63 do
  begin
    LHeaders := LHeaders.Append('x', 'same');
  end;
  LFailed := False;
  try
    LHeaders := LHeaders.Append('x', 'overflow');
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LHeaders.Count = 64), 'header-count overflow preserves the immutable baseline');
end;

procedure TRequest.Send(const AResult: TNyxResourceHTTPResult);
begin
  Complete(AResult);
end;

procedure TDelivery.Execute(const AExecution: INyxExecution);
begin
  Request.Send(Reply);
  Request.Send(Reply);
end;

function TTransport.Send(const ARequest: TNyxHTTPRequest;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
var
  LReply: TNyxResourceHTTPResult;
  LDelivery: TDelivery;
  LWork: INyxWork;
begin
  Inc(Calls);
  Last := TNyxHTTPRequest.FromData(ARequest.ToData);

  if Mode = hmThrow then
  begin
    raise Exception.Create('provider admission refused');
  end;

  if Mode = hmNoToken then
  begin
    Exit(nil);
  end;
  Pending := TRequest.Create(AReply);
  Lease := Pending;
  Result := Lease;
  LReply := Default(TNyxResourceHTTPResult);
  LReply.Status := 200;
  LReply.Bytes := NyxEncodeUTF8('{"exact":9007199254740993,"text":"Hello 🌟\u0000!"}');
  LReply.Headers := NyxHTTPHeaders.Append('x-fixture', 'first').Append('x-fixture', 'second');
  LReply.HeaderVisibility := nhvBrowserFiltered;

  if Mode = hmError then
  begin
    LReply.Status := 0;
    LReply.Bytes := nil;
    LReply.Error := 'offline';
  end;

  if Mode = hmStatus then
  begin
    LReply.Status := 422;
  end;

  if Mode in [hmCancel, hmGeneration] then
  begin
    Exit;
  end;

  if Mode = hmDeferred then
  begin
    LDelivery := TDelivery.Create;
    LWork := LDelivery;
    LDelivery.Request := Pending;
    LDelivery.Lease := Lease;
    LDelivery.Reply := LReply;
    Scheduler.PostUI(LWork);
  end
  else
  begin
    Pending.Send(LReply);
    Pending.Send(LReply);
  end;

  if Mode = hmInlineNoToken then
  begin
    Result := nil;
  end;
end;

function TPort.Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
begin
  Check(Length(AValues) = 1, 'exactly one complete result');
  Inc(Calls);
  Last := AValues[0];
  Result := True;
end;

procedure Step;
var
  LRequest: TNyxHTTPRequest;
  LAuthority: INyxHTTPTransport;
  LBuilder: INyxFlowBuilder;
  LOutputs: INyxFlowResult;
  LStatus: Integer;
  LBytes: TNyxBytes;
  LCopy: TNyxHTTPRequest;
  LRejected: Boolean;
  LInputs: array of TNyxFlowInput;
begin

  if not GWaiting then
  begin

    if GCase > Ord(High(TMode)) then
    begin
      GDone := True;
      Exit;
    end;
    GTransport := TTransport.Create;
    GTransportLease := GTransport;
    GTransport.Mode := TMode(GCase);
    GTransport.Scheduler := GScheduler;
    GPort := TPort.Create;
    GPortLease := GPort;
    GScope := NewNyxCancellationScope;
    LRequest := NyxHTTPRequest(NyxResourceURL('https://example.invalid/api'))
      .Method(nhmPatch).Header('x-owned', 'first')
      .WithBody(NyxEncodeUTF8('Upload 🌟' + #0 + ' bytes'));
    LCopy := LRequest.Header('x-owned', 'second');
    LBytes := LRequest.Body;
    LBytes[0] := 0;
    Check((LRequest.Headers.First('x-owned') = 'first') and
      (LCopy.Headers.First('x-owned') = 'second') and
      (NyxDecodeUTF8(LRequest.Body) = TNyxText('Upload 🌟') + #0 + TNyxText(' bytes')),
      'request headers/body remain detached');
    LAuthority := GTransportLease;

    if TMode(GCase) = hmNoAuthority then
    begin
      LAuthority := nil;
    end;
    LBuilder := NewNyxFlowGraph.WithStep(NyxFlowStep('request'),
      NewNyxHTTPCall(LRequest, LAuthority));

    if TMode(GCase) in [hmInline, hmDeferred, hmInlineNoToken, hmConnectedURL] then
    begin
      LBuilder.WithStep(NyxFlowStep('decode'), NewNyxUTF8Decode)
        .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
        .Connect(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpBody)),
          NyxFlowEndpoint(NyxFlowStep('decode'), NyxHTTPPort(nhpBody)))
        .Connect(NyxFlowEndpoint(NyxFlowStep('decode'), NyxLogicPort(nlpText)),
          NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpText)));
    end;
    GGraph := LBuilder.Build;
    GGraph := NyxFlowGraphFromData(GGraph.ToData, NewNyxHTTPLogicResolver(LAuthority));
    Check(GGraph.ToData.ToJSON = NyxFlowGraphFromData(GGraph.ToData,
      NewNyxHTTPLogicResolver(nil)).ToData.ToJSON, 'wire stores data without authority');
    LRejected := False;
    try
      LCopy := LRequest.Header('x', 'bad' + #13#10 + 'field');
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'header injection refuses before transport');
    SetLength(LInputs, 1);
    LInputs[0] := NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpTrigger)),
      NyxFlowSignal);

    if TMode(GCase) in [hmConnectedURL, hmEmptyURL, hmBadURL, hmWrongURLType] then
    begin
      SetLength(LInputs, 2);
      LInputs[1] := NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpURL)),
        NyxFlowText('https://example.invalid/connected?name=River%20Moon'));

      if TMode(GCase) = hmEmptyURL then
      begin
        LInputs[1].Value := NyxFlowText('');
      end;

      if TMode(GCase) = hmBadURL then
      begin
        LInputs[1].Value := NyxFlowText('file:///private/input.json');
      end;

      if TMode(GCase) = hmWrongURLType then
      begin
        LInputs[1].Value := NyxFlowInteger(42);
      end;
    end;
    LRejected := False;
    try
      GRun := TNyxFlowExecution.Submit(GGraph,
        LInputs, GScheduler, GPortLease, neAsynchronous, GScope);
    except
      on E: ENyxFlow do
      begin

        if TMode(GCase) <> hmWrongURLType then
        begin
          raise;
        end;
        LRejected := True;
      end;
    end;

    if TMode(GCase) = hmWrongURLType then
    begin
      Check(LRejected and (GTransport.Calls = 0) and (GPort.Calls = 0),
        'wrong URL value family refuses synchronously before scheduling or transport');
      GGraph := nil;
      GPortLease := nil;
      GScope := nil;
      GTransportLease := nil;
      Inc(GCase);
      Exit;
    end;
    GCaseAt := Clock;
    GWaiting := True;
    Exit;
  end;

  if TMode(GCase) in [hmCancel, hmGeneration] then
  begin

    if GTransport.Calls = 0 then
    begin
      Exit;
    end;

    if TMode(GCase) = hmCancel then
    begin
      GRun.Cancel;
    end
    else
    begin
      GScope.Cancel;
      GRun := nil;
      { No run polling after dropping the caller lease: real delayed ownership
        observation must still cancel the pending host request. }
      if Clock - GCaseAt < 100 then
      begin
        Exit;
      end;
    end;
    GTransport.Pending.Send(Default(TNyxResourceHTTPResult));
    Check(GPort.Calls = 0, 'late response after cancellation/retirement publishes nothing');
  end
  else
  begin

    if GRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
    begin
      Exit;
    end;

    if TMode(GCase) in [hmNoAuthority, hmNoToken, hmThrow, hmEmptyURL, hmBadURL, hmWrongURLType] then
    begin
      Check((GRun.Phase = nqpFailed) and (GPort.Calls = 0), 'host admission failure publishes no prefix');

      if TMode(GCase) in [hmNoAuthority, hmEmptyURL, hmBadURL, hmWrongURLType] then
      begin
        Check(GTransport.Calls = 0, 'missing authority or invalid target has no transport effect');
      end;
    end
    else
    begin
      Check((GRun.Phase = nqpSucceeded) and (GPort.Calls = 1),
        'HTTP outcome publishes once: ' + GRun.Failure);
      LOutputs := GPort.Last;
      LStatus := 200;

      if TMode(GCase) = hmStatus then
      begin
        LStatus := 422;
      end;

      if TMode(GCase) = hmError then
      begin
        LStatus := 0;
      end;
      Check(LOutputs.Value(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpStatus)))
        .AsInteger = LStatus, 'HTTP status remains usable output');
      Check(LOutputs.Value(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpSucceeded)))
        .AsBoolean = (LStatus = 200), 'HTTP success distinguishes failure status/transport');

      if TMode(GCase) = hmError then
      begin
        Check(LOutputs.Value(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpError)))
          .AsText = 'offline', 'transport error is a usable output');
      end
      else
      begin
        Check(LOutputs.Value(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpHeaders)))
          .AsJSON.Count = 2, 'duplicate observable headers survive');
      end;

      if TMode(GCase) in [hmInline, hmDeferred, hmInlineNoToken, hmConnectedURL] then
      begin
        Check(LOutputs.Value(NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)))
          .AsJSON.Field('exact').ToJSON = '9007199254740993', 'resumed parse retains exact numbers');
        Check(LOutputs.Value(NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)))
          .AsJSON.Field('text').AsText = TNyxText('Hello 🌟') + #0 + TNyxText('!'),
          'resumed decode retains supplementary text and NUL');
      end;

      if TMode(GCase) = hmConnectedURL then
      begin
        Check((GTransport.Last.URL.Address = 'https://example.invalid/connected?name=River%20Moon') and
          (GTransport.Last.RequestMethod.Name = 'PATCH') and
          (GTransport.Last.Headers.First('x-owned') = 'first') and
          (NyxDecodeUTF8(GTransport.Last.Body) = TNyxText('Upload 🌟') + #0 + TNyxText(' bytes')),
          'connected URL reaches the transport while preserving method, owned headers and exact payload');
      end
      else
      begin
        Check(GTransport.Last.URL.Address = 'https://example.invalid/api',
          'unwired legacy graphs retain their exact configured target');
      end;
    end;
  end;
  GRun := nil;
  GGraph := nil;
  GPortLease := nil;
  GScope := nil;
  GTransportLease := nil;
  GWaiting := False;
  Inc(GCase);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if GDone then
    begin
      GScheduler.Shutdown;
      document.body.setAttribute('data-http-contract', 'passed');
      document.body.setAttribute('data-http-contract-checks', IntToStr(GChecks));
      Exit;
    end;

    if Clock - GStarted > 30000 then
    begin
      raise Exception.Create('HTTP contract fixture deadline ' + IntToStr(GCase));
    end;
    window.setTimeout(@Drive, 5);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-http-contract', 'failed');
      document.body.setAttribute('data-http-contract-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  try
    RequestBoundaries;
    GScheduler := NewNyxScheduler;
    GStarted := Clock;
    {$ifdef PAS2JS}Drive;{$else}
    while not GDone do
    begin
      CheckSynchronize;
      Step;

      if Clock - GStarted > 30000 then
      begin
        raise Exception.Create('HTTP contract fixture deadline ' + IntToStr(GCase));
      end;
      Sleep(1);
    end;
    GScheduler.Shutdown;
    GMonitor := GScheduler as INyxSchedulerMonitor;
    GDrainUntil := Clock + 100;
    { Product shutdown cancels without joining. The checked console fixture
      must continue servicing its real UI couriers and let free-on-terminate
      native workers retire before the RTL/heap tracer finalizes its globals. }
    while (Clock < GDrainUntil) or (GMonitor.WorkerLoad.ActiveWorkers > 0) do
    begin
      CheckSynchronize;
      Sleep(1);

      if Clock > GDrainUntil + 5000 then
      begin
        raise Exception.Create('Owned HTTP contract workers did not retire');
      end;
    end;
    Check(GMonitor.WorkerLoad.ActiveWorkers = 0, 'owned native workers retire before heap finalization');
    GMonitor := nil;
    GScheduler := nil;
    WriteLn('PASS ', GChecks, ' HTTP component contract checks / substituted transport');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-http-contract', 'failed');
      document.body.setAttribute('data-http-contract-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
