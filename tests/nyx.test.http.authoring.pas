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

unit nyx.test.http.authoring;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.model, nyx.http.types, nyx.resources.loader, nyx.scheduler;

function HTTPAuthoringRequest: TNyxHTTPRequest;
function HTTPAuthoringDocument: TNyxDocument;

type
  { Explicit substituted provider for the actual-control consumer. It performs no
    I/O. Its deferred byte reply tests the same application resolver, graph, UI
    courier and state bindings without claiming sockets, Fetch or TLS. }
  TNyxTestHTTPTransport = class(TInterfacedObject, INyxHTTPTransport)
  private
    FScheduler: INyxScheduler;
  public
    Calls: Integer;
    Last: TNyxHTTPRequest;
    constructor Create(const AScheduler: INyxScheduler);
    function Send(const ARequest: TNyxHTTPRequest;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
  end;

implementation

uses
  nyx.bytes, nyx.resource.sources, nyx.controls, nyx.types, nyx.state,
  nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.flow.routes,
  nyx.flow.route.model, nyx.logic, nyx.logic.http, nyx.test.logic.document;

type
  TReply = class(TNyxResourceRequest)
  public
    procedure Send;
  end;
  TDelivery = class(TInterfacedObject, INyxWork)
  public
    Reply: TReply;
    Lease: INyxResourceRequest;
    procedure Execute(const AExecution: INyxExecution);
  end;

function HTTPAuthoringRequest: TNyxHTTPRequest;
begin
  Result := NyxHTTPRequest(NyxResourceURL('https://example.invalid/api/profile'))
    .Method(nhmPost).Header('content-type', 'application/json')
    .AppendHeader('x-label', 'first').AppendHeader('x-label', 'second')
    .WithBody(NyxEncodeUTF8('{"name":"English demo"}'))
    .WholeRequest(12000).MaximumBytes(65536);
end;

function HTTPAuthoringDocument: TNyxDocument;
begin
  Result := LogicDocument;
  try
    Result.Title := 'HTTP Logic workshop';
    Result.Find('intro').Configure.Text(
      'Configure a typed request, decode its bytes and format the JSON through connected operations.').Done;
    Result.State.SetValue(NyxIntegerState('http-status'), 0);
    Result.State.SetValue(NyxBooleanState('http-success'), False);
    Result.Flows.Define(NyxFlowRef('Download'), NewNyxFlowGraph
      .WithStep(NyxFlowStep('request'), NewNyxHTTPCall(HTTPAuthoringRequest))
      .WithStep(NyxFlowStep('decode'), NewNyxUTF8Decode)
      .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
      .WithStep(NyxFlowStep('format'), NewNyxJSONFormat)
      .Connect(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpBody)),
        NyxFlowEndpoint(NyxFlowStep('decode'), NyxHTTPPort(nhpBody)))
      .Connect(NyxFlowEndpoint(NyxFlowStep('decode'), NyxLogicPort(nlpText)),
        NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpText)))
      .Connect(NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)),
        NyxFlowEndpoint(NyxFlowStep('format'), NyxLogicPort(nlpJSON))).Build);
    SetNyxFlowRoutes(Result.Find('run-button'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Download profile'), NyxFlowRef('Download'), ntClick,
        nfcmExclusive)
        .InputSignal(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpTrigger)))
        .OutputState(NyxFlowEndpoint(NyxFlowStep('format'), NyxLogicPort(nlpText)),
          NyxTextState('pretty'))
        .OutputState(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpStatus)),
          NyxIntegerState('http-status'))
        .OutputState(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpSucceeded)),
          NyxBooleanState('http-success'))
        .Using(neAsynchronous)));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure TReply.Send;
var
  LReply: TNyxResourceHTTPResult;
begin
  LReply := Default(TNyxResourceHTTPResult);
  LReply.Status := 200;
  LReply.Bytes := NyxEncodeUTF8('{"title":"Fetched profile","rank":9007199254740993}');
  LReply.Headers := NyxHTTPHeaders.Append('content-type', 'application/json');
  LReply.HeaderVisibility := nhvBrowserFiltered;
  Complete(LReply);
end;

procedure TDelivery.Execute(const AExecution: INyxExecution);
begin
  Reply.Send;
end;

constructor TNyxTestHTTPTransport.Create(const AScheduler: INyxScheduler);
begin
  inherited Create;
  FScheduler := AScheduler;
end;

function TNyxTestHTTPTransport.Send(const ARequest: TNyxHTTPRequest;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
var
  LReply: TReply;
  LDelivery: TDelivery;
  LWork: INyxWork;
begin
  FScheduler.RequireUI;
  Inc(Calls);
  Last := TNyxHTTPRequest.FromData(ARequest.ToData);
  LReply := TReply.Create(AReply);
  Result := LReply;
  LDelivery := TDelivery.Create;
  LWork := LDelivery;
  LDelivery.Reply := LReply;
  LDelivery.Lease := Result;
  FScheduler.PostUI(LWork);
end;

end.
