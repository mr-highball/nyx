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

unit nyx.logic.http;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.http.types, nyx.resources.loader,
  nyx.logic, nyx.flow, nyx.flow.types, nyx.scheduler;

type
  { Stable port identities append optional per-invocation URL data without
    changing earlier ordinals. Text becomes a nominal HTTP(S) URL only at the
    validated request boundary; it never grants transport authority. }
  TNyxHTTPPort = (nhpTrigger, nhpBody, nhpStatus, nhpHeaders, nhpError, nhpSucceeded,
    nhpHeaderVisibility, nhpURL);

  { Managed, immutable nonvisual HTTP component. The saved request contains data
    only. An explicitly injected transport supplies host authority at runtime.
    Each BeginInvoke owns an independent cancellable request. Transport failures
    and non-2xx status are usable output data; malformed configuration/schema or
    missing authority refuse the invocation. Cancellation publishes no outcome.
    A present URL input replaces the configured target for that invocation only;
    absent input keeps it. Empty/malformed present input refuses before sending.
    Request always returns the independent saved configuration, never run data. }
  INyxHTTPCall = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-00000000002C}']
    function GetRequest: TNyxHTTPRequest;
    function BeginInvoke(const AInputs: TNyxFlowValues; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope;
      const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
    property Request: TNyxHTTPRequest read GetRequest;
  end;
  INyxUTF8Decode = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-00000000002D}']
    { Explicit decoding refuses malformed UTF-8; HTTP bytes are never implicitly
      decoded using the machine code page or response MIME charset. }
    function Decode(const ABytes: TNyxBytes): TNyxText;
  end;

function NyxHTTPPort(APort: TNyxHTTPPort): TNyxFlowPortRef;
function NewNyxHTTPCall(const ARequest: TNyxHTTPRequest;
  const ATransport: INyxHTTPTransport = nil): INyxHTTPCall;
function NewNyxUTF8Decode: INyxUTF8Decode;
{ Explicit resolver adds HTTP authority and the byte decoder to existing pure
  built-ins. It retains no document, renderer, application or editor receiver. }
function NewNyxHTTPLogicResolver(const ATransport: INyxHTTPTransport): INyxLogicResolver;

implementation

uses nyx.resource.sources;

type
  THTTPInvocation = class(TInterfacedObject, INyxLogicInvocation)
  private
    FScheduler: INyxScheduler;
    FScope: INyxCancellationScope;
    FCompletion: INyxLogicCompletion;
    FRequest: INyxResourceRequest;
    FLease: INyxLogicInvocation;
    procedure Loaded(const AResult: TNyxResourceHTTPResult);
  public
    constructor Create(const AScheduler: INyxScheduler;
      const AScope: INyxCancellationScope; const ACompletion: INyxLogicCompletion);
    procedure Start(const AHTTP: TNyxHTTPRequest; const ATransport: INyxHTTPTransport);
    procedure Cancel;
  end;
  { Immutable configured operation. Connected URL data derives a request value
    before allocating a pending invocation; concurrent calls cannot replace the
    configured target or another call's method, payload, headers or limits. }
  THTTPCall = class(TNyxLogicOperation, INyxHTTPCall, INyxAsyncLogicOperation)
  private
    FHTTP: TNyxHTTPRequest;
    FTransport: INyxHTTPTransport;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const ARequest: TNyxHTTPRequest;
      const ATransport: INyxHTTPTransport);
    function GetRequest: TNyxHTTPRequest;
    function BeginInvoke(const AInputs: TNyxFlowValues; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope;
      const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
  end;
  TUTF8Decode = class(TNyxLogicOperation, INyxUTF8Decode)
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create;
    function Decode(const ABytes: TNyxBytes): TNyxText;
  end;
  THTTPResolver = class(TInterfacedObject, INyxLogicResolver)
  private
    FTransport: INyxHTTPTransport;
  public
    constructor Create(const ATransport: INyxHTTPTransport);
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: TNyxDataValue): INyxLogicOperation;
  end;

function NyxHTTPPort(APort: TNyxHTTPPort): TNyxFlowPortRef;
const
  CNames: array[TNyxHTTPPort] of TNyxText =
    ('trigger', 'body', 'status', 'headers', 'error', 'succeeded', 'header-visibility', 'url');
begin

  if (Ord(APort) < Ord(Low(TNyxHTTPPort))) or (Ord(APort) > Ord(High(TNyxHTTPPort))) then
  begin
    raise ENyxFlow.Create('Unknown HTTP port');
  end;
  Result := NyxFlowPort(CNames[APort]);
end;

constructor THTTPInvocation.Create(const AScheduler: INyxScheduler;
  const AScope: INyxCancellationScope; const ACompletion: INyxLogicCompletion);
begin
  inherited Create;
  FScheduler := AScheduler;
  FScope := AScope;
  FCompletion := ACompletion;
end;

procedure THTTPInvocation.Start(const AHTTP: TNyxHTTPRequest;
  const ATransport: INyxHTTPTransport);
var
  LRequest: INyxResourceRequest;
begin
  FScheduler.RequireUI;
  FLease := Self;
  try
    LRequest := ATransport.Send(AHTTP, Loaded);

    if FCompletion = nil then
    begin
      { Inline completion wins. Do not retain a completed token returned later. }
      if LRequest <> nil then
      begin
        LRequest.Cancel;
      end;
    end
    else
    begin

      if LRequest = nil then
      begin
        raise ENyxFlow.Create('HTTP transport returned no pending request token');
      end;
      FRequest := LRequest;
    end;
  except
    Cancel;
    raise;
  end;
end;

procedure THTTPInvocation.Cancel;
var
  LLease: INyxLogicInvocation;
  LRequest: INyxResourceRequest;
begin
  FScheduler.RequireUI;
  LLease := Self;
  FCompletion := nil;
  FScope := nil;
  LRequest := FRequest;
  FRequest := nil;
  FLease := nil;

  if LRequest <> nil then
  begin
    LRequest.Cancel;
  end;
end;

procedure THTTPInvocation.Loaded(const AResult: TNyxResourceHTTPResult);
var
  LLease: INyxLogicInvocation;
  LCompletion: INyxLogicCompletion;
  LOutputs: TNyxFlowValues;
begin
  FScheduler.RequireUI;
  LLease := Self;

  if (FCompletion = nil) or ((FScope <> nil) and FScope.Cancelled) then
  begin
    Cancel;
    Exit;
  end;
  LCompletion := FCompletion;
  { Retire the borrowed callback before invoking arbitrary downstream code.
    The transport retires its own method receiver before entering this method. }
  FCompletion := nil;
  FRequest := nil;
  FScope := nil;
  FLease := nil;
  try
    LOutputs := NyxFlowValues
      .WithValue(NyxHTTPPort(nhpBody), NyxFlowBytes(AResult.Bytes))
      .WithValue(NyxHTTPPort(nhpStatus), NyxFlowInteger(AResult.Status))
      .WithValue(NyxHTTPPort(nhpHeaders), NyxFlowJSON(AResult.Headers.ToData))
      .WithValue(NyxHTTPPort(nhpHeaderVisibility),
        NyxFlowInteger(Ord(AResult.HeaderVisibility)))
      .WithValue(NyxHTTPPort(nhpError), NyxFlowText(AResult.Error))
      .WithValue(NyxHTTPPort(nhpSucceeded), NyxFlowBoolean((AResult.Error = '') and
        (AResult.Status >= 200) and (AResult.Status < 300)));
  except
    on LException: Exception do
    begin
      LCompletion.Fail(LException.Message);
      Exit;
    end;
  end;
  LCompletion.Succeed(LOutputs);
end;

constructor THTTPCall.Create(const ARequest: TNyxHTTPRequest;
  const ATransport: INyxHTTPTransport);
begin
  inherited Create;
  FHTTP := TNyxHTTPRequest.FromData(ARequest.ToData);
  FTransport := ATransport;
  FConfiguration := FHTTP.ToData;
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('http-call'), 'HTTP request',
    'Send owned bytes with typed method, headers, deadline and cancellation. ' +
    'Status and transport errors remain available for connected error handling.',
    nlgNetwork, nlaUI)
    .Input(NyxHTTPPort(nhpTrigger), nftSignal, 'Explicit invocation trigger')
    .OptionalInput(NyxHTTPPort(nhpURL), nftText,
      'Absolute HTTP(S) URL for this invocation; absence uses the configured URL')
    .Output(NyxHTTPPort(nhpBody), nftBytes, 'Complete response bytes')
    .Output(NyxHTTPPort(nhpStatus), nftInteger, 'HTTP status; zero without a response')
    .Output(NyxHTTPPort(nhpHeaders), nftJSON, 'Observable ordered header name/value pairs')
    .Output(NyxHTTPPort(nhpHeaderVisibility), nftInteger,
      'TNyxHTTPHeaderVisibility: not collected, native or browser filtered')
    .Output(NyxHTTPPort(nhpError), nftText, 'Transport or framing failure; empty for an HTTP response')
    .Output(NyxHTTPPort(nhpSucceeded), nftBoolean, 'Complete transport and 2xx status')
    .Labelled('network').Labelled('request').Labelled('fetch').Labelled('api');
end;

function THTTPCall.GetRequest: TNyxHTTPRequest;
begin
  Result := TNyxHTTPRequest.FromData(FHTTP.ToData);
end;

function THTTPCall.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues;
  raise ENyxFlow.Create('HTTP requires asynchronous invocation and an explicit transport');
end;

function THTTPCall.BeginInvoke(const AInputs: TNyxFlowValues;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
var
  LInvocation: THTTPInvocation;
  LRequest: TNyxHTTPRequest;
begin

  if (AScheduler = nil) or (ACompletion = nil) or (FTransport = nil) then
  begin
    raise ENyxFlow.Create('HTTP invocation requires a scheduler, completion and explicit transport');
  end;
  AScheduler.RequireUI;
  ValidateNyxLogicFrame(FDescriptor, nfdInput, AInputs);

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxFlowCancelled.Create('HTTP invocation cancelled before admission');
  end;
  LRequest := FHTTP;

  if AInputs.Has(NyxHTTPPort(nhpURL)) then
  begin
    LRequest := LRequest.WithURL(NyxResourceURL(AInputs.Value(NyxHTTPPort(nhpURL)).AsText));
  end;
  LInvocation := THTTPInvocation.Create(AScheduler, ACancellation, ACompletion);
  Result := LInvocation;
  LInvocation.Start(LRequest, FTransport);
end;

constructor TUTF8Decode.Create;
begin
  inherited Create;
  FConfiguration := NyxObject([]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('utf8-decode'), 'Decode UTF-8',
    'Decode complete bytes as strict UTF-8, retaining supplementary text and NUL.', nlgText)
    .Input(NyxHTTPPort(nhpBody), nftBytes, 'Owned UTF-8 bytes')
    .Output(NyxLogicPort(nlpText), nftText, 'Exact Unicode text')
    .Labelled('encoding').Labelled('bytes').Labelled('unicode');
end;

function TUTF8Decode.Decode(const ABytes: TNyxBytes): TNyxText;
begin
  Result := NyxDecodeUTF8(ABytes);
end;

function TUTF8Decode.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpText),
    NyxFlowText(Decode(AInputs.Value(NyxHTTPPort(nhpBody)).AsBytes)));
end;

constructor THTTPResolver.Create(const ATransport: INyxHTTPTransport);
begin
  inherited Create;
  FTransport := ATransport;
end;

function THTTPResolver.Resolve(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
begin

  if AKind.Name = 'http-call' then
  begin
    Exit(NewNyxHTTPCall(TNyxHTTPRequest.FromData(AConfiguration), FTransport));
  end;

  if AKind.Name = 'utf8-decode' then
  begin
    ValidateNyxFlowFields(AConfiguration, []);
    Exit(NewNyxUTF8Decode);
  end;
  Result := NyxLogicFromData(AKind, AConfiguration);
end;

function NewNyxHTTPCall(const ARequest: TNyxHTTPRequest;
  const ATransport: INyxHTTPTransport): INyxHTTPCall;
begin
  Result := THTTPCall.Create(ARequest, ATransport);
end;

function NewNyxUTF8Decode: INyxUTF8Decode;
begin
  Result := TUTF8Decode.Create;
end;

function NewNyxHTTPLogicResolver(const ATransport: INyxHTTPTransport): INyxLogicResolver;
begin
  Result := THTTPResolver.Create(ATransport);
end;

end.
