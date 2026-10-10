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

program nyx_resource_stream_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.model, nyx.controls, nyx.codec,
  nyx.resources, nyx.resource.sources, nyx.resources.loader, nyx.resource.context,
  nyx.application.resources, nyx.scheduler, nyx.test.resource.stream
  {$ifdef PAS2JS}, Web, nyx.application.browser, nyx.resources.http.browser
  {$else}, Interfaces, Forms, StdCtrls, Graphics, IntfGraphics, FPWritePNG,
    nyx.application.lcl, nyx.resources.http.lcl{$endif};

type
  TStreamApplication = {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif};
  { The resolver still uses the actual adapter. This observer owns only its last
    request token, allowing public optional progress inspection, without wrapping
    replies, fabricating bytes or touching the application's private lifetime. }
  TObservedTransport = class(TInterfacedObject, INyxResourceTransport)
  private
    FInner: INyxResourceTransport;
    FLast: INyxResourceRequest;
  public
    constructor Create(const AInner: INyxResourceTransport);
    function Request(const AURL: TNyxResourceURL;
      const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
    function LastProgress: INyxResourceRequestProgress;
  end;

  { Independent application/document owner. Method subscriptions disconnect
    before this receiver retires. Runtime and transport descriptors may outlive
    their host; neither owns the document tree. }
  TJourney = class
  private
    FDocument: TNyxDocument;
    FApplication: TStreamApplication;
    FResources: INyxApplicationResources;
    FSubscription: INyxResourceSubscription;
    FScheduler: INyxScheduler;
    FTransport: TObservedTransport;
    FTransportLease: INyxResourceTransport;
    FCancelled: INyxResourceRequestProgress;
    FTerminal: TNyxResourceTransferProgress;
    FBefore: TNyxText;
    FBeforeRuntime: TNyxText;
    FBeforeChanges: Integer;
    FChanges: Integer;
    FStage: Integer;
    FStarted: Double;
    FRetiredAt: Double;
    FChecks: Integer;
    FFinished: Boolean;
    FDeliveryCase: TNyxTestDeliveryCase;
    FSecureURL: TNyxText;
    FSecureCaptionIdentity: {$ifdef PAS2JS}TJSHTMLElement{$else}TObject{$endif};
    FSecureInputIdentity: {$ifdef PAS2JS}TJSHTMLElement{$else}TObject{$endif};
    {$ifdef PAS2JS}
    FTimer: NativeInt;
    FHost: TJSHTMLElement;
    FCaptionIdentity: TJSHTMLElement;
    FInputIdentity: TJSHTMLElement;
    {$else}
    FServer: TNyxResourceStreamFixture;
    FCaptionIdentity: TObject;
    FInputIdentity: TObject;
    {$endif}
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Changed(const AContext: INyxResourceContext);
    procedure Controls(const ACaption, APrompt: TNyxText);
    function Checkpoint(const AName: TNyxText): Boolean;
    procedure BeginHeld;
    procedure RetainTerminal;
    procedure CheckTerminal;
    { Coordinate real wire cases before the existing held-body lifecycle.
      Authored defaults never change; HTTPS variants have distinct typed locales. }
    procedure BeginDelivery;
    procedure CheckDelivery;
    procedure Finish;
  public
    procedure Start;
    procedure Next;
    destructor Destroy; override;
    property Finished: Boolean read FFinished;
  end;

function Milliseconds: Double;
begin
  {$ifdef PAS2JS}
  Result := window.performance.now;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

constructor TObservedTransport.Create(const AInner: INyxResourceTransport);
begin
  inherited Create;
  FInner := AInner;
end;

function TObservedTransport.Request(const AURL: TNyxResourceURL;
  const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
begin
  Result := FInner.Request(AURL, AOptions, AMaximumBytes, AReply);
  FLast := Result;
end;

function TObservedTransport.LastProgress: INyxResourceRequestProgress;
begin

  if not Supports(FLast, INyxResourceRequestProgress, Result) then
  begin
    raise ENyxResource.Create('The built-in transport must expose typed transfer progress');
  end;
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxResource.Create(TNyxText('Resource stream: ') + AReason);
  end;
  Inc(FChecks);
end;

procedure TJourney.Changed(const AContext: INyxResourceContext);
begin
  Inc(FChanges);
end;

procedure TJourney.Controls(const ACaption, APrompt: TNyxText);
begin
  {$ifdef PAS2JS}
  Check(FApplication.View.ElementFor('caption') = FCaptionIdentity,
    'browser label identity stays mounted');
  Check(FApplication.View.InputFor('project-name') = FInputIdentity,
    'browser input identity stays mounted');
  Check(FCaptionIdentity.textContent = ACaption, 'actual browser label retains exact content');
  Check(TJSHTMLInputElement(FInputIdentity).placeholder = APrompt,
    'actual browser prompt retains exact content');
  {$else}
  Check(FApplication.View.ControlFor('caption') = FCaptionIdentity,
    'native label identity stays mounted');
  Check(FApplication.View.InputFor('project-name') = FInputIdentity,
    'native input identity stays mounted');
  Check(TNyxText(RawByteString(TLabel(FCaptionIdentity).Caption)) = ACaption,
    'actual native label retains exact content');
  Check(TNyxText(RawByteString(TEdit(FInputIdentity).TextHint)) = APrompt,
    'actual native prompt retains exact content');
  {$endif}
  Check(TNyxCodec.Encode(FDocument) = FBefore, 'loading/cancellation never rewrites authored defaults');
end;

function TJourney.Checkpoint(const AName: TNyxText): Boolean;
{$ifndef PAS2JS}
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LPath: String;
{$endif}
begin
  {$ifdef PAS2JS}
  document.body.setAttribute('data-capture-checkpoint', AName);
  Result := document.body.getAttribute('data-capture-observed') = AName;
  {$else}
  Check(FServer.Error = '', 'bounded actual producer remains healthy');

  if FApplication <> nil then
  begin
    LPath := ParamStr(1) + '.' + String(AName) + '.png';
    Check(not FileExists(LPath), 'native checkpoint requires a fresh owned PNG path');
    LBitmap := TBitmap.Create;
    LImage := nil;
    LWriter := TFPWriterPNG.Create;
    try
      Application.ProcessMessages;
      LBitmap.SetSize(FApplication.Window.ClientWidth, FApplication.Window.ClientHeight);
      FApplication.Window.PaintTo(LBitmap.Canvas, 0, 0);
      LImage := LBitmap.CreateIntfImage;
      LImage.SaveToFile(LPath, LWriter);
    finally
      LWriter.Free;
      LImage.Free;
      LBitmap.Free;
    end;
  end;
  Result := True;
  {$endif}
end;

procedure TJourney.BeginHeld;
begin
  FBeforeRuntime := FResources.Context.Snapshot.ToData.ToJSON;
  FBeforeChanges := FChanges;
  {$ifndef PAS2JS}
  FServer.ArmNext(True, False);
  {$endif}
  FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
end;

procedure TJourney.RetainTerminal;
var
  LReceiving: TNyxResourceTransferProgress;
begin
  FCancelled := FTransport.LastProgress;
  LReceiving := FCancelled.Progress;
  Check((LReceiving.Phase = nrtReceiving) and (LReceiving.BytesReceived > 0) and
    (LReceiving.BytesReceived <= NyxStreamPrefixBytes),
    'real body prefix arrived while its tail is withheld');
  {$ifdef PAS2JS}
  document.body.setAttribute('data-received-prefix', IntToStr(LReceiving.BytesReceived));
  {$else}
  WriteLn('Observed actual received prefix / ', LReceiving.BytesReceived, ' bytes');
  {$endif}

  if FStage = 1 then
  begin
    FResources.Cancel;
  end
  else
  begin
    FSubscription.Disconnect;
    FSubscription := nil;
    FreeAndNil(FApplication);
    Check(NyxApplicationResourceDiagnostics(FResources).CaptureRuntime.Stopped,
      'host disposal stops the retained resource owner');
  end;
  FTerminal := FCancelled.Progress;
  Check((FTerminal.Phase = nrtCancelled) and
    (FTerminal.BytesReceived >= LReceiving.BytesReceived),
    'cancellation immediately exposes a coherent terminal prefix');
  LReceiving.BytesReceived := -1;
  LReceiving.Phase := nrtDelivered;
  Check(FCancelled.Progress.BytesReceived > 0, 'editing a copied snapshot cannot change its request');
end;

procedure TJourney.CheckTerminal;
var
  LCurrent: TNyxResourceTransferProgress;
begin
  LCurrent := FCancelled.Progress;
  Check((LCurrent.Phase = FTerminal.Phase) and
    (LCurrent.BytesReceived = FTerminal.BytesReceived), 'terminal snapshot ignores late platform work');
  Check((FResources.Context.Snapshot.ToData.ToJSON = FBeforeRuntime) and
    (FChanges = FBeforeChanges), 'partial body cannot publish a catalog or Changed notification');
end;

procedure TJourney.Start;
var
  LURL: TNyxText;
  {$ifdef PAS2JS}
  LParameters: TJSURLSearchParams;
  {$endif}
  LInner: INyxResourceTransport;
  LResolver: INyxResourceResolver;
  LPage: INyxColumn;
begin
  FStarted := Milliseconds;
  {$ifdef PAS2JS}
  LParameters := TJSURLSearchParams.new(window.location.search);
  Check(LParameters.has('resource'), 'actual producer URL is explicit');
  LURL := String(LParameters.get('resource'));

  if LParameters.has('secure') then
  begin
    FSecureURL := String(LParameters.get('secure'));
  end;
  Check(LURL <> '', 'actual producer URL is explicit');
  LInner := NewNyxBrowserResourceTransport;
  {$else}
  Check(ParamCount in [1, 2], 'supply a fresh owned PNG prefix and optional immutable HTTPS JSON URL');
  FSecureURL := TNyxText(ParamStr(2));
  FServer := TNyxResourceStreamFixture.Create('');
  LURL := FServer.URL;
  FScheduler := NewNyxScheduler;
  LInner := NewNyxNativeResourceTransport(FScheduler);
  {$endif}
  FTransport := TObservedTransport.Create(LInner);
  FTransportLease := FTransport;
  LResolver := NewNyxResourceResolver(FTransportLease);
  FDocument := TNyxDocument.Create;
  FDocument.Title := 'Resource stream workshop';
  FDocument.Resources.Define(NyxResourceRef('copy'),
    NyxHostedResource(nrkJSON, NyxResourceURL(LURL)).Cache(
      NyxResourceCache.Bypass.MaximumBytes(NyxStreamMaximumBytes))
      .Fallback(NyxJSONResource('{"headline":"Ready while loading","prompt":"Local project name"}')));
  LPage := NewNyxColumn('home');
  LPage.Configure.Padding(24).Gap(16).Done;
  FDocument.AddPage(LPage);
  LPage.Add(NewNyxHeading('workshop-title').Configure.Text('Keep your workspace close').Done);
  LPage.Add(NewNyxLabel('caption').Binds.Text(NyxResourceValue(NyxResourceRef('copy'))
    .Field('headline')).Done);
  LPage.Add(NewNyxInput('project-name').Binds.Placeholder(NyxResourceValue(NyxResourceRef('copy'))
    .Field('prompt')).Done);

  if FSecureURL <> '' then
  begin
    Check(Copy(FSecureURL, 1, 8) = 'https://', 'positive secure fixture explicitly uses HTTPS');
    FDocument.Resources.Define(NyxResourceRef('secure-copy'),
      NyxHostedResource(nrkJSON, NyxResourceURL(FSecureURL))
        .Cache(NyxResourceCache.Bypass.MaximumBytes(NyxStreamMaximumBytes))
        .Fallback(NyxJSONResource('{"headline":"Secure copy unavailable","prompt":"A local secure prompt"}')));
    FDocument.Resources.Define(NyxResourceRef('secure-copy'), NyxLocale('en-GB'),
      NyxHostedResource(nrkJSON, NyxResourceURL('https://expired.badssl.com/'))
        .Cache(NyxResourceCache.Bypass.MaximumBytes(NyxStreamMaximumBytes))
        .Fallback(NyxJSONResource('{"headline":"Secure copy unavailable","prompt":"A local secure prompt"}')));
    LPage.Add(NewNyxLabel('secure-caption').Binds.Text(
      NyxResourceValue(NyxResourceRef('secure-copy')).Field('headline')).Done);
    LPage.Add(NewNyxInput('secure-project-name').Binds.Placeholder(
      NyxResourceValue(NyxResourceRef('secure-copy')).Field('prompt')).Done);
  end;
  FBefore := TNyxCodec.Encode(FDocument);
  FApplication := TStreamApplication.Create;
  FApplication.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand)
    .Request(NyxResourceLoadOptions.WholeRequest(8000)), LResolver);
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(FHost);
  FApplication.Run(FDocument, FHost);
  FCaptionIdentity := FApplication.View.ElementFor('caption');
  FInputIdentity := FApplication.View.InputFor('project-name');
  {$else}
  FApplication.Mount(FDocument);
  FApplication.Window.Show;
  FCaptionIdentity := FApplication.View.ControlFor('caption');
  FInputIdentity := FApplication.View.InputFor('project-name');
  {$endif}

  if FSecureURL <> '' then
  begin
    {$ifdef PAS2JS}
    FSecureCaptionIdentity := FApplication.View.ElementFor('secure-caption');
    FSecureInputIdentity := FApplication.View.InputFor('secure-project-name');
    {$else}
    FSecureCaptionIdentity := FApplication.View.ControlFor('secure-caption');
    FSecureInputIdentity := FApplication.View.InputFor('secure-project-name');
    {$endif}
  end;
  FResources := FApplication.Resources;
  FSubscription := FResources.Subscribe(nil, Changed);
  FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
  {$ifdef PAS2JS}
  FTimer := window.setInterval(@Next, 20);
  {$endif}
end;

procedure TJourney.BeginDelivery;
var
  LReference: TNyxResourceRef;
  LLocale: TNyxLocaleRef;
begin
  LReference := NyxResourceRef('copy');
  LLocale := NyxDefaultLocale;

  if FDeliveryCase in [ndcHTTPS, ndcInvalidTLS] then
  begin
    LReference := NyxResourceRef('secure-copy');

    if FDeliveryCase = ndcInvalidTLS then
    begin
      LLocale := NyxLocale('en-GB');
    end;
    FResources.Localize(LLocale, NyxDefaultLocale);
  end
  else
  begin
    {$ifndef PAS2JS}
    FServer.ArmPolicy(NyxTestDeliveryReply(FDeliveryCase));
    {$endif}
  end;
  FResources.Reload(LReference, LLocale);
end;

procedure TJourney.CheckDelivery;
var
  LStatus: TNyxApplicationResourceStatus;
  LSuccess: Boolean;
  LCaption: TNyxText;
  LPrompt: TNyxText;
  LLocale: TNyxLocaleRef;
begin
  LSuccess := FDeliveryCase in [ndcGzip, ndcChunked, ndcRecovered, ndcHTTPS];
  {$ifndef PAS2JS}
  LSuccess := LSuccess or (FDeliveryCase = ndcCorsDenied);
  {$endif}
  LLocale := NyxDefaultLocale;

  if FDeliveryCase = ndcInvalidTLS then
  begin
    LLocale := NyxLocale('en-GB');
  end;

  if FDeliveryCase in [ndcHTTPS, ndcInvalidTLS] then
  begin
    LStatus := FResources.Status(NyxResourceRef('secure-copy'), LLocale);
    LCaption := 'Secure copy unavailable';
    LPrompt := 'A local secure prompt';
  end
  else
  begin
    LStatus := FResources.Status(NyxResourceRef('copy'), LLocale);
    LCaption := 'Ready while loading';
    LPrompt := 'Local project name';
  end;

  if LSuccess then
  begin
    Check((LStatus.Origin = rloNetwork) and (LStatus.Error = ''),
      'actual admitted transport case publishes a complete network value');
    LCaption := 'Keep creating 🌙';
    LPrompt := 'Project name 🌙';
  end
  else
  begin
    Check((LStatus.Origin = rloFallback) and (LStatus.Error <> ''),
      NyxTestDeliveryName(FDeliveryCase) +
        ': real transport refusal reports diagnostic and explicit whole fallback');
  end;

  if FDeliveryCase = ndcGzipLimit then
  begin
    Check(Pos('byte budget', LStatus.Error) > 0,
      'caller budget bounds decoded gzip bytes rather than the small wire envelope');
  end;
  {$ifndef PAS2JS}

  if FDeliveryCase = ndcInvalidTLS then
  begin
    { ERROR_WINHTTP_SECURE_FAILURE proves trust refusal, rather than accepting
      a different DNS/JSON failure as expired-certificate evidence. }
    Check(Pos('12175', LStatus.Error) > 0, 'the system refuses the real invalid TLS certificate');
  end;
  {$endif}

  if FDeliveryCase in [ndcHTTPS, ndcInvalidTLS] then
  begin
    {$ifdef PAS2JS}
    Check((FApplication.View.ElementFor('secure-caption') = FSecureCaptionIdentity) and
      (FApplication.View.InputFor('secure-project-name') = FSecureInputIdentity),
      'secure publication retains the actual browser controls');
    Check((FSecureCaptionIdentity.textContent = LCaption) and
      (TJSHTMLInputElement(FSecureInputIdentity).placeholder = LPrompt),
      'actual HTTPS result/fallback paints the browser caption and prompt');
    {$else}
    Check((FApplication.View.ControlFor('secure-caption') = FSecureCaptionIdentity) and
      (FApplication.View.InputFor('secure-project-name') = FSecureInputIdentity),
      'secure publication retains the actual native controls');
    Check((TNyxText(RawByteString(TLabel(FSecureCaptionIdentity).Caption)) = LCaption) and
      (TNyxText(RawByteString(TEdit(FSecureInputIdentity).TextHint)) = LPrompt),
      'actual HTTPS result/fallback paints the native caption and prompt');
    {$endif}
  end
  else
  begin
    Controls(LCaption, LPrompt);
  end;
  Check(TNyxCodec.Encode(FDocument) = FBefore,
    'wire admission/refusal never rewrites authored defaults or typed selectors');
end;

procedure TJourney.Next;
var
  LProgress: TNyxResourceTransferProgress;
  LStatus: TNyxApplicationResourceStatus;
  {$ifndef PAS2JS}
  LWorkers: TNyxWorkerPoolSnapshot;
  {$endif}
begin

  if FFinished then
  begin
    Exit;
  end;
  try
    Check(Milliseconds - FStarted < 30000, 'actual stream journey remains within thirty seconds');
    {$ifndef PAS2JS}
    Check(FServer.Error = '', 'actual streaming producer reports no failure');
    {$endif}
    case FStage of
      0, 3:
        begin

          if FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale).Phase <> nrpReady then
          begin
            Exit;
          end;
          LProgress := FTransport.LastProgress.Progress;
          Check((LProgress.Phase = nrtDelivered) and (LProgress.BytesReceived > 24),
            'complete actual HTTP body delivers a terminal byte count');

          if FStage = 0 then
          begin
            Controls('Keep creating 🌙', 'Project name 🌙');

            if not Checkpoint('initial') then
            begin
              Exit;
            end;
            FDeliveryCase := Low(TNyxTestDeliveryCase);
            FStage := 6;
          end
          else
          begin
            Controls('Back to creating 🌙', 'A refreshed project 🌙');

            if not Checkpoint('recovered') then
            begin
              Exit;
            end;
            BeginHeld;
            FStage := 4;
          end;
        end;
      6:
        begin

          if (FDeliveryCase in [ndcHTTPS, ndcInvalidTLS]) and (FSecureURL = '') then
          begin
            { Ordinary existing invocations still run every local transport case;
              optional public HTTPS evidence is reported separately by the gate. }
            BeginHeld;
            FStage := 1;
            Exit;
          end;

          if not Checkpoint('transport-' + NyxTestDeliveryName(FDeliveryCase)) then
          begin
            Exit;
          end;
          BeginDelivery;
          FStage := 7;
        end;
      7:
        begin

          if FDeliveryCase in [ndcHTTPS, ndcInvalidTLS] then
          begin
            LStatus := FResources.Status(NyxResourceRef('secure-copy'), NyxDefaultLocale);

            if FDeliveryCase = ndcInvalidTLS then
            begin
              LStatus := FResources.Status(NyxResourceRef('secure-copy'), NyxLocale('en-GB'));
            end;
          end
          else
          begin
            LStatus := FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale);
          end;

          if LStatus.Phase <> nrpReady then
          begin
            Exit;
          end;
          CheckDelivery;

          if not Checkpoint('observed-' + NyxTestDeliveryName(FDeliveryCase)) then
          begin
            Exit;
          end;

          if FDeliveryCase = High(TNyxTestDeliveryCase) then
          begin
            FResources.Localize(NyxDefaultLocale, NyxDefaultLocale);
            BeginHeld;
            FStage := 1;
          end
          else
          begin
            FDeliveryCase := TNyxTestDeliveryCase(Ord(FDeliveryCase) + 1);
            FStage := 6;
          end;
        end;
      1, 4:
        begin
          LProgress := FTransport.LastProgress.Progress;

          if LProgress.BytesReceived = 0 then
          begin
            Exit;
          end;

          if FStage = 1 then
          begin
            Controls('Keep creating 🌙', 'Project name 🌙');
          end
          else
          begin
            Controls('Back to creating 🌙', 'A refreshed project 🌙');
          end;
          RetainTerminal;

          if FStage = 1 then
          begin
            FStage := 2;
          end
          else
          begin
            FRetiredAt := Milliseconds;
            FStage := 5;
          end;
        end;
      2:
        begin
          CheckTerminal;
          Controls('Keep creating 🌙', 'Project name 🌙');
          {$ifndef PAS2JS}

          if FServer.ClosedBodies < 1 then
          begin
            Exit;
          end;
          {$endif}

          if not Checkpoint('cancelled') then
          begin
            Exit;
          end;
          {$ifndef PAS2JS}
          FServer.ArmNext(False, True);
          {$endif}
          FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
          FStage := 3;
        end;
      5:
        begin
          CheckTerminal;
          {$ifndef PAS2JS}

          if FServer.ClosedBodies < 2 then
          begin
            Exit;
          end;

          { A retained pool legitimately keeps idle threads. Job retirement
            requires one coherent snapshot's Running/Pending counts, not total
            thread disappearance before its owner requests Shutdown. }
          LWorkers := (FScheduler as INyxSchedulerMonitor).WorkerLoad;

          if (LWorkers.Running > 0) or (LWorkers.Pending > 0) then
          begin
            Exit;
          end;
          Check((LWorkers.Running = 0) and (LWorkers.Pending = 0),
            'actual cancelled native I/O jobs retire before the fixture completes');
          {$endif}

          if Milliseconds - FRetiredAt < 150 then
          begin
            Exit;
          end;

          if not Checkpoint('retired') then
          begin
            Exit;
          end;
          Finish;
        end;
    end;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
      window.clearInterval(FTimer);
      FFinished := True;
      {$else}
      raise;
      {$endif}
    end;
  end;
end;

procedure TJourney.Finish;
begin
  FFinished := True;
  {$ifdef PAS2JS}
  window.clearInterval(FTimer);
  document.body.setAttribute('data-stream-checks', IntToStr(FChecks));
  document.body.setAttribute('data-test-result', 'passed');
  {$else}
  WriteLn('PASS / actual stream application / ', FChecks, ' checks / closed bodies ', FServer.ClosedBodies);
  {$endif}
end;

destructor TJourney.Destroy;
{$ifndef PAS2JS}
var
  LStarted: QWord;
{$endif}
begin

  if FSubscription <> nil then
  begin
    FSubscription.Disconnect;
  end;
  FApplication.Free;
  FResources := nil;
  FDocument.Free;

  if FScheduler <> nil then
  begin
    FScheduler.Shutdown;
    {$ifndef PAS2JS}
    LStarted := GetTickCount64;
    while ((FScheduler as INyxSchedulerMonitor).WorkerLoad.ActiveWorkers > 0) and
      (GetTickCount64 - LStarted < 2000) do
    begin
      CheckSynchronize(0);
      Application.ProcessMessages;
      Sleep(1);
    end;
    {$endif}
  end;
  FCancelled := nil;
  FTransportLease := nil;
  {$ifndef PAS2JS}
  FServer.Free;
  {$endif}
  inherited Destroy;
end;

var
  GJourney: TJourney;
begin
  {$ifndef PAS2JS}
  Application.Initialize;
  {$endif}
  GJourney := TJourney.Create;
  try
    GJourney.Start;
    {$ifndef PAS2JS}
    try
      while not GJourney.Finished do
      begin
        CheckSynchronize(0);
        Application.ProcessMessages;
        GJourney.Next;
        Sleep(1);
      end;
    finally
      FreeAndNil(GJourney);
      CheckSynchronize(0);
      Application.ProcessMessages;
    end;
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
      {$else}
      FreeAndNil(GJourney);
      WriteLn('FAIL / ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
