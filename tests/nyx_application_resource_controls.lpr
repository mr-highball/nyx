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

program nyx_application_resource_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils,
  Classes,
  nyx.text,
  nyx.bytes,
  nyx.types,
  nyx.model,
  nyx.controls,
  nyx.state,
  nyx.codec,
  nyx.binding.types,
  nyx.resources,
  nyx.resource.sources,
  nyx.resource.cache,
  nyx.resources.loader,
  nyx.application.resources,
  nyx.resource.context,
  {$ifdef PAS2JS}
  JS, Web, nyx.application.browser
  {$else}
  Interfaces, Forms, StdCtrls, nyx.application.lcl
  {$endif};

type
  TTestApplication = {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif};
  TReplyRequest = class(TNyxResourceRequest)
  public
    procedure Reply(const AValue: TNyxText);
  end;
  { The real resolver handles byte/kind/cache admission. This deterministic
    transport exposes retained pending tokens to qualify application receiver
    retirement and concurrency without a new HTTP service/listener. }
  TTransport = class(TInterfacedObject, INyxResourceTransport)
  private
    FTokens: array of INyxResourceRequest;
    FRequests: array of TReplyRequest;
  public
    Calls: Integer;
    CompleteInline: Boolean;
    Value: TNyxText;
    function Request(const AURL: TNyxResourceURL;
      const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
    procedure Send(AIndex: Integer; const AValue: TNyxText);
  end;

  TJourney = class
  private
    FDocument: TNyxDocument;
    FBefore: TNyxText;
    FApplication: TTestApplication;
    FOther: TTestApplication;
    FTransport: TTransport;
    FTransportLease: INyxResourceTransport;
    FResources: INyxApplicationResources;
    FRetained: INyxResourceContext;
    FToken: INyxResourceSubscription;
    FRejectToken: INyxResourceSubscription;
    FBusy: Boolean;
    FChanged: Integer;
    FStage: Integer;
    FChecks: Integer;
    FStartedAt: {$ifdef PAS2JS}Double{$else}QWord{$endif};
    FFinished: Boolean;
    procedure StartConcurrency;
    procedure StartHosted;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    function Ready: Boolean;
    function Validate(const AContext: INyxResourceContext): Boolean;
    procedure Changed(const AContext: INyxResourceContext);
    procedure FailedObserver(const AContext: INyxResourceContext);
    procedure RetireHost(const AContext: INyxResourceContext);
    function Caption(const AID: TNyxText): TNyxText;
    function Prompt(const AID: TNyxText): TNyxText;
    procedure Mount(AApplication: TTestApplication);
    procedure Finish;
  public
    destructor Destroy; override;
    procedure Start;
    procedure Next;
    property Finished: Boolean read FFinished;
  end;

function BuildWorkshop: TNyxDocument;
var
  LHome: INyxColumn;
  LDetails: INyxColumn;
  LCard: INyxColumn;
  LCount: INyxSlider;
  LCopy: TNyxResourceRef;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Resource workshop';
    LCopy := NyxResourceRef('copy');
    Result.Resources.Define(LCopy,
      NyxHostedResource(nrkJSON, NyxResourceURL('https://example.com/workshop.json'))
        .Cache(NyxResourceCache.Bypass)
        .Fallback(NyxJSONResource('{"headline":"Ready to create","prompt":"Project name","detail":"English defaults","maximum":10}')));
    Result.Resources.Define(LCopy, NyxLocale('en-GB'),
      NyxJSONResource('{"headline":"Your workbench","prompt":"Programme name","detail":"Shared English copy","maximum":30}'));
    Result.Resources.Define(LCopy, NyxLocale('unicode'),
      NyxJSONResource('{"headline":"Moon 🌙","prompt":"Café 🌙","detail":"Exact Unicode 🌙","maximum":30}'));
    Result.Resources.Define(LCopy, NyxLocale('incomplete'),
      NyxJSONResource('{"headline":"Incomplete","prompt":"Missing detail","maximum":30}'));
    Result.State.SetValue(NyxIntegerState('count'), 2);
    LCard := NewNyxColumn('workshop-card');
    LCard.Add(NewNyxLabel('headline').Binds.Text(NyxResourceValue(LCopy).Field('headline')).Done);
    LCard.Add(NewNyxInput('name').Binds.Placeholder(NyxResourceValue(LCopy).Field('prompt')).Done);
    Result.AddComponent(LCard);
    LHome := NewNyxColumn('home');
    LHome.Configure.Padding(24).Gap(16).Done;
    LHome.Add(NewNyxComponent('first-card').Configure.Component(NyxComponent('workshop-card')).Done);
    Result.AddPage(LHome);
    LDetails := NewNyxColumn('details');
    LDetails.Add(NewNyxLabel('detail').Binds.Text(NyxResourceValue(LCopy).Field('detail')).Done);
    LCount := NewNyxSlider('count');
    LCount.Configure.Minimum(0).Maximum(10).Done;
    LCount.Binds.Value(NyxIntegerState('count'))
      .Maximum(NyxResourceValue(LCopy).Field('maximum').AsInteger).Done;
    LDetails.Add(LCount);
    LDetails.Add(NewNyxComponent('second-card').Configure.Component(NyxComponent('workshop-card')).Done);
    Result.AddPage(LDetails);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure TReplyRequest.Reply(const AValue: TNyxText);
var
  LResponse: TNyxResourceHTTPResult;
begin
  LResponse := Default(TNyxResourceHTTPResult);
  LResponse.Status := 200;
  LResponse.Bytes := NyxEncodeUTF8(AValue);
  LResponse.Hints := NyxResourceCacheHeaders('', '');
  Complete(LResponse);
end;

function TTransport.Request(const AURL: TNyxResourceURL;
  const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
var
  LRequest: TReplyRequest;
  LIndex: Integer;
begin
  LRequest := TReplyRequest.Create(AReply);
  Result := LRequest;
  LIndex := Length(FTokens);
  SetLength(FTokens, LIndex + 1);
  SetLength(FRequests, LIndex + 1);
  FTokens[LIndex] := Result;
  FRequests[LIndex] := LRequest;
  Inc(Calls);

  if CompleteInline then
  begin
    LRequest.Reply(Value);
  end;
end;

procedure TTransport.Send(AIndex: Integer; const AValue: TNyxText);
begin
  FRequests[AIndex].Reply(AValue);
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxResource.Create('Application resources: ' + AReason);
  end;
  Inc(FChecks);
end;

function TJourney.Validate(const AContext: INyxResourceContext): Boolean;
begin
  Result := not FBusy;
end;

procedure TJourney.Changed(const AContext: INyxResourceContext);
begin
  Inc(FChanged);
end;

procedure TJourney.FailedObserver(const AContext: INyxResourceContext);
begin
  raise ENyxResource.Create('Observer 🌙 failed after publication');
end;

procedure TJourney.RetireHost(const AContext: INyxResourceContext);
begin
  FreeAndNil(FApplication);
end;

function TJourney.Ready: Boolean;
begin
  Result := FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale).Phase in
    [nrpReady, nrpFailed, nrpRejected, nrpCancelled];
end;

function TJourney.Caption(const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := FApplication.View.ElementFor(AID).textContent;
  {$else}
  Result := TNyxText(RawByteString(TLabel(FApplication.View.ControlFor(AID)).Caption));
  {$endif}
end;

function TJourney.Prompt(const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(FApplication.View.InputFor(AID)).placeholder;
  {$else}
  Result := TNyxText(RawByteString(TEdit(FApplication.View.InputFor(AID)).TextHint));
  {$endif}
end;

procedure TJourney.Mount(AApplication: TTestApplication);
{$ifdef PAS2JS}
var
  LHost: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  AApplication.Run(FDocument, LHost);
  {$else}
  AApplication.Mount(FDocument);
  AApplication.Window.Show;
  {$endif}
end;

procedure TJourney.Start;
var
  LResolver: INyxResourceResolver;
begin
  FDocument := BuildWorkshop;
  FBefore := TNyxCodec.Encode(FDocument);
  FTransport := TTransport.Create;
  FTransportLease := FTransport;
  LResolver := NewNyxResourceResolver(FTransportLease);
  FApplication := TTestApplication.Create;
  FApplication.ConfigureResources(NyxApplicationResourceOptions, LResolver);
  Mount(FApplication);
  FResources := FApplication.Resources;
  FToken := FResources.Subscribe(Validate, Changed);
  Check((FTransport.Calls = 0) and (Caption('first-card/headline') = 'Ready to create'),
    'automatic requests are deferred until after complete mount');
  Check(Prompt('first-card/name') = 'Project name', 'authored prompt paints before loading');
  FOther := TTestApplication.Create;
  FOther.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand), LResolver);
  Mount(FOther);
  Check(FOther.Resources.Status(NyxResourceRef('copy'), NyxDefaultLocale).Phase = nrpIdle,
    'on-demand host keeps declarations without fetching');
  FStage := 1;
  {$ifdef PAS2JS}
  FStartedAt := TJSDate.now;
  window.setTimeout(@Next, 10);
  {$else}
  FStartedAt := GetTickCount64;
  {$endif}
end;

procedure TJourney.Next;
var
  LStatus: TNyxApplicationResourceStatus;
  LRefused: Boolean;
  LPrevious: Integer;
begin
  try

    if FFinished then
    begin
      Exit;
    end;
    {$ifdef PAS2JS}
    window.setTimeout(@Next, 10);
    if TJSDate.now - FStartedAt > 20000 then
    {$else}
    if GetTickCount64 - FStartedAt > 20000 then
    {$endif}
    begin
      raise ENyxResource.Create('Application resource journey timed out');
    end;
    case FStage of
      1:
        begin

          if FTransport.Calls < 1 then
          begin
            Exit;
          end;
          Check(FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale).Phase = nrpLoading,
            'typed loading status precedes publication');
          FApplication.ShowPage('details');
          FTransport.Send(0, '{"headline":"Loaded workshop","prompt":"Your next project","detail":"Loaded details","maximum":20}');
          FStage := 2;
        end;
      2:
        begin

          if not Ready then
          begin
            Exit;
          end;
          LStatus := FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale);
          Check((LStatus.Phase = nrpReady) and (LStatus.Origin = rloNetwork),
            'application accepts loaded catalog with typed network origin');
          Check(Caption('detail') = 'Loaded details', 'current page receives completion after navigation');
          Check(Caption('second-card/headline') = 'Loaded workshop', 'reusable consumer receives application data');
          FApplication.ShowPage('home');
          Check((Caption('first-card/headline') = 'Loaded workshop') and
            (Prompt('first-card/name') = 'Your next project'), 'return navigation retains loaded caption and prompt');
          Check(FApplication.View.TryRefresh(FDocument, FDocument.Pages[0], False),
            'retained arrangement accepts the same authored binding contracts');
          Check(Caption('first-card/headline') = 'Loaded workshop',
            'retained arrangement preserves accepted runtime resource values');
          Check(FResources.Declaration(NyxResourceRef('copy'), NyxDefaultLocale).Source.Kind = rskHosted,
            'loaded runtime bytes never replace retry declarations');
          Check(FOther.Resources.Context.Snapshot.Definition(NyxResourceRef('copy'),
            NyxDefaultLocale).Data.Field('headline').AsText = 'Ready to create',
            'sibling application retains independent resources');
          FApplication.State.SetValue(NyxIntegerState('count'), 15);
          Check(FApplication.State.GetValue(NyxIntegerState('count')) = 15,
            'hidden page state validation uses loaded maximum rather than saved maximum');
          FResources.Localize(NyxLocale('missing'), NyxLocale('en-GB'));
          Check((Caption('first-card/headline') = 'Your workbench') and
            (Prompt('first-card/name') = 'Programme name'), 'explicit fallback locale reaches existing controls');
          FApplication.ShowPage('details');
          Check(Caption('detail') = 'Shared English copy', 'locale survives page remount');
          FResources.Localize(NyxLocale('unicode'), NyxDefaultLocale);
          Check(Caption('detail') = TNyxText('Exact Unicode 🌙'), 'supplementary Unicode reaches actual target caption');
          FApplication.ShowPage('home');
          Check(Prompt('first-card/name') = TNyxText('Café 🌙'), 'locale prompt survives reusable remount');
          LRefused := False;
          try
            FResources.Localize(NyxLocale('incomplete'), NyxDefaultLocale);
          except
            on ENyxResource do
            begin
              LRefused := True;
            end;
          end;
          Check(LRefused and (Caption('first-card/headline') = TNyxText('Moon 🌙')),
            'missing hidden-page selector refuses locale atomically');
          FResources.Localize(NyxDefaultLocale, NyxDefaultLocale);
          FRetained := FResources.Context;
          FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
          FStage := 3;
        end;
      3:
        begin

          if FTransport.Calls < 2 then
          begin
            Exit;
          end;
          FTransport.Send(1, '{"headline":"Looks valid","prompt":"Visible fields exist","maximum":20}');
          FStage := 4;
        end;
      4:
        begin

          if not Ready then
          begin
            Exit;
          end;
          Check((FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale).Phase = nrpRejected) and
            (Caption('first-card/headline') = 'Loaded workshop'),
            'hidden-page missing path rejects entire loaded catalog');
          Check(FResources.Context.Snapshot.Definition(NyxResourceRef('copy'),
            NyxDefaultLocale).Data.Field('detail').AsText = 'Loaded details',
            'rejected completion preserves accepted hidden data');
          FTransport.CompleteInline := True;
          FTransport.Value := '{"headline":"Inline completion","prompt":"Still deferred","detail":"Inline detail","maximum":20}';
          FBusy := True;
          FResources.Reload;
          FStage := 5;
        end;
      5:
        begin

          if FTransport.Calls < 3 then
          begin
            Exit;
          end;
          LStatus := FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale);

          if LStatus.Phase <> nrpWaiting then
          begin
            Exit;
          end;
          Check(Caption('first-card/headline') = 'Loaded workshop',
            'busy receiver retains loaded result without early publication');
          FBusy := False;
          FResources.Wake;
          FStage := 6;
        end;
      6:
        begin

          if not Ready then
          begin
            Exit;
          end;
          Check(Caption('first-card/headline') = 'Inline completion',
            'explicit idle wake publishes retained synchronous resolver reply');
          Check(FRetained.Snapshot.Definition(NyxResourceRef('copy'), NyxDefaultLocale)
            .Data.Field('headline').AsText = 'Loaded workshop', 'retained immutable frame is independent');
          FTransport.CompleteInline := False;
          FResources.Reload;
          FStage := 7;
        end;
      7:
        begin

          if FTransport.Calls < 4 then
          begin
            Exit;
          end;
          FResources.Cancel;
          LPrevious := FChanged;
          FTransport.Send(3, '{"headline":"Late cancelled reply","prompt":"Wrong","detail":"Wrong","maximum":20}');
          Check((FChanged = LPrevious) and (Caption('first-card/headline') = 'Inline completion') and
            (FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale).Phase = nrpCancelled),
            'cancel disconnects pending borrowed receiver before late completion');
          FToken.Disconnect;
          FToken := nil;
          FRejectToken := FResources.Subscribe(nil, FailedObserver);
          FTransport.CompleteInline := True;
          FTransport.Value := '{"headline":"Published despite observer","prompt":"Accepted","detail":"Accepted detail","maximum":20}';
          FResources.Reload;
          FStage := 8;
        end;
      8:
        begin

          if not Ready then
          begin
            Exit;
          end;
          LStatus := FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale);
          Check((LStatus.Phase = nrpReady) and
            (LStatus.NotificationError = TNyxText('Observer 🌙 failed after publication')) and
            (Caption('first-card/headline') = 'Published despite observer'),
            'post-publication observer failure reports separately and keeps accepted controls');
          FRejectToken.Disconnect;
          FRejectToken := nil;
          FTransport.CompleteInline := False;
          FResources.Reload;
          FStage := 9;
        end;
      9:
        begin

          if FTransport.Calls < 6 then
          begin
            Exit;
          end;
          FreeAndNil(FApplication);
          FTransport.Send(5, '{"headline":"Disposed receiver","prompt":"Wrong","detail":"Wrong","maximum":20}');
          Check(FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale).Phase = nrpCancelled,
            'host destruction retires outstanding request while retained owner remains inspectable');
          LRefused := False;
          try
            FResources.Reload;
          except
            on ENyxResource do
            begin
              LRefused := True;
            end;
          end;
          Check(LRefused, 'retained stopped owner refuses new work');
          Check(TNyxCodec.Encode(FDocument) = FBefore, 'runtime loading/locales/state preserve exact saved document');
          StartConcurrency;
          FStage := 10;
        end;
      10:
        begin

          if FTransport.Calls < 8 then
          begin
            Exit;
          end;
          Check(FTransport.Calls = 8, 'automatic loading admits only two concurrent requests');
          FResources.Reload(NyxResourceRef('first'), NyxDefaultLocale);
          FTransport.Send(6, 'Retired generation');
          FStage := 11;
        end;
      11:
        begin

          if FTransport.Calls < 9 then
          begin
            Exit;
          end;
          Check((FTransport.Calls = 9) and (Caption('caption') = 'Waiting'),
            'replacement cancels exact prior receiver and retains current caption');
          FTransport.Send(7, 'Second value');
          FTransport.Send(8, 'First value');
          FStage := 12;
        end;
      12:
        begin

          if FTransport.Calls < 10 then
          begin
            Exit;
          end;
          Check(Caption('caption') = 'First value', 'replacement completion publishes actual label');
          Check(FResources.Status(NyxResourceRef('second'), NyxDefaultLocale).Phase = nrpReady,
            'independent second completion publishes before third admission');
          FTransport.Send(9, 'Third value');
          FStage := 13;
        end;
      13:
        begin

          if FResources.Status(NyxResourceRef('third'), NyxDefaultLocale).Phase <> nrpReady then
          begin
            Exit;
          end;
          Check(FTransport.Calls = 10, 'bounded queue drains every hosted variant once');
          Check(TNyxCodec.Encode(FDocument) = FBefore, 'bounded loading preserves independent saved declarations');
          {$ifndef PAS2JS}

          if ParamCount = 0 then
          begin
            Finish;
            Exit;
          end;
          {$endif}
          StartHosted;
          FStage := 14;
        end;
      14:
        begin

          if FResources.Status(NyxResourceRef('service'), NyxDefaultLocale).Phase
            in [nrpIdle, nrpQueued, nrpLoading, nrpWaiting] then
          begin
            Exit;
          end;
          Check((FResources.Status(NyxResourceRef('service'), NyxDefaultLocale).Phase = nrpReady) and
            (FResources.Status(NyxResourceRef('service'), NyxDefaultLocale).Origin = rloNetwork),
            'default application adapter automatically loads real HTTP data');
          Check((Caption('service') = 'nyx-studio-server') and (Prompt('name') = 'nyx-studio-server'),
            'automatic real hosted completion paints caption and prompt');
          FApplication.ShowPage('home');
          Check(Caption('service') = 'nyx-studio-server', 'real loaded data survives application remount');
          Check(TNyxCodec.Encode(FDocument) = FBefore, 'real automatic loading preserves authored fallback and URL');
          FToken := FResources.Subscribe(nil, RetireHost);
          FResources.Localize(NyxDefaultLocale, NyxDefaultLocale);
          Check((FApplication = nil) and
            (FResources.Context.Snapshot.Definition(NyxResourceRef('service'),
            NyxDefaultLocale).Data.Field('service').AsText = 'nyx-studio-server'),
            'a changed callback can retire its host without invalidating the current dispatch');
          Finish;
        end;
    end;

    {$ifdef PAS2JS}
    if TJSDate.now - FStartedAt > 20000 then
    {$else}
    if GetTickCount64 - FStartedAt > 20000 then
    {$endif}
    begin
      raise ENyxResource.Create('Application resource journey timed out');
    end;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-result', 'failed');
      document.body.setAttribute('data-nyx-error', LException.Message);
      {$endif}
      raise;
    end;
  end;
end;

procedure TJourney.StartHosted;
var
  LPage: INyxColumn;
  LURL: TNyxText;
begin
  FreeAndNil(FApplication);
  FResources := nil;
  FreeAndNil(FDocument);
  FDocument := TNyxDocument.Create;
  {$ifdef PAS2JS}
  LURL := window.location.origin + '/api/health';
  {$else}
  LURL := TNyxText(ParamStr(1)) + '/api/health';
  {$endif}
  FDocument.Resources.Define(NyxResourceRef('service'),
    NyxHostedResource(nrkJSON, NyxResourceURL(LURL))
      .Cache(NyxResourceCache.Bypass)
      .Fallback(NyxJSONResource('{"service":"Ready while loading"}')));
  LPage := NewNyxColumn('home');
  LPage.Add(NewNyxLabel('service').Binds.Text(
    NyxResourceValue(NyxResourceRef('service')).Field('service')).Done);
  LPage.Add(NewNyxInput('name').Binds.Placeholder(
    NyxResourceValue(NyxResourceRef('service')).Field('service')).Done);
  FDocument.AddPage(LPage);
  FBefore := TNyxCodec.Encode(FDocument);
  FApplication := TTestApplication.Create;
  Mount(FApplication);
  FResources := FApplication.Resources;
end;

procedure TJourney.StartConcurrency;
var
  LPage: INyxColumn;
  LResolver: INyxResourceResolver;
begin
  FreeAndNil(FOther);
  FResources := nil;
  FreeAndNil(FDocument);
  FDocument := TNyxDocument.Create;
  FDocument.Resources.Define(NyxResourceRef('first'),
    NyxHostedResource(nrkText, NyxResourceURL('https://example.com/first.txt'))
      .Cache(NyxResourceCache.Bypass).Fallback(NyxTextResource('Waiting')));
  FDocument.Resources.Define(NyxResourceRef('second'),
    NyxHostedResource(nrkText, NyxResourceURL('https://example.com/second.txt'))
      .Cache(NyxResourceCache.Bypass).Fallback(NyxTextResource('Waiting')));
  FDocument.Resources.Define(NyxResourceRef('third'),
    NyxHostedResource(nrkText, NyxResourceURL('https://example.com/third.txt'))
      .Cache(NyxResourceCache.Bypass).Fallback(NyxTextResource('Waiting')));
  LPage := NewNyxColumn('home');
  LPage.Add(NewNyxLabel('caption').Binds.Text(NyxResourceValue(NyxResourceRef('first'))).Done);
  FDocument.AddPage(LPage);
  FBefore := TNyxCodec.Encode(FDocument);
  LResolver := NewNyxResourceResolver(FTransportLease);
  FApplication := TTestApplication.Create;
  FApplication.ConfigureResources(NyxApplicationResourceOptions.ConcurrentRequests(2)
    .Request(NyxResourceLoadOptions.WholeRequest(2500)), LResolver);
  Mount(FApplication);
  FResources := FApplication.Resources;
end;

procedure TJourney.Finish;
begin
  FFinished := True;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-nyx-result', 'passed');
  document.body.setAttribute('data-nyx-checks', IntToStr(FChecks));
  {$else}
  WriteLn('PASS / application resource controls / ', FChecks, ' checks');
  {$endif}
end;

destructor TJourney.Destroy;
begin

  if FToken <> nil then
  begin
    FToken.Disconnect;
  end;

  if FRejectToken <> nil then
  begin
    FRejectToken.Disconnect;
  end;
  FApplication.Free;
  FOther.Free;
  FResources := nil;
  FRetained := nil;
  FDocument.Free;
  inherited Destroy;
end;

var
  GJourney: TJourney;

begin
  {$ifndef PAS2JS}
  Application.Initialize;
  {$endif}
  GJourney := TJourney.Create;
  {$ifdef PAS2JS}
  GJourney.Start;
  {$else}
  try
    try
      GJourney.Start;
      while not GJourney.Finished do
      begin
        CheckSynchronize(0);
        Application.ProcessMessages;
        GJourney.Next;
        Sleep(1);
      end;
    finally
      GJourney.Free;
      { Cancelled native UI couriers still retire through the message queue.
        Drain their revoked ports after hosts and receivers have been freed. }
      CheckSynchronize(0);
      Application.ProcessMessages;
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / application resources / ', LException.Message);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
