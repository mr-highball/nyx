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

program nyx_resource_policy_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.model, nyx.controls, nyx.codec,
  nyx.resources, nyx.resource.sources, nyx.resource.cache, nyx.resource.context,
  nyx.resources.loader, nyx.application.resources, nyx.scheduler,
  nyx.test.resource.stream, nyx.test.resource.policy,
  {$ifdef PAS2JS}
  JS, Web, nyx.application.browser, nyx.resources.http.browser
  {$else}
  Interfaces, Forms, StdCtrls, Graphics, IntfGraphics, FPWritePNG,
  nyx.application.lcl, nyx.resources.http.lcl
  {$endif};

type
  TPolicyApplication = {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif};
  { One receiver owns each independent document/application pair. Real platform
    transport and real clocks supply bytes and cache age. The HTTP driver only
    arms fixed replies and verifies its independent count; it never edits a
    control, substitutes a host API or mutates an editor project. }
  TJourney = class
  private
    FApplication: TPolicyApplication;
    FDocument: TNyxDocument;
    FResources: INyxApplicationResources;
    FSubscription: INyxResourceSubscription;
    FScheduler: INyxScheduler;
    FURL: TNyxText;
    FBefore: TNyxText;
    FWarm: TNyxText;
    FCase: TNyxTestPolicyCase;
    FPlan: TNyxTestPolicyPlan;
    FStage: Integer;
    FChanges: Integer;
    FChecks: Integer;
    FStarted: Double;
    FFinished: Boolean;
    {$ifdef PAS2JS}
    FHost: TJSHTMLElement;
    FCaptionIdentity: TJSHTMLElement;
    FInputIdentity: TJSHTMLElement;
    FTimer: NativeInt;
    {$else}
    FServer: TNyxResourceStreamFixture;
    FBaseRequests: Integer;
    FCaptionIdentity: TObject;
    FInputIdentity: TObject;
    {$endif}
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Changed(const AContext: INyxResourceContext);
    procedure BeginCase;
    procedure StopCase;
    function Checkpoint(const AMoment: TNyxText): Boolean;
    procedure CheckControls(const ACaption, APrompt: TNyxText);
    procedure CheckRequests(AExpected: Integer);
    procedure Finish;
  public
    { Start creates one owned producer natively, or uses the driver's explicit
      capability URL in the browser. The program drives Next on its UI loop. }
    procedure Start;
    procedure Next;
    { Disconnect callbacks before receiver retirement; dispose hosts before
      their documents and join only this qualification producer/scheduler. }
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

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(FChecks);

  if not ACondition then
  begin
    raise Exception.Create(String(FPlan.Title + ': ' + AReason));
  end;
end;

procedure TJourney.Changed(const AContext: INyxResourceContext);
begin
  Inc(FChanges);
end;

procedure TJourney.StopCase;
begin

  if FSubscription <> nil then
  begin
    FSubscription.Disconnect;
    FSubscription := nil;
  end;
  FreeAndNil(FApplication);
  FResources := nil;
  FreeAndNil(FDocument);
  FCaptionIdentity := nil;
  FInputIdentity := nil;
end;

procedure TJourney.BeginCase;
var
  LPage: INyxColumn;
  LTransport: INyxResourceTransport;
begin
  FPlan := NyxTestPolicyPlan(FCase);
  FChanges := 0;
  FDocument := TNyxDocument.Create;
  FDocument.Title := 'Hosted resource workshop';
  FDocument.Resources.Define(NyxResourceRef('copy'),
    NyxHostedResource(nrkJSON, NyxResourceURL(FURL))
      .Tagged(NyxResourceLabel('Workshop')).Tagged(NyxResourceLabel('Copy'))
      .Cache(FPlan.Policy)
      .Fallback(NyxJSONResource('{"headline":"Ready while loading","prompt":"Local project name"}'))
      .Describe('Workshop copy', 'A shared caption and project prompt.'));
  LPage := NewNyxColumn('home');
  LPage.Configure.Padding(24).Gap(16).Done;
  FDocument.AddPage(LPage);
  LPage.Add(NewNyxHeading('workshop-title').Configure.Text('Keep your workspace close').Done);
  LPage.Add(NewNyxLabel('policy-title').Configure.Text(FPlan.Title).Done);
  LPage.Add(NewNyxLabel('caption').Binds.Text(NyxResourceValue(NyxResourceRef('copy'))
    .Field('headline')).Done);
  LPage.Add(NewNyxInput('project-name').Binds.Placeholder(NyxResourceValue(NyxResourceRef('copy'))
    .Field('prompt')).Done);
  FBefore := TNyxCodec.Encode(FDocument);
  {$ifdef PAS2JS}
  LTransport := NewNyxBrowserResourceTransport;
  {$else}
  LTransport := NewNyxNativeResourceTransport(FScheduler);
  FBaseRequests := FServer.Requests;
  {$endif}
  FApplication := TPolicyApplication.Create;
  FApplication.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand)
    .Request(NyxResourceLoadOptions.WholeRequest(3000)), NewNyxResourceResolver(LTransport));
  {$ifdef PAS2JS}
  FApplication.Run(FDocument, FHost);
  FCaptionIdentity := FApplication.View.ElementFor('caption');
  FInputIdentity := FApplication.View.InputFor('project-name');
  document.body.setAttribute('data-policy-case', IntToStr(Ord(FCase)));
  {$else}
  FApplication.Mount(FDocument);
  FApplication.Window.Show;
  FCaptionIdentity := FApplication.View.ControlFor('caption');
  FInputIdentity := FApplication.View.InputFor('project-name');
  {$endif}
  FResources := FApplication.Resources;
  FSubscription := FResources.Subscribe(nil, Changed);
  CheckControls('Ready while loading', 'Local project name');
end;

function TJourney.Checkpoint(const AMoment: TNyxText): Boolean;
{$ifndef PAS2JS}
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LPath: String;
{$endif}
begin
  {$ifdef PAS2JS}
  document.body.setAttribute('data-policy-moment', AMoment);
  document.body.setAttribute('data-capture-checkpoint', IntToStr(Ord(FCase)) + '-' + AMoment);
  Result := document.body.getAttribute('data-capture-observed') =
    document.body.getAttribute('data-capture-checkpoint');
  {$else}
  Result := True;

  if (FCase in [npcStale, npcExpired, npcOverrideNoStore]) and
    (AMoment <> 'start') and (AMoment <> 'warm') then
  begin
    LPath := ParamStr(1) + '.' + IntToStr(Ord(FCase)) + '.' + String(AMoment) + '.png';
    Check(not FileExists(LPath), 'selective capture requires a fresh owned path');
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
  {$endif}
end;

procedure TJourney.CheckControls(const ACaption, APrompt: TNyxText);
var
  LDefinition: INyxResourceDefinition;
begin
  Check(TNyxCodec.Encode(FDocument) = FBefore, 'runtime loading preserves complete authored defaults');
  {$ifdef PAS2JS}
  Check(FApplication.View.ElementFor('caption') = FCaptionIdentity, 'caption identity remains mounted');
  Check(FApplication.View.InputFor('project-name') = FInputIdentity, 'prompt identity remains mounted');
  Check(FCaptionIdentity.textContent = ACaption, 'actual caption retains exact Unicode');
  Check(TJSHTMLInputElement(FInputIdentity).placeholder = APrompt, 'actual prompt retains exact Unicode');
  {$else}
  Check(FApplication.View.ControlFor('caption') = FCaptionIdentity, 'caption identity remains mounted');
  Check(FApplication.View.InputFor('project-name') = FInputIdentity, 'prompt identity remains mounted');
  Check(TNyxText(RawByteString(TLabel(FCaptionIdentity).Caption)) = ACaption, 'actual caption retains exact Unicode');
  Check(TNyxText(RawByteString(TEdit(FInputIdentity).TextHint)) = APrompt, 'actual prompt retains exact Unicode');
  {$endif}
  LDefinition := FResources.Context.Snapshot.Resolve(NyxResourceRef('copy'),
    NyxDefaultLocale, NyxDefaultLocale);
  Check((LDefinition.Title = 'Workshop copy') and
    NyxResourceLabelsOf(LDefinition).Contains(NyxResourceLabel('Workshop')) and
    NyxResourceLabelsOf(LDefinition).Contains(NyxResourceLabel('Copy')), 'current creator metadata remains owned');
end;

procedure TJourney.CheckRequests(AExpected: Integer);
begin
  {$ifndef PAS2JS}
  Check(FServer.Requests - FBaseRequests = AExpected, 'producer proves actual network request count');
  {$endif}
end;

procedure TJourney.Start;
var
  LEntry: TNyxResourceCacheEntry;
  LPolicy: TNyxResourceCachePolicy;
begin
  FStarted := Milliseconds;
  { Exact scalar boundary checks accompany the real HTTP journey. Round-trip
    an already exhausted envelope so persisted Age cannot renew stale content. }
  LPolicy := NyxResourceCache.Memory.FreshFor(600).StaleFor(30);
  LEntry := NyxResourceCacheEntry(NyxResourceURL('https://example.com/copy.json'),
    NyxJSONResource('{"headline":"Cached"}'), 1000,
    NyxResourceCacheHeaders('max-age=60', '59'));
  Check(LEntry.StateAt(LPolicy, 1000) = rcsFresh, 'server age leaves one fresh second');
  Check(LEntry.StateAt(LPolicy, 1001) = rcsStale, 'exact fresh boundary enters allowed stale state');
  LEntry := NyxResourceCacheEntry(LEntry.URL, LEntry.Definition, 1000,
    NyxResourceCacheHeaders('max-age=60', '61'));
  Check(LEntry.StateAt(LPolicy, 1000) = rcsStale, 'one already stale second consumes its allowance');
  Check(LEntry.StateAt(LPolicy, 1028.5) = rcsStale, 'remaining stale allowance is usable before its boundary');
  Check(LEntry.StateAt(LPolicy, 1029) = rcsExpired, 'exact consumed stale boundary expires');
  LEntry := NyxResourceCacheEntry(LEntry.URL, LEntry.Definition, 1000,
    NyxResourceCacheHeaders('max-age=60', '90'));
  Check(LEntry.StateAt(LPolicy, 1000) = rcsExpired, 'receipt cannot renew an exhausted stale allowance');
  LEntry := TNyxResourceCacheEntry.FromData(LEntry.ToData);
  Check(LEntry.StateAt(LPolicy, 1000) = rcsExpired, 'persisted envelope retains consumed age');
  Check(LEntry.StateAt(LPolicy.ServerPolicy(rcspOverride), 1000) = rcsFresh,
    'explicit override keeps the caller freshness choice');
  {$ifdef PAS2JS}
  Check(Copy(window.location.search, 1, 10) = '?resource=', 'driver supplies an explicit capability URL');
  FURL := Copy(window.location.search, 11, MaxInt);
  FHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(FHost);
  {$else}
  Check(ParamCount = 1, 'supply a fresh owned evidence prefix');
  FServer := TNyxResourceStreamFixture.Create('');
  FURL := FServer.URL;
  FScheduler := NewNyxScheduler;
  {$endif}
  FCase := Low(TNyxTestPolicyCase);
  BeginCase;
  {$ifdef PAS2JS}
  FTimer := window.setInterval(@Next, 20);
  {$endif}
end;

procedure TJourney.Next;
var
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
    Check(Milliseconds - FStarted < 30000, 'all policy consumers finish within thirty seconds');
    {$ifndef PAS2JS}
    Check(FServer.Error = '', 'actual producer reports no socket failure');
    {$endif}

    if FStage = 9 then
    begin
      {$ifndef PAS2JS}
      LWorkers := (FScheduler as INyxSchedulerMonitor).WorkerLoad;

      if (LWorkers.Running <> 0) or (LWorkers.Pending <> 0) then
      begin
        Exit;
      end;
      Check((LWorkers.Running = 0) and (LWorkers.Pending = 0), 'all actual native jobs retire');
      {$endif}
      Finish;
      Exit;
    end;
    case FStage of
      0:
        begin

          if not Checkpoint('start') then
          begin
            Exit;
          end;
          {$ifndef PAS2JS}
          FServer.ArmPolicy(FPlan.Reply);
          {$endif}
          FStage := 1;
          FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
        end;
      1:
        begin
          LStatus := FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale);

          if LStatus.Phase <> nrpReady then
          begin
            Exit;
          end;
          Check(LStatus.Origin = rloNetwork, 'first actual load reaches HTTP');
          Check((LStatus.CacheWrite = rcuMemory) = FPlan.Writes, 'server/caller policy governs first storage');
          Check((LStatus.Error = '') and (LStatus.CacheWarning = ''), 'healthy network load has no hidden failure');
          CheckControls('Keep creating 🌙', 'Project name 🌙');
          CheckRequests(1);
          FWarm := FResources.Context.Snapshot.ToData.ToJSON;
          FStage := 2;
        end;
      2:
        begin

          if not Checkpoint('warm') then
          begin
            Exit;
          end;
          {$ifndef PAS2JS}
          FServer.ArmPolicy(ntrUnavailable);
          {$endif}
          FStage := 3;
          FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
        end;
      3:
        begin
          LStatus := FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale);

          if LStatus.Phase <> nrpReady then
          begin
            Exit;
          end;
          {$ifdef PAS2JS}
          document.body.setAttribute('data-policy-origin', IntToStr(Ord(LStatus.Origin)));
          document.body.setAttribute('data-policy-expected', IntToStr(Ord(FPlan.Expected)));
          document.body.setAttribute('data-policy-error', LStatus.Error);
          {$else}
          WriteLn('Observed policy / ', FPlan.Title, ' / origin ', Ord(LStatus.Origin),
            ' / expected ', Ord(FPlan.Expected));
          {$endif}
          Check(LStatus.Origin = FPlan.Expected, 'unavailable reload honors the exact expected policy origin');
          Check(LStatus.CacheWarning = '', 'policy result has no fabricated storage failure');

          if FPlan.Expected = rloFallback then
          begin
            CheckControls('Ready while loading', 'Local project name');
            Check(LStatus.CacheRead = rcuNone, 'fallback reports no cache admission');
          end
          else
          begin
            CheckControls('Keep creating 🌙', 'Project name 🌙');
            Check(FResources.Context.Snapshot.ToData.ToJSON = FWarm, 'cache reuse retains the complete accepted catalog');
            Check(LStatus.CacheRead = rcuMemory, 'cache reuse reports actual memory read');
          end;

          if FPlan.Expected = rloFreshCache then
          begin
            Check(LStatus.Error = '', 'fresh hit never fabricates a network failure');
            CheckRequests(1);
          end
          else
          begin
            Check(Pos('503', LStatus.Error) > 0, 'fallback/stale retains actual unavailable-host diagnostic');
            CheckRequests(2);
          end;
          FStage := 4;
        end;
      4:
        begin

          if not Checkpoint('decision') then
          begin
            Exit;
          end;

          if FPlan.Expected = rloFreshCache then
          begin
            FStage := 6;
          end
          else
          begin
            {$ifndef PAS2JS}
            FServer.ArmPolicy(FPlan.Reply, True);
            {$endif}
            FStage := 5;
            FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
          end;
        end;
      5:
        begin
          LStatus := FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale);

          if LStatus.Phase <> nrpReady then
          begin
            Exit;
          end;
          Check((LStatus.Origin = rloNetwork) and (LStatus.Error = ''), 'same-URL recovery reaches real HTTP');
          CheckControls('Back to creating 🌙', 'A refreshed project 🌙');
          CheckRequests(3);
          FStage := 7;
        end;
      7:
        begin

          if not Checkpoint('recovered') then
          begin
            Exit;
          end;
          FStage := 6;
        end;
      6:
        begin
          Check(FChanges > 0, 'real resource publication notified its subscriber');
          {$ifndef PAS2JS}
          WriteLn('PASS / policy / ', FPlan.Title, ' / actual requests ', FServer.Requests - FBaseRequests);
          {$endif}
          StopCase;

          if FCase = High(TNyxTestPolicyCase) then
          begin
            FStage := 9;
          end
          else
          begin
            FCase := Succ(FCase);
            FStage := 0;
            BeginCase;
          end;
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
  document.body.setAttribute('data-policy-checks', IntToStr(FChecks));
  document.body.setAttribute('data-test-result', 'passed');
  {$else}
  WriteLn('PASS / actual hosted policy application / ', FChecks, ' checks / requests ', FServer.Requests);
  {$endif}
end;

destructor TJourney.Destroy;
{$ifndef PAS2JS}
var
  LStarted: QWord;
{$endif}
begin
  StopCase;

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
