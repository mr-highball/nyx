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
  nyx.data,
  nyx.codegen,
  nyx.studio.agents,
  nyx.studio.projects,
  nyx.studio.session,
  nyx.studio.agentview,
  nyx.studio.view,
  nyx.studio.runtimeclient,
  nyx.resources.runtime.view,
  nyx.binding.types,
  nyx.resources,
  nyx.resource.sources,
  nyx.resource.cache,
  nyx.resources.loader,
  nyx.application.resources,
  nyx.resource.context,
  {$ifdef PAS2JS}
  JS, Web, nyx.application.browser, nyx.render.browser
  {$else}
  Interfaces, Forms, StdCtrls, Controls, Graphics, IntfGraphics, FPWritePNG, LCLIntf,
  nyx.application.lcl, nyx.render.lcl
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
    FRuntimeAgent: TNyxAgentSession;
    FObservation: TNyxStudioResourceObservation;
    FInitialRuntime: INyxResourceRuntimeSnapshot;
    function RuntimePage: TNyxDataValue;
    procedure ReportRuntime;
    procedure StartObservation;
    procedure CheckObservationRetirement;
    procedure ShowRuntimeObserver;
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
  { Locale-only notifications also use this receiver after the first load. The
    installed-load fact must already agree with the published runtime context. }
  Check(NyxApplicationResourceDiagnostics(FResources).CaptureRuntime.Entry(0).HasPublishedLoad,
    'resource Changed callback sees installed-load evidence before notification');
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

{ Exercise the actual application owner, copied trusted-host publication and
  normal semantic dispatcher together. This is an in-process observing journey;
  HTTP authentication and cross-process enrollment need their separate rollout. }
function TJourney.RuntimePage: TNyxDataValue;
var
  LReports: TNyxDataValue;
  LSequence: Integer;
begin
  LReports := FRuntimeAgent.Call('nyx_resources', 'Runtime reviewer', NyxObject([
    NyxField('mode', NyxData('runtimes')),
    NyxField('expectedRevision', NyxData(FRuntimeAgent.Revision))]));
  LSequence := LReports.Field('items').Item(0).Field('sequence').AsInteger;
  Result := FRuntimeAgent.Call('nyx_resources', 'Runtime reviewer', NyxObject([
    NyxField('mode', NyxData('runtime')), NyxField('expectedRevision', NyxData(FRuntimeAgent.Revision)),
    NyxField('run', NyxData('workshop-run')), NyxField('expectedSequence', NyxData(LSequence)),
    NyxField('limit', NyxData(1))])).Field('page');
end;

procedure TJourney.ReportRuntime;
var
  LOriginal: INyxResourceRuntimeSnapshot;
  LDecoded: INyxResourceRuntimeSnapshot;
begin
  LOriginal := NyxApplicationResourceDiagnostics(FResources).CaptureRuntime;
  LDecoded := DecodeNyxResourceRuntime(EncodeNyxResourceRuntime(LOriginal), FDocument.Resources);
  Check((LDecoded.Page(0, 1).ToJSON = LOriginal.Page(0, 1).ToJSON) and
    LDecoded.MatchesDeclarations(FDocument.Resources),
    'private runtime wire preserves actual control load/cache and exact variant declarations');
  FRuntimeAgent.PublishResourceRuntime(FObservation, LDecoded);
end;

procedure TJourney.StartObservation;
var
  LOther: TNyxAgentSession;
  LForeign: TNyxDocument;
  LLastObservation: TNyxStudioResourceObservation;
  LIndex: Integer;
  LRejected: Boolean;
  LBefore: TNyxText;
  LPage: TNyxDataValue;
begin
  FRuntimeAgent := TNyxAgentSession.Create(NyxProjectPair(FBefore, TNyxCodegen.Generate(FDocument)));
  FInitialRuntime := NyxApplicationResourceDiagnostics(FResources).CaptureRuntime;
  LBefore := EncodeNyxProject(FRuntimeAgent.ReviewSeed(FRuntimeAgent.Revision));
  FObservation := FRuntimeAgent.ObserveResourceRuntime(FRuntimeAgent.Revision,
    NyxStudioRuntime('workshop-run'), srsApplication,
    {$ifdef PAS2JS}npfBrowser{$else}npfNativeLCL{$endif}, '', FInitialRuntime);
  LPage := RuntimePage;
  Check((LPage.Field('items').Count = 1) and (LPage.Field('total').AsInteger = 4) and
    (LPage.Field('items').Item(0).Field('phase').AsText = 'queued') and
    (LPage.Field('items').Item(0).Field('publishedOrigin').Kind = ndNull),
    'bounded report distinguishes authored fallback from an uncompleted request');
  Check(EncodeNyxProject(FRuntimeAgent.ReviewSeed(FRuntimeAgent.Revision)) = LBefore,
    'runtime enrollment/query preserves exact authoring pair and history');
  LOther := FRuntimeAgent.Clone;
  try
    LOther.RetireResourceRuntime(FObservation);
    Check(FRuntimeAgent.Call('nyx_resources', 'Runtime reviewer', NyxObject([
      NyxField('mode', NyxData('runtimes')), NyxField('expectedRevision', NyxData(1))]))
      .Field('items').Item(0).Field('active').AsBoolean,
      'rollback copies retain independent runtime metadata and shared immutable snapshots');
  finally
    LOther.Free;
  end;
  LOther := TNyxAgentSession.CreateRecovered(FRuntimeAgent.RecoveryFrame);
  try
    LRejected := False;
    try
      LOther.PublishResourceRuntime(FObservation, FInitialRuntime);
    except
      on ENyxResource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'durable recovery cannot inherit runtime publication authority');
  finally
    LOther.Free;
  end;
  LRejected := False;
  try
    FRuntimeAgent.Call('nyx_resources', 'Runtime reviewer', NyxObject([
      NyxField('mode', NyxData('runtime')), NyxField('expectedRevision', NyxData(1)),
      NyxField('run', NyxData('workshop-run')), NyxField('expectedSequence', NyxData(2))]));
  except
    on ENyxResource do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'runtime query refuses a stale observation sequence');
  LRejected := False;
  try
    FInitialRuntime.Page(0, 17);
  except
    on ENyxResource do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'runtime page refuses more than sixteen entries');
  LOther := FRuntimeAgent.Clone;
  try
    for LIndex := 1 to 7 do
    begin
      LLastObservation := LOther.ObserveResourceRuntime(1,
        NyxStudioRuntime('additional-' + TNyxText(IntToStr(LIndex))), srsResources,
        {$ifdef PAS2JS}npfBrowser{$else}npfNativeLCL{$endif}, '', FInitialRuntime);
    end;
    LRejected := False;
    try
      LOther.ObserveResourceRuntime(1, NyxStudioRuntime('ninth'), srsResources,
        {$ifdef PAS2JS}npfBrowser{$else}npfNativeLCL{$endif}, '', FInitialRuntime);
    except
      on ENyxResource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'eight active observations bound runtime membership');
    LOther.RetireResourceRuntime(LLastObservation);
    LOther.ObserveResourceRuntime(1, NyxStudioRuntime('replacement'), srsResources,
      {$ifdef PAS2JS}npfBrowser{$else}npfNativeLCL{$endif}, '', FInitialRuntime);
    Check(LOther.Call('nyx_resources', 'Runtime reviewer', NyxObject([
      NyxField('mode', NyxData('runtimes')), NyxField('expectedRevision', NyxData(1))]))
      .Field('items').Count = 8, 'retired evidence yields capacity without requiring a design edit');
  finally
    LOther.Free;
  end;
  LForeign := FDocument.Clone;
  try
    LForeign.Resources.Define(NyxResourceRef('copy'), LForeign.Resources.Definition(
      NyxResourceRef('copy'), NyxDefaultLocale).Describe('Other run catalog', 'Different metadata'));
    LOther := TNyxAgentSession.Create(NyxProjectPair(TNyxCodec.Encode(LForeign),
      TNyxCodegen.Generate(LForeign)));
    try
      LRejected := False;
      try
        LOther.ObserveResourceRuntime(1, NyxStudioRuntime('foreign-run'), srsApplication,
          {$ifdef PAS2JS}npfBrowser{$else}npfNativeLCL{$endif}, '', FInitialRuntime);
      except
        on ENyxResource do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'runtime enrollment compares exact declarations including metadata');
    finally
      LOther.Free;
    end;
  finally
    LForeign.Free;
  end;
end;

procedure TJourney.CheckObservationRetirement;
var
  LSnapshot: INyxResourceRuntimeSnapshot;
  LRejected: Boolean;
  LPair: TNyxProjectPair;
begin
  LSnapshot := NyxApplicationResourceDiagnostics(FResources).CaptureRuntime;
  FRuntimeAgent.PublishResourceRuntime(FObservation, LSnapshot);
  Check(LSnapshot.Stopped and not FInitialRuntime.Stopped,
    'stopped and retained initial snapshots are independent of the retired host');
  Check(not FRuntimeAgent.Call('nyx_resources', 'Runtime reviewer', NyxObject([
    NyxField('mode', NyxData('runtimes')), NyxField('expectedRevision', NyxData(1))]))
    .Field('items').Item(0).Field('active').AsBoolean,
    'final stopped snapshot retires publication authority');
  LRejected := False;
  try
    FRuntimeAgent.PublishResourceRuntime(FObservation, FInitialRuntime);
  except
    on ENyxResource do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'a retained capability cannot revive a stopped run');
  LPair := FRuntimeAgent.ReviewSeed(1);
  FRuntimeAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
    NyxField('expectedRevision', NyxData(1)), NyxField('project', NyxData(EncodeNyxProject(LPair))),
    NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
  Check(FRuntimeAgent.Call('nyx_resources', 'Runtime reviewer', NyxObject([
    NyxField('mode', NyxData('runtimes')), NyxField('expectedRevision', NyxData(2))]))
    .Field('items').Count = 0,
    'accepted design revision revokes previous runtime context');
  LRejected := False;
  try
    FRuntimeAgent.PublishResourceRuntime(FObservation, LSnapshot);
  except
    on ENyxResource do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'paired revision invalidates the exact old publication ticket');
end;

{ Compose the real common Studio view from the engine's observing packet. This
  checks its public Nyx runtime card on target controls, rather than maintaining
  a separate diagnostic widget or interpreting an authored resource as a run. }
procedure TJourney.ShowRuntimeObserver;
var
  LSession: TNyxStudioSession;
  LState: TNyxStudioViewState;
  LShell: TNyxDocument;
  LFrame: TNyxDataValue;
  LPass: Integer;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LWindow: TForm;
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LPath: String;
  LTarget: TControl;
  LAncestor: TWinControl;
  LLocation: TPoint;
  LBounds: TRect;
  {$endif}
begin
  LFrame := FRuntimeAgent.Exchange(NyxObject([NyxField('op', NyxData('observe')),
    NyxField('after', NyxData(FRuntimeAgent.Revision)), NyxField('resource', NyxObject([
      NyxField('reference', NyxData('copy')), NyxField('locale', NyxData(''))]))]));
  LSession := TNyxStudioSession.Create(FRuntimeAgent.ReviewSeed(FRuntimeAgent.Revision));
  try
    for LPass := 0 to 1 do
    begin
      LState := DefaultNyxStudioViewState;
      LState.ResourcesVisible := True;
      LState.ResourceSelection.Reference := NyxResourceRef('copy');
      LState.ResourceSelection.Locale := NyxDefaultLocale;
      LState.CodeVisible := False;
      LState.Compact := LPass = 1;
      LState.Panel := nspProject;
      LState.Agents := DefaultNyxStudioAgentView;
      LState.Agents.CanInspectResourceRuntime := LFrame.Field('resourceRuntimeSelection').AsBoolean;
      LState.Agents.ResourceRuntimes := LFrame.Field('resourceRuntimes');
      LShell := BuildNyxStudioView(LSession, LState);
      {$ifdef PAS2JS}
      LRenderer := TNyxBrowserRenderer.Create;
      LHost := TJSHTMLElement(document.createElement('section'));
      document.body.appendChild(LHost);
      try
        LRenderer.Render(LShell, LShell.Pages[0], LHost);
        Check(LRenderer.ElementFor('studio-resource-runtime-0-status-published').textContent =
          'Published loads: 1 / Resource variants: 4', 'common Studio browser controls paint actual run summary');
        Check(LRenderer.ElementFor('studio-resource-runtime-0-selection-displayed').textContent =
          'Displayed content: Network', 'browser detail paints the actual installed publication');
        Check(LRenderer.ElementFor('studio-resource-runtime-0-selection-notification-error').textContent =
          TNyxText('Publication callback: Observer 🌙 failed after publication'),
          'browser detail paints the actual Unicode publication callback failure');
      finally
        LRenderer.Free;
        LHost.remove;
        LShell.Free;
      end;
      {$else}
      LWindow := TForm.CreateNew(nil);
      LRenderer := TNyxLCLRenderer.Create;
      try

        if LPass = 0 then
        begin
          LWindow.ClientWidth := 1240;
          LWindow.ClientHeight := 820;
        end
        else
        begin
          LWindow.ClientWidth := 390;
          LWindow.ClientHeight := 700;
        end;
        LRenderer.Render(LShell, LShell.Pages[0], LWindow);
        LWindow.Show;
        Application.ProcessMessages;
        LTarget := LRenderer.ControlFor('studio-resource-runtime-0-status-cache-writes');
        LAncestor := LTarget.Parent;
        while LAncestor <> nil do
        begin

          if LAncestor is TScrollingWinControl then
          begin
            TScrollingWinControl(LAncestor).ScrollInView(LTarget);
          end;
          LAncestor := LAncestor.Parent;
        end;
        Application.ProcessMessages;
        LLocation := LWindow.ScreenToClient(LTarget.ClientToScreen(Point(0, 0)));
        Check((LLocation.Y >= 0) and
          (LLocation.Y + LTarget.Height <= LWindow.ClientHeight),
          'runtime observer capture includes the complete cache status row');
        Check(TNyxText(RawByteString(TLabel(LRenderer.ControlFor(
          'studio-resource-runtime-0-status-published')).Caption)) =
          'Published loads: 1 / Resource variants: 4',
          'common Studio native controls paint actual run summary');
        Check(TNyxText(RawByteString(TLabel(LRenderer.ControlFor(
          'studio-resource-runtime-0-selection-displayed')).Caption)) =
          'Displayed content: Network', 'native detail paints the actual installed publication');
        Check(TNyxText(RawByteString(TLabel(LRenderer.ControlFor(
          'studio-resource-runtime-0-selection-notification-error')).Caption)) =
          TNyxText('Publication callback: Observer 🌙 failed after publication'),
          'native detail paints the actual Unicode publication callback failure');
        LBitmap := TBitmap.Create;
        LImage := nil;
        try
          { Win32 PaintTo includes native decorations. LCL form dimensions may
            describe the client allocation, so use the actual outer rectangle
            to retain every visible row rather than cropping beneath its title. }
          Check(GetWindowRect(LWindow.Handle, LBounds) <> 0,
            'runtime observer capture obtains actual outer window bounds');
          LBitmap.SetSize(LBounds.Right - LBounds.Left, LBounds.Bottom - LBounds.Top);
          LWindow.PaintTo(LBitmap.Canvas, 0, 0);
          LImage := LBitmap.CreateIntfImage;
          LPath := 'build/resource-runtime/desktop.png';

          if LPass = 1 then
          begin
            LPath := 'build/resource-runtime/compact-native.png';
          end;
          LImage.SaveToFile(LPath);
        finally
          LImage.Free;
          LBitmap.Free;
        end;
      finally
        LRenderer.Free;
        LWindow.Free;
        LShell.Free;
      end;
      {$endif}
    end;
  finally
    LSession.Free;
  end;
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
  StartObservation;
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
      raise ENyxResource.Create('Application resource journey timed out at stage ' +
        TNyxText(IntToStr(FStage)) + ' after ' + TNyxText(IntToStr(FChecks)) + ' checks');
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
          ReportRuntime;
          Check((RuntimePage.Field('items').Item(0).Field('phase').AsText = 'loading') and
            (FInitialRuntime.Entry(0).Status.Phase = nrpQueued),
            'real loading report advances without mutating retained snapshots');
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
          ReportRuntime;
          Check((RuntimePage.Field('items').Item(0).Field('publishedOrigin').AsText = 'network') and
            (RuntimePage.Field('items').Item(0).Field('cacheWrite').AsText = 'none'),
            'successful actual control publication reports network origin and bypass storage');
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
          ReportRuntime;
          Check((RuntimePage.Field('items').Item(0).Field('phase').AsText = 'rejected') and
            (RuntimePage.Field('items').Item(0).Field('publishedOrigin').AsText = 'network'),
            'rejected reload reports its failure while retaining previous installed-load evidence');
          LStatus := TNyxResourceRuntimeDetail.FromData(
            RuntimePage.Field('items').Item(0)).Entry.Status;
          Check((LStatus.Phase = nrpRejected) and (LStatus.Error <> ''),
            'public typed detail retains the actual rejected application attempt');
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
          ReportRuntime;
          Check(RuntimePage.Field('items').Item(0).Field('phase').AsText = 'waiting',
            'busy actual target reports waiting rather than successful new publication');
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
          ReportRuntime;
          Check((RuntimePage.Field('items').Item(0).Field('phase').AsText = 'cancelled') and
            (RuntimePage.Field('items').Item(0).Field('publishedOrigin').AsText = 'network'),
            'cancelled attempt preserves previous installed-load origin');
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
          ReportRuntime;
          Check(RuntimePage.Field('items').Item(0).Field('notificationError').AsText =
            TNyxText('Observer 🌙 failed after publication'),
            'bounded semantic diagnostics retain supplementary Unicode observer failures');
          ShowRuntimeObserver;
          { The static Studio view/capture is synchronous qualification work,
            not application network waiting. Restart that wait budget after it
            returns, keeping the later real resource request deadline intact. }
          FStartedAt := {$ifdef PAS2JS}TJSDate.now{$else}GetTickCount64{$endif};
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
          CheckObservationRetirement;
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
          FreeAndNil(FRuntimeAgent);
          FRuntimeAgent := TNyxAgentSession.Create(NyxProjectPair(FBefore,
            TNyxCodegen.Generate(FDocument)));
          FObservation := FRuntimeAgent.ObserveResourceRuntime(1, NyxStudioRuntime('workshop-run'),
            srsApplication, {$ifdef PAS2JS}npfBrowser{$else}npfNativeLCL{$endif}, '',
            NyxApplicationResourceDiagnostics(FResources).CaptureRuntime);
          Check((RuntimePage.Field('items').Item(0).Field('publishedOrigin').AsText = 'network') and
            (RuntimePage.Field('items').Item(0).Field('publishedCacheWrite').AsText = 'memory'),
            'real automatic HTTP publication exposes actual memory storage through semantic observation');
          FResources.Reload(NyxResourceRef('service'), NyxDefaultLocale);
          ReportRuntime;
          Check((RuntimePage.Field('items').Item(0).Field('phase').AsText = 'queued') and
            (RuntimePage.Field('items').Item(0).Field('publishedCacheWrite').AsText = 'memory'),
            'queued reload retains previous installed cache-tier evidence');
          FStage := 15;
        end;
      15:
        begin

          if FResources.Status(NyxResourceRef('service'), NyxDefaultLocale).Phase <> nrpReady then
          begin
            Exit;
          end;
          ReportRuntime;
          Check((RuntimePage.Field('items').Item(0).Field('publishedOrigin').AsText = 'fresh-cache') and
            (RuntimePage.Field('items').Item(0).Field('publishedCacheRead').AsText = 'memory') and
            (Caption('service') = 'nyx-studio-server'),
            'actual application cache hit agrees with semantic installed-origin and ordinary caption');
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
      .Cache(NyxResourceCache.Memory.FreshFor(60).ServerPolicy(rcspOverride))
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
  FRuntimeAgent.Free;
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
