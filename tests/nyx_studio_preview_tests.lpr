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
program nyx_studio_preview_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.data, nyx.state,
  nyx.events, nyx.behavior, nyx.scheduler, nyx.publication,
  nyx.application.context, nyx.root.types, nyx.composition,
  nyx.view.publication, nyx.codec, nyx.studio.previewruntime,
  nyx.test.flow.routes
  {$ifdef PAS2JS}, JS, Web{$else}, Classes{$endif};

type
  { Real scheduler delegation permits one-shot caller-slot release, retirement
    and cleanup failure. Submissions still use the existing scheduler/UI queue. }
  TReviewScheduler = class(TInterfacedObject, INyxScheduler)
  public
    Inner: INyxScheduler;
    FailRequire: Boolean;
    ReleaseOwner: Boolean;
    ReleaseView: Boolean;
    RetirePlan: Boolean;
    FailedCalls: Integer;
    ReleasedViews: Integer;
    RetiredPlans: Integer;
    function GetCapabilities: TNyxSchedulerCapabilities;
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    function Submit(const AWork: INyxWork;
      APolicy: TNyxExecutionPolicy): INyxExecution;
    function PostUI(const AWork: INyxWork;
      const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
  end;
  { Forward declaration lets a later validator exercise the same public host. }
  TReview = class;
  { Closed fixture actions target actual admission boundaries only. }
  TProbeMode = (pmRefuse, pmReenter, pmCancelFuture, pmRevokeCandidate,
    pmDisposeSlot, pmFailCleanup, pmRetireCleanup);
  { An independent participant changes authority after both validators. Its
    Install is empty; deferred cleanup is the first permitted scheduler call. }
  TProbe = class(TInterfacedObject, INyxPreparedPublication)
  public
    Host: TReview;
    Mode: TProbeMode;
    ReentryRefused: Boolean;
    Notified: Boolean;
    Retired: Boolean;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { Alternate public wrappers are not the exact private runtime authority.
    Admission must refuse before querying this deliberately throwing getter. }
  TForeignOwner = class(TInterfacedObject, INyxStudioPreviewOwner)
  public
    Reads: Integer;
    function GetRuntime: TNyxStudioPreviewRuntime;
    procedure Revoke;
    procedure Close;
  end;
  { Public application host owns the real source, realized tree and router.
    Sources/slots use only public APIs; no renderer, private hook or fake store. }
  TReview = class
  public
    Document: TNyxDocument;
    Root: TNyxNode;
    Events: INyxEvents;
    Scheduler: INyxScheduler;
    SchedulerObject: TReviewScheduler;
    Slot: TNyxStudioPreviewSlot;
    AcceptedSource: INyxViewPublicationSource;
    Source: INyxViewPublicationSource;
    Candidate: INyxStudioPreviewOwner;
    View: INyxPreparedEventView;
    Plan: INyxPreparedPublication;
    constructor Create;
    destructor Destroy; override;
    function NewOwner: INyxStudioPreviewOwner;
    procedure Prepare(const AOwner: INyxStudioPreviewOwner;
      AReuseSource: Boolean = False; AClearView: Boolean = False);
    procedure Accept(const AProbe: INyxPreparedPublication = nil);
    procedure Abandon;
    procedure Change(const AText: TNyxText);
  end;

var
  GChecks: Integer;
  GReleaseOwner: INyxStudioPreviewOwner;
  GReleaseViewHost: TReview;
  GCleanupPlan: INyxPreparedPublication;
  GFormerSource: INyxViewPublicationSource;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Studio preview: ' + AReason);
  end;
  Inc(GChecks);
end;

function TReviewScheduler.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := Inner.Capabilities;
end;

procedure TReviewScheduler.RequireUI;
var
  LPlan: INyxPreparedPublication;
begin

  if ReleaseView then
  begin
    ReleaseView := False;
    Inc(ReleasedViews);
    GReleaseViewHost.View := nil;
    GReleaseViewHost := nil;
  end;

  if RetirePlan then
  begin
    RetirePlan := False;
    Inc(RetiredPlans);
    LPlan := GCleanupPlan;
    GCleanupPlan := nil;
    GFormerSource := nil;
    LPlan.Retire;
  end;

  if ReleaseOwner then
  begin
    ReleaseOwner := False;
    GReleaseOwner := nil;
  end;

  if FailRequire then
  begin
    FailRequire := False;
    Inc(FailedCalls);
    raise ENyxSchedule.Create('Deliberate deferred preview cleanup failure');
  end;
  Inner.RequireUI;
end;

procedure TReviewScheduler.Admit(APolicy: TNyxExecutionPolicy);
begin
  Inner.Admit(APolicy);
end;

function TReviewScheduler.Submit(const AWork: INyxWork;
  APolicy: TNyxExecutionPolicy): INyxExecution;
begin
  Result := Inner.Submit(AWork, APolicy);
end;

function TReviewScheduler.PostUI(const AWork: INyxWork;
  const AParent: INyxExecution): INyxExecution;
begin
  Result := Inner.PostUI(AWork, AParent);
end;

procedure TReviewScheduler.Shutdown;
begin
  Inner.Shutdown;
end;

procedure TProbe.Validate;
var
  LSource: INyxViewPublicationSource;
  LPlan: INyxPreparedPublication;
begin
  case Mode of
    pmRefuse:
      raise ENyxPublication.Create('Deliberate later validator refusal');
    pmReenter:
      begin
        try
          LSource := NewNyxStudioPreviewSource(Host.Slot, Host.Candidate, Host.Events);
          LPlan := LSource.Prepare(Host.Document, Host.Root, Host.View);
        except
          on Exception do
          begin
            ReentryRefused := True;
          end;
        end;
        Check(ReentryRefused, 'reentrant source cannot replace a reserved slot');
        raise ENyxPublication.Create('Refuse after the reentrant attempt');
      end;
    pmCancelFuture:
      Host.View.Generation.Cancel;
    pmRevokeCandidate:
      Host.Candidate.Revoke;
    pmDisposeSlot:
      FreeAndNil(Host.Slot);
    pmFailCleanup:
      begin
        { All outward validators have returned. Final fences and Install must
          not call the scheduler; the first deferred cleanup call will fail. }
        Host.SchedulerObject.FailRequire := True;
      end;
    pmRetireCleanup:
      begin
        { All validators have returned. The first cleanup RequireUI reenters
          public Retire while the displaced mount is still executing Close. }
        GCleanupPlan := Host.Plan;
        Host.SchedulerObject.RetirePlan := True;
      end;
  end;
end;

procedure TProbe.Install;
begin
end;

procedure TProbe.Notify;
begin
  Notified := True;
end;

procedure TProbe.Retire;
begin
  Retired := True;
end;

function TForeignOwner.GetRuntime: TNyxStudioPreviewRuntime;
begin
  Result := nil;
  Inc(Reads);
  raise ENyxApplicationContext.Create('Foreign getter must not be observed');
end;

procedure TForeignOwner.Revoke;
begin
end;

procedure TForeignOwner.Close;
begin
end;

constructor TReview.Create;
begin
  inherited Create;
  Document := RoutedLogicDocument;
  Root := RealizeNyxView(Document, Document.Pages[0]);
  SchedulerObject := TReviewScheduler.Create;
  Scheduler := SchedulerObject;
  SchedulerObject.Inner := NewNyxScheduler;
  Events := NewNyxEvents(Scheduler);
  Slot := TNyxStudioPreviewSlot.Create(Scheduler);
end;

destructor TReview.Destroy;
begin
  SchedulerObject.FailRequire := False;
  SchedulerObject.ReleaseView := False;
  SchedulerObject.RetirePlan := False;

  if Slot <> nil then
  begin
    Slot.Revoke;
  end;
  Abandon;
  Events.Close;
  Source := nil;
  AcceptedSource := nil;
  Slot.Free;
  Candidate := nil;
  Root.Free;
  Document.Free;
  Scheduler.Shutdown;
  inherited Destroy;
end;

function TReview.NewOwner: INyxStudioPreviewOwner;
begin
  Result := NewNyxStudioPreviewOwner(Document, NyxPageRoot(Document.Pages[0].ID),
    {$ifdef PAS2JS}npfBrowser{$else}npfNativeLCL{$endif}, Scheduler);
end;

procedure TReview.Prepare(const AOwner: INyxStudioPreviewOwner;
  AReuseSource: Boolean; AClearView: Boolean);
var
  LPreparation: INyxEventViewPreparation;
  LView: INyxPreparedEventView;
begin
  Candidate := AOwner;

  if AReuseSource then
  begin
    Source := AcceptedSource;
  end
  else
  begin
    Source := NewNyxStudioPreviewSource(Slot, Candidate, Events);
  end;
  Check(Supports(Events, INyxEventViewPreparation, LPreparation),
    'actual router supplies its public successor capability');
  View := LPreparation.PrepareView;
  LView := View;

  if AClearView then
  begin
    GReleaseViewHost := Self;
    SchedulerObject.ReleaseView := True;
  end;
  try
    { Pass the mutable caller slot deliberately, not its local lease. The
      product must keep using its own captured interface after the callout. }
    Plan := Source.Prepare(Document, Root, View);

    if AClearView then
    begin
      Check((View = nil) and (SchedulerObject.ReleasedViews = 1),
        'RequireUI releases the exact caller event-view slot during preparation');
      View := LView;
    end;
  finally
    GReleaseViewHost := nil;
    SchedulerObject.ReleaseView := False;
  end;
end;

procedure TReview.Accept(const AProbe: INyxPreparedPublication);
var
  LRootFence: INyxPublicationReadiness;
  LEventFence: INyxPublicationReadiness;
  LFormerSource: INyxViewPublicationSource;
  LFailure: TNyxText;
begin
  LFormerSource := AcceptedSource;
  Check(Supports(Plan, INyxPublicationReadiness, LRootFence) and
    Supports(View, INyxPublicationReadiness, LEventFence),
    'public source and event owner expose final local fences');
  try
    { Match renderer order: root validator, event validator, later host work,
      all final fences, then event scope and pure owner adoption. }
    Plan.Validate;
    View.Validate;

    if AProbe <> nil then
    begin
      AProbe.Validate;
    end;
    LRootFence.CheckReady;
    LEventFence.CheckReady;
    View.Install;
    Plan.Install;
    AcceptedSource := Source;

    if AProbe <> nil then
    begin
      AProbe.Install;
    end;
    LFailure := '';
    try
      Plan.Notify;
    except
      on E: Exception do LFailure := TNyxText(E.Message);
    end;
    try
      View.Notify;
    except
      on E: Exception do
      begin

        if LFailure = '' then
        begin
          LFailure := TNyxText(E.Message);
        end;
      end;
    end;

    if AProbe <> nil then
    begin
      AProbe.Notify;
    end;

    if LFailure <> '' then
    begin
      raise ENyxPublicationNotification.CreateReceiverFailure(LFailure);
    end;
  finally
    try
      Plan.Retire;
    finally
      try
        View.Retire;
      finally

        if AProbe <> nil then
        begin
          AProbe.Retire;
        end;
      end;
    end;
    Plan := nil;
    View := nil;
    Source := nil;
    Candidate := nil;
  end;
end;

procedure TReview.Abandon;
begin

  if Plan <> nil then
  begin
    Plan.Retire;
    Plan := nil;
  end;

  if View <> nil then
  begin
    View.Retire;
    View := nil;
  end;
  Source := nil;
  Candidate := nil;
end;

procedure TReview.Change(const AText: TNyxText);
var
  LInfo: TNyxEventInfo;
begin
  LInfo := Default(TNyxEventInfo);
  LInfo.Trigger := ntChange;
  LInfo.Name := NyxEvent('change');
  LInfo.OriginID := 'source-memo';
  LInfo.SourceID := 'source-memo';
  LInfo.TargetID := 'source-memo';
  LInfo.HasValue := True;
  LInfo.ValueKind := nskText;
  LInfo.Value := NyxData(AText);
  LInfo.Details := NyxNull;
  Events.Dispatch(LInfo, 'source-memo', 'source-memo');
  LInfo.Value := NyxData('caller replaced its copied event');
end;

{$ifdef PAS2JS}
function UITurn: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(JSValue(True));
        end, 0);
    end);
end;
{$endif}

{ Public flow results still publish on UI after Sequential graph computation.
  Await real state under retained owners; no fixed-delay success assumption. }
procedure AwaitCompact(const AOwner: INyxStudioPreviewOwner; const AExpected: TNyxText);
  {$ifdef PAS2JS}async;{$endif}
var
  LOwner: INyxStudioPreviewOwner;
  LState: TNyxState;
  {$ifdef PAS2JS}LStarted: Double;
  {$else}LStarted: QWord;{$endif}
begin
  LOwner := AOwner;
  LState := LOwner.Runtime.State;
  {$ifdef PAS2JS}LStarted := TJSDate.now;
  {$else}LStarted := GetTickCount64;{$endif}
  while LState.GetValue(NyxTextState('compact')) <> AExpected do
  begin
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 10000 then
    {$else}

    if GetTickCount64 - LStarted > 10000 then
    {$endif}
    begin
      raise ENyxApplicationContext.Create('Preview route output timed out: ' +
        LState.GetValue(NyxTextState('compact')));
    end;
    {$ifdef PAS2JS}await(TJSPromise.resolve(UITurn));
    {$else}CheckSynchronize(1);{$endif}
  end;
end;

procedure Cases; {$ifdef PAS2JS}async;{$endif}
var
  LHost: TReview;
  LFirst: INyxStudioPreviewOwner;
  LNext: INyxStudioPreviewOwner;
  LOldContext: INyxApplicationContext;
  LGeneration: INyxCancellationScope;
  LOldLifetime: INyxCancellationScope;
  LState: TNyxState;
  LSaved: TNyxText;
  LBefore: TNyxText;
  LProbe: TProbe;
  LProbeLease: INyxPreparedPublication;
  LForeign: TForeignOwner;
  LForeignLease: INyxStudioPreviewOwner;
  LUnused: INyxViewPublicationSource;
  LRejected: Boolean;
  LIndex: Integer;
  LCleanupCalls: Integer;
begin
  LHost := TReview.Create;
  try
    LSaved := TNyxCodec.Encode(LHost.Document);
    LFirst := LHost.NewOwner;
    LState := LFirst.Runtime.State;
    Check(LState <> LHost.Document.State, 'managed runtime owns an independent state');
    LHost.Prepare(LFirst);
    LBefore := LState.GetValue(NyxTextState('compact'));
    LHost.Change('{"profile":{"name":"dormant"}}');
    Check((LHost.Slot.Current = nil) and
      (LState.GetValue(NyxTextState('compact')) = LBefore),
      'first prospective routes remain dormant and do not adopt the runtime');
    LHost.Accept;
    Check(LHost.Slot.Current = LFirst, 'pure publication adopts the exact managed owner');
    LHost.Change('{"profile":{"name":"first"}}');
    {$ifdef PAS2JS}await(AwaitCompact(LFirst, '{"name":"first"}'));
    {$else}AwaitCompact(LFirst, '{"name":"first"}');{$endif}
    Check(LState.GetValue(NyxTextState('compact')) = '{"name":"first"}',
      'real saved graph publishes the copied EventValue into the accepted store');

    LNext := LHost.NewOwner;
    LHost.Prepare(LNext);
    LHost.Abandon;
    Check((LHost.Slot.Current = LFirst) and LFirst.Runtime.Context.Active and
      LNext.Runtime.Context.Active, 'abandonment closes only candidate routes/context');
    LHost.Change('{"profile":{"name":"after-abandon"}}');
    {$ifdef PAS2JS}await(AwaitCompact(LFirst, '{"name":"after-abandon"}'));
    {$else}AwaitCompact(LFirst, '{"name":"after-abandon"}');{$endif}

    for LIndex := 0 to 3 do
    begin
      LNext := LHost.NewOwner;
      LHost.Prepare(LNext);
      LGeneration := NyxEventGeneration(LHost.Events);
      LProbe := TProbe.Create;
      LProbeLease := LProbe;
      LProbe.Host := LHost;
      LProbe.Mode := TProbeMode(LIndex);
      LRejected := False;
      try
        LHost.Accept(LProbeLease);
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LHost.Slot.Current = LFirst) and
        not LGeneration.Cancelled and LFirst.Runtime.Context.Active,
        'late refusal/reentry/cancellation/revocation preserves the old runtime');
      Check(LProbe.Retired, 'failed host admission retires its later participant');
      LProbeLease := nil;
    end;
    LHost.Change('{"profile":{"name":"still-live"}}');
    {$ifdef PAS2JS}await(AwaitCompact(LFirst, '{"name":"still-live"}'));
    {$else}AwaitCompact(LFirst, '{"name":"still-live"}');{$endif}

    LGeneration := NyxEventGeneration(LHost.Events);
    LOldContext := LFirst.Runtime.Context;
    LOldLifetime := LOldContext.Generation;
    LHost.Prepare(LFirst, True);
    LHost.Accept;
    Check(LGeneration.Cancelled and not LOldLifetime.Cancelled and
      LOldContext.Active and (LHost.Slot.Current = LFirst) and
      (LFirst.Runtime.State = LState),
      'same-owner remount replaces the view generation without retagging or retiring stores');
    LHost.Change('{"profile":{"name":"remounted"}}');
    {$ifdef PAS2JS}await(AwaitCompact(LFirst, '{"name":"remounted"}'));
    {$else}AwaitCompact(LFirst, '{"name":"remounted"}');{$endif}

    LNext := LHost.NewOwner;
    LHost.Prepare(LNext);
    LProbe := TProbe.Create;
    LProbeLease := LProbe;
    LProbe.Host := LHost;
    LProbe.Mode := pmFailCleanup;
    LRejected := False;
    try
      LHost.Accept(LProbeLease);
    except
      on ENyxPublicationNotification do
      begin
        LRejected := True;
      end;
    end;
    LHost.SchedulerObject.FailRequire := False;
    Check(LRejected and LProbe.Notified and LProbe.Retired and
      (LHost.SchedulerObject.FailedCalls = 1),
      'committed route cleanup failure still attempts old owner and later notification');
    Check((LHost.Slot.Current = LNext) and not LOldContext.Active and
      LOldLifetime.Cancelled and LNext.Runtime.Context.Active,
      'notification failure leaves the newly accepted owner committed and old authority revoked');
    LHost.Change('{"profile":{"name":"committed"}}');
    {$ifdef PAS2JS}await(AwaitCompact(LNext, '{"name":"committed"}'));
    {$else}AwaitCompact(LNext, '{"name":"committed"}');{$endif}
    LProbeLease := nil;
    LOldContext := LNext.Runtime.Context;
    LGeneration := NyxEventGeneration(LHost.Events);
    LHost.Prepare(nil);
    LHost.Accept;
    Check((LHost.Slot.Current = nil) and not LOldContext.Active and LGeneration.Cancelled,
      'explicit nil design source clears the slot only at publication');
    Check(TNyxCodec.Encode(LHost.Document) = LSaved,
      'runtime output, refusal, remount and retirement never rewrite saved defaults');

    LForeign := TForeignOwner.Create;
    LForeignLease := LForeign;
    LRejected := False;
    try
      LUnused := NewNyxStudioPreviewSource(LHost.Slot, LForeignLease, LHost.Events);
    except
      on ENyxApplicationContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LForeign.Reads = 0),
      'alternate wrapper refuses before its public runtime getter');
    LForeignLease := nil;

    LNext := LHost.NewOwner;
    LHost.Prepare(LNext);
    LProbe := TProbe.Create;
    LProbeLease := LProbe;
    LProbe.Host := LHost;
    LProbe.Mode := pmDisposeSlot;
    LRejected := False;
    try
      LHost.Accept(LProbeLease);
    except
      on ENyxApplicationContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LHost.Slot = nil) and LProbe.Retired,
      'weak slot revocation refuses safely after an outward validator disposes its owner');
    Check(LNext.Runtime.Context.Active,
      'abandoned view cleanup does not revoke a separately retained candidate store');
    LOldContext := LNext.Runtime.Context;
    LOldLifetime := LOldContext.Generation;
    LCleanupCalls := LHost.SchedulerObject.FailedCalls;
    LHost.SchedulerObject.FailRequire := True;
    LNext.Revoke;
    Check(not LOldContext.Active and not LOldLifetime.Cancelled and
      LHost.SchedulerObject.FailRequire and
      (LHost.SchedulerObject.FailedCalls = LCleanupCalls),
      'pure revocation masks contexts without scheduler work or cancellation');
    LHost.SchedulerObject.FailRequire := False;
    LRejected := False;
    try
      LState := LNext.Runtime.State;
    except
      on ENyxApplicationContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'revoked owner refuses new borrowed runtime access');
    LNext.Close;
    Check(LOldLifetime.Cancelled, 'deferred Close performs actual lifetime cancellation');

    GReleaseOwner := LHost.NewOwner;
    LOldContext := GReleaseOwner.Runtime.Context;
    LOldLifetime := LOldContext.Generation;
    LHost.SchedulerObject.ReleaseOwner := True;
    GReleaseOwner.Close;
    Check((GReleaseOwner = nil) and not LOldContext.Active and LOldLifetime.Cancelled,
      'Close retains its managed receiver when a scheduler callback clears the last caller slot');
  finally
    GReleaseOwner := nil;
    LProbeLease := nil;
    LUnused := nil;
    LForeignLease := nil;
    LOldContext := nil;
    LFirst := nil;
    LNext := nil;
    LHost.Free;
    {$ifndef PAS2JS}
    for LIndex := 0 to 1023 do
    begin

      if not CheckSynchronize(1) then
      begin
        Break;
      end;
    end;
    {$endif}
  end;
end;

{ Reuse the same real public host to qualify leases across outward preparation
  and cleanup. The former source releases inside cleanup before public Retire. }
procedure ReentrantCases; {$ifdef PAS2JS}async;{$endif}
var
  LHost: TReview;
  LFirst: INyxStudioPreviewOwner;
  LNext: INyxStudioPreviewOwner;
  LOldContext: INyxApplicationContext;
  LOldLifetime: INyxCancellationScope;
  LNextSource: INyxViewPublicationSource;
  LProbe: TProbe;
  LProbeLease: INyxPreparedPublication;
  LSaved: TNyxText;
  LIndex: Integer;
begin
  LHost := TReview.Create;
  try
    LSaved := TNyxCodec.Encode(LHost.Document);
    LFirst := LHost.NewOwner;
    LHost.Prepare(LFirst, False, True);
    LHost.Accept;
    Check((LHost.Slot.Current = LFirst) and LFirst.Runtime.Context.Active,
      'retained event view installs after its original caller slot was released');
    LHost.Change('{"profile":{"name":"retained-view"}}');
    {$ifdef PAS2JS}await(AwaitCompact(LFirst, '{"name":"retained-view"}'));
    {$else}AwaitCompact(LFirst, '{"name":"retained-view"}');{$endif}
    Check(LFirst.Runtime.State.GetValue(NyxTextState('compact')) =
      '{"name":"retained-view"}',
      'routes prepared from the retained view publish real copied input');

    LOldContext := LFirst.Runtime.Context;
    LOldLifetime := LOldContext.Generation;
    { Accept must retain no former source of its own. The callback releases this
      sole source lease after old Close begins, immediately before public Retire
      clears the plan's mount lease. The independently retained store owner
      makes cancellation prove explicit Close instead of destructor fallback. }
    GFormerSource := LHost.AcceptedSource;
    LHost.AcceptedSource := nil;
    LNext := LHost.NewOwner;
    LHost.Prepare(LNext);
    LNextSource := LHost.Source;
    LProbe := TProbe.Create;
    LProbeLease := LProbe;
    LProbe.Host := LHost;
    LProbe.Mode := pmRetireCleanup;
    LHost.Accept(LProbeLease);
    Check((LHost.SchedulerObject.RetiredPlans = 1) and (GFormerSource = nil) and
      LProbe.Notified and LProbe.Retired,
      'public Retire reenters old cleanup once and later participants still complete');
    Check(not LOldContext.Active and LOldLifetime.Cancelled,
      'reentrant retirement cannot skip independent displaced-owner Close');
    Check((LHost.Slot.Current = LNext) and
      (LHost.AcceptedSource = LNextSource) and LNext.Runtime.Context.Active,
      'reentrant old cleanup preserves the accepted next source and slot owner');
    LHost.Change('{"profile":{"name":"after-retire"}}');
    {$ifdef PAS2JS}await(AwaitCompact(LNext, '{"name":"after-retire"}'));
    {$else}AwaitCompact(LNext, '{"name":"after-retire"}');{$endif}
    Check(LNext.Runtime.State.GetValue(NyxTextState('compact')) =
      '{"name":"after-retire"}',
      'accepted next routes remain usable after preparation fields were retired reentrantly');
    Check(TNyxCodec.Encode(LHost.Document) = LSaved,
      'caller release and reentrant cleanup leave authored defaults unchanged');
  finally
    LHost.SchedulerObject.ReleaseView := False;
    LHost.SchedulerObject.RetirePlan := False;
    GReleaseViewHost := nil;
    GCleanupPlan := nil;
    GFormerSource := nil;
    LProbeLease := nil;
    LNextSource := nil;
    LOldContext := nil;
    LFirst := nil;
    LNext := nil;
    LHost.Free;
    {$ifndef PAS2JS}
    for LIndex := 0 to 1023 do
    begin

      if not CheckSynchronize(1) then
      begin
        Break;
      end;
    end;
    {$endif}
  end;
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
begin
  try
    {$ifdef PAS2JS}await(Cases);
    await(ReentrantCases);
    document.body.setAttribute('data-studio-preview', 'passed');
    document.body.setAttribute('data-studio-preview-checks', IntToStr(GChecks));
    {$else}Cases;
    ReentrantCases;
    WriteLn('PASS ', GChecks, ' Studio preview ownership checks');{$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-studio-preview', 'failed');
      document.body.setAttribute('data-studio-preview-error', E.Message);
      {$else}
      WriteLn('FAIL ', GChecks, ' ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  Run;
end.
