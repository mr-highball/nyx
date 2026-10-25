{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
unit nyx.live.inspection;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model, nyx.application.context, nyx.events, nyx.collections.view;

const
  InspectionPeerID = 'live-service-peer';
  InspectionStatusID = 'inspection-status';
  InspectionPauseID = 'pause-inspection';
  InspectionResumeID = 'resume-inspection';
  InspectionStepID = 'next-feed-update';

type
  { Own this UI-only binding beside the mounted application. Close before page
    replacement/application disposal. Its callbacks hold no application, router
    or document; the revocable context/generation refuses late invocations. }
  TNyxLiveInspectionDemo = class
  private
    FCallback: INyxEventCallback;
    FCallbackObject: TObject; { Borrowed only while FCallback retains it. }
    FSubscriptions: array of INyxEventSubscription;
  public
    constructor Create(const AContext: INyxApplicationContext;
      const AEvents: INyxEvents; const AInspection, APeer: INyxCollectionView);
    destructor Destroy; override;
    procedure Close;
  end;

{ Ordinary portable Nyx controls share one runtime collection. Only runtime
  attachment enables pause; neither saved designs nor history contain a paused
  frame, machine resource or live subscription. Caller owns the document. }
function BuildLiveInspectionDocument: TNyxDocument;

implementation

uses SysUtils, nyx.text, nyx.types, nyx.controls, nyx.state, nyx.behavior, nyx.responsive,
  nyx.scheduler, nyx.collections, nyx.collections.inspection, nyx.service.monitor;

type
  { Router registrations retain this callback, never their owning binding.
    The status token has the ordinary weak receiver lifetime. Closing disconnects
    that token before clearing managed view/context leases, so no cycle or raw
    application reference survives. All replay work is synchronous on UI. }
  TInspectionCallback = class(TNyxEventCallback)
  private
    FContext: INyxApplicationContext;
    FGeneration: INyxCancellationScope;
    FInspection: INyxCollectionInspection;
    FPeer: INyxCollectionView;
    FStatusToken: INyxCollectionViewSubscription;
    FStep: Integer;
    function Active: Boolean;
    procedure StatusChanged(const AStatus: TNyxCollectionInspectionObservation;
      ACause: TNyxCollectionInspectionCause);
    procedure AdvanceFeed;
  public
    constructor Create(const AContext: INyxApplicationContext;
      const AInspection, APeer: INyxCollectionView);
    destructor Destroy; override;
    procedure Close;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

function BuildLiveInspectionDocument: TNyxDocument;
var
  LPage: TNyxNode;
  LCommands: INyxRow;
  LPause: INyxButton;
  LResume: INyxButton;
  LStep: INyxButton;
  LStatus: INyxLabel;
  LPeerTitle: INyxLabel;
  LPeer: INyxTable;
begin
  Result := BuildServiceMonitorDocument;
  try
    Result.Title := 'Live service inspection';
    Result.State.SetValue(NyxTextState(InspectionStatusID), 'Following live updates.');
    LPage := Result.Find('services');
    LPage.Configure.Gap(8).Done;
    Result.Find('service-title').Configure.Text('Keep your place while services change').Done;
    Result.Find('service-introduction').Configure.Text(
      'Pause the upper table, then advance the sample feed. The lower table stays live.').Done;
    Result.Find(ServiceTableID).Configure.Height(160).Done;
    LCommands := NewNyxRow('inspection-commands');
    LPage.Insert(2, LCommands);
    LCommands.Configure.Gap(8).Done;
    LCommands.Configure.WhenViewport(TNyxViewportWidth.Below(560)).Layout(nlColumn).Done;
    LPause := NewNyxButton(InspectionPauseID).WithText('Pause inspection');
    LCommands.Add(LPause);
    LResume := NewNyxButton(InspectionResumeID).WithText('Resume latest');
    LCommands.Add(LResume);
    LStep := NewNyxButton(InspectionStepID).WithText('Next feed update');
    LCommands.Add(LStep);
    LStatus := NewNyxLabel(InspectionStatusID);
    LPage.Insert(3, LStatus);
    LStatus.Binds.Text(NyxTextState(InspectionStatusID)).Done;
    LStatus.Configure.AccessibleName('Inspection update status').Done;
    LPeerTitle := NewNyxLabel('inspection-peer-title').WithText('Independent live view');
    LPage.Insert(5, LPeerTitle);
    LPeer := NewNyxTable(InspectionPeerID);
    LPage.Insert(6, LPeer);
    LPeer.Configure.Height(160).AccessibleName('Live service records').Done;
    LPeer.Binds.Collection(Result.Find(ServiceTableID).CollectionView).Done;
    Result.Find('service-help').Configure.Text(
      'Each click replays one atomic update: change and insert, remove, then replace a record. ' +
      'Paused values can be selected, filtered and copied; edits refuse until Resume. ' +
      'A removed or replaced selection is explained when you resume. No timer or external service is involved.').Done;
  except
    Result.Free;
    raise;
  end;
end;

constructor TInspectionCallback.Create(const AContext: INyxApplicationContext;
  const AInspection, APeer: INyxCollectionView);
begin
  inherited Create;

  if (AContext = nil) or not AContext.Active or (AInspection = nil) or
    (APeer = nil) or (AInspection = APeer) or (AInspection.Store <> APeer.Store) then
  begin
    raise ENyxCollectionInspection.Create('The demo needs two active views of the same source');
  end;
  AContext.Scheduler.RequireUI;
  FContext := AContext;
  FGeneration := AContext.Generation;
  FInspection := NyxCollectionInspection(AInspection);

  if FInspection = nil then
  begin
    raise ENyxCollectionInspection.Create('The inspected view has no inspection capability');
  end;
  FPeer := APeer;
  FInspection.ConfigureInspection(NyxInspectionPolicy);
  FStatusToken := FInspection.SubscribeInspection(StatusChanged);
  StatusChanged(FInspection.CaptureInspection, ncicPolicyChanged);
end;

destructor TInspectionCallback.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TInspectionCallback.Active: Boolean;
begin
  Result := (FContext <> nil) and FContext.Active and (FGeneration <> nil) and
    not FGeneration.Cancelled;
end;

procedure TInspectionCallback.Close;
begin
  FStatusToken := nil;
  FInspection := nil;
  FPeer := nil;
  FGeneration := nil;
  FContext := nil;
end;

procedure TInspectionCallback.StatusChanged(const AStatus: TNyxCollectionInspectionObservation;
  ACause: TNyxCollectionInspectionCause);
var
  LText: TNyxText;
  LLease: INyxEventCallback;
begin
  LLease := Self;

  if not Active then
  begin
    Exit;
  end;

  if AStatus.Mode = ncimPaused then
  begin
    LText := 'Paused at revision ' + TNyxText(IntToStr(AStatus.DisplayRevision)) +
      ' - ' + TNyxText(IntToStr(AStatus.PendingPublications)) + ' pending updates. The lower view is live.';
  end
  else
  begin
    LText := 'Following live updates - revision ' + TNyxText(IntToStr(AStatus.DisplayRevision)) + '.';

    if AStatus.ContextResetOnResume then
    begin
      LText := LText + ' Selection was cleared because removal tracking exceeded its budget.';
    end
    else if AStatus.MissingSelectedRows > 0 then
    begin
      LText := LText + ' ' + TNyxText(IntToStr(AStatus.MissingSelectedRows)) +
        ' inspected records were removed or replaced. Choose a current record.';
    end;
  end;
  FContext.State.SetValue(NyxTextState(InspectionStatusID), LText);
end;

procedure TInspectionCallback.AdvanceFeed;
var
  LStore: INyxCollection;
  LSource: INyxCollectionSnapshot;
  LGateway: TNyxItemRef;
  LHealth: TNyxItemRef;
begin
  LStore := FPeer.Store;
  LSource := LStore.Snapshot;
  LGateway := NyxItem(LSource.Key, 'gateway');
  LHealth := NyxItem(LSource.Key, 'health');
  { A fixed three-step replay, not an unbounded snapshot queue. Admission uses
    the captured authoritative revision, independent of the paused display. }
  case FStep of
    0:
      LStore.Apply([
        NyxUpdate(NyxCollectionItem(LGateway).WithValue(NyxTextField('status'), 'Busy')),
        NyxInsert(LSource.Count, NyxCollectionItem(LHealth)
          .WithValue(NyxTextField('name'), 'Health check worker')
          .WithValue(NyxTextField('status'), 'Running'))], LSource.Revision);
    1:
      LStore.Apply([NyxRemove(LHealth)], LSource.Revision);
    2:
      LStore.Apply([NyxRemove(LGateway), NyxInsert(0, NyxCollectionItem(LGateway)
        .WithValue(NyxTextField('name'), 'Replacement request gateway')
        .WithValue(NyxTextField('status'), 'Restarted'))], LSource.Revision);
  end;
  FStep := (FStep + 1) mod 3;
end;

procedure TInspectionCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LLease: INyxEventCallback;
begin
  LLease := Self;

  if not Active or (AExecution = nil) or AExecution.Cancelled then
  begin
    Exit;
  end;
  FContext.Scheduler.RequireUI;

  if AEvent.OriginID = InspectionPauseID then
  begin
    FInspection.PauseInspection;
  end
  else if AEvent.OriginID = InspectionResumeID then
  begin
    FInspection.ResumeInspection;
  end
  else if AEvent.OriginID = InspectionStepID then
  begin
    AdvanceFeed;
  end;
end;

constructor TNyxLiveInspectionDemo.Create(const AContext: INyxApplicationContext;
  const AEvents: INyxEvents; const AInspection, APeer: INyxCollectionView);
var
  LCallback: TInspectionCallback;
begin
  inherited Create;

  if AEvents = nil then
  begin
    raise ENyxCollectionInspection.Create('The demo needs its mounted event router');
  end;
  LCallback := TInspectionCallback.Create(AContext, AInspection, APeer);
  FCallback := LCallback;
  FCallbackObject := LCallback;
  SetLength(FSubscriptions, 3);
  FSubscriptions[0] := AEvents.On(NyxControlEvents(InspectionPauseID), ntClick)
    .Policy(neSequential).Subscribe(FCallback);
  FSubscriptions[1] := AEvents.On(NyxControlEvents(InspectionResumeID), ntClick)
    .Policy(neSequential).Subscribe(FCallback);
  FSubscriptions[2] := AEvents.On(NyxControlEvents(InspectionStepID), ntClick)
    .Policy(neSequential).Subscribe(FCallback);
end;

destructor TNyxLiveInspectionDemo.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TNyxLiveInspectionDemo.Close;
var
  LIndex: Integer;
begin

  if FCallback <> nil then
  begin
    TInspectionCallback(FCallbackObject).Close;
  end;
  for LIndex := 0 to High(FSubscriptions) do
  begin

    if FSubscriptions[LIndex] <> nil then
    begin
      FSubscriptions[LIndex].Cancel;
    end;
  end;
  FSubscriptions := nil;
  FCallbackObject := nil;
  FCallback := nil;
end;

end.
