{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
unit nyx.live.inspection;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model, nyx.application.context, nyx.events, nyx.collections.view,
  nyx.collections.position, nyx.collections.recovery;

const
  InspectionPeerID = 'live-service-peer';
  InspectionStatusID = 'inspection-status';
  InspectionPauseID = 'pause-inspection';
  InspectionResumeID = 'resume-inspection';
  InspectionStepID = 'next-feed-update';
  InspectionFollowID = 'follow-last-visible';
  InspectionKeepID = 'keep-reading-position';
  InspectionCanFollowID = 'inspection-can-follow';
  RecoverySummaryID = 'recovered-draft-summary';
  RecoveryTextID = 'recovered-draft-text';
  RecoveryVisibleID = 'recovered-drafts-visible';
  RecoveryPreviousID = 'previous-recovered-draft';
  RecoveryNextID = 'next-recovered-draft';
  RecoveryDiscardID = 'discard-recovered-draft';
  RecoveryDiscardTextID = 'discard-recovered-draft-caption';

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
      const AEvents: INyxEvents; const AInspection, APeer: INyxCollectionView;
      const APosition: INyxCollectionPositionState = nil;
      const ARecovery: INyxCollectionDraftRecovery = nil);
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
    FFollowing: INyxCollectionFollowing;
    FPeer: INyxCollectionView;
    FStatusToken: INyxCollectionViewSubscription;
    FPosition: INyxCollectionPositionState;
    FPositionSerial: Integer;
    FRecovery: INyxCollectionDraftRecovery;
    FRecoveryIndex: Integer;
    FRecovered: INyxRecoveredCollectionDraft;
    FDiscard: INyxRecoveredCollectionDraft;
    FStep: Integer;
    function Active: Boolean;
    procedure StatusChanged(const AStatus: TNyxCollectionInspectionObservation;
      ACause: TNyxCollectionInspectionCause);
    procedure AdvanceFeed;
    procedure PublishState(const AStatus: TNyxText);
    procedure RecoveryAction(const AID: TNyxText);
  public
    constructor Create(const AContext: INyxApplicationContext;
      const AInspection, APeer: INyxCollectionView; const APosition: INyxCollectionPositionState;
      const ARecovery: INyxCollectionDraftRecovery);
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
  LFollow: INyxButton;
  LKeep: INyxButton;
  LStatus: INyxLabel;
  LPeerTitle: INyxLabel;
  LPeer: INyxTable;
  LRecovery: INyxCard;
  LRecoverySummary: INyxLabel;
  LRecoveryText: INyxMemo;
  LRecoveryCommands: INyxRow;
  LPreviousDraft: INyxButton;
  LNextDraft: INyxButton;
  LDiscardDraft: INyxButton;
begin
  Result := BuildServiceMonitorDocument;
  try
    Result.Title := 'Live service inspection';
    Result.State.SetValue(NyxTextState(InspectionStatusID), 'Following live updates.');
    Result.State.SetValue(NyxTextState(RecoverySummaryID), 'No recovered drafts.');
    Result.State.SetValue(NyxTextState(RecoveryTextID), '');
    Result.State.SetValue(NyxTextState(RecoveryDiscardTextID), 'Discard draft');
    Result.State.SetValue(NyxBooleanState(RecoveryVisibleID), False);
    Result.State.SetValue(NyxBooleanState(InspectionCanFollowID), True);
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
    LFollow := NewNyxButton(InspectionFollowID).WithText('Follow last visible');
    LCommands.Add(LFollow);
    LFollow.Binds.Enabled(NyxBooleanState(InspectionCanFollowID)).Done;
    LKeep := NewNyxButton(InspectionKeepID).WithText('Keep reading position');
    LCommands.Add(LKeep);
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
    LRecovery := NewNyxCard('recovered-drafts');
    LPage.Add(LRecovery);
    LRecovery.Configure.Layout(nlColumn).Padding(12).Gap(8).Done;
    LRecovery.Binds.Visible(NyxBooleanState(RecoveryVisibleID)).Done;
    LRecoverySummary := NewNyxLabel(RecoverySummaryID);
    LRecovery.Add(LRecoverySummary);
    LRecoverySummary.Binds.Text(NyxTextState(RecoverySummaryID)).Done;
    LRecoveryText := NewNyxMemo(RecoveryTextID).WithText('Recovered text');
    LRecovery.Add(LRecoveryText);
    LRecoveryText.Configure.Height(140).ReadOnly(True)
      .AccessibleName('Recovered draft text; null characters shown as backslash zero').Done;
    LRecoveryText.Binds.Value(NyxTextState(RecoveryTextID)).Done;
    LRecoveryCommands := NewNyxRow('recovery-commands');
    LRecovery.Add(LRecoveryCommands);
    LRecoveryCommands.Configure.Gap(8).Done;
    LRecoveryCommands.Configure.WhenViewport(TNyxViewportWidth.Below(560)).Layout(nlColumn).Done;
    LPreviousDraft := NewNyxButton(RecoveryPreviousID).WithText('Previous draft');
    LRecoveryCommands.Add(LPreviousDraft);
    LNextDraft := NewNyxButton(RecoveryNextID).WithText('Next draft');
    LRecoveryCommands.Add(LNextDraft);
    LDiscardDraft := NewNyxButton(RecoveryDiscardID);
    LRecoveryCommands.Add(LDiscardDraft);
    LDiscardDraft.Binds.Text(NyxTextState(RecoveryDiscardTextID)).Done;
  except
    Result.Free;
    raise;
  end;
end;

constructor TInspectionCallback.Create(const AContext: INyxApplicationContext;
  const AInspection, APeer: INyxCollectionView; const APosition: INyxCollectionPositionState;
  const ARecovery: INyxCollectionDraftRecovery);
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
  FFollowing := NyxCollectionFollowing(AInspection);
  FPosition := APosition;
  FRecovery := ARecovery;
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
  FFollowing := nil;
  FPeer := nil;
  FPosition := nil;
  FRecovery := nil;
  FRecovered := nil;
  FDiscard := nil;
  FGeneration := nil;
  FContext := nil;
end;

procedure TInspectionCallback.StatusChanged(const AStatus: TNyxCollectionInspectionObservation;
  ACause: TNyxCollectionInspectionCause);
var
  LText: TNyxText;
  LLease: INyxEventCallback;
  LRecovery: TNyxCollectionPositionRecovery;
  LPosition: INyxCollectionPositionState;
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

    if AStatus.FollowMode = ncfLastVisible then
    begin
      LText := LText + ' Following the last visible row when its identity changes. Choose Keep reading position to stop.';
    end
    else
    begin
      LText := LText + ' Your reading position is retained.';
    end;

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

  LPosition := FPosition;

  if (AStatus.Mode = ncimFollowing) and (LPosition <> nil) and LPosition.Active then
  begin
    LRecovery := LPosition.Recovery;

    if (LRecovery.CurrentRevision = AStatus.DisplayRevision) and
      (LRecovery.ObservationSerial <> FPositionSerial) then
    begin
      FPositionSerial := LRecovery.ObservationSerial;
      case LRecovery.Kind of
        ncpFollowing:
          LText := LText + ' The reading row disappeared; showing its next surviving record.';
        ncpPreceding:
          LText := LText + ' The reading row disappeared; showing its previous surviving record.';
        ncpReset:
          LText := LText + ' The reading position was reset; choose a current record.';
      else
        begin
          { Retained/empty/unavailable positions need no recovery warning. }
        end;
      end;
    end;
  end;

  if not Active then
  begin
    Exit;
  end;
  PublishState(LText);
end;

{ A physical memo cannot display literal NUL consistently on both hosts. Escape
  only its preview; the journal retains the complete original bytes and caret. }
function DraftPreview(const AText: TNyxText): TNyxText;
var
  LIndex: Integer;
  LStart: Integer;
begin
  Result := '';
  LStart := 1;
  for LIndex := 1 to Length(AText) do
  begin

    if AText[LIndex] = #0 then
    begin
      Result := Result + Copy(AText, LStart, LIndex - LStart) + '\0';
      LStart := LIndex + 1;
    end;
  end;
  Result := Result + Copy(AText, LStart, Length(AText) - LStart + 1);
end;

{ Explain lost automatic compatibility without implying that an old draft may
  safely be applied to a current row. The journal keeps the typed reason. }
function DraftLossCaption(AReason: TNyxCollectionDraftLoss): TNyxText;
begin
  case AReason of
    ncdlRowRemoved: Result := 'The record was removed.';
    ncdlIdentityReplaced: Result := 'The original record was replaced.';
    ncdlHidden: Result := 'The record is hidden by the current view.';
    ncdlValueChanged: Result := 'The accepted field value changed.';
    ncdlFieldUnavailable: Result := 'The edited field is no longer available.';
    ncdlUnknownIdentity: Result := 'The original record identity could not be verified.';
  end;
end;

procedure TInspectionCallback.PublishState(const AStatus: TNyxText);
var
  LJournal: INyxCollectionDraftRecovery;
  LEntry: INyxRecoveredCollectionDraft;
  LObservation: TNyxDraftRecoveryObservation;
  LSummary: TNyxText;
  LText: TNyxText;
  LCaption: TNyxText;
  LCanFollow: Boolean;
  LInspection: INyxCollectionInspection;
begin
  LJournal := FRecovery;
  LObservation := Default(TNyxDraftRecoveryObservation);
  LSummary := 'No recovered drafts.';
  LText := '';
  LCaption := 'Discard draft';

  if LJournal <> nil then
  begin
    LObservation := LJournal.Capture;
  end;

  if not Active then
  begin
    Exit;
  end;
  { An alternative inspection getter may retire this binding while returning
    its value. Keep that exact interface alive and recheck authority afterward. }
  LInspection := FInspection;

  if LInspection = nil then
  begin
    Exit;
  end;
  LCanFollow := LInspection.CaptureInspection.Mode <> ncimPaused;

  if not Active then
  begin
    Exit;
  end;

  if LObservation.Count > 0 then
  begin

    if (FRecoveryIndex < 0) or (FRecoveryIndex >= LObservation.Count) then
    begin
      FRecoveryIndex := 0;
    end;
    LEntry := LJournal.EntryAt(FRecoveryIndex);

    if not Active then
    begin
      Exit;
    end;

    if LEntry <> FRecovered then
    begin
      FDiscard := nil;
    end;
    FRecovered := LEntry;
    LText := DraftPreview(LEntry.Cell.Text);
    LSummary := 'Recovered draft ' + TNyxText(IntToStr(FRecoveryIndex + 1)) + ' of ' +
      TNyxText(IntToStr(LObservation.Count)) + ' / ' + LEntry.Cell.Item.ID + ' / ' +
      LEntry.Cell.Field.Name + '. ' + DraftLossCaption(LEntry.Reason) +
      ' Your text is safe; review and copy it before discarding.';

    if LObservation.Warning then
    begin
      LSummary := LSummary + ' The journal has reached its configured retention warning.';
    end;

    if FDiscard = LEntry then
    begin
      LSummary := LSummary + ' Discard this draft? Choose Confirm discard to remove this exact entry.';
      LCaption := 'Confirm discard';
    end;
  end
  else
  begin
    FRecovered := nil;
    FDiscard := nil;
  end;

  if not Active then
  begin
    Exit;
  end;
  FContext.State.Apply([
    NyxStateValue(NyxTextState(InspectionStatusID), AStatus),
    NyxStateValue(NyxTextState(RecoverySummaryID), LSummary),
    NyxStateValue(NyxTextState(RecoveryTextID), LText),
    NyxStateValue(NyxTextState(RecoveryDiscardTextID), LCaption),
    NyxStateValue(NyxBooleanState(RecoveryVisibleID), LObservation.Count > 0),
    NyxStateValue(NyxBooleanState(InspectionCanFollowID), LCanFollow)]);
end;

procedure TInspectionCallback.RecoveryAction(const AID: TNyxText);
var
  LJournal: INyxCollectionDraftRecovery;
  LEntry: INyxRecoveredCollectionDraft;
  LCount: Integer;
  LStatus: TNyxText;
begin
  LJournal := FRecovery;

  if LJournal = nil then
  begin
    Exit;
  end;
  LStatus := FContext.State.GetValue(NyxTextState(InspectionStatusID));
  LCount := LJournal.Capture.Count;

  if not Active or (LCount = 0) then
  begin
    Exit;
  end;

  if (FRecoveryIndex < 0) or (FRecoveryIndex >= LCount) then
  begin
    FRecoveryIndex := 0;
  end;

  if AID = RecoveryDiscardID then
  begin
    LEntry := LJournal.EntryAt(FRecoveryIndex);

    if not Active then
    begin
      Exit;
    end;

    if (FDiscard = LEntry) and (FRecovered = LEntry) then
    begin
      LJournal.Discard(LEntry);
      FDiscard := nil;
    end
    else
    begin
      FRecovered := LEntry;
      FDiscard := LEntry;
    end;
  end
  else
  begin
    FDiscard := nil;

    if AID = RecoveryNextID then
    begin
      FRecoveryIndex := (FRecoveryIndex + 1) mod LCount;
    end
    else if FRecoveryIndex = 0 then
    begin
      FRecoveryIndex := LCount - 1;
    end
    else
    begin
      Dec(FRecoveryIndex);
    end;
  end;

  if Active then
  begin
    PublishState(LStatus);
  end;
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
  end
  else if (AEvent.OriginID = InspectionFollowID) and (FFollowing <> nil) then
  begin
    FFollowing.FollowLatest;
  end
  else if (AEvent.OriginID = InspectionKeepID) and (FFollowing <> nil) then
  begin
    FFollowing.KeepReading;
  end
  else if (AEvent.OriginID = RecoveryPreviousID) or (AEvent.OriginID = RecoveryNextID) or
    (AEvent.OriginID = RecoveryDiscardID) then
  begin
    RecoveryAction(AEvent.OriginID);
  end;
end;

constructor TNyxLiveInspectionDemo.Create(const AContext: INyxApplicationContext;
  const AEvents: INyxEvents; const AInspection, APeer: INyxCollectionView;
  const APosition: INyxCollectionPositionState; const ARecovery: INyxCollectionDraftRecovery);
var
  LCallback: TInspectionCallback;
begin
  inherited Create;

  if AEvents = nil then
  begin
    raise ENyxCollectionInspection.Create('The demo needs its mounted event router');
  end;
  LCallback := TInspectionCallback.Create(AContext, AInspection, APeer, APosition, ARecovery);
  FCallback := LCallback;
  FCallbackObject := LCallback;
  SetLength(FSubscriptions, 8);
  FSubscriptions[0] := AEvents.On(NyxControlEvents(InspectionPauseID), ntClick)
    .Policy(neSequential).Subscribe(FCallback);
  FSubscriptions[1] := AEvents.On(NyxControlEvents(InspectionResumeID), ntClick)
    .Policy(neSequential).Subscribe(FCallback);
  FSubscriptions[2] := AEvents.On(NyxControlEvents(InspectionStepID), ntClick)
    .Policy(neSequential).Subscribe(FCallback);
  FSubscriptions[3] := AEvents.On(NyxControlEvents(RecoveryPreviousID), ntClick)
    .Policy(neSequential).Subscribe(FCallback);
  FSubscriptions[4] := AEvents.On(NyxControlEvents(RecoveryNextID), ntClick)
    .Policy(neSequential).Subscribe(FCallback);
  FSubscriptions[5] := AEvents.On(NyxControlEvents(RecoveryDiscardID), ntClick)
    .Policy(neSequential).Subscribe(FCallback);
  FSubscriptions[6] := AEvents.On(NyxControlEvents(InspectionFollowID), ntClick)
    .Policy(neSequential).Subscribe(FCallback);
  FSubscriptions[7] := AEvents.On(NyxControlEvents(InspectionKeepID), ntClick)
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
