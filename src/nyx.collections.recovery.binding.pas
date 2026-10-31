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
unit nyx.collections.recovery.binding;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.application.context, nyx.events, nyx.collections.mount,
  nyx.collections.recovery.panel, nyx.collections.recovery.status, nyx.files.save;

type
  { A journal entry is admitted storage; a held receipt is an exact snapshot of
    work the journal refused. Neither choice grants authority to restore text. }
  TNyxRecoveryReviewKind = (nrrNone, nrrJournal, nrrHeld);
  { Small copied runtime status. EntryIndex is zero based for a journal review
    and -1 otherwise; no text vector, live
    widget, application, journal, callback or mount is exposed. }
  TNyxRecoveryPanelObservation = record
    Active: Boolean;
    Status: TNyxCollectionRecoveryStatus;
    ReviewKind: TNyxRecoveryReviewKind;
    EntryIndex: Integer;
    ConfirmingDiscard: Boolean;
    Exporting: Boolean;
  end;
  { UI-owner binding; retain beside the application and Close before disposal.
    It writes only the panel's declared scalar state, never source rows, query,
    selection, disclosure or saved design. Discard is explicit and confirmed;
    pending discard uses the mount's exact receipt and explicit Refresh. Export
    never consumes a draft and starts inside the original user action. }
  INyxCollectionRecoveryBinding = interface(IInterface)
    ['{2EF5B0CC-EC03-48EF-8A85-99A848264ECB}']
    procedure Refresh;
    function Capture: TNyxRecoveryPanelObservation;
    procedure ConfigureExport(const AOptions: TNyxFileSaveStreamOptions);
    procedure Close;
  end;

{ The authored panel/state must already exist in this application's document.
  The factory requires the mount's optional recovery-notification facet, so its
  automatic updates cannot silently degrade to a polling-only presentation.
  A missing save host disables Export with explanatory text; review/discard/
  retry still work. Runtime options do not change authored defaults. }
function BindNyxCollectionRecoveryPanel(const AContext: INyxApplicationContext;
  const AEvents: INyxEvents; const AMount: INyxCollectionMount;
  const AParts: TNyxCollectionRecoveryParts;
  const ASaveHost: INyxFileSaveHost = nil): INyxCollectionRecoveryBinding;

implementation

uses SysUtils, Math, nyx.types, nyx.behavior, nyx.state, nyx.scheduler, nyx.publication, nyx.bytes,
  nyx.bytes.sources,
  nyx.files, nyx.collections, nyx.collections.edits, nyx.collections.recovery,
  nyx.collections.recovery.controls;

type
  { The router retains this receiver, not its public binding owner. Context and
    mount flags revoke authority; its notification token borrows the receiver.
    Entered methods retain Self. No renderer/document/widget is retained. }
  TRecoveryCallback = class(TNyxEventCallback)
  private
    FContext: INyxApplicationContext;
    FGeneration: INyxCancellationScope;
    FScheduler: INyxScheduler;
    FMount: INyxCollectionMount;
    FInput: INyxCollectionInputLifetime;
    FNotifications: INyxCollectionRecoveryNotifications;
    FToken: INyxCollectionSubscription;
    FJournal: INyxCollectionDraftRecovery;
    FPendingMount: INyxCollectionPendingDraftMount;
    FParts: TNyxCollectionRecoveryParts;
    FEntry: INyxRecoveredCollectionDraft;
    FPending: INyxPendingCollectionDraft;
    FKind: TNyxRecoveryReviewKind;
    FIndex: Integer;
    FJournalRevision: Integer;
    FConfirm: Boolean;
    FClosed: Boolean;
    FRefreshing: Boolean;
    FDirty: Boolean;
    FSerial: Integer;
    FOutcome: TNyxText;
    FObservation: TNyxRecoveryPanelObservation;
    FSaveHost: INyxFileSaveHost;
    FSaveOptions: TNyxFileSaveStreamOptions;
    FSaveRequest: INyxFileSaveRequest;
    FExporting: Boolean;
    FExportBytes: Integer;
    FExportLimit: Integer;
    function Active: Boolean;
    procedure Changed(const AStatus: TNyxCollectionRecoveryStatus);
    procedure SelectEntry(const AStatus: TNyxCollectionRecoveryStatus);
    procedure Publish(const AStatus: TNyxCollectionRecoveryStatus);
    procedure Discard;
    procedure Export;
    procedure Saved(const AResult: TNyxFileSaveResult);
  public
    constructor Create(const AContext: INyxApplicationContext;
      const AMount: INyxCollectionMount; const AParts: TNyxCollectionRecoveryParts;
      const ASaveHost: INyxFileSaveHost);
    destructor Destroy; override;
    procedure Refresh;
    procedure Close;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Owns subscriptions separately from the callback. This avoids the cycle of
    a router-owned callback retaining subscriptions that themselves retain it.
    Closing revokes the receiver before unregistering any borrowed event route. }
  TRecoveryBinding = class(TInterfacedObject, INyxCollectionRecoveryBinding)
  private
    FCallback: INyxEventCallback;
    FCore: TRecoveryCallback;
    FSubscriptions: array of INyxEventSubscription;
  public
    constructor Create(const AContext: INyxApplicationContext;
      const AEvents: INyxEvents; const AMount: INyxCollectionMount;
      const AParts: TNyxCollectionRecoveryParts; const ASaveHost: INyxFileSaveHost);
    destructor Destroy; override;
    procedure Refresh;
    function Capture: TNyxRecoveryPanelObservation;
    procedure ConfigureExport(const AOptions: TNyxFileSaveStreamOptions);
    procedure Close;
  end;

constructor TRecoveryCallback.Create(const AContext: INyxApplicationContext;
  const AMount: INyxCollectionMount; const AParts: TNyxCollectionRecoveryParts;
  const ASaveHost: INyxFileSaveHost);
var
  LAdmission: INyxCollectionInputAdmission;
  LNotifications: INyxCollectionRecoveryNotifications;
  LPending: INyxCollectionPendingDraftMount;
begin
  inherited Create;
  AParts.Validate;

  if (AContext = nil) or not AContext.Active or (AMount = nil) or
    not AMount.Connected or not Supports(AMount, INyxCollectionInputAdmission, LAdmission) or
    not Supports(AMount, INyxCollectionRecoveryNotifications, LNotifications) or
    not Supports(AMount, INyxCollectionPendingDraftMount, LPending) then
  begin
    raise ENyxCollection.Create('Recovery panel requires an active context and an observable mount');
  end;
  FContext := AContext;
  FScheduler := AContext.Scheduler;
  FScheduler.RequireUI;
  FGeneration := AContext.Generation;
  FMount := AMount;
  FInput := LAdmission.GetInputLifetime;
  FNotifications := LNotifications;
  FPendingMount := LPending;
  FParts := AParts;
  FSaveHost := ASaveHost;
  FSaveOptions := NyxFileSaveStreamOptions(NyxFileSaveOptions('unfinished-edit.txt')
    .Title('Export unfinished text').MediaType(NyxMediaType(nfmtText)).Allow('txt')
    .DownloadActivation(nfdaPlatform));
  FJournal := NyxCollectionDraftRecovery(AMount);

  if not Active or (FJournal = nil) then
  begin
    raise ENyxCollection.Create('Recovery panel lost its original mount while connecting');
  end;
  FToken := FNotifications.SubscribeRecoveryStatus(
    {$ifdef PAS2JS}@{$endif}Changed);
  Refresh;
end;

destructor TRecoveryCallback.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TRecoveryCallback.Active: Boolean;
begin
  Result := not FClosed and (FContext <> nil) and FContext.Active and
    (FGeneration <> nil) and not FGeneration.Cancelled and
    (FInput <> nil) and FInput.Current and (FMount <> nil) and FMount.Connected;
end;

procedure TRecoveryCallback.Close;
var
  LRequest: INyxFileSaveRequest;
begin

  if FClosed then
  begin
    Exit;
  end;
  FClosed := True;

  if FToken <> nil then
  begin
    FToken.Disconnect;
    FToken := nil;
  end;
  LRequest := FSaveRequest;
  FSaveRequest := nil;
  FExporting := False;
  FEntry := nil;
  FPending := nil;
  FJournal := nil;
  FMount := nil;
  FNotifications := nil;
  FPendingMount := nil;
  FInput := nil;
  FContext := nil;
  FGeneration := nil;
  FObservation.Active := False;

  if LRequest <> nil then
  begin
    LRequest.Cancel;
  end;
end;

procedure TRecoveryCallback.Changed(const AStatus: TNyxCollectionRecoveryStatus);
var
  LLease: INyxEventCallback;
begin
  LLease := Self;

  if FSerial = High(Integer) then
  begin
    Close;
    raise ENyxCollection.Create('Recovery presentation serial is exhausted');
  end;
  Inc(FSerial);
  FDirty := True;
  Refresh;
end;

procedure TRecoveryCallback.SelectEntry(const AStatus: TNyxCollectionRecoveryStatus);
var
  LEntry: INyxRecoveredCollectionDraft;
begin

  if (FKind = nrrHeld) and not AStatus.Held then
  begin
    FKind := nrrNone;
    FPending := nil;
    FConfirm := False;
  end;

  if FKind = nrrHeld then
  begin
    Exit;
  end;
  LEntry := nil;

  if AStatus.Journal.Count > 0 then
  begin

    if (FIndex < 0) or (FIndex >= AStatus.Journal.Count) then
    begin
      FIndex := 0;
    end;
    LEntry := FJournal.EntryAt(FIndex);
  end;

  if (LEntry <> FEntry) or (FJournalRevision <> AStatus.Journal.Revision) then
  begin

    if FConfirm then
    begin
      FOutcome := 'The recovery list changed. Review the current edit before confirming discard again.';
    end;
    FConfirm := False;
  end;
  FJournalRevision := AStatus.Journal.Revision;
  FEntry := LEntry;
  FKind := nrrNone;

  if LEntry <> nil then
  begin
    FKind := nrrJournal;
  end;
end;

procedure TRecoveryCallback.Publish(const AStatus: TNyxCollectionRecoveryStatus);
var
  LPresentation: TNyxRecoveredDraftPresentation;
  LReview: TNyxRecoveredDraftBindings;
  LSummary: TNyxText;
  LDiscard: TNyxText;
  LHasReview: Boolean;
  LNext: TNyxRecoveryPanelObservation;
  LState: TNyxState;
  LPlan: INyxPreparedPublication;
  LSerial: Integer;
begin
  LSerial := FSerial;
  LPresentation := Default(TNyxRecoveredDraftPresentation);
  LHasReview := False;

  if (FKind = nrrJournal) and (FEntry <> nil) then
  begin
    LPresentation := NyxRecoveredDraftPresentation(FEntry);
    LPresentation.Notice := 'Review and export keep your text. Discard removes only the reviewed edit ' +
      'after confirmation; source records stay unchanged. Controls and backslashes are escaped in both previews.';
    LHasReview := True;
  end
  else if (FKind = nrrHeld) and (FPending <> nil) then
  begin
    LPresentation := NyxPendingDraftPresentation(FPending.Cell);
    LHasReview := True;
  end;
  LSummary := TNyxText(IntToStr(AStatus.Journal.Count)) + ' recovered edits.';

  if AStatus.Journal.Count = 1 then
  begin
    LSummary := '1 recovered edit.';
  end;

  if AStatus.Held then
  begin
    LSummary := LSummary + ' Recovery capacity is full. Review the held edit, free space in the list, ' +
      'or increase the recovery limit, then retry this display. Other views can keep updating.';
  end
  else if AStatus.Journal.Count > 0 then
  begin
    LSummary := LSummary + ' Review ' + TNyxText(IntToStr(FIndex + 1)) + ' of ' +
      TNyxText(IntToStr(AStatus.Journal.Count)) + '. Newer source data has not been overwritten.';
  end;

  if FSaveHost = nil then
  begin
    LSummary := LSummary + ' Configure a file-save service to enable Export.';
  end;
  LDiscard := 'Discard reviewed edit';

  if FConfirm then
  begin
    LDiscard := 'Confirm discard of reviewed edit';
  end;
  LNext := Default(TNyxRecoveryPanelObservation);
  LNext.Active := Active;
  LNext.Status := AStatus;
  LNext.ReviewKind := FKind;
  LNext.EntryIndex := -1;

  if FKind = nrrJournal then
  begin
    LNext.EntryIndex := FIndex;
  end;
  LNext.ConfirmingDiscard := FConfirm;
  LNext.Exporting := FExporting;
  LReview := FParts.Review;

  if not Active or (LSerial <> FSerial) then
  begin
    FDirty := Active;
    Exit;
  end;
  LState := FContext.State;

  if not Active or (LSerial <> FSerial) then
  begin
    FDirty := Active;
    Exit;
  end;
  LPlan := LState.PrepareApply([
    NyxStateValue(FParts.Text(nrtSummary), LSummary),
    NyxStateValue(FParts.Text(nrtOutcome), FOutcome),
    NyxStateValue(FParts.Text(nrtDiscardCaption), LDiscard),
    NyxStateValue(FParts.Flag(nrfVisible), (AStatus.Journal.Count > 0) or AStatus.Held),
    NyxStateValue(FParts.Flag(nrfReviewVisible), LHasReview),
    NyxStateValue(FParts.Flag(nrfPrevious), (FKind = nrrJournal) and (FIndex > 0)),
    NyxStateValue(FParts.Flag(nrfNext), (FKind = nrrJournal) and (FIndex < AStatus.Journal.Count - 1)),
    NyxStateValue(FParts.Flag(nrfReviewHeld), AStatus.Held),
    NyxStateValue(FParts.Flag(nrfReviewList), (FKind = nrrHeld) and (AStatus.Journal.Count > 0)),
    NyxStateValue(FParts.Flag(nrfExport), LHasReview and (FSaveHost <> nil) and not FExporting),
    NyxStateValue(FParts.Flag(nrfDiscard), LHasReview and not FExporting),
    NyxStateValue(FParts.Flag(nrfRetry), AStatus.Held),
    NyxStateValue(FParts.Flag(nrfIdle), not FExporting),
    NyxStateValue(LReview.Heading, LPresentation.Heading),
    NyxStateValue(LReview.Provenance, LPresentation.Provenance),
    NyxStateValue(LReview.Reason, LPresentation.Reason),
    NyxStateValue(LReview.Notice, LPresentation.Notice),
    NyxStateValue(LReview.Draft, LPresentation.Draft),
    NyxStateValue(LReview.Accepted, LPresentation.Accepted)]);
  try
    LPlan.Validate;

    if not Active or (LSerial <> FSerial) then
    begin
      FDirty := Active;
      Exit;
    end;
    LPlan.Install;
    FObservation := LNext;
    LPlan.Notify;
  finally
    LPlan.Retire;
  end;
end;

procedure TRecoveryCallback.Refresh;
var
  LLease: INyxEventCallback;
  LStatus: TNyxCollectionRecoveryStatus;
  LSerial: Integer;
begin
  LLease := Self;

  if FRefreshing or not Active then
  begin
    Exit;
  end;
  FScheduler.RequireUI;
  FRefreshing := True;
  try
    repeat
      FDirty := False;
      LSerial := FSerial;
      LStatus := FNotifications.CaptureRecoveryStatus;

      if not Active then
      begin
        Exit;
      end;
      SelectEntry(LStatus);

      if not Active then
      begin
        Exit;
      end;

      if LSerial <> FSerial then
      begin
        FDirty := True;
        Continue;
      end;
      Publish(LStatus);
    until not FDirty or not Active;
  finally
    FRefreshing := False;
  end;
end;

procedure TRecoveryCallback.Discard;
var
  LEntry: INyxRecoveredCollectionDraft;
  LPending: INyxPendingCollectionDraft;
  LDiscarded: Boolean;
begin

  if FKind = nrrNone then
  begin
    Exit;
  end;

  if not FConfirm then
  begin
    FConfirm := True;
    FOutcome := 'Discard permanently removes only this reviewed draft. Click Confirm discard to continue.';
    Exit;
  end;
  FConfirm := False;
  LDiscarded := False;
  LEntry := FEntry;
  LPending := FPending;

  if (FKind = nrrJournal) and (LEntry <> nil) then
  begin
    LDiscarded := FJournal.Discard(LEntry);
  end
  else if (FKind = nrrHeld) and (LPending <> nil) then
  begin
    LDiscarded := FPendingMount.DiscardPendingDraft(LPending);

    if LDiscarded and Active then
    begin
      FPending := nil;
      FKind := nrrNone;
      FMount.Refresh;
    end;
  end;

  if LDiscarded then
  begin
    FOutcome := 'Discarded the reviewed edit. Source records were not changed.';
  end
  else
  begin
    FOutcome := 'The reviewed edit changed or is no longer available. Review it again before discarding.';
    FPending := nil;
    FKind := nrrNone;
  end;
end;

procedure TRecoveryCallback.Saved(const AResult: TNyxFileSaveResult);
var
  LLease: INyxEventCallback;
begin
  LLease := Self;

  if not Active or not FExporting then
  begin
    Exit;
  end;
  FExporting := False;
  FSaveRequest := nil;
  try
    ValidateNyxStreamFileSaveResult(AResult, FExportBytes, FExportLimit);
    FOutcome := 'Export: ' + NyxFileSaveOutcomeName(AResult.Outcome) +
      '. The original draft has not been discarded.';
  except
    on LError: Exception do
    begin
      FOutcome := 'The file-save service returned an invalid result: ' +
        TNyxText(LError.Message) + '. The original draft remains available.';
    end;
  end;
  Refresh;
end;

procedure TRecoveryCallback.Export;
var
  LCell: TNyxCollectionCellEdit;
  LSource: INyxByteSource;
  LStreaming: INyxFileSaveStreamHost;
  LRequest: INyxFileSaveRequest;
  LOptions: TNyxFileSaveOptions;
  LBytes: TNyxBytes;
  LHost: INyxFileSaveHost;
  LGeneration: INyxCancellationScope;
  LScheduler: INyxScheduler;
  LStreamOptions: TNyxFileSaveStreamOptions;

  function Current: Boolean;
  begin
    Result := Active and (FSaveHost = LHost) and (FGeneration = LGeneration) and
      (FScheduler = LScheduler);
  end;
begin

  if (FSaveHost = nil) or FExporting then
  begin
    Exit;
  end;
  LHost := FSaveHost;
  LGeneration := FGeneration;
  LScheduler := FScheduler;
  LStreamOptions := FSaveOptions;

  if (FKind = nrrJournal) and (FEntry <> nil) then
  begin
    LCell := FEntry.Cell;
  end
  else if (FKind = nrrHeld) and (FPending <> nil) then
  begin
    LCell := FPending.Cell;
  end
  else
  begin
    Exit;
  end;

  if not Current then
  begin
    Exit;
  end;
  FConfirm := False;
  LSource := NyxUTF8ByteSource(LCell.Text);
  FExportBytes := LSource.ByteCount;
  FExportLimit := LStreamOptions.ByteLimit;
  FExporting := True;
  FOutcome := 'Export pending. The original draft remains available.';
  try

    if Supports(LHost, INyxFileSaveStreamHost, LStreaming) then
    begin
      { Capability lookup is foreign code. Revocation there must precede any
        external save effect; entered requests are cancelled after return. }

      if not Current then
      begin
        Exit;
      end;
      LRequest := LStreaming.BeginStream(LStreamOptions, LSource, LScheduler, LGeneration,
        {$ifdef PAS2JS}@{$endif}Saved);
    end
    else
    begin

      if not Current then
      begin
        Exit;
      end;
      LOptions := LStreamOptions.FileOptions.MaximumBytes(
        Min(LStreamOptions.ByteLimit, NyxTextFileMaximumBytes));

      if FExportBytes > LOptions.ByteLimit then
      begin
        raise ENyxFile.Create('The reviewed text exceeds the configured byte-only save limit');
      end;
      FExportLimit := LOptions.ByteLimit;
      LBytes := NyxEncodeUTF8(LCell.Text);
      LRequest := LHost.BeginSave(LOptions, LBytes, LScheduler, LGeneration,
        {$ifdef PAS2JS}@{$endif}Saved);
    end;

    if Current and FExporting then
    begin

      if LRequest = nil then
      begin
        raise ENyxFile.Create('The file-save service did not return a request');
      end;
      FSaveRequest := LRequest;
    end
    else if LRequest <> nil then
    begin
      LRequest.Cancel;
    end;
  except
    on LError: Exception do
    begin
      FExporting := False;
      FOutcome := TNyxText(LError.Message) + '. The draft is unchanged.';
    end;
  end;
end;

procedure TRecoveryCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LLease: INyxEventCallback;
  LAction: TNyxRecoveryAction;
begin
  LLease := Self;

  if not Active or FRefreshing or FExporting or (AEvent.Trigger <> ntClick) or
    (AExecution = nil) or AExecution.Cancelled then
  begin
    Exit;
  end;
  FScheduler.RequireUI;

  if not Active then
  begin
    Exit;
  end;
  for LAction := Low(TNyxRecoveryAction) to High(TNyxRecoveryAction) do
  begin

    if AEvent.OriginID <> FParts.ActionID(LAction) then
    begin
      Continue;
    end;
    case LAction of
      nraPrevious, nraNext:
      begin
        FConfirm := False;
        FOutcome := '';
        FPending := nil;
        FKind := nrrNone;

        if LAction = nraPrevious then
        begin
          FIndex := Max(0, FIndex - 1);
        end
        else
        begin
          FIndex := Min(FJournal.Capture.Count - 1, FIndex + 1);
        end;
      end;
      nraReviewHeld:
      begin
        FConfirm := False;
        FPending := FPendingMount.CapturePendingDraft;

        if FPending <> nil then
        begin
          FKind := nrrHeld;
          FOutcome := 'Reviewing a snapshot of the held edit. Newer typing requires a fresh review.';
        end
        else
        begin
          FOutcome := 'No held edit is available for review.';
        end;
      end;
      nraReviewList:
      begin
        FConfirm := False;
        FPending := nil;
        FKind := nrrNone;
        FIndex := 0;
        FOutcome := '';
      end;
      nraExport:
      begin
        Export;
      end;
      nraDiscard:
      begin
        Discard;
      end;
      nraRetry:
      begin
        FConfirm := False;
        FMount.Refresh;

        if Active and NyxCollectionDraftRecoveryHeld(FMount) then
        begin
          FOutcome := 'Recovery capacity is still full. Free a recovered edit or increase its limit, then retry.';
        end
        else
        begin
          FOutcome := 'The display has caught up. Source records were not changed.';
        end;
      end;
    end;

    if Active then
    begin
      Refresh;
    end;
    Exit;
  end;
end;

constructor TRecoveryBinding.Create(const AContext: INyxApplicationContext;
  const AEvents: INyxEvents; const AMount: INyxCollectionMount;
  const AParts: TNyxCollectionRecoveryParts; const ASaveHost: INyxFileSaveHost);
var
  LCore: TRecoveryCallback;
  LAction: TNyxRecoveryAction;
begin
  inherited Create;

  if (AEvents = nil) or (AContext = nil) or (AEvents.Scheduler <> AContext.Scheduler) then
  begin
    raise ENyxCollection.Create('Recovery panel events must share the application scheduler');
  end;
  LCore := TRecoveryCallback.Create(AContext, AMount, AParts, ASaveHost);
  FCallback := LCore;
  FCore := LCore;
  SetLength(FSubscriptions, Ord(High(TNyxRecoveryAction)) + 1);
  for LAction := Low(TNyxRecoveryAction) to High(TNyxRecoveryAction) do
  begin
    FSubscriptions[Ord(LAction)] := AEvents.On(NyxControlEvents(AParts.ActionID(LAction)), ntClick)
      .Policy(neSequential).Subscribe(FCallback);
  end;
end;

destructor TRecoveryBinding.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TRecoveryBinding.Close;
var
  LLease: INyxEventCallback;
  LCore: TRecoveryCallback;
  LIndex: Integer;
begin
  LLease := FCallback;
  LCore := FCore;
  FCore := nil;
  FCallback := nil;
  try

    if LCore <> nil then
    begin
      LCore.Close;
    end;
  finally
    for LIndex := 0 to High(FSubscriptions) do
    begin

      if FSubscriptions[LIndex] <> nil then
      begin
        FSubscriptions[LIndex].Cancel;
      end;
    end;
    FSubscriptions := nil;
  end;
end;

procedure TRecoveryBinding.Refresh;
var
  LLease: INyxEventCallback;
  LCore: TRecoveryCallback;
begin
  LLease := FCallback;
  LCore := FCore;

  if LCore <> nil then
  begin
    LCore.Refresh;
  end;
end;

function TRecoveryBinding.Capture: TNyxRecoveryPanelObservation;
var
  LLease: INyxEventCallback;
  LCore: TRecoveryCallback;
begin
  LLease := FCallback;
  LCore := FCore;
  Result := Default(TNyxRecoveryPanelObservation);
  Result.EntryIndex := -1;

  if LCore <> nil then
  begin
    Result := LCore.FObservation;
    Result.Active := LCore.Active;
  end;
end;

procedure TRecoveryBinding.ConfigureExport(const AOptions: TNyxFileSaveStreamOptions);
var
  LLease: INyxEventCallback;
  LCore: TRecoveryCallback;
begin
  AOptions.Validate;
  LLease := FCallback;
  LCore := FCore;

  if (LCore = nil) or not LCore.Active or LCore.FExporting then
  begin
    raise ENyxFile.Create('Export configuration requires an idle live recovery binding');
  end;
  LCore.FScheduler.RequireUI;

  if not LCore.Active or LCore.FExporting then
  begin
    raise ENyxFile.Create('Recovery binding changed while configuring export');
  end;
  LCore.FSaveOptions := AOptions;
end;

function BindNyxCollectionRecoveryPanel(const AContext: INyxApplicationContext;
  const AEvents: INyxEvents; const AMount: INyxCollectionMount;
  const AParts: TNyxCollectionRecoveryParts;
  const ASaveHost: INyxFileSaveHost): INyxCollectionRecoveryBinding;
begin
  Result := TRecoveryBinding.Create(AContext, AEvents, AMount, AParts, ASaveHost);
end;

end.
