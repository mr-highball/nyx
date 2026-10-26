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
program nyx_collection_recovery_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, Grids, Windows,
  nyx.text, nyx.model, nyx.codec, nyx.state, nyx.application.lcl,
  nyx.collections, nyx.collections.mount, nyx.collections.recovery,
  nyx.collections.position, nyx.collections.query, nyx.live.inspection,
  nyx.service.monitor, nyx.files.save, nyx.bytes, nyx.bytes.sources, nyx.scheduler
  {$ifdef NYX_INSPECTION_GENERATED}, nyx.generated.view{$endif};

const
  CFirstDraft: TNyxText = 'A careful unfinished note 🌙';
  CSecondDraft: TNyxText = 'Another unfinished note 🌙';
  CThirdDraft: TNyxText = 'Keep this while the record is hidden 🌙';

type
  { Inline deterministic save host qualifies the UI's exact bytes and typed
    completion, without pretending that injected dialogs wrote a real file. }
  TReviewSaveHost = class(TInterfacedObject, INyxFileSaveHost, INyxFileSaveStreamHost)
  public
    Bytes: TNyxBytes;
    Outcome: TNyxFileSaveOutcome;
    Calls: Integer;
    StreamCalls: Integer;
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
    function BeginStream(const AOptions: TNyxFileSaveStreamOptions;
      const ASource: INyxByteSource; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

  { Uses the production request lifetime and receipt validation with an inline
    terminal outcome; it owns copied bytes and no mount/editor references. }
  TReviewSaveRequest = class(TNyxFileSaveRequest)
  public
    procedure Finish(AOutcome: TNyxFileSaveOutcome);
    function CollectSmallFixture: TNyxBytes;
  end;

procedure TReviewSaveRequest.Finish(AOutcome: TNyxFileSaveOutcome);
var
  LCount: Integer;
begin
  LCount := 0;

  if AOutcome = nfsoWritten then
  begin
    LCount := FExpectedBytes;
  end;
  Complete(AOutcome, FOptions.SuggestedName, LCount);
end;

function TReviewSaveRequest.CollectSmallFixture: TNyxBytes;
var
  LChunk: TNyxBytes;
  LIndex: Integer;
  LPosition: Integer;
begin
  Result := nil;

  if FExpectedBytes > 65536 then
  begin
    raise Exception.Create('Inline recovery save fixture is deliberately small');
  end;
  SetLength(Result, FExpectedBytes);
  LPosition := 0;
  repeat
    LChunk := ReadPayloadChunk;
    for LIndex := 0 to High(LChunk) do
    begin
      Result[LPosition] := LChunk[LIndex];
      Inc(LPosition);
    end;
  until Length(LChunk) = 0;
end;

function TReviewSaveHost.BeginStream(const AOptions: TNyxFileSaveStreamOptions;
  const ASource: INyxByteSource; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LRequest: TReviewSaveRequest;
begin
  LRequest := TReviewSaveRequest.CreateStream(AOptions, ASource, AScheduler, ACancellation, AReply);
  Result := LRequest;
  Bytes := LRequest.CollectSmallFixture;
  Inc(Calls);
  Inc(StreamCalls);
  LRequest.Finish(Outcome);
end;

function TReviewSaveHost.BeginSave(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LRequest: TReviewSaveRequest;
begin
  AOptions.Admit(ABytes);
  Bytes := Copy(ABytes, 0, Length(ABytes));
  Inc(Calls);
  LRequest := TReviewSaveRequest.Create(AOptions, ABytes, AScheduler, ACancellation, AReply);
  Result := LRequest;
  LRequest.Finish(Outcome);
end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    WriteLn('ASSERTION ', GChecks, ' / ', AReason);
    Flush(Output);
    raise Exception.Create('Recovery controls: ' + AReason);
  end;
end;

{ Flush phase evidence before entering host input/message loops. A bounded
  timeout must retain the last completed operation, not just an empty stdout. }
procedure Stage(const AName: TNyxText);
begin
  WriteLn('TRACE recovery ', GChecks, ' / ', AName);
  Flush(Output);
end;

procedure Journey(AWidth: Integer);
var
  LDocument: TNyxDocument;
  LApplication: TNyxLCLApplication;
  LDemo: TNyxLiveInspectionDemo;
  LFirst: INyxCollectionMount;
  LPeer: INyxCollectionMount;
  LJournal: INyxCollectionDraftRecovery;
  LOriginal: INyxRecoveredCollectionDraft;
  LRemoved: INyxRecoveredCollectionDraft;
  LGrid: TStringGrid;
  LEditor: TCustomEdit;
  LStore: INyxCollection;
  LBefore: INyxCollectionSnapshot;
  LGateway: TNyxItemRef;
  LSaved: TNyxText;
  LColumn: Integer;
  LBudget: INyxCollectionDraftRecoveryBudget;
  LCapacity: TNyxDraftRecoveryBudgetObservation;
  LInput: INyxCollectionInputAdmission;
  LSave: TReviewSaveHost;
  LSaveHost: INyxFileSaveHost;
  LPending: INyxCollectionPendingDraftMount;
  LReview: INyxPendingCollectionDraft;

  procedure Click(const AID: TNyxText);
  begin
    Stage('click ' + AID);
    SendMessage(TWinControl(LApplication.View.ControlFor(AID)).Handle, BM_CLICK, 0, 0);
    Stage('click callback returned');
    Application.ProcessMessages;
    Stage('click messages drained');
  end;

  procedure BeginDraft(const AText: TNyxText);
  var
    LIndex: Integer;
  begin
    LFirst.Select(LGateway);
    LGrid := TStringGrid(LApplication.View.ControlFor(ServiceTableID));
    LColumn := -1;
    for LIndex := 0 to LGrid.ColCount - 1 do
    begin

      if Pos('Notes', TNyxText(LGrid.Cells[LIndex, 0])) = 1 then
      begin
        LColumn := LIndex;
        Break;
      end;
    end;
    Check(LColumn >= 0, 'the public center pane exposes its Notes field');
    LGrid.Row := LFirst.View.Snapshot.IndexOf(LGateway) + 1;
    LGrid.Col := LColumn;
    LGrid.SetFocus;
    LGrid.EditorMode := True;
    Application.ProcessMessages;
    Check(LGrid.EditorMode and (LGrid.Editor is TCustomEdit), 'the actual native cell editor is active');
    LEditor := TCustomEdit(LGrid.Editor);
    LBefore := LStore.Snapshot;
    LEditor.Text := AText;
    LEditor.SelStart := 2;
    LEditor.SelLength := 4;
    Check((TNyxText(LEditor.Text) = AText) and (LStore.Snapshot = LBefore),
      'typing changes only the actual editor draft, preserving supplementary text and source revision');
  end;

begin
  {$ifdef NYX_INSPECTION_GENERATED}
  LDocument := BuildNyxDocument;
  {$else}
  LDocument := BuildLiveInspectionDocument;
  {$endif}
  LApplication := nil;
  LDemo := nil;
  try
    LSaved := TNyxCodec.Encode(LDocument);
    LApplication := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LApplication.Window.ClientWidth := AWidth;
    LApplication.Window.ClientHeight := 1000;
    LApplication.Window.Show;
    Application.ProcessMessages;
    LFirst := LApplication.View.CollectionMount(ServiceTableID);
    LPeer := LApplication.View.CollectionMount(InspectionPeerID);
    LStore := LFirst.View.Store;
    LJournal := NyxCollectionDraftRecovery(LFirst);
    LSave := TReviewSaveHost.Create;
    LSaveHost := LSave;
    LSave.Outcome := nfsoWritten;
    LDemo := TNyxLiveInspectionDemo.Create(LApplication.Context, LApplication.View.Events,
      LFirst.View, LPeer.View, NyxCollectionPositionState(LFirst), LJournal, LFirst, LSaveHost);
    LGateway := NyxItem(NyxCollection(ServiceCollectionName), 'gateway');
    Check(not LApplication.View.ControlFor('recovered-drafts').Visible,
      'the ordinary bound recovery card starts hidden');
    BeginDraft(CFirstDraft);
    LStore.Update(NyxCollectionItem(NyxItem(LGateway.Collection, 'reports'))
      .WithValue(NyxTextField('status'), 'Busy'));
    Check((LJournal.Capture.Count = 0) and LGrid.EditorMode and
      (TNyxText(TCustomEdit(LGrid.Editor).Text) = CFirstDraft),
      'an unrelated live update keeps the real editor and its unfinished text');
    LStore.Update(NyxCollectionItem(LGateway).WithValue(NyxTextField('notes'), 'Producer note'));
    Check((LJournal.Capture.Count = 1) and (LJournal.EntryAt(0).Reason = ncdlValueChanged) and
      (LJournal.EntryAt(0).Cell.Text = CFirstDraft), 'external accepted-value change recovers the exact physical draft');
    Check(LStore.Snapshot.Item(LGateway).GetValue(NyxTextField('notes')) = 'Producer note',
      'recovery does not write the old draft into the live source');
    Check(LApplication.View.InputFor(RecoveryTextID) is TMemo,
      'the recovery text exposes its actual native memo through the input contract');
    Check(LApplication.View.ControlFor('recovered-drafts').Visible and
      TMemo(LApplication.View.InputFor(RecoveryTextID)).ReadOnly and
      (Pos(CFirstDraft, TNyxText(TMemo(LApplication.View.InputFor(RecoveryTextID)).Text)) > 0),
      'ordinary Nyx bindings reveal the recovered text in a readonly memo');
    LOriginal := LJournal.EntryAt(0);
    BeginDraft(CSecondDraft);
    LBefore := LStore.Snapshot;
    LStore.Apply([NyxRemove(LGateway), NyxInsert(LBefore.IndexOf(LGateway), LBefore.Item(LGateway))]);
    Check((LStore.Snapshot.Revision = LBefore.Revision + 1) and
      (LJournal.Capture.Count = 2) and (LJournal.EntryAt(1).Reason = ncdlIdentityReplaced) and
      (LJournal.EntryAt(1).Cell.Text = CSecondDraft),
      'same-value/order replacement recovers the old editor instead of lending it to the new row');
    BeginDraft(CThirdDraft);
    LFirst.View.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxTextField('name')).EqualTo('Report scheduler')));
    Check((LJournal.Capture.Count = 3) and (LJournal.EntryAt(2).Reason = ncdlHidden) and
      (LJournal.EntryAt(2).Cell.Text = CThirdDraft), 'filtering the edited row out recovers its text');
    LFirst.View.ConfigureQuery(Default(TNyxCollectionQuery));
    Check(LJournal.Capture.Count = 3, 'clearing a filter does not reapply or discard recovered text');
    Click(RecoveryDiscardID);
    Check((LJournal.Capture.Count = 3) and
      (LApplication.State.GetValue(NyxTextState(RecoveryDiscardTextID)) = 'Confirm discard'),
      'the first discard click warns without removing user work');
    Click(RecoveryNextID);
    Check(LApplication.State.GetValue(NyxTextState(RecoveryDiscardTextID)) = 'Discard draft',
      'changing the reviewed entry cancels the older discard confirmation');
    LRemoved := LJournal.EntryAt(1);
    Click(RecoveryDiscardID);
    Check(LJournal.Capture.Count = 3, 'the newly reviewed draft needs its own confirmation');
    LBefore := LStore.Snapshot;
    Click(RecoveryDiscardID);
    Check((LJournal.Capture.Count = 2) and (LJournal.EntryAt(0) = LOriginal) and
      (LStore.Snapshot = LBefore) and (LRemoved.Cell.Text = CSecondDraft),
      'confirmation discards only the reviewed entry, leaving source data and independent facts untouched');
    Check(TNyxCodec.Encode(LDocument) = LSaved, 'recovery review is runtime state, not authored history');
    Stage('baseline completed');
    LBudget := NyxCollectionDraftRecoveryBudget(LJournal);
    Supports(LFirst, INyxCollectionInputAdmission, LInput);
    LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(2));
    BeginDraft(CSecondDraft);
    Stage('entry pressure editor opened');
    LBefore := LStore.Snapshot;
    LStore.Update(NyxCollectionItem(LGateway).WithValue(NyxTextField('notes'), 'New value while capacity is full'));
    Stage('entry pressure source accepted');
    LCapacity := LBudget.CaptureBudget;
    Check((LStore.Snapshot.Revision = LBefore.Revision + 1) and
      (LPeer.View.Snapshot.Revision = LStore.Snapshot.Revision) and (LJournal.Capture.Count = 2),
      'a full recovery journal does not roll back committed source updates or freeze the peer');
    Check((LCapacity.LastRefusal = ncrpEntryCount) and not LCapacity.CanRetry and
      not LInput.InputReady and LGrid.EditorMode and (TCustomEdit(LGrid.Editor) = LEditor) and
      (TNyxText(LEditor.Text) = CSecondDraft) and (LEditor.SelStart = 2) and (LEditor.SelLength = 4),
      'capacity refusal keeps the actual native editor/text/caret while shielding its stale row map');
    Check(LApplication.State.GetValue(NyxBooleanState(RecoveryRetryVisibleID)) and
      (Pos('Recovery capacity reached', LApplication.State.GetValue(NyxTextState(RecoverySummaryID))) > 0),
      'ordinary recovery bindings expose useful pressure feedback and an explicit Retry display action');
    Stage('entry pressure assertions completed');
    Click(RecoveryRetryID);
    Stage('premature retry completed');
    Check(not LInput.InputReady and LGrid.EditorMode and (TNyxText(LEditor.Text) = CSecondDraft),
      'premature user retry keeps the same refused physical draft');
    Click(RecoveryDiscardID);
    Click(RecoveryDiscardID);
    Check((LJournal.Capture.Count = 1) and LBudget.CaptureBudget.CanRetry and not LInput.InputReady,
      'confirmed exact-entry discard frees capacity without pretending the held display already refreshed');
    LBefore := LStore.Snapshot;
    Click(RecoveryRetryID);
    Stage('entry capacity retry completed');
    Check(LInput.InputReady and (LJournal.Capture.Count = 2) and
      (LJournal.EntryAt(1).Cell.Text = CSecondDraft) and (LStore.Snapshot = LBefore) and
      (LBudget.CaptureBudget.LastRefusal = ncrpNone) and
      not LApplication.State.GetValue(NyxBooleanState(RecoveryRetryVisibleID)),
      'explicit Retry display recovers the held draft, releases the shield and preserves accepted source');
    LCapacity := LBudget.CaptureBudget;
    LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(3).PayloadBytes(LCapacity.PayloadBytes));
    BeginDraft(CThirdDraft);
    Stage('payload pressure editor opened');
    LStore.Update(NyxCollectionItem(LGateway).WithValue(NyxTextField('notes'), 'New value under byte pressure'));
    LCapacity := LBudget.CaptureBudget;
    Check((LCapacity.LastRefusal = ncrpPayloadBytes) and LGrid.EditorMode and
      (TNyxText(TCustomEdit(LGrid.Editor).Text) = CThirdDraft) and not LInput.InputReady,
      'payload pressure also preserves the actual native editor without evicting retained work');
    LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(3)
      .PayloadBytes(LCapacity.PayloadBytes + LCapacity.RequiredPayloadBytes));
    Check(LBudget.CaptureBudget.CanRetry, 'the caller may raise the exact payload limit without evicting or rewriting data');
    Click(RecoveryRetryID);
    Stage('payload capacity retry completed');
    Check(LInput.InputReady and (LJournal.Capture.Count = 3) and
      (LJournal.EntryAt(2).Cell.Text = CThirdDraft) and
      (LBudget.CaptureBudget.PayloadBytes = LBudget.CaptureBudget.Limits.MaximumPayloadBytes),
      'an exact-capacity retry admits every retained byte and resumes normal native display');
    Check(TNyxCodec.Encode(LDocument) = LSaved, 'runtime pressure and limit overrides remain outside design history');
    LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(3));
    BeginDraft(CFirstDraft);
    LBefore := LStore.Snapshot;
    LStore.Update(NyxCollectionItem(LGateway).WithValue(NyxTextField('notes'), 'Latest accepted after cancellation'));
    Check(NyxCollectionDraftRecoveryHeld(LFirst), 'a later full journal holds another incompatible dirty editor');
    LEditor.Text := LBefore.Item(LGateway).GetValue(NyxTextField('notes'));
    Click(RecoveryRetryID);
    Check(LInput.InputReady and not NyxCollectionDraftRecoveryHeld(LFirst) and
      (LJournal.Capture.Count = 3) and (LBudget.CaptureBudget.LastRefusal <> ncrpNone) and
      not LApplication.State.GetValue(NyxBooleanState(RecoveryRetryVisibleID)),
      'deliberately reverting a draft releases the display without misreading an old refusal as current pressure');
    BeginDraft(CFirstDraft);
    LStore.Update(NyxCollectionItem(LGateway).WithValue(NyxTextField('notes'), 'Accepted while draft is pending'));
    LBefore := LStore.Snapshot;
    Supports(LFirst, INyxCollectionPendingDraftMount, LPending);
    LReview := LPending.CapturePendingDraft;
    Check((LReview <> nil) and (LReview.Cell.Text = CFirstDraft) and
      LReview.Cell.Selection.Defined and (LReview.Cell.Selection.Start = 2) and
      (LJournal.Capture.Count = 3) and not LInput.InputReady,
      'bounded pending capture reads exact real editor text/caret without journal admission');
    Click(RecoveryReviewPendingID);
    Check(LApplication.State.GetValue(NyxBooleanState(RecoveryPendingReviewedID)) and
      (Pos(CFirstDraft, TNyxText(TMemo(LApplication.View.InputFor(RecoveryTextID)).Text)) > 0),
      'ordinary review controls show the still-pending draft independently of saved journal entries');
    Click(RecoveryNextID);
    Check(not LApplication.State.GetValue(NyxBooleanState(RecoveryPendingReviewedID)) and
      NyxCollectionDraftRecoveryHeld(LFirst),
      'saved-entry navigation leaves pending review without discarding the original');
    Click(RecoveryReviewPendingID);
    Click(RecoveryExportPendingID);
    Check((LSave.Calls = 1) and (LSave.StreamCalls = 1) and
      (NyxDecodeUTF8(LSave.Bytes) = CFirstDraft) and
      (LJournal.Capture.Count = 3) and LGrid.EditorMode and (TNyxText(LEditor.Text) = CFirstDraft),
      'inline file export uses exact UTF-8 and never consumes or reapplies the original pending draft');
    LSave.Outcome := nfsoCancelled;
    Click(RecoveryExportPendingID);
    Check((LSave.Calls = 2) and NyxCollectionDraftRecoveryHeld(LFirst) and
      (Pos('cancelled', LApplication.State.GetValue(NyxTextState(RecoverySummaryID))) > 0),
      'save cancellation is visible and keeps the original unfinished editor');
    Click(RecoveryDiscardPendingID);
    Check((TNyxText(LEditor.Text) = CFirstDraft) and not LInput.InputReady,
      'pending discard first warns without touching original text');
    LEditor.Text := CSecondDraft;
    Click(RecoveryDiscardPendingID);
    Check((TNyxText(LEditor.Text) = CSecondDraft) and NyxCollectionDraftRecoveryHeld(LFirst) and
      (Pos('draft changed', LApplication.State.GetValue(NyxTextState(RecoverySummaryID))) > 0),
      'a stale UI confirmation refuses newer typing and requires a new review/confirmation');
    Click(RecoveryDiscardPendingID);
    Click(RecoveryDiscardPendingID);
    Check(LInput.InputReady and not NyxCollectionDraftRecoveryHeld(LFirst) and
      (LJournal.Capture.Count = 3) and (LStore.Snapshot = LBefore),
      'confirmed pending discard resumes the latest display with zero journal capacity and no source write');
    Check(not LPending.DiscardPendingDraft(LReview) and
      (LReview.Cell.Text = CFirstDraft) and (TNyxCodec.Encode(LDocument) = LSaved),
      'an old review remains inert and cannot discard a later frame or alter saved design');
    LDemo.Close;
    LApplication.Free;
    LApplication := nil;
    Check(not LJournal.Capture.Active and (LJournal.Capture.Count = 3) and
      (LJournal.EntryAt(0).Cell.Text = CFirstDraft), 'retained recovery remains readable after its actual native application retires');
  finally
    Stage('journey finalization entered');
    LDemo.Free;
    LFirst := nil;
    LPeer := nil;
    LInput := nil;
    LApplication.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Application.Initialize;
    { Owned test failures propagate to the console guard rather than opening
      an unattended LCL exception dialog while a timeout waits on the process. }
    Application.CaptureExceptions := False;
    Journey(StrToInt(ParamStr(1)));
    WriteLn('PASS ', GChecks, ' native recovery control checks at ', ParamStr(1));
  except
    on E: Exception do
    begin
      WriteLn('FAIL after ', GChecks, ' checks: ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
