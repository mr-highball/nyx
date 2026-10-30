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
program nyx_collection_recovery_mount_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.editing, nyx.state, nyx.collections,
  nyx.binding.types, nyx.clipboard,
  nyx.collections.arrangement, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.mount, nyx.collections.recovery, nyx.collections.inspection,
  nyx.collections.selection,
  nyx.collections.query, nyx.collections.position, nyx.collections.recovery.policy,
  nyx.collections.recovery.controls, nyx.collections.representation, nyx.controls, nyx.types,
  nyx.model, nyx.root.types, nyx.menu, nyx.popover, nyx.scheduler, nyx.events,
  nyx.collections.cell.actions, nyx.collections.workbench.runtime,
  nyx.collections.clipboard, nyx.collections.clipboard.actions,
  nyx.collections.export, nyx.collections.export.actions,
  nyx.clipboard.unavailable, nyx.files.save.unavailable
  {$ifdef PAS2JS}, Web{$endif};

type
  { One controlled target callout proves feedback admission/retirement ordering
    without pretending to exercise a native or browser presentation surface. }
  TFeedbackReentry = (frNone, frRetire, frThrow);
  { Model-only presenter proves factory ownership across default replacement.
    Physical menu/popover input belongs to the maintained control consumers. }
  TAdoptionFactory = class(TInterfacedObject, INyxCollectionCellPresenterFactory)
  public
    Revocations: Integer;
    function CreateMenu(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
      const AItems: TNyxMenuItems): INyxMenu;
    function CreateInspector(ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxPopover;
    procedure Revoke;
  end;

  { An alternative scheduler refuses admission from an actual RequireUI call.
    It retains only its base scheduler; it owns no mount, presenter or callbacks. }
  TAdoptionRefusal = class(TInterfacedObject, INyxScheduler)
  public
    Base: INyxScheduler;
    function GetCapabilities: TNyxSchedulerCapabilities;
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    function Submit(const AWork: INyxWork; APolicy: TNyxExecutionPolicy): INyxExecution;
    function PostUI(const AWork: INyxWork;
      const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
  end;

  { Model-only target exercises the actual mount notification, recovery and
    input-lifetime machinery, including capacity holds and follow acknowledgement after success.
    Its copied frame/drafts/position are values, not widgets. Multiple dirty
    cells survive a partial capture until the complete target can adopt data.
    Actual native/browser controls remain separate qualification. }
  TRecoveryTarget = class(TNyxCollectionMountBase)
  public
    Frame: INyxCollectionSnapshot;
    Draft: TNyxCollectionCellEdit;
    ExtraDraft: TNyxCollectionCellEdit;
    Fail: Boolean;
    Loss: TNyxCollectionDraftLoss;
    Position: TNyxCollectionPositionRecovery;
    FeedbackReentry: TFeedbackReentry;
    procedure ClipboardFeedbackChanged; override;
    function ReadRecoveryDraft(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity; out ACell: TNyxCollectionCellEdit): Boolean; override;
    function DiscardRecoveryDraft(const ACell: TNyxCollectionCellEdit): Boolean; override;
    procedure RenderDataset; override;
    procedure DetachTarget; override;
  end;

  { An alternative receipt counts getter calls. Retired command owners must
    refuse before consulting caller-supplied values or invoking their code. }
  TReceiptProbe = class(TInterfacedObject, INyxPendingCollectionDraft)
  public
    Reads: Integer;
    function GetCell: TNyxCollectionCellEdit;
    function GetFrame: TNyxDraftRecoveryFrame;
  end;

var
  GChecks: Integer;

function TAdoptionFactory.CreateMenu(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; const AItems: TNyxMenuItems): INyxMenu;
begin
  Result := nil;
  raise Exception.Create('Model adoption must not present physical menus');
end;

function TAdoptionFactory.CreateInspector(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef): INyxPopover;
begin
  Result := nil;
  raise Exception.Create('Model adoption must not present physical popovers');
end;

procedure TAdoptionFactory.Revoke;
begin
  Inc(Revocations);
end;

function TAdoptionRefusal.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := Base.Capabilities;
end;

procedure TAdoptionRefusal.RequireUI;
begin
  raise ENyxSchedule.Create('Deliberate presenter admission refusal');
end;

procedure TAdoptionRefusal.Admit(APolicy: TNyxExecutionPolicy);
begin
  Base.Admit(APolicy);
end;

function TAdoptionRefusal.Submit(const AWork: INyxWork;
  APolicy: TNyxExecutionPolicy): INyxExecution;
begin
  Result := Base.Submit(AWork, APolicy);
end;

function TAdoptionRefusal.PostUI(const AWork: INyxWork;
  const AParent: INyxExecution): INyxExecution;
begin
  Result := Base.PostUI(AWork, AParent);
end;

procedure TAdoptionRefusal.Shutdown;
begin
  Base.Shutdown;
end;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
end;

procedure TRecoveryTarget.ClipboardFeedbackChanged;
var
  LAction: TFeedbackReentry;
begin
  LAction := FeedbackReentry;
  FeedbackReentry := frNone;
  case LAction of
    frRetire:
      begin
        Disconnect;
      end;
    frThrow:
      begin
        raise Exception.Create('Deliberate feedback paint failure');
      end;
    frNone:
      begin
        { This model target claims no physical feedback surface. }
      end;
  end;
end;

function TRecoveryTarget.ReadRecoveryDraft(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity; out ACell: TNyxCollectionCellEdit): Boolean;
begin
  ACell := Draft.Copy;
  Result := Draft.Defined and (Draft.Item.ID = AItem.ID) and Draft.Field.Same(AField);
end;

function TRecoveryTarget.DiscardRecoveryDraft(const ACell: TNyxCollectionCellEdit): Boolean;
begin
  Result := NyxSameCollectionDraft(Draft, ACell);

  if Result then
  begin
    Draft := Default(TNyxCollectionCellEdit);
  end;
end;

function TReceiptProbe.GetCell: TNyxCollectionCellEdit;
begin
  Inc(Reads);
  Result := Default(TNyxCollectionCellEdit);
end;

function TReceiptProbe.GetFrame: TNyxDraftRecoveryFrame;
begin
  Inc(Reads);
  Result := Default(TNyxDraftRecoveryFrame);
end;

procedure TRecoveryTarget.RenderDataset;
var
  LNext: INyxCollectionSnapshot;
  LRecover: Boolean;
  LRecoverExtra: Boolean;
begin
  LNext := FView.Snapshot;
  Position := ResolvePosition(Frame, LNext, 0);

  if Fail then
  begin
    raise ENyxCollection.Create('Expected physical refresh failure');
  end;

  LRecover := Draft.Defined and RecoverCell(Draft, Frame, LNext, Loss);
  LRecoverExtra := ExtraDraft.Defined and RecoverCell(ExtraDraft, Frame, LNext, Loss);

  if LRecover then
  begin
    Draft := Default(TNyxCollectionCellEdit);
  end;

  if LRecoverExtra then
  begin
    ExtraDraft := Default(TNyxCollectionCellEdit);
  end;
  Frame := LNext;
end;

procedure TRecoveryTarget.DetachTarget;
begin
  Frame := nil;
  Draft := Default(TNyxCollectionCellEdit);
  ExtraDraft := Default(TNyxCollectionCellEdit);
end;

{ Pressure is an intentional held target, not a failed source transaction.
  The same portable base must keep dirty text and block stale input until retry. }
procedure PressureChecks;
const
  CDraft: TNyxText = 'Hold 🌙' + #0 + #13#10 + 'this draft';
var
  LKey: TNyxCollectionRef;
  LItem: TNyxItemRef;
  LSecond: TNyxItemRef;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LPeer: INyxCollectionView;
  LMount: INyxCollectionMount;
  LInput: INyxCollectionInputAdmission;
  LTarget: TRecoveryTarget;
  LJournal: INyxCollectionDraftRecovery;
  LBudget: INyxCollectionDraftRecoveryBudget;
  LFrame: INyxCollectionSnapshot;
  LAccepted: INyxCollectionSnapshot;
  LBytes: Integer;
  LSaturated: Boolean;
  LFirstCapture: INyxRecoveredCollectionDraft;
  LPending: INyxCollectionPendingDraftMount;
  LReview: INyxPendingCollectionDraft;
  LProbe: TReceiptProbe;
  LProbeLease: INyxPendingCollectionDraft;
begin
  LKey := NyxCollection('pressure');
  LItem := NyxItem(LKey, 'reader');
  LSecond := NyxItem(LKey, 'peer');
  LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField('name'), ''), [
    NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Original'),
    NyxCollectionItem(LSecond).WithValue(NyxTextField('name'), 'Peer')]);
  LView := NewNyxCollectionView(LStore, NyxCollectionView(LKey)
    .Column(NyxTextField('name'), 'Name', cmEditable), cpTable);
  LPeer := NewNyxCollectionView(LStore, LView.Spec, cpTable);
  LTarget := TRecoveryTarget.Create(LView);
  LMount := LTarget;
  LTarget.Activate;
  Supports(LMount, INyxCollectionInputAdmission, LInput);
  LJournal := NyxCollectionDraftRecovery(LMount);
  LBudget := NyxCollectionDraftRecoveryBudget(LJournal);
  LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(0).PayloadBytes(0));
  LTarget.Draft := NyxCollectionCellEdit(LItem, NyxColumnIdentity(NyxTextField('name')),
    TNyxStateValue.FromText('Original'), CDraft, NyxTextSelection(CDraft, 1, 4), True, True);
  LBytes := NyxDraftRecoveryPayloadBytes(LTarget.Draft, LSaturated);
  LFrame := LTarget.Frame;
  LStore.Update(NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Incoming'));
  LAccepted := LStore.Snapshot;
  Check((LPeer.Snapshot.Revision = LAccepted.Revision) and (LTarget.Frame = LFrame) and
    LTarget.Draft.Defined and (LTarget.Draft.Text = CDraft) and not LInput.InputReady and
    NyxCollectionDraftRecoveryHeld(LMount),
    'zero-capacity refusal holds the draft/display and shields stale input while source/peer advance');
  Check((LJournal.Capture.Count = 0) and (LBudget.CaptureBudget.PayloadBytes = 0) and
    (LBudget.CaptureBudget.LastRefusal = ncrpEntryCount),
    'a held zero-capacity display cannot leak its refused value into journal storage');
  LMount.Refresh;
  Check((LTarget.Frame = LFrame) and LTarget.Draft.Defined and not LInput.InputReady,
    'repeated premature retry preserves the original held frame and exact text');
  Supports(LMount, INyxCollectionPendingDraftMount, LPending);
  LReview := LPending.CapturePendingDraft;
  Check((LReview <> nil) and (LReview.Cell.Text = CDraft) and
    (LJournal.Capture.Count = 0) and not LInput.InputReady,
    'explicit pending review retains exact supplementary/NUL text without journal admission or refresh');
  LTarget.Draft := NyxCollectionCellEdit(LItem, LReview.Cell.Field,
    LReview.Cell.Accepted, CDraft + 'newer', NyxTextSelection(CDraft + 'newer', 1, 4), True, True);
  Check(not LPending.DiscardPendingDraft(LReview) and LTarget.Draft.Defined,
    'a pending confirmation cannot discard newer text in the same held physical frame');
  LTarget.Draft := LReview.Cell;
  LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(1).PayloadBytes(LBytes));
  Check(not LSaturated and LBudget.CaptureBudget.CanRetry and not LInput.InputReady,
    'raising exact capacity does not silently claim that old target input is ready');
  LMount.Refresh;
  Check((LStore.Snapshot = LAccepted) and (LTarget.Frame = LView.Snapshot) and
    not LTarget.Draft.Defined and LInput.InputReady and
    (LJournal.EntryAt(0).Cell.Text = CDraft) and
    (LBudget.CaptureBudget.LastRefusal = ncrpNone) and not NyxCollectionDraftRecoveryHeld(LMount),
    'one successful explicit retry preserves the draft before adopting current display and releasing input');
  LJournal.Discard(LJournal.EntryAt(0));
  LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(1));
  LTarget.Draft := NyxCollectionCellEdit(LItem, NyxColumnIdentity(NyxTextField('name')),
    TNyxStateValue.FromText('Incoming'), CDraft, Default(TNyxTextSelection), False, False);
  LTarget.ExtraDraft := NyxCollectionCellEdit(LSecond, NyxColumnIdentity(NyxTextField('name')),
    TNyxStateValue.FromText('Peer'), CDraft + ' second', Default(TNyxTextSelection), False, False);
  LStore.Apply([
    NyxUpdate(NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Changed first')),
    NyxUpdate(NyxCollectionItem(LSecond).WithValue(NyxTextField('name'), 'Changed second'))]);
  LFirstCapture := LJournal.EntryAt(0);
  Check(NyxCollectionDraftRecoveryHeld(LMount) and LTarget.Draft.Defined and
    LTarget.ExtraDraft.Defined and (LJournal.Capture.Count = 1),
    'partial admission keeps every original physical draft while one receipt is already stored');
  LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(2));
  LMount.Refresh;
  Check(LInput.InputReady and not LTarget.Draft.Defined and not LTarget.ExtraDraft.Defined and
    (LJournal.Capture.Count = 2) and (LJournal.EntryAt(0) = LFirstCapture) and
    (LFirstCapture.Reason = ncdlValueChanged),
    'retry coalesces an already stored physical draft even when later provenance is only unknown');
  LMount.Disconnect;
  Check((LPending.CapturePendingDraft = nil) and not LPending.DiscardPendingDraft(LReview) and
    (LReview.Cell.Text = CDraft), 'pending receipts stay readable but grant no retired mount authority');
  LProbe := TReceiptProbe.Create;
  LProbeLease := LProbe;
  Check(not LPending.DiscardPendingDraft(LProbeLease) and (LProbe.Reads = 0),
    'a retired command refuses before invoking alternative receipt getters');
end;

procedure Run;
const
  CDraft: TNyxText = 'Keep 🌙' + #0 + #13#10 + 'this unfinished text';
var
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LMount: INyxCollectionMount;
  LInput: INyxCollectionInputAdmission;
  LJournal: INyxCollectionDraftRecovery;
  LInspection: INyxCollectionInspection;
  LFollowing: INyxCollectionFollowing;
  LTarget: TRecoveryTarget;
  LKey: TNyxCollectionRef;
  LItem: TNyxItemRef;
  LPeer: TNyxItemRef;
  LBefore: INyxCollectionSnapshot;
  LRefused: Boolean;
  LCount: Integer;

  procedure SetDraft;
  begin
    LTarget.Draft := NyxCollectionCellEdit(LItem, NyxColumnIdentity(NyxTextField('name')),
      LView.Spec.ColumnAt(0).Read(LView.Snapshot.Item(LItem)), CDraft,
      NyxTextSelection(CDraft, 1, 5), False, False);
  end;

begin
  LKey := NyxCollection('recovery');
  LItem := NyxItem(LKey, 'inspected');
  LPeer := NyxItem(LKey, 'peer');
  LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField('name'), ''), [
    NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Original'),
    NyxCollectionItem(LPeer).WithValue(NyxTextField('name'), 'Peer')]);
  LView := NewNyxCollectionView(LStore, NyxCollectionView(LKey)
    .Column(NyxTextField('name'), 'Name', cmEditable), cpTable);
  LTarget := TRecoveryTarget.Create(LView);
  LMount := LTarget;
  LTarget.Activate;
  LJournal := NyxCollectionDraftRecovery(LMount);
  Supports(LMount, INyxCollectionInputAdmission, LInput);
  LInspection := NyxCollectionInspection(LView);
  LInspection.ConfigureInspection(NyxInspectionPolicy);
  SetDraft;
  LStore.Update(NyxCollectionItem(LPeer).WithValue(NyxTextField('name'), 'Updated peer'));
  Check(LTarget.Draft.Defined and (LJournal.Capture.Count = 0),
    'an unrelated source edit preserves the active dirty value without a recovery record');
  LStore.Update(NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Producer value'));
  Check(not LTarget.Draft.Defined and (LTarget.Loss = ncdlValueChanged) and
    (LJournal.Capture.Count = 1) and (LJournal.EntryAt(0).Cell.Text = CDraft),
    'changed accepted data retires old automatic editing only after exact text is preserved');
  Check(LView.CellText(LItem, 0) = 'Producer value', 'recovery never overwrites the producer value');
  SetDraft;
  LStore.Apply([NyxRemove(LItem), NyxInsert(0,
    NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Producer value'))]);
  Check((LTarget.Loss = ncdlIdentityReplaced) and (LJournal.Capture.Count = 2),
    'same-value remove/reinsert still cannot inherit an old physical draft; reason=' +
    TNyxText(IntToStr(Ord(LTarget.Loss))) + ', count=' +
    TNyxText(IntToStr(LJournal.Capture.Count)) + ', revision=' +
    TNyxText(IntToStr(LStore.Snapshot.Revision)));
  SetDraft;
  LView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxTextField('name')).EqualTo('Updated peer')));
  Check((LTarget.Loss = ncdlHidden) and (LJournal.Capture.Count = 3),
    'filtering an edited row out preserves its draft as hidden, not removed');
  LView.ConfigureQuery(Default(TNyxCollectionQuery));
  Check(not LTarget.Draft.Defined and (LJournal.Capture.Count = 3),
    'relaxing a filter does not reapply old recovered text implicitly');
  SetDraft;
  LInspection.PauseInspection;
  LBefore := LTarget.Frame;
  LStore.Remove(LItem);
  Check(LTarget.Draft.Defined and (LTarget.Frame = LBefore) and (LJournal.Capture.Count = 3),
    'paused source removal leaves the physical draft and frame intact');
  LInspection.ResumeInspection;
  Check((LTarget.Loss = ncdlRowRemoved) and not LTarget.Draft.Defined and
    (LJournal.Capture.Count = 4), 'resume preserves the deleted edited row before withdrawing its old draft');
  LCount := LJournal.Capture.Count;
  LTarget.Fail := True;
  LRefused := False;
  try
    LStore.Update(NyxCollectionItem(LPeer).WithValue(NyxTextField('name'), 'After failure'));
  except
    on E: ENyxCollectionNotification do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LView.CellText(LPeer, 0) = 'After failure') and
    not LInput.InputReady and (LJournal.Capture.Count = LCount),
    'a failed physical refresh leaves committed data but blocks old-map input');
  LTarget.Fail := False;
  LMount.Refresh;
  Check(LInput.InputReady and (LTarget.Frame = LView.Snapshot),
    'successful retry releases the same input shield only after current data is installed');
  LFollowing := NyxCollectionFollowing(LView);
  LBefore := LStore.Snapshot;
  LTarget.Fail := True;
  LRefused := False;
  try
    LFollowing.FollowLatest;
  except
    on LException: ENyxCollectionNotification do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LInput.InputReady and (LStore.Snapshot = LBefore) and
    (LTarget.Position.Kind = ncpLatest),
    'a failed follow render blocks input without consuming a source revision');
  LTarget.Fail := False;
  LMount.Refresh;
  Check(LInput.InputReady and (LTarget.Position.Kind = ncpLatest),
    'an explicit follow request survives render failure and is proposed again on retry');
  LMount.Refresh;
  Check(LTarget.Position.Kind = ncpUnavailable,
    'successful physical installation acknowledges the request instead of replaying it on every refresh');
  LMount.Disconnect;
  LInput := nil;
  LMount := nil;
  LFollowing := nil;
  Check(not LJournal.Capture.Active and (LJournal.Capture.Count = 4) and
    (LJournal.EntryAt(0).Cell.Text = CDraft),
    'a retained journal survives physical target disposal without retaining its authority');
end;

procedure SavedPolicy;
var
  LKey: TNyxCollectionRef;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LMount: INyxCollectionMount;
  LTarget: TRecoveryTarget;
  LJournal: INyxCollectionDraftRecovery;
  LBudget: INyxCollectionDraftRecoveryBudget;
  LSaved: TNyxText;
begin
  LKey := NyxCollection('saved-recovery');
  LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField('name'), ''), [
    NyxCollectionItem(NyxItem(LKey, 'reader')).WithValue(NyxTextField('name'), 'Original')]);
  LView := NewNyxCollectionView(LStore, NyxCollectionView(LKey)
    .Column(NyxTextField('name'), 'Name', cmEditable)
    .DraftRecovery(NyxDraftRecoveryPolicy.Entries(0).PayloadBytes(0)
      .WarnEntries(0).WarnTextBytes(0)), cpTable);
  LSaved := LView.Spec.ToData.ToJSON;
  LTarget := TRecoveryTarget.Create(LView);
  LMount := LTarget;
  LTarget.Activate;
  LJournal := NyxCollectionDraftRecovery(LMount);
  LBudget := NyxCollectionDraftRecoveryBudget(LJournal);
  Check((LBudget.CaptureBudget.Limits.MaximumEntries = 0) and
    (LBudget.CaptureBudget.Limits.MaximumPayloadBytes = 0),
    'a fresh mount consumes independent saved recovery limits before its first capture');
  LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(2).PayloadBytes(8192));
  LStore.Update(NyxCollectionItem(NyxItem(LKey, 'reader')).WithValue(NyxTextField('name'), 'Updated'));
  LMount.Refresh;
  Check((LBudget.CaptureBudget.Limits.MaximumEntries = 2) and
    (LView.Spec.ToData.ToJSON = LSaved), 'source/explicit refresh preserves caller runtime overrides and saved defaults');
  LMount.Disconnect;
  LTarget := TRecoveryTarget.Create(LView);
  LMount := LTarget;
  LTarget.Activate;
  LBudget := NyxCollectionDraftRecoveryBudget(NyxCollectionDraftRecovery(LMount));
  Check(LBudget.CaptureBudget.Limits.MaximumEntries = 0,
    'a replacement mount initializes its own journal from saved defaults, not the former runtime override');
  LMount.Disconnect;
end;

{ A detached review must outlive the original entry/journal without borrowing
  editing authority. This exercises the same entry produced by real recovery,
  including preview-safe control characters and exact supplementary text/caret. }
procedure ReviewChecks;
const
  CDraft: TNyxText = 'Hold 🌙' + #0 + #13#10 + 'this draft';
  CPreview: TNyxText = 'Hold 🌙\0\r' + #10 + 'this draft';
  COriginal: TNyxText = 'Original 🌙';
var
  LWriter: INyxCollectionDraftRecoveryWriter;
  LJournal: INyxCollectionDraftRecovery;
  LEntry: INyxRecoveredCollectionDraft;
  LReview: INyxRecoveredDraftReview;
  LCell: TNyxCollectionCellEdit;
  LMemo: INyxMemo;
  LText: INyxCollectionCellText;
  LNormalized: INyxCollectionCellText;
  LFailed: Boolean;
begin
  LWriter := NewNyxCollectionDraftRecovery;
  LJournal := LWriter.Recovery;
  LCell := NyxCollectionCellEdit(NyxItem(NyxCollection('review'), 'gateway'),
    NyxColumnIdentity(NyxTextField('name')), TNyxStateValue.FromText(COriginal),
    CDraft, NyxTextSelection(CDraft, 1, 4), True, True);
  LEntry := LWriter.Preserve(LCell, ncdlHidden, 3, 4);
  LText := NewNyxCollectionRecoveredDraftText(LEntry);
  LNormalized := CopyNyxCollectionCellText(LText);
  Check((LNormalized.Representation = nccrExplicitDraft) and
    (LNormalized.Text = CDraft) and (LNormalized.RawText = COriginal) and
    (LNormalized.ByteCount = 22) and (LNormalized.Revision = 3) and
    LNormalized.Value.SameValue(LCell.Accepted) and
    LNormalized.Field.Same(LCell.Field) and (LNormalized.Item.ID = 'gateway') and
    not LNormalized.HasFormatter and not LNormalized.Domain.Defined,
    'recovered copy is an explicit exact draft with accepted provenance, not RAW or focused editor authority');
  LFailed := False;
  try
    NewNyxCollectionRecoveredDraftText(LEntry, 21);
  except
    on ENyxCollection do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'complete recovered-text bytes must fit before represented host admission');
  LReview := NewNyxRecoveredDraftReview('recovered-edit', LEntry);
  Check((LJournal.Capture.Count = 1) and (LJournal.EntryAt(0) = LEntry),
    'constructing a read-only review neither discards nor replaces its journal entry');
  Check((LReview.Cell.Text = CDraft) and
    LReview.Cell.Selection.SameRange(LCell.Selection) and
    LReview.Cell.Accepted.SameValue(LCell.Accepted) and
    LReview.Cell.Field.Same(LCell.Field) and (LReview.Cell.Item.ID = 'gateway') and
    (LReview.Reason = ncdlHidden) and (LReview.BeforeRevision = 3) and
    (LReview.AfterRevision = 4) and not LReview.Cell.Editing and not LReview.Cell.Focused,
    'review preserves exact typed provenance and scalar caret without live editor flags');
  Check(Supports(LReview.Children[4], INyxMemo, LMemo) and
    (LReview.Node.Part(NyxPart(NyxRecoveredDraftTextPart)) = LMemo.Node) and
    LMemo.ReadOnly and (LMemo.Value = CPreview),
    'unfinished preview is a real read-only Memo with explicit control escaping');
  Check(Supports(LReview.Children[5], INyxMemo, LMemo) and
    (LReview.Node.Part(NyxPart(NyxRecoveredDraftAcceptedPart)) = LMemo.Node) and
    LMemo.ReadOnly and (LMemo.Value = COriginal),
    'accepted value has a separate read-only named control');
  LJournal.Discard(LEntry);
  LWriter.Retire;
  LEntry := nil;
  LWriter := nil;
  LJournal := nil;
  LText := NewNyxCollectionRecoveredDraftText(LReview.Cell, LReview.BeforeRevision);
  Check((LText.Text = CDraft) and (LText.RawText = COriginal) and
    (LText.Revision = 3),
    'lazy represented Copy uses the frozen review after the original journal and entry retire');
  Check((LNormalized.Text = CDraft) and (LNormalized.RawText = COriginal),
    'normalized recovered transfer owns its original bytes after journal retirement');
  Check((LReview.Cell.Text = CDraft) and
    (LReview.Cell.Accepted.TextValue = COriginal) and
    (LReview.Reason = ncdlHidden),
    'detached review remains exact after explicit discard and journal retirement');
  LFailed := False;
  try
    NewNyxRecoveredDraftReview('missing', nil);
  except
    on ENyxCollection do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'absent recovery entry refuses instead of displaying invented data');
end;

procedure ActionAdoption;
const
  CInertDraft: TNyxText = 'An inert draft 🌙';
  CLiveDraft: TNyxText = 'Still editing 🌙';
var
  LKey: TNyxCollectionRef;
  LItem: TNyxItemRef;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LTarget: TRecoveryTarget;
  LMount: INyxCollectionMount;
  LJournal: INyxCollectionDraftRecovery;
  LEntry: INyxRecoveredCollectionDraft;
  LDefaultFactory: TAdoptionFactory;
  LDefaultLease: INyxCollectionCellPresenterFactory;
  LExplicitFactory: TAdoptionFactory;
  LExplicitLease: INyxCollectionCellPresenterFactory;
  LScheduler: INyxScheduler;
  LRefusal: TAdoptionRefusal;
  LRefusalLease: INyxScheduler;
  LEvents: INyxEvents;
  LCells: INyxCollectionCellActionsMount;
  LClipboard: INyxCollectionClipboardMount;
  LExport: INyxCollectionExportMount;
  LRecovery: INyxCollectionCellRecoveryActions;
  LExportActions: INyxCollectionCellExportActions;
  LOld: INyxCollectionCellActions;
  LIntent: INyxCollectionCellActionIntent;
  LSource: INyxCollectionSnapshot;
  LSelection: TNyxCollectionSelectionSnapshot;
  LFailed: Boolean;
begin
  LKey := NyxCollection('adoption');
  LItem := NyxItem(LKey, 'reader');
  LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField('name'), ''), [
    NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Original')]);
  LView := NewNyxCollectionView(LStore, NyxCollectionView(LKey)
    .Column(NyxTextField('name'), 'Name', cmEditable), cpTable);
  LTarget := TRecoveryTarget.Create(LView);
  LMount := LTarget;
  LTarget.Activate;
  LJournal := NyxCollectionDraftRecovery(LMount);
  LTarget.Draft := NyxCollectionCellEdit(LItem, NyxColumnIdentity(NyxTextField('name')),
    TNyxStateValue.FromText('Original'), CInertDraft,
    NyxTextSelection(CInertDraft, 1, 4), True, True);
  LStore.Update(NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Incoming'));
  LEntry := LJournal.EntryAt(0);
  LSource := LStore.Snapshot;
  LSelection := LView.Selection.Snapshot;
  LTarget.Draft := NyxCollectionCellEdit(LItem, LEntry.Cell.Field,
    TNyxStateValue.FromText('Incoming'), CLiveDraft,
    NyxTextSelection(CLiveDraft, 2, 5), True, True);
  LScheduler := NewNyxScheduler;
  LEvents := NewNyxEvents(LScheduler);
  LDefaultFactory := TAdoptionFactory.Create;
  LDefaultLease := LDefaultFactory;
  LExplicitFactory := TAdoptionFactory.Create;
  LExplicitLease := LExplicitFactory;
  LRefusal := TAdoptionRefusal.Create;
  LRefusal.Base := LScheduler;
  LRefusalLease := LRefusal;
  ConfigureNyxTableRecoveryReview(LMount, LDefaultLease, LEvents);
  Check(Supports(LMount, INyxCollectionCellActionsMount, LCells) and
    Supports(LMount, INyxCollectionClipboardMount, LClipboard) and
    Supports(LMount, INyxCollectionExportMount, LExport),
    'the ordinary mount exposes unchanged explicit contracts beside automatic recovery');
  LOld := LCells.CellActions;
  Check((LOld <> nil) and (LClipboard.ClipboardCommands = nil) and
    (LExport.ExportCommands = nil) and
    Supports(LOld, INyxCollectionCellRecoveryActions, LRecovery) and
    (LRecovery.RecoveryCount = 1),
    'automatic review retains the original journal without granting transfer services');
  LFailed := False;
  try
    LCells.ConfigureCellActions(LExplicitLease, LRefusalLease);
  except
    on ENyxSchedule do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LCells.CellActions = LOld) and
    (LDefaultFactory.Revocations = 0) and (LExplicitFactory.Revocations = 0) and
    (NyxCollectionDraftRecovery(LMount) = LJournal) and (LJournal.EntryAt(0) = LEntry),
    'outward scheduler admission failure preserves the useful default and both factory owners');
  LFailed := False;
  try
    LClipboard.ConfigureClipboard(NewNyxUnavailableClipboardWriteHost, LEvents,
      LRefusalLease, TNyxCollectionClipboardOptions.Defaults);
  except
    on ENyxSchedule do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LCells.CellActions = LOld) and
    (LClipboard.ClipboardCommands = nil) and (LDefaultFactory.Revocations = 0),
    'failed clipboard authority admission leaves the old readonly default installed');
  LClipboard.ConfigureClipboard(NewNyxUnavailableClipboardWriteHost, LEvents,
    LScheduler, TNyxCollectionClipboardOptions.Defaults);
  Check((LCells.CellActions <> LOld) and (LDefaultFactory.Revocations = 0) and
    Supports(LOld, INyxCollectionCellActionIntent, LIntent) and
    (LIntent.Intent = 0),
    'first explicit clipboard service retires old action authority without revoking the default anchor');
  LOld := LCells.CellActions;
  LFailed := False;
  try
    LExport.ConfigureExport(NewNyxUnavailableFileSaveHost, LEvents, LRefusalLease,
      TNyxCollectionExportOptions.Defaults);
  except
    on ENyxSchedule do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LCells.CellActions = LOld) and
    (LExport.ExportCommands = nil) and (LDefaultFactory.Revocations = 0),
    'failed export authority admission preserves the existing clipboard/default incarnation');
  LExport.ConfigureExport(NewNyxUnavailableFileSaveHost, LEvents, LScheduler,
    TNyxCollectionExportOptions.Defaults);
  Check((LCells.CellActions <> LOld) and (LDefaultFactory.Revocations = 0) and
    Supports(LCells.CellActions, INyxCollectionCellExportActions, LExportActions) and
    LExportActions.ExportEnabled and
    Supports(LCells.CellActions, INyxCollectionCellRecoveryActions, LRecovery) and
    (LRecovery.RecoveryCount = 1),
    'export refreshes the managed default against the same explicit controller and journal');
  LOld := LCells.CellActions;
  LFailed := False;
  try
    LExport.ConfigureExport(NewNyxUnavailableFileSaveHost, LEvents, LScheduler,
      TNyxCollectionExportOptions.Defaults);
  except
    on ENyxCollectionExport do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LCells.CellActions = LOld) and (LDefaultFactory.Revocations = 0),
    'duplicate explicit export refuses without replacing a valid default');
  LCells.ConfigureCellActions(LExplicitLease, LScheduler);
  Check((LCells.CellActions <> LOld) and (LDefaultFactory.Revocations = 1) and
    (LExplicitFactory.Revocations = 0) and
    Supports(LOld, INyxCollectionCellActionIntent, LIntent) and
    (LIntent.Intent = 0),
    'the first explicit presenter takes precedence and revokes only the displaced default factory');
  LOld := LCells.CellActions;
  LFailed := False;
  try
    LCells.ConfigureCellActions(LExplicitLease, LScheduler);
  except
    on ENyxCollectionClipboard do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LCells.CellActions = LOld) and (LExplicitFactory.Revocations = 0),
    'automatic adoption does not weaken the original duplicate explicit presenter refusal');
  Check((LStore.Snapshot = LSource) and LView.Selection.Snapshot.SameState(LSelection) and
    (LJournal.EntryAt(0) = LEntry) and (LTarget.Draft.Text = CLiveDraft) and
    LTarget.Draft.Selection.SameRange(NyxTextSelection(CLiveDraft, 2, 5)),
    'default/service/presenter adoption never refreshes accepted rows, selection or exact live draft/caret');
  LMount.Disconnect;
  Check((LExplicitFactory.Revocations = 1) and (LDefaultFactory.Revocations = 1) and
    (LEntry.Cell.Text = CInertDraft),
    'final retirement revokes each real factory once while detached journal entries remain readable');
  LScheduler.Shutdown;
end;

procedure FeedbackAdmission;
var
  LMode: TFeedbackReentry;
  LKey: TNyxCollectionRef;
  LItem: TNyxItemRef;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LTarget: TRecoveryTarget;
  LMount: INyxCollectionMount;
  LFactory: TAdoptionFactory;
  LFactoryLease: INyxCollectionCellPresenterFactory;
  LScheduler: INyxScheduler;
  LEvents: INyxEvents;
  LClipboard: INyxCollectionClipboardMount;
  LCells: INyxCollectionCellActionsMount;
  LFeedback: INyxCollectionClipboardFeedbackMount;
  LCommand: INyxCollectionClipboardCommand;
  LSource: INyxCollectionSnapshot;
begin
  for LMode := frRetire to frThrow do
  begin
    LKey := NyxCollection('feedback-admission');
    LItem := NyxItem(LKey, 'reader');
    LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField('name'), ''), [
      NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Original')]);
    LView := NewNyxCollectionView(LStore, NyxCollectionView(LKey)
      .Column(NyxTextField('name'), 'Name', cmEditable), cpTable);
    LTarget := TRecoveryTarget.Create(LView);
    LMount := LTarget;
    LTarget.Activate;
    LSource := LStore.Snapshot;
    LFactory := TAdoptionFactory.Create;
    LFactoryLease := LFactory;
    LScheduler := NewNyxScheduler;
    LEvents := NewNyxEvents(LScheduler);
    ConfigureNyxTableRecoveryReview(LMount, LFactoryLease, LEvents);
    Supports(LMount, INyxCollectionClipboardMount, LClipboard);
    Supports(LMount, INyxCollectionCellActionsMount, LCells);
    Supports(LMount, INyxCollectionClipboardFeedbackMount, LFeedback);
    LTarget.FeedbackReentry := LMode;
    LClipboard.ConfigureClipboard(NewNyxUnavailableClipboardWriteHost, LEvents,
      LScheduler, TNyxCollectionClipboardOptions.Defaults);

    if LMode = frRetire then
    begin
      Check(not LMount.Connected and (LClipboard.ClipboardCommands = nil) and
        (LCells.CellActions = nil) and (LFactory.Revocations = 1),
        'feedback reentry cannot reinstall a candidate presenter after original mount retirement');
    end
    else
    begin
      Check(LMount.Connected and (LClipboard.ClipboardCommands <> nil) and
        (LCells.CellActions <> nil) and (LMount.Failure = nbfNotificationFailed) and
        (LMount.ErrorText = 'Deliberate feedback paint failure'),
        'post-admission presentation failure is diagnostic and preserves the admitted authority');
      LCommand := LClipboard.ClipboardCommands.BeginCell(LItem,
        NyxColumnIdentity(NyxTextField('name')));
      LCommand.Prepare;
      LCommand.Complete(True);
      Check((LFeedback.ClipboardFeedback.Phase = nccpCompleted) and
        (LFeedback.ClipboardFeedback.Result.Outcome = ncwoUnavailable),
        'a presentation failure cannot prevent the original typed host result');
      LCommand := nil;
      LMount.Disconnect;
    end;
    Check((LStore.Snapshot = LSource) and (LFactory.Revocations = 1),
      'feedback target callouts preserve source ownership and retire the exact factory once');
    LScheduler.Shutdown;
    LClipboard := nil;
    LCells := nil;
    LFeedback := nil;
    LMount := nil;
    LFactoryLease := nil;
    LEvents := nil;
    LScheduler := nil;
    LView := nil;
    LStore := nil;
  end;
end;

begin
  try
    Run;
    ReviewChecks;
    SavedPolicy;
    PressureChecks;
    ActionAdoption;
    FeedbackAdmission;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-recovery-mount-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' model-mounted recovery checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-recovery-mount-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
