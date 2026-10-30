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
  nyx.collections.arrangement, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.mount, nyx.collections.recovery, nyx.collections.inspection,
  nyx.collections.query, nyx.collections.position, nyx.collections.recovery.policy,
  nyx.collections.recovery.controls, nyx.controls, nyx.types
  {$ifdef PAS2JS}, Web{$endif};

type
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

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AMessage);
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
  LFailed: Boolean;
begin
  LWriter := NewNyxCollectionDraftRecovery;
  LJournal := LWriter.Recovery;
  LCell := NyxCollectionCellEdit(NyxItem(NyxCollection('review'), 'gateway'),
    NyxColumnIdentity(NyxTextField('name')), TNyxStateValue.FromText(COriginal),
    CDraft, NyxTextSelection(CDraft, 1, 4), True, True);
  LEntry := LWriter.Preserve(LCell, ncdlHidden, 3, 4);
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

begin
  try
    Run;
    ReviewChecks;
    SavedPolicy;
    PressureChecks;
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
