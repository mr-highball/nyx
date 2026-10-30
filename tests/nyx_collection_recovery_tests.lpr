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
program nyx_collection_recovery_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.state, nyx.editing, nyx.bytes, nyx.collections,
  nyx.collections.arrangement, nyx.collections.edits, nyx.collections.recovery
  {$ifdef PAS2JS}, Web, JS{$endif};

const
  CExact: TNyxText = 'A moon 🌙' + #0 + #13#10 + 'unfinished tail';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

type
  { Borrowed notification receiver exercises committed membership, mutation
    refusal and sibling delivery. Its journal reference owns data only; tokens
    are disconnected before this receiver is freed. Browser raw failure is a
    deliberate foreign throw, not a platform/UI dependency. }
  TRecoveryProbe = class
  public
    Calls: Integer;
    Last: TNyxDraftRecoveryObservation;
    Journal: INyxCollectionDraftRecovery;
    TryMutation: Boolean;
    MutationRefused: Boolean;
    Fail: Boolean;
    RawFailure: Boolean;
    { Kept only until a callback explicitly retires its capture owner. The
      channel borrows this receiver and therefore cannot form a cycle. }
    RetireWriter: INyxCollectionDraftRecoveryWriter;
    procedure Changed(const AObservation: TNyxDraftRecoveryObservation);
  end;

procedure TRecoveryProbe.Changed(const AObservation: TNyxDraftRecoveryObservation);
begin
  Inc(Calls);
  Last := AObservation;

  if RetireWriter <> nil then
  begin
    RetireWriter.Retire;
    RetireWriter := nil;
  end;

  if TryMutation then
  begin
    try
      Journal.ConfigureWarnings(NyxDraftRecoveryWarnings.Entries(1));
    except
      on ENyxCollection do
      begin
        MutationRefused := True;
      end;
    end;
  end;

  if Fail then
  begin
    raise ENyxCollection.Create('Recovery receiver 🌙 failed');
  end;
  {$ifdef PAS2JS}

  if RawFailure then
  begin
    asm throw undefined; end;
  end;
  {$endif}
end;

{ Observing the capture writer never lends subscription/capture authority to
  retained readers. Explicit data mutations notify after admission, independently
  of widgets and with the same borrowed-token discipline as ordinary stores. }
procedure ObservationChecks;
const
  CFailure: TNyxText =
    'Recovery membership committed; observer failed: Recovery receiver 🌙 failed';
var
  LWriter: INyxCollectionDraftRecoveryWriter;
  LJournal: INyxCollectionDraftRecovery;
  LChanges: INyxCollectionDraftRecoveryChanges;
  LForbidden: INyxCollectionDraftRecoveryChanges;
  LFirstToken: INyxCollectionSubscription;
  LSecondToken: INyxCollectionSubscription;
  LFirst: TRecoveryProbe;
  LSecond: TRecoveryProbe;
  LCell: TNyxCollectionCellEdit;
  LEntry: INyxRecoveredCollectionDraft;
  LRefused: Boolean;
  LFailureText: TNyxText;
  LBeforeRetireCalls: Integer;
  {$ifndef PAS2JS}LFailureBytes: RawByteString;{$endif}
  {$ifdef PAS2JS}
  LRawFailed: Boolean;
  LBeforeCalls: Integer;
  {$endif}
begin
  LFirst := TRecoveryProbe.Create;
  LSecond := TRecoveryProbe.Create;
  try
    LWriter := NewNyxCollectionDraftRecovery;
    LJournal := LWriter.Recovery;
    Check(Supports(LWriter, INyxCollectionDraftRecoveryChanges, LChanges) and
      not Supports(LJournal, INyxCollectionDraftRecoveryChanges, LForbidden),
      'only the capture writer can register borrowed membership observers');
    LFirst.Journal := LJournal;
    LFirst.TryMutation := True;
    LFirstToken := LChanges.Subscribe({$ifdef PAS2JS}@{$endif}LFirst.Changed);
    LSecondToken := LChanges.Subscribe({$ifdef PAS2JS}@{$endif}LSecond.Changed);
    LCell := NyxCollectionCellEdit(NyxItem(NyxCollection('services'), 'gateway'),
      NyxColumnIdentity(NyxTextField('notes')), TNyxStateValue.FromText('Accepted'),
      CExact, NyxTextSelection(CExact, 1, 9), False, False);
    LEntry := LWriter.Preserve(LCell, ncdlHidden, 1, 2);
    Check((LFirst.Calls = 1) and (LSecond.Calls = 1) and
      (LFirst.Last.Count = 1) and (LSecond.Last.Revision = LJournal.Capture.Revision) and
      LFirst.MutationRefused and (LJournal.EntryAt(0) = LEntry),
      'observers see committed membership while reentrant journal mutation refuses');
    LFirst.TryMutation := False;
    LFirst.Fail := True;
    LRefused := False;
    LFailureText := '';
    try
      LJournal.Discard(LEntry);
    except
      on E: ENyxCollectionNotification do
      begin
        LRefused := True;
        {$ifdef PAS2JS}
        LFailureText := E.Message;
        {$else}
        LFailureBytes := RawByteString(E.Message);
        SetCodePage(LFailureBytes, CP_UTF8, False);
        LFailureText := TNyxText(LFailureBytes);
        {$endif}
      end;
    end;
    Check(LRefused and (LJournal.Capture.Count = 0) and (LSecond.Calls = 2) and
      (LSecond.Last.Count = 0) and
      (LFailureText = CFailure),
      'a committed observer failure preserves exact Unicode and never skips independent receivers');
    LFirstToken.Disconnect;
    LWriter.Preserve(LCell, ncdlHidden, 2, 3);
    Check(not LFirstToken.Connected and (LFirst.Calls = 2) and (LSecond.Calls = 3),
      'disconnecting one token revokes only that borrowed callback');
    LSecondToken := nil;
    LWriter.Preserve(LCell, ncdlHidden, 3, 4);
    Check(LSecond.Calls = 3, 'last-token release disconnects without the channel retaining it');
    LSecondToken := LChanges.Subscribe({$ifdef PAS2JS}@{$endif}LSecond.Changed);
    LChanges := nil;
    LWriter := nil;
    Check(not LSecondToken.Connected and not LJournal.Capture.Active and
      LJournal.Discard(LJournal.EntryAt(0)) and (LSecond.Calls = 3),
      'writer destruction detaches all callbacks while retained data remains explicitly discardable');
    {$ifdef PAS2JS}
    LWriter := NewNyxCollectionDraftRecovery;
    LJournal := LWriter.Recovery;
    Supports(LWriter, INyxCollectionDraftRecoveryChanges, LChanges);
    LFirst.Fail := False;
    LFirst.RawFailure := True;
    LFirstToken := LChanges.Subscribe(@LFirst.Changed);
    LSecondToken := LChanges.Subscribe(@LSecond.Changed);
    LBeforeCalls := LSecond.Calls;
    LRawFailed := False;
    try
      LWriter.Preserve(LCell, ncdlHidden, 4, 5);
    except
      LRawFailed := isUndefined(JSExceptValue);
    end;
    Check(LRawFailed and (LJournal.Capture.Count = 1) and
      (LSecond.Calls = LBeforeCalls + 1),
      'a raw undefined observer failure stays exact after other browser observers run');
    {$endif}
    LWriter := NewNyxCollectionDraftRecovery;
    LJournal := LWriter.Recovery;
    Supports(LWriter, INyxCollectionDraftRecoveryChanges, LChanges);
    LFirst.Fail := False;
    LFirst.RawFailure := False;
    LFirst.RetireWriter := LWriter;
    LFirstToken := LChanges.Subscribe({$ifdef PAS2JS}@{$endif}LFirst.Changed);
    LSecondToken := LChanges.Subscribe({$ifdef PAS2JS}@{$endif}LSecond.Changed);
    LBeforeRetireCalls := LSecond.Calls;
    LEntry := LWriter.Preserve(LCell, ncdlHidden, 5, 6);
    Check(not LFirstToken.Connected and not LSecondToken.Connected and
      not LJournal.Capture.Active and (LJournal.EntryAt(0) = LEntry) and
      (LSecond.Calls = LBeforeRetireCalls),
      'retirement inside a notification revokes remaining callbacks without losing the committed entry');
  finally
    LFirstToken := nil;
    LSecondToken := nil;
    LChanges := nil;
    LWriter := nil;
    LFirst.Free;
    LSecond.Free;
  end;
end;

{ Qualify hard ownership admission independently of warnings and physical hosts.
  Rejected captures never acquire another text/cell owner inside the journal. }
procedure BudgetChecks;
var
  LWriter: INyxCollectionDraftRecoveryWriter;
  LJournal: INyxCollectionDraftRecovery;
  LBudget: INyxCollectionDraftRecoveryBudget;
  LCell: TNyxCollectionCellEdit;
  LNext: TNyxCollectionCellEdit;
  LEntry: INyxRecoveredCollectionDraft;
  LRetained: INyxRecoveredCollectionDraft;
  LBefore: TNyxDraftRecoveryBudgetObservation;
  LReceipt: TNyxDraftRecoveryBudgetObservation;
  LBytes: Integer;
  LSaturated: Boolean;
  LRefused: Boolean;
  LScoped: INyxCollectionDraftRecoveryScopedWriter;
  LFrame: TNyxDraftRecoveryFrame;
  LOtherFrame: TNyxDraftRecoveryFrame;
begin
  LWriter := NewNyxCollectionDraftRecovery;
  LJournal := LWriter.Recovery;
  LBudget := NyxCollectionDraftRecoveryBudget(LJournal);
  Check((LBudget <> nil) and (LBudget.CaptureBudget.Limits.MaximumEntries = 256) and
    (LBudget.CaptureBudget.Limits.MaximumPayloadBytes = 16 * 1024 * 1024),
    'the same journal exposes finite independent entry/payload defaults');
  LCell := NyxCollectionCellEdit(NyxItem(NyxCollection('services'), 'gateway'),
    NyxColumnIdentity(NyxTextField('notes')), TNyxStateValue.FromText('Accepted 🌙' + #0),
    CExact, NyxTextSelection(CExact, 1, 9), True, True);
  LBytes := NyxDraftRecoveryPayloadBytes(LCell, LSaturated);
  Check(not LSaturated and (LBytes = NyxUTF8ByteCount(CExact) +
    NyxUTF8ByteCount('Accepted 🌙' + #0) + Length('servicesgatewaynotes')),
    'logical payload includes supplementary/NUL draft, accepted text and all retained names');
  LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(1).PayloadBytes(LBytes));
  LEntry := LWriter.Preserve(LCell, ncdlValueChanged, 1, 2);
  Check((LBudget.CaptureBudget.PayloadBytes = LBytes) and (LJournal.Capture.Count = 1),
    'exact-capacity admission preserves every byte without truncation');
  Check(LWriter.Preserve(LCell, ncdlValueChanged, 1, 3) = LEntry,
    'an exact retry coalesces even at both hard limits');
  LNext := NyxCollectionCellEdit(LCell.Item, LCell.Field, LCell.Accepted,
    CExact + 'x', Default(TNyxTextSelection), False, False);
  LRefused := False;
  try
    LWriter.Preserve(LNext, ncdlValueChanged, 2, 3);
  except
    on E: ENyxDraftRecoveryPressure do
    begin
      LRefused := True;
      LReceipt := E.Budget;
    end;
  end;
  Check(LRefused and (LReceipt.LastRefusal = ncrpEntryCount) and
    (LReceipt.RequiredPayloadBytes = LBytes + 1) and not LReceipt.CanRetry and
    (LJournal.Capture.Count = 1) and (LJournal.EntryAt(0) = LEntry),
    'entry pressure returns a scalar receipt and leaves all existing user work untouched');
  LBefore := LBudget.CaptureBudget;
  LRefused := False;
  try
    LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(0));
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LBudget.CaptureBudget.Revision = LBefore.Revision) and
    (LBudget.CaptureBudget.PayloadBytes = LBytes), 'lower limits cannot implicitly evict accepted entries');
  LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(2).PayloadBytes(LBytes));
  LRefused := False;
  try
    LWriter.Preserve(LNext, ncdlValueChanged, 2, 3);
  except
    on E: ENyxDraftRecoveryPressure do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LBudget.CaptureBudget.LastRefusal = ncrpPayloadBytes) and
    (LBudget.CaptureBudget.PayloadBytes = LBytes), 'payload capacity is enforced independently of entry capacity');
  LRetained := LEntry;
  Check(LJournal.Discard(LEntry) and (LBudget.CaptureBudget.PayloadBytes = 0) and
    (LBudget.CaptureBudget.LastRefusal = ncrpPayloadBytes) and not LBudget.CaptureBudget.CanRetry,
    'deliberate discard releases journal ownership but does not misreport an oversized candidate as ready');
  LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(2).PayloadBytes(LBytes + 1));
  Check(LBudget.CaptureBudget.CanRetry and (LRetained.Cell.Text = CExact),
    'caller override admits a retry while independently retained discarded facts remain readable');
  LEntry := LWriter.Preserve(LNext, ncdlValueChanged, 2, 4);
  Check((LBudget.CaptureBudget.LastRefusal = ncrpNone) and
    (LBudget.CaptureBudget.PayloadBytes = LBytes + 1) and (LEntry.Cell.Text = CExact + 'x'),
    'successful retry clears only pressure facts and installs exact new ownership');
  LBefore := LBudget.CaptureBudget;
  LRefused := False;
  try
    LBudget.ConfigureLimits(Default(TNyxDraftRecoveryLimits));
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LBudget.CaptureBudget.Revision = LBefore.Revision),
    'undefined limits cannot replace accepted configuration');
  LWriter.Retire;
  Check(not LBudget.CaptureBudget.Active and not LBudget.CaptureBudget.CanRetry and
    (LBudget.CaptureBudget.PayloadBytes = LBytes + 1), 'retirement preserves bounded data without capture/retry authority');
  Check(LJournal.Discard(LEntry) and (LBudget.CaptureBudget.PayloadBytes = 0),
    'retired data owners can deliberately release exact admitted payload');
  LWriter := NewNyxCollectionDraftRecovery;
  LJournal := LWriter.Recovery;
  LBudget := NyxCollectionDraftRecoveryBudget(LJournal);
  Check(Supports(LWriter, INyxCollectionDraftRecoveryScopedWriter, LScoped),
    'the capture owner has a scoped capability separate from readonly data');
  LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(1));
  LFrame := NewNyxDraftRecoveryFrame;
  LOtherFrame := NewNyxDraftRecoveryFrame;
  LEntry := LScoped.PreserveInFrame(LCell, ncdlValueChanged, 1, 2, LFrame);
  Check((LScoped.PreserveInFrame(LCell, ncdlUnknownIdentity, 1, 4, LFrame) = LEntry) and
    (LEntry.Reason = ncdlValueChanged) and (LEntry.AfterRevision = 2),
    'same-frame retry preserves the first receipt when later loss provenance weakens');
  LRefused := False;
  try
    LScoped.PreserveInFrame(LCell, ncdlValueChanged, 1, 4, LOtherFrame);
  except
    on E: ENyxDraftRecoveryPressure do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LJournal.Capture.Count = 1),
    'another frame is independent work even with identical source revision, text and reason');
  LJournal.Discard(LEntry);
  LRetained := LScoped.PreserveInFrame(LCell, ncdlValueChanged, 1, 4, LOtherFrame);
  Check((LRetained <> LEntry) and (LJournal.Capture.Count = 1),
    'a new frame gets its own exact receipt after deliberate capacity release');
end;

procedure Run;
var
  LWriter: INyxCollectionDraftRecoveryWriter;
  LOther: INyxCollectionDraftRecoveryWriter;
  LUnexpected: INyxCollectionDraftRecoveryWriter;
  LJournal: INyxCollectionDraftRecovery;
  LEntry: INyxRecoveredCollectionDraft;
  LRetained: INyxRecoveredCollectionDraft;
  LForeign: INyxRecoveredCollectionDraft;
  LCell: TNyxCollectionCellEdit;
  LValue: TNyxCollectionCellEdit;
  LBefore: TNyxDraftRecoveryObservation;
  LAfter: TNyxDraftRecoveryObservation;
  LRefused: Boolean;
begin
  LWriter := NewNyxCollectionDraftRecovery;
  LJournal := LWriter.Recovery;
  Check(LJournal.Capture.Active and (LJournal.Capture.Count = 0),
    'a new recovery journal admits captures without owning any physical target');
  Check(not Supports(LJournal, INyxCollectionDraftRecoveryWriter, LUnexpected),
    'the readonly data port cannot acquire its former capture/lifecycle writer');
  LCell := NyxCollectionCellEdit(NyxItem(NyxCollection('services'), 'gateway'),
    NyxColumnIdentity(NyxTextField('notes')), TNyxStateValue.FromText('Accepted'),
    CExact, NyxTextSelection(CExact, 1, 9), True, True);
  LEntry := LWriter.Preserve(LCell, ncdlRowRemoved, 7, 8);
  LValue := LEntry.Cell;
  Check((LValue.Text = CExact) and LValue.Selection.SameRange(LCell.Selection),
    'recovery preserves supplementary Unicode, NUL, original line endings and scalar caret');
  Check(not LValue.Editing and not LValue.Focused and
    LValue.Accepted.SameValue(LCell.Accepted) and LValue.Field.Same(LCell.Field),
    'retained value preserves typed provenance while withdrawing physical edit/focus authority');
  Check((LEntry.BeforeRevision = 7) and (LEntry.AfterRevision = 8) and
    (LEntry.Reason = ncdlRowRemoved) and (LEntry.ByteCount = NyxUTF8ByteCount(CExact)),
    'reason/revision and exact UTF-8 text size are immutable facts');
  LValue := Default(TNyxCollectionCellEdit);
  Check(LEntry.Cell.Defined and (LEntry.Cell.Text = CExact),
    'changing a returned local does not alter the retained draft');
  LBefore := LJournal.Capture;
  LRetained := LWriter.Preserve(LCell, ncdlRowRemoved, 7, 9);
  Check((LRetained = LEntry) and (LJournal.Capture.Revision = LBefore.Revision) and
    (LJournal.Capture.Count = 1), 'a deferred retry coalesces the exact original physical draft');
  LRefused := False;
  try
    LWriter.Preserve(LCell, ncdlRowRemoved, 7, 6);
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LJournal.Capture.Count = 1),
    'a malformed retry cannot bypass revision validation through coalescing');
  LJournal.ConfigureWarnings(NyxDraftRecoveryWarnings.Entries(1).Bytes(1));
  Check(LJournal.Capture.Warning and (LJournal.Capture.Count = 1) and
    (LJournal.Capture.RetainedBytes = NyxUTF8ByteCount(CExact)),
    'advisory thresholds report retained text without truncating or discarding it');
  LJournal.ConfigureWarnings(NyxDraftRecoveryWarnings.Entries(0).Bytes(0));
  Check(not LJournal.Capture.Warning and (LJournal.Capture.Count = 1),
    'callers can disable warnings without changing retained user work');
  LBefore := LJournal.Capture;
  LRefused := False;
  try
    LJournal.ConfigureWarnings(NyxDraftRecoveryWarnings.Entries(-1));
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LJournal.Capture.Revision = LBefore.Revision),
    'invalid warning configuration leaves the accepted journal unchanged');
  LOther := NewNyxCollectionDraftRecovery;
  LForeign := LOther.Preserve(LCell, ncdlRowRemoved, 7, 8);
  Check(not LJournal.Discard(LForeign) and (LJournal.Capture.Count = 1),
    'equal text and cell identity from another journal cannot discard this exact entry');
  LRetained := LWriter.Preserve(LCell, ncdlValueChanged, 9, 10);
  Check((LRetained <> LEntry) and (LJournal.Capture.Count = 2),
    'later user work from another source revision is not conflated with an old recovery');
  Check(LJournal.Discard(LEntry) and (LJournal.Capture.Count = 1) and
    (LJournal.EntryAt(0) = LRetained), 'deliberate discard removes only the reviewed interface identity');
  Check(not LJournal.Discard(LEntry) and (LJournal.Capture.Count = 1),
    'a repeated stale discard cannot remove a new occupant of that old index');
  Check(LEntry.Cell.Text = CExact, 'discarded immutable facts stay readable by an independent owner');
  LBefore := LJournal.Capture;
  LWriter.Retire;
  LAfter := LJournal.Capture;
  Check(not LAfter.Active and (LAfter.Count = 1) and (LAfter.Revision > LBefore.Revision),
    'retirement changes lifecycle first while retaining recovery text');
  LRefused := False;
  try
    LWriter.Preserve(LCell, ncdlIdentityReplaced, 10, 11);
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LJournal.Capture.Count = 1), 'retired capture cannot revive former authority');
  LWriter := nil;
  Check((LJournal.EntryAt(0).Cell.Text = CExact) and LJournal.Discard(LRetained),
    'a data-only journal survives its writer and supports deliberate discard after retirement');
  LJournal := nil;
  Check(LRetained.Cell.Text = CExact, 'an immutable recovered value can outlive both writer and journal');
end;

begin
  try
    Run;
    BudgetChecks;
    ObservationChecks;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-recovery-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' collection draft recovery checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-recovery-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
