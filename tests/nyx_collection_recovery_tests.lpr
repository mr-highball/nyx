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
  {$ifdef PAS2JS}, Web{$endif};

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
    'later user work or a different loss is not conflated with an old recovery');
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
