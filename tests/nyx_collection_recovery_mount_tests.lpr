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
  nyx.collections.query
  {$ifdef PAS2JS}, Web{$endif};

type
  { Model-only target exercises the actual mount notification, recovery and
    input-lifetime machinery. Its copied frame/draft are values, not widgets.
    Actual native/browser controls remain separate qualification. }
  TRecoveryTarget = class(TNyxCollectionMountBase)
  public
    Frame: INyxCollectionSnapshot;
    Draft: TNyxCollectionCellEdit;
    Fail: Boolean;
    Loss: TNyxCollectionDraftLoss;
    procedure RenderDataset; override;
    procedure DetachTarget; override;
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

procedure TRecoveryTarget.RenderDataset;
var
  LNext: INyxCollectionSnapshot;
begin

  if Fail then
  begin
    raise ENyxCollection.Create('Expected physical refresh failure');
  end;
  LNext := FView.Snapshot;

  if Draft.Defined and RecoverCell(Draft, Frame, LNext, Loss) then
  begin
    Draft := Default(TNyxCollectionCellEdit);
  end;
  Frame := LNext;
end;

procedure TRecoveryTarget.DetachTarget;
begin
  Frame := nil;
  Draft := Default(TNyxCollectionCellEdit);
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
  LMount.Disconnect;
  LInput := nil;
  LMount := nil;
  Check(not LJournal.Capture.Active and (LJournal.Capture.Count = 4) and
    (LJournal.EntryAt(0).Cell.Text = CDraft),
    'a retained journal survives physical target disposal without retaining its authority');
end;

begin
  try
    Run;
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
