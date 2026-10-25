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
  nyx.service.monitor
  {$ifdef NYX_INSPECTION_GENERATED}, nyx.generated.view{$endif};

const
  CFirstDraft: TNyxText = 'A careful unfinished note 🌙';
  CSecondDraft: TNyxText = 'Another unfinished note 🌙';
  CThirdDraft: TNyxText = 'Keep this while the record is hidden 🌙';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create('Recovery controls: ' + AReason);
  end;
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

  procedure Click(const AID: TNyxText);
  begin
    SendMessage(TWinControl(LApplication.View.ControlFor(AID)).Handle, BM_CLICK, 0, 0);
    Application.ProcessMessages;
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
    LDemo := TNyxLiveInspectionDemo.Create(LApplication.Context, LApplication.View.Events,
      LFirst.View, LPeer.View, NyxCollectionPositionState(LFirst), LJournal);
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
    LDemo.Close;
    LApplication.Free;
    LApplication := nil;
    Check(not LJournal.Capture.Active and (LJournal.Capture.Count = 2) and
      (LJournal.EntryAt(0).Cell.Text = CFirstDraft), 'retained recovery remains readable after its actual native application retires');
  finally
    LDemo.Free;
    LFirst := nil;
    LPeer := nil;
    LApplication.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Application.Initialize;
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
