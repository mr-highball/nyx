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
program nyx_table_recovery_application_controls;

{$mode delphi}{$H+}{$codepage utf8}

{ Compile beside the unchanged Service monitor unit emitted by its Studio
  application job. This qualifies that saved application's real native editor,
  empty-result launcher and Nyx review controls; it constructs no alternate seed.
  Public view query changes deliberately isolate recovery from pointer/cursor
  availability. The full table consumer separately owns physical filter input. }
uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, Grids, Windows,
  nyx.text, nyx.model, nyx.controls, nyx.codec, nyx.scheduler, nyx.application.lcl,
  nyx.state,
  nyx.collections, nyx.collections.query, nyx.collections.mount,
  nyx.collections.lcl.grid, nyx.collections.cell.actions, nyx.collections.recovery,
  nyx.popover, nyx.popover.lcl, nyx.generated.view;

const
  CTable: TNyxText = 'service-table';
  CDraft: TNyxText = 'Recovered gateway 🌙';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure Pump;
begin
  Application.ProcessMessages;
  CheckSynchronize(1);
end;

procedure AwaitAction(const AActions: INyxCollectionCellActions);
var
  LStarted: QWord;
  LExecution: INyxExecution;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    LExecution := AActions.LastExecution;

    if (LExecution <> nil) and not (LExecution.Status in [nesPending, nesRunning]) then
    begin
      Break;
    end;
  until GetTickCount64 - LStarted > 1000;
  Check((LExecution <> nil) and (LExecution.Status = nesSucceeded),
    'Original recovery work must complete successfully within one second');
end;

procedure Journey(AWidth: Integer);
var
  LDocument: TNyxDocument;
  LApp: TNyxLCLApplication;
  LAuthored: TNyxText;
  LMount: INyxCollectionMount;
  LReview: INyxCollectionRecoveryReviewMount;
  LCells: INyxCollectionCellActionsMount;
  LActions: INyxCollectionCellActions;
  LJournal: INyxCollectionDraftRecovery;
  LOriginal: INyxCollectionSnapshot;
  LGrid: TStringGrid;
  LFace: TStringGrid;
  LPhysical: Integer;
  LEditor: TCustomEdit;
  LButton: TNyxCollectionColumnButton;
  LPopup: INyxLCLPopover;
  LMemo: INyxMemo;

  { Pinned fields can live in an ordinary child grid. Resolve the saved field's
    actual header face before F2, rather than mistaking the outer spacer for
    column zero. Compact pin suspension may put it back in the outer grid. }
  function ServiceGrid(out AColumn: Integer): TStringGrid;
  var
    LTitle: TNyxText;
    LChild: Integer;
    LCandidate: TStringGrid;

    function FindIn(AGrid: TStringGrid): Boolean;
    var
      LColumn: Integer;
    begin
      Result := False;
      for LColumn := AGrid.FixedCols to AGrid.ColCount - 1 do
      begin

        if Pos(LTitle, TNyxText(AGrid.Cells[LColumn, 0])) = 1 then
        begin
          AColumn := LColumn;
          Exit(True);
        end;
      end;
    end;

  begin
    LTitle := LMount.View.Spec.ColumnAt(0).Title;
    AColumn := -1;
    Result := LGrid;

    if FindIn(LGrid) then
    begin
      Exit;
    end;
    for LChild := 0 to LGrid.ControlCount - 1 do
    begin

      if (LGrid.Controls[LChild] is TStringGrid) and LGrid.Controls[LChild].Visible then
      begin
        LCandidate := TStringGrid(LGrid.Controls[LChild]);

        if FindIn(LCandidate) then
        begin
          Exit(LCandidate);
        end;
      end;
    end;
    Result := nil;
  end;
begin
  LDocument := BuildNyxDocument;
  LApp := nil;
  try
    LAuthored := TNyxCodec.Encode(LDocument);
    Check((LDocument.Title = 'Service monitor') and (LDocument.Find(CTable) <> nil),
      'Use the same saved Service monitor rather than a replacement fixture');
    LApp := TNyxLCLApplication.Create;
    LApp.Mount(LDocument);
    LApp.Window.ClientWidth := AWidth;
    LApp.Window.ClientHeight := 900;
    LApp.Window.Show;
    LApp.Window.BringToFront;
    Pump;
    Check(LApp.Window.ClientWidth = AWidth, 'The actual native window retains the requested width');
    LMount := LApp.View.CollectionMount(CTable);
    Check(Supports(LMount, INyxCollectionRecoveryReviewMount, LReview) and
      Supports(LMount, INyxCollectionCellActionsMount, LCells),
      'The saved table exposes its original recovery and presentation capabilities');
    LActions := LCells.CellActions;
    LOriginal := LMount.View.Store.Snapshot;
    LGrid := TStringGrid(LApp.View.ControlFor(CTable));
    LFace := ServiceGrid(LPhysical);
    Check((LFace <> nil) and (LPhysical >= 0) and
      (LFace.Cells[LPhysical, 1] = 'Request gateway'),
      'Resolve the actual saved Service field and source row before typing');
    LFace.Col := LPhysical;
    LFace.Row := 1;
    LFace.SetFocus;
    SendMessage(LFace.Handle, WM_KEYDOWN, VK_F2, 0);
    Pump;
    Check(LFace.EditorMode and (LFace.Editor is TCustomEdit),
      'F2 enters the actual saved application cell editor');
    LEditor := TCustomEdit(LFace.Editor);
    LEditor.Text := CDraft;
    LEditor.SelStart := 1;
    LEditor.SelLength := 3;
    LMount.View.ConfigureQuery(NyxCollectionQuery.Where(
      NyxWhere(NyxTextField('status')).EqualTo('no matching service')));
    Pump;
    LJournal := NyxCollectionDraftRecovery(LMount);
    Check((LMount.View.Snapshot.Count = 0) and (LReview.RecoveredEditCount = 1) and
      (LJournal.EntryAt(0).Cell.Text = CDraft) and
      (LJournal.EntryAt(0).Cell.Field.Name = 'name') and
      (LJournal.EntryAt(0).Reason = ncdlHidden) and
      (LJournal.EntryAt(0).Cell.Selection.Start = 1) and
      (LJournal.EntryAt(0).Cell.Selection.Finish = 4),
      'An empty result retains exact real draft text, scalar caret and hidden-row provenance');
    LButton := TNyxCollectionColumnButton(LGrid.FindComponent('NyxRecoveredEditsButton'));
    Check((LButton <> nil) and LButton.Visible and LButton.Enabled and
      (LButton.AccessibleName = 'Review recovered edits'),
      'The empty saved table exposes an enabled accessible recovery launcher');
    LButton.Click;
    AwaitAction(LActions);
    Check((LActions.Inspector <> nil) and LActions.Inspector.IsOpen and
      Supports(LActions.Inspector, INyxLCLPopover, LPopup),
      'The real saved native application opens its managed Nyx review window');
    Check(Supports(RetainNyxControl(LPopup.Content.Node.Find('nyx-recovered-edit-draft')),
      INyxMemo, LMemo) and LMemo.ReadOnly and (LMemo.Value = CDraft),
      'The specialized recovered Memo presents the exact unfinished value');
    Check(TCustomMemo(LPopup.Renderer.InputFor('nyx-recovered-edit-draft')).ReadOnly,
      'The actual native recovered editor is readonly');
    Check(Supports(RetainNyxControl(LPopup.Content.Node.Find('nyx-recovered-edit-accepted')),
      INyxMemo, LMemo) and LMemo.ReadOnly and (LMemo.Value = 'Request gateway'),
      'Accepted source text remains separate from the inert recovered draft');
    TButton(LPopup.Renderer.ControlFor(NyxRecoveredEditsCopyID)).Click;
    AwaitAction(LActions);
    Check(Pos('not supplied a clipboard service', LPopup.State.GetValue(
      NyxTextState(NyxCellInspectorStatusState))) > 0,
      'The original unavailable clipboard host gives a visible refusal');
    Check((LMount.View.Store.Snapshot = LOriginal) and
      (LJournal.Capture.Count = 1) and (TNyxCodec.Encode(LDocument) = LAuthored),
      'Review and refused Copy preserve accepted source, journal and authored design exactly');
    LActions.Close;
    Check(not LPopup.IsOpen, 'Close retires only the managed review presentation');
    LMount.Disconnect;
    Check(not LReview.OpenRecoveredEdits and (LReview.RecoveredEditCount = 0) and
      (LJournal.EntryAt(0).Cell.Text = CDraft),
      'Retired mounted callbacks cannot reopen review while detached retained text stays readable');
  finally
    LPopup := nil;
    LMemo := nil;
    LActions := nil;
    LCells := nil;
    LReview := nil;
    LMount := nil;
    LJournal := nil;
    LOriginal := nil;
    LApp.Free;
    LDocument.Free;
    Pump;
  end;
end;

var
  LWidth: Integer;
begin
  try

    if (ParamCount <> 1) or not TryStrToInt(ParamStr(1), LWidth) or
      ((LWidth <> 1280) and (LWidth <> 390)) then
    begin
      raise Exception.Create('Expected the actual native window width 1280 or 390');
    end;
    Application.Initialize;
    Application.CaptureExceptions := False;
    Journey(LWidth);
    WriteLn('PASS ', GChecks, ' saved Service monitor recovery checks at ', LWidth);
  except
    on E: Exception do
    begin
      WriteLn('FAIL after ', GChecks, ' checks: ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
