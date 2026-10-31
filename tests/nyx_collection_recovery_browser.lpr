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
program nyx_collection_recovery_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, JS, Web, nyx.text, nyx.model, nyx.codec, nyx.state,
  nyx.application.browser, nyx.collections, nyx.collections.mount,
  nyx.collections.recovery, nyx.collections.position, nyx.live.inspection,
  nyx.service.monitor, nyx.files.save, nyx.bytes, nyx.scheduler,
  nyx.types, nyx.test.keyboard.browser
  {$ifdef NYX_INSPECTION_GENERATED}, nyx.generated.view{$endif};

type
  { Synchronous injected save boundary records exact bytes and reports only a
    simulated terminal outcome. This exercises UI delivery, not actual disk or
    user-activation/picker behavior, which the platform host qualifies elsewhere. }
  TSaveHost = class(TInterfacedObject, INyxFileSaveHost)
  public
    Bytes: TNyxBytes;
    Calls: Integer;
    Outcome: TNyxFileSaveOutcome;
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

var
  GChecks: Integer;

function TSaveHost.BeginSave(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LResult: TNyxFileSaveResult;
begin
  AScheduler.RequireUI;
  AOptions.Admit(ABytes);
  Bytes := Copy(ABytes, 0, Length(ABytes));
  Inc(Calls);
  LResult := Default(TNyxFileSaveResult);
  LResult.Outcome := Outcome;
  LResult.Name := AOptions.SuggestedName;

  if Outcome = nfsoWritten then
  begin
    LResult.ByteCount := Length(ABytes);
  end;
  AReply(LResult);
  Result := nil;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create('Browser recovery: ' + AReason);
  end;
end;

procedure Journey(AWidth: Integer);
const
  CDraft: TNyxText = 'Keep this unfinished 🌙' + #0 + ' note';
var
  LDocument: TNyxDocument;
  LApplication: TNyxBrowserApplication;
  LDemo: TNyxLiveInspectionDemo;
  LHost: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LRecoveryButton: TJSHTMLElement;
  LTable: TJSHTMLElement;
  LFirst: INyxCollectionMount;
  LPeer: INyxCollectionMount;
  LAdmission: INyxCollectionInputAdmission;
  LPending: INyxCollectionPendingDraftMount;
  LReview: INyxPendingCollectionDraft;
  LJournal: INyxCollectionDraftRecovery;
  LBudget: INyxCollectionDraftRecoveryBudget;
  LBefore: INyxCollectionSnapshot;
  LGateway: TNyxItemRef;
  LSave: TSaveHost;
  LSaveLease: INyxFileSaveHost;
  LSaved: TNyxText;

  procedure Click(const AID: TNyxText);
  begin
    TJSHTMLElement(LApplication.View.ElementFor(AID)).click;
  end;

begin
  {$ifdef NYX_INSPECTION_GENERATED}
  LDocument := BuildNyxDocument;
  {$else}
  LDocument := BuildLiveInspectionDocument;
  {$endif}
  LApplication := nil;
  LDemo := nil;
  LHost := TJSHTMLElement(document.createElement('div'));
  LHost.style.setProperty('width', IntToStr(AWidth) + 'px');
  document.body.appendChild(LHost);
  try
    LSaved := TNyxCodec.Encode(LDocument);
    LApplication := TNyxBrowserApplication.Create;
    LApplication.Run(LDocument, LHost);
    LFirst := LApplication.View.CollectionMount(ServiceTableID);
    LPeer := LApplication.View.CollectionMount(InspectionPeerID);
    Supports(LFirst, INyxCollectionPendingDraftMount, LPending);
    Supports(LFirst, INyxCollectionInputAdmission, LAdmission);
    LJournal := NyxCollectionDraftRecovery(LFirst);
    LBudget := NyxCollectionDraftRecoveryBudget(LJournal);
    LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(0).PayloadBytes(0));
    LSave := TSaveHost.Create;
    LSaveLease := LSave;
    LSave.Outcome := nfsoWritten;
    LDemo := TNyxLiveInspectionDemo.Create(LApplication.Context, LApplication.View.Events,
      LFirst.View, LPeer.View, NyxCollectionPositionState(LFirst), LJournal, LFirst, LSaveLease);
    LGateway := NyxItem(NyxCollection(ServiceCollectionName), 'gateway');
    LInput := TJSHTMLInputElement(LApplication.View.ElementFor(ServiceTableID)
      .querySelector('[data-nyx-item="gateway"] input[aria-label="Notes"]'));
    Check(LInput <> nil, 'the actual mounted Notes input is available');
    { Service is editable too. Enter the Notes cell through ordinary navigation
      so the producer update conflicts with this exact field's active draft,
      rather than writing into the first hidden editor in the row. }
    LCell := TJSHTMLElement(LInput.closest('[role="gridcell"]'));
    LCell.focus;
    LCell.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'F2'));
    Check((document.activeElement = LInput) and not LInput.hasAttribute('hidden'),
      'ordinary F2 navigation enters the visible Notes editor before typing');
    LInput.value := CDraft;
    LInput.setSelectionRange(2, 6);
    LFirst.View.Store.Update(NyxCollectionItem(LGateway)
      .WithValue(NyxTextField('notes'), 'Producer changed the accepted note'));
    Check(NyxCollectionDraftRecoveryHeld(LFirst) and not LAdmission.InputReady and
      (LInput.value = CDraft) and (LJournal.Capture.Count = 0) and
      (LPeer.View.Snapshot.Revision = LFirst.View.Store.Snapshot.Revision),
      'zero-capacity pressure holds the real DOM draft while source and peer remain live' +
      ' [held=' + BoolToStr(NyxCollectionDraftRecoveryHeld(LFirst), True) +
      '; ready=' + BoolToStr(LAdmission.InputReady, True) +
      '; text=' + BoolToStr(LInput.value = CDraft, True) +
      '; journal=' + IntToStr(LJournal.Capture.Count) +
      '; peer/source=' + IntToStr(LPeer.View.Snapshot.Revision) + '/' +
      IntToStr(LFirst.View.Store.Snapshot.Revision) + ']');
    LReview := LPending.CapturePendingDraft;
    Check((LReview <> nil) and (LReview.Cell.Text = CDraft) and
      LReview.Cell.Selection.Defined and (LReview.Cell.Selection.Start = 2),
      'pending capture retains exact NUL/supplementary text and scalar caret');
    Click(RecoveryReviewPendingID);
    Check(LApplication.State.GetValue(NyxBooleanState(RecoveryPendingReviewedID)),
      'ordinary pending review controls are bound to the mounted UI');
    Click(RecoveryExportPendingID);
    Check((LSave.Calls = 1) and (NyxDecodeUTF8(LSave.Bytes) = CDraft) and
      (LInput.value = CDraft) and NyxCollectionDraftRecoveryHeld(LFirst),
      'browser export delivers exact UTF-8 without consuming original work');
    LSave.Outcome := nfsoCancelled;
    Click(RecoveryExportPendingID);
    Check((LSave.Calls = 2) and (LInput.value = CDraft),
      'cancelled export leaves the original pending draft intact');
    Click(RecoveryDiscardPendingID);
    Check((LInput.value = CDraft) and NyxCollectionDraftRecoveryHeld(LFirst),
      'first discard click only asks for confirmation');
    LInput.value := CDraft + ' newer';
    Click(RecoveryDiscardPendingID);
    Check((LInput.value = CDraft + ' newer') and NyxCollectionDraftRecoveryHeld(LFirst),
      'a stale confirmation cannot discard newer DOM text');
    LBefore := LFirst.View.Store.Snapshot;
    Click(RecoveryDiscardPendingID);
    Click(RecoveryDiscardPendingID);
    Check(not NyxCollectionDraftRecoveryHeld(LFirst) and LAdmission.InputReady and
      (LFirst.View.Store.Snapshot = LBefore) and (LJournal.Capture.Count = 0),
      'confirmed pending discard restores current display without source writes or journal capacity');
    Check(not LPending.DiscardPendingDraft(LReview) and
      (TNyxCodec.Encode(LDocument) = LSaved), 'old receipts cannot alter a new frame or saved design');

    { The shared caption must also keep its recovered-edit command reachable
      when a real journal entry exists, alongside pin and clipboard status. }
    LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(4).PayloadBytes(8192));
    LTable := LApplication.View.ElementFor(ServiceTableID);
    LInput := TJSHTMLInputElement(LTable.querySelector(
      '[data-nyx-item="gateway"] input[aria-label="Notes"]'));
    Check(LInput <> nil, 'resumed display retains its mounted Notes editor');
    LCell := TJSHTMLElement(LInput.closest('[role="gridcell"]'));
    LCell.focus;
    LCell.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'F2'));
    Check(document.activeElement = LInput,
      'the resumed Notes editor still accepts ordinary navigation');
    LInput.value := CDraft;
    LInput.setSelectionRange(2, 6);
    LFirst.View.Store.Update(NyxCollectionItem(LGateway)
      .WithValue(NyxTextField('notes'), 'A later accepted note'));
    Check((LJournal.Capture.Count = 1) and not NyxCollectionDraftRecoveryHeld(LFirst) and
      (LJournal.EntryAt(0).Cell.Text = CDraft),
      'available journal capacity preserves the exact conflicting draft and resumes display');
    LRecoveryButton := TJSHTMLElement(LTable.querySelector('[data-nyx-recovery-button]'));
    Check((LRecoveryButton <> nil) and not LRecoveryButton.hasAttribute('disabled') and
      (LRecoveryButton.style.getPropertyValue('display') <> 'none'),
      'a real recovered entry exposes its enabled caption command');
    Check((LRecoveryButton.getBoundingClientRect.width > 0) and
      (LRecoveryButton.getBoundingClientRect.left >= LTable.getBoundingClientRect.left - 1) and
      (LRecoveryButton.getBoundingClientRect.right <= LTable.getBoundingClientRect.right + 1) and
      (TNyxCodec.Encode(LDocument) = LSaved),
      'recovery access fits the table clip without changing the authored project');
    LDemo.Close;
    LApplication.Free;
    LApplication := nil;
    Check((LPending.CapturePendingDraft = nil) and
      not LPending.DiscardPendingDraft(LReview) and (LReview.Cell.Text = CDraft),
      'retirement revokes action while detached review data stays readable');
  finally
    LDemo.Free;
    LApplication.Free;
    LDocument.Free;
    LHost.remove;
  end;
end;

begin
  try
    Journey(1280);
    Journey(390);
    document.body.setAttribute('data-pending-recovery-checks', IntToStr(GChecks));
  except
    on LError: Exception do
    begin
      document.body.setAttribute('data-pending-recovery-error', LError.Message);
    end;
  end;
end.
