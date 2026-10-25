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
  nyx.service.monitor, nyx.files.save, nyx.bytes, nyx.scheduler
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
  LInput: TJSHTMLInputElement;
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
      .querySelector('[data-nyx-item="gateway"] input[type="text"]'));
    Check(LInput <> nil, 'an actual mounted text input is available');
    LInput.focus;
    LInput.value := CDraft;
    LInput.setSelectionRange(2, 6);
    LFirst.View.Store.Update(NyxCollectionItem(LGateway)
      .WithValue(NyxTextField('notes'), 'Producer changed the accepted note'));
    Check(NyxCollectionDraftRecoveryHeld(LFirst) and not LAdmission.InputReady and
      (LInput.value = CDraft) and (LJournal.Capture.Count = 0) and
      (LPeer.View.Snapshot.Revision = LFirst.View.Store.Snapshot.Revision),
      'zero-capacity pressure holds the real DOM draft while source and peer remain live');
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
