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
program nyx_tree_recovery_application_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}
  JS, Web, nyx.application.browser, nyx.test.keyboard.browser,
  {$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, ComCtrls, Windows, nyx.application.lcl,
  nyx.collections.lcl.tree, nyx.editing.lcl,
  {$endif}
  SysUtils, nyx.text, nyx.types, nyx.behavior, nyx.events, nyx.model, nyx.state, nyx.codec, nyx.codegen,
  nyx.collections, nyx.collections.view, nyx.collections.mount,
  nyx.collections.recovery, nyx.collections.recovery.controls,
  nyx.collections.recovery.panel, nyx.collections.recovery.binding,
  nyx.collections.recovery.status, nyx.files.save, nyx.files, nyx.bytes,
  nyx.scheduler, nyx.editing, nyx.process.tree
  {$ifdef NYX_TREE_RECOVERY_GENERATED}, nyx.generated.tree.recovery{$endif};

type
  { Owned pending test request. Cancel revokes its borrowed receiver; Finish
    proves that a simulated late platform result cannot touch a retired app. }
  TSaveRequest = class(TInterfacedObject, INyxFileSaveRequest)
  public
    Cancelled: Boolean;
    Reply: TNyxFileSaveReply;
    Value: TNyxFileSaveResult;
    procedure Cancel;
    function Finish: Boolean;
  end;
  { Injected byte host validates actual panel requests, with inline and pending
    completion. It does not pretend to prove a native picker or a disk write. }
  TSaveRecorder = class(TInterfacedObject, INyxFileSaveHost)
  public
    Calls: Integer;
    Bytes: TNyxBytes;
    Hold: Boolean;
    Request: INyxFileSaveRequest;
    Pending: TSaveRequest;
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create('Tree recovery panel: ' + AReason);
  end;
end;

procedure TSaveRequest.Cancel;
begin
  Cancelled := True;
  Reply := nil;
end;

function TSaveRequest.Finish: Boolean;
var
  LReply: TNyxFileSaveReply;
begin
  Result := not Cancelled and Assigned(Reply);
  LReply := Reply;
  Reply := nil;

  if Result then
  begin
    LReply(Value);
  end;
end;

function TSaveRecorder.BeginSave(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LResult: TNyxFileSaveResult;
  LRequest: TSaveRequest;
begin
  AScheduler.RequireUI;
  AOptions.Admit(ABytes);
  Bytes := Copy(ABytes, 0, Length(ABytes));
  Inc(Calls);
  LResult := Default(TNyxFileSaveResult);
  LResult.Outcome := nfsoWritten;
  LResult.Name := AOptions.SuggestedName;
  LResult.ByteCount := Length(ABytes);

  if Hold then
  begin
    LRequest := TSaveRequest.Create;
    Result := LRequest;
    Request := Result;
    Pending := LRequest;
    LRequest.Reply := AReply;
    LRequest.Value := LResult;
  end
  else
  begin
    AReply(LResult);
    Result := nil;
  end;
end;

function SameBytes(const ALeft, ARight: TNyxBytes): Boolean;
var
  LIndex: Integer;
begin
  Result := Length(ALeft) = Length(ARight);

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to High(ALeft) do
  begin

    if ALeft[LIndex] <> ARight[LIndex] then
    begin
      Exit(False);
    end;
  end;
end;

procedure Journey(AWidth: Integer);
const
  CDraft: TNyxText = 'Keep 🌙 this unfinished label';
var
  LDocument: TNyxDocument;
  {$ifdef PAS2JS}
  LApplication: TNyxBrowserApplication;
  LHost: TJSHTMLElement;
  LEditor: TJSHTMLInputElement;
  {$else}
  LApplication: TNyxLCLApplication;
  LTree: TTreeView;
  LEditor: TCustomEdit;
  LNode: TTreeNode;
  LStream: TFileStream;
  LGenerated: UTF8String;
  {$endif}
  LDemo: TNyxProcessTreeDemo;
  LBinding: INyxCollectionRecoveryBinding;
  LMount: INyxCollectionMount;
  LPeer: INyxCollectionMount;
  LView: INyxCollectionView;
  LStore: INyxCollection;
  LJournal: INyxCollectionDraftRecovery;
  LBudget: INyxCollectionDraftRecoveryBudget;
  LNotifications: INyxCollectionRecoveryNotifications;
  LSave: TSaveRecorder;
  LSaveLease: INyxFileSaveHost;
  LParts: TNyxCollectionRecoveryParts;
  LReview: TNyxRecoveredDraftBindings;
  LGateway: TNyxItemRef;
  LBefore: INyxCollectionSnapshot;
  LSaved: TNyxText;
  LOldCalls: Integer;
  LAction: TNyxRecoveryAction;

  procedure Settle;
  {$ifndef PAS2JS}
  var
    LTurn: Integer;
  {$endif}
  begin
    {$ifndef PAS2JS}
    for LTurn := 1 to 4 do
    begin
      Application.ProcessMessages;
      Application.Idle(False);
      CheckSynchronize(0);
    end;
    {$endif}
  end;

  procedure ClickID(const AID: TNyxText);
  {$ifndef PAS2JS}
  var
    LButton: TButton;
  {$endif}
  begin
    {$ifdef PAS2JS}
    Check(not TJSHTMLButtonElement(LApplication.View.ElementFor(AID)).disabled,
      'the actual requested browser action is enabled');
    TJSHTMLElement(LApplication.View.ElementFor(AID)).click;
    {$else}
    LButton := TButton(LApplication.View.ControlFor(AID));
    Check(LButton.Enabled and LButton.Visible, 'the actual requested native action is enabled and visible');
    LButton.SetFocus;
    LButton.Click;
    {$endif}
    Settle;
  end;

  procedure Click(AAction: TNyxRecoveryAction);
  begin
    ClickID(LParts.ActionID(AAction));
  end;

  function PanelVisible: Boolean;
  begin
    {$ifdef PAS2JS}
    Result := TJSHTMLElement(LApplication.View.ElementFor(LParts.RootID)).offsetHeight > 0;
    {$else}
    Result := LApplication.View.ControlFor(LParts.RootID).Visible;
    {$endif}
  end;

  function ReviewedText: TNyxText;
  {$ifdef PAS2JS}
  var
    LInput: TJSHTMLTextAreaElement;
  {$else}
  var
    LInput: TControl;
  {$endif}
  begin
    { Framed inputs expose their outer face separately. Read the SAME public
      input port on both adapters, rather than casting a caption/frame as Memo. }
    {$ifdef PAS2JS}
    LInput := TJSHTMLTextAreaElement(LApplication.View.InputFor(LParts.RootID + '-review-draft'));

    if (LInput = nil) or (LowerCase(LInput.tagName) <> 'textarea') or not LInput.readOnly then
    begin
      raise ENyxCollection.Create('Review requires its real read-only browser memo');
    end;
    Result := LInput.value;
    {$else}
    LInput := LApplication.View.InputFor(LParts.RootID + '-review-draft');

    if not (LInput is TMemo) or not TMemo(LInput).ReadOnly then
    begin
      raise ENyxCollection.Create('Review requires its real read-only native memo');
    end;
    Result := TNyxText(TMemo(LInput).Text);
    {$endif}
  end;

  procedure SetDraftText(const AText: TNyxText);
  begin
    {$ifdef PAS2JS}
    LEditor.value := AText;
    LEditor.setSelectionRange(5, 7);
    {$else}
    RestoreNyxLCLInputText(LEditor, AText);
    SelectNyxLCLText(LEditor, NyxTextSelection(AText, 5, 6));
    {$endif}
  end;

  procedure BeginDraft(const AText: TNyxText);
  {$ifdef PAS2JS}
  var
    LSummary: TJSHTMLElement;
  {$endif}
  begin
    LView.Select(LGateway);
    Settle;
    {$ifdef PAS2JS}
    LSummary := TJSHTMLElement(LApplication.View.ElementFor(ProcessTreeID).querySelector(
      '[data-nyx-item="gateway"] > summary'));
    LSummary.focus;
    LSummary.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'F2'));
    LEditor := TJSHTMLInputElement(LSummary.querySelector('input'));
    {$else}
    LNode := LTree.Items.FindNodeWithText(String(LView.CellText(LGateway, 0)));
    Check(LNode <> nil, 'the actual gateway label is present');
    LNode.EditText;
    Settle;
    LEditor := NyxLCLTreeEditor(LTree);
    {$endif}
    Check(LEditor <> nil, 'the real Tree editor is available');
    SetDraftText(AText);
  end;

  procedure CheckPeer(const AName: TNyxText);
  {$ifdef PAS2JS}
  var
    LRow: TJSHTMLElement;
  {$endif}
  begin
    Check(LPeer.View.Snapshot.Item(LGateway).GetValue(NyxTextField('name')) = AName,
      'the independent peer receives the committed producer value');
    {$ifdef PAS2JS}
    LRow := TJSHTMLElement(LApplication.View.ElementFor(ProcessPeerID).querySelector(
      '[data-nyx-item="gateway"] > summary'));
    Check((LRow <> nil) and (Pos(AName, LRow.textContent) > 0),
      'the independent browser peer also renders the current label');
    {$else}
    Check(TTreeView(LApplication.View.ControlFor(ProcessPeerID)).Items.FindNodeWithText(String(AName)) <> nil,
      'the independent native peer also renders the current label');
    {$endif}
  end;

  procedure Incoming(const AName: TNyxText);
  begin
    LStore.Update(NyxCollectionItem(LGateway).WithValue(NyxTextField('name'), AName));
    Settle;
    CheckPeer(AName);
  end;

  procedure ConfirmDiscard;
  begin
    Click(nraDiscard);
    Check(LBinding.Capture.ConfirmingDiscard, 'first discard exposes a visible confirmation without consuming text');
    Click(nraDiscard);
  end;

begin
  {$ifdef NYX_TREE_RECOVERY_GENERATED}
  LDocument := nyx.generated.tree.recovery.BuildNyxDocument;
  {$else}
  LDocument := BuildProcessTreeDocument(True, True);
  {$endif}
  LApplication := nil;
  LDemo := nil;
  LSave := TSaveRecorder.Create;
  LSaveLease := LSave;
  LParts := NyxCollectionRecoveryParts(ProcessRecoveryName);
  LReview := LParts.Review;
  try
    LSaved := TNyxCodec.Encode(LDocument);
    {$ifndef PAS2JS}

    if ParamCount > 1 then
    begin
      Check(not FileExists(ParamStr(2)), 'the recovery companion source is freshly exported');
      LGenerated := TNyxCodegen.Generate(LDocument, 'nyx.generated.tree.recovery');
      LStream := TFileStream.Create(ParamStr(2), fmCreate);
      try
        LStream.WriteBuffer(Pointer(LGenerated)^, Length(LGenerated));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
    {$ifdef PAS2JS}
    LHost := TJSHTMLElement(document.createElement('section'));
    LHost.style.setProperty('width', IntToStr(AWidth) + 'px');
    document.body.appendChild(LHost);
    LApplication := TNyxBrowserApplication.Create;
    LApplication.Run(LDocument, LHost);
    {$else}
    LApplication := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LApplication.Window.ClientWidth := AWidth;
    LApplication.Window.ClientHeight := 844;
    LApplication.Window.Show;
    Settle;
    LTree := TTreeView(LApplication.View.ControlFor(ProcessTreeID));
    {$endif}
    LMount := LApplication.View.CollectionMount(ProcessTreeID);
    LPeer := LApplication.View.CollectionMount(ProcessPeerID);
    LView := LMount.View;
    LStore := LView.Store;
    LGateway := NyxItem(NyxCollection(ProcessCollectionName), 'gateway');
    LDemo := TNyxProcessTreeDemo.Create(LApplication.Context, LApplication.View.Events, LView);
    LBinding := BindNyxCollectionRecoveryPanel(LApplication.Context, LApplication.View.Events,
      LMount, LParts, LSaveLease);
    NyxTreeHierarchy(LPeer.View).ExpandAll;
    Settle;
    LJournal := NyxCollectionDraftRecovery(LMount);
    LBudget := NyxCollectionDraftRecoveryBudget(LJournal);
    Supports(LMount, INyxCollectionRecoveryNotifications, LNotifications);
    Check(not PanelVisible and (LJournal.Capture.Count = 0), 'an empty journal occupies no recovery panel space');
    BeginDraft(CDraft);
    {$ifdef PAS2JS}
    TJSHTMLElement(LApplication.View.ElementFor(ProcessTreeID).querySelector(
      '[data-nyx-item="archive"] > summary')).click;
    {$else}
    LTree.Selected := LTree.Items.FindNodeWithText('Archive worker');
    {$endif}
    Settle;
    ClickID(ProcessIncomingID);
    CheckPeer('Request gateway update 1');
    Check(PanelVisible and (LJournal.Capture.Count = 1) and
      (ReviewedText = CDraft) and (LBinding.Capture.ReviewKind = nrrJournal),
      'the actual replay button surfaces a displaced unfinished Tree label in the shared Card' +
      ' [visible=' + BoolToStr(PanelVisible, True) +
      '; count=' + IntToStr(LJournal.Capture.Count) +
      '; text=' + ReviewedText +
      '; text-length=' + IntToStr(Length(ReviewedText)) + '/' + IntToStr(Length(CDraft)) +
      '; kind=' + IntToStr(Ord(LBinding.Capture.ReviewKind)) +
      '; active=' + BoolToStr(LBinding.Capture.Active, True) +
      '; same-mount=' + BoolToStr(LApplication.View.CollectionMount(ProcessTreeID) = LMount, True) +
      '; source=' + LStore.Snapshot.Item(LGateway).GetValue(NyxTextField('name')) + ']');
    Check(Pos('changed', LApplication.Context.State.GetValue(LReview.Reason)) > 0,
      'review explains why newer source data displaced this label');
    LBefore := LStore.Snapshot;
    Click(nraExport);
    Check((LSave.Calls = 1) and SameBytes(LSave.Bytes, NyxEncodeUTF8(CDraft)) and
      (LStore.Snapshot = LBefore) and (LJournal.Capture.Count = 1) and not LBinding.Capture.Exporting,
      'inline export preserves exact Unicode bytes without consuming the entry or source');
    ConfirmDiscard;
    Check((LJournal.Capture.Count = 0) and not PanelVisible and (LStore.Snapshot = LBefore),
      'confirmed journal discard hides the empty panel and preserves producer data');

    BeginDraft(CDraft + ' saved');
    Incoming('First retained conflict');
    LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(1).PayloadBytes(8192));
    BeginDraft(CDraft + ' held');
    Incoming('Capacity conflict');
    Check(PanelVisible and LBinding.Capture.Status.Held and
      LApplication.Context.State.GetValue(LParts.Flag(nrfReviewHeld)),
      'capacity refusal is automatically visible after the completed physical refresh');
    Click(nraReviewHeld);
    Check((LBinding.Capture.ReviewKind = nrrHeld) and (ReviewedText = CDraft + ' held'),
      'Review held edit presents its exact receipt separately from admitted recovery entries' +
      ' [kind=' + TNyxText(IntToStr(Ord(LBinding.Capture.ReviewKind))) +
      '; held=' + TNyxText(BoolToStr(LBinding.Capture.Status.Held, True)) +
      '; reviewed=' + ReviewedText + '; source=' +
      LStore.Snapshot.Item(LGateway).GetValue(NyxTextField('name')) + ']');
    Click(nraExport);
    Check(SameBytes(LSave.Bytes, NyxEncodeUTF8(CDraft + ' held')) and
      (LJournal.Capture.Count = 1) and NyxCollectionDraftRecoveryHeld(LMount),
      'exporting the held receipt does not free or consume either kind of unfinished work');
    Click(nraReviewList);
    ConfirmDiscard;
    Check((LJournal.Capture.Count = 0) and NyxCollectionDraftRecoveryHeld(LMount),
      'freeing journal capacity does not silently claim that the physical display refreshed');
    LBefore := LStore.Snapshot;
    Click(nraRetry);
    Check(not NyxCollectionDraftRecoveryHeld(LMount) and (LJournal.Capture.Count = 1) and
      (ReviewedText = CDraft + ' held') and (LStore.Snapshot = LBefore),
      'Retry admits the original held text before catching up, without overwriting the source');
    ConfirmDiscard;

    LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(0).PayloadBytes(0));
    BeginDraft(CDraft + ' stale');
    {$ifndef PAS2JS}
    SendMessage(LEditor.Handle, WM_IME_STARTCOMPOSITION, 0, 0);
    {$endif}
    Incoming('Latest source');
    {$ifndef PAS2JS}
    Check(not PanelVisible and not LBinding.Capture.Status.Held,
      'composition keeps its physical editor before a delayed capacity decision');
    SendMessage(LEditor.Handle, WM_IME_ENDCOMPOSITION, 0, 0);
    Settle;
    Check(PanelVisible and LBinding.Capture.Status.Held,
      'post-composition capacity refusal automatically reaches the panel without caller polling');
    {$endif}
    Click(nraReviewHeld);
    Click(nraDiscard);
    Check(LBinding.Capture.ConfirmingDiscard, 'pending discard requires its own visible confirmation');
    SetDraftText(CDraft + ' newer');
    Click(nraDiscard);
    Check(NyxCollectionDraftRecoveryHeld(LMount) and (LJournal.Capture.Count = 0) and
      (Pos('changed', LApplication.Context.State.GetValue(LParts.Text(nrtOutcome))) > 0),
      'a stale reviewed receipt cannot discard newer native or browser typing');
    Click(nraReviewHeld);
    LOldCalls := LSave.Calls;
    LBinding.ConfigureExport(NyxFileSaveStreamOptions(NyxFileSaveOptions('review.txt')).MaximumBytes(1));
    Click(nraExport);
    Check((LSave.Calls = LOldCalls) and not LBinding.Capture.Exporting and
      NyxCollectionDraftRecoveryHeld(LMount), 'caller export limits refuse before host effects and keep the held draft');
    LBinding.ConfigureExport(NyxFileSaveStreamOptions(NyxFileSaveOptions('review.txt')).MaximumBytes(8192));
    Click(nraExport);
    Check(SameBytes(LSave.Bytes, NyxEncodeUTF8(CDraft + ' newer')),
      'fresh review exports the newer exact bytes after a refused stale confirmation');
    LBefore := LStore.Snapshot;
    ConfirmDiscard;
    Check(not NyxCollectionDraftRecoveryHeld(LMount) and (LJournal.Capture.Count = 0) and
      not PanelVisible and (LStore.Snapshot = LBefore),
      'confirmed current pending discard explicitly refreshes the same mount without source changes');

    LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(2).PayloadBytes(8192));
    BeginDraft(CDraft + ' retirement');
    Incoming('Final producer');
    LSave.Hold := True;
    Click(nraExport);
    Check(LBinding.Capture.Exporting and (LSave.Pending <> nil), 'a pending save retains its cancellable receiver lease');
    LBinding.Close;
    Check(LSave.Pending.Cancelled and not LSave.Pending.Finish and not LBinding.Capture.Active and
      (LBinding.Capture.EntryIndex = -1),
      'Close cancels pending export before a simulated late result can touch its app');
    for LAction := Low(TNyxRecoveryAction) to High(TNyxRecoveryAction) do
    begin
      Check(LApplication.View.Events.On(NyxControlEvents(LParts.ActionID(LAction)), ntClick).Count = 0,
        'Close removes the original recovery action registration');
    end;
    Check(TNyxCodec.Encode(LDocument) = LSaved, 'all review/export/discard/retry activity leaves saved defaults exact');
    LBinding := nil;
    LDemo.Free;
    LDemo := nil;
    LApplication.Free;
    LApplication := nil;
    Check(not LNotifications.CaptureRecoveryStatus.Active and not LJournal.Capture.Active and
      (LJournal.EntryAt(0).Cell.Text = CDraft + ' retirement'),
      'retirement revokes the mount while admitted review data stays independently readable');
  finally
    LBinding := nil;
    LDemo.Free;
    LApplication.Free;
    LDocument.Free;
    {$ifdef PAS2JS}LHost.remove;{$endif}
  end;
end;

begin
  try
    {$ifdef PAS2JS}
    Journey(1280);
    Journey(390);
    document.body.setAttribute('data-tree-recovery-checks', IntToStr(GChecks));
    {$else}
    Application.Initialize;
    Application.CaptureExceptions := False;
    Journey(StrToIntDef(ParamStr(1), 1280));
    WriteLn('PASS ', GChecks, ' Tree recovery application checks');
    {$endif}
  except
    on LError: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-tree-recovery-error', LError.Message);
      {$else}
      WriteLn('FAIL after ', GChecks, ' checks: ', LError.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
