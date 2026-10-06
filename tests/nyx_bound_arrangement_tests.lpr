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
program nyx_bound_arrangement_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Classes, Forms, Controls, StdCtrls,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.state, nyx.binding.types, nyx.behavior, nyx.projection.refresh
  {$ifdef NYX_ARRANGEMENT_MCP}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, nyx.render.lcl{$endif};

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  TFace = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  TFace = TControl;
  TControlAccess = class(TControl);
  {$endif}

  { Receivers borrow the view/candidate only within Run. The explicit token is
    disconnected before those objects leave scope. Reentry is attempted after
    ordinary renderer notification returns, so the store's busy guard matters. }
  TObservation = class
  public
    Renderer: TRenderer;
    Candidate: TNyxDocument;
    Clicks: Integer;
    Edits: Integer;
    Notifications: Integer;
    ReentryChecks: Integer;
    AttemptReentry: Boolean;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure StateChanged(AState: TNyxState; AChanges: TNyxStateChanges);
    procedure ValidateState(ACandidate: TNyxState; AChanges: TNyxStateChanges);
  end;

var
  GChecks: Integer;

const
  { Match the public Double contract. Untyped native decimal constants may be
    evaluated as Extended, so comparing them directly would test excess compiler
    precision rather than exact storage of the authored Double. }
  CInitialAmount: Double = 0.1;
  COtherAmount: Double = 0.2;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise ENyxModel.Create('Bound arrangement: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TObservation.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (ANode.ID = 'action') and (AEvent.Trigger = ntClick) then
  begin
    Inc(Clicks);
  end;

  if (ANode.ID = 'entry') and (AEvent.Trigger = ntChange) then
  begin
    Inc(Edits);
  end;
end;

procedure TObservation.ValidateState(ACandidate: TNyxState; AChanges: TNyxStateChanges);
begin

  if AttemptReentry then
  begin
    Check(Renderer.State.Busy, 'Store is busy while candidate validators run');
    Check(not Renderer.TryRefresh(Candidate, Candidate.Pages[0], False),
      'Validation cannot rearrange the view used by its current command');
    Inc(ReentryChecks);
  end;
end;

procedure TObservation.StateChanged(AState: TNyxState; AChanges: TNyxStateChanges);
begin
  Inc(Notifications);

  if AttemptReentry then
  begin
    Check(AState.Busy, 'Store stays busy throughout committed notifications');
    Check(not Renderer.TryRefresh(Candidate, Candidate.Pages[0], False),
      'Notification cannot publish a competing view after renderer Sync returns');
    Inc(ReentryChecks);
  end;
end;

function TextOf(AFace: TFace): TNyxText;
begin
  {$ifdef PAS2JS}Result := TJSHTMLInputElement(AFace).value;
  {$else}Result := TCustomEdit(AFace).Text;{$endif}
end;

function CaptionOf(AFace: TFace): TNyxText;
begin
  {$ifdef PAS2JS}Result := AFace.textContent;
  {$else}Result := TLabel(AFace).Caption;{$endif}
end;

function FaceOf(ARenderer: TRenderer; const AID: TNyxText): TFace;
begin
  {$ifdef PAS2JS}Result := ARenderer.ElementFor(AID);
  {$else}Result := ARenderer.ControlFor(AID);{$endif}
end;

procedure EditText(AFace: TFace; const AValue: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(AFace).value := AValue;
  AFace.dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(AFace).Text := AValue;
  {$endif}
end;

procedure CommitNumber(AFace: TFace; const AValue: TNyxText);
begin
  EditText(AFace, AValue);
  {$ifdef PAS2JS}AFace.dispatchEvent(TJSEvent.new('change'));
  {$else}TEdit(AFace).OnEditingDone(AFace);{$endif}
end;

procedure Click(AFace: TFace);
begin
  {$ifdef PAS2JS}AFace.click;
  {$else}TControlAccess(AFace).Click;{$endif}
end;

procedure FocusAt(AFace: TFace; APosition: Integer);
begin
  {$ifdef PAS2JS}
  AFace.focus;
  TJSHTMLInputElement(AFace).selectionStart := APosition;
  TJSHTMLInputElement(AFace).selectionEnd := APosition;
  {$else}
  TWinControl(AFace).SetFocus;
  TCustomEdit(AFace).SelStart := APosition;
  TCustomEdit(AFace).SelLength := 0;
  {$endif}
end;

function Caret(AFace: TFace): Integer;
begin
  {$ifdef PAS2JS}Result := TJSHTMLInputElement(AFace).selectionStart;
  {$else}Result := TCustomEdit(AFace).SelStart;{$endif}
end;

function WidthOf(AFace: TFace): Integer;
begin
  {$ifdef PAS2JS}Result := Round(AFace.getBoundingClientRect.width);
  {$else}Result := AFace.Width;{$endif}
end;

function Visible(AFace: TFace): Boolean;
begin
  {$ifdef PAS2JS}Result := AFace.getBoundingClientRect.height > 0;
  {$else}Result := AFace.Visible;{$endif}
end;

function NewReview: TNyxDocument;
begin
  {$ifdef NYX_ARRANGEMENT_MCP}
  Result := BuildNyxDocument;
  {$else}
  Result := TNyxDocument.Create;
  Result.Title := 'A view that keeps your work';
  Result.AddPage(NewNyxPage('review'));
  Result.Pages[0].Configure.Layout(nlColumn).Gap(12).Done;
  Result.Pages[0].Add(NewNyxLabel('caption').WithText('Original caption'));
  Result.Pages[0].Add(NewNyxRow('workspace'));
  Result.Find('workspace').Configure.Gap(12).Done;
  Result.Find('workspace').Add(NewNyxColumn('left-room'));
  Result.Find('workspace').Add(NewNyxColumn('right-room'));
  Result.Find('left-room').Configure.Width(300).Gap(12).Done;
  Result.Find('right-room').Configure.Width(300).Gap(12).Done;
  Result.Find('left-room').Add(NewNyxInput('entry').WithText('Your notes'));
  Result.Find('entry').Configure.Value('Original default').Done;
  Result.Find('right-room').Add(NewNyxInput('other-entry').WithText('Independent notes'));
  Result.Find('other-entry').Configure.Value('Other default').Done;
  Result.Find('left-room').Add(NewNyxButton('action').WithText('Continue'));
  Result.Find('left-room').Add(NewNyxInput('amount').WithText('Amount'));
  Result.Find('amount').Configure.InputType(niNumber).Done;
  Result.Find('right-room').Add(NewNyxInput('other-amount').WithText('Independent amount'));
  Result.Find('other-amount').Configure.InputType(niNumber).Done;
  {$endif}
  { General semantic state/binding authoring is still an open MCP workflow.
    These explicit public typed attachments qualify the library consumers;
    they must never be represented as an agent-authored binding transaction. }
  Result.State.SetValue(NyxTextState('notes'), 'Authored notes');
  Result.State.SetValue(NyxTextState('other-notes'), 'Other authored notes');
  Result.State.SetValue(NyxNumberState('amount'), CInitialAmount);
  Result.State.SetValue(NyxNumberState('other-amount'), COtherAmount);
  Result.State.SetValue(NyxBooleanState('caption-visible'), True);
  Result.State.SetValue(NyxIntegerState('room-width'), 300);
  Result.Find('entry').Binds.Value(NyxTextState('notes')).Done;
  Result.Find('caption').Binds.Text(NyxTextState('notes'))
    .Visible(NyxBooleanState('caption-visible')).Done;
  Result.Find('other-entry').Binds.Value(NyxTextState('other-notes')).Done;
  Result.Find('amount').Binds.Value(NyxNumberState('amount')).Done;
  Result.Find('other-amount').Binds.Value(NyxNumberState('other-amount')).Done;
  Result.Find('left-room').Binds.Width(NyxIntegerState('room-width')).Done;
end;

procedure MoveParts(ADocument: TNyxDocument);
var
  LInput: TNyxNode;

  procedure Move(ANode: TNyxNode; APosition: Integer);
  var
    LParent: TNyxNode;
    LIndex: Integer;
  begin
    LParent := ANode.Parent;
    for LIndex := 0 to LParent.Count - 1 do
    begin

      if LParent.Children[LIndex] = ANode then
      begin
        ADocument.Find('right-room').Insert(APosition, LParent.Extract(LIndex));
        Exit;
      end;
    end;
    raise ENyxModel.Create('The review part must have its exact owned parent');
  end;
begin
  LInput := ADocument.Find('entry');
  Move(LInput, 0);
  LInput := ADocument.Find('amount');
  Move(LInput, 1);
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LRenderer: TRenderer;
  LOtherRenderer: TRenderer;
  LStore: TNyxState;
  LOtherStore: TNyxState;
  LObserver: TObservation;
  LToken: TNyxStateSubscription;
  LRoot: TNyxNode;
  LEntryNode: TNyxNode;
  LInput: TFace;
  LAmount: TFace;
  LOtherAmount: TFace;
  LCaption: TFace;
  LButton: TFace;
  LRestores: TNyxProjectionValueRestores;
  LRevision: Integer;
  LNotifications: Integer;
  LEdits: Integer;
  LClicks: Integer;
  LAmountDraft: TNyxText;
  LOtherAmountDraft: TNyxText;
  LRepeat: Integer;
  LRefused: Boolean;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LOtherHost: TJSHTMLElement;
  {$else}
  LHost: TForm;
  LOtherHost: TForm;
  {$endif}
begin
  LDocument := NewReview;
  LCandidate := nil;
  LRenderer := TRenderer.Create;
  LOtherRenderer := TRenderer.Create;
  LStore := LDocument.State.Clone;
  LOtherStore := LDocument.State.Clone;
  LObserver := TObservation.Create;
  LToken := nil;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  LOtherHost := TJSHTMLElement(document.createElement('div'));
  LHost.style.setProperty('width', '720px');

  if window.location.search = '?compact' then
  begin
    LHost.style.setProperty('width', '390px');
  end;
  LHost.style.setProperty('height', '540px');
  LOtherHost.style.setProperty('width', '720px');
  document.body.appendChild(LHost);
  document.body.appendChild(LOtherHost);
  {$else}
  Application.Initialize;
  LHost := TForm.CreateNew(nil);
  LOtherHost := TForm.CreateNew(nil);
  LHost.ClientWidth := 720;
  LHost.ClientHeight := 540;
  LOtherHost.ClientWidth := 720;
  LOtherHost.ClientHeight := 540;
  LHost.Show;
  {$endif}
  try
    LRenderer.OnEvent := LObserver.Event;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost, False, LStore);
    LOtherRenderer.Render(LDocument, LDocument.Pages[0], LOtherHost, False, LOtherStore);
    LObserver.Renderer := LRenderer;
    LToken := LStore.Subscribe(LObserver.StateChanged, LObserver.ValidateState);
    LRoot := LRenderer.Root;
    LEntryNode := LRoot.Find('entry');
    LInput := LRenderer.InputFor('entry');
    LAmount := LRenderer.InputFor('amount');
    LOtherAmount := LRenderer.InputFor('other-amount');
    LCaption := FaceOf(LRenderer, 'caption');
    LButton := FaceOf(LRenderer, 'action');
    Check(not CanRefreshNyxProjection(LRoot, LRoot) and
      not CanArrangeNyxProjection(LRoot, LRoot), 'Strict unbound APIs retain their original meaning');
    Check((LRenderer.State = LStore) and (LOtherRenderer.State = LOtherStore),
      'Views borrow their own independent runtime store');
    EditText(LInput, 'Live notes / 🌙');
    Check((LStore.GetValue(NyxTextState('notes')) = TNyxText('Live notes / 🌙')) and
      (CaptionOf(LCaption) = TNyxText('Live notes / 🌙')) and (LObserver.Edits = 1),
      'Real text input publishes exact supplementary state and one callback');
    Check(TextOf(LOtherRenderer.InputFor('entry')) = 'Authored notes',
      'The other application remains independent');
    CommitNumber(LAmount, '0.375');
    Check(LStore.GetValue(NyxNumberState('amount')) = 0.375,
      'Real number completion publishes a typed accepted value');
    EditText(LAmount, '0.');
    EditText(LOtherAmount, '0.');
    { HTML number inputs sanitize trailing decimal text; the browser draft may
      be empty while native retains 0. literally. Capture each target's actual
      physical draft and compare it throughout, rather than inventing parity. }
    LAmountDraft := TextOf(LAmount);
    LOtherAmountDraft := TextOf(LOtherAmount);
    Check((LAmountDraft <> '0.375') and (LOtherAmountDraft <> '0.2') and
      (LStore.GetValue(NyxNumberState('amount')) = 0.375) and
      (LStore.GetValue(NyxNumberState('other-amount')) = COtherAmount),
      'Unfinished physical number drafts stay independent of accepted state: drafts ' +
      LAmountDraft + ' / ' + LOtherAmountDraft + ', state ' +
      LStore.Value('amount').NumberText + ' / ' + LStore.Value('other-amount').NumberText);
    LCandidate := LDocument.Clone;
    MoveParts(LCandidate);
    LCandidate.Find('entry').Configure.Text('Notes in another room').Done;
    LObserver.Candidate := LCandidate;
    for LRepeat := 1 to 4 do
    begin
      FocusAt(LInput, 4);
      LRevision := LStore.Revision;
      LNotifications := LObserver.Notifications;
      LEdits := LObserver.Edits;
      Check(LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False),
        'Exact bound contracts admit a detached rearrangement');
      Check((LRenderer.Root = LRoot) and (LRoot.Find('entry') = LEntryNode) and
        (LRoot.Find('entry').Parent.ID = 'right-room') and (LRenderer.InputFor('entry') = LInput),
        'Bound logical nodes and actual input faces stay retained');
      Check((TextOf(LInput) = TNyxText('Live notes / 🌙')) and (Caret(LInput) = 4) and
        (CaptionOf(LCaption) = TNyxText('Live notes / 🌙')),
        'Current store values and physical caret survive moved ownership');
      {$ifdef PAS2JS}
      Check(document.activeElement = LInput, 'Browser keeps the bound input focused');
      {$else}
      Check(LHost.ActiveControl = LInput, 'Native keeps the bound input focused');
      {$endif}
      Check((TextOf(LAmount) = LAmountDraft) and (TextOf(LOtherAmount) = LOtherAmountDraft),
        'Each target retains its actual unfinished number drafts through reparent');
      Check((LRenderer.InputFor('amount') = LAmount) and
        (LRenderer.State = LStore) and (LStore.Revision = LRevision) and
        (LObserver.Notifications = LNotifications) and (LObserver.Edits = LEdits),
        'Rearrangement replaces neither store nor input and publishes no state/edit event');
      LClicks := LObserver.Clicks;
      Click(LButton);
      Check(LObserver.Clicks = LClicks + 1, 'The retained button callback is registered exactly once');
      Check(LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False) and
        (LRoot.Find('entry').Parent.ID = 'left-room') and
        (TextOf(LInput) = TNyxText('Live notes / 🌙')), 'Reversal keeps live bound state');
    end;
    LStore.SetValue(NyxTextState('notes'), 'Externally updated / 🌙');
    Check((TextOf(LInput) = TNyxText('Externally updated / 🌙')) and
      (CaptionOf(LCaption) = TNyxText('Externally updated / 🌙')),
      'The original subscription still synchronizes external writes after repeated moves');
    LStore.SetValue(NyxIntegerState('room-width'), 280);
    LStore.SetValue(NyxBooleanState('caption-visible'), False);
    Check(LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False) and
      (LRoot.Find('left-room').StoredProp(NyxAttributeName(atWidth)) = '280') and
      (LRoot.Find('caption').StoredProp(NyxAttributeName(atVisible)) = 'false'),
      'Numeric layout and Boolean bindings project current runtime values');
    Check((WidthOf(FaceOf(LRenderer, 'left-room')) = 280) and not Visible(LCaption),
      'Actual retained target controls apply bound width and visibility after rearrangement');
    LStore.SetValue(NyxBooleanState('caption-visible'), True);
    SetLength(LRestores, 2);
    LRestores[0] := TNyxProjectionValueRestore.ForField('amount', 'amount');
    LRestores[1] := TNyxProjectionValueRestore.ForField('missing', 'amount');
    LRevision := LStore.Revision;
    Check(not LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False, LRestores) and
      (LRoot.Find('entry').Parent.ID = 'right-room') and (LStore.Revision = LRevision),
      'One stale restore refuses the entire group before ownership or state changes');
    SetLength(LRestores, 1);
    Check(LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False, LRestores) and
      (TextOf(LAmount) = '0.375') and (LStore.Revision = LRevision),
      'Explicit numeric restoration uses current accepted state with no store write');
    Check(TextOf(LOtherAmount) = LOtherAmountDraft,
      'Selective restoration leaves the other physical number draft untouched');
    Check(LDocument.State.GetValue(NyxNumberState('amount')) = CInitialAmount,
      'Runtime edits and restores never rewrite authored state defaults');

    LObserver.AttemptReentry := True;
    LStore.SetValue(NyxTextState('notes'), 'After busy refusal');
    LObserver.AttemptReentry := False;
    Check((LObserver.ReentryChecks = 2) and (LRoot.Find('entry').Parent.ID = 'left-room') and
      (TextOf(LInput) = 'After busy refusal'), 'Both reentry refusals retain the admitted view');
    Check(LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False),
      'Refresh becomes available after notification completes');
    FreeAndNil(LCandidate);
    LCandidate := LDocument.Clone;
    MoveParts(LCandidate);
    LCandidate.Find('entry').Binds.Value(NyxTextState('other-notes')).Done;
    Check(not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False) and
      (TextOf(LInput) = 'After busy refusal'), 'Retargeting a binding cannot reuse the original coordinator');
    FreeAndNil(LCandidate);
    LCandidate := LDocument.Clone;
    LCandidate.Find('entry').SetBinding(TNyxBindingSpec.Bound(bpValue, 'notes', nskText, bdFromState));
    Check(not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False),
      'A direction change requires a fresh binding lifetime');
    FreeAndNil(LCandidate);
    LCandidate := LDocument.Clone;
    LCandidate.Find('entry').Binds.Clear(bpValue).Done;
    Check(not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False),
      'Explicit clearing cannot discard a retained two-way subscription');
    FreeAndNil(LCandidate);
    LCandidate := LDocument.Clone;
    LCandidate.Find('entry').Configure.InputType(niPassword).Done;
    Check(not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False),
      'Constructor differences still refuse through the bound path');
    FreeAndNil(LCandidate);
    LCandidate := LDocument.Clone;
    LCandidate.Find('entry').Configure.Width(100).Done;
    LCandidate.Find('entry').Binds.Width(NyxIntegerState('room-width')).Done;
    Check(not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False),
      'Adding a descriptor cannot silently extend an existing validator');
    FreeAndNil(LCandidate);
    LCandidate := LDocument.Clone;
    LCandidate.Find('amount').SetBinding(TNyxBindingSpec.Bound(bpValue, 'notes', nskText, bdTwoWay));
    LRefused := False;
    try
      LRefused := not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRenderer.Root = LRoot) and
      (LStore.GetValue(NyxNumberState('amount')) = 0.375),
      'Incompatible binding kinds refuse before publishing any view or state');
    LObserver.Candidate := nil;
    LRenderer.Unmount;
    LStore.SetValue(NyxTextState('notes'), 'After view teardown');
    Check((LObserver.Notifications > 0) and LToken.Connected,
      'Independent observers remain connected after the view disconnects its own token');
    Check(TextOf(LOtherRenderer.InputFor('entry')) = 'Authored notes',
      'Teardown and first-store writes retain the other live application');
    LOtherStore.SetValue(NyxTextState('notes'), 'Other application update');
    Check(TextOf(LOtherRenderer.InputFor('entry')) = 'Other application update',
      'The independent second subscription continues working');
    FreeAndNil(LStore);
    Check(not LToken.Connected, 'Store destruction retires retained external tokens safely');
    WriteLn('PASS ', GChecks, ' actual bound arrangement checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-projection-refresh', 'passed');
    document.body.setAttribute('data-projection-refresh-checks', IntToStr(GChecks));
    {$endif}
  finally
    LToken.Free;
    LRenderer.Free;
    LOtherRenderer.Free;
    LObserver.Free;
    LCandidate.Free;
    LDocument.Free;
    LStore.Free;
    LOtherStore.Free;
    {$ifdef PAS2JS}
    LHost.remove;
    LOtherHost.remove;
    {$else}
    LHost.Free;
    LOtherHost.Free;
    {$endif}
  end;
end;

begin
  try
    Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-refresh', 'failed');
      document.body.setAttribute('data-projection-refresh-error', LException.Message);
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
