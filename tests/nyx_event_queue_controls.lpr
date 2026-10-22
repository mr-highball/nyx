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

program nyx_event_queue_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Types, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
  IntfGraphics, FPWritePNG, nyx.text, nyx.types, nyx.data, nyx.model, nyx.callbacks,
  nyx.scheduler, nyx.schema, nyx.studio.projects, nyx.studio.session,
  nyx.studio.inspector, nyx.studio.sourcejobs, nyx.studio.lcl, nyx.render.lcl,
  nyx.event.discovery,
  nyx.event.payload, nyx.studio.callbackedits,
  nyx.test.event.queue;

type
  TControlAccess = class(TControl);
  { Real LCL exception dispatch must fail the journey, including worker retirement.
    This observer owns neither the editor nor any of its presentation controls. }
  TFailureObserver = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  GStudio: TNyxNativeStudio;
  GForm: TForm;
  GObserver: TFailureObserver;
  GChecks: Integer;

procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.Message;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition or (GObserver.Error <> '') then
  begin
    raise ENyxModel.Create('Event controls: ' + AReason + ' / ' + GObserver.Error);
  end;
  Inc(GChecks);
end;

procedure Pump;
begin
  CheckSynchronize;
  Application.ProcessMessages;
end;

{ Observe the existing wait without moving its deadline or servicing extra work.
  Pump duration includes all synchronized/UI work, not exclusive paint time.
  Failure facts are copied once; realizing a projection here would add work. }
procedure Ready(const AContext: TNyxText);
var
  LStart: QWord;
  LPumpStart, LLastPump, LMaxPump, LIterations, LElapsed: QWord;
  LStartPaint, LEndPaint, LClientWidth: Integer;
  LSourceBusy, LPresentationPending: Boolean;
  LState: TNyxSourceCommandState;
  LSelection, LStatus, LObserverError: TNyxText;
begin
  LStartPaint := GStudio.PaintCount;
  LMaxPump := 0;
  LIterations := 0;
  LStart := GetTickCount64;
  repeat
    LPumpStart := GetTickCount64;
    Pump;
    LLastPump := GetTickCount64 - LPumpStart;
    Inc(LIterations);

    if LLastPump > LMaxPump then
    begin
      LMaxPump := LLastPump;
    end;

    if GetTickCount64 - LStart > 30000 then
    begin
      LElapsed := GetTickCount64 - LStart;
      LSourceBusy := GStudio.SourceBusy;
      LPresentationPending := GStudio.PresentationPending;
      LState := GStudio.SourceCommands.State;
      LSelection := GStudio.Session.SelectedID;
      LEndPaint := GStudio.PaintCount;
      LClientWidth := GForm.ClientWidth;
      LStatus := GStudio.Status;
      LObserverError := GObserver.Error;
      raise ENyxModel.Create('Event command did not retire / ' + LStatus +
        ' / context=' + AContext + ' / checks=' + IntToStr(GChecks) +
        ' / elapsed-ms=' + IntToStr(LElapsed) +
        ' / pumps=' + IntToStr(LIterations) +
        ' / last-pump-ms=' + IntToStr(LLastPump) +
        ' / max-pump-ms=' + IntToStr(LMaxPump) +
        ' / paints=' + IntToStr(Int64(LEndPaint) - Int64(LStartPaint)) +
        ' / source-busy=' + BoolToStr(LSourceBusy, True) +
        ' / presentation-pending=' + BoolToStr(LPresentationPending, True) +
        ' / state=' + IntToStr(Ord(LState)) + ' / selected=' + LSelection +
        ' / width=' + IntToStr(LClientWidth) + ' / observer=' + LObserverError);
    end;
    Sleep(1);
  until not GStudio.SourceBusy and not GStudio.PresentationPending;
  Pump;
end;

procedure Click(const AID: TNyxText; AWait: Boolean = True);
var
  LControl: TControl;
begin
  if GStudio.ShellView.Root.Find(AID) <> nil then
  begin
    LControl := GStudio.ShellView.ControlFor(AID);
  end
  else
  begin
    LControl := GStudio.SourceView.ControlFor(AID);
  end;
  Check(LControl <> nil, 'Actual command exists / ' + AID);
  TControlAccess(LControl).Click;

  if AWait then
  begin
    Ready('Click/' + AID);
  end;
end;

procedure Text(const AID, AValue: TNyxText);
var
  LInput: TControl;
begin
  { Standalone Studio owns the source editor in its independent CodeView.
    Shell inspector fields belong to ShellView; never assume one mounted root. }

  if AID = 'studio-code' then
  begin
    LInput := GStudio.CodeView.InputFor(AID);
  end
  else
  begin
    LInput := GStudio.ShellView.InputFor(AID);
  end;
  Check(LInput is TCustomEdit, 'Actual text editor exists / ' + AID);
  TCustomEdit(LInput).Text := AValue;
end;

procedure Choice(const AID, AValue: TNyxText);
var
  LCombo: TComboBox;
  LIndex: Integer;
begin
  LCombo := TComboBox(GStudio.ShellView.InputFor(AID));
  Check(LCombo <> nil, 'Actual choice editor exists / ' + AID);
  LIndex := LCombo.Items.IndexOf(AValue);
  Check(LIndex >= 0, 'Closed choice exists / ' + AValue);

  if LCombo.CanFocus then
  begin
    LCombo.SetFocus;
  end;
  LCombo.ItemIndex := LIndex;
  LCombo.OnChange(LCombo);
end;

function PairText: TNyxText;
begin
  Result := EncodeNyxProject(GStudio.ProjectSnapshot);
end;

procedure Select(const AID: TNyxText);
begin
  GStudio.Session.Select(AID);
  GStudio.RequestRefresh;
  Ready('Select/' + AID);
end;

procedure Capture(const AName: TNyxText);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(GForm.ClientWidth, GForm.ClientHeight);
    GForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

{ Inspect a copied authored contract, never retain a realized node across paint. }
function EventInfo(ATrigger: TNyxTrigger; const AName: TNyxEventRef): TNyxAuthoredEventInfo;
var
  LProjection: TNyxNode;
  LEvents: TNyxAuthoredEventInfos;
  LIndex: Integer;
begin
  Result := Default(TNyxAuthoredEventInfo);
  LProjection := GStudio.Session.SelectedProjection;
  try
    LEvents := NyxAuthoredEvents(LProjection);
    for LIndex := 0 to High(LEvents) do
    begin

      if (LEvents[LIndex].Trigger = ATrigger) and (LEvents[LIndex].Name.Name = AName.Name) then
      begin
        Exit(LEvents[LIndex]);
      end;
    end;
  finally
    LProjection.Free;
  end;
end;

function EventKey(ATrigger: TNyxTrigger; const AName: TNyxEventRef): TNyxText;
var
  LMetadata: TNyxEventSchemas;
  LIndex: Integer;
begin
  Result := 'event-' + NyxTriggerName(ATrigger);

  if ATrigger <> ntNamed then
  begin
    Exit;
  end;
  LMetadata := NyxEventsMetadata(GStudio.Session.Selected, GStudio.Session.Document);
  for LIndex := 0 to High(LMetadata) do
  begin

    if (LMetadata[LIndex].Trigger = ATrigger) and (LMetadata[LIndex].Name.Name = AName.Name) then
    begin
      Exit(Result + '-' + IntToStr(LIndex));
    end;
  end;
  raise ENyxModel.Create('Named event metadata is absent');
end;

{ BEGIN MISSING_POLICY_OBSERVATION }
procedure ObserveMissingPolicy(const AKey, AName: TNyxText; ALoop: Integer);
const
  CMaxNodes = 4096;
  CMaxDepth = 64;
  CMaxEntries = 8; { Eight semantic matches plus eight named policy candidates. }
  CMaxSectionText = 3072;
  CMaxOutput = 8192;
  CMaxScalars = 32;
var
  LRoot: TNyxNode;
  LOwner, LView, LRootID, LMatches, LCandidates, LText, LFooter: TNyxText;
  LLoop, LWidth, LChecks, LVisited, LMatchCount, LCandidateCount: Integer;
  LMatchEntries, LCandidateEntries: Integer;
  LFilter, LCard, LPolicy, LIncomplete, LTruncated: Boolean;

  function Safe(const AValue: TNyxText): TNyxText;
  var
    LIndex, LScalar, LCount: Integer;
  begin
    Result := '"';
    LIndex := 1;
    LCount := 0;
    while (LIndex <= Length(AValue)) and (LCount < CMaxScalars) do
    begin

      if not NyxNextScalar(AValue, LIndex, LScalar) then
      begin
        Exit(Result + '<invalid-text>"');
      end;
      Inc(LCount);

      if (LScalar >= 32) and (LScalar <= 126) and
        (LScalar <> 34) and (LScalar <> 92) then
      begin
        Result := Result + NyxScalarText(LScalar);
      end
      else
      begin
        Result := Result + '\u{' + IntToHex(LScalar, 6) + '}';
      end;
    end;

    if LIndex <= Length(AValue) then
    begin
      Result := Result + '<scalar-truncated>';
    end;
    Result := Result + '"';
  end;

  procedure AppendEntry(var AText: TNyxText; var AEntries: Integer;
    const AEntry: TNyxText);
  begin

    if (AEntries >= CMaxEntries) or
      (Length(AText) + Length(AEntry) > CMaxSectionText) then
    begin
      LTruncated := True;
      Exit;
    end;
    AText := AText + AEntry;
    Inc(AEntries);
  end;

  procedure Visit(ANode: TNyxNode; ADepth: Integer);
  var
    LIndex: Integer;
    LCommand, LEventOwner, LTrigger, LName, LEntry: TNyxText;
    LMatch, LCandidate: Boolean;
  begin

    if (LVisited >= CMaxNodes) or (ADepth > CMaxDepth) then
    begin
      LIncomplete := True;
      Exit;
    end;
    Inc(LVisited);
    LFilter := LFilter or (ANode.ID = NyxStudioEventFilterID);
    LCard := LCard or (ANode.ID = AKey);
    LPolicy := LPolicy or (ANode.ID = AKey + '-policy');
    LCommand := ANode.Prop(NyxStudioEventCommandKey);
    LEventOwner := ANode.Prop(NyxStudioEventOwnerKey);
    LTrigger := ANode.Prop(NyxStudioEventTriggerKey);
    LName := ANode.Prop(NyxStudioEventNameKey);
    LMatch := (LCommand <> '') and (LEventOwner = LOwner) and
      (LTrigger = NyxTriggerName(ntNamed)) and (LName = AName);
    { Select kind plus exported event identity keys identifies policy candidates
      without importing the Inspector's private command enum or ordinal. }
    LCandidate := (ANode.Kind = NyxKindName(nkSelect)) and (LCommand <> '') and
      (LTrigger = NyxTriggerName(ntNamed));

    if LMatch or LCandidate then
    begin
      LEntry := ' [id=' + Safe(ANode.ID) + ';source=' + Safe(ANode.SourceID) +
        ';command=' + Safe(LCommand) + ';owner=' + Safe(LEventOwner) +
        ';trigger=' + Safe(LTrigger) + ';name=' + Safe(LName) + ']';

      if LMatch then
      begin
        Inc(LMatchCount);
        AppendEntry(LMatches, LMatchEntries, LEntry);
      end;

      if LCandidate then
      begin
        Inc(LCandidateCount);
        AppendEntry(LCandidates, LCandidateEntries, LEntry);
      end;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin

      if (LVisited >= CMaxNodes) or (ADepth >= CMaxDepth) then
      begin
        LIncomplete := True;
        Break;
      end;
      Visit(ANode.Children[LIndex], ADepth + 1);
    end;
  end;

  function Presence(ASeen: Boolean): TNyxText;
  begin
    Result := 'present';

    if not ASeen then
    begin
      Result := 'absent';

      if LIncomplete then
      begin
        Result := 'not-seen-incomplete';
      end;
    end;
  end;
begin
  { Failure-only, synchronous copied evidence. No node survives the scan; it
    never realizes metadata, reads widgets, services work or repairs the model.
    Both sections have independent entry/text caps, and all text is scalar-safe
    ASCII-escaped. Counts mean observed nodes; incomplete is never full absence. }
  try
    LOwner := GStudio.Session.SelectedID;
    LView := GStudio.Session.ActiveViewID;
    LLoop := ALoop;
    LWidth := GForm.ClientWidth;
    LChecks := GChecks;
    LRoot := nil;
    LRootID := '';
    LMatches := '';
    LCandidates := '';
    LVisited := 0;
    LMatchCount := 0;
    LCandidateCount := 0;
    LMatchEntries := 0;
    LCandidateEntries := 0;
    LFilter := False;
    LCard := False;
    LPolicy := False;
    LIncomplete := False;
    LTruncated := False;

    if GStudio.ShellView.Root <> nil then
    begin
      LRoot := GStudio.ShellView.RootFor('studio-right');
    end;

    if LRoot <> nil then
    begin
      LRootID := LRoot.ID;
      Visit(LRoot, 0);
    end;
    LText := 'MISSING_POLICY owner=' + Safe(LOwner) + ' / view=' + Safe(LView) +
      ' / loop=' + IntToStr(LLoop) + ' / width=' + IntToStr(LWidth) +
      ' / checks=' + IntToStr(LChecks) + ' / expected-card=' + Safe(AKey) +
      ' / event-name=' + Safe(AName) +
      ' / inspector=' + BoolToStr(LRoot <> nil, True) + ' / root=' + Safe(LRootID) +
      ' / filter=' + Presence(LFilter) + ' / card=' + Presence(LCard) +
      ' / policy=' + Presence(LPolicy) + ' / semantic-matches=' + IntToStr(LMatchCount) +
      LMatches + ' / named-policy-candidates=' + IntToStr(LCandidateCount) + LCandidates;
    LRoot := nil;

    if Length(LText) > CMaxOutput - 256 then
    begin
      LText := Copy(LText, 1, CMaxOutput - 256);
      LTruncated := True;
    end;
    LFooter := ' / visited=' + IntToStr(LVisited) +
      ' / incomplete=' + BoolToStr(LIncomplete, True) +
      ' / output-truncated=' + BoolToStr(LTruncated, True);
    WriteLn(LText + LFooter);
    Flush(Output);
  except
    { The unchanged original lookup/assertion remains the failure authority when
      copying or writing this supplementary diagnostic itself refuses. }
  end;
end;
{ END MISSING_POLICY_OBSERVATION }

function CodeShowing: Boolean;
var
  LCode: TControl;
begin

  if (GStudio.CodeView.Root = nil) or
    (GStudio.CodeView.Root.Find('studio-code') = nil) then
  begin
    Exit(False);
  end;
  LCode := GStudio.CodeView.InputFor('studio-code');
  { LCL can retain the child's Showing flag after reparenting into Studio's
    hidden parking host. IsVisible qualifies the complete parent chain; both
    predicates must hold before this fixture calls the source editor visible. }
  Result := (LCode is TWinControl) and LCode.IsVisible and TWinControl(LCode).Showing;
end;

procedure CheckPendingConfirmation(const AReview: TNyxCallbackRemoval;
  const APending: TNyxStudioPendingDesign);
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LProjection: TNyxNode;
  LHost: TPanel;
  LView: TNyxLCLRenderer;
begin
  { Pumping Studio can deliver a finished worker before its preparing paint.
    Mount the exact pending snapshot through the public Nyx inspector and real
    LCL renderer without servicing delivery; this cannot depend on worker speed.
    The ordinary Studio journey separately proves warning lifetime/publication. }
  LDocument := TNyxDocument.Create;
  LHost := TPanel.Create(GForm);
  LView := TNyxLCLRenderer.Create;
  LProjection := nil;
  try
    LHost.Parent := GForm;
    LHost.Visible := False;
    LRoot := TNyxNode.Create(nkColumn, 'pending-event-inspector');
    LDocument.AddPage(LRoot);
    LProjection := GStudio.Session.SelectedProjection;
    AddNyxEventsInspector(LRoot, GStudio.Session, LProjection, AReview, APending);
    LView.Render(LDocument, LRoot, LHost);
    Check(not LView.ControlFor('event-removal-confirm').Enabled,
      'Exact pending confirmation mounts as a physically disabled native control');
  finally
    LProjection.Free;
    LView.Free;
    LHost.Free;
    LDocument.Free;
  end;
end;

procedure RevealInspectorControl(const AKey: TNyxText);
var
  LScroll: TScrollBox;
  LCard: TControl;
  LPosition: TPoint;

begin
  LScroll := TScrollBox(GStudio.ShellView.ControlFor('studio-right'));
  LCard := GStudio.ShellView.ControlFor(AKey);
  Check((LScroll <> nil) and (LCard <> nil), 'Actual event card and inspector viewport exist');
  LPosition := LScroll.ScreenToClient(LCard.ClientToScreen(Point(0, 0)));
  LScroll.VertScrollBar.Position := LScroll.VertScrollBar.Position + LPosition.Y;
  Pump;
end;

procedure CaptureEvents(const AKey: TNyxText);
begin
  RevealInspectorControl(AKey);
  Capture('events-desktop');
  GForm.ClientWidth := 390;
  GStudio.RequestRefresh;
  Ready('Capture events compact viewport');
  Click('action-panel-inspector');
  RevealInspectorControl(AKey);
  Capture('events-390');
end;

procedure Run;
const
  CHandwrittenDraftNote = '{ Handwritten application draft }';
var
  LSeed: TNyxProjectPair;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LDraft: TNyxText;
  LDraftBase: TNyxText;
  LInputDraft, LNativeDraft, LNativeSuffix: TNyxText;
  LKey: TNyxText;
  LInfo: TNyxAuthoredEventInfo;
  LHandler: TNyxHandlerRef;
  LEdit: TNyxStudioDesignEdit;
  LPending: TNyxStudioPendingDesign;
  LPolicy: TNyxExecutionPolicy;
  LFailed: Boolean;
  LReview: TNyxCallbackRemoval;
  LSchema: TNyxEventSchema;
  LName: TNyxEventRef;
  LWidthIndex: Integer;

  procedure QualifiedPhase(const AName: TNyxText);
  begin
    WriteLn('Qualified actual callback phase: ', AName);
    Flush(Output);
  end;

  function Difference(const AExpected, AActual: TNyxText): TNyxText;
  var
    LAt: Integer;
  begin
    LAt := 1;
    while (LAt <= Length(AExpected)) and (LAt <= Length(AActual)) and
      (AExpected[LAt] = AActual[LAt]) do
    begin
      Inc(LAt);
    end;
    Result := IntToStr(Length(AExpected)) + '/' + IntToStr(Length(AActual)) +
      '@' + IntToStr(LAt);

    if LAt <= Length(AExpected) then
    begin
      Result := Result + ':expected=' + IntToStr(Ord(AExpected[LAt]));
    end;

    if LAt <= Length(AActual) then
    begin
      Result := Result + ':actual=' + IntToStr(Ord(AActual[LAt]));
    end;
  end;

  function DraftEvidence: TNyxText;
  var
    LActualDraft, LActualBase: TNyxText;
    LActualPolicy: TNyxExecutionPolicy;
  begin
    { Diagnostic reads only: keep the original condition, gestures and deadline.
      Lengths/first differing code units expose normalization or lost capture
      without dumping either the accepted source or the handwritten draft. }
    LActualDraft := GStudio.Session.DraftSource;
    LActualBase := GStudio.Session.SourceDraftBase;
    LActualPolicy := EventInfo(ntBeforeKeyPress, Default(TNyxEventRef)).Policy;
    Result := ' / draft=' + BoolToStr(LActualDraft = LDraft, True) +
      ' / base=' + BoolToStr(LActualBase = LDraftBase, True) +
      ' / pair=' + BoolToStr(PairText = LBefore, True) +
      ' / policy=' + NyxPolicyName(LActualPolicy) +
      ' / pending=' + BoolToStr(GStudio.ProjectSnapshot.Pending, True) +
      ' / state=' + IntToStr(Ord(GStudio.SourceCommands.State)) +
      ' / draft-diff=' + Difference(LDraft, LActualDraft) +
      ' / base-diff=' + Difference(LDraftBase, LActualBase) +
      ' / status=' + Copy(GStudio.Status, 1, 160);
  end;
begin
  Application.Initialize;
  { A creator may require a non-sequential initial policy. This real native
    Inspector path exercises Policy-before-Add without changing the existing
    unrestricted keyboard scenarios or their assertions/deadlines. }
  LName := NyxEvent('deferred-review');
  LSchema := NyxNamedEventSchema(LName, 'OnDeferredReview',
    'An explicitly queued creator callback.', ncCustom, ncCustom, NyxSignalPayload);
  LSchema.AllowedPolicies := [neUIQueue];
  RegisterNyxSchema(NyxCustomKind(NyxKindName(nkMemo)), [], [LSchema]);
  ForceDirectories(ParamStr(1));
  GObserver := TFailureObserver.Create;
  Application.OnException := GObserver.Failed;
  GForm := TForm.Create(nil);
  GForm.ClientWidth := 1100;
  GForm.ClientHeight := 800;
  GStudio := TNyxNativeStudio.Create(GForm,
    IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects');
  try
    LSeed := CreateNyxEventQueueSeed;
    GStudio.LoadProject(LSeed);
    GStudio.Run;
    GForm.Show;
    Pump;
    Select('reply-memo');
    Click(NyxInspectorEventsID);
    LKey := EventKey(ntBeforeKeyPress, Default(TNyxEventRef));
    LBefore := PairText;
    Text(NyxEventFilterPartID(NyxStudioEventFilterID, nefpQuery), 'KEY press');
    Check(GStudio.ShellView.ControlFor(LKey).Visible and
      not GStudio.ShellView.ControlFor(EventKey(ntClick, Default(TNyxEventRef))).Visible,
      'Actual discovery matches multiple ASCII-insensitive terms');
    Choice(NyxEventFilterPartID(NyxStudioEventFilterID, nefpCategory),
      NyxEventCategoryCaption(necFocus));
    Check(not GStudio.ShellView.ControlFor(LKey).Visible,
      'Family and text filters combine rather than replacing each other');
    Click(NyxEventFilterPartID(NyxStudioEventFilterID, nefpClear));
    Check(GStudio.ShellView.ControlFor(LKey).Visible and (PairText = LBefore),
      'Clear restores all events without changing the authored pair');
    Click(LKey + '-add', False);
    Check(GStudio.SourceCommands.Busy and (PairText = LBefore),
      'Actual Add captures intent and retains the accepted pair during preparation');
    Ready('Initial callback addition');
    LInfo := EventInfo(ntBeforeKeyPress, Default(TNyxEventRef));
    Check(Length(LInfo.Callbacks) = 1, 'Actual Add creates one ordered registration');
    LHandler := LInfo.Callbacks[0].Handler;
    LBefore := PairText;
    TCheckBox(GStudio.ShellView.InputFor(NyxEventFilterPartID(
      NyxStudioEventFilterID, nefpRegistered))).Checked := True;
    Check(GStudio.ShellView.ControlFor(LKey).Visible and
      not GStudio.ShellView.ControlFor(EventKey(ntAfterKeyPress, Default(TNyxEventRef))).Visible,
      'Actual Boolean discovery keeps only registered events');
    Text(NyxEventFilterPartID(NyxStudioEventFilterID, nefpQuery), LHandler.Name);
    Check(GStudio.ShellView.ControlFor(LKey).Visible and
      not GStudio.ShellView.ControlFor(EventKey(ntAfterKeyPress, Default(TNyxEventRef))).Visible and
      (PairText = LBefore), 'Registered Pascal handler names are searchable without source edits');
    Click(NyxEventFilterPartID(NyxStudioEventFilterID, nefpClear));
    Check(CodeShowing and (Pos('TODO', GStudio.Session.Source) > 0) and
      (TMemo(GStudio.CodeView.InputFor('studio-code')).CaretPos.Y + 1 =
        GStudio.Session.CallbackLine(LHandler)) and
      (Screen.ActiveControl = GStudio.CodeView.InputFor('studio-code')),
      'Admitted handler opens real Pascal control at its TODO implementation line');

    Choice(LKey + '-policy', NyxPolicyName(neUIQueue));
    Choice(LKey + '-policy', NyxPolicyName(neAsynchronous));
    Choice(LKey + '-policy', NyxPolicyName(neThreaded));
    LPending := GStudio.SourceCommands.PendingDesign;
    Check(LPending.EventPolicy('reply-memo', ntBeforeKeyPress, Default(TNyxEventRef), LPolicy)
      and (LPolicy = neThreaded), 'Latest pending policy owns exact event presentation');
    Pump;
    Check((TComboBox(GStudio.ShellView.InputFor(LKey + '-policy')).Text = NyxPolicyName(neThreaded))
      and (Screen.ActiveControl = GStudio.ShellView.InputFor(LKey + '-policy')),
      'Earlier painting retains latest policy input and exact event field focus');
    Ready('Rapid callback policy queue');
    LInfo := EventInfo(ntBeforeKeyPress, Default(TNyxEventRef));
    Check((LInfo.Policy = neThreaded) and (Length(LInfo.Callbacks) = 1),
      'Rapid policy queue retains its registration and latest choice');
    LAfter := PairText;
    Click('action-undo');
    Check(EventInfo(ntBeforeKeyPress, Default(TNyxEventRef)).Policy <> neThreaded,
      'One Undo removes the latest admitted policy step');
    Click('action-redo');
    Check(PairText = LAfter, 'Redo restores its exact paired source and design');
    QualifiedPhase('add, navigation, pending policy and paired history');

    Click(LKey + '-add');
    Check(Length(EventInfo(ntBeforeKeyPress, Default(TNyxEventRef)).Callbacks) = 2,
      'Second actual Add appends a distinct ordered registration');
    Check(Screen.ActiveControl = GStudio.CodeView.InputFor('studio-code'),
      'Admitted handler navigation owns focus over the previously focused policy');
    LBefore := PairText;
    Click(LKey + '-callback-0-remove');
    Check((GStudio.ShellView.ControlFor('event-removal-warning') <> nil) and
      not GStudio.SourceCommands.Busy and (PairText = LBefore),
      'Actual removal request displays a warning without mutation');
    Check(GStudio.ShellView.Root.Find('event-removal-warning').Parent.ID =
      LKey + '-callback-0', 'Warning belongs to the exact native callback row');
    Text(NyxEventFilterPartID(NyxStudioEventFilterID, nefpQuery), 'no matching callback');
    Check(GStudio.ShellView.ControlFor(LKey).Visible and
      (Pos('pending review shown', GStudio.ShellView.Root.Find(
      NyxEventFilterPartID(NyxStudioEventFilterID, nefpStatus)).Prop('text')) > 0) and
      (PairText = LBefore), 'Unmatched pending removal stays visible and retains exact source');
    Click(NyxEventFilterPartID(NyxStudioEventFilterID, nefpClear));
    RevealInspectorControl('event-removal-warning');
    Capture('warning-desktop');
    Click('event-removal-cancel');
    Check((PairText = LBefore) and
      (GStudio.ShellView.Root.Find('event-removal-warning') = nil),
      'Keep registration changes only presentation');
    Click(LKey + '-callback-0-remove');
    LInfo := EventInfo(ntBeforeKeyPress, Default(TNyxEventRef));
    LReview := Default(TNyxCallbackRemoval);
    LReview.Pending := True;
    LReview.OwnerID := GStudio.Session.SelectedID;
    LReview.Trigger := ntBeforeKeyPress;
    LReview.ID := LInfo.Callbacks[0].ID;
    LReview.Handler := LInfo.Callbacks[0].Handler;
    LReview.Context := GStudio.Session.CommandContext;
    Click('event-removal-confirm', False);
    Check(GStudio.SourceCommands.Busy and (PairText = LBefore) and
      (GStudio.ShellView.ControlFor('event-removal-warning') <> nil),
      'Confirmation keeps its warning and exact pair until independent admission');
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaEvent;
    LEdit.Selection := 'reply-memo';
    LEdit.View := 'home';
    LEdit.Event.Action := seaPolicy;
    LEdit.Event.Trigger := ntBeforeKeyPress;
    LFailed := False;
    try
      GStudio.SourceCommands.Edit(LEdit);
    except
      on ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Pre-paint typed guard refuses mutation of an event being removed');
    CheckPendingConfirmation(LReview, GStudio.SourceCommands.PendingDesign);
    Ready('Callback removal publication');
    Check((Length(EventInfo(ntBeforeKeyPress, Default(TNyxEventRef)).Callbacks) = 1) and
      (GStudio.ShellView.Root.Find('event-removal-warning') = nil) and
      (GStudio.Session.CallbackLine(LHandler) > 0),
      'Publication removes exact registration, clears its warning and retains implementation');
    Click('action-undo');
    Check(PairText = LBefore, 'One Undo restores both registrations and their exact source');
    Click('action-redo');
    QualifiedPhase('warning, confirmation, native disabled snapshot and exact removal');

    LDraftBase := GStudio.Session.Source;
    LInputDraft := LDraftBase + #10 + CHandwrittenDraftNote;
    { Keep the original LF physical assignment. The native memo presents its
      new insertion with the host line ending; ntePreserve retains those newly
      authored units while preserving every preexisting source byte. Compute
      that exact expectation independently, never from the session's draft. }
    LDraft := LDraftBase + TNyxText(LineEnding) + CHandwrittenDraftNote;
    Text('studio-code', LInputDraft);
    LNativeDraft := TNyxText(TCustomEdit(GStudio.CodeView.InputFor('studio-code')).Text);
    LNativeSuffix := TNyxText(LineEnding) + CHandwrittenDraftNote;
    Check((Length(LNativeDraft) >= Length(LNativeSuffix)) and
      (Copy(LNativeDraft, Length(LNativeDraft) - Length(LNativeSuffix) + 1,
        MaxInt) = LNativeSuffix),
      'Actual native memo exposes the expected inserted line-ending suffix');
    LBefore := PairText;
    Text(NyxEventFilterPartID(NyxStudioEventFilterID, nefpQuery), 'key press');
    Check(PairText = LBefore, 'Discovery remains available without rewriting pending Pascal');
    Click(NyxEventFilterPartID(NyxStudioEventFilterID, nefpClear));
    Click(EventKey(ntAfterKeyPress, Default(TNyxEventRef)) + '-add', False);
    Ready('Pending Pascal addition refusal');
    Check((PairText = LBefore) and
      (Length(EventInfo(ntAfterKeyPress, Default(TNyxEventRef)).Callbacks) = 0),
      'Actual Add refuses independent pending Pascal without replacing it');
    Choice(LKey + '-policy', NyxPolicyName(neSequential));
    Ready('Pending Pascal policy refusal');
    Check((GStudio.Session.DraftSource = LDraft) and
      (GStudio.Session.SourceDraftBase = LDraftBase) and
      (PairText = LBefore) and
      (EventInfo(ntBeforeKeyPress, Default(TNyxEventRef)).Policy = neThreaded),
      'Pending Pascal refuses policy publication and retains exact pair/draft/base' + DraftEvidence);
    Click('action-reset-source');
    QualifiedPhase('independent Pascal draft and refusal');

    Select('first-search');
    LKey := EventKey(ntNamed, NyxSemantic(nseSearch));
    Click(LKey + '-add');
    Check(Length(EventInfo(ntNamed, NyxSemantic(nseSearch)).Callbacks) = 1,
      'Reusable instance exposes its typed semantic event');
    Check(not GStudio.Session.Document.Find('second-search').Extensions.Has(
      NyxExtension(NyxCallbacksKey)), 'Named addition leaves the other reusable instance independent');
    Click('action-code');
    Check((GStudio.ShellView.Root.Find('studio-source-mount') = nil) and not CodeShowing,
      'Source is deliberately hidden before navigation during preparation');
    Click(LKey + '-add', False);
    Select('second-search');
    Ready('Reusable navigation during preparation');
    Check((GStudio.Session.SelectedID = 'second-search') and not CodeShowing,
      'Older admitted handler cannot steal later selection or reopen hidden Pascal');
    Select('first-search');
    Check(Length(EventInfo(ntNamed, NyxSemantic(nseSearch)).Callbacks) = 2,
      'The captured first instance owns the addition after later navigation');
    CaptureEvents(LKey);
    QualifiedPhase('named reusable ownership, independent navigation and English captures');

    for LWidthIndex := 0 to 1 do
    begin

      if LWidthIndex = 0 then
      begin
        GForm.ClientWidth := 1100;
      end
      else
      begin
        GForm.ClientWidth := 390;
      end;
      GStudio.RequestRefresh;
      Ready('Atomic callback viewport');
      Check(((LWidthIndex = 0) and (GForm.ClientWidth = 1100)) or
        ((LWidthIndex = 1) and (GForm.ClientWidth = 390)),
        'Atomic callback phase runs at its requested actual client width');
      { BEGIN COMPACT_POLICY_PANEL_NAVIGATION }
      { TODO navigation selected Design. Open the real compact Inspector before
        the existing selection/Ready coalesces this deferred panel change. }

      if LWidthIndex = 1 then
      begin
        Click('action-panel-inspector', False);
      end;
      { END COMPACT_POLICY_PANEL_NAVIGATION }
      Select('reply-memo');
      LKey := EventKey(ntNamed, LName);
      { BEGIN MISSING_POLICY_OBSERVATION_CALL }

      if (GStudio.ShellView.Root = nil) or
        (GStudio.ShellView.Root.Find(LKey + '-policy') = nil) then
      begin
        ObserveMissingPolicy(LKey, LName.Name, LWidthIndex);
      end;
      { END MISSING_POLICY_OBSERVATION_CALL }
      Check((TComboBox(GStudio.ShellView.InputFor(LKey + '-policy')).Items.Count = 1) and
        (TComboBox(GStudio.ShellView.InputFor(LKey + '-policy')).Text = NyxPolicyName(neUIQueue)),
        'Actual creator event exposes only its allowed initial policy');
      LBefore := PairText;
      Click(LKey + '-add', False);
      LPending := GStudio.SourceCommands.PendingDesign;
      Check(LPending.EventLocked('reply-memo', ntNamed, LName) and
        not LPending.EventLocked('reply-memo', ntKeyDown, Default(TNyxEventRef)) and
        not LPending.EventLocked('send-button', ntNamed, LName) and
        LPending.EventPolicy('reply-memo', ntNamed, LName, LPolicy) and (LPolicy = neUIQueue),
        'Grouped Add locks only its exact event and presents its ordered policy');
      LEdit := Default(TNyxStudioDesignEdit);
      LEdit.Action := sdaEvent;
      LEdit.Selection := 'reply-memo';
      LEdit.View := 'home';
      LEdit.Event.Action := seaAdd;
      LEdit.Event.Trigger := ntNamed;
      LEdit.Event.Name := LName;
      LFailed := False;
      try
        GStudio.SourceCommands.Edit(LEdit);
      except
        on ENyxModel do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed and (PairText = LBefore),
        'A pre-paint Add cannot duplicate a pending grouped registration');
      LEdit.Action := sdaCallbacks;
      LEdit.Event := Default(TNyxStudioEventIntent);
      LEdit.Callbacks := CaptureNyxCallbackBatch(NyxCallbackPatch([
        NyxSetCallbackPolicy('reply-memo', NyxCallbackEvent(LName), neUIQueue),
        NyxAddCallback('reply-memo', NyxCallbackEvent(LName))]));
      LFailed := False;
      try
        GStudio.SourceCommands.Edit(LEdit);
      except
        on ENyxModel do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed, 'A second batch checks exact pending-event locks before enqueue');
      Ready('Atomic callback publication');
      LInfo := EventInfo(ntNamed, LName);
      Check((LInfo.Policy = neUIQueue) and (Length(LInfo.Callbacks) = 1),
        'One actual Add admits its non-sequential policy and sole callback together');
      LHandler := LInfo.Callbacks[0].Handler;
      Check(CodeShowing and
        (TMemo(GStudio.CodeView.InputFor('studio-code')).CaretPos.Y + 1 =
          GStudio.Session.CallbackLine(LHandler)) and
        (Screen.ActiveControl = GStudio.CodeView.InputFor('studio-code')),
        'Grouped completion opens the actual Pascal editor at its fresh TODO line');
      Check(not GStudio.SourceCommands.PendingDesign.EventLocked('reply-memo', ntNamed, LName),
        'Publication retires the exact grouped event lock');
      LAfter := PairText;
      Click('action-undo');
      Check(PairText = LBefore, 'One actual Undo removes the entire policy/addition pair');
      Click('action-redo');
      Check(PairText = LAfter, 'One actual Redo restores the entire policy/addition pair');

      if LWidthIndex = 0 then
      begin
        Click('action-undo');
        Check(PairText = LBefore, 'Wide group retires as one Undo before the compact journey');
      end;
    end;
    { BEGIN STALE_CALLBACK_PANEL_NAVIGATION }
    { Compact TODO focus selected Design again. Mount the first-search controls
      through the existing Select/Ready before the deliberately unpainted reload. }
    Click('action-panel-inspector', False);
    { END STALE_CALLBACK_PANEL_NAVIGATION }
    Select('first-search');
    LKey := EventKey(ntNamed, NyxSemantic(nseSearch));
    QualifiedPhase('restricted creator policy, atomic Add, exact locks and TODO focus');

    { Exact same IDs after reload still retire old mounted event controls. }
    GStudio.Session.LoadProject(LSeed);
    LBefore := PairText;
    Click(LKey + '-add', False);
    Check(not GStudio.SourceCommands.Busy and (PairText = LBefore),
      'Pre-paint event from the older mounted load cannot target new matching IDs');
    GStudio.RequestRefresh;
    Pump;
    Select('reply-memo');
    LKey := EventKey(ntBeforeKeyPress, Default(TNyxEventRef));
    Click(LKey + '-add', False);
    Check(GStudio.SourceCommands.Busy, 'Retirement begins with real callback preparation');
    FreeAndNil(GStudio);
    Pump;
    Check(GObserver.Error = '', 'Detached callback worker cannot call freed editor or controls');
  finally
    GStudio.Free;
    GStudio := nil;
    GForm.Free;
    Application.OnException := nil;
    GObserver.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' actual native queued callback checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
