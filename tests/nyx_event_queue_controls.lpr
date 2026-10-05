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

procedure Ready;
var
  LStart: QWord;
begin
  LStart := GetTickCount64;
  repeat
    Pump;

    if GetTickCount64 - LStart > 30000 then
    begin
      raise ENyxModel.Create('Event command did not retire / ' + GStudio.Status);
    end;
    Sleep(1);
  until not GStudio.SourceCommands.Busy and not GStudio.PresentationPending;
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
    Ready;
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
  Result := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
end;

procedure Select(const AID: TNyxText);
begin
  GStudio.Session.Select(AID);
  GStudio.RequestRefresh;
  Ready;
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

procedure CaptureEvents(const AKey: TNyxText);
var
  LScroll: TScrollBox;
  LCard: TControl;
  LPosition: TPoint;

  procedure Reveal;
  begin
    LScroll := TScrollBox(GStudio.ShellView.ControlFor('studio-right'));
    LCard := GStudio.ShellView.ControlFor(AKey);
    Check((LScroll <> nil) and (LCard <> nil), 'Actual event card and inspector viewport exist');
    LPosition := LScroll.ScreenToClient(LCard.ClientToScreen(Point(0, 0)));
    LScroll.VertScrollBar.Position := LScroll.VertScrollBar.Position + LPosition.Y;
    Pump;
  end;

begin
  Reveal;
  Capture('events-desktop');
  GForm.ClientWidth := 390;
  GStudio.RequestRefresh;
  Ready;
  Click('action-panel-inspector');
  Reveal;
  Capture('events-390');
end;

procedure Run;
var
  LSeed: TNyxProjectPair;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LDraft: TNyxText;
  LDraftBase: TNyxText;
  LKey: TNyxText;
  LInfo: TNyxAuthoredEventInfo;
  LHandler: TNyxHandlerRef;
  LEdit: TNyxStudioDesignEdit;
  LPending: TNyxStudioPendingDesign;
  LPolicy: TNyxExecutionPolicy;
  LFailed: Boolean;
  LReview: TNyxCallbackRemoval;

  procedure QualifiedPhase(const AName: TNyxText);
  begin
    WriteLn('Qualified actual callback phase: ', AName);
    Flush(Output);
  end;
begin
  Application.Initialize;
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
    Click(LKey + '-add', False);
    Check(GStudio.SourceCommands.Busy and (PairText = LBefore),
      'Actual Add captures intent and retains the accepted pair during preparation');
    Ready;
    LInfo := EventInfo(ntBeforeKeyPress, Default(TNyxEventRef));
    Check(Length(LInfo.Callbacks) = 1, 'Actual Add creates one ordered registration');
    LHandler := LInfo.Callbacks[0].Handler;
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
    Ready;
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
    Ready;
    Check((Length(EventInfo(ntBeforeKeyPress, Default(TNyxEventRef)).Callbacks) = 1) and
      (GStudio.ShellView.Root.Find('event-removal-warning') = nil) and
      (GStudio.Session.CallbackLine(LHandler) > 0),
      'Publication removes exact registration, clears its warning and retains implementation');
    Click('action-undo');
    Check(PairText = LBefore, 'One Undo restores both registrations and their exact source');
    Click('action-redo');
    QualifiedPhase('warning, confirmation, native disabled snapshot and exact removal');

    LDraft := GStudio.Session.Source + #10 + '{ Handwritten application draft }';
    LDraftBase := GStudio.Session.Source;
    Text('studio-code', LDraft);
    LBefore := PairText;
    Click(EventKey(ntAfterKeyPress, Default(TNyxEventRef)) + '-add', False);
    Ready;
    Check((PairText = LBefore) and
      (Length(EventInfo(ntAfterKeyPress, Default(TNyxEventRef)).Callbacks) = 0),
      'Actual Add refuses independent pending Pascal without replacing it');
    Choice(LKey + '-policy', NyxPolicyName(neSequential));
    Ready;
    Check((GStudio.Session.DraftSource = LDraft) and
      (GStudio.Session.SourceDraftBase = LDraftBase) and
      (EventInfo(ntBeforeKeyPress, Default(TNyxEventRef)).Policy = neSequential),
      'Ordinary policy publication retains exact independent draft and original base');
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
    Ready;
    Check((GStudio.Session.SelectedID = 'second-search') and not CodeShowing,
      'Older admitted handler cannot steal later selection or reopen hidden Pascal');
    Select('first-search');
    Check(Length(EventInfo(ntNamed, NyxSemantic(nseSearch)).Callbacks) = 2,
      'The captured first instance owns the addition after later navigation');
    CaptureEvents(LKey);
    QualifiedPhase('named reusable ownership, independent navigation and English captures');

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
