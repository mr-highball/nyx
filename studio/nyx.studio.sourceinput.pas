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

unit nyx.studio.sourceinput;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.types, nyx.events, nyx.behavior, nyx.scheduler, nyx.editing,
  nyx.editing.commands, nyx.editing.history, nyx.text.projection, nyx.pascal.project,
  nyx.pascal.sources, nyx.pascal.drafts, nyx.pascal.completion, nyx.pascal.definition,
  nyx.pascal.navigation,
  nyx.pascal.completion.view, nyx.studio.completion, nyx.studio.session;

type
  { UI-only physical reader returns the current session as well as copied text.
    An unavailable/readonly/retired mount returns an undefined snapshot. This
    lets a stale command compare session identity before touching its borrow. }
  TNyxStudioSourceRead = function(out ASession: TNyxStudioSession): TNyxEditingSnapshot of object;
  { Present the exact physical frame. Changed=True also notifies the ordinary
    draft bridge after source admission; False repairs presentation only, without
    a new draft/command. Authored baseline bytes may use different line endings.
    Do not destroy the dispatching input/owner inline; queue their retirement. }
  TNyxStudioSourceWrite = procedure(const AFrame: TNyxTextEditorFrame; AChanged: Boolean) of object;
  { The current project's managed weak service. Resolve on each query; switching
    projects must not retain authority from the previous observer. }
  TNyxStudioCompletionContextRead = function: INyxStudioCompletionContext of object;
  { Explicit unit context for an include, selected by the ordinary Outline owner
    control. Main/unit files use themselves; no include owner is guessed. }
  TNyxStudioCompletionOwnerRead = function: TNyxPascalSourceRef of object;
  { Queue popup presentation after input/transport dispatch. The receiver must
    outlive the controller and must not destroy its dispatching callbacks inline. }
  TNyxStudioCompletionChanged = procedure of object;
  { Forward owner for managed input callbacks; the full class owns timelines and
    subscriptions, while the callback only borrows it until explicit detach. }
  TNyxStudioSourceInput = class;
  { One query's revocable weak receiver. The client retains this lease while
    dispatching; retirement clears Owner before replacing a ticket or session. }
  TNyxStudioSourceCompletionReply = class(TInterfacedObject, INyxStudioCompletionReply)
  public
    Owner: TNyxStudioSourceInput;
    Ticket: TNyxPascalCompletionTicket;
    procedure Receive(const AQuery: INyxPascalCompletionRequest;
      const AResult: INyxPascalCompletionResult);
  end;
  { One selected-file projected timeline. AuthorText detects external changes;
    each history frame owns exact authored/displayed bytes independently. The
    temporary composition base restores cancelled authored bytes exactly. }
  TNyxStudioSourceBuffer = class
  public
    FileReference: TNyxPascalSourceRef;
    AuthorText: TNyxText;
    CompositionBase: TNyxTextEditorFrame;
    HasCompositionBase: Boolean;
    History: INyxTextEditHistory;
  end;
  { Revocable mounted-input command/key receiver. Renderer/subscriptions retain
    this lease, never the owning Studio/session. Detach precedes owner teardown. }
  TNyxStudioSourceInputPort = class(TNyxEventCallback, INyxTextHistoryCommands)
  public
    Owner: TNyxStudioSourceInput;
    function CanExecute(ACommand: TNyxTextHistoryCommand): Boolean;
    procedure Execute(ACommand: TNyxTextHistoryCommand);
    procedure BeforeEdit(const AEditing: TNyxEditingSnapshot);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  { UI-owned shared source input controller for both Studio adapters. Borrows its
    current session and host methods; owns eight recent file timelines (128
    entries/16 MiB accounted UTF-8 each), never controls or accepted documents.
    Project/load/accepted identity and external draft changes reset stale history.
    Physical composition shares one entry; ordinary typing and every routed
    history producer use the same timeline. Paired document history stays separate. }
  TNyxStudioSourceInput = class
  private
    FRead: TNyxStudioSourceRead;
    FWrite: TNyxStudioSourceWrite;
    FSession: TNyxStudioSession;
    FContext: TNyxStudioCommandContext;
    FProject: INyxPascalProject;
    FBuffers: array of TNyxStudioSourceBuffer;
    FActive: TNyxStudioSourceBuffer;
    FPresentation: TNyxTextLineEndings;
    FPort: TNyxStudioSourceInputPort;
    FLease: INyxTextHistoryCommands;
    FSubscriptions: array of INyxEventSubscription;
    FCompletionContextRead: TNyxStudioCompletionContextRead;
    FCompletionOwnerRead: TNyxStudioCompletionOwnerRead;
    FCompletionChanged: TNyxStudioCompletionChanged;
    FCompletionContext: INyxStudioCompletionContext;
    FCompletionMenu: INyxPascalCompletionMenu;
    FCompletionQuery: INyxPascalCompletionRequest;
    FCompletionReply: TNyxStudioSourceCompletionReply;
    FCompletionLease: INyxStudioCompletionReply;
    FCompletionGeneration: Integer;
    FCompletionRevision: Integer;
    FCompletionWaiting: Boolean;
    FDefinitionLookup: Boolean;
    { A row click requests navigation for the next host turn. No file switches
      while its popup/input callback is dispatching; that turn rechecks authority. }
    FDefinitionPending: Boolean;
    FDefinitionRow: Integer;
    { Local navigation owns values, not a compiler/service lease. Pending moves
      remain unpublished until the queued host turn rechecks the live mount. }
    FNavigationTrail: INyxPascalNavigationTrail;
    FNavigationPending: INyxPascalNavigationTrail;
    FNavigationQuery: INyxPascalCompletionRequest;
    FNavigationGeneration: Integer;
    FNavigationMessage: TNyxText;
    function NavigationPoint: TNyxPascalNavigationPoint;
    procedure RequestNavigation(ADirection: TNyxPascalNavigationDirection);
    procedure CompletionChanged;
    function CompletionCurrent: Boolean;
    procedure SubmitCompletion;
    procedure StartCompletion(AOffset: Integer = 0;
      AKind: TNyxStudioPascalQueryKind = spqCompletion);
    procedure CompletionReply(AReply: TNyxStudioSourceCompletionReply;
      const AQuery: INyxPascalCompletionRequest; const AResult: INyxPascalCompletionResult);
    procedure CompletionAction(AAction: TNyxPascalCompletionAction);
    procedure Clear;
    function Current(out AEditing: TNyxEditingSnapshot): Boolean;
    function NextFrame(const AEditing: TNyxEditingSnapshot): TNyxTextEditorFrame;
    procedure Observe(const AEditing: TNyxEditingSnapshot);
    procedure Event(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
    function CanExecute(ACommand: TNyxTextHistoryCommand): Boolean;
    procedure Execute(ACommand: TNyxTextHistoryCommand);
  public
    { Subscribe only to the retained source view; callbacks are UI-thread only.
      The host methods outlive this controller and must identify their live mount. }
    constructor Create(const AEvents: INyxEvents;
      ARead: TNyxStudioSourceRead; AWrite: TNyxStudioSourceWrite;
      APresentation: TNyxTextLineEndings = ntePreserve);
    destructor Destroy; override;
    { Reconcile after ordinary source render/reparent/navigation. Capture the
      physical baseline without publishing a draft merely for native formatting. }
    procedure Bind;
    { Admit one normal source change before shared draft notification. Selection/
      composition-only callbacks update the timeline without publishing a draft. }
    function Change(ANode: TNyxNode; ATrigger: TNyxTrigger): Boolean;
    { Captured immutable accepted/current files for proposal staging. This is
      inspection data, never server acknowledgement or compilation authority. }
    function Sources: INyxPascalSourceSnapshot;
    { Configure borrowed host callbacks before ordinary input. The managed
      context itself is resolved per query and revoked on project retirement. }
    procedure ConfigureCompletion(AContext: TNyxStudioCompletionContextRead;
      AOwner: TNyxStudioCompletionOwnerRead; AChanged: TNyxStudioCompletionChanged);
    { Close/revoke a local query without cancelling admitted server analysis. }
    procedure DismissCompletion;
    { Inspect the identifier at the current caret. A single project target is
      queued for normal navigation; declared alternatives use the guarded popup. }
    procedure RequestDefinition;
    { Local caret trail remains available without a compiler server. Shortcuts
      Alt+Left/Right and visible actions share this queued, undo-free contract. }
    procedure NavigateBack;
    procedure NavigateForward;
    { Consume either local traversal or the existing guarded definition jump.
      Call before rebinding the physical source, after input dispatch returns. }
    function TakeSourceNavigation(out ALine, AColumn: Integer): Boolean;
    { Independent public Nyx card document, or nil when closed. Caller owns it.
      Presentation must run after input/transport return, preserving source focus. }
    function BuildCompletion: TNyxDocument;
    { Exact current popup mount and typed action/ticket guard every row action. }
    function RouteCompletion(ARoot, ANode: TNyxNode; ATrigger: TNyxTrigger): Boolean;
    { Consume a queued project-definition request before the host rebinds input.
      False includes stale selection/drafts/service/project or absent metadata.
      True switches only ActiveSourceFile and returns its one-based source caret;
      the host queues/renders its normal source pane. No draft or Undo is added. }
    function TakeDefinitionNavigation(out ALine, AColumn: Integer): Boolean;
    { Managed dismissal callback remains safe after this controller retires. }
    function CompletionDismissReceiver: INyxEventCallback;
    property CompletionRevision: Integer read FCompletionRevision;
    property Commands: INyxTextHistoryCommands read FLease;
  end;

implementation

uses SysUtils, nyx.controls;

procedure TNyxStudioSourceCompletionReply.Receive(const AQuery: INyxPascalCompletionRequest;
  const AResult: INyxPascalCompletionResult);
begin

  if Owner <> nil then
  begin
    Owner.CompletionReply(Self, AQuery, AResult);
  end;
end;

function TNyxStudioSourceInputPort.CanExecute(ACommand: TNyxTextHistoryCommand): Boolean;
begin
  Result := (Owner <> nil) and Owner.CanExecute(ACommand);
end;

procedure TNyxStudioSourceInputPort.Execute(ACommand: TNyxTextHistoryCommand);
begin

  if Owner <> nil then
  begin
    Owner.Execute(ACommand);
  end;
end;

procedure TNyxStudioSourceInputPort.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if Owner <> nil then
  begin

    if AEvent.IsNamed(NyxSemantic(nseDismiss)) then
    begin
      Owner.DismissCompletion;
    end
    else
    begin
      Owner.Event(AEvent, AExecution);
    end;
  end;
end;

procedure TNyxStudioSourceInputPort.BeforeEdit(const AEditing: TNyxEditingSnapshot);
var
  LCurrent: TNyxEditingSnapshot;
begin

  if (Owner <> nil) and Owner.Current(LCurrent) and
    (LCurrent.Text = AEditing.Text) and LCurrent.Selection.SameRange(AEditing.Selection) then
  begin
    Owner.Observe(LCurrent);
  end;
end;

constructor TNyxStudioSourceInput.Create(const AEvents: INyxEvents;
  ARead: TNyxStudioSourceRead; AWrite: TNyxStudioSourceWrite;
  APresentation: TNyxTextLineEndings);
var
  LTarget: TNyxEventTarget;
begin
  inherited Create;

  if (AEvents = nil) or not Assigned(ARead) or not Assigned(AWrite) then
  begin
    raise EArgumentException.Create('Source input requires its retained view and UI owner');
  end;
  AEvents.Scheduler.RequireUI;
  NyxPresentText('', APresentation);
  FPresentation := APresentation;
  FRead := ARead;
  FWrite := AWrite;
  FPort := TNyxStudioSourceInputPort.Create;
  FLease := FPort;
  FPort.Owner := Self;
  SetLength(FSubscriptions, 5);
  LTarget := NyxControlEvents('studio-code');
  FSubscriptions[0] := AEvents.OnKeyDown(LTarget).Subscribe(FPort);
  FSubscriptions[1] := AEvents.OnCompositionStart(LTarget).Subscribe(FPort);
  FSubscriptions[2] := AEvents.OnCompositionUpdate(LTarget).Subscribe(FPort);
  FSubscriptions[3] := AEvents.OnCompositionEnd(LTarget).Subscribe(FPort);
  FSubscriptions[4] := AEvents.OnTextSelectionChange(LTarget).Subscribe(FPort);
end;

destructor TNyxStudioSourceInput.Destroy;
var
  LIndex: Integer;
begin
  FCompletionChanged := nil;
  DismissCompletion;
  FCompletionContextRead := nil;
  FCompletionOwnerRead := nil;

  if FPort <> nil then
  begin
    FPort.Owner := nil;
  end;
  for LIndex := Low(FSubscriptions) to High(FSubscriptions) do
  begin

    if FSubscriptions[LIndex] <> nil then
    begin
      FSubscriptions[LIndex].Cancel;
      FSubscriptions[LIndex] := nil;
    end;
  end;
  FLease := nil;
  FPort := nil;
  Clear;
  FRead := nil;
  FWrite := nil;
  FSession := nil;
  inherited Destroy;
end;

procedure TNyxStudioSourceInput.Clear;
var
  LIndex: Integer;
begin
  DismissCompletion;
  FNavigationTrail := nil;
  FCompletionMenu := nil;
  FActive := nil;
  for LIndex := 0 to High(FBuffers) do
  begin
    FBuffers[LIndex].Free;
  end;
  SetLength(FBuffers, 0);
end;

procedure TNyxStudioSourceInput.Bind;
var
  LSession: TNyxStudioSession;
  LEditing: TNyxEditingSnapshot;
  LProject: INyxPascalProject;
  LBuffer: TNyxStudioSourceBuffer;
  LAt: Integer;
  LIndex: Integer;
begin
  LEditing := FRead(LSession);

  if not LEditing.Defined or (LSession = nil) then
  begin
    DismissCompletion;
    Exit;
  end;
  LProject := LSession.AcceptedPascalProject;

  if (LSession <> FSession) or (LProject <> FProject) or
    not LSession.MatchesCommandContext(FContext) then
  begin
    Clear;
    FSession := LSession;
    FProject := LProject;
    FContext := FSession.CommandContext;
  end;
  LAt := -1;
  for LIndex := 0 to High(FBuffers) do
  begin

    if FBuffers[LIndex].FileReference.SameFile(FSession.ActiveSourceFile) then
    begin
      LAt := LIndex;
      Break;
    end;
  end;

  if LAt < 0 then
  begin
    LBuffer := TNyxStudioSourceBuffer.Create;
    try
      LBuffer.FileReference := FSession.ActiveSourceFile;
      LBuffer.AuthorText := FSession.ActiveSourceText;
      LBuffer.History := NewNyxTextEditHistory(NyxProjectedTextEditorFrame(
        NewNyxTextProjection(LBuffer.AuthorText, LEditing.Text), LEditing.Selection, FPresentation));

      if Length(FBuffers) = 8 then
      begin
        FBuffers[High(FBuffers)].Free;
        FBuffers[High(FBuffers)] := nil;
        LAt := High(FBuffers);
      end
      else
      begin
        LAt := Length(FBuffers);
        SetLength(FBuffers, LAt + 1);
      end;
      FBuffers[LAt] := LBuffer;
      LBuffer := nil;
    finally
      LBuffer.Free;
    end;
  end;
  LBuffer := FBuffers[LAt];
  for LIndex := LAt downto 1 do
  begin
    FBuffers[LIndex] := FBuffers[LIndex - 1];
  end;
  FBuffers[0] := LBuffer;

  if FActive <> LBuffer then
  begin
    DismissCompletion;
    FCompletionMenu := nil;
  end;
  FActive := LBuffer;

  if FActive.AuthorText <> FSession.ActiveSourceText then
  begin
    FActive.History.Reset(NyxProjectedTextEditorFrame(
      NewNyxTextProjection(FSession.ActiveSourceText, LEditing.Text), LEditing.Selection, FPresentation));
    FActive.AuthorText := FSession.ActiveSourceText;
    FActive.HasCompositionBase := False;
  end
  else if not LEditing.Composing and (FActive.History.Current.Text <> LEditing.Text) then
  begin
    { Reparenting may recreate a native handle and normalize its line endings.
      Restore the owned physical frame without publishing another draft. }
    FWrite(FActive.History.Current, False);
  end;

  if FCompletionMenu = nil then
  begin
    FCompletionMenu := NewNyxPascalCompletionMenu(FActive.History);
  end;

  if FNavigationTrail = nil then FNavigationTrail := NewNyxPascalNavigationTrail;

  if FCompletionQuery <> nil then
  begin

    if not CompletionCurrent or (Assigned(FCompletionContextRead) and
      (FCompletionContextRead() <> FCompletionContext)) then
    begin
      DismissCompletion;
    end
    else if FCompletionWaiting then
    begin
      SubmitCompletion;
    end;
  end;
end;

function TNyxStudioSourceInput.NextFrame(const AEditing: TNyxEditingSnapshot): TNyxTextEditorFrame;
var
  LBefore: TNyxTextEditorFrame;
  LProjection: INyxTextProjection;
begin
  LBefore := FActive.History.Current;

  if AEditing.Composing and not FActive.History.Composing then
  begin
    FActive.CompositionBase := LBefore;
    FActive.HasCompositionBase := True;
  end;

  if not AEditing.Composing and FActive.History.Composing and FActive.HasCompositionBase and
    (AEditing.Text = FActive.CompositionBase.Text) then
  begin
    LProjection := FActive.CompositionBase.Projection;
  end
  else
  begin
    LProjection := LBefore.Projection.ObservePresented(AEditing.Text);
  end;
  Result := NyxProjectedTextEditorFrame(LProjection, AEditing.Selection, FPresentation);
end;

procedure TNyxStudioSourceInput.Observe(const AEditing: TNyxEditingSnapshot);
begin

  if FNavigationMessage <> '' then DismissCompletion;
  FActive.History.Observe(NextFrame(AEditing), AEditing.Composing);

  if not AEditing.Composing then
  begin
    FActive.HasCompositionBase := False;
  end;

  if (FCompletionQuery <> nil) and not CompletionCurrent then
  begin
    DismissCompletion;
  end;
end;

function TNyxStudioSourceInput.Current(out AEditing: TNyxEditingSnapshot): Boolean;
var
  LSession: TNyxStudioSession;
begin
  AEditing := FRead(LSession);
  Result := AEditing.Defined and (FActive <> nil) and (LSession <> nil) and
    (LSession = FSession) and LSession.MatchesCommandContext(FContext) and
    (LSession.AcceptedPascalProject = FProject) and
    LSession.ActiveSourceFile.SameFile(FActive.FileReference) and
    (LSession.ActiveSourceText = FActive.AuthorText);
end;

function TNyxStudioSourceInput.Change(ANode: TNyxNode; ATrigger: TNyxTrigger): Boolean;
var
  LEditing: TNyxEditingSnapshot;
  LFrame: TNyxTextEditorFrame;
begin
  Result := (ANode <> nil) and (ANode.ID = 'studio-code') and (ATrigger = ntChange);

  if not Result then
  begin
    Exit;
  end;

  if not Current(LEditing) then
  begin
    raise ENyxTextHistory.Create('Source input changed; reconcile its current source before editing');
  end;
  LFrame := NextFrame(LEditing);
  FSession.SetFileDraft(FActive.FileReference, LFrame.AuthoredText);
  FActive.AuthorText := FSession.ActiveSourceText;
  FActive.History.Observe(LFrame, LEditing.Composing);

  if not LEditing.Composing then
  begin
    FActive.HasCompositionBase := False;
  end;
  DismissCompletion;
end;

procedure TNyxStudioSourceInput.Event(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LEditing: TNyxEditingSnapshot;
  LCommand: TNyxTextHistoryCommand;
  LAction: TNyxPascalCompletionAction;
  LNavigationKey: Boolean;
  LDirection: TNyxPascalNavigationDirection;
begin
  LNavigationKey := AEvent.HasKeyboard and
    (AEvent.Keyboard.Matches(nkLeftKey, [nmAlt], True) or
    AEvent.Keyboard.Matches(nkRightKey, [nmAlt], True));
  LDirection := pndBack;

  if AEvent.HasKeyboard and AEvent.Keyboard.Matches(nkRightKey, [nmAlt], True) then
    LDirection := pndForward;

  if not Current(LEditing) then
  begin

    if LNavigationKey or
      (AEvent.HasKeyboard and TryNyxTextHistoryKey(AEvent.Keyboard, LCommand)) then
    begin
      NyxEventResponse(AExecution).Consume;
    end;
    Exit;
  end;
  Observe(LEditing);

  if LNavigationKey and not LEditing.Composing then
  begin
    NyxEventResponse(AExecution).Consume;
    RequestNavigation(LDirection);
    Exit;
  end;

  if AEvent.HasKeyboard and not LEditing.Composing and
    TryNyxTextHistoryKey(AEvent.Keyboard, LCommand) then
  begin
    NyxEventResponse(AExecution).Consume;
    DismissCompletion;
    Execute(LCommand);
    Exit;
  end;

  if AEvent.HasKeyboard and not LEditing.Composing and (FCompletionMenu <> nil) then
  begin
    LAction := FCompletionMenu.Key(AEvent.Keyboard, Sources);

    if LAction <> pcaNone then
    begin
      NyxEventResponse(AExecution).Consume;
      CompletionAction(LAction);
    end;

    if (LAction = pcaNone) and AEvent.Keyboard.Matches(nkF12Key) then
    begin
      NyxEventResponse(AExecution).Consume;
      RequestDefinition;
    end;
  end;
end;

procedure TNyxStudioSourceInput.ConfigureCompletion(AContext: TNyxStudioCompletionContextRead;
  AOwner: TNyxStudioCompletionOwnerRead; AChanged: TNyxStudioCompletionChanged);
begin
  DismissCompletion;
  FCompletionContextRead := AContext;
  FCompletionOwnerRead := AOwner;
  FCompletionChanged := AChanged;
end;

procedure TNyxStudioSourceInput.CompletionChanged;
begin

  if FCompletionRevision = High(Integer) then
  begin
    raise ENyxTextHistory.Create('Completion presentation identity exhausted');
  end;
  Inc(FCompletionRevision);

  if Assigned(FCompletionChanged) then
  begin
    FCompletionChanged;
  end;
end;

procedure TNyxStudioSourceInput.DismissCompletion;
var
  LVisible: Boolean;
begin
  LVisible := (FCompletionQuery <> nil) or
    (FNavigationMessage <> '') or (FNavigationPending <> nil) or
    ((FCompletionMenu <> nil) and (FCompletionMenu.State <> pcvClosed));

  if FCompletionReply <> nil then
  begin
    FCompletionReply.Owner := nil;
  end;
  FCompletionReply := nil;
  FCompletionLease := nil;

  if FCompletionContext <> nil then
  begin
    FCompletionContext.Cancel;
  end;
  FCompletionContext := nil;
  FCompletionQuery := nil;
  FCompletionWaiting := False;
  FDefinitionLookup := False;
  FDefinitionPending := False;
  FNavigationPending := nil;
  FNavigationQuery := nil;
  FNavigationMessage := '';

  if FCompletionMenu <> nil then
  begin
    FCompletionMenu.Dismiss;
  end;

  if LVisible then
  begin
    CompletionChanged;
  end;
end;

function TNyxStudioSourceInput.CompletionCurrent: Boolean;
var
  LEditing: TNyxEditingSnapshot;
  LOwner: TNyxPascalSourceRef;
begin
  Result := (FCompletionQuery <> nil) and Current(LEditing);

  if Result then
  begin
    LOwner := FActive.FileReference;

    if Assigned(FCompletionOwnerRead) then LOwner := FCompletionOwnerRead();
    { An include's explicit translation unit can change without any buffer or
      history change. Re-read its current owner at presentation and queued-jump
      boundaries, rather than letting the old service query certify itself. }
    Result := FCompletionQuery.Owner.SameFile(LOwner) and
      not LEditing.Composing and not FActive.History.Composing and
      (FCompletionGeneration = FActive.History.Generation) and
      (FActive.History.Current.Text = LEditing.Text) and
      FActive.History.Current.Selection.SameRange(LEditing.Selection) and
      FCompletionQuery.FileReference.SameFile(FActive.FileReference) and
      FCompletionQuery.Matches(Sources);
  end;
end;

procedure TNyxStudioSourceInput.RequestDefinition;
var
  LEditing: TNyxEditingSnapshot;
begin

  if not Current(LEditing) then Exit;
  Observe(LEditing);
  StartCompletion(0, spqDefinition);
end;

function TNyxStudioSourceInput.NavigationPoint: TNyxPascalNavigationPoint;
var
  LFrame: TNyxTextEditorFrame;
begin
  LFrame := FActive.History.Current;

  if not LFrame.Selection.Defined then
    raise ENyxPascalNavigation.Create('Source navigation requires a known physical selection');
  { A range departs from its known start; an unknown active endpoint is never
    inferred. Definition queries themselves already require a collapsed caret. }
  Result := NyxPascalNavigationPoint(FActive.FileReference, LFrame.AuthoredText,
    LFrame.Projection.ToAuthored(LFrame.Selection.Start));
end;

procedure TNyxStudioSourceInput.NavigateBack;
begin
  RequestNavigation(pndBack);
end;

procedure TNyxStudioSourceInput.NavigateForward;
begin
  RequestNavigation(pndForward);
end;

procedure TNyxStudioSourceInput.RequestNavigation(ADirection: TNyxPascalNavigationDirection);
var
  LEditing: TNyxEditingSnapshot;
  LSources: INyxPascalSourceSnapshot;
  LCandidate: INyxPascalNavigationTrail;
  LQuery: INyxPascalCompletionRequest;
  LFrom, LTarget: TNyxPascalNavigationPoint;
  LOwner: TNyxPascalSourceRef;
  LStatus: TNyxPascalNavigationStatus;
begin
  DismissCompletion;

  if not Current(LEditing) or LEditing.Composing or FActive.History.Composing or
    not LEditing.Selection.Defined then Exit;
  Observe(LEditing);

  if FActive.History.Current.AuthoredText <> FSession.ActiveSourceText then Exit;

  if (FNavigationTrail = nil) or not FNavigationTrail.CanTravel(ADirection) then
  begin

    if ADirection = pndBack then FNavigationMessage := 'No earlier definition location.'
    else FNavigationMessage := 'No later definition location.';
    CompletionChanged;
    Exit;
  end;
  LSources := Sources;
  LFrom := NavigationPoint;
  LTarget := FNavigationTrail.Destination(ADirection);
  { SourceSnapshot.Text validates exact current membership. A removed/renamed
    project cannot be reached through a retained selector or filesystem guess. }
  LCandidate := FNavigationTrail.Travel(ADirection, LFrom,
    LSources.Text(LTarget.FileReference), LStatus);

  if LCandidate = nil then
  begin
    case LStatus of
      pnsAmbiguous: FNavigationMessage := 'That source context now occurs more than once. The location was not guessed.';
      pnsCancelled: FNavigationMessage := 'Source navigation was cancelled.';
    else
      FNavigationMessage := 'That source context has changed. Undo the edit or make a new definition jump.';
    end;
    CompletionChanged;
    Exit;
  end;
  LOwner := FActive.FileReference;

  if Assigned(FCompletionOwnerRead) then LOwner := FCompletionOwnerRead();
  LQuery := NewNyxPascalCompletionRequest(LSources)
    .At(FActive.FileReference, LFrom.Caret).From(LOwner);
  FNavigationPending := LCandidate;
  FNavigationQuery := LQuery;
  FNavigationGeneration := FActive.History.Generation;
  CompletionChanged;
end;

function TNyxStudioSourceInput.TakeSourceNavigation(out ALine, AColumn: Integer): Boolean;
var
  LPending: INyxPascalNavigationTrail;
  LQuery: INyxPascalCompletionRequest;
  LEditing: TNyxEditingSnapshot;
  LFrame: TNyxTextEditorFrame;
  LTarget: TNyxPascalNavigationPoint;
  LOwner: TNyxPascalSourceRef;
begin

  if FNavigationPending = nil then Exit(TakeDefinitionNavigation(ALine, AColumn));
  Result := False;
  ALine := 0;
  AColumn := 0;
  LPending := FNavigationPending;
  LQuery := FNavigationQuery;
  FNavigationPending := nil;
  FNavigationQuery := nil;
  { Retire any old assistance popup on a later host turn, including a refused
    one-shot move. This callback only queues; it cannot replace the input here. }
  CompletionChanged;

  if not Current(LEditing) or LEditing.Composing or FActive.History.Composing or
    (LQuery = nil) or (FActive.History.Generation <> FNavigationGeneration) then
  begin
    DismissCompletion;
    Exit;
  end;
  LFrame := FActive.History.Current;
  LOwner := FActive.FileReference;

  if Assigned(FCompletionOwnerRead) then LOwner := FCompletionOwnerRead();

  if (LFrame.Text <> LEditing.Text) or (LFrame.AuthoredText <> FSession.ActiveSourceText) or
    not LFrame.Selection.SameRange(LEditing.Selection) or
    not LQuery.Owner.SameFile(LOwner) or not LQuery.Matches(Sources) then
  begin
    DismissCompletion;
    Exit;
  end;
  LTarget := LPending.Current;
  LTarget.Position(ALine, AColumn);
  { All allocations, relocation and full-source guards precede this synchronous
    selector/candidate publication. No callback or renderer is retired inline. }
  DismissCompletion;
  FSession.ActiveSourceFile := LTarget.FileReference;
  FNavigationTrail := LPending;
  Result := True;
end;

procedure TNyxStudioSourceInput.StartCompletion(AOffset: Integer;
  AKind: TNyxStudioPascalQueryKind);
var
  LEditing: TNyxEditingSnapshot;
  LOwner: TNyxPascalSourceRef;
  LTicket: TNyxPascalCompletionTicket;
  LDefinitions: INyxPascalDefinitionMenu;
begin
  DismissCompletion;

  if not Current(LEditing) or LEditing.Composing or not LEditing.Selection.Defined or
    not LEditing.Selection.Collapsed or (FCompletionMenu = nil) then
  begin
    Exit;
  end;
  LOwner := FActive.FileReference;

  if Assigned(FCompletionOwnerRead) then
  begin
    LOwner := FCompletionOwnerRead();
  end;
  FDefinitionLookup := AKind = spqDefinition;

  if FDefinitionLookup then
  begin

    if not Supports(FCompletionMenu, INyxPascalDefinitionMenu, LDefinitions) then Exit;
    FCompletionQuery := NyxPascalDefinitionQuery(LDefinitions.BeginDefinitionQuery(
      Sources, FActive.FileReference, LOwner, LTicket, AOffset));
  end
  else
  begin
    FCompletionQuery := FCompletionMenu.BeginQuery(Sources, FActive.FileReference, LOwner,
      LTicket, AOffset);
  end;
  FCompletionGeneration := FActive.History.Generation;
  FCompletionReply := TNyxStudioSourceCompletionReply.Create;
  FCompletionLease := FCompletionReply;
  FCompletionReply.Owner := Self;
  FCompletionReply.Ticket := LTicket;

  if Assigned(FCompletionContextRead) then
  begin
    FCompletionContext := FCompletionContextRead();
  end;
  SubmitCompletion;
  CompletionChanged;
end;

procedure TNyxStudioSourceInput.SubmitCompletion;
var
  LReadiness: TNyxStudioCompletionReadiness;
  LWasWaiting: Boolean;
  LMessage: TNyxText;
  LReply: TNyxStudioSourceCompletionReply;
  LDefinitions: INyxStudioDefinitionContext;
  LMenu: INyxPascalDefinitionMenu;
  LUnavailable: INyxPascalCompletionResult;
begin

  if not CompletionCurrent or (FCompletionReply = nil) then
  begin
    DismissCompletion;
    Exit;
  end;
  LWasWaiting := FCompletionWaiting;
  LReadiness := scrOffline;
  LReply := FCompletionReply;

  if FCompletionContext <> nil then
  begin

    if FDefinitionLookup then
    begin
      LReadiness := scrUnavailable;

      if Supports(FCompletionContext, INyxStudioDefinitionContext, LDefinitions) then
      begin
        LReadiness := LDefinitions.RequestDefinition(
          NyxPascalDefinitionRequest(FCompletionQuery), FCompletionLease);
      end;
    end
    else
    begin
      LReadiness := FCompletionContext.Request(FCompletionQuery, FCompletionLease);
    end;
  end;
  FCompletionWaiting := LReadiness = scrSynchronizing;

  if LWasWaiting <> FCompletionWaiting then
  begin
    CompletionChanged;
  end;

  if LReadiness in [scrOffline, scrUnavailable] then
  begin
    LMessage := 'Connect Studio to its local service for Pascal completion';

    if LReadiness = scrUnavailable then
    begin
      LMessage := 'Pascal completion is unavailable for this editor context';
    end;

    if FDefinitionLookup then
    begin
      LMessage := 'Connect Studio to its local service for Pascal definition lookup';

      if LReadiness = scrUnavailable then
      begin
        LMessage := 'Pascal definition lookup is unavailable for this editor context';
      end;
    end;
    LUnavailable := NewNyxPascalCompletionResult(FCompletionQuery, pcsUnavailable, LMessage, 0, '', [], 0);

    if FDefinitionLookup and Supports(FCompletionMenu, INyxPascalDefinitionMenu, LMenu) then
    begin
      LMenu.PresentDefinition(LReply.Ticket,
        NyxPascalDefinitionResult(NyxPascalDefinitionRequest(FCompletionQuery), LUnavailable), Sources);
    end
    else
    begin
      FCompletionMenu.Present(LReply.Ticket, LUnavailable, Sources);
    end;
    LReply.Owner := nil;
    FCompletionReply := nil;
    FCompletionLease := nil;
  end;
end;

procedure TNyxStudioSourceInput.CompletionReply(AReply: TNyxStudioSourceCompletionReply;
  const AQuery: INyxPascalCompletionRequest; const AResult: INyxPascalCompletionResult);
var
  LMenu: INyxPascalDefinitionMenu;
  LResult: INyxPascalDefinitionResult;
  LPresented: Boolean;
begin

  if AReply <> FCompletionReply then
  begin
    Exit;
  end;

  if not CompletionCurrent or (FCompletionContext = nil) or
    not FCompletionContext.Current(AQuery) then
  begin
    DismissCompletion;
    Exit;
  end;

  if FDefinitionLookup and Supports(FCompletionMenu, INyxPascalDefinitionMenu, LMenu) then
  begin
    LResult := NyxPascalDefinitionResult(NyxPascalDefinitionRequest(FCompletionQuery), AResult);
    LPresented := LMenu.PresentDefinition(AReply.Ticket, LResult, Sources);

    if LPresented and (LResult.Count = 1) and (LResult.Total = 1) and
      LResult.Target(0).Location.Available then
    begin
      FDefinitionRow := 0;
      FDefinitionPending := True;
    end;
  end
  else
  begin
    LPresented := FCompletionMenu.Present(AReply.Ticket, AResult, Sources);
  end;

  if LPresented then
  begin
    FCompletionWaiting := False;
    AReply.Owner := nil;
    FCompletionReply := nil;
    FCompletionLease := nil;
    CompletionChanged;
  end;
end;

procedure TNyxStudioSourceInput.CompletionAction(AAction: TNyxPascalCompletionAction);
var
  LOffset: Integer;
  LSources: INyxPascalSourceSnapshot;
  LCandidate: INyxPascalSourceSnapshot;
  LPreparation: INyxPascalCompletionPreparation;
  LPreparedDrafts: TNyxPascalFileDrafts;
  LKind: TNyxStudioPascalQueryKind;
begin
  case AAction of
    pcaRequest: StartCompletion;
    pcaDismiss: DismissCompletion;
    pcaSelect: CompletionChanged;
    pcaDefinition:
      begin
        FDefinitionRow := FCompletionMenu.Selected;
        FDefinitionPending := True;
        CompletionChanged;
      end;
    pcaUndo:
      begin
        DismissCompletion;
        Execute(nhcUndo);
      end;
    pcaRedo:
      begin
        DismissCompletion;
        Execute(nhcRedo);
      end;
    pcaPrevious, pcaNext:
      begin

        if not CompletionCurrent then
        begin
          DismissCompletion;
          Exit;
        end;
        LOffset := FCompletionQuery.Offset - FCompletionQuery.Limit;

        if AAction = pcaNext then
        begin
          LOffset := FCompletionQuery.Offset + FCompletionQuery.Limit;
        end;

        if LOffset < 0 then
        begin
          LOffset := 0;
        end;
        LKind := spqCompletion;

        if FDefinitionLookup then LKind := spqDefinition;
        StartCompletion(LOffset, LKind);
      end;
    pcaAccept:
      begin

        if not CompletionCurrent or (FCompletionContext = nil) or
          not FCompletionContext.Current(FCompletionQuery) then
        begin
          DismissCompletion;
          Exit;
        end;
        LSources := Sources;

        if not Supports(FCompletionMenu, INyxPascalCompletionPreparation, LPreparation) then
        begin
          raise ENyxTextHistory.Create('Completion requires detached draft preparation');
        end;
        LCandidate := LPreparation.Preview(LSources);

        if FActive.FileReference.Role <> npsMain then
        begin
          { Validate helper draft membership/size before the text timeline moves.
            No reentrant callback occurs between this preparation and admission. }
          LPreparedDrafts := FSession.FileDrafts.Editing(FActive.FileReference,
            FSession.SourceFileText(FActive.FileReference), LCandidate.Text(FActive.FileReference));
          LPreparedDrafts.ValidateAgainst(FProject);
        end;
        LCandidate := FCompletionMenu.Accept(LSources);
        FSession.SetFileDraft(FActive.FileReference, LCandidate.Text(FActive.FileReference));
        FActive.AuthorText := FSession.ActiveSourceText;
        DismissCompletion;
        FWrite(FActive.History.Current, True);
      end;
    pcaNone:
      begin
        { Unhandled input retains its ordinary physical platform behavior. }
      end;
  end;
end;

function TNyxStudioSourceInput.TakeDefinitionNavigation(
  out ALine, AColumn: Integer): Boolean;
var
  LNavigation: INyxPascalCompletionNavigation;
  LLocation: TNyxPascalCompletionLocation;
  LTarget: TNyxPascalNavigationPoint;
  LTrail: INyxPascalNavigationTrail;
begin
  Result := False;
  ALine := 0;
  AColumn := 0;

  if not FDefinitionPending then
  begin
    Exit;
  end;
  FDefinitionPending := False;

  if not CompletionCurrent or (FCompletionContext = nil) or
    not FCompletionContext.Current(FCompletionQuery) or
    (Assigned(FCompletionContextRead) and (FCompletionContextRead() <> FCompletionContext)) or
    (FCompletionMenu.Selected <> FDefinitionRow) or
    not Supports(FCompletionMenu, INyxPascalCompletionNavigation, LNavigation) then
  begin
    DismissCompletion;
    Exit;
  end;
  LLocation := LNavigation.SelectedDefinition(Sources);

  if not LLocation.Available then
  begin
    Exit;
  end;
  LTarget := NyxPascalNavigationPoint(LLocation.FileReference,
    FSession.DraftFileText(LLocation.FileReference), LLocation.Start);
  LTarget.Position(ALine, AColumn);
  LTrail := FNavigationTrail.Visit(NavigationPoint, LTarget);
  { SourceFileText validates current membership before the UI selector changes.
    The query already checks every accepted/draft buffer, including this target.
    Stored per-file text histories and unfinished buffers remain independent. }
  DismissCompletion;
  FSession.ActiveSourceFile := LLocation.FileReference;
  FNavigationTrail := LTrail;
  Result := True;
end;

function TNyxStudioSourceInput.BuildCompletion: TNyxDocument;
var
  LCard: INyxCard;
  LEditing: TNyxEditingSnapshot;
begin
  Result := nil;

  if FNavigationPending <> nil then Exit;

  if FNavigationMessage <> '' then
  begin

    if not Current(LEditing) then
    begin
      DismissCompletion;
      Exit;
    end;
    LCard := NewNyxCard('studio-completion');
    LCard.Configure.Padding(16).Gap(8).Done;
    LCard.Add(NewNyxLabel('studio-navigation-title').WithText('Source navigation'));
    LCard.Add(NewNyxLabel('studio-navigation-message').WithText(FNavigationMessage));
  end
  else
  begin

    if not CompletionCurrent then
    begin
      DismissCompletion;
      Exit;
    end;
    LCard := FCompletionMenu.Build('studio-completion', Sources);

    if FCompletionMenu.State = pcvClosed then
    begin
      Exit;
    end;

    if FCompletionWaiting then
    begin
      LCard.Add(NewNyxLabel('studio-completion-waiting')
        .WithText('Waiting for this Pascal draft to synchronize'));
    end;
  end;
  Result := TNyxDocument.Create;
  try
    Result.AddPage(LCard);
  except
    Result.Free;
    raise;
  end;
end;

function TNyxStudioSourceInput.RouteCompletion(ARoot, ANode: TNyxNode;
  ATrigger: TNyxTrigger): Boolean;
var
  LAction: TNyxPascalCompletionAction;
begin
  Result := False;

  if not CompletionCurrent then
  begin
    DismissCompletion;
    Exit;
  end;
  LAction := FCompletionMenu.Route('studio-completion', ARoot, ANode, ATrigger, Sources);
  Result := LAction <> pcaNone;

  if Result then
  begin
    CompletionAction(LAction);
  end;
end;

function TNyxStudioSourceInput.CompletionDismissReceiver: INyxEventCallback;
begin
  Result := FPort;
end;

function TNyxStudioSourceInput.Sources: INyxPascalSourceSnapshot;
var
  LEditing: TNyxEditingSnapshot;
  LDrafts: TNyxPascalFileDrafts;
  LIndex: Integer;
begin

  if not Current(LEditing) then
  begin
    raise ENyxTextHistory.Create('Source input context changed');
  end;
  Result := CaptureNyxPascalSources(FProject)
    .WithBuffer(TNyxPascalSourceRef.Main, FSession.DraftSource);
  LDrafts := FSession.FileDrafts;
  for LIndex := 0 to LDrafts.Count - 1 do
  begin
    Result := Result.WithBuffer(LDrafts.Item(LIndex).FileReference, LDrafts.Item(LIndex).Text);
  end;
end;

function TNyxStudioSourceInput.CanExecute(ACommand: TNyxTextHistoryCommand): Boolean;
var
  LEditing: TNyxEditingSnapshot;
begin
  Result := Current(LEditing) and not LEditing.Composing and not FActive.History.Composing;

  if Result then
  begin

    if ACommand = nhcUndo then
    begin
      Result := FActive.History.CanUndo;
    end
    else
    begin
      Result := FActive.History.CanRedo;
    end;
  end;
end;

procedure TNyxStudioSourceInput.Execute(ACommand: TNyxTextHistoryCommand);
var
  LFrame: TNyxTextEditorFrame;
  LCandidate: INyxPascalSourceSnapshot;
begin

  if not CanExecute(ACommand) then
  begin
    Exit;
  end;

  if ACommand = nhcUndo then
  begin
    FActive.History.UndoFrame(LFrame);
  end
  else
  begin
    FActive.History.RedoFrame(LFrame);
  end;
  { Stage complete source/budget admission before moving either text stack. }
  LCandidate := Sources.WithBuffer(FActive.FileReference, LFrame.AuthoredText);
  FSession.SetFileDraft(FActive.FileReference, LCandidate.Text(FActive.FileReference));
  FActive.AuthorText := FSession.ActiveSourceText;

  if ACommand = nhcUndo then
  begin
    FActive.History.Undo;
  end
  else
  begin
    FActive.History.Redo;
  end;
  FWrite(FActive.History.Current, True);
end;

end.
