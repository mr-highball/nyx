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
  nyx.editing.commands, nyx.editing.history, nyx.pascal.project,
  nyx.pascal.sources, nyx.pascal.drafts, nyx.studio.session;

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
  { Forward owner for managed input callbacks; the full class owns timelines and
    subscriptions, while the callback only borrows it until explicit detach. }
  TNyxStudioSourceInput = class;
  { One selected-file timeline. AuthorText detects external paired/draft changes
    independently of physical native line-ending normalization. }
  TNyxStudioSourceBuffer = class
  public
    FileReference: TNyxPascalSourceRef;
    AuthorText: TNyxText;
    { Initial authored bytes may use different line endings from their physical
      native projection. Returning to that text restores the original bytes. }
    BaselineAuthor: TNyxText;
    Baseline: TNyxTextEditorFrame;
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
    FPort: TNyxStudioSourceInputPort;
    FLease: INyxTextHistoryCommands;
    FSubscriptions: array of INyxEventSubscription;
    procedure Clear;
    function Current(out AEditing: TNyxEditingSnapshot): Boolean;
    procedure Event(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
    function CanExecute(ACommand: TNyxTextHistoryCommand): Boolean;
    procedure Execute(ACommand: TNyxTextHistoryCommand);
  public
    { Subscribe only to the retained source view; callbacks are UI-thread only.
      The host methods outlive this controller and must identify their live mount. }
    constructor Create(const AEvents: INyxEvents;
      ARead: TNyxStudioSourceRead; AWrite: TNyxStudioSourceWrite);
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
    property Commands: INyxTextHistoryCommands read FLease;
  end;

implementation

uses SysUtils;

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
    Owner.Event(AEvent, AExecution);
  end;
end;

procedure TNyxStudioSourceInputPort.BeforeEdit(const AEditing: TNyxEditingSnapshot);
var
  LCurrent: TNyxEditingSnapshot;
begin

  if (Owner <> nil) and Owner.Current(LCurrent) and
    (LCurrent.Text = AEditing.Text) and LCurrent.Selection.SameRange(AEditing.Selection) then
  begin
    Owner.FActive.History.Observe(NyxTextEditorFrame(LCurrent.Text, LCurrent.Selection),
      LCurrent.Composing);
  end;
end;

constructor TNyxStudioSourceInput.Create(const AEvents: INyxEvents;
  ARead: TNyxStudioSourceRead; AWrite: TNyxStudioSourceWrite);
var
  LTarget: TNyxEventTarget;
begin
  inherited Create;

  if (AEvents = nil) or not Assigned(ARead) or not Assigned(AWrite) then
  begin
    raise EArgumentException.Create('Source input requires its retained view and UI owner');
  end;
  AEvents.Scheduler.RequireUI;
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
      LBuffer.BaselineAuthor := LBuffer.AuthorText;
      LBuffer.Baseline := NyxTextEditorFrame(LEditing.Text, LEditing.Selection);
      LBuffer.History := NewNyxTextEditHistory(NyxTextEditorFrame(LEditing.Text, LEditing.Selection));

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
  FActive := LBuffer;

  if FActive.AuthorText <> FSession.ActiveSourceText then
  begin
    FActive.History.Reset(NyxTextEditorFrame(LEditing.Text, LEditing.Selection));
    FActive.AuthorText := FSession.ActiveSourceText;
    FActive.BaselineAuthor := FActive.AuthorText;
    FActive.Baseline := FActive.History.Current;
  end
  else if not LEditing.Composing and (FActive.History.Current.Text <> LEditing.Text) then
  begin
    { Reparenting may recreate a native handle and normalize its line endings.
      Restore the owned physical frame without publishing another draft. }
    FWrite(FActive.History.Current, False);
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
  LText: TNyxText;
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
  LText := LEditing.Text;

  if LText = FActive.Baseline.Text then
  begin
    LText := FActive.BaselineAuthor;
  end;
  FSession.SetFileDraft(FActive.FileReference, LText);
  FActive.AuthorText := FSession.ActiveSourceText;
  FActive.History.Observe(NyxTextEditorFrame(LEditing.Text, LEditing.Selection), LEditing.Composing);
end;

procedure TNyxStudioSourceInput.Event(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LEditing: TNyxEditingSnapshot;
  LCommand: TNyxTextHistoryCommand;
begin

  if not Current(LEditing) then
  begin

    if AEvent.HasKeyboard and TryNyxTextHistoryKey(AEvent.Keyboard, LCommand) then
    begin
      NyxEventResponse(AExecution).Consume;
    end;
    Exit;
  end;
  FActive.History.Observe(NyxTextEditorFrame(LEditing.Text, LEditing.Selection), LEditing.Composing);

  if AEvent.HasKeyboard and not LEditing.Composing and
    TryNyxTextHistoryKey(AEvent.Keyboard, LCommand) then
  begin
    NyxEventResponse(AExecution).Consume;
    Execute(LCommand);
  end;
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
  LText: TNyxText;
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
  LText := LFrame.Text;

  if LText = FActive.Baseline.Text then
  begin
    LText := FActive.BaselineAuthor;
  end;
  LCandidate := Sources.WithBuffer(FActive.FileReference, LText);
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
