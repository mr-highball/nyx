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
program nyx_draft_capture_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}Web,{$else}Interfaces,{$endif}
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.types, nyx.controls,
  nyx.codec, nyx.codegen, nyx.studio.session, nyx.studio.projects,
  nyx.studio.agents, nyx.studio.agentbridge, nyx.studio.workspaces,
  nyx.test.editor.exchange, nyx.test.hierarchy;

type
  { The persistence observer deliberately owns only exact bytes. It must not
    retain the controller/session or reread their trees during notification. }
  TCaptureObserver = class
  public
    Count: Integer;
    Last: TNyxText;
    Refreshes: Integer;
    ThrowRefresh: Boolean;
    procedure Captured(const AProject: TNyxText);
    procedure Refreshed(AContentChanged: Boolean);
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TCaptureObserver.Captured(const AProject: TNyxText);
begin
  Inc(Count);
  Last := AProject;
end;

procedure TCaptureObserver.Refreshed(AContentChanged: Boolean);
begin
  Inc(Refreshes);

  if ThrowRefresh then
  begin
    raise Exception.Create('Owned presentation failure');
  end;
end;

function SharedPair(ASession: TNyxAgentSession): TNyxProjectPair;
var
  LState: TNyxDataValue;
begin
  LState := ASession.Exchange(NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
  Result := DecodeNyxProject(LState.Field('project').AsText);
end;

procedure Drain(ABridge: TNyxStudioAgentBridge; AExchange: TNyxTestEditorExchange);
var
  LIndex: Integer;
begin
  for LIndex := 1 to 8 do
  begin

    if AExchange.RequestPending then
    begin
      AExchange.Deliver;
    end;

    if ABridge.SourceSynchronized or ABridge.State.Conflict then
    begin
      Exit;
    end;
    AExchange.FireTick;
  end;
  raise Exception.Create('The bounded local publications did not settle');
end;

procedure CaptureJourney;
var
  LSession: TNyxStudioSession;
  LServer: TNyxAgentSession;
  LExchange: TNyxTestEditorExchange;
  LBridge: TNyxStudioAgentBridge;
  LObserver: TCaptureObserver;
  LBefore: TNyxText;
  LDraft: TNyxText;
  LPair: TNyxProjectPair;
  LIndex: Integer;
  LCount: Integer;
  LSchedules: Integer;
  LPosts: Integer;
  LShared: TNyxDataValue;
begin
  LSession := TNyxStudioSession.Create;
  LServer := TNyxAgentSession.Create;
  LObserver := TCaptureObserver.Create;
  LBridge := nil;
  try
    LExchange := TNyxTestEditorExchange.Create(LServer);
    LBridge := TNyxStudioAgentBridge.Create(LSession, nil, NyxPrimaryWorkspace, LExchange);
    LBridge.OnProjectCaptured := LObserver.Captured;
    LBridge.Connect;
    LExchange.Deliver;
    Check(LBridge.SourceSynchronized, 'Initial exact private-protocol pair is acknowledged');
    LBefore := LSession.Source;
    LCount := LObserver.Count;
    LSchedules := LExchange.Schedules;
    LPosts := LExchange.Posts;
    for LIndex := 1 to 80 do
    begin
      LDraft := LBefore + #10 + TNyxText('// Private input ') + IntToStr(LIndex) +
        TNyxText(' / 🌙 漢字');
      LSession.SetSourceDraft(LDraft);
      LBridge.RecordDraft;
    end;
    Check((LObserver.Count = LCount) and (LExchange.Posts = LPosts),
      'Eighty synchronous keystrokes capture/post no whole project');
    Check((LExchange.Schedules = LSchedules + 1) and (LExchange.Delay = 250),
      'Continued typing cannot postpone the first owned capture window');
    Check(LSession.DraftSource = LDraft, 'Local exact supplementary text is immediate');
    Check(LBridge.DraftCapturePending and not LBridge.SourceSynchronized and
      not LBridge.CanSwitchWorkspace, 'An unsent draft cannot grant source or navigation authority');
    Check(Pos('waiting', LBridge.State.Status) > 0, 'Observing users can see the local pending state');
    LExchange.FireTick;
    Check((LObserver.Count = LCount + 1) and LExchange.RequestPending,
      'One fresh snapshot feeds persistence and a single paired commit');
    LPair := DecodeNyxProject(LObserver.Last);
    Check(LPair.Pending and (LPair.Draft = LDraft) and (LPair.DraftBase = LBefore) and
      (LPair.Source = LBefore), 'Captured recovery owns the exact accepted pair and original draft base');
    LExchange.PrepareReply;
    LDraft := LDraft + #10 + '// Typed while the acknowledgement was in flight';
    LSession.SetSourceDraft(LDraft);
    LBridge.RecordDraft;
    LExchange.Deliver;
    Check((LSession.DraftSource = LDraft) and LBridge.DraftCapturePending,
      'An older commit acknowledgement cannot replace later unsent typing');
    Drain(LBridge, LExchange);
    LPair := SharedPair(LServer);
    Check((LExchange.Commits = 2) and (LPair.Draft = LDraft) and LBridge.SourceSynchronized,
      'The next capture preserves the newest draft after the in-flight head retires');
    LShared := LServer.Call('nyx_session', 'Capture qualification', NyxObject([]));
    Check(not LShared.Field('canUndo').AsBoolean,
      'Draft publications do not fabricate accepted-content history');

    { Discard is still a publication boundary. The original pending draft base
      and baseline must disappear together on the authoritative pair. }
    LSession.DiscardSourceDraft;
    LBridge.RecordLocal;
    Drain(LBridge, LExchange);
    LPair := SharedPair(LServer);
    Check(not LPair.Pending and (LPair.Source = LBefore),
      'Explicit restore publishes the exact accepted pair without an extra Undo step');

    { Force an ordinary complete, unchanged observation to arrive after typing
      but before its capture window. Empty queue alone cannot authorize adoption. }
    LExchange.FireTick;
    LExchange.PrepareReply(True);
    LDraft := LBefore + #10 + '// Unsent draft before an unchanged observation';
    LSession.SetSourceDraft(LDraft);
    LBridge.RecordDraft;
    LCount := LObserver.Count;
    LExchange.Deliver;
    Check((LSession.DraftSource = LDraft) and LBridge.DraftCapturePending and
      (LObserver.Count = LCount), 'A complete unchanged observation preserves unsent local text');
    Drain(LBridge, LExchange);

    LSession.DiscardSourceDraft;
    LBridge.RecordLocal;
    Drain(LBridge, LExchange);
    LExchange.FireTick;
    LDraft := LBefore + #10 + '// Local draft concurrent with a semantic edit';
    LSession.SetSourceDraft(LDraft);
    LBridge.RecordDraft;
    LServer.Call('nyx_transaction', 'Capture qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LServer.Revision)),
      NyxField('operationId', NyxData('capture-conflict')),
      NyxField('operations', NyxArray([
        NyxObject([NyxField('op', NyxData('title')),
          NyxField('value', NyxData('A concurrent English project'))])]))]));
    LExchange.Deliver;
    Check(LBridge.State.Conflict and (LSession.DraftSource = LDraft) and
      (LSession.Source = LBefore), 'A concurrent semantic revision retains both local files and draft');
    Check(not LBridge.CanSwitchWorkspace, 'Conflict cannot grant a project jump');
    LBridge.AcceptRemote;
    LExchange.Deliver;
    Check(not LBridge.State.Conflict and not LBridge.DraftCapturePending and
      (LSession.Document.Title = 'A concurrent English project'),
      'Only explicit operator resolution adopts the concurrent pair');

    { Accepted commands must never be coalesced like unsent typing. }
    LSession.SetTitle('First accepted title');
    LBridge.RecordLocal;
    LSession.SetTitle('Second accepted title');
    LBridge.RecordLocal;
    Drain(LBridge, LExchange);
    LBridge.History(nehUndo);
    Drain(LBridge, LExchange);
    Check(LSession.Document.Title = 'First accepted title',
      'Ordered accepted title commands remain separate authoritative Undo entries');
    LBridge.History(nehRedo);
    Drain(LBridge, LExchange);
    Check(LSession.Document.Title = 'Second accepted title',
      'Authoritative Redo restores the exact second pair');

    LBefore := LSession.Source;
    LDraft := LBefore + #10 + '// Recovery continues while sharing is paused';
    LSession.SetSourceDraft(LDraft);
    LBridge.RecordDraft;
    LBridge.Pause;
    LPosts := LExchange.Posts;
    LCount := LObserver.Count;
    LExchange.FireTick;
    LPair := DecodeNyxProject(LObserver.Last);
    Check((LObserver.Count = LCount + 1) and (LPair.Draft = LDraft) and
      (LExchange.Posts = LPosts), 'Paused sharing still persists its local draft without HTTP admission');
    Check(not LBridge.DraftCapturePending and not LBridge.SourceSynchronized,
      'Local persistence alone does not manufacture shared acknowledgement');
  finally
    LBridge.Free;
    LObserver.Free;
    LServer.Free;
    LSession.Free;
  end;
end;

procedure FailureJourney;
var
  LSession: TNyxStudioSession;
  LServer: TNyxAgentSession;
  LBridge: TNyxStudioAgentBridge;
  LExchange: TNyxTestEditorExchange;
  LObserver: TCaptureObserver;
  LBefore: TNyxText;
  LLast: TNyxText;
begin
  LSession := TNyxStudioSession.Create;
  LServer := TNyxAgentSession.Create;
  LObserver := TCaptureObserver.Create;
  LBridge := nil;
  try
    LExchange := TNyxTestEditorExchange.Create(LServer);
    LBridge := TNyxStudioAgentBridge.Create(LSession, nil, NyxPrimaryWorkspace, LExchange);
    LBridge.OnProjectCaptured := LObserver.Captured;
    LBridge.Connect;
    LExchange.Deliver;
    LBefore := LSession.Source;
    LLast := LObserver.Last;
    LExchange.FireTick;
    LExchange.PrepareReply(True);
    LSession.SetSourceDraft(LBefore + #10 + '// Retain me on failed capture');
    LBridge.RecordDraft;
    LSession.Document.Find('welcome-title').Named('welcome-description');
    LExchange.FireTick;
    Check(LBridge.State.Conflict and LBridge.DraftCapturePending,
      'A malformed direct mutation refuses timer capture and retains its marker');
    Check(LObserver.Last = LLast, 'Capture failure never overwrites the last readable recovery');
    Check(not LExchange.TickPending, 'Failed capture does not spin another timer');
    LExchange.Deliver;
    Check(LBridge.State.Conflict and
      (LSession.DraftSource = LBefore + #10 + '// Retain me on failed capture'),
      'A late older reply cannot clear the capture refusal or discard its draft');
    LSession.Document.Find('welcome-card').Children[0].Named('welcome-title');
    Check(LSession.DraftSource = LBefore + #10 + '// Retain me on failed capture',
      'Repair retains the exact private input and its original base');
    LBridge.RecordLocal;
    Check(DecodeNyxProject(LObserver.Last).Pending,
      'Explicit repaired capture can persist local text while conflict remains unresolved');
  finally
    LBridge.Free;
    LObserver.Free;
    LServer.Free;
    LSession.Free;
  end;
end;

{ A redundant local capture has no new server revision. Its status still needs
  presentation, and even a failing receiver must not prevent protocol dispatch
  or turn the valid captured pair into a refusal/history mutation. }
procedure PresentationJourney;
var
  LSession: TNyxStudioSession;
  LServer: TNyxAgentSession;
  LBridge: TNyxStudioAgentBridge;
  LExchange: TNyxTestEditorExchange;
  LObserver: TCaptureObserver;
  LBefore: TNyxText;
  LRevision: Integer;
  LRefreshes: Integer;
  LThrown: Boolean;
begin
  LSession := TNyxStudioSession.Create;
  LServer := TNyxAgentSession.Create;
  LObserver := TCaptureObserver.Create;
  LBridge := nil;
  try
    LExchange := TNyxTestEditorExchange.Create(LServer);
    LBridge := TNyxStudioAgentBridge.Create(LSession, LObserver.Refreshed,
      NyxPrimaryWorkspace, LExchange);
    LBridge.Connect;
    LExchange.Deliver;
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LRevision := LServer.Revision;
    LRefreshes := LObserver.Refreshes;
    LSession.SetSourceDraft(LSession.Source);
    LBridge.RecordDraft;
    LObserver.ThrowRefresh := True;
    LThrown := False;
    try
      LExchange.FireTick;
    except
      on Exception do
      begin
        LThrown := True;
      end;
    end;
    Check(LThrown and (LObserver.Refreshes = LRefreshes + 1),
      'Redundant capture publishes its local status transition exactly once');
    Check(LExchange.RequestPending and not LBridge.State.Conflict and
      not LBridge.DraftCapturePending,
      'A throwing presentation receiver cannot strand dispatch or falsify capture refusal');
    LObserver.ThrowRefresh := False;
    LExchange.Deliver;
    Check(LBridge.SourceSynchronized and (LServer.Revision = LRevision) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'Status-only capture/acknowledgement preserves revision and complete project/history meaning');
  finally
    LBridge.Free;
    LObserver.Free;
    LServer.Free;
    LSession.Free;
  end;
end;

begin
  try
    CaptureJourney;
    FailureJourney;
    PresentationJourney;
    Inc(GChecks, RunNyxStudioHierarchyTests);
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' draft capture/protocol checks';
    document.body.setAttribute('data-nyx-draft-capture', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' draft capture/protocol checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-draft-capture', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
