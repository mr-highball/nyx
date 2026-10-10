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
program nyx_studio_recovery_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, Process, md5, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.codec,
  nyx.studio.directories, nyx.studio.mcp, nyx.studio.outputs,
  nyx.studio.projects, nyx.studio.workspaces, nyx.generated.view,
  nyx.studio.agents, nyx.studio.session, nyx.studio.recovery;

var
  GEngine: TNyxStudioMCP;
  GDirectories: TNyxStudioDirectories;
  GToken: TNyxText;
  GSeed: TNyxProjectPair;
  GProfile: TNyxText;
  GChecks: Integer;
  GSourceDirectory: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
  WriteLn('Check ', GChecks, ': ', AReason);
end;

function ReadBytes(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, LStream.Size);
    SetCodePage(RawByteString(Result), CP_UTF8, False);

    if LStream.Size > 0 then
    begin
      LStream.ReadBuffer(Result[1], LStream.Size);
    end;
  finally
    LStream.Free;
  end;
end;

procedure WriteBytes(const APath, ABytes: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try
    LStream.WriteBuffer(ABytes[1], Length(ABytes));

    if not FileFlush(LStream.Handle) then
    begin
      raise Exception.Create('Cannot flush the owned process readiness frame');
    end;
  finally
    LStream.Free;
  end;
end;

{ Deployment qualification consumes an explicitly copied private checkpoint and
  bounded authenticated observing baseline. The frozen candidate must admit all
  accepted/history pairs before any replacement of the live host. Only the copied
  runtime is opened or rewritten: no enrollment, listener, compiler or live editor
  request is owned here. Byte-exact Save/Load establishes the complete history,
  naming counters and registry identity beyond public CanUndo/CanRedo summaries. }
procedure VerifyRetainedCheckpoint(const ARelease, ARuntime, ABaseline: TNyxText);
const
  CFields: array[0..6] of TNyxText = ('selection', 'revision', 'canUndo',
    'view', 'canRedo', 'pendingDraft', 'permission');
var
  LDirectories: TNyxStudioDirectories;
  LStore: TNyxStudioRuntimeStore;
  LPrimary: TNyxAgentSession;
  LWorkspaces: TNyxStudioWorkspaces;
  LSession: TNyxAgentSession;
  LStream: TFileStream;
  LBaselineText: TNyxText;
  LBaseline: TNyxDataValue;
  LExpected: TNyxDataValue;
  LActual: TNyxDataValue;
  LRows: TNyxDataValue;
  LBefore: TNyxText;
  LReference: TNyxWorkspaceRef;
  LIndex: Integer;
  LField: Integer;
  LRow: Integer;
  LMatches: Integer;
  LPrior: Integer;
begin
  LStream := TFileStream.Create(ABaseline, fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size <= 4 * 1024 * 1024),
      'Retained observing baseline stays within its private input bound');
    SetLength(LBaselineText, LStream.Size);
    LStream.ReadBuffer(LBaselineText[1], Length(LBaselineText));
  finally
    LStream.Free;
  end;
  LBaseline := TNyxDataValue.ParseJSON(LBaselineText);
  Check((LBaseline.Kind = ndArray) and (LBaseline.Count in [1..9]) and
    (LBaseline.Item(0).Field('workspace').AsText = ''),
    'Retained baseline identifies the primary and bounded ordinary registry');
  { A duplicate expected handle must not let one project stand in for another
    while the counts still agree. Admit the complete baseline before opening the
    copied store; project identity is exact and case sensitive. }
  for LIndex := 0 to LBaseline.Count - 1 do
  begin
    for LPrior := 0 to LIndex - 1 do
    begin

      if LBaseline.Item(LIndex).Field('workspace').AsText =
        LBaseline.Item(LPrior).Field('workspace').AsText then
      begin
        raise Exception.Create('Retained baseline contains a duplicate project handle');
      end;
    end;
  end;
  LDirectories := TNyxStudioDirectories.ForRelease(ARelease, ARuntime);
  Check(FileExists(LDirectories.SessionCheckpoint),
    'Retained qualification requires an explicit copied checkpoint');
  LBefore := ReadBytes(LDirectories.SessionCheckpoint);
  LStore := nil;
  LPrimary := nil;
  LWorkspaces := nil;
  try
    LStore := TNyxStudioRuntimeStore.Create(LDirectories);
    Check(LStore.Load(LPrimary, LWorkspaces),
      'Frozen candidate admits all retained accepted and full-history pairs');
    LRows := LWorkspaces.Observe;
    Check(LRows.Count = LBaseline.Count,
      'Candidate retains the exact ordinary project count');
    for LIndex := 0 to LBaseline.Count - 1 do
    begin
      LExpected := LBaseline.Item(LIndex);
      LReference := NyxPrimaryWorkspace;

      if LIndex > 0 then
      begin
        LReference := NyxWorkspace(LExpected.Field('workspace').AsText);
      end;
      LSession := LWorkspaces.Find(LReference);
      Check(LSession <> nil, 'Candidate resolves the exact retained project handle');
      LActual := LSession.Exchange(NyxObject([NyxField('op', NyxData('observe')),
        NyxField('after', NyxData(0))]));
      Check(LActual.Field('project').AsText = LExpected.Field('project').AsText,
        'Candidate retains the complete accepted pair and pending draft/base');
      for LField := 0 to High(CFields) do
      begin
        Check(LActual.Field('session').Field(CFields[LField]).ToJSON =
          LExpected.Field(CFields[LField]).ToJSON,
          'Candidate retains ' + CFields[LField]);
      end;
      LMatches := 0;
      for LRow := 0 to LRows.Count - 1 do
      begin

        if LRows.Item(LRow).Field('workspace').AsText = LReference.ID then
        begin
          Inc(LMatches);
          Check(LRows.Item(LRow).Field('label').AsText = LExpected.Field('label').AsText,
            'Candidate retains the exact project label');
        end;
      end;
      Check(LMatches = 1, 'Candidate retains one exact registry identity per project');
    end;
    Check(ReadBytes(LDirectories.SessionCheckpoint) = LBefore,
      'Candidate admission and observation leave the copied checkpoint byte-exact');
    LStore.Save(LPrimary, LWorkspaces);
    Check(ReadBytes(LDirectories.SessionCheckpoint) = LBefore,
      'Candidate round trip retains complete history and private registry bytes');
  finally
    { Workspaces borrow Primary; store owns only its independent filesystem lock. }
    LWorkspaces.Free;
    LPrimary.Free;
    LStore.Free;
  end;
  WriteLn('PASS ', GChecks, ' retained checkpoint candidate checks');
end;

{ Actual owned file recovery of full editor history. A sealed release supplies
  only directory admission; the current codec/session implementation owns every
  Save/Load. No listener, enrollment or protected user runtime is involved. }
procedure VerifyDraftCheckpoint(const ARelease, ARuntime: TNyxText);
var
  LDirectories: TNyxStudioDirectories;
  LStore: TNyxStudioRuntimeStore;
  LAuthor: TNyxStudioSession;
  LPrimary: TNyxAgentSession;
  LRegistry: TNyxStudioWorkspaces;
  LRecovered: TNyxAgentSession;
  LRecoveredRegistry: TNyxStudioWorkspaces;
  LChild: TNyxWorkspaceRef;
  LPair: TNyxProjectPair;
  LBefore: TNyxText;
  LPending: TNyxText;
  LChildPending: TNyxText;
  LBytes: TNyxText;

  procedure Traverse(ASession: TNyxAgentSession; const ADirection: TNyxText);
  begin
    ASession.Exchange(NyxObject([NyxField('op', NyxData('history')),
      NyxField('expectedRevision', NyxData(ASession.Revision)),
      NyxField('direction', NyxData(ADirection))]));
  end;

  function Packet(ASession: TNyxAgentSession): TNyxText;
  begin
    Result := ASession.Exchange(NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', NyxData(0))])).Field('project').AsText;
  end;

  procedure Pending(ASession: TNyxAgentSession; const ABuffer: TNyxText);
  var
    LObserved: TNyxDataValue;
    LCandidate: TNyxProjectPair;
    LPacket: TNyxText;
    LReply: TNyxDataValue;
    LImport: TNyxText;
    LTicket: TNyxText;
    LPrefix: TNyxText;
    LIndex: Integer;
    LStart: Integer;
    LCount: Integer;
    LScalar: Integer;
    LOffset: Integer;
    LSerial: Integer;

    function Transfer(const AMode, AOperation: TNyxText;
      const AExtra: array of TNyxDataField): TNyxDataValue;
    var
      LFields: array of TNyxDataField;
      LField: Integer;
    begin
      SetLength(LFields, 3 + Length(AExtra));
      LFields[0] := NyxField('mode', NyxData(AMode));
      LFields[1] := NyxField('expectedRevision', NyxData(ASession.Revision));
      LFields[2] := NyxField('operationId', NyxData(LPrefix + AOperation));
      for LField := 0 to High(AExtra) do
      begin
        LFields[LField + 3] := AExtra[LField];
      end;
      Result := ASession.Call('nyx_project', 'Draft recovery', NyxObject(LFields), 'owned-file');
    end;
  begin
    LObserved := ASession.Exchange(NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', NyxData(0))]));
    LCandidate := DecodeNyxProject(LObserved.Field('project').AsText);
    LCandidate.Pending := True;
    LCandidate.Draft := ABuffer;
    LCandidate.DraftBase := 'An independent saved baseline / 🚀';
    { A deliberate file transfer uses the semantic project tool. Ordinary
      typing/observer commit remains synchronization metadata, not a command. }
    LPacket := EncodeNyxProject(LCandidate);
    LPrefix := 'pending-file-' + IntToStr(ASession.Revision) + '-';
    LReply := Transfer('begin-import', 'reserve',
      [NyxField('bytes', NyxData(NyxUTF8ByteCount(LPacket)))]);
    LImport := LReply.Field('projectImport').Field('import').AsText;
    LIndex := 1;
    LOffset := 0;
    LSerial := 0;
    while LIndex <= Length(LPacket) do
    begin
      LStart := LIndex;
      LCount := 0;
      while (LIndex <= Length(LPacket)) and (LCount < 4096) do
      begin

        if not NyxNextScalar(LPacket, LIndex, LScalar) then
        begin
          raise Exception.Create('Malformed owned draft input');
        end;
        Inc(LCount);
      end;
      Inc(LSerial);
      LReply := Transfer('append-import', 'chunk-' + IntToStr(LSerial), [
        NyxField('import', NyxData(LImport)), NyxField('offset', NyxData(LOffset)),
        NyxField('text', NyxData(Copy(LPacket, LStart, LIndex - LStart)))]);
      LOffset := LReply.Field('projectImport').Field('nextOffset').AsInteger;
    end;
    LReply := Transfer('review-import', 'review', [NyxField('import', NyxData(LImport)),
      NyxField('resolution', NyxData('match'))]);
    LTicket := LReply.Field('projectImport').Field('reviewID').AsText;
    Transfer('apply', 'apply', [NyxField('import', NyxData(LImport)),
      NyxField('reviewID', NyxData(LTicket))]);
  end;

  function Version: Integer;
  var
    LData: TNyxText;
  begin
    LData := ReadBytes(LDirectories.SessionCheckpoint);
    { Length-prefixed UTF-8 magic precedes the little-endian version number. }
    Result := Ord(LData[Length('NYX-STUDIO-RUNTIME') + 5]);
  end;

begin

  if DirectoryExists(ARuntime) or FileExists(ARuntime) then
  begin
    raise Exception.Create('Draft-history qualification requires a new owned runtime');
  end;
  LStore := nil;
  LAuthor := nil;
  LPrimary := nil;
  LRegistry := nil;
  LRecovered := nil;
  LRecoveredRegistry := nil;
  try
    LDirectories := TNyxStudioDirectories.ForRelease(ARelease, ARuntime);
    LAuthor := TNyxStudioSession.Create;
    LPair := LAuthor.ProjectSnapshot;
    FreeAndNil(LAuthor);
    LPrimary := TNyxAgentSession.Create(LPair);
    LRegistry := TNyxStudioWorkspaces.Create(LPrimary, 'draft-history-proof');
    LChild := LRegistry.OpenProject('A separate history', LPair);
    LStore := TNyxStudioRuntimeStore.Create(LDirectories);
    LBefore := Packet(LPrimary);
    LStore.Save(LPrimary, LRegistry);
    Check(Version = 1, 'Accepted-only history retains legacy checkpoint format');
    LBytes := ReadBytes(LDirectories.SessionCheckpoint);
    Check(LStore.Load(LRecovered, LRecoveredRegistry), 'Legacy checkpoint is admitted');
    LStore.Save(LRecovered, LRecoveredRegistry);
    Check(ReadBytes(LDirectories.SessionCheckpoint) = LBytes,
      'Legacy admission/round trip preserves exact checkpoint bytes');
    FreeAndNil(LRecoveredRegistry);
    FreeAndNil(LRecovered);

    Pending(LPrimary, 'An unfinished buffer / 😀' + #10);
    LPending := Packet(LPrimary);
    Traverse(LPrimary, 'undo');
    Check(Packet(LPrimary) = LBefore, 'Pending-file Undo restores the complete accepted pair');
    LStore.Save(LPrimary, LRegistry);
    Check(Version = 2, 'Historical pending buffer selects extended checkpoint format');
    LBytes := ReadBytes(LDirectories.SessionCheckpoint);
    Check(LStore.Load(LRecovered, LRecoveredRegistry), 'Full draft/base history is admitted from disk');
    LStore.Save(LRecovered, LRecoveredRegistry);
    Check(ReadBytes(LDirectories.SessionCheckpoint) = LBytes,
      'Extended admission/round trip preserves exact checkpoint bytes');
    Traverse(LRecovered, 'redo');
    Check(Packet(LRecovered) = LPending, 'Recovered Redo restores exact Unicode buffer and stale base');
    Traverse(LRecovered, 'undo');
    Check(Packet(LRecovered) = LBefore, 'Recovered Undo remains a complete paired command');
    FreeAndNil(LRecoveredRegistry);
    FreeAndNil(LRecovered);

    Traverse(LPrimary, 'redo');
    LStore.Save(LPrimary, LRegistry);
    Check(Version = 1, 'Current pending source alone still uses the legacy layout');
    Pending(LRegistry.Find(LChild), 'A separate unfinished buffer / 🌙');
    LChildPending := Packet(LRegistry.Find(LChild));
    Traverse(LRegistry.Find(LChild), 'undo');
    LStore.Save(LPrimary, LRegistry);
    Check(Version = 2, 'Historical draft in another workspace selects the extension');
    Check(LStore.Load(LRecovered, LRecoveredRegistry), 'Concurrent project draft history is admitted');
    Traverse(LRecoveredRegistry.Find(LChild), 'redo');
    Check(Packet(LRecoveredRegistry.Find(LChild)) = LChildPending,
      'Recovered child restores its own exact unfinished buffer');
    Check(Packet(LRecovered) = LPending, 'Child history never changes the primary project');
  finally
    LRecoveredRegistry.Free;
    LRecovered.Free;
    LStore.Free;
    LRegistry.Free;
    LPrimary.Free;
    LAuthor.Free;
  end;
  WriteLn('PASS ', GChecks, ' actual draft checkpoint checks');
end;

function Routed(const AValue: TNyxDataValue; const AWorkspace: TNyxText): TNyxDataValue;
begin
  Result := AValue;

  if AWorkspace <> '' then
  begin
    Result := NyxWithWorkspace(AValue, NyxWorkspace(AWorkspace));
  end;
end;

function Observe(const AWorkspace: TNyxText = ''): TNyxDataValue;
begin
  Result := GEngine.EditorExchange(GToken, Routed(NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]), AWorkspace));
end;

{ Exercise the actual native router rather than the workspaces manager directly.
  Default-context queries previously bypassed its roster. Same display names
  still identify independent authenticated owners; read-only presence never
  becomes durable authoring or attributes an isolated review to the primary. }
procedure VerifyPrimaryPresence;
var
  LBefore: TNyxText;
  LConnections: TNyxDataValue;
  LReview: TNyxDataValue;
  LHandle: TNyxText;
begin
  LBefore := ReadBytes(GDirectories.SessionCheckpoint);
  GEngine.InvokeTool('nyx_session', 'primary-reader-one', 'Scooty primary reader',
    NyxObject([]));
  LConnections := Observe.Field('workspaces').Item(0).Field('connections');
  Check((LConnections.Count = 1) and
    (LConnections.Item(0).Field('actor').AsText = 'Scooty primary reader'),
    'Actual primary tool dispatch publishes its authenticated connection presence');
  GEngine.InvokeTool('nyx_session', 'primary-reader-one', 'Scooty primary reader',
    NyxObject([]));
  Check(Observe.Field('workspaces').Item(0).Field('connections').Count = 1,
    'Repeated primary queries retain one connection identity');
  GEngine.InvokeTool('nyx_session', 'primary-reader-two', 'Scooty primary reader',
    NyxObject([]));
  LConnections := Observe.Field('workspaces').Item(0).Field('connections');
  Check((LConnections.Count = 2) and
    (LConnections.Item(0).Field('session').AsInteger <>
      LConnections.Item(1).Field('session').AsInteger),
    'Same-display primary callers retain independent private owners');
  LReview := GEngine.InvokeTool('nyx_reviews', 'isolated-reader', 'Scooty review reader',
    NyxObject([NyxField('mode', NyxData('create')), NyxField('base', NyxData('empty')),
      NyxField('label', NyxData('Independent presence review')),
      NyxField('expectedRevision', Observe.Field('session').Field('revision')),
      NyxField('operationId', NyxData('primary-presence-review'))]));
  LHandle := LReview.Field('review').AsText;
  LReview := GEngine.InvokeTool('nyx_session', 'isolated-reader', 'Scooty review reader',
    NyxObject([NyxField('review', NyxData(LHandle))]));
  Check(Observe.Field('workspaces').Item(0).Field('connections').Count = 2,
    'Isolated review dispatch does not claim primary-project presence');
  GEngine.InvokeTool('nyx_reviews', 'isolated-reader', 'Scooty review reader',
    NyxObject([NyxField('mode', NyxData('discard')), NyxField('review', NyxData(LHandle)),
      NyxField('expectedRevision', LReview.Field('revision')),
      NyxField('operationId', NyxData('primary-presence-retire'))]));
  GEngine.InvokeBuild('primary-build-reader', 'Scooty compiler reader',
    NyxObject([NyxField('mode', NyxData('outputs'))]));
  Check(Observe.Field('workspaces').Item(0).Field('connections').Count = 3,
    'Default compiler discovery publishes primary-project presence');
  Check(ReadBytes(GDirectories.SessionCheckpoint) = LBefore,
    'Read-only primary/review/compile presence leaves the full checkpoint byte-exact');
end;

function Invoke(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := GEngine.InvokeTool(ATool, 'owned-recovery-transport', 'Scooty recovery', AArguments);
end;

function TitleRequest(const ATitle, AOperation, AWorkspace: TNyxText): TNyxDataValue;
begin
  Result := Routed(NyxObject([
    NyxField('expectedRevision', Observe(AWorkspace).Field('session').Field('revision')),
    NyxField('operationId', NyxData(AOperation)),
    NyxField('operations', NyxArray([NyxObject([NyxField('op', NyxData('title')),
      NyxField('value', NyxData(ATitle))])]))]), AWorkspace);
end;

procedure Title(const ATitle, AOperation: TNyxText; const AWorkspace: TNyxText = '');
begin
  Invoke('nyx_transaction', TitleRequest(ATitle, AOperation, AWorkspace));
end;

procedure History(const ADirection: TNyxText; const AWorkspace: TNyxText = '');
begin
  GEngine.EditorExchange(GToken, Routed(NyxObject([NyxField('op', NyxData('history')),
    NyxField('after', NyxData(0)),
    NyxField('expectedRevision', Observe(AWorkspace).Field('session').Field('revision')),
    NyxField('direction', NyxData(ADirection))]), AWorkspace));
end;

procedure Commit(const APair: TNyxProjectPair; const AWorkspace: TNyxText = '');
var
  LBefore: TNyxDataValue;
begin
  LBefore := Observe(AWorkspace);
  GEngine.EditorExchange(GToken, Routed(NyxObject([
    NyxField('op', NyxData('commit')), NyxField('after', NyxData(0)),
    NyxField('expectedRevision', LBefore.Field('session').Field('revision')),
    NyxField('project', NyxData(EncodeNyxProject(APair))),
    NyxField('selection', LBefore.Field('session').Field('selection')),
    NyxField('view', LBefore.Field('session').Field('view'))]), AWorkspace));
end;

function CreateProject(const AOperation: TNyxText): TNyxDataValue;
begin
  Result := Invoke('nyx_workspaces', NyxObject([
    NyxField('mode', NyxData('create')), NyxField('operationId', NyxData(AOperation)),
    NyxField('expectedRevision', Observe.Field('session').Field('revision')),
    NyxField('label', NyxData('Recovery project 😀')), NyxField('base', NyxData('accepted'))]));
end;

function Connect: TNyxDataValue;
begin
  Result := GEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
    NyxField('project', NyxData(EncodeNyxProject(GSeed))),
    NyxField('selection', NyxData('workspace')), NyxField('view', NyxData('home'))]));
  GToken := Result.Field('token').AsText;
end;

procedure Recreate;
begin
  { The actual protocol constructor reloads the committed runtime. Neither its
    suspended thread nor a Studio HTTP host is started. Expired connection data
    is intentionally replaced; protected services remain wholly independent. }
  FreeAndNil(GEngine);
  GEngine := TNyxStudioMCP.Create(GDirectories, 8608, 8609, GProfile);
  Connect;
end;

procedure SameFrame(const AExpected, AActual: TNyxDataValue; const ALabel: TNyxText);
const
  CFields: array[0..5] of TNyxText =
    ('revision', 'selection', 'view', 'pendingDraft', 'canUndo', 'canRedo');
var
  LField: TNyxText;
  LIndex: Integer;
begin
  Check(AExpected.Field('project').AsText = AActual.Field('project').AsText,
    ALabel + ' retains the exact paired files and draft/base');
  for LIndex := 0 to High(CFields) do
  begin
    LField := CFields[LIndex];
    Check(AExpected.Field('session').Field(LField).ToJSON =
      AActual.Field('session').Field(LField).ToJSON, ALabel + ' retains ' + LField);
  end;
end;

procedure CorruptRefusal(const AFileBytes, ALeaf: TNyxText; AChange: Integer);
var
  LDirectory: TNyxText;
  LPath: TNyxText;
  LBytes: TNyxText;
  LStream: TFileStream;
  LCandidate: TNyxStudioMCP;
  LRefused: Boolean;
begin
  LDirectory := GDirectories.RuntimeRoot + ALeaf + PathDelim;
  LPath := LDirectory + '.local' + PathDelim + 'studio-session.nyx';
  ForceDirectories(ExtractFileDir(LPath));
  LBytes := AFileBytes;

  if (AChange = 0) or (AChange = 3) then
  begin
    { Recomputed accidental-integrity digest cannot admit invalid semantic state:
      the first session revision follows magic length/content and format version. }

    if AChange = 0 then
    begin
      LBytes[4 + Length('NYX-STUDIO-RUNTIME') + 4 + 1] := #0;
      LBytes[4 + Length('NYX-STUDIO-RUNTIME') + 4 + 2] := #0;
      LBytes[4 + Length('NYX-STUDIO-RUNTIME') + 4 + 3] := #0;
      LBytes[4 + Length('NYX-STUDIO-RUNTIME') + 4 + 4] := #0;
    end
    else
    begin
      LBytes[4 + Length('NYX-STUDIO-RUNTIME') + 1] := #2;
    end;
    LBytes := Copy(LBytes, 1, Length(LBytes) - 32) +
      TNyxText(MD5Print(MD5String(Copy(LBytes, 1, Length(LBytes) - 32))));
  end
  else if AChange = 1 then
  begin
    LBytes[Length(LBytes)] := 'x';
  end
  else
  begin
    LBytes := LBytes + 'unexpected tail';
  end;
  LStream := TFileStream.Create(LPath, fmCreate);
  try
    LStream.WriteBuffer(LBytes[1], Length(LBytes));
  finally
    LStream.Free;
  end;
  LCandidate := nil;
  LRefused := False;
  try
    LCandidate := TNyxStudioMCP.Create(TNyxStudioDirectories.ForRepository(LDirectory),
      8618, 8619, GProfile);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  LCandidate.Free;
  Check(LRefused and (ReadBytes(LPath) = LBytes), 'Invalid ' + ALeaf + ' refuses without replacing recovery');
  Check(not DirectoryExists(LDirectory + '.codex'), 'Invalid ' + ALeaf + ' never enrolls a replacement endpoint');
end;

procedure ProduceAcknowledgedWork;
var
  LPair: TNyxProjectPair;
  LWorkspace: TNyxText;
begin
  Connect;
  Title('Fresh process first idea 😀', 'process-first');
  Title('Fresh process second idea 😀', 'process-second');
  History('undo');
  LWorkspace := CreateProject('process-project').Field('workspace').AsText;
  Title('Fresh process child idea 😀', 'process-child', LWorkspace);
  LPair := DecodeNyxProject(Observe.Field('project').AsText);
  LPair.Pending := True;
  LPair.Draft := LPair.Source + LineEnding + TNyxText('// unfinished process draft 😀');
  LPair.DraftBase := LPair.Source;
  Commit(LPair);
  { Readiness is emitted only after every actual editor/semantic call has returned.
    The producer remains alive with its store open; its owned parent terminates
    this exact TProcess handle, so no graceful destructor can save the session. }
  WriteBytes(GDirectories.RuntimeRoot + 'ready.nyx.next', NyxObject([
    NyxField('primary', Observe), NyxField('child', Observe(LWorkspace)),
    NyxField('workspace', NyxData(LWorkspace))]).ToJSON);

  if not RenameFile(GDirectories.RuntimeRoot + 'ready.nyx.next',
    GDirectories.RuntimeRoot + 'ready.nyx') then
  begin
    raise Exception.Create('Cannot publish the complete owned readiness frame');
  end;
  while True do
  begin
    Sleep(100);
  end;
end;

procedure ResumeAcknowledgedWork;
var
  LReady: TNyxDataValue;
  LPair: TNyxProjectPair;
begin
  LReady := TNyxDataValue.ParseJSON(ReadBytes(GDirectories.RuntimeRoot + 'ready.nyx'));
  Connect;
  SameFrame(LReady.Field('primary'), Observe, 'Abrupt-process primary');
  SameFrame(LReady.Field('child'), Observe(LReady.Field('workspace').AsText),
    'Abrupt-process child');
  LPair := DecodeNyxProject(Observe.Field('project').AsText);
  LPair.Pending := False;
  LPair.Draft := '';
  LPair.DraftBase := '';
  Commit(LPair);
  History('redo');
  Check(Pos('Fresh process second idea 😀',
    DecodeNyxProject(Observe.Field('project').AsText).Source) > 0,
    'Fresh process executes its recovered paired Redo');
  History('undo');
  Check(Pos('Fresh process first idea 😀',
    DecodeNyxProject(Observe.Field('project').AsText).Source) > 0,
    'Fresh process executes its recovered paired Undo');
end;

procedure AbruptProcessRecovery;
var
  LProducer: TProcess;
  LResumer: TProcess;
  LStarted: QWord;
  LReadyPath: TNyxText;
  LBuffer: array[0..2047] of Byte;
  LCount: Integer;
  LLog: TFileStream;

  function StartOwned(const AMode: TNyxText): TProcess;
  begin
    Result := TProcess.Create(nil);
    try
      Result.Executable := ExpandFileName(ParamStr(0));
      Result.Parameters.Add(AMode);
      Result.Parameters.Add(GDirectories.RuntimeRoot);
      Result.Parameters.Add(GSourceDirectory);
      Result.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
      Result.Execute;
    except
      Result.Free;
      raise;
    end;
  end;

  procedure Drain(AProcess: TProcess);
  begin
    while AProcess.Output.NumBytesAvailable > 0 do
    begin
      LCount := AProcess.Output.Read(LBuffer, SizeOf(LBuffer));

      if (LCount <= 0) or (LLog.Size + LCount > 65536) then
      begin
        raise Exception.Create('Owned recovery subprocess output exceeds its bound');
      end;
      LLog.WriteBuffer(LBuffer, LCount);
    end;
  end;

begin
  LProducer := nil;
  LResumer := nil;
  LLog := TFileStream.Create(GDirectories.RuntimeRoot + 'process.log', fmCreate);
  try
    LReadyPath := GDirectories.RuntimeRoot + 'ready.nyx';
    LProducer := StartOwned('--produce');
    LStarted := GetTickCount64;
    while not FileExists(LReadyPath) and LProducer.Running and
      (GetTickCount64 - LStarted < 60000) do
    begin
      Drain(LProducer);
      Sleep(10);
    end;
    Check(FileExists(LReadyPath) and LProducer.Running,
      'Owned producer acknowledges its durable paired work while still alive');
    LProducer.Terminate(17);
    LProducer.WaitOnExit(5000);
    Check(not LProducer.Running, 'Only the owned producer process is terminated');
    Drain(LProducer);
    LResumer := StartOwned('--resume');
    LStarted := GetTickCount64;
    while LResumer.Running and (GetTickCount64 - LStarted < 60000) do
    begin
      Drain(LResumer);
      Sleep(10);
    end;
    Check(not LResumer.Running, 'Fresh recovery process completes within its deadline');
    Drain(LResumer);
    Check(LResumer.ExitStatus = 0, 'Fresh process admits exact acknowledged work and executes history');
  finally
    { These handles are created by this fixture and never refer to an existing
      service. Failure also joins each owned child before releasing its handle. }

    if (LProducer <> nil) and LProducer.Running then
    begin
      LProducer.Terminate(17);
      LProducer.WaitOnExit(5000);
    end;

    if (LResumer <> nil) and LResumer.Running then
    begin
      LResumer.Terminate(17);
      LResumer.WaitOnExit(5000);
    end;
    LProducer.Free;
    LResumer.Free;
    LLog.Free;
  end;
end;

var
  LDocument: TNyxDocument;
  LProfile: TNyxOutputConfiguration;
  LPair: TNyxProjectPair;
  LPrimary: TNyxDataValue;
  LChild: TNyxDataValue;
  LProject: TNyxDataValue;
  LWorkspace: TNyxText;
  LOldToken: TNyxText;
  LBeforeBytes: TNyxText;
  LRequest: TNyxDataValue;
  LRetry: TNyxDataValue;
  LLock: TFileStream;
  LRefused: Boolean;
  LIndex: Integer;
  LMode: TNyxText;
  LCandidate: TNyxStudioMCP;
  LEnrollment: TNyxText;
begin
  GEngine := nil;
  LDocument := nil;
  LProfile := nil;
  LLock := nil;
  try

    if (ParamCount = 3) and (ParamStr(1) = '--draft-history') then
    begin
      VerifyDraftCheckpoint(ParamStr(2), ParamStr(3));
      Exit;
    end;

    if (ParamCount = 4) and (ParamStr(1) = '--retained') then
    begin
      VerifyRetainedCheckpoint(ParamStr(2), ParamStr(3), ParamStr(4));
      Exit;
    end;

    if not (ParamCount in [2, 3]) then
    begin
      raise Exception.Create('Supply new owned runtime and maintained semantic source directory');
    end;
    LMode := '';

    if ParamCount = 3 then
    begin
      LMode := ParamStr(1);

      if (LMode <> '--produce') and (LMode <> '--resume') and (LMode <> '--process') then
      begin
        raise Exception.Create('Unknown owned process recovery mode');
      end;
    end;
    GDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1 + Ord(LMode <> '')));
    GSourceDirectory := IncludeTrailingPathDelimiter(ParamStr(2 + Ord(LMode <> '')));

    if (LMode <> '--resume') and (LMode <> '--produce') then
    begin
      Check(not DirectoryExists(GDirectories.RuntimeRoot), 'Recovery runtime is new');
    end;
    LDocument := BuildNyxDocument;
    GSeed := NyxProjectPair(TNyxCodec.Encode(LDocument), ReadBytes(
      GSourceDirectory + 'nyx.generated.view.pas'));
    LProfile := TNyxOutputConfiguration.Create;
    GProfile := LProfile.Encode;

    if LMode = '--process' then
    begin
      ForceDirectories(GDirectories.RuntimeRoot);
      AbruptProcessRecovery;
    end
    else
    begin
      GEngine := TNyxStudioMCP.Create(GDirectories, 8608, 8609, GProfile);

      if LMode = '--produce' then
      begin
        ProduceAcknowledgedWork;
      end
      else if LMode = '--resume' then
      begin
        ResumeAcknowledgedWork;
      end
      else
      begin
        Connect;
        Check(FileExists(GDirectories.SessionCheckpoint), 'Actual claim is durable without any output compiler');
        VerifyPrimaryPresence;
        LPair := DecodeNyxProject(Observe.Field('project').AsText);
        LPair.Source := TNyxText('// Authored recovery note 😀') + LineEnding + LPair.Source;
        Commit(LPair);
        Title('A shared idea 😀', 'first-title');
        Title('A second idea 😀', 'second-title');
        History('undo');
        Check(Observe.Field('session').Field('canRedo').AsBoolean, 'Primary owns paired Redo before recovery');
        LProject := CreateProject('first-project');
        LWorkspace := LProject.Field('workspace').AsText;
        Title('A separate idea 😀', 'child-title', LWorkspace);
        Title('A separate second idea 😀', 'child-title-two', LWorkspace);
        History('undo', LWorkspace);
        LPair := DecodeNyxProject(Observe(LWorkspace).Field('project').AsText);
        LPair.Pending := True;
        LPair.Draft := LPair.Source + LineEnding + TNyxText('// unfinished 😀');
        LPair.DraftBase := 'A deliberately stale accepted baseline 😀';
        Commit(LPair, LWorkspace);
        LChild := Observe(LWorkspace);
        LPair := DecodeNyxProject(Observe.Field('project').AsText);
        LPair.Pending := True;
        LPair.Draft := '';
        LPair.DraftBase := LPair.Source;
        Commit(LPair);
        LPrimary := Observe;
        LOldToken := GToken;
        LBeforeBytes := ReadBytes(GDirectories.SessionCheckpoint);
        Check(Pos(LOldToken, LBeforeBytes) = 0, 'Checkpoint excludes editor connection credentials');
        LEnrollment := ReadBytes(GDirectories.EnrollmentRoot + '.codex' + PathDelim + 'config.toml');
        LCandidate := nil;
        LRefused := False;
        try
          LCandidate := TNyxStudioMCP.Create(GDirectories, 8608, 8609, GProfile);
        except
          on Exception do
          begin
            LRefused := True;
          end;
        end;
        LCandidate.Free;
        Check(LRefused, 'Concurrent protocol engines refuse the same owned runtime');
        Check((ReadBytes(GDirectories.SessionCheckpoint) = LBeforeBytes) and
          (ReadBytes(GDirectories.EnrollmentRoot + '.codex' + PathDelim + 'config.toml') = LEnrollment),
          'Concurrent refusal retains exact checkpoint and connection enrollment');
        Recreate;
        Check(GToken <> LOldToken, 'Recreated protocol rotates editor authority');
        SameFrame(LPrimary, Observe, 'Recovered primary');
        SameFrame(LChild, Observe(LWorkspace), 'Recovered ordinary project');
        LRefused := False;
        try
          GEngine.EditorExchange(LOldToken, NyxObject([NyxField('op', NyxData('observe'))]));
        except
          on Exception do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused, 'Expired editor capability refuses after recreation');
        Check(ReadBytes(GDirectories.SessionCheckpoint) = LBeforeBytes,
          'Observe and refused duplicate local claim leave the checkpoint byte-exact');
        LPair := DecodeNyxProject(Observe.Field('project').AsText);
        LPair.Pending := False;
        LPair.Draft := '';
        LPair.DraftBase := '';
        Commit(LPair);
        History('redo');
        LPair := DecodeNyxProject(Observe.Field('project').AsText);
        Check(Pos('A second idea 😀', LPair.Source) > 0, 'Recovered Redo executes exact supplementary-Unicode source');
        Check(Pos('// Authored recovery note 😀', LPair.Source) = 1, 'Recovered history retains authored source notes');
        History('undo');
        LRequest := TitleRequest('A committed retry 😀', 'durable-retry', '');
        LRetry := Invoke('nyx_transaction', LRequest);
        LPrimary := Observe;
        LChild := Observe(LWorkspace);
        LBeforeBytes := ReadBytes(GDirectories.SessionCheckpoint);
        LLock := TFileStream.Create(GDirectories.SessionCheckpoint, fmOpenRead or fmShareExclusive);
        LRefused := False;
        try
          Title('Must stay refused 😀', 'denied-title');
        except
          on Exception do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused, 'Denied atomic replacement refuses the semantic mutation');
        SameFrame(LPrimary, Observe, 'Denied write primary');
        SameFrame(LChild, Observe(LWorkspace), 'Denied write child');
        Check(Invoke('nyx_transaction', LRequest).ToJSON = LRetry.ToJSON,
          'Prepared rollback retains exact successful retry receipts');
        LRefused := False;
        try
          CreateProject('denied-project');
        except
          on Exception do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused, 'Denied project creation also rolls back its registry and receipt');
        FreeAndNil(LLock);
        Check(ReadBytes(GDirectories.SessionCheckpoint) = LBeforeBytes, 'Denied writes retain the exact durable checkpoint');
        LProject := CreateProject('denied-project');
        Check(LProject.Field('workspace').AsText <> LWorkspace, 'Refused creation can later commit without ghost projects');
        for LIndex := 3 to 8 do
        begin
          CreateProject('project-' + IntToStr(LIndex));
        end;
        Check(Observe.Field('workspaces').Count = 9, 'All eight ordinary projects coexist with primary');
        Recreate;
        Check(Observe.Field('workspaces').Count = 9, 'Actual recreation retains the full ordinary project registry');
        SameFrame(LChild, Observe(LWorkspace), 'Full registry child');
        GEngine.EditorExchange(GToken, NyxObject([
          NyxField('op', NyxData('close-workspace')), NyxField('after', NyxData(0)),
          NyxField('target', LProject.Field('workspace')),
          NyxField('expectedRevision', Observe(LProject.Field('workspace').AsText).Field('session').Field('revision')),
          NyxField('confirmed', NyxData(True))]));
        Check(Observe.Field('workspaces').Count = 8, 'Confirmed close retains the other ordinary projects');
        Recreate;
        Check(Observe.Field('workspaces').Count = 8, 'Closed project remains closed after recreation');
        LProject := CreateProject('after-recovered-close');
        Check(Pos('.project-9', LProject.Field('workspace').AsText) > 0,
          'Recovered registry serial never reuses a closed project handle');
        GEngine.EditorExchange(GToken, NyxObject([NyxField('op', NyxData('configure')),
          NyxField('permission', NyxData('disabled'))]));
        Recreate;
        Check(Observe.Field('session').Field('permission').AsText = 'disabled', 'Operator enablement persists across recreation');
        LRefused := False;
        try
          Invoke('nyx_session', NyxObject([]));
        except
          on Exception do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused, 'Recreated agent dispatch still respects disabled access');
        LBeforeBytes := ReadBytes(GDirectories.SessionCheckpoint);
        CorruptRefusal(LBeforeBytes, 'bad-digest', 1);
        CorruptRefusal(LBeforeBytes, 'bad-tail', 2);
        CorruptRefusal(LBeforeBytes, 'bad-revision', 0);
        CorruptRefusal(LBeforeBytes, 'bad-version', 3);
      end;
    end;
    WriteLn('PASS ', GChecks, ' actual protocol recovery checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.ClassName, ': ', LException.Message);
      ExitCode := 1;
    end;
  end;
  LLock.Free;
  GEngine.Free;
  LProfile.Free;
  LDocument.Free;
end.
