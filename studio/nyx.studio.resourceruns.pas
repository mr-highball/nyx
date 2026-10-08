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


unit nyx.studio.resourceruns;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, Classes, nyx.text, nyx.data, nyx.application.resources,
  nyx.studio.agents, nyx.studio.projects, nyx.studio.workspaces;

type
  { Borrowed resolver, used only during a serialized call. Rollback may replace
    session objects, so grants retain exact context/pair values rather than them. }
  TNyxRuntimeSessionLookup = function(const AWorkspace: TNyxWorkspaceRef): TNyxAgentSession of object;
  { Borrowed monotonic millisecond clock for deterministic qualification. Its
    receiver outlives the broker; nil selects the system monotonic clock. }
  TNyxRuntimeClock = function: QWord of object;

  { Trusted launch broker. Sixteen private grants bound global memory; at most
    eight unexpired grants belong to one project. A grant expires sixty seconds
    after its last accepted report, or immediately after a paired revision/draft.
    No credential, immutable pair or resource bytes enter public MCP responses.
    Caller owns locking and supplies compiler admission before Issue. }
  TNyxResourceRuntimeBroker = class
  private
    FGrants: TList;
    FClock: TNyxRuntimeClock;
    function Now: QWord;
    function IndexOf(const AToken: TNyxText): Integer;
  public
    constructor Create(AClock: TNyxRuntimeClock = nil);
    destructor Destroy; override;
    { Copies the exact admitted pair/declarations and returns private launch
      data. The host has already checked job/manifest ownership; unavailable
      capacity or stale/draft/failed compiler context refuses without a grant. }
    function Issue(ASession: TNyxAgentSession; const AWorkspace: TNyxWorkspaceRef;
      const APair: TNyxProjectPair; const ABuild: TNyxDataValue): TNyxDataValue;
    { Resolves only live private membership. The host then obtains the current
      session for this workspace under its lock; no session is retained here. }
    function Workspace(const AToken: TNyxText): TNyxWorkspaceRef;
    { Unknown, expired and wrong-context tokens fail before report decoding.
      One monotonic client sequence admits exact retries without duplicate logs.
      Heartbeats retain liveness without pretending that resource state changed. }
    function Exchange(const AToken: TNyxText; ASession: TNyxAgentSession;
      const AMessage: TNyxDataValue): TNyxDataValue;
    { Reacquires each current owner, retires copied evidence and frees expired or
      revoked grants. Invoked during serialized observations and requests; no
      background thread or callback borrows an old session after rollback. }
    procedure Expire(ALookup: TNyxRuntimeSessionLookup);
  end;

implementation

uses nyx.bytes, nyx.codec, nyx.model, nyx.types, nyx.resources, nyx.studio.builds;

type
  TRuntimeGrant = class
  public
    Token: TNyxText;
    Run: TNyxStudioRuntimeRef;
    Workspace: TNyxWorkspaceRef;
    Pair: TNyxProjectPair;
    Revision: Integer;
    Scope: TNyxStudioRuntimeScope;
    Target: TNyxPlatform;
    View: TNyxText;
    Declarations: INyxResources;
    Observation: TNyxStudioResourceObservation;
    Enrolled: Boolean;
    Retired: Boolean;
    Deadline: QWord;
    Sequence: Integer;
    LastRequest: TNyxText;
    LastSnapshot: TNyxText;
    Reply: TNyxDataValue;
  end;

constructor TNyxResourceRuntimeBroker.Create(AClock: TNyxRuntimeClock);
begin
  inherited Create;
  FGrants := TList.Create;
  FClock := AClock;
end;

destructor TNyxResourceRuntimeBroker.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to FGrants.Count - 1 do
  begin
    TObject(FGrants[LIndex]).Free;
  end;
  FGrants.Free;
  inherited Destroy;
end;

function TNyxResourceRuntimeBroker.Now: QWord;
begin
  Result := GetTickCount64;

  if Assigned(FClock) then
  begin
    Result := FClock();
  end;
end;

function TNyxResourceRuntimeBroker.IndexOf(const AToken: TNyxText): Integer;
var
  LIndex: Integer;
begin

  if (Length(AToken) <> 76) then
  begin
    raise ENyxResource.Create('Runtime reporting capability is absent or expired');
  end;
  for LIndex := 0 to FGrants.Count - 1 do
  begin

    if TRuntimeGrant(FGrants[LIndex]).Token = AToken then
    begin
      Exit(LIndex);
    end;
  end;
  raise ENyxResource.Create('Runtime reporting capability is absent or expired');
end;

function TNyxResourceRuntimeBroker.Issue(ASession: TNyxAgentSession;
  const AWorkspace: TNyxWorkspaceRef; const APair: TNyxProjectPair;
  const ABuild: TNyxDataValue): TNyxDataValue;
var
  LGrant: TRuntimeGrant;
  LID: TGUID;
  LDocument: TNyxDocument;
  LIndex: Integer;
  LCount: Integer;
  procedure NextIdentity;
  begin

    if CreateGUID(LID) <> 0 then
    begin
      raise ENyxResource.Create('Runtime launch identity could not be created');
    end;
  end;
begin

  if (ASession = nil) or not ASession.CurrentPair(APair) or
    (ABuild.Field('state').AsText <> 'succeeded') or
    not ABuild.Field('currentSource').AsBoolean or
    not ABuild.Field('currentOutput').AsBoolean or ASession.PendingDraft then
  begin
    raise ENyxResource.Create('Runtime grant requires a current accepted compiler result without a draft');
  end;
  LCount := 0;
  for LIndex := 0 to FGrants.Count - 1 do
  begin

    if (TRuntimeGrant(FGrants[LIndex]).Workspace.ID = AWorkspace.ID) and
      not TRuntimeGrant(FGrants[LIndex]).Retired then
    begin
      Inc(LCount);
    end;
  end;

  if (FGrants.Count >= 16) or (LCount >= 8) then
  begin
    raise ENyxResource.Create('Runtime launch grant budget exhausted; retire or await expiry');
  end;
  LGrant := TRuntimeGrant.Create;
  LDocument := nil;
  try
    NextIdentity;
    LGrant.Token := TNyxText(GUIDToString(LID));
    NextIdentity;
    LGrant.Token := LGrant.Token + TNyxText(GUIDToString(LID));
    NextIdentity;
    LGrant.Run := NyxStudioRuntime('run-' + ABuild.Field('job').AsText + '-' +
      TNyxText(Copy(GUIDToString(LID), 2, 36)));
    LGrant.Workspace := AWorkspace;
    LGrant.Pair := APair;
    LGrant.Revision := ASession.Revision;
    LGrant.Target := npfBrowser;

    if ParseNyxBuildTarget(ABuild.Field('target').AsText) = btNativeLCL then
    begin
      LGrant.Target := npfNativeLCL;
    end;
    LGrant.Scope := srsApplication;

    if ParseNyxBuildScope(ABuild.Field('scope').AsText) <> bsApplication then
    begin
      LGrant.Scope := srsView;
      LGrant.View := ABuild.Field('view').AsText;
    end;
    LDocument := TNyxCodec.Decode(APair.Design);
    LGrant.Declarations := LDocument.Resources.Clone;
    LGrant.Deadline := Now + 60000;
    Result := NyxObject([NyxField('version', NyxData(1)),
      NyxField('endpoint', NyxData('/api/resource-runtime')),
      NyxField('token', NyxData(LGrant.Token)), NyxField('run', NyxData(LGrant.Run.Name)),
      NyxField('intervalMilliseconds', NyxData(1000))]);
    FGrants.Add(LGrant);
    LGrant := nil;
  finally
    LDocument.Free;
    LGrant.Free;
  end;
end;

function TNyxResourceRuntimeBroker.Workspace(const AToken: TNyxText): TNyxWorkspaceRef;
var
  LGrant: TRuntimeGrant;
begin
  LGrant := TRuntimeGrant(FGrants[IndexOf(AToken)]);

  if Now >= LGrant.Deadline then
  begin
    raise ENyxResource.Create('Runtime reporting capability is absent or expired');
  end;
  Result := LGrant.Workspace;
end;

function TNyxResourceRuntimeBroker.Exchange(const AToken: TNyxText;
  ASession: TNyxAgentSession; const AMessage: TNyxDataValue): TNyxDataValue;
var
  LGrant: TRuntimeGrant;
  LSequence: Integer;
  LRequest: TNyxText;
  LSnapshotText: TNyxText;
  LSnapshot: INyxResourceRuntimeSnapshot;
  LOperation: TNyxText;
begin
  LGrant := TRuntimeGrant(FGrants[IndexOf(AToken)]);

  if (Now >= LGrant.Deadline) or (ASession = nil) or
    (ASession.Revision <> LGrant.Revision) or not ASession.CurrentPair(LGrant.Pair) or
    ASession.PendingDraft then
  begin
    raise ENyxResource.Create('Runtime reporting context changed or expired');
  end;
  NyxAgentFields(AMessage, '|version|operation|sequence|snapshot|');
  LSequence := AMessage.Field('sequence').AsInteger;
  LOperation := AMessage.Field('operation').AsText;
  LRequest := AMessage.ToJSON;

  if (AMessage.Field('version').AsInteger <> 1) or
    (LSequence < 1) or (NyxUTF8ByteCount(LRequest) > NyxMaximumRuntimeReportBytes) then
  begin
    raise ENyxResource.Create('Runtime request requires bounded version-one sequence data');
  end;

  if (LSequence = LGrant.Sequence) and (LRequest = LGrant.LastRequest) then
  begin
    Exit(LGrant.Reply);
  end;

  if LGrant.Retired or (LGrant.Sequence = High(Integer)) or
    (LSequence <> LGrant.Sequence + 1) then
  begin
    raise ENyxResource.Create('Runtime sequence is stale, skipped or retired');
  end;

  if LOperation = 'publish' then
  begin
    LSnapshot := DecodeNyxResourceRuntime(AMessage.Field('snapshot'), LGrant.Declarations);
    LSnapshotText := AMessage.Field('snapshot').ToJSON;

    if not LGrant.Enrolled then
    begin
      LGrant.Observation := ASession.ObserveResourceRuntime(LGrant.Revision, LGrant.Run,
        LGrant.Scope, LGrant.Target, LGrant.View, LSnapshot);
      LGrant.Enrolled := True;
    end
    else if LSnapshotText <> LGrant.LastSnapshot then
    begin
      ASession.PublishResourceRuntime(LGrant.Observation, LSnapshot);
    end;
    LGrant.LastSnapshot := LSnapshotText;
    LGrant.Retired := LSnapshot.Stopped;
  end
  else if LOperation = 'heartbeat' then
  begin

    if not LGrant.Enrolled or NyxAgentHas(AMessage, 'snapshot') then
    begin
      raise ENyxResource.Create('Runtime heartbeat requires an enrolled producer and omits snapshot data');
    end;
  end
  else if LOperation = 'retire' then
  begin

    if NyxAgentHas(AMessage, 'snapshot') then
    begin
      raise ENyxResource.Create('Runtime retirement omits snapshot data');
    end;

    if LGrant.Enrolled then
    begin
      ASession.RetireResourceRuntime(LGrant.Observation);
    end;
    LGrant.Retired := True;
  end
  else
  begin
    raise ENyxResource.Create('Runtime operation must publish, heartbeat or retire');
  end;
  LGrant.Sequence := LSequence;
  LGrant.LastRequest := LRequest;
  LGrant.Deadline := Now + 60000;
  LGrant.Reply := NyxObject([NyxField('sequence', NyxData(LSequence)),
    NyxField('active', NyxData(not LGrant.Retired))]);
  Result := LGrant.Reply;
end;

procedure TNyxResourceRuntimeBroker.Expire(ALookup: TNyxRuntimeSessionLookup);
var
  LIndex: Integer;
  LGrant: TRuntimeGrant;
  LSession: TNyxAgentSession;
begin
  for LIndex := FGrants.Count - 1 downto 0 do
  begin
    LGrant := TRuntimeGrant(FGrants[LIndex]);
    LSession := ALookup(LGrant.Workspace);

    if (Now >= LGrant.Deadline) or (LSession = nil) or
      (LSession.Revision <> LGrant.Revision) or not LSession.CurrentPair(LGrant.Pair) or
      LSession.PendingDraft then
    begin

      if (LSession <> nil) and LGrant.Enrolled then
      begin
        LSession.RetireResourceRuntime(LGrant.Observation);
      end;
      FGrants.Delete(LIndex);
      LGrant.Free;
    end;
  end;
end;

end.
