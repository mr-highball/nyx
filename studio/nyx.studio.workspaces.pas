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
unit nyx.studio.workspaces;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.studio.projects, nyx.studio.agents;

type
  { Service-scoped project identity, separate from a document root, title or
    transport connection. Empty explicitly denotes the primary project; an
    explicitly supplied empty wire reference is invalid. IDs are never reused. }
  TNyxWorkspaceRef = record
  private
    FID: TNyxText;
  public
    property ID: TNyxText read FID;
  end;

  { Creation is independent of build targets. An accepted copy excludes the
    primary project's pending draft; an empty project copies no authored data. }
  TNyxWorkspaceBase = (nwbEmpty, nwbAccepted);

  { Owns eight ordinary independent project sessions beside a borrowed primary
    session. These projects survive agent disconnection; server shutdown or an
    explicit trusted operator close ends their lifetime. This is open-session
    retention, not disk persistence. Every borrowed session pointer is valid
    only under the owner's existing document lock. Workers capture owned text.
    No browser's currently displayed project is stored here: callers always
    resolve their own reference and switching an editor cannot retarget them. }
  TNyxStudioWorkspaces = class
  private
    FPrimary: TNyxAgentSession;
    FIdentity: TNyxText;
    FSerial: Integer;
    FPresenceSerial: Integer;
    FEntries: array of record
      Reference: TNyxWorkspaceRef;
      LabelText: TNyxText;
      Session: TNyxAgentSession;
    end;
    { Private authority stays outside observer summaries. One connection may
      work in several projects, but at most 64 connection/project pairs exist.
      Display names are not retry or removal-review authority. }
    FPresence: array of record
      Owner: TNyxText;
      Actor: TNyxText;
      Reference: TNyxWorkspaceRef;
      Serial: Integer;
    end;
    { 64 immutable creation receipts per live transport, never evicted. Closing
      a project does not turn an old retry into another project creation. }
    FReceipts: array of record
      Owner: TNyxText;
      Operation: TNyxText;
      Request: TNyxText;
      Result: TNyxDataValue;
    end;
    function Index(const AReference: TNyxWorkspaceRef): Integer;
    procedure RequireAccess(AEdit: Boolean);
    function Describe(const AReference: TNyxWorkspaceRef): TNyxDataValue;
    procedure Touch(const AOwner, AActor: TNyxText; const AReference: TNyxWorkspaceRef);
  public
    constructor Create(APrimary: TNyxAgentSession; const AServiceIdentity: TNyxText);
    destructor Destroy; override;
    { Admit a full independent pair before publishing a project handle. Failed
      source/model admission leaves the registry and primary project untouched. }
    function OpenProject(const ALabel: TNyxText; const APair: TNyxProjectPair): TNyxWorkspaceRef;
    function CreateProject(const ALabel: TNyxText; ABase: TNyxWorkspaceBase;
      AExpected: Integer): TNyxWorkspaceRef;
    { Agent resolution inherits global operator permission on every call. A
      missing/closed reference refuses, with no primary-project fallback. }
    function Resolve(const AReference: TNyxWorkspaceRef): TNyxAgentSession;
    { Trusted editor/completion lookup, independent of agent enablement. Nil
      means closed; even a matching pair elsewhere must not receive its report. }
    function Find(const AReference: TNyxWorkspaceRef): TNyxAgentSession;
    { Build/preview-only consumers also establish visible connection presence.
      Admission checks access/context before publishing this bounded metadata. }
    procedure RecordRequest(const AOwner, AActor: TNyxText;
      const AReference: TNyxWorkspaceRef);
    function Call(const ATool, AOwner, AActor: TNyxText;
      const AArguments: TNyxDataValue): TNyxDataValue;
    { Focused MCP lifecycle: list/inspect/create. Agents cannot close a user
      project or promote permissions. Creation guards the primary revision. }
    function Manage(const AOwner, AActor: TNyxText;
      const AArguments: TNyxDataValue): TNyxDataValue;
    { Trusted operator-only close. The UI must present a warning and obtain
      explicit confirmation. Exact revision rechecks that consent; declined or
      stale confirmation leaves every project and its history intact. }
    procedure CloseProject(const AReference: TNyxWorkspaceRef;
      AExpected: Integer; AConfirmed: Boolean);
    { Drop only connection presence and retry state. User projects remain open,
      including accepted pair, pending draft/base, selection/view and history. }
    procedure ReleaseOwner(const AOwner: TNyxText);
    { At most nine project summaries (primary plus eight). No complete pair,
      pending draft, private owner identity or machine output paths are returned. }
    function Observe: TNyxDataValue;
  end;

function NyxPrimaryWorkspace: TNyxWorkspaceRef;
function NyxWorkspace(const AID: TNyxText): TNyxWorkspaceRef;
function NyxWorkspaceArgument(const AArguments: TNyxDataValue): TNyxWorkspaceRef;
{ Routing changes only outer fields. Nested mutations retain strict admission;
  simultaneous review/project references refuse instead of guessing a target. }
function NyxWorkspaceArguments(const AArguments: TNyxDataValue): TNyxDataValue;
function NyxWithWorkspace(const AValue: TNyxDataValue;
  const AReference: TNyxWorkspaceRef): TNyxDataValue;

implementation

uses
  nyx.codec, nyx.codegen, nyx.editing;

function NyxPrimaryWorkspace: TNyxWorkspaceRef;
begin
  Result.FID := '';
end;

function NyxWorkspace(const AID: TNyxText): TNyxWorkspaceRef;
begin

  if (AID = '') or (NyxTextScalarCount(AID) > 120) then
  begin
    raise ENyxModel.Create('A supplied workspace reference must contain 1..120 characters');
  end;
  Result.FID := AID;
end;

function NyxWorkspaceArgument(const AArguments: TNyxDataValue): TNyxWorkspaceRef;
begin
  Result := NyxPrimaryWorkspace;

  if NyxAgentHas(AArguments, 'workspace') then
  begin

    if NyxAgentHas(AArguments, 'review') then
    begin
      raise ENyxModel.Create('Supply one project or review context, never both');
    end;
    Result := NyxWorkspace(AArguments.Field('workspace').AsText);
  end;
end;

function NyxWorkspaceArguments(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  NyxWorkspaceArgument(AArguments);
  LFields := nil;
  for LIndex := 0 to AArguments.Count - 1 do
  begin

    if AArguments.Key(LIndex) <> 'workspace' then
    begin
      SetLength(LFields, Length(LFields) + 1);
      LFields[High(LFields)] := NyxField(AArguments.Key(LIndex),
        AArguments.Field(AArguments.Key(LIndex)));
    end;
  end;
  Result := NyxObject(LFields);
end;

function NyxWithWorkspace(const AValue: TNyxDataValue;
  const AReference: TNyxWorkspaceRef): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  Result := AValue;

  if AReference.ID = '' then
  begin
    Exit;
  end;
  SetLength(LFields, AValue.Count + 1);
  for LIndex := 0 to AValue.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AValue.Key(LIndex), AValue.Field(AValue.Key(LIndex)));
  end;
  LFields[High(LFields)] := NyxField('workspace', NyxData(AReference.ID));
  Result := NyxObject(LFields);
end;

constructor TNyxStudioWorkspaces.Create(APrimary: TNyxAgentSession;
  const AServiceIdentity: TNyxText);
begin
  inherited Create;

  if (APrimary = nil) or (AServiceIdentity = '') or
    (NyxTextScalarCount(AServiceIdentity) > 80) then
  begin
    raise ENyxModel.Create('Project workspaces require a primary session and service identity');
  end;
  FPrimary := APrimary;
  FIdentity := AServiceIdentity;
end;

destructor TNyxStudioWorkspaces.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FEntries) do
  begin
    FEntries[LIndex].Session.Free;
  end;
  FPrimary := nil;
  inherited Destroy;
end;

function TNyxStudioWorkspaces.Index(const AReference: TNyxWorkspaceRef): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex].Reference.ID = AReference.ID then
    begin
      Exit(LIndex);
    end;
  end;
end;

procedure TNyxStudioWorkspaces.RequireAccess(AEdit: Boolean);
begin

  if FPrimary.Permission = apDisabled then
  begin
    raise ENyxModel.Create('Agent access is disabled in Studio');
  end;

  if AEdit and (FPrimary.Permission <> apEdit) then
  begin
    raise ENyxModel.Create('Project creation requires Allow edits in Studio');
  end;
end;

function TNyxStudioWorkspaces.Find(const AReference: TNyxWorkspaceRef): TNyxAgentSession;
var
  LIndex: Integer;
begin
  Result := FPrimary;

  if AReference.ID <> '' then
  begin
    Result := nil;
    LIndex := Index(AReference);

    if LIndex >= 0 then
    begin
      Result := FEntries[LIndex].Session;
    end;
  end;

  if Result <> nil then
  begin
    Result.InheritPermission(FPrimary.Permission);
  end;
end;

function TNyxStudioWorkspaces.Resolve(const AReference: TNyxWorkspaceRef): TNyxAgentSession;
begin
  RequireAccess(False);
  Result := Find(AReference);

  if Result = nil then
  begin
    raise ENyxModel.Create('Project workspace is missing, closed or belongs to another Studio service');
  end;
end;

function TNyxStudioWorkspaces.OpenProject(const ALabel: TNyxText;
  const APair: TNyxProjectPair): TNyxWorkspaceRef;
var
  LSession: TNyxAgentSession;
  LIndex: Integer;
begin

  if (ALabel = '') or (NyxTextScalarCount(ALabel) > 256) or
    (Length(FEntries) >= 8) or (FSerial = High(Integer)) then
  begin
    raise ENyxModel.Create('Project label must contain 1..256 characters; at most eight projects can open');
  end;
  LSession := TNyxAgentSession.Create(APair);
  try
    Result := NyxWorkspace(FIdentity + '.project-' + IntToStr(FSerial + 1));
    LIndex := Length(FEntries);
    SetLength(FEntries, LIndex + 1);
    FEntries[LIndex].Reference := Result;
    FEntries[LIndex].LabelText := ALabel;
    FEntries[LIndex].Session := LSession;
    Inc(FSerial);
    LSession := nil;
  finally
    LSession.Free;
  end;
end;

function TNyxStudioWorkspaces.CreateProject(const ALabel: TNyxText;
  ABase: TNyxWorkspaceBase; AExpected: Integer): TNyxWorkspaceRef;
var
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
begin
  RequireAccess(True);

  if AExpected <> FPrimary.Revision then
  begin
    raise ENyxModel.Create('Project creation revision conflict; inspect the primary project');
  end;

  if ABase = nwbAccepted then
  begin
    LPair := FPrimary.ReviewSeed(AExpected);
  end
  else
  begin
    LDocument := TNyxDocument.Create;
    try
      LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    finally
      LDocument.Free;
    end;
  end;
  Result := OpenProject(ALabel, LPair);
end;

procedure TNyxStudioWorkspaces.Touch(const AOwner, AActor: TNyxText;
  const AReference: TNyxWorkspaceRef);
var
  LIndex: Integer;
begin

  if (AOwner = '') or (NyxTextScalarCount(AOwner) > 120) or
    (NyxTextScalarCount(AActor) > 100) then
  begin
    raise ENyxModel.Create('Project requests require a bounded authenticated owner and display actor');
  end;
  for LIndex := 0 to High(FPresence) do
  begin

    if (FPresence[LIndex].Owner = AOwner) and
      (FPresence[LIndex].Reference.ID = AReference.ID) then
    begin
      Exit;
    end;
  end;

  if (Length(FPresence) >= 64) or (FPresenceSerial = High(Integer)) then
  begin
    raise ENyxModel.Create('Project connection budget reached; disconnect an idle agent');
  end;
  LIndex := Length(FPresence);
  SetLength(FPresence, LIndex + 1);
  FPresence[LIndex].Owner := AOwner;
  FPresence[LIndex].Actor := AActor;
  FPresence[LIndex].Reference := AReference;
  Inc(FPresenceSerial);
  FPresence[LIndex].Serial := FPresenceSerial;
end;

procedure TNyxStudioWorkspaces.RecordRequest(const AOwner, AActor: TNyxText;
  const AReference: TNyxWorkspaceRef);
begin
  Resolve(AReference);
  Touch(AOwner, AActor, AReference);
end;

function TNyxStudioWorkspaces.Describe(const AReference: TNyxWorkspaceRef): TNyxDataValue;
var
  LSession: TNyxAgentSession;
  LState: TNyxDataValue;
  LConnections: array of TNyxDataValue;
  LIndex: Integer;
  LLabel: TNyxText;
begin
  LSession := Find(AReference);

  if LSession = nil then
  begin
    raise ENyxModel.Create('Project workspace is closed');
  end;
  LState := LSession.Exchange(NyxObject([NyxField('op', NyxData('observe')),
    NyxField('after', NyxData(LSession.Revision))]));
  LLabel := 'Primary project';

  if AReference.ID <> '' then
  begin
    LLabel := FEntries[Index(AReference)].LabelText;
  end;
  LConnections := nil;
  for LIndex := 0 to High(FPresence) do
  begin

    if FPresence[LIndex].Reference.ID = AReference.ID then
    begin
      SetLength(LConnections, Length(LConnections) + 1);
      LConnections[High(LConnections)] := NyxObject([
        NyxField('session', NyxData(FPresence[LIndex].Serial)),
        NyxField('actor', NyxData(FPresence[LIndex].Actor))]);
    end;
  end;
  Result := NyxObject([NyxField('workspace', NyxData(AReference.ID)),
    NyxField('label', NyxData(LLabel)), NyxField('lifetime', NyxData('project')),
    NyxField('session', LState.Field('session')),
    NyxField('connections', NyxArray(LConnections))]);
end;

function TNyxStudioWorkspaces.Observe: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, Length(FEntries) + 1);
  LItems[0] := Describe(NyxPrimaryWorkspace);
  for LIndex := 0 to High(FEntries) do
  begin
    LItems[LIndex + 1] := Describe(FEntries[LIndex].Reference);
  end;
  Result := NyxArray(LItems);
end;

function TNyxStudioWorkspaces.Call(const ATool, AOwner, AActor: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReference: TNyxWorkspaceRef;
  LSession: TNyxAgentSession;
begin
  LReference := NyxWorkspaceArgument(AArguments);
  LSession := Resolve(LReference);
  Touch(AOwner, AActor, LReference);
  try
    Result := NyxWithWorkspace(LSession.Call(ATool, AActor,
      NyxWorkspaceArguments(AArguments), AOwner), LReference);

    if LReference.ID <> '' then
    begin
      FPrimary.RecordActivity(AActor, ATool, LReference.ID + ' / completed');
    end;
  except
    on LException: Exception do
    begin

      if LReference.ID <> '' then
      begin
        FPrimary.RecordActivity(AActor, ATool, LReference.ID + ' / refused');
      end;
      raise;
    end;
  end;
end;

function TNyxStudioWorkspaces.Manage(const AOwner, AActor: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LMode: TNyxText;
  LOperation: TNyxText;
  LRequest: TNyxText;
  LIndex: Integer;
  LUsed: Integer;
  LBase: TNyxWorkspaceBase;
  LReference: TNyxWorkspaceRef;
begin
  RequireAccess(False);
  LMode := AArguments.Field('mode').AsText;

  if LMode = 'list' then
  begin
    NyxAgentFields(AArguments, '|mode|');
    Exit(NyxObject([NyxField('items', Observe),
      NyxField('total', NyxData(Length(FEntries) + 1)), NyxField('maximum', NyxData(9))]));
  end;

  if LMode = 'inspect' then
  begin
    NyxAgentFields(AArguments, '|mode|workspace|');
    LReference := NyxWorkspace(AArguments.Field('workspace').AsText);
    Resolve(LReference);
    Touch(AOwner, AActor, LReference);
    Exit(Describe(LReference));
  end;
  RequireAccess(True);

  if LMode <> 'create' then
  begin
    raise ENyxModel.Create('Project mode must be list, inspect or create; operator closes user projects');
  end;
  NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|label|base|');
  LOperation := AArguments.Field('operationId').AsText;

  if (AOwner = '') or (NyxTextScalarCount(AOwner) > 120) or
    (LOperation = '') or (NyxTextScalarCount(LOperation) > 120) then
  begin
    raise ENyxModel.Create('Project creation requires a bounded owner and unique operation ID');
  end;
  LRequest := AArguments.ToJSON;
  LUsed := 0;
  for LIndex := 0 to High(FReceipts) do
  begin

    if FReceipts[LIndex].Owner = AOwner then
    begin
      Inc(LUsed);

      if FReceipts[LIndex].Operation = LOperation then
      begin

        if FReceipts[LIndex].Request <> LRequest then
        begin
          raise ENyxModel.Create('Project operationId was used with different arguments');
        end;
        Exit(FReceipts[LIndex].Result.Copy);
      end;
    end;
  end;

  if LUsed >= 64 then
  begin
    raise ENyxModel.Create('Project creation receipt budget reached; reconnect explicitly');
  end;

  if AArguments.Field('base').AsText = 'empty' then
  begin
    LBase := nwbEmpty;
  end
  else if AArguments.Field('base').AsText = 'accepted' then
  begin
    LBase := nwbAccepted;
  end
  else
  begin
    raise ENyxModel.Create('Project base must be empty or accepted');
  end;
  LReference := CreateProject(AArguments.Field('label').AsText, LBase,
    AArguments.Field('expectedRevision').AsInteger);
  Result := Describe(LReference);
  LIndex := Length(FReceipts);
  SetLength(FReceipts, LIndex + 1);
  FReceipts[LIndex].Owner := AOwner;
  FReceipts[LIndex].Operation := LOperation;
  FReceipts[LIndex].Request := LRequest;
  FReceipts[LIndex].Result := Result.Copy;
  FPrimary.RecordActivity(AActor, 'nyx_workspaces', 'created project ' + LReference.ID);
end;

procedure TNyxStudioWorkspaces.CloseProject(const AReference: TNyxWorkspaceRef;
  AExpected: Integer; AConfirmed: Boolean);
var
  LSession: TNyxAgentSession;
  LIndex: Integer;
  LMove: Integer;
begin

  if not AConfirmed then
  begin
    raise ENyxModel.Create('Closing releases unsaved work and drafts; save a project backup and confirm');
  end;

  if AReference.ID = '' then
  begin
    raise ENyxModel.Create('The primary project cannot be closed through the project registry');
  end;
  LSession := Find(AReference);

  if (LSession = nil) or (AExpected <> LSession.Revision) then
  begin
    raise ENyxModel.Create('Project close revision conflict or missing workspace');
  end;
  LIndex := Index(AReference);
  LSession.Free;
  for LMove := LIndex + 1 to High(FEntries) do
  begin
    FEntries[LMove - 1] := FEntries[LMove];
  end;
  SetLength(FEntries, Length(FEntries) - 1);
  for LIndex := High(FPresence) downto 0 do
  begin

    if FPresence[LIndex].Reference.ID = AReference.ID then
    begin
      for LMove := LIndex + 1 to High(FPresence) do
      begin
        FPresence[LMove - 1] := FPresence[LMove];
      end;
      SetLength(FPresence, Length(FPresence) - 1);
    end;
  end;
  FPrimary.RecordActivity('Studio operator', 'project close', AReference.ID);
end;

procedure TNyxStudioWorkspaces.ReleaseOwner(const AOwner: TNyxText);
var
  LIndex: Integer;
  LMove: Integer;
begin
  for LIndex := High(FPresence) downto 0 do
  begin

    if FPresence[LIndex].Owner = AOwner then
    begin
      for LMove := LIndex + 1 to High(FPresence) do
      begin
        FPresence[LMove - 1] := FPresence[LMove];
      end;
      SetLength(FPresence, Length(FPresence) - 1);
    end;
  end;
  for LIndex := High(FReceipts) downto 0 do
  begin

    if FReceipts[LIndex].Owner = AOwner then
    begin
      for LMove := LIndex + 1 to High(FReceipts) do
      begin
        FReceipts[LMove - 1] := FReceipts[LMove];
      end;
      SetLength(FReceipts, Length(FReceipts) - 1);
    end;
  end;
end;

end.
