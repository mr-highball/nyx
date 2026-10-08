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

unit nyx.studio.buildlaunches;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.studio.editorbuild, nyx.studio.workspaces,
  nyx.studio.builds;

type
  { Transient bounded launch mailbox. Caller holds the Studio authority lock and
    rechecks the exact compiler/session pair before admission, delivery and grant.
    Sixteen project intents and 64 actor/context-bound retry receipts are retained.
    No document, widget, owner credential or process is owned here. Recovery never
    serializes this mailbox: a restarted host cannot replay old execution intent. }
  TNyxStudioBuildLaunches = class
  private
    FSequence: Integer;
    FEntries: array of record
      Workspace: TNyxWorkspaceRef;
      Launch: TNyxCompilerLaunch;
      Retired: Boolean;
      Results: array[TNyxBuildTarget] of TNyxDataValue;
    end;
    FReceipts: array of record
      Owner: TNyxText;
      Operation: TNyxText;
      Arguments: TNyxText;
      Reply: TNyxDataValue;
    end;
    function IndexOf(const AWorkspace: TNyxWorkspaceRef): Integer;
  public
    { Validate all shape/types before retry or source capture. }
    procedure Admit(const AArguments: TNyxDataValue);
    { Exact retry returns its original receipt and never publishes another
      sequence. Reusing an operation for changed arguments refuses. }
    function Retry(const AOwner: TNyxText; const AArguments: TNyxDataValue;
      out AReply: TNyxDataValue): Boolean;
    { ABuild is already a successful, current, exact-context status receipt. }
    function Request(const AWorkspace: TNyxWorkspaceRef;
      const AOwner, AActor: TNyxText; const AArguments,
      ABuild: TNyxDataValue): TNyxDataValue;
    { Detached last intent; null when this context has never requested a mount.
      Currentness is deliberately the model/worker owner's responsibility. }
    function Pending(const AWorkspace: TNyxWorkspaceRef): TNyxDataValue;
    { Definitive source/output/permission retirement cannot become executable
      again after an Undo or profile restoration. Receipts remain retryable. }
    procedure Retire(const AWorkspace: TNyxWorkspaceRef);
    { Global permission or machine-output changes retire every project intent,
      including projects with no observing window at that instant. }
    procedure RetireAll;
    { DELETE retires transport identity, so its exact retries are no longer
      reachable. Release their memory without stopping an already mounted view. }
    procedure ReleaseOwner(const AOwner: TNyxText);
    { Operator-confirmed project closure frees its mailbox slot. Old context
      identities cannot be reused by a different project. }
    procedure Forget(const AWorkspace: TNyxWorkspaceRef);
    { Latest acknowledgment per adapter, not an assertion about every observing
      window or execution success. Private editor capability is checked outside. }
    function Acknowledge(const AWorkspace: TNyxWorkspaceRef;
      const AArguments: TNyxDataValue): TNyxDataValue;
  end;

implementation

uses nyx.model, nyx.editing, nyx.studio.agents;

function TNyxStudioBuildLaunches.IndexOf(const AWorkspace: TNyxWorkspaceRef): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex].Workspace.ID = AWorkspace.ID then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

procedure TNyxStudioBuildLaunches.Admit(const AArguments: TNyxDataValue);
begin
  NyxAgentFields(AArguments, '|mode|job|expectedRevision|operationId|');
  NyxBuildJob(AArguments.Field('job').AsText);
  NyxBuildOperation(AArguments.Field('operationId').AsText);

  if AArguments.Field('expectedRevision').AsInteger < 1 then
  begin
    raise ENyxModel.Create('Launch requires a positive exact revision');
  end;
end;

function TNyxStudioBuildLaunches.Retry(const AOwner: TNyxText;
  const AArguments: TNyxDataValue; out AReply: TNyxDataValue): Boolean;
var
  LIndex: Integer;
begin
  AReply := NyxNull;
  for LIndex := 0 to High(FReceipts) do
  begin

    if (FReceipts[LIndex].Owner = AOwner) and
      (FReceipts[LIndex].Operation = AArguments.Field('operationId').AsText) then
    begin

      if FReceipts[LIndex].Arguments <> AArguments.ToJSON then
      begin
        raise ENyxModel.Create('Launch operationId already belongs to different arguments');
      end;
      AReply := FReceipts[LIndex].Reply.Copy;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxStudioBuildLaunches.Pending(const AWorkspace: TNyxWorkspaceRef): TNyxDataValue;
var
  LIndex: Integer;
  LLaunch: TNyxCompilerLaunch;
  LState: TNyxText;
begin
  LIndex := IndexOf(AWorkspace);

  if LIndex < 0 then
  begin
    Exit(NyxNull);
  end;
  LLaunch := FEntries[LIndex].Launch;
  LState := 'requested';

  if FEntries[LIndex].Retired then
  begin
    LState := 'retired';
  end;
  Result := NyxObject([NyxField('sequence', NyxData(LLaunch.Sequence)),
    NyxField('job', NyxData(LLaunch.Job.ID)), NyxField('revision', NyxData(LLaunch.Revision)),
    NyxField('outputID', NyxData(LLaunch.Output.ID)), NyxField('actor', NyxData(LLaunch.Actor)),
    NyxField('target', NyxData(NyxBuildTargetName(LLaunch.Target))),
    NyxField('scope', NyxData(NyxBuildScopeName(LLaunch.Scope))),
    NyxField('view', NyxData(LLaunch.Root.ID)), NyxField('state', NyxData(LState)),
    NyxField('browser', FEntries[LIndex].Results[btBrowser]),
    NyxField('lcl', FEntries[LIndex].Results[btNativeLCL])]);
end;

procedure TNyxStudioBuildLaunches.Retire(const AWorkspace: TNyxWorkspaceRef);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AWorkspace);

  if LIndex >= 0 then
  begin
    FEntries[LIndex].Retired := True;
  end;
end;

procedure TNyxStudioBuildLaunches.RetireAll;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FEntries) do
  begin
    FEntries[LIndex].Retired := True;
  end;
end;

function TNyxStudioBuildLaunches.Request(const AWorkspace: TNyxWorkspaceRef;
  const AOwner, AActor: TNyxText; const AArguments,
  ABuild: TNyxDataValue): TNyxDataValue;
var
  LIndex: Integer;
  LReceipt: Integer;
  LEntry: TNyxCompilerLaunch;
begin

  if (Length(FReceipts) >= 64) or (FSequence = High(Integer)) then
  begin
    raise ENyxModel.Create('Launch receipt budget is full; current previews are retained');
  end;
  LIndex := IndexOf(AWorkspace);

  if (LIndex < 0) and (Length(FEntries) >= 16) then
  begin
    raise ENyxModel.Create('Launch context budget is full; current previews are retained');
  end;
  LEntry := DecodeNyxCompilerLaunch(NyxObject([
    NyxField('sequence', NyxData(FSequence + 1)), NyxField('job', ABuild.Field('job')),
    NyxField('revision', ABuild.Field('revision')), NyxField('outputID', ABuild.Field('outputID')),
    NyxField('actor', NyxData(AActor)), NyxField('target', ABuild.Field('target')),
    NyxField('scope', ABuild.Field('scope')), NyxField('view', ABuild.Field('view'))]));

  if LIndex < 0 then
  begin
    LIndex := Length(FEntries);
    SetLength(FEntries, LIndex + 1);
  end;
  FEntries[LIndex].Workspace := AWorkspace;
  FEntries[LIndex].Launch := LEntry;
  FEntries[LIndex].Retired := False;
  FEntries[LIndex].Results[btBrowser] := NyxNull;
  FEntries[LIndex].Results[btNativeLCL] := NyxNull;
  FSequence := LEntry.Sequence;
  Result := Pending(AWorkspace);
  LReceipt := Length(FReceipts);
  SetLength(FReceipts, LReceipt + 1);
  FReceipts[LReceipt].Owner := AOwner;
  FReceipts[LReceipt].Operation := AArguments.Field('operationId').AsText;
  FReceipts[LReceipt].Arguments := AArguments.ToJSON;
  FReceipts[LReceipt].Reply := Result.Copy;
end;

procedure TNyxStudioBuildLaunches.ReleaseOwner(const AOwner: TNyxText);
var
  LRead: Integer;
  LWrite: Integer;
begin
  LWrite := 0;
  for LRead := 0 to High(FReceipts) do
  begin

    if TNyxDataValue.ParseJSON(FReceipts[LRead].Owner).Field('owner').AsText <> AOwner then
    begin
      FReceipts[LWrite] := FReceipts[LRead];
      Inc(LWrite);
    end;
  end;
  SetLength(FReceipts, LWrite);
end;

procedure TNyxStudioBuildLaunches.Forget(const AWorkspace: TNyxWorkspaceRef);
var
  LIndex: Integer;
  LNext: Integer;
  LRead: Integer;
  LWrite: Integer;
begin
  { Closed contexts cannot be addressed even by their original connection.
    Release unreachable receipts as well as the intent, so repeated disposable
    project work does not consume the launch budget indefinitely. }
  LWrite := 0;
  for LRead := 0 to High(FReceipts) do
  begin

    if TNyxDataValue.ParseJSON(FReceipts[LRead].Owner).Field('workspace').AsText <> AWorkspace.ID then
    begin
      FReceipts[LWrite] := FReceipts[LRead];
      Inc(LWrite);
    end;
  end;
  SetLength(FReceipts, LWrite);
  LIndex := IndexOf(AWorkspace);

  if LIndex < 0 then
  begin
    Exit;
  end;
  for LNext := LIndex + 1 to High(FEntries) do
  begin
    FEntries[LNext - 1] := FEntries[LNext];
  end;
  SetLength(FEntries, Length(FEntries) - 1);
end;

function TNyxStudioBuildLaunches.Acknowledge(const AWorkspace: TNyxWorkspaceRef;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LIndex: Integer;
  LHost: TNyxBuildTarget;
  LResult: TNyxText;
begin
  NyxAgentFields(AArguments, '|mode|job|sequence|host|result|detail|');
  LHost := ParseNyxBuildTarget(AArguments.Field('host').AsText);
  LResult := AArguments.Field('result').AsText;

  if (LResult <> 'mounted') and (LResult <> 'unavailable') and (LResult <> 'refused') then
  begin
    raise ENyxModel.Create('Unknown observer launch acknowledgment');
  end;

  if NyxTextScalarCount(AArguments.Field('detail').AsText) > 256 then
  begin
    raise ENyxModel.Create('Launch acknowledgment detail exceeds its Unicode bound');
  end;
  LIndex := IndexOf(AWorkspace);

  if (LIndex < 0) or FEntries[LIndex].Retired or
    (FEntries[LIndex].Launch.Job.ID <> AArguments.Field('job').AsText) or
    (FEntries[LIndex].Launch.Sequence <> AArguments.Field('sequence').AsInteger) then
  begin
    raise ENyxModel.Create('Launch was replaced; old acknowledgment refused');
  end;
  FEntries[LIndex].Results[LHost] := NyxObject([
    NyxField('result', NyxData(LResult)), NyxField('detail', AArguments.Field('detail'))]);
  Result := Pending(AWorkspace);
end;

end.
