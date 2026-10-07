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

unit nyx.studio.legacy;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.studio.agents, nyx.studio.workspaces;

type
  { A legacy observing snapshot contains no serialized history or naming counter.
    Normal admission refuses reported history. ResetTestHistory is an explicit
    operator/test policy, never an inferred fallback for corrupted recovery. Both
    modes start fresh naming counters and fresh ordinary workspace identities;
    a complete native runtime checkpoint remains the production migration path. }
  TNyxLegacyHistoryPolicy = (nlhRequireEmptyHistory, nlhResetTestHistory);

  { Owns a fully admitted independent primary and project registry. Construction
    borrows only immutable descriptor values. Every exact pair/draft, public
    revision, selection, view and permission survives; unavailable history can
    reset only under the explicit policy. Old ordinary handles retire rather than
    aliasing a later project. Borrowed owners below must not be freed by callers. }
  TNyxLegacyStudioSnapshot = class
  private
    FPrimary: TNyxAgentSession;
    FWorkspaces: TNyxStudioWorkspaces;
    FMapping: TNyxDataValue;
    FHistoryResetCount: Integer;
  public
    { Entries are a bounded array: primary first (workspace empty), then up to
      eight ordinary projects. Each closed entry has workspace/label/project/
      session; session has revision/permission/selection/view/pendingDraft/
      canUndo/canRedo. Invalid/mismatched pairs, navigation, duplicate handles,
      unknown fields or reused creation identity refuse the entire constructor. }
    constructor Create(const AEntries: TNyxDataValue;
      APolicy: TNyxLegacyHistoryPolicy; const ANewIdentity: TNyxText);
    destructor Destroy; override;
    { Small copied mapping and reset disclosure; contains no authored files,
      credentials, old machine profiles or mutable session owner. }
    function Report: TNyxDataValue;
    property Primary: TNyxAgentSession read FPrimary;
    property Workspaces: TNyxStudioWorkspaces read FWorkspaces;
    property HistoryResetCount: Integer read FHistoryResetCount;
  end;

implementation

uses
  SysUtils, nyx.model, nyx.editing, nyx.studio.projects;

constructor TNyxLegacyStudioSnapshot.Create(const AEntries: TNyxDataValue;
  APolicy: TNyxLegacyHistoryPolicy; const ANewIdentity: TNyxText);
var
  LRegistry: TNyxWorkspaceRecoveryFrame;
  LFrame: TNyxAgentRecoveryFrame;
  LEntry: TNyxDataValue;
  LSession: TNyxDataValue;
  LMapping: array of TNyxDataValue;
  LPrevious: array of TNyxText;
  LOldID: TNyxText;
  LNewID: TNyxText;
  LPermission: TNyxText;
  LLabel: TNyxText;
  LReset: Boolean;
  LIndex: Integer;
  LPrior: Integer;
begin
  inherited Create;

  if not (APolicy in [nlhRequireEmptyHistory, nlhResetTestHistory]) then
  begin
    raise ENyxModel.Create('Legacy history policy is outside its closed vocabulary');
  end;

  if (AEntries.Kind <> ndArray) or (AEntries.Count < 1) or (AEntries.Count > 9) then
  begin
    raise ENyxModel.Create('Legacy snapshot requires primary and at most eight projects');
  end;
  LRegistry := Default(TNyxWorkspaceRecoveryFrame);
  LRegistry.Identity := ANewIdentity;
  LRegistry.Serial := AEntries.Count - 1;
  SetLength(LRegistry.Entries, LRegistry.Serial);
  SetLength(LMapping, AEntries.Count);
  SetLength(LPrevious, AEntries.Count);
  for LIndex := 0 to AEntries.Count - 1 do
  begin
    LEntry := AEntries.Item(LIndex);
    NyxAgentFields(LEntry, '|workspace|label|project|session|');
    LLabel := LEntry.Field('label').AsText;

    if (LLabel = '') or (NyxTextScalarCount(LLabel) > 256) then
    begin
      raise ENyxModel.Create('Legacy project label requires 1..256 characters');
    end;
    LOldID := LEntry.Field('workspace').AsText;

    if (LIndex = 0) <> (LOldID = '') then
    begin
      raise ENyxModel.Create('Legacy snapshot requires its exact primary first');
    end;
    for LPrior := 0 to LIndex - 1 do
    begin

      if LPrevious[LPrior] = LOldID then
      begin
        raise ENyxModel.Create('Legacy workspace identity is duplicated');
      end;
    end;
    LPrevious[LIndex] := LOldID;

    if LIndex > 0 then
    begin
      NyxWorkspace(LOldID);

      if Copy(LOldID, 1, Length(ANewIdentity) + 9) = ANewIdentity + '.project-' then
      begin
        raise ENyxModel.Create('Legacy bootstrap requires a fresh creation identity');
      end;
    end;
    LSession := LEntry.Field('session');
    NyxAgentFields(LSession,
      '|revision|permission|selection|view|pendingDraft|canUndo|canRedo|');
    LReset := LSession.Field('canUndo').AsBoolean or LSession.Field('canRedo').AsBoolean;

    if LReset and (APolicy <> nlhResetTestHistory) then
    begin
      raise ENyxModel.Create('Legacy history is unavailable; explicit test reset is required');
    end;
    LFrame := Default(TNyxAgentRecoveryFrame);
    LFrame.Session.Pair := DecodeNyxProject(LEntry.Field('project').AsText);

    if LFrame.Session.Pair.Pending <> LSession.Field('pendingDraft').AsBoolean then
    begin
      raise ENyxModel.Create('Legacy pending-draft metadata differs from its exact pair');
    end;
    LFrame.Session.Selection := LSession.Field('selection').AsText;
    LFrame.Session.View := LSession.Field('view').AsText;
    LFrame.Session.NextID := 0;
    LFrame.Revision := LSession.Field('revision').AsInteger;
    LFrame.Claimed := True;
    LPermission := LSession.Field('permission').AsText;

    if LPermission = 'disabled' then
    begin
      LFrame.Permission := apDisabled;
    end
    else if LPermission = 'readOnly' then
    begin
      LFrame.Permission := apReadOnly;
    end
    else if LPermission = 'edit' then
    begin
      LFrame.Permission := apEdit;
    end
    else
    begin
      raise ENyxModel.Create('Legacy permission is outside its closed vocabulary');
    end;
    LNewID := '';

    if LIndex = 0 then
    begin
      FPrimary := TNyxAgentSession.CreateRecovered(LFrame);
    end
    else
    begin
      LNewID := ANewIdentity + '.project-' + TNyxText(IntToStr(LIndex));
      LRegistry.Entries[LIndex - 1].Reference := NyxWorkspace(LNewID);
      LRegistry.Entries[LIndex - 1].LabelText := LLabel;
      LRegistry.Entries[LIndex - 1].Session := LFrame;
    end;

    if LReset then
    begin
      Inc(FHistoryResetCount);
    end;
    LMapping[LIndex] := NyxObject([NyxField('previousWorkspace', NyxData(LOldID)),
      NyxField('workspace', NyxData(LNewID)), NyxField('historyReset', NyxData(LReset))]);
  end;
  FWorkspaces := TNyxStudioWorkspaces.CreateRecovered(FPrimary, LRegistry);
  FMapping := NyxArray(LMapping);
end;

destructor TNyxLegacyStudioSnapshot.Destroy;
begin
  FWorkspaces.Free;
  FPrimary.Free;
  inherited Destroy;
end;

function TNyxLegacyStudioSnapshot.Report: TNyxDataValue;
begin
  Result := NyxObject([NyxField('projects', NyxData(FMapping.Count)),
    NyxField('historyResetCount', NyxData(FHistoryResetCount)),
    NyxField('namingCountersReset', NyxData(True)),
    NyxField('ordinaryHandlesRetired', NyxData(True)),
    NyxField('mapping', FMapping.Copy)]);
end;

end.
