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

unit nyx.studio.agentview;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.responsive, nyx.model, nyx.studio.agents,
  nyx.studio.workspaces, nyx.studio.editorbuild;

type
  { Immutable-by-copy private editor presentation. No editor/MCP credential,
    borrowed widget or document reference is retained. A private BuildReply may
    carry a transient producer grant; never export this record as design data. }
  TNyxStudioAgentView = record
    Connected: Boolean;
    Busy: Boolean;
    Conflict: Boolean;
    Permission: TNyxAgentPermission;
    Revision: Integer;
    Endpoint: TNyxText;
    Status: TNyxText;
    Activity: TNyxDataValue;
    { Bounded compiler observer metadata, with no duplicated Pascal source. }
    Compiler: TNyxDataValue;
    { At most eight trusted runtime summaries, without payloads or authority.
      Empty means no host has enrolled; authored defaults are not observations. }
    ResourceRuntimes: TNyxDataValue;
    { Negotiated with the connected service; older servers keep ordinary preview
      execution without requesting a reporting operation they do not expose. }
    CanReportRuntime: Boolean;
    { Operator-only bounded review summaries; no transport owner credentials or
      user document buffers. Preview links show independent, live review views. }
    Reviews: TNyxDataValue;
    { Full editor context is fixed for this observer, independently of agents.
      Project summaries include bounded session/activity metadata only. }
    Workspace: TNyxWorkspaceRef;
    Workspaces: TNyxDataValue;
    { Operator consent is an exact project/revision snapshot, never a label or
      current list index. It is ephemeral UI state, not a persisted design. }
    CloseWorkspace: TNyxWorkspaceRef;
    CloseRevision: Integer;
    CloseLabel: TNyxText;
    { Explicit editor capability advertisement. A newer client must not offer
      confirmation against an older service that lacks operator closure. }
    CanCloseWorkspace: Boolean;
    { Private compiler capability and its latest bounded reply. Job replies omit
      full source/document ownership. Operator profile replies contain local
      paths; never export this private view as a public MCP response or design. }
    CanBuild: Boolean;
    { Separate capability gates newer job-list/cancel UI against older hosts.
      BuildJobs is bounded active metadata, not source or machine profiles. }
    CanControlBuilds: Boolean;
    BuildJobs: TNyxDataValue;
    BuildReply: TNyxDataValue;
    BuildReplyKind: TNyxCompilerOperation;
    BuildReplySequence: Integer;
  end;

function DefaultNyxStudioAgentView: TNyxStudioAgentView;
{ Typed editor-only URL metadata. Target controllers decide how to show this
  independent view; it never routes a document mutation or a control property. }
function NyxStudioReviewPreviewKey: TNyxExtensionRef;
{ Typed editor navigation metadata. Empty denotes the primary project; it is
  never a document root ID, compiler target or agent-retargeting instruction. }
function NyxStudioWorkspaceJumpKey: TNyxExtensionRef;
{ Operator-only closure metadata; agents have no project-close tool. }
function NyxStudioWorkspaceCloseKey: TNyxExtensionRef;
{ Owned ordinary Nyx controls; both adapters can mount the same permission and
  activity UI. Controllers route explicit operator actions to their service. }
function BuildNyxStudioAgents(const AState: TNyxStudioAgentView): TNyxNode;

implementation

function NyxStudioReviewPreviewKey: TNyxExtensionRef;
begin
  Result := NyxExtension('studio.review-preview');
end;

function NyxStudioWorkspaceJumpKey: TNyxExtensionRef;
begin
  Result := NyxExtension('studio.workspace-jump');
end;

function NyxStudioWorkspaceCloseKey: TNyxExtensionRef;
begin
  Result := NyxExtension('studio.workspace-close');
end;

function DefaultNyxStudioAgentView: TNyxStudioAgentView;
begin
  Result.Connected := False;
  Result.Busy := False;
  Result.Conflict := False;
  Result.Permission := apEdit;
  Result.Revision := 0;
  Result.Endpoint := '';
  Result.Status := 'Connecting agent session';
  Result.Activity := NyxArray([]);
  Result.Compiler := NyxNull;
  Result.ResourceRuntimes := NyxArray([]);
  Result.Reviews := NyxArray([]);
  Result.Workspace := NyxPrimaryWorkspace;
  Result.Workspaces := NyxArray([]);
  Result.CloseWorkspace := NyxPrimaryWorkspace;
  Result.CloseRevision := 0;
  Result.CloseLabel := '';
  Result.CanCloseWorkspace := False;
  Result.CanBuild := False;
  Result.CanControlBuilds := False;
  Result.BuildJobs := NyxObject([NyxField('offset', NyxData(0)), NyxField('total', NyxData(0)),
    NyxField('queued', NyxData(0)), NyxField('running', NyxData(0)),
    NyxField('cancelling', NyxData(0)), NyxField('items', NyxArray([]))]);
  Result.BuildReplyKind := coNone;
  Result.BuildReply := NyxNull;
  Result.BuildReplySequence := 0;
end;

function LabelNode(const AID, AText: TNyxText): TNyxNode;
begin
  Result := TNyxNode.Create(nkLabel, AID).Configure.Text(AText).Done;
end;

function BuildNyxStudioAgents(const AState: TNyxStudioAgentView): TNyxNode;
const
  CActivitySeparator: TNyxText = ' · ';
var
  LButtons: TNyxNode;
  LButton: TNyxNode;
  LActivity: TNyxNode;
  LPermission: TNyxAgentPermission;
  LIndex: Integer;
  LItem: TNyxDataValue;
  LCaption: TNyxText;
  LReview: TNyxNode;
  LSummary: TNyxDataValue;
  LConnection: Integer;
  LWorkspace: TNyxNode;
  LWorkspaceKey: TNyxText;
begin
  Result := TNyxNode.Create(nkCard, 'studio-agents');
  try
    Result.Configure.Layout(nlColumn).Gap(10).Padding(16).Surface(True)
      .WhenViewport(TNyxViewportWidth.Below(640)).Gap(6).Padding(10).Done;
    Result.Add(TNyxNode.Create(nkHeading, 'studio-agents-title').Configure.Text('Agents')
      .WhenViewport(TNyxViewportWidth.Below(640)).Visible(False).Done);
    Result.Add(LabelNode('studio-agents-status', AState.Status));

    if AState.Conflict then
    begin
      { Resolve choices precede optional configuration/help. An observing phone
        can reach them without scrolling past transport and permission details.
        The controller still downloads a backup before accepting the shared pair. }
      Result.Find('studio-agents-status').Configure
        .WhenViewport(TNyxViewportWidth.Below(640))
        .Text('This device has a different saved project.').Done;
      Result.Add(LabelNode('studio-agent-conflict',
        'Your local changes are retained. Another view or agent changed the shared revision. ' +
        'Save a project backup before choosing the shared design.').Configure
        .WhenViewport(TNyxViewportWidth.Below(640))
        .Text('Choose the shared project after saving a backup, or keep this device separate.').Done);
      Result.Add(TNyxNode.Create(nkButton, 'action-agent-pause')
        .Configure.Text('Keep local and pause sync').Done);
      LButton := TNyxNode.Create(nkButton, 'action-agent-accept')
        .Configure.Text('Download local backup and use shared design').Done;
      LButton.Configure.ForPlatform(npfNativeLCL)
        .Text('Save local backup and use shared design').Done;
      LButton.Configure.WhenViewport(TNyxViewportWidth.Below(640))
        .Text('Back up and use shared').Done;
      Result.Add(LButton);
    end;
    { Revision already appears in connected status. Redundant transport/help
      remains available at wider widths without occupying scarce compact space.
      Public viewport rules preserve these controls and their ownership. }
    Result.Add(LabelNode('studio-agents-revision', 'Shared revision ' + IntToStr(AState.Revision))
      .Configure.WhenViewport(TNyxViewportWidth.Below(640)).Visible(False).Done);
    Result.Add(LabelNode('studio-agents-endpoint', AState.Endpoint)
      .Configure.WhenViewport(TNyxViewportWidth.Below(640)).Visible(False).Done);

    if (AState.Compiler.Kind = ndObject) and
      (AState.Compiler.Field('total').AsInteger > 0) then
    begin
      Result.Add(LabelNode('studio-agents-compiler-summary',
        'Build diagnostics: ' + IntToStr(AState.Compiler.Field('items').Count) +
        ' shown of ' + IntToStr(AState.Compiler.Field('total').AsInteger) +
        '. Errors are shown first.'));
    end;
    Result.Add(LabelNode('studio-agents-explanation',
      'Agents use semantic tools against this design. You control access here. ' +
      'Codex configuration is updated locally; a client already running may need to reconnect.')
      .Configure.WhenViewport(TNyxViewportWidth.Below(640)).Visible(False).Done);
    LButtons := TNyxNode.Create(nkRow, 'studio-agents-permissions').Configure.Gap(8).Done;
    Result.Add(LButtons);
    for LPermission := Low(TNyxAgentPermission) to High(TNyxAgentPermission) do
    begin
      case LPermission of
        apDisabled: LCaption := 'Disabled';
        apReadOnly: LCaption := 'Read only';
        apEdit: LCaption := 'Allow edits';
      end;
      LButton := TNyxNode.Create(nkButton, 'action-agent-' + NyxAgentPermissionName(LPermission));
      LButton.Configure.Text(LCaption).Enabled(AState.Connected and not AState.Busy).Done;

      if LPermission = AState.Permission then
      begin
        LButton.Configure.Variant(nvPrimary).Done;
      end;
      LButtons.Add(LButton);
    end;

    if not AState.Connected then
    begin
      Result.Add(TNyxNode.Create(nkButton, 'action-agent-connect').Configure.Text('Connect agents').Done);
    end;

    if AState.Reviews.Defined then
    begin
      for LIndex := 0 to AState.Reviews.Count - 1 do
      begin
        LItem := AState.Reviews.Item(LIndex);
        LSummary := LItem.Field('session');
        LReview := TNyxNode.Create(nkCard, 'studio-agent-review-' + IntToStr(LIndex))
          .Configure.Gap(6).Padding(12).Done;
        Result.Add(LReview);
        LReview.Add(LabelNode('studio-agent-review-label-' + IntToStr(LIndex),
          LItem.Field('label').AsText));
        LReview.Add(LabelNode('studio-agent-review-owner-' + IntToStr(LIndex),
          LItem.Field('actor').AsText + ' / revision ' +
          IntToStr(LSummary.Field('revision').AsInteger)));
        LReview.Add(LabelNode('studio-agent-review-explanation-' + IntToStr(LIndex),
          'Independent review. Your project and Undo history are retained.'));

        if NyxAgentHas(LItem, 'preview') then
        begin
          LButton := TNyxNode.Create(nkButton, 'studio-agent-review-preview-' + IntToStr(LIndex))
            .Configure.Text('Watch live review').Done;
          LButton.Extensions.SetValue(NyxStudioReviewPreviewKey, LItem.Field('preview'));
          LReview.Add(LButton);
        end;
      end;
    end;
    for LIndex := 0 to AState.Workspaces.Count - 1 do
    begin
      LItem := AState.Workspaces.Item(LIndex);
      LSummary := LItem.Field('session');
      LWorkspaceKey := LItem.Field('workspace').AsText;

      if LWorkspaceKey = '' then
      begin
        LWorkspaceKey := 'primary';
      end;
      { Navigation controls keep their exact project identity when another
        project closes. An array position must never identify an action target. }
      LWorkspace := TNyxNode.Create(nkCard, 'studio-agent-workspace-' + LWorkspaceKey)
        .Configure.Layout(nlColumn).Gap(6).Padding(12).Done;
      Result.Add(LWorkspace);
      LWorkspace.Add(LabelNode('studio-agent-workspace-label-' + LWorkspaceKey,
        LItem.Field('label').AsText + ' / ' + LSummary.Field('title').AsText));
      LWorkspace.Add(LabelNode('studio-agent-workspace-revision-' + LWorkspaceKey,
        'Project / revision ' + IntToStr(LSummary.Field('revision').AsInteger) +
        ' / ' + IntToStr(LItem.Field('connections').Count) + ' agent sessions'));
      for LConnection := 0 to LItem.Field('connections').Count - 1 do
      begin
        LWorkspace.Add(LabelNode('studio-agent-workspace-connection-' +
          LWorkspaceKey + '-' + IntToStr(LConnection),
          LItem.Field('connections').Item(LConnection).Field('actor').AsText +
          ' / session ' + IntToStr(LItem.Field('connections').Item(LConnection)
            .Field('session').AsInteger)));
      end;

      if LItem.Field('workspace').AsText = AState.Workspace.ID then
      begin
        LWorkspace.Add(LabelNode('studio-agent-workspace-current-' + LWorkspaceKey,
          'You are editing this project. Agents retain their own explicit targets.'));
      end
      else
      begin
        LButton := TNyxNode.Create(nkButton, 'studio-agent-workspace-jump-' + LWorkspaceKey)
          .Configure.Text('Jump into project').Enabled(AState.Connected and not AState.Conflict).Done;
        LButton.Extensions.SetValue(NyxStudioWorkspaceJumpKey, LItem.Field('workspace'));
        LWorkspace.Add(LButton);

        if LItem.Field('workspace').AsText <> '' then
        begin
          LButton := TNyxNode.Create(nkButton, 'studio-agent-workspace-close-' + LWorkspaceKey)
            .Configure.Text('Close project...')
            .Enabled(AState.Connected and not AState.Conflict and not AState.Busy).Done;
          LButton.Extensions.SetValue(NyxStudioWorkspaceCloseKey, LItem.Field('workspace'));
          LWorkspace.Add(LButton);
        end;
      end;
    end;

    if AState.CloseWorkspace.ID <> '' then
    begin
      LWorkspace := TNyxNode.Create(nkCard, 'studio-agent-workspace-close-warning')
        .Configure.Layout(nlColumn).Gap(8).Padding(12).Done;
      Result.Add(LWorkspace);
      LWorkspace.Add(LabelNode('studio-agent-workspace-close-title',
        'Close ' + AState.CloseLabel + '?'));
      LWorkspace.Add(LabelNode('studio-agent-workspace-close-explanation',
        'Unsaved work, pending Pascal drafts and Undo history in this project will be released. ' +
        'Save a project backup first. Agents using it will lose access; running builds retain their inputs. ' +
        'Confirmation applies only to revision ' + IntToStr(AState.CloseRevision) + '.'));
      LWorkspace.Add(TNyxNode.Create(nkButton, 'action-workspace-close-cancel')
        .Configure.Text('Keep project').Done);
      LWorkspace.Add(TNyxNode.Create(nkButton, 'action-workspace-close-confirm')
        .Configure.Text('Close and release unsaved work')
        .Enabled(AState.CanCloseWorkspace and AState.Connected and
          not AState.Conflict and not AState.Busy).Done);

      if not AState.CanCloseWorkspace then
      begin
        LWorkspace.Add(LabelNode('studio-agent-workspace-close-unavailable',
          'Closing projects is unavailable for this connection. Your project remains open.'));
      end;
    end;
    Result.Add(LabelNode('studio-agents-activity-title', 'Recent agent activity'));
    LActivity := TNyxNode.Create(nkScroll, 'studio-agents-activity')
      .Configure.Layout(nlColumn).Gap(8).Height(220).Done;
    Result.Add(LActivity);

    if AState.Activity.Count = 0 then
    begin
      LActivity.Add(LabelNode('studio-agent-empty', 'Ready for an agent to inspect or edit.'));
    end;
    for LIndex := AState.Activity.Count - 1 downto 0 do
    begin
      LItem := AState.Activity.Item(LIndex);
      LActivity.Add(LabelNode('studio-agent-activity-' + IntToStr(LIndex),
        LItem.Field('actor').AsText + CActivitySeparator + LItem.Field('operation').AsText +
        CActivitySeparator + LItem.Field('outcome').AsText + CActivitySeparator + 'r' +
        IntToStr(LItem.Field('revision').AsInteger)));
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
