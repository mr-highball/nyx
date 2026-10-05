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
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.studio.agents;

type
  { Immutable-by-copy observer presentation. No transport credentials, borrowed
    widgets or references to the active document appear in this record. }
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
    { Operator-only bounded review summaries; no transport owner credentials or
      user document buffers. Preview links show independent, live review views. }
    Reviews: TNyxDataValue;
  end;

function DefaultNyxStudioAgentView: TNyxStudioAgentView;
{ Typed editor-only URL metadata. Target controllers decide how to show this
  independent view; it never routes a document mutation or a control property. }
function NyxStudioReviewPreviewKey: TNyxExtensionRef;
{ Owned ordinary Nyx controls; both adapters can mount the same permission and
  activity UI. Controllers route explicit operator actions to their service. }
function BuildNyxStudioAgents(const AState: TNyxStudioAgentView): TNyxNode;

implementation

function NyxStudioReviewPreviewKey: TNyxExtensionRef;
begin
  Result := NyxExtension('studio.review-preview');
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
  Result.Reviews := NyxArray([]);
end;

function LabelNode(const AID, AText: TNyxText): TNyxNode;
begin
  Result := TNyxNode.Create(nkLabel, AID).Configure.Text(AText).Done;
end;

function BuildNyxStudioAgents(const AState: TNyxStudioAgentView): TNyxNode;
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
begin
  Result := TNyxNode.Create(nkCard, 'studio-agents');
  try
    Result.Configure.Layout(nlColumn).Gap(10).Padding(16).Surface(True).Done;
    Result.Add(TNyxNode.Create(nkHeading, 'studio-agents-title').Configure.Text('Agents').Done);
    Result.Add(LabelNode('studio-agents-status', AState.Status));
    Result.Add(LabelNode('studio-agents-revision', 'Shared revision ' + IntToStr(AState.Revision)));
    Result.Add(LabelNode('studio-agents-endpoint', AState.Endpoint));

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
      'Codex configuration is updated locally; a client already running may need to reconnect.'));
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

    if AState.Conflict then
    begin
      Result.Add(LabelNode('studio-agent-conflict',
        'Your local changes are retained. Another view or agent changed the shared revision. ' +
        'Save a project backup before choosing the shared design.'));
      Result.Add(TNyxNode.Create(nkButton, 'action-agent-pause')
        .Configure.Text('Keep local and pause sync').Done);
      Result.Add(TNyxNode.Create(nkButton, 'action-agent-accept')
        .Configure.Text('Download local backup and use shared design').Done);
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
        LItem.Field('actor').AsText + ' · ' + LItem.Field('operation').AsText +
        ' · ' + LItem.Field('outcome').AsText + ' · r' +
        IntToStr(LItem.Field('revision').AsInteger)));
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
