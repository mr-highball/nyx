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

unit nyx.studio.projectnavigation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.types, nyx.controls, nyx.events,
  nyx.studio.agentview, nyx.studio.projects;

type
  { Project navigation is editor presentation. Neither opening this panel nor
    editing a proposed name changes the accepted document or its Undo history. }
  TNyxStudioProjectPanel = (sppClosed, sppNew, sppSwitch);
  { Copied unfinished creation input, independent of the project being edited.
    The controllers retain it while controls are parked or the host resizes. }
  TNyxStudioProjectNavigation = record
    Panel: TNyxStudioProjectPanel;
    Name: TNyxText;
  end;

{ Ordinary Nyx controls shared by browser and LCL. Each switch carries the exact
  opaque project reference, never a row number. No borrowed session is retained. }
function BuildNyxStudioProjectNavigation(const AState: TNyxStudioProjectNavigation;
  const AAgents: TNyxStudioAgentView): INyxColumn;
{ Routes presentation only; actual creation, storage and navigation remain with
  the controller and its acknowledged private service connection. }
function RouteNyxStudioProjectNavigation(var AState: TNyxStudioProjectNavigation;
  ANode: TNyxNode; ATrigger: TNyxTrigger): Boolean;
{ A fresh target-independent project has one empty home page and no compiler
  prerequisite. The returned pair owns its exact design and Pascal text. }
function NewNyxStudioProjectPair(const ATitle: TNyxText): TNyxProjectPair;

implementation

uses SysUtils, nyx.data, nyx.codec, nyx.codegen, nyx.editing, nyx.presentations;

function NewNyxStudioProjectPair(const ATitle: TNyxText): TNyxProjectPair;
var
  LDocument: TNyxDocument;
begin

  if (Trim(ATitle) = '') or (NyxTextScalarCount(ATitle) > 256) then
  begin
    raise ENyxModel.Create('Enter a project name containing 1..256 characters');
  end;
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := ATitle;
    LDocument.AddPage(NewNyxPage('home'));
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  finally
    LDocument.Free;
  end;
end;

function RouteNyxStudioProjectNavigation(var AState: TNyxStudioProjectNavigation;
  ANode: TNyxNode; ATrigger: TNyxTrigger): Boolean;
begin
  Result := True;

  if (ATrigger = ntChange) and (ANode.ID = 'studio-new-project-name') then
  begin
    AState.Name := ANode.Prop('value');
  end
  else if (ATrigger = ntClick) and (ANode.ID = 'action-project-new') then
  begin
    AState.Panel := sppNew;
  end
  else if (ATrigger = ntClick) and (ANode.ID = 'action-project-switch') then
  begin
    AState.Panel := sppSwitch;
  end
  else if (ATrigger = ntClick) and (ANode.ID = 'action-project-navigation-close') then
  begin
    AState.Panel := sppClosed;
  end
  else
  begin
    Result := False;
  end;
end;

function BuildNyxStudioProjectNavigation(const AState: TNyxStudioProjectNavigation;
  const AAgents: TNyxStudioAgentView): INyxColumn;
var
  LActions: INyxGrid;
  LCard: INyxColumn;
  LButton: INyxButton;
  LIndex: Integer;
  LItem: TNyxDataValue;
  LName: TNyxText;
begin
  Result := NewNyxColumn('studio-project-navigation');
  Result.Configure.Gap(8);
  LActions := NewNyxGrid('studio-project-actions');
  LActions.Configure.Columns(1).Gap(6)
    .WhenPresentation(NyxPresentation('compact')).Columns(2);
  Result.Add(LActions);
  LActions.Add(NewNyxButton('action-project-new').WithText('New project'));
  LActions.Add(NewNyxButton('action-project-browse').WithText('Open project'));
  LActions.Add(NewNyxButton('action-project-save-current').WithText('Save project'));
  LActions.Add(NewNyxButton('action-project-switch').WithText('Switch project'));

  if AState.Panel = sppClosed then
  begin
    Exit;
  end;
  LCard := NewNyxColumn('studio-project-navigation-panel');
  LCard.Configure.Padding(10).Gap(8).Surface(True);
  Result.Add(LCard);

  if AState.Panel = sppNew then
  begin
    LCard.Add(NewNyxInput('studio-new-project-name').Configure.Text('New project name')
      .Placeholder('Untitled project').Value(AState.Name).Done);
    LCard.Add(NewNyxLabel('studio-new-project-help').WithText(
      'Start with an empty page. Your current project and Pascal draft stay open.'));
    LCard.Add(NewNyxButton('action-project-create').Configure.Text('Create project')
      .Enabled(AAgents.Connected and not AAgents.Conflict and
        AAgents.CanCreateWorkspace).Done);

    if not AAgents.CanCreateWorkspace or not AAgents.Connected then
    begin
      LCard.Add(NewNyxLabel('studio-new-project-unavailable').WithText(
        'Connect to an updated Studio service to create another open project. ' +
        'You can still open or save project files.'));
    end;
  end
  else
  begin
    LCard.Add(NewNyxLabel('studio-project-switch-help').WithText(
      'Open projects keep their own design, Pascal draft and Undo history.'));
    for LIndex := 0 to AAgents.Workspaces.Count - 1 do
    begin
      LItem := AAgents.Workspaces.Item(LIndex);
      LName := LItem.Field('session').Field('title').AsText;

      if LItem.Field('workspace').AsText = AAgents.Workspace.ID then
      begin
        LCard.Add(NewNyxLabel('studio-project-current-' + IntToStr(LIndex))
          .WithText(LName + ' (current)'));
      end
      else
      begin
        { Labels distinguish separate projects with the same document title.
          A tooltip alone is insufficient on touch-only hosts. }

        if LItem.Field('label').AsText <> LName then
        begin
          LName := LName + ' / ' + LItem.Field('label').AsText;
        end;
        LButton := NewNyxButton('studio-project-switch-' + IntToStr(LIndex));
        LButton.Configure.Text(LName).Hint(LItem.Field('label').AsText)
          .Enabled(AAgents.Connected and not AAgents.Conflict and not AAgents.Busy);
        LButton.Node.Extensions.SetValue(NyxStudioWorkspaceJumpKey, LItem.Field('workspace'));
        LCard.Add(LButton);
      end;
    end;

    if not AAgents.Connected then
    begin
      LCard.Add(NewNyxLabel('studio-project-switch-unavailable').WithText(
        'Connect to a Studio service to see its open projects.'));
    end;
  end;
  LCard.Add(NewNyxButton('action-project-navigation-close').WithText('Done'));
end;

end.
