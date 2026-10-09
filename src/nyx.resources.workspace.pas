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
unit nyx.resources.workspace;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.presentations, nyx.model, nyx.controls;

type
  { Compact presentation chooses one retained pane. Wide presentation shows both.
    This is editor presentation, never resource data, consent or Undo history. }
  TNyxResourceWorkspacePane = (rwpFiles, rwpEditor);

{ Caller-supplied unparented controls become children of independent scroll panes.
  The result owns those descendants; it borrows no document, controller or target
  handle. The named compact presentation belongs to the caller's registry.
  Wide layout allocates the catalog and editor side by side. Compact layout keeps
  both mounted, showing Files or Edit and an always reachable navigation row. }
function NewNyxResourceWorkspace(const AID: TNyxText;
  const ACatalog, AEditor: INyxNode; const ACompact: TNyxPresentationRef;
  APane: TNyxResourceWorkspacePane = rwpFiles): INyxColumn;
{ Stable owned scroll identity lets a host borrow its target viewport while
  retaining only copied positions. Undefined pane values refuse. }
function NyxResourceWorkspaceScrollID(const AID: TNyxText;
  APane: TNyxResourceWorkspacePane): TNyxText;
{ Stable navigation identity; deriving it never changes the workspace or pane. }
function NyxResourceWorkspaceActionID(const AID: TNyxText;
  APane: TNyxResourceWorkspacePane): TNyxText;
{ Presentation-only intent. Unknown controls return False without choosing a pane.
  Restore validates all fixed descendants before writing visibility/pressed state;
  hosts retain target scroll positions and synchronize existing mounted controls. }
function NyxResourceWorkspaceAction(ANode: TNyxNode; const AID: TNyxText;
  out APane: TNyxResourceWorkspacePane): Boolean;
procedure RestoreNyxResourceWorkspace(AWorkspace: TNyxNode;
  const ACompact: TNyxPresentationRef; APane: TNyxResourceWorkspacePane);

implementation

uses SysUtils, nyx.layout.policy;

const
  CPaneNames: array[TNyxResourceWorkspacePane] of TNyxText = ('files', 'editor');
  CPaneTitles: array[TNyxResourceWorkspacePane] of TNyxText = ('Files', 'Edit');

procedure RequirePane(APane: TNyxResourceWorkspacePane);
begin

  if not (Ord(APane) in [Ord(rwpFiles), Ord(rwpEditor)]) then
  begin
    raise ENyxModel.Create('Unsupported resource workspace pane');
  end;
end;

function NyxResourceWorkspaceScrollID(const AID: TNyxText;
  APane: TNyxResourceWorkspacePane): TNyxText;
begin
  RequirePane(APane);
  Result := AID + TNyxText('-') + CPaneNames[APane] + TNyxText('-scroll');
end;

function NyxResourceWorkspaceActionID(const AID: TNyxText;
  APane: TNyxResourceWorkspacePane): TNyxText;
begin
  RequirePane(APane);
  Result := AID + TNyxText('-show-') + CPaneNames[APane];
end;

function NyxResourceWorkspaceAction(ANode: TNyxNode; const AID: TNyxText;
  out APane: TNyxResourceWorkspacePane): Boolean;
var
  LPane: TNyxResourceWorkspacePane;
begin
  APane := rwpFiles;
  Result := False;

  if ANode = nil then
  begin
    Exit;
  end;
  for LPane := Low(TNyxResourceWorkspacePane) to High(TNyxResourceWorkspacePane) do
  begin

    if ANode.ID = NyxResourceWorkspaceActionID(AID, LPane) then
    begin
      APane := LPane;
      Exit(True);
    end;
  end;
end;

procedure RestoreNyxResourceWorkspace(AWorkspace: TNyxNode;
  const ACompact: TNyxPresentationRef; APane: TNyxResourceWorkspacePane);
var
  LPane: TNyxResourceWorkspacePane;
  LScrolls: array[TNyxResourceWorkspacePane] of TNyxNode;
  LActions: array[TNyxResourceWorkspacePane] of TNyxNode;
begin
  RequirePane(APane);

  if (AWorkspace = nil) or not ACompact.Defined then
  begin
    raise ENyxModel.Create('A resource workspace and compact presentation are required');
  end;
  for LPane := Low(TNyxResourceWorkspacePane) to High(TNyxResourceWorkspacePane) do
  begin
    LScrolls[LPane] := AWorkspace.Find(NyxResourceWorkspaceScrollID(AWorkspace.ID, LPane));
    LActions[LPane] := AWorkspace.Find(NyxResourceWorkspaceActionID(AWorkspace.ID, LPane));

    if (LScrolls[LPane] = nil) or (LActions[LPane] = nil) then
    begin
      raise ENyxModel.Create('The resource workspace is missing its fixed owned pane controls');
    end;
  end;
  for LPane := Low(TNyxResourceWorkspacePane) to High(TNyxResourceWorkspacePane) do
  begin
    LScrolls[LPane].Configure.Visible(True)
      .WhenPresentation(ACompact).Visible(LPane = APane).Done;
    LActions[LPane].Configure.Pressed(LPane = APane).Done;
  end;
end;

function NewNyxResourceWorkspace(const AID: TNyxText;
  const ACatalog, AEditor: INyxNode; const ACompact: TNyxPresentationRef;
  APane: TNyxResourceWorkspacePane): INyxColumn;
var
  LNavigation: INyxRow;
  LBody: INyxRow;
  LScroll: INyxScroll;
  LPane: TNyxResourceWorkspacePane;
begin
  RequirePane(APane);

  if (ACatalog = nil) or (AEditor = nil) or not ACompact.Defined then
  begin
    raise ENyxModel.Create('Resource workspace requires two controls and a compact presentation');
  end;

  if (ACatalog.Node = AEditor.Node) or (ACatalog.Node.Parent <> nil) or
    (AEditor.Node.Parent <> nil) then
  begin
    raise ENyxModel.Create('Resource workspace children must be distinct and unparented');
  end;
  Result := NewNyxColumn(AID);
  Result.Configure.Layout(TNyxLayoutPolicy.Column).Flex(1).HeightSizing(nsFill)
    .WidthSizing(nsFill).Gap(12).Done;
  LNavigation := NewNyxRow(AID + TNyxText('-navigation'));
  LNavigation.Configure.Layout(TNyxLayoutPolicy.Row.Wrap(nfwNoWrap)).Gap(10)
    .Visible(False).WhenPresentation(ACompact).Visible(True).Done;
  Result.Add(LNavigation);
  for LPane := Low(TNyxResourceWorkspacePane) to High(TNyxResourceWorkspacePane) do
  begin
    LNavigation.Add(NewNyxButton(NyxResourceWorkspaceActionID(AID, LPane)).Configure
      .Text(CPaneTitles[LPane]).AccessibleName('Resource ' + CPaneTitles[LPane])
      .Flex(1).Done);
  end;
  LBody := NewNyxRow(AID + TNyxText('-body'));
  LBody.Configure.Layout(TNyxLayoutPolicy.Row.Wrap(nfwNoWrap).Align(ncaStretch))
    .Flex(1).HeightSizing(nsFill).WidthSizing(nsFill).Gap(16).Done;
  Result.Add(LBody);
  for LPane := Low(TNyxResourceWorkspacePane) to High(TNyxResourceWorkspacePane) do
  begin
    LScroll := NewNyxScroll(NyxResourceWorkspaceScrollID(AID, LPane));
    LScroll.Configure.Layout(TNyxLayoutPolicy.Column).HeightSizing(nsFill).Padding(0).Gap(0).Done;

    if LPane = rwpFiles then
    begin
      LScroll.Configure.Width(300).WhenPresentation(ACompact)
        .Clear(atWidth).WidthSizing(nsFill).Flex(1).Done;
      LScroll.Add(ACatalog);
    end
    else
    begin
      LScroll.Configure.Flex(1).WidthSizing(nsFill).Done;
      LScroll.Add(AEditor);
    end;
    LBody.Add(LScroll);
  end;
  RestoreNyxResourceWorkspace(Result.Node, ACompact, APane);
end;

end.
