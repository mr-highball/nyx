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
unit nyx.studio.sections;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.root.types, nyx.collections,
  nyx.studio.hierarchy;

type
  { These are editor presentation roles, never application output targets.
    Inactive compact panels have no document; their mounted lifetimes can be
    parked by the target adapter without retaining an authored design. }
  { Workspace details contain agent activity, jobs and output configuration.
    Their changing descendants must not retire another section's live inputs.
    Within a composed Design area, its stable details root keeps membership
    stable during activity. Inactive compact Design areas can still be omitted. }
  TNyxStudioSection = (nssChrome, nssProject, nssInspector, nssResources, nssDetails);
  { Complete preserves every creator context by default. The stock composer
    explicitly declares its editor-only hierarchy dependency; custom composers
    using that default from callbacks/recipes can retain the complete context. }
  TNyxStudioSectionContext = (nscComplete, nscEditorOwnedHierarchy);

  { Owns independent copies of the existing shared Nyx shell composition.
    Chrome substitutes empty public panel hosts for Project/Inspector/Resources/Details
    roots. Other sections retain their complete compounds and private context.
    No borrowed subtree is attached to a second owner; failure releases all
    copies and leaves the caller's source document unchanged. }
  TNyxStudioSectionDocuments = class
  private
    FDocuments: array[TNyxStudioSection] of TNyxDocument;
    function GetDocument(ASection: TNyxStudioSection): TNyxDocument;
  public
    constructor Create(AShell: TNyxDocument;
      AContext: TNyxStudioSectionContext = nscComplete);
    destructor Destroy; override;
    property Documents[ASection: TNyxStudioSection]: TNyxDocument read GetDocument; default;
  end;

  { A lookup forest, deliberately not a TNyxNode. Each registered root remains
    owned by its independent view. The view owner clears/replaces a registration
    before retiring that root and outlives this borrowed lookup object. Lookups
    return actual mounted nodes for compound commands and draft capture; an
    ambiguous ID raises instead of choosing a different component silently. }
  TNyxStudioSectionRoots = class
  private
    FRoots: array[TNyxStudioSection] of TNyxNode;
    function GetRoot(ASection: TNyxStudioSection): TNyxNode;
    function GetID: TNyxText;
  public
    procedure SetRoot(ASection: TNyxStudioSection; ARoot: TNyxNode);
    function Find(const AID: TNyxText): TNyxNode;
    { Nil when the ID is unmounted. RootFor preserves the real ancestry and
      ownership of the returned node's view; it never synthesizes a model tree. }
    function RootFor(const AID: TNyxText): TNyxNode;
    function SectionFor(const AID: TNyxText; out ASection: TNyxStudioSection): Boolean;
    property Roots[ASection: TNyxStudioSection]: TNyxNode read GetRoot;
    { Compatibility identity for callers identifying the chrome view only. }
    property ID: TNyxText read GetID;
  end;

const
  NyxStudioProjectMountID = 'studio-project-mount';
  NyxStudioInspectorMountID = 'studio-inspector-mount';
  NyxStudioResourcesMountID = 'studio-resources-mount';
  NyxStudioDetailsMountID = 'studio-details-mount';

{ Canonical role-to-root and role-to-host mapping. Chrome is the surrounding
  frame, so it has no child mount ID; undefined role values raise. }
function NyxStudioSectionRootID(ASection: TNyxStudioSection): TNyxText;
function NyxStudioSectionMountID(ASection: TNyxStudioSection): TNyxText;

implementation

procedure RequireSection(AOrdinal: Integer);
begin

  if (AOrdinal < Ord(Low(TNyxStudioSection))) or
    (AOrdinal > Ord(High(TNyxStudioSection))) then
  begin
    raise ENyxModel.Create('Unknown Studio section role');
  end;
end;

procedure RequireContext(AOrdinal: Integer);
begin

  if (AOrdinal < Ord(Low(TNyxStudioSectionContext))) or
    (AOrdinal > Ord(High(TNyxStudioSectionContext))) then
  begin
    raise ENyxModel.Create('Unknown Studio section context policy');
  end;
end;

function NyxStudioSectionRootID(ASection: TNyxStudioSection): TNyxText;
begin
  RequireSection(Ord(ASection));
  case ASection of
    nssChrome:
      begin
        Result := 'studio-shell';
      end;
    nssProject:
      begin
        Result := 'studio-left';
      end;
    nssInspector:
      begin
        Result := 'studio-right';
      end;
    nssResources:
      begin
        Result := 'studio-resources';
      end;
    nssDetails:
      begin
        Result := 'studio-details';
      end;
  end;
end;

function NyxStudioSectionMountID(ASection: TNyxStudioSection): TNyxText;
begin
  RequireSection(Ord(ASection));
  case ASection of
    nssChrome:
      begin
        Result := '';
      end;
    nssProject:
      begin
        Result := NyxStudioProjectMountID;
      end;
    nssInspector:
      begin
        Result := NyxStudioInspectorMountID;
      end;
    nssResources:
      begin
        Result := NyxStudioResourcesMountID;
      end;
    nssDetails:
      begin
        Result := NyxStudioDetailsMountID;
      end;
  end;
end;

function ExtractSection(ADocument: TNyxDocument; ASection: TNyxStudioSection;
  AReplace: Boolean): TNyxNode;
var
  LParent: TNyxNode;
  LIndex: Integer;
  LHost: INyxPanel;
begin
  Result := ADocument.Find(NyxStudioSectionRootID(ASection));

  if Result = nil then
  begin
    Exit;
  end;
  LParent := Result.Parent;

  if LParent = nil then
  begin
    raise ENyxModel.Create('A Studio child section must belong to the workspace');
  end;
  LIndex := 0;
  while LParent.Children[LIndex] <> Result do
  begin
    Inc(LIndex);
  end;
  LHost := nil;

  if AReplace then
  begin
    LHost := NewNyxPanel(NyxStudioSectionMountID(ASection));
    { Explicit internal composition boundary: preserve creator-supplied outer
      layout/platform directives while removing the scroll view's inner spacing.
      The original scroll root remains responsible for its content styling. }
    LHost.Node.Props.Assign(Result.Props);
    LHost.Configure.Layout(nlColumn).Padding(0).Gap(0).HeightSizing(nsFill)
      .ForPlatform(npfNativeLCL).Padding(0).Gap(0).HeightSizing(nsFill).Done;
  end;
  Result := LParent.Extract(LIndex);

  if LHost <> nil then
  begin
    try
      LParent.Insert(LIndex, LHost.Node);
    except
      Result.Free;
      raise;
    end;
  end;
end;

function ReferencesHierarchy(ARoot: TNyxNode): Boolean;
var
  LIndex: Integer;
begin
  Result := ARoot.HasCollectionView and
    (ARoot.CollectionView.Key.Name = NyxStudioHierarchyCollection.Name);

  if Result then
  begin
    Exit;
  end;
  for LIndex := 0 to ARoot.Count - 1 do
  begin

    if ReferencesHierarchy(ARoot.Children[LIndex]) then
    begin
      Exit(True);
    end;
  end;
end;

constructor TNyxStudioSectionDocuments.Create(AShell: TNyxDocument;
  AContext: TNyxStudioSectionContext);
var
  LSection: TNyxStudioSection;
  LRemoved: TNyxNode;
  LRoot: TNyxNode;
  LDocument: TNyxDocument;
begin
  inherited Create;
  RequireContext(Ord(AContext));

  if (AShell = nil) or (AShell.Count <> 1) or
    (AShell.Pages[0].ID <> NyxStudioSectionRootID(nssChrome)) then
  begin
    raise ENyxModel.Create('Studio sections require one shared shell root');
  end;
  FDocuments[nssChrome] := AShell.Clone;
  for LSection := nssProject to High(TNyxStudioSection) do
  begin
    LRemoved := ExtractSection(FDocuments[nssChrome], LSection, True);
    LRemoved.Free;

    if AShell.Find(NyxStudioSectionRootID(LSection)) = nil then
    begin
      Continue;
    end;
    LDocument := AShell.Clone;
    FDocuments[LSection] := LDocument;
    LRoot := ExtractSection(LDocument, LSection, False);
    try
      LDocument.RemoveRoot(NyxPageRoot(NyxStudioSectionRootID(nssChrome)));
      LRoot.Configure.Clear(atWidth).WidthSizing(nsFill).HeightSizing(nsFill)
        .ForPlatform(npfNativeLCL).Clear(atWidth).WidthSizing(nsFill)
        .HeightSizing(nsFill).Done;
      LDocument.AddPage(LRoot);
      LRoot := nil;
    finally
      LRoot.Free;
    end;
  end;
  for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
  begin
    LDocument := FDocuments[LSection];

    if LDocument = nil then
    begin
      Continue;
    end;
    { This known editor-only default changes with design structure. Retain it
      only in roots which bind it; preserve every other creator context. }

    if (AContext = nscEditorOwnedHierarchy) and
      not ReferencesHierarchy(LDocument.Pages[0]) and
      LDocument.Collections.Has(NyxStudioHierarchyCollection) then
    begin
      LDocument.Collections.Remove(NyxStudioHierarchyCollection);
    end;
    LDocument.Validate;
  end;
end;

destructor TNyxStudioSectionDocuments.Destroy;
var
  LSection: TNyxStudioSection;
begin
  for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
  begin
    FDocuments[LSection].Free;
  end;
  inherited Destroy;
end;

function TNyxStudioSectionDocuments.GetDocument(ASection: TNyxStudioSection): TNyxDocument;
begin
  RequireSection(Ord(ASection));
  Result := FDocuments[ASection];
end;

procedure TNyxStudioSectionRoots.SetRoot(ASection: TNyxStudioSection; ARoot: TNyxNode);
begin
  RequireSection(Ord(ASection));
  FRoots[ASection] := ARoot;
end;

function TNyxStudioSectionRoots.GetRoot(ASection: TNyxStudioSection): TNyxNode;
begin
  RequireSection(Ord(ASection));
  Result := FRoots[ASection];
end;

function TNyxStudioSectionRoots.GetID: TNyxText;
begin
  Result := '';

  if FRoots[nssChrome] <> nil then
  begin
    Result := FRoots[nssChrome].ID;
  end;
end;

function TNyxStudioSectionRoots.SectionFor(const AID: TNyxText;
  out ASection: TNyxStudioSection): Boolean;
var
  LSection: TNyxStudioSection;
begin
  Result := False;
  ASection := nssChrome;
  for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
  begin

    if (FRoots[LSection] <> nil) and (FRoots[LSection].Find(AID) <> nil) then
    begin

      if Result then
      begin
        raise ENyxModel.Create('Ambiguous mounted Studio control identity');
      end;
      ASection := LSection;
      Result := True;
    end;
  end;
end;

function TNyxStudioSectionRoots.RootFor(const AID: TNyxText): TNyxNode;
var
  LSection: TNyxStudioSection;
begin
  Result := nil;

  if SectionFor(AID, LSection) then
  begin
    Result := FRoots[LSection];
  end;
end;

function TNyxStudioSectionRoots.Find(const AID: TNyxText): TNyxNode;
var
  LRoot: TNyxNode;
begin
  Result := nil;
  LRoot := RootFor(AID);

  if LRoot <> nil then
  begin
    Result := LRoot.Find(AID);
  end;
end;

end.
