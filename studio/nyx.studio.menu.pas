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

unit nyx.studio.menu;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.model, nyx.menu, nyx.events, nyx.studio.session;

const
  NyxStudioActionMenuID: TNyxText = 'action-actions';
  NyxStudioActionMenuRoot: TNyxText = 'studio-component-actions';

type
  { Copied admission inputs for the stock menu recipe. A matching physical
    anchor alone does not establish the same project or command availability.
    This value retains only opaque context, typed identities and flags, never
    the session, a mutable node, a renderer or an event registration. Tooling
    capability does not change these navigation commands or admit operations. }
  TNyxStudioActionMenuState = record
  private
    FContext: TNyxStudioCommandContext;
    FSelected: TNyxControlRef;
    FView: TNyxControlRef;
    FCanUndo: Boolean;
    FCanRedo: Boolean;
    FHasSelection: Boolean;
  public
    { Capture requires a live borrowed session; subsequent ownership is scalar. }
    class function Capture(ASession: TNyxStudioSession): TNyxStudioActionMenuState; static;
    { False requires a fresh independent menu, including an identical-ID load.
      The target must separately establish that the actual anchor is retained. }
    function Matches(ASession: TNyxStudioSession): Boolean;
  end;

  { Closed semantic editor commands. Targets are UI identities, never strings
    that change application behavior. Hidden compact faces share these routes. }
  TNyxStudioMenuAction = (smaUndo, smaRedo, smaProperties, smaEvents, smaHelp,
    smaCode, smaDesktop, smaPhone, smaInteract, smaCanvasTools, smaCanvasExpand, smaPalette,
    smaOpen, smaSave, smaOutputs, smaAgents, smaBuilds, smaBuildView, smaBuildApp);
  TNyxStudioMenuHandler = procedure(AAction: TNyxStudioMenuAction) of object;

{ Independently owned public Nyx recipe, plus a copied typed item plan. Selection
  and history remain owned by the ordinary Studio session; building changes none.
  The caller frees the document after the public menu has copied its content.
  Workspace navigation includes Build jobs before connection; the ordinary panel
  explains availability and the bridge independently admits service operations. }
function BuildNyxStudioActionMenu(ASession: TNyxStudioSession;
  out AItems: TNyxMenuItems; AWorkspaceActions: Boolean = False): TNyxDocument;
function NyxStudioMenuActionTarget(AAction: TNyxStudioMenuAction): TNyxText;
{ UI-thread controller callback is borrowed. Studio keeps this stream sequential
  and retires its menu before controller teardown or project/chrome replacement. }
function NewNyxStudioMenuCallback(AHandler: TNyxStudioMenuHandler): INyxEventCallback;

implementation

uses SysUtils, nyx.controls, nyx.behavior, nyx.scheduler, nyx.root.types,
  nyx.studio.inspector, nyx.studio.help;

class function TNyxStudioActionMenuState.Capture(ASession: TNyxStudioSession):
  TNyxStudioActionMenuState;
begin

  if ASession = nil then
  begin
    raise ENyxModel.Create('Studio menu state requires a live authoring session');
  end;
  Result := Default(TNyxStudioActionMenuState);
  Result.FContext := ASession.CommandContext;

  if ASession.SelectedID <> '' then
  begin
    Result.FSelected := NyxControl(ASession.SelectedID);
  end;

  if ASession.ActiveViewID <> '' then
  begin
    Result.FView := NyxControl(ASession.ActiveViewID);
  end;
  Result.FCanUndo := ASession.CanUndo;
  Result.FCanRedo := ASession.CanRedo;
  Result.FHasSelection := ASession.Selected <> nil;
end;

function TNyxStudioActionMenuState.Matches(ASession: TNyxStudioSession): Boolean;
begin
  Result := (ASession <> nil) and ASession.MatchesCommandContext(FContext) and
    (FSelected.ID = ASession.SelectedID) and (FView.ID = ASession.ActiveViewID) and
    (FCanUndo = ASession.CanUndo) and (FCanRedo = ASession.CanRedo) and
    (FHasSelection = (ASession.Selected <> nil));
end;

const
  CCommands: array[TNyxStudioMenuAction] of TNyxText =
    ('undo', 'redo', 'properties', 'events', 'help', 'code', 'desktop', 'phone',
      'interact', 'canvas-tools', 'canvas-expand', 'palette', 'open', 'save', 'outputs',
      'agents', 'builds', 'build-view', 'build-app');
  CLabels: array[TNyxStudioMenuAction] of TNyxText =
    ('Undo', 'Redo', 'Properties', 'Events', 'About this component', 'Pascal source',
      'Desktop preview', 'Phone preview', 'Interact with design', 'Canvas tools',
      'Expand or restore canvas', 'Project and components', 'Open project', 'Save project', 'Outputs',
      'Agents and sync', 'Build jobs', 'Build view', 'Build application');
  CTargets: array[TNyxStudioMenuAction] of TNyxText =
    ('action-undo', 'action-redo', NyxInspectorPropertiesID,
      NyxInspectorEventsID, NyxStudioComponentHelpID, 'action-code',
      'action-desktop', 'action-phone', 'action-preview', 'action-canvas-tools',
      'action-canvas-expand', 'action-panel-project', 'action-import', 'action-save', 'action-outputs',
      'action-agents', 'action-builds', 'action-build-view', 'action-build-app');

type
  TStudioMenuCallback = class(TNyxEventCallback)
  private
    FHandler: TNyxStudioMenuHandler;
  public
    constructor Create(AHandler: TNyxStudioMenuHandler);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

constructor TStudioMenuCallback.Create(AHandler: TNyxStudioMenuHandler);
begin
  inherited Create;

  if not Assigned(AHandler) then
  begin
    raise ENyxModel.Create('Studio action menu requires its live UI controller');
  end;
  FHandler := AHandler;
end;

procedure TStudioMenuCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LInvocation: TNyxMenuInvocation;
  LAction: TNyxStudioMenuAction;
begin
  LInvocation := NyxMenuInvocation(AEvent);
  for LAction := Low(TNyxStudioMenuAction) to High(TNyxStudioMenuAction) do
  begin

    if LInvocation.Command.Name = CCommands[LAction] then
    begin
      FHandler(LAction);
      Exit;
    end;
  end;
  raise ENyxModel.Create('Unknown Studio menu command');
end;

function NewNyxStudioMenuCallback(AHandler: TNyxStudioMenuHandler): INyxEventCallback;
begin
  Result := TStudioMenuCallback.Create(AHandler);
end;

function NyxStudioMenuActionTarget(AAction: TNyxStudioMenuAction): TNyxText;
begin

  if (Ord(AAction) < Ord(Low(TNyxStudioMenuAction))) or
    (Ord(AAction) > Ord(High(TNyxStudioMenuAction))) then
  begin
    raise ENyxModel.Create('Unknown Studio menu action');
  end;
  Result := CTargets[AAction];
end;

function BuildNyxStudioActionMenu(ASession: TNyxStudioSession;
  out AItems: TNyxMenuItems; AWorkspaceActions: Boolean): TNyxDocument;
var
  LRoot: INyxColumn;
  LInspector: INyxColumn;
  LInspectorItems: TNyxMenuItems;
  LButton: INyxButton;
  LSeparator: INyxSeparator;
  LItem: TNyxMenuItem;
  LAction: TNyxStudioMenuAction;

  { Each branch owns independent ordinary Nyx content. Recipes retain immutable
    copies; no menu or item holds the Studio session/controller alive. }
  procedure AddBranch(const AName, ACaption: TNyxText;
    AFirst, ALast: TNyxStudioMenuAction);
  var
    LContent: INyxColumn;
    LPlan: TNyxMenuItems;
    LChoice: TNyxStudioMenuAction;
    LFace: INyxButton;
  begin
    LContent := NewNyxColumn('studio-menu-branch-' + AName);
    LContent.Configure.Padding(8).Gap(4).Align(ncaStretch).Compound(True);
    LPlan := NyxMenuItems;
    { Build jobs is stable workspace navigation. The ordinary panel explains
      missing service capability; the bridge still admits every operation.
      A discovery reply must not replace an unrelated open View submenu. }
    for LChoice := AFirst to ALast do
    begin
      LFace := NewNyxButton('studio-menu-' + CCommands[LChoice]);
      LFace.Configure.Text(CLabels[LChoice]).PartName(NyxPart(CCommands[LChoice]));
      LContent.Add(LFace);
      LPlan := LPlan.Add(NyxMenuAction(NyxPart(CCommands[LChoice]),
        NyxMenuCommand(CCommands[LChoice])));
    end;
    Result.AddPage(LContent);
    LFace := NewNyxButton('studio-menu-' + AName);
    LFace.Configure.Text(ACaption).PartName(NyxPart(AName));
    LRoot.Add(LFace);
    AItems := AItems.Add(NyxMenuSubmenu(NyxPart(AName), NewNyxMenuRecipe(Result,
      NyxPageRoot('studio-menu-branch-' + AName), LPlan)));
  end;
begin

  if ASession = nil then
  begin
    raise ENyxModel.Create('Studio action menu requires an authoring session');
  end;
  AItems := NyxMenuItems;
  LRoot := NewNyxColumn(NyxStudioActionMenuRoot);
  LRoot.Configure.Padding(8).Gap(4).Align(ncaStretch).Compound(True);
  LInspector := NewNyxColumn('studio-component-inspector');
  LInspector.Configure.Padding(8).Gap(4).Align(ncaStretch).Compound(True);
  LInspectorItems := NyxMenuItems;
  for LAction := smaUndo to smaHelp do
  begin

    if LAction = smaProperties then
    begin
      LSeparator := NewNyxSeparator('studio-menu-separator');
      LSeparator.Configure.PartName(NyxPart('separator')).Height(1);
      LRoot.Add(LSeparator);
      AItems := AItems.Add(NyxMenuSeparator(NyxPart('separator')));
    end;
    LButton := NewNyxButton('studio-menu-' + CCommands[LAction]);
    LButton.Text := CLabels[LAction];
    LButton.Configure.PartName(NyxPart(CCommands[LAction])).Variant(nvSecondary);

    if LAction < smaProperties then
    begin
      LRoot.Add(LButton);
    end
    else
    begin
      LInspector.Add(LButton);
    end;
    LItem := NyxMenuAction(NyxPart(CCommands[LAction]), NyxMenuCommand(CCommands[LAction]));
    case LAction of
      smaUndo:
        begin
          LItem := LItem.Enabled(ASession.CanUndo);
        end;
      smaRedo:
        begin
          LItem := LItem.Enabled(ASession.CanRedo);
        end;
    else
      begin
        LItem := LItem.Enabled(ASession.Selected <> nil);
      end;
    end;

    if LAction < smaProperties then
    begin
      AItems := AItems.Add(LItem);
    end
    else
    begin
      LInspectorItems := LInspectorItems.Add(LItem);
    end;
  end;
  LButton := NewNyxButton('studio-menu-inspect');
  LButton.Text := 'Inspect';
  LButton.Configure.PartName(NyxPart('inspect')).Variant(nvSecondary);
  LRoot.Add(LButton);
  Result := TNyxDocument.Create;
  try
    Result.AddPage(LRoot);
    Result.AddPage(LInspector);
    AItems := AItems.Add(NyxMenuSubmenu(NyxPart('inspect'), NewNyxMenuRecipe(Result,
      NyxPageRoot('studio-component-inspector'), LInspectorItems))
      .Enabled(ASession.Selected <> nil));

    if AWorkspaceActions then
    begin
      AddBranch('view', 'View', smaCode, smaPalette);
      AddBranch('project', 'Project', smaOpen, smaAgents);
      AddBranch('build', 'Build', smaBuilds, smaBuildApp);
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
