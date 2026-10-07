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

uses nyx.text, nyx.model, nyx.menu, nyx.events, nyx.studio.session;

const
  NyxStudioActionMenuID: TNyxText = 'action-actions';
  NyxStudioActionMenuRoot: TNyxText = 'studio-component-actions';

type
  TNyxStudioMenuAction = (smaUndo, smaRedo, smaProperties, smaEvents, smaHelp);
  TNyxStudioMenuHandler = procedure(AAction: TNyxStudioMenuAction) of object;

{ Independently owned public Nyx recipe, plus a copied typed item plan. Selection
  and history remain owned by the ordinary Studio session; building changes none.
  The caller frees the document after the public menu has copied its content. }
function BuildNyxStudioActionMenu(ASession: TNyxStudioSession;
  out AItems: TNyxMenuItems): TNyxDocument;
function NyxStudioMenuActionTarget(AAction: TNyxStudioMenuAction): TNyxText;
{ UI-thread controller callback is borrowed. Studio keeps this stream sequential
  and retires its menu before controller teardown or project/chrome replacement. }
function NewNyxStudioMenuCallback(AHandler: TNyxStudioMenuHandler): INyxEventCallback;

implementation

uses SysUtils, nyx.types, nyx.controls, nyx.behavior, nyx.scheduler, nyx.root.types,
  nyx.studio.inspector, nyx.studio.help;

const
  CCommands: array[TNyxStudioMenuAction] of TNyxText =
    ('undo', 'redo', 'properties', 'events', 'help');
  CLabels: array[TNyxStudioMenuAction] of TNyxText =
    ('Undo', 'Redo', 'Properties', 'Events', 'About this component');
  CTargets: array[TNyxStudioMenuAction] of TNyxText =
    ('action-undo', 'action-redo', NyxInspectorPropertiesID,
      NyxInspectorEventsID, NyxStudioComponentHelpID);

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
  out AItems: TNyxMenuItems): TNyxDocument;
var
  LRoot: INyxColumn;
  LInspector: INyxColumn;
  LInspectorItems: TNyxMenuItems;
  LButton: INyxButton;
  LSeparator: INyxSeparator;
  LItem: TNyxMenuItem;
  LAction: TNyxStudioMenuAction;
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
  for LAction := Low(TNyxStudioMenuAction) to High(TNyxStudioMenuAction) do
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
  except
    Result.Free;
    raise;
  end;
end;

end.
