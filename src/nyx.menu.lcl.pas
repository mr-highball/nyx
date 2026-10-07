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

unit nyx.menu.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Controls, nyx.text, nyx.model, nyx.root.types, nyx.theme, nyx.menu,
  nyx.popover.lcl, nyx.render.lcl, nyx.menu.button;

type
  INyxLCLMenu = interface(INyxMenu)
    ['{1A8B0719-0647-444E-A241-061026000003}']
    function GetPopover: INyxLCLPopover;
    property Presentation: INyxLCLPopover read GetPopover;
  end;

{ Anchor/theme are borrowed target seams; content and runtime item state are
  independent. Physical standard LCL buttons remain enabled for menu focus. }
function NewNyxLCLMenu(AAnchor: TWinControl; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; const AItems: TNyxMenuItems;
  ATheme: TNyxTheme = nil): INyxLCLMenu;
{ Owns an independent binding for each realized invoker. Release the returned
  owner before renderer remount/retirement; document defaults stay unchanged. }
function BindNyxLCLMenus(ADocument: TNyxDocument;
  ARenderer: TNyxLCLRenderer): INyxMenuBindings;

implementation

uses SysUtils, Classes, Forms, Graphics, LCLType, nyx.types, nyx.behavior,
  nyx.events, nyx.scheduler, nyx.widgets.lcl, nyx.controls,
  nyx.menu.bar, nyx.menu.bar.lcl, nyx.menu.bar.declarations;

type
  TMenuControlAccess = class(TWinControl);
  TLCLMenu = class(TNyxMenuPresenter, INyxLCLMenu)
  private
    FHost: INyxLCLPopover;
    FStyled: Boolean;
    FPreviousText: array of TUTF8KeyPressEvent;
    FColors: array of TColor;
    FTheme: TNyxTheme;
    procedure TextKey(ASender: TObject; var AKey: TUTF8Char);
  protected
    procedure ApplyFaces; override;
    function FocusFace(AIndex: Integer): Boolean; override;
    procedure TabExit(AReverse: Boolean; const AExecution: INyxExecution); override;
    function CreateSubmenu(AIndex: Integer; const ARecipe: INyxMenuRecipe):
      TNyxMenuPresenter; override;
  public
    constructor Create(AAnchor: TWinControl; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef; const AItems: TNyxMenuItems; ATheme: TNyxTheme;
      const AParent: INyxLCLPopover = nil);
    destructor Destroy; override;
    function GetPopover: INyxLCLPopover;
  end;

constructor TLCLMenu.Create(AAnchor: TWinControl; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; const AItems: TNyxMenuItems; ATheme: TNyxTheme;
  const AParent: INyxLCLPopover);
begin
  FTheme := ATheme;
  FHost := NewNyxLCLPopover(AAnchor, ADocument, ARoot, ATheme, AParent);
  inherited Create(FHost, AItems);
  SetLength(FPreviousText, Plan.Count);
  SetLength(FColors, Plan.Count);
end;

destructor TLCLMenu.Destroy;
var
  LIndex: Integer;
  LFace: TWinControl;
begin

  if FStyled then
  begin
    for LIndex := 0 to Plan.Count - 1 do
    begin

      if Plan[LIndex].Kind <> nmiSeparator then
      begin
        LFace := FHost.Renderer.FocusFor(ItemID(LIndex), niDesign);
        TMenuControlAccess(LFace).OnUTF8KeyPress := FPreviousText[LIndex];
        TMenuControlAccess(LFace).Font.Color := FColors[LIndex];

        if LFace is TNyxLCLButton then
        begin
          TNyxLCLButton(LFace).Presentation(nbpControl);
        end;
      end;
    end;
  end;
  inherited Destroy;
  FHost := nil;
end;

function TLCLMenu.GetPopover: INyxLCLPopover;
begin
  Result := FHost;
end;

procedure TLCLMenu.ApplyFaces;
var
  LIndex: Integer;
  LFace: TWinControl;
  LCaption: TNyxText;
begin
  for LIndex := 0 to Plan.Count - 1 do
  begin

    if Plan[LIndex].Kind = nmiSeparator then
    begin
      Continue;
    end;
    LFace := FHost.Renderer.FocusFor(ItemID(LIndex), niDesign);

    if not FStyled then
    begin
      FPreviousText[LIndex] := TMenuControlAccess(LFace).OnUTF8KeyPress;
      FColors[LIndex] := TMenuControlAccess(LFace).Font.Color;
      TMenuControlAccess(LFace).OnUTF8KeyPress := TextKey;
    end;
    LFace.TabStop := False;

    if LFace is TNyxLCLButton then
    begin
      TNyxLCLButton(LFace).Presentation(nbpMenuItem, Plan[LIndex].IsEnabled);
    end;
    LCaption := Button(Plan[LIndex].Part).Text;

    if Plan[LIndex].Kind = nmiSubmenu then
    begin
      LCaption := LCaption + TNyxText('  ›');
    end;

    if Plan[LIndex].Kind in [nmiCheck, nmiRadio] then
    begin

      if Plan[LIndex].IsChecked then
      begin
        LCaption := TNyxText('✓  ') + LCaption;
      end
      else
      begin
        LCaption := TNyxText('    ') + LCaption;
      end;
    end;
    { LCL's captions carry UTF-8 bytes. An explicit String cast preserves the
      portable text bytes instead of invoking an ANSI codepage conversion. }
    TMenuControlAccess(LFace).Caption := String(LCaption);

    if Plan[LIndex].IsEnabled then
    begin
      TMenuControlAccess(LFace).Font.Color := FColors[LIndex];
    end
    else
    begin
      TMenuControlAccess(LFace).Font.Color := clGrayText;
    end;
  end;
  FStyled := True;
end;

function TLCLMenu.FocusFace(AIndex: Integer): Boolean;
var
  LFace: TWinControl;
begin
  LFace := FHost.Renderer.FocusFor(ItemID(AIndex), niDesign);
  Result := LFace.CanSetFocus;

  if Result then
  begin
    LFace.SetFocus;
    Result := Screen.ActiveControl = LFace;
  end;
end;

procedure TLCLMenu.TabExit(AReverse: Boolean; const AExecution: INyxExecution);
var
  LInvoker: TWinControl;
begin
  { Native dispatch originated in the now-hidden form. Explicitly traverse the
    restored invoker's real container so that form's dialog manager cannot move
    focus back into hidden menu controls. }
  NyxEventResponse(AExecution).Consume;
  LInvoker := Screen.ActiveControl;

  if (LInvoker <> nil) and (LInvoker.Parent <> nil) then
  begin
    TMenuControlAccess(LInvoker.Parent).SelectNext(LInvoker, not AReverse, True);
  end;
end;

procedure TLCLMenu.TextKey(ASender: TObject; var AKey: TUTF8Char);
var
  LIndex: Integer;
  LKeepAlive: INyxMenu;
begin
  LKeepAlive := Self;
  for LIndex := 0 to Plan.Count - 1 do
  begin

    if (Plan[LIndex].Kind <> nmiSeparator) and
      (FHost.Renderer.FocusFor(ItemID(LIndex), niDesign) = ASender) then
    begin

      if Assigned(FPreviousText[LIndex]) then
      begin
        FPreviousText[LIndex](ASender, AKey);
      end;

      if not (GetKeyShiftState * [ssCtrl, ssAlt, ssMeta] <> []) and
        TextInput(TNyxText(AKey), GetTickCount64) then
      begin
        AKey := '';
      end;
      Break;
    end;
  end;
  LKeepAlive.GetOpen;
end;

function TLCLMenu.CreateSubmenu(AIndex: Integer;
  const ARecipe: INyxMenuRecipe): TNyxMenuPresenter;
var
  LDocument: TNyxDocument;
begin
  LDocument := ARecipe.CopyDocument;
  try
    Result := TLCLMenu.Create(FHost.Renderer.FocusFor(ItemID(AIndex), niDesign),
      LDocument, ARecipe.Root, ARecipe.Items, FTheme, FHost);
  finally
    LDocument.Free;
  end;
end;

function NewNyxLCLMenu(AAnchor: TWinControl; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; const AItems: TNyxMenuItems;
  ATheme: TNyxTheme): INyxLCLMenu;
begin
  Result := TLCLMenu.Create(AAnchor, ADocument, ARoot, AItems, ATheme);
end;

function BindNyxLCLMenus(ADocument: TNyxDocument;
  ARenderer: TNyxLCLRenderer): INyxMenuBindings;
var
  LBindings: INyxMenuBindings;
  LBarBindings: INyxMenuBarBindings;
  LGroupedIDs: array of TNyxText;

  procedure Visit(ANode: TNyxNode; ABars: Boolean);
  var
    LIndex: Integer;
    LPosition: Integer;
    LRecipe: INyxMenuRecipe;
    LMenu: INyxLCLMenu;
    LRow: INyxRow;
    LBar: INyxMenuBar;
    LHeading: INyxMenuBarHeading;
    LButton: INyxButton;
  begin

    if ABars and ANode.HasMenuBar and (ANode.MenuBar <> nil) then
    begin

      if not Supports(RetainNyxControl(ANode), INyxRow, LRow) then
      begin
        raise ENyxModel.Create('Declared menu bar must project to a specialized row');
      end;
      LBar := NewNyxLCLMenuBar(LRow, ARenderer, ANode.MenuBar.Options);
      for LIndex := 0 to ANode.MenuBar.Count - 1 do
      begin
        LHeading := ANode.MenuBar.Item(LIndex);

        if not Supports(LRow.Part(LHeading.Part), INyxButton, LButton) then
        begin
          raise ENyxModel.Create('Declared menu bar heading must be a specialized button');
        end;

        if LButton.Node.HasMenu and (LButton.Node.MenuReference.Name <> '') then
        begin
          raise ENyxModel.Create('A grouped heading cannot also attach a standalone menu');
        end;
        LRecipe := NewNyxDeclaredMenuRecipe(ADocument, LHeading.Menu);
        LMenu := NewNyxLCLMenu(ARenderer.FocusFor(LButton.ID, niRuntime),
          ADocument, LRecipe.Root, LRecipe.Items);
        LBar.Add(LHeading.Part, LMenu,
          ADocument.Menus.Definition(LHeading.Menu).Options);
        LBar.SetEnabled(LHeading.Part, LHeading.IsEnabled);
        LPosition := Length(LGroupedIDs);
        SetLength(LGroupedIDs, LPosition + 1);
        LGroupedIDs[LPosition] := LButton.ID;
      end;
      LBarBindings.AddBar(LBar, ARenderer.Events);
    end;

    if not ABars and ANode.HasMenu and (ANode.MenuReference.Name <> '') then
    begin
      for LIndex := 0 to High(LGroupedIDs) do
      begin

        if LGroupedIDs[LIndex] = ANode.ID then
        begin
          raise ENyxModel.Create('A grouped heading cannot also attach a standalone menu');
        end;
      end;
      LRecipe := NewNyxDeclaredMenuRecipe(ADocument, ANode.MenuReference);
      LMenu := NewNyxLCLMenu(ARenderer.FocusFor(ANode.ID, niRuntime),
        ADocument, LRecipe.Root, LRecipe.Items);
      LBindings.Add(RetainNyxControl(ANode) as INyxButton, ARenderer.Events, LMenu,
        ADocument.Menus.Definition(ANode.MenuReference).Options);
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LIndex], ABars);
    end;
  end;

begin

  if (ADocument = nil) or (ARenderer = nil) or (ARenderer.Root = nil) then
  begin
    raise ENyxModel.Create('Declared lcl menus require a mounted view');
  end;
  LBindings := NewNyxMenuBindings;
  Supports(LBindings, INyxMenuBarBindings, LBarBindings);
  { Coordinate groups before standalone invokers. The candidate owner retires
    every completed family if any later admission refuses. No descriptor changes. }
  Visit(ARenderer.Root, True);
  Visit(ARenderer.Root, False);
  Result := LBindings;
end;

end.
