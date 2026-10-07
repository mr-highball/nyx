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

unit nyx.menu.bar.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.controls, nyx.menu.bar, nyx.render.lcl;

{ Content is the exact mounted specialized row. The renderer and target controls
  remain borrowed. Release the returned bar before remounting/freeing the view. }
function NewNyxLCLMenuBar(const AContent: INyxRow; ARenderer: TNyxLCLRenderer;
  const AOptions: TNyxMenuBarOptions): INyxMenuBar;

implementation

uses
  SysUtils, Classes, Controls, Forms, LCLType, nyx.text, nyx.types,
  nyx.model, nyx.behavior, nyx.events, nyx.scheduler, nyx.widgets.lcl, nyx.errors,
  nyx.menu, nyx.menu.lcl, nyx.popover.lcl;

type
  TControlAccess = class(TWinControl);
  TFaceSnapshot = record
    TabStop: Boolean;
    Text: TUTF8KeyPressEvent;
    Role: TLazAccessibilityRole;
  end;
  TLCLMenuBar = class(TNyxMenuBarPresenter)
  private
    FRenderer: TNyxLCLRenderer;
    FRoot: TControl;
    FRole: TLazAccessibilityRole;
    FLabel: TNyxText;
    FDecorated: Boolean;
    FFaces: array of TFaceSnapshot;
    function Face(AIndex: Integer): TWinControl;
    procedure TextKey(ASender: TObject; var AKey: TUTF8Char);
  protected
    procedure ValidateFamily(const AButton: INyxButton;
      const AMenu: INyxMenu); override;
    procedure PrepareFace(AIndex: Integer); override;
    procedure RestoreFace(AIndex: Integer); override;
    procedure ApplyFaces; override;
    function FocusFace(AIndex: Integer): Boolean; override;
    procedure TabExit(AIndex: Integer; AReverse: Boolean;
      const AExecution: INyxExecution); override;
  public
    constructor Create(const AContent: INyxRow; ARenderer: TNyxLCLRenderer;
      const AOptions: TNyxMenuBarOptions);
    destructor Destroy; override;
  end;

procedure TLCLMenuBar.ValidateFamily(const AButton: INyxButton;
  const AMenu: INyxMenu);
var
  LMenu: INyxLCLMenu;
  LAnchor: INyxLCLPopoverAnchor;
begin

  if not Supports(AMenu, INyxLCLMenu, LMenu) or
    not Supports(LMenu.Presentation, INyxLCLPopoverAnchor, LAnchor) or
    not LAnchor.AnchoredTo(FRenderer.FocusFor(AButton.ID)) then
  begin
    raise ENyxModel.Create('Menu bar family must use the exact heading anchor');
  end;
end;

constructor TLCLMenuBar.Create(const AContent: INyxRow; ARenderer: TNyxLCLRenderer;
  const AOptions: TNyxMenuBarOptions);
begin

  if (AContent = nil) or (ARenderer = nil) or (ARenderer.Root = nil) or
    (ARenderer.Root.Find(AContent.ID) <> AContent.Node) then
  begin
    raise ENyxModel.Create('Native menu bar requires its exact mounted row');
  end;
  FRenderer := ARenderer;
  inherited Create(AContent, ARenderer.Events, AOptions);
  FRoot := FRenderer.ControlFor(AContent.ID);
  FRole := FRoot.AccessibleRole;
  FLabel := TNyxText(FRoot.AccessibleName);
  FDecorated := True;
  ApplyFaces;
end;

destructor TLCLMenuBar.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Count - 1 do
  begin
    RestoreFace(LIndex);
  end;

  if FDecorated then
  begin
    FRoot.AccessibleRole := FRole;
    FRoot.AccessibleName := String(FLabel);
  end;
  inherited Destroy;
end;

function TLCLMenuBar.Face(AIndex: Integer): TWinControl;
begin
  Result := FRenderer.FocusFor(ButtonAt(AIndex).ID);
end;

procedure TLCLMenuBar.PrepareFace(AIndex: Integer);
var
  LFace: TWinControl;
begin
  SetLength(FFaces, AIndex + 1);
  LFace := Face(AIndex);
  FFaces[AIndex].TabStop := LFace.TabStop;
  FFaces[AIndex].Text := TControlAccess(LFace).OnUTF8KeyPress;
  FFaces[AIndex].Role := LFace.AccessibleRole;
  TControlAccess(LFace).OnUTF8KeyPress := TextKey;
end;

procedure TLCLMenuBar.RestoreFace(AIndex: Integer);
var
  LFace: TWinControl;
begin
  LFace := Face(AIndex);
  LFace.TabStop := FFaces[AIndex].TabStop;
  TControlAccess(LFace).OnUTF8KeyPress := FFaces[AIndex].Text;
  LFace.AccessibleRole := FFaces[AIndex].Role;

  if LFace is TNyxLCLButton then
  begin
    TNyxLCLButton(LFace).Presentation(nbpControl);
  end;
end;

procedure TLCLMenuBar.ApplyFaces;
var
  LIndex: Integer;
  LTab: Integer;
  LFace: TWinControl;
begin
  FRoot.AccessibleRole := larMenuBar;
  FRoot.AccessibleName := String(Options.Caption);
  LTab := TabIndex;
  for LIndex := 0 to Count - 1 do
  begin
    LFace := Face(LIndex);
    LFace.AccessibleRole := larMenuItem;
    LFace.TabStop := LIndex = LTab;

    if LFace is TNyxLCLButton then
    begin
      { Logical disabled state preserves keyboard focus. The ordinary custom
        button paints the unavailable command without physically disabling it. }
      TNyxLCLButton(LFace).Presentation(nbpMenuItem, Enabled(LIndex));
    end;
  end;
end;

function TLCLMenuBar.FocusFace(AIndex: Integer): Boolean;
begin
  Result := Face(AIndex).CanSetFocus;

  if Result then
  begin
    Face(AIndex).SetFocus;
    Result := Screen.ActiveControl = Face(AIndex);
  end;
end;

procedure TLCLMenuBar.TabExit(AIndex: Integer; AReverse: Boolean;
  const AExecution: INyxExecution);
var
  LInvoker: TWinControl;
  LContainer: TWinControl;
begin
  { A dropdown's hidden form cannot own dialog traversal. Restore the heading,
    then traverse its containing form with all sibling headings excluded from
    Tab. This is the same scope as the standard mounted Nyx controls. }
  NyxEventResponse(AExecution).Consume;
  LInvoker := Face(AIndex);
  LContainer := GetParentForm(LInvoker);

  if LContainer = nil then
  begin
    LContainer := LInvoker.Parent;
  end;

  if LContainer <> nil then
  begin
    TControlAccess(LContainer).SelectNext(LInvoker, not AReverse, True);
  end;
end;

procedure TLCLMenuBar.TextKey(ASender: TObject; var AKey: TUTF8Char);
var
  LIndex: Integer;
  LOwner: INyxMenuBar;
begin
  LOwner := Self;
  for LIndex := 0 to Count - 1 do
  begin

    if Face(LIndex) = ASender then
    begin

      if Assigned(FFaces[LIndex].Text) then
      begin
        FFaces[LIndex].Text(ASender, AKey);
      end;

      if (GetKeyShiftState * [ssCtrl, ssAlt, ssMeta] = []) and
        TextInput(TNyxText(AKey), GetTickCount64) then
      begin
        AKey := '';
      end;
      Break;
    end;
  end;
  LOwner.GetCount;
end;

function NewNyxLCLMenuBar(const AContent: INyxRow; ARenderer: TNyxLCLRenderer;
  const AOptions: TNyxMenuBarOptions): INyxMenuBar;
begin
  Result := TLCLMenuBar.Create(AContent, ARenderer, AOptions);
end;

end.
