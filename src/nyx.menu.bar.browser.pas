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

unit nyx.menu.bar.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  JS, Web, nyx.text, nyx.controls, nyx.menu.bar, nyx.render.browser;

{ Content must be the exact mounted row from Renderer.Root. Both are borrowed
  target seams: retire the returned owner before remounting/destroying the view.
  Menus are added through the portable fluent contract after factory creation. }
function NewNyxBrowserMenuBar(const AContent: INyxRow;
  ARenderer: TNyxBrowserRenderer; const AOptions: TNyxMenuBarOptions): INyxMenuBar;

implementation

uses SysUtils, nyx.types, nyx.behavior, nyx.scheduler, nyx.model, nyx.errors,
  nyx.menu, nyx.menu.browser, nyx.popover.browser;

type
  TFaceSnapshot = record
    Role: TNyxText;
    Tab: TNyxText;
    Disabled: TNyxText;
    HasRole: Boolean;
    HasTab: Boolean;
    HasDisabled: Boolean;
  end;
  TBrowserMenuBar = class(TNyxMenuBarPresenter)
  private
    FRenderer: TNyxBrowserRenderer;
    FElement: TJSHTMLElement;
    FRole: TNyxText;
    FLabel: TNyxText;
    FOrientation: TNyxText;
    FHasRole: Boolean;
    FHasLabel: Boolean;
    FHasOrientation: Boolean;
    FDecorated: Boolean;
    FFaces: array of TFaceSnapshot;
    function Face(AIndex: Integer): TJSHTMLElement;
    function TextKey(AEvent: TJSKeyboardEvent): Boolean;
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
    constructor Create(const AContent: INyxRow; ARenderer: TNyxBrowserRenderer;
      const AOptions: TNyxMenuBarOptions);
    destructor Destroy; override;
  end;

procedure TBrowserMenuBar.ValidateFamily(const AButton: INyxButton;
  const AMenu: INyxMenu);
var
  LMenu: INyxBrowserMenu;
  LAnchor: INyxBrowserPopoverAnchor;
begin

  if not Supports(AMenu, INyxBrowserMenu, LMenu) or
    not Supports(LMenu.Presentation, INyxBrowserPopoverAnchor, LAnchor) or
    not LAnchor.AnchoredTo(FRenderer.FocusFor(AButton.ID)) then
  begin
    raise ENyxModel.Create('Menu bar family must use the exact heading anchor');
  end;
end;

procedure RestoreAttribute(AElement: TJSHTMLElement; const AName, AValue: TNyxText;
  AExisted: Boolean);
begin

  if AExisted then
  begin
    AElement.setAttribute(AName, AValue);
  end
  else
  begin
    AElement.removeAttribute(AName);
  end;
end;

constructor TBrowserMenuBar.Create(const AContent: INyxRow;
  ARenderer: TNyxBrowserRenderer; const AOptions: TNyxMenuBarOptions);
begin

  if (AContent = nil) or (ARenderer = nil) or (ARenderer.Root = nil) or
    (ARenderer.Root.Find(AContent.ID) <> AContent.Node) then
  begin
    raise ENyxModel.Create('Browser menu bar requires its exact mounted row');
  end;
  FRenderer := ARenderer;
  FElement := FRenderer.ElementFor(AContent.ID);
  inherited Create(AContent, ARenderer.Events, AOptions);
  FHasRole := FElement.hasAttribute('role');
  FHasLabel := FElement.hasAttribute('aria-label');
  FHasOrientation := FElement.hasAttribute('aria-orientation');
  FRole := FElement.getAttribute('role');
  FLabel := FElement.getAttribute('aria-label');
  FOrientation := FElement.getAttribute('aria-orientation');
  FDecorated := True;
  FElement.addEventListener('keydown', @TextKey);
  ApplyFaces;
end;

destructor TBrowserMenuBar.Destroy;
var
  LIndex: Integer;
begin

  if FDecorated then
  begin
    FElement.removeEventListener('keydown', @TextKey);
    for LIndex := 0 to Count - 1 do
    begin
      RestoreFace(LIndex);
    end;
    RestoreAttribute(FElement, 'role', FRole, FHasRole);
    RestoreAttribute(FElement, 'aria-label', FLabel, FHasLabel);
    RestoreAttribute(FElement, 'aria-orientation', FOrientation, FHasOrientation);
  end;
  inherited Destroy;
end;

function TBrowserMenuBar.Face(AIndex: Integer): TJSHTMLElement;
begin
  Result := FRenderer.FocusFor(ButtonAt(AIndex).ID);
end;

procedure TBrowserMenuBar.PrepareFace(AIndex: Integer);
var
  LFace: TJSHTMLElement;
begin
  SetLength(FFaces, AIndex + 1);
  LFace := Face(AIndex);
  FFaces[AIndex].Role := LFace.getAttribute('role');
  FFaces[AIndex].Tab := LFace.getAttribute('tabindex');
  FFaces[AIndex].Disabled := LFace.getAttribute('aria-disabled');
  FFaces[AIndex].HasRole := LFace.hasAttribute('role');
  FFaces[AIndex].HasTab := LFace.hasAttribute('tabindex');
  FFaces[AIndex].HasDisabled := LFace.hasAttribute('aria-disabled');
end;

procedure TBrowserMenuBar.RestoreFace(AIndex: Integer);
var
  LFace: TJSHTMLElement;
begin
  LFace := Face(AIndex);
  RestoreAttribute(LFace, 'role', FFaces[AIndex].Role, FFaces[AIndex].HasRole);
  RestoreAttribute(LFace, 'tabindex', FFaces[AIndex].Tab, FFaces[AIndex].HasTab);
  RestoreAttribute(LFace, 'aria-disabled', FFaces[AIndex].Disabled, FFaces[AIndex].HasDisabled);
end;

procedure TBrowserMenuBar.ApplyFaces;
var
  LIndex: Integer;
  LTab: Integer;
  LFace: TJSHTMLElement;
begin
  FElement.setAttribute('role', 'menubar');
  FElement.setAttribute('aria-label', Options.Caption);
  FElement.setAttribute('aria-orientation', 'horizontal');
  LTab := TabIndex;
  for LIndex := 0 to Count - 1 do
  begin
    LFace := Face(LIndex);
    LFace.setAttribute('role', 'menuitem');
    LFace.setAttribute('tabindex', '-1');

    if LIndex = LTab then
    begin
      LFace.setAttribute('tabindex', '0');
    end;
    LFace.setAttribute('aria-disabled', 'false');

    if not Enabled(LIndex) then
    begin
      LFace.setAttribute('aria-disabled', 'true');
    end;
  end;
end;

function TBrowserMenuBar.FocusFace(AIndex: Integer): Boolean;
begin
  NyxFocusWithoutScroll(Face(AIndex));
  Result := document.activeElement = Face(AIndex);
end;

procedure TBrowserMenuBar.TabExit(AIndex: Integer; AReverse: Boolean;
  const AExecution: INyxExecution);
begin
  { Close restores the current heading. One Tab stop leaves traversal to the
    browser's real document order in either direction, without synthetic focus. }
end;

function TBrowserMenuBar.TextKey(AEvent: TJSKeyboardEvent): Boolean;
var
  LOwner: INyxMenuBar;
  LIndex: Integer;
  LOwned: Boolean;
begin
  Result := True;
  LOwned := False;
  for LIndex := 0 to Count - 1 do
  begin

    if Face(LIndex) = AEvent.target then
    begin
      LOwned := True;
      Break;
    end;
  end;

  if not LOwned or AEvent.defaultPrevented or AEvent.isComposing or AEvent._repeat or
    AEvent.ctrlKey or AEvent.altKey or AEvent.metaKey then
  begin
    Exit;
  end;
  LOwner := Self;

  if TextInput(AEvent.key, window.performance.now) then
  begin
    AEvent.preventDefault;
  end;
  LOwner.GetCount;
end;

function NewNyxBrowserMenuBar(const AContent: INyxRow;
  ARenderer: TNyxBrowserRenderer; const AOptions: TNyxMenuBarOptions): INyxMenuBar;
begin
  Result := TBrowserMenuBar.Create(AContent, ARenderer, AOptions);
end;

end.
