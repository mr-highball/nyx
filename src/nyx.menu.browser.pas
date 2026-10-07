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

unit nyx.menu.browser;

{$mode delphi}{$H+}{$codepage utf8}{$modeswitch externalclass}

interface

uses JS, Web, nyx.text, nyx.model, nyx.root.types, nyx.theme, nyx.menu,
  nyx.popover.browser;

type
  { The observed popover is managed and may outlive the menu. Its keyboard
    registrations/listeners retire with the menu; its renderer remains owned. }
  INyxBrowserMenu = interface(INyxMenu)
    ['{1A8B0719-0647-444E-A241-061026000002}']
    function GetPopover: INyxBrowserPopover;
    property Presentation: INyxBrowserPopover read GetPopover;
  end;

function NewNyxBrowserMenu(AAnchor: TJSHTMLElement; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; const AItems: TNyxMenuItems;
  ATheme: TNyxTheme = nil): INyxBrowserMenu;

implementation

uses SysUtils, nyx.types, nyx.behavior, nyx.events, nyx.scheduler, nyx.render.browser;

type
  TBrowserMenu = class(TNyxMenuPresenter, INyxBrowserMenu)
  private
    FHost: INyxBrowserPopover;
    FParent: INyxBrowserPopover;
    FAnchor: TJSHTMLElement;
    FTheme: TNyxTheme;
    FPreviousPopup: TNyxText;
    FPreviousExpanded: TNyxText;
    FPreviousControls: TNyxText;
    FDecorated: Boolean;
    function TextKey(AEvent: TJSKeyboardEvent): Boolean;
  protected
    procedure ApplyFaces; override;
    function FocusFace(AIndex: Integer): Boolean; override;
    procedure TabExit(AReverse: Boolean; const AExecution: INyxExecution); override;
    function CreateSubmenu(AIndex: Integer; const ARecipe: INyxMenuRecipe):
      TNyxMenuPresenter; override;
    procedure PresentationChanged; override;
  public
    constructor Create(AAnchor: TJSHTMLElement; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef; const AItems: TNyxMenuItems; ATheme: TNyxTheme;
      const AParent: INyxBrowserPopover = nil);
    destructor Destroy; override;
    function GetPopover: INyxBrowserPopover;
  end;

var
  GMenuIdentity: Integer;

constructor TBrowserMenu.Create(AAnchor: TJSHTMLElement; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; const AItems: TNyxMenuItems; ATheme: TNyxTheme;
  const AParent: INyxBrowserPopover);
begin
  FAnchor := AAnchor;
  FTheme := ATheme;
  FParent := AParent;
  FHost := NewNyxBrowserPopover(AAnchor, ADocument, ARoot, ATheme);
  inherited Create(FHost, AItems);
  Inc(GMenuIdentity);
  FHost.Element.id := 'nyx-menu-' + IntToStr(GMenuIdentity);
  FPreviousPopup := FAnchor.getAttribute('aria-haspopup');
  FPreviousExpanded := FAnchor.getAttribute('aria-expanded');
  FPreviousControls := FAnchor.getAttribute('aria-controls');
  FAnchor.setAttribute('aria-haspopup', 'menu');
  FAnchor.setAttribute('aria-expanded', 'false');
  FAnchor.setAttribute('aria-controls', FHost.Element.id);
  FDecorated := True;

  if FParent <> nil then
  begin
    { Descendant DOM semantics keep the ancestor's outside-press check correct.
      Browser top-layer painting still places this host beside its parent. }
    FParent.Element.appendChild(FHost.Element);
  end;
  { Ordinary Nyx key cycles run on each button first. The bubble text listener
    respects consumption, modifiers and composition instead of guessing letters
    from physical keyboard codes or bypassing application callbacks. }
  FHost.Element.addEventListener('keydown', @TextKey);
end;

destructor TBrowserMenu.Destroy;
begin

  if FHost <> nil then
  begin
    FHost.Element.removeEventListener('keydown', @TextKey);
  end;
  inherited Destroy;

  if FDecorated and (FAnchor <> nil) then
  begin
    { The invoker is borrowed. Empty previous attributes are removed below. }
    FAnchor.setAttribute('aria-haspopup', FPreviousPopup);
    FAnchor.setAttribute('aria-expanded', FPreviousExpanded);
    FAnchor.setAttribute('aria-controls', FPreviousControls);

    if FPreviousPopup = '' then
    begin
      FAnchor.removeAttribute('aria-haspopup');
    end;

    if FPreviousExpanded = '' then
    begin
      FAnchor.removeAttribute('aria-expanded');
    end;

    if FPreviousControls = '' then
    begin
      FAnchor.removeAttribute('aria-controls');
    end;
  end;
  FHost := nil;
  FParent := nil;
end;

function TBrowserMenu.GetPopover: INyxBrowserPopover;
begin
  Result := FHost;
end;

procedure TBrowserMenu.ApplyFaces;
var
  LIndex: Integer;
  LFace: TJSHTMLElement;
  LRole: TNyxText;
begin
  FHost.Element.setAttribute('role', 'menu');
  for LIndex := 0 to Plan.Count - 1 do
  begin
    LFace := FHost.Renderer.ElementFor(ItemID(LIndex), niDesign);

    if Plan[LIndex].Kind = nmiSeparator then
    begin
      LFace.setAttribute('role', 'separator');
      LFace.setAttribute('aria-orientation', 'horizontal');
      Continue;
    end;
    LFace := FHost.Renderer.FocusFor(ItemID(LIndex), niDesign);
    LRole := 'menuitem';
    case Plan[LIndex].Kind of
      nmiCheck:
        begin
          LRole := 'menuitemcheckbox';
        end;
      nmiRadio:
        begin
          LRole := 'menuitemradio';
        end;
    else
      begin
        { Ordinary action uses the default role. }
      end;
    end;
    LFace.setAttribute('role', LRole);
    LFace.setAttribute('tabindex', '-1');
    LFace.style.setProperty('text-align', 'left');
    LFace.style.setProperty('width', '100%');
    LFace.style.setProperty('min-height', '40px');
    LFace.textContent := Button(Plan[LIndex].Part).Text;

    if Plan[LIndex].IsEnabled then
    begin
      LFace.setAttribute('aria-disabled', 'false');
      LFace.style.setProperty('opacity', '1');
    end
    else
    begin
      LFace.setAttribute('aria-disabled', 'true');
      LFace.style.setProperty('opacity', '0.55');
    end;

    if Plan[LIndex].Kind in [nmiCheck, nmiRadio] then
    begin

      if Plan[LIndex].IsChecked then
      begin
        LFace.setAttribute('aria-checked', 'true');
        LFace.textContent := TNyxText('✓  ') + Button(Plan[LIndex].Part).Text;
      end
      else
      begin
        LFace.setAttribute('aria-checked', 'false');
        LFace.textContent := TNyxText('    ') + Button(Plan[LIndex].Part).Text;
      end;
    end;
    { A visual marker never changes the creator's accessible caption. }
    LFace.setAttribute('aria-label', Button(Plan[LIndex].Part).Text);

    if Plan[LIndex].Kind = nmiSubmenu then
    begin
      LFace.setAttribute('aria-haspopup', 'menu');

      if BranchOpen(LIndex) then
      begin
        LFace.setAttribute('aria-expanded', 'true');
      end
      else
      begin
        LFace.setAttribute('aria-expanded', 'false');
      end;
      LFace.textContent := Button(Plan[LIndex].Part).Text + '  ›';
    end;
  end;
end;

function TBrowserMenu.FocusFace(AIndex: Integer): Boolean;
var
  LFace: TJSHTMLElement;
begin
  LFace := FHost.Renderer.FocusFor(ItemID(AIndex), niDesign);
  NyxFocusWithoutScroll(LFace);
  Result := document.activeElement = LFace;
end;

procedure TBrowserMenu.TabExit(AReverse: Boolean; const AExecution: INyxExecution);
begin
  { Close has restored the invoking control. Leave Tab unconsumed so the browser
    performs ordinary document traversal from that control in either direction. }
end;

function TBrowserMenu.TextKey(AEvent: TJSKeyboardEvent): Boolean;
var
  LKeepAlive: INyxMenu;
begin
  Result := True;

  if AEvent.defaultPrevented or AEvent.isComposing or AEvent.altKey or
    AEvent.ctrlKey or AEvent.metaKey or
    (TJSHTMLElement(AEvent.target).closest('[role=menu]') <> FHost.Element) then
  begin
    Exit;
  end;
  LKeepAlive := Self;

  if TextInput(AEvent.key, window.performance.now) then
  begin
    AEvent.preventDefault;
  end;
  LKeepAlive.GetOpen;
end;

procedure TBrowserMenu.PresentationChanged;
begin
  inherited PresentationChanged;

  if FAnchor = nil then
  begin
    Exit;
  end;

  if GetOpen then
  begin
    FAnchor.setAttribute('aria-expanded', 'true');
  end
  else
  begin
    FAnchor.setAttribute('aria-expanded', 'false');
  end;
end;

function TBrowserMenu.CreateSubmenu(AIndex: Integer;
  const ARecipe: INyxMenuRecipe): TNyxMenuPresenter;
var
  LDocument: TNyxDocument;
begin
  LDocument := ARecipe.CopyDocument;
  try
    Result := TBrowserMenu.Create(FHost.Renderer.FocusFor(ItemID(AIndex), niDesign),
      LDocument, ARecipe.Root, ARecipe.Items, FTheme, FHost);
  finally
    LDocument.Free;
  end;
end;

function NewNyxBrowserMenu(AAnchor: TJSHTMLElement; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; const AItems: TNyxMenuItems;
  ATheme: TNyxTheme): INyxBrowserMenu;
begin
  Result := TBrowserMenu.Create(AAnchor, ADocument, ARoot, AItems, ATheme);
end;

end.
