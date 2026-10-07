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

unit nyx.popover.browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

interface

uses
  JS, Web, nyx.text, nyx.types, nyx.root.types, nyx.model, nyx.theme,
  nyx.behavior, nyx.events, nyx.popover, nyx.render.browser;

type
  { Element and Renderer are borrowed observations, owned by the presenter.
    Anchor is weak in the DOM sense: detachment dismisses and refuses reopening.
    Removing either host's children or freeing Renderer externally is invalid. }
  INyxBrowserPopover = interface(INyxPopover)
    ['{B2672EF9-496D-4EC9-8006-061026000002}']
    function GetElement: TJSHTMLElement;
    function GetRenderer: TNyxBrowserRenderer;
    property Element: TJSHTMLElement read GetElement;
    property Renderer: TNyxBrowserRenderer read GetRenderer;
  end;

{ Anchor is a borrowed physical target seam. Pass renderer.FocusFor(Control.ID)
  for a Nyx button. The document/root are copied; theme must outlive presenter. }
function NewNyxBrowserPopover(AAnchor: TJSHTMLElement;
  ADocument: TNyxDocument; const ARoot: TNyxRootRef;
  ATheme: TNyxTheme = nil): INyxBrowserPopover;

implementation

uses
  SysUtils, Math, nyx.interaction;

type
  { Standard HTML top-layer operations absent in the older installed Web binding.
    No script injection, framework, custom layout toolkit or modal background. }
  TPopoverElement = class external name 'HTMLElement'(TJSHTMLElement)
    procedure showPopover;
    procedure hidePopover;
  end;
  TPopoverDocument = class external name 'Document'(TJSDocument)
    { Older Web declares only the two-argument removal form. Capture must match
      registration exactly, otherwise a retired presenter leaves an input hook. }
    procedure removeEventListener(const AName: String;
      const AListener: TJSEventHandler; ACapture: Boolean); reintroduce;
  end;
  TVisualViewport = class external name 'VisualViewport'(TJSObject)
    offsetLeft: Double;
    offsetTop: Double;
    width: Double;
    height: Double;
  end;
  TPopoverWindow = class external name 'Window'(TJSWindow)
    visualViewport: TVisualViewport;
  end;

  TBrowserPopover = class(TNyxPopoverPresenter, INyxPopover, INyxBrowserPopover)
  private
    FAnchor: TJSHTMLElement;
    FElement: TJSHTMLElement;
    FInvoker: TJSHTMLElement;
    FRenderer: TNyxBrowserRenderer;
    FOptions: TNyxPopoverOptions;
    FTimer: NativeInt;
    FVisible: Boolean;
    function AnchorAvailable: Boolean;
    procedure Reposition;
    procedure Tick;
    function PointerDown(AEvent: TJSEvent): Boolean;
    function KeyDown(AEvent: TJSEvent): Boolean;
    procedure Semantic(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  protected
    procedure Present(const AOptions: TNyxPopoverOptions;
      const AFocusID: TNyxText); override;
    procedure Conceal(ARestoreFocus: Boolean); override;
  public
    constructor Create(AAnchor: TJSHTMLElement; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef; ATheme: TNyxTheme);
    destructor Destroy; override;
    function GetElement: TJSHTMLElement;
    function GetRenderer: TNyxBrowserRenderer;
    function GetEvents: INyxEvents; override;
  end;

constructor TBrowserPopover.Create(AAnchor: TJSHTMLElement;
  ADocument: TNyxDocument; const ARoot: TNyxRootRef; ATheme: TNyxTheme);
begin
  inherited Create(ADocument, ARoot);

  if AAnchor = nil then
  begin
    raise ENyxModel.Create('Popover requires an anchor');
  end;
  FAnchor := AAnchor;
  FTimer := -1;
  FElement := TJSHTMLElement(Web.document.createElement('div'));
  FElement.setAttribute('popover', 'manual');
  FElement.setAttribute('role', 'dialog');
  FElement.setAttribute('tabindex', '-1');
  FElement.classList.add('nyx-popover');
  FElement.style.cssText := 'position:fixed;inset:auto;margin:0;padding:0;' +
    'box-sizing:border-box;overflow:auto;border:1px solid #dddce8;' +
    'border-radius:16px;background:white;color:#202331;' +
    'box-shadow:0 14px 44px rgba(24,24,46,.18);';
  Web.document.body.appendChild(FElement);
  FRenderer := TNyxBrowserRenderer.Create(ATheme);
  FRenderer.OnEvent := Semantic;
end;

destructor TBrowserPopover.Destroy;
begin
  Conceal(False);

  if FRenderer <> nil then
  begin
    FRenderer.OnEvent := nil;
    FRenderer.Free;
  end;

  if FElement <> nil then
  begin
    FElement.remove;
  end;
  inherited Destroy;
end;

procedure TBrowserPopover.Semantic(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (AEvent.SourceID = GetContent.ID) and AEvent.IsNamed(NyxSemantic(nseDismiss)) then
  begin
    Dismiss(nprAction);
  end;
end;

function TBrowserPopover.AnchorAvailable: Boolean;
var
  LRect: TJSDOMRect;
begin
  Result := (FAnchor <> nil) and Web.document.body.contains(FAnchor);

  if Result then
  begin
    LRect := FAnchor.getBoundingClientRect;
    Result := (LRect.width > 0) and (LRect.height > 0) and
      not FAnchor.hasAttribute('disabled') and (FAnchor.closest('[inert]') = nil);
  end;
end;

procedure TBrowserPopover.Reposition;
var
  LAnchor: TJSDOMRect;
  LViewport: TNyxPopoverRect;
  LPlaced: TNyxPopoverRect;
  LVisual: TVisualViewport;
  LOptions: TNyxPopoverOptions;
begin
  LAnchor := FAnchor.getBoundingClientRect;
  LVisual := TPopoverWindow(window).visualViewport;

  if LVisual <> nil then
  begin
    LViewport := NyxPopoverRect(Floor(LVisual.offsetLeft), Floor(LVisual.offsetTop),
      Floor(LVisual.width), Floor(LVisual.height));
  end
  else
  begin
    LViewport := NyxPopoverRect(0, 0, window.innerWidth, window.innerHeight);
  end;
  LOptions := FOptions;

  if FVisible and (FOptions.SizeMode = npzContent) then
  begin
    LOptions := FOptions.Size(FOptions.Width, Min(FOptions.Height,
      Max(16, Ceil(FRenderer.ElementFor(GetContent.ID).getBoundingClientRect.height) +
        Ceil(FElement.offsetHeight) - Floor(FElement.clientHeight))));
  end;
  LPlaced := PlaceNyxPopover(NyxPopoverRect(Floor(LAnchor.left), Floor(LAnchor.top),
    Ceil(LAnchor.width), Ceil(LAnchor.height)), LViewport, LOptions);
  FElement.style.setProperty('left', IntToStr(LPlaced.Left) + 'px');
  FElement.style.setProperty('top', IntToStr(LPlaced.Top) + 'px');
  FElement.style.setProperty('width', IntToStr(LPlaced.Width) + 'px');
  FElement.style.setProperty('height', IntToStr(LPlaced.Height) + 'px');
end;

procedure TBrowserPopover.Present(const AOptions: TNyxPopoverOptions;
  const AFocusID: TNyxText);
var
  LFocus: TJSHTMLElement;
  LTheme: TJSCSSStyleDeclaration;
begin

  if not AnchorAvailable then
  begin
    raise ENyxModel.Create('Popover anchor is unavailable');
  end;
  FOptions := AOptions;
  FInvoker := TJSHTMLElement(Web.document.activeElement);
  FElement.setAttribute('aria-label', AOptions.Title);

  if not Mounted then
  begin
    FRenderer.Render(Document, GetContent.Node, FElement, False, State, Collections);
  end;
  { The ordinary rendered view supplies its semantic palette. Contextual chrome
    follows that exact theme instead of baking a light-only surface around it. }
  LTheme := window.getComputedStyle(FRenderer.ElementFor(GetContent.ID));
  FElement.style.setProperty('background', LTheme.getPropertyValue('--nyx-surface'));
  FElement.style.setProperty('color', LTheme.getPropertyValue('--nyx-text'));
  FElement.style.setProperty('border-color', LTheme.getPropertyValue('--nyx-border'));
  FElement.style.setProperty('border-radius', LTheme.getPropertyValue('--nyx-radius'));
  LFocus := nil;

  if AFocusID <> '' then
  begin
    LFocus := FRenderer.FocusFor(AFocusID, niDesign);

    if (LFocus = nil) or not NyxInteractionPolicy(
      FRenderer.Root.Find(AFocusID)).CanIssueCommand then
    begin
      raise ENyxModel.Create('Popover initial part has no available focus face');
    end;
  end;
  try
    Reposition;
    TPopoverElement(FElement).showPopover;
    FVisible := True;
    Reposition;
    Web.document.addEventListener('pointerdown', @PointerDown, True);
    { Bubble after the input's ordinary Nyx before/main/after hooks. A consumed
      Escape stays with the child; it must not close the containing presentation. }
    Web.document.addEventListener('keydown', @KeyDown);
    FTimer := window.setInterval(@Tick, 100);

    if LFocus <> nil then
    begin
      NyxFocusWithoutScroll(LFocus);

      if Web.document.activeElement <> LFocus then
      begin
        raise ENyxModel.Create('Popover initial focus was refused');
      end;
    end;
  except
    Conceal(True);
    raise;
  end;
end;

procedure TBrowserPopover.Conceal(ARestoreFocus: Boolean);
var
  LInvoker: TJSHTMLElement;
begin

  if FTimer >= 0 then
  begin
    window.clearInterval(FTimer);
    FTimer := -1;
  end;
  TPopoverDocument(Web.document).removeEventListener('pointerdown', @PointerDown, True);
  Web.document.removeEventListener('keydown', @KeyDown);
  LInvoker := FInvoker;
  FInvoker := nil;

  if FVisible then
  begin
    FVisible := False;
    TPopoverElement(FElement).hidePopover;
  end;

  if ARestoreFocus and (LInvoker <> nil) and Web.document.body.contains(LInvoker) then
  begin
    NyxFocusWithoutScroll(LInvoker);
  end;
end;

procedure TBrowserPopover.Tick;
var
  LKeepAlive: INyxPopover;
begin
  LKeepAlive := Self;

  if IsOpen then
  begin

    if not AnchorAvailable then
    begin
      Dismiss(nprAnchorUnavailable);
    end
    else
    begin
      Reposition;
    end;
  end;
  LKeepAlive.GetOpen;
end;

function TBrowserPopover.PointerDown(AEvent: TJSEvent): Boolean;
var
  LKeepAlive: INyxPopover;
begin
  LKeepAlive := Self;
  Result := True;

  if IsOpen and (npdOutsidePress in FOptions.Dismissals) and
    not FElement.contains(TJSNode(AEvent.target)) and
    not FAnchor.contains(TJSNode(AEvent.target)) then
  begin
    Dismiss(nprOutsidePress);
  end;
  LKeepAlive.GetOpen;
end;

function TBrowserPopover.KeyDown(AEvent: TJSEvent): Boolean;
var
  LKeepAlive: INyxPopover;
begin
  LKeepAlive := Self;
  Result := True;

  if IsOpen and (npdEscape in FOptions.Dismissals) and
    (TJSKeyboardEvent(AEvent).key = 'Escape') and not AEvent.defaultPrevented and
    (FElement.contains(TJSNode(AEvent.target)) or
      FAnchor.contains(TJSNode(AEvent.target))) then
  begin
    AEvent.preventDefault;
    AEvent.stopPropagation;
    Dismiss(nprEscape);
  end;
  LKeepAlive.GetOpen;
end;

function TBrowserPopover.GetElement: TJSHTMLElement;
begin
  Result := FElement;
end;

function TBrowserPopover.GetRenderer: TNyxBrowserRenderer;
begin
  Result := FRenderer;
end;

function TBrowserPopover.GetEvents: INyxEvents;
begin
  Result := FRenderer.Events;
end;

function NewNyxBrowserPopover(AAnchor: TJSHTMLElement; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; ATheme: TNyxTheme): INyxBrowserPopover;
begin
  Result := TBrowserPopover.Create(AAnchor, ADocument, ARoot, ATheme);
end;

end.
