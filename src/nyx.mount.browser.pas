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


unit nyx.mount.browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

interface

uses JS, Web, SysUtils, Math, nyx.text;

type
  { A live browsing context must remain connected while its editor placement
    changes. This adapter owns an unattached content element and one body-level
    clipping layer; it borrows an empty placement host. Only a lightweight
    placeholder moves. The actual content never changes parent until Destroy.

    Geometry follows ordinary axis-aligned hosts, clipping ancestors and the
    viewport. Detached/hidden placement hides the layer without retiring its
    document. Arbitrary rotations, perspective and transformed body roots are
    outside this adapter's geometry contract. Applications own semantic state;
    no reference to the caller's document or renderer is retained. }
  TNyxBrowserPersistentMount = class
  private
    FHost: TJSHTMLElement;
    FContent: TJSHTMLElement;
    FPlaceholder: TJSHTMLElement;
    FLayer: TJSHTMLElement;
    FObserver: TJSHTMLResizeObserver;
    FQueued: Boolean;
    FFrame: NativeInt;
    function AllocationChanged(AEvent: TEventListenerEvent): Boolean;
    procedure BoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
      AObserver: TJSHTMLResizeObserver);
    procedure Queue;
    procedure Paint(ATime: Double);
    procedure Observe;
  public
    { Ownership transfers only after validation. Height is in logical CSS pixels;
      width follows the placement host. Missing/occupied hosts, already parented
      content and nonfinite/nonpositive heights refuse before DOM mutation. }
    constructor Create(AContent, AHost: TJSHTMLElement; AHeight: Double);
    { Revoke observers/queued work before removing the owned layer and content.
      Destruction is the explicit browsing-context retirement boundary. }
    destructor Destroy; override;
    { Placement hosts must be empty and outside the old placeholder. Moving or
      temporarily detaching the host preserves the connected content and focus. }
    procedure MoveHost(AHost: TJSHTMLElement);
    { Recompute visible clipping after synchronous shell layout changes. Scroll,
      resize and allocation observers also coalesce updates into one frame. }
    procedure Sync;
  end;

implementation

type
  { The matched RTL omits capture on listener removal. Revocation must use the
    same capture flag used for ancestor scroll observation. }
  TNyxCaptureTarget = class external name 'EventTarget' (TJSEventTarget)
    procedure Listen(const AName: String; AHandler: TJSEventHandler;
      ACapture: Boolean); external name 'addEventListener';
    procedure Unlisten(const AName: String; AHandler: TJSEventHandler;
      ACapture: Boolean); external name 'removeEventListener';
  end;

function Pixels(AValue: Double): String;
var
  LFormat: TFormatSettings;
begin
  LFormat := FormatSettings;
  LFormat.DecimalSeparator := '.';
  Result := FloatToStr(AValue, LFormat) + 'px';
end;

function Clips(const AOverflow: String): Boolean;
begin
  Result := (AOverflow = 'hidden') or (AOverflow = 'auto') or
    (AOverflow = 'scroll') or (AOverflow = 'clip');
end;

constructor TNyxBrowserPersistentMount.Create(AContent, AHost: TJSHTMLElement;
  AHeight: Double);
begin
  inherited Create;

  if (AContent = nil) or (AHost = nil) or (document.body = nil) or
    (AContent.parentNode <> nil) or (AHost.firstChild <> nil) or
    IsNan(AHeight) or IsInfinite(AHeight) or (AHeight <= 0) then
  begin
    raise Exception.Create('Persistent browser mount needs unattached content and an empty host');
  end;
  FHost := AHost;
  FPlaceholder := TJSHTMLElement(document.createElement('div'));
  FPlaceholder.className := 'nyx-live-placement';
  FPlaceholder.style.setProperty('width', '100%');
  FPlaceholder.style.setProperty('height', Pixels(AHeight));
  FPlaceholder.style.setProperty('flex-shrink', '0');
  FLayer := TJSHTMLElement(document.createElement('div'));
  FLayer.className := 'nyx-live-layer';
  FLayer.style.setProperty('position', 'fixed');
  FLayer.style.setProperty('overflow', 'hidden');
  FLayer.style.setProperty('z-index', '1');
  FLayer.style.setProperty('display', 'none');
  FContent := AContent;
  FContent.style.setProperty('position', 'absolute');
  FContent.style.setProperty('margin', '0');
  FHost.appendChild(FPlaceholder);
  document.body.appendChild(FLayer);
  { One initial connection creates the document. Later geometry/hide operations
    never remove, reparent or rewrite the content's source attribute. }
  FLayer.appendChild(FContent);
  TNyxCaptureTarget(window).Listen('scroll', @AllocationChanged, True);
  TNyxCaptureTarget(window).Listen('resize', @AllocationChanged, False);
  Observe;
  Sync;
end;

destructor TNyxBrowserPersistentMount.Destroy;
begin

  if FObserver <> nil then
  begin
    FObserver.disconnect;
    FObserver := nil;
  end;
  TNyxCaptureTarget(window).Unlisten('scroll', @AllocationChanged, True);
  TNyxCaptureTarget(window).Unlisten('resize', @AllocationChanged, False);

  if FQueued then
  begin
    window.cancelAnimationFrame(FFrame);
    FQueued := False;
  end;

  if FLayer <> nil then
  begin
    FLayer.remove;
  end;

  if FPlaceholder <> nil then
  begin
    FPlaceholder.remove;
  end;
  FContent := nil;
  FHost := nil;
  FLayer := nil;
  FPlaceholder := nil;
  inherited Destroy;
end;

function TNyxBrowserPersistentMount.AllocationChanged(
  AEvent: TEventListenerEvent): Boolean;
begin
  Result := True;
  Queue;
end;

procedure TNyxBrowserPersistentMount.BoundsChanged(
  AEntries: TJSHTMLResizeObserverEntryArray; AObserver: TJSHTMLResizeObserver);
begin
  Queue;
end;

procedure TNyxBrowserPersistentMount.Queue;
begin

  if FQueued or (FLayer = nil) then
  begin
    Exit;
  end;
  FQueued := True;
  FFrame := window.requestAnimationFrame(@Paint);
end;

procedure TNyxBrowserPersistentMount.Paint(ATime: Double);
begin
  FQueued := False;
  Sync;
end;

procedure TNyxBrowserPersistentMount.Observe;
var
  LAncestor: TJSHTMLElement;
begin

  if FObserver <> nil then
  begin
    FObserver.disconnect;
  end
  else
  begin
    FObserver := TJSHTMLResizeObserver.new(@BoundsChanged);
  end;
  FObserver.observe(FPlaceholder);
  LAncestor := FHost;
  while LAncestor <> nil do
  begin
    FObserver.observe(LAncestor);
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;
end;

procedure TNyxBrowserPersistentMount.MoveHost(AHost: TJSHTMLElement);
begin

  if AHost = nil then
  begin
    raise Exception.Create('Persistent browser placement requires a host');
  end;

  if AHost <> FHost then
  begin

    if FPlaceholder.contains(AHost) or (AHost.firstChild <> nil) then
    begin
      raise Exception.Create('Replacement browser placement must be empty and outside content');
    end;
    AHost.appendChild(FPlaceholder);
    FHost := AHost;
    Observe;
  end;
  Sync;
end;

procedure TNyxBrowserPersistentMount.Sync;
var
  LRect: TJSDOMRect;
  LClip: TJSDOMRect;
  LAncestor: TJSHTMLElement;
  LStyle: TJSCSSStyleDeclaration;
  LLeft: Double;
  LTop: Double;
  LRight: Double;
  LBottom: Double;
  LScaleX: Double;
  LScaleY: Double;
begin

  if (FLayer = nil) or (FHost = nil) then
  begin
    Exit;
  end;

  if not document.body.contains(FPlaceholder) then
  begin
    FLayer.style.setProperty('display', 'none');
    Exit;
  end;
  LRect := FPlaceholder.getBoundingClientRect;
  LLeft := Max(0, LRect.left);
  LTop := Max(0, LRect.top);
  LRight := Min(window.innerWidth, LRect.right);
  LBottom := Min(window.innerHeight, LRect.bottom);
  LAncestor := FHost;
  while LAncestor <> nil do
  begin
    LStyle := window.getComputedStyle(LAncestor);

    if (LStyle.getPropertyValue('display') = 'none') or
      (LStyle.getPropertyValue('visibility') = 'hidden') or
      (LStyle.getPropertyValue('visibility') = 'collapse') then
    begin
      FLayer.style.setProperty('display', 'none');
      Exit;
    end;
    LClip := LAncestor.getBoundingClientRect;
    LScaleX := 1;
    LScaleY := 1;

    if LAncestor.offsetWidth > 0 then
    begin
      LScaleX := LClip.width / LAncestor.offsetWidth;
    end;

    if LAncestor.offsetHeight > 0 then
    begin
      LScaleY := LClip.height / LAncestor.offsetHeight;
    end;

    if Clips(LStyle.getPropertyValue('overflow-x')) then
    begin
      LLeft := Max(LLeft, LClip.left + LAncestor.clientLeft * LScaleX);
      LRight := Min(LRight, LClip.left +
        (LAncestor.clientLeft + LAncestor.clientWidth) * LScaleX);
    end;

    if Clips(LStyle.getPropertyValue('overflow-y')) then
    begin
      LTop := Max(LTop, LClip.top + LAncestor.clientTop * LScaleY);
      LBottom := Min(LBottom, LClip.top +
        (LAncestor.clientTop + LAncestor.clientHeight) * LScaleY);
    end;
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;

  if (LRect.width <= 0) or (LRect.height <= 0) or
    (LRight <= LLeft) or (LBottom <= LTop) then
  begin
    FLayer.style.setProperty('display', 'none');
    Exit;
  end;
  FLayer.style.setProperty('left', Pixels(LLeft));
  FLayer.style.setProperty('top', Pixels(LTop));
  FLayer.style.setProperty('width', Pixels(LRight - LLeft));
  FLayer.style.setProperty('height', Pixels(LBottom - LTop));
  FContent.style.setProperty('left', Pixels(LRect.left - LLeft));
  FContent.style.setProperty('top', Pixels(LRect.top - LTop));
  { Clipping the face must not resize the application's layout viewport. }
  FContent.style.setProperty('width', Pixels(LRect.width));
  FContent.style.setProperty('height', Pixels(LRect.height));
  FLayer.style.setProperty('display', 'block');
end;

end.
