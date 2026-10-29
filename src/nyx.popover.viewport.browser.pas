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

unit nyx.popover.viewport.browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

interface

uses Web, nyx.popover.types;

{ Visible allocation in this window's client coordinates. Same-origin frame
  ancestors contribute their viewport and per-axis overflow clips. All DOM and
  Window objects are borrowed for this synchronous read; nothing is retained.
  Supports positive axis-aligned translation/scale. Cross-origin parents expose
  no frameElement: their unavailable geometry cannot be inferred here. Arbitrary
  rotation, perspective and nonrectangular clipping are outside this contract. }
function NyxBrowserPopoverViewport: TNyxPopoverRect;

{ Geometry-only weak-anchor observation. The anchor must retain a nonempty
  intersection with its own overflow ancestors and the embedded viewport.
  No containing clip constrains the popup itself: top-layer content may escape
  the anchor's scroll container while remaining inside the visible frame. }
function NyxBrowserPopoverAnchorVisible(AAnchor: TJSHTMLElement): Boolean;

implementation

uses Math, JS;

type
  { Keep subpixel edges through nested coordinate conversions. Round inward only
    once at the public integer boundary, so rounding never enlarges a clip. }
  TViewportEdges = record
    Left: Double;
    Top: Double;
    Right: Double;
    Bottom: Double;
  end;

  { Standard VisualViewport geometry, absent from the installed Web binding.
    The Window owns this borrowed host object; older hosts may omit it. }
  TVisualViewport = class external name 'VisualViewport'(TJSObject)
    offsetLeft: Double;
    offsetTop: Double;
    width: Double;
    height: Double;
  end;

  { Optional browser visual viewport. Never retain this bridge across a layout
    sample or substitute parent screen coordinates for child client coordinates. }
  TViewportWindow = class external name 'Window'(TJSWindow)
    visualViewport: TVisualViewport;
  end;

  { Standard Node connectivity includes shadow descendants. The underlying DOM
    owns the frame; a detached browsing context provides no reachable allocation. }
  TConnectedElement = class external name 'Element'(TJSHTMLElement)
    isConnected: Boolean;
  end;

function EmptyEdges: TViewportEdges;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
end;

function Clips(const AOverflow: String): Boolean;
begin
  Result := (AOverflow = 'hidden') or (AOverflow = 'clip') or
    (AOverflow = 'auto') or (AOverflow = 'scroll');
end;

function Padding(const AStyle: TJSCSSStyleDeclaration; const AName: String): Double;
begin
  Result := JS.parseFloat(AStyle.getPropertyValue(AName));

  if IsNan(Result) or IsInfinite(Result) then
  begin
    Result := 0;
  end;
end;

function ClipHostAncestors(AElement: TJSHTMLElement; var AEdges: TViewportEdges): Boolean;
var
  LWindow: TJSWindow;
  LAncestor: TJSHTMLElement;
  LStyle: TJSCSSStyleDeclaration;
  LRect: TJSDOMRect;
  LScaleX: Double;
  LScaleY: Double;
begin
  LWindow := AElement.ownerDocument.defaultView;
  LAncestor := AElement;
  while LAncestor <> nil do
  begin
    LStyle := LWindow.getComputedStyle(LAncestor);

    if (LStyle.getPropertyValue('display') = 'none') or
      (LStyle.getPropertyValue('visibility') = 'hidden') or
      (LStyle.getPropertyValue('visibility') = 'collapse') then
    begin
      Result := False;
      Exit;
    end;
    { The observed element's own allocation is already represented by its
      rectangle. Only containing element overflow contributes another clip. }

    if LAncestor <> AElement then
    begin
      LRect := LAncestor.getBoundingClientRect;
      LScaleX := 1;
      LScaleY := 1;

      if LAncestor.offsetWidth > 0 then
      begin
        LScaleX := LRect.width / LAncestor.offsetWidth;
      end;

      if LAncestor.offsetHeight > 0 then
      begin
        LScaleY := LRect.height / LAncestor.offsetHeight;
      end;

      if Clips(LStyle.getPropertyValue('overflow-x')) then
      begin
        AEdges.Left := Max(AEdges.Left, LRect.left + LAncestor.clientLeft * LScaleX);
        AEdges.Right := Min(AEdges.Right, LRect.left +
          (LAncestor.clientLeft + LAncestor.clientWidth) * LScaleX);
      end;

      if Clips(LStyle.getPropertyValue('overflow-y')) then
      begin
        AEdges.Top := Max(AEdges.Top, LRect.top + LAncestor.clientTop * LScaleY);
        AEdges.Bottom := Min(AEdges.Bottom, LRect.top +
          (LAncestor.clientTop + LAncestor.clientHeight) * LScaleY);
      end;
    end;
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;
  Result := (AEdges.Right > AEdges.Left) and (AEdges.Bottom > AEdges.Top);
end;

function VisibleEdges(AWindow: TJSWindow): TViewportEdges;
var
  LVisual: TVisualViewport;
  LFrame: TJSHTMLElement;
  LParentWindow: TJSWindow;
  LParent: TViewportEdges;
  LFrameStyle: TJSCSSStyleDeclaration;
  LFrameRect: TJSDOMRect;
  LScaleX: Double;
  LScaleY: Double;
  LContentLeft: Double;
  LContentTop: Double;
begin
  LVisual := TViewportWindow(AWindow).visualViewport;

  if LVisual <> nil then
  begin
    Result.Left := LVisual.offsetLeft;
    Result.Top := LVisual.offsetTop;
    Result.Right := Result.Left + LVisual.width;
    Result.Bottom := Result.Top + LVisual.height;
  end
  else
  begin
    Result := EmptyEdges;
    Result.Right := AWindow.innerWidth;
    Result.Bottom := AWindow.innerHeight;
  end;
  { frameElement is null at the top level and at a cross-origin boundary. Reading
    the exposed frame's ownerDocument stays within the same-origin contract. }
  LFrame := TJSHTMLElement(AWindow.frameElement);

  if LFrame = nil then
  begin
    Exit;
  end;

  if not TConnectedElement(LFrame).isConnected or
    (LFrame.offsetWidth <= 0) or (LFrame.offsetHeight <= 0) then
  begin
    Result := EmptyEdges;
    Exit;
  end;
  LParentWindow := LFrame.ownerDocument.defaultView;
  LParent := VisibleEdges(LParentWindow);

  if not ClipHostAncestors(LFrame, LParent) then
  begin
    Result := EmptyEdges;
    Exit;
  end;
  LFrameRect := LFrame.getBoundingClientRect;
  LFrameStyle := LParentWindow.getComputedStyle(LFrame);
  LScaleX := LFrameRect.width / LFrame.offsetWidth;
  LScaleY := LFrameRect.height / LFrame.offsetHeight;

  if (LScaleX <= 0) or (LScaleY <= 0) or
    (LParent.Right <= LParent.Left) or (LParent.Bottom <= LParent.Top) then
  begin
    Result := EmptyEdges;
    Exit;
  end;
  { Child coordinates start at the frame content box, after the scaled border
    and padding. Ancestor scrolling/translation is already in the client rect. }
  LContentLeft := LFrameRect.left +
    (LFrame.clientLeft + Padding(LFrameStyle, 'padding-left')) * LScaleX;
  LContentTop := LFrameRect.top +
    (LFrame.clientTop + Padding(LFrameStyle, 'padding-top')) * LScaleY;
  Result.Left := Max(Result.Left, (LParent.Left - LContentLeft) / LScaleX);
  Result.Top := Max(Result.Top, (LParent.Top - LContentTop) / LScaleY);
  Result.Right := Min(Result.Right, (LParent.Right - LContentLeft) / LScaleX);
  Result.Bottom := Min(Result.Bottom, (LParent.Bottom - LContentTop) / LScaleY);
end;

function NyxBrowserPopoverAnchorVisible(AAnchor: TJSHTMLElement): Boolean;
var
  LBounds: TViewportEdges;
  LRect: TJSDOMRect;
begin
  Result := False;

  if (AAnchor = nil) or not TConnectedElement(AAnchor).isConnected then
  begin
    Exit;
  end;
  LBounds := VisibleEdges(AAnchor.ownerDocument.defaultView);
  LRect := AAnchor.getBoundingClientRect;
  LBounds.Left := Max(LBounds.Left, LRect.left);
  LBounds.Top := Max(LBounds.Top, LRect.top);
  LBounds.Right := Min(LBounds.Right, LRect.right);
  LBounds.Bottom := Min(LBounds.Bottom, LRect.bottom);
  Result := ClipHostAncestors(AAnchor, LBounds);
end;

function NyxBrowserPopoverViewport: TNyxPopoverRect;
var
  LEdges: TViewportEdges;
begin
  LEdges := VisibleEdges(window);
  Result := NyxPopoverRect(Ceil(LEdges.Left), Ceil(LEdges.Top),
    Max(0, Floor(LEdges.Right) - Ceil(LEdges.Left)),
    Max(0, Floor(LEdges.Bottom) - Ceil(LEdges.Top)));
end;

end.
