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

program nyx_popover_embedded_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Web, nyx.root.types, nyx.controls,
  nyx.model, nyx.popover, nyx.popover.browser;

var
  GPopover: INyxBrowserPopover;
  GAnchor: TJSHTMLElement;
  GAnchorHost: TJSHTMLElement;
  GClip: TJSHTMLElement;
  GFrame: TJSHTMLIFrameElement;
  GTimer: NativeInt;
  GPhase: Integer;
  GWait: Integer;
  GChecks: Integer;
  GCommand: String;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Fail(const AReason: String);
begin
  window.clearInterval(GTimer);
  document.body.setAttribute('data-result', 'failed');
  document.body.setAttribute('data-error', AReason);
end;

procedure OpenPopover;
begin
  GPopover.Open(NyxPopover('Embedded actions').Size(280, 340));
end;

procedure ChildTick;
var
  LCommand: String;
begin
  try
    LCommand := document.body.getAttribute('data-command');

    if LCommand <> GCommand then
    begin
      GCommand := LCommand;
      case LCommand of
        'hide':
          begin
            GAnchor.style.setProperty('display', 'none');
          end;
        'clip-local':
          begin
            GAnchorHost.style.setProperty('overflow', 'hidden');
            GAnchorHost.style.setProperty('width', '80px');
          end;
        'open', 'reopen':
          begin
            GAnchor.style.removeProperty('display');
            GAnchorHost.style.setProperty('overflow', 'visible');
            GAnchorHost.style.setProperty('width', '640px');
            OpenPopover;
          end;
        'release':
          begin
            GPopover := nil;
          end;
      end;
    end;

    if GPopover <> nil then
    begin
      document.body.setAttribute('data-open', BoolToStr(GPopover.IsOpen, True));
      document.body.setAttribute('data-reason', IntToStr(Ord(GPopover.LastReason)));
    end
    else
    begin
      document.body.setAttribute('data-open', 'released');
    end;
  except
    on LError: Exception do
    begin
      Fail(LError.Message);
    end;
  end;
end;

procedure Child;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LIndex: Integer;
begin
  TJSHTMLElement(document.body).style.cssText := 'margin:0;background:#f3f2f9;';
  GAnchor := TJSHTMLElement(document.createElement('button'));
  GAnchorHost := TJSHTMLElement(document.createElement('div'));
  GAnchorHost.style.cssText := 'position:absolute;left:0;top:0;width:640px;height:620px;';
  GAnchor.textContent := 'Open embedded actions';
  GAnchor.style.cssText := 'position:absolute;left:100px;top:112px;width:160px;height:36px;';
  GAnchorHost.appendChild(GAnchor);
  document.body.appendChild(GAnchorHost);
  LDocument := TNyxDocument.Create;
  try
    LPage := NewNyxPage('actions');
    LPage.Add(NewNyxButton('first-action').WithText('First reachable action'));
    for LIndex := 1 to 16 do
    begin
      LPage.Add(NewNyxLabel('detail-' + IntToStr(LIndex)).WithText('Action details'));
    end;
    LDocument.AddPage(LPage);
    GPopover := NewNyxBrowserPopover(GAnchor, LDocument, NyxPageRoot('actions'));
  finally
    LDocument.Free;
  end;
  OpenPopover;
  document.body.setAttribute('data-ready', 'true');
  GTimer := window.setInterval(@ChildTick, 50);
end;

{ The fixture deliberately maps the rendered allocation through known physical
  frames. It never calls the product viewport helper or inspects presenter state. }
procedure Fits(AFrame: TJSHTMLIFrameElement; AClip: TJSHTMLElement;
  const AReason: String; AClipX: Boolean = True; AClipY: Boolean = True);
var
  LPopup: TJSHTMLElement;
  LRect: TJSDOMRect;
  LFrameRect: TJSDOMRect;
  LClipRect: TJSDOMRect;
  LScaleX: Double;
  LScaleY: Double;
  LLeft: Double;
  LTop: Double;
  LRight: Double;
  LBottom: Double;
  LWindow: TJSWindow;
  LCurrentFrame: TJSHTMLIFrameElement;
  LLeaf: TJSDocument;
begin
  LLeaf := AFrame.contentDocument;
  while LLeaf.querySelector('iframe') <> nil do
  begin
    LLeaf := TJSHTMLIFrameElement(LLeaf.querySelector('iframe')).contentDocument;
  end;
  LPopup := TJSHTMLElement(LLeaf.querySelector('.nyx-popover'));
  Check(LPopup <> nil, AReason + ': mounted');
  LRect := LPopup.getBoundingClientRect;
  LClipRect := AClip.getBoundingClientRect;
  LLeft := LRect.left;
  LTop := LRect.top;
  LRight := LRect.right;
  LBottom := LRect.bottom;
  LWindow := LLeaf.defaultView;
  while LWindow <> AClip.ownerDocument.defaultView do
  begin
    LCurrentFrame := TJSHTMLIFrameElement(LWindow.frameElement);
    LFrameRect := LCurrentFrame.getBoundingClientRect;
    LScaleX := LFrameRect.width / LCurrentFrame.offsetWidth;
    LScaleY := LFrameRect.height / LCurrentFrame.offsetHeight;
    { Both fixture frames have the explicit 3px border and 8px padding below. }
    LLeft := LFrameRect.left + (11 + LLeft) * LScaleX;
    LTop := LFrameRect.top + (11 + LTop) * LScaleY;
    LRight := LFrameRect.left + (11 + LRight) * LScaleX;
    LBottom := LFrameRect.top + (11 + LBottom) * LScaleY;
    LWindow := LCurrentFrame.ownerDocument.defaultView;
  end;
  Check((LRect.width > 0) and (LRect.height > 0), AReason + ': nonempty');

  if AClipX then
  begin
    Check((LLeft >= LClipRect.left - 0.5) and (LRight <= LClipRect.right + 0.5),
      AReason + ': horizontally reachable');
  end
  else
  begin
    Check(LRight > LClipRect.right, AReason + ': visible horizontal overflow stays available');
  end;

  if AClipY then
  begin
    Check((LTop >= LClipRect.top - 0.5) and (LBottom <= LClipRect.bottom + 0.5),
      AReason + ': vertically reachable');
  end
  else
  begin
    Check(LBottom > LClipRect.bottom, AReason + ': visible vertical overflow stays available');
  end;
  Check(LPopup.scrollHeight > LPopup.clientHeight,
    AReason + ': oversized content scrolls internally');
end;

procedure ParentTick;
var
  LChild: TJSDocument;
  LNested: TJSHTMLIFrameElement;
begin
  try
    Inc(GWait);
    LChild := GFrame.contentDocument;

    if (LChild = nil) or (LChild.body = nil) or
      (LChild.body.getAttribute('data-ready') <> 'true') then
    begin
      Check(GWait < 40, 'Child becomes ready within the fixture deadline');
      Exit;
    end;

    if LChild.body.getAttribute('data-result') = 'failed' then
    begin
      raise Exception.Create(LChild.body.getAttribute('data-error'));
    end;
    case GPhase of
      0:
        begin
          Fits(GFrame, GClip, 'Negative offset with frame border and padding');
          GClip.style.setProperty('overflow', 'auto');
          GFrame.style.setProperty('left', '24px');
          GFrame.style.setProperty('top', '24px');
          GClip.scrollLeft := 48;
          GClip.scrollTop := 68;
        end;
      1:
        begin
          Fits(GFrame, GClip, 'Independent ancestor scroll');
          GFrame.style.setProperty('transform-origin', '0 0');
          GFrame.style.setProperty('transform', 'scale(.75)');
        end;
      2:
        begin
          Fits(GFrame, GClip, 'Scaled browsing context');
          GFrame.style.setProperty('transform', 'scale(.75,.6)');
          GClip.style.setProperty('height', '220px');
        end;
      3:
        begin
          Fits(GFrame, GClip, 'Nonuniform scale and ancestor allocation shrink');
          GClip.style.setProperty('overflow-x', 'clip');
          GClip.style.setProperty('overflow-y', 'visible');
          GClip.style.setProperty('height', '100px');
        end;
      4:
        begin
          Fits(GFrame, GClip, 'Horizontal-only ancestor clip', True, False);
          GClip.style.setProperty('overflow-x', 'visible');
          GClip.style.setProperty('overflow-y', 'clip');
          GClip.style.setProperty('height', '280px');
          GClip.style.setProperty('width', '120px');
        end;
      5:
        begin
          Fits(GFrame, GClip, 'Vertical-only ancestor clip', False, True);
          GClip.style.setProperty('overflow', 'hidden');
          GClip.style.setProperty('height', '220px');
          GClip.style.setProperty('width', 'calc(100% - 48px)');
        end;
      6:
        begin
          Fits(GFrame, GClip, 'Restored allocation');
          LChild.body.setAttribute('data-command', 'hide');
        end;
      7:
        begin
          Check(LChild.body.getAttribute('data-open') = 'False', 'Hidden anchor dismisses');
          Check(LChild.body.getAttribute('data-reason') = IntToStr(Ord(nprAnchorUnavailable)),
            'Hidden anchor reports its typed reason');
          LChild.body.setAttribute('data-command', 'open');
        end;
      8:
        begin
          Fits(GFrame, GClip, 'Reopening samples current parent allocation');
          GFrame.style.setProperty('left', '-1000px');
        end;
      9:
        begin
          Check(LChild.body.getAttribute('data-open') = 'False', 'Wholly clipped anchor dismisses');
          GFrame.style.setProperty('left', '24px');
          LChild.body.setAttribute('data-command', 'reopen');
        end;
      10:
        begin
          Fits(GFrame, GClip, 'Restored frame anchor');
          LChild.body.setAttribute('data-command', 'clip-local');
        end;
      11:
        begin
          Check(LChild.body.getAttribute('data-open') = 'False',
            'Anchor clipped by its own scroll container dismisses');
          LChild.body.setAttribute('data-command', 'release');
        end;
      12:
        begin
          Check(LChild.querySelectorAll('.nyx-popover').length = 0, 'Released host retires');
          GClip.style.setProperty('height', '280px');
        end;
      13:
        begin
          Check(LChild.querySelectorAll('.nyx-popover').length = 0,
            'Parent movement cannot revive retired presenter');
          GFrame.src := window.location.pathname + '?middle';
          GWait := 0;
        end;
      14:
        begin
          LNested := TJSHTMLIFrameElement(LChild.querySelector('iframe'));

          if (LNested = nil) or (LNested.contentDocument.body = nil) or
            (LNested.contentDocument.body.getAttribute('data-open') <> 'True') then
          begin
            Check(GWait < 40, 'Nested child becomes ready');
            Exit;
          end;
          Fits(LNested, TJSHTMLElement(LChild.querySelector('#fixture-clip')),
            'Nested browsing context');
          Fits(GFrame, GClip, 'Outer ancestor of nested browsing context');
          Check(LNested.contentDocument.body.getAttribute('data-open') = 'True',
            'Nested popup remains open');
          window.clearInterval(GTimer);
          document.body.setAttribute('data-checks', IntToStr(GChecks));
          document.body.setAttribute('data-result', 'passed');
        end;
    end;
    Inc(GPhase);
  except
    on LError: Exception do
    begin
      Fail(LError.Message);
    end;
  end;
end;

procedure Parent(AMiddle: Boolean);
begin
  TJSHTMLElement(document.body).style.cssText := 'margin:0;font:16px sans-serif;background:#eceaf7;';
  GClip := TJSHTMLElement(document.createElement('div'));
  GClip.id := 'fixture-clip';
  GClip.style.cssText := 'position:absolute;left:24px;top:32px;width:calc(100% - 48px);' +
    'max-width:600px;height:280px;overflow:hidden;background:white;';
  document.body.appendChild(GClip);
  GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
  GFrame.title := 'Embedded popover consumer';
  GFrame.style.cssText := 'position:absolute;left:-24px;top:-44px;width:640px;' +
    'height:620px;border:3px solid #6654ce;padding:8px;box-sizing:content-box;';
  GFrame.src := window.location.pathname + '?child';
  GClip.appendChild(GFrame);

  if AMiddle then
  begin
    document.body.setAttribute('data-ready', 'true');
  end
  else
  begin
    GTimer := window.setInterval(@ParentTick, 300);
  end;
end;

begin
  try

    if window.location.search = '?child' then
    begin
      Child;
    end
    else
    begin
      Parent(window.location.search = '?middle');
    end;
  except
    on LError: Exception do
    begin
      Fail(LError.Message);
    end;
  end;
end.
