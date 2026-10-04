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

program nyx_studio_layout_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  Web,
  nyx.text,
  nyx.render.browser,
  nyx.studio.browser;

var
  LStudio: TNyxStudio;
  LCount: Integer;
  LCompact: Boolean;
  LSource: TNyxText;
  LInput: TJSHTMLInputElement;
  LWorkspace: TJSHTMLElement;
  LCanvas: TJSHTMLElement;
  LCenter: TJSHTMLElement;
  LFrame: TJSHTMLIframeElement;
  LFrameSource: TNyxText;
  LFrameMemo: TJSHTMLTextAreaElement;
  LFrameMemoID: TNyxText;
  LFrameMemoScroll: NativeInt;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if Result = nil then
  begin
    raise Exception.Create('Missing layout control: ' + AID);
  end;
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('FAIL Studio layout: ' + AMessage);
  end;
  Inc(LCount);
end;

procedure CheckBounds;
begin
  { Measure actual target geometry. A screenshot's requested size alone cannot
    prove layout because headless browsers can impose a wider minimum viewport. }
  LWorkspace := Find('studio-workspace');
  LCenter := Find('studio-center');
  LCanvas := Find('studio-canvas');
  Check(document.documentElement.scrollWidth <= window.innerWidth + 1,
    'editor has no document-level horizontal overflow');
  Check((LCanvas.getBoundingClientRect.width > 250) and
    (Find('studio-canvas-wrap').getBoundingClientRect.height > 100),
    'canvas retains useful width and height with the optional source view');

  if LCompact then
  begin
    Check(Abs(LCenter.getBoundingClientRect.width - LWorkspace.getBoundingClientRect.width) < 2,
      'compact design receives the full workspace width');
  end
  else
  begin
    Check((Find('studio-left').getBoundingClientRect.right <=
      LCenter.getBoundingClientRect.left + 1) and
      (LCenter.getBoundingClientRect.right <=
      Find('studio-right').getBoundingClientRect.left + 1),
      'desktop side panels and design do not overlap');
  end;
end;

procedure CheckCanvasEditing;
var
  LMemo: TJSHTMLTextAreaElement;
  LScroll: TJSHTMLElement;
  LID: TNyxText;
  LTop: NativeInt;
  LClick: TJSMouseEvent;
  LOptions: TJSEventInit;
  LFields: TJSNodeList;
begin
  { Reproduce selection/editing inside a catalog compound at a nonzero canvas
    scroll position. Both wide and exact-phone hosts run the same real controls. }

  if LCompact then
  begin
    Find('action-panel-project').click;
  end;
  Find('palette-comment-thread').click;
  LFields := Find('studio-canvas').querySelectorAll('textarea');
  LMemo := TJSHTMLTextAreaElement(LFields[LFields.length - 1]);
  LID := TJSHTMLElement(LMemo.closest('[data-node]')).getAttribute('data-node');
  LScroll := Find('studio-canvas-wrap');
  LScroll.scrollTop := LScroll.scrollHeight;
  LMemo.focus;
  LTop := LScroll.scrollTop;
  Check(LTop > 0, 'memo fixture exercises an actually scrolled canvas');
  LMemo.value := 'Pending reply / 🌙';
  LMemo.selectionStart := 2;
  LMemo.selectionEnd := 5;
  LOptions.bubbles := True;
  LOptions.cancelable := True;
  LOptions.composed := False;
  LOptions.scoped := False;
  LClick := TJSMouseEvent.new('click', LOptions);
  LMemo.dispatchEvent(LClick);
  Check(not LClick.defaultPrevented, 'design selection permits native memo interaction');
  Check((Find(LID).querySelector('textarea') = LMemo) and
    (document.activeElement = LMemo) and (LMemo.selectionStart = 2) and
    (LMemo.selectionEnd = 5) and (LMemo.value = 'Pending reply / 🌙'),
    'selecting a memo retains its control, focused draft and text selection');
  Check(Find('studio-canvas-wrap').scrollTop = LTop,
    'selecting a memo retains canvas scrolling');
  LMemo.dispatchEvent(TJSEvent.new('change'));
  Check((Find(LID).querySelector('textarea') = LMemo) and
    (document.activeElement = LMemo) and (Find('studio-canvas-wrap').scrollTop = LTop),
    'committing a canvas edit retains field identity, focus and scrolling');
  Find('action-code').click;
  Check(Pos('Pending reply / 🌙', TJSHTMLTextAreaElement(Find('studio-code')).value) > 0,
    'memo editing updates generated Pascal through the design command');
  Find('action-undo').click;
  Check(Pos('Pending reply / 🌙', TJSHTMLTextAreaElement(Find('studio-code')).value) = 0,
    'undo removes the memo edit from generated code');
  Find('action-redo').click;
  Check(Pos('Pending reply / 🌙', TJSHTMLTextAreaElement(Find('studio-code')).value) > 0,
    'redo restores the canvas memo edit');
  Find('action-code').click;
end;

procedure RunStandalone;
begin
  LStudio := TNyxStudio.Create;
  try
    { This fixture never reads or writes the user's recovery/project/output
      choice. The controller remains alive for the displayed callback lifetime. }
    LStudio.Run(False);
    LCompact := window.innerWidth <= 960;
    Check(document.querySelector('[data-node=studio-code]') = nil,
      'source split starts only when requested');
    Check((document.querySelector('[data-node=studio-panelbar]') <> nil) = LCompact,
      'panel navigation follows the real viewport boundary');
    CheckBounds;
    Find('action-code').click;
    LSource := TJSHTMLTextAreaElement(Find('studio-code')).value;
    Check(Pos('BuildNyxDocument', LSource) > 0, 'optional source uses the public Nyx editor');
    CheckBounds;
    Find('action-code').click;

    if LCompact then
    begin
      Find('action-panel-project').click;
      Check((document.querySelector('[data-node=studio-center]') = nil) and
        (Abs(Find('studio-left').getBoundingClientRect.width -
          Find('studio-workspace').getBoundingClientRect.width) < 2),
        'project panel has full width instead of squeezing the canvas');
    end;
    LInput := TJSHTMLInputElement(Find('project-title').querySelector('input'));
    LInput.value := 'Layout review / 🌙';
    LInput.dispatchEvent(TJSEvent.new('change'));
    Find('palette-card').click;
    Check((document.querySelector('[data-node=studio-center]') <> nil) and
      (Pos('Layout review / 🌙', document.title) > 0),
      'project edits and palette insertion return to the design without losing content');

    if LCompact then
    begin
      Find('action-panel-inspector').click;
      Check((document.querySelector('[data-node=studio-center]') = nil) and
        (Find('selected-label').textContent = 'card / card-1'),
        'inspector follows the selected component in a full width panel');
    end;
    LInput := TJSHTMLInputElement(Find('inspector-padding').querySelector('input'));
    LInput.value := '24';
    LInput.dispatchEvent(TJSEvent.new('change'));
    Find('action-code').click;
    LSource := TJSHTMLTextAreaElement(Find('studio-code')).value;
    Check((Pos('Layout review / 🌙', LSource) > 0) and
      (Pos('.Padding(24)', LSource) > 0),
      'panel switching retains edited design and generated source');
    CheckBounds;
    Find('action-code').click;
    Find('action-outputs').click;
    Check((Find('studio-outputs') <> nil) and (Find('output-none') <> nil),
      'output configuration remains available from the compact or desktop header');
    Find('action-outputs').click;
    Find('action-phone').click;
    Check(Abs(Find('studio-canvas').getBoundingClientRect.width - 390) < 2,
      'phone emulation retains its requested design width inside the scroll host');
    Check(document.documentElement.scrollWidth <= window.innerWidth + 1,
      'intentional preview scrolling does not widen the editor document');
    CheckCanvasEditing;
    document.body.setAttribute('data-nyx-studio-layout', 'passed');
    document.body.setAttribute('data-nyx-studio-layout-checks', IntToStr(LCount));
    document.body.setAttribute('data-nyx-studio-layout-width', IntToStr(window.innerWidth));
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-studio-layout', 'failed');
      document.body.setAttribute('data-nyx-studio-layout-error', LException.Message);
    end;
  end;
end;

function FrameFind(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(LFrame.contentDocument.querySelector('[data-node="' + AID + '"]'));

  if Result = nil then
  begin
    raise Exception.Create('Missing resized layout control: ' + AID);
  end;
end;

procedure ResizeFailure(const AMessage: TNyxText);
begin
  document.body.setAttribute('data-nyx-studio-resize', 'failed');
  document.body.setAttribute('data-nyx-studio-resize-error', AMessage);
end;

procedure CheckCanvasPhoneResize;
var
  LScroll: TJSHTMLElement;
begin
  try
    Check(LFrame.contentWindow.innerWidth = 390, 'focused canvas returns to real phone width');
    Check(FrameFind(LFrameMemoID).querySelector('textarea') = LFrameMemo,
      'canvas memo identity survives wide-to-compact resize');
    Check((LFrame.contentDocument.activeElement = LFrameMemo) and
      (LFrameMemo.value = 'Pending resize draft / 🌙') and
      (LFrameMemo.selectionStart = 1) and (LFrameMemo.selectionEnd = 4),
      'compact canvas retains focus, draft and text selection');
    LScroll := FrameFind('studio-canvas-wrap');
    Check(LScroll.scrollTop = LFrameMemoScroll, 'compact resize retains canvas scroll');
    document.body.setAttribute('data-nyx-studio-resize', 'passed');
    document.body.setAttribute('data-nyx-studio-resize-checks', IntToStr(LCount));
    document.body.setAttribute('data-nyx-studio-frame-width', '390');
  except
    on LException: Exception do
    begin
      ResizeFailure(LException.Message);
    end;
  end;
end;

procedure CheckCanvasWideResize;
var
  LScroll: TJSHTMLElement;
begin
  try
    Check(LFrame.contentWindow.innerWidth = 1100, 'focused canvas reaches real desktop width');
    Check(FrameFind(LFrameMemoID).querySelector('textarea') = LFrameMemo,
      'canvas memo identity survives compact-to-wide resize');
    Check((LFrame.contentDocument.activeElement = LFrameMemo) and
      (LFrameMemo.value = 'Pending resize draft / 🌙') and
      (LFrameMemo.selectionStart = 1) and (LFrameMemo.selectionEnd = 4),
      'wide canvas retains focus, draft and text selection');
    LScroll := FrameFind('studio-canvas-wrap');
    Check(LScroll.scrollTop = LFrameMemoScroll, 'wide resize retains canvas scroll');
    LFrame.style.setProperty('width', '390px');
    window.setTimeout(@CheckCanvasPhoneResize, 200);
  except
    on LException: Exception do
    begin
      ResizeFailure(LException.Message);
    end;
  end;
end;

procedure CheckPhoneResize;
begin
  try
    Check(LFrame.contentWindow.innerWidth = 390, 'real phone frame is exactly 390 pixels');
    Check(LFrame.contentDocument.documentElement.scrollWidth <= 390,
      'real phone editor remains confined to its viewport');
    Check(TJSHTMLTextAreaElement(FrameFind('studio-code')).value = LFrameSource,
      'same-mode phone resize retains the generated source');
    Check((FrameFind('studio-code').getBoundingClientRect.height >= 100) and
      (FrameFind('studio-canvas-wrap').getBoundingClientRect.height > 100),
      'real phone split gives both code and canvas useful space');
    LFrameMemo := TJSHTMLTextAreaElement(FrameFind('studio-canvas').querySelector('textarea'));
    LFrameMemoID := TJSHTMLElement(LFrameMemo.closest('[data-node]')).getAttribute('data-node');
    LFrameMemo.value := 'Pending resize draft / 🌙';
    LFrameMemo.selectionStart := 1;
    LFrameMemo.selectionEnd := 4;
    NyxFocusWithoutScroll(LFrameMemo);
    FrameFind('studio-canvas-wrap').scrollTop := 150;
    LFrameMemoScroll := FrameFind('studio-canvas-wrap').scrollTop;
    LFrame.style.setProperty('width', '1100px');
    window.setTimeout(@CheckCanvasWideResize, 200);
  except
    on LException: Exception do
    begin
      ResizeFailure(LException.Message);
    end;
  end;
end;

procedure CheckTabletResize;
begin
  try
    Check((LFrame.contentWindow.innerWidth = 800) and
      (FrameFind('studio-shell').getAttribute('data-nyx-studio-compact') = 'true'),
      'real desktop-to-tablet resize enters compact mode');
    Check(TJSHTMLTextAreaElement(FrameFind('studio-code')).value = LFrameSource,
      'mode change retains the complete edited Pascal');
    Check(Abs(FrameFind('studio-center').getBoundingClientRect.width - 800) < 2,
      'tablet canvas receives its full workspace');
    LFrame.style.setProperty('width', '390px');
    window.setTimeout(@CheckPhoneResize, 200);
  except
    on LException: Exception do
    begin
      ResizeFailure(LException.Message);
    end;
  end;
end;

procedure CheckWideResize;
var
  LField: TJSHTMLInputElement;
begin
  try
    Check((LFrame.contentWindow.innerWidth = 1100) and
      (FrameFind('studio-shell').getAttribute('data-nyx-studio-compact') = 'false'),
      'real phone-to-desktop resize restores the three panel workspace');
    LField := TJSHTMLInputElement(FrameFind('project-title').querySelector('input'));
    Check((LField.value = 'Pending layout draft / 🌙') and
      (LFrame.contentDocument.activeElement = LField),
      'resize preserves the focused uncommitted field draft');
    Check(Pos('Pending layout draft', LFrame.contentDocument.title) = 0,
      'restoring a draft does not commit it to the project');
    LField.dispatchEvent(TJSEvent.new('change'));
    FrameFind('action-code').click;
    LFrameSource := TJSHTMLTextAreaElement(FrameFind('studio-code')).value;
    Check(Pos('Pending layout draft / 🌙', LFrameSource) > 0,
      'normal change commits the restored draft through the session');
    LFrame.style.setProperty('width', '800px');
    window.setTimeout(@CheckTabletResize, 200);
  except
    on LException: Exception do
    begin
      ResizeFailure(LException.Message);
    end;
  end;
end;

function FrameLoaded(AEvent: TJSEvent): Boolean;
var
  LField: TJSHTMLInputElement;
begin
  Result := True;
  try
    Check(LFrame.contentDocument.body.getAttribute('data-nyx-studio-layout') = 'passed',
      'all standalone journeys pass inside a real 390 pixel viewport');
    Check(LFrame.contentWindow.innerWidth = 390, 'iframe removes headless minimum-width ambiguity');
    FrameFind('action-panel-project').click;
    LField := TJSHTMLInputElement(FrameFind('project-title').querySelector('input'));
    LField.value := 'Pending layout draft / 🌙';
    LField.focus;
    LFrame.style.setProperty('width', '1100px');
    window.setTimeout(@CheckWideResize, 200);
  except
    on LException: Exception do
    begin
      ResizeFailure(LException.Message);
    end;
  end;
end;

begin

  if window.location.search = '?host=1' then
  begin
    { A same-origin iframe supplies exact real viewports and native resize events.
      Its child runs the same Pascal journey, with user recovery disabled. This
      validates 390 → 1100 → 800 → 390 without changing read-only browser globals. }
    TJSHTMLElement(document.body).style.setProperty('margin', '0');
    TJSHTMLElement(document.body).style.setProperty('width', '390px');
    TJSHTMLElement(document.body).style.setProperty('overflow', 'hidden');
    LFrame := TJSHTMLIframeElement(document.createElement('iframe'));
    LFrame.style.setProperty('width', '390px');
    LFrame.style.setProperty('height', '900px');
    LFrame.style.setProperty('display', 'block');
    LFrame.style.setProperty('border', '0');
    LFrame.addEventListener('load', @FrameLoaded);
    LFrame.src := 'studio-layout.html';
    document.body.appendChild(LFrame);
  end
  else
  begin
    RunStandalone;
  end;
end.
