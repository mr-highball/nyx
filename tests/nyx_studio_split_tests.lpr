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
program nyx_studio_split_tests;

{$mode delphi}{$H+}
{$codepage utf8}
{$modeswitch externalclass}

uses
  SysUtils, JS, Web, nyx.text, nyx.types, nyx.studio.browser,
  nyx.test.keyboard.browser;

type
  { Synthetic touch events exercise the public DOM handlers without claiming
    trusted pointer capture or physical-device coverage. No host object is owned. }
  TPointer = class external name 'PointerEvent' (TJSPointerEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;

  { Ordinary Studio commands publish after their originating callback returns.
    Observe each resulting presentation before the next input, within the same
    bounded observation budget used by the compact host. Rendering time remains
    part of the separate source-workspace wall-clock performance regression. }
  TSplitStage = (ssOpenCode, ssHideCode, ssShowCode, ssOpenOutputs,
    ssCloseOutputs, ssComplete);

var
  GStudio: TNyxStudio;
  GChecks: Integer;
  GFrame: TJSHTMLIFrameElement;
  GPolls: Integer;
  GSource: TNyxText;
  GPercent: TNyxText;
  GStage: TSplitStage;
  GJourneyPolls: Integer;

procedure ObserveJourney; forward;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Studio split: ' + AReason);
  end;
  Inc(GChecks);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if Result = nil then
  begin
    raise Exception.Create('Missing Studio control: ' + AID);
  end;
end;

function Grip: TJSHTMLElement;
begin
  Result := TJSHTMLElement(Find('studio-split').querySelector('.nyx-split-divider'));
end;

{ Compact chrome exposes hidden header commands through the public Nyx menu.
  Exercise that ordinary operator route; clicking a hidden face correctly does
  nothing. These are control identities at the host-input boundary, not design
  composition or a substitute for semantic MCP. }
procedure WorkspaceAction(const AID, ABranch, ACommand: TNyxText);
var
  LFace: TJSHTMLElement;
begin
  LFace := Find(AID);

  if LFace.getBoundingClientRect.height > 0 then
  begin
    LFace.click;
    Exit;
  end;
  Find('action-actions').click;
  Find('studio-menu-' + ABranch).click;
  Find('studio-menu-' + ACommand).click;
end;

procedure Pointer(const AType: String; AX, AY: Double; AID: Integer = 1);
var
  LOptions: TJSObject;
begin
  LOptions := TJSObject.new;
  LOptions['clientX'] := AX;
  LOptions['clientY'] := AY;
  LOptions['pointerId'] := AID;
  LOptions['pointerType'] := 'touch';
  LOptions['isPrimary'] := True;
  LOptions['button'] := 0;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  Grip.dispatchEvent(TPointer.new(AType, LOptions));
end;

procedure Failure(const AMessage: TNyxText);
begin
  document.body.setAttribute('data-studio-split', 'failed');
  document.body.setAttribute('data-studio-split-host', 'failed');
  document.body.setAttribute('data-studio-split-error', AMessage);
  document.body.setAttribute('data-event-error', AMessage);
  document.body.appendChild(document.createTextNode('FAIL ' + AMessage));
end;

procedure Journey;
var
  LCode: TJSHTMLTextAreaElement;
  LHeight: Double;
  LX: Double;
  LY: Double;
  LScroll: Integer;
  LCanvasScroll: Integer;
  LPercent: TNyxText;
begin
  try
    LCode := TJSHTMLTextAreaElement(Find('studio-code'));
    Check(Grip.getBoundingClientRect.height = 44, 'visible touch-friendly grip');
    Check(Grip.getAttribute('role') = 'separator', 'semantic separator role');
    Check(Grip.getAttribute('data-requested-position') = '65', 'portable default requested proportion');
    GSource := LCode.value + #10 + '{ Pending draft / 🌙漢字 }';
    LCode.value := GSource;
    LCode.dispatchEvent(TJSEvent.new('change'));
    LCode.focus;
    LCode.selectionStart := 13;
    LCode.selectionEnd := 71;
    LCode.scrollTop := 180;
    LScroll := LCode.scrollTop;
    Find('studio-canvas-wrap').scrollTop := 100;
    LCanvasScroll := Find('studio-canvas-wrap').scrollTop;
    LHeight := LCode.getBoundingClientRect.height;
    LX := Grip.getBoundingClientRect.left + 14;
    LY := Grip.getBoundingClientRect.top + 14;
    Pointer('pointerdown', LX, LY);
    Check(Grip.getAttribute('data-requested-position') = '65', 'touch does not snap initially');
    Pointer('pointermove', LX, LY - 70, 2);
    Check(Grip.getAttribute('data-requested-position') = '65', 'unrelated touch cannot take over');
    Pointer('pointermove', LX, LY - 70);
    Pointer('pointerup', LX, LY - 70);
    Check(Find('studio-code') = LCode, 'drag never remounts source control');
    Check((LCode.value = GSource) and (LCode.selectionStart = 13) and
      (LCode.selectionEnd = 71), 'exact Unicode draft and caret retained');
    Check(document.activeElement = LCode, 'drag preserves source focus');
    Check(LCode.scrollTop = LScroll, 'source scroll retained');
    Check(Find('studio-canvas-wrap').scrollTop = LCanvasScroll, 'canvas scroll retained');
    Check(LCode.getBoundingClientRect.height > LHeight, 'drag reveals more Pascal');
    LPercent := Grip.getAttribute('aria-valuenow');
    Pointer('pointerdown', LX, LY);
    Pointer('pointermove', LX, LY + 100);
    Pointer('pointercancel', LX, LY + 100);
    Check(Grip.getAttribute('aria-valuenow') = LPercent, 'cancellation restores geometry');
    Grip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Home'));
    Check(Grip.getAttribute('data-requested-position') = '10', 'minimum request keeps canvas recoverable');
    LPercent := Grip.getAttribute('aria-valuenow');
    Grip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'ArrowDown', [nmShift]));
    Check(StrToInt(Grip.getAttribute('aria-valuenow')) > StrToInt(LPercent),
      'keyboard resizing starts at the physical constrained edge');
    Grip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'End'));
    Check(Grip.getAttribute('data-requested-position') = '90', 'maximum request keeps divider recoverable');
    Grip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Home'));
    Grip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'ArrowDown', [nmShift]));
    Grip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'ArrowDown', [nmShift]));
    GPercent := Grip.getAttribute('data-requested-position');
    GStage := ssHideCode;
    WorkspaceAction('action-code', 'view', 'code');
  except
    on LException: Exception do
    begin
      Failure(LException.Message);
    end;
  end;
end;

{ Poll only observable control publication; never force a renderer refresh or
  inspect Studio's private session. Original assertions and deadlines remain. }
procedure ObserveJourney;
var
  LCode: TJSHTMLElement;
  LOutputs: TJSHTMLElement;
begin
  try

    Inc(GJourneyPolls);

    if GJourneyPolls > 100 then
    begin
      raise Exception.Create('Studio split publication deadline at stage ' +
        IntToStr(Ord(GStage)));
    end;
    LCode := TJSHTMLElement(document.querySelector('[data-node="studio-code"]'));
    LOutputs := TJSHTMLElement(document.querySelector('[data-node="studio-outputs"]'));
    case GStage of
      ssOpenCode:
        begin

          if (LCode <> nil) and (LCode.getBoundingClientRect.height > 0) then
          begin
            Journey;
          end;
        end;
      ssHideCode:
        begin

          if (LCode = nil) or (LCode.getBoundingClientRect.height = 0) then
          begin
            Check(document.querySelector('.nyx-split-divider') = nil,
              'hidden code releases divider');
            GStage := ssShowCode;
            WorkspaceAction('action-code', 'view', 'code');
          end;
        end;
      ssShowCode:
        begin

          if (LCode <> nil) and (LCode.getBoundingClientRect.height > 0) then
          begin
            Check(Grip.getAttribute('data-requested-position') = GPercent,
              'show/hide retains chosen proportion');
            Check(TJSHTMLTextAreaElement(LCode).value = GSource,
              'show/hide retains pending source draft');
            GStage := ssOpenOutputs;
            WorkspaceAction('action-outputs', 'project', 'outputs');
          end;
        end;
      ssOpenOutputs:
        begin

          if (LOutputs <> nil) and (LOutputs.getBoundingClientRect.height > 0) then
          begin
            Check(Grip.getAttribute('data-requested-position') = GPercent,
              'output section retains requested proportion');
            Check(TJSHTMLTextAreaElement(Find('studio-code')).getBoundingClientRect.height > 40,
              'source remains readable beside optional output section');
            GStage := ssCloseOutputs;
            WorkspaceAction('action-outputs', 'project', 'outputs');
          end;
        end;
      ssCloseOutputs:
        begin

          if (LOutputs = nil) or (LOutputs.getBoundingClientRect.height = 0) then
          begin
            GStage := ssComplete;
            document.body.setAttribute('data-studio-split', 'passed');
            document.body.setAttribute('data-studio-split-checks', IntToStr(GChecks));
            document.body.setAttribute('data-studio-split-width', IntToStr(window.innerWidth));
            document.body.setAttribute('data-studio-split-position', GPercent);
          end;
        end;
      ssComplete:
        begin
          Exit;
        end;
    end;

    if (GStage <> ssComplete) and
      (document.body.getAttribute('data-studio-split') <> 'failed') then
    begin
      window.setTimeout(@ObserveJourney, 100);
    end;
  except
    on LException: Exception do
    begin
      Failure(LException.Message);
    end;
  end;
end;

procedure StartJourney;
begin
  GJourneyPolls := 0;
  GStage := ssOpenCode;
  WorkspaceAction('action-code', 'view', 'code');
  window.setTimeout(@ObserveJourney, 100);
end;

function FrameFind(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(GFrame.contentDocument.querySelector('[data-node="' + AID + '"]'));
end;

procedure Narrow;
var
  LGrip: TJSHTMLElement;
begin
  try
    LGrip := TJSHTMLElement(FrameFind('studio-split').querySelector('.nyx-split-divider'));
    Check(GFrame.contentWindow.innerWidth = 390, 'returns to actual narrow viewport');
    Check(LGrip.getAttribute('data-requested-position') =
      GFrame.contentDocument.body.getAttribute('data-studio-split-position'),
      'desktop-to-phone retains requested proportion');
    Check(TJSHTMLTextAreaElement(FrameFind('studio-code')).value =
      GFrame.contentDocument.body.getAttribute('data-studio-split-source'),
      'round trip retains exact draft');
    document.body.setAttribute('data-studio-split-host', 'passed');
    document.body.setAttribute('data-studio-split-host-checks', IntToStr(GChecks));
  except
    on LException: Exception do
    begin
      Failure(LException.Message);
    end;
  end;
end;

procedure Wide;
var
  LGrip: TJSHTMLElement;
begin
  try
    LGrip := TJSHTMLElement(FrameFind('studio-split').querySelector('.nyx-split-divider'));
    Check(GFrame.contentWindow.innerWidth = 1100, 'actual wide viewport');
    Check(LGrip.getAttribute('data-requested-position') =
      GFrame.contentDocument.body.getAttribute('data-studio-split-position'),
      'phone-to-desktop retains requested proportion');
    Check(TJSHTMLTextAreaElement(FrameFind('studio-code')).value =
      GFrame.contentDocument.body.getAttribute('data-studio-split-source'),
      'viewport transition retains exact draft');
    GFrame.style.setProperty('height', '620px');
    Check(LGrip.getAttribute('data-requested-position') =
      GFrame.contentDocument.body.getAttribute('data-studio-split-position'),
      'available-height change retains requested proportion');
    GFrame.style.setProperty('width', '390px');
    GFrame.style.setProperty('height', '760px');
    window.setTimeout(@Narrow, 200);
  except
    on LException: Exception do
    begin
      Failure(LException.Message);
    end;
  end;
end;

procedure Observe;
begin
  Inc(GPolls);

  if (GFrame.contentDocument <> nil) and
    (GFrame.contentDocument.body.getAttribute('data-studio-split') = 'failed') then
  begin
    Failure(GFrame.contentDocument.body.getAttribute('data-studio-split-error'));
    Exit;
  end;

  if (GFrame.contentDocument = nil) or
    (GFrame.contentDocument.body.getAttribute('data-studio-split') <> 'passed') then
  begin

    if GPolls > 100 then
    begin
      Failure('compact child did not pass its journey');
      Exit;
    end;
    window.setTimeout(@Observe, 100);
    Exit;
  end;
  try
    Check(GFrame.contentWindow.innerWidth = 390, 'exact physical iframe width');
    Check(GFrame.contentDocument.body.getAttribute('data-studio-split-checks') = '20',
      'compact executes all interaction checks');
    GFrame.contentDocument.body.setAttribute('data-studio-split-source',
      TJSHTMLTextAreaElement(FrameFind('studio-code')).value);
    GFrame.style.setProperty('width', '1100px');
    window.setTimeout(@Wide, 200);
  except
    on LException: Exception do
    begin
      Failure(LException.Message);
    end;
  end;
end;

begin

  if window.location.search = '?host=1' then
  begin
    GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
    GFrame.style.cssText := 'width:390px;height:900px;border:0;display:block;';
    TJSHTMLElement(document.body).style.setProperty('margin', '0');
    document.body.appendChild(GFrame);
    { Keep the current host's deployment path while dropping only host mode.
      Qualification hosts may use a distinct filename beside the real HTTP API. }
    GFrame.src := window.location.pathname;
    window.setTimeout(@Observe, 100);
  end
  else
  begin
    GStudio := TNyxStudio.Create;
    GStudio.Run(False);
    window.setTimeout(@StartJourney, 250);
  end;
end.
