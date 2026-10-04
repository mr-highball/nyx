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
program nyx_gesture_studio_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils, Web, JS, nyx.text, nyx.studio.browser, nyx.studio.inspector;

var
  GStudio: TNyxStudio;
  GChecks: Integer;
  GFrame: TJSHTMLIFrameElement;
  GPolls: Integer;
  GCompact: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Gesture Studio: ' + AReason);
  end;
  Inc(GChecks);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if (Result = nil) and GCompact then
  begin
    { Phone panels expose the same controls, one workspace at a time. Navigate
      exactly as the user does before addressing the corresponding Nyx face. }

    if (Pos('event-', AID) = 1) or (Pos('inspector-', AID) = 1) or
      (AID = 'studio-right') then
    begin
      TJSHTMLElement(document.querySelector('[data-node=action-panel-inspector]')).click;
    end
    else
    begin
      TJSHTMLElement(document.querySelector('[data-node=action-panel-design]')).click;
    end;
    Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
  end;

  if Result = nil then
  begin
    raise Exception.Create('Missing gesture Studio control: ' + AID);
  end;
end;

procedure Click(const AID: TNyxText);
begin
  Find(AID).click;
end;

function Code: TJSHTMLTextAreaElement;
var
  LElement: TJSHTMLElement;
begin
  LElement := Find('studio-code');

  if LElement is TJSHTMLTextAreaElement then
  begin
    Result := TJSHTMLTextAreaElement(LElement);
  end
  else
  begin
    Result := TJSHTMLTextAreaElement(LElement.querySelector('textarea'));
  end;
end;

procedure Run;
var
  LIndex: Integer;
  LKey: TNyxText;
  LBefore: TNyxText;
  LSource: TNyxText;
  LPolicy: TJSHTMLSelectElement;
  LKeys: array[0..9] of TNyxText;
begin
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  GCompact := window.innerWidth < 900;
  Click('action-code');

  if GCompact then
  begin
    Click('action-panel-project');
  end;
  Click('palette-memo');

  if GCompact then
  begin
    Click('action-panel-inspector');
  end;
  { The Nyx inspector consumes the same strongly typed configuration metadata;
    its wire-facing choice controls produce crafted fluent Pascal calls. }
  Click(NyxInspectorPropertiesID);
  LPolicy := TJSHTMLSelectElement(Find('inspector-drag-source').querySelector('select'));
  LPolicy.value := 'true';
  LPolicy.dispatchEvent(TJSEvent.new('change'));
  Check(Pos('.DragSource(True)', Code.value) > 0, 'source setting emits a typed Boolean call');
  LPolicy := TJSHTMLSelectElement(Find('inspector-drop-target').querySelector('select'));
  LPolicy.value := 'true';
  LPolicy.dispatchEvent(TJSEvent.new('change'));
  Check(Pos('.DropTarget(True)', Code.value) > 0, 'target setting emits a typed Boolean call');
  LPolicy := TJSHTMLSelectElement(Find('inspector-touch-behavior').querySelector('select'));
  LPolicy.value := 'none';
  LPolicy.dispatchEvent(TJSEvent.new('change'));
  Check(Pos('.TouchBehavior(ntbNone)', Code.value) > 0, 'touch directive emits a closed Pascal enum');
  Click(NyxInspectorEventsID);
  LKeys[0] := 'event-pointer-cancel';
  LKeys[1] := 'event-pointer-capture';
  LKeys[2] := 'event-pointer-capture-lost';
  LKeys[3] := 'event-drag-start';
  LKeys[4] := 'event-drag';
  LKeys[5] := 'event-drag-enter';
  LKeys[6] := 'event-drag-over';
  LKeys[7] := 'event-drag-exit';
  LKeys[8] := 'event-drop';
  LKeys[9] := 'event-drag-end';
  LBefore := Code.value;
  for LIndex := 0 to High(LKeys) do
  begin
    LKey := LKeys[LIndex];
    Check(Find(LKey + '-title') <> nil, 'actual inspector publishes gesture event');
    Check(Find(LKey + '-description').textContent <> '',
      'gesture event exposes intent and target help');
    Click(LKey + '-add');
    { Check source focus before Find changes the compact workspace back to the
      inspector. That panel switch deliberately rebuilds the source pane. }
    Check(Pos('// TODO:', Copy(Code.value, Code.selectionStart + 1, 80)) = 3,
      'actual Add action focuses its authored TODO');
    Check(Find(LKey + '-count').textContent = '1 registrations',
      'the inspector retains the new gesture registration');
  end;
  Check((Pos('.OnPointerCancel', Code.value) > 0) and
    (Pos('.OnDragStart', Code.value) > 0) and
    (Pos('.OnDrop', Code.value) > 0),
    'accepted companion uses named fluent editing methods');
  LKey := LKeys[3];
  Click(LKey + '-add');
  Check(Find(LKey + '-count').textContent = '2 registrations',
    'gesture event supports independent ordered callbacks');
  LPolicy := TJSHTMLSelectElement(Find(LKey + '-policy').querySelector('select'));
  LPolicy.value := 'ui-queue';
  LPolicy.dispatchEvent(TJSEvent.new('change'));
  LSource := Code.value;
  Check(Pos('.Policy(neUIQueue)', LSource) > 0, 'gesture policy uses typed source');
  Click(LKey + '-callback-0-remove');
  Check((Find('event-removal-warning') <> nil) and
    (Find(LKey + '-count').textContent = '2 registrations'),
    'gesture removal warns before changing the accepted pair');
  Click('event-removal-cancel');
  Check(Code.value = LSource, 'cancel preserves exact accepted source');
  Click(LKey + '-callback-0-remove');
  Click('event-removal-confirm');
  Check(Find(LKey + '-count').textContent = '1 registrations',
    'confirmation removes only one gesture callback');
  Click('action-undo');
  Check((Code.value = LSource) and
    (Find(LKey + '-count').textContent = '2 registrations'),
    'undo restores exact source and registrations');
  Check(Code.value <> LBefore, 'gesture source remains alongside the accepted design');
  Check(Find('studio-right').getBoundingClientRect.width <= window.innerWidth,
    'gesture cards fit the active Studio panel');
  document.body.setAttribute('data-gesture-studio', 'passed');
  document.body.setAttribute('data-gesture-studio-width', IntToStr(window.innerWidth));
  document.body.setAttribute('data-gesture-studio-checks', IntToStr(GChecks));
end;

procedure CheckFrame;
var
  LBody: TJSHTMLElement;
  LState: TNyxText;
begin
  Inc(GPolls);
  LBody := TJSHTMLElement(GFrame.contentDocument.body);
  LState := LBody.getAttribute('data-gesture-studio');

  if LState = 'passed' then
  begin
    document.body.setAttribute('data-gesture-studio-host', 'passed');
    document.body.setAttribute('data-gesture-studio-width',
      LBody.getAttribute('data-gesture-studio-width'));
    document.body.setAttribute('data-gesture-studio-checks',
      LBody.getAttribute('data-gesture-studio-checks'));
  end
  else if (LState = 'failed') or (GPolls >= 400) then
  begin
    document.body.setAttribute('data-gesture-studio-host', 'failed');
    document.body.setAttribute('data-gesture-studio-error',
      LBody.getAttribute('data-gesture-studio-error'));
  end
  else
  begin
    window.setTimeout(@CheckFrame, 25);
  end;
end;

begin
  try

    if window.location.search = '?host=1' then
    begin
      TJSHTMLElement(document.body).style.setProperty('margin', '0');
      GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
      GFrame.style.setProperty('width', '390px');
      GFrame.style.setProperty('height', '844px');
      GFrame.style.setProperty('border', '0');
      GFrame.src := 'gesture-studio.html?frame=1';
      document.body.appendChild(GFrame);
      window.setTimeout(@CheckFrame, 25);
    end
    else
    begin
      Run;
    end;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-gesture-studio', 'failed');
      document.body.setAttribute('data-gesture-studio-error', LException.Message);
    end
    else
    begin
      document.body.setAttribute('data-gesture-studio', 'failed');
      document.body.setAttribute('data-gesture-studio-error',
        'Browser host: ' + String(TJSObject(JSExceptValue)['stack']));
    end;
  end;
end.
