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
program nyx_editing_studio_tests;

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
    raise Exception.Create('Editing Studio: ' + AReason);
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

    if (Pos('event-', AID) = 1) or (AID = 'studio-right') then
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
    raise Exception.Create('Missing editing Studio control: ' + AID);
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
  LKeys: array[0..4] of TNyxText;
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
  Click(NyxInspectorEventsID);
  LKeys[0] := 'event-before-edit';
  LKeys[1] := 'event-composition-start';
  LKeys[2] := 'event-composition-update';
  LKeys[3] := 'event-composition-end';
  LKeys[4] := 'event-text-selection-change';
  LBefore := Code.value;
  for LIndex := 0 to High(LKeys) do
  begin
    LKey := LKeys[LIndex];
    Check(Find(LKey + '-title') <> nil, 'actual inspector publishes editing event');
    Check(Find(LKey + '-description').textContent <> '',
      'editing event exposes intent and target help');
    Click(LKey + '-add');
    { Check source focus before Find changes the compact workspace back to the
      inspector. That panel switch deliberately rebuilds the source pane. }
    Check(Pos('// TODO:', Copy(Code.value, Code.selectionStart + 1, 80)) = 3,
      'actual Add action focuses its authored TODO');
    Check(Find(LKey + '-count').textContent = '1 registrations',
      'the inspector retains the new editing registration');
  end;
  Check((Pos('.OnBeforeEdit', Code.value) > 0) and
    (Pos('.OnCompositionStart', Code.value) > 0) and
    (Pos('.OnTextSelectionChange', Code.value) > 0),
    'accepted companion uses named fluent editing methods');
  LKey := LKeys[3];
  Click(LKey + '-add');
  Check(Find(LKey + '-count').textContent = '2 registrations',
    'composition event supports independent ordered callbacks');
  LPolicy := TJSHTMLSelectElement(Find(LKey + '-policy').querySelector('select'));
  LPolicy.value := 'ui-queue';
  LPolicy.dispatchEvent(TJSEvent.new('change'));
  LSource := Code.value;
  Check(Pos('.Policy(neUIQueue)', LSource) > 0, 'editing policy uses typed source');
  Click(LKey + '-callback-0-remove');
  Check((Find('event-removal-warning') <> nil) and
    (Find(LKey + '-count').textContent = '2 registrations'),
    'editing removal warns before changing the accepted pair');
  Click('event-removal-cancel');
  Check(Code.value = LSource, 'cancel preserves exact accepted source');
  Click(LKey + '-callback-0-remove');
  Click('event-removal-confirm');
  Check(Find(LKey + '-count').textContent = '1 registrations',
    'confirmation removes only one editing callback');
  Click('action-undo');
  Check((Code.value = LSource) and
    (Find(LKey + '-count').textContent = '2 registrations'),
    'undo restores exact source and registrations');
  Check(Code.value <> LBefore, 'editing source remains alongside the accepted design');
  Check(Find('studio-right').getBoundingClientRect.width <= window.innerWidth,
    'editing cards fit the active Studio panel');
  document.body.setAttribute('data-editing-studio', 'passed');
  document.body.setAttribute('data-editing-studio-width', IntToStr(window.innerWidth));
  document.body.setAttribute('data-editing-studio-checks', IntToStr(GChecks));
end;

procedure CheckFrame;
var
  LBody: TJSHTMLElement;
  LState: TNyxText;
begin
  Inc(GPolls);
  LBody := TJSHTMLElement(GFrame.contentDocument.body);
  LState := LBody.getAttribute('data-editing-studio');

  if LState = 'passed' then
  begin
    document.body.setAttribute('data-editing-studio-host', 'passed');
    document.body.setAttribute('data-editing-studio-width',
      LBody.getAttribute('data-editing-studio-width'));
    document.body.setAttribute('data-editing-studio-checks',
      LBody.getAttribute('data-editing-studio-checks'));
  end
  else if (LState = 'failed') or (GPolls >= 400) then
  begin
    document.body.setAttribute('data-editing-studio-host', 'failed');
    document.body.setAttribute('data-editing-studio-error',
      LBody.getAttribute('data-editing-studio-error'));
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
      GFrame.src := 'editing-studio.html?frame=1';
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
      document.body.setAttribute('data-editing-studio', 'failed');
      document.body.setAttribute('data-editing-studio-error', LException.Message);
    end
    else
    begin
      document.body.setAttribute('data-editing-studio', 'failed');
      document.body.setAttribute('data-editing-studio-error',
        'Browser host: ' + String(TJSObject(JSExceptValue)['stack']));
    end;
  end;
end.
