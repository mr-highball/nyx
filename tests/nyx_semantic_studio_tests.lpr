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
program nyx_semantic_studio_tests;

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
    raise Exception.Create('Semantic Studio: ' + AReason);
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
    raise Exception.Create('Missing semantic Studio control: ' + AID);
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
  LKey: TNyxText;
  LTitles: TJSNodeList;
  LIndex: Integer;
  LTitle: TJSHTMLElement;
  LBefore: TNyxText;
  LSource: TNyxText;
  LPolicy: TJSHTMLSelectElement;
  LCompact: Boolean;
begin
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  LCompact := window.innerWidth < 900;
  GCompact := LCompact;
  Click('action-code');

  if LCompact then
  begin
    Click('action-panel-project');
  end;
  Click('palette-search-field');

  if LCompact then
  begin
    Click('action-panel-inspector');
  end;
  Click(NyxInspectorEventsID);
  LKey := '';
  LTitles := document.querySelectorAll('[data-node^="event-named-"][data-node$="-title"]');
  for LIndex := 0 to LTitles.length - 1 do
  begin
    LTitle := TJSHTMLElement(LTitles[LIndex]);

    if LTitle.textContent = 'OnSearch' then
    begin
      LKey := LTitle.getAttribute('data-node');
      LKey := Copy(LKey, 1, Length(LKey) - 6);
    end;
  end;
  Check(LKey <> '', 'search compound exposes its semantic action');
  Check(Find(LKey + '-routes-count').textContent = '1 source controls',
    'event card exposes its physical producer count');
  Check(Pos('value may be absent', Find(LKey + '-payload').textContent) > 0,
    'event card explains optional scalar input');
  Check(Pos('from ', Find(LKey + '-route-0').textContent) > 0,
    'event card identifies the sibling value source');
  LBefore := Code.value;
  Click(LKey + '-add');
  Check((Pos('.OnNamed(NyxSemantic(nseSearch))', Code.value) > 0) and
    (Pos('// TODO:', Copy(Code.value, Code.selectionStart + 1, 80)) = 3),
    'actual Add action creates typed registration and focuses its TODO');
  Click(LKey + '-add');
  Check(Find(LKey + '-count').textContent = '2 registrations',
    'actual Studio lists multiple semantic callbacks');
  Check(Find(LKey + '-callback-0-remove').getBoundingClientRect.height < 56,
    'the removal label stays on one line in a narrow inspector');
  LPolicy := TJSHTMLSelectElement(Find(LKey + '-policy').querySelector('select'));
  LPolicy.value := 'ui-queue';
  LPolicy.dispatchEvent(TJSEvent.new('change'));
  LSource := Code.value;
  Check(Pos('.Policy(neUIQueue)', LSource) > 0, 'semantic policy is fluently generated');
  Click(LKey + '-callback-0-remove');
  Check((Find('event-removal-warning') <> nil) and
    (Find(LKey + '-count').textContent = '2 registrations'),
    'semantic removal warns before changing the accepted pair');
  Click('event-removal-cancel');
  Check(Find(LKey + '-count').textContent = '2 registrations', 'cancel keeps both callbacks');
  Click(LKey + '-callback-0-remove');
  Click('event-removal-confirm');
  Check(Find(LKey + '-count').textContent = '1 registrations', 'confirmation removes only one callback');
  Click('action-undo');
  Check((Code.value = LSource) and (Find(LKey + '-count').textContent = '2 registrations'),
    'undo restores exact source and both semantic registrations');
  Click('action-redo');
  Check(Find(LKey + '-count').textContent = '1 registrations', 'redo restores exact semantic removal');
  Click('action-undo');
  Check(Code.value <> LBefore, 'semantic edits retain their accepted Pascal companion');
  Check(Find('studio-right').getBoundingClientRect.width <= window.innerWidth,
    'semantic event cards fit the active Studio panel');
  document.body.setAttribute('data-semantic-studio', 'passed');
  document.body.setAttribute('data-semantic-studio-width', IntToStr(window.innerWidth));
  document.body.setAttribute('data-semantic-studio-checks', IntToStr(GChecks));
end;

procedure CheckFrame;
var
  LBody: TJSHTMLElement;
  LState: TNyxText;
begin
  Inc(GPolls);
  LBody := TJSHTMLElement(GFrame.contentDocument.body);
  LState := LBody.getAttribute('data-semantic-studio');

  if LState = 'passed' then
  begin
    document.body.setAttribute('data-semantic-studio-host', 'passed');
    document.body.setAttribute('data-semantic-studio-width',
      LBody.getAttribute('data-semantic-studio-width'));
    document.body.setAttribute('data-semantic-studio-checks',
      LBody.getAttribute('data-semantic-studio-checks'));
  end
  else if (LState = 'failed') or (GPolls >= 400) then
  begin
    document.body.setAttribute('data-semantic-studio-host', 'failed');
    document.body.setAttribute('data-semantic-studio-error',
      LBody.getAttribute('data-semantic-studio-error'));
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
      GFrame.src := 'semantic-studio.html?frame=1';
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
      document.body.setAttribute('data-semantic-studio', 'failed');
      document.body.setAttribute('data-semantic-studio-error', LException.Message);
    end
    else
    begin
      document.body.setAttribute('data-semantic-studio', 'failed');
      document.body.setAttribute('data-semantic-studio-error',
        'Browser host: ' + String(TJSObject(JSExceptValue)['stack']));
    end;
  end;
end.
