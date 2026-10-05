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

program nyx_agent_state_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Web, nyx.text, nyx.model, nyx.codec, nyx.state,
  nyx.render.browser, nyx.generated.view;

var
  LDocument: TNyxDocument;
  LRenderer: TNyxBrowserRenderer;
  LOtherRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LOtherHost: TJSHTMLElement;
  LMemo: TJSHTMLTextAreaElement;
  LReusableMemo: TJSHTMLTextAreaElement;
  LCheckbox: TJSHTMLInputElement;
  LNumber: TJSHTMLInputElement;
  LBefore: TNyxText;
  LCount: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxState.Create('Compiled semantic browser: ' + AReason);
  end;
  Inc(LCount);
end;

begin
  LDocument := nil;
  LRenderer := nil;
  LOtherRenderer := nil;
  try
    try
      { The unchanged semantic export is a compiler input on both targets. This
        staged consumer must execute on an admitted HTTP host before parity can
        be claimed. DOM dispatch qualifies host input, not a physical IME. }
      LDocument := BuildNyxDocument;
      LBefore := TNyxCodec.Encode(LDocument);
      Check(LDocument.State.GetValue(NyxTextState('qualification-text')) =
        TNyxText('A🌙') + #0 + TNyxText('éZ'), 'compiled supplementary/NUL default');
      LHost := TJSHTMLElement(document.createElement('section'));
      document.body.appendChild(LHost);
      LRenderer := TNyxBrowserRenderer.Create;
      LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
      LMemo := TJSHTMLTextAreaElement(LRenderer.ElementFor('reply-memo').querySelector('textarea'));
      LReusableMemo := TJSHTMLTextAreaElement(LRenderer.ElementFor(
        LRenderer.Root.Find(NyxQualifiedID('first-card', 'reply-card')).Part('editor').ID).querySelector('textarea'));
      LCheckbox := TJSHTMLInputElement(LRenderer.ElementFor('remember-checkbox').querySelector('input'));
      LNumber := TJSHTMLInputElement(LRenderer.ElementFor('ratio-input').querySelector('input'));
      Check((LMemo.value = 'A thoughtful reply.') and
        (LReusableMemo.value = 'A thoughtful reply.'), 'renamed inherited bindings reach DOM controls');
      Check(LCheckbox.checked and (LNumber.value = '0.375'), 'typed Boolean/number controls');
      LMemo.value := 'Written through a browser memo.';
      LMemo.dispatchEvent(TJSEvent.new('change'));
      Check((LRenderer.State.GetValue(NyxTextState('response')) = LMemo.value) and
        (LReusableMemo.value = LMemo.value), 'two-way host memo event updates reusable projection');
      Check(LDocument.State.GetValue(NyxTextState('response')) = 'A thoughtful reply.',
        'runtime host input leaves authored defaults unchanged');
      LCheckbox.checked := False;
      LCheckbox.dispatchEvent(TJSEvent.new('change'));
      Check(not LRenderer.State.GetValue(NyxBooleanState('checked')), 'Boolean host input');
      LNumber.value := '0.625';
      LNumber.dispatchEvent(TJSEvent.new('change'));
      Check(LRenderer.State.GetValue(NyxNumberState('ratio')) = 0.625, 'numeric host input');
      LOtherHost := TJSHTMLElement(document.createElement('section'));
      document.body.appendChild(LOtherHost);
      LOtherRenderer := TNyxBrowserRenderer.Create;
      LOtherRenderer.Render(LDocument, LDocument.Pages[0], LOtherHost);
      Check(LOtherRenderer.State.GetValue(NyxTextState('response')) = 'A thoughtful reply.',
        'another runtime receives independent defaults');
      LOtherRenderer.State.SetValue(NyxTextState('response'), 'Independent browser runtime.');
      Check(LRenderer.State.GetValue(NyxTextState('response')) = 'Written through a browser memo.',
        'runtime stores remain independent');
      LRenderer.State.SetValue(NyxBooleanState('enabled'), False);
      LMemo.value := 'Disabled host proposal';
      LMemo.dispatchEvent(TJSEvent.new('change'));
      Check(LRenderer.State.GetValue(NyxTextState('response')) = 'Written through a browser memo.',
        'disabled parent policy refuses binding writes');
      Check(TNyxCodec.Encode(LDocument) = LBefore, 'host input retains exact authored design');
      document.body.setAttribute('data-nyx-agent-state-controls', 'passed');
      TJSHTMLElement(document.getElementById('result')).textContent :=
        'PASS ' + IntToStr(LCount) + ' compiled semantic browser control checks';
    except
      on LException: Exception do
      begin
        document.body.setAttribute('data-nyx-agent-state-controls', 'failed');
        TJSHTMLElement(document.getElementById('result')).textContent := 'FAIL ' + LException.Message;
      end;
    end;
  finally
    LOtherRenderer.Free;
    LRenderer.Free;
    LDocument.Free;
  end;
end.
