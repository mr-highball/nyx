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

program nyx_placement_browser;
{$mode delphi}{$H+}{$codepage utf8}
uses
  SysUtils, Web, nyx.text, nyx.model, nyx.render.browser, nyx.generated.view;
var
  LDocument: TNyxDocument;
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LMemo: TJSHTMLTextAreaElement;
  LChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Browser placement: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  try
    { Consume the exact semantic export rather than rebuilding a second tree.
      Actual DOM assertions execute only when this staged harness is hosted. }
    LDocument := BuildNyxDocument;
    LRenderer := TNyxBrowserRenderer.Create;
    LHost := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(LHost);
    try
      LRenderer.Render(LDocument, LDocument.Find('home'), LHost, False);
      LMemo := TJSHTMLTextAreaElement(
        LRenderer.ElementFor('reusable-instance/notes-editor').querySelector('textarea'));
      Check(LMemo <> nil, 'customized reusable payload mounts');
      Check(LMemo.value = 'Keep these notes.', 'moved memo retains its English content');
      Check(LRenderer.ElementFor('reply-action') <> nil, 'new compound retains its parts');
      Check(LRenderer.ElementFor('first-caption').parentElement =
        LRenderer.ElementFor('right-layout'), 'relative placement reaches the correct browser parent');
      Check(LDocument.Find('send-button').Parent.ID = 'archive-layout', 'cross-page ownership survives compilation');
      LMemo.value := 'An English reply from the browser.';
      LMemo.dispatchEvent(TJSEvent.new('input'));
      Check(LMemo.value = 'An English reply from the browser.', 'actual browser edit is retained');
      LRenderer.Render(LDocument, LDocument.Find('archive'), LHost, False);
      Check(LRenderer.ElementFor('send-button').textContent = 'Post reply',
        'moved control mounts on its owning page');
      document.body.setAttribute('data-nyx-placement-controls', 'passed');
      LHost.textContent := 'PASS ' + IntToStr(LChecks) + ' browser placement checks';
    finally
      LRenderer.Free;
      LDocument.Free;
    end;
  except
    on E: Exception do
    begin
      document.body.textContent := E.Message;
      document.body.setAttribute('data-nyx-placement-controls', 'failed');
    end;
  end;
end.
