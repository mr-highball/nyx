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


program nyx_resize_preview_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, Web, nyx.text, nyx.types, nyx.model, nyx.designer.resize,
  nyx.layout.constraints, nyx.render.browser, nyx.generated.view;

var
  GDocument: TNyxDocument;
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  GInput: TJSHTMLTextAreaElement;
  GChecks: Integer;
  GFrame: TJSHTMLIFrameElement;
  GPolls: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Browser resize preview: ' + AReason);
  end;
  Inc(GChecks);
end;

function Ink(AIndex: Integer): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-nyx-resize-edge="' +
    IntToStr(AIndex) + '"]'));
end;

procedure RetainedInput;
begin
  Check(GRenderer.InputFor('notes-editor') = GInput, 'the same textarea remains mounted');
  Check(GInput.value = 'An English draft stays independent.', 'uncommitted input remains exact');
  Check((GInput.selectionStart = 3) and (GInput.selectionEnd = 8), 'caret range remains exact');
  Check(document.activeElement = GInput, 'paint never takes input focus');
end;

procedure Journey;
var
  LFace: TJSHTMLElement;
  LBefore: TNyxResizeSize;
  LProposal: TNyxResizeSize;
  LInk: TJSHTMLElement;
  LOrigin: TJSDOMRect;
  LIndex: Integer;
  LRefused: Boolean;
  LSettings: TFormatSettings;
begin
  try
    GDocument := BuildNyxDocument;
    GRenderer := TNyxBrowserRenderer.Create;
    GHost := TJSHTMLElement(document.createElement('main'));
    GHost.style.cssText := 'height:340px;overflow:auto;width:100%;';
    document.body.appendChild(GHost);
    GRenderer.Render(GDocument, GDocument.Pages[0], GHost, True);
    GRenderer.Select('notes-editor');
    GInput := TJSHTMLTextAreaElement(GRenderer.InputFor('notes-editor'));
    GInput.value := 'An English draft stays independent.';
    GInput.focus;
    GInput.selectionStart := 3;
    GInput.selectionEnd := 8;
    LBefore := GRenderer.SizeFor('notes-editor', niDesign);
    LProposal := NyxResizeSize(Max(100, LBefore.Width - 32), LBefore.Height + 40);
    LFace := GRenderer.ElementFor('notes-editor', niDesign);
    LOrigin := LFace.getBoundingClientRect;
    GRenderer.PreviewResize(NyxResizePreview(NyxControl('notes-editor'), LProposal));
    Check(document.querySelectorAll('[data-nyx-resize-edge]').length = 4,
      'proposal owns exactly four real paint strips');
    Check(Abs(Ink(1).getBoundingClientRect.top - LOrigin.top - LProposal.Height) < 1,
      'proposed bottom reaches beyond the unchanged containing row');
    Check(Abs(Ink(3).getBoundingClientRect.left - LOrigin.left - LProposal.Width) < 1,
      'proposed width uses exact outer-face geometry');
    Check(GRenderer.SizeFor('notes-editor', niDesign).SameSize(LBefore),
      'preview never resizes the live control');
    RetainedInput;
    LInk := Ink(3);
    for LIndex := 0 to 3 do
    begin
      Check((Ink(LIndex).getAttribute('aria-hidden') = 'true') and
        (Ink(LIndex).style.getPropertyValue('pointer-events') = 'none'),
        'paint is non-interactive and hidden from the accessibility tree');
    end;
    LRefused := False;
    try
      GRenderer.PreviewResize(NyxResizePreview(NyxControl('other-editor'), LProposal));
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Ink(3) = LInk), 'foreign selection refuses without retiring current paint');
    GHost.scrollLeft := 16;
    GHost.scrollTop := 16;
    GHost.dispatchEvent(TJSEvent.new('scroll'));
    LOrigin := LFace.getBoundingClientRect;
    Check((Ink(3) = LInk) and
      (Abs(Ink(3).getBoundingClientRect.left - LOrigin.left - LProposal.Width) < 1),
      'captured viewport notifications move the same proposal with its real face');
    RetainedInput;
    LSettings := FormatSettings;
    try
      FormatSettings.DecimalSeparator := ',';
      GHost.style.setProperty('margin-left', '0.5px');
      LOrigin := LFace.getBoundingClientRect;
      GRenderer.PreviewResize(NyxResizePreview(NyxControl('notes-editor'), LProposal));
      Check(Abs(Ink(3).getBoundingClientRect.left - LOrigin.left - LProposal.Width) < 0.1,
        'fractional CSS geometry remains exact with a comma application locale');
    finally
      FormatSettings := LSettings;
      GHost.style.removeProperty('margin-left');
    end;
    GRenderer.PreviewResize(NyxResizePreview(NyxControl('notes-editor'),
      NyxResizeSize(MaximumNyxLayoutBound, MaximumNyxLayoutBound)));
    for LIndex := 0 to 3 do
    begin
      Check((Ink(LIndex).getBoundingClientRect.width <= GHost.clientWidth) and
        (Ink(LIndex).getBoundingClientRect.height <= GHost.clientHeight),
        'large logical proposal uses bounded viewport paint');
    end;
    GRenderer.PreviewResize(Default(TNyxResizePreview));
    Check(document.querySelectorAll('[data-nyx-resize-edge]').length = 0,
      'cancel/commit clear removes presentation');
    RetainedInput;
    GRenderer.PreviewResize(NyxResizePreview(NyxControl('notes-editor'), LProposal));
    GRenderer.Select('other-editor');
    Check(document.querySelectorAll('[data-nyx-resize-edge]').length = 0,
      'independent selection retires the proposal');
    GRenderer.Select('notes-editor');
    GRenderer.PreviewResize(NyxResizePreview(NyxControl('notes-editor'), LProposal));
    GRenderer.Unmount;
    Check(document.querySelectorAll('[data-nyx-resize-edge]').length = 0,
      'unmount releases paint and captured producers before retiring controls');
    GHost.dispatchEvent(TJSEvent.new('scroll'));

    { Remount the same semantic compiler input for a selective English capture.
      The driver exercises real DOM consumers and owned callbacks; synthetic
      viewport notifications do not qualify trusted touch, capture or IME.
      Globals own the renderer/document until this isolated page ends. }
    GRenderer.Render(GDocument, GDocument.Pages[0], GHost, True);
    GRenderer.Select('notes-editor');
    GRenderer.PreviewResize(NyxResizePreview(NyxControl('notes-editor'), LProposal));
    document.body.setAttribute('data-resize-preview', 'passed');
    document.body.setAttribute('data-resize-preview-checks', IntToStr(GChecks));
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-resize-preview', 'failed');
      document.body.setAttribute('data-resize-preview-error', LException.Message);
    end;
  end;
end;

procedure Observe;
var
  LBody: TJSHTMLElement;
  LResult: TNyxText;
begin
  Inc(GPolls);

  if GFrame.contentDocument <> nil then
  begin
    LBody := TJSHTMLElement(GFrame.contentDocument.body);
    LResult := LBody.getAttribute('data-resize-preview');

    if (LResult = 'passed') or (LResult = 'failed') then
    begin
      document.body.setAttribute('data-resize-preview', LResult);
      document.body.setAttribute('data-resize-preview-checks',
        LBody.getAttribute('data-resize-preview-checks'));
      document.body.setAttribute('data-resize-preview-error',
        LBody.getAttribute('data-resize-preview-error'));
      Exit;
    end;
  end;

  if GPolls >= 100 then
  begin
    document.body.setAttribute('data-resize-preview', 'failed');
    document.body.setAttribute('data-resize-preview-error', 'Narrow viewport did not finish');
    Exit;
  end;
  window.setTimeout(@Observe, 100);
end;

begin
  TJSHTMLElement(document.body).style.setProperty('margin', '0');

  if window.location.search = '?host=1' then
  begin
    GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
    GFrame.style.cssText := 'width:390px;height:900px;border:0;display:block;';
    document.body.appendChild(GFrame);
    GFrame.src := 'resize-preview.html';
    window.setTimeout(@Observe, 100);
  end
  else
  begin
    window.setTimeout(@Journey, 50);
  end;
end.
