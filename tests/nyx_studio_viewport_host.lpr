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
program nyx_studio_viewport_host;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, JS, Web;

type
  { Select only maintained same-origin editor entries. The recovery consumer
    changes its public file host, while viewport geometry stays identical. }
  TViewportApplication = (vaStudio, vaRecovery);

var
  GFrame: TJSHTMLIframeElement;
  LParameters: TJSURLSearchParams;
  LWidth: Integer;
  LHeight: Integer;
  LWorkspace: String;
  LObserveSource: Boolean;
  LApplication: TViewportApplication;

{ URLSearchParams returns an external nullable value. Only an actual string
  becomes a copied argument; absent keys retain explicit harmless defaults. }
function ViewportArgument(const AName, ADefault: String): String;
var
  LValue: JSValue;
begin
  LValue := LParameters.get(AName);
  Result := ADefault;

  if isString(LValue) then
  begin
    Result := String(LValue);
  end;
end;

{ The browser owns the child and its ordinary editor. Copy actual DOM extents
  after load; a requested CSS dimension alone never proves the child's viewport.
  The parent owns no Studio controller, source buffer or editor capability. }
function FrameLoaded(AEvent: TJSEvent): Boolean;
begin
  Result := True;
  document.body.setAttribute('data-studio-viewport-width',
    IntToStr(GFrame.contentDocument.documentElement.clientWidth));
  document.body.setAttribute('data-studio-viewport-height',
    IntToStr(GFrame.contentDocument.documentElement.clientHeight));
end;

{ Optional read-only qualification evidence for browser clients whose DOM reader
  cannot traverse an iframe. Borrow only the currently mounted DOM textarea;
  copy its exact text/range into this fixture's attributes. Never call a Studio
  controller, retain a child widget, write its value or alter its selection.
  The containing page owns this timer and retires it when the fixture closes. }
procedure ObserveSource;
const
  CRecoveryAttributes: array[0..6] of String = (
    'data-recovery-local-packet', 'data-recovery-backup-attempt',
    'data-recovery-backup-packet', 'data-recovery-backup-outcome',
    'data-recovery-fixture-error', 'data-recovery-file-host',
    'data-recovery-observation');
var
  LDocument: TJSDocument;
  LEditor: TJSHTMLTextAreaElement;
  LIndex: Integer;
  LAttribute: JSValue;
begin
  LDocument := GFrame.contentDocument;
  LEditor := nil;
  { Forward only declared evidence published by the controlled file-host entry.
    Never call its controller or retain a child object. Navigation removes old
    evidence, so a returning production editor cannot masquerade as this fixture. }
  for LIndex := Low(CRecoveryAttributes) to High(CRecoveryAttributes) do
  begin
    document.body.removeAttribute(CRecoveryAttributes[LIndex]);

    if (LDocument <> nil) and (LDocument.body <> nil) then
    begin
      LAttribute := LDocument.body.getAttribute(CRecoveryAttributes[LIndex]);

      if isString(LAttribute) then
      begin
        document.body.setAttribute(CRecoveryAttributes[LIndex], String(LAttribute));
      end;
    end;
  end;

  if LDocument <> nil then
  begin
    LEditor := TJSHTMLTextAreaElement(
      LDocument.querySelector('textarea[aria-label="Pascal source"]'));
  end;
  document.body.setAttribute('data-studio-source-mounted', BoolToStr(LEditor <> nil, True));

  if LEditor <> nil then
  begin
    document.body.setAttribute('data-studio-source-text', LEditor.value);
    document.body.setAttribute('data-studio-source-start', IntToStr(LEditor.selectionStart));
    document.body.setAttribute('data-studio-source-end', IntToStr(LEditor.selectionEnd));
  end
  else
  begin
    document.body.removeAttribute('data-studio-source-text');
    document.body.removeAttribute('data-studio-source-start');
    document.body.removeAttribute('data-studio-source-end');
  end;
end;

begin
  LParameters := TJSURLSearchParams.new(window.location.search);
  LWidth := StrToIntDef(ViewportArgument('width', '390'), 390);
  LHeight := StrToIntDef(ViewportArgument('height', '844'), 844);
  LObserveSource := ViewportArgument('observe-source', 'false') = 'true';
  LApplication := vaStudio;

  if ViewportArgument('application', 'studio') = 'recovery' then
  begin
    LApplication := vaRecovery;
  end
  else if ViewportArgument('application', 'studio') <> 'studio' then
  begin
    raise Exception.Create('Unknown maintained Studio viewport application');
  end;

  if (LWidth < 240) or (LWidth > 4096) or (LHeight < 240) or (LHeight > 4096) then
  begin
    raise Exception.Create('Studio viewport dimensions must be in 240..4096');
  end;
  TJSHTMLElement(document.body).style.setProperty('margin', '0');
  TJSHTMLElement(document.body).style.setProperty('padding', '0');
  GFrame := TJSHTMLIframeElement(document.createElement('iframe'));
  GFrame.id := 'studio-viewport';
  GFrame.title := 'Nyx Studio viewport';
  GFrame.style.setProperty('display', 'block');
  GFrame.style.setProperty('border', '0');
  GFrame.style.setProperty('width', IntToStr(LWidth) + 'px');
  GFrame.style.setProperty('height', IntToStr(LHeight) + 'px');
  GFrame.addEventListener('load', @FrameLoaded);
  { The production entry is the default. The controlled file-host consumer uses
    the same controller and normal service admission; no global viewport value,
    editor method or application model is replaced by this containing host. }
  LWorkspace := ViewportArgument('workspace', '');
  GFrame.src := 'index.html';

  if LApplication = vaRecovery then
  begin
    GFrame.src := 'studio-recovery-controls.html?backup=' +
      encodeURIComponent(ViewportArgument('backup', 'request')) + '&connect=' +
      encodeURIComponent(ViewportArgument('connect', 'auto'));
  end;

  if LWorkspace <> '' then
  begin

    if Pos('?', GFrame.src) = 0 then
    begin
      GFrame.src := GFrame.src + '?';
    end
    else
    begin
      GFrame.src := GFrame.src + '&';
    end;
    GFrame.src := GFrame.src + 'workspace=' + encodeURIComponent(LWorkspace);
  end;
  document.body.appendChild(GFrame);

  if LObserveSource then
  begin
    window.setInterval(@ObserveSource, 100);
  end;
end.
