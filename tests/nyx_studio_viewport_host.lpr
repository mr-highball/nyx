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

var
  GFrame: TJSHTMLIframeElement;
  LParameters: TJSURLSearchParams;
  LWidth: Integer;
  LHeight: Integer;
  LWorkspace: String;

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

begin
  LParameters := TJSURLSearchParams.new(window.location.search);
  LWidth := StrToIntDef(ViewportArgument('width', '390'), 390);
  LHeight := StrToIntDef(ViewportArgument('height', '844'), 844);

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
  { Only the unchanged same-origin production editor may be embedded. The opaque
    reference is URL-encoded; the editor/service perform their normal admission.
    No global viewport value, editor method or application model is replaced. }
  LWorkspace := ViewportArgument('workspace', '');
  GFrame.src := 'index.html';

  if LWorkspace <> '' then
  begin
    GFrame.src := GFrame.src + '?workspace=' + encodeURIComponent(LWorkspace);
  end;
  document.body.appendChild(GFrame);
end.
