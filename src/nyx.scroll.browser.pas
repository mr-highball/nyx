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
unit nyx.scroll.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Web, nyx.text, nyx.scroll.presentation, nyx.theme;

{ Scoped to the actual native DOM scrolling face, never its descendants. These
  rules retain browser scrolling, focus, keyboard and accessibility ownership. }
function NyxBrowserScrollChromeCSS: TNyxText;
{ First use measures two different allocations, since a selector-support query
  can report support without implementing scrollbar sizing. The receipt lasts
  for this window; a browser/platform setting change requires a fresh window. }
procedure RequireNyxBrowserScrollChrome;
{ Absolute CSS thickness for the original scrolling face. Invalid or unbounded
  transform compensation refuses before any accepted view styles change. }
function NyxBrowserScrollChromeExtent(const APresentation: TNyxScrollPresentation;
  AContentScale: Double): Double;
procedure ProjectNyxBrowserScrollChrome(AElement: TJSHTMLElement;
  AExplicit: Boolean; const APresentation: TNyxScrollPresentation;
  AContentScale: Double; ATheme: TNyxTheme);

implementation

uses SysUtils, Math;

var
  GMeasured: Boolean;
  GSupported: Boolean;

function NyxBrowserScrollChromeExtent(const APresentation: TNyxScrollPresentation;
  AContentScale: Double): Double;
begin
  Result := 17 * APresentation.EffectiveScale(AContentScale, 17) / AContentScale;

  if IsNan(Result) or IsInfinite(Result) or (Result <= 0) or
    (Result > High(Integer) - 1.0) then
  begin
    raise ENyxScrollPresentation.Create('Browser scrollbar compensation exceeds its CSS metric domain');
  end;
end;

function NyxBrowserScrollChromeCSS: TNyxText;
begin
  Result := '.nyx-scroll-chrome{scrollbar-width:auto!important;' +
    'scrollbar-color:auto!important}' +
    '.nyx-scroll-chrome::-webkit-scrollbar{' +
    'width:var(--nyx-scroll-extent)!important;' +
    'height:var(--nyx-scroll-extent)!important}' +
    '.nyx-scroll-chrome::-webkit-scrollbar-track,' +
    '.nyx-scroll-chrome::-webkit-scrollbar-corner{' +
    'background:var(--nyx-scroll-track)}' +
    '.nyx-scroll-chrome::-webkit-scrollbar-thumb{' +
    'background:var(--nyx-scroll-thumb);border-radius:999px;' +
    'border:2px solid var(--nyx-scroll-track);background-clip:padding-box}';
end;

procedure RequireNyxBrowserScrollChrome;
var
  LStyle: TJSHTMLElement;
  LProbe: TJSHTMLElement;
  LChild: TJSHTMLElement;
  LSize: Integer;
begin

  if not GMeasured then
  begin
    LStyle := TJSHTMLElement(document.createElement('style'));
    LProbe := TJSHTMLElement(document.createElement('div'));
    try
      LStyle.textContent := '.nyx-scroll-measure{scrollbar-width:auto!important;' +
        'scrollbar-color:auto!important}' +
        '.nyx-scroll-measure::-webkit-scrollbar{' +
        'width:var(--nyx-measure)!important;height:var(--nyx-measure)!important}';
      LProbe.className := 'nyx-scroll-measure';
      LProbe.style.cssText := 'position:fixed;left:-10000px;top:0;width:100px;' +
        'height:100px;border:0;padding:0;overflow:scroll;visibility:hidden;contain:strict';
      LChild := TJSHTMLElement(document.createElement('div'));
      LChild.style.cssText := 'width:200px;height:200px';
      LProbe.appendChild(LChild);
      document.head.appendChild(LStyle);
      document.body.appendChild(LProbe);
      GSupported := True;
      for LSize := 23 to 24 do
      begin
        LProbe.style.setProperty('--nyx-measure', IntToStr(LSize) + 'px');
        GSupported := GSupported and
          (LProbe.offsetWidth - LProbe.clientWidth = LSize) and
          (LProbe.offsetHeight - LProbe.clientHeight = LSize);
      end;
      GMeasured := True;
    finally
      LProbe.remove;
      LStyle.remove;
    end;
  end;

  if not GSupported then
  begin
    raise ENyxScrollPresentation.Create(
      'This browser does not provide measured pixel-sized scrollbar presentation');
  end;
end;

procedure ProjectNyxBrowserScrollChrome(AElement: TJSHTMLElement;
  AExplicit: Boolean; const APresentation: TNyxScrollPresentation;
  AContentScale: Double; ATheme: TNyxTheme);
var
  LExtent: Double;
  LFormat: TFormatSettings;
begin

  if not AExplicit then
  begin
    AElement.classList.remove('nyx-scroll-chrome');
    AElement.style.removeProperty('--nyx-scroll-extent');
    AElement.style.removeProperty('--nyx-scroll-track');
    AElement.style.removeProperty('--nyx-scroll-thumb');
    Exit;
  end;
  RequireNyxBrowserScrollChrome;
  { Seventeen is this explicit themed face's logical baseline, not an inferred
    UA metric. Undo the ancestor content transform only for its cross axis. }
  LExtent := NyxBrowserScrollChromeExtent(APresentation, AContentScale);
  LFormat := FormatSettings;
  LFormat.DecimalSeparator := '.';
  AElement.style.setProperty('--nyx-scroll-extent', FloatToStr(LExtent, LFormat) + 'px');
  AElement.style.setProperty('--nyx-scroll-track', ATheme.Background);
  AElement.style.setProperty('--nyx-scroll-thumb', ATheme.Muted);
  AElement.classList.add('nyx-scroll-chrome');
end;

end.
