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

program nyx_browser_visual;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  Web,
  nyx.text,
  nyx.model,
  nyx.theme,
  nyx.render.browser,
  nyx.sample;

var
  GRenderer: TNyxBrowserRenderer;
  GTheme: TNyxTheme;
  GChecks: Integer;

function CSSRGB(const AValue: TNyxText): TNyxText;
var
  LRGB: Integer;
begin
  LRGB := NyxThemeRGB(AValue);
  Result := 'rgb(' + IntToStr((LRGB shr 16) and $ff) + ', ' +
    IntToStr((LRGB shr 8) and $ff) + ', ' + IntToStr(LRGB and $ff) + ')';
end;

procedure CheckStyle(AElement: TJSHTMLElement; const AProperty, AExpected: TNyxText);
var
  LActual: TNyxText;
begin
  { Read the browser's computed result, not the generated stylesheet text. This
    detects scoping, cascade or native HTML control defaults that override tokens. }
  LActual := window.getComputedStyle(AElement).getPropertyValue(AProperty);

  if LActual <> AExpected then
  begin
    raise ENyxModel.Create('Browser visual ' + AProperty + ': actual ' +
      LActual + ', expected ' + AExpected);
  end;
  Inc(GChecks);
end;

procedure ReleaseView(AEvent: TJSEvent);
begin
  { The page owns its mounted renderer and borrowed theme for interactive use.
    Release bindings before their host disappears, and the theme after its user. }
  FreeAndNil(GRenderer);
  FreeAndNil(GTheme);
end;

procedure Mount;
var
  LDocument: TNyxDocument;
  LSurface: TJSHTMLElement;
  LButton: TJSHTMLElement;
  LInput: TJSHTMLElement;
begin
  { Render exactly the document used by native visual captures. Query flags only
    select known fixtures; no source, CSS selector or configuration is evaluated. }
  GTheme := TNyxTheme.Create(Pos('dark', window.location.search) > 0);

  if Pos('custom', window.location.search) > 0 then
  begin
    GTheme.Accent := '#123456';
    GTheme.AccentText := '#fedcba';
    GTheme.Border := '#8899aa';
    GTheme.Radius := 23;
    GTheme.ControlRadius := 19;
    GTheme.FontSize := 17;
  end;
  GRenderer := TNyxBrowserRenderer.Create(GTheme);
  LDocument := CreateNyxSample;
  try

    if Pos('parts', window.location.search) > 0 then
    begin
      { Presentation examples start in English. Dedicated override/control
        journeys retain multilingual input coverage independently of this view. }
      LDocument.Find('welcome-instance').OverridePart('title')
        .SetProp('text', 'My activity');
      LDocument.Find('welcome-instance').OverridePart('.', 'append')
        .Add(TNyxNode.Create('button', 'custom-view-action')
          .SetProp('text', '+ Add to this view').SetProp('emit', 'add'));
    end;
    TJSHTMLElement(document.body).style.setProperty('margin', '0');
    TJSHTMLElement(document.body).style.setProperty('height', '100vh');

    if Pos('narrow=1', window.location.search) > 0 then
    begin
      { Headless Edge on Windows imposes a minimum outer-window width. Fix this
        fixture's host to the requested 390 logical pixels instead of mistaking
        a cropped screenshot of its larger viewport for a narrow layout check. }
      TJSHTMLElement(document.body).style.setProperty('width', '390px');
    end;
    GRenderer.Render(LDocument, LDocument.Pages[0], TJSHTMLElement(document.body));
  finally
    { Realization is independent of the source document. The interactive view
      remains owned by GRenderer until this dedicated fixture page unloads. }
    LDocument.Free;
  end;
  window.addEventListener('unload', @ReleaseView);
  LSurface := GRenderer.ElementFor('welcome-instance/welcome-card');
  LButton := GRenderer.ElementFor('create-project');
  LInput := TJSHTMLElement(GRenderer.ElementFor('project-name').querySelector('input'));
  CheckStyle(GRenderer.ElementFor('home'), 'background-color', CSSRGB(GTheme.Background));
  CheckStyle(LSurface, 'background-color', CSSRGB(GTheme.Surface));
  CheckStyle(GRenderer.ElementFor('form-title'), 'color', CSSRGB(GTheme.Text));
  CheckStyle(GRenderer.ElementFor('welcome-instance/welcome-description'),
    'color', CSSRGB(GTheme.Muted));
  CheckStyle(LSurface, 'border-top-color', CSSRGB(GTheme.Border));
  CheckStyle(LButton, 'background-color', CSSRGB(GTheme.Accent));
  CheckStyle(LButton, 'color', CSSRGB(GTheme.AccentText));
  CheckStyle(LButton, 'font-size', IntToStr(GTheme.FontSize) + 'px');
  CheckStyle(LSurface, 'border-radius', IntToStr(GTheme.Radius) + 'px');
  CheckStyle(LButton, 'border-radius', IntToStr(GTheme.ControlRadius) + 'px');
  CheckStyle(LInput, 'border-radius', IntToStr(GTheme.ControlRadius) + 'px');
  CheckStyle(LInput, 'font-size', IntToStr(GTheme.FontSize) + 'px');
  CheckStyle(LInput, 'background-color', CSSRGB(GTheme.Surface));
  CheckStyle(LInput, 'color', CSSRGB(GTheme.Text));

  if (GRenderer.ElementFor('home').offsetWidth > window.innerWidth) or
    ((Pos('narrow=1', window.location.search) > 0) and
    (GRenderer.ElementFor('home').offsetWidth <> 390)) then
  begin
    raise ENyxModel.Create('Browser visual page exceeds its requested layout host');
  end;
  Inc(GChecks);
  document.body.setAttribute('data-nyx-visual', 'passed');
  document.body.setAttribute('data-nyx-visual-checks', IntToStr(GChecks));
  document.body.setAttribute('data-nyx-visual-width', IntToStr(window.innerWidth));
  document.body.setAttribute('data-nyx-visual-page-width',
    IntToStr(Round(GRenderer.ElementFor('home').offsetWidth)));
end;

begin
  try
    Mount;
  except
    on LException: Exception do
    begin
      ReleaseView(nil);
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-visual', 'failed');
      document.body.setAttribute('data-nyx-visual-error', LException.Message);
    end;
  end;
end.
