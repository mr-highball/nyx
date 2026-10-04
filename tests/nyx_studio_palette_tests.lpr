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

program nyx_studio_palette_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  JS,
  Web,
  nyx.text,
  nyx.data,
  nyx.studio.palette,
  nyx.studio.browser;

var
  GStudio: TNyxStudio;
  GChecks: Integer;
  GCompact: Boolean;
  GFrame: TJSHTMLIFrameElement;
  GPolls: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Studio palette: ' + AReason);
  end;
  Inc(GChecks);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if Result = nil then
  begin
    raise Exception.Create('Missing palette control: ' + AID);
  end;
end;

function Field(const AID: TNyxText): TJSHTMLElement;
begin
  Result := Find(AID);

  if not ((Result is TJSHTMLInputElement) or (Result is TJSHTMLSelectElement)) then
  begin
    Result := TJSHTMLElement(Result.querySelector('input,select'));
  end;

  if Result = nil then
  begin
    raise Exception.Create('Missing palette input: ' + AID);
  end;
end;

procedure Change(const AID, AValue: TNyxText);
var
  LField: TJSHTMLElement;
begin
  LField := Field(AID);
  TJSHTMLInputElement(LField).value := AValue;
  LField.dispatchEvent(TJSEvent.new('change'));
end;

procedure Click(const AID: TNyxText);
begin
  Find(AID).click;
end;

procedure ProjectPanel;
begin

  if GCompact then
  begin
    Click('action-panel-project');
  end;
end;

function Commands: Integer;
begin
  Result := Find('studio-palette').querySelectorAll('button').length;
end;

function Recovery: TNyxDataValue;
begin
  Result := TNyxDataValue.ParseJSON(window.localStorage.getItem('nyx-studio-project-v2'));
  Result := TNyxDataValue.ParseJSON(Result.Field('project').AsText);
end;

procedure Restore(const AKey, AValue: TNyxText);
begin

  if isString(AValue) then
  begin
    window.localStorage.setItem(AKey, AValue);
  end
  else
  begin
    window.localStorage.removeItem(AKey);
  end;
end;

procedure Run;
var
  LOldProject: TNyxText;
  LOldPalette: TNyxText;
  LOldOutput: TNyxText;
  LBefore: TNyxText;
  LAcceptedSource: TNyxText;
  LCanvas: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LCount: Integer;
  LPreferences: TNyxDataValue;
  LBounds: TJSDOMRect;
begin
  LOldProject := window.localStorage.getItem('nyx-studio-project-v2');
  LOldPalette := window.localStorage.getItem(NyxStudioPalettePreferencesKey);
  LOldOutput := window.localStorage.getItem('nyx-studio-output-target-v1');
  GCompact := window.innerWidth <= 960;
  try
    window.localStorage.removeItem('nyx-studio-project-v2');
    window.localStorage.removeItem(NyxStudioPalettePreferencesKey);
    window.localStorage.removeItem('nyx-studio-output-target-v1');
    GStudio := TNyxStudio.Create;
    GStudio.Run(True);
    LBefore := Recovery.ToJSON;
    LAcceptedSource := Recovery.Field('source').AsText;
    LCanvas := Find('project-description');
    ProjectPanel;
    LCount := Commands;
    Check((LCount = 73) and (Find('palette-items-list') <> nil),
      'default list includes every palette control');
    Click(NyxStudioPaletteGroupedID);
    Check((Commands = LCount) and (Find('palette-section-inputs') <> nil),
      'grouping retains the complete command set');
    Check(Find('palette-section-inputs').querySelector('[data-node=palette-search-field]') <> nil,
      'search compound shares the Inputs group');
    Check(Find(NyxStudioPaletteGroupedID).getAttribute('aria-pressed') = 'true',
      'mode exposes accessible pressed state');
    Change(NyxStudioPaletteSearchID, 'MeMo');
    Check((Commands = 1) and (Find('palette-memo') <> nil),
      'developer naming finds the text area');
    Check(Pos('multiline', Find('palette-memo').getAttribute('title')) > 0,
      'shared creator description reaches the actual browser tooltip');
    Click(NyxStudioPaletteDetailsID);
    Check(Pos('reply', Find('palette-description-memo').textContent) > 0,
      'touch-visible Details explains intent');
    Check(Find('palette-description-memo').getBoundingClientRect.width >=
      Find('studio-palette').getBoundingClientRect.width - 4,
      'help descriptions use the full palette width on both viewport sizes');
    Change(NyxStudioPaletteSearchID, 'description reply');
    Check((Commands = 1) and (Find('palette-memo') <> nil),
      'description terms support a help-style search');
    Change(NyxStudioPaletteSearchID, 'text input');
    Check((Commands > 1) and (Commands < 20) and (Find('palette-memo') <> nil) and
      (document.querySelector('[data-node=palette-button]') = nil),
      'multiword labels narrow the results without unrelated actions');
    Change(NyxStudioPaletteGroupID, 'Navigation');
    Check((Find('palette-empty') <> nil) and (Find(NyxStudioPaletteResetID) <> nil),
      'group and query intersect with an actionable empty state');
    Click(NyxStudioPaletteResetID);
    Check((Commands = LCount) and (Find('palette-section-navigation') <> nil) and
      (Find('palette-description-memo') <> nil), 'reset retains grouping and Details');
    Change(NyxStudioPaletteGroupID, 'Navigation');
    Check((document.querySelector('[data-node=palette-memo]') = nil) and
      (Find('palette-breadcrumbs') <> nil), 'group filtering works without a query');
    Click(NyxStudioPaletteListID);
    Check((Find('palette-items-list') <> nil) and (Commands < LCount),
      'list mode retains the chosen group filter');
    Click(NyxStudioPaletteGroupedID);
    Check(Recovery.ToJSON = LBefore, 'discovery leaves accepted design, source and drafts byte-identical');
    LPreferences := TNyxDataValue.ParseJSON(window.localStorage.getItem(NyxStudioPalettePreferencesKey));
    Check(LPreferences.Field('grouped').AsBoolean and LPreferences.Field('details').AsBoolean and
      (LPreferences.Field('group').AsText = 'navigation'), 'chosen presentation is saved separately');
    LBounds := Find('palette-mode').getBoundingClientRect;
    Check((LBounds.width > 0) and (LBounds.right <= window.innerWidth),
      'mode controls fit the actual viewport');
    LBounds := Field(NyxStudioPaletteSearchID).getBoundingClientRect;
    Check((LBounds.width > 100) and (LBounds.right <= window.innerWidth),
      'search keeps useful width on the compact host');

    if not GCompact then
    begin
      Check(Find('project-description') = LCanvas,
        'desktop discovery preserves the mounted preview control');
    end;
    GStudio.Free;
    GStudio := TNyxStudio.Create;
    GStudio.Run(True);
    ProjectPanel;
    Check((Find('palette-section-navigation') <> nil) and
      (Find('palette-description-breadcrumbs') <> nil) and
      (TJSHTMLSelectElement(Field(NyxStudioPaletteGroupID)).value = 'Navigation'),
      'a new Studio instance recovers mode, group and help preference');
    LInput := TJSHTMLInputElement(Field(NyxStudioPaletteSearchID));
    Check(LInput.value = '', 'search terms do not survive as a hidden recovery filter');
    Change(NyxStudioPaletteGroupID, 'Inputs');
    Click('palette-memo');
    Check((Recovery.Field('source').AsText <> LAcceptedSource) and
      (Pos('NewNyxMemo', Recovery.Field('source').AsText) > 0),
      'a discovered component still enters the real undoable design command');

    if GCompact then
    begin
      Click('action-panel-inspector');
    end;
    Check(Pos('description or reply', Find('selected-component-help').textContent) > 0,
      'selected control exposes its intent beside the real inspector');
    LBounds := Find('selected-component-help').getBoundingClientRect;
    Check((LBounds.width > 100) and (LBounds.right <= window.innerWidth),
      'contextual help fits the desktop and exact compact viewport');
    Click('action-undo');
    Check(Recovery.Field('source').AsText = LAcceptedSource,
      'presentation choices create no extra undo entries');
    document.body.setAttribute('data-nyx-palette-width', IntToStr(window.innerWidth));
    document.body.setAttribute('data-nyx-palette', 'passed');
    document.body.setAttribute('data-nyx-palette-checks', IntToStr(GChecks));
  finally
    FreeAndNil(GStudio);
    Restore('nyx-studio-project-v2', LOldProject);
    Restore(NyxStudioPalettePreferencesKey, LOldPalette);
    Restore('nyx-studio-output-target-v1', LOldOutput);
  end;
end;

procedure CheckFrame;
var
  LBody: TJSHTMLElement;
  LResult: TNyxText;
begin
  Inc(GPolls);
  LBody := TJSHTMLElement(GFrame.contentDocument.body);
  LResult := LBody.getAttribute('data-nyx-palette');

  if LResult = 'passed' then
  begin
    document.body.setAttribute('data-nyx-palette-host', 'passed');
    document.body.setAttribute('data-nyx-palette-frame-width',
      LBody.getAttribute('data-nyx-palette-width'));
    document.body.setAttribute('data-nyx-palette-frame-checks',
      LBody.getAttribute('data-nyx-palette-checks'));
  end
  else if (LResult = 'failed') or (GPolls >= 200) then
  begin
    document.body.setAttribute('data-nyx-palette-host', 'failed');
    document.body.setAttribute('data-nyx-palette-error', LBody.getAttribute('data-nyx-palette-error'));
  end
  else
  begin
    window.setTimeout(@CheckFrame, 25);
  end;
end;

procedure Host;
begin
  TJSHTMLElement(document.body).style.setProperty('margin', '0');
  GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
  GFrame.style.setProperty('width', '390px');
  GFrame.style.setProperty('height', '844px');
  GFrame.style.setProperty('border', '0');
  GFrame.src := 'palette.html?frame=1';

  if window.location.search = '?show-host=1' then
  begin
    GFrame.src := 'palette.html?show=1';
  end;
  document.body.appendChild(GFrame);

  if window.location.search <> '?show-host=1' then
  begin
    window.setTimeout(@CheckFrame, 25);
  end;
end;

begin
  try

    if window.location.search = '?show=1' then
    begin
      { A reviewable real Studio frame for visual QA, not a second mockup UI. }
      GCompact := window.innerWidth <= 960;
      GStudio := TNyxStudio.Create;
      GStudio.Run(False);
      ProjectPanel;
      Click(NyxStudioPaletteGroupedID);
      Click(NyxStudioPaletteDetailsID);
      Change(NyxStudioPaletteGroupID, 'Inputs');
      Find('studio-left').scrollTop := Round(Find('palette-title').getBoundingClientRect.top -
        Find('studio-left').getBoundingClientRect.top);
    end
    else if (window.location.search = '?host=1') or
      (window.location.search = '?show-host=1') then
    begin
      Host;
    end
    else
    begin
      Run;
    end;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-palette', 'failed');
      document.body.setAttribute('data-nyx-palette-error', LException.Message);
    end;
  end;
end.
