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

unit nyx.test.palette;

{$mode delphi}{$H+}
{$codepage utf8}

interface

{ Shared semantic discovery and authoring checks, executed by native and browser
  suites. Actual input, tooltip and mobile geometry checks live in target tests. }
function RunNyxPaletteTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.codec,
  nyx.catalog,
  nyx.catalog.labels,
  nyx.studio.session,
  nyx.studio.view,
  nyx.studio.palette;

function RunNyxPaletteTests: Integer;
var
  LSession: TNyxStudioSession;
  LView: TNyxDocument;
  LTemplate: TNyxNode;
  LState: TNyxStudioViewState;
  LPalette: TNyxStudioPaletteState;
  LInfo: TNyxComponentInfo;
  LIndex: Integer;
  LBefore: TNyxText;
  LPreferences: TNyxText;
  LRejected: Boolean;
  LExpected: Integer;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Palette: ' + AReason);
    end;
    Inc(Result);
  end;

  function AddCommands(ANode: TNyxNode): Integer;
  var
    LChild: Integer;
  begin
    Result := Ord(ANode.Prop('add-kind') <> '');
    for LChild := 0 to ANode.Count - 1 do
    begin
      Inc(Result, AddCommands(ANode.Children[LChild]));
    end;
  end;

  procedure Rebuild;
  begin
    FreeAndNil(LView);
    LView := BuildNyxStudioView(LSession, LState);
    LView.Validate;
  end;

begin
  Result := 0;
  LSession := TNyxStudioSession.Create;
  LView := nil;
  LTemplate := TNyxNode.Create(nkColumn, 'creator-template');
  try
    LBefore := LSession.Save;
    LState := DefaultNyxStudioViewState;
    Check((LState.Palette.Mode = pmList) and (LState.Palette.Group = pgAll) and
      not LState.Palette.Details, 'default presentation preserves the full list');
    for LIndex := 0 to LSession.Catalog.Count - 1 do
    begin
      LInfo := LSession.Catalog[LIndex];
      Check((LInfo.Discovery.Group <> pgAll) and (LInfo.Discovery.Group <> pgOther) and
        (Trim(LInfo.Discovery.Description) <> ''),
        'default component has a purpose group and help description: ' + LInfo.Kind);
    end;
    Check(LSession.Catalog.Matches(LSession.Catalog.IndexOf('memo'), 'MeMo'),
      'developer control names are searchable regardless of ASCII case');
    Check(LSession.Catalog.Matches(LSession.Catalog.IndexOf('memo'), 'text input'),
      'multiword search combines an intent label and a group');
    Check(LSession.Catalog.Matches(LSession.Catalog.IndexOf('memo'), 'description reply'),
      'high-level descriptions contribute useful help search terms');
    Check(not LSession.Catalog.Matches(LSession.Catalog.IndexOf('memo'), 'text navigation'),
      'all query terms are required, rather than matching one broad term');
    Check(LSession.Catalog.Matches(LSession.Catalog.IndexOf('breadcrumbs'), 'navigation', pgNavigation)
      and not LSession.Catalog.Matches(LSession.Catalog.IndexOf('memo'), '', pgNavigation),
      'typed group filters include navigation and exclude unrelated controls');
    Check(LSession.Catalog.Matches(LSession.Catalog.IndexOf('search-field'), 'query compound') and
      not LSession.Catalog.Matches(LSession.Catalog.IndexOf('column'), 'query compound'),
      'cross-cutting intent finds a compound without tagging every container');
    Check(LSession.Catalog.Matches(LSession.Catalog.IndexOf('select'), 'dropdown') and
      LSession.Catalog.Matches(LSession.Catalog.IndexOf('stat-card'), 'dashboard'),
      'common alternate terminology finds differently titled controls');
    Rebuild;
    Check(AddCommands(LView.Find('studio-palette')) = LSession.Catalog.Count - 2,
      'flat presentation includes all authorable entries exactly once');
    Check((LView.Find('palette-group') <> nil) and
      (LView.Find(NyxStudioPaletteGroupID) <> nil), 'group box and filter have distinct stable identities');
    Check(Pos('multiline', LView.Find('palette-memo').Prop('hint')) > 0,
      'accessible component hint includes its high-level description');
    LState.Palette.Mode := pmGrouped;
    Rebuild;
    Check(AddCommands(LView.Find('studio-palette')) = LSession.Catalog.Count - 2,
      'grouped presentation preserves the complete command set without duplicates');
    Check((LView.Find('palette-section-inputs').Find('palette-search-field') <> nil) and
      (LView.Find('palette-section-navigation').Find('palette-breadcrumbs') <> nil),
      'compound components appear with their primary intent');
    Check((LView.Find('palette-section-other') = nil) and
      (LView.Find('palette-section-composition') = nil), 'empty groups are omitted');
    LState.Palette.Search := 'text input';
    LState.Palette.Details := True;
    Rebuild;
    LExpected := 0;
    for LIndex := 0 to LSession.Catalog.Count - 1 do
    begin

      if LSession.Catalog.Matches(LIndex, LState.Palette.Search) then
      begin
        Inc(LExpected);
      end;
    end;
    Check(AddCommands(LView.Find('studio-palette')) = LExpected,
      'filtered grouped counts use the same portable discovery query');
    Check(LView.Find('palette-description-memo').Prop('text') =
      LSession.Catalog[LSession.Catalog.IndexOf('memo')].Discovery.Description,
      'touch-readable Details uses the creator metadata verbatim');
    LState.Palette.Group := pgNavigation;
    Rebuild;
    Check((LView.Find('palette-empty') <> nil) and
      (LView.Find(NyxStudioPaletteResetID) <> nil), 'combined filters provide a useful empty state');
    Check(RouteNyxStudioPalette(LView.Find(NyxStudioPaletteResetID), ntClick, LState.Palette) and
      (LState.Palette.Search = '') and (LState.Palette.Group = pgAll) and
      (LState.Palette.Mode = pmGrouped) and LState.Palette.Details,
      'clear filters retains the chosen presentation');
    LSession.Catalog.RegisterRecipe(NyxCustomKind('moon-notes'), 'Moon notes', 'Extensions', LTemplate);
    LSession.Catalog.Describe(NyxCustomKind('moon-notes'), pgForms, [clMessaging, clText],
      'journal', 'Capture observatory notes for the night crew / 🌙 漢字');
    LIndex := LSession.Catalog.IndexOf('moon-notes');
    Check(LSession.Catalog.Matches(LIndex, 'observatory crew', pgForms) and
      LSession.Catalog.Matches(LIndex, 'journal') and
      LSession.Catalog.Matches(LIndex, '🌙 漢字'),
      'creator descriptions, intent and exact Unicode terms participate in discovery');
    Rebuild;
    Check(LView.Find('palette-section-forms').Find('palette-moon-notes') <> nil,
      'custom recipes join the same optional grouping without browser special cases');
    LInfo := LSession.Catalog[LIndex];
    LRejected := False;
    try
      LSession.Catalog.Describe(NyxCustomKind('moon-notes'), pgAll, [], 'wrong', 'wrong');
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Catalog[LIndex].Discovery.Description = LInfo.Discovery.Description),
      'invalid metadata cannot replace the creator description');
    LInfo.Discovery.Description := 'A detached metadata edit';
    LInfo.Discovery.Group := pgOther;
    Check((LSession.Catalog[LIndex].Discovery.Group = pgForms) and
      (Pos('observatory', LSession.Catalog[LIndex].Discovery.Description) > 0),
      'a borrowed metadata value cannot mutate the registered creator intent');
    LPalette := DefaultNyxStudioPaletteState;
    LPreferences := EncodeNyxStudioPalettePreferences(LState.Palette);
    DecodeNyxStudioPalettePreferences(LPreferences, LPalette);
    Check((LPalette.Mode = pmGrouped) and (LPalette.Group = pgAll) and LPalette.Details and
      (LPalette.Search = ''), 'preferences retain presentation and leave search transient');
    LRejected := False;
    try
      DecodeNyxStudioPalettePreferences(
        '{"version":1,"grouped":false,"group":"made-up","details":true}', LPalette);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (EncodeNyxStudioPalettePreferences(LPalette) = LPreferences),
      'unknown preference values preserve all accepted presentation fields');
    LRejected := False;
    try
      DecodeNyxStudioPalettePreferences(
        '{"version":1,"grouped":false,"group":"all","details":"true"}', LPalette);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (EncodeNyxStudioPalettePreferences(LPalette) = LPreferences),
      'wrong preference types cannot publish partial mode changes');
    Check(LSession.Save = LBefore, 'finding components never changes the portable project');
    { Context help must follow the registered recipe even when its root projects
      as a generic column. This also exercises detached Unicode metadata through
      the same public view builder used by browser and LCL Studio shells. }
    LSession.Document.Pages[0].Add(LSession.Catalog.NewNode(
      NyxCustomKind('moon-notes'), 'night-shift-notes'));
    LSession.Select('night-shift-notes');
    LBefore := LSession.Save;
    Rebuild;
    Check(LView.Find('selected-component-help').Prop('text') =
      LSession.Catalog[LSession.Catalog.IndexOf('moon-notes')].Discovery.Description,
      'Inspector context preserves a registered recipe creator intent and Unicode');
    Check(LSession.Save = LBefore, 'reading contextual help never authors metadata into the design');
  finally
    LView.Free;
    LTemplate.Free;
    LSession.Free;
  end;
end;

end.
