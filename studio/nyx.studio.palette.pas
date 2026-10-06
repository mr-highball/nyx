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

unit nyx.studio.palette;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.catalog,
  nyx.catalog.labels;

type
  TNyxStudioPaletteMode = (pmList, pmGrouped);
  { Presentation belongs to the editor, never the design or undo history. Help
    remains available on touch hosts through Details, as well as native hints. }
  TNyxStudioPaletteState = record
    Mode: TNyxStudioPaletteMode;
    Group: TNyxPaletteGroup;
    Search: TNyxText;
    Details: Boolean;
  end;

const
  NyxStudioPaletteSearchID = 'palette-search';
  NyxStudioPaletteGroupID = 'palette-group-filter';
  NyxStudioPaletteListID = 'action-palette-list';
  NyxStudioPaletteGroupedID = 'action-palette-grouped';
  NyxStudioPaletteDetailsID = 'action-palette-details';
  NyxStudioPaletteResetID = 'action-palette-reset';
  NyxStudioPalettePreferencesKey = 'nyx-studio-palette-v1';

function DefaultNyxStudioPaletteState: TNyxStudioPaletteState;
{ Builds ordinary public Nyx controls. Parent owns all appended descendants;
  catalog and state are borrowed and remain unmodified. Empty groups disappear,
  each matching component appears once, and counts reflect the current filter. }
procedure AddNyxStudioPalette(AParent: TNyxNode; ACatalog: TNyxCatalog;
  const AState: TNyxStudioPaletteState);
{ Both target controllers use this same router. Unknown group values raise
  before changing state; unrelated events return False. No project command runs. }
function RouteNyxStudioPalette(ANode: TNyxNode; ATrigger: TNyxTrigger;
  var AState: TNyxStudioPaletteState): Boolean;
{ Small, versioned presentation packet. Search is intentionally transient.
  Decode stages all fields and preserves state on malformed/unknown input. }
function EncodeNyxStudioPalettePreferences(const AState: TNyxStudioPaletteState): TNyxText;
procedure DecodeNyxStudioPalettePreferences(const ASource: TNyxText;
  var AState: TNyxStudioPaletteState);

implementation

uses
  nyx.data;

function DefaultNyxStudioPaletteState: TNyxStudioPaletteState;
begin
  Result.Mode := pmList;
  Result.Group := pgAll;
  Result.Search := '';
  Result.Details := False;
end;

function EncodeNyxStudioPalettePreferences(const AState: TNyxStudioPaletteState): TNyxText;
begin
  Result := NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('grouped', NyxData(AState.Mode = pmGrouped)),
    NyxField('group', NyxData(NyxPaletteGroupKey(AState.Group))),
    NyxField('details', NyxData(AState.Details))]).ToJSON;
end;

procedure DecodeNyxStudioPalettePreferences(const ASource: TNyxText;
  var AState: TNyxStudioPaletteState);
var
  LData: TNyxDataValue;
  LGroup: TNyxPaletteGroup;
  LMode: TNyxStudioPaletteMode;
  LDetails: Boolean;
begin

  if Length(ASource) > 1024 then
  begin
    raise ENyxModel.Create('Palette preferences exceed their size limit');
  end;
  LData := TNyxDataValue.ParseJSON(ASource);

  if (LData.Kind <> ndObject) or (LData.Count <> 4) or
    (LData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Unsupported palette preferences');
  end;
  LMode := pmList;

  if LData.Field('grouped').AsBoolean then
  begin
    LMode := pmGrouped;
  end;

  if not TryNyxPaletteGroup(LData.Field('group').AsText, LGroup) then
  begin
    raise ENyxModel.Create('Unknown palette group');
  end;
  LDetails := LData.Field('details').AsBoolean;
  AState.Mode := LMode;
  AState.Group := LGroup;
  AState.Details := LDetails;
end;

function RouteNyxStudioPalette(ANode: TNyxNode; ATrigger: TNyxTrigger;
  var AState: TNyxStudioPaletteState): Boolean;
var
  LGroup: TNyxPaletteGroup;
begin
  Result := True;

  if (ATrigger = ntChange) and (ANode.ID = NyxStudioPaletteSearchID) then
  begin
    AState.Search := ANode.Prop('value');
  end
  else if (ATrigger = ntChange) and (ANode.ID = NyxStudioPaletteGroupID) then
  begin

    if not TryNyxPaletteGroup(ANode.Prop('value'), LGroup) then
    begin
      raise ENyxModel.Create('Unknown palette group');
    end;
    AState.Group := LGroup;
  end
  else if (ATrigger = ntClick) and (ANode.ID = NyxStudioPaletteListID) then
  begin
    AState.Mode := pmList;
  end
  else if (ATrigger = ntClick) and (ANode.ID = NyxStudioPaletteGroupedID) then
  begin
    AState.Mode := pmGrouped;
  end
  else if (ATrigger = ntClick) and (ANode.ID = NyxStudioPaletteDetailsID) then
  begin
    AState.Details := not AState.Details;
  end
  else if (ATrigger = ntClick) and (ANode.ID = NyxStudioPaletteResetID) then
  begin
    AState.Group := pgAll;
    AState.Search := '';
  end
  else
  begin
    Result := False;
  end;
end;

procedure AddNyxStudioPalette(AParent: TNyxNode; ACatalog: TNyxCatalog;
  const AState: TNyxStudioPaletteState);
var
  LBar: TNyxNode;
  LPalette: TNyxNode;
  LSection: TNyxNode;
  LItems: TNyxNode;
  LGroup: TNyxPaletteGroup;
  LIndex: Integer;
  LCount: Integer;
  LColumns: Integer;
  LCounts: array[TNyxPaletteGroup] of Integer;
  LChoices: TNyxText;

  function Matching(AIndex: Integer): Boolean;
  begin
    Result := (ACatalog[AIndex].Kind <> NyxKindName(nkPage)) and
      (ACatalog[AIndex].Kind <> NyxKindName(nkComponent)) and
      ACatalog.Matches(AIndex, AState.Search, AState.Group);
  end;

  procedure AddItems(AItems: TNyxNode; AGroup: TNyxPaletteGroup);
  var
    LIndex: Integer;
    LInfo: TNyxComponentInfo;
    LHint: TNyxText;
    LButton: TNyxNode;
    LItem: TNyxNode;
  begin
    for LIndex := 0 to ACatalog.Count - 1 do
    begin
      LInfo := ACatalog[LIndex];

      if Matching(LIndex) and
        ((AGroup = pgAll) or (LInfo.Discovery.Group = AGroup)) then
      begin
        LHint := LInfo.Discovery.Description + #10 +
          NyxPaletteGroupName(LInfo.Discovery.Group);

        if LInfo.Discovery.Labels <> [] then
        begin
          LHint := LHint + ' / ' + NyxComponentLabelNames(LInfo.Discovery.Labels);
        end;
        LButton := TNyxNode.Create(nkButton, 'palette-' + LInfo.Kind)
          .Configure.Text(LInfo.Title).Hint(LHint).DragSource(True).Done
          .SetProp('add-kind', LInfo.Kind);

        if AState.Details then
        begin
          LItem := TNyxNode.Create(nkColumn, 'palette-entry-' + LInfo.Kind)
            .Configure.Gap(4).Done;
          AItems.Add(LItem);
          LItem.Add(LButton);
          LItem.Add(TNyxNode.Create(nkLabel, 'palette-description-' + LInfo.Kind)
            .Configure.Text(LInfo.Discovery.Description).Done);
        end
        else
        begin
          AItems.Add(LButton);
        end;
      end;
    end;
  end;

begin
  LColumns := 2;

  if AState.Details then
  begin
    { Descriptions use the whole sidebar/touch width, avoiding narrow columns of
      long help text. Both adapters consume the same layout choice. }
    LColumns := 1;
  end;
  AParent.Add(TNyxNode.Create(nkHeading, 'palette-title').Configure.Text('COMPONENTS').Done);
  AParent.Add(TNyxNode.Create(nkInput, NyxStudioPaletteSearchID).Configure
    .Text('Find a component').Placeholder('Name, purpose or label...').Value(AState.Search).Done);
  LBar := TNyxNode.Create(nkRow, 'palette-mode').Configure.Gap(6).Done;
  AParent.Add(LBar);
  LBar.Add(TNyxNode.Create(nkButton, NyxStudioPaletteListID).Configure
    .Text('List').Pressed(AState.Mode = pmList).Variant(nvDefault).Done);
  LBar.Add(TNyxNode.Create(nkButton, NyxStudioPaletteGroupedID).Configure
    .Text('Grouped').Pressed(AState.Mode = pmGrouped).Variant(nvDefault).Done);
  LBar.Add(TNyxNode.Create(nkButton, NyxStudioPaletteDetailsID).Configure
    .Text('Details').Pressed(AState.Details).Hint('Show component descriptions').Done);
  LBar.Children[Ord(AState.Mode)].Configure.Variant(nvPrimary).Done;

  if AState.Details then
  begin
    LBar.Children[2].Configure.Variant(nvPrimary).Done;
  end;
  LChoices := '';
  for LGroup := Low(TNyxPaletteGroup) to High(TNyxPaletteGroup) do
  begin
    LCounts[LGroup] := 0;

    if LChoices <> '' then
    begin
      LChoices := LChoices + #10;
    end;
    LChoices := LChoices + NyxPaletteGroupName(LGroup);
  end;
  AParent.Add(TNyxNode.Create(nkSelect, NyxStudioPaletteGroupID).Configure
    .Text('Show group').Items(LChoices).Value(NyxPaletteGroupName(AState.Group)).Done);
  LCount := 0;
  for LIndex := 0 to ACatalog.Count - 1 do
  begin

    if Matching(LIndex) then
    begin
      Inc(LCount);
      Inc(LCounts[ACatalog[LIndex].Discovery.Group]);
    end;
  end;
  AParent.Add(TNyxNode.Create(nkLabel, 'palette-result-count').Configure
    .Text(IntToStr(LCount) + ' components').Done);
  LPalette := TNyxNode.Create(nkColumn, 'studio-palette').Configure.Gap(12).Done;
  AParent.Add(LPalette);

  if LCount = 0 then
  begin
    LPalette.Add(TNyxNode.Create(nkLabel, 'palette-empty').Configure
      .Text('No components match. Try another purpose or clear the filters.').Done);
    LPalette.Add(TNyxNode.Create(nkButton, NyxStudioPaletteResetID).Configure
      .Text('Clear filters').Done);
  end
  else if AState.Mode = pmList then
  begin
    LItems := TNyxNode.Create(nkGrid, 'palette-items-list').Configure.Columns(LColumns).Gap(6).Done;
    LPalette.Add(LItems);
    AddItems(LItems, pgAll);
  end
  else
  begin
    for LGroup := Succ(pgAll) to High(TNyxPaletteGroup) do
    begin

      if LCounts[LGroup] > 0 then
      begin
        LSection := TNyxNode.Create(nkColumn, 'palette-section-' + NyxPaletteGroupKey(LGroup))
          .Configure.Gap(6).Done;
        LPalette.Add(LSection);
        LSection.Add(TNyxNode.Create(nkHeading, 'palette-heading-' + NyxPaletteGroupKey(LGroup))
          .Configure.Text(NyxPaletteGroupName(LGroup) + ' (' + IntToStr(LCounts[LGroup]) + ')').Done);
        LItems := TNyxNode.Create(nkGrid, 'palette-items-' + NyxPaletteGroupKey(LGroup))
          .Configure.Columns(LColumns).Gap(6).Done;
        LSection.Add(LItems);
        AddItems(LItems, LGroup);
      end;
    end;
  end;
end;

end.
