{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
unit nyx.studio.chrome;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model, nyx.theme;

{ A shared editor palette, independent of the application being designed.
  The caller owns it. Platform hosts supply allocation, never a second UI skin. }
function NewNyxStudioChromeTheme: TNyxTheme;
{ Apply an ordinary Nyx composition/presentation profile after shell creation.
  Existing action identities and controller routes stay attached to the same
  nodes. The borrowed document owns one atlas resource, groups and disclosures. }
procedure ApplyNyxStudioChrome(ADocument: TNyxDocument; ACompact: Boolean;
  ASelectionToolsExpanded: Boolean = False; AAssetsExpanded: Boolean = False);
{ Capture only disposable editor disclosure state; absent parked panels leave
  the previous preference untouched. Neither project nor history is modified. }
procedure CaptureNyxStudioChrome(ARoot: TNyxNode; var ASelectionToolsExpanded: Boolean);
{ The quick asset picker is optional chrome; its retained search/selection and
  full Resources workspace keep their existing controllers and identities. }
procedure CaptureNyxStudioAssets(ARoot: TNyxNode; var AExpanded: Boolean);

implementation

uses SysUtils, Math, nyx.text, nyx.types, nyx.resources, nyx.controls,
  nyx.layout.policy, nyx.button.presentation, nyx.button.view, nyx.studio.glyphs, nyx.disclosure;

function NewNyxStudioChromeTheme: TNyxTheme;
begin
  Result := TNyxTheme.Create;
  Result.Background := '#f4f5fa';
  Result.Surface := '#ffffff';
  Result.Text := '#26283b';
  Result.Muted := '#71758c';
  Result.Border := '#e4e6ef';
  Result.Accent := '#7357e8';
  Result.AccentText := '#ffffff';
  Result.Radius := 14;
  Result.ControlRadius := 9;
  Result.FontSize := 14;
end;

procedure CaptureNyxStudioChrome(ARoot: TNyxNode; var ASelectionToolsExpanded: Boolean);
var
  LTools: TNyxNode;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LTools := ARoot.Find('studio-selection-tools');

  if (LTools <> nil) and NyxIsDisclosure(LTools) then
  begin
    ASelectionToolsExpanded := NyxDisclosureExpanded(LTools);
  end;
end;

procedure CaptureNyxStudioAssets(ARoot: TNyxNode; var AExpanded: Boolean);
var
  LPicker: TNyxNode;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LPicker := ARoot.Find('studio-assets-picker');

  if (LPicker <> nil) and NyxIsDisclosure(LPicker) then
  begin
    AExpanded := NyxDisclosureExpanded(LPicker);
  end;
end;

procedure ApplyNyxStudioChrome(ADocument: TNyxDocument; ACompact: Boolean;
  ASelectionToolsExpanded: Boolean; AAssetsExpanded: Boolean);
var
  LRoot, LHeader, LNode, LGroup, LRight, LTabs: TNyxNode;
  LDensity: TNyxButtonDensity;
  LIndex: Integer;
  LTools: INyxDisclosure;
  LAssets: INyxDisclosure;

  procedure Face(ANode: TNyxNode);
  var
    LChild: Integer;
    LPresentation: TNyxButtonPresentation;
  begin

    if ANode.ProjectionKind = 'button' then
    begin
      LPresentation := NyxButtonPresentation.Density(LDensity).FontWeight(nbwRegular);

      if ANode.Parent <> nil then
      begin

        if (ANode.Parent.ID = 'studio-views') or (ANode.Parent.ID = 'studio-left') or
          (Copy(ANode.ID, 1, 8) = 'palette-') then
        begin
          LPresentation := LPresentation.Alignment(nbaStart);
          ANode.Configure.WidthSizing(nsFill).Done;

          if ANode.Prop('variant') = '' then
          begin
            ANode.Configure.Variant(nvGhost).Done;
          end;
        end;
      end;
      ANode.Configure.ButtonPresentation(LPresentation).Done;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      Face(ANode.Children[LChild]);
    end;
  end;

  procedure Glyph(const AID: TNyxText; AGlyph: TNyxStudioGlyph;
    AContent: TNyxButtonContent = nbcGlyphCaption);
  var
    LButton: TNyxNode;
    LValue: TNyxButtonPresentation;
  begin
    LButton := LRoot.Find(AID);

    if LButton = nil then
    begin
      Exit;
    end;
    LValue := NyxStudioGlyphPresentation(AGlyph).Content(AContent).Density(LDensity);

    if (LButton.Parent <> nil) and (LButton.Parent.ID = 'studio-left') then
    begin
      LValue := LValue.Alignment(nbaStart);
    end;

    if LButton.Prop('variant') = NyxVariantName(nvPrimary) then
    begin
      LValue := LValue.GlyphTone(ngtForeground);
    end;
    LButton.Configure.ButtonPresentation(LValue).Done;

    if LButton.Prop('aria-label') = '' then
    begin
      LButton.Configure.AccessibleName(LButton.Prop('text')).Done;
    end;

    if LButton.Prop('hint') = '' then
    begin
      LButton.Configure.Hint(LButton.Prop('text')).Done;
    end;
  end;

  procedure Group(const AID: TNyxText; const ACommands: array of TNyxText);
  var
    LAt, LChild, LVisibleCount: Integer;
    LCommand: TNyxNode;
    LVisible: Boolean;
  begin
    LGroup := TNyxNode.Create(nkRow, AID);
    LGroup.Configure.Layout(TNyxLayoutPolicy.Row.Wrap(nfwNoWrap))
      .Padding(0).Gap(4).Done;
    LHeader.Add(LGroup);
    LVisible := False;
    LVisibleCount := 0;
    for LAt := 0 to High(ACommands) do
    begin
      LCommand := LHeader.Find(ACommands[LAt]);

      if (LCommand = nil) or (LCommand.Parent <> LHeader) then
      begin
        Continue;
      end;
      LVisible := LVisible or (LCommand.Prop('visible', 'true') <> 'false');

      if LCommand.Prop('visible', 'true') <> 'false' then
      begin
        Inc(LVisibleCount);
      end;
      for LChild := 0 to LHeader.Count - 1 do
      begin

        if LHeader.Children[LChild] = LCommand then
        begin
          LGroup.Add(LHeader.Extract(LChild));
          Break;
        end;
      end;
    end;
    LGroup.Configure.Visible(LVisible).Done;

    if ACompact and (AID = 'studio-workspace-commands') and (LVisibleCount > 1) then
    begin
      LGroup.Configure.WidthSizing(nsFill).Layout(TNyxLayoutPolicy.Row.Wrap(nfwWrap)).Done;
    end;
  end;

  procedure ProjectTools;
  const
    CCommands: array[0..3] of TNyxText = ('action-theme-toggle',
      'action-resources-toggle', 'action-logic-toggle', 'action-programs-toggle');
  var
    LLeft, LButton: TNyxNode;
    LAt, LChild, LPosition: Integer;
    LRow: INyxRow;
  begin
    LLeft := LRoot.Find('studio-left');

    if LLeft = nil then
    begin
      Exit;
    end;
    LRow := NewNyxRow('studio-project-tools');
    LRow.Configure.Padding(0).Gap(0).Layout(TNyxLayoutPolicy.Row.Wrap(nfwWrap)).Done;
    LPosition := LLeft.Count;
    for LAt := 0 to High(CCommands) do
    begin
      LButton := LLeft.Find(CCommands[LAt]);

      if (LButton = nil) or (LButton.Parent <> LLeft) then
      begin
        Continue;
      end;
      for LChild := 0 to LLeft.Count - 1 do
      begin

        if LLeft.Children[LChild] = LButton then
        begin
          LPosition := Min(LPosition, LChild);
          LRow.Node.Add(LLeft.Extract(LChild));
          LButton.Configure.WidthSizing(nsContent).ButtonPresentation(
            NyxButtonPresentationOf(LButton).Content(nbcGlyphOnly).GlyphSize(22).GlyphGap(0)).Done;
          Break;
        end;
      end;
    end;

    if LRow.Count > 0 then
    begin
      LLeft.Insert(LPosition, LRow);
    end;
  end;

  procedure Arrange(const AID: TNyxText);
  var
    LExisting: TNyxNode;
    LAt: Integer;
  begin
    LExisting := LRight.Find(AID);

    if (LExisting = nil) or (LExisting.Parent <> LRight) then
    begin
      Exit;
    end;
    for LAt := 0 to LRight.Count - 1 do
    begin

      if LRight.Children[LAt] = LExisting then
      begin
        LTools.BodyColumn.Node.Add(LRight.Extract(LAt));
        Break;
      end;
    end;
  end;

begin

  if (ADocument = nil) or (ADocument.Count = 0) then
  begin
    raise EArgumentException.Create('Studio chrome requires its owning shell');
  end;
  LRoot := ADocument.Pages[0];
  LDensity := nbdCompact;

  if ACompact then
  begin
    LDensity := nbdTouch;
  end;
  NyxResourceCatalogImport(ADocument.Resources).Import(NyxStudioGlyphResources);
  Face(LRoot);
  LNode := LRoot.Find('studio-left');

  if LNode <> nil then
  begin
    LNode.Configure.Padding(16).Gap(12).Surface(True).Done;

    if not ACompact then
    begin
      LNode.Configure.ForPlatform(npfNativeLCL).Width(248).Padding(16).Gap(12).Done;
    end;
  end;
  LNode := LRoot.Find('studio-right');

  if LNode <> nil then
  begin
    LNode.Configure.Padding(16).Gap(12).Surface(True).Done;

    if not ACompact then
    begin
      LNode.Configure.ForPlatform(npfNativeLCL).Width(304).Padding(16).Gap(12).Done;
    end;
  end;
  Glyph('action-undo', nsgUndo, nbcGlyphOnly);
  Glyph('action-redo', nsgRedo, nbcGlyphOnly);
  Glyph('action-code', nsgPascal);
  Glyph('action-import', nsgOpen, nbcGlyphOnly);
  Glyph('action-save', nsgSave, nbcGlyphOnly);
  Glyph('action-outputs', nsgOutputs);
  Glyph('action-agents', nsgAgents);
  Glyph('action-actions', nsgActions);
  Glyph('action-build-view', nsgBuildView);
  Glyph('action-build-app', nsgBuildApp);
  Glyph('action-theme-toggle', nsgTheme);
  Glyph('action-resources-toggle', nsgOpen);
  Glyph('action-logic-toggle', nsgActions);
  Glyph('action-programs-toggle', nsgPascal);
  Glyph('action-desktop', nsgDesktop);
  Glyph('action-phone', nsgPhone);
  Glyph('action-preview', nsgInteract);
  Glyph('action-canvas-tools', nsgTools);
  Glyph('action-canvas-expand', nsgExpand);
  ProjectTools;

  if ACompact then
  begin
    Glyph('action-outputs', nsgOutputs, nbcGlyphOnly);
    Glyph('action-agents', nsgAgents, nbcGlyphOnly);
    Glyph('action-actions', nsgActions, nbcGlyphOnly);
    Glyph('action-preview', nsgInteract, nbcGlyphOnly);
    Glyph('action-canvas-tools', nsgTools, nbcGlyphOnly);
    Glyph('action-canvas-expand', nsgExpand, nbcGlyphOnly);
  end;
  LNode := LRoot.Find('studio-resource-picker');

  if LNode <> nil then
  begin
    LGroup := LNode.Parent;
    for LIndex := 0 to LGroup.Count - 1 do
    begin

      if LGroup.Children[LIndex] = LNode then
      begin
        LAssets := NewNyxDisclosure('studio-assets-picker').WithText('Project assets');
        LAssets.Configure.Value(AAssetsExpanded).Surface(False).Padding(0).Gap(8).Done;
        LAssets.BodyColumn.Node.Add(LGroup.Extract(LIndex));
        LGroup.Insert(LIndex, LAssets);
        Face(LAssets.Node);
        Break;
      end;
    end;
  end;
  LHeader := LRoot.Find('studio-header');

  if LHeader <> nil then
  begin
    LHeader.Configure.Padding(12).Gap(12).Surface(True)
      .Layout(TNyxLayoutPolicy.Row.Wrap(nfwWrap).Align(ncaCenter)).Done;
    Group('studio-history-commands', ['action-undo', 'action-redo']);
    Group('studio-file-commands', ['action-code', 'action-import', 'action-save']);
    Group('studio-workspace-commands', ['action-outputs', 'action-agents', 'action-actions', 'action-builds']);
    Group('studio-build-commands', ['action-build-view', 'action-build-app']);
  end;
  LNode := LRoot.Find('studio-subtitle');

  if LNode <> nil then
  begin
    LNode.Configure.Text(ADocument.Title).Width(120).Done;
  end;
  LNode := LRoot.Find('views-title');

  if LNode <> nil then
  begin
    LNode.Configure.Text('Project').Done;
  end;
  LNode := LRoot.Find('palette-title');

  if LNode <> nil then
  begin
    LNode.Configure.Text('Components').Done;
  end;
  LNode := LRoot.Find('inspector-title');

  if LNode <> nil then
  begin
    LNode.Configure.Text('Inspector').Done;
  end;
  { Properties/Events must precede optional resize guidance and catalog help.
    Move the existing tabs, retaining their original action IDs and handlers. }
  LRight := LRoot.Find('studio-right');
  LTabs := LRoot.Find('inspector-tabs');

  if (LRight <> nil) and (LTabs <> nil) and (LTabs.Parent = LRight) then
  begin
    for LIndex := 0 to LRight.Count - 1 do
    begin

      if LRight.Children[LIndex] = LTabs then
      begin
        LRight.Insert(Min(2, LIndex), LRight.Extract(LIndex));
        Break;
      end;
    end;
    LTools := NewNyxDisclosure('studio-selection-tools').WithText('Arrange selection');
    LTools.Configure.Value(ASelectionToolsExpanded).Surface(False).Padding(0).Gap(8).Done;
    Arrange('studio-resize-tools');
    Arrange('studio-position-tools');
    Arrange('action-place-start');

    if LTools.BodyColumn.Count > 0 then
    begin
      LRight.Insert(3, LTools);
      Face(LTools.Node);
    end;
  end;
end;

end.
