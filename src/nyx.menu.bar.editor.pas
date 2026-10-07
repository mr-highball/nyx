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

unit nyx.menu.bar.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.menu.editor,
  nyx.menu.bar.declarations;

type
  { Every mutation captures the complete edited group. Mask and inheritance
    remain distinct; no action publishes a document or retains a renderer. }
  TNyxMenuBarEditorAction = (nmbSave, nmbAddHeading, nmbMoveEarlier,
    nmbMoveLater, nmbRemoveHeading, nmbMask, nmbInherit);
  TNyxMenuBarEditorField = (nbfLabel, nbfWrap, nbfHover, nbfSearch,
    nbfSearchWindow, nbfSearchMatch, nbfPart, nbfMenu, nbfEnabled, nbfConfirm);
  TNyxMenuBarEditorChange = record
    Owner: TNyxControlRef;
    Baseline: TNyxText;
    Action: TNyxMenuBarEditorAction;
  end;
  { Reuse the public menu form's scalar draft protocol. A bar has its own draft
    value and context; copied input cannot cross owners or changed inheritance. }
  TNyxMenuBarEditorDraft = TNyxMenuEditorDraft;

{ Stable typed field/action identities for ordinary host navigation and tests.
  Heading fields/actions use their heading card's ID as the prefix. }
function NyxMenuBarEditorFieldID(const AEditorID: TNyxText;
  AField: TNyxMenuBarEditorField): TNyxText;
function NyxMenuBarEditorActionID(const AEditorID: TNyxText;
  AAction: TNyxMenuBarEditorAction): TNyxText;
function NyxMenuBarEditorHeadingID(const AEditorID: TNyxText; AIndex: Integer): TNyxText;
{ Exact local/effective grouping, menu registry and resolvable heading context.
  The document is borrowed only for this call; a missing/non-row owner refuses. }
function NyxMenuBarEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
{ Reusable compound of specialized Nyx controls. Effective inherited settings
  seed the form; Save creates a local override. Choice captions are bounded but
  their separate identity maps retain exact Unicode. Removal requires reviewed
  confirmation. The returned card owns its descendants and copied metadata;
  neither its lifetime nor its draft keeps the application document alive. }
function NewNyxMenuBarEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument): INyxCard;
{ Unrelated buttons return False. Exact form shape, closed choices, Boolean and
  numeric values are checked before returning independent immutable intent.
  No tree is changed. A host must compare Baseline and admit through its atomic
  paired boundary. Definition is separate because pas2js records cannot own
  COM interfaces. Mask/Inherit return nil; their Action preserves the distinction. }
function CaptureNyxMenuBarEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxMenuBarEditorChange; out ADefinition: INyxMenuBarDefinition): Boolean;

implementation

uses
  SysUtils, nyx.errors, nyx.data, nyx.composition, nyx.menu.types,
  nyx.menu.declarations, nyx.typeahead;

const
  CEditor = 'nyx.menu-bar-editor';
  CAction = 'nyx.menu-bar-editor.action';
  CIndex = 'nyx.menu-bar-editor.index';
  CCount = 'nyx.menu-bar-editor.count';
  CParts = 'nyx.menu-bar-editor.parts';
  CMenus = 'nyx.menu-bar-editor.menus';
  CFields: array[TNyxMenuBarEditorField] of TNyxText =
    ('label', 'wrap', 'hover', 'search', 'search-window', 'search-match',
     'part', 'menu', 'enabled', 'confirm');
  CActions: array[TNyxMenuBarEditorAction] of TNyxText =
    ('save', 'add-heading', 'move-earlier', 'move-later', 'remove-heading', 'mask', 'inherit');
  CMatches: array[TNyxTypeAheadMatch] of TNyxText = ('Unicode folded', 'Exact');

function NyxMenuBarEditorFieldID(const AEditorID: TNyxText;
  AField: TNyxMenuBarEditorField): TNyxText;
begin
  Result := AEditorID + TNyxText('-') + CFields[AField];
end;

function NyxMenuBarEditorActionID(const AEditorID: TNyxText;
  AAction: TNyxMenuBarEditorAction): TNyxText;
begin
  Result := AEditorID + TNyxText('-') + CActions[AAction];
end;

function NyxMenuBarEditorHeadingID(const AEditorID: TNyxText; AIndex: Integer): TNyxText;
begin
  Result := AEditorID + TNyxText('-heading-') + TNyxText(IntToStr(AIndex));
end;

function HeadingChoices(ARow: TNyxNode): TNyxDataValue;
var
  LValues: array of TNyxDataValue;

  procedure Collect(ANode: TNyxNode; const APath: TNyxText);
  var
    LIndex: Integer;
    LCount: Integer;
    LPart: TNyxText;
    LPath: TNyxText;
    LChild: TNyxNode;
  begin
    for LIndex := 0 to ANode.Count - 1 do
    begin
      LChild := ANode.Children[LIndex];
      LPart := LChild.Prop(NyxAttributeName(atPart));

      if LPart = '' then
      begin
        Continue;
      end;
      LPath := LPart;

      if APath <> '' then
      begin
        LPath := APath + TNyxText('/') + LPart;
      end;
      { Part paths resolve through named immediate descendants only. Do not
        offer inaccessible buttons below an unnamed container as usable parts. }

      if (LChild.ProjectionKind = NyxKindName(nkButton)) and
        (LChild.Prop(NyxAttributeName(atEnabled), 'true') = 'true') and
        (LChild.Prop(NyxAttributeName(atAction), NyxActionName(naNone)) =
          NyxActionName(naNone)) and
        (not LChild.HasMenu or (LChild.MenuReference.Name = '')) then
      begin
        LCount := Length(LValues);
        SetLength(LValues, LCount + 1);
        LValues[LCount] := NyxData(LPath);
      end;
      Collect(LChild, LPath);
    end;
  end;
begin
  Collect(ARow, '');
  Result := NyxArray(LValues);
end;

function BarData(const ADefinition: INyxMenuBarDefinition): TNyxDataValue;
begin
  Result := NyxNull;

  if ADefinition <> nil then
  begin
    Result := ADefinition.ToData;
  end;
end;

function NyxMenuBarEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
var
  LOwner: TNyxNode;
  LContext: TNyxNode;
  LRow: TNyxNode;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Menu bar editor requires a document');
  end;
  LOwner := ADocument.Find(AOwner.ID);

  if LOwner = nil then
  begin
    raise ENyxModel.Create('Menu bar editor owner is no longer in the document');
  end;
  LContext := RealizeNyxContext(ADocument, LOwner, LRow);
  try

    if (LRow = nil) or (LRow.ProjectionKind <> NyxKindName(nkRow)) then
    begin
      raise ENyxModel.Create('Menu bar editor requires a specialized row');
    end;
    Result := NyxObject([
      NyxField('menus', ADocument.Menus.ToData),
      NyxField('localDeclared', NyxData(LOwner.HasMenuBar)),
      NyxField('local', BarData(LOwner.MenuBar)),
      NyxField('effective', BarData(LRow.MenuBar)),
      NyxField('parts', HeadingChoices(LRow))]).ToJSON;
  finally
    LContext.Free;
  end;
end;

function NewNyxMenuBarEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument): INyxCard;
var
  LBaseline: TNyxDataValue;
  LParts: TNyxDataValue;
  LMenus: TNyxDataValue;
  LMenuNames: array of TNyxDataValue;
  LDefinition: INyxMenuBarDefinition;
  LHeading: INyxMenuBarHeading;
  LOptions: TNyxMenuBarOptions;
  LRow: INyxCard;
  LOwner: TNyxNode;
  LIndex: Integer;
  LCount: Integer;
  LStatus: TNyxText;

  procedure Flag(AParent: INyxControl; const APrefix: TNyxText;
    AField: TNyxMenuBarEditorField; const ALabel: TNyxText; AValue: Boolean);
  begin
    AParent.Add(NewNyxCheckbox(NyxMenuBarEditorFieldID(APrefix, AField)).Configure
      .PartName(NyxPart(CFields[AField])).Text(ALabel).Value(AValue).Done);
  end;

  procedure Button(AParent: INyxControl; const APrefix: TNyxText;
    AAction: TNyxMenuBarEditorAction; const ALabel: TNyxText;
    AEnabled: Boolean = True; AIndex: Integer = -1);
  var
    LButton: INyxButton;
  begin
    LButton := NewNyxButton(NyxMenuBarEditorActionID(APrefix, AAction));
    LButton.Configure.PartName(NyxPart(CActions[AAction])).Text(ALabel).Enabled(AEnabled).Done;
    LButton.Node.SetProp(CEditor, AID).SetProp(CAction, CActions[AAction])
      .SetProp(CIndex, TNyxText(IntToStr(AIndex)));
    AParent.Add(LButton);
  end;

  procedure Choice(AParent: INyxControl; const APrefix: TNyxText;
    AField: TNyxMenuBarEditorField; const ALabel: TNyxText;
    const AChoices: TNyxDataValue; const ASelected: TNyxText);
  var
    LChoice: Integer;
    LItems: TNyxText;
    LValue: TNyxText;
    LCaption: TNyxText;
  begin
    LItems := '';
    LValue := '';
    for LChoice := 0 to AChoices.Count - 1 do
    begin
      LCaption := NyxMenuEditorChoiceCaption(LChoice, AChoices.Item(LChoice).AsText);

      if LChoice > 0 then
      begin
        LItems := LItems + #10;
      end;
      LItems := LItems + LCaption;

      if (LChoice = 0) or (AChoices.Item(LChoice).AsText = ASelected) then
      begin
        LValue := LCaption;
      end;
    end;
    AParent.Add(NewNyxSelect(NyxMenuBarEditorFieldID(APrefix, AField)).Configure
      .PartName(NyxPart(CFields[AField])).Text(ALabel).Items(LItems).Value(LValue).Done);
  end;

  procedure Heading(AParent: INyxControl; const APrefix: TNyxText;
    const AHeading: INyxMenuBarHeading);
  var
    LPart: TNyxText;
    LMenu: TNyxText;
    LEnabled: Boolean;
    LChoice: Integer;
    LExisting: Integer;
    LUsed: Boolean;
  begin
    LPart := '';
    LMenu := '';
    LEnabled := True;

    if (AHeading = nil) and (LDefinition <> nil) then
    begin
      for LChoice := 0 to LParts.Count - 1 do
      begin
        LUsed := False;
        for LExisting := 0 to LDefinition.Count - 1 do
        begin
          LUsed := LUsed or
            (LDefinition.Item(LExisting).Part.Name = LParts.Item(LChoice).AsText);
        end;

        if not LUsed then
        begin
          LPart := LParts.Item(LChoice).AsText;
          Break;
        end;
      end;
    end;

    if AHeading <> nil then
    begin
      LPart := AHeading.Part.Name;
      LMenu := AHeading.Menu.Name;
      LEnabled := AHeading.IsEnabled;
    end;
    Choice(AParent, APrefix, nbfPart, 'Named heading button', LParts, LPart);
    Choice(AParent, APrefix, nbfMenu, 'Saved dropdown menu', LMenus, LMenu);
    Flag(AParent, APrefix, nbfEnabled, 'Allow this heading to open', LEnabled);
  end;

begin
  LBaseline := TNyxDataValue.ParseJSON(NyxMenuBarEditorBaseline(ADocument, AOwner));
  LOwner := ADocument.Find(AOwner.ID);
  LParts := LBaseline.Field('parts');
  SetLength(LMenuNames, ADocument.Menus.Count);
  for LIndex := 0 to High(LMenuNames) do
  begin
    LMenuNames[LIndex] := NyxData(ADocument.Menus.Reference(LIndex).Name);
  end;
  LMenus := NyxArray(LMenuNames);
  LOptions := NyxMenuBar('Component commands');
  LCount := 0;
  LStatus := 'No menu bar configured';

  if LBaseline.Field('effective').Kind <> ndNull then
  begin
    LDefinition := NyxMenuBarDefinitionFromData(LBaseline.Field('effective'));
    LOptions := LDefinition.Options;
    LCount := LDefinition.Count;
    LStatus := 'Inherited menu bar / Save creates a local override';

    if LOwner.HasMenuBar then
    begin
      LStatus := 'Local menu bar';
    end;
  end
  else if LOwner.HasMenuBar then
  begin
    LStatus := 'Menu bar suppressed on this component';
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Layout(nlColumn).Gap(8).Padding(12).Compound(True).Done;
  Result.Node.SetProp(NyxMenuFormOwnerKey, AOwner.ID)
    .SetProp(NyxMenuFormBaselineKey, LBaseline.ToJSON)
    .SetProp(NyxMenuFormReferenceKey, '')
    .SetProp(CCount, TNyxText(IntToStr(LCount)))
    .SetProp(CParts, LParts.ToJSON).SetProp(CMenus, LMenus.ToJSON);
  Result.Add(NewNyxHeading(AID + TNyxText('-title')).Configure
    .PartName(NyxPart('title')).Text('Menu bar').Done);
  Result.Add(NewNyxLabel(AID + TNyxText('-status')).Configure
    .PartName(NyxPart('status')).Text(LStatus).Done);
  Result.Add(NewNyxInput(NyxMenuBarEditorFieldID(AID, nbfLabel)).Configure
    .PartName(NyxPart(CFields[nbfLabel])).Text('Accessible bar label').Value(LOptions.Caption).Done);
  Flag(Result, AID, nbfWrap, 'Wrap heading traversal', LOptions.Wraps);
  Flag(Result, AID, nbfHover, 'Switch open menus on mouse entry', LOptions.Hovers);
  Flag(Result, AID, nbfSearch, 'Enable heading typeahead', LOptions.Search.IsEnabled);
  Result.Add(NewNyxSpin(NyxMenuBarEditorFieldID(AID, nbfSearchWindow)).Configure
    .PartName(NyxPart(CFields[nbfSearchWindow])).Text('Typeahead interval (milliseconds)')
    .Minimum(1).Maximum(60000)
    .Value(LOptions.Search.WindowMS).Done);
  Result.Add(NewNyxSelect(NyxMenuBarEditorFieldID(AID, nbfSearchMatch)).Configure
    .PartName(NyxPart(CFields[nbfSearchMatch])).Text('Typeahead matching')
    .Items(CMatches[ntmFolded] + #10 + CMatches[ntmExact])
    .Value(CMatches[LOptions.Search.MatchMode]).Done);
  Result.Add(NewNyxLabel(AID + TNyxText('-help')).Configure.PartName(NyxPart('help')).Text(
    'Use named button parts and saved dropdown menus. Save, add and reorder capture the whole form as one Undo step.').Done);
  for LIndex := 0 to LCount - 1 do
  begin
    LHeading := LDefinition.Item(LIndex);
    LRow := NewNyxCard(NyxMenuBarEditorHeadingID(AID, LIndex));
    LRow.Configure.Layout(nlColumn).Gap(6).Padding(8).Compound(True)
      .PartName(NyxPart(TNyxText('heading-') + TNyxText(IntToStr(LIndex)))).Done;
    Heading(LRow, LRow.ID, LHeading);
    Button(LRow, LRow.ID, nmbMoveEarlier, 'Move earlier', LIndex > 0, LIndex);
    Button(LRow, LRow.ID, nmbMoveLater, 'Move later', LIndex < LCount - 1, LIndex);
    Flag(LRow, LRow.ID, nbfConfirm, 'I reviewed removal of this heading', False);
    Button(LRow, LRow.ID, nmbRemoveHeading, 'Remove reviewed heading', LCount > 1, LIndex);
    Result.Add(LRow);
  end;
  LRow := NewNyxCard(AID + TNyxText('-new-heading'));
  LRow.Configure.Layout(nlColumn).Gap(6).Padding(8).Compound(True)
    .PartName(NyxPart('new-heading')).Done;
  Heading(LRow, LRow.ID, nil);
  Button(LRow, AID, nmbAddHeading, 'Add heading and save bar',
    (LParts.Count > LCount) and (LMenus.Count > 0) and
      (LCount < NyxMaximumMenuBarHeadings));
  Result.Add(LRow);
  Button(Result, AID, nmbSave, 'Save menu bar', LCount > 0);

  if (LParts.Count = 0) or (LMenus.Count = 0) then
  begin
    Result.Add(NewNyxLabel(AID + TNyxText('-choices-help')).Configure
      .PartName(NyxPart('choices-help')).Text(
      'Add named button parts to this row and define dropdown menus before saving a bar.').Done);
  end;
  Result.Add(NewNyxLabel(AID + TNyxText('-remove-warning')).Configure
    .PartName(NyxPart('remove-warning')).Text(
    'Removing a heading changes this component''s command navigation. Suppressing the whole bar overrides inheritance here; its menu definitions and other instances remain available.').Done);
  Flag(Result, AID, nbfConfirm, 'I reviewed suppression of this component''s bar', False);
  Button(Result, AID, nmbMask, 'Suppress menu bar here');
  Button(Result, AID, nmbInherit, 'Restore inherited menu bar', LOwner.HasMenuBar);
end;

function CaptureNyxMenuBarEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxMenuBarEditorChange; out ADefinition: INyxMenuBarDefinition): Boolean;
var
  LEditor: TNyxNode;
  LEditorID: TNyxText;
  LPrefix: TNyxText;
  LCount: Integer;
  LIndex: Integer;
  LSelected: Integer;
  LOther: Integer;
  LMatch: TNyxTypeAheadMatch;
  LAction: TNyxMenuBarEditorAction;
  LParts: TNyxDataValue;
  LMenus: TNyxDataValue;
  LOptions: TNyxMenuBarOptions;
  LHeadings: array of INyxMenuBarDefinition;
  LTemporary: INyxMenuBarDefinition;

  function Value(const APrefix: TNyxText; AField: TNyxMenuBarEditorField): TNyxText;
  var
    LField: TNyxNode;
  begin
    LField := LEditor.Find(NyxMenuBarEditorFieldID(APrefix, AField));

    if LField = nil then
    begin
      raise ENyxModel.Create('Menu bar editor field is no longer mounted');
    end;
    Result := LField.Prop('value');
  end;

  function Flag(const APrefix: TNyxText; AField: TNyxMenuBarEditorField): Boolean;
  var
    LValue: TNyxText;
  begin
    LValue := Value(APrefix, AField);

    if (LValue <> 'true') and (LValue <> 'false') then
    begin
      raise ENyxModel.Create('Menu bar checkbox requires an explicit Boolean');
    end;
    Result := LValue = 'true';
  end;

  function Reference(const APrefix: TNyxText; AField: TNyxMenuBarEditorField;
    const AChoices: TNyxDataValue): TNyxText;
  var
    LChoice: Integer;
    LValue: TNyxText;
  begin
    LValue := Value(APrefix, AField);
    for LChoice := 0 to AChoices.Count - 1 do
    begin

      if LValue = NyxMenuEditorChoiceCaption(LChoice, AChoices.Item(LChoice).AsText) then
      begin
        Exit(AChoices.Item(LChoice).AsText);
      end;
    end;
    raise ENyxModel.Create('Choose an available named heading and saved menu');
  end;

  function Heading(const APrefix: TNyxText): INyxMenuBarDefinition;
  begin
    Result := NewNyxMenuBarDefinition(LOptions).Heading(
      NyxPart(Reference(APrefix, nbfPart, LParts)),
      NyxMenuRef(Reference(APrefix, nbfMenu, LMenus)), Flag(APrefix, nbfEnabled));
  end;

begin
  Result := False;
  AChange := Default(TNyxMenuBarEditorChange);
  ADefinition := nil;

  if (AButton = nil) or (AShellRoot = nil) or (AButton.Prop(CEditor) = '') then
  begin
    Exit;
  end;
  LEditorID := AButton.Prop(CEditor);
  LEditor := AShellRoot.Find(LEditorID);

  if (LEditor = nil) or (LEditor.Prop(NyxMenuFormOwnerKey) = '') or
    (LEditor.Prop(NyxMenuFormBaselineKey) = '') then
  begin
    raise ENyxModel.Create('Menu bar editor context is no longer mounted');
  end;
  LSelected := -1;
  for LAction := Low(TNyxMenuBarEditorAction) to High(TNyxMenuBarEditorAction) do
  begin

    if AButton.Prop(CAction) = CActions[LAction] then
    begin
      LSelected := Ord(LAction);
      Break;
    end;
  end;

  if LSelected < 0 then
  begin
    raise ENyxModel.Create('Unknown menu bar editor action');
  end;
  AChange.Action := TNyxMenuBarEditorAction(LSelected);
  AChange.Owner := NyxControl(LEditor.Prop(NyxMenuFormOwnerKey));
  AChange.Baseline := LEditor.Prop(NyxMenuFormBaselineKey);

  if AChange.Action = nmbMask then
  begin

    if not Flag(LEditorID, nbfConfirm) then
    begin
      raise ENyxModel.Create('Review the warning and confirm suppression of this bar');
    end;
    Exit(True);
  end;

  if AChange.Action = nmbInherit then
  begin
    Exit(True);
  end;

  if not TryStrToInt(LEditor.Prop(CCount), LCount) or (LCount < 0) or
    (LCount > NyxMaximumMenuBarHeadings) or
    not TryStrToInt(Value(LEditorID, nbfSearchWindow), LIndex) or
    (LIndex < 1) or (LIndex > 60000) then
  begin
    raise ENyxModel.Create('Menu bar form requires bounded headings and integer search interval');
  end;
  LMatch := ntmFolded;

  if Value(LEditorID, nbfSearchMatch) = CMatches[ntmExact] then
  begin
    LMatch := ntmExact;
  end
  else if Value(LEditorID, nbfSearchMatch) <> CMatches[ntmFolded] then
  begin
    raise ENyxModel.Create('Choose the displayed typeahead matching policy');
  end;
  LOptions := NyxMenuBar(Value(LEditorID, nbfLabel))
    .Wrap(Flag(LEditorID, nbfWrap)).HoverSwitch(Flag(LEditorID, nbfHover))
    .TypeAhead(NyxTypeAhead.Enabled(Flag(LEditorID, nbfSearch))
      .WindowMilliseconds(LIndex).Match(LMatch));
  LParts := TNyxDataValue.ParseJSON(LEditor.Prop(CParts));
  LMenus := TNyxDataValue.ParseJSON(LEditor.Prop(CMenus));
  SetLength(LHeadings, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LHeadings[LIndex] := Heading(NyxMenuBarEditorHeadingID(LEditorID, LIndex));
  end;

  if AChange.Action = nmbAddHeading then
  begin
    SetLength(LHeadings, LCount + 1);
    LHeadings[LCount] := Heading(LEditorID + TNyxText('-new-heading'));
  end;

  if AChange.Action in [nmbMoveEarlier, nmbMoveLater, nmbRemoveHeading] then
  begin

    if not TryStrToInt(AButton.Prop(CIndex), LSelected) or
      (LSelected < 0) or (LSelected >= LCount) then
    begin
      raise ENyxModel.Create('This exact heading is no longer present');
    end;
    LPrefix := NyxMenuBarEditorHeadingID(LEditorID, LSelected);

    if AChange.Action = nmbRemoveHeading then
    begin

      if (LCount < 2) or not Flag(LPrefix, nbfConfirm) then
      begin
        raise ENyxModel.Create('Review heading removal; suppress the bar to remove its last heading');
      end;
      for LIndex := LSelected to LCount - 2 do
      begin
        LHeadings[LIndex] := LHeadings[LIndex + 1];
      end;
      SetLength(LHeadings, LCount - 1);
    end
    else
    begin
      LOther := LSelected - 1;

      if AChange.Action = nmbMoveLater then
      begin
        LOther := LSelected + 1;
      end;

      if (LOther < 0) or (LOther >= LCount) then
      begin
        raise ENyxModel.Create('Heading order has reached its boundary');
      end;
      LTemporary := LHeadings[LSelected];
      LHeadings[LSelected] := LHeadings[LOther];
      LHeadings[LOther] := LTemporary;
    end;
  end;
  ADefinition := NewNyxMenuBarDefinition(LOptions);
  for LIndex := 0 to High(LHeadings) do
  begin
    ADefinition := ADefinition.Heading(LHeadings[LIndex].Item(0).Part,
      LHeadings[LIndex].Item(0).Menu, LHeadings[LIndex].Item(0).IsEnabled);
  end;
  ADefinition := CopyNyxMenuBarDefinition(ADefinition);
  Result := True;
end;

end.
