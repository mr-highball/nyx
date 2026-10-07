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

unit nyx.menu.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.menu.declarations;

const
  { Shared scalar-form context at the chrome metadata boundary. The menu and
    menu-bar compounds use the same owned draft capture/restore implementation;
    these keys never change authored application meaning. }
  NyxMenuFormOwnerKey = 'nyx.menu-editor.owner';
  NyxMenuFormBaselineKey = 'nyx.menu-editor.baseline';
  NyxMenuFormReferenceKey = 'nyx.menu-editor.reference';

type
  { Choice is presentation only. Other actions describe copied authoring intent;
    add/reorder/remove-item are complete definition replacements, never edits to
    the borrowed registry. A host must compare Baseline before publication. }
  TNyxMenuEditorAction = (nmeChoose, nmeSave, nmeAddItem, nmeMoveUp,
    nmeMoveDown, nmeRemoveItem, nmeAttach, nmeMask, nmeInherit, nmeRemove);
  TNyxMenuEditorField = (nmfDefinition, nmfName, nmfRoot, nmfTitle, nmfSide,
    nmfAlignment, nmfSizing, nmfWidth, nmfHeight, nmfGap, nmfMargin, nmfFocus,
    nmfEscape, nmfOutside, nmfOpening, nmfWrap, nmfSearch, nmfSearchWindow,
    nmfSearchMatch, nmfKind, nmfPart, nmfCommand, nmfGroup, nmfChecked,
    nmfEnabled, nmfSubmenu, nmfConfirm);
  TNyxMenuEditorChange = record
    Owner: TNyxControlRef;
    Baseline: TNyxText;
    Action: TNyxMenuEditorAction;
    Reference: TNyxMenuRef;
  end;
  { An immutable value snapshot of one mounted form, independent of renderer,
    document and interfaces. Incomplete input is retained verbatim until Save
    validates it. Copies share only an immutable private array; Capture creates
    a fresh array. Restore requires the same owner, registry/attachment baseline
    and inspected definition, so drafts cannot cross selection or menu changes.
    This is ephemeral presentation, never part of an exported design. }
  TNyxMenuEditorDraft = record
  private
    FEditorID: TNyxText;
    FOwner: TNyxText;
    FBaseline: TNyxText;
    FReference: TNyxText;
    FValues: array of record
      ID: TNyxText;
      Value: TNyxText;
    end;
  public
    { Absent editor keeps a parked draft across panel switches. A present form
      replaces the snapshot with copied scalar values and borrows no nodes. }
    procedure Capture(const AEditorID: TNyxText; AShellRoot: TNyxNode);
    { Applies all matching values before rendering; changed context retires the
      snapshot without editing the new form. An absent editor remains parked. }
    function Restore(AShellRoot: TNyxNode): Boolean;
    { Explicitly retires copied input when its host changes projects. }
    procedure Clear;
  end;

{ Stable identities for host navigation/input harnesses. Item fields use their
  row's ID as the prefix; enum values never denote executable property names. }
function NyxMenuEditorFieldID(const AEditorID: TNyxText;
  AField: TNyxMenuEditorField): TNyxText;
function NyxMenuEditorActionID(const AEditorID: TNyxText;
  AAction: TNyxMenuEditorAction): TNyxText;
function NyxMenuEditorItemID(const AEditorID: TNyxText; AIndex: Integer): TNyxText;
{ Bounded single-line Unicode caption. The ordinal distinguishes clipped names;
  callers retain the complete exact identity separately from the display text. }
function NyxMenuEditorChoiceCaption(AIndex: Integer; const AName: TNyxText): TNyxText;
{ Exact registry/local attachment baseline. The document is borrowed for this
  call only. A missing owner refuses rather than using the current selection. }
function NyxMenuEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
{ A reusable compound made entirely from specialized Nyx controls. The document
  is borrowed during construction; the result owns copied metadata/descendants.
  Empty Reference opens a new definition. Existing names are immutable here;
  new names cannot overwrite a saved definition. Roots and menu choices map
  indexed captions to exact identities, including embedded newlines/Unicode.
  Removal visibly requires a separate confirmation and remains dependency-safe
  only when the host admits the candidate with ordinary document validation. }
function NewNyxMenuEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument; const AReference: TNyxMenuRef): INyxCard;
{ Unrelated controls return False. Mounted identity, complete fields, closed
  choices, integers and explicit removal confirmation are checked before capture.
  Invalid input raises before mutating either tree. Definition is an
  independent immutable plan, returned separately because pas2js value records
  cannot safely contain COM interfaces. Complete registry/part/root admission
  belongs to the host's atomic candidate, including for inactive submenu branches. }
function CaptureNyxMenuEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxMenuEditorChange; out ADefinition: INyxMenuDefinition): Boolean;

implementation

uses
  SysUtils, nyx.data, nyx.root.types, nyx.menu.types, nyx.popover.types,
  nyx.typeahead;

const
  CEditor = 'nyx.menu-editor';
  COwner = NyxMenuFormOwnerKey;
  CBaseline = NyxMenuFormBaselineKey;
  CReference = NyxMenuFormReferenceKey;
  CMenus = 'nyx.menu-editor.menus';
  CRoots = 'nyx.menu-editor.roots';
  CCount = 'nyx.menu-editor.count';
  CIndex = 'nyx.menu-editor.index';
  CAction = 'nyx.menu-editor.action';
  CFields: array[TNyxMenuEditorField] of TNyxText =
    ('definition', 'name', 'root', 'title', 'side', 'alignment', 'sizing',
     'width', 'height', 'gap', 'margin', 'focus', 'escape', 'outside', 'opening',
     'wrap', 'search', 'search-window', 'search-match', 'kind', 'part', 'command',
     'group', 'checked', 'enabled', 'submenu', 'confirm');
  CActions: array[TNyxMenuEditorAction] of TNyxText =
    ('choose', 'save', 'add-item', 'move-up', 'move-down', 'remove-item', 'attach',
     'mask', 'inherit', 'remove');
  CSides: array[TNyxPopoverSide] of TNyxText = ('Below', 'Above', 'Right', 'Left');
  CAlignments: array[TNyxPopoverAlignment] of TNyxText = ('Start', 'Center', 'End');
  CSizings: array[TNyxPopoverSizing] of TNyxText = ('Fit content', 'Fixed allocation');
  COpenings: array[TNyxMenuOpening] of TNyxText = ('First command', 'Last command');
  CMatches: array[TNyxTypeAheadMatch] of TNyxText = ('Unicode folded', 'Exact');
  CKinds: array[TNyxMenuItemKind] of TNyxText =
    ('Action', 'Check', 'Radio', 'Separator', 'Submenu');

function NyxMenuEditorFieldID(const AEditorID: TNyxText;
  AField: TNyxMenuEditorField): TNyxText;
begin
  Result := AEditorID + TNyxText('-') + CFields[AField];
end;

procedure TNyxMenuEditorDraft.Clear;
begin
  FEditorID := '';
  FOwner := '';
  FBaseline := '';
  FReference := '';
  FValues := nil;
end;

procedure TNyxMenuEditorDraft.Capture(const AEditorID: TNyxText;
  AShellRoot: TNyxNode);
var
  LEditor: TNyxNode;

  procedure Collect(ANode: TNyxNode);
  var
    LIndex: Integer;
    LCount: Integer;
  begin

    if (ANode.Kind = NyxKindName(nkInput)) or
      (ANode.Kind = NyxKindName(nkSelect)) or
      (ANode.Kind = NyxKindName(nkSpin)) or
      (ANode.Kind = NyxKindName(nkCheckbox)) then
    begin
      LCount := Length(FValues);
      SetLength(FValues, LCount + 1);
      FValues[LCount].ID := ANode.ID;
      FValues[LCount].Value := ANode.Prop('value');
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Collect(ANode.Children[LIndex]);
    end;
  end;
begin
  LEditor := nil;

  if AShellRoot <> nil then
  begin
    LEditor := AShellRoot.Find(AEditorID);
  end;

  if (LEditor = nil) or (LEditor.Prop(COwner) = '') or
    (LEditor.Prop(CBaseline) = '') then
  begin
    Exit;
  end;
  Clear;
  FEditorID := AEditorID;
  FOwner := LEditor.Prop(COwner);
  FBaseline := LEditor.Prop(CBaseline);
  FReference := LEditor.Prop(CReference);
  Collect(LEditor);
end;

function TNyxMenuEditorDraft.Restore(AShellRoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LIndex: Integer;
begin
  Result := False;

  if (FEditorID = '') or (AShellRoot = nil) then
  begin
    Exit;
  end;
  LEditor := AShellRoot.Find(FEditorID);

  if LEditor = nil then
  begin
    Exit;
  end;

  if (LEditor.Prop(COwner) <> FOwner) or
    (LEditor.Prop(CBaseline) <> FBaseline) or
    (LEditor.Prop(CReference) <> FReference) then
  begin
    Clear;
    Exit;
  end;
  { Check the complete shape before writing any value. An item replacement
    changes the registry baseline; a malformed host form cannot partially use
    an older snapshot even when its context metadata happens to match. }
  for LIndex := 0 to High(FValues) do
  begin

    if LEditor.Find(FValues[LIndex].ID) = nil then
    begin
      Clear;
      Exit;
    end;
  end;
  for LIndex := 0 to High(FValues) do
  begin
    LEditor.Find(FValues[LIndex].ID).SetProp('value', FValues[LIndex].Value);
  end;
  Result := True;
end;

function NyxMenuEditorActionID(const AEditorID: TNyxText;
  AAction: TNyxMenuEditorAction): TNyxText;
begin
  Result := AEditorID + TNyxText('-') + CActions[AAction];
end;

function NyxMenuEditorItemID(const AEditorID: TNyxText; AIndex: Integer): TNyxText;
begin
  Result := AEditorID + TNyxText('-item-') + TNyxText(IntToStr(AIndex));
end;

function NyxMenuEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
var
  LOwner: TNyxNode;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Menu editor requires a document');
  end;
  LOwner := ADocument.Find(AOwner.ID);

  if LOwner = nil then
  begin
    raise ENyxModel.Create('Menu editor owner is no longer in the document');
  end;
  Result := NyxObject([
    NyxField('menus', ADocument.Menus.ToData),
    NyxField('local', NyxData(LOwner.HasMenu)),
    NyxField('name', NyxData(LOwner.MenuReference.Name))]).ToJSON;
end;

{ Keep exact names in metadata, while a bounded, single-line caption is readable
  in a select. The ordinal prevents collisions after clipping/control replacement.
  Decode scalar boundaries so native UTF-8 and browser UTF-16 stay equivalent. }
function InlineName(const AName: TNyxText): TNyxText;
var
  LPosition: Integer;
  LStart: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  Result := '';
  LPosition := 1;
  LCount := 0;
  while (LPosition <= Length(AName)) and (LCount < 60) do
  begin
    LStart := LPosition;

    if not NyxNextScalar(AName, LPosition, LScalar) then
    begin
      raise ENyxModel.Create('Menu choice name contains malformed Unicode');
    end;

    if (LScalar < 32) or (LScalar = 127) or (LScalar = $2028) or (LScalar = $2029) then
    begin
      Result := Result + TNyxText(' ');
    end
    else
    begin
      Result := Result + Copy(AName, LStart, LPosition - LStart);
    end;
    Inc(LCount);
  end;

  if LPosition <= Length(AName) then
  begin
    Result := Result + TNyxText('…');
  end;
end;

function MenuCaption(AIndex: Integer; const AName: TNyxText): TNyxText;
begin
  Result := TNyxText(IntToStr(AIndex + 1)) + TNyxText(' / ') + InlineName(AName);
end;

function NyxMenuEditorChoiceCaption(AIndex: Integer; const AName: TNyxText): TNyxText;
begin
  Result := MenuCaption(AIndex, AName);
end;

function RootCaption(AIndex: Integer; const ARoot: TNyxRootRef): TNyxText;
begin
  Result := TNyxText(IntToStr(AIndex + 1)) + TNyxText(' / ') +
    NyxRootKindName(ARoot.Kind) + TNyxText(' / ') + InlineName(ARoot.Name);
end;

function Joined(const AValues: array of TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  for LIndex := 0 to High(AValues) do
  begin

    if LIndex > 0 then
    begin
      Result := Result + #10;
    end;
    Result := Result + AValues[LIndex];
  end;
end;

function NewNyxMenuEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument; const AReference: TNyxMenuRef): INyxCard;
var
  LDefinition: INyxMenuDefinition;
  LItem: INyxMenuDeclarationItem;
  LOptions: TNyxMenuOptions;
  LPlacement: TNyxPopoverOptions;
  LRoot: TNyxRootRef;
  LRoots: array of TNyxDataValue;
  LMenus: array of TNyxDataValue;
  LRootItems: TNyxText;
  LMenuItems: TNyxText;
  LRootChoice: TNyxText;
  LMenuChoice: TNyxText;
  LIndex: Integer;
  LRow: INyxCard;
  LOwner: TNyxNode;

  procedure Text(AParent: INyxControl; const APrefix: TNyxText;
    AField: TNyxMenuEditorField; const ALabel, AValue: TNyxText; AEnabled: Boolean = True);
  begin
    AParent.Add(NewNyxInput(NyxMenuEditorFieldID(APrefix, AField)).Configure
      .Text(ALabel).Value(AValue).Enabled(AEnabled).Done);
  end;

  procedure Choice(AParent: INyxControl; const APrefix: TNyxText;
    AField: TNyxMenuEditorField; const ALabel: TNyxText;
    const AChoices: array of TNyxText; AValue: Integer);
  begin
    AParent.Add(NewNyxSelect(NyxMenuEditorFieldID(APrefix, AField)).Configure
      .Text(ALabel).Items(Joined(AChoices)).Value(AChoices[AValue]).Done);
  end;

  procedure Flag(AParent: INyxControl; const APrefix: TNyxText;
    AField: TNyxMenuEditorField; const ALabel: TNyxText; AValue: Boolean);
  begin
    AParent.Add(NewNyxCheckbox(NyxMenuEditorFieldID(APrefix, AField)).Configure
      .Text(ALabel).Value(AValue).Done);
  end;

  procedure Number(AField: TNyxMenuEditorField; const ALabel: TNyxText;
    AValue, AMinimum, AMaximum: Integer);
  begin
    Result.Add(NewNyxSpin(NyxMenuEditorFieldID(AID, AField)).Configure
      .Text(ALabel).Minimum(AMinimum).Maximum(AMaximum).Value(AValue).Done);
  end;

  procedure Button(AParent: INyxControl; const APrefix: TNyxText;
    AAction: TNyxMenuEditorAction; const ALabel: TNyxText;
    AEnabled: Boolean = True; AIndex: Integer = -1);
  var
    LButton: INyxButton;
  begin
    LButton := NewNyxButton(NyxMenuEditorActionID(APrefix, AAction));
    LButton.Configure.Text(ALabel).Enabled(AEnabled).Done;
    LButton.Node.SetProp(CEditor, AID).SetProp(CAction, CActions[AAction])
      .SetProp(CIndex, TNyxText(IntToStr(AIndex)));
    AParent.Add(LButton);
  end;

  procedure ItemFields(AParent: INyxControl; const APrefix: TNyxText;
    const AItem: INyxMenuDeclarationItem);
  var
    LKind: TNyxMenuItemKind;
    LMenuIndex: Integer;
    LPart: TNyxText;
    LCommand: TNyxText;
    LGroup: TNyxText;
    LSubmenu: TNyxText;
    LChecked: Boolean;
    LEnabled: Boolean;
  begin
    LKind := nmiAction;
    LPart := '';
    LCommand := '';
    LGroup := '';
    LSubmenu := 'Choose a menu';
    LChecked := False;
    LEnabled := True;

    if AItem <> nil then
    begin
      LKind := AItem.Kind;
      LPart := AItem.Part.Name;
      LCommand := AItem.Command.Name;
      LGroup := AItem.Group.Name;
      LChecked := AItem.IsChecked;
      LEnabled := AItem.IsEnabled;
      for LMenuIndex := 0 to ADocument.Menus.Count - 1 do
      begin

        if ADocument.Menus.Reference(LMenuIndex).Name = AItem.Submenu.Name then
        begin
          LSubmenu := MenuCaption(LMenuIndex, ADocument.Menus.Reference(LMenuIndex).Name);
        end;
      end;
    end;
    Choice(AParent, APrefix, nmfKind, 'Item kind', CKinds, Ord(LKind));
    Text(AParent, APrefix, nmfPart, 'Named content part', LPart);
    Text(AParent, APrefix, nmfCommand, 'Command (action, check or radio)', LCommand);
    Text(AParent, APrefix, nmfGroup, 'Exclusive group (radio)', LGroup);
    Flag(AParent, APrefix, nmfChecked, 'Initially checked (check or radio)', LChecked);
    Flag(AParent, APrefix, nmfEnabled, 'Initially enabled', LEnabled);
    AParent.Add(NewNyxSelect(NyxMenuEditorFieldID(APrefix, nmfSubmenu)).Configure
      .Text('Branch (submenu)').Items(LMenuItems).Value(LSubmenu).Done);
  end;

begin
  LOwner := nil;

  if ADocument <> nil then
  begin
    LOwner := ADocument.Find(AOwner.ID);
  end;

  if (AID = '') or (LOwner = nil) then
  begin
    raise ENyxModel.Create('Menu editor requires exact editor and document-owner identities');
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Layout(nlColumn).Gap(8).Padding(12).Done;
  Result.Node.SetProp(COwner, AOwner.ID).SetProp(CBaseline, NyxMenuEditorBaseline(ADocument, AOwner))
    .SetProp(CReference, AReference.Name);
  Result.Add(NewNyxHeading(AID + TNyxText('-heading')).Configure.Text('Menus').Done);
  Result.Add(NewNyxLabel(AID + TNyxText('-help')).Configure.Text(
    'Reuse named button and separator parts as a menu. Saved defaults are independent of runtime check and radio state.').Done);
  LMenuItems := 'Choose a menu';
  LMenuChoice := 'Choose a menu';
  SetLength(LMenus, ADocument.Menus.Count);
  for LIndex := 0 to ADocument.Menus.Count - 1 do
  begin
    LMenus[LIndex] := NyxData(ADocument.Menus.Reference(LIndex).Name);
    LMenuItems := LMenuItems + #10 + MenuCaption(LIndex, LMenus[LIndex].AsText);
    Result.Add(NewNyxLabel(AID + TNyxText('-menu-name-') + TNyxText(IntToStr(LIndex)))
      .Configure.Text(TNyxText('Menu ') + TNyxText(IntToStr(LIndex + 1)) +
        TNyxText(' / ') + LMenus[LIndex].AsText).Done);

    if LMenus[LIndex].AsText = AReference.Name then
    begin
      LMenuChoice := MenuCaption(LIndex, LMenus[LIndex].AsText);
    end;
  end;
  Result.Node.SetProp(CMenus, NyxArray(LMenus).ToJSON);
  Result.Add(NewNyxSelect(NyxMenuEditorFieldID(AID, nmfDefinition)).Configure
    .Text('Inspect or attach a saved menu').Items(LMenuItems).Value(LMenuChoice).Done);
  Button(Result, AID, nmeChoose, 'Open menu or start a new definition');
  Button(Result, AID, nmeAttach, 'Attach selected menu to this button',
    (LOwner.Kind = 'button') and (LOwner.ProjectionKind = 'button') and (ADocument.Menus.Count > 0));
  Button(Result, AID, nmeMask, 'Suppress inherited menu', LOwner.ProjectionKind = 'button');
  Button(Result, AID, nmeInherit, 'Restore menu inheritance', LOwner.ProjectionKind = 'button');
  LDefinition := nil;

  if AReference.Name <> '' then
  begin
    LDefinition := ADocument.Menus.Definition(AReference);
  end;
  LOptions := NyxMenu('Menu');

  if LDefinition <> nil then
  begin
    LOptions := LDefinition.Options;
  end;
  LPlacement := LOptions.Placement;
  Text(Result, AID, nmfName, 'Definition name', AReference.Name, LDefinition = nil);
  LRootItems := '';
  LRootChoice := '';
  SetLength(LRoots, ADocument.Count + ADocument.ComponentCount);
  for LIndex := 0 to High(LRoots) do
  begin

    if LIndex < ADocument.Count then
    begin
      LRoot := NyxPageRoot(ADocument.Pages[LIndex].ID);
    end
    else
    begin
      LRoot := NyxReusableRoot(ADocument.Components[LIndex - ADocument.Count].ID);
    end;
    LRoots[LIndex] := NyxObject([NyxField('kind', NyxData(Ord(LRoot.Kind))),
      NyxField('name', NyxData(LRoot.Name))]);

    if LIndex > 0 then
    begin
      LRootItems := LRootItems + #10;
    end;
    LRootItems := LRootItems + RootCaption(LIndex, LRoot);
    Result.Add(NewNyxLabel(AID + TNyxText('-root-name-') + TNyxText(IntToStr(LIndex)))
      .Configure.Text(TNyxText('Content ') + TNyxText(IntToStr(LIndex + 1)) +
        TNyxText(' / ') + NyxRootKindName(LRoot.Kind) + TNyxText(' / ') + LRoot.Name).Done);

    if (LIndex = 0) or ((LDefinition <> nil) and (LDefinition.Root.Kind = LRoot.Kind)
      and (LDefinition.Root.Name = LRoot.Name)) then
    begin
      LRootChoice := RootCaption(LIndex, LRoot);
    end;
  end;
  Result.Node.SetProp(CRoots, NyxArray(LRoots).ToJSON);
  Result.Add(NewNyxSelect(NyxMenuEditorFieldID(AID, nmfRoot)).Configure
    .Text('Content root').Items(LRootItems).Value(LRootChoice).Done);
  Text(Result, AID, nmfTitle, 'Accessible menu title', LPlacement.Title);
  Choice(Result, AID, nmfSide, 'Preferred side', CSides, Ord(LPlacement.Side));
  Choice(Result, AID, nmfAlignment, 'Alignment', CAlignments, Ord(LPlacement.Alignment));
  Choice(Result, AID, nmfSizing, 'Sizing', CSizings, Ord(LPlacement.SizeMode));
  Number(nmfWidth, 'Maximum width (logical pixels)', LPlacement.Width, 16, 16384);
  Number(nmfHeight, 'Maximum height (logical pixels)', LPlacement.Height, 16, 16384);
  Number(nmfGap, 'Anchor gap (logical pixels)', LPlacement.Gap, 0, 4096);
  Number(nmfMargin, 'Viewport margin (logical pixels)', LPlacement.Margin, 0, 4096);
  Text(Result, AID, nmfFocus, 'Popover focus part (menu opening takes precedence)',
    LPlacement.InitialFocus.Name);
  Flag(Result, AID, nmfEscape, 'Dismiss with Escape', npdEscape in LPlacement.Dismissals);
  Flag(Result, AID, nmfOutside, 'Dismiss on outside press', npdOutsidePress in LPlacement.Dismissals);
  Choice(Result, AID, nmfOpening, 'Initial command', COpenings, Ord(LOptions.OpenAt));
  Flag(Result, AID, nmfWrap, 'Wrap keyboard traversal', LOptions.Wraps);
  Flag(Result, AID, nmfSearch, 'Enable typeahead', LOptions.Search.IsEnabled);
  Number(nmfSearchWindow, 'Typeahead interval (milliseconds)', LOptions.Search.WindowMS, 1, 60000);
  Choice(Result, AID, nmfSearchMatch, 'Typeahead matching', CMatches, Ord(LOptions.Search.MatchMode));
  LIndex := 0;

  if LDefinition <> nil then
  begin
    LIndex := LDefinition.Count;
  end;
  Result.Node.SetProp(CCount, TNyxText(IntToStr(LIndex)));
  Result.Add(NewNyxHeading(AID + TNyxText('-items-heading')).Configure.Text('Ordered menu items').Done);
  Result.Add(NewNyxLabel(AID + TNyxText('-items-help')).Configure.Text(
    'Parts resolve in the chosen content root. Only fields relevant to the item kind are applied. Save, add and reorder capture the whole form as one Undo step.').Done);

  if LDefinition <> nil then
  begin
    for LIndex := 0 to LDefinition.Count - 1 do
    begin
      LItem := LDefinition.Item(LIndex);
      LRow := NewNyxCard(NyxMenuEditorItemID(AID, LIndex));
      LRow.Configure.Layout(nlColumn).Gap(6).Padding(8).Done;
      ItemFields(LRow, LRow.ID, LItem);
      Button(LRow, LRow.ID, nmeMoveUp, 'Move earlier', LIndex > 0, LIndex);
      Button(LRow, LRow.ID, nmeMoveDown, 'Move later', LIndex < LDefinition.Count - 1, LIndex);
      Button(LRow, LRow.ID, nmeRemoveItem, 'Remove item', LDefinition.Count > 1, LIndex);
      Result.Add(LRow);
    end;
  end;
  LRow := NewNyxCard(AID + TNyxText('-new-item'));
  LRow.Configure.Layout(nlColumn).Gap(6).Padding(8).Done;
  ItemFields(LRow, LRow.ID, nil);
  Button(LRow, AID, nmeAddItem, 'Add item and save definition');
  Result.Add(LRow);
  Button(Result, AID, nmeSave, 'Save menu definition', LDefinition <> nil);

  if LDefinition <> nil then
  begin
    Result.Add(NewNyxLabel(AID + TNyxText('-remove-warning')).Configure.Text(
      'Removing a definition affects every invoker and submenu that uses it. Referenced definitions cannot be removed; detach or update those uses first.').Done);
    Flag(Result, AID, nmfConfirm, 'I have reviewed removal of this definition', False);
    Button(Result, AID, nmeRemove, 'Remove reviewed menu definition');
  end;
end;

function CaptureNyxMenuEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxMenuEditorChange; out ADefinition: INyxMenuDefinition): Boolean;
var
  LEditor: TNyxNode;
  LEditorID: TNyxText;
  LRoots: TNyxDataValue;
  LMenus: TNyxDataValue;
  LRoot: TNyxRootRef;
  LPlacement: TNyxPopoverOptions;
  LDismissals: TNyxPopoverDismissals;
  LOptions: TNyxMenuOptions;
  LItems: array of INyxMenuDefinition;
  LCount: Integer;
  LIndex: Integer;
  LSelected: Integer;
  LOther: Integer;
  LTemporary: INyxMenuDefinition;
  LAction: TNyxMenuEditorAction;

  function Value(const APrefix: TNyxText; AField: TNyxMenuEditorField;
    const AProperty: TNyxText = 'value'): TNyxText;
  var
    LField: TNyxNode;
  begin
    LField := LEditor.Find(NyxMenuEditorFieldID(APrefix, AField));

    if LField = nil then
    begin
      raise ENyxModel.Create('Menu editor field is no longer mounted');
    end;
    Result := LField.Prop(AProperty);
  end;

  function Choice(const AValue: TNyxText; const AChoices: array of TNyxText): Integer;
  var
    LChoice: Integer;
  begin
    for LChoice := 0 to High(AChoices) do
    begin

      if AValue = AChoices[LChoice] then
      begin
        Exit(LChoice);
      end;
    end;
    raise ENyxModel.Create('Choose a current menu editor option');
  end;

  function Number(AField: TNyxMenuEditorField; AMinimum, AMaximum: Integer): Integer;
  begin

    if not TryStrToInt(Value(LEditorID, AField), Result) or
      (Result < AMinimum) or (Result > AMaximum) then
    begin
      raise ENyxModel.Create('Menu editor requires a complete integer in the displayed range');
    end;
  end;

  function Flag(const APrefix: TNyxText; AField: TNyxMenuEditorField): Boolean;
  var
    LValue: TNyxText;
  begin
    LValue := Value(APrefix, AField);

    if (LValue <> 'true') and (LValue <> 'false') then
    begin
      raise ENyxModel.Create('Menu editor checkbox requires an explicit Boolean');
    end;
    Result := LValue = 'true';
  end;

  function Menu(const APrefix: TNyxText; AField: TNyxMenuEditorField;
    AAllowEmpty: Boolean): TNyxMenuRef;
  var
    LChoice: Integer;
    LValue: TNyxText;
  begin
    Result := Default(TNyxMenuRef);
    LValue := Value(APrefix, AField);

    if AAllowEmpty and (LValue = 'Choose a menu') then
    begin
      Exit;
    end;
    for LChoice := 0 to LMenus.Count - 1 do
    begin

      if LValue = MenuCaption(LChoice, LMenus.Item(LChoice).AsText) then
      begin
        Exit(NyxMenuRef(LMenus.Item(LChoice).AsText));
      end;
    end;
    raise ENyxModel.Create('Choose an available saved menu');
  end;

  function Item(const APrefix: TNyxText): INyxMenuDefinition;
  var
    LKind: TNyxMenuItemKind;
    LPart: TNyxPartRef;
  begin
    LKind := TNyxMenuItemKind(Choice(Value(APrefix, nmfKind), CKinds));
    LPart := NyxPart(Value(APrefix, nmfPart));
    Result := NewNyxMenuDefinition(LRoot, LOptions);
    case LKind of
      nmiAction:
        Result := Result.Action(LPart, NyxMenuCommand(Value(APrefix, nmfCommand)),
          Flag(APrefix, nmfEnabled));
      nmiCheck:
        Result := Result.Check(LPart, NyxMenuCommand(Value(APrefix, nmfCommand)),
          Flag(APrefix, nmfChecked), Flag(APrefix, nmfEnabled));
      nmiRadio:
        Result := Result.Radio(LPart, NyxMenuCommand(Value(APrefix, nmfCommand)),
          NyxMenuGroup(Value(APrefix, nmfGroup)), Flag(APrefix, nmfChecked), Flag(APrefix, nmfEnabled));
      nmiSeparator:
        Result := Result.Separator(LPart);
      nmiSubmenu:
        Result := Result.Submenu(LPart, Menu(APrefix, nmfSubmenu, False), Flag(APrefix, nmfEnabled));
    end;
  end;

begin
  AChange := Default(TNyxMenuEditorChange);
  ADefinition := nil;
  Result := (AButton <> nil) and (AButton.Prop(CEditor) <> '');

  if not Result then
  begin
    Exit;
  end;
  LEditorID := AButton.Prop(CEditor);
  LEditor := nil;

  if AShellRoot <> nil then
  begin
    LEditor := AShellRoot.Find(LEditorID);
  end;

  if (LEditor = nil) or (LEditor.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxModel.Create('Select the current menu editor before applying a change');
  end;
  AChange.Owner := NyxControl(LEditor.Prop(COwner));
  AChange.Baseline := LEditor.Prop(CBaseline);
  LMenus := TNyxDataValue.ParseJSON(LEditor.Prop(CMenus));
  LAction := TNyxMenuEditorAction(Choice(AButton.Prop(CAction), CActions));
  { Metadata marks routing, but exact mounted identities authorize commands. }

  if not (LAction in [nmeMoveUp, nmeMoveDown, nmeRemoveItem]) and
    (AButton.ID <> NyxMenuEditorActionID(LEditorID, LAction)) then
  begin
    raise ENyxModel.Create('Unknown menu editor command identity');
  end;
  AChange.Action := LAction;
  AChange.Reference.Name := LEditor.Prop(CReference);

  if LAction in [nmeChoose, nmeAttach] then
  begin
    AChange.Reference := Menu(LEditorID, nmfDefinition, LAction = nmeChoose);
    Exit;
  end;

  if LAction in [nmeMask, nmeInherit] then
  begin
    AChange.Reference := Default(TNyxMenuRef);
    Exit;
  end;

  if LAction = nmeRemove then
  begin

    if (AChange.Reference.Name = '') or not Flag(LEditorID, nmfConfirm) then
    begin
      raise ENyxModel.Create('Review the visible menu removal warning and explicitly confirm first');
    end;
    Exit;
  end;

  if AChange.Reference.Name = '' then
  begin
    AChange.Reference := NyxMenuRef(Value(LEditorID, nmfName));
    for LIndex := 0 to LMenus.Count - 1 do
    begin

      if LMenus.Item(LIndex).AsText = AChange.Reference.Name then
      begin
        raise ENyxModel.Create('Open the saved definition to edit it, or choose a new name');
      end;
    end;
  end
  else if Value(LEditorID, nmfName) <> AChange.Reference.Name then
  begin
    raise ENyxModel.Create('Saved menu names cannot be renamed by a definition edit');
  end;
  LRoots := TNyxDataValue.ParseJSON(LEditor.Prop(CRoots));
  LSelected := -1;
  for LIndex := 0 to LRoots.Count - 1 do
  begin
    LRoot := NyxRoot(TNyxRootKind(LRoots.Item(LIndex).Field('kind').AsInteger),
      LRoots.Item(LIndex).Field('name').AsText);

    if Value(LEditorID, nmfRoot) = RootCaption(LIndex, LRoot) then
    begin
      LSelected := LIndex;
    end;
  end;

  if LSelected < 0 then
  begin
    raise ENyxModel.Create('Choose an available menu content root');
  end;
  LRoot := NyxRoot(TNyxRootKind(LRoots.Item(LSelected).Field('kind').AsInteger),
    LRoots.Item(LSelected).Field('name').AsText);
  LDismissals := [];

  if Flag(LEditorID, nmfEscape) then
  begin
    Include(LDismissals, npdEscape);
  end;

  if Flag(LEditorID, nmfOutside) then
  begin
    Include(LDismissals, npdOutsidePress);
  end;
  LPlacement := NyxPopover(Value(LEditorID, nmfTitle))
    .Placement(TNyxPopoverSide(Choice(Value(LEditorID, nmfSide), CSides)),
      TNyxPopoverAlignment(Choice(Value(LEditorID, nmfAlignment), CAlignments)))
    .Sizing(TNyxPopoverSizing(Choice(Value(LEditorID, nmfSizing), CSizings)))
    .Size(Number(nmfWidth, 16, 16384), Number(nmfHeight, 16, 16384))
    .Spacing(Number(nmfGap, 0, 4096), Number(nmfMargin, 0, 4096)).DismissOn(LDismissals);

  if Value(LEditorID, nmfFocus) <> '' then
  begin
    LPlacement := LPlacement.Focus(NyxPart(Value(LEditorID, nmfFocus)));
  end;
  LOptions := NyxMenu(LPlacement.Title).Presentation(LPlacement)
    .Opening(TNyxMenuOpening(Choice(Value(LEditorID, nmfOpening), COpenings)))
    .Wrap(Flag(LEditorID, nmfWrap))
    .TypeAhead(NyxTypeAhead.Enabled(Flag(LEditorID, nmfSearch))
      .WindowMilliseconds(Number(nmfSearchWindow, 1, 60000))
      .Match(TNyxTypeAheadMatch(Choice(Value(LEditorID, nmfSearchMatch), CMatches))));

  if not TryStrToInt(LEditor.Prop(CCount), LCount) or
    (LCount < 0) or (LCount > NyxMaximumMenuItems) then
  begin
    raise ENyxModel.Create('Menu editor requires its exact mounted item count');
  end;
  SetLength(LItems, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LItems[LIndex] := Item(NyxMenuEditorItemID(LEditorID, LIndex));
  end;

  if LAction = nmeAddItem then
  begin

    if LCount = NyxMaximumMenuItems then
    begin
      raise ENyxModel.Create('The saved menu item limit has been reached');
    end;
    SetLength(LItems, LCount + 1);
    LItems[LCount] := Item(LEditorID + TNyxText('-new-item'));
    Inc(LCount);
  end
  else if LAction in [nmeMoveUp, nmeMoveDown, nmeRemoveItem] then
  begin

    if not TryStrToInt(AButton.Prop(CIndex), LSelected) or
      (LSelected < 0) or (LSelected >= LCount) or
      (AButton.ID <> NyxMenuEditorActionID(NyxMenuEditorItemID(LEditorID, LSelected), LAction)) then
    begin
      raise ENyxModel.Create('Menu item command requires its exact mounted row');
    end;

    if LAction = nmeRemoveItem then
    begin
      for LIndex := LSelected to LCount - 2 do
      begin
        LItems[LIndex] := LItems[LIndex + 1];
      end;
      Dec(LCount);
      SetLength(LItems, LCount);
    end
    else
    begin
      LOther := LSelected - 1;

      if LAction = nmeMoveDown then
      begin
        LOther := LSelected + 1;
      end;

      if (LOther < 0) or (LOther >= LCount) then
      begin
        raise ENyxModel.Create('This menu item is already at that boundary');
      end;
      LTemporary := LItems[LOther];
      LItems[LOther] := LItems[LSelected];
      LItems[LSelected] := LTemporary;
    end;
  end;

  if LCount = 0 then
  begin
    raise ENyxModel.Create('A saved menu needs at least one item; use reviewed definition removal instead');
  end;
  { Merge immutable single-entry plans through public typed builders. No wire
    round-trip or foreign-interface retention substitutes for authored meaning. }
  ADefinition := NewNyxMenuDefinition(LRoot, LOptions);
  for LIndex := 0 to LCount - 1 do
  begin
    LTemporary := LItems[LIndex];
    case LTemporary.Item(0).Kind of
      nmiAction:
        ADefinition := ADefinition.Action(LTemporary.Item(0).Part,
          LTemporary.Item(0).Command, LTemporary.Item(0).IsEnabled);
      nmiCheck:
        ADefinition := ADefinition.Check(LTemporary.Item(0).Part,
          LTemporary.Item(0).Command, LTemporary.Item(0).IsChecked, LTemporary.Item(0).IsEnabled);
      nmiRadio:
        ADefinition := ADefinition.Radio(LTemporary.Item(0).Part,
          LTemporary.Item(0).Command, LTemporary.Item(0).Group,
          LTemporary.Item(0).IsChecked, LTemporary.Item(0).IsEnabled);
      nmiSeparator:
        ADefinition := ADefinition.Separator(LTemporary.Item(0).Part);
      nmiSubmenu:
        ADefinition := ADefinition.Submenu(LTemporary.Item(0).Part,
          LTemporary.Item(0).Submenu, LTemporary.Item(0).IsEnabled);
    end;
  end;
end;

end.
