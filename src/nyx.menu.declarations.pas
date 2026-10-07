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

unit nyx.menu.declarations;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.errors, nyx.data, nyx.root.types,
  nyx.menu.types, nyx.popover.types, nyx.typeahead;

const
  NyxMenusWireField = 'menus';
  NyxMenuAttachmentWireField = 'menu';
  NyxMaximumMenuDefinitions = 64;
  NyxMaximumMenuItems = 256;
  NyxMaximumMenuDepth = 8;
  NyxMaximumMenuFamilyItems = 2048;

type
  { Exact open application identity, distinct from a command, group, root or part.
    This value never denotes a target control or executable callback. }
  TNyxMenuRef = nyx.types.TNyxMenuRef;

  { Immutable authored entry. Initial check/enable values are saved defaults;
    each mounted presentation owns independent runtime copies. Submenu refers
    to another declaration in the same document, never to a borrowed document. }
  INyxMenuDeclarationItem = interface(IInterface)
    ['{06C00707-BA22-4531-9030-000000000001}']
    function GetKind: TNyxMenuItemKind;
    function GetPart: TNyxPartRef;
    function GetCommand: TNyxMenuCommandRef;
    function GetGroup: TNyxMenuGroupRef;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetSubmenu: TNyxMenuRef;
    property Kind: TNyxMenuItemKind read GetKind;
    property Part: TNyxPartRef read GetPart;
    property Command: TNyxMenuCommandRef read GetCommand;
    property Group: TNyxMenuGroupRef read GetGroup;
    property IsChecked: Boolean read GetChecked;
    property IsEnabled: Boolean read GetEnabled;
    property Submenu: TNyxMenuRef read GetSubmenu;
  end;

  { Immutable, fluent definition of existing specialized content. Every builder
    returns a new plan; previous plans and their ordering remain unchanged.
    Root/options are copied values. Reading a returned entry retains no tree.
    Empty intermediate plans are allowed; registry admission requires 1..256
    distinct parts and at most one initially checked radio per group. }
  INyxMenuDefinition = interface(IInterface)
    ['{06C00707-BA22-4531-9030-000000000002}']
    function GetRoot: TNyxRootRef;
    function GetOptions: TNyxMenuOptions;
    function GetCount: Integer;
    function Item(AIndex: Integer): INyxMenuDeclarationItem;
    function Action(const APart: TNyxPartRef; const ACommand: TNyxMenuCommandRef;
      AEnabled: Boolean = True): INyxMenuDefinition;
    function Check(const APart: TNyxPartRef; const ACommand: TNyxMenuCommandRef;
      AChecked: Boolean; AEnabled: Boolean = True): INyxMenuDefinition;
    function Radio(const APart: TNyxPartRef; const ACommand: TNyxMenuCommandRef;
      const AGroup: TNyxMenuGroupRef; AChecked: Boolean;
      AEnabled: Boolean = True): INyxMenuDefinition;
    function Separator(const APart: TNyxPartRef): INyxMenuDefinition;
    function Submenu(const APart: TNyxPartRef; const AMenu: TNyxMenuRef;
      AEnabled: Boolean = True): INyxMenuDefinition;
    function ToData: TNyxDataValue;
    property Root: TNyxRootRef read GetRoot;
    property Options: TNyxMenuOptions read GetOptions;
    property Count: Integer read GetCount;
  end;

  { Document-owned registry. Define normalizes even foreign implementations into
    immutable owned plans and replaces an existing name at its original position.
    Clone owns a new registry; definitions can safely outlive either owner.
    References may be forward-declared during a grouped edit. Validate admits the
    complete graph: missing branches, cycles, depth and expanded entry budgets
    refuse. The document separately owns/checks content roots and invokers. }
  INyxMenuDeclarations = interface(IInterface)
    ['{06C00707-BA22-4531-9030-000000000003}']
    function Define(const AReference: TNyxMenuRef;
      const ADefinition: INyxMenuDefinition): INyxMenuDeclarations;
    function Remove(const AReference: TNyxMenuRef): INyxMenuDeclarations;
    function Contains(const AReference: TNyxMenuRef): Boolean;
    function Definition(const AReference: TNyxMenuRef): INyxMenuDefinition;
    function Reference(AIndex: Integer): TNyxMenuRef;
    function GetCount: Integer;
    function Clone: INyxMenuDeclarations;
    procedure Validate;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
  end;

function NyxMenuRef(const AName: TNyxText): TNyxMenuRef;
function NewNyxMenuDefinition(const ARoot: TNyxRootRef;
  const AOptions: TNyxMenuOptions): INyxMenuDefinition;
function NewNyxMenuDeclarations: INyxMenuDeclarations;
{ Retarget independently owned content while preserving every ordered entry and
  policy. Used when a standalone reusable build promotes its root to a page. }
function CopyNyxMenuDefinitionToRoot(const ADefinition: INyxMenuDefinition;
  const ARoot: TNyxRootRef): INyxMenuDefinition;
{ Strict versioned transport boundaries, with closed spellings/native scalar
  kinds and complete refusal. Application authoring uses the typed builders. }
function NyxMenuDefinitionFromData(const AData: TNyxDataValue): INyxMenuDefinition;
function NyxMenuDeclarationsFromData(const AData: TNyxDataValue): INyxMenuDeclarations;

implementation

type
  TMenuEntry = class(TInterfacedObject, INyxMenuDeclarationItem)
  private
    FKind: TNyxMenuItemKind;
    FPart: TNyxPartRef;
    FCommand: TNyxMenuCommandRef;
    FGroup: TNyxMenuGroupRef;
    FChecked: Boolean;
    FEnabled: Boolean;
    FSubmenu: TNyxMenuRef;
  public
    function GetKind: TNyxMenuItemKind;
    function GetPart: TNyxPartRef;
    function GetCommand: TNyxMenuCommandRef;
    function GetGroup: TNyxMenuGroupRef;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetSubmenu: TNyxMenuRef;
  end;

  TMenuDefinition = class(TInterfacedObject, INyxMenuDefinition)
  private
    FRoot: TNyxRootRef;
    FOptions: TNyxMenuOptions;
    FItems: array of INyxMenuDeclarationItem;
    function Append(AKind: TNyxMenuItemKind; const APart: TNyxPartRef;
      const ACommand: TNyxMenuCommandRef; const AGroup: TNyxMenuGroupRef;
      AChecked, AEnabled: Boolean; const ASubmenu: TNyxMenuRef): INyxMenuDefinition;
  public
    constructor Create(const ARoot: TNyxRootRef; const AOptions: TNyxMenuOptions);
    function GetRoot: TNyxRootRef;
    function GetOptions: TNyxMenuOptions;
    function GetCount: Integer;
    function Item(AIndex: Integer): INyxMenuDeclarationItem;
    function Action(const APart: TNyxPartRef; const ACommand: TNyxMenuCommandRef;
      AEnabled: Boolean = True): INyxMenuDefinition;
    function Check(const APart: TNyxPartRef; const ACommand: TNyxMenuCommandRef;
      AChecked: Boolean; AEnabled: Boolean = True): INyxMenuDefinition;
    function Radio(const APart: TNyxPartRef; const ACommand: TNyxMenuCommandRef;
      const AGroup: TNyxMenuGroupRef; AChecked: Boolean;
      AEnabled: Boolean = True): INyxMenuDefinition;
    function Separator(const APart: TNyxPartRef): INyxMenuDefinition;
    function Submenu(const APart: TNyxPartRef; const AMenu: TNyxMenuRef;
      AEnabled: Boolean = True): INyxMenuDefinition;
    function ToData: TNyxDataValue;
  end;

  TMenuDeclarations = class(TInterfacedObject, INyxMenuDeclarations)
  private
    FReferences: array of TNyxMenuRef;
    FDefinitions: array of INyxMenuDefinition;
    function IndexOf(const AReference: TNyxMenuRef): Integer;
  public
    function Define(const AReference: TNyxMenuRef;
      const ADefinition: INyxMenuDefinition): INyxMenuDeclarations;
    function Remove(const AReference: TNyxMenuRef): INyxMenuDeclarations;
    function Contains(const AReference: TNyxMenuRef): Boolean;
    function Definition(const AReference: TNyxMenuRef): INyxMenuDefinition;
    function Reference(AIndex: Integer): TNyxMenuRef;
    function GetCount: Integer;
    function Clone: INyxMenuDeclarations;
    procedure Validate;
    function ToData: TNyxDataValue;
  end;

const
  CItemNames: array[TNyxMenuItemKind] of TNyxText =
    ('action', 'check', 'radio', 'separator', 'submenu');
  CSideNames: array[TNyxPopoverSide] of TNyxText = ('below', 'above', 'right', 'left');
  CAlignmentNames: array[TNyxPopoverAlignment] of TNyxText = ('start', 'center', 'end');
  CSizingNames: array[TNyxPopoverSizing] of TNyxText = ('content', 'fixed');
  COpeningNames: array[TNyxMenuOpening] of TNyxText = ('first', 'last');
  CMatchNames: array[TNyxTypeAheadMatch] of TNyxText = ('folded', 'exact');

function NyxMenuRef(const AName: TNyxText): TNyxMenuRef;
begin
  Result.Name := NyxNamedEvent(AName).Name;
end;

function NewNyxMenuDefinition(const ARoot: TNyxRootRef;
  const AOptions: TNyxMenuOptions): INyxMenuDefinition;
begin
  Result := TMenuDefinition.Create(ARoot, AOptions);
end;

function NewNyxMenuDeclarations: INyxMenuDeclarations;
begin
  Result := TMenuDeclarations.Create;
end;

function TMenuEntry.GetKind: TNyxMenuItemKind;
begin
  Result := FKind;
end;

function TMenuEntry.GetPart: TNyxPartRef;
begin
  Result := NyxPart(FPart.Name);
end;

function TMenuEntry.GetCommand: TNyxMenuCommandRef;
begin
  Result := Default(TNyxMenuCommandRef);
  Result.Name := FCommand.Name;
end;

function TMenuEntry.GetGroup: TNyxMenuGroupRef;
begin
  Result := Default(TNyxMenuGroupRef);
  Result.Name := FGroup.Name;
end;

function TMenuEntry.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TMenuEntry.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TMenuEntry.GetSubmenu: TNyxMenuRef;
begin
  Result := Default(TNyxMenuRef);
  Result.Name := FSubmenu.Name;
end;

constructor TMenuDefinition.Create(const ARoot: TNyxRootRef;
  const AOptions: TNyxMenuOptions);
begin
  inherited Create;
  FRoot := NyxRoot(ARoot.Kind, ARoot.Name);
  AOptions.Validate;
  { The typed data boundary also validates the title's Unicode bytes. }
  NyxData(AOptions.Placement.Title).Validate;
  FOptions := AOptions;
end;

function TMenuDefinition.GetRoot: TNyxRootRef;
begin
  Result := FRoot;
end;

function TMenuDefinition.GetOptions: TNyxMenuOptions;
begin
  Result := FOptions;
end;

function TMenuDefinition.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TMenuDefinition.Item(AIndex: Integer): INyxMenuDeclarationItem;
begin

  if (AIndex < 0) or (AIndex >= GetCount) then
  begin
    raise ENyxModel.Create('Menu declaration item is outside its plan');
  end;
  Result := FItems[AIndex];
end;

function TMenuDefinition.Append(AKind: TNyxMenuItemKind; const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef; const AGroup: TNyxMenuGroupRef;
  AChecked, AEnabled: Boolean; const ASubmenu: TNyxMenuRef): INyxMenuDefinition;
var
  LIndex: Integer;
  LItem: TMenuEntry;
  LItemOwner: INyxMenuDeclarationItem;
  LPlan: TMenuDefinition;
  LCount: Integer;
begin

  if (Ord(AKind) < Ord(Low(TNyxMenuItemKind))) or
    (Ord(AKind) > Ord(High(TNyxMenuItemKind))) or
    (GetCount >= NyxMaximumMenuItems) then
  begin
    raise ENyxModel.Create('Menu declaration has an unknown kind or too many items');
  end;
  NyxNamedEvent(APart.Name);
  for LIndex := 0 to GetCount - 1 do
  begin

    if FItems[LIndex].Part.Name = APart.Name then
    begin
      raise ENyxModel.Create('Menu declaration repeats a named part');
    end;

    if (AKind = nmiRadio) and AChecked and
      (FItems[LIndex].Kind = nmiRadio) and FItems[LIndex].IsChecked and
      (FItems[LIndex].Group.Name = AGroup.Name) then
    begin
      raise ENyxModel.Create('Menu radio group has two initial selections');
    end;
  end;
  LItem := TMenuEntry.Create;
  LItemOwner := LItem;
  LItem.FKind := AKind;
  LItem.FPart := NyxPart(APart.Name);
  LItem.FEnabled := AEnabled;
  LItem.FChecked := AChecked;

  if AKind in [nmiAction, nmiCheck, nmiRadio] then
  begin
    LItem.FCommand := NyxMenuCommand(ACommand.Name);
  end;

  if AKind = nmiRadio then
  begin
    LItem.FGroup := NyxMenuGroup(AGroup.Name);
  end;

  if AKind = nmiSubmenu then
  begin
    LItem.FSubmenu := NyxMenuRef(ASubmenu.Name);
  end;
  LPlan := TMenuDefinition.Create(FRoot, FOptions);
  Result := LPlan;
  { Capture the ordinal explicitly. Some pas2js versions treat a parameterless
    method used directly as an array index as a function object, not its result. }
  LCount := GetCount;
  SetLength(LPlan.FItems, LCount + 1);
  for LIndex := 0 to LCount - 1 do
  begin
    LPlan.FItems[LIndex] := FItems[LIndex];
  end;
  LPlan.FItems[LCount] := LItemOwner;
end;

function TMenuDefinition.Action(const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef; AEnabled: Boolean): INyxMenuDefinition;
begin
  Result := Append(nmiAction, APart, ACommand, Default(TNyxMenuGroupRef),
    False, AEnabled, Default(TNyxMenuRef));
end;

function TMenuDefinition.Check(const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef; AChecked, AEnabled: Boolean): INyxMenuDefinition;
begin
  Result := Append(nmiCheck, APart, ACommand, Default(TNyxMenuGroupRef),
    AChecked, AEnabled, Default(TNyxMenuRef));
end;

function TMenuDefinition.Radio(const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef; const AGroup: TNyxMenuGroupRef;
  AChecked, AEnabled: Boolean): INyxMenuDefinition;
begin
  Result := Append(nmiRadio, APart, ACommand, AGroup, AChecked, AEnabled,
    Default(TNyxMenuRef));
end;

function TMenuDefinition.Separator(const APart: TNyxPartRef): INyxMenuDefinition;
begin
  Result := Append(nmiSeparator, APart, Default(TNyxMenuCommandRef),
    Default(TNyxMenuGroupRef), False, False, Default(TNyxMenuRef));
end;

function TMenuDefinition.Submenu(const APart: TNyxPartRef; const AMenu: TNyxMenuRef;
  AEnabled: Boolean): INyxMenuDefinition;
begin
  Result := Append(nmiSubmenu, APart, Default(TNyxMenuCommandRef),
    Default(TNyxMenuGroupRef), False, AEnabled, AMenu);
end;

function OptionsData(const AOptions: TNyxMenuOptions): TNyxDataValue;
var
  LPlacement: TNyxPopoverOptions;
begin
  AOptions.Validate;
  LPlacement := AOptions.Placement;
  Result := NyxObject([
    NyxField('title', NyxData(LPlacement.Title)),
    NyxField('side', NyxData(CSideNames[LPlacement.Side])),
    NyxField('alignment', NyxData(CAlignmentNames[LPlacement.Alignment])),
    NyxField('sizing', NyxData(CSizingNames[LPlacement.SizeMode])),
    NyxField('width', NyxData(LPlacement.Width)),
    NyxField('height', NyxData(LPlacement.Height)),
    NyxField('gap', NyxData(LPlacement.Gap)),
    NyxField('margin', NyxData(LPlacement.Margin)),
    NyxField('focus', NyxData(LPlacement.InitialFocus.Name)),
    NyxField('escape', NyxData(npdEscape in LPlacement.Dismissals)),
    NyxField('outsidePress', NyxData(npdOutsidePress in LPlacement.Dismissals)),
    NyxField('opening', NyxData(COpeningNames[AOptions.OpenAt])),
    NyxField('wrap', NyxData(AOptions.Wraps)),
    NyxField('searchEnabled', NyxData(AOptions.Search.IsEnabled)),
    NyxField('searchWindowMS', NyxData(AOptions.Search.WindowMS)),
    NyxField('searchMatch', NyxData(CMatchNames[AOptions.Search.MatchMode]))]);
end;

function TMenuDefinition.ToData: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LItem: INyxMenuDeclarationItem;
begin
  SetLength(LItems, GetCount);
  for LIndex := 0 to GetCount - 1 do
  begin
    LItem := FItems[LIndex];
    LItems[LIndex] := NyxObject([
      NyxField('kind', NyxData(CItemNames[LItem.Kind])),
      NyxField('part', NyxData(LItem.Part.Name)),
      NyxField('command', NyxData(LItem.Command.Name)),
      NyxField('group', NyxData(LItem.Group.Name)),
      NyxField('checked', NyxData(LItem.IsChecked)),
      NyxField('enabled', NyxData(LItem.IsEnabled)),
      NyxField('submenu', NyxData(LItem.Submenu.Name))]);
  end;
  Result := NyxObject([
    NyxField('root', NyxObject([
      NyxField('kind', NyxData(NyxRootKindName(FRoot.Kind))),
      NyxField('name', NyxData(FRoot.Name))])),
    NyxField('options', OptionsData(FOptions)),
    NyxField('items', NyxArray(LItems))]);
end;

{ Reconstruct from public getters, never a foreign object's serialized claim.
  Closed-field meanings are checked before immutable entries are retained. }
function CopyDefinition(const ADefinition: INyxMenuDefinition): INyxMenuDefinition;
var
  LIndex: Integer;
  LCount: Integer;
  LItem: INyxMenuDeclarationItem;
  LKind: TNyxMenuItemKind;
  LPart: TNyxPartRef;
  LCommand: TNyxMenuCommandRef;
  LGroup: TNyxMenuGroupRef;
  LSubmenu: TNyxMenuRef;
  LChecked: Boolean;
  LEnabled: Boolean;
begin

  if ADefinition = nil then
  begin
    raise ENyxModel.Create('Menu definition is absent');
  end;
  LCount := ADefinition.Count;

  if (LCount < 1) or (LCount > NyxMaximumMenuItems) then
  begin
    raise ENyxModel.Create('Menu definition requires 1..256 entries');
  end;
  Result := NewNyxMenuDefinition(ADefinition.Root, ADefinition.Options);
  for LIndex := 0 to LCount - 1 do
  begin
    LItem := ADefinition.Item(LIndex);

    if LItem = nil then
    begin
      raise ENyxModel.Create('Menu definition returned an absent entry');
    end;
    { Capture every getter once before checking kind-specific meaning. A foreign
      implementation cannot smuggle contradictory defaults through normalization. }
    LKind := LItem.Kind;
    LPart := LItem.Part;
    LCommand := LItem.Command;
    LGroup := LItem.Group;
    LSubmenu := LItem.Submenu;
    LChecked := LItem.IsChecked;
    LEnabled := LItem.IsEnabled;

    if ((LKind <> nmiRadio) and (LGroup.Name <> '')) or
      ((LKind <> nmiSubmenu) and (LSubmenu.Name <> '')) or
      ((LKind in [nmiSeparator, nmiSubmenu]) and (LCommand.Name <> '')) or
      ((LKind in [nmiAction, nmiSeparator, nmiSubmenu]) and LChecked) or
      ((LKind = nmiSeparator) and LEnabled) then
    begin
      raise ENyxModel.Create('Menu entry contains fields belonging to another kind');
    end;
    case Ord(LKind) of
      Ord(nmiAction):
        Result := Result.Action(LPart, LCommand, LEnabled);
      Ord(nmiCheck):
        Result := Result.Check(LPart, LCommand, LChecked, LEnabled);
      Ord(nmiRadio):
        Result := Result.Radio(LPart, LCommand, LGroup, LChecked, LEnabled);
      Ord(nmiSeparator):
        Result := Result.Separator(LPart);
      Ord(nmiSubmenu):
        Result := Result.Submenu(LPart, LSubmenu, LEnabled);
      else
        raise ENyxModel.Create('Unknown menu declaration item kind');
    end;
  end;
end;

function CopyNyxMenuDefinitionToRoot(const ADefinition: INyxMenuDefinition;
  const ARoot: TNyxRootRef): INyxMenuDefinition;
var
  LSource: INyxMenuDefinition;
  LItem: INyxMenuDeclarationItem;
  LIndex: Integer;
begin
  LSource := CopyDefinition(ADefinition);
  Result := NewNyxMenuDefinition(ARoot, LSource.Options);
  for LIndex := 0 to LSource.Count - 1 do
  begin
    LItem := LSource.Item(LIndex);
    case LItem.Kind of
      nmiAction: Result := Result.Action(LItem.Part, LItem.Command, LItem.IsEnabled);
      nmiCheck: Result := Result.Check(LItem.Part, LItem.Command, LItem.IsChecked, LItem.IsEnabled);
      nmiRadio: Result := Result.Radio(LItem.Part, LItem.Command, LItem.Group,
        LItem.IsChecked, LItem.IsEnabled);
      nmiSeparator: Result := Result.Separator(LItem.Part);
      nmiSubmenu: Result := Result.Submenu(LItem.Part, LItem.Submenu, LItem.IsEnabled);
    end;
  end;
end;

function TMenuDeclarations.IndexOf(const AReference: TNyxMenuRef): Integer;
var
  LIndex: Integer;
begin
  NyxMenuRef(AReference.Name);
  for LIndex := 0 to GetCount - 1 do
  begin

    if FReferences[LIndex].Name = AReference.Name then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TMenuDeclarations.Define(const AReference: TNyxMenuRef;
  const ADefinition: INyxMenuDefinition): INyxMenuDeclarations;
var
  LPosition: Integer;
  LDefinition: INyxMenuDefinition;
begin
  LPosition := IndexOf(AReference);

  if (LPosition < 0) and (GetCount >= NyxMaximumMenuDefinitions) then
  begin
    raise ENyxModel.Create('Document has too many menu definitions');
  end;
  LDefinition := CopyDefinition(ADefinition);

  if LPosition < 0 then
  begin
    LPosition := GetCount;
    SetLength(FReferences, LPosition + 1);
    SetLength(FDefinitions, LPosition + 1);
    FReferences[LPosition] := NyxMenuRef(AReference.Name);
  end;
  FDefinitions[LPosition] := LDefinition;
  Result := Self;
end;

function TMenuDeclarations.Remove(const AReference: TNyxMenuRef): INyxMenuDeclarations;
var
  LPosition: Integer;
  LIndex: Integer;
begin
  LPosition := IndexOf(AReference);

  if LPosition < 0 then
  begin
    raise ENyxModel.Create('Menu definition is missing');
  end;
  for LIndex := LPosition to GetCount - 2 do
  begin
    FReferences[LIndex] := FReferences[LIndex + 1];
    FDefinitions[LIndex] := FDefinitions[LIndex + 1];
  end;
  SetLength(FReferences, GetCount - 1);
  SetLength(FDefinitions, Length(FReferences));
  Result := Self;
end;

function TMenuDeclarations.Contains(const AReference: TNyxMenuRef): Boolean;
begin
  Result := IndexOf(AReference) >= 0;
end;

function TMenuDeclarations.Definition(const AReference: TNyxMenuRef): INyxMenuDefinition;
var
  LPosition: Integer;
begin
  LPosition := IndexOf(AReference);

  if LPosition < 0 then
  begin
    raise ENyxModel.Create('Menu definition is missing');
  end;
  Result := FDefinitions[LPosition];
end;

function TMenuDeclarations.Reference(AIndex: Integer): TNyxMenuRef;
begin

  if (AIndex < 0) or (AIndex >= GetCount) then
  begin
    raise ENyxModel.Create('Menu declaration index is outside its registry');
  end;
  Result := NyxMenuRef(FReferences[AIndex].Name);
end;

function TMenuDeclarations.GetCount: Integer;
begin
  Result := Length(FDefinitions);
end;

function TMenuDeclarations.Clone: INyxMenuDeclarations;
var
  LIndex: Integer;
begin
  Result := NewNyxMenuDeclarations;
  for LIndex := 0 to GetCount - 1 do
  begin
    Result.Define(FReferences[LIndex], FDefinitions[LIndex]);
  end;
end;

procedure TMenuDeclarations.Validate;
var
  LIndex: Integer;
  LBudget: Integer;
  LAncestors: array[0..NyxMaximumMenuDepth - 1] of Integer;

  procedure Visit(APosition, ADepth: Integer);
  var
    LItemIndex: Integer;
    LAncestor: Integer;
    LChild: Integer;
    LDefinition: INyxMenuDefinition;
    LItem: INyxMenuDeclarationItem;
  begin

    if ADepth >= NyxMaximumMenuDepth then
    begin
      raise ENyxModel.Create('Menu family exceeds eight levels');
    end;
    for LAncestor := 0 to ADepth - 1 do
    begin

      if LAncestors[LAncestor] = APosition then
      begin
        raise ENyxModel.Create('Menu declarations contain a submenu cycle');
      end;
    end;
    LAncestors[ADepth] := APosition;
    LDefinition := FDefinitions[APosition];
    Inc(LBudget, LDefinition.Count);

    if LBudget > NyxMaximumMenuFamilyItems then
    begin
      raise ENyxModel.Create('Menu family exceeds 2048 expanded entries');
    end;
    for LItemIndex := 0 to LDefinition.Count - 1 do
    begin
      LItem := LDefinition.Item(LItemIndex);

      if LItem.Kind = nmiSubmenu then
      begin
        LChild := IndexOf(LItem.Submenu);

        if LChild < 0 then
        begin
          raise ENyxModel.Create('Menu declaration has an unresolved submenu');
        end;
        Visit(LChild, ADepth + 1);
      end;
    end;
  end;

begin
  for LIndex := 0 to GetCount - 1 do
  begin
    LBudget := 0;
    Visit(LIndex, 0);
  end;
end;

function TMenuDeclarations.ToData: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  Validate;
  SetLength(LItems, GetCount);
  for LIndex := 0 to GetCount - 1 do
  begin
    LItems[LIndex] := NyxObject([
      NyxField('name', NyxData(FReferences[LIndex].Name)),
      NyxField('definition', FDefinitions[LIndex].ToData)]);
  end;
  Result := NyxObject([
    NyxField('version', NyxData(1)), NyxField('definitions', NyxArray(LItems))]);
end;

{ Resolve only a closed wire spelling; never cast a supplied integer into an
  enum. Native booleans and exact integer reads refuse string coercion. }
function Choice(const AValue: TNyxDataValue; const ANames: array of TNyxText): Integer;
begin
  for Result := 0 to High(ANames) do
  begin

    if ANames[Result] = AValue.AsText then
    begin
      Exit;
    end;
  end;
  raise ENyxModel.Create('Unknown closed menu policy choice');
end;

function OptionsFromData(const AData: TNyxDataValue): TNyxMenuOptions;
var
  LPlacement: TNyxPopoverOptions;
  LDismissals: TNyxPopoverDismissals;
  LSearch: TNyxTypeAheadOptions;
  LFocus: TNyxText;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 16) then
  begin
    raise ENyxModel.Create('Menu options require exactly the typed policy fields');
  end;
  LDismissals := [];

  if AData.Field('escape').AsBoolean then
  begin
    Include(LDismissals, npdEscape);
  end;

  if AData.Field('outsidePress').AsBoolean then
  begin
    Include(LDismissals, npdOutsidePress);
  end;
  LPlacement := NyxPopover(AData.Field('title').AsText)
    .Placement(TNyxPopoverSide(Choice(AData.Field('side'), CSideNames)),
      TNyxPopoverAlignment(Choice(AData.Field('alignment'), CAlignmentNames)))
    .Sizing(TNyxPopoverSizing(Choice(AData.Field('sizing'), CSizingNames)))
    .Size(AData.Field('width').AsInteger, AData.Field('height').AsInteger)
    .Spacing(AData.Field('gap').AsInteger, AData.Field('margin').AsInteger)
    .DismissOn(LDismissals);
  LFocus := AData.Field('focus').AsText;

  if LFocus <> '' then
  begin
    LPlacement := LPlacement.Focus(NyxPart(LFocus));
  end;
  LSearch := NyxTypeAhead.Enabled(AData.Field('searchEnabled').AsBoolean)
    .WindowMilliseconds(AData.Field('searchWindowMS').AsInteger)
    .Match(TNyxTypeAheadMatch(Choice(AData.Field('searchMatch'), CMatchNames)));
  Result := NyxMenu(LPlacement.Title).Presentation(LPlacement).TypeAhead(LSearch)
    .Opening(TNyxMenuOpening(Choice(AData.Field('opening'), COpeningNames)))
    .Wrap(AData.Field('wrap').AsBoolean);
end;

function NyxMenuDefinitionFromData(const AData: TNyxDataValue): INyxMenuDefinition;
var
  LRoot: TNyxDataValue;
  LItems: TNyxDataValue;
  LItem: TNyxDataValue;
  LPart: TNyxPartRef;
  LCommand: TNyxMenuCommandRef;
  LGroup: TNyxMenuGroupRef;
  LSubmenu: TNyxMenuRef;
  LKind: TNyxMenuItemKind;
  LIndex: Integer;
  LChecked: Boolean;
  LEnabled: Boolean;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 3) then
  begin
    raise ENyxModel.Create('Menu definition requires root/options/items');
  end;
  LRoot := AData.Field('root');

  if (LRoot.Kind <> ndObject) or (LRoot.Count <> 2) then
  begin
    raise ENyxModel.Create('Menu root requires kind/name');
  end;
  Result := NewNyxMenuDefinition(NyxRoot(
    TNyxRootKind(Choice(LRoot.Field('kind'), ['page', 'component'])),
    LRoot.Field('name').AsText), OptionsFromData(AData.Field('options')));
  LItems := AData.Field('items');

  if (LItems.Kind <> ndArray) or (LItems.Count < 1) or
    (LItems.Count > NyxMaximumMenuItems) then
  begin
    raise ENyxModel.Create('Menu definition requires 1..256 ordered entries');
  end;
  for LIndex := 0 to LItems.Count - 1 do
  begin
    LItem := LItems.Item(LIndex);

    if (LItem.Kind <> ndObject) or (LItem.Count <> 7) then
    begin
      raise ENyxModel.Create('Menu item requires its exact typed fields');
    end;
    LKind := TNyxMenuItemKind(Choice(LItem.Field('kind'), CItemNames));
    LPart := NyxPart(LItem.Field('part').AsText);
    LCommand.Name := LItem.Field('command').AsText;
    LGroup.Name := LItem.Field('group').AsText;
    LSubmenu.Name := LItem.Field('submenu').AsText;
    LChecked := LItem.Field('checked').AsBoolean;
    LEnabled := LItem.Field('enabled').AsBoolean;

    if ((LKind <> nmiRadio) and (LGroup.Name <> '')) or
      ((LKind <> nmiSubmenu) and (LSubmenu.Name <> '')) or
      ((LKind in [nmiSeparator, nmiSubmenu]) and (LCommand.Name <> '')) or
      ((LKind in [nmiAction, nmiSeparator, nmiSubmenu]) and LChecked) or
      ((LKind = nmiSeparator) and LEnabled) then
    begin
      raise ENyxModel.Create('Menu entry contains fields belonging to another kind');
    end;
    case LKind of
      nmiAction: Result := Result.Action(LPart, LCommand, LEnabled);
      nmiCheck: Result := Result.Check(LPart, LCommand, LChecked, LEnabled);
      nmiRadio: Result := Result.Radio(LPart, LCommand, LGroup, LChecked, LEnabled);
      nmiSeparator: Result := Result.Separator(LPart);
      nmiSubmenu: Result := Result.Submenu(LPart, LSubmenu, LEnabled);
    end;
  end;
end;

function NyxMenuDeclarationsFromData(const AData: TNyxDataValue): INyxMenuDeclarations;
var
  LDefinitions: TNyxDataValue;
  LEntry: TNyxDataValue;
  LReference: TNyxMenuRef;
  LIndex: Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 2) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Menu declarations require version/definitions');
  end;
  LDefinitions := AData.Field('definitions');

  if (LDefinitions.Kind <> ndArray) or (LDefinitions.Count > NyxMaximumMenuDefinitions) then
  begin
    raise ENyxModel.Create('Menu declarations require a bounded definition array');
  end;
  Result := NewNyxMenuDeclarations;
  for LIndex := 0 to LDefinitions.Count - 1 do
  begin
    LEntry := LDefinitions.Item(LIndex);

    if (LEntry.Kind <> ndObject) or (LEntry.Count <> 2) then
    begin
      raise ENyxModel.Create('Menu registry entry requires name/definition');
    end;
    LReference := NyxMenuRef(LEntry.Field('name').AsText);

    if Result.Contains(LReference) then
    begin
      raise ENyxModel.Create('Menu registry repeats a definition name');
    end;
    Result.Define(LReference, NyxMenuDefinitionFromData(LEntry.Field('definition')));
  end;
  Result.Validate;
end;

end.
