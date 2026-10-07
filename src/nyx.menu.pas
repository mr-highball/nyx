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

unit nyx.menu;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.controls, nyx.model, nyx.behavior,
  nyx.events, nyx.scheduler, nyx.popover, nyx.typeahead, nyx.root.types,
  nyx.menu.types, nyx.menu.declarations;

const
  nmiAction = nyx.menu.types.nmiAction;
  nmiCheck = nyx.menu.types.nmiCheck;
  nmiRadio = nyx.menu.types.nmiRadio;
  nmiSeparator = nyx.menu.types.nmiSeparator;
  nmiSubmenu = nyx.menu.types.nmiSubmenu;
  nmoFirst = nyx.menu.types.nmoFirst;
  nmoLast = nyx.menu.types.nmoLast;

type
  INyxMenuRecipe = interface;
  INyxMenuItem = interface;
  TNyxMenuCommandRef = nyx.menu.types.TNyxMenuCommandRef;
  TNyxMenuGroupRef = nyx.menu.types.TNyxMenuGroupRef;
  TNyxMenuItemKind = nyx.menu.types.TNyxMenuItemKind;
  TNyxMenuOpening = nyx.menu.types.TNyxMenuOpening;

  { Immutable registration of an existing named Nyx part. Disabled commands
    remain keyboard-focusable; physical Enabled=False is deliberately distinct.
    Check/radio state belongs to this managed runtime, not document defaults. }
  INyxMenuItem = interface(IInterface)
    ['{1A8B0719-0647-444E-A241-061026000006}']
    function GetPart: TNyxPartRef;
    function GetCommand: TNyxMenuCommandRef;
    function GetGroup: TNyxMenuGroupRef;
    function GetKind: TNyxMenuItemKind;
    function GetEnabled: Boolean;
    function GetChecked: Boolean;
    function GetRecipe: INyxMenuRecipe;
    { Return a detached choice; disabling preserves keyboard eligibility. }
    function Enabled(AValue: Boolean): INyxMenuItem;
    property Part: TNyxPartRef read GetPart;
    property Command: TNyxMenuCommandRef read GetCommand;
    property Group: TNyxMenuGroupRef read GetGroup;
    property Kind: TNyxMenuItemKind read GetKind;
    property IsEnabled: Boolean read GetEnabled;
    property IsChecked: Boolean read GetChecked;
    property Recipe: INyxMenuRecipe read GetRecipe;
  end;
  { Source-compatible names for the original value-plan spelling. Both now
    denote specialized reference-counted immutable contracts on both targets. }
  TNyxMenuItem = INyxMenuItem;

  { Immutable managed ordered plan, maximum 256 entries. Add copies every item
    into a new plan; prior plans stay unchanged on both compilers. Missing/duplicate parts,
    invalid groups and multiple initially checked radios refuse before opening. }
  INyxMenuItems = interface(IInterface)
    ['{1A8B0719-0647-444E-A241-061026000007}']
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TNyxMenuItem;
    function Add(const AItem: TNyxMenuItem): INyxMenuItems;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TNyxMenuItem read GetItem; default;
  end;
  TNyxMenuItems = INyxMenuItems;

  { Immutable owned submenu content and plan. CopyDocument returns a caller-owned
    independent clone. Implementations must preserve their construction snapshot;
    presenters validate every branch before opening any host. Eight levels and
    2048 total entries bound recipes, including external/cyclic implementations. }
  { Optional authored policy. Legacy runtime recipes inherit the parent's branch
    navigation/geometry. Document declarations carry their own copied policy;
    presenters consult this separate interface without changing foreign recipes. }
  INyxMenuRecipePolicy = interface(IInterface)
    ['{06C00707-BA22-4531-9030-000000000004}']
    function GetHasPolicy: Boolean;
    function GetPolicy: TNyxMenuOptions;
    property HasPolicy: Boolean read GetHasPolicy;
    property Policy: TNyxMenuOptions read GetPolicy;
  end;

  INyxMenuRecipe = interface(IInterface)
    ['{1A8B0719-0647-444E-A241-061026000004}']
    function CopyDocument: TNyxDocument;
    function GetRoot: TNyxRootRef;
    function GetItems: TNyxMenuItems;
    property Root: TNyxRootRef read GetRoot;
    property Items: TNyxMenuItems read GetItems;
  end;

  TNyxMenuOptions = nyx.menu.types.TNyxMenuOptions;

  { Detached completion snapshot survives close/reopen and queued callbacks. }
  TNyxMenuInvocation = record
    Command: TNyxMenuCommandRef;
    Part: TNyxPartRef;
    HasChecked: Boolean;
    Checked: Boolean;
  end;

  { Ref-counted command menu on public Nyx content/presentation. It owns its
    independent document through the popover. No callback retains the menu back.
    All operations require the UI thread. Content is specialized managed Nyx
    controls; configure parts before first Open. Buttons must have no renderer
    Action: dispatch commands through OnInvoke so disabled choices cannot run a
    renderer default before admission. Activation closes and returns focus
    before ordered callbacks. Space toggles check/radio items without closing;
    Enter activates and closes. Separators never receive focus. }
  INyxMenu = interface(IInterface)
    ['{1A8B0719-0647-444E-A241-061026000001}']
    function GetContent: INyxControl;
    function GetEvents: INyxEvents;
    function GetOpen: Boolean;
    function GetFocused: TNyxPartRef;
    function OnInvoke: INyxEventStream;
    function OnDismiss: INyxEventStream;
    { Specialized content access. Foreign/separator parts raise ENyxModel. }
    function Button(const APart: TNyxPartRef): INyxButton;
    { Validate and mount; an already open host or unavailable invoker refuses. }
    procedure Open(const AOptions: TNyxMenuOptions);
    { Silent idempotent close, returning focus when the invoker is available. }
    procedure Close;
    { Update logical admission/appearance without modifying document defaults. }
    procedure SetEnabled(const APart: TNyxPartRef; AValue: Boolean);
    { Current independent check/radio state; action/separator/foreign parts raise. }
    function Checked(const APart: TNyxPartRef): Boolean;
    { Open the enabled named branch; lazily clone its independent content. A
      parent must be open. Disabled branches do nothing. Observe an already
      created child with Submenu; nil means it has never been mounted. }
    procedure OpenSubmenu(const APart: TNyxPartRef);
    function Submenu(const APart: TNyxPartRef): INyxMenu;
    property Content: INyxControl read GetContent;
    property Events: INyxEvents read GetEvents;
    property IsOpen: Boolean read GetOpen;
    property Focused: TNyxPartRef read GetFocused;
  end;

  TNyxMenuPresenter = class;
  { Internal weak callback lease; adapters never retain an owner through it. }
  TNyxMenuLease = class(TInterfacedObject)
  public
    Owner: TNyxMenuPresenter;
  end;
  { Adapter seam owns the popover and weak callback lease. Subclasses only map
    focus, item semantics and monotonic text input to physical target controls. }
  TNyxMenuPresenter = class(TInterfacedObject, INyxMenu)
  private
    FPopover: INyxPopover;
    FItems: TNyxMenuItems;
    { Borrowed implementation pointer to this presenter's private copied plan.
      FItems owns it; only internal check/enable changes use this pointer. }
    FMutableItems: TObject;
    FCompletion: INyxEvents;
    FLease: TNyxMenuLease;
    FLeaseOwner: IInterface;
    FSubscriptions: array of INyxEventSubscription;
    FOptions: TNyxMenuOptions;
    FSearch: INyxTypeAhead;
    FFocused: Integer;
    FChildren: array of TNyxMenuPresenter;
    FChildOwners: array of INyxMenu;
    FParentLease: TNyxMenuLease;
    FParentOwner: IInterface;
    FReady: Boolean;
    procedure ValidateCommand(const APart: INyxControl);
    function ItemIndex(const APart: TNyxPartRef): Integer;
    function LabelAt(AIndex: Integer): TNyxText;
    function Visible(AIndex: Integer): Boolean;
    function Boundary(ALast: Boolean): Integer;
    procedure Focus(AIndex: Integer);
    procedure Activate(AIndex: Integer; AClose: Boolean);
    procedure Input(AIndex: Integer; const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution);
    procedure ChildInvoke(const AEvent: TNyxEventInfo; AIndex: Integer);
    procedure Dismissed(const AEvent: TNyxEventInfo);
    procedure CloseChildren(AExcept: Integer = -1);
    function FamilyRoot: TNyxMenuPresenter;
  protected
    procedure ApplyFaces; virtual; abstract;
    function FocusFace(AIndex: Integer): Boolean; virtual; abstract;
    { Called after Close restores the invoker. Browser leaves native Tab default;
      LCL explicitly traverses the restored invoker's container and consumes it. }
    procedure TabExit(AReverse: Boolean; const AExecution: INyxExecution); virtual; abstract;
    function TextInput(const AText: TNyxText; ATimeMS: Double): Boolean;
    function ItemID(AIndex: Integer): TNyxText;
    function BranchOpen(AIndex: Integer): Boolean;
    { Child factories borrow a mounted parent face; their popover family keeps
      native outside presses and browser descendant semantics coherent. }
    function CreateSubmenu(AIndex: Integer; const ARecipe: INyxMenuRecipe):
      TNyxMenuPresenter; virtual; abstract;
    procedure PresentationChanged; virtual;
    property Popover: INyxPopover read FPopover;
    property Plan: TNyxMenuItems read FItems;
  public
    constructor Create(const APopover: INyxPopover; const AItems: TNyxMenuItems);
    destructor Destroy; override;
    function GetContent: INyxControl;
    function GetEvents: INyxEvents;
    function GetOpen: Boolean;
    function GetFocused: TNyxPartRef;
    function OnInvoke: INyxEventStream;
    function OnDismiss: INyxEventStream;
    function Button(const APart: TNyxPartRef): INyxButton;
    procedure Open(const AOptions: TNyxMenuOptions);
    procedure Close;
    procedure SetEnabled(const APart: TNyxPartRef; AValue: Boolean);
    function Checked(const APart: TNyxPartRef): Boolean;
    procedure OpenSubmenu(const APart: TNyxPartRef);
    function Submenu(const APart: TNyxPartRef): INyxMenu;
    property Content: INyxControl read GetContent;
  end;

{ Open names use the shared bounded Unicode event-identity admission. }
function NyxMenuCommand(const AName: TNyxText): TNyxMenuCommandRef;
function NyxMenuGroup(const AName: TNyxText): TNyxMenuGroupRef;
function NyxMenuAction(const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef): TNyxMenuItem;
function NyxMenuCheck(const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef; AChecked: Boolean): TNyxMenuItem;
function NyxMenuRadio(const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef; const AGroup: TNyxMenuGroupRef;
  AChecked: Boolean): TNyxMenuItem;
function NyxMenuSeparator(const APart: TNyxPartRef): TNyxMenuItem;
function NyxMenuSubmenu(const APart: TNyxPartRef;
  const ARecipe: INyxMenuRecipe): TNyxMenuItem;
{ Clone the document immediately; caller can release it after construction. }
function NewNyxMenuRecipe(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
  const AItems: TNyxMenuItems): INyxMenuRecipe; overload;
{ Capture complete authored policy as well as the independently owned recipe. }
function NewNyxMenuRecipe(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
  const AItems: TNyxMenuItems; const AOptions: TNyxMenuOptions): INyxMenuRecipe; overload;
{ Resolve a saved graph into immutable runtime recipes. Every branch owns its
  snapshot, initial state and policy. Later source edits cannot alter this family;
  missing roots/parts, cycles or budget overflow refuse before any host mounts. }
function NewNyxDeclaredMenuRecipe(ADocument: TNyxDocument;
  const AReference: TNyxMenuRef): INyxMenuRecipe;
function NyxMenuItems: TNyxMenuItems;
function NyxMenu(const ATitle: TNyxText): TNyxMenuOptions;
{ Validate/copy the exact private completion payload. Unrelated events and
  malformed snapshots raise ENyxModel or the typed data admission exception. }
function NyxMenuInvocation(const AEvent: TNyxEventInfo): TNyxMenuInvocation;

implementation

uses nyx.data, nyx.interaction, nyx.composition;

type
  { Owned classes avoid unsupported COM-interface record fields in pas2js.
    Only a presenter's independent copies are mutable; public plans are sealed. }
  TMenuItem = class(TInterfacedObject, INyxMenuItem)
  private
    FPart: TNyxPartRef;
    FCommand: TNyxMenuCommandRef;
    FGroup: TNyxMenuGroupRef;
    FKind: TNyxMenuItemKind;
    FEnabled: Boolean;
    FChecked: Boolean;
    FRecipe: INyxMenuRecipe;
  public
    constructor Create(const AItem: INyxMenuItem = nil);
    function GetPart: TNyxPartRef;
    function GetCommand: TNyxMenuCommandRef;
    function GetGroup: TNyxMenuGroupRef;
    function GetKind: TNyxMenuItemKind;
    function GetEnabled: Boolean;
    function GetChecked: Boolean;
    function GetRecipe: INyxMenuRecipe;
    function Enabled(AValue: Boolean): INyxMenuItem;
  end;
  TMenuItems = class(TInterfacedObject, INyxMenuItems)
  private
    FItems: array of INyxMenuItem;
    FObjects: array of TMenuItem;
    procedure Append(const AItem: INyxMenuItem);
  public
    constructor Create(const AItems: INyxMenuItems = nil);
    function GetCount: Integer;
    function GetItem(AIndex: Integer): INyxMenuItem;
    function Add(const AItem: INyxMenuItem): INyxMenuItems;
  end;
  TMenuRecipe = class(TInterfacedObject, INyxMenuRecipe, INyxMenuRecipePolicy)
  private
    FDocument: TNyxDocument;
    FRoot: TNyxRootRef;
    FItems: TNyxMenuItems;
    FHasPolicy: Boolean;
    FPolicy: TNyxMenuOptions;
  public
    constructor Create(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
      const AItems: TNyxMenuItems);
    destructor Destroy; override;
    function CopyDocument: TNyxDocument;
    function GetRoot: TNyxRootRef;
    function GetItems: TNyxMenuItems;
    function GetHasPolicy: Boolean;
    function GetPolicy: TNyxMenuOptions;
  end;
  { Borrowed controller pointer lives behind the same weak menu lease. }
  TMenuCompletion = class(TNyxEventCallback)
  private
    FLease: TNyxMenuLease;
    FOwner: IInterface;
    FIndex: Integer;
  public
    constructor Create(ALease: TNyxMenuLease; AIndex: Integer);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  { Queued/externally retained callbacks keep this lease, never their owner.
    Retiring the owner clears the pointer before cancellation and host teardown. }
  TMenuInput = class(TNyxEventCallback)
  private
    FLease: TNyxMenuLease;
    FOwner: IInterface;
    FIndex: Integer;
  public
    constructor Create(ALease: TNyxMenuLease; AIndex: Integer);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

constructor TMenuInput.Create(ALease: TNyxMenuLease; AIndex: Integer);
begin
  inherited Create;
  FLease := ALease;
  FOwner := ALease;
  FIndex := AIndex;
end;

procedure TMenuInput.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LKeepAlive: INyxMenu;
begin

  if FLease.Owner <> nil then
  begin
    LKeepAlive := FLease.Owner;
    FLease.Owner.Input(FIndex, AEvent, AExecution);
    LKeepAlive.GetOpen;
  end;
end;

function NyxMenuCommand(const AName: TNyxText): TNyxMenuCommandRef;
begin
  Result := nyx.menu.types.NyxMenuCommand(AName);
end;

function NyxMenuGroup(const AName: TNyxText): TNyxMenuGroupRef;
begin
  Result := nyx.menu.types.NyxMenuGroup(AName);
end;

function NyxMenuAction(const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef): TNyxMenuItem;
var
  LItem: TMenuItem;
begin
  NyxMenuCommand(ACommand.Name);
  LItem := TMenuItem.Create;
  Result := LItem;
  LItem.FPart := APart;
  LItem.FCommand := ACommand;
  LItem.FEnabled := True;
end;

function NyxMenuCheck(const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef; AChecked: Boolean): TNyxMenuItem;
var
  LItem: TMenuItem;
begin
  LItem := TMenuItem.Create(NyxMenuAction(APart, ACommand));
  Result := LItem;
  LItem.FKind := nmiCheck;
  LItem.FChecked := AChecked;
end;

function NyxMenuRadio(const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef; const AGroup: TNyxMenuGroupRef;
  AChecked: Boolean): TNyxMenuItem;
var
  LItem: TMenuItem;
begin
  NyxMenuGroup(AGroup.Name);
  LItem := TMenuItem.Create(NyxMenuCheck(APart, ACommand, AChecked));
  Result := LItem;
  LItem.FKind := nmiRadio;
  LItem.FGroup := AGroup;
end;

function NyxMenuSeparator(const APart: TNyxPartRef): TNyxMenuItem;
var
  LItem: TMenuItem;
begin
  LItem := TMenuItem.Create;
  Result := LItem;
  LItem.FPart := APart;
  LItem.FKind := nmiSeparator;
end;

function NyxMenuSubmenu(const APart: TNyxPartRef;
  const ARecipe: INyxMenuRecipe): TNyxMenuItem;
var
  LItem: TMenuItem;
begin

  if ARecipe = nil then
  begin
    raise ENyxModel.Create('Submenu requires an immutable content recipe');
  end;
  LItem := TMenuItem.Create;
  Result := LItem;
  LItem.FPart := APart;
  LItem.FKind := nmiSubmenu;
  LItem.FEnabled := True;
  LItem.FRecipe := ARecipe;
end;

function TMenuItem.GetRecipe: INyxMenuRecipe;
begin
  Result := FRecipe;
end;

procedure ValidateMenuTree(const AContent: INyxControl; const AItems: TNyxMenuItems;
  ADepth: Integer; var ABudget: Integer);
var
  LIndex: Integer;
  LPart: INyxControl;
  LDocument: TNyxDocument;
  LNode: TNyxNode;
  LContent: INyxControl;
  LAction: TNyxAction;
  LOther: Integer;
begin

  if AItems = nil then
  begin
    raise ENyxModel.Create('Menu recipe requires an item plan');
  end;
  if (AItems.Count < 1) or (AItems.Count > 256) then
  begin
    raise ENyxModel.Create('Menu recipe requires 1..256 items');
  end;
  Dec(ABudget, AItems.Count);

  if (ADepth > 8) or (ABudget < 0) then
  begin
    raise ENyxModel.Create('Menu recipe exceeds depth/entry bounds or has no items');
  end;
  for LIndex := 0 to AItems.Count - 1 do
  begin

    if AItems[LIndex] = nil then
    begin
      raise ENyxModel.Create('Menu recipe returned an absent item');
    end;

    if (Ord(AItems[LIndex].Kind) < Ord(Low(TNyxMenuItemKind))) or
      (Ord(AItems[LIndex].Kind) > Ord(High(TNyxMenuItemKind))) then
    begin
      raise ENyxModel.Create('Menu recipe returned an unknown item kind');
    end;
    for LOther := 0 to LIndex - 1 do
    begin

      if AItems[LOther].Part.Name = AItems[LIndex].Part.Name then
      begin
        raise ENyxModel.Create('Menu recipe repeats a named part');
      end;
    end;
    LPart := AContent.Part(AItems[LIndex].Part);

    if AItems[LIndex].Kind = nmiSeparator then
    begin

      if LPart.Kind <> NyxKindName(nkSeparator) then
      begin
        raise ENyxModel.Create('Menu separator part must be a Nyx separator');
      end;
      Continue;
    end;

    if (LPart.Kind <> NyxKindName(nkButton)) or
      not NyxInteractionPolicy(LPart.Node).Enabled then
    begin
      raise ENyxModel.Create('Menu commands require physically enabled Nyx buttons');
    end;

    if not TryNyxAction(LPart.Node.Prop(NyxAttributeName(atAction)), LAction) or
      (LAction <> naNone) then
    begin
      raise ENyxModel.Create('Menu buttons use OnInvoke instead of a renderer action');
    end;

    if AItems[LIndex].Kind = nmiSubmenu then
    begin

      if AItems[LIndex].Recipe = nil then
      begin
        raise ENyxModel.Create('Submenu recipe is absent');
      end;
      LDocument := AItems[LIndex].Recipe.CopyDocument;
      LNode := nil;
      try

        if LDocument = nil then
        begin
          raise ENyxModel.Create('Submenu recipe returned no owned document');
        end;
        LDocument.Validate;
        if LDocument.FindRoot(AItems[LIndex].Recipe.Root) = nil then
        begin
          raise ENyxModel.Create('Submenu recipe root is absent');
        end;
        LNode := RealizeNyxView(LDocument, LDocument.FindRoot(AItems[LIndex].Recipe.Root));
        LContent := RetainNyxControl(LNode);
        ValidateMenuTree(LContent, AItems[LIndex].Recipe.Items,
          ADepth + 1, ABudget);
      finally
        LContent := nil;
        LNode.Free;
        LDocument.Free;
      end;
    end
    else
    begin
      NyxMenuCommand(AItems[LIndex].Command.Name);

      if AItems[LIndex].Kind = nmiRadio then
      begin
        NyxMenuGroup(AItems[LIndex].Group.Name);
        for LOther := 0 to LIndex - 1 do
        begin

          if AItems[LIndex].IsChecked and AItems[LOther].IsChecked and
            (AItems[LOther].Kind = nmiRadio) and
            (AItems[LIndex].Group.Name = AItems[LOther].Group.Name) then
          begin
            raise ENyxModel.Create('Radio menu group has more than one initial selection');
          end;
        end;
      end;
    end;
  end;
end;

constructor TMenuRecipe.Create(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
  const AItems: TNyxMenuItems);
var
  LBudget: Integer;
  LRuntime: TNyxNode;
  LContent: INyxControl;
begin
  inherited Create;

  if (ADocument = nil) or (ADocument.FindRoot(ARoot) = nil) then
  begin
    raise ENyxModel.Create('Menu recipe requires an exact owned document root');
  end;
  FDocument := ADocument.Clone;
  FRoot := ARoot;
  FItems := TMenuItems.Create(AItems);
  LBudget := 2048;
  LRuntime := RealizeNyxView(FDocument, FDocument.FindRoot(FRoot));
  try
    LContent := RetainNyxControl(LRuntime);
    ValidateMenuTree(LContent, FItems, 1, LBudget);
  finally
    LContent := nil;
    LRuntime.Free;
  end;
end;

destructor TMenuRecipe.Destroy;
begin
  FDocument.Free;
  inherited Destroy;
end;

function TMenuRecipe.CopyDocument: TNyxDocument;
begin
  Result := FDocument.Clone;
end;

function TMenuRecipe.GetRoot: TNyxRootRef;
begin
  Result := FRoot;
end;

function TMenuRecipe.GetItems: TNyxMenuItems;
begin
  Result := FItems;
end;

function NewNyxMenuRecipe(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
  const AItems: TNyxMenuItems): INyxMenuRecipe;
begin
  Result := TMenuRecipe.Create(ADocument, ARoot, AItems);
end;

function TMenuRecipe.GetHasPolicy: Boolean;
begin
  Result := FHasPolicy;
end;

function TMenuRecipe.GetPolicy: TNyxMenuOptions;
begin

  if not FHasPolicy then
  begin
    raise ENyxModel.Create('This runtime recipe inherits its branch policy');
  end;
  Result := FPolicy;
end;

function NewNyxMenuRecipe(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
  const AItems: TNyxMenuItems; const AOptions: TNyxMenuOptions): INyxMenuRecipe;
var
  LRecipe: TMenuRecipe;
begin
  AOptions.Validate;
  LRecipe := TMenuRecipe.Create(ADocument, ARoot, AItems);
  Result := LRecipe;
  LRecipe.FPolicy := AOptions;
  LRecipe.FHasPolicy := True;
end;

function NewNyxDeclaredMenuRecipe(ADocument: TNyxDocument;
  const AReference: TNyxMenuRef): INyxMenuRecipe;

  function Cook(const AName: TNyxMenuRef): INyxMenuRecipe;
  var
    LDefinition: INyxMenuDefinition;
    LItem: INyxMenuDeclarationItem;
    LRuntime: INyxMenuItem;
    LItems: INyxMenuItems;
    LIndex: Integer;
  begin
    LDefinition := ADocument.Menus.Definition(AName);
    LItems := NyxMenuItems;
    for LIndex := 0 to LDefinition.Count - 1 do
    begin
      LItem := LDefinition.Item(LIndex);
      case LItem.Kind of
        nmiAction: LRuntime := NyxMenuAction(LItem.Part, LItem.Command);
        nmiCheck: LRuntime := NyxMenuCheck(LItem.Part, LItem.Command, LItem.IsChecked);
        nmiRadio: LRuntime := NyxMenuRadio(LItem.Part, LItem.Command, LItem.Group, LItem.IsChecked);
        nmiSeparator: LRuntime := NyxMenuSeparator(LItem.Part);
        nmiSubmenu: LRuntime := NyxMenuSubmenu(LItem.Part, Cook(LItem.Submenu));
      end;
      LItems := LItems.Add(LRuntime.Enabled(LItem.IsEnabled));
    end;
    Result := NewNyxMenuRecipe(ADocument, LDefinition.Root, LItems, LDefinition.Options);
  end;

begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Declared menus require an owned document');
  end;
  ADocument.Validate;
  Result := Cook(NyxMenuRef(AReference.Name));
end;

constructor TMenuCompletion.Create(ALease: TNyxMenuLease; AIndex: Integer);
begin
  inherited Create;
  FLease := ALease;
  FOwner := ALease;
  FIndex := AIndex;
end;

procedure TMenuCompletion.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LKeepAlive: INyxMenu;
begin

  if FLease.Owner = nil then
  begin
    Exit;
  end;
  LKeepAlive := FLease.Owner;

  if FIndex < 0 then
  begin
    FLease.Owner.Dismissed(AEvent);
  end
  else
  begin
    FLease.Owner.ChildInvoke(AEvent, FIndex);
  end;
  LKeepAlive.GetOpen;
end;

constructor TMenuItem.Create(const AItem: INyxMenuItem);
begin
  inherited Create;

  if AItem <> nil then
  begin
    FPart := AItem.Part;
    FCommand := AItem.Command;
    FGroup := AItem.Group;
    FKind := AItem.Kind;
    FEnabled := AItem.IsEnabled;
    FChecked := AItem.IsChecked;
    FRecipe := AItem.Recipe;
  end;
end;

function TMenuItem.GetPart: TNyxPartRef;
begin
  Result := FPart;
end;

function TMenuItem.GetCommand: TNyxMenuCommandRef;
begin
  Result := FCommand;
end;

function TMenuItem.GetGroup: TNyxMenuGroupRef;
begin
  Result := FGroup;
end;

function TMenuItem.GetKind: TNyxMenuItemKind;
begin
  Result := FKind;
end;

function TMenuItem.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TMenuItem.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TMenuItem.Enabled(AValue: Boolean): INyxMenuItem;
var
  LItem: TMenuItem;
begin
  LItem := TMenuItem.Create(Self);
  LItem.FEnabled := AValue;
  Result := LItem;
end;

function NyxMenuItems: TNyxMenuItems;
begin
  Result := TMenuItems.Create;
end;

constructor TMenuItems.Create(const AItems: INyxMenuItems);
var
  LIndex: Integer;
  LCount: Integer;
begin
  inherited Create;

  if AItems <> nil then
  begin
    LCount := AItems.Count;

    if (LCount < 0) or (LCount > 256) then
    begin
      raise ENyxModel.Create('Menu requires at most 256 items');
    end;
    for LIndex := 0 to LCount - 1 do
    begin
      Append(AItems[LIndex]);
    end;
  end;
end;

function TMenuItems.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TMenuItems.GetItem(AIndex: Integer): TNyxMenuItem;
begin

  if (AIndex < 0) or (AIndex >= GetCount) then
  begin
    raise ENyxModel.Create('Menu item index is outside its plan');
  end;
  Result := FItems[AIndex];
end;

procedure TMenuItems.Append(const AItem: INyxMenuItem);
var
  LItem: TMenuItem;
  LCount: Integer;
begin

  if AItem = nil then
  begin
    raise ENyxModel.Create('Menu plan cannot contain an absent item');
  end;
  LItem := TMenuItem.Create(AItem);
  LCount := GetCount;
  SetLength(FItems, LCount + 1);
  FItems[LCount] := LItem;
  SetLength(FObjects, LCount + 1);
  FObjects[LCount] := LItem;
end;

function TMenuItems.Add(const AItem: TNyxMenuItem): TNyxMenuItems;
var
  LIndex: Integer;
  LItems: TMenuItems;
begin

  if (GetCount >= 256) or (AItem = nil) or (AItem.Part.Name = '') then
  begin
    raise ENyxModel.Create('Menu requires named parts and at most 256 items');
  end;
  for LIndex := 0 to GetCount - 1 do
  begin

    if FItems[LIndex].Part.Name = AItem.Part.Name then
    begin
      raise ENyxModel.Create('Menu part occurs more than once');
    end;
  end;
  LItems := TMenuItems.Create(Self);
  Result := LItems;
  LItems.Append(AItem);
end;

function NyxMenu(const ATitle: TNyxText): TNyxMenuOptions;
begin
  Result := nyx.menu.types.NyxMenu(ATitle);
end;

function NyxMenuInvocation(const AEvent: TNyxEventInfo): TNyxMenuInvocation;
var
  LDetails: TNyxDataValue;
begin

  if not AEvent.IsNamed(NyxSemantic(nseActivate)) or not AEvent.HasDetails then
  begin
    raise ENyxModel.Create('Event is not a menu invocation');
  end;
  LDetails := AEvent.Details;

  if (LDetails.Kind <> ndObject) or (LDetails.Count <> 4) then
  begin
    raise ENyxModel.Create('Invalid menu invocation snapshot');
  end;
  Result.Command := NyxMenuCommand(LDetails.Field('menu-command').AsText);
  Result.Part := NyxPart(LDetails.Field('menu-part').AsText);
  Result.HasChecked := LDetails.Field('has-checked').AsBoolean;
  Result.Checked := LDetails.Field('checked').AsBoolean;
end;

constructor TNyxMenuPresenter.Create(const APopover: INyxPopover; const AItems: TNyxMenuItems);
var
  LIndex: Integer;
  LOther: Integer;
  LPart: INyxControl;
  LBudget: Integer;
  LItems: TMenuItems;
begin
  inherited Create;

  if (APopover = nil) or (AItems = nil) or (AItems.Count = 0) then
  begin
    raise ENyxModel.Create('Menu requires an owned presentation and named item plan');
  end;
  FPopover := APopover;
  { Copy the caller's immutable plan before runtime check/radio changes. }
  LItems := TMenuItems.Create(AItems);
  FItems := LItems;
  FMutableItems := LItems;
  FFocused := -1;
  FCompletion := NewNyxEvents;
  FLease := TNyxMenuLease.Create;
  FLeaseOwner := FLease;
  FLease.Owner := Self;
  LBudget := 2048;
  ValidateMenuTree(FPopover.Content, FItems, 1, LBudget);
  SetLength(FChildren, AItems.Count);
  SetLength(FChildOwners, AItems.Count);
  SetLength(FSubscriptions, AItems.Count * 2);
  for LIndex := 0 to AItems.Count - 1 do
  begin
    LPart := FPopover.Content.Part(AItems[LIndex].Part);

    if AItems[LIndex].Kind = nmiSeparator then
    begin

      if LPart.Kind <> NyxKindName(nkSeparator) then
      begin
        raise ENyxModel.Create('Menu separator part must be a Nyx separator');
      end;
      Continue;
    end;

    ValidateCommand(LPart);
    if AItems[LIndex].Kind <> nmiSubmenu then
    begin
      NyxMenuCommand(AItems[LIndex].Command.Name);
    end;

    if AItems[LIndex].Kind = nmiRadio then
    begin
      NyxMenuGroup(AItems[LIndex].Group.Name);
      for LOther := 0 to LIndex - 1 do
      begin

        if AItems[LIndex].IsChecked and AItems[LOther].IsChecked and
          (AItems[LOther].Kind = nmiRadio) and
          (AItems[LIndex].Group.Name = AItems[LOther].Group.Name) then
        begin
          raise ENyxModel.Create('Radio menu group has more than one initial selection');
        end;
      end;
    end;
    { Before callbacks may consume first. The main phase can suppress platform
      defaults; after hooks are observation-only and cannot own menu navigation. }
    FSubscriptions[LIndex * 2] := FPopover.Events.OnKeyDown(
      NyxControlEvents(LPart.ID)).Subscribe(TMenuInput.Create(FLease, LIndex));
    FSubscriptions[LIndex * 2 + 1] := FPopover.Events.On(
      NyxControlEvents(LPart.ID), ntClick).Subscribe(TMenuInput.Create(FLease, LIndex));
  end;
  FPopover.OnDismiss.Subscribe(TMenuCompletion.Create(FLease, -1));
end;

destructor TNyxMenuPresenter.Destroy;
var
  LIndex: Integer;
begin

  if FLease <> nil then
  begin
    FLease.Owner := nil;
  end;
  for LIndex := 0 to High(FSubscriptions) do
  begin

    if FSubscriptions[LIndex] <> nil then
    begin
      FSubscriptions[LIndex].Cancel;
    end;
  end;

  if FCompletion <> nil then
  begin
    FCompletion.Close;
  end;

  if FPopover <> nil then
  begin
    CloseChildren;
    FPopover.Close;
  end;
  FChildren := nil;
  FChildOwners := nil;
  FPopover := nil;
  FCompletion := nil;
  FLeaseOwner := nil;
  inherited Destroy;
end;

function TNyxMenuPresenter.GetContent: INyxControl;
begin
  Result := FPopover.Content;
end;

function TNyxMenuPresenter.GetEvents: INyxEvents;
begin
  Result := FPopover.Events;
end;

function TNyxMenuPresenter.GetOpen: Boolean;
begin
  Result := FPopover.IsOpen;
end;

function TNyxMenuPresenter.GetFocused: TNyxPartRef;
begin
  Result := NyxPart('');

  if GetOpen and (FFocused >= 0) then
  begin
    Result := FItems[FFocused].Part;
  end;
end;

function TNyxMenuPresenter.OnInvoke: INyxEventStream;
begin
  Result := FCompletion.OnNamed(NyxCompoundEvents(Content.ID), NyxSemantic(nseActivate));
end;

function TNyxMenuPresenter.OnDismiss: INyxEventStream;
begin
  Result := FPopover.OnDismiss;
end;

function TNyxMenuPresenter.ItemIndex(const APart: TNyxPartRef): Integer;
begin
  for Result := 0 to FItems.Count - 1 do
  begin

    if FItems[Result].Part.Name = APart.Name then
    begin
      Exit;
    end;
  end;
  raise ENyxModel.Create('Part is not a registered menu item');
end;

procedure TNyxMenuPresenter.ValidateCommand(const APart: INyxControl);
var
  LAction: TNyxAction;
begin

  if (APart.Kind <> NyxKindName(nkButton)) or
    not NyxInteractionPolicy(APart.Node).Enabled then
  begin
    raise ENyxModel.Create('Menu commands require focusable Nyx buttons; ' +
      'use menu item Enabled for disabled commands');
  end;
  { Renderer actions run before callbacks. Keeping commands in OnInvoke gives
    logical disablement and check/radio admission one portable authority. }

  if not TryNyxAction(APart.Node.Prop(NyxAttributeName(atAction)), LAction) or
    (LAction <> naNone) then
  begin
    raise ENyxModel.Create('Menu buttons use OnInvoke instead of a renderer action');
  end;
end;

function TNyxMenuPresenter.Button(const APart: TNyxPartRef): INyxButton;
begin

  if FItems[ItemIndex(APart)].Kind = nmiSeparator then
  begin
    raise ENyxModel.Create('Separator is not a menu button');
  end;
  Result := Content.Part(APart) as INyxButton;
end;

function TNyxMenuPresenter.ItemID(AIndex: Integer): TNyxText;
begin
  Result := Content.Part(FItems[AIndex].Part).ID;
end;

function TNyxMenuPresenter.Visible(AIndex: Integer): Boolean;
begin
  Result := (FItems[AIndex].Kind <> nmiSeparator) and
    NyxInteractionPolicy(Content.Part(FItems[AIndex].Part).Node).Visible;
end;

function TNyxMenuPresenter.Boundary(ALast: Boolean): Integer;
var
  LOffset: Integer;
begin
  for LOffset := 0 to FItems.Count - 1 do
  begin
    Result := LOffset;

    if ALast then
    begin
      Result := FItems.Count - 1 - LOffset;
    end;

    if Visible(Result) then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TNyxMenuPresenter.Focus(AIndex: Integer);
begin
  CloseChildren(AIndex);

  if (AIndex < 0) or not Visible(AIndex) or not FocusFace(AIndex) then
  begin
    raise ENyxModel.Create('Menu item cannot receive physical focus');
  end;
  FFocused := AIndex;
end;

procedure TNyxMenuPresenter.Open(const AOptions: TNyxMenuOptions);
var
  LKeepAlive: INyxMenu;
  LInitial: Integer;
  LIndex: Integer;
begin
  LKeepAlive := Self;
  FCompletion.Scheduler.RequireUI;
  AOptions.Validate;
  { Specialized content can be configured after construction. Revalidate before
    mounting, so that a late Action/Enabled edit cannot bypass menu admission. }
  for LIndex := 0 to FItems.Count - 1 do
  begin

    if FItems[LIndex].Kind <> nmiSeparator then
    begin
      ValidateCommand(Content.Part(FItems[LIndex].Part));
    end;
  end;
  LInitial := Boundary(AOptions.OpenAt = nmoLast);

  if LInitial < 0 then
  begin
    raise ENyxModel.Create('Menu has no visible command item');
  end;
  FOptions := AOptions;
  FSearch := NewNyxTypeAhead(AOptions.Search);
  FPopover.Open(AOptions.Placement.Focus(FItems[LInitial].Part));
  try
    ApplyFaces;
    Focus(LInitial);
    FReady := True;
    PresentationChanged;
  except
    FPopover.Close;
    raise;
  end;
  LKeepAlive.GetOpen;
end;

procedure TNyxMenuPresenter.Close;
begin
  CloseChildren;
  FPopover.Close;
  FSearch := nil;
  FFocused := -1;
  PresentationChanged;
end;

procedure TNyxMenuPresenter.PresentationChanged;
begin

  if FReady then
  begin
    ApplyFaces;
  end;
end;

function TNyxMenuPresenter.BranchOpen(AIndex: Integer): Boolean;
begin
  Result := (FChildren[AIndex] <> nil) and FChildren[AIndex].GetOpen;
end;

procedure TNyxMenuPresenter.CloseChildren(AExcept: Integer);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FChildren) do
  begin

    if (LIndex <> AExcept) and BranchOpen(LIndex) then
    begin
      FChildren[LIndex].Close;
    end;
  end;
end;

function TNyxMenuPresenter.FamilyRoot: TNyxMenuPresenter;
begin
  Result := Self;
  while (Result.FParentLease <> nil) and (Result.FParentLease.Owner <> nil) do
  begin
    Result := Result.FParentLease.Owner;
  end;
end;

function TNyxMenuPresenter.Submenu(const APart: TNyxPartRef): INyxMenu;
var
  LIndex: Integer;
begin
  LIndex := ItemIndex(APart);

  if FItems[LIndex].Kind <> nmiSubmenu then
  begin
    raise ENyxModel.Create('Menu part is not a submenu');
  end;
  Result := FChildOwners[LIndex];
end;

procedure TNyxMenuPresenter.OpenSubmenu(const APart: TNyxPartRef);
var
  LIndex: Integer;
  LChild: TNyxMenuPresenter;
  LPolicy: INyxMenuRecipePolicy;
  LOptions: TNyxMenuOptions;
begin
  FCompletion.Scheduler.RequireUI;
  LIndex := ItemIndex(APart);

  if not GetOpen or (FItems[LIndex].Kind <> nmiSubmenu) then
  begin
    raise ENyxModel.Create('Submenu requires an open parent and a named branch');
  end;

  if not Visible(LIndex) or not FItems[LIndex].IsEnabled then
  begin
    Exit;
  end;
  Focus(LIndex);

  if FChildren[LIndex] = nil then
  begin
    LChild := CreateSubmenu(LIndex, FItems[LIndex].Recipe);
    FChildOwners[LIndex] := LChild;
    FChildren[LIndex] := LChild;
    LChild.FParentLease := FLease;
    LChild.FParentOwner := FLeaseOwner;
    LChild.OnInvoke.Subscribe(TMenuCompletion.Create(FLease, LIndex));
  end;

  if not BranchOpen(LIndex) then
  begin
    LOptions := FOptions.Presentation(NyxPopover(LabelAt(LIndex))
      .Placement(npsRight, npaStart).Size(FOptions.Placement.Width, FOptions.Placement.Height)
      .Spacing(FOptions.Placement.Gap, FOptions.Placement.Margin)
      .Sizing(FOptions.Placement.SizeMode).DismissOn(FOptions.Placement.Dismissals))
      .Opening(nmoFirst);

    if Supports(FItems[LIndex].Recipe, INyxMenuRecipePolicy, LPolicy) and LPolicy.HasPolicy then
    begin
      LOptions := LPolicy.Policy;
    end;
    FChildren[LIndex].Open(LOptions);
  end;
  PresentationChanged;
end;

procedure TNyxMenuPresenter.ChildInvoke(const AEvent: TNyxEventInfo; AIndex: Integer);
var
  LEvent: TNyxEventInfo;
  LEvents: INyxEvents;
begin
  { A closed leaf means activation has completed. Retire every ancestor before
    dispatching the detached snapshot; Space check/radio changes keep the chain. }
  LEvents := FCompletion;

  if not BranchOpen(AIndex) then
  begin
    Close;
  end;
  LEvent := AEvent.Copy;
  LEvent.SourceID := Content.ID;
  LEvent.OriginID := Content.ID;
  LEvent.TargetID := Content.ID;
  LEvents.Dispatch(LEvent, LEvent.OriginID, LEvent.SourceID);
end;

procedure TNyxMenuPresenter.Dismissed(const AEvent: TNyxEventInfo);
var
  LIndex: Integer;
  LReason: TNyxPopoverReason;
begin
  LReason := NyxPopoverDismissReason(AEvent);
  for LIndex := 0 to High(FChildren) do
  begin

    if BranchOpen(LIndex) then
    begin
      { Outside/retired-anchor dismissal must not steal focus back to an already
        hidden ancestor. The same reason cascades through the owned hosts. }
      FChildren[LIndex].Popover.Dismiss(LReason);
    end;
  end;
  PresentationChanged;

  if (FParentLease <> nil) and (FParentLease.Owner <> nil) then
  begin
    FParentLease.Owner.PresentationChanged;
  end;
end;

procedure TNyxMenuPresenter.SetEnabled(const APart: TNyxPartRef; AValue: Boolean);
var
  LIndex: Integer;
begin
  FCompletion.Scheduler.RequireUI;
  LIndex := ItemIndex(APart);

  if FItems[LIndex].Kind = nmiSeparator then
  begin
    raise ENyxModel.Create('Separator has no command enablement');
  end;
  TMenuItems(FMutableItems).FObjects[LIndex].FEnabled := AValue;

  if GetOpen then
  begin
    ApplyFaces;
  end;
end;

function TNyxMenuPresenter.Checked(const APart: TNyxPartRef): Boolean;
var
  LIndex: Integer;
begin
  LIndex := ItemIndex(APart);

  if not (FItems[LIndex].Kind in [nmiCheck, nmiRadio]) then
  begin
    raise ENyxModel.Create('Menu action has no checked state');
  end;
  Result := FItems[LIndex].IsChecked;
end;

procedure TNyxMenuPresenter.Activate(AIndex: Integer; AClose: Boolean);
var
  LEvent: TNyxEventInfo;
  LIndex: Integer;
  LEvents: INyxEvents;
begin

  if not Visible(AIndex) or not FItems[AIndex].IsEnabled then
  begin
    Exit;
  end;

  if FItems[AIndex].Kind = nmiSubmenu then
  begin
    OpenSubmenu(FItems[AIndex].Part);
    Exit;
  end;
  { Opening a branch is navigation, like Right; read-only refuses leaf commands
    without disabling focus or changing Enter/Space navigation semantics. }

  if NyxInteractionPolicy(Content.Part(FItems[AIndex].Part).Node).ReadOnly then
  begin
    Exit;
  end;
  LEvents := FCompletion;
  case FItems[AIndex].Kind of
    nmiCheck:
      begin
        TMenuItems(FMutableItems).FObjects[AIndex].FChecked := not FItems[AIndex].IsChecked;
      end;
    nmiRadio:
      begin
        for LIndex := 0 to FItems.Count - 1 do
        begin

          if (FItems[LIndex].Kind = nmiRadio) and
            (FItems[LIndex].Group.Name = FItems[AIndex].Group.Name) then
          begin
            TMenuItems(FMutableItems).FObjects[LIndex].FChecked := LIndex = AIndex;
          end;
        end;
      end;
  else
    begin
      { Ordinary actions have no check state. }
    end;
  end;
  LEvent := Default(TNyxEventInfo);
  { Event copies validate every owned value, even without a value payload.
    An explicit null keeps the completion packet valid on both compilers. }
  LEvent.Value := NyxNull;
  LEvent.Trigger := ntNamed;
  LEvent.Name := NyxSemantic(nseActivate);
  LEvent.SourceID := Content.ID;
  LEvent.OriginID := Content.ID;
  LEvent.TargetID := Content.ID;
  LEvent.HasDetails := True;
  LEvent.Details := NyxObject([
    NyxField('menu-command', NyxData(FItems[AIndex].Command.Name)),
    NyxField('menu-part', NyxData(FItems[AIndex].Part.Name)),
    NyxField('has-checked', NyxData(FItems[AIndex].Kind in [nmiCheck, nmiRadio])),
    NyxField('checked', NyxData(FItems[AIndex].IsChecked))]);

  if AClose then
  begin
    Close;
  end
  else
  begin
    ApplyFaces;
  end;
  LEvents.Dispatch(LEvent, LEvent.OriginID, LEvent.SourceID);
end;

procedure TNyxMenuPresenter.Input(AIndex: Integer; const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LResponse: INyxEventResponse;
  LTarget: Integer;
  LStep: Integer;
  LCount: Integer;
begin

  if not GetOpen then
  begin
    Exit;
  end;

  if AEvent.Trigger = ntClick then
  begin
    Activate(AIndex, True);
    Exit;
  end;

  if not AEvent.HasKeyboard or AEvent.DefaultPrevented then
  begin
    Exit;
  end;
  LResponse := NyxEventResponse(AExecution);

  if not LResponse.CanConsume or LResponse.Consumed then
  begin
    Exit;
  end;
  FFocused := AIndex;

  if (AEvent.Keyboard.Key = nkTabKey) and
    (AEvent.Keyboard.Modifiers <= [nmShift]) then
  begin
    FamilyRoot.Close;
    FamilyRoot.TabExit(nmShift in AEvent.Keyboard.Modifiers, AExecution);
    Exit;
  end;

  if AEvent.Keyboard.Modifiers <> [] then
  begin
    Exit;
  end;
  LTarget := -1;
  case AEvent.Keyboard.Key of
    nkRightKey:
      begin

        if FItems[AIndex].Kind = nmiSubmenu then
        begin
          LResponse.Consume;
          Activate(AIndex, False);
        end;
        Exit;
      end;
    nkLeftKey:
      begin

        if (FParentLease <> nil) and (FParentLease.Owner <> nil) then
        begin
          LResponse.Consume;
          Close;
          FParentLease.Owner.PresentationChanged;
        end;
        Exit;
      end;
    nkHomeKey:
      begin
        LTarget := Boundary(False);
      end;
    nkEndKey:
      begin
        LTarget := Boundary(True);
      end;
    nkDownKey, nkUpKey:
      begin
        LTarget := AIndex;
        LStep := 1;

        if AEvent.Keyboard.Key = nkUpKey then
        begin
          LStep := -1;
        end;
        LCount := 0;
        repeat
          Inc(LTarget, LStep);

          if (LTarget < 0) or (LTarget >= FItems.Count) then
          begin

            if not FOptions.Wraps then
            begin
              LTarget := AIndex;
              Break;
            end;
            LTarget := (LTarget + FItems.Count) mod FItems.Count;
          end;
          Inc(LCount);
        until Visible(LTarget) or (LCount >= FItems.Count);
      end;
    nkEnterKey, nkSpaceKey:
      begin
        LResponse.Consume;

        if not AEvent.Keyboard.Repeating then
        begin
          Activate(AIndex, (AEvent.Keyboard.Key = nkEnterKey) or
            not (FItems[AIndex].Kind in [nmiCheck, nmiRadio]));
        end;
        Exit;
      end;
    nkEscapeKey:
      begin
        LResponse.Consume;
        FPopover.Dismiss(nprEscape);
        Exit;
      end;
  else
    begin
      { Text keys are handled by the adapter's decoded-character bridge. }
    end;
  end;

  if LTarget >= 0 then
  begin
    LResponse.Consume;
    FSearch.Reset;
    Focus(LTarget);
  end;
end;

function TNyxMenuPresenter.LabelAt(AIndex: Integer): TNyxText;
begin
  Result := '';

  if Visible(AIndex) then
  begin
    Result := Button(FItems[AIndex].Part).Text;
  end;
end;

function TNyxMenuPresenter.TextInput(const AText: TNyxText; ATimeMS: Double): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if not GetOpen or (FSearch = nil) or not NyxTypeAheadCharacter(AText) then
  begin
    Exit;
  end;
  LIndex := FSearch.Find(AText, ATimeMS, FItems.Count, FFocused, LabelAt);

  if LIndex >= 0 then
  begin
    Focus(LIndex);
    Result := True;
  end;
end;

end.
