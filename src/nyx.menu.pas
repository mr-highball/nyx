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
  nyx.events, nyx.scheduler, nyx.popover, nyx.typeahead;

type
  { Open application identities own text, never execution keywords or controls. }
  TNyxMenuCommandRef = record
    Name: TNyxText;
  end;
  TNyxMenuGroupRef = record
    Name: TNyxText;
  end;
  TNyxMenuItemKind = (nmiAction, nmiCheck, nmiRadio, nmiSeparator);
  TNyxMenuOpening = (nmoFirst, nmoLast);

  { Immutable registration of an existing named Nyx part. Disabled commands
    remain keyboard-focusable; physical Enabled=False is deliberately distinct.
    Check/radio state belongs to this managed runtime, not document defaults. }
  TNyxMenuItem = record
  private
    FPart: TNyxPartRef;
    FCommand: TNyxMenuCommandRef;
    FGroup: TNyxMenuGroupRef;
    FKind: TNyxMenuItemKind;
    FEnabled: Boolean;
    FChecked: Boolean;
  public
    { Return a detached choice; disabling preserves keyboard eligibility. }
    function Enabled(AValue: Boolean): TNyxMenuItem;
    property Part: TNyxPartRef read FPart;
    property Command: TNyxMenuCommandRef read FCommand;
    property Group: TNyxMenuGroupRef read FGroup;
    property Kind: TNyxMenuItemKind read FKind;
    property IsEnabled: Boolean read FEnabled;
    property IsChecked: Boolean read FChecked;
  end;

  { Value-owned ordered plan, maximum 256 entries. Add returns an independent
    array; prior plans stay unchanged on both compilers. Missing/duplicate parts,
    invalid groups and multiple initially checked radios refuse before opening. }
  TNyxMenuItems = record
  private
    FItems: array of TNyxMenuItem;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TNyxMenuItem;
  public
    function Add(const AItem: TNyxMenuItem): TNyxMenuItems;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TNyxMenuItem read GetItem; default;
  end;

  { Menu navigation is an explicit typed runtime policy. Every open starts at
    first/last visible item. Typeahead uses the shared Unicode search contract. }
  TNyxMenuOptions = record
  private
    FPresentation: TNyxPopoverOptions;
    FTypeAhead: TNyxTypeAheadOptions;
    FOpening: TNyxMenuOpening;
    FWrap: Boolean;
  public
    { Copy an existing validated popover policy; the menu owns its initial focus. }
    function Presentation(const AValue: TNyxPopoverOptions): TNyxMenuOptions;
    { Copy enabled/matching/inter-key timing policy into the next open. }
    function TypeAhead(const AValue: TNyxTypeAheadOptions): TNyxMenuOptions;
    { First/last visible command, including logically disabled commands. }
    function Opening(AValue: TNyxMenuOpening): TNyxMenuOptions;
    { Choose whether arrow traversal wraps at the first/last command. }
    function Wrap(AValue: Boolean): TNyxMenuOptions;
    { Undefined policies and unknown enum ordinals raise before presentation. }
    procedure Validate;
    property Placement: TNyxPopoverOptions read FPresentation;
    property Search: TNyxTypeAheadOptions read FTypeAhead;
    property OpenAt: TNyxMenuOpening read FOpening;
    property Wraps: Boolean read FWrap;
  end;

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
    FCompletion: INyxEvents;
    FLease: TNyxMenuLease;
    FLeaseOwner: IInterface;
    FSubscriptions: array of INyxEventSubscription;
    FOptions: TNyxMenuOptions;
    FSearch: INyxTypeAhead;
    FFocused: Integer;
    procedure ValidateCommand(const APart: INyxControl);
    function ItemIndex(const APart: TNyxPartRef): Integer;
    function LabelAt(AIndex: Integer): TNyxText;
    function Visible(AIndex: Integer): Boolean;
    function Boundary(ALast: Boolean): Integer;
    procedure Focus(AIndex: Integer);
    procedure Activate(AIndex: Integer; AClose: Boolean);
    procedure Input(AIndex: Integer; const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution);
  protected
    procedure ApplyFaces; virtual; abstract;
    function FocusFace(AIndex: Integer): Boolean; virtual; abstract;
    { Called after Close restores the invoker. Browser leaves native Tab default;
      LCL explicitly traverses the restored invoker's container and consumes it. }
    procedure TabExit(AReverse: Boolean; const AExecution: INyxExecution); virtual; abstract;
    function TextInput(const AText: TNyxText; ATimeMS: Double): Boolean;
    function ItemID(AIndex: Integer): TNyxText;
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
function NyxMenuItems: TNyxMenuItems;
function NyxMenu(const ATitle: TNyxText): TNyxMenuOptions;
{ Validate/copy the exact private completion payload. Unrelated events and
  malformed snapshots raise ENyxModel or the typed data admission exception. }
function NyxMenuInvocation(const AEvent: TNyxEventInfo): TNyxMenuInvocation;

implementation

uses nyx.data, nyx.interaction;

type
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
  Result.Name := NyxNamedEvent(AName).Name;
end;

function NyxMenuGroup(const AName: TNyxText): TNyxMenuGroupRef;
begin
  Result.Name := NyxNamedEvent(AName).Name;
end;

function NyxMenuAction(const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef): TNyxMenuItem;
begin
  Result := Default(TNyxMenuItem);
  Result.FPart := APart;
  Result.FCommand := NyxMenuCommand(ACommand.Name);
  Result.FEnabled := True;
end;

function NyxMenuCheck(const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef; AChecked: Boolean): TNyxMenuItem;
begin
  Result := NyxMenuAction(APart, ACommand);
  Result.FKind := nmiCheck;
  Result.FChecked := AChecked;
end;

function NyxMenuRadio(const APart: TNyxPartRef;
  const ACommand: TNyxMenuCommandRef; const AGroup: TNyxMenuGroupRef;
  AChecked: Boolean): TNyxMenuItem;
begin
  Result := NyxMenuCheck(APart, ACommand, AChecked);
  Result.FKind := nmiRadio;
  Result.FGroup := NyxMenuGroup(AGroup.Name);
end;

function NyxMenuSeparator(const APart: TNyxPartRef): TNyxMenuItem;
begin
  Result := Default(TNyxMenuItem);
  Result.FPart := APart;
  Result.FKind := nmiSeparator;
end;

function TNyxMenuItem.Enabled(AValue: Boolean): TNyxMenuItem;
begin
  Result := Self;
  Result.FEnabled := AValue;
end;

function NyxMenuItems: TNyxMenuItems;
begin
  Result := Default(TNyxMenuItems);
end;

function TNyxMenuItems.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TNyxMenuItems.GetItem(AIndex: Integer): TNyxMenuItem;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxModel.Create('Menu item index is outside its plan');
  end;
  Result := FItems[AIndex];
end;

function TNyxMenuItems.Add(const AItem: TNyxMenuItem): TNyxMenuItems;
var
  LIndex: Integer;
begin

  if (Count >= 256) or (AItem.Part.Name = '') then
  begin
    raise ENyxModel.Create('Menu requires named parts and at most 256 items');
  end;
  for LIndex := 0 to Count - 1 do
  begin

    if FItems[LIndex].Part.Name = AItem.Part.Name then
    begin
      raise ENyxModel.Create('Menu part occurs more than once');
    end;
  end;
  Result := Default(TNyxMenuItems);
  SetLength(Result.FItems, Count + 1);
  for LIndex := 0 to Count - 1 do
  begin
    Result.FItems[LIndex] := FItems[LIndex];
  end;
  Result.FItems[Count] := AItem;
end;

function NyxMenu(const ATitle: TNyxText): TNyxMenuOptions;
begin
  Result := Default(TNyxMenuOptions);
  Result.FPresentation := NyxPopover(ATitle).Size(280, 600);
  Result.FTypeAhead := NyxTypeAhead;
  Result.FWrap := True;
end;

procedure TNyxMenuOptions.Validate;
begin
  FPresentation.Validate;
  FTypeAhead.Validate;

  if (Ord(FOpening) < Ord(Low(TNyxMenuOpening))) or
    (Ord(FOpening) > Ord(High(TNyxMenuOpening))) then
  begin
    raise ENyxModel.Create('Unknown menu opening policy');
  end;
end;

function TNyxMenuOptions.Presentation(const AValue: TNyxPopoverOptions): TNyxMenuOptions;
begin
  Result := Self;
  Result.FPresentation := AValue;
  Result.Validate;
end;

function TNyxMenuOptions.TypeAhead(const AValue: TNyxTypeAheadOptions): TNyxMenuOptions;
begin
  Result := Self;
  Result.FTypeAhead := AValue;
  Result.Validate;
end;

function TNyxMenuOptions.Opening(AValue: TNyxMenuOpening): TNyxMenuOptions;
begin
  Result := Self;
  Result.FOpening := AValue;
  Result.Validate;
end;

function TNyxMenuOptions.Wrap(AValue: Boolean): TNyxMenuOptions;
begin
  Result := Self;
  Result.FWrap := AValue;
  Result.Validate;
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
begin
  inherited Create;

  if (APopover = nil) or (AItems.Count = 0) then
  begin
    raise ENyxModel.Create('Menu requires an owned presentation and named item plan');
  end;
  FPopover := APopover;
  FItems := AItems;
  { Detach the caller's plan before runtime check/radio/enablement changes. }
  FItems.FItems := Copy(AItems.FItems);
  FFocused := -1;
  FCompletion := NewNyxEvents;
  FLease := TNyxMenuLease.Create;
  FLeaseOwner := FLease;
  FLease.Owner := Self;
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
    { Before callbacks may consume first. The main phase can suppress platform
      defaults; after hooks are observation-only and cannot own menu navigation. }
    FSubscriptions[LIndex * 2] := FPopover.Events.OnKeyDown(
      NyxControlEvents(LPart.ID)).Subscribe(TMenuInput.Create(FLease, LIndex));
    FSubscriptions[LIndex * 2 + 1] := FPopover.Events.On(
      NyxControlEvents(LPart.ID), ntClick).Subscribe(TMenuInput.Create(FLease, LIndex));
  end;
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
    FPopover.Close;
  end;
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
  except
    FPopover.Close;
    raise;
  end;
  LKeepAlive.GetOpen;
end;

procedure TNyxMenuPresenter.Close;
begin
  FPopover.Close;
  FSearch := nil;
  FFocused := -1;
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
  FItems.FItems[LIndex].FEnabled := AValue;

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

  if not Visible(AIndex) or not FItems[AIndex].IsEnabled or
    NyxInteractionPolicy(Content.Part(FItems[AIndex].Part).Node).ReadOnly then
  begin
    Exit;
  end;
  LEvents := FCompletion;
  case FItems[AIndex].Kind of
    nmiCheck:
      begin
        FItems.FItems[AIndex].FChecked := not FItems[AIndex].IsChecked;
      end;
    nmiRadio:
      begin
        for LIndex := 0 to FItems.Count - 1 do
        begin

          if (FItems[LIndex].Kind = nmiRadio) and
            (FItems[LIndex].Group.Name = FItems[AIndex].Group.Name) then
          begin
            FItems.FItems[LIndex].FChecked := LIndex = AIndex;
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
    Close;
    TabExit(nmShift in AEvent.Keyboard.Modifiers, AExecution);
    Exit;
  end;

  if AEvent.Keyboard.Modifiers <> [] then
  begin
    Exit;
  end;
  LTarget := -1;
  case AEvent.Keyboard.Key of
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
