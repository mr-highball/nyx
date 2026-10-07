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

unit nyx.menu.button;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.menu, nyx.menu.bar, nyx.controls, nyx.events, nyx.types;

type
  { Managed registration on an ordinary specialized Nyx button. Owns the menu,
    descriptor handle and event router, with two cancellable weak callbacks.
    Release before its renderer/controller teardown. No DOM/LCL types enter this
    contract. The button must have no renderer Action throughout registration.

    Pointer click toggles; Enter/Space/Down open first, Up opens last. Main-phase
    keys consume browser/native defaults, with repeats/modifiers and already
    consumed input left alone. Disabled/hidden authoring policy refuses intent;
    read-only retains nonmutating navigation through the ordinary input policy.
    The menu adapter owns aria-haspopup/expanded/controls, including silent Close. }
  INyxMenuButton = interface(IInterface)
    ['{1A8B0719-0647-444E-A241-061026000005}']
    function GetMenu: INyxMenu;
    property Menu: INyxMenu read GetMenu;
  end;

  { Owned bindings for one mounted view. Holds independent menus/invokers and
    forwards their ordered command completions as OnActivate on the exact
    invoking control. Runtime IDs distinguish sibling reusable instances;
    design IDs retain callback scope. Release before remounting or destroying
    the renderer. Runtime check changes never edit document defaults/history. }
  INyxMenuBindings = interface(IInterface)
    ['{06C00707-BA22-4531-9030-000000000005}']
    function GetCount: Integer;
    function Add(const AButton: INyxButton; const AEvents: INyxEvents;
      const AMenu: INyxMenu; const AOptions: TNyxMenuOptions): INyxMenuBindings;
    function Menu(const AControl: TNyxControlRef): INyxMenu;
    property Count: Integer read GetCount;
  end;

  { Optional coordinated-group extension of the same mounted owner. Bar retains
    its families; this owner cancels forwarding before releasing any coordinator.
    Menu(control) also observes each exact grouped heading. AddBar refuses duplicate
    headings/rows and nonsequential forwarding without replacing prior bindings. }
  INyxMenuBarBindings = interface(IInterface)
    ['{71000707-BA22-4531-9030-000000000003}']
    function AddBar(const ABar: INyxMenuBar; const AEvents: INyxEvents): INyxMenuBindings;
    function Bar(const AControl: TNyxControlRef): INyxMenuBar;
    function GetBarCount: Integer;
    property BarCount: Integer read GetBarCount;
  end;

{ Target factories populate this portable owner after mounting a view. It
  retains no renderer and creates no cycle into the document or menu family. }
function NewNyxMenuBindings: INyxMenuBindings;

{ Register synchronously on sequential click/key streams without changing any
  existing execution policy. Nil owners, renderer actions or nonsequential
  streams refuse before subscriptions; the caller retains previous registrations. }
function NewNyxMenuButton(const AButton: INyxButton; const AEvents: INyxEvents;
  const AMenu: INyxMenu; const AOptions: TNyxMenuOptions): INyxMenuButton;

implementation

uses nyx.model, nyx.behavior, nyx.scheduler, nyx.interaction, nyx.text;

type
  TCommandForwarder = class(TNyxEventCallback)
  private
    FEvents: INyxEvents;
    FRuntimeID: TNyxText;
    FDesignID: TNyxText;
    FKeepOrigin: Boolean;
  public
    constructor Create(const AEvents: INyxEvents; const ARuntimeID, ADesignID: TNyxText;
      AKeepOrigin: Boolean = False);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  TMenuBindings = class(TInterfacedObject, INyxMenuBindings, INyxMenuBarBindings)
  private
    FIDs: array of TNyxText;
    FInvokers: array of INyxMenuButton;
    FCompletions: array of INyxEventSubscription;
    FFamilies: array of INyxMenu;
    FBars: array of INyxMenuBar;
    FBarCompletions: array of INyxEventSubscription;
  public
    destructor Destroy; override;
    function GetCount: Integer;
    function Add(const AButton: INyxButton; const AEvents: INyxEvents;
      const AMenu: INyxMenu; const AOptions: TNyxMenuOptions): INyxMenuBindings;
    function Menu(const AControl: TNyxControlRef): INyxMenu;
    function AddBar(const ABar: INyxMenuBar; const AEvents: INyxEvents): INyxMenuBindings;
    function Bar(const AControl: TNyxControlRef): INyxMenuBar;
    function GetBarCount: Integer;
  end;
  TMenuButton = class;
  TButtonLease = class(TInterfacedObject)
  public
    Owner: TMenuButton;
  end;
  TButtonInput = class(TNyxEventCallback)
  private
    FLease: TButtonLease;
    FLeaseOwner: IInterface;
  public
    constructor Create(ALease: TButtonLease);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  TMenuButton = class(TInterfacedObject, INyxMenuButton)
  private
    FButton: INyxButton;
    FEvents: INyxEvents;
    FMenu: INyxMenu;
    FOptions: TNyxMenuOptions;
    FLease: TButtonLease;
    FLeaseOwner: IInterface;
    FClick: INyxEventSubscription;
    FKey: INyxEventSubscription;
    procedure Input(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
  public
    constructor Create(const AButton: INyxButton; const AEvents: INyxEvents;
      const AMenu: INyxMenu; const AOptions: TNyxMenuOptions);
    destructor Destroy; override;
    function GetMenu: INyxMenu;
  end;

function NewNyxMenuBindings: INyxMenuBindings;
begin
  Result := TMenuBindings.Create;
end;

constructor TCommandForwarder.Create(const AEvents: INyxEvents;
  const ARuntimeID, ADesignID: TNyxText; AKeepOrigin: Boolean);
begin
  inherited Create;
  FEvents := AEvents;
  FRuntimeID := ARuntimeID;
  FDesignID := ADesignID;
  FKeepOrigin := AKeepOrigin;
end;

procedure TCommandForwarder.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LEvent: TNyxEventInfo;
begin
  { Keep the immutable command/part/check payload, while routing to the control
    whose saved menu attachment produced this independent mounted family. }
  NyxMenuInvocation(AEvent);
  LEvent := AEvent.Copy;
  { A bar already carries the exact runtime heading as origin. Menu buttons
    instead need their root invoker here, without leaking popup-content IDs. }

  if not FKeepOrigin then
  begin
    LEvent.OriginID := FRuntimeID;
  end;
  LEvent.SourceID := FRuntimeID;
  LEvent.TargetID := FRuntimeID;
  FEvents.Dispatch(LEvent, FDesignID, FDesignID);
end;

destructor TMenuBindings.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FCompletions) do
  begin

    if FCompletions[LIndex] <> nil then
    begin
      FCompletions[LIndex].Cancel;
    end;
  end;
  FCompletions := nil;
  for LIndex := 0 to High(FBarCompletions) do
  begin
    FBarCompletions[LIndex].Cancel;
  end;
  FBarCompletions := nil;
  FBars := nil;
  FInvokers := nil;
  FFamilies := nil;
  inherited Destroy;
end;

function TMenuBindings.GetCount: Integer;
begin
  Result := Length(FIDs);
end;

function TMenuBindings.Add(const AButton: INyxButton; const AEvents: INyxEvents;
  const AMenu: INyxMenu; const AOptions: TNyxMenuOptions): INyxMenuBindings;
var
  LIndex: Integer;
  LInvoker: INyxMenuButton;
  LCompletion: INyxEventSubscription;
begin

  if AButton = nil then
  begin
    raise ENyxModel.Create('Menu bindings require an exact mounted button');
  end;
  for LIndex := 0 to High(FIDs) do
  begin

    if FIDs[LIndex] = AButton.ID then
    begin
      raise ENyxModel.Create('A mounted control already has a menu binding');
    end;
  end;
  LInvoker := NewNyxMenuButton(AButton, AEvents, AMenu, AOptions);
  LCompletion := AMenu.OnInvoke.Subscribe(TCommandForwarder.Create(AEvents,
    AButton.ID, AButton.Node.DesignID));
  LIndex := GetCount;
  SetLength(FIDs, LIndex + 1);
  SetLength(FInvokers, LIndex + 1);
  SetLength(FCompletions, LIndex + 1);
  SetLength(FFamilies, LIndex + 1);
  FIDs[LIndex] := AButton.ID;
  FInvokers[LIndex] := LInvoker;
  FCompletions[LIndex] := LCompletion;
  FFamilies[LIndex] := AMenu;
  Result := Self;
end;

function TMenuBindings.Menu(const AControl: TNyxControlRef): INyxMenu;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FIDs) do
  begin

    if FIDs[LIndex] = AControl.ID then
    begin
      Exit(FFamilies[LIndex]);
    end;
  end;
  raise ENyxModel.Create('No declared menu is mounted on this exact control');
end;

function TMenuBindings.GetBarCount: Integer;
begin
  Result := Length(FBars);
end;

function TMenuBindings.Bar(const AControl: TNyxControlRef): INyxMenuBar;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FBars) do
  begin

    if FBars[LIndex].Content.ID = AControl.ID then
    begin
      Exit(FBars[LIndex]);
    end;
  end;
  raise ENyxModel.Create('No declared menu bar is mounted on this exact row');
end;

function TMenuBindings.AddBar(const ABar: INyxMenuBar;
  const AEvents: INyxEvents): INyxMenuBindings;
var
  LIndex: Integer;
  LOther: Integer;
  LOffset: Integer;
  LIDs: array of TNyxText;
  LMenus: array of INyxMenu;
  LCompletion: INyxEventSubscription;
  LCallback: INyxEventCallback;
begin

  if (ABar = nil) or (AEvents = nil) or (ABar.Count < 1) or
    (ABar.Count > 64) or (ABar.OnInvoke.ExecutionPolicy <> neSequential) then
  begin
    raise ENyxModel.Create('Declared bar requires bounded sequential UI forwarding');
  end;
  AEvents.Scheduler.RequireUI;
  for LIndex := 0 to High(FBars) do
  begin

    if FBars[LIndex].Content.ID = ABar.Content.ID then
    begin
      raise ENyxModel.Create('This exact row already owns a declared bar binding');
    end;
  end;
  SetLength(LIDs, ABar.Count);
  SetLength(LMenus, ABar.Count);
  for LIndex := 0 to High(LIDs) do
  begin
    LIDs[LIndex] := ABar.Content.Part(ABar.Heading(LIndex)).ID;
    LMenus[LIndex] := ABar.Menu(ABar.Heading(LIndex));
    for LOther := 0 to High(FIDs) do
    begin

      if FIDs[LOther] = LIDs[LIndex] then
      begin
        raise ENyxModel.Create('A grouped heading already owns another menu binding');
      end;
    end;
  end;
  LCallback := TCommandForwarder.Create(AEvents, ABar.Content.ID,
    ABar.Content.Node.DesignID, True);
  LCompletion := ABar.OnInvoke.Subscribe(LCallback);
  LOffset := GetCount;
  SetLength(FIDs, LOffset + Length(LIDs));
  SetLength(FFamilies, LOffset + Length(LIDs));
  for LIndex := 0 to High(LIDs) do
  begin
    FIDs[LOffset + LIndex] := LIDs[LIndex];
    FFamilies[LOffset + LIndex] := LMenus[LIndex];
  end;
  LIndex := GetBarCount;
  SetLength(FBars, LIndex + 1);
  SetLength(FBarCompletions, LIndex + 1);
  FBars[LIndex] := ABar;
  FBarCompletions[LIndex] := LCompletion;
  Result := Self;
end;

constructor TButtonInput.Create(ALease: TButtonLease);
begin
  inherited Create;
  FLease := ALease;
  FLeaseOwner := ALease;
end;

procedure TButtonInput.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LKeepAlive: INyxMenuButton;
begin

  if FLease.Owner <> nil then
  begin
    LKeepAlive := FLease.Owner;
    FLease.Owner.Input(AEvent, AExecution);
    LKeepAlive.GetMenu;
  end;
end;

constructor TMenuButton.Create(const AButton: INyxButton; const AEvents: INyxEvents;
  const AMenu: INyxMenu; const AOptions: TNyxMenuOptions);
var
  LAction: TNyxAction;
  LClick: INyxEventStream;
  LKey: INyxEventStream;
begin
  inherited Create;

  if (AButton = nil) or (AEvents = nil) or (AMenu = nil) then
  begin
    raise ENyxModel.Create('Menu button requires a button, router and managed menu');
  end;
  AOptions.Validate;

  if not TryNyxAction(AButton.Node.Prop(NyxAttributeName(atAction)), LAction) or
    (LAction <> naNone) then
  begin
    raise ENyxModel.Create('Menu invoker uses managed invocation instead of a renderer action');
  end;
  LClick := AEvents.On(NyxControlEvents(AButton.ID), ntClick);
  LKey := AEvents.OnKeyDown(NyxControlEvents(AButton.ID));

  if (LClick.ExecutionPolicy <> neSequential) or
    (LKey.ExecutionPolicy <> neSequential) then
  begin
    raise ENyxModel.Create('Menu invoker requires sequential UI input streams');
  end;
  FButton := AButton;
  FEvents := AEvents;
  FMenu := AMenu;
  FOptions := AOptions;
  FLease := TButtonLease.Create;
  FLeaseOwner := FLease;
  FLease.Owner := Self;
  FClick := LClick.Subscribe(TButtonInput.Create(FLease));
  FKey := LKey.Subscribe(TButtonInput.Create(FLease));
end;

destructor TMenuButton.Destroy;
begin

  if FLease <> nil then
  begin
    FLease.Owner := nil;
  end;

  if FClick <> nil then
  begin
    FClick.Cancel;
  end;

  if FKey <> nil then
  begin
    FKey.Cancel;
  end;
  FClick := nil;
  FKey := nil;
  FMenu := nil;
  FButton := nil;
  FEvents := nil;
  FLeaseOwner := nil;
  inherited Destroy;
end;

function TMenuButton.GetMenu: INyxMenu;
begin
  Result := FMenu;
end;

procedure TMenuButton.Input(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LOpening: TNyxMenuOpening;
begin
  FEvents.Scheduler.RequireUI;

  if not NyxInteractionPolicy(FButton.Node).CanIssueCommand then
  begin
    Exit;
  end;

  if AEvent.Trigger = ntClick then
  begin

    if FMenu.IsOpen then
    begin
      FMenu.Close;
    end
    else
    begin
      FMenu.Open(FOptions.Opening(nmoFirst));
    end;
    Exit;
  end;

  if not AEvent.HasKeyboard or AEvent.DefaultPrevented or
    (AEvent.Keyboard.Modifiers <> []) or AEvent.Keyboard.Repeating or
    not (AEvent.Keyboard.Key in [nkEnterKey, nkSpaceKey, nkDownKey, nkUpKey]) then
  begin
    Exit;
  end;
  NyxEventResponse(AExecution).Consume;
  LOpening := nmoFirst;

  if AEvent.Keyboard.Key = nkUpKey then
  begin
    LOpening := nmoLast;
  end;

  if not FMenu.IsOpen then
  begin
    FMenu.Open(FOptions.Opening(LOpening));
  end;
end;

function NewNyxMenuButton(const AButton: INyxButton; const AEvents: INyxEvents;
  const AMenu: INyxMenu; const AOptions: TNyxMenuOptions): INyxMenuButton;
begin
  Result := TMenuButton.Create(AButton, AEvents, AMenu, AOptions);
end;

end.
