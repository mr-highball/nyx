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

unit nyx.menu.bar;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.controls, nyx.menu, nyx.menu.types, nyx.events,
  nyx.behavior, nyx.scheduler, nyx.typeahead;

type
  { The runtime and saved declaration share the same portable value contract. }
  TNyxMenuBarOptions = nyx.menu.types.TNyxMenuBarOptions;

  { Managed coordinator over a mounted ordinary Nyx row and its named buttons.
    Add retains independently owned menu families, not the renderer or document.
    Release BEFORE the borrowed target renderer. It retires every registration
    and closes its families; retained menus/content remain valid afterward.

    Headings open dropdowns; command/check/radio entries live in those menus.
    One visible heading is a Tab stop. Logical disabled headings retain arrow
    focus but refuse opening. Hidden headings are skipped. Left/Right, Home/End,
    Enter/Space/Down/Up, Escape, whole-family Tab and Unicode typeahead use the
    same shared controller. Mouse hover switches only an already-open family.

    Add is bounded to 64 unique named headings and unique closed root families.
    Renderer actions, foreign parts, wrong physical anchors, disabled controls,
    duplicate coordinators and nonsequential input streams refuse registration.
    Public methods require the UI thread. Existing execution policies are never
    silently changed. Before-consumed input remains owned by the application. }
  INyxMenuBar = interface(IInterface)
    ['{17E9CC06-A34B-4EBB-BDC6-071026000004}']
    function Add(const APart: TNyxPartRef; const AMenu: INyxMenu;
      const AOptions: TNyxMenuOptions): INyxMenuBar;
    function Policy(const AOptions: TNyxMenuBarOptions): INyxMenuBar;
    function GetContent: INyxRow;
    function GetCount: Integer;
    function GetFocused: TNyxPartRef;
    function GetOpen: Boolean;
    function Menu(const APart: TNyxPartRef): INyxMenu;
    { Exact declared order, used by mounted binding owners. Out of range refuses. }
    function Heading(AIndex: Integer): TNyxPartRef;
    { Close any family and focus this exact visible named heading; unknown or
      hidden parts refuse. Target focus failure raises ENyxModel. }
    procedure Focus(const APart: TNyxPartRef);
    { Open an enabled heading with typed first/last item selection.
      Logical disabled headings leave the current presentation unchanged. }
    procedure Open(const APart: TNyxPartRef; AOpening: TNyxMenuOpening = nmoFirst);
    { Silent idempotent whole-bar dismissal; independently retained menus live. }
    procedure Close;
    { Runtime logical admission only. Disabling an open family closes it;
      its heading remains physically focusable without editing saved defaults. }
    procedure SetEnabled(const APart: TNyxPartRef; AValue: Boolean);
    { After an application changes heading visibility/enablement in the mounted
      descriptor, requalify the Tab stop and close newly unavailable families.
      A renderer remount instead requires retiring/rebinding the whole bar. }
    procedure Refresh;
    { Ordered detached menu-command snapshots. OriginID identifies the heading;
      NyxMenuInvocation retains the leaf command/part and independent check state.
      Callbacks may release the bar; they must not strongly retain it themselves. }
    function OnInvoke: INyxEventStream;
    property Content: INyxRow read GetContent;
    property Count: Integer read GetCount;
    property Focused: TNyxPartRef read GetFocused;
    property IsOpen: Boolean read GetOpen;
  end;

  TNyxMenuBarPresenter = class;
  TNyxMenuBarLease = class(TInterfacedObject)
  public
    { Weak borrowed pointer, cleared before subscriptions/presentations retire. }
    Owner: TNyxMenuBarPresenter;
  end;
  { Internal owned registration record; adapters consume only ButtonAt/Enabled.
    Kept as a class because COM interface fields in records differ in pas2js. }
  TNyxMenuBarEntry = class
  public
    Part: TNyxPartRef;
    Button: INyxButton;
    Menu: INyxMenu;
    Options: TNyxMenuOptions;
    Enabled: Boolean;
    Tokens: array of INyxEventSubscription;
    Navigation: INyxMenuFamilyRegistration;
    destructor Destroy; override;
  end;

  { Target seam: borrowed physical row/renderer, shared ownership and input.
    Adapters restore only the semantics they decorated, preserving host handlers. }
  TNyxMenuBarPresenter = class(TInterfacedObject, INyxMenuBar)
  private
    FContent: INyxRow;
    FEvents: INyxEvents;
    FCompletion: INyxEvents;
    FOptions: TNyxMenuBarOptions;
    FEntries: array of TNyxMenuBarEntry;
    FFocused: Integer;
    FChanging: Boolean;
    FSearch: INyxTypeAhead;
    FLease: TNyxMenuBarLease;
    FLeaseOwner: IInterface;
    function IndexOf(const APart: TNyxPartRef): Integer;
    function Boundary(ALast: Boolean): Integer;
    function Next(AFrom, AStep: Integer): Integer;
    function LabelAt(AIndex: Integer): TNyxText;
    procedure FocusIndex(AIndex: Integer);
    procedure OpenIndex(AIndex: Integer; AOpening: TNyxMenuOpening);
    procedure Input(AIndex: Integer; const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution);
    procedure Navigate(AIndex: Integer; ADirection: TNyxMenuFamilyDirection;
      const AExecution: INyxExecution);
    procedure Completed(AIndex: Integer; const AEvent: TNyxEventInfo);
  protected
    { Target adapters verify exact physical heading/anchor identity before any
      callback registration. No DOM or LCL type enters this portable seam. }
    procedure ValidateFamily(const AButton: INyxButton;
      const AMenu: INyxMenu); virtual; abstract;
    function ButtonAt(AIndex: Integer): INyxButton;
    function Visible(AIndex: Integer): Boolean;
    function Enabled(AIndex: Integer): Boolean;
    function TabIndex: Integer;
    function TextInput(const AText: TNyxText; ATimeMS: Double): Boolean;
    procedure PrepareFace(AIndex: Integer); virtual; abstract;
    procedure RestoreFace(AIndex: Integer); virtual; abstract;
    procedure ApplyFaces; virtual; abstract;
    function FocusFace(AIndex: Integer): Boolean; virtual; abstract;
    procedure TabExit(AIndex: Integer; AReverse: Boolean;
      const AExecution: INyxExecution); virtual; abstract;
    property Options: TNyxMenuBarOptions read FOptions;
  public
    constructor Create(const AContent: INyxRow; const AEvents: INyxEvents;
      const AOptions: TNyxMenuBarOptions);
    destructor Destroy; override;
    function Add(const APart: TNyxPartRef; const AMenu: INyxMenu;
      const AOptions: TNyxMenuOptions): INyxMenuBar;
    function Policy(const AOptions: TNyxMenuBarOptions): INyxMenuBar;
    function GetContent: INyxRow;
    function GetCount: Integer;
    function GetFocused: TNyxPartRef;
    function GetOpen: Boolean;
    function Menu(const APart: TNyxPartRef): INyxMenu;
    function Heading(AIndex: Integer): TNyxPartRef;
    procedure Focus(const APart: TNyxPartRef);
    procedure Open(const APart: TNyxPartRef; AOpening: TNyxMenuOpening);
    procedure Close;
    procedure SetEnabled(const APart: TNyxPartRef; AValue: Boolean);
    procedure Refresh;
    function OnInvoke: INyxEventStream;
    property Count: Integer read GetCount;
    property Content: INyxRow read GetContent;
  end;

function NyxMenuBar(const ALabel: TNyxText): TNyxMenuBarOptions;

implementation

uses nyx.interaction, nyx.errors;

type
  TBarInput = class(TNyxEventCallback)
  private
    FLease: TNyxMenuBarLease;
    FLeaseOwner: IInterface;
    FIndex: Integer;
    FCompletion: Boolean;
  public
    constructor Create(ALease: TNyxMenuBarLease; AIndex: Integer; ACompletion: Boolean);
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  TBarNavigator = class(TInterfacedObject, INyxMenuFamilyNavigator)
  private
    FLease: TNyxMenuBarLease;
    FLeaseOwner: IInterface;
    FIndex: Integer;
  public
    constructor Create(ALease: TNyxMenuBarLease; AIndex: Integer);
    procedure Navigate(ADirection: TNyxMenuFamilyDirection;
      const AExecution: INyxExecution);
  end;

var
  { UI-thread-only weak identities, never ownership. One exact mounted row has
    one coordinator; independent realized copies can each own their own bar. }
  GBarLeases: array of TNyxMenuBarLease;

function NyxMenuBar(const ALabel: TNyxText): TNyxMenuBarOptions;
begin
  Result := nyx.menu.types.NyxMenuBar(ALabel);
end;

destructor TNyxMenuBarEntry.Destroy;
var
  LIndex: Integer;
begin

  if Navigation <> nil then
  begin
    Navigation.Cancel;
  end;
  for LIndex := 0 to High(Tokens) do
  begin

    if Tokens[LIndex] <> nil then
    begin
      Tokens[LIndex].Cancel;
    end;
  end;
  Menu := nil;
  Button := nil;
  inherited Destroy;
end;

constructor TBarInput.Create(ALease: TNyxMenuBarLease; AIndex: Integer;
  ACompletion: Boolean);
begin
  inherited Create;
  FLease := ALease;
  FLeaseOwner := ALease;
  FIndex := AIndex;
  FCompletion := ACompletion;
end;

procedure TBarInput.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LOwner: INyxMenuBar;
begin

  if FLease.Owner <> nil then
  begin
    LOwner := FLease.Owner;

    if FCompletion then
    begin
      FLease.Owner.Completed(FIndex, AEvent);
    end
    else
    begin
      FLease.Owner.Input(FIndex, AEvent, AExecution);
    end;
    LOwner.GetCount;
  end;
end;

constructor TBarNavigator.Create(ALease: TNyxMenuBarLease; AIndex: Integer);
begin
  inherited Create;
  FLease := ALease;
  FLeaseOwner := ALease;
  FIndex := AIndex;
end;

procedure TBarNavigator.Navigate(ADirection: TNyxMenuFamilyDirection;
  const AExecution: INyxExecution);
var
  LOwner: INyxMenuBar;
begin

  if FLease.Owner <> nil then
  begin
    LOwner := FLease.Owner;
    FLease.Owner.Navigate(FIndex, ADirection, AExecution);
    LOwner.GetCount;
  end;
end;

constructor TNyxMenuBarPresenter.Create(const AContent: INyxRow;
  const AEvents: INyxEvents; const AOptions: TNyxMenuBarOptions);
var
  LIndex: Integer;
begin
  inherited Create;

  if (AContent = nil) or (AEvents = nil) then
  begin
    raise ENyxModel.Create('Menu bar requires a mounted Nyx row and input router');
  end;
  AEvents.Scheduler.RequireUI;
  AOptions.Validate;
  for LIndex := 0 to High(GBarLeases) do
  begin

    if (GBarLeases[LIndex].Owner <> nil) and
      (GBarLeases[LIndex].Owner.Content.Node = AContent.Node) then
    begin
      raise ENyxModel.Create('This exact mounted row already has a menu bar');
    end;
  end;
  FContent := AContent;
  FEvents := AEvents;
  FOptions := AOptions;
  FCompletion := NewNyxEvents;
  FSearch := NewNyxTypeAhead(AOptions.Search);
  FFocused := -1;
  FLease := TNyxMenuBarLease.Create;
  FLeaseOwner := FLease;
  FLease.Owner := Self;
  LIndex := Length(GBarLeases);
  SetLength(GBarLeases, LIndex + 1);
  GBarLeases[LIndex] := FLease;
end;

destructor TNyxMenuBarPresenter.Destroy;
var
  LIndex: Integer;
  LOther: Integer;
begin

  if FLease <> nil then
  begin
    FLease.Owner := nil;
  end;
  for LIndex := High(GBarLeases) downto 0 do
  begin

    if GBarLeases[LIndex] = FLease then
    begin
      for LOther := LIndex to High(GBarLeases) - 1 do
      begin
        GBarLeases[LOther] := GBarLeases[LOther + 1];
      end;
      SetLength(GBarLeases, Length(GBarLeases) - 1);
    end;
  end;
  Close;
  for LIndex := High(FEntries) downto 0 do
  begin
    FEntries[LIndex].Free;
  end;
  FEntries := nil;

  if FCompletion <> nil then
  begin
    FCompletion.Close;
  end;
  FCompletion := nil;
  FEvents := nil;
  FContent := nil;
  FLeaseOwner := nil;
  inherited Destroy;
end;

function TNyxMenuBarPresenter.Add(const APart: TNyxPartRef; const AMenu: INyxMenu;
  const AOptions: TNyxMenuOptions): INyxMenuBar;
var
  LIndex: Integer;
  LOther: Integer;
  LButton: INyxButton;
  LFamily: INyxMenuFamilyInput;
  LNavigator: INyxMenuFamilyNavigator;
  LCallback: INyxEventCallback;
  LAction: TNyxAction;
  LTarget: TNyxEventTarget;
  LStreams: array of INyxEventStream;
  LEntry: TNyxMenuBarEntry;
  LPrepared: Boolean;
begin
  FEvents.Scheduler.RequireUI;
  AOptions.Validate;

  if (APart.Name = '') or (AMenu = nil) or (Count >= 64) or GetOpen or
    not Supports(AMenu, INyxMenuFamilyInput, LFamily) or AMenu.IsOpen then
  begin
    raise ENyxModel.Create('Menu bar requires bounded headings and closed coordinatable menus');
  end;
  for LOther := 0 to High(FEntries) do
  begin

    if (FEntries[LOther].Part.Name = APart.Name) or (FEntries[LOther].Menu = AMenu) then
    begin
      raise ENyxModel.Create('Menu bar heading or family is already registered');
    end;
  end;

  if not Supports(FContent.Part(APart), INyxButton, LButton) then
  begin
    raise ENyxModel.Create('Menu bar heading must be a named Nyx button');
  end;

  if not NyxInteractionPolicy(LButton.Node).Enabled or
    not TryNyxAction(LButton.Node.Prop(NyxAttributeName(atAction)), LAction) or
    (LAction <> naNone) then
  begin
    raise ENyxModel.Create('Menu bar heading requires an enabled button without renderer action');
  end;
  for LOther := 0 to High(FEntries) do
  begin

    if FEntries[LOther].Button.ID = LButton.ID then
    begin
      raise ENyxModel.Create('Menu bar parts must identify distinct buttons');
    end;
  end;
  ValidateFamily(LButton, AMenu);
  LTarget := NyxControlEvents(LButton.ID);
  SetLength(LStreams, 4);
  LStreams[0] := FEvents.On(LTarget, ntClick);
  LStreams[1] := FEvents.OnKeyDown(LTarget);
  LStreams[2] := FEvents.OnAfterEnter(LTarget);
  LStreams[3] := FEvents.OnPointerEnter(LTarget);
  for LOther := 0 to High(LStreams) do
  begin

    if LStreams[LOther].ExecutionPolicy <> neSequential then
    begin
      raise ENyxModel.Create('Menu bar input requires sequential UI streams');
    end;
  end;

  if AMenu.OnInvoke.ExecutionPolicy <> neSequential then
  begin
    raise ENyxModel.Create('Menu bar family forwarding requires sequential UI completion');
  end;
  LIndex := Count;
  LEntry := TNyxMenuBarEntry.Create;
  SetLength(LEntry.Tokens, 5);
  LPrepared := False;
  SetLength(FEntries, LIndex + 1);
  FEntries[LIndex] := LEntry;
  try
    LEntry.Part := APart;
    LEntry.Button := LButton;
    LEntry.Menu := AMenu;
    LEntry.Options := AOptions;
    LEntry.Enabled := True;
    { Give callbacks an explicit managed owner BEFORE invoking an extension.
      Native compiler argument temporaries can leak a freshly constructed class
      when a foreign coordinator/subscription refuses by raising an exception. }
    LNavigator := TBarNavigator.Create(FLease, LIndex);
    LEntry.Navigation := LFamily.ConnectNavigation(LNavigator);
    PrepareFace(LIndex);
    LPrepared := True;
    for LOther := 0 to High(LStreams) do
    begin
      LCallback := TBarInput.Create(FLease, LIndex, False);
      LEntry.Tokens[LOther] := LStreams[LOther].Subscribe(LCallback);
    end;
    LCallback := TBarInput.Create(FLease, LIndex, True);
    LEntry.Tokens[4] := AMenu.OnInvoke.Subscribe(LCallback);

    if (FFocused < 0) and Visible(LIndex) then
    begin
      FFocused := LIndex;
    end;
    ApplyFaces;
  except

    if LPrepared then
    begin
      RestoreFace(LIndex);
    end;
    LEntry.Free;
    SetLength(FEntries, LIndex);

    if FFocused >= LIndex then
    begin
      FFocused := Boundary(False);
    end;
    raise;
  end;
  Result := Self;
end;

function TNyxMenuBarPresenter.Policy(const AOptions: TNyxMenuBarOptions): INyxMenuBar;
begin
  FEvents.Scheduler.RequireUI;
  AOptions.Validate;
  FSearch := NewNyxTypeAhead(AOptions.Search);
  FOptions := AOptions;
  ApplyFaces;
  Result := Self;
end;

function TNyxMenuBarPresenter.GetContent: INyxRow;
begin
  Result := FContent;
end;

function TNyxMenuBarPresenter.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function TNyxMenuBarPresenter.GetFocused: TNyxPartRef;
begin
  Result := NyxPart('');

  if (FFocused >= 0) and (FFocused < Count) then
  begin
    Result := FEntries[FFocused].Part;
  end;
end;

function TNyxMenuBarPresenter.GetOpen: Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex].Menu.IsOpen then
    begin
      Exit(True);
    end;
  end;
end;

function TNyxMenuBarPresenter.IndexOf(const APart: TNyxPartRef): Integer;
begin
  for Result := 0 to High(FEntries) do
  begin

    if FEntries[Result].Part.Name = APart.Name then
    begin
      Exit;
    end;
  end;
  raise ENyxModel.Create('Menu bar heading is not registered');
end;

function TNyxMenuBarPresenter.Menu(const APart: TNyxPartRef): INyxMenu;
begin
  Result := FEntries[IndexOf(APart)].Menu;
end;

function TNyxMenuBarPresenter.Heading(AIndex: Integer): TNyxPartRef;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxModel.Create('Menu bar heading index is outside its bindings');
  end;
  Result := NyxPart(FEntries[AIndex].Part.Name);
end;

function TNyxMenuBarPresenter.ButtonAt(AIndex: Integer): INyxButton;
begin
  Result := FEntries[AIndex].Button;
end;

function TNyxMenuBarPresenter.Visible(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Count) and
    NyxInteractionPolicy(FEntries[AIndex].Button.Node).Visible;
end;

function TNyxMenuBarPresenter.Enabled(AIndex: Integer): Boolean;
begin
  Result := Visible(AIndex) and FEntries[AIndex].Enabled and
    NyxInteractionPolicy(FEntries[AIndex].Button.Node).CanIssueCommand;
end;

function TNyxMenuBarPresenter.Boundary(ALast: Boolean): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Count - 1 do
  begin
    Result := LIndex;

    if ALast then
    begin
      Result := Count - 1 - LIndex;
    end;

    if Visible(Result) then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TNyxMenuBarPresenter.Next(AFrom, AStep: Integer): Integer;
var
  LCount: Integer;
begin
  Result := AFrom;
  for LCount := 1 to Count do
  begin
    Inc(Result, AStep);

    if (Result < 0) or (Result >= Count) then
    begin

      if not FOptions.Wraps then
      begin
        Exit(AFrom);
      end;
      Result := (Result + Count) mod Count;
    end;

    if Visible(Result) then
    begin
      Exit;
    end;
  end;
  Result := AFrom;
end;

function TNyxMenuBarPresenter.TabIndex: Integer;
begin
  Result := FFocused;

  if not Visible(Result) then
  begin
    Result := Boundary(False);
  end;
end;

function TNyxMenuBarPresenter.LabelAt(AIndex: Integer): TNyxText;
begin
  Result := '';

  if Visible(AIndex) then
  begin
    Result := FEntries[AIndex].Button.Text;
  end;
end;

procedure TNyxMenuBarPresenter.FocusIndex(AIndex: Integer);
begin

  if not Visible(AIndex) then
  begin
    raise ENyxModel.Create('Menu bar heading is not visible');
  end;
  FFocused := AIndex;
  ApplyFaces;

  if not FocusFace(AIndex) then
  begin
    raise ENyxModel.Create('Menu bar heading cannot receive physical focus');
  end;
end;

procedure TNyxMenuBarPresenter.Focus(const APart: TNyxPartRef);
begin
  FEvents.Scheduler.RequireUI;
  FChanging := True;
  try
    Close;
    FocusIndex(IndexOf(APart));
    FSearch.Reset;
  finally
    FChanging := False;
  end;
end;

procedure TNyxMenuBarPresenter.OpenIndex(AIndex: Integer; AOpening: TNyxMenuOpening);
begin

  if not Enabled(AIndex) then
  begin
    Exit;
  end;
  FChanging := True;
  try
    Close;
    FocusIndex(AIndex);
    FEntries[AIndex].Menu.Open(FEntries[AIndex].Options.Opening(AOpening));
  finally
    FChanging := False;
  end;
end;

procedure TNyxMenuBarPresenter.Open(const APart: TNyxPartRef; AOpening: TNyxMenuOpening);
begin
  FEvents.Scheduler.RequireUI;
  OpenIndex(IndexOf(APart), AOpening);
  FSearch.Reset;
end;

procedure TNyxMenuBarPresenter.Close;
var
  LIndex: Integer;
begin
  { Constructor failure may call destruction before the input router exists. }

  if FEvents <> nil then
  begin
    FEvents.Scheduler.RequireUI;
  end;
  for LIndex := 0 to High(FEntries) do
  begin

    if (FEntries[LIndex].Menu <> nil) and FEntries[LIndex].Menu.IsOpen then
    begin
      FEntries[LIndex].Menu.Close;
    end;
  end;
end;

procedure TNyxMenuBarPresenter.SetEnabled(const APart: TNyxPartRef; AValue: Boolean);
var
  LIndex: Integer;
begin
  FEvents.Scheduler.RequireUI;
  LIndex := IndexOf(APart);
  FEntries[LIndex].Enabled := AValue;

  if not AValue then
  begin
    FEntries[LIndex].Menu.Close;
  end;
  ApplyFaces;
end;

procedure TNyxMenuBarPresenter.Refresh;
var
  LIndex: Integer;
begin
  FEvents.Scheduler.RequireUI;
  for LIndex := 0 to High(FEntries) do
  begin

    if not Enabled(LIndex) then
    begin
      FEntries[LIndex].Menu.Close;
    end;
  end;

  if not Visible(FFocused) then
  begin
    FFocused := Boundary(False);
  end;
  ApplyFaces;
end;

function TNyxMenuBarPresenter.OnInvoke: INyxEventStream;
begin
  Result := FCompletion.OnNamed(NyxCompoundEvents(Content.ID), NyxSemantic(nseActivate));
end;

procedure TNyxMenuBarPresenter.Completed(AIndex: Integer; const AEvent: TNyxEventInfo);
var
  LEvent: TNyxEventInfo;
  LCompletion: INyxEvents;
begin
  NyxMenuInvocation(AEvent);
  LEvent := AEvent.Copy;
  LEvent.SourceID := Content.ID;
  LEvent.TargetID := Content.ID;
  LEvent.OriginID := FEntries[AIndex].Button.ID;
  LCompletion := FCompletion;
  LCompletion.Dispatch(LEvent, LEvent.OriginID, LEvent.SourceID);
end;

procedure TNyxMenuBarPresenter.Navigate(AIndex: Integer; ADirection: TNyxMenuFamilyDirection;
  const AExecution: INyxExecution);
var
  LTarget: Integer;
  LResponse: INyxEventResponse;
begin
  FEvents.Scheduler.RequireUI;
  LResponse := NyxEventResponse(AExecution);

  if not LResponse.CanConsume or LResponse.Consumed then
  begin
    Exit;
  end;

  if ADirection in [nmfTabForward, nmfTabBackward] then
  begin
    Close;
    TabExit(AIndex, ADirection = nmfTabBackward, AExecution);
    Exit;
  end;
  LResponse.Consume;
  LTarget := Next(AIndex, 1);

  if ADirection = nmfPrevious then
  begin
    LTarget := Next(AIndex, -1);
  end;
  Close;

  if LTarget >= 0 then
  begin
    FChanging := True;
    try
      FocusIndex(LTarget);
    finally
      FChanging := False;
    end;
    OpenIndex(LTarget, nmoFirst);
  end;
  FSearch.Reset;
end;

procedure TNyxMenuBarPresenter.Input(AIndex: Integer; const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LResponse: INyxEventResponse;
  LTarget: Integer;
  LWasOpen: Boolean;
  LOpening: TNyxMenuOpening;
begin
  FEvents.Scheduler.RequireUI;

  if FChanging or not Visible(AIndex) then
  begin
    Exit;
  end;

  if AEvent.Trigger = ntAfterEnter then
  begin
    FFocused := AIndex;
    ApplyFaces;
    Exit;
  end;

  if AEvent.Trigger = ntPointerEnter then
  begin

    if not AEvent.DefaultPrevented and FOptions.Hovers and GetOpen and
      Enabled(AIndex) and AEvent.HasPointer and
      (AEvent.Pointer.Kind = npiMouse) and not FEntries[AIndex].Menu.IsOpen then
    begin
      OpenIndex(AIndex, nmoFirst);
      FSearch.Reset;
    end;
    Exit;
  end;

  if AEvent.DefaultPrevented then
  begin
    Exit;
  end;

  { Click is an admitted activation notification, with no cancellable keyboard
    lease. Requiring CanConsume here discards ordinary renderer clicks on both
    targets. Cancellation admission belongs to the keyboard path below. }
  if AEvent.Trigger = ntClick then
  begin

    if FEntries[AIndex].Menu.IsOpen then
    begin
      Close;
    end
    else
    begin
      OpenIndex(AIndex, nmoFirst);
    end;
    FSearch.Reset;
    Exit;
  end;
  LResponse := NyxEventResponse(AExecution);

  if not LResponse.CanConsume or LResponse.Consumed then
  begin
    Exit;
  end;

  if not AEvent.HasKeyboard or (AEvent.Keyboard.Modifiers - [nmShift] <> []) then
  begin
    Exit;
  end;

  if AEvent.Keyboard.Key = nkTabKey then
  begin
    Close;
    TabExit(AIndex, nmShift in AEvent.Keyboard.Modifiers, AExecution);
    Exit;
  end;

  if AEvent.Keyboard.Modifiers <> [] then
  begin
    Exit;
  end;
  FFocused := AIndex;
  LTarget := -1;
  case AEvent.Keyboard.Key of
    nkLeftKey:
      begin
        LTarget := Next(AIndex, -1);
      end;
    nkRightKey:
      begin
        LTarget := Next(AIndex, 1);
      end;
    nkHomeKey:
      begin
        LTarget := Boundary(False);
      end;
    nkEndKey:
      begin
        LTarget := Boundary(True);
      end;
    nkEnterKey, nkSpaceKey, nkDownKey, nkUpKey:
      begin
        LResponse.Consume;
        LOpening := nmoFirst;

        if AEvent.Keyboard.Key = nkUpKey then
        begin
          LOpening := nmoLast;
        end;

        if not AEvent.Keyboard.Repeating then
        begin
          OpenIndex(AIndex, LOpening);
        end;
        FSearch.Reset;
        Exit;
      end;
    nkEscapeKey:
      begin
        LResponse.Consume;
        Close;
        FSearch.Reset;
        Exit;
      end;
  else
    begin
      Exit;
    end;
  end;
  LResponse.Consume;
  LWasOpen := GetOpen;
  FChanging := True;
  try
    Close;
    FocusIndex(LTarget);
  finally
    FChanging := False;
  end;

  if LWasOpen then
  begin
    OpenIndex(LTarget, nmoFirst);
  end;
  FSearch.Reset;
end;

function TNyxMenuBarPresenter.TextInput(const AText: TNyxText; ATimeMS: Double): Boolean;
var
  LTarget: Integer;
  LWasOpen: Boolean;
begin
  FEvents.Scheduler.RequireUI;
  Result := False;

  if (Count = 0) or not FOptions.Search.IsEnabled then
  begin
    Exit;
  end;
  LTarget := FSearch.Find(AText, ATimeMS, Count, FFocused, LabelAt);

  if LTarget >= 0 then
  begin
    LWasOpen := GetOpen;
    FChanging := True;
    try
      Close;
      FocusIndex(LTarget);
    finally
      FChanging := False;
    end;

    if LWasOpen then
    begin
      OpenIndex(LTarget, nmoFirst);
    end;
    Result := True;
  end;
end;

end.
