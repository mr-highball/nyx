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
program nyx_view_section_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Classes, Forms, Controls, StdCtrls, ExtCtrls,
    Types, LCLType, LCLIntf, LMessages, Grids, nyx.editing.lcl, nyx.collections.lcl.grid,
    nyx.test.capture.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.data, nyx.coordinators, nyx.collections.bindings, nyx.publication,
  nyx.behavior, nyx.view.sections, nyx.collections, nyx.collections.view.types,
  nyx.collections.arrangement, nyx.editing,
  nyx.collections.mount, nyx.collections.view, nyx.events, nyx.scheduler
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.view.sections.browser
  {$else}, nyx.render.lcl, nyx.view.sections.lcl{$endif};

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  ISection = INyxBrowserViewSection;
  TFace = TJSHTMLElement;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  ISection = INyxLCLViewSection;
  TFace = TControl;
  THost = TPanel;
  TControlAccess = class(TControl);
  {$endif}

  {$ifndef PAS2JS}
  { The ordinary accepted host stays unarmed through every original case. A
    prepared native candidate's real visibility notification injects one failure
    AFTER activation, replacing the obsolete physical-reparent failure probe.
    This callback borrows the sender for that call only; no candidate is owned. }
  TPhysicalMoveHost = class(TPanel)
  public
    Armed: Boolean;
    Entered: Boolean;
    OldHidden: Boolean;
    Activated: Boolean;
    Attempts: Integer;
    procedure PlaneVisibilityChanged(Sender: TObject);
  end;
  {$endif}

  { Managed observer contains no section/view reference. The candidate's same
    IDs must not impersonate an accepted node at this receiver. }
  TScenario = class(TInterfacedObject, INyxViewSectionObserver)
  public
    Clicks: Integer;
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Configure(ARenderer: TRenderer);
  end;

  { Fixture-only temporary registration. Cancel its token and release Change
    before disposal so its strong handle never forms a persistent router cycle. }
  TReentrantProbe = class(TNyxEventCallback)
  public
    Change: INyxViewSectionChange;
    Refused: Boolean;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
    procedure ViewChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;

  { Shared cleanup observation is borrowed by the ordinary creator factory.
    The fixture keeps it alive through all candidates and accepted views, while
    each participant owns only copied identity and its own lifecycle flags. }
  TSectionCleanupProbe = class
  public
    CleanupAttempts: Integer;
    CoordinatorsCreated: Integer;
    CoordinatorsDestroyed: Integer;
    FailCleanup: Boolean;
    {$ifdef PAS2JS}
    ThrowFirstUndefined: Boolean;
    CaptureCleanupScopes: Boolean;
    CleanupScopes: array of INyxCoordinatorProjection;
    {$endif}
    procedure Configure(ARenderer: TRenderer);
  end;

  {$ifndef PAS2JS}
  { Direct renderer streams and the exact accepted data store are observed
    independently of the section's muted candidate observer. Tokens are always
    detached before this receiver's managed lease is released. }
  TTransitionProbe = class(TNyxEventCallback)
  public
    FocusEvents: Integer;
    Changes: Integer;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
    procedure StoreChanged(const ACollection: INyxCollection;
      const AChanges: INyxCollectionChanges);
  end;

  { The ordinary factory returns this TButton descendant on native targets.
    Its optional nonthrowing destruction callback tests real owned control
    disposal. Fallible cleanup belongs to the explicit coordinator port below. }
  TCleanupButton = class(TButton)
  public
    ControlID: TNyxText;
    OnRetire: TNotifyEvent;
    destructor Destroy; override;
  end;

  { Destruction either probes synchronous refusal or observes final disposal;
    neither mode violates the native nonthrowing destructor obligation. }
  TNativeCleanupMode = (ncmReenter, ncmObserve);

  { A native-only borrowed interface pointer is invoked solely from its actual
    entered control destruction callback. It deliberately owns NO section lease
    so final-owner release can test terminal busy refusal without resurrection.
    The caller retains this receiver and its document/host through every call. }
  TNativeCleanupProbe = class(TSectionCleanupProbe)
  public
    BorrowedSection: Pointer;
    Document: TNyxDocument;
    Host: TWinControl;
    Mode: TNativeCleanupMode;
    Calls: Integer;
    CloseRefused: Boolean;
    PrepareRefused: Boolean;
    ObserverReleases: Integer;
    OwnerReleasedAfterPlanes: Boolean;
    PreviewShows: Integer;
    procedure Retiring(ASender: TObject);
    procedure Previewed(ASender: TObject);
  end;

  { Final observer release records whether publication kept the owner alive
    until the actual stage plane was gone. Its destructor never raises. }
  TCleanupObserver = class(TInterfacedObject, INyxViewSectionObserver)
  public
    Probe: TNativeCleanupProbe;
    destructor Destroy; override;
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;
  {$endif}

  { An ordinary managed creator participant owns copied role identity and local
    lifecycle flags. Disconnect is the existing supported fallible cleanup
    boundary; both failures happen before nonthrowing final object release. }
  TCleanupCoordinator = class(TInterfacedObject, INyxPreparedCoordinator,
    INyxPreparedPublication, INyxPublicationReadiness, INyxCoordinatorBinding)
  private
    FProbe: TSectionCleanupProbe;
    FRole: TNyxControlRef;
    FOrdinal: Integer;
    FValidated: Boolean;
    FInstalled: Boolean;
    FRevoked: Boolean;
    FDisconnected: Boolean;
  public
    constructor Create(AProbe: TSectionCleanupProbe;
      const ARole: TNyxControlRef; AOrdinal: Integer);
    destructor Destroy; override;
    function GetBinding: INyxCoordinatorBinding;
    function GetConnected: Boolean;
    procedure Revoke;
    procedure Disconnect;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { The same target candidate receives two independently owned participants
    through the normal renderer registry, without another publication engine. }
  TCleanupCoordinatorFactory = class(TInterfacedObject, INyxCoordinatorFactory)
  public
    Probe: TSectionCleanupProbe;
    function Prepare(const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
  end;

const
  CNativeCleanupIntent: TNyxText = 'example.section-cleanup';

var
  GChecks: Integer;
  GFailBuild: Boolean;
  GFailUpdate: Boolean;
  GFailCleanupHeaderPreview: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('View sections: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifdef PAS2JS}
function CustomButton(ANode: TNyxNode): TJSHTMLElement;
begin

  if GFailBuild then
  begin
    raise ENyxModel.Create('Intentional section factory refusal');
  end;
  Result := TJSHTMLElement(document.createElement('button'));
  Result.className := 'nyx-button';
  Result.textContent := ANode.Prop('text');
end;

procedure UpdateButton(ANode: TNyxNode; AFace: TJSHTMLElement);
begin

  if GFailUpdate then
  begin
    GFailUpdate := False;
    raise ENyxModel.Create('Intentional section preview refusal');
  end;
  AFace.textContent := ANode.Prop('text');
end;
{$else}
function CustomButton(ANode: TNyxNode; AOwner: TComponent): TControl;
begin

  if GFailBuild then
  begin
    raise ENyxModel.Create('Intentional section factory refusal');
  end;
  Result := TCleanupButton.Create(AOwner);
  TCleanupButton(Result).ControlID := ANode.ID;
  TButton(Result).Caption := ANode.Prop('text');
end;

procedure UpdateButton(ANode: TNyxNode; AFace: TControl);
begin

  if GFailUpdate then
  begin
    GFailUpdate := False;
    raise ENyxModel.Create('Intentional section preview refusal');
  end;
  TButton(AFace).Caption := ANode.Prop('text');
end;
{$endif}

procedure TScenario.Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (AEvent.Trigger = ntClick) and (ANode.Kind = NyxKindName(nkButton)) then
  begin
    Inc(Clicks);
  end;
end;

procedure TScenario.Configure(ARenderer: TRenderer);
begin
  ARenderer.RegisterFactory(NyxKindName(nkButton), CustomButton, UpdateButton);
end;

procedure TReentrantProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin

  if (AEvent.Trigger = ntClick) and (AExecution <> nil) then
  begin
    Refused := not PublishNyxViewSections([Change]);
  end;
end;

procedure TReentrantProbe.ViewChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin

  if (AView <> nil) and (AChanges = nil) then
  begin
    Refused := not PublishNyxViewSections([Change]);
  end;
end;

function Example(const APrefix, ATitle: TNyxText; AExtra: Boolean): TNyxDocument;
var
  LPage: INyxColumn;
  LTable: INyxTable;
begin
  Result := TNyxDocument.Create;
  try
    LPage := NewNyxColumn(APrefix + '-section');
    LPage.Configure.Padding(18).Gap(12).Done;
    LPage.Add(NewNyxHeading(APrefix + '-heading').WithText(ATitle));
    LPage.Add(NewNyxInput(APrefix + '-input').WithText('Design notes')
      .Configure.Value('Ready to create').Done);
    LPage.Add(NewNyxButton(APrefix + '-action').WithText('Save notes'));
    Result.Collections.Define(NyxCollection(APrefix + '-notes'),
      NyxCollectionSchema.Text(NyxTextField('title'), ''),
      [NyxCollectionItem(NyxItem(NyxCollection(APrefix + '-notes'), 'first'))
        .WithValue(NyxTextField('title'), 'A thoughtful interface')]);
    LTable := NewNyxTable(APrefix + '-table');
    LTable.Configure.Height(96).Done;
    LTable.Binds.Collection(NyxCollectionView(NyxCollection(APrefix + '-notes'))
      .Column(NyxTextField('title'), 'Note', cmEditable)).Done;
    LPage.Add(LTable);

    if AExtra then
    begin
      LPage.Add(NewNyxLabel(APrefix + '-extra').WithText('A new independently published control.'));
    end;
    Result.AddPage(LPage);
  except
    Result.Free;
    raise;
  end;
end;

function Face(ASection: ISection; const AID: TNyxText): TFace;
begin
  {$ifdef PAS2JS}
  Result := ASection.Renderer.ElementFor(AID);
  {$else}
  Result := ASection.Renderer.ControlFor(AID);
  {$endif}
end;

function Input(ASection: ISection; const AID: TNyxText): TFace;
begin
  Result := ASection.Renderer.InputFor(AID);
end;

function TextOf(AFace: TFace): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(AFace).value;
  {$else}
  Result := TCustomEdit(AFace).Text;
  {$endif}
end;

procedure Draft(AFace: TFace);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(AFace).value := 'Independent draft / 🌙';
  AFace.dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(AFace).Text := TNyxText('Independent draft / 🌙');
  {$endif}
end;

procedure Click(AFace: TFace);
begin
  {$ifdef PAS2JS}
  AFace.click;
  {$else}
  TControlAccess(AFace).Click;
  {$endif}
end;

{$ifndef PAS2JS}
destructor TCleanupButton.Destroy;
var
  LRetire: TNotifyEvent;
begin
  LRetire := OnRetire;
  OnRetire := nil;
  try

    if Assigned(LRetire) then
    begin
      LRetire(Self);
    end;
  finally
    inherited Destroy;
  end;
end;

procedure TNativeCleanupProbe.Retiring(ASender: TObject);
var
  LChange: INyxViewSectionChange;
begin
  Inc(Calls);

  if Mode = ncmObserve then
  begin
    Exit;
  end;
  CloseRefused := False;
  PrepareRefused := False;
  try
    ISection(BorrowedSection).Close;
  except
    on LException: ENyxModel do
    begin

      if Pos('cannot close during dispatch/publication', LException.Message) = 0 then
      begin
        raise;
      end;
      CloseRefused := True;
    end;
  end;
  try
    LChange := ISection(BorrowedSection).Prepare(Document, Document.Pages[0]);
    LChange.Cancel;
  except
    on LException: ENyxModel do
    begin

      if Pos('cannot prepare during dispatch/publication', LException.Message) = 0 then
      begin
        raise;
      end;
      PrepareRefused := True;
    end;
  end;
end;

{$endif}

{ The second grouped browser member fails its REAL ordinary updater during
  MoveHost synchronization. Preparation and native physical activation retain
  their own existing failure seams; no successful participant is fabricated. }
procedure CleanupUpdateButton(ANode: TNyxNode;
  AFace: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif});
begin

  if GFailCleanupHeaderPreview and (ANode.ID = 'header-action') then
  begin
    GFailCleanupHeaderPreview := False;
    raise ENyxModel.Create('Intentional second grouped section preview refusal');
  end;
  UpdateButton(ANode, AFace);
end;

procedure TSectionCleanupProbe.Configure(ARenderer: TRenderer);
var
  LRegistry: INyxCoordinatorRegistry;
  LFactory: TCleanupCoordinatorFactory;
  LFactoryLease: INyxCoordinatorFactory;
begin
  ARenderer.RegisterFactory(NyxKindName(nkButton), CustomButton, CleanupUpdateButton);
  LRegistry := NewNyxCoordinatorRegistry;
  LFactory := TCleanupCoordinatorFactory.Create;
  LFactory.Probe := Self;
  LFactoryLease := LFactory;
  LRegistry.RegisterFactory(NyxCustomKind(NyxKindName(nkButton)),
    NyxExtension(CNativeCleanupIntent), LFactoryLease);
  LRegistry.RegisterFactory(NyxCustomKind(NyxKindName(nkInput)),
    NyxExtension(CNativeCleanupIntent), LFactoryLease);
  ARenderer.CoordinatorRegistry := LRegistry;
end;

{$ifndef PAS2JS}
procedure TNativeCleanupProbe.Previewed(ASender: TObject);
begin

  if (ASender is TWinControl) and TWinControl(ASender).Visible and
    (TWinControl(ASender).Parent = Host) then
  begin
    Inc(PreviewShows);
  end;
end;

destructor TCleanupObserver.Destroy;
var
  LProbe: TNativeCleanupProbe;
begin
  LProbe := Probe;
  Probe := nil;
  Inc(LProbe.ObserverReleases);
  LProbe.OwnerReleasedAfterPlanes := LProbe.Host.ControlCount = 0;
  inherited Destroy;
end;

procedure TCleanupObserver.Changed(ANode: TNyxNode;
  const AEvent: TNyxEventInfo);
begin
  { This receiver exists only for its real managed final-release boundary. }
end;
{$endif}

constructor TCleanupCoordinator.Create(AProbe: TSectionCleanupProbe;
  const ARole: TNyxControlRef; AOrdinal: Integer);
begin
  inherited Create;
  FProbe := AProbe;
  FRole := ARole;
  FOrdinal := AOrdinal;
  Inc(FProbe.CoordinatorsCreated);
end;

destructor TCleanupCoordinator.Destroy;
begin
  Inc(FProbe.CoordinatorsDestroyed);
  inherited Destroy;
end;

function TCleanupCoordinator.GetBinding: INyxCoordinatorBinding;
begin
  Result := Self;
end;

function TCleanupCoordinator.GetConnected: Boolean;
begin
  Result := FInstalled and not FRevoked and not FDisconnected;
end;

procedure TCleanupCoordinator.Revoke;
begin
  FRevoked := True;
end;

procedure TCleanupCoordinator.Disconnect;
begin

  if FDisconnected then
  begin
    Exit;
  end;
  FDisconnected := True;
  FRevoked := True;
  Inc(FProbe.CleanupAttempts);

  if not FProbe.FailCleanup then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}

  if FProbe.ThrowFirstUndefined and (FOrdinal = 1) then
  begin
    { Exception(value) inserts rtl.getObject and would coerce undefined to null.
      This bridge injects the real platform value at the supported boundary. }
    asm
      throw undefined;
    end;
  end;
  {$endif}

  if FOrdinal = 1 then
  begin
    raise ENyxCoordinator.Create('Intentional first section coordinator cleanup refusal');
  end;
  raise ENyxCoordinator.Create('Intentional later section coordinator cleanup refusal');
end;

procedure TCleanupCoordinator.Validate;
begin

  if (FRole.ID = '') or FRevoked or FDisconnected then
  begin
    raise ENyxCoordinator.Create('Cleanup participant requires its exact live copied role');
  end;
  FValidated := True;
end;

procedure TCleanupCoordinator.CheckReady;
begin

  if not FValidated or FRevoked or FDisconnected then
  begin
    raise ENyxCoordinatorCancelled.Create('Cleanup participant was revoked before install');
  end;
end;

procedure TCleanupCoordinator.Install;
begin
  FInstalled := True;
end;

procedure TCleanupCoordinator.Notify;
begin
  { This cleanup-only creator has no model notifications or user callbacks. }
end;

procedure TCleanupCoordinator.Retire;
begin
  { No separate preparation resource remains after this participant installs. }
end;

function TCleanupCoordinatorFactory.Prepare(const AProjection: INyxCoordinatorProjection;
  const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
  const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
{$ifdef PAS2JS}
var
  LIndex: Integer;
{$endif}
begin

  if (AIntent.AsInteger < 1) or (AIntent.AsInteger > 2) then
  begin
    raise ENyxCoordinator.Create('Cleanup participant requires its exact typed ordinal');
  end;
  {$ifdef PAS2JS}

  if Probe.CaptureCleanupScopes then
  begin
    { The real driver owns an ordinary enabled-policy claim for EACH observed
      participant; retained facades must be inert after candidate cancellation. }
    AProjection.Enabled(AProjection.Root, True);
    LIndex := Length(Probe.CleanupScopes);
    SetLength(Probe.CleanupScopes, LIndex + 1);
    Probe.CleanupScopes[LIndex] := AProjection;
  end;
  {$endif}
  Result := TCleanupCoordinator.Create(Probe, AProjection.Root, AIntent.AsInteger);
end;

{$ifndef PAS2JS}
procedure TTransitionProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if AEvent.Trigger in [ntAfterEnter, ntAfterExit, ntChange] then
  begin
    Inc(FocusEvents);
  end;
end;

procedure TTransitionProbe.StoreChanged(const ACollection: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin

  if AChanges <> nil then
  begin
    Inc(Changes);
  end;
end;

procedure TPhysicalMoveHost.PlaneVisibilityChanged(Sender: TObject);
var
  LIndex: Integer;
  LPlane: TWinControl;
begin

  if Armed and (Sender is TWinControl) and TWinControl(Sender).Visible then
  begin
    LPlane := TWinControl(Sender);
    Armed := False;
    Entered := True;
    Inc(Attempts);
    OldHidden := True;
    for LIndex := 0 to ControlCount - 1 do
    begin

      if (Controls[LIndex] <> LPlane) and Controls[LIndex].Visible then
      begin
        OldHidden := False;
      end;
    end;
    Activated := (LPlane.Parent = Self) and LPlane.Visible;
    raise ENyxModel.Create('Intentional physical section activation refusal');
  end;
end;

procedure NativePublicationMoveRecovery(ASection: ISection;
  AHost: TPhysicalMoveHost; ADocument: TNyxDocument; AObserver: TScenario);
var
  LRenderer: TRenderer;
  LChange: INyxViewSectionChange;
  LParent: TWinControl;
  LScrollHost: TWinControl;
  LPlane: TWinControl;
  LCandidatePlane: TWinControl;
  LRootFace: TControl;
  LRoot: TNyxNode;
  LInput: TFace;
  LAction: TFace;
  LMount: INyxCollectionMount;
  LText: TNyxText;
  LOriginalWidth: Integer;
  LHostWidth: Integer;
  LParentChildren: Integer;
  LRevision: Integer;
  LClicks: Integer;
  LRefused: Boolean;
  LBefore: TNyxText;
  LIndex: Integer;
  LInputHandle: HWND;
  LScrollHandle: HWND;
  LPlaneHandle: HWND;
  LSelection: TNyxTextSelection;
  LActualSelection: TNyxTextSelection;
  LInputHandleSame: Boolean;
  LScrollHandleSame: Boolean;
  LPlaneHandleSame: Boolean;
  LFocusSame: Boolean;
  LProbe: TTransitionProbe;
  LProbeLease: INyxEventCallback;
  LEnterToken: INyxEventSubscription;
  LExitToken: INyxEventSubscription;
  LChangeToken: INyxEventSubscription;
begin
  LRenderer := ASection.Renderer;
  LRoot := ASection.Root;
  LRevision := ASection.Revision;
  LRootFace := LRenderer.ControlFor(LRoot.ID);
  LScrollHost := LRootFace.Parent;
  LPlane := LScrollHost.Parent;
  LInput := Input(ASection, 'header-input');
  LAction := Face(ASection, 'header-action');
  LMount := LRenderer.CollectionMount('header-table');
  Draft(LInput);
  LText := TextOf(LInput);
  TWinControl(LInput).SetFocus;
  LSelection := NyxTextSelection(LText, 2, 8, ntdUnknown);
  SelectNyxLCLText(TWinControl(LInput), LSelection);
  LInputHandle := TWinControl(LInput).Handle;
  LScrollHandle := LScrollHost.Handle;
  LPlaneHandle := LPlane.Handle;
  LOriginalWidth := LRootFace.Width;
  LHostWidth := AHost.Width;
  LParent := AHost.Parent;
  LParentChildren := LParent.ControlCount;
  LClicks := AObserver.Clicks;
  LBefore := TNyxCodec.Encode(ADocument);
  LProbe := TTransitionProbe.Create;
  LProbeLease := LProbe;
  LEnterToken := LRenderer.Events.OnAfterEnter(
    NyxControlEvents('header-input', niRuntime)).Subscribe(LProbeLease);
  LExitToken := LRenderer.Events.OnAfterExit(
    NyxControlEvents('header-input', niRuntime)).Subscribe(LProbeLease);
  LChangeToken := LRenderer.Events.On(
    NyxControlEvents('header-input', niRuntime), ntChange).Subscribe(LProbeLease);
  try
    Check(not AHost.Armed and (LScrollHost.Parent = LPlane) and (LPlane.Parent = AHost) and
      (AHost.ControlCount = 1) and (LOriginalWidth > AHost.ClientWidth - 100),
      'native publication recovery starts with one accepted tree in the unchanged host');
    LChange := ASection.Prepare(ADocument, ADocument.Pages[0]);
    Check((LChange.State = vcsPrepared) and (ASection.Renderer = LRenderer) and
      (ASection.Root = LRoot) and (Input(ASection, 'header-input') = LInput) and
      (TextOf(LInput) = LText) and not AHost.Armed,
      'native preparation retains accepted identities and draft before arming the host');
    Check((TWinControl(LInput).Handle = LInputHandle) and
      (LScrollHost.Handle = LScrollHandle) and (LPlane.Handle = LPlaneHandle),
      'native preparation never reparents or recreates the accepted HWND cohort');
    LCandidatePlane := nil;
    for LIndex := 0 to AHost.ControlCount - 1 do
    begin

      if (AHost.Controls[LIndex] <> LPlane) and
        (AHost.Controls[LIndex] is TWinControl) then
      begin
        LCandidatePlane := TWinControl(AHost.Controls[LIndex]);
      end;
    end;
    Check((LCandidatePlane <> nil) and not LCandidatePlane.Visible and
      (LCandidatePlane.Parent = AHost),
      'native candidate is prepared hidden under the SAME stable slot');
    LCandidatePlane.AddHandlerOnVisibleChanged(AHost.PlaneVisibilityChanged);
    AHost.Armed := True;
    LRefused := False;
    try
      PublishNyxViewSections([LChange]);
    except
      on LException: ENyxModel do
      begin

        if LException.Message <> 'Intentional physical section activation refusal' then
        begin
          raise;
        end;
        LRefused := True;
      end;
    end;
    Check(LRefused and AHost.Entered and AHost.OldHidden and AHost.Activated and
      (AHost.Attempts = 1) and not AHost.Armed and
      (LChange.State = vcsCanceled),
      'public native publication actually activates its candidate before one target refusal');
    LChange := nil;
    Check((ASection.Renderer = LRenderer) and (ASection.Root = LRoot) and
      (LRenderer.Root = LRoot) and (ASection.Revision = LRevision) and
      (LScrollHost.Parent = LPlane) and (LPlane.Parent = AHost) and
      LPlane.Visible and (AHost.ControlCount = 1) and
      (LParent.ControlCount = LParentChildren) and
      (Input(ASection, 'header-input') = LInput) and
      (Face(ASection, 'header-action') = LAction) and
      (TextOf(LInput) = LText) and LMount.Connected and
      (LRenderer.CollectionMount('header-table') = LMount),
      'public rollback restores exact accepted renderer, roots, input, draft, collection and hosts');
    LInputHandleSame := TWinControl(LInput).HandleAllocated and
      (TWinControl(LInput).Handle = LInputHandle);
    LScrollHandleSame := LScrollHost.HandleAllocated and (LScrollHost.Handle = LScrollHandle);
    LPlaneHandleSame := LPlane.HandleAllocated and (LPlane.Handle = LPlaneHandle);
    LFocusSame := TWinControl(LInput).Focused;
    LActualSelection := CaptureNyxLCLSelection(TWinControl(LInput));
    Check(LInputHandleSame and LScrollHandleSame and LPlaneHandleSame and
      LFocusSame and LActualSelection.SameRange(LSelection),
      'failed native activation restores exact HWNDs, Unicode draft focus and scalar caret' +
      ' / input-hwnd=' + BoolToStr(LInputHandleSame, True) +
      ' / scroll-hwnd=' + BoolToStr(LScrollHandleSame, True) +
      ' / plane-hwnd=' + BoolToStr(LPlaneHandleSame, True) +
      ' / focused=' + BoolToStr(LFocusSame, True) +
      ' / actual-range=' + BoolToStr(LActualSelection.Defined, True) + ':' +
      IntToStr(LActualSelection.Start) + ':' + IntToStr(LActualSelection.Finish) + ':' +
      IntToStr(Ord(LActualSelection.Direction)) + ' / expected-range=' +
      IntToStr(LSelection.Start) + ':' + IntToStr(LSelection.Finish) + ':' +
      IntToStr(Ord(LSelection.Direction)));
    Check(LProbe.FocusEvents = 0,
      'native visibility rollback and focused draft recovery emit no extra focus or change event');
    { Resize the accepted host through ordinary native allocation. This proves
      accepted layout/rollback recovery, NOT reuse of the rejected candidate:
      cancellation retires that candidate, whose private finally is reviewed in
      source. Borrowed Renderer access below is lookup only, never lifetime work. }
    AHost.Width := LHostWidth - 100;
    Check((LScrollHost.Parent = LPlane) and (LPlane.Parent = AHost) and (AHost.ControlCount = 1) and
      (LRootFace.Width > 0) and (LRootFace.Width <= AHost.ClientWidth) and
      (LRootFace.Width < LOriginalWidth),
      'accepted native renderer responds to a narrower host after public rollback');
    AHost.Width := LHostWidth;
    Check((LScrollHost.Parent = LPlane) and (LPlane.Parent = AHost) and (AHost.ControlCount = 1) and
      (LRootFace.Width = LOriginalWidth) and (ASection.Root = LRoot) and
      (Input(ASection, 'header-input') = LInput) and
      (TextOf(LInput) = LText) and LMount.Connected,
      'accepted native layout returns exact input, draft and geometry at its original host width');
    Click(LAction);
    Check((AObserver.Clicks = LClicks + 1) and
      (Face(ASection, 'header-action') = LAction) and
      (ASection.Revision = LRevision),
      'accepted native event route executes once after public publication rollback');
    LChange := ASection.Prepare(ADocument, ADocument.Pages[0]);
    Check(PublishNyxViewSections([LChange]) and (LChange.State = vcsPublished) and
      (ASection.Revision = LRevision + 1) and (ASection.Root <> LRoot) and
      (ASection.Renderer <> LRenderer) and
      (Input(ASection, 'header-input') <> LInput) and not LMount.Connected and
      (AHost.Attempts = 1) and not AHost.Armed and (AHost.ControlCount = 1),
      'fresh public native publication succeeds and retires the former accepted scope');
    LChange := nil;
    Check((LParent.ControlCount = LParentChildren) and
      (TNyxCodec.Encode(ADocument) = LBefore),
      'native section recovery releases both proposal parking hosts without editing its source');
  finally
    AHost.Armed := False;
    AHost.Width := LHostWidth;
    LChange := nil;

    if LEnterToken <> nil then
    begin
      LEnterToken.Cancel;
    end;

    if LExitToken <> nil then
    begin
      LExitToken.Cancel;
    end;

    if LChangeToken <> nil then
    begin
      LChangeToken.Cancel;
    end;
    LEnterToken := nil;
    LExitToken := nil;
    LChangeToken := nil;
    LProbeLease := nil;
  end;
end;

procedure NativeGridPlaneRecovery(ASection: ISection;
  AHost: TPhysicalMoveHost; ADocument: TNyxDocument);
var
  LDocument: TNyxDocument;
  LRenderer: TRenderer;
  LGrid: TNyxCollectionStringGrid;
  LEditor: TCustomEdit;
  LPlane: TWinControl;
  LCandidate: TWinControl;
  LCandidatePanel: TWinControl;
  LMount: INyxCollectionMount;
  LStore: INyxCollection;
  LSnapshot: INyxCollectionSnapshot;
  LStoreToken: INyxCollectionSubscription;
  LFirst: INyxViewSectionChange;
  LSecond: INyxViewSectionChange;
  LProbe: TTransitionProbe;
  LProbeLease: INyxEventCallback;
  LEnterToken: INyxEventSubscription;
  LExitToken: INyxEventSubscription;
  LChangeToken: INyxEventSubscription;
  LForeign: TLabel;
  LSelection: TNyxTextSelection;
  LDraft: TNyxText;
  LSource: TNyxText;
  LGridHandle: HWND;
  LEditorHandle: HWND;
  LPlaneHandle: HWND;
  LCandidateHandle: HWND;
  LCandidatePanelHandle: HWND;
  LMode: Integer;
  LRevision: Integer;
  LAttempts: Integer;
  LRefused: Boolean;

  function HiddenCandidate: TWinControl;
  var
    LChild: Integer;
  begin
    Result := nil;
    for LChild := 0 to AHost.ControlCount - 1 do
    begin

      if (AHost.Controls[LChild] is TWinControl) and
        not AHost.Controls[LChild].Visible then
      begin
        Check(Result = nil, 'one remaining prepared candidate owns one hidden plane');
        Result := TWinControl(AHost.Controls[LChild]);
      end;
    end;
  end;
begin
  { Both existing table protocols use the same real section/host and authored
    rows. F2 enters the actual LCL producer; visibility must neither accept the
    unfinished scalar nor reset its native editor while focus leaves the plane. }
  LSource := TNyxCodec.Encode(ADocument);
  for LMode := 0 to 1 do
  begin
    LDocument := ADocument.Clone;
    try

      if LMode = 1 then
      begin
        LDocument.Find('header-table').Binds.Collection(
          LDocument.Find('header-table').CollectionView.TableLayout(NyxTableLayout)).Done;
      end;
      LFirst := ASection.Prepare(LDocument, LDocument.Pages[0]);
      Check(PublishNyxViewSections([LFirst]),
        'same native owner admits the legacy or arranged grid for draft recovery');
      LFirst := nil;
      LRenderer := ASection.Renderer;
      LGrid := TNyxCollectionStringGrid(LRenderer.ControlFor('header-table'));
      LMount := LRenderer.CollectionMount('header-table');
      LStore := LMount.View.Store;
      LSnapshot := LStore.Snapshot;
      LGrid.Col := 0;
      LGrid.Row := 1;
      LGrid.SetFocus;
      SendMessage(LGrid.Handle, LM_KEYDOWN, VK_F2, 0);
      Application.ProcessMessages;
      Check(LGrid.EditorMode and (LGrid.Editor is TCustomEdit),
        'real native F2 enters the legacy or arranged grid editor');
      LEditor := TCustomEdit(LGrid.Editor);
      LDraft := TNyxText('Unaccepted section row / 🌙');
      RestoreNyxLCLInputText(LEditor, LDraft);
      LSelection := NyxTextSelection(LDraft, NyxTextScalarCount(LDraft) - 1,
        NyxTextScalarCount(LDraft), ntdUnknown);
      SelectNyxLCLText(LEditor, LSelection);
      Check(LEditor.Focused and CaptureNyxLCLSelection(LEditor).SameRange(LSelection),
        'native draft selects the supplementary scalar through the exact Unicode bridge');
      LPlane := LRenderer.ControlFor(ASection.Root.ID).Parent.Parent;
      LGridHandle := LGrid.Handle;
      LEditorHandle := LEditor.Handle;
      LPlaneHandle := LPlane.Handle;
      LRevision := ASection.Revision;
      LAttempts := AHost.Attempts;
      LProbe := TTransitionProbe.Create;
      LProbeLease := LProbe;
      LEnterToken := LRenderer.Events.OnAfterEnter(
        NyxControlEvents('header-table', niRuntime)).Subscribe(LProbeLease);
      LExitToken := LRenderer.Events.OnAfterExit(
        NyxControlEvents('header-table', niRuntime)).Subscribe(LProbeLease);
      LChangeToken := LRenderer.Events.On(
        NyxControlEvents('header-table', niRuntime), ntChange).Subscribe(LProbeLease);
      LStoreToken := LStore.Subscribe(LProbe.StoreChanged);
      try
        LFirst := ASection.Prepare(LDocument, LDocument.Pages[0]);
        Check((LGrid.Handle = LGridHandle) and (LEditor.Handle = LEditorHandle) and
          (LPlane.Handle = LPlaneHandle) and LGrid.EditorMode and
          (NyxLCLInputText(LEditor) = LDraft),
          'hidden native preparation preserves the accepted grid, editor and draft HWNDs');
        LCandidate := HiddenCandidate;
        Check(LCandidate <> nil, 'native grid recovery has a real hidden candidate');
        LCandidate.AddHandlerOnVisibleChanged(AHost.PlaneVisibilityChanged);
        AHost.Armed := True;
        LRefused := False;
        try
          PublishNyxViewSections([LFirst]);
        except
          on LException: ENyxModel do
          begin

            if LException.Message <> 'Intentional physical section activation refusal' then
            begin
              raise;
            end;
            LRefused := True;
          end;
        end;
        Check(LRefused and (LFirst.State = vcsCanceled) and
          (AHost.Attempts = LAttempts + 1) and AHost.OldHidden and AHost.Activated,
          'legacy and arranged rollback follows an actual failed target activation');
        LFirst := nil;
        Check((ASection.Revision = LRevision) and (ASection.Renderer = LRenderer) and
          (LGrid.Handle = LGridHandle) and (LEditor.Handle = LEditorHandle) and
          (LPlane.Handle = LPlaneHandle) and (LPlane.Parent = AHost) and LPlane.Visible and
          LGrid.EditorMode and (LGrid.Editor = LEditor) and LEditor.Focused and
          (NyxLCLInputText(LEditor) = LDraft) and
          CaptureNyxLCLSelection(LEditor).SameRange(LSelection),
          'native rollback retains exact editor mode, HWNDs, Unicode text, focus and scalar range');
        Check((LProbe.FocusEvents = 0) and (LProbe.Changes = 0) and
          (LStore.Snapshot.Revision = LSnapshot.Revision) and
          LStore.Snapshot.ItemAt(0).SameItem(LSnapshot.ItemAt(0)),
          'native hide, rollback and focus restoration emit no focus/change or accepted data edit');
        LFirst := ASection.Prepare(LDocument, LDocument.Pages[0]);
        LSecond := ASection.Prepare(LDocument, LDocument.Pages[0]);
        Check((AHost.ControlCount = 3) and LPlane.Visible and
          (LGrid.Handle = LGridHandle) and (LEditor.Handle = LEditorHandle),
          'concurrent prepared candidates coexist hidden beside the unchanged accepted plane');
        LFirst.Cancel;
        LFirst := nil;
        LCandidate := HiddenCandidate;
        Check((AHost.ControlCount = 2) and (LCandidate <> nil) and
          (LCandidate.ControlCount = 1),
          'canceling one prepared candidate leaves the other candidate and accepted plane alive');
        LCandidatePanel := TWinControl(LCandidate.Controls[0]);
        LCandidateHandle := LCandidate.Handle;
        LCandidatePanelHandle := LCandidatePanel.Handle;
        LForeign := TLabel.Create(nil);
        try
          LForeign.Parent := AHost;
          Check(not PublishNyxViewSections([LSecond]) and
            (LSecond.State = vcsPrepared) and (ASection.Revision = LRevision) and
            (LGrid.Handle = LGridHandle) and (LEditor.Handle = LEditorHandle) and
            (LForeign.Parent = AHost),
            'an arbitrary caller child refuses admission without being ignored or removed');
        finally
          LForeign.Free;
        end;
        Check(PublishNyxViewSections([LSecond]) and (LSecond.State = vcsPublished) and
          (ASection.Revision = LRevision + 1) and (AHost.ControlCount = 1) and
          (LCandidate.Parent = AHost) and LCandidate.Visible and
          (LCandidate.Handle = LCandidateHandle) and
          (LCandidatePanel.Parent = LCandidate) and
          (LCandidatePanel.Handle = LCandidatePanelHandle) and not LMount.Connected,
          'successful native activation retains prepared handles and retires only the former view');
        Check((LProbe.Changes = 0) and
          (LStore.Snapshot.Revision = LSnapshot.Revision) and
          LStore.Snapshot.ItemAt(0).SameItem(LSnapshot.ItemAt(0)),
          'retiring the unaccepted legacy or arranged editor never commits its draft');
        LSecond := nil;
      finally
        AHost.Armed := False;
        LFirst := nil;
        LSecond := nil;

        if LStoreToken <> nil then
        begin
          LStoreToken.Disconnect;
        end;
        LStoreToken := nil;

        if LEnterToken <> nil then
        begin
          LEnterToken.Cancel;
        end;

        if LExitToken <> nil then
        begin
          LExitToken.Cancel;
        end;

        if LChangeToken <> nil then
        begin
          LChangeToken.Cancel;
        end;
        LEnterToken := nil;
        LExitToken := nil;
        LChangeToken := nil;
        LProbeLease := nil;
      end;
    finally
      LDocument.Free;
    end;
  end;
  Check(TNyxCodec.Encode(ADocument) = LSource,
    'legacy and arranged native plane recovery never changes the authored source');
end;

procedure NativeCleanupRecovery(ASection: ISection; AHost: TWinControl;
  ADocument: TNyxDocument; AScenario: TScenario);
var
  LProbe: TNativeCleanupProbe;
  LFinal: ISection;
  LChange: INyxViewSectionChange;
  LMount: INyxCollectionMount;
  LRoot: TNyxNode;
  LButton: TCleanupButton;
  LObserver: TCleanupObserver;
  LObserverLease: INyxViewSectionObserver;
  LCleanupDocument: TNyxDocument;
  LRevision: Integer;
  LRefused: Boolean;
  LSource: TNyxText;

  function CandidateButton(AParent: TWinControl): TCleanupButton;
  var
    LIndex: Integer;
    LFound: TCleanupButton;
  begin
    Result := nil;
    for LIndex := 0 to AParent.ControlCount - 1 do
    begin
      LFound := nil;

      if AParent.Controls[LIndex] is TCleanupButton then
      begin
        LFound := TCleanupButton(AParent.Controls[LIndex]);
      end
      else if AParent.Controls[LIndex] is TWinControl then
      begin
        LFound := CandidateButton(TWinControl(AParent.Controls[LIndex]));
      end;

      if LFound <> nil then
      begin
        Check(Result = nil, 'cleanup candidate has one exact ordinary factory button');
        Result := LFound;
      end;
    end;
  end;
begin
  LCleanupDocument := nil;
  LSource := TNyxCodec.Encode(ADocument);
  LProbe := TNativeCleanupProbe.Create;
  LProbe.Document := ADocument;
  LProbe.Host := AHost;
  try
    LMount := ASection.Renderer.CollectionMount('details-table');
    LRevision := ASection.Revision;
    LProbe.BorrowedSection := Pointer(ASection);
    LButton := TCleanupButton(Face(ASection, 'details-action'));
    LButton.OnRetire := LProbe.Retiring;
    ASection.Close;
    Check((LProbe.Calls = 1) and LProbe.CloseRefused and LProbe.PrepareRefused and
      (ASection.Revision = LRevision + 1) and (ASection.Root = nil) and
      (AHost.ControlCount = 0) and not LMount.Connected,
      'explicit native Close rejects destructor reentry and retires exact renderer and plane once');

    { Reuse the SAME empty physical slot for a genuinely final owner release.
      The probe has only a borrowed interface pointer, not a lease preventing
      the native section destructor from entering its real control cleanup. }
    LFinal := NewNyxLCLViewSection(TNyxViewSectionRef.Named('cleanup-final'),
      AHost, AScenario.Configure);
    LChange := LFinal.Prepare(ADocument, ADocument.Pages[0]);
    Check(PublishNyxViewSections([LChange]),
      'native final-release regression admits its actual ordinary factory tree');
    LChange := nil;
    LProbe.BorrowedSection := Pointer(LFinal);
    LButton := TCleanupButton(Face(LFinal, 'details-action'));
    LButton.OnRetire := LProbe.Retiring;
    LFinal := nil;
    Check((LProbe.Calls = 2) and LProbe.CloseRefused and LProbe.PrepareRefused and
      (AHost.ControlCount = 0),
      'final native owner release rejects Close and Prepare without reviving a zero-reference section');

    { The source document is unchanged. Only this copied candidate declares two
      actual creator participants whose explicit Disconnect methods may fail. }
    LCleanupDocument := ADocument.Clone;
    LCleanupDocument.Find('details-action').Extensions.SetValue(
      NyxExtension(CNativeCleanupIntent), NyxData(2));
    LCleanupDocument.Find('details-input').Extensions.SetValue(
      NyxExtension(CNativeCleanupIntent), NyxData(1));
    LObserver := TCleanupObserver.Create;
    LObserver.Probe := LProbe;
    LObserverLease := LObserver;
    LFinal := NewNyxLCLViewSection(TNyxViewSectionRef.Named('cleanup-stage'),
      AHost, LProbe.Configure, LObserverLease);
    LObserverLease := nil;
    LChange := LFinal.Prepare(LCleanupDocument, LCleanupDocument.Pages[0]);
    Check((LFinal.Root = nil) and (AHost.ControlCount = 1),
      'cleanup refusal starts from a real hidden unpublished native plane');
    LButton := CandidateButton(AHost);
    Check((LButton <> nil) and (LButton.ControlID = 'details-action'),
      'cleanup callback attaches to the exact real candidate factory button');
    LProbe.Mode := ncmObserve;
    LProbe.FailCleanup := True;
    LButton.OnRetire := LProbe.Retiring;
    LFinal := nil; { Publication is now the section's sole owner lease. }
    LRefused := False;
    try
      LChange.Cancel;
    except
      on LException: ENyxCoordinator do
      begin

        if Pos('Intentional first section coordinator cleanup refusal',
          LException.Message) = 0 then
        begin
          raise;
        end;
        LRefused := True;
      end;
    end;
    Check(LRefused and (LChange.State = vcsCanceled) and (LProbe.Calls = 3) and
      (LProbe.ObserverReleases = 1) and LProbe.OwnerReleasedAfterPlanes and
      (LProbe.CleanupAttempts = 2) and (LProbe.CoordinatorsCreated = 2) and
      (LProbe.CoordinatorsDestroyed = 2) and (AHost.ControlCount = 0),
      'native cancellation frees the stage before its owner and retains the FIRST of two cleanup failures');
    LChange := nil;
    { Implicit abandonment has the SAME fallible participants, but destructors
      cannot report an error. Every coordinator, renderer, plane and owner must
      still be retired, with no raised destructor and no retained token cycle. }
    LObserver := TCleanupObserver.Create;
    LObserver.Probe := LProbe;
    LObserverLease := LObserver;
    LFinal := NewNyxLCLViewSection(TNyxViewSectionRef.Named('cleanup-abandoned'),
      AHost, LProbe.Configure, LObserverLease);
    LObserverLease := nil;
    LChange := LFinal.Prepare(LCleanupDocument, LCleanupDocument.Pages[0]);
    LFinal := nil;
    LChange := nil;
    Check((LProbe.CleanupAttempts = 4) and (LProbe.CoordinatorsCreated = 4) and
      (LProbe.CoordinatorsDestroyed = 4) and (LProbe.ObserverReleases = 2) and
      LProbe.OwnerReleasedAfterPlanes and (AHost.ControlCount = 0),
      'implicit native abandonment completes all fallible cleanup without a throwing destructor');
    LRoot := ASection.Root;
    Check(LRoot = nil, 'candidate cleanup failure cannot acquire the independent original section');
    LChange := ASection.Prepare(ADocument, ADocument.Pages[0]);
    Check(PublishNyxViewSections([LChange]) and (ASection.Root <> nil) and
      (AHost.ControlCount = 1) and (TNyxCodec.Encode(ADocument) = LSource),
      'fresh ordinary publication succeeds after both cleanup refusals without changing source');
    LChange := nil;
  finally
    LProbe.BorrowedSection := nil;
    LFinal := nil;
    LChange := nil;
    LObserverLease := nil;
    LCleanupDocument.Free;
    LProbe.Free;
  end;
end;

{ The existing pair of physical slots hosts two independent ordinary views.
  A real second-plane activation failure exercises reverse rollback plus BOTH
  fallible cancellations. A later committed pair exercises BOTH former-view
  retirements even when the first disconnect raises. No destructor throws. }
procedure NativeCleanupGroupRecovery(AHeaderHost: TPhysicalMoveHost;
  ADetailsHost: TWinControl; AHeaderDocument, ADetailsDocument: TNyxDocument);
var
  LSections: array[0..1] of ISection;
  LChanges: array[0..1] of INyxViewSectionChange;
  LHosts: array[0..1] of TWinControl;
  LProbes: array[0..1] of TNativeCleanupProbe;
  LDocuments: array[0..1] of TNyxDocument;
  LPrefixes: array[0..1] of TNyxText;
  LBeforeSource: array[0..1] of TNyxText;
  LRenderers: array[0..1] of TRenderer;
  LRoots: array[0..1] of TNyxNode;
  LMounts: array[0..1] of INyxCollectionMount;
  LInputs: array[0..1] of TWinControl;
  LInputHandles: array[0..1] of HWND;
  LPlanes: array[0..1] of TWinControl;
  LPlaneHandles: array[0..1] of HWND;
  LStages: array[0..1] of TWinControl;
  LStageHandles: array[0..1] of HWND;
  LTexts: array[0..1] of TNyxText;
  LRevisions: array[0..1] of Integer;
  LIndex: Integer;
  LAttempts: Integer;
  LRefused: Boolean;
  LMessage: TNyxText;

  function CandidatePlane(AHost, ACurrent: TWinControl): TWinControl;
  var
    LChild: Integer;
  begin
    Result := nil;
    for LChild := 0 to AHost.ControlCount - 1 do
    begin

      if (AHost.Controls[LChild] <> ACurrent) and
        (AHost.Controls[LChild] is TWinControl) then
      begin

        if Result <> nil then
        begin
          raise ENyxModel.Create('Grouped cleanup requires one exact hidden candidate plane');
        end;
        Result := TWinControl(AHost.Controls[LChild]);
      end;
    end;
    Check((Result <> nil) and (Result.Parent = AHost) and not Result.Visible,
      'grouped cleanup prepares its actual hidden plane beside the accepted plane');
  end;

  procedure PrepareBoth;
  var
    LMember: Integer;
  begin
    for LMember := 0 to 1 do
    begin
      LChanges[LMember] := LSections[LMember].Prepare(
        LDocuments[LMember], LDocuments[LMember].Pages[0]);
      LStages[LMember] := CandidatePlane(LHosts[LMember], LPlanes[LMember]);
      LStageHandles[LMember] := LStages[LMember].Handle;
    end;
  end;

begin
  { Details is deliberately first; the header's existing physical fault seam
    then fails the SECOND preview after the independent details view was shown. }
  LHosts[0] := ADetailsHost;
  LHosts[1] := AHeaderHost;
  LPrefixes[0] := 'details';
  LPrefixes[1] := 'header';
  LBeforeSource[0] := TNyxCodec.Encode(ADetailsDocument);
  LBeforeSource[1] := TNyxCodec.Encode(AHeaderDocument);
  for LIndex := 0 to 1 do
  begin
    LDocuments[LIndex] := nil;
    LProbes[LIndex] := nil;
  end;
  try
    LDocuments[0] := ADetailsDocument.Clone;
    LDocuments[1] := AHeaderDocument.Clone;
    for LIndex := 0 to 1 do
    begin
      LDocuments[LIndex].Find(LPrefixes[LIndex] + '-action').Extensions.SetValue(
        NyxExtension(CNativeCleanupIntent), NyxData(LIndex + 1));
      LProbes[LIndex] := TNativeCleanupProbe.Create;
      LProbes[LIndex].Host := LHosts[LIndex];
      LSections[LIndex] := NewNyxLCLViewSection(TNyxViewSectionRef.Named(
        LPrefixes[LIndex] + '-cleanup-group'), LHosts[LIndex], LProbes[LIndex].Configure);
      LChanges[LIndex] := LSections[LIndex].Prepare(
        LDocuments[LIndex], LDocuments[LIndex].Pages[0]);
    end;
    Check(PublishNyxViewSections([LChanges[0], LChanges[1]]),
      'grouped cleanup starts from two real independently accepted coordinator views');
    LChanges[0] := nil;
    LChanges[1] := nil;
    for LIndex := 0 to 1 do
    begin
      LRenderers[LIndex] := LSections[LIndex].Renderer;
      LRoots[LIndex] := LSections[LIndex].Root;
      LRevisions[LIndex] := LSections[LIndex].Revision;
      LMounts[LIndex] := LRenderers[LIndex].CollectionMount(LPrefixes[LIndex] + '-table');
      LInputs[LIndex] := TWinControl(Input(LSections[LIndex], LPrefixes[LIndex] + '-input'));
      Draft(LInputs[LIndex]);
      LTexts[LIndex] := TextOf(LInputs[LIndex]);
      LInputHandles[LIndex] := LInputs[LIndex].Handle;
      LPlanes[LIndex] := TWinControl(LHosts[LIndex].Controls[0]);
      LPlaneHandles[LIndex] := LPlanes[LIndex].Handle;
      LProbes[LIndex].FailCleanup := True;
    end;
    PrepareBoth;
    LStages[0].AddHandlerOnVisibleChanged(LProbes[0].Previewed);
    LStages[1].AddHandlerOnVisibleChanged(AHeaderHost.PlaneVisibilityChanged);
    LAttempts := AHeaderHost.Attempts;
    AHeaderHost.Armed := True;
    LRefused := False;
    LMessage := '';
    try
      PublishNyxViewSections([LChanges[0], LChanges[1]]);
    except
      on E: ENyxModel do
      begin
        LMessage := E.Message;
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProbes[0].PreviewShows = 1) and
      (AHeaderHost.Attempts = LAttempts + 1) and AHeaderHost.OldHidden and
      AHeaderHost.Activated and not AHeaderHost.Armed and
      (Pos('Intentional physical section activation refusal', LMessage) > 0) and
      (Pos('Intentional first section coordinator cleanup refusal', LMessage) > 0) and
      (Pos('Intentional later section coordinator cleanup refusal', LMessage) = 0),
      'second real preview failure preserves its primary error and first cancellation failure');
    for LIndex := 0 to 1 do
    begin
      Check((LChanges[LIndex].State = vcsCanceled) and
        (LSections[LIndex].Renderer = LRenderers[LIndex]) and
        (LSections[LIndex].Root = LRoots[LIndex]) and
        (LSections[LIndex].Revision = LRevisions[LIndex]) and
        (LHosts[LIndex].ControlCount = 1) and LPlanes[LIndex].Visible and
        (LPlanes[LIndex].Parent = LHosts[LIndex]) and
        (LPlanes[LIndex].Handle = LPlaneHandles[LIndex]) and
        (Input(LSections[LIndex], LPrefixes[LIndex] + '-input') = LInputs[LIndex]) and
        LInputs[LIndex].HandleAllocated and
        (LInputs[LIndex].Handle = LInputHandles[LIndex]) and
        (TextOf(LInputs[LIndex]) = LTexts[LIndex]) and LMounts[LIndex].Connected and
        (LProbes[LIndex].CleanupAttempts = 1) and
        (LProbes[LIndex].CoordinatorsCreated = 2) and
        (LProbes[LIndex].CoordinatorsDestroyed = 1),
        'group rollback restores exact accepted input HWND draft and independent mount while BOTH candidates retire');
      LChanges[LIndex] := nil;
    end;

    PrepareBoth;
    LRefused := False;
    LMessage := '';
    try
      PublishNyxViewSections([LChanges[0], LChanges[1]]);
    except
      on E: ENyxCoordinator do
      begin
        LMessage := E.Message;
        LRefused := True;
      end;
    end;
    Check(LRefused and
      (Pos('Intentional first section coordinator cleanup refusal', LMessage) > 0) and
      (Pos('Intentional later section coordinator cleanup refusal', LMessage) = 0),
      'postcommit group retirement reports the exact FIRST supported disconnect failure');
    for LIndex := 0 to 1 do
    begin
      Check((LChanges[LIndex].State = vcsPublished) and
        (LSections[LIndex].Revision = LRevisions[LIndex] + 1) and
        (LSections[LIndex].Renderer <> LRenderers[LIndex]) and
        (LSections[LIndex].Root <> LRoots[LIndex]) and
        (LHosts[LIndex].ControlCount = 1) and LStages[LIndex].Visible and
        (LStages[LIndex].Parent = LHosts[LIndex]) and
        (LStages[LIndex].Handle = LStageHandles[LIndex]) and
        not LMounts[LIndex].Connected and
        LSections[LIndex].Renderer.CollectionMount(LPrefixes[LIndex] + '-table').Connected and
        (LProbes[LIndex].CleanupAttempts = 2) and
        (LProbes[LIndex].CoordinatorsCreated = 3) and
        (LProbes[LIndex].CoordinatorsDestroyed = 2),
        'both replacements stay committed with exact prepared HWNDs and BOTH former owners retire after the first failure');
      LProbes[LIndex].FailCleanup := False;
      LChanges[LIndex] := nil;
    end;
    for LIndex := 0 to 1 do
    begin
      LSections[LIndex].Close;
      Check((LHosts[LIndex].ControlCount = 0) and
        (LProbes[LIndex].CleanupAttempts = 3) and
        (LProbes[LIndex].CoordinatorsCreated = LProbes[LIndex].CoordinatorsDestroyed),
        'explicit final group Close releases each remaining real coordinator exactly once');
    end;
    Check((TNyxCodec.Encode(ADetailsDocument) = LBeforeSource[0]) and
      (TNyxCodec.Encode(AHeaderDocument) = LBeforeSource[1]),
      'multi-section cleanup failure never changes either original authored document');
  finally
    AHeaderHost.Armed := False;
    for LIndex := 0 to 1 do
    begin

      if LProbes[LIndex] <> nil then
      begin
        LProbes[LIndex].FailCleanup := False;
      end;
      LChanges[LIndex] := nil;
      LSections[LIndex] := nil;
      LDocuments[LIndex].Free;
      LProbes[LIndex].Free;
    end;
  end;
end;
{$endif}

{$ifdef PAS2JS}
{ The browser exercises the SAME real creator cleanup boundary and existing
  two slots. Parking sibling counts qualify their ownership; exact accepted
  elements/drafts qualify rollback, independently of native HWND evidence. }
procedure BrowserCleanupGroupRecovery(AHeaderHost, ADetailsHost: TJSHTMLElement;
  AHeaderDocument, ADetailsDocument: TNyxDocument);
var
  LSections: array of ISection;
  LChanges: array of INyxViewSectionChange;
  LHosts: array[0..1] of TJSHTMLElement;
  LProbes: array[0..1] of TSectionCleanupProbe;
  LDocuments: array[0..1] of TNyxDocument;
  LPrefixes: array[0..1] of TNyxText;
  LBeforeSource: array[0..1] of TNyxText;
  LRenderers: array[0..1] of TRenderer;
  LRoots: array[0..1] of TNyxNode;
  LMounts: array of INyxCollectionMount;
  LInputs: array[0..1] of TJSHTMLElement;
  LElements: array[0..1] of TJSHTMLElement;
  LTexts: array[0..1] of TNyxText;
  LRevisions: array[0..1] of Integer;
  LParent: TJSNode;
  LParentChildren: Integer;
  LIndex: Integer;
  LRefused: Boolean;
  LMessage: TNyxText;
  LRawDocument: TNyxDocument;
  LRawSource: TNyxText;
  LRawRevision: Integer;
  LRawAttempts: Integer;
  LRawCreated: Integer;
  LRawDestroyed: Integer;
  LRawCaught: Boolean;
  LScopeRefused: Boolean;
  LScopeIndex: Integer;
  LObserved: JSValue;

  procedure PrepareBoth;
  var
    LMember: Integer;
  begin
    for LMember := 0 to 1 do
    begin
      LChanges[LMember] := LSections[LMember].Prepare(
        LDocuments[LMember], LDocuments[LMember].Pages[0]);
    end;
    Check(LParent.childNodes.length = LParentChildren + 4,
      'both real browser proposals own distinct staging and rollback parking siblings');
  end;

  procedure AcceptedViewsStay(AExpected: Integer; const AReason: TNyxText);
  var
    LMember: Integer;
  begin
    for LMember := 0 to 1 do
    begin
      Check((LSections[LMember].Renderer = LRenderers[LMember]) and
        (LSections[LMember].Root = LRoots[LMember]) and
        (LSections[LMember].Revision = LRevisions[LMember]) and
        (Input(LSections[LMember], LPrefixes[LMember] + '-input') = LInputs[LMember]) and
        (TextOf(LInputs[LMember]) = LTexts[LMember]) and
        (LRenderers[LMember].ElementFor(LRoots[LMember].ID) = LElements[LMember]) and
        (LElements[LMember].parentNode = LHosts[LMember]) and
        (LHosts[LMember].childNodes.length = 2) and LMounts[LMember].Connected and
        (LProbes[LMember].CleanupAttempts = AExpected) and
        (LProbes[LMember].CoordinatorsCreated = AExpected + 1) and
        (LProbes[LMember].CoordinatorsDestroyed = AExpected), AReason);
    end;
    Check(LParent.childNodes.length = LParentChildren,
      'browser candidate cleanup frees ALL owned parking without removing the accepted hosts');
  end;

begin
  { pas2js supports managed interface vectors through dynamic arrays. Their
    explicit two-slot allocation preserves the native fixture's same owners. }
  SetLength(LSections, 2);
  SetLength(LChanges, 2);
  SetLength(LMounts, 2);
  LHosts[0] := ADetailsHost;
  LHosts[1] := AHeaderHost;
  LPrefixes[0] := 'details';
  LPrefixes[1] := 'header';
  LBeforeSource[0] := TNyxCodec.Encode(ADetailsDocument);
  LBeforeSource[1] := TNyxCodec.Encode(AHeaderDocument);
  LParent := AHeaderHost.parentNode;
  Check((LParent <> nil) and (ADetailsHost.parentNode = LParent),
    'browser cleanup group reuses the actual original pair of attached physical slots');
  LParentChildren := LParent.childNodes.length;
  LRawDocument := nil;
  for LIndex := 0 to 1 do
  begin
    LDocuments[LIndex] := nil;
    LProbes[LIndex] := nil;
  end;
  try
    LDocuments[0] := ADetailsDocument.Clone;
    LDocuments[1] := AHeaderDocument.Clone;
    for LIndex := 0 to 1 do
    begin
      LDocuments[LIndex].Find(LPrefixes[LIndex] + '-action').Extensions.SetValue(
        NyxExtension(CNativeCleanupIntent), NyxData(LIndex + 1));
      LProbes[LIndex] := TSectionCleanupProbe.Create;
      LSections[LIndex] := NewNyxBrowserViewSection(TNyxViewSectionRef.Named(
        LPrefixes[LIndex] + '-cleanup-group'), LHosts[LIndex], @LProbes[LIndex].Configure);
      LChanges[LIndex] := LSections[LIndex].Prepare(
        LDocuments[LIndex], LDocuments[LIndex].Pages[0]);
    end;
    Check(PublishNyxViewSections([LChanges[0], LChanges[1]]) and
      (LParent.childNodes.length = LParentChildren),
      'browser cleanup starts with independently mounted real coordinator views and no leaked parking');
    LChanges[0] := nil;
    LChanges[1] := nil;
    for LIndex := 0 to 1 do
    begin
      LRenderers[LIndex] := LSections[LIndex].Renderer;
      LRoots[LIndex] := LSections[LIndex].Root;
      LElements[LIndex] := LRenderers[LIndex].ElementFor(LRoots[LIndex].ID);
      LRevisions[LIndex] := LSections[LIndex].Revision;
      LMounts[LIndex] := LRenderers[LIndex].CollectionMount(LPrefixes[LIndex] + '-table');
      LInputs[LIndex] := Input(LSections[LIndex], LPrefixes[LIndex] + '-input');
      Draft(LInputs[LIndex]);
      LTexts[LIndex] := TextOf(LInputs[LIndex]);
      LProbes[LIndex].FailCleanup := True;
    end;
    PrepareBoth;
    for LIndex := 0 to 1 do
    begin
      LRefused := False;
      try
        LChanges[LIndex].Cancel;
      except
        on E: ENyxCoordinator do LRefused := True;
      end;
      Check(LRefused and (LChanges[LIndex].State = vcsCanceled),
        'explicit browser Cancel reports supported cleanup failure after terminal revocation');
      LChanges[LIndex] := nil;
    end;
    AcceptedViewsStay(1,
      'explicit browser candidate failure preserves exact accepted elements drafts revisions and mounts');
    PrepareBoth;
    LChanges[0] := nil;
    LChanges[1] := nil;
    AcceptedViewsStay(2,
      'implicit browser abandonment retires fallible owners without throwing from destructors');

    PrepareBoth;
    GFailCleanupHeaderPreview := True;
    LRefused := False;
    LMessage := '';
    try
      PublishNyxViewSections([LChanges[0], LChanges[1]]);
    except
      on E: ENyxModel do
      begin
        LMessage := E.Message;
        LRefused := True;
      end;
    end;
    Check(LRefused and not GFailCleanupHeaderPreview and
      (Pos('Intentional second grouped section preview refusal', LMessage) > 0) and
      (Pos('Intentional first section coordinator cleanup refusal', LMessage) > 0) and
      (Pos('Intentional later section coordinator cleanup refusal', LMessage) = 0) and
      (LChanges[0].State = vcsCanceled) and (LChanges[1].State = vcsCanceled),
      'second real browser preview failure rolls back BOTH views and preserves first cancellation error');
    AcceptedViewsStay(3,
      'both browser rollback members recover exact original elements and Unicode drafts despite cleanup errors');
    LChanges[0] := nil;
    LChanges[1] := nil;

    PrepareBoth;
    LRefused := False;
    LMessage := '';
    try
      PublishNyxViewSections([LChanges[0], LChanges[1]]);
    except
      on E: ENyxCoordinator do
      begin
        LMessage := E.Message;
        LRefused := True;
      end;
    end;
    Check(LRefused and
      (Pos('Intentional first section coordinator cleanup refusal', LMessage) > 0) and
      (Pos('Intentional later section coordinator cleanup refusal', LMessage) = 0),
      'browser postcommit group cleanup reports the FIRST supported disconnect failure');
    for LIndex := 0 to 1 do
    begin
      Check((LChanges[LIndex].State = vcsPublished) and
        (LSections[LIndex].Revision = LRevisions[LIndex] + 1) and
        (LSections[LIndex].Renderer <> LRenderers[LIndex]) and
        (LSections[LIndex].Root <> LRoots[LIndex]) and not LMounts[LIndex].Connected and
        LSections[LIndex].Renderer.CollectionMount(LPrefixes[LIndex] + '-table').Connected and
        (LSections[LIndex].Renderer.ElementFor(LSections[LIndex].Root.ID).parentNode = LHosts[LIndex]) and
        (LHosts[LIndex].childNodes.length = 2) and
        (LProbes[LIndex].CleanupAttempts = 4) and
        (LProbes[LIndex].CoordinatorsCreated = 5) and
        (LProbes[LIndex].CoordinatorsDestroyed = 4),
        'BOTH browser replacements remain committed and BOTH original coordinator owners retire');
      LProbes[LIndex].FailCleanup := False;
      LChanges[LIndex] := nil;
      LSections[LIndex].Close;
      Check((LHosts[LIndex].childNodes.length = 0) and
        (LProbes[LIndex].CleanupAttempts = 5) and
        (LProbes[LIndex].CoordinatorsCreated = LProbes[LIndex].CoordinatorsDestroyed),
        'final browser Close frees every remaining real coordinator exactly once');
    end;
    Check((LParent.childNodes.length = LParentChildren) and
      (TNyxCodec.Encode(ADetailsDocument) = LBeforeSource[0]) and
      (TNyxCodec.Encode(AHeaderDocument) = LBeforeSource[1]),
      'browser group cleanup keeps both caller hosts and original authored documents exact');

    { Reuse the SAME closed section/slot for one direct-cancellation boundary.
      Its candidate contains two real independently scoped creator owners;
      unlike the pure vector matrix, this also traverses renderer and adapter
      cleanup before the original raw value can reach the caller. }
    LRawDocument := LDocuments[0].Clone;
    LRawDocument.Find('details-input').Extensions.SetValue(
      NyxExtension(CNativeCleanupIntent), NyxData(1));
    LRawDocument.Find('details-action').Extensions.SetValue(
      NyxExtension(CNativeCleanupIntent), NyxData(2));
    LRawSource := TNyxCodec.Encode(LRawDocument);
    LRawRevision := LSections[0].Revision;
    LRawAttempts := LProbes[0].CleanupAttempts;
    LRawCreated := LProbes[0].CoordinatorsCreated;
    LRawDestroyed := LProbes[0].CoordinatorsDestroyed;
    LProbes[0].FailCleanup := True;
    LProbes[0].ThrowFirstUndefined := True;
    LProbes[0].CaptureCleanupScopes := True;
    LChanges[0] := LSections[0].Prepare(LRawDocument, LRawDocument.Pages[0]);
    Check((Length(LProbes[0].CleanupScopes) = 2) and
      (LParent.childNodes.length = LParentChildren + 2),
      'raw browser cancellation owns two actual scopes and its distinct staging/rollback parking');
    LRawCaught := False;
    LObserved := Null;
    try
      LChanges[0].Cancel;
    except
      LRawCaught := True;
      LObserved := JSExceptValue;
    end;
    Check(LRawCaught and isUndefined(LObserved) and
      (LChanges[0].State = vcsCanceled) and
      (LSections[0].Revision = LRawRevision) and
      (LSections[0].Root = nil) and (LSections[0].Renderer = nil) and
      (LProbes[0].CleanupAttempts = LRawAttempts + 2) and
      (LProbes[0].CoordinatorsCreated = LRawCreated + 2) and
      (LProbes[0].CoordinatorsDestroyed = LRawDestroyed + 2) and
      (LHosts[0].childNodes.length = 0) and
      (LParent.childNodes.length = LParentChildren),
      'actual renderer and adapter rethrow undefined after BOTH owners retire and all candidate parking is removed');
    for LScopeIndex := 0 to High(LProbes[0].CleanupScopes) do
    begin
      LScopeRefused := False;
      try
        LProbes[0].CleanupScopes[LScopeIndex].Root;
      except
        on E: ENyxCoordinatorCancelled do
        begin
          LScopeRefused := True;
        end;
      end;
      Check(LScopeRefused,
        'each retained real projection remains revoked after raw candidate cleanup');
    end;
    Check((TNyxCodec.Encode(LRawDocument) = LRawSource) and
      (TNyxCodec.Encode(ADetailsDocument) = LBeforeSource[0]) and
      (TNyxCodec.Encode(AHeaderDocument) = LBeforeSource[1]),
      'raw platform cleanup never changes candidate defaults or either original authored document');
    LChanges[0] := nil;
    LProbes[0].CleanupScopes := nil;
    LProbes[0].CaptureCleanupScopes := False;
    LProbes[0].ThrowFirstUndefined := False;
    LProbes[0].FailCleanup := False;
  finally
    GFailCleanupHeaderPreview := False;
    for LIndex := 0 to 1 do
    begin

      if LProbes[LIndex] <> nil then
      begin
        LProbes[LIndex].FailCleanup := False;
      end;
      LChanges[LIndex] := nil;
      LSections[LIndex] := nil;

      if LProbes[LIndex] <> nil then
      begin
        LProbes[LIndex].CleanupScopes := nil;
      end;
      LDocuments[LIndex].Free;
      LProbes[LIndex].Free;
    end;
    LRawDocument.Free;
  end;
end;

procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  { Surface timer setup failure through the same awaited promise. }
  try
    window.setTimeout(procedure
      begin
        AResolve('');
      end, 15);
  except
    on LException: Exception do
    begin
      AReject(LException.Message);
    end;
  end;
end;

function ContinueCapture(AEvent: TJSMouseEvent): Boolean;
begin
  { A visible harness action acknowledges an ACTUAL observed capture. It never
    fabricates a screenshot or advances the fixture before that ordinary click. }

  if document.body.getAttribute('data-capture-checkpoint') = 'view-sections-live' then
  begin
    document.body.setAttribute('data-capture-observed', 'view-sections-live');
  end;
  Result := True;
end;

procedure Capture; async;
var
  LStarted: Double;
  LContinue: TJSHTMLElement;
begin
  document.body.setAttribute('data-capture-checkpoint', 'view-sections-live');
  LStarted := window.performance.now;
  LContinue := TJSHTMLElement(document.createElement('button'));
  LContinue.id := 'view-sections-capture-continue';
  LContinue.setAttribute('type', 'button');
  LContinue.className := 'nyx-button';
  LContinue.textContent := 'Continue after capture';
  LContinue.style.setProperty('position', 'fixed');
  LContinue.style.setProperty('right', '12px');
  LContinue.style.setProperty('bottom', '12px');
  LContinue.style.setProperty('z-index', '1000');
  LContinue.onclick := @ContinueCapture;
  document.body.appendChild(LContinue);
  try
    repeat
      await(TJSPromise.resolve(TJSPromise.new(@Pause)));

      if window.performance.now - LStarted > 30000 then
      begin
        raise ENyxModel.Create('View section capture acknowledgment timed out');
      end;
    until document.body.getAttribute('data-capture-observed') = 'view-sections-live';
  finally
    LContinue.onclick := nil;
    LContinue.remove;
  end;
end;
{$endif}

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LHeader: ISection;
  LDetails: ISection;
  LHeaderDoc: TNyxDocument;
  LDetailsDoc: TNyxDocument;
  LNextHeader: TNyxDocument;
  LNextDetails: TNyxDocument;
  LHeaderChange: INyxViewSectionChange;
  LDetailsChange: INyxViewSectionChange;
  LStale: INyxViewSectionChange;
  LObserver: TScenario;
  LObserverLease: INyxViewSectionObserver;
  LHeaderHost: THost;
  LDetailsHost: THost;
  LInput: TFace;
  LAction: TFace;
  LDetailsInput: TFace;
  LHeaderRoot: TNyxNode;
  LDetailsRoot: TNyxNode;
  LBefore: TNyxText;
  LRefused: Boolean;
  LRevision: Integer;
  LHeaderMount: INyxCollectionMount;
  LFormerDetailsMount: INyxCollectionMount;
  LDetailsMount: INyxCollectionMount;
  LHostWidth: Integer;
  LProbe: TReentrantProbe;
  LProbeLease: INyxEventCallback;
  LProbeToken: INyxEventSubscription;
  LViewProbeToken: INyxCollectionViewSubscription;
  {$ifdef PAS2JS}LHostStyle: TNyxText;{$endif}
  {$ifndef PAS2JS}LWindow: TForm;{$endif}
begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  LHeaderDoc := Example('header', 'Keep your ideas close', False);
  LDetailsDoc := Example('details', 'Shape the details', False);
  LNextHeader := Example('header', 'Your next idea', True);
  LNextDetails := Example('details', 'Explore another possibility', True);
  LObserver := TScenario.Create;
  LObserverLease := LObserver;
  {$ifdef PAS2JS}
  LHeaderHost := TJSHTMLElement(document.createElement('section'));
  LDetailsHost := TJSHTMLElement(document.createElement('section'));
  LHeaderHost.style.cssText := 'height:340px;flex:1;min-width:0;max-width:440px;';
  LDetailsHost.style.cssText := LHeaderHost.style.cssText;
  document.getElementById('sections').appendChild(LHeaderHost);
  document.getElementById('sections').appendChild(LDetailsHost);
  {$else}
  LWindow := TForm.CreateNew(nil);
  LWindow.Caption := 'Nyx view sections';
  LWindow.ClientWidth := 900;
  LWindow.ClientHeight := 380;
  LHeaderHost := TPhysicalMoveHost.Create(LWindow);
  LHeaderHost.Parent := LWindow;
  LHeaderHost.SetBounds(12, 12, 430, 350);
  LHeaderHost.BevelOuter := bvNone;
  LDetailsHost := TPanel.Create(LWindow);
  LDetailsHost.Parent := LWindow;
  LDetailsHost.SetBounds(454, 12, 430, 350);
  LDetailsHost.BevelOuter := bvNone;
  LWindow.Show;
  Application.ProcessMessages;
  {$endif}
  try
    {$ifdef PAS2JS}
    LHeader := NewNyxBrowserViewSection(TNyxViewSectionRef.Named('header'), LHeaderHost,
      nil, LObserverLease);
    LDetails := NewNyxBrowserViewSection(TNyxViewSectionRef.Named('details'), LDetailsHost,
      @LObserver.Configure, LObserverLease);
    {$else}
    LHeader := NewNyxLCLViewSection(TNyxViewSectionRef.Named('header'), LHeaderHost,
      nil, LObserverLease);
    LDetails := NewNyxLCLViewSection(TNyxViewSectionRef.Named('details'), LDetailsHost,
      LObserver.Configure, LObserverLease);
    {$endif}
    Check((LHeader.Root = nil) and (LHeader.Revision = 0), 'new section has no accepted view');
    LHeaderChange := LHeader.Prepare(LHeaderDoc, LHeaderDoc.Pages[0]);
    LDetailsChange := LDetails.Prepare(LDetailsDoc, LDetailsDoc.Pages[0]);
    Check((LHeader.Root = nil) and (LDetails.Root = nil) and (LObserver.Clicks = 0),
      'preparation leaves both live hosts and observers unchanged');
    FreeAndNil(LHeaderDoc);
    FreeAndNil(LDetailsDoc);
    Check(PublishNyxViewSections([LHeaderChange, LDetailsChange]),
      'independent source snapshots publish as one actual two-view group');
    Check((LHeaderChange.State = vcsPublished) and (LDetailsChange.State = vcsPublished) and
      (LHeader.Revision = 1) and (LDetails.Revision = 1), 'publication has explicit revisions/states');
    LInput := Input(LHeader, 'header-input');
    LAction := Face(LHeader, 'header-action');
    LHeaderRoot := LHeader.Root;
    LHeaderMount := LHeader.Renderer.CollectionMount('header-table');
    LFormerDetailsMount := LDetails.Renderer.CollectionMount('details-table');
    Check(LHeaderMount.EditCell(NyxItem(NyxCollection('header-notes'), 'first'),
      0, 'Retained row edit'), 'actual table mount admits a typed-scoped row edit');
    Draft(LInput);
    LBefore := TNyxCodec.Encode(LNextDetails);
    LDetailsChange := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    Check((LHeader.Root = LHeaderRoot) and (Input(LHeader, 'header-input') = LInput) and
      (TextOf(LInput) = TNyxText('Independent draft / 🌙')), 'staging another section keeps live input');
    Check(PublishNyxViewSections([LDetailsChange]) and
      (Input(LHeader, 'header-input') = LInput) and (Face(LHeader, 'header-action') = LAction) and
      (LHeader.Root = LHeaderRoot) and (TextOf(LInput) = TNyxText('Independent draft / 🌙')),
      'structural sibling publication preserves actual unrelated controls and draft');
    Check(LHeaderMount.Connected and not LFormerDetailsMount.Connected and
      (LHeaderMount.View.CellText(NyxItem(NyxCollection('header-notes'), 'first'), 0) =
        'Retained row edit'), 'unmentioned collection retains its edit; former sibling mount disconnects');
    Check(TNyxCodec.Encode(LNextDetails) = LBefore, 'view publication does not mutate its source document');
    Click(LAction);
    Click(Face(LDetails, 'details-action'));
    Check(LObserver.Clicks = 2, 'retained and newly published actual buttons reach the managed observer');
    Check(LDetails.Root.Find('details-extra') <> nil, 'new structure belongs to the published section');

    LDetailsRoot := LDetails.Root;
    LDetailsMount := LDetails.Renderer.CollectionMount('details-table');
    LDetailsInput := Input(LDetails, 'details-input');
    GFailBuild := True;
    LRefused := False;
    try
      LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    GFailBuild := False;
    Check(LRefused and (LDetails.Root = LDetailsRoot) and
      (Input(LDetails, 'details-input') = LDetailsInput), 'failed real target factory retains accepted view');

    LHeaderChange := LHeader.Prepare(LNextHeader, LNextHeader.Pages[0]);
    LDetailsChange := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    GFailUpdate := True;
    LRefused := False;
    try
      PublishNyxViewSections([LHeaderChange, LDetailsChange]);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not GFailUpdate and (LHeader.Root = LHeaderRoot) and
      (LDetails.Root = LDetailsRoot) and (Input(LHeader, 'header-input') = LInput) and
      (Input(LDetails, 'details-input') = LDetailsInput),
      'failed second physical preview restores BOTH exact old control trees');
    Check(LHeaderMount.Connected and LDetailsMount.Connected,
      'failed publication retains both accepted collection scopes');
    Check((LHeaderChange.State = vcsCanceled) and (LDetailsChange.State = vcsCanceled) and
      (LHeader.Revision = 1) and (LDetails.Revision = 2) and
      (TextOf(LInput) = TNyxText('Independent draft / 🌙')),
      'rollback preserves revisions/draft and cancels the whole group');
    Click(LAction);
    Check(LObserver.Clicks = 3, 'rollback retains original button event route');

    LStale := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    LDetailsChange := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    Check(PublishNyxViewSections([LDetailsChange]), 'a newer view supersedes an outstanding proposal');
    LDetailsRoot := LDetails.Root;
    Check(not PublishNyxViewSections([LStale]) and (LDetails.Root = LDetailsRoot) and
      (LStale.State = vcsPrepared), 'stale revision refuses before placement changes');
    LStale.Cancel;
    Check(LStale.State = vcsCanceled, 'explicit cancellation releases an unpublished candidate');
    LDetailsChange := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    LRefused := False;
    try
      PublishNyxViewSections([LDetailsChange, LDetailsChange]);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LDetails.Root = LDetailsRoot) and
      (LDetailsChange.State = vcsPrepared), 'duplicate group refuses before preview');
    LDetailsChange.Cancel;
    LHeaderChange := LHeader.Prepare(LNextHeader, LNextHeader.Pages[0]);
    LProbe := TReentrantProbe.Create;
    LProbeLease := LProbe;
    LProbe.Change := LHeaderChange;
    { EditCell can already focus/select this row. Force a real subsequent
      selection transition so the guard is exercised inside Notify. }
    LHeaderMount.View.ClearSelection;
    LViewProbeToken := LHeaderMount.View.Subscribe({$ifdef PAS2JS}@{$endif}LProbe.ViewChanged);
    try
      LHeaderMount.View.Select(NyxItem(NyxCollection('header-notes'), 'first'));
      Check(LProbe.Refused and (LHeader.Root = LHeaderRoot),
        'collection selection notification cannot retire its borrowed view');
    finally
      LViewProbeToken.Disconnect;
      LViewProbeToken := nil;
      LProbe.Change := nil;
      LProbeLease := nil;
      LHeaderChange.Cancel;
    end;
    LDetailsChange := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    LProbe := TReentrantProbe.Create;
    LProbeLease := LProbe;
    LProbe.Change := LDetailsChange;
    LProbeToken := LDetails.Renderer.Events.On(
      NyxControlEvents('details-action', niRuntime), ntClick).Subscribe(LProbeLease);
    try
      Click(Face(LDetails, 'details-action'));
      Check(LProbe.Refused and (LDetailsChange.State = vcsPrepared) and
        (LDetails.Root = LDetailsRoot), 'actual callback cannot retire its borrowed input view');
      LProbe.Change := nil;
      Check(PublishNyxViewSections([LDetailsChange]) and not LProbeToken.Active,
        'later idle publication succeeds and retires the former event scope');
      LDetailsRoot := LDetails.Root;
    finally
      LProbeToken.Cancel;
      LProbe.Change := nil;
      LProbeToken := nil;
      LProbeLease := nil;
    end;
    LDetailsChange := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    {$ifdef PAS2JS}
    LHostStyle := LDetailsHost.style.cssText;
    LHostWidth := LDetailsHost.clientWidth;
    LDetailsHost.style.setProperty('flex', 'none');
    LDetailsHost.style.setProperty('max-width', 'none');
    LDetailsHost.style.setProperty('width', IntToStr(LHostWidth + 7) + 'px');
    {$else}
    LHostWidth := LDetailsHost.Width;
    LDetailsHost.Width := LHostWidth + 7;
    {$endif}
    Check(not PublishNyxViewSections([LDetailsChange]) and (LDetails.Root = LDetailsRoot),
      'changed host allocation refuses stale prepared target geometry');
    {$ifdef PAS2JS}LDetailsHost.style.cssText := LHostStyle;
    {$else}LDetailsHost.Width := LHostWidth;{$endif}
    LDetailsChange.Cancel;
    LHeaderChange := LHeader.Prepare(LNextHeader, LNextHeader.Pages[0]);
    Check(PublishNyxViewSections([LHeaderChange]), 'later fresh replacement succeeds after rollback');
    Check((LHeader.Root.Find('header-extra') <> nil) and (LHeader.Revision = 2),
      'successful structural publication transfers the new owned tree');
    LRevision := LHeader.Revision;
    LHeaderChange := LHeader.Prepare(LNextHeader, LNextHeader.Pages[0]);
    LHeaderChange.Cancel;
    Check((LHeader.Revision = LRevision) and (LHeader.Root.Find('header-extra') <> nil),
      'canceling a staged candidate retains the accepted view');
    Check(PublishNyxViewSections([]), 'empty publication group is an explicit no-op');
    {$ifndef PAS2JS}
    NativePublicationMoveRecovery(LHeader, TPhysicalMoveHost(LHeaderHost),
      LNextHeader, LObserver);
    NativeGridPlaneRecovery(LHeader, TPhysicalMoveHost(LHeaderHost), LNextHeader);
    NativeCleanupRecovery(LDetails, LDetailsHost, LNextDetails, LObserver);
    LRevision := LHeader.Revision;
    {$endif}
    {$ifdef PAS2JS}await(Capture);{$else}
    Application.ProcessMessages;

    if ParamCount > 0 then
    begin
      SaveNyxNativeCapture(LWindow, ParamStr(1), ncmPrint);
    end;
    {$endif}
    LDetailsMount := LDetails.Renderer.CollectionMount('details-table');
    LDetails.Close;
    Check((LDetails.Root = nil) and (LHeader.Root <> nil),
      'explicit section retirement keeps its independent sibling alive');
    Check(not LDetailsMount.Connected,
      'closed section no longer has a live collection mount');
    LHeader.Close;
    Check((LHeader.Root = nil) and (LHeader.Revision = LRevision + 1),
      'explicit retirement revokes the mounted owner');
    {$ifdef PAS2JS}
    BrowserCleanupGroupRecovery(LHeaderHost, LDetailsHost, LNextHeader, LNextDetails);
    {$else}
    NativeCleanupGroupRecovery(TPhysicalMoveHost(LHeaderHost), LDetailsHost,
      LNextHeader, LNextDetails);
    {$endif}
    WriteLn('PASS ', GChecks, ' actual staged section checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-section-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
    {$endif}
  finally
    LProbeToken := nil;
    LViewProbeToken := nil;
    LProbeLease := nil;
    LHeaderChange := nil;
    LDetailsChange := nil;
    LStale := nil;
    LHeader := nil;
    LDetails := nil;
    LObserverLease := nil;
    LHeaderDoc.Free;
    LDetailsDoc.Free;
    LNextHeader.Free;
    LNextDetails.Free;
    {$ifdef PAS2JS}
    LHeaderHost.remove;
    LDetailsHost.remove;
    {$else}
    LWindow.Free;
    {$endif}
  end;
end;

{$ifdef PAS2JS}
procedure Start; async;
begin
  try
    await(Run);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-section-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}Start;{$else}
  try
    Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
