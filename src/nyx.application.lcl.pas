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

unit nyx.application.lcl;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.behavior,
  nyx.text,
  nyx.publication,
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  nyx.model,
  nyx.types,
  nyx.state,
  nyx.collections.registry,
  nyx.application.state,
  nyx.application.view,
  nyx.application.context,
  nyx.flow.routing, nyx.flow, nyx.files.save, nyx.files.open,
  nyx.application.navigation,
  nyx.selection,
  nyx.scheduler,
  nyx.application.resources,
  nyx.resource.context,
  nyx.resources,
  nyx.resource.sources,
  nyx.resources.loader,
  nyx.render.lcl,
  nyx.menu.button;

type
  { CreateNew avoids a resource-file dependency for a code-first main window.
    Application.CreateForm still establishes Lazarus main-form lifetime. }
  TNyxApplicationForm = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Native application host for ordinary pages or an explicitly focused authored
    control. The complete admitted context stays borrowed; runtime stores and
    resource owners are independent. Generated entry points retire this host and
    its window/receivers on the UI thread before releasing the source document. }
  TNyxLCLApplication = class
  private
    FForm: TNyxApplicationForm;
    FDocument: TNyxDocument;
    FViewOptions: TNyxApplicationView;
    FNavigation: INyxPageNavigation;
    FNavigationOptions: TNyxSelectionOptions;
    FNavigator: TNyxLCLRenderer;
    FRenderer: TNyxLCLRenderer;
    FViewHost: TPanel;
    FRuntime: TNyxApplicationState;
    FContextLease: INyxApplicationContextLease;
    FFlowRoutes: INyxFlowRouteBinding;
    FFlowResolver: INyxLogicResolver;
    FFileSaveHost: INyxFileSaveHost;
    FFileOpenHost: INyxFileOpenHost;
    FMenus: INyxMenuBindings;
    FResources: INyxApplicationResources;
    FResourceOptions: TNyxApplicationResourceOptions;
    FResourceResolver: INyxResourceResolver;
    FResourceSubscription: INyxResourceSubscription;
    procedure SynchronizeNavigation(const APage: TNyxControlRef);
    function GetNavigationState: INyxPageNavigationState;
    function GetState: TNyxState;
    function GetCollections: INyxCollections;
    function GetResources: INyxApplicationResources;
    function GetGeneration: INyxCancellationScope;
    function GetContext: INyxApplicationContext;
    procedure PrepareResources(ADocument: TNyxDocument);
    function ValidateResources(const AContext: INyxResourceContext): Boolean;
    function PrepareResourcePublication(const AContext: INyxResourceContext;
      out APrepared: INyxPreparedPublication): Boolean;
    procedure Navigate(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run(ADocument: TNyxDocument);
    { Mount an admitted window without starting Lazarus's blocking message loop.
      Useful when embedding in an existing LCL application or exercising real
      controls. The caller may show Window; Run supplies Show/Application.Run.
      Mount is single-use and borrows a document that remains unchanged. }
    procedure Mount(ADocument: TNyxDocument);
    { Select one authored control before mounting, retaining its admitted context
      without automatic page navigation. Empty/default options keep normal pages. }
    function ConfigureView(const AView: TNyxApplicationView): TNyxLCLApplication;
    procedure ShowPage(const AID: TNyxText);
    { Configure copied current-page appearance before mounting. Default variants
      distinguish the current page; authored mode leaves creator variants intact. }
    function ConfigureNavigation(const AOptions: TNyxSelectionOptions): TNyxLCLApplication;
    { Nil for single-page/unmounted applications; retained state owns only the
      independent navigation recipe and remains safe after application disposal. }
    property NavigationState: INyxPageNavigationState read GetNavigationState;
    property State: TNyxState read GetState;
    { Independent runtime collection stores persist across ShowPage. }
    property Collections: INyxCollections read GetCollections;
    { Callback/runtime capability survives navigation and revokes before disposal.
      It retains no application/router/tree; access is UI-only. }
    property Context: INyxApplicationContext read GetContext;
    { Explicit creator authority is supplied before mounting. Saved definitions
      never select a host implementation implicitly. Runtime attachment/reporting
      retains no application or document and revokes before view teardown. }
    function ConfigureLogic(const AResolver: INyxLogicResolver): TNyxLCLApplication;
    { Explicit runtime authority, never inferred from saved graph data. The
      retained host composes with ConfigureLogic regardless of call order; nil
      disables this layer. Configure before mounting. Hosts must not retain this
      application/document, and any borrowed host owner must outlive the host.
      Pending routes retire before the host or rendered controls are released. }
    function ConfigureFileSaving(const AHost: INyxFileSaveHost): TNyxLCLApplication;
    { Independent single-file selection/read authority, supplied before Mount.
      Composes with saving and ConfigureLogic in either call order; nil removes
      only this layer. The host must not retain the application/document; any
      borrowed dialog owner must outlive it. Host checks and byte budgets remain
      authoritative. Pending routes retire before this host and its controls. }
    function ConfigureFileOpening(const AHost: INyxFileOpenHost): TNyxLCLApplication;
    property FlowRoutes: INyxFlowRouteBinding read FFlowRoutes;
    { Private application catalog/load diagnostics survive navigation. Configure
      before mounting; supplying a resolver replaces the target HTTP/cache path.
      Stop retires requests before controls and borrowed callbacks are freed. }
    function ConfigureResources(const AOptions: TNyxApplicationResourceOptions;
      const AResolver: INyxResourceResolver = nil): TNyxLCLApplication;
    property Resources: INyxApplicationResources read GetResources;
    property View: TNyxLCLRenderer read FRenderer;
    { Borrowed navigation renderer for embedding/input; destroy neither view. }
    property NavigationView: TNyxLCLRenderer read FNavigator;
    { Retained observations must retire before page changes/window disposal. }
    property Menus: INyxMenuBindings read FMenus;
    property Window: TNyxApplicationForm read FForm;
  end;

implementation

uses
  nyx.logic.files, nyx.logic.files.open,
  nyx.callbacks,
  nyx.events,
  nyx.resources.http.lcl,
  nyx.resource.cache.lcl,
  nyx.menu.lcl;

constructor TNyxApplicationForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
end;

constructor TNyxLCLApplication.Create;
begin
  inherited Create;
  FRenderer := TNyxLCLRenderer.Create;
  FNavigator := TNyxLCLRenderer.Create;
  FNavigator.OnEvent := Navigate;
  FResourceOptions := NyxApplicationResourceOptions;
  FNavigationOptions := TNyxSelectionOptions.Defaults;
end;

destructor TNyxLCLApplication.Destroy;
begin

  if FContextLease <> nil then
  begin
    FContextLease.Close;
    FContextLease := nil;
  end;

  if FFlowRoutes <> nil then
  begin
    FFlowRoutes.Close;
    FFlowRoutes := nil;
  end;

  if FResources <> nil then
  begin
    FResources.Stop;
  end;

  if FResourceSubscription <> nil then
  begin
    FResourceSubscription.Disconnect;
    FResourceSubscription := nil;
  end;
  FMenus := nil;
  FRenderer.Free;
  FNavigator.Free;
  FRuntime.Free;
  FNavigation := nil;
  FForm.Free;
  inherited Destroy;
end;

procedure TNyxLCLApplication.Run(ADocument: TNyxDocument);
begin
  Mount(ADocument);
  FForm.Show;
  Application.Run;
end;

procedure TNyxLCLApplication.Mount(ADocument: TNyxDocument);
var
  LHost: TPanel;
  LResolver: INyxLogicResolver;
  LRoot: TNyxNode;
begin
  LRoot := FViewOptions.Resolve(ADocument);

  if FDocument <> nil then
  begin
    raise ENyxModel.Create('Native application can only be mounted once');
  end;
  FRuntime := FViewOptions.CreateRuntime(ADocument, npfNativeLCL,
    FResourceOptions.Locale, FResourceOptions.Fallback);
  FDocument := ADocument;
  try
    FContextLease := NewNyxApplicationContext(FRenderer.Events.Scheduler,
      GetState, GetCollections, GetGeneration);
    LResolver := FFlowResolver;

    if FFileSaveHost <> nil then
    begin
      LResolver := NewNyxFileLogicResolver(FFileSaveHost, LResolver);
    end;

    if FFileOpenHost <> nil then
    begin
      LResolver := NewNyxOpenFileLogicResolver(FFileOpenHost, LResolver);
    end;
    FRenderer.ConfigureEmbeddedBehavior(FContextLease.Context, LResolver);
    { A focused reusable/part can retain its original owner without any page.
      Supplement page registrations with its exact realized identities, keeping
      explicit ShowPage usable without duplicate page-owned registrations.
      The router owns callbacks; both attachments use revocable application
      context and retire before rendered controls are disposed. }

    if FViewOptions.IsFocused then
    begin
      BindNyxCallbacks(ADocument, FRenderer.Events, nil, FContextLease.Context, LRoot);
      FFlowRoutes := BindNyxFlowRoutes(ADocument, FRenderer.Events,
        FContextLease.Context, LResolver, LRoot);
    end
    else
    begin
      BindNyxCallbacks(ADocument, FRenderer.Events, nil, FContextLease.Context);
      FFlowRoutes := BindNyxFlowRoutes(ADocument, FRenderer.Events,
        FContextLease.Context, LResolver);
    end;
    PrepareResources(ADocument);
    Application.CreateForm(TNyxApplicationForm, FForm);
    FForm.Caption := ADocument.Title;
    FForm.SetBounds(100, 100, 840, 720);

    if (ADocument.Count > 1) and not FViewOptions.IsFocused then
    begin
      FNavigation := NewNyxPageNavigation(ADocument);
      NyxPageNavigationState(FNavigation).Configure(FNavigationOptions);
      NyxPageNavigationState(FNavigation).SelectPage(NyxControl(ADocument.Pages[0].ID));
      LHost := TPanel.Create(FForm);
      LHost.Parent := FForm;
      LHost.Align := alTop;
      LHost.Height := 68;
      LHost.BevelOuter := bvNone;
      FNavigator.Render(FNavigation.Document, FNavigation.Root.Node, LHost);
    end;
    FViewHost := TPanel.Create(FForm);
    FViewHost.Parent := FForm;
    FViewHost.Align := alClient;
    FViewHost.BevelOuter := bvNone;
    FRenderer.Render(FDocument, LRoot, FViewHost, FRuntime.State,
      FViewOptions.InitialCollections(FDocument, FRuntime));
    FMenus := BindNyxLCLMenus(FDocument, FRenderer);
    SynchronizeNavigation(NyxControl(LRoot.ID));
    FResources.Start;
  except

    if FContextLease <> nil then
    begin
      FContextLease.Close;
      FContextLease := nil;
    end;

    if FFlowRoutes <> nil then
    begin
      FFlowRoutes.Close;
      FFlowRoutes := nil;
    end;

    if FResources <> nil then
    begin
      FResources.Stop;
    end;
    FResourceSubscription := nil;
    FMenus := nil;
    FRenderer.Unmount;
    FRenderer.ResourceUpdates := nil;
    FRenderer.ResourceContext := nil;
    FResources := nil;
    FNavigator.Unmount;
    FNavigation := nil;
    FreeAndNil(FRuntime);
    FreeAndNil(FForm);
    FDocument := nil;
    FViewHost := nil;
    raise;
  end;
end;

function TNyxLCLApplication.GetGeneration: INyxCancellationScope;
begin

  if FViewHost = nil then
  begin
    raise ENyxApplicationContext.Create('View generation requires a mounted application');
  end;
  Result := NyxEventGeneration(FRenderer.Events);
end;

function TNyxLCLApplication.ConfigureView(const AView: TNyxApplicationView): TNyxLCLApplication;
begin

  if FDocument <> nil then
  begin
    raise ENyxModel.Create('Configure the application view before mounting');
  end;
  FViewOptions := AView;
  Result := Self;
end;

function TNyxLCLApplication.ConfigureLogic(const AResolver: INyxLogicResolver): TNyxLCLApplication;
begin

  if FDocument <> nil then
  begin
    raise ENyxModel.Create('Configure Logic authority before mounting the application');
  end;
  FFlowResolver := AResolver;
  Result := Self;
end;

function TNyxLCLApplication.ConfigureFileSaving(const AHost: INyxFileSaveHost): TNyxLCLApplication;
begin

  if FDocument <> nil then
  begin
    raise ENyxModel.Create('Configure file-save authority before mounting the application');
  end;
  FFileSaveHost := AHost;
  Result := Self;
end;

function TNyxLCLApplication.ConfigureFileOpening(const AHost: INyxFileOpenHost): TNyxLCLApplication;
begin

  if FDocument <> nil then
  begin
    raise ENyxModel.Create('Configure file-open authority before mounting the application');
  end;
  FFileOpenHost := AHost;
  Result := Self;
end;

function TNyxLCLApplication.GetContext: INyxApplicationContext;
begin

  if FContextLease = nil then
  begin
    raise ENyxApplicationContext.Create('Application context requires a mounted application');
  end;
  Result := FContextLease.Context;
end;

function TNyxLCLApplication.GetState: TNyxState;
begin

  if FRuntime = nil then
  begin
    raise ENyxState.Create('Application state requires a mounted application');
  end;
  Result := FRuntime.State;
end;

function TNyxLCLApplication.ConfigureResources(const AOptions: TNyxApplicationResourceOptions;
  const AResolver: INyxResourceResolver): TNyxLCLApplication;
begin

  if FRuntime <> nil then
  begin
    raise ENyxResource.Create('Configure application resources before mounting');
  end;
  AOptions.Validate;
  FResourceOptions := AOptions;
  FResourceResolver := AResolver;
  Result := Self;
end;

procedure TNyxLCLApplication.PrepareResources(ADocument: TNyxDocument);
var
  LResolver: INyxResourceResolver;
  LIndex: Integer;
begin
  LResolver := FResourceResolver;

  if LResolver = nil then
  begin
    for LIndex := 0 to ADocument.Resources.Count - 1 do
    begin

      if ADocument.Resources.Definition(ADocument.Resources.Reference(LIndex),
        ADocument.Resources.Locale(LIndex)).Source.Kind = rskHosted then
      begin
        LResolver := NewNyxResourceResolver(NewNyxNativeResourceTransport(FRenderer.Events.Scheduler),
          nil, NewNyxFileResourceCache);
        Break;
      end;
    end;
  end;
  FResources := NewNyxApplicationResources(ADocument.Resources,
    FRenderer.Events.Scheduler, FResourceOptions, LResolver);
  FRuntime.AttachResources(FResources);
  FResourceSubscription := NyxPreparedApplicationResources(FResources).SubscribePrepared(
    ValidateResources, PrepareResourcePublication);
  FRenderer.ResourceUpdates := FResources;
end;

function TNyxLCLApplication.GetResources: INyxApplicationResources;
begin

  if FResources = nil then
  begin
    raise ENyxResource.Create('Application resources require a mounted application');
  end;
  Result := FResources;
end;

function TNyxLCLApplication.ValidateResources(const AContext: INyxResourceContext): Boolean;
begin
  Result := FRenderer.CanReloadResources(AContext);
end;

function TNyxLCLApplication.PrepareResourcePublication(const AContext: INyxResourceContext;
  out APrepared: INyxPreparedPublication): Boolean;
begin
  { Runtime preparation owns shared/hidden/instance rows. This mounted receiver
    contributes scalar properties only, against the same held state snapshot. }
  APrepared := FRenderer.PrepareResourcePublication(AContext);
  Result := True;
end;

function TNyxLCLApplication.GetCollections: INyxCollections;
begin

  if FRuntime = nil then
  begin
    raise ENyxState.Create('Runtime collections require a mounted application');
  end;
  Result := FRuntime.Collections;
end;

procedure TNyxLCLApplication.ShowPage(const AID: TNyxText);
var
  LIndex: Integer;
begin

  if (FRuntime <> nil) and FRuntime.ResourcePublicationBusy then
  begin
    raise ENyxState.Create('Navigation refuses during application model publication');
  end;

  if FDocument = nil then
  begin
    raise ENyxModel.Create('Application must be mounted before navigating');
  end;
  for LIndex := 0 to FDocument.Count - 1 do
  begin

    if FDocument.Pages[LIndex].ID = AID then
    begin
      FMenus := nil;
      FRenderer.Render(FDocument, FDocument.Pages[LIndex], FViewHost, FRuntime.State,
        FRuntime.PageCollections(AID));
      FMenus := BindNyxLCLMenus(FDocument, FRenderer);
      SynchronizeNavigation(NyxControl(AID));
      Exit;
    end;
  end;
  raise ENyxModel.Create('Application page not found: ' + AID);
end;

procedure TNyxLCLApplication.Navigate(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LPage: TNyxControlRef;
begin

  if (FNavigation <> nil) and FNavigation.Resolve(AEvent, LPage) then
  begin
    ShowPage(LPage.ID);
  end;
end;

function TNyxLCLApplication.ConfigureNavigation(
  const AOptions: TNyxSelectionOptions): TNyxLCLApplication;
begin

  if FDocument <> nil then
  begin
    raise ENyxModel.Create('Configure navigation before mounting');
  end;
  AOptions.Validate;
  FNavigationOptions := AOptions;
  Result := Self;
end;

function TNyxLCLApplication.GetNavigationState: INyxPageNavigationState;
begin
  Result := nil;

  if FNavigation <> nil then
  begin
    Result := NyxPageNavigationState(FNavigation);
  end;
end;

procedure TNyxLCLApplication.SynchronizeNavigation(const APage: TNyxControlRef);
var
  LState: INyxPageNavigationState;
  LIndex: Integer;
  LButton: TControl;
begin

  if FNavigation = nil then
  begin
    Exit;
  end;
  LState := NyxPageNavigationState(FNavigation);
  LState.SelectPage(APage);

  { Keep notifying controls mounted. Ordinary document refresh refuses an active
    command; this explicit runtime projection changes only our variant policy. }
  LState.Project(FNavigator.Root);
  FNavigator.Sync;
  for LIndex := 0 to FNavigation.Root.Node.Count - 1 do
  begin
    LButton := FNavigator.ControlFor(NyxPageNavigationButtonID(LIndex));
    { LCL exposes a description rather than aria-current. Keep this separate
      from scalar AccessibleValue, which ordinary renderer synchronization owns.
      Description survives layout/value synchronization without changing the
      exact caption/name. Assistive/widgetset interpretation remains separate. }

    if LState.IsCurrent(NyxControl(NyxPageNavigationButtonID(LIndex))) then
    begin
      LButton.AccessibleDescription := 'Current page';
    end
    else
    begin
      LButton.AccessibleDescription := '';
    end;
  end;
end;

end.
