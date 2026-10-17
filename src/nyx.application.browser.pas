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

unit nyx.application.browser;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.behavior,
  nyx.text,
  nyx.publication,
  SysUtils,
  Web,
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
  nyx.render.browser,
  nyx.menu.button;

type
  { Application host for generated/handwritten documents and explicitly focused
    authored controls. Ordinary navigation is a public Nyx toolbar; focused mode
    retains dependency context without displaying its other pages automatically.
    The document is borrowed and must outlive this host. Runtime stores/resources
    are independent; connected host/receiver retirement belongs to the UI. }
  TNyxBrowserApplication = class
  private
    FDocument: TNyxDocument;
    FViewOptions: TNyxApplicationView;
    FNavigation: INyxPageNavigation;
    FNavigationOptions: TNyxSelectionOptions;
    FNavigator: TNyxBrowserRenderer;
    FRenderer: TNyxBrowserRenderer;
    FViewHost: TJSHTMLElement;
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
    procedure Run(ADocument: TNyxDocument; AHost: TJSHTMLElement);
    { Select one authored control before mounting, retaining its admitted context
      without automatic page navigation. Empty/default options keep normal pages. }
    function ConfigureView(const AView: TNyxApplicationView): TNyxBrowserApplication;
    procedure ShowPage(const AID: TNyxText);
    { Configure copied current-page appearance before mounting. Default variants
      distinguish the current page; authored mode leaves creator variants intact. }
    function ConfigureNavigation(const AOptions: TNyxSelectionOptions): TNyxBrowserApplication;
    { Nil for single-page/unmounted applications; retained state owns only the
      independent navigation recipe and remains safe after application disposal. }
    property NavigationState: INyxPageNavigationState read GetNavigationState;
    { Runtime state survives page changes. The owned renderer is borrowed for
      factory registration, event/error callbacks and identity lookup. Never free
      it. Run mounts once; destroy the application before mutating its document. }
    property State: TNyxState read GetState;
    { Independent runtime collection stores persist across ShowPage. }
    property Collections: INyxCollections read GetCollections;
    { Callback/runtime capability survives navigation and revokes before disposal.
      It retains no application/router/tree; access is UI-only. }
    property Context: INyxApplicationContext read GetContext;
    { Explicit creator authority is supplied before mounting. Saved definitions
      never select a host implementation implicitly. Runtime attachment/reporting
      retains no application or document and revokes before view teardown. }
    function ConfigureLogic(const AResolver: INyxLogicResolver): TNyxBrowserApplication;
    { Explicit runtime authority, never inferred from saved graph data. The
      retained host composes with ConfigureLogic regardless of call order; nil
      disables this layer. Configure before mounting. Hosts must not retain this
      application/document, and any borrowed host owner must outlive the host.
      Pending routes retire before the host or rendered controls are released. }
    function ConfigureFileSaving(const AHost: INyxFileSaveHost): TNyxBrowserApplication;
    { Independent single-file selection/read authority, supplied before Run.
      Composes with saving and ConfigureLogic in either call order; nil removes
      only this layer. The host must not retain the application/document.
      Browser activation/selection and byte budgets still apply. A deferred
      route can report ActivationRequired; configuration fabricates no gesture.
      Pending routes retire before this retained host and its controls. }
    function ConfigureFileOpening(const AHost: INyxFileOpenHost): TNyxBrowserApplication;
    property FlowRoutes: INyxFlowRouteBinding read FFlowRoutes;
    { Private application catalog/load diagnostics survive navigation. Configure
      before mounting; supplying a resolver replaces the target HTTP/cache path.
      Stop retires requests before controls and borrowed callbacks are freed. }
    function ConfigureResources(const AOptions: TNyxApplicationResourceOptions;
      const AResolver: INyxResourceResolver = nil): TNyxBrowserApplication;
    property Resources: INyxApplicationResources read GetResources;
    property View: TNyxBrowserRenderer read FRenderer;
    { Borrowed navigation renderer for embedding and input; destroy neither view. }
    property NavigationView: TNyxBrowserRenderer read FNavigator;
    { Mounted menu context. Release any retained menus/bindings before changing
      pages or destroying the application; target anchors are borrowed. }
    property Menus: INyxMenuBindings read FMenus;
  end;

implementation

uses
  nyx.logic.files, nyx.logic.files.open,
  nyx.callbacks,
  nyx.events,
  nyx.resources.http.browser,
  nyx.resource.cache.browser,
  nyx.menu.browser;

constructor TNyxBrowserApplication.Create;
begin
  inherited Create;
  FRenderer := TNyxBrowserRenderer.Create;
  FNavigator := TNyxBrowserRenderer.Create;
  FNavigator.OnEvent := Navigate;
  FResourceOptions := NyxApplicationResourceOptions;
  FNavigationOptions := TNyxSelectionOptions.Defaults;
end;

destructor TNyxBrowserApplication.Destroy;
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
  inherited Destroy;
end;

procedure TNyxBrowserApplication.Run(ADocument: TNyxDocument; AHost: TJSHTMLElement);
var
  LHost: TJSHTMLElement;
  LContainer: TJSHTMLElement;
  LResolver: INyxLogicResolver;
  LRoot: TNyxNode;
begin
  LRoot := FViewOptions.Resolve(ADocument);

  if (AHost = nil) or (FDocument <> nil) then
  begin
    raise ENyxModel.Create('Application requires a host and can only be mounted once');
  end;
  FRuntime := FViewOptions.CreateRuntime(ADocument, npfBrowser,
    FResourceOptions.Locale, FResourceOptions.Fallback);
  FDocument := ADocument;
  LContainer := TJSHTMLElement(document.createElement('div'));
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

    if (ADocument.Count > 1) and not FViewOptions.IsFocused then
    begin
      FNavigation := NewNyxPageNavigation(ADocument);
      NyxPageNavigationState(FNavigation).Configure(FNavigationOptions);
      NyxPageNavigationState(FNavigation).SelectPage(NyxControl(ADocument.Pages[0].ID));
      LHost := TJSHTMLElement(document.createElement('nav'));
      LHost.setAttribute('aria-label', 'Application pages');
      LContainer.appendChild(LHost);
      FNavigator.Render(FNavigation.Document, FNavigation.Root.Node, LHost);
    end;
    FViewHost := TJSHTMLElement(document.createElement('main'));
    LContainer.appendChild(FViewHost);
    FRenderer.Render(FDocument, LRoot, FViewHost, False, FRuntime.State,
      FViewOptions.InitialCollections(FDocument, FRuntime));
    FMenus := BindNyxBrowserMenus(FDocument, FRenderer);
    FViewHost.setAttribute('data-nyx-page', LRoot.ID);
    SynchronizeNavigation(NyxControl(LRoot.ID));
    { Both view trees and subscriptions are admitted before replacing host content. }
    AHost.textContent := '';
    while LContainer.firstChild <> nil do
    begin
      AHost.appendChild(LContainer.firstChild);
    end;
    document.title := FDocument.Title;
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
    FDocument := nil;
    FViewHost := nil;
    raise;
  end;
end;

function TNyxBrowserApplication.GetGeneration: INyxCancellationScope;
begin

  if FViewHost = nil then
  begin
    raise ENyxApplicationContext.Create('View generation requires a mounted application');
  end;
  Result := NyxEventGeneration(FRenderer.Events);
end;

function TNyxBrowserApplication.ConfigureView(const AView: TNyxApplicationView): TNyxBrowserApplication;
begin

  if FDocument <> nil then
  begin
    raise ENyxModel.Create('Configure the application view before mounting');
  end;
  FViewOptions := AView;
  Result := Self;
end;

function TNyxBrowserApplication.ConfigureLogic(const AResolver: INyxLogicResolver): TNyxBrowserApplication;
begin

  if FDocument <> nil then
  begin
    raise ENyxModel.Create('Configure Logic authority before mounting the application');
  end;
  FFlowResolver := AResolver;
  Result := Self;
end;

function TNyxBrowserApplication.ConfigureFileSaving(const AHost: INyxFileSaveHost): TNyxBrowserApplication;
begin

  if FDocument <> nil then
  begin
    raise ENyxModel.Create('Configure file-save authority before mounting the application');
  end;
  FFileSaveHost := AHost;
  Result := Self;
end;

function TNyxBrowserApplication.ConfigureFileOpening(const AHost: INyxFileOpenHost): TNyxBrowserApplication;
begin

  if FDocument <> nil then
  begin
    raise ENyxModel.Create('Configure file-open authority before mounting the application');
  end;
  FFileOpenHost := AHost;
  Result := Self;
end;

function TNyxBrowserApplication.GetContext: INyxApplicationContext;
begin

  if FContextLease = nil then
  begin
    raise ENyxApplicationContext.Create('Application context requires a mounted application');
  end;
  Result := FContextLease.Context;
end;

function TNyxBrowserApplication.GetState: TNyxState;
begin

  if FRuntime = nil then
  begin
    raise ENyxState.Create('Application state requires a mounted application');
  end;
  Result := FRuntime.State;
end;

function TNyxBrowserApplication.ConfigureResources(const AOptions: TNyxApplicationResourceOptions;
  const AResolver: INyxResourceResolver): TNyxBrowserApplication;
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

procedure TNyxBrowserApplication.PrepareResources(ADocument: TNyxDocument);
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
        LResolver := NewNyxResourceResolver(NewNyxBrowserResourceTransport,
          nil, NewNyxBrowserResourceCache);
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

function TNyxBrowserApplication.GetResources: INyxApplicationResources;
begin

  if FResources = nil then
  begin
    raise ENyxResource.Create('Application resources require a mounted application');
  end;
  Result := FResources;
end;

function TNyxBrowserApplication.ValidateResources(const AContext: INyxResourceContext): Boolean;
begin
  Result := FRenderer.CanReloadResources(AContext);
end;

function TNyxBrowserApplication.PrepareResourcePublication(const AContext: INyxResourceContext;
  out APrepared: INyxPreparedPublication): Boolean;
begin
  { Runtime preparation owns shared/hidden/instance rows. This mounted receiver
    contributes scalar properties only, against the same held state snapshot. }
  APrepared := FRenderer.PrepareResourcePublication(AContext);
  Result := True;
end;

function TNyxBrowserApplication.GetCollections: INyxCollections;
begin

  if FRuntime = nil then
  begin
    raise ENyxState.Create('Runtime collections require a mounted application');
  end;
  Result := FRuntime.Collections;
end;

procedure TNyxBrowserApplication.ShowPage(const AID: TNyxText);
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
      FRenderer.Render(FDocument, FDocument.Pages[LIndex], FViewHost, False, FRuntime.State,
        FRuntime.PageCollections(AID));
      FMenus := BindNyxBrowserMenus(FDocument, FRenderer);
      SynchronizeNavigation(NyxControl(AID));
      FViewHost.setAttribute('data-nyx-page', AID);
      document.title := FDocument.Title;
      Exit;
    end;
  end;
  raise ENyxModel.Create('Application page not found: ' + AID);
end;

procedure TNyxBrowserApplication.Navigate(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LPage: TNyxControlRef;
begin

  if (FNavigation <> nil) and FNavigation.Resolve(AEvent, LPage) then
  begin
    ShowPage(LPage.ID);
  end;
end;

function TNyxBrowserApplication.ConfigureNavigation(
  const AOptions: TNyxSelectionOptions): TNyxBrowserApplication;
begin

  if FDocument <> nil then
  begin
    raise ENyxModel.Create('Configure navigation before mounting');
  end;
  AOptions.Validate;
  FNavigationOptions := AOptions;
  Result := Self;
end;

function TNyxBrowserApplication.GetNavigationState: INyxPageNavigationState;
begin
  Result := nil;

  if FNavigation <> nil then
  begin
    Result := NyxPageNavigationState(FNavigation);
  end;
end;

procedure TNyxBrowserApplication.SynchronizeNavigation(const APage: TNyxControlRef);
var
  LState: INyxPageNavigationState;
  LIndex: Integer;
  LButton: TJSHTMLElement;
begin

  if FNavigation = nil then
  begin
    Exit;
  end;
  LState := NyxPageNavigationState(FNavigation);
  LState.SelectPage(APage);

  { This is a runtime selection inside the notifying navigation view, not a
    document refresh. The ordinary refresh API correctly refuses an active
    command. Project only our typed presentation into the admitted runtime tree. }
  LState.Project(FNavigator.Root);
  FNavigator.Sync;
  for LIndex := 0 to FNavigation.Root.Node.Count - 1 do
  begin
    LButton := FNavigator.ElementFor(NyxPageNavigationButtonID(LIndex));

    if LState.IsCurrent(NyxControl(NyxPageNavigationButtonID(LIndex))) then
    begin
      LButton.setAttribute('aria-current', 'page');
    end
    else
    begin
      LButton.removeAttribute('aria-current');
    end;
  end;
end;

end.
