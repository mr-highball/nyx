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

  { The native counterpart of the browser application host. The document is
    borrowed; generated entry points release this host before the document. }
  TNyxLCLApplication = class
  private
    FForm: TNyxApplicationForm;
    FDocument: TNyxDocument;
    FNavigation: TNyxDocument;
    FNavigator: TNyxLCLRenderer;
    FRenderer: TNyxLCLRenderer;
    FViewHost: TPanel;
    FRuntime: TNyxApplicationState;
    FMenus: INyxMenuBindings;
    FResources: INyxApplicationResources;
    FResourceOptions: TNyxApplicationResourceOptions;
    FResourceResolver: INyxResourceResolver;
    FResourceSubscription: INyxResourceSubscription;
    function GetState: TNyxState;
    function GetCollections: INyxCollections;
    function GetResources: INyxApplicationResources;
    procedure PrepareResources(ADocument: TNyxDocument);
    function ValidateResources(const AContext: INyxResourceContext): Boolean;
    procedure ResourcesChanged(const AContext: INyxResourceContext);
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
    procedure ShowPage(const AID: TNyxText);
    property State: TNyxState read GetState;
    { Independent runtime collection stores persist across ShowPage. }
    property Collections: INyxCollections read GetCollections;
    { Private application catalog/load diagnostics survive navigation. Configure
      before mounting; supplying a resolver replaces the target HTTP/cache path.
      Stop retires requests before controls and borrowed callbacks are freed. }
    function ConfigureResources(const AOptions: TNyxApplicationResourceOptions;
      const AResolver: INyxResourceResolver = nil): TNyxLCLApplication;
    property Resources: INyxApplicationResources read GetResources;
    property View: TNyxLCLRenderer read FRenderer;
    { Retained observations must retire before page changes/window disposal. }
    property Menus: INyxMenuBindings read FMenus;
    property Window: TNyxApplicationForm read FForm;
  end;

implementation

uses
  nyx.callbacks,
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
end;

destructor TNyxLCLApplication.Destroy;
begin

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
  FNavigation.Free;
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
  LBar: TNyxNode;
  LHost: TPanel;
  LIndex: Integer;
  LButton: TNyxNode;
begin

  if (ADocument = nil) or (ADocument.Count = 0) then
  begin
    raise ENyxModel.Create('An application needs at least one page');
  end;

  if FDocument <> nil then
  begin
    raise ENyxModel.Create('Native application can only be mounted once');
  end;
  BindNyxCallbacks(ADocument, FRenderer.Events);
  FRuntime := TNyxApplicationState.Create(ADocument, npfNativeLCL);
  FDocument := ADocument;
  try
    PrepareResources(ADocument);
    Application.CreateForm(TNyxApplicationForm, FForm);
    FForm.Caption := ADocument.Title;
    FForm.SetBounds(100, 100, 840, 720);

    if ADocument.Count > 1 then
    begin
      FNavigation := TNyxDocument.Create;
      LBar := TNyxNode.Create(nkToolbar, 'app-navigation');
      FNavigation.AddPage(LBar);
      LBar.Configure.Layout(nlRow).Padding(12).Done;
      for LIndex := 0 to ADocument.Count - 1 do
      begin
        LButton := TNyxNode.Create(nkButton, 'nav-' + IntToStr(LIndex));
        LBar.Add(LButton);
        LButton.Configure.Text(ADocument.Pages[LIndex].ID)
          .Extension('page-id', ADocument.Pages[LIndex].ID).Done;
      end;
      LHost := TPanel.Create(FForm);
      LHost.Parent := FForm;
      LHost.Align := alTop;
      LHost.Height := 68;
      LHost.BevelOuter := bvNone;
      FNavigator.Render(FNavigation, LBar, LHost);
    end;
    FViewHost := TPanel.Create(FForm);
    FViewHost.Parent := FForm;
    FViewHost.Align := alClient;
    FViewHost.BevelOuter := bvNone;
    ShowPage(ADocument.Pages[0].ID);
    FResources.Start;
  except

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
    FreeAndNil(FNavigation);
    FreeAndNil(FRuntime);
    FreeAndNil(FForm);
    FDocument := nil;
    FViewHost := nil;
    raise;
  end;
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
  FResourceSubscription := FResources.Subscribe(ValidateResources, ResourcesChanged);
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

procedure TNyxLCLApplication.ResourcesChanged(const AContext: INyxResourceContext);
begin
  FRenderer.ReloadResources(AContext.Snapshot, AContext.Locale, AContext.Fallback);
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
      Exit;
    end;
  end;
  raise ENyxModel.Create('Application page not found: ' + AID);
end;

procedure TNyxLCLApplication.Navigate(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (AEvent.Trigger = ntClick) and (ANode.Prop('page-id') <> '') then
  begin
    ShowPage(ANode.Prop('page-id'));
  end;
end;

end.
