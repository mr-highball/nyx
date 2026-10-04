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
  SysUtils,
  Web,
  nyx.model,
  nyx.types,
  nyx.state,
  nyx.collections.registry,
  nyx.application.state,
  nyx.render.browser;

type
  { Small application host for generated and handwritten documents. Pages are
    independently mounted into one view host. Navigation is itself a Nyx toolbar,
    keeping application builds distinct from a single-view compilation harness.
    The supplied document is borrowed and must outlive this application. }
  TNyxBrowserApplication = class
  private
    FDocument: TNyxDocument;
    FNavigation: TNyxDocument;
    FNavigator: TNyxBrowserRenderer;
    FRenderer: TNyxBrowserRenderer;
    FViewHost: TJSHTMLElement;
    FRuntime: TNyxApplicationState;
    function GetState: TNyxState;
    function GetCollections: INyxCollections;
    procedure Navigate(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run(ADocument: TNyxDocument; AHost: TJSHTMLElement);
    procedure ShowPage(const AID: TNyxText);
    { Runtime state survives page changes. The owned renderer is borrowed for
      factory registration, event/error callbacks and identity lookup. Never free
      it. Run mounts once; destroy the application before mutating its document. }
    property State: TNyxState read GetState;
    { Independent runtime collection stores persist across ShowPage. }
    property Collections: INyxCollections read GetCollections;
    property View: TNyxBrowserRenderer read FRenderer;
  end;

implementation

uses
  nyx.callbacks;

constructor TNyxBrowserApplication.Create;
begin
  inherited Create;
  FRenderer := TNyxBrowserRenderer.Create;
  FNavigator := TNyxBrowserRenderer.Create;
  FNavigator.OnEvent := Navigate;
end;

destructor TNyxBrowserApplication.Destroy;
begin
  FRenderer.Free;
  FNavigator.Free;
  FRuntime.Free;
  FNavigation.Free;
  inherited Destroy;
end;

procedure TNyxBrowserApplication.Run(ADocument: TNyxDocument; AHost: TJSHTMLElement);
var
  LBar: TNyxNode;
  LHost: TJSHTMLElement;
  LIndex: Integer;
  LContainer: TJSHTMLElement;
  LButton: TNyxNode;
begin

  if (ADocument = nil) or (ADocument.Count = 0) then
  begin
    raise ENyxModel.Create('An application needs at least one page');
  end;

  if (AHost = nil) or (FDocument <> nil) then
  begin
    raise ENyxModel.Create('Application requires a host and can only be mounted once');
  end;
  BindNyxCallbacks(ADocument, FRenderer.Events);
  FRuntime := TNyxApplicationState.Create(ADocument);
  FDocument := ADocument;
  LContainer := TJSHTMLElement(document.createElement('div'));
  try

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
      LHost := TJSHTMLElement(document.createElement('nav'));
      LHost.setAttribute('aria-label', 'Application pages');
      LContainer.appendChild(LHost);
      FNavigator.Render(FNavigation, LBar, LHost);
    end;
    FViewHost := TJSHTMLElement(document.createElement('main'));
    LContainer.appendChild(FViewHost);
    FRenderer.Render(FDocument, FDocument.Pages[0], FViewHost, False, FRuntime.State,
      FRuntime.PageCollections(FDocument.Pages[0].ID));
    FViewHost.setAttribute('data-nyx-page', FDocument.Pages[0].ID);
    { Both view trees and subscriptions are admitted before replacing host content. }
    AHost.textContent := '';
    while LContainer.firstChild <> nil do
    begin
      AHost.appendChild(LContainer.firstChild);
    end;
    document.title := FDocument.Title;
  except
    FRenderer.Unmount;
    FNavigator.Unmount;
    FreeAndNil(FNavigation);
    FreeAndNil(FRuntime);
    FDocument := nil;
    FViewHost := nil;
    raise;
  end;
end;

function TNyxBrowserApplication.GetState: TNyxState;
begin

  if FRuntime = nil then
  begin
    raise ENyxState.Create('Application state requires a mounted application');
  end;
  Result := FRuntime.State;
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

  if FDocument = nil then
  begin
    raise ENyxModel.Create('Application must be mounted before navigating');
  end;
  for LIndex := 0 to FDocument.Count - 1 do
  begin

    if FDocument.Pages[LIndex].ID = AID then
    begin
      FRenderer.Render(FDocument, FDocument.Pages[LIndex], FViewHost, False, FRuntime.State,
        FRuntime.PageCollections(AID));
      FViewHost.setAttribute('data-nyx-page', AID);
      document.title := FDocument.Title;
      Exit;
    end;
  end;
  raise ENyxModel.Create('Application page not found: ' + AID);
end;

procedure TNyxBrowserApplication.Navigate(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (AEvent.Trigger = ntClick) and (ANode.Prop('page-id') <> '') then
  begin
    ShowPage(ANode.Prop('page-id'));
  end;
end;

end.
