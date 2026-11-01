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
program resource_workshop;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.model, nyx.resource.workshop, nyx.application.resources,
  nyx.logic.http,
  {$ifdef PAS2JS}
  Web, nyx.application.browser, nyx.resources.http.browser;
  {$else}
  Interfaces, Forms, nyx.application.lcl, nyx.resources.http.lcl;
  {$endif}

type
  {$ifdef PAS2JS}
  { The page owns one application and its borrowed portable blueprint. }
  TWorkshopApplication = TNyxBrowserApplication;
  {$else}
  { The native message loop hosts the identical document and resource graph. }
  TWorkshopApplication = TNyxLCLApplication;
  {$endif}

var
  GDocument: TNyxDocument;
  GApplication: TWorkshopApplication;

{$ifdef PAS2JS}
function Retire(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    FreeAndNil(GApplication);
  finally
    FreeAndNil(GDocument);
  end;
end;

function Restore(AEvent: TJSEvent): Boolean;
begin
  Result := False;

  if GApplication <> nil then
  begin
    Exit;
  end;
  GDocument := BuildResourceWorkshopDocument;
  GApplication := TWorkshopApplication.Create;
  try
    GApplication.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand));
    GApplication.ConfigureLogic(NewNyxHTTPLogicResolver(NewNyxBrowserHTTPTransport));
    GApplication.Run(GDocument, TJSHTMLElement(document.body));
  except
    FreeAndNil(GApplication);
    FreeAndNil(GDocument);
    raise;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}
  Restore(nil);
  window.addEventListener('pagehide', @Retire);
  window.addEventListener('pageshow', @Restore);
  {$else}
  Application.Initialize;
  GDocument := BuildResourceWorkshopDocument;
  GApplication := TWorkshopApplication.Create;
  try
    GApplication.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand));
    GApplication.ConfigureLogic(NewNyxHTTPLogicResolver(
      NewNyxNativeHTTPTransport(GApplication.View.Events.Scheduler)));
    GApplication.Mount(GDocument);
    GApplication.Window.ClientWidth := 1200;
    GApplication.Window.ClientHeight := 880;
    GApplication.Window.Show;
    Application.Run;
  finally
    FreeAndNil(GApplication);
    FreeAndNil(GDocument);
  end;
  {$endif}
end.
