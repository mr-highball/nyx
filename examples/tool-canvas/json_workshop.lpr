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
program json_workshop;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.model, nyx.json.workshop,
  {$ifdef PAS2JS}
  Web, nyx.application.browser, nyx.files.save.browser, nyx.files.save.activation.browser;
  {$else}
  Interfaces, Forms, nyx.application.lcl, nyx.files.save.lcl;
  {$endif}

type
  {$ifdef PAS2JS}
  { Browser page owns the app before its borrowed source document. }
  TWorkshopApplication = TNyxBrowserApplication;
  {$else}
  { Native message loop hosts the same portable document and logic. }
  TWorkshopApplication = TNyxLCLApplication;
  {$endif}

var
  GDocument: TNyxDocument;
  GApplication: TWorkshopApplication;

{$ifdef PAS2JS}
function Retire(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  { Revoke pending graph/file work before releasing its borrowed blueprint. }
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
  { Back/forward cache can restore the page after pagehide retired its app.
    Rebuild a fresh runtime rather than leaving disconnected controls visible. }
  GDocument := BuildJSONWorkshopDocument;
  GApplication := TWorkshopApplication.Create;
  try
    GApplication.ConfigureFileSaving(NewNyxBrowserActivatedFileSaveHost(NewNyxBrowserFileSaveHost));
    GApplication.Run(GDocument, TJSHTMLElement(document.body));
    ShowJSONWorkshopGraph(GApplication.View.ToolCanvasView(JSONWorkshopCanvas.ID));
    document.body.setAttribute('data-json-workshop-ready', 'passed');
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
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  GDocument := BuildJSONWorkshopDocument;
  GApplication := TWorkshopApplication.Create;
  try
    GApplication.ConfigureFileSaving(NewNyxLCLFileSaveHost);
    GApplication.Mount(GDocument);
    GApplication.Window.ClientWidth := 1200;
    GApplication.Window.ClientHeight := 880;
    ShowJSONWorkshopGraph(GApplication.View.ToolCanvasView(JSONWorkshopCanvas.ID));
    GApplication.Window.Show;
    Application.Run;
  except
    FreeAndNil(GApplication);
    FreeAndNil(GDocument);
    raise;
  end;
  FreeAndNil(GApplication);
  FreeAndNil(GDocument);
  {$endif}
end.
