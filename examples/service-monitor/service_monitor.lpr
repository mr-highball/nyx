{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
program service_monitor;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}
  Web, nyx.application.browser, nyx.clipboard.browser, nyx.files.save.browser,
  {$else}
  Interfaces, Forms, nyx.application.lcl, nyx.clipboard.lcl, nyx.files.save.lcl,
  {$endif}
  nyx.model, nyx.service.monitor, nyx.collections.workbench.runtime;

var
  GDocument: TNyxDocument;
  {$ifdef PAS2JS}
  GApplication: TNyxBrowserApplication;
  {$else}
  GApplication: TNyxLCLApplication;
  {$endif}

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  GDocument := BuildServiceMonitorDocument;
  try
    {$ifdef PAS2JS}
    GApplication := TNyxBrowserApplication.Create;
    GApplication.View.TableWorkbenchServices := NewNyxTableWorkbenchServices(
      NewNyxBrowserClipboardWriteHost, NewNyxBrowserFileSaveHost,
      ServiceMonitorExportOptions);
    GApplication.Run(GDocument, TJSHTMLElement(document.body));
    document.body.setAttribute('data-nyx-service-monitor-ready', 'passed');
    { Page lifetime retains the application before its borrowed document.
      Embedded hosts explicitly retire the application before freeing it. }
    {$else}
    GApplication := TNyxLCLApplication.Create;
    try
      GApplication.View.TableWorkbenchServices := NewNyxTableWorkbenchServices(
        NewNyxLCLClipboardWriteHost, NewNyxLCLFileSaveHost,
        ServiceMonitorExportOptions);
      GApplication.Run(GDocument);
    finally
      GApplication.Free;
    end;
    {$endif}
  except
    {$ifdef PAS2JS}GApplication.Free;{$endif}
    GDocument.Free;
    raise;
  end;
  {$ifndef PAS2JS}GDocument.Free;{$endif}
end.
