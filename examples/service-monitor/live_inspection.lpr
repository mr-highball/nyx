{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
program live_inspection;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}
  Web, nyx.application.browser, nyx.clipboard.browser,
  {$else}
  Interfaces, Forms, nyx.application.lcl, nyx.clipboard.lcl,
  {$endif}
  nyx.model, nyx.service.monitor, nyx.live.inspection, nyx.collections.workbench.runtime,
  nyx.collections.position;

var
  GDocument: TNyxDocument;
  GInspection: TNyxLiveInspectionDemo;
  {$ifdef PAS2JS}
  GApplication: TNyxBrowserApplication;
  {$else}
  GApplication: TNyxLCLApplication;
  {$endif}

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  GDocument := BuildLiveInspectionDocument;
  try
    {$ifdef PAS2JS}
    GApplication := TNyxBrowserApplication.Create;
    GApplication.View.TableWorkbenchServices := NewNyxTableWorkbenchServices(NewNyxBrowserClipboardWriteHost);
    GApplication.Run(GDocument, TJSHTMLElement(document.body));
    {$else}
    GApplication := TNyxLCLApplication.Create;
    try
      GApplication.View.TableWorkbenchServices := NewNyxTableWorkbenchServices(NewNyxLCLClipboardWriteHost);
      GApplication.Mount(GDocument);
    {$endif}
    GInspection := TNyxLiveInspectionDemo.Create(GApplication.Context, GApplication.View.Events,
      GApplication.View.CollectionMount(ServiceTableID).View,
      GApplication.View.CollectionMount(InspectionPeerID).View,
      NyxCollectionPositionState(GApplication.View.CollectionMount(ServiceTableID)));
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-live-inspection-ready', 'passed');
    { The page owns this demo for its lifetime. Embedded hosts call Close/Free
      before retiring their application and its borrowed document. }
    {$else}
      GApplication.Window.Show;
      Application.Run;
    finally
      GInspection.Free;
      GApplication.Free;
    end;
    {$endif}
  except
    {$ifdef PAS2JS}
    GInspection.Free;
    GApplication.Free;
    {$endif}
    GDocument.Free;
    raise;
  end;
  {$ifndef PAS2JS}GDocument.Free;{$endif}
end.
