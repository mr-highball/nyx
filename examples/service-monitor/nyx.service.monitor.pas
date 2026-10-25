{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
unit nyx.service.monitor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.collections.export;

const
  ServiceCollectionName: TNyxText = 'services';
  ServiceTableID: TNyxText = 'service-table';

{ Returns an owned portable document. Its sample records are authored defaults;
  each application receives independent runtime data, selection and preferences.
  The caller frees the document after its application has retired. }
function BuildServiceMonitorDocument: TNyxDocument;

{ Runtime file authority remains separate from saved table intent. Both result
  scopes are explicit, bounded by the ordinary export defaults. No file opens
  until a user invokes a managed table command. }
function ServiceMonitorExportOptions: TNyxCollectionExportOptions;

implementation

uses nyx.types, nyx.controls, nyx.contract, nyx.state, nyx.collections,
  nyx.collections.view.types, nyx.collections.columns,
  nyx.collections.arrangement, nyx.collections.presentation,
  nyx.collections.workbench.policy, nyx.files.save;

function ServiceMonitorExportOptions: TNyxCollectionExportOptions;
begin
  Result := TNyxCollectionExportOptions.Defaults
    .AllowRows([ncerCurrentPage, ncerAllFiltered])
    .SaveAs(NyxFileSaveOptions('services.tsv').Title('Export service records'));
end;

function BuildServiceMonitorDocument: TNyxDocument;
var
  LPage: INyxPage;
  LTitle: INyxHeading;
  LIntroduction: INyxLabel;
  LServices: INyxTable;
  LHelp: INyxLabel;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Service monitor';
    Result.Collections.Define(NyxCollection(ServiceCollectionName),
      NyxCollectionSchema
        .Text(NyxTextField('name'), '', NyxTextDomain)
        .Integer(NyxIntegerField('pid'), 0, NyxIntegerDomain)
        .Text(NyxTextField('command'), '', NyxTextDomain)
        .Text(NyxTextField('notes'), '', NyxTextDomain)
        .Text(NyxTextField('status'), '', NyxTextDomain),
      [
        NyxCollectionItem(NyxItem(NyxCollection(ServiceCollectionName), 'gateway'))
          .WithValue(NyxTextField('name'), 'Request gateway')
          .WithValue(NyxIntegerField('pid'), 2401)
          .WithValue(NyxTextField('command'), '/opt/example/bin/gateway --listen=127.0.0.1 --config=/opt/example/config/gateway.json')
          .WithValue(NyxTextField('notes'), 'Accepts incoming requests and routes them to healthy workers.')
          .WithValue(NyxTextField('status'), 'Running'),
        NyxCollectionItem(NyxItem(NyxCollection(ServiceCollectionName), 'indexer'))
          .WithValue(NyxTextField('name'), 'Search index worker')
          .WithValue(NyxIntegerField('pid'), 2408)
          .WithValue(NyxTextField('command'), '/opt/example/bin/indexer --collection=documents --batch-size=128')
          .WithValue(NyxTextField('notes'), 'Waiting for new documents. Edit this note to try draft handling.')
          .WithValue(NyxTextField('status'), 'Idle'),
        NyxCollectionItem(NyxItem(NyxCollection(ServiceCollectionName), 'archive'))
          .WithValue(NyxTextField('name'), 'Archive and retention worker')
          .WithValue(NyxIntegerField('pid'), 2420)
          .WithValue(NyxTextField('command'), '/opt/example/bin/archive --destination=/var/example/archive --retention-days=30')
          .WithValue(NyxTextField('notes'), 'The destination is temporarily unavailable. Full-value inspection keeps this explanation readable without narrowing the entire workspace.')
          .WithValue(NyxTextField('status'), 'Attention'),
        NyxCollectionItem(NyxItem(NyxCollection(ServiceCollectionName), 'reports'))
          .WithValue(NyxTextField('name'), 'Report scheduler')
          .WithValue(NyxIntegerField('pid'), 2427)
          .WithValue(NyxTextField('command'), '/opt/example/bin/reports --schedule=hourly')
          .WithValue(NyxTextField('notes'), 'The next report is ready to run.')
          .WithValue(NyxTextField('status'), 'Running')
      ]);

    LPage := NewNyxPage('services');
    Result.AddPage(LPage);
    LPage.Configure.Layout(nlColumn).Padding(16).Gap(16).Done;

    LTitle := NewNyxHeading('service-title');
    LPage.Add(LTitle);
    LTitle.WithText('A clearer view of your services');

    LIntroduction := NewNyxLabel('service-introduction');
    LPage.Add(LIntroduction);
    LIntroduction.WithText('Sample services, real table controls. Filter a column, arrange your workspace, or edit a name, note or status.');

    LServices := NewNyxTable(ServiceTableID);
    LPage.Add(LServices);
    LServices.Configure.Height(480).AccessibleName('Service records').Done;
    LServices.Binds.Collection(NyxCollectionView(NyxCollection(ServiceCollectionName))
      .Column(NyxTextField('name'), 'Service', cmEditable)
        .ColumnSizing(NyxColumnSizing.Fixed(144).Minimum(112).Maximum(420))
        .ColumnText(NyxColumnText.Ellipsis)
        .ColumnLayout(NyxColumnLayout.PinStart)
      .Column(NyxIntegerField('pid'), 'Process ID')
        .ColumnSizing(NyxColumnSizing.Fixed(104).Minimum(80).Maximum(180))
      .Column(NyxTextField('command'), 'Command line')
        .ColumnSizing(NyxColumnSizing.Fixed(360).Minimum(160).Maximum(800))
        .ColumnText(NyxColumnText.Ellipsis)
      .Column(NyxTextField('notes'), 'Notes', cmEditable)
        .ColumnSizing(NyxColumnSizing.Fixed(320).Minimum(160).Maximum(800))
        .ColumnText(NyxColumnText.Wrap(3))
      .Column(NyxTextField('status'), 'Status', cmEditable)
        .ColumnSizing(NyxColumnSizing.Fixed(96).Minimum(80).Maximum(240))
        .ColumnLayout(NyxColumnLayout.PinEnd)
      .TableLayout(NyxTableLayout.MinimumCenter(80).AutoSizeSample(32))
      .TablePresentation(NyxTablePresentation.Density(ntrdComfortable)
        .Header(NyxTableHeader.Lines(1).MinimumHeight(32)))
      .TableWorkbench(NyxTableWorkbench.RangeSelection(True)
        .AllowCopy([nwcaCell, nwcaSelectedSource, nwcaSelectedPage, nwcaCellRange])
        .Export(NyxTableExport.Allow([nweaCurrentPage, nweaAllFiltered]))))
      .Done;

    LHelp := NewNyxLabel('service-help');
    LPage.Add(LHelp);
    LHelp.WithText('Use column controls to resize, reorder, hide, pin or restore defaults. Service and Status start pinned at opposite edges. Cell actions offer inspection, copying and TSV export.');
  except
    Result.Free;
    raise;
  end;
end;

end.
