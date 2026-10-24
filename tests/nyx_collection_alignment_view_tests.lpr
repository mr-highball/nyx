{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
program nyx_collection_alignment_view_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.state, nyx.collections,
  nyx.collections.arrangement, nyx.collections.alignment,
  nyx.collections.presentation, nyx.collections.presentation.controls,
  nyx.collections.view.types, nyx.collections.view, nyx.publication
  {$ifdef PAS2JS}, Web{$endif};

const
  CExact: TNyxText = '服务🌙' + #0 + #9 + 'é' + #13#10 + 'tail';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

{ Bound fluent self-return temporaries to this entered procedure, so detached
  captures in the caller do not accidentally lease a live view on pas2js. }
procedure Configure(const AView: INyxCollectionView;
  const APreferences: TNyxTablePresentationPreferences);
var
  LPresentation: INyxCollectionTablePresentation;
begin
  LPresentation := NyxCollectionTablePresentation(AView);
  LPresentation.ConfigurePresentation(APreferences);
end;

procedure Run;
var
  LKey: TNyxCollectionRef;
  LField: TNyxColumnIdentity;
  LBase: TNyxCollectionViewSpec;
  LSpec: TNyxCollectionViewSpec;
  LPeerSpec: TNyxCollectionViewSpec;
  LDecoded: TNyxCollectionViewSpec;
  LWire: TNyxText;
  LRuntimeWire: TNyxText;
  LBefore: TNyxText;
  LPreferences: TNyxTablePresentationPreferences;
  LCopy: TNyxTablePresentationPreferences;
  LObserver: TNyxTablePresentationObservation;
  LRetained: TNyxTablePresentationObservation;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LPeer: INyxCollectionView;
  LNext: INyxCollectionView;
  LPresentation: INyxCollectionTablePresentation;
  LCapture: INyxCollectionPresentationCapture;
  LColumns: INyxCollectionColumns;
  LPrepared: INyxPreparedPublication;
  LOther: INyxPreparedPublication;
  LSource: INyxCollectionSnapshot;
  LItem: TNyxItemRef;
  LRefused: Boolean;
begin
  LKey := NyxCollection('alignment 🌙');
  LField := NyxColumnIdentity(NyxTextField('Name'));
  LBase := NyxCollectionView(LKey).Column(NyxTextField('Name'), CExact, cmEditable);
  LWire := LBase.ToData.ToJSON;
  Check((LBase.ToData.Field('version').AsInteger = 1) and not LBase.HasColumnAlignment,
    'absent alignment retains the original version-one descriptor');
  LSpec := LBase.ColumnAlignment(NyxColumnAlignment.Horizontal(nchaEnd).Vertical(ncvaTop));
  LPeerSpec := LSpec.Copy;
  LSpec := LSpec.ColumnAlignment(NyxColumnAlignment.Vertical(ncvaBottom));
  Check(LPeerSpec.ColumnAt(0).HasAlignmentPolicy and
    (LPeerSpec.ColumnAt(0).AlignmentPolicy.HorizontalAlignment = nchaEnd) and
    not LSpec.ColumnAt(0).AlignmentPolicy.HasHorizontal and not LBase.HasPresentationPolicy,
    'column fluent alignment copies complete presence independently of prior descriptors');
  LDecoded := TNyxCollectionViewSpec.FromData(LPeerSpec.ToData);
  Check((LDecoded.ToData.ToJSON = LPeerSpec.ToData.ToJSON) and
    (LDecoded.ToData.Field('version').AsInteger = 12) and
    (LDecoded.ToData.Count = 13) and
    (LDecoded.ToData.Field('columns').Item(0).Count = 10) and
    (LDecoded.ColumnAt(0).Title = CExact),
    'descriptor twelve has complete nullable columns and preserves exact Unicode control captions');
  LDecoded := LDecoded.UseDefaultColumnAlignment;
  Check(not LDecoded.HasPresentationPolicy and (LDecoded.ToData.ToJSON = LWire),
    'removing the only alignment restores exact older descriptor bytes');
  LBase := LBase.ColumnText(NyxColumnText.Wrap(3)).TablePresentation(NyxTablePresentation);
  LBefore := LBase.ToData.ToJSON;
  LDecoded := LBase.ColumnAlignment(NyxColumnAlignment.Horizontal(nchaCenter))
    .UseDefaultColumnAlignment;
  Check((LBase.ToData.Field('version').AsInteger = 11) and
    (LDecoded.ToData.ToJSON = LBefore),
    'alignment set reset restores exact version-eleven text density wire');
  LRefused := False;
  try
    ValidateNyxCollectionViewProjection(LPeerSpec, cpList);
  except
    on ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'logical alignment is a table-only policy at actual projection admission');

  LPreferences := NyxTablePresentationPreferences.Density(ntrdCompact)
    .WithColumn(NyxColumnTextPreference(LField, NyxColumnText.Ellipsis));
  LRuntimeWire := LPreferences.ToData.ToJSON;
  Check(LPreferences.ToData.Field('version').AsInteger = 1,
    'alignment-free runtime preferences preserve their version-one wire');
  LPreferences := LPreferences.WithAlignment(NyxColumnAlignmentPreference(LField,
    NyxColumnAlignment.Vertical(ncvaBottom)));
  LCopy := LPreferences.Copy;
  LPreferences := LPreferences.WithAlignment(NyxColumnAlignmentPreference(LField,
    NyxColumnAlignment.Horizontal(nchaCenter)));
  Check((LCopy.AlignmentAt(0).Alignment.VerticalAlignment = ncvaBottom) and
    not LCopy.AlignmentAt(0).Alignment.HasHorizontal,
    'runtime preference vector copy detaches independent axis values');
  LPreferences := TNyxTablePresentationPreferences.FromData(LCopy.ToData);
  LPreferences.ValidateFields([LField]);
  Check((LPreferences.ToData.Field('version').AsInteger = 2) and
    (LPreferences.ToData.ToJSON = LCopy.ToData.ToJSON) and
    (LPreferences.ResolveAlignment(LField, LPeerSpec.ColumnAt(0).AlignmentPolicy)
      .HorizontalAlignment = nchaEnd) and
    (LPreferences.ResolveAlignment(LField, LPeerSpec.ColumnAt(0).AlignmentPolicy)
      .VerticalAlignment = ncvaBottom),
    'runtime version two replays independent overlays over complete authored axes');
  LPreferences := LPreferences.WithoutAlignment(LField);
  Check((LPreferences.ToData.ToJSON = LRuntimeWire) and (LPreferences.Count = 1) and
    LPreferences.HasDensity, 'alignment reset preserves exact legacy runtime density and text wire');
  LBefore := LCopy.ToData.ToJSON;
  LRefused := False;
  try
    LCopy := LCopy.WithColumn(NyxColumnTextPreference(
      NyxColumnIdentity(NyxNumberField('Name')), NyxColumnText));
  except
    on EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LCopy.ToData.ToJSON = LBefore),
    'cross text alignment vectors cannot silently change a shared field family');

  try
    LItem := NyxItem(LKey, 'one');
    LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField('Name'), ''),
      [NyxCollectionItem(LItem).WithValue(NyxTextField('Name'), CExact)]);
    LView := NewNyxCollectionView(LStore, LPeerSpec, cpTable);
    LPeer := NewNyxCollectionView(LStore, LPeerSpec, cpTable);
    LPresentation := NyxCollectionTablePresentation(LView);
    LColumns := NyxCollectionColumns(LView);
    LSource := LStore.Snapshot;
    LCapture := LPresentation.CapturePresentation;
    LObserver := TNyxTablePresentationObservation.Read(LColumns.CaptureColumns);
    LRetained := LObserver.Copy;
    LPrepared := LPresentation.PreparePresentation(LCapture, LCopy);
    LRefused := False;
    try
      LOther := LColumns.PrepareColumns(LColumns.CaptureColumns, NyxColumnPreferences);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'alignment and width commands reserve the same original publication slot');
    LPrepared.Validate;
    LPrepared.Retire;
    LPrepared := nil;
    Check(LPresentation.CapturePresentation.Serial = LCapture.Serial,
      'cancelled alignment preparation publishes no serial or physical authority');
    Configure(LView, LCopy);
    LObserver := TNyxTablePresentationObservation.Read(LColumns.CaptureColumns);
    Check(LObserver.HasRuntimeOverrides and (LObserver.Serial = LCapture.Serial + 1) and
      (LObserver.AlignmentAt(0).HorizontalAlignment = nchaEnd) and
      (LObserver.AlignmentAt(0).VerticalAlignment = ncvaBottom) and
      (LRetained.AlignmentAt(0).VerticalAlignment = ncvaTop),
      'new column serial atomically publishes copied effective axes and preserves an old observation');
    Check(NyxCollectionTablePresentation(LPeer).CapturePresentation.Preferences.AlignmentCount = 0,
      'same store peers keep independent runtime alignment');
    LRefused := False;
    try
      LOther := LPresentation.PreparePresentation(LCapture, NyxTablePresentationPreferences);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'an old captured alignment intent cannot clear a newer preference');
    LNext := NewNyxCollectionView(LStore, LPeerSpec.UseDefaultColumnAlignment, cpTable);
    LPrepared := PrepareNyxCollectionColumnTransfer(LView, LNext);
    PublishNyxGroup([LPrepared]);
    LPrepared := nil;
    Check(not LNext.Spec.HasColumnAlignment and
      (NyxCollectionTablePresentation(LNext).CapturePresentation.Preferences.AlignmentCount = 1),
      'same typed field transfer preserves runtime alignment without inventing authored presence');
    Configure(LView, NyxTablePresentationPreferences);
    LObserver := TNyxTablePresentationObservation.Read(LColumns.CaptureColumns);
    Check(not LObserver.HasRuntimeOverrides and
      (LObserver.AlignmentAt(0).VerticalAlignment = ncvaTop) and
      (LView.Spec.ToData.ToJSON = LPeerSpec.ToData.ToJSON) and
      (LStore.Snapshot = LSource) and (LView.CellText(LItem, 0) = CExact),
      'reset restores authored axes with unchanged accepted RAW source and descriptor');
  finally
    LOther := nil;
    LPrepared := nil;
    LColumns := nil;
    LCapture := nil;
    LPresentation := nil;
    LNext := nil;
    LPeer := nil;
    LView := nil;
    LStore := nil;
  end;
  Check((LRetained.AlignmentAt(0).HorizontalAlignment = nchaEnd) and
    (LRetained.FieldAt(0).Name = 'Name') and (LSource.ItemAt(0).GetValue(NyxTextField('Name')) = CExact),
    'retained alignment observation owns only detached values after view retirement');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-column-alignment-view', 'passed');
    document.body.setAttribute('data-column-alignment-view-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' column alignment view checks; no target paint claim');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-column-alignment-view', 'failed');
      document.body.setAttribute('data-column-alignment-view-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
