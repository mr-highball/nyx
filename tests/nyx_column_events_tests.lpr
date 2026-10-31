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
program nyx_column_events_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.state, nyx.types, nyx.collections, nyx.collections.columns,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.arrangement,
  nyx.collections.presentation, nyx.collections.column.events, nyx.publication
  {$ifdef PAS2JS}, Web{$endif};

type
  { Alternative implementation preserves only the original Columns GUID.
    The new helper must forward its preparation without requiring a new facet
    or inventing an origin that this implementation did not report. }
  TLegacyColumns = class(TInterfacedObject, INyxCollectionColumns)
  public
    Original: INyxCollectionColumns;
    function CaptureColumns: INyxCollectionColumnsCapture;
    function PrepareColumns(const ACapture: INyxCollectionColumnsCapture;
      const APreferences: TNyxColumnPreferences): INyxPreparedPublication;
    function PrepareAutoSize(const ACapture: INyxCollectionColumnsCapture;
      const AField: TNyxColumnIdentity; AWidth: Integer): INyxPreparedPublication;
    function ConfigureColumns(const APreferences: TNyxColumnPreferences): INyxCollectionColumns;
  end;
  { Forward fixture owns every borrowed method receiver until subscriptions
    disconnect. Its observable order deliberately mixes old and new observers. }
  TFixture = class;
  { Managed receiver belongs to its token; Owner remains borrowed. The test can
    retire its preparation or disconnect a sibling during the first callback. }
  TReceiver = class(TNyxColumnChangeObserver)
  public
    Owner: TFixture;
    Number: Integer;
    procedure ColumnsChanged(const AChanges: INyxColumnChanges); override;
  end;
  { One portable public view over real accepted data. No renderer or UI behavior
    is inferred from these checks; target/Studio consumers qualify that separately. }
  TFixture = class
  public
    Store: INyxCollection;
    View: INyxCollectionView;
    Columns: INyxCollectionColumns;
    Events: INyxCollectionColumnEvents;
    First: INyxCollectionViewSubscription;
    Second: INyxCollectionViewSubscription;
    GenericBefore: INyxCollectionViewSubscription;
    GenericAfter: INyxCollectionViewSubscription;
    StoreToken: INyxCollectionSubscription;
    Stage: INyxPreparedPublication;
    Last: INyxColumnChanges;
    Other: INyxColumnChanges;
    Order: TNyxText;
    FailGeneric: Boolean;
    FailFirst: Boolean;
    RetirePlan: Boolean;
    CancelSecond: Boolean;
    ReleaseView: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure BeforeChanged(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
    procedure AfterChanged(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
    procedure RowsChanged(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
    procedure Received(ANumber: Integer; const AChanges: INyxColumnChanges);
  end;

var
  GChecks: Integer;

function TLegacyColumns.CaptureColumns: INyxCollectionColumnsCapture;
begin
  Result := Original.CaptureColumns;
end;

function TLegacyColumns.PrepareColumns(const ACapture: INyxCollectionColumnsCapture;
  const APreferences: TNyxColumnPreferences): INyxPreparedPublication;
begin
  Result := Original.PrepareColumns(ACapture, APreferences);
end;

function TLegacyColumns.PrepareAutoSize(const ACapture: INyxCollectionColumnsCapture;
  const AField: TNyxColumnIdentity; AWidth: Integer): INyxPreparedPublication;
begin
  Result := Original.PrepareAutoSize(ACapture, AField, AWidth);
end;

function TLegacyColumns.ConfigureColumns(const APreferences: TNyxColumnPreferences): INyxCollectionColumns;
begin
  Original.ConfigureColumns(APreferences);
  Result := Self;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Column events: ' + AReason);
  end;
  Inc(GChecks);
end;

function NameField: TNyxColumnIdentity;
begin
  Result := NyxColumnIdentity(NyxTextField('service 🌙'));
end;

function Spec: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(NyxCollection('jobs'))
    .Column(NyxTextField('service 🌙'), 'Service')
      .ColumnSizing(NyxColumnSizing.Fixed(160).Minimum(30).Maximum(500))
    .Column(NyxIntegerField('pid'), 'Process ID')
      .ColumnSizing(NyxColumnSizing.Fixed(100).Minimum(30).Maximum(500))
    .TableLayout(NyxTableLayout);
end;

constructor TFixture.Create;
var
  LReceiver: TReceiver;
begin
  inherited Create;
  Store := NewNyxCollection(NyxCollection('jobs'),
    NyxCollectionSchema.Text(NyxTextField('service 🌙'), '')
      .Integer(NyxIntegerField('pid'), 0), [
      NyxCollectionItem(NyxItem(NyxCollection('jobs'), 'gateway'))
        .WithValue(NyxTextField('service 🌙'), 'Request gateway')
        .WithValue(NyxIntegerField('pid'), 2401)]);
  View := NewNyxCollectionView(Store, Spec, cpTable);
  Columns := NyxCollectionColumns(View);
  Check(Supports(View, INyxCollectionColumnEvents, Events), 'optional event capability exists');
  GenericBefore := View.Subscribe(BeforeChanged);
  LReceiver := TReceiver.Create;
  LReceiver.Owner := Self;
  LReceiver.Number := 1;
  First := Events.SubscribeColumns(LReceiver);
  LReceiver := TReceiver.Create;
  LReceiver.Owner := Self;
  LReceiver.Number := 2;
  Second := Events.SubscribeColumns(LReceiver);
  GenericAfter := View.Subscribe(AfterChanged);
  StoreToken := Store.Subscribe(RowsChanged);
end;

destructor TFixture.Destroy;
begin
  Stage := nil;
  First := nil;
  Second := nil;
  GenericBefore := nil;
  GenericAfter := nil;
  StoreToken := nil;
  Events := nil;
  Columns := nil;
  View := nil;
  Store := nil;
  inherited Destroy;
end;

procedure TFixture.BeforeChanged(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
begin
  Order := Order + 'before/';

  if FailGeneric then
  begin
    raise Exception.Create('Expected ordinary observer failure');
  end;
end;

procedure TFixture.AfterChanged(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
begin
  Order := Order + 'after/';
end;

procedure TFixture.RowsChanged(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
begin
  raise Exception.Create('A column publication must not write source rows');
end;

procedure TReceiver.ColumnsChanged(const AChanges: INyxColumnChanges);
begin
  Owner.Received(Number, AChanges);
end;

procedure TFixture.Received(ANumber: Integer; const AChanges: INyxColumnChanges);
var
  LRefused: Boolean;
begin
  Order := Order + TNyxText(IntToStr(ANumber)) + '/';
  Check(AChanges <> nil, 'every entered typed observer receives its owned payload');

  if ANumber = 1 then
  begin
    Last := AChanges;
    LRefused := False;
    try
      Columns.ConfigureColumns(NyxColumnPreferences);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'notification retains the existing no-reentry reservation');

    if CancelSecond then
    begin
      Second := nil;
    end;

    if RetirePlan then
    begin
      Stage.Retire;
      Stage := nil;
    end;

    if ReleaseView then
    begin
      Events := nil;
      Columns := nil;
      View := nil;
      Store := nil;
    end;

    if FailFirst then
    begin
      raise Exception.Create('Expected typed observer failure');
    end;
  end
  else
  begin
    Other := AChanges;
    Check(Other = Last, 'ordered observers share the same immutable admitted snapshot');
  end;
end;

{ Fluent results live until the enclosing routine exits on FPC. Keep their
  temporary leases in this short command frame so retirement below tests the
  subscription/payload ownership rather than the compiler's return temporary. }
procedure Configure(AFixture: TFixture; const APreferences: TNyxColumnPreferences);
var
  LResult: INyxCollectionColumns;
begin
  LResult := AFixture.Columns.ConfigureColumns(APreferences);
  Check(LResult <> nil, 'fluent column configuration returns its original capability');
end;

procedure ConfigureDensity(AFixture: TFixture);
var
  LResult: INyxCollectionTablePresentation;
begin
  LResult := NyxCollectionTablePresentation(AFixture.View).ConfigurePresentation(
    NyxTablePresentationPreferences.Density(ntrdComfortable));
  Check(LResult <> nil, 'presentation configuration retains its own fluent capability');
end;

procedure SnapshotCases;
var
  LChanges: INyxColumnChanges;
  LSnapshot: TNyxColumnChangesSnapshot;
  LCopy: TNyxColumnChangesSnapshot;
  LPreferences: TNyxColumnPreferences;
  LFailed: Boolean;
begin
  LSnapshot := Default(TNyxColumnChangesSnapshot);
  Check(not LSnapshot.Defined and (LSnapshot.Count = 0), 'default event snapshot is safely absent');
  LFailed := False;
  try
    LSnapshot.ChangeAt(0);
  except
    on EArgumentOutOfRangeException do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'absent snapshot cannot invent a first field');
  LPreferences := NyxColumnPreferences.WithColumn(NyxColumnPreference(NameField)
    .Width(160).Visible(False).Pinned(ncpEnd)).Order([NameField]);
  LChanges := NewNyxColumnChanges(NewNyxColumnViewRef, Spec, NyxColumnPreferences,
    LPreferences, 4, 5, nccoManager);
  LSnapshot := LChanges.Snapshot;
  LCopy := LSnapshot;
  LChanges := nil;
  Check(LCopy.Defined and (LCopy.ChangeAt(0).Properties =
    [nccWidth, nccOrder, nccVisibility, nccPin]),
    'equal-to-default width and explicit order prefix still report override ownership');
  LPreferences := LCopy.After.WithoutColumn(NameField);
  Check((LPreferences.Count = 0) and (LSnapshot.After.Count = 1),
    'copied portable payload getters keep their private immutable storage');
  LFailed := False;
  try
    LChanges := NewNyxColumnChanges(NewNyxColumnViewRef, Spec, NyxColumnPreferences,
      NyxColumnPreferences.WithColumn(NyxColumnPreference(
        NyxColumnIdentity(NyxBooleanField('pid'))).Visible(False)), 0, 1, nccoProgrammatic);
  except
    on EArgumentException do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'same textual field with a different scalar family refuses');
end;

procedure Run;
var
  LFixture: TFixture;
  LCapture: INyxCollectionColumnsCapture;
  LFirst: INyxColumnChanges;
  LPreferences: TNyxColumnPreferences;
  LCopy: TNyxColumnPreferences;
  LScope: TNyxColumnViewRef;
  LForeign: TNyxColumnViewRef;
  LSnapshot: INyxCollectionSnapshot;
  LFailed: Boolean;
  LDelta: TNyxColumnChange;
  LSerial: Integer;
  LLegacyObject: TLegacyColumns;
  LLegacy: INyxCollectionColumns;
begin
  LScope := NewNyxColumnViewRef;
  LForeign := NewNyxColumnViewRef;
  Check(LScope.Same(LScope) and not LScope.Same(LForeign), 'inert scopes distinguish independent views');
  Check(NewNyxColumnChanges(LScope, Spec, NyxColumnPreferences,
    NyxColumnPreferences, 0, 1, nccoUnspecified) = nil, 'equal preferences have no change payload');
  LFixture := TFixture.Create;
  try
    LSnapshot := LFixture.Store.Snapshot;
    Configure(LFixture, NyxColumnPreferences);
    Check(LFixture.Order = '', 'no-op emits neither generic nor typed notification');
    LCapture := LFixture.Columns.CaptureColumns;
    LPreferences := NyxColumnPreferences.WithColumn(
      NyxColumnPreference(NameField).Width(170).Pinned(ncpStart));
    Configure(LFixture, LPreferences);
    LFirst := LFixture.Last;
    Check(LFixture.Order = 'before/1/2/after/', 'legacy and typed observers retain one ordered sequence');
    Check((LFirst.BeforeSerial = 0) and (LFirst.Serial = 1) and
      (LFirst.Origin = nccoProgrammatic) and (LFirst.Count = 1), 'first committed operation has exact serials and origin');
    LDelta := LFirst.ChangeAt(0);
    Check(LDelta.Field.Same(NameField) and (LDelta.Properties = [nccWidth, nccPin]) and
      not LDelta.Before.HasWidth and (LDelta.After.WidthValue = 170), 'typed Unicode field and multi-axis changes remain exact');
    LCopy := LFirst.After.WithColumn(NyxColumnPreference(NameField).Width(300));
    Check((LCopy.ColumnAt(0).WidthValue = 300) and (LFirst.After.ColumnAt(0).WidthValue = 170),
      'mutating a returned preference value cannot change retained data');
    Check(LFixture.Store.Snapshot = LSnapshot, 'column notification never publishes source data');
    LFixture.Order := '';
    LFailed := False;
    try
      PublishNyxGroup([LFixture.Columns.PrepareColumns(LCapture, NyxColumnPreferences)]);
    except
      on ENyxCollection do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LFixture.Order = '') and (LFixture.Columns.CaptureColumns.Serial = 1),
      'stale capture refuses without a successful change event');
    LFixture.Stage := LFixture.Events.PrepareColumnChange(LFixture.Columns.CaptureColumns,
      LPreferences.Order([NyxColumnIdentity(NyxIntegerField('pid')), NameField]), nccoManager);
    LFixture.RetirePlan := True;
    PublishNyxGroup([LFixture.Stage]);
    Check((LFixture.Order = 'before/1/2/after/') and (LFixture.Last.Origin = nccoManager) and
      (LFixture.Last.Count = 2), 'retiring the entered plan cannot clear later observers payloads');
    Check(LFixture.Last.View.Same(LFirst.View) and
      (LFixture.Last.ChangeAt(0).BeforePosition = 0) and (LFixture.Last.ChangeAt(0).Position = 1),
      'order changes retain canonical positions and exact view identity');
    LFixture.RetirePlan := False;
    LFixture.Order := '';
    LFixture.FailGeneric := True;
    LFixture.FailFirst := True;
    LFailed := False;
    LSerial := LFixture.Columns.CaptureColumns.Serial;
    try
      Configure(LFixture, NyxColumnPreferences);
    except
      on ENyxPublicationNotification do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LFixture.Order = 'before/1/2/after/') and
      (LFixture.Columns.CaptureColumns.Serial = LSerial + 1),
      'generic and typed failures preserve committed preferences and independent delivery');
    LFixture.FailGeneric := False;
    LFixture.FailFirst := False;
    LFixture.Order := '';
    ConfigureDensity(LFixture);
    Check(LFixture.Order = 'before/after/', 'density-only changes do not masquerade as column arrangement');
    LFixture.Order := '';
    LFixture.CancelSecond := True;
    PublishNyxGroup([LFixture.Columns.PrepareAutoSize(LFixture.Columns.CaptureColumns, NameField, 180)]);
    Check((LFixture.Order = 'before/1/after/') and (LFixture.Last.Origin = nccoAutoSize),
      'cancelled sibling is skipped and Fit retains its authoritative operation path');
    LFixture.CancelSecond := False;
    LFixture.Order := '';
    LLegacyObject := TLegacyColumns.Create;
    LLegacy := LLegacyObject;
    LLegacyObject.Original := LFixture.Columns;
    PublishNyxGroup([PrepareNyxColumnChange(LLegacy, LLegacy.CaptureColumns,
      NyxColumnPreferences, nccoResize)]);
    Check((LFixture.Order = 'before/1/after/') and
      (LFixture.Last.Origin = nccoUnspecified),
      'older alternative Columns remains usable without a fabricated reported origin');
    LLegacy := nil;
    Configure(LFixture, NyxColumnPreferences.WithColumn(NyxColumnPreference(NameField).Width(190)));
    LFixture.Order := '';
    LFixture.ReleaseView := True;
    Configure(LFixture, NyxColumnPreferences);
    Check(not LFixture.First.Connected and not LFixture.GenericAfter.Connected and
      not LFixture.StoreToken.Connected, 'entered publication can release the whole view/store without a retained cycle');
    Check((LFirst.After.ColumnAt(0).WidthValue = 170) and
      (LFirst.ChangeAt(0).Field.Name = 'service 🌙'), 'payload remains exact after its view and store retire');
  finally
    LFixture.Free;
  end;
end;

begin
  try
    SnapshotCases;
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-column-events', 'passed');
    document.body.setAttribute('data-column-events-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable column notification checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-column-events', 'failed');
      document.body.setAttribute('data-column-events-error', LException.Message);
      {$else}
      WriteLn('FAIL ', TNyxText(LException.Message));
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
