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
program nyx_collection_range_navigation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.state, nyx.collections, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.selection, nyx.collections.query,
  nyx.collections.arrangement, nyx.collections.arrangement.controls,
  nyx.collections.clipboard, nyx.collections.range, nyx.collections.range.actions,
  nyx.collections.range.navigation, nyx.publication
  {$ifdef PAS2JS}, Web{$endif};

type
  { The actual source/view are shared by every movement. Observer tokens prove
    that proposals/publications retain immutable data, never those live owners. }
  TFixture = class
  public
    Store: INyxCollection;
    View: INyxCollectionView;
    StoreToken: INyxCollectionSubscription;
    ViewToken: INyxCollectionViewSubscription;
    Changes: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure StoreChanged(const AStore: INyxCollection;
      const AChanges: INyxCollectionChanges);
    procedure ViewChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure ReleaseOwners;
  end;

  { One source getter can enter a newer public selection while the original
    preparation normalizes its proposal. This proxy changes no scalar data. }
  TReadCallback = procedure of object;

  { Read-only page projection delegates actual data; Count can invoke one
    borrowed callback, owned explicitly by the surrounding fixture frame. }
  TReadPage = class(TInterfacedObject, INyxCollectionSnapshot)
  public
    Inner: INyxCollectionSnapshot;
    OnRead: TReadCallback;
    RowReads: Integer;
    function GetKey: TNyxCollectionRef;
    function GetSchema: TNyxCollectionSchema;
    function GetCount: Integer;
    function GetRevision: Integer;
    function GetDataBytes: Integer;
    function ItemAt(AIndex: Integer): TNyxCollectionItem;
    function Item(const ARef: TNyxItemRef): TNyxCollectionItem;
    function IndexOf(const ARef: TNyxItemRef): Integer;
    function Has(const ARef: TNyxItemRef): Boolean;
  end;

  { The source callback acts on the real controller; no private flags or
    implementation coordinates are changed by the regression. }
  TReadProbe = class
  public
    State: INyxCollectionCellRangeSelection;
    Source: INyxCollectionSnapshot;
    Page: INyxCollectionSnapshot;
    Fields: TNyxCollectionClipboardFields;
    Cell: TNyxCollectionCellIdentity;
    Serial: Integer;
    Newer: INyxCollectionCellRangeIntent;
    Calls: Integer;
    procedure ClearSelection;
    procedure SelectOne;
  end;

  { Foreign movement getters are genuine public extension callouts. One source
    getter can select newer cells; another case forges only its proposed focus,
    which must be rejected by the shared typed movement admission. }
  TNavigationProxy = class(TInterfacedObject, INyxCollectionRangeNavigation)
  public
    Inner: INyxCollectionRangeNavigation;
    OnSource: TReadCallback;
    WrongFocus: Boolean;
    PageReads: Integer;
    function GetKey: TNyxCollectionRangeKey;
    function GetSource: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetPrevious: INyxCollectionCellRangeIntent;
    function GetOrigin: TNyxCollectionCellIdentity;
    function GetAnchor: TNyxCollectionCellIdentity;
    function GetFocus: TNyxCollectionCellIdentity;
    function GetColumnSerial: Integer;
    function GetFieldCount: Integer;
    function FieldAt(AIndex: Integer): TNyxColumnIdentity;
    function GetIncludeHeaders: Boolean;
    function GetLineEnding: TNyxCollectionCopyLineEnding;
    function GetLimits: TNyxCollectionClipboardLimits;
  end;

const
  CFull: TNyxText = 'full 🌙' + #9 + '"quoted"' + #13#10 + 'tail' + #0 + 'end';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Key: TNyxCollectionRef;
begin
  Result := NyxCollection('navigation source 🌙');
end;

function Cell(const AID: TNyxText; const AField: TNyxColumnIdentity): TNyxCollectionCellIdentity;
begin
  Result := NyxCollectionCellIdentity(NyxItem(Key, AID), AField);
end;

constructor TFixture.Create;
var
  LSpec: TNyxCollectionViewSpec;
begin
  inherited Create;
  Store := NewNyxCollection(Key, NyxCollectionSchema.Text(NyxTextField('Name'), '')
    .Boolean(NyxBooleanField('Flag'), False).Integer(NyxIntegerField('Score'), 0)
    .Text(NyxTextField('Hidden'), ''), [
      NyxCollectionItem(NyxItem(Key, 'a')).WithValue(NyxTextField('Name'), CFull)
        .WithValue(NyxBooleanField('Flag'), True).WithValue(NyxIntegerField('Score'), 2),
      NyxCollectionItem(NyxItem(Key, 'b')).WithValue(NyxTextField('Name'), 'B')
        .WithValue(NyxIntegerField('Score'), 4),
      NyxCollectionItem(NyxItem(Key, 'c')).WithValue(NyxTextField('Name'), 'C')
        .WithValue(NyxIntegerField('Score'), 3),
      NyxCollectionItem(NyxItem(Key, 'off page')).WithValue(NyxIntegerField('Score'), 1)]);
  LSpec := NyxCollectionView(Key).Selection(nsmMultiple)
    .Column(NyxIntegerField('Score'), 'Score').ColumnLayout(NyxColumnLayout.PinEnd)
    .Column(NyxBooleanField('Flag'), 'Flag')
    .Column(NyxTextField('Name'), 'Name').ColumnLayout(NyxColumnLayout.PinStart)
    .Column(NyxTextField('Hidden'), 'Hidden').ColumnLayout(NyxColumnLayout.Visible(False))
    .TableLayout(NyxTableLayout);
  View := NewNyxCollectionView(Store, LSpec, cpTable);
  View.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('Score')).AtLeast(2))
    .OrderBy(NyxIntegerField('Score'), nsdDescending));
  View.SetSelection([NyxItem(Key, 'off page')], NyxItem(Key, 'b'), NyxItem(Key, 'b'));
  StoreToken := Store.Subscribe({$ifdef PAS2JS}@{$endif}StoreChanged);
  ViewToken := View.Subscribe({$ifdef PAS2JS}@{$endif}ViewChanged);
end;

destructor TFixture.Destroy;
begin
  StoreToken.Disconnect;
  ViewToken.Disconnect;
  ReleaseOwners;
  inherited Destroy;
end;

procedure TFixture.StoreChanged(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Changes);
end;

procedure TFixture.ViewChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Changes);
end;

procedure TFixture.ReleaseOwners;
begin
  View := nil;
  Store := nil;
end;

function TReadPage.GetKey: TNyxCollectionRef;
begin
  Result := Inner.Key;
end;

function TReadPage.GetSchema: TNyxCollectionSchema;
begin
  Result := Inner.Schema;
end;

function TReadPage.GetCount: Integer;
var
  LRead: TReadCallback;
begin
  LRead := OnRead;
  OnRead := nil;

  if Assigned(LRead) then
  begin
    LRead;
  end;
  Result := Inner.Count;
end;

function TReadPage.GetRevision: Integer;
begin
  Result := Inner.Revision;
end;

function TReadPage.GetDataBytes: Integer;
begin
  Result := Inner.DataBytes;
end;

function TReadPage.ItemAt(AIndex: Integer): TNyxCollectionItem;
begin
  Inc(RowReads);
  Result := Inner.ItemAt(AIndex);
end;

function TReadPage.Item(const ARef: TNyxItemRef): TNyxCollectionItem;
begin
  Inc(RowReads);
  Result := Inner.Item(ARef);
end;

function TReadPage.IndexOf(const ARef: TNyxItemRef): Integer;
begin
  Result := Inner.IndexOf(ARef);
end;

function TReadPage.Has(const ARef: TNyxItemRef): Boolean;
begin
  Result := Inner.Has(ARef);
end;

procedure TReadProbe.ClearSelection;
begin
  Inc(Calls);
  State.Clear;
end;

procedure TReadProbe.SelectOne;
begin
  Inc(Calls);
  Newer := State.Select(Source, Page, Fields, Cell, Cell, Serial,
    True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
end;

function TNavigationProxy.GetKey: TNyxCollectionRangeKey;
begin
  Result := Inner.Key;
end;

function TNavigationProxy.GetSource: INyxCollectionSnapshot;
var
  LRead: TReadCallback;
begin
  LRead := OnSource;
  OnSource := nil;

  if Assigned(LRead) then
  begin
    LRead;
  end;
  Result := Inner.Source;
end;

function TNavigationProxy.GetPage: INyxCollectionSnapshot;
begin
  Inc(PageReads);
  Result := Inner.Page;
end;

function TNavigationProxy.GetPrevious: INyxCollectionCellRangeIntent;
begin
  Result := Inner.Previous;
end;

function TNavigationProxy.GetOrigin: TNyxCollectionCellIdentity;
begin
  Result := Inner.Origin;
end;

function TNavigationProxy.GetAnchor: TNyxCollectionCellIdentity;
begin
  Result := Inner.Anchor;
end;

function TNavigationProxy.GetFocus: TNyxCollectionCellIdentity;
begin
  Result := Inner.Focus;

  if WrongFocus then
  begin
    Result := Inner.Origin;
  end;
end;

function TNavigationProxy.GetColumnSerial: Integer;
begin
  Result := Inner.ColumnSerial;
end;

function TNavigationProxy.GetFieldCount: Integer;
begin
  Result := Inner.FieldCount;
end;

function TNavigationProxy.FieldAt(AIndex: Integer): TNyxColumnIdentity;
begin
  Result := Inner.FieldAt(AIndex);
end;

function TNavigationProxy.GetIncludeHeaders: Boolean;
begin
  Result := Inner.IncludeHeaders;
end;

function TNavigationProxy.GetLineEnding: TNyxCollectionCopyLineEnding;
begin
  Result := Inner.LineEnding;
end;

function TNavigationProxy.GetLimits: TNyxCollectionClipboardLimits;
begin
  Result := Inner.Limits;
end;

{ End any helper-returned view/columns facade before real owner retirement. }
procedure VisibleColumns(const AView: INyxCollectionView;
  out ASerial: Integer; out AFields: TNyxCollectionClipboardFields);
var
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
begin
  LColumns := NyxCollectionColumns(AView);
  LCapture := LColumns.CaptureColumns;
  ASerial := LCapture.Serial;
  AFields := NyxCollectionClipboardFields(TNyxColumnLayoutObservation.Read(LCapture),
    nccfVisiblePresentation);
end;

procedure Run;
const
  CRow: array[0..7] of Integer = (1, 1, 0, 2, 1, 1, 0, 2);
  CColumn: array[0..7] of Integer = (0, 2, 1, 1, 0, 2, 0, 2);
var
  LFixture: TFixture;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LFields: TNyxCollectionClipboardFields;
  LBadFields: TNyxCollectionClipboardFields;
  LRows: TNyxCollectionSelectionSnapshot;
  LSerial: Integer;
  LState: INyxCollectionCellRangeSelection;
  LPreparer: INyxCollectionCellRangePreparation;
  LOriginal: INyxCollectionCellRangeIntent;
  LCurrent: INyxCollectionCellRangeIntent;
  LNavigation: INyxCollectionRangeNavigation;
  LSaved: INyxCollectionRangeNavigation;
  LPlan: INyxPreparedPublication;
  LOther: INyxPreparedPublication;
  LReady: INyxPublicationReadiness;
  LOrigin: TNyxCollectionCellIdentity;
  LIndex: Integer;
  LRefused: Boolean;
  LProxy: TReadPage;
  LProxyLease: INyxCollectionSnapshot;
  LProbe: TReadProbe;
  LBefore: Integer;
  LForeign: TNavigationProxy;
  LForeignLease: INyxCollectionRangeNavigation;

  function Move(AKey: TNyxCollectionRangeKey; const ACell: TNyxCollectionCellIdentity;
    const APrevious: INyxCollectionCellRangeIntent): INyxCollectionRangeNavigation;
  begin
    Result := NewNyxCollectionRangeNavigation(LSource, LPage, LFields,
      ACell, APrevious, AKey, LSerial, True, nccleCRLF,
      TNyxCollectionClipboardLimits.Defaults);
  end;

  procedure Prepare(const AValue: INyxCollectionRangeNavigation);
  begin
    LPlan := LPreparer.PrepareNavigation(AValue);
    LPlan.Validate;
    Check(Supports(LPlan, INyxPublicationReadiness, LReady),
      'actual prepared selection exposes a callback-free final fence');
    LReady.CheckReady;
  end;

  procedure Publish;
  begin
    PublishNyxGroup([LPlan]);
    LReady := nil;
    LPlan := nil;
    LCurrent := LState.Selection;
  end;

begin
  LFixture := TFixture.Create;
  LProbe := TReadProbe.Create;
  try
    LSource := LFixture.Store.Snapshot;
    LPage := LFixture.View.Snapshot;
    LRows := LFixture.View.Selection.Snapshot;
    VisibleColumns(LFixture.View, LSerial, LFields);
    Check((LPage.Count = 3) and (LPage.ItemAt(0).Ref.ID = 'b') and
      (LPage.ItemAt(2).Ref.ID = 'a') and (Length(LFields) = 3) and
      (LFields[0].Name = 'Name') and (LFields[1].Name = 'Flag') and
      (LFields[2].Name = 'Score'), 'real filtered page and both logical pins define navigation order');
    LOrigin := Cell('c', LFields[1]);
    for LIndex := 0 to 7 do
    begin
      LNavigation := Move(TNyxCollectionRangeKey(LIndex), LOrigin, nil);
      Check(LNavigation.Anchor.Same(LOrigin) and
        (LNavigation.Focus.Item.ID = LPage.ItemAt(CRow[LIndex]).Ref.ID) and
        LNavigation.Focus.Field.Same(LFields[CColumn[LIndex]]),
        'typed extended key uses actual visible/page order and retains its anchor');
    end;
    LNavigation := Move(ncrkExtendLeft, Cell('b', LFields[0]), nil);
    Check(LNavigation.Focus.Same(Cell('b', LFields[0])), 'left clamps without wrapping the first cell');
    LNavigation := Move(ncrkExtendDown, Cell('a', LFields[2]), nil);
    Check(LNavigation.Focus.Same(Cell('a', LFields[2])), 'down clamps without leaving the last page row');
    LState := NewNyxCollectionCellRangeSelection;
    Check(Supports(LState, INyxCollectionCellRangePreparation, LPreparer),
      'existing independent selection opts into prepared navigation without an ABI change');
    LOriginal := LState.Select(LSource, LPage, LFields, LOrigin, LOrigin,
      LSerial, True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
    LNavigation := Move(ncrkExtendRight, LOrigin, LOriginal);
    Prepare(LNavigation);
    Check((LState.Selection = LOriginal) and LOriginal.Current,
      'preparation and final checks preserve the accepted original range before callbacks');
    LPlan.Retire;
    Check((LState.Selection = LOriginal) and LOriginal.Current,
      'abandoned uninstalled movement preserves accepted selection and authority');
    Prepare(LNavigation);
    Publish;
    Check(not LOriginal.Current and LCurrent.Current and
      LCurrent.Capture.Anchor.Same(LOrigin) and LCurrent.Capture.Focus.Same(Cell('c', LFields[2])),
      'publication installs the original extension once and retires the old cancellation token');
    LNavigation := Move(ncrkExtendDown, LCurrent.Capture.Focus, LCurrent);
    Prepare(LNavigation);
    Publish;
    Check((LCurrent.Capture.Payload.Text = 'Flag' + #9 + 'Score' + #13#10 +
      'false' + #9 + '3' + #13#10 + 'true' + #9 + '2') and
      LCurrent.Capture.Anchor.Same(LOrigin) and (LCurrent.Capture.Focus.Item.ID = 'a'),
      'continued extension preserves directional anchor and exact existing SelectedPage RAW TSV');
    LOriginal := LCurrent;
    LNavigation := Move(ncrkExtendRowStart, LCurrent.Capture.Focus, LCurrent);
    Prepare(LNavigation);
    LState.Clear;
    LRefused := False;
    try
      LReady.CheckReady;
    except
      on E: ENyxCollectionRangeStale do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LOriginal.Current and (LState.Selection = nil),
      'real canonical-style Clear defeats the older prepared extension before final install');
    LPlan.Retire;
    LReady := nil;
    LPlan := nil;
    LNavigation := Move(ncrkExtendRight, LOrigin, nil);
    Prepare(LNavigation);
    LOther := LPreparer.PrepareNavigation(Move(ncrkExtendLeft, LOrigin, nil));
    LRefused := False;
    try
      LReady.CheckReady;
    except
      on E: ENyxCollectionRangeStale do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LState.Selection = nil),
      'a newer preparation invalidates its older request without publishing either');
    LPlan.Retire;
    LReady := nil;
    LPlan := nil;
    PublishNyxGroup([LOther]);
    LOther := nil;
    LCurrent := LState.Selection;
    Check(LCurrent.Capture.Focus.Same(Cell('c', LFields[0])),
      'retiring an older request cannot consume the newer prepared selection');
    LNavigation := NewNyxCollectionRangeNavigation(nil, nil, [],
      Default(TNyxCollectionCellIdentity), LCurrent, ncrkClear, LSerial,
      False, nccleLF, Default(TNyxCollectionClipboardLimits));
    Prepare(LNavigation);
    Check(LCurrent.Current, 'prepared Escape does not clear before canonical cancellation is decided');
    Publish;
    Check((LCurrent = nil) and not LNavigation.Previous.Current,
      'explicit prepared Escape withdraws the original range without requiring a fresh dataset');
    LNavigation := Move(ncrkExtendRight, LOrigin, nil);
    LSaved := LNavigation;
    LRefused := False;
    try
      LNavigation := Move(ncrkExtendRight, Cell('off page', LFields[0]), nil);
    except
      on E: ENyxCollectionRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LNavigation = LSaved),
      'off-page movement refuses atomically without clearing the caller proposal');
    LBadFields := Copy(LFields, 0, Length(LFields));
    LBadFields[1] := NyxColumnIdentity(NyxTextField('Flag'));
    LRefused := False;
    try
      NewNyxCollectionRangeNavigation(LSource, LPage, LBadFields, LOrigin, nil,
        ncrkExtendRight, LSerial, True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'movement cannot reinterpret an existing Boolean field as text');
    LProxy := TReadPage.Create;
    LProxyLease := LProxy;
    LProxy.Inner := LPage;
    LRefused := False;
    try
      NewNyxCollectionRangeNavigation(LSource, LProxyLease, LFields, LOrigin, nil,
        ncrkExtendRight, LSerial, False, nccleLF,
        TNyxCollectionClipboardLimits.Defaults.ScannedRows(1));
    except
      on E: ENyxCollectionRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProxy.RowReads = 0), 'bounded movement refuses excessive page work before reading rows');
    LForeign := TNavigationProxy.Create;
    LForeignLease := LForeign;
    LForeign.Inner := Move(ncrkExtendRight, LOrigin, nil);
    LProbe.State := LState;
    LProbe.Source := LSource;
    LProbe.Page := LPage;
    LProbe.Fields := Copy(LFields, 0, Length(LFields));
    LProbe.Cell := Cell('b', LFields[0]);
    LProbe.Serial := LSerial;
    LForeign.OnSource := {$ifdef PAS2JS}@{$endif}LProbe.SelectOne;
    LRefused := False;
    try
      LPlan := LPreparer.PrepareNavigation(LForeignLease);
    except
      on E: ENyxCollectionRangeStale do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProbe.Calls = 1) and (LForeign.PageReads = 0) and
      LProbe.Newer.Current and (LState.Selection = LProbe.Newer),
      'newer selection from a real proposal getter defeats the original request before more getters');
    LState.Clear;
    LForeign.WrongFocus := True;
    LRefused := False;
    try
      LPlan := LPreparer.PrepareNavigation(LForeignLease);
    except
      on E: ENyxCollectionRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LState.Selection = nil),
      'foreign movement cannot substitute endpoints that contradict its typed extended key');
    LForeignLease := nil;
    LProbe.Calls := 0;
    LCurrent := LState.Select(LSource, LProxyLease, LFields, LOrigin, LOrigin,
      LSerial, True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
    LNavigation := NewNyxCollectionRangeNavigation(LSource, LProxyLease, LFields,
      LOrigin, LCurrent, ncrkExtendRight, LSerial, True, nccleCRLF,
      TNyxCollectionClipboardLimits.Defaults);
    LProbe.State := LState;
    LProxy.OnRead := {$ifdef PAS2JS}@{$endif}LProbe.ClearSelection;
    LBefore := LProxy.RowReads;
    LRefused := False;
    try
      LPlan := LPreparer.PrepareNavigation(LNavigation);
    except
      on E: ENyxCollectionRangeStale do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProbe.Calls = 1) and (LState.Selection = nil) and
      not LCurrent.Current and (LProxy.RowReads >= LBefore),
      'actual proposal normalization getter can clear without older preparation overwriting it');
    LNavigation := Move(ncrkExtendFirstCell, Cell('a', LFields[2]), nil);
    Prepare(LNavigation);
    Publish;
    Check((LCurrent.Capture.Payload.RowCount = 3) and (LCurrent.Capture.Payload.FieldCount = 3) and
      (LCurrent.Capture.Payload.CellValue(2, 0).TextValue = CFull),
      'whole-page extension keeps exact supplementary Unicode, NUL, newlines and typed values');
    Check((LFixture.Changes = 0) and (LFixture.Store.Snapshot = LSource) and
      LFixture.View.Selection.Snapshot.SameState(LRows),
      'independent movement changes no accepted data, ordinary row selection or query view');
    LState.Revoke;
    LProbe.State := nil;
    LPreparer := nil;
    LState := nil;
    LFixture.ReleaseOwners;
    Check(not LFixture.ViewToken.Connected and not LFixture.StoreToken.Connected and
      not LCurrent.Current and (LCurrent.Capture.Payload.CellValue(2, 0).TextValue = CFull),
      'retained navigation/preparations/payload hold no live view/store owner after explicit retirement');
  finally

    if LProxyLease <> nil then
    begin
      LProxy.OnRead := nil;
    end;

    if LPlan <> nil then
    begin
      LPlan.Retire;
    end;

    if LOther <> nil then
    begin
      LOther.Retire;
    end;
    LProbe.Free;
    LFixture.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-range-navigation-tests', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed range navigation checks; no UI or host effects');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-range-navigation-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
