{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
program nyx_collection_range_pointer_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.state, nyx.collections, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.selection, nyx.collections.query,
  nyx.collections.arrangement, nyx.collections.arrangement.controls,
  nyx.collections.clipboard, nyx.collections.range, nyx.collections.range.actions,
  nyx.collections.range.pointer
  {$ifdef PAS2JS}, Web{$endif};

type
  { Real data/query/row-selection owners are independent from the cell range.
    Their retained subscriptions prove that detached pointer values keep no
    store/view alive. Fluent owner calls end in this bounded constructor. }
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

  { One borrowed synchronous source callback models a genuine nested command
    or caller-owned interface withdrawal while preparation reads immutable data. }
  TReadCallback = procedure of object;

  { A read-only page proxy delegates every real row. Its one-shot Count getter
    can reenter the public selection owner without changing accepted data. }
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

  { Actions operate only through actual public interfaces. Newer remains an
    immutable data/cancellation token, never a controller backreference. }
  TProbe = class
  public
    State: INyxCollectionCellRangeSelection;
    Preparation: INyxCollectionCellRangePointerPreparation;
    Source: INyxCollectionSnapshot;
    Page: INyxCollectionSnapshot;
    Fields: TNyxCollectionClipboardFields;
    Cell: TNyxCollectionCellIdentity;
    Serial: Integer;
    Newer: INyxCollectionCellRangeIntent;
    Calls: Integer;
    procedure Clear;
    procedure SelectOne;
    procedure WithdrawOwners;
  end;

const
  CFull: TNyxText = 'full 🌙' + #9 + '"quoted"' + #13#10 + 'tail' + #0 + 'end';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Key: TNyxCollectionRef;
begin
  Result := NyxCollection('pointer source 🌙');
end;

function Cell(const AID: TNyxText;
  const AField: TNyxColumnIdentity): TNyxCollectionCellIdentity;
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
      NyxCollectionItem(NyxItem(Key, 'c🌙')).WithValue(NyxTextField('Name'), 'C')
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
  LReply: TReadCallback;
begin
  LReply := OnRead;
  OnRead := nil;

  if Assigned(LReply) then
  begin
    LReply;
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

procedure TProbe.Clear;
begin
  Inc(Calls);
  State.Clear;
end;

procedure TProbe.SelectOne;
begin
  Inc(Calls);
  Newer := State.Select(Source, Page, Fields, Cell, Cell, Serial,
    True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
end;

procedure TProbe.WithdrawOwners;
begin
  Inc(Calls);
  Preparation := nil;
  State := nil;
end;

{ End helper-returned view/column interfaces before asserting real retirement. }
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

{ This bounded method call prevents a pas2js receiver temporary in Run from
  masking the preparation's own lease when the getter withdraws both owners. }
function EnterWithWithdrawal(AProbe: TProbe;
  const APrevious: INyxCollectionCellRangeIntent): INyxCollectionCellRangePointerCommand;
begin
  Result := AProbe.Preparation.PreparePointer(AProbe.Source, AProbe.Page,
    AProbe.Fields, AProbe.Cell, AProbe.Cell, APrevious, ncrpReplace,
    AProbe.Serial, True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
end;

{ Keep self-returning selection temporaries outside the final owner-release
  assertion; they are legitimate language leases, not payload ownership. }
function SelectProbe(AProbe: TProbe): INyxCollectionCellRangeIntent;
begin
  Result := AProbe.State.Select(AProbe.Source, AProbe.Page, AProbe.Fields,
    AProbe.Cell, AProbe.Cell, AProbe.Serial, False, nccleLF,
    TNyxCollectionClipboardLimits.Defaults);
end;

{ Factory/QI result temporaries also belong to their Pascal routine. End those
  legitimate owner leases before testing withdrawal from an entered getter. }
procedure CreateProbeOwner(AProbe: TProbe);
var
  LState: INyxCollectionCellRangeSelection;
  LPreparation: INyxCollectionCellRangePointerPreparation;
begin
  LState := NewNyxCollectionCellRangeSelection;
  Check(Supports(LState, INyxCollectionCellRangePointerPreparation, LPreparation),
    'independent preparation owner is available for entered receiver withdrawal');
  AProbe.State := LState;
  AProbe.Preparation := LPreparation;
end;

{ Bound getter-returned interfaces so this semantic assertion cannot itself
  retain an owner past the later real subscription-retirement check. }
procedure CheckOriginalOwners(AFixture: TFixture;
  const ASource, APage: INyxCollectionSnapshot;
  const ARows: TNyxCollectionSelectionSnapshot);
begin
  Check((AFixture.Changes = 0) and (AFixture.Store.Snapshot = ASource) and
    (AFixture.View.Snapshot = APage) and AFixture.View.Selection.Snapshot.SameState(ARows),
    'all endpoint actions preserve accepted data query page and independent ordinary row selection');
end;

procedure Run;
var
  LFixture: TFixture;
  LProbe: TProbe;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LRows: TNyxCollectionSelectionSnapshot;
  LFields: TNyxCollectionClipboardFields;
  LOtherFields: TNyxCollectionClipboardFields;
  LSerial: Integer;
  LState: INyxCollectionCellRangeSelection;
  LPreparation: INyxCollectionCellRangePointerPreparation;
  LCommand: INyxCollectionCellRangePointerCommand;
  LOther: INyxCollectionCellRangePointerCommand;
  LSavedCommand: INyxCollectionCellRangePointerCommand;
  LCurrent: INyxCollectionCellRangeIntent;
  LPrevious: INyxCollectionCellRangeIntent;
  LPointer: INyxCollectionRangePointer;
  LSaved: INyxCollectionRangePointer;
  LExpected: INyxCollectionCellRangeCapture;
  LOrigin: TNyxCollectionCellIdentity;
  LTarget: TNyxCollectionCellIdentity;
  LProxy: TReadPage;
  LProxyLease: INyxCollectionSnapshot;
  LRefused: Boolean;
  LBefore: Integer;

  function Prepare(AAction: TNyxCollectionRangePointerAction;
    const AFrom, ATo: TNyxCollectionCellIdentity;
    const APrevious: INyxCollectionCellRangeIntent): INyxCollectionCellRangePointerCommand;
  begin
    Result := LPreparation.PreparePointer(LSource, LPage, LFields, AFrom, ATo,
      APrevious, AAction, LSerial, True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
  end;

begin
  LFixture := TFixture.Create;
  LProbe := TProbe.Create;
  try
    LPointer := nil;
    LSource := LFixture.Store.Snapshot;
    LPage := LFixture.View.Snapshot;
    LRows := LFixture.View.Selection.Snapshot;
    VisibleColumns(LFixture.View, LSerial, LFields);
    Check((LPage.Count = 3) and (LPage.ItemAt(0).Ref.ID = 'b') and
      (LPage.ItemAt(2).Ref.ID = 'a') and (Length(LFields) = 3) and
      (LFields[0].Name = 'Name') and (LFields[2].Name = 'Score'),
      'actual filtered page and both logical pins determine endpoint order');
    Check((LPointer = nil),
      'default pointer proposal has no implicit cell or device authority');
    LOrigin := Cell('a', LFields[2]);
    LTarget := Cell('b', LFields[0]);
    LOtherFields := Copy(LFields, 0, Length(LFields));
    LPointer := NewNyxCollectionRangePointer(LSource, LPage, LOtherFields,
      LOrigin, LTarget, nil, ncrpReplace, LSerial, True, nccleCRLF,
      TNyxCollectionClipboardLimits.Defaults);
    Check(LPointer.Defined and LPointer.Anchor.Same(LOrigin) and
      LPointer.Focus.Same(LTarget) and (LPointer.ColumnSerial = LSerial),
      'reverse pointer endpoints stay exact without a keyboard approximation');
    LSaved := LPointer.Copy;
    LOtherFields[0] := NyxColumnIdentity(NyxTextField('Hidden'));
    Check(LPointer.FieldAt(0).Same(LFields[0]) and LSaved.FieldAt(0).Same(LFields[0]),
      'proposal copies expose no mutable caller field vector');
    LState := NewNyxCollectionCellRangeSelection;
    Check(Supports(LState, INyxCollectionCellRangePointerPreparation, LPreparation),
      'the SAME range-selection owner exposes additive pointer preparation');
    LCommand := Prepare(ncrpReplace, LOrigin, LTarget, nil);
    Check(LCommand.Current and (LState.Selection = nil),
      'complete RAW admission prepares without replacing accepted cells');
    Check(not LCommand.Complete(False) and not LCommand.Current and
      not LCommand.Complete(True) and (LState.Selection = nil),
      'consumed first completion cannot be retried as an allowed pointer default');
    LCommand.Retire;
    LCommand := Prepare(ncrpReplace, LOrigin, LTarget, nil);
    Check(LCommand.Complete(True), 'one original allowed pointer candidate installs');
    LCurrent := LState.Selection;
    LExpected := NewNyxCollectionCellRange(LSource, LPage, LFields, LOrigin, LTarget,
      LSerial, True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
    Check(LCurrent.Current and (LCurrent.Capture.Payload.Text = LExpected.Payload.Text) and
      (LCurrent.Capture.Payload.CellValue(2, 0).TextValue = CFull) and
      (LCurrent.Capture.Payload.RowCount = 3) and (LCurrent.Capture.Payload.FieldCount = 3),
      'existing SelectedPage producer supplies exact Unicode NUL newlines headers and typed TSV');
    Check(not LCommand.Complete(True) and (LState.Selection = LCurrent),
      'successful pointer completion is single-use too');
    LCommand.Retire;
    Check(LCurrent.Current, 'retiring an installed envelope does not revoke accepted selection');
    LPrevious := LCurrent;
    LCommand := Prepare(ncrpExtend, LPrevious.Capture.Focus, Cell('c🌙', LFields[1]), LPrevious);
    Check(LCommand.Proposal.Anchor.Same(LOrigin) and LPrevious.Current,
      'extension preserves the original directional anchor before callbacks');
    Check(LCommand.Complete(True) and not LPrevious.Current,
      'extension installs once through the existing publication and retires the old token');
    LCurrent := LState.Selection;
    LCommand := Prepare(ncrpExtend, LCurrent.Capture.Focus, LTarget, LCurrent);
    LCommand.Retire;
    Check(LCurrent.Current and (LState.Selection = LCurrent) and not LCommand.Complete(True),
      'abandoning a fully prepared endpoint preserves the prior rectangle');
    LCommand := Prepare(ncrpExtend, LCurrent.Capture.Focus, LTarget, LCurrent);
    LOther := Prepare(ncrpExtend, LCurrent.Capture.Focus, Cell('a', LFields[0]), LCurrent);
    Check(not LCommand.Current and not LCommand.Complete(True) and LOther.Current,
      'newer preparation defeats the older request without installing either');
    LCommand.Retire;
    Check(LOther.Complete(True), 'retiring older preparation leaves the newer command usable');
    LCurrent := LState.Selection;
    LOther.Retire;
    LCommand := Prepare(ncrpExtend, LCurrent.Capture.Focus, LTarget, LCurrent);
    LPrevious := LState.Select(LSource, LPage, LFields, LTarget, LTarget,
      LSerial, False, nccleLF, TNyxCollectionClipboardLimits.Defaults);
    Check(not LCommand.Current and not LCommand.Complete(True) and
      (LState.Selection = LPrevious) and LPrevious.Current,
      'a nested public selection survives the original pointer completion');
    LCommand.Retire;
    LState.Clear;
    LRefused := False;
    try
      LPointer := NewNyxCollectionRangePointer(LSource, LPage, LFields, LOrigin,
        Cell('off page', LFields[0]), nil, ncrpReplace, LSerial, False, nccleLF,
        TNyxCollectionClipboardLimits.Defaults);
    except
      on E: ENyxCollectionRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LPointer.Focus.Same(LSaved.Focus),
      'off-page endpoints refuse atomically without replacing a caller proposal');
    LRefused := False;
    try
      Prepare(ncrpReplace, LOrigin, Cell('b', NyxColumnIdentity(NyxTextField('Hidden'))), nil);
    except
      on E: ENyxCollectionRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LState.Selection = nil), 'hidden fields cannot enter pointer range membership');
    LOtherFields := Copy(LFields, 0, Length(LFields));
    LOtherFields[1] := NyxColumnIdentity(NyxTextField('Flag'));
    LRefused := False;
    try
      NewNyxCollectionRangePointer(LSource, LPage, LOtherFields, LOrigin, LTarget,
        nil, ncrpReplace, LSerial, False, nccleLF, TNyxCollectionClipboardLimits.Defaults);
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'endpoint fields must match the exact schema scalar family');
    LPrevious := LState.Select(LSource, LPage, LFields, LTarget, LTarget,
      LSerial, False, nccleLF, TNyxCollectionClipboardLimits.Defaults);
    LCommand := Prepare(ncrpReplace, LOrigin, LTarget, LPrevious);
    LSavedCommand := LCommand;
    LRefused := False;
    try
      LCommand := LPreparation.PreparePointer(LSource, LPage, LFields, LOrigin,
        LTarget, LPrevious, ncrpReplace, LSerial, True, nccleCRLF,
        TNyxCollectionClipboardLimits.Defaults.UTF8Bytes(1));
    except
      on E: ENyxCollectionClipboard do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LCommand = LSavedCommand) and
      (LState.Selection = LPrevious) and LPrevious.Current,
      'full output-byte refusal changes no selection and preserves the caller envelope');
    LCommand.Retire;
    LSavedCommand := nil;
    LState.Clear;
    LProxy := TReadPage.Create;
    LProxyLease := LProxy;
    LProxy.Inner := LPage;
    LProbe.State := LState;
    LProbe.Source := LSource;
    LProbe.Page := LProxyLease;
    LProbe.Fields := Copy(LFields, 0, Length(LFields));
    LProbe.Cell := LTarget;
    LProbe.Serial := LSerial;
    LProxy.OnRead := {$ifdef PAS2JS}@{$endif}LProbe.Clear;
    LRefused := False;
    try
      LPreparation.PreparePointer(LSource, LProxyLease, LFields, LOrigin, LTarget,
        nil, ncrpReplace, LSerial, False, nccleLF, TNyxCollectionClipboardLimits.Defaults);
    except
      on E: ENyxCollectionRangeStale do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProbe.Calls = 1) and (LState.Selection = nil),
      'nested nil-to-nil Clear during a real page getter defeats the entered reservation');
    LProxy.OnRead := {$ifdef PAS2JS}@{$endif}LProbe.SelectOne;
    LRefused := False;
    try
      LPreparation.PreparePointer(LSource, LProxyLease, LFields, LOrigin, LTarget,
        nil, ncrpReplace, LSerial, False, nccleLF, TNyxCollectionClipboardLimits.Defaults);
    except
      on E: ENyxCollectionRangeStale do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProbe.Calls = 2) and LProbe.Newer.Current and
      (LState.Selection = LProbe.Newer),
      'a real nested selection getter wins without older endpoint overwrite');
    LCurrent := LProbe.Newer;
    LRefused := False;
    try
      LPreparation.PreparePointer(LSource, LProxyLease, LFields, LCurrent.Capture.Focus,
        LTarget, LCurrent, ncrpExtend, LSerial + 1, False, nccleLF,
        TNyxCollectionClipboardLimits.Defaults);
    except
      on E: ENyxCollectionRangeStale do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LCurrent.Current, 'column serial mismatch cannot retarget an old pointer extension');
    LRefused := False;
    try
      LPreparation.PreparePointer(LSource, LPage, LFields, LCurrent.Capture.Focus,
        LTarget, LCurrent, ncrpExtend, LSerial, False, nccleLF,
        TNyxCollectionClipboardLimits.Defaults);
    except
      on E: ENyxCollectionRangeStale do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LCurrent.Current,
      'equal-byte but independently supplied page cannot replace original extension provenance');
    LRefused := False;
    try
      LPreparation.PreparePointer(LSource, LProxyLease, LFields, LOrigin,
        LTarget, LCurrent, ncrpExtend, LSerial, False, nccleLF,
        TNyxCollectionClipboardLimits.Defaults);
    except
      on E: ENyxCollectionRangeStale do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LCurrent.Current,
      'pointer extension cannot borrow a different original focused endpoint');
    LRefused := False;
    LBefore := LProxy.RowReads;
    try
      NewNyxCollectionRangePointer(LSource, LProxyLease, LFields, LOrigin, LTarget,
        nil, ncrpReplace, LSerial, False, nccleLF,
        TNyxCollectionClipboardLimits.Defaults.ScannedRows(1));
    except
      on E: ENyxCollectionRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProxy.RowReads = LBefore),
      'pointer proposal refuses an excessive page before indexed row reads');
    LState.Revoke;
    LPreparation := nil;
    LState := nil;
    CreateProbeOwner(LProbe);
    LPrevious := SelectProbe(LProbe);
    LProxy.OnRead := {$ifdef PAS2JS}@{$endif}LProbe.WithdrawOwners;
    LOther := EnterWithWithdrawal(LProbe, LPrevious);
    Check((LProbe.State = nil) and (LProbe.Preparation = nil) and LOther.Current and LPrevious.Current,
      'entered preparation retains its receiver after both caller interfaces withdraw');
    LPointer := LOther.Proposal;
    LOther.Retire;
    Check(not LPrevious.Current and LPointer.Defined and LPointer.Focus.Same(LTarget),
      'retiring the final uninstalled command releases its owner while detached values stay usable' +
      ' retired=' + TNyxText(BoolToStr(not LPrevious.Current, True)) +
      ' defined=' + TNyxText(BoolToStr(LPointer.Defined, True)) +
      ' focus=' + TNyxText(BoolToStr(LPointer.Focus.Same(LTarget), True)));
    CheckOriginalOwners(LFixture, LSource, LPage, LRows);
    LFixture.ReleaseOwners;
    Check(not LFixture.ViewToken.Connected and not LFixture.StoreToken.Connected and
      (LFixture.Changes = 0) and (LPointer.Source = LSource) and
      (LExpected.Payload.CellValue(2, 0).TextValue = CFull),
      'retained pointer values and RAW output keep no live view/store or ordinary row authority');
    Check(LRows.Contains(NyxItem(Key, 'off page')),
      'the explicitly hidden selected row remains an independent captured row selection');
  finally

    if LProxyLease <> nil then
    begin
      LProxy.OnRead := nil;
    end;

    if LCommand <> nil then
    begin
      LCommand.Retire;
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
    document.body.setAttribute('data-range-pointer-tests', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed pointer endpoint checks; no UI or host effects');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-range-pointer-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
