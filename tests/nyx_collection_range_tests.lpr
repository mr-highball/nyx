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
program nyx_collection_range_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.state, nyx.collections,
  nyx.collections.selection, nyx.collections.query, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.arrangement,
  nyx.collections.arrangement.controls,
  nyx.collections.clipboard, nyx.collections.range,
  nyx.collections.range.actions, nyx.scheduler
  {$ifdef PAS2JS}, Web{$endif};

type
  { Real subscriptions observe that capture is read-only and that retaining a
    rectangle/payload cannot keep the actual view/store owners connected. }
  TReceiver = class
  public
    ViewChanges: Integer;
    StoreChanges: Integer;
    procedure ViewChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure StoreChanged(const AStore: INyxCollection;
      const AChanges: INyxCollectionChanges);
  end;

  { Borrowed one-shot getter callback is paired with its explicit owner lease
    by TCallbackPage. It exercises real snapshot callouts during range capture. }
  TRangeRead = procedure of object;

  { Read-only snapshot proxy delegates every actual data fact. Its first ItemAt
    can install a newer selection or withdraw the caller's last owner lease;
    the rectangle producer must not assume snapshot reads are callback-free. }
  TCallbackPage = class(TInterfacedObject, INyxCollectionSnapshot)
  public
    Inner: INyxCollectionSnapshot;
    OnRead: TRangeRead;
    ReadOwner: IInterface;
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

  { Closed reentry modes distinguish a genuinely newer accepted rectangle from
    clearing, terminal retirement and withdrawal of the caller's owner lease. }
  TRangeReentryKind = (nrrSelect, nrrClear, nrrRevoke, nrrReleaseOwner);

  { Owns only test entry interfaces and immutable data. The proxy retains this
    actor through its one outward call; no live store/view is retained here. }
  TRangeReentry = class(TInterfacedObject)
  public
    Controller: INyxCollectionCellRangeSelection;
    Source: INyxCollectionSnapshot;
    Page: INyxCollectionSnapshot;
    Fields: TNyxCollectionClipboardFields;
    Cell: TNyxCollectionCellIdentity;
    Serial: Integer;
    Kind: TRangeReentryKind;
    Calls: Integer;
    Newer: INyxCollectionCellRangeIntent;
    procedure Enter;
  end;

const
  CName: TNyxText = 'Name" 🌙';
  CFull: TNyxText = 'path 🌙' + #9 + '"quote"' + #13#10 + 'tail' + #0 + 'end';

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

procedure TReceiver.ViewChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(ViewChanges);
end;

procedure TReceiver.StoreChanged(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
  Inc(StoreChanges);
end;

function TCallbackPage.GetKey: TNyxCollectionRef;
begin
  Result := Inner.Key;
end;

function TCallbackPage.GetSchema: TNyxCollectionSchema;
begin
  Result := Inner.Schema;
end;

function TCallbackPage.GetCount: Integer;
begin
  Result := Inner.Count;
end;

function TCallbackPage.GetRevision: Integer;
begin
  Result := Inner.Revision;
end;

function TCallbackPage.GetDataBytes: Integer;
begin
  Result := Inner.DataBytes;
end;

function TCallbackPage.ItemAt(AIndex: Integer): TNyxCollectionItem;
var
  LInner: INyxCollectionSnapshot;
  LRead: TRangeRead;
  LOwner: IInterface;
begin
  LInner := Inner;
  LRead := OnRead;
  LOwner := ReadOwner;
  OnRead := nil;
  ReadOwner := nil;

  if Assigned(LRead) then
  begin
    LRead();
  end;
  Result := LInner.ItemAt(AIndex);
end;

function TCallbackPage.Item(const ARef: TNyxItemRef): TNyxCollectionItem;
begin
  Result := Inner.Item(ARef);
end;

function TCallbackPage.IndexOf(const ARef: TNyxItemRef): Integer;
begin
  Result := Inner.IndexOf(ARef);
end;

function TCallbackPage.Has(const ARef: TNyxItemRef): Boolean;
begin
  Result := Inner.Has(ARef);
end;

procedure TRangeReentry.Enter;
begin
  Inc(Calls);
  case Kind of
    nrrSelect:
    begin
      Newer := Controller.Select(Source, Page, Fields, Cell, Cell, Serial,
        False, nccleLF, TNyxCollectionClipboardLimits.Defaults);
    end;
    nrrClear:
    begin
      Controller.Clear;
    end;
    nrrRevoke:
    begin
      Controller.Revoke;
    end;
    nrrReleaseOwner:
    begin
      Controller := nil;
    end;
  end;
end;

{ End factory/receiver interface temporaries before the actual owner-retirement
  assertion. The public field is still withdrawn by the real ItemAt callback;
  a compiler-owned call expression must not artificially extend its lifetime. }
procedure CreateActorSelection(AActor: TRangeReentry);
begin
  AActor.Controller := NewNyxCollectionCellRangeSelection;
end;

function SelectFromActor(AActor: TRangeReentry;
  const APage: INyxCollectionSnapshot;
  const AAnchor, AFocus: TNyxCollectionCellIdentity): INyxCollectionCellRangeIntent;
begin
  Result := AActor.Controller.Select(AActor.Source, APage, AActor.Fields,
    AAnchor, AFocus, AActor.Serial, True, nccleCRLF,
    TNyxCollectionClipboardLimits.Defaults);
end;

function Fields: TNyxCollectionClipboardFields;
begin
  Result := nil;
  SetLength(Result, 4);
  Result[0] := NyxColumnIdentity(NyxTextField(CName));
  Result[1] := NyxColumnIdentity(NyxBooleanField('Flag'));
  Result[2] := NyxColumnIdentity(NyxIntegerField('Score'));
  Result[3] := NyxColumnIdentity(NyxNumberField('Rate'));
end;

{ End compiler-owned fluent self-return temporaries before later owner-release
  assertions. These helpers do not suppress or replace any model operation. }
procedure ConfigureQuery(const AView: INyxCollectionView;
  const AQuery: TNyxCollectionQuery);
begin
  AView.ConfigureQuery(AQuery);
end;

procedure UpdateStore(const AStore: INyxCollection;
  const AItem: TNyxCollectionItem);
begin
  AStore.Update(AItem);
end;

{ The optional columns facet may return its live view. Keep that temporary and
  the launch capture inside this bounded read, before owner-retirement checks. }
procedure CaptureVisibleColumns(const AView: INyxCollectionView;
  out ASerial: Integer; out AFields: TNyxCollectionClipboardFields);
var
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LObservation: TNyxColumnLayoutObservation;
begin
  LColumns := NyxCollectionColumns(AView);
  LCapture := LColumns.CaptureColumns;
  LObservation := TNyxColumnLayoutObservation.Read(LCapture);
  ASerial := LCapture.Serial;
  AFields := NyxCollectionClipboardFields(LObservation, nccfVisiblePresentation);
end;

{ Each refusal calls the actual rectangle/SelectedPage producer. No clipboard
  host is constructed, and an earlier immutable output remains independently
  available after the failed assignment. }
procedure Refuse(const ASource, APage: INyxCollectionSnapshot;
  const AFields: array of TNyxColumnIdentity;
  const AAnchor, AFocus: TNyxCollectionCellIdentity; ASerial: Integer;
  const ALimits: TNyxCollectionClipboardLimits; const AReason: TNyxText);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    NewNyxCollectionCellRange(ASource, APage, AFields, AAnchor, AFocus,
      ASerial, True, nccleCRLF, ALimits);
  except
    on E: ENyxCollectionClipboard do
    begin
      LRefused := True;
    end;
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
end;

{ The same accepted page/scalars exercise a separate optional cell selection.
  Every nested action comes from a real ItemAt callout during pure capture;
  no private selection fields or platform clipboard effects are simulated. }
procedure SelectionCases(const ASource, APage: INyxCollectionSnapshot;
  const AFields: TNyxCollectionClipboardFields;
  const AAnchor, AFocus: TNyxCollectionCellIdentity; ASerial: Integer);
var
  LState: INyxCollectionCellRangeSelection;
  LOther: INyxCollectionCellRangeSelection;
  LFirst: INyxCollectionCellRangeIntent;
  LSecond: INyxCollectionCellRangeIntent;
  LCandidate: INyxCollectionCellRangeIntent;
  LReady: INyxCancellationReadiness;
  LActor: TRangeReentry;
  LActorLease: IInterface;
  LProxy: TCallbackPage;
  LProxyLease: INyxCollectionSnapshot;
  LRefused: Boolean;
  LText: TNyxText;

  procedure Proxy(AKind: TRangeReentryKind);
  begin
    LActor.Kind := AKind;
    LProxy := TCallbackPage.Create;
    LProxyLease := LProxy;
    LProxy.Inner := APage;
    LProxy.OnRead := {$ifdef PAS2JS}@{$endif}LActor.Enter;
    LProxy.ReadOwner := LActorLease;
  end;

  procedure RefuseOuter;
  begin
    LRefused := False;
    try
      LCandidate := LState.Select(ASource, LProxyLease, AFields,
        AAnchor, AFocus, ASerial, True, nccleCRLF,
        TNyxCollectionClipboardLimits.Defaults);
    except
      on E: ENyxCollectionRange do
      begin
        LRefused := True;
      end;
    end;
  end;

begin
  LState := NewNyxCollectionCellRangeSelection;
  LOther := NewNyxCollectionCellRangeSelection;
  Check(LState.Selection = nil, 'new opt-in cell selection is inactive and owns no data');
  LFirst := LState.Select(ASource, APage, AFields, AAnchor, AFocus, ASerial,
    True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
  LText := LFirst.Capture.Payload.Text;
  LReady := LFirst.Readiness;
  LReady.CheckReady;
  Check(LFirst.Current and (LFirst.Source = ASource) and (LFirst.Page = APage) and
    LFirst.Capture.Anchor.Same(AAnchor) and (LState.Selection = LFirst),
    'accepted range intent carries original immutable provenance and independent lifetime');
  LSecond := LState.Select(ASource, APage, AFields, AFocus, AFocus, ASerial,
    False, nccleLF, TNyxCollectionClipboardLimits.Defaults);
  Check(not LFirst.Current and LSecond.Current and (LState.Selection = LSecond) and
    (LFirst.Capture.Payload.Text = LText),
    'newer range retires only old authority while retained exact bytes stay available');
  LRefused := False;
  try
    LReady.CheckReady;
  except
    on E: ENyxSchedule do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'captured old flags-only range readiness cannot revive after replacement');
  LCandidate := LSecond;
  LRefused := False;
  try
    LCandidate := LState.Select(ASource, APage, [], AAnchor, AFocus, ASerial,
      True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
  except
    on E: ENyxCollectionRange do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LCandidate = LSecond) and (LState.Selection = LSecond) and
    LSecond.Current, 'refused range admission preserves caller result and previous accepted selection');
  LOther.Select(ASource, APage, AFields, AAnchor, AFocus, ASerial,
    False, nccleLF, TNyxCollectionClipboardLimits.Defaults);
  LState.Clear;
  LState.Clear;
  Check((LState.Selection = nil) and not LSecond.Current and LOther.Selection.Current,
    'idempotent clear revokes only this cell selection and leaves a peer owner independent');
  LFirst := LState.Select(ASource, APage, AFields, AAnchor, AFocus, ASerial,
    True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
  Check(LFirst.Current and not LSecond.Current,
    'fresh selection after clear cannot revive an older intent');
  LActor := TRangeReentry.Create;
  LActorLease := LActor;
  LActor.Controller := LState;
  LActor.Source := ASource;
  LActor.Page := APage;
  LActor.Fields := Copy(AFields);
  LActor.Cell := AFocus;
  LActor.Serial := ASerial;
  LCandidate := LFirst;
  Proxy(nrrSelect);
  RefuseOuter;
  Check(LRefused and (LCandidate = LFirst) and (LActor.Calls = 1) and
    (LState.Selection = LActor.Newer) and LActor.Newer.Current and
    not LFirst.Current and (LActor.Newer.Capture.Payload.RowCount = 1),
    'nested actual snapshot getter selection wins without outer replacement or mixed endpoints');
  LSecond := LActor.Newer;
  Proxy(nrrClear);
  RefuseOuter;
  Check(LRefused and (LActor.Calls = 2) and (LState.Selection = nil) and not LSecond.Current,
    'nested getter clear stays cleared instead of accepting the older captured rectangle');
  LFirst := LState.Select(ASource, APage, AFields, AAnchor, AFocus, ASerial,
    True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
  Proxy(nrrRevoke);
  RefuseOuter;
  Check(LRefused and (LActor.Calls = 3) and (LState.Selection = nil) and not LFirst.Current,
    'terminal retirement during a getter defeats the original reservation');
  LRefused := False;
  try
    LState.Select(ASource, APage, AFields, AAnchor, AFocus, ASerial,
      True, nccleCRLF, TNyxCollectionClipboardLimits.Defaults);
  except
    on E: ENyxCollectionRange do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'retired range owner explicitly refuses later fresh selection');
  CreateActorSelection(LActor);
  LState := nil;
  LActor.Newer := nil;
  Proxy(nrrReleaseOwner);
  { This is the caller's actual last owner field, not a copied local receiver.
    Select's own entry lease must survive its withdrawal during ItemAt. }
  LCandidate := SelectFromActor(LActor, LProxyLease, AAnchor, AFocus);
  Check((LActor.Controller = nil) and (LActor.Calls = 4) and
    not LCandidate.Current and (LCandidate.Capture.Payload.Text = LText),
    'entered selection survives caller withdrawal and final-owner retirement leaves retained data inert' +
    ' (withdrawn=' + BoolToStr(LActor.Controller = nil, True) +
    ', calls=' + IntToStr(LActor.Calls) + ', current=' + BoolToStr(LCandidate.Current, True) +
    ', sameText=' + BoolToStr(LCandidate.Capture.Payload.Text = LText, True) + ')');
  LOther.Revoke;
  LOther.Clear;
  Check(LOther.Selection = nil,
    'clearing a retired peer cannot reintroduce its former selection');
end;

procedure Run;
var
  LKey: TNyxCollectionRef;
  LA: TNyxItemRef;
  LB: TNyxItemRef;
  LC: TNyxItemRef;
  LD: TNyxItemRef;
  LSchema: TNyxCollectionSchema;
  LSpec: TNyxCollectionViewSpec;
  LStore: INyxCollection;
  LOther: INyxCollection;
  LView: INyxCollectionView;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LOtherPage: INyxCollectionSnapshot;
  LViewToken: INyxCollectionViewSubscription;
  LStoreToken: INyxCollectionSubscription;
  LReceiver: TReceiver;
  LSelection: TNyxCollectionSelectionSnapshot;
  LFields: TNyxCollectionClipboardFields;
  LAnchor: TNyxCollectionCellIdentity;
  LFocus: TNyxCollectionCellIdentity;
  LRange: INyxCollectionCellRangeCapture;
  LBounds: INyxCollectionCellRangeBounds;
  LReverse: INyxCollectionCellRangeCapture;
  LSingle: INyxCollectionCellRangeCapture;
  LCandidate: INyxCollectionCellRangeCapture;
  LPayload: INyxCollectionClipboardText;
  LRaw: INyxCollectionClipboardText;
  LExpected: TNyxText;
  LLimits: TNyxCollectionClipboardLimits;
  LViewsBefore: Integer;
  LStoresBefore: Integer;
  LSerial: Integer;
  LRefused: Boolean;
begin
  LKey := NyxCollection('range source 🌙');
  LA := NyxItem(LKey, 'a');
  LB := NyxItem(LKey, 'b');
  LC := NyxItem(LKey, 'c');
  LD := NyxItem(LKey, 'filtered out');
  LSchema := NyxCollectionSchema.Text(NyxTextField(CName), '')
    .Boolean(NyxBooleanField('Flag'), False).Integer(NyxIntegerField('Score'), 0)
    .Number(NyxNumberField('Rate'), 0).Text(NyxTextField('Hidden'), 'hidden');
  LStore := NewNyxCollection(LKey, LSchema, [
    NyxCollectionItem(LA).WithValue(NyxTextField(CName), CFull)
      .WithValue(NyxBooleanField('Flag'), True).WithValue(NyxIntegerField('Score'), 2)
      .WithValue(NyxNumberField('Rate'), 1.25),
    NyxCollectionItem(LB).WithValue(NyxTextField(CName), 'B')
      .WithValue(NyxIntegerField('Score'), 4),
    NyxCollectionItem(LC).WithValue(NyxTextField(CName), 'C')
      .WithValue(NyxIntegerField('Score'), 3),
    NyxCollectionItem(LD).WithValue(NyxTextField(CName), 'D')
      .WithValue(NyxIntegerField('Score'), 1)]);
  LSpec := NyxCollectionView(LKey).Selection(nsmMultiple)
    .Column(NyxTextField(CName), 'Formatted title')
    .Column(NyxBooleanField('Flag'), 'Flag')
    .Column(NyxIntegerField('Score'), 'Score')
    .Column(NyxNumberField('Rate'), 'Rate')
    .Column(NyxTextField('Hidden'), 'Hidden')
      .ColumnLayout(NyxColumnLayout.Visible(False));
  LView := NewNyxCollectionView(LStore, LSpec, cpTable);
  ConfigureQuery(LView, NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('Score')).AtLeast(2))
    .OrderBy(NyxIntegerField('Score'), nsdDescending));
  LView.SetSelection([LB, LD], LB, LB);
  LSelection := LView.Selection.Snapshot;
  LSource := LStore.Snapshot;
  LPage := LView.Snapshot;
  Check((LPage.Count = 3) and (LPage.ItemAt(0).Ref.ID = LB.ID) and
    (LPage.ItemAt(1).Ref.ID = LC.ID) and (LPage.ItemAt(2).Ref.ID = LA.ID),
    'real filtered/reordered page differs from source and hidden selected-row membership');
  LReceiver := TReceiver.Create;
  LViewToken := LView.Subscribe({$ifdef PAS2JS}@{$endif}LReceiver.ViewChanged);
  LStoreToken := LStore.Subscribe({$ifdef PAS2JS}@{$endif}LReceiver.StoreChanged);
  try
    LViewsBefore := LReceiver.ViewChanges;
    LStoresBefore := LReceiver.StoreChanges;
    LLimits := TNyxCollectionClipboardLimits.Defaults;
    CaptureVisibleColumns(LView, LSerial, LFields);
    Check((Length(LFields) = 4) and (LView.Spec.Count = 5) and
      not LView.Spec.ColumnAt(4).LayoutPolicy.IsVisible,
      'actual column observation excludes the genuinely authored hidden field');
    LAnchor := NyxCollectionCellIdentity(LC, LFields[2]);
    LFocus := NyxCollectionCellIdentity(LA, LFields[0]);
    LRange := NewNyxCollectionCellRange(LSource, LPage, LFields, LAnchor, LFocus,
      LSerial, True, nccleCRLF, LLimits);
    LPayload := LRange.Payload;
    LExpected := '"Name"" 🌙"' + #9 + 'Flag' + #9 + 'Score' + #13#10 +
      'C' + #9 + 'false' + #9 + '3' + #13#10 +
      '"path 🌙' + #9 + '""quote""' + #13#10 + 'tail' + #0 + 'end"' +
      #9 + 'true' + #9 + '2';
    Check((LPayload.Text = LExpected) and (LPayload.ByteCount = NyxUTF8ByteCount(LExpected)),
      'rectangle reuses exact TSV escaping/header/newline rules for Unicode, quotes and embedded NUL');
    Check((LPayload.RowCount = 2) and (LPayload.FieldCount = 3) and
      (LPayload.ItemAt(0).ID = LC.ID) and (LPayload.ItemAt(1).ID = LA.ID),
      'inclusive rectangle follows actual page order, not source or unrelated row selection');
    Check(LPayload.FieldAt(0).Same(LFields[0]) and LPayload.FieldAt(2).Same(LFields[2]) and
      (LPayload.CellValue(0, 2).Kind = nskInteger) and
      (LPayload.CellValue(0, 2).IntegerValue = 3) and
      LPayload.CellValue(1, 1).BooleanValue and
      (LPayload.CellValue(1, 0).TextValue = CFull),
      'complete typed fields/scalars remain accepted RAW facts');
    Check(LRange.Anchor.Same(LAnchor) and LRange.Focus.Same(LFocus) and
      (LRange.ColumnSerial = LSerial) and LRange.IncludeHeaders and
      (LRange.LineEnding = nccleCRLF), 'direction and explicit output/column provenance are retained');
    Check(Supports(LRange, INyxCollectionCellRangeBounds, LBounds) and
      (LBounds.AnchorRow = 1) and (LBounds.FocusRow = 2) and
      (LBounds.FirstRow = 1) and (LBounds.LastRow = 2),
      'optional paint bounds describe the exact captured page without a live row getter');
    Check(LRange.Contains(LA, LFields[0]) and LRange.Contains(LC, LFields[2]) and
      not LRange.Contains(LB, LFields[0]) and not LRange.Contains(LA, LFields[3]) and
      not LRange.Contains(LA, NyxColumnIdentity(NyxTextField('Hidden'))),
      'range membership is the exact rectangle, excluding other rows/fields and hidden columns');
    Check(not LRange.Contains(NyxItem(NyxCollection('foreign'), 'a'), LFields[0]) and
      not LRange.Contains(Default(TNyxItemRef), LFields[0]) and
      not LRange.Contains(LA, Default(TNyxColumnIdentity)),
      'optional/foreign membership reads cannot impersonate the captured cell');
    LReverse := NewNyxCollectionCellRange(LSource, LPage, LFields, LFocus, LAnchor,
      LSerial, True, nccleCRLF, LLimits);
    Check((LReverse.Payload.Text = LExpected) and LReverse.Anchor.Same(LFocus) and
      LReverse.Focus.Same(LAnchor), 'reverse gesture normalizes output without losing directional endpoints');
    LSingle := NewNyxCollectionCellRange(LSource, LPage, LFields, LFocus, LFocus,
      LSerial, False, nccleLF, LLimits);
    LRaw := NewNyxCollectionClipboardCell(LSource, LA, LFields[0], LLimits);
    Check((LSingle.Payload.RowCount = 1) and (LSingle.Payload.FieldCount = 1) and
      (LSingle.Payload.Text <> LRaw.Text) and (LRaw.Text = CFull) and
      (LSingle.Payload.CellValue(0, 0).TextValue = CFull),
      'singleton range remains explicit TSV while established one-cell RAW semantics stay unchanged');
    LFields[0] := LFields[3];
    LAnchor := LFocus;
    Check(LRange.Payload.FieldAt(0).Same(NyxColumnIdentity(NyxTextField(CName))) and
      (LRange.Anchor.Item.ID = LC.ID) and (LRange.Payload.Text = LExpected),
      'caller field-vector/endpoint replacements cannot alter admitted range or bytes');
    LFields := Fields;
    LAnchor := NyxCollectionCellIdentity(LC, LFields[2]);
    Check(LView.Selection.Snapshot.SameState(LSelection) and
      (LReceiver.ViewChanges = LViewsBefore) and (LReceiver.StoreChanges = LStoresBefore) and
      (LStore.Snapshot = LSource) and (LView.Snapshot = LPage),
      'range capture never mutates row selection, query, accepted data or notifications');
    Refuse(LSource, LPage, LFields, LAnchor,
      NyxCollectionCellIdentity(LD, LFields[0]), LSerial, LLimits,
      'filtered-out selected row cannot silently become an off-page range endpoint');
    Refuse(LSource, LPage, LFields, LAnchor,
      NyxCollectionCellIdentity(NyxItem(LKey, 'missing'), LFields[0]), LSerial, LLimits,
      'missing stable row identity refuses');
    Refuse(LSource, LPage, LFields, LAnchor,
      NyxCollectionCellIdentity(LA, NyxColumnIdentity(NyxTextField('Hidden'))), LSerial, LLimits,
      'hidden field endpoint refuses rather than widening the visible rectangle');
    Refuse(LSource, LPage, LFields, LAnchor,
      NyxCollectionCellIdentity(NyxItem(NyxCollection('foreign'), 'a'), LFields[0]), LSerial, LLimits,
      'same row ID from another collection refuses');
    Refuse(LSource, LPage, LFields, Default(TNyxCollectionCellIdentity), LFocus,
      LSerial, LLimits, 'absent endpoint refuses without coordinate fallback');
    Refuse(LSource, LPage, LFields, LAnchor, LFocus, -1, LLimits,
      'negative column cohort refuses');
    Refuse(LSource, LPage, LFields, LAnchor, LFocus, High(Integer), LLimits,
      'saturated column cohort cannot revive as a new range');
    LFields[1] := LFields[0];
    Refuse(LSource, LPage, LFields, LAnchor, LFocus, LSerial, LLimits,
      'duplicate visible field identity refuses before output');
    LFields := Fields;
    LFields[1] := NyxColumnIdentity(NyxIntegerField(CName));
    Refuse(LSource, LPage, LFields, LAnchor, LFocus, LSerial, LLimits,
      'same field name with another scalar family refuses');
    LFields := Fields;
    Refuse(LSource, LPage, LFields, LAnchor, LFocus, LSerial, LLimits.Rows(1),
      'range output rows are bounded');
    Refuse(LSource, LPage, LFields, LAnchor, LFocus, LSerial, LLimits.Fields(2),
      'range output typed fields are bounded');
    Refuse(LSource, LPage, LFields, LAnchor, LFocus, LSerial, LLimits.Cells(5),
      'range output cell product is bounded before allocation');
    Refuse(LSource, LPage, LFields, LAnchor, LFocus, LSerial, LLimits.UTF8Bytes(1),
      'escaped TSV UTF-8 output is bounded');
    Refuse(LSource, LPage, LFields, LAnchor, LFocus, LSerial, LLimits.ScannedRows(4),
      'page endpoint/membership work is bounded independently from output size');
    LOther := NewNyxCollection(LKey, LSchema, [
      NyxCollectionItem(LA).WithValue(NyxTextField(CName), 'changed')
        .WithValue(NyxIntegerField('Score'), 2),
      LSource.Item(LB), LSource.Item(LC)]);
    LOtherPage := LOther.Snapshot;
    Refuse(LSource, LOtherPage, LFields, LAnchor, LFocus, LSerial, LLimits,
      'same key/revision page with changed accepted scalars refuses');
    LOther := NewNyxCollection(LKey,
      NyxCollectionSchema.Integer(NyxIntegerField(CName), 0), [
        NyxCollectionItem(LA).WithValue(NyxIntegerField(CName), 1)]);
    LOtherPage := LOther.Snapshot;
    Refuse(LSource, LOtherPage, LFields, LAnchor, LFocus, LSerial, LLimits,
      'same key/revision page with another schema refuses');
    SelectionCases(LSource, LPage, LFields, LAnchor, LFocus, LSerial);
    Check(LView.Selection.Snapshot.SameState(LSelection) and
      (LReceiver.ViewChanges = LViewsBefore) and (LReceiver.StoreChanges = LStoresBefore) and
      (LStore.Snapshot = LSource) and (LView.Snapshot = LPage),
      'optional cell selection lifecycle does not publish ordinary row/query/data changes');
    LCandidate := LRange;
    LRefused := False;
    try
      LCandidate := NewNyxCollectionCellRange(LSource, LPage, [], LAnchor, LFocus,
        LSerial, True, nccleCRLF, LLimits);
    except
      on E: ENyxCollectionRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LCandidate = LRange) and (LCandidate.Payload.Text = LExpected),
      'failed complete capture leaves the caller previous range and exact output intact');
    UpdateStore(LStore, NyxCollectionItem(LA).WithValue(NyxTextField(CName), 'new accepted data'));
    Refuse(LStore.Snapshot, LPage, LFields, LAnchor, LFocus, LSerial, LLimits,
      'old captured page cannot combine with a newer accepted source revision');
    Check((LRange.Payload.Text = LExpected) and
      (LRange.Payload.CellValue(1, 0).TextValue = CFull),
      'later accepted edit cannot redirect retained range values or bytes');
    LSource := nil;
    LPage := nil;
    LOtherPage := nil;
    LOther := nil;
    LView := nil;
    LStore := nil;
    Check(not LViewToken.Connected and not LStoreToken.Connected and
      (LRange.Payload.Text = LExpected),
      'retained rectangle/payload own no actual view/store and allow real subscription retirement');
  finally
    LViewToken := nil;
    LStoreToken := nil;
    LView := nil;
    LStore := nil;
    LReceiver.Free;
  end;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-collection-range-tests', IntToStr(GChecks));
  {$else}
  WriteLn('PASS ', GChecks, ' collection cell-range capture checks; no host effects');
  {$endif}
end;

begin
  try
    Run;
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-collection-range-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
