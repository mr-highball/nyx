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
program nyx_collection_clipboard_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.bytes, nyx.state, nyx.collections,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.selection,
  nyx.collections.query, nyx.collections.paging, nyx.collections.arrangement,
  nyx.collections.arrangement.controls, nyx.collections.clipboard,
  nyx.scheduler, nyx.clipboard
  {$ifdef PAS2JS}, Web{$endif};

type
  { A no-effect authority runs the real shared managed terminal operation. The
    fixture can inspect exact handoff bytes without permission or OS mutation. }
  TWrite = class(TNyxClipboardWriteRequest)
  public
    function Admitted: Boolean;
    procedure Finish(AOutcome: TNyxClipboardWriteOutcome);
  end;

  { Runtime host is injected explicitly; no default target is constructed. }
  THost = class(TInterfacedObject, INyxClipboardWriteHost)
  public
    Text: TNyxText;
    Writes: Integer;
    Outcome: TNyxClipboardWriteOutcome;
    function BeginWriteText(const AText: TNyxText;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
  end;

  { Borrowed receiver and real view/store observer outlive every subscription.
    Counts distinguish read-only production from a hidden edit/query command. }
  TReceiver = class
  public
    Replies: Integer;
    ViewChanges: Integer;
    StoreChanges: Integer;
    Last: TNyxClipboardWriteResult;
    procedure Reply(const AResult: TNyxClipboardWriteResult);
    procedure ViewChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure StoreChanged(const AStore: INyxCollection;
      const AChanges: INyxCollectionChanges);
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxCollectionClipboard.Create(AReason);
  end;
  Inc(GChecks);
end;

function TWrite.Admitted: Boolean;
begin
  Result := Pending;
end;

procedure TWrite.Finish(AOutcome: TNyxClipboardWriteOutcome);
begin

  if AOutcome = ncwoWritten then
  begin
    Complete(AOutcome, FByteCount, '', nctrExactUTF8);
  end
  else
  begin
    Complete(AOutcome, 0);
  end;
end;

function THost.BeginWriteText(const AText: TNyxText;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
var
  LWrite: TWrite;
begin
  LWrite := TWrite.Create(AText, AScheduler, ACancellation, AReply);
  Result := LWrite;

  if not LWrite.Admitted then
  begin
    LWrite.Cancel;
    Exit;
  end;
  Inc(Writes);
  Text := AText;
  LWrite.Finish(Outcome);
end;

procedure TReceiver.Reply(const AResult: TNyxClipboardWriteResult);
begin
  Inc(Replies);
  Last := AResult;
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

{ Exercise refusal through the public producer with original immutable inputs;
  a rejected operation has no host/receiver to accidentally clear the clipboard. }
procedure RefuseRows(const ASource: INyxCollectionSnapshot;
  const ASelection: TNyxCollectionSelectionSnapshot;
  const AFields: array of TNyxColumnIdentity;
  AMembership: TNyxCollectionCopyMembership; const APage: INyxCollectionSnapshot;
  const ALimits: TNyxCollectionClipboardLimits; const AReason: TNyxText);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    NewNyxCollectionClipboardRows(ASource, ASelection, AFields, AMembership,
      APage, True, nccleLF, ALimits);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
end;

{ Fluent setup returns its live receiver. Both compilers can retain that return
  temporary until the containing procedure exits, including an assigned result.
  These bounded calls release every such temporary before Run intentionally
  withdraws the actual view/store owners and tests retained-payload independence. }
procedure ConfigureQuery(const AView: INyxCollectionView;
  const APolicy: TNyxCollectionQuery);
begin
  AView.ConfigureQuery(APolicy);
end;

procedure ConfigurePaging(const APages: INyxCollectionPages;
  const APolicy: TNyxCollectionPaging);
begin
  APages.ConfigurePaging(APolicy);
end;

procedure NextPage(const APages: INyxCollectionPages);
begin
  APages.NextPage;
end;

procedure ConfigureColumns(const AColumns: INyxCollectionColumns;
  const APreferences: TNyxColumnPreferences);
begin
  AColumns.ConfigureColumns(APreferences);
end;

procedure UpdateStore(const AStore: INyxCollection;
  const AItem: TNyxCollectionItem);
begin
  AStore.Update(AItem);
end;

procedure Run;
const
  CName: TNyxText = 'Name';
  CQuoted: TNyxText = 'Label"quoted';
  CFull: TNyxText = 'path 🌙' + #9 + '"quote"' + #13#10 + 'tail' + #0 + 'end';
  CHeaders: TNyxText = 'Name' + #9 + '"Label""quoted"' + #9 + 'Flag' + #9 + 'Score' + #9 + 'Rate';
  CFirst: TNyxText = '"path 🌙' + #9 + '""quote""' + #13#10 + 'tail' + #0 + 'end"' +
    #9 + 'plain' + #9 + 'true' + #9 + '-2147483648' + #9 + '1.25';
var
  LKey: TNyxCollectionRef;
  LA: TNyxItemRef;
  LB: TNyxItemRef;
  LC: TNyxItemRef;
  LSchema: TNyxCollectionSchema;
  LStore: INyxCollection;
  LOther: INyxCollection;
  LView: INyxCollectionView;
  LPages: INyxCollectionPages;
  LColumns: INyxCollectionColumns;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LCapture: INyxCollectionColumnsCapture;
  LSelection: TNyxCollectionSelectionSnapshot;
  LMissing: INyxCollectionSelection;
  LSpec: TNyxCollectionViewSpec;
  LPreferences: TNyxColumnPreferences;
  LObservation: TNyxColumnLayoutObservation;
  LFields: TNyxCollectionClipboardFields;
  LVisible: TNyxCollectionClipboardFields;
  LAll: TNyxCollectionClipboardFields;
  LCell: INyxCollectionClipboardText;
  LRows: INyxCollectionClipboardText;
  LLimits: TNyxCollectionClipboardLimits;
  LCopy: TNyxStateValue;
  LFieldCopy: TNyxColumnIdentity;
  LExpected: TNyxText;
  LBefore: Integer;
  LSerial: Integer;
  LRefused: Boolean;
  LReceiver: TReceiver;
  LViewToken: INyxCollectionViewSubscription;
  LStoreToken: INyxCollectionSubscription;
  LHostObject: THost;
  LHost: INyxClipboardWriteHost;
  LScheduler: INyxScheduler;
  LScope: INyxCancellationScope;
  LRequest: INyxClipboardWriteRequest;
begin
  LLimits := TNyxCollectionClipboardLimits.Defaults;
  LKey := NyxCollection('clipboard-source');
  LA := NyxItem(LKey, 'a 🌙');
  LB := NyxItem(LKey, 'b');
  LC := NyxItem(LKey, 'c');
  LSchema := NyxCollectionSchema.Text(NyxTextField(CName), '')
    .Text(NyxTextField(CQuoted), '').Boolean(NyxBooleanField('Flag'), False)
    .Integer(NyxIntegerField('Score'), 0).Number(NyxNumberField('Rate'), 0);
  LStore := NewNyxCollection(LKey, LSchema, [
    NyxCollectionItem(LA).WithValue(NyxTextField(CName), CFull)
      .WithValue(NyxTextField(CQuoted), 'plain').WithValue(NyxBooleanField('Flag'), True)
      .WithValue(NyxIntegerField('Score'), Low(Integer)).WithValue(NyxNumberField('Rate'), 1.25),
    NyxCollectionItem(LB).WithValue(NyxTextField(CName), 'B')
      .WithValue(NyxIntegerField('Score'), 42),
    NyxCollectionItem(LC).WithValue(NyxTextField(CName), 'C')
      .WithValue(NyxIntegerField('Score'), 7)]);
  LSpec := NyxCollectionView(LKey).Selection(nsmMultiple)
    .Column(NyxTextField(CName), 'Display name')
    .Column(NyxTextField(CQuoted), 'Display label')
    .Column(NyxBooleanField('Flag'), 'Display flag')
    .Column(NyxIntegerField('Score'), 'Display score')
    .Column(NyxNumberField('Rate'), 'Display rate').TableLayout(NyxTableLayout);
  LView := NewNyxCollectionView(LStore, LSpec, cpTable);
  LSource := LStore.Snapshot;
  LView.SetSelection([LC, LA, LB], LC, LA);
  LSelection := LView.Selection.Snapshot;
  LReceiver := TReceiver.Create;
  LScheduler := NewNyxScheduler;
  LHostObject := THost.Create;
  LHost := LHostObject;
  try
    LFields := nil;
    SetLength(LFields, 5);
    LFields[0] := NyxColumnIdentity(NyxTextField(CName));
    LFields[1] := NyxColumnIdentity(NyxTextField(CQuoted));
    LFields[2] := NyxColumnIdentity(NyxBooleanField('Flag'));
    LFields[3] := NyxColumnIdentity(NyxIntegerField('Score'));
    LFields[4] := NyxColumnIdentity(NyxNumberField('Rate'));
    LCell := NewNyxCollectionClipboardCell(LSource, LA, LFields[0], LLimits);
    Check((LCell.Text = CFull) and (LCell.ByteCount = NyxUTF8ByteCount(CFull)) and
      (LCell.RowCount = 1) and (LCell.FieldCount = 1),
      'raw single cell keeps the whole Unicode/tab/quote/CRLF/NUL text without TSV quoting');
    Check((LCell.Key.Name = LKey.Name) and (LCell.Revision = 0) and
      (LCell.ItemAt(0).ID = LA.ID) and LCell.FieldAt(0).Same(LFields[0]) and
      (LCell.CellValue(0, 0).TextValue = CFull),
      'single cell carries exact scoped row/full typed field/revision and accepted scalar');
    LCopy := LCell.CellValue(0, 0);
    LCopy := TNyxStateValue.FromText('caller replacement');
    LFieldCopy := LCell.FieldAt(0);
    LFieldCopy := NyxColumnIdentity(NyxIntegerField('Score'));
    Check((LCopy.TextValue = 'caller replacement') and
      (LFieldCopy.Kind = nskInteger) and (LCell.CellValue(0, 0).TextValue = CFull) and
      LCell.FieldAt(0).Same(LFields[0]), 'returned record replacements cannot change captured provenance/scalars');
    LRows := NewNyxCollectionClipboardRows(LSource, LSelection, LFields,
      nccmAllSelectedSource, nil, True, nccleLF, LLimits);
    LExpected := CHeaders + #10 + CFirst + #10 + 'B' + #9#9 + 'false' + #9 + '42' + #9 + '0' +
      #10 + 'C' + #9#9 + 'false' + #9 + '7' + #9 + '0';
    Check((LRows.Text = LExpected) and (LRows.ByteCount = NyxUTF8ByteCount(LExpected)),
      'TSV has exact escaped field-name headers/cells, invariant values and no trailing terminator');
    Check((LRows.RowCount = 3) and (LRows.FieldCount = 5) and
      (LRows.ItemAt(0).ID = LA.ID) and (LRows.ItemAt(2).ID = LC.ID),
      'all-selected copy uses source order rather than gesture/selection enumeration order');
    Check((LRows.CellValue(0, 2).Kind = nskBoolean) and LRows.CellValue(0, 2).BooleanValue and
      (LRows.CellValue(0, 3).IntegerValue = Low(Integer)) and
      (LRows.CellValue(0, 4).NumberText = '1.25'), 'raw numeric/Boolean provenance remains typed and invariant');
    LRows := NewNyxCollectionClipboardRows(LSource, LSelection, [LFields[3]],
      nccmAllSelectedSource, nil, False, nccleCRLF, LLimits);
    Check(LRows.Text = '-2147483648' + #13#10 + '42' + #13#10 + '7',
      'explicit CRLF rows do not add a trailing line or change scalar values');
    ConfigureQuery(LView, NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('Score')).AtLeast(0))
      .OrderBy(NyxIntegerField('Score')));
    LPage := LView.Snapshot;
    Check((LPage.Count = 2) and (LPage.ItemAt(0).Ref.ID = LC.ID),
      'ordinary query supplies a genuinely filtered/reordered accepted page');
    LRows := NewNyxCollectionClipboardRows(LSource, LSelection, [LFields[0]],
      nccmSelectedPage, LPage, False, nccleLF, LLimits);
    Check((LRows.Text = 'C' + #10 + 'B') and (LRows.RowCount = 2),
      'page copy excludes filtered selected row and preserves explicit page order');
    LRows := NewNyxCollectionClipboardRows(LSource, LSelection, [LFields[0]],
      nccmAllSelectedSource, nil, False, nccleLF, LLimits);
    Check((LRows.RowCount = 3) and (LRows.ItemAt(0).ID = LA.ID) and
      (LRows.CellValue(0, 0).TextValue = CFull),
      'all-selected copy includes hidden membership explicitly without asking the view for visible text');
    LPages := NyxCollectionPages(LView);
    ConfigurePaging(LPages, NyxCollectionPaging.PageSize(1));
    LPage := LView.Snapshot;
    LRows := NewNyxCollectionClipboardRows(LSource, LSelection, [LFields[0]],
      nccmSelectedPage, LPage, False, nccleLF, LLimits.Rows(1).Cells(1).ScannedRows(4));
    Check((LRows.Text = 'C') and (LRows.RowCount = 1),
      'one-row page budget admits an intersection of a larger retained selection');
    NextPage(LPages);
    LRows := NewNyxCollectionClipboardRows(LSource, LSelection, [LFields[0]],
      nccmSelectedPage, LView.Snapshot, False, nccleLF, LLimits);
    Check(LRows.Text = 'B', 'explicit current page choice differs from all selected rows');
    LView.ClearSelection;
    LRows := NewNyxCollectionClipboardRows(LSource, LSelection, [LFields[0]],
      nccmAllSelectedSource, nil, False, nccleLF, LLimits);
    Check((LRows.RowCount = 3) and (LView.Selection.Count = 0),
      'one immutable selection capture cannot mix with later live membership changes');
    RefuseRows(LSource, LView.Selection.Snapshot, LFields, nccmAllSelectedSource, nil, LLimits,
      'empty current selection refuses instead of silently clearing clipboard');
    LColumns := NyxCollectionColumns(LView);
    LPreferences := NyxColumnPreferences
      .WithColumn(NyxColumnPreference(LFields[0]).Pinned(ncpStart))
      .WithColumn(NyxColumnPreference(LFields[2]).Visible(False))
      .WithColumn(NyxColumnPreference(LFields[3]).Pinned(ncpEnd))
      .Order([LFields[4], LFields[0], LFields[1], LFields[3], LFields[2]]);
    ConfigureColumns(LColumns, LPreferences);
    LCapture := LColumns.CaptureColumns;
    LSerial := LCapture.Serial;
    LObservation := TNyxColumnLayoutObservation.Read(LCapture);
    LVisible := NyxCollectionClipboardFields(LObservation, nccfVisiblePresentation);
    LAll := NyxCollectionClipboardFields(LObservation, nccfAllArrangement);
    Check((Length(LVisible) = 4) and LVisible[0].Same(LFields[0]) and
      LVisible[1].Same(LFields[4]) and LVisible[3].Same(LFields[3]),
      'visible field choice follows real logical start/center/end grouping and excludes hidden fields');
    Check((Length(LAll) = 5) and LAll[0].Same(LFields[4]) and LAll[4].Same(LFields[2]),
      'all-fields choice deliberately includes hidden columns in complete arrangement order');
    LRows := NewNyxCollectionClipboardRows(LSource, LSelection, LVisible,
      nccmAllSelectedSource, nil, False, nccleLF, LLimits);
    Check((LRows.FieldCount = 4) and LRows.FieldAt(3).Same(LFields[3]) and
      (LColumns.CaptureColumns.Serial = LSerial), 'copying arrangement fields never publishes layout preferences');
    RefuseRows(nil, LSelection, LFields, nccmAllSelectedSource, nil, LLimits, 'missing source refuses');
    RefuseRows(LSource, Default(TNyxCollectionSelectionSnapshot), LFields,
      nccmAllSelectedSource, nil, LLimits, 'undefined selection capture refuses');
    RefuseRows(LSource, LSelection, [], nccmAllSelectedSource, nil, LLimits, 'empty explicit fields refuse');
    RefuseRows(LSource, LSelection, [LFields[0], LFields[0]], nccmAllSelectedSource, nil, LLimits,
      'duplicate full typed fields refuse');
    RefuseRows(LSource, LSelection, [NyxColumnIdentity(NyxBooleanField(CName))],
      nccmAllSelectedSource, nil, LLimits, 'same field name with changed scalar family refuses');
    RefuseRows(LSource, LSelection, [NyxColumnIdentity(NyxTextField('absent'))],
      nccmAllSelectedSource, nil, LLimits, 'missing typed field refuses');
    RefuseRows(LSource, LSelection, LFields, nccmSelectedPage, nil, LLimits, 'page mode never guesses a page');
    RefuseRows(LSource, LSelection, LFields, nccmAllSelectedSource, LPage, LLimits,
      'source-order mode never silently ignores a supplied page');
    RefuseRows(LSource, LSelection, LFields, nccmAllSelectedSource, nil, LLimits.Rows(2),
      'output row budget refuses before source scanning');
    RefuseRows(LSource, LSelection, LFields, nccmAllSelectedSource, nil, LLimits.Fields(4),
      'field budget refuses before copying an unbounded open array');
    RefuseRows(LSource, LSelection, LFields, nccmAllSelectedSource, nil, LLimits.Cells(14),
      'output cell product is bounded independently of byte count');
    RefuseRows(LSource, LSelection, [LFields[0]], nccmAllSelectedSource, nil, LLimits.ScannedRows(5),
      'row inspection budget counts immutable membership plus source scan');
    RefuseRows(LSource, LSelection, [LFields[0]], nccmSelectedPage, LPage, LLimits.ScannedRows(3),
      'page inspection budget counts retained selection plus actual page rows');
    RefuseRows(LSource, LSelection, LFields, nccmAllSelectedSource, nil, LLimits.UTF8Bytes(8),
      'UTF-8 byte budget includes TSV headers/escaping and supplementary text');
    RefuseRows(LSource, LSelection, LFields, nccmAllSelectedSource, nil,
      Default(TNyxCollectionClipboardLimits), 'uninitialized limits never imply unbounded work');
    LOther := NewNyxCollection(LKey, LSchema, [NyxCollectionItem(LC)
      .WithValue(NyxTextField(CName), 'different accepted row')]);
    RefuseRows(LSource, LSelection, LFields, nccmSelectedPage, LOther.Snapshot, LLimits,
      'same-key/same-revision page with different row scalar values refuses');
    LOther := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField(CName), ''),
      [NyxCollectionItem(LC)]);
    RefuseRows(LSource, LSelection, LFields, nccmSelectedPage, LOther.Snapshot, LLimits,
      'same-key page with changed schema refuses');
    LOther := NewNyxCollection(LKey, LSchema, [NyxCollectionItem(NyxItem(LKey, 'removed'))]);
    LMissing := NewNyxCollectionSelection(LOther.Snapshot, [NyxItem(LKey, 'removed')],
      NyxItem(LKey, 'removed'), NyxItem(LKey, 'removed'));
    RefuseRows(LSource, LMissing.Snapshot, LFields, nccmAllSelectedSource, nil, LLimits,
      'selected stable identity absent from this source refuses even at the same revision');
    LOther := NewNyxCollection(NyxCollection('foreign'), LSchema);
    RefuseRows(LSource, LSelection, LFields, nccmSelectedPage, LOther.Snapshot, LLimits,
      'foreign collection page never maps equal coordinate positions');
    LBefore := LSource.Revision;
    UpdateStore(LStore, NyxCollectionItem(LB).WithValue(NyxTextField(CName), 'new accepted value'));
    RefuseRows(LStore.Snapshot, LSelection, LFields, nccmAllSelectedSource, nil, LLimits,
      'old selection revision refuses against a newer source');
    LRows := NewNyxCollectionClipboardRows(LSource, LSelection, [LFields[0]],
      nccmAllSelectedSource, nil, False, nccleLF, LLimits);
    Check((LRows.Revision = LBefore) and (LRows.CellValue(1, 0).TextValue = 'B') and
      (LStore.Snapshot.Item(LB).GetValue(NyxTextField(CName)) = 'new accepted value'),
      'an explicit retained source freezes old accepted values instead of redirecting to later data');
    LRefused := False;
    try
      NewNyxCollectionClipboardCell(LSource, NyxItem(LKey, 'absent'), LFields[0], LLimits);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'missing cell identity refuses without coordinate fallback');
    LRefused := False;
    try
      NewNyxCollectionClipboardCell(LSource, LA, LFields[0], LLimits.UTF8Bytes(1));
    except
      on ENyxCollectionClipboard do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'single-cell byte budget is enforced before host handoff');
    LViewToken := LView.Subscribe({$ifdef PAS2JS}@{$endif}LReceiver.ViewChanged);
    LStoreToken := LStore.Subscribe({$ifdef PAS2JS}@{$endif}LReceiver.StoreChanged);
    LCell := NewNyxCollectionClipboardCell(LSource, LA, LFields[0], LLimits);
    Check((LReceiver.ViewChanges = 0) and (LReceiver.StoreChanges = 0) and
      (LColumns.CaptureColumns.Serial = LSerial), 'read-only cell capture does not notify or alter accepted layout');
    LPages := nil;
    LColumns := nil;
    LView := nil;
    LStore := nil;
    LSource := nil;
    LPage := nil;
    Check(not LViewToken.Connected and not LStoreToken.Connected,
      'retained payload holds no view/store owner and permits real subscription retirement' +
      ' (view connected=' + BoolToStr(LViewToken.Connected, True) +
      ', store connected=' + BoolToStr(LStoreToken.Connected, True) + ')');
    Check((LCell.Text = CFull) and (LRows.CellValue(1, 0).TextValue = 'B') and
      (LCell.ItemAt(0).ID = LA.ID) and LCell.FieldAt(0).Same(LFields[0]),
      'copied accepted payload and typed provenance survive complete source/view retirement');
    LRequest := WriteNyxCollectionClipboard(LCell, LHost, LScheduler, nil,
      {$ifdef PAS2JS}@{$endif}LReceiver.Reply);
    Check((LReceiver.Replies = 1) and (LHostObject.Writes = 1) and
      (LHostObject.Text = CFull) and (LReceiver.Last.ByteCount = NyxUTF8ByteCount(CFull)),
      'retired-source payload reaches injected host with whole exact text and one typed receipt');
    LHostObject.Outcome := ncwoUnavailable;
    LRequest := WriteNyxCollectionClipboard(LRows, LHost, LScheduler, nil,
      {$ifdef PAS2JS}@{$endif}LReceiver.Reply);
    Check((LReceiver.Last.Outcome = ncwoUnavailable) and
      (LReceiver.Last.ByteCount = 0) and (LRows.RowCount = 3),
      'typed unavailable host preserves copied selection/provenance rather than pretending success');
    LScope := NewNyxCancellationScope;
    LScope.Cancel;
    LBefore := LHostObject.Writes;
    LRequest := WriteNyxCollectionClipboard(LCell, LHost, LScheduler, LScope,
      {$ifdef PAS2JS}@{$endif}LReceiver.Reply);
    Check((LHostObject.Writes = LBefore) and (LReceiver.Replies = 2),
      'original cancellation silently refuses injected host work without recapturing a source');
  finally

    if LRequest <> nil then
    begin
      LRequest.Cancel;
    end;
    LRequest := nil;
    LViewToken := nil;
    LStoreToken := nil;
    LMissing := nil;
    LPages := nil;
    LColumns := nil;
    LView := nil;
    LStore := nil;
    LOther := nil;
    LHost := nil;
    LScheduler.Shutdown;
    LScheduler := nil;
    LReceiver.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-collection-clipboard', 'passed');
    document.body.setAttribute('data-collection-clipboard-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' collection clipboard producer checks; no OS write claim');
    {$endif}
  except
    on LError: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-collection-clipboard', 'failed');
      document.body.setAttribute('data-collection-clipboard-error', LError.Message);
      {$else}
      WriteLn('FAIL ', LError.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
