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
program nyx_collection_representation_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  Classes, SysUtils, nyx.text, nyx.types, nyx.state, nyx.model, nyx.controls,
  nyx.codec, nyx.contract, nyx.formatting, nyx.resources, nyx.collections,
  nyx.times, nyx.numbers.decimal,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.mount,
  nyx.collections.columns, nyx.collections.arrangement,
  nyx.collections.arrangement.controls, nyx.collections.arrangement.presenter,
  nyx.collections.clipboard
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.collections.arrangement.browser
  {$else}, Interfaces, Forms, Controls, Grids, StdCtrls, Graphics, Windows,
    nyx.render.lcl, nyx.collections.lcl.grid, nyx.collections.arrangement.lcl,
    nyx.editing.lcl{$endif};

type
  {$ifdef PAS2JS}
  { Synthetic standard key enters the actual owned table route. It establishes
    semantic/editor integration, not trusted hardware or assistive input. }
  TDisplayKey = class external name 'KeyboardEvent'(TJSKeyboardEvent)
  public
    constructor new(const AType: String; AInit: TJSObject); reintroduce;
  end;
  {$else}
  { Native fixture errors remain visible without opening modal dialogs. }
  TDisplayErrors = class
  public
    Text: TNyxText;
    procedure Failed(Sender: TObject; E: Exception);
  end;
  {$endif}

var
  GChecks: Integer;
  {$ifndef PAS2JS}GErrors: TDisplayErrors;{$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{$ifdef PAS2JS}
function Frame: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.requestAnimationFrame(procedure(ATime: Double)
        begin
          AResolve(JSValue(True));
        end);
    end);
end;
{$else}
procedure TDisplayErrors.Failed(Sender: TObject; E: Exception);
begin
  Text := TNyxText(E.Message);
end;
{$endif}

procedure Drain; {$ifdef PAS2JS}async;{$endif}
{$ifndef PAS2JS}var LTurn: Integer;{$endif}
begin
  {$ifdef PAS2JS}
  await(Frame);
  await(Frame);
  await(Frame);
  {$else}
  for LTurn := 1 to 5 do
  begin
    Application.ProcessMessages;
    Application.Idle(False);
    CheckSynchronize(0);
  end;
  {$endif}
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LTable: INyxTable;
  LKey: TNyxCollectionRef;
begin
  Result := TNyxDocument.Create;
  try
    LKey := NyxCollection('display-controls');
    Result.Collections.Define(LKey, NyxCollectionSchema.Text(NyxTextField('Name'), '')
      .Number(NyxNumberField('Price'), 0).Text(NyxTextField('Date'), '', NyxDateDomain)
      .Boolean(NyxBooleanField('Active'), False)
      .Number(NyxNumberField('Huge'), 0).Number(NyxNumberField('Exact'), 0)
      .Text(NyxTextField('Clock'), '', NyxTimeDomain), [NyxCollectionItem(NyxItem(LKey, 'one'))
        .WithValue(NyxTextField('Name'), TNyxText('raw 🌙'))
        .WithValue(NyxNumberField('Price'), 1234.5)
        .WithValue(NyxTextField('Date'), '2024-02-29')
        .WithValue(NyxBooleanField('Active'), True)
        .WithValue(NyxNumberField('Huge'), 1E100)
        .WithValue(NyxNumberField('Exact'), 1.125)
        .WithValue(NyxTextField('Clock'), '09:10:11')]);
    LPage := NewNyxPage('display-page');
    LPage.Configure.Padding(12).Done;
    LTable := NewNyxTable('display-table');
    LTable.Configure.Height(260).Done;
    LTable.Binds.Collection(NyxCollectionView(LKey)
      .Column(NyxTextField('Name'), 'Name')
        .ColumnFormat(NyxValueFormat.TextAffixes('Visible full value: ', ' / exact display'))
        .ColumnSizing(NyxColumnSizing.Minimum(30).Maximum(900))
        .ColumnLayout(NyxColumnLayout.PinStart)
      .Column(NyxNumberField('Price'), 'Price', cmEditable)
        .ColumnFormat(NyxValueFormat.Decimal(2).Localized(
          NyxFormatLocale.Named(NyxLocale('de-DE')).NumberSeparators(',', '.')))
      .Column(NyxTextField('Date'), 'Date')
        .ColumnFormat(NyxValueFormat.CalendarDate(nfdoDayMonthYear, '/'))
      .Column(NyxBooleanField('Active'), 'Active')
        .ColumnFormat(NyxValueFormat.BooleanLabels('Enabled 🌙', 'Disabled'))
        .ColumnLayout(NyxColumnLayout.PinEnd)
      .Column(NyxNumberField('Huge'), 'Huge').ColumnFormat(NyxValueFormat.Decimal(2))
      .Column(NyxNumberField('Exact'), 'Exact').ColumnFormat(NyxValueFormat.Decimal(2, ndrExact))
      .Column(NyxTextField('Clock'), 'Clock').ColumnFormat(NyxValueFormat.ClockTime(ntpMinute))
      .TableLayout(NyxTableLayout));
    LPage.Add(LTable);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LDocument: TNyxDocument;
  LState: TNyxState;
  LView: INyxCollectionView;
  LMount: INyxCollectionMount;
  LColumns: INyxCollectionColumns;
  LSampling: INyxCollectionColumnSampling;
  LSample: INyxCollectionColumnSample;
  LManagerMount: INyxCollectionColumnArrangementMount;
  LManager: INyxColumnArrangementManager;
  LPresenter: INyxColumnArrangementPresenter;
  LEdits: INyxCollectionEditMount;
  LDrafts: INyxCollectionEditsSnapshot;
  LRaw: INyxCollectionClipboardText;
  LKey: TNyxCollectionRef;
  LRow: TNyxItemRef;
  LField: TNyxColumnIdentity;
  LAuthored: TNyxText;
  LAction: TNyxText;
  LSerial: Integer;
  LIndex: Integer;
  LFound: Boolean;
  LPresentation: TNyxValuePresentation;
  LRefusalSample: INyxCollectionColumnSample;
  LRefusalColumn: Integer;
  LRefusalTitles: array[0..2] of TNyxText;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LSurface: TJSHTMLElement;
  LTable: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LInit: TJSObject;
  LTarget: INyxBrowserColumnArrangementPresenter;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LSurface: TForm;
  LGrid: TNyxCollectionStringGrid;
  LPriceGrid: TNyxCollectionStringGrid;
  LPriceColumn: Integer;
  LProbeGrid: TNyxCollectionStringGrid;
  LProbeColumn: Integer;
  LTarget: INyxLCLColumnArrangementPresenter;

  function NativeField(const ATitle: TNyxText; out AColumn: Integer): TNyxCollectionStringGrid;
  var
    LChild: Integer;
    LColumn: Integer;
    LCandidate: TNyxCollectionStringGrid;
  begin
    Result := nil;
    AColumn := -1;
    for LChild := -1 to LGrid.ControlCount - 1 do
    begin
      LCandidate := LGrid;

      if LChild >= 0 then
      begin

        if not (LGrid.Controls[LChild] is TNyxCollectionStringGrid) then
        begin
          Continue;
        end;
        LCandidate := TNyxCollectionStringGrid(LGrid.Controls[LChild]);
      end;
      for LColumn := 0 to LCandidate.ColCount - 1 do
      begin

        if Pos(ATitle, TNyxText(LCandidate.Cells[LColumn, 0])) = 1 then
        begin
          AColumn := LColumn;
          Exit(LCandidate);
        end;
      end;
    end;
  end;
  {$endif}
begin
  LDocument := nil;
  LState := nil;
  LRenderer := nil;
  LSurface := nil;
  try
    LDocument := Fixture;
    LAuthored := TNyxCodec.Encode(LDocument);
    LState := LDocument.State.Clone;
    {$ifdef PAS2JS}
    LSurface := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(LSurface);
    LRenderer := TNyxBrowserRenderer.Create;
    {$else}
    LSurface := TForm.CreateNew(nil);
    LSurface.ClientWidth := 900;
    LSurface.ClientHeight := 540;
    LRenderer := TNyxLCLRenderer.Create;
    {$endif}
    LRenderer.Render(LDocument, LDocument.Pages[0], LSurface, False, LState);
    {$ifndef PAS2JS}LSurface.Show;{$endif}
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    LView := LRenderer.CollectionView('display-table');
    LMount := LRenderer.CollectionMount('display-table');
    LColumns := NyxCollectionColumns(LView);
    LKey := NyxCollection('display-controls');
    LRow := NyxItem(LKey, 'one');
    Check((LView <> nil) and (LMount <> nil) and LMount.Connected and (LMount.ErrorText = ''),
      'actual display table mounts without a presentation error');
    {$ifdef PAS2JS}
    LTable := LRenderer.ElementFor('display-table');
    Check(TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="1"] .nyx-cell-value'))
      .textContent = '1.234,50', 'actual browser accepted label uses explicit number locale');
    Check(TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="2"] span'))
      .textContent = '29/02/2024', 'actual browser calendar label uses the admitted date domain');
    Check(TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="3"] span'))
      .textContent = TNyxText('Enabled 🌙'), 'actual browser logical end pin retains exact Boolean display');
    {$else}
    LGrid := TNyxCollectionStringGrid(LRenderer.ControlFor('display-table'));
    LPriceGrid := NativeField('Price', LPriceColumn);
    Check((LPriceGrid <> nil) and (LPriceColumn >= 0) and
      (LPriceGrid.DisplayCellText(LPriceColumn, 1) = '1.234,50') and
      (TNyxText(LPriceGrid.Cells[LPriceColumn, 1]) = '1234.5'),
      'actual native paint reader formats while the same grid Cells/editor reader remains RAW');
    LProbeGrid := NativeField('Date', LProbeColumn);
    Check((LProbeGrid <> nil) and (LProbeGrid.DisplayCellText(LProbeColumn, 1) = '29/02/2024'),
      'actual native calendar label uses the admitted date domain');
    LProbeGrid := NativeField('Active', LProbeColumn);
    Check((LProbeGrid <> nil) and (LProbeGrid.DisplayCellText(LProbeColumn, 1) = TNyxText('Enabled 🌙')),
      'actual native logical end pin retains exact Boolean display');
    {$endif}
    Check((LView.CellText(LRow, 1) = '1234.5') and (LView.CellText(LRow, 2) = '2024-02-29'),
      'actual mounted view retains canonical accepted editor/query/RAW text');
    Check(Supports(LView, INyxCollectionColumnSampling, LSampling),
      'mounted view exposes canonical detached Fit sampling');
    LField := NyxColumnIdentity(NyxTextField('Name'));
    LSample := LSampling.CaptureColumnSample(LColumns.CaptureColumns, LField);
    Check(LSample.TextAt(0) = TNyxText('Visible full value: raw 🌙 / exact display'),
      'actual mounted Fit samples the same full accepted formatted label');
    LRefusalTitles[0] := 'Huge';
    LRefusalTitles[1] := 'Exact';
    LRefusalTitles[2] := 'Clock';
    for LRefusalColumn := 4 to 6 do
    begin
      LPresentation := LView.Spec.ColumnAt(LRefusalColumn).Present(
        LView.Snapshot.Item(LRow), LView.Snapshot.Schema);
      {$ifdef PAS2JS}
      LCell := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="' +
        IntToStr(LRefusalColumn) + '"] span'));
      Check((LCell <> nil) and (LCell.textContent = LPresentation.Text) and
        (LCell.getAttribute('data-nyx-display-outcome') = 'unavailable') and
        (LCell.getAttribute('title') = LPresentation.Diagnostic),
        'actual browser label exposes the expected refusal separately from diagnostic/RAW: ' + LRefusalTitles[LRefusalColumn - 4]);
      {$else}
      LProbeGrid := NativeField(LRefusalTitles[LRefusalColumn - 4], LProbeColumn);
      Check((LProbeGrid <> nil) and (LProbeGrid.DisplayCellText(LProbeColumn, 1) = LPresentation.Text) and
        (LProbeGrid.DisplayCellPresentation(LProbeColumn, 1).Outcome = nvpoUnavailable) and
        (LProbeGrid.DisplayCellPresentation(LProbeColumn, 1).Diagnostic = LPresentation.Diagnostic),
        'actual native paint exposes the expected refusal separately from diagnostic/RAW: ' + LRefusalTitles[LRefusalColumn - 4]);
      {$endif}
      Check((LPresentation.Outcome = nvpoUnavailable) and (LPresentation.Diagnostic <> '') and
        (LPresentation.RawText = LView.CellText(LRow, LRefusalColumn)),
        'accepted canonical value survives expected target display refusal: ' + LRefusalTitles[LRefusalColumn - 4]);
      LRefusalSample := LSampling.CaptureColumnSample(LColumns.CaptureColumns,
        LView.Spec.ColumnAt(LRefusalColumn).Identity);
      Check(LRefusalSample.TextAt(0) = LPresentation.Text,
        'detached Fit samples the same explicit unavailable presentation as paint: ' + LRefusalTitles[LRefusalColumn - 4]);
    end;
    LRaw := NewNyxCollectionClipboardCell(LView.Store.Snapshot, LRow, LField,
      TNyxCollectionClipboardLimits.Defaults);
    Check(LRaw.Text = TNyxText('raw 🌙'), 'RAW copy producer never substitutes visible affixes');
    {$ifdef PAS2JS}
    LCell := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [role="gridcell"][data-nyx-column="1"]'));
    LCell.focus;
    LInit := TJSObject.new;
    LInit['key'] := 'F2';
    LInit['bubbles'] := True;
    LInit['cancelable'] := True;
    LCell.dispatchEvent(TDisplayKey.new('keydown', LInit));
    await(Drain);
    LInput := TJSHTMLInputElement(LCell.querySelector('input'));
    Check((LInput <> nil) and not LInput.hasAttribute('hidden') and (LInput.value = '1234.5'),
      'actual browser F2 starts from canonical numeric text rather than formatted label');
    LInput.value := '-';
    LInput.dispatchEvent(TJSEvent.new('input'));
    {$else}
    LPriceGrid.Col := LPriceColumn;
    LPriceGrid.Row := 1;
    LPriceGrid.SetFocus;
    SendMessage(LPriceGrid.Handle, WM_KEYDOWN, VK_F2, 1);
    Drain;
    Check(LPriceGrid.EditorMode and (NyxLCLInputText(LPriceGrid.Editor) = '1234.5'),
      'actual native F2 starts from canonical numeric text rather than formatted paint');
    RestoreNyxLCLInputText(LPriceGrid.Editor, '-');
    {$endif}
    Check(Supports(LMount, INyxCollectionColumnArrangementMount, LManagerMount),
      'actual display table retains existing managed Columns integration');
    LManager := LManagerMount.ColumnArrangement;
    LManager.Open;
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    LPresenter := LManager.Presenter;
    Check((LPresenter <> nil) and LPresenter.IsOpen and Supports(LPresenter,
      {$ifdef PAS2JS}INyxBrowserColumnArrangementPresenter{$else}INyxLCLColumnArrangementPresenter{$endif}, LTarget),
      'actual managed Columns presentation opens without accepting the unfinished cell');
    LSerial := LColumns.CaptureColumns.Serial;
    LAction := NyxColumnArrangementActionID(NyxColumnArrangementEditorID,
      LManager.Observation, LField, ncaaFitWidth);
    LTarget.Presentation.Renderer.Reveal(LAction, niRuntime);
    {$ifdef PAS2JS}
    TJSHTMLElement(LTarget.Presentation.Renderer.ElementFor(LAction)).click;
    await(Drain);
    {$else}
    TButton(LTarget.Presentation.Renderer.ControlFor(LAction)).Click;
    Drain;
    {$endif}
    Check((LColumns.CaptureColumns.Serial = LSerial + 1) and
      (LManager.Observation.ColumnAt(0).Width >= 150),
      'real manager Fit measures the expanded display label and publishes one logical width');
    Check((LView.CellText(LRow, 1) = '1234.5') and (LView.Store.Snapshot.Revision = 0) and
      (TNyxCodec.Encode(LDocument) = LAuthored),
      'display inspection and Fit preserve accepted source/data while a numeric draft is incomplete');
    Check(Supports(LMount, INyxCollectionEditMount, LEdits), 'actual table supports explicit draft capture');
    LDrafts := LEdits.SuspendCellEdits;
    LFound := False;
    for LIndex := 0 to LDrafts.Count - 1 do
    begin

      if LDrafts.CellAt(LIndex).Field.Same(NyxColumnIdentity(NyxNumberField('Price'))) and
        (LDrafts.CellAt(LIndex).Item.ID = 'one') and (LDrafts.CellAt(LIndex).Text = '-') then
      begin
        LFound := True;
      end;
    end;
    Check(LFound, 'format-aware paint and Fit keep the exact unfinished numeric draft detached from accepted data');
    LEdits.ResumeCellEdits(LDrafts, False);
    LDrafts := nil;
    LRenderer.Unmount;
    Check(not LMount.Connected and (LSample.TextAt(0) = TNyxText('Visible full value: raw 🌙 / exact display')),
      'actual renderer retirement revokes input while copied formatted sample remains owner-free');
    {$ifndef PAS2JS}Check(GErrors.Text = '', 'native display journey has no deferred application error');{$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-representation-controls', 'passed');
    document.body.setAttribute('data-representation-control-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' actual display controls; no external output effects');
    {$endif}
  finally
    LDrafts := nil;
    LEdits := nil;
    LTarget := nil;
    LPresenter := nil;
    LManager := nil;
    LManagerMount := nil;
    LRaw := nil;
    LSample := nil;
    LRefusalSample := nil;
    LSampling := nil;
    LColumns := nil;
    LMount := nil;
    LView := nil;
    LRenderer.Free;
    {$ifdef PAS2JS}

    if LSurface <> nil then
    begin
      LSurface.remove;
    end;
    {$else}
    Drain;
    LSurface.Free;
    {$endif}
    LState.Free;
    LDocument.Free;
  end;
end;

procedure Start; {$ifdef PAS2JS}async;{$endif}
begin
  try
    {$ifdef PAS2JS}await(Run);{$else}Run;{$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-representation-controls', 'failed');
      document.body.setAttribute('data-representation-control-checks', IntToStr(GChecks));
      document.body.setAttribute('data-representation-control-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  {$ifndef PAS2JS}
  Application.Initialize;
  Application.ShowMainForm := False;
  GErrors := TDisplayErrors.Create;
  Application.OnException := GErrors.Failed;
  {$endif}
  try
    Start;
  finally
    {$ifndef PAS2JS}
    Application.OnException := nil;
    GErrors.Free;
    {$endif}
  end;
end.
