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
program nyx_collection_presentation_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses Classes, SysUtils, Math, nyx.text, nyx.types, nyx.state, nyx.model, nyx.controls,
  nyx.codec, nyx.contract, nyx.collections, nyx.collections.columns,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.mount,
  nyx.collections.arrangement, nyx.collections.arrangement.controls,
  nyx.collections.arrangement.presenter, nyx.collections.presentation,
  nyx.collections.text.layout
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.collections.arrangement.browser
  {$else}, Interfaces, Forms, Controls, Grids, StdCtrls, Graphics, LCLType,
  LMessages, LCLIntf, nyx.render.lcl, nyx.collections.lcl.grid,
  nyx.collections.arrangement.lcl, nyx.editing.lcl, nyx.test.capture.lcl{$endif};

type
  {$ifdef PAS2JS}
  { Synthetic keyboard entry exercises the real bound table producer without
    claiming trusted hardware or assistive-technology behavior. }
  TLayoutKey = class external name 'KeyboardEvent'(TJSKeyboardEvent)
  public
    constructor new(const AType: String; AInit: TJSObject); reintroduce;
  end;
  { Dispatches composition lifecycle through the actual owned input. This
    exercises DOM routing and deferral without claiming a physical IME. }
  TLayoutComposition = class external name 'CompositionEvent'(TJSEvent)
  public
    constructor new(const AType: String; AInit: TJSObject); reintroduce;
  end;
  {$else}
  { Deferred native errors are captured, never hidden behind a modal dialog. }
  TLayoutErrors = class
  public
    Text: TNyxText;
    procedure Failed(Sender: TObject; E: Exception);
  end;
  {$endif}

const
  CPath: TNyxText = 'C:\Users\🌙\very-long-component-name\another-component\file.json';
  CNotes: TNyxText = 'first' + #13#10 + 'second full line 🌙';
  CClip: TNyxText = 'complete clipped' + #0 + 'value 🌙';

var
  GChecks: Integer;
  GClientWidth: Integer;
  {$ifndef PAS2JS}GErrors: TLayoutErrors;{$endif}

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
procedure TLayoutErrors.Failed(Sender: TObject; E: Exception);
begin
  Text := TNyxText(E.Message);
end;
{$endif}

procedure Drain; {$ifdef PAS2JS}async;{$endif}
{$ifndef PAS2JS}var I: Integer;{$endif}
begin
  {$ifdef PAS2JS}
  await(Frame);
  await(Frame);
  await(Frame);
  {$else}
  for I := 1 to 5 do
  begin
    Application.ProcessMessages;
    Application.Idle(False);
    CheckSynchronize(0);
  end;
  {$endif}
end;

{$ifdef PAS2JS}
function ContinueCapture(AEvent: TJSMouseEvent): Boolean;
begin
  document.body.setAttribute('data-capture-observed',
    document.body.getAttribute('data-capture-checkpoint'));
  Result := True;
end;

{ Optional visual qualification pauses this same application, never substitutes
  a second fixture. Only an actual click acknowledges the observed live image. }
procedure Capture(const AName: TNyxText); async;
var
  LStarted: Double;
  LContinue: TJSHTMLElement;
begin

  if Pos('capture=1', window.location.search) = 0 then
  begin
    Exit;
  end;
  document.body.setAttribute('data-capture-checkpoint', AName);
  document.body.setAttribute('data-capture-observed', 'pending');
  LStarted := window.performance.now;
  LContinue := TJSHTMLElement(document.createElement('button'));
  LContinue.id := 'presentation-capture-continue';
  LContinue.setAttribute('type', 'button');
  LContinue.textContent := 'Continue after capture';
  LContinue.style.cssText := 'position:fixed;right:12px;bottom:12px;z-index:1000';
  LContinue.onclick := @ContinueCapture;
  document.body.appendChild(LContinue);
  try
    repeat
      await(Frame);

      if window.performance.now - LStarted > 120000 then
      begin
        raise ENyxModel.Create('Presentation capture acknowledgment timed out');
      end;
    until document.body.getAttribute('data-capture-observed') = AName;
  finally
    LContinue.onclick := nil;
    LContinue.remove;
  end;
end;
{$endif}

{ Isolate a self-returning fluent result from this fixture's outer lifecycle;
  pas2js interface temporaries must not keep the mounted view alive accidentally. }
procedure ApplyPresentation(const AView: INyxCollectionView;
  const APreferences: TNyxTablePresentationPreferences);
var
  LPolicy: INyxCollectionTablePresentation;
begin
  LPolicy := NyxCollectionTablePresentation(AView);
  LPolicy.ConfigurePresentation(APreferences);
end;

function Fixture(AArranged: Boolean): TNyxDocument;
var
  LPage: INyxPage;
  LTable: INyxTable;
  LKey: TNyxCollectionRef;
  LSpec: TNyxCollectionViewSpec;
begin
  Result := TNyxDocument.Create;
  try
    LKey := NyxCollection('layout-controls');
    Result.Collections.Define(LKey, NyxCollectionSchema.Text(NyxTextField('Name'), '')
      .Number(NyxNumberField('Amount'), 0).Text(NyxTextField('Notes'), '')
      .Text(NyxTextField('Clip'), '').Text(NyxTextField('Budget'), ''),
      [NyxCollectionItem(NyxItem(LKey, 'one')).WithValue(NyxTextField('Name'), CPath)
        .WithValue(NyxNumberField('Amount'), 1234.5).WithValue(NyxTextField('Notes'), CNotes)
        .WithValue(NyxTextField('Clip'), CClip)
        .WithValue(NyxTextField('Budget'), 'budget text'),
       NyxCollectionItem(NyxItem(LKey, 'two')).WithValue(NyxTextField('Name'), 'short')
        .WithValue(NyxNumberField('Amount'), 7).WithValue(NyxTextField('Notes'), 'short')
        .WithValue(NyxTextField('Clip'), 'short').WithValue(NyxTextField('Budget'), 'short')]);
    LPage := NewNyxPage('layout-page');
    LPage.Configure.Padding(12).Done;
    LTable := NewNyxTable('layout-table');
    LTable.Configure.Height(300).Done;
    LSpec := NyxCollectionView(LKey)
      .Column(NyxTextField('Name'), 'Name')
        .ColumnSizing(NyxColumnSizing.Fixed(120).Minimum(50).Maximum(700));

    if AArranged then
    begin
      LSpec := LSpec.ColumnLayout(NyxColumnLayout.PinStart);
    end;
    LSpec := LSpec.ColumnText(NyxColumnText.Wrap(3))
      .Column(NyxNumberField('Amount'), 'Amount', cmEditable)
        .ColumnSizing(NyxColumnSizing.Fixed(90))
      .Column(NyxTextField('Notes'), 'Notes')
        .ColumnSizing(NyxColumnSizing.Fixed(100));

    if AArranged then
    begin
      LSpec := LSpec.ColumnLayout(NyxColumnLayout.PinEnd);
    end;
    LSpec := LSpec.ColumnText(NyxColumnText.Ellipsis)
      .Column(NyxTextField('Clip'), 'Clip')
        .ColumnSizing(NyxColumnSizing.Fixed(80)).ColumnText(NyxColumnText.Clip)
      .Column(NyxTextField('Budget'), 'Budget')
        .ColumnSizing(NyxColumnSizing.Fixed(100)).ColumnText(NyxColumnText.Wrap(1000))
      .TablePresentation(NyxTablePresentation.Density(ntrdCompact));

    if AArranged then
    begin
      LSpec := LSpec.TableLayout(NyxTableLayout);
    end;
    LTable.Binds.Collection(LSpec);
    LPage.Add(LTable);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure Run(AArranged: Boolean; ARestrictedWork: Boolean = False); {$ifdef PAS2JS}async;{$endif}
var
  LDocument: TNyxDocument;
  LState: TNyxState;
  LView: INyxCollectionView;
  LMount: INyxCollectionMount;
  LColumns: INyxCollectionColumns;
  LManagerMount: INyxCollectionColumnArrangementMount;
  LManager: INyxColumnArrangementManager;
  LPresenter: INyxColumnArrangementPresenter;
  LEdits: INyxCollectionEditMount;
  LDrafts: INyxCollectionEditsSnapshot;
  LRow: TNyxItemRef;
  LAction: TNyxText;
  LAuthored: TNyxText;
  LSerial: Integer;
  LPreferences: TNyxText;
  LPhysicalWidth: Integer;
  LHeader: Integer;
  LCompact: Integer;
  I: Integer;
  LFound: Boolean;
  LExpectedAdmission: TNyxTableTextAdmission;
  LRefused: Boolean;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LSurface: TJSHTMLElement;
  LTable: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LLabel: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LInit: TJSObject;
  LFontSize: Double;
  LLineCount: Integer;
  LAt: Integer;
  LTarget: INyxBrowserColumnArrangementPresenter;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LSurface: TForm;
  LGrid: TNyxCollectionStringGrid;
  LNameGrid: TNyxCollectionStringGrid;
  LNotesGrid: TNyxCollectionStringGrid;
  LAmountGrid: TNyxCollectionStringGrid;
  LNameColumn: Integer;
  LNotesColumn: Integer;
  LAmountColumn: Integer;
  LColumn: Integer;
  LProbe: TNyxCollectionStringGrid;
  LLayout: TNyxTableTextLayout;
  LTarget: INyxLCLColumnArrangementPresenter;
  LStyle: TTextStyle;

  function Field(const ATitle: TNyxText; out AColumn: Integer): TNyxCollectionStringGrid;
  var
    LChild: Integer;
    LAt: Integer;
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
      for LAt := 0 to LCandidate.ColCount - 1 do
      begin

        if Pos(ATitle, TNyxText(LCandidate.Cells[LAt, 0])) = 1 then
        begin
          AColumn := LAt;
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
  LExpectedAdmission := nttaLineLimit;

  if ARestrictedWork then
  begin
    LExpectedAdmission := nttaCellLimit;
  end;
  try
    LDocument := Fixture(AArranged);
    LAuthored := TNyxCodec.Encode(LDocument);
    LState := LDocument.State.Clone;
    {$ifdef PAS2JS}
    LSurface := TJSHTMLElement(document.createElement('div'));
    LSurface.style.setProperty('width', IntToStr(Max(1, Round(window.innerWidth) - 16)) + 'px');
    document.body.appendChild(LSurface);
    LRenderer := TNyxBrowserRenderer.Create;

    if ARestrictedWork then
    begin
      LRenderer.TableTextLimits := TNyxTableTextWorkLimits.Defaults.Cells(1);
    end;
    LRenderer.Render(LDocument, LDocument.Pages[0], LSurface, False, LState);
    LTable := TJSHTMLElement(LRenderer.ElementFor('layout-table'));
    {$else}
    LSurface := TForm.CreateNew(nil);
    LSurface.SetBounds(20, 20, GClientWidth, 600);
    LSurface.ClientWidth := GClientWidth;
    LSurface.Show;
    LRenderer := TNyxLCLRenderer.Create;

    if ARestrictedWork then
    begin
      LRenderer.TableTextLimits := TNyxTableTextWorkLimits.Defaults.Cells(1);
    end;
    LRenderer.Render(LDocument, LDocument.Pages[0], LSurface, False, LState);
    LGrid := TNyxCollectionStringGrid(LRenderer.ControlFor('layout-table'));
    {$endif}
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    LView := LRenderer.CollectionView('layout-table');
    LMount := LRenderer.CollectionMount('layout-table');
    LColumns := NyxCollectionColumns(LView);
    LRow := NyxItem(NyxCollection('layout-controls'), 'one');
    Check(LView.Spec.HasLayoutControls = AArranged,
      'ordinary presentation has no table or per-column arrangement opt-in');
    Check(LMount.Connected and (LMount.ErrorText = ''), 'real table presentation mounts without a target admission error');
    LRefused := False;
    try
      LRenderer.TableTextLimits := TNyxTableTextWorkLimits.Defaults;
    except
      on E: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'an active table keeps its exact original runtime measurement limits');
    {$ifdef PAS2JS}
    Check((LSurface.clientWidth <= window.innerWidth) and (LTable.clientWidth > 0),
      'actual table allocation is bounded by the measured browser viewport');
    document.body.setAttribute('data-presentation-viewport-width', IntToStr(Round(window.innerWidth)));
    document.body.setAttribute('data-presentation-table-width-' + LowerCase(BoolToStr(AArranged, True)),
      IntToStr(Round(LTable.clientWidth)));
    {$else}
    Check(LSurface.ClientWidth = GClientWidth, 'native host retains the exact requested client width');
    WriteLn('presentation client=', LSurface.ClientWidth, ' table=', LGrid.ClientWidth,
      ' arranged=', AArranged);
    {$endif}
    {$ifdef PAS2JS}
    LCell := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="0"]'));
    LLabel := TJSHTMLElement(LCell.querySelector('.nyx-cell-value'));
    Check((LLabel.getAttribute('data-nyx-text-layout') = 'ellipsized') and
      (Pos(#10, LLabel.textContent) > 0), 'actual browser label wraps an unspaced path and marks its clamped last line');
    LFontSize := JS.parseFloat(window.getComputedStyle(LLabel).getPropertyValue('font-size'));
    LLineCount := 1;
    for LAt := 1 to Length(LLabel.textContent) do
    begin

      if LLabel.textContent[LAt] = #10 then
      begin
        Inc(LLineCount);
      end;
    end;
    Check((LFontSize > 0) and not IsNan(LFontSize) and
      (LLabel.style.getPropertyValue('line-height') = '') and
      (LLabel.getBoundingClientRect.height >= LLineCount * LFontSize) and
      (LCell.getBoundingClientRect.height >= LLabel.getBoundingClientRect.height) and
      (LCell.parentElement.getBoundingClientRect.height >= LCell.getBoundingClientRect.height),
      'actual wrapped browser lines have readable font-scale height inside their body row');
    Check(LCell.getAttribute('aria-label') = CPath, 'accessible browser cell retains the complete unclipped value');
    LLabel := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="4"] .nyx-cell-value'));
    Check((LLabel.getAttribute('data-nyx-text-layout') = 'unavailable') and
      (LLabel.getAttribute('data-nyx-text-layout-diagnostic') <> '') and
      (((LExpectedAdmission = nttaLineLimit) and (LLabel.getAttribute('data-nyx-text-layout-admission') = 'line-limit')) or
       ((LExpectedAdmission = nttaCellLimit) and (LLabel.getAttribute('data-nyx-text-layout-admission') = 'cell-limit'))),
      'actual browser work refusal stays visible and separately typed');
    LHeader := Round(TJSHTMLElement(LTable.querySelector('thead')).offsetHeight);
    LCompact := Round(TJSHTMLElement(LTable.querySelector('[data-nyx-item="two"]')).offsetHeight);

    if not ARestrictedWork then
    begin

      if AArranged then
      begin
        await(Capture('table-presentation-arranged'));
      end
      else
      begin
        await(Capture('table-presentation-ordinary'));
      end;
    end;
    LCell := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="1"]'));
    LCell.focus;
    LInit := TJSObject.new;
    LInit['key'] := 'F2';
    LInit['bubbles'] := True;
    LInit['cancelable'] := True;
    LCell.dispatchEvent(TLayoutKey.new('keydown', LInit));
    LInput := TJSHTMLInputElement(LCell.querySelector('input'));
    LInput.value := '-';
    {$else}
    LNameGrid := Field('Name', LNameColumn);
    LNotesGrid := Field('Notes', LNotesColumn);
    LAmountGrid := Field('Amount', LAmountColumn);
    Check((LNameGrid <> nil) and (LNotesGrid <> nil) and (LAmountGrid <> nil), 'native logical fields resolve across actual editable pin panes');
    Check(LNameGrid.CellTextLayout(LNameColumn, 1, LLayout) and
      (LLayout.Count > 1) and (LLayout.Outcome = ntloEllipsized), 'actual native path uses measured wrapping with a clamped visible tail');
    LProbe := Field('Budget', LColumn);
    Check((LProbe <> nil) and LProbe.CellTextLayout(LColumn, 1, LLayout) and
      (LLayout.Admission = LExpectedAdmission) and (LLayout.Diagnostic <> ''), 'native runtime line or cell refusal has a visible measured result and typed reason');
    Check((LNameGrid.RowHeights[1] = LGrid.RowHeights[1]) and
      (LNotesGrid.RowHeights[1] = LGrid.RowHeights[1]), 'both logical pin edges share exact outer variable row geometry');
    LHeader := LGrid.RowHeights[0];
    LCompact := LGrid.RowHeights[2];

    if not ARestrictedWork and (ParamCount > 1) then
    begin
      SaveNyxNativeCapture(LSurface, TNyxText(ParamStr(2)) + '-' +
        TNyxText(LowerCase(BoolToStr(AArranged, True))) + '.png', ncmPrint);
      WriteLn('Native presentation capture mode=Print diagnostic redraw');
    end;
    LAmountGrid.Col := LAmountColumn;
    LAmountGrid.Row := 1;
    LAmountGrid.SetFocus;
    LCLIntf.SendMessage(LAmountGrid.Handle, LM_KEYDOWN, VK_F2, 0);
    Check(LAmountGrid.EditorMode and (LAmountGrid.Editor is TCustomEdit), 'actual native F2 enters the original numeric editor');
    RestoreNyxLCLInputText(TCustomEdit(LAmountGrid.Editor), '-');
    {$endif}
    Check((LView.CellText(LRow, 0) = CPath) and (LView.CellText(LRow, 2) = CNotes) and
      (LView.CellText(LRow, 1) = '1234.5') and (LView.CellText(LRow, 3) = CClip),
      'wrapping truncation and draft entry leave exact RAW Unicode/NUL/line values untouched');
    {$ifdef PAS2JS}
    LInput.setSelectionRange(1, 1);
    LInit := TJSObject.new;
    LInit['bubbles'] := True;
    LInput.dispatchEvent(TLayoutComposition.new('compositionstart', LInit));
    {$endif}
    ApplyPresentation(LView, NyxTablePresentationPreferences.Density(ntrdComfortable));
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    {$ifdef PAS2JS}
    Check((LCell.querySelector('input') = LInput) and
      (document.activeElement = LInput) and (LInput.value = '-') and
      (LInput.selectionStart = 1) and (LInput.selectionEnd = 1) and
      (TJSHTMLElement(LTable.querySelector('[data-nyx-item="two"]')).offsetHeight = LCompact),
      'composition defers density geometry while retaining exact editor focus and scalar caret');
    LInput.dispatchEvent(TLayoutComposition.new('compositionend', LInit));
    await(Drain);
    Check((LCell.querySelector('input') = LInput) and
      (document.activeElement = LInput) and (LInput.value = '-') and
      (LInput.selectionStart = 1) and (LInput.selectionEnd = 1),
      'composition completion applies latest presentation without blur or draft acceptance');
    Check(TJSHTMLElement(LTable.querySelector('[data-nyx-item="two"]')).offsetHeight > LCompact,
      'runtime comfortable density increases actual browser body allocation');
    Check(TJSHTMLElement(LTable.querySelector('thead')).offsetHeight = LHeader, 'browser density leaves header allocation unchanged');
    {$else}
    Check(LGrid.RowHeights[2] > LCompact, 'runtime comfortable density increases actual native body allocation');
    Check(LGrid.RowHeights[0] = LHeader, 'native density leaves header and management allocation unchanged');
    {$endif}
    { Unsupported extra shaping is an explicit visible result. The original
      typed value and incomplete numeric editor remain independent of it. }
    {$ifdef PAS2JS}
    LLabel := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="0"] .nyx-cell-value'));
    LLabel.style.setProperty('letter-spacing', '2px');
    LMount.Refresh;
    await(Drain);
    Check((LLabel.getAttribute('data-nyx-text-layout') = 'unavailable') and
      (LLabel.getAttribute('data-nyx-text-layout-admission') = 'host-metrics'),
      'browser extra shaping refuses visibly instead of pretending Canvas measured that style');
    LLabel.style.removeProperty('letter-spacing');
    {$else}
    LStyle := LGrid.DefaultTextStyle;
    LStyle.SystemFont := True;
    LGrid.DefaultTextStyle := LStyle;
    LMount.Refresh;
    Drain;
    Check(LNameGrid.CellTextLayout(LNameColumn, 1, LLayout) and
      (LLayout.Admission = nttaHostMetrics),
      'native unresolved SystemFont style reports explicit host-metric refusal');
    LStyle.SystemFont := False;
    LGrid.DefaultTextStyle := LStyle;
    {$endif}
    LMount.Refresh;
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(Supports(LMount, INyxCollectionEditMount, LEdits), 'actual mount retains its existing editor handoff contract');
    LDrafts := LEdits.SuspendCellEdits;
    LFound := False;
    for I := 0 to LDrafts.Count - 1 do
    begin

      if (LDrafts.CellAt(I).Item.ID = 'one') and
        LDrafts.CellAt(I).Field.Same(NyxColumnIdentity(NyxNumberField('Amount'))) and
        (LDrafts.CellAt(I).Text = '-') then
      begin
        LFound := True;
      end;
    end;
    Check(LFound, 'density reflow preserves the exact incomplete numeric editor draft');
    LEdits.ResumeCellEdits(LDrafts, False);
    LDrafts := nil;
    if AArranged then
    begin
      Check(Supports(LMount, INyxCollectionColumnArrangementMount, LManagerMount), 'actual table retains its ordinary managed Fit owner');
      LManager := LManagerMount.ColumnArrangement;
      LManager.Open;
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      LPresenter := LManager.Presenter;
      Check((LPresenter <> nil) and LPresenter.IsOpen, 'actual ordinary Columns form is open before invoking its retained action');
      LSerial := LColumns.CaptureColumns.Serial;
      LPreferences := LColumns.CaptureColumns.Preferences.ToData.ToJSON;
      {$ifdef PAS2JS}
      LPhysicalWidth := Round(TJSHTMLElement(LTable.querySelector(
        '[data-nyx-item="one"] [data-nyx-column="0"]')).getBoundingClientRect.width);
      {$else}
      LPhysicalWidth := LNameGrid.ColWidths[LNameColumn];
      {$endif}
      LAction := NyxColumnArrangementActionID(NyxColumnArrangementEditorID, LManager.Observation,
        NyxColumnIdentity(NyxTextField('Name')), ncaaFitWidth);
      {$ifdef PAS2JS}
      Check(Supports(LPresenter, INyxBrowserColumnArrangementPresenter, LTarget), 'actual browser Fit exposes its managed ordinary popup');
      LTarget.Presentation.Renderer.Reveal(LAction, niRuntime);
      TJSHTMLElement(LTarget.Presentation.Renderer.ElementFor(LAction)).click;
      await(Drain);
      {$else}
      Check(Supports(LPresenter, INyxLCLColumnArrangementPresenter, LTarget), 'actual native Fit exposes its managed ordinary popup');
      LTarget.Presentation.Renderer.Reveal(LAction, niRuntime);
      TButton(LTarget.Presentation.Renderer.ControlFor(LAction)).Click;
      Drain;
      {$endif}

      if ARestrictedWork then
      begin
        Check((LColumns.CaptureColumns.Serial = LSerial) and
          (LColumns.CaptureColumns.Preferences.ToData.ToJSON = LPreferences) and
          not LManager.Observation.ColumnAt(0).HasWidth,
          'Fit refuses a full sample beyond the original pass allowance without applying a prefix width');
        {$ifdef PAS2JS}
        Check(Round(TJSHTMLElement(LTable.querySelector(
          '[data-nyx-item="one"] [data-nyx-column="0"]')).getBoundingClientRect.width) = LPhysicalWidth,
          'refused Fit retains the exact original physical width');
        {$else}
        Check(LNameGrid.ColWidths[LNameColumn] = LPhysicalWidth,
          'refused Fit retains the exact original physical width');
        {$endif}
        Check(Pos('cell measurement pass budget', LTarget.Presentation.Renderer.State
          .Value(NyxColumnArrangementStatusState).TextValue) > 0,
          'real managed Fit displays the operational cell-budget refusal');
      end
      else
      begin
        Check((LColumns.CaptureColumns.Serial = LSerial + 1) and
          (LManager.Observation.ColumnAt(0).Width > 120), 'real Fit measures the complete path rather than its three-line clipped label');
      end;
    end;
    Check((LView.Store.Snapshot.Revision = 0) and (TNyxCodec.Encode(LDocument) = LAuthored),
      'runtime presentation and Fit preserve exact persisted design and accepted source revision');
    LRenderer.Unmount;
    Check(not LMount.Connected, 'renderer retirement ends this exact text-layout input incarnation');
    {$ifndef PAS2JS}Check(GErrors.Text = '', 'native presentation journey has no deferred errors');{$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-presentation-control-progress', IntToStr(GChecks));
    {$else}
    WriteLn('presentation phase checks=', GChecks, ' arranged=', AArranged);
    {$endif}
  finally
    LDrafts := nil;
    LEdits := nil;
    LTarget := nil;
    LPresenter := nil;
    LManager := nil;
    LManagerMount := nil;
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
    {$ifdef PAS2JS}
    await(Run(True));
    await(Run(False));
    await(Run(True, True));
    document.body.setAttribute('data-presentation-controls', 'passed');
    document.body.setAttribute('data-presentation-control-checks', IntToStr(GChecks));
    {$else}
    Run(True);
    Run(False);
    Run(True, True);
    WriteLn('PASS ', GChecks, ' actual presentation control checks; no external effects');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-presentation-controls', 'failed');
      document.body.setAttribute('data-presentation-control-checks', IntToStr(GChecks));
      document.body.setAttribute('data-presentation-control-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  GClientWidth := 1280;
  {$ifndef PAS2JS}

  if ParamCount > 0 then
  begin
    GClientWidth := StrToInt(ParamStr(1));
  end;

  if (GClientWidth <> 390) and (GClientWidth <> 1280) then
  begin
    raise EArgumentException.Create('Presentation controls expect native client width390 or1280');
  end;
  Application.Initialize;
  Application.ShowMainForm := False;
  GErrors := TLayoutErrors.Create;
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
