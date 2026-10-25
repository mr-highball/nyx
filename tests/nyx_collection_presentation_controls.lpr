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
  nyx.collections.text.layout, nyx.collections.alignment, nyx.publication
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.collections.arrangement.browser
  {$else}, Interfaces, Forms, Controls, Grids, StdCtrls, Graphics, LCLType,
  LMessages, LCLIntf, Windows, nyx.render.lcl, nyx.collections.lcl, nyx.collections.lcl.grid,
  nyx.collections.arrangement.lcl, nyx.editing, nyx.editing.lcl, nyx.test.capture.lcl{$endif};

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
  { The DOM Range measurement API exposes the actual glyph bounds for this
    fixture's borrowed rendered text. It neither edits text nor owns a host. }
  TAlignmentRange = class external name 'Range'(TJSRange)
  public
    function getBoundingClientRect: TJSDOMRect;
  end;
  {$else}
  { Deferred native errors are captured, never hidden behind a modal dialog. }
  TLayoutErrors = class
  public
    Text: TNyxText;
    procedure Failed(Sender: TObject; E: Exception);
  end;
  { Inspect the real inherited native prepare/paint style after the mount's
    installed reader runs; this does not synthesize trusted hardware input. }
  TAlignmentGridAccess = class(TNyxCollectionStringGrid)
  public
    function PreparedStyle(AColumn, ARow: Integer): TTextStyle;
  end;
  { Expose the standard native edit's actual alignment without replacing its
    producer or reading a Nyx test-only mirror of the presentation policy. }
  TAlignmentEditAccess = class(TCustomEdit);
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
  WriteLn('Deferred native failure: ', E.ClassName, ': ', E.Message);
  DumpExceptionBackTrace(Output);
end;

function TAlignmentGridAccess.PreparedStyle(AColumn, ARow: Integer): TTextStyle;
begin
  PrepareCanvas(AColumn, ARow, []);
  Result := Canvas.TextStyle;
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

function Fixture(AArranged: Boolean; APresentation: Boolean = True;
  AAlignment: Boolean = False): TNyxDocument;
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

    if APresentation then
    begin
      LSpec := LSpec.ColumnText(NyxColumnText.Wrap(3));
    end;

    if AAlignment then
    begin
      LSpec := LSpec.ColumnAlignment(NyxColumnAlignment.Horizontal(nchaStart).Vertical(ncvaTop));
    end;
    LSpec := LSpec.Column(NyxNumberField('Amount'), 'Amount', cmEditable)
        .ColumnSizing(NyxColumnSizing.Fixed(90));

    if AAlignment then
    begin
      LSpec := LSpec.ColumnAlignment(NyxColumnAlignment.Horizontal(nchaEnd).Vertical(ncvaBottom));
    end;
    LSpec := LSpec.Column(NyxTextField('Notes'), 'Notes')
        .ColumnSizing(NyxColumnSizing.Fixed(100));

    if AArranged then
    begin
      LSpec := LSpec.ColumnLayout(NyxColumnLayout.PinEnd);
    end;

    if APresentation then
    begin
      LSpec := LSpec.ColumnText(NyxColumnText.Ellipsis);
    end;
    LSpec := LSpec.Column(NyxTextField('Clip'), 'Clip')
        .ColumnSizing(NyxColumnSizing.Fixed(80));

    if APresentation then
    begin
      LSpec := LSpec.ColumnText(NyxColumnText.Clip);
    end;
    LSpec := LSpec
      .Column(NyxTextField('Budget'), 'Budget')
        .ColumnSizing(NyxColumnSizing.Fixed(100));

    if APresentation then
    begin
      LSpec := LSpec.ColumnText(NyxColumnText.Wrap(1000))
        .TablePresentation(NyxTablePresentation.Density(ntrdCompact));
    end;

    if AArranged then
    begin
      LSpec := LSpec.TableLayout(NyxTableLayout);

      if APresentation then
      begin
        LSpec := LSpec.TablePresentation(LSpec.TablePresentationPolicy
          .Header(NyxTableHeader.Lines(1).MinimumHeight(32)));
      end;
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
    LLabel := TJSHTMLElement(LTable.querySelector('thead th'));
    Check((LLabel.getAttribute('data-nyx-header-lines') = '1') = AArranged,
      'browser captions apply the explicit header budget only to its authored table');
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
    Check((LNameGrid.RowHeights[0] = LHeader) and
      (LNotesGrid.RowHeights[0] = LHeader),
      'both logical pin headers retain the same independently reserved space');
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
    Check(LView.Spec.TablePresentationPolicy.HasHeader = AArranged,
      'runtime body density preserves explicit header presence and legacy absence');
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

{ Runtime policy is deliberately absent from the complete document. Both paths
  use real renderer-managed controls: one applies before a new mount, the other
  while an initially ordinary editor is composing. A peer remains independent. }
procedure RuntimeCases(AInitial: Boolean); {$ifdef PAS2JS}async;{$endif}
var
  LDocument: TNyxDocument;
  LState: TNyxState;
  LView: INyxCollectionView;
  LPeer: INyxCollectionView;
  LMount: INyxCollectionMount;
  LColumns: INyxCollectionColumns;
  LPresentation: INyxCollectionTablePresentation;
  LCapture: INyxCollectionPresentationCapture;
  LPrepared: INyxPreparedPublication;
  LRejected: INyxPreparedPublication;
  LAdmission: INyxCollectionInputAdmission;
  LOldInput: INyxCollectionInputLifetime;
  LEdits: INyxCollectionEditMount;
  LDrafts: INyxCollectionEditsSnapshot;
  LSource: INyxCollectionSnapshot;
  LPeerTable: INyxTable;
  LSpec: TNyxCollectionViewSpec;
  LPrefs: TNyxTablePresentationPreferences;
  LAuthored: TNyxText;
  LPeerPreferences: TNyxText;
  LHeight: Integer;
  LPeerHeight: Integer;
  LSerial: Integer;
  LRefused: Boolean;
  LFound: Boolean;
  I: Integer;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LSurface: TJSHTMLElement;
  LTable: TJSHTMLElement;
  LPeerElement: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LLabel: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LInit: TJSObject;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LSurface: TForm;
  LGrid: TNyxCollectionStringGrid;
  LPeerGrid: TNyxCollectionStringGrid;
  LEditor: TCustomEdit;
  LLayout: TNyxTableTextLayout;
  LRaw: TStringGrid;
  LRawMount: INyxCollectionMount;
  {$endif}
begin
  LDocument := nil;
  LState := nil;
  LRenderer := nil;
  LSurface := nil;
  {$ifndef PAS2JS}LRaw := nil;{$endif}
  try
    LDocument := Fixture(False, False);
    LSpec := LDocument.Find('layout-table').CollectionView;
    LPeerTable := NewNyxTable('layout-peer');
    LPeerTable.Configure.Height(200).Done;
    LPeerTable.Binds.Collection(LSpec);
    LDocument.Pages[0].Add(LPeerTable);
    LDocument.Validate;
    LAuthored := TNyxCodec.Encode(LDocument);
    LState := LDocument.State.Clone;
    {$ifdef PAS2JS}
    LSurface := TJSHTMLElement(document.createElement('div'));
    LSurface.style.setProperty('width', IntToStr(Max(1, Round(window.innerWidth) - 16)) + 'px');
    document.body.appendChild(LSurface);
    LRenderer := TNyxBrowserRenderer.Create;
    LRenderer.Render(LDocument, LDocument.Pages[0], LSurface, False, LState);
    LTable := TJSHTMLElement(LRenderer.ElementFor('layout-table'));
    LPeerElement := TJSHTMLElement(LRenderer.ElementFor('layout-peer'));
    {$else}
    LSurface := TForm.CreateNew(nil);
    LSurface.SetBounds(20, 20, GClientWidth, 650);
    LSurface.ClientWidth := GClientWidth;
    LSurface.Show;
    LRenderer := TNyxLCLRenderer.Create;
    LRenderer.Render(LDocument, LDocument.Pages[0], LSurface, False, LState);
    LGrid := TNyxCollectionStringGrid(LRenderer.ControlFor('layout-table'));
    LPeerGrid := TNyxCollectionStringGrid(LRenderer.ControlFor('layout-peer'));
    {$endif}
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    LView := LRenderer.CollectionView('layout-table');
    LPeer := LRenderer.CollectionView('layout-peer');
    LMount := LRenderer.CollectionMount('layout-table');
    LPresentation := NyxCollectionTablePresentation(LView);
    LColumns := NyxCollectionColumns(LView);
    LSource := LView.Store.Snapshot;
    LPeerPreferences := NyxCollectionTablePresentation(LPeer).CapturePresentation.Preferences.ToData.ToJSON;
    Check(not LView.Spec.HasPresentationPolicy and not LView.Spec.HasLayoutControls,
      'runtime-only controls begin with exact authored text density and arrangement absence');
    {$ifdef PAS2JS}
    LHeight := Round(TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"]')).offsetHeight);
    LPeerHeight := Round(TJSHTMLElement(LPeerElement.querySelector('[data-nyx-item="one"]')).offsetHeight);
    Check(LTable.querySelector('[data-nyx-text-layout]') = nil,
      'an initially absent browser table keeps its ordinary text path');
    {$else}
    LHeight := LGrid.RowHeights[1];
    LPeerHeight := LPeerGrid.RowHeights[1];
    Check(not LGrid.CellTextLayout(0, 1, LLayout),
      'an initially absent native table keeps its ordinary text path');
    {$endif}
    LPrefs := NyxTablePresentationPreferences.Density(ntrdComfortable).WithColumn(
      NyxColumnTextPreference(NyxColumnIdentity(NyxTextField('Name')), NyxColumnText.Wrap(3)));
    LCapture := LPresentation.CapturePresentation;
    LSerial := LCapture.Serial;
    LPrepared := LPresentation.PreparePresentation(LCapture, LPrefs);
    LRefused := False;
    try
      LRejected := LColumns.PrepareColumns(LColumns.CaptureColumns, NyxColumnPreferences);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'runtime-only presentation reserves the original shared column command slot');
    LPrepared.Retire;
    LPrepared := nil;
    Check(LPresentation.CapturePresentation.Serial = LSerial,
      'cancelling an uninstalled runtime-only plan leaves the original serial and physical table');

    if AInitial then
    begin
      Check(Supports(LMount, INyxCollectionInputAdmission, LAdmission),
        'renderer mount exposes its canonical input incarnation');
      LOldInput := LAdmission.GetInputLifetime;
      LMount.Disconnect;
      ApplyPresentation(LView, LPrefs);
      LMount := LRenderer.BindCollection('layout-table', LView);
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      Check(not LOldInput.Current and LMount.Connected,
        'a new observed mount adopts already accepted runtime preferences without reviving old input');
      {$ifdef PAS2JS}
      Check(LTable.querySelector('[data-nyx-text-layout="ellipsized"]') <> nil,
        'pre-mount runtime text choices activate actual browser wrapping');
      {$else}
      Check(LGrid.CellTextLayout(0, 1, LLayout) and (LLayout.Count = 3),
        'pre-mount runtime text choices activate actual native wrapping');
      {$endif}
      ApplyPresentation(LView, NyxTablePresentationPreferences);
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    end;
    {$ifdef PAS2JS}
    LCell := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="1"]'));
    LCell.focus;
    LInit := TJSObject.new;
    LInit['key'] := 'F2';
    LInit['bubbles'] := True;
    LInit['cancelable'] := True;
    LCell.dispatchEvent(TLayoutKey.new('keydown', LInit));
    LInput := TJSHTMLInputElement(LCell.querySelector('input'));
    LInput.value := '-';
    LInput.setSelectionRange(1, 1);
    LInput.dispatchEvent(TLayoutComposition.new('compositionstart', LInit));
    {$else}
    LGrid.Col := 1;
    LGrid.Row := 1;
    LGrid.SetFocus;
    LCLIntf.SendMessage(LGrid.Handle, LM_KEYDOWN, VK_F2, 0);
    Check(LGrid.EditorMode and (LGrid.Editor is TCustomEdit),
      'initially unconfigured native table enters its original numeric editor');
    LEditor := TCustomEdit(LGrid.Editor);
    RestoreNyxLCLInputText(LEditor, '-');
    LEditor.SelStart := 1;
    LEditor.SelLength := 0;
    { Own-window message injection qualifies the installed composition bridge,
      never trusted hardware, a physical IME session or an OS clipboard effect. }
    Windows.SendMessage(LEditor.Handle, WM_IME_STARTCOMPOSITION, 0, 0);
    Drain;
    {$endif}
    LCapture := LPresentation.CapturePresentation;
    LSerial := LCapture.Serial;
    ApplyPresentation(LView, LPrefs);
    ApplyPresentation(LView, NyxTablePresentationPreferences);
    ApplyPresentation(LView, LPrefs);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(LPresentation.CapturePresentation.Serial = LSerial + 3,
      'runtime set reset and set publish distinct canonical serials while composition holds geometry');
    {$ifdef PAS2JS}
    Check((LCell.querySelector('input') = LInput) and (document.activeElement = LInput) and
      (LInput.value = '-') and (LInput.selectionStart = 1) and
      (TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"]')).offsetHeight = LHeight),
      'first runtime activation defers browser geometry and preserves the composing editor');
    LInput.dispatchEvent(TLayoutComposition.new('compositionend', LInit));
    {$else}
    Check(LGrid.EditorMode and (LGrid.Editor = LEditor) and (NyxLCLInputText(LEditor) = '-') and
      (LEditor.SelStart = 1) and (LGrid.RowHeights[1] = LHeight),
      'first runtime activation defers native geometry and preserves the composing editor');
    Windows.SendMessage(LEditor.Handle, WM_IME_ENDCOMPOSITION, 0, 0);
    {$endif}
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    {$ifdef PAS2JS}
    LLabel := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="0"] .nyx-cell-value'));
    Check((LLabel.getAttribute('data-nyx-text-layout') = 'ellipsized') and
      (LLabel.getBoundingClientRect.height >= 3 * JS.parseFloat(
        window.getComputedStyle(LLabel).getPropertyValue('font-size'))) and
      (TJSHTMLElement(LLabel.parentElement).style.getPropertyValue('padding-top') = '10px'),
      'completed composition applies the latest runtime-only wrap and density');
    {$else}
    Check(LGrid.CellTextLayout(0, 1, LLayout) and (LLayout.Count = 3) and
      (LGrid.RowHeights[1] >= 3 * Max(1, Abs(LGrid.Font.Height))),
      'completed composition applies the latest runtime-only wrap and density');
    {$endif}
    LRefused := False;
    try
      LRejected := LPresentation.PreparePresentation(LCapture, NyxTablePresentationPreferences);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'runtime-only preference ABA cannot revive the original presentation capture');
    ApplyPresentation(LView, NyxTablePresentationPreferences);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    {$ifdef PAS2JS}
    Check((LTable.querySelector('[data-nyx-text-layout]') = nil) and
      (TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"]')).offsetHeight = LHeight),
      'runtime reset removes generated browser styles and restores ordinary row geometry');
    Check(TJSHTMLElement(LPeerElement.querySelector('[data-nyx-item="one"]')).offsetHeight = LPeerHeight,
      'peer browser row geometry remains independent of another runtime table');
    {$else}
    Check(not LGrid.CellTextLayout(0, 1, LLayout) and (LGrid.RowHeights[1] = LHeight),
      'runtime reset clears native measured lines and restores ordinary row geometry');
    Check(LPeerGrid.RowHeights[1] = LPeerHeight,
      'peer native row geometry remains independent of another runtime table');
    {$endif}
    Check(Supports(LMount, INyxCollectionEditMount, LEdits),
      'runtime activation retains the ordinary typed draft handoff');
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
    Check(LFound, 'runtime activation and reset retain the exact incomplete accepted-cell draft');
    LEdits.ResumeCellEdits(LDrafts, False);
    LDrafts := nil;
    Check((TNyxCodec.Encode(LDocument) = LAuthored) and
      not LView.Spec.HasPresentationPolicy and (LView.Store.Snapshot = LSource) and
      (LView.CellText(NyxItem(LSpec.Key, 'one'), 0) = CPath) and
      (LView.CellText(NyxItem(LSpec.Key, 'one'), 2) = CNotes) and
      (LView.CellText(NyxItem(LSpec.Key, 'one'), 3) = CClip) and
      (NyxCollectionTablePresentation(LPeer).CapturePresentation.Preferences.ToData.ToJSON = LPeerPreferences),
      'runtime-only choices preserve authored bytes RAW Unicode NUL line endings source and peer preferences');
    LRenderer.Unmount;
    Check(not LMount.Connected, 'runtime-only retirement ends the original observed mount');
    {$ifndef PAS2JS}

    if not AInitial then
    begin
      LRaw := TStringGrid.Create(LSurface);
      LRaw.Parent := LSurface;
      ApplyPresentation(LView, LPrefs);
      LRefused := False;
      try
        LRawMount := MountNyxLCLCollection(LRaw, LView);
      except
        on ENyxCollection do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LRawMount = nil),
        'initial runtime presentation refuses an unobserved direct native grid honestly');
      ApplyPresentation(LView, NyxTablePresentationPreferences);
      LRawMount := MountNyxLCLCollection(LRaw, LView);
      LRefused := False;
      try
        ApplyPresentation(LView, LPrefs);
      except
        on ENyxPublicationNotification do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and LRawMount.Connected and not LView.Spec.HasPresentationPolicy and
        LPresentation.CapturePresentation.Preferences.HasDensity,
        'late unsupported native activation reports committed model notification failure without inventing host support');
      ApplyPresentation(LView, NyxTablePresentationPreferences);
      LRawMount.Disconnect;
      LRawMount := nil;
      FreeAndNil(LRaw);
    end;
    Check(GErrors.Text = '', 'runtime-only native controls leave no deferred error');
    {$endif}
  finally
    {$ifndef PAS2JS}

    if LRawMount <> nil then
    begin
      LRawMount.Disconnect;
    end;
    LRawMount := nil;
    LRaw.Free;
    {$endif}
    LDrafts := nil;
    LEdits := nil;
    LPrepared := nil;
    LRejected := nil;
    LOldInput := nil;
    LAdmission := nil;
    LCapture := nil;
    LPresentation := nil;
    LColumns := nil;
    LMount := nil;
    LPeer := nil;
    LView := nil;
    LSource := nil;
    LPeerTable := nil;
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

{ Authored and runtime alignment share the existing input, column publication
  and composition owners. Exercise ordinary and pinned physical producers,
  including the actual unfinished native/browser editor and independent axes. }
procedure AlignmentCases(AArranged: Boolean; AOnlyRuntime: Boolean = False); {$ifdef PAS2JS}async;{$endif}
var
  LDocument: TNyxDocument;
  LState: TNyxState;
  LView: INyxCollectionView;
  LMount: INyxCollectionMount;
  LPresentation: INyxCollectionTablePresentation;
  LPreferences: TNyxTablePresentationPreferences;
  LSource: INyxCollectionSnapshot;
  LAuthored: TNyxText;
  LSerial: Integer;
  LName: TNyxColumnIdentity;
  LAmount: TNyxColumnIdentity;
  LAdmission: INyxCollectionInputAdmission;
  LFormerInput: INyxCollectionInputLifetime;
  LManagerMount: INyxCollectionColumnArrangementMount;
  LManager: INyxColumnArrangementManager;
  LPresenter: INyxColumnArrangementPresenter;
  LColumns: INyxCollectionColumns;
  LAction: TNyxText;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LSurface: TJSHTMLElement;
  LTable: TJSHTMLElement;
  LNameCell: TJSHTMLElement;
  LAmountCell: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LInit: TJSObject;
  LTarget: INyxBrowserColumnArrangementPresenter;
  LRange: TAlignmentRange;
  LTextBounds: TJSDOMRect;
  LCellBounds: TJSDOMRect;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LSurface: TForm;
  LGrid: TNyxCollectionStringGrid;
  LNameGrid: TNyxCollectionStringGrid;
  LAmountGrid: TNyxCollectionStringGrid;
  LNameColumn: Integer;
  LAmountColumn: Integer;
  LEditor: TCustomEdit;
  LStyle: TTextStyle;
  LTarget: INyxLCLColumnArrangementPresenter;

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
  { An invisible native editor may retain its admitted text/range while LCL
    deliberately drops its HWND during a style change. Later handle creation
    must restore the same scalar range without accepting that parked draft. }
  procedure HiddenAlignmentSelection;
  var
    LDraft: TNyxText;
    LSelection: TNyxTextSelection;
    LRestored: TNyxTextSelection;
    LNativeStart: Integer;
    LNativeLength: Integer;
    LWindow: HWND;
  begin
    LAmountGrid := Field('Amount', LAmountColumn);
    LAmountGrid.Col := LAmountColumn;
    LAmountGrid.Row := 1;
    LAmountGrid.SetFocus;
    LCLIntf.SendMessage(LAmountGrid.Handle, LM_KEYDOWN, VK_F2, 0);
    Check(LAmountGrid.EditorMode and (LAmountGrid.Editor is TCustomEdit),
      'hidden alignment regression enters the actual original text editor');
    LEditor := TCustomEdit(LAmountGrid.Editor);
    LDraft := '-🌙 unfinished';
    RestoreNyxLCLInputText(LEditor, LDraft);
    LSelection := NyxTextSelection(LDraft, NyxTextScalarCount('-'),
      NyxTextScalarCount('-') + 1, ntdUnknown);
    SelectNyxLCLText(LEditor, LSelection);
    LNativeStart := LEditor.SelStart;
    LNativeLength := LEditor.SelLength;
    Check((LNativeLength = 2) and CaptureNyxLCLSelection(LEditor).SameRange(LSelection),
      'actual live editor selection crosses the supplementary scalar in native UTF16');
    LAmountGrid.BeginPresentationChange;
    try
      LAmountGrid.EditorMode := False;
    finally
      LAmountGrid.EndPresentationChange;
    end;
    Check(not LEditor.Visible and LEditor.HandleAllocated,
      'ordinary hide parks the admitted native editor before alignment changes');
    ApplyPresentation(LView, NyxTablePresentationPreferences.WithAlignment(
      NyxColumnAlignmentPreference(LAmount, NyxColumnAlignment.Horizontal(nchaStart))));
    Drain;
    Check(not LEditor.HandleAllocated and (NyxLCLInputText(LEditor) = LDraft) and
      (LEditor.SelStart = LNativeStart) and (LEditor.SelLength = LNativeLength),
      'hidden style recreation preserves exact cached native range without allocating an HWND');
    { Request the normal LCL handle explicitly as the host would on reveal.
      The production restore itself above must leave the hidden edit windowless. }
    LWindow := LEditor.Handle;
    LRestored := CaptureNyxLCLSelection(LEditor);
    Check((LWindow <> 0) and (NyxLCLInputText(LEditor) = LDraft) and
      LRestored.Defined and LRestored.SameRange(LSelection) and not LEditor.Focused,
      'later native handle creation restores the exact supplementary range without stealing focus');
    Check((LView.Store.Snapshot = LSource) and (TNyxCodec.Encode(LDocument) = LAuthored),
      'hidden editor alignment never accepts its draft or rewrites authored policy');
    ApplyPresentation(LView, NyxTablePresentationPreferences);
    Drain;
  end;
  {$endif}
begin
  LDocument := nil;
  LState := nil;
  LRenderer := nil;
  LSurface := nil;
  try
    LDocument := Fixture(AArranged, not AOnlyRuntime, not AOnlyRuntime);
    LAuthored := TNyxCodec.Encode(LDocument);
    LState := LDocument.State.Clone;
    {$ifdef PAS2JS}
    LSurface := TJSHTMLElement(document.createElement('div'));
    LSurface.style.setProperty('width', IntToStr(Max(1, Round(window.innerWidth) - 16)) + 'px');
    document.body.appendChild(LSurface);
    LRenderer := TNyxBrowserRenderer.Create;
    LRenderer.Render(LDocument, LDocument.Pages[0], LSurface, False, LState);
    LTable := TJSHTMLElement(LRenderer.ElementFor('layout-table'));
    {$else}
    LSurface := TForm.CreateNew(nil);
    LSurface.SetBounds(20, 20, GClientWidth, 650);
    LSurface.ClientWidth := GClientWidth;
    LSurface.Show;
    LRenderer := TNyxLCLRenderer.Create;
    LRenderer.Render(LDocument, LDocument.Pages[0], LSurface, False, LState);
    LGrid := TNyxCollectionStringGrid(LRenderer.ControlFor('layout-table'));
    {$endif}
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    LView := LRenderer.CollectionView('layout-table');
    LMount := LRenderer.CollectionMount('layout-table');
    LPresentation := NyxCollectionTablePresentation(LView);
    LSource := LView.Store.Snapshot;
    LName := NyxColumnIdentity(NyxTextField('Name'));
    LAmount := NyxColumnIdentity(NyxNumberField('Amount'));

    if AOnlyRuntime then
    begin
      Check(not LView.Spec.HasPresentationPolicy and not LView.Spec.HasLayoutControls,
        'alignment-only activation starts without any saved text density alignment or arrangement');
      LPreferences := NyxTablePresentationPreferences.WithAlignment(
        NyxColumnAlignmentPreference(LAmount,
          NyxColumnAlignment.Horizontal(nchaEnd).Vertical(ncvaBottom)));
      {$ifdef PAS2JS}
      LAmountCell := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="1"]'));
      LAmountCell.focus;
      LInit := TJSObject.new;
      LInit['key'] := 'F2';
      LInit['bubbles'] := True;
      LInit['cancelable'] := True;
      LAmountCell.dispatchEvent(TLayoutKey.new('keydown', LInit));
      LInput := TJSHTMLInputElement(LAmountCell.querySelector('input'));
      LInput.value := '-';
      LInput.setSelectionRange(1, 1);
      LInput.dispatchEvent(TLayoutComposition.new('compositionstart', LInit));
      {$else}
      LAmountGrid := Field('Amount', LAmountColumn);
      LAmountGrid.Col := LAmountColumn;
      LAmountGrid.Row := 1;
      LAmountGrid.SetFocus;
      LCLIntf.SendMessage(LAmountGrid.Handle, LM_KEYDOWN, VK_F2, 0);
      Check(LAmountGrid.EditorMode and (LAmountGrid.Editor is TCustomEdit),
        'unaligned ordinary native cell enters its original editor before runtime activation');
      LEditor := TCustomEdit(LAmountGrid.Editor);
      RestoreNyxLCLInputText(LEditor, '-');
      LEditor.SelStart := 1;
      LEditor.SelLength := 0;
      Windows.SendMessage(LEditor.Handle, WM_IME_STARTCOMPOSITION, 0, 0);
      {$endif}
      LSerial := LPresentation.CapturePresentation.Serial;
      ApplyPresentation(LView, LPreferences);
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      Check((LPresentation.CapturePresentation.Serial = LSerial + 1) and
        not LView.Spec.HasPresentationPolicy,
        'alignment-only runtime publication uses the canonical serial without authored opt-in');
      {$ifdef PAS2JS}
      Check((LInput.value = '-') and (LInput.selectionStart = 1) and
        (LAmountCell.style.getPropertyValue('text-align') = ''),
        'first alignment-only activation keeps the composing browser editor and inherited style');
      LInput.dispatchEvent(TLayoutComposition.new('compositionend', LInit));
      {$else}
      Check((NyxLCLInputText(LEditor) = '-') and (LEditor.SelStart = 1) and
        (TAlignmentEditAccess(LEditor).Alignment = taLeftJustify),
        'first alignment-only activation keeps the composing native editor and inherited style');
      Windows.SendMessage(LEditor.Handle, WM_IME_ENDCOMPOSITION, 0, 0);
      {$endif}
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      {$ifdef PAS2JS}
      Check((LInput.value = '-') and (LInput.selectionStart = 1) and
        (document.activeElement = LInput) and
        (window.getComputedStyle(LInput).getPropertyValue('text-align') = 'end') and
        (window.getComputedStyle(LAmountCell).getPropertyValue('vertical-align') = 'bottom'),
        'only runtime alignment activates browser paint and the exact retained draft after composition');
      {$else}
      LStyle := TAlignmentGridAccess(LAmountGrid).PreparedStyle(LAmountColumn, 1);
      Check((LStyle.Alignment = taRightJustify) and (LStyle.Layout = tlBottom) and
        (LAmountGrid.Editor = LEditor) and LEditor.Focused and
        (NyxLCLInputText(LEditor) = '-') and (LEditor.SelStart = 1) and
        (TAlignmentEditAccess(LEditor).Alignment = taRightJustify),
        'only runtime alignment activates native paint and the exact retained draft after composition');
      {$endif}
      ApplyPresentation(LView, NyxTablePresentationPreferences);
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      {$ifdef PAS2JS}
      Check((LAmountCell.style.getPropertyValue('text-align') = '') and
        (LAmountCell.style.getPropertyValue('vertical-align') = '') and
        (LInput.style.getPropertyValue('text-align') = '') and (LInput.value = '-'),
        'alignment-only browser reset restores inherited axes without accepting a draft');
      {$else}
      LStyle := TAlignmentGridAccess(LAmountGrid).PreparedStyle(LAmountColumn, 1);
      Check((LStyle.Alignment = taLeftJustify) and
        (TAlignmentEditAccess(LEditor).Alignment = taLeftJustify) and
        (NyxLCLInputText(LEditor) = '-') and (LEditor.SelStart = 1),
        'alignment-only native reset restores inherited axes without accepting a draft');
      {$endif}
      Check(Supports(LMount, INyxCollectionInputAdmission, LAdmission),
        'alignment-only mount retains its original canonical input lifetime');
      LFormerInput := LAdmission.GetInputLifetime;
      LMount.Disconnect;
      ApplyPresentation(LView, LPreferences);
      LMount := LRenderer.BindCollection('layout-table', LView);
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      Check(not LFormerInput.Current and LMount.Connected,
        'initial alignment-only preferences activate a fresh mount while the old input stays retired');
      {$ifdef PAS2JS}
      LAmountCell := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="1"]'));
      Check((window.getComputedStyle(LAmountCell).getPropertyValue('text-align') = 'end') and
        (window.getComputedStyle(LAmountCell).getPropertyValue('vertical-align') = 'bottom'),
        'fresh browser mount reads nonempty alignment-only runtime presence');
      {$else}
      LAmountGrid := Field('Amount', LAmountColumn);
      LStyle := TAlignmentGridAccess(LAmountGrid).PreparedStyle(LAmountColumn, 1);
      Check((LStyle.Alignment = taRightJustify) and (LStyle.Layout = tlBottom),
        'fresh native mount reads nonempty alignment-only runtime presence');
      {$endif}
      Check((TNyxCodec.Encode(LDocument) = LAuthored) and (LView.Store.Snapshot = LSource) and
        (LView.CellText(NyxItem(LView.Spec.Key, 'one'), 1) = '1234.5'),
        'runtime-only alignment initial and reset paths preserve exact saved absence and accepted data');
      LRenderer.Unmount;
      Check(not LMount.Connected, 'alignment-only native/browser retirement disconnects the fresh mount');
      Exit;
    end;
    Check(LView.Spec.HasColumnAlignment and
      (LView.Spec.ToData.Field('version').AsInteger = 12),
      'real table admits exact authored alignment through the ordinary descriptor');
    {$ifdef PAS2JS}
    LNameCell := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="0"]'));
    LAmountCell := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="1"]'));
    Check((window.getComputedStyle(LNameCell).getPropertyValue('text-align') = 'start') and
      (window.getComputedStyle(LNameCell).getPropertyValue('vertical-align') = 'top') and
      (window.getComputedStyle(LAmountCell).getPropertyValue('text-align') = 'end') and
      (window.getComputedStyle(LAmountCell).getPropertyValue('vertical-align') = 'bottom'),
      'actual browser body cells use independent authored logical and vertical alignment');
    LAmountCell.focus;
    LInit := TJSObject.new;
    LInit['key'] := 'F2';
    LInit['bubbles'] := True;
    LInit['cancelable'] := True;
    LAmountCell.dispatchEvent(TLayoutKey.new('keydown', LInit));
    LInput := TJSHTMLInputElement(LAmountCell.querySelector('input'));
    LInput.value := '-';
    LInput.setSelectionRange(1, 1);
    Check(window.getComputedStyle(LInput).getPropertyValue('text-align') = 'end',
      'actual browser numeric editor follows explicit alignment without guessing its scalar family');
    LInput.dispatchEvent(TLayoutComposition.new('compositionstart', LInit));
    {$else}
    LNameGrid := Field('Name', LNameColumn);
    LAmountGrid := Field('Amount', LAmountColumn);
    Check((LNameGrid <> nil) and (LAmountGrid <> nil),
      'native alignment resolves canonical fields across the actual ordinary or pinned panes');
    LStyle := TAlignmentGridAccess(LNameGrid).PreparedStyle(LNameColumn, 1);
    Check((LStyle.Alignment = taLeftJustify) and (LStyle.Layout = tlTop),
      'actual native wrapped text prepare uses authored Start Top');
    LStyle := TAlignmentGridAccess(LAmountGrid).PreparedStyle(LAmountColumn, 1);
    Check((LStyle.Alignment = taRightJustify) and (LStyle.Layout = tlBottom),
      'actual native numeric text prepare uses authored End Bottom without data inference');
    LAmountGrid.Col := LAmountColumn;
    LAmountGrid.Row := 1;
    LAmountGrid.SetFocus;
    LCLIntf.SendMessage(LAmountGrid.Handle, LM_KEYDOWN, VK_F2, 0);
    Check(LAmountGrid.EditorMode and (LAmountGrid.Editor is TCustomEdit),
      'aligned native numeric cell enters the original editor');
    LEditor := TCustomEdit(LAmountGrid.Editor);
    RestoreNyxLCLInputText(LEditor, '-');
    LEditor.SelStart := 1;
    LEditor.SelLength := 0;
    Check(TAlignmentEditAccess(LEditor).Alignment = taRightJustify,
      'actual native editor follows the same explicit logical alignment');
    Windows.SendMessage(LEditor.Handle, WM_IME_STARTCOMPOSITION, 0, 0);
    {$endif}
    LSerial := LPresentation.CapturePresentation.Serial;
    LPreferences := NyxTablePresentationPreferences
      .WithAlignment(NyxColumnAlignmentPreference(LName,
        NyxColumnAlignment.Horizontal(nchaCenter)))
      .WithAlignment(NyxColumnAlignmentPreference(LAmount,
        NyxColumnAlignment.Horizontal(nchaStart).Vertical(ncvaMiddle)));
    ApplyPresentation(LView, LPreferences);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check(LPresentation.CapturePresentation.Serial = LSerial + 1,
      'alignment publishes through the existing canonical column serial');
    {$ifdef PAS2JS}
    Check((document.activeElement = LInput) and (LInput.value = '-') and
      (LInput.selectionStart = 1) and
      (window.getComputedStyle(LInput).getPropertyValue('text-align') = 'end'),
      'browser composition keeps original alignment unfinished entry focus and caret');
    LInput.dispatchEvent(TLayoutComposition.new('compositionend', LInit));
    {$else}
    Check((LAmountGrid.Editor = LEditor) and (NyxLCLInputText(LEditor) = '-') and
      (LEditor.SelStart = 1) and (TAlignmentEditAccess(LEditor).Alignment = taRightJustify),
      'native composition keeps original alignment unfinished entry focus and caret');
    Windows.SendMessage(LEditor.Handle, WM_IME_ENDCOMPOSITION, 0, 0);
    {$endif}
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    {$ifdef PAS2JS}
    Check((LAmountCell.querySelector('input') = LInput) and (LInput.value = '-') and
      (LInput.selectionStart = 1) and (document.activeElement = LInput) and
      (window.getComputedStyle(LInput).getPropertyValue('text-align') = 'start') and
      (window.getComputedStyle(LAmountCell).getPropertyValue('vertical-align') = 'middle') and
      (window.getComputedStyle(LNameCell).getPropertyValue('text-align') = 'center') and
      (window.getComputedStyle(LNameCell).getPropertyValue('vertical-align') = 'top'),
      'browser applies completed alignment with separate inherited axis and exact draft');
    {$else}
    LStyle := TAlignmentGridAccess(LNameGrid).PreparedStyle(LNameColumn, 1);
    Check((LStyle.Alignment = taCenter) and (LStyle.Layout = tlTop),
      'native runtime horizontal alignment retains the independent authored vertical axis');
    LStyle := TAlignmentGridAccess(LAmountGrid).PreparedStyle(LAmountColumn, 1);
    Check((LStyle.Alignment = taLeftJustify) and (LStyle.Layout = tlCenter) and
      (LAmountGrid.Editor = LEditor) and (NyxLCLInputText(LEditor) = '-') and
      (LEditor.SelStart = 1) and LEditor.Focused and
      (TAlignmentEditAccess(LEditor).Alignment = taLeftJustify),
      'native completed alignment preserves the actual draft object caret focus through HWND styling');
    {$endif}
    LPreferences := LPreferences.WithoutAlignment(LAmount);
    ApplyPresentation(LView, LPreferences);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    {$ifdef PAS2JS}
    Check((window.getComputedStyle(LInput).getPropertyValue('text-align') = 'end') and
      (window.getComputedStyle(LAmountCell).getPropertyValue('vertical-align') = 'bottom') and
      (window.getComputedStyle(LNameCell).getPropertyValue('text-align') = 'center'),
      'browser resetting one runtime field restores its authored axes independently');
    {$else}
    LStyle := TAlignmentGridAccess(LAmountGrid).PreparedStyle(LAmountColumn, 1);
    Check((LStyle.Alignment = taRightJustify) and (LStyle.Layout = tlBottom) and
      (TAlignmentEditAccess(LEditor).Alignment = taRightJustify) and
      (NyxLCLInputText(LEditor) = '-') and (LEditor.SelStart = 1),
      'native resetting one runtime field restores authored axes without accepting its draft');
    {$endif}
    ApplyPresentation(LView, NyxTablePresentationPreferences);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}

    if AArranged then
    begin
      LColumns := NyxCollectionColumns(LView);
      ApplyPresentation(LView, NyxTablePresentationPreferences.WithAlignment(
        NyxColumnAlignmentPreference(LName, NyxColumnAlignment.Horizontal(nchaEnd))));
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      Check(Supports(LMount, INyxCollectionColumnArrangementMount, LManagerMount),
        'aligned table retains its original managed Fit owner');
      LManager := LManagerMount.ColumnArrangement;
      LManager.Open;
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      LPresenter := LManager.Presenter;
      LSerial := LColumns.CaptureColumns.Serial;
      LAction := NyxColumnArrangementActionID(NyxColumnArrangementEditorID,
        LManager.Observation, LName, ncaaFitWidth);
      {$ifdef PAS2JS}
      Check(Supports(LPresenter, INyxBrowserColumnArrangementPresenter, LTarget),
        'aligned browser Fit exposes the actual retained managed action');
      LTarget.Presentation.Renderer.Reveal(LAction, niRuntime);
      TJSHTMLElement(LTarget.Presentation.Renderer.ElementFor(LAction)).click;
      await(Drain);
      {$else}
      Check(Supports(LPresenter, INyxLCLColumnArrangementPresenter, LTarget),
        'aligned native Fit exposes the actual retained managed action');
      LTarget.Presentation.Renderer.Reveal(LAction, niRuntime);
      TButton(LTarget.Presentation.Renderer.ControlFor(LAction)).Click;
      Drain;
      {$endif}
      Check((LColumns.CaptureColumns.Serial = LSerial + 1) and
        LManager.Observation.ColumnAt(0).HasWidth and
        (LManager.Observation.ColumnAt(0).Width > 120),
        'actual Fit publishes full text measurement while explicit alignment is active');
      LPresenter.Close;
      ApplyPresentation(LView, NyxTablePresentationPreferences);
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    end;
    Check((TNyxCodec.Encode(LDocument) = LAuthored) and (LView.Store.Snapshot = LSource) and
      (LView.CellText(NyxItem(LView.Spec.Key, 'one'), 1) = '1234.5') and
      (LView.CellText(NyxItem(LView.Spec.Key, 'one'), 0) = CPath),
      'all alignment changes preserve authored wire accepted scalar comparison text and complete RAW');
    {$ifdef PAS2JS}
    LInput.focus;
    LInit['key'] := 'Escape';
    LInput.dispatchEvent(TLayoutKey.new('keydown', LInit));
    LTable.style.setProperty('direction', 'rtl');
    LMount.Refresh;
    await(Drain);
    LAmountCell := TJSHTMLElement(LTable.querySelector('[data-nyx-item="one"] [data-nyx-column="1"]'));
    LRange := TAlignmentRange(document.createRange);
    LRange.selectNodeContents(LAmountCell.querySelector('.nyx-cell-value'));
    LTextBounds := LRange.getBoundingClientRect;
    LCellBounds := LAmountCell.getBoundingClientRect;
    Check((window.getComputedStyle(LAmountCell).getPropertyValue('direction') = 'rtl') and
      (window.getComputedStyle(LAmountCell).getPropertyValue('text-align') = 'end') and
      (LTextBounds.width > 0) and (LTextBounds.left <= LCellBounds.left + 12) and
      (LTextBounds.right <= LCellBounds.right - 12),
      'actual browser RTL End places the rendered numeric glyphs at the logical trailing edge');
    await(Capture('alignment-rtl-' + LowerCase(BoolToStr(AArranged, True))));
    {$else}

    if LEditor.CanFocus then
    begin
      LEditor.SetFocus;
    end;
    LCLIntf.SendMessage(LEditor.Handle, LM_KEYDOWN, VK_ESCAPE, 0);
    LGrid.BiDiMode := bdRightToLeft;
    LMount.Refresh;
    Drain;
    LNameGrid := Field('Name', LNameColumn);
    LAmountGrid := Field('Amount', LAmountColumn);
    LStyle := TAlignmentGridAccess(LNameGrid).PreparedStyle(LNameColumn, 1);
    Check((LStyle.Alignment = taRightJustify) and (LStyle.Layout = tlTop),
      'actual native RTL Start mirrors the authored wrapped body text');
    LStyle := TAlignmentGridAccess(LAmountGrid).PreparedStyle(LAmountColumn, 1);
    Check((LStyle.Alignment = taLeftJustify) and (LStyle.Layout = tlBottom),
      'actual native RTL End mirrors numeric body text without guessing from its value');
    HiddenAlignmentSelection;
    {$endif}
    Check((LView.Store.Snapshot = LSource) and (TNyxCodec.Encode(LDocument) = LAuthored),
      'direction layout and editor cancellation do not publish data or rewrite saved alignment');
    LRenderer.Unmount;
    Check(not LMount.Connected, 'aligned table retirement ends the original input mount');
    {$ifndef PAS2JS}
    Check(GErrors.Text = '', 'alignment control journey leaves no deferred native failure: ' + GErrors.Text);
    {$endif}
  finally
    LFormerInput := nil;
    LAdmission := nil;
    LTarget := nil;
    LColumns := nil;
    LPresenter := nil;
    LManager := nil;
    LManagerMount := nil;
    LPresentation := nil;
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
    await(RuntimeCases(True));
    await(RuntimeCases(False));
    await(AlignmentCases(True));
    await(AlignmentCases(False));
    await(AlignmentCases(False, True));
    document.body.setAttribute('data-presentation-controls', 'passed');
    document.body.setAttribute('data-presentation-control-checks', IntToStr(GChecks));
    {$else}
    Run(True);
    Run(False);
    Run(True, True);
    RuntimeCases(True);
    RuntimeCases(False);
    AlignmentCases(True);
    AlignmentCases(False);
    AlignmentCases(False, True);
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
