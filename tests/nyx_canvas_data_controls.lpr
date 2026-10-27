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
program nyx_canvas_data_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Math, Forms, Controls, Grids, StdCtrls, LCLIntf, LCLType,
  LMessages, Types,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.state, nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.columns, nyx.collections.query, nyx.collections.selection,
  nyx.collections.presentation,
  nyx.collections.text.layout,
  nyx.collections.arrangement,
  nyx.content.projection.lcl,
  nyx.collections.lcl.grid, nyx.canvas.geometry, nyx.canvas.scene,
  nyx.canvas.workspace, nyx.canvas.lcl, nyx.test.capture.lcl;

type
  { Exposes installed LCL input entries on the borrowed actual grid. These
    calls qualify native dispatch, separately from physical hardware input. }
  TGridInput = class(TNyxCollectionStringGrid);

  { Application borrows this exception receiver for the test's native lifetime.
    Deferred LCL failures remain process failures instead of opening a modal. }
  TFailureReceiver = class
  public
    Failure: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

const
  CNamePrefix: TNyxText = 'Service 🌙 ';
  CDescription: TNyxText = 'A live service with useful details. Keep the full description ' +
    'available while browsing, sorting and zooming this table.';
  CZooms: array[0..4] of Double = (1, 0.5, 1.5, 0.5, 1);

var
  GDocument: TNyxDocument;
  GWorkspace: INyxCanvasWorkspace;
  GScene: INyxCanvasScene;
  GView: INyxLCLCanvasWorkspaceView;
  GData: INyxCollectionView;
  GHost: TForm;
  GGrid: TNyxCollectionStringGrid;
  GItem: TNyxCanvasItemRef;
  GFailures: TFailureReceiver;
  GChecks: Integer;
  GStage: Integer;
  GBaseColumns: array[0..3] of Integer;
  GBaseRowHeight: Integer;
  GBaseHeaderHeight: Integer;
  GBaseTitleFont: Integer;
  GBaseSampleHeight: Integer;
  GTopRow: Integer;
  GLeftColumn: Integer;
  GWrapped: Boolean;
  GArranged: Boolean;
  GPinned: Boolean;
  GBefore: TNyxText;

function Services: TNyxCollectionRef;
begin
  Result := NyxCollection('services');
end;

function Service(const AID: TNyxText): TNyxItemRef;
begin
  Result := NyxItem(Services, AID);
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TFailureReceiver.Failed(ASender: TObject; AException: Exception);
begin
  Failure := AException.Message;
end;

procedure Pump;
begin
  Application.ProcessMessages;

  if GFailures.Failure <> '' then
  begin
    raise Exception.Create(GFailures.Failure);
  end;
end;

{ This is a real bound table, including its native vertical/horizontal viewport.
  The authored document stays independent of runtime selection/query commands. }
procedure Build;
var
  LRows: array[0..39] of TNyxCollectionItem;
  LIndex: Integer;
  LCard: INyxCard;
  LTable: INyxTable;
  LSpec: TNyxCollectionViewSpec;
  LLayer: TNyxCanvasLayerRef;
begin
  GDocument := TNyxDocument.Create;
  for LIndex := Low(LRows) to High(LRows) do
  begin
    LRows[LIndex] := NyxCollectionItem(Service(TNyxText(IntToStr(LIndex))))
      .WithValue(NyxTextField('name'), CNamePrefix + TNyxText(IntToStr(LIndex)))
      .WithValue(NyxTextField('status'), 'Running')
      .WithValue(NyxIntegerField('requests'), LIndex)
      .WithValue(NyxTextField('description'), CDescription);
  end;
  GDocument.Collections.Define(Services,
    NyxCollectionSchema.Text(NyxTextField('name'), '')
      .Text(NyxTextField('status'), '').Integer(NyxIntegerField('requests'), 0)
      .Text(NyxTextField('description'), ''), LRows);
  GDocument.AddPage(NewNyxPage('home'));
  LCard := NewNyxCard('service-card');
  LCard.Configure.Padding(20).Gap(12).Done;
  LCard.Add(NewNyxHeading('service-title').WithText('Services at a glance'));
  LCard.Add(NewNyxLabel('service-help').WithText(
    'Keep your place while zooming. Columns and rows belong to the same logical table.'));
  LTable := NewNyxTable('service-table');
  LTable.Configure.Height(300).Done;
  LSpec := NyxCollectionView(Services)
    .Column(NyxTextField('name'), 'Service', cmEditable)
      .ColumnSizing(NyxColumnSizing.Fixed(220));

  if GPinned then
  begin
    LSpec := LSpec.ColumnLayout(NyxColumnLayout.PinStart);
  end;
  LSpec := LSpec.Column(NyxTextField('status'), 'Status').ColumnSizing(NyxColumnSizing.Fixed(180));

  if GPinned then
  begin
    LSpec := LSpec.ColumnLayout(NyxColumnLayout.PinEnd.AllowOnly([nclfAutoSize]));
  end;
  LSpec := LSpec
    .Column(NyxIntegerField('requests'), 'Requests').ColumnSizing(NyxColumnSizing.Fixed(140))
    .Column(NyxTextField('description'), 'Description').ColumnSizing(NyxColumnSizing.Fixed(260));

  if GWrapped then
  begin
    LSpec := LSpec.ColumnText(NyxColumnText.Wrap(3))
      .TablePresentation(NyxTablePresentation.Density(ntrdComfortable));
  end;

  if GArranged then
  begin
    LSpec := LSpec.TableLayout(NyxTableLayout);
  end;
  LTable.Binds.Collection(LSpec);
  LCard.Add(LTable);
  LCard.Add(NewNyxLabel('service-result').WithText('Selection and source stay yours.'));
  GScene := NewNyxCanvasScene;
  LLayer := NyxCanvasLayer('Services');
  GScene.AddLayer(LLayer);
  GItem := NyxCanvasItem('Service table');
  GScene.AddControl(GItem, NyxCanvasPlacement(LLayer,
    NyxCanvasBounds(0, 0, 720, 600)), LCard);
  GWorkspace := NewNyxCanvasWorkspace('service-workspace', GScene)
    .WithScaling(nccsUniform)
    .WithCamera(NyxCanvasCamera(1280, 800, NyxCanvasPoint(360, 300)));
  GBefore := TNyxCodec.Encode(GDocument);
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(20, 20, StrToIntDef(ParamStr(1), 1280), 800);
  GHost.Show;
  GView := MountNyxLCLCanvasWorkspace(GWorkspace, GDocument, GHost);
  Pump;
  GGrid := TNyxCollectionStringGrid(GView.RendererFor(GItem).ControlFor('service-table'));
  GData := GView.RendererFor(GItem).CollectionView('service-table');
  GData.Select(Service('5'));
  Pump;
  GGrid.SetFocus;
  GGrid.TopRow := 4;
  GGrid.LeftCol := 1;
  Pump;
  GTopRow := GGrid.TopRow;
  GLeftColumn := GGrid.LeftCol;
  for LIndex := Low(GBaseColumns) to High(GBaseColumns) do
  begin
    GBaseColumns[LIndex] := GGrid.ColWidths[LIndex];
  end;
  GBaseRowHeight := GGrid.DefaultRowHeight;
  GBaseHeaderHeight := GGrid.RowHeights[0];
  GBaseTitleFont := GGrid.TitleFont.Height;
  GBaseSampleHeight := GGrid.RowHeights[6];
end;

procedure CheckMetrics(AZoom: Double);
var
  LIndex: Integer;
  LSampleRow: Integer;
  LLayout: TNyxTableTextLayout;
begin
  for LIndex := Low(GBaseColumns) to High(GBaseColumns) do
  begin
    Check(Abs(GGrid.ColWidths[LIndex] - Round(GBaseColumns[LIndex] * AZoom)) <= 1,
      'column ' + TNyxText(IntToStr(LIndex)) + ' scales from its logical width');
  end;
  Check(Abs(GGrid.DefaultRowHeight - Round(GBaseRowHeight * AZoom)) <= 1,
    'body rows scale with their retained logical metrics');
  Check(Abs(GGrid.RowHeights[0] - Round(GBaseHeaderHeight * AZoom)) <= 1,
    'the fixed header scales with the table');
  Check(Abs(GGrid.TitleFont.Height - Round(GBaseTitleFont * AZoom)) <= 1,
    'header typography scales with body typography / actual ' +
      TNyxText(IntToStr(GGrid.TitleFont.Height)) + ' baseline ' +
      TNyxText(IntToStr(GBaseTitleFont)) + ' zoom ' + TNyxText(FloatToStr(AZoom)));
  LSampleRow := 6;

  if GWrapped then
  begin
    LSampleRow := Max(1, GGrid.TopRow);
  end;
  Check(Abs(GGrid.RowHeights[LSampleRow] - Round(GBaseSampleHeight * AZoom)) <= 1,
    'an explicitly measured body row keeps its logical height');

  if GWrapped then
  begin
    Check(GGrid.CellTextLayout(3, LSampleRow, LLayout),
      'the visible wrapped body row has an actual text layout');
    Check(LLayout.Admission = nttaReady,
      'the wrapped text metric is usable / ' + LLayout.Diagnostic +
      ' / SystemFont ' + TNyxText(BoolToStr(GGrid.DefaultTextStyle.SystemFont, True)) +
      ' orientation ' + TNyxText(IntToStr(GGrid.Font.Orientation)));
    Check(LLayout.Count >= 2, 'the admitted description occupies real wrapped lines');
  end;
end;

{ Genuine native F2 enters the bound cell editor. Zoom changes its physical
  face while the same unfinished UTF-8 draft, selection and store stay intact. }
procedure CheckEditorZoom;
var
  LEditor: TCustomEdit;
  LRevision: Integer;
  LIndex: Integer;
begin
  GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(360, 300), 1));
  GGrid.Col := 0;
  GGrid.Row := 6;
  GGrid.SetFocus;
  SendMessage(GGrid.Handle, LM_KEYDOWN, VK_F2, 0);
  Pump;
  Check(GGrid.EditorMode and (GGrid.Editor is TCustomEdit),
    'native F2 enters the actual bound cell editor');
  LEditor := TCustomEdit(GGrid.Editor);
  LRevision := GData.Store.Snapshot.Revision;
  LEditor.Text := TNyxText('Unfinished service / 🌙');
  LEditor.SelStart := 2;
  LEditor.SelLength := 5;
  for LIndex := Low(CZooms) to High(CZooms) do
  begin
    GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(360, 300), CZooms[LIndex]));
    Pump;
    Check(GGrid.EditorMode and (GGrid.Editor = LEditor),
      'zoom retains the exact unfinished cell editor');
    Check(TNyxText(LEditor.Text) = TNyxText('Unfinished service / 🌙'),
      'zoom preserves supplementary Unicode in the unfinished draft');
    Check((LEditor.SelStart = 2) and (LEditor.SelLength = 5),
      'zoom preserves the unfinished native caret and selection');
    Check(GData.Store.Snapshot.Revision = LRevision,
      'geometry never commits an unfinished cell edit');
  end;
  SendMessage(LEditor.Handle, LM_KEYDOWN, VK_ESCAPE, 0);
  Pump;
  Check(GData.Store.Snapshot.Revision = LRevision,
    'cancelling the real editor leaves accepted rows unchanged');
end;

{ Drag the actual header at half-size zoom. The preference is logical width,
  independent of native monitor DPI and this canvas's physical allocation. }
procedure CheckColumnResize;
var
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LRect: TRect;
  LIndex: Integer;
  LPreference: Integer;
  LExpected: Integer;
  LScale: Double;
  LBefore: TNyxText;
begin
  GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(360, 300), 0.5));
  GGrid.LeftCol := 0;
  Pump;
  LColumns := NyxCollectionColumns(GData);
  LRect := GGrid.CellRect(0, 0);
  TGridInput(GGrid).MouseMove([], LRect.Right - 1, LRect.Bottom - 5);
  TGridInput(GGrid).MouseDown(mbLeft, [], LRect.Right - 1, LRect.Bottom - 5);
  TGridInput(GGrid).MouseMove([ssLeft], LRect.Right + 29, LRect.Bottom - 5);
  TGridInput(GGrid).MouseUp(mbLeft, [], LRect.Right + 29, LRect.Bottom - 5);
  for LIndex := 1 to 3 do
  begin
    Pump;
    Application.Idle(False);
  end;
  LCapture := LColumns.CaptureColumns;
  LPreference := LCapture.Preferences.IndexOf(NyxColumnIdentity(NyxTextField('name')));
  Check(LPreference >= 0, 'the real native header drag publishes a column preference');
  LScale := Max(1, GGrid.Font.PixelsPerInch) / 96 * 0.5;
  LExpected := Round(GGrid.ColWidths[0] / LScale);
  Check(LCapture.Preferences.ColumnAt(LPreference).HasWidth and
    (LCapture.Preferences.ColumnAt(LPreference).WidthValue = LExpected),
    'header dragging stores world width, applying monitor DPI and zoom once');
  Check(LExpected > 220, 'the real resize increases the authored column allocation');
  GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(360, 300), 1));
  Pump;
  Check(Abs(GGrid.ColWidths[0] - Round(LExpected * Max(1, GGrid.Font.PixelsPerInch) / 96)) <= 1,
    'the user width remains logical after returning to full-size zoom');
  LBefore := LColumns.CaptureColumns.Preferences.ToData.ToJSON;
  LRect := GGrid.CellRect(0, 0);
  TGridInput(GGrid).MouseMove([], LRect.Right - 1, LRect.Bottom - 5);
  TGridInput(GGrid).MouseDown(mbLeft, [], LRect.Right - 1, LRect.Bottom - 5);
  TGridInput(GGrid).MouseMove([ssLeft], LRect.Right + 19, LRect.Bottom - 5);
  GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(360, 300), 0.5));
  TGridInput(GGrid).MouseUp(mbLeft, [], LRect.Right + 19, LRect.Bottom - 5);
  for LIndex := 1 to 3 do
  begin
    Pump;
    Application.Idle(False);
  end;
  Check(LColumns.CaptureColumns.Preferences.ToData.ToJSON = LBefore,
    'a zoom change retires a header drag before its late native release');
end;

{ An entered logical section pins its original native frame. Retirement may
  release the renderer, but closing this lease cannot touch a successor tree. }
procedure CheckLogicalRetirement;
var
  LUpdate: INyxLCLLogicalUpdate;
  LNested: INyxLCLLogicalUpdate;
begin
  GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(360, 300), 0.5));
  LUpdate := GGrid.BeginLogicalUpdate;
  Check(LUpdate <> nil, 'an accepted grid can open one logical metric section');
  Check(GGrid.ContentProjectionScale = 1,
    'the admitted section exposes logical geometry to native adapter work');
  LNested := GGrid.BeginLogicalUpdate;
  Check(LNested = nil, 'nested updates reuse the existing logical section');
  GView.Disconnect;
  LUpdate.Close;
  LUpdate.Close;
  LUpdate := nil;
  Pump;
  Check(not GView.Connected, 'retired logical work closes idempotently without reviving a renderer');
end;

{ Use the actual native Ctrl+Shift+F route on one logical field at each zoom.
  The physical glyph size is not the user's new persisted column preference. }
procedure CheckColumnFit(AGrid: TNyxCollectionStringGrid; APhysical: Integer);
var
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LPreference: Integer;
  LWidth: Integer;
  LExpected: Integer;
  LIndex: Integer;
  LTurn: Integer;
  LKey: Word;
begin
  LColumns := NyxCollectionColumns(GData);
  LExpected := 0;
  for LIndex := Low(CZooms) to High(CZooms) do
  begin
    GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(360, 300), CZooms[LIndex]));
    AGrid.Col := APhysical;
    AGrid.Row := 6;
    AGrid.SetFocus;
    Pump;
    LKey := Ord('F');
    TGridInput(AGrid).KeyDown(LKey, [ssCtrl, ssShift]);
    for LTurn := 1 to 3 do
    begin
      Pump;
      Application.Idle(False);
    end;
    LCapture := LColumns.CaptureColumns;
    LPreference := LCapture.Preferences.IndexOf(NyxColumnIdentity(NyxTextField('name')));
    Check(LPreference >= 0, 'native Fit publishes its actual typed field preference');
    LWidth := LCapture.Preferences.ColumnAt(LPreference).WidthValue;

    if LIndex = Low(CZooms) then
    begin
      LExpected := LWidth;
    end;
    Check(LWidth = LExpected,
      'Fit keeps the same logical width across zoom / actual ' + TNyxText(IntToStr(LWidth)) +
      ' expected ' + TNyxText(IntToStr(LExpected)) +
      ' zoom ' + TNyxText(FloatToStr(CZooms[LIndex])));
  end;
end;

{ Resolve a typed fixture field in its actual native grid or pinned pane. The
  bounded visible header walk observes physical controls, not a private mount. }
function FieldGrid(const AField: TNyxColumnIdentity; out AColumn: Integer):
  TNyxCollectionStringGrid;
var
  LTitle: TNyxText;
  LIndex: Integer;

  function Matches(AGrid: TNyxCollectionStringGrid): Boolean;
  var
    LColumn: Integer;
  begin
    Result := False;
    for LColumn := 0 to AGrid.ColCount - 1 do
    begin

      if Pos(LTitle, TNyxText(AGrid.Cells[LColumn, 0])) = 1 then
      begin
        AColumn := LColumn;
        Exit(True);
      end;
    end;
  end;

begin
  LTitle := GData.Spec.ColumnAt(GData.Spec.ColumnIndex(AField)).Title;
  AColumn := -1;
  Result := GGrid;

  if Matches(Result) then
  begin
    Exit;
  end;
  for LIndex := 0 to GGrid.ControlCount - 1 do
  begin

    if (GGrid.Controls[LIndex] is TNyxCollectionStringGrid) and
      GGrid.Controls[LIndex].Visible then
    begin
      Result := TNyxCollectionStringGrid(GGrid.Controls[LIndex]);

      if Matches(Result) then
      begin
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

procedure CheckPinnedRows;
var
  LStart: TNyxCollectionStringGrid;
  LEnd: TNyxCollectionStringGrid;
  LStartColumn: Integer;
  LEndColumn: Integer;
  LDescriptionColumn: Integer;
  LIndex: Integer;
  LRow: Integer;
  LLayout: TNyxTableTextLayout;
  LEditor: TCustomEdit;
  LRevision: Integer;
  LStartPoint: TPoint;
  LEndPoint: TPoint;
  LMiddlePoint: TPoint;
  LKey: Word;
  LTurn: Integer;
  LFit: TNyxCollectionFitButton;
  LFitWidth: Integer;
  LFitHeight: Integer;

  procedure CheckRows;
  var
    LRow: Integer;
    LMainPoint: TPoint;
    LStartPoint: TPoint;
    LEndPoint: TPoint;
  begin
    Check((LStart.TopRow = GGrid.TopRow) and (LEnd.TopRow = GGrid.TopRow),
      'both pinned panes share the actual scrolling-middle row anchor');
    for LRow := GGrid.TopRow to Min(GGrid.RowCount - 1, GGrid.TopRow + 2) do
    begin
      Check((Abs(LStart.RowHeights[LRow] - GGrid.RowHeights[LRow]) <= 1) and
        (Abs(LEnd.RowHeights[LRow] - GGrid.RowHeights[LRow]) <= 1),
        'pinned body heights match the wrapped middle / row ' + TNyxText(IntToStr(LRow)));
      LMainPoint := GGrid.ClientToScreen(GGrid.CellRect(LDescriptionColumn, LRow).TopLeft);
      LStartPoint := LStart.ClientToScreen(LStart.CellRect(LStartColumn, LRow).TopLeft);
      LEndPoint := LEnd.ClientToScreen(LEnd.CellRect(LEndColumn, LRow).TopLeft);
      Check((Abs(LMainPoint.Y - LStartPoint.Y) <= 1) and
        (Abs(LMainPoint.Y - LEndPoint.Y) <= 1),
        'both pinned cell tops align with the real middle on screen');
    end;
    Check(GGrid.CellTextLayout(LDescriptionColumn, GGrid.TopRow, LLayout) and
      (LLayout.Admission = nttaReady) and (LLayout.Count >= 2),
      'the current pinned-table viewport contains usable wrapped descriptions');
    Check(LFit.Visible and (Abs(LFit.Width - Round(LFitWidth * CZooms[GStage])) <= 1) and
      (Abs(LFit.Height - Round(LFitHeight * CZooms[GStage])) <= 1),
      'the real pinned Fit button keeps its projected chrome after scrolling');
  end;

begin
  LStart := FieldGrid(NyxColumnIdentity(NyxTextField('name')), LStartColumn);
  LEnd := FieldGrid(NyxColumnIdentity(NyxTextField('status')), LEndColumn);
  Check((LStart <> nil) and (LEnd <> nil) and (LStart <> GGrid) and
    (LEnd <> GGrid) and (LStart <> LEnd), 'the table has two distinct real pinned editor panes');
  Check(FieldGrid(NyxColumnIdentity(NyxTextField('description')), LDescriptionColumn) = GGrid,
    'wrapped description remains in the real scrolling middle');
  LFit := nil;
  for LIndex := 0 to LEnd.ControlCount - 1 do
  begin

    if LEnd.Controls[LIndex] is TNyxCollectionFitButton then
    begin
      LFit := TNyxCollectionFitButton(LEnd.Controls[LIndex]);
      Break;
    end;
  end;
  Check((LFit <> nil) and LFit.Visible, 'the pinned auto-fit-only field exposes its actual button');
  LFitWidth := LFit.Width;
  LFitHeight := LFit.Height;
  for LIndex := Low(CZooms) to High(CZooms) do
  begin
    GStage := LIndex;
    GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(360, 300), CZooms[LIndex]));
    Pump;
    Check(FieldGrid(NyxColumnIdentity(NyxTextField('name')), LRow) = LStart,
      'zoom retains the original pinned editable name pane');
    CheckRows;
    GGrid.TopRow := 7 + LIndex;
    Pump;
    CheckRows;
    GGrid.LeftCol := GGrid.FixedCols;
    Pump;
    LStartPoint := LStart.ClientToScreen(Point(0, 0));
    LEndPoint := LEnd.ClientToScreen(Point(0, 0));
    LMiddlePoint := GGrid.ClientToScreen(GGrid.CellRect(LDescriptionColumn, GGrid.TopRow).TopLeft);
    GGrid.LeftCol := Min(GGrid.ColCount - 2, GGrid.FixedCols + 1);
    Pump;
    Check(GGrid.ClientToScreen(GGrid.CellRect(LDescriptionColumn, GGrid.TopRow).TopLeft).X <
      LMiddlePoint.X - 8 * CZooms[LIndex], 'horizontal scrolling moves the actual middle cell');
    Check((LStart.ClientToScreen(Point(0, 0)).X = LStartPoint.X) and
      (LEnd.ClientToScreen(Point(0, 0)).X = LEndPoint.X),
      'both real pinned edges remain fixed during horizontal scrolling at each zoom');
  end;
  GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(360, 300), 0.5));
  GStage := 1;
  GData.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxIntegerField('requests'), nsdDescending));
  Pump;
  CheckRows;
  Check(GData.Selection.Contains(Service('5')), 'query refresh retains the selected pinned-table item');
  CheckColumnFit(LStart, LStartColumn);
  LStart.Col := LStartColumn;
  LStart.Row := GData.Snapshot.IndexOf(Service('5')) + 1;
  LStart.SetFocus;
  SendMessage(LStart.Handle, LM_KEYDOWN, VK_F2, 0);
  Pump;
  Check(LStart.EditorMode and (LStart.Editor is TCustomEdit),
    'native F2 enters the actual pinned editor');
  LEditor := TCustomEdit(LStart.Editor);
  LRevision := GData.Store.Snapshot.Revision;
  LEditor.Text := TNyxText('Pinned draft / 🌙');
  LEditor.SelStart := 2;
  LEditor.SelLength := 4;
  for LIndex := Low(CZooms) to High(CZooms) do
  begin
    GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(360, 300), CZooms[LIndex]));
    Pump;
    LKey := Ord('F');
    TGridInput(LStart).KeyDown(LKey, [ssCtrl, ssShift]);
    for LTurn := 1 to 3 do
    begin
      Pump;
      Application.Idle(False);
    end;
    Check((LStart.Editor = LEditor) and LStart.EditorMode and
      (TNyxText(LEditor.Text) = TNyxText('Pinned draft / 🌙')) and
      (LEditor.SelStart = 2) and (LEditor.SelLength = 4),
      'zoom preserves the same pinned editor, Unicode draft and caret');
    Check(GData.Store.Snapshot.Revision = LRevision, 'pinned zoom never commits an unfinished draft');
  end;
  SendMessage(LEditor.Handle, LM_KEYDOWN, VK_ESCAPE, 0);
  Pump;
end;

procedure Run;
var
  LIndex: Integer;
  LZoom: Double;
begin
  Build;
  Check((GData <> nil) and (GData.Snapshot.Count = 40), 'the real collection remains bound');

  if GPinned then
  begin
    CheckPinnedRows;
    Check(TNyxCodec.Encode(GDocument) = GBefore, 'pinned runtime work leaves the authored document exact');
    GView.Execute(nccFit);
    Pump;

    if ParamCount >= 2 then
    begin
      SaveNyxNativeCapture(GHost, ParamStr(2), ncmPrint);
    end;
    CheckLogicalRetirement;
    Exit;
  end;

  if GWrapped then
  begin
    Check((GGrid.ProjectionRowCount > 0) and (GGrid.ProjectionRowCount < 40),
      'only the bounded measured body window joins row projection');
  end;
  for LIndex := Low(CZooms) to High(CZooms) do
  begin
    GStage := LIndex;
    LZoom := CZooms[LIndex];
    GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(360, 300), LZoom));
    Pump;
    Check(GView.RendererFor(GItem).ControlFor('service-table') = GGrid,
      'zoom retains the original native table');
    CheckMetrics(LZoom);
    Check((GGrid.TopRow = GTopRow) and (GGrid.LeftCol = GLeftColumn),
      'zoom retains vertical and horizontal scroll anchors');
    Check(GData.Selection.Contains(Service('5')), 'zoom retains selected item identity');
    Check(TNyxText(GGrid.Cells[0, 6]) = CNamePrefix + TNyxText('5'),
      'zoom retains the exact accepted Unicode cell');
  end;
  GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(360, 300), 0.5));
  GStage := Length(CZooms);
  GData.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxIntegerField('requests'), nsdDescending));
  Pump;
  Check(GView.RendererFor(GItem).ControlFor('service-table') = GGrid,
    'a query refresh retains the original native table');
  Check(GData.Selection.Contains(Service('5')), 'sorting a zoomed table retains selected identity');
  Check(GData.Snapshot.ItemAt(0).Ref.ID = '39', 'sorting uses the live typed query');
  CheckMetrics(0.5);
  GData.ConfigureQuery(NyxCollectionQuery);
  Pump;
  CheckMetrics(0.5);
  CheckEditorZoom;

  if GArranged then
  begin
    CheckColumnResize;
    CheckColumnFit(GGrid, 0);
  end;
  Check(TNyxCodec.Encode(GDocument) = GBefore, 'runtime zoom/query leave the authored document exact');
  GView.Execute(nccFit);
  Pump;

  if ParamCount >= 2 then
  begin
    SaveNyxNativeCapture(GHost, ParamStr(2), ncmPrint);
  end;
  CheckLogicalRetirement;
end;

begin
  Application.Initialize;
  GPinned := ParamStr(3) = 'pinned';
  GWrapped := (ParamStr(3) = 'wrapped') or GPinned;
  GArranged := (ParamStr(3) = 'arranged') or GPinned;
  GFailures := TFailureReceiver.Create;
  Application.OnException := GFailures.Failed;
  try
    try
      Run;
      WriteLn('PASS ', GChecks, ' live canvas data-control checks');
    except
      on LException: Exception do
      begin
        WriteLn('FAIL at stage ', GStage, ' after ', GChecks, ' checks / ', LException.Message);
        ExitCode := 1;
      end;
    end;
  finally

    if GView <> nil then
    begin
      GView.Disconnect;
    end;
    GData := nil;
    GView := nil;
    GWorkspace := nil;
    GScene := nil;
    GHost.Free;
    GDocument.Free;
    Application.OnException := nil;
    GFailures.Free;
  end;
end.
