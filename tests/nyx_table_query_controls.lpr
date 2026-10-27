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
program nyx_table_query_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils,
  {$ifdef PAS2JS}JS, Web, nyx.application.browser, nyx.render.browser,
    nyx.collections.query.browser,
  {$else}Classes, Types, Interfaces, Forms, Controls, StdCtrls, Grids, Graphics,
    LazFileUtils,
    LCLType, nyx.application.lcl, nyx.render.lcl, nyx.collections.query.lcl,
  {$endif}
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen, nyx.contract,
  nyx.source, nyx.schema, nyx.data, nyx.state, nyx.events, nyx.behavior,
  nyx.scheduler, nyx.publication, nyx.collections, nyx.collections.query,
  nyx.collections.query.policy, nyx.collections.query.session,
  nyx.collections.query.editor, nyx.collections.query.controls,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.selection,
  { BEGIN COLUMN ARRANGEMENT IMPORTS }
  nyx.collections.arrangement, nyx.collections.columns,
  nyx.collections.arrangement.controls, nyx.collections.arrangement.presenter,
  {$ifdef PAS2JS}nyx.collections.arrangement.browser,{$else}
  nyx.collections.arrangement.lcl, nyx.collections.lcl.grid,{$endif}
  nyx.application.state, nyx.collections.bindings, nyx.composition,
  nyx.popover, nyx.editing,
  {$ifdef PAS2JS}nyx.editing.browser,{$else}nyx.editing.lcl,{$endif}
  {$ifndef PAS2JS}ExtCtrls, Windows,{$endif}
  { END COLUMN ARRANGEMENT IMPORTS }
  nyx.collections.paging, nyx.collections.mount, nyx.menu.editor, nyx.popover.types;

type
  {$ifdef PAS2JS}
  TReviewApplication = TNyxBrowserApplication;
  TReviewRenderer = TNyxBrowserRenderer;
  { Standard browser input construction reaches the actual DOM handlers. These
    synthesized events make no trusted hardware or assistive technology claim. }
  TReviewKey = class external name 'KeyboardEvent'(TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  TReviewMouse = class external name 'MouseEvent'(TJSMouseEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TReviewApplication = TNyxLCLApplication;
  TReviewRenderer = TNyxLCLRenderer;
  { Protected-access bridge enters installed LCL's complete focus, header hit,
    resize/drag admission and release path, rather than invoking its callbacks. }
  TGridAccess = class(TStringGrid);
  { Widgetset exceptions must fail this consumer rather than open a blocking
    message dialog or disappear from the physical input outcome. }
  TFailureObserver = class
    Failure: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}
  { A normal named receiver decodes copied Details and optionally vetoes or
    fails a proposal. No fake participant or alternate query engine is used. }
  TQueryReceiver = class(TNyxEventCallback)
  public
    BeforeCalls, ChangedCalls: Integer;
    Veto, Fail, AdvanceClear: Boolean;
    Workbench: INyxCollectionQueryWorkbench;
    LastBefore, LastChanged: TNyxQueryWorkbenchChange;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  { An ordered UI queue barrier proves the earlier captured Apply has actually
    reached execution. Completion, not a fixed delay, owns asynchronous checks. }
  TQueueBarrier = class(TInterfacedObject, INyxWork)
  public
    Done: Boolean;
    procedure Execute(const AExecution: INyxExecution);
  end;

const
  CFirst = 'people-first';
  CSecond = 'people-second';
  CLiteral = 'literal-table';
  CStatus = 'nyx-query-workbench-status';
  CCounts = 'nyx-query-workbench-counts';
  CChip = 'nyx-query-workbench-chip-0';

var
  GDocument: TNyxDocument;
  GApp: TReviewApplication;
  GViews: array of INyxCollectionView;
  GWorkbenches: array of INyxCollectionQueryWorkbench;
  GSource: INyxCollectionSnapshot;
  GAuthored: TNyxText;
  GEvidence: String;
  GChecks, GWidth: Integer;
  GReceiver: TQueryReceiver;
  GReceiverLease: INyxEventCallback;
  GDesignReceiver: TQueryReceiver;
  GDesignLease: INyxEventCallback;
  GBeforeToken, GChangedToken, GDesignToken: INyxEventSubscription;
  {$ifdef PAS2JS}GHost: TJSHTMLElement;{$else}GFailures: TFailureObserver;{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if (GFailures <> nil) and (GFailures.Failure <> '') then
  begin
    raise Exception.Create('Native query callback: ' + GFailures.Failure);
  end;
  {$endif}

  if not ACondition then
  begin
    raise Exception.Create('Table query controls: ' + AReason);
  end;
  Inc(GChecks);
end;

function People: TNyxCollectionRef;
begin
  Result := NyxCollection('people');
end;

function PersonName: TNyxTextFieldRef;
begin
  Result := NyxTextField('name');
end;

function Score: TNyxIntegerFieldRef;
begin
  Result := NyxIntegerField('score');
end;

function Active: TNyxBooleanFieldRef;
begin
  Result := NyxBooleanField('active');
end;

function Person(const AID: TNyxText): TNyxItemRef;
begin
  Result := NyxItem(People, AID);
end;

function Baseline: TNyxCollectionQuery;
begin
  Result := NyxCollectionQuery.Where(NyxWhere(Active).EqualTo(True))
    .OrderBy(PersonName, nsdDescending);
end;

function Spec(ASecond: Boolean): TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(People).Column(PersonName, 'Name')
    .ColumnQuery(NyxColumnQuery.TextComparison(nqtAsciiInsensitive))
    .Column(Score, 'Score').ColumnQuery(NyxColumnQuery.Operators(
      [nqcEqual, nqcAtLeast, nqcAtMost]))
    .Column(Active, 'Active').Selection(nsmMultiple).Query(Baseline)
    .Paged(NyxCollectionPaging.PageSize(2));

  if ASecond then
  begin
    Result := Result.ColumnQuery(NyxColumnQuery.Filtering(False))
      .TableQuery(NyxTableQuery.SortMode(nqsmSingle));
  end;
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LTable: INyxTable;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Two independent query workbenches';
    Result.Collections.Define(People,
      NyxCollectionSchema.Text(PersonName, '').Integer(Score, 0).Boolean(Active, False), [
      NyxCollectionItem(Person('a')).WithValue(PersonName, 'Ada').WithValue(Score, 4).WithValue(Active, True),
      NyxCollectionItem(Person('b')).WithValue(PersonName, 'Bea').WithValue(Score, 1).WithValue(Active, False),
      NyxCollectionItem(Person('c')).WithValue(PersonName, 'Cy').WithValue(Score, 3).WithValue(Active, True),
      NyxCollectionItem(Person('d')).WithValue(PersonName, 'Dee').WithValue(Score, 2).WithValue(Active, True),
      NyxCollectionItem(Person('e')).WithValue(PersonName, 'Eli').WithValue(Score, 5).WithValue(Active, False),
      NyxCollectionItem(Person('f')).WithValue(PersonName, 'Fox').WithValue(Score, 6).WithValue(Active, True)]);
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(12).Gap(12).Done;
    LTable := NewNyxTable(CFirst);
    LTable.Configure.Height(180).ReadOnly(True).Done;
    LTable.Binds.Collection(Spec(False));
    LPage.Add(LTable);
    LTable := NewNyxTable(CSecond);
    LTable.Configure.Height(180).Done;
    LTable.Binds.Collection(Spec(True));
    LPage.Add(LTable);
    LTable := NewNyxTable(CLiteral);
    LTable.Configure.Height(72).Items('One literal row').Done;
    LPage.Add(LTable);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure Pump;
begin
  {$ifndef PAS2JS}Application.ProcessMessages; CheckSynchronize(1);{$endif}
end;

{$ifdef PAS2JS}
function UITurn: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(JSValue(True));
        end, 0);
    end);
end;
{$else}
procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Failure := TNyxText(AException.Message);
end;
{$endif}

procedure TQueueBarrier.Execute(const AExecution: INyxExecution);
begin
  Done := not AExecution.Cancelled;
end;

procedure TQueryReceiver.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LChange: TNyxQueryWorkbenchChange;
begin
  LChange := NyxQueryWorkbenchChange(AEvent);
  Check(AEvent.OriginID = CFirst, 'query Details retain the exact runtime table origin');
  Check(AEvent.SourceID = CFirst, 'query Details retain the runtime event source');

  if AEvent.Name.Name = NyxQueryWorkbenchEvent(nqweBeforeChange).Name then
  begin
    Inc(BeforeCalls);
    LastBefore := LChange.Copy;
    Check(not LChange.HasAfter and (LChange.Edits.Count > 0),
      'before event contains requested edits and accepted baseline, no invented result');

    if AdvanceClear then
    begin
      AdvanceClear := False;
      Workbench.ClearColumn(1);
    end;

    if Fail then
    begin
      raise Exception.Create('Deliberate query proposal receiver refusal');
    end;

    if Veto then
    begin
      AExecution.Cancel;
    end;
  end
  else
  begin
    Inc(ChangedCalls);
    LastChanged := LChange.Copy;
    Check(LChange.HasAfter, 'changed event contains an accepted copied observation');
  end;
end;

function TableID(AIndex: Integer): TNyxText;
begin

  if AIndex = 0 then
  begin
    Result := CFirst;
  end
  else
  begin
    Result := CSecond;
  end;
end;

procedure CaptureOwners;
var
  LIndex: Integer;
  LMount: INyxCollectionMount;
  LQueries: INyxCollectionQueryMount;
  LMissing: Boolean;
begin
  for LIndex := 0 to 1 do
  begin
    GViews[LIndex] := GApp.View.CollectionView(TableID(LIndex));
    LMount := GApp.View.CollectionMount(TableID(LIndex));
    LQueries := nil;
    Check((GViews[LIndex] <> nil) and Supports(LMount, INyxCollectionQueryMount, LQueries),
      'ordinary application renderer exposes the optional bound table query mount');
    GWorkbenches[LIndex] := LQueries.QueryWorkbench;
    Check((GWorkbenches[LIndex] <> nil) and GWorkbenches[LIndex].Connected,
      'accepted renderer adoption keeps the workbench and real event router live');
  end;
  Check(GViews[0] <> GViews[1], 'the two tables have independent view/session owners');
  Check(GViews[0].Store = GViews[1].Store, 'application-scoped tables share only their source store');
  LMissing := False;
  try
    GApp.View.CollectionView(CLiteral);
  except
    on ENyxCollection do LMissing := True;
  end;
  Check(LMissing, 'literal Items never manufactures a bound query view');
  LMissing := False;
  try
    GApp.View.CollectionMount(CLiteral);
  except
    on ENyxModel do LMissing := True;
  end;
  Check(LMissing, 'literal Items never acquires query controls');
end;

function FormRenderer(AIndex: Integer): TReviewRenderer;
var
  LPresenter: INyxQueryFormPresenter;
  {$ifdef PAS2JS}LTarget: INyxBrowserQueryFormPresenter;
  {$else}LTarget: INyxLCLQueryFormPresenter;{$endif}
begin
  LPresenter := GWorkbenches[AIndex].Presenter;
  Check((LPresenter <> nil) and LPresenter.Connected,
    'query form is an actual live managed presentation');
  {$ifdef PAS2JS}
  Check(Supports(LPresenter, INyxBrowserQueryFormPresenter, LTarget),
    'browser form exposes its real public popover');
  {$else}
  Check(Supports(LPresenter, INyxLCLQueryFormPresenter, LTarget),
    'native form exposes its real public popover');
  {$endif}
  Result := LTarget.Presentation.Renderer;
  Check(Result.Root <> nil, 'form owns a realized root for live value capture');
end;

function FormText(AIndex: Integer; const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := FormRenderer(AIndex).ElementFor(AID).textContent;
  {$else}
  Result := TLabel(FormRenderer(AIndex).ControlFor(AID)).Caption;
  {$endif}
end;

function DraftValue(AIndex: Integer; const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(FormRenderer(AIndex).InputFor(AID)).value;
  {$else}
  Result := TCustomEdit(FormRenderer(AIndex).InputFor(AID)).Text;
  {$endif}
end;

procedure TextInput(AIndex: Integer; const AID, AText: TNyxText);
var
  LRenderer: TReviewRenderer;
  {$ifdef PAS2JS}LInput: TJSHTMLInputElement;
  {$else}LInput: TEdit;{$endif}
begin
  LRenderer := FormRenderer(AIndex);
  {$ifdef PAS2JS}
  LInput := TJSHTMLInputElement(LRenderer.InputFor(AID));
  LInput.value := AText;
  LInput.dispatchEvent(TJSEvent.new('input'));
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LInput := TEdit(LRenderer.InputFor(AID));
  LInput.Text := AText;
  Check(Assigned(LInput.OnChange), 'live text binding owns the actual editor change handler');
  LInput.OnChange(LInput);
  {$endif}
  Pump;
  Check(LRenderer.Root.Find(AID).Prop(NyxAttributeName(atValue)) = AText,
    'physical input reaches mounted live Root, not authored descriptor setters');
end;

procedure Choose(AIndex: Integer; const AID, ACaption: TNyxText);
var
  LRenderer: TReviewRenderer;
  LIndex: Integer;
  {$ifdef PAS2JS}LInput: TJSHTMLSelectElement;
  {$else}LInput: TComboBox;{$endif}
begin
  LRenderer := FormRenderer(AIndex);
  {$ifdef PAS2JS}
  LInput := TJSHTMLSelectElement(LRenderer.InputFor(AID));
  LIndex := 0;
  while (LIndex < LInput.options.length) and
    (TJSHTMLOptionElement(LInput.options[LIndex]).text <> ACaption) do
  begin
    Inc(LIndex);
  end;
  Check(LIndex < LInput.options.length, 'typed query choice is physically exposed');
  LInput.selectedIndex := LIndex;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LInput := TComboBox(LRenderer.InputFor(AID));
  LIndex := LInput.Items.IndexOf(ACaption);
  Check(LIndex >= 0, 'typed query choice is physically exposed');
  LInput.ItemIndex := LIndex;
  Check(Assigned(LInput.OnChange), 'live Select binding owns actual change input');
  LInput.OnChange(LInput);
  {$endif}
  Pump;
end;

procedure FormClick(AIndex: Integer; const AID: TNyxText; APump: Boolean = True);
var
  LRenderer: TReviewRenderer;
begin
  LRenderer := FormRenderer(AIndex);
  LRenderer.Reveal(AID, niRuntime);
  {$ifdef PAS2JS}
  TJSHTMLElement(LRenderer.ElementFor(AID)).click;
  {$else}
  Check(LRenderer.ControlFor(AID).Enabled, 'actual query action is enabled');
  TButton(LRenderer.ControlFor(AID)).Click;
  {$endif}

  if APump then
  begin
    Pump;
  end;
end;

procedure Header(AIndex, AColumn: Integer; AFilter: Boolean; AAppend: Boolean = False);
{$ifdef PAS2JS}
var
  LButton: TJSHTMLElement;
  LOptions: TJSObject;
  LAttribute: TNyxText;
{$else}
var
  LGrid: TStringGrid;
  LBounds: TRect;
  LX: Integer;
  LShift: TShiftState;
{$endif}
begin
  GApp.View.Reveal(TableID(AIndex), niRuntime);
  {$ifdef PAS2JS}
  LAttribute := 'data-nyx-query-sort';

  if AFilter then
  begin
    LAttribute := 'data-nyx-query-column';
  end;
  LButton := TJSHTMLElement(GApp.View.ElementFor(TableID(AIndex)).querySelector(
    '[' + LAttribute + '="' + IntToStr(AColumn) + '"]'));
  Check(LButton <> nil, 'the actual table header exposes the requested query action');
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LOptions['shiftKey'] := AAppend;
  LButton.dispatchEvent(TReviewMouse.new('click', LOptions));
  {$else}
  LGrid := TStringGrid(GApp.View.ControlFor(TableID(AIndex)));
  LBounds := LGrid.CellRect(AColumn, 0);
  LX := LBounds.Left + 12;

  if AFilter then
  begin
    LX := LBounds.Right - 14;
  end;
  LShift := [];

  if AAppend then
  begin
    Include(LShift, ssShift);
  end;
  Check(Assigned(LGrid.OnMouseDown), 'native row-zero header owns its ordinary mouse handler');
  Check(Assigned(LGrid.OnHeaderClick), 'native query action runs only after accepted header release');
  TGridAccess(LGrid).MouseMove(LShift, LX, LBounds.Top + 8);
  TGridAccess(LGrid).MouseDown(mbLeft, LShift, LX, LBounds.Top + 8);
  TGridAccess(LGrid).MouseUp(mbLeft, LShift, LX, LBounds.Top + 8);
  {$endif}
  Pump;
end;

procedure KeyboardOpen(AIndex, AColumn: Integer);
{$ifdef PAS2JS}
var
  LOptions: TJSObject;
  LButton: TJSHTMLElement;
{$else}
var
  LGrid: TStringGrid;
  LKey: Word;
{$endif}
begin
  GApp.View.Reveal(TableID(AIndex), niRuntime);
  {$ifdef PAS2JS}
  LButton := TJSHTMLElement(GApp.View.ElementFor(TableID(AIndex)).querySelector(
    '[data-nyx-query-column="' + IntToStr(AColumn) + '"]'));
  Check(LButton <> nil, 'keyboard query target is a real focusable header action');
  LButton.focus;
  LOptions := TJSObject.new;
  LOptions['key'] := 'ArrowDown';
  LOptions['altKey'] := True;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LButton.dispatchEvent(TReviewKey.new('keydown', LOptions));
  {$else}
  LGrid := TStringGrid(GApp.View.ControlFor(TableID(AIndex)));
  LGrid.Col := AColumn;
  LGrid.SetFocus;
  LKey := VK_DOWN;
  Check(Assigned(LGrid.OnKeyDown), 'native table owns its actual key handler');
  LGrid.OnKeyDown(LGrid, LKey, [ssAlt]);
  Check(LKey = 0, 'Alt+Down consumes the native default only after opening its form');
  {$endif}
  Pump;
  Check(GWorkbenches[AIndex].Presenter <> nil, 'ordinary keyboard input opens the managed query form');
end;

procedure Visible(AIndex: Integer; const AIDs: array of TNyxText);
var
  LIndex: Integer;
  {$ifdef PAS2JS}LRows: TJSNodeList;
  {$else}LGrid: TStringGrid;{$endif}
begin
  Check(GViews[AIndex].Snapshot.Count = Length(AIDs), 'query has expected page membership');
  {$ifdef PAS2JS}
  LRows := GApp.View.ElementFor(TableID(AIndex)).querySelectorAll('[data-nyx-item]');
  Check(LRows.length = Length(AIDs), 'actual browser rows follow accepted page membership');
  {$else}
  LGrid := TStringGrid(GApp.View.ControlFor(TableID(AIndex)));

  if Length(AIDs) = 0 then
  begin
    Check((LGrid.RowCount = 2) and (LGrid.Cells[0, 1] = ''),
      'empty native table retains the required blank widget row, with no data identity');
  end
  else
  begin
    Check(LGrid.RowCount = Length(AIDs) + 1, 'native row-zero ABI retains exact data row count');
  end;
  {$endif}
  for LIndex := 0 to High(AIDs) do
  begin
    Check(GViews[AIndex].Snapshot.ItemAt(LIndex).Ref.ID = AIDs[LIndex], 'stable typed row order');
    {$ifdef PAS2JS}
    Check(TJSHTMLElement(LRows[LIndex]).getAttribute('data-nyx-item') = AIDs[LIndex],
      'actual browser row identity matches admitted query');
    {$else}
    Check(LGrid.Cells[0, LIndex + 1] = GViews[AIndex].CellText(Person(AIDs[LIndex]), 0),
      'physical native cell follows accepted typed ordering');
    {$endif}
  end;
end;

function HeaderText(AIndex, AColumn: Integer): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(GApp.View.ElementFor(TableID(AIndex)).querySelector(
    '[data-nyx-query-sort="' + IntToStr(AColumn) + '"]')).textContent;
  {$else}
  Result := TStringGrid(GApp.View.ControlFor(TableID(AIndex))).Cells[AColumn, 0];
  {$endif}
end;

procedure SourceUnchanged;
begin
  Check(GViews[0].Store.Snapshot = GSource, 'query commands never replace source rows');
  Check(GViews[1].Store.Snapshot = GSource, 'sibling view still shares exact source defaults');
  Check(TNyxCodec.Encode(GDocument) = GAuthored, 'runtime query/draft never edits authored document');
end;

{$ifndef PAS2JS}
{ This is the consumer's own authored/default document and typed source, not a
  Studio Save/Open packet or proof that the emitted companion was compiled. }
procedure WriteEvidence(const AName: String; const AText: TNyxText);
var
  LPath: String;
  LStream: TFileStream;
begin

  if GEvidence = '' then
  begin
    Exit;
  end;
  LPath := IncludeTrailingPathDelimiter(GEvidence) + AName;
  Check(not FileExists(LPath), 'own evidence output must be new: ' + TNyxText(AName));
  ForceDirectories(GEvidence);
  LStream := TFileStream.Create(LPath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;
{$endif}

procedure PolicyRoundTrip;
var
  LSource: TNyxText;
  LWorkspace: TNyxSourceWorkspace;
  LRead, LDecoded: TNyxDocument;
  LCopy: TNyxCollectionViewSpec;
begin
  LWorkspace := nil;
  LRead := nil;
  LDecoded := nil;
  try
    ValidateNyxDocumentProperties(GDocument);
    LCopy := TNyxCollectionViewSpec.FromData(Spec(False).ToData);
    Check(LCopy.ToData.ToJSON = Spec(False).ToData.ToJSON, 'v7 query policy descriptor round trip');
    Check(LCopy.ColumnAt(0).QueryPolicy.TextMode = nqtAsciiInsensitive,
      'saved column text comparison is typed and exact');
    Check(not Spec(True).ColumnAt(2).QueryPolicy.FilterEnabled,
      'second table explicitly opts out one column filter');
    Check(Spec(True).TableQueryPolicy.Mode = nqsmSingle, 'saved table single-sort policy');
    LSource := TNyxCodegen.Generate(GDocument, 'nyx.fixture.table.query');
    Check((Pos('NyxColumnQuery', LSource) > 0) and (Pos('NyxTableQuery', LSource) > 0),
      'generated source exposes typed fluent query policies');
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = GAuthored, 'strict source reader reconstructs both exact policy-bearing tables');
    LDecoded := TNyxCodec.Decode(GAuthored);
    Check(TNyxCodec.Encode(LDecoded) = GAuthored, 'saved codec retains policy absence and explicit choices');
    Check(LWorkspace.Render(LRead) = LSource, 'unchanged source reconciliation retains authored unit');
    {$ifndef PAS2JS}
    WriteEvidence('query-workbench-defaults.nyx', GAuthored);
    WriteEvidence('nyx.generated.table.query.pas', LSource);
    {$endif}
  finally
    LDecoded.Free;
    LRead.Free;
    LWorkspace.Free;
  end;
end;

procedure OpenFilter(AIndex, AColumn: Integer);
begin
  Header(AIndex, AColumn, True);
  Check(GWorkbenches[AIndex].Presenter <> nil, 'header filter opens an actual managed form');
  {$ifdef PAS2JS}
  Check(Web.document.activeElement = FormRenderer(AIndex).FocusFor(
    NyxQueryEditorFieldID(NyxQueryWorkbenchEditorID, nqfField)),
    'actual header release leaves focus inside the query form');
  {$else}
  Check(Screen.ActiveControl = FormRenderer(AIndex).FocusFor(
    NyxQueryEditorFieldID(NyxQueryWorkbenchEditorID, nqfField)),
    'native grid default does not steal popup focus after header release');
  {$endif}
end;

procedure AddCondition(AIndex: Integer);
begin
  FormClick(AIndex, NyxQueryEditorActionID(
    NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqeAnd));
  Check(FormRenderer(AIndex).Root.Find(NyxQueryEditorFieldID(
    NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqfValue)) <> nil,
    'structural action rebuilds a real typed predicate form');
  {$ifdef PAS2JS}
  Check(Web.document.activeElement = FormRenderer(AIndex).FocusFor(
    NyxQueryEditorFieldID(NyxQueryWorkbenchEditorID, nqfField)),
    'structural form replacement retains exact field focus');
  {$else}
  Check(Screen.ActiveControl = FormRenderer(AIndex).FocusFor(
    NyxQueryEditorFieldID(NyxQueryWorkbenchEditorID, nqfField)),
    'structural form replacement retains exact field focus');
  {$endif}
end;

procedure Apply(AIndex: Integer);
begin
  FormClick(AIndex, NyxQueryEditorActionID(NyxQueryWorkbenchEditorID, nqeSave));
end;

procedure InitialAndSortCases;

  { Diagnose the unchanged first-click oracle through public observations. No
    handler is replaced and the actual gesture/query admission remains intact. }
  function HeaderDetails: TNyxText;
  var
    LMount: INyxCollectionMount;
    {$ifndef PAS2JS}
    LGrid: TStringGrid;
    LBounds: TRect;
    LColumn, LRow: Integer;
    {$endif}
  begin
    Result := '';
    try
      LMount := GApp.View.CollectionMount(CFirst);
      Result := '; mountError="' + LMount.ErrorText + '"; failure=' +
        TNyxText(IntToStr(Ord(LMount.Failure))) + '; before=' +
        TNyxText(IntToStr(GReceiver.BeforeCalls)) + '; changed=' +
        TNyxText(IntToStr(GReceiver.ChangedCalls)) + '; presenter=' +
        TNyxText(BoolToStr(GWorkbenches[0].Presenter <> nil, True));
      {$ifndef PAS2JS}
      LGrid := TStringGrid(GApp.View.ControlFor(CFirst));
      LBounds := LGrid.CellRect(0, 0);
      LGrid.MouseToCell(LBounds.Left + 12, LBounds.Top + 8, LColumn, LRow);
      Result := Result + '; canFocus=' + TNyxText(BoolToStr(LGrid.CanFocus, True)) +
        '; focused=' + TNyxText(BoolToStr(LGrid.Focused, True)) +
        '; editorMode=' + TNyxText(BoolToStr(LGrid.EditorMode, True)) +
        '; clientWidth=' + TNyxText(IntToStr(LGrid.ClientWidth)) +
        '; appClientWidth=' + TNyxText(IntToStr(GApp.Window.ClientWidth)) +
        '; fixed=' + TNyxText(IntToStr(LGrid.FixedCols)) + ',' + TNyxText(IntToStr(LGrid.FixedRows)) +
        '; cellRect=' + TNyxText(IntToStr(LBounds.Left)) + ',' + TNyxText(IntToStr(LBounds.Top)) +
        ',' + TNyxText(IntToStr(LBounds.Right)) + ',' + TNyxText(IntToStr(LBounds.Bottom)) +
        '; mouseCell=' + TNyxText(IntToStr(LColumn)) + ',' + TNyxText(IntToStr(LRow)) +
        '; windowActive=' + TNyxText(BoolToStr(GApp.Window.Active, True));
      {$endif}
    except
      on E: Exception do Result := Result + '; diagnostic="' + TNyxText(E.Message) + '"';
    end;
  end;

begin
  Visible(0, ['f', 'd']);
  Visible(1, ['f', 'd']);
  Check(GWorkbenches[0].Observation.MatchedRows = 4, 'counts are query-before-page');
  Check(GWorkbenches[0].Observation.SourceRows = 6, 'source count includes baseline-excluded rows');
  Check(GWorkbenches[0].Observation.PageRows = 2, 'page count is distinct from matching count');
  Check(Pos('4 / 6', HeaderText(0, 0)) > 0, 'persistent first header exposes matching/source count');
  Check(Pos('[filtered]', HeaderText(0, 2)) > 0, 'authored baseline field is honestly marked filtered');
  Header(0, 0, False);
  Check(GWorkbenches[0].Observation.HasUserSort,
    'first header gesture creates a user override' + HeaderDetails);
  Check(GWorkbenches[0].Observation.Query.SortAt(0).Direction = nsdAscending,
    'authored descending baseline does not skip first ascending user step');
  Visible(0, ['a', 'c']);
  Header(0, 0, False);
  Check(GWorkbenches[0].Observation.Query.SortAt(0).Direction = nsdDescending,
    'second ordinary header gesture selects descending');
  Header(0, 0, False);
  Check(not GWorkbenches[0].Observation.HasUserSort, 'third header gesture restores authored sort ownership');
  Visible(0, ['f', 'd']);
  Header(0, 1, False, True);
  Check(GWorkbenches[0].Observation.Query.SortCount = 1,
    'first Shift sort starts a user vector without absorbing authored keys');
  Header(0, 0, False, True);
  Check(GWorkbenches[0].Observation.Query.SortCount = 2, 'explicit multikey gesture appends a distinct key');
  GWorkbenches[0].ResetSort;
  KeyboardOpen(0, 1);
  Check(Pos('4 matching source rows / 6 source rows', FormText(0, CCounts)) > 0,
    'form counts do not claim baseline eligibility or page size');
  Check(Pos('authored and other filters', FormText(0, NyxQueryWorkbenchEditorID + '-no-filter')) > 0,
    'empty own filter help preserves baseline/peer meaning');
  GWorkbenches[0].Presenter.Close;
  SourceUnchanged;
end;

procedure DraftAndNegotiationCases;
var
  LPresenter: INyxQueryFormPresenter;
  LBefore, LChanged: Integer;
  LValueID, LComparisonID: TNyxText;
begin
  LValueID := NyxQueryEditorFieldID(NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqfValue);
  LComparisonID := NyxQueryEditorFieldID(NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqfComparison);
  OpenFilter(0, 1);
  AddCondition(0);
  Choose(0, LComparisonID, 'At least');
  TextInput(0, LValueID, '-');
  LPresenter := GWorkbenches[0].Presenter;
  Apply(0);
  Check(GWorkbenches[0].Presenter = LPresenter, 'invalid numeric Apply preserves exact presenter');
  Check(DraftValue(0, LValueID) = '-', 'invalid numeric text remains correctable');
  Check(FormText(0, CStatus) <> '', 'invalid draft gets visible refusal status');
  Visible(0, ['f', 'd']);
  TextInput(0, LValueID, '3');
  GReceiver.Veto := True;
  LBefore := GReceiver.BeforeCalls;
  LChanged := GReceiver.ChangedCalls;
  Apply(0);
  Check(GReceiver.BeforeCalls = LBefore + 1, 'named before receiver sees a real user proposal');
  Check(GReceiver.ChangedCalls = LChanged, 'veto produces no committed Changed');
  Check(GWorkbenches[0].Presenter = LPresenter, 'veto keeps same active draft');
  Check(DraftValue(0, LValueID) = '3', 'veto does not roll back physical draft');
  GReceiver.Veto := False;
  GDesignReceiver := TQueryReceiver.Create;
  GDesignLease := GDesignReceiver;
  GDesignReceiver.Veto := True;
  GDesignToken := GApp.View.Events.OnNamed(NyxControlEvents(CFirst, niDesign),
    NyxQueryWorkbenchEvent(nqweBeforeChange)).Subscribe(GDesignLease);
  Apply(0);
  Check(GDesignReceiver.BeforeCalls = 1, 'real copied design identity participates in before negotiation');
  Check(GWorkbenches[0].Presenter = LPresenter, 'design-target veto preserves exact form and accepted query');
  Check(GReceiver.ChangedCalls = LChanged, 'design-target veto prevents runtime committed event');
  GDesignToken.Cancel;
  GDesignToken := nil;
  GDesignReceiver := nil;
  GDesignLease := nil;
  GReceiver.Fail := True;
  Apply(0);
  Check(GWorkbenches[0].Presenter = LPresenter, 'failed before receiver preserves draft');
  Check(GReceiver.ChangedCalls = LChanged, 'failed callback cannot publish a query');
  GReceiver.Fail := False;
  Apply(0);
  Check(GWorkbenches[0].Presenter = nil, 'successful Apply retires exact form capture');
  Check(not LPresenter.Connected, 'retained former presenter is disconnected');
  Check(GReceiver.ChangedCalls = LChanged + 1, 'accepted query emits exactly one committed event');
  Check(GReceiver.LastChanged.After.MatchedRows = 3, 'changed copied observation is query-before-page');
  Visible(0, ['f', 'c']);
  Visible(1, ['f', 'd']);
  Check(GWorkbenches[0].Observation.Count = 1, 'column filter owns one contribution');
  Check(Pos('[filtered]', HeaderText(0, 1)) > 0, 'actual filtered column header updates');
  OpenFilter(0, 1);
  Check(Pos('Score is at least 3', FormText(0, CChip)) > 0, 'chip explains a readable typed predicate');
  LChanged := GReceiver.ChangedCalls;
  Apply(0);
  Check(GReceiver.ChangedCalls = LChanged, 'same-value accepted Apply suppresses Changed');
  Check(GWorkbenches[0].Presenter = nil, 'same-value accepted Apply may close while serial-fencing');
  GWorkbenches[0].ClearColumn(1);
  Visible(0, ['f', 'd']);
  SourceUnchanged;
end;

procedure ScalarAndSortFormCases;
var
  LValueID: TNyxText;
  {$ifdef PAS2JS}LCheck: TJSHTMLInputElement;
  {$else}LCheck: TCheckBox;{$endif}
begin
  LValueID := NyxQueryEditorFieldID(NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqfValue);
  OpenFilter(0, 0);
  AddCondition(0);
  Choose(0, NyxQueryEditorFieldID(NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqfComparison),
    'Contains');
  TextInput(0, LValueID, 'f');
  Apply(0);
  Visible(0, ['f']);
  Check(GWorkbenches[0].Observation.Query.Filter <> nil, 'typed text filter composes with baseline');
  GWorkbenches[0].ClearColumn(0);
  OpenFilter(0, 2);
  AddCondition(0);
  {$ifdef PAS2JS}
  LCheck := TJSHTMLInputElement(FormRenderer(0).InputFor(LValueID));
  LCheck.checked := True;
  LCheck.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LCheck := TCheckBox(FormRenderer(0).InputFor(LValueID));
  LCheck.Checked := True;
  Check(Assigned(LCheck.OnChange), 'Boolean query value owns actual checkbox handler');
  LCheck.OnChange(LCheck);
  {$endif}
  Apply(0);
  Check(GWorkbenches[0].Observation.Count = 1, 'live Boolean input publishes a typed predicate');
  Visible(0, ['f', 'd']);
  GWorkbenches[0].ClearColumn(2);
  GWorkbenches[0].OpenSort;
  Choose(0, NyxQueryEditorFieldID(NyxQueryWorkbenchEditorID, nqfField),
    NyxMenuEditorChoiceCaption(1, 'score'));
  FormClick(0, NyxQueryEditorActionID(NyxQueryWorkbenchEditorID, nqeAddSort));
  Choose(0, NyxQueryEditorFieldID(NyxQueryEditorSortID(NyxQueryWorkbenchEditorID, 0), nqfDirection),
    'Descending');
  Apply(0);
  Check(GWorkbenches[0].Observation.HasUserSort, 'actual sort form owns the shared user vector');
  Check(GWorkbenches[0].Observation.Query.SortAt(0).FieldName = 'score',
    'mounted Select chooses exact scalar field identity');
  Visible(0, ['f', 'a']);
  GWorkbenches[0].ResetSort;
  GWorkbenches[0].OpenSort;
  Apply(0);
  Check(GWorkbenches[0].Observation.HasUserSort and
    (GWorkbenches[0].Observation.Query.SortCount = 0),
    'empty sort Apply explicitly unsorts instead of importing baseline keys');
  Visible(0, ['a', 'c']);
  GWorkbenches[0].ResetSort;
  Visible(0, ['f', 'd']);
  SourceUnchanged;
end;

procedure PeerAndRecoveryCases;
var
  LSession: INyxCollectionQuerySession;
  LPeer: INyxCollectionQueryOwner;
  LStage: INyxPreparedPublication;
  LOldPresenter: INyxQueryFormPresenter;
  LIndex: Integer;
  LValueID, LLong, LCaption: TNyxText;
begin
  LValueID := NyxQueryEditorFieldID(NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqfValue);
  GViews[0].Select(Person('f'));
  LSession := NyxCollectionQuerySession(GViews[0]);
  LPeer := LSession.AcquireOwner(NyxQueryOrigin('global-search'), 'Global search');
  LStage := LSession.PrepareSession(LSession.Capture, LPeer,
    NyxQuerySessionEdits.Filter(NyxQueryChannel('search'), NyxWhere(PersonName).Contains('a', nqtAsciiInsensitive)));
  PublishNyxGroup([LStage]);
  Check(Pos('[filtered]', HeaderText(0, 0)) > 0,
    'peer arbitrary channel marks typed predicate field instead of guessing channel identity');
  Visible(0, ['a']);
  Check(GViews[0].Selection.Contains(Person('f')), 'query preserves hidden selected item identity');
  Visible(1, ['f', 'd']);
  OpenFilter(0, 1);
  AddCondition(0);
  TextInput(0, LValueID, '99');
  Apply(0);
  Visible(0, []);
  Check(GWorkbenches[0].Observation.MatchedRows = 0, 'empty result count is honest');
  KeyboardOpen(0, 1);
  Check(FormRenderer(0).Root.Find(CCounts + '-empty') <> nil,
    'empty native/browser table still exposes keyboard recovery help');
  Check(GWorkbenches[0].Observation.Count = 2, 'peer and column contributions coexist');
  LIndex := 0;
  while (LIndex < GWorkbenches[0].Observation.Count) and
    (GWorkbenches[0].Observation.ContributionAt(LIndex).Owner.ID.Value <> LPeer.ID.Value) do
  begin
    Inc(LIndex);
  end;
  Check(LIndex < GWorkbenches[0].Observation.Count, 'peer owner is explicitly discoverable in chips');
  FormClick(0, 'nyx-query-workbench-chip-' + TNyxText(IntToStr(LIndex)));
  Check(GWorkbenches[0].Observation.Count = 1, 'physical chip removes only selected peer predicate');
  GWorkbenches[0].ClearOwner;
  Check(GWorkbenches[0].Observation.Count = 0, 'surface Clear removes its remaining filter');
  Check(GViews[0].Selection.Contains(Person('f')), 'Clear restores hidden membership without rewriting it');
  Visible(0, ['f', 'd']);
  OpenFilter(0, 1);
  AddCondition(0);
  TextInput(0, LValueID, '4');
  LOldPresenter := GWorkbenches[0].Presenter;
  LStage := LSession.PrepareSession(LSession.Capture, LPeer,
    NyxQuerySessionEdits.Filter(NyxQueryChannel('other'), NyxWhere(PersonName).StartsWith('F')));
  PublishNyxGroup([LStage]);
  Apply(0);
  Check(GWorkbenches[0].Presenter = LOldPresenter, 'peer edit refuses stale form without replacing draft');
  Check(DraftValue(0, LValueID) = '4', 'stale form retains original live text');
  Check(GWorkbenches[0].Observation.Count = 1, 'stale Apply cannot overwrite peer query');
  FormClick(0, 'nyx-query-workbench-reload');
  Check((GWorkbenches[0].Presenter <> LOldPresenter) and not LOldPresenter.Connected,
    'explicit reload adopts a fresh form and retires only the discarded draft');
  AddCondition(0);
  TextInput(0, LValueID, '6');
  Apply(0);
  Check(GWorkbenches[0].Observation.Count = 2,
    'after explicit reload a valid Apply composes with current peer rules');
  Visible(0, ['f']);
  GWorkbenches[0].ClearAllFilters;
  Check(GWorkbenches[0].Observation.Count = 0, 'explicit global Clear withdraws all user filters');
  Check(GWorkbenches[0].Observation.Query.Filter <> nil, 'global Clear preserves authored baseline filter');
  LLong := '';
  for LIndex := 1 to 100 do
  begin
    LLong := LLong + NyxScalarText($754C) + NyxScalarText($1F642);
  end;
  LStage := LSession.PrepareSession(LSession.Capture, LPeer,
    NyxQuerySessionEdits.Filter(NyxQueryChannel('unicode-search'),
      NyxWhere(PersonName).Contains(LLong, nqtAsciiInsensitive)));
  PublishNyxGroup([LStage]);
  OpenFilter(0, 1);
  LCaption := FormText(0, CChip);
  Check(Pos('…', LCaption) > 0, 'long Unicode predicate gets a bounded readable chip');
  Check(NyxData(LCaption).AsText = LCaption,
    'chip clipping preserves complete UTF-8/native and UTF-16/browser scalars');
  GWorkbenches[0].ClearAllFilters;
  LPeer := nil;
  SourceUnchanged;
end;

procedure PolicyAndTakeoverCases;
var
  LRefused: Boolean;
  LBefore: TNyxText;
  LActor: INyxCollectionQueryOwner;
begin
  LBefore := GViews[1].QueryPolicy.ToData.ToJSON;
  LRefused := False;
  try
    GWorkbenches[1].OpenColumn(2);
  except
    on ENyxCollection do LRefused := True;
  end;
  Check(LRefused, 'explicit column filtering opt-out rejects retained public command');
  Check(GViews[1].QueryPolicy.ToData.ToJSON = LBefore, 'policy refusal preserves accepted query');
  Header(1, 1, False);
  Header(1, 0, False, True);
  Check(GWorkbenches[1].Observation.Query.SortCount = 1, 'single-sort table never admits multiple user keys');
  GWorkbenches[1].ResetSort;
  GWorkbenches[0].SetEnabled(False);
  LRefused := False;
  try
    GWorkbenches[0].OpenColumn(1);
  except
    on ENyxCollection do LRefused := True;
  end;
  Check(LRefused, 'disabled table cannot be bypassed by retained workbench intent');
  GWorkbenches[0].SetEnabled(True);
  LActor := NyxCollectionQuerySession(GViews[0]).AcquireOwner(NyxQueryOrigin('takeover-proof'), 'Former owner');
  GViews[0].ConfigureQuery(NyxCollectionQuery.OrderBy(Score));
  Check(not LActor.Connected, 'actual raw takeover revokes old session authority');
  Check(GWorkbenches[0].Connected, 'raw takeover does not destroy mounted workbench port');
  OpenFilter(0, 1);
  Check(GWorkbenches[0].Presenter <> nil, 'fresh physical Open deliberately joins new generation');
  GWorkbenches[0].ClearOwner;
  Check(GViews[0].QueryPolicy.ToData.ToJSON = NyxCollectionQuery.OrderBy(Score).ToData.ToJSON,
    'fresh same-value Clear preserves raw takeover baseline');
  GViews[0].ConfigureQuery(Baseline);
  SourceUnchanged;
end;

{ This case keeps popup/app/command owners alive across async completion. }
procedure QueuedAndFinalFenceCases; {$ifdef PAS2JS}async;{$endif}
var
  LValueID: TNyxText;
  LPresenter: INyxQueryFormPresenter;
  LBarrier: TQueueBarrier;
  LBarrierLease: INyxWork;
  LExecution: INyxExecution;
  LBefore: TNyxText;
  {$ifdef PAS2JS}LTarget: INyxBrowserQueryFormPresenter;
  {$else}LTarget: INyxLCLQueryFormPresenter;{$endif}
  {$ifdef PAS2JS}LStarted: Double;{$else}LStarted: QWord;{$endif}
begin
  LValueID := NyxQueryEditorFieldID(NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqfValue);
  OpenFilter(0, 1);
  AddCondition(0);
  TextInput(0, LValueID, '6');
  LPresenter := GWorkbenches[0].Presenter;
  LPresenter.Events.On(NyxControlEvents(NyxQueryEditorActionID(NyxQueryWorkbenchEditorID, nqeSave),
    niRuntime), ntClick).Policy(neUIQueue);
  LBefore := GViews[0].QueryPolicy.ToData.ToJSON;
  FormClick(0, NyxQueryEditorActionID(NyxQueryWorkbenchEditorID, nqeSave), False);
  GWorkbenches[0].ClearColumn(1);
  LBarrier := TQueueBarrier.Create;
  LBarrierLease := LBarrier;
  LExecution := GApp.View.Events.Scheduler.Submit(LBarrierLease, neUIQueue);
  {$ifdef PAS2JS}LStarted := TJSDate.now;{$else}LStarted := GetTickCount64;{$endif}
  while not LBarrier.Done do
  begin
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 10000 then
    {$else}

    if GetTickCount64 - LStarted > 10000 then
    {$endif}
    begin
      raise Exception.Create('Actual query UI queue barrier did not complete');
    end;
    {$ifdef PAS2JS}await(UITurn);{$else}Pump;{$endif}
  end;
  Check(LExecution.Status = nesSucceeded, 'ordered actual UI queue barrier completed');
  Check(GViews[0].QueryPolicy.ToData.ToJSON = LBefore,
    'same-value Clear serial-fences captured older queued Apply');
  Check(GWorkbenches[0].Observation.Count = 0, 'late command cannot recreate withdrawn contribution');
  OpenFilter(0, 1);
  AddCondition(0);
  TextInput(0, LValueID, '4');
  LPresenter := GWorkbenches[0].Presenter;
  LPresenter.Events.On(NyxControlEvents(NyxQueryEditorActionID(NyxQueryWorkbenchEditorID, nqeSave),
    niRuntime), ntClick).Policy(neUIQueue);
  FormClick(0, NyxQueryEditorActionID(NyxQueryWorkbenchEditorID, nqeSave), False);
  {$ifdef PAS2JS}
  Check(Supports(LPresenter, INyxBrowserQueryFormPresenter, LTarget), 'actual browser dismissal owner');
  {$else}
  Check(Supports(LPresenter, INyxLCLQueryFormPresenter, LTarget), 'actual native dismissal owner');
  {$endif}
  LTarget.Presentation.Dismiss(nprEscape);
  Check(not LPresenter.IsOpen and LPresenter.Connected,
    'Escape dismissal closes but deliberately retains the mounted form');
  LBarrier := TQueueBarrier.Create;
  LBarrierLease := LBarrier;
  LExecution := GApp.View.Events.Scheduler.Submit(LBarrierLease, neUIQueue);
  {$ifdef PAS2JS}LStarted := TJSDate.now;{$else}LStarted := GetTickCount64;{$endif}
  while not LBarrier.Done do
  begin
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 10000 then
    {$else}

    if GetTickCount64 - LStarted > 10000 then
    {$endif}
    begin
      raise Exception.Create('Dismissed-query queue barrier did not complete');
    end;
    {$ifdef PAS2JS}await(UITurn);{$else}Pump;{$endif}
  end;
  Check(LExecution.Status = nesSucceeded, 'dismissal oracle waited for actual queued work');
  Check((GViews[0].QueryPolicy.ToData.ToJSON = LBefore) and
    (GWorkbenches[0].Observation.Count = 0),
    'queued Apply cannot publish from a dismissed retained form');
  OpenFilter(0, 1);
  Check((GWorkbenches[0].Presenter = LPresenter) and (DraftValue(0, LValueID) = '4'),
    'new physical Open can recover closed invalid/unsubmitted draft without running old intent');
  GWorkbenches[0].ClearColumn(1);
  OpenFilter(0, 1);
  AddCondition(0);
  TextInput(0, LValueID, '4');
  GReceiver.AdvanceClear := True;
  GReceiver.Workbench := GWorkbenches[0];
  Apply(0);
  GReceiver.Workbench := nil;
  Check(GWorkbenches[0].Observation.Count = 0,
    'before callback same-value Clear supersedes outer publication final fence');
  SourceUnchanged;
end;

procedure LifetimeCases;
var
  LOld: INyxCollectionQueryWorkbench;
  LPresenter: INyxQueryFormPresenter;
  LRefused: Boolean;
  LStore: INyxCollection;
begin
  OpenFilter(0, 1);
  LOld := GWorkbenches[0];
  LPresenter := LOld.Presenter;
  LStore := GViews[0].Store;
  GApp.ShowPage('home');
  Check(not LOld.Connected, 'same-ID replacement revokes former workbench');
  Check(not LPresenter.Connected, 'same-ID replacement disconnects former form');
  LRefused := False;
  try
    LOld.ClearOwner;
  except
    on ENyxCollection do LRefused := True;
  end;
  Check(LRefused, 'old retained command cannot touch replacement with same IDs');
  CaptureOwners;
  Check(GViews[0].Store = LStore, 'ordinary replacement keeps accepted application source store');
  KeyboardOpen(0, 0);
  Check(GWorkbenches[0].Presenter <> nil, 'replacement routes actual input through accepted parent events');
  LOld := GWorkbenches[0];
  LPresenter := LOld.Presenter;
  GApp.Free;
  GApp := nil;
  Check(not LOld.Connected, 'application disposal revokes retained workbench');
  Check(not LPresenter.Connected, 'application disposal revokes managed popup target');
end;

procedure Start;
begin
  SetLength(GViews, 2);
  SetLength(GWorkbenches, 2);
  GDocument := Fixture;
  GAuthored := TNyxCodec.Encode(GDocument);
  PolicyRoundTrip;
  GApp := TReviewApplication.Create;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(Web.document.createElement('main'));
  GHost.style.setProperty('width', IntToStr(GWidth) + 'px');
  Web.document.body.appendChild(GHost);
  GApp.Run(GDocument, GHost);
  {$else}
  GApp.Mount(GDocument);
  GApp.Window.ClientWidth := GWidth;
  GApp.Window.ClientHeight := 800;
  GApp.Window.Show;
  Pump;
  Check(GApp.Window.ClientWidth = GWidth, 'actual native client width equals requested allocation');
  WriteEvidence('query-workbench-client-width.txt',
    TNyxText(IntToStr(GApp.Window.ClientWidth)));
  {$endif}
  CaptureOwners;
  GSource := GViews[0].Store.Snapshot;
  GReceiver := TQueryReceiver.Create;
  GReceiverLease := GReceiver;
  GBeforeToken := GApp.View.Events.OnNamed(NyxControlEvents(CFirst, niRuntime),
    NyxQueryWorkbenchEvent(nqweBeforeChange)).Subscribe(GReceiverLease);
  GChangedToken := GApp.View.Events.OnNamed(NyxControlEvents(CFirst, niRuntime),
    NyxQueryWorkbenchEvent(nqweChanged)).Subscribe(GReceiverLease);
end;

procedure Cleanup;
begin

  if GBeforeToken <> nil then
  begin
    GBeforeToken.Cancel;
  end;

  if GChangedToken <> nil then
  begin
    GChangedToken.Cancel;
  end;

  if GDesignToken <> nil then
  begin
    GDesignToken.Cancel;
  end;
  GBeforeToken := nil;
  GChangedToken := nil;
  GDesignToken := nil;
  GDesignReceiver := nil;
  GDesignLease := nil;

  if GReceiver <> nil then
  begin
    GReceiver.Workbench := nil;
  end;
  GReceiver := nil;
  GReceiverLease := nil;
  GWorkbenches := nil;
  GViews := nil;
  GSource := nil;
  GApp.Free;
  GApp := nil;
  GDocument.Free;
  GDocument := nil;
  {$ifdef PAS2JS}

  if GHost <> nil then
  begin
    GHost.remove;
    GHost := nil;
  end;
  {$endif}
end;

{ BEGIN COLUMN ARRANGEMENT CASES }
const
  CArrangementFirst = 'arrangement-first/' + CFirst;
  CArrangementSecond = 'arrangement-second/' + CSecond;
  CArrangementClick = 'arrangement-columns-activated';

type
  { Callback behavior is selected only for the added activation journey. The
    original arrangement/query journeys keep their authored behavior unchanged. }
  TArrangementClickMode = (acmOrdinary, acmPreventClick, acmConsumeRelease,
    acmCloseDouble, acmUnmount);
  { Event callbacks borrow their review until its actual subscriptions detach. }
  TArrangementReview = class;
  { A real main-router receiver observes the existing authored Click and key
    phases; it never calls the manager default or a fabricated event engine. }
  TArrangementClickReceiver = class(TNyxEventCallback)
  public
    Owner: TArrangementReview;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  { Separate owned application over an independent copy of the existing seed.
    The original fixture/journey and its globals are untouched. Captures and
    prepared publications below use the public optional facet; this first
    portion proves runtime admission, not physical management-form input. }
  TArrangementReview = class
  private
    FDocument: TNyxDocument;
    FApp: TReviewApplication;
    { Two managed slots are allocated before mounting. Dynamic interface arrays
      have portable reference finalization; pas2js rejects static COM arrays. }
    FViews: array of INyxCollectionView;
    FColumns: array of INyxCollectionColumns;
    FSubscription: INyxCollectionViewSubscription;
    FAuthored: TNyxText;
    FNotifications: Integer;
    { These observers count accepted data only. Selection/editor presentation
      notices remain visible to Changed and are not scalar-commit evidence. }
    FCommitStoreNotifications: Integer;
    FCommitViewNotifications: Integer;
    FCommitStoreChanges: INyxCollectionChanges;
    FCommitViewChanges: INyxCollectionChanges;
    FReenter, FReentryRefused, FLastChangesNil: Boolean;
    FClickToken: INyxEventSubscription;
    FReleaseToken: INyxEventSubscription;
    FDoubleToken: INyxEventSubscription;
    FClickReceiver: INyxEventCallback;
    FClickMode: TArrangementClickMode;
    FSemanticClicks: Integer;
    FLegacyClicks: Integer;
    FReleaseCalls: Integer;
    FDoubleCalls: Integer;
    FProofManager: INyxColumnArrangementManager;
    {$ifdef PAS2JS}FCurrentClick: TJSMouseEvent;{$endif}
    {$ifdef PAS2JS}FHost: TJSHTMLElement;{$endif}
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure CommitStoreChanged(const ACollection: INyxCollection;
      const AChanges: INyxCollectionChanges);
    procedure CommitViewChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure Refuses(const AColumns: INyxCollectionColumns;
      const ACapture: INyxCollectionColumnsCapture;
      const APreferences: TNyxColumnPreferences; const AReason: TNyxText);
    procedure CapabilityCases;
    procedure RebindCases; {$ifdef PAS2JS}async;{$endif}
    procedure ColumnClick(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution);
    procedure LegacyClick(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  public
    constructor Create(AClickProof: Boolean = False);
    destructor Destroy; override;
    procedure Run; {$ifdef PAS2JS}async;{$endif}
    procedure QueryLifetimeCases; {$ifdef PAS2JS}async;{$endif}
    procedure ColumnManagerCases; {$ifdef PAS2JS}async;{$endif}
    procedure ColumnActivationCases; {$ifdef PAS2JS}async;{$endif}
    {$ifdef PAS2JS}procedure OpenForReview;{$endif}
  end;

{$ifdef PAS2JS}
var
  { Manual review deliberately retains this SAME owner until page retirement.
    Its path publishes no automated pass/count and runs none of the journeys. }
  GColumnReview: TArrangementReview;
{$endif}

function ArrangementClicks: TNyxIntegerStateRef;
begin
  Result := NyxIntegerState('arrangement-columns-clicks');
end;

procedure TArrangementClickReceiver.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Owner.ColumnClick(AEvent, AExecution);
end;

function ArrangementSpec: TNyxCollectionViewSpec;
begin
  { Only this owned setup opts in; the original Spec/Fixture bytes and defaults
    still qualify their complete ordinary query journey before this case. }
  Result := NyxCollectionView(People).Scoped(csInstance)
    .Column(PersonName, 'Name', cmEditable)
      .ColumnSizing(NyxColumnSizing.Fixed(160).Minimum(50).Maximum(300))
    .Column(Score, 'Score', cmEditable)
      .ColumnSizing(NyxColumnSizing.Fixed(90).Minimum(50).Maximum(220))
    .Column(Active, 'Active', cmEditable)
      .ColumnSizing(NyxColumnSizing.Fixed(80).Minimum(50).Maximum(140))
    .Selection(nsmMultiple).Query(Baseline)
    .Paged(NyxCollectionPaging.PageSize(2)).TableLayout(NyxTableLayout);
end;

constructor TArrangementReview.Create(AClickProof: Boolean);
var
  LDefinition: TNyxNode;
  LReceiver: TArrangementClickReceiver;
begin
  inherited Create;
  SetLength(FViews, 2);
  SetLength(FColumns, 2);
  FDocument := Fixture;
  FDocument.Find(CFirst).SetCollectionView(ArrangementSpec);
  FDocument.Find(CSecond).SetCollectionView(ArrangementSpec);
  FDocument.Find(CFirst).Configure.ReadOnly(False);

  if AClickProof then
  begin
    { Only the separate activation invocation opts into a real scalar Click
      action. The same seed, reusable definitions, stores and table policy stay
      intact; no synthetic child action stands in for LiveBindings.Dispatch. }
    FDocument.State.SetValue(ArrangementClicks, 0);
    FDocument.Find(CFirst).Contract.Value(NyxIntegerDomain);
    FDocument.Find(CFirst).Configure.Value(0).Action(naIncrement)
      .OnClick(NyxEvent(CArrangementClick)).Done;
    FDocument.Find(CFirst).Binds.Value(ArrangementClicks).Done;
  end;
  { Instance scope belongs to the nearest reusable owner, not each control on
    a page. Real reusable instances make the independent-store assertion below
    exercise that public contract; the original query fixture stays unchanged. }
  LDefinition := FDocument.Pages[0].Extract(0);
  try
    FDocument.AddComponent(LDefinition);
    LDefinition := nil;
  finally
    ReleaseNyxNode(LDefinition);
  end;
  LDefinition := FDocument.Pages[0].Extract(0);
  try
    FDocument.AddComponent(LDefinition);
    LDefinition := nil;
  finally
    ReleaseNyxNode(LDefinition);
  end;
  FDocument.Pages[0].Add(NewNyxComponent('arrangement-first')
    .Configure.Component(NyxComponent(CFirst)).Done);
  FDocument.Pages[0].Add(NewNyxComponent('arrangement-second')
    .Configure.Component(NyxComponent(CSecond)).Done);
  FDocument.Validate;
  FAuthored := TNyxCodec.Encode(FDocument);
  FApp := TReviewApplication.Create;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(Web.document.createElement('main'));
  FHost.style.setProperty('width', IntToStr(GWidth) + 'px');
  Web.document.body.appendChild(FHost);
  FApp.Run(FDocument, FHost);
  {$else}
  FApp.Mount(FDocument);
  FApp.Window.ClientWidth := GWidth;
  FApp.Window.ClientHeight := 800;
  FApp.Window.Show;
  Pump;
  Check(FApp.Window.ClientWidth = GWidth,
    'arrangement application retains the actual requested native allocation');
  {$endif}
  FViews[0] := FApp.View.CollectionView(CArrangementFirst);
  FViews[1] := FApp.View.CollectionView(CArrangementSecond);
  FColumns[0] := NyxCollectionColumns(FViews[0]);
  FColumns[1] := NyxCollectionColumns(FViews[1]);

  if AClickProof then
  begin
    LReceiver := TArrangementClickReceiver.Create;
    LReceiver.Owner := Self;
    FClickReceiver := LReceiver;
    FClickToken := FApp.View.Events.OnNamed(NyxControlEvents(CArrangementFirst, niRuntime),
      NyxEvent(CArrangementClick)).Subscribe(FClickReceiver);
    FReleaseToken := FApp.View.Events.OnKeyUp(NyxControlEvents(CArrangementFirst, niRuntime))
      .Subscribe(FClickReceiver);
    FDoubleToken := FApp.View.Events.OnDoubleClick(NyxControlEvents(CArrangementFirst, niRuntime))
      .Subscribe(FClickReceiver);
    FApp.View.OnEvent := LegacyClick;
  end;
end;

destructor TArrangementReview.Destroy;
begin

  if FClickToken <> nil then
  begin
    FClickToken.Cancel;
  end;

  if FReleaseToken <> nil then
  begin
    FReleaseToken.Cancel;
  end;

  if FDoubleToken <> nil then
  begin
    FDoubleToken.Cancel;
  end;
  FClickToken := nil;
  FReleaseToken := nil;
  FDoubleToken := nil;
  FClickReceiver := nil;
  FProofManager := nil;

  if FApp <> nil then
  begin
    FApp.View.OnEvent := nil;
  end;

  if FSubscription <> nil then
  begin
    FSubscription.Disconnect;
  end;
  FSubscription := nil;
  FColumns[0] := nil;
  FColumns[1] := nil;
  FViews[0] := nil;
  FViews[1] := nil;
  FApp.Free;
  FApp := nil;
  FDocument.Free;
  FDocument := nil;
  {$ifdef PAS2JS}

  if FHost <> nil then
  begin
    FHost.remove;
    FHost := nil;
  end;
  {$endif}
  inherited Destroy;
end;

procedure TArrangementReview.ColumnClick(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LResponse: INyxEventResponse;
begin
  Check((AEvent.OriginID = CArrangementFirst) and
    (AEvent.SourceID = CArrangementFirst),
    'Columns canonical callback retains its exact realized table identity');

  if AEvent.Trigger = ntKeyUp then
  begin
    Inc(FReleaseCalls);

    if FClickMode = acmConsumeRelease then
    begin
      Check(Supports(AExecution, INyxEventResponse, LResponse) and
        LResponse.CanConsume, 'Columns release exposes its real canonical input response');
      LResponse.Consume;
    end;
    Exit;
  end;

  if AEvent.Trigger = ntDoubleClick then
  begin
    Inc(FDoubleCalls);

    if FClickMode = acmCloseDouble then
    begin
      { Closing inside the REAL double-click callback makes an illicit third
        default Open observable; retained form identity alone could not do so. }
      FProofManager.Presenter.Close;
    end;
    Exit;
  end;
  Check((AEvent.Trigger = ntClick) and AEvent.IsNamed(NyxEvent(CArrangementClick)),
    'Columns activation delivers the authored Click instead of a replacement Signal');
  Check(AEvent.HasValue and (AEvent.ValueKind = nskInteger) and
    (AEvent.Value.AsInteger = FApp.State.GetValue(ArrangementClicks)),
    'Columns semantic Click observes its actual committed scalar action');
  Inc(FSemanticClicks);
  {$ifdef PAS2JS}

  if FClickMode = acmPreventClick then
  begin
    Check(FCurrentClick <> nil, 'Columns callback owns the exact entered DOM event');
    FCurrentClick.preventDefault;
  end;
  {$endif}

  if FClickMode = acmUnmount then
  begin
    FApp.View.Unmount;
  end;
end;

procedure TArrangementReview.LegacyClick(ANode: TNyxNode;
  const AEvent: TNyxEventInfo);
begin

  if (AEvent.Trigger = ntClick) and AEvent.IsNamed(NyxEvent(CArrangementClick)) then
  begin
    Check((ANode.ID = CArrangementFirst) and (AEvent.SourceID = CArrangementFirst),
      'Columns legacy Click retains the exact realized table and semantic name');
    Inc(FLegacyClicks);
  end;
end;

procedure TArrangementReview.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(FNotifications);
  FLastChangesNil := AChanges = nil;

  if FReenter then
  begin
    FReenter := False;
    try
      NyxCollectionColumns(AView).ConfigureColumns(NyxColumnPreferences);
    except
      on Exception do FReentryRefused := True;
    end;
  end;
end;

procedure TArrangementReview.CommitStoreChanged(const ACollection: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
  Check((ACollection = FViews[0].Store) and (AChanges <> nil),
    'pinned scalar observer receives the original store and accepted data changes');
  Inc(FCommitStoreNotifications);
  FCommitStoreChanges := AChanges;
end;

procedure TArrangementReview.CommitViewChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Check(AView = FViews[0], 'pinned scalar view observer retains the original view');

  if AChanges <> nil then
  begin
    Inc(FCommitViewNotifications);
    FCommitViewChanges := AChanges;
  end;
end;

procedure TArrangementReview.Refuses(const AColumns: INyxCollectionColumns;
  const ACapture: INyxCollectionColumnsCapture;
  const APreferences: TNyxColumnPreferences; const AReason: TNyxText);
var
  LStage: INyxPreparedPublication;
  LRefused: Boolean;
begin
  LRefused := False;
  try
    LStage := AColumns.PrepareColumns(ACapture, APreferences);
    PublishNyxGroup([LStage]);
  except
    on Exception do LRefused := True;
  end;
  LStage := nil;
  Check(LRefused, AReason);
end;

procedure TArrangementReview.CapabilityCases;
var
  LSpec: TNyxCollectionViewSpec;
  LView: INyxCollectionView;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LStage: INyxPreparedPublication;
  LAccepted: TNyxText;
begin
  { These detached views exercise capability admission over the actual store.
    They are not substitute physical controls or arrangement-transfer proof. }
  LSpec := ArrangementSpec.TableLayout(NyxTableLayout.AllowOnly([nclfAutoSize]));
  LView := NewNyxCollectionView(FViews[0].Store, LSpec, cpTable);
  LColumns := NyxCollectionColumns(LView);
  LCapture := LColumns.CaptureColumns;
  LStage := LColumns.PrepareAutoSize(LCapture, NyxColumnIdentity(PersonName), 180);
  PublishNyxGroup([LStage]);
  LStage := nil;
  LCapture := LColumns.CaptureColumns;
  Check((LCapture.Preferences.Count = 1) and
    (LCapture.Preferences.ColumnAt(0).WidthValue = 180),
    'auto-size-only permission can publish a host-measured width without resize permission');
  LAccepted := LCapture.Preferences.ToData.ToJSON;
  Refuses(LColumns, LCapture, NyxColumnPreferences.WithColumn(
    NyxColumnPreference(NyxColumnIdentity(PersonName)).Width(190)),
    'ordinary width command cannot borrow the separate auto-size permission');
  Check(LColumns.CaptureColumns.Preferences.ToData.ToJSON = LAccepted,
    'refused resize retains accepted auto-size result');
  LColumns.ConfigureColumns(NyxColumnPreferences);
  Check(LColumns.CaptureColumns.Preferences.Count = 0,
    'removing a width override needs no resize permission');
  LColumns := nil;
  LView := nil;
  LSpec := NyxCollectionView(People).Column(PersonName, 'Name')
    .Column(Score, 'Score').ColumnLayout(NyxColumnLayout.Visible(False)
      .AllowOnly([nclfResize, nclfAutoSize, nclfVisibility, nclfPinning]))
    .Column(Active, 'Active').TableLayout(NyxTableLayout);
  LView := NewNyxCollectionView(FViews[0].Store, LSpec, cpTable);
  LColumns := NyxCollectionColumns(LView);
  LCapture := LColumns.CaptureColumns;
  Refuses(LColumns, LCapture, NyxColumnPreferences.Order([
    NyxColumnIdentity(Active), NyxColumnIdentity(Score), NyxColumnIdentity(PersonName)]),
    'peers cannot cross a hidden reorder barrier even when its ordinal stays unchanged');
  Refuses(LColumns, LCapture, NyxColumnPreferences.Order([
    NyxColumnIdentity(Score), NyxColumnIdentity(PersonName), NyxColumnIdentity(Active)]),
    'hidden reorder-disabled field cannot change complete global ordinal');
  Check(LColumns.CaptureColumns.Preferences.OrderCount = 0,
    'reorder refusals retain current authored order without a partial prefix');
end;

{$ifdef PAS2JS}
function ArrangementFrame: TJSPromise; forward;
{$endif}

procedure TArrangementReview.RebindCases; {$ifdef PAS2JS}async;{$endif}
var
  LInitial, LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LRuntime: TNyxApplicationState;
  LBindings, LNextBindings: INyxCollectionBindings;
  LRenderer: TReviewRenderer;
  LView: INyxCollectionView;
  LColumns: INyxCollectionColumns;
  LSpec: TNyxCollectionViewSpec;
  LFormer, LCurrent: INyxCollectionColumnsCapture;
  LPreference: TNyxColumnPreference;
  LPreferences: TNyxColumnPreferences;
  LStore: INyxCollection;
  LData: INyxCollectionSnapshot;
  LDraft: TNyxText;
  LSelection: TNyxTextSelection;
  LBefore, LAuthored, LInitialAuthored, LAppPreferences: TNyxText;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LTable: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LEditor: TJSHTMLInputElement;
  LOptions: TJSObject;
  {$else}
  LHost: TPanel;
  LGrid: TStringGrid;
  LFace: TStringGrid;
  LEditor: TCustomEdit;
  LPhysical: Integer;
  {$endif}

  {$ifndef PAS2JS}
  function NameFace(out APhysical: Integer): TStringGrid;
  var
    LChild: Integer;
    LCandidate: TStringGrid;

    function Matches(AGrid: TStringGrid): Boolean;
    var
      LColumn: Integer;
    begin
      Result := False;
      for LColumn := 0 to AGrid.ColCount - 1 do
      begin

        if Pos(LView.Spec.ColumnAt(0).Title, TNyxText(AGrid.Cells[LColumn, 0])) = 1 then
        begin
          APhysical := LColumn;
          Exit(True);
        end;
      end;
    end;

  begin
    LGrid := TStringGrid(LRenderer.ControlFor(CArrangementFirst));
    APhysical := -1;
    Result := LGrid;

    if Matches(LGrid) then
    begin
      Exit;
    end;
    for LChild := 0 to LGrid.ControlCount - 1 do
    begin

      if (LGrid.Controls[LChild] is TStringGrid) and LGrid.Controls[LChild].Visible then
      begin
        LCandidate := TStringGrid(LGrid.Controls[LChild]);

        if Matches(LCandidate) then
        begin
          Exit(LCandidate);
        end;
      end;
    end;
    Result := nil;
  end;
  {$endif}

  procedure CheckFocusedDraft(const AStage: TNyxText);
  var
    LActualSelection: TNyxTextSelection;
    LActualText: TNyxText;
    LFocused: Boolean;
  begin
    { Resolve the CURRENT physical face by the same typed field/item. Neither
      the moved row coordinate nor an editor borrowed from the old renderer
      can establish preservation across this controlled replacement. }
    {$ifdef PAS2JS}
    LTable := LRenderer.ElementFor(CArrangementFirst);
    LCell := TJSHTMLElement(LTable.querySelector(
      'tr[data-nyx-item="d"] td[data-nyx-column="0"]'));
    Check(LCell <> nil, AStage + ': exact arranged Name/d cell remains realized');
    LEditor := TJSHTMLInputElement(LCell.querySelector('input.nyx-cell-editor'));
    Check(LEditor <> nil, AStage + ': the current cell owns a real text editor');
    LFocused := (Web.document.activeElement = LEditor) and not LEditor.hasAttribute('hidden');
    LActualText := NyxBrowserInputText(LEditor);
    LActualSelection := CaptureNyxBrowserSelection(LEditor);
    {$else}
    LFace := NameFace(LPhysical);
    Check((LFace <> nil) and (LPhysical >= 0),
      AStage + ': exact arranged Name field remains on a current native surface');
    Check(LFace.EditorMode and (LFace.Editor is TCustomEdit),
      AStage + ': the current native face owns an active text editor');
    LEditor := TCustomEdit(LFace.Editor);
    LFocused := LEditor.Focused and (LFace.Col = LPhysical) and
      (LFace.Row = LView.Snapshot.IndexOf(Person('d')) + 1);
    LActualText := NyxLCLInputText(LEditor);
    LActualSelection := CaptureNyxLCLSelection(LEditor);
    {$endif}
    Check(LFocused and (LActualText = LDraft) and LActualSelection.Defined and
      LActualSelection.SameRange(LSelection),
      AStage + ': exact unfinished Unicode text, scalar caret and actual focus survive');
    Check(LView.Store.Snapshot = LData,
      AStage + ': physical draft transfer publishes no accepted scalar');
  end;
begin
  { This controlled renderer replacement owns an independent runtime over the
    same admitted arrangement seed. Retain its REAL initial page bindings from
    first mount: the preceding application's private bindings are not public,
    and a fresh default context would not preserve its instance-store identity.
    No store below is attributed to FApp or substituted for its earlier journey. }
  LInitial := nil;
  LDocument := nil;
  LRoot := nil;
  LRuntime := nil;
  LRenderer := nil;
  LHost := nil;
  LAppPreferences := FColumns[0].CaptureColumns.Preferences.ToData.ToJSON;
  try
    LInitial := FDocument.Clone;
    LInitialAuthored := TNyxCodec.Encode(LInitial);
    {$ifdef PAS2JS}
    LRuntime := TNyxApplicationState.Create(LInitial, npfBrowser);
    LHost := TJSHTMLElement(Web.document.createElement('section'));
    LHost.style.setProperty('width', IntToStr(GWidth) + 'px');
    LHost.style.setProperty('height', '800px');
    FHost.appendChild(LHost);
    {$else}
    LRuntime := TNyxApplicationState.Create(LInitial, npfNativeLCL);
    LHost := TPanel.Create(nil);
    LHost.Parent := FApp.Window;
    LHost.BevelOuter := bvNone;
    LHost.Align := alClient;
    LHost.BringToFront;
    {$endif}
    LBindings := LRuntime.PageCollections(LInitial.Pages[0].ID);
    LRenderer := TReviewRenderer.Create;
    LRenderer.Render(LInitial, LInitial.Pages[0], LHost, False,
      LRuntime.State, LBindings);
    LView := LRenderer.CollectionView(CArrangementFirst);
    LColumns := NyxCollectionColumns(LView);
    Check((LView = LBindings.ViewFor(CArrangementFirst)) and
      (LView.Store <> FViews[0].Store),
      'controlled replacement starts with its real independent runtime binding');
    LPreferences := NyxColumnPreferences
      .WithColumn(NyxColumnPreference(NyxColumnIdentity(PersonName)).Width(140).Pinned(ncpStart))
      .WithColumn(NyxColumnPreference(NyxColumnIdentity(Score)).Width(80).Pinned(ncpEnd))
      .Order([NyxColumnIdentity(Score), NyxColumnIdentity(PersonName), NyxColumnIdentity(Active)]);
    LColumns.ConfigureColumns(LPreferences);
    LView.ConfigureQuery(NyxCollectionQuery.OrderBy(Score));
    LView.Select(Person('b'));
    LStore := LView.Store;
    LData := LStore.Snapshot;
    LDraft := TNyxText('Draft 🌙 / retained Dee');
    LSelection := NyxTextSelection(LDraft, 8, 11);
    LRenderer.Reveal(CArrangementFirst, niRuntime);
    {$ifdef PAS2JS}
    await(ArrangementFrame);
    LTable := LRenderer.ElementFor(CArrangementFirst);
    LCell := TJSHTMLElement(LTable.querySelector(
      'tr[data-nyx-item="d"] td[data-nyx-column="0"]'));
    Check(LCell <> nil, 'replacement draft starts in its actual arranged Name/d cell');
    LCell.focus;
    LOptions := TJSObject.new;
    LOptions['key'] := 'F2';
    LOptions['bubbles'] := True;
    LOptions['cancelable'] := True;
    LCell.dispatchEvent(TReviewKey.new('keydown', LOptions));
    await(ArrangementFrame);
    LEditor := TJSHTMLInputElement(LCell.querySelector('input.nyx-cell-editor'));
    Check(LEditor <> nil, 'replacement draft uses the actual browser F2 editor');
    LEditor.value := LDraft;
    SelectNyxBrowserText(LEditor, LSelection);
    LSelection := CaptureNyxBrowserSelection(LEditor);
    {$else}
    LFace := NameFace(LPhysical);
    Check(LFace <> nil, 'replacement draft starts on the actual arranged Name surface');
    LFace.Col := LPhysical;
    LFace.Row := LView.Snapshot.IndexOf(Person('d')) + 1;
    LFace.SetFocus;
    SendMessage(LFace.Handle, WM_KEYDOWN, VK_F2, 0);
    Pump;
    Check(LFace.EditorMode and (LFace.Editor is TCustomEdit),
      'replacement draft uses the actual native F2 editor');
    LEditor := TCustomEdit(LFace.Editor);
    LEditor.Text := LDraft;
    SelectNyxLCLText(LEditor, LSelection);
    LSelection := CaptureNyxLCLSelection(LEditor);
    {$endif}
    { Native text slots report Unknown endpoint direction; retain the actual
      platform representation after proving the requested scalar boundaries. }
    Check(LSelection.Defined and (LSelection.Start = 8) and (LSelection.Finish = 11),
      'replacement draft selection uses exact Unicode scalar boundaries after the supplementary character');
    Check(LView.Snapshot.IndexOf(Person('d')) = 1,
      'arranged draft starts on the second current-page item');
    CheckFocusedDraft('before filtering');
    { The public query path both filters accepted rows and changes their order
      while the SAME dirty cell stays focused. Its item moves from row two to
      row one; the following descriptor replacement restores the authored
      baseline and moves that item back to row two without accepting its text. }
    LView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(Score).AtMost(4))
      .OrderBy(PersonName, nsdDescending));
    {$ifdef PAS2JS}await(ArrangementFrame);{$else}Pump;{$endif}
    Check((LView.Snapshot.Count = 2) and
      (LView.Snapshot.ItemAt(0).Ref.ID = 'd') and
      (LView.Snapshot.ItemAt(1).Ref.ID = 'c'),
      'real filter and order move the focused draft by item identity on the current page');
    CheckFocusedDraft('after filtering and ordering');
    LFormer := LColumns.CaptureColumns;
    LStore := LView.Store;
    LBefore := LFormer.Preferences.ToData.ToJSON;
    LDocument := LInitial.Clone;
    LSpec := NyxCollectionView(People).Scoped(csInstance)
      .Column(PersonName, 'Current service name', cmEditable)
        .ColumnSizing(NyxColumnSizing.Fixed(200).Minimum(50).Maximum(300))
      .Column(Active, 'Current running state', cmEditable)
        .ColumnLayout(NyxColumnLayout.Visible(False).PinEnd)
      .Selection(nsmMultiple).Query(Baseline)
      .Paged(NyxCollectionPaging.PageSize(2))
      .TableLayout(NyxTableLayout.Enabled(False));
    LDocument.Find(CFirst).SetCollectionView(LSpec);
    LDocument.Validate;
    LAuthored := TNyxCodec.Encode(LDocument);
    Check(not LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False),
      'raw renderer refresh retains its exact collection-descriptor refusal');
    { Recompose is the existing model transfer seam. Full Render then admits
      those exact views against fresh physical controls, borrowing the SAME
      runtime state and bindings. Both source documents outlive the renderer,
      including any exception after its accepted ownership transition. }
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LNextBindings := LBindings.Recompose(LRoot);
    LRoot.Free;
    LRoot := nil;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost, False,
      LRuntime.State, LNextBindings);
    LColumns := nil;
    LView := LRenderer.CollectionView(CArrangementFirst);
    LColumns := NyxCollectionColumns(LView);
    {$ifdef PAS2JS}await(ArrangementFrame);{$else}Pump;{$endif}
    Check((LView.Snapshot.ItemAt(0).Ref.ID = 'f') and
      (LView.Snapshot.ItemAt(1).Ref.ID = 'd'),
      'full replacement restores its authored page independently of the copied cell draft');
    CheckFocusedDraft('after full renderer replacement');
    Check(LView = LNextBindings.ViewFor(CArrangementFirst),
      'full renderer replacement mounts the actual recomposed binding');
    LCurrent := LColumns.CaptureColumns;
    Check((LView.Store = LStore) and (LCurrent.Preferences.Count = 1) and
      (LCurrent.Preferences.OrderCount = 2),
      'compatible replacement retains the same instance store and prunes removed-field preferences');
    LPreference := LCurrent.Preferences.ColumnAt(0);
    Check(LPreference.Field.Same(NyxColumnIdentity(PersonName)) and
      (LPreference.WidthValue = 140) and (LPreference.Pin = ncpStart) and
      (LFormer.Preferences.ToData.ToJSON = LBefore),
      'retained typed choices stay independent of copied old captures and new authored defaults');
    Check(not LCurrent.Spec.TableLayoutPolicy.IsEnabled and
      not LCurrent.Spec.ColumnAt(1).LayoutPolicy.IsVisible and
      (LCurrent.Spec.ColumnAt(1).LayoutPolicy.Pin = ncpEnd),
      'new static visibility and logical pin defaults apply despite disabled user commands');
    Check((LView.QueryPolicy.ToData.ToJSON = Baseline.ToData.ToJSON) and
      not LView.HasSelection,
      'column transfer does not broaden old exact-Spec query or selection admission');
    Refuses(LColumns, LFormer, NyxColumnPreferences,
      'former view capture cannot target its same-ID renderer replacement');
    Refuses(LColumns, LCurrent, LCurrent.Preferences.WithColumn(
      NyxColumnPreference(NyxColumnIdentity(PersonName)).Width(150)),
      'disabled replacement refuses new user layout commands');
    LColumns.ConfigureColumns(NyxColumnPreferences);
    Check((LColumns.CaptureColumns.Preferences.Count = 0) and
      (LColumns.CaptureColumns.Preferences.OrderCount = 0),
      'reset remains usable after authored policy disables further layout edits');
    Check((TNyxCodec.Encode(LDocument) = LAuthored) and
      (TNyxCodec.Encode(LInitial) = LInitialAuthored) and
      (TNyxCodec.Encode(FDocument) = FAuthored),
      'replacement and reset do not rewrite either local source or the preceding application document');
    Check((FViews[0] = FApp.View.CollectionView(CArrangementFirst)) and
      (FColumns[0].CaptureColumns.Preferences.ToData.ToJSON = LAppPreferences),
      'controlled replacement does not retarget the preceding application or its preferences');
  finally
    { A renderer borrows its state/source/host; retire its hooks FIRST. The
      retained runtime and both exact documents remain alive throughout cleanup. }
    LRenderer.Free;
    LRenderer := nil;
    LColumns := nil;
    LView := nil;
    LNextBindings := nil;
    LBindings := nil;
    LRuntime.Free;
    LRuntime := nil;
    LRoot.Free;
    LDocument.Free;
    LInitial.Free;
    {$ifdef PAS2JS}

    if LHost <> nil then
    begin
      LHost.remove;
    end;
    {$else}
    LHost.Free;
    {$endif}
  end;
end;

procedure TArrangementReview.Run; {$ifdef PAS2JS}async;{$endif}
var
  LCapture, LPeer, LCurrent: INyxCollectionColumnsCapture;
  LPreferences: TNyxColumnPreferences;
  LStage: INyxPreparedPublication;
  LSnapshot: INyxCollectionSnapshot;
  LSelection: TNyxCollectionSelectionSnapshot;
  LQuery, LSpec, LAccepted: TNyxText;
  LRevision, LSerial: Integer;
begin
  Check((FViews[0] <> FViews[1]) and (FViews[0].Store <> FViews[1].Store),
    'arrangement setup uses independent real instance views and stores');
  FViews[0].Select(Person('f'));
  LSnapshot := FViews[0].Snapshot;
  LSelection := FViews[0].Selection.Snapshot;
  LRevision := FViews[0].Store.Snapshot.Revision;
  LQuery := FViews[0].QueryPolicy.ToData.ToJSON;
  LSpec := FViews[0].Spec.ToData.ToJSON;
  LCapture := FColumns[0].CaptureColumns;
  LPeer := FColumns[1].CaptureColumns;
  Check(LCapture.Spec.HasLayoutControls and LCapture.Preferences.Defined and
    (LCapture.Preferences.Count = 0) and (LCapture.Preferences.OrderCount = 0),
    'accepted table exposes authored descriptors and independent empty preferences');
  FSubscription := FViews[0].Subscribe(Changed);
  LPreferences := NyxColumnPreferences
    .WithColumn(NyxColumnPreference(NyxColumnIdentity(PersonName)).Width(140).Pinned(ncpStart))
    .WithColumn(NyxColumnPreference(NyxColumnIdentity(Score)).Width(80).Pinned(ncpEnd))
    .Order([NyxColumnIdentity(Score), NyxColumnIdentity(PersonName), NyxColumnIdentity(Active)]);
  LStage := FColumns[0].PrepareColumns(LCapture, LPreferences);
  Check((FColumns[0].CaptureColumns.Preferences.Count = 0) and (FNotifications = 0),
    'preparing layout is detached before ordinary coordinated publication');
  FReenter := True;
  PublishNyxGroup([LStage]);
  LStage := nil;
  LCurrent := FColumns[0].CaptureColumns;
  Check((FNotifications = 1) and FLastChangesNil and FReentryRefused,
    'layout notifies once through the view and refuses reentrant layout mutation');
  Check((LCurrent.Preferences.ToData.ToJSON = LPreferences.ToData.ToJSON) and
    (LCapture.Preferences.Count = 0),
    'accepted typed layout is copied while the former immutable capture stays unchanged');
  Check((FViews[0].Snapshot = LSnapshot) and
    FViews[0].Selection.Snapshot.SameState(LSelection) and
    (FViews[0].Store.Snapshot.Revision = LRevision) and
    (FViews[0].QueryPolicy.ToData.ToJSON = LQuery) and
    (FViews[0].Spec.ToData.ToJSON = LSpec) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'layout publication changes no accepted rows selection query store source or authored defaults');
  Check((FColumns[1].CaptureColumns.Preferences.Count = 0) and
    (LPeer.Preferences.Count = 0), 'sibling instance preferences remain independent');
  LAccepted := LCurrent.Preferences.ToData.ToJSON;
  LSerial := LCurrent.Serial;
  Refuses(FColumns[0], LCapture, NyxColumnPreferences,
    'stale same-table capture cannot replace a newer accepted layout');
  Refuses(FColumns[0], LPeer, NyxColumnPreferences,
    'another table capture cannot retarget a prepared layout');
  Refuses(FColumns[0], LCurrent, LPreferences.WithColumn(NyxColumnPreference(
    NyxColumnIdentity(NyxTextField('missing-service-field'))).Width(100)),
    'unknown schema field refuses before layout publication');
  Refuses(FColumns[0], LCurrent, NyxColumnPreferences.WithColumn(
    NyxColumnPreference(NyxColumnIdentity(NyxTextField('score'))).Width(100)),
    'same field spelling with a different scalar family refuses');
  Refuses(FColumns[0], LCurrent, LPreferences.WithColumn(
    NyxColumnPreference(NyxColumnIdentity(Score)).Width(221)),
    'runtime width respects the column authored maximum');
  Check((FColumns[0].CaptureColumns.Serial = LSerial) and
    (FColumns[0].CaptureColumns.Preferences.ToData.ToJSON = LAccepted) and
    (FNotifications = 1), 'rejected layout candidates preserve serial preferences and observers');
  FColumns[0].ConfigureColumns(NyxColumnPreferences);
  Check((FColumns[0].CaptureColumns.Preferences.Count = 0) and
    (FColumns[0].CaptureColumns.Preferences.OrderCount = 0) and
    (FNotifications = 2), 'runtime reset removes width pin and order overrides together');
  Check((FViews[0].Spec.ToData.ToJSON = LSpec) and
    (FViews[0].QueryPolicy.ToData.ToJSON = LQuery) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'runtime default reset preserves all authored and query choices');
  CapabilityCases;
  FSubscription.Disconnect;
  FSubscription := nil;
  FColumns[0].ConfigureColumns(LPreferences);
  {$ifdef PAS2JS}await(RebindCases);{$else}RebindCases;{$endif}
  LSpec := FViews[0].Spec.ToData.ToJSON;
  LCurrent := FColumns[0].CaptureColumns;
  FColumns[0] := nil;
  FColumns[1] := nil;
  FViews[0] := nil;
  FViews[1] := nil;
  FApp.Free;
  FApp := nil;
  Check(LCurrent.Preferences.Defined and (LCurrent.Spec.ToData.ToJSON = LSpec),
    'copied column observation remains readable after its application and view retire');
  Check(TNyxCodec.Encode(GDocument) = GAuthored,
    'independent arrangement journey preserves the original fixture and globals');
end;

procedure ArrangementCases; {$ifdef PAS2JS}async;{$endif}
var
  LReview: TArrangementReview;
begin
  LReview := TArrangementReview.Create;
  try
    {$ifdef PAS2JS}await(LReview.Run);{$else}LReview.Run;{$endif}
  finally
    LReview.Free;
  end;
end;

{$ifdef PAS2JS}
function ArrangementFrame: TJSPromise;
begin
  { Wait for the adapter's ordinary animation-frame work after real focus/input.
    No publisher is invoked directly and no timeout stands in for completion. }
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.requestAnimationFrame(procedure(ATime: Double)
        begin
          AResolve(JSValue(True));
        end);
    end);
end;
{$endif}

procedure TArrangementReview.QueryLifetimeCases; {$ifdef PAS2JS}async;{$endif}
var
  LMount: INyxCollectionMount;
  LQueryMount: INyxCollectionQueryMount;
  LQueries: INyxCollectionQueryWorkbench;
  LPresenter: INyxQueryFormPresenter;
  LPresentation: INyxPopover;
  LFormRenderer: TReviewRenderer;
  LMainGeneration: INyxCancellationScope;
  LFormGeneration: INyxCancellationScope;
  LFreshGeneration: INyxCancellationScope;
  LUnderlyingGeneration: INyxCancellationScope;
  LMainReady: INyxCancellationReadiness;
  LFormReady: INyxCancellationReadiness;
  LFreshReady: INyxCancellationReadiness;
  LEditMount: INyxCollectionEditMount;
  LEdits: INyxCollectionEditsSnapshot;
  LCellEdit: TNyxCollectionCellEdit;
  LData: INyxCollectionSnapshot;
  LItem: TNyxItemRef;
  LQuery: TNyxText;
  LValueID: TNyxText;
  LDraft: TNyxText;
  LIndex: Integer;
  LRefused: Boolean;
  LFound: Boolean;
  {$ifdef PAS2JS}
  LTarget: INyxBrowserQueryFormPresenter;
  LTable: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LEditor: TJSHTMLInputElement;
  LQueryInput: TJSHTMLInputElement;
  LHeader: TJSHTMLElement;
  LOptions: TJSObject;
  {$else}
  LTarget: INyxLCLQueryFormPresenter;
  LGrid: TStringGrid;
  LEditor: TCustomEdit;
  LQueryInput: TEdit;
  LBounds: TRect;
  {$endif}

  procedure ResolveForm;
  begin
    LPresenter := LQueries.Presenter;
    Check((LPresenter <> nil) and LPresenter.Connected and LPresenter.IsOpen,
      'arranged query work uses an actual connected open form');
    {$ifdef PAS2JS}
    Check(Supports(LPresenter, INyxBrowserQueryFormPresenter, LTarget),
      'arranged browser form retains its original platform presenter facet');
    {$else}
    Check(Supports(LPresenter, INyxLCLQueryFormPresenter, LTarget),
      'arranged native form retains its original platform presenter facet');
    {$endif}
    LPresentation := LTarget.Presentation;
    LFormRenderer := LTarget.Presentation.Renderer;
    Check((LPresentation <> nil) and (LFormRenderer.Root <> nil),
      'arranged form exposes the exact actual presentation and realized controls');
  end;

  procedure FormAction(const AID: TNyxText);
  begin
    LFormRenderer.Reveal(AID, niRuntime);
    {$ifdef PAS2JS}
    TJSHTMLElement(LFormRenderer.ElementFor(AID)).click;
    {$else}
    Check(LFormRenderer.ControlFor(AID).Enabled,
      'arranged query structural action is physically enabled');
    TButton(LFormRenderer.ControlFor(AID)).Click;
    {$endif}
    Pump;
  end;
begin
  { This uses the same real arrangement owner as the preceding journey, with
    its own instance stores. No global GApp/GViews or accepted query fixture is
    substituted. The existing default query policy is enabled even when absent. }
  Check(FViews[0].Spec.HasLayoutControls and
    FViews[0].Spec.TableQueryPolicy.IsEnabled,
    'actual arranged table also admits its ordinary default-enabled query controls');
  LMount := FApp.View.CollectionMount(CArrangementFirst);
  Check(Supports(LMount, INyxCollectionQueryMount, LQueryMount),
    'actual arranged mount owns its existing query workbench');
  LQueries := LQueryMount.QueryWorkbench;
  Check((LQueries <> nil) and LQueries.Connected,
    'configured arranged query factory is reachable through the mounted table');
  LMainGeneration := NyxEventGeneration(FApp.View.Events);
  Check(Supports(LMainGeneration, INyxCancellationReadiness, LMainReady),
    'actual underlying main router supplies callback-free final readiness');
  LMainReady.CheckReady;
  LQuery := FViews[0].QueryPolicy.ToData.ToJSON;
  LQueries.SortColumn(1, nsdAscending);
  Check(FViews[0].QueryPolicy.ToData.ToJSON <> LQuery,
    'actual arranged main command publishes through its configured generation fence');
  LQueries.ResetAll;
  Check(FViews[0].QueryPolicy.ToData.ToJSON = LQuery,
    'actual main reset restores the existing authored query without source edits');
  LData := FViews[0].Store.Snapshot;
  LItem := FViews[0].Snapshot.ItemAt(0).Ref;
  LDraft := TNyxText('Unfinished arranged cell / 🌙');
  FApp.View.Reveal(CArrangementFirst, niRuntime);
  {$ifdef PAS2JS}
  LTable := FApp.View.ElementFor(CArrangementFirst);
  LCell := TJSHTMLElement(LTable.querySelector('tr[data-nyx-item="' + LItem.ID +
    '"] td[data-nyx-column="0"]'));
  Check(LCell <> nil, 'arranged browser draft starts in the exact retained row and typed field');
  LCell.focus;
  LOptions := TJSObject.new;
  LOptions['key'] := 'F2';
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LCell.dispatchEvent(TReviewKey.new('keydown', LOptions));
  await(ArrangementFrame);
  LEditor := TJSHTMLInputElement(LCell.querySelector('input.nyx-cell-editor'));
  Check((LEditor <> nil) and (Web.document.activeElement = LEditor),
    'arranged browser F2 enters the actual cell editor');
  LEditor.value := LDraft;
  LEditor.setSelectionRange(2, 7);
  LHeader := TJSHTMLElement(LTable.querySelector('[data-nyx-query-column="1"]'));
  Check(LHeader <> nil, 'arranged browser header exposes its real Score filter');
  LHeader.click;
  await(ArrangementFrame);
  {$else}
  LGrid := TStringGrid(FApp.View.ControlFor(CArrangementFirst));
  LGrid.Col := 0;
  LGrid.Row := 1;
  LGrid.SetFocus;
  SendMessage(LGrid.Handle, WM_KEYDOWN, VK_F2, 0);
  Pump;
  Check(LGrid.EditorMode and (LGrid.Editor is TCustomEdit),
    'arranged native F2 enters the actual cell editor');
  LEditor := TCustomEdit(LGrid.Editor);
  LEditor.Text := LDraft;
  LEditor.SelStart := 2;
  LEditor.SelLength := 5;
  LBounds := TNyxCollectionStringGrid(LGrid).ColumnHeaderRect(1);
  TGridAccess(LGrid).MouseMove([], LBounds.Right - 14, LBounds.Top + 8);
  TGridAccess(LGrid).MouseDown(mbLeft, [], LBounds.Right - 14, LBounds.Top + 8);
  TGridAccess(LGrid).MouseUp(mbLeft, [], LBounds.Right - 14, LBounds.Top + 8);
  Pump;
  Application.Idle(False);
  Pump;
  {$endif}
  ResolveForm;
  FormAction(NyxQueryEditorActionID(
    NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqeAnd));
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  ResolveForm;
  LValueID := NyxQueryEditorFieldID(
    NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqfValue);
  {$ifdef PAS2JS}
  LQueryInput := TJSHTMLInputElement(LFormRenderer.InputFor(LValueID));
  LQueryInput.value := '-';
  LQueryInput.dispatchEvent(TJSEvent.new('input'));
  LQueryInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LQueryInput := TEdit(LFormRenderer.InputFor(LValueID));
  LQueryInput.Text := '-';
  Check(Assigned(LQueryInput.OnChange), 'arranged numeric query draft has a live text binding');
  LQueryInput.OnChange(LQueryInput);
  {$endif}
  Pump;
  LFormGeneration := NyxEventGeneration(LPresenter.Events);
  LUnderlyingGeneration := NyxEventGeneration(LPresentation.Events);
  Check(Supports(LFormGeneration, INyxCancellationReadiness, LFormReady),
    'actual configured form generation supplies callback-free final readiness');
  LFormReady.CheckReady;
  Check(not LFormGeneration.Cancelled and not LUnderlyingGeneration.Cancelled,
    'open arranged form captures its actual independent current router generation');
  { Bypass every query wrapper: this is the original actual host's silent Close.
    Final readiness must notice it without first sampling Cancelled. }
  LPresentation.Close;
  LRefused := False;
  try
    LFormReady.CheckReady;
  except
    on ENyxSchedule do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and LFormGeneration.Cancelled and not LUnderlyingGeneration.Cancelled,
    'raw actual Close revokes final form readiness without cancelling the underlying router');
  LQueries.OpenColumn(1);
  {$ifdef PAS2JS}await(ArrangementFrame);{$else}Pump;{$endif}
  Check((LQueries.Presenter = LPresenter) and LPresenter.IsOpen and
    (LTarget.Presentation.Renderer = LFormRenderer),
    'reopen preserves the exact actual presenter and platform host');
  LFreshGeneration := NyxEventGeneration(LPresenter.Events);
  Check(Supports(LFreshGeneration, INyxCancellationReadiness, LFreshReady),
    'reopened actual form exposes fresh final readiness');
  LFreshReady.CheckReady;
  Check(LFormGeneration.Cancelled and not LFreshGeneration.Cancelled,
    'actual close/reopen does not revive its captured earlier form generation');
  {$ifdef PAS2JS}
  Check(TJSHTMLInputElement(LFormRenderer.InputFor(LValueID)).value = '-',
    'actual form reopen retains unrelated incomplete numeric query input');
  {$else}
  Check(TNyxText(TEdit(LFormRenderer.InputFor(LValueID)).Text) = '-',
    'actual form reopen retains unrelated incomplete numeric query input');
  {$endif}
  LPresentation.Close;
  Check(Supports(LMount, INyxCollectionEditMount, LEditMount),
    'arranged mount exposes its real copied draft observation seam');
  LEdits := LEditMount.SuspendCellEdits;
  try
    LFound := False;
    for LIndex := 0 to LEdits.Count - 1 do
    begin
      LCellEdit := LEdits.CellAt(LIndex);

      if LCellEdit.Field.Same(NyxColumnIdentity(PersonName)) and
        (LCellEdit.Item.ID = LItem.ID) and (LCellEdit.Text = LDraft) then
      begin
        LFound := True;
      end;
    end;
    Check((LEdits.Store = FViews[0].Store) and LFound,
      'real query open/close/reopen retains the unrelated exact row/field cell draft');
  finally
    LEditMount.ResumeCellEdits(LEdits, False);
  end;
  LEdits := nil;
  Check((FViews[0].Store.Snapshot = LData) and
    (FViews[0].QueryPolicy.ToData.ToJSON = LQuery) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'arranged form lifetime and incomplete inputs preserve accepted data query and authored source');
end;

procedure ArrangedQueryCases; {$ifdef PAS2JS}async;{$endif}
var
  LReview: TArrangementReview;
begin
  { A fresh instance of the same maintained owner isolates this added physical
    journey from all preceding arrangement/query assertions and their captures. }
  LReview := TArrangementReview.Create;
  try
    {$ifdef PAS2JS}await(LReview.QueryLifetimeCases);{$else}LReview.QueryLifetimeCases;{$endif}
  finally
    LReview.Free;
  end;
end;
procedure TArrangementReview.ColumnManagerCases; {$ifdef PAS2JS}async;{$endif}
var
  LMount: INyxCollectionMount;
  LManagerMount: INyxCollectionColumnArrangementMount;
  LManager: INyxColumnArrangementManager;
  LPresenter: INyxColumnArrangementPresenter;
  LPresentation: INyxPopover;
  LFormRenderer: TReviewRenderer;
  LObservation: TNyxColumnLayoutObservation;
  LData: INyxCollectionSnapshot;
  LItem: TNyxItemRef;
  LPeer: TNyxText;
  LQuery: TNyxText;
  LExternal: TNyxText;
  LDraft: TNyxText;
  LWidthID: TNyxText;
  LGeneration: INyxCancellationScope;
  LEditMount: INyxCollectionEditMount;
  LEdits: INyxCollectionEditsSnapshot;
  LEdit: TNyxCollectionCellEdit;
  LIndex: Integer;
  LBeforeSerial: Integer;
  LPreference: TNyxColumnPreference;
  LFound: Boolean;
  LInputPreferences: TNyxText;
  LInputSemantic: Integer;
  LInputLegacy: Integer;
  LCommitStore: INyxCollection;
  LCommitStoreToken: INyxCollectionSubscription;
  LCommitViewToken: INyxCollectionViewSubscription;
  LCommitBefore: INyxCollectionSnapshot;
  LCommitAfter: INyxCollectionSnapshot;
  LCommitPeer: INyxCollectionSnapshot;
  LCommitColumns: INyxCollectionColumnsCapture;
  LCommitExpected: TNyxCollectionItem;
  LCommitChanges: INyxCollectionChanges;
  LCommitField: TNyxColumnIdentity;
  LCommitText: TNyxText;
  LCommitIndex: Integer;
  LCommitCanonical: Integer;
  LCommitRow: Integer;
  LCommitChangeIndex: Integer;
  LCommitValueIndex: Integer;
  {$ifdef PAS2JS}
  LTarget: INyxBrowserColumnArrangementPresenter;
  LTable: TJSHTMLElement;
  LButton: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LEditor: TJSHTMLInputElement;
  LWidth: TJSHTMLInputElement;
  LGrip: TJSHTMLElement;
  LOptions: TJSObject;
  LScrollOwner: TJSHTMLElement;
  LScrollStyle: String;
  LScrollLeft: Integer;
  LScrollStart: Integer;
  LScrollHeaders: array[0..2] of TJSHTMLElement;
  LScrollCells: array[0..2] of TJSHTMLElement;
  LHeaderBounds: array[0..2] of TJSDOMRect;
  LCellBounds: array[0..2] of TJSDOMRect;
  {$else}
  LTarget: INyxLCLColumnArrangementPresenter;
  LGrid: TStringGrid;
  LButton: TButton;
  LEditor: TCustomEdit;
  LWidth: TEdit;
  LPhysical: Integer;
  LScorePhysical: Integer;
  LFace: TStringGrid;
  LRestoreSemantic: Integer;
  LRestoreLegacy: Integer;
  LBeforeCursor: TNyxText;
  LBeforeF2: TNyxText;
  LAfterF2: TNyxText;
  LLaunchFar: Boolean;
  LResizeFar: Boolean;
  LExpectedDragWidth: Integer;
  LRowsSame: Boolean;
  LQuerySame: Boolean;
  LPeerSame: Boolean;
  LSourceSame: Boolean;
  LBarrier: TQueueBarrier;
  LBarrierLease: INyxWork;
  LExecution: INyxExecution;
  LStarted: QWord;
  LScrollAlign: TAlign;
  LScrollAnchors: TAnchors;
  LScrollBounds: Types.TRect;
  LScrollLeft: Integer;
  LScrollFaces: array[0..2] of TStringGrid;
  LScrollColumns: array[0..2] of Integer;
  LHeaderBounds: array[0..2] of Types.TPoint;
  LCellBounds: array[0..2] of Types.TPoint;
  LScrollPoint: Types.TPoint;
  LScrollRect: Types.TRect;
  LScrollPPI: Integer;
  LCommitHandle: HWND;
  {$endif}

  {$ifdef PAS2JS}
  { A height-constrained table is itself a block scroll viewport. Borrow the
    nearest real horizontal scroll owner, including that table, rather than
    manufacturing overflow on an unrelated parent layout. }
  function ScrollSurface(AControl: TJSHTMLElement): TJSHTMLElement;
  var
    LAncestor: TJSHTMLElement;
    LStyle: TJSCSSStyleDeclaration;
    LOverflow: String;
  begin
    LAncestor := AControl;
    while LAncestor <> nil do
    begin
      LStyle := window.getComputedStyle(LAncestor);
      LOverflow := LStyle.getPropertyValue('overflow-x');

      if (LOverflow = 'auto') or (LOverflow = 'scroll') then
      begin
        Exit(LAncestor);
      end;
      LAncestor := TJSHTMLElement(LAncestor.parentElement);
    end;
    Result := nil;
  end;
  {$endif}

  { Manager commands run ordinary renderer synchronization, which can restore
    authored dimensions. Reapply this same temporary allocation before each
    scoped scroll/editor check; the outer finally restores its exact baseline. }
  procedure ConstrainScrollAllocation;
  {$ifdef PAS2JS}
  var
    LOuterWidth: Integer;
  {$else}
  var
    LPass: Integer;
    LClientWidth: Integer;
  {$endif}
  begin
    {$ifdef PAS2JS}
    { Pin budgets use the usable client, not the surrounding CSS border box.
      Preserve 360 CSS pixels of client space even when a vertical scrollbar
      consumes part of the existing table viewport. The measured overflow and
      both-edge displacement assertions below remain independent of this setup. }
    LOuterWidth := Round(360 + LScrollOwner.offsetWidth - LScrollOwner.clientWidth);
    LScrollOwner.style.setProperty('box-sizing', 'border-box');
    LScrollOwner.style.setProperty('width', IntToStr(LOuterWidth) + 'px');
    LScrollOwner.style.setProperty('max-width', IntToStr(LOuterWidth) + 'px');
    LScrollOwner.style.setProperty('min-width', '0');
    LScrollOwner.style.setProperty('overflow-x', 'auto');
    LMount.Refresh;
    {$else}
    LGrid.Align := alNone;
    LGrid.Anchors := [akLeft, akTop];
    { Match the browser's usable-client allocation. Native borders/scrollbars
      vary with Windows settings; a fixed outer width cannot promise the same
      pin budgets. Keep all overflow/displacement assertions independent. }
    LClientWidth := Round(360 * LScrollPPI / 96);
    for LPass := 0 to 2 do
    begin
      LGrid.Width := LClientWidth + LGrid.Width - LGrid.ClientWidth;
      LMount.Refresh;

      if Abs(LGrid.ClientWidth - LClientWidth) <= 1 then
      begin
        Break;
      end;
    end;
    {$endif}
  end;

  procedure Resolve;
  var
    LConnected: Boolean;
    LOpen: Boolean;
    LDetails: TNyxText;
  begin
    LPresenter := LManager.Presenter;
    LConnected := (LPresenter <> nil) and LPresenter.Connected;
    LOpen := (LPresenter <> nil) and LPresenter.IsOpen;
    LDetails := '';

    if not (LConnected and LOpen) then
    begin
      LDetails := '; managerConnected=' + TNyxText(BoolToStr(LManager.Connected, True)) +
        '; presenterConnected=' + TNyxText(BoolToStr(LConnected, True)) +
        '; presenterOpen=' + TNyxText(BoolToStr(LOpen, True)) +
        '; layoutSerial=' + TNyxText(IntToStr(FColumns[0].CaptureColumns.Serial)) +
        '; bindingError="' + FApp.View.LastBindingError +
        '"; mountError="' + LMount.ErrorText + '"';
      {$ifdef PAS2JS}
      LDetails := LDetails + '; launcherEnabled=' +
        TNyxText(BoolToStr((LButton <> nil) and not LButton.hasAttribute('disabled'), True));
      {$else}
      LDetails := LDetails + '; launcherEnabled=' +
        TNyxText(BoolToStr((LButton <> nil) and LButton.Enabled, True)) +
        '; launcherVisible=' + TNyxText(BoolToStr((LButton <> nil) and LButton.Visible, True)) +
        '; editorMode=' + TNyxText(BoolToStr(LGrid.EditorMode, True));
      {$endif}
    end;
    Check(LConnected and LOpen,
      'column launcher opens its actual connected manager' + LDetails);
    {$ifdef PAS2JS}
    Check(Supports(LPresenter, INyxBrowserColumnArrangementPresenter, LTarget),
      'browser manager retains its actual platform presentation');
    {$else}
    Check(Supports(LPresenter, INyxLCLColumnArrangementPresenter, LTarget),
      'native manager retains its actual platform presentation');
    {$endif}
    LPresentation := LTarget.Presentation;
    LFormRenderer := LTarget.Presentation.Renderer;
    Check((LFormRenderer.Root <> nil) and
      (LPresenter.CaptureRoot = LFormRenderer.Root),
      'manager captures synchronized realized controls rather than authored Content');
    LObservation := LManager.Observation;
  end;

  procedure ClickAction(const AID: TNyxText);
  begin
    LFormRenderer.Reveal(AID, niRuntime);
    {$ifdef PAS2JS}
    TJSHTMLElement(LFormRenderer.ElementFor(AID)).click;
    {$else}
    Check(LFormRenderer.ControlFor(AID).Enabled,
      'column manager action is physically enabled');
    TButton(LFormRenderer.ControlFor(AID)).Click;
    {$endif}
    Pump;
  end;

  procedure FieldAction(const AField: TNyxColumnIdentity;
    AAction: TNyxColumnArrangementAction);
  begin
    ClickAction(NyxColumnArrangementActionID(NyxColumnArrangementEditorID,
      LObservation, AField, AAction));
  end;

  procedure EnterWidth(const AField: TNyxColumnIdentity; AWidth: Integer);
  var
    LID: TNyxText;
    {$ifdef PAS2JS}LInput: TJSHTMLInputElement;{$else}LInput: TEdit;{$endif}
  begin
    LID := NyxColumnArrangementFieldID(NyxColumnArrangementEditorID,
      LObservation, AField, ncafWidth);
    LFormRenderer.Reveal(LID, niRuntime);
    {$ifdef PAS2JS}
    LInput := TJSHTMLInputElement(LFormRenderer.InputFor(LID));
    Check((LInput <> nil) and not LInput.disabled, 'manager width is a real enabled input');
    LInput.value := IntToStr(AWidth);
    LInput.dispatchEvent(TJSEvent.new('input'));
    LInput.dispatchEvent(TJSEvent.new('change'));
    {$else}
    LInput := TEdit(LFormRenderer.InputFor(LID));
    Check((LInput <> nil) and LInput.Enabled and Assigned(LInput.OnChange),
      'manager width is a real enabled bound native input');
    LInput.Text := IntToStr(AWidth);
    LInput.OnChange(LInput);
    {$endif}
    Pump;
  end;

  procedure EnterPin(const AField: TNyxColumnIdentity; const ACaption: TNyxText);
  var
    LID: TNyxText;
    LChoice: Integer;
    {$ifdef PAS2JS}LInput: TJSHTMLSelectElement;{$else}LInput: TComboBox;{$endif}
  begin
    LID := NyxColumnArrangementFieldID(NyxColumnArrangementEditorID,
      LObservation, AField, ncafPin);
    LFormRenderer.Reveal(LID, niRuntime);
    {$ifdef PAS2JS}
    LInput := TJSHTMLSelectElement(LFormRenderer.InputFor(LID));
    LChoice := 0;
    while (LChoice < LInput.options.length) and
      (TJSHTMLOptionElement(LInput.options[LChoice]).text <> ACaption) do
    begin
      Inc(LChoice);
    end;
    Check(LChoice < LInput.options.length, 'manager pin choice is physically exposed');
    LInput.selectedIndex := LChoice;
    LInput.dispatchEvent(TJSEvent.new('change'));
    {$else}
    LInput := TComboBox(LFormRenderer.InputFor(LID));
    LChoice := LInput.Items.IndexOf(String(ACaption));
    Check((LChoice >= 0) and LInput.Enabled and Assigned(LInput.OnChange),
      'manager pin choice is a real enabled native select');
    LInput.ItemIndex := LChoice;
    LInput.OnChange(LInput);
    {$endif}
    Pump;
  end;

  function Preference(const AField: TNyxColumnIdentity): TNyxColumnPreference;
  var
    LPreferences: TNyxColumnPreferences;
    LPosition: Integer;
  begin
    LPreferences := FColumns[0].CaptureColumns.Preferences;
    LPosition := LPreferences.IndexOf(AField);
    Check(LPosition >= 0, 'real manager command installs the intended typed field preference');
    Result := LPreferences.ColumnAt(LPosition);
  end;

  {$ifdef PAS2JS}
  function HeaderFor(const AField: TNyxColumnIdentity): TJSHTMLElement;
  var
    LSort: TJSHTMLElement;
    LGrip: TJSHTMLElement;
    LCanonical: Integer;
  begin
    LCanonical := LObservation.Spec.ColumnIndex(AField);
    { Query chrome carries the canonical field index. The resize attribute is
      deliberately only a marker; its exact handle belongs to this same TH. }
    LSort := TJSHTMLElement(LTable.querySelector('[data-nyx-query-sort="' +
      IntToStr(LCanonical) + '"]'));
    Result := nil;

    if LSort <> nil then
    begin
      Result := TJSHTMLElement(LSort.closest('th'));
    end;
    LGrip := nil;

    if (Result <> nil) and (Result.closest('table') = LTable) then
    begin
      LGrip := TJSHTMLElement(Result.querySelector('[data-nyx-column-resize]'));
    end;
    Check((Result <> nil) and (LGrip <> nil) and
      (LGrip.closest('th') = Result),
      'typed field owns its actual browser header and grip');
  end;

  procedure FocusResizeGrip;
  begin
    LGrip := TJSHTMLElement(HeaderFor(NyxColumnIdentity(PersonName)).querySelector(
      '[data-nyx-column-resize]'));
    Check((LGrip <> nil) and (LGrip.getAttribute('role') = 'separator'),
      'Name resize keys target the actual owned separator');
    LGrip.focus;
    Check(Web.document.activeElement = LGrip,
      'resize keyboard input begins on its real focused separator');
  end;

  procedure ResizeKey(const AKey: String; AShift: Boolean = False);
  var
    LKeyOptions: TJSObject;
  begin
    { These ordinary DOM events exercise the owned key route; they establish
      neither trusted pointer capture nor physical keyboard/AT delivery. }
    LKeyOptions := TJSObject.new;
    LKeyOptions['key'] := AKey;
    LKeyOptions['shiftKey'] := AShift;
    LKeyOptions['bubbles'] := True;
    LKeyOptions['cancelable'] := True;
    LGrip.dispatchEvent(TReviewKey.new('keydown', LKeyOptions));
    LGrip.dispatchEvent(TReviewKey.new('keyup', LKeyOptions));
  end;
  {$else}
  function NativeFieldGrid(const AField: TNyxColumnIdentity;
    out APhysical: Integer): TStringGrid;
  var
    LTitle: TNyxText;
    LChild: Integer;
    LCandidate: TStringGrid;

    function FindIn(AGrid: TStringGrid): Boolean;
    var
      LColumn: Integer;
    begin
      Result := False;
      for LColumn := 0 to AGrid.ColCount - 1 do
      begin

        if Pos(LTitle, TNyxText(AGrid.Cells[LColumn, 0])) = 1 then
        begin
          APhysical := LColumn;
          Exit(True);
        end;
      end;
    end;

  begin
    LTitle := LObservation.Spec.ColumnAt(LObservation.Spec.ColumnIndex(AField)).Title;
    APhysical := -1;
    Result := LGrid;

    if FindIn(LGrid) then
    begin
      Exit;
    end;
    for LChild := 0 to LGrid.ControlCount - 1 do
    begin

      if (LGrid.Controls[LChild] is TStringGrid) and LGrid.Controls[LChild].Visible then
      begin
        LCandidate := TStringGrid(LGrid.Controls[LChild]);

        if FindIn(LCandidate) then
        begin
          Exit(LCandidate);
        end;
      end;
    end;
    Result := nil;
  end;

  procedure DragNativeHeader(ACancel: Boolean; out AExpectedWidth: Integer);
  var
    LNative: TStringGrid;
    LColumn: Integer;
    LBounds: Types.TRect;
    LNearPoint: Types.TPoint;
    LFarPoint: Types.TPoint;
    LHitPoint: Types.TPoint;
    LMovePoint: Types.TPoint;
    LScreenPoint: Types.TPoint;
    LFormerCursor: Types.TPoint;
    LActualCursor: Types.TPoint;
    LPPI: Integer;
    LPixels: Integer;
    LDelta: Integer;

    procedure PlacePointer(const AClient: Types.TPoint);
    begin
      LScreenPoint := LNative.ClientToScreen(AClient);
      Check(Windows.SetCursorPos(LScreenPoint.X, LScreenPoint.Y) and
        Windows.GetCursorPos(Windows.POINT(LActualCursor)) and
        (LActualCursor.X = LScreenPoint.X) and (LActualCursor.Y = LScreenPoint.Y),
        'native resize pointer and client messages describe the same actual hit');
    end;

  begin
    LNative := NativeFieldGrid(NyxColumnIdentity(PersonName), LColumn);
    Check((LNative <> nil) and (LColumn >= 0),
      'Name resize retains its actual typed native header surface');
    LPPI := LNative.Font.PixelsPerInch;
    Check(LPPI > 0, 'native resize reads the actual target font PPI');
    LPixels := LNative.ColWidths[LColumn];
    LDelta := Round(40 * LPPI / 96);
    LBounds := TNyxCollectionStringGrid(LNative).ColumnHeaderRect(LColumn);
    LNearPoint := Types.Point(LBounds.Right - 1, LBounds.Top + 4);
    LFarPoint := Types.Point(LBounds.Right - 1, LBounds.Bottom - 5);

    if LNative.UseRightToLeftAlignment then
    begin
      LNearPoint.X := LBounds.Left;
      LFarPoint.X := LBounds.Left;
      LDelta := -LDelta;
    end;
    Check((LFarPoint.Y - LNearPoint.Y > Windows.GetSystemMetrics(SM_CYDOUBLECLK)) and
      (LFarPoint.Y - LNearPoint.Y > 3) and
      (LNearPoint.Y >= LBounds.Top) and (LFarPoint.Y < LBounds.Bottom),
      'independent native header drags have valid hits beyond both double-click bounds');
    LHitPoint := LNearPoint;

    if LResizeFar then
    begin
      LHitPoint := LFarPoint;
    end;
    LResizeFar := not LResizeFar;
    LMovePoint := Types.Point(LHitPoint.X + LDelta, LHitPoint.Y);
    { Installed LCL StartColSizing deliberately uses DeltaOff=0. Derive the
      release width from its actual captured header boundary, including the
      one-pixel inset of the starting hit, rather than adding an assumed delta
      to the original width. Convert that physical result exactly once. }

    if LNative.UseRightToLeftAlignment then
    begin
      AExpectedWidth := Round((LBounds.Right - LMovePoint.X) * 96 / LPPI);
    end
    else
    begin
      AExpectedWidth := Round((LMovePoint.X - LBounds.Left) * 96 / LPPI);
    end;
    Check((Abs(LDelta) > 0) and (AExpectedWidth > 160) and (AExpectedWidth <= 300) and
      (LPixels = LBounds.Right - LBounds.Left),
      'fresh drag has a bounded width after one physical-to-logical conversion');
    Check(Windows.GetCursorPos(Windows.POINT(LFormerCursor)),
      'native resize retains the caller cursor before entering real grid input');
    try

      if Windows.GetForegroundWindow <> FApp.Window.Handle then
      begin
        FApp.Window.BringToFront;
        Windows.SetForegroundWindow(FApp.Window.Handle);
      end;
      Check(Windows.GetForegroundWindow = FApp.Window.Handle,
        'native resize activates its actual application window');
      PlacePointer(LHitPoint);
      { Hover owns LCL's actual header-edge cursor/hit cache. A paired WM path
        then enters native sizing; no direct header callback or MouseUp replay
        substitutes for the original release after Escape. }
      SendMessage(LNative.Handle, WM_MOUSEMOVE, 0,
        LHitPoint.X or (LHitPoint.Y shl 16));
      SendMessage(LNative.Handle, WM_LBUTTONDOWN, MK_LBUTTON,
        LHitPoint.X or (LHitPoint.Y shl 16));
      PlacePointer(LMovePoint);
      SendMessage(LNative.Handle, WM_MOUSEMOVE, MK_LBUTTON,
        LMovePoint.X or (LMovePoint.Y shl 16));

      if ACancel then
      begin
        SendMessage(LNative.Handle, WM_KEYDOWN, VK_ESCAPE, 0);
        SendMessage(LNative.Handle, WM_KEYUP, VK_ESCAPE, 0);
      end;
      SendMessage(LNative.Handle, WM_LBUTTONUP, 0,
        LMovePoint.X or (LMovePoint.Y shl 16));
    finally
      Windows.SetCursorPos(LFormerCursor.X, LFormerCursor.Y);
    end;
    Pump;
  end;
  {$endif}

  procedure PaintedWidth(const AField: TNyxColumnIdentity; AWidth: Integer);
  {$ifdef PAS2JS}
  var
    LHeader: TJSHTMLElement;
  {$else}
  var
    LNative: TStringGrid;
    LColumn: Integer;
    LPPI: Integer;
  {$endif}
  begin
    {$ifdef PAS2JS}
    LHeader := HeaderFor(AField);
    Check(Abs(LHeader.getBoundingClientRect.width - AWidth) <= 2,
      'Set width changes the actual browser column in logical CSS pixels');
    {$else}
    LNative := NativeFieldGrid(AField, LColumn);
    Check(LNative <> nil, 'typed field retains a real native paint surface');
    LPPI := LNative.Font.PixelsPerInch;
    Check((LPPI > 0) and
      (Abs(LNative.ColWidths[LColumn] * 96 - AWidth * LPPI) <= LPPI),
      'Set width changes the actual native column with one font-PPI conversion');
    {$endif}
  end;

  procedure PaintedOrder(AFirst, ASecond, AThird: Integer);
  {$ifdef PAS2JS}
  var
    LRow: TJSHTMLElement;
    LHeader: TJSHTMLElement;
    LSort: TJSHTMLElement;
    LGrip: TJSHTMLElement;
    LPosition: Integer;
    LExpected: Integer;
  {$endif}
  begin
    Check((LObservation.OrderedAt(0) = AFirst) and
      (LObservation.OrderedAt(1) = ASecond) and (LObservation.OrderedAt(2) = AThird),
      'manager movement changes complete canonical order including typed identities');
    {$ifdef PAS2JS}
    LRow := TJSHTMLElement(HeaderFor(NyxColumnIdentity(PersonName)).parentElement);
    Check(LRow.children.length = 3, 'actual browser header keeps all three authored fields');
    for LPosition := 0 to 2 do
    begin
      LExpected := AFirst;

      if LPosition = 1 then
      begin
        LExpected := ASecond;
      end
      else if LPosition = 2 then
      begin
        LExpected := AThird;
      end;
      LHeader := TJSHTMLElement(LRow.children[LPosition]);
      LSort := TJSHTMLElement(LHeader.querySelector('[data-nyx-query-sort]'));
      LGrip := TJSHTMLElement(LHeader.querySelector('[data-nyx-column-resize]'));
      Check((LGrip <> nil) and (LSort <> nil) and
        (LSort.getAttribute('data-nyx-query-sort') = IntToStr(LExpected)),
        'real browser header order retains exact canonical field mapping');
    end;
    {$else}
    Check((Pos(LObservation.Spec.ColumnAt(AFirst).Title, TNyxText(LGrid.Cells[0, 0])) = 1) and
      (Pos(LObservation.Spec.ColumnAt(ASecond).Title, TNyxText(LGrid.Cells[1, 0])) = 1) and
      (Pos(LObservation.Spec.ColumnAt(AThird).Title, TNyxText(LGrid.Cells[2, 0])) = 1),
      'real native middle header order matches the complete canonical order');
    {$endif}
  end;

  procedure PaintedPin(const AField: TNyxColumnIdentity; APin: TNyxColumnPin);
  {$ifdef PAS2JS}
  var
    LHeader: TJSHTMLElement;
  {$else}
  var
    LNative: TStringGrid;
    LColumn: Integer;
  {$endif}
  begin
    {$ifdef PAS2JS}
    LHeader := HeaderFor(AField);

    if APin = ncpNone then
    begin
      Check(LHeader.style.getPropertyValue('position') <> 'sticky',
        'Unpinned removes sticky authority from the actual browser header');
    end
    else
    begin
      Check(LHeader.style.getPropertyValue('position') = 'sticky',
        'Set pinned edge makes the actual browser field sticky');

      if APin = ncpStart then
      begin
        Check(LHeader.style.getPropertyValue('inset-inline-start') <> '',
          'start pin uses the logical start edge');
      end
      else
      begin
        Check(LHeader.style.getPropertyValue('inset-inline-end') <> '',
          'end pin uses the logical end edge');
      end;
    end;
    {$else}
    LNative := NativeFieldGrid(AField, LColumn);
    Check(LNative <> nil, 'pin retains the actual typed native field');

    if APin = ncpNone then
    begin
      Check(LNative = LGrid, 'Unpinned returns the native field to its same outer grid');
    end
    else
    begin
      Check((LNative <> LGrid) and (LNative.Parent = LGrid),
        'Set pinned edge uses a borrowed child pane under the same outer grid');

      if APin = ncpStart then
      begin
        Check(LNative.Left = 0, 'native start pane is placed at the physical LTR start edge');
      end
      else
      begin
        Check(Abs(LNative.Left + LNative.Width - LGrid.ClientWidth) <= 1,
          'native end pane respects the outer client and scrollbar edge');
      end;
    end;
    {$endif}
  end;

  procedure Launch;
  {$ifndef PAS2JS}
  var
    LNearPoint: Types.TPoint;
    LFarPoint: Types.TPoint;
    LHitPoint: Types.TPoint;
    LScreenPoint: Types.TPoint;
    LFormerCursor: Types.TPoint;
    LActualCursor: Types.TPoint;
    LDoubleWidth: Integer;
    LDoubleHeight: Integer;
  {$endif}
  begin
    {$ifdef PAS2JS}
    LButton := TJSHTMLElement(LTable.querySelector('[data-nyx-columns-button]'));
    Check((LButton <> nil) and not LButton.hasAttribute('disabled'),
      'browser column launcher remains available independently of visible fields');
    LOptions := TJSObject.new;
    LOptions['bubbles'] := True;
    LOptions['cancelable'] := True;
    Check(not LButton.dispatchEvent(TReviewMouse.new('pointerdown', LOptions)),
      'manager pointer entry defers premature browser focus before draft capture');
    LButton.click;
    {$else}
    LButton := TButton(LGrid.FindComponent('NyxColumnManagerButton'));
    Check((LButton <> nil) and LButton.Visible and LButton.Enabled,
      'native column launcher remains available independently of visible fields');
    Check(Windows.GetCursorPos(Windows.POINT(LFormerCursor)),
      'native launcher captures the caller cursor before physical input');
    try

      if Windows.GetForegroundWindow <> FApp.Window.Handle then
      begin
        FApp.Window.BringToFront;
        Windows.SetForegroundWindow(FApp.Window.Handle);
      end;
      Check(Windows.GetForegroundWindow = FApp.Window.Handle,
        'native launcher activates its actual application window');
      LNearPoint := Types.Point(4, 4);
      LFarPoint := Types.Point(LButton.ClientWidth - 5, LButton.ClientHeight - 5);
      LDoubleWidth := Windows.GetSystemMetrics(SM_CXDOUBLECLK);
      LDoubleHeight := Windows.GetSystemMetrics(SM_CYDOUBLECLK);
      Check((LButton.ClientWidth > 9) and (LButton.ClientHeight > 9) and
        (((LFarPoint.X - LNearPoint.X > LDoubleWidth) and
          (LFarPoint.X - LNearPoint.X > 3)) or
        ((LFarPoint.Y - LNearPoint.Y > LDoubleHeight) and
          (LFarPoint.Y - LNearPoint.Y > 3))),
        'native launcher has distinct valid single-click hit positions');
      LHitPoint := LNearPoint;

      if LLaunchFar then
      begin
        LHitPoint := LFarPoint;
      end;
      LLaunchFar := not LLaunchFar;
      LScreenPoint := LButton.ClientToScreen(LHitPoint);
      Check(Windows.SetCursorPos(LScreenPoint.X, LScreenPoint.Y) and
        Windows.GetCursorPos(Windows.POINT(LActualCursor)) and
        (LActualCursor.X = LScreenPoint.X) and (LActualCursor.Y = LScreenPoint.Y),
        'native launcher pointer and client message coordinates describe the same hit');
      { Win32/LCL classifies multi-clicks from GetCursorPos, not LPARAM alone.
        Independent launches alternate actual hit points beyond both the system
        double-click rectangle and LCL's three-pixel threshold. Preserve the
        real down/up path without timers, toolkit-state resets or direct Click.
        This input harness does not claim physical hardware coverage. }
      SendMessage(LButton.Handle, WM_MOUSEMOVE, 0,
        LHitPoint.X or (LHitPoint.Y shl 16));
      SendMessage(LButton.Handle, WM_LBUTTONDOWN, MK_LBUTTON,
        LHitPoint.X or (LHitPoint.Y shl 16));
      SendMessage(LButton.Handle, WM_LBUTTONUP, 0,
        LHitPoint.X or (LHitPoint.Y shl 16));
    finally
      Windows.SetCursorPos(LFormerCursor.X, LFormerCursor.Y);
    end;
    {$endif}
    Pump;
  end;

  {$ifndef PAS2JS}
  function NativeEditDetails: TNyxText;
  var
    LCurrentData: INyxCollectionSnapshot;
  begin
    Result := 'mode=' + TNyxText(BoolToStr(LGrid.EditorMode, True)) +
      '; col=' + TNyxText(IntToStr(LGrid.Col)) +
      '; row=' + TNyxText(IntToStr(LGrid.Row));

    if LGrid.Editor = nil then
    begin
      Result := Result + '; editorClass=<nil>';
    end
    else
    begin
      Result := Result + '; editorClass=' + TNyxText(LGrid.Editor.ClassName);
    end;

    if LGrid.Editor is TCustomEdit then
    begin
      Result := Result + '; text="' + TNyxText(TCustomEdit(LGrid.Editor).Text) + '"';
    end;
    LCurrentData := FViews[0].Snapshot;
    Result := Result + '; intendedItem="' + LItem.ID + '"';

    if (LGrid.Row > 0) and (LGrid.Row <= LCurrentData.Count) then
    begin
      Result := Result + '; currentItem="' +
        LCurrentData.ItemAt(LGrid.Row - 1).Ref.ID + '"';
    end
    else
    begin
      Result := Result + '; currentItem=<none>';
    end;
    Result := Result + '; mountError="' + LMount.ErrorText + '"';
  end;

  procedure SaveManagerImage;
  var
    LImage: Graphics.TBitmap;
    LPng: Graphics.TPortableNetworkGraphic;
    LPath: String;
  begin

    if GEvidence = '' then
    begin
      Exit;
    end;
    LPath := IncludeTrailingPathDelimiter(GEvidence) +
      'columns-manager-' + IntToStr(GWidth) + '.png';

    if FileExists(LPath) then
    begin
      Exit;
    end;
    ForceDirectories(GEvidence);
    { GetFormImage captures the actual drawn LCL window/controls. No document
      reconstruction, substitute renderer or synthetic evidence image is used. }
    LTarget.Presentation.Window.Update;
    LImage := LTarget.Presentation.Window.GetFormImage;
    try
      LPng := Graphics.TPortableNetworkGraphic.Create;
      try
        LPng.Assign(LImage);
        LPng.SaveToFile(LPath);
      finally
        LPng.Free;
      end;
    finally
      LImage.Free;
    end;
  end;
  {$endif}

begin
  LMount := FApp.View.CollectionMount(CArrangementFirst);
  Check(Supports(LMount, INyxCollectionColumnArrangementMount, LManagerMount),
    'arranged table exposes its optional typed manager mount');
  LManager := LManagerMount.ColumnArrangement;
  Check((LManager <> nil) and LManager.Connected,
    'renderer configures the actual view-owned column manager');
  LData := FViews[0].Store.Snapshot;
  LItem := FViews[0].Snapshot.ItemAt(0).Ref;
  LPeer := FColumns[1].CaptureColumns.Preferences.ToData.ToJSON;
  LQuery := FViews[0].QueryPolicy.ToData.ToJSON;
  LDraft := TNyxText('Unfinished manager cell / 🌙');
  FApp.View.Reveal(CArrangementFirst, niRuntime);
  {$ifdef PAS2JS}
  LTable := FApp.View.ElementFor(CArrangementFirst);
  LCell := TJSHTMLElement(LTable.querySelector('tr[data-nyx-item="' + LItem.ID +
    '"] td[data-nyx-column="0"]'));
  Check(LCell <> nil, 'manager draft starts in the actual original Name cell');
  LCell.focus;
  LOptions := TJSObject.new;
  LOptions['key'] := 'F2';
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LCell.dispatchEvent(TReviewKey.new('keydown', LOptions));
  await(ArrangementFrame);
  LEditor := TJSHTMLInputElement(LCell.querySelector('input.nyx-cell-editor'));
  Check((LEditor <> nil) and (Web.document.activeElement = LEditor),
    'manager browser launch begins with an actual focused cell editor');
  LEditor.value := LDraft;
  LEditor.setSelectionRange(2, 7);
  {$else}
  LLaunchFar := False;
  LResizeFar := False;
  LGrid := TStringGrid(FApp.View.ControlFor(CArrangementFirst));
  LGrid.Col := 0;
  LGrid.Row := 1;
  LGrid.SetFocus;
  SendMessage(LGrid.Handle, WM_KEYDOWN, VK_F2, 0);
  Pump;
  Check(LGrid.EditorMode and (LGrid.Editor is TCustomEdit),
    'manager native launch begins with an actual cell editor');
  LEditor := TCustomEdit(LGrid.Editor);
  LEditor.Text := LDraft;
  LEditor.SelStart := 2;
  LEditor.SelLength := 5;
  {$endif}
  Launch;
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check(FViews[0].Store.Snapshot = LData,
    'physical manager launch does not commit unfinished cell text');
  {$ifdef PAS2JS}
  Check(Web.document.activeElement = LFormRenderer.ElementFor(NyxColumnArrangementCloseID),
    'manager initial focus is the actual harmless Close button');
  {$else}
  Check(TWinControl(LFormRenderer.ControlFor(NyxColumnArrangementCloseID)).Focused,
    'manager initial native focus is the actual harmless Close button');
  {$endif}
  LWidthID := NyxColumnArrangementFieldID(NyxColumnArrangementEditorID,
    LObservation, NyxColumnIdentity(Score), ncafWidth);
  LFormRenderer.Reveal(LWidthID, niRuntime);
  {$ifdef PAS2JS}
  LWidth := TJSHTMLInputElement(LFormRenderer.InputFor(LWidthID));
  LWidth.value := '-';
  LWidth.dispatchEvent(TJSEvent.new('input'));
  LWidth.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LWidth := TEdit(LFormRenderer.InputFor(LWidthID));
  LWidth.Text := '-';
  Check(Assigned(LWidth.OnChange), 'manager width has a live text binding');
  LWidth.OnChange(LWidth);
  SaveManagerImage;
  {$endif}
  LBeforeSerial := FColumns[0].CaptureColumns.Serial;
  LGeneration := NyxEventGeneration(LPresenter.Events);
  ClickAction(NyxColumnArrangementCloseID);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Check(not LPresenter.IsOpen and LGeneration.Cancelled and
    (FColumns[0].CaptureColumns.Serial = LBeforeSerial),
    'actual manager Close retires its open generation without changing columns');
  Launch;
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Check(LManager.Presenter = LPresenter,
    'actual Close and Columns reopen retain the same real form owner');
  Resolve;
  {$ifdef PAS2JS}
  Check(TJSHTMLInputElement(LFormRenderer.InputFor(LWidthID)).value = '-',
    'actual Close and reopen preserve the unfinished browser width draft');
  {$else}
  Check(TNyxText(TEdit(LFormRenderer.InputFor(LWidthID)).Text) = '-',
    'actual Close and reopen preserve the unfinished native width draft');
  {$endif}
  FieldAction(NyxColumnIdentity(Active), ncaaHide);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check(LObservation.VisibleCount = 2, 'real manager action hides the intended typed field');
  LWidthID := NyxColumnArrangementFieldID(NyxColumnArrangementEditorID,
    LObservation, NyxColumnIdentity(Score), ncafWidth);
  {$ifdef PAS2JS}
  Check(TJSHTMLInputElement(LFormRenderer.InputFor(LWidthID)).value = '-',
    'unrelated hide preserves unfinished browser manager width');
  {$else}
  Check(TNyxText(TEdit(LFormRenderer.InputFor(LWidthID)).Text) = '-',
    'unrelated hide preserves unfinished native manager width');
  {$endif}
  FieldAction(NyxColumnIdentity(Active), ncaaShow);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  FieldAction(NyxColumnIdentity(PersonName), ncaaHide);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  FieldAction(NyxColumnIdentity(Score), ncaaHide);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  FieldAction(NyxColumnIdentity(Active), ncaaHide);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check(LObservation.VisibleCount = 0, 'real manager can hide every permitted column');
  Check(Supports(LMount, INyxCollectionEditMount, LEditMount),
    'all-hidden table still exposes its typed draft handoff');
  LEdits := LEditMount.SuspendCellEdits;
  try
    LFound := False;
    for LIndex := 0 to LEdits.Count - 1 do
    begin
      LEdit := LEdits.CellAt(LIndex);

      if LEdit.Field.Same(NyxColumnIdentity(PersonName)) and
        (LEdit.Item.ID = LItem.ID) and (LEdit.Text = LDraft) then
      begin
        LFound := True;
        Check(not LEdit.Editing and not LEdit.Focused,
          'hidden unfinished text is dormant and carries no editor or focus authority');
      end;
    end;
    Check(LFound, 'all-hidden handoff retains exact row and typed-field unfinished text');
  finally
    LEditMount.ResumeCellEdits(LEdits, False);
  end;
  LEdits := nil;
  LGeneration := NyxEventGeneration(LPresenter.Events);
  LPresentation.Close;
  Check(LGeneration.Cancelled, 'raw actual manager Close revokes its captured form generation');
  Launch;
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Check(LManager.Presenter = LPresenter,
    'all-hidden recovery reopens the retained actual manager and its drafts');
  Resolve;
  FieldAction(NyxColumnIdentity(PersonName), ncaaShow);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check(LObservation.VisibleCount = 1, 'all-hidden recovery shows a selected field through real controls');
  {$ifdef PAS2JS}
  LCell := TJSHTMLElement(LTable.querySelector('tr[data-nyx-item="' + LItem.ID +
    '"] td[data-nyx-column="0"]'));
  LEditor := TJSHTMLInputElement(LCell.querySelector('input.nyx-cell-editor'));
  Check((LEditor <> nil) and LEditor.hasAttribute('hidden') and
    (Web.document.activeElement <> LEditor),
    'revealing a dormant browser draft does not enter editing or steal focus');
  {$else}
  Check(not LGrid.EditorMode,
    'revealing a dormant native draft does not enter editing or steal focus');
  {$endif}
  ClickAction(NyxColumnArrangementActionID(NyxColumnArrangementEditorID, ncaaResetAll));
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check(LObservation.VisibleCount = 3, 'manager reset restores authored visibility');
  { External runtime changes intentionally leave the form's OLD baseline in
    place. Its next action must refuse; only deliberate Reload adopts current
    preferences. Retain this exact form observation for the stale action ID. }
  LWidthID := NyxColumnArrangementFieldID(NyxColumnArrangementEditorID,
    LObservation, NyxColumnIdentity(Score), ncafWidth);
  LFormRenderer.Reveal(LWidthID, niRuntime);
  {$ifdef PAS2JS}
  LWidth := TJSHTMLInputElement(LFormRenderer.InputFor(LWidthID));
  LWidth.value := '-';
  LWidth.dispatchEvent(TJSEvent.new('input'));
  LWidth.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LWidth := TEdit(LFormRenderer.InputFor(LWidthID));
  LWidth.Text := '-';
  Check(Assigned(LWidth.OnChange), 'stale manager keeps its real numeric input binding');
  LWidth.OnChange(LWidth);
  {$endif}
  FColumns[0].ConfigureColumns(NyxColumnPreferences.WithColumn(
    NyxColumnPreference(NyxColumnIdentity(PersonName)).Width(200)));
  LExternal := FColumns[0].CaptureColumns.Preferences.ToData.ToJSON;
  ClickAction(NyxColumnArrangementCloseID);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Check(not LPresenter.IsOpen, 'real Close dismisses the retained stale manager');
  Launch;
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check(LPresentation.State.GetValue(NyxTextState(NyxColumnArrangementStatusState)) <> '',
    'reopening stale manager immediately explains its retained baseline before an action');
  {$ifdef PAS2JS}
  Check(TJSHTMLInputElement(LFormRenderer.InputFor(LWidthID)).value = '-',
    'stale manager explanation preserves exact unfinished browser width text');
  {$else}
  Check(TNyxText(TEdit(LFormRenderer.InputFor(LWidthID)).Text) = '-',
    'stale manager explanation preserves exact unfinished native width text');
  {$endif}
  FieldAction(NyxColumnIdentity(Active), ncaaHide);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Check((FColumns[0].CaptureColumns.Preferences.ToData.ToJSON = LExternal) and
    (LPresentation.State.GetValue(NyxTextState(NyxColumnArrangementStatusState)) <> ''),
    'stale manager action refuses without rewriting newer preferences and explains reload');
  ClickAction(NyxColumnArrangementReloadID);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  FieldAction(NyxColumnIdentity(Active), ncaaHide);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check(LObservation.VisibleCount = 2, 'explicit reload admits a fresh manager action');
  ClickAction(NyxColumnArrangementActionID(NyxColumnArrangementEditorID, ncaaResetAll));
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  { The SAME real manager now exercises every remaining authored action through
    its ordinary bound fields/buttons. No ConfigureColumns shortcut substitutes
    for Set width, Fit, order or pinning in this physical-control journey. }
  LBeforeSerial := LObservation.Serial;
  EnterWidth(NyxColumnIdentity(Score), 132);
  FieldAction(NyxColumnIdentity(Score), ncaaApplyWidth);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  LPreference := Preference(NyxColumnIdentity(Score));
  Check(LPreference.HasWidth and (LPreference.WidthValue = 132) and
    (LObservation.Serial = LBeforeSerial + 1),
    'real Set width installs one exact logical-pixel preference publication');
  PaintedWidth(NyxColumnIdentity(Score), 132);
  LBeforeSerial := LObservation.Serial;
  FieldAction(NyxColumnIdentity(Score), ncaaDefaultWidth);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check(not LObservation.ColumnAt(1).HasWidth and
    (LObservation.Serial = LBeforeSerial + 1),
    'Restore default width removes the override in one publication');
  PaintedWidth(NyxColumnIdentity(Score), 90);

  LBeforeSerial := LObservation.Serial;
  FieldAction(NyxColumnIdentity(PersonName), ncaaFitWidth);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  LPreference := Preference(NyxColumnIdentity(PersonName));
  Check(LPreference.HasWidth and (LPreference.WidthValue >= 50) and
    (LPreference.WidthValue <= 300) and (LObservation.Serial = LBeforeSerial + 1),
    'real Fit current page measures accepted text within authored width bounds once');
  PaintedWidth(NyxColumnIdentity(PersonName), LPreference.WidthValue);
  Check(FViews[0].Store.Snapshot = LData,
    'manager Fit never publishes the unrelated unfinished Name cell text');
  FieldAction(NyxColumnIdentity(PersonName), ncaaResetColumn);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  PaintedWidth(NyxColumnIdentity(PersonName), 160);

  LBeforeSerial := LObservation.Serial;
  FieldAction(NyxColumnIdentity(Score), ncaaMoveEarlier);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check(LObservation.Serial = LBeforeSerial + 1, 'Move earlier publishes its one original order intent');
  PaintedOrder(1, 0, 2);
  LBeforeSerial := LObservation.Serial;
  FieldAction(NyxColumnIdentity(Score), ncaaMoveLater);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check(LObservation.Serial = LBeforeSerial + 1, 'Move later publishes its one original order intent');
  PaintedOrder(0, 1, 2);

  LBeforeSerial := LObservation.Serial;
  EnterPin(NyxColumnIdentity(Score), 'Start edge');
  FieldAction(NyxColumnIdentity(Score), ncaaApplyPin);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  LPreference := Preference(NyxColumnIdentity(Score));
  Check(LPreference.HasPin and (LPreference.Pin = ncpStart) and
    (LObservation.Serial = LBeforeSerial + 1), 'real Set pinned edge installs start pin once');
  PaintedPin(NyxColumnIdentity(Score), ncpStart);
  LBeforeSerial := LObservation.Serial;
  EnterPin(NyxColumnIdentity(Score), 'End edge');
  FieldAction(NyxColumnIdentity(Score), ncaaApplyPin);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  LPreference := Preference(NyxColumnIdentity(Score));
  Check(LPreference.HasPin and (LPreference.Pin = ncpEnd) and
    (LObservation.Serial = LBeforeSerial + 1), 'real Set pinned edge installs end pin once');
  PaintedPin(NyxColumnIdentity(Score), ncpEnd);
  LBeforeSerial := LObservation.Serial;
  EnterPin(NyxColumnIdentity(Score), 'Unpinned');
  FieldAction(NyxColumnIdentity(Score), ncaaApplyPin);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  LPreference := Preference(NyxColumnIdentity(Score));
  Check(LPreference.HasPin and (LPreference.Pin = ncpNone) and
    (LObservation.Serial = LBeforeSerial + 1), 'real Unpinned command withdraws the effective edge once');
  PaintedPin(NyxColumnIdentity(Score), ncpNone);

  { Exercise BOTH editable logical edges around an actually moving middle on
    this SAME table. Its three bounded columns do not overflow a desktop host,
    so temporarily constrain only the existing runtime allocation. Restore its
    exact target geometry afterwards; authored nodes, stores and owners stay
    unchanged, and the compact/wide runs both require physical displacement. }
  EnterWidth(NyxColumnIdentity(PersonName), 160);
  FieldAction(NyxColumnIdentity(PersonName), ncaaApplyWidth);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  EnterWidth(NyxColumnIdentity(Score), 90);
  FieldAction(NyxColumnIdentity(Score), ncaaApplyWidth);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  EnterWidth(NyxColumnIdentity(Active), 140);
  FieldAction(NyxColumnIdentity(Active), ncaaApplyWidth);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  EnterPin(NyxColumnIdentity(PersonName), 'Start edge');
  FieldAction(NyxColumnIdentity(PersonName), ncaaApplyPin);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  EnterPin(NyxColumnIdentity(Score), 'End edge');
  FieldAction(NyxColumnIdentity(Score), ncaaApplyPin);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  ClickAction(NyxColumnArrangementCloseID);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Check(not LPresenter.IsOpen, 'middle scrolling starts on the actual table after real Close');
  LBeforeSerial := FColumns[0].CaptureColumns.Serial;
  LInputPreferences := FColumns[0].CaptureColumns.Preferences.ToData.ToJSON;
  {$ifdef PAS2JS}
  LScrollOwner := ScrollSurface(LTable);
  Check(LScrollOwner <> nil, 'actual table retains its existing horizontal scroll owner');
  LScrollStyle := LScrollOwner.style.cssText;
  LScrollLeft := LScrollOwner.scrollLeft;
  try
    ConstrainScrollAllocation;
    await(ArrangementFrame);
    Check((LScrollOwner.clientWidth >= 346) and (LScrollOwner.clientWidth <= 364) and
      (LScrollOwner.scrollWidth > LScrollOwner.clientWidth + 8),
      'existing browser clip has real horizontal overflow while both pin budgets fit' +
      ' / clip=' + IntToStr(LScrollOwner.clientWidth) +
      ' / content=' + IntToStr(LScrollOwner.scrollWidth) +
      ' / table=' + IntToStr(LTable.clientWidth) +
      ' / parent=' + LScrollOwner.tagName +
      ' / suspended=' + LTable.getAttribute('data-nyx-pins-suspended'));
    Check(LTable.getAttribute('data-nyx-pins-suspended') = 'false',
      'both browser pin edges stay effective during genuine middle overflow');
    LScrollOwner.scrollLeft := 0;
    LScrollOwner.dispatchEvent(TJSEvent.new('scroll'));
    await(ArrangementFrame);
    for LIndex := 0 to 2 do
    begin
      LScrollHeaders[LIndex] := HeaderFor(LObservation.Spec.ColumnAt(LIndex).Identity);
      LScrollCells[LIndex] := TJSHTMLElement(LTable.querySelector(
        'tr[data-nyx-item="' + LItem.ID + '"] td[data-nyx-column="' + IntToStr(LIndex) + '"]'));
      Check(LScrollCells[LIndex] <> nil, 'scroll geometry uses the exact current-page body cell');
      LHeaderBounds[LIndex] := LScrollHeaders[LIndex].getBoundingClientRect;
      LCellBounds[LIndex] := LScrollCells[LIndex].getBoundingClientRect;
    end;
    LScrollStart := LScrollOwner.scrollLeft;
    LScrollOwner.scrollLeft := LScrollOwner.scrollWidth - LScrollOwner.clientWidth;
    LScrollOwner.dispatchEvent(TJSEvent.new('scroll'));
    await(ArrangementFrame);
    Check((LScrollOwner.scrollLeft > LScrollStart + 8) and
      (LScrollHeaders[2].getBoundingClientRect.left < LHeaderBounds[2].left - 8) and
      (LScrollCells[2].getBoundingClientRect.left < LCellBounds[2].left - 8),
      'real browser scroll moves both the middle header and its current-page cell');
    for LIndex := 0 to 1 do
    begin
      Check((Abs(LScrollHeaders[LIndex].getBoundingClientRect.left - LHeaderBounds[LIndex].left) <= 1) and
        (Abs(LScrollHeaders[LIndex].getBoundingClientRect.right - LHeaderBounds[LIndex].right) <= 1) and
        (Abs(LScrollCells[LIndex].getBoundingClientRect.left - LCellBounds[LIndex].left) <= 1) and
        (Abs(LScrollCells[LIndex].getBoundingClientRect.right - LCellBounds[LIndex].right) <= 1),
        'both editable browser edge headers and body cells remain physically fixed while middle scrolls');
    end;
  {$else}
  LScrollAlign := LGrid.Align;
  LScrollAnchors := LGrid.Anchors;
  LScrollBounds := LGrid.BoundsRect;
  LScrollLeft := LGrid.LeftCol;
  LScrollPPI := LGrid.Font.PixelsPerInch;
  Check(LScrollPPI > 0, 'native scroll allocation has an actual font-PPI scale');
  try
    ConstrainScrollAllocation;
    Pump;
    Check((LGrid.ClientWidth * 96 >= 346 * LScrollPPI) and
      (LGrid.ClientWidth * 96 <= 364 * LScrollPPI),
      'existing native clip keeps both pin budgets while constraining its middle / client ' +
        TNyxText(IntToStr(LGrid.ClientWidth)) + ' outer ' + TNyxText(IntToStr(LGrid.Width)) +
        ' PPI ' + TNyxText(IntToStr(LScrollPPI)));
    PaintedPin(NyxColumnIdentity(PersonName), ncpStart);
    PaintedPin(NyxColumnIdentity(Score), ncpEnd);
    LGrid.LeftCol := LGrid.FixedCols;
    Pump;
    for LIndex := 0 to 2 do
    begin
      LScrollFaces[LIndex] := NativeFieldGrid(LObservation.Spec.ColumnAt(LIndex).Identity,
        LScrollColumns[LIndex]);
      Check(LScrollFaces[LIndex] <> nil, 'native scroll geometry resolves the exact typed field');
      LScrollRect := TNyxCollectionStringGrid(LScrollFaces[LIndex]).ColumnHeaderRect(LScrollColumns[LIndex]);
      LHeaderBounds[LIndex] := LScrollFaces[LIndex].ClientToScreen(
        Types.Point(LScrollRect.Left, LScrollRect.Top));
      LScrollRect := LScrollFaces[LIndex].CellRect(LScrollColumns[LIndex], 1);
      LCellBounds[LIndex] := LScrollFaces[LIndex].ClientToScreen(
        Types.Point(LScrollRect.Left, LScrollRect.Top));
    end;
    Check((LScrollFaces[0] <> LGrid) and (LScrollFaces[1] <> LGrid) and
      (LScrollFaces[2] = LGrid),
      'native outer surface scrolls only the real middle between two distinct editor panes');
    { The owned pinned grid keeps LCL's whole-column scrolling. A real page
      request crosses the middle column boundary; a smaller line increment
      may legitimately snap back to the same column without any displacement. }
    SendMessage(LGrid.Handle, WM_HSCROLL, SB_PAGERIGHT, 0);
    Pump;
    LScrollRect := TNyxCollectionStringGrid(LGrid).ColumnHeaderRect(LScrollColumns[2]);
    LScrollPoint := LGrid.ClientToScreen(Types.Point(LScrollRect.Left, LScrollRect.Top));
    Check(LScrollPoint.X < LHeaderBounds[2].X - Round(8 * LScrollPPI / 96),
      'actual native horizontal scroll physically moves the middle header');
    LScrollRect := LGrid.CellRect(LScrollColumns[2], 1);
    LScrollPoint := LGrid.ClientToScreen(Types.Point(LScrollRect.Left, LScrollRect.Top));
    Check(LScrollPoint.X < LCellBounds[2].X - Round(8 * LScrollPPI / 96),
      'actual native horizontal scroll physically moves the middle current-page cell');
    for LIndex := 0 to 1 do
    begin
      LScrollRect := TNyxCollectionStringGrid(LScrollFaces[LIndex]).ColumnHeaderRect(LScrollColumns[LIndex]);
      LScrollPoint := LScrollFaces[LIndex].ClientToScreen(Types.Point(LScrollRect.Left, LScrollRect.Top));
      Check((LScrollPoint.X = LHeaderBounds[LIndex].X) and
        (LScrollPoint.Y = LHeaderBounds[LIndex].Y),
        'both native pinned headers remain physically fixed during middle scroll');
      LScrollRect := LScrollFaces[LIndex].CellRect(LScrollColumns[LIndex], 1);
      LScrollPoint := LScrollFaces[LIndex].ClientToScreen(Types.Point(LScrollRect.Left, LScrollRect.Top));
      Check((LScrollPoint.X = LCellBounds[LIndex].X) and
        (LScrollPoint.Y = LCellBounds[LIndex].Y),
        'both native pinned current-page cells remain physically fixed during middle scroll');
    end;
  {$endif}
    PaintedWidth(NyxColumnIdentity(PersonName), 160);
    PaintedWidth(NyxColumnIdentity(Score), 90);
    PaintedPin(NyxColumnIdentity(PersonName), ncpStart);
    PaintedPin(NyxColumnIdentity(Score), ncpEnd);
    { Enter each still-pinned field through its actual F2 editor. Name must
      recover the exact preexisting unfinished draft; Score still reads its
      accepted typed scalar. No focus/resize path may commit either field. }
    for LIndex := 0 to 1 do
    begin
      {$ifdef PAS2JS}
      LCell := TJSHTMLElement(LTable.querySelector(
        'tr[data-nyx-item="' + LItem.ID + '"] td[data-nyx-column="' + IntToStr(LIndex) + '"]'));
      Check(LCell <> nil, 'pinned editing resolves its current exact row and field after synchronization');
      LCell.focus;
      LOptions := TJSObject.new;
      LOptions['key'] := 'F2';
      LOptions['bubbles'] := True;
      LOptions['cancelable'] := True;
      LCell.dispatchEvent(TReviewKey.new('keydown', LOptions));
      await(ArrangementFrame);
      LEditor := TJSHTMLInputElement(LCell.querySelector('input.nyx-cell-editor'));
      Check((LEditor <> nil) and (Web.document.activeElement = LEditor),
        'each effective browser pin owns a real focused editor after middle scroll');

      if LIndex = 0 then
      begin
        Check(LEditor.value = LDraft, 'start pin resumes the exact unrelated unfinished Name draft');
      end
      else
      begin
        Check(LEditor.value = FViews[0].CellText(LItem, 1),
          'end pin edits the same accepted typed Score scalar after middle scroll');
      end;
      {$else}
      LFace := NativeFieldGrid(LObservation.Spec.ColumnAt(LIndex).Identity,
        LScrollColumns[LIndex]);
      Check(LFace <> nil, 'pinned editing resolves its current native face after synchronization');
      LFace.Col := LScrollColumns[LIndex];
      LFace.Row := 1;
      LFace.SetFocus;
      SendMessage(LFace.Handle, WM_KEYDOWN, VK_F2, 0);
      Pump;
      Check(LFace.EditorMode and (LFace.Editor is TCustomEdit) and LFace.Editor.Focused,
        'each effective native pin owns a real focused editor after middle scroll');
      LEditor := TCustomEdit(LFace.Editor);

      if LIndex = 0 then
      begin
        Check(NyxLCLInputText(LEditor) = LDraft,
          'native start pin resumes the exact unrelated unfinished Name draft');
      end
      else
      begin
        Check(NyxLCLInputText(LEditor) = FViews[0].CellText(LItem, 1),
          'native end pin edits the same accepted typed Score scalar after middle scroll');
      end;
      {$endif}
      Launch;
      {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
      Resolve;
      Check(FViews[0].Store.Snapshot = LData,
        'moving from either pinned editor to the real manager accepts no unfinished cell');

      if LIndex = 0 then
      begin
        ClickAction(NyxColumnArrangementCloseID);
        {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
        ConstrainScrollAllocation;
        {$ifdef PAS2JS}
        await(ArrangementFrame);
        LScrollOwner.scrollLeft := LScrollOwner.scrollWidth - LScrollOwner.clientWidth;
        LScrollOwner.dispatchEvent(TJSEvent.new('scroll'));
        await(ArrangementFrame);
        Check((LScrollOwner.scrollLeft > 8) and
          (LTable.getAttribute('data-nyx-pins-suspended') = 'false') and
          (LScrollHeaders[2].getBoundingClientRect.left < LHeaderBounds[2].left - 8) and
          (LScrollCells[2].getBoundingClientRect.left < LCellBounds[2].left - 8),
          'end-pin editing retains genuine middle displacement after manager synchronization');
        {$else}
        Pump;
        LGrid.LeftCol := LGrid.FixedCols;
        SendMessage(LGrid.Handle, WM_HSCROLL, SB_PAGERIGHT, 0);
        Pump;
        LScrollRect := TNyxCollectionStringGrid(LGrid).ColumnHeaderRect(LScrollColumns[2]);
        LScrollPoint := LGrid.ClientToScreen(Types.Point(LScrollRect.Left, LScrollRect.Top));
        Check(LScrollPoint.X < LHeaderBounds[2].X - Round(8 * LScrollPPI / 96),
          'native end-pin editing retains genuine middle displacement after manager synchronization');
        PaintedPin(NyxColumnIdentity(PersonName), ncpStart);
        PaintedPin(NyxColumnIdentity(Score), ncpEnd);
        {$endif}
      end;
    end;
  finally
    {$ifdef PAS2JS}
    LScrollOwner.style.cssText := LScrollStyle;
    LScrollOwner.scrollLeft := LScrollLeft;
    LMount.Refresh;
    await(ArrangementFrame);
    {$else}
    LGrid.BoundsRect := LScrollBounds;
    LGrid.Anchors := LScrollAnchors;
    LGrid.Align := LScrollAlign;
    LMount.Refresh;
    LGrid.LeftCol := LScrollLeft;
    Pump;
    {$endif}
  end;
  Check((FColumns[0].CaptureColumns.Serial = LBeforeSerial) and
    (FColumns[0].CaptureColumns.Preferences.ToData.ToJSON = LInputPreferences) and
    (FViews[0].Store.Snapshot = LData) and
    (FViews[0].QueryPolicy.ToData.ToJSON = LQuery) and
    (FColumns[1].CaptureColumns.Preferences.ToData.ToJSON = LPeer) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'real middle scroll and both pinned editors preserve original layout, data, query, peer and source');

  EnterWidth(NyxColumnIdentity(PersonName), 300);
  FieldAction(NyxColumnIdentity(PersonName), ncaaApplyWidth);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  EnterWidth(NyxColumnIdentity(Score), 220);
  FieldAction(NyxColumnIdentity(Score), ncaaApplyWidth);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  EnterWidth(NyxColumnIdentity(Active), 140);
  FieldAction(NyxColumnIdentity(Active), ncaaApplyWidth);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  EnterPin(NyxColumnIdentity(PersonName), 'Start edge');
  FieldAction(NyxColumnIdentity(PersonName), ncaaApplyPin);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  EnterPin(NyxColumnIdentity(Score), 'End edge');
  FieldAction(NyxColumnIdentity(Score), ncaaApplyPin);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check((LObservation.ColumnAt(0).Pin = ncpStart) and
    (LObservation.ColumnAt(1).Pin = ncpEnd) and
    (LObservation.ColumnAt(0).Width = 300) and (LObservation.ColumnAt(1).Width = 220),
    'oversized pins retain both original typed edges and exact preferred widths');

  if GWidth < 640 then
  begin
    {$ifdef PAS2JS}
    Check(LTable.getAttribute('data-nyx-pins-suspended') = 'true',
      'actual compact horizontal clip suspends oversized pins as one group');
    {$else}
    Check(Pos('Pinned columns are scrolling at this width.', TNyxText(LGrid.Hint)) > 0,
      'actual compact native allocation exposes the suspended-pin explanation');
    {$endif}
    PaintedPin(NyxColumnIdentity(PersonName), ncpNone);
    PaintedPin(NyxColumnIdentity(Score), ncpNone);
  end
  else
  begin
    {$ifdef PAS2JS}
    Check(LTable.getAttribute('data-nyx-pins-suspended') = 'false',
      'actual wide horizontal clip retains both effective pin edges');
    {$else}
    Check(Pos('Pinned columns are scrolling at this width.', TNyxText(LGrid.Hint)) = 0,
      'actual wide native allocation retains both effective pin edges');
    {$endif}
    PaintedPin(NyxColumnIdentity(PersonName), ncpStart);
    PaintedPin(NyxColumnIdentity(Score), ncpEnd);
  end;
  PaintedWidth(NyxColumnIdentity(PersonName), 300);
  PaintedWidth(NyxColumnIdentity(Score), 220);
  { Genuine F2 still reaches the same typed field, through an edge pane when
    wide or the scrolling outer surface when compact pins were suspended. }
  LPresentation.Close;
  {$ifdef PAS2JS}
  LCell := TJSHTMLElement(LTable.querySelector('tr[data-nyx-item="' + LItem.ID +
    '"] td[data-nyx-column="1"]'));
  Check((LCell <> nil) and not LCell.hasAttribute('aria-hidden'),
    'overflow keeps the real Score cell available');
  LCell.focus;
  LOptions := TJSObject.new;
  LOptions['key'] := 'F2';
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LCell.dispatchEvent(TReviewKey.new('keydown', LOptions));
  await(ArrangementFrame);
  LEditor := TJSHTMLInputElement(LCell.querySelector('input.nyx-cell-editor'));
  Check((LEditor <> nil) and (Web.document.activeElement = LEditor) and
    (LEditor.value = FViews[0].CellText(LItem, 1)),
    'pinned or suspended browser Score editing uses accepted canonical scalar text');
  {$else}
  LFace := NativeFieldGrid(NyxColumnIdentity(Score), LPhysical);
  Check(LFace <> nil, 'pinned or suspended native Score retains a physical editor surface');
  LFace.Col := LPhysical;
  LFace.Row := 1;
  LFace.SetFocus;
  SendMessage(LFace.Handle, WM_KEYDOWN, VK_F2, 0);
  Pump;
  Check(LFace.EditorMode and (LFace.Editor is TCustomEdit) and
    (TNyxText(TCustomEdit(LFace.Editor).Text) = FViews[0].CellText(LItem, 1)),
    'pinned or suspended native Score editing uses accepted canonical scalar text');
  {$endif}
  Launch;
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check((LObservation.ColumnAt(0).Pin = ncpStart) and
    (LObservation.ColumnAt(1).Pin = ncpEnd),
    'editing and real manager reopen do not rewrite suspended pin preferences');
  ClickAction(NyxColumnArrangementActionID(NyxColumnArrangementEditorID, ncaaResetAll));
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check((LObservation.Preferences.Count = 0) and (LObservation.Preferences.OrderCount = 0),
    'real Reset all withdraws width, order and pin overrides without rewriting authored defaults');
  { Continue on the SAME owner and original unfinished Name draft. Header input
    changes runtime arrangement only; the retained manager deliberately keeps
    its old observation until its explicit Reload adopts those publications. }
  ClickAction(NyxColumnArrangementCloseID);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Check(not LPresenter.IsOpen, 'actual Close returns header input to its original table');
  LBeforeSerial := FColumns[0].CaptureColumns.Serial;
  LInputPreferences := FColumns[0].CaptureColumns.Preferences.ToData.ToJSON;
  LInputSemantic := FSemanticClicks;
  LInputLegacy := FLegacyClicks;
  PaintedWidth(NyxColumnIdentity(PersonName), 160);
  {$ifdef PAS2JS}
  FocusResizeGrip;
  ResizeKey('ArrowRight');
  await(ArrangementFrame);
  Check((LGrip.getAttribute('aria-valuenow') = '161') and
    (FColumns[0].CaptureColumns.Serial = LBeforeSerial) and
    (FColumns[0].CaptureColumns.Preferences.ToData.ToJSON = LInputPreferences),
    'Right previews one logical pixel without publishing a width preference');
  PaintedWidth(NyxColumnIdentity(PersonName), 160);
  ResizeKey('ArrowRight', True);
  await(ArrangementFrame);
  Check((LGrip.getAttribute('aria-valuenow') = '171') and
    (FColumns[0].CaptureColumns.Serial = LBeforeSerial) and
    (FColumns[0].CaptureColumns.Preferences.ToData.ToJSON = LInputPreferences),
    'Shift Right previews ten further pixels without changing the accepted layout');
  PaintedWidth(NyxColumnIdentity(PersonName), 160);
  ResizeKey('Escape');
  await(ArrangementFrame);
  Check((LGrip.getAttribute('aria-valuenow') = '160') and
    (FColumns[0].CaptureColumns.Serial = LBeforeSerial) and
    (FColumns[0].CaptureColumns.Preferences.ToData.ToJSON = LInputPreferences),
    'Escape cancels the separator preview and restores its accepted accessible width');
  PaintedWidth(NyxColumnIdentity(PersonName), 160);
  FocusResizeGrip;
  ResizeKey('Home');
  Check((LGrip.getAttribute('aria-valuenow') = '50') and
    (FColumns[0].CaptureColumns.Serial = LBeforeSerial),
    'Home previews the authored minimum before release');
  ResizeKey('Enter');
  await(ArrangementFrame);
  LPreference := Preference(NyxColumnIdentity(PersonName));
  Check(LPreference.HasWidth and (LPreference.WidthValue = 50) and
    (FColumns[0].CaptureColumns.Serial = LBeforeSerial + 1),
    'Home then Enter publishes the minimum width once for the exact typed field');
  PaintedWidth(NyxColumnIdentity(PersonName), 50);
  LBeforeSerial := FColumns[0].CaptureColumns.Serial;
  FocusResizeGrip;
  ResizeKey('End');
  Check((LGrip.getAttribute('aria-valuenow') = '300') and
    (FColumns[0].CaptureColumns.Serial = LBeforeSerial),
    'End previews the authored maximum without early publication');
  ResizeKey('Enter');
  await(ArrangementFrame);
  LPreference := Preference(NyxColumnIdentity(PersonName));
  Check(LPreference.HasWidth and (LPreference.WidthValue = 300) and
    (FColumns[0].CaptureColumns.Serial = LBeforeSerial + 1),
    'End then Enter publishes the maximum width once');
  PaintedWidth(NyxColumnIdentity(PersonName), 300);
  LBeforeSerial := FColumns[0].CaptureColumns.Serial;
  LInputPreferences := FColumns[0].CaptureColumns.Preferences.ToData.ToJSON;
  FocusResizeGrip;
  ResizeKey('ArrowLeft');
  Check(LGrip.getAttribute('aria-valuenow') = '299',
    'Tab cancellation begins with an actual different separator preview');
  ResizeKey('Tab');
  await(ArrangementFrame);
  Check((LGrip.getAttribute('aria-valuenow') = '300') and
    (FColumns[0].CaptureColumns.Serial = LBeforeSerial) and
    (FColumns[0].CaptureColumns.Preferences.ToData.ToJSON = LInputPreferences),
    'Tab cancels unfinished separator input without accepting its preview');
  PaintedWidth(NyxColumnIdentity(PersonName), 300);
  {$else}
  DragNativeHeader(True, LExpectedDragWidth);
  Check((FColumns[0].CaptureColumns.Serial = LBeforeSerial) and
    (FColumns[0].CaptureColumns.Preferences.ToData.ToJSON = LInputPreferences),
    'native Escape followed by the original Up leaves preferences and serial unchanged');
  PaintedWidth(NyxColumnIdentity(PersonName), 160);
  Check((FSemanticClicks = LInputSemantic) and (FLegacyClicks = LInputLegacy),
    'cancelled native header sizing never becomes a ghost semantic or legacy Click');
  DragNativeHeader(False, LExpectedDragWidth);
  LPreference := Preference(NyxColumnIdentity(PersonName));
  Check(LPreference.HasWidth and (LPreference.WidthValue = LExpectedDragWidth) and
    (FColumns[0].CaptureColumns.Serial = LBeforeSerial + 1),
    'fresh native drag after Escape publishes one typed width with one PPI conversion');
  PaintedWidth(NyxColumnIdentity(PersonName), LExpectedDragWidth);
  {$endif}
  Check((FSemanticClicks = LInputSemantic) and (FLegacyClicks = LInputLegacy),
    'header sizing input never fabricates a canonical table Click');
  Check((FViews[0].Store.Snapshot = LData) and
    (FViews[0].QueryPolicy.ToData.ToJSON = LQuery) and
    (FColumns[1].CaptureColumns.Preferences.ToData.ToJSON = LPeer) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'header resize input preserves accepted data, query, independent peer and authored source');
  LExternal := FColumns[0].CaptureColumns.Preferences.ToData.ToJSON;
  LBeforeSerial := FColumns[0].CaptureColumns.Serial;
  Launch;
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check(LPresentation.State.GetValue(NyxTextState(NyxColumnArrangementStatusState)) <> '',
    'reopening the retained manager explains the externally resized stale baseline');
  ClickAction(NyxColumnArrangementReloadID);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check((LObservation.Serial = LBeforeSerial) and
    (LObservation.Preferences.ToData.ToJSON = LExternal),
    'explicit Reload adopts the exact accepted header input observation');
  ClickAction(NyxColumnArrangementActionID(NyxColumnArrangementEditorID, ncaaResetAll));
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check((LObservation.Preferences.Count = 0) and (LObservation.Preferences.OrderCount = 0),
    'real Reset all restores authored arrangement after accepted header input');
  PaintedWidth(NyxColumnIdentity(PersonName), 160);
  LMount.SetInteraction(False, False);
  Check(not LPresenter.IsOpen, 'disabling the actual table closes its manager');
  LMount.SetInteraction(True, False);
  Launch;
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  LPresentation.Close;
  Check(Supports(LMount, INyxCollectionEditMount, LEditMount),
    'manager table retains its actual copied cell-draft observation');
  LEdits := LEditMount.SuspendCellEdits;
  try
    LFound := False;
    for LIndex := 0 to LEdits.Count - 1 do
    begin
      LEdit := LEdits.CellAt(LIndex);

      if LEdit.Field.Same(NyxColumnIdentity(PersonName)) and
        (LEdit.Item.ID = LItem.ID) and (LEdit.Text = LDraft) then
      begin
        LFound := True;
      end;
    end;
    Check(LFound, 'manager focus and hide/show/reset preserve the exact unrelated cell draft');
  finally
    LEditMount.ResumeCellEdits(LEdits, False);
  end;
  LEdits := nil;
  Check((FViews[0].Store.Snapshot = LData) and
    (FViews[0].QueryPolicy.ToData.ToJSON = LQuery) and
    (FColumns[1].CaptureColumns.Preferences.ToData.ToJSON = LPeer) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'manager operations preserve accepted data, query, independent peer and authored source');
  {$ifdef PAS2JS}
  LCell := TJSHTMLElement(LTable.querySelector('tr[data-nyx-item="' + LItem.ID +
    '"] td[data-nyx-column="0"]'));
  LCell.focus;
  LOptions := TJSObject.new;
  LOptions['key'] := 'F2';
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LCell.dispatchEvent(TReviewKey.new('keydown', LOptions));
  await(ArrangementFrame);
  LEditor := TJSHTMLInputElement(LCell.querySelector('input.nyx-cell-editor'));
  Check((LEditor <> nil) and (Web.document.activeElement = LEditor) and
    (LEditor.value = LDraft),
    'fresh explicit browser editing resumes the retained dormant text');
  {$else}
  LBeforeCursor := NativeEditDetails;
  LGrid.Col := 0;
  LGrid.Row := 1;
  LGrid.SetFocus;
  LBeforeF2 := NativeEditDetails;
  SendMessage(LGrid.Handle, WM_KEYDOWN, VK_F2, 0);
  Pump;
  LAfterF2 := NativeEditDetails;
  Check(LGrid.EditorMode and (LGrid.Editor is TCustomEdit) and
    (TNyxText(TCustomEdit(LGrid.Editor).Text) = LDraft),
    'fresh explicit native editing resumes the retained dormant text' +
    '; beforeCursor={' + LBeforeCursor + '}; beforeF2={' + LBeforeF2 +
    '}; afterF2={' + LAfterF2 + '}');
  { Capture that SAME live Name draft behind the manager, then deliberately
    move only the physical cursor while the popup owns focus. Restoration must
    not turn its internal Col/Row writes into another canonical table Click. }
  Launch;
  Resolve;
  LFace := NativeFieldGrid(NyxColumnIdentity(Score), LScorePhysical);
  Check((LFace = LGrid) and (LScorePhysical >= 0),
    'cursor restoration probe uses the original unpinned outer grid');
  LGrid.Col := LScorePhysical;
  Pump;
  Check(LGrid.Col = LScorePhysical,
    'cursor restoration probe actually moves to another physical canonical field');
  LRestoreSemantic := FSemanticClicks;
  LRestoreLegacy := FLegacyClicks;
  LPresentation.Close;
  LBarrier := TQueueBarrier.Create;
  LBarrierLease := LBarrier;
  LExecution := FApp.View.Events.Scheduler.Submit(LBarrierLease, neUIQueue);
  LStarted := GetTickCount64;
  while not LBarrier.Done do
  begin

    if GetTickCount64 - LStarted > 10000 then
    begin
      raise Exception.Create('Actual native cursor restoration UI barrier did not complete');
    end;
    Pump;
  end;
  Check(LExecution.Status = nesSucceeded, 'actual cursor restoration UI queue completed');
  Check((FSemanticClicks = LRestoreSemantic) and (FLegacyClicks = LRestoreLegacy),
    'internal cursor restoration never dispatches another semantic or legacy table Click');
  LEdits := LEditMount.SuspendCellEdits;
  try
    LFound := False;
    for LIndex := 0 to LEdits.Count - 1 do
    begin
      LEdit := LEdits.CellAt(LIndex);

      if LEdit.Field.Same(NyxColumnIdentity(PersonName)) and
        (LEdit.Item.ID = LItem.ID) and (LEdit.Text = LDraft) then
      begin
        LFound := True;
      end;
    end;
    Check(LFound, 'internal cursor restoration retains the exact public Name draft lease');
  finally
    LEditMount.ResumeCellEdits(LEdits, False);
  end;
  LEdits := nil;
  LRowsSame := FViews[0].Store.Snapshot = LData;
  LQuerySame := FViews[0].QueryPolicy.ToData.ToJSON = LQuery;
  LPeerSame := FColumns[1].CaptureColumns.Preferences.ToData.ToJSON = LPeer;
  LSourceSame := TNyxCodec.Encode(FDocument) = FAuthored;
  Check(LRowsSame and LQuerySame and LPeerSame and LSourceSame,
    'cursor restoration preserves accepted rows, query, independent peer and authored source' +
    '; rows=' + TNyxText(BoolToStr(LRowsSame, True)) +
    '; query=' + TNyxText(BoolToStr(LQuerySame, True)) +
    '; peer=' + TNyxText(BoolToStr(LPeerSame, True)) +
    '; source=' + TNyxText(BoolToStr(LSourceSame, True)));
  {$endif}

  { All original unchanged-data/dormant-draft assertions above precede these
    intentional accepted edits. Use the SAME row/store and actual manager to
    restore both logical pins, then commit and restore each typed scalar through
    its real cell producer. A presentation-only view notice is not a data edit. }
  Check(LItem.ID = Person('f').ID,
    'positive pinned commits retain the original current-page Fox item identity');
  Launch;
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  EnterPin(NyxColumnIdentity(PersonName), 'Start edge');
  FieldAction(NyxColumnIdentity(PersonName), ncaaApplyPin);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  EnterPin(NyxColumnIdentity(Score), 'End edge');
  FieldAction(NyxColumnIdentity(Score), ncaaApplyPin);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  PaintedPin(NyxColumnIdentity(PersonName), ncpStart);
  PaintedPin(NyxColumnIdentity(Score), ncpEnd);
  ClickAction(NyxColumnArrangementCloseID);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Check(not LPresenter.IsOpen, 'positive pinned edits start after the real manager Close');
  LCommitStore := FViews[0].Store;
  LCommitPeer := FViews[1].Store.Snapshot;
  LCommitColumns := FColumns[0].CaptureColumns;
  LCommitStoreToken := LCommitStore.Subscribe(CommitStoreChanged);
  try
    LCommitViewToken := FViews[0].Subscribe(CommitViewChanged);
    try
      for LCommitIndex := 0 to 3 do
      begin
        LCommitField := NyxColumnIdentity(PersonName);
        LCommitText := TNyxText('Fox pinned / 🌙');

        if LCommitIndex = 1 then
        begin
          LCommitText := 'Fox';
        end
        else if LCommitIndex >= 2 then
        begin
          LCommitField := NyxColumnIdentity(Score);
          LCommitText := '17';

          if LCommitIndex = 3 then
          begin
            LCommitText := '6';
          end;
        end;
        LCommitCanonical := LObservation.Spec.ColumnIndex(LCommitField);
        LCommitRow := FViews[0].Snapshot.IndexOf(LItem) + 1;
        Check((LCommitCanonical >= 0) and (LCommitRow > 0) and
          (FViews[0].Snapshot.ItemAt(LCommitRow - 1).Ref.ID = LItem.ID),
          'each pinned edit resolves its full typed field and current stable item before F2');
        {$ifdef PAS2JS}
        LCell := TJSHTMLElement(LTable.querySelector('tr[data-nyx-item="' + LItem.ID +
          '"] td[data-nyx-column="' + IntToStr(LCommitCanonical) + '"]'));
        Check(LCell <> nil, 'positive pinned browser edit uses its resident canonical cell');
        LCell.focus;
        LOptions := TJSObject.new;
        LOptions['key'] := 'F2';
        LOptions['bubbles'] := True;
        LOptions['cancelable'] := True;
        LCell.dispatchEvent(TReviewKey.new('keydown', LOptions));
        await(ArrangementFrame);
        LEditor := TJSHTMLInputElement(LCell.querySelector('input.nyx-cell-editor'));
        Check((LEditor <> nil) and (Web.document.activeElement = LEditor),
          'positive pinned browser commit owns the actual focused input');
        {$else}
        LFace := NativeFieldGrid(LCommitField, LPhysical);
        Check((LFace <> nil) and (LFace <> LGrid) and (LPhysical >= 0),
          'positive native pinned commit resolves the actual separate edge editor pane');
        LFace.Col := LPhysical;
        LFace.Row := LCommitRow;
        LFace.SetFocus;
        SendMessage(LFace.Handle, WM_KEYDOWN, VK_F2, 0);
        Pump;
        Check(LFace.EditorMode and (LFace.Editor is TCustomEdit) and LFace.Editor.Focused,
          'positive native pinned commit owns its real focused cell editor');
        LEditor := TCustomEdit(LFace.Editor);
        {$endif}
        PaintedPin(NyxColumnIdentity(PersonName), ncpStart);
        PaintedPin(NyxColumnIdentity(Score), ncpEnd);
        LCommitBefore := LCommitStore.Snapshot;
        LCommitExpected := LCommitBefore.Item(LItem);

        if LCommitIndex < 2 then
        begin
          LCommitExpected := LCommitExpected.WithValue(PersonName, LCommitText);
        end
        else if LCommitIndex = 2 then
        begin
          LCommitExpected := LCommitExpected.WithValue(Score, 17);
        end
        else
        begin
          LCommitExpected := LCommitExpected.WithValue(Score, 6);
        end;
        FCommitStoreNotifications := 0;
        FCommitViewNotifications := 0;
        FCommitStoreChanges := nil;
        FCommitViewChanges := nil;
        {$ifdef PAS2JS}
        LEditor.value := LCommitText;
        LEditor.dispatchEvent(TJSEvent.new('input'));
        {$else}
        RestoreNyxLCLInputText(LEditor, LCommitText);
        {$endif}
        Check((LCommitStore.Snapshot = LCommitBefore) and
          (FCommitStoreNotifications = 0) and (FCommitViewNotifications = 0),
          'editing pinned text alone accepts no scalar and emits no data notification');
        {$ifdef PAS2JS}
        { A synthetic Enter does not run the HTML default change algorithm.
          The resident input's real change handler is this adapter's producer. }
        LEditor.dispatchEvent(TJSEvent.new('change'));
        await(ArrangementFrame);
        {$else}
        { TStringCellEditor forwards Return to the arranged grid's existing
          CommitColumnEditor/ValidateEntry route. It consumes KeyDown; sending
          a fabricated WM_CHAR afterwards could reopen the accepted editor. }
        LCommitHandle := LEditor.Handle;
        SendMessage(LCommitHandle, WM_KEYDOWN, VK_RETURN, 0);
        SendMessage(LCommitHandle, WM_KEYUP, VK_RETURN, 0);
        Pump;
        {$endif}
        LCommitAfter := LCommitStore.Snapshot;
        Check((FViews[0].Store = LCommitStore) and
          (LCommitAfter.Revision = LCommitBefore.Revision + 1) and
          (FCommitStoreNotifications = 1) and (FCommitViewNotifications = 1),
          'one pinned scalar commit advances once and notifies the original Store and data View once' +
          '; field=' + LCommitField.Name + '; store=' + IntToStr(FCommitStoreNotifications) +
          '; view=' + IntToStr(FCommitViewNotifications) +
          '; revision=' + IntToStr(LCommitAfter.Revision - LCommitBefore.Revision) +
          '; mountError="' + LMount.ErrorText + '"');
        for LCommitChangeIndex := 0 to 1 do
        begin
          LCommitChanges := FCommitStoreChanges;

          if LCommitChangeIndex = 1 then
          begin
            LCommitChanges := FCommitViewChanges;
          end;
          Check((LCommitChanges <> nil) and (LCommitChanges.Count = 1) and
            (LCommitChanges.Kind(0) = nceUpdate) and
            (LCommitChanges.ItemRef(0).ID = LItem.ID) and
            (LCommitChanges.Before = LCommitBefore) and
            (LCommitChanges.After = LCommitAfter) and
            (LCommitChanges.BeforeIndex(0) = LCommitBefore.IndexOf(LItem)) and
            (LCommitChanges.AfterIndex(0) = LCommitAfter.IndexOf(LItem)) and
            LCommitChanges.BeforeItem(0).SameItem(LCommitBefore.Item(LItem)) and
            LCommitChanges.AfterItem(0).SameItem(LCommitExpected),
            'Store and data View each report one exact typed nceUpdate for the same item and accepted scalars');
        end;
        Check(LCommitAfter.Count = LCommitBefore.Count,
          'pinned scalar commit neither inserts nor removes any accepted row');
        for LCommitValueIndex := 0 to LCommitBefore.Count - 1 do
        begin

          if LCommitBefore.ItemAt(LCommitValueIndex).Ref.ID = LItem.ID then
          begin
            Check(LCommitAfter.ItemAt(LCommitValueIndex).SameItem(LCommitExpected),
              'accepted pinned text/integer changes only its exact typed scalar on the original row');
          end
          else
          begin
            Check(LCommitAfter.ItemAt(LCommitValueIndex).SameItem(
              LCommitBefore.ItemAt(LCommitValueIndex)),
              'positive pinned scalar commit preserves every other row and typed value');
          end;
        end;
        Check((FColumns[0].CaptureColumns.Serial = LCommitColumns.Serial) and
          (FColumns[0].CaptureColumns.Preferences.ToData.ToJSON =
            LCommitColumns.Preferences.ToData.ToJSON) and
          (FViews[0].QueryPolicy.ToData.ToJSON = LQuery) and
          (FViews[1].Store.Snapshot = LCommitPeer) and
          (FColumns[1].CaptureColumns.Preferences.ToData.ToJSON = LPeer) and
          (TNyxCodec.Encode(FDocument) = FAuthored),
          'accepted pinned commits preserve query, both arrangements, independent peer snapshot and authored source');
      end;
    finally
      LCommitViewToken.Disconnect;
      LCommitViewToken := nil;
    end;
  finally
    LCommitStoreToken.Disconnect;
    LCommitStoreToken := nil;
    FCommitStoreChanges := nil;
    FCommitViewChanges := nil;
  end;
  Check((LCommitStore.Snapshot.Revision = LData.Revision + 4) and
    (LCommitStore.Snapshot.Count = LData.Count),
    'four real pinned commits include both explicit restores and exactly four accepted revisions');
  for LIndex := 0 to LData.Count - 1 do
  begin
    Check(LCommitStore.Snapshot.ItemAt(LIndex).SameItem(LData.ItemAt(LIndex)),
      'explicit pinned editor restores recover every original typed row value');
  end;
  Launch;
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  ClickAction(NyxColumnArrangementActionID(NyxColumnArrangementEditorID, ncaaResetAll));
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Resolve;
  Check((LObservation.Preferences.Count = 0) and (LObservation.Preferences.OrderCount = 0),
    'real manager Reset all retires only the positive-commit pin setup');
  ClickAction(NyxColumnArrangementCloseID);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
end;

procedure TArrangementReview.ColumnActivationCases; {$ifdef PAS2JS}async;{$endif}
var
  LMount: INyxCollectionMount;
  LManagerMount: INyxCollectionColumnArrangementMount;
  LManager: INyxColumnArrangementManager;
  LEditMount: INyxCollectionEditMount;
  LEdits: INyxCollectionEditsSnapshot;
  LGeneration: INyxCancellationScope;
  LData: INyxCollectionSnapshot;
  LQuery: TNyxText;
  LPeer: TNyxText;
  LBaseClicks: Integer;
  LBaseSemantic: Integer;
  LBaseLegacy: Integer;
  LExpected: Integer;
  {$ifdef PAS2JS}
  LBeforeRelease: Integer;
  LTable: TJSHTMLElement;
  LButton: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LOptions: TJSObject;
  LEvent: TJSMouseEvent;
  {$else}
  LGrid: TStringGrid;
  LButton: TButton;
  {$endif}

  function ManagerOpen: Boolean;
  begin
    Result := (LManager.Presenter <> nil) and LManager.Presenter.IsOpen;
  end;

  procedure Delivery;
  begin
    Check(FApp.State.GetValue(ArrangementClicks) = LBaseClicks + LExpected,
      'real Columns activation executes its authored scalar action exactly once');
    Check((FSemanticClicks = LBaseSemantic + LExpected) and
      (FLegacyClicks = LBaseLegacy + LExpected),
      'Columns activation delivers its semantic and legacy Click exactly once / semantic got=' +
      TNyxText(IntToStr(FSemanticClicks)) + ' expected=' +
      TNyxText(IntToStr(LBaseSemantic + LExpected)) + ' / legacy got=' +
      TNyxText(IntToStr(FLegacyClicks)) + ' expected=' +
      TNyxText(IntToStr(LBaseLegacy + LExpected)));
  end;

  procedure CloseManager;
  begin

    if ManagerOpen then
    begin
      LManager.Presenter.Close;
    end;
    Pump;
  end;

  procedure FocusLauncher;
  begin
    {$ifdef PAS2JS}LButton.focus;{$else}LButton.SetFocus;{$endif}
    Pump;
  end;

  procedure Activate;
  begin
    {$ifdef PAS2JS}
    LButton.click;
    {$else}
    SendMessage(LButton.Handle, WM_LBUTTONDOWN, MK_LBUTTON, 8 or (8 shl 16));
    SendMessage(LButton.Handle, WM_LBUTTONUP, 0, 8 or (8 shl 16));
    {$endif}
    Pump;
  end;

  procedure KeyPhase(AKey: Word; const AName: String; ADown: Boolean);
  {$ifdef PAS2JS}
  var
    LKeyOptions: TJSObject;
    LType: String;
  {$endif}
  begin
    {$ifdef PAS2JS}
    LKeyOptions := TJSObject.new;
    LKeyOptions['key'] := AName;
    LKeyOptions['bubbles'] := True;
    LKeyOptions['cancelable'] := True;
    LType := 'keyup';

    if ADown then
    begin
      LType := 'keydown';
    end;
    LButton.dispatchEvent(TReviewKey.new(LType, LKeyOptions));
    {$else}

    if ADown then
    begin
      SendMessage(LButton.Handle, WM_KEYDOWN, AKey, 0);
    end
    else
    begin
      SendMessage(LButton.Handle, WM_KEYUP, AKey, 0);
    end;
    {$endif}
    Pump;
  end;

begin
  { This follows the original manager/draft journey on its SAME fresh owner.
    The optional authored Click was installed before mounting; scalar state is
    runtime-owned and is independent of both accepted collection stores. }
  LMount := FApp.View.CollectionMount(CArrangementFirst);
  Check(Supports(LMount, INyxCollectionColumnArrangementMount, LManagerMount),
    'canonical Columns journey uses the actual maintained manager mount');
  LManager := LManagerMount.ColumnArrangement;
  FProofManager := LManager;
  LBaseClicks := FApp.State.GetValue(ArrangementClicks);
  LBaseSemantic := FSemanticClicks;
  LBaseLegacy := FLegacyClicks;
  LExpected := 0;
  LData := FViews[0].Store.Snapshot;
  LQuery := FViews[0].QueryPolicy.ToData.ToJSON;
  LPeer := FColumns[1].CaptureColumns.Preferences.ToData.ToJSON;
  FApp.View.Reveal(CArrangementFirst, niRuntime);
  {$ifdef PAS2JS}
  LTable := FApp.View.ElementFor(CArrangementFirst);
  LButton := TJSHTMLElement(LTable.querySelector('[data-nyx-columns-button]'));
  Check((LButton <> nil) and (LButton.tagName = 'BUTTON'),
    'canonical Columns input owns the real persistent native browser button');
  {$else}
  LGrid := TStringGrid(FApp.View.ControlFor(CArrangementFirst));
  LButton := TButton(LGrid.FindComponent('NyxColumnManagerButton'));
  Check((LButton <> nil) and LButton.Visible and LButton.Enabled,
    'canonical Columns input owns the real persistent native button');
  {$endif}
  CloseManager;
  Activate;
  Inc(LExpected);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Delivery;
  Check(ManagerOpen, 'canonical single Click opens the actual column manager');
  CloseManager;

  FocusLauncher;
  KeyPhase(13, 'Enter', True);
  KeyPhase(13, 'Enter', False);
  Inc(LExpected);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Delivery;
  Check(ManagerOpen, 'Columns Enter invokes the same real Click and Open path');
  CloseManager;

  FocusLauncher;
  KeyPhase(32, ' ', True);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Delivery;
  Check(not ManagerOpen, 'holding Columns Space never activates on keydown');
  KeyPhase(32, ' ', False);
  Inc(LExpected);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Delivery;
  Check(ManagerOpen, 'Columns Space activates exactly once on its matching release');
  CloseManager;

  FocusLauncher;
  KeyPhase(32, ' ', True);
  {$ifdef PAS2JS}
  LCell := TJSHTMLElement(LTable.querySelector('[role=gridcell]'));
  Check(LCell <> nil, 'Columns blur target is an actual cell in the same table');
  LCell.focus;
  {$else}LGrid.SetFocus;{$endif}
  FocusLauncher;
  KeyPhase(32, ' ', False);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Delivery;
  Check(not ManagerOpen, 'focus away and back cannot revive a held Columns press');

  FocusLauncher;
  KeyPhase(32, ' ', True);
  LMount.SetInteraction(False, False);
  LMount.SetInteraction(True, False);
  FocusLauncher;
  KeyPhase(32, ' ', False);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Delivery;
  Check(not ManagerOpen, 'disable and re-enable cannot dispatch an old Columns release');

  Check(Supports(LMount, INyxCollectionEditMount, LEditMount),
    'Columns cancellation uses the actual public edit-suspension lease');
  FocusLauncher;
  KeyPhase(32, ' ', True);
  LEdits := LEditMount.SuspendCellEdits;
  try
    LEditMount.ResumeCellEdits(LEdits, False);
  finally
    LEdits := nil;
  end;
  FocusLauncher;
  KeyPhase(32, ' ', False);
  {$ifdef PAS2JS}await(ArrangementFrame);{$endif}
  Delivery;
  Check(not ManagerOpen, 'suspend and resume cannot dispatch an old Columns release');

  {$ifdef PAS2JS}
  FocusLauncher;
  KeyPhase(32, ' ', True);
  KeyPhase(27, 'Escape', True);
  KeyPhase(32, ' ', False);
  await(ArrangementFrame);
  Delivery;
  Check(not ManagerOpen, 'Escape terminally cancels the held browser Columns press');

  FClickMode := acmConsumeRelease;
  LBeforeRelease := FReleaseCalls;
  try
    FocusLauncher;
    KeyPhase(32, ' ', True);
    KeyPhase(32, ' ', False);
  finally
    FClickMode := acmOrdinary;
  end;
  await(ArrangementFrame);
  Delivery;
  Check((FReleaseCalls = LBeforeRelease + 1) and not ManagerOpen,
    'consumed real Columns KeyUp delivers its phase without Click or Open');

  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LOptions['button'] := 0;
  LEvent := TReviewMouse.new('click', LOptions);
  LEvent.preventDefault;
  LButton.dispatchEvent(LEvent);
  await(ArrangementFrame);
  Delivery;
  Check(not ManagerOpen, 'already-prevented same-host Columns Click performs no action or Open');

  FClickMode := acmPreventClick;
  FCurrentClick := TReviewMouse.new('click', LOptions);
  try
    LButton.dispatchEvent(FCurrentClick);
  finally
    FCurrentClick := nil;
    FClickMode := acmOrdinary;
  end;
  Inc(LExpected);
  await(ArrangementFrame);
  Delivery;
  Check(not ManagerOpen,
    'preventing Columns Click during its semantic callback preserves action/delivery but denies Open');

  Activate;
  Activate;
  Inc(LExpected, 2);
  await(ArrangementFrame);
  Delivery;
  Check(ManagerOpen, 'two browser Columns Clicks each retain canonical activation');
  FClickMode := acmCloseDouble;
  LBeforeRelease := FDoubleCalls;
  try
    LButton.dispatchEvent(TReviewMouse.new('dblclick', LOptions));
  finally
    FClickMode := acmOrdinary;
  end;
  await(ArrangementFrame);
  Delivery;
  Check((FDoubleCalls = LBeforeRelease + 1) and not ManagerOpen,
    'canonical DoubleClick may Close the real popup without a third default Open');
  {$endif}

  Check((FViews[0].Store.Snapshot = LData) and
    (FViews[0].QueryPolicy.ToData.ToJSON = LQuery) and
    (FColumns[1].CaptureColumns.Preferences.ToData.ToJSON = LPeer) and
    (TNyxCodec.Encode(FDocument) = FAuthored),
    'canonical Columns actions preserve accepted rows, query, independent peer and authored source');
  CloseManager;
  LGeneration := NyxEventGeneration(FApp.View.Events);
  LBaseLegacy := FLegacyClicks;
  LBaseSemantic := FSemanticClicks;
  LBaseClicks := FApp.State.GetValue(ArrangementClicks);
  FClickMode := acmUnmount;
  Activate;
  Check((FApp.State.GetValue(ArrangementClicks) = LBaseClicks + 1) and
    (FSemanticClicks = LBaseSemantic + 1),
    'callback Unmount retains the already committed Columns action and semantic Click');
  Check(LGeneration.Cancelled and not LManager.Connected and
    (FLegacyClicks = LBaseLegacy) and (FApp.View.Root = nil),
    'callback Unmount retires the actual manager and prevents stale legacy/default tails');
  FProofManager := nil;
end;

{$ifdef PAS2JS}
procedure TArrangementReview.OpenForReview;
var
  LTable: TJSHTMLElement;
  LButton: TJSHTMLElement;
begin
  FApp.View.Reveal(CArrangementFirst, niRuntime);
  LTable := FApp.View.ElementFor(CArrangementFirst);
  LButton := TJSHTMLElement(LTable.querySelector('[data-nyx-columns-button]'));

  if LButton = nil then
  begin
    raise Exception.Create('The actual Columns launcher is missing');
  end;
  LButton.click;
end;

function ReleaseColumnReview(AEvent: TEventListenerEvent): Boolean;
var
  LReview: TArrangementReview;
begin
  Result := True;
  LReview := GColumnReview;
  GColumnReview := nil;
  LReview.Free;
end;

procedure ReviewColumns; async;
begin
  { The same owned seed/spec is available for visual/manual inspection. There
    is deliberately no Run, Check, terminal PASS/count or automatic retirement. }
  GColumnReview := TArrangementReview.Create;
  Web.document.title := 'Column manager manual review';
  Web.document.body.setAttribute('data-column-review', 'columns');
  window.addEventListener('pagehide', @ReleaseColumnReview);
  await(ArrangementFrame);
  GColumnReview.OpenForReview;
end;
{$endif}

procedure ManagedColumnCases; {$ifdef PAS2JS}async;{$endif}
var
  LReview: TArrangementReview;
begin
  LReview := TArrangementReview.Create(True);
  try
    {$ifdef PAS2JS}await(LReview.ColumnManagerCases);{$else}LReview.ColumnManagerCases;{$endif}
    {$ifdef PAS2JS}await(LReview.ColumnActivationCases);{$else}LReview.ColumnActivationCases;{$endif}
  finally
    LReview.Free;
  end;
end;
{ END COLUMN ARRANGEMENT CASES }

procedure Run; {$ifdef PAS2JS}async;{$endif}
begin
  try
    Start;
    InitialAndSortCases;
    DraftAndNegotiationCases;
    ScalarAndSortFormCases;
    PeerAndRecoveryCases;
    PolicyAndTakeoverCases;
    {$ifdef PAS2JS}await(QueuedAndFinalFenceCases);{$else}QueuedAndFinalFenceCases;{$endif}
    LifetimeCases;
    { BEGIN COLUMN ARRANGEMENT CALL }
    {$ifdef PAS2JS}await(ArrangementCases);{$else}ArrangementCases;{$endif}
    {$ifdef PAS2JS}await(ArrangedQueryCases);{$else}ArrangedQueryCases;{$endif}
    {$ifdef PAS2JS}await(ManagedColumnCases);{$else}ManagedColumnCases;{$endif}
    { END COLUMN ARRANGEMENT CALL }
    Cleanup;
    {$ifdef PAS2JS}
    Web.document.body.setAttribute('data-query-workbench-checks', IntToStr(GChecks));
    Web.document.body.setAttribute('data-query-workbench', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' actual table query workbench checks at ', GWidth);
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      Web.document.body.setAttribute('data-query-workbench-error', E.Message);
      Web.document.body.setAttribute('data-query-workbench', 'failed');
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Cleanup;
    end;
  end;
end;

begin
  GWidth := 1280;
  {$ifdef PAS2JS}

  if window.innerWidth < 640 then
  begin
    GWidth := 390;
  end;

  if Pos('&review=columns&', '&' + Copy(window.location.search, 2,
    Length(window.location.search)) + '&') > 0 then
  begin
    ReviewColumns;
  end
  else
  begin
    Run;
  end;
  {$else}

  if ParamCount > 1 then
  begin
    GWidth := StrToInt(ParamStr(2));
  end;

  if ParamCount > 0 then
  begin
    GEvidence := ParamStr(1);
    Check(LazFileUtils.FilenameIsAbsolute(GEvidence) and DirectoryExists(GEvidence),
      'qualification evidence uses a caller supplied absolute existing directory');
  end;
  Application.Initialize;
  GFailures := TFailureObserver.Create;
  Application.OnException := GFailures.Failed;
  try
    Run;
  finally
    Application.OnException := nil;
    GFailures.Free;
    GFailures := nil;
  end;
  {$endif}
end.
