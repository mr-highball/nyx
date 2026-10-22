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
  {$else}Classes, Types, Interfaces, Forms, Controls, StdCtrls, Grids,
    LCLType, nyx.application.lcl, nyx.render.lcl, nyx.collections.query.lcl,
  {$endif}
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.source, nyx.schema, nyx.data, nyx.state, nyx.events, nyx.behavior,
  nyx.scheduler, nyx.publication, nyx.collections, nyx.collections.query,
  nyx.collections.query.policy, nyx.collections.query.session,
  nyx.collections.query.editor, nyx.collections.query.controls,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.selection,
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
  Run;
  {$else}

  if ParamCount > 1 then
  begin
    GWidth := StrToInt(ParamStr(2));
  end;

  if ParamCount > 0 then
  begin
    GEvidence := ParamStr(1);
    Check(UpperCase(ExtractFileDrive(GEvidence)) = 'E:',
      'qualification evidence is confined to the caller supplied E: output directory');
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
