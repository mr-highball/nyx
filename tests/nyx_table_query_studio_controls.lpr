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

program nyx_table_query_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser, nyx.application.browser, nyx.collections.query.browser,
  nyx.studio.browser, nyx.studio.agents, nyx.studio.exchange, nyx.test.editor.exchange,{$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, Grids, LCLType, Types,
  nyx.render.lcl, nyx.application.lcl, nyx.collections.query.lcl,
  nyx.collections.lcl.grid,
  nyx.studio.lcl, nyx.studio.projectstore, nyx.files.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data, nyx.state,
  nyx.behavior, nyx.callbacks, nyx.events, nyx.scheduler, nyx.schema,
  nyx.codec, nyx.codegen, nyx.source, nyx.disclosure, nyx.files, nyx.studio.files,
  nyx.collections, nyx.collections.codec, nyx.collections.selection, nyx.collections.view.types,
  nyx.collections.columns, nyx.collections.paging, nyx.collections.query,
  nyx.collections.query.editor, nyx.collections.query.policy, nyx.collections.view,
  nyx.collections.mount, nyx.collections.query.controls, nyx.studio.section.views,
  nyx.collections.arrangement,
  nyx.collections.workbench.policy,
  nyx.collections.presentation,
  nyx.collections.alignment,
  nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs,
  nyx.studio.view, nyx.studio.collections, nyx.studio.collectionintent,
  nyx.studio.collectionedits, nyx.studio.stateedits, nyx.studio.authoring,
  nyx.studio.inspector, nyx.studio.menu, nyx.menu.editor
  {$ifdef NYX_COMPILED_TABLE_QUERY_STUDIO}, nyx.fixture.table.query.studio{$endif};

type
  TRenderer = TNyxStudioSectionViews;
  {$ifdef PAS2JS}
  TApplicationHost = TNyxBrowserApplication;
  TApplicationRenderer = TNyxBrowserRenderer;
  { Construct ordinary DOM input. These are synthetic events and make no
    hardware/trusted-default or assistive-technology claim. }
  TPhysicalMouse = class external name 'MouseEvent'(TJSMouseEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  TPhysicalKey = class external name 'KeyboardEvent'(TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TApplicationHost = TNyxLCLApplication;
  TApplicationRenderer = TNyxLCLRenderer;
  TControlAccess = class(TControl);
  { Enter installed LCL's real focus/hit/resize admission and release path,
    rather than calling its header callback as a bypass. }
  TGridAccess = class(TStringGrid);
  { Observe exceptions that an ordinary LCL event callback would otherwise
    report through Application rather than returning to the test frame. }
  TFailureObserver = class
    Text: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}
  { Only the copied file-delivery boundary is controlled. Full Studio owns
    import/export, strict source admission, history and the actual controls. }
  TProjectFiles = class(TInterfacedObject, INyxTextFileExchange)
  public
    Reply: TNyxTextFileReply;
    Exported: TNyxTextFiles;
    procedure Pick(const ASelection: TNyxTextFileSelection; AReply: TNyxTextFileReply);
    function ExportFiles(const AFiles: TNyxTextFiles): Boolean;
    procedure Cancel;
    procedure Deliver;
  end;
  { Ordinary host ownership stays in Studio. Overrides supply only copied file
    bytes and the maintained asynchronous in-process editor transport. }
  {$ifdef PAS2JS}TWorkshopStudio = class(TNyxStudio)
  {$else}TWorkshopStudio = class(TNyxNativeStudio){$endif}
  protected
    function CreateProjectFiles: INyxTextFileExchange; override;
    {$ifdef PAS2JS}
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
    {$endif}
  end;
  { Both targets own full Studio and ordinary live widget input. The browser's
    separately owned session is a settled, exact accepted-pair inspection copy;
    only full Studio/exchange owns mutations, history, file input and source jobs. }
  TReview = class
  private
    FRenderer: TRenderer;
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FStudio: TWorkshopStudio;
    {$ifndef PAS2JS}
    FHost: TForm;
    FFailure: TFailureObserver;
    {$endif}
    FStage: Integer;
    { Layout authoring extends this same Studio journey before its existing
      Save/Open and compiled-companion stages; the original query stages stay. }
    FLayoutStage: Integer;
    FLayoutFinished: Boolean;
    FLayoutInitial: TNyxCollectionViewSpec;
    { Workbench authoring runs before the same Save/Open producer. The optional
      focused build exercises only this ordinary Studio path and reports its
      own narrower marker, never the full query/application acceptance marker. }
    FWorkbenchStage: Integer;
    FWorkbenchFinished: Boolean;
    FWorkbenchInitial: TNyxCollectionViewSpec;
    FWorkbenchAccepted: TNyxText;
    { The same ordinary workbench path accepts presentation before Save/Open.
      Retain its complete pair so independent resets and Undo cannot quietly
      substitute a regenerated source or discard another policy's declaration. }
    FPresentationAccepted: TNyxText;
    FBefore, FAfter, FDefinition, FSibling, FDefaults: TNyxText;
    { Copied descriptive handler/registration names outlive source admission and
      model replacement. They are checked against typed authored snapshots. }
    FSaved, FBeforeHandler, FChangedHandler, FBeforeRegistration, FChangedRegistration: TNyxText;
    {$ifdef PAS2JS}FSetup: Integer;{$endif}
    procedure Ready;
    procedure Refresh; {$ifdef PAS2JS}async;{$endif}
    {$ifdef PAS2JS}
    procedure InspectAccepted;
    procedure AwaitStudio; async;
    {$endif}
    procedure Change(const AID, AValue: TNyxText);
    procedure Click(const AID: TNyxText);
    procedure Expand(AColumn: Integer);
    procedure ExpandLayout(AColumn: Integer);
    function LayoutStep: Boolean; {$ifdef PAS2JS}async;{$endif}
    function WorkbenchStep: Boolean; {$ifdef PAS2JS}async;{$endif}
    function Pair: TNyxText;
    { Return a real section root rather than passing the native lookup forest
      to a compound capture that requires an actual owned model tree. }
    function Root: TNyxNode;
    function Spec: TNyxCollectionViewSpec;
    procedure Isolated;
    procedure History; {$ifdef PAS2JS}async;{$endif}
    procedure CaptureRefused(const AID: TNyxText);
    procedure Inspector;
    function EventKey(AEvent: TNyxQueryWorkbenchEvent): TNyxText;
    function AuthoredEvent(AEvent: TNyxQueryWorkbenchEvent): TNyxAuthoredEventInfo;
    procedure CheckSavedEvents;
    procedure ProjectName;
  public
    constructor Create;
    destructor Destroy; override;
    function Step: Boolean; {$ifdef PAS2JS}async;{$endif}
  end;

const
  CEditor = 'inspector-collection-query';
  CFirst = 'first-table';
  CSecond = 'second-table';
  CDefinition = 'table-definition';
var
  GChecks, GPolls, GWidth: Integer;
  GReview: TReview;
  GEvidence: TNyxText;
  GSaved: TNyxText;
  GClosing, GDriving: Boolean;
  GFiles: TProjectFiles;
  GFileLease: INyxTextFileExchange;
  {$ifdef PAS2JS}
  GCore: TNyxAgentSession;
  { Studio's main bridge and completion client each own a distinct transport.
    This vector only borrows them for the qualification clock; clear it after
    Studio frees those owners, before releasing their shared backend session. }
  GExchanges: array of TNyxTestEditorExchange;
  {$endif}

{ Diagnostic presentation only. Native output is flushed at each completed
  boundary; browser console capture retains the last entered phase even when a
  synchronous call prevents DOM inspection. No widget, scheduler, query or
  pending-state getter is evaluated for logging. }
procedure Trace(const APhase: TNyxText; AStage: Integer = -1);
begin
  {$ifdef PAS2JS}
  document.body.setAttribute('data-query-authoring-stage', APhase +
    ';stage=' + IntToStr(AStage) + ';checks=' + IntToStr(GChecks));
  document.body.setAttribute('data-query-authoring-polls', IntToStr(GPolls));
  document.title := 'Studio trace / ' + APhase + ' / ' + IntToStr(GChecks);
  console.log('TRACE ', APhase, ';stage=', AStage, ';checks=', GChecks);
  {$else}
  WriteLn('TRACE ticks=', GetTickCount64, ' stage=', AStage,
    ' checks=', GChecks, ' ', APhase);
  Flush(Output);
  {$endif}
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Table query authoring: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TProjectFiles.Pick(const ASelection: TNyxTextFileSelection;
  AReply: TNyxTextFileReply);
begin
  ASelection.Validate;
  Reply := AReply;
end;

function TProjectFiles.ExportFiles(const AFiles: TNyxTextFiles): Boolean;
begin
  ValidateNyxTextFiles(AFiles, NyxTextFileSelection.UpToFiles(2));
  Exported := Copy(AFiles, 0, Length(AFiles));
  Result := True;
end;

procedure TProjectFiles.Cancel;
begin
  Reply := nil;
end;

procedure TProjectFiles.Deliver;
var
  LReply: TNyxTextFileReply;
  LLease: INyxTextFileExchange;
begin
  LLease := Self;
  LReply := Reply;
  Reply := nil;
  Check(Assigned(LReply), 'ordinary Import requested its copied saved project');
  LReply(fpsSelected, Exported, '');
end;

function TWorkshopStudio.CreateProjectFiles: INyxTextFileExchange;
begin
  Result := GFileLease;
end;

{$ifdef PAS2JS}
function TWorkshopStudio.CreateEditorExchange: TNyxStudioEditorExchange;
var
  LExchange: TNyxTestEditorExchange;
begin
  LExchange := TNyxTestEditorExchange.Create(GCore);
  try
    SetLength(GExchanges, Length(GExchanges) + 1);
    GExchanges[High(GExchanges)] := LExchange;
    Result := LExchange;
  except
    LExchange.Free;
    raise;
  end;
end;

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

procedure ServiceExchange;
var
  LIndex: Integer;
  LExchange: TNyxTestEditorExchange;
  LState: TNyxText;
begin
  LIndex := 0;
  while LIndex < Length(GExchanges) do
  begin
    LExchange := GExchanges[LIndex];

    if LExchange.RequestPending then
    begin
      Trace('exchange:deliver:before', LIndex);
      LExchange.Deliver;
      Trace('exchange:deliver:after', LIndex);
    end
    else if LExchange.TickPending then
    begin
      Trace('exchange:tick:before', LIndex);
      LExchange.FireTick;
      Trace('exchange:tick:after', LIndex);
    end;
    { A reply can replace or retire its owning Studio. Reacquire the vector at
      the next iteration and never touch the just-serviced receiver afterward. }
    Inc(LIndex);
  end;
  { Copied protocol-clock progress only: these public pending/counter reads do
    not dispatch another reply, mutate a source or claim synchronization. }
  LState := '';
  for LIndex := 0 to High(GExchanges) do
  begin
    LExchange := GExchanges[LIndex];
    LState := LState + IntToStr(LIndex) + ':request=' +
      BoolToStr(LExchange.RequestPending, True) + ',tick=' +
      BoolToStr(LExchange.TickPending, True) + ',posts=' +
      IntToStr(LExchange.Posts) + ',commits=' + IntToStr(LExchange.Commits) + ';';
  end;
  document.body.setAttribute('data-query-authoring-exchanges', IntToStr(Length(GExchanges)));
  document.body.setAttribute('data-query-authoring-exchange-state', LState);
end;

procedure ClickProjectMenu(const AID: TNyxText);
var
  LFace: TJSHTMLElement;
begin
  LFace := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
  Check((LFace <> nil) and (LFace.getBoundingClientRect.width > 0),
    'ordinary browser Project popup is physically mounted: ' + AID);
  LFace.click;
end;
{$else}
function MenuButton(AParent: TWinControl; const ACaption: String): TControl;
var
  LIndex: Integer;
  LChild: TControl;
begin
  Result := nil;
  for LIndex := 0 to AParent.ControlCount - 1 do
  begin
    LChild := AParent.Controls[LIndex];

    if LChild.IsVisible and ((TControlAccess(LChild).Caption = ACaption) or
      (Pos(ACaption + ' ', TControlAccess(LChild).Caption) = 1)) then
    begin
      Exit(LChild);
    end;

    if LChild is TWinControl then
    begin
      Result := MenuButton(TWinControl(LChild), ACaption);

      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure ClickProjectMenu(const AWindow, ACaption: String);
var
  LIndex: Integer;
  LButton: TControl;
begin
  LButton := nil;
  for LIndex := 0 to Screen.FormCount - 1 do
  begin

    if Screen.Forms[LIndex].Visible and (Screen.Forms[LIndex].Caption = AWindow) then
    begin
      LButton := MenuButton(Screen.Forms[LIndex], ACaption);
      Break;
    end;
  end;
  Check((LButton <> nil) and LButton.Enabled,
    'ordinary native Project popup is physically mounted: ' + ACaption);
  TControlAccess(LButton).Click;
end;
{$endif}

function People: TNyxCollectionRef;
begin
  Result := NyxCollection('people');
end;

function PeopleSchema: TNyxCollectionSchema;
begin
  Result := NyxCollectionSchema.Text(NyxTextField('name'), '')
    .Integer(NyxIntegerField('age'), 0).Number(NyxNumberField('score'), 0)
    .Boolean(NyxBooleanField('active'), False).Text(NyxTextField('notes'), '');
end;

function InitialSpec: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(People).Column(NyxTextField('name'), 'Name')
    .ColumnQuery(NyxColumnQuery.Operators([nqcEqual, nqcContains])
      .TextComparison(nqtAsciiInsensitive))
    .Column(NyxIntegerField('age'), 'Age')
    .ColumnQuery(NyxColumnQuery.Operators([nqcEqual, nqcAtLeast]))
    .Column(NyxNumberField('score'), 'Score')
    .ColumnQuery(NyxColumnQuery.Sorting(False))
    .Column(NyxBooleanField('active'), 'Active')
    .ColumnQuery(NyxColumnQuery.Filtering(False).Operators([nqcEqual]))
    .Scoped(csInstance).Selection(nsmMultiple)
    .Query(NyxCollectionQuery.OrderBy(NyxTextField('name'), nsdDescending))
    .Paged(NyxCollectionPaging.PageSize(2))
    .TableQuery(NyxTableQuery.SortMode(nqsmSingle));
end;

function Fixture: TNyxDocument;
var
  LTable: INyxTable;
  LInstance: INyxComponent;
  LPage: INyxPage;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Typed query controls authoring';
    Result.Collections.Define(People, PeopleSchema, [
      NyxCollectionItem(NyxItem(People, 'ada')).WithValue(NyxTextField('name'), 'Ada')
        .WithValue(NyxIntegerField('age'), 36).WithValue(NyxNumberField('score'), 9.5)
        .WithValue(NyxBooleanField('active'), True),
      NyxCollectionItem(NyxItem(People, 'grace')).WithValue(NyxTextField('name'), 'Grace')
        .WithValue(NyxIntegerField('age'), 40).WithValue(NyxNumberField('score'), 8.25)
        .WithValue(NyxBooleanField('active'), False)]);
    LTable := NewNyxTable(CDefinition);
    LTable.Configure.Height(200).Done;
    LTable.Binds.Collection(InitialSpec);
    Result.AddComponent(LTable);
    LPage := NewNyxPage('home');
    LInstance := NewNyxComponent(CFirst);
    LInstance.Reference := NyxComponent(CDefinition);
    LPage.Add(LInstance);
    LInstance := NewNyxComponent(CSecond);
    LInstance.Reference := NyxComponent(CDefinition);
    LPage.Add(LInstance);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function FixturePair: TNyxProjectPair;
var
  LDocument: TNyxDocument;
begin
  LDocument := Fixture;
  try
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument, 'nyx.fixture.table.query.studio'));
  finally
    LDocument.Free;
  end;
end;

function Intent(AAction: TNyxStudioCollectionAction;
  const ASpec: TNyxCollectionViewSpec): TNyxStudioCollectionIntent;
begin
  Result := Default(TNyxStudioCollectionIntent);
  Result.Action := AAction;
  Result.Key := People;
  Result.Projection := cpTable;

  if AAction in [scaQuery, scaTableQuery, scaColumnQuery,
    scaDefaultTableQuery, scaDefaultColumnQuery] then
  begin
    Result.QueryBaseline := NyxQueryEditorBaseline(PeopleSchema, ASpec);
  end;
end;

function HasField(const AData: TNyxDataValue; const AName: TNyxText): Boolean;
var LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
end;

procedure RefuseIntent(const AIntent: TNyxStudioCollectionIntent;
  const ASpec: TNyxCollectionViewSpec; const AReason: TNyxText);
var
  LRefused: Boolean;
  LResult: TNyxCollectionViewSpec;
begin
  LRefused := False;
  try
    LResult := ApplyNyxStudioCollectionViewIntent(AIntent, ASpec, PeopleSchema);
    LResult.Validate;
  except
    on E: ENyxCollection do LRefused := True;
    on E: EArgumentException do LRefused := True;
  end;
  Check(LRefused, AReason);
end;

{ Pure descriptor/candidate checks accompany, rather than replace, the physical
  Studio journey below. These use the same immutable spec and public patch API. }
procedure DescriptorCases;
var
  LSpec, LChanged, LDefault, LAdded: TNyxCollectionViewSpec;
  LIntent, LRead: TNyxStudioCollectionIntent;
  LData, LDiscovery, LBranches, LProperties, LVersion: TNyxDataValue;
  LPatch, LReadPatch: INyxCollectionPatch;
  LPair, LCandidate, LResolved: TNyxProjectPair;
  LDoc: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LIndex, LBranch, LFound, LCount: Integer;
  LAction: TNyxStudioCollectionAction;
begin
  LSpec := InitialSpec;
  Check(LSpec.ToData.Field('version').AsInteger = 7,
    'explicit policies use the saved v7 descriptor');
  Check(TNyxCollectionViewSpec.FromData(LSpec.ToData).ToData.ToJSON = LSpec.ToData.ToJSON,
    'table and four scalar column policies survive strict descriptor roundtrip');
  LIntent := Intent(scaTitle, LSpec);
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
  LIntent.Value := 'Display name';
  Check(LIntent.ToData.Count = 11, 'former title intent retains eleven fields');
  LChanged := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, PeopleSchema);
  Check(LChanged.TableQueryPolicy.ToData.ToJSON = LSpec.TableQueryPolicy.ToData.ToJSON,
    'unrelated title reconstruction preserves explicit table policy');
  for LIndex := 0 to LSpec.Count - 1 do
  begin
    Check(LChanged.ColumnAt(LIndex).HasQueryPolicy and
      (LChanged.ColumnAt(LIndex).QueryPolicy.ToData.ToJSON =
      LSpec.ColumnAt(LIndex).QueryPolicy.ToData.ToJSON),
      'unrelated title reconstruction preserves each existing column policy');
  end;
  LIntent := Intent(scaMode, LSpec);
  LIntent.Field := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('age'));
  LIntent.Mode := cmEditable;
  LChanged := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, PeopleSchema);
  Check((LChanged.ColumnAt(1).Mode = cmEditable) and LChanged.HasTableQueryPolicy and
    LChanged.ColumnAt(1).HasQueryPolicy, 'editable mode does not clear query capability choices');
  LIntent := Intent(scaScope, LSpec);
  LIntent.Scope := csApplication;
  LChanged := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, PeopleSchema);
  Check((LChanged.Scope = csApplication) and LChanged.ColumnAt(0).HasQueryPolicy and
    LChanged.HasTableQueryPolicy, 'scope change retains raw policy declarations');
  LIntent := Intent(scaColumnFixed, LSpec);
  LIntent.Field := TNyxStudioCollectionFieldRef.Number(NyxNumberField('score'));
  LIntent.Value := '120';
  LChanged := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, PeopleSchema);
  Check(LChanged.ColumnAt(2).Sizing.Defined and LChanged.ColumnAt(2).HasQueryPolicy and
    LChanged.HasTableQueryPolicy, 'column sizing changes retain policies');
  LIntent := Intent(scaAddColumn, LSpec);
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('notes'));
  LAdded := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, PeopleSchema);
  Check((LAdded.Count = 5) and not LAdded.ColumnAt(4).HasQueryPolicy and
    LAdded.ColumnAt(0).HasQueryPolicy and LAdded.HasTableQueryPolicy,
    'new column uses defaults while existing raw choices survive');
  LIntent := Intent(scaRemoveColumn, LAdded);
  LIntent.Field := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('age'));
  LChanged := ApplyNyxStudioCollectionViewIntent(LIntent, LAdded, PeopleSchema);
  Check((LChanged.Count = 4) and (LChanged.ColumnAt(1).FieldName = 'score') and
    LChanged.ColumnAt(1).HasQueryPolicy and LChanged.HasTableQueryPolicy,
    'removing a column preserves surviving field policies at their new positions');
  LIntent := Intent(scaQuery, LSpec);
  LIntent.Query := NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('age')).AtLeast(18));
  Check(LIntent.ToData.Count = 13, 'former query intent retains thirteen fields');
  for LAction := scaTableQuery to scaDefaultColumnQuery do
  begin
    LIntent := Intent(LAction, LSpec);

    if LAction in [scaColumnQuery, scaDefaultColumnQuery] then
    begin
      LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
    end;

    if LAction = scaTableQuery then
    begin
      LIntent.TableQuery := NyxTableQuery.Filtering(False);
    end;

    if LAction = scaColumnQuery then
    begin
      LIntent.ColumnQuery := NyxColumnQuery.Sorting(False);
    end;
    LCount := 12;

    if LAction in [scaTableQuery, scaColumnQuery] then
    begin
      LCount := 13;
    end;
    Check(LIntent.ToData.Count = LCount, 'policy wire contains only its action-specific payload');
    LRead := TNyxStudioCollectionIntent.FromData(LIntent.ToData);
    Check(LIntent.SameIntent(LRead), 'four typed actions retain closed values and exact baseline');
    Check(NyxStudioCollectionStructuralAction(LAction), 'all policy actions hold structural pending locks');
  end;
  LIntent := Intent(scaTableQuery, LSpec);
  RefuseIntent(LIntent, LSpec, 'undefined table setter refuses');
  LIntent := Intent(scaColumnQuery, LSpec);
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
  RefuseIntent(LIntent, LSpec, 'undefined column setter refuses');
  LIntent := Intent(scaDefaultTableQuery, LSpec);
  LIntent.TableQuery := NyxTableQuery;
  RefuseIntent(LIntent, LSpec, 'default command refuses an unused table policy');
  LIntent := Intent(scaDefaultColumnQuery, LSpec);
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
  LIntent.ColumnQuery := NyxColumnQuery;
  RefuseIntent(LIntent, LSpec, 'default command refuses an unused column policy');
  LIntent := Intent(scaTableQuery, LSpec);
  LIntent.TableQuery := NyxTableQuery;
  LIntent.Projection := cpList;
  RefuseIntent(LIntent, LSpec, 'table policy cannot be retargeted to a list');
  LIntent := Intent(scaColumnQuery, LSpec);
  LIntent.ColumnQuery := NyxColumnQuery;
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('notes'));
  RefuseIntent(LIntent, LSpec, 'policy cannot append a missing column');
  LIntent.Field := TNyxStudioCollectionFieldRef.Number(NyxNumberField('age'));
  RefuseIntent(LIntent, LSpec, 'changed field family refuses');
  LIntent := Intent(scaTableQuery, LSpec);
  LIntent.TableQuery := NyxTableQuery;
  RefuseIntent(LIntent, LSpec.TableQuery(NyxTableQuery.Enabled(False)),
    'stale full-spec baseline refuses even when the field schema is unchanged');
  LIntent := Intent(scaDefaultTableQuery, LSpec);
  LDefault := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, PeopleSchema);
  Check(not LDefault.HasTableQueryPolicy and LDefault.ColumnAt(0).HasQueryPolicy,
    'table defaults clear only the raw table declaration');
  LIntent := Intent(scaDefaultColumnQuery, LDefault);
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
  LDefault := ApplyNyxStudioCollectionViewIntent(LIntent, LDefault, PeopleSchema);
  Check(not LDefault.ColumnAt(0).HasQueryPolicy and LDefault.ColumnAt(1).HasQueryPolicy,
    'column defaults clear only the chosen raw column declaration');
  LDefault := NyxCollectionView(People).Column(NyxTextField('name'), 'Name');
  LIntent := Intent(scaTitle, LDefault);
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
  LIntent.Value := 'Renamed';
  LChanged := ApplyNyxStudioCollectionViewIntent(LIntent, LDefault, PeopleSchema);
  Check(not LChanged.HasQueryControls and (LChanged.ToData.Field('version').AsInteger = 1),
    'unrelated edits never materialize resolved defaults or upgrade old descriptor bytes');

  LPair := FixturePair;
  LIntent := Intent(scaTableQuery, LSpec);
  LIntent.TableQuery := NyxTableQuery.Filtering(False).SortMode(nqsmMultiple);
  LPatch := NyxCollectionPatch([NyxCollectionIntentChange(NyxBindingOwner(CFirst), LIntent)]);
  LReadPatch := ReadNyxCollectionPatch(LPatch.ToData);
  Check(LReadPatch.ToData.ToJSON = LPatch.ToData.ToJSON,
    'semantic patch advertises the same typed action/baseline as ordinary authoring');
  LCandidate := LReadPatch.Candidate(LPair);
  LDoc := nil;
  LWorkspace := nil;
  try
    AdmitNyxProject(LCandidate, nprRequireMatch, LDoc, LWorkspace, LResolved);
    Check(LDoc.Find(CFirst).HasCollectionView and
      not LDoc.Find(CFirst).CollectionView.TableQueryPolicy.FilterEnabled,
      'semantic patch has a matching local binding and generated Pascal');
    Check(not LDoc.Find(CSecond).HasCollectionView and
      LDoc.Find(CDefinition).CollectionView.TableQueryPolicy.FilterEnabled,
      'semantic patch preserves definition and sibling ownership');
    Check(Pos('NyxTableQuery', LCandidate.Source) > 0,
      'policy candidate uses a typed source factory');
  finally
    LWorkspace.Free;
    LDoc.Free;
  end;

  LDiscovery := NyxCollectionAgentSchema;
  LBranches := LDiscovery.Field('oneOf');
  LBranches := LBranches.Item(LBranches.Count - 1).Field('properties')
    .Field('changes').Field('items').Field('oneOf');
  LFound := 0;
  for LBranch := 0 to LBranches.Count - 1 do
  begin
    LProperties := LBranches.Item(LBranch).Field('properties');

    if HasField(LProperties.Field('op'), 'const') and
      (LProperties.Field('op').Field('const').AsText = 'bind') then
    begin
      LData := LProperties.Field('spec').Field('oneOf');
      Check(LData.Count = 12,
        'semantic discovery retains all former versions plus column alignment v12');
      LVersion := LData.Item(6);
      Check((LVersion.Field('properties').Field('version').Field('const').AsInteger = 7) and
        (LVersion.Field('required').Count = 10) and
        not LVersion.Field('additionalProperties').AsBoolean,
        'v7 discovery is closed and requires the complete ten-field root');
      LData := LVersion.Field('properties').Field('columns').Field('items');
      Check((LData.Field('required').Count = 6) and
        (LData.Field('properties').Field('sizing').Field('anyOf').Item(0)
          .Field('type').AsText = 'null') and
        (LData.Field('properties').Field('queryControls').Field('anyOf').Item(0)
          .Field('type').AsText = 'null'),
        'v7 six-field columns independently admit absent sizing and query choices');
      Check(LVersion.Field('anyOf').Item(1).Field('properties').Field('columns')
        .Field('contains').Field('properties').Field('queryControls')
        .Field('type').AsText = 'object',
        'all-null column choices cannot falsely promote a descriptor to v7');
      Inc(LFound);
    end;
    for LIndex := 0 to LProperties.Count - 1 do
    begin

      if LProperties.Key(LIndex) = 'action' then
      begin
        LData := LProperties.Field('action').Field('const');

        if (LData.AsText = 'table-query') or (LData.AsText = 'column-query') or
          (LData.AsText = 'default-table-query') or (LData.AsText = 'default-column-query') then
        begin
          Check((LProperties.Field('projection').Field('const').AsText = 'table') and
            (LProperties.Field('baseline').Field('minLength').AsInteger = 1) and
            not LBranches.Item(LBranch).Field('additionalProperties').AsBoolean,
            'each semantic policy action requires exact table/baseline and closed fields');
          Inc(LFound);
        end;
      end;
    end;
  end;
  Check(LFound = 5, 'semantic discovery contains v7 binding and all four policy actions');
end;

{ Malformed, stale, pending and mismatched-owner checks operate on an isolated
  ordinary panel. Mutations below deliberately corrupt that test panel to prove
  refusal; successful policy choices use real mounted inputs in TReview. }
procedure AdmissionCases;
var
  LSession: TNyxStudioSession;
  LPanel, LProjection, LForm, LRole: TNyxNode;
  LEdit: TNyxStudioDesignEdit;
  LPending: TNyxStudioPendingDesign;
  LBefore, LValue: TNyxText;
  LIndex: Integer;
  LAction: TNyxStudioCollectionAction;
  LIntent, LRead: TNyxStudioCollectionIntent;
  LFields: array of TNyxDataField;
  LData: TNyxDataValue;
  LRefused: Boolean;

  procedure Panel;
  begin
    FreeAndNil(LPanel);
    LPanel := TNyxNode.Create(nkColumn, 'test-inspector');
    LProjection := LSession.SelectedProjection;
    try
      AddNyxCollectionBindingPanel(LPanel, LSession, LProjection);
    finally
      LProjection.Free;
    end;
  end;

  procedure Refused(AColumn: Integer; const AReason: TNyxText);
  var LFailed: Boolean;
  begin
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LFailed := False;
    try
      CaptureNyxStudioQuery(LSession, LPanel.Find(NyxStudioQueryPolicyApplyID(AColumn)),
        LPanel, LPending, LEdit);
    except
      on E: ENyxCollection do LFailed := True;
      on E: EArgumentException do LFailed := True;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore), AReason);
  end;

begin
  LPanel := nil;
  LSession := TNyxStudioSession.Create(FixturePair);
  try
    LSession.Activate('home');
    LSession.Select(CFirst);
    LPending := Default(TNyxStudioPendingDesign);
    Panel;
    Check(CaptureNyxStudioQuery(LSession, LPanel.Find(NyxStudioQueryPolicyApplyID(-1)),
      LPanel, LPending, LEdit) and (LEdit.Action = sdaCollection) and
      (LEdit.Selection = CFirst) and (LEdit.View = 'home') and
      (LEdit.Collection.Action = scaTableQuery),
      'complete ordinary form captures one existing paired collection edit');
    SetLength(LPending.Collections, 1);
    LPending.Collections[0].Owner := CFirst;
    LPending.Collections[0].Intent := LEdit.Collection;
    Refused(-1, 'pending structural policy blocks duplicate pre-paint capture');
    LPending := Default(TNyxStudioPendingDesign);
    LSession.Select(CSecond);
    Refused(-1, 'old first-owner form cannot target a newly selected sibling');
    LSession.Select(CFirst);
    LSession.SetCollectionView(InitialSpec.TableQuery(NyxTableQuery.Enabled(False)));
    Refused(-1, 'copied baseline refuses a changed complete binding');
    LSession.Undo;
    Panel;
    LForm := LPanel.Find(NyxStudioQueryPolicyFormID(0));
    LValue := LForm.Prop(NyxStudioCollectionKindKey);
    LForm.SetProp(NyxStudioCollectionKindKey, 'number');
    Refused(0, 'same positional column cannot impersonate another scalar family');
    LForm.SetProp(NyxStudioCollectionKindKey, LValue);
    LRole := LPanel.Find(NyxStudioQueryPolicyApplyID(0));
    LRole.Configure.ProjectAs(nkLabel).Done;
    Refused(0, 'projected button role cannot authorize whole-form Apply');
    LRole.Configure.ProjectAs(nkButton).Done;
    LForm.Add(TNyxNode.Create(nkCheckbox, 'extra-policy-role').Configure.Value(True).Done);
    Refused(0, 'unexpected form member refuses before producing an intent');
    Panel;
    LSession.SetSourceDraft(LSession.Source + #10 + '{ parked handwritten draft }');
    Refused(-1, 'parked Pascal draft prevents form capture without rewriting either side');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    for LAction := scaTableQuery to scaDefaultColumnQuery do
    begin
      LIntent := Intent(LAction, InitialSpec);

      if LAction in [scaColumnQuery, scaDefaultColumnQuery] then
      begin
        LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
      end;

      if LAction = scaTableQuery then
      begin
        LIntent.TableQuery := NyxTableQuery;
      end;

      if LAction = scaColumnQuery then
      begin
        LIntent.ColumnQuery := NyxColumnQuery;
      end;
      LRefused := False;
      try
        LSession.ApplyCollectionIntent(LIntent);
      except
        on E: ENyxCollection do LRefused := True;
        on E: EArgumentException do LRefused := True;
      end;
      Check(LRefused and LSession.SourceDraftPending and
        (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
        'each replay action refuses pending source before any paired mutation');
    end;
    LSession.DiscardSourceDraft;
    LIntent := Intent(scaTableQuery, InitialSpec);
    LIntent.TableQuery := NyxTableQuery;
    LData := LIntent.ToData;
    SetLength(LFields, LData.Count + 1);
    for LIndex := 0 to LData.Count - 1 do
    begin
      LFields[LIndex] := NyxField(LData.Key(LIndex), LData.Field(LData.Key(LIndex)));
    end;
    {$ifdef PAS2JS}
    { Count is a zero-argument typed-data method. Its explicit invocation keeps
      the browser array index a number instead of a method reference; the same
      unexpected member still exercises the unchanged strict refusal below. }
    LFields[LData.Count()] := NyxField('unexpected', NyxData(True));
    {$else}
    LFields[LData.Count] := NyxField('unexpected', NyxData(True));
    {$endif}
    LRefused := False;
    try
      LRead := TNyxStudioCollectionIntent.FromData(NyxObject(LFields));
      LRead.Validate;
    except
      on E: ENyxCollection do LRefused := True;
      on E: EArgumentException do LRefused := True;
    end;
    Check(LRefused, 'private intent decoder rejects extra payload');
  finally
    LPanel.Free;
    LSession.Free;
  end;
end;

{$ifndef PAS2JS}
procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Text := TNyxText(AException.Message);
end;
{$endif}

constructor TReview.Create;
begin
  inherited Create;
  Trace('constructor:begin', FStage);
  GFiles := TProjectFiles.Create;
  GFileLease := GFiles;
  {$ifdef PAS2JS}
  Trace('constructor:browser-session:before', FStage);
  { Inspection is a detached copy, never the editor's mutation owner. It is
    replaced only after full Studio acknowledges its actual accepted frame. }
  FSession := TNyxStudioSession.Create(FixturePair);
  Trace('constructor:browser-session:after', FStage);
  Trace('constructor:activate-select:before', FStage);
  FSession.Activate('home');
  FSession.Select(CFirst);
  Trace('constructor:activate-select:after', FStage);
  GCore := TNyxAgentSession.Create(FixturePair);
  GCore.Call('nyx_select', 'Query workshop', NyxObject([
    NyxField('expectedRevision', NyxData(GCore.Revision)),
    NyxField('operationId', NyxData('query-activate-home')),
    NyxField('id', NyxData('home')), NyxField('activate', NyxData(True))]), 'query-workshop');
  GCore.Call('nyx_select', 'Query workshop', NyxObject([
    NyxField('expectedRevision', NyxData(GCore.Revision)),
    NyxField('operationId', NyxData('query-select-first')),
    NyxField('id', NyxData(CFirst))]), 'query-workshop');
  FStudio := TWorkshopStudio.Create;
  FStudio.Run(False);
  FStudio.ConnectAgents;
  FRenderer := FStudio.ShellView;
  FCommands := FStudio.SourceCommands;
  Trace('constructor:browser-refresh:before', FStage);
  Trace('constructor:browser-refresh:after', FStage);
  {$else}
  Trace('constructor:native-host:before', FStage);
  FFailure := TFailureObserver.Create;
  Application.OnException := FFailure.Failed;
  FHost := TForm.CreateNew(nil);
  FHost.ClientWidth := GWidth;
  FHost.ClientHeight := 900;
  FHost.Show;
  Check(FHost.ClientWidth = GWidth, 'ordinary Studio actual client width equals requested allocation');
  WriteLn('CLIENT_WIDTH=', FHost.ClientWidth);
  Trace('constructor:native-host:after', FStage);
  Trace('constructor:studio-create:before', FStage);
  FStudio := TWorkshopStudio.Create(FHost, IncludeTrailingPathDelimiter(GEvidence) + 'studio-projects');
  Trace('constructor:studio-create:after', FStage);
  Trace('constructor:fixture-and-load-project:before', FStage);
  FStudio.LoadProject(FixturePair);
  Trace('constructor:fixture-and-load-project:after', FStage);
  FSession := FStudio.Session;
  Trace('constructor:activate-select:before', FStage);
  FSession.Activate('home');
  FSession.Select(CFirst);
  Trace('constructor:activate-select:after', FStage);
  Trace('constructor:studio-run:before', FStage);
  FStudio.Run;
  Trace('constructor:studio-run:after', FStage);
  FRenderer := FStudio.ShellView;
  FCommands := FStudio.SourceCommands;
  Trace('constructor:initial-ready:before', FStage);
  Ready;
  Trace('constructor:initial-ready:after', FStage);

  if FRenderer.RootFor('action-panel-inspector') <> nil then
  begin
    Trace('constructor:inspector:before', FStage);
    Click('action-panel-inspector');
    Trace('constructor:inspector:after', FStage);
  end;
  Trace('constructor:bindings:before', FStage);
  Click(NyxStudioBindingsToggleID);
  Trace('constructor:bindings:after', FStage);
  {$endif}
  Trace('constructor:baseline-snapshots:before', FStage);
  FDefinition := FSession.Document.Find(CDefinition).CollectionView.ToData.ToJSON;
  FSibling := TNyxCodec.NodeSnapshot(FSession.Document.Find(CSecond)).ToJSON;
  FDefaults := EncodeNyxCollectionDefaults(FSession.Document.Collections);
  Trace('constructor:baseline-snapshots:after', FStage);
  Trace('constructor:end', FStage);
end;

destructor TReview.Destroy;
begin
  {$ifdef PAS2JS}
  try
    FStudio.Free;
  finally
    GExchanges := nil;
  end;
  FreeAndNil(GCore);
  FSession.Free;
  {$else}
  FStudio.Free;
  FHost.Free;
  Application.OnException := nil;
  FFailure.Free;
  {$endif}
  GFileLease := nil;
  GFiles := nil;
  inherited Destroy;
end;

procedure TReview.Ready;
{$ifndef PAS2JS}var LStarted: QWord;{$endif}
begin
  Trace('ready:before', FStage);
  {$ifndef PAS2JS}
  LStarted := GetTickCount64;
  repeat
    Application.ProcessMessages;
    CheckSynchronize;

    if FFailure.Text <> '' then
    begin
      raise Exception.Create(FFailure.Text);
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Ordinary policy authoring did not finish: ' + FStudio.Status);
    end;
    Sleep(1);
  until not FStudio.PresentationPending and not FCommands.Busy;
  {$endif}
  Trace('ready:after', FStage);
end;

procedure TReview.Refresh; {$ifdef PAS2JS}async;{$endif}
begin
  {$ifdef PAS2JS}
  { Full Studio owns draft capture/replacement. These ordinary tabs exercise
    the former manual-render checkpoint without replacing its state owner. }
  Click(NyxInspectorEventsID);
  await(AwaitStudio);
  Click(NyxInspectorPropertiesID);
  await(AwaitStudio);
  {$else}
  Ready;
  {$endif}
end;

{$ifdef PAS2JS}
procedure TReview.InspectAccepted;
var
  LState: TNyxDataValue;
  LInspection: TNyxStudioSession;
begin
  { Public semantic reads intentionally record operator activity. This copied
    test inspection uses the existing neutral editor observation instead;
    FStudio.ProjectSnapshot below still owns the accepted paired state. }
  LState := GCore.Exchange(NyxObject([
    NyxField('op', NyxData('observe')),
    NyxField('after', NyxData(GCore.Revision))])).Field('session');
  LInspection := TNyxStudioSession.Create(FStudio.ProjectSnapshot);
  try
    LInspection.Activate(LState.Field('view').AsText);
    LInspection.Select(LState.Field('selection').AsText);
    FSession.Free;
    FSession := LInspection;
    LInspection := nil;
  finally
    LInspection.Free;
  end;
end;

procedure TReview.AwaitStudio; async;
begin
  repeat
    ServiceExchange;
    Inc(GPolls);

    if GPolls > 2400 then
    begin
      raise Exception.Create('Policy authoring queue exceeded its completion bound');
    end;
    await(UITurn);

    if GClosing then
    begin
      raise Exception.Create('Query Studio page retired during its awaited operation');
    end;
  until not FStudio.SourceBusy and not FStudio.PresentationPending and
    FStudio.SharedSourceSynchronized;
  InspectAccepted;
end;
{$endif}

function TReview.Pair: TNyxText;
begin
  Result := EncodeNyxProject(FStudio.ProjectSnapshot);
end;

function TReview.Root: TNyxNode;
begin
  Result := FRenderer.RootFor(CEditor);
end;

function TReview.Spec: TNyxCollectionViewSpec;
var LProjection: TNyxNode;
begin
  LProjection := FSession.SelectedProjection;
  try
    Result := LProjection.CollectionView;
  finally
    LProjection.Free;
  end;
end;

procedure TReview.Isolated;
begin
  Check(FSession.Document.Find(CDefinition).CollectionView.ToData.ToJSON = FDefinition,
    'policy edits leave the reusable definition exact');
  Check(TNyxCodec.NodeSnapshot(FSession.Document.Find(CSecond)).ToJSON = FSibling,
    'policy edits leave the independent sibling exact');
  Check(EncodeNyxCollectionDefaults(FSession.Document.Collections) = FDefaults,
    'query-control edits never mutate authored row/default data');
end;

procedure TReview.Change(const AID, AValue: TNyxText);
var
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  Trace('change:before:' + AID, FStage);
  LInput := FRenderer.InputFor(AID);
  Check(LInput <> nil, 'ordinary Inspector input is physically mounted: ' + AID);
  {$ifdef PAS2JS}

  if Root.Find(AID).ProjectionKind = NyxKindName(nkCheckbox) then
  begin
    TJSHTMLInputElement(LInput).checked := AValue = 'true';
  end
  else
  begin
    TJSHTMLInputElement(LInput).value := AValue;
  end;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}

  if LInput is TCheckBox then
  begin
    TCheckBox(LInput).Checked := AValue = 'true';
    TCheckBox(LInput).OnChange(LInput);
  end
  else if LInput is TComboBox then
  begin
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end
  else
  begin
    TEdit(LInput).Text := AValue;
    TEdit(LInput).OnChange(LInput);
  end;
  {$endif}
  Check(Root.Find(AID).Prop(NyxAttributeName(atValue)) = AValue,
    'normal input event retains the exact mounted policy value');
  Trace('change:after:' + AID, FStage);
end;

procedure TReview.Click(const AID: TNyxText);
begin
  Trace('click:before:' + AID, FStage);
  Check(FRenderer.Root.Find(AID) <> nil, 'ordinary Inspector action exists: ' + AID);
  FRenderer.ViewFor(AID).Reveal(AID, niRuntime);
  {$ifdef PAS2JS}
  FRenderer.ElementFor(AID).click;
  {$else}
  TControlAccess(FRenderer.ControlFor(AID)).Click;
  Ready;
  {$endif}
  Trace('click:after:' + AID, FStage);
end;

procedure TReview.Expand(AColumn: Integer);
var LSection, LHeader: TNyxNode;
begin
  LSection := Root.Find(NyxStudioQueryPolicyFormID(AColumn) + '-section');
  Check((LSection <> nil) and NyxIsDisclosure(LSection),
    'each column uses existing retained Disclosure presentation');

  if not NyxDisclosureExpanded(LSection) then
  begin
    LHeader := LSection.Part(NyxPart('header'));
    Click(LHeader.ID);
  end;
end;

procedure TReview.ExpandLayout(AColumn: Integer);
var
  LSection: TNyxNode;
  LHeader: TNyxNode;
begin
  LSection := Root.Find(NyxStudioLayoutPolicyFormID(AColumn) + '-section');
  Check((LSection <> nil) and NyxIsDisclosure(LSection),
    'column layout uses an independent ordinary disclosure');

  if not NyxDisclosureExpanded(LSection) then
  begin
    LHeader := LSection.Part(NyxPart('header'));
    Click(LHeader.ID);
  end;
end;

procedure TReview.History; {$ifdef PAS2JS}async;{$endif}
begin
  FAfter := Pair;
  Check(FAfter <> FBefore, 'whole-policy Apply changes the paired project');
  Click('action-undo');
  {$ifdef PAS2JS}await(AwaitStudio);{$endif}
  Check(Pair = FBefore, 'one ordinary Undo restores exact design and adjacent source');
  Click('action-redo');
  {$ifdef PAS2JS}await(AwaitStudio);{$endif}
  Check(Pair = FAfter, 'one ordinary Redo restores exact policy and source');
  Isolated;
end;

procedure TReview.Inspector;
begin

  if FRenderer.RootFor('action-panel-inspector') <> nil then
  begin
    Click('action-panel-inspector');
  end;
end;

function TReview.EventKey(AEvent: TNyxQueryWorkbenchEvent): TNyxText;
var
  LMetadata: TNyxEventSchemas;
  LIndex: Integer;
begin
  LMetadata := NyxEventsMetadata(FSession.Selected, FSession.Document);
  for LIndex := 0 to High(LMetadata) do
  begin

    if (LMetadata[LIndex].Trigger = ntNamed) and
      (LMetadata[LIndex].Name.Name = NyxQueryWorkbenchEvent(AEvent).Name) then
    begin
      Exit('event-' + NyxTriggerName(ntNamed) + '-' + TNyxText(IntToStr(LIndex)));
    end;
  end;
  raise ENyxModel.Create('Selected table query event metadata is absent');
end;

function TReview.AuthoredEvent(AEvent: TNyxQueryWorkbenchEvent): TNyxAuthoredEventInfo;
var
  LEvents: TNyxAuthoredEventInfos;
  LProjection: TNyxNode;
  LIndex: Integer;
begin
  Result := Default(TNyxAuthoredEventInfo);
  LProjection := FSession.SelectedProjection;
  try
    LEvents := NyxAuthoredEvents(LProjection);
    for LIndex := 0 to High(LEvents) do
    begin

      if (LEvents[LIndex].Trigger = ntNamed) and
        (LEvents[LIndex].Name.Name = NyxQueryWorkbenchEvent(AEvent).Name) then
      begin
        Exit(LEvents[LIndex]);
      end;
    end;
  finally
    LProjection.Free;
  end;
end;

procedure TReview.CheckSavedEvents;
var
  LDocument: TNyxDocument;
  LEvents: TNyxAuthoredEventInfos;
  LIndex: Integer;
  LBefore, LChanged: Boolean;
  LSource: TNyxText;
begin
  LDocument := TNyxCodec.Decode(FStudio.ProjectSnapshot.Design);
  try
    LEvents := NyxAuthoredEvents(LDocument.Find(CFirst));
    LBefore := False;
    LChanged := False;
    for LIndex := 0 to High(LEvents) do
    begin

      if LEvents[LIndex].Name.Name = NyxQueryWorkbenchEvent(nqweBeforeChange).Name then
      begin
        LBefore := (LEvents[LIndex].Policy = neSequential) and
          (Length(LEvents[LIndex].Callbacks) = 1) and
          (LEvents[LIndex].Callbacks[0].Handler.Name = FBeforeHandler) and
          (LEvents[LIndex].Callbacks[0].ID.Name = FBeforeRegistration);
      end;

      if LEvents[LIndex].Name.Name = NyxQueryWorkbenchEvent(nqweChanged).Name then
      begin
        LChanged := (LEvents[LIndex].Policy = neSequential) and
          (Length(LEvents[LIndex].Callbacks) = 1) and
          (LEvents[LIndex].Callbacks[0].Handler.Name = FChangedHandler) and
          (LEvents[LIndex].Callbacks[0].ID.Name = FChangedRegistration);
      end;
    end;
    LSource := FStudio.ProjectSnapshot.Source;
    Check(LBefore and LChanged and (FBeforeRegistration <> FChangedRegistration),
      'saved query streams retain exact event, policy, registration and handler identities');
    Check((Pos('NyxQueryWorkbenchEvent(nqweBeforeChange)', LSource) > 0) and
      (Pos('NyxQueryWorkbenchEvent(nqweChanged)', LSource) > 0) and
      (Pos('TODO: implement ' + FBeforeHandler + '.', LSource) > 0) and
      (Pos('TODO: implement ' + FChangedHandler + '.', LSource) > 0),
      'own Pascal retains both typed query factories and ordinary authored TODO implementations');
  finally
    LDocument.Free;
  end;
end;

procedure TReview.ProjectName;
var
  {$ifdef PAS2JS}LInput: TJSHTMLInputElement;{$else}LInput: TEdit;{$endif}
begin
  { This field belongs to Project, not the query-editor section. Resolve its
    fresh exact face instead of changing the generic policy capture root. }
  {$ifdef PAS2JS}
  LInput := TJSHTMLInputElement(FRenderer.InputFor('project-file-name'));
  LInput.value := 'table-query-workshop';
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LInput := TEdit(FRenderer.InputFor('project-file-name'));
  Check(LInput <> nil, 'named Save uses the actual Project name input');
  LInput.Text := 'table-query-workshop';
  LInput.OnChange(LInput);
  {$endif}
end;

procedure TReview.CaptureRefused(const AID: TNyxText);
var
  LEdit: TNyxStudioDesignEdit;
  LRefused: Boolean;
  LBefore: TNyxText;
begin
  LBefore := Pair;
  LRefused := False;
  try
    if not CaptureNyxStudioLayout(FSession, Root.Find(AID), Root,
      FCommands.PendingDesign, LEdit) then
    begin
      CaptureNyxStudioQuery(FSession, Root.Find(AID), Root,
        FCommands.PendingDesign, LEdit);
    end;
  except
    on E: ENyxCollection do LRefused := True;
    on E: EArgumentException do LRefused := True;
  end;
  Check(LRefused and (Pair = LBefore), 'invalid policy draft refuses without changing the accepted pair');
end;

{ Ordinary controls and SourceJobs own every mutation here. The selected table
  begins inherited; its reusable definition, peer and exact collection defaults
  remain independently checked. In the complete journey these choices enter
  the original Save/Open producer before its independently compiled companion. }
function TReview.WorkbenchStep: Boolean; {$ifdef PAS2JS}async;{$endif}
var
  LSection: TNyxNode;
  LExpected: TNyxTableWorkbenchPolicy;

  procedure OpenPolicy(const AFormID: TNyxText);
  var
    LDisclosure: TNyxNode;
  begin
    LDisclosure := Root.Find(AFormID + '-section');
    Check((LDisclosure <> nil) and NyxIsDisclosure(LDisclosure),
      'presentation uses an ordinary mounted Disclosure: ' + AFormID);

    if not NyxDisclosureExpanded(LDisclosure) then
    begin
      Click(LDisclosure.Part(NyxPart('header')).ID);
    end;
  end;

begin
  Result := False;
  LExpected := NyxTableWorkbench.RangeSelection(True)
    .AllowCopy([nwcaSelectedSource, nwcaSelectedPage, nwcaCellRange])
    .CopyFields(nwcfAllArrangement).Headers(False).LineEnding(nwleCRLF).FocusedDraft(True);
  Trace('workbench:before', FWorkbenchStage);
  case FWorkbenchStage of
    0:
      begin
        Inspector;
        {$ifdef PAS2JS}await(AwaitStudio);{$endif}
        Click(NyxInspectorPropertiesID);
        {$ifdef PAS2JS}await(AwaitStudio);{$endif}
        LSection := Root.Find(NyxStudioWorkbenchPolicyFormID + '-section');
        Check((LSection <> nil) and NyxIsDisclosure(LSection),
          'table actions use the ordinary independent Disclosure control');

        if not NyxDisclosureExpanded(LSection) then
        begin
          Click(LSection.Part(NyxPart('header')).ID);
        end;
        FWorkbenchInitial := Spec;
        FBefore := Pair;
        Change(NyxStudioWorkbenchPolicyFieldID(swpfRangeSelection), 'true');
        Change(NyxStudioWorkbenchPolicyFieldID(swpfCopyCell), 'false');
        Change(NyxStudioWorkbenchPolicyFieldID(swpfCopySelectedSource), 'true');
        Change(NyxStudioWorkbenchPolicyFieldID(swpfCopySelectedPage), 'true');
        Change(NyxStudioWorkbenchPolicyFieldID(swpfCopyCellRange), 'true');
        Change(NyxStudioWorkbenchPolicyFieldID(swpfCopyFields), 'All authored columns, including hidden');
        Change(NyxStudioWorkbenchPolicyFieldID(swpfHeaders), 'false');
        Change(NyxStudioWorkbenchPolicyFieldID(swpfLineEnding), 'CRLF');
        Change(NyxStudioWorkbenchPolicyFieldID(swpfFocusedDraft), 'true');
        Check(Pair = FBefore, 'actual action inputs stay drafts before the complete Apply');
        Click(NyxStudioWorkbenchPolicyApplyID);
      end;
    1:
      begin
        Check((FCommands.State = nssApplied) and Spec.HasTableWorkbenchPolicy and
          (Spec.TableWorkbenchPolicy.ToData.ToJSON = LExpected.ToData.ToJSON),
          'ordinary SourceJobs accepts the complete typed action form: ' + FCommands.Message);
        Check((Pos('.TableWorkbench(NyxTableWorkbench', FSession.Source) > 0) and
          (Pos('nyx.collections.workbench.policy', FSession.Source) > 0),
          'ordinary source generation promotes the typed workbench contract');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        FWorkbenchAccepted := Pair;
        LSection := Root.Find(NyxStudioWorkbenchPolicyFormID + '-section');

        if not NyxDisclosureExpanded(LSection) then
        begin
          Click(LSection.Part(NyxPart('header')).ID);
        end;
        Change(NyxStudioWorkbenchPolicyFieldID(swpfCopyCell), 'true');
        Click(NyxInspectorEventsID);
        {$ifdef PAS2JS}await(AwaitStudio);{$endif}
        Click(NyxInspectorPropertiesID);
        {$ifdef PAS2JS}await(AwaitStudio);{$endif}
        Check((Root.Find(NyxStudioWorkbenchPolicyFieldID(swpfCopyCell))
          .Prop(NyxAttributeName(atValue)) = 'true') and (Pair = FWorkbenchAccepted),
          'the actual Inspector retains an independent action draft without changing accepted Pascal');
        LSection := Root.Find(NyxStudioWorkbenchPolicyFormID + '-section');

        if not NyxDisclosureExpanded(LSection) then
        begin
          Click(LSection.Part(NyxPart('header')).ID);
        end;
        FBefore := Pair;
        Click(NyxStudioWorkbenchPolicyDefaultID);
      end;
    2:
      begin
        Check((FCommands.State = nssApplied) and not Spec.HasTableWorkbenchPolicy and
          (Spec.ToData.ToJSON = FWorkbenchInitial.UseDefaultTableWorkbench.ToData.ToJSON),
          'ordinary reset removes only workbench presence, preserving earlier table choices: ' +
          FCommands.Message);
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Click('action-undo');
        {$ifdef PAS2JS}await(AwaitStudio);{$endif}
      end;
    3:
      begin
        Check((Pair = FWorkbenchAccepted) and
          (Spec.TableWorkbenchPolicy.ToData.ToJSON = LExpected.ToData.ToJSON),
          'ordinary Undo restores the exact action policy/source pair for subsequent Save/Open');
        Isolated;
        FBefore := Pair;
        OpenPolicy(NyxStudioPresentationPolicyFormID(-1));
        Change(NyxStudioPresentationPolicyFieldID(-1, sppfDensity), 'Comfortable');
        Check(Pair = FBefore, 'mounted spacing input is a draft until ordinary Apply');
        Click(NyxStudioPresentationPolicyApplyID(-1));
      end;
    4:
      begin
        Check((FCommands.State = nssApplied) and Spec.HasTablePresentationPolicy and
          (Spec.TablePresentationPolicy.RowDensity = ntrdComfortable) and
          (Pos('.TablePresentation(NyxTablePresentation.Density(ntrdComfortable))',
            FSession.Source) > 0),
          'ordinary spacing Apply produces typed adjacent Pascal: ' + FCommands.Message);
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        FBefore := Pair;
        OpenPolicy(NyxStudioPresentationPolicyFormID(0));
        Change(NyxStudioPresentationPolicyFieldID(0, sppfTextOverflow), 'Wrap');
        Change(NyxStudioPresentationPolicyFieldID(0, sppfMaximumLines), '-');
        Click(NyxInspectorEventsID);
        {$ifdef PAS2JS}await(AwaitStudio);{$endif}
        Click(NyxInspectorPropertiesID);
        {$ifdef PAS2JS}await(AwaitStudio);{$endif}
        Check((Pair = FBefore) and
          (Root.Find(NyxStudioPresentationPolicyFieldID(0, sppfMaximumLines))
            .Prop(NyxAttributeName(atValue)) = '-') and
          (Root.Find(NyxStudioPresentationPolicyFieldID(0, sppfTextOverflow))
            .Prop(NyxAttributeName(atValue)) = 'Wrap'),
          'an unfinished wrap count survives the actual Inspector tab round trip');
        OpenPolicy(NyxStudioPresentationPolicyFormID(0));
        Change(NyxStudioPresentationPolicyFieldID(0, sppfMaximumLines), '4');
        Click(NyxStudioPresentationPolicyApplyID(0));
      end;
    5:
      begin
        Check((FCommands.State = nssApplied) and Spec.ColumnAt(0).HasTextPolicy and
          (Spec.ColumnAt(0).TextPolicy.Overflow = nctoWrap) and
          (Spec.ColumnAt(0).TextPolicy.MaximumLines = 4) and
          not Spec.ColumnAt(1).HasTextPolicy and
          (Spec.TablePresentationPolicy.RowDensity = ntrdComfortable) and
          (Pos('.ColumnText(NyxColumnText.Wrap(4))', FSession.Source) > 0),
          'ordinary wrap Apply preserves independent density, peer columns and typed source');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        FBefore := Pair;
        OpenPolicy(NyxStudioColumnAlignmentFormID(0));
        Change(NyxStudioColumnAlignmentFieldID(0, scafHorizontal), 'End');
        Change(NyxStudioColumnAlignmentFieldID(0, scafVertical), 'Middle');
        Check(Pair = FBefore, 'mounted alignment choices do not publish before Apply');
        Click(NyxStudioColumnAlignmentApplyID(0));
      end;
    6:
      begin
        Check((FCommands.State = nssApplied) and Spec.ColumnAt(0).HasAlignmentPolicy and
          (Spec.ColumnAt(0).AlignmentPolicy.HorizontalAlignment = nchaEnd) and
          (Spec.ColumnAt(0).AlignmentPolicy.VerticalAlignment = ncvaMiddle) and
          not Spec.ColumnAt(1).HasAlignmentPolicy and
          (Pos('.ColumnAlignment(NyxColumnAlignment.Horizontal(nchaEnd).Vertical(ncvaMiddle))',
            FSession.Source) > 0),
          'ordinary alignment Apply admits both typed axes and crafted adjacent Pascal');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        FPresentationAccepted := Pair;
        FBefore := Pair;
        OpenPolicy(NyxStudioColumnAlignmentFormID(0));
        Change(NyxStudioColumnAlignmentFieldID(0, scafHorizontal), 'Start');
        Click(NyxInspectorEventsID);
        {$ifdef PAS2JS}await(AwaitStudio);{$endif}
        Click(NyxInspectorPropertiesID);
        {$ifdef PAS2JS}await(AwaitStudio);{$endif}
        Check((Pair = FBefore) and
          (Root.Find(NyxStudioColumnAlignmentFieldID(0, scafHorizontal))
            .Prop(NyxAttributeName(atValue)) = 'Start'),
          'actual Inspector tabs retain an independent unsubmitted alignment draft');
        OpenPolicy(NyxStudioColumnAlignmentFormID(0));
        Click(NyxStudioColumnAlignmentDefaultID(0));
      end;
    7:
      begin
        Check((FCommands.State = nssApplied) and not Spec.ColumnAt(0).HasAlignmentPolicy and
          Spec.HasTablePresentationPolicy and Spec.ColumnAt(0).HasTextPolicy and
          (Spec.TableWorkbenchPolicy.ToData.ToJSON = LExpected.ToData.ToJSON),
          'ordinary alignment reset clears only alignment, ignoring its unsubmitted draft');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Click('action-undo');
        {$ifdef PAS2JS}await(AwaitStudio);{$endif}
      end;
    8:
      begin
        Check(Pair = FPresentationAccepted,
          'one ordinary Undo restores the complete pre-reset alignment/source pair');
        FBefore := Pair;
        OpenPolicy(NyxStudioPresentationPolicyFormID(0));
        Change(NyxStudioPresentationPolicyFieldID(0, sppfMaximumLines), '-');
        Click(NyxStudioPresentationPolicyDefaultID(0));
      end;
    9:
      begin
        Check((FCommands.State = nssApplied) and not Spec.ColumnAt(0).HasTextPolicy and
          Spec.ColumnAt(0).HasAlignmentPolicy and Spec.HasTablePresentationPolicy,
          'ordinary text reset ignores its partial count and retains alignment/spacing');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Click('action-undo');
        {$ifdef PAS2JS}await(AwaitStudio);{$endif}
      end;
    10:
      begin
        Check(Pair = FPresentationAccepted,
          'text reset Undo restores the same complete accepted presentation pair');
        FBefore := Pair;
        OpenPolicy(NyxStudioPresentationPolicyFormID(-1));
        Click(NyxStudioPresentationPolicyDefaultID(-1));
      end;
    11:
      begin
        Check((FCommands.State = nssApplied) and not Spec.HasTablePresentationPolicy and
          Spec.ColumnAt(0).HasAlignmentPolicy and Spec.ColumnAt(0).HasTextPolicy,
          'ordinary spacing reset preserves independent text layout and alignment');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Click('action-undo');
        {$ifdef PAS2JS}await(AwaitStudio);{$endif}
      end;
    12:
      begin
        Check((Pair = FPresentationAccepted) and
          (Spec.TableWorkbenchPolicy.ToData.ToJSON = LExpected.ToData.ToJSON),
          'the original Save/Open producer receives exact presentation and workbench policies');
        Isolated;
        Result := True;
        {$ifdef NYX_WORKBENCH_STUDIO_ONLY}
        GSaved := Pair;
        {$ifndef PAS2JS}
        WriteNyxTextFile(IncludeTrailingPathDelimiter(GEvidence) + 'accepted-project.json',
          NyxTextFile('accepted-project.json', GSaved));
        WriteNyxTextFile(IncludeTrailingPathDelimiter(GEvidence) + 'nyx.fixture.table.query.studio.pas',
          NyxTextFile('nyx.fixture.table.query.studio.pas', FSession.Source));
        {$endif}
        {$endif}
      end;
  end;
  Trace('workbench:after', FWorkbenchStage);
  Inc(FWorkbenchStage);
end;

{ Physical inspector input precedes this journey's original Save/Open/export.
  Its generated companion therefore consumes these exact accepted policies,
  with the existing query callbacks, handwritten Pascal and independent peer. }
function TReview.LayoutStep: Boolean; {$ifdef PAS2JS}async;{$endif}
var
  LFeature: TNyxColumnLayoutFeature;
  LIndex: Integer;
begin
  Result := False;
  case FLayoutStage of
    0:
      begin
        Inspector;
        {$ifdef PAS2JS}
        { Compact panel selection publishes asynchronously. Wait for the
          ordinary Studio fence before resolving its fresh Properties action;
          native Inspector already pumps Ready through Click. }
        await(AwaitStudio);
        {$endif}
        Click(NyxInspectorPropertiesID);
      end;
    1:
      begin
        Check(Root.Find(NyxStudioLayoutEditorID) <> nil,
          'bound table exposes its independent layout-policy inspector');
        FLayoutInitial := Spec;
        FBefore := Pair;
        Change(NyxStudioLayoutPolicyFieldID(-1, slpfMinimumCenter), '-');
        {$ifdef PAS2JS}await(Refresh);{$else}
        Click(NyxInspectorEventsID);
        Click(NyxInspectorPropertiesID);
        {$endif}
        Check((Root.Find(NyxStudioLayoutPolicyFieldID(-1, slpfMinimumCenter))
          .Prop(NyxAttributeName(atValue)) = '-') and (Pair = FBefore),
          'independent layout draft preserves incomplete numeric text through inspector replacement');
        CaptureRefused(NyxStudioLayoutPolicyApplyID(-1));
        Change(NyxStudioLayoutPolicyFieldID(-1, slpfEnabled), 'true');
        Change(NyxStudioLayoutPolicyFieldID(-1, slpfMinimumCenter), '112');
        Change(NyxStudioLayoutPolicyFieldID(-1, slpfAutoSizeRows), '32');
        for LFeature := Low(TNyxColumnLayoutFeature) to High(TNyxColumnLayoutFeature) do
        begin
          Change(NyxStudioLayoutPolicyFeatureID(-1, LFeature),
            LowerCase(BoolToStr(LFeature in [nclfResize, nclfAutoSize, nclfPinning], True)));
        end;
        Check(Pair = FBefore, 'layout field changes remain draft until whole-policy Apply');
        Click(NyxStudioLayoutPolicyApplyID(-1));
      end;
    2:
      begin
        Check((FCommands.State = nssApplied) and Spec.HasTableLayoutPolicy and
          Spec.TableLayoutPolicy.IsEnabled and
          (Spec.TableLayoutPolicy.MinimumScrollableWidth = 112) and
          (Spec.TableLayoutPolicy.AutoSizeRows = 32) and
          (Spec.TableLayoutPolicy.Features = [nclfResize, nclfAutoSize, nclfPinning]),
          'ordinary layout Apply admits the complete typed table policy: ' + FCommands.Message);
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Inspector;
        Click(NyxInspectorPropertiesID);
      end;
    3:
      begin
        ExpandLayout(0);
        FBefore := Pair;
        Change(NyxStudioLayoutPolicyFieldID(0, slpfVisible), 'true');
        Change(NyxStudioLayoutPolicyFieldID(0, slpfPin), 'Start edge');
        Click(NyxStudioLayoutPolicyApplyID(0));
      end;
    4:
      begin
        Check((FCommands.State = nssApplied) and Spec.ColumnAt(0).HasLayoutPolicy and
          Spec.ColumnAt(0).LayoutPolicy.IsVisible and (Spec.ColumnAt(0).LayoutPolicy.Pin = ncpStart),
          'ordinary column layout Apply retains a logical start pin');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Inspector;
        Click(NyxInspectorPropertiesID);
      end;
    5:
      begin
        ExpandLayout(0);
        FBefore := Pair;
        Click(NyxStudioLayoutPolicyDefaultID(0));
      end;
    6:
      begin
        Check((FCommands.State = nssApplied) and not Spec.ColumnAt(0).HasLayoutPolicy and
          Spec.HasTableLayoutPolicy and FSession.Selected.HasCollectionView,
          'Use Nyx defaults withdraws only the column declaration and keeps local table ownership');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Inspector;
        Click(NyxInspectorPropertiesID);
      end;
    7:
      begin
        ExpandLayout(0);
        Change(NyxStudioLayoutPolicyFieldID(0, slpfPin), 'Start edge');
        Click(NyxStudioLayoutPolicyApplyID(0));
      end;
    8:
      begin
        Check(Spec.ColumnAt(0).LayoutPolicy.Pin = ncpStart,
          'a fresh inspector command can restore the authored start pin');
        ExpandLayout(1);
        FBefore := Pair;
        Change(NyxStudioLayoutPolicyFieldID(1, slpfVisible), 'true');
        Change(NyxStudioLayoutPolicyFieldID(1, slpfPin), 'End edge');
        Click(NyxStudioLayoutPolicyApplyID(1));
      end;
    9:
      begin
        Check((FCommands.State = nssApplied) and Spec.ColumnAt(1).HasLayoutPolicy and
          (Spec.ColumnAt(0).LayoutPolicy.Pin = ncpStart) and
          (Spec.ColumnAt(1).LayoutPolicy.Pin = ncpEnd),
          'independent column forms author both logical pinned edges');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Inspector;
        Click(NyxInspectorPropertiesID);
      end;
    10:
      begin
        FBefore := Pair;
        Click(NyxStudioLayoutPolicyDefaultID(-1));
      end;
    11:
      begin
        Check((FCommands.State = nssApplied) and not Spec.HasTableLayoutPolicy and
          Spec.ColumnAt(0).HasLayoutPolicy and Spec.ColumnAt(1).HasLayoutPolicy,
          'table defaults preserve the two independently authored column policies');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Inspector;
        Click(NyxInspectorPropertiesID);
      end;
    12:
      begin
        FBefore := Pair;
        Change(NyxStudioLayoutPolicyFieldID(-1, slpfEnabled), 'true');
        Change(NyxStudioLayoutPolicyFieldID(-1, slpfMinimumCenter), '112');
        Change(NyxStudioLayoutPolicyFieldID(-1, slpfAutoSizeRows), '32');
        for LFeature := Low(TNyxColumnLayoutFeature) to High(TNyxColumnLayoutFeature) do
        begin
          Change(NyxStudioLayoutPolicyFeatureID(-1, LFeature),
            LowerCase(BoolToStr(LFeature in [nclfResize, nclfAutoSize, nclfPinning], True)));
        end;
        Click(NyxStudioLayoutPolicyApplyID(-1));
      end;
    13:
      begin
        Check((FCommands.State = nssApplied) and Spec.HasTableLayoutPolicy and
          Spec.TableLayoutPolicy.IsEnabled and
          (Spec.TableLayoutPolicy.MinimumScrollableWidth = 112) and
          (Spec.TableLayoutPolicy.AutoSizeRows = 32) and
          (Spec.TableLayoutPolicy.Features = [nclfResize, nclfAutoSize, nclfPinning]),
          'the exact saved project keeps enabled authored column management and its feature mask');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Check((Spec.QueryPolicy.ToData.ToJSON = FLayoutInitial.QueryPolicy.ToData.ToJSON) and
          (Spec.TableQueryPolicy.ToData.ToJSON = FLayoutInitial.TableQueryPolicy.ToData.ToJSON) and
          (Spec.PagingPolicy.ToData.ToJSON = FLayoutInitial.PagingPolicy.ToData.ToJSON),
          'layout policy admission preserves the complete accepted query and paging choices');
        for LIndex := 0 to Spec.Count - 1 do
        begin
          Check(Spec.ColumnAt(LIndex).QueryPolicy.ToData.ToJSON =
            FLayoutInitial.ColumnAt(LIndex).QueryPolicy.ToData.ToJSON,
            'layout defaults preserve every scalar column query policy');
        end;
        CheckSavedEvents;
        Isolated;
        Result := True;
      end;
  end;
  Inc(FLayoutStage);
end;

function TReview.Step: Boolean; {$ifdef PAS2JS}async;{$endif}
var
  LIndex: Integer;
  LComparison: TNyxQueryComparison;
  LFamily: TNyxQueryComparisons;
  LInfo: TNyxAuthoredEventInfo;
  {$ifndef PAS2JS}LStore: TNyxProjectStore; LRevision: TNyxText;{$endif}
begin
  Result := False;
  {$ifdef PAS2JS}Trace('step:readiness:before', FSetup);{$endif}

  if FCommands.Busy then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}

  if FStudio.SourceBusy or FStudio.PresentationPending or
    not FStudio.SharedSourceSynchronized then
  begin
    Exit;
  end;
  Trace('step:inspect:before', FSetup);
  InspectAccepted;
  Trace('step:inspect:after', FSetup);

  if FSetup < 2 then
  begin

    if FSetup = 0 then
    begin
      Inspector;
    end
    else
    begin
      Click(NyxStudioBindingsToggleID);
    end;
    Inc(FSetup);
    Exit;
  end;
  {$endif}
  {$ifdef NYX_WORKBENCH_STUDIO_ONLY}
  {$ifdef PAS2JS}Result := await(WorkbenchStep);{$else}Result := WorkbenchStep;{$endif}
  {$else}
  { Keep the original twenty-six query/event/file stages intact. Layout input
    finishes before their existing Save/Open captures the actual whole pair. }

  if (FStage = 18) and not FWorkbenchFinished then
  begin
    {$ifdef PAS2JS}FWorkbenchFinished := await(WorkbenchStep);{$else}
    FWorkbenchFinished := WorkbenchStep;
    {$endif}
    Exit;
  end;

  if (FStage = 18) and not FLayoutFinished then
  begin
    {$ifdef PAS2JS}FLayoutFinished := await(LayoutStep);{$else}
    FLayoutFinished := LayoutStep;
    {$endif}
    Exit;
  end;
  Trace('step:before', FStage);
  case FStage of
    0:
      begin
        Check(not FSession.Selected.HasCollectionView and Spec.HasTableQueryPolicy,
          'initial selected instance inherits its complete bound table');
        for LIndex := 0 to 3 do
        begin
          Check(not NyxDisclosureExpanded(Root.Find(
            NyxStudioQueryPolicyFormID(LIndex) + '-section')),
            'column operator forms start collapsed');
          LFamily := NyxQueryFamilyComparisons(Spec.ColumnAt(LIndex).Kind);
          for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
          begin
            Check((Root.Find(NyxStudioQueryPolicyOperatorID(LIndex, LComparison)) <> nil) =
              (LComparison in LFamily), 'each scalar form exposes exactly its portable operator family');
          end;
        end;
        FBefore := Pair;
        Change(NyxStudioQueryPolicyFieldID(-1, sqpfFiltering), 'false');
        Change(NyxStudioQueryPolicyFieldID(-1, sqpfSortMode), 'Multiple keys');
        Check(Pair = FBefore, 'live choices remain draft until whole-policy Apply');
        {$ifdef PAS2JS}await(Refresh);{$else}
        Click(NyxInspectorEventsID);
        Click(NyxInspectorPropertiesID);
        {$endif}
        Check((Root.Find(NyxStudioQueryPolicyFieldID(-1, sqpfFiltering))
          .Prop(NyxAttributeName(atValue)) = 'false') and (Pair = FBefore),
          'ordinary Inspector replacement preserves the unsubmitted policy draft');
        Click(NyxStudioQueryPolicyApplyID(-1));
      end;
    1:
      begin
        Check((FCommands.State = nssApplied) and FSession.Selected.HasCollectionView and
          not Spec.TableQueryPolicy.FilterEnabled and (Spec.TableQueryPolicy.Mode = nqsmMultiple),
          'ordinary Apply admits one complete local table policy: ' + FCommands.Message);
        Check((Pos('.TableQuery(', FSession.Source) > 0) and
          (Pos('NyxTableQuery', FSession.Source) > 0), 'adjacent Pascal expresses typed table policy');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Expand(0);
        FBefore := Pair;
        Change(NyxStudioQueryPolicyFieldID(0, sqpfSorting), 'false');
        Change(NyxStudioQueryPolicyFieldID(0, sqpfTextComparison), 'Exact text');
        Click(NyxStudioQueryPolicyApplyID(0));
      end;
    2:
      begin
        Check((FCommands.State = nssApplied) and not Spec.ColumnAt(0).QueryPolicy.SortEnabled and
          (Spec.ColumnAt(0).QueryPolicy.TextMode = nqtExact),
          'text column form applies sorting and text comparison together');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Expand(1);
        FBefore := Pair;
        Change(NyxStudioQueryPolicyFieldID(1, sqpfFamilyOperators), 'false');
        LFamily := NyxQueryFamilyComparisons(nskInteger);
        for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
        begin

          if LComparison in LFamily then
          begin
            Change(NyxStudioQueryPolicyOperatorID(1, LComparison), 'false');
          end;
        end;
        CaptureRefused(NyxStudioQueryPolicyApplyID(1));
        Check(Root.Find(NyxStudioQueryPolicyFieldID(1, sqpfFamilyOperators))
          .Prop(NyxAttributeName(atValue)) = 'false', 'invalid explicit-empty draft remains visible');
        {$ifdef PAS2JS}await(Refresh);{$else}
        Click(NyxStudioBindingsToggleID);
        Click(NyxStudioBindingsToggleID);
        {$endif}
        Expand(1);
        Check((Root.Find(NyxStudioQueryPolicyFieldID(1, sqpfFamilyOperators))
          .Prop(NyxAttributeName(atValue)) = 'false') and (Pair = FBefore),
          'hide/reopen retains invalid policy draft flags and the exact accepted pair');
        for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
        begin

          if LComparison in LFamily then
          begin
            Check(Root.Find(NyxStudioQueryPolicyOperatorID(1, LComparison))
              .Prop(NyxAttributeName(atValue)) = 'false',
              'hide/reopen retains each unsubmitted empty operator choice');
          end;
        end;
        Change(NyxStudioQueryPolicyOperatorID(1, nqcAtLeast), 'true');
        Click(NyxStudioQueryPolicyApplyID(1));
      end;
    3:
      begin
        Check((FCommands.State = nssApplied) and
          (Spec.ColumnAt(1).QueryPolicy.ComparisonCount = 1) and
          Spec.ColumnAt(1).QueryPolicy.Allows(nskInteger, nqcAtLeast),
          'integer form admits only the chosen compatible operator');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Expand(2);
        FBefore := Pair;
        Change(NyxStudioQueryPolicyFieldID(2, sqpfSorting), 'true');
        Click(NyxStudioQueryPolicyApplyID(2));
      end;
    4:
      begin
        Check((FCommands.State = nssApplied) and Spec.ColumnAt(2).QueryPolicy.SortEnabled,
          'number column form has independent capability choices');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Expand(3);
        Check(Root.Find(NyxStudioQueryPolicyOperatorID(3, nqcContains)) = nil,
          'Boolean form never advertises text-only operators');
        FBefore := Pair;
        Change(NyxStudioQueryPolicyFieldID(3, sqpfFiltering), 'true');
        Click(NyxStudioQueryPolicyApplyID(3));
      end;
    5:
      begin
        Check((FCommands.State = nssApplied) and Spec.ColumnAt(3).QueryPolicy.FilterEnabled and
          (Spec.ColumnAt(3).QueryPolicy.ComparisonCount = 1) and
          Spec.ColumnAt(3).QueryPolicy.Allows(nskBoolean, nqcEqual) and
          not Spec.ColumnAt(3).QueryPolicy.Allows(nskBoolean, nqcNotEqual),
          'enabling Boolean filtering retains its previously disabled explicit operator set');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        FBefore := Pair;
        Click(NyxStudioQueryPolicyDefaultID(-1));
      end;
    6:
      begin
        Check((FCommands.State = nssApplied) and not Spec.HasTableQueryPolicy and
          FSession.Selected.HasCollectionView and Spec.TableQueryPolicy.FilterEnabled and
          Spec.ColumnAt(0).HasQueryPolicy, 'library defaults remove only the local table choice');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Expand(0);
        FBefore := Pair;
        Click(NyxStudioQueryPolicyDefaultID(0));
      end;
    7:
      begin
        Check((FCommands.State = nssApplied) and not Spec.ColumnAt(0).HasQueryPolicy and
          (Spec.ColumnAt(0).QueryPolicy.TextMode = nqtExact) and
          Spec.ColumnAt(0).QueryPolicy.SortEnabled and Spec.ColumnAt(1).HasQueryPolicy,
          'column defaults mask inherited explicit choices with a local full spec');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        FBefore := Pair;
        Change('collection-column-0-title', 'Display name');
      end;
    8:
      begin
        Check((FCommands.State = nssApplied) and (Spec.ColumnAt(0).Title = 'Display name') and
          not Spec.HasTableQueryPolicy and not Spec.ColumnAt(0).HasQueryPolicy and
          Spec.ColumnAt(1).HasQueryPolicy, 'ordinary title edit preserves raw absence and other policies');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        FBefore := Pair;
        Change('collection-binding-scope', 'Application');
      end;
    9:
      begin
        Check((FCommands.State = nssApplied) and (Spec.Scope = csApplication) and
          Spec.ColumnAt(1).HasQueryPolicy, 'ordinary scope edit preserves surviving explicit policies');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        FBefore := Pair;
        Click('collection-binding-inherit');
      end;
    10:
      begin
        Check((FCommands.State = nssApplied) and not FSession.Selected.HasCollectionView and
          (Spec.ToData.ToJSON = InitialSpec.ToData.ToJSON),
          'whole-binding Inherit restores current definition independently of library defaults');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
      end;
    11:
      begin
        FBefore := Pair;
        Change(NyxStudioQueryPolicyFieldID(-1, sqpfSortMode), 'Multiple keys');
        Click(NyxStudioQueryPolicyApplyID(-1));
      end;
    12:
      begin
        Check(Spec.HasTableQueryPolicy and (Spec.TableQueryPolicy.Mode = nqsmMultiple),
          'saved first instance has a useful explicit multiple-key query policy');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Expand(3);
        FBefore := Pair;
        Change(NyxStudioQueryPolicyFieldID(3, sqpfFiltering), 'true');
        Click(NyxStudioQueryPolicyApplyID(3));
      end;
    13:
      begin
        Check(Spec.ColumnAt(3).QueryPolicy.FilterEnabled and
          (Spec.ColumnAt(3).QueryPolicy.ComparisonCount = 1) and
          Spec.ColumnAt(3).QueryPolicy.Allows(nskBoolean, nqcEqual),
          'saved first instance enables only the deliberately retained Boolean comparison');
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Inspector;
        Click(NyxInspectorEventsID);
      end;
    14:
      begin
        FBefore := Pair;
        Click(EventKey(nqweBeforeChange) + '-add');
      end;
    15:
      begin
        LInfo := AuthoredEvent(nqweBeforeChange);
        Check((LInfo.Policy = neSequential) and (Length(LInfo.Callbacks) = 1),
          'ordinary Events Add saves one sequential Before query registration');
        FBeforeHandler := LInfo.Callbacks[0].Handler.Name;
        FBeforeRegistration := LInfo.Callbacks[0].ID.Name;
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        Inspector;{$ifdef PAS2JS}
        { Browser Inspector navigation publishes after the current dispatch. }

        if FStudio.PresentationPending then
        begin
          await(AwaitStudio);
        end;{$endif}
        Click(NyxInspectorEventsID);
      end;
    16:
      begin
        FBefore := Pair;
        Click(EventKey(nqweChanged) + '-add');
      end;
    17:
      begin
        LInfo := AuthoredEvent(nqweChanged);
        Check((LInfo.Policy = neSequential) and (Length(LInfo.Callbacks) = 1),
          'ordinary Events Add retains the normal Changed scheduling policy');
        FChangedHandler := LInfo.Callbacks[0].Handler.Name;
        FChangedRegistration := LInfo.Callbacks[0].ID.Name;
        {$ifdef PAS2JS}await(History);{$else}History;{$endif}
        CheckSavedEvents;
      end;
    18:
      begin

        if FRenderer.RootFor('action-panel-project') <> nil then
        begin
          Click('action-panel-project');
        end;
      end;
    19: Click(NyxStudioActionMenuID);
    20:
      begin
        {$ifdef PAS2JS}ClickProjectMenu('studio-menu-project');
        {$else}ClickProjectMenu('Component actions', 'Project');{$endif}
      end;
    21:
      begin
        {$ifdef PAS2JS}ClickProjectMenu('studio-menu-open');
        {$else}ClickProjectMenu('Project', 'Open project');{$endif}
      end;
    22:
      begin
        FSaved := Pair;
        {$ifdef PAS2JS}Click('action-project-export');{$else}
        ProjectName;
        Click('action-project-save');
        {$endif}
      end;
    23:
      begin
        {$ifdef PAS2JS}Click('action-undo');{$else}
        LStore := TNyxProjectStore.Create(IncludeTrailingPathDelimiter(GEvidence) + 'studio-projects');
        try
          Check(LStore.ReadProject('table-query-workshop', LRevision) = FSaved,
            'real named Save persists this journey exact accepted pair');
        finally
          LStore.Free;
        end;
        { Native Open deliberately keeps its unsaved-pair refusal. History
          before Save is nonempty, so fresh history independently proves Open. }
        Click('action-project-open');
        {$endif}
      end;
    24:
      begin
        {$ifdef PAS2JS}
        Check((Pair <> FSaved) and (Length(GFiles.Exported) = 1) and
          (GFiles.Exported[0].Text = FSaved),
          'browser Import must restore the actual changed project from its own exported packet');
        Click('action-project-import');
        GFiles.Deliver;
        {$endif}
      end;
    25:
      begin
        Check(Pair = FSaved, 'ordinary Open admits the exact own policy/callback/Pascal pair');
        CheckSavedEvents;
        Isolated;
        {$ifndef PAS2JS}
        Check(not FSession.CanUndo and not FSession.CanRedo,
          'real native Open begins deliberate fresh project history');
        WriteNyxTextFile(IncludeTrailingPathDelimiter(GEvidence) + 'accepted-project.nyxproject',
          NyxTextFile('accepted-project.nyxproject', FSaved));
        WriteNyxTextFile(IncludeTrailingPathDelimiter(GEvidence) + 'nyx.fixture.table.query.studio.pas',
          NyxTextFile('nyx.fixture.table.query.studio.pas', FStudio.ProjectSnapshot.Source));
        {$endif}
        GSaved := FSaved;
        Result := True;
      end;
  end;
  Trace('step:after', FStage);
  Inc(FStage);
  {$endif}
end;

{ The same proof runs against the ordinary reopened document and the independently
  compiled own unit. Target helpers borrow fresh faces only inside one input. }
type
  TQueryReceiver = class(TNyxEventCallback)
  public
    ExpectedID: TNyxText;
    BeforeCalls, ChangedCalls: Integer;
    Veto: Boolean;
    Captured: TNyxQueryWorkbenchChange;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  { Completion of the actual UI queue owns the assertion boundary. This work
    contains no widget, document, callback owner or borrowed application. }
  TQueueBarrier = class(TInterfacedObject, INyxWork)
  public
    Done: Boolean;
    procedure Execute(const AExecution: INyxExecution);
  end;
  {$ifndef NYX_COMPILED_TABLE_QUERY_STUDIO}
  { Ordinary descriptor binding cannot execute saved Pascal. Only the two exact
    inspected TODO class names receive this equivalent no-op implementation.
    The compiled branch never registers this adapter; its emitted classes own
    actual Pascal execution. Registry ownership lasts to process finalization. }
  TOrdinaryTodo = class(TNyxApplicationEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  {$endif}
  { Own a reopened or compiled document and its actual target application.
    Each physical helper resolves the current typed column's borrowed face;
    neither pinned panes nor a toolbar may lend it an old physical coordinate. }
  TApplicationReview = class
  private
    FDocument: TNyxDocument;
    FApp: TApplicationHost;
    FAuthored: TNyxText;
    FViews: array of INyxCollectionView;
    FWorkbenches: array of INyxCollectionQueryWorkbench;
    FSources: array of INyxCollectionSnapshot;
    FReceiver: TQueryReceiver;
    FReceiverLease: INyxEventCallback;
    FBeforeToken, FChangedToken: INyxEventSubscription;
    {$ifdef PAS2JS}FHost: TJSHTMLElement;
    {$else}FFailure: TFailureObserver;{$endif}
    function TableID(AIndex: Integer): TNyxText;
    procedure CaptureOwners;
    procedure Observe;
    function AuthoredStream(AEvent: TNyxQueryWorkbenchEvent): INyxEventStream;
    function FormRenderer(AIndex: Integer): TApplicationRenderer;
    function FormText(AIndex: Integer; const AID: TNyxText): TNyxText;
    function DraftValue(AIndex: Integer; const AID: TNyxText): TNyxText;
    procedure TextInput(AIndex: Integer; const AID, AText: TNyxText);
    procedure Choose(AIndex: Integer; const AID, ACaption: TNyxText);
    procedure FormClick(AIndex: Integer; const AID: TNyxText; APump: Boolean = True);
    procedure Header(AIndex, AColumn: Integer; AFilter: Boolean; AAppend: Boolean = False);
    procedure KeyboardOpen(AIndex, AColumn: Integer);
    procedure Visible(AIndex: Integer; const AIDs: array of TNyxText);
    function HeaderText(AIndex, AColumn: Integer): TNyxText;
    {$ifndef PAS2JS}
    function NativeColumnGrid(AIndex, AColumn: Integer; out APhysical: Integer): TStringGrid;
    {$endif}
    procedure OpenFilter(AIndex, AColumn: Integer);
    procedure AddCondition(AIndex: Integer);
    procedure Apply(AIndex: Integer);
    procedure SourcesUnchanged;
    procedure Settle; {$ifdef PAS2JS}async;{$endif}
  public
    constructor Create(ADocument: TNyxDocument);
    destructor Destroy; override;
    procedure Run; {$ifdef PAS2JS}async;{$endif}
  end;

procedure Pump;
begin
  {$ifndef PAS2JS}Application.ProcessMessages; CheckSynchronize(1);{$endif}
end;

procedure TQueueBarrier.Execute(const AExecution: INyxExecution);
begin
  Done := not AExecution.Cancelled;
end;

procedure TQueryReceiver.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Captured := NyxQueryWorkbenchChange(AEvent).Copy;
  Check((AEvent.SourceID = ExpectedID) and (AEvent.OriginID = ExpectedID),
    'actual runtime query dispatch retains its exact qualified instance identity');

  if AEvent.Name.Name = NyxQueryWorkbenchEvent(nqweBeforeChange).Name then
  begin
    Inc(BeforeCalls);
    Check(not Captured.HasAfter and (Captured.Edits.Count > 0),
      'sequential Before carries copied requested edits without an invented result');

    if Veto then
    begin
      AExecution.Cancel;
    end;
  end
  else
  begin
    Inc(ChangedCalls);
    Check((AEvent.Name.Name = NyxQueryWorkbenchEvent(nqweChanged).Name) and Captured.HasAfter,
      'committed Changed carries its copied accepted observation');
  end;
end;

{$ifndef NYX_COMPILED_TABLE_QUERY_STUDIO}
procedure TOrdinaryTodo.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin

  if AExecution.Cancelled then
  begin
    Exit;
  end;
  { Same no-op as the two inspected ordinary TODO bodies. }
end;
{$endif}

function TApplicationReview.TableID(AIndex: Integer): TNyxText;
begin

  if AIndex = 0 then
  begin
    Result := NyxQualifiedID(CFirst, CDefinition);
  end
  else
  begin
    Result := NyxQualifiedID(CSecond, CDefinition);
  end;
end;

procedure TApplicationReview.CaptureOwners;
var
  LIndex: Integer;
  LMount: INyxCollectionMount;
  LQueries: INyxCollectionQueryMount;
begin
  for LIndex := 0 to 1 do
  begin
    Check(FApp.View.Root.Find(TableID(LIndex)) <> nil, 'application realizes the exact reusable table root');
    FViews[LIndex] := FApp.View.CollectionView(TableID(LIndex));
    LMount := FApp.View.CollectionMount(TableID(LIndex));
    LQueries := nil;
    Check(LMount.Connected and Supports(LMount, INyxCollectionQueryMount, LQueries),
      'actual bound table exposes its existing optional query mount');
    FWorkbenches[LIndex] := LQueries.QueryWorkbench;
    Check(FWorkbenches[LIndex].Connected, 'accepted application owns an active query workbench');
  end;
  Check((FViews[0] <> FViews[1]) and (FViews[0].Store <> FViews[1].Store),
    'two csInstance tables retain independent views, query sessions and source stores');
end;

function TApplicationReview.AuthoredStream(AEvent: TNyxQueryWorkbenchEvent): INyxEventStream;
var
  LTarget: TNyxEventTarget;
begin
  LTarget := NyxControlEvents(TableID(0), niRuntime);

  if FApp.View.Root.Find(TableID(0)).Prop(NyxAttributeName(atCompound)) = 'true' then
  begin
    LTarget := NyxCompoundEvents(TableID(0), niRuntime);
  end;
  Result := FApp.View.Events.OnNamed(LTarget, NyxQueryWorkbenchEvent(AEvent));
end;

procedure TApplicationReview.Observe;
begin

  if FBeforeToken <> nil then
  begin
    FBeforeToken.Cancel;
    FChangedToken.Cancel;
  end;
  Check((AuthoredStream(nqweBeforeChange).Count = 1) and (AuthoredStream(nqweChanged).Count = 1),
    'own authored query callbacks are bound before independent runtime observers');
  Check(AuthoredStream(nqweBeforeChange).ExecutionPolicy = neSequential,
    'actual authored Before stream retains its required sequential policy');
  FBeforeToken := FApp.View.Events.OnNamed(NyxControlEvents(TableID(0), niRuntime),
    NyxQueryWorkbenchEvent(nqweBeforeChange)).Subscribe(FReceiverLease);
  FChangedToken := FApp.View.Events.OnNamed(NyxControlEvents(TableID(0), niRuntime),
    NyxQueryWorkbenchEvent(nqweChanged)).Subscribe(FReceiverLease);
end;

constructor TApplicationReview.Create(ADocument: TNyxDocument);
begin
  inherited Create;
  FDocument := ADocument;
  FAuthored := TNyxCodec.Encode(ADocument);
  SetLength(FViews, 2);
  SetLength(FWorkbenches, 2);
  SetLength(FSources, 2);
  FApp := TApplicationHost.Create;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('main'));
  FHost.style.setProperty('width', IntToStr(GWidth) + 'px');
  document.body.appendChild(FHost);
  FApp.Run(ADocument, FHost);
  {$else}
  FFailure := TFailureObserver.Create;
  Application.OnException := FFailure.Failed;
  FApp.Mount(ADocument);
  FApp.Window.ClientWidth := GWidth;
  FApp.Window.ClientHeight := 900;
  FApp.Window.Show;
  Pump;
  Check(FApp.Window.ClientWidth = GWidth, 'actual saved/compiled application client width matches its allocation');
  WriteLn('CLIENT_WIDTH=', FApp.Window.ClientWidth);
  {$endif}
  CaptureOwners;
  { Runtime setup is explicit, uses the existing typed store and touches only
    the first instance. The saved document/default rows remain the original two. }
  Check((FViews[0].Store.Snapshot.Count = 2) and (FViews[1].Store.Snapshot.Count = 2),
    'own saved defaults initialize both instance stores with the original two rows');
  FViews[0].Store.Apply([
    NyxInsert(2, NyxCollectionItem(NyxItem(People, 'linus')).WithValue(NyxTextField('name'), 'Linus')
      .WithValue(NyxIntegerField('age'), 28).WithValue(NyxNumberField('score'), 10.0)
      .WithValue(NyxBooleanField('active'), True)),
    NyxInsert(3, NyxCollectionItem(NyxItem(People, 'alan')).WithValue(NyxTextField('name'), 'Alan')
      .WithValue(NyxIntegerField('age'), 25).WithValue(NyxNumberField('score'), 7.0)
      .WithValue(NyxBooleanField('active'), True))]);
  FSources[0] := FViews[0].Store.Snapshot;
  FSources[1] := FViews[1].Store.Snapshot;
  Check((FSources[0].Count = 4) and (FSources[1].Count = 2),
    'first-only runtime rows enable meaningful paging without changing the sibling store');
  FReceiver := TQueryReceiver.Create;
  FReceiver.ExpectedID := TableID(0);
  FReceiverLease := FReceiver;
  Observe;
end;

destructor TApplicationReview.Destroy;
begin

  if FBeforeToken <> nil then
  begin
    FBeforeToken.Cancel;
    FChangedToken.Cancel;
  end;
  FBeforeToken := nil;
  FChangedToken := nil;
  FReceiver := nil;
  FReceiverLease := nil;
  FWorkbenches := nil;
  FViews := nil;
  FSources := nil;
  FApp.Free;
  {$ifdef PAS2JS}

  if FHost <> nil then
  begin
    FHost.remove;
  end;
  {$else}
  Application.OnException := nil;
  FFailure.Free;
  {$endif}
  inherited Destroy;
end;

function TApplicationReview.FormRenderer(AIndex: Integer): TApplicationRenderer;
var
  LPresenter: INyxQueryFormPresenter;
  {$ifdef PAS2JS}LTarget: INyxBrowserQueryFormPresenter;
  {$else}LTarget: INyxLCLQueryFormPresenter;{$endif}
begin
  LPresenter := FWorkbenches[AIndex].Presenter;
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

function TApplicationReview.FormText(AIndex: Integer; const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := FormRenderer(AIndex).ElementFor(AID).textContent;
  {$else}
  Result := TLabel(FormRenderer(AIndex).ControlFor(AID)).Caption;
  {$endif}
end;

function TApplicationReview.DraftValue(AIndex: Integer; const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(FormRenderer(AIndex).InputFor(AID)).value;
  {$else}
  Result := TCustomEdit(FormRenderer(AIndex).InputFor(AID)).Text;
  {$endif}
end;

procedure TApplicationReview.TextInput(AIndex: Integer; const AID, AText: TNyxText);
var
  LRenderer: TApplicationRenderer;
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
  Check(FormRenderer(AIndex).Root.Find(AID).Prop(NyxAttributeName(atValue)) = AText,
    'physical input reaches mounted live Root, not authored descriptor setters');
end;

procedure TApplicationReview.Choose(AIndex: Integer; const AID, ACaption: TNyxText);
var
  LRenderer: TApplicationRenderer;
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

procedure TApplicationReview.FormClick(AIndex: Integer; const AID: TNyxText; APump: Boolean);
var
  LRenderer: TApplicationRenderer;
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

{$ifndef PAS2JS}
function TApplicationReview.NativeColumnGrid(AIndex, AColumn: Integer;
  out APhysical: Integer): TStringGrid;
var
  LOuter: TStringGrid;
  LCandidate: TStringGrid;
  LTitle: TNyxText;
  LChild: Integer;

  function FindIn(AGrid: TStringGrid): Boolean;
  var
    LPhysical: Integer;
  begin
    Result := False;
    for LPhysical := 0 to AGrid.ColCount - 1 do
    begin

      if (AGrid.ColWidths[LPhysical] > 0) and
        (Pos(LTitle, TNyxText(AGrid.Cells[LPhysical, 0])) = 1) then
      begin
        APhysical := LPhysical;
        Result := True;
        Exit;
      end;
    end;
  end;

begin
  Result := nil;
  APhysical := -1;
  LOuter := TStringGrid(FApp.View.ControlFor(TableID(AIndex)));
  LTitle := FViews[AIndex].Spec.ColumnAt(AColumn).Title;
  Check((LOuter <> nil) and (LTitle <> ''),
    'typed application column has an actual native table and authored title');

  if FindIn(LOuter) then
  begin
    Exit(LOuter);
  end;
  for LChild := 0 to LOuter.ControlCount - 1 do
  begin

    if (LOuter.Controls[LChild] is TStringGrid) and LOuter.Controls[LChild].Visible then
    begin
      LCandidate := TStringGrid(LOuter.Controls[LChild]);

      if FindIn(LCandidate) then
      begin
        Exit(LCandidate);
      end;
    end;
  end;
  Check(False, 'typed application column keeps a current center or pinned native face');
end;
{$endif}

procedure TApplicationReview.Header(AIndex, AColumn: Integer; AFilter: Boolean; AAppend: Boolean);
{$ifdef PAS2JS}
var
  LButton: TJSHTMLElement;
  LOptions: TJSObject;
  LAttribute: TNyxText;
{$else}
var
  LGrid: TStringGrid;
  LBounds: TRect;
  LPhysical: Integer;
  LX: Integer;
  LShift: TShiftState;
{$endif}
begin
  FApp.View.Reveal(TableID(AIndex), niRuntime);
  {$ifdef PAS2JS}
  LAttribute := 'data-nyx-query-sort';

  if AFilter then
  begin
    LAttribute := 'data-nyx-query-column';
  end;
  LButton := TJSHTMLElement(FApp.View.ElementFor(TableID(AIndex)).querySelector(
    '[' + LAttribute + '="' + IntToStr(AColumn) + '"]'));
  Check(LButton <> nil, 'the actual table header exposes the requested query action');
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LOptions['shiftKey'] := AAppend;
  LButton.dispatchEvent(TPhysicalMouse.new('click', LOptions));
  {$else}
  LGrid := NativeColumnGrid(AIndex, AColumn, LPhysical);
  LBounds := TNyxCollectionStringGrid(LGrid).ColumnHeaderRect(LPhysical);
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

procedure TApplicationReview.KeyboardOpen(AIndex, AColumn: Integer);
{$ifdef PAS2JS}
var
  LOptions: TJSObject;
  LButton: TJSHTMLElement;
{$else}
var
  LGrid: TStringGrid;
  LPhysical: Integer;
  LKey: Word;
{$endif}
begin
  FApp.View.Reveal(TableID(AIndex), niRuntime);
  {$ifdef PAS2JS}
  LButton := TJSHTMLElement(FApp.View.ElementFor(TableID(AIndex)).querySelector(
    '[data-nyx-query-column="' + IntToStr(AColumn) + '"]'));
  Check(LButton <> nil, 'keyboard query target is a real focusable header action');
  LButton.focus;
  LOptions := TJSObject.new;
  LOptions['key'] := 'ArrowDown';
  LOptions['altKey'] := True;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LButton.dispatchEvent(TPhysicalKey.new('keydown', LOptions));
  {$else}
  LGrid := NativeColumnGrid(AIndex, AColumn, LPhysical);
  LGrid.Col := LPhysical;
  LGrid.SetFocus;
  LKey := VK_DOWN;
  Check(Assigned(LGrid.OnKeyDown), 'native table owns its actual key handler');
  LGrid.OnKeyDown(LGrid, LKey, [ssAlt]);
  Check(LKey = 0, 'Alt+Down consumes the native default only after opening its form');
  {$endif}
  Pump;
  Check(FWorkbenches[AIndex].Presenter <> nil, 'ordinary keyboard input opens the managed query form');
end;

procedure TApplicationReview.Visible(AIndex: Integer; const AIDs: array of TNyxText);
var
  LIndex: Integer;
  {$ifdef PAS2JS}LRows: TJSNodeList;
  {$else}
  LGrid: TStringGrid;
  LPhysical: Integer;
  {$endif}
begin
  Check(FViews[AIndex].Snapshot.Count = Length(AIDs), 'query has expected page membership');
  {$ifdef PAS2JS}
  LRows := FApp.View.ElementFor(TableID(AIndex)).querySelectorAll('[data-nyx-item]');
  Check(LRows.length = Length(AIDs), 'actual browser rows follow accepted page membership');
  {$else}
  LGrid := NativeColumnGrid(AIndex, 0, LPhysical);

  if Length(AIDs) = 0 then
  begin
    Check((LGrid.RowCount = 2) and (LGrid.Cells[LPhysical, 1] = ''),
      'empty native table retains the required blank widget row, with no data identity');
  end
  else
  begin
    Check(LGrid.RowCount = Length(AIDs) + 1, 'native row-zero ABI retains exact data row count');
  end;
  {$endif}
  for LIndex := 0 to High(AIDs) do
  begin
    Check(FViews[AIndex].Snapshot.ItemAt(LIndex).Ref.ID = AIDs[LIndex], 'stable typed row order');
    {$ifdef PAS2JS}
    Check(TJSHTMLElement(LRows[LIndex]).getAttribute('data-nyx-item') = AIDs[LIndex],
      'actual browser row identity matches admitted query');
    {$else}
    Check(LGrid.Cells[LPhysical, LIndex + 1] = FViews[AIndex].CellText(NyxItem(People, AIDs[LIndex]), 0),
      'physical native cell follows accepted typed ordering');
    {$endif}
  end;
end;

function TApplicationReview.HeaderText(AIndex, AColumn: Integer): TNyxText;
{$ifndef PAS2JS}
var
  LGrid: TStringGrid;
  LPhysical: Integer;
{$endif}
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(FApp.View.ElementFor(TableID(AIndex)).querySelector(
    '[data-nyx-query-sort="' + IntToStr(AColumn) + '"]')).textContent;
  {$else}
  LGrid := NativeColumnGrid(AIndex, AColumn, LPhysical);
  Result := LGrid.Cells[LPhysical, 0];
  {$endif}
end;



procedure TApplicationReview.OpenFilter(AIndex, AColumn: Integer);
begin
  Header(AIndex, AColumn, True);
  Check(FWorkbenches[AIndex].Presenter <> nil, 'actual header release opens its managed query form');
  {$ifdef PAS2JS}
  Check(document.activeElement = FormRenderer(AIndex).FocusFor(
    NyxQueryEditorFieldID(NyxQueryWorkbenchEditorID, nqfField)), 'header release leaves actual focus inside the query form');
  {$else}
  Check(Screen.ActiveControl = FormRenderer(AIndex).FocusFor(
    NyxQueryEditorFieldID(NyxQueryWorkbenchEditorID, nqfField)), 'native grid default does not steal query form focus');
  {$endif}
end;

procedure TApplicationReview.AddCondition(AIndex: Integer);
begin
  FormClick(AIndex, NyxQueryEditorActionID(NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqeAnd));
  Check(FormRenderer(AIndex).Root.Find(NyxQueryEditorFieldID(
    NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqfValue)) <> nil,
    'structural query input creates its real typed predicate editor');
end;

procedure TApplicationReview.Apply(AIndex: Integer);
begin
  FormClick(AIndex, NyxQueryEditorActionID(NyxQueryWorkbenchEditorID, nqeSave));
end;

procedure TApplicationReview.SourcesUnchanged;
begin
  Check((FViews[0].Store.Snapshot = FSources[0]) and (FViews[1].Store.Snapshot = FSources[1]),
    'query input preserves each independent source snapshot exactly');
  Check(TNyxCodec.Encode(FDocument) = FAuthored,
    'runtime data/query/selection/drafts never rewrite the own saved design or defaults');
  {$ifndef PAS2JS}
  Check(FFailure.Text = '', 'native application widget callbacks reported no hidden exception');
  {$endif}
end;

procedure TApplicationReview.Settle; {$ifdef PAS2JS}async;{$endif}
var
  LBarrier: TQueueBarrier;
  LWork: INyxWork;
  LTicket: INyxExecution;
  {$ifdef PAS2JS}LStarted: Double;{$else}LStarted: QWord;{$endif}
begin
  {$ifdef PAS2JS}

  if GClosing then
  begin
    raise Exception.Create('Query application page is retired');
  end;
  {$endif}
  LBarrier := TQueueBarrier.Create;
  LWork := LBarrier;
  LTicket := FApp.View.Events.Scheduler.Submit(LWork, neUIQueue);
  {$ifdef PAS2JS}LStarted := TJSDate.now;{$else}LStarted := GetTickCount64;{$endif}
  while not LBarrier.Done do
  begin
    {$ifdef PAS2JS}if TJSDate.now - LStarted > 10000 then
    {$else}if GetTickCount64 - LStarted > 10000 then{$endif}
    begin
      raise Exception.Create('Actual application query queue did not complete');
    end;
    {$ifdef PAS2JS}await(UITurn);{$else}Pump;{$endif}
    {$ifdef PAS2JS}

    if GClosing then
    begin
      raise Exception.Create('Query application page retired before queue completion');
    end;
    {$endif}
  end;
  Check(LTicket.Status = nesSucceeded, 'actual application UI queue barrier completes');
  {$ifndef PAS2JS}
  Check(FFailure.Text = '', 'settled native application reports every captured widget exception');
  {$endif}

  if FBeforeToken.LastExecution <> nil then
  begin
    Check((FBeforeToken.LastExecution.Status = nesSucceeded) or
      (FReceiver.Veto and (FBeforeToken.LastExecution.Status = nesCancelled)),
      'runtime Before observer succeeds or deliberately cancels, never hides a callback failure');
  end;

  if FChangedToken.LastExecution <> nil then
  begin
    Check(FChangedToken.LastExecution.Status = nesSucceeded,
      'runtime Changed observer reports its actual successful execution');
  end;
end;

procedure TApplicationReview.Run; {$ifdef PAS2JS}async;{$endif}
var
  LValueID, LComparisonID, LBefore, LRetainedJSON: TNyxText;
  LChanged: Integer;
  LRetained: TNyxQueryWorkbenchChange;
  LPresenter: INyxQueryFormPresenter;
  LOld: INyxCollectionQueryWorkbench;
  LFirstStore, LSecondStore: INyxCollection;
  LRefused: Boolean;
  {$ifdef PAS2JS}LBoolean: TJSHTMLInputElement;{$else}LBoolean: TCheckBox;{$endif}
begin
  LValueID := NyxQueryEditorFieldID(NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqfValue);
  LComparisonID := NyxQueryEditorFieldID(NyxQueryEditorPredicateID(NyxQueryWorkbenchEditorID), nqfComparison);
  Check(FWorkbenches[0].Observation.SourceRows = 4, 'matching counts distinguish source rows from the current page');
  Visible(0, ['linus', 'grace']);
  Visible(1, ['grace', 'ada']);
  LBefore := FViews[1].QueryPolicy.ToData.ToJSON;
  Header(0, 0, False);
  Header(0, 1, False, True);
  {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
  Check(FWorkbenches[0].Observation.HasUserSort and
    (FWorkbenches[0].Observation.Query.SortCount = 2), 'physical Shift header adds a second sort key under the saved local policy');
  Check((AuthoredStream(nqweBeforeChange).Registrations[0].LastExecution <> nil) and
    (AuthoredStream(nqweBeforeChange).Registrations[0].LastExecution.Status = nesSucceeded) and
    (AuthoredStream(nqweChanged).Registrations[0].LastExecution <> nil) and
    (AuthoredStream(nqweChanged).Registrations[0].LastExecution.Status = nesSucceeded),
    'both bound authored registrations actually execute successfully before the observer subscriptions');
  Check(FViews[1].QueryPolicy.ToData.ToJSON = LBefore, 'first table sorting leaves independent sibling query exact');
  Header(1, 1, False);
  Header(1, 0, False, True);
  Check(FWorkbenches[1].Observation.Query.SortCount = 1, 'inherited sibling single-sort policy remains effective');
  FWorkbenches[1].ResetAll;
  { BEGIN P4C VISIBLE SELECTION SETUP
    The preceding physical sort proof can move Linus off the current page.
    Restore the authored query before selecting this deliberately visible row. }
  FWorkbenches[0].ResetAll;
  Visible(0, ['linus', 'grace']);
  { END P4C VISIBLE SELECTION SETUP }
  { Deliberate typed selection precedes physical filtering. It is not presented
    as keyboard/hardware selection evidence. }
  FViews[0].Select(NyxItem(People, 'linus'));
  OpenFilter(0, 0);
  AddCondition(0);
  Choose(0, LComparisonID, 'Contains');
  TextInput(0, LValueID, 'a');
  Apply(0);
  {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
  Check((FWorkbenches[0].Observation.MatchedRows = 3) and
    FViews[0].Selection.Contains(NyxItem(People, 'linus')),
    'actual text filter preserves selected identity while hiding the selected row');
  LRetained := FReceiver.Captured.Copy;
  LRetainedJSON := LRetained.ToData.ToJSON;
  OpenFilter(0, 1);
  AddCondition(0);
  Choose(0, LComparisonID, 'At least');
  TextInput(0, LValueID, '36');
  Apply(0);
  {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
  Check((FWorkbenches[0].Observation.Count = 2) and
    (FWorkbenches[0].Observation.MatchedRows = 2), 'typed integer and text filters compose on the same accepted session');
  OpenFilter(0, 2);
  AddCondition(0);
  Choose(0, LComparisonID, 'At least');
  TextInput(0, LValueID, '9');
  Apply(0);
  {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
  Visible(0, ['ada']);
  OpenFilter(0, 3);
  AddCondition(0);
  {$ifdef PAS2JS}
  LBoolean := TJSHTMLInputElement(FormRenderer(0).InputFor(LValueID));
  LBoolean.checked := True;
  LBoolean.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LBoolean := TCheckBox(FormRenderer(0).InputFor(LValueID));
  LBoolean.Checked := True;
  LBoolean.OnChange(LBoolean);
  {$endif}
  Apply(0);
  {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
  Check((FWorkbenches[0].Observation.Count = 4) and
    (FWorkbenches[0].Observation.MatchedRows = 1), 'all four actual scalar form families compose under saved operator choices');
  LRefused := False;
  try
    FWorkbenches[1].OpenColumn(3);
  except
    on ENyxCollection do LRefused := True;
  end;
  Check(LRefused, 'second instance still enforces its inherited Boolean filtering opt-out');
  OpenFilter(0, 1);
  TextInput(0, LValueID, '-');
  LPresenter := FWorkbenches[0].Presenter;
  LBefore := FViews[0].QueryPolicy.ToData.ToJSON;
  Apply(0);
  {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
  Check((FWorkbenches[0].Presenter = LPresenter) and (DraftValue(0, LValueID) = '-') and
    (FViews[0].QueryPolicy.ToData.ToJSON = LBefore), 'invalid numeric input remains correctable in its same live draft without publication');
  Check(FormText(0, 'nyx-query-workbench-status') <> '', 'invalid draft has a visible refusal explanation');
  TextInput(0, LValueID, '40');
  FReceiver.Veto := True;
  LChanged := FReceiver.ChangedCalls;
  Apply(0);
  {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
  Check((FReceiver.ChangedCalls = LChanged) and (FViews[0].QueryPolicy.ToData.ToJSON = LBefore),
    'actual sequential Before cancels a proposal without a Changed notification');
  FReceiver.Veto := False;
  Apply(0);
  {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
  Visible(0, []);
  KeyboardOpen(0, 1);
  Check(FormRenderer(0).Root.Find('nyx-query-workbench-counts-empty') <> nil,
    'zero-match table retains keyboard-accessible empty recovery');
  Check(FReceiver.ChangedCalls > LChanged, 'corrected actual input publishes its committed Changed event');
  FormClick(0, 'nyx-query-workbench-chip-0');
  {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
  Check(FWorkbenches[0].Observation.Count = 3, 'physical active chip removes exactly one filter contribution');
  { BEGIN P4C RETIRED CHIP FORM REOPEN
    Accepted chip removal retires its launch form. Reopen through real keyboard
    input before borrowing the next command's current presenter. }
  KeyboardOpen(0, 1);
  { END P4C RETIRED CHIP FORM REOPEN }
  FormClick(0, 'nyx-query-workbench-clear-owner');
  {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
  Check((FWorkbenches[0].Observation.Count = 0) and
    FViews[0].Selection.Contains(NyxItem(People, 'linus')), 'Clear restores hidden membership without rewriting deliberate selection');
  { BEGIN P4C PHYSICAL RESET SORT SETUP
    Selection setup reset the earlier sort override. Add a real user sort so
    the unchanged physical Reset still has a sorting choice to clear. Its
    accepted header command retires the prior form, so reacquire by keyboard. }
  Header(0, 0, False);
  Check(FWorkbenches[0].Observation.HasUserSort,
    'physical Reset begins with an actual user sort override');
  KeyboardOpen(0, 1);
  { END P4C PHYSICAL RESET SORT SETUP }
  FormClick(0, 'nyx-query-workbench-reset-all');
  {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
  Check(not FWorkbenches[0].Observation.HasUserSort and
    (FViews[0].QueryPolicy.ToData.ToJSON = InitialSpec.QueryPolicy.ToData.ToJSON),
    'explicit Reset restores the exact authored query baseline');
  { Public page capability is command evidence, not a physical Pager claim. }
  Check(NyxCollectionPages(FViews[0]).Window.Pages = 2, 'first-only runtime data supports two real pages');
  NyxCollectionPages(FViews[0]).NextPage;
  Check((NyxCollectionPages(FViews[0]).Window.Page = 2) and
    (NyxCollectionPages(FViews[1]).Window.Page = 1), 'public paging changes one independent view only');
  Visible(0, ['alan', 'ada']);
  Visible(1, ['grace', 'ada']);
  NyxCollectionPages(FViews[0]).FirstPage;
  Check(LRetained.ToData.ToJSON = LRetainedJSON, 'copied earlier query Details remain exact after later commands and paging');
  SourcesUnchanged;
  OpenFilter(0, 1);
  LPresenter := FWorkbenches[0].Presenter;
  FormClick(0, 'nyx-query-workbench-reload');
  Check(not LPresenter.Connected, 'explicit draft reload retires the earlier retained capture');
  AddCondition(0);
  TextInput(0, LValueID, '43');
  LOld := FWorkbenches[0];
  LPresenter := LOld.Presenter;
  { Queue through the real form stream, then replace before delivery. }
  LPresenter.Events.On(NyxControlEvents(NyxQueryEditorActionID(NyxQueryWorkbenchEditorID, nqeSave),
    niRuntime), ntClick).Policy(neUIQueue);
  FormClick(0, NyxQueryEditorActionID(NyxQueryWorkbenchEditorID, nqeSave), False);
  LFirstStore := FViews[0].Store;
  LSecondStore := FViews[1].Store;
  FApp.ShowPage('home');
  Check(not LOld.Connected and not LPresenter.Connected, 'same-ID replacement revokes the old query owner and queued popup');
  LRefused := False;
  try
    LOld.ClearOwner;
  except
    on ENyxCollection do LRefused := True;
  end;
  Check(LRefused, 'retained command cannot retarget the replacement with identical IDs');
  CaptureOwners;
  Observe;
  {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
  Check((FViews[0].Store = LFirstStore) and (FViews[1].Store = LSecondStore) and
    (FWorkbenches[0].Observation.Count = 0), 'replacement retains both independent stores and refuses the former queued Apply');
  SourcesUnchanged;
  KeyboardOpen(0, 0);
  LOld := FWorkbenches[0];
  LPresenter := LOld.Presenter;
  FApp.Free;
  FApp := nil;
  {$ifndef PAS2JS}
  Check(FFailure.Text = '', 'native application disposal reports no hidden widget/callback failure');
  {$endif}
  Check(not LOld.Connected and not LPresenter.Connected, 'application disposal revokes retained workbench and popup capabilities');
end;

procedure CheckOwnPair(const APair: TNyxProjectPair);
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
  LEvents: TNyxAuthoredEventInfos;
  LIndex, LFound, LRegistryIndex, LRegistered: Integer;
  LSource, LName, LExpectedBody, LBody: TNyxText;
  LHandlers: array[0..1] of TNyxHandlerRef;
  LRegistrations: array[0..1] of TNyxCallbackRef;
  LCallbacks: INyxCallbackSnapshot;
begin
  LDocument := nil;
  LWorkspace := nil;
  try
    AdmitNyxProject(APair, nprRequireMatch, LDocument, LWorkspace, LResolved);
    Check(EncodeNyxProject(LResolved) = EncodeNyxProject(APair),
      'independent strict admission retains the own reopened design/Pascal pair');
    LEvents := NyxAuthoredEvents(LDocument.Find(CFirst));
    LFound := 0;
    LSource := APair.Source;
    for LIndex := 0 to High(LEvents) do
    begin

      if (LEvents[LIndex].Name.Name = NyxQueryWorkbenchEvent(nqweBeforeChange).Name) or
        (LEvents[LIndex].Name.Name = NyxQueryWorkbenchEvent(nqweChanged).Name) then
      begin
        Check((LEvents[LIndex].Trigger = ntNamed) and (LEvents[LIndex].Policy = neSequential) and
          (Length(LEvents[LIndex].Callbacks) = 1), 'own saved stream has its exact single sequential query registration');
        Check(LFound < 2, 'own saved query event identities are unique');
        LHandlers[LFound] := LEvents[LIndex].Callbacks[0].Handler;
        LRegistrations[LFound] := LEvents[LIndex].Callbacks[0].ID;
        LName := LEvents[LIndex].Callbacks[0].Handler.Name;
        Check((LName <> '') and (LEvents[LIndex].Callbacks[0].ID.Name <> '') and
          (Pos(LName + ' = class(TNyxApplicationEventCallback)', LSource) > 0) and
          (Pos('procedure ' + LName + '.Invoke(', LSource) > 0) and
          (Pos('TODO: implement ' + LName + '.', LSource) > 0) and
          (Pos('RegisterNyxCallback(NyxHandler(''' + LName + '''), ' + LName + ')', LSource) > 0),
          'own source retains the precise application class, TODO body and startup registration');
        { The public lexical reader admits one uniquely qualified Invoke. Only
          its exact complete generated empty implementation permits the ordinary
          no-op adapter; an extra statement/comment or changed template refuses.
          Line endings alone may differ between source-file host boundaries. }
        LExpectedBody := 'begin' + #10 + #10 +
          '  if AExecution.Cancelled then' + #10 + '  begin' + #10 +
          '    Exit;' + #10 + '  end;' + #10 +
          '  // TODO: implement ' + LName + '.' + #10 + 'end;';
        LBody := ReadNyxHandlerSource(LSource, LHandlers[LFound]).Code;
        LBody := Trim(StringReplace(LBody, #13#10, #10, [rfReplaceAll]));
        Check(LBody = LExpectedBody,
          'exact saved Invoke contains only the unchanged cancellation check and empty TODO body');
        Inc(LFound);
      end;
    end;
    Check((LFound = 2) and (LHandlers[0].Name <> LHandlers[1].Name) and
      (LRegistrations[0].Name <> LRegistrations[1].Name),
      'only two distinct own authored query handlers/registrations are admitted for application binding');
    {$ifndef NYX_COMPILED_TABLE_QUERY_STUDIO}
    { Both complete bodies and registration identities passed before either
      global startup registration. No universal fallback or other name is added. }
    RegisterNyxCallback(LHandlers[0], TOrdinaryTodo);
    RegisterNyxCallback(LHandlers[1], TOrdinaryTodo);
    {$endif}
    LCallbacks := CaptureNyxCallbacks;
    LRegistered := 0;
    for LIndex := 0 to 1 do
    begin
      for LRegistryIndex := 0 to LCallbacks.Count - 1 do
      begin

        if LCallbacks.HandlerAt(LRegistryIndex).Name = LHandlers[LIndex].Name then
        begin
          Inc(LRegistered);
        end;
      end;
    end;
    Check(LRegistered = 2,
      'both exact saved handler names have real startup implementations before runtime observers');
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

procedure ApplicationProof(const APair: TNyxProjectPair; ADocument: TNyxDocument);
  {$ifdef PAS2JS}async;{$endif}
var
  LReview: TApplicationReview;
begin
  CheckOwnPair(APair);
  Check(TNyxCodec.Encode(ADocument) = APair.Design, 'application builder reconstructs its own exact saved Studio design');
  LReview := TApplicationReview.Create(ADocument);
  try
    {$ifdef PAS2JS}await(LReview.Run);{$else}LReview.Run;{$endif}
  finally
    LReview.Free;
  end;
end;

procedure Passed;
{$ifdef PAS2JS}
var
  LArtifact: TJSHTMLElement;
{$endif}
begin
  {$ifdef NYX_COMPILED_TABLE_QUERY_STUDIO}
  WriteLn('PASS ', GChecks, ' compiled Studio query-policy application checks');
  {$else}
  WriteLn('PASS ', GChecks, ' actual Studio query-policy checks');
  {$endif}
  {$ifdef PAS2JS}
  { Publish only the completed proof's copied accepted packet. DOM readers can
    retain its exact text without accessing program globals or executing it.
    The non-executable JSON node is not a saved-source constructor or endpoint. }
  LArtifact := TJSHTMLElement(document.createElement('script'));
  LArtifact.setAttribute('type', 'application/json');
  LArtifact.setAttribute('id', 'query-authoring-project');
  LArtifact.textContent := GSaved;
  document.body.appendChild(LArtifact);
  document.body.setAttribute('data-query-authoring-project-length', IntToStr(Length(GSaved)));
  document.body.setAttribute('data-query-authoring', 'passed');
  document.body.setAttribute('data-query-authoring-checks', IntToStr(GChecks));
  {$endif}
end;

procedure Drive; {$ifdef PAS2JS}async;{$endif}
var
  LPair: TNyxProjectPair;
  LDocument: TNyxDocument;
  {$if defined(PAS2JS) and defined(NYX_WORKBENCH_STUDIO_ONLY)}
  LArtifact: TJSHTMLElement;
  {$endif}
begin
  LDocument := nil;

  if GDriving or GClosing then
  begin
    Exit;
  end;
  GDriving := True;
  try
    try
    Inc(GPolls);

    if GPolls > 2400 then
    begin
      raise Exception.Create('Policy authoring queue exceeded its completion bound');
    end;

    {$ifdef PAS2JS}
    Trace('drive:exchange:before');
    ServiceExchange;
    Trace('drive:exchange:after');

    if await(GReview.Step) then
    {$else}if GReview.Step then{$endif}
    begin
      {$if not defined(PAS2JS) or not defined(NYX_WORKBENCH_STUDIO_ONLY)}
      FreeAndNil(GReview);
      {$endif}
      {$ifdef NYX_WORKBENCH_STUDIO_ONLY}
      {$ifdef PAS2JS}
      { Keep the actual completed editor visible until pagehide for inspection.
        The copied packet is inert DOM text; no generated code is evaluated. }
      LArtifact := TJSHTMLElement(document.createElement('script'));
      LArtifact.setAttribute('type', 'application/json');
      LArtifact.setAttribute('id', 'workbench-studio-project');
      LArtifact.textContent := GSaved;
      document.body.appendChild(LArtifact);
      document.body.setAttribute('data-workbench-studio-project-length', IntToStr(Length(GSaved)));
      document.body.setAttribute('data-workbench-studio', 'passed');
      document.body.setAttribute('data-workbench-studio-checks', IntToStr(GChecks));
      {$else}
      WriteLn('PASS ', GChecks, ' actual focused Studio workbench checks');
      {$endif}
      {$else}
      LPair := DecodeNyxProject(GSaved);
      LDocument := TNyxCodec.Decode(LPair.Design);
      {$ifdef PAS2JS}await(ApplicationProof(LPair, LDocument));
      {$else}ApplicationProof(LPair, LDocument);{$endif}
      Passed;
      {$endif}
    end
    {$ifdef PAS2JS}else
    begin
      window.setTimeout(procedure
        begin
          Drive;
        end, 25);
    end{$endif};
  except
    on E: Exception do
    begin
      FreeAndNil(GReview);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-query-authoring', 'failed: ' + E.Message);
      {$else}WriteLn('FAIL ', E.Message); ExitCode := 1;{$endif}
    end;
    end;
  finally
    LDocument.Free;
    GDriving := False;
  end;
end;

{$ifdef NYX_COMPILED_TABLE_QUERY_STUDIO}
{ This frame owns the exact independently emitted BuildNyxDocument through the
  entire awaited proof. It creates no Studio or alternate saved fixture. }
procedure RunCompiled; {$ifdef PAS2JS}async;{$endif}
var
  LPair: TNyxProjectPair;
  LDocument: TNyxDocument;
begin
  LDocument := nil;
  GDriving := True;
  try
    try
      {$ifdef PAS2JS}
      Check(document.getElementById('expected-project') <> nil,
        'compiled browser page supplies its exact own accepted packet');
      GSaved := document.getElementById('expected-project').textContent;
      {$else}GSaved := ReadNyxTextFile(ParamStr(3)).Text;{$endif}
      LPair := DecodeNyxProject(GSaved);
      LDocument := BuildNyxDocument;
      {$ifdef PAS2JS}await(ApplicationProof(LPair, LDocument));
      {$else}ApplicationProof(LPair, LDocument);{$endif}
      Passed;
    except
      on E: Exception do
      begin
        {$ifdef PAS2JS}
        document.body.setAttribute('data-query-authoring', 'failed: ' + E.Message);
        {$else}WriteLn('FAIL ', E.Message); ExitCode := 1;{$endif}
      end;
    end;
  finally
    LDocument.Free;
    GDriving := False;
  end;
end;
{$endif}

{$ifdef PAS2JS}
function QueryStudioPageClosed(AEvent: TJSEvent): Boolean;
begin
  GClosing := True;

  if not GDriving then
  begin
    FreeAndNil(GReview);
  end;
  Result := True;
end;
{$endif}

begin
  GClosing := False;
  try
    {$ifdef PAS2JS}
    GWidth := window.innerWidth;
    {$else}

    if (ParamCount <> {$ifdef NYX_COMPILED_TABLE_QUERY_STUDIO}3{$else}2{$endif}) or
      not TryStrToInt(ParamStr(2), GWidth) or
      ((GWidth <> 390) and (GWidth <> 1280)) then
    begin
      raise Exception.Create('Expected fresh evidence directory and actual client width 1280/390');
    end;
    GEvidence := ExpandFileName(ParamStr(1));

    if DirectoryExists(GEvidence) then
    begin
      raise Exception.Create('Policy evidence directory must be new');
    end;
    Check(ForceDirectories(GEvidence), 'fresh evidence directory is created explicitly');
    Application.Initialize;
    {$endif}
    {$ifdef PAS2JS}
    window.addEventListener('pagehide', @QueryStudioPageClosed);
    {$endif}
    {$ifdef NYX_COMPILED_TABLE_QUERY_STUDIO}
    RunCompiled;
    {$else}
    Trace('descriptor-cases:before');
    DescriptorCases;
    Trace('descriptor-cases:after');
    Trace('admission-cases:before');
    AdmissionCases;
    Trace('admission-cases:after');
    GReview := TReview.Create;
    {$ifdef PAS2JS}
    Drive;
    {$else}
    while GReview <> nil do
    begin
      Drive;
      Application.ProcessMessages;
      CheckSynchronize(1);
    end;
    CheckSynchronize;
    {$endif}
    {$endif}
  except
    on E: Exception do
    begin
      FreeAndNil(GReview);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-query-authoring', 'failed: ' + E.Message);
      {$else}WriteLn('FAIL ', E.Message); ExitCode := 1;{$endif}
    end;
  end;
end.
