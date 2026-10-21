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
program nyx_coordinator_reuse_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils,
  {$ifdef PAS2JS}
  JS, Web, nyx.application.browser, nyx.render.browser, nyx.canvas.browser,
  {$else}
  Classes, Interfaces, Forms, Controls, StdCtrls, LCLType, LMessages,
  nyx.application.lcl, nyx.render.lcl, nyx.canvas.lcl,
  {$endif}
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.query, nyx.collections.selection, nyx.coordinators,
  nyx.selection.summary, nyx.master.detail, nyx.kanban, nyx.kanban.intent,
  nyx.events, nyx.scheduler, nyx.data, nyx.root.types, nyx.canvas.types, nyx.canvas.geometry,
  nyx.canvas.scene, nyx.canvas.workspace, nyx.canvas.declarations,
  nyx.presentations, nyx.publication.types;

type
  {$ifdef PAS2JS}
  { The ordinary browser application owns its renderer and independent runtime. }
  TReviewApplication = TNyxBrowserApplication;
  { Child renderers are borrowed afresh from the accepted canvas item. }
  TEmbeddedRenderer = TNyxBrowserRenderer;
  { The platform capability exposes the actual child renderer and input owner. }
  TEmbeddedCanvas = INyxBrowserCanvasWorkspaceView;
  { Exact mounted element identity is checked only while its view is accepted. }
  TReviewFace = TJSHTMLElement;
  { Installed Web declarations omit these standard MouseEventInit properties. }
  TReviewMouseOptions = class external name 'Object'(TJSObject)
    bubbles: Boolean;
    cancelable: Boolean;
    ctrlKey: Boolean;
  end;
  { Standard DOM input enters the real renderer's row-selection handler. }
  TReviewMouseEvent = class external name 'MouseEvent'(TJSMouseEvent)
    constructor new(const AType: String; AOptions: TReviewMouseOptions);
  end;
  {$else}
  { Mount supplies an actual native application without a second message loop. }
  TReviewApplication = TNyxLCLApplication;
  { Child renderers are borrowed afresh from the accepted canvas item. }
  TEmbeddedRenderer = TNyxLCLRenderer;
  { The platform capability exposes the actual child renderer and input owner. }
  TEmbeddedCanvas = INyxLCLCanvasWorkspaceView;
  { Exact mounted widget identity is checked only across a refused candidate. }
  TReviewFace = TControl;
  { Retain asynchronous widgetset errors rather than opening a modal dialog. }
  TFailureObserver = class
    Failure: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}
  { Three independent data owners coexist inside each reusable workspace. }
  TCompound = (rcSummary, rcDetail, rcKanban);

  { R2 owns a separate immutable design and ordinary applications across every
    asynchronous turn. No borrowed child renderer survives item retirement. }
  TEmbeddedReview = class
  private
    FDocument: TNyxDocument;
    FRegistry: INyxCoordinatorRegistry;
    FApps: array[0..1] of TReviewApplication;
    FDesign: TNyxText;
    FSource: TNyxText;
    {$ifdef PAS2JS}
    FHosts: array[0..1] of TJSHTMLElement;
    {$endif}
    procedure BuildFixture;
    function Canvas(AApp: Integer): TEmbeddedCanvas;
    function Child(AApp: Integer): TEmbeddedRenderer;
    function Role(AApp: Integer; const APath: TNyxPartRef): TNyxNode;
    function Summary(AApp: Integer; AFuture: Boolean = False): INyxSelectionSummaryBinding;
    function ParentSummary(AApp: Integer;
      AFuture: Boolean = False): INyxSelectionSummaryBinding;
    function Detail(AApp: Integer): INyxMasterDetailBinding;
    function Board(AApp: Integer): INyxKanbanBinding;
    function Widget(ARenderer: TEmbeddedRenderer; const AID: TNyxText): TReviewFace;
    function Caption(ARenderer: TEmbeddedRenderer; const AID: TNyxText): TNyxText;
    procedure SelectRows(AApp: Integer; const APath: TNyxPartRef; AMultiple: Boolean);
    procedure Press(AApp: Integer; const AID: TNyxText);
    procedure WriteNote(AApp: Integer; const AText: TNyxText);
    function Note(AApp: Integer): TNyxText;
    procedure CheckAuthored;
    procedure CheckMembership(AApp: Integer; ANormal, AFuture: Boolean);
    procedure CheckRetainedStores(AApp: Integer; const AStores: array of INyxCollection);
    procedure ReplaceItem(AApp: Integer; AMissingRole: Boolean);
    procedure AwaitCount(AApp, ACount: Integer;
      AFuture: Boolean = False); {$ifdef PAS2JS}async;{$endif}
    procedure AwaitRecord(AApp: Integer); {$ifdef PAS2JS}async;{$endif}
    procedure AwaitMount(AApp: Integer; const AFormer: INyxCancellationScope;
      ANormal, AFuture: Boolean); {$ifdef PAS2JS}async;{$endif}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Exercise; {$ifdef PAS2JS}async;{$endif}
  end;

const
  CHome = 'home';
  CRefusal = 'refusal';
  CWorkspace = 'workspace';
  CPlaceholder = 'Choose a person';
  CDescription = 'The selected record stays in this workspace.';
  CEmbeddedHome = 'embedded-home';
  CEmbeddedCanvas = 'embedded-canvas';
  CFutureIntent = 'example.selection-summary-future';
  CAlternate = 'alternate-decoration';

var
  GDocument: TNyxDocument;
  GRegistry: INyxCoordinatorRegistry;
  GApps: array[0..1] of TReviewApplication;
  { Dynamic interface vectors are supported by both maintained compilers. }
  GSummaries: array of INyxSelectionSummaryBinding;
  GDetails: array of INyxMasterDetailBinding;
  GBoards: array of INyxKanbanBinding;
  GStores: array of INyxCollection;
  GBaselines: array of INyxCollectionSnapshot;
  GDesign: TNyxText;
  GSource: TNyxText;
  GChecks: Integer;
  {$ifdef PAS2JS}
  GHosts: array[0..1] of TJSHTMLElement;
  {$else}
  GFailures: TFailureObserver;
  GEvidence: String;
  GWidth: Integer;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if GFailures.Failure <> '' then
  begin
    raise Exception.Create('Native reusable callback: ' + GFailures.Failure);
  end;
  {$endif}

  if not ACondition then
  begin
    raise Exception.Create('Coordinator reuse: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Failure := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

function SummaryPeople: TNyxCollectionRef;
begin
  Result := NyxCollection('summary-people');
end;

function SummaryName: TNyxTextFieldRef;
begin
  Result := NyxTextField('summary-name');
end;

function DetailPeople: TNyxCollectionRef;
begin
  Result := NyxCollection('directory-people');
end;

function DetailTitle: TNyxTextFieldRef;
begin
  Result := NyxTextField('detail-title');
end;

function DetailDescription: TNyxTextFieldRef;
begin
  Result := NyxTextField('detail-description');
end;

function WorkflowCards: TNyxCollectionRef;
begin
  Result := NyxCollection('workflow-cards');
end;

function CardTitle: TNyxTextFieldRef;
begin
  Result := NyxTextField('card-title');
end;

function CardLane: TNyxTextFieldRef;
begin
  Result := NyxTextField('card-lane');
end;

function Plan: TNyxKanbanLaneRef;
begin
  Result := NyxKanbanLane('plan');
end;

function Build: TNyxKanbanLaneRef;
begin
  Result := NyxKanbanLane('build');
end;

function Complete: TNyxKanbanLaneRef;
begin
  Result := NyxKanbanLane('complete');
end;

function Card: TNyxItemRef;
begin
  Result := NyxItem(WorkflowCards, 'welcome');
end;

function InstanceID(AInstance: Integer): TNyxText;
begin

  if AInstance = 0 then
  begin
    Result := 'first-workspace';
  end
  else
  begin
    Result := 'second-workspace';
  end;
end;

function BindingIndex(AApp, AInstance: Integer): Integer;
begin
  Result := AApp * 2 + AInstance;
end;

function StoreIndex(AApp, AInstance: Integer; ACompound: TCompound): Integer;
begin
  Result := BindingIndex(AApp, AInstance) * 3 + Ord(ACompound);
end;

{ All roles are resolved from the actual accepted instance, never by parsing a
  runtime ID or expecting the authoring Card subclass to survive realization. }
function Owner(AApp, AInstance: Integer; ACompound: TCompound): TNyxNode;
var
  LWorkspace: TNyxNode;
begin
  LWorkspace := GApps[AApp].View.Root.Find(NyxQualifiedID(InstanceID(AInstance), CWorkspace));

  if LWorkspace = nil then
  begin
    raise Exception.Create('The ordinary application has no exact workspace instance');
  end;
  case Ord(ACompound) of
    Ord(rcSummary):
      Result := LWorkspace.Part(NyxPart('insights/selection'));
    Ord(rcDetail):
      Result := LWorkspace.Part(NyxPart('insights/directory'));
    Ord(rcKanban):
      Result := LWorkspace.Part(NyxPart('insights/workflow'));
    else
      begin
        raise Exception.Create('Unknown reusable compound');
      end;
  end;
end;

function RoleID(AApp, AInstance: Integer; ACompound: TCompound;
  const APath: TNyxPartRef): TNyxText;
begin
  Result := Owner(AApp, AInstance, ACompound).Part(APath).ID;
end;

function Rows(AApp, AInstance: Integer; ACompound: TCompound): INyxCollectionView;
var
  LID: TNyxText;
begin
  case Ord(ACompound) of
    Ord(rcSummary):
      LID := RoleID(AApp, AInstance, ACompound, SelectionSummaryItems);
    Ord(rcDetail):
      LID := RoleID(AApp, AInstance, ACompound, NyxPart('master'));
    Ord(rcKanban):
      LID := RoleID(AApp, AInstance, ACompound, NyxPart('lane-0/items'));
    else
      begin
        raise Exception.Create('Unknown reusable collection role');
      end;
  end;
  Result := GApps[AApp].View.CollectionView(LID);

  if Result = nil then
  begin
    raise Exception.Create('The nested list has no ordinary application collection view');
  end;
end;

function Face(AApp: Integer; const AID: TNyxText): TReviewFace;
begin
  {$ifdef PAS2JS}
  Result := GApps[AApp].View.ElementFor(AID);
  {$else}
  Result := GApps[AApp].View.ControlFor(AID);
  {$endif}

  if Result = nil then
  begin
    raise Exception.Create('The accepted nested role has no platform face: ' + AID);
  end;
end;

function FaceText(AApp: Integer; const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := Face(AApp, AID).textContent;
  {$else}
  Result := TLabel(Face(AApp, AID)).Caption;
  {$endif}
end;

function FaceEnabled(AApp: Integer; const AID: TNyxText): Boolean;
begin
  {$ifdef PAS2JS}
  Result := not TJSHTMLButtonElement(Face(AApp, AID)).disabled;
  {$else}
  Result := Face(AApp, AID).Enabled;
  {$endif}
end;

procedure Pump;
begin
  {$ifndef PAS2JS}
  Application.ProcessMessages;
  CheckSynchronize(1);
  {$endif}
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
{$endif}

{ Completion predicates retain their managed binding across browser suspension.
  A turn only permits publication; elapsed time never stands in for success. }
procedure AwaitSummary(AApp, AInstance, ACount: Integer); {$ifdef PAS2JS}async;{$endif}
var
  LBinding: INyxSelectionSummaryBinding;
  LCountID: TNyxText;
  LClearID: TNyxText;
  {$ifdef PAS2JS}
  LStarted: Double;
  {$else}
  LStarted: QWord;
  {$endif}
begin
  LBinding := GSummaries[BindingIndex(AApp, AInstance)];
  LCountID := RoleID(AApp, AInstance, rcSummary, SelectionSummaryCount);
  LClearID := RoleID(AApp, AInstance, rcSummary, SelectionSummaryClear);
  {$ifdef PAS2JS}LStarted := TJSDate.now;{$else}LStarted := GetTickCount64;{$endif}
  while (LBinding.SelectedCount <> ACount) or
    (FaceText(AApp, LCountID) <> SelectionSummaryCaption(ACount)) or
    (FaceEnabled(AApp, LClearID) <> (ACount > 0)) do
  begin
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 10000 then
    {$else}

    if GetTickCount64 - LStarted > 10000 then
    {$endif}
    begin
      raise Exception.Create('Summary completion: cached=' + IntToStr(LBinding.SelectedCount) +
        ', physical=' + FaceText(AApp, LCountID));
    end;
    {$ifdef PAS2JS}await(UITurn);{$else}Pump;{$endif}
  end;
  Check(LBinding.Connected, 'the exact creator binding completes physical presentation');
end;

procedure AwaitDetail(AApp, AInstance: Integer; const ATitle: TNyxText;
  AEnabled: Boolean); {$ifdef PAS2JS}async;{$endif}
var
  LBinding: INyxMasterDetailBinding;
  LTitleID: TNyxText;
  LEditID: TNyxText;
  {$ifdef PAS2JS}
  LStarted: Double;
  {$else}
  LStarted: QWord;
  {$endif}
begin
  LBinding := GDetails[BindingIndex(AApp, AInstance)];
  LTitleID := RoleID(AApp, AInstance, rcDetail, NyxPart('detail/title'));
  LEditID := RoleID(AApp, AInstance, rcDetail, NyxPart('detail/edit'));
  {$ifdef PAS2JS}LStarted := TJSDate.now;{$else}LStarted := GetTickCount64;{$endif}
  while (FaceText(AApp, LTitleID) <> ATitle) or
    (FaceEnabled(AApp, LEditID) <> AEnabled) do
  begin
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 10000 then
    {$else}

    if GetTickCount64 - LStarted > 10000 then
    {$endif}
    begin
      raise Exception.Create('MasterDetail completion: ' + FaceText(AApp, LTitleID));
    end;
    {$ifdef PAS2JS}await(UITurn);{$else}Pump;{$endif}
  end;
  Check(LBinding.Connected, 'the migrated detail owner completes actual title/Edit projection');
end;

function SameSnapshot(const ALeft, ARight: INyxCollectionSnapshot): Boolean;
var
  LIndex: Integer;
begin
  Result := (ALeft.Key.Name = ARight.Key.Name) and
    ALeft.Schema.SameSchema(ARight.Schema) and (ALeft.Revision = ARight.Revision) and
    (ALeft.Count = ARight.Count);

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to ALeft.Count - 1 do
  begin

    if not ALeft.ItemAt(LIndex).SameItem(ARight.ItemAt(LIndex)) then
    begin
      Exit(False);
    end;
  end;
end;

procedure CheckStores(AExcept: Integer = -1);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(GStores) do
  begin

    if LIndex <> AExcept then
    begin
      Check(SameSnapshot(GStores[LIndex].Snapshot, GBaselines[LIndex]),
        'every other accepted instance/application store retains exact rows and revision');
    end;
  end;
end;

procedure CheckAuthored;
begin
  Check(TNyxCodec.Encode(GDocument) = GDesign,
    'definitions, occurrence descriptors, sibling ownership and declared defaults remain exact');
  Check(TNyxCodegen.Generate(GDocument, 'nyx.generated.coordinator.reuse') = GSource,
    'runtime input and navigation preserve the complete authored Pascal source');
end;

{ The second page is deliberately incomplete from the start. Both App contracts
  borrow an immutable document, so refusal never mutates mounted descriptors. }
function Fixture: TNyxDocument;
var
  LWorkspace: INyxCard;
  LInsights: INyxCard;
  LSummary: INyxSelectionSummary;
  LDetail: INyxMasterDetail;
  LBoard: INyxKanbanBoard;
  LOptions: TNyxKanbanOptions;
  LPage: INyxPage;
  LInstance: INyxComponent;
  LIndex: Integer;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'A shared design, independent work';
    Result.Collections.Define(SummaryPeople,
      NyxCollectionSchema.Text(SummaryName, ''), [
      NyxCollectionItem(NyxItem(SummaryPeople, 'alder')).WithValue(SummaryName, 'Alder'),
      NyxCollectionItem(NyxItem(SummaryPeople, 'birch')).WithValue(SummaryName, 'Birch'),
      NyxCollectionItem(NyxItem(SummaryPeople, 'cedar')).WithValue(SummaryName, 'Cedar')]);
    Result.Collections.Define(DetailPeople,
      NyxCollectionSchema.Text(DetailTitle, '').Text(DetailDescription, ''), [
      NyxCollectionItem(NyxItem(DetailPeople, 'ada')).WithValue(DetailTitle, 'Ada Lovelace').
        WithValue(DetailDescription, CDescription),
      NyxCollectionItem(NyxItem(DetailPeople, 'grace')).WithValue(DetailTitle, 'Grace Hopper').
        WithValue(DetailDescription, 'A second independently owned record.')]);
    Result.Collections.Define(WorkflowCards,
      NyxCollectionSchema.Text(CardTitle, '').Text(CardLane, Plan.Name), [
      NyxCollectionItem(Card).WithValue(CardTitle, 'Make something welcoming').
        WithValue(CardLane, Plan.Name)]);
    LWorkspace := NewNyxCard(CWorkspace);
    LWorkspace.Configure.Layout(nlColumn).Gap(12).Padding(12).Done;
    LInsights := NewNyxCard('insights');
    LInsights.Configure.PartName(NyxPart('insights')).Layout(nlColumn).Gap(12).Done;
    LSummary := NewSelectionSummary('selection', NyxCollectionView(SummaryPeople).
      Column(SummaryName, 'People').Selection(nsmMultiple).Scoped(csInstance));
    LSummary.Configure.PartName(NyxPart('selection')).Done;
    LSummary.ItemsList.Configure.Height(96).Done;
    Check(LSummary.CountLabel.Node = LSummary.Node.Part(SelectionSummaryCount),
      'the creator authoring helper supplies exact ordinary typed parts');
    LInsights.Add(LSummary);
    LDetail := NewNyxMasterDetail('directory');
    LDetail.Configure.PartName(NyxPart('directory')).Done;
    LDetail.MasterList.Configure.Height(96).Done;
    LDetail.MasterList.Binds.Collection(NyxCollectionView(DetailPeople).
      Column(DetailTitle, 'People').Selection(nsmSingle).Scoped(csInstance));
    LDetail.DetailTitleHeading.WithText(CPlaceholder);
    SetNyxMasterDetailIntent(LDetail, TNyxMasterDetailIntent.Configured(
      NyxMasterDetailOptions(DetailTitle, DetailDescription)));
    LInsights.Add(LDetail);
    LBoard := NewNyxKanbanBoard('workflow');
    LBoard.Configure.PartName(NyxPart('workflow')).Done;
    LOptions := NyxKanbanOptions(WorkflowCards, CardTitle, CardLane).
      Lane(Plan, 'Plan').Lane(Build, 'Build').Lane(Complete, 'Complete').Scoped(csInstance);
    ConfigureNyxKanbanBoard(LBoard, LOptions);
    SetNyxKanbanIntent(LBoard, TNyxKanbanIntent.Configured(LOptions));
    LBoard.Lane0ItemsList.Configure.Height(96).Done;
    LBoard.Lane1ItemsList.Configure.Height(96).Done;
    LBoard.Lane2ItemsList.Configure.Height(96).Done;
    LInsights.Add(LBoard);
    LWorkspace.Add(LInsights);
    Result.AddComponent(LWorkspace);
    LPage := NewNyxPage(CHome);
    LPage.Configure.Padding(12).Gap(12).Done;
    for LIndex := 0 to 1 do
    begin
      LInstance := NewNyxComponent(InstanceID(LIndex));
      LInstance.Reference := NyxComponent(CWorkspace);
      LPage.Add(LInstance);
    end;
    Result.AddPage(LPage);
    LPage := NewNyxPage(CRefusal);
    LInstance := NewNyxComponent('refusal-workspace');
    LInstance.Reference := NyxComponent(CWorkspace);
    LInstance.Node.OverridePart(NyxPart('insights/selection/selection-count'), noRemove).
      Named('missing-count');
    LPage.Add(LInstance);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure CaptureBindings(AApp: Integer; AFirst: Boolean);
var
  LInstance: Integer;
  LSlot: Integer;
  LKind: TCompound;
  LCommon: INyxCoordinatorBinding;
  LSummary: INyxSelectionSummaryBinding;
  LDetail: INyxMasterDetailBinding;
  LBoard: INyxKanbanBinding;
  LControl: INyxControl;
  LList: INyxList;
  LLabel: INyxLabel;
  LButton: INyxButton;
  LStore: INyxCollection;
begin
  for LInstance := 0 to 1 do
  begin
    LSlot := BindingIndex(AApp, LInstance);
    LCommon := GApps[AApp].View.CoordinatorBinding(Owner(AApp, LInstance, rcSummary).ID);
    { Query locals own temporary leases. Retain a separate global cell only
      after the same capability/connection assertion succeeds. }
    LSummary := nil;
    Check(Supports(LCommon, INyxSelectionSummaryBinding, LSummary) and
      LSummary.Connected,
      'generic application lookup preserves the exact creator specialized binding');
    GSummaries[LSlot] := LSummary;
    LCommon := GApps[AApp].View.CoordinatorBinding(Owner(AApp, LInstance, rcDetail).ID);
    LDetail := nil;
    Check(Supports(LCommon, INyxMasterDetailBinding, LDetail) and
      (LDetail = GApps[AApp].View.MasterDetailBinding(Owner(AApp, LInstance, rcDetail).ID)) and
      LDetail.Connected, 'common and typed detail lookup have one accepted identity');
    GDetails[LSlot] := LDetail;
    LCommon := GApps[AApp].View.CoordinatorBinding(Owner(AApp, LInstance, rcKanban).ID);
    LBoard := nil;
    Check(Supports(LCommon, INyxKanbanBinding, LBoard) and
      (LBoard = GApps[AApp].View.KanbanBinding(Owner(AApp, LInstance, rcKanban).ID)) and
      LBoard.Connected, 'common and typed board lookup have one accepted identity');
    GBoards[LSlot] := LBoard;
    LControl := RetainNyxControl(Owner(AApp, LInstance, rcSummary));
    LList := nil;
    LLabel := nil;
    LButton := nil;
    Check(Supports(LControl.Part(SelectionSummaryItems), INyxList, LList) and
      Supports(LControl.Part(SelectionSummaryCount), INyxLabel, LLabel) and
      Supports(LControl.Part(SelectionSummaryClear), INyxButton, LButton),
      'realized creator Card exposes ordinary typed nested list/label/button parts');
    for LKind := Low(TCompound) to High(TCompound) do
    begin
      LStore := Rows(AApp, LInstance, LKind).Store;

      if AFirst then
      begin
        GStores[StoreIndex(AApp, LInstance, LKind)] := LStore;
        GBaselines[StoreIndex(AApp, LInstance, LKind)] := LStore.Snapshot;
      end
      else
      begin
        Check(GStores[StoreIndex(AApp, LInstance, LKind)] = LStore,
          'ordinary navigation/remount preserves the same accepted instance store');
      end;
    end;
  end;
end;

{ All physical input goes through actual application-owned widget/DOM handlers.
  Browser trusted input and hardware remain separate qualification boundaries. }
procedure SelectSummary(AApp, AInstance: Integer);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LRow: TJSHTMLElement;
  LOptions: TReviewMouseOptions;
  {$else}
  LList: TListBox;
  {$endif}
begin
  LID := RoleID(AApp, AInstance, rcSummary, SelectionSummaryItems);
  GApps[AApp].View.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  LRow := TJSHTMLElement(Face(AApp, LID).querySelector('[data-nyx-item="alder"]'));
  Check(LRow <> nil, 'the actual first selected summary row exists');
  LRow.click;
  LOptions := TReviewMouseOptions.new;
  LOptions.bubbles := True;
  LOptions.cancelable := True;
  LOptions.ctrlKey := True;
  LRow := TJSHTMLElement(Face(AApp, LID).querySelector('[data-nyx-item="birch"]'));
  Check(LRow <> nil, 'the actual second selected summary row exists');
  LRow.dispatchEvent(TReviewMouseEvent.new('click', LOptions));
  {$else}
  GApps[AApp].Window.BringToFront;
  LList := TListBox(Face(AApp, LID));
  Check(LList.MultiSelect and (LList.Items.Count = 3),
    'reusable summary keeps its authored multiple-selection list');
  LList.SetFocus;
  LList.ItemIndex := 0;
  LList.Selected[0] := True;
  LList.Selected[1] := True;
  Check(Assigned(LList.OnSelectionChange), 'ordinary summary selection handler is mounted');
  LList.OnSelectionChange(LList, True);
  {$endif}
  Pump;
end;

procedure PressButton(AApp: Integer; const AID: TNyxText);
{$ifndef PAS2JS}
var
  LButton: TWinControl;
{$endif}
begin
  GApps[AApp].View.Reveal(AID, niRuntime);
  Check(FaceEnabled(AApp, AID), 'the actual action is enabled before activation');
  {$ifdef PAS2JS}
  Face(AApp, AID).click;
  {$else}
  GApps[AApp].Window.BringToFront;
  LButton := TWinControl(Face(AApp, AID));
  LButton.SetFocus;
  Check(GApps[AApp].Window.ActiveControl = LButton,
    'the ordinary action has actual native keyboard focus');

  if LButton.Perform(CN_KEYDOWN, VK_SPACE, 0) = 0 then
  begin
    LButton.Perform(LM_KEYDOWN, VK_SPACE, 0);
  end;

  if LButton.Perform(CN_KEYUP, VK_SPACE, 0) = 0 then
  begin
    LButton.Perform(LM_KEYUP, VK_SPACE, 0);
  end;
  {$endif}
  Pump;
end;

procedure SelectDetail(AApp, AInstance: Integer);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LRow: TJSHTMLElement;
  {$else}
  LList: TListBox;
  {$endif}
begin
  LID := RoleID(AApp, AInstance, rcDetail, NyxPart('master'));
  GApps[AApp].View.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  LRow := TJSHTMLElement(Face(AApp, LID).querySelector('[data-nyx-item="ada"]'));
  Check(LRow <> nil, 'the actual detail source row exists');
  LRow.click;
  {$else}
  GApps[AApp].Window.BringToFront;
  LList := TListBox(Face(AApp, LID));
  Check(not LList.MultiSelect and (LList.Items.Count = 2),
    'reusable detail source remains single-selection');
  LList.SetFocus;
  LList.ItemIndex := 0;
  Check(Assigned(LList.OnSelectionChange), 'ordinary detail selection handler is mounted');
  LList.OnSelectionChange(LList, True);
  {$endif}
  Pump;
end;

procedure MoveCard(AApp, AInstance: Integer);
var
  LBinding: INyxKanbanBinding;
  LView: INyxCollectionView;
  LRevision: Integer;
  LID: TNyxText;
  {$ifdef PAS2JS}
  LRow: TJSHTMLElement;
  {$else}
  LList: TListBox;
  {$endif}
begin
  LBinding := GBoards[BindingIndex(AApp, AInstance)];
  LID := LBinding.Items(Plan).ID;
  LView := GApps[AApp].View.CollectionView(LID);
  LRevision := LView.Store.Snapshot.Revision;
  GApps[AApp].View.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  LRow := TJSHTMLElement(Face(AApp, LID).querySelector('[data-nyx-item="welcome"]'));
  Check(LRow <> nil, 'the actual source card row exists');
  LRow.click;
  {$else}
  GApps[AApp].Window.BringToFront;
  LList := TListBox(Face(AApp, LID));
  Check(LList.Items.Count = 1, 'the initial reusable lane contains its one stable card');
  LList.SetFocus;
  LList.ItemIndex := 0;
  Check(Assigned(LList.OnSelectionChange), 'ordinary lane selection handler is mounted');
  LList.OnSelectionChange(LList, True);
  {$endif}
  Pump;
  Check(LView.HasSelection and (LView.Selected.ID = Card.ID),
    'actual lane input selects the stable logical card');
  PressButton(AApp, LBinding.MoveButton(Build).ID);
  Check((LView.Store.Snapshot.Revision = LRevision + 1) and
    (LView.Store.Snapshot.Item(Card).GetValue(CardLane) = Build.Name) and
    GApps[AApp].View.CollectionView(LBinding.Items(Build).ID).Snapshot.Has(Card),
    'actual Move uses the existing store/revision owner and preserves card identity');
end;

procedure Cases; {$ifdef PAS2JS}async;{$endif}
var
  LApp: Integer;
  LInstance: Integer;
  LIndex: Integer;
  LOther: Integer;
  LQuery: TNyxCollectionQuery;
  LQueryText: TNyxText;
  LSummary: INyxSelectionSummaryBinding;
  LDetail: INyxMasterDetailBinding;
  LBoard: INyxKanbanBinding;
  LMove: INyxKanbanMove;
  LRoot: TNyxNode;
  LFace: TReviewFace;
  LListID: TNyxText;
  LGeneration: INyxCancellationScope;
  LRejected: Boolean;
begin
  GDocument := Fixture;
  GDesign := TNyxCodec.Encode(GDocument);
  GSource := TNyxCodegen.Generate(GDocument, 'nyx.generated.coordinator.reuse');
  GRegistry := NewNyxCoordinatorRegistry;
  RegisterSelectionSummary(GRegistry);
  for LApp := 0 to 1 do
  begin
    GApps[LApp] := TReviewApplication.Create;
    GApps[LApp].View.CoordinatorRegistry := GRegistry;
    {$ifdef PAS2JS}
    GHosts[LApp] := TJSHTMLElement(document.createElement('main'));
    GHosts[LApp].style.cssText := 'min-height:900px;width:100%;';
    document.body.appendChild(GHosts[LApp]);
    GApps[LApp].Run(GDocument, GHosts[LApp]);
    {$else}
    GApps[LApp].Mount(GDocument);
    GApps[LApp].Window.ClientWidth := GWidth;
    GApps[LApp].Window.ClientHeight := 900;
    GApps[LApp].Window.Show;
    Check(GApps[LApp].Window.ClientWidth = GWidth,
      'each ordinary native application has the requested actual client allocation');
    {$endif}
    Pump;
    CaptureBindings(LApp, True);
    for LInstance := 0 to 1 do
    begin
      {$ifdef PAS2JS}
      await(AwaitSummary(LApp, LInstance, 0));
      await(AwaitDetail(LApp, LInstance, CPlaceholder, False));
      {$else}
      AwaitSummary(LApp, LInstance, 0);
      AwaitDetail(LApp, LInstance, CPlaceholder, False);
      {$endif}
    end;
  end;
  for LIndex := 0 to High(GStores) do
  begin
    for LOther := LIndex + 1 to High(GStores) do
    begin
      Check(GStores[LIndex] <> GStores[LOther],
        'all twelve accepted home stores have independent managed identities');
    end;
  end;
  SelectSummary(0, 0);
  {$ifdef PAS2JS}await(AwaitSummary(0, 0, 2));{$else}AwaitSummary(0, 0, 2);{$endif}
  for LIndex := 1 to High(GSummaries) do
  begin
    LApp := LIndex div 2;
    LInstance := LIndex mod 2;
    {$ifdef PAS2JS}await(AwaitSummary(LApp, LInstance, 0));
    {$else}AwaitSummary(LApp, LInstance, 0);{$endif}
    Check(Rows(LApp, LInstance, rcSummary).Selection.Count = 0,
      'first-occurrence input leaves every sibling and second-App membership empty');
  end;
  LQuery := NyxCollectionQuery.Where(NyxWhere(SummaryName).EqualTo('Cedar'));
  Rows(0, 0, rcSummary).ConfigureQuery(LQuery);
  {$ifdef PAS2JS}await(AwaitSummary(0, 0, 2));{$else}AwaitSummary(0, 0, 2);{$endif}
  Check((Rows(0, 0, rcSummary).Snapshot.Count = 1) and
    (Rows(0, 0, rcSummary).Selection.Count = 2),
    'query-hidden selected members remain selected and truthfully counted');
  LQueryText := Rows(0, 0, rcSummary).QueryPolicy.ToData.ToJSON;
  PressButton(0, RoleID(0, 0, rcSummary, SelectionSummaryClear));
  {$ifdef PAS2JS}await(AwaitSummary(0, 0, 0));{$else}AwaitSummary(0, 0, 0);{$endif}
  Check((Rows(0, 0, rcSummary).Selection.Count = 0) and
    (Rows(0, 0, rcSummary).Snapshot.Count = 1) and
    (Rows(0, 0, rcSummary).QueryPolicy.ToData.ToJSON = LQueryText),
    'real Clear resets retained membership without rewriting the query or rows');
  Rows(0, 0, rcSummary).ConfigureQuery(NyxCollectionQuery);
  for LApp := 0 to 1 do
  begin
    for LInstance := 0 to 1 do
    begin
      Check((Rows(LApp, LInstance, rcSummary).Selection.Count = 0) and
        (GSummaries[BindingIndex(LApp, LInstance)].SelectedCount = 0),
        'first application selection and Clear leave both siblings/apps independent');
    end;
  end;
  SelectSummary(0, 1);
  {$ifdef PAS2JS}await(AwaitSummary(0, 1, 2));{$else}AwaitSummary(0, 1, 2);{$endif}
  Check((GSummaries[0].SelectedCount = 0) and (GSummaries[2].SelectedCount = 0) and
    (GSummaries[3].SelectedCount = 0) and
    (Rows(0, 0, rcSummary).Selection.Count = 0),
    'actual second-occurrence input resolves its own nested roles and leaves all peers alone');
  PressButton(0, RoleID(0, 1, rcSummary, SelectionSummaryClear));
  {$ifdef PAS2JS}await(AwaitSummary(0, 1, 0));{$else}AwaitSummary(0, 1, 0);{$endif}
  CheckStores;
  SelectDetail(0, 0);
  {$ifdef PAS2JS}await(AwaitDetail(0, 0, 'Ada Lovelace', True));
  {$else}AwaitDetail(0, 0, 'Ada Lovelace', True);{$endif}
  Check(GDetails[0].CaptureEdit.Item.GetValue(DetailDescription) = CDescription,
    'the nested migrated detail owner captures its exact independent record');
  for LIndex := 1 to High(GDetails) do
  begin
    LApp := LIndex div 2;
    LInstance := LIndex mod 2;
    {$ifdef PAS2JS}await(AwaitDetail(LApp, LInstance, CPlaceholder, False));
    {$else}AwaitDetail(LApp, LInstance, CPlaceholder, False);{$endif}
  end;
  MoveCard(0, 0);
  LIndex := StoreIndex(0, 0, rcKanban);
  CheckStores(LIndex);
  Check(GStores[LIndex].Snapshot.Item(Card).GetValue(CardTitle) = 'Make something welcoming',
    'nested card movement preserves the authored title and other record data');
  GBaselines[LIndex] := GStores[LIndex].Snapshot;
  CheckAuthored;

  { A permanently authored invalid page proves refusal without changing either
    App's borrowed document. Its never-accepted stores are not the twelve home
    owners measured above. The current physical list must remain usable. }
  SelectSummary(0, 0);
  {$ifdef PAS2JS}await(AwaitSummary(0, 0, 2));{$else}AwaitSummary(0, 0, 2);{$endif}
  LSummary := GSummaries[0];
  LDetail := GDetails[0];
  LBoard := GBoards[0];
  LRoot := GApps[0].View.Root;
  LGeneration := NyxEventGeneration(GApps[0].View.Events);
  LListID := RoleID(0, 0, rcSummary, SelectionSummaryItems);
  LFace := Face(0, LListID);
  LRejected := False;
  try
    GApps[0].ShowPage(CRefusal);
  except
    on ENyxModel do
    begin
      LRejected := True;
    end;
    on ENyxCoordinator do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (GApps[0].View.Root = LRoot) and
    (NyxEventGeneration(GApps[0].View.Events) = LGeneration) and
    not LGeneration.Cancelled and LSummary.Connected and LDetail.Connected and
    LBoard.Connected and (Face(0, LListID) = LFace),
    'missing nested required role refuses before accepted root/token/physical input retire');
  PressButton(0, RoleID(0, 0, rcSummary, SelectionSummaryClear));
  {$ifdef PAS2JS}await(AwaitSummary(0, 0, 0));{$else}AwaitSummary(0, 0, 0);{$endif}
  CheckStores;
  CheckAuthored;
  LMove := LBoard.CaptureMove(Card, Build, Plan,
    GStores[StoreIndex(0, 0, rcKanban)].Snapshot.Revision);
  GApps[0].ShowPage(CHome);
  CaptureBindings(0, False);
  Check(LGeneration.Cancelled and not LSummary.Connected and not LDetail.Connected and
    not LBoard.Connected and (GSummaries[0] <> LSummary) and (GDetails[0] <> LDetail) and
    (GBoards[0] <> LBoard),
    'ordinary same-page replacement adopts fresh bindings and retires all former commands');
  LRejected := False;
  try
    LSummary.Clear;
  except
    on ENyxCoordinator do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'retained creator command cannot mutate a same-ID successor');
  LRejected := False;
  try
    LDetail.CaptureEdit;
  except
    on ENyxMasterDetailCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'retained built-in detail command cannot read successor selection');
  LRejected := False;
  try
    LMove.Commit;
  except
    on ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and not LMove.Committed,
    'captured card command cannot cross ordinary application replacement');
  {$ifdef PAS2JS}await(AwaitSummary(0, 0, 0));{$else}AwaitSummary(0, 0, 0);{$endif}
  SelectSummary(0, 0);
  {$ifdef PAS2JS}await(AwaitSummary(0, 0, 2));{$else}AwaitSummary(0, 0, 2);{$endif}
  PressButton(0, RoleID(0, 0, rcSummary, SelectionSummaryClear));
  {$ifdef PAS2JS}await(AwaitSummary(0, 0, 0));{$else}AwaitSummary(0, 0, 0);{$endif}
  LSummary := GSummaries[0];
  LDetail := GDetails[0];
  LBoard := GBoards[0];
  LGeneration := NyxEventGeneration(GApps[0].View.Events);
  GApps[0].View.Unmount;
  Check(not LSummary.Connected and not LDetail.Connected and not LBoard.Connected and
    LGeneration.Cancelled, 'application view Unmount revokes creator and both built-in owners');
  GApps[0].ShowPage(CHome);
  CaptureBindings(0, False);
  SelectSummary(0, 0);
  {$ifdef PAS2JS}await(AwaitSummary(0, 0, 2));{$else}AwaitSummary(0, 0, 2);{$endif}
  PressButton(0, RoleID(0, 0, rcSummary, SelectionSummaryClear));
  {$ifdef PAS2JS}await(AwaitSummary(0, 0, 0));{$else}AwaitSummary(0, 0, 0);{$endif}
  CheckStores;
  CheckAuthored;

  { App disposal owns renderer retirement. Retained custom/built-in interfaces
    and immutable store snapshots remain safe, with no borrowed view accesses. }
  FreeAndNil(GApps[0]);
  for LInstance := 0 to 1 do
  begin
    LIndex := BindingIndex(0, LInstance);
    Check(not GSummaries[LIndex].Connected and not GDetails[LIndex].Connected and
      not GBoards[LIndex].Connected,
      'disposing one ordinary App revokes every retained instance behavior');
  end;
  Check(GSummaries[2].Connected and GDetails[2].Connected and GBoards[2].Connected,
    'the independently owned second App survives first App disposal');
  SelectSummary(1, 0);
  {$ifdef PAS2JS}await(AwaitSummary(1, 0, 2));{$else}AwaitSummary(1, 0, 2);{$endif}
  Check((GSummaries[3].SelectedCount = 0) and
    (Rows(1, 1, rcSummary).Selection.Count = 0),
    'surviving application input still targets only its chosen occurrence');
  PressButton(1, RoleID(1, 0, rcSummary, SelectionSummaryClear));
  {$ifdef PAS2JS}await(AwaitSummary(1, 0, 0));{$else}AwaitSummary(1, 0, 0);{$endif}
  CheckStores;
  CheckAuthored;
  FreeAndNil(GApps[1]);
  for LIndex := 0 to High(GSummaries) do
  begin
    Check(not GSummaries[LIndex].Connected and not GDetails[LIndex].Connected and
      not GBoards[LIndex].Connected,
      'ordinary application disposal leaves every retained behavior disconnected');
  end;
end;

{$ifndef PAS2JS}
procedure WriteEvidence(const AName: String; const AText: TNyxText);
var
  LStream: TFileStream;
begin

  if GEvidence = '' then
  begin
    Exit;
  end;
  ForceDirectories(GEvidence);
  LStream := TFileStream.Create(IncludeTrailingPathDelimiter(GEvidence) + AName, fmCreate);
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

{ The late key uses the same public creator factory and copied versioned intent.
  Its presence tests membership capture, not a second coordinator implementation. }
function FuturePeople: TNyxCollectionRef;
begin
  Result := NyxCollection('future-summary-people');
end;

function EmbeddedItem: TNyxCanvasItemRef;
begin
  Result := NyxCanvasItem('Workspace');
end;

function NewFutureSummary(const AID, APart: TNyxText): INyxSelectionSummary;
begin
  Result := NewSelectionSummary(AID, NyxCollectionView(FuturePeople).
    Column(SummaryName, 'Future people').Selection(nsmMultiple).Scoped(csInstance));
  Result.Configure.PartName(NyxPart(APart)).Done;
  Result.ItemsList.Configure.Height(64).Done;
  Result.Extensions.Remove(SelectionSummaryIntent);
  Result.Extensions.SetValue(NyxExtension(CFutureIntent),
    NyxObject([NyxField('version', NyxData(1))]));
end;

constructor TEmbeddedReview.Create;
begin
  inherited Create;
  BuildFixture;
  FRegistry := NewNyxCoordinatorRegistry;
  RegisterSelectionSummary(FRegistry);
end;

destructor TEmbeddedReview.Destroy;
var
  LApp: Integer;
begin
  for LApp := 0 to 1 do
  begin
    FreeAndNil(FApps[LApp]);
    {$ifdef PAS2JS}

    if FHosts[LApp] <> nil then
    begin
      FHosts[LApp].remove;
      FHosts[LApp] := nil;
    end;
    {$endif}
  end;
  FRegistry := nil;
  FDocument.Free;
  inherited Destroy;
end;

procedure TEmbeddedReview.BuildFixture;
var
  LWorkspace: TNyxNode;
  LDecoration: INyxCard;
  LSlot: INyxComponent;
  LInput: INyxInput;
  LPage: INyxPage;
  LSummary: INyxSelectionSummary;
  LDefinition: INyxCanvasDefinition;
  LLayer: TNyxCanvasLayerRef;
begin
  { Fixture is unchanged R1 source. This independent copy is completed before
    either ordinary App borrows it, and never edited during mounted lifetimes. }
  FDocument := Fixture;
  FDocument.Collections.Define(FuturePeople,
    NyxCollectionSchema.Text(SummaryName, ''), [
    NyxCollectionItem(NyxItem(FuturePeople, 'alder')).WithValue(SummaryName, 'Alder'),
    NyxCollectionItem(NyxItem(FuturePeople, 'birch')).WithValue(SummaryName, 'Birch'),
    NyxCollectionItem(NyxItem(FuturePeople, 'cedar')).WithValue(SummaryName, 'Cedar')]);
  FDocument.Presentations.Define(NyxPresentation(CAlternate),
    TNyxPresentationCondition.Manual);
  LDecoration := NewNyxCard('default-decoration');
  LDecoration.Add(NewNyxLabel('default-decoration-text').WithText('Default decoration'));
  FDocument.AddComponent(LDecoration);
  LDecoration := NewNyxCard(CAlternate);
  LDecoration.Add(NewNyxLabel('alternate-decoration-text').WithText('Alternate decoration'));
  LDecoration.Add(NewNyxLabel('alternate-extra-text').WithText('A structural extra face'));
  FDocument.AddComponent(LDecoration);
  LWorkspace := FDocument.FindComponent(CWorkspace);
  LWorkspace.Part(NyxPart('insights')).Add(NewFutureSummary('future-selection', 'future'));
  LInput := NewNyxInput('embedded-note');
  LInput.Configure.PartName(NyxPart('note')).Done;
  LInput.Value := 'Child note default';
  LWorkspace.Add(LInput);
  LSlot := NewNyxComponent('embedded-decoration');
  LSlot.Configure.PartName(NyxPart('decoration')).Done;
  LSlot.Content.Use(NyxComponent('default-decoration')).
    WhenPresentation(NyxPresentation(CAlternate)).Use(NyxComponent(CAlternate)).Done;
  LWorkspace.Add(LSlot);
  LLayer := NyxCanvasLayer('Workspace');
  LDefinition := NewNyxCanvasDefinition.WithLayer(LLayer).
    WithCamera(NyxCanvasCamera(700, 560, NyxCanvasPoint(260, 450))).
    WithScaling(nccsReflow).
    WithItem(EmbeddedItem, NyxReusableRoot(CWorkspace),
      NyxCanvasPlacement(LLayer, NyxCanvasBounds(0, 0, 520, 900)));
  FDocument.Canvases.Define(NyxCanvasRef('embedded-scene'), LDefinition);
  LPage := NewNyxPage(CEmbeddedHome);
  LPage.Configure.Padding(8).Gap(8).Done;
  LSummary := NewSelectionSummary('parent-selection', NyxCollectionView(SummaryPeople).
    Column(SummaryName, 'Parent people').Selection(nsmMultiple).Scoped(csInstance));
  LSummary.Configure.PartName(NyxPart('parent-selection')).Done;
  LSummary.ItemsList.Configure.Height(64).Done;
  LPage.Add(LSummary);
  LPage.Add(NewFutureSummary('parent-future', 'parent-future'));
  LPage.Add(NewNyxCanvas(CEmbeddedCanvas).WithScene(NyxCanvasRef('embedded-scene')).
    Configure.Height(560).MinimumHeight(300).Done);
  FDocument.AddPage(LPage);
  FDocument.Validate;
  FDesign := TNyxCodec.Encode(FDocument);
  FSource := TNyxCodegen.Generate(FDocument, 'nyx.generated.coordinator.embedded');
end;

function TEmbeddedReview.Canvas(AApp: Integer): TEmbeddedCanvas;
begin
  Result := nil;

  if not Supports(FApps[AApp].View.CanvasView(CEmbeddedCanvas, niRuntime),
    TEmbeddedCanvas, Result) then
  begin
    raise Exception.Create('The ordinary App has no platform embedded canvas view');
  end;
end;

function TEmbeddedReview.Child(AApp: Integer): TEmbeddedRenderer;
begin
  Result := Canvas(AApp).RendererFor(EmbeddedItem);

  if Result = nil then
  begin
    raise Exception.Create('The accepted saved item has no child renderer');
  end;
end;

function TEmbeddedReview.Role(AApp: Integer; const APath: TNyxPartRef): TNyxNode;
begin
  Result := Child(AApp).Root.Part(APath);

  if Result = nil then
  begin
    raise Exception.Create('The child has no required named role: ' + APath.Name);
  end;
end;

function TEmbeddedReview.Summary(AApp: Integer;
  AFuture: Boolean): INyxSelectionSummaryBinding;
var
  LPath: TNyxPartRef;
  LCommon: INyxCoordinatorBinding;
begin
  Result := nil;

  if AFuture then
  begin
    LPath := NyxPart('insights/future');
  end
  else
  begin
    LPath := NyxPart('insights/selection');
  end;
  LCommon := Child(AApp).CoordinatorBinding(Role(AApp, LPath).ID);

  if LCommon <> nil then
  begin
    Check(Supports(LCommon, INyxSelectionSummaryBinding, Result),
      'the child preserves the creator specialized command interface');
  end;
end;

function TEmbeddedReview.ParentSummary(AApp: Integer;
  AFuture: Boolean): INyxSelectionSummaryBinding;
var
  LPath: TNyxPartRef;
  LCommon: INyxCoordinatorBinding;
begin
  Result := nil;

  if AFuture then
  begin
    LPath := NyxPart('parent-future');
  end
  else
  begin
    LPath := NyxPart('parent-selection');
  end;
  LCommon := FApps[AApp].View.CoordinatorBinding(FApps[AApp].View.Root.Part(LPath).ID);

  if LCommon <> nil then
  begin
    Check(Supports(LCommon, INyxSelectionSummaryBinding, Result),
      'the parent preserves the creator specialized command interface');
  end;
end;

function TEmbeddedReview.Detail(AApp: Integer): INyxMasterDetailBinding;
var
  LCommon: INyxCoordinatorBinding;
begin
  LCommon := Child(AApp).CoordinatorBinding(Role(AApp, NyxPart('insights/directory')).ID);
  Check(Supports(LCommon, INyxMasterDetailBinding, Result) and Result.Connected and
    (Result = Child(AApp).MasterDetailBinding(Role(AApp, NyxPart('insights/directory')).ID)),
    'embedded built-in detail remains one common and specialized owner');
end;

function TEmbeddedReview.Board(AApp: Integer): INyxKanbanBinding;
var
  LCommon: INyxCoordinatorBinding;
begin
  LCommon := Child(AApp).CoordinatorBinding(Role(AApp, NyxPart('insights/workflow')).ID);
  Check(Supports(LCommon, INyxKanbanBinding, Result) and Result.Connected and
    (Result = Child(AApp).KanbanBinding(Role(AApp, NyxPart('insights/workflow')).ID)),
    'embedded built-in board remains one common and specialized owner');
end;

function TEmbeddedReview.Widget(ARenderer: TEmbeddedRenderer;
  const AID: TNyxText): TReviewFace;
begin
  {$ifdef PAS2JS}
  Result := ARenderer.ElementFor(AID);
  {$else}
  Result := ARenderer.ControlFor(AID);
  {$endif}

  if Result = nil then
  begin
    raise Exception.Create('The current embedded control has no actual face: ' + AID);
  end;
end;

function TEmbeddedReview.Caption(ARenderer: TEmbeddedRenderer;
  const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := Widget(ARenderer, AID).textContent;
  {$else}
  Result := TLabel(Widget(ARenderer, AID)).Caption;
  {$endif}
end;

procedure TEmbeddedReview.SelectRows(AApp: Integer; const APath: TNyxPartRef;
  AMultiple: Boolean);
var
  LRenderer: TEmbeddedRenderer;
  LID: TNyxText;
  {$ifdef PAS2JS}
  LRow: TJSHTMLElement;
  LOptions: TReviewMouseOptions;
  {$else}
  LList: TListBox;
  {$endif}
begin
  LRenderer := Child(AApp);
  LID := Role(AApp, APath).ID;
  LRenderer.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  LRow := TJSHTMLElement(Widget(LRenderer, LID).querySelector('[data-nyx-item]'));
  Check(LRow <> nil, 'the embedded physical source has an actual first row');
  LRow.click;

  if AMultiple then
  begin
    LOptions := TReviewMouseOptions.new;
    LOptions.bubbles := True;
    LOptions.cancelable := True;
    LOptions.ctrlKey := True;
    LRow := TJSHTMLElement(Widget(LRenderer, LID).
      querySelector('[data-nyx-item="birch"]'));
    Check(LRow <> nil, 'the embedded second multiple-selection row exists');
    LRow.dispatchEvent(TReviewMouseEvent.new('click', LOptions));
  end;
  {$else}
  FApps[AApp].Window.BringToFront;
  LList := TListBox(Widget(LRenderer, LID));
  Check((LList.Items.Count > 0) and (LList.MultiSelect = AMultiple),
    'the embedded list retains its actual source and declared selection mode');
  LList.SetFocus;
  LList.ItemIndex := 0;

  if AMultiple then
  begin
    LList.Selected[0] := True;
    LList.Selected[1] := True;
  end;
  Check(Assigned(LList.OnSelectionChange), 'the embedded native selection route is mounted');
  LList.OnSelectionChange(LList, True);
  {$endif}
  Pump;
end;

procedure TEmbeddedReview.Press(AApp: Integer; const AID: TNyxText);
var
  LRenderer: TEmbeddedRenderer;
  {$ifndef PAS2JS}
  LButton: TWinControl;
  {$endif}
begin
  LRenderer := Child(AApp);
  LRenderer.Reveal(AID, niRuntime);
  {$ifdef PAS2JS}
  Check(not TJSHTMLButtonElement(Widget(LRenderer, AID)).disabled,
    'the actual embedded action is enabled before input');
  Widget(LRenderer, AID).click;
  {$else}
  FApps[AApp].Window.BringToFront;
  LButton := TWinControl(Widget(LRenderer, AID));
  Check(LButton.Enabled, 'the actual embedded action is enabled before input');
  LButton.SetFocus;
  Check(FApps[AApp].Window.ActiveControl = LButton,
    'the embedded action receives real native keyboard focus');

  if LButton.Perform(CN_KEYDOWN, VK_SPACE, 0) = 0 then
  begin
    LButton.Perform(LM_KEYDOWN, VK_SPACE, 0);
  end;

  if LButton.Perform(CN_KEYUP, VK_SPACE, 0) = 0 then
  begin
    LButton.Perform(LM_KEYUP, VK_SPACE, 0);
  end;
  {$endif}
  Pump;
end;

procedure TEmbeddedReview.WriteNote(AApp: Integer; const AText: TNyxText);
var
  LInput: TReviewFace;
  LID: TNyxText;
begin
  LID := Role(AApp, NyxPart('note')).ID;
  Child(AApp).Reveal(LID, niRuntime);
  LInput := Canvas(AApp).InputFor(EmbeddedItem, NyxControl(LID));
  Check((LInput <> nil) and (LInput = Child(AApp).InputFor(LID, niRuntime)),
    'canvas InputFor resolves the exact current child-owned ordinary input');
  {$ifdef PAS2JS}
  TJSHTMLInputElement(LInput).value := AText;
  LInput.dispatchEvent(TJSEvent.new('input'));
  {$else}
  FApps[AApp].Window.BringToFront;
  TEdit(LInput).SetFocus;
  TEdit(LInput).Text := AText;
  {$endif}
  Pump;
  Check(Note(AApp) = AText, 'embedded input is accepted through its ordinary platform owner');
end;

function TEmbeddedReview.Note(AApp: Integer): TNyxText;
var
  LInput: TReviewFace;
begin
  LInput := Canvas(AApp).InputFor(EmbeddedItem, NyxControl(Role(AApp, NyxPart('note')).ID));

  if LInput = nil then
  begin
    raise Exception.Create('The embedded ordinary input is missing');
  end;
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(LInput).value;
  {$else}
  Result := TEdit(LInput).Text;
  {$endif}
end;

procedure TEmbeddedReview.CheckAuthored;
begin
  Check(TNyxCodec.Encode(FDocument) = FDesign,
    'embedded actions preserve the immutable document, scenes, reusable definitions and defaults');
  Check(TNyxCodegen.Generate(FDocument, 'nyx.generated.coordinator.embedded') = FSource,
    'embedded caller choices and runtime data never leak into authored Pascal');
end;

procedure TEmbeddedReview.CheckMembership(AApp: Integer; ANormal, AFuture: Boolean);
var
  LNormal: INyxSelectionSummaryBinding;
  LFuture: INyxSelectionSummaryBinding;
  LDetail: INyxMasterDetailBinding;
  LBoard: INyxKanbanBinding;
begin
  LNormal := Summary(AApp);
  LFuture := Summary(AApp, True);
  Check((LNormal <> nil) = ANormal, 'the child has exactly its admitted normal caller membership');
  Check((LFuture <> nil) = AFuture, 'the child has exactly its admitted future caller membership');

  if LNormal <> nil then
  begin
    Check(LNormal.Connected, 'the inherited normal creator owner is active');
  end;

  if LFuture <> nil then
  begin
    Check(LFuture.Connected, 'the explicitly selected future creator owner is active');
  end;
  LDetail := Detail(AApp);
  LBoard := Board(AApp);
  Check(LDetail.Connected and LBoard.Connected,
    'built-ins compose separately and survive caller override, empty and inherited membership');
end;

procedure TEmbeddedReview.CheckRetainedStores(AApp: Integer;
  const AStores: array of INyxCollection);
begin
  Check((Length(AStores) = 4) and
    (Child(AApp).CollectionView(Role(AApp, NyxPart('insights/selection/items')).ID).
      Store = AStores[0]) and
    (Child(AApp).CollectionView(Role(AApp, NyxPart('insights/future/items')).ID).
      Store = AStores[1]) and
    (Child(AApp).CollectionView(Role(AApp, NyxPart('insights/directory/master')).ID).
      Store = AStores[2]) and
    (Child(AApp).CollectionView(Role(AApp, NyxPart('insights/workflow/lane-0/items')).ID).
      Store = AStores[3]),
    'structural child admission preserves all four compatible existing collection owners');
end;

procedure TEmbeddedReview.AwaitCount(AApp, ACount: Integer;
  AFuture: Boolean); {$ifdef PAS2JS}async;{$endif}
var
  LSummary: INyxSelectionSummaryBinding;
  LCountID: TNyxText;
  {$ifdef PAS2JS}
  LStarted: Double;
  {$else}
  LStarted: QWord;
  {$endif}
begin
  LSummary := Summary(AApp, AFuture);
  Check(LSummary <> nil, 'count completion requires an admitted creator');

  if AFuture then
  begin
    LCountID := Role(AApp, NyxPart('insights/future/selection-count')).ID;
  end
  else
  begin
    LCountID := Role(AApp, NyxPart('insights/selection/selection-count')).ID;
  end;
  {$ifdef PAS2JS}LStarted := TJSDate.now;{$else}LStarted := GetTickCount64;{$endif}
  while (LSummary.SelectedCount <> ACount) or
    (Caption(Child(AApp), LCountID) <> SelectionSummaryCaption(ACount)) do
  begin
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 10000 then
    {$else}

    if GetTickCount64 - LStarted > 10000 then
    {$endif}
    begin
      raise Exception.Create('Embedded count completion: ' + Caption(Child(AApp), LCountID));
    end;
    {$ifdef PAS2JS}await(UITurn);{$else}Pump;{$endif}
  end;
  Check(LSummary.Connected, 'actual embedded count presentation completes on its accepted owner');
end;

procedure TEmbeddedReview.AwaitMount(AApp: Integer;
  const AFormer: INyxCancellationScope; ANormal, AFuture: Boolean);
  {$ifdef PAS2JS}async;{$endif}
var
  LCurrent: INyxCancellationScope;
  {$ifdef PAS2JS}
  LStarted: Double;
  {$else}
  LStarted: QWord;
  {$endif}
begin
  {$ifdef PAS2JS}LStarted := TJSDate.now;{$else}LStarted := GetTickCount64;{$endif}
  LCurrent := NyxEventGeneration(Child(AApp).Events);
  while LCurrent = AFormer do
  begin
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 10000 then
    {$else}

    if GetTickCount64 - LStarted > 10000 then
    {$endif}
    begin
      raise Exception.Create('The structural child presentation did not publish a successor');
    end;
    {$ifdef PAS2JS}await(UITurn);{$else}Pump;{$endif}
    LCurrent := NyxEventGeneration(Child(AApp).Events);
  end;
  Check(AFormer.Cancelled and not LCurrent.Cancelled,
    'structural child remount revokes only its former exact event generation');
  CheckMembership(AApp, ANormal, AFuture);
end;

procedure TEmbeddedReview.AwaitRecord(AApp: Integer); {$ifdef PAS2JS}async;{$endif}
var
  LBinding: INyxMasterDetailBinding;
  LTitleID: TNyxText;
  {$ifdef PAS2JS}
  LStarted: Double;
  {$else}
  LStarted: QWord;
  {$endif}
begin
  LBinding := Detail(AApp);
  LTitleID := Role(AApp, NyxPart('insights/directory/detail/title')).ID;
  {$ifdef PAS2JS}LStarted := TJSDate.now;{$else}LStarted := GetTickCount64;{$endif}
  while not LBinding.Current.Defined or
    (Caption(Child(AApp), LTitleID) <> 'Ada Lovelace') do
  begin
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 10000 then
    {$else}

    if GetTickCount64 - LStarted > 10000 then
    {$endif}
    begin
      raise Exception.Create('Embedded detail did not publish the actual selected record');
    end;
    {$ifdef PAS2JS}await(UITurn);{$else}Pump;{$endif}
  end;
  Check(LBinding.Connected and (LBinding.CaptureEdit.Ref.ID = 'ada'),
    'embedded detail completes physical title and source-owned record projection');
end;

{ The scene's existing prepared content owner stages a real new child. Missing
  role refusal must leave the accepted scene/control/generation untouched. }
procedure TEmbeddedReview.ReplaceItem(AApp: Integer; AMissingRole: Boolean);
var
  LCanvas: TEmbeddedCanvas;
  LRuntime: INyxCanvasRuntimeView;
  LContent: INyxCanvasSceneContent;
  LLayout: INyxCanvasLayoutView;
  LPlan: INyxCanvasLayoutPlan;
  LPublication: INyxPreparedPublication;
  LReplacement: INyxControl;
  LRemoved: INyxControl;
  LNode: TNyxNode;
  LRoot: TNyxNode;
  LFace: TReviewFace;
  LGeneration: INyxCancellationScope;
  LSummary: INyxSelectionSummaryBinding;
  LRevision: Integer;
  LPresentation: Integer;
  LAccepted: Boolean;
  LRefused: Boolean;
begin
  LCanvas := Canvas(AApp);
  Check(Supports(LCanvas, INyxCanvasRuntimeView, LRuntime) and
    Supports(LRuntime.Scene, INyxCanvasSceneContent, LContent) and
    Supports(LCanvas, INyxCanvasLayoutView, LLayout),
    'ordinary saved canvas exposes its existing prepared scene/layout owners');
  LReplacement := RetainNyxControl(LRuntime.Scene.Control(EmbeddedItem).Node.Clone);

  if AMissingRole then
  begin
    LNode := LReplacement.Node.Part(NyxPart('insights/selection/selection-count'));
    Check(LNode <> nil, 'the detached refusal candidate starts with the exact required role');
    LRemoved := RetainNyxControl(LNode);
    LNode.Parent.Remove(LNode);
    LRemoved := nil;
  end;
  LRoot := Child(AApp).Root;
  LFace := Child(AApp).InputFor(Role(AApp, NyxPart('note')).ID, niRuntime);
  LGeneration := NyxEventGeneration(Child(AApp).Events);
  LSummary := Summary(AApp);
  LRevision := LRuntime.Scene.Revision;
  LPresentation := LLayout.PresentationRevision;
  LAccepted := False;
  LRefused := False;
  LPlan := nil;
  LPublication := nil;
  try
    try
      LPlan := LContent.PrepareContent([NyxCanvasReplaceControl(EmbeddedItem,
        LRuntime.Scene.Snapshot.Items[0].Placement, LReplacement)], LRevision);
      Check(Supports(LPlan, INyxPreparedPublication, LPublication),
        'scene replacement retains the existing prepared-publication lifetime');
      LAccepted := LLayout.ApplyLayout(LPlan, LCanvas.Camera, LPresentation);
    except
      on ENyxCoordinator do
      begin
        LRefused := True;
      end;
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;

    if AMissingRole then
    begin
      Check(LRefused and not LAccepted and (LRuntime.Scene.Revision = LRevision) and
        (LLayout.PresentationRevision = LPresentation) and (Child(AApp).Root = LRoot) and
        (NyxEventGeneration(Child(AApp).Events) = LGeneration) and not LGeneration.Cancelled and
        (Child(AApp).InputFor(Role(AApp, NyxPart('note')).ID, niRuntime) = LFace) and
        LSummary.Connected,
        'failed child admission preserves accepted scene, face, token and creator input owner');
    end
    else
    begin
      Check(not LRefused and LAccepted and (LRuntime.Scene.Revision = LRevision + 1) and
        LGeneration.Cancelled and not LSummary.Connected,
        'real scene replacement adopts a fresh child and retires the old commands');
    end;
  finally

    if LPublication <> nil then
    begin
      LPublication.Retire;
    end;
  end;
end;

procedure TEmbeddedReview.Exercise; {$ifdef PAS2JS}async;{$endif}
var
  LApp: Integer;
  LIndex: Integer;
  LOther: Integer;
  LStores: array of INyxCollection;
  LSnapshots: array of INyxCollectionSnapshot;
  LViews: array of INyxCollectionView;
  LRetainedStores: array of INyxCollection;
  LSummary: INyxSelectionSummaryBinding;
  LOldSummary: INyxSelectionSummaryBinding;
  LDetail: INyxMasterDetailBinding;
  LBoard: INyxKanbanBinding;
  LMove: INyxKanbanMove;
  LCanvas: TEmbeddedCanvas;
  LPeerCanvas: TEmbeddedCanvas;
  LGeneration: INyxCancellationScope;
  LParentGeneration: INyxCancellationScope;
  LPeerGeneration: INyxCancellationScope;
  LOverride: INyxCoordinatorRegistry;
  LQuery: TNyxText;
  LRejected: Boolean;
begin
  SetLength(LStores, 12);
  SetLength(LSnapshots, 12);
  SetLength(LViews, 4);
  SetLength(LRetainedStores, 4);
  for LApp := 0 to 1 do
  begin
    FApps[LApp] := TReviewApplication.Create;
    FApps[LApp].View.CoordinatorRegistry := FRegistry;
    {$ifdef PAS2JS}
    FHosts[LApp] := TJSHTMLElement(document.createElement('main'));
    FHosts[LApp].style.cssText := 'min-height:900px;width:100%;';
    document.body.appendChild(FHosts[LApp]);
    FApps[LApp].Run(FDocument, FHosts[LApp]);
    {$else}
    FApps[LApp].Mount(FDocument);
    FApps[LApp].Window.ClientWidth := GWidth;
    FApps[LApp].Window.ClientHeight := 900;
    FApps[LApp].Window.Show;
    Check(FApps[LApp].Window.ClientWidth = GWidth,
      'the embedded ordinary App keeps the requested actual client allocation');
    {$endif}
    FApps[LApp].ShowPage(CEmbeddedHome);
    Pump;
    FApps[LApp].View.Reveal(CEmbeddedCanvas, niRuntime);
    Canvas(LApp).Execute(nccFit);
    CheckMembership(LApp, True, False);
    Check((ParentSummary(LApp) <> nil) and (ParentSummary(LApp, True) = nil),
      'initial parent and child capture the same caller membership before Build');
    Check((Child(LApp).CoordinatorRegistry = nil) and
      (NyxEventGeneration(Child(LApp).Events) <> NyxEventGeneration(FApps[LApp].View.Events)),
      'inherited child membership leaves public registry untouched and owns its own generation');
    LViews[0] := FApps[LApp].View.CollectionView(FApps[LApp].View.Root.
      Part(NyxPart('parent-selection/items')).ID);
    LViews[1] := FApps[LApp].View.CollectionView(FApps[LApp].View.Root.
      Part(NyxPart('parent-future/items')).ID);
    LViews[2] := Child(LApp).CollectionView(Role(LApp,
      NyxPart('insights/selection/items')).ID);
    LViews[3] := Child(LApp).CollectionView(Role(LApp,
      NyxPart('insights/future/items')).ID);
    for LIndex := 0 to 3 do
    begin
      Check(LViews[LIndex] <> nil, 'every parent/child caller list owns a real instance view');
      LStores[LApp * 6 + LIndex] := LViews[LIndex].Store;
    end;
    LStores[LApp * 6 + 4] := Child(LApp).CollectionView(Role(LApp,
      NyxPart('insights/directory/master')).ID).Store;
    LBoard := Board(LApp);
    LStores[LApp * 6 + 5] := Child(LApp).CollectionView(LBoard.Items(Plan).ID).Store;
    {$ifdef PAS2JS}await(AwaitCount(LApp, 0));{$else}AwaitCount(LApp, 0);{$endif}
    Check(Note(LApp) = 'Child note default', 'each saved child starts from its authored input');
  end;
  for LIndex := 0 to High(LStores) do
  begin
    LSnapshots[LIndex] := LStores[LIndex].Snapshot;
    for LOther := LIndex + 1 to High(LStores) do
    begin
      Check(LStores[LIndex] <> LStores[LOther],
        'parent, child and peer-App collections have independent managed store identities');
    end;
  end;
  LPeerCanvas := Canvas(1);
  LPeerGeneration := NyxEventGeneration(Child(1).Events);
  LParentGeneration := NyxEventGeneration(FApps[0].View.Events);
  WriteNote(0, 'Only the first embedded child');
  SelectRows(0, NyxPart('insights/selection/items'), True);
  {$ifdef PAS2JS}await(AwaitCount(0, 2));{$else}AwaitCount(0, 2);{$endif}
  LViews[0] := Child(0).CollectionView(Role(0,
    NyxPart('insights/selection/items')).ID);
  LViews[0].ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(SummaryName).EqualTo('Cedar')));
  {$ifdef PAS2JS}await(AwaitCount(0, 2));{$else}AwaitCount(0, 2);{$endif}
  LQuery := LViews[0].QueryPolicy.ToData.ToJSON;
  Check((LViews[0].Snapshot.Count = 1) and (LViews[0].Selection.Count = 2),
    'embedded creator counts hidden selected membership through its existing query owner');
  Press(0, Role(0, NyxPart('insights/selection/clear-selection')).ID);
  {$ifdef PAS2JS}await(AwaitCount(0, 0));{$else}AwaitCount(0, 0);{$endif}
  Check((LViews[0].Selection.Count = 0) and
    (LViews[0].QueryPolicy.ToData.ToJSON = LQuery),
    'embedded physical Clear preserves the child query and copied source rows');
  LViews[0].ConfigureQuery(NyxCollectionQuery);
  SelectRows(0, NyxPart('insights/directory/master'), False);
  {$ifdef PAS2JS}await(AwaitRecord(0));{$else}AwaitRecord(0);{$endif}
  LDetail := Detail(0);
  Check(LDetail.CaptureEdit.Ref.ID = 'ada',
    'embedded built-in detail captures the selected source-owned record');
  SelectRows(0, NyxPart('insights/workflow/lane-0/items'), False);
  LBoard := Board(0);
  Press(0, LBoard.MoveButton(Build).ID);
  Check(LStores[5].Snapshot.Item(Card).GetValue(CardLane) = Build.Name,
    'embedded physical Move uses the built-in stable collection owner');
  LSnapshots[5] := LStores[5].Snapshot;
  Check((Summary(1).SelectedCount = 0) and not Detail(1).Current.Defined and
    (Note(1) = 'Child note default') and (ParentSummary(0).SelectedCount = 0) and
    (ParentSummary(1).SelectedCount = 0),
    'child input leaves parent caller state and the entire peer application independent');
  for LIndex := 0 to High(LStores) do
  begin
    Check(SameSnapshot(LStores[LIndex].Snapshot, LSnapshots[LIndex]),
      'only the admitted first child card move changes any captured store rows/revision');
  end;
  CheckAuthored;

  { Mutating a caller registry cannot retarget either accepted parent or its
    future scene children. A later parent candidate deliberately captures it. }
  FRegistry.RegisterFactory(NyxCustomKind(NyxKindName(nkCard)),
    NyxExtension(CFutureIntent), NewSelectionSummaryFactory);
  Check((ParentSummary(0, True) = nil) and (ParentSummary(1, True) = nil) and
    (Summary(0, True) = nil) and (Summary(1, True) = nil),
    'late registration leaves both already accepted parent/child memberships unchanged');
  LOldSummary := Summary(0);
  LDetail := Detail(0);
  LBoard := Board(0);
  LMove := LBoard.CaptureMove(Card, Build, Plan, LStores[5].Snapshot.Revision);
  LGeneration := NyxEventGeneration(Child(0).Events);
  ReplaceItem(0, True);
  Check((Note(0) = 'Only the first embedded child') and
    (NyxEventGeneration(Child(0).Events) = LGeneration),
    'required-role refusal preserves actual accepted child input and generation');
  SelectRows(0, NyxPart('insights/selection/items'), True);
  {$ifdef PAS2JS}await(AwaitCount(0, 2));{$else}AwaitCount(0, 2);{$endif}
  Press(0, Role(0, NyxPart('insights/selection/clear-selection')).ID);
  {$ifdef PAS2JS}await(AwaitCount(0, 0));{$else}AwaitCount(0, 0);{$endif}
  ReplaceItem(0, False);
  {$ifdef PAS2JS}await(AwaitMount(0, LGeneration, True, False));
  {$else}AwaitMount(0, LGeneration, True, False);{$endif}
  Check(not LOldSummary.Connected and not LDetail.Connected and not LBoard.Connected,
    'scene replacement retires all former child command families');
  LRejected := False;
  try
    LOldSummary.Clear;
  except
    on ENyxCoordinator do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'a retained creator command cannot address the same-ID new scene child');
  LRejected := False;
  try
    LMove.Commit;
  except
    on ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and not LMove.Committed,
    'a retained built-in move cannot cross a scene content lifetime');
  Check(not LParentGeneration.Cancelled and not LPeerGeneration.Cancelled and
    (Canvas(1) = LPeerCanvas) and (Note(1) = 'Child note default'),
    'scene replacement preserves parent and independent peer child lifetimes');
  SelectRows(0, NyxPart('insights/selection/items'), True);
  {$ifdef PAS2JS}await(AwaitCount(0, 2));{$else}AwaitCount(0, 2);{$endif}
  Press(0, Role(0, NyxPart('insights/selection/clear-selection')).ID);
  {$ifdef PAS2JS}await(AwaitCount(0, 0));{$else}AwaitCount(0, 0);{$endif}

  LCanvas := Canvas(0);
  LOldSummary := Summary(0);
  FApps[0].ShowPage(CEmbeddedHome);
  Pump;
  FApps[0].View.Reveal(CEmbeddedCanvas, niRuntime);
  Canvas(0).Execute(nccFit);
  Check(not LCanvas.Connected and not LOldSummary.Connected and LParentGeneration.Cancelled,
    'ordinary parent navigation retires its former embedded view before replacement input');
  Check((ParentSummary(0, True) <> nil) and (ParentSummary(1, True) = nil),
    'next parent candidate captures late membership while the peer retains its accepted snapshot');
  CheckMembership(0, True, True);
  CheckMembership(1, True, False);
  LParentGeneration := NyxEventGeneration(FApps[0].View.Events);
  LRetainedStores[0] := Child(0).CollectionView(Role(0,
    NyxPart('insights/selection/items')).ID).Store;
  LRetainedStores[1] := Child(0).CollectionView(Role(0,
    NyxPart('insights/future/items')).ID).Store;
  LRetainedStores[2] := Child(0).CollectionView(Role(0,
    NyxPart('insights/directory/master')).ID).Store;
  LRetainedStores[3] := Child(0).CollectionView(Role(0,
    NyxPart('insights/workflow/lane-0/items')).ID).Store;

  { Child overrides use only its own saved blueprint/presentation queue. Public
    registry assignment alone is inert; every choice needs a new admission. }
  LGeneration := NyxEventGeneration(Child(0).Events);
  LOldSummary := Summary(0);
  LOverride := NewNyxCoordinatorRegistry;
  LOverride.RegisterFactory(NyxCustomKind(NyxKindName(nkCard)),
    NyxExtension(CFutureIntent), NewSelectionSummaryFactory);
  Child(0).CoordinatorRegistry := LOverride;
  Check((NyxEventGeneration(Child(0).Events) = LGeneration) and
    (Summary(0) = LOldSummary), 'setting child override affects only a future candidate');
  Child(0).Presentations.Select(NyxPresentation(CAlternate));
  {$ifdef PAS2JS}await(AwaitMount(0, LGeneration, False, True));
  {$else}AwaitMount(0, LGeneration, False, True);{$endif}
  CheckRetainedStores(0, LRetainedStores);
  Check(Child(0).Presentations.Selection.Reference.Name = CAlternate,
    'explicit child override publishes through its own structural presentation');
  Check(not LParentGeneration.Cancelled and not LPeerGeneration.Cancelled,
    'child presentation never revokes parent or peer generations');
  SelectRows(0, NyxPart('insights/future/items'), True);
  {$ifdef PAS2JS}await(AwaitCount(0, 2, True));{$else}AwaitCount(0, 2, True);{$endif}
  Press(0, Role(0, NyxPart('insights/future/clear-selection')).ID);
  {$ifdef PAS2JS}await(AwaitCount(0, 0, True));{$else}AwaitCount(0, 0, True);{$endif}
  LGeneration := NyxEventGeneration(Child(0).Events);
  Child(0).CoordinatorRegistry := NewNyxCoordinatorRegistry;
  Child(0).Presentations.Automatic;
  {$ifdef PAS2JS}await(AwaitMount(0, LGeneration, False, False));
  {$else}AwaitMount(0, LGeneration, False, False);{$endif}
  CheckRetainedStores(0, LRetainedStores);
  Check(not Child(0).Presentations.Selection.Reference.Defined,
    'explicit empty callers publish through the automatic structural alternative');
  SelectRows(0, NyxPart('insights/directory/master'), False);
  {$ifdef PAS2JS}await(AwaitRecord(0));{$else}AwaitRecord(0);{$endif}
  LBoard := Board(0);
  LViews[0] := Child(0).CollectionView(LBoard.Items(Plan).ID);
  LIndex := LViews[0].Store.Snapshot.Revision;
  SelectRows(0, NyxPart('insights/workflow/lane-0/items'), False);
  Press(0, LBoard.MoveButton(Build).ID);
  Check((LViews[0].Store.Snapshot.Revision = LIndex + 1) and
    (LViews[0].Store.Snapshot.Item(Card).GetValue(CardLane) = Build.Name),
    'explicit empty callers retain real built-in detail and physical card movement');
  LGeneration := NyxEventGeneration(Child(0).Events);
  Child(0).CoordinatorRegistry := nil;
  Child(0).Presentations.Select(NyxPresentation(CAlternate));
  {$ifdef PAS2JS}await(AwaitMount(0, LGeneration, True, True));
  {$else}AwaitMount(0, LGeneration, True, True);{$endif}
  CheckRetainedStores(0, LRetainedStores);
  WriteNote(0, 'Restored inherited child');
  SelectRows(0, NyxPart('insights/selection/items'), True);
  {$ifdef PAS2JS}await(AwaitCount(0, 2));{$else}AwaitCount(0, 2);{$endif}
  Press(0, Role(0, NyxPart('insights/selection/clear-selection')).ID);
  {$ifdef PAS2JS}await(AwaitCount(0, 0));{$else}AwaitCount(0, 0);{$endif}
  Check((Note(1) = 'Child note default') and (Summary(1, True) = nil) and
    not LPeerGeneration.Cancelled and not LParentGeneration.Cancelled,
    'restored inheritance and fresh child input preserve every peer/parent boundary');
  for LIndex := 0 to High(LStores) do
  begin
    Check(SameSnapshot(LStores[LIndex].Snapshot, LSnapshots[LIndex]),
      'later child lifetime changes leave displaced, parent and peer stores unchanged');
  end;
  CheckAuthored;
  {$ifndef PAS2JS}
  WriteEvidence('embedded-authored-design.nyx', FDesign);
  WriteEvidence('nyx.generated.coordinator.embedded.pas', FSource);
  {$endif}

  LCanvas := Canvas(0);
  LSummary := Summary(0);
  LDetail := Detail(0);
  LBoard := Board(0);
  LGeneration := NyxEventGeneration(Child(0).Events);
  FreeAndNil(FApps[0]);
  Check(not LCanvas.Connected and LGeneration.Cancelled and not LSummary.Connected and
    not LDetail.Connected and not LBoard.Connected,
    'ordinary App disposal revokes its embedded view and every retained command owner');
  Check(LPeerCanvas.Connected and not LPeerGeneration.Cancelled,
    'disposing one App leaves the independent peer canvas and generation accepted');
  WriteNote(1, 'The peer remains usable');
  SelectRows(1, NyxPart('insights/selection/items'), True);
  {$ifdef PAS2JS}await(AwaitCount(1, 2));{$else}AwaitCount(1, 2);{$endif}
  Press(1, Role(1, NyxPart('insights/selection/clear-selection')).ID);
  {$ifdef PAS2JS}await(AwaitCount(1, 0));{$else}AwaitCount(1, 0);{$endif}
  CheckAuthored;
  FreeAndNil(FApps[1]);
  Check(not LPeerCanvas.Connected and LPeerGeneration.Cancelled,
    'independent peer disposal retires its own final child generation');
end;

procedure EmbeddedCases; {$ifdef PAS2JS}async;{$endif}
var
  LReview: TEmbeddedReview;
begin
  LReview := TEmbeddedReview.Create;
  try
    {$ifdef PAS2JS}await(LReview.Exercise);{$else}LReview.Exercise;{$endif}
  finally
    LReview.Free;
  end;
end;

procedure Release;
var
  LApp: Integer;
begin
  for LApp := 0 to 1 do
  begin
    FreeAndNil(GApps[LApp]);
    {$ifdef PAS2JS}

    if GHosts[LApp] <> nil then
    begin
      GHosts[LApp].remove;
      GHosts[LApp] := nil;
    end;
    {$endif}
  end;
  GSummaries := nil;
  GDetails := nil;
  GBoards := nil;
  GStores := nil;
  GBaselines := nil;
  GRegistry := nil;
  FreeAndNil(GDocument);
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
begin
  try
    try
      {$ifdef PAS2JS}
      await(Cases);
      document.body.setAttribute('data-coordinator-reuse-r1-checks', IntToStr(GChecks));
      await(EmbeddedCases);
      document.body.setAttribute('data-coordinator-reuse', 'passed');
      document.body.setAttribute('data-coordinator-reuse-checks', IntToStr(GChecks));
      {$else}
      Cases;
      WriteEvidence('authored-design.nyx', GDesign);
      WriteEvidence('nyx.generated.coordinator.reuse.pas', GSource);
      WriteLn('PASS ', GChecks, ' ordinary reusable coordinator checks / width=', GWidth);
      WriteLn('R1 completed ', GChecks, ' checks before embedded caller-snapshot cases');
      EmbeddedCases;
      WriteLn('PASS ', GChecks, ' aggregate reusable/embedded coordinator checks / width=', GWidth);
      {$endif}
    except
      on E: Exception do
      begin
        {$ifdef PAS2JS}
        document.body.setAttribute('data-coordinator-reuse', 'failed');
        document.body.setAttribute('data-coordinator-reuse-checks', IntToStr(GChecks));
        document.body.setAttribute('data-coordinator-reuse-error', E.Message);
        {$else}
        WriteLn('FAIL ', GChecks, ' ', E.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
        {$endif}
      end;
    end;
  finally
    Release;
  end;
end;

begin
  SetLength(GSummaries, 4);
  SetLength(GDetails, 4);
  SetLength(GBoards, 4);
  SetLength(GStores, 12);
  SetLength(GBaselines, 12);
  {$ifndef PAS2JS}
  Application.Initialize;
  GEvidence := ParamStr(1);
  GWidth := StrToIntDef(ParamStr(2), 1280);
  GFailures := TFailureObserver.Create;
  Application.OnException := GFailures.Failed;
  try
    Run;
  finally
    Application.OnException := nil;
    GFailures.Free;
  end;
  {$else}
  Run;
  {$endif}
end.
