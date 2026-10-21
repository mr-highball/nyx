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
  JS, Web, nyx.application.browser,
  {$else}
  Classes, Interfaces, Forms, Controls, StdCtrls, LCLType, LMessages,
  nyx.application.lcl,
  {$endif}
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.query, nyx.collections.selection, nyx.coordinators,
  nyx.selection.summary, nyx.master.detail, nyx.kanban, nyx.kanban.intent,
  nyx.events, nyx.scheduler;

type
  {$ifdef PAS2JS}
  { The ordinary browser application owns its renderer and independent runtime. }
  TReviewApplication = TNyxBrowserApplication;
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

const
  CHome = 'home';
  CRefusal = 'refusal';
  CWorkspace = 'workspace';
  CPlaceholder = 'Choose a person';
  CDescription = 'The selected record stays in this workspace.';

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
    Check(Supports(LCommon, INyxSelectionSummaryBinding, GSummaries[LSlot]) and
      GSummaries[LSlot].Connected,
      'generic application lookup preserves the exact creator specialized binding');
    LCommon := GApps[AApp].View.CoordinatorBinding(Owner(AApp, LInstance, rcDetail).ID);
    Check(Supports(LCommon, INyxMasterDetailBinding, LDetail) and
      (LDetail = GApps[AApp].View.MasterDetailBinding(Owner(AApp, LInstance, rcDetail).ID)) and
      LDetail.Connected, 'common and typed detail lookup have one accepted identity');
    GDetails[LSlot] := LDetail;
    LCommon := GApps[AApp].View.CoordinatorBinding(Owner(AApp, LInstance, rcKanban).ID);
    Check(Supports(LCommon, INyxKanbanBinding, LBoard) and
      (LBoard = GApps[AApp].View.KanbanBinding(Owner(AApp, LInstance, rcKanban).ID)) and
      LBoard.Connected, 'common and typed board lookup have one accepted identity');
    GBoards[LSlot] := LBoard;
    LControl := RetainNyxControl(Owner(AApp, LInstance, rcSummary));
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
      document.body.setAttribute('data-coordinator-reuse', 'passed');
      document.body.setAttribute('data-coordinator-reuse-checks', IntToStr(GChecks));
      {$else}
      Cases;
      WriteEvidence('authored-design.nyx', GDesign);
      WriteEvidence('nyx.generated.coordinator.reuse.pas', GSource);
      WriteLn('PASS ', GChecks, ' ordinary reusable coordinator checks / width=', GWidth);
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
