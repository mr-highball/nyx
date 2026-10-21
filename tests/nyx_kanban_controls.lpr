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
program nyx_kanban_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, LCLType, LCLIntf, LMessages,
    Graphics, IntfGraphics, FPWritePNG, nyx.render.lcl,{$endif}
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.composition, nyx.responsive, nyx.collections, nyx.collections.view,
  nyx.collections.bindings, nyx.collections.view.types, nyx.kanban,
  nyx.kanban.intent, nyx.events, nyx.scheduler, nyx.coordinators, nyx.data,
  nyx.publication.types
  {$ifdef NYX_COMPILED_KANBAN}, nyx.generated.kanban{$endif};

{$ifndef PAS2JS}
type
  { Expose ordinary LCL drag slots without replacing the connected Nyx handlers.
    These synthesized host callbacks do not qualify physical pointer hardware. }
  TControlAccess = class(TControl);
  { Native exceptions are retained for the next assertion instead of obscuring
    the first failure with a modal widgetset exception dialog. }
  TFailureObserver = class
  public
    MessageText: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
{$endif}

const
  CNotificationIntent: TNyxText = 'test-kanban-notification';

type
  { A separately registered creator participates beside the real automatic
    boards. Its postcommit notification either fails or disposes the renderer;
    this tests accepted Kanban ownership, rather than an already-retired binding. }
  TNotificationMode = (nnmFailure, nnmDispose);
  { Minimal creator-owned prepared lifetime. Install only records adoption;
    Notify exercises its copied failure/disposal mode after built-ins commit. }
  TNotificationPlan = class(TInterfacedObject, INyxPreparedCoordinator,
    INyxPreparedPublication, INyxPublicationReadiness, INyxCoordinatorBinding)
  private
    FMode: TNotificationMode;
    FInstalled: Boolean;
    FRevoked: Boolean;
  public
    constructor Create(AMode: TNotificationMode);
    function GetBinding: INyxCoordinatorBinding;
    function GetConnected: Boolean;
    procedure Revoke;
    procedure Disconnect;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { Public creator path remains Node-free even while built-in Kanban uses the
    private canonical admission channel. One explicit caller owns this registry. }
  TNotificationFactory = class(TInterfacedObject, INyxCoordinatorFactory)
  public
    Mode: TNotificationMode;
    function Prepare(const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
  end;

var
  GDocument: TNyxDocument;
  GViews: INyxCollectionBindings;
  GOptions: TNyxKanbanOptions;
  GBindings: array of INyxKanbanBinding;
  GRefuseCandidate: Boolean;
  GAuthored: TNyxText;
  GChecks: Integer;
  GReview: Boolean;
  GPublished: INyxKanbanBinding;
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  {$else}
  GRenderer: TNyxLCLRenderer;
  GHost: TForm;
  GFailures: TFailureObserver;
  GWidth: Integer;
  GEvidence: String;
  {$endif}

constructor TNotificationPlan.Create(AMode: TNotificationMode);
begin
  inherited Create;
  FMode := AMode;
end;

function TNotificationPlan.GetBinding: INyxCoordinatorBinding;
begin
  Result := Self;
end;

function TNotificationPlan.GetConnected: Boolean;
begin
  Result := FInstalled and not FRevoked;
end;

procedure TNotificationPlan.Revoke;
begin
  FRevoked := True;
end;

procedure TNotificationPlan.Disconnect;
begin
  Revoke;
end;

procedure TNotificationPlan.Validate;
begin
  CheckReady;
end;

procedure TNotificationPlan.CheckReady;
begin

  if FRevoked then
  begin
    raise ENyxCoordinatorCancelled.Create('Test notification participant is retired');
  end;
end;

procedure TNotificationPlan.Install;
begin
  FInstalled := True;
end;

procedure TNotificationPlan.Notify;
var
  {$ifdef PAS2JS}LRenderer: TNyxBrowserRenderer;{$else}LRenderer: TNyxLCLRenderer;{$endif}
begin
  GPublished := GRenderer.KanbanBinding(NyxQualifiedID('first-board', 'workflow'));

  if FMode = nnmDispose then
  begin
    LRenderer := GRenderer;
    GRenderer := nil;
    LRenderer.Free;
  end;
  raise ENyxCoordinator.Create('Deliberate committed notification failure');
end;

procedure TNotificationPlan.Retire;
begin
  { Installed lifetime belongs to the common mount, not preparation retirement. }
end;

function TNotificationFactory.Prepare(const AProjection: INyxCoordinatorProjection;
  const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
  const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
begin
  Result := TNotificationPlan.Create(Mode);
end;

function Cards: TNyxCollectionRef;
begin
  Result := NyxCollection('workflow-cards');
end;

function CardTitle: TNyxTextFieldRef;
begin
  Result := NyxTextField('title');
end;

function CardLane: TNyxTextFieldRef;
begin
  Result := NyxTextField('lane');
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

function FirstCard: TNyxItemRef;
begin
  Result := NyxItem(Cards, 'welcome');
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if GFailures.MessageText <> '' then
  begin
    raise Exception.Create('Native Kanban callback: ' + GFailures.MessageText);
  end;
  {$endif}

  if not ACondition then
  begin
    raise Exception.Create('Kanban controls: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Pump;
begin
  {$ifndef PAS2JS}
  Application.ProcessMessages;
  {$endif}
end;

{ A creator-supplied ordinary label proves refusal after dormant board hooks
  were prepared. The same factory renders real English content on success. }
{$ifdef PAS2JS}
function ReviewLabel(ANode: TNyxNode): TJSHTMLElement;
begin

  if GRefuseCandidate then
  begin
    raise ENyxModel.Create('Deliberate candidate label failure');
  end;
  Result := TJSHTMLElement(document.createElement('span'));
  Result.textContent := ANode.Prop(NyxAttributeName(atText));
end;
{$else}
function ReviewLabel(ANode: TNyxNode; AOwner: TComponent): TControl;
var
  LLabel: TLabel;
begin

  if GRefuseCandidate then
  begin
    raise ENyxModel.Create('Deliberate candidate label failure');
  end;
  LLabel := TLabel.Create(AOwner);
  LLabel.AutoSize := False;
  LLabel.WordWrap := True;
  LLabel.Caption := String(ANode.Prop(NyxAttributeName(atText)));
  Result := LLabel;
end;
{$endif}

{$ifndef PAS2JS}
procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  MessageText := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

function Fixture: TNyxDocument;
var
  LBoard: INyxKanbanBoard;
  LPage: INyxPage;
  LInstance: INyxComponent;
begin
  Result := TNyxDocument.Create;
  try
    Result.Collections.Define(Cards,
      NyxCollectionSchema.Text(CardTitle, '').Text(CardLane, Plan.Name), [
      NyxCollectionItem(FirstCard).WithValue(CardTitle, 'Build a welcoming page')
        .WithValue(CardLane, Plan.Name),
      NyxCollectionItem(NyxItem(Cards, 'details')).WithValue(CardTitle, 'Refine the details')
        .WithValue(CardLane, Build.Name),
      NyxCollectionItem(NyxItem(Cards, 'publish')).WithValue(CardTitle, 'Share a finished view')
        .WithValue(CardLane, Complete.Name)]);
    LBoard := NewNyxKanbanBoard('workflow');
    ConfigureNyxKanbanBoard(LBoard, GOptions);
    SetNyxKanbanIntent(LBoard, TNyxKanbanIntent.Configured(GOptions));
    LBoard.Configure.WhenViewport(TNyxViewportWidth.Below(700)).Layout(nlColumn).Done;
    LBoard.Lane0ItemsList.Configure.Height(112).Done;
    LBoard.Lane1ItemsList.Configure.Height(112).Done;
    LBoard.Lane2ItemsList.Configure.Height(112).Done;
    Result.AddComponent(LBoard);
    LPage := NewNyxPage('board-review');
    LPage.Configure.Padding(16).Gap(16).Done;
    LPage.Add(NewNyxHeading('review-title').WithText('A little work, beautifully organized'));
    LPage.Add(NewNyxLabel('review-help').WithText(
      'Select a card, then choose its destination. Each board keeps its own work.'));
    LInstance := NewNyxComponent('first-board');
    LInstance.Configure.Component(NyxComponent('workflow')).Done;
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('second-board');
    LInstance.Configure.Component(NyxComponent('workflow')).Done;
    LPage.Add(LInstance);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function LaneView(ABoard: Integer; const ALane: TNyxKanbanLaneRef): INyxCollectionView;
begin
  Result := GViews.ViewFor(GBindings[ABoard].Items(ALane).ID);
  Check(Result <> nil, 'current physical list has its retained collection view');
end;

procedure SelectFirstCard(ABoard: Integer; const ALane: TNyxKanbanLaneRef);
var
  LView: INyxCollectionView;
  LIndex: Integer;
  {$ifdef PAS2JS}
  LRow: TJSHTMLElement;
  {$else}
  LList: TListBox;
  {$endif}
begin
  LView := LaneView(ABoard, ALane);
  LIndex := LView.Snapshot.IndexOf(FirstCard);
  Check(LIndex >= 0, 'source lane visibly contains the stable card');
  {$ifdef PAS2JS}
  LRow := TJSHTMLElement(GRenderer.ElementFor(GBindings[ABoard].Items(ALane).ID)
    .querySelector('[data-nyx-item="welcome"]'));
  Check(LRow <> nil, 'source list has a physical row for the stable card');
  LRow.focus;
  LRow.click;
  {$else}
  LList := TListBox(GRenderer.ControlFor(GBindings[ABoard].Items(ALane).ID));
  LList.SetFocus;
  LList.ItemIndex := LIndex;
  Check(Assigned(LList.OnSelectionChange), 'ordinary native selection handler is mounted');
  LList.OnSelectionChange(LList, True);
  {$endif}
  Pump;
  Check(LView.HasSelection and (LView.Selected.ID = FirstCard.ID),
    'physical selection reaches the typed view');
end;

function MoveEnabled(ABoard: Integer; const ALane: TNyxKanbanLaneRef): Boolean;
var
  LID: TNyxText;
begin
  LID := GBindings[ABoard].MoveButton(ALane).ID;
  {$ifdef PAS2JS}
  Result := not TJSHTMLButtonElement(GRenderer.ElementFor(LID)).disabled;
  {$else}
  Result := GRenderer.ControlFor(LID).Enabled;
  {$endif}
end;

procedure MoveByKeyboard(ABoard: Integer; const ATo: TNyxKanbanLaneRef);
var
  LID: TNyxText;
  {$ifndef PAS2JS}
  LButton: TWinControl;
  {$endif}
begin
  LID := GBindings[ABoard].MoveButton(ATo).ID;
  Check(MoveEnabled(ABoard, ATo), 'actual destination button is enabled');
  {$ifdef PAS2JS}
  { DOM click exercises the actual button/event route. Browser trusted keyboard
    defaults require a separately admitted browser input run. }
  GRenderer.ElementFor(LID).click;
  {$else}
  LButton := TWinControl(GRenderer.ControlFor(LID));
  LButton.SetFocus;
  Check(GHost.ActiveControl = LButton, 'Move is an ordinary focusable native button');

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

{$ifndef PAS2JS}
{ Current standard LCL hooks construct the actual Nyx drag object and negotiate
  it with another mounted list. This is host synthesis, not hardware input. }
procedure DragSelected(const ASource, ATarget: INyxList; AAccepted: Boolean);
var
  LSource: TControl;
  LTarget: TControl;
  LDrag: TDragObject;
  LAccept: Boolean;
begin
  LSource := GRenderer.ControlFor(ASource.ID);
  LTarget := GRenderer.ControlFor(ATarget.ID);
  Check(Assigned(TControlAccess(LSource).OnStartDrag), 'selected-card source has a drag hook');
  Check(Assigned(TControlAccess(LTarget).OnDragOver), 'lane target has a negotiation hook');
  LDrag := nil;
  TControlAccess(LSource).OnStartDrag(LSource, LDrag);
  try
    Check(LDrag <> nil, 'native drag object carries the offered selected card');
    LAccept := False;
    TControlAccess(LTarget).OnDragOver(LTarget, LDrag, 5, 5, dsDragEnter, LAccept);
    Check(LAccept = AAccepted, 'target admits only its own live card transfer');

    if LAccept then
    begin
      TControlAccess(LTarget).OnDragDrop(LTarget, LDrag, 5, 5);
    end;
    TControlAccess(LSource).OnEndDrag(LSource, LTarget, 5, 5);
  finally
    LDrag.Free;
  end;
  Pump;
end;

procedure Capture(const AName: String);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LDC: HDC;
begin

  if GEvidence = '' then
  begin
    Exit;
  end;
  GHost.Update;
  Pump;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(GHost.ClientWidth, GHost.ClientHeight);
    LDC := GetDC(GHost.Handle);
    try
      Check(BitBlt(LBitmap.Canvas.Handle, 0, 0, LBitmap.Width, LBitmap.Height,
        LDC, 0, 0, SRCCOPY), 'owned native client capture succeeds');
    finally
      ReleaseDC(GHost.Handle, LDC);
    end;
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(GEvidence) + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;
{$endif}

procedure Run;
var
  LStore: INyxCollection;
  LSibling: INyxCollection;
  LRevision: Integer;
  LMove: INyxKanbanMove;
  LRejected: Boolean;
  LFormer: INyxKanbanBinding;
  LGeneration: INyxCancellationScope;
  LFormerRoot: TNyxNode;
  LInstance: INyxControl;
  LMaskedMove: TNyxNode;

  procedure ReadAutomaticBindings;
  begin
    GBindings[0] := GRenderer.KanbanBinding(NyxQualifiedID('first-board', 'workflow'));
    GBindings[1] := GRenderer.KanbanBinding(NyxQualifiedID('second-board', 'workflow'));
    Check((GBindings[0] <> nil) and GBindings[0].Connected and
      (GBindings[1] <> nil) and GBindings[1].Connected,
      'accepted view supplies both managed automatic bindings');
  end;

begin
  LStore := LaneView(0, Plan).Store;
  LSibling := LaneView(1, Plan).Store;
  Check(LStore <> LSibling, 'reusable boards own independent instance stores');
  Check((LaneView(0, Plan).Snapshot.Count = 1) and
    (LaneView(0, Build).Snapshot.Count = 1) and
    (LaneView(0, Complete).Snapshot.Count = 1), 'three query views start in distinct lanes');
  Check(not MoveEnabled(0, Build), 'Move has no implicit card before physical selection');
  SelectFirstCard(0, Plan);
  LRevision := LStore.Snapshot.Revision;
  MoveByKeyboard(0, Build);
  Check(LStore.Snapshot.Revision = LRevision + 1, 'one keyboard Move publishes one revision');
  Check((not LaneView(0, Plan).Snapshot.Has(FirstCard)) and
    LaneView(0, Build).Snapshot.Has(FirstCard), 'same stable card moves between actual bound lists');
  Check(LStore.Snapshot.Count = 3, 'lane movement neither removes nor duplicates the card');
  Check((LSibling.Snapshot.Revision = 0) and
    (LSibling.Snapshot.Item(FirstCard).GetValue(CardLane) = Plan.Name),
    'the second rendered reusable board remains unchanged');
  Check(LStore.Snapshot.Item(FirstCard).GetValue(CardTitle) = 'Build a welcoming page',
    'moving changes only the lane field');
  {$ifndef PAS2JS}
  Capture('moved');
  SelectFirstCard(0, Build);
  LRevision := LStore.Snapshot.Revision;
  DragSelected(GBindings[0].Items(Build), GBindings[0].Items(Plan), True);
  Check((LStore.Snapshot.Revision = LRevision + 1) and
    LaneView(0, Plan).Snapshot.Has(FirstCard), 'actual native drop uses the same typed move');
  SelectFirstCard(0, Plan);
  LRevision := LStore.Snapshot.Revision;
  DragSelected(GBindings[0].Items(Plan), GBindings[1].Items(Build), False);
  Check((LStore.Snapshot.Revision = LRevision) and (LSibling.Snapshot.Revision = 0),
    'foreign board drop preserves both complete datasets');
  {$else}
  SelectFirstCard(0, Build);
  MoveByKeyboard(0, Plan);
  Check(LaneView(0, Plan).Snapshot.Has(FirstCard), 'second actual button reverses the move');
  {$endif}
  SelectFirstCard(0, Plan);
  LMove := GBindings[0].CaptureMove(FirstCard, Plan, Build, LStore.Snapshot.Revision);
  LStore.Apply([NyxUpdate(NyxCollectionItem(NyxItem(Cards, 'details'))
    .WithValue(CardTitle, 'A newer accepted title'))], LStore.Snapshot.Revision);
  LRevision := LStore.Snapshot.Revision;
  LRejected := False;
  try
    LMove.Commit;
  except
    on ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and not LMove.Committed and
    (LStore.Snapshot.Revision = LRevision) and LaneView(0, Plan).Snapshot.Has(FirstCard),
    'captured stale move refuses without overwriting accepted work');
  Check(TNyxCodec.Encode(GDocument) = GAuthored, 'runtime changes preserve authored defaults and source');
  LMove := GBindings[0].CaptureMove(FirstCard, Plan, Complete, LStore.Snapshot.Revision);
  LFormer := GBindings[0];
  LGeneration := NyxEventGeneration(GRenderer.Events);
  LFormerRoot := GRenderer.Root;
  GRefuseCandidate := True;
  LRejected := False;
  try
    try
      GRenderer.Render(GDocument, GDocument.Find('board-review'), GHost, False, nil, GViews);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
  finally
    GRefuseCandidate := False;
  end;
  Check(LRejected and (GRenderer.Root = LFormerRoot) and LFormer.Connected and
    not LGeneration.Cancelled and (NyxEventGeneration(GRenderer.Events) = LGeneration) and
    (LStore.Snapshot.Revision = LRevision),
    'failed real factory preserves current boards, generation and accepted data');
  GRenderer.Render(GDocument, GDocument.Find('board-review'), GHost, False, nil, GViews);
  Pump;
  ReadAutomaticBindings;
  Check(not LFormer.Connected and LGeneration.Cancelled and
    (GBindings[0] <> LFormer) and (LaneView(0, Plan).Store = LStore),
    'successful same-ID replacement owns a fresh binding and preserves the runtime dataset');
  LRejected := False;
  try
    LMove.Commit;
  except
    on ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and not LMove.Committed and (LStore.Snapshot.Revision = LRevision),
    'retained command cannot cross a same-ID automatic view replacement');
  SelectFirstCard(0, Plan);
  MoveByKeyboard(0, Complete);
  Check(LaneView(0, Complete).Snapshot.Has(FirstCard),
    'fresh automatic callbacks move a card after successful replacement');
  LInstance := RetainNyxControl(GDocument.Find('first-board'));
  SetNyxKanbanIntent(LInstance, TNyxKanbanIntent.Disabled);
  GRenderer.Render(GDocument, GDocument.Find('board-review'), GHost, False, nil, GViews);
  Pump;
  Check(GRenderer.KanbanBinding(NyxQualifiedID('first-board', 'workflow')) = nil,
    'one disabled reusable occurrence exposes no movement controller');
  LMaskedMove := GRenderer.Root.Find(NyxQualifiedID('first-board', 'workflow'))
    .Part(NyxPart('lane-1/move'));
  {$ifdef PAS2JS}
  Check(TJSHTMLButtonElement(GRenderer.ElementFor(LMaskedMove.ID)).disabled,
    'disabled intent masks the actual inherited Move face');
  {$else}
  Check(not GRenderer.ControlFor(LMaskedMove.ID).Enabled,
    'disabled intent masks the actual inherited Move face');
  {$endif}
  Check(LMaskedMove.StoredProp(NyxAttributeName(atEnabled), 'true') = 'true',
    'runtime mask preserves the authored Enabled setting');
  GBindings[1] := GRenderer.KanbanBinding(NyxQualifiedID('second-board', 'workflow'));
  Check((GBindings[1] <> nil) and GBindings[1].Connected,
    'disabled instance leaves the rendered sibling functional');
  SelectFirstCard(1, Plan);
  MoveByKeyboard(1, Build);
  Check(LaneView(1, Build).Snapshot.Has(FirstCard) and
    (LStore.Snapshot.Item(FirstCard).GetValue(CardLane) = Complete.Name),
    'the independent sibling moves while the masked board retains its data');
  {$ifndef PAS2JS}
  Capture('independent');
  {$endif}
  InheritNyxKanbanIntent(LInstance);
  LInstance := nil;
  Check(TNyxCodec.Encode(GDocument) = GAuthored,
    'inherit restores the complete original authored document');
  GRenderer.Render(GDocument, GDocument.Find('board-review'), GHost, False, nil, GViews);
  Pump;
  ReadAutomaticBindings;
  LFormer := GBindings[0];
  GRenderer.Render(GDocument, GDocument.Find('board-review'), GHost, True, nil, GViews);
  Check(not LFormer.Connected and
    (GRenderer.KanbanBinding(NyxQualifiedID('first-board', 'workflow')) = nil),
    'design mode retires live movement and keeps authored parts editable');
  GRenderer.Render(GDocument, GDocument.Find('board-review'), GHost, False, nil, GViews);
  Pump;
  ReadAutomaticBindings;
  SelectFirstCard(0, Complete);
  LRevision := LStore.Snapshot.Revision;
  LMove := GBindings[0].CaptureMove(FirstCard, Complete, Plan, LRevision);
  GRenderer.Unmount;
  Check(not GBindings[0].Connected and not GBindings[1].Connected,
    'ordinary Unmount revokes every automatic board before controls retire');
  LRejected := False;
  try
    LMove.Commit;
  except
    on ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and not LMove.Committed and (LStore.Snapshot.Revision = LRevision),
    'retained automatic command refuses safely after full view retirement');
  GRenderer.Render(GDocument, GDocument.Find('board-review'), GHost, False, nil, GViews);
  Pump;
  ReadAutomaticBindings;
  SelectFirstCard(0, Complete);
  MoveByKeyboard(0, Plan);
  Check((LStore.Snapshot.Revision = LRevision + 1) and
    LaneView(0, Plan).Snapshot.Has(FirstCard),
    'the same renderer becomes interactive again after Unmount and remount');
end;

{ Continue the original complete journey with common-binding identity and
  deliberate retirement. No old stage, assertion, allocation or wait is changed.
  Retained facades own the old nodes; their existence is not command authority. }
procedure CheckCommonOwnership;
var
  LCommon: INyxCoordinatorBinding;
  LTyped: INyxKanbanBinding;
  LFormer: INyxKanbanBinding;
  LManual: INyxKanbanBinding;
  LItems: INyxList;
  LMoveFace: INyxButton;
  LControl: INyxControl;
  LBoard: INyxKanbanBoard;
  LGeneration: INyxCancellationScope;
  LStore: INyxCollection;
  LRevision: Integer;
  LRejected: Boolean;
  LIndex: Integer;
  LFactory: TNotificationFactory;
  LFactoryLease: INyxCoordinatorFactory;
  LRegistry: INyxCoordinatorRegistry;
  LInstanceID: TNyxText;
begin
  for LIndex := 0 to 1 do
  begin

    if LIndex = 0 then
    begin
      LInstanceID := 'first-board';
    end
    else
    begin
      LInstanceID := 'second-board';
    end;
    LCommon := GRenderer.CoordinatorBinding(NyxQualifiedID(LInstanceID, 'workflow'));
    LTyped := nil;
    Check(Supports(LCommon, INyxKanbanBinding, LTyped) and
      (LTyped = GBindings[LIndex]), 'generic lookup preserves exact specialized board binding');
  end;
  LStore := LaneView(0, Plan).Store;
  LFormer := GBindings[0];
  LItems := LFormer.Items(Plan);
  LMoveFace := LFormer.MoveButton(Build);
  Check((LItems.Node = GRenderer.Root.Find(LItems.ID)) and
    (LMoveFace.Node = GRenderer.Root.Find(LMoveFace.ID)),
    'public specialized facades retain actual canonical list and Move nodes');
  LItems.Configure.ReadOnly(True).Done;
  Check(GRenderer.Root.Find(LItems.ID).Prop(NyxAttributeName(atReadOnly), 'false') = 'true',
    'specialized Configure changes the actual mounted list');
  LRevision := LStore.Snapshot.Revision;
  LRejected := False;
  try
    LFormer.CaptureMove(FirstCard, Plan, Build, LRevision);
  except
    on ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LStore.Snapshot.Revision = LRevision),
    'canonical read-only policy refuses movement without changing rows');
  GRenderer.Render(GDocument, GDocument.Find('board-review'), GHost, False, nil, GViews);
  Pump;
  GBindings[0] := GRenderer.KanbanBinding(NyxQualifiedID('first-board', 'workflow'));
  GBindings[1] := GRenderer.KanbanBinding(NyxQualifiedID('second-board', 'workflow'));
  Check(not LFormer.Connected and (LItems.ID = GBindings[0].Items(Plan).ID) and
    (LItems.Node <> GBindings[0].Items(Plan).Node) and
    (LMoveFace.Node <> GBindings[0].MoveButton(Build).Node),
    'same-ID successor keeps fresh canonical facades distinct from retained old nodes');
  LItems.Configure.ReadOnly(False).Done;
  LItems.Configure.ReadOnly(True).Done;
  Check((LItems.Node.Prop(NyxAttributeName(atReadOnly), 'false') = 'true') and
    (GBindings[0].Items(Plan).Node.Prop(NyxAttributeName(atReadOnly), 'false') = 'false'),
    'mutating a retained former facade cannot change the new control');

  LFormer := GBindings[0];
  LGeneration := NyxEventGeneration(GRenderer.Events);
  LFormer.Disconnect;
  LFormer.Disconnect;
  LCommon := GRenderer.CoordinatorBinding(NyxQualifiedID('first-board', 'workflow'));
  Check(not LFormer.Connected and not LCommon.Connected and
    not LGeneration.Cancelled and GBindings[1].Connected,
    'exact typed Disconnect is idempotent and preserves sibling and enclosing generation');
  LControl := RetainNyxControl(GRenderer.Root.Find(NyxQualifiedID('first-board', 'workflow')));
  Check(Supports(LControl, INyxKanbanBoard, LBoard),
    'same accepted runtime board retains its specialized control interface');
  LManual := NewNyxKanbanBinding(LBoard, GOptions, GViews, GRenderer.Events,
    GRenderer.Events.Scheduler, nil);
  GBindings[0] := LManual;
  Check(LManual.Connected and (LManual.Items(Plan).Node =
    GRenderer.Root.Find(LManual.Items(Plan).ID)),
    'manual attachment reacquires exact canonical parts on the same live event generation');
  SelectFirstCard(0, Plan);
  GRenderer.Sync;
  MoveByKeyboard(0, Build);
  Check((LStore.Snapshot.Revision = LRevision + 1) and
    LaneView(0, Build).Snapshot.Has(FirstCard),
    'real input works after deliberate common-to-manual handoff');
  LManual.Disconnect;
  LManual := nil;

  { A normal creator notification runs after accepted built-in ownership. Both
    failure and disposal must preserve the distinction from precommit refusal. }
  LRegistry := NewNyxCoordinatorRegistry;
  LFactory := TNotificationFactory.Create;
  LFactoryLease := LFactory;
  LFactory.Mode := nnmFailure;
  LRegistry.RegisterFactory(NyxCustomKind(NyxKindName(nkLabel)),
    NyxExtension(CNotificationIntent), LFactoryLease);
  GDocument.Find('review-help').Extensions.SetValue(
    NyxExtension(CNotificationIntent), NyxData(True));
  GRenderer.CoordinatorRegistry := LRegistry;
  LGeneration := NyxEventGeneration(GRenderer.Events);
  LRejected := False;
  try
    GRenderer.Render(GDocument, GDocument.Find('board-review'), GHost, False, nil, GViews);
  except
    on E: Exception do
    begin
      LRejected := Pos('Deliberate committed notification failure', E.Message) > 0;
    end;
  end;
  Check(LRejected and LGeneration.Cancelled and (GPublished <> nil) and GPublished.Connected,
    'notification failure reports committed acceptance of the new real Kanban binding');
  GBindings[0] := GRenderer.KanbanBinding(NyxQualifiedID('first-board', 'workflow'));
  GBindings[1] := GRenderer.KanbanBinding(NyxQualifiedID('second-board', 'workflow'));
  Check(GPublished = GBindings[0], 'postcommit failure retains the exact published binding');
  LFactory.Mode := nnmDispose;
  LRejected := False;
  try
    GRenderer.Render(GDocument, GDocument.Find('board-review'), GHost, False, nil, GViews);
  except
    on E: Exception do
    begin
      LRejected := Pos('Deliberate committed notification failure', E.Message) > 0;
    end;
  end;
  Check(LRejected and (GRenderer = nil) and (GPublished <> nil) and not GPublished.Connected,
    'notification-triggered disposal disconnects the newly accepted board safely');
  GDocument.Find('review-help').Extensions.Remove(NyxExtension(CNotificationIntent));
  Check(TNyxCodec.Encode(GDocument) = GAuthored,
    'common lifecycle proof restores the complete original authored document');
end;

procedure Initialize;
var
  LRoot: TNyxNode;
  LExpected: TNyxDocument;
  LCode: TNyxText;
  LIndex: Integer;
  {$ifndef PAS2JS}
  LStream: TFileStream;
  {$endif}
begin
  GOptions := NyxKanbanOptions(Cards, CardTitle, CardLane)
    .Lane(Plan, 'Plan').Lane(Build, 'Build').Lane(Complete, 'Complete').Scoped(csInstance);
  LExpected := Fixture;
  {$ifdef NYX_COMPILED_KANBAN}
  GDocument := BuildNyxDocument;
  {$else}
  GDocument := Fixture;
  {$endif}
  try
    Check(TNyxCodec.Encode(LExpected) = TNyxCodec.Encode(GDocument),
      'full authored or compiled companion reproduces exact board definitions');
  finally
    LExpected.Free;
  end;
  GAuthored := TNyxCodec.Encode(GDocument);
  LCode := TNyxCodegen.Generate(GDocument, 'nyx.generated.kanban');
  Check((Pos('INyxKanbanBoard', LCode) > 0) and (Pos('INyxButton', LCode) > 0) and
    (Pos('NyxCollectionView', LCode) > 0) and (Pos('SetNyxKanbanIntent', LCode) > 0),
    'crafted source retains specialized parts, typed queries and automatic intent');
  {$ifndef PAS2JS}

  if GEvidence <> '' then
  begin
    ForceDirectories(GEvidence);
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(GEvidence) +
      'nyx.generated.kanban.pas', fmCreate);
    try

      if Length(LCode) > 0 then
      begin
        LStream.WriteBuffer(LCode[1], Length(LCode));
      end;
    finally
      LStream.Free;
    end;
  end;
  {$endif}
  LRoot := RealizeNyxView(GDocument, GDocument.Find('board-review'));
  try
    GViews := NewNyxCollectionBindings(LRoot, NewNyxCollectionContext(GDocument.Collections));
  finally
    LRoot.Free;
  end;
  {$ifdef PAS2JS}
  GRenderer.RegisterFactory('label', @ReviewLabel);
  {$else}
  GRenderer.RegisterFactory('label', ReviewLabel);
  {$endif}
  GRenderer.Render(GDocument, GDocument.Find('board-review'), GHost, False, nil, GViews);
  Pump;
  SetLength(GBindings, 2);
  for LIndex := 0 to 1 do
  begin

    if LIndex = 0 then
    begin
      LRoot := GRenderer.Root.Find(NyxQualifiedID('first-board', 'workflow'));
    end
    else
    begin
      LRoot := GRenderer.Root.Find(NyxQualifiedID('second-board', 'workflow'));
    end;
    GBindings[LIndex] := GRenderer.KanbanBinding(LRoot.ID);
    Check((GBindings[LIndex] <> nil) and GBindings[LIndex].Connected,
      'renderer automatically attaches the actual mounted board without handwritten binding code');
  end;
end;

procedure Release;
var
  LIndex: Integer;
begin
  { The renderer owns automatic attachment retirement. Retained public binding
    references must already be revoked when its actual controls are gone. }
  GRenderer.Free;
  GRenderer := nil;
  for LIndex := 0 to High(GBindings) do
  begin

    if GBindings[LIndex] <> nil then
    begin

      if GBindings[LIndex].Connected then
      begin
        raise ENyxKanban.Create('Renderer disposal retained a live automatic binding');
      end;
      GBindings[LIndex] := nil;
    end;
  end;
  GViews := nil;
  GPublished := nil;
  GDocument.Free;
  GDocument := nil;
  {$ifndef PAS2JS}
  GHost.Free;
  GHost := nil;
  Application.OnException := nil;
  GFailures.Free;
  {$endif}
end;

begin
  {$ifndef PAS2JS}
  Application.Initialize;
  GFailures := TFailureObserver.Create;
  Application.OnException := GFailures.Failed;
  GEvidence := ParamStr(1);
  GWidth := StrToIntDef(ParamStr(2), 1280);
  GReview := ParamStr(3) = '--review';
  GHost := TForm.CreateNew(nil);
  GHost.Caption := 'Nyx Kanban';
  GHost.ClientWidth := GWidth;
  GHost.ClientHeight := 900;
  GHost.Show;
  GRenderer := TNyxLCLRenderer.Create;
  {$else}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  GRenderer := TNyxBrowserRenderer.Create;
  GReview := window.location.search = '?review';
  {$endif}
  try
    try
      Initialize;
      {$ifndef PAS2JS}
      Check(GHost.ClientWidth = GWidth, 'native owner has the requested actual client width');
      {$endif}

      if not GReview then
      begin
        Run;
        CheckCommonOwnership;
      end;
      {$ifdef PAS2JS}

      if GReview then
      begin
        document.body.setAttribute('data-kanban-controls', 'ready');
      end
      else
      begin
        document.body.setAttribute('data-kanban-controls', 'passed');
      end;
      document.body.setAttribute('data-kanban-checks', IntToStr(GChecks));
      {$else}

      if GReview then
      begin
        WriteLn('READY interactive native Kanban / width=', GHost.ClientWidth);
      end
      else
      begin
        WriteLn('PASS ', GChecks, ' actual native Kanban checks / width=', GHost.ClientWidth);
      end;
      {$endif}
      {$ifndef PAS2JS}

      if GReview then
      begin
        { Explicit interactive launch keeps the same public application route
          alive. Default qualification remains finite and exits independently. }
        while GHost.Visible and not Application.Terminated do
        begin
          Application.HandleMessage;
        end;
      end;
      {$endif}
    except
      on LException: Exception do
      begin
        {$ifdef PAS2JS}
        document.body.setAttribute('data-kanban-controls', 'failed');
        document.body.setAttribute('data-kanban-error', LException.Message);
        {$else}
        WriteLn('FAIL ', LException.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
        {$endif}
      end;
    end;
  finally
    {$ifdef PAS2JS}

    if not GReview then
    begin
      Release;
    end;
    {$else}
    Release;
    {$endif}
  end;
end.
