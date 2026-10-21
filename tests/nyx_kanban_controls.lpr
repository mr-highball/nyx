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
  nyx.collections.bindings, nyx.collections.view.types, nyx.kanban
  {$ifdef NYX_COMPILED_KANBAN}, nyx.generated.kanban{$endif};

type
  { Presentation borrows the current renderer only while both attachments live.
    It is released after Disconnect and before the renderer or form retires. }
  TKanbanPresentation = class
  public
    procedure Refresh;
  end;
  {$ifndef PAS2JS}
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

var
  GDocument: TNyxDocument;
  GViews: INyxCollectionBindings;
  GOptions: TNyxKanbanOptions;
  GBindings: array of INyxKanbanBinding;
  GPresentation: TKanbanPresentation;
  GAuthored: TNyxText;
  GChecks: Integer;
  GReview: Boolean;
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

procedure TKanbanPresentation.Refresh;
begin
  GRenderer.Sync;
end;

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
  GBindings[0].Disconnect;
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
    'retained command safely refuses after explicit page attachment retirement');
  Check(GBindings[1].Connected, 'retiring one board preserves the other attachment');
  {$ifndef PAS2JS}
  Capture('independent');
  {$endif}
end;

procedure Initialize;
var
  LRoot: TNyxNode;
  LBoard: INyxKanbanBoard;
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
    (Pos('NyxCollectionView', LCode) > 0), 'crafted source retains specialized parts and typed queries');
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
    LBoard := RetainNyxControl(LRoot) as INyxKanbanBoard;
    GBindings[LIndex] := NewNyxKanbanBinding(LBoard, GOptions, GViews,
      GRenderer.Events, GRenderer.Events.Scheduler, GPresentation.Refresh);
    Check(GBindings[LIndex].Connected, 'explicit code-first binding attaches to the actual mounted board');
  end;
end;

procedure Release;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(GBindings) do
  begin

    if GBindings[LIndex] <> nil then
    begin
      GBindings[LIndex].Disconnect;
      GBindings[LIndex] := nil;
    end;
  end;
  GRenderer.Free;
  GRenderer := nil;
  GViews := nil;
  GPresentation.Free;
  GPresentation := nil;
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
  GPresentation := TKanbanPresentation.Create;
  try
    try
      Initialize;
      {$ifndef PAS2JS}
      Check(GHost.ClientWidth = GWidth, 'native owner has the requested actual client width');
      {$endif}

      if not GReview then
      begin
        Run;
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
