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
program nyx_service_monitor_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, Grids, Windows, Types,
  nyx.text, nyx.types, nyx.model, nyx.codec, nyx.codegen, nyx.files, nyx.files.lcl,
  nyx.disclosure, nyx.state, nyx.scheduler, nyx.clipboard, nyx.bytes,
  nyx.files.save, nyx.editing.lcl, nyx.menu.lcl, nyx.popover.lcl,
  nyx.collections, nyx.collections.codec, nyx.collections.view.types, nyx.collections.view,
  nyx.collections.mount, nyx.collections.arrangement,
  nyx.collections.arrangement.controls, nyx.collections.arrangement.presenter,
  nyx.collections.arrangement.lcl, nyx.collections.query, nyx.collections.paging,
  nyx.collections.cell.actions, nyx.collections.clipboard.actions,
  nyx.collections.export, nyx.collections.export.actions, nyx.collections.workbench.runtime,
  nyx.application.lcl, nyx.service.monitor,
  nyx.studio.lcl, nyx.studio.projects, nyx.studio.projectstore,
  nyx.studio.section.views, nyx.studio.collections, nyx.studio.inspector,
  nyx.studio.menu, nyx.studio.authoring, nyx.studio.collectionintent
  {$ifdef NYX_SERVICE_STUDIO_GENERATED}, nyx.generated.view{$endif};

type
  { Invoke ordinary LCL Click dispatch on borrowed mounted controls. This is
    synthetic widget input, not hardware mouse or accessibility evidence. }
  TControlAccess = class(TControl);

  { Capture exceptions from asynchronous native UI notifications so a failed
    update cannot be mistaken for a quiescent successful Studio operation. }
  TFailureObserver = class
  public
    Text: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

  { Shared request lifetime exercises ordinary terminal delivery. The fixture
    records text only; no platform clipboard API is invoked. }
  TCopyRequest = class(TNyxClipboardWriteRequest)
  public
    procedure Finish(AOutcome: TNyxClipboardWriteOutcome);
  end;

  { Explicit test authority captures complete text and a caller-selected typed
    outcome. It never owns a widget, document or borrowed reply after delivery. }
  TCopyHost = class(TInterfacedObject, INyxClipboardWriteHost)
  public
    Calls: Integer;
    Text: TNyxText;
    Outcome: TNyxClipboardWriteOutcome;
    function BeginWriteText(const AText: TNyxText; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope;
      AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
  end;

  { Ordinary save-request validation preserves written versus cancelled or
    unavailable outcomes. This fixture grants no disk or picker authority. }
  TSaveRequest = class(TNyxFileSaveRequest)
  public
    procedure Finish(AOutcome: TNyxFileSaveOutcome);
  end;

  { Exact copied bytes qualify the menu-to-host transfer independently of the
    real native file adapter's separately maintained filesystem checks. }
  TSaveHost = class(TInterfacedObject, INyxFileSaveHost)
  public
    Calls: Integer;
    Bytes: TNyxBytes;
    Outcome: TNyxFileSaveOutcome;
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

var
  GChecks: Integer;
  GWidth: Integer;
  GEvidence: String;
  GFailure: TFailureObserver;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Service Studio: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Text := TNyxText(AException.Message);
end;

procedure TCopyRequest.Finish(AOutcome: TNyxClipboardWriteOutcome);
begin

  if AOutcome = ncwoWritten then
  begin
    Complete(AOutcome, FByteCount, '', nctrExactUTF8);
  end
  else
  begin
    Complete(AOutcome, 0);
  end;
end;

function TCopyHost.BeginWriteText(const AText: TNyxText;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
var
  LRequest: TCopyRequest;
begin
  LRequest := TCopyRequest.Create(AText, AScheduler, ACancellation, AReply);
  Result := LRequest;
  Inc(Calls);
  Text := AText;
  LRequest.Finish(Outcome);
end;

procedure TSaveRequest.Finish(AOutcome: TNyxFileSaveOutcome);
begin

  if AOutcome in [nfsoWritten, nfsoDownloadRequested] then
  begin
    Complete(AOutcome, FOptions.SuggestedName, Length(FBytes));
  end
  else
  begin
    Complete(AOutcome, FOptions.SuggestedName, 0);
  end;
end;

function TSaveHost.BeginSave(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope;
  AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LRequest: TSaveRequest;
begin
  LRequest := TSaveRequest.Create(AOptions, ABytes, AScheduler, ACancellation, AReply);
  Result := LRequest;
  Inc(Calls);
  Bytes := Copy(ABytes, 0, Length(ABytes));
  LRequest.Finish(Outcome);
end;

procedure Pump;
begin
  Application.ProcessMessages;
  CheckSynchronize;

  if GFailure.Text <> '' then
  begin
    raise Exception.Create(GFailure.Text);
  end;
end;

{ A completed source command can still have a pending presentation turn. Wait
  on both existing owners, with a fixed deadline and no nested test mutation. }
procedure Ready(AStudio: TNyxNativeStudio);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Studio did not settle: ' + AStudio.Status);
    end;
    Sleep(1);
  until not AStudio.PresentationPending and not AStudio.SourceBusy;
end;

procedure Click(AStudio: TNyxNativeStudio; const AID: TNyxText);
var
  LControl: TControl;
begin
  Check(AStudio.ShellView.RootFor(AID) <> nil, 'mounted Studio action ' + AID);
  AStudio.ShellView.ViewFor(AID).Reveal(AID, niRuntime);
  LControl := AStudio.ShellView.ControlFor(AID);
  Check((LControl <> nil) and LControl.Enabled, 'enabled Studio action ' + AID);
  TControlAccess(LControl).Click;
  Ready(AStudio);
end;

procedure Change(AStudio: TNyxNativeStudio; const AID, AValue: TNyxText);
var
  LControl: TControl;
  LIndex: Integer;
begin
  LControl := AStudio.ShellView.InputFor(AID);
  Check((LControl <> nil) and LControl.Enabled, 'mounted Studio input ' + AID);
  AStudio.ShellView.ViewFor(AID).Reveal(AID, niRuntime);

  if LControl is TCheckBox then
  begin
    TCheckBox(LControl).Checked := AValue = 'true';
    TCheckBox(LControl).OnChange(LControl);
  end
  else if LControl is TComboBox then
  begin
    LIndex := TComboBox(LControl).Items.IndexOf(AValue);
    Check(LIndex >= 0, 'requested typed choice is available');
    TComboBox(LControl).ItemIndex := LIndex;
    TComboBox(LControl).OnChange(LControl);
  end
  else
  begin
    Check(LControl is TEdit, 'ordinary scalar input uses its edit adapter');
    TEdit(LControl).Text := AValue;
    TEdit(LControl).OnChange(LControl);
  end;
  Ready(AStudio);
end;

{ Resolve the real visible Project popup by caption, as the existing complete
  query journey does. The returned control is borrowed for this dispatch only. }
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

procedure MenuClick(AStudio: TNyxNativeStudio;
  const AWindow, ACaption: String);
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
  Check((LButton <> nil) and LButton.Enabled, 'visible Project menu ' + ACaption);
  TControlAccess(LButton).Click;
  Ready(AStudio);
end;

function StudioPair(AStudio: TNyxNativeStudio): TNyxText;
begin
  Result := EncodeNyxProject(AStudio.ProjectSnapshot);
end;

procedure History(AStudio: TNyxNativeStudio; const ABefore: TNyxText);
var
  LAfter: TNyxText;
begin
  LAfter := StudioPair(AStudio);
  Check(LAfter <> ABefore, 'one visible Apply changes the accepted pair');
  Click(AStudio, 'action-undo');
  Check(StudioPair(AStudio) = ABefore, 'Undo restores exact design and Pascal');
  Click(AStudio, 'action-redo');
  Check(StudioPair(AStudio) = LAfter, 'Redo restores exact design and Pascal');
end;

{ The actual public English example supplies all data and table policies.
  No private approximation of the workbench or replacement query model is used. }
function StudioJourney: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LSeed: TNyxProjectPair;
  LHost: TForm;
  LStudio: TNyxNativeStudio;
  LBefore: TNyxText;
  LSaved: TNyxText;
  LDefaults: TNyxText;
  LStore: TNyxProjectStore;
  LRevision: TNyxText;
  LSection: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
begin
  LDocument := BuildServiceMonitorDocument;
  LHost := nil;
  LStudio := nil;
  try
    LSeed := NyxProjectPair(TNyxCodec.Encode(LDocument),
      '{ Service workspace: keep this handwritten comment. }' + #10 +
      TNyxCodegen.Generate(LDocument));
    LDefaults := EncodeNyxCollectionDefaults(LDocument.Collections);
    LHost := TForm.CreateNew(nil);
    LHost.ClientWidth := GWidth;
    LHost.ClientHeight := 900;
    LHost.Show;
    Check(LHost.ClientWidth = GWidth, 'Studio uses the requested actual width');
    LStudio := TNyxNativeStudio.Create(LHost, GEvidence + '/projects');
    LStudio.LoadProject(LSeed);
    LStudio.Session.Activate('services');
    LStudio.Session.Select(ServiceTableID);
    LStudio.Run;
    Ready(LStudio);

    if LStudio.ShellView.RootFor('action-panel-inspector') <> nil then
    begin
      Click(LStudio, 'action-panel-inspector');
    end;
    Click(LStudio, NyxStudioBindingsToggleID);
    Check(LStudio.Session.SelectedID = ServiceTableID, 'public service table selected');
    LBefore := StudioPair(LStudio);
    Change(LStudio, 'collection-column-0-fixed', '160');
    LSpec := LStudio.Session.Selected.CollectionView;
    Check((LSpec.ColumnAt(0).FieldName = 'name') and
      (LSpec.ColumnAt(0).Sizing.Value = 160), 'visible width field authors Service width');
    History(LStudio, LBefore);

    Check(not LStudio.ShellView.ControlFor(NyxStudioColumnOrderID(0, scaColumnEarlier)).Enabled and
      not LStudio.ShellView.ControlFor(NyxStudioColumnOrderID(4, scaColumnLater)).Enabled,
      'actual first/last column controls expose their disabled edge moves');
    LBefore := StudioPair(LStudio);
    Click(LStudio, NyxStudioColumnOrderID(3, scaColumnEarlier));
    Check(LStudio.Session.Selected.CollectionView.ColumnAt(2).FieldName = 'notes',
      'ordinary Move earlier moves Notes with its own settings');
    History(LStudio, LBefore);
    LBefore := StudioPair(LStudio);
    Click(LStudio, NyxStudioColumnOrderID(1, scaColumnLater));
    LSpec := LStudio.Session.Selected.CollectionView;
    Check((LSpec.ColumnAt(1).FieldName = 'notes') and
      (LSpec.ColumnAt(2).FieldName = 'pid') and (LSpec.ColumnAt(3).FieldName = 'command'),
      'ordinary Move later authors a complete new default column order');
    History(LStudio, LBefore);

    LSection := LStudio.ShellView.Root.Find(NyxStudioLayoutPolicyFormID(3) + '-section');
    Check(LSection <> nil, 'Command line layout is discoverable in the Inspector');

    if not NyxDisclosureExpanded(LSection) then
    begin
      Click(LStudio, LSection.Part(NyxPart('header')).ID);
    end;
    LBefore := StudioPair(LStudio);
    Change(LStudio, NyxStudioLayoutPolicyFieldID(3, slpfVisible), 'false');
    Check(StudioPair(LStudio) = LBefore, 'visibility remains a draft before Apply');
    Click(LStudio, NyxStudioLayoutPolicyApplyID(3));
    Check(not LStudio.Session.Selected.CollectionView.ColumnAt(3).LayoutPolicy.IsVisible,
      'Apply authors hidden Command line without deleting it');
    History(LStudio, LBefore);
    Check(EncodeNyxCollectionDefaults(LStudio.Session.Document.Collections) = LDefaults,
      'column authoring preserves all public service data');
    Check(Pos('keep this handwritten comment', LStudio.ProjectSnapshot.Source) > 0,
      'ordinary editing retains handwritten source');

    if LStudio.ShellView.RootFor('action-panel-project') <> nil then
    begin
      Click(LStudio, 'action-panel-project');
    end;
    Click(LStudio, NyxStudioActionMenuID);
    MenuClick(LStudio, 'Component actions', 'Project');
    MenuClick(LStudio, 'Project', 'Open project');
    Change(LStudio, 'project-file-name', 'service-workspace');
    LSaved := StudioPair(LStudio);
    Click(LStudio, 'action-project-save');
    LStore := TNyxProjectStore.Create(GEvidence + '/projects');
    try
      Check(LStore.ReadProject('service-workspace', LRevision) = LSaved,
        'ordinary Save writes this exact accepted project to disk');
    finally
      LStore.Free;
    end;
    Click(LStudio, 'action-project-open');
    Check(StudioPair(LStudio) = LSaved, 'ordinary Open restores the own saved pair');
    Check(not LStudio.Session.CanUndo and not LStudio.Session.CanRedo,
      'Open establishes fresh history rather than keeping the previous document');
    Result := LStudio.ProjectSnapshot;
    WriteNyxTextFile(GEvidence + '/accepted-project.nyxproject',
      NyxTextFile('accepted-project.nyxproject', LSaved));
    WriteNyxTextFile(GEvidence + '/nyx.generated.view.pas',
      NyxTextFile('nyx.generated.view.pas', Result.Source));
  finally
    LStudio.Free;
    LHost.Free;
    LDocument.Free;
  end;
end;

{ Exercise real managed Columns buttons. Manager/presenter/renderer are leases
  scoped to the mounted application; no design node borrows these references. }
procedure ColumnClick(const AManager: INyxColumnArrangementManager;
  const AField: TNyxColumnIdentity; AAction: TNyxColumnArrangementAction);
var
  LTarget: INyxLCLColumnArrangementPresenter;
  LID: TNyxText;
begin
  Check(Supports(AManager.Presenter, INyxLCLColumnArrangementPresenter, LTarget),
    'Columns has its real native presenter');

  if AAction in [ncaaResetOrder, ncaaResetAll] then
  begin
    LID := NyxColumnArrangementActionID(NyxColumnArrangementEditorID, AAction);
  end
  else
  begin
    LID := NyxColumnArrangementActionID(NyxColumnArrangementEditorID,
      AManager.Observation, AField, AAction);
  end;
  LTarget.Presentation.Renderer.Reveal(LID, niRuntime);
  Check(LTarget.Presentation.Renderer.ControlFor(LID) <> nil,
    'ordinary Columns action is physically mounted');
  TButton(LTarget.Presentation.Renderer.ControlFor(LID)).Click;
  Pump;
end;

{ Continue the same public application through actual context menus and Nyx
  inspector controls. Only clipboard/file effects are substituted; captures,
  selection, query/paging, editor suspension and command results stay real. }
procedure DataActions(AApplication: TNyxLCLApplication; const AMount: INyxCollectionMount;
  const AManager: INyxColumnArrangementManager; ACopy: TCopyHost; ASave: TSaveHost);
var
  LActionsMount: INyxCollectionCellActionsMount;
  LActions: INyxCollectionCellActions;
  LExports: INyxCollectionExportMount;
  LExportActions: INyxCollectionCellExportActions;
  LClipboard: INyxCollectionClipboardMount;
  LPages: INyxCollectionPages;
  LInspector: INyxLCLPopover;
  LGrid: TStringGrid;
  LColumn: Integer;
  LExpected: TNyxText;
  LPayload: INyxCollectionExportText;
  LRevision: Integer;
  LIndex: Integer;

  procedure DrainActions;
  var
    LTurn: Integer;
  begin
    { Match the established native context driver: idle callbacks own queued
      menu work, so one ProcessMessages call is not a completion fence. }
    for LTurn := 1 to 4 do
    begin
      Pump;
      Application.Idle(False);
    end;
  end;

  function GridFor(const ACaption: TNyxText; out AColumn: Integer): TStringGrid;
  var
    LTable: TStringGrid;
    LCandidate: TStringGrid;
    LChild: Integer;
    LPhysical: Integer;
  begin
    Result := nil;
    LTable := TStringGrid(AApplication.View.ControlFor(ServiceTableID));
    for LChild := -1 to LTable.ControlCount - 1 do
    begin
      LCandidate := LTable;

      if LChild >= 0 then
      begin

        if not (LTable.Controls[LChild] is TStringGrid) then
        begin
          Continue;
        end;
        LCandidate := TStringGrid(LTable.Controls[LChild]);
      end;
      for LPhysical := 0 to LCandidate.ColCount - 1 do
      begin

        if LCandidate.IsVisible and (LCandidate.ColWidths[LPhysical] > 0) and
          (Pos(ACaption, TNyxText(LCandidate.Cells[LPhysical, 0])) = 1) then
        begin
          AColumn := LPhysical;
          Exit(LCandidate);
        end;
      end;
    end;
    raise Exception.Create('Visible service field is missing: ' + ACaption);
  end;

  procedure Focus(const ACaption: TNyxText);
  begin
    LGrid := GridFor(ACaption, LColumn);
    LGrid.LeftCol := LColumn;
    LGrid.Col := LColumn;
    LGrid.Row := 1;
    LGrid.SetFocus;
    DrainActions;
  end;

  procedure Context(const ACaption: TNyxText; AFocus: Boolean = True);
  var
    LBounds: Types.TRect;
    LPoint: Types.TPoint;
  begin

    if AFocus then
    begin
      Focus(ACaption);
    end;
    LGrid := GridFor(ACaption, LColumn);
    LBounds := LGrid.CellRect(LColumn, 1);
    Check((LBounds.Right > LBounds.Left) and (LBounds.Bottom > LBounds.Top) and
      PtInRect(LGrid.ClientRect, Types.Point(LBounds.Left + 4, LBounds.Top + 4)),
      'context point is inside the actual visible cell: ' + ACaption);
    LPoint := LGrid.ClientToScreen(Types.Point(LBounds.Left + 4, LBounds.Top + 4));
    SendMessage(LGrid.Handle, WM_CONTEXTMENU, LGrid.Handle, MakeLParam(LPoint.X, LPoint.Y));
    DrainActions;
    Check((LActions.Menu <> nil) and LActions.Menu.IsOpen,
      'real context input opens the managed cell menu: ' + ACaption +
      ', editor=' + TNyxText(BoolToStr(LGrid.EditorMode, True)));
  end;

  procedure Menu(const AID: TNyxText);
  var
    LMenu: INyxLCLMenu;
    LButton: TButton;
  begin
    Check(Supports(LActions.Menu, INyxLCLMenu, LMenu), 'menu uses its native Nyx presentation');
    LMenu.Presentation.Renderer.Reveal(AID, niRuntime);
    LButton := TButton(LMenu.Presentation.Renderer.FocusFor(AID));
    Check((LButton <> nil) and LButton.Enabled, 'enabled cell action ' + AID);
    LButton.Click;
    DrainActions;
  end;

  procedure Inspector(const AID: TNyxText);
  var
    LButton: TButton;
  begin
    Check(Supports(LActions.Inspector, INyxLCLPopover, LInspector) and LInspector.IsOpen,
      'cell inspector uses its native Nyx presentation');
    LInspector.Renderer.Reveal(AID, niRuntime);
    LButton := TButton(LInspector.Renderer.FocusFor(AID));
    Check((LButton <> nil) and LButton.Enabled, 'enabled inspector action ' + AID);
    LButton.Click;
    DrainActions;
  end;

  function Status: TNyxText;
  begin
    Result := LActions.Inspector.State.GetValue(NyxTextState(NyxCellInspectorStatusState));
  end;

  procedure CloseInspection;
  begin
    LInspector := nil;

    if (LActions.Inspector <> nil) and LActions.Inspector.IsOpen then
    begin
      Inspector(NyxCellInspectorCloseID);
    end;
    LInspector := nil;
    LActions.Close;
  end;

begin
  Check(Supports(AMount, INyxCollectionCellActionsMount, LActionsMount) and
    Supports(AMount, INyxCollectionExportMount, LExports) and
    Supports(AMount, INyxCollectionClipboardMount, LClipboard),
    'public services supply the ordinary inspection/copy/export mounts');
  LActions := LActionsMount.CellActions;
  Check(Supports(LActions, INyxCollectionCellExportActions, LExportActions),
    'cell actions retain typed export receipts');
  LRevision := AMount.View.Store.Snapshot.Revision;
  ColumnClick(AManager, NyxColumnIdentity(NyxTextField('command')), ncaaShow);
  AManager.Presenter.Close;
  Pump;
  Context('Command line');
  LExpected := AMount.View.Store.Snapshot.Item(NyxItem(NyxCollection(ServiceCollectionName),
    'gateway')).GetValue(NyxTextField('command'));
  Menu(NyxCellActionsInspectID);
  Check(Supports(LActions.Inspector, INyxLCLPopover, LInspector),
    'long-value inspector is a real native Nyx popover');
  Check(TCustomEdit(LInspector.Renderer.FocusFor(NyxCellInspectorValueID)).ReadOnly and
    (NyxLCLInputText(TCustomEdit(LInspector.Renderer.FocusFor(NyxCellInspectorValueID))) = LExpected),
    'readonly inspection exposes the complete long command line');
  Inspector(NyxCellInspectorCopyID);
  Check((ACopy.Calls = 1) and (ACopy.Text = LExpected) and
    (LActions.LastResult.Outcome = ncwoWritten), 'Copy passes the exact accepted raw value once');
  ACopy.Outcome := ncwoUnavailable;
  Inspector(NyxCellInspectorCopyID);
  Check((LActions.LastResult.Outcome = ncwoUnavailable) and (Status <> ''),
    'unavailable clipboard produces a visible typed outcome');
  ACopy.Outcome := ncwoWritten;
  CloseInspection;

  Focus('Service');
  SendMessage(LGrid.Handle, WM_KEYDOWN, VK_F2, 1);
  Pump;
  Check(LGrid.EditorMode, 'public pinned Service cell enters its ordinary editor');
  RestoreNyxLCLInputText(TCustomEdit(LGrid.Editor), 'Unfinished service name');
  { Default-off editor context deliberately keeps the platform text menu.
    Inspect a different readonly cell without focusing it or accepting the draft. }
  Context('Command line', False);
  Menu(NyxCellActionsInspectID);
  Inspector(NyxCellInspectorCopyID);
  Check((ACopy.Text = LExpected) and
    (AMount.View.Store.Snapshot.Revision = LRevision),
    'inspection and accepted-value Copy never commit the unfinished editor');
  CloseInspection;
  Focus('Service');
  SendMessage(LGrid.Handle, WM_KEYDOWN, VK_F2, 1);
  Pump;
  Check(LGrid.EditorMode and
    (NyxLCLInputText(TCustomEdit(LGrid.Editor)) = 'Unfinished service name'),
    'the exact dormant draft resumes after inspection');
  SendMessage(LGrid.Handle, WM_KEYDOWN, VK_ESCAPE, 1);
  Pump;

  AManager.Open;
  Pump;
  ColumnClick(AManager, NyxColumnIdentity(NyxTextField('command')), ncaaHide);
  AManager.Presenter.Close;
  Check(Supports(AMount.View, INyxCollectionPages, LPages), 'public view exposes optional runtime paging');
  LPages.ConfigurePaging(NyxCollectionPaging.PageSize(1));
  Pump;
  Check((AMount.View.Snapshot.Count = 1) and (AMount.View.Snapshot.ItemAt(0).Ref.ID = 'gateway'),
    'page membership is deliberately smaller than the filtered result');
  Focus('Process ID');
  AMount.View.SetSelection([NyxItem(NyxCollection(ServiceCollectionName), 'reports')],
    NyxItem(NyxCollection(ServiceCollectionName), 'gateway'),
    NyxItem(NyxCollection(ServiceCollectionName), 'reports'));
  Pump;
  Context('Process ID', False);
  Menu(NyxCellActionsCopySourceRowsID);
  Check((LClipboard.ClipboardCommands.LastPayload.RowCount = 1) and
    (LClipboard.ClipboardCommands.LastPayload.ItemAt(0).ID = 'reports') and
    (ACopy.Text = LClipboard.ClipboardCommands.LastPayload.Text),
    'selected-source Copy includes the selected record outside the current page');
  CloseInspection;
  Context('Process ID');
  Menu(NyxCellActionsExportPageID);
  LPayload := LExports.ExportCommands.LastPayload;
  Check((ASave.Calls = 1) and (LPayload.RowCount = 1) and
    (LPayload.Membership = ncerCurrentPage) and (LPayload.ItemAt(0).ID = 'gateway') and
    (LPayload.FieldCount = 4) and (NyxDecodeUTF8(ASave.Bytes) = LPayload.Text),
    'current-page export captures exact raw rows and visible fields in saved order');
  for LIndex := 0 to LPayload.FieldCount - 1 do
  begin
    Check(LPayload.FieldAt(LIndex).Name <> 'command', 'hidden Command line is excluded explicitly');
  end;
  Check(Pos('current page', Status) > 0, 'save status identifies its captured current-page scope');
  CloseInspection;
  Context('Process ID');
  Menu(NyxCellActionsExportFilteredID);
  LPayload := LExports.ExportCommands.LastPayload;
  Check((ASave.Calls = 2) and (LPayload.RowCount = 2) and
    (LPayload.Membership = ncerAllFiltered) and (LPayload.ItemAt(1).ID = 'reports') and
    (NyxDecodeUTF8(ASave.Bytes) = LPayload.Text),
    'all-filtered export includes other pages without exporting unfiltered source rows');
  Check(Pos('all filtered rows', Status) > 0, 'save status identifies its captured all-filtered scope');
  CloseInspection;
  ASave.Outcome := nfsoCancelled;
  Context('Process ID');
  Menu(NyxCellActionsExportFilteredID);
  Check((LExportActions.LastExportResult.Outcome = nfsoCancelled) and
    (Pos('all filtered rows', Status) > 0), 'cancelled export keeps its scope and reports no write');
  CloseInspection;
  ASave.Outcome := nfsoUnavailable;
  Context('Process ID');
  Menu(NyxCellActionsExportPageID);
  Check((LExportActions.LastExportResult.Outcome = nfsoUnavailable) and
    (Pos('current page', Status) > 0), 'unavailable save keeps its exact requested scope visible');
  CloseInspection;
  ASave.Outcome := nfsoDownloadRequested;
  Context('Process ID');
  Menu(NyxCellActionsExportFilteredID);
  Check((LExportActions.LastExportResult.Outcome = nfsoDownloadRequested) and
    (Pos('all filtered rows', Status) > 0) and (Pos('download was requested', Status) > 0),
    'download request names the captured scope without claiming a disk write');
  CloseInspection;
  ASave.Outcome := nfsoActivationRequired;
  Context('Process ID');
  Menu(NyxCellActionsExportPageID);
  Check((LExportActions.LastExportResult.Outcome = nfsoActivationRequired) and
    (Pos('current page', Status) > 0), 'activation refusal keeps the requested scope visible');
  CloseInspection;
  ASave.Outcome := nfsoFailed;
  Context('Process ID');
  Menu(NyxCellActionsExportFilteredID);
  Check((LExportActions.LastExportResult.Outcome = nfsoFailed) and
    (Pos('all filtered rows', Status) > 0), 'failed export names the admitted filtered scope');
  CloseInspection;
  Check(AMount.View.Store.Snapshot.Revision = LRevision,
    'the complete inspection/copy/export journey never mutates service data');
end;

procedure ApplicationJourney(const APair: TNyxProjectPair);
var
  LDocument: TNyxDocument;
  LApplication: TNyxLCLApplication;
  LPeer: TNyxLCLApplication;
  LMount: INyxCollectionMount;
  LColumns: INyxCollectionColumnArrangementMount;
  LManager: INyxColumnArrangementManager;
  LBefore: TNyxText;
  LCopyHost: TCopyHost;
  LCopyLease: INyxClipboardWriteHost;
  LSaveHost: TSaveHost;
  LSaveLease: INyxFileSaveHost;
begin
  {$ifdef NYX_SERVICE_STUDIO_GENERATED}
  LDocument := BuildNyxDocument;
  {$else}
  LDocument := TNyxCodec.Decode(APair.Design);
  {$endif}
  LApplication := nil;
  LPeer := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    Check(LBefore = APair.Design, 'application constructor exactly matches its own saved design');
    Check((LDocument.Find(ServiceTableID).CollectionView.ColumnAt(0).Sizing.Value = 160) and
      not LDocument.Find(ServiceTableID).CollectionView.ColumnAt(3).LayoutPolicy.IsVisible and
      (LDocument.Find(ServiceTableID).CollectionView.ColumnAt(1).FieldName = 'notes') and
      (LDocument.Find(ServiceTableID).CollectionView.ColumnAt(2).FieldName = 'pid'),
      'application retains Studio-authored order, width and visibility');
    Check((LDocument.Find(ServiceTableID).CollectionView.ColumnAt(0).LayoutPolicy.Pin = ncpStart) and
      (LDocument.Find(ServiceTableID).CollectionView.ColumnAt(4).LayoutPolicy.Pin = ncpEnd),
      'Studio ordering retains both authored logical edge pins');
    LApplication := TNyxLCLApplication.Create;
    LCopyHost := TCopyHost.Create;
    LCopyLease := LCopyHost;
    LCopyHost.Outcome := ncwoWritten;
    LSaveHost := TSaveHost.Create;
    LSaveLease := LSaveHost;
    LSaveHost.Outcome := nfsoWritten;
    LApplication.View.TableWorkbenchServices := NewNyxTableWorkbenchServices(
      LCopyLease, LSaveLease, ServiceMonitorExportOptions);
    LApplication.Mount(LDocument);
    LApplication.Window.ClientWidth := GWidth;
    LApplication.Window.ClientHeight := 760;
    LApplication.Window.Show;
    LPeer := TNyxLCLApplication.Create;
    LPeer.Mount(LDocument);
    Pump;
    LMount := LApplication.View.CollectionMount(ServiceTableID);
    Check(Supports(LMount, INyxCollectionColumnArrangementMount, LColumns),
      'saved public table provides native Columns controls');
    LManager := LColumns.ColumnArrangement;
    LManager.Open;
    Pump;
    Check((LManager.Presenter <> nil) and LManager.Presenter.IsOpen,
      'ordinary Columns popup opens');
    Check(not LManager.Observation.ColumnAt(3).IsVisible,
      'hidden authored field remains discoverable for reveal');
    ColumnClick(LManager, NyxColumnIdentity(NyxTextField('command')), ncaaShow);
    Check(LManager.Observation.ColumnAt(3).IsVisible, 'Show reveals Command line');
    ColumnClick(LManager, NyxColumnIdentity(NyxIntegerField('pid')), ncaaMoveEarlier);
    Check(LManager.Observation.OrderedAt(1) = 2, 'Move earlier reorders Process ID by stable field');
    ColumnClick(LManager, NyxColumnIdentity(NyxTextField('notes')), ncaaHide);
    Check(not LManager.Observation.ColumnAt(1).IsVisible, 'Hide preserves the Notes field');
    ColumnClick(LManager, NyxColumnIdentity(NyxTextField('notes')), ncaaShow);
    Check(LManager.Observation.ColumnAt(1).IsVisible, 'Show restores hidden Notes');
    LMount.View.ConfigureQuery(NyxCollectionQuery.Where(
      NyxWhere(NyxTextField('status')).EqualTo('Running')));
    Pump;
    Check((LMount.View.Snapshot.Count = 2) and
      (LPeer.View.CollectionMount(ServiceTableID).View.Snapshot.Count = 4),
      'filtering works with arrangement while the independent peer remains unchanged');
    ColumnClick(LManager, Default(TNyxColumnIdentity), ncaaResetAll);
    Check(not LManager.Observation.ColumnAt(3).IsVisible and
      (LManager.Observation.Preferences.Count = 0) and
      (LManager.Observation.Preferences.OrderCount = 0),
      'Restore defaults returns to saved visibility/order rather than factory defaults');
    Check(LMount.View.Snapshot.Count = 2, 'arrangement reset preserves the active filter');
    DataActions(LApplication, LMount, LManager, LCopyHost, LSaveHost);
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'runtime commands never rewrite the saved design');
  finally
    LManager := nil;
    LColumns := nil;
    LMount := nil;
    LPeer.Free;
    LApplication.Free;
    LCopyLease := nil;
    LSaveLease := nil;
    LDocument.Free;
  end;
end;

var
  LPair: TNyxProjectPair;
begin
  GFailure := TFailureObserver.Create;
  try
    try

      if (ParamCount <> {$ifdef NYX_SERVICE_STUDIO_GENERATED}3{$else}2{$endif}) or
        not TryStrToInt(ParamStr(2), GWidth) or
        ((GWidth <> 390) and (GWidth <> 1280)) then
      begin
        raise Exception.Create('Expected NEW evidence path, width 1280/390 and optional own project');
      end;
      GEvidence := ExpandFileName(ParamStr(1));
      Check(not DirectoryExists(GEvidence), 'evidence directory is fresh');
      Check(ForceDirectories(GEvidence), 'evidence directory created');
      Application.Initialize;
      Application.OnException := GFailure.Failed;
      {$ifdef NYX_SERVICE_STUDIO_GENERATED}
      LPair := DecodeNyxProject(ReadNyxTextFile(ParamStr(3)).Text);
      {$else}
      LPair := StudioJourney;
      {$endif}
      ApplicationJourney(LPair);
      CheckSynchronize;
      WriteLn('PASS ', GChecks, ' actual service Studio/application checks at ', GWidth);
    except
      on E: Exception do
      begin
        WriteLn('FAIL ', E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    Application.OnException := nil;
    GFailure.Free;
  end;
end.
