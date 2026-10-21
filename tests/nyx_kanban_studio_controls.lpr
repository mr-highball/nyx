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

program nyx_kanban_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web,{$else}Interfaces, Classes, Forms, Controls, StdCtrls,
  LCLType, LMessages, nyx.files.lcl, nyx.studio.projectstore,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.composition, nyx.source, nyx.files, nyx.studio.files, nyx.responsive,
  nyx.collections, nyx.collections.selection, nyx.collections.view,
  nyx.collections.view.types, nyx.menu.editor, nyx.kanban, nyx.kanban.intent,
  nyx.kanban.editor, nyx.studio.inspector, nyx.studio.projects, nyx.studio.menu,
  {$ifdef PAS2JS}nyx.studio.browser, nyx.studio.agents, nyx.studio.exchange,
  nyx.test.editor.exchange, nyx.application.browser, nyx.render.browser
  {$else}nyx.studio.lcl, nyx.application.lcl, nyx.render.lcl{$endif}
  {$ifdef NYX_COMPILED_KANBAN_STUDIO}, nyx.generated.kanban.studio{$endif};

type
  { Only the file-delivery boundary is controlled. Ordinary Studio import/export
    still admits the exact bytes, source/design pair and project lifetime. }
  TProjectFiles = class(TInterfacedObject, INyxTextFileExchange)
  public
    Reply: TNyxTextFileReply;
    Exported: TNyxTextFiles;
    procedure Pick(const ASelection: TNyxTextFileSelection; AReply: TNyxTextFileReply);
    function ExportFiles(const AFiles: TNyxTextFiles): Boolean;
    procedure Cancel;
    procedure Deliver;
  end;
  {$ifdef PAS2JS}
  { Full browser Studio with the maintained asynchronous semantic exchange. }
  TWorkshopStudio = class(TNyxStudio)
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
    function CreateProjectFiles: INyxTextFileExchange; override;
  end;
  { The same consumer uses the real browser adapter and application host. }
  TApplicationHost = TNyxBrowserApplication;
  { Realized adapter lifetime is borrowed only while its app/Studio is alive. }
  TView = TNyxBrowserRenderer;
  {$else}
  { Full native Studio keeps its ordinary paired queue and on-disk project store. }
  TWorkshopStudio = class(TNyxNativeStudio)
  protected
    function CreateProjectFiles: INyxTextFileExchange; override;
  end;
  { Expose standard LCL click dispatch, without calling any Studio mutation. }
  TControlAccess = class(TControl);
  { Captured widget exceptions become failed assertions, not blocking dialogs. }
  TFailureObserver = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  { The actual native application is mounted without a blocking message loop. }
  TApplicationHost = TNyxLCLApplication;
  { Native and browser branches exercise the same semantic view owner. }
  TView = TNyxLCLRenderer;
  {$endif}
  { Every source-producing action settles before the next assertion/input.
    A stage is an ordinary controller turn, never a direct document rewrite. }
  TStage = (ksSelectDefinition, ksConfigureDefinition, ksDefinitionUndo,
    ksDefinitionRedo, ksSelectInstance, ksConfigureInstance, ksInstanceUndo,
    ksApplyInstance, ksInstanceRedo, ksOpenPreview, ksPreview, ksPreviewStart,
    ksRestoreInspector, ksDisable,
    ksInherit, ksCheckInheritedUndo, ksInheritedRedo, ksRestoreLocal,
    ksProjectPanel, ksOpenFiles, ksOpenFileBranch, ksOpenFileCommand,
    ksSave,
    {$ifdef PAS2JS}ksUndoSaved,{$endif}
    ksOpen, ksReopened);

var
  GStudio: TWorkshopStudio;
  GFiles: TProjectFiles;
  GFileLease: INyxTextFileExchange;
  GStage: TStage;
  GChecks: Integer;
  GFinished: Boolean;
  GBefore: TNyxText;
  GDefinitionPair: TNyxText;
  GInstancePair: TNyxText;
  GDisabledPair: TNyxText;
  GInheritedPair: TNyxText;
  GSaved: TNyxText;
  GDefinition: TNyxText;
  GOther: TNyxText;
  {$ifdef PAS2JS}
  GCore: TNyxAgentSession;
  GExchange: TNyxTestEditorExchange;
  GStarted: Double;
  {$else}
  GForm: TForm;
  GObserver: TFailureObserver;
  GDirectory: TNyxText;
  GWidth: Integer;
  {$endif}

function Cards(AAlternate: Boolean): TNyxCollectionRef;
begin
  Result := NyxCollection('cards');

  if AAlternate then
  begin
    Result := NyxCollection('requests');
  end;
end;

function TitleField(AAlternate: Boolean): TNyxTextFieldRef;
begin
  Result := NyxTextField('title');

  if AAlternate then
  begin
    Result := NyxTextField('name');
  end;
end;

function LaneField(AAlternate: Boolean): TNyxTextFieldRef;
begin
  Result := NyxTextField('lane');

  if AAlternate then
  begin
    Result := NyxTextField('stage');
  end;
end;

function Lane(AAlternate: Boolean; AIndex: Integer): TNyxKanbanLaneRef;
const
  CPrimary: array[0..2] of TNyxText = ('plan', 'build', 'complete');
  CAlternate: array[0..2] of TNyxText = ('queue', 'active', 'ready');
begin
  Result := NyxKanbanLane(CPrimary[AIndex]);

  if AAlternate then
  begin
    Result := NyxKanbanLane(CAlternate[AIndex]);
  end;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if (GObserver <> nil) and (GObserver.Error <> '') then
  begin
    raise Exception.Create('Kanban native callback: ' + GObserver.Error);
  end;
  {$endif}

  if not AValue then
  begin
    raise Exception.Create('Kanban Studio stage ' + IntToStr(Ord(GStage)) + ': ' + AReason);
  end;
  Inc(GChecks);
end;

function Fixture: TNyxDocument;
var
  LBoard: INyxKanbanBoard;
  LPage: INyxPage;
  LFirst: INyxComponent;
  LSecond: INyxComponent;
  LAlternate: Boolean;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'A thoughtful workflow';
    for LAlternate := False to True do
    begin
      Result.Collections.Define(Cards(LAlternate),
        NyxCollectionSchema.Text(TitleField(LAlternate), '').
          Text(LaneField(LAlternate), Lane(LAlternate, 0).Name), [
        NyxCollectionItem(NyxItem(Cards(LAlternate), 'first')).
          WithValue(TitleField(LAlternate), 'Make a welcoming place').
          WithValue(LaneField(LAlternate), Lane(LAlternate, 0).Name)]);
    end;
    LBoard := NewNyxKanbanBoard('workflow');
    LBoard.Configure.WhenViewport(TNyxViewportWidth.Below(700)).Layout(nlColumn).Done;
    LBoard.Lane0ItemsList.Configure.Height(96).Done;
    LBoard.Lane1ItemsList.Configure.Height(96).Done;
    LBoard.Lane2ItemsList.Configure.Height(96).Done;
    Result.AddComponent(LBoard);
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(16).Gap(16).Done;
    LPage.Add(NewNyxHeading('welcome').WithText('Make room for a good idea'));
    LFirst := NewNyxComponent('first-board');
    LFirst.Reference := NyxComponent('workflow');
    LFirst.Node.OverridePart(NyxPart('lane-0/title')).Named('private-title').
      SetProp(NyxAttributeName(atText), 'Private ideas');
    LFirst.Node.OverridePart(NyxPart('lane-1'), noAppend).Named('private-details').
      Add(NewNyxLabel('private-note').Configure.PartName(NyxPart('note')).
        Text('This detail stays yours.').Done.Node);
    LPage.Add(LFirst);
    LSecond := NewNyxComponent('second-board');
    LSecond.Reference := NyxComponent('workflow');
    LPage.Add(LSecond);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
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
  Check(Assigned(LReply), 'ordinary Open requested the copied project bytes');
  LReply(fpsSelected, Exported, '');
end;

function TWorkshopStudio.CreateProjectFiles: INyxTextFileExchange;
begin
  Result := GFileLease;
end;

{$ifdef PAS2JS}
function TWorkshopStudio.CreateEditorExchange: TNyxStudioEditorExchange;
begin
  GExchange := TNyxTestEditorExchange.Create(GCore);
  Result := GExchange;
end;
{$else}
procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

function Pair: TNyxText;
begin
  Result := EncodeNyxProject(GStudio.ProjectSnapshot);
end;

function SnapshotNode(const AID: TNyxText): TNyxText;
var
  LDocument: TNyxDocument;
begin
  LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
  try
    Result := TNyxCodec.NodeSnapshot(LDocument.Find(AID)).ToJSON;
  finally
    LDocument.Free;
  end;
end;

procedure SelectOwner(const AID: TNyxText; ADefinition: Boolean);
begin
  {$ifdef PAS2JS}

  if not ADefinition then
  begin
    GCore.Call('nyx_select', 'Kanban workshop', NyxObject([
      NyxField('expectedRevision', NyxData(GCore.Revision)),
      NyxField('operationId', NyxData('kanban-activate-home')),
      NyxField('id', NyxData('home')), NyxField('activate', NyxData(True))]),
      'kanban-workshop');
  end;
  GCore.Call('nyx_select', 'Kanban workshop', NyxObject([
    NyxField('expectedRevision', NyxData(GCore.Revision)),
    NyxField('operationId', NyxData('kanban-select-' + AID)),
    NyxField('id', NyxData(AID)), NyxField('activate', NyxData(ADefinition))]), 'kanban-workshop');
  {$else}

  if ADefinition then
  begin
    GStudio.Session.Activate('workflow');
  end
  else
  begin
    GStudio.Session.Activate('home');
  end;
  GStudio.Session.Select(AID);
  GStudio.RequestRefresh(True);
  {$endif}
end;

procedure Click(const AID: TNyxText);
{$ifdef PAS2JS}
var
  LFace: TJSHTMLElement;
begin
  LFace := GStudio.ShellView.ElementFor(AID);
  Check(LFace <> nil, 'ordinary browser action is mounted: ' + AID);
  GStudio.ShellView.ViewFor(AID).Reveal(AID, niRuntime);
  LFace.click;
end;
{$else}
var
  LFace: TControl;
begin
  LFace := GStudio.ShellView.ControlFor(AID);
  Check(LFace <> nil, 'ordinary native action is mounted: ' + AID);
  GStudio.ShellView.ViewFor(AID).Reveal(AID, niRuntime);
  Check(LFace.IsVisible and LFace.Enabled, 'ordinary action is visible and enabled: ' + AID);
  TControlAccess(LFace).Click;
end;
{$endif}

{$ifdef PAS2JS}
{ Popovers have their own renderer; resolve their actual mounted DOM face
  independently of the Studio section views. }
procedure ClickPopup(const AID: TNyxText);
var
  LFace: TJSHTMLElement;
begin
  LFace := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
  Check((LFace <> nil) and (LFace.getBoundingClientRect.width > 0),
    'ordinary browser popup is mounted and visible: ' + AID);
  LFace.click;
end;
{$else}
{ The compact workspace exposes project commands through ordinary Nyx menus.
  Borrow the visible popup button, just as a user does, without invoking the
  controller target of a deliberately hidden header face. }
function MenuButton(AParent: TWinControl; const ACaption: String): TControl;
var
  LIndex: Integer;
  LChild: TControl;
begin
  Result := nil;
  for LIndex := 0 to AParent.ControlCount - 1 do
  begin
    LChild := AParent.Controls[LIndex];

    if LChild.IsVisible and
      ((TControlAccess(LChild).Caption = ACaption) or
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

procedure ClickMenu(const AWindow, ACaption: String);
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
  Check((LButton <> nil) and LButton.Enabled, 'ordinary popup action is available: ' + ACaption);
  TControlAccess(LButton).Click;
end;
{$endif}

procedure Change(const AID, AValue: TNyxText);
{$ifdef PAS2JS}
var
  LInput: TJSHTMLElement;
begin
  LInput := GStudio.ShellView.InputFor(AID);
  Check(LInput <> nil, 'browser form field is mounted');

  if LInput.tagName = 'SELECT' then
  begin
    TJSHTMLSelectElement(LInput).value := AValue;
  end
  else
  begin
    TJSHTMLInputElement(LInput).value := AValue;
  end;
  LInput.dispatchEvent(TJSEvent.new('change'));
end;
{$else}
var
  LInput: TControl;
begin
  LInput := GStudio.ShellView.InputFor(AID);
  Check(LInput <> nil, 'native form field is mounted');

  if LInput is TComboBox then
  begin
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(String(AValue));
    Check(TComboBox(LInput).ItemIndex >= 0, 'typed choice is present in the actual dropdown');
    TComboBox(LInput).OnChange(LInput);
  end
  else
  begin
    TEdit(LInput).Text := String(AValue);
    TEdit(LInput).OnChange(LInput);
  end;
end;
{$endif}

procedure ConfigureForm(AAlternate: Boolean);
var
  LIndex: Integer;
  LOffset: Integer;
begin
  LOffset := 0;

  if AAlternate then
  begin
    LOffset := 1;
  end;
  Change(NyxKanbanEditorFieldID(NyxStudioKanbanEditorID, nkfCollection),
    NyxMenuEditorChoiceCaption(LOffset, Cards(AAlternate).Name));
  Change(NyxKanbanEditorFieldID(NyxStudioKanbanEditorID, nkfTitleField),
    NyxMenuEditorChoiceCaption(LOffset * 2,
      Cards(AAlternate).Name + ' / ' + TitleField(AAlternate).Name));
  Change(NyxKanbanEditorFieldID(NyxStudioKanbanEditorID, nkfLaneField),
    NyxMenuEditorChoiceCaption(LOffset * 2 + 1,
      Cards(AAlternate).Name + ' / ' + LaneField(AAlternate).Name));
  for LIndex := 0 to 2 do
  begin
    Change(NyxKanbanEditorFieldID(NyxStudioKanbanEditorID,
      TNyxKanbanEditorField(Ord(nkfLane0) + LIndex * 2)), Lane(AAlternate, LIndex).Name);
    Change(NyxKanbanEditorFieldID(NyxStudioKanbanEditorID,
      TNyxKanbanEditorField(Ord(nkfCaption0) + LIndex * 2)),
      'Stage ' + TNyxText(IntToStr(LIndex + 1)));
  end;
  Click(NyxKanbanEditorFieldID(NyxStudioKanbanEditorID, nkfApply));
end;

procedure ClickField(AField: TNyxKanbanEditorField);
begin
  Click(NyxKanbanEditorFieldID(NyxStudioKanbanEditorID, AField));
end;

procedure InspectorPanel;
begin

  if GStudio.ShellView.RootFor('action-panel-inspector') <> nil then
  begin
    Click('action-panel-inspector');
  end;
end;

procedure DesignPanel;
begin

  if GStudio.ShellView.RootFor('action-panel-design') <> nil then
  begin
    Click('action-panel-design');
  end;
end;

procedure MoveCard(AView: TView; AAlternate: Boolean; const ABoardID: TNyxText);
var
  LBinding: INyxKanbanBinding;
  LView: INyxCollectionView;
  LItem: TNyxItemRef;
  LRevision: Integer;
  LIndex: Integer;
  LID: TNyxText;
  {$ifdef PAS2JS}
  LRow: TJSHTMLElement;
  {$else}
  LList: TListBox;
  LButton: TWinControl;
  {$endif}
begin
  LBinding := AView.KanbanBinding(ABoardID);
  Check((LBinding <> nil) and LBinding.Connected, 'accepted host attaches Kanban automatically');
  LView := AView.CollectionView(LBinding.Items(Lane(AAlternate, 0)).ID);
  LItem := NyxItem(Cards(AAlternate), 'first');
  LRevision := LView.Store.Snapshot.Revision;
  LIndex := LView.Snapshot.IndexOf(LItem);
  Check(LIndex >= 0, 'typed source card is visible in the mounted list');
  LID := LBinding.Items(Lane(AAlternate, 0)).ID;
  AView.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  LRow := TJSHTMLElement(AView.ElementFor(LID).querySelector('[data-nyx-item="first"]'));
  Check(LRow <> nil, 'actual browser row exists');
  LRow.focus;
  LRow.click;
  {$else}
  LList := TListBox(AView.ControlFor(LID));
  LList.SetFocus;
  LList.ItemIndex := LIndex;
  LList.OnSelectionChange(LList, True);
  {$endif}
  Check(LView.HasSelection and (LView.Selected.ID = LItem.ID),
    'ordinary input selects the stable typed card');
  LID := LBinding.MoveButton(Lane(AAlternate, 1)).ID;
  AView.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  Check(not TJSHTMLButtonElement(AView.ElementFor(LID)).disabled,
    'actual destination button is enabled');
  AView.ElementFor(LID).click;
  {$else}
  LButton := TWinControl(AView.ControlFor(LID));
  Check(LButton.Enabled, 'actual native destination button is enabled');
  LButton.SetFocus;

  if LButton.Perform(CN_KEYDOWN, VK_SPACE, 0) = 0 then
  begin
    LButton.Perform(LM_KEYDOWN, VK_SPACE, 0);
  end;

  if LButton.Perform(CN_KEYUP, VK_SPACE, 0) = 0 then
  begin
    LButton.Perform(LM_KEYUP, VK_SPACE, 0);
  end;
  CheckSynchronize;
  Application.ProcessMessages;
  {$endif}
  Check((LView.Store.Snapshot.Revision = LRevision + 1) and
    (LView.Store.Snapshot.Item(LItem).GetValue(LaneField(AAlternate)) =
      Lane(AAlternate, 1).Name), 'host input commits exactly one typed lane move');
end;

procedure CheckInherited;
var
  LDocument: TNyxDocument;
  LContext: TNyxNode;
  LBoard: TNyxNode;
  LIndex: Integer;
begin
  LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
  LContext := nil;
  try
    Check(not HasNyxKanbanIntent(LDocument.Find('first-board')),
      'Inherit removes the local whole intent');
    LContext := RealizeNyxContext(LDocument, LDocument.Find('first-board'), LBoard);
    Check(GetNyxKanbanIntent(LBoard).Options.Collection.Name = Cards(False).Name,
      'Inherit reveals the reusable definition configuration');
    for LIndex := 0 to 2 do
    begin
      Check(LBoard.Part(NyxPart('lane-' + IntToStr(LIndex) + '/items')).
        CollectionView.Key.Name = Cards(False).Name,
        'all three formerly local lane bindings reconcile together');
    end;
    Check(LBoard.Part(NyxPart('lane-0/title')).Prop(NyxAttributeName(atText)) = 'Private ideas',
      'configuration preserves the independently customized heading');
    Check(LBoard.Part(NyxPart('lane-1/note')).Prop(NyxAttributeName(atText)) =
      'This detail stays yours.', 'configuration preserves appended custom content');
  finally
    LContext.Free;
    LDocument.Free;
  end;
end;

procedure Stage;
{$ifndef PAS2JS}
var
  LStore: TNyxProjectStore;
  LRevision: TNyxText;
{$endif}
begin
  case GStage of
    ksSelectDefinition: SelectOwner('workflow', True);
    ksConfigureDefinition:
      begin
        InspectorPanel;
      end;
    ksDefinitionUndo:
      begin
        GBefore := Pair;
        ConfigureForm(False);
      end;
    ksDefinitionRedo:
      begin
        Check(Pair <> GBefore, 'ordinary form publishes a whole typed definition configuration');
        GDefinitionPair := Pair;
        GDefinition := SnapshotNode('workflow');
        GOther := SnapshotNode('second-board');
        Click('action-undo');
      end;
    ksSelectInstance:
      begin
        Check(Pair = GBefore, 'one Undo restores the complete preconfiguration source/design pair');
        Click('action-redo');
      end;
    ksConfigureInstance:
      begin
        Check(Pair = GDefinitionPair, 'one Redo restores all three roles and generated Pascal');
        SelectOwner('first-board', False);
      end;
    ksInstanceUndo: InspectorPanel;
    ksApplyInstance: ConfigureForm(True);
    ksInstanceRedo:
      begin
        Check((SnapshotNode('workflow') = GDefinition) and
          (SnapshotNode('second-board') = GOther), 'local configuration preserves definition and sibling');
        GInstancePair := Pair;
        Check(GInstancePair <> GDefinitionPair, 'instance form publishes its own collection and fields');
        Click('action-undo');
      end;
    ksOpenPreview:
      begin
        Check(Pair = GDefinitionPair, 'one Undo restores all instance role overrides');
        Click('action-redo');
      end;
    ksPreview:
      begin
        Check(Pair = GInstancePair, 'instance Redo restores the exact complete pair');
        DesignPanel;
      end;
    ksPreviewStart: Click('action-preview');
    ksRestoreInspector:
      begin
        MoveCard(GStudio.CanvasView, True, NyxQualifiedID('first-board', 'workflow'));
        Check(Pair = GInstancePair, 'interacting with preview never rewrites authored default rows');
        Click('action-preview');
        InspectorPanel;
      end;
    ksDisable: ClickField(nkfDisable);
    ksInherit:
      begin
        GDisabledPair := Pair;
        Check(GDisabledPair <> GInstancePair, 'Disable is one saved explicit local mask');
        ClickField(nkfInherit);
      end;
    ksCheckInheritedUndo:
      begin
        CheckInherited;
        GInheritedPair := Pair;
        Click('action-undo');
      end;
    ksInheritedRedo:
      begin
        Check(Pair = GDisabledPair, 'one Undo restores disabled intent with its previous bindings');
        Click('action-redo');
      end;
    ksRestoreLocal:
      begin
        Check(Pair = GInheritedPair, 'one Redo restores inherited intent and reconciled roles');
        ConfigureForm(True);
      end;
    ksProjectPanel:
      begin
        Check(Pair = GInstancePair, 'reconfiguration reuses exact existing descriptors and Pascal');

        if GStudio.ShellView.RootFor('action-panel-project') <> nil then
        begin
          Click('action-panel-project');
        end;
      end;
    ksOpenFiles: Click(NyxStudioActionMenuID);
    ksOpenFileBranch:
      begin
        {$ifdef PAS2JS}
        ClickPopup('studio-menu-project');
        {$else}
        ClickMenu('Component actions', 'Project');
        {$endif}
      end;
    ksOpenFileCommand:
      begin
        {$ifdef PAS2JS}
        ClickPopup('studio-menu-open');
        {$else}
        ClickMenu('Project', 'Open project');
        {$endif}
      end;
    ksSave:
      begin
        GSaved := Pair;
        Check(Pos('SetNyxKanbanIntent', GStudio.ProjectSnapshot.Source) > 0,
          'ordinary source generation retains crafted typed automatic intent');
        {$ifdef PAS2JS}
        Click('action-project-export');
        {$else}
        Change('project-file-name', 'kanban-workshop');
        Click('action-project-save');
        {$endif}
      end;
    {$ifdef PAS2JS}
    ksUndoSaved: Click('action-undo');
    {$endif}
    ksOpen:
      begin
        {$ifdef PAS2JS}
        Check(Pair <> GSaved,
          'ordinary Undo changes the current project before testing browser Import');
        Check((Length(GFiles.Exported) = 1) and (GFiles.Exported[0].Text = GSaved),
          'ordinary backup saves the complete source/design packet');
        Click('action-project-import');
        GFiles.Deliver;
        {$else}
        LStore := TNyxProjectStore.Create(GDirectory);
        try
          Check(LStore.ReadProject('kanban-workshop', LRevision) = GSaved,
            'ordinary Save writes the exact accepted project pair to the isolated directory');
        finally
          LStore.Free;
        end;
        Click('action-project-open');
        {$endif}
      end;
    ksReopened:
      begin
        Check(Pair = GSaved, 'ordinary Open restores the exact configured design and Pascal');
        {$ifndef PAS2JS}
        Check(not GStudio.Session.CanUndo and not GStudio.Session.CanRedo,
          'successful Open begins deliberate fresh project history');
        WriteNyxTextFile(GDirectory + 'accepted-project.nyxproject',
          NyxTextFile('accepted-project.nyxproject', GSaved));
        WriteNyxTextFile(GDirectory + 'nyx.generated.kanban.studio.pas',
          NyxTextFile('nyx.generated.kanban.studio.pas', GStudio.ProjectSnapshot.Source));
        {$endif}
        GFinished := True;
      end;
  end;

  if not GFinished then
  begin
    GStage := Succ(GStage);
  end;
end;

procedure ApplicationProof(ADocument: TNyxDocument);
var
  LApplication: TApplicationHost;
  LFirst: INyxKanbanBinding;
  LSecond: INyxKanbanBinding;
  LView: INyxCollectionView;
  LBefore: TNyxText;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  {$endif}
begin
  LBefore := TNyxCodec.Encode(ADocument);
  LApplication := TApplicationHost.Create;
  try
    {$ifdef PAS2JS}
    LHost := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(LHost);
    LApplication.Run(ADocument, LHost);
    {$else}
    LApplication.Mount(ADocument);
    LApplication.Window.ClientWidth := GWidth;
    LApplication.Window.ClientHeight := 900;
    LApplication.Window.Show;
    Application.ProcessMessages;
    {$endif}
    LFirst := LApplication.View.KanbanBinding(NyxQualifiedID('first-board', 'workflow'));
    LSecond := LApplication.View.KanbanBinding(NyxQualifiedID('second-board', 'workflow'));
    Check((LFirst <> nil) and (LSecond <> nil), 'ordinary application automatically mounts both boards');
    LView := LApplication.View.CollectionView(LSecond.Items(Lane(False, 0)).ID);
    MoveCard(LApplication.View, True, NyxQualifiedID('first-board', 'workflow'));
    Check(LView.Snapshot.Has(NyxItem(Cards(False), 'first')),
      'first application board movement leaves the independent sibling source visible');
    MoveCard(LApplication.View, False, NyxQualifiedID('second-board', 'workflow'));
    Check(TNyxCodec.Encode(ADocument) = LBefore, 'application runtime never changes saved design defaults');
  finally
    LApplication.Free;
  end;
  Check(not LFirst.Connected and not LSecond.Connected,
    'application disposal revokes retained board references before document retirement');
end;

procedure Finish;
var
  LPair: TNyxProjectPair;
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
begin
  LPair := DecodeNyxProject(GSaved);
  FreeAndNil(GStudio);
  GFileLease := nil;
  GFiles := nil;
  {$ifdef PAS2JS}
  GExchange := nil;
  FreeAndNil(GCore);
  {$endif}
  LDocument := nil;
  LWorkspace := nil;
  AdmitNyxProject(LPair, nprRequireMatch, LDocument, LWorkspace, LResolved);
  try
    Check(EncodeNyxProject(LResolved) = GSaved,
      'independent complete source/design admission preserves the saved project exactly');
    ApplicationProof(LDocument);
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if GExchange.RequestPending then
    begin
      GExchange.Deliver;
    end
    else if GExchange.TickPending then
    begin
      GExchange.FireTick;
    end;

    if TJSDate.now - GStarted > 180000 then
    begin
      raise Exception.Create('Kanban Studio timed out');
    end;

    if GStudio.SourceBusy or GStudio.PresentationPending or not GStudio.SharedSourceSynchronized then
    begin
      window.setTimeout(@Drive, 20);
      Exit;
    end;
    Stage;

    if GFinished then
    begin
      Finish;
      document.body.setAttribute('data-kanban-studio', 'passed');
      document.body.setAttribute('data-kanban-checks', IntToStr(GChecks));
    end
    else
    begin
      window.setTimeout(@Drive, 20);
    end;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-kanban-studio', 'failed');
      document.body.setAttribute('data-kanban-error', E.Message);
      FreeAndNil(GStudio);
      GExchange := nil;
      FreeAndNil(GCore);
      GFileLease := nil;
      GFiles := nil;
    end;
  end;
end;
{$else}
procedure AwaitChanges;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    CheckSynchronize;
    Application.ProcessMessages;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Kanban Studio did not settle at stage ' + IntToStr(Ord(GStage)));
    end;

    if GStudio.SourceBusy or GStudio.PresentationPending then
    begin
      Sleep(1);
    end;
  until not GStudio.SourceBusy and not GStudio.PresentationPending;
end;
{$endif}

var
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
begin
  LDocument := nil;
  try
    {$ifndef PAS2JS}

    if ParamCount < 2 then
    begin
      raise Exception.Create('Supply a fresh isolated evidence directory and client width');
    end;
    GDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
    GWidth := StrToInt(ParamStr(2));
    ForceDirectories(GDirectory);
    Application.Initialize;
    GObserver := TFailureObserver.Create;
    Application.OnException := GObserver.Failed;
    {$endif}
    {$ifdef NYX_COMPILED_KANBAN_STUDIO}
    {$ifdef PAS2JS}
    GSaved := document.getElementById('expected-project').textContent;
    {$else}

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Compiled companion needs its exact pinned accepted project packet');
    end;
    GSaved := ReadNyxTextFile(ParamStr(3)).Text;
    {$endif}
    LPair := DecodeNyxProject(GSaved);
    LDocument := BuildNyxDocument;
    Check(TNyxCodec.Encode(LDocument) = LPair.Design,
      'compiled designer output reconstructs the exact saved Studio design');
    ApplicationProof(LDocument);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-kanban-studio', 'passed');
    document.body.setAttribute('data-kanban-checks', IntToStr(GChecks));
    {$endif}
    {$else}
    LDocument := Fixture;
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument, 'nyx.generated.kanban.studio'));
    FreeAndNil(LDocument);
    GFiles := TProjectFiles.Create;
    GFileLease := GFiles;
    {$ifdef PAS2JS}
    GStarted := TJSDate.now;
    GCore := TNyxAgentSession.Create(LPair);
    GStudio := TWorkshopStudio.Create;
    GStudio.Run(False);
    GStudio.ConnectAgents;
    Drive;
    {$else}
    GForm := TForm.CreateNew(nil);
    GForm.ClientWidth := GWidth;
    GForm.ClientHeight := 900;
    GForm.Show;
    GStudio := TWorkshopStudio.Create(GForm, GDirectory);
    GStudio.LoadProject(LPair);
    GStudio.Run;
    while not GFinished do
    begin
      AwaitChanges;
      WriteLn('Kanban Studio stage ', Ord(GStage), ' / width=', GForm.ClientWidth);
      Flush(Output);
      Stage;
    end;
    Finish;
    {$endif}
    {$endif}
    {$ifndef PAS2JS}
    WriteLn('PASS ', GChecks, ' ordinary Kanban Studio/application checks / width=', GWidth);
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-kanban-studio', 'failed');
      document.body.setAttribute('data-kanban-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message, ' / checks=', GChecks);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}
  FreeAndNil(GStudio);
  FreeAndNil(GForm);
  Application.OnException := nil;
  FreeAndNil(GObserver);
  GFileLease := nil;
  {$endif}
  LDocument.Free;
end.
