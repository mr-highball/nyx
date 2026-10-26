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

program nyx_data_toolbar_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web,{$else}Interfaces, Classes, Forms, Controls, StdCtrls,
  LCLType, LMessages, nyx.files.lcl, nyx.studio.projectstore,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.composition, nyx.source, nyx.files, nyx.studio.files, nyx.responsive,
  nyx.collections, nyx.collections.selection, nyx.collections.view,
  nyx.collections.view.types, nyx.menu.editor, nyx.data.toolbar,
  nyx.data.toolbar.query, nyx.collections.query.controls,
  nyx.data.toolbar.editor, nyx.studio.inspector, nyx.studio.projects, nyx.studio.menu,
  {$ifdef PAS2JS}nyx.studio.browser, nyx.studio.agents, nyx.studio.exchange,
  nyx.test.editor.exchange, nyx.application.browser, nyx.render.browser, nyx.collections.query.browser
  {$else}nyx.studio.lcl, nyx.application.lcl, nyx.render.lcl, nyx.collections.query.lcl{$endif}
  {$ifdef NYX_COMPILED_TOOLBAR_STUDIO}, nyx.generated.toolbar.studio{$endif};

const
  CQueryCaption: TNyxText = 'People 🌙';
  CQueryButtonCaption: TNyxText = 'Inspect filters';
  { This is an ordinary control identity in the public workbench presentation,
    not an actor token or a command dispatched directly into its controller. }
  CFirstRemovalAction = NyxQueryWorkbenchEditorID + '-chip-0';

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
  { The exact popup exposes its own renderer, separate from the Studio shell. }
  TQueryPresenter = INyxBrowserQueryFormPresenter;
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
  { Borrow the exact native popup while its managed presenter remains retained. }
  TQueryPresenter = INyxLCLQueryFormPresenter;
  {$endif}
  { Every source-producing action settles before the next assertion/input.
    A stage is an ordinary controller turn, never a direct document rewrite. }
  TStage = (ksSelect, ksConfigure, ksUndo, ksRedo, ksDesignPanel, ksPreviewOpen,
    ksPreview, ksInspector, ksSelectNamed, ksConfigureNamed, ksCheckNamed,
    ksProjectPanel, ksOpenFiles, ksOpenFileBranch, ksOpenFileCommand,
    ksSave, {$ifdef PAS2JS}ksUndoSaved,{$endif} ksOpen, ksReopened);

var
  GStudio: TWorkshopStudio;
  GFiles: TProjectFiles;
  GFileLease: INyxTextFileExchange;
  GStage: TStage;
  GChecks: Integer;
  GFinished: Boolean;
  GBefore: TNyxText;
  GInstancePair: TNyxText;
  GSaved: TNyxText;
  GDefinition: TNyxText;
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



procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if (GObserver <> nil) and (GObserver.Error <> '') then
  begin
    raise Exception.Create('DataToolbar native callback: ' + GObserver.Error);
  end;
  {$endif}

  if not AValue then
  begin
    raise Exception.Create('DataToolbar Studio stage ' + IntToStr(Ord(GStage)) + ': ' + AReason);
  end;
  Inc(GChecks);
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LWorkspace: INyxCard;
  LToolbar: INyxDataToolbar;
  LTable: INyxTable;
  LInstance: INyxComponent;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'People worth finding';
    Result.Collections.Define(NyxCollection('people'), NyxCollectionSchema.
      Text(NyxTextField('name'), '').Integer(NyxIntegerField('rank'), 0), [
      NyxCollectionItem(NyxItem(NyxCollection('people'), 'ada')).WithValue(NyxTextField('name'), 'Ada'),
      NyxCollectionItem(NyxItem(NyxCollection('people'), 'grace')).WithValue(NyxTextField('name'), 'Grace')]);
    LPage := NewNyxPage('home');
    LToolbar := NewNyxDataToolbar('local-tools');
    LToolbar.CreateButton.WithText('Invite a person');
    LPage.Add(LToolbar);
    LTable := NewNyxTable('people');
    LTable.Binds.Collection(NyxCollectionView(NyxCollection('people')).
      Column(NyxTextField('name'), 'Name'));
    LPage.Add(LTable);
    LWorkspace := NewNyxCard('workspace');
    LToolbar := NewNyxDataToolbar('tools');
    LToolbar.Configure.PartName(NyxPart('tools')).Done;
    LWorkspace.Add(LToolbar);
    LTable := NewNyxTable('grid');
    LTable.Binds.Collection(NyxCollectionView(NyxCollection('people')).
      Column(NyxTextField('name'), 'Name').Scoped(csInstance));
    LWorkspace.Add(LTable);
    Result.AddComponent(LWorkspace);
    LInstance := NewNyxComponent('left');
    LInstance.Reference := NyxComponent('workspace');
    LInstance.Node.OverridePart(NyxPart('tools')).Named('left-tools-rule');
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('right');
    LInstance.Reference := NyxComponent('workspace');
    LPage.Add(LInstance);
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
    GCore.Call('nyx_select', 'DataToolbar workshop', NyxObject([
      NyxField('expectedRevision', NyxData(GCore.Revision)),
      NyxField('operationId', NyxData('toolbar-activate-home')),
      NyxField('id', NyxData('home')), NyxField('activate', NyxData(True))]),
      'toolbar-workshop');
  end;
  GCore.Call('nyx_select', 'DataToolbar workshop', NyxObject([
    NyxField('expectedRevision', NyxData(GCore.Revision)),
    NyxField('operationId', NyxData('toolbar-select-' + AID)),
    NyxField('id', NyxData(AID)), NyxField('activate', NyxData(ADefinition))]), 'toolbar-workshop');
  {$else}

  if ADefinition then
  begin
    GStudio.Session.Activate('home');
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

procedure IncludeChoice(const AID: TNyxText);
{$ifdef PAS2JS}
var
  LInput: TJSHTMLInputElement;
begin
  LInput := TJSHTMLInputElement(GStudio.ShellView.InputFor(AID));
  Check(LInput <> nil, 'ordinary include-choice checkbox is mounted');
  LInput.checked := True;
  LInput.dispatchEvent(TJSEvent.new('change'));
end;
{$else}
var
  LInput: TControl;
begin
  LInput := GStudio.ShellView.InputFor(AID);
  Check(LInput is TCheckBox, 'ordinary include-choice checkbox is mounted');
  TCheckBox(LInput).Checked := True;
end;
{$endif}

procedure ConfigureForm;
var
  LRow: TNyxText;
  LKey: TNyxText;
begin
  LRow := NyxDataToolbarEditorSortID(NyxStudioDataToolbarEditorID, 0);
  LKey := NyxDataToolbarEditorKeyID(LRow, 0);
  Change(NyxDataToolbarEditorFieldID(LRow, ntefCaption), 'Name descending');
  IncludeChoice(NyxDataToolbarEditorFieldID(LRow, ntefIncluded));
  Change(NyxDataToolbarEditorFieldID(LKey, ntefSortField), NyxMenuEditorChoiceCaption(1, 'name'));
  Change(NyxDataToolbarEditorFieldID(LKey, ntefDirection), NyxMenuEditorChoiceCaption(1, 'Descending'));
  Change(NyxDataToolbarEditorFieldID(NyxStudioDataToolbarEditorID, ntefQueryDisplay),
    NyxMenuEditorChoiceCaption(Ord(ntqdDetails), 'Results summary and filter details'));
  Change(NyxDataToolbarEditorFieldID(NyxStudioDataToolbarEditorID, ntefQueryCaption), CQueryCaption);
  Change(NyxDataToolbarEditorFieldID(NyxStudioDataToolbarEditorID, ntefQueryButton), CQueryButtonCaption);
  Click(NyxDataToolbarEditorActionID(NyxStudioDataToolbarEditorID, nteaConfigure));
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

{ Read the actual status face; the authored label is only a default. }
procedure CheckQueryStatus(AView: TView; const AToolbar: TNyxText;
  const AQuery: INyxDataToolbarQueryBinding);
var
  LID: TNyxText;
  LText: TNyxText;
begin
  LID := AView.Root.Find(AToolbar).Part(NyxPart(NyxDataToolbarQueryStatusPart)).ID;
  {$ifdef PAS2JS}
  LText := AView.ElementFor(LID).textContent;
  {$else}
  LText := TLabel(AView.ControlFor(LID)).Caption;
  {$endif}
  Check((LText = AQuery.QueryStatus) and (Pos(CQueryCaption + ': ', LText) = 1),
    'real status follows accepted query state and the exact caption configured in Studio');
end;

procedure SearchAndClear(AView: TView; const AToolbar, ATarget: TNyxText);
var
  LID: TNyxText;
  LSearchID: TNyxText;
  LRows: INyxCollectionView;
  LSource: INyxCollectionSnapshot;
  LSelected: TNyxItemRef;
  LQuery: INyxDataToolbarQueryBinding;
  LPresenter: INyxQueryFormPresenter;
  LTarget: TQueryPresenter;
  {$ifdef PAS2JS}
  LInput: TJSHTMLInputElement;
  {$else}
  LInput: TControl;
  {$endif}
begin
  LRows := AView.CollectionView(ATarget);
  Check((LRows <> nil) and (LRows.Snapshot.Count = 2), 'ordinary bound table starts with both people');
  LSource := LRows.Store.Snapshot;
  LSelected := NyxItem(LSource.Key, 'grace');
  { Seed selection through the public typed view command; this journey qualifies
    query controls, not a physical row-selection gesture. Filtering hides Grace
    but must never remove her accepted selection membership. }
  LRows.Select(LSelected);
  LID := AView.Root.Find(AToolbar).Part(NyxPart('search')).ID;
  LSearchID := LID;
  AView.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  LInput := TJSHTMLInputElement(AView.InputFor(LID));
  LInput.value := 'Ada';
  LInput.dispatchEvent(TJSEvent.new('input'));
  {$else}
  LInput := AView.InputFor(LID);
  Check(LInput is TCustomEdit, 'ordinary preview search input is mounted');
  TCustomEdit(LInput).Text := 'Ada';
  Application.ProcessMessages;
  {$endif}
  Check(LRows.Snapshot.Count = 1, 'actual typed Search filters the visible table');
  Check(LRows.Selection.Contains(LSelected), 'query filtering preserves hidden selected membership');
  Check(Supports(AView.CoordinatorBinding(AToolbar), INyxDataToolbarQueryBinding, LQuery),
    'Studio-configured application exposes its specialized query context');
  Check((LQuery.Observation.Count = 1) and (LQuery.Observation.MatchedRows = 1) and
    (LQuery.Observation.SourceRows = 2), 'query context reports the accepted filter and source counts');
  CheckQueryStatus(AView, AToolbar, LQuery);
  LID := AView.Root.Find(AToolbar).Part(NyxPart(NyxDataToolbarQueryDetailsPart)).ID;
  AView.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  Check(AView.ElementFor(LID).textContent = CQueryButtonCaption,
    'real detail button retains the caption configured in Studio');
  AView.ElementFor(LID).click;
  {$else}
  Check(TButton(AView.ControlFor(LID)).Caption = CQueryButtonCaption,
    'real detail button retains the caption configured in Studio');
  TControlAccess(AView.ControlFor(LID)).Click;
  Application.ProcessMessages;
  {$endif}
  Check(LQuery.QueryControls <> nil, 'real detail button creates the managed query surface');
  LPresenter := LQuery.QueryControls.Presenter;
  Check((LPresenter <> nil) and LPresenter.Connected and LPresenter.IsOpen and
    Supports(LPresenter, TQueryPresenter, LTarget), 'ordinary detail popup opens on the actual view');
  LTarget.Presentation.Renderer.Reveal(CFirstRemovalAction, niRuntime);
  {$ifdef PAS2JS}
  LTarget.Presentation.Renderer.ElementFor(CFirstRemovalAction).click;
  Check(TJSHTMLInputElement(AView.InputFor(LSearchID)).value = 'Ada',
    'explicit filter removal preserves the local search entry');
  {$else}
  TControlAccess(LTarget.Presentation.Renderer.ControlFor(CFirstRemovalAction)).Click;
  Application.ProcessMessages;
  Check(TCustomEdit(AView.InputFor(LSearchID)).Text = 'Ada',
    'explicit filter removal preserves the local search entry');
  {$endif}
  Check((LRows.Snapshot.Count = 2) and (LQuery.Observation.Count = 0) and
    LRows.Selection.Contains(LSelected) and (LRows.Store.Snapshot = LSource),
    'real popup removes one contribution while preserving source data and hidden selection');
  Check(not LPresenter.Connected and not LPresenter.IsOpen,
    'accepted removal retires its captured physical detail form');
  CheckQueryStatus(AView, AToolbar, LQuery);
  LID := AView.Root.Find(AToolbar).Part(NyxPart('clear')).ID;
  AView.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  AView.ElementFor(LID).click;
  {$else}
  TControlAccess(AView.ControlFor(LID)).Click;
  Application.ProcessMessages;
  {$endif}
  Check(LRows.Snapshot.Count = 2, 'ordinary accessible Clear restores the table');
  {$ifdef PAS2JS}
  Check(TJSHTMLInputElement(AView.InputFor(LSearchID)).value = '',
    'local Clear resets the retained entry after explicit query removal');
  {$else}
  Check(TCustomEdit(AView.InputFor(LSearchID)).Text = '',
    'local Clear resets the retained entry after explicit query removal');
  {$endif}
end;

procedure Stage;
{$ifndef PAS2JS}
var
  LStore: TNyxProjectStore;
  LRevision: TNyxText;
{$endif}
begin
  case GStage of
    ksSelect:
      begin
        GBefore := Pair;
        GDefinition := SnapshotNode('workspace');
        SelectOwner('local-tools', False);
        InspectorPanel;
      end;
    ksConfigure: ConfigureForm;
    ksUndo:
      begin
        GInstancePair := Pair;
        Check(GInstancePair <> GBefore, 'ordinary Inspector applies one complete configuration');
        Click('action-undo');
      end;
    ksRedo:
      begin
        Check(Pair = GBefore, 'one ordinary Undo restores exact roles and Pascal');
        Click('action-redo');
      end;
    ksDesignPanel: DesignPanel;
    ksPreviewOpen:
      begin
        Check(Pair = GInstancePair, 'ordinary Redo restores the complete pair');
        Click('action-preview');
      end;
    ksPreview:
      begin
        SearchAndClear(GStudio.CanvasView, 'local-tools', 'people');
        Check(Pair = GInstancePair, 'preview interaction preserves saved defaults and source');
        Click('action-preview');
      end;
    ksInspector: InspectorPanel;
    ksSelectNamed: SelectOwner('left-tools-rule', False);
    ksConfigureNamed: ConfigureForm;
    ksCheckNamed:
      begin
        Check(Pair <> GInstancePair, 'named reusable toolbar configures through ordinary Inspector');
        GInstancePair := Pair;
      end;
    ksProjectPanel:
      begin
        Check(SnapshotNode('workspace') = GDefinition, 'shared definition survives named configuration');

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
        Check(Pos('SetNyxDataToolbarIntent', GStudio.ProjectSnapshot.Source) > 0,
          'ordinary source generation retains crafted typed automatic intent');
        Check((Pos('.QueryContext(NyxDataToolbarQueryOptions.Display(ntqdDetails)',
          GStudio.ProjectSnapshot.Source) > 0) and
          (Pos(CQueryCaption, GStudio.ProjectSnapshot.Source) > 0),
          'ordinary source generation retains the exact typed query presentation');
        {$ifdef PAS2JS}
        Click('action-project-export');
        {$else}
        Change('project-file-name', 'toolbar-workshop');
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
          Check(LStore.ReadProject('toolbar-workshop', LRevision) = GSaved,
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
        WriteNyxTextFile(GDirectory + 'nyx.generated.toolbar.studio.pas',
          NyxTextFile('nyx.generated.toolbar.studio.pas', GStudio.ProjectSnapshot.Source));
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
  LBefore: TNyxText;
  LBinding: INyxDataToolbarBinding;
  LQuery: INyxDataToolbarQueryBinding;
  LControls: INyxCollectionQueryWorkbench;
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
    SearchAndClear(LApplication.View, 'local-tools', 'people');
    SearchAndClear(LApplication.View, NyxQualifiedID('left', 'tools'), NyxQualifiedID('left', 'grid'));
    Check(LApplication.View.CollectionView(NyxQualifiedID('right', 'grid')).Snapshot.Count = 2,
      'unconfigured sibling retains its independent reusable source');
    Check(Supports(LApplication.View.CoordinatorBinding('local-tools'), INyxDataToolbarBinding, LBinding)
      and LBinding.Connected, 'ordinary saved application automatically mounts its toolbar');
    Check(Supports(LBinding, INyxDataToolbarQueryBinding, LQuery),
      'saved application retains specialized query inspection');
    LControls := LQuery.QueryControls;
    LBinding.Sort(NyxToolbarSort('Name descending'));
    Check(LApplication.View.CollectionView('people').Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = 'Grace',
      'saved typed sort choice orders the actual application table');
    Check(TNyxCodec.Encode(ADocument) = LBefore, 'runtime commands never modify saved defaults');
  finally
    LApplication.Free;
  end;
  Check(not LBinding.Connected, 'application disposal retires retained toolbar authority');
  Check((LControls <> nil) and not LControls.Connected,
    'application disposal also revokes retained query-detail authority');
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
      raise Exception.Create('DataToolbar Studio timed out');
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
      document.body.setAttribute('data-toolbar-studio', 'passed');
      document.body.setAttribute('data-toolbar-checks', IntToStr(GChecks));
    end
    else
    begin
      window.setTimeout(@Drive, 20);
    end;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-toolbar-studio', 'failed');
      document.body.setAttribute('data-toolbar-error', E.Message);
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
      raise Exception.Create('DataToolbar Studio did not settle at stage ' + IntToStr(Ord(GStage)));
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
    {$ifdef NYX_COMPILED_TOOLBAR_STUDIO}
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
    document.body.setAttribute('data-toolbar-studio', 'passed');
    document.body.setAttribute('data-toolbar-checks', IntToStr(GChecks));
    {$endif}
    {$else}
    LDocument := Fixture;
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument, 'nyx.generated.toolbar.studio'));
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
      WriteLn('DataToolbar Studio stage ', Ord(GStage), ' / width=', GForm.ClientWidth);
      Flush(Output);
      Stage;
    end;
    Finish;
    {$endif}
    {$endif}
    {$ifndef PAS2JS}
    WriteLn('PASS ', GChecks, ' ordinary DataToolbar Studio/application checks / width=', GWidth);
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-toolbar-studio', 'failed');
      document.body.setAttribute('data-toolbar-error', E.Message);
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
