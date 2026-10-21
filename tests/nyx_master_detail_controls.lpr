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

program nyx_master_detail_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web,{$else}Interfaces, Classes, Forms, Controls, StdCtrls,
  LCLType, LMessages, nyx.files.lcl, nyx.studio.projectstore,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.composition, nyx.source, nyx.files, nyx.studio.files,
  nyx.contract, nyx.state, nyx.responsive, nyx.collections, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.query, nyx.collections.selection,
  nyx.behavior, nyx.events, nyx.scheduler,
  nyx.master.detail, nyx.master.detail.editor, nyx.studio.inspector,
  nyx.logic, nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.flow.routes,
  nyx.flow.route.model, nyx.schema, nyx.event.payload,
  nyx.studio.projects, nyx.studio.menu,
  {$ifdef PAS2JS}nyx.studio.browser, nyx.studio.agents, nyx.studio.exchange,
  nyx.test.editor.exchange, nyx.application.browser, nyx.render.browser
  {$else}nyx.studio.lcl, nyx.application.lcl, nyx.render.lcl{$endif}
  {$ifdef NYX_COMPILED_MASTER_DETAIL}, nyx.generated.master.detail{$endif};

type
  { The callback retains only a copied descriptive record, never a renderer.
    Native UI-queued dispatch must keep the record captured before selection moves. }
  TEditObserver = class(TNyxEventCallback)
  public
    Calls: Integer;
    Captured: TNyxMasterDetailRecord;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
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
  { Source-producing inputs settle before the next controller turn. }
  TStage = (dsSelectDefinition, dsDefinitionPanel, dsConfigureDefinition,
    dsDefinitionUndo, dsDefinitionRedo, dsSelectInstance, dsInstancePanel,
    dsIndependent, dsMaskUndo, dsMaskRedo, dsInherit, dsInheritedUndo,
    dsInheritedRedo, dsEventsShow, dsEventsCheck, dsLogicProject, dsLogicOpen, dsLogicInputs,
    dsLogicOutputs, dsLogicPolicy, dsLogicApply, dsLogicUndo,
    dsLogicRedo, dsLogicClose, dsPreviewPanel, dsPreviewStart, dsPreview,
    dsProjectPanel, dsActions, dsProjectMenu, dsOpenMenu, dsSave,
    {$ifdef PAS2JS}dsUndoSaved,{$endif}dsOpen, dsReopened);

const
  CRouteForm = 'studio-logic-editor-control-routes';
  CReviewFlow = 'Review selected record';
  CReviewState = 'record-preview';
  CReviewMemo = 'record-preview-memo';
  CReviewInitial = 'Choose a person, then Edit to inspect the copied record.';

var
  GStudio: TWorkshopStudio;
  GFiles: TProjectFiles;
  GFileLease: INyxTextFileExchange;
  GStage: TStage;
  GChecks: Integer;
  GFinished: Boolean;
  GBefore: TNyxText;
  GDefinitionPair: TNyxText;
  GIndependentPair: TNyxText;
  GRoutedPair: TNyxText;
  GDefinition: TNyxText;
  GOther: TNyxText;
  GSaved: TNyxText;
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
    raise Exception.Create('MasterDetail native callback: ' + GObserver.Error);
  end;
  {$endif}

  if not AValue then
  begin
    raise Exception.Create('MasterDetail Studio stage ' + IntToStr(Ord(GStage)) + ': ' + AReason);
  end;
  Inc(GChecks);
end;

function Person(ASecond: Boolean = False): TNyxItemRef;
begin
  Result := NyxItem(NyxCollection('people'), 'one');

  if ASecond then
  begin
    Result := NyxItem(NyxCollection('people'), 'two');
  end;
end;

function Fixture: TNyxDocument;
var
  LDetail: INyxMasterDetail;
  LPage: INyxPage;
  LFirst: INyxComponent;
  LSecond: INyxComponent;
  LSchema: TNyxCollectionSchema;
  LPreview: INyxMemo;
begin
  Result := TNyxDocument.Create;
  try
    { Seed a useful operation and destination, leaving the route unwired. The
      ordinary Studio form must author the exact Edit Details connection. }
    Result.State.SetValue(NyxTextState(CReviewState), CReviewInitial);
    Result.Flows.Define(NyxFlowRef(CReviewFlow), NewNyxFlowGraph.WithStep(
      NyxFlowStep('preview'), NewNyxJSONFormat(njlIndented, 2)).Build);
    LSchema := NyxCollectionSchema.Text(NyxTextField('short-name'), '').
      Text(NyxTextField('full-name'), '').Text(NyxTextField('summary'), '').
      Integer(NyxIntegerField('score'), 0);
    Result.Collections.Define(NyxCollection('people'), LSchema, [
      NyxCollectionItem(Person).WithValue(NyxTextField('short-name'), 'Ada').
        WithValue(NyxTextField('full-name'), 'Ada Lovelace').
        WithValue(NyxTextField('summary'), 'A carefully written first record.').
        WithValue(NyxIntegerField('score'), 7),
      NyxCollectionItem(Person(True)).WithValue(NyxTextField('short-name'), 'Grace').
        WithValue(NyxTextField('full-name'), 'Grace Hopper').
        WithValue(NyxTextField('summary'), 'A different second record.')]);
    LDetail := NewNyxMasterDetail('directory');
    LDetail.Configure.WhenViewport(TNyxViewportWidth.Below(700)).Layout(nlColumn).Done;
    LDetail.MasterList.Binds.Collection(NyxCollectionView(NyxCollection('people')).
      Column(NyxTextField('short-name'), 'People').Scoped(csInstance)).Done;
    Result.AddComponent(LDetail);
    LFirst := NewNyxComponent('first-directory');
    LFirst.Reference := NyxComponent('directory');
    LFirst.Node.OverridePart(NyxPart('detail/title')).Named('first-title').
      Configure.Text('Your contact').Done;
    LFirst.Node.OverridePart(NyxPart('detail/edit')).Named('first-edit').
      Configure.Text('Edit this contact').Done;
    LFirst.Node.OverridePart(NyxPart('detail'), noAppend).Named('first-detail-content').
      Add(NewNyxLabel('profile-note').WithText('A little context makes a difference.').Node);
    LSecond := NewNyxComponent('second-directory');
    LSecond.Reference := NyxComponent('directory');
    LPage := NewNyxPage('home');
    LPage.Configure.Gap(16).Padding(16).Done;
    LPage.Add(NewNyxHeading('welcome').WithText('People worth getting to know'));
    LPage.Add(LFirst);
    LPage.Add(LSecond);
    LPreview := NewNyxMemo(CReviewMemo).WithText('Selected record JSON');
    LPreview.Configure.ReadOnly(True).Height(240).Done;
    LPreview.Binds.Value(NyxTextState(CReviewState)).Done;
    LPage.Add(LPreview);
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
    GCore.Call('nyx_select', 'MasterDetail workshop', NyxObject([
      NyxField('expectedRevision', NyxData(GCore.Revision)),
      NyxField('operationId', NyxData('master-detail-activate-home')),
      NyxField('id', NyxData('home')), NyxField('activate', NyxData(True))]),
      'master-detail-workshop');
  end;
  GCore.Call('nyx_select', 'MasterDetail workshop', NyxObject([
    NyxField('expectedRevision', NyxData(GCore.Revision)),
    NyxField('operationId', NyxData('master-detail-select-' + AID)),
    NyxField('id', NyxData(AID)), NyxField('activate', NyxData(ADefinition))]), 'master-detail-workshop');
  {$else}

  if ADefinition then
  begin
    GStudio.Session.Activate('directory');
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

procedure InspectorPanel;
begin

  if GStudio.ShellView.RootFor('action-panel-inspector') <> nil then
  begin
    Click('action-panel-inspector');
  end;
end;

{ Read the actual form's owned dropdown labels. Captions stay user-facing;
  their numeric choice prefixes never become programmatic event identities. }
procedure RouteChoice(const AField, ACaption: TNyxText);
var
  LItems: TNyxStrings;
  LIndex: Integer;
  LValue: TNyxText;
  LField: TNyxNode;
begin
  LField := GStudio.ShellView.RootFor(CRouteForm + '-' + AField).
    Find(CRouteForm + '-' + AField);
  Check(LField <> nil, 'ordinary route field is mounted: ' + AField);
  LItems := TNyxStrings.Create;
  try
    LItems.Text := LField.Prop(NyxAttributeName(atItems));
    for LIndex := 0 to LItems.Count - 1 do
    begin
      LValue := LItems[LIndex];

      if (LValue = ACaption) or
        (Copy(LValue, Length(LValue) - Length(ACaption) + 1, Length(ACaption)) = ACaption) then
      begin
        Change(CRouteForm + '-' + AField, LValue);
        Exit;
      end;
    end;
  finally
    LItems.Free;
  end;
  raise Exception.Create('Missing ordinary route choice: ' + AField + ' / ' + ACaption);
end;

procedure DesignPanel;
begin

  if GStudio.ShellView.RootFor('action-panel-design') <> nil then
  begin
    Click('action-panel-design');
  end;
end;

procedure ClickPolicy(AAction: TNyxMasterDetailEditorAction);
begin
  Click(NyxMasterDetailEditorActionID(NyxStudioMasterDetailEditorID, AAction));
end;

{ Derive the stable command index from the exact accepted public metadata, then
  inspect the visible real label. No assumption about enum/card ordering leaks
  into this ordinary user journey. }
procedure CheckInspectorDetails;
var
  LDocument: TNyxDocument;
  LMetadata: TNyxEventSchemas;
  LIndex: Integer;
  LID: TNyxText;
  LText: TNyxText;
begin
  LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
  try
    LMetadata := NyxEventsMetadata(LDocument.Find('first-directory'), LDocument);
    LID := '';
    for LIndex := 0 to High(LMetadata) do
    begin

      if (LMetadata[LIndex].Trigger = ntNamed) and
        (LMetadata[LIndex].Name.Name = NyxSemantic(nseEdit).Name) then
      begin
        LID := 'event-' + NyxTriggerName(ntNamed) + '-' + IntToStr(LIndex) + '-details';
        Break;
      end;
    end;
    Check((LID <> '') and (GStudio.ShellView.RootFor(LID) <> nil),
      'ordinary Events tab mounts the independent Details label for the selected instance');
    LText := 'Details: ' + NyxDataPayload(ndObject).Description;
    {$ifdef PAS2JS}
    Check(GStudio.ShellView.ElementFor(LID).textContent = LText,
      'browser Events Inspector shows the owned object channel');
    {$else}
    Check(TLabel(GStudio.ShellView.ControlFor(LID)).Caption = String(LText),
      'native Events Inspector shows the owned object channel');
    {$endif}
  finally
    LDocument.Free;
  end;
end;

procedure TEditObserver.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if not TryNyxMasterDetailEdit(AEvent, Captured) then
  begin
    raise Exception.Create('The ordinary Edit callback needs its copied record Details');
  end;
  Inc(Calls);
end;

function DetailRoot(AView: TView; ASecond: Boolean): TNyxNode;
var
  LInstance: TNyxText;
begin
  LInstance := 'first-directory';

  if ASecond then
  begin
    LInstance := 'second-directory';
  end;
  Result := AView.Root.Find(NyxQualifiedID(LInstance, 'directory'));
  Check((Result <> nil) and (Result.Kind = NyxKindName(nkMasterDetail)),
    'ordinary host realizes the exact specialized reusable detail owner');
end;

procedure CheckText(AView: TView; ANode: TNyxNode; const AText: TNyxText);
begin
  Check(ANode.Prop(NyxAttributeName(atText)) = AText, 'accepted projected detail text');
  {$ifdef PAS2JS}
  Check(AView.ElementFor(ANode.ID).textContent = AText, 'actual browser detail face');
  {$else}
  Check(TLabel(AView.ControlFor(ANode.ID)).Caption = String(AText),
    'actual native detail face');
  {$endif}
end;

{$ifdef PAS2JS}
{ Yield one browser turn without making elapsed time an acceptance condition.
  The caller still checks the actual publication/callback predicate. }
function YieldUI: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(True);
        end, 0);
    end);
end;
{$endif}

{ Both Sequential graph execution and native UI callbacks may publish later.
  The awaiting caller owns the application/Studio and observer until return. }
procedure AwaitRecord(AView: TView; AObserver: TEditObserver);
  {$ifdef PAS2JS}async;{$endif}
var
  {$ifdef PAS2JS}
  LStarted: Double;
  {$else}
  LStarted: QWord;
  {$endif}
begin
  {$ifdef PAS2JS}LStarted := TJSDate.now;
  {$else}LStarted := GetTickCount64;{$endif}
  while (AView.State.GetValue(NyxTextState(CReviewState)) = CReviewInitial) or
    (AObserver.Calls <> 1) do
  begin

    if AObserver.Calls > 1 then
    begin
      raise Exception.Create('One physical Edit invoked its callback more than once');
    end;
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 60000 then
    {$else}

    if GetTickCount64 - LStarted > 60000 then
    {$endif}
    begin
      raise Exception.Create('Edit record publication did not settle / callbacks=' +
        IntToStr(AObserver.Calls) + ' / preview=' +
        AView.State.GetValue(NyxTextState(CReviewState)));
    end;
    {$ifdef PAS2JS}
    await(TJSPromise.resolve(YieldUI));
    {$else}
    CheckSynchronize;
    Application.ProcessMessages;

    if (GObserver <> nil) and (GObserver.Error <> '') then
    begin
      raise Exception.Create('MasterDetail native callback: ' + GObserver.Error);
    end;
    {$endif}
  end;
end;

procedure ExerciseRecords(AView: TView); {$ifdef PAS2JS}async;{$endif}
var
  LRoot: TNyxNode;
  LOther: TNyxNode;
  LMaster: TNyxNode;
  LEdit: TNyxNode;
  LView: INyxCollectionView;
  LBinding: INyxMasterDetailBinding;
  LOtherBinding: INyxMasterDetailBinding;
  LObserver: TEditObserver;
  LObserverLease: INyxEventCallback;
  LToken: INyxEventSubscription;
  LRetained: TNyxMasterDetailRecord;
  LRevision: Integer;
  LPreview: TNyxText;
  LPreviewData: TNyxDataValue;
  {$ifdef PAS2JS}
  LRow: TJSHTMLElement;
  {$else}
  LList: TListBox;
  LKey: Word;
  {$endif}
begin
  LRoot := DetailRoot(AView, False);
  LOther := DetailRoot(AView, True);
  LBinding := AView.MasterDetailBinding(LRoot.ID);
  LOtherBinding := AView.MasterDetailBinding(LOther.ID);
  Check((LBinding <> nil) and LBinding.Connected and (LOtherBinding <> nil),
    'ordinary applications attach the persisted behavior automatically');
  Check(not LBinding.Current.Defined and not LOtherBinding.Current.Defined,
    'no selected record never chooses the first row implicitly');
  LMaster := LRoot.Part(NyxPart('master'));
  LEdit := LRoot.Part(NyxPart('detail/edit'));
  LView := AView.CollectionView(LMaster.ID);
  LRevision := LView.Store.Snapshot.Revision;
  Check(AView.State.GetValue(NyxTextState(CReviewState)) = CReviewInitial,
    'reopened runtime begins with the authored preview text, not an earlier record');
  AView.Reveal(LMaster.ID, niRuntime);
  {$ifdef PAS2JS}
  LRow := TJSHTMLElement(AView.ElementFor(LMaster.ID).
    querySelector('[data-nyx-item="one"]'));
  Check(LRow <> nil, 'ordinary browser master row is mounted');
  LRow.focus;
  LRow.click;
  {$else}
  LList := TListBox(AView.ControlFor(LMaster.ID));
  LList.SetFocus;
  LList.ItemIndex := LView.Snapshot.IndexOf(Person);
  LList.OnSelectionChange(LList, True);
  {$endif}
  Check(LView.HasSelection and (LView.Selected.ID = 'one') and
    (LBinding.Current.Ref.ID = 'one'), 'actual selection publishes the stable record identity');
  CheckText(AView, LRoot.Part(NyxPart('detail/title')), 'Ada Lovelace');
  CheckText(AView, LRoot.Part(NyxPart('detail/description')),
    'A carefully written first record.');
  CheckText(AView, LOther.Part(NyxPart('detail/title')), 'Record details');
  LObserver := TEditObserver.Create;
  LObserverLease := LObserver;
  LToken := AView.Events.On(NyxControlEvents(LEdit.ID, niRuntime), ntClick).
    {$ifndef PAS2JS}Policy(neUIQueue).{$endif}Subscribe(LObserverLease);
  try
    AView.Reveal(LEdit.ID, niRuntime);
    {$ifdef PAS2JS}AView.ElementFor(LEdit.ID).click;
    {$else}TButton(AView.ControlFor(LEdit.ID)).Click;
    Check(LObserver.Calls = 0, 'native UI-queued Edit has not run inside physical dispatch');
    {$endif}
    { Move selection before any queued graph/UI work is serviced. Both the
      ordinary callback and the formatted result must still describe row one. }
    LView.Select(Person(True));
    {$ifdef PAS2JS}await(AwaitRecord(AView, LObserver));
    {$else}AwaitRecord(AView, LObserver);{$endif}
    LPreview := AView.State.GetValue(NyxTextState(CReviewState));
    LPreviewData := TNyxDataValue.ParseJSON(LPreview);
    Check(TNyxMasterDetailRecord.FromData(LPreviewData).Ref.ID = 'one',
      'ordinary semantic Edit route sends the copied record through JSONFormat to bound text');
    Check(Pos(#10, LPreview) > 0, 'saved graph uses readable indented JSON');
    {$ifdef PAS2JS}
    Check(TJSHTMLTextAreaElement(AView.InputFor(CReviewMemo)).value = LPreview,
      'actual browser Memo displays the event-driven JSON result');
    {$else}
    Check(TNyxDataValue.ParseJSON(TNyxText(TMemo(AView.InputFor(CReviewMemo)).Lines.Text)).ToJSON =
      LPreviewData.ToJSON,
      'actual native Memo displays the event-driven JSON result');
    {$endif}
    Check((LObserver.Calls = 1) and LObserver.Captured.Defined and
      (LObserver.Captured.Ref.ID = 'one') and
      (LObserver.Captured.Item.GetValue(NyxIntegerField('score')) = 7) and
      (LBinding.Current.Ref.ID = 'two'),
      'later selection cannot retarget the copied accepted Edit record or its scalar families');
    Check(AView.State.GetValue(NyxTextState(CReviewState)) = LPreview,
      'changing selection never retargets the already formatted Edit record');
    LRetained := LObserver.Captured.Copy;
  finally
    LToken.Cancel;
    LToken := nil;
    LObserverLease := nil;
  end;
  CheckText(AView, LRoot.Part(NyxPart('detail/title')), 'Grace Hopper');
  LView.ConfigureQuery(NyxCollectionQuery.Where(
    NyxWhere(NyxTextField('short-name')).EqualTo('Ada')));
  Check((LView.Selection.Count = 1) and not LBinding.Current.Defined,
    'hidden selected membership is retained without showing another record');
  CheckText(AView, LRoot.Part(NyxPart('detail/title')), 'Your contact');
  {$ifdef PAS2JS}
  Check(TJSHTMLButtonElement(AView.ElementFor(LEdit.ID)).disabled,
    'hidden record disables the actual browser Edit face');
  {$else}
  Check(not AView.ControlFor(LEdit.ID).Enabled, 'hidden record disables actual native Edit');
  {$endif}
  LView.ConfigureQuery(NyxCollectionQuery);
  Check(LBinding.Current.Ref.ID = 'two', 'restoring visibility restores retained selected identity');
  {$ifndef PAS2JS}
  AView.Reveal(LMaster.ID, niRuntime);
  LList := TListBox(AView.ControlFor(LMaster.ID));
  LList.SetFocus;
  LKey := VK_HOME;
  LList.OnKeyDown(LList, LKey, []);
  Check((LKey = 0) and (LBinding.Current.Ref.ID = 'one'),
    'ordinary list keyboard navigation refreshes detail using the same selection owner');
  {$else}
  LView.Select(Person);
  {$endif}
  LView.Store.Apply([NyxUpdate(NyxCollectionItem(Person).
    WithValue(NyxTextField('summary'), 'A thoughtful update.'))]);
  CheckText(AView, LRoot.Part(NyxPart('detail/description')), 'A thoughtful update.');
  Check((LBinding.Current.DataRevision = LRevision + 1) and
    (LRetained.Item.GetValue(NyxTextField('summary')) = 'A carefully written first record.'),
    'accepted data refreshes live detail without changing earlier record snapshots');
  LView.Store.Apply([NyxRemove(Person)]);
  Check(not LBinding.Current.Defined and not LOtherBinding.Current.Defined,
    'removal clears its detail without selecting the nearby keyboard cursor or sibling');
  CheckText(AView, LRoot.Part(NyxPart('detail/title')), 'Your contact');
  Check((AView.CollectionView(LOther.Part(NyxPart('master')).ID).Store.Snapshot.Revision = 0) and
    (LRetained.Ref.ID = 'one'), 'independent reusable stores and detached Edit data survive');
end;
procedure ApplicationProof(ADocument: TNyxDocument); {$ifdef PAS2JS}async;{$endif}
var
  LApplication: TApplicationHost;
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
    {$ifdef PAS2JS}await(ExerciseRecords(LApplication.View));
    {$else}ExerciseRecords(LApplication.View);{$endif}
    Check(TNyxCodec.Encode(ADocument) = LBefore,
      'ordinary application input never rewrites the saved collection defaults or intent');
  finally
    LApplication.Free;
  end;
end;

procedure Stage; {$ifdef PAS2JS}async;{$endif}
{$ifndef PAS2JS}
var
  LStore: TNyxProjectStore;
  LRevision: TNyxText;
{$endif}
begin
  case GStage of
    dsSelectDefinition: SelectOwner('directory', True);
    dsDefinitionPanel: InspectorPanel;
    dsConfigureDefinition:
      begin
        GBefore := Pair;
        Change(NyxMasterDetailEditorFieldID(NyxStudioMasterDetailEditorID, False), 'full-name');
        Change(NyxMasterDetailEditorFieldID(NyxStudioMasterDetailEditorID, True), 'summary');
        ClickPolicy(ndmaConfigure);
      end;
    dsDefinitionUndo:
      begin
        Check(Pair <> GBefore, 'ordinary policy action publishes typed detail fields with Pascal');
        GDefinitionPair := Pair;
        GDefinition := SnapshotNode('directory');
        GOther := SnapshotNode('second-directory');
        Click('action-undo');
      end;
    dsDefinitionRedo:
      begin
        Check(Pair = GBefore, 'one Undo restores the exact legacy static project pair');
        Click('action-redo');
      end;
    dsSelectInstance:
      begin
        Check(Pair = GDefinitionPair, 'one Redo restores the exact configured definition pair');
        SelectOwner('first-directory', False);
      end;
    dsInstancePanel: InspectorPanel;
    dsIndependent: ClickPolicy(ndmaDisable);
    dsMaskUndo:
      begin
        Check((SnapshotNode('directory') = GDefinition) and
          (SnapshotNode('second-directory') = GOther), 'instance policy preserves definition and sibling');
        GIndependentPair := Pair;
        Check(GIndependentPair <> GDefinitionPair, 'explicit Disabled stores a distinct local mask');
        Click('action-undo');
      end;
    dsMaskRedo:
      begin
        Check(Pair = GDefinitionPair, 'mask Undo restores inherited behavior with its complete source');
        Click('action-redo');
      end;
    dsInherit:
      begin
        Check(Pair = GIndependentPair, 'mask Redo restores the complete disabled declaration');
        ClickPolicy(ndmaInherit);
      end;
    dsInheritedUndo:
      begin
        Check(Pair = GDefinitionPair,
          'Inherit removes only the local policy and restores the exact prior pair');
        Click('action-undo');
      end;
    dsInheritedRedo:
      begin
        Check(Pair = GIndependentPair, 'inherit Undo restores the explicit local mask');
        Click('action-redo');
      end;
    dsEventsShow: Click(NyxInspectorEventsID);
    dsEventsCheck: CheckInspectorDetails;
    dsLogicProject:
      begin
        Check(Pair = GDefinitionPair, 'inherit Redo restores the exact saved-source candidate');

        if GStudio.ShellView.RootFor('action-panel-project') <> nil then
        begin
          Click('action-panel-project');
        end;
      end;
    dsLogicOpen: Click('action-logic-toggle');
    dsLogicInputs:
      begin
        Change(CRouteForm + '-name', CReviewFlow);
        RouteChoice('event', 'edit');
        RouteChoice('graph', CReviewFlow);
        RouteChoice('input', 'preview / ' + NyxLogicPort(nlpJSON).Name + ' / JSON');
        RouteChoice('source', 'Event details');
        Click(CRouteForm + '-path-clear');
        Click(CRouteForm + '-map-input');
      end;
    dsLogicOutputs:
      begin
        RouteChoice('output', 'preview / ' + NyxLogicPort(nlpText).Name + ' / Text');
        RouteChoice('destination', CReviewMemo + ' / Value');
        Click(CRouteForm + '-map-output');
      end;
    dsLogicPolicy:
      begin
        Click(GStudio.ShellView.RootFor(CRouteForm + '-execution').
          Find(CRouteForm + '-execution').Part(NyxPart('header')).ID);
        RouteChoice('policy', 'Sequential');
        Check(Pair = GDefinitionPair, 'unfinished route mappings never mutate the design/source pair');
      end;
    dsLogicApply: Click(CRouteForm + '-apply');
    dsLogicUndo:
      begin
        GRoutedPair := Pair;
        Check((GRoutedPair <> GDefinitionPair) and
          (Pos('.InputEventDetails(', GStudio.ProjectSnapshot.Source) > 0) and
          (Pos('.OutputState(', GStudio.ProjectSnapshot.Source) > 0),
          'ordinary Apply publishes the typed Details-to-state route with its Pascal');
        Check((SnapshotNode('directory') = GDefinition) and
          (SnapshotNode('second-directory') = GOther),
          'local event wiring preserves the reusable definition and sibling');
        Click('action-undo');
      end;
    dsLogicRedo:
      begin
        Check(Pair = GDefinitionPair, 'one route Undo restores the exact unwired source/design pair');
        Click('action-redo');
      end;
    dsLogicClose:
      begin
        Check(Pair = GRoutedPair, 'one route Redo restores the complete wired pair');
        GDefinitionPair := GRoutedPair;
        Click('action-logic-close');
      end;
    dsPreviewPanel:
      begin
        DesignPanel;
      end;
    dsPreviewStart: Click('action-preview');
    dsPreview:
      begin
        {$ifdef PAS2JS}await(ExerciseRecords(GStudio.CanvasView));
        {$else}ExerciseRecords(GStudio.CanvasView);{$endif}
        Check(Pair = GDefinitionPair, 'Interact does not replace authored collection defaults');
        Click('action-preview');
      end;
    dsProjectPanel:
      begin

        if GStudio.ShellView.RootFor('action-panel-project') <> nil then
        begin
          Click('action-panel-project');
        end;
      end;
    dsActions: Click(NyxStudioActionMenuID);
    dsProjectMenu:
      begin
        {$ifdef PAS2JS}ClickPopup('studio-menu-project');
        {$else}ClickMenu('Component actions', 'Project');{$endif}
      end;
    dsOpenMenu:
      begin
        {$ifdef PAS2JS}ClickPopup('studio-menu-open');
        {$else}ClickMenu('Project', 'Open project');{$endif}
      end;
    dsSave:
      begin
        GSaved := Pair;
        Check(Pos('SetNyxMasterDetailIntent', GStudio.ProjectSnapshot.Source) > 0,
          'ordinary Pascal uses the public typed detail intent');
        {$ifdef PAS2JS}Click('action-project-export');
        {$else}
        Change('project-file-name', 'master-detail-workshop');
        Click('action-project-save');
        {$endif}
      end;
    {$ifdef PAS2JS}dsUndoSaved: Click('action-undo');{$endif}
    dsOpen:
      begin
        {$ifdef PAS2JS}
        Check(Pair <> GSaved, 'browser Import must restore an actually changed project');
        Check((Length(GFiles.Exported) = 1) and (GFiles.Exported[0].Text = GSaved),
          'ordinary backup retains the complete typed master-detail project');
        Click('action-project-import');
        GFiles.Deliver;
        {$else}
        LStore := TNyxProjectStore.Create(GDirectory);
        try
          Check(LStore.ReadProject('master-detail-workshop', LRevision) = GSaved,
            'ordinary named Save persists the exact accepted pair');
        finally
          LStore.Free;
        end;
        Click('action-project-open');
        {$endif}
      end;
    dsReopened:
      begin
        Check(Pair = GSaved, 'ordinary Open restores the exact master-detail pair');
        {$ifndef PAS2JS}
        Check(not GStudio.Session.CanUndo and not GStudio.Session.CanRedo,
          'actual Open begins deliberate fresh project history');
        WriteNyxTextFile(GDirectory + 'accepted-project.nyxproject',
          NyxTextFile('accepted-project.nyxproject', GSaved));
        WriteNyxTextFile(GDirectory + 'nyx.generated.master.detail.pas',
          NyxTextFile('nyx.generated.master.detail.pas', GStudio.ProjectSnapshot.Source));
        {$endif}
        GFinished := True;
      end;
  end;

  if not GFinished then
  begin
    GStage := Succ(GStage);
  end;
end;
procedure Finish; {$ifdef PAS2JS}async;{$endif}
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
    {$ifdef PAS2JS}await(ApplicationProof(LDocument));
    {$else}ApplicationProof(LDocument);{$endif}
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

{$ifdef PAS2JS}
{ One awaited controller loop prevents another stage or cleanup from overtaking
  a suspended physical Edit proof. Source exchange keeps its existing gates. }
procedure Drive; async;
begin
  while not GFinished do
  begin

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
      raise Exception.Create('MasterDetail Studio timed out');
    end;

    if not GStudio.SourceBusy and not GStudio.PresentationPending and
      GStudio.SharedSourceSynchronized then
    begin
      await(Stage);
    end;

    if not GFinished then
    begin
      await(TJSPromise.resolve(YieldUI));
    end;
  end;
  await(Finish);
end;

{ In compiled mode this frame owns BuildNyxDocument across the entire awaited
  application proof. No program-tail free can retire the document prematurely. }
procedure RunBrowser; async;
var
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
begin
  LDocument := nil;
  try
    try
      {$ifdef NYX_COMPILED_MASTER_DETAIL}
      GSaved := document.getElementById('expected-project').textContent;
      LPair := DecodeNyxProject(GSaved);
      LDocument := BuildNyxDocument;
      Check(TNyxCodec.Encode(LDocument) = LPair.Design,
        'compiled designer output reconstructs the exact saved Studio design');
      await(ApplicationProof(LDocument));
      {$else}
      LDocument := Fixture;
      LPair := NyxProjectPair(TNyxCodec.Encode(LDocument),
        TNyxCodegen.Generate(LDocument, 'nyx.generated.master.detail'));
      FreeAndNil(LDocument);
      GFiles := TProjectFiles.Create;
      GFileLease := GFiles;
      GStarted := TJSDate.now;
      GCore := TNyxAgentSession.Create(LPair);
      GStudio := TWorkshopStudio.Create;
      GStudio.Run(False);
      GStudio.ConnectAgents;
      await(Drive);
      {$endif}
    finally
      FreeAndNil(GStudio);
      GExchange := nil;
      FreeAndNil(GCore);
      GFileLease := nil;
      GFiles := nil;
      LDocument.Free;
    end;
    document.body.setAttribute('data-master-detail-studio', 'passed');
    document.body.setAttribute('data-master-detail-checks', IntToStr(GChecks));
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-master-detail-studio', 'failed');
      document.body.setAttribute('data-master-detail-error', E.Message);
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
      raise Exception.Create('MasterDetail Studio did not settle at stage ' + IntToStr(Ord(GStage)));
    end;

    if GStudio.SourceBusy or GStudio.PresentationPending then
    begin
      Sleep(1);
    end;
  until not GStudio.SourceBusy and not GStudio.PresentationPending;
end;

{ Owners are already revoked/freed before this drain. Cancelled scheduler
  couriers still need their ordinary UI delivery to release their work leases. }
procedure DrainCancelledUI;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  while CheckSynchronize(0) do
  begin

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Cancelled native UI couriers did not retire');
    end;
  end;
end;

procedure RunNative;
var
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
begin
  LDocument := nil;
  try

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
    {$ifdef NYX_COMPILED_MASTER_DETAIL}

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Compiled companion needs its exact pinned accepted project packet');
    end;
    GSaved := ReadNyxTextFile(ParamStr(3)).Text;
    LPair := DecodeNyxProject(GSaved);
    LDocument := BuildNyxDocument;
    Check(TNyxCodec.Encode(LDocument) = LPair.Design,
      'compiled designer output reconstructs the exact saved Studio design');
    ApplicationProof(LDocument);
    {$else}
    LDocument := Fixture;
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument, 'nyx.generated.master.detail'));
    FreeAndNil(LDocument);
    GFiles := TProjectFiles.Create;
    GFileLease := GFiles;
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
      WriteLn('MasterDetail Studio stage ', Ord(GStage), ' / width=', GForm.ClientWidth);
      Flush(Output);
      Stage;
    end;
    Finish;
    {$endif}
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message, ' / checks=', GChecks);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  FreeAndNil(GStudio);
  FreeAndNil(GForm);
  FreeAndNil(LDocument);
  try
    DrainCancelledUI;

    if (GObserver <> nil) and (GObserver.Error <> '') then
    begin
      raise Exception.Create('MasterDetail native callback: ' + GObserver.Error);
    end;
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message, ' / native teardown');
      ExitCode := 1;
    end;
  end;
  Application.OnException := nil;
  FreeAndNil(GObserver);
  GFileLease := nil;
  GFiles := nil;

  if ExitCode = 0 then
  begin
    WriteLn('PASS ', GChecks, ' ordinary MasterDetail Studio/application checks / width=', GWidth);
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}RunBrowser;
  {$else}RunNative;{$endif}
end.
