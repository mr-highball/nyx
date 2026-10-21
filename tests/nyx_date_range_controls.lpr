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

program nyx_date_range_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web,{$else}Interfaces, Classes, Forms, Controls, StdCtrls,
  Calendar, LCLType, nyx.dates.lcl, nyx.files.lcl, nyx.studio.projectstore,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.composition, nyx.source, nyx.files, nyx.studio.files,
  nyx.dates, nyx.contract, nyx.state, nyx.responsive,
  nyx.date.range, nyx.date.range.editor, nyx.studio.inspector,
  nyx.studio.projects, nyx.studio.menu,
  {$ifdef PAS2JS}nyx.studio.browser, nyx.studio.agents, nyx.studio.exchange,
  nyx.test.editor.exchange, nyx.application.browser, nyx.render.browser
  {$else}nyx.studio.lcl, nyx.application.lcl, nyx.render.lcl{$endif}
  {$ifdef NYX_COMPILED_DATE_RANGE}, nyx.generated.date.range{$endif};

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
  { Source-producing inputs settle before the next controller turn. }
  TStage = (dsSelectDefinition, dsDefinitionPanel, dsOrderDefinition,
    dsDefinitionUndo, dsDefinitionRedo, dsSelectInstance, dsInstancePanel,
    dsIndependent, dsMaskUndo, dsMaskRedo, dsInherit, dsInheritedUndo,
    dsInheritedRedo, dsPreviewPanel, dsPreviewStart, dsPreview,
    dsProjectPanel, dsActions, dsProjectMenu, dsOpenMenu, dsSave,
    {$ifdef PAS2JS}dsUndoSaved,{$endif}dsOpen, dsReopened);

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
    raise Exception.Create('DateRange native callback: ' + GObserver.Error);
  end;
  {$endif}

  if not AValue then
  begin
    raise Exception.Create('DateRange Studio stage ' + IntToStr(Ord(GStage)) + ': ' + AReason);
  end;
  Inc(GChecks);
end;

function EndpointKey(ASecond, AFinish: Boolean): TNyxTextStateRef;
const
  CNames: array[Boolean, Boolean] of TNyxText =
    (('first-start', 'first-finish'), ('second-start', 'second-finish'));
begin
  Result := NyxTextState(CNames[ASecond, AFinish]);
end;

function Fixture: TNyxDocument;
var
  LRange: INyxDateRange;
  LPage: INyxPage;
  LFirst: INyxComponent;
  LSecond: INyxComponent;
  LReference: INyxComponent;
  LRule: TNyxNode;
  LSecondIndex: Integer;
  LFinishIndex: Integer;
  LPart: TNyxText;
  LValue: TNyxCalendarDate;
begin
  Result := TNyxDocument.Create;
  try
    LRange := NewNyxDateRange('stay');
    LRange.Configure.WhenViewport(TNyxViewportWidth.Below(700)).Layout(nlColumn).Done;
    LRange.StartDate.WithDate(NyxDate(2026, 11, 1));
    LRange.FinishDate.WithDate(NyxDate(2026, 11, 10));
    LRange.StartDate.Contract.Value(NyxDateDomain.Range(
      NyxDate(2026, 1, 1), NyxDate(2027, 12, 31)));
    LRange.FinishDate.Contract.Value(NyxDateDomain.Range(
      NyxDate(2026, 1, 1), NyxDate(2027, 12, 31)));
    Result.AddComponent(LRange);
    LFirst := NewNyxComponent('first-stay');
    LFirst.Reference := NyxComponent('stay');
    LSecond := NewNyxComponent('second-stay');
    LSecond.Reference := NyxComponent('stay');
    for LSecondIndex := 0 to 1 do
    begin
      LReference := LFirst;

      if LSecondIndex = 1 then
      begin
        LReference := LSecond;
      end;
      for LFinishIndex := 0 to 1 do
      begin
        LPart := 'start';
        LValue := NyxDate(2026, 11, 1);

        if LFinishIndex = 1 then
        begin
          LPart := 'finish';
          LValue := NyxDate(2026, 11, 10);
        end;
        Result.State.SetValue(EndpointKey(LSecondIndex = 1, LFinishIndex = 1), LValue.ToText);
        LRule := LReference.Node.OverridePart(NyxPart(LPart)).
          Named(LReference.ID + '-' + LPart + '-binding');
        LRule.Binds.Value(EndpointKey(LSecondIndex = 1, LFinishIndex = 1)).Done;
      end;
    end;
    LFirst.Node.OverridePart(NyxPart('finish')).Configure.Text('Return date').Done;
    LFirst.Node.OverridePart(NyxPart('.'), noAppend).Named('first-details').
      Add(NewNyxLabel('first-note').WithText('Your dates, your plans.').Node);
    LPage := NewNyxPage('home');
    LPage.Configure.Gap(16).Padding(16).Done;
    LPage.Add(NewNyxHeading('welcome').WithText('Make time for a good journey'));
    LPage.Add(LFirst);
    LPage.Add(LSecond);
    LPage.Add(NewNyxInput('draft-note').WithText('Travel notes'));
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
    GCore.Call('nyx_select', 'DateRange workshop', NyxObject([
      NyxField('expectedRevision', NyxData(GCore.Revision)),
      NyxField('operationId', NyxData('date-range-activate-home')),
      NyxField('id', NyxData('home')), NyxField('activate', NyxData(True))]),
      'date-range-workshop');
  end;
  GCore.Call('nyx_select', 'DateRange workshop', NyxObject([
    NyxField('expectedRevision', NyxData(GCore.Revision)),
    NyxField('operationId', NyxData('date-range-select-' + AID)),
    NyxField('id', NyxData(AID)), NyxField('activate', NyxData(ADefinition))]), 'date-range-workshop');
  {$else}

  if ADefinition then
  begin
    GStudio.Session.Activate('stay');
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

procedure DesignPanel;
begin

  if GStudio.ShellView.RootFor('action-panel-design') <> nil then
  begin
    Click('action-panel-design');
  end;
end;

procedure ClickPolicy(AAction: TNyxDateRangeEditorAction);
begin
  Click(NyxDateRangeEditorActionID(NyxStudioDateRangeEditorID, AAction));
end;

function EndpointID(AView: TView; ASecond, AFinish: Boolean): TNyxText;
var
  LInstance: TNyxText;
  LPart: TNyxText;
  LRange: TNyxNode;
begin
  LInstance := 'first-stay';
  LPart := 'start';

  if ASecond then
  begin
    LInstance := 'second-stay';
  end;

  if AFinish then
  begin
    LPart := 'finish';
  end;
  LRange := AView.Root.Find(NyxQualifiedID(LInstance, 'stay'));
  Check((LRange <> nil) and (GetNyxDateRangeOptions(LRange).Order = ndroAscending),
    'mounted reusable range inherits its ordered policy');
  Result := LRange.Part(NyxPart(LPart)).ID;
end;

function InputText(AView: TView; const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(AView.InputFor(AID, niRuntime)).value;
  {$else}
  Result := TCustomEdit(AView.InputFor(AID, niRuntime)).Text;
  {$endif}
end;

{ Native widgets own their synchronous Change path; their date field withholds
  partial values itself. Browser stimuli explicitly decide when to send Change. }
procedure PhysicalText(AView: TView; const AID, AValue: TNyxText; ADispatch: Boolean);
{$ifndef PAS2JS}
var
  LInput: TControl;
{$endif}
begin
  AView.Reveal(AID, niRuntime);
  {$ifdef PAS2JS}
  TJSHTMLInputElement(AView.InputFor(AID, niRuntime)).value := AValue;

  if ADispatch then
  begin
    AView.InputFor(AID, niRuntime).dispatchEvent(TJSEvent.new('change'));
  end;
  {$else}
  LInput := AView.InputFor(AID, niRuntime);

  if LInput.Parent is TNyxLCLDateField then
  begin
    TNyxLCLDateField(LInput.Parent).Text := AValue;
  end
  else
  begin
    TCustomEdit(LInput).Text := AValue;
  end;
  Application.ProcessMessages;
  {$endif}
end;

procedure ExerciseRanges(AView: TView);
var
  LStart: TNyxText;
  LFinish: TNyxText;
  LRevision: Integer;
  {$ifndef PAS2JS}
  LField: TNyxLCLDateField;
  LKey: Word;
  {$endif}
begin
  LStart := EndpointID(AView, False, False);
  LFinish := EndpointID(AView, False, True);
  PhysicalText(AView, 'draft-note', 'An unfinished travel note', False);
  {$ifndef PAS2JS}
  PhysicalText(AView, LStart, '2026-1', False);
  Check(InputText(AView, LStart) = '2026-1', 'native partial peer remains an unsubmitted draft');
  {$endif}
  LRevision := AView.State.Revision;
  PhysicalText(AView, LFinish, '2026-10-31', True);
  Check((AView.State.Revision = LRevision) and
    (AView.State.GetValue(EndpointKey(False, True)) = '2026-11-10') and
    (InputText(AView, LFinish) = '2026-11-10') and (AView.LastBindingError <> ''),
    'reversed finish rejects before publication and restores only its physical input');
  Check(InputText(AView, 'draft-note') = 'An unfinished travel note',
    {$ifdef PAS2JS}'unsubmitted unrelated text survives a rejected date edit'
    {$else}'accepted unrelated text remains unchanged after date rejection'{$endif});
  {$ifndef PAS2JS}
  Check(InputText(AView, LStart) = '2026-1', 'rejection preserves the other date draft');
  PhysicalText(AView, LStart, '2026-11-01', True);
  {$endif}
  PhysicalText(AView, LFinish, '2026-11-15', True);
  Check((AView.State.Revision = LRevision + 1) and
    (AView.State.GetValue(EndpointKey(False, True)) = '2026-11-15') and
    (AView.State.GetValue(EndpointKey(True, True)) = '2026-11-10'),
    'accepted range input changes its own store field and preserves the sibling');
  PhysicalText(AView, LFinish, '2026-11-01', True);
  Check(AView.State.GetValue(EndpointKey(False, True)) =
    AView.State.GetValue(EndpointKey(False, False)), 'equal endpoints are an admitted range');
  PhysicalText(AView, LFinish, '', True);
  PhysicalText(AView, LStart, '2026-12-01', True);
  Check((AView.State.GetValue(EndpointKey(False, True)) = '') and
    (AView.State.GetValue(EndpointKey(False, False)) = '2026-12-01'),
    'empty and one-sided ranges remain explicit while either endpoint can change');
  LRevision := AView.State.Revision;
  PhysicalText(AView, LFinish, '2026-11-05', True);
  Check((AView.State.Revision = LRevision) and
    (AView.State.GetValue(EndpointKey(False, True)) = ''),
    'completing a reversed one-sided range refuses atomically');
  PhysicalText(AView, LFinish, '2026-12-10', True);
  LRevision := AView.State.Revision;
  PhysicalText(AView, LStart, '2028-01-01', True);
  Check((AView.State.Revision = LRevision) and
    (AView.State.GetValue(EndpointKey(False, False)) = '2026-12-01'),
    'coordinated ordering retains the independent per-field calendar domain');
  {$ifndef PAS2JS}
  LField := TNyxLCLDateField(AView.InputFor(LFinish, niRuntime).Parent);
  LField.Button.Click;
  Application.ProcessMessages;
  Check((LField.Popup <> nil) and LField.Popup.Visible, 'actual end-date calendar opens');
  LField.CalendarControl.DateTime := EncodeDate(2026, 12, 14);
  LKey := VK_RETURN;
  LField.CalendarControl.OnKeyDown(LField.CalendarControl, LKey, []);
  Application.ProcessMessages;
  Check((LKey = 0) and not LField.Popup.Visible and
    (AView.State.GetValue(EndpointKey(False, True)) = '2026-12-14'),
    'calendar Enter accepts through the same coordinated value publication');
  LRevision := AView.State.Revision;
  LField.Button.Click;
  Application.ProcessMessages;
  LField.CalendarControl.DateTime := EncodeDate(2026, 11, 30);
  LKey := VK_RETURN;
  LField.CalendarControl.OnKeyDown(LField.CalendarControl, LKey, []);
  Application.ProcessMessages;
  Check((LKey = 0) and (AView.State.Revision = LRevision) and
    (InputText(AView, LFinish) = '2026-12-14'),
    'calendar selection cannot commit an end date before its start');
  {$endif}
  Check((InputText(AView, EndpointID(AView, True, False)) = '2026-11-01') and
    (InputText(AView, EndpointID(AView, True, True)) = '2026-11-10'),
    'the second reusable range retains both independent accepted inputs');
end;

procedure ApplicationProof(ADocument: TNyxDocument);
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
    ExerciseRanges(LApplication.View);
    Check(TNyxCodec.Encode(ADocument) = LBefore,
      'ordinary application input never rewrites the saved date defaults or policies');
  finally
    LApplication.Free;
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
    dsSelectDefinition: SelectOwner('stay', True);
    dsDefinitionPanel: InspectorPanel;
    dsOrderDefinition:
      begin
        GBefore := Pair;
        ClickPolicy(ndraOrdered);
      end;
    dsDefinitionUndo:
      begin
        Check(Pair <> GBefore, 'ordinary policy action publishes typed ordering with Pascal');
        GDefinitionPair := Pair;
        GDefinition := SnapshotNode('stay');
        GOther := SnapshotNode('second-stay');
        Click('action-undo');
      end;
    dsDefinitionRedo:
      begin
        Check(Pair = GBefore, 'one Undo restores the exact legacy independent project pair');
        Click('action-redo');
      end;
    dsSelectInstance:
      begin
        Check(Pair = GDefinitionPair, 'one Redo restores the exact ordered definition pair');
        SelectOwner('first-stay', False);
      end;
    dsInstancePanel: InspectorPanel;
    dsIndependent: ClickPolicy(ndraIndependent);
    dsMaskUndo:
      begin
        Check((SnapshotNode('stay') = GDefinition) and
          (SnapshotNode('second-stay') = GOther), 'instance policy preserves definition and sibling');
        GIndependentPair := Pair;
        Check(GIndependentPair <> GDefinitionPair, 'explicit Independent stores a distinct local mask');
        Click('action-undo');
      end;
    dsMaskRedo:
      begin
        Check(Pair = GDefinitionPair, 'mask Undo restores inherited ordering with its complete source');
        Click('action-redo');
      end;
    dsInherit:
      begin
        Check(Pair = GIndependentPair, 'mask Redo restores the complete independent declaration');
        ClickPolicy(ndraInherit);
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
    dsPreviewPanel:
      begin
        Check(Pair = GDefinitionPair, 'inherit Redo restores the exact saved-source candidate');
        DesignPanel;
      end;
    dsPreviewStart: Click('action-preview');
    dsPreview:
      begin
        ExerciseRanges(GStudio.CanvasView);
        Check(Pair = GDefinitionPair, 'Interact does not replace authored date defaults');
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
        Check(Pos('SetNyxDateRangeOptions', GStudio.ProjectSnapshot.Source) > 0,
          'ordinary Pascal uses the public typed range policy');
        {$ifdef PAS2JS}Click('action-project-export');
        {$else}
        Change('project-file-name', 'date-range-workshop');
        Click('action-project-save');
        {$endif}
      end;
    {$ifdef PAS2JS}dsUndoSaved: Click('action-undo');{$endif}
    dsOpen:
      begin
        {$ifdef PAS2JS}
        Check(Pair <> GSaved, 'browser Import must restore an actually changed project');
        Check((Length(GFiles.Exported) = 1) and (GFiles.Exported[0].Text = GSaved),
          'ordinary backup retains the complete typed date-range project');
        Click('action-project-import');
        GFiles.Deliver;
        {$else}
        LStore := TNyxProjectStore.Create(GDirectory);
        try
          Check(LStore.ReadProject('date-range-workshop', LRevision) = GSaved,
            'ordinary named Save persists the exact accepted pair');
        finally
          LStore.Free;
        end;
        Click('action-project-open');
        {$endif}
      end;
    dsReopened:
      begin
        Check(Pair = GSaved, 'ordinary Open restores the exact date-range pair');
        {$ifndef PAS2JS}
        Check(not GStudio.Session.CanUndo and not GStudio.Session.CanRedo,
          'actual Open begins deliberate fresh project history');
        WriteNyxTextFile(GDirectory + 'accepted-project.nyxproject',
          NyxTextFile('accepted-project.nyxproject', GSaved));
        WriteNyxTextFile(GDirectory + 'nyx.generated.date.range.pas',
          NyxTextFile('nyx.generated.date.range.pas', GStudio.ProjectSnapshot.Source));
        {$endif}
        GFinished := True;
      end;
  end;

  if not GFinished then
  begin
    GStage := Succ(GStage);
  end;
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
      raise Exception.Create('DateRange Studio timed out');
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
      document.body.setAttribute('data-date-range-studio', 'passed');
      document.body.setAttribute('data-date-range-checks', IntToStr(GChecks));
    end
    else
    begin
      window.setTimeout(@Drive, 20);
    end;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-date-range-studio', 'failed');
      document.body.setAttribute('data-date-range-error', E.Message);
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
      raise Exception.Create('DateRange Studio did not settle at stage ' + IntToStr(Ord(GStage)));
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
    {$ifdef NYX_COMPILED_DATE_RANGE}
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
    document.body.setAttribute('data-date-range-studio', 'passed');
    document.body.setAttribute('data-date-range-checks', IntToStr(GChecks));
    {$endif}
    {$else}
    LDocument := Fixture;
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument, 'nyx.generated.date.range'));
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
      WriteLn('DateRange Studio stage ', Ord(GStage), ' / width=', GForm.ClientWidth);
      Flush(Output);
      Stage;
    end;
    Finish;
    {$endif}
    {$endif}
    {$ifndef PAS2JS}
    WriteLn('PASS ', GChecks, ' ordinary DateRange Studio/application checks / width=', GWidth);
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-date-range-studio', 'failed');
      document.body.setAttribute('data-date-range-error', E.Message);
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
