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

uses Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls,
  nyx.text, nyx.types, nyx.model, nyx.codec, nyx.codegen, nyx.files, nyx.files.lcl,
  nyx.disclosure,
  nyx.collections, nyx.collections.codec, nyx.collections.view.types,
  nyx.collections.mount, nyx.collections.arrangement,
  nyx.collections.arrangement.controls, nyx.collections.arrangement.presenter,
  nyx.collections.arrangement.lcl, nyx.collections.query,
  nyx.application.lcl, nyx.service.monitor,
  nyx.studio.lcl, nyx.studio.projects, nyx.studio.projectstore,
  nyx.studio.section.views, nyx.studio.collections, nyx.studio.inspector,
  nyx.studio.menu, nyx.studio.authoring
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

    LSection := LStudio.ShellView.Root.Find(NyxStudioLayoutPolicyFormID(2) + '-section');
    Check(LSection <> nil, 'Command line layout is discoverable in the Inspector');

    if not NyxDisclosureExpanded(LSection) then
    begin
      Click(LStudio, LSection.Part(NyxPart('header')).ID);
    end;
    LBefore := StudioPair(LStudio);
    Change(LStudio, NyxStudioLayoutPolicyFieldID(2, slpfVisible), 'false');
    Check(StudioPair(LStudio) = LBefore, 'visibility remains a draft before Apply');
    Click(LStudio, NyxStudioLayoutPolicyApplyID(2));
    Check(not LStudio.Session.Selected.CollectionView.ColumnAt(2).LayoutPolicy.IsVisible,
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

procedure ApplicationJourney(const APair: TNyxProjectPair);
var
  LDocument: TNyxDocument;
  LApplication: TNyxLCLApplication;
  LPeer: TNyxLCLApplication;
  LMount: INyxCollectionMount;
  LColumns: INyxCollectionColumnArrangementMount;
  LManager: INyxColumnArrangementManager;
  LBefore: TNyxText;
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
      not LDocument.Find(ServiceTableID).CollectionView.ColumnAt(2).LayoutPolicy.IsVisible,
      'application retains both Studio-authored column defaults');
    LApplication := TNyxLCLApplication.Create;
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
    Check(not LManager.Observation.ColumnAt(2).IsVisible,
      'hidden authored field remains discoverable for reveal');
    ColumnClick(LManager, NyxColumnIdentity(NyxTextField('command')), ncaaShow);
    Check(LManager.Observation.ColumnAt(2).IsVisible, 'Show reveals Command line');
    ColumnClick(LManager, NyxColumnIdentity(NyxTextField('notes')), ncaaMoveEarlier);
    Check(LManager.Observation.OrderedAt(2) = 3, 'Move earlier reorders Notes by stable field');
    ColumnClick(LManager, NyxColumnIdentity(NyxTextField('notes')), ncaaHide);
    Check(not LManager.Observation.ColumnAt(3).IsVisible, 'Hide preserves the Notes field');
    ColumnClick(LManager, NyxColumnIdentity(NyxTextField('notes')), ncaaShow);
    Check(LManager.Observation.ColumnAt(3).IsVisible, 'Show restores hidden Notes');
    LMount.View.ConfigureQuery(NyxCollectionQuery.Where(
      NyxWhere(NyxTextField('status')).EqualTo('Running')));
    Pump;
    Check((LMount.View.Snapshot.Count = 2) and
      (LPeer.View.CollectionMount(ServiceTableID).View.Snapshot.Count = 4),
      'filtering works with arrangement while the independent peer remains unchanged');
    ColumnClick(LManager, Default(TNyxColumnIdentity), ncaaResetAll);
    Check(not LManager.Observation.ColumnAt(2).IsVisible and
      (LManager.Observation.Preferences.Count = 0) and
      (LManager.Observation.Preferences.OrderCount = 0),
      'Restore defaults returns to saved visibility/order rather than factory defaults');
    Check(LMount.View.Snapshot.Count = 2, 'arrangement reset preserves the active filter');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'runtime commands never rewrite the saved design');
  finally
    LManager := nil;
    LColumns := nil;
    LMount := nil;
    LPeer.Free;
    LApplication.Free;
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
