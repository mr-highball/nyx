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



program nyx_collection_queue_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Types, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
  IntfGraphics, FPWritePNG, nyx.text, nyx.types, nyx.state, nyx.model, nyx.collections,
  nyx.collections.view.types, nyx.collections.selection, nyx.studio.projects,
  nyx.studio.session, nyx.studio.authoring, nyx.studio.collections,
  nyx.studio.sourcejobs, nyx.studio.lcl, nyx.render.lcl, nyx.test.collection.queue;

type
  TControlAccess = class(TControl);
  { Observe actual native dispatch failures without owning Studio or its controls. }
  TFailureObserver = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  GStudio: TNyxNativeStudio;
  GForm: TForm;
  GObserver: TFailureObserver;
  GChecks: Integer;

procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.Message;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition or (GObserver.Error <> '') then
  begin
    raise ENyxCollection.Create('Collection controls: ' + AReason + ' / ' + GObserver.Error);
  end;
  Inc(GChecks);
end;

procedure Pump;
begin
  CheckSynchronize;
  Application.ProcessMessages;
end;

procedure Ready;
var
  LStart: QWord;
begin
  LStart := GetTickCount64;
  repeat
    Pump;

    if GetTickCount64 - LStart > 30000 then
    begin
      raise ENyxCollection.Create('Collection command did not retire / ' + GStudio.Status);
    end;
    Sleep(1);
  until not GStudio.SourceCommands.Busy and not GStudio.PresentationPending;
  Pump;
end;

procedure Phase(const AName: TNyxText);
begin
  WriteLn('Qualified actual collection phase: ', AName);
  Flush(Output);
end;

procedure Click(const AID: TNyxText; AWait: Boolean = True);
var
  LControl: TControl;
begin
  if GStudio.ShellView.Root.Find(AID) <> nil then
  begin
    LControl := GStudio.ShellView.ControlFor(AID);
  end
  else
  begin
    LControl := GStudio.SourceView.ControlFor(AID);
  end;
  Check(LControl <> nil, 'Actual command exists / ' + AID);
  TControlAccess(LControl).Click;

  if AWait then
  begin
    Ready;
  end;
end;

function Input(const AID: TNyxText): TCustomEdit;
var
  LControl: TControl;
begin

  if AID = 'studio-code' then
  begin
    LControl := GStudio.CodeView.InputFor(AID);
  end
  else
  begin
    LControl := GStudio.ShellView.InputFor(AID);
  end;
  Check(LControl is TCustomEdit, 'Actual text editor exists / ' + AID);
  Result := TCustomEdit(LControl);
end;

procedure Text(const AID, AValue: TNyxText);
begin
  Input(AID).Text := AValue;
end;

procedure Choice(const AID, AValue: TNyxText);
var
  LCombo: TComboBox;
  LIndex: Integer;
begin
  LCombo := TComboBox(GStudio.ShellView.InputFor(AID));
  LIndex := LCombo.Items.IndexOf(AValue);
  Check(LIndex >= 0, 'Closed choice exists / ' + AValue);

  if LCombo.CanSetFocus then
  begin
    LCombo.SetFocus;
  end;
  LCombo.ItemIndex := LIndex;
  LCombo.OnChange(LCombo);
end;

function PairText: TNyxText;
begin
  Result := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
end;

procedure Select(const AID: TNyxText);
begin
  GStudio.Session.Select(AID);
  GStudio.RequestRefresh;
  Ready;
end;

function Spec(const AOwner: TNyxText): TNyxCollectionViewSpec;
var
  LProjection: TNyxNode;
  LSelection: TNyxText;
begin
  LSelection := GStudio.Session.SelectedID;
  GStudio.Session.Select(AOwner);
  LProjection := GStudio.Session.SelectedProjection;
  try
    Result := LProjection.CollectionView;
  finally
    LProjection.Free;
    GStudio.Session.Select(LSelection);
  end;
end;

{ Inspect the exact pending public panels through real LCL controls before
  delivering a worker reply. This proves physical enablement independently of
  compiler/thread speed; the ordinary journey proves publication separately. }
procedure CheckPending(const APending: TNyxStudioPendingDesign;
  const AID: TNyxText; AEnabled: Boolean);
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LProjection: TNyxNode;
  LHost: TPanel;
  LRenderer: TNyxLCLRenderer;
begin
  LDocument := TNyxDocument.Create;
  LHost := TPanel.Create(GForm);
  LRenderer := TNyxLCLRenderer.Create;
  LProjection := nil;
  try
    LHost.Parent := GForm;
    LHost.Visible := False;
    LRoot := TNyxNode.Create(nkColumn, 'pending-collection-panels');
    LDocument.AddPage(LRoot);
    AddNyxCollectionDefaultsPanel(LRoot, GStudio.Session, True, APending);
    LProjection := GStudio.Session.SelectedProjection;
    AddNyxCollectionBindingPanel(LRoot, GStudio.Session, LProjection, APending);
    LRenderer.Render(LDocument, LRoot, LHost);
    Check(LRenderer.ControlFor(AID).Enabled = AEnabled,
      'Exact pending panel mounts expected native enablement / ' + AID);
  finally
    LProjection.Free;
    LRenderer.Free;
    LHost.Free;
    LDocument.Free;
  end;
end;

procedure Capture(const AName: TNyxText);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(GForm.ClientWidth, GForm.ClientHeight);
    GForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

procedure RevealBinding;
var
  LScroll: TScrollBox;
  LControl: TControl;
  LPosition: TPoint;
begin
  LScroll := TScrollBox(GStudio.ShellView.ControlFor('studio-right'));
  LControl := GStudio.ShellView.ControlFor('studio-collection-binding');
  Check((LScroll <> nil) and (LControl <> nil), 'Actual binding viewport exists');
  LPosition := LScroll.ScreenToClient(LControl.ClientToScreen(Point(0, 0)));
  LScroll.VertScrollBar.Position := LScroll.VertScrollBar.Position + LPosition.Y;
  Pump;
end;

procedure Run;
var
  LSeed: TNyxProjectPair;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LDraft: TNyxText;
  LBase: TNyxText;
  LPending: TNyxStudioPendingDesign;
  LSnapshot: INyxCollectionSnapshot;
  LSpec: TNyxCollectionViewSpec;
  LEditor: TCustomEdit;
begin
  Application.Initialize;
  ForceDirectories(ParamStr(1));
  GObserver := TFailureObserver.Create;
  Application.OnException := GObserver.Failed;
  GForm := TForm.Create(nil);
  GForm.ClientWidth := 1100;
  GForm.ClientHeight := 800;
  GStudio := TNyxNativeStudio.Create(GForm,
    IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects');
  try
    LSeed := CreateNyxCollectionQueueSeed;
    GStudio.LoadProject(LSeed);
    GStudio.Run;
    GForm.Show;
    Ready;
    Click(NyxStudioStateToggleID);
    Select('tasks-table');
    Click(NyxStudioBindingsToggleID);

    LEditor := Input('collection-column-1-title');
    LEditor.SetFocus;
    Text('collection-column-1-title', 'First title');
    Text('collection-column-1-title', 'Task details');
    LPending := GStudio.SourceCommands.PendingDesign;
    Check(Length(LPending.Collections) > 0, 'Rapid title changes enter isolated preparation');
    Pump;
    Check((Input('collection-column-1-title').Text = 'Task details') and
      (Screen.ActiveControl = GStudio.ShellView.InputFor('collection-column-1-title')),
      'Pending title and exact field focus survive earlier paint');
    Ready;
    LSpec := Spec('tasks-table');
    Check((LSpec.ColumnAt(0).FieldName = 'priority') and
      (LSpec.ColumnAt(1).Title = 'Task details'), 'Title follows reordered typed field identity');
    Choice('collection-column-1-mode', 'Editable');
    Ready;
    Choice('collection-binding-selection', 'Multiple items');
    Ready;
    Check((Spec('tasks-table').ColumnAt(1).Mode = cmEditable) and
      (Spec('tasks-table').SelectionMode = nsmMultiple), 'Editing and selection choices publish typed values');

    Click('collection-column-2-remove', False);
    LPending := GStudio.SourceCommands.PendingDesign;
    CheckPending(LPending, 'collection-binding-scope', False);
    Ready;
    Check(Spec('tasks-table').Count = 3, 'Exact column is removed');
    Click('collection-column-add-1');
    Check(Spec('tasks-table').ColumnAt(3).FieldName = 'done', 'Typed column is restored after reordered neighbors');
    Phase('pending title/focus, typed selection/mode and structural view lock');

    Text('collection-0-row-0-cell-0', 'Earlier wording');
    Text('collection-0-row-0-cell-0', 'Ready for release');
    Ready;
    LSnapshot := GStudio.Session.Document.Collections.Snapshot(NyxCollection('tasks'));
    Check(LSnapshot.ItemAt(0).GetValue(NyxTextField('caption')) = 'Ready for release',
      'Newer row text wins through ordinary queued controls');
    LEditor := Input('collection-0-row-0-cell-2');
    LEditor.SetFocus;
    LBefore := PairText;
    Text('collection-0-row-0-cell-2', '-');
    Ready;
    Check((PairText = LBefore) and (Input('collection-0-row-0-cell-2').Text = '1') and
      (Screen.ActiveControl = GStudio.ShellView.InputFor('collection-0-row-0-cell-2')),
      'Rejected partial Integer restores accepted text without moving field focus');
    Text('collection-0-field-2', '7');
    Ready;
    Check(GStudio.Session.Document.Collections.Snapshot(NyxCollection('tasks')).Schema
      .FieldAt(2).DefaultValue.IntegerValue = 7, 'Typed default remains independent of existing row values');
    Choice('collection-0-row-0-cell-1', 'true');
    Ready;
    Check(GStudio.Session.Document.Collections.Snapshot(NyxCollection('tasks')).ItemAt(0)
      .GetValue(NyxBooleanField('done')), 'Boolean editor publishes its exact family');
    Phase('newer row text, rejected numeric input/focus and independent defaults');

    LBase := GStudio.Session.Source;
    LDraft := LBase + #10 + '{ Independent application draft }';
    Click('action-code');
    Text('studio-code', LDraft);
    LBefore := PairText;
    Text('collection-0-row-0-cell-3', '0.375');
    Ready;
    LAfter := PairText;
    Check((GStudio.Session.DraftSource = LDraft) and
      (GStudio.Session.SourceDraftBase = LBase), 'Collection publication retains independent Pascal draft/base');
    Click('action-undo');
    Check(PairText = LBefore, 'One ordinary Undo restores the exact source/design pair');
    Click('action-redo');
    Check(PairText = LAfter, 'One ordinary Redo restores the admitted collection pair');
    Click('action-reset-source');

    Click('collection-0-add-row', False);
    LPending := GStudio.SourceCommands.PendingDesign;
    CheckPending(LPending, 'collection-0-row-0-remove', False);
    CheckPending(LPending, 'collection-0-field-2', False);
    Ready;
    Check(GStudio.Session.Document.Collections.Snapshot(NyxCollection('tasks')).Count = 3,
      'Allocated new row is admitted once');
    Click('collection-0-row-2-remove');
    Check(GStudio.Session.Document.Collections.Snapshot(NyxCollection('tasks')).Count = 2,
      'Captured row removal leaves its neighbors intact');
    Click('collection-create', False);
    CheckPending(GStudio.SourceCommands.PendingDesign, 'collection-create', False);
    Ready;
    Click('collection-1-add-integer');
    Check(GStudio.Session.Document.Collections.Snapshot(NyxCollection('collection1')).Schema
      .FieldAt(1).Kind = nskInteger, 'New collection accepts a typed Integer field');
    Click('collection-1-remove');
    Check(GStudio.Session.Document.Collections.Count = 1, 'Unreferenced temporary collection is removed');
    Phase('paired history, independent source draft and data structural locks');

    Select('unbound-table');
    Click('collection-bind-0');
    Check(Spec('unbound-table').Count = 5, 'Unbound view accepts current typed schema');
    Click('collection-binding-clear');
    Check(not Spec('unbound-table').Defined, 'Clear admits explicit absent binding');
    Select('left-list');
    Choice('collection-binding-scope', 'Application');
    Ready;
    Check((Spec('left-list').Scope = csApplication) and
      (Spec('right-list').Scope = csInstance), 'Reusable collection scope override is independently owned');
    Select('left-list');
    Click('collection-binding-inherit');
    Check(Spec('left-list').Scope = csInstance, 'Inherit restores reusable recipe binding');
    Click('collection-binding-clear');
    Check(not Spec('left-list').Defined and (Spec('right-list').Scope = csInstance),
      'Cleared reusable view leaves the other instance bound');
    Check(GStudio.ShellView.ControlFor('collection-binding-inherit') <> nil,
      'Cleared binding keeps an actual Restore inherited binding control');
    Click('collection-binding-inherit');
    Check(Spec('left-list').Defined and (Spec('left-list').Scope = csInstance),
      'Actual queued Restore button removes the exact reusable clear mask');

    GStudio.Session.Activate('other');
    Select('tasks-tree');
    Click('collection-parent-none');
    Check(Spec('tasks-tree').ParentField = '', 'Tree parent mapping can be removed');
    Click('collection-parent-4');
    Check(Spec('tasks-tree').ParentField = 'parent', 'Typed tree parent mapping can be restored');
    GStudio.Session.Activate('home');
    Select('tasks-table');
    Choice('collection-binding-selection', 'Single item');
    Select('unbound-table');
    Check((GStudio.Session.SelectedID = 'unbound-table') and
      (Spec('tasks-table').SelectionMode = nsmSingle),
      'Captured view intent publishes independently of later navigation');
    Phase('bind/clear/inherit, independent reusable scope and tree parent mapping');

    Select('tasks-table');
    RevealBinding;
    Capture('collections-desktop');
    GForm.ClientWidth := 390;
    GStudio.RequestRefresh;
    Ready;
    Click('action-panel-inspector');
    RevealBinding;
    Capture('collections-390');
    Phase('English desktop and narrow binding presentation');

    GStudio.Session.LoadProject(LSeed);
    LBefore := PairText;
    Click('collection-column-1-remove', False);
    Check(not GStudio.SourceCommands.Busy and (PairText = LBefore),
      'Older mounted collection inspector cannot edit a new load with the same IDs');
    GStudio.RequestRefresh;
    Ready;
    Select('tasks-table');
    Text('collection-column-1-title', 'Detached preparation');
    Check(GStudio.SourceCommands.Busy, 'Retirement starts with an actual collection request');
    FreeAndNil(GStudio);
    Pump;
    Check(GObserver.Error = '', 'Detached worker cannot call freed editor or field controls');
  finally
    GStudio.Free;
    GStudio := nil;
    GForm.Free;
    Application.OnException := nil;
    GObserver.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' actual native queued collection checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
