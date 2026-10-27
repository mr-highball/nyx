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

{ Included by the maintained Studio consumer for an explicit, immutable MCP
  project input. Uses ordinary mounted forms, source/history and project store;
  no fixture reconstruction or test-side policy installation substitutes for it. }
procedure ImportedStage;
{$ifndef PAS2JS}
var
  LStore: TNyxProjectStore;
  LRevision: TNyxText;
{$endif}
begin
  case GStage of
    ksSelect:
      begin
        Check(Pair = GBefore, 'ordinary Open retains the complete exact MCP export');
        SelectOwner('people-tools', False, 'directory');
        InspectorPanel;
      end;
    ksConfigure: ConfigureForm('people-table');
    ksUndo:
      begin
        GInstancePair := Pair;
        Check(GInstancePair <> GBefore, 'ordinary Inspector changes the imported policy/source pair');
        Click('action-undo');
      end;
    ksRedo:
      begin
        Check(Pair = GBefore, 'one ordinary Undo restores the exact original MCP project');
        Click('action-redo');
      end;
    ksDesignPanel:
      begin
        Check(Pair = GInstancePair, 'one ordinary Redo restores the exact Inspector result');
        DesignPanel;
      end;
    ksPreviewOpen: Click('action-preview');
    ksPreview:
      begin
        SearchAndClear(GStudio.CanvasView, 'people-tools', 'people-table');
        Check(Pair = GInstancePair, 'preview commands preserve the imported source and data defaults');
        Click('action-preview');
      end;
    ksInspector:
      begin
        InspectorPanel;
        GStage := Pred(ksProjectPanel);
      end;
    ksProjectPanel:
      begin

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
        Check((Pos('unit nyx.generated.view;', GStudio.ProjectSnapshot.Source) > 0) and
          (Pos(CQueryCaption, GStudio.ProjectSnapshot.Source) > 0),
          'Inspector keeps the imported owning unit and exact authored caption');
        {$ifdef PAS2JS}
        Click('action-project-export');
        {$else}
        Change('project-file-name', 'people-directory');
        Click('action-project-save');
        {$endif}
      end;
    {$ifdef PAS2JS}
    ksUndoSaved: Click('action-undo');
    {$endif}
    ksOpen:
      begin
        {$ifdef PAS2JS}
        Check(Pair <> GSaved, 'ordinary Undo changes the pair before importing the saved backup');
        Check((Length(GFiles.Exported) = 1) and (GFiles.Exported[0].Text = GSaved),
          'ordinary export contains the complete saved imported project');
        Click('action-project-import');
        GFiles.Deliver;
        {$else}
        LStore := TNyxProjectStore.Create(GDirectory);
        try
          Check(LStore.ReadProject('people-directory', LRevision) = GSaved,
            'ordinary named Save writes the exact accepted source/design to disk');
        finally
          LStore.Free;
        end;
        Click('action-project-open');
        {$endif}
      end;
    ksReopened:
      begin
        Check(Pair = GSaved, 'ordinary Open restores the same exact saved Inspector result');
        {$ifndef PAS2JS}
        Check(not GStudio.Session.CanUndo and not GStudio.Session.CanRedo,
          'named Open begins fresh deliberate project history');
        WriteNyxTextFile(GDirectory + 'accepted-project.nyxproject',
          NyxTextFile('accepted-project.nyxproject', GSaved));
        WriteNyxTextFile(GDirectory + 'nyx.generated.view.pas',
          NyxTextFile('nyx.generated.view.pas', GStudio.ProjectSnapshot.Source));
        {$endif}
        GFinished := True;
      end;
  else
    begin
      raise Exception.Create('Unexpected imported Studio stage');
    end;
  end;

  if not GFinished then
  begin
    GStage := Succ(GStage);
  end;
end;

{ Drive the real selector rather than its coordinator. This preserves the same
  adapter event route used by an application caller, without physical gestures. }
procedure ImportedChooseSort(AView: TView; const ACaption: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LInput: TJSHTMLSelectElement;
  {$else}
  LInput: TComboBox;
  {$endif}
begin
  LID := AView.Root.Find('people-tools').Part(NyxPart('sort')).ID;
  AView.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  LInput := TJSHTMLSelectElement(AView.InputFor(LID));
  LInput.value := ACaption;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  Check(AView.InputFor(LID) is TComboBox, 'saved application mounts the actual sort selector');
  LInput := TComboBox(AView.InputFor(LID));
  LInput.ItemIndex := LInput.Items.IndexOf(String(ACaption));
  Check(LInput.ItemIndex >= 0, 'both MCP and Inspector choices survive in the actual selector');
  LInput.OnChange(LInput);
  Application.ProcessMessages;
  {$endif}
end;

function ImportedFirstCell(AView: TView): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := AView.ElementFor('people-table').querySelector('tbody tr td').textContent;
  {$else}
  Check(AView.ControlFor('people-table') is TStringGrid, 'actual saved application table is mounted');
  Result := TNyxText(TStringGrid(AView.ControlFor('people-table')).Cells[0, 1]);
  {$endif}
end;

{ Mount the admitted/saved document in an independent ordinary application. Its
  final retained interfaces must retire; the caller continues to own the input. }
procedure ImportedApplicationProof(ADocument: TNyxDocument);
var
  LApplication: TApplicationHost;
  LBefore: TNyxText;
  LBinding: INyxDataToolbarBinding;
  LQuery: INyxDataToolbarQueryBinding;
  LControls: INyxCollectionQueryWorkbench;
  LClearID: TNyxText;
  LOptional: TNyxNode;
  LIndex: Integer;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LFace: TJSHTMLElement;
  {$else}
  LFace: TControl;
  {$endif}
const
  COptionalParts: array[0..1] of TNyxText = ('create', 'refresh');
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
    for LIndex := 0 to High(COptionalParts) do
    begin
      LOptional := LApplication.View.Root.Find('people-tools').Part(NyxPart(COptionalParts[LIndex]));
      Check(LOptional.Prop(NyxAttributeName(atVisible)) = 'false',
        'Inspector preserves hidden optional actions that have no application handler');
      {$ifdef PAS2JS}
      LFace := LApplication.View.ElementFor(LOptional.ID);
      Check((LFace = nil) or (LFace.getBoundingClientRect.height = 0),
        'the starter application exposes no unwired optional action');
      {$else}
      LFace := LApplication.View.ControlFor(LOptional.ID);
      Check((LFace = nil) or not LFace.IsVisible,
        'the starter application exposes no unwired optional action');
      {$endif}
    end;
    SearchAndClear(LApplication.View, 'people-tools', 'people-table');
    Check(Supports(LApplication.View.CoordinatorBinding('people-tools'),
      INyxDataToolbarBinding, LBinding) and LBinding.Connected,
      'the saved MCP/Inspector application mounts its typed toolbar automatically');
    Check(Supports(LBinding, INyxDataToolbarQueryBinding, LQuery),
      'the saved application retains specialized query context');
    LControls := LQuery.QueryControls;
    ImportedChooseSort(LApplication.View, 'Name descending');
    Check(ImportedFirstCell(LApplication.View) = 'Jordan Ellis',
      'Inspector-edited sort orders the actual native/browser table');
    ImportedChooseSort(LApplication.View, 'Priority, then name');
    Check(ImportedFirstCell(LApplication.View) = 'Ada Brooks',
      'the untouched multi-key MCP choice remains usable after Inspector Save/Open');
    LClearID := LApplication.View.Root.Find('people-tools').Part(NyxPart('clear')).ID;
    RuntimeClick(LApplication.View, LClearID);
    Check((LApplication.View.CollectionView('people-table').Snapshot.Count = 3) and
      (ImportedFirstCell(LApplication.View) = 'Ada Brooks'), 'Clear restores all three source records');
    Check(TNyxCodec.Encode(ADocument) = LBefore,
      'saved application input and query commands preserve all authored defaults');
  finally
    LApplication.Free;
  end;
  Check(not LBinding.Connected and (LControls <> nil) and not LControls.Connected,
    'application disposal revokes retained toolbar and query-detail authority');
end;
