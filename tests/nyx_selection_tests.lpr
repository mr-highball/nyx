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
program nyx_selection_tests;

{$mode delphi}{$H+}
{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.controls, nyx.model, nyx.codec,
  nyx.codegen, nyx.source, nyx.contract, nyx.behavior, nyx.schema, nyx.events,
  nyx.callbacks, nyx.scheduler, nyx.collections, nyx.collections.selection,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.mount,
  nyx.studio.session, nyx.studio.collections,
  nyx.studio.agents, nyx.studio.projects,
  {$ifdef NYX_COMPILED_SELECTION}nyx.selection.fixture,{$endif}
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Classes, Types, Interfaces, Forms, Controls, StdCtrls, Grids, ComCtrls,
  LCLType, LMessages, nyx.render.lcl;{$endif}

type
  {$ifdef PAS2JS}
  TSelectionKeyEvent = class external name 'KeyboardEvent' (TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  TSelectionMouseEvent = class external name 'MouseEvent' (TJSMouseEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  TSelectionDetails = class external name 'HTMLDetailsElement' (TJSHTMLElement)
    open: Boolean;
  end;
  {$endif}
  { The callback retains owned values only. A renderer pointer is borrowed for a
    deliberate navigation test and cleared before the owner's final disposal. }
  TSelectionProbe = class(TNyxEventCallback, INyxCallbackFactory)
    Calls: Integer;
    Last: TNyxEventInfo;
    Navigate: Boolean;
    {$ifdef PAS2JS}Renderer: TNyxBrowserRenderer;
    {$else}Renderer: TNyxLCLRenderer;{$endif}
    function Resolve(const AHandler: TNyxHandlerRef): INyxEventCallback;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  TKeyConsumption = class(TNyxEventCallback)
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$ifndef PAS2JS}
  TControlAccess = class(TWinControl);
  {$endif}

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Selection: ' + AReason);
  end;
  Inc(GChecks);
end;

function Key: TNyxCollectionRef;
begin
  Result := NyxCollection('tasks / 🌙');
end;

function Row(AIndex: Integer): TNyxItemRef;
const
  CIDs: array[0..3] of TNyxText = ('idea / 漢字', 'sketch', 'build', 'share');
begin
  Result := NyxItem(Key, CIDs[AIndex]);
end;

function Store: INyxCollection;
begin
  Result := NewNyxCollection(Key,
    NyxCollectionSchema.Text(NyxTextField('caption'), '')
      .Text(NyxTextField('parent'), '').Integer(NyxIntegerField('priority'), 1), [
    NyxCollectionItem(Row(0)).WithValue(NyxTextField('caption'), 'An idea 🌙'),
    NyxCollectionItem(Row(1)).WithValue(NyxTextField('caption'), 'A sketch'),
    NyxCollectionItem(Row(2)).WithValue(NyxTextField('caption'), 'A build'),
    NyxCollectionItem(Row(3)).WithValue(NyxTextField('caption'), 'A shared result')]);
end;

function Spec: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(Key).Column(NyxTextField('caption'), 'Task')
    .Selection(nsmMultiple);
end;

function Fixture: TNyxDocument;
var
  LHome: INyxColumn;
  LBrowser: INyxColumn;
  LTasksList: INyxList;
  LTasksTable: INyxTable;
  LTasksTree: INyxTree;
begin
  Result := TNyxDocument.Create;
  Result.Collections.Define(Store.Snapshot);
  LHome := NewNyxColumn('home');
  LBrowser := NewNyxColumn('task-browser');
  LBrowser.Configure.Compound(True).Gap(12).Done;
  LHome.Add(LBrowser);
  Result.AddPage(LHome);
  LTasksList := NewNyxList('tasks-list');
  LTasksList.Configure.AccessibleName('Tasks').Height(120).Done;
  LTasksList.Binds.Collection(Spec).Done;
  LTasksTable := NewNyxTable('tasks-table');
  LTasksTable.Configure.AccessibleName('Task table').Height(160).Done;
  LTasksTable.Binds.Collection(NyxCollectionView(Key)
    .Column(NyxTextField('caption'), 'Task', cmEditable)
    .Column(NyxIntegerField('priority'), 'Priority', cmEditable)
    .Selection(nsmMultiple)).Done;
  LTasksTree := NewNyxTree('tasks-tree');
  LTasksTree.Configure.AccessibleName('Task tree').Height(160).Done;
  LTasksTree.Binds.Collection(Spec.Parent(NyxTextField('parent'))).Done;
  LBrowser.Add(LTasksList).Add(LTasksTable).Add(LTasksTree);
  NyxCallbacks(LTasksList).OnSelectionChange
    .Add(NyxHandler('TSelectionCapture'), NyxCallbackID('list.selection'));
  NyxCallbacks(LTasksTable).OnSelectionChange
    .Add(NyxHandler('TSelectionCapture'), NyxCallbackID('table.selection'));
  NyxCallbacks(LTasksTree).OnSelectionChange
    .Add(NyxHandler('TSelectionCapture'), NyxCallbackID('tree.selection'));
  ValidateNyxDocumentProperties(Result);
end;

function TSelectionProbe.Resolve(const AHandler: TNyxHandlerRef): INyxEventCallback;
begin
  Result := Self as INyxEventCallback;
end;

procedure TSelectionProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(Calls);
  Last := AEvent.Copy;
  Check(AEvent.HasCollectionSelection and
    not NyxEventResponse(AExecution).CanConsume, 'selection observes published owned data');

  if Navigate then
  begin
    Renderer.Unmount;
  end;
end;

procedure TKeyConsumption.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  NyxEventResponse(AExecution).Consume;
end;

procedure ModelChecks;
var
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LOther: INyxCollectionView;
  LSelection: INyxCollectionSelection;
  LSnapshot: TNyxCollectionSelectionSnapshot;
  LSingle: TNyxCollectionViewSpec;
  LMultiple: TNyxCollectionViewSpec;
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LBad: Boolean;
  LEvents: TNyxEventSchemas;
  LIndex: Integer;
  LFound: Boolean;
  LSession: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LValue: TNyxDataValue;
  LPair: TNyxProjectPair;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LHandler: TNyxHandlerRef;
  LLine: Integer;
begin
  LSnapshot := Default(TNyxCollectionSelectionSnapshot);
  Check(not LSnapshot.Defined and (LSnapshot.Count = 0), 'default snapshot is absent');
  LStore := Store;
  LSingle := NyxCollectionView(Key).Column(NyxTextField('caption'), 'Task');
  LMultiple := LSingle.Selection(nsmMultiple);
  Check((LSingle.SelectionMode = nsmSingle) and
    (LMultiple.SelectionMode = nsmMultiple), 'fluent choices retain their baseline');
  Check(LSingle.ToData.Field('version').AsInteger = 1, 'single wire remains canonical version 1');
  Check((LMultiple.ToData.Field('version').AsInteger = 2) and
    (TNyxCollectionViewSpec.FromData(LMultiple.ToData).SelectionMode = nsmMultiple),
    'multiple mode has an explicit versioned round trip');
  LView := NewNyxCollectionView(LStore, LMultiple, cpList);
  LOther := NewNyxCollectionView(LStore, LSingle, cpList);
  LView.Select(Row(0));
  LView.Select(Row(2), nsaToggle);
  Check((LView.Selection.Count = 2) and LView.Selection.Contains(Row(0)) and
    LView.Selection.Contains(Row(2)), 'discontiguous identity membership');
  Check((LView.Selection.Focus.ID = Row(2).ID) and
    (LView.Selection.Anchor.ID = Row(2).ID), 'focus and anchor are typed identities');
  LSelection := LView.Selection;
  LSnapshot := LSelection.Snapshot;
  LView.Select(Row(1), nsaFocus);
  Check((LView.Selection.Count = 2) and not LView.Selection.Contains(Row(1)) and
    (LView.Selection.Focus.ID = Row(1).ID), 'focus-only movement preserves membership');
  Check(LSnapshot.Focus.ID = Row(2).ID, 'retained event value does not observe later focus');
  LView.Select(Row(3), nsaRange);
  Check((LView.Selection.Count = 2) and LView.Selection.Contains(Row(2)) and
    LView.Selection.Contains(Row(3)), 'range follows its anchor rather than focus');
  LView.SelectRange(Row(0), [Row(0), Row(2), Row(3)], False);
  Check((LView.Selection.Count = 2) and not LView.Selection.Contains(Row(1)),
    'visible ranges exclude hidden identities');
  LView.SelectAll;
  Check(LView.Selection.Count = 4, 'select all has complete identity membership');
  LOther.Select(Row(1));
  Check((LOther.Selection.Count = 1) and (LView.Selection.Count = 4),
    'views on the same store have independent selection state');
  LSelection := LView.Selection;
  LBad := False;
  try
    LView.SetSelection([Row(0), Row(0)], Row(0), Row(0));
  except
    on ENyxCollection do
    begin
      LBad := True;
    end;
  end;
  Check(LBad and LView.Selection.SameState(LSelection), 'duplicate candidate rejects atomically');
  LBad := False;
  try
    LView.Select(NyxItem(NyxCollection('other'), Row(0).ID));
  except
    on ENyxCollection do
    begin
      LBad := True;
    end;
  end;
  Check(LBad and LView.Selection.SameState(LSelection), 'foreign collection refuses matching ID');
  LBad := False;
  try
    LOther.SetSelection([Row(0), Row(1)], Row(0), Row(0));
  except
    on ENyxCollection do
    begin
      LBad := True;
    end;
  end;
  Check(LBad and (LOther.Selected.ID = Row(1).ID), 'single mode rejects multiple membership');
  LStore.Move(Row(0), 3);
  Check((LView.Selection.Count = 4) and (LView.Selection.ItemAt(3).ID = Row(0).ID),
    'move preserves membership and publishes current dataset order');
  LStore.Remove(Row(0));
  Check((LView.Selection.Count = 3) and LSnapshot.Contains(Row(0)),
    'removal prunes live membership without changing retained snapshots');
  LView.ClearSelection;
  Check(not LView.HasSelection and not LView.Selection.Focus.Defined,
    'clear removes membership and optional cursor');
  LDocument := Fixture;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LSource := LWorkspace.Render(LDocument);
    Check((Pos('.Selection(nsmMultiple)', LSource) > 0) and
      (Pos('.OnSelectionChange', LSource) > 0), 'crafted typed binding and callback source');
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    try
      Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDocument),
        'source reconstruction preserves modes and authored registrations');
    finally
      LCandidate.Free;
    end;
    LDecoded := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
    try
      Check(LDecoded.Find('tasks-list').CollectionView.SelectionMode = nsmMultiple,
        'document codec preserves multiple selection without runtime membership');
    finally
      LDecoded.Free;
    end;
    LEvents := NyxEventsMetadata(LDocument.Find('task-browser'), LDocument);
    LFound := False;
    for LIndex := 0 to High(LEvents) do
    begin

      if LEvents[LIndex].Trigger = ntSelectionChange then
      begin
        LFound := (LEvents[LIndex].Browser = ncAvailable) and
          (LEvents[LIndex].Native = ncAvailable) and (LEvents[LIndex].Description <> '');
      end;
    end;
    Check(LFound, 'compound discovers its actual bound parts selection capability');
    LSession := TNyxStudioSession.Create;
    LAgent := TNyxAgentSession.Create;
    try
      LSession.Load(TNyxCodec.Encode(LDocument));
      LSession.Select('tasks-list');
      LBefore := LSession.Source;
      LHandler := LSession.AddCallback(ntSelectionChange, LLine);
      LAfter := LSession.Source;
      Check((LLine > 0) and (Pos(LHandler.Name, LAfter) > 0) and
        (Pos('TODO', LAfter) > 0), 'Studio creates a navigable selection handler template');
      LSession.Undo;
      Check(LSession.Source = LBefore, 'callback undo restores exact source');
      LSession.Redo;
      Check(LSession.Source = LAfter, 'callback redo restores exact source and handler identity');
      LPair := LSession.ProjectSnapshot;
      LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
        NyxField('expectedRevision', NyxData(LAgent.Revision)),
        NyxField('project', NyxData(EncodeNyxProject(LPair))),
        NyxField('selection', NyxData('tasks-list')), NyxField('view', NyxData('home'))]));
      LValue := LAgent.Call('nyx_node', 'selection qualification', NyxObject([
        NyxField('events', NyxData(True)), NyxField('limit', NyxData(1)),
        NyxField('eventLimit', NyxData(50))]));
      LFound := False;
      for LIndex := 0 to LValue.Field('events').Count - 1 do
      begin

        if LValue.Field('events').Item(LIndex).Field('trigger').AsText = 'selection-change' then
        begin
          LFound := (LValue.Field('events').Item(LIndex).Field('browser').AsText =
            NyxCapabilityText(ncAvailable)) and
            (LValue.Field('events').Item(LIndex).Field('native').AsText =
            NyxCapabilityText(ncAvailable));
        end;
      end;
      Check(LFound and (LValue.Field('properties').Count = 1),
        'bounded semantic agent discovery exposes actual selection support');
    finally
      LAgent.Free;
      LSession.Free;
    end;
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
  LSelection := nil;
  LView := nil;
  LOther := nil;
  LStore := nil;
  Check(LSnapshot.Contains(Row(0)) and (LSnapshot.Count = 2),
    'owned snapshot survives every runtime owner');
end;

procedure ControlChecks;
const
  CIDs: array[0..2] of TNyxText = ('tasks-list', 'tasks-table', 'tasks-tree');
var
  LDocument: TNyxDocument;
  LView: INyxCollectionView;
  LProbe: TSelectionProbe;
  LFactory: INyxCallbackFactory;
  LCallback: INyxEventCallback;
  LIndex: Integer;
  LBefore: Integer;
  LSource: TNyxText;
  LSelection: INyxCollectionSelection;
  LConsume: INyxEventCallback;
  LToken: INyxEventSubscription;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LControl: TJSHTMLElement;
  LCellInput: TJSHTMLInputElement;
  LCellOptions: TJSObject;
  LNumberInput: TJSHTMLInputElement;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LControl: TWinControl;
  LGrid: TStringGrid;
  LCellInput: TCustomEdit;
  LInputKey: Word;
  LNumberDraft: String;
  {$endif}

  procedure Press(AKey: TNyxKey; AShift, AControl: Boolean);
  {$ifdef PAS2JS}
  var
    LOptions: TJSObject;
    LEvent: TJSKeyboardEvent;
    LElement: TJSHTMLElement;
    LKey: String;
  begin
    LKey := 'ArrowDown';
    case AKey of
      nkSpaceKey:
        begin
          LKey := ' ';
        end;
      nkAKey:
        begin
          LKey := 'a';
        end;
      nkEndKey:
        begin
          LKey := 'End';
        end;
      nkLeftKey:
        begin
          LKey := 'ArrowLeft';
        end;
      nkRightKey:
        begin
          LKey := 'ArrowRight';
        end;
    end;
    LElement := TJSHTMLElement(LControl.querySelector('[tabindex="0"][data-nyx-item]'));

    if LElement = nil then
    begin
      { A disabled composite deliberately has no Tab entry. Deliver a forced
        key to its existing row to verify refusal independently of reachability. }
      LElement := TJSHTMLElement(LControl.querySelector('[data-nyx-item]'));
    end;
    LOptions := TJSObject.new;
    LOptions['key'] := LKey;
    LOptions['shiftKey'] := AShift;
    LOptions['ctrlKey'] := AControl;
    LOptions['bubbles'] := True;
    LOptions['cancelable'] := True;
    LEvent := TSelectionKeyEvent.new('keydown', LOptions);
    LElement.dispatchEvent(LEvent);

    if LRenderer.Root <> nil then
    begin
      LElement.dispatchEvent(TSelectionKeyEvent.new('keyup', LOptions));
    end;
  end;
  {$else}
  var
    LKey: Word;
    LReleaseKey: Word;
    LShift: TShiftState;
  begin
    LKey := VK_DOWN;
    case AKey of
      nkSpaceKey:
        begin
          LKey := VK_SPACE;
        end;
      nkAKey:
        begin
          LKey := VK_A;
        end;
      nkEndKey:
        begin
          LKey := VK_END;
        end;
      nkLeftKey:
        begin
          LKey := VK_LEFT;
        end;
      nkRightKey:
        begin
          LKey := VK_RIGHT;
        end;
      nkF2Key:
        begin
          LKey := VK_F2;
        end;
    end;
    LShift := [];

    if AShift then
    begin
      Include(LShift, ssShift);
    end;

    if AControl then
    begin
      Include(LShift, ssCtrl);
    end;
    LReleaseKey := LKey;
    TControlAccess(LControl).KeyDown(LKey, LShift);

    if LRenderer.Root <> nil then
    begin
      TControlAccess(LControl).KeyUp(LReleaseKey, LShift);
    end;
  end;
  {$endif}

  procedure PointerRow(AIndex: Integer; AControl: Boolean);
  {$ifdef PAS2JS}
  var
    LElement: TJSHTMLElement;
    LOptions: TJSObject;
  begin
    LElement := TJSHTMLElement(LControl.querySelectorAll('[data-nyx-item]')[AIndex]);
    LOptions := TJSObject.new;
    LOptions['ctrlKey'] := AControl;
    LOptions['bubbles'] := True;
    LElement.dispatchEvent(TSelectionMouseEvent.new('click', LOptions));
  end;
  {$else}
  var
    LRect: TRect;
    LShift: TShiftState;
    LX: Integer;
    LY: Integer;
  begin
    { Execute the actual TStringGrid virtual mouse path, including hit testing,
      cell admission and its unchanged-cell case. No shared view call substitutes
      for the native pointer gesture. Other native widgets keep LCL selection. }
    LRect := TStringGrid(LControl).CellRect(0, AIndex + 1);
    LX := (LRect.Left + LRect.Right) div 2;
    LY := (LRect.Top + LRect.Bottom) div 2;
    LShift := [];

    if AControl then
    begin
      Include(LShift, ssCtrl);
    end;
    TControlAccess(LControl).MouseMove(LShift, LX, LY);
    TControlAccess(LControl).MouseDown(mbLeft, LShift + [ssLeft], LX, LY);
    TControlAccess(LControl).MouseUp(mbLeft, LShift, LX, LY);
  end;
  {$endif}

begin
  {$ifdef NYX_COMPILED_SELECTION}
  LDocument := nyx.selection.fixture.BuildNyxDocument;
  {$else}
  LDocument := Fixture;
  {$endif}
  LProbe := TSelectionProbe.Create;
  LFactory := LProbe;
  LCallback := LProbe;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('section'));
  document.body.appendChild(LHost);
  LRenderer := TNyxBrowserRenderer.Create;
  {$else}
  LHost := TForm.Create(nil);
  LHost.SetBounds(0, 0, 700, 700);
  LHost.Show;
  LRenderer := TNyxLCLRenderer.Create;
  {$endif}
  try
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LProbe.Renderer := LRenderer;
    BindNyxCallbacks(LDocument, LRenderer.Events, LFactory);
    LSource := TNyxCodegen.Generate(LDocument);
    for LIndex := 0 to 2 do
    begin
      LView := LRenderer.CollectionView(CIDs[LIndex]);
      Check(LView.Selection.Count = 0, 'mounting preserves empty membership: ' +
        CIDs[LIndex] + ' count=' + IntToStr(LView.Selection.Count));
      {$ifdef PAS2JS}LControl := LRenderer.ElementFor(CIDs[LIndex]);
      Check(LControl.getAttribute('aria-multiselectable') = 'true', 'multiple role is exposed');
      {$else}LControl := TWinControl(LRenderer.ControlFor(CIDs[LIndex]));
      LControl.HandleNeeded;
      {$endif}
      Check(LView.Selection.Count = 0, 'creating the widget preserves empty membership: ' +
        CIDs[LIndex] + ' count=' + IntToStr(LView.Selection.Count));
      LBefore := LProbe.Calls;
      LView.Select(Row(0));
      Check(LProbe.Calls = LBefore + 1, 'one authored callback per actual selection publication: ' +
        CIDs[LIndex] + ' before=' + IntToStr(LBefore) + ' after=' + IntToStr(LProbe.Calls));
      Check((LProbe.Last.SourceID = 'task-browser') and
        (LProbe.Last.OriginID = CIDs[LIndex]), 'bound part routes to its semantic compound');

      if LIndex = 1 then
      begin
        {$ifdef PAS2JS}
        LCellInput := TJSHTMLInputElement(LControl.querySelector('input'));
        LCellInput.focus;
        LCellInput.value := 'A cell draft / 🌙';
        LCellInput.click;
        LView.Select(Row(0), nsaToggle);
        Check((document.activeElement = LCellInput) and
          (LCellInput.value = 'A cell draft / 🌙'),
          'row selection preserves the focused cell editor and its draft');
        LView.Store.Update(NyxCollectionItem(Row(1))
          .WithValue(NyxIntegerField('priority'), 2));
        Check((document.activeElement = LCellInput) and
          (LCellInput.value = 'A cell draft / 🌙'),
          'an unrelated item publication preserves the active browser draft');
        LSelection := LView.Selection;
        LCellInput.setSelectionRange(3, 3);
        LCellOptions := TJSObject.new;
        LCellOptions['key'] := 'ArrowLeft';
        LCellOptions['bubbles'] := True;
        LCellInput.dispatchEvent(TSelectionKeyEvent.new('keydown', LCellOptions));
        Check(LView.Selection.SameState(LSelection) and
          (document.activeElement = LCellInput),
          'cell editor owns its text-navigation key');
        {$else}
        LGrid := TStringGrid(LControl);
        LGrid.Col := 0;
        LGrid.SetFocus;
        Press(nkF2Key, False, False);
        Check(LGrid.Editor <> nil, 'native editable grid supplies its actual cell editor');
        LCellInput := TCustomEdit(LGrid.Editor);
        LCellInput.Text := 'A cell draft';
        LView.Select(Row(0), nsaToggle);
        Check(LGrid.EditorMode and (LCellInput.Text = 'A cell draft'),
          'row selection preserves the active native cell editor and its draft');
        LView.Store.Update(NyxCollectionItem(Row(1))
          .WithValue(NyxIntegerField('priority'), 2));
        Check(LGrid.EditorMode and (LCellInput.Text = 'A cell draft'),
          'an unrelated item publication preserves the active native draft');
        LSelection := LView.Selection;
        { Horizontal arrows belong to text editing. LCL's Up/Down editor path
          and horizontal keys at text boundaries navigate cells. Place the caret
          inside the draft to exercise actual text editing on both platforms. }
        LCellInput.SelStart := 3;
        LCellInput.SelLength := 0;
        LInputKey := VK_LEFT;
        TControlAccess(TWinControl(LCellInput)).KeyDown(LInputKey, []);
        Check(LView.Selection.SameState(LSelection),
          'active native editor owns its text-navigation key: before=' +
          IntToStr(LSelection.Count) + ' after=' + IntToStr(LView.Selection.Count) +
          ' editor=' + BoolToStr(LGrid.EditorMode, True));
        LGrid.EditorMode := False;
        {$endif}
        LView.Select(Row(0));
        {$ifdef PAS2JS}
        LNumberInput := TJSHTMLInputElement(LControl
          .querySelector('input[data-nyx-column="1"]'));
        LNumberInput.value := '01';
        LNumberInput.dispatchEvent(TJSEvent.new('change'));
        Check((LNumberInput.value = '1') and
          (LView.Store.Snapshot.Item(Row(0)).GetValue(NyxIntegerField('priority')) = 1),
          'no-op typed edit still normalizes its actual browser display');
        Check((document.activeElement = LCellInput) and
          (LCellInput.value = 'A cell draft / 🌙'),
          'normalizing another cell preserves the active browser draft');
        {$else}
        LNumberDraft := '01';
        LGrid.OnValidateEntry(LGrid, 1, 1, '1', LNumberDraft);
        Check((LNumberDraft = '1') and
          (LView.Store.Snapshot.Item(Row(0)).GetValue(NyxIntegerField('priority')) = 1),
          'no-op typed edit still normalizes its actual native display');
        {$endif}
      end;
      Press(nkDownKey, False, True);
      Check((LView.Selection.Count = 1) and (LView.Selection.Focus.ID = Row(1).ID) and
        not LView.Selection.Contains(Row(1)), 'real control ctrl-arrow moves focus independently');
      Press(nkSpaceKey, False, True);
      Check((LView.Selection.Count = 2) and LView.Selection.Contains(Row(1)),
        'real control toggles focused membership');
      Press(nkEndKey, True, False);
      Check((LView.Selection.Count = 3) and not LView.Selection.Contains(Row(0)),
        'real control extends a range from the toggle anchor');
      Press(nkAKey, False, True);
      Check(LView.Selection.Count = 4, 'real control select-all');
      Check(LProbe.Last.SelectionBefore.Defined and LProbe.Last.Selection.Defined and
        (LProbe.Last.Selection.Count = 4), 'event owns both accepted selection states');
      Check(TNyxCodegen.Generate(LDocument) = LSource, 'runtime selection leaves authored source intact');
      LBefore := LProbe.Calls;
      LView.SelectAll;
      Check(LProbe.Calls = LBefore, 'unchanged selection emits no duplicate callback');
      LView.ClearSelection;
      LView.Select(Row(0));
      LSelection := LView.Selection;
      LConsume := TKeyConsumption.Create;
      LToken := LRenderer.Events.OnBeforeKeyDown(NyxControlEvents(CIDs[LIndex]))
        .Subscribe(LConsume);
      Press(nkDownKey, False, False);
      Check(LView.Selection.SameState(LSelection),
        'consumed canonical key cannot execute a collection default');
      LToken.Cancel;
      LToken := nil;
      LConsume := nil;
      LRenderer.Root.Find(CIDs[LIndex]).Configure.ReadOnly(True).Done;
      LRenderer.Sync;
      Press(nkDownKey, False, False);
      Check(LView.Selection.Focus.ID = Row(1).ID,
        'read-only data still permits keyboard selection');
      LRenderer.Root.Find(CIDs[LIndex]).Configure.Enabled(False).Done;
      LRenderer.Sync;
      LSelection := LView.Selection;
      Press(nkDownKey, False, False);
      Check(LView.Selection.SameState(LSelection),
        'disabled control refuses a forced keyboard gesture');
      LRenderer.Root.Find(CIDs[LIndex]).Configure.Enabled(True).ReadOnly(False).Done;
      LRenderer.Sync;
      {$ifndef PAS2JS}

      if LIndex = 1 then
      {$endif}
      begin
        LView.ClearSelection;
        PointerRow(0, False);
        PointerRow(2, True);
        Check((LView.Selection.Count = 2) and LView.Selection.Contains(Row(0)) and
          LView.Selection.Contains(Row(2)), 'real pointer adds discontiguous membership');
        PointerRow(2, True);
        Check((LView.Selection.Count = 1) and not LView.Selection.Contains(Row(2)),
          'ctrl-click toggles the current cursor row without duplicate selection');
      end;
      LView.ClearSelection;

      if LIndex = 2 then
      begin
        LView.Store.Update(NyxCollectionItem(Row(1))
          .WithValue(NyxTextField('parent'), Row(0).ID));
        LView.Select(Row(0));
        Press(nkRightKey, False, False);
        Press(nkDownKey, False, False);
        Check(LView.Selection.Focus.ID = Row(1).ID,
          'tree right expands and down visits its visible child');
        Press(nkLeftKey, False, False);
        Check(LView.Selection.Focus.ID = Row(0).ID,
          'tree left navigates from a leaf to its parent');
        Press(nkLeftKey, False, False);
        Press(nkEndKey, True, False);
        Check((LView.Selection.Count = 3) and not LView.Selection.Contains(Row(1)),
          'actual tree range excludes a collapsed child');
        LView.ClearSelection;
      end;
    end;
    LView.Select(Row(0));
    LProbe.Navigate := True;
    Press(nkEndKey, False, False);
    Check(LProbe.Last.Selection.Contains(Row(3)),
      'real keyboard navigation teardown retains exact event identities');
    LBefore := LProbe.Calls;
    LView.Select(Row(1));
    Check(LProbe.Calls = LBefore, 'navigation revokes collection routes');
  finally
    LProbe.Renderer := nil;
    LRenderer.Free;
    {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
    LCallback := nil;
    LFactory := nil;
    LToken := nil;
    LConsume := nil;
    LSelection := nil;
    LView := nil;
    LDocument.Free;
  end;
end;

{ Physical focus belongs to the mounted control, while focus identity and
  membership belong to the portable view. Dataset replacement must reconcile
  both without selecting an unintended item or publishing duplicate callbacks. }
procedure FocusChecks;
const
  CIDs: array[0..2] of TNyxText = ('tasks-list', 'tasks-table', 'tasks-tree');
var
  LDocument: TNyxDocument;
  LView: INyxCollectionView;
  LProbe: TSelectionProbe;
  LFactory: INyxCallbackFactory;
  LRows: INyxCollectionSnapshot;
  LIndex: Integer;
  LRow: Integer;
  LBefore: Integer;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LControl: TJSHTMLElement;
  LFocus: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LEditor: TJSHTMLInputElement;
  LVersion: Integer;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LControl: TWinControl;
  {$endif}

  {$ifdef PAS2JS}
  function EditorPress(AElement: TJSHTMLElement; AKey: TNyxKey;
    AShift: Boolean = False): Boolean;
  var
    LOptions: TJSObject;
    LKey: String;
    LEvent: TJSKeyboardEvent;
  begin
    case AKey of
      nkF2Key:
        begin
          LKey := 'F2';
        end;
      nkEscapeKey:
        begin
          LKey := 'Escape';
        end;
      nkTabKey:
        begin
          LKey := 'Tab';
        end;
      else
        begin
          raise ENyxModel.Create('This editor fixture requires F2, Escape or Tab');
        end;
    end;
    LOptions := TJSObject.new;
    LOptions['key'] := LKey;
    LOptions['shiftKey'] := AShift;
    LOptions['bubbles'] := True;
    LOptions['cancelable'] := True;
    LEvent := TSelectionKeyEvent.new('keydown', LOptions);
    AElement.dispatchEvent(LEvent);
    Result := LEvent.defaultPrevented;
  end;
  {$endif}
begin
  {$ifdef NYX_COMPILED_SELECTION}
  LDocument := nyx.selection.fixture.BuildNyxDocument;
  {$else}
  LDocument := Fixture;
  {$endif}
  LProbe := TSelectionProbe.Create;
  LFactory := LProbe;
  LRows := Store.Snapshot;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('section'));
  document.body.appendChild(LHost);
  LRenderer := TNyxBrowserRenderer.Create;
  {$else}
  LHost := TForm.Create(nil);
  LHost.SetBounds(0, 0, 700, 700);
  LHost.Show;
  LRenderer := TNyxLCLRenderer.Create;
  {$endif}
  try
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LProbe.Renderer := LRenderer;
    BindNyxCallbacks(LDocument, LRenderer.Events, LFactory);
    for LIndex := 0 to High(CIDs) do
    begin
      LView := LRenderer.CollectionView(CIDs[LIndex]);
      {$ifdef PAS2JS}
      LControl := LRenderer.ElementFor(CIDs[LIndex]);
      Check(LControl.getAttribute('aria-label') <> '', 'rich collection has its accessible name');
      LFocus := TJSHTMLElement(LControl.querySelector('[data-nyx-item="sketch"]'));
      LFocus.focus;
      {$else}
      LControl := TWinControl(LRenderer.ControlFor(CIDs[LIndex]));
      Check(LControl.AccessibleName <> '', 'native rich collection has its accessible name');
      LControl.SetFocus;
      {$endif}
      LView.Select(Row(1));
      LBefore := LProbe.Calls;
      LView.Store.Remove(Row(1));
      Check((LView.Selection.Focus.ID = Row(2).ID) and (LView.Selection.Count = 0),
        'removal retains next focus identity without selecting a replacement');
      Check(LProbe.Calls = LBefore + 1, 'removal publishes exactly one selection callback');
      {$ifdef PAS2JS}
      LFocus := TJSHTMLElement(LControl.querySelector('[data-nyx-item="build"]'));
      Check(document.activeElement = LFocus, 'removed focused row transfers physical browser focus');
      Check(LControl.querySelectorAll('[data-nyx-item][tabindex="0"]').length = 1,
        'rich collection has exactly one row tab stop');

      if LIndex = 1 then
      begin
        Check(LControl.querySelector('input:not([tabindex="-1"])') = nil,
          'grid editors do not create extra page Tab entries');
      end;
      {$else}
      Check(LControl.Focused, 'row removal retains native control focus');
      {$endif}
      LView.Store.Apply([NyxRemove(Row(0)), NyxRemove(Row(2)), NyxRemove(Row(3))]);
      Check((LView.Snapshot.Count = 0) and not LView.Selection.Focus.Defined,
        'empty dataset has no manufactured selection focus');
      {$ifdef PAS2JS}
      Check((document.activeElement = LControl) and (LControl.getAttribute('tabindex') = '0'),
        'empty composite retains a keyboard entry and physical focus');
      {$else}
      Check(LControl.Focused, 'empty native composite retains physical focus');
      {$endif}
      for LRow := 0 to LRows.Count - 1 do
      begin
        LView.Store.Insert(LRow, LRows.ItemAt(LRow));
      end;
      LView.ClearSelection;
      LRenderer.Root.Find(CIDs[LIndex]).Configure.Enabled(False).Done;
      LRenderer.Sync;
      {$ifdef PAS2JS}
      Check((LControl.querySelectorAll('[tabindex="0"]').length = 0) and
        (LControl.getAttribute('tabindex') <> '0'), 'disabled collection has no tab stops');
      {$else}
      Check(not LControl.CanFocus, 'disabled native collection cannot receive focus');
      {$endif}
      LRenderer.Root.Find(CIDs[LIndex]).Configure.Enabled(True).ReadOnly(True).Done;
      LRenderer.Sync;
      {$ifdef PAS2JS}

      if LIndex = 1 then
      begin
        LInput := TJSHTMLInputElement(LControl.querySelector('input'));
        Check(LInput.readOnly and not LInput.disabled,
          'read-only text cell remains focusable for selection and copying');
        LInput.focus;
        Check(document.activeElement = LInput, 'read-only text cell accepts actual focus');
        LFocus := TJSHTMLElement(LControl.querySelector('[data-nyx-item][tabindex="0"]'));
        LFocus.focus;
        LVersion := LView.Store.Snapshot.Revision;
        Check(EditorPress(LFocus, nkF2Key) and (document.activeElement = LInput),
          'F2 enters the read-only browser text editor for keyboard inspection');
        LEditor := TJSHTMLInputElement(LControl.querySelector('input[data-nyx-column="1"]'));
        Check(EditorPress(LInput, nkTabKey) and (document.activeElement = LEditor),
          'editing Tab reaches the next owned column');
        Check(EditorPress(LEditor, nkTabKey, True) and (document.activeElement = LInput),
          'editing Shift-Tab reaches the previous owned column');
        Check(EditorPress(LInput, nkEscapeKey) and (document.activeElement = LFocus),
          'Escape returns to row navigation without changing read-only data');
        Check(LView.Store.Snapshot.Revision = LVersion,
          'keyboard inspection changes no admitted collection values');
        LRenderer.Root.Find(CIDs[LIndex]).Configure.ReadOnly(False).Done;
        LRenderer.Sync;
        EditorPress(LFocus, nkF2Key);
        LInput.value := 'A draft that should not be admitted';
        Check(EditorPress(LInput, nkEscapeKey) and (document.activeElement = LFocus) and
          (LInput.value = LView.CellText(Row(0), 0)),
          'Escape discards the pending cell draft and retains physical row focus');
        Check(LView.Store.Snapshot.Revision = LVersion,
          'discarding a physical draft leaves the accepted store revision intact');
      end;
      {$else}
      Check(LControl.CanFocus, 'read-only native collection remains focusable');
      {$endif}
      LRenderer.Root.Find(CIDs[LIndex]).Configure.ReadOnly(False).Done;
      LRenderer.Sync;
    end;
  finally
    LProbe.Renderer := nil;
    LRenderer.Free;
    {$ifdef PAS2JS}
    LHost.remove;
    {$else}
    LHost.Free;
    {$endif}
    LFactory := nil;
    LView := nil;
    LRows := nil;
    LDocument.Free;
  end;
end;

{$ifndef PAS2JS}
procedure ExportFixture;
var
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LStream: TFileStream;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LDocument := Fixture;
  try
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.selection.fixture');
    LStream := TFileStream.Create(ParamStr(1), fmCreate);
    try
      LStream.WriteBuffer(LSource[1], Length(LSource));
    finally
      LStream.Free;
    end;
  finally
    LDocument.Free;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    ModelChecks;
    ControlChecks;
    FocusChecks;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' selection checks';
    document.body.setAttribute('data-selection-tests', 'passed');
    {$else}
    ExportFixture;
    WriteLn('PASS ', GChecks, ' selection checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-selection-tests', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end
    {$ifdef PAS2JS}
    else
    begin
      document.body.textContent := 'FAIL browser host: ' +
        String(TJSObject(JSExceptValue)['stack']);
      document.body.setAttribute('data-selection-tests', 'failed');
    end
    {$endif};
  end;
end.
