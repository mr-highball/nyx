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
unit nyx.test.collections.authoring;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses nyx.text, nyx.model;

{ Public-contract fixture used by both compilers and the real target adapters.
  Caller owns the returned document; source has no target-specific dependencies. }
function CreateNyxCollectionAuthoringFixture: TNyxDocument;
function RunNyxCollectionAuthoringTests: Integer;
function RunNyxCollectionAuthoringJourney: Integer;

implementation

uses
  SysUtils,
  nyx.types,
  nyx.behavior,
  nyx.state,
  nyx.data,
  nyx.contract,
  nyx.controls,
  nyx.codec,
  nyx.codegen,
  nyx.source,
  nyx.schema,
  nyx.composition,
  nyx.collections,
  nyx.collections.view,
  nyx.collections.view.types,
  nyx.collections.selection,
  nyx.collections.bindings,
  nyx.application.state,
  nyx.studio.session,
  nyx.studio.view,
  nyx.studio.commands,
  nyx.studio.authoring,
  {$ifdef PAS2JS}
  JS, Web, nyx.render.browser, nyx.application.browser;
  {$else}
  Forms, Controls, StdCtrls, Grids, nyx.widgets.lcl, nyx.render.lcl, nyx.application.lcl;
  {$endif}

function CreateNyxCollectionAuthoringFixture: TNyxDocument;
var
  LPage, LDefinition: INyxColumn;
  LSpec: TNyxCollectionViewSpec;
begin
  Result := TNyxDocument.Create;
  try
    Result.Collections.Define(NyxCollection('tasks'),
      NyxCollectionSchema.Text(NyxTextField('caption'), '')
        .Boolean(NyxBooleanField('done'), False)
        .Integer(NyxIntegerField('priority'), 1)
        .Text(NyxTextField('parent'), ''),
      [NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'root'))
         .WithValue(NyxTextField('caption'), 'Root 漢字 🌙'),
       NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'child'))
         .WithValue(NyxTextField('caption'), 'Child')
         .WithValue(NyxTextField('parent'), 'root')]);
    LSpec := NyxCollectionView(NyxCollection('tasks'))
      .Column(NyxTextField('caption'), 'Task', cmEditable)
      .Column(NyxBooleanField('done'), 'Done', cmEditable)
      .Column(NyxIntegerField('priority'), 'Priority', cmEditable);
    LDefinition := NewNyxColumn('task-set');
    LDefinition.Add(NewNyxList('items').Configure.PartName(NyxPart('items')).Done
      .Binds.Collection(LSpec.Scoped(csInstance)).Done);
    Result.AddComponent(LDefinition);
    LPage := NewNyxColumn('home');
    LPage.Add(NewNyxTable('tasks-table').Binds.Collection(LSpec).Done);
    LPage.Add(NewNyxComponent('left').Configure.Component(NyxComponent('task-set')).Done);
    LPage.Add(NewNyxComponent('right').Configure.Component(NyxComponent('task-set')).Done);
    Result.AddPage(LPage);
    LPage := NewNyxColumn('other');
    LPage.Add(NewNyxTree('tasks-tree').Binds.Collection(
      LSpec.Parent(NyxTextField('parent'))).Done);
    Result.AddPage(LPage);
    ValidateNyxDocumentProperties(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var AChecks: Integer);
begin

  if not ACondition then
  begin
    raise Exception.Create('Collection authoring: ' + AMessage);
  end;
  Inc(AChecks);
end;

function RunNyxCollectionAuthoringTests: Integer;
var
  LDocument, LDecoded, LCandidate, LLegacy, LIsolated: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LRuntime: TNyxApplicationState;
  LView, LRight, LTree: INyxCollectionView;
  LRoot: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LSource, LWire, LBefore: TNyxText;
  LRejected: Boolean;
begin
  Result := 0;
  LDocument := CreateNyxCollectionAuthoringFixture;
  LDecoded := nil;
  LCandidate := nil;
  LLegacy := nil;
  LIsolated := nil;
  LRoot := nil;
  LRuntime := nil;
  LSession := TNyxStudioSession.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LWire := TNyxCodec.Encode(LDocument);
    Check(TNyxDataValue.ParseJSON(LWire).Field('version').AsInteger = 3, 'typed bindings select v3', Result);
    LDecoded := TNyxCodec.Decode(LWire);
    Check(TNyxCodec.Encode(LDecoded) = LWire, 'v3 exact design round trip', Result);
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('INyxTable', LSource) > 0) and (Pos('.Collection(', LSource) > 0) and
      (Pos('cmEditable', LSource) > 0) and (Pos('csInstance', LSource) > 0),
      'crafted specialized source uses fluent typed collection values', Result);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = LWire, 'source reconstructs every saved binding', Result);
    FreeAndNil(LCandidate);

    LCandidate := TNyxDocument.Create;
    LCandidate.AddPage(NewNyxList('empty-list').Binds.ClearCollection.Done);
    LBefore := TNyxCodec.Encode(LCandidate);
    LIsolated := TNyxCodec.Decode(LBefore);
    Check((LIsolated.Collections.Count = 0) and LIsolated.Pages[0].HasCollectionView and
      not LIsolated.Pages[0].CollectionView.Defined and
      (TNyxCodec.Encode(LIsolated) = LBefore), 'clear-only v3 survives with no collection defaults', Result);
    FreeAndNil(LCandidate);
    FreeAndNil(LIsolated);
    LCandidate := LDocument.Clone;
    LCandidate.AddComponent(NewNyxColumn('outer-set').Add(NewNyxComponent('inner')
      .Configure.Component(NyxComponent('task-set')).Done));
    LCandidate.Find('home').Add(TNyxNode.Create(nkComponent, 'outer')
      .Configure.Component(NyxComponent('outer-set')).Done);
    LRoot := RealizeNyxView(LCandidate, LCandidate.Pages[0]);
    Check(LRoot.Find('outer/inner/items').InstanceScopeID = 'outer/inner/task-set',
      'nested reusable controls choose nearest independent owner', Result);
    FreeAndNil(LRoot);
    FreeAndNil(LCandidate);
    LBefore := StringReplace(LSource, 'cmEditable', '''editable''', []);
    LRejected := False;
    try
      LCandidate := LWorkspace.Candidate(LDocument, LBefore);
    except
      on ENyxSource do LRejected := True;
    end;
    Check(LRejected, 'source refuses string editability', Result);
    LBefore := StringReplace(LSource, 'NyxTextField(''caption'')', 'NyxBooleanField(''caption'')', []);
    LRejected := False;
    try
      LCandidate := LWorkspace.Candidate(LDocument, LBefore);
    except
      on Exception do LRejected := True;
    end;
    Check(LRejected, 'source refuses schema/column family mismatch', Result);

    LIsolated := CloneNyxViewDocument(LDocument, LDocument.Components[0]);
    LBefore := PrepareNyxCompanion(LDocument, LIsolated, LSource, True);
    LCandidate := LWorkspace.Candidate(LIsolated, LBefore);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LIsolated),
      'isolated reusable companion preserves typed bindings/defaults', Result);
    FreeAndNil(LCandidate);
    FreeAndNil(LIsolated);
    LIsolated := CloneNyxViewDocument(LDocument, LDocument.Pages[1]);
    LBefore := PrepareNyxCompanion(LDocument, LIsolated, LSource, True);
    LCandidate := LWorkspace.Candidate(LIsolated, LBefore);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LIsolated),
      'isolated page companion preserves tree hierarchy', Result);
    FreeAndNil(LCandidate);
    FreeAndNil(LIsolated);

    LCandidate := LDocument.Clone;
    LCandidate.Find('left').OverridePart(NyxPart('items')).Binds.ClearCollection.Done;
    LRoot := RealizeNyxView(LCandidate, LCandidate.Pages[0]);
    Check(LRoot.Find('left/items').HasCollectionView and
      not LRoot.Find('left/items').CollectionView.Defined and
      LRoot.Find('right/items').CollectionView.Defined,
      'part clear affects only chosen instance', Result);
    FreeAndNil(LRoot);
    LCandidate.Find('left').OverridePart(NyxPart('items')).Binds.InheritCollection.Done;
    LRoot := RealizeNyxView(LCandidate, LCandidate.Pages[0]);
    Check(LRoot.Find('left/items').CollectionView.Defined,
      'part inheritance restores definition binding', Result);
    FreeAndNil(LRoot);
    FreeAndNil(LCandidate);

    LLegacy := TNyxCodec.Decode('{"version":1,"title":"Legacy","pages":[{"kind":"list","id":"old","props":{},"children":[],"collectionView":{"intent":"🌙"}}],"components":[]}');
    Check(not LLegacy.HasCollectionViews and
      (LLegacy.Pages[0].Extensions.Value(NyxExtension('collectionView')).Field('intent').AsText = '🌙'),
      'v1 same-name node extension stays opaque', Result);
    LLegacy.Collections.Define(LDocument.Collections.Snapshot(NyxCollection('tasks')));
    LBefore := TNyxCodec.Encode(LLegacy);
    LCandidate := TNyxCodec.Decode(LBefore);
    Check(not LCandidate.HasCollectionViews and (TNyxCodec.Encode(LCandidate) = LBefore),
      'v2 same-name extension also remains opaque', Result);
    FreeAndNil(LCandidate);
    LBefore := TNyxCodec.Encode(LLegacy);
    LLegacy.Collections.Define(LDocument.Collections.Snapshot(NyxCollection('tasks')));
    LLegacy.Pages[0].Binds.Collection(LDocument.Find('tasks-table').CollectionView).Done;
    LRejected := False;
    try
      TNyxCodec.Encode(LLegacy);
    except
      on ENyxModel do LRejected := True;
    end;
    Check(LRejected, 'promotion refuses opaque collision', Result);

    LSession.Load(LWire);
    LSession.Select('tasks-table');
    LBefore := StringReplace(LSession.Source,
      '.Column(NyxTextField(''caption''), ''Task'', cmEditable)',
      '.Column(NyxTextField(''caption''), ''Ta'' + ''sk'', { keep my column note } cmEditable)', []);
    LSession.SetSourceDraft(LBefore);
    LSession.ApplySourceDraft;
    LSpec := LSession.Selected.CollectionView;
    LSession.SetCollectionView(LSpec.Scoped(csInstance));
    Check(LSession.Selected.CollectionView.Scope = csInstance, 'session admits typed binding edit', Result);
    Check((Pos('{ keep my column note }', LSession.Source) > 0) and
      (Pos('''Ta'' + ''sk''', LSession.Source) > 0),
      'visual scope edit preserves authored column expression/comment', Result);
    LBefore := LSession.Source;
    LSession.Undo;
    Check(LSession.Save = LWire, 'undo restores exact paired binding', Result);
    LSession.Redo;
    Check(LSession.Source = LBefore, 'redo restores exact companion', Result);
    LBefore := LSession.Save;
    LRejected := False;
    try
      LSession.RemoveCollection(NyxCollection('tasks'));
    except
      on Exception do LRejected := True;
    end;
    Check(LRejected and (LSession.Save = LBefore), 'used collection removal preserves accepted pair', Result);
    LSession.SetCollectionView(Default(TNyxCollectionViewSpec));
    Check(LSession.Selected.HasCollectionView and not LSession.Selected.CollectionView.Defined and
      (Pos('.ClearCollection', LSession.Source) > 0), 'explicit clear persists and generates', Result);
    LCandidate := LWorkspace.Candidate(LSession.Document, LSession.Source);
    Check(TNyxCodec.Encode(LCandidate) = LSession.Save, 'clear source is replayable without parentheses', Result);
    FreeAndNil(LCandidate);
    LSession.InheritCollectionView;
    Check(not LSession.Selected.HasCollectionView, 'inherit removes only local binding', Result);

    LRuntime := TNyxApplicationState.Create(LDocument);
    LView := LRuntime.PageCollections('home').ViewFor('left/items');
    LRight := LRuntime.PageCollections('home').ViewFor('right/items');
    LView.Edit(NyxItem(NyxCollection('tasks'), 'root'), 0, TNyxStateValue.FromText('Left only'));
    Check((LRight.CellText(NyxItem(NyxCollection('tasks'), 'root'), 0) = 'Root 漢字 🌙') and
      (LRuntime.Collections.Collection(NyxCollection('tasks')).Snapshot.ItemAt(0)
       .GetValue(NyxTextField('caption')) = 'Root 漢字 🌙'),
      'automatic reusable owners isolate stores', Result);
    LView.Select(NyxItem(NyxCollection('tasks'), 'child'));
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LRuntime.PageCollections('home').ValidateRoot(LRoot);
    Check((LRoot.Find('left/items').InstanceScopeID = 'left/task-set') and
      (LRoot.Find('right/items').InstanceScopeID = 'right/task-set'),
      'composition propagates nearest reusable owner', Result);
    LTree := LRuntime.PageCollections('other').ViewFor('tasks-tree');
    LRejected := False;
    try
      LRuntime.Collections.Collection(NyxCollection('tasks')).Apply([
        NyxUpdate(NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'root'))
          .WithValue(NyxTextField('parent'), 'child'))]);
    except
      on ENyxCollection do LRejected := True;
    end;
    Check(LRejected and (LTree.Snapshot.Revision = 0), 'hidden page rejects hierarchy cycle before publication', Result);
    FreeAndNil(LRuntime);
    Check(LView.HasSelection and (LView.Selected.ID = 'child') and
      (LView.CellText(NyxItem(NyxCollection('tasks'), 'root'), 0) = 'Left only'),
      'retained view safely outlives application', Result);
  finally
    LRoot.Free;
    LRuntime.Free;
    LWorkspace.Free;
    LIsolated.Free;
    LSession.Free;
    LLegacy.Free;
    LCandidate.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end;

type
  TAuthoringProbe = class
  public
    Session: TNyxStudioSession;
    Calls: Integer;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

procedure TAuthoringProbe.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if RouteNyxStudioAuthoring(Session, ANode, AEvent.Trigger) then
  begin
    Inc(Calls);
  end;
end;

function RunNyxCollectionAuthoringJourney: Integer;
var
  LDocument, LShell: TNyxDocument;
  LRuntime: TNyxApplicationState;
  LSession: TNyxStudioSession;
  LState: TNyxStudioViewState;
  LProbe: TAuthoringProbe;
  LView: INyxCollectionView;
  LBefore: TNyxText;
  LSelectionBefore: TNyxText;
  LSelectionSource: TNyxText;
  LSelectionAfter: TNyxText;
  {$ifdef PAS2JS}
  LRenderer, LEditor: TNyxBrowserRenderer;
  LApplication: TNyxBrowserApplication;
  LHost, LEditorHost: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  {$else}
  LRenderer, LEditor: TNyxLCLRenderer;
  LApplication: TNyxLCLApplication;
  LHost, LEditorHost: TForm;
  LDraft: String;
  LGrid: TStringGrid;
  {$endif}

  procedure Mount(const APage: TNyxText);
  begin
    {$ifdef PAS2JS}
    LRenderer.Render(LDocument, LDocument.Find(APage), LHost, False, LRuntime.State,
      LRuntime.PageCollections(APage));
    {$else}
    LRenderer.Render(LDocument, LDocument.Find(APage), LHost, LRuntime.State,
      LRuntime.PageCollections(APage));
    {$endif}
  end;

  procedure Shell;
  begin
    FreeAndNil(LShell);
    LShell := BuildNyxStudioView(LSession, LState);
    LEditor.Render(LShell, LShell.Pages[0], LEditorHost);
  end;

  procedure Click(const AID: TNyxText);
  begin
    {$ifdef PAS2JS}
    LEditor.ElementFor(AID).click;
    {$else}
    TNyxLCLButton(LEditor.ControlFor(AID)).Click;
    {$endif}
  end;

  procedure Change(const AID, AValue: TNyxText);
  {$ifndef PAS2JS}
  var
    LEdit: TCustomEdit;
    LChoice: TComboBox;
  {$endif}
  begin
    {$ifdef PAS2JS}
    LInput := TJSHTMLInputElement(LEditor.ElementFor(AID).querySelector('input,textarea,select'));
    LInput.value := AValue;
    LInput.dispatchEvent(TJSEvent.new('change'));
    {$else}

    if LEditor.InputFor(AID) is TComboBox then
    begin
      LChoice := TComboBox(LEditor.InputFor(AID));
      LChoice.ItemIndex := LChoice.Items.IndexOf(AValue);
      LChoice.OnChange(LChoice);
    end
    else
    begin
      LEdit := TCustomEdit(LEditor.InputFor(AID));
      LEdit.Text := AValue;

      if Assigned(LEdit.OnExit) then
      begin
        LEdit.OnExit(LEdit);
      end;
    end;
    {$endif}
  end;

begin
  Result := 0;
  LDocument := CreateNyxCollectionAuthoringFixture;
  LRuntime := TNyxApplicationState.Create(LDocument);
  LSession := TNyxStudioSession.Create;
  LShell := nil;
  LApplication := nil;
  LProbe := TAuthoringProbe.Create;
  LProbe.Session := LSession;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  LEditorHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  document.body.appendChild(LEditorHost);
  LRenderer := TNyxBrowserRenderer.Create;
  LEditor := TNyxBrowserRenderer.Create;
  LEditor.OnEvent := @LProbe.Event;
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(0, 0, 900, 900);
  LEditorHost := TForm.CreateNew(nil);
  LEditorHost.SetBounds(0, 0, 900, 900);
  LRenderer := TNyxLCLRenderer.Create;
  LEditor := TNyxLCLRenderer.Create;
  LEditor.OnEvent := LProbe.Event;
  {$endif}
  try
    LBefore := TNyxCodec.Encode(LDocument);
    Mount('home');
    LView := LRenderer.CollectionView('tasks-table');
    {$ifdef PAS2JS}
    Check(LRenderer.ElementFor('tasks-table').querySelectorAll('[data-nyx-item]').length >= 2,
      'authored table mounts actual rows', Result);
    LInput := TJSHTMLInputElement(LRenderer.ElementFor('tasks-table').querySelector('input'));
    LInput.value := 'Edited from control';
    LInput.dispatchEvent(TJSEvent.new('change'));
    {$else}
    LGrid := TStringGrid(LRenderer.ControlFor('tasks-table'));
    Check((LGrid.RowCount = 3) and (TListBox(LRenderer.ControlFor('left/items')).Items.Count = 2),
      'authored table/list mount actual rows', Result);
    LDraft := 'Edited from control';
    LGrid.OnValidateEntry(LGrid, 0, 1, LGrid.Cells[0, 1], LDraft);
    {$endif}
    Check(LView.CellText(NyxItem(NyxCollection('tasks'), 'root'), 0) = 'Edited from control',
      'automatic physical editor updates shared collection', Result);
    LView.Select(NyxItem(NyxCollection('tasks'), 'child'));
    LRenderer.CollectionView('left/items').Edit(NyxItem(NyxCollection('tasks'), 'root'), 0,
      TNyxStateValue.FromText('Independent left'));
    Mount('other');
    Check(LRenderer.CollectionView('tasks-tree').CellText(NyxItem(NyxCollection('tasks'), 'root'), 0)
      = 'Edited from control', 'navigation shares admitted application data', Result);
    Mount('home');
    Check(LRenderer.CollectionView('tasks-table').HasSelection and
      (LRenderer.CollectionView('tasks-table').Selected.ID = 'child'),
      'selection survives navigation and control recreation', Result);
    Check((LRenderer.CollectionView('left/items').CellText(NyxItem(NyxCollection('tasks'), 'root'), 0)
      = 'Independent left') and (LRenderer.CollectionView('right/items').CellText(
      NyxItem(NyxCollection('tasks'), 'root'), 0) = 'Root 漢字 🌙'),
      'instance data survives navigation without sibling leakage', Result);
    LRenderer.Root.Configure.ReadOnly(True).Done;
    LRenderer.Sync;
    {$ifdef PAS2JS}
    LInput := TJSHTMLInputElement(LRenderer.ElementFor('tasks-table').querySelector('input'));
    Check(LInput.disabled and LInput.readOnly, 'ancestor read-only reaches generated editor', Result);
    LInput.value := 'Rejected read-only';
    LInput.dispatchEvent(TJSEvent.new('change'));
    {$else}
    LGrid := TStringGrid(LRenderer.ControlFor('tasks-table'));
    Check(not (goEditing in LGrid.Options), 'ancestor read-only reaches actual grid', Result);
    LDraft := 'Rejected read-only';
    LGrid.OnValidateEntry(LGrid, 0, 1, LGrid.Cells[0, 1], LDraft);
    {$endif}
    Check(LView.CellText(NyxItem(NyxCollection('tasks'), 'root'), 0) = 'Edited from control',
      'forced read-only physical callback cannot mutate data', Result);
    LRenderer.Root.Configure.ReadOnly(False).Enabled(False).Done;
    LRenderer.Sync;
    {$ifdef PAS2JS}
    LInput := TJSHTMLInputElement(LRenderer.ElementFor('tasks-table').querySelector('input'));
    Check(LInput.disabled, 'ancestor disabled reaches generated input', Result);
    LInput.value := 'Rejected disabled';
    LInput.dispatchEvent(TJSEvent.new('change'));
    {$else}
    Check(not LGrid.Enabled, 'ancestor disabled reaches actual grid', Result);
    LDraft := 'Rejected disabled';
    LGrid.OnValidateEntry(LGrid, 0, 1, LGrid.Cells[0, 1], LDraft);
    {$endif}
    Check(LView.CellText(NyxItem(NyxCollection('tasks'), 'root'), 0) = 'Edited from control',
      'forced disabled callback cannot mutate data', Result);
    {$ifdef PAS2JS}
    TJSHTMLElement(LRenderer.ElementFor('tasks-table').querySelector('[data-nyx-item="root"]')).click;
    Check(LView.HasSelection and (LView.Selected.ID = 'child'),
      'disabled physical selection preserves accepted item identity', Result);
    {$else}
    LRenderer.CollectionView('left/items').Select(NyxItem(NyxCollection('tasks'), 'root'));
    TListBox(LRenderer.ControlFor('left/items')).ItemIndex := -1;
    TListBox(LRenderer.ControlFor('left/items')).OnSelectionChange(
      LRenderer.ControlFor('left/items'), True);
    Check(LRenderer.CollectionView('left/items').HasSelection and
      (TListBox(LRenderer.ControlFor('left/items')).ItemIndex = 0),
      'disabled list deselection restores accepted item identity', Result);
    {$endif}
    {$ifdef PAS2JS}
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost, True);
    LInput := TJSHTMLInputElement(LRenderer.ElementFor('tasks-table').querySelector('input'));
    Check(LInput.disabled and LInput.readOnly, 'design preview editors cannot write runtime data', Result);
    {$endif}
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'runtime commands never change saved defaults', Result);
    {$ifdef PAS2JS}
    LApplication := TNyxBrowserApplication.Create;
    LRenderer.Unmount;
    LApplication.Run(LDocument, LHost);
    {$else}
    LApplication := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    {$endif}
    LView := LApplication.View.CollectionView('tasks-table');
    LView.Select(NyxItem(NyxCollection('tasks'), 'child'));
    LView.Edit(NyxItem(NyxCollection('tasks'), 'root'), 0, TNyxStateValue.FromText('Application edit'));
    LApplication.ShowPage('other');
    Check(LApplication.View.CollectionView('tasks-tree').CellText(
      NyxItem(NyxCollection('tasks'), 'root'), 0) = 'Application edit',
      'actual application ShowPage reuses runtime data', Result);
    LApplication.ShowPage('home');
    Check(LApplication.View.CollectionView('tasks-table').HasSelection and
      (LApplication.View.CollectionView('tasks-table').Selected.ID = 'child'),
      'actual application ShowPage preserves selection', Result);
    FreeAndNil(LApplication);
    Check(LView.HasSelection and (LView.CellText(NyxItem(NyxCollection('tasks'), 'root'), 0)
      = 'Application edit'), 'view survives actual application disposal', Result);

    LSession.Load(LBefore);
    LSession.Select('tasks-table');
    LState := DefaultNyxStudioViewState;
    LState.StateVisible := True;
    LState.BindingsVisible := True;
    Shell;
    LSelectionBefore := LSession.Save;
    LSelectionSource := LSession.Source;
    Change('collection-binding-selection', 'Multiple items');
    Check((LSession.Selected.CollectionView.SelectionMode = nsmMultiple) and
      (Pos('.Selection(nsmMultiple)', LSession.Source) > 0),
      'actual inspector authors typed multiple selection and adjacent source', Result);
    LSelectionAfter := LSession.Source;
    LSession.Undo;
    Check((LSession.Save = LSelectionBefore) and (LSession.Source = LSelectionSource),
      'selection choice undo restores the exact accepted design/source pair', Result);
    LSession.Redo;
    Check(LSession.Source = LSelectionAfter,
      'selection choice redo restores exact crafted source', Result);
    Shell;
    Change('collection-column-0-title', 'Work item');
    Check(LSession.Selected.CollectionView.ColumnAt(0).Title = 'Work item',
      'actual inspector edits typed column caption', Result);
    Shell;
    Change('collection-column-0-mode', 'Read only');
    Check(LSession.Selected.CollectionView.ColumnAt(0).Mode = cmReadOnly,
      'actual inspector edits enum column policy', Result);
    Shell;
    Change('collection-binding-scope', 'Reusable instance');
    Check(LSession.Selected.CollectionView.Scope = csInstance,
      'actual inspector edits enum data scope', Result);
    Check(LSession.Selected.CollectionView.SelectionMode = nsmMultiple,
      'column and scope edits preserve selection mode', Result);
    Shell;
    Change('collection-0-row-0-cell-0', 'Saved from Studio 🌙');
    Check(LSession.Document.Collections.Snapshot(NyxCollection('tasks')).ItemAt(0)
      .GetValue(NyxTextField('caption')) = 'Saved from Studio 🌙',
      'actual Studio row editor updates exact saved default', Result);
    LSession.DefineCollection(NyxCollection('tasks'),
      LSession.Document.Collections.Snapshot(NyxCollection('tasks')).Schema,
      [LSession.Document.Collections.Snapshot(NyxCollection('tasks')).ItemAt(0)
         .WithValue(NyxTextField('caption'), 'Saved' + #0 + '🌙'),
       LSession.Document.Collections.Snapshot(NyxCollection('tasks')).ItemAt(1)]);
    Shell;
    Change('collection-0-row-0-cell-0', NyxStudioStateEditorText(
      TNyxStateValue.FromText('Exact' + #0 + '🌙')));
    Check(LSession.Document.Collections.Snapshot(NyxCollection('tasks')).ItemAt(0)
      .GetValue(NyxTextField('caption')) = 'Exact' + #0 + '🌙',
      'actual escaped row editor preserves NUL and supplementary Unicode', Result);
    Shell;
    Click('collection-0-add-row');
    Check((LProbe.Calls >= 5) and (LSession.Document.Collections.Snapshot(NyxCollection('tasks')).Count = 3),
      'actual Studio button adds a saved row through shared router', Result);
    Shell;
    Click('collection-create');
    Check((LProbe.Calls >= 6) and (LSession.Document.Collections.Count = 2),
      'actual Studio button creates typed collection', Result);
    Shell;
    Click('collection-1-add-integer');
    Check(LSession.Document.Collections.Snapshot(NyxCollection('collection1')).Schema.Count = 2,
      'actual Studio button adds strongly typed field', Result);
    Shell;
    Click('collection-bind-1');
    Check(LSession.Selected.CollectionView.Key.Name = 'collection1',
      'actual inspector binds selected control', Result);
    Shell;
    Click('collection-binding-clear');
    Check(LSession.Selected.HasCollectionView and not LSession.Selected.CollectionView.Defined,
      'actual inspector explicitly unbinds selected control', Result);
    LSession.Undo;
    Check(LSession.Selected.CollectionView.Key.Name = 'collection1', 'Studio binding command is undoable', Result);
  finally
    LEditor.Free;
    LApplication.Free;
    LRenderer.Free;
    {$ifdef PAS2JS}
    LEditorHost.remove;
    LHost.remove;
    {$else}
    LEditorHost.Free;
    LHost.Free;
    {$endif}
    LShell.Free;
    LProbe.Free;
    LSession.Free;
    LRuntime.Free;
    LDocument.Free;
  end;
end;

end.
