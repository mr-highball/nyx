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
program nyx_tree_hierarchy_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.state, nyx.types, nyx.data, nyx.controls, nyx.model,
  nyx.codec, nyx.codegen, nyx.binding.types, nyx.collections, nyx.collections.view,
  nyx.collections.selection, nyx.collections.query, nyx.collections.view.types,
  nyx.collections.mount,
  {$ifdef NYX_COMPILED_TREE}nyx.generated.view,{$endif}
  {$ifndef PAS2JS}Classes, nyx.studio.edits, nyx.studio.collectionedits,
    nyx.studio.transactions, nyx.studio.stateedits, nyx.test.mcp.client,{$endif}
  {$ifndef NYX_TREE_SHARED_ONLY}
    {$ifdef PAS2JS}JS, Web, nyx.render.browser;
    {$else}Interfaces, Forms, Controls, ComCtrls, LCLType, nyx.render.lcl;{$endif}
  {$else}nyx.contract;{$endif}

type
  { Owns no view/tree. A subscription must disconnect before freeing this
    receiver. Reentry and post-publication failure are separate assertions. }
  TDisclosureProbe = class
  public
    Calls: Integer;
    TryReentry: Boolean;
    ReentryRefused: Boolean;
    Fail: Boolean;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;
  { Owns a revocable attachment, not a host widget. Disconnect during disclosure
    publication must safely remove the later renderer observer and restore host
    event slots, while the retained capability stays independently usable. }
  TDetachProbe = class
  public
    Mount: INyxCollectionMount;
    Calls: Integer;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;
  {$ifndef NYX_TREE_SHARED_ONLY}
  { Borrows a renderer owned by Controls until deliberate retirement. Its token
    disconnects before this callback receiver is released. }
  TRendererRetirement = class
  public
    {$ifdef PAS2JS}Renderer: TNyxBrowserRenderer;
    {$else}Renderer: TNyxLCLRenderer;{$endif}
    Calls: Integer;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;
  {$ifdef PAS2JS}
  TTreeDetails = class external name 'HTMLDetailsElement' (TJSHTMLElement)
    open: Boolean;
  end;
  TTreeKey = class external name 'KeyboardEvent' (TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TControlAccess = class(TWinControl);
  {$endif}
  {$endif}

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Tree hierarchy: ' + AReason);
  end;
  Inc(GChecks);
end;

function Key: TNyxCollectionRef;
begin
  Result := NyxCollection('handbook');
end;

function Item(const AID: TNyxText): TNyxItemRef;
begin
  Result := NyxItem(Key, AID);
end;

function Seed: INyxCollection;
begin
  Result := NewNyxCollection(Key, NyxCollectionSchema
    .Text(NyxTextField('caption'), '').Text(NyxTextField('parent'), ''), [
      NyxCollectionItem(Item('chapter')).WithValue(NyxTextField('caption'), 'First steps')
        .WithValue(NyxTextField('parent'), 'guide'),
      NyxCollectionItem(Item('docs')).WithValue(NyxTextField('caption'), 'Documentation'),
      NyxCollectionItem(Item('guide')).WithValue(NyxTextField('caption'), 'Guides')
        .WithValue(NyxTextField('parent'), 'docs'),
      NyxCollectionItem(Item('example')).WithValue(NyxTextField('caption'), 'Welcome example')
        .WithValue(NyxTextField('parent'), 'playground'),
      NyxCollectionItem(Item('playground')).WithValue(NyxTextField('caption'), 'Examples')]);
end;

function Binding: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(Key).Column(NyxTextField('caption'), 'Section', cmEditable)
    .Parent(NyxTextField('parent')).Selection(nsmMultiple);
end;

procedure TDisclosureProbe.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Calls);

  if TryReentry then
  begin
    try
      NyxTreeHierarchy(AView).CollapseAll;
    except
      on E: ENyxCollection do
      begin
        ReentryRefused := True;
      end;
    end;
  end;

  if Fail then
  begin
    raise Exception.Create('Expected observer refusal after publication');
  end;
end;

procedure TDetachProbe.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Calls);
  Mount.Disconnect;
end;

{$ifndef NYX_TREE_SHARED_ONLY}
procedure TRendererRetirement.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Calls);
  Renderer.Free;
  Renderer := nil;
end;
{$endif}

procedure Visible(const ATree: INyxTreeHierarchy; const AIDs: array of TNyxText);
var
  LItems: TNyxItemRefs;
  LIndex: Integer;
begin
  LItems := ATree.VisibleItems;
  Check(Length(LItems) = Length(AIDs), 'visible preorder count');
  for LIndex := 0 to High(AIDs) do
  begin
    Check(LItems[LIndex].ID = AIDs[LIndex], 'visible preorder identity ' + AIDs[LIndex]);
  end;
end;

procedure Shared;
var
  LStore: INyxCollection;
  LView, LOther: INyxCollectionView;
  LTree, LOtherTree: INyxTreeHierarchy;
  LToken: INyxCollectionViewSubscription;
  LProbe: TDisclosureProbe;
  LItems: TNyxItemRefs;
  LRetained: INyxCollectionSelection;
  LRevision: Integer;
  LCalls: Integer;
  LRejected: Boolean;
  LSchema: TNyxCollectionSchema;
  LRows: array of TNyxCollectionItem;
  LIndex: Integer;
begin
  LStore := Seed;
  LView := NewNyxCollectionView(LStore, Binding, cpTree);
  LTree := NyxTreeHierarchy(LView);
  LOther := NewNyxCollectionView(LStore, Binding, cpTree);
  LOtherTree := NyxTreeHierarchy(LOther);
  LRevision := LStore.Snapshot.Revision;
  LProbe := TDisclosureProbe.Create;
  LToken := LView.Subscribe(LProbe.Changed);
  try
    Visible(LTree, ['docs', 'playground']);
    Check(not LTree.IsExpanded(Item('docs')) and LTree.HasChildren(Item('docs')),
      'closed default is distinct from leaf');
    Check(not LTree.HasChildren(Item('chapter')), 'leaf has no disclosure');
    LTree.SetExpanded(Item('chapter'), True);
    Check(LProbe.Calls = 0, 'leaf change is silent');
    LTree.SetExpanded(Item('docs'), True).SetExpanded(Item('guide'), True);
    Visible(LTree, ['docs', 'guide', 'chapter', 'playground']);
    Visible(LOtherTree, ['docs', 'playground']);
    Check(LStore.Snapshot.Revision = LRevision, 'runtime disclosure never edits source data');
    LCalls := LProbe.Calls;
    LTree.SetExpanded(Item('docs'), True);
    Check(LProbe.Calls = LCalls, 'unchanged disclosure does not notify');
    LItems := LTree.VisibleItems;
    LItems[0] := Item('chapter');
    Check(LTree.VisibleItems[0].ID = 'docs', 'returned order has independent ownership');
    LView.SetSelection([Item('chapter'), Item('example')], Item('chapter'), Item('chapter'));
    LRetained := LView.Selection;
    LTree.SetExpanded(Item('docs'), False);
    Check((LView.Selection.Focus.ID = 'docs') and (LView.Selection.Anchor.ID = 'chapter') and
      (LView.Selection.Count = 2) and LView.Selection.Contains(Item('chapter')),
      'collapse returns cursor without changing membership or anchor');
    Check(LRetained.Focus.ID = 'chapter', 'retained selection is immutable');
    Check(LTree.IsExpanded(Item('guide')), 'collapsed ancestor retains descendant disclosure');
    LView.Select(Item('chapter'), nsaFocus);
    Check(LTree.IsExpanded(Item('docs')) and (LView.Selection.Focus.ID = 'chapter'),
      'explicit hidden focus reveals ancestors atomically');
    LView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxTextField('caption')).EqualTo('Examples')));
    Visible(LTree, ['playground']);
    LView.ConfigureQuery(NyxCollectionQuery);
    Check(LTree.IsExpanded(Item('docs')) and LTree.IsExpanded(Item('guide')),
      'filter displacement retains exact branch identities');
    LView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxTextField('caption')).EqualTo('Examples')));
    LTree.CollapseAll;
    LView.ConfigureQuery(NyxCollectionQuery);
    Visible(LTree, ['docs', 'playground']);
    LCalls := LProbe.Calls;
    LTree.ExpandAll;
    Check(LProbe.Calls = LCalls + 1, 'expand all publishes one group');
    Visible(LTree, ['docs', 'guide', 'chapter', 'playground', 'example']);
    LStore.Move(Item('playground'), 0);
    Check(LTree.IsExpanded(Item('docs')) and LTree.IsExpanded(Item('playground')),
      'move preserves disclosure by exact identity');
    LStore.Update(NyxCollectionItem(Item('guide')).WithValue(NyxTextField('parent'), 'playground'));
    Visible(LTree, ['playground', 'guide', 'chapter', 'example', 'docs']);
    LStore.Apply([NyxRemove(Item('chapter')), NyxRemove(Item('guide'))]);
    LStore.Append(NyxCollectionItem(Item('guide')).WithValue(NyxTextField('parent'), 'playground'));
    LStore.Append(NyxCollectionItem(Item('chapter')).WithValue(NyxTextField('parent'), 'guide'));
    Check(not LTree.IsExpanded(Item('guide')), 'removed identity starts closed on later recreation');
    LTree.SetExpanded(Item('guide'), True);
    LStore.Apply([NyxRemove(Item('guide')),
      NyxInsert(0, NyxCollectionItem(Item('guide')).WithValue(NyxTextField('parent'), 'playground')
        .WithValue(NyxTextField('caption'), 'A fresh guide'))]);
    Check(not LTree.IsExpanded(Item('guide')), 'grouped removal and recreation retires disclosure');
    LRejected := False;
    LCalls := LProbe.Calls;
    try
      LTree.SetExpanded(NyxItem(NyxCollection('foreign'), 'docs'), True);
    except
      on E: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LProbe.Calls = LCalls), 'foreign scoped disclosure refuses without publication');
    LProbe.TryReentry := True;
    LTree.SetExpanded(Item('guide'), True);
    Check(LProbe.ReentryRefused and LTree.IsExpanded(Item('guide')), 'reentrant command refuses');
    LProbe.TryReentry := False;
    LProbe.Fail := True;
    LRejected := False;
    try
      LTree.SetExpanded(Item('guide'), False);
    except
      on E: ENyxCollectionNotification do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LTree.IsExpanded(Item('guide')), 'observer error follows accepted publication');
    LToken.Disconnect;
    LView := nil;
    LStore := nil;
    Check(LTree.HasChildren(Item('guide')), 'capability retains its independent live view');
    LTree.CollapseAll;
    Visible(LTree, ['playground', 'docs']);
  finally
    LToken.Disconnect;
    LToken := nil;
    LProbe.Free;
  end;
  LRejected := False;
  try
    NyxTreeHierarchy(NewNyxCollectionView(Seed,
      NyxCollectionView(Key).Column(NyxTextField('caption'), 'Caption'), cpList));
  except
    on E: ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'list capability refuses');
  LSchema := NyxCollectionSchema.Text(NyxTextField('caption'), '')
    .Text(NyxTextField('parent'), '');
  SetLength(LRows, 4096);
  for LIndex := 0 to High(LRows) do
  begin
    LRows[LIndex] := NyxCollectionItem(Item(IntToStr(LIndex)));

    if LIndex > 0 then
    begin
      LRows[LIndex] := LRows[LIndex].WithValue(NyxTextField('parent'), IntToStr(LIndex - 1));
    end;
  end;
  LStore := NewNyxCollection(Key, LSchema, LRows);
  LView := NewNyxCollectionView(LStore, Binding, cpTree);
  LTree := NyxTreeHierarchy(LView);
  LTree.ExpandAll;
  LItems := LTree.VisibleItems;
  Check((Length(LItems) = 4096) and (LItems[4095].ID = '4095'), 'deep hierarchy uses iterative preorder');
  LView.Select(Item('4095'));
  LTree.CollapseAll;
  Check((LView.Selection.Focus.ID = '0') and LView.Selection.Contains(Item('4095')),
    'deep collapse returns cursor without recursive traversal');
  { Broader characters are dedicated qualification data, not starter text. }
  LStore := NewNyxCollection(Key, LSchema, [
    NyxCollectionItem(Item('é🚀')).WithValue(NyxTextField('caption'), 'Unicode branch'),
    NyxCollectionItem(Item('child')).WithValue(NyxTextField('parent'), 'é🚀')]);
  LView := NewNyxCollectionView(LStore, Binding, cpTree);
  LTree := NyxTreeHierarchy(LView);
  LTree.SetExpanded(Item('é🚀'), True);
  LStore.Move(Item('é🚀'), 1);
  Check(LTree.IsExpanded(Item('é🚀')), 'supplementary identity survives structural publication exactly');
end;

{$ifndef PAS2JS}
function Composition: TNyxDataValue;
var
  LLayout: INyxDesignPatch;
  LCollections: INyxCollectionPatch;
  LTransaction: INyxProjectTransaction;
  LStore: INyxCollection;
  LRows: array of TNyxCollectionItem;
  LIndex: Integer;
begin
  LLayout := ReadNyxDesignPatch(TNyxDataValue.ParseJSON(
    '[{"op":"create","kind":"page","id":"tree-review","root":"page","properties":{"gap":12,"padding":20}},' +
    '{"op":"create","kind":"heading","id":"tree-title","parent":"tree-review","properties":{"text":"Explore the handbook"}},' +
    '{"op":"create","kind":"label","id":"tree-help","parent":"tree-review","properties":{"text":"Open a section to explore its pages. Arrow keys navigate the tree."}},' +
    '{"op":"create","kind":"tree","id":"handbook-tree","parent":"tree-review","properties":{"height":320,"aria-label":"Handbook sections"}}]'));
  LStore := Seed;
  SetLength(LRows, LStore.Snapshot.Count);
  for LIndex := 0 to High(LRows) do
  begin
    LRows[LIndex] := LStore.Snapshot.ItemAt(LIndex);
  end;
  LCollections := NyxCollectionPatch([
    NyxDefineCollection(Key, LStore.Snapshot.Schema, LRows),
    NyxBindCollection(NyxBindingOwner('handbook-tree'), cpTree, Binding)]);
  LTransaction := NyxProjectTransaction([NyxDesignStep(LLayout), NyxCollectionStep(LCollections)]);
  Result := LTransaction.ToData;
end;

procedure WriteBytes(const AFile: String; const AText: TNyxText);
var
  LBytes: UTF8String;
  LFile: TFileStream;
begin
  LBytes := UTF8String(AText);
  LFile := TFileStream.Create(AFile, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LFile.WriteBuffer(LBytes[1], Length(LBytes));
    end;
  finally
    LFile.Free;
  end;
end;

{ Use one authenticated transport for a temporary review. This accepts an
  existing configuration, never creates a listener or edits enrollment/profiles.
  Public typed Pascal builds the grouped wire payload. Bounded source windows
  and paired Undo/Redo establish the exact semantic compiler companion. }
procedure Semantic(const AConfig, AOutput: String);
var
  LClient: TNyxMCPTestClient;
  LReview: TNyxText;
  LRevision: Integer;
  LPrimary: Integer;
  LInitial, LSource: TNyxText;
  LReply: TNyxDataValue;

  function Call(const AName: TNyxText; const AFields: array of TNyxDataField;
    AContext: Boolean = True): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
    LIndex: Integer;
    LPacket: TNyxDataValue;
  begin
    SetLength(LFields, Length(AFields) + Ord(AContext));
    for LIndex := 0 to High(AFields) do
    begin
      LFields[LIndex] := AFields[LIndex];
    end;

    if AContext then
    begin
      LFields[High(LFields)] := NyxField('review', NyxData(LReview));
    end;
    LPacket := LClient.Tool(AName, NyxObject(LFields));
    Check(not LPacket.Field('isError').AsBoolean, AName + ' refused: ' + LPacket.ToJSON);
    Result := LPacket.Field('structuredContent');
  end;

  function Source: TNyxText;
  var
    LReply, LLines: TNyxDataValue;
    LLine, LIndex: Integer;
  begin
    Result := '';
    LLine := 1;
    repeat
      LReply := Call('nyx_source', [NyxField('line', NyxData(LLine)), NyxField('count', NyxData(80))]);
      Check(LReply.Field('revision').AsInteger = LRevision, 'bounded source stays at one revision');
      LLines := LReply.Field('lines');
      Check(LLines.Count > 0, 'source window makes progress');
      for LIndex := 0 to LLines.Count - 1 do
      begin
        Result := Result + LLines.Item(LIndex).AsText + TNyxText(#10);
      end;
      Inc(LLine, LLines.Count);
    until LLine > LReply.Field('totalLines').AsInteger;
  end;

  procedure History(const AMode: TNyxText);
  begin
    Call('nyx_history', [NyxField('direction', NyxData(AMode)),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('tree-' + AMode))]);
    LRevision := Call('nyx_session', []).Field('revision').AsInteger;
  end;

begin

  if DirectoryExists(AOutput) then
  begin
    raise Exception.Create('Semantic export destination must be new');
  end;
  LClient := TNyxMCPTestClient.Create(AConfig, 'Scooty tree hierarchy companion');
  LReview := '';
  try
    LPrimary := Call('nyx_session', [], False).Field('revision').AsInteger;
    LReply := Call('nyx_reviews', [NyxField('mode', NyxData('create')),
      NyxField('base', NyxData('empty')), NyxField('label', NyxData('Tree hierarchy review')),
      NyxField('expectedRevision', NyxData(LPrimary)),
      NyxField('operationId', NyxData('tree-review-create'))], False);
    LReview := LReply.Field('review').AsText;
    LRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LInitial := Source;
    Call('nyx_transaction', [NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('tree-compose')), NyxField('operations', Composition)]);
    LRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LSource := Source;
    Check((Pos('INyxTree', LSource) > 0) and (Pos('NyxCollectionView', LSource) > 0),
      'specialized semantic tree companion');
    LReply := Call('nyx_collections', [NyxField('mode', NyxData('rows')),
      NyxField('key', NyxData('handbook')), NyxField('limit', NyxData(5))]);
    Check(LReply.Field('total').AsInteger = 5, 'bounded semantic dataset inspection');
    History('undo');
    Check(Source = LInitial, 'one paired Undo removes whole composition');
    Check(not Call('nyx_session', []).Field('canUndo').AsBoolean, 'composition has no hidden intermediate history');
    History('redo');
    Check(Source = LSource, 'one paired Redo restores exact source');
    ForceDirectories(AOutput);
    WriteBytes(IncludeTrailingPathDelimiter(AOutput) + 'nyx.generated.view.pas', LSource);
    Call('nyx_reviews', [NyxField('mode', NyxData('discard')), NyxField('review', NyxData(LReview)),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('tree-review-discard'))], False);
    LReview := '';
    Check(Call('nyx_session', [], False).Field('revision').AsInteger = LPrimary,
      'primary project remains untouched');
    WriteLn('PASS ', GChecks, ' semantic tree companion checks');
  finally
    { Even a refused grouped composition leaves no review or agent presence. }
    try

      if LReview <> '' then
      begin
        LRevision := Call('nyx_session', []).Field('revision').AsInteger;
        Call('nyx_reviews', [NyxField('mode', NyxData('discard')), NyxField('review', NyxData(LReview)),
          NyxField('expectedRevision', NyxData(LRevision)),
          NyxField('operationId', NyxData('tree-review-cleanup'))], False);
      end;
    finally
      try
        LClient.Close;
      finally
        LClient.Free;
      end;
    end;
  end;
end;
{$endif}

{$ifndef NYX_TREE_SHARED_ONLY}
procedure Controls;
var
  LDocument: TNyxDocument;
  LView: INyxCollectionView;
  LTree: INyxTreeHierarchy;
  LSource: TNyxText;
  LOriginal: TNyxText;
  LDetach: TDetachProbe;
  LRetire: TRendererRetirement;
  LToken: INyxCollectionViewSubscription;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost, LControl: TJSHTMLElement;
  LOptions: TJSObject;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LControl: TTreeView;
  LKey: Word;
  {$endif}

  procedure Physical(const AID: TNyxText; AExpanded: Boolean);
  begin
    {$ifdef PAS2JS}
    Check(TTreeDetails(LControl.querySelector('[data-nyx-item="' + AID + '"]')).open = AExpanded,
      'actual DOM disclosure ' + AID);
    {$else}
    { Native Items enumerate hierarchy preorder, whereas the source deliberately
      places children before parents. Resolve the fixture's unique caption. }
    Check(LControl.Items.FindNodeWithText(String(LView.CellText(Item(AID), 0))).Expanded = AExpanded,
      'actual native disclosure ' + AID);
    {$endif}
  end;

  procedure PressRight;
  begin
    {$ifdef PAS2JS}
    LOptions := TJSObject.new;
    LOptions['key'] := 'ArrowRight';
    LOptions['bubbles'] := True;
    LControl.dispatchEvent(TTreeKey.new('keydown', LOptions));
    {$else}
    LKey := VK_RIGHT;
    TControlAccess(TWinControl(LControl)).KeyDown(LKey, []);
    Check(LKey = 0, 'native collection consumes disclosure key');
    {$endif}
  end;

begin
  {$ifdef NYX_COMPILED_TREE}
  LDocument := nyx.generated.view.BuildNyxDocument;
  {$else}
  LDocument := TNyxDocument.Create;
  LDocument.Collections.Define(Seed.Snapshot);
  LDocument.AddPage(NewNyxColumn('tree-review'));
  LDocument.Pages[0].Add(NewNyxTree('handbook-tree'));
  LDocument.Find('handbook-tree').Binds.Collection(Binding).Done;
  {$endif}
  LOriginal := TNyxCodec.Encode(LDocument);
  LSource := TNyxCodegen.Generate(LDocument);
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  LRenderer := TNyxBrowserRenderer.Create;
  {$else}
  LHost := TForm.Create(nil);
  LHost.SetBounds(0, 0, 700, 600);
  LHost.Show;
  LRenderer := TNyxLCLRenderer.Create;
  {$endif}
  try
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LView := LRenderer.CollectionView('handbook-tree');
    LTree := NyxTreeHierarchy(LView);
    {$ifdef PAS2JS}LControl := LRenderer.ElementFor('handbook-tree');
    {$else}LControl := TTreeView(LRenderer.ControlFor('handbook-tree'));{$endif}
    Physical('docs', False);
    {$ifdef PAS2JS}
    Check(not LControl.querySelector('[data-nyx-item="chapter"]').hasAttribute('aria-expanded'),
      'actual leaf exposes no disclosure role');
    {$endif}
    LView.Select(Item('docs'));
    PressRight;
    Check(LTree.IsExpanded(Item('docs')), 'actual right key publishes typed disclosure');
    Physical('docs', True);
    PressRight;
    Check(LView.Selection.Focus.ID = 'guide', 'second right key moves to first child');
    LTree.ExpandAll;
    LView.Select(Item('chapter'));
    {$ifdef PAS2JS}
    TJSHTMLElement(LControl.querySelector('[data-nyx-item="chapter"] input')).focus;
    {$else}
    LControl.SetFocus;
    {$endif}
    LTree.SetExpanded(Item('docs'), False);
    Physical('docs', False);
    Check((LView.Selection.Focus.ID = 'docs') and LView.Selection.Contains(Item('chapter')),
      'actual collapsed tree has visible cursor and retained membership');
    {$ifdef PAS2JS}
    Check(TJSHTMLElement(document.activeElement).closest('[data-nyx-item]')
      .getAttribute('data-nyx-item') = 'docs', 'physical focus returns to visible branch');
    {$else}
    Check(LControl.Selected.Text = 'Documentation', 'native cursor returns to visible branch');
    {$endif}
    LView.Select(Item('chapter'), nsaFocus);
    Physical('docs', True);
    {$ifdef PAS2JS}
    TTreeDetails(LControl.querySelector('[data-nyx-item="docs"]')).open := False;
    LControl.querySelector('[data-nyx-item="docs"]').dispatchEvent(TJSEvent.new('toggle'));
    {$else}
    LControl.Items.FindNodeWithText('Documentation').Collapse(False);
    Application.ProcessMessages;
    {$endif}
    Check((LView.Selection.Focus.ID = 'docs') and LView.Selection.Contains(Item('chapter')),
      'physical collapse preserves membership and returns cursor');
    LView.Select(Item('chapter'), nsaFocus);
    LView.Store.Move(Item('playground'), 0);
    Physical('docs', True);
    LView.Store.Update(NyxCollectionItem(Item('guide')).WithValue(NyxTextField('parent'), 'playground'));
    Physical('guide', True);
    Physical('playground', True);
    LTree.CollapseAll;
    LRenderer.Root.Find('handbook-tree').Configure.Enabled(False).Done;
    LRenderer.Sync;
    {$ifdef PAS2JS}
    TTreeDetails(LControl.querySelector('[data-nyx-item="playground"]')).open := True;
    LControl.querySelector('[data-nyx-item="playground"]').dispatchEvent(TJSEvent.new('toggle'));
    {$else}
    LControl.Items.FindNodeWithText('Examples').Expand(False);
    Application.ProcessMessages;
    {$endif}
    Check(not LTree.IsExpanded(Item('playground')), 'disabled physical disclosure cannot mutate runtime state');
    Physical('playground', False);
    LRenderer.Root.Find('handbook-tree').Configure.Enabled(True).ReadOnly(True).Done;
    LRenderer.Sync;
    {$ifdef PAS2JS}
    TTreeDetails(LControl.querySelector('[data-nyx-item="playground"]')).open := True;
    LControl.querySelector('[data-nyx-item="playground"]').dispatchEvent(TJSEvent.new('toggle'));
    {$else}
    LControl.Items.FindNodeWithText('Examples').Expand(False);
    Application.ProcessMessages;
    {$endif}
    Check(LTree.IsExpanded(Item('playground')), 'physical host disclosure publishes portable state');
    LView.Store.Update(NyxCollectionItem(Item('docs')).WithValue(NyxTextField('caption'), 'Documentation updated'));
    Physical('playground', True);
    Check((TNyxCodec.Encode(LDocument) = LOriginal) and (TNyxCodegen.Generate(LDocument) = LSource),
      'runtime hierarchy leaves authored pair exact');
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    Check(not LTree.IsExpanded(Item('docs')) and LTree.IsExpanded(Item('playground')),
      'retained detached capability still owns its runtime state');
    LTree.ExpandAll;
    LView := LRenderer.CollectionView('handbook-tree');
    Check(not NyxTreeHierarchy(LView).IsExpanded(Item('docs')),
      'new application mount has independent closed state');
    LDetach := TDetachProbe.Create;
    LDetach.Mount := LRenderer.CollectionMount('handbook-tree');
    LToken := LView.Subscribe(LDetach.Changed);
    try
      NyxTreeHierarchy(LView).SetExpanded(Item('docs'), True);
      Check((LDetach.Calls = 1) and not LDetach.Mount.Connected,
        'disclosure observer can retire the renderer attachment');
      LToken.Disconnect;
      NyxTreeHierarchy(LView).CollapseAll;
      Check(LDetach.Calls = 1, 'retired observer receives no later disclosure');
    finally
      LToken.Disconnect;
      LToken := nil;
      LDetach.Free;
    end;
    { A new mount supplies the live host after the preceding attachment was
      deliberately retired. Now dispose the actual renderer inside a physical
      disclosure publication, not merely by manually invoking an observer. }
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LView := LRenderer.CollectionView('handbook-tree');
    {$ifndef PAS2JS}
    LControl := TTreeView(LRenderer.ControlFor('handbook-tree'));
    LControl.Items.FindNodeWithText('Documentation').Expand(False);
    NyxTreeHierarchy(LView).CollapseAll;
    Application.ProcessMessages;
    Check(not NyxTreeHierarchy(LView).IsExpanded(Item('docs')),
      'silent application collapse supersedes queued host expansion');
    Physical('docs', False);
    LControl.Items.FindNodeWithText('Documentation').Expand(False);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    Application.ProcessMessages;
    Check(not NyxTreeHierarchy(LView).IsExpanded(Item('docs')),
      'unmount revokes pending physical publication');
    LView := LRenderer.CollectionView('handbook-tree');
    {$endif}
    LRetire := TRendererRetirement.Create;
    LRetire.Renderer := LRenderer;
    LToken := LView.Subscribe(LRetire.Changed);
    try
      {$ifdef PAS2JS}
      LControl := LRenderer.ElementFor('handbook-tree');
      TTreeDetails(LControl.querySelector('[data-nyx-item="docs"]')).open := True;
      LControl.querySelector('[data-nyx-item="docs"]').dispatchEvent(TJSEvent.new('toggle'));
      {$else}
      LControl := TTreeView(LRenderer.ControlFor('handbook-tree'));
      LControl.Items.FindNodeWithText('Documentation').Expand(False);
      Check(LRetire.Calls = 0, 'native host disclosure waits until its call stack returns');
      Application.ProcessMessages;
      {$endif}
      LRenderer := LRetire.Renderer;
      Check((LRetire.Calls = 1) and (LRenderer = nil),
        'physical disclosure publication safely retires the whole renderer');
      Check(NyxTreeHierarchy(LView).IsExpanded(Item('docs')),
        'retained view observes accepted disclosure after physical host retirement');
    finally
      LToken.Disconnect;
      LToken := nil;
      LRenderer := LRetire.Renderer;
      LRetire.Free;
    end;
  finally
    LRenderer.Free;
    {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
    LDocument.Free;
  end;
end;
{$endif}

begin
  {$ifndef PAS2JS}
  if (ParamCount = 2) and (ParamStr(1) = '--compose') then
  begin
    WriteBytes(ParamStr(2), Composition.ToJSON);
    Exit;
  end;

  if (ParamCount = 3) and (ParamStr(1) = '--semantic') then
  begin
    Semantic(ParamStr(2), ParamStr(3));
    Exit;
  end;
  {$ifndef NYX_TREE_SHARED_ONLY}Application.Initialize;{$endif}
  {$endif}
  try
    Shared;
    {$ifndef NYX_TREE_SHARED_ONLY}Controls;{$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-tree-hierarchy', 'passed');
    document.body.appendChild(document.createTextNode('PASS ' + IntToStr(GChecks) + ' tree hierarchy checks'));
    {$else}
    WriteLn('PASS ', GChecks, ' tree hierarchy checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-tree-hierarchy', 'failed');
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn('FAIL ', E.Message);
      Halt(1);
      {$endif}
    end;
  end;
end.
