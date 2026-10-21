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

program nyx_kanban_studio_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.source, nyx.composition, nyx.schema, nyx.catalog,
  nyx.kanban, nyx.kanban.intent, nyx.kanban.editor, nyx.studio.edits,
  nyx.collections, nyx.collections.view.types, nyx.collections.query,
  nyx.collections.selection, nyx.collections.columns, nyx.collections.paging,
  nyx.typeahead
  {$ifdef PAS2JS}, Web{$endif};

type
  { Real source-owned schema and two reusable occurrences. Candidate tests use
    only the public typed editor/patch, never the internal mutator or a renderer. }
  TBoardDesign = class
    Document: TNyxDocument;
    Definition: INyxKanbanBoard;
    First: INyxComponent;
    Second: INyxComponent;
    constructor Create(AConfigured: Boolean);
    destructor Destroy; override;
  end;

var
  GCatalog: TNyxCatalog;
  GChecks: Integer;

function KeyA: TNyxCollectionRef;
begin
  Result := NyxCollection('cards');
end;

function KeyB: TNyxCollectionRef;
begin
  Result := NyxCollection('projects');
end;

function OptionsA: TNyxKanbanOptions;
begin
  Result := NyxKanbanOptions(KeyA, NyxTextField('title'), NyxTextField('lane')).
    Lane(NyxKanbanLane('ready'), 'Ready').
    Lane(NyxKanbanLane('active'), 'Active').
    Lane(NyxKanbanLane('done'), 'Done');
end;

function OptionsB: TNyxKanbanOptions;
begin
  Result := NyxKanbanOptions(KeyB, NyxTextField('subject'), NyxTextField('stage')).
    Lane(NyxKanbanLane('queued'), 'Queued').
    Lane(NyxKanbanLane('doing'), 'Doing').
    Lane(NyxKanbanLane('finished'), 'Finished').Scoped(csApplication);
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(String(AMessage));
  end;
end;

procedure DefineData(ADocument: TNyxDocument);
begin
  ADocument.Collections.Define(KeyA,
    NyxCollectionSchema.Text(NyxTextField('title'), '').
      Text(NyxTextField('lane'), 'ready'), [
    NyxCollectionItem(NyxItem(KeyA, 'one')).WithValue(NyxTextField('title'), 'One')]);
  ADocument.Collections.Define(KeyB,
    NyxCollectionSchema.Text(NyxTextField('subject'), '').
      Text(NyxTextField('stage'), 'queued'), [
    NyxCollectionItem(NyxItem(KeyB, 'one')).
      WithValue(NyxTextField('subject'), 'Project one')]);
end;

constructor TBoardDesign.Create(AConfigured: Boolean);
var
  LPage: INyxPage;
begin
  inherited Create;
  Document := TNyxDocument.Create;
  DefineData(Document);
  Definition := NewNyxKanbanBoard('board-definition');

  if AConfigured then
  begin
    ConfigureNyxKanbanBoard(Definition, OptionsA);
    SetNyxKanbanIntent(Definition, TNyxKanbanIntent.Configured(OptionsA));
  end;
  Document.AddComponent(Definition);
  First := NewNyxComponent('first-board');
  First.Configure.Component(NyxComponent('board-definition')).Done;
  Second := NewNyxComponent('second-board');
  Second.Configure.Component(NyxComponent('board-definition')).Done;
  LPage := NewNyxPage('home');
  LPage.Add(First);
  LPage.Add(Second);
  Document.AddPage(LPage);
  Document.Validate;
end;

destructor TBoardDesign.Destroy;
begin
  Definition := nil;
  First := nil;
  Second := nil;
  Document.Free;
  inherited Destroy;
end;

function Candidate(ADocument: TNyxDocument; const AOwner: TNyxText;
  const AIntent: TNyxKanbanIntent): TNyxDocument;
var
  LChange: TNyxKanbanEditorChange;
  LPatch: INyxDesignPatch;
begin
  LChange := NyxKanbanEditorChange(ADocument, NyxControl(AOwner), AIntent);
  LPatch := NyxKanbanPolicyPatch(LChange);
  Check(NyxDesignChanges(LPatch).Count = 1,
    'one typed design operation owns all three lane bindings and the intent');
  Result := LPatch.Candidate(ADocument, GCatalog);
end;

procedure RoundTrip(ADocument: TNyxDocument);
var
  LJSON: TNyxText;
  LSource: TNyxText;
  LDecoded: TNyxDocument;
  LReplayed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
begin
  LJSON := TNyxCodec.Encode(ADocument);
  LSource := TNyxCodegen.Generate(ADocument);
  LDecoded := TNyxCodec.Decode(LJSON);
  LReplayed := nil;
  LWorkspace := nil;
  try
    Check(TNyxCodec.Encode(LDecoded) = LJSON,
      'the complete candidate round trips through ordinary design persistence');
    LReplayed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check((TNyxCodec.Encode(LReplayed) = LJSON) and
      (LWorkspace.Capture.Source = LSource),
      'crafted Pascal independently reconstructs the exact admitted whole design');
  finally
    LWorkspace.Free;
    LReplayed.Free;
    LDecoded.Free;
  end;
end;

procedure CheckBoard(ADocument: TNyxDocument; const AOwner: TNyxText;
  const AOptions: TNyxKanbanOptions; AHasMoves: Boolean);
var
  LContext: TNyxNode;
  LBoard: TNyxNode;
  LItems: TNyxNode;
  LLane: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LIndex: Integer;
  LChild: Integer;
  LMoves: Integer;
begin
  LContext := RealizeNyxContext(ADocument, ADocument.Find(AOwner), LBoard);
  try
    Check(LBoard.Kind = NyxKindName(nkKanbanBoard),
      'the exact authored target remains a specialized board');
    for LIndex := 0 to 2 do
    begin
      LLane := LBoard.Part(NyxPart('lane-' + IntToStr(LIndex)));
      LItems := LLane.Part(NyxPart('items'));
      LSpec := LItems.CollectionView;
      Check(LSpec.Defined and (LSpec.Key.Name = AOptions.Collection.Name) and
        (LSpec.Scope = AOptions.Scope) and (LSpec.SelectionMode = nsmSingle) and
        (LSpec.Count = 1) and (LSpec.ColumnAt(0).FieldName = AOptions.TitleField.Name) and
        (LSpec.ColumnAt(0).Mode = cmReadOnly) and
        (LSpec.QueryPolicy.ToData.ToJSON = NyxCollectionQuery.Where(
          NyxWhere(AOptions.LaneField).EqualTo(AOptions.LaneAt(LIndex).Name,
            nqtExact)).ToData.ToJSON),
        'each effective list receives the complete required typed behavior');
      LMoves := 0;
      for LChild := 0 to LLane.Count - 1 do
      begin

        if LLane.Children[LChild].StoredProp(NyxAttributeName(atPart)) = 'move' then
        begin
          Inc(LMoves);
          Check(LLane.Children[LChild].ProjectionKind = NyxKindName(nkButton),
            'each created Move role is an ordinary button');
        end;
      end;

      if AHasMoves then
      begin
        Check(LMoves = 1, 'the effective lane owns exactly one Move face');
      end
      else
      begin
        Check(LMoves = 0, 'an unchanged static lane does not gain a Move face');
      end;
    end;
  finally
    LContext.Free;
  end;
end;

procedure DirectCase;
var
  LDocument: TNyxDocument;
  LNext: TNyxDocument;
  LPage: INyxPage;
  LBoard: INyxKanbanBoard;
  LBefore: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  LNext := nil;
  try
    DefineData(LDocument);
    LPage := NewNyxPage('direct-page');
    LBoard := NewNyxKanbanBoard('direct-board');
    LPage.Add(LBoard);
    LDocument.AddPage(LPage);
    LBefore := TNyxCodec.Encode(LDocument);
    LNext := Candidate(LDocument, 'direct-board', TNyxKanbanIntent.Configured(OptionsA));
    CheckBoard(LNext, 'direct-board', OptionsA, True);
    Check((GetNyxKanbanIntent(LNext.Find('direct-board')).Mode = nkiConfigured) and
      (TNyxCodec.Encode(LDocument) = LBefore),
      'initial concrete configuration changes only its independently owned candidate');
    RoundTrip(LNext);
  finally
    LBoard := nil;
    LPage := nil;
    LNext.Free;
    LDocument.Free;
  end;
end;

procedure ReusablePayloadCases;
var
  LFixture: TBoardDesign;
  LNext: TNyxDocument;
  LRule: TNyxNode;
  LPayload: INyxColumn;
  LList: INyxList;
  LExtra: INyxLabel;
  LHeading: INyxHeading;
  LMode: TNyxOverrideMode;
  LCase: Integer;
  LBefore: TNyxText;
  LDefinition: TNyxText;
  LSibling: TNyxText;
begin
  for LCase := 0 to 1 do
  begin
    LFixture := TBoardDesign.Create(False);
    LNext := nil;
    try
      LFixture.First.Node.OverridePart(NyxPart('lane-0'), noProperties).
        Named('lane-zero').SetProp(NyxAttributeName(atHint), 'Keep this lane hint');
      LMode := noAppend;

      if LCase = 1 then
      begin
        LMode := noPrepend;
      end;
      LRule := LFixture.First.Node.OverridePart(NyxPart('lane-1'), LMode).
        Named('lane-one');
      LExtra := NewNyxLabel('retained-extra');
      LExtra.Configure.PartName(NyxPart('extra')).Text('Keep this appended content').Done;
      LRule.Add(LExtra);
      LPayload := NewNyxColumn('local-lane');
      LHeading := NewNyxHeading('custom-heading').WithText('Our custom final lane');
      LHeading.Configure.PartName(NyxPart('title')).Done;
      LPayload.Add(LHeading);
      LList := NewNyxList('custom-items');
      LList.Configure.PartName(NyxPart('items')).Done;
      LPayload.Add(LList);
      LExtra := NewNyxLabel('retained-replacement-content');
      LExtra.Configure.PartName(NyxPart('extra')).Text('Keep this replacement content').Done;
      LPayload.Add(LExtra);
      LRule := LFixture.First.Node.OverridePart(NyxPart('lane-2'), noReplace).
        Named('lane-two');
      LRule.Add(LPayload);
      LBefore := TNyxCodec.Encode(LFixture.Document);
      LDefinition := TNyxCodec.NodeSnapshot(LFixture.Definition.Node).ToJSON;
      LSibling := TNyxCodec.NodeSnapshot(LFixture.Second.Node).ToJSON;
      LNext := Candidate(LFixture.Document, 'first-board',
        TNyxKanbanIntent.Configured(OptionsA));
      CheckBoard(LNext, 'first-board', OptionsA, True);
      Check((LNext.Find('lane-zero').StoredProp(NyxAttributeName(atOverrideMode)) =
        NyxOverrideName(noAppend)) and
        (LNext.Find('lane-zero').StoredProp(NyxAttributeName(atHint)) =
          'Keep this lane hint'),
        'missing initial actions use ordinary properties-to-append promotion');
      Check((LNext.Find('lane-one').StoredProp(NyxAttributeName(atOverrideMode)) =
        NyxOverrideName(LMode)) and
        (LNext.Find('retained-extra').Parent.ID = 'lane-one') and
        (LNext.Find('retained-extra').Prop(NyxAttributeName(atText)) =
          'Keep this appended content'),
        'append/prepend mode and existing local payload identity remain exact');
      Check((LNext.Find('lane-two').StoredProp(NyxAttributeName(atOverrideMode)) =
        NyxOverrideName(noReplace)) and (LNext.Find('lane-two').Count = 1) and
        (LNext.Find('lane-two').Children[0].ID = 'local-lane') and
        (LNext.Find('retained-replacement-content').Parent.ID = 'local-lane') and
        (LNext.Find('custom-heading').Prop(NyxAttributeName(atText)) =
          'Our custom final lane'),
        'replace retains its single column payload rather than appending another replacement');
      Check((TNyxCodec.NodeSnapshot(LNext.Find('board-definition')).ToJSON = LDefinition) and
        (TNyxCodec.NodeSnapshot(LNext.Find('second-board')).ToJSON = LSibling) and
        (TNyxCodec.Encode(LFixture.Document) = LBefore),
        'all local role writes preserve original, definition and sibling exactly');
      RoundTrip(LNext);
    finally
      LExtra := nil;
      LList := nil;
      LHeading := nil;
      LPayload := nil;
      LNext.Free;
      LFixture.Free;
    end;
  end;
end;

function FancySpec: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(KeyA).Column(NyxTextField('title'), 'Our card caption').
    ColumnSizing(NyxColumnSizing.Flex(2).Minimum(91).Maximum(351)).
    Scoped(csInstance).Selection(nsmSingle).
    TypeAhead(NyxTypeAhead.WindowMilliseconds(1700).Match(ntmExact)).
    Paged(NyxCollectionPaging.PageSize(5).InitialPage(2));
end;

procedure ReconfigureAndInherit;
var
  LFixture: TBoardDesign;
  LFirst: TNyxDocument;
  LSecond: TNyxDocument;
  LDisabled: TNyxDocument;
  LInherited: TNyxDocument;
  LSpec: TNyxCollectionViewSpec;
  LActual: TNyxCollectionViewSpec;
  LContext: TNyxNode;
  LBoard: TNyxNode;
  LIndex: Integer;
  LDefinition: TNyxText;
  LSibling: TNyxText;
  LBefore: TNyxText;
begin
  LFixture := TBoardDesign.Create(True);
  LFirst := nil;
  LSecond := nil;
  LDisabled := nil;
  LInherited := nil;
  LContext := nil;
  try
    LSpec := FancySpec;
    for LIndex := 0 to 2 do
    begin
      LFixture.First.Node.OverridePart(
        NyxPart('lane-' + IntToStr(LIndex) + '/items'), noProperties).
        Named('items-rule-' + IntToStr(LIndex)).SetCollectionView(
          LSpec.Query(NyxCollectionQuery.Where(NyxWhere(NyxTextField('lane')).
            EqualTo(OptionsA.LaneAt(LIndex).Name, nqtExact))));
    end;
    LFixture.First.Node.OverridePart(NyxPart('lane-1/title'), noProperties).
      Named('heading-rule').SetProp(NyxAttributeName(atText), 'Team-specific heading');
    LFixture.First.Node.OverridePart(NyxPart('lane-1/move'), noProperties).
      Named('move-rule').SetProp(NyxAttributeName(atText), 'Bring the selected card');
    ValidateNyxDocumentProperties(LFixture.Document);
    LDefinition := TNyxCodec.NodeSnapshot(LFixture.Definition.Node).ToJSON;
    LSibling := TNyxCodec.NodeSnapshot(LFixture.Second.Node).ToJSON;
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LFirst := Candidate(LFixture.Document, 'first-board',
      TNyxKanbanIntent.Configured(OptionsB));
    CheckBoard(LFirst, 'first-board', OptionsB, True);
    LContext := RealizeNyxContext(LFirst, LFirst.Find('first-board'), LBoard);
    for LIndex := 0 to 2 do
    begin
      LActual := LBoard.Part(NyxPart('lane-' + IntToStr(LIndex) + '/items')).
        CollectionView;
      Check((LActual.ColumnAt(0).Title = LSpec.ColumnAt(0).Title) and
        (LActual.ColumnAt(0).Sizing.ToData.ToJSON = LSpec.ColumnAt(0).Sizing.ToData.ToJSON) and
        (LActual.TypeAheadPolicy.ToData.ToJSON = LSpec.TypeAheadPolicy.ToData.ToJSON) and
        (LActual.PagingPolicy.ToData.ToJSON = LSpec.PagingPolicy.ToData.ToJSON),
        'field/key/schema reconfiguration preserves each legal column policy and caption');
    end;
    Check((LBoard.Part(NyxPart('lane-1/title')).Prop(NyxAttributeName(atText)) =
      'Team-specific heading') and
      (LBoard.Part(NyxPart('lane-1/move')).Prop(NyxAttributeName(atText)) =
        'Bring the selected card'),
      'required behavioral changes never rewrite heading or Move cosmetics');
    FreeAndNil(LContext);
    RoundTrip(LFirst);
    LDisabled := Candidate(LFirst, 'first-board', TNyxKanbanIntent.Disabled);
    LContext := RealizeNyxContext(LDisabled, LDisabled.Find('first-board'), LBoard);
    Check((GetNyxKanbanIntent(LBoard).Mode = nkiDisabled) and
      (LBoard.Part(NyxPart('lane-0/items')).CollectionView.Key.Name = KeyB.Name),
      'Disable is extension-only and preserves the formerly configured roles');
    FreeAndNil(LContext);
    LInherited := Candidate(LDisabled, 'first-board', Default(TNyxKanbanIntent));
    Check(not HasNyxKanbanIntent(LInherited.Find('first-board')),
      'Inherit removes the exact local disabled/configured declaration');
    CheckBoard(LInherited, 'first-board', OptionsA, True);
    LContext := RealizeNyxContext(LInherited, LInherited.Find('first-board'), LBoard);
    Check((GetNyxKanbanIntent(LBoard).ToData.ToJSON =
      TNyxKanbanIntent.Configured(OptionsA).ToData.ToJSON) and
      (LBoard.Part(NyxPart('lane-1/items')).CollectionView.ColumnAt(0).Title =
        'Our card caption'),
      'inherited A reconciles former B behavior while preserving local column presentation');
    FreeAndNil(LContext);
    Check((TNyxCodec.NodeSnapshot(LInherited.Find('board-definition')).ToJSON = LDefinition) and
      (TNyxCodec.NodeSnapshot(LInherited.Find('second-board')).ToJSON = LSibling) and
      (TNyxCodec.Encode(LFixture.Document) = LBefore),
      'reconfigure/Disable/Inherit leave original and independent reusable owners exact');
    RoundTrip(LInherited);
    LSecond := Candidate(LInherited, 'first-board', TNyxKanbanIntent.Configured(OptionsB));
    CheckBoard(LSecond, 'first-board', OptionsB, True);
  finally
    LContext.Free;
    LSecond.Free;
    LInherited.Free;
    LDisabled.Free;
    LFirst.Free;
    LFixture.Free;
  end;
end;

procedure InheritStatic;
var
  LFixture: TBoardDesign;
  LConfigured: TNyxDocument;
  LInherited: TNyxDocument;
  LContext: TNyxNode;
  LBoard: TNyxNode;
begin
  LFixture := TBoardDesign.Create(False);
  LConfigured := nil;
  LInherited := nil;
  LContext := nil;
  try
    LConfigured := Candidate(LFixture.Document, 'first-board',
      TNyxKanbanIntent.Configured(OptionsB));
    LInherited := Candidate(LConfigured, 'first-board', Default(TNyxKanbanIntent));
    LContext := RealizeNyxContext(LInherited, LInherited.Find('first-board'), LBoard);
    Check(not HasNyxKanbanIntent(LInherited.Find('first-board')) and
      (GetNyxKanbanIntent(LBoard).Mode = nkiUndefined),
      'Inherit to a static definition does not manufacture automatic intent');
    FreeAndNil(LContext);
    CheckBoard(LInherited, 'first-board', OptionsB, True);
    RoundTrip(LInherited);
  finally
    LContext.Free;
    LInherited.Free;
    LConfigured.Free;
    LFixture.Free;
  end;
end;

procedure NamedAndNestedCases;
var
  LDocument: TNyxDocument;
  LNext: TNyxDocument;
  LPage: INyxPage;
  LContainer: INyxColumn;
  LBoard: INyxKanbanBoard;
  LReference: INyxComponent;
  LLaneDefinition: INyxColumn;
  LLaneReference: INyxComponent;
  LItems: INyxList;
  LHeading: INyxHeading;
  LBefore: TNyxText;
  LRule: TNyxNode;
  LOldLane: TNyxNode;
  LNested: Boolean;
  LCase: Integer;
  LChild: Integer;
  LPrefixed: Boolean;
  LReferenceNode: TNyxNode;
begin
  for LCase := 0 to 1 do
  begin
    LDocument := TNyxDocument.Create;
    LNext := nil;
    try
      DefineData(LDocument);
      LBoard := NewNyxKanbanBoard('embedded-board');
      LNested := LCase = 1;

      if LNested then
      begin
        LLaneDefinition := NewNyxColumn('lane-definition');
        LHeading := NewNyxHeading('lane-heading').WithText('A reusable lane heading');
        LHeading.Configure.PartName(NyxPart('title')).Done;
        LItems := NewNyxList('lane-items');
        LItems.Configure.PartName(NyxPart('items')).Done;
        LLaneDefinition.Add(LHeading);
        LLaneDefinition.Add(LItems);
        LDocument.AddComponent(LLaneDefinition);
        LOldLane := LBoard.Node.Part(NyxPart('lane-0'));
        LBoard.Node.Remove(LOldLane);
        LLaneReference := NewNyxComponent('lane-reference');
        LLaneReference.Configure.Component(NyxComponent('lane-definition')).
          PartName(NyxPart('lane-0')).Done;
        LBoard.Add(LLaneReference);
      end;
      LBoard.Configure.PartName(NyxPart('work')).Done;
      LContainer := NewNyxColumn('nested-definition');
      LContainer.Add(LBoard);
      LDocument.AddComponent(LContainer);
      LReference := NewNyxComponent('named-instance');
      LReference.Configure.Component(NyxComponent('nested-definition')).Done;
      LRule := LReference.Node.OverridePart(NyxPart('work'), noProperties).
        Named('selected-board-rule');
      LRule.SetProp(NyxAttributeName(atHint), 'Keep the selected descriptor');
      LPage := NewNyxPage('named-page');
      LPage.Add(LReference);
      LDocument.AddPage(LPage);
      LBefore := TNyxCodec.Encode(LDocument);
      LNext := Candidate(LDocument, 'selected-board-rule',
        TNyxKanbanIntent.Configured(OptionsA));
      CheckBoard(LNext, 'selected-board-rule', OptionsA, True);
      Check((LNext.Find('selected-board-rule').StoredProp('path') = 'work') and
        (LNext.Find('selected-board-rule').StoredProp(NyxAttributeName(atOverrideMode)) =
          NyxOverrideName(noProperties)) and
        (LNext.Find('selected-board-rule').StoredProp(NyxAttributeName(atHint)) =
          'Keep the selected descriptor'),
        'the selected named-board descriptor preserves its exact path, mode and properties');
      LReferenceNode := LNext.Find('named-instance');
      LPrefixed := LReferenceNode.Count > 1;
      for LChild := 0 to LReferenceNode.Count - 1 do
      begin

        if LReferenceNode.Children[LChild].ID <> 'selected-board-rule' then
        begin
          LPrefixed := LPrefixed and (Copy(LReferenceNode.Children[LChild].
            StoredProp(NyxAttributeName(atPath)), 1, 5) = 'work/');
        end;
      end;
      Check(LPrefixed,
        'every additional named role descriptor stays under the exact work path prefix');

      if LNested then
      begin
        Check(TNyxCodec.NodeSnapshot(LNext.Find('lane-definition')).ToJSON =
          TNyxCodec.NodeSnapshot(LDocument.Find('lane-definition')).ToJSON,
          'a nested reusable lane never writes its upstream shared definition');
      end;
      Check(TNyxCodec.Encode(LDocument) = LBefore,
        'named and nested configuration leaves the original input untouched');
      RoundTrip(LNext);
    finally
      LHeading := nil;
      LItems := nil;
      LLaneReference := nil;
      LLaneDefinition := nil;
      LReference := nil;
      LContainer := nil;
      LBoard := nil;
      LPage := nil;
      LNext.Free;
      LDocument.Free;
    end;
  end;
end;

procedure RefusalCases;
var
  LFixture: TBoardDesign;
  LChange: TNyxKanbanEditorChange;
  LPatch: INyxDesignPatch;
  LUnexpected: TNyxDocument;
  LBefore: TNyxText;
  LRejected: Boolean;
  LIndex: Integer;
  LMove: INyxButton;
  LWire: TNyxDataValue;
begin
  for LIndex := 0 to 3 do
  begin
    LFixture := TBoardDesign.Create(False);
    LUnexpected := nil;
    try

      if LIndex = 3 then
      begin
        LMove := NewNyxButton('ambiguous-one');
        LMove.Configure.PartName(NyxPart('move')).Done;
        LFixture.Definition.Lane0Column.Add(LMove);
        LMove := NewNyxButton('ambiguous-two');
        LMove.Configure.PartName(NyxPart('move')).Done;
        LFixture.Definition.Lane0Column.Add(LMove);
      end;
      LChange := NyxKanbanEditorChange(LFixture.Document, NyxControl('first-board'),
        TNyxKanbanIntent.Configured(OptionsA));

      if LIndex = 0 then
      begin
        LFixture.Document.Pages[0].Add(TNyxNode.Create(
          NyxKindName(nkLabel), 'kanban-move-button'));
      end
      else if LIndex = 1 then
      begin
        LFixture.First.Node.OverridePart(NyxPart('lane-1/items'), noRemove).
          Named('now-removed-items');
      end
      else if LIndex = 2 then
      begin
        LWire := NyxObject([NyxField('version', NyxData(1)),
          NyxField('target', NyxObject([NyxField('owner', NyxData('first-board')),
            NyxField('instance', NyxData('second-board')), NyxField('path', NyxData('.'))])),
          NyxField('baseline', NyxData(LChange.Baseline)),
          NyxField('intent', LChange.Intent.ToData)]);
        LChange := TNyxKanbanEditorChange.FromData(LWire);
      end;
      LBefore := TNyxCodec.Encode(LFixture.Document);
      LPatch := NyxKanbanPolicyPatch(LChange);
      LRejected := False;
      try
        LUnexpected := LPatch.Candidate(LFixture.Document, GCatalog);
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LUnexpected = nil),
        'stale/removed/forged/ambiguous whole candidate refuses for case ' + IntToStr(LIndex));
      Check(TNyxCodec.Encode(LFixture.Document) = LBefore,
        'refusal preserves every original source descriptor for case ' + IntToStr(LIndex));
    finally
      LMove := nil;
      LPatch := nil;
      LUnexpected.Free;
      LFixture.Free;
    end;
  end;
end;

procedure OccupiedIdentityCase;
var
  LFixture: TBoardDesign;
  LNext: TNyxDocument;
  LBefore: TNyxText;
begin
  LFixture := TBoardDesign.Create(False);
  LNext := nil;
  try
    LFixture.Document.Pages[0].Add(TNyxNode.Create(
      NyxKindName(nkLabel), 'kanban-move-button').SetProp(
      NyxAttributeName(atText), 'This existing identity stays'));
    LFixture.Document.Pages[0].Add(TNyxNode.Create(
      NyxKindName(nkLabel), 'kanban-items-binding'));
    LFixture.Document.Pages[0].Add(TNyxNode.Create(
      NyxKindName(nkLabel), 'kanban-lane-actions'));
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LNext := Candidate(LFixture.Document, 'first-board',
      TNyxKanbanIntent.Configured(OptionsA));
    CheckBoard(LNext, 'first-board', OptionsA, True);
    Check((LNext.Find('kanban-move-button').Kind = NyxKindName(nkLabel)) and
      (LNext.Find('kanban-move-button').Prop(NyxAttributeName(atText)) =
        'This existing identity stays') and
      (LNext.Find('kanban-items-binding').Kind = NyxKindName(nkLabel)) and
      (LNext.Find('kanban-lane-actions').Kind = NyxKindName(nkLabel)) and
      (TNyxCodec.Encode(LFixture.Document) = LBefore),
      'known occupied identities are preserved while fresh candidate IDs avoid collisions');
    RoundTrip(LNext);
  finally
    LNext.Free;
    LFixture.Free;
  end;
end;

procedure MalformedPatchCase;
var
  LFixture: TBoardDesign;
  LChange: TNyxKanbanEditorChange;
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  LFixture := TBoardDesign.Create(False);
  try
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LChange := NyxKanbanEditorChange(LFixture.Document, NyxControl('first-board'),
      TNyxKanbanIntent.Configured(OptionsA));
    LRejected := False;
    try
      ReadNyxDesignPatch(NyxArray([NyxObject([
        NyxField('op', NyxData('kanban-policy')),
        NyxField('change', LChange.ToData), NyxField('extra', NyxData(True))])]));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LFixture.Document) = LBefore),
      'the public Kanban patch decoder refuses extra fields without touching source');
  finally
    LFixture.Free;
  end;
end;

begin
  GCatalog := TNyxCatalog.Create;
  try
    try
      DirectCase;
      ReusablePayloadCases;
      ReconfigureAndInherit;
      InheritStatic;
      NamedAndNestedCases;
      RefusalCases;
      OccupiedIdentityCase;
      MalformedPatchCase;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-kanban-studio', 'passed');
      document.body.setAttribute('data-kanban-studio-checks', IntToStr(GChecks));
      document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' Kanban candidate checks';
      {$else}
      WriteLn('PASS ', GChecks, ' Kanban candidate checks');
      {$endif}
    except
      on LException: Exception do
      begin
        {$ifdef PAS2JS}
        document.body.setAttribute('data-kanban-studio', 'failed');
        document.body.setAttribute('data-kanban-studio-checks', IntToStr(GChecks));
        document.body.textContent := 'FAIL at check count ' + IntToStr(GChecks) +
          ': ' + LException.Message;
        {$else}
        WriteLn('FAIL at check count ', GChecks, ': ', LException.Message);
        ExitCode := 1;
        {$endif}
      end;
    end;
  finally
    GCatalog.Free;
  end;
end.
