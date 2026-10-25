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

program nyx_kanban_editor_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.kanban, nyx.kanban.intent, nyx.kanban.editor, nyx.codec, nyx.composition,
  nyx.collections, nyx.collections.view.types, nyx.collections.selection,
  nyx.collections.query, nyx.collections.columns, nyx.collections.paging,
  nyx.collections.query.policy,
  nyx.collections.workbench.policy, nyx.collections.arrangement,
  nyx.collections.presentation, nyx.collections.alignment, nyx.formatting,
  nyx.collections.view,
  nyx.typeahead, nyx.state, nyx.codegen, nyx.source
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { Real authored boards, independent reusable roots and a named board
    descriptor exercise the same canonical context owner used by Studio. }
  TEditorFixture = class
    Document: TNyxDocument;
    Board: INyxKanbanBoard;
    Whole: INyxComponent;
    Nested: INyxComponent;
    Rule: TNyxNode;
    Schema: TNyxCollectionSchema;
    Options: TNyxKanbanOptions;
    constructor Create;
    destructor Destroy; override;
  end;
  { Each family is valid on a Table but incompatible with a required Kanban
    List role, including explicit choices equal to today's effective default. }
  TLaneUnsupportedPolicy = (lupColumnLayout, lupColumnText, lupAlignment,
    lupTableLayout, lupDensity, lupWorkbench, lupFormat);

var
  GChecks: Integer;

function Cards: TNyxCollectionRef;
begin
  Result := NyxCollection('cards');
end;

function TitleField: TNyxTextFieldRef;
begin
  Result := NyxTextField('title');
end;

function LaneField: TNyxTextFieldRef;
begin
  Result := NyxTextField('lane');
end;

function OptionsFor(const ATitle: TNyxTextFieldRef): TNyxKanbanOptions;
begin
  Result := NyxKanbanOptions(Cards, ATitle, LaneField).
    Lane(NyxKanbanLane('ready'), 'Ready').
    Lane(NyxKanbanLane('active'), 'Active').
    Lane(NyxKanbanLane('done'), 'Done').Scoped(csInstance);
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(String(AMessage));
  end;
end;

constructor TEditorFixture.Create;
var
  LPage: INyxPage;
  LDefinition: INyxKanbanBoard;
  LContainer: INyxColumn;
  LNestedBoard: INyxKanbanBoard;
begin
  inherited Create;
  Document := TNyxDocument.Create;
  Schema := NyxCollectionSchema.Text(TitleField, '').Text(LaneField, 'ready').
    Text(NyxTextField('summary'), '').Boolean(NyxBooleanField('flag'), False);
  Document.Collections.Define(Cards, Schema, [
    NyxCollectionItem(NyxItem(Cards, 'one')).WithValue(TitleField, 'First card').
      WithValue(LaneField, 'ready')]);
  Options := OptionsFor(TitleField);
  Board := NewNyxKanbanBoard('direct-board');
  ConfigureNyxKanbanBoard(Board, Options);
  SetNyxKanbanIntent(Board, TNyxKanbanIntent.Configured(Options));
  LDefinition := NewNyxKanbanBoard('board-definition');
  ConfigureNyxKanbanBoard(LDefinition, Options);
  SetNyxKanbanIntent(LDefinition, TNyxKanbanIntent.Configured(Options));
  Document.AddComponent(LDefinition);
  LContainer := NewNyxColumn('nested-definition');
  LNestedBoard := NewNyxKanbanBoard('nested-board');
  LNestedBoard.Configure.PartName(NyxPart('work')).Done;
  ConfigureNyxKanbanBoard(LNestedBoard, Options);
  SetNyxKanbanIntent(LNestedBoard, TNyxKanbanIntent.Configured(Options));
  LContainer.Add(LNestedBoard);
  Document.AddComponent(LContainer);
  Whole := NewNyxComponent('whole-instance');
  Whole.Configure.Component(NyxComponent('board-definition')).Done;
  Nested := NewNyxComponent('nested-instance');
  Nested.Configure.Component(NyxComponent('nested-definition')).Done;
  Rule := Nested.Node.OverridePart(NyxPart('work'), noProperties).Named('board-rule');
  LPage := NewNyxPage('home');
  LPage.Add(Board);
  LPage.Add(Whole);
  LPage.Add(Nested);
  LPage.Add(NewNyxLabel('unrelated'));
  Document.AddPage(LPage);
  Document.Validate;
end;

destructor TEditorFixture.Destroy;
begin
  Board := nil;
  Whole := nil;
  Nested := nil;
  Document.Free;
  inherited Destroy;
end;

procedure ExpectWireRefusal(const AData: TNyxDataValue; const AMessage: TNyxText);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    TNyxKanbanEditorChange.FromData(AData);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AMessage);
end;

procedure TargetCases;
var
  LFixture: TEditorFixture;
  LTarget: TNyxKanbanEditorTarget;
  LRejected: Boolean;
  LExtra: TNyxNode;
  LPayload: TNyxNode;
begin
  LFixture := TEditorFixture.Create;
  try
    LTarget := NyxKanbanEditorTarget(LFixture.Document, NyxControl('direct-board'));
    Check((LTarget.Owner.ID = 'direct-board') and (LTarget.Instance.ID = '') and
      (LTarget.Path.Name = '.'), 'a concrete board retains its direct authored identity');
    LTarget := NyxKanbanEditorTarget(LFixture.Document, NyxControl('whole-instance'));
    Check((LTarget.Owner.ID = 'whole-instance') and
      (LTarget.Instance.ID = 'whole-instance') and (LTarget.Path.Name = '.'),
      'a whole reusable board addresses its own exact instance root');
    LTarget := NyxKanbanEditorTarget(LFixture.Document, NyxControl('board-rule'));
    Check((LTarget.Owner.ID = 'board-rule') and
      (LTarget.Instance.ID = 'nested-instance') and (LTarget.Path.Name = 'work'),
      'a named board selection retains descriptor, parent and stored path separately');
    LFixture.Rule.SetProp(NyxAttributeName(atOverrideMode), NyxOverrideName(noAppend));
    LPayload := TNyxNode.Create(NyxKindName(nkLabel), 'custom-board-content');
    LFixture.Rule.Add(LPayload);
    LTarget := NyxKanbanEditorTarget(LFixture.Document, NyxControl('board-rule'));
    Check((LTarget.Owner.ID = 'board-rule') and
      (LFixture.Rule.StoredProp(NyxAttributeName(atOverrideMode)) =
        NyxOverrideName(noAppend)),
      'reading a named target preserves its authored override mode');
    LFixture.Rule.Remove(LPayload);
    LFixture.Rule.SetProp(NyxAttributeName(atOverrideMode), NyxOverrideName(noRemove));
    LRejected := False;
    try
      NyxKanbanEditorTarget(LFixture.Document, NyxControl('board-rule'));
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'a hidden named board grants no editable target');
    LFixture.Rule.SetProp(NyxAttributeName(atOverrideMode), NyxOverrideName(noProperties));
    LExtra := TNyxNode.Create(NyxKindName(nkSlotOverride), 'duplicate-rule');
    LExtra.SetProp('path', 'work').
      SetProp(NyxAttributeName(atOverrideMode), NyxOverrideName(noProperties));
    LFixture.Nested.Node.Add(LExtra);
    LRejected := False;
    try
      NyxKanbanEditorTarget(LFixture.Document, NyxControl('board-rule'));
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'two same-path descriptors refuse instead of selecting an arbitrary rule');
    LFixture.Nested.Node.Remove(LExtra);
    LRejected := False;
    try
      NyxKanbanEditorTarget(LFixture.Document, NyxControl('unrelated'));
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'an unrelated concrete projection is not a board');
    LRejected := False;
    try
      NyxKanbanEditorTarget(LFixture.Document, NyxControl('absent'));
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'an absent authored identity never falls back to selection');
  finally
    LFixture.Free;
  end;
end;

procedure WireCases;
var
  LFixture: TEditorFixture;
  LChange: TNyxKanbanEditorChange;
  LCopy: TNyxKanbanEditorChange;
  LWire: TNyxDataValue;
  LTarget: TNyxDataValue;
  LIntent: TNyxKanbanIntent;
  LRejected: Boolean;
begin
  LFixture := TEditorFixture.Create;
  try
    LChange := NyxKanbanEditorChange(LFixture.Document, NyxControl('direct-board'),
      TNyxKanbanIntent.Configured(LFixture.Options));
    LWire := LChange.ToData;
    LCopy := TNyxKanbanEditorChange.FromData(LWire);
    ValidateNyxKanbanEditorChange(LFixture.Document, LCopy);
    Check(LCopy.ToData.ToJSON = LWire.ToJSON,
      'configured values round trip exact copied target, intent and baseline');
    LCopy := LChange.Copy;
    LIntent := TNyxKanbanIntent.Configured(OptionsFor(NyxTextField('summary')));
    LChange := NyxKanbanEditorChange(LFixture.Document, NyxControl('direct-board'), LIntent);
    Check(LCopy.Intent.Options.TitleField.Name = 'title',
      'replacing one change cannot mutate an earlier copied options vector');
    LChange := NyxKanbanEditorChange(LFixture.Document, NyxControl('direct-board'),
      TNyxKanbanIntent.Disabled);
    Check(TNyxKanbanEditorChange.FromData(LChange.ToData).Intent.Mode = nkiDisabled,
      'explicit disabled survives value reconstruction');
    LIntent := Default(TNyxKanbanIntent);
    LChange := NyxKanbanEditorChange(LFixture.Document, NyxControl('direct-board'), LIntent);
    Check((LChange.ToData.Field('intent').Kind = ndNull) and
      (TNyxKanbanEditorChange.FromData(LChange.ToData).Intent.Mode = nkiUndefined),
      'inherit serializes explicit null without serializing an undefined declared intent');
    LTarget := LChange.Target.ToData;
    ExpectWireRefusal(NyxObject([NyxField('version', NyxData(1)),
      NyxField('target', LTarget), NyxField('baseline', NyxData(LChange.Baseline)),
      NyxField('intent', NyxNull), NyxField('extra', NyxData(True))]),
      'unknown top-level change fields refuse');
    ExpectWireRefusal(NyxObject([NyxField('version', NyxData(2)),
      NyxField('target', LTarget), NyxField('baseline', NyxData(LChange.Baseline)),
      NyxField('intent', NyxNull)]), 'unknown change versions refuse');
    ExpectWireRefusal(NyxObject([NyxField('version', NyxData(1)),
      NyxField('target', LTarget), NyxField('baseline', NyxData('')),
      NyxField('intent', NyxNull)]), 'an empty baseline refuses before authority exists');
    ExpectWireRefusal(NyxObject([NyxField('version', NyxData(1)),
      NyxField('target', NyxObject([NyxField('owner', NyxData('direct-board')),
        NyxField('instance', NyxNull), NyxField('path', NyxData('nested'))])),
      NyxField('baseline', NyxData(LChange.Baseline)), NyxField('intent', NyxNull)]),
      'a direct owner cannot smuggle a reusable path');
    ExpectWireRefusal(NyxObject([NyxField('version', NyxData(1)),
      NyxField('target', NyxObject([NyxField('owner', NyxData('')),
        NyxField('instance', NyxNull), NyxField('path', NyxData('.'))])),
      NyxField('baseline', NyxData(LChange.Baseline)), NyxField('intent', NyxNull)]),
      'empty owner identity refuses at the value boundary');
    ExpectWireRefusal(NyxObject([NyxField('version', NyxData(1)),
      NyxField('target', LTarget), NyxField('baseline', NyxData(LChange.Baseline)),
      NyxField('intent', NyxData(False))]), 'a Boolean cannot stand in for disabled intent');
    LRejected := False;
    try
      NyxKanbanEditorChange(LFixture.Document, NyxControl('direct-board'),
        TNyxKanbanIntent.Configured(OptionsFor(NyxTextField('flag'))));
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'an existing Boolean schema field never becomes a text role by name');
  finally
    LFixture.Free;
  end;
end;

procedure BaselineCases;
var
  LFixture: TEditorFixture;
  LChange: TNyxKanbanEditorChange;
  LBefore: TNyxText;
  LRejected: Boolean;
  LCase: Integer;
  LWire: TNyxDataValue;
begin
  for LCase := 0 to 4 do
  begin
    LFixture := TEditorFixture.Create;
    try
      LChange := NyxKanbanEditorChange(LFixture.Document, NyxControl('direct-board'),
        TNyxKanbanIntent.Disabled);
      LBefore := LChange.Baseline;
      LFixture.Document.Collections.Define(Cards, LFixture.Schema, [
        NyxCollectionItem(NyxItem(Cards, 'different-row')).
          WithValue(TitleField, 'Updated row')]);
      Check(NyxKanbanEditorBaseline(LFixture.Document, NyxControl('direct-board')) =
        LBefore, 'row replacement alone does not invalidate a configuration form');
      ValidateNyxKanbanEditorChange(LFixture.Document, LChange);
      case LCase of
        0:
          begin
            LFixture.Document.Collections.Define(Cards,
              LFixture.Schema.Text(NyxTextField('added'), ''), []);
          end;
        1:
          begin
            LFixture.Board.Lane0TitleHeading.Text := 'Changed heading';
          end;
        2:
          begin
            LFixture.Document.Pages[0].Add(TNyxNode.Create(
              NyxKindName(nkLabel), 'new-occupied-id'));
          end;
        3:
          begin
            SetNyxKanbanIntent(LFixture.Board, TNyxKanbanIntent.Disabled);
          end;
        4:
          begin
            LFixture.Rule.SetProp(NyxAttributeName(atHint), 'New descriptor property');
          end;
      end;
      LRejected := False;
      try
        ValidateNyxKanbanEditorChange(LFixture.Document, LChange);
      except
        on ENyxKanban do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'schema/parts/intent/occupied-descriptor change refuses for case ' +
        IntToStr(LCase));
    finally
      LFixture.Free;
    end;
  end;
  LFixture := TEditorFixture.Create;
  try
    LChange := NyxKanbanEditorChange(LFixture.Document, NyxControl('board-rule'),
      TNyxKanbanIntent.Disabled);
    LWire := NyxObject([NyxField('version', NyxData(1)),
      NyxField('target', NyxObject([NyxField('owner', NyxData('board-rule')),
        NyxField('instance', NyxData('whole-instance')), NyxField('path', NyxData('work'))])),
      NyxField('baseline', NyxData(LChange.Baseline)),
      NyxField('intent', TNyxKanbanIntent.Disabled.ToData)]);
    LChange := TNyxKanbanEditorChange.FromData(LWire);
    LRejected := False;
    try
      ValidateNyxKanbanEditorChange(LFixture.Document, LChange);
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'a copied forged parent cannot borrow a genuine named-board baseline');
  finally
    LFixture.Free;
  end;
end;

procedure LaneSpecCases;
var
  LOld: TNyxCollectionViewSpec;
  LNew: TNyxCollectionViewSpec;
  LBefore: TNyxText;
  LOptions: TNyxKanbanOptions;
  LRejected: Boolean;
  LIndex: Integer;
begin
  LOptions := OptionsFor(NyxTextField('summary')).Scoped(csApplication);
  LOld := NyxCollectionView(Cards).Column(TitleField, 'Our customer cards').
    ColumnSizing(NyxColumnSizing.Fixed(193).Minimum(101).Maximum(300)).
    ColumnQuery(NyxColumnQuery.Filtering(False).Sorting(False).Operators([]).
      TextComparison(nqtAsciiInsensitive)).
    TableQuery(NyxTableQuery.Enabled(False).Filtering(False).SortMode(nqsmSingle)).
    Scoped(csInstance).Selection(nsmSingle).
    TypeAhead(NyxTypeAhead.WindowMilliseconds(2300).Match(ntmExact)).
    Paged(NyxCollectionPaging.PageSize(7).InitialPage(2));
  LBefore := LOld.ToData.ToJSON;
  for LIndex := 0 to 2 do
  begin
    LNew := NyxKanbanEditorLaneSpec(LOptions, LIndex, LOld);
    Check((LNew.Key.Name = Cards.Name) and (LNew.Scope = csApplication) and
      (LNew.ColumnAt(0).FieldName = 'summary') and (LNew.SelectionMode = nsmSingle) and
      (LNew.ParentField = '') and (LNew.ColumnAt(0).Mode = cmReadOnly) and
      (LNew.QueryPolicy.ToData.ToJSON = NyxCollectionQuery.Where(
        NyxWhere(LaneField).EqualTo(LOptions.LaneAt(LIndex).Name, nqtExact)).ToData.ToJSON),
      'one lane rewrite admits every required behavioral field together');
    Check((LNew.ColumnAt(0).Title = 'Our customer cards') and
      (LNew.ColumnAt(0).Sizing.ToData.ToJSON = LOld.ColumnAt(0).Sizing.ToData.ToJSON) and
      (LNew.TypeAheadPolicy.ToData.ToJSON = LOld.TypeAheadPolicy.ToData.ToJSON) and
      (LNew.PagingPolicy.ToData.ToJSON = LOld.PagingPolicy.ToData.ToJSON),
      'each lane preserves title, sizing, search and page cosmetics/policies');
    Check(LNew.HasTableQueryPolicy and LNew.ColumnAt(0).HasQueryPolicy and
      (LNew.TableQueryPolicy.ToData.ToJSON = LOld.TableQueryPolicy.ToData.ToJSON) and
      (LNew.ColumnAt(0).QueryPolicy.ToData.ToJSON = LOld.ColumnAt(0).QueryPolicy.ToData.ToJSON),
      'each lane preserves explicit query opt-outs, empty operators and text comparison');
  end;
  Check(LOld.ToData.ToJSON = LBefore, 'role rewrites preserve the earlier immutable specification');
  LNew := NyxKanbanEditorLaneSpec(LOptions, 0, Default(TNyxCollectionViewSpec));
  Check((LNew.ColumnAt(0).Title = 'Card') and not LNew.HasTypeAhead and
    not LNew.PagingPolicy.Defined and not LNew.ColumnAt(0).Sizing.Defined and
    not LNew.HasTableQueryPolicy and not LNew.ColumnAt(0).HasQueryPolicy,
    'an initially unbound lane receives only ordinary missing defaults');
  LRejected := False;
  try
    NyxKanbanEditorLaneSpec(LOptions, 3, LOld);
  except
    on ENyxKanban do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'a lane outside the three-part recipe refuses');
  LRejected := False;
  try
    NyxKanbanEditorLaneSpec(LOptions, 0, LOld.Column(NyxTextField('other'), 'Other'));
  except
    on ENyxKanban do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'extra authored columns cannot silently disappear during reconfiguration');
  LRejected := False;
  try
    NyxKanbanEditorLaneSpec(LOptions, 0,
      NyxCollectionView(Cards).Column(NyxBooleanField('flag'), 'Flag'));
  except
    on ENyxKanban do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'a nontext previous column is not relabeled as text');
end;

{ One shared document admits every rebuilt lane through codec and crafted
  source. The optional native output is the exact generated unit for separate
  compiler checks; it is written as UTF-8 bytes, never through ANSI text files. }
procedure LaneQueryPolicyCases;
var
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPage: INyxPage;
  LList: INyxList;
  LOptions: TNyxKanbanOptions;
  LOld: TNyxCollectionViewSpec;
  LNew: TNyxCollectionViewSpec;
  LIndependent: TNyxCollectionViewSpec;
  LBefore: TNyxText;
  LOptionsBefore: TNyxText;
  LIndependentBefore: TNyxText;
  LSource: TNyxText;
  LWire: TNyxText;
  LVersion: Integer;
  LTableChoice: Integer;
  LColumnChoice: Integer;
  LLane: Integer;
  LCase: Integer;
  {$ifndef PAS2JS}
  LStream: TFileStream;
  {$endif}

  procedure AddLane(const ASpec: TNyxCollectionViewSpec);
  begin
    LList := NewNyxList('policy-lane-' + IntToStr(LCase));
    Inc(LCase);
    LPage.Add(LList);
    LList.Binds.Collection(ASpec).Done;
  end;

begin
  LDocument := TNyxDocument.Create;
  LDecoded := nil;
  LParsed := nil;
  LWorkspace := nil;
  try
    LDocument.Collections.Define(Cards, NyxCollectionSchema.Text(TitleField, '').
      Text(NyxTextField('summary'), '').Text(LaneField, 'ready'), []);
    LPage := NewNyxPage('lane-policies');
    LDocument.AddPage(LPage);
    LCase := 0;
    LOptions := OptionsFor(NyxTextField('summary')).Scoped(csApplication);
    LOptionsBefore := TNyxKanbanIntent.Configured(LOptions).ToData.ToJSON;
    LOld := NyxCollectionView(Cards).Column(TitleField, 'Original lane');
    for LVersion := 1 to 6 do
    begin
      case LVersion of
        2: LOld := LOld.Selection(nsmMultiple);
        3: LOld := LOld.Query(NyxCollectionQuery.OrderBy(TitleField));
        4: LOld := LOld.TypeAhead(NyxTypeAhead.WindowMilliseconds(1900));
        5: LOld := LOld.ColumnSizing(NyxColumnSizing.Fixed(171));
        6: LOld := LOld.Paged(NyxCollectionPaging.PageSize(5));
      else
        { The first version uses the original, unadorned descriptor. }
      end;
      Check(LOld.ToData.Field('version').AsInteger = LVersion,
        'legacy fixture covers its actual complete descriptor version');
      LOld := TNyxCollectionViewSpec.FromData(LOld.ToData);
      LBefore := LOld.ToData.ToJSON;
      { Reading an effective default must not create an authored declaration. }
      Check(LOld.TableQueryPolicy.IsEnabled and LOld.ColumnAt(0).QueryPolicy.FilterEnabled,
        'ordinary query defaults remain available on legacy descriptors');
      for LLane := 0 to 2 do
      begin
        LNew := NyxKanbanEditorLaneSpec(LOptions, LLane, LOld);
        Check(not LNew.HasTableQueryPolicy and not LNew.ColumnAt(0).HasQueryPolicy and
          (LNew.ToData.Field('version').AsInteger < 7),
          'legacy reconfiguration never promotes resolved defaults to version seven');
        AddLane(LNew);
      end;
      Check(LOld.ToData.ToJSON = LBefore, 'legacy source is unchanged by every lane rebuild');
    end;

    LIndependent := NyxCollectionView(Cards).Column(TitleField, 'Independent lane').
      ColumnQuery(NyxColumnQuery.TextComparison(nqtExact).Operators([nqcStartsWith])).
      TableQuery(NyxTableQuery.SortMode(nqsmSingle));
    LIndependentBefore := LIndependent.ToData.ToJSON;
    for LTableChoice := 0 to 2 do
    begin
      for LColumnChoice := 0 to 2 do
      begin
        LOld := NyxCollectionView(Cards).Column(TitleField, 'Chosen lane');
        case LTableChoice of
          1: LOld := LOld.TableQuery(NyxTableQuery.Enabled(False).Filtering(False).
            Sorting(False).SortMode(nqsmSingle));
          2: LOld := LOld.TableQuery(NyxTableQuery.Filtering(False).SortMode(nqsmMultiple));
        else
          { Absence must be independent of the column declaration. }
        end;
        case LColumnChoice of
          1: LOld := LOld.ColumnQuery(NyxColumnQuery.Filtering(False).Sorting(False).
            Operators([]).TextComparison(nqtAsciiInsensitive));
          2: LOld := LOld.ColumnQuery(NyxColumnQuery.Operators([nqcEqual, nqcContains,
            nqcEndsWith]).Sorting(False).TextComparison(nqtAsciiInsensitive));
        else
          { Absence must be independent of the table declaration. }
        end;
        LBefore := LOld.ToData.ToJSON;
        for LLane := 0 to 2 do
        begin
          LNew := NyxKanbanEditorLaneSpec(LOptions, LLane, LOld);
          Check((LNew.HasTableQueryPolicy = (LTableChoice <> 0)) and
            (LNew.ColumnAt(0).HasQueryPolicy = (LColumnChoice <> 0)) and
            (LNew.TableQueryPolicy.ToData.ToJSON = LOld.TableQueryPolicy.ToData.ToJSON) and
            (LNew.ColumnAt(0).QueryPolicy.ToData.ToJSON = LOld.ColumnAt(0).QueryPolicy.ToData.ToJSON),
            'independent table/column choices retain their exact admitted values');
          Check((LNew.ColumnAt(0).FieldName = 'summary') and
            (LNew.ColumnAt(0).Mode = cmReadOnly) and (LNew.Scope = csApplication) and
            (LNew.SelectionMode = nsmSingle) and
            (LNew.QueryPolicy.ToData.ToJSON = NyxCollectionQuery.Where(NyxWhere(LaneField).
              EqualTo(LOptions.LaneAt(LLane).Name, nqtExact)).ToData.ToJSON),
            'policy preservation retains the intended new lane behavior');
          AddLane(LNew);
          LWire := LNew.ToData.ToJSON;
          NyxKanbanEditorLaneSpec(LOptions, (LLane + 1) mod 3, LIndependent);
          Check((LNew.ToData.ToJSON = LWire) and
            (LIndependent.ToData.ToJSON = LIndependentBefore),
            'another lane cannot mutate the rebuilt result or its independent source');
        end;
        Check(LOld.ToData.ToJSON = LBefore, 'explicit caller specification remains immutable');
      end;
    end;
    { Explicit default-valued declarations differ from absence even when the
      current resolved values happen to match. Future defaults cannot erase
      the author's deliberate decision to pin today's behavior. }
    LOld := NyxCollectionView(Cards).Column(TitleField, 'Declared defaults').
      ColumnQuery(NyxColumnQuery).TableQuery(NyxTableQuery);
    LNew := NyxKanbanEditorLaneSpec(LOptions, 0, LOld);
    Check(LNew.HasTableQueryPolicy and LNew.ColumnAt(0).HasQueryPolicy and
      (LNew.TableQueryPolicy.ToData.ToJSON = LOld.TableQueryPolicy.ToData.ToJSON) and
      (LNew.ColumnAt(0).QueryPolicy.ToData.ToJSON = LOld.ColumnAt(0).QueryPolicy.ToData.ToJSON),
      'explicit default-valued policies remain authored declarations');
    AddLane(LNew);
    Check(TNyxKanbanIntent.Configured(LOptions).ToData.ToJSON = LOptionsBefore,
      'caller recipe options remain immutable');
    LWire := TNyxCodec.Encode(LDocument);
    LDecoded := TNyxCodec.Decode(LWire);
    Check(TNyxCodec.Encode(LDecoded) = LWire, 'all rebuilt lane declarations persist exactly');
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.fixture.kanban.query');
    Check((Pos('.ColumnQuery(', LSource) > 0) and (Pos('.TableQuery(', LSource) > 0) and
      (Pos('.Operators([])', LSource) > 0) and (Pos('SetProp(', LSource) = 0),
      'crafted lane source retains typed policies, including empty disabled operators');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LWire, 'crafted-source admission retains every lane policy');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin

      if FileExists(ParamStr(1)) then
      begin
        raise Exception.Create('Generated lane-policy unit needs a fresh output path');
      end;
      LStream := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LStream.WriteBuffer(LSource[1], Length(LSource));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LWorkspace.Free;
    LParsed.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end;

{ A descriptor can carry Table-only choices, but this public builder promises
  List roles. Reject incompatibility instead of silently removing the choice. }
procedure LaneProjectionCases;
var
  LPrevious: TNyxCollectionViewSpec;
  LBefore: TNyxText;
  LRefused: Boolean;
  LPolicy: TLaneUnsupportedPolicy;
  LReason: TNyxText;
begin
  for LPolicy := Low(TLaneUnsupportedPolicy) to High(TLaneUnsupportedPolicy) do
  begin
    LPrevious := NyxCollectionView(Cards).Column(TitleField, 'Unsupported lane');
    case LPolicy of
      lupColumnLayout: LPrevious := LPrevious.ColumnLayout(NyxColumnLayout);
      lupColumnText: LPrevious := LPrevious.ColumnText(NyxColumnText);
      lupAlignment: LPrevious := LPrevious.ColumnAlignment(NyxColumnAlignment.Horizontal(nchaStart));
      lupTableLayout: LPrevious := LPrevious.TableLayout(NyxTableLayout);
      lupDensity: LPrevious := LPrevious.TablePresentation(NyxTablePresentation);
      lupWorkbench: LPrevious := LPrevious.TableWorkbench(NyxTableWorkbench);
      lupFormat: LPrevious := LPrevious.ColumnFormat(TNyxValueFormat.Canonical);
    end;
    case LPolicy of
      lupColumnLayout, lupTableLayout: LReason := 'Column arrangement requires a table projection';
      lupColumnText, lupAlignment, lupDensity: LReason := 'Table text and density policies require a table projection';
      lupWorkbench: LReason := 'Authored workbench actions require a table projection';
      lupFormat: LReason := 'Column display policies require a table projection';
    end;
    ValidateNyxCollectionViewProjection(LPrevious, cpTable);
    LBefore := LPrevious.ToData.ToJSON;
    LRefused := False;
    try
      NyxKanbanEditorLaneSpec(OptionsFor(TitleField), 0, LPrevious);
    except
      on LError: Exception do
      begin

        if Pos(LReason, LError.Message) = 0 then
        begin
          raise;
        end;
        LRefused := True;
      end;
    end;
    Check(LRefused, 'the public List-role builder refuses unsupported Table-only choices');
    Check(LPrevious.ToData.ToJSON = LBefore, 'projection refusal preserves the caller descriptor');

    if LPolicy = lupAlignment then
    begin
      Check(not LPrevious.ColumnAt(0).AlignmentPolicy.HasVertical,
        'refusal does not manufacture an unconfigured alignment axis');
    end;
  end;
end;

function ActionNode(const AForm: INyxCard; AField: TNyxKanbanEditorField): TNyxNode;
begin
  Result := AForm.Node.Find(NyxKanbanEditorFieldID(AForm.ID, AField));
end;

procedure FormCases;
var
  LFixture: TEditorFixture;
  LForm: INyxCard;
  LOther: INyxCard;
  LLabel: INyxLabel;
  LChange: TNyxKanbanEditorChange;
  LDraft: TNyxKanbanEditorDraft;
  LCopy: TNyxKanbanEditorDraft;
  LBefore: TNyxText;
  LRejected: Boolean;
  LField: TNyxNode;
begin
  LFixture := TEditorFixture.Create;
  try
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LForm := NewNyxKanbanEditor('board-form', LFixture.Document, NyxControl('direct-board'));
    Check(CaptureNyxKanbanEditor(ActionNode(LForm, nkfApply), LForm.Node,
      LFixture.Document, LChange) and (LChange.Intent.Mode = nkiConfigured),
      'the ordinary public form captures its complete effective configured options');
    Check((LChange.Intent.Options.TitleField.Name = 'title') and
      (LChange.Intent.Options.LaneAt(2).Name = 'done') and
      (LChange.Target.Owner.ID = 'direct-board'),
      'displayed choices preserve separately typed exact fields and lane values');
    Check(CaptureNyxKanbanEditor(ActionNode(LForm, nkfDisable), LForm.Node,
      LFixture.Document, LChange) and (LChange.Intent.Mode = nkiDisabled),
      'Disable is an explicit independent action');
    Check(CaptureNyxKanbanEditor(ActionNode(LForm, nkfInherit), LForm.Node,
      LFixture.Document, LChange) and (LChange.Intent.Mode = nkiUndefined),
      'Inherit captures only when an exact local declaration exists');
    LLabel := NewNyxLabel('unrelated-action');
    Check(not CaptureNyxKanbanEditor(LLabel.Node, LForm.Node, LFixture.Document, LChange),
      'unrelated public controls do not masquerade as form actions');
    LForm.Node.Part(NyxPart('lane-0')).SetProp(NyxAttributeName(atValue), '');
    LDraft.Capture('board-form', LForm.Node);
    LCopy := LDraft;
    LDraft.Clear;
    LOther := NewNyxKanbanEditor('board-form', LFixture.Document, NyxControl('direct-board'));
    Check(LCopy.Restore(LOther.Node) and
      (LOther.Node.Part(NyxPart('lane-0')).Prop(NyxAttributeName(atValue)) = ''),
      'copied drafts preserve incomplete text independently of the original record');
    LRejected := False;
    try
      CaptureNyxKanbanEditor(ActionNode(LOther, nkfApply), LOther.Node,
        LFixture.Document, LChange);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LChange.Baseline = ''),
      'incomplete lane input refuses with an unpublished change');
    Check(not LCopy.Restore(nil), 'a missing panel parks the existing draft');
    LOther := NewNyxKanbanEditor('board-form', LFixture.Document, NyxControl('direct-board'));
    Check(LCopy.Restore(LOther.Node), 'a parked draft can return to the same exact context');
    LField := LOther.Node.Part(NyxPart('scope'));
    LOther.Node.Remove(LField);
    LOther.Add(NewNyxLabel(NyxKanbanEditorFieldID('board-form', nkfScope)).
      Configure.PartName(NyxPart('scope')).Done);
    LOther.Node.Part(NyxPart('collection')).
      SetProp(NyxAttributeName(atValue), 'untouched');
    LRejected := False;
    try
      LCopy.Restore(LOther.Node);
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and
      (LOther.Node.Part(NyxPart('collection')).Prop(NyxAttributeName(atValue)) =
        'untouched'), 'draft shape admission precedes every scalar write');
    LRejected := False;
    try
      CaptureNyxKanbanEditor(ActionNode(LForm, nkfDisable), LOther.Node,
        LFixture.Document, LChange);
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'same-spelled actions from another form owner refuse');
    LFixture.Document.Collections.Define(Cards,
      LFixture.Schema.Text(NyxTextField('added'), ''), []);
    LOther := NewNyxKanbanEditor('board-form', LFixture.Document, NyxControl('direct-board'));
    Check(not LCopy.Restore(LOther.Node) and not LCopy.Restore(LForm.Node),
      'schema changes retire drafts without later reviving their old context');
    LRejected := False;
    try
      CaptureNyxKanbanEditor(ActionNode(LForm, nkfDisable), LForm.Node,
        LFixture.Document, LChange);
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'even Disable cannot capture from a stale form baseline');
    LForm := NewNyxKanbanEditor('fresh-form', LFixture.Document, NyxControl('direct-board'));
    LField := LForm.Node.Part(NyxPart('scope'));
    LField.SetProp(NyxAttributeName(atValue), 'unknown scope');
    LRejected := False;
    try
      CaptureNyxKanbanEditor(ActionNode(LForm, nkfApply), LForm.Node,
        LFixture.Document, LChange);
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'unknown closed form choices refuse instead of choosing a default');
    Check(TNyxCodec.Encode(LFixture.Document) <> LBefore,
      'only the explicit schema mutation changed the supplied document');
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LForm := NewNyxKanbanEditor('final-form', LFixture.Document, NyxControl('board-rule'));
    CaptureNyxKanbanEditor(ActionNode(LForm, nkfApply), LForm.Node,
      LFixture.Document, LChange);
    Check((TNyxCodec.Encode(LFixture.Document) = LBefore) and
      (LChange.Target.Path.Name = 'work'),
      'named-part form construction and capture never mutate source, payload or history');
  finally
    LOther := nil;
    LForm := nil;
    LLabel := nil;
    LFixture.Free;
  end;
end;

procedure EmptyCases;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LBoard: INyxKanbanBoard;
  LForm: INyxCard;
  LChange: TNyxKanbanEditorChange;
  LRejected: Boolean;
begin
  LDocument := TNyxDocument.Create;
  try
    LPage := NewNyxPage('empty-page');
    LBoard := NewNyxKanbanBoard('empty-board');
    LPage.Add(LBoard);
    LDocument.AddPage(LPage);
    LForm := NewNyxKanbanEditor('empty-form', LDocument, NyxControl('empty-board'));
    Check(ActionNode(LForm, nkfApply).Prop(NyxAttributeName(atEnabled), 'true') = 'false',
      'a board without collections presents an inert Apply action');
    Check(LForm.Node.Find('empty-form-empty') <> nil,
      'empty configuration explains the missing authored collection prerequisite');
    Check(CaptureNyxKanbanEditor(ActionNode(LForm, nkfDisable), LForm.Node,
      LDocument, LChange) and (LChange.Intent.Mode = nkiDisabled),
      'Disable remains useful on a static board without collections');
    LRejected := False;
    try
      CaptureNyxKanbanEditor(ActionNode(LForm, nkfInherit), LForm.Node,
        LDocument, LChange);
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'no local declaration means no invented inherited operation');
    SetNyxKanbanIntent(LBoard, TNyxKanbanIntent.Disabled);
    LForm := NewNyxKanbanEditor('empty-form', LDocument, NyxControl('empty-board'));
    Check(CaptureNyxKanbanEditor(ActionNode(LForm, nkfInherit), LForm.Node,
      LDocument, LChange) and (LChange.Intent.Mode = nkiUndefined),
      'a local disabled declaration can inherit even without a collection');
    LDocument.Collections.Define(Cards,
      NyxCollectionSchema.Text(TitleField, '').Boolean(NyxBooleanField('lane'), False), []);
    LForm := NewNyxKanbanEditor('empty-form', LDocument, NyxControl('empty-board'));
    Check(ActionNode(LForm, nkfApply).Prop(NyxAttributeName(atEnabled), 'true') = 'false',
      'one text field and one Boolean field cannot silently become two text roles');
  finally
    LForm := nil;
    LBoard := nil;
    LPage := nil;
    LDocument.Free;
  end;
end;

begin
  try
    TargetCases;
    WireCases;
    BaselineCases;
    LaneSpecCases;
    LaneQueryPolicyCases;
    LaneProjectionCases;
    FormCases;
    EmptyCases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-kanban-editor', 'passed');
    document.body.setAttribute('data-kanban-editor-checks', IntToStr(GChecks));
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' Kanban editor checks';
    {$else}
    WriteLn('PASS ', GChecks, ' Kanban editor checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-kanban-editor', 'failed');
      document.body.setAttribute('data-kanban-editor-checks', IntToStr(GChecks));
      document.body.textContent := 'FAIL at check count ' + IntToStr(GChecks) +
        ': ' + LException.Message;
      {$else}
      WriteLn('FAIL at check count ', GChecks, ': ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
