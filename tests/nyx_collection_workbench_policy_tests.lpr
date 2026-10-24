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
program nyx_collection_workbench_policy_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.collections,
  nyx.collections.view, nyx.collections.view.types,
  nyx.collections.workbench.policy, nyx.collections.clipboard.actions,
  nyx.collections.clipboard, nyx.collections.selection,
  nyx.collections.query, nyx.collections.query.policy,
  nyx.collections.columns, nyx.collections.paging,
  nyx.collections.arrangement, nyx.typeahead, nyx.formatting
  {$ifdef PAS2JS}, Web{$endif};

type
  { Actual subscriptions detect accidental live ownership or mutation. They
    borrow this receiver only until both original view/store tokens retire. }
  TReceiver = class
  public
    ViewChanges: Integer;
    StoreChanges: Integer;
    procedure ViewChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure StoreChanged(const AStore: INyxCollection;
      const AChanges: INyxCollectionChanges);
  end;

const
  CTitle: TNyxText = 'Name 🌙' + #0 + ' tail';
  CPrefix: TNyxText = '[🌙';
  CValue: TNyxText = 'accepted 🌙' + #9 + 'quoted "value"' + #13#10 + #0 + 'end';
  { Frozen bytes from the pre-workbench descriptor implementation, captured
    through a byte stream rather than an ANSI console. These do not call the
    current serializer to manufacture their expected legacy representations. }
  CLegacy: array[1..9] of TNyxText = (
    '{ "version" : 1, "key" : "policy-rows", "scope" : "instance", "parent" : "", "columns" : [{ "field" : "name", "kind" : "text", "title" : "Name 🌙\u0000 tail", "editable" : true }] }',
    '{ "version" : 2, "key" : "policy-rows", "scope" : "instance", "parent" : "", "columns" : [{ "field" : "name", "kind" : "text", "title" : "Name 🌙\u0000 tail", "editable" : true }], "selection" : "multiple" }',
    '{ "version" : 3, "key" : "policy-rows", "scope" : "instance", "parent" : "", "columns" : [{ "field" : "name", "kind" : "text", "title" : "Name 🌙\u0000 tail", "editable" : true }], "selection" : "single", "query" : { "version" : 1, "filter" : null, "order" : [{ "field" : "name", "kind" : "text", "direction" : "descending", "textComparison" : "exact" }] } }',
    '{ "version" : 4, "key" : "policy-rows", "scope" : "instance", "parent" : "", "columns" : [{ "field" : "name", "kind" : "text", "title" : "Name 🌙\u0000 tail", "editable" : true }], "selection" : "single", "query" : null, "typeAhead" : { "version" : 1, "enabled" : false, "windowMS" : 1000, "match" : "folded" } }',
    '{ "version" : 5, "key" : "policy-rows", "scope" : "instance", "parent" : "", "columns" : [{ "field" : "name", "kind" : "text", "title" : "Name 🌙\u0000 tail", "editable" : true, "sizing" : { "version" : 1, "mode" : "fixed", "value" : 160, "minimum" : 1, "maximum" : 100000 } }], "selection" : "single", "query" : null, "typeAhead" : null }',
    '{ "version" : 6, "key" : "policy-rows", "scope" : "instance", "parent" : "", "columns" : [{ "field" : "name", "kind" : "text", "title" : "Name 🌙\u0000 tail", "editable" : true }], "selection" : "single", "query" : null, "typeAhead" : null, "paging" : { "version" : 1, "pageSize" : 12, "initialPage" : 1 } }',
    '{ "version" : 7, "key" : "policy-rows", "scope" : "instance", "parent" : "", "columns" : [{ "field" : "name", "kind" : "text", "title" : "Name 🌙\u0000 tail", "editable" : true, "sizing" : null, "queryControls" : { "version" : 1, "filtering" : false, "sorting" : true, "operators" : [], "textComparison" : "exact" } }], "selection" : "single", "query" : null, "typeAhead" : null, "paging" : null, "queryControls" : null }',
    '{ "version" : 8, "key" : "policy-rows", "scope" : "instance", "parent" : "", "columns" : [{ "field" : "name", "kind" : "text", "title" : "Name 🌙\u0000 tail", "editable" : true, "sizing" : null, "queryControls" : null, "layoutControls" : { "version" : 1, "visible" : false, "pin" : "end", "features" : ["resize", "auto-size", "reorder", "visibility", "pinning"] } }], "selection" : "single", "query" : null, "typeAhead" : null, "paging" : null, "queryControls" : null, "layoutControls" : { "version" : 1, "enabled" : true, "features" : ["resize", "auto-size", "reorder", "visibility", "pinning"], "minimumCenter" : 96, "autoSizeRows" : 64 } }',
    '{ "version" : 9, "key" : "policy-rows", "scope" : "instance", "parent" : "", "columns" : [{ "field" : "name", "kind" : "text", "title" : "Name 🌙\u0000 tail", "editable" : true, "sizing" : null, "queryControls" : null, "layoutControls" : null, "display" : { "version" : 1, "name" : "nyx.canonical", "revision" : 1, "locale" : { "name" : "", "decimal" : ".", "group" : "" }, "style" : "text-affixes", "prefix" : "[🌙", "suffix" : "]" } }], "selection" : "single", "query" : null, "typeAhead" : null, "paging" : null, "queryControls" : null, "layoutControls" : null }');

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TReceiver.ViewChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(ViewChanges);
end;

procedure TReceiver.StoreChanged(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
  Inc(StoreChanges);
end;

function Replacing(const AData: TNyxDataValue; const AKey: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AData.Count);
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) = AKey then
    begin
      LFields[LIndex] := NyxField(AKey, AValue);
    end
    else
    begin
      LFields[LIndex] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));
    end;
  end;
  Result := NyxObject(LFields);
end;

function Without(const AData: TNyxDataValue; const AKey: TNyxText): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
begin
  SetLength(LFields, AData.Count);
  LCount := 0;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) <> AKey then
    begin
      LFields[LCount] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));
      Inc(LCount);
    end;
  end;
  SetLength(LFields, LCount);
  Result := NyxObject(LFields);
end;

function LegacySpec(AVersion: Integer): TNyxCollectionViewSpec;
var
  LBase: TNyxCollectionViewSpec;
begin
  LBase := NyxCollectionView(NyxCollection('policy-rows'))
    .Column(NyxTextField('name'), CTitle, cmEditable).Scoped(csInstance);
  Result := LBase;
  case AVersion of
    2:
      begin
        Result := LBase.Selection(nsmMultiple);
      end;
    3:
      begin
        Result := LBase.Query(NyxCollectionQuery.OrderBy(NyxTextField('name'), nsdDescending));
      end;
    4:
      begin
        Result := LBase.TypeAhead(NyxTypeAhead.Enabled(False));
      end;
    5:
      begin
        Result := LBase.ColumnSizing(NyxColumnSizing.Fixed(160));
      end;
    6:
      begin
        Result := LBase.Paged(NyxCollectionPaging.PageSize(12));
      end;
    7:
      begin
        Result := LBase.ColumnQuery(NyxColumnQuery.Filtering(False).Operators([]));
      end;
    8:
      begin
        Result := LBase.TableLayout(NyxTableLayout.MinimumCenter(96))
          .ColumnLayout(NyxColumnLayout.Visible(False).PinEnd);
      end;
    9:
      begin
        Result := LBase.ColumnFormat(NyxValueFormat.TextAffixes(CPrefix, ']'));
      end;
  end;
end;

procedure RefusesPolicy(const AData: TNyxDataValue; const AReason: TNyxText);
var
  LPolicy: TNyxTableWorkbenchPolicy;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LPolicy := NyxTableWorkbench.RangeSelection(True).Headers(False).FocusedDraft(True);
  LBefore := LPolicy.ToData.ToJSON;
  LRefused := False;
  try
    LPolicy := TNyxTableWorkbenchPolicy.FromData(AData);
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LPolicy.ToData.ToJSON = LBefore), AReason + '; accepted policy remains exact');
end;

procedure RefusesSpec(const AData: TNyxDataValue; const AReason: TNyxText);
var
  LSpec: TNyxCollectionViewSpec;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LSpec := LegacySpec(9).TableWorkbench(NyxTableWorkbench.RangeSelection(True));
  LBefore := LSpec.ToData.ToJSON;
  LRefused := False;
  try
    LSpec := TNyxCollectionViewSpec.FromData(AData);
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LSpec.ToData.ToJSON = LBefore), AReason + '; accepted descriptor remains exact');
end;

procedure PolicyCases;
var
  LDefault: TNyxTableWorkbenchPolicy;
  LPolicy: TNyxTableWorkbenchPolicy;
  LDecoded: TNyxTableWorkbenchPolicy;
  LOptions: TNyxCollectionClipboardOptions;
  LData: TNyxDataValue;
  LActions: TNyxWorkbenchCopyActions;
  LAction: TNyxWorkbenchCopyAction;
  LMask: Integer;
  LIndex: Integer;
  LCount: Integer;
  LRefused: Boolean;
begin
  LDefault := NyxTableWorkbench;
  LOptions := TNyxCollectionClipboardOptions.Defaults;
  Check(LDefault.Defined and not LDefault.RangeSelectionEnabled and
    not LDefault.HasFocusedDraft and
    (LDefault.CopyActions = [nwcaCell, nwcaSelectedSource, nwcaSelectedPage]) and
    (LOptions.Actions = [nccaCell, nccaSelectedSource, nccaSelectedPage]) and
    (LDefault.FieldChoice = nwcfVisiblePresentation) and
    (LOptions.FieldChoice = nccfVisiblePresentation) and
    (LDefault.IncludeHeaders = LOptions.IncludeHeaders) and LDefault.IncludeHeaders and
    (LDefault.Lines = nwleLF) and (LOptions.Lines = nccleLF),
    'explicit author defaults exactly match current runtime copy choices without enabling range or draft input');
  LDecoded := TNyxTableWorkbenchPolicy.FromData(NyxNull);
  Check(not LDecoded.Defined and (LDecoded.ToData.Kind = ndNull),
    'null policy is absent independently of explicit factory defaults');
  LRefused := False;
  try
    LDecoded := LDecoded.Headers(False);
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LDecoded.Defined,
    'an absent value cannot silently become an authored policy through a setter');
  for LMask := 0 to 15 do
  begin
    LActions := [];
    LCount := 0;
    for LAction := Low(TNyxWorkbenchCopyAction) to High(TNyxWorkbenchCopyAction) do
    begin

      if (LMask and (1 shl Ord(LAction))) <> 0 then
      begin
        Include(LActions, LAction);
        Inc(LCount);
      end;
    end;
    LPolicy := LDefault.AllowCopy(LActions).RangeSelection((LMask and 1) <> 0)
      .CopyFields(nwcfAllArrangement).Headers(False).LineEnding(nwleCRLF).FocusedDraft(True);
    LData := LPolicy.ToData;
    LDecoded := TNyxTableWorkbenchPolicy.FromData(LData);
    Check((LData.Count = 7) and (LData.Field('copyActions').Count = LCount) and
      (LDecoded.CopyActions = LActions) and (LDecoded.ToData.ToJSON = LData.ToJSON) and
      (LDecoded.FieldChoice = nwcfAllArrangement) and (LDecoded.Lines = nwleCRLF) and
      not LDecoded.IncludeHeaders and LDecoded.HasFocusedDraft and
      (LDefault.ToData.Field('copyFields').AsText = 'visible-presentation') and
      not LDefault.HasFocusedDraft,
      'every closed permission combination roundtrips independently without mutating its default source');
    for LAction := Low(TNyxWorkbenchCopyAction) to High(TNyxWorkbenchCopyAction) do
    begin
      Check(LDecoded.AllowsCopy(LAction) = (LAction in LActions),
        'typed permission query matches exact captured membership');
    end;
  end;
  LPolicy := LDefault.AllowCopy([]).RangeSelection(True).FocusedDraft(True);
  Check((LPolicy.CopyActions = []) and LPolicy.RangeSelectionEnabled and LPolicy.HasFocusedDraft,
    'range and draft inspection flags do not implicitly grant a copy permission');
  LPolicy := LDefault.AllowCopy([nwcaCellRange]);
  Check(LPolicy.AllowsCopy(nwcaCellRange) and not LPolicy.RangeSelectionEnabled and
    not LPolicy.HasFocusedDraft,
    'range copy permission does not implicitly enable range input or focused draft capture');
  LData := LDefault.ToData;
  LDecoded := TNyxTableWorkbenchPolicy.FromData(Replacing(LData, 'copyActions',
    NyxArray([NyxData('selected-page'), NyxData('cell')])));
  Check((LDecoded.CopyActions = [nwcaCell, nwcaSelectedPage]) and
    (LDecoded.ToData.Field('copyActions').Item(0).AsText = 'cell'),
    'action input order has canonical closed-set output order');
  RefusesPolicy(NyxData(True), 'scalar cannot stand in for a complete policy');
  RefusesPolicy(Replacing(LData, 'version', NyxData(2)), 'future versions refuse');
  RefusesPolicy(Replacing(LData, 'version', NyxData('1')), 'version is typed integer');
  RefusesPolicy(Replacing(LData, 'rangeSelection', NyxData('true')), 'range enablement is Boolean');
  RefusesPolicy(Replacing(LData, 'headers', NyxData(1)), 'headers is Boolean');
  RefusesPolicy(Replacing(LData, 'focusedDraft', NyxNull), 'draft opt-in cannot be null');
  RefusesPolicy(Replacing(LData, 'copyFields', NyxData('all-schema')), 'all arrangement is never relabelled as unbound schema fields');
  RefusesPolicy(Replacing(LData, 'lineEnding', NyxData(#13#10)), 'saved line ending uses its closed name');
  RefusesPolicy(Replacing(LData, 'copyActions', NyxData('cell')), 'permissions require an array');
  RefusesPolicy(Replacing(LData, 'copyActions', NyxArray([NyxData('cell'), NyxData('cell')])),
    'duplicate actions refuse');
  RefusesPolicy(Replacing(LData, 'copyActions', NyxArray([NyxData('CELL')])), 'unknown action spellings refuse');
  RefusesPolicy(Replacing(LData, 'copyActions', NyxArray([NyxData(0)])), 'actions require closed text names');
  RefusesPolicy(Replacing(LData, 'copyActions', NyxArray([NyxData('cell'), NyxData('selected-source'),
    NyxData('selected-page'), NyxData('cell-range'), NyxData('other')])), 'action scan is bounded to four choices');
  for LIndex := 0 to LData.Count - 1 do
  begin
    RefusesPolicy(Without(LData, LData.Key(LIndex)), 'each policy member is required');
  end;
  RefusesPolicy(NyxObject([NyxField('version', NyxData(1)),
    NyxField('rangeSelection', NyxData(False)), NyxField('copyActions', NyxArray([])),
    NyxField('copyFields', NyxData('visible-presentation')), NyxField('headers', NyxData(True)),
    NyxField('lineEnding', NyxData('lf')), NyxField('runtimeHost', NyxData('forbidden'))]),
    'unknown runtime authority cannot replace an authored member');
end;

procedure DescriptorCases;
var
  LVersion: Integer;
  LBase: TNyxCollectionViewSpec;
  LSpec: TNyxCollectionViewSpec;
  LDecoded: TNyxCollectionViewSpec;
  LPeer: TNyxCollectionViewSpec;
  LPolicy: TNyxTableWorkbenchPolicy;
  LData: TNyxDataValue;
  LColumns: TNyxDataValue;
  LRefused: Boolean;
begin
  LPolicy := NyxTableWorkbench.RangeSelection(True).AllowCopy([nwcaCell, nwcaCellRange])
    .CopyFields(nwcfAllArrangement).Headers(False).LineEnding(nwleCRLF).FocusedDraft(True);
  for LVersion := 1 to 9 do
  begin
    LBase := LegacySpec(LVersion);
    Check((LBase.ToData.ToJSON = CLegacy[LVersion]) and not LBase.HasTableWorkbenchPolicy,
      'legacy version bytes stay exact without the new policy');
    LDecoded := TNyxCollectionViewSpec.FromData(TNyxDataValue.ParseJSON(CLegacy[LVersion]));
    Check((LDecoded.ToData.ToJSON = CLegacy[LVersion]) and
      (LDecoded.ColumnAt(0).Title = CTitle),
      'legacy saved wire and supplementary Unicode/NUL title roundtrip exactly');
    LSpec := LBase.TableWorkbench(LPolicy);
    LPeer := LSpec.Copy;
    LData := LSpec.ToData;
    Check((LData.Field('version').AsInteger = 10) and (LData.Count = 12) and
      (LData.Field('columns').Item(0).Count = 8) and
      (LData.Field('workbench').ToJSON = LPolicy.ToData.ToJSON),
      'authored presence selects one complete version-ten envelope');
    LDecoded := TNyxCollectionViewSpec.FromData(LData);
    Check((LDecoded.ToData.ToJSON = LData.ToJSON) and LDecoded.HasTableWorkbenchPolicy and
      (LDecoded.TableWorkbenchPolicy.ToData.ToJSON = LPolicy.ToData.ToJSON),
      'version ten preserves all preceding independent descriptor choices');
    LSpec := LSpec.UseDefaultTableWorkbench;
    Check((LSpec.ToData.ToJSON = CLegacy[LVersion]) and LPeer.HasTableWorkbenchPolicy and
      (LPeer.ToData.ToJSON = LData.ToJSON) and (LBase.ToData.ToJSON = CLegacy[LVersion]),
      'policy-only reset restores exact predecessor wire and cannot mutate a copied peer');
  end;
  LBase := LegacySpec(1);
  LSpec := LBase.TableWorkbench(NyxTableWorkbench);
  Check(LSpec.HasTableWorkbenchPolicy and not LBase.HasTableWorkbenchPolicy and
    (LSpec.TableWorkbenchPolicy.ToData.ToJSON = LBase.TableWorkbenchPolicy.ToData.ToJSON) and
    (LBase.ToData.Field('version').AsInteger = 1) and
    (LSpec.ToData.Field('version').AsInteger = 10),
    'absent and explicit default resolve equally but retain different authored presence');
  LDecoded := Default(TNyxCollectionViewSpec);
  LRefused := False;
  try
    LDecoded := LDecoded.TableWorkbench(LPolicy);
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LDecoded.Defined, 'an absent binding cannot acquire a workbench descriptor');
  LRefused := False;
  try
    LSpec := LSpec.TableWorkbench(Default(TNyxTableWorkbenchPolicy));
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and LSpec.HasTableWorkbenchPolicy, 'undefined policy admission leaves the previous binding intact');
  LData := LSpec.ToData;
  RefusesSpec(Replacing(LData, 'workbench', NyxNull), 'version ten requires actual authored presence');
  RefusesSpec(Without(LData, 'workbench'), 'the workbench member is required');
  RefusesSpec(Replacing(LData, 'version', NyxData(11)), 'unknown future descriptor refuses');
  RefusesSpec(Replacing(LData, 'workbench', NyxData(False)), 'descriptor policy is a typed object');
  RefusesSpec(Replacing(LData, 'workbench', Replacing(LData.Field('workbench'), 'headers', NyxData('yes'))),
    'a nested policy scalar refusal is atomic for the accepted descriptor');
  RefusesSpec(Replacing(LData, 'layoutControls', NyxData('default')), 'unrelated nullable policies remain strict');
  LColumns := LData.Field('columns');
  RefusesSpec(Replacing(LData, 'columns', NyxArray([Without(LColumns.Item(0), 'display')])),
    'version-ten columns require their complete nullable display slot');
  RefusesSpec(Replacing(LData, 'columns', NyxArray([Replacing(LColumns.Item(0), 'display', NyxData('RAW'))])),
    'unrelated column policies remain typed');
  RefusesSpec(Replacing(LData, 'version', NyxData(9)), 'workbench data cannot hide inside old wire versions');
end;

procedure ProjectionAndLifetimeCases;
var
  LStore: INyxCollection;
  LSource: INyxCollectionSnapshot;
  LView: INyxCollectionView;
  LViewToken: INyxCollectionViewSubscription;
  LStoreToken: INyxCollectionSubscription;
  LSpec: TNyxCollectionViewSpec;
  LRetained: TNyxCollectionViewSpec;
  LPolicy: TNyxTableWorkbenchPolicy;
  LReceiver: TReceiver;
  LProjection: TNyxCollectionProjection;
  LRefused: Boolean;
begin
  LReceiver := TReceiver.Create;
  try
    LStore := NewNyxCollection(NyxCollection('policy-rows'),
      NyxCollectionSchema.Text(NyxTextField('name'), ''),
      [NyxCollectionItem(NyxItem(NyxCollection('policy-rows'), 'first'))
        .WithValue(NyxTextField('name'), CValue)]);
    LSource := LStore.Snapshot;
    LSpec := LegacySpec(1).TableWorkbench(NyxTableWorkbench.RangeSelection(True));
    ValidateNyxCollectionDescriptorSnapshot(LSource, LSpec);
    Check(LSource.ItemAt(0).GetValue(NyxTextField('name')) = CValue,
      'neutral descriptor admission validates data without guessing a table projection or changing exact values');
    for LProjection := cpList to cpTree do
    begin

      if LProjection = cpTable then
      begin
        Continue;
      end;
      LRefused := False;
      try
        ValidateNyxCollectionViewProjection(LSpec, LProjection);
      except
        on E: ENyxCollection do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'known non-table projection refuses authored table workbench actions');
      LRefused := False;
      try
        LView := NewNyxCollectionView(LStore, LSpec, LProjection);
      except
        on E: ENyxCollection do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LView = nil), 'actual live non-table view refuses before retaining its owner');
    end;
    ValidateNyxCollectionViewProjection(LSpec, cpTable);
    LView := NewNyxCollectionView(LStore, LSpec, cpTable);
    LViewToken := LView.Subscribe({$ifdef PAS2JS}@{$endif}LReceiver.ViewChanged);
    LStoreToken := LStore.Subscribe({$ifdef PAS2JS}@{$endif}LReceiver.StoreChanged);
    LRetained := LView.Spec.Copy;
    LPolicy := LRetained.TableWorkbenchPolicy.Copy;
    Check((LStore.Snapshot = LSource) and (LReceiver.ViewChanges = 0) and
      (LReceiver.StoreChanges = 0) and (LPolicy.ToData.Field('copyActions').Count = 3),
      'pure authored capture invokes no write, selection, mutation or observer');
    LView := nil;
    LStore := nil;
    Check(not LViewToken.Connected and not LStoreToken.Connected and
      LPolicy.RangeSelectionEnabled and LRetained.HasTableWorkbenchPolicy and
      (LRetained.ColumnAt(0).Title = CTitle) and
      (LSource.ItemAt(0).GetValue(NyxTextField('name')) = CValue),
      'retained copied policy/descriptor/data hold no live view or store owner');
  finally
    LViewToken := nil;
    LStoreToken := nil;
    LView := nil;
    LStore := nil;
    LReceiver.Free;
  end;
end;

{ Exhaust independent export choices without lending them clipboard settings.
  Malformed nested values must fail before replacing an already accepted policy. }
procedure ExportCases;
var
  LMask: Integer;
  LFields: TNyxWorkbenchCopyFields;
  LLines: TNyxWorkbenchLineEnding;
  LHeaders: Boolean;
  LAllowed: TNyxWorkbenchExportActions;
  LExport: TNyxTableExportPolicy;
  LPolicy: TNyxTableWorkbenchPolicy;
  LDecoded: TNyxTableWorkbenchPolicy;
  LData: TNyxDataValue;
  LBaseline: TNyxText;
begin
  LBaseline := NyxTableWorkbench.CopyFields(nwcfAllArrangement).Headers(False).ToData.ToJSON;
  for LMask := 0 to 3 do
  begin
    LAllowed := [];

    if (LMask and 1) <> 0 then
    begin
      Include(LAllowed, nweaCurrentPage);
    end;

    if (LMask and 2) <> 0 then
    begin
      Include(LAllowed, nweaAllFiltered);
    end;
    for LFields := Low(TNyxWorkbenchCopyFields) to High(TNyxWorkbenchCopyFields) do
    begin
      for LLines := Low(TNyxWorkbenchLineEnding) to High(TNyxWorkbenchLineEnding) do
      begin
        for LHeaders := False to True do
        begin
          LExport := NyxTableExport.Allow(LAllowed).Columns(LFields)
            .Headers(LHeaders).LineEnding(LLines);
          Check(TNyxTableExportPolicy.FromData(LExport.ToData).ToData.ToJSON = LExport.ToData.ToJSON,
            'independent export choices have one exact strict portable representation');
          LPolicy := NyxTableWorkbench.CopyFields(nwcfAllArrangement).Headers(False).Export(LExport);
          LData := LPolicy.ToData;
          LDecoded := TNyxTableWorkbenchPolicy.FromData(LData);
          Check((LData.Field('version').AsInteger = 2) and
            (LDecoded.ExportPolicy.Actions = LAllowed) and
            (LDecoded.ExportPolicy.FieldChoice = LFields) and
            (LDecoded.ExportPolicy.Lines = LLines) and
            (LDecoded.ExportPolicy.IncludeHeaders = LHeaders),
            'version two retains explicit export permissions, including an empty set');
          Check(LDecoded.UseDefaultExport.ToData.ToJSON = LBaseline,
            'reset only export reproduces the exact version-one clipboard policy');
        end;
      end;
    end;
  end;
  LData := NyxTableWorkbench.Export(NyxTableExport).ToData;
  RefusesPolicy(Replacing(LData, 'export', NyxNull), 'version two requires actual export presence');
  RefusesPolicy(Without(LData, 'export'), 'missing export is not an implicit default');
  RefusesPolicy(Replacing(LData, 'version', NyxData(1)), 'version one cannot conceal export data');
  RefusesPolicy(Replacing(LData, 'export',
    Replacing(LData.Field('export'), 'actions', NyxArray([NyxData('all-filtered'), NyxData('all-filtered')]))),
    'duplicate export permissions refuse atomically');
  RefusesPolicy(Replacing(LData, 'export',
    Replacing(LData.Field('export'), 'actions', NyxArray([NyxData('selected-source')]))),
    'clipboard membership cannot masquerade as export membership');
  RefusesPolicy(Replacing(LData, 'export',
    Replacing(LData.Field('export'), 'headers', NyxData(1))),
    'export headers require a Boolean');
  RefusesPolicy(Replacing(LData, 'export',
    Replacing(LData.Field('export'), 'lineEnding', NyxData('native'))),
    'export row separators remain closed portable choices');
end;

begin
  try
    PolicyCases;
    ExportCases;
    DescriptorCases;
    ProjectionAndLifetimeCases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-workbench-policy', 'passed');
    document.body.setAttribute('data-workbench-policy-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' pure table workbench policy checks; no runtime service wiring');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-workbench-policy', 'failed');
      document.body.setAttribute('data-workbench-policy-checks', IntToStr(GChecks));
      document.body.setAttribute('data-workbench-policy-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
