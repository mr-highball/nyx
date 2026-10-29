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



program nyx_agent_collection_schema;
{$mode delphi}{$H+}{$codepage utf8}

uses
  { BEGIN QUERY_POLICY_SCHEMA_IMPORTS }
  nyx.studio.collectionintent, nyx.studio.stateedits,
  { END QUERY_POLICY_SCHEMA_IMPORTS }
  { BEGIN QUERY_CONTROL_DISCOVERY_IMPORTS }
  nyx.collections, nyx.collections.view.types, nyx.collections.columns,
  nyx.collections.paging, nyx.collections.query, nyx.collections.query.policy,
  nyx.collections.arrangement, nyx.formatting, nyx.collections.workbench.policy,
  nyx.collections.presentation, nyx.collections.alignment, nyx.collections.inspection,
  nyx.collections.recovery.policy,
  nyx.studio.collectionedits,
  { END QUERY_CONTROL_DISCOVERY_IMPORTS }
  SysUtils, nyx.text, nyx.data, nyx.studio.mcp, nyx.studio.agents;

var
  LTools: TNyxDataValue;
  LTool: TNyxDataValue;
  LSchema: TNyxDataValue;
  LVariant: TNyxDataValue;
  LChanges: TNyxDataValue;
  LChange: TNyxDataValue;
  LProperties: TNyxDataValue;
  LDefinition: TNyxDataValue;
  LIndex: Integer;
  LMode: Integer;
  LChecks: Integer;
  LFound: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Collection discovery: ' + AReason);
  end;
  Inc(LChecks);
end;

{ BEGIN QUERY_CONTROL_DISCOVERY_CASES }
{ Compare the real discovery builder with the existing portable contract, then
  admit/refuse independently constructed descriptors through the public codec.
  This checks current shapes without implementing another schema validator. }
procedure CurrentPolicyDiscovery(const ATool: TNyxDataValue);
var
  LActual: TNyxDataValue;
  LPortable: TNyxDataValue;
  LBranch: TNyxDataValue;
  LProperties: TNyxDataValue;
  LSpecs: TNyxDataValue;
  LSpecSchema: TNyxDataValue;
  LColumnSchema: TNyxDataValue;
  LData: TNyxDataValue;
  LColumn: TNyxDataValue;
  LPolicy: TNyxDataValue;
  LBad: TNyxDataValue;
  LBind: TNyxDataValue;
  LBase: TNyxCollectionViewSpec;
  LSpec: TNyxCollectionViewSpec;
  LDecoded: TNyxCollectionViewSpec;
  LIndex: Integer;
  LCount: Integer;
  LLayoutCount: Integer;
  LFormatCount: Integer;
  LWorkbenchCount: Integer;
  LPresentationCount: Integer;
  LAlignmentCount: Integer;
  LVersion: Integer;
  LMode: TNyxText;

  function Required(const ASchema: TNyxDataValue; const AName: TNyxText): Boolean;
  var
    LAt: Integer;
  begin
    Result := False;
    for LAt := 0 to ASchema.Field('required').Count - 1 do
    begin

      if ASchema.Field('required').Item(LAt).AsText = AName then
      begin
        Exit(True);
      end;
    end;
  end;

  function Replacing(const AObject: TNyxDataValue; const AName: TNyxText;
    const AValue: TNyxDataValue): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
    LAt: Integer;
  begin
    SetLength(LFields, AObject.Count);
    for LAt := 0 to AObject.Count - 1 do
    begin

      if AObject.Key(LAt) = AName then
      begin
        LFields[LAt] := NyxField(AName, AValue);
      end
      else
      begin
        LFields[LAt] := NyxField(AObject.Key(LAt), AObject.Field(AObject.Key(LAt)));
      end;
    end;
    Result := NyxObject(LFields);
  end;

  procedure RefusesView(const AData: TNyxDataValue; const AReason: TNyxText);
  var
    LRejected: Boolean;
    LParsed: TNyxCollectionViewSpec;
  begin
    LRejected := False;
    try
      LParsed := TNyxCollectionViewSpec.FromData(AData);
      LParsed.Validate;
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, AReason);
  end;

begin
  LActual := ATool.Field('inputSchema').Field('oneOf');
  LPortable := NyxCollectionAgentSchema.Field('oneOf');
  LCount := 0;
  LLayoutCount := 0;
  LFormatCount := 0;
  LWorkbenchCount := 0;
  LPresentationCount := 0;
  LAlignmentCount := 0;
  for LIndex := 0 to LActual.Count - 1 do
  begin
    LBranch := LActual.Item(LIndex);
    LProperties := LBranch.Field('properties');
    LMode := LProperties.Field('mode').Field('const').AsText;

    if NyxAgentHas(LProperties, 'alignmentControls') then
    begin
      Inc(LAlignmentCount);
      Check((LProperties.Field('alignmentControls').Field('type').AsText = 'boolean') and
        not LProperties.Field('alignmentControls').Field('default').AsBoolean and
        not Required(LBranch, 'alignmentControls') and
        ((LMode = 'bindings') or (LMode = 'column')),
        'alignment reads remain optional bounded Boolean selectors');
    end;

    if NyxAgentHas(LProperties, 'presentationControls') then
    begin
      Inc(LPresentationCount);
      Check((LProperties.Field('presentationControls').Field('type').AsText = 'boolean') and
        not LProperties.Field('presentationControls').Field('default').AsBoolean and
        not Required(LBranch, 'presentationControls') and
        ((LProperties.Field('mode').Field('const').AsText = 'bindings') or
          (LProperties.Field('mode').Field('const').AsText = 'column')),
        'presentation reads are opt-in bounded Boolean selectors on binding and column modes');
    end;

    if NyxAgentHas(LProperties, 'workbenchControls') then
    begin
      Inc(LWorkbenchCount);
      Check((LMode = 'bindings') and
        (LProperties.Field('workbenchControls').Field('type').AsText = 'boolean') and
        not LProperties.Field('workbenchControls').Field('default').AsBoolean and
        not Required(LBranch, 'workbenchControls') and
        (LProperties.Field('workbenchControls').ToJSON = LPortable.Item(LIndex)
          .Field('properties').Field('workbenchControls').ToJSON),
        'workbench discovery is an optional bounded Boolean on binding reads');
    end
    else
    begin
      Check((LMode <> 'bindings') and
        not NyxAgentHas(LPortable.Item(LIndex).Field('properties'), 'workbenchControls'),
        'other closed modes do not advertise table action metadata');
    end;

    if NyxAgentHas(LProperties, 'formatControls') then
    begin
      Inc(LFormatCount);
      Check(((LMode = 'bindings') or (LMode = 'column')) and
        (LProperties.Field('formatControls').Field('type').AsText = 'boolean') and
        not LProperties.Field('formatControls').Field('default').AsBoolean and
        not Required(LBranch, 'formatControls'),
        'display discovery is an optional typed bounded selector on ' + LMode);
    end
    else
    begin
      Check((LMode <> 'bindings') and (LMode <> 'column'),
        'other modes retain closed shapes without a display selector');
    end;

    if NyxAgentHas(LProperties, 'layoutControls') then
    begin
      Inc(LLayoutCount);
      Check(((LMode = 'bindings') or (LMode = 'column')) and
        (LProperties.Field('layoutControls').Field('type').AsText = 'boolean') and
        not LProperties.Field('layoutControls').Field('default').AsBoolean and
        not Required(LBranch, 'layoutControls') and
        (LProperties.Field('layoutControls').ToJSON = LPortable.Item(LIndex)
          .Field('properties').Field('layoutControls').ToJSON),
        'layout discovery is an optional strict Boolean bounded selector on ' + LMode);
    end
    else
    begin
      Check((LMode <> 'bindings') and (LMode <> 'column') and
        not NyxAgentHas(LPortable.Item(LIndex).Field('properties'), 'layoutControls'),
        'other closed modes do not advertise layout policy reads ' + IntToStr(LIndex));
    end;

    if NyxAgentHas(LProperties, 'queryControls') then
    begin
      Inc(LCount);
      Check(((LMode = 'bindings') or (LMode = 'column')) and
        (LProperties.Field('queryControls').Field('type').AsText = 'boolean') and
        not LProperties.Field('queryControls').Field('default').AsBoolean and
        not Required(LBranch, 'queryControls') and
        (LProperties.Field('queryControls').ToJSON = LPortable.Item(LIndex)
          .Field('properties').Field('queryControls').ToJSON),
        'actual discovery keeps an optional strict Boolean selector on ' + LMode);
    end
    else
    begin
      Check((LMode <> 'bindings') and (LMode <> 'column') and
        not NyxAgentHas(LPortable.Item(LIndex).Field('properties'), 'queryControls'),
        'other closed modes do not advertise the policy selector ' + IntToStr(LIndex));
    end;
  end;
  Check(LCount = 2, 'only bindings and exact column reads expose queryControls');
  Check(LLayoutCount = 2, 'only bindings and exact column reads expose layoutControls');
  Check(LFormatCount = 2, 'only bindings and exact column reads expose formatControls');
  Check(LWorkbenchCount = 1, 'only binding reads expose complete table action choices');
  Check(LPresentationCount = 2, 'only bounded binding and exact column reads expose presentation');
  LSpecs := LActual.Item(LActual.Count - 1).Field('properties').Field('changes')
    .Field('items').Field('oneOf').Item(5).Field('properties').Field('spec').Field('oneOf');
  LSpecSchema := LSpecs.Item(4);
  LColumnSchema := LSpecSchema.Field('properties').Field('columns').Field('items');
  Check((LSpecSchema.Field('properties').Field('version').Field('const').AsInteger = 5) and
    (LSpecSchema.Field('required').Count = 8) and
    (LColumnSchema.Field('properties').Field('sizing').Field('required').Count = 5) and
    Required(LSpecSchema.Field('properties').Field('columns').Field('contains'), 'sizing'),
    'version five requires a real sizing declaration and its strict scalar policy');
  LSpecSchema := LSpecs.Item(5);
  LProperties := LSpecSchema.Field('properties').Field('paging');
  Check((LSpecSchema.Field('properties').Field('version').Field('const').AsInteger = 6) and
    (LSpecSchema.Field('required').Count = 9) and Required(LSpecSchema, 'paging') and
    (LProperties.Field('required').Count = 3) and
    (LProperties.Field('properties').Field('pageSize').Field('minimum').AsInteger = 1) and
    (LProperties.Field('properties').Field('initialPage').Field('minimum').AsInteger = 1),
    'version six carries independent positive paging choices');
  LSpecSchema := LSpecs.Item(6);
  LColumnSchema := LSpecSchema.Field('properties').Field('columns').Field('items');
  Check((LSpecSchema.Field('properties').Field('version').Field('const').AsInteger = 7) and
    (LSpecSchema.Field('required').Count = 10) and
    (LColumnSchema.Field('required').Count = 6) and
    Required(LColumnSchema, 'sizing') and Required(LColumnSchema, 'queryControls') and
    (LColumnSchema.Field('properties').Field('sizing').Field('anyOf').Item(0)
      .Field('type').AsText = 'null') and
    (LColumnSchema.Field('properties').Field('queryControls').Field('anyOf').Item(0)
      .Field('type').AsText = 'null') and
    (LSpecSchema.Field('properties').Field('paging').Field('anyOf').Item(0)
      .Field('type').AsText = 'null'), 'version seven preserves the six-field nullable column envelope');
  Check((LSpecSchema.Field('anyOf').Item(0).Field('properties').Field('queryControls')
      .Field('type').AsText = 'object') and
    (LSpecSchema.Field('anyOf').Item(1).Field('properties').Field('columns').Field('contains')
      .Field('properties').Field('queryControls').Field('type').AsText = 'object'),
    'version seven needs an actual table or column declaration rather than null presence');

  LBase := NyxCollectionView(NyxCollection('discovery-rows'))
    .Column(NyxTextField('caption'), 'Caption');
  for LVersion := 5 to 7 do
  begin
    case LVersion of
      5: LSpec := LBase.ColumnSizing(NyxColumnSizing.Fixed(160));
      6: LSpec := LBase.Paged(NyxCollectionPaging.PageSize(12));
      7: LSpec := LBase.ColumnQuery(NyxColumnQuery.Filtering(False).Operators([]));
    else
      begin
        raise Exception.Create('Unexpected current discovery version');
      end;
    end;
    LData := LSpec.ToData;
    LDecoded := TNyxCollectionViewSpec.FromData(LData);
    LBind := NyxObject([NyxField('op', NyxData('bind')),
      NyxField('owner', NyxData('discovery-table')), NyxField('projection', NyxData('table')),
      NyxField('spec', LData)]);
    Check((LData.Field('version').AsInteger = LVersion) and
      (LDecoded.ToData.ToJSON = LData.ToJSON) and
      (ReadNyxCollectionPatch(NyxArray([LBind])).ToData.Item(0).Field('spec').ToJSON = LData.ToJSON),
      'current typed version admits through the real strict bind decoder ' + IntToStr(LVersion));
  end;
  LPolicy := LData.Field('columns').Item(0).Field('queryControls');
  Check((LData.Field('queryControls').Kind = ndNull) and
    (LPolicy.Field('operators').Count = 0) and not LPolicy.Field('filtering').AsBoolean,
    'a column-only disabled empty declaration remains version seven without a table default');
  LColumn := Replacing(LData.Field('columns').Item(0), 'queryControls', NyxNull);
  RefusesView(Replacing(LData, 'columns', NyxArray([LColumn])),
    'an all-null version-seven declaration cannot masquerade as an explicit policy');
  RefusesView(Replacing(LData, 'queryControls', NyxData('default')),
    'table policy strings cannot replace the typed nullable record');
  LBad := Replacing(LPolicy, 'filtering', NyxData(True));
  LColumn := Replacing(LData.Field('columns').Item(0), 'queryControls', LBad);
  RefusesView(Replacing(LData, 'columns', NyxArray([LColumn])),
    'an enabled empty operator set refuses through actual policy admission');
  LBad := Replacing(LPolicy, 'operators', NyxArray([NyxData('equal'), NyxData('equal')]));
  LColumn := Replacing(LData.Field('columns').Item(0), 'queryControls', LBad);
  RefusesView(Replacing(LData, 'columns', NyxArray([LColumn])),
    'duplicate operators refuse even while filtering is disabled');
  LData := LBase.Paged(NyxCollectionPaging.PageSize(12)).ToData;
  RefusesView(Replacing(LData, 'paging', Replacing(LData.Field('paging'), 'pageSize', NyxData(0))),
    'version-six paging does not admit zero size');

  { Version eight appends authored layout without changing any earlier wire
    envelope. Exercise the actual bind decoder with independent policy data;
    discovery alone cannot prove that saved declarations are admitted. }
  Check((LSpecs.Count = 14) and (LAlignmentCount = 2),
    'all former descriptors remain discoverable beside recovery v14 with two alignment read modes');
  LSpecSchema := LSpecs.Item(7);
  LColumnSchema := LSpecSchema.Field('properties').Field('columns').Field('items');
  Check((LSpecSchema.Field('properties').Field('version').Field('const').AsInteger = 8) and
    (LSpecSchema.Field('required').Count = 11) and
    (LColumnSchema.Field('required').Count = 7) and
    Required(LSpecSchema, 'layoutControls') and Required(LColumnSchema, 'layoutControls') and
    (LSpecSchema.Field('properties').Field('columns').Field('maxItems').AsInteger = 64),
    'version eight has exact table and column layout slots within the existing field budget');
  LProperties := LSpecSchema.Field('properties').Field('layoutControls').Field('anyOf');
  Check((LProperties.Item(0).Field('type').AsText = 'null') and
    (LProperties.Item(1).Field('required').Count = 5) and
    not LProperties.Item(1).Field('additionalProperties').AsBoolean and
    (LProperties.Item(1).Field('properties').Field('autoSizeRows').Field('maximum')
      .AsInteger = NyxMaximumColumnAutoSizeRows),
    'table layout discovery preserves absence and bounded complete typed policies');
  LProperties := LColumnSchema.Field('properties').Field('layoutControls').Field('anyOf');
  Check((LProperties.Item(0).Field('type').AsText = 'null') and
    (LProperties.Item(1).Field('required').Count = 4) and
    not LProperties.Item(1).Field('additionalProperties').AsBoolean and
    (LProperties.Item(1).Field('properties').Field('pin').Field('enum').Count = 3),
    'column layout discovery keeps Boolean visibility and three logical pin choices');
  LSpec := LBase.TableLayout(NyxTableLayout.Enabled(False).MinimumCenter(96)
    .AutoSizeSample(32)).ColumnLayout(NyxColumnLayout.Visible(False).PinEnd.AllowOnly([]));
  LData := LSpec.ToData;
  LDecoded := TNyxCollectionViewSpec.FromData(LData);
  LBind := NyxObject([NyxField('op', NyxData('bind')),
    NyxField('owner', NyxData('discovery-table')), NyxField('projection', NyxData('table')),
    NyxField('spec', LData)]);
  Check((LData.Field('version').AsInteger = 8) and
    (LDecoded.ToData.ToJSON = LData.ToJSON) and
    (ReadNyxCollectionPatch(NyxArray([LBind])).ToData.Item(0).Field('spec').ToJSON = LData.ToJSON),
    'disabled table and hidden pinned column declarations round trip through actual binding admission');
  LPolicy := LData.Field('layoutControls');
  RefusesView(Replacing(LData, 'layoutControls', Replacing(LPolicy, 'enabled', NyxData('false'))),
    'layout enablement cannot be supplied as a string');
  RefusesView(Replacing(LData, 'layoutControls', Replacing(LPolicy, 'minimumCenter', NyxData(0))),
    'layout rejects a missing usable middle allocation');
  RefusesView(Replacing(LData, 'layoutControls', Replacing(LPolicy, 'autoSizeRows',
    NyxData(NyxMaximumColumnAutoSizeRows + 1))),
    'authored layout cannot exceed the bounded autosize sample');
  RefusesView(Replacing(LData, 'layoutControls', Replacing(LPolicy, 'features',
    NyxArray([NyxData('resize'), NyxData('resize')]))),
    'layout feature sets reject duplicate entries');
  LColumn := LData.Field('columns').Item(0);
  LBad := Replacing(LColumn.Field('layoutControls'), 'pin', NyxData('left'));
  RefusesView(Replacing(LData, 'columns', NyxArray([Replacing(LColumn, 'layoutControls', LBad)])),
    'physical left is not a logical pinned edge');
  LColumn := Replacing(LColumn, 'layoutControls', NyxNull);
  RefusesView(Replacing(Replacing(LData, 'layoutControls', NyxNull), 'columns', NyxArray([LColumn])),
    'an all-null layout envelope cannot claim a version-eight declaration');
  LData := LBase.ColumnLayout(NyxColumnLayout.AllowOnly([])).ToData;
  Check((LData.Field('version').AsInteger = 8) and (LData.Field('layoutControls').Kind = ndNull) and
    (LData.Field('columns').Item(0).Field('layoutControls').Field('features').Count = 0),
    'an explicit empty column feature set remains distinct from an absent policy');
  LSpecSchema := LSpecs.Item(8);
  LColumnSchema := LSpecSchema.Field('properties').Field('columns').Field('items');
  Check((LSpecSchema.Field('properties').Field('version').Field('const').AsInteger = 9) and
    (LSpecSchema.Field('required').Count = 11) and
    (LColumnSchema.Field('required').Count = 8) and Required(LColumnSchema, 'display') and
    (LColumnSchema.Field('properties').Field('display').Field('anyOf').Item(1)
      .Field('oneOf').Count = 6),
    'v9 discovers six closed display styles while retaining every earlier complete slot');
  LSpec := LBase.ColumnFormat(NyxValueFormat.TextAffixes('[🌙', ']'));
  LData := LSpec.ToData;
  LBind := NyxObject([NyxField('op', NyxData('bind')),
    NyxField('owner', NyxData('discovery-table')), NyxField('projection', NyxData('table')),
    NyxField('spec', LData)]);
  Check((LData.Field('version').AsInteger = 9) and
    (ReadNyxCollectionPatch(NyxArray([LBind])).ToData.Item(0).Field('spec').ToJSON = LData.ToJSON),
    'actual strict bind admission preserves exact display policy and nullable absence');
  LColumn := LData.Field('columns').Item(0);
  RefusesView(Replacing(LData, 'columns', NyxArray([Replacing(LColumn, 'display', NyxNull)])),
    'an all-null display envelope cannot manufacture version-nine presence');
  LBad := Replacing(LColumn.Field('display'), 'style', NyxData('execute-text'));
  RefusesView(Replacing(LData, 'columns', NyxArray([Replacing(LColumn, 'display', LBad)])),
    'display styles are a closed typed choice and never execute saved text');
  LBad := Replacing(LColumn.Field('display'), 'revision', NyxData('1'));
  RefusesView(Replacing(LData, 'columns', NyxArray([Replacing(LColumn, 'display', LBad)])),
    'formatter revision is an exact numeric family at the persistence boundary');

  LSpecSchema := LSpecs.Item(9);
  LPolicy := LSpecSchema.Field('properties').Field('workbench');
  Check((LSpecSchema.Field('properties').Field('version').Field('const').AsInteger = 10) and
    Required(LSpecSchema, 'workbench') and
    (LPolicy.Field('required').Count = 7) and
    not LPolicy.Field('additionalProperties').AsBoolean and
    LPolicy.Field('properties').Field('copyActions').Field('uniqueItems').AsBoolean and
    (LPolicy.Field('properties').Field('copyActions').Field('items').Field('enum').Count = 4) and
    not NyxAgentHas(LSpecSchema, 'allOf'),
    'v10 requires closed action choices without manufacturing a display declaration');
  LData := LBase.TableWorkbench(NyxTableWorkbench.AllowCopy([]).RangeSelection(True)).ToData;
  LBind := Replacing(LBind, 'spec', LData);
  Check((ReadNyxCollectionPatch(NyxArray([LBind])).ToData.Item(0).Field('spec').ToJSON = LData.ToJSON) and
    (LData.Field('columns').Item(0).Field('display').Kind = ndNull),
    'table-only v10 admits an independent explicit empty permission set');
  LPolicy := LData.Field('workbench');
  RefusesView(Replacing(LData, 'workbench', NyxNull),
    'v10 requires an actual workbench declaration');
  RefusesView(Replacing(LData, 'workbench', Replacing(LPolicy, 'rangeSelection', NyxData('true'))),
    'workbench enablement retains its exact Boolean family');
  LBad := LBind;
  try
    ReadNyxCollectionPatch(NyxArray([Replacing(LBad, 'projection', NyxData('list'))]));
    Check(False, 'a table workbench cannot bind to a list');
  except
    on E: ENyxCollection do
    begin
      Check(True, 'actual strict bind refuses a table workbench on a list');
    end;
  end;

  LSpecSchema := LSpecs.Item(11);
  LColumnSchema := LSpecSchema.Field('properties').Field('columns').Field('items');
  Check((LSpecSchema.Field('properties').Field('version').Field('const').AsInteger = 12) and
    (LSpecSchema.Field('required').Count = 13) and (LColumnSchema.Field('required').Count = 10) and
    Required(LColumnSchema, 'alignment') and
    (LSpecSchema.Field('allOf').Item(0).Field('properties').Field('columns').Field('contains')
      .Field('properties').Field('alignment').Field('type').AsText = 'object'),
    'v12 adds a nullable per-column slot but requires actual independent alignment presence');
  LPolicy := LColumnSchema.Field('properties').Field('alignment').Field('anyOf').Item(1);
  Check((LPolicy.Field('required').Count = 3) and (LPolicy.Field('anyOf').Count = 2) and
    (LPolicy.Field('properties').Field('horizontal').Field('enum').Item(0).Kind = ndNull) and
    not LPolicy.Field('additionalProperties').AsBoolean,
    'alignment schema keeps independent nullable axes and refuses an all-null object');
  LData := LBase.ColumnAlignment(NyxColumnAlignment.Horizontal(nchaEnd)).ToData;
  LColumn := LData.Field('columns').Item(0);
  LDecoded := TNyxCollectionViewSpec.FromData(LData);
  Check(LDecoded.HasColumnAlignment and not LDecoded.HasTablePresentationPolicy and
    not LDecoded.ColumnAt(0).AlignmentPolicy.HasVertical,
    'alignment-only descriptors preserve independent absent presentation and vertical axes');
  RefusesView(Replacing(LData, 'columns', NyxArray([Replacing(LColumn, 'alignment', NyxNull)])),
    'v12 cannot manufacture presence from an entirely null alignment slot');

  LSpecSchema := LSpecs.Item(12);
  LColumnSchema := LSpecSchema.Field('properties').Field('columns').Field('items');
  LPolicy := LSpecSchema.Field('properties').Field('inspection');
  Check((LSpecSchema.Field('properties').Field('version').Field('const').AsInteger = 13) and
    (LSpecSchema.Field('required').Count = 14) and (LColumnSchema.Field('required').Count = 10) and
    Required(LSpecSchema, 'inspection') and (LPolicy.Field('required').Count = 4) and
    (LPolicy.Field('properties').Field('maximumPausedBytes').Field('minimum').AsInteger = 0) and
    (LPolicy.Field('properties').Field('removedIdentityLimit').Field('maximum').AsInteger = High(Integer)) and
    not LPolicy.Field('additionalProperties').AsBoolean and not NyxAgentHas(LSpecSchema, 'allOf'),
    'inspection discovery has complete nullable columns, exact bounds and no unrelated alignment requirement');
  LData := LBase.Inspection(NyxInspectionPolicy.RemovedIdentities(0)).ToData;
  LDecoded := TNyxCollectionViewSpec.FromData(LData);
  Check(LDecoded.InspectionPolicy.Defined and not LDecoded.HasPresentationPolicy and
    (LDecoded.InspectionPolicy.IdentityLimit = 0), 'inspection-only v13 retains unrelated absent defaults');
  RefusesView(Replacing(LData, 'inspection', NyxNull), 'v13 cannot manufacture a policy from null presence');

  LSpecSchema := LSpecs.Item(13);
  LPolicy := LSpecSchema.Field('properties').Field('draftRecovery');
  Check((LSpecSchema.Field('required').Count = 15) and
    (LPolicy.Field('required').Count = 5) and Required(LSpecSchema, 'draftRecovery') and
    (LPolicy.Field('properties').Field('maximumEntries').Field('minimum').AsInteger = 0) and
    (LPolicy.Field('properties').Field('maximumPayloadBytes').Field('maximum').AsInteger = High(Integer)) and
    (LSpecSchema.Field('properties').Field('inspection').Field('anyOf').Count = 2) and
    not LPolicy.Field('additionalProperties').AsBoolean,
    'recovery discovery exposes exact finite bounds without requiring inspection presence');
  LData := LBase.DraftRecovery(NyxDraftRecoveryPolicy.Entries(0)).ToData;
  LDecoded := TNyxCollectionViewSpec.FromData(LData);
  Check(LDecoded.DraftRecoveryPolicy.Defined and not LDecoded.InspectionPolicy.Defined,
    'recovery-only v14 retains unrelated absent inspection defaults');
  RefusesView(Replacing(LData, 'draftRecovery', NyxNull), 'v14 cannot manufacture recovery from null presence');

  LSpecSchema := LSpecs.Item(10);
  LColumnSchema := LSpecSchema.Field('properties').Field('columns').Field('items');
  Check((LSpecSchema.Field('properties').Field('version').Field('const').AsInteger = 11) and
    (LSpecSchema.Field('required').Count = 13) and (LColumnSchema.Field('required').Count = 9) and
    Required(LSpecSchema, 'tablePresentation') and Required(LSpecSchema, 'workbench') and
    Required(LColumnSchema, 'textPresentation') and
    (LSpecSchema.Field('anyOf').Count = 2),
    'v11 owns complete nullable policy slots and requires independent presentation presence');
  LPolicy := LColumnSchema.Field('properties').Field('textPresentation').Field('anyOf').Item(1);
  Check((LPolicy.Field('required').Count = 3) and
    (LPolicy.Field('properties').Field('maximumLines').Field('maximum').AsInteger = High(Integer)) and
    not LPolicy.Field('additionalProperties').AsBoolean and
    (LPolicy.Field('allOf').Item(0).Field('then').Field('properties')
      .Field('maximumLines').Field('const').AsInteger = 1),
    'text schema preserves exact integer bounds and single-line policy coupling');
  LData := LBase.TablePresentation(NyxTablePresentation.Density(ntrdCompact)).ToData;
  LBind := Replacing(LBind, 'spec', LData);
  Check(ReadNyxCollectionPatch(NyxArray([LBind])).ToData.Item(0).Field('spec').ToJSON = LData.ToJSON,
    'table-only spacing binds with absent workbench/display/text slots intact');
  RefusesView(Replacing(LData, 'tablePresentation', NyxNull),
    'v11 cannot manufacture presence from entirely null presentation slots');
  LData := LBase.ColumnText(NyxColumnText.Wrap(2000000000)).ToData;
  LColumn := LData.Field('columns').Item(0);
  LDecoded := TNyxCollectionViewSpec.FromData(LData);
  Check(not LDecoded.HasTablePresentationPolicy and
    (LDecoded.ColumnAt(0).TextPolicy.MaximumLines = 2000000000),
    'column-only presentation has no runtime measurement ceiling in its authored wire');
  LBad := Replacing(LColumn.Field('textPresentation'), 'overflow', NyxData('clip'));
  RefusesView(Replacing(LData, 'columns', NyxArray([Replacing(LColumn, 'textPresentation', LBad)])),
    'single-line policies refuse a retained multiline count');
  LBad := Replacing(LColumn.Field('textPresentation'), 'maximumLines', NyxData('4'));
  RefusesView(Replacing(LData, 'columns', NyxArray([Replacing(LColumn, 'textPresentation', LBad)])),
    'presentation counts retain their native scalar family');
  try
    ReadNyxCollectionPatch(NyxArray([Replacing(LBind, 'projection', NyxData('tree'))]));
    Check(False, 'table presentation cannot bind to a tree');
  except
    on E: ENyxCollection do
    begin
      Check(True, 'actual bind refuses table presentation on a tree');
    end;
  end;
end;

{ END QUERY_CONTROL_DISCOVERY_CASES }
{ BEGIN CURRENT_CATALOG_NAMES }
{ Independent expected identities catch missing, substituted or duplicate tools.
  Their order is deliberately irrelevant to collection request discovery. }
function ExactCatalogNames(const ATools: TNyxDataValue): Boolean;
const
  CExpected: array[0..23] of TNyxText = (
    'nyx_session', 'nyx_outline', 'nyx_node', 'nyx_logic', 'nyx_components',
    'nyx_menus', 'nyx_tokens', 'nyx_presentations', 'nyx_state', 'nyx_collections',
    'nyx_resources', 'nyx_diagnostics', 'nyx_source', 'nyx_transaction',
    'nyx_select', 'nyx_history', 'nyx_preview', 'nyx_callbacks', 'nyx_build',
    'nyx_pascal', 'nyx_roots', 'nyx_reviews', 'nyx_workspaces', 'nyx_project');
var
  LExpected, LActual, LMatches: Integer;
begin
  Result := False;

  if ATools.Count <> Length(CExpected) then
  begin
    Exit;
  end;
  for LExpected := Low(CExpected) to High(CExpected) do
  begin
    LMatches := 0;
    for LActual := 0 to ATools.Count - 1 do
    begin

      if ATools.Item(LActual).Field('name').AsText = CExpected[LExpected] then
      begin
        Inc(LMatches);
      end;
    end;

    if LMatches <> 1 then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

{ END CURRENT_CATALOG_NAMES }
{ BEGIN EXPANDED_ACTION_NAMES }
{ The two scalar-edit actions each have four mutually exclusive family branches.
  Check their identities independently rather than treating enum length as the
  number of schema branches. Named operations retain separate closed shapes. }
function ExactChangeBranches(const AChanges: TNyxDataValue): Boolean;
const
  CActions: array[0..50] of TNyxText = (
    'create', 'remove', 'add-field', 'default', 'add-row', 'remove-row', 'cell',
    'bind', 'scope', 'column-title', 'column-mode', 'parent', 'remove-column',
    'add-column', 'clear', 'inherit', 'selection', 'query',
    'column-content', 'column-fixed', 'column-flex', 'column-minimum',
    'column-maximum', 'column-default', 'page-size', 'initial-page', 'default-paging',
    'table-query', 'column-query', 'default-table-query', 'default-column-query',
    'table-layout', 'column-layout', 'default-table-layout', 'default-column-layout',
    'column-format', 'default-column-format', 'table-workbench', 'default-table-workbench',
    'table-presentation', 'default-table-presentation', 'column-text', 'default-column-text',
    'column-alignment', 'default-column-alignment', 'inspection', 'default-inspection',
    'draft-recovery', 'default-draft-recovery', 'column-earlier', 'column-later');
  CNamed: array[0..19] of TNyxText = (
    'define', 'field', 'remove-field', 'append/update-row', 'move-row',
    'bind', 'query', 'typeahead', 'table-query', 'column-query', 'table-layout', 'column-layout',
    'column-format', 'table-workbench', 'table-presentation', 'column-text', 'column-alignment', 'inspection', 'draft-recovery',
    'move-column');
  CRequired: array[0..19] of TNyxText = (
    '|op|key|fields|rows|', '|op|key|definition|', '|op|key|field|kind|',
    '|op|key|item|values|', '|op|key|item|index|', '|op|owner|projection|spec|',
    '|op|owner|key|projection|query|', '|op|owner|key|projection|policy|',
    '|op|owner|key|projection|policy|', '|op|owner|key|projection|field|kind|policy|',
    '|op|owner|key|projection|policy|', '|op|owner|key|projection|field|kind|policy|',
    '|op|owner|key|projection|field|kind|policy|', '|op|owner|key|projection|policy|',
    '|op|owner|key|projection|policy|', '|op|owner|key|projection|field|kind|policy|',
    '|op|owner|key|projection|field|kind|policy|', '|op|owner|key|projection|policy|',
    '|op|owner|key|projection|policy|', '|op|owner|key|projection|field|kind|direction|');
  CFamilies: array[0..3] of TNyxText = ('text', 'boolean', 'integer', 'number');
  CScalars: array[0..3] of TNyxText = ('string', 'boolean', 'integer', 'number');
var
  LCounts: array[0..50] of Integer;
  LFamilies: array[0..50, 0..3] of Boolean;
  LNamed: array[0..19] of Boolean;
  LBranch, LProperties, LOperation, LChoices: TNyxDataValue;
  LIndex, LAt, LFamily, LAction, LShape: Integer;
  LName, LKind, LExpected: TNyxText;

  function ExactRequired(const ABranch: TNyxDataValue;
    const AExpected: TNyxText): Boolean;
  var
    LRequired: TNyxDataValue;
    LAt: Integer;
    LKey, LSeen: TNyxText;
  begin
    Result := False;
    LRequired := ABranch.Field('required');
    LSeen := '|';
    for LAt := 0 to LRequired.Count - 1 do
    begin
      LKey := LRequired.Item(LAt).AsText;

      if (Pos('|' + LKey + '|', AExpected) = 0) or
        (Pos('|' + LKey + '|', LSeen) > 0) or
        not NyxAgentHas(ABranch.Field('properties'), LKey) then
      begin
        Exit;
      end;
      LSeen := LSeen + LKey + '|';
    end;
    Result := (Length(LSeen) = Length(AExpected)) and
      (LRequired.Count = ABranch.Field('properties').Count);
  end;

begin
  Result := False;
  for LAt := Low(LCounts) to High(LCounts) do
  begin
    LCounts[LAt] := 0;
    for LFamily := Low(CFamilies) to High(CFamilies) do
    begin
      LFamilies[LAt, LFamily] := False;
    end;
  end;
  for LAt := Low(LNamed) to High(LNamed) do
  begin
    LNamed[LAt] := False;
  end;
  for LIndex := 0 to AChanges.Count - 1 do
  begin
    LBranch := AChanges.Item(LIndex);
    LProperties := LBranch.Field('properties');
    LOperation := LProperties.Field('op');
    LName := '';

    if NyxAgentHas(LOperation, 'const') then
    begin
      LName := LOperation.Field('const').AsText;
    end
    else if NyxAgentHas(LOperation, 'enum') then
    begin
      LChoices := LOperation.Field('enum');

      if (LChoices.Count <> 2) or not
        (((LChoices.Item(0).AsText = 'append') and (LChoices.Item(1).AsText = 'update-row')) or
         ((LChoices.Item(1).AsText = 'append') and (LChoices.Item(0).AsText = 'update-row'))) then
      begin
        Exit;
      end;
      LName := 'append/update-row';
    end;

    if LName = 'intent' then
    begin
      LName := LProperties.Field('action').Field('const').AsText;
      LAction := -1;
      for LAt := Low(CActions) to High(CActions) do
      begin

        if LName = CActions[LAt] then
        begin
          LAction := LAt;
        end;
      end;

      if LAction < 0 then
      begin
        Exit;
      end;
      Inc(LCounts[LAction]);

      if (LName = 'default') or (LName = 'cell') then
      begin
        LExpected := '|op|action|key|field|kind|value|';

        if LName = 'cell' then
        begin
          LExpected := '|op|action|key|item|field|kind|value|';
        end;

        if not ExactRequired(LBranch, LExpected) then
        begin
          Exit;
        end;
        LKind := LProperties.Field('kind').Field('const').AsText;
        LFamily := -1;
        for LAt := Low(CFamilies) to High(CFamilies) do
        begin

          if LKind = CFamilies[LAt] then
          begin
            LFamily := LAt;
          end;
        end;

        if LFamily < 0 then
        begin
          Exit;
        end;

        if LFamilies[LAction, LFamily] or
          (LProperties.Field('value').Field('type').AsText <> CScalars[LFamily]) then
        begin
          Exit;
        end;
        LFamilies[LAction, LFamily] := True;
      end;
    end
    else
    begin
      LShape := -1;
      for LAt := Low(CNamed) to High(CNamed) do
      begin

        if LName = CNamed[LAt] then
        begin
          LShape := LAt;
        end;
      end;

      if LShape < 0 then
      begin
        Exit;
      end;

      if LNamed[LShape] or not ExactRequired(LBranch, CRequired[LShape]) then
      begin
        Exit;
      end;
      LNamed[LShape] := True;

      if (LName = 'move-column') and
        ((LProperties.Field('projection').Field('const').AsText <> 'table') or
        (LProperties.Field('direction').Field('enum').ToJSON <>
          TNyxDataValue.ParseJSON('["earlier","later"]').ToJSON) or
        (LProperties.Field('kind').Field('enum').ToJSON <>
          TNyxDataValue.ParseJSON('["text","boolean","integer","number"]').ToJSON)) then
      begin
        Exit;
      end;
    end;
  end;
  for LAt := Low(CActions) to High(CActions) do
  begin

    if (CActions[LAt] = 'default') or (CActions[LAt] = 'cell') then
    begin

      if LCounts[LAt] <> Length(CFamilies) then
      begin
        Exit;
      end;
    end
    else if LCounts[LAt] <> 1 then
    begin
      Exit;
    end;
  end;
  for LAt := Low(LNamed) to High(LNamed) do
  begin

    if not LNamed[LAt] then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

{ END EXPANDED_ACTION_NAMES }
{ BEGIN QUERY_POLICY_SCHEMA_CASES }
{ New named commands retain ordinary intent and descriptor versions. Discovery
  advertises only typed policy/null and current identities, not a UI baseline. }
procedure PolicyEditDiscovery(const ATool: TNyxDataValue);
var
  LChanges, LTable, LColumn, LWire, LBad, LProperties: TNyxDataValue;
  LPatch: INyxCollectionPatch;
  LKey: TNyxCollectionRef;
  LField: TNyxStudioCollectionFieldRef;
  LTablePolicy: TNyxTableQueryPolicy;
  LColumnPolicy: TNyxColumnQueryPolicy;
  LRejected: Boolean;

  function Replacing(const AData: TNyxDataValue; const AName: TNyxText;
    const AValue: TNyxDataValue): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
    LAt: Integer;
  begin
    SetLength(LFields, AData.Count);
    for LAt := 0 to AData.Count - 1 do
    begin

      if AData.Key(LAt) = AName then
      begin
        LFields[LAt] := NyxField(AName, AValue);
      end
      else
      begin
        LFields[LAt] := NyxField(AData.Key(LAt), AData.Field(AData.Key(LAt)));
      end;
    end;
    Result := NyxObject(LFields);
  end;

  procedure Refuses(const AData: TNyxDataValue; const AReason: TNyxText);
  var
    LRejected: Boolean;
  begin
    LRejected := False;
    try
      ReadNyxCollectionPatch(AData);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, AReason);
  end;

begin
  LChanges := ATool.Field('inputSchema').Field('oneOf');
  LChanges := LChanges.Item(LChanges.Count - 1).Field('properties').Field('changes')
    .Field('items').Field('oneOf');
  LTable := LChanges.Item(8);
  LColumn := LChanges.Item(9);
  Check((LTable.Field('properties').Field('op').Field('const').AsText = 'table-query') and
    (LColumn.Field('properties').Field('op').Field('const').AsText = 'column-query') and
    (LChanges.Item(5).Field('properties').Field('spec').Field('oneOf').Count = 14) and
    (LChanges.Item(7).Field('properties').Field('op').Field('const').AsText = 'typeahead'),
    'query policy commands retain their positions beside the appended layout binding version');
  Check((LTable.Field('required').Count = 5) and (LColumn.Field('required').Count = 7) and
    (LTable.Field('properties').Field('projection').Field('const').AsText = 'table') and
    (LColumn.Field('properties').Field('projection').Field('const').AsText = 'table') and
    not NyxAgentHas(LTable.Field('properties'), 'baseline') and
    not NyxAgentHas(LColumn.Field('properties'), 'baseline'),
    'table-only named commands require no caller form baseline');
  LProperties := LTable.Field('properties').Field('policy');
  Check((LProperties.Field('oneOf').Count = 2) and
    (LProperties.Field('oneOf').Item(0).Field('type').AsText = 'null') and
    (LProperties.Field('oneOf').Item(1).Field('required').Count = 5) and
    not LProperties.Field('oneOf').Item(1).Field('additionalProperties').AsBoolean,
    'named table policy discovery preserves strict object versus null reset');
  LProperties := LColumn.Field('properties').Field('policy').Field('oneOf');
  Check((LProperties.Item(0).Field('type').AsText = 'null') and
    (LProperties.Item(1).Field('required').Count = 5) and
    (LProperties.Item(1).Field('properties').Field('operators').Field('anyOf').Item(0)
      .Field('type').AsText = 'null') and
    (LColumn.Field('properties').Field('kind').Field('enum').Count = 4),
    'named column policy discovery keeps typed families and nullable default operators');

  LKey := NyxCollection('policy-discovery-rows');
  LField := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('priority'));
  LTablePolicy := NyxTableQuery.Sorting(False);
  LColumnPolicy := NyxColumnQuery.Filtering(False).Operators([nqcEqual, nqcAtLeast]);
  LPatch := NyxCollectionPatch([
    NyxSetCollectionTableQuery(NyxBindingOwner('policy-table'), LKey, cpTable, LTablePolicy),
    NyxUseDefaultCollectionTableQuery(NyxBindingOwner('policy-table'), LKey, cpTable),
    NyxSetCollectionColumnQuery(NyxBindingOwner('policy-table'), LKey, cpTable, LField, LColumnPolicy),
    NyxUseDefaultCollectionColumnQuery(NyxBindingOwner('policy-table'), LKey, cpTable, LField)]);
  LTablePolicy := LTablePolicy.Sorting(True);
  LColumnPolicy := LColumnPolicy.Operators([]);
  LWire := LPatch.ToData;
  Check((LWire.Item(0).Field('policy').Kind = ndObject) and
    not LWire.Item(0).Field('policy').Field('sorting').AsBoolean and
    (LWire.Item(1).Field('policy').Kind = ndNull) and
    (LWire.Item(2).Field('policy').Field('operators').Count = 2) and
    (LWire.Item(3).Field('policy').Kind = ndNull) and
    (ReadNyxCollectionPatch(LWire).ToData.ToJSON = LWire.ToJSON),
    'all four factories own scalar copies and round trip independently of later caller edits');
  Refuses(NyxArray([Replacing(LWire.Item(0), 'projection', NyxData('list'))]),
    'table policy wire cannot target a non-table projection');
  Refuses(NyxArray([Replacing(LWire.Item(2), 'kind', NyxData('boolean'))]),
    'column policy wire rejects an operator from the wrong scalar family');
  Refuses(NyxArray([Replacing(LWire.Item(2), 'policy', NyxData(False))]),
    'a Boolean is neither a column policy record nor a null reset');
  LBad := NyxObject([NyxField('op', NyxData('column-query')),
    NyxField('owner', NyxData('policy-table')), NyxField('key', NyxData(LKey.Name)),
    NyxField('projection', NyxData('table')), NyxField('kind', NyxData('integer')),
    NyxField('policy', NyxNull)]);
  Refuses(NyxArray([LBad]), 'a column reset still requires its exact field');
  LRejected := False;
  try
    NyxSetCollectionTableQuery(NyxBindingOwner('policy-table'), LKey, cpTable,
      Default(TNyxTableQueryPolicy));
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'an undefined typed table setter does not silently mean reset');
  LRejected := False;
  try
    NyxSetCollectionColumnQuery(NyxBindingOwner('policy-table'), LKey, cpTable,
      LField, Default(TNyxColumnQueryPolicy));
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'an undefined typed column setter does not silently mean reset');
end;

{ END QUERY_POLICY_SCHEMA_CASES }
begin
  try
    { This is the actual MCP discovery builder. No server constructor, listener,
      personal configuration or authenticated active project is touched. }
    LTools := NyxStudioMCPTools.Field('tools');
    Check(LTools.Count = 24, 'complete source catalog count');
    { BEGIN CURRENT_CATALOG_CHECK }
    Check(ExactCatalogNames(LTools), 'complete exact tool names are unique');
    { END CURRENT_CATALOG_CHECK }
    LFound := False;
    for LIndex := 0 to LTools.Count - 1 do
    begin
      LTool := LTools.Item(LIndex);

      if LTool.Field('name').AsText = 'nyx_collections' then
      begin
        LFound := True;
        Break;
      end;
    end;
    Check(LFound, 'new focused tool is actually discoverable');
    Check(not LTool.Field('annotations').Field('readOnlyHint').AsBoolean,
      'mixed query/edit tool advertises conservative mutation hint');
    LSchema := LTool.Field('inputSchema').Field('oneOf');
    Check(LSchema.Count = 13, 'all thirteen closed context/apply branches are discoverable');
    for LMode := 0 to LSchema.Count - 1 do
    begin
      LVariant := LSchema.Item(LMode);
      Check(not LVariant.Field('additionalProperties').AsBoolean and
        NyxAgentHas(LVariant.Field('properties'), 'workspace') and
        NyxAgentHas(LVariant.Field('properties'), 'review'),
        'strict outer project/review routing ' + IntToStr(LMode));

      if LMode = 3 then
      begin
        Check(LVariant.Field('not').Field('anyOf').Count = 2,
          'row/choice and project/review exclusivity both remain composed');
      end
      else
      begin
        Check(LVariant.Field('not').Field('required').Count = 2,
          'outer project/review exclusivity ' + IntToStr(LMode));
      end;
    end;
    LProperties := LSchema.Item(2).Field('properties');
    Check((LProperties.Field('limit').Field('maximum').AsInteger = 20) and
      (LProperties.Field('fields').Field('maxItems').AsInteger = 16) and
      LProperties.Field('fields').Field('uniqueItems').AsBoolean, 'row query budgets');
    Check(LSchema.Item(3).Field('properties').Field('count').Field('maximum').AsInteger = 4096,
      'Unicode scalar window limit');
    Check((LSchema.Item(6).Field('properties').Field('source').Field('enum').Count = 3) and
      (LSchema.Item(6).Field('properties').Field('source').Field('enum').Item(2).AsText = 'restorable'),
      'masked inherited titles expose the same exact-window query contract');
    LProperties := LSchema.Item(LSchema.Count - 1).Field('properties');
    Check((LProperties.Field('changes').Field('maxItems').AsInteger = 32) and
      (LProperties.Field('operationId').Field('maxLength').AsInteger = 120),
      'group and retry identity limits');
    LChanges := LProperties.Field('changes').Field('items').Field('oneOf');
    Check(LChanges.Count = 77, 'twenty named commands and all fifty-one typed ordinary actions');
    { BEGIN EXPANDED_ACTION_CHECK }
    Check(ExactChangeBranches(LChanges), 'twenty named shapes and every typed action/family occur exactly once');
    { END EXPANDED_ACTION_CHECK }
    LDefinition := LChanges.Item(1).Field('properties').Field('definition').Field('oneOf');
    Check((LDefinition.Count = 4) and
      (LDefinition.Item(0).Field('properties').Field('default').Field('type').AsText = 'string') and
      (LDefinition.Item(2).Field('properties').Field('default').Field('minimum').AsInteger = Low(Integer)),
      'typed named defaults and signed integer constraints');
    Check(LDefinition.Item(0).Field('properties').Field('domain').Field('oneOf').Item(1)
      .Field('properties').Field('choices').Field('maxItems').AsInteger = 128,
      'bounded typed domain choices');
    LDefinition := LChanges.Item(5).Field('properties').Field('spec').Field('oneOf');
    Check((LDefinition.Count = 14) and
      (LDefinition.Item(0).Field('properties').Field('version').Field('const').AsInteger = 1) and
      (LDefinition.Item(1).Field('required').Count = 6) and
      (LDefinition.Item(1).Field('properties').Field('selection').Field('const').AsText = 'multiple'),
      'exact versioned single/multiple fluent view descriptor');
    Check((LDefinition.Item(2).Field('properties').Field('version').Field('const').AsInteger = 3) and
      (LDefinition.Item(3).Field('properties').Field('version').Field('const').AsInteger = 4) and
      (LDefinition.Item(3).Field('required').Count = 8) and
      (LDefinition.Item(3).Field('properties').Field('query').Field('anyOf').Item(0)
        .Field('type').AsText = 'null'), 'query and saved search binding versions are discoverable');
    LProperties := LDefinition.Item(3).Field('properties').Field('typeAhead');
    Check((LProperties.Field('required').Count = 4) and
      (LProperties.Field('properties').Field('enabled').Field('type').AsText = 'boolean') and
      (LProperties.Field('properties').Field('windowMS').Field('type').AsText = 'integer') and
      (LProperties.Field('properties').Field('windowMS').Field('maximum').AsInteger = 60000) and
      not LProperties.Field('additionalProperties').AsBoolean,
      'saved policy has the closed native scalar descriptor');
    LProperties := LChanges.Item(7).Field('properties');
    Check((LProperties.Field('op').Field('const').AsText = 'typeahead') and
      (LProperties.Field('projection').Field('enum').Count = 2) and
      (LProperties.Field('policy').Field('oneOf').Item(0).Field('type').AsText = 'null'),
      'policy-only set/default reset is discoverable through the existing tool');
    for LIndex := 0 to LChanges.Count - 1 do
    begin
      LChange := LChanges.Item(LIndex);
      Check(not LChange.Field('additionalProperties').AsBoolean and
        not NyxAgentHas(LChange.Field('properties'), 'workspace') and
        not NyxAgentHas(LChange.Field('properties'), 'review'),
        'nested operation keeps strict ownership ' + IntToStr(LIndex));
    end;
    { BEGIN QUERY_CONTROL_DISCOVERY_CALL }
    CurrentPolicyDiscovery(LTool);
    { END QUERY_CONTROL_DISCOVERY_CALL }
    { BEGIN QUERY_POLICY_SCHEMA_CALL }
    PolicyEditDiscovery(LTool);
    { END QUERY_POLICY_SCHEMA_CALL }
    WriteLn('PASS ', LChecks, ' actual collection MCP discovery checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
