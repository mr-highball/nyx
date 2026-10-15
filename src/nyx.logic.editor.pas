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
unit nyx.logic.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls,
  nyx.flow.types, nyx.flow, nyx.flow.declarations, nyx.flow.editing,
  nyx.logic, nyx.logic.catalog, nyx.http.editor, nyx.flow.rows.editor,
  nyx.files.save.editor, nyx.files.open.editor, nyx.flow.conditions.editor;

type
  { Open is presentation only. Change contains an exact guarded proposal; the
    owner publishes it through its ordinary revision-aware paired command. }
  TNyxLogicEditorCapture = record
    Open: Boolean;
    Reference: TNyxFlowRef;
    Change: TNyxFlowChange;
  end;
  { Independent unfinished controls, including malformed creator JSON/numbers,
    typed JSON/relational policies, independent join paths and Boolean choices.
    Restoration requires the exact graph baseline. Unused child forms retain
    legacy draft bytes; no tree is retained. }
  TNyxLogicEditorDraft = record
  private
    FContext: TNyxText;
    FValues: TNyxDataValue;
    FPath: TNyxDataValue;
    FRightPath: TNyxDataValue;
    FBranches: TNyxDataValue;
    FDescriptor: TNyxDataValue;
    FHTTP: TNyxHTTPRequestEditorDraft;
    FRows: TNyxFlowRowsEditorDraft;
    FSave: TNyxFileSaveEditorDraft;
    FOpen: TNyxFileOpenEditorDraft;
    FConditions: TNyxFlowConditionEditorDraft;
    function GetDefined: Boolean;
  public
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode);
    function Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    procedure Clear;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxLogicEditorDraft; static;
    property Defined: Boolean read GetDefined;
  end;

{ Public compound Nyx form: discovery/help, typed built-in configuration, saved
  steps/ports and keyboard-operable wire choices. No document/operation is retained.
  An optional immutable creator catalog supplies discovery/default configuration;
  creator JSON is explicitly a configuration-data extension boundary. }
function NewNyxLogicEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxFlowRef; const ACatalog: INyxLogicCatalog = nil): INyxCard;
{ Handles local search/grouping, configuration loading and typed path construction.
  True requests ordinary renderer Sync, never document history or a remount. }
function HandleNyxLogicEditor(ANode, ARoot: TNyxNode): Boolean;
{ Capture belongs to the exact mounted tree. Removal needs explicit warning
  acknowledgement. Invalid configuration raises and leaves every input intact. }
function CaptureNyxLogicEditor(ANode, ARoot: TNyxNode;
  out ACapture: TNyxLogicEditorCapture): Boolean;

implementation

uses nyx.logic.collections, nyx.logic.json.editing, nyx.logic.http, nyx.http.types, nyx.resource.sources,
  nyx.logic.relations, nyx.numbers.decimal, nyx.logic.joins,
  nyx.logic.files, nyx.files.save, nyx.logic.files.open, nyx.files.open;

const
  CEditor = 'nyx.logic-editor';
  CPath = 'nyx.logic-path';
  CRightPath = 'nyx.logic-right-path';
  CBranches = 'nyx.logic-join-inputs';
  CDescriptor = 'nyx.logic-active-descriptor';
  CFields: array[0..37] of TNyxText =
    ('graph', 'graph-name', 'step-name', 'operation', 'step', 'source', 'target',
     'layout', 'indent', 'separator', 'path-part', 'path-kind', 'creator-config',
     'search', 'group', 'grouped', 'confirm', 'comparison', 'expected-kind',
     'expected', 'key-kind', 'direction', 'offset', 'count',
     'set-mode', 'missing-policy', 'merge-policy', 'path-side', 'join-kind',
     'aggregation', 'rounding', 'empty-aggregate', 'null-keys', 'branch-name',
     'branch-choice', 'branch-type', 'branch-selection', 'branch-empty');
  CComparisons: array[TNyxJSONComparison] of TNyxText =
    ('Equal', 'Not equal', 'Less than', 'At most', 'Greater than', 'At least',
     'Contains', 'Starts with', 'Ends with');
  CKeyKinds: array[TNyxJSONKeyKind] of TNyxText = ('Text', 'Boolean', 'Exact decimal');
  CSetModes: array[TNyxJSONSetMode] of TNyxText = ('Upsert', 'Replace existing', 'Insert');
  CMissing: array[TNyxJSONMissing] of TNyxText = ('Require existing', 'Ignore missing');
  CMergePolicies: array[TNyxJSONMergePolicy] of TNyxText = ('Overlay (preserve null)', 'Merge Patch (null deletes)');
  CJoinKinds: array[TNyxJSONJoinKind] of TNyxText = ('Inner', 'Left', 'Right', 'Full');
  CNullKeys: array[TNyxJSONNullKeys] of TNyxText = ('Reject null', 'Match null');
  CAggregations: array[TNyxJSONAggregation] of TNyxText = ('Sum', 'Average', 'Minimum', 'Maximum', 'Count rows');
  CRoundings: array[TNyxDecimalRounding] of TNyxText =
    ('Exact (refuse loss)', 'Nearest, ties to even', 'Nearest, ties away from zero',
     'Toward zero', 'Floor', 'Ceiling');
  CEmptyAggregates: array[TNyxJSONEmptyAggregate] of TNyxText = ('Return null', 'Report error');
  CBranchSelections: array[TNyxFlowJoinSelection] of TNyxText =
    ('Exactly one', 'First available', 'Last available');
  CBranchEmpty: array[TNyxFlowJoinEmpty] of TNyxText = ('Skip outputs', 'Report error');

function RelationOperation(const AKind: TNyxText): Boolean;
begin
  Result := (AKind = 'json-group') or (AKind = 'json-distinct') or (AKind = 'json-join');
end;

function PathOperation(const AKind: TNyxText): Boolean;
begin
  Result := (AKind = 'json-select') or (AKind = 'json-filter') or
    (AKind = 'json-sort') or (AKind = 'json-map') or
    (AKind = 'json-set') or (AKind = 'json-remove') or
    RelationOperation(AKind) or (AKind = 'json-aggregate');
end;

function FormFor(ANode, ARoot: TNyxNode): TNyxNode;
begin
  Result := nil;

  if (ANode = nil) or (ARoot = nil) or (ARoot.Find(ANode.ID) <> ANode) then
  begin
    Exit;
  end;
  Result := ANode;
  while (Result <> nil) and not Result.Extensions.Has(NyxExtension(CEditor)) do
  begin
    Result := Result.Parent;
  end;
end;

function Field(AForm: TNyxNode; const AName: TNyxText): TNyxNode;
begin
  Result := AForm.Find(AForm.ID + '-' + AName);

  if Result = nil then
  begin
    raise ENyxFlow.Create('Logic editor field is unavailable');
  end;
end;

function Value(AForm: TNyxNode; const AName: TNyxText): TNyxText;
begin
  Result := Field(AForm, AName).Prop('value');
end;

procedure SetValue(AForm: TNyxNode; const AName, AValue: TNyxText);
begin
  Field(AForm, AName).Configure.Value(AValue).Done;
end;

{ Left/right selectors share the ordinary typed path editor but retain separate
  independent paths. Unknown unfinished side text displays left without losing
  either value; the select's canonical choices are used by normal interaction. }
function PathTarget(AForm: TNyxNode): TNyxText;
begin
  Result := CPath;

  if (Value(AForm, 'operation') = 'json-join') and
    (Value(AForm, 'path-side') = 'Right key') then
  begin
    Result := CRightPath;
  end;
end;

{ Names are data references, not a newline-delimited behavior language. Only the
  ordinary select display joins names with LF; the owned ordered array remains
  the authoritative configuration. Updating choices never rewrites draft text. }
procedure ShowBranches(AForm: TNyxNode);
var
  LInputs: TNyxDataValue;
  LItems: TNyxText;
  LIndex: Integer;
begin
  LInputs := AForm.Extensions.Value(NyxExtension(CBranches));
  LItems := '';
  for LIndex := 0 to LInputs.Count - 1 do
  begin

    if LIndex > 0 then
    begin
      LItems := LItems + #10;
    end;
    LItems := LItems + LInputs.Item(LIndex).AsText;
  end;
  Field(AForm, 'branch-choice').Configure.Items(LItems).Done;
  Field(AForm, 'branch-preview').Configure.Text(LInputs.ToJSON).Done;
end;

{ Edit only the form's ordered typed references. Existing graph wires are never
  silently detached; final grouped admission still owns compatibility/history. }
function EditBranches(AForm: TNyxNode; const AAction: TNyxText): Boolean;
var
  LInputs: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LCount: Integer;
  LSelected: Integer;
  LTarget: Integer;
  LTemp: TNyxDataValue;
  LOptions: TNyxFlowJoinOptions;
begin
  Result := (AAction = 'branch-add') or (AAction = 'branch-remove') or
    (AAction = 'branch-up') or (AAction = 'branch-down');

  if not Result then
  begin
    Exit;
  end;
  LInputs := AForm.Extensions.Value(NyxExtension(CBranches));
  LCount := LInputs.Count;
  SetLength(LItems, LCount);
  LSelected := -1;
  for LIndex := 0 to LCount - 1 do
  begin
    LItems[LIndex] := LInputs.Item(LIndex);

    if LItems[LIndex].AsText = Value(AForm, 'branch-choice') then
    begin
      LSelected := LIndex;
    end;
  end;

  if AAction = 'branch-add' then
  begin
    SetLength(LItems, LCount + 1);
    LItems[LCount] := NyxData(Value(AForm, 'branch-name'));
  end
  else
  begin

    if LSelected < 0 then
    begin
      raise ENyxFlow.Create('Choose an exact alternative first');
    end;

    if AAction = 'branch-remove' then
    begin

      if Value(AForm, 'confirm') <> 'Confirm removal' then
      begin
        raise ENyxFlow.Create('Confirm removal before changing branch inputs; existing graph wires still require review');
      end;
      for LIndex := LSelected to LCount - 2 do
      begin
        LItems[LIndex] := LItems[LIndex + 1].Copy;
      end;
      SetLength(LItems, LCount - 1);
    end
    else
    begin
      LTarget := LSelected - 1;

      if AAction = 'branch-down' then
      begin
        LTarget := LSelected + 1;
      end;

      if (LTarget < 0) or (LTarget >= LCount) then
      begin
        Exit;
      end;
      LTemp := LItems[LSelected].Copy;
      LItems[LSelected] := LItems[LTarget].Copy;
      LItems[LTarget] := LTemp;
    end;
  end;
  { Validate even a partial empty recipe's remaining references before changing
    the local form. Zero alternatives remain an unfinished draft, not a graph. }
  LOptions := NyxFlowJoinOptions(nftText);
  for LIndex := 0 to High(LItems) do
  begin
    LOptions := LOptions.Input(NyxFlowPort(LItems[LIndex].AsText));
  end;
  AForm.Extensions.SetValue(NyxExtension(CBranches), NyxArray(LItems));
  ShowBranches(AForm);

  if AAction = 'branch-add' then
  begin
    SetValue(AForm, 'branch-choice', Value(AForm, 'branch-name'));
  end
  else if AAction = 'branch-remove' then
  begin
    SetValue(AForm, 'branch-choice', '');

    if Length(LItems) > 0 then
    begin
      SetValue(AForm, 'branch-choice', LItems[0].AsText);
    end;
  end;
end;

{ Built-in configuration presents only its typed controls. Creator JSON is an
  explicit extension-data editor, never the default way to configure a builtin. }
procedure ShowConfiguration(AForm: TNyxNode; const AKind: TNyxText);
var
  LPath: Boolean;
begin
  LPath := PathOperation(AKind) and not ((AKind = 'json-aggregate') and
    (Value(AForm, 'aggregation') = CAggregations[njaCount]));
  Field(AForm, 'layout').Configure.Visible(AKind = 'json-format').Done;
  Field(AForm, 'indent').Configure.Visible(AKind = 'json-format').Done;
  Field(AForm, 'separator').Configure.Visible(AKind = 'text-join').Done;
  Field(AForm, 'path-kind').Configure.Visible(LPath).Done;
  Field(AForm, 'path-part').Configure.Visible(LPath).Done;
  Field(AForm, 'path-add').Configure.Visible(LPath).Done;
  Field(AForm, 'path-clear').Configure.Visible(LPath).Done;
  Field(AForm, 'path-preview').Configure.Visible(LPath).Done;
  Field(AForm, 'path-side').Configure.Visible(AKind = 'json-join').Done;
  Field(AForm, 'comparison').Configure.Visible(AKind = 'json-filter').Done;
  Field(AForm, 'expected-kind').Configure.Visible(AKind = 'json-filter').Done;
  Field(AForm, 'expected').Configure.Visible(AKind = 'json-filter').Done;
  Field(AForm, 'key-kind').Configure.Visible((AKind = 'json-sort') or RelationOperation(AKind)).Done;
  Field(AForm, 'direction').Configure.Visible(AKind = 'json-sort').Done;
  Field(AForm, 'offset').Configure.Visible(AKind = 'json-slice').Done;
  Field(AForm, 'count').Configure.Visible(AKind = 'json-slice').Done;
  Field(AForm, 'set-mode').Configure.Visible(AKind = 'json-set').Done;
  Field(AForm, 'missing-policy').Configure.Visible(AKind = 'json-remove').Done;
  Field(AForm, 'merge-policy').Configure.Visible(AKind = 'json-merge').Done;
  Field(AForm, 'join-kind').Configure.Visible(AKind = 'json-join').Done;
  Field(AForm, 'null-keys').Configure.Visible(RelationOperation(AKind)).Done;
  Field(AForm, 'aggregation').Configure.Visible(AKind = 'json-aggregate').Done;
  Field(AForm, 'rounding').Configure.Visible(AKind = 'json-aggregate').Done;
  Field(AForm, 'empty-aggregate').Configure.Visible(AKind = 'json-aggregate').Done;
  Field(AForm, 'branch-name').Configure.Visible(AKind = 'flow-join').Done;
  Field(AForm, 'branch-choice').Configure.Visible(AKind = 'flow-join').Done;
  Field(AForm, 'branch-type').Configure.Visible(AKind = 'flow-join').Done;
  Field(AForm, 'branch-selection').Configure.Visible(AKind = 'flow-join').Done;
  Field(AForm, 'branch-empty').Configure.Visible(AKind = 'flow-join').Done;
  Field(AForm, 'branch-add').Configure.Visible(AKind = 'flow-join').Done;
  Field(AForm, 'branch-remove').Configure.Visible(AKind = 'flow-join').Done;
  Field(AForm, 'branch-up').Configure.Visible(AKind = 'flow-join').Done;
  Field(AForm, 'branch-down').Configure.Visible(AKind = 'flow-join').Done;
  Field(AForm, 'branch-preview').Configure.Visible(AKind = 'flow-join').Done;
  Field(AForm, 'http').Configure.Visible(AKind = 'http-call').Done;
  Field(AForm, 'save').Configure.Visible(AKind = 'file-save').Done;
  Field(AForm, 'file-open').Configure.Visible(AKind = 'file-open').Done;
  Field(AForm, 'creator-config').Configure.Visible((AKind <> 'json-parse') and
    not PathOperation(AKind) and (AKind <> 'json-slice') and (AKind <> 'json-merge') and
    (AKind <> 'http-call') and (AKind <> 'utf8-decode') and
    (AKind <> 'file-save') and (AKind <> 'file-open') and (AKind <> 'utf8-encode') and (AKind <> 'flow-join') and
    (AKind <> 'json-format') and (AKind <> 'text-join')).Done;
end;

function OperationData(const ADescriptor: TNyxLogicDescriptor;
  const AConfiguration: TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('descriptor', ADescriptor.ToData),
    NyxField('configuration', AConfiguration)]);
end;

function Entry(AForm: TNyxNode; const AKind: TNyxText): TNyxDataValue;
var
  LCatalog: TNyxDataValue;
  LIndex: Integer;
begin
  LCatalog := AForm.Extensions.Value(NyxExtension(CEditor)).Field('catalog');
  for LIndex := 0 to LCatalog.Count - 1 do
  begin
    Result := LCatalog.Item(LIndex);

    if Result.Field('descriptor').Field('kind').AsText = AKind then
    begin
      Exit;
    end;
  end;
  raise ENyxFlow.Create('Choose an operation from this Logic collection');
end;

procedure LoadConfiguration(AForm: TNyxNode; const AOperation: TNyxDataValue);
var
  LKind: TNyxText;
  LConfiguration: TNyxDataValue;
begin
  LKind := AOperation.Field('descriptor').Field('kind').AsText;
  LConfiguration := AOperation.Field('configuration');
  AForm.Extensions.SetValue(NyxExtension(CDescriptor), AOperation.Field('descriptor'));
  SetValue(AForm, 'operation', LKind);

  if LKind = 'json-format' then
  begin
    SetValue(AForm, 'layout', 'Indented');

    if LConfiguration.Field('layout').AsInteger = Ord(njlCompact) then
    begin
      SetValue(AForm, 'layout', 'Compact');
    end;
    SetValue(AForm, 'indent', LConfiguration.Field('indent').ToJSON);
  end
  else if LKind = 'text-join' then
  begin
    SetValue(AForm, 'separator', LConfiguration.Field('separator').AsText);
  end
  else if LKind = 'json-join' then
  begin
    AForm.Extensions.SetValue(NyxExtension(CPath), LConfiguration.Field('left'));
    AForm.Extensions.SetValue(NyxExtension(CRightPath), LConfiguration.Field('right'));
    SetValue(AForm, 'path-side', 'Left key');
    Field(AForm, 'path-preview').Configure.Text(LConfiguration.Field('left').ToJSON).Done;
  end
  else if PathOperation(LKind) then
  begin
    AForm.Extensions.SetValue(NyxExtension(CPath), LConfiguration.Field('path'));
    Field(AForm, 'path-preview').Configure.Text(LConfiguration.Field('path').ToJSON).Done;
  end;

  if LKind = 'json-filter' then
  begin
    SetValue(AForm, 'comparison', CComparisons[TNyxJSONComparison(LConfiguration.Field('comparison').AsInteger)]);
    case LConfiguration.Field('expected').Kind of
      ndNull: SetValue(AForm, 'expected-kind', 'Null');
      ndText: SetValue(AForm, 'expected-kind', 'Text');
      ndBoolean: SetValue(AForm, 'expected-kind', 'Boolean');
      ndNumber: SetValue(AForm, 'expected-kind', 'Exact decimal');
      else
      begin
        raise ENyxFlow.Create('Saved JSON filter expected value must be scalar');
      end;
    end;

    if LConfiguration.Field('expected').Kind = ndText then
    begin
      SetValue(AForm, 'expected', LConfiguration.Field('expected').AsText);
    end
    else
    begin
      SetValue(AForm, 'expected', LConfiguration.Field('expected').ToJSON);
    end;
  end
  else if LKind = 'json-sort' then
  begin
    SetValue(AForm, 'key-kind', CKeyKinds[TNyxJSONKeyKind(LConfiguration.Field('kind').AsInteger)]);
    SetValue(AForm, 'direction', 'Ascending');

    if LConfiguration.Field('direction').AsInteger = Ord(njdDescending) then
    begin
      SetValue(AForm, 'direction', 'Descending');
    end;
  end
  else if LKind = 'json-slice' then
  begin
    SetValue(AForm, 'offset', LConfiguration.Field('offset').ToJSON);
    SetValue(AForm, 'count', LConfiguration.Field('count').ToJSON);
  end;

  if RelationOperation(LKind) then
  begin
    SetValue(AForm, 'key-kind', CKeyKinds[TNyxJSONKeyKind(LConfiguration.Field('kind').AsInteger)]);
    SetValue(AForm, 'null-keys', CNullKeys[TNyxJSONNullKeys(LConfiguration.Field('nulls').AsInteger)]);

    if LKind = 'json-join' then
    begin
      SetValue(AForm, 'join-kind', CJoinKinds[TNyxJSONJoinKind(LConfiguration.Field('join').AsInteger)]);
    end;
  end
  else if LKind = 'json-aggregate' then
  begin
    SetValue(AForm, 'aggregation', CAggregations[TNyxJSONAggregation(LConfiguration.Field('aggregation').AsInteger)]);
    SetValue(AForm, 'rounding', CRoundings[TNyxDecimalRounding(LConfiguration.Field('rounding').AsInteger)]);
    SetValue(AForm, 'empty-aggregate', CEmptyAggregates[TNyxJSONEmptyAggregate(LConfiguration.Field('empty').AsInteger)]);
  end;

  if LKind = 'flow-join' then
  begin
    AForm.Extensions.SetValue(NyxExtension(CBranches), LConfiguration.Field('inputs'));
    ShowBranches(AForm);
    SetValue(AForm, 'branch-choice', LConfiguration.Field('inputs').Item(0).AsText);
    SetValue(AForm, 'branch-type', NyxFlowTypeName(TNyxFlowType(LConfiguration.Field('type').AsInteger)));
    SetValue(AForm, 'branch-selection', CBranchSelections[TNyxFlowJoinSelection(LConfiguration.Field('selection').AsInteger)]);
    SetValue(AForm, 'branch-empty', CBranchEmpty[TNyxFlowJoinEmpty(LConfiguration.Field('empty').AsInteger)]);
  end;

  if LKind = 'json-set' then
  begin
    SetValue(AForm, 'set-mode', CSetModes[TNyxJSONSetMode(LConfiguration.Field('mode').AsInteger)]);
  end
  else if LKind = 'json-remove' then
  begin
    SetValue(AForm, 'missing-policy', CMissing[TNyxJSONMissing(LConfiguration.Field('missing').AsInteger)]);
  end
  else if LKind = 'json-merge' then
  begin
    SetValue(AForm, 'merge-policy', CMergePolicies[TNyxJSONMergePolicy(LConfiguration.Field('policy').AsInteger)]);
  end;

  if LKind = 'http-call' then
  begin
    LoadNyxHTTPRequestEditor(AForm.ID + '-http', AForm,
      TNyxHTTPRequest.FromData(LConfiguration));
  end;

  if LKind = 'file-save' then
  begin
    LoadNyxFileSaveEditor(AForm.ID + '-save', AForm,
      TNyxFileSaveOptions.FromData(LConfiguration));
  end;

  if LKind = 'file-open' then
  begin
    LoadNyxFileOpenEditor(AForm.ID + '-file-open', AForm,
      TNyxFileOpenOptions.FromData(LConfiguration));
  end;
  SetValue(AForm, 'creator-config', LConfiguration.ToJSON);
  ShowConfiguration(AForm, LKind);
end;

{ Captions belong to the form boundary; only an exact closed choice becomes a
  typed enum. Unknown/unfinished text cannot quietly select a default policy. }
function ChoiceIndex(AForm: TNyxNode; const AName: TNyxText;
  const AChoices: array of TNyxText): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(AChoices) do
  begin

    if Value(AForm, AName) = AChoices[LIndex] then
    begin
      Exit(LIndex);
    end;
  end;
  raise ENyxFlow.Create('Choose a typed JSON editing policy');
end;

function ConfiguredOperation(AForm: TNyxNode): INyxLogicOperation;
var
  LKind: TNyxText;
  LEntry: TNyxDataValue;
  LIndent: Integer;
  LLayout: TNyxJSONLayout;
  LDescriptor: TNyxDataValue;
  LComparison: TNyxJSONComparison;
  LKeyKind: TNyxJSONKeyKind;
  LDirection: TNyxJSONDirection;
  LExpected: TNyxDataValue;
  LFound: Boolean;
  LOffset: Integer;
  LCount: Integer;
  LAggregation: TNyxJSONAggregation;
  LPath: TNyxDataPath;
  LOptions: TNyxFlowJoinOptions;
  LType: TNyxFlowType;
  LInputs: TNyxDataValue;
  LIndex: Integer;
begin
  LKind := Value(AForm, 'operation');
  LEntry := Entry(AForm, LKind);

  if LKind = 'json-parse' then
  begin
    Result := NewNyxJSONParse;
  end
  else if LKind = 'json-format' then
  begin
    LLayout := njlIndented;

    if Value(AForm, 'layout') = 'Compact' then
    begin
      LLayout := njlCompact;
    end
    else if Value(AForm, 'layout') <> 'Indented' then
    begin
      raise ENyxFlow.Create('Choose a typed JSON layout');
    end;

    if not TryStrToInt(Value(AForm, 'indent'), LIndent) then
    begin
      raise ENyxFlow.Create('Indent must be an integer from 1 to 8');
    end;
    Result := NewNyxJSONFormat(LLayout, LIndent);
  end
  else if LKind = 'json-select' then
  begin
    Result := NewNyxJSONSelect(TNyxDataPath.FromData(
      AForm.Extensions.Value(NyxExtension(CPath))));
  end
  else if LKind = 'json-set' then
  begin
    Result := NewNyxJSONSet(TNyxDataPath.FromData(AForm.Extensions.Value(NyxExtension(CPath))),
      TNyxJSONSetMode(ChoiceIndex(AForm, 'set-mode', CSetModes)));
  end
  else if LKind = 'json-remove' then
  begin
    Result := NewNyxJSONRemove(TNyxDataPath.FromData(AForm.Extensions.Value(NyxExtension(CPath))),
      TNyxJSONMissing(ChoiceIndex(AForm, 'missing-policy', CMissing)));
  end
  else if LKind = 'json-merge' then
  begin
    Result := NewNyxJSONMerge(TNyxJSONMergePolicy(ChoiceIndex(AForm, 'merge-policy', CMergePolicies)));
  end
  else if LKind = 'json-group' then
  begin
    Result := NewNyxJSONGroup(TNyxDataPath.FromData(AForm.Extensions.Value(NyxExtension(CPath))),
      TNyxJSONKeyKind(ChoiceIndex(AForm, 'key-kind', CKeyKinds)),
      TNyxJSONNullKeys(ChoiceIndex(AForm, 'null-keys', CNullKeys)));
  end
  else if LKind = 'json-distinct' then
  begin
    Result := NewNyxJSONDistinct(TNyxDataPath.FromData(AForm.Extensions.Value(NyxExtension(CPath))),
      TNyxJSONKeyKind(ChoiceIndex(AForm, 'key-kind', CKeyKinds)),
      TNyxJSONNullKeys(ChoiceIndex(AForm, 'null-keys', CNullKeys)));
  end
  else if LKind = 'json-join' then
  begin
    Result := NewNyxJSONJoin(TNyxDataPath.FromData(AForm.Extensions.Value(NyxExtension(CPath))),
      TNyxDataPath.FromData(AForm.Extensions.Value(NyxExtension(CRightPath))),
      TNyxJSONKeyKind(ChoiceIndex(AForm, 'key-kind', CKeyKinds)),
      TNyxJSONJoinKind(ChoiceIndex(AForm, 'join-kind', CJoinKinds)),
      TNyxJSONNullKeys(ChoiceIndex(AForm, 'null-keys', CNullKeys)));
  end
  else if LKind = 'json-aggregate' then
  begin
    LAggregation := TNyxJSONAggregation(ChoiceIndex(AForm, 'aggregation', CAggregations));
    LPath := NyxDataPath;

    if LAggregation <> njaCount then
    begin
      LPath := TNyxDataPath.FromData(AForm.Extensions.Value(NyxExtension(CPath)));
    end;
    Result := NewNyxJSONAggregate(LPath, LAggregation,
      TNyxDecimalRounding(ChoiceIndex(AForm, 'rounding', CRoundings)),
      TNyxJSONEmptyAggregate(ChoiceIndex(AForm, 'empty-aggregate', CEmptyAggregates)));
  end
  else if LKind = 'flow-join' then
  begin
    LFound := False;
    for LType := Low(TNyxFlowType) to High(TNyxFlowType) do
    begin

      if Value(AForm, 'branch-type') = NyxFlowTypeName(LType) then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxFlow.Create('Choose an exact branch payload family');
    end;
    LOptions := NyxFlowJoinOptions(LType)
      .Select(TNyxFlowJoinSelection(ChoiceIndex(AForm, 'branch-selection', CBranchSelections)))
      .WhenEmpty(TNyxFlowJoinEmpty(ChoiceIndex(AForm, 'branch-empty', CBranchEmpty)));
    LInputs := AForm.Extensions.Value(NyxExtension(CBranches));
    for LIndex := 0 to LInputs.Count - 1 do
    begin
      LOptions := LOptions.Input(NyxFlowPort(LInputs.Item(LIndex).AsText));
    end;
    Result := NewNyxFlowJoin(LOptions);
  end
  else if LKind = 'json-map' then
  begin
    Result := NewNyxJSONMap(TNyxDataPath.FromData(AForm.Extensions.Value(NyxExtension(CPath))));
  end
  else if LKind = 'json-filter' then
  begin
    LFound := False;
    for LComparison := Low(TNyxJSONComparison) to High(TNyxJSONComparison) do
    begin

      if Value(AForm, 'comparison') = CComparisons[LComparison] then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxFlow.Create('Choose a typed filter comparison');
    end;

    if Value(AForm, 'expected-kind') = 'Text' then
    begin
      LExpected := NyxData(Value(AForm, 'expected'));
    end
    else if Value(AForm, 'expected-kind') = 'Null' then
    begin
      LExpected := NyxNull;
    end
    else if Value(AForm, 'expected-kind') = 'Exact decimal' then
    begin
      LExpected := NyxData(NyxDecimal(Value(AForm, 'expected')));
    end
    else if (Value(AForm, 'expected-kind') = 'Boolean') and
      ((Value(AForm, 'expected') = 'true') or (Value(AForm, 'expected') = 'false')) then
    begin
      LExpected := NyxData(Value(AForm, 'expected') = 'true');
    end
    else
    begin
      raise ENyxFlow.Create('Expected value must match its selected scalar family');
    end;
    Result := NewNyxJSONFilter(TNyxDataPath.FromData(AForm.Extensions.Value(NyxExtension(CPath))),
      LComparison, LExpected);
  end
  else if LKind = 'json-sort' then
  begin
    LFound := False;
    for LKeyKind := Low(TNyxJSONKeyKind) to High(TNyxJSONKeyKind) do
    begin

      if Value(AForm, 'key-kind') = CKeyKinds[LKeyKind] then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxFlow.Create('Choose a typed sort key family');
    end;
    LDirection := njdAscending;

    if Value(AForm, 'direction') = 'Descending' then
    begin
      LDirection := njdDescending;
    end
    else if Value(AForm, 'direction') <> 'Ascending' then
    begin
      raise ENyxFlow.Create('Choose a typed sort direction');
    end;
    Result := NewNyxJSONSort(TNyxDataPath.FromData(AForm.Extensions.Value(NyxExtension(CPath))),
      LKeyKind, LDirection);
  end
  else if LKind = 'json-slice' then
  begin

    if not TryStrToInt(Value(AForm, 'offset'), LOffset) or
      not TryStrToInt(Value(AForm, 'count'), LCount) then
    begin
      raise ENyxFlow.Create('Page offset and count must be nonnegative integers');
    end;
    Result := NewNyxJSONSlice(LOffset, LCount);
  end
  else if LKind = 'http-call' then
  begin
    Result := NewNyxHTTPCall(CaptureNyxHTTPRequestEditor(AForm.ID + '-http', AForm));
  end
  else if LKind = 'file-save' then
  begin
    Result := NewNyxSaveFile(CaptureNyxFileSaveEditor(AForm.ID + '-save', AForm));
  end
  else if LKind = 'file-open' then
  begin
    Result := NewNyxOpenFile(CaptureNyxFileOpenEditor(AForm.ID + '-file-open', AForm));
  end
  else if LKind = 'utf8-encode' then
  begin
    Result := NewNyxUTF8Encode;
  end
  else if LKind = 'utf8-decode' then
  begin
    Result := NewNyxUTF8Decode;
  end
  else if LKind = 'text-join' then
  begin
    Result := NewNyxTextJoin(Value(AForm, 'separator'));
  end
  else
  begin
    LDescriptor := AForm.Extensions.Value(NyxExtension(CDescriptor));

    if (LDescriptor.Kind <> ndObject) or (LDescriptor.Field('kind').AsText <> LKind) then
    begin
      LDescriptor := LEntry.Field('descriptor');
    end;
    Result := NewNyxLogicDeclaration(
      TNyxLogicDescriptor.FromData(LDescriptor),
      TNyxDataValue.ParseJSON(Value(AForm, 'creator-config')));
  end;
end;

function NewNyxLogicEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxFlowRef; const ACatalog: INyxLogicCatalog): INyxCard;
var
  LCatalog: INyxLogicCatalog;
  LEntries: array of TNyxDataValue;
  LRegistry: INyxFlowDeclarations;
  LNames: array of TNyxDataValue;
  LEndpoints: array of TNyxDataValue;
  LDefinition: INyxFlowDefinition;
  LDescriptor: TNyxLogicDescriptor;
  LPort: TNyxFlowPort;
  LBaseline: TNyxDataValue;
  LIndex: Integer;
  LPart: Integer;
  LCount: Integer;
  LGraphNames: TNyxText;
  LKinds: TNyxText;
  LStepNames: TNyxText;
  LSources: TNyxText;
  LTargets: TNyxText;
  LLabel: TNyxText;
  LGroup: TNyxLogicGroup;
  LColumn: INyxColumn;
  LCard: INyxCard;
  LGroupColumn: INyxColumn;

  procedure Section(const AName, ACaption: TNyxText; const ANames: array of TNyxText);
  var
    LSection: INyxDisclosure;
    LNode: TNyxNode;
    LPartIndex: Integer;
    LChildIndex: Integer;
  begin
    LSection := NewNyxDisclosure(AID + '-' + AName).WithText(ACaption);
    LSection.Configure.Value(True).Surface(False).Padding(0).Gap(10).Done;
    Result.Add(LSection);
    for LPartIndex := 0 to High(ANames) do
    begin
      LNode := Result.Node.Find(AID + '-' + ANames[LPartIndex]);

      if (LNode = nil) or (LNode.Parent <> Result.Node) then
      begin
        raise ENyxFlow.Create('Logic section must adopt its exact independently owned controls');
      end;
      for LChildIndex := 0 to Result.Node.Count - 1 do
      begin

        if Result.Node.Children[LChildIndex] = LNode then
        begin
          Result.Node.Extract(LChildIndex);
          Break;
        end;
      end;
      LSection.BodyColumn.Node.Add(LNode);
    end;
  end;

  procedure Input(const AName, ACaption, AValue: TNyxText);
  begin
    Result.Add(NewNyxInput(AID + '-' + AName).Configure.Text(ACaption).Value(AValue).Done);
  end;

  procedure Choice(const AName, ACaption, AItems, AValue: TNyxText);
  begin
    Result.Add(NewNyxSelect(AID + '-' + AName).Configure.Text(ACaption)
      .Items(AItems).Value(AValue).Done);
  end;

  procedure Button(const AName, ACaption: TNyxText);
  begin
    Result.Add(NewNyxButton(AID + '-' + AName).WithText(ACaption));
  end;

begin

  if ADocument = nil then
  begin
    raise ENyxFlow.Create('Logic authoring requires a document');
  end;
  { Snapshot document input before calling an alternate creator catalog. Its
    getters may retire the caller's document; the form needs only owned values. }
  LRegistry := ADocument.Flows.Clone;
  LCatalog := ACatalog;

  if LCatalog = nil then
  begin
    LCatalog := NewNyxLogicCatalog;
  end;
  LDefinition := nil;
  LBaseline := NyxObject([NyxField('exists', NyxData(False)), NyxField('definition', NyxNull)]);

  if AReference.Defined then
  begin
    LDefinition := LRegistry.Definition(AReference);
    LBaseline := NyxFlowBaseline(LRegistry, AReference);
    { Imported creator declarations remain discoverable without manufacturing
      executable implementations. Retain the host's catalog entry when present. }
    for LIndex := 0 to LDefinition.StepCount - 1 do
    begin
      LDescriptor := LDefinition.Descriptor(LIndex);
      LPart := 0;
      while (LPart < LCatalog.Count) and
        (LCatalog.Descriptor(LPart).Kind.Name <> LDescriptor.Kind.Name) do
      begin
        Inc(LPart);
      end;

      if LPart = LCatalog.Count then
      begin
        LCatalog := LCatalog.WithOperation(NewNyxLogicDeclaration(LDescriptor,
          LDefinition.Configuration(LIndex)));
      end;
    end;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(16).Gap(12).Done;
  LGraphNames := '';
  SetLength(LNames, LRegistry.Count);
  for LIndex := 0 to LRegistry.Count - 1 do
  begin
    LNames[LIndex] := NyxData(LRegistry.Reference(LIndex).Name);

    if LIndex > 0 then
    begin
      LGraphNames := LGraphNames + #10;
    end;
    LGraphNames := LGraphNames + LRegistry.Reference(LIndex).Name;
  end;
  Result.Add(NewNyxHeading(AID + '-title').WithText('Logic'));
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'Compose reusable operations with typed connections. Each applied change is undoable.'));
  Choice('graph', 'Saved graph', LGraphNames, AReference.Name);
  Button('open', 'Open graph');
  Input('graph-name', 'New graph name', 'My tool');
  Button('create', 'Create graph with JSON parse');
  Input('search', 'Find an operation', '');
  Choice('group', 'Intent', 'All groups' + #10 + 'JSON' + #10 + 'Text' + #10 +
    'Files' + #10 + 'Collections' + #10 + 'Network' + #10 + 'Data' + #10 + 'Flow', 'All groups');
  Choice('grouped', 'Toolbox view', 'List' + #10 + 'Grouped', 'List');
  SetLength(LEntries, LCatalog.Count);
  LKinds := '';
  for LIndex := 0 to LCatalog.Count - 1 do
  begin
    LDescriptor := LCatalog.Descriptor(LIndex);
    LEntries[LIndex] := OperationData(LDescriptor, LCatalog.Configuration(LIndex));

    if LIndex > 0 then
    begin
      LKinds := LKinds + #10;
    end;
    LKinds := LKinds + LDescriptor.Kind.Name;
  end;
  LColumn := NewNyxColumn(AID + '-collection');
  LColumn.Configure.Gap(10).Done;
  Result.Add(LColumn);
  for LGroup := Low(TNyxLogicGroup) to High(TNyxLogicGroup) do
  begin
    LGroupColumn := NewNyxColumn(AID + '-group-' + TNyxText(IntToStr(Ord(LGroup))));
    LGroupColumn.Configure.Gap(10).Done;
    LGroupColumn.Add(NewNyxHeading(LGroupColumn.Node.ID + '-heading')
      .Configure.Text(NyxLogicGroupName(LGroup)).Visible(False).Done);
    for LIndex := 0 to LCatalog.Count - 1 do
    begin
      LDescriptor := LCatalog.Descriptor(LIndex);

      if LDescriptor.Group <> LGroup then
      begin
        Continue;
      end;
      LCard := NewNyxCard(AID + '-tool-' + TNyxText(IntToStr(LIndex)));
      LCard.Configure.Padding(12).Gap(8).Done;
      LCard.Node.Extensions.SetValue(NyxExtension('nyx.logic-kind'), NyxData(LDescriptor.Kind.Name));
      LCard.Add(NewNyxLabel(LCard.Node.ID + '-title').WithText(LDescriptor.Title));
      LCard.Add(NewNyxLabel(LCard.Node.ID + '-help').WithText(LDescriptor.Description));
      LCard.Add(NewNyxButton(LCard.Node.ID + '-use').WithText('Configure'));
      LGroupColumn.Add(LCard);
    end;
    LGroupColumn.Configure.Visible(LGroupColumn.Node.Count > 1).Done;
    LColumn.Add(LGroupColumn);
  end;
  Input('step-name', 'New step name', 'step');
  Choice('operation', 'Operation', LKinds, 'json-parse');
  Button('defaults', 'Load operation defaults');
  Choice('layout', 'JSON layout', 'Indented' + #10 + 'Compact', 'Indented');
  Input('indent', 'Indent width', '2');
  Input('separator', 'Join separator', '');
  Choice('comparison', 'Filter comparison', 'Equal' + #10 + 'Not equal' + #10 +
    'Less than' + #10 + 'At most' + #10 + 'Greater than' + #10 + 'At least' + #10 +
    'Contains' + #10 + 'Starts with' + #10 + 'Ends with', 'Equal');
  Choice('expected-kind', 'Expected family', 'Text' + #10 + 'Boolean' + #10 +
    'Exact decimal' + #10 + 'Null', 'Boolean');
  Input('expected', 'Expected value (Boolean: true/false)', 'true');
  Choice('key-kind', 'Key family', 'Text' + #10 + 'Boolean' + #10 + 'Exact decimal', 'Text');
  Choice('direction', 'Sort direction', 'Ascending' + #10 + 'Descending', 'Ascending');
  Input('offset', 'Page offset', '0');
  Input('count', 'Page size', '20');
  Choice('set-mode', 'Update behavior (parents must exist)',
    'Upsert' + #10 + 'Replace existing' + #10 + 'Insert', 'Upsert');
  Choice('missing-policy', 'Missing path behavior',
    'Require existing' + #10 + 'Ignore missing', 'Require existing');
  Choice('merge-policy', 'Object merge behavior (arrays replace)',
    'Overlay (preserve null)' + #10 + 'Merge Patch (null deletes)', 'Overlay (preserve null)');
  Choice('join-kind', 'Join behavior', 'Inner' + #10 + 'Left' + #10 + 'Right' + #10 + 'Full', 'Inner');
  Choice('null-keys', 'Null key behavior', 'Reject null' + #10 + 'Match null', 'Reject null');
  Choice('aggregation', 'Summary operation',
    'Sum' + #10 + 'Average' + #10 + 'Minimum' + #10 + 'Maximum' + #10 + 'Count rows', 'Sum');
  Choice('rounding', 'Sum/average rounding', 'Exact (refuse loss)' + #10 +
    'Nearest, ties to even' + #10 + 'Nearest, ties away from zero' + #10 +
    'Toward zero' + #10 + 'Floor' + #10 + 'Ceiling', 'Exact (refuse loss)');
  Choice('empty-aggregate', 'Empty average/minimum/maximum',
    'Return null' + #10 + 'Report error', 'Return null');
  Choice('branch-type', 'Branch payload family',
    'Text' + #10 + 'Boolean' + #10 + 'Integer' + #10 + 'Number' + #10 + 'JSON' + #10 + 'Bytes' + #10 + 'Signal', 'Text');
  Choice('branch-selection', 'Available branch selection',
    'Exactly one' + #10 + 'First available' + #10 + 'Last available', 'Exactly one');
  Choice('branch-empty', 'No available branch', 'Skip outputs' + #10 + 'Report error', 'Skip outputs');
  Input('branch-name', 'New input reference (value/source are reserved)', '');
  Button('branch-add', 'Add branch input');
  Choice('branch-choice', 'Ordered alternatives', 'primary' + #10 + 'fallback', 'primary');
  Button('branch-up', 'Move alternative up');
  Button('branch-down', 'Move alternative down');
  Button('branch-remove', 'Remove alternative (confirmation required)');
  Result.Add(NewNyxCode(AID + '-branch-preview').WithText('["primary","fallback"]'));
  Result.Add(NewNyxHTTPRequestEditor(AID + '-http',
    NyxHTTPRequest(NyxResourceURL('https://example.invalid/api'))));
  Result.Add(NewNyxFileSaveEditor(AID + '-save', NyxFileSaveOptions('report.json')
    .MediaType(NyxMediaType(nfmtJSON)).Allow('json')));
  Result.Add(NewNyxFileOpenEditor(AID + '-file-open', NyxFileOpenOptions.Allow('json').Allow('txt')));
  Choice('path-side', 'Key path to edit', 'Left key' + #10 + 'Right key', 'Left key');
  Choice('path-kind', 'Path part', 'Field' + #10 + 'Item', 'Field');
  Input('path-part', 'Field name or zero-based item index', '');
  Button('path-add', 'Append path part');
  Button('path-clear', 'Use JSON root');
  Result.Add(NewNyxCode(AID + '-path-preview').WithText('[]'));
  Result.Add(NewNyxMemo(AID + '-creator-config').Configure
    .Text('Creator configuration JSON (extension data)').Value('{}').Done);
  Button('add', 'Add step');
  LStepNames := '';
  LSources := '';
  LTargets := '';
  LCount := 0;
  LEndpoints := nil;

  if LDefinition <> nil then
  begin
    for LIndex := 0 to LDefinition.StepCount - 1 do
    begin

      if LIndex > 0 then
      begin
        LStepNames := LStepNames + #10;
      end;
      LStepNames := LStepNames + LDefinition.Step(LIndex).Name;
      LDescriptor := LDefinition.Descriptor(LIndex);
      LCard := NewNyxCard(AID + '-step-card-' + TNyxText(IntToStr(LIndex)));
      LCard.Configure.Padding(12).Gap(8).Done;
      LCard.Add(NewNyxLabel(LCard.Node.ID + '-title').WithText(
        LDefinition.Step(LIndex).Name + ' / ' + LDescriptor.Title));
      LCard.Add(NewNyxLabel(LCard.Node.ID + '-help').WithText(LDescriptor.Description));
      LCard.Add(NewNyxLabel(LCard.Node.ID + '-condition').WithText(
        NyxFlowConditionCaption(LDefinition, LDefinition.Step(LIndex))));
      for LPart := 0 to LDescriptor.PortCount - 1 do
      begin
        LPort := LDescriptor.Port(LPart);
        LLabel := TNyxText(IntToStr(LCount + 1)) + ' / ' +
          LDefinition.Step(LIndex).Name + ' / ' + LPort.Reference.Name +
          ' (' + NyxFlowTypeName(LPort.ValueType) + ')';

        if LPort.Optional then
        begin
          LLabel := LLabel + ' optional';
        end;
        SetLength(LEndpoints, LCount + 1);
        LEndpoints[LCount] := NyxObject([NyxField('label', NyxData(LLabel)),
          NyxField('direction', NyxData(Ord(LPort.Direction))),
          NyxField('step', NyxData(LDefinition.Step(LIndex).Name)),
          NyxField('port', NyxData(LPort.Reference.Name))]);
        Inc(LCount);
        LCard.Add(NewNyxLabel(LCard.Node.ID + '-port-' + TNyxText(IntToStr(LPart)))
          .WithText(LLabel + ' / ' + LPort.Description));

        if LPort.Direction = nfdOutput then
        begin
          LSources := LSources + LLabel + #10;
        end
        else
        begin
          LTargets := LTargets + LLabel + #10;
        end;
      end;
      Result.Add(LCard);
    end;
    for LIndex := 0 to LDefinition.WireCount - 1 do
    begin
      LLabel := LDefinition.ToData.Field('wires').Item(LIndex).Field('source').Field('step').AsText +
        ' / ' + LDefinition.ToData.Field('wires').Item(LIndex).Field('source').Field('port').AsText +
        ' -> ' + LDefinition.ToData.Field('wires').Item(LIndex).Field('target').Field('step').AsText +
        ' / ' + LDefinition.ToData.Field('wires').Item(LIndex).Field('target').Field('port').AsText;
      Result.Add(NewNyxLabel(AID + '-wire-' + TNyxText(IntToStr(LIndex))).WithText(LLabel));
    end;
  end;
  Choice('step', 'Selected step', LStepNames, '');
  Button('load', 'Load selected step');
  Button('set', 'Apply configuration to step');
  Result.Add(NewNyxFlowConditionEditor(AID + '-condition-editor', LDefinition));
  Choice('source', 'Output port', LSources, '');
  Choice('target', 'Input port', LTargets, '');
  Button('connect', 'Connect ports');
  Result.Add(NewNyxLabel(AID + '-warning').WithText(
    'Removal deletes the chosen graph, step or exact connection. Step removal also detaches its data and condition dependencies.'));
  Choice('confirm', 'Removal warning', 'Keep' + #10 + 'Confirm removal', 'Keep');
  Button('disconnect', 'Remove connection');
  Button('remove-step', 'Remove step and its dependencies');
  Button('remove-graph', 'Remove graph');
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), NyxObject([
    NyxField('flow', NyxData(AReference.Name)), NyxField('baseline', LBaseline),
    NyxField('names', NyxArray(LNames)), NyxField('catalog', NyxArray(LEntries)),
    NyxField('endpoints', NyxArray(LEndpoints))]));
  Result.Node.Extensions.SetValue(NyxExtension(CPath), NyxArray([]));
  Result.Node.Extensions.SetValue(NyxExtension(CRightPath), NyxArray([]));
  Result.Node.Extensions.SetValue(NyxExtension(CBranches), NyxArray([NyxData('primary'), NyxData('fallback')]));
  Result.Node.Extensions.SetValue(NyxExtension(CDescriptor), NyxNull);
  Section('toolbox', 'Logic collection', ['search', 'group', 'grouped', 'collection']);
  Section('configuration', 'Configure operation', ['step-name', 'operation', 'defaults',
    'layout', 'indent', 'separator', 'path-side', 'path-kind', 'path-part', 'path-add', 'path-clear',
    'path-preview', 'creator-config', 'comparison', 'expected-kind', 'expected',
    'key-kind', 'direction', 'offset', 'count', 'set-mode', 'missing-policy',
    'merge-policy', 'join-kind', 'null-keys', 'aggregation', 'rounding', 'empty-aggregate',
    'branch-type', 'branch-selection', 'branch-empty', 'branch-name', 'branch-add',
    'branch-choice', 'branch-up', 'branch-down', 'branch-remove', 'branch-preview',
    'http', 'save', 'file-open', 'add', 'step', 'load', 'set']);
  Section('conditions', 'Run conditions', ['condition-editor']);
  Section('connections', 'Connect ports', ['source', 'target', 'connect']);
  Section('removal', 'Remove with confirmation', ['warning', 'confirm', 'disconnect',
    'remove-step', 'remove-graph']);
  ShowConfiguration(Result.Node, 'json-parse');
  Field(Result.Node, 'open').Configure.Enabled(LRegistry.Count > 0).Done;
  Field(Result.Node, 'add').Configure.Enabled(AReference.Defined).Done;
  Field(Result.Node, 'set').Configure.Enabled(AReference.Defined).Done;
  Field(Result.Node, 'load').Configure.Enabled(AReference.Defined).Done;
  Field(Result.Node, 'connect').Configure.Enabled(AReference.Defined).Done;
  Field(Result.Node, 'disconnect').Configure.Enabled(AReference.Defined).Done;
  Field(Result.Node, 'remove-step').Configure.Enabled(AReference.Defined).Done;
  Field(Result.Node, 'remove-graph').Configure.Enabled(AReference.Defined).Done;
end;

function HandleNyxLogicEditor(ANode, ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LContext: TNyxDataValue;
  LEntry: TNyxDataValue;
  LPath: TNyxDataValue;
  LParts: array of TNyxDataValue;
  LDescriptor: TNyxLogicDescriptor;
  LCatalog: INyxLogicCatalog;
  LIndex: Integer;
  LPart: Integer;
  LItem: Integer;
  LGroup: TNyxNode;
  LCard: TNyxNode;
  LAny: Boolean;
  LMatch: Boolean;
begin
  Result := False;
  LForm := FormFor(ANode, ARoot);

  if LForm = nil then
  begin
    Exit;
  end;
  LContext := LForm.Extensions.Value(NyxExtension(CEditor));

  if HandleNyxHTTPRequestEditor(ANode, ARoot) then
  begin
    Exit(True);
  end;

  if EditBranches(LForm, Copy(ANode.ID, Length(LForm.ID) + 2, MaxInt)) then
  begin
    Exit(True);
  end;

  if HandleNyxFlowConditionEditor(ANode, ARoot) then
  begin
    Exit(True);
  end;

  if (ANode.ID = LForm.ID + '-search') or (ANode.ID = LForm.ID + '-group') or
    (ANode.ID = LForm.ID + '-grouped') then
  begin
    LCatalog := NewNyxLogicCatalog;
    for LIndex := 0 to LContext.Field('catalog').Count - 1 do
    begin
      LEntry := LContext.Field('catalog').Item(LIndex);
      LCatalog := LCatalog.WithOperation(NewNyxLogicDeclaration(
        TNyxLogicDescriptor.FromData(LEntry.Field('descriptor')), LEntry.Field('configuration')));
    end;
    for LIndex := 0 to Field(LForm, 'collection').Count - 1 do
    begin
      LGroup := Field(LForm, 'collection').Children[LIndex];
      LAny := False;
      LGroup.Children[0].Configure.Visible(Value(LForm, 'grouped') = 'Grouped').Done;
      for LPart := 1 to LGroup.Count - 1 do
      begin
        LCard := LGroup.Children[LPart];
        LEntry := Entry(LForm, LCard.Extensions.Value(NyxExtension('nyx.logic-kind')).AsText);
        LDescriptor := TNyxLogicDescriptor.FromData(LEntry.Field('descriptor'));
        LMatch := False;
        for LItem := 0 to LCatalog.Count - 1 do
        begin

          if LCatalog.Descriptor(LItem).Kind.Name = LDescriptor.Kind.Name then
          begin
            LMatch := LCatalog.Matches(LItem, Value(LForm, 'search'));
            Break;
          end;
        end;
        LMatch := LMatch and ((Value(LForm, 'group') = 'All groups') or
          (Value(LForm, 'group') = NyxLogicGroupName(LDescriptor.Group)));
        LCard.Configure.Visible(LMatch).Done;
        LAny := LAny or LMatch;
      end;
      LGroup.Configure.Visible(LAny).Done;
    end;
    Exit(True);
  end;

  if (ANode.Parent <> nil) and ANode.Parent.Extensions.Has(NyxExtension('nyx.logic-kind')) then
  begin
    LoadConfiguration(LForm, Entry(LForm,
      ANode.Parent.Extensions.Value(NyxExtension('nyx.logic-kind')).AsText));
    Exit(True);
  end;

  if (ANode.ID = LForm.ID + '-defaults') or (ANode.ID = LForm.ID + '-operation') then
  begin
    LoadConfiguration(LForm, Entry(LForm, Value(LForm, 'operation')));
    Exit(True);
  end;

  if ANode.ID = LForm.ID + '-load' then
  begin
    LEntry := LContext.Field('baseline').Field('definition').Field('steps');
    for LIndex := 0 to LEntry.Count - 1 do
    begin

      if LEntry.Item(LIndex).Field('id').AsText = Value(LForm, 'step') then
      begin
        LoadConfiguration(LForm, LEntry.Item(LIndex));
        Exit(True);
      end;
    end;
    raise ENyxFlow.Create('Choose a saved step to load');
  end;

  if (ANode.ID = LForm.ID + '-path-side') or (ANode.ID = LForm.ID + '-aggregation') then
  begin
    ShowConfiguration(LForm, Value(LForm, 'operation'));
    Field(LForm, 'path-preview').Configure.Text(
      LForm.Extensions.Value(NyxExtension(PathTarget(LForm))).ToJSON).Done;
    Exit(True);
  end;

  if (ANode.ID = LForm.ID + '-path-add') or (ANode.ID = LForm.ID + '-path-clear') then
  begin
    LParts := nil;

    if ANode.ID = LForm.ID + '-path-add' then
    begin
      LPath := LForm.Extensions.Value(NyxExtension(PathTarget(LForm)));
      SetLength(LParts, LPath.Count + 1);
      for LIndex := 0 to LPath.Count - 1 do
      begin
        LParts[LIndex] := LPath.Item(LIndex);
      end;

      if Value(LForm, 'path-kind') = 'Field' then
      begin
        LParts[High(LParts)] := NyxData(Value(LForm, 'path-part'));
      end
      else if (Value(LForm, 'path-kind') = 'Item') and
        TryStrToInt(Value(LForm, 'path-part'), LItem) and (LItem >= 0) then
      begin
        LParts[High(LParts)] := NyxData(LItem);
      end
      else
      begin
        raise ENyxFlow.Create('Choose Field or a nonnegative Item index');
      end;
    end;
    LPath := TNyxDataPath.FromData(NyxArray(LParts)).ToData;
    LForm.Extensions.SetValue(NyxExtension(PathTarget(LForm)), LPath);
    Field(LForm, 'path-preview').Configure.Text(LPath.ToJSON).Done;
    Exit(True);
  end;
end;

function CaptureNyxLogicEditor(ANode, ARoot: TNyxNode;
  out ACapture: TNyxLogicEditorCapture): Boolean;
var
  LForm: TNyxNode;
  LContext: TNyxDataValue;
  LNames: TNyxDataValue;
  LIndex: Integer;
  LSource: TNyxFlowEndpoint;
  LTarget: TNyxFlowEndpoint;

  function Endpoint(const AField: TNyxText; ADirection: TNyxFlowDirection): TNyxFlowEndpoint;
  var
    LItems: TNyxDataValue;
    LPart: Integer;
    LValue: TNyxDataValue;
  begin
    LItems := LContext.Field('endpoints');
    for LPart := 0 to LItems.Count - 1 do
    begin
      LValue := LItems.Item(LPart);

      if (LValue.Field('label').AsText = Value(LForm, AField)) and
        (LValue.Field('direction').AsInteger = Ord(ADirection)) then
      begin
        Exit(NyxFlowEndpoint(NyxFlowStep(LValue.Field('step').AsText),
          NyxFlowPort(LValue.Field('port').AsText)));
      end;
    end;
    raise ENyxFlow.Create('Choose an exact typed port from this graph');
  end;

begin
  ACapture := Default(TNyxLogicEditorCapture);
  Result := False;
  LForm := FormFor(ANode, ARoot);

  if LForm = nil then
  begin
    Exit;
  end;
  LContext := LForm.Extensions.Value(NyxExtension(CEditor));

  if ANode.ID = LForm.ID + '-open' then
  begin
    ACapture.Open := True;
    ACapture.Reference := NyxFlowRef(Value(LForm, 'graph'));
    Exit(True);
  end;

  if ANode.ID = LForm.ID + '-create' then
  begin
    ACapture.Reference := NyxFlowRef(Value(LForm, 'graph-name'));
    LNames := LContext.Field('names');
    for LIndex := 0 to LNames.Count - 1 do
    begin

      if LNames.Item(LIndex).AsText = ACapture.Reference.Name then
      begin
        raise ENyxFlow.Create('Choose a new graph name or open the existing graph');
      end;
    end;
    ACapture.Change := NyxDefineFlow(ACapture.Reference, NyxFlowDefinitionFromData(
      NewNyxFlowGraph.WithStep(NyxFlowStep('parse'), NewNyxJSONParse).Build.ToData))
      .Expect(NyxObject([NyxField('exists', NyxData(False)), NyxField('definition', NyxNull)]));
    Exit(True);
  end;

  if LContext.Field('flow').AsText = '' then
  begin
    Exit;
  end;
  ACapture.Reference := NyxFlowRef(LContext.Field('flow').AsText);

  if CaptureNyxFlowConditionEditor(ANode, ARoot, ACapture.Reference, ACapture.Change) then
  begin
    ACapture.Change := ACapture.Change.Expect(LContext.Field('baseline'));
    Exit(True);
  end;

  if ANode.ID = LForm.ID + '-add' then
  begin
    ACapture.Change := NyxAddFlowStep(ACapture.Reference,
      NyxFlowStep(Value(LForm, 'step-name')), ConfiguredOperation(LForm));
  end
  else if ANode.ID = LForm.ID + '-set' then
  begin
    ACapture.Change := NyxReplaceFlowStep(ACapture.Reference,
      NyxFlowStep(Value(LForm, 'step')), ConfiguredOperation(LForm));
  end
  else if (ANode.ID = LForm.ID + '-connect') or (ANode.ID = LForm.ID + '-disconnect') then
  begin
    LSource := Endpoint('source', nfdOutput);
    LTarget := Endpoint('target', nfdInput);
    ACapture.Change := NyxConnectFlow(ACapture.Reference, LSource, LTarget);

    if ANode.ID = LForm.ID + '-disconnect' then
    begin
      ACapture.Change := NyxDisconnectFlow(ACapture.Reference, LSource, LTarget);
    end;
  end
  else if ANode.ID = LForm.ID + '-remove-step' then
  begin
    ACapture.Change := NyxRemoveFlowStep(ACapture.Reference, NyxFlowStep(Value(LForm, 'step')), True);
  end
  else if ANode.ID = LForm.ID + '-remove-graph' then
  begin
    ACapture.Change := NyxRemoveFlow(ACapture.Reference);
  end
  else
  begin
    Exit;
  end;

  if (ACapture.Change.Kind in [nfcRemove, nfcRemoveStep, nfcDisconnect]) and
    (Value(LForm, 'confirm') <> 'Confirm removal') then
  begin
    raise ENyxFlow.Create('Review and confirm the removal warning first');
  end;
  ACapture.Change := ACapture.Change.Expect(LContext.Field('baseline'));
  Result := True;
end;

procedure TNyxLogicEditorDraft.Clear;
begin
  Self := Default(TNyxLogicEditorDraft);
end;

function TNyxLogicEditorDraft.GetDefined: Boolean;
begin
  Result := FContext <> '';
end;

function TNyxLogicEditorDraft.ToData: TNyxDataValue;
var
  LVersion: Integer;
begin
  Result := NyxNull;

  if Defined then
  begin
    LVersion := 1;

    if FValues.Count > 17 then
    begin
      LVersion := 2;
    end;

    if FValues.Count > 33 then
    begin
      Exit(NyxObject([NyxField('version', NyxData(10)),
        NyxField('context', TNyxDataValue.ParseJSON(FContext)),
        NyxField('values', FValues), NyxField('path', FPath), NyxField('right-path', FRightPath),
        NyxField('branches', FBranches), NyxField('descriptor', FDescriptor),
        NyxField('http', FHTTP.ToData), NyxField('rows', FRows.ToData), NyxField('save', FSave.ToData),
        NyxField('open', FOpen.ToData), NyxField('conditions', FConditions.ToData)]));
    end;

    if FValues.Count > 27 then
    begin
      Exit(NyxObject([NyxField('version', NyxData(9)),
        NyxField('context', TNyxDataValue.ParseJSON(FContext)),
        NyxField('values', FValues), NyxField('path', FPath), NyxField('right-path', FRightPath),
        NyxField('descriptor', FDescriptor), NyxField('http', FHTTP.ToData),
        NyxField('rows', FRows.ToData), NyxField('save', FSave.ToData),
        NyxField('open', FOpen.ToData), NyxField('conditions', FConditions.ToData)]));
    end;

    if FValues.Count > 24 then
    begin
      { New controls have independent unfinished values. Earlier drafts keep
        their exact version/bytes; restoring them leaves new controls untouched. }
      Exit(NyxObject([NyxField('version', NyxData(8)),
        NyxField('context', TNyxDataValue.ParseJSON(FContext)),
        NyxField('values', FValues), NyxField('path', FPath),
        NyxField('descriptor', FDescriptor), NyxField('http', FHTTP.ToData),
        NyxField('rows', FRows.ToData), NyxField('save', FSave.ToData),
        NyxField('open', FOpen.ToData), NyxField('conditions', FConditions.ToData)]));
    end;

    if FConditions.Defined then
    begin
      Exit(NyxObject([NyxField('version', NyxData(7)),
        NyxField('context', TNyxDataValue.ParseJSON(FContext)),
        NyxField('values', FValues), NyxField('path', FPath),
        NyxField('descriptor', FDescriptor), NyxField('http', FHTTP.ToData),
        NyxField('rows', FRows.ToData), NyxField('save', FSave.ToData),
        NyxField('open', FOpen.ToData), NyxField('conditions', FConditions.ToData)]));
    end;

    if FOpen.Defined then
    begin
      Exit(NyxObject([NyxField('version', NyxData(6)),
        NyxField('context', TNyxDataValue.ParseJSON(FContext)),
        NyxField('values', FValues), NyxField('path', FPath),
        NyxField('descriptor', FDescriptor), NyxField('http', FHTTP.ToData),
        NyxField('rows', FRows.ToData), NyxField('save', FSave.ToData),
        NyxField('open', FOpen.ToData)]));
    end;

    if FSave.Defined then
    begin
      Exit(NyxObject([NyxField('version', NyxData(5)),
        NyxField('context', TNyxDataValue.ParseJSON(FContext)),
        NyxField('values', FValues), NyxField('path', FPath),
        NyxField('descriptor', FDescriptor), NyxField('http', FHTTP.ToData),
        NyxField('rows', FRows.ToData), NyxField('save', FSave.ToData)]));
    end;

    if FRows.Defined then
    begin
      Exit(NyxObject([NyxField('version', NyxData(4)),
        NyxField('context', TNyxDataValue.ParseJSON(FContext)),
        NyxField('values', FValues), NyxField('path', FPath),
        NyxField('descriptor', FDescriptor), NyxField('http', FHTTP.ToData),
        NyxField('rows', FRows.ToData)]));
    end;

    if FHTTP.Defined then
    begin
      Exit(NyxObject([NyxField('version', NyxData(3)),
        NyxField('context', TNyxDataValue.ParseJSON(FContext)),
        NyxField('values', FValues), NyxField('path', FPath),
        NyxField('descriptor', FDescriptor), NyxField('http', FHTTP.ToData)]));
    end;
    Result := NyxObject([NyxField('version', NyxData(LVersion)),
      NyxField('context', TNyxDataValue.ParseJSON(FContext)),
      NyxField('values', FValues), NyxField('path', FPath), NyxField('descriptor', FDescriptor)]);
  end;
end;

class function TNyxLogicEditorDraft.FromData(const AData: TNyxDataValue): TNyxLogicEditorDraft;
var
  LContext: TNyxDataValue;
  LIndex: Integer;
  LCount: Integer;
  LOptions: TNyxFlowJoinOptions;
begin
  Result := Default(TNyxLogicEditorDraft);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if AData.Field('version').AsInteger = 10 then
  begin
    ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'path', 'right-path', 'branches',
      'descriptor', 'http', 'rows', 'save', 'open', 'conditions']);
    Result.FBranches := AData.Field('branches').Copy;

    if (Result.FBranches.Kind <> ndArray) or (Result.FBranches.Count > 14) then
    begin
      raise ENyxFlow.Create('Join draft requires at most fourteen ordered alternatives');
    end;
    LOptions := NyxFlowJoinOptions(nftText);
    for LIndex := 0 to Result.FBranches.Count - 1 do
    begin
      LOptions := LOptions.Input(NyxFlowPort(Result.FBranches.Item(LIndex).AsText));
    end;
    Result.FRightPath := TNyxDataPath.FromData(AData.Field('right-path')).ToData;
    Result.FHTTP := TNyxHTTPRequestEditorDraft.FromData(AData.Field('http'));
    Result.FRows := TNyxFlowRowsEditorDraft.FromData(AData.Field('rows'));
    Result.FSave := TNyxFileSaveEditorDraft.FromData(AData.Field('save'));
    Result.FOpen := TNyxFileOpenEditorDraft.FromData(AData.Field('open'));
    Result.FConditions := TNyxFlowConditionEditorDraft.FromData(AData.Field('conditions'));
  end
  else if AData.Field('version').AsInteger = 9 then
  begin
    ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'path', 'right-path', 'descriptor',
      'http', 'rows', 'save', 'open', 'conditions']);
    Result.FRightPath := TNyxDataPath.FromData(AData.Field('right-path')).ToData;
    Result.FHTTP := TNyxHTTPRequestEditorDraft.FromData(AData.Field('http'));
    Result.FRows := TNyxFlowRowsEditorDraft.FromData(AData.Field('rows'));
    Result.FSave := TNyxFileSaveEditorDraft.FromData(AData.Field('save'));
    Result.FOpen := TNyxFileOpenEditorDraft.FromData(AData.Field('open'));
    Result.FConditions := TNyxFlowConditionEditorDraft.FromData(AData.Field('conditions'));
  end
  else if AData.Field('version').AsInteger = 8 then
  begin
    ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'path', 'descriptor',
      'http', 'rows', 'save', 'open', 'conditions']);
    Result.FHTTP := TNyxHTTPRequestEditorDraft.FromData(AData.Field('http'));
    Result.FRows := TNyxFlowRowsEditorDraft.FromData(AData.Field('rows'));
    Result.FSave := TNyxFileSaveEditorDraft.FromData(AData.Field('save'));
    Result.FOpen := TNyxFileOpenEditorDraft.FromData(AData.Field('open'));
    Result.FConditions := TNyxFlowConditionEditorDraft.FromData(AData.Field('conditions'));
  end
  else if AData.Field('version').AsInteger = 7 then
  begin
    ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'path', 'descriptor',
      'http', 'rows', 'save', 'open', 'conditions']);
    Result.FHTTP := TNyxHTTPRequestEditorDraft.FromData(AData.Field('http'));
    Result.FRows := TNyxFlowRowsEditorDraft.FromData(AData.Field('rows'));
    Result.FSave := TNyxFileSaveEditorDraft.FromData(AData.Field('save'));
    Result.FOpen := TNyxFileOpenEditorDraft.FromData(AData.Field('open'));
    Result.FConditions := TNyxFlowConditionEditorDraft.FromData(AData.Field('conditions'));

    if not Result.FConditions.Defined then
    begin
      raise ENyxFlow.Create('Version-7 Logic drafts require their owned condition fields');
    end;
  end
  else if AData.Field('version').AsInteger = 6 then
  begin
    ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'path', 'descriptor',
      'http', 'rows', 'save', 'open']);
    Result.FHTTP := TNyxHTTPRequestEditorDraft.FromData(AData.Field('http'));
    Result.FRows := TNyxFlowRowsEditorDraft.FromData(AData.Field('rows'));
    Result.FSave := TNyxFileSaveEditorDraft.FromData(AData.Field('save'));
    Result.FOpen := TNyxFileOpenEditorDraft.FromData(AData.Field('open'));

    if not Result.FOpen.Defined then
    begin
      raise ENyxFlow.Create('Version-6 Logic drafts require their owned open fields');
    end;
  end
  else if AData.Field('version').AsInteger = 5 then
  begin
    ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'path', 'descriptor', 'http', 'rows', 'save']);
    Result.FHTTP := TNyxHTTPRequestEditorDraft.FromData(AData.Field('http'));
    Result.FRows := TNyxFlowRowsEditorDraft.FromData(AData.Field('rows'));
    Result.FSave := TNyxFileSaveEditorDraft.FromData(AData.Field('save'));

    if not Result.FSave.Defined then
    begin
      raise ENyxFlow.Create('Version-5 Logic drafts require their owned save fields');
    end;
  end
  else if AData.Field('version').AsInteger = 4 then
  begin
    ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'path', 'descriptor', 'http', 'rows']);
    Result.FHTTP := TNyxHTTPRequestEditorDraft.FromData(AData.Field('http'));
    Result.FRows := TNyxFlowRowsEditorDraft.FromData(AData.Field('rows'));

    if not Result.FRows.Defined then
    begin
      raise ENyxFlow.Create('Version-4 Logic drafts require their owned Table mapping');
    end;
  end
  else if AData.Field('version').AsInteger = 3 then
  begin
    ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'path', 'descriptor', 'http']);
    Result.FHTTP := TNyxHTTPRequestEditorDraft.FromData(AData.Field('http'));

    if not Result.FHTTP.Defined then
    begin
      raise ENyxFlow.Create('Version-3 Logic drafts require their owned HTTP fields');
    end;
  end
  else
  begin
    ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'path', 'descriptor']);
  end;

  if not (AData.Field('version').AsInteger in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) then
  begin
    raise ENyxFlow.Create('Unknown Logic editor draft version');
  end;
  LContext := AData.Field('context');
  ValidateNyxFlowFields(LContext, ['flow', 'baseline', 'names', 'catalog', 'endpoints']);
  NyxRemoveFlow(NyxFlowRef('draft')).Expect(LContext.Field('baseline'));
  LCount := Length(CFields);

  if AData.Field('version').AsInteger = 1 then
  begin
    LCount := 17;
  end;

  if AData.Field('version').AsInteger in [2, 3, 4, 5, 6, 7] then
  begin
    LCount := 24;
  end;

  if AData.Field('version').AsInteger = 8 then
  begin
    LCount := 27;
  end;

  if AData.Field('version').AsInteger = 9 then
  begin
    LCount := 33;
  end;

  if (AData.Field('values').Kind <> ndObject) or
    (AData.Field('values').Count <> LCount) then
  begin
    raise ENyxFlow.Create('Logic editor draft fields differ from their version');
  end;
  for LIndex := 0 to LCount - 1 do
  begin
    AData.Field('values').Field(CFields[LIndex]).AsText;
  end;
  Result.FContext := LContext.ToJSON;
  Result.FValues := AData.Field('values');
  Result.FPath := TNyxDataPath.FromData(AData.Field('path')).ToData;
  Result.FDescriptor := AData.Field('descriptor');

  if Result.FDescriptor.Kind <> ndNull then
  begin
    Result.FDescriptor := TNyxLogicDescriptor.FromData(Result.FDescriptor).ToData;
  end;
end;

procedure TNyxLogicEditorDraft.Capture(const AID: TNyxText; ARoot: TNyxNode);
var
  LForm: TNyxNode;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
  LBranches: TNyxDataValue;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LForm := ARoot.Find(AID);

  if LForm = nil then
  begin
    Exit;
  end;
  LCount := Length(CFields);
  LBranches := LForm.Extensions.Value(NyxExtension(CBranches));

  { Compare typed members rather than serialized JSON whitespace: untouched
    new controls must preserve the native and browser legacy draft versions. }
  if (Value(LForm, 'operation') <> 'flow-join') and
    (Value(LForm, 'branch-name') = '') and (Value(LForm, 'branch-choice') = 'primary') and
    (Value(LForm, 'branch-type') = NyxFlowTypeName(nftText)) and
    (Value(LForm, 'branch-selection') = CBranchSelections[nfjExactlyOne]) and
    (Value(LForm, 'branch-empty') = CBranchEmpty[nfjeSkip]) and
    (LBranches.Count = 2) and (LBranches.Item(0).AsText = 'primary') and
    (LBranches.Item(1).AsText = 'fallback') then
  begin
    LCount := 33;
  end;

  if (LCount = 33) and not RelationOperation(Value(LForm, 'operation')) and
    (Value(LForm, 'operation') <> 'json-aggregate') and
    (Value(LForm, 'path-side') = 'Left key') and
    (Value(LForm, 'join-kind') = CJoinKinds[njjInner]) and
    (Value(LForm, 'aggregation') = CAggregations[njaSum]) and
    (Value(LForm, 'rounding') = CRoundings[ndrExact]) and
    (Value(LForm, 'empty-aggregate') = CEmptyAggregates[njaeNull]) and
    (Value(LForm, 'null-keys') = CNullKeys[njnReject]) and
    (LForm.Extensions.Value(NyxExtension(CRightPath)).Count = 0) then
  begin
    LCount := 27;
  end;

  if (LCount = 27) and (Value(LForm, 'set-mode') = CSetModes[njsUpsert]) and
    (Value(LForm, 'missing-policy') = CMissing[njmRequire]) and
    (Value(LForm, 'merge-policy') = CMergePolicies[njmpOverlay]) and
    (Value(LForm, 'operation') <> 'json-set') and
    (Value(LForm, 'operation') <> 'json-remove') and
    (Value(LForm, 'operation') <> 'json-merge') then
  begin
    LCount := 24;
  end;
  SetLength(LFields, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LFields[LIndex] := NyxField(CFields[LIndex], NyxData(Value(LForm, CFields[LIndex])));
  end;
  FContext := LForm.Extensions.Value(NyxExtension(CEditor)).ToJSON;
  FValues := NyxObject(LFields);
  FPath := LForm.Extensions.Value(NyxExtension(CPath));
  FRightPath := LForm.Extensions.Value(NyxExtension(CRightPath));
  FBranches := LForm.Extensions.Value(NyxExtension(CBranches));
  FDescriptor := LForm.Extensions.Value(NyxExtension(CDescriptor));
  FHTTP.Capture(AID + '-http', ARoot);
  FRows.Capture(AID + '-rows', ARoot);
  FSave.Capture(AID + '-save', ARoot, Value(LForm, 'operation') = 'file-save');
  FOpen.Capture(AID + '-file-open', ARoot, Value(LForm, 'operation') = 'file-open');
  FConditions.Capture(AID + '-condition-editor', ARoot, False);
end;

function TNyxLogicEditorDraft.Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LIndex: Integer;
  LContext: TNyxDataValue;
  LCurrent: TNyxDataValue;
begin
  Result := False;

  if (ARoot = nil) or (FContext = '') then
  begin
    Exit;
  end;
  LForm := ARoot.Find(AID);

  if LForm = nil then
  begin
    Exit;
  end;
  LContext := TNyxDataValue.ParseJSON(FContext);
  LCurrent := LForm.Extensions.Value(NyxExtension(CEditor));
  { Discovery may gain operations after an upgrade. It is not accepted design
    authority: retain unfinished values when the exact flow/baseline/membership/
    endpoints still match. A changed graph remains stale and refuses. }

  if (LCurrent.Field('flow').ToJSON <> LContext.Field('flow').ToJSON) or
    (LCurrent.Field('baseline').ToJSON <> LContext.Field('baseline').ToJSON) or
    (LCurrent.Field('names').ToJSON <> LContext.Field('names').ToJSON) or
    (LCurrent.Field('endpoints').ToJSON <> LContext.Field('endpoints').ToJSON) then
  begin
    Exit;
  end;
  for LIndex := 0 to FValues.Count - 1 do
  begin
    SetValue(LForm, CFields[LIndex], FValues.Field(CFields[LIndex]).AsText);
  end;
  LForm.Extensions.SetValue(NyxExtension(CPath), FPath);

  if FRightPath.Defined then
  begin
    LForm.Extensions.SetValue(NyxExtension(CRightPath), FRightPath);
  end;

  if FBranches.Defined then
  begin
    LForm.Extensions.SetValue(NyxExtension(CBranches), FBranches);
    ShowBranches(LForm);
  end;
  LForm.Extensions.SetValue(NyxExtension(CDescriptor), FDescriptor);
  FHTTP.Restore(AID + '-http', ARoot);
  FRows.Restore(AID + '-rows', ARoot);
  FSave.Restore(AID + '-save', ARoot);
  FOpen.Restore(AID + '-file-open', ARoot);
  FConditions.Restore(AID + '-condition-editor', ARoot);
  Field(LForm, 'path-preview').Configure.Text(
    LForm.Extensions.Value(NyxExtension(PathTarget(LForm))).ToJSON).Done;
  ShowConfiguration(LForm, Value(LForm, 'operation'));
  HandleNyxLogicEditor(Field(LForm, 'search'), ARoot);
  Result := True;
end;

end.
