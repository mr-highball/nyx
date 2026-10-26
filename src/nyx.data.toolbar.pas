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
unit nyx.data.toolbar;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.controls, nyx.model, nyx.collections,
  nyx.collections.query, nyx.coordinators, nyx.contract, nyx.data.toolbar.query,
  nyx.collections.query.controls;

const
  NyxDataToolbarIntentKey = 'nyx.data-toolbar';
  { Descriptor-boundary provenance retains the original caller value contract
    across Configure/Disable/Inherit, independently of the current intent mask. }
  NyxDataToolbarSortDomainKey = 'nyx.data-toolbar.sort-domain';
  NyxMaximumToolbarSorts = 64;

type
  { Configuration and command refusals use the portable collection family. }
  ENyxDataToolbar = class(ENyxCollection);
  { An old toolbar cannot reclaim a replaced query or mounted generation. }
  ENyxDataToolbarCancelled = class(ENyxDataToolbar);
  { Closed text matching behavior; field names and entered text remain data. }
  TNyxToolbarSearchMode = (ntsmContains, ntsmStartsWith, ntsmEqual);
  { An open presentation identity for one admitted sort choice, never a field
    name to interpret or a serialized instruction to execute. }
  TNyxToolbarSortRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  { A copied choice associates an open presentation name with a typed sort-only
    query. The separate baseline choice withdraws this actor's shared sort. }
  TNyxToolbarSortChoice = record
  private
    FReference: TNyxToolbarSortRef;
    FQuery: TNyxCollectionQuery;
    function GetQuery: TNyxCollectionQuery;
  public
    property Reference: TNyxToolbarSortRef read FReference;
    property Query: TNyxCollectionQuery read GetQuery;
  end;
  { Bounded immutable builder. Target is relative to the reusable expansion,
    not the global document. Copies detach choices; no runtime handles persist. }
  TNyxDataToolbarOptions = record
  private
    FDefined: Boolean;
    FTarget: TNyxControlRef;
    FSearch: TNyxTextFieldRef;
    FMode: TNyxToolbarSearchMode;
    FComparison: TNyxQueryTextComparison;
    FBaseline: TNyxToolbarSortRef;
    FChoices: array of TNyxToolbarSortChoice;
    FQueryContext: TNyxDataToolbarQueryOptions;
    function GetCount: Integer;
    function GetQueryContext: TNyxDataToolbarQueryOptions;
  public
    function SearchMode(AMode: TNyxToolbarSearchMode;
      AComparison: TNyxQueryTextComparison = nqtAsciiInsensitive): TNyxDataToolbarOptions;
    function Baseline(const AReference: TNyxToolbarSortRef): TNyxDataToolbarOptions;
    function Sort(const AReference: TNyxToolbarSortRef;
      const ASort: TNyxCollectionSort): TNyxDataToolbarOptions; overload;
    function Sort(const AReference: TNyxToolbarSortRef;
      const AQuery: TNyxCollectionQuery): TNyxDataToolbarOptions; overload;
    function Copy: TNyxDataToolbarOptions;
    { Optional summary/details do not change target query authority. Absence
      preserves old source/codec meaning; captions and visibility are caller data. }
    function QueryContext(const AOptions: TNyxDataToolbarQueryOptions): TNyxDataToolbarOptions;
    function ChoiceAt(AIndex: Integer): TNyxToolbarSortChoice;
    function SortIndex(const AReference: TNyxToolbarSortRef): Integer;
    procedure Validate;
    procedure ValidateSchema(const ASchema: TNyxCollectionSchema);
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDataToolbarOptions; static;
    property Target: TNyxControlRef read FTarget;
    property SearchField: TNyxTextFieldRef read FSearch;
    property MatchMode: TNyxToolbarSearchMode read FMode;
    property Comparison: TNyxQueryTextComparison read FComparison;
    property BaselineChoice: TNyxToolbarSortRef read FBaseline;
    property Count: Integer read GetCount;
    property QueryPresentation: TNyxDataToolbarQueryOptions read GetQueryContext;
  end;
  { Absence inherits the static recipe; Disabled deliberately masks runtime
    coordination. Configured persists complete options without machine paths. }
  TNyxDataToolbarIntentMode = (ntiUndefined, ntiDisabled, ntiConfigured);
  { Versioned, closed persisted intent. Loading metadata grants no executable
    factory, file service, renderer or collection mutation authority. }
  TNyxDataToolbarIntent = record
  private
    FMode: TNyxDataToolbarIntentMode;
    FOptions: TNyxDataToolbarOptions;
    function GetOptions: TNyxDataToolbarOptions;
  public
    class function Disabled: TNyxDataToolbarIntent; static;
    class function Configured(const AOptions: TNyxDataToolbarOptions):
      TNyxDataToolbarIntent; static;
    function Copy: TNyxDataToolbarIntent;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDataToolbarIntent; static;
    property Mode: TNyxDataToolbarIntentMode read FMode;
    property Options: TNyxDataToolbarOptions read GetOptions;
  end;
  { UI-thread-confined commands use the accepted scalar/query owners together.
    Clear removes only this actor's predicates and resets sort only if owned.
    Disconnect revokes authority without restoring an obsolete query. }
  INyxDataToolbarBinding = interface(INyxCoordinatorBinding)
    ['{D70659E5-53A3-4D10-A45D-F19D2D5E3A49}']
    procedure Search(const AText: TNyxText);
    procedure Sort(const AReference: TNyxToolbarSortRef);
    procedure Clear;
  end;
  { Optional copied observation and managed inspection preserve the original
    toolbar binding GUID. No executable actor token escapes. The query form
    retires with the same common coordinator scope as Search/Sort/Clear. }
  INyxDataToolbarQueryBinding = interface(IInterface)
    ['{F0BF0579-2E22-466C-A9E4-6B79951BCD65}']
    function GetObservation: TNyxQueryWorkbenchObservation;
    function GetQueryStatus: TNyxText;
    function GetQueryControls: INyxCollectionQueryWorkbench;
    procedure OpenQueryDetails;
    property Observation: TNyxQueryWorkbenchObservation read GetObservation;
    property QueryStatus: TNyxText read GetQueryStatus;
    { Nil until first OpenQueryDetails. The scope owns the lazily created form
      controller; caller copies become inert when that same scope retires. }
    property QueryControls: INyxCollectionQueryWorkbench read GetQueryControls;
  end;

function NyxToolbarSort(const AName: TNyxText): TNyxToolbarSortRef;
function NyxDataToolbarOptions(const ATarget: TNyxControlRef;
  const ASearchField: TNyxTextFieldRef): TNyxDataToolbarOptions;
{ Build a detached configured copy. This explicit value-style operation leaves
  the supplied authored toolbar unchanged on success AND refusal. Custom parts,
  captions, bindings and callback policies are copied. Keep the returned control
  when adding/replacing the application view. A new Clear part is opt-in only. }
function ConfigureNyxDataToolbar(const AToolbar: INyxDataToolbar;
  const AOptions: TNyxDataToolbarOptions): INyxDataToolbar;
{ Optional typed accessor preserves the original four-part interface/GUID. }
function NyxDataToolbarClearButton(const AToolbar: INyxDataToolbar): INyxButton;
{ Pure prospective selector contract. An exact list installed by the current
  configured intent can be replaced; other caller constraints remain enforced.
  No node/default/binding is changed. Both authored and realized roles qualify. }
function NyxDataToolbarSortDomain(AToolbar: TNyxNode;
  const AOptions: TNyxDataToolbarOptions): TNyxValueDomain; overload;
{ Explicit previous intent supports a detached inheritance edit whose metadata
  has already changed while its selector still has the previous owned choices. }
function NyxDataToolbarSortDomain(AToolbar: TNyxNode;
  const AOptions: TNyxDataToolbarOptions;
  const APrevious: TNyxDataToolbarIntent): TNyxValueDomain; overload;
{ Apply the prospective contract to an authored Sort or named descriptor. Exact
  original/applied descriptors distinguish our choices from a later caller edit.
  AToolbar is borrowed read-only and may be the pre-edit realized projection. }
procedure SetNyxDataToolbarSortDomain(const ASort: INyxControl;
  AToolbar: TNyxNode; const AOptions: TNyxDataToolbarOptions;
  const APrevious: TNyxDataToolbarIntent);
{ Authored concrete toolbars, reusable references and named overrides use the
  same typed metadata. Effective specialization is checked after realization. }
function HasNyxDataToolbarIntent(ANode: TNyxNode): Boolean;
function GetNyxDataToolbarIntent(ANode: TNyxNode): TNyxDataToolbarIntent;
procedure SetNyxDataToolbarIntent(const AToolbar: INyxControl;
  const AIntent: TNyxDataToolbarIntent);
procedure InheritNyxDataToolbarIntent(const AToolbar: INyxControl);
{ Strict metadata/known-host admission; unresolved references defer their
  effective target type. Runtime factory admission still checks exact roles. }
procedure ValidateNyxDataToolbarIntent(ANode: TNyxNode);
function NewNyxDataToolbarCoordinatorFactory: INyxCoordinatorFactory;

implementation

uses
  nyx.schema, nyx.state, nyx.binding.inputs, nyx.binding.types,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.query.session,
  nyx.collections.bindings, nyx.events, nyx.behavior, nyx.scheduler,
  nyx.publication.types, nyx.publication
  {$ifdef PAS2JS}, JS{$endif};

const
  CSearchNames: array[TNyxToolbarSearchMode] of TNyxText =
    ('contains', 'starts-with', 'equal');
  CComparisonNames: array[TNyxQueryTextComparison] of TNyxText =
    ('exact', 'ascii-insensitive');

function NyxToolbarSort(const AName: TNyxText): TNyxToolbarSortRef;
begin

  if (AName = '') or (Pos(#0, AName) > 0) or (Pos(#10, AName) > 0) or
    (Pos(#13, AName) > 0) then
  begin
    raise ENyxDataToolbar.Create('Sort choices require nonempty single-line names');
  end;
  Result.FName := AName;
end;

function NyxDataToolbarOptions(const ATarget: TNyxControlRef;
  const ASearchField: TNyxTextFieldRef): TNyxDataToolbarOptions;
begin
  Result := Default(TNyxDataToolbarOptions);
  Result.FDefined := True;
  Result.FTarget := ATarget;
  Result.FSearch := ASearchField;
  Result.FMode := ntsmContains;
  Result.FComparison := nqtAsciiInsensitive;
  Result.FBaseline := NyxToolbarSort('Original order');
  Result.Validate;
end;

function TNyxToolbarSortChoice.GetQuery: TNyxCollectionQuery;
begin
  Result := FQuery.Copy;
end;

function TNyxDataToolbarOptions.GetCount: Integer;
begin
  Result := Length(FChoices);
end;

function TNyxDataToolbarOptions.Copy: TNyxDataToolbarOptions;
var
  LIndex: Integer;
begin
  Result := Self;
  Result.FQueryContext := FQueryContext.Copy;
  SetLength(Result.FChoices, 0);
  SetLength(Result.FChoices, Length(FChoices));
  for LIndex := 0 to High(FChoices) do
  begin
    Result.FChoices[LIndex].FReference := FChoices[LIndex].Reference;
    Result.FChoices[LIndex].FQuery := FChoices[LIndex].Query;
  end;
end;

function TNyxDataToolbarOptions.GetQueryContext: TNyxDataToolbarQueryOptions;
begin
  Result := FQueryContext.Copy;
end;

function TNyxDataToolbarOptions.QueryContext(const AOptions: TNyxDataToolbarQueryOptions): TNyxDataToolbarOptions;
begin
  Result := Copy;
  Result.FQueryContext := AOptions.Copy;
  Result.Validate;
end;

function TNyxDataToolbarOptions.SearchMode(AMode: TNyxToolbarSearchMode;
  AComparison: TNyxQueryTextComparison): TNyxDataToolbarOptions;
begin
  Result := Copy;
  Result.FMode := AMode;
  Result.FComparison := AComparison;
  Result.Validate;
end;

function TNyxDataToolbarOptions.Baseline(const AReference: TNyxToolbarSortRef):
  TNyxDataToolbarOptions;
begin
  Result := Copy;
  Result.FBaseline := NyxToolbarSort(AReference.Name);
  Result.Validate;
end;

function TNyxDataToolbarOptions.Sort(const AReference: TNyxToolbarSortRef;
  const ASort: TNyxCollectionSort): TNyxDataToolbarOptions;
begin
  Result := Sort(AReference, NyxCollectionQuery.OrderBy(ASort));
end;

function TNyxDataToolbarOptions.Sort(const AReference: TNyxToolbarSortRef;
  const AQuery: TNyxCollectionQuery): TNyxDataToolbarOptions;
var
  LIndex: Integer;
begin
  Result := Copy;
  LIndex := Length(Result.FChoices);
  SetLength(Result.FChoices, LIndex + 1);
  Result.FChoices[LIndex].FReference := NyxToolbarSort(AReference.Name);
  Result.FChoices[LIndex].FQuery := AQuery.Copy;
  Result.Validate;
end;

function TNyxDataToolbarOptions.ChoiceAt(AIndex: Integer): TNyxToolbarSortChoice;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxDataToolbar.Create('Sort choice index is outside this toolbar');
  end;
  Result.FReference := FChoices[AIndex].Reference;
  Result.FQuery := FChoices[AIndex].Query;
end;

function TNyxDataToolbarOptions.SortIndex(const AReference: TNyxToolbarSortRef): Integer;
var
  LIndex: Integer;
begin

  if AReference.Name = FBaseline.Name then
  begin
    Exit(-1);
  end;
  for LIndex := 0 to High(FChoices) do
  begin

    if FChoices[LIndex].Reference.Name = AReference.Name then
    begin
      Exit(LIndex);
    end;
  end;
  raise ENyxDataToolbar.Create('The sort choice is not configured on this toolbar');
end;

procedure TNyxDataToolbarOptions.Validate;
var
  LIndex: Integer;
  LPrevious: Integer;
begin
  FQueryContext.Validate;

  if not FDefined or (FTarget.ID = '') or (FSearch.Name = '') or
    (Ord(FMode) < Ord(Low(TNyxToolbarSearchMode))) or
    (Ord(FMode) > Ord(High(TNyxToolbarSearchMode))) or
    (Ord(FComparison) < Ord(Low(TNyxQueryTextComparison))) or
    (Ord(FComparison) > Ord(High(TNyxQueryTextComparison))) or
    (Count >= NyxMaximumToolbarSorts) then
  begin
    raise ENyxDataToolbar.Create('Toolbar requires a target, text field and bounded choices');
  end;
  NyxControl(FTarget.ID);
  NyxTextField(FSearch.Name);
  NyxToolbarSort(FBaseline.Name);
  for LIndex := 0 to High(FChoices) do
  begin
    NyxToolbarSort(FChoices[LIndex].Reference.Name);

    if (FChoices[LIndex].Reference.Name = FBaseline.Name) or
      (FChoices[LIndex].Query.Filter <> nil) then
    begin
      raise ENyxDataToolbar.Create('Sort choices are unique and contain ordering only');
    end;
    for LPrevious := 0 to LIndex - 1 do
    begin

      if FChoices[LPrevious].Reference.Name = FChoices[LIndex].Reference.Name then
      begin
        raise ENyxDataToolbar.Create('Sort choice names must be unique');
      end;
    end;
  end;
end;

procedure TNyxDataToolbarOptions.ValidateSchema(const ASchema: TNyxCollectionSchema);
var
  LIndex: Integer;
begin
  Validate;
  NyxCollectionQuery.Where(NyxWhere(FSearch).Contains('', FComparison)).Validate(ASchema);
  for LIndex := 0 to High(FChoices) do
  begin
    FChoices[LIndex].FQuery.Validate(ASchema);
  end;
end;

function TNyxDataToolbarOptions.ToData: TNyxDataValue;
var
  LChoices: array of TNyxDataValue;
  LIndex: Integer;
  LFields: array of TNyxDataField;
begin
  Validate;
  SetLength(LChoices, Count);
  for LIndex := 0 to High(LChoices) do
  begin
    LChoices[LIndex] := NyxObject([
      NyxField('name', NyxData(FChoices[LIndex].Reference.Name)),
      NyxField('query', FChoices[LIndex].Query.ToData)]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('target', NyxData(FTarget.ID)), NyxField('search', NyxData(FSearch.Name)),
    NyxField('match', NyxData(CSearchNames[FMode])),
    NyxField('comparison', NyxData(CComparisonNames[FComparison])),
    NyxField('baseline', NyxData(FBaseline.Name)), NyxField('sorts', NyxArray(LChoices))]);

  if FQueryContext.Defined then
  begin
    SetLength(LFields, Result.Count + 1);
    for LIndex := 0 to Result.Count - 1 do
    begin
      LFields[LIndex] := NyxField(Result.Key(LIndex), Result.Field(Result.Key(LIndex)));
    end;
    LFields[High(LFields)] := NyxField('queryContext', FQueryContext.ToData);
    Result := NyxObject(LFields);
  end;
end;

class function TNyxDataToolbarOptions.FromData(const AData: TNyxDataValue):
  TNyxDataToolbarOptions;
var
  LMatch: TNyxToolbarSearchMode;
  LComparison: TNyxQueryTextComparison;
  LFound: Boolean;
  LChoices: TNyxDataValue;
  LChoice: TNyxDataValue;
  LIndex: Integer;
begin

  if (AData.Kind <> ndObject) or not (AData.Count in [7, 8]) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxDataToolbar.Create('Toolbar options require an exact version-one object');
  end;
  Result := NyxDataToolbarOptions(NyxControl(AData.Field('target').AsText),
    NyxTextField(AData.Field('search').AsText));
  Result.FBaseline := NyxToolbarSort(AData.Field('baseline').AsText);
  LFound := False;
  for LMatch := Low(TNyxToolbarSearchMode) to High(TNyxToolbarSearchMode) do
  begin

    if AData.Field('match').AsText = CSearchNames[LMatch] then
    begin
      Result.FMode := LMatch;
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    raise ENyxDataToolbar.Create('Unknown toolbar search mode');
  end;
  LFound := False;
  for LComparison := Low(TNyxQueryTextComparison) to High(TNyxQueryTextComparison) do
  begin

    if AData.Field('comparison').AsText = CComparisonNames[LComparison] then
    begin
      Result.FComparison := LComparison;
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    raise ENyxDataToolbar.Create('Unknown toolbar text comparison');
  end;
  LChoices := AData.Field('sorts');

  if (LChoices.Kind <> ndArray) or (LChoices.Count >= NyxMaximumToolbarSorts) then
  begin
    raise ENyxDataToolbar.Create('Toolbar sort choices must be a bounded array');
  end;
  for LIndex := 0 to LChoices.Count - 1 do
  begin
    LChoice := LChoices.Item(LIndex);

    if (LChoice.Kind <> ndObject) or (LChoice.Count <> 2) then
    begin
      raise ENyxDataToolbar.Create('A sort choice requires exactly name and query');
    end;
    Result := Result.Sort(NyxToolbarSort(LChoice.Field('name').AsText),
      TNyxCollectionQuery.FromData(LChoice.Field('query')));
  end;

  if AData.Count = 8 then
  begin
    Result.FQueryContext := TNyxDataToolbarQueryOptions.FromData(AData.Field('queryContext'));

    if not Result.FQueryContext.Defined then
    begin
      raise ENyxDataToolbar.Create('An explicit queryContext requires defined presentation options');
    end;
  end;
  Result.Validate;
end;

class function TNyxDataToolbarIntent.Disabled: TNyxDataToolbarIntent;
begin
  Result := Default(TNyxDataToolbarIntent);
  Result.FMode := ntiDisabled;
end;

class function TNyxDataToolbarIntent.Configured(const AOptions: TNyxDataToolbarOptions):
  TNyxDataToolbarIntent;
begin
  AOptions.Validate;
  Result := Default(TNyxDataToolbarIntent);
  Result.FMode := ntiConfigured;
  Result.FOptions := AOptions.Copy;
end;

function TNyxDataToolbarIntent.GetOptions: TNyxDataToolbarOptions;
begin

  if FMode <> ntiConfigured then
  begin
    raise ENyxDataToolbar.Create('Only configured toolbar intent has options');
  end;
  Result := FOptions.Copy;
end;

function TNyxDataToolbarIntent.Copy: TNyxDataToolbarIntent;
begin
  Result := Default(TNyxDataToolbarIntent);
  Result.FMode := FMode;

  if FMode = ntiConfigured then
  begin
    Result.FOptions := FOptions.Copy;
  end;
end;

function TNyxDataToolbarIntent.ToData: TNyxDataValue;
begin

  if FMode = ntiDisabled then
  begin
    Exit(NyxObject([NyxField('version', NyxData(1)),
      NyxField('mode', NyxData('disabled'))]));
  end;

  if FMode <> ntiConfigured then
  begin
    raise ENyxDataToolbar.Create('Toolbar inheritance is absence, not serialized intent');
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('mode', NyxData('configured')), NyxField('options', FOptions.ToData)]);
end;

class function TNyxDataToolbarIntent.FromData(const AData: TNyxDataValue):
  TNyxDataToolbarIntent;
begin

  if (AData.Kind <> ndObject) or (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxDataToolbar.Create('Toolbar intent requires a version-one object');
  end;

  if (AData.Field('mode').AsText = 'disabled') and (AData.Count = 2) then
  begin
    Exit(Disabled);
  end;

  if (AData.Field('mode').AsText = 'configured') and (AData.Count = 3) then
  begin
    Exit(Configured(TNyxDataToolbarOptions.FromData(AData.Field('options'))));
  end;
  raise ENyxDataToolbar.Create('Unknown or malformed toolbar intent');
end;

function NyxDataToolbarClearButton(const AToolbar: INyxDataToolbar): INyxButton;
begin

  if (AToolbar = nil) or
    not Supports(AToolbar.Part(NyxPart('clear')), INyxButton, Result) then
  begin
    raise ENyxDataToolbar.Create('Configured toolbar requires its Clear button');
  end;
end;

function HasNyxDataToolbarIntent(ANode: TNyxNode): Boolean;
begin
  Result := (ANode <> nil) and
    ANode.Extensions.Has(NyxExtension(NyxDataToolbarIntentKey));
end;

function GetNyxDataToolbarIntent(ANode: TNyxNode): TNyxDataToolbarIntent;
begin
  Result := Default(TNyxDataToolbarIntent);

  if HasNyxDataToolbarIntent(ANode) then
  begin
    Result := TNyxDataToolbarIntent.FromData(
      ANode.Extensions.Value(NyxExtension(NyxDataToolbarIntentKey)));
  end;
end;

procedure CheckIntentHost(const AControl: INyxControl);
begin

  if (AControl = nil) or AControl.Node.IsRealized or
    not ((AControl.Node.Kind = NyxKindName(nkDataToolbar)) or
      (AControl.Node.Kind = NyxKindName(nkComponent)) or
      (AControl.Node.Kind = NyxKindName(nkSlotOverride))) then
  begin
    raise ENyxDataToolbar.Create('Toolbar intent requires its authored toolbar or reusable descriptor');
  end;
end;

procedure SetNyxDataToolbarIntent(const AToolbar: INyxControl;
  const AIntent: TNyxDataToolbarIntent);
var
  LControl: INyxControl;
  LData: TNyxDataValue;
begin
  LControl := AToolbar;
  LData := AIntent.ToData;
  CheckIntentHost(LControl);
  LControl.Extensions.SetValue(NyxExtension(NyxDataToolbarIntentKey), LData);
end;

procedure InheritNyxDataToolbarIntent(const AToolbar: INyxControl);
var
  LControl: INyxControl;
begin
  LControl := AToolbar;
  CheckIntentHost(LControl);
  LControl.Extensions.Remove(NyxExtension(NyxDataToolbarIntentKey));
end;

procedure ValidateNyxDataToolbarIntent(ANode: TNyxNode);
var
  LIntent: TNyxDataToolbarIntent;
begin

  if not HasNyxDataToolbarIntent(ANode) then
  begin
    Exit;
  end;
  LIntent := GetNyxDataToolbarIntent(ANode);
  LIntent.ToData;

  if not ANode.IsRealized and ((ANode.Kind = NyxKindName(nkComponent)) or
    (ANode.Kind = NyxKindName(nkSlotOverride))) then
  begin
    Exit;
  end;

  if ANode.Kind <> NyxKindName(nkDataToolbar) then
  begin
    raise ENyxDataToolbar.Create('Toolbar intent requires its specialized effective target');
  end;
end;

function NyxDataToolbarSortDomain(AToolbar: TNyxNode;
  const AOptions: TNyxDataToolbarOptions): TNyxValueDomain;
begin
  Result := NyxDataToolbarSortDomain(AToolbar, AOptions, GetNyxDataToolbarIntent(AToolbar));
end;

function ToolbarBaseDomain(AToolbar: TNyxNode;
  const APrevious: TNyxDataToolbarIntent): TNyxValueDomain;
var
  LDomain: TNyxValueDomain;
  LData: TNyxDataValue;
  LPrevious: TNyxDataToolbarIntent;
  LOptions: TNyxDataToolbarOptions;
  LFields: array of TNyxDataField;
  LStamp: TNyxDataValue;
  LBase: TNyxValueDomain;
  LApplied: TNyxValueDomain;
  LSort: TNyxNode;
  LIndex: Integer;
  LCount: Integer;
  LOwned: Boolean;
begin
  if (AToolbar = nil) or (AToolbar.Kind <> NyxKindName(nkDataToolbar)) then
  begin
    raise ENyxDataToolbar.Create('Sort contract requires a specialized toolbar');
  end;
  LSort := AToolbar.Part(NyxPart('sort'));
  LDomain := NyxNodeValueDomain(LSort);

  if not LDomain.Defined or (LDomain.Kind <> nskText) then
  begin
    raise ENyxDataToolbar.Create('Toolbar Sort requires a text value domain');
  end;
  LData := LDomain.ToData;

  if LSort.Extensions.Has(NyxExtension(NyxDataToolbarSortDomainKey)) then
  begin
    LStamp := LSort.Extensions.Value(NyxExtension(NyxDataToolbarSortDomainKey));

    if (LStamp.Kind <> ndObject) or (LStamp.Count <> 3) or
      (LStamp.Field('version').AsInteger <> 1) then
    begin
      raise ENyxDataToolbar.Create('Toolbar sort provenance requires its exact version-one descriptor');
    end;
    LBase := TNyxValueDomain.FromData(LStamp.Field('base'));
    LApplied := TNyxValueDomain.FromData(LStamp.Field('applied'));

    if (LBase.Kind <> nskText) or (LApplied.Kind <> nskText) then
    begin
      raise ENyxDataToolbar.Create('Toolbar sort provenance requires text value contracts');
    end;

    if LApplied.ToData.ToJSON = LData.ToJSON then
    begin
      Exit(LBase);
    end;
    { A caller subsequently changed the contract. Its current complete value
      is authoritative; old toolbar provenance cannot weaken that declaration. }
    Exit(LDomain);
  end;
  LPrevious := APrevious.Copy;
  LOwned := False;

  if LPrevious.Mode = ntiConfigured then
  begin
    LOptions := LPrevious.Options;
    for LIndex := 0 to LData.Count - 1 do
    begin

      if LData.Key(LIndex) = 'choices' then
      begin
        LOwned := (LData.Field('choices').Kind = ndArray) and
          (LData.Field('choices').Count = LOptions.Count + 1);
      end;
    end;

    if LOwned then
    begin
      LOwned := LData.Field('choices').Item(0).AsText = LOptions.BaselineChoice.Name;
      for LIndex := 0 to LOptions.Count - 1 do
      begin
        LOwned := LOwned and (LData.Field('choices').Item(LIndex + 1).AsText =
          LOptions.ChoiceAt(LIndex).Reference.Name);
      end;
    end;
  end;

  if LOwned then
  begin
    { This is the explicit domain descriptor boundary. Remove only the exact
      toolbar-owned choice vector; retain every other declared constraint. }
    SetLength(LFields, LData.Count - 1);
    LCount := 0;
    for LIndex := 0 to LData.Count - 1 do
    begin

      if LData.Key(LIndex) <> 'choices' then
      begin
        LFields[LCount] := NyxField(LData.Key(LIndex), LData.Field(LData.Key(LIndex)));
        Inc(LCount);
      end;
    end;
    LDomain := TNyxValueDomain.FromData(NyxObject(LFields));
  end;
  Result := LDomain;
end;

function NyxDataToolbarSortDomain(AToolbar: TNyxNode;
  const AOptions: TNyxDataToolbarOptions;
  const APrevious: TNyxDataToolbarIntent): TNyxValueDomain;
var
  LDomain: TNyxValueDomain;
  LChoices: array of TNyxText;
  LIndex: Integer;
begin
  AOptions.Validate;
  LDomain := ToolbarBaseDomain(AToolbar, APrevious);
  SetLength(LChoices, AOptions.Count + 1);
  LChoices[0] := AOptions.BaselineChoice.Name;
  for LIndex := 0 to AOptions.Count - 1 do
  begin
    LChoices[LIndex + 1] := AOptions.ChoiceAt(LIndex).Reference.Name;
  end;
  for LIndex := 0 to High(LChoices) do
  begin
    LDomain.Admit(NyxData(LChoices[LIndex]));
  end;
  Result := NyxTextDomain(LDomain).Choices(LChoices).Definition;
end;

procedure SetNyxDataToolbarSortDomain(const ASort: INyxControl;
  AToolbar: TNyxNode; const AOptions: TNyxDataToolbarOptions;
  const APrevious: TNyxDataToolbarIntent);
var
  LBase: TNyxValueDomain;
  LDomain: TNyxValueDomain;
  LStamp: TNyxDataValue;
begin

  if (ASort = nil) or ASort.Node.IsRealized or
    not ((ASort.Node.Kind = NyxKindName(nkSelect)) or
      (ASort.Node.Kind = NyxKindName(nkSlotOverride))) then
  begin
    raise ENyxDataToolbar.Create('Apply the sort contract to its authored selector or descriptor');
  end;
  LBase := ToolbarBaseDomain(AToolbar, APrevious);
  LDomain := NyxDataToolbarSortDomain(AToolbar, AOptions, APrevious);
  LStamp := NyxObject([NyxField('version', NyxData(1)), NyxField('base', LBase.ToData),
    NyxField('applied', LDomain.ToData)]);
  ASort.Contract.Value(LDomain);
  ASort.Extensions.SetValue(NyxExtension(NyxDataToolbarSortDomainKey), LStamp);
end;

function ConfigureNyxDataToolbar(const AToolbar: INyxDataToolbar;
  const AOptions: TNyxDataToolbarOptions): INyxDataToolbar;
var
  LCopy: INyxControl;
  LSearch: INyxInput;
  LSort: INyxSelect;
  LClear: INyxButton;
  LChoices: array of TNyxText;
  LItems: TNyxText;
  LDomain: TNyxValueDomain;
  LIndex: Integer;
  LChild: INyxControl;
  LPrevious: TNyxDataToolbarIntent;
  LStamp: TNyxDataValue;
  LOwnDefault: Boolean;
  LContext: INyxControl;
  LPart: INyxControl;
  LPresentation: TNyxDataToolbarQueryOptions;
  LStatusNode: TNyxNode;
  LDetailsNode: TNyxNode;
  LBinding: TNyxBindingSpec;
  LPrimitive: TNyxPrimitiveInfo;
begin
  AOptions.Validate;

  if (AToolbar = nil) or AToolbar.Node.IsRealized or
    (AToolbar.Node.Kind <> NyxKindName(nkDataToolbar)) then
  begin
    raise ENyxDataToolbar.Create('Configure a detached authored toolbar');
  end;
  LCopy := AToolbar.Clone;

  if not Supports(LCopy, INyxDataToolbar, Result) then
  begin
    raise ENyxDataToolbar.Create('The copied control must retain its toolbar specialization');
  end;
  { All edits belong to the detached result. Any contract/role/domain refusal
    releases it without touching the caller's accepted controls or defaults. }
  LSearch := Result.SearchInput;
  LDomain := NyxNodeValueDomain(LSearch.Node);

  if not LDomain.Defined or (LDomain.Kind <> nskText) then
  begin
    raise ENyxDataToolbar.Create('Toolbar Search requires a text input');
  end;
  LDomain.Admit(NyxData(''));
  LSort := Result.SortSelect;
  LPrevious := GetNyxDataToolbarIntent(Result.Node);
  LOwnDefault := LSort.Node.Props.IndexOfName(NyxAttributeName(atValue)) < 0;

  if LPrevious.Mode = ntiConfigured then
  begin
    LOwnDefault := LOwnDefault or (LSort.Node.Prop(NyxAttributeName(atValue)) =
      LPrevious.Options.BaselineChoice.Name);
  end
  else if LSort.Extensions.Has(NyxExtension(NyxDataToolbarSortDomainKey)) then
  begin
    LStamp := LSort.Extensions.Value(NyxExtension(NyxDataToolbarSortDomainKey));

    if (LStamp.Field('applied').ToJSON = NyxNodeValueDomain(LSort.Node).ToData.ToJSON) then
    begin
      LOwnDefault := LOwnDefault or (LSort.Node.Prop(NyxAttributeName(atValue)) =
        LStamp.Field('applied').Field('choices').Item(0).AsText);
    end;
  end;
  LClear := nil;
  for LIndex := 0 to Result.Count - 1 do
  begin
    LChild := Result.Children[LIndex];

    if LChild.Node.Prop(NyxAttributeName(atPart)) = 'clear' then
    begin

      if (LClear <> nil) or not Supports(LChild, INyxButton, LClear) or
        (LChild.Node.ProjectionKind <> NyxKindName(nkButton)) then
      begin
        raise ENyxDataToolbar.Create('The Clear role is ambiguous or incompatible');
      end;
    end;
  end;

  if LClear = nil then
  begin

    if Result.Node.Find(Result.ID + '-clear') <> nil then
    begin
      raise ENyxDataToolbar.Create('The new Clear identity is already occupied');
    end;
    LClear := NewNyxButton(Result.ID + '-clear').WithText('Clear');
    LClear.Configure.PartName(NyxPart('clear')).AccessibleName('Clear search and sorting')
      .OnClick(NyxSemantic(nseClear)).Done;
    Result.Add(LClear);
  end;
  { A scalar behavior would run before the grouped query command. Refuse that
    competing owner instead of silently changing a custom action or callbacks. }

  if LClear.Node.Prop(NyxAttributeName(atAction), '') <> '' then
  begin
    raise ENyxDataToolbar.Create('Clear must not have a separate scalar action');
  end;
  SetLength(LChoices, AOptions.Count + 1);
  LChoices[0] := AOptions.BaselineChoice.Name;
  LItems := LChoices[0];
  for LIndex := 0 to AOptions.Count - 1 do
  begin
    LChoices[LIndex + 1] := AOptions.ChoiceAt(LIndex).Reference.Name;
    LItems := LItems + #10 + LChoices[LIndex + 1];
  end;
  { Keep the caller's declared domain constraints, then narrow its choices. }
  SetNyxDataToolbarSortDomain(LSort, Result.Node, AOptions, GetNyxDataToolbarIntent(Result.Node));
  LSort.Configure.Items(LItems).Done;

  if LOwnDefault then
  begin
    LSort.Configure.Value(AOptions.BaselineChoice.Name).Done;
  end;
  AOptions.SortIndex(NyxToolbarSort(LSort.Node.Prop(NyxAttributeName(atValue))));
  LPresentation := AOptions.QueryPresentation;

  if LPresentation.Defined then
  begin
    LContext := nil;
    for LIndex := 0 to Result.Count - 1 do
    begin
      LPart := Result.Children[LIndex];

      if LPart.Node.StoredProp(NyxAttributeName(atPart)) = NyxDataToolbarQueryContextPart then
      begin

        if LContext <> nil then
        begin
          raise ENyxDataToolbar.Create('Toolbar query context is ambiguous');
        end;
        LContext := LPart;
      end;
    end;

    if LContext = nil then
    begin

      if (Result.Node.Find(Result.ID + '-query-context') <> nil) or
        (Result.Node.Find(Result.ID + '-query-context-status') <> nil) or
        (Result.Node.Find(Result.ID + '-query-context-details') <> nil) then
      begin
        raise ENyxDataToolbar.Create('Toolbar query context identities are already occupied');
      end;
      LContext := NewNyxDataToolbarQueryContent(Result.ID + '-query-context', LPresentation);
      Result.Add(LContext);
    end;

    if not FindNyxPrimitive(LContext.Node.ProjectionKind, LPrimitive) or not LPrimitive.Container then
    begin
      raise ENyxDataToolbar.Create('Toolbar query context requires a layout container');
    end;
    LContext.Configure.Visible(LPresentation.DisplayMode <> ntqdHidden).Done;

    if LPresentation.DisplayMode <> ntqdHidden then
    begin
      LStatusNode := LContext.Node.Part(NyxPart('status'));

      if (LStatusNode.Kind <> NyxKindName(nkLabel)) or
        (LStatusNode.ProjectionKind <> NyxKindName(nkLabel)) or
        (LStatusNode.Prop(NyxAttributeName(atCompound), 'false') = 'true') or
        (LStatusNode.Prop(NyxAttributeName(atAction), NyxActionName(naNone)) <> NyxActionName(naNone)) or
        LStatusNode.FindBinding(bpText, LBinding) then
      begin
        raise ENyxDataToolbar.Create('Query status requires an independent unbound Label');
      end;
      RetainNyxControl(LStatusNode).Configure.Text(LPresentation.StatusCaption).Done;
    end;
    { A summary needs no invoker. Preserve any existing themed container and
      extra parts, while updating a present button's default caption/visibility. }
    LDetailsNode := nil;
    for LIndex := 0 to LContext.Count - 1 do
    begin

      if LContext.Children[LIndex].Node.StoredProp(NyxAttributeName(atPart)) = 'details' then
      begin

        if LDetailsNode <> nil then
        begin
          raise ENyxDataToolbar.Create('Toolbar query details are ambiguous');
        end;
        LDetailsNode := LContext.Children[LIndex].Node;
      end;
    end;

    if LPresentation.DisplayMode = ntqdDetails then
    begin

      if (LDetailsNode = nil) or (LDetailsNode.Kind <> NyxKindName(nkButton)) or
        (LDetailsNode.ProjectionKind <> NyxKindName(nkButton)) or
        (LDetailsNode.Prop(NyxAttributeName(atCompound), 'false') = 'true') or
        (LDetailsNode.Prop(NyxAttributeName(atAction), NyxActionName(naNone)) <> NyxActionName(naNone)) then
      begin
        raise ENyxDataToolbar.Create('Query details require an independent ordinary Button');
      end;
    end;

    if LDetailsNode <> nil then
    begin
      RetainNyxControl(LDetailsNode).Configure.Text(LPresentation.ButtonCaption).
        Visible(LPresentation.DisplayMode = ntqdDetails).Done;
    end;
  end;
  SetNyxDataToolbarIntent(Result, TNyxDataToolbarIntent.Configured(AOptions));
end;

{$I nyx.data.toolbar.runtime.inc}

end.
