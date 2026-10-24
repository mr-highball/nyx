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
unit nyx.collections.view.types;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.state,
  nyx.data,
  nyx.collections,
  nyx.collections.query,
  nyx.collections.query.policy,
  nyx.collections.selection,
  nyx.typeahead,
  nyx.collections.columns,
  nyx.collections.arrangement,
  nyx.collections.paging;

const
  { Reserved only by design version 3. Earlier designs may retain an opaque
    extension of this name; they must never be silently reinterpreted. }
  NyxCollectionViewWireField = 'collectionView';

type
  { Closed presentation choices stay typed. Application scope shares a runtime
    store; instance scope starts from authored defaults for each reusable owner.
    Selection is runtime item identity and is never persisted as a row index. }
  TNyxCollectionScope = (csApplication, csInstance);
  TNyxCollectionProjection = (cpList, cpTable, cpTree);
  TNyxCollectionCellMode = (cmReadOnly, cmEditable);
  { Display order contains canonical descriptor indexes, including hidden
    columns. Widgets may build visible panes from it but never use an ordinal
    as a field identity or write it into a collection row. }
  TNyxCollectionColumnOrder = array of Integer;

  { Immutable column value, owning its captions/reference text. Exact Kind is
    checked against the collection schema before a view is admitted. }
  TNyxCollectionColumn = record
  private
    FField: TNyxText;
    FKind: TNyxStateKind;
    FTitle: TNyxText;
    FMode: TNyxCollectionCellMode;
    FSizing: TNyxColumnSizing;
    FQueryPolicy: TNyxColumnQueryPolicy;
    FLayoutPolicy: TNyxColumnLayoutPolicy;
    function GetQueryPolicy: TNyxColumnQueryPolicy;
    function GetHasQueryPolicy: Boolean;
    function GetLayoutPolicy: TNyxColumnLayoutPolicy;
    function GetHasLayoutPolicy: Boolean;
    function GetIdentity: TNyxColumnIdentity;
  public
    function Copy: TNyxCollectionColumn;
    { Read the declared scalar family from an admitted item. Missing fields and
      incompatible families reject instead of comparing formatted display text. }
    function Read(const AItem: TNyxCollectionItem): TNyxStateValue;
    property FieldName: TNyxText read FField;
    property Kind: TNyxStateKind read FKind;
    property Identity: TNyxColumnIdentity read GetIdentity;
    property Title: TNyxText read FTitle;
    property Mode: TNyxCollectionCellMode read FMode;
    { Undefined preserves the library's default content allocation and legacy
      wire version. A defined value owns only logical dimensions and bounds. }
    property Sizing: TNyxColumnSizing read FSizing;
    { Resolved library default and authored presence are distinct. Query controls
      affect user commands; they never rewrite the accepted baseline query. }
    property QueryPolicy: TNyxColumnQueryPolicy read GetQueryPolicy;
    property HasQueryPolicy: Boolean read GetHasQueryPolicy;
    { Presence belongs to authoring; the resolved getter supplies the library's
      visible, unpinned defaults without manufacturing a saved choice. }
    property LayoutPolicy: TNyxColumnLayoutPolicy read GetLayoutPolicy;
    property HasLayoutPolicy: Boolean read GetHasLayoutPolicy;
  end;

  { Renderer-independent authored binding. Fluent calls return detached copies,
    including their column records on pas2js. First column labels lists/trees;
    tables display all columns unless an explicit layout choice hides them.
    Parent is an optional text field containing an
    item ID, with empty text denoting a root. Hierarchy admission belongs to the
    live view so invalid edits reject before the store publishes them.
    The default record means no binding, rather than an invalid collection key. }
  TNyxCollectionViewSpec = record
  private
    FDefined: Boolean;
    FKey: TNyxCollectionRef;
    FScope: TNyxCollectionScope;
    FSelectionMode: TNyxSelectionMode;
    FParent: TNyxText;
    FQuery: TNyxCollectionQuery;
    FHasTypeAhead: Boolean;
    FTypeAhead: TNyxTypeAheadOptions;
    FPaging: TNyxCollectionPaging;
    FTableQuery: TNyxTableQueryPolicy;
    FTableLayout: TNyxTableLayoutPolicy;
    FColumns: array of TNyxCollectionColumn;
    function AddColumn(const AName: TNyxText; AKind: TNyxStateKind;
      const ATitle: TNyxText; AMode: TNyxCollectionCellMode): TNyxCollectionViewSpec;
    function GetCount: Integer;
    function GetKey: TNyxCollectionRef;
    function GetQuery: TNyxCollectionQuery;
    function GetTypeAhead: TNyxTypeAheadOptions;
    function GetHasColumnSizing: Boolean;
    function GetHasQueryControls: Boolean;
    function GetHasTableQueryPolicy: Boolean;
    function GetTableQueryPolicy: TNyxTableQueryPolicy;
    function GetHasColumnLayout: Boolean;
    function GetHasTableLayoutPolicy: Boolean;
    function GetHasLayoutControls: Boolean;
    function GetTableLayoutPolicy: TNyxTableLayoutPolicy;
  public
    function Column(const AField: TNyxTextFieldRef; const ATitle: TNyxText;
      AMode: TNyxCollectionCellMode = cmReadOnly): TNyxCollectionViewSpec; overload;
    function Column(const AField: TNyxBooleanFieldRef; const ATitle: TNyxText;
      AMode: TNyxCollectionCellMode = cmReadOnly): TNyxCollectionViewSpec; overload;
    function Column(const AField: TNyxIntegerFieldRef; const ATitle: TNyxText;
      AMode: TNyxCollectionCellMode = cmReadOnly): TNyxCollectionViewSpec; overload;
    function Column(const AField: TNyxNumberFieldRef; const ATitle: TNyxText;
      AMode: TNyxCollectionCellMode = cmReadOnly): TNyxCollectionViewSpec; overload;
    function Scoped(AScope: TNyxCollectionScope): TNyxCollectionViewSpec;
    function Selection(AMode: TNyxSelectionMode): TNyxCollectionViewSpec;
    function Parent(const AField: TNyxTextFieldRef): TNyxCollectionViewSpec;
    { Authored immutable query default. Runtime views copy this value and can
      independently configure another policy without editing the document. }
    function Query(const APolicy: TNyxCollectionQuery): TNyxCollectionViewSpec;
    { Authored list/tree search choice. Returns an independent binding, keeping
      columns/query/selection/scope exact. Undefined or invalid options refuse
      before changing Self. Runtime mounts copy this policy into separate engines. }
    function TypeAhead(const APolicy: TNyxTypeAheadOptions): TNyxCollectionViewSpec;
    { Remove the explicit saved choice and use the library's enabled, folded,
      one-second default. This restores the original version 1/2/3 descriptor. }
    function UseDefaultTypeAhead: TNyxCollectionViewSpec;
    { Configure the immediately preceding Column in a readable fluent block.
      Copies every column before mutation. Missing columns/undefined policies
      refuse; the previous binding remains unchanged. Applies to table sizing. }
    function ColumnSizing(const APolicy: TNyxColumnSizing): TNyxCollectionViewSpec;
    { Clear only the immediately preceding column's explicit choice. Missing
      columns refuse; other sizing/query/selection policies remain independent. }
    function UseDefaultColumnSizing: TNyxCollectionViewSpec;
    { Configure the preceding column's user filter/sort affordances. Typed
      operators are validated against that column before returning a copy. }
    function ColumnQuery(const APolicy: TNyxColumnQueryPolicy): TNyxCollectionViewSpec;
    function UseDefaultColumnQuery: TNyxCollectionViewSpec;
    { Tables resolve absent choices to enabled scalar filtering/sorting. These
      saved choices are independent of runtime owner contributions and drafts. }
    function TableQuery(const APolicy: TNyxTableQueryPolicy): TNyxCollectionViewSpec;
    function UseDefaultTableQuery: TNyxCollectionViewSpec;
    { Configure the preceding column's authored visibility, logical pin and
      permitted user actions. These immutable defaults do not contain a live
      table's width/order preferences or its active cell editor. }
    function ColumnLayout(const APolicy: TNyxColumnLayoutPolicy): TNyxCollectionViewSpec;
    function UseDefaultColumnLayout: TNyxCollectionViewSpec;
    { Explicit layout choices opt a bound table into column management. Removing
      this choice preserves per-column choices; removing all choices restores
      the legacy descriptor/geometry rather than persisting runtime preferences. }
    function TableLayout(const APolicy: TNyxTableLayoutPolicy): TNyxCollectionViewSpec;
    function UseDefaultTableLayout: TNyxCollectionViewSpec;
    { Save independent page defaults; runtime page changes never rewrite this
      authored value. Tables/lists consume it after their query. Trees refuse
      during projection admission until hierarchy-aware paging is supported. }
    function Paged(const APolicy: TNyxCollectionPaging): TNyxCollectionViewSpec;
    function UseDefaultPaging: TNyxCollectionViewSpec;
    function ColumnAt(AIndex: Integer): TNyxCollectionColumn;
    function ColumnIndex(const AField: TNyxColumnIdentity): Integer;
    function ResolveColumnOrder(const APreferences: TNyxColumnPreferences): TNyxCollectionColumnOrder;
    function Copy: TNyxCollectionViewSpec;
    procedure Validate;
    { Explicit versioned descriptor boundary, used by codec/extensions. Unknown
      choices, wrong scalar types, duplicate fields and extra members reject.
      Null represents an absent binding. No target or runtime handle is retained. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxCollectionViewSpec; static;
    property Defined: Boolean read FDefined;
    property Key: TNyxCollectionRef read GetKey;
    property Scope: TNyxCollectionScope read FScope;
    property SelectionMode: TNyxSelectionMode read FSelectionMode;
    property ParentField: TNyxText read FParent;
    property QueryPolicy: TNyxCollectionQuery read GetQuery;
    { Distinguish an explicit saved choice from the library default. The policy
      getter returns a copied default when absent, including an unbound record;
      attaching a choice still requires a defined, valid collection binding. }
    property HasTypeAhead: Boolean read FHasTypeAhead;
    property TypeAheadPolicy: TNyxTypeAheadOptions read GetTypeAhead;
    property Count: Integer read GetCount;
    { True when any column carries an explicit policy, selecting descriptor v5. }
    property HasColumnSizing: Boolean read GetHasColumnSizing;
    property PagingPolicy: TNyxCollectionPaging read FPaging;
    property HasQueryControls: Boolean read GetHasQueryControls;
    property HasTableQueryPolicy: Boolean read GetHasTableQueryPolicy;
    property TableQueryPolicy: TNyxTableQueryPolicy read GetTableQueryPolicy;
    property HasColumnLayout: Boolean read GetHasColumnLayout;
    property HasTableLayoutPolicy: Boolean read GetHasTableLayoutPolicy;
    property HasLayoutControls: Boolean read GetHasLayoutControls;
    property TableLayoutPolicy: TNyxTableLayoutPolicy read GetTableLayoutPolicy;
  end;

function NyxCollectionView(const AKey: TNyxCollectionRef): TNyxCollectionViewSpec;

implementation

function TNyxCollectionViewSpec.ColumnIndex(const AField: TNyxColumnIdentity): Integer;
var
  LIndex: Integer;
begin
  AField.Validate;
  for LIndex := 0 to Count - 1 do
  begin

    if (FColumns[LIndex].FieldName = AField.Name) and
      (FColumns[LIndex].Kind = AField.Kind) then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TNyxCollectionViewSpec.ResolveColumnOrder(
  const APreferences: TNyxColumnPreferences): TNyxCollectionColumnOrder;
var
  LUsed: array of Boolean;
  LIndex, LColumn, LCount: Integer;
begin
  Result := nil;
  Validate;
  APreferences.Validate;
  SetLength(Result, Count);
  SetLength(LUsed, Count);
  LCount := 0;
  for LIndex := 0 to APreferences.OrderCount - 1 do
  begin
    LColumn := ColumnIndex(APreferences.OrderAt(LIndex));

    if LColumn < 0 then
    begin
      raise ENyxCollection.Create('Column order contains a missing or changed-family field');
    end;
    Result[LCount] := LColumn;
    LUsed[LColumn] := True;
    Inc(LCount);
  end;
  for LIndex := 0 to Count - 1 do
  begin

    if not LUsed[LIndex] then
    begin
      Result[LCount] := LIndex;
      Inc(LCount);
    end;
  end;
end;

function TNyxCollectionColumn.Copy: TNyxCollectionColumn;
begin
  Result := Default(TNyxCollectionColumn);
  Result.FField := FField;
  Result.FKind := FKind;
  Result.FTitle := FTitle;
  Result.FMode := FMode;
  Result.FSizing := FSizing;
  Result.FQueryPolicy := FQueryPolicy;
  Result.FLayoutPolicy := FLayoutPolicy.Copy;
end;

function TNyxCollectionColumn.GetIdentity: TNyxColumnIdentity;
begin
  { A display ordinal is never a field reference. Hosts retain this exact
    name/family pair across column reorder, hidden fields and editor transfer. }

  if (Ord(FKind) < Ord(nskText)) or (Ord(FKind) > Ord(nskNumber)) then
  begin
    raise ENyxCollection.Create('Collection column has no scalar identity');
  end;
  case FKind of
    nskText: Result := NyxColumnIdentity(NyxTextField(FField));
    nskBoolean: Result := NyxColumnIdentity(NyxBooleanField(FField));
    nskInteger: Result := NyxColumnIdentity(NyxIntegerField(FField));
    nskNumber: Result := NyxColumnIdentity(NyxNumberField(FField));
  end;
end;

function TNyxCollectionColumn.GetQueryPolicy: TNyxColumnQueryPolicy;
begin
  Result := NyxColumnQuery;

  if FQueryPolicy.Defined then
  begin
    Result := FQueryPolicy;
  end;
end;

function TNyxCollectionColumn.GetHasQueryPolicy: Boolean;
begin
  Result := FQueryPolicy.Defined;
end;

function TNyxCollectionColumn.GetLayoutPolicy: TNyxColumnLayoutPolicy;
begin
  Result := NyxColumnLayout;

  if FLayoutPolicy.Defined then
  begin
    Result := FLayoutPolicy.Copy;
  end;
end;

function TNyxCollectionColumn.GetHasLayoutPolicy: Boolean;
begin
  Result := FLayoutPolicy.Defined;
end;

function TNyxCollectionViewSpec.GetTableLayoutPolicy: TNyxTableLayoutPolicy;
begin
  Result := NyxTableLayout;

  if FTableLayout.Defined then
  begin
    Result := FTableLayout.Copy;
  end;
end;

function TNyxCollectionViewSpec.GetHasTableLayoutPolicy: Boolean;
begin
  Result := FTableLayout.Defined;
end;

function TNyxCollectionViewSpec.GetHasColumnLayout: Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to Count - 1 do
  begin

    if FColumns[LIndex].HasLayoutPolicy then
    begin
      Exit(True);
    end;
  end;
end;

function TNyxCollectionViewSpec.GetHasLayoutControls: Boolean;
begin
  Result := FTableLayout.Defined or GetHasColumnLayout;
end;

function TNyxCollectionViewSpec.ColumnLayout(
  const APolicy: TNyxColumnLayoutPolicy): TNyxCollectionViewSpec;
begin

  if not FDefined or (Count = 0) then
  begin
    raise ENyxCollection.Create('Declare a column before configuring its layout');
  end;
  APolicy.Validate;
  Result := Copy;
  Result.FColumns[Count - 1].FLayoutPolicy := APolicy.Copy;
end;

function TNyxCollectionViewSpec.UseDefaultColumnLayout: TNyxCollectionViewSpec;
begin

  if not FDefined or (Count = 0) then
  begin
    raise ENyxCollection.Create('Declare a column before restoring its layout');
  end;
  Result := Copy;
  Result.FColumns[Count - 1].FLayoutPolicy := Default(TNyxColumnLayoutPolicy);
end;

function TNyxCollectionViewSpec.TableLayout(
  const APolicy: TNyxTableLayoutPolicy): TNyxCollectionViewSpec;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before configuring its layout');
  end;
  APolicy.Validate;
  Result := Copy;
  Result.FTableLayout := APolicy.Copy;
end;

function TNyxCollectionViewSpec.UseDefaultTableLayout: TNyxCollectionViewSpec;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before restoring its layout');
  end;
  Result := Copy;
  Result.FTableLayout := Default(TNyxTableLayoutPolicy);
end;

function TNyxCollectionViewSpec.GetTableQueryPolicy: TNyxTableQueryPolicy;
begin
  Result := NyxTableQuery;

  if FTableQuery.Defined then
  begin
    Result := FTableQuery;
  end;
end;

function TNyxCollectionViewSpec.GetHasTableQueryPolicy: Boolean;
begin
  Result := FTableQuery.Defined;
end;

function TNyxCollectionViewSpec.GetHasQueryControls: Boolean;
var
  LIndex: Integer;
begin
  Result := FTableQuery.Defined;
  for LIndex := 0 to Count - 1 do
  begin

    if FColumns[LIndex].HasQueryPolicy then
    begin
      Exit(True);
    end;
  end;
end;

function TNyxCollectionViewSpec.ColumnQuery(
  const APolicy: TNyxColumnQueryPolicy): TNyxCollectionViewSpec;
begin

  if not FDefined or (Count = 0) then
  begin
    raise ENyxCollection.Create('Declare a column before configuring its query controls');
  end;
  APolicy.Validate(FColumns[Count - 1].Kind);
  Result := Copy;
  Result.FColumns[Count - 1].FQueryPolicy := APolicy;
end;

function TNyxCollectionViewSpec.UseDefaultColumnQuery: TNyxCollectionViewSpec;
begin

  if not FDefined or (Count = 0) then
  begin
    raise ENyxCollection.Create('Declare a column before restoring its query controls');
  end;
  Result := Copy;
  Result.FColumns[Count - 1].FQueryPolicy := Default(TNyxColumnQueryPolicy);
end;

function TNyxCollectionViewSpec.TableQuery(
  const APolicy: TNyxTableQueryPolicy): TNyxCollectionViewSpec;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before configuring query controls');
  end;
  APolicy.Validate;
  Result := Copy;
  Result.FTableQuery := APolicy;
end;

function TNyxCollectionViewSpec.UseDefaultTableQuery: TNyxCollectionViewSpec;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before restoring query controls');
  end;
  Result := Copy;
  Result.FTableQuery := Default(TNyxTableQueryPolicy);
end;

function TNyxCollectionViewSpec.GetHasColumnSizing: Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to Count - 1 do
  begin

    if FColumns[LIndex].Sizing.Defined then
    begin
      Exit(True);
    end;
  end;
end;

function TNyxCollectionViewSpec.ColumnSizing(const APolicy: TNyxColumnSizing): TNyxCollectionViewSpec;
begin

  if not FDefined or (Count = 0) then
  begin
    raise ENyxCollection.Create('Declare a collection column before configuring its sizing');
  end;
  APolicy.Validate;
  Result := Copy;
  Result.FColumns[Count - 1].FSizing := APolicy;
end;

function TNyxCollectionViewSpec.UseDefaultColumnSizing: TNyxCollectionViewSpec;
begin

  if not FDefined or (Count = 0) then
  begin
    raise ENyxCollection.Create('Declare a collection column before restoring default sizing');
  end;
  Result := Copy;
  Result.FColumns[Count - 1].FSizing := Default(TNyxColumnSizing);
end;

function TNyxCollectionViewSpec.Paged(const APolicy: TNyxCollectionPaging): TNyxCollectionViewSpec;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before configuring paging');
  end;
  APolicy.Validate;
  Result := Copy;
  Result.FPaging := APolicy;
end;

function TNyxCollectionViewSpec.UseDefaultPaging: TNyxCollectionViewSpec;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before restoring default paging');
  end;
  Result := Copy;
  Result.FPaging := Default(TNyxCollectionPaging);
end;

function TNyxCollectionColumn.Read(const AItem: TNyxCollectionItem): TNyxStateValue;
begin
  { Revalidate stored ordinals at the typed projection boundary. }
  case Ord(FKind) of
    Ord(nskText):
      begin
        Result := TNyxStateValue.FromText(AItem.GetValue(NyxTextField(FField)));
      end;
    Ord(nskBoolean):
      begin
        Result := TNyxStateValue.FromBoolean(AItem.GetValue(NyxBooleanField(FField)));
      end;
    Ord(nskInteger):
      begin
        Result := TNyxStateValue.FromInteger(AItem.GetValue(NyxIntegerField(FField)));
      end;
    Ord(nskNumber):
      begin
        Result := TNyxStateValue.FromNumber(AItem.GetValue(NyxNumberField(FField)));
      end;
    else
      begin
        raise ENyxCollection.Create('Unknown collection column scalar family');
      end;
  end;
end;

function NyxCollectionView(const AKey: TNyxCollectionRef): TNyxCollectionViewSpec;
begin
  Result := Default(TNyxCollectionViewSpec);
  { Read Name to refuse an uninitialized reference before publishing a builder. }
  Result.FKey := NyxCollection(AKey.Name);
  Result.FDefined := True;
  Result.FScope := csApplication;
  Result.FSelectionMode := nsmSingle;
  Result.FParent := '';
  Result.FQuery := NyxCollectionQuery;
  Result.FHasTypeAhead := False;
  Result.FTypeAhead := Default(TNyxTypeAheadOptions);
  SetLength(Result.FColumns, 0);
end;

function TNyxCollectionViewSpec.GetKey: TNyxCollectionRef;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('An absent collection view has no key');
  end;
  Result := NyxCollection(FKey.Name);
end;

function TNyxCollectionViewSpec.GetCount: Integer;
begin
  Result := Length(FColumns);
end;

function TNyxCollectionViewSpec.Copy: TNyxCollectionViewSpec;
var
  LIndex: Integer;
begin
  Result.FDefined := FDefined;
  Result.FKey := FKey;
  Result.FScope := FScope;
  Result.FSelectionMode := FSelectionMode;
  Result.FParent := FParent;
  Result.FQuery := FQuery.Copy;
  Result.FHasTypeAhead := FHasTypeAhead;
  Result.FTypeAhead := FTypeAhead;
  Result.FPaging := FPaging;
  Result.FTableQuery := FTableQuery;
  Result.FTableLayout := FTableLayout.Copy;
  SetLength(Result.FColumns, Count);
  for LIndex := 0 to Count - 1 do
  begin
    Result.FColumns[LIndex] := FColumns[LIndex].Copy;
  end;
end;

function TNyxCollectionViewSpec.GetQuery: TNyxCollectionQuery;
begin
  Result := FQuery.Copy;
end;

function TNyxCollectionViewSpec.GetTypeAhead: TNyxTypeAheadOptions;
begin
  Result := NyxTypeAhead;

  if FHasTypeAhead then
  begin
    Result := FTypeAhead;
  end;
end;

function TNyxCollectionViewSpec.TypeAhead(
  const APolicy: TNyxTypeAheadOptions): TNyxCollectionViewSpec;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before choosing typeahead');
  end;
  APolicy.Validate;
  Result := Copy;
  Result.FTypeAhead := APolicy;
  Result.FHasTypeAhead := True;
end;

function TNyxCollectionViewSpec.UseDefaultTypeAhead: TNyxCollectionViewSpec;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before restoring default typeahead');
  end;
  Result := Copy;
  Result.FHasTypeAhead := False;
  Result.FTypeAhead := Default(TNyxTypeAheadOptions);
end;

function TNyxCollectionViewSpec.Query(const APolicy: TNyxCollectionQuery): TNyxCollectionViewSpec;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before choosing a query');
  end;
  Result := Copy;
  Result.FQuery := TNyxCollectionQuery.FromData(APolicy.ToData);
end;

function TNyxCollectionViewSpec.AddColumn(const AName: TNyxText;
  AKind: TNyxStateKind; const ATitle: TNyxText;
  AMode: TNyxCollectionCellMode): TNyxCollectionViewSpec;
var
  LIndex: Integer;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before adding columns');
  end;

  if Count >= NyxMaximumCollectionFields then
  begin
    raise ENyxCollection.Create('Collection views support at most 64 columns');
  end;
  for LIndex := 0 to Count - 1 do
  begin

    if FColumns[LIndex].FieldName = AName then
    begin
      raise ENyxCollection.Create('A collection view column is already defined: ' + AName);
    end;
  end;
  Result := Copy;
  SetLength(Result.FColumns, Count + 1);
  Result.FColumns[Count].FField := AName;
  Result.FColumns[Count].FKind := AKind;
  Result.FColumns[Count].FTitle := ATitle;
  Result.FColumns[Count].FMode := AMode;
end;

function TNyxCollectionViewSpec.Column(const AField: TNyxTextFieldRef;
  const ATitle: TNyxText; AMode: TNyxCollectionCellMode): TNyxCollectionViewSpec;
begin
  Result := AddColumn(AField.Name, nskText, ATitle, AMode);
end;

function TNyxCollectionViewSpec.Column(const AField: TNyxBooleanFieldRef;
  const ATitle: TNyxText; AMode: TNyxCollectionCellMode): TNyxCollectionViewSpec;
begin
  Result := AddColumn(AField.Name, nskBoolean, ATitle, AMode);
end;

function TNyxCollectionViewSpec.Column(const AField: TNyxIntegerFieldRef;
  const ATitle: TNyxText; AMode: TNyxCollectionCellMode): TNyxCollectionViewSpec;
begin
  Result := AddColumn(AField.Name, nskInteger, ATitle, AMode);
end;

function TNyxCollectionViewSpec.Column(const AField: TNyxNumberFieldRef;
  const ATitle: TNyxText; AMode: TNyxCollectionCellMode): TNyxCollectionViewSpec;
begin
  Result := AddColumn(AField.Name, nskNumber, ATitle, AMode);
end;

function TNyxCollectionViewSpec.Scoped(AScope: TNyxCollectionScope): TNyxCollectionViewSpec;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before choosing its scope');
  end;
  Result := Copy;
  Result.FScope := AScope;
end;

function TNyxCollectionViewSpec.Selection(AMode: TNyxSelectionMode): TNyxCollectionViewSpec;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before choosing selection');
  end;
  Result := Copy;
  Result.FSelectionMode := AMode;
end;

function TNyxCollectionViewSpec.Parent(const AField: TNyxTextFieldRef): TNyxCollectionViewSpec;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Create a collection view before choosing its parent field');
  end;
  Result := Copy;
  Result.FParent := AField.Name;
end;

function TNyxCollectionViewSpec.ColumnAt(AIndex: Integer): TNyxCollectionColumn;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxCollection.Create('Collection view column index is out of range');
  end;
  Result := FColumns[AIndex].Copy;
end;

procedure TNyxCollectionViewSpec.Validate;
var
  LIndex: Integer;
  LOther: Integer;
  LReference: TNyxCollectionRef;
  LField: TNyxTextFieldRef;
begin

  if not FDefined then
  begin

    if (Count <> 0) or (FParent <> '') or FQuery.Defined or FHasTypeAhead or
      FPaging.Defined or FTableQuery.Defined or FTableLayout.Defined then
    begin
      raise ENyxCollection.Create('Absent collection view contains a descriptor');
    end;
    Exit;
  end;
  LReference := NyxCollection(FKey.Name);

  if FTableLayout.Defined then
  begin
    FTableLayout.Validate;
  end;

  if FTableQuery.Defined then
  begin
    FTableQuery.Validate;
  end;

  if FPaging.Defined then
  begin
    FPaging.Validate;
  end;

  if FHasTypeAhead then
  begin
    FTypeAhead.Validate;
  end;

  if (Ord(FSelectionMode) < Ord(Low(TNyxSelectionMode))) or
    (Ord(FSelectionMode) > Ord(High(TNyxSelectionMode))) then
  begin
    raise ENyxCollection.Create('Unknown collection selection mode');
  end;

  if (Count < 1) or (Count > NyxMaximumCollectionFields) then
  begin
    raise ENyxCollection.Create('A collection view requires 1..64 columns');
  end;

  if (Ord(FScope) < Ord(Low(TNyxCollectionScope))) or
    (Ord(FScope) > Ord(High(TNyxCollectionScope))) then
  begin
    raise ENyxCollection.Create('Unknown collection view scope');
  end;

  if FParent <> '' then
  begin
    LField := NyxTextField(FParent);
  end;
  for LIndex := 0 to Count - 1 do
  begin
    LField := NyxTextField(FColumns[LIndex].FieldName);

    if FColumns[LIndex].HasLayoutPolicy then
    begin
      FColumns[LIndex].LayoutPolicy.Validate;
    end;

    if FColumns[LIndex].Sizing.Defined then
    begin
      FColumns[LIndex].Sizing.Validate;
    end;

    if (Ord(FColumns[LIndex].Kind) < Ord(Low(TNyxStateKind))) or
      (Ord(FColumns[LIndex].Kind) > Ord(High(TNyxStateKind))) or
      (Ord(FColumns[LIndex].Mode) < Ord(Low(TNyxCollectionCellMode))) or
      (Ord(FColumns[LIndex].Mode) > Ord(High(TNyxCollectionCellMode))) then
    begin
      raise ENyxCollection.Create('Unknown collection view column choice');
    end;

    if FColumns[LIndex].HasQueryPolicy then
    begin
      FColumns[LIndex].QueryPolicy.Validate(FColumns[LIndex].Kind);
    end;
    for LOther := 0 to LIndex - 1 do
    begin

      if FColumns[LOther].FieldName = FColumns[LIndex].FieldName then
      begin
        raise ENyxCollection.Create('Duplicate collection view column');
      end;
    end;
  end;
end;

function TNyxCollectionViewSpec.ToData: TNyxDataValue;
var
  LColumns: array of TNyxDataValue;
  LIndex: Integer;
  LScope: TNyxText;
  LSelection: TNyxText;
  LTypeAhead: TNyxDataValue;
begin
  Validate;

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  LScope := 'application';

  if FScope = csInstance then
  begin
    LScope := 'instance';
  end;
  SetLength(LColumns, Count);
  for LIndex := 0 to Count - 1 do
  begin
    LColumns[LIndex] := NyxObject([
      NyxField('field', NyxData(FColumns[LIndex].FieldName)),
      NyxField('kind', NyxData(NyxStateKindName(FColumns[LIndex].Kind))),
      NyxField('title', NyxData(FColumns[LIndex].Title)),
      NyxField('editable', NyxData(FColumns[LIndex].Mode = cmEditable))]);

    if FColumns[LIndex].Sizing.Defined then
    begin
      LColumns[LIndex] := NyxObject([
        NyxField('field', NyxData(FColumns[LIndex].FieldName)),
        NyxField('kind', NyxData(NyxStateKindName(FColumns[LIndex].Kind))),
        NyxField('title', NyxData(FColumns[LIndex].Title)),
        NyxField('editable', NyxData(FColumns[LIndex].Mode = cmEditable)),
        NyxField('sizing', FColumns[LIndex].Sizing.ToData)]);
    end;

    if HasQueryControls then
    begin
      { Version seven has a closed complete column shape. Null independently
        preserves each absent sizing/query choice; versions one through six
        retain their exact former descriptors when no query choice is saved. }
      LColumns[LIndex] := NyxObject([
        NyxField('field', NyxData(FColumns[LIndex].FieldName)),
        NyxField('kind', NyxData(NyxStateKindName(FColumns[LIndex].Kind))),
        NyxField('title', NyxData(FColumns[LIndex].Title)),
        NyxField('editable', NyxData(FColumns[LIndex].Mode = cmEditable)),
        NyxField('sizing', FColumns[LIndex].Sizing.ToData),
        NyxField('queryControls', FColumns[LIndex].FQueryPolicy.ToData)]);
    end;

    if HasLayoutControls then
    begin
      { Version eight owns one complete shape. Each null retains absence;
        runtime preferences never enter the authored binding descriptor. }
      LColumns[LIndex] := NyxObject([
        NyxField('field', NyxData(FColumns[LIndex].FieldName)),
        NyxField('kind', NyxData(NyxStateKindName(FColumns[LIndex].Kind))),
        NyxField('title', NyxData(FColumns[LIndex].Title)),
        NyxField('editable', NyxData(FColumns[LIndex].Mode = cmEditable)),
        NyxField('sizing', FColumns[LIndex].Sizing.ToData),
        NyxField('queryControls', FColumns[LIndex].FQueryPolicy.ToData),
        NyxField('layoutControls', FColumns[LIndex].FLayoutPolicy.ToData)]);
    end;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('key', NyxData(FKey.Name)), NyxField('scope', NyxData(LScope)),
    NyxField('parent', NyxData(FParent)), NyxField('columns', NyxArray(LColumns))]);
  { Keep the canonical version-1 single descriptor byte-compatible. Multiple
    uses an explicit version-2 field; old opaque data is not reinterpreted. }

  if FSelectionMode = nsmMultiple then
  begin
    Result := NyxObject([NyxField('version', NyxData(2)),
      NyxField('key', NyxData(FKey.Name)), NyxField('scope', NyxData(LScope)),
      NyxField('parent', NyxData(FParent)), NyxField('columns', NyxArray(LColumns)),
      NyxField('selection', NyxData('multiple'))]);
  end;

  if FQuery.Defined then
  begin
    LSelection := 'single';

    if FSelectionMode = nsmMultiple then
    begin
      LSelection := 'multiple';
    end;
    Result := NyxObject([NyxField('version', NyxData(3)),
      NyxField('key', NyxData(FKey.Name)), NyxField('scope', NyxData(LScope)),
      NyxField('parent', NyxData(FParent)), NyxField('columns', NyxArray(LColumns)),
      NyxField('selection', NyxData(LSelection)), NyxField('query', FQuery.ToData)]);
  end;

  if FHasTypeAhead then
  begin
    { Version four is explicit even for a default-valued authored choice.
      Earlier descriptors remain byte-compatible when no policy is declared.
      Query is present and may be null, preserving one exact complete shape. }
    LSelection := 'single';

    if FSelectionMode = nsmMultiple then
    begin
      LSelection := 'multiple';
    end;
    Result := NyxObject([NyxField('version', NyxData(4)),
      NyxField('key', NyxData(FKey.Name)), NyxField('scope', NyxData(LScope)),
      NyxField('parent', NyxData(FParent)), NyxField('columns', NyxArray(LColumns)),
      NyxField('selection', NyxData(LSelection)), NyxField('query', FQuery.ToData),
      NyxField('typeAhead', FTypeAhead.ToData)]);
  end;

  if HasColumnSizing then
  begin
    LSelection := 'single';

    if FSelectionMode = nsmMultiple then
    begin
      LSelection := 'multiple';
    end;
    LTypeAhead := NyxNull;

    if FHasTypeAhead then
    begin
      LTypeAhead := FTypeAhead.ToData;
    end;
    Result := NyxObject([NyxField('version', NyxData(5)),
      NyxField('key', NyxData(FKey.Name)), NyxField('scope', NyxData(LScope)),
      NyxField('parent', NyxData(FParent)), NyxField('columns', NyxArray(LColumns)),
      NyxField('selection', NyxData(LSelection)), NyxField('query', FQuery.ToData),
      NyxField('typeAhead', LTypeAhead)]);
  end;

  if FPaging.Defined then
  begin
    LSelection := 'single';

    if FSelectionMode = nsmMultiple then
    begin
      LSelection := 'multiple';
    end;
    LTypeAhead := NyxNull;

    if FHasTypeAhead then
    begin
      LTypeAhead := FTypeAhead.ToData;
    end;
    Result := NyxObject([NyxField('version', NyxData(6)),
      NyxField('key', NyxData(FKey.Name)), NyxField('scope', NyxData(LScope)),
      NyxField('parent', NyxData(FParent)), NyxField('columns', NyxArray(LColumns)),
      NyxField('selection', NyxData(LSelection)), NyxField('query', FQuery.ToData),
      NyxField('typeAhead', LTypeAhead), NyxField('paging', FPaging.ToData)]);
  end;

  if HasQueryControls then
  begin
    LSelection := 'single';

    if FSelectionMode = nsmMultiple then
    begin
      LSelection := 'multiple';
    end;
    LTypeAhead := NyxNull;

    if FHasTypeAhead then
    begin
      LTypeAhead := FTypeAhead.ToData;
    end;
    Result := NyxObject([NyxField('version', NyxData(7)),
      NyxField('key', NyxData(FKey.Name)), NyxField('scope', NyxData(LScope)),
      NyxField('parent', NyxData(FParent)), NyxField('columns', NyxArray(LColumns)),
      NyxField('selection', NyxData(LSelection)), NyxField('query', FQuery.ToData),
      NyxField('typeAhead', LTypeAhead), NyxField('paging', FPaging.ToData),
      NyxField('queryControls', FTableQuery.ToData)]);
  end;

  if HasLayoutControls then
  begin
    LSelection := 'single';

    if FSelectionMode = nsmMultiple then
    begin
      LSelection := 'multiple';
    end;
    LTypeAhead := NyxNull;

    if FHasTypeAhead then
    begin
      LTypeAhead := FTypeAhead.ToData;
    end;
    Result := NyxObject([NyxField('version', NyxData(8)),
      NyxField('key', NyxData(FKey.Name)), NyxField('scope', NyxData(LScope)),
      NyxField('parent', NyxData(FParent)), NyxField('columns', NyxArray(LColumns)),
      NyxField('selection', NyxData(LSelection)), NyxField('query', FQuery.ToData),
      NyxField('typeAhead', LTypeAhead), NyxField('paging', FPaging.ToData),
      NyxField('queryControls', FTableQuery.ToData),
      NyxField('layoutControls', FTableLayout.ToData)]);
  end;
end;

class function TNyxCollectionViewSpec.FromData(
  const AData: TNyxDataValue): TNyxCollectionViewSpec;
var
  LColumns: TNyxDataValue;
  LColumn: TNyxDataValue;
  LKind: TNyxText;
  LName: TNyxText;
  LTitle: TNyxText;
  LMode: TNyxCollectionCellMode;
  LIndex: Integer;
  LVersion: Integer;
  LFieldIndex: Integer;
  LPolicy: TNyxColumnSizing;
begin
  Result := Default(TNyxCollectionViewSpec);
  AData.Validate;

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if (AData.Kind <> ndObject) or
    not (((AData.Count = 5) and (AData.Field('version').AsInteger = 1)) or
      ((AData.Count = 6) and (AData.Field('version').AsInteger = 2)) or
      ((AData.Count = 7) and (AData.Field('version').AsInteger = 3)) or
      ((AData.Count = 8) and (AData.Field('version').AsInteger in [4, 5])) or
      ((AData.Count = 9) and (AData.Field('version').AsInteger = 6)) or
      ((AData.Count = 10) and (AData.Field('version').AsInteger = 7)) or
      ((AData.Count = 11) and (AData.Field('version').AsInteger = 8))) then
  begin
    raise ENyxCollection.Create('Unsupported collection view descriptor');
  end;
  Result := NyxCollectionView(NyxCollection(AData.Field('key').AsText));
  LVersion := AData.Field('version').AsInteger;

  if LVersion in [5, 6, 7] then
  begin
    for LFieldIndex := 0 to AData.Count - 1 do
    begin

      if Pos('|' + AData.Key(LFieldIndex) + '|',
        '|version|key|scope|parent|columns|selection|query|typeAhead|paging|queryControls|') = 0 then
      begin
        raise ENyxCollection.Create('Unknown sized collection view field');
      end;
    end;
  end;

  if LVersion = 8 then
  begin
    for LFieldIndex := 0 to AData.Count - 1 do
    begin

      if Pos('|' + AData.Key(LFieldIndex) + '|',
        '|version|key|scope|parent|columns|selection|query|typeAhead|paging|queryControls|layoutControls|') = 0 then
      begin
        raise ENyxCollection.Create('Unknown arranged collection view field');
      end;
    end;
  end;

  if LVersion in [3, 4, 5, 6, 7, 8] then
  begin
    Result := Result.Query(TNyxCollectionQuery.FromData(AData.Field('query')));

    if (AData.Field('version').AsInteger = 3) and not Result.QueryPolicy.Defined then
    begin
      raise ENyxCollection.Create('Version-3 collection views require a nonempty query');
    end;

    if AData.Field('selection').AsText = 'multiple' then
    begin
      Result := Result.Selection(nsmMultiple);
    end
    else if AData.Field('selection').AsText <> 'single' then
    begin
      raise ENyxCollection.Create('Unknown collection selection mode');
    end;
  end;

  if AData.Field('version').AsInteger = 4 then
  begin
    Result := Result.TypeAhead(TNyxTypeAheadOptions.FromData(AData.Field('typeAhead')));
  end;

  if (LVersion in [5, 6, 7, 8]) and (AData.Field('typeAhead').Kind <> ndNull) then
  begin
    Result := Result.TypeAhead(TNyxTypeAheadOptions.FromData(AData.Field('typeAhead')));
  end;

  if AData.Field('version').AsInteger = 2 then
  begin

    if AData.Field('selection').AsText <> 'multiple' then
    begin
      raise ENyxCollection.Create('Unknown version-2 collection selection mode');
    end;
    Result := Result.Selection(nsmMultiple);
  end;

  if AData.Field('scope').AsText = 'instance' then
  begin
    Result := Result.Scoped(csInstance);
  end
  else if AData.Field('scope').AsText <> 'application' then
  begin
    raise ENyxCollection.Create('Unknown collection view scope');
  end;
  LName := AData.Field('parent').AsText;

  if LName <> '' then
  begin
    Result := Result.Parent(NyxTextField(LName));
  end;
  LColumns := AData.Field('columns');

  if (LColumns.Kind <> ndArray) or (LColumns.Count < 1) or
    (LColumns.Count > NyxMaximumCollectionFields) then
  begin
    raise ENyxCollection.Create('Collection view requires an array of 1..64 columns');
  end;
  for LIndex := 0 to LColumns.Count - 1 do
  begin
    LColumn := LColumns.Item(LIndex);

    if (LColumn.Kind <> ndObject) or
      not (((LVersion < 7) and (LColumn.Count = 4)) or
        ((LVersion in [5, 6]) and (LColumn.Count = 5)) or
        ((LVersion = 7) and (LColumn.Count = 6)) or
        ((LVersion = 8) and (LColumn.Count = 7))) then
    begin
      raise ENyxCollection.Create('Invalid collection view column descriptor');
    end;
    LName := LColumn.Field('field').AsText;
    LKind := LColumn.Field('kind').AsText;
    LTitle := LColumn.Field('title').AsText;
    LMode := cmReadOnly;

    if LColumn.Field('editable').AsBoolean then
    begin
      LMode := cmEditable;
    end;

    if LKind = 'text' then
    begin
      Result := Result.Column(NyxTextField(LName), LTitle, LMode);
    end
    else if LKind = 'boolean' then
    begin
      Result := Result.Column(NyxBooleanField(LName), LTitle, LMode);
    end
    else if LKind = 'integer' then
    begin
      Result := Result.Column(NyxIntegerField(LName), LTitle, LMode);
    end
    else if LKind = 'number' then
    begin
      Result := Result.Column(NyxNumberField(LName), LTitle, LMode);
    end
    else
    begin
      raise ENyxCollection.Create('Unknown collection view column kind');
    end;

    if (LVersion in [5, 6]) and (LColumn.Count = 5) then
    begin
      for LFieldIndex := 0 to LColumn.Count - 1 do
      begin

        if Pos('|' + LColumn.Key(LFieldIndex) + '|', '|field|kind|title|editable|sizing|') = 0 then
        begin
          raise ENyxCollection.Create('Unknown sized collection column field');
        end;
      end;
      LPolicy := TNyxColumnSizing.FromData(LColumn.Field('sizing'));
      Result := Result.ColumnSizing(LPolicy);
    end;

    if LVersion = 7 then
    begin
      for LFieldIndex := 0 to LColumn.Count - 1 do
      begin

        if Pos('|' + LColumn.Key(LFieldIndex) + '|',
          '|field|kind|title|editable|sizing|queryControls|') = 0 then
        begin
          raise ENyxCollection.Create('Unknown queryable collection column field');
        end;
      end;

      if LColumn.Field('sizing').Kind <> ndNull then
      begin
        Result := Result.ColumnSizing(TNyxColumnSizing.FromData(LColumn.Field('sizing')));
      end;

      if LColumn.Field('queryControls').Kind <> ndNull then
      begin
        Result := Result.ColumnQuery(TNyxColumnQueryPolicy.FromData(LColumn.Field('queryControls')));
      end;
    end;
  end;

  { Layout v8 is deliberately decoded separately from all earlier column
    shapes, preserving their original closed-member admission rules. }

  if LVersion = 8 then
  begin
    for LIndex := 0 to LColumns.Count - 1 do
    begin
      LColumn := LColumns.Item(LIndex);
      for LFieldIndex := 0 to LColumn.Count - 1 do
      begin

        if Pos('|' + LColumn.Key(LFieldIndex) + '|',
          '|field|kind|title|editable|sizing|queryControls|layoutControls|') = 0 then
        begin
          raise ENyxCollection.Create('Unknown arranged collection column field');
        end;
      end;
      { The columns were already admitted in canonical order. Configure this
        exact owned record directly, rather than applying a fluent last-column
        method repeatedly to whichever column happens to be last. }
      Result.FColumns[LIndex].FSizing := TNyxColumnSizing.FromData(LColumn.Field('sizing'));
      Result.FColumns[LIndex].FQueryPolicy := TNyxColumnQueryPolicy.FromData(LColumn.Field('queryControls'));
      Result.FColumns[LIndex].FLayoutPolicy := TNyxColumnLayoutPolicy.FromData(LColumn.Field('layoutControls'));
    end;
  end;

  if (LVersion = 5) and not Result.HasColumnSizing then
  begin
    raise ENyxCollection.Create('Version-five collection views require an explicit column sizing');
  end;

  if (LVersion = 6) or ((LVersion in [7, 8]) and (AData.Field('paging').Kind <> ndNull)) then
  begin
    Result := Result.Paged(TNyxCollectionPaging.FromData(AData.Field('paging')));
  end;

  if LVersion = 7 then
  begin

    if AData.Field('queryControls').Kind <> ndNull then
    begin
      Result := Result.TableQuery(TNyxTableQueryPolicy.FromData(AData.Field('queryControls')));
    end;

    if not Result.HasQueryControls then
    begin
      raise ENyxCollection.Create('Version-seven views require an explicit query control choice');
    end;
  end;

  if LVersion = 8 then
  begin
    Result.FTableQuery := TNyxTableQueryPolicy.FromData(AData.Field('queryControls'));
    Result.FTableLayout := TNyxTableLayoutPolicy.FromData(AData.Field('layoutControls'));

    if not Result.HasLayoutControls then
    begin
      raise ENyxCollection.Create('Version-eight views require an explicit layout choice');
    end;
  end;
  Result.Validate;
end;

end.
