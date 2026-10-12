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
  nyx.collections.selection,
  nyx.typeahead,
  nyx.collections.columns,
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

  { Immutable column value, owning its captions/reference text. Exact Kind is
    checked against the collection schema before a view is admitted. }
  TNyxCollectionColumn = record
  private
    FField: TNyxText;
    FKind: TNyxStateKind;
    FTitle: TNyxText;
    FMode: TNyxCollectionCellMode;
    FSizing: TNyxColumnSizing;
  public
    function Copy: TNyxCollectionColumn;
    { Read the declared scalar family from an admitted item. Missing fields and
      incompatible families reject instead of comparing formatted display text. }
    function Read(const AItem: TNyxCollectionItem): TNyxStateValue;
    property FieldName: TNyxText read FField;
    property Kind: TNyxStateKind read FKind;
    property Title: TNyxText read FTitle;
    property Mode: TNyxCollectionCellMode read FMode;
    { Undefined preserves the library's default content allocation and legacy
      wire version. A defined value owns only logical dimensions and bounds. }
    property Sizing: TNyxColumnSizing read FSizing;
  end;

  { Renderer-independent authored binding. Fluent calls return detached copies,
    including their column records on pas2js. First column labels lists/trees;
    tables display all columns. Parent is an optional text field containing an
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
    FColumns: array of TNyxCollectionColumn;
    function AddColumn(const AName: TNyxText; AKind: TNyxStateKind;
      const ATitle: TNyxText; AMode: TNyxCollectionCellMode): TNyxCollectionViewSpec;
    function GetCount: Integer;
    function GetKey: TNyxCollectionRef;
    function GetQuery: TNyxCollectionQuery;
    function GetTypeAhead: TNyxTypeAheadOptions;
    function GetHasColumnSizing: Boolean;
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
    { Save independent page defaults; runtime page changes never rewrite this
      authored value. Tables/lists consume it after their query. Trees refuse
      during projection admission until hierarchy-aware paging is supported. }
    function Paged(const APolicy: TNyxCollectionPaging): TNyxCollectionViewSpec;
    function UseDefaultPaging: TNyxCollectionViewSpec;
    function ColumnAt(AIndex: Integer): TNyxCollectionColumn;
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
  end;

function NyxCollectionView(const AKey: TNyxCollectionRef): TNyxCollectionViewSpec;

implementation

function TNyxCollectionColumn.Copy: TNyxCollectionColumn;
begin
  Result := Default(TNyxCollectionColumn);
  Result.FField := FField;
  Result.FKind := FKind;
  Result.FTitle := FTitle;
  Result.FMode := FMode;
  Result.FSizing := FSizing;
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

    if (Count <> 0) or (FParent <> '') or FQuery.Defined or FHasTypeAhead or FPaging.Defined then
    begin
      raise ENyxCollection.Create('Absent collection view contains a descriptor');
    end;
    Exit;
  end;
  LReference := NyxCollection(FKey.Name);

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
      ((AData.Count = 9) and (AData.Field('version').AsInteger = 6))) then
  begin
    raise ENyxCollection.Create('Unsupported collection view descriptor');
  end;
  Result := NyxCollectionView(NyxCollection(AData.Field('key').AsText));
  LVersion := AData.Field('version').AsInteger;

  if LVersion in [5, 6] then
  begin
    for LFieldIndex := 0 to AData.Count - 1 do
    begin

      if Pos('|' + AData.Key(LFieldIndex) + '|',
        '|version|key|scope|parent|columns|selection|query|typeAhead|paging|') = 0 then
      begin
        raise ENyxCollection.Create('Unknown sized collection view field');
      end;
    end;
  end;

  if LVersion in [3, 4, 5, 6] then
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

  if (LVersion in [5, 6]) and (AData.Field('typeAhead').Kind <> ndNull) then
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
      not ((LColumn.Count = 4) or ((LVersion in [5, 6]) and (LColumn.Count = 5))) then
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
  end;

  if (LVersion = 5) and not Result.HasColumnSizing then
  begin
    raise ENyxCollection.Create('Version-five collection views require an explicit column sizing');
  end;

  if LVersion = 6 then
  begin
    Result := Result.Paged(TNyxCollectionPaging.FromData(AData.Field('paging')));
  end;
  Result.Validate;
end;

end.
