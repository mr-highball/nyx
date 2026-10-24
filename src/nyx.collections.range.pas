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
unit nyx.collections.range;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections, nyx.collections.arrangement, nyx.collections.clipboard;

type
  { Invalid endpoints, absent page membership and capture work limits refuse
    before a clipboard host or a mutable selection is touched. }
  ENyxCollectionRange = class(ENyxCollectionClipboard);

  { Stable cell identity has no physical column/row coordinate. A default value
    is absent; a defined endpoint owns its scoped item and full scalar-family
    field. Returned records never borrow a widget or a mutable field vector. }
  TNyxCollectionCellIdentity = record
  private
    FDefined: Boolean;
    FItem: TNyxItemRef;
    FField: TNyxColumnIdentity;
    function GetItem: TNyxItemRef;
    function GetField: TNyxColumnIdentity;
  public
    function Same(const AOther: TNyxCollectionCellIdentity): Boolean;
    property Defined: Boolean read FDefined;
    property Item: TNyxItemRef read GetItem;
    property Field: TNyxColumnIdentity read GetField;
  end;

  { Immutable rectangle in ONE captured page and visible presentation order.
    Anchor/focus retain gesture direction, while Payload orders rows/fields by
    that page/presentation, independent of source order or physical RTL panes.
    Payload is accepted RAW TSV, even for a singleton; it never takes formatted
    text or a draft. This capture owns copied facts only, never a source, view,
    mount, selection, host, formatter or editor. ColumnSerial is descriptive
    provenance; a command must separately fence its original live incarnation. }
  INyxCollectionCellRangeCapture = interface(IInterface)
    ['{4D1D6219-E2CE-4D5E-8240-C7E8987C4AD6}']
    function GetAnchor: TNyxCollectionCellIdentity;
    function GetFocus: TNyxCollectionCellIdentity;
    function GetColumnSerial: Integer;
    function GetPayload: INyxCollectionClipboardText;
    function GetIncludeHeaders: Boolean;
    function GetLineEnding: TNyxCollectionCopyLineEnding;
    function Contains(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity): Boolean;
    property Anchor: TNyxCollectionCellIdentity read GetAnchor;
    property Focus: TNyxCollectionCellIdentity read GetFocus;
    property ColumnSerial: Integer read GetColumnSerial;
    property Payload: INyxCollectionClipboardText read GetPayload;
    property IncludeHeaders: Boolean read GetIncludeHeaders;
    property LineEnding: TNyxCollectionCopyLineEnding read GetLineEnding;
  end;

{ Admit an exact typed endpoint. Uninitialized/empty references refuse rather
  than becoming a physical-coordinate fallback. }
function NyxCollectionCellIdentity(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity): TNyxCollectionCellIdentity;

{ Explicit opt-in capture does not change ordinary ROW selection. Endpoints
  must belong to the supplied page and visible-field vector. Hidden/off-page
  endpoints refuse; no source row/hidden field is silently added. Both endpoints
  are included and reversed gestures produce the same ordered rectangle.

  Copy all borrowed records/arrays and retain both snapshots before getters.
  The endpoint/membership phase charges Page.Count + rectangle rows against
  MaximumScannedRows. Existing SelectedPage production independently charges
  that same bounded sum and checks every actual page row against Source. Thus
  total row inspections never exceed twice the caller's scan limit. Existing
  output row/field/cell/UTF-8 limits, TSV escaping and exact accepted schemas
  remain authoritative. Refusal publishes nothing and enters no host. }
function NewNyxCollectionCellRange(const ASource, APage: INyxCollectionSnapshot;
  const AVisibleFields: array of TNyxColumnIdentity;
  const AAnchor, AFocus: TNyxCollectionCellIdentity; AColumnSerial: Integer;
  AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionCellRangeCapture;

implementation

uses nyx.state, nyx.collections.selection;

type
  { Only the normalized row-membership snapshot, copied field subset and
    immutable RAW payload survive construction. No private array is exposed. }
  TCellRange = class(TInterfacedObject, INyxCollectionCellRangeCapture)
  private
    FAnchor: TNyxCollectionCellIdentity;
    FFocus: TNyxCollectionCellIdentity;
    FColumnSerial: Integer;
    FPayload: INyxCollectionClipboardText;
    FRows: TNyxCollectionSelectionSnapshot;
    FFields: TNyxCollectionClipboardFields;
    FHeaders: Boolean;
    FLines: TNyxCollectionCopyLineEnding;
  public
    function GetAnchor: TNyxCollectionCellIdentity;
    function GetFocus: TNyxCollectionCellIdentity;
    function GetColumnSerial: Integer;
    function GetPayload: INyxCollectionClipboardText;
    function GetIncludeHeaders: Boolean;
    function GetLineEnding: TNyxCollectionCopyLineEnding;
    function Contains(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity): Boolean;
  end;

function NyxCollectionCellIdentity(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity): TNyxCollectionCellIdentity;
begin
  Result := Default(TNyxCollectionCellIdentity);

  if not AItem.Defined or not AField.Defined then
  begin
    raise ENyxCollectionRange.Create('A cell endpoint requires an exact item and typed field');
  end;
  Result.FItem := NyxItem(AItem.Collection, AItem.ID);
  Result.FField := TNyxColumnIdentity.FromData(AField.ToData);
  Result.FDefined := True;
end;

function TNyxCollectionCellIdentity.GetItem: TNyxItemRef;
begin

  if not FDefined then
  begin
    raise ENyxCollectionRange.Create('A cell endpoint has not been constructed');
  end;
  Result := NyxItem(FItem.Collection, FItem.ID);
end;

function TNyxCollectionCellIdentity.GetField: TNyxColumnIdentity;
begin

  if not FDefined then
  begin
    raise ENyxCollectionRange.Create('A cell endpoint has not been constructed');
  end;
  Result := TNyxColumnIdentity.FromData(FField.ToData);
end;

function TNyxCollectionCellIdentity.Same(
  const AOther: TNyxCollectionCellIdentity): Boolean;
begin
  Result := FDefined = AOther.FDefined;

  if Result and FDefined then
  begin
    Result := (FItem.Collection.Name = AOther.FItem.Collection.Name) and
      (FItem.ID = AOther.FItem.ID) and FField.Same(AOther.FField);
  end;
end;

procedure RequireField(const ASchema: TNyxCollectionSchema;
  const AField: TNyxColumnIdentity);
begin
  case AField.Kind of
    nskText: ASchema.Definition(NyxTextField(AField.Name));
    nskBoolean: ASchema.Definition(NyxBooleanField(AField.Name));
    nskInteger: ASchema.Definition(NyxIntegerField(AField.Name));
    nskNumber: ASchema.Definition(NyxNumberField(AField.Name));
  end;
end;

function NewNyxCollectionCellRange(const ASource, APage: INyxCollectionSnapshot;
  const AVisibleFields: array of TNyxColumnIdentity;
  const AAnchor, AFocus: TNyxCollectionCellIdentity; AColumnSerial: Integer;
  AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionCellRangeCapture;
var
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LFields: TNyxCollectionClipboardFields;
  LSelectedFields: TNyxCollectionClipboardFields;
  LAnchor: TNyxCollectionCellIdentity;
  LFocus: TNyxCollectionCellIdentity;
  LLimits: TNyxCollectionClipboardLimits;
  LKey: TNyxCollectionRef;
  LSchema: TNyxCollectionSchema;
  LPageSchema: TNyxCollectionSchema;
  LRevision: Integer;
  LSourceCount: Integer;
  LPageCount: Integer;
  LAnchorRow: Integer;
  LFocusRow: Integer;
  LAnchorField: Integer;
  LFocusField: Integer;
  LFirstRow: Integer;
  LLastRow: Integer;
  LFirstField: Integer;
  LLastField: Integer;
  LRowCount: Integer;
  LFieldCount: Integer;
  LIndex: Integer;
  LOther: Integer;
  LRefs: TNyxItemRefs;
  LSelected: TNyxItemRefs;
  LRef: TNyxItemRef;
  LSelection: INyxCollectionSelection;
  LMembership: TNyxCollectionSelectionSnapshot;
  LPayload: INyxCollectionClipboardText;
  LRange: TCellRange;
  LRangeLease: INyxCollectionCellRangeCapture;
begin
  LSource := ASource;
  LPage := APage;
  LLimits := ALimits;
  LLimits.Validate;

  if not AAnchor.Defined or not AFocus.Defined or
    (AColumnSerial < 0) or (AColumnSerial >= High(Integer)) or
    (Length(AVisibleFields) < 1) or (Length(AVisibleFields) > NyxMaximumCollectionFields) then
  begin
    raise ENyxCollectionRange.Create('Cell range requires live bounded endpoint/column facts');
  end;
  LAnchor := NyxCollectionCellIdentity(AAnchor.Item, AAnchor.Field);
  LFocus := NyxCollectionCellIdentity(AFocus.Item, AFocus.Field);
  SetLength(LFields, Length(AVisibleFields));
  for LIndex := 0 to High(LFields) do
  begin
    LFields[LIndex] := TNyxColumnIdentity.FromData(AVisibleFields[LIndex].ToData);
  end;

  if (LSource = nil) or (LPage = nil) then
  begin
    raise ENyxCollectionRange.Create('Cell range requires its actual accepted source and page');
  end;
  LKey := NyxCollection(LSource.Key.Name);
  LRevision := LSource.Revision;
  LSchema := LSource.Schema.Copy;
  LSchema.Validate;
  LSourceCount := LSource.Count;
  LPageCount := LPage.Count;
  LPageSchema := LPage.Schema;

  if (LRevision < 0) or (LSourceCount < 0) or
    (LSourceCount > NyxMaximumCollectionItems) or (LPageCount < 1) or
    (LPageCount > LSourceCount) or (LPageCount > LLimits.MaximumScannedRows) or
    (LPage.Key.Name <> LKey.Name) or (LPage.Revision <> LRevision) or
    not LSchema.SameSchema(LPageSchema) or
    (LAnchor.Item.Collection.Name <> LKey.Name) or
    (LFocus.Item.Collection.Name <> LKey.Name) then
  begin
    raise ENyxCollectionRange.Create('Cell range page/schema/revision does not match its source');
  end;
  LAnchorField := -1;
  LFocusField := -1;
  for LIndex := 0 to High(LFields) do
  begin
    RequireField(LSchema, LFields[LIndex]);
    for LOther := 0 to LIndex - 1 do
    begin

      if LFields[LIndex].Same(LFields[LOther]) then
      begin
        raise ENyxCollectionRange.Create('Visible range fields contain a duplicate');
      end;
    end;

    if LFields[LIndex].Same(LAnchor.Field) then
    begin
      LAnchorField := LIndex;
    end;

    if LFields[LIndex].Same(LFocus.Field) then
    begin
      LFocusField := LIndex;
    end;
  end;

  if (LAnchorField < 0) or (LFocusField < 0) then
  begin
    raise ENyxCollectionRange.Create('Cell range endpoint is absent from visible typed fields');
  end;
  LFirstField := LAnchorField;
  LLastField := LFocusField;

  if LFirstField > LLastField then
  begin
    LFirstField := LFocusField;
    LLastField := LAnchorField;
  end;
  LFieldCount := LLastField - LFirstField + 1;

  if LFieldCount > LLimits.MaximumFields then
  begin
    raise ENyxCollectionRange.Create('Cell range exceeds its output field budget');
  end;
  LAnchorRow := -1;
  LFocusRow := -1;
  SetLength(LRefs, LPageCount);
  for LIndex := 0 to LPageCount - 1 do
  begin
    LRef := LPage.ItemAt(LIndex).Ref;
    LRefs[LIndex] := NyxItem(LRef.Collection, LRef.ID);

    if (LRef.Collection.Name = LKey.Name) and (LRef.ID = LAnchor.Item.ID) then
    begin
      LAnchorRow := LIndex;
    end;

    if (LRef.Collection.Name = LKey.Name) and (LRef.ID = LFocus.Item.ID) then
    begin
      LFocusRow := LIndex;
    end;
  end;

  if (LAnchorRow < 0) or (LFocusRow < 0) then
  begin
    raise ENyxCollectionRange.Create('Cell range endpoint is absent from the captured page');
  end;
  LFirstRow := LAnchorRow;
  LLastRow := LFocusRow;

  if LFirstRow > LLastRow then
  begin
    LFirstRow := LFocusRow;
    LLastRow := LAnchorRow;
  end;
  LRowCount := LLastRow - LFirstRow + 1;

  if (LRowCount > LLimits.MaximumRows) or
    (LRowCount > LLimits.MaximumCells div LFieldCount) or
    (LRowCount > LLimits.MaximumScannedRows - LPageCount) then
  begin
    raise ENyxCollectionRange.Create('Cell range exceeds its row/cell/capture-work budget');
  end;
  SetLength(LSelected, LRowCount);
  for LIndex := 0 to LRowCount - 1 do
  begin
    LSelected[LIndex] := LRefs[LFirstRow + LIndex];
  end;
  SetLength(LSelectedFields, LFieldCount);
  for LIndex := 0 to LFieldCount - 1 do
  begin
    LSelectedFields[LIndex] := LFields[LFirstField + LIndex];
  end;
  { Membership is immutable and belongs to the page's admitted ordering. The
    existing producer separately verifies its exact source membership/scalars
    and serializes in PAGE order, never the row-selection gesture's order. }
  LSelection := NewNyxCollectionSelection(LPage, LSelected, LAnchor.Item, LFocus.Item);
  LMembership := LSelection.Snapshot;
  LPayload := NewNyxCollectionClipboardRows(LSource, LMembership, LSelectedFields,
    nccmSelectedPage, LPage, AHeaders, ALineEnding, LLimits);
  LRange := TCellRange.Create;
  LRangeLease := LRange;
  LRange.FAnchor := LAnchor;
  LRange.FFocus := LFocus;
  LRange.FColumnSerial := AColumnSerial;
  LRange.FRows := LMembership;
  LRange.FFields := LSelectedFields;
  LRange.FPayload := LPayload;
  LRange.FHeaders := AHeaders;
  LRange.FLines := ALineEnding;
  { FPC can pass a caller's existing interface variable as the hidden Result
    destination. Publish only this complete local owner; refusal must not clear
    that caller value while the candidate is still being admitted. }
  Result := LRangeLease;
end;

function TCellRange.GetAnchor: TNyxCollectionCellIdentity;
begin
  Result := FAnchor;
end;

function TCellRange.GetFocus: TNyxCollectionCellIdentity;
begin
  Result := FFocus;
end;

function TCellRange.GetColumnSerial: Integer;
begin
  Result := FColumnSerial;
end;

function TCellRange.GetPayload: INyxCollectionClipboardText;
begin
  Result := FPayload;
end;

function TCellRange.GetIncludeHeaders: Boolean;
begin
  Result := FHeaders;
end;

function TCellRange.GetLineEnding: TNyxCollectionCopyLineEnding;
begin
  Result := FLines;
end;

function TCellRange.Contains(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if not AItem.Defined or not AField.Defined or
    (AItem.Collection.Name <> FRows.Key.Name) or not FRows.Contains(AItem) then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FFields) do
  begin

    if FFields[LIndex].Same(AField) then
    begin
      Exit(True);
    end;
  end;
end;

end.
