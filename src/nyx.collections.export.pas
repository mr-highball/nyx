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
unit nyx.collections.export;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.bytes, nyx.state, nyx.collections,
  nyx.collections.arrangement, nyx.collections.view, nyx.collections.clipboard,
  nyx.collections.selection, nyx.files.save;

type
  { Invalid captures and bounded serialization refuse before file authority;
    operational save outcomes remain the existing typed files.save results. }
  ENyxCollectionExport = class(ENyxCollection);

  { Actual accepted page versus complete filtered/ordered result. Neither
    means selected rows, and hidden selection never changes export membership. }
  TNyxCollectionExportRows = (ncerCurrentPage, ncerAllFiltered);

  { Closed runtime export permission set. Empty denies both memberships; source
    selection never implicitly broadens this independently configured authority. }
  TNyxCollectionExportMemberships = set of TNyxCollectionExportRows;

  { Runtime RAW TSV policy is independent of authored defaults. Headers use
    exact typed field names; field order/hidden inclusion is explicit. Text
    values never normalize NUL, Unicode or embedded line endings. }
  TNyxCollectionExportOptions = record
  private
    FDefined: Boolean;
    FFields: TNyxCollectionCopyFields;
    FHeaders: Boolean;
    FLines: TNyxCollectionCopyLineEnding;
    FLimits: TNyxCollectionClipboardLimits;
    FSave: TNyxFileSaveOptions;
    FAllowedRows: TNyxCollectionExportMemberships;
  public
    class function Defaults: TNyxCollectionExportOptions; static;
    function Copy: TNyxCollectionExportOptions;
    function Fields(AChoice: TNyxCollectionCopyFields): TNyxCollectionExportOptions;
    function Headers(AEnabled: Boolean): TNyxCollectionExportOptions;
    function LineEnding(AChoice: TNyxCollectionCopyLineEnding): TNyxCollectionExportOptions;
    function Limits(const AValue: TNyxCollectionClipboardLimits): TNyxCollectionExportOptions;
    function SaveAs(const AValue: TNyxFileSaveOptions): TNyxCollectionExportOptions;
    function AllowRows(AValue: TNyxCollectionExportMemberships): TNyxCollectionExportOptions;
    function Allows(AMembership: TNyxCollectionExportRows): Boolean;
    procedure Validate;
    property FieldChoice: TNyxCollectionCopyFields read FFields;
    property IncludeHeaders: Boolean read FHeaders;
    property Lines: TNyxCollectionCopyLineEnding read FLines;
    property WorkLimits: TNyxCollectionClipboardLimits read FLimits;
    property SaveOptions: TNyxFileSaveOptions read FSave;
    property AllowedRows: TNyxCollectionExportMemberships read FAllowedRows;
  end;

  { Independently owned immutable RAW output. It retains exact copied item and
    typed field identities/scalars plus UTF-8 bytes, never a capture/view/store,
    document, selection, editor or host. Byte getters copy their mutable arrays.
    TSV quotes fields containing tab/CR/LF/NUL/quote and doubles quotes. No final
    row terminator is emitted; an empty result has only headers, or zero bytes. }
  INyxCollectionExportText = interface(IInterface)
    ['{BB7E3BA5-92FB-48CF-833B-04653B1479E5}']
    function GetText: TNyxText;
    function GetBytes: TNyxBytes;
    function GetByteCount: Integer;
    function GetKey: TNyxCollectionRef;
    function GetRevision: Integer;
    function GetMembership: TNyxCollectionExportRows;
    function GetRowCount: Integer;
    function GetFieldCount: Integer;
    function ItemAt(AIndex: Integer): TNyxItemRef;
    function FieldAt(AIndex: Integer): TNyxColumnIdentity;
    function CellValue(ARow, AField: Integer): TNyxStateValue;
    property Text: TNyxText read GetText;
    property Bytes: TNyxBytes read GetBytes;
    property ByteCount: Integer read GetByteCount;
    property Key: TNyxCollectionRef read GetKey;
    property Revision: Integer read GetRevision;
    property Membership: TNyxCollectionExportRows read GetMembership;
    property RowCount: Integer read GetRowCount;
    property FieldCount: Integer read GetFieldCount;
  end;

{ Pure bounded preparation uses the capture's ACTUAL filtered/page membership,
  not a re-evaluated query or synthetic selection. Every result row must equal
  its accepted source row and share exact schema/key/revision; duplicates refuse.
  Work includes one result inspection and one exact source-row check per row. }
function NewNyxCollectionExportText(const ACapture: INyxCollectionExportCapture;
  AMembership: TNyxCollectionExportRows; const AOptions: TNyxCollectionExportOptions):
  INyxCollectionExportText;

implementation

uses nyx.collections.arrangement.controls;

type
  { Builder is private until full admission. Published getters return copied
    scalar/identity records and independent bytes on both targets. }
  TExportText = class(TInterfacedObject, INyxCollectionExportText)
  public
    Text: TNyxText;
    Bytes: TNyxBytes;
    Key: TNyxCollectionRef;
    Revision: Integer;
    Membership: TNyxCollectionExportRows;
    Items: TNyxItemRefs;
    Fields: TNyxCollectionClipboardFields;
    Values: array of TNyxStateValue;
    function GetText: TNyxText;
    function GetBytes: TNyxBytes;
    function GetByteCount: Integer;
    function GetKey: TNyxCollectionRef;
    function GetRevision: Integer;
    function GetMembership: TNyxCollectionExportRows;
    function GetRowCount: Integer;
    function GetFieldCount: Integer;
    function ItemAt(AIndex: Integer): TNyxItemRef;
    function FieldAt(AIndex: Integer): TNyxColumnIdentity;
    function CellValue(ARow, AField: Integer): TNyxStateValue;
  end;

class function TNyxCollectionExportOptions.Defaults: TNyxCollectionExportOptions;
begin
  Result := Default(TNyxCollectionExportOptions);
  Result.FDefined := True;
  Result.FAllowedRows := [ncerCurrentPage, ncerAllFiltered];
  Result.FFields := nccfVisiblePresentation;
  Result.FHeaders := True;
  Result.FLines := nccleLF;
  Result.FLimits := TNyxCollectionClipboardLimits.Defaults;
  Result.FSave := NyxFileSaveOptions('collection.tsv').Title('Export accepted table rows')
    .MediaType(NyxMediaType('text/tab-separated-values')).Allow('tsv')
    .MaximumBytes(Result.FLimits.MaximumUTF8Bytes);
end;

procedure TNyxCollectionExportOptions.Validate;
begin

  if not FDefined or (Ord(FFields) < Ord(Low(TNyxCollectionCopyFields))) or
    (Ord(FFields) > Ord(High(TNyxCollectionCopyFields))) or
    (Ord(FLines) < Ord(Low(TNyxCollectionCopyLineEnding))) or
    (Ord(FLines) > Ord(High(TNyxCollectionCopyLineEnding))) or
    (FAllowedRows - [Low(TNyxCollectionExportRows)..High(TNyxCollectionExportRows)] <> []) then
  begin
    raise ENyxCollectionExport.Create('Export requires initialized known RAW TSV options');
  end;
  FLimits.Validate;
  FSave.Validate;
end;

function TNyxCollectionExportOptions.Copy: TNyxCollectionExportOptions;
begin
  Validate;
  Result := Self;
  Result.FSave := FSave.Copy;
end;

function TNyxCollectionExportOptions.AllowRows(
  AValue: TNyxCollectionExportMemberships): TNyxCollectionExportOptions;
begin
  Result := Copy;
  Result.FAllowedRows := AValue;
  Result.Validate;
end;

function TNyxCollectionExportOptions.Allows(AMembership: TNyxCollectionExportRows): Boolean;
begin
  Validate;
  Result := (Ord(AMembership) >= Ord(Low(TNyxCollectionExportRows))) and
    (Ord(AMembership) <= Ord(High(TNyxCollectionExportRows))) and
    (AMembership in FAllowedRows);
end;

function TNyxCollectionExportOptions.Fields(AChoice: TNyxCollectionCopyFields): TNyxCollectionExportOptions;
begin
  Result := Copy;
  Result.FFields := AChoice;
  Result.Validate;
end;

function TNyxCollectionExportOptions.Headers(AEnabled: Boolean): TNyxCollectionExportOptions;
begin
  Result := Copy;
  Result.FHeaders := AEnabled;
end;

function TNyxCollectionExportOptions.LineEnding(AChoice: TNyxCollectionCopyLineEnding): TNyxCollectionExportOptions;
begin
  Result := Copy;
  Result.FLines := AChoice;
  Result.Validate;
end;

function TNyxCollectionExportOptions.Limits(const AValue: TNyxCollectionClipboardLimits): TNyxCollectionExportOptions;
begin
  Result := Copy;
  Result.FLimits := AValue;
  Result.Validate;
end;

function TNyxCollectionExportOptions.SaveAs(const AValue: TNyxFileSaveOptions): TNyxCollectionExportOptions;
begin
  Result := Copy;
  Result.FSave := AValue.Copy;
  Result.Validate;
end;

function RawValue(const AItem: TNyxCollectionItem;
  const AField: TNyxColumnIdentity): TNyxStateValue;
begin
  AField.Validate;
  case AField.Kind of
    nskText: Result := TNyxStateValue.FromText(AItem.GetValue(NyxTextField(AField.Name)));
    nskBoolean: Result := TNyxStateValue.FromBoolean(AItem.GetValue(NyxBooleanField(AField.Name)));
    nskInteger: Result := TNyxStateValue.FromInteger(AItem.GetValue(NyxIntegerField(AField.Name)));
    nskNumber: Result := TNyxStateValue.FromNumber(AItem.GetValue(NyxNumberField(AField.Name)));
  end;
end;

function RawText(const AValue: TNyxStateValue): TNyxText;
begin

  if (Ord(AValue.Kind) < Ord(nskText)) or (Ord(AValue.Kind) > Ord(nskNumber)) then
  begin
    raise ENyxCollectionExport.Create('Export scalar family is unknown');
  end;
  case AValue.Kind of
    nskText: Result := AValue.TextValue;
    nskBoolean:
      begin
        Result := 'false';

        if AValue.BooleanValue then
        begin
          Result := 'true';
        end;
      end;
    nskInteger: Result := TNyxText(IntToStr(AValue.IntegerValue));
    nskNumber: Result := AValue.NumberText;
  end;
end;

function NewNyxCollectionExportText(const ACapture: INyxCollectionExportCapture;
  AMembership: TNyxCollectionExportRows; const AOptions: TNyxCollectionExportOptions):
  INyxCollectionExportText;
var
  LCapture: INyxCollectionExportCapture;
  LSource: INyxCollectionSnapshot;
  LRows: INyxCollectionSnapshot;
  LColumns: INyxCollectionColumnsCapture;
  LOptions: TNyxCollectionExportOptions;
  LFields: TNyxCollectionClipboardFields;
  LSchema: TNyxCollectionSchema;
  LPayload: TExportText;
  LLease: INyxCollectionExportText;
  LParts: TNyxStrings;
  LSeen: array of Boolean;
  LKey: TNyxCollectionRef;
  LSourceCount: Integer;
  LRowCount: Integer;
  LRevision: Integer;
  LRow: Integer;
  LField: Integer;
  LPosition: Integer;
  LOther: Integer;
  LByteCount: Integer;
  LLine: TNyxText;
  LItem: TNyxCollectionItem;

  procedure Append(const AText: TNyxText);
  var
    LBytes: Integer;
  begin
    LBytes := NyxUTF8ByteCount(AText);

    if LBytes > LOptions.WorkLimits.MaximumUTF8Bytes - LByteCount then
    begin
      raise ENyxCollectionExport.Create('Export exceeds its UTF-8 output budget');
    end;
    Inc(LByteCount, LBytes);
    LParts.Add(AText);
  end;

  procedure AppendCell(const AText: TNyxText);
  var
    LIndex: Integer;
    LStart: Integer;
  begin

    if NyxUTF8ByteCount(AText) > LOptions.WorkLimits.MaximumUTF8Bytes - LByteCount then
    begin
      raise ENyxCollectionExport.Create('Export cell exceeds its UTF-8 output budget');
    end;

    if (Pos(#9, AText) = 0) and (Pos(#13, AText) = 0) and
      (Pos(#10, AText) = 0) and (Pos(#0, AText) = 0) and (Pos('"', AText) = 0) then
    begin
      Append(AText);
      Exit;
    end;
    Append('"');
    LStart := 1;
    for LIndex := 1 to Length(AText) do
    begin

      if AText[LIndex] = '"' then
      begin
        Append(Copy(AText, LStart, LIndex - LStart));
        Append('""');
        LStart := LIndex + 1;
      end;
    end;
    Append(Copy(AText, LStart, Length(AText) - LStart + 1));
    Append('"');
  end;

begin
  LCapture := ACapture;
  LOptions := AOptions.Copy;

  if (LCapture = nil) or (Ord(AMembership) < Ord(Low(TNyxCollectionExportRows))) or
    (Ord(AMembership) > Ord(High(TNyxCollectionExportRows))) or
    not LOptions.Allows(AMembership) then
  begin
    raise ENyxCollectionExport.Create('Export needs an explicit captured row membership');
  end;
  LSource := LCapture.Source;
  LColumns := LCapture.Columns;
  LRows := LCapture.Page;

  if AMembership = ncerAllFiltered then
  begin
    LRows := LCapture.Filtered;
  end;

  if (LSource = nil) or (LRows = nil) or (LColumns = nil) then
  begin
    raise ENyxCollectionExport.Create('Export capture is incomplete');
  end;
  LKey := NyxCollection(LSource.Key.Name);
  LRevision := LSource.Revision;
  LSchema := LSource.Schema.Copy;
  LSchema.Validate;
  LSourceCount := LSource.Count;
  LRowCount := LRows.Count;
  LFields := NyxCollectionClipboardFields(TNyxColumnLayoutObservation.Read(LColumns),
    LOptions.FieldChoice);

  if (LRevision < 0) or (LSourceCount < 0) or
    (LSourceCount > NyxMaximumCollectionItems) or (LRowCount < 0) or
    (LRowCount > LSourceCount) or (LRows.Key.Name <> LKey.Name) or
    (LRows.Revision <> LRevision) or not LSchema.SameSchema(LRows.Schema) or
    (Length(LFields) < 1) or (Length(LFields) > LOptions.WorkLimits.MaximumFields) or
    (LRowCount > LOptions.WorkLimits.MaximumRows) or
    (LRowCount > LOptions.WorkLimits.MaximumCells div Length(LFields)) or
    (LRowCount > LOptions.WorkLimits.MaximumScannedRows div 2) then
  begin
    raise ENyxCollectionExport.Create('Export membership/schema or bounded work does not match its source');
  end;
  for LField := 0 to High(LFields) do
  begin
    LFields[LField].Validate;
    case LFields[LField].Kind of
      nskText: LSchema.Definition(NyxTextField(LFields[LField].Name));
      nskBoolean: LSchema.Definition(NyxBooleanField(LFields[LField].Name));
      nskInteger: LSchema.Definition(NyxIntegerField(LFields[LField].Name));
      nskNumber: LSchema.Definition(NyxNumberField(LFields[LField].Name));
    end;
    for LOther := 0 to LField - 1 do
    begin

      if LFields[LField].Same(LFields[LOther]) then
      begin
        raise ENyxCollectionExport.Create('Export fields repeat a typed identity');
      end;
    end;
  end;
  LPayload := TExportText.Create;
  LLease := LPayload;
  LPayload.Key := LKey;
  LPayload.Revision := LRevision;
  LPayload.Membership := AMembership;
  LPayload.Fields := LFields;
  SetLength(LPayload.Items, LRowCount);
  SetLength(LPayload.Values, LRowCount * Length(LFields));
  SetLength(LSeen, LSourceCount);
  LParts := TNyxStrings.Create;
  try
    LByteCount := 0;
    LLine := #10;

    if LOptions.Lines = nccleCRLF then
    begin
      LLine := #13#10;
    end;

    if LOptions.IncludeHeaders then
    begin
      for LField := 0 to High(LFields) do
      begin

        if LField > 0 then
        begin
          Append(#9);
        end;
        AppendCell(LFields[LField].Name);
      end;
    end;
    for LRow := 0 to LRowCount - 1 do
    begin
      LItem := LRows.ItemAt(LRow);
      LPosition := LSource.IndexOf(LItem.Ref);

      if (LPosition < 0) or (LPosition >= LSourceCount) or LSeen[LPosition] or
        not LItem.SameItem(LSource.Item(LItem.Ref)) then
      begin
        raise ENyxCollectionExport.Create('Export result repeats, changes or leaves its exact accepted source');
      end;
      LSeen[LPosition] := True;
      LPayload.Items[LRow] := NyxItem(LKey, LItem.Ref.ID);

      if (LRow > 0) or LOptions.IncludeHeaders then
      begin
        Append(LLine);
      end;
      for LField := 0 to High(LFields) do
      begin

        if LField > 0 then
        begin
          Append(#9);
        end;
        LPayload.Values[LRow * Length(LFields) + LField] := RawValue(LItem, LFields[LField]);
        AppendCell(RawText(LPayload.Values[LRow * Length(LFields) + LField]));
      end;
    end;
    LPayload.Text := LParts.Join;
    LPayload.Bytes := NyxEncodeUTF8(LPayload.Text);
    LOptions.SaveOptions.Admit(LPayload.Bytes);
    Result := LLease;
  finally
    LParts.Free;
  end;
end;

function TExportText.GetText: TNyxText;
begin
  Result := Text;
end;

function TExportText.GetBytes: TNyxBytes;
var
  LIndex: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Bytes));
  for LIndex := 0 to High(Bytes) do
  begin
    Result[LIndex] := Bytes[LIndex];
  end;
end;

function TExportText.GetKey: TNyxCollectionRef;
begin
  Result := NyxCollection(Key.Name);
end;

function TExportText.GetByteCount: Integer;
begin
  Result := Length(Bytes);
end;

function TExportText.GetRevision: Integer;
begin
  Result := Revision;
end;

function TExportText.GetMembership: TNyxCollectionExportRows;
begin
  Result := Membership;
end;

function TExportText.GetRowCount: Integer;
begin
  Result := Length(Items);
end;

function TExportText.GetFieldCount: Integer;
begin
  Result := Length(Fields);
end;

function TExportText.ItemAt(AIndex: Integer): TNyxItemRef;
begin

  if (AIndex < 0) or (AIndex >= Length(Items)) then
  begin
    raise ENyxCollectionExport.Create('Export row index is outside its immutable payload');
  end;
  Result := NyxItem(Key, Items[AIndex].ID);
end;

function TExportText.FieldAt(AIndex: Integer): TNyxColumnIdentity;
begin

  if (AIndex < 0) or (AIndex >= Length(Fields)) then
  begin
    raise ENyxCollectionExport.Create('Export field index is outside its immutable payload');
  end;
  Result := TNyxColumnIdentity.FromData(Fields[AIndex].ToData);
end;

function TExportText.CellValue(ARow, AField: Integer): TNyxStateValue;
begin
  ItemAt(ARow);
  FieldAt(AField);
  Result := Values[ARow * Length(Fields) + AField].Copy;
end;

end.
