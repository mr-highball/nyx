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
unit nyx.collections.clipboard;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.state, nyx.collections, nyx.collections.selection,
  nyx.collections.arrangement, nyx.collections.arrangement.controls,
  nyx.scheduler, nyx.clipboard;

type
  { Invalid/stale immutable inputs or a work/output budget refusal occur before
    any clipboard authority is entered. Operational writes use typed host results. }
  ENyxCollectionClipboard = class(ENyxClipboard);

  { Whole membership includes filtered-out/off-page selected source rows, in
    admitted SOURCE order. Page membership intersects selection with an explicit
    accepted page and uses that PAGE order. Neither means all filtered matches. }
  TNyxCollectionCopyMembership = (nccmAllSelectedSource, nccmSelectedPage);

  { Row serialization uses an explicit line ending; cells are never normalized.
    No trailing row terminator is emitted. PlatformText host mode may normalize
    line endings separately and reports that weaker representation explicitly. }
  TNyxCollectionCopyLineEnding = (nccleLF, nccleCRLF);

  { Visible presentation order includes logical pin grouping. All arrangement
    order includes hidden fields and uses the complete preference order. These
    names do not imply physical RTL coordinates or infer a selection range. }
  TNyxCollectionCopyFields = (nccfVisiblePresentation, nccfAllArrangement);

  { An independently owned typed field vector; callers choose its exact order.
    No public operation accepts physical grid/DOM column coordinates. }
  TNyxCollectionClipboardFields = array of TNyxColumnIdentity;

  { Owned admitted output rows in explicit source/page order. The private row
    vector is immutable after capture, owns values only and exposes copied rows;
    identity-only output can reuse membership admission without a fake field. }
  TNyxCollectionCopyMembers = record
  private
    FKey: TNyxCollectionRef;
    FRevision: Integer;
    FItems: array of TNyxCollectionItem;
    function GetCount: Integer;
  public
    function ItemAt(AIndex: Integer): TNyxCollectionItem;
    property Key: TNyxCollectionRef read FKey;
    property Revision: Integer read FRevision;
    property Count: Integer read GetCount;
  end;

  { Caller-adjustable bounded work, capped by existing collection/clipboard
    limits. Defaults allow 16384 rows, 64 fields, 65536 output cells, 32768 row
    inspections and four MiB UTF-8 output. A row inspection includes each source/
    page row read and each selected identity admitted against its source. Page
    row equality additionally checks at most the existing 64 schema fields.
    Zero/default records refuse; fluent methods detach ordinary scalar values. }
  TNyxCollectionClipboardLimits = record
  private
    FDefined: Boolean;
    FRows: Integer;
    FFields: Integer;
    FCells: Integer;
    FScannedRows: Integer;
    FBytes: Integer;
  public
    class function Defaults: TNyxCollectionClipboardLimits; static;
    function Rows(AValue: Integer): TNyxCollectionClipboardLimits;
    function Fields(AValue: Integer): TNyxCollectionClipboardLimits;
    function Cells(AValue: Integer): TNyxCollectionClipboardLimits;
    function ScannedRows(AValue: Integer): TNyxCollectionClipboardLimits;
    function UTF8Bytes(AValue: Integer): TNyxCollectionClipboardLimits;
    procedure Validate;
    property MaximumRows: Integer read FRows;
    property MaximumFields: Integer read FFields;
    property MaximumCells: Integer read FCells;
    property MaximumScannedRows: Integer read FScannedRows;
    property MaximumUTF8Bytes: Integer read FBytes;
  end;

  { Immutable accepted-RAW payload and complete typed provenance. It owns copied
    identities/scalars/text, never a view, source/store, mount, editor, host or
    callback. A later edit/reorder/rebind/retirement cannot redirect these bytes.
    CellValue retains its accepted family; text copying never substitutes display
    formatting or an unfinished draft. Getters return independent record values. }
  INyxCollectionClipboardText = interface(IInterface)
    ['{405E953B-1194-4D9F-B8CF-7D221E36E64E}']
    function GetText: TNyxText;
    function GetByteCount: Integer;
    function GetKey: TNyxCollectionRef;
    function GetRevision: Integer;
    function GetRowCount: Integer;
    function GetFieldCount: Integer;
    function ItemAt(AIndex: Integer): TNyxItemRef;
    function FieldAt(AIndex: Integer): TNyxColumnIdentity;
    function CellValue(ARow: Integer; AField: Integer): TNyxStateValue;
    property Text: TNyxText read GetText;
    property ByteCount: Integer read GetByteCount;
    property Key: TNyxCollectionRef read GetKey;
    property Revision: Integer read GetRevision;
    property RowCount: Integer read GetRowCount;
    property FieldCount: Integer read GetFieldCount;
  end;

{ Pure arrangement-to-fields choice. No capture/getter of a live view occurs;
  the caller supplies its already admitted complete immutable observation. }
function NyxCollectionClipboardFields(const AObservation: TNyxColumnLayoutObservation;
  AChoice: TNyxCollectionCopyFields): TNyxCollectionClipboardFields;

{ Shared source/page admission used by typed row and identity representations.
  Every selected identity must exist in the exact source revision; explicit
  pages must match schema and actual row values. Source/page scans, output rows
  and cells are bounded before publication. Inputs are retained before getters.
  ACellsPerRow describes the real output, including one cell for identity TSV. }
function CaptureNyxCollectionCopyMembers(const ASource: INyxCollectionSnapshot;
  const ASelection: TNyxCollectionSelectionSnapshot;
  AMembership: TNyxCollectionCopyMembership; const APage: INyxCollectionSnapshot;
  ACellsPerRow: Integer; const ALimits: TNyxCollectionClipboardLimits): TNyxCollectionCopyMembers;

{ One bounded TSV cell. Tab/CR/LF/NUL/quote requires quotation; embedded quotes
  double. Exact Unicode and controls are preserved, never platform-normalized. }
function NyxCollectionTSVCell(const AText: TNyxText; AMaximumUTF8Bytes: Integer): TNyxText;

{ One entire accepted cell, including a hidden source row/field when explicitly
  requested. Text is unchanged; Boolean/integer/number use invariant lossless
  scalar wire values. A single cell is NOT TSV-quoted or newline-normalized. }
function NewNyxCollectionClipboardCell(const ASource: INyxCollectionSnapshot;
  const AItem: TNyxItemRef; const AField: TNyxColumnIdentity;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionClipboardText;

{ Copy one immutable Selection.Snapshot, not a sequence of mutable selection
  getters. Its exact key/data revision must match Source. Page mode requires a
  retained page with the same key/revision/schema and exact source row values;
  all-source mode requires Page=nil. Empty membership/intersection refuses.
  Explicit fields select hidden-column inclusion and order. Optional headers
  use exact FIELD NAMES, not authored/display titles. TSV uses tab delimiters;
  cells/headers containing tab, CR, LF, NUL or quote are quoted and quotes doubled.
  Full Unicode/NUL remain in the text; an exact-capable host is needed to preserve
  them externally. Neither capture mutates selection/data/drafts or invokes a host. }
function NewNyxCollectionClipboardRows(const ASource: INyxCollectionSnapshot;
  const ASelection: TNyxCollectionSelectionSnapshot;
  const AFields: array of TNyxColumnIdentity;
  AMembership: TNyxCollectionCopyMembership;
  const APage: INyxCollectionSnapshot; AHeaders: Boolean;
  ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionClipboardText;

{ Retain original inputs before entering either payload/host getter. This writes
  only captured bytes, never a newer source value. The command owner separately
  owns canonical-event/input Epoch/lifetime admission before calling this helper.
  Host is explicit (and may be injected); its typed unavailable/denied/cancelled
  results and managed receiver contract remain unchanged. }
function WriteNyxCollectionClipboard(const APayload: INyxCollectionClipboardText;
  const AHost: INyxClipboardWriteHost; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;

implementation

uses SysUtils, nyx.bytes;

type
  { Detached private builder becomes immutable before its first interface is
    published. Flat scalar storage is bounded by admitted output cells. }
  TClipboardText = class(TInterfacedObject, INyxCollectionClipboardText)
  private
    FText: TNyxText;
    FBytes: Integer;
    FKey: TNyxCollectionRef;
    FRevision: Integer;
    FItems: TNyxItemRefs;
    FFields: TNyxCollectionClipboardFields;
    FValues: array of TNyxStateValue;
    function GetText: TNyxText;
    function GetByteCount: Integer;
    function GetKey: TNyxCollectionRef;
    function GetRevision: Integer;
    function GetRowCount: Integer;
    function GetFieldCount: Integer;
    function ItemAt(AIndex: Integer): TNyxItemRef;
    function FieldAt(AIndex: Integer): TNyxColumnIdentity;
    function CellValue(ARow: Integer; AField: Integer): TNyxStateValue;
  end;

class function TNyxCollectionClipboardLimits.Defaults: TNyxCollectionClipboardLimits;
begin
  Result := Default(TNyxCollectionClipboardLimits);
  Result.FDefined := True;
  Result.FRows := NyxMaximumCollectionItems;
  Result.FFields := NyxMaximumCollectionFields;
  Result.FCells := 65536;
  Result.FScannedRows := NyxMaximumCollectionItems * 2;
  Result.FBytes := NyxClipboardMaximumBytes;
end;

procedure TNyxCollectionClipboardLimits.Validate;
begin

  if not FDefined or (FRows < 1) or (FRows > NyxMaximumCollectionItems) or
    (FFields < 1) or (FFields > NyxMaximumCollectionFields) or
    (FCells < 1) or (FCells > NyxMaximumCollectionItems * NyxMaximumCollectionFields) or
    (FScannedRows < 1) or (FScannedRows > NyxMaximumCollectionItems * 2) or
    (FBytes < 1) or (FBytes > NyxClipboardMaximumBytes) then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy limits exceed the supported work/byte budgets');
  end;
end;

function TNyxCollectionClipboardLimits.Rows(AValue: Integer): TNyxCollectionClipboardLimits;
begin
  Validate;
  Result := Self;
  Result.FRows := AValue;
  Result.Validate;
end;

function TNyxCollectionClipboardLimits.Fields(AValue: Integer): TNyxCollectionClipboardLimits;
begin
  Validate;
  Result := Self;
  Result.FFields := AValue;
  Result.Validate;
end;

function TNyxCollectionClipboardLimits.Cells(AValue: Integer): TNyxCollectionClipboardLimits;
begin
  Validate;
  Result := Self;
  Result.FCells := AValue;
  Result.Validate;
end;

function TNyxCollectionClipboardLimits.ScannedRows(AValue: Integer): TNyxCollectionClipboardLimits;
begin
  Validate;
  Result := Self;
  Result.FScannedRows := AValue;
  Result.Validate;
end;

function TNyxCollectionClipboardLimits.UTF8Bytes(AValue: Integer): TNyxCollectionClipboardLimits;
begin
  Validate;
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function CopyField(const AField: TNyxColumnIdentity): TNyxColumnIdentity;
begin
  AField.Validate;
  case AField.Kind of
    nskText: Result := NyxColumnIdentity(NyxTextField(AField.Name));
    nskBoolean: Result := NyxColumnIdentity(NyxBooleanField(AField.Name));
    nskInteger: Result := NyxColumnIdentity(NyxIntegerField(AField.Name));
    nskNumber: Result := NyxColumnIdentity(NyxNumberField(AField.Name));
  end;
end;

function ReadValue(const AItem: TNyxCollectionItem;
  const AField: TNyxColumnIdentity): TNyxStateValue;
begin
  case AField.Kind of
    nskText: Result := TNyxStateValue.FromText(AItem.GetValue(NyxTextField(AField.Name)));
    nskBoolean: Result := TNyxStateValue.FromBoolean(AItem.GetValue(NyxBooleanField(AField.Name)));
    nskInteger: Result := TNyxStateValue.FromInteger(AItem.GetValue(NyxIntegerField(AField.Name)));
    nskNumber: Result := TNyxStateValue.FromNumber(AItem.GetValue(NyxNumberField(AField.Name)));
  end;
end;

function ScalarText(const AValue: TNyxStateValue): TNyxText;
begin
  case AValue.Kind of
    nskText: Result := AValue.TextValue;
    nskBoolean:
      begin

        if AValue.BooleanValue then
        begin
          Result := 'true';
        end
        else
        begin
          Result := 'false';
        end;
      end;
    nskInteger: Result := TNyxText(IntToStr(AValue.IntegerValue));
    nskNumber: Result := AValue.NumberText;
  end;
end;

procedure CheckField(const ASchema: TNyxCollectionSchema;
  const AField: TNyxColumnIdentity);
begin
  AField.Validate;
  case AField.Kind of
    nskText: ASchema.Definition(NyxTextField(AField.Name));
    nskBoolean: ASchema.Definition(NyxBooleanField(AField.Name));
    nskInteger: ASchema.Definition(NyxIntegerField(AField.Name));
    nskNumber: ASchema.Definition(NyxNumberField(AField.Name));
  end;
end;

function NyxCollectionClipboardFields(const AObservation: TNyxColumnLayoutObservation;
  AChoice: TNyxCollectionCopyFields): TNyxCollectionClipboardFields;
var
  LObservation: TNyxColumnLayoutObservation;
  LIndex: Integer;
  LCanonical: Integer;
  LCount: Integer;
begin

  if (Ord(AChoice) < Ord(Low(TNyxCollectionCopyFields))) or
    (Ord(AChoice) > Ord(High(TNyxCollectionCopyFields))) then
  begin
    raise ENyxCollectionClipboard.Create('Unknown collection copy field choice');
  end;
  LObservation := AObservation.Copy;
  LCount := LObservation.Count;

  if AChoice = nccfVisiblePresentation then
  begin
    LCount := LObservation.VisibleCount;
  end;
  Result := nil;
  SetLength(Result, LCount);
  for LIndex := 0 to LCount - 1 do
  begin

    if AChoice = nccfVisiblePresentation then
    begin
      LCanonical := LObservation.VisibleAt(LIndex);
    end
    else
    begin
      LCanonical := LObservation.OrderedAt(LIndex);
    end;
    Result[LIndex] := CopyField(LObservation.ColumnAt(LCanonical).Column.Identity);
  end;
end;

procedure AdmitSource(const ASource: INyxCollectionSnapshot;
  const ALimits: TNyxCollectionClipboardLimits; out AKey: TNyxCollectionRef;
  out ARevision: Integer; out ASchema: TNyxCollectionSchema; out ACount: Integer);
begin
  ALimits.Validate;

  if ASource = nil then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy requires an accepted immutable source');
  end;
  AKey := NyxCollection(ASource.Key.Name);
  ARevision := ASource.Revision;
  ASchema := ASource.Schema.Copy;
  ASchema.Validate;
  ACount := ASource.Count;

  if (ARevision < 0) or (ACount < 0) or (ACount > NyxMaximumCollectionItems) then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy source exceeds the admitted dataset bounds');
  end;
end;

function NewNyxCollectionClipboardCell(const ASource: INyxCollectionSnapshot;
  const AItem: TNyxItemRef; const AField: TNyxColumnIdentity;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionClipboardText;
var
  LSource: INyxCollectionSnapshot;
  LItem: TNyxItemRef;
  LField: TNyxColumnIdentity;
  LLimits: TNyxCollectionClipboardLimits;
  LSchema: TNyxCollectionSchema;
  LCount: Integer;
  LValue: TNyxCollectionItem;
  LPayload: TClipboardText;
begin
  LSource := ASource;
  LItem := NyxItem(AItem.Collection, AItem.ID);
  LField := CopyField(AField);
  LLimits := ALimits;
  LPayload := TClipboardText.Create;
  try
    AdmitSource(LSource, LLimits, LPayload.FKey, LPayload.FRevision, LSchema, LCount);
    CheckField(LSchema, LField);
    LValue := LSource.Item(LItem);
    SetLength(LPayload.FItems, 1);
    SetLength(LPayload.FFields, 1);
    SetLength(LPayload.FValues, 1);
    LPayload.FItems[0] := NyxItem(LPayload.FKey, LValue.Ref.ID);
    LPayload.FFields[0] := LField;
    LPayload.FValues[0] := ReadValue(LValue, LField);
    LPayload.FText := ScalarText(LPayload.FValues[0]);
    LPayload.FBytes := NyxClipboardTextByteCount(LPayload.FText);

    if LPayload.FBytes > LLimits.MaximumUTF8Bytes then
    begin
      raise ENyxCollectionClipboard.Create('Collection cell copy exceeds its UTF-8 output budget');
    end;
    Result := LPayload;
  except
    LPayload.Free;
    raise;
  end;
end;

procedure AppendPart(AParts: TNyxStrings; const AText: TNyxText;
  var ABytes: Integer; AMaximum: Integer);
var
  LBytes: Integer;
begin
  LBytes := NyxUTF8ByteCount(AText);

  if LBytes > AMaximum - ABytes then
  begin
    raise ENyxCollectionClipboard.Create('Collection row copy exceeds its UTF-8 output budget');
  end;
  Inc(ABytes, LBytes);
  AParts.Add(AText);
end;

procedure AppendTSV(AParts: TNyxStrings; const AText: TNyxText;
  var ABytes: Integer; AMaximum: Integer);
var
  LIndex: Integer;
  LStart: Integer;
  LChunk: TNyxText;
begin

  if NyxUTF8ByteCount(AText) > AMaximum - ABytes then
  begin
    raise ENyxCollectionClipboard.Create('Collection row copy exceeds its UTF-8 output budget');
  end;

  if (Pos(#9, AText) = 0) and (Pos(#13, AText) = 0) and
    (Pos(#10, AText) = 0) and (Pos(#0, AText) = 0) and (Pos('"', AText) = 0) then
  begin
    AppendPart(AParts, AText, ABytes, AMaximum);
    Exit;
  end;
  AppendPart(AParts, '"', ABytes, AMaximum);
  LStart := 1;
  LChunk := '';
  for LIndex := 1 to Length(AText) do
  begin

    if AText[LIndex] = '"' then
    begin
      { Split only beside ASCII quotes, never inside UTF-8 bytes or a UTF-16
        surrogate pair. Bounded aggregation avoids one allocation/part for each
        quote in a large command line or error message. }
      LChunk := LChunk + Copy(AText, LStart, LIndex - LStart) + '""';

      if Length(LChunk) >= 1024 then
      begin
        AppendPart(AParts, LChunk, ABytes, AMaximum);
        LChunk := '';
      end;
      LStart := LIndex + 1;
    end;
  end;
  LChunk := LChunk + Copy(AText, LStart, Length(AText) - LStart + 1);
  AppendPart(AParts, LChunk, ABytes, AMaximum);
  AppendPart(AParts, '"', ABytes, AMaximum);
end;

function TNyxCollectionCopyMembers.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TNyxCollectionCopyMembers.ItemAt(AIndex: Integer): TNyxCollectionItem;
begin

  if (AIndex < 0) or (AIndex >= Length(FItems)) then
  begin
    raise ENyxCollectionClipboard.Create('Copied output row index is outside its admitted membership');
  end;
  Result := FItems[AIndex].Copy;
end;

function NyxCollectionTSVCell(const AText: TNyxText; AMaximumUTF8Bytes: Integer): TNyxText;
var
  LParts: TNyxStrings;
  LBytes: Integer;
begin

  if (AMaximumUTF8Bytes < 0) or (AMaximumUTF8Bytes > NyxClipboardMaximumBytes) then
  begin
    raise ENyxCollectionClipboard.Create('TSV cell requires a supported output-byte bound');
  end;
  LParts := TNyxStrings.Create;
  try
    LBytes := 0;
    AppendTSV(LParts, AText, LBytes, AMaximumUTF8Bytes);
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function CaptureNyxCollectionCopyMembers(const ASource: INyxCollectionSnapshot;
  const ASelection: TNyxCollectionSelectionSnapshot;
  AMembership: TNyxCollectionCopyMembership; const APage: INyxCollectionSnapshot;
  ACellsPerRow: Integer; const ALimits: TNyxCollectionClipboardLimits): TNyxCollectionCopyMembers;
var
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LSelection: TNyxCollectionSelectionSnapshot;
  LLimits: TNyxCollectionClipboardLimits;
  LSchema: TNyxCollectionSchema;
  LKey: TNyxCollectionRef;
  LRevision: Integer;
  LSourceCount: Integer;
  LScanCount: Integer;
  LIndex: Integer;
  LRow: Integer;
  LPosition: Integer;
  LCapacity: Integer;
  LSeen: array of Boolean;
  LItem: TNyxCollectionItem;
  LRef: TNyxItemRef;
  LResult: TNyxCollectionCopyMembers;
begin
  LSource := ASource;
  LPage := APage;
  LSelection := ASelection;
  LLimits := ALimits;
  LLimits.Validate;

  if (ACellsPerRow < 1) or (ACellsPerRow > LLimits.MaximumFields) or
    (ACellsPerRow > LLimits.MaximumCells) then
  begin
    raise ENyxCollectionClipboard.Create('Copied membership requires its bounded actual cell count');
  end;
  AdmitSource(LSource, LLimits, LKey, LRevision, LSchema, LSourceCount);

  if (Ord(AMembership) < Ord(Low(TNyxCollectionCopyMembership))) or
    (Ord(AMembership) > Ord(High(TNyxCollectionCopyMembership))) or
    not LSelection.Defined or (LSelection.Key.Name <> LKey.Name) or
    (LSelection.DataRevision <> LRevision) or (LSelection.Count = 0) or
    (LSelection.Count > NyxMaximumCollectionItems) then
  begin
    raise ENyxCollectionClipboard.Create('Copied membership is unknown, missing, stale or exceeds its bounds');
  end;
  LScanCount := LSelection.Count;

  if AMembership = nccmAllSelectedSource then
  begin

    if LPage <> nil then
    begin
      raise ENyxCollectionClipboard.Create('All-selected source copy does not consume a page');
    end;

    if (LSelection.Count > LLimits.MaximumRows) or
      (LSelection.Count > LLimits.MaximumCells div ACellsPerRow) then
    begin
      raise ENyxCollectionClipboard.Create('Collection copy exceeds its output row/cell budget');
    end;
    Inc(LScanCount, LSourceCount);
  end
  else
  begin

    if LPage = nil then
    begin
      raise ENyxCollectionClipboard.Create('Selected-page copy requires its explicit accepted page');
    end;

    if (LPage.Key.Name <> LKey.Name) or (LPage.Revision <> LRevision) or
      not LSchema.SameSchema(LPage.Schema) or (LPage.Count < 0) or
      (LPage.Count > LSourceCount) then
    begin
      raise ENyxCollectionClipboard.Create('Collection copy page does not match its source schema/revision');
    end;
    Inc(LScanCount, LPage.Count);
  end;

  if LScanCount > LLimits.MaximumScannedRows then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy exceeds its row-inspection budget');
  end;
  for LIndex := 0 to LSelection.Count - 1 do
  begin
    LRef := LSelection.ItemAt(LIndex);

    if not LSource.Has(LRef) then
    begin
      raise ENyxCollectionClipboard.Create('Selected copy row is absent from its accepted source');
    end;
  end;
  LResult := Default(TNyxCollectionCopyMembers);
  LCapacity := LLimits.MaximumCells div ACellsPerRow;

  if LCapacity > LLimits.MaximumRows then
  begin
    LCapacity := LLimits.MaximumRows;
  end;

  if LCapacity > LSelection.Count then
  begin
    LCapacity := LSelection.Count;
  end;
  SetLength(LResult.FItems, LCapacity);
  LRow := 0;

  if AMembership = nccmAllSelectedSource then
  begin
    for LIndex := 0 to LSourceCount - 1 do
    begin
      LItem := LSource.ItemAt(LIndex);

      if LSelection.Contains(LItem.Ref) then
      begin

        if LRow >= LCapacity then
        begin
          raise ENyxCollectionClipboard.Create('Collection copy exceeds its output row/cell budget');
        end;
        LResult.FItems[LRow] := LItem.Copy;
        Inc(LRow);
      end;
    end;
  end
  else
  begin
    LSeen := nil;
    SetLength(LSeen, LSourceCount);
    for LIndex := 0 to LPage.Count - 1 do
    begin
      LItem := LPage.ItemAt(LIndex);
      LPosition := LSource.IndexOf(LItem.Ref);

      if (LPosition < 0) or (LPosition >= LSourceCount) then
      begin
        raise ENyxCollectionClipboard.Create('Collection copy page contains a foreign/missing source row');
      end;

      if LSeen[LPosition] or not LItem.SameItem(LSource.Item(LItem.Ref)) then
      begin
        raise ENyxCollectionClipboard.Create('Collection copy page repeats or changes an accepted source row');
      end;
      LSeen[LPosition] := True;

      if LSelection.Contains(LItem.Ref) then
      begin

        if LRow >= LCapacity then
        begin
          raise ENyxCollectionClipboard.Create('Collection copy exceeds its output row/cell budget');
        end;
        LResult.FItems[LRow] := LItem.Copy;
        Inc(LRow);
      end;
    end;
  end;

  if (LRow = 0) or (LRow > LLimits.MaximumRows) or
    (LRow > LLimits.MaximumCells div ACellsPerRow) then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy has no selected output rows or exceeds its cell budget');
  end;
  SetLength(LResult.FItems, LRow);
  LResult.FKey := LKey;
  LResult.FRevision := LRevision;
  Result := LResult;
end;

function NewNyxCollectionClipboardRows(const ASource: INyxCollectionSnapshot;
  const ASelection: TNyxCollectionSelectionSnapshot;
  const AFields: array of TNyxColumnIdentity;
  AMembership: TNyxCollectionCopyMembership;
  const APage: INyxCollectionSnapshot; AHeaders: Boolean;
  ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionClipboardText;
var
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LSelection: TNyxCollectionSelectionSnapshot;
  LFields: TNyxCollectionClipboardFields;
  LLimits: TNyxCollectionClipboardLimits;
  LSchema: TNyxCollectionSchema;
  LKey: TNyxCollectionRef;
  LRevision: Integer;
  LSourceCount: Integer;
  LIndex: Integer;
  LField: Integer;
  LOther: Integer;
  LRow: Integer;
  LPosition: Integer;
  LRows: TNyxCollectionCopyMembers;
  LItem: TNyxCollectionItem;
  LPayload: TClipboardText;
  LParts: TNyxStrings;
  LLine: TNyxText;
begin
  { Copy ALL caller-owned values/open arrays before the first snapshot getter.
    Selection's private record vectors are immutable after canonical capture. }
  LSource := ASource;
  LPage := APage;
  LSelection := ASelection;
  LLimits := ALimits;
  LLimits.Validate;

  if (Length(AFields) = 0) or (Length(AFields) > LLimits.MaximumFields) or
    (Length(AFields) > LLimits.MaximumCells) then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy requires bounded explicit fields');
  end;
  LFields := nil;
  SetLength(LFields, Length(AFields));
  for LIndex := 0 to High(AFields) do
  begin
    LFields[LIndex] := CopyField(AFields[LIndex]);
  end;
  AdmitSource(LSource, LLimits, LKey, LRevision, LSchema, LSourceCount);

  if (Ord(AMembership) < Ord(Low(TNyxCollectionCopyMembership))) or
    (Ord(AMembership) > Ord(High(TNyxCollectionCopyMembership))) or
    (Ord(ALineEnding) < Ord(Low(TNyxCollectionCopyLineEnding))) or
    (Ord(ALineEnding) > Ord(High(TNyxCollectionCopyLineEnding))) then
  begin
    raise ENyxCollectionClipboard.Create('Unknown collection copy membership or line ending');
  end;

  if not LSelection.Defined or (LSelection.Key.Name <> LKey.Name) or
    (LSelection.DataRevision <> LRevision) or (LSelection.Count = 0) or
    (LSelection.Count > NyxMaximumCollectionItems) then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy selection is missing, stale or exceeds its row budget');
  end;

  for LField := 0 to High(LFields) do
  begin
    CheckField(LSchema, LFields[LField]);
    for LOther := 0 to LField - 1 do
    begin

      if LFields[LField].Same(LFields[LOther]) then
      begin
        raise ENyxCollectionClipboard.Create('Collection copy fields contain a duplicate');
      end;
    end;
  end;
  LRows := CaptureNyxCollectionCopyMembers(LSource, LSelection, AMembership,
    LPage, Length(LFields), LLimits);

  if (LRows.Key.Name <> LKey.Name) or (LRows.Revision <> LRevision) then
  begin
    raise ENyxCollectionClipboard.Create('Copied output membership changed source identity/revision');
  end;
  LRow := LRows.Count;
  LPayload := TClipboardText.Create;
  try
    LPayload.FKey := LKey;
    LPayload.FRevision := LRevision;
    LPayload.FFields := LFields;
    SetLength(LPayload.FItems, LRow);
    SetLength(LPayload.FValues, LRow * Length(LFields));
    LParts := TNyxStrings.Create;
    try
      LLine := #10;

      if ALineEnding = nccleCRLF then
      begin
        LLine := #13#10;
      end;

      if AHeaders then
      begin
        for LField := 0 to High(LFields) do
        begin

          if LField > 0 then
          begin
            AppendPart(LParts, #9, LPayload.FBytes, LLimits.MaximumUTF8Bytes);
          end;
          AppendTSV(LParts, LFields[LField].Name, LPayload.FBytes, LLimits.MaximumUTF8Bytes);
        end;
        AppendPart(LParts, LLine, LPayload.FBytes, LLimits.MaximumUTF8Bytes);
      end;
      for LIndex := 0 to LRow - 1 do
      begin

        if LIndex > 0 then
        begin
          AppendPart(LParts, LLine, LPayload.FBytes, LLimits.MaximumUTF8Bytes);
        end;
        LItem := LRows.ItemAt(LIndex);
        LPayload.FItems[LIndex] := NyxItem(LKey, LItem.Ref.ID);
        for LField := 0 to High(LFields) do
        begin

          if LField > 0 then
          begin
            AppendPart(LParts, #9, LPayload.FBytes, LLimits.MaximumUTF8Bytes);
          end;
          LPosition := LIndex * Length(LFields) + LField;
          LPayload.FValues[LPosition] := ReadValue(LItem, LFields[LField]);
          AppendTSV(LParts, ScalarText(LPayload.FValues[LPosition]), LPayload.FBytes,
            LLimits.MaximumUTF8Bytes);
        end;
      end;
      LPayload.FText := LParts.Join;
    finally
      LParts.Free;
    end;
    Result := LPayload;
  except
    LPayload.Free;
    raise;
  end;
end;

function TClipboardText.GetText: TNyxText;
begin
  Result := FText;
end;

function TClipboardText.GetByteCount: Integer;
begin
  Result := FBytes;
end;

function TClipboardText.GetKey: TNyxCollectionRef;
begin
  Result := NyxCollection(FKey.Name);
end;

function TClipboardText.GetRevision: Integer;
begin
  Result := FRevision;
end;

function TClipboardText.GetRowCount: Integer;
begin
  Result := Length(FItems);
end;

function TClipboardText.GetFieldCount: Integer;
begin
  Result := Length(FFields);
end;

function TClipboardText.ItemAt(AIndex: Integer): TNyxItemRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FItems)) then
  begin
    raise ENyxCollectionClipboard.Create('Copied row index is outside the captured payload');
  end;
  Result := NyxItem(FKey, FItems[AIndex].ID);
end;

function TClipboardText.FieldAt(AIndex: Integer): TNyxColumnIdentity;
begin

  if (AIndex < 0) or (AIndex >= Length(FFields)) then
  begin
    raise ENyxCollectionClipboard.Create('Copied field index is outside the captured payload');
  end;
  Result := CopyField(FFields[AIndex]);
end;

function TClipboardText.CellValue(ARow: Integer; AField: Integer): TNyxStateValue;
begin

  if (ARow < 0) or (ARow >= Length(FItems)) or
    (AField < 0) or (AField >= Length(FFields)) then
  begin
    raise ENyxCollectionClipboard.Create('Copied cell is outside the captured typed payload');
  end;
  Result := FValues[ARow * Length(FFields) + AField].Copy;
end;

function WriteNyxCollectionClipboard(const APayload: INyxCollectionClipboardText;
  const AHost: INyxClipboardWriteHost; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
var
  LPayload: INyxCollectionClipboardText;
  LHost: INyxClipboardWriteHost;
  LScheduler: INyxScheduler;
  LScope: INyxCancellationScope;
  LReply: TNyxClipboardWriteReply;
  LText: TNyxText;
begin
  LPayload := APayload;
  LHost := AHost;
  LScheduler := AScheduler;
  LScope := ACancellation;
  LReply := AReply;

  if (LPayload = nil) or (LHost = nil) then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy handoff requires captured text and an explicit host');
  end;
  LText := LPayload.Text;
  NyxClipboardTextByteCount(LText);
  Result := LHost.BeginWriteText(LText, LScheduler, LScope, LReply);
end;

end.
