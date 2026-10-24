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
unit nyx.collections.range.navigation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections, nyx.collections.arrangement, nyx.collections.clipboard,
  nyx.collections.range, nyx.collections.range.actions, nyx.publication;

type
  { Already-admitted key intentions. Extend actions require Shift; first/last
    additionally mean Control/Meta+Home/End. Host adapters retain ordinary
    arrows, editors and default-off behavior. Escape clears an existing range
    rather than manufacturing a singleton or selecting a row. }
  TNyxCollectionRangeKey = (ncrkExtendLeft, ncrkExtendRight, ncrkExtendUp,
    ncrkExtendDown, ncrkExtendRowStart, ncrkExtendRowEnd, ncrkExtendFirstCell,
    ncrkExtendLastCell, ncrkClear);

  { Single-use mounted key envelope. Entry reserves original input/epoch before
    any item getter. ResolveOrigin supplies that original physical cell once;
    Prepare admits the full candidate before canonical callbacks. Complete
    claims stale/consumed input too, but never captures a replacement selection.
    All methods run on the mounted UI owner; Current is not a detached worker
    cancellation token. No envelope is retained by its mount or transferred to
    a clipboard host. }
  INyxCollectionCellRangeKeyCommand = interface(IInterface)
    ['{99450DDD-F090-4969-83F0-1395BE5A508E}']
    procedure ResolveOrigin(const AOrigin: TNyxCollectionCellIdentity);
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
    property Current: Boolean read GetCurrent;
    property DispatchCurrent: Boolean read GetDispatchCurrent;
  end;

  { Optional actual-mount input reserves the same envelope used by target keys.
    Callers remain on the UI owner and finish canonical dispatch synchronously;
    the envelope may retain its original mount only until Retire. Complete is
    single-use even when consumed, stale or interrupted by focus callbacks.
    Default-off mounts return nil and never reinterpret editor/row input. }
  INyxCollectionCellRangeKeyInput = interface(IInterface)
    ['{5E9E72AA-29DC-43CB-B84F-6620AE570B37}']
    function BeginCellRangeKey(AKey: TNyxCollectionRangeKey): INyxCollectionCellRangeKeyCommand;
  end;

  { Immutable movement proposal in one original page/presentation. It retains
    immutable snapshots and the previous range's cancellation/data token only,
    never a live view, selection controller, mount, editor, router or host.
    No mutable vector is exposed. A proposal is NOT publication authority:
    PrepareNavigation must admit its full RAW candidate before canonical
    callbacks, and the mounted input must also retain its original input/epoch,
    source/page/column and event-generation fences through final installation. }
  INyxCollectionRangeNavigation = interface(IInterface)
    ['{89D4568C-A704-4F13-AD99-8CAD627B1045}']
    function GetKey: TNyxCollectionRangeKey;
    function GetSource: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetPrevious: INyxCollectionCellRangeIntent;
    function GetOrigin: TNyxCollectionCellIdentity;
    function GetAnchor: TNyxCollectionCellIdentity;
    function GetFocus: TNyxCollectionCellIdentity;
    function GetColumnSerial: Integer;
    function GetFieldCount: Integer;
    function FieldAt(AIndex: Integer): TNyxColumnIdentity;
    function GetIncludeHeaders: Boolean;
    function GetLineEnding: TNyxCollectionCopyLineEnding;
    function GetLimits: TNyxCollectionClipboardLimits;
    property Key: TNyxCollectionRangeKey read GetKey;
    property Source: INyxCollectionSnapshot read GetSource;
    property Page: INyxCollectionSnapshot read GetPage;
    property Previous: INyxCollectionCellRangeIntent read GetPrevious;
    property Origin: TNyxCollectionCellIdentity read GetOrigin;
    property Anchor: TNyxCollectionCellIdentity read GetAnchor;
    property Focus: TNyxCollectionCellIdentity read GetFocus;
    property ColumnSerial: Integer read GetColumnSerial;
    property FieldCount: Integer read GetFieldCount;
    property IncludeHeaders: Boolean read GetIncludeHeaders;
    property LineEnding: TNyxCollectionCopyLineEnding read GetLineEnding;
    property Limits: TNyxCollectionClipboardLimits read GetLimits;
  end;

  { Optional selection-owner facet preserves the original selection ABI. Entry
    reserves its own request before proposal getters and admits a complete RAW
    rectangle without replacing the accepted selection. A newer Select/Clear,
    preparation or Revoke defeats that original request. Validate/final readiness
    must remain callback-free after construction; Install only adopts prepared
    data and retires the old canonical flag. Abandonment preserves the previous
    accepted selection. The returned preparation retains the selection owner,
    which must never retain its preparation. }
  INyxCollectionCellRangePreparation = interface(IInterface)
    ['{EAF09031-BF3B-49FA-80B3-6FD8952DBA71}']
    function PrepareNavigation(const ANavigation: INyxCollectionRangeNavigation): INyxPreparedPublication;
  end;

{ Resolve a typed movement using MoveNyxGridCell, exact page order and visible
  typed fields. All borrowed records/arrays are copied before snapshot getters.
  Existing range continuation requires its original source/page/serial and exact
  focused endpoint; stale/off-page/hidden identities never fall back to newer
  coordinates. Bounds clamp without wrapping; anchor direction is retained.

  This bounded proposal uses at most three indexed identities/row reads after
  checking page cardinality. Its subsequent prepared selection uses the existing
  complete range/SelectedPage admission for exact scalars, work/output/UTF-8
  budgets and TSV. Escape needs an active original range but reads no rows and
  permits nil source/page/empty fields: cancellation must not depend on a value
  formatter, clipboard availability or a fresh dataset getter. }
function NewNyxCollectionRangeNavigation(const ASource, APage: INyxCollectionSnapshot;
  const AVisibleFields: array of TNyxColumnIdentity;
  const AOrigin: TNyxCollectionCellIdentity;
  const APrevious: INyxCollectionCellRangeIntent; AKey: TNyxCollectionRangeKey;
  AColumnSerial: Integer; AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionRangeNavigation;

implementation

uses SysUtils, nyx.collections.grid, nyx.state;

type
  { Private admitted movement stores no callback. Snapshot/cancel leases are
    immutable original facts; arrays are copied and exposed only by value. }
  TNavigation = class(TInterfacedObject, INyxCollectionRangeNavigation)
  private
    FKey: TNyxCollectionRangeKey;
    FSource: INyxCollectionSnapshot;
    FPage: INyxCollectionSnapshot;
    FPrevious: INyxCollectionCellRangeIntent;
    FOrigin: TNyxCollectionCellIdentity;
    FAnchor: TNyxCollectionCellIdentity;
    FFocus: TNyxCollectionCellIdentity;
    FSerial: Integer;
    FFields: TNyxCollectionClipboardFields;
    FHeaders: Boolean;
    FLines: TNyxCollectionCopyLineEnding;
    FLimits: TNyxCollectionClipboardLimits;
  public
    function GetKey: TNyxCollectionRangeKey;
    function GetSource: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetPrevious: INyxCollectionCellRangeIntent;
    function GetOrigin: TNyxCollectionCellIdentity;
    function GetAnchor: TNyxCollectionCellIdentity;
    function GetFocus: TNyxCollectionCellIdentity;
    function GetColumnSerial: Integer;
    function GetFieldCount: Integer;
    function FieldAt(AIndex: Integer): TNyxColumnIdentity;
    function GetIncludeHeaders: Boolean;
    function GetLineEnding: TNyxCollectionCopyLineEnding;
    function GetLimits: TNyxCollectionClipboardLimits;
  end;

function NewNyxCollectionRangeNavigation(const ASource, APage: INyxCollectionSnapshot;
  const AVisibleFields: array of TNyxColumnIdentity;
  const AOrigin: TNyxCollectionCellIdentity;
  const APrevious: INyxCollectionCellRangeIntent; AKey: TNyxCollectionRangeKey;
  AColumnSerial: Integer; AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionRangeNavigation;
var
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LPrevious: INyxCollectionCellRangeIntent;
  LCapture: INyxCollectionCellRangeCapture;
  LFields: TNyxCollectionClipboardFields;
  LOrigin: TNyxCollectionCellIdentity;
  LAnchor: TNyxCollectionCellIdentity;
  LFocus: TNyxCollectionCellIdentity;
  LLimits: TNyxCollectionClipboardLimits;
  LSchema: TNyxCollectionSchema;
  LPageSchema: TNyxCollectionSchema;
  LCollection: TNyxCollectionRef;
  LRevision: Integer;
  LSourceCount: Integer;
  LCount: Integer;
  LRow: Integer;
  LColumn: Integer;
  LIndex: Integer;
  LOther: Integer;
  LCheck: Integer;
  LMove: TNyxGridMove;
  LDestination: TNyxGridCell;
  LItem: TNyxItemRef;
  LNavigation: TNavigation;
  LLease: INyxCollectionRangeNavigation;

  procedure RequirePrevious;
  begin

    if (LPrevious <> nil) and not LPrevious.Current then
    begin
      raise ENyxCollectionRangeStale.Create('Original cell range retired during movement capture');
    end;
  end;

  function PageRow(const AItem: TNyxItemRef): Integer;
  var
    LActual: TNyxItemRef;
  begin
    Result := LPage.IndexOf(AItem);

    if (Result < 0) or (Result >= LCount) then
    begin
      raise ENyxCollectionRange.Create('Range movement endpoint is absent from its actual page');
    end;
    LActual := LPage.ItemAt(Result).Ref;

    if (LActual.Collection.Name <> AItem.Collection.Name) or (LActual.ID <> AItem.ID) then
    begin
      raise ENyxCollectionRange.Create('Range movement page position does not match its exact item');
    end;
  end;

begin
  LSource := ASource;
  LPage := APage;
  LPrevious := APrevious;
  LLimits := ALimits;
  LOrigin := Default(TNyxCollectionCellIdentity);

  if (Length(AVisibleFields) > NyxMaximumCollectionFields) or
    ((AKey <> ncrkClear) and (Length(AVisibleFields) < 1)) then
  begin
    raise ENyxCollectionRange.Create('Range movement visible fields exceed the bounded column count');
  end;

  if AOrigin.Defined then
  begin
    LOrigin := NyxCollectionCellIdentity(AOrigin.Item, AOrigin.Field);
  end;
  SetLength(LFields, Length(AVisibleFields));
  for LIndex := 0 to High(LFields) do
  begin
    { These records have private immutable scalar/name fields. Validate and
      copy by value; no JSON re-admission belongs on each repeated arrow key. }
    AVisibleFields[LIndex].Validate;
    LFields[LIndex] := AVisibleFields[LIndex];
  end;

  if (AColumnSerial < 0) or (AColumnSerial >= High(Integer)) then
  begin
    raise ENyxCollectionRange.Create('Range movement requires its original column serial');
  end;
  RequirePrevious;
  LAnchor := LOrigin;
  LFocus := LOrigin;

  if LPrevious <> nil then
  begin
    LCapture := LPrevious.Capture;

    if (LCapture = nil) or (LCapture.ColumnSerial <> AColumnSerial) then
    begin
      raise ENyxCollectionRangeStale.Create('Range movement column observation is stale');
    end;
    LAnchor := LCapture.Anchor;
    LFocus := LCapture.Focus;
    RequirePrevious;
  end;

  if AKey = ncrkClear then
  begin

    if LPrevious = nil then
    begin
      raise ENyxCollectionRange.Create('Escape requires an original selected cell range');
    end;
  end
  else
  begin
    LLimits.Validate;

    if (LSource = nil) or (LPage = nil) or not LOrigin.Defined or
      (Length(LFields) < 1) or (Length(LFields) > NyxMaximumCollectionFields) or
      not (ALineEnding in [nccleLF, nccleCRLF]) then
    begin
      raise ENyxCollectionRange.Create('Range movement requires bounded original page/visible-cell facts');
    end;

    if (LPrevious <> nil) and ((LPrevious.Source <> LSource) or
      (LPrevious.Page <> LPage) or not LOrigin.Same(LFocus)) then
    begin
      raise ENyxCollectionRangeStale.Create('Range movement cannot retarget a different page or focused cell');
    end;
    LCollection := NyxCollection(LSource.Key.Name);
    LRevision := LSource.Revision;
    LSchema := LSource.Schema.Copy;
    LSchema.Validate;
    LPageSchema := LPage.Schema;
    LSourceCount := LSource.Count;
    LCount := LPage.Count;

    if (LRevision < 0) or (LSourceCount < 1) or
      (LSourceCount > NyxMaximumCollectionItems) or (LCount < 1) or
      (LCount > LSourceCount) or (LCount > LLimits.MaximumScannedRows) or
      (LPage.Key.Name <> LCollection.Name) or (LPage.Revision <> LRevision) or
      not LSchema.SameSchema(LPageSchema) or
      (LOrigin.Item.Collection.Name <> LCollection.Name) or
      (LAnchor.Item.Collection.Name <> LCollection.Name) then
    begin
      raise ENyxCollectionRange.Create('Range movement page/schema/revision does not match its source');
    end;
    LColumn := -1;
    LOther := -1;
    for LIndex := 0 to High(LFields) do
    begin
      for LCheck := 0 to LIndex - 1 do
      begin

        if LFields[LIndex].Same(LFields[LCheck]) then
        begin
          raise ENyxCollectionRange.Create('Range movement visible fields contain a duplicate');
        end;
      end;
      case LFields[LIndex].Kind of
        nskText:
          begin
            LSchema.Definition(NyxTextField(LFields[LIndex].Name));
          end;
        nskBoolean:
          begin
            LSchema.Definition(NyxBooleanField(LFields[LIndex].Name));
          end;
        nskInteger:
          begin
            LSchema.Definition(NyxIntegerField(LFields[LIndex].Name));
          end;
        nskNumber:
          begin
            LSchema.Definition(NyxNumberField(LFields[LIndex].Name));
          end;
      end;

      if LFields[LIndex].Same(LOrigin.Field) then
      begin
        LColumn := LIndex;
      end;

      if LFields[LIndex].Same(LAnchor.Field) then
      begin
        LOther := LIndex;
      end;
    end;

    if (LColumn < 0) or (LOther < 0) then
    begin
      raise ENyxCollectionRange.Create('Range movement endpoint is not a visible typed field');
    end;
    LRow := PageRow(LOrigin.Item);
    PageRow(LAnchor.Item);
    case AKey of
      ncrkExtendLeft:
        begin
          LMove := ngmLeft;
        end;
      ncrkExtendRight:
        begin
          LMove := ngmRight;
        end;
      ncrkExtendUp:
        begin
          LMove := ngmUp;
        end;
      ncrkExtendDown:
        begin
          LMove := ngmDown;
        end;
      ncrkExtendRowStart:
        begin
          LMove := ngmRowStart;
        end;
      ncrkExtendRowEnd:
        begin
          LMove := ngmRowEnd;
        end;
      ncrkExtendFirstCell:
        begin
          LMove := ngmFirstCell;
        end;
      ncrkExtendLastCell:
        begin
          LMove := ngmLastCell;
        end;
    else
      raise ENyxCollectionRange.Create('Unsupported cell range movement');
    end;
    LDestination := MoveNyxGridCell(LRow, LColumn, LCount, Length(LFields), LMove);
    LItem := LPage.ItemAt(LDestination.Row).Ref;

    if LItem.Collection.Name <> LCollection.Name then
    begin
      raise ENyxCollectionRange.Create('Range movement destination has a foreign collection scope');
    end;
    LFocus := NyxCollectionCellIdentity(LItem, LFields[LDestination.Column]);
    RequirePrevious;
  end;
  LNavigation := TNavigation.Create;
  LLease := LNavigation;
  LNavigation.FKey := AKey;
  LNavigation.FSource := LSource;
  LNavigation.FPage := LPage;
  LNavigation.FPrevious := LPrevious;
  LNavigation.FOrigin := LOrigin;
  LNavigation.FAnchor := LAnchor;
  LNavigation.FFocus := LFocus;
  LNavigation.FSerial := AColumnSerial;
  LNavigation.FFields := LFields;
  LNavigation.FHeaders := AHeaders;
  LNavigation.FLines := ALineEnding;
  LNavigation.FLimits := LLimits;
  { Final-only assignment preserves a caller's previous proposal on refusal. }
  Result := LLease;
end;

function TNavigation.GetKey: TNyxCollectionRangeKey;
begin
  Result := FKey;
end;

function TNavigation.GetSource: INyxCollectionSnapshot;
begin
  Result := FSource;
end;

function TNavigation.GetPage: INyxCollectionSnapshot;
begin
  Result := FPage;
end;

function TNavigation.GetPrevious: INyxCollectionCellRangeIntent;
begin
  Result := FPrevious;
end;

function TNavigation.GetOrigin: TNyxCollectionCellIdentity;
begin
  Result := FOrigin;
end;

function TNavigation.GetAnchor: TNyxCollectionCellIdentity;
begin
  Result := FAnchor;
end;

function TNavigation.GetFocus: TNyxCollectionCellIdentity;
begin
  Result := FFocus;
end;

function TNavigation.GetColumnSerial: Integer;
begin
  Result := FSerial;
end;

function TNavigation.GetFieldCount: Integer;
begin
  Result := Length(FFields);
end;

function TNavigation.FieldAt(AIndex: Integer): TNyxColumnIdentity;
begin

  if (AIndex < 0) or (AIndex >= Length(FFields)) then
  begin
    raise ENyxCollectionRange.Create('Range navigation field index is outside its captured vector');
  end;
  Result := FFields[AIndex];
end;

function TNavigation.GetIncludeHeaders: Boolean;
begin
  Result := FHeaders;
end;

function TNavigation.GetLineEnding: TNyxCollectionCopyLineEnding;
begin
  Result := FLines;
end;

function TNavigation.GetLimits: TNyxCollectionClipboardLimits;
begin
  Result := FLimits;
end;

end.
