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
unit nyx.collections.range.pointer;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections, nyx.collections.arrangement, nyx.collections.clipboard,
  nyx.collections.range, nyx.collections.range.actions;

type
  { Endpoint intent is independent of a device or physical coordinate. Replace
    starts at the explicit launch cell; Extend keeps a compatible existing
    anchor, or starts at the launch cell when no range exists. Neither choice
    changes ordinary row selection or grants permission to intercept editors. }
  TNyxCollectionRangePointerAction = (ncrpReplace, ncrpExtend);

  { Immutable original endpoint proposal, not mounted input authority. It owns
    copied fields/identities and immutable snapshots plus an old range token;
    no view, store, mount, widget, event router or selection owner is retained.
    Focus is the exact supplied target, never a keyboard movement approximation.
    A target must separately retain its original input/epoch/event generation
    through canonical dispatch and focus before completing a prepared command. }
  INyxCollectionRangePointer = interface(IInterface)
    ['{EF62D1F6-7578-4367-A45E-AF9FC2877741}']
    function Copy: INyxCollectionRangePointer;
    function GetDefined: Boolean;
    function GetAction: TNyxCollectionRangePointerAction;
    function GetSource: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetPrevious: INyxCollectionCellRangeIntent;
    function GetOrigin: TNyxCollectionCellIdentity;
    function GetTarget: TNyxCollectionCellIdentity;
    function GetAnchor: TNyxCollectionCellIdentity;
    function GetFieldCount: Integer;
    function FieldAt(AIndex: Integer): TNyxColumnIdentity;
    function GetColumnSerial: Integer;
    function GetIncludeHeaders: Boolean;
    function GetLineEnding: TNyxCollectionCopyLineEnding;
    function GetLimits: TNyxCollectionClipboardLimits;
    property Defined: Boolean read GetDefined;
    property Action: TNyxCollectionRangePointerAction read GetAction;
    property Source: INyxCollectionSnapshot read GetSource;
    property Page: INyxCollectionSnapshot read GetPage;
    property Previous: INyxCollectionCellRangeIntent read GetPrevious;
    property Origin: TNyxCollectionCellIdentity read GetOrigin;
    property Target: TNyxCollectionCellIdentity read GetTarget;
    property Anchor: TNyxCollectionCellIdentity read GetAnchor;
    property Focus: TNyxCollectionCellIdentity read GetTarget;
    property FieldCount: Integer read GetFieldCount;
    property ColumnSerial: Integer read GetColumnSerial;
    property IncludeHeaders: Boolean read GetIncludeHeaders;
    property LineEnding: TNyxCollectionCopyLineEnding read GetLineEnding;
    property Limits: TNyxCollectionClipboardLimits read GetLimits;
  end;
  { Synchronous UI-owner preparation on one original selection generation.
    Preparation has already admitted the COMPLETE existing RAW rectangle but
    changes no accepted range. Complete claims its first call before checking
    permission/currentness: consumed, stale and retired commands cannot retry.
    True means this one original candidate installed; False publishes nothing.
    Retire abandons an uninstalled candidate and preserves the old selection.
    It cancels no physical pointer or clipboard effect. Retained Proposal data
    stays safe after retirement and carries no fresh mounted authority. }
  INyxCollectionCellRangePointerCommand = interface(IInterface)
    ['{25D61C25-3697-4F73-ACD7-652780894E9C}']
    function GetProposal: INyxCollectionRangePointer;
    function GetCurrent: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
    property Proposal: INyxCollectionRangePointer read GetProposal;
    property Current: Boolean read GetCurrent;
  end;

  { Optional existing selection-owner facet; original selection/navigation
    interfaces and GUIDs remain unchanged. Entry reserves its request BEFORE
    snapshot getters and compares the supplied original previous token. Nested
    Clear/Select/Revoke or a newer preparation defeats the older request even
    when both accepted selections are nil. The target reserves original input
    before collecting these arguments and must finish its canonical event
    synchronously; this portable facet alone cannot admit physical input. }
  INyxCollectionCellRangePointerPreparation = interface(IInterface)
    ['{9E52FB19-456E-4B2B-9B82-5BB0ABBB5EC3}']
    function PreparePointer(const ASource, APage: INyxCollectionSnapshot;
      const AVisibleFields: array of TNyxColumnIdentity;
      const AOrigin, ATarget: TNyxCollectionCellIdentity;
      const APrevious: INyxCollectionCellRangeIntent;
      AAction: TNyxCollectionRangePointerAction; AColumnSerial: Integer;
      AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
      const ALimits: TNyxCollectionClipboardLimits): INyxCollectionCellRangePointerCommand;
  end;

  { Single-use mounted pointer envelope, reserved BEFORE hit/source/page/field
    getters or native focus. ResolveCells supplies the original physical focus
    and endpoint once; Prepare admits the complete RAW rectangle before one
    canonical pointer dispatch. Complete claims consumed/stale input too (True
    means claimed, not installed), and repeats original input/epoch fences after
    target focus. All calls are synchronous on the UI owner. Retire releases its
    mount lease; neither the mount nor a detached accepted range retains it. }
  INyxCollectionCellRangePointerEntry = interface(IInterface)
    ['{48A71853-E21C-467B-B1D2-22C72CC56866}']
    procedure ResolveCells(const AOrigin, ATarget: TNyxCollectionCellIdentity);
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
    property Current: Boolean read GetCurrent;
    property DispatchCurrent: Boolean read GetDispatchCurrent;
  end;

  { Optional actual-mount pointer reservation shares the original range owner
    and request serial with keyboard navigation. Default-off mounts return nil;
    an admitted but subsequently stale envelope remains claimed and never falls
    through to a fresh row/editor default. This facet grants no global pointer
    interception or asynchronous gesture authority. }
  INyxCollectionCellRangePointerInput = interface(IInterface)
    ['{0E99247C-CF62-4AD3-AAC9-8B7A5C8F4536}']
    function BeginCellRangePointer(AAction: TNyxCollectionRangePointerAction):
      INyxCollectionCellRangePointerEntry;
  end;

{ Copy all records/fields and retain snapshots before outward getters. At most
  three indexed page rows are checked after bounded page cardinality; this
  proposal performs no complete output scan. The prepared selection subsequently
  uses NewNyxCollectionCellRange and its existing SelectedPage source-membership,
  schema, scan/cell/byte budgets and exact TSV admission. Hidden/off-page fields
  and endpoints refuse; a previous extension must have the SAME page/source,
  serial and original focused endpoint. Refusal preserves the caller's value. }
function NewNyxCollectionRangePointer(const ASource, APage: INyxCollectionSnapshot;
  const AVisibleFields: array of TNyxColumnIdentity;
  const AOrigin, ATarget: TNyxCollectionCellIdentity;
  const APrevious: INyxCollectionCellRangeIntent;
  AAction: TNyxCollectionRangePointerAction; AColumnSerial: Integer;
  AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionRangePointer;

implementation

uses nyx.state;

type
  { Private immutable endpoint facts. The interface shell is portable to pas2js,
    which cannot store COM interfaces inside a record. Copies share only private
    read-only data; no mutator or borrowed field array crosses this boundary. }
  TPointer = class(TInterfacedObject, INyxCollectionRangePointer)
  private
    FDefined: Boolean;
    FAction: TNyxCollectionRangePointerAction;
    FSource: INyxCollectionSnapshot;
    FPage: INyxCollectionSnapshot;
    FPrevious: INyxCollectionCellRangeIntent;
    FOrigin: TNyxCollectionCellIdentity;
    FTarget: TNyxCollectionCellIdentity;
    FAnchor: TNyxCollectionCellIdentity;
    FFields: TNyxCollectionClipboardFields;
    FSerial: Integer;
    FHeaders: Boolean;
    FLines: TNyxCollectionCopyLineEnding;
    FLimits: TNyxCollectionClipboardLimits;
  public
    function Copy: INyxCollectionRangePointer;
    function GetDefined: Boolean;
    function GetAction: TNyxCollectionRangePointerAction;
    function GetSource: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetPrevious: INyxCollectionCellRangeIntent;
    function GetOrigin: TNyxCollectionCellIdentity;
    function GetTarget: TNyxCollectionCellIdentity;
    function GetAnchor: TNyxCollectionCellIdentity;
    function GetFieldCount: Integer;
    function FieldAt(AIndex: Integer): TNyxColumnIdentity;
    function GetColumnSerial: Integer;
    function GetIncludeHeaders: Boolean;
    function GetLineEnding: TNyxCollectionCopyLineEnding;
    function GetLimits: TNyxCollectionClipboardLimits;
  end;

function TPointer.GetFieldCount: Integer;
begin
  Result := Length(FFields);
end;

function TPointer.FieldAt(AIndex: Integer): TNyxColumnIdentity;
begin

  if (AIndex < 0) or (AIndex >= Length(FFields)) then
  begin
    raise ENyxCollectionRange.Create('Pointer range field is outside its captured vector');
  end;
  Result := FFields[AIndex];
end;

function TPointer.Copy: INyxCollectionRangePointer;
begin
  Result := Self;
end;

function TPointer.GetDefined: Boolean;
begin
  Result := FDefined;
end;

function TPointer.GetAction: TNyxCollectionRangePointerAction;
begin
  Result := FAction;
end;

function TPointer.GetSource: INyxCollectionSnapshot;
begin
  Result := FSource;
end;

function TPointer.GetPage: INyxCollectionSnapshot;
begin
  Result := FPage;
end;

function TPointer.GetPrevious: INyxCollectionCellRangeIntent;
begin
  Result := FPrevious;
end;

function TPointer.GetOrigin: TNyxCollectionCellIdentity;
begin
  Result := FOrigin;
end;

function TPointer.GetTarget: TNyxCollectionCellIdentity;
begin
  Result := FTarget;
end;

function TPointer.GetAnchor: TNyxCollectionCellIdentity;
begin
  Result := FAnchor;
end;

function TPointer.GetColumnSerial: Integer;
begin
  Result := FSerial;
end;

function TPointer.GetIncludeHeaders: Boolean;
begin
  Result := FHeaders;
end;

function TPointer.GetLineEnding: TNyxCollectionCopyLineEnding;
begin
  Result := FLines;
end;

function TPointer.GetLimits: TNyxCollectionClipboardLimits;
begin
  Result := FLimits;
end;
function NewNyxCollectionRangePointer(const ASource, APage: INyxCollectionSnapshot;
  const AVisibleFields: array of TNyxColumnIdentity;
  const AOrigin, ATarget: TNyxCollectionCellIdentity;
  const APrevious: INyxCollectionCellRangeIntent;
  AAction: TNyxCollectionRangePointerAction; AColumnSerial: Integer;
  AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionRangePointer;
var
  LValue: TPointer;
  LLease: INyxCollectionRangePointer;
  LCapture: INyxCollectionCellRangeCapture;
  LSchema: TNyxCollectionSchema;
  LPageSchema: TNyxCollectionSchema;
  LCollection: TNyxCollectionRef;
  LRevision: Integer;
  LSourceCount: Integer;
  LCount: Integer;
  LIndex: Integer;
  LOther: Integer;
  LOriginVisible: Boolean;
  LTargetVisible: Boolean;
  LAnchorVisible: Boolean;

  procedure RequirePrevious;
  begin

    if (LValue.FPrevious <> nil) and not LValue.FPrevious.Current then
    begin
      raise ENyxCollectionRangeStale.Create('Pointer range original selection retired during capture');
    end;
  end;

  procedure RequirePageCell(const ACell: TNyxCollectionCellIdentity);
  var
    LAt: Integer;
    LActual: TNyxItemRef;
  begin

    if ACell.Item.Collection.Name <> LCollection.Name then
    begin
      raise ENyxCollectionRange.Create('Pointer range endpoint has a foreign collection scope');
    end;
    LAt := LValue.FPage.IndexOf(ACell.Item);

    if (LAt < 0) or (LAt >= LCount) then
    begin
      raise ENyxCollectionRange.Create('Pointer range endpoint is outside the original page');
    end;
    LActual := LValue.FPage.ItemAt(LAt).Ref;

    if (LActual.Collection.Name <> ACell.Item.Collection.Name) or
      (LActual.ID <> ACell.Item.ID) then
    begin
      raise ENyxCollectionRange.Create('Pointer range page index does not match its exact item');
    end;
    RequirePrevious;
  end;

begin
  LValue := TPointer.Create;
  LLease := LValue;
  LValue.FSource := ASource;
  LValue.FPage := APage;
  LValue.FPrevious := APrevious;
  LValue.FLimits := ALimits;

  if not (AAction in [ncrpReplace, ncrpExtend]) or
    (Length(AVisibleFields) < 1) or (Length(AVisibleFields) > NyxMaximumCollectionFields) or
    not AOrigin.Defined or not ATarget.Defined or
    (AColumnSerial < 0) or (AColumnSerial >= High(Integer)) or
    not (ALineEnding in [nccleLF, nccleCRLF]) then
  begin
    raise ENyxCollectionRange.Create('Pointer range requires explicit bounded endpoint choices');
  end;
  LValue.FOrigin := NyxCollectionCellIdentity(AOrigin.Item, AOrigin.Field);
  LValue.FTarget := NyxCollectionCellIdentity(ATarget.Item, ATarget.Field);
  LValue.FAnchor := LValue.FOrigin;
  LValue.FAction := AAction;
  LValue.FSerial := AColumnSerial;
  LValue.FHeaders := AHeaders;
  LValue.FLines := ALineEnding;
  SetLength(LValue.FFields, Length(AVisibleFields));
  for LIndex := 0 to High(LValue.FFields) do
  begin
    AVisibleFields[LIndex].Validate;
    LValue.FFields[LIndex] := AVisibleFields[LIndex];
  end;
  LValue.FLimits.Validate;

  if (LValue.FSource = nil) or (LValue.FPage = nil) then
  begin
    raise ENyxCollectionRange.Create('Pointer range requires original source and page snapshots');
  end;
  RequirePrevious;

  if (AAction = ncrpExtend) and (LValue.FPrevious <> nil) then
  begin
    LCapture := LValue.FPrevious.Capture;

    if (LCapture = nil) or (LValue.FPrevious.Source <> LValue.FSource) or
      (LValue.FPrevious.Page <> LValue.FPage) or
      (LCapture.ColumnSerial <> AColumnSerial) or
      not LValue.FOrigin.Same(LCapture.Focus) then
    begin
      raise ENyxCollectionRangeStale.Create('Pointer extension cannot retarget another page or focused cell');
    end;
    LValue.FAnchor := LCapture.Anchor;
    RequirePrevious;
  end;
  LCollection := NyxCollection(LValue.FSource.Key.Name);
  LRevision := LValue.FSource.Revision;
  LSchema := LValue.FSource.Schema.Copy;
  LSchema.Validate;
  LPageSchema := LValue.FPage.Schema;
  LSourceCount := LValue.FSource.Count;
  LCount := LValue.FPage.Count;

  if (LRevision < 0) or (LSourceCount < 1) or (LSourceCount > NyxMaximumCollectionItems) or
    (LCount < 1) or (LCount > LSourceCount) or
    (LCount > LValue.FLimits.MaximumScannedRows) or
    (LValue.FPage.Key.Name <> LCollection.Name) or
    (LValue.FPage.Revision <> LRevision) or not LSchema.SameSchema(LPageSchema) then
  begin
    raise ENyxCollectionRange.Create('Pointer range page/schema/revision does not match its source');
  end;
  LOriginVisible := False;
  LTargetVisible := False;
  LAnchorVisible := False;
  for LIndex := 0 to High(LValue.FFields) do
  begin
    for LOther := 0 to LIndex - 1 do
    begin

      if LValue.FFields[LIndex].Name = LValue.FFields[LOther].Name then
      begin
        raise ENyxCollectionRange.Create('Pointer range visible fields contain a duplicate name');
      end;
    end;
    case LValue.FFields[LIndex].Kind of
      nskText:
        begin
          LSchema.Definition(NyxTextField(LValue.FFields[LIndex].Name));
        end;
      nskBoolean:
        begin
          LSchema.Definition(NyxBooleanField(LValue.FFields[LIndex].Name));
        end;
      nskInteger:
        begin
          LSchema.Definition(NyxIntegerField(LValue.FFields[LIndex].Name));
        end;
      nskNumber:
        begin
          LSchema.Definition(NyxNumberField(LValue.FFields[LIndex].Name));
        end;
    end;
    LOriginVisible := LOriginVisible or LValue.FFields[LIndex].Same(LValue.FOrigin.Field);
    LTargetVisible := LTargetVisible or LValue.FFields[LIndex].Same(LValue.FTarget.Field);
    LAnchorVisible := LAnchorVisible or LValue.FFields[LIndex].Same(LValue.FAnchor.Field);
  end;

  if not LOriginVisible or not LTargetVisible or not LAnchorVisible then
  begin
    raise ENyxCollectionRange.Create('Pointer range endpoint is not an original visible typed field');
  end;
  RequirePageCell(LValue.FOrigin);
  RequirePageCell(LValue.FTarget);
  RequirePageCell(LValue.FAnchor);
  RequirePrevious;
  LValue.FDefined := True;
  Result := LLease;
end;

end.
