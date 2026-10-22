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
unit nyx.collections.query.session;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text,
  nyx.data,
  nyx.collections,
  nyx.collections.query,
  nyx.collections.view.types,
  nyx.publication.types;

const
  NyxMaximumQuerySessionEdits = 64;
  NyxMaximumQuerySessionOwners = 64;
  NyxMaximumQuerySessionContributions = 64;

type
  { A copied display/diagnostic origin, never a behavioral switch or authority.
    Same-named incarnations in one generation receive different owner IDs.
    New generations may reuse numbers, but never an old command token's proof. }
  TNyxQueryOriginRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;

  { An admitted owner-local predicate slot, such as a search or column filter.
    Its open name identifies data; the query descriptor defines behavior. }
  TNyxQueryChannelRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;

  { A session-local copied identity. Default is undefined, not owner zero.
    Its number is meaningful only within a captured session generation; copied
    IDs remain data, never command authority over a new generation. }
  TNyxQueryOwnerID = record
  private
    FValue: Integer;
    function GetDefined: Boolean;
  public
    property Value: Integer read FValue;
    property Defined: Boolean read GetDefined;
  end;

  { Read-only snapshot presentation data, with no lease or receiver reference.
    Connected describes capture time; it cannot revive a revoked owner. }
  TNyxQueryOwnerInfo = record
    ID: TNyxQueryOwnerID; { Captured generation-local identity. }
    Origin: TNyxQueryOriginRef; { Exact diagnostic origin, not a dispatch key. }
    Caption: TNyxText; { Independently copied human-readable caption. }
    Connected: Boolean; { Liveness at capture time, not a live getter. }
  end;

  { One copied predicate and its owner/channel. Filter is a closed predicate
    descriptor, not a retained executable interface or a whole effective query. }
  TNyxQueryContribution = record
    Owner: TNyxQueryOwnerInfo; { Snapshot owner presentation, without a lease. }
    Channel: TNyxQueryChannelRef; { The exact owner-local predicate slot. }
    Filter: TNyxDataValue; { Closed predicate data; no sort or callback. }
  end;

  { Closed edit meanings. A batch is interpreted in order against one admitted
    session candidate and publishes once through the existing model phases. }
  TNyxQuerySessionEditKind = (nqseFilter, nqseClearFilter, nqseSortBy,
    nqseResetSort, nqseClearOwner, nqseClearAllFilters, nqseResetAll,
    nqseRemoveFilter);

  { Immutable batch member. Only the relevant typed fields are defined: Filter
    carries a filter-only Query, SortBy a sort-only Query, RemoveFilter an ID.
    These records contain no interfaces, preserving pas2js value ownership. }
  TNyxQuerySessionEdit = record
  private
    FKind: TNyxQuerySessionEditKind;
    FChannel: TNyxQueryChannelRef;
    FOwnerID: TNyxQueryOwnerID;
    FQuery: TNyxCollectionQuery;
    function GetQuery: TNyxCollectionQuery;
  public
    function Copy: TNyxQuerySessionEdit;
    property Kind: TNyxQuerySessionEditKind read FKind;
    property Channel: TNyxQueryChannelRef read FChannel;
    property OwnerID: TNyxQueryOwnerID read FOwnerID;
    property Query: TNyxCollectionQuery read GetQuery;
  end;

  { Independently owned, bounded fluent edits. Filter normalizes an alternative
    predicate immediately; later preparation consumes closed copied values.
    Scalar Search/Sort resets can join this query stage in PublishNyxGroup. }
  TNyxQuerySessionEdits = record
  private
    FEdits: array of TNyxQuerySessionEdit;
    function GetCount: Integer;
    function Add(const AEdit: TNyxQuerySessionEdit): TNyxQuerySessionEdits;
  public
    function Copy: TNyxQuerySessionEdits;
    function EditAt(AIndex: Integer): TNyxQuerySessionEdit;
    function Filter(const AChannel: TNyxQueryChannelRef;
      const APredicate: INyxCollectionPredicate): TNyxQuerySessionEdits;
    function ClearFilter(const AChannel: TNyxQueryChannelRef): TNyxQuerySessionEdits;
    { Empty SortBy explicitly selects unsorted source order. ResetSort instead
      withdraws the shared user sort and restores the admitted baseline sort. }
    function SortBy(const AQuery: TNyxCollectionQuery): TNyxQuerySessionEdits;
    function ResetSort: TNyxQuerySessionEdits;
    { Clear only this actor's predicates; reset the shared sort only when this
      exact actor ID owns it. Peer filters and a peer's sort remain intact. }
    function ClearOwner: TNyxQuerySessionEdits;
    function ClearAllFilters: TNyxQuerySessionEdits;
    function ResetAll: TNyxQuerySessionEdits;
    { Explicit chip/orphan withdrawal, checked against a fresh session capture.
      The copied ID is data; a live command owner and capture still authorize it. }
    function RemoveFilter(const AOwnerID: TNyxQueryOwnerID;
      const AChannel: TNyxQueryChannelRef): TNyxQuerySessionEdits;
    property Count: Integer read GetCount;
  end;

  { Scoped command authority for one view/session generation. Revoke and final
    release only revoke owned flags; neither changes a query nor rolls it back.
    Accepted predicates remain visible as orphans until an explicit edit removes
    them. The lease retains no document, widget or controller. }
  INyxCollectionQueryOwner = interface(IInterface)
    ['{AE1CE24A-A94F-4967-AE08-BFAAFBB34A6F}']
    function GetID: TNyxQueryOwnerID;
    function GetOrigin: TNyxQueryOriginRef;
    function GetCaption: TNyxText;
    function GetConnected: Boolean;
    procedure Revoke;
    property ID: TNyxQueryOwnerID read GetID;
    property Origin: TNyxQueryOriginRef read GetOrigin;
    property Caption: TNyxText read GetCaption;
    property Connected: Boolean read GetConnected;
  end;

  { Copied canonical metadata plus a private exact-session/intent proof. No
    source/store/view, command lease or old authority is retained. SortOwner is
    defined only with HasUserSort, including an explicit empty/unsorted choice. }
  INyxCollectionQuerySessionSnapshot = interface(IInterface)
    ['{C0668F18-128D-4A70-855B-B141A0BDB8A6}']
    function GetBaseline: TNyxCollectionQuery;
    function GetEffective: TNyxCollectionQuery;
    function GetHasUserSort: Boolean;
    function GetUserSort: TNyxCollectionQuery;
    function GetSortOwner: TNyxQueryOwnerInfo;
    function GetCount: Integer;
    function ContributionAt(AIndex: Integer): TNyxQueryContribution;
    property Baseline: TNyxCollectionQuery read GetBaseline;
    property Effective: TNyxCollectionQuery read GetEffective;
    property HasUserSort: Boolean read GetHasUserSort;
    property UserSort: TNyxCollectionQuery read GetUserSort;
    property SortOwner: TNyxQueryOwnerInfo read GetSortOwner;
    property Count: Integer read GetCount;
  end;

  { Optional canonical session on the existing live view, preserving its old
    interface/GUID. CurrentQuery is the initial baseline. Additional surfaces
    join it; they never recapture a composed effective query as another baseline.
    Independent predicates AND together; one shared ordered sort overrides the
    baseline sort. Every model command belongs to the owning UI thread. }
  INyxCollectionQuerySession = interface(IInterface)
    ['{F303E794-535E-46AB-AC71-93606D288345}']
    function Capture: INyxCollectionQuerySessionSnapshot;
    { Allocate an exact token without changing baseline, query or metadata.
      A new same-named incarnation cannot steal an earlier orphan's identity. }
    function AcquireOwner(const AOrigin: TNyxQueryOriginRef;
      const ALabel: TNyxText): INyxCollectionQueryOwner;
    { Use one fresh whole-session capture and live owner. Stale/foreign/revoked
      commands refuse without mutation. Metadata-only ownership changes
      advance intent; exact same-owner/value batches remain no-ops. Validate,
      final flag-only fences, pure Install and all-attempt Notify/Retire reuse
      the same prepared-query participant and reservation as scalar groups. }
    function PrepareSession(const ACapture: INyxCollectionQuerySessionSnapshot;
      const AOwner: INyxCollectionQueryOwner;
      const AEdits: TNyxQuerySessionEdits): INyxPreparedPublication;
    { Explicit authored-baseline restoration requires the owner's complete
      current specification, exact view capture and live actor. It is never a
      side effect of acquiring/releasing a lease or observing equal query text. }
    function PrepareRebase(const ACapture: INyxCollectionQuerySessionSnapshot;
      const AOwner: INyxCollectionQueryOwner;
      const AAuthoredSpec: TNyxCollectionViewSpec): INyxPreparedPublication;
    { Deliberate copied-metadata takeover, not latest-at-commit source tracking.
      Validate bounded descriptors and recompute output from current target rows.
      Install a fresh generation with remapped disconnected owner IDs and revoke
      target leases; never revoke or retain a source view's command authority. }
    function PrepareImport(const ATargetCapture,
      ASourceSnapshot: INyxCollectionQuerySessionSnapshot): INyxPreparedPublication;
  end;

function NyxQueryOrigin(const AName: TNyxText): TNyxQueryOriginRef;
function NyxQueryChannel(const AName: TNyxText): TNyxQueryChannelRef;
function NyxQueryOwnerID(AValue: Integer): TNyxQueryOwnerID;
function NyxQuerySessionEdits: TNyxQuerySessionEdits;

implementation

function NyxQueryOrigin(const AName: TNyxText): TNyxQueryOriginRef;
begin
  Result.FName := NyxCollection(AName).Name;
end;

function NyxQueryChannel(const AName: TNyxText): TNyxQueryChannelRef;
begin
  Result.FName := NyxCollection(AName).Name;
end;

function NyxQueryOwnerID(AValue: Integer): TNyxQueryOwnerID;
begin

  if AValue < 1 then
  begin
    raise ENyxCollection.Create('Query owner IDs must be positive');
  end;
  Result.FValue := AValue;
end;

function TNyxQueryOwnerID.GetDefined: Boolean;
begin
  Result := FValue > 0;
end;

function TNyxQuerySessionEdit.GetQuery: TNyxCollectionQuery;
begin
  Result := FQuery.Copy;
end;

function TNyxQuerySessionEdit.Copy: TNyxQuerySessionEdit;
begin
  Result.FKind := FKind;
  Result.FChannel := FChannel;
  Result.FOwnerID := FOwnerID;
  Result.FQuery := FQuery.Copy;
end;

function NyxQuerySessionEdits: TNyxQuerySessionEdits;
begin
  Result := Default(TNyxQuerySessionEdits);
end;

function TNyxQuerySessionEdits.GetCount: Integer;
begin
  Result := Length(FEdits);
end;

function TNyxQuerySessionEdits.Copy: TNyxQuerySessionEdits;
var
  LIndex: Integer;
begin
  Result := NyxQuerySessionEdits;
  SetLength(Result.FEdits, Count);
  for LIndex := 0 to Count - 1 do
  begin
    Result.FEdits[LIndex] := FEdits[LIndex].Copy;
  end;
end;

function TNyxQuerySessionEdits.EditAt(AIndex: Integer): TNyxQuerySessionEdit;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxCollection.Create('Query session edit index is out of range');
  end;
  Result := FEdits[AIndex].Copy;
end;

function TNyxQuerySessionEdits.Add(
  const AEdit: TNyxQuerySessionEdit): TNyxQuerySessionEdits;
begin

  if Count >= NyxMaximumQuerySessionEdits then
  begin
    raise ENyxCollection.Create('Query session edit budget is exceeded');
  end;
  Result := Copy;
  SetLength(Result.FEdits, Count + 1);
  Result.FEdits[Count] := AEdit.Copy;
end;

function TNyxQuerySessionEdits.Filter(const AChannel: TNyxQueryChannelRef;
  const APredicate: INyxCollectionPredicate): TNyxQuerySessionEdits;
var
  LEdit: TNyxQuerySessionEdit;
begin
  { Copy before an alternative predicate's ToData can change a caller variable. }
  Result := Copy;
  LEdit := Default(TNyxQuerySessionEdit);
  LEdit.FKind := nqseFilter;
  LEdit.FChannel := NyxQueryChannel(AChannel.Name);
  LEdit.FQuery := NyxCollectionQuery.Where(APredicate);
  Result := Result.Add(LEdit);
end;

function TNyxQuerySessionEdits.ClearFilter(
  const AChannel: TNyxQueryChannelRef): TNyxQuerySessionEdits;
var
  LEdit: TNyxQuerySessionEdit;
begin
  LEdit := Default(TNyxQuerySessionEdit);
  LEdit.FKind := nqseClearFilter;
  LEdit.FChannel := NyxQueryChannel(AChannel.Name);
  Result := Add(LEdit);
end;

function TNyxQuerySessionEdits.SortBy(
  const AQuery: TNyxCollectionQuery): TNyxQuerySessionEdits;
var
  LEdit: TNyxQuerySessionEdit;
begin
  LEdit := Default(TNyxQuerySessionEdit);
  LEdit.FKind := nqseSortBy;
  LEdit.FQuery := TNyxCollectionQuery.FromData(AQuery.ToData);

  if LEdit.FQuery.Filter <> nil then
  begin
    raise ENyxCollection.Create('Shared query sort must contain only sort keys');
  end;
  Result := Add(LEdit);
end;

function TNyxQuerySessionEdits.ResetSort: TNyxQuerySessionEdits;
var
  LEdit: TNyxQuerySessionEdit;
begin
  LEdit := Default(TNyxQuerySessionEdit);
  LEdit.FKind := nqseResetSort;
  Result := Add(LEdit);
end;

function TNyxQuerySessionEdits.ClearOwner: TNyxQuerySessionEdits;
var
  LEdit: TNyxQuerySessionEdit;
begin
  LEdit := Default(TNyxQuerySessionEdit);
  LEdit.FKind := nqseClearOwner;
  Result := Add(LEdit);
end;

function TNyxQuerySessionEdits.ClearAllFilters: TNyxQuerySessionEdits;
var
  LEdit: TNyxQuerySessionEdit;
begin
  LEdit := Default(TNyxQuerySessionEdit);
  LEdit.FKind := nqseClearAllFilters;
  Result := Add(LEdit);
end;

function TNyxQuerySessionEdits.ResetAll: TNyxQuerySessionEdits;
var
  LEdit: TNyxQuerySessionEdit;
begin
  LEdit := Default(TNyxQuerySessionEdit);
  LEdit.FKind := nqseResetAll;
  Result := Add(LEdit);
end;

function TNyxQuerySessionEdits.RemoveFilter(const AOwnerID: TNyxQueryOwnerID;
  const AChannel: TNyxQueryChannelRef): TNyxQuerySessionEdits;
var
  LEdit: TNyxQuerySessionEdit;
begin
  LEdit := Default(TNyxQuerySessionEdit);
  LEdit.FKind := nqseRemoveFilter;
  LEdit.FOwnerID := NyxQueryOwnerID(AOwnerID.Value);
  LEdit.FChannel := NyxQueryChannel(AChannel.Name);
  Result := Add(LEdit);
end;

end.
