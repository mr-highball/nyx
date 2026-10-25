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
unit nyx.collections.tree.inspection;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections, nyx.collections.view;

type
  { Query inclusion and physical disclosure are different facts. Context denotes
    an included ancestor that does not itself satisfy the complete predicate. }
  TNyxTreeRowRole = (ntrAbsent, ntrMatch, ntrContext);

  { Missing means absent from the captured displayed source. Filtered means the
    source row exists but is excluded by the complete accepted query. }
  TNyxTreeVisibility = (ntvMissing, ntvFiltered, ntvCollapsed, ntvDisclosed);

  { Admission outcome of an explicit reveal. Filtered never clears a query, and
    missing never substitutes another identity. Later observers may change data
    after a revealed command commits; this result is not a lasting visibility lock. }
  TNyxTreeRevealResult = (ntrrMissing, ntrrFiltered, ntrrRevealed);

  { Copied logical counts, not painted DOM rows or viewport capacity. Matches
    satisfy ALL accepted predicates; ContextRows are required ancestors only.
    Hidden selected counts are disjoint: query exclusion versus closed branches.
    A default observation is undefined. It owns no live objects or authority. }
  TNyxTreeInspectionObservation = record
    Defined: Boolean;
    SourceRevision: Integer;
    CommandSerial: Integer;
    SourceRows: Integer;
    ResultRows: Integer;
    MatchedRows: Integer;
    ContextRows: Integer;
    DisclosedRows: Integer;
    SelectedRows: Integer;
    FilteredSelectedRows: Integer;
    CollapsedSelectedRows: Integer;
  end;

  { Immutable UI-captured inspection. Retains immutable source/query snapshots
    and disclosed metadata until released, never a store, view, widget or token.
    MatchAt follows accepted query ordering, including currently closed matches.
    Capture before handing these readonly facts to a worker. Retaining a capture
    preserves its logical source payload; it is not a bounded screenshot cache. }
  INyxTreeInspectionCapture = interface(IInterface)
    ['{D70E06EF-094C-4453-9B94-9A246A834E41}']
    function GetObservation: TNyxTreeInspectionObservation;
    function MatchAt(AIndex: Integer): TNyxItemRef;
    function RoleOf(const AItem: TNyxItemRef): TNyxTreeRowRole;
    function VisibilityOf(const AItem: TNyxItemRef): TNyxTreeVisibility;
    property Observation: TNyxTreeInspectionObservation read GetObservation;
  end;

{ UI-thread read with coherent snapshot/selection/disclosure checks. Requires
  the public displayed-source inspection facet, including for alternative views.
  No query, selection or disclosure mutation. Counts use the effective predicate,
  not just one search owner's text. O(result rows + selected rows + hierarchy). }
function CaptureNyxTreeInspection(const AView: INyxCollectionView): INyxTreeInspectionCapture;

{ Explicit UI navigation through INyxTreeReveal. Opens ancestors and
  focuses the admitted item while preserving exact selection membership/anchor.
  Each admitted call requests viewport movement even at unchanged logical focus.
  Preserve is the default host-focus policy; Take explicitly transfers focus.
  Never clears filters, selects a substitute, mutates a source or restores an old
  disclosure snapshot. Foreign/undefined identities and non-Tree views refuse.
  The caller owns its normal view/generation authority; this helper grants none. }
function RevealNyxTreeItem(const AView: INyxCollectionView;
  const AItem: TNyxItemRef; AFocus: TNyxTreeRevealFocus = ntrfPreserve): TNyxTreeRevealResult;

implementation

uses SysUtils, nyx.collections.query, nyx.collections.selection, nyx.collections.tree.rows;

type
  { Detached data projection owns no outward method receiver. Immutable arrays
    are populated before exposure and never returned to writers on either target. }
  TTreeCapture = class(TInterfacedObject, INyxTreeInspectionCapture)
  private
    FSource: INyxCollectionSnapshot;
    FResult: INyxCollectionSnapshot;
    FRows: INyxTreeRows;
    FMatches: TNyxItemRefs;
    FIsMatch: array of Boolean;
    FObservation: TNyxTreeInspectionObservation;
  public
    constructor Create(const AView: INyxCollectionView);
    function GetObservation: TNyxTreeInspectionObservation;
    function MatchAt(AIndex: Integer): TNyxItemRef;
    function RoleOf(const AItem: TNyxItemRef): TNyxTreeRowRole;
    function VisibilityOf(const AItem: TNyxItemRef): TNyxTreeVisibility;
  end;

constructor TTreeCapture.Create(const AView: INyxCollectionView);
var
  LHierarchy: INyxTreeHierarchy;
  LInspection: INyxCollectionInspection;
  LSelection: INyxCollectionSelection;
  LFilter: INyxCollectionPredicate;
  LIndex: Integer;
begin
  inherited Create;
  LHierarchy := NyxTreeHierarchy(AView);
  LInspection := NyxCollectionInspection(AView);

  if LInspection = nil then
  begin
    raise ENyxCollection.Create('Tree inspection requires its displayed-source capability');
  end;
  FSource := LInspection.GetDisplayedSource;
  FResult := AView.Snapshot;
  LSelection := AView.Selection;
  FRows := NyxTreeRows(AView);
  LFilter := AView.QueryPolicy.Filter;
  FObservation.SourceRevision := FSource.Revision;
  FObservation.CommandSerial := FRows.CommandSerial;
  FObservation.SourceRows := FSource.Count;
  FObservation.ResultRows := FResult.Count;
  FObservation.DisclosedRows := FRows.Count;
  FObservation.SelectedRows := LSelection.Count;
  SetLength(FIsMatch, FResult.Count);
  SetLength(FMatches, FResult.Count);
  for LIndex := 0 to FResult.Count - 1 do
  begin
    FIsMatch[LIndex] := (LFilter = nil) or LFilter.Matches(FResult.ItemAt(LIndex));

    if FIsMatch[LIndex] then
    begin
      FMatches[FObservation.MatchedRows] := FResult.ItemAt(LIndex).Ref;
      Inc(FObservation.MatchedRows);
    end;
  end;
  SetLength(FMatches, FObservation.MatchedRows);
  FObservation.ContextRows := FResult.Count - FObservation.MatchedRows;
  for LIndex := 0 to LSelection.Count - 1 do
  begin
    case VisibilityOf(LSelection.ItemAt(LIndex)) of
      ntvFiltered:
        Inc(FObservation.FilteredSelectedRows);
      ntvCollapsed:
        Inc(FObservation.CollapsedSelectedRows);
      ntvMissing:
        raise ENyxCollection.Create('Tree inspection selection is outside its displayed source');
      ntvDisclosed:
        begin
          { A disclosed member is neither hidden by query nor disclosure. }
        end;
    end;
  end;

  if (AView.Snapshot <> FResult) or (AView.Selection <> LSelection) or
    (LInspection.GetDisplayedSource <> FSource) or (FRows.Snapshot <> FResult) or
    (LHierarchy.CommandSerial <> FRows.CommandSerial) or
    (LSelection.DataRevision <> FSource.Revision) then
  begin
    raise ENyxCollection.Create('Tree inspection changed during capture');
  end;
  FObservation.Defined := True;
end;

function TTreeCapture.GetObservation: TNyxTreeInspectionObservation;
begin
  Result := FObservation;
end;

function TTreeCapture.MatchAt(AIndex: Integer): TNyxItemRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FMatches)) then
  begin
    raise ENyxCollection.Create('Tree match index is outside this capture');
  end;
  Result := FMatches[AIndex];
end;

function TTreeCapture.RoleOf(const AItem: TNyxItemRef): TNyxTreeRowRole;
var
  LIndex: Integer;
begin
  Result := ntrAbsent;
  LIndex := FResult.IndexOf(AItem);

  if LIndex >= 0 then
  begin

    if FIsMatch[LIndex] then
    begin
      Result := ntrMatch;
    end
    else
    begin
      Result := ntrContext;
    end;
  end;
end;

function TTreeCapture.VisibilityOf(const AItem: TNyxItemRef): TNyxTreeVisibility;
begin
  Result := ntvMissing;

  if FSource.Has(AItem) then
  begin
    Result := ntvFiltered;

    if FResult.Has(AItem) then
    begin
      Result := ntvCollapsed;

      if FRows.IndexOf(AItem) >= 0 then
      begin
        Result := ntvDisclosed;
      end;
    end;
  end;
end;

function CaptureNyxTreeInspection(const AView: INyxCollectionView): INyxTreeInspectionCapture;
begin
  Result := TTreeCapture.Create(AView);
end;

function RevealNyxTreeItem(const AView: INyxCollectionView;
  const AItem: TNyxItemRef; AFocus: TNyxTreeRevealFocus): TNyxTreeRevealResult;
var
  LCapture: INyxTreeInspectionCapture;
  LReveal: INyxTreeReveal;
begin
  LCapture := CaptureNyxTreeInspection(AView);

  if not AItem.Defined or (AItem.Collection.Name <> AView.Snapshot.Key.Name) then
  begin
    raise ENyxCollection.Create('Tree reveal requires an identity from this source');
  end;
  case LCapture.VisibilityOf(AItem) of
    ntvMissing:
      Exit(ntrrMissing);
    ntvFiltered:
      Exit(ntrrFiltered);
    ntvCollapsed, ntvDisclosed:
      begin
        { The normal view command admits all selection and ancestry before commit. }
      end;
  end;
  if not Supports(AView, INyxTreeReveal, LReveal) then
  begin
    raise ENyxCollection.Create('Tree view does not support explicit viewport reveal');
  end;
  LReveal.RevealTreeItem(AItem, AFocus);
  Result := ntrrRevealed;
end;

end.
