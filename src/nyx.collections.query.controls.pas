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

unit nyx.collections.query.controls;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.types, nyx.root.types, nyx.model,
  nyx.state, nyx.behavior, nyx.events, nyx.popover.types,
  nyx.collections, nyx.collections.query, nyx.collections.query.session,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.query.editor;

const
  { Each presenter owns a separate document/root, so this exact form identity
    is independent across tables without encoding a runtime node identity. }
  NyxQueryWorkbenchEditorID = 'nyx-query-workbench';

type
  { User intentions distinguish withdrawing an override from explicit unsorted
    order. ColumnIndex is -1 for commands addressing the complete workbench. }
  TNyxQueryWorkbenchCommand = (nqwcApplyFilter, nqwcApplySort, nqwcSortColumn,
    nqwcClearColumn, nqwcClearOwner, nqwcClearAllFilters, nqwcResetSort,
    nqwcResetAll, nqwcRemoveFilter);
  { A before proposal is synchronous and cancellable through its ordinary
    sequential execution. Changed is a committed notification, never a veto. }
  TNyxQueryWorkbenchEvent = (nqweBeforeChange, nqweChanged);

  { Value-only accepted observation. Matching rows are counted before paging;
    SourceRows counts the source dataset, not rows eligible under the baseline.
    Contributions and owner information are immutable capture-time values. }
  TNyxQueryWorkbenchObservation = record
  private
    FQuery: TNyxCollectionQuery;
    FContributions: array of TNyxQueryContribution;
    FHasUserSort: Boolean;
    FSortOwner: TNyxQueryOwnerInfo;
    FMatchedRows: Integer;
    FSourceRows: Integer;
    FPageRows: Integer;
    function GetQuery: TNyxCollectionQuery;
    function GetCount: Integer;
  public
    function Copy: TNyxQueryWorkbenchObservation;
    function ContributionAt(AIndex: Integer): TNyxQueryContribution;
    function SamePolicy(const AOther: TNyxQueryWorkbenchObservation): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxQueryWorkbenchObservation; static;
    class function Read(const AView: INyxCollectionView): TNyxQueryWorkbenchObservation; static;
    property Query: TNyxCollectionQuery read GetQuery;
    property Count: Integer read GetCount;
    property HasUserSort: Boolean read FHasUserSort;
    property SortOwner: TNyxQueryOwnerInfo read FSortOwner;
    property MatchedRows: Integer read FMatchedRows;
    property SourceRows: Integer read FSourceRows;
    property PageRows: Integer read FPageRows;
  end;

  { Before carries accepted data plus requested edits, not an invented candidate
    effective query. After exists only following committed model publication.
    No interface, node, source, presenter or executable authority is serialized. }
  TNyxQueryWorkbenchChange = record
  private
    FCommand: TNyxQueryWorkbenchCommand;
    FColumnIndex: Integer;
    FActor: TNyxQueryOwnerInfo;
    FBefore: TNyxQueryWorkbenchObservation;
    FHasAfter: Boolean;
    FAfter: TNyxQueryWorkbenchObservation;
    FEdits: TNyxQuerySessionEdits;
    function GetBefore: TNyxQueryWorkbenchObservation;
    function GetAfter: TNyxQueryWorkbenchObservation;
    function GetEdits: TNyxQuerySessionEdits;
  public
    function Copy: TNyxQueryWorkbenchChange;
    function ToData: TNyxDataValue;
    function Committed(const AAfter: TNyxQueryWorkbenchObservation): TNyxQueryWorkbenchChange;
    class function Proposal(ACommand: TNyxQueryWorkbenchCommand; AColumnIndex: Integer;
      const AActor: TNyxQueryOwnerInfo; const ABefore: TNyxQueryWorkbenchObservation;
      const AEdits: TNyxQuerySessionEdits): TNyxQueryWorkbenchChange; static;
    class function FromData(const AData: TNyxDataValue): TNyxQueryWorkbenchChange; static;
    property Command: TNyxQueryWorkbenchCommand read FCommand;
    property ColumnIndex: Integer read FColumnIndex;
    property Actor: TNyxQueryOwnerInfo read FActor;
    property Before: TNyxQueryWorkbenchObservation read GetBefore;
    property HasAfter: Boolean read FHasAfter;
    property After: TNyxQueryWorkbenchObservation read GetAfter;
    property Edits: TNyxQuerySessionEdits read GetEdits;
  end;

  { A managed ordinary Nyx form. Its target implementation borrows its own
    mounted Root only during Capture/CaptureDraft, returning detached values.
    Content's authored node is not a substitute for the mounted live form. }
  INyxQueryFormPresenter = interface(IInterface)
    ['{BD127FC3-3B1E-42B6-9B22-9F1F7C331043}']
    function GetEvents: INyxEvents;
    function GetConnected: Boolean;
    { Closing a popover preserves its controls/events. Open state separately
      fences queued default actions from a dismissed but retained form. }
    function GetOpen: Boolean;
    function Capture(const AActionID: TNyxText; out AChange: TNyxQueryEditorChange): Boolean;
    function CaptureDraft: TNyxQueryEditorDraft;
    { Update the presenter's real scalar state bound to its status label. }
    procedure SetStatus(const AText: TNyxText);
    procedure Show(const AOptions: TNyxPopoverOptions);
    procedure Close;
    { Revoke borrowed receivers before closing/releasing physical owners. }
    procedure Disconnect;
    property Events: INyxEvents read GetEvents;
    property Connected: Boolean read GetConnected;
    property IsOpen: Boolean read GetOpen;
  end;

  { Exact-table weak target port. It copies the borrowed form document through
    the existing popover constructor and retains no renderer/document/widget.
    Root's target adapters revoke it before collection target destruction. }
  INyxQueryFormPresenterFactory = interface(IInterface)
    ['{D43D034D-5518-40FE-83B6-083CBEFD4BA0}']
    function CreateForm(AColumn: Integer; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxQueryFormPresenter;
    procedure Revoke;
  end;

  { One surface joins the existing view-owned canonical query session. Target
    adapters own input and invoke these typed intentions. Disconnection revokes
    pending commands without secretly rolling back accepted contributions. }
  INyxCollectionQueryWorkbench = interface(IInterface)
    ['{6F20A48F-62C2-454F-A228-AF415FB7C71D}']
    procedure OpenColumn(AColumn: Integer);
    procedure OpenSort;
    { Explicitly discard the current draft and stage fresh accepted rules. Plain
      Open preserves drafts; a stale launch capture never silently rebases. }
    procedure ReloadDraft;
    procedure SortColumn(AColumn: Integer; ADirection: TNyxSortDirection;
      AAppend: Boolean = False);
    procedure ClearColumn(AColumn: Integer);
    procedure ClearOwner;
    procedure ClearAllFilters;
    procedure ResetSort;
    procedure ResetAll;
    procedure RemoveFilter(const AOwner: TNyxQueryOwnerID;
      const AChannel: TNyxQueryChannelRef);
    { Disabling supersedes pending intentions and closes the form; it never
      withdraws accepted filters. Read-only data remains independently queryable. }
    procedure SetEnabled(AEnabled: Boolean);
    function GetConnected: Boolean;
    function GetObservation: TNyxQueryWorkbenchObservation;
    function GetPresenter: INyxQueryFormPresenter;
    procedure Revoke;
    procedure Disconnect;
    property Connected: Boolean read GetConnected;
    property Observation: TNyxQueryWorkbenchObservation read GetObservation;
    { Optional target facets on this exact presenter permit physical consumers. }
    property Presenter: INyxQueryFormPresenter read GetPresenter;
  end;

function NyxQueryWorkbenchEvent(AEvent: TNyxQueryWorkbenchEvent): TNyxEventRef;
{ Strict immutable Details decoder for these two library-owned event names. }
function NyxQueryWorkbenchChange(const AEvent: TNyxEventInfo): TNyxQueryWorkbenchChange;
{ A presentation-only scalar key. Hosts set it through their live state owner. }
function NyxQueryFormStatusState: TNyxTextStateRef;
{ Row-zero/native and TH/browser share portable state labels without adding a
  fake source row, changing grid offsets or counting only the painted page. }
function NyxQueryWorkbenchHeader(const AObservation: TNyxQueryWorkbenchObservation;
  const ASpec: TNyxCollectionViewSpec; AColumn: Integer): TNyxText;

implementation

uses
  SysUtils, nyx.collections.paging;

const
  CCommands: array[TNyxQueryWorkbenchCommand] of TNyxText =
    ('apply-filter', 'apply-sort', 'sort-column', 'clear-column', 'clear-owner',
     'clear-all-filters', 'reset-sort', 'reset-all', 'remove-filter');
  CEdits: array[TNyxQuerySessionEditKind] of TNyxText =
    ('filter', 'clear-filter', 'sort-by', 'reset-sort', 'clear-owner',
     'clear-all-filters', 'reset-all', 'remove-filter');

procedure Shape(const AData: TNyxDataValue; const AKeys: array of TNyxText);
var
  LIndex, LKey: Integer;
  LFound: Boolean;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> Length(AKeys)) then
  begin
    raise ENyxCollection.Create('Invalid query command object shape');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LFound := False;
    for LKey := 0 to High(AKeys) do
    begin
      LFound := LFound or (AData.Key(LIndex) = AKeys[LKey]);
    end;

    if not LFound then
    begin
      raise ENyxCollection.Create('Unknown query command member');
    end;
  end;
end;

function OwnerData(const AOwner: TNyxQueryOwnerInfo): TNyxDataValue;
begin

  if not AOwner.ID.Defined then
  begin
    Exit(NyxNull);
  end;
  Result := NyxObject([NyxField('id', NyxData(AOwner.ID.Value)),
    NyxField('origin', NyxData(AOwner.Origin.Name)), NyxField('caption', NyxData(AOwner.Caption)),
    NyxField('connected', NyxData(AOwner.Connected))]);
end;

function ReadOwner(const AData: TNyxDataValue): TNyxQueryOwnerInfo;
begin
  Result := Default(TNyxQueryOwnerInfo);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  Shape(AData, ['id', 'origin', 'caption', 'connected']);
  Result.ID := NyxQueryOwnerID(AData.Field('id').AsInteger);
  Result.Origin := NyxQueryOrigin(AData.Field('origin').AsText);
  Result.Caption := NyxCollection(AData.Field('caption').AsText).Name;
  Result.Connected := AData.Field('connected').AsBoolean;
end;

function TNyxQueryWorkbenchObservation.GetQuery: TNyxCollectionQuery;
begin
  Result := FQuery.Copy;
end;

function TNyxQueryWorkbenchObservation.GetCount: Integer;
begin
  Result := Length(FContributions);
end;

function TNyxQueryWorkbenchObservation.Copy: TNyxQueryWorkbenchObservation;
var
  LIndex: Integer;
begin
  Result := Self;
  Result.FQuery := FQuery.Copy;
  SetLength(Result.FContributions, Count);
  for LIndex := 0 to Count - 1 do
  begin
    Result.FContributions[LIndex] := ContributionAt(LIndex);
  end;
end;

function TNyxQueryWorkbenchObservation.ContributionAt(AIndex: Integer): TNyxQueryContribution;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxCollection.Create('Query contribution index is outside this observation');
  end;
  Result := FContributions[AIndex];
  Result.Filter := Result.Filter.Copy;
end;

class function TNyxQueryWorkbenchObservation.Read(
  const AView: INyxCollectionView): TNyxQueryWorkbenchObservation;
var
  LView: INyxCollectionView;
  LSession: INyxCollectionQuerySessionSnapshot;
  LIndex: Integer;
begin
  LView := AView;

  if LView = nil then
  begin
    raise ENyxCollection.Create('Query observation requires its live view');
  end;
  Result := Default(TNyxQueryWorkbenchObservation);
  LSession := NyxCollectionQuerySession(LView).Capture;
  Result.FQuery := LSession.Effective;
  Result.FHasUserSort := LSession.HasUserSort;

  if Result.FHasUserSort then
  begin
    Result.FSortOwner := LSession.SortOwner;
  end;
  SetLength(Result.FContributions, LSession.Count);
  for LIndex := 0 to LSession.Count - 1 do
  begin
    Result.FContributions[LIndex] := LSession.ContributionAt(LIndex);
  end;
  Result.FMatchedRows := NyxCollectionPages(LView).Window.Total;
  Result.FSourceRows := LView.Store.Snapshot.Count;
  Result.FPageRows := LView.Snapshot.Count;
end;

function TNyxQueryWorkbenchObservation.ToData: TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LValues, Count);
  for LIndex := 0 to Count - 1 do
  begin
    LValues[LIndex] := NyxObject([
      NyxField('owner', OwnerData(FContributions[LIndex].Owner)),
      NyxField('channel', NyxData(FContributions[LIndex].Channel.Name)),
      NyxField('filter', FContributions[LIndex].Filter.Copy)]);
  end;
  Result := NyxObject([NyxField('query', FQuery.ToData),
    NyxField('contributions', NyxArray(LValues)), NyxField('hasUserSort', NyxData(FHasUserSort)),
    NyxField('sortOwner', OwnerData(FSortOwner)), NyxField('matchingRows', NyxData(FMatchedRows)),
    NyxField('sourceRows', NyxData(FSourceRows)), NyxField('pageRows', NyxData(FPageRows))]);
end;

class function TNyxQueryWorkbenchObservation.FromData(
  const AData: TNyxDataValue): TNyxQueryWorkbenchObservation;
var
  LArray, LEntry: TNyxDataValue;
  LIndex, LPrevious: Integer;
  LPredicate: INyxCollectionPredicate;
begin
  Shape(AData, ['query', 'contributions', 'hasUserSort', 'sortOwner',
    'matchingRows', 'sourceRows', 'pageRows']);
  Result := Default(TNyxQueryWorkbenchObservation);
  Result.FQuery := TNyxCollectionQuery.FromData(AData.Field('query'));
  Result.FHasUserSort := AData.Field('hasUserSort').AsBoolean;
  Result.FSortOwner := ReadOwner(AData.Field('sortOwner'));

  if Result.FHasUserSort <> Result.FSortOwner.ID.Defined then
  begin
    raise ENyxCollection.Create('Query observation has inconsistent sort ownership');
  end;
  Result.FMatchedRows := AData.Field('matchingRows').AsInteger;
  Result.FSourceRows := AData.Field('sourceRows').AsInteger;
  Result.FPageRows := AData.Field('pageRows').AsInteger;

  if (Result.FMatchedRows < 0) or (Result.FSourceRows < Result.FMatchedRows) or
    (Result.FPageRows < 0) or (Result.FPageRows > Result.FMatchedRows) then
  begin
    raise ENyxCollection.Create('Query observation has invalid row counts');
  end;
  LArray := AData.Field('contributions');

  if (LArray.Kind <> ndArray) or (LArray.Count > NyxMaximumQuerySessionContributions) then
  begin
    raise ENyxCollection.Create('Query observation contribution budget exceeded');
  end;
  SetLength(Result.FContributions, LArray.Count);
  for LIndex := 0 to LArray.Count - 1 do
  begin
    LEntry := LArray.Item(LIndex);
    Shape(LEntry, ['owner', 'channel', 'filter']);
    Result.FContributions[LIndex].Owner := ReadOwner(LEntry.Field('owner'));

    if not Result.FContributions[LIndex].Owner.ID.Defined then
    begin
      raise ENyxCollection.Create('Query contribution requires an owner identity');
    end;
    Result.FContributions[LIndex].Channel := NyxQueryChannel(LEntry.Field('channel').AsText);
    LPredicate := NyxPredicateFromData(LEntry.Field('filter'));
    Result.FContributions[LIndex].Filter := LPredicate.ToData;
    for LPrevious := 0 to LIndex - 1 do
    begin

      if (Result.FContributions[LPrevious].Owner.ID.Value = Result.FContributions[LIndex].Owner.ID.Value) and
        (Result.FContributions[LPrevious].Channel.Name = Result.FContributions[LIndex].Channel.Name) then
      begin
        raise ENyxCollection.Create('Duplicate query contribution identity');
      end;
    end;
  end;
end;

function TNyxQueryWorkbenchObservation.SamePolicy(
  const AOther: TNyxQueryWorkbenchObservation): Boolean;
var
  LLeft, LRight: TNyxQueryWorkbenchObservation;
  LIndex: Integer;
begin
  LLeft := Copy;
  LRight := AOther.Copy;
  LLeft.FMatchedRows := 0;
  LRight.FMatchedRows := 0;
  LLeft.FSourceRows := 0;
  LRight.FSourceRows := 0;
  LLeft.FPageRows := 0;
  LRight.FPageRows := 0;
  LLeft.FSortOwner.Connected := False;
  LRight.FSortOwner.Connected := False;
  for LIndex := 0 to LLeft.Count - 1 do
  begin
    LLeft.FContributions[LIndex].Owner.Connected := False;
  end;
  for LIndex := 0 to LRight.Count - 1 do
  begin
    LRight.FContributions[LIndex].Owner.Connected := False;
  end;
  Result := LLeft.ToData.ToJSON = LRight.ToData.ToJSON;
end;

function EditsData(const AEdits: TNyxQuerySessionEdits): TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LEdit: TNyxQuerySessionEdit;
  LIndex: Integer;
begin
  SetLength(LValues, AEdits.Count);
  for LIndex := 0 to AEdits.Count - 1 do
  begin
    LEdit := AEdits.EditAt(LIndex);
    LValues[LIndex] := NyxObject([NyxField('kind', NyxData(CEdits[LEdit.Kind])),
      NyxField('channel', NyxData(LEdit.Channel.Name)),
      NyxField('owner', NyxData(LEdit.OwnerID.Value)), NyxField('query', LEdit.Query.ToData)]);
  end;
  Result := NyxArray(LValues);
end;

function ReadEdits(const AData: TNyxDataValue): TNyxQuerySessionEdits;
var
  LIndex, LKind: Integer;
  LEntry: TNyxDataValue;
  LQuery: TNyxCollectionQuery;
  LChannel: TNyxQueryChannelRef;
  LOwner: Integer;
  LPredicate: INyxCollectionPredicate;
begin

  if (AData.Kind <> ndArray) or (AData.Count > NyxMaximumQuerySessionEdits) then
  begin
    raise ENyxCollection.Create('Invalid query command edit budget');
  end;
  Result := NyxQuerySessionEdits;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LEntry := AData.Item(LIndex);
    Shape(LEntry, ['kind', 'channel', 'owner', 'query']);
    LKind := 0;
    while (LKind < Length(CEdits)) and (CEdits[TNyxQuerySessionEditKind(LKind)] <> LEntry.Field('kind').AsText) do
    begin
      Inc(LKind);
    end;

    if LKind = Length(CEdits) then
    begin
      raise ENyxCollection.Create('Unknown query command edit');
    end;
    LQuery := TNyxCollectionQuery.FromData(LEntry.Field('query'));
    LChannel := Default(TNyxQueryChannelRef);

    if LEntry.Field('channel').AsText <> '' then
    begin
      LChannel := NyxQueryChannel(LEntry.Field('channel').AsText);
    end;
    LOwner := LEntry.Field('owner').AsInteger;
    case TNyxQuerySessionEditKind(LKind) of
      nqseFilter:
      begin
        LPredicate := LQuery.Filter;

        if (LPredicate = nil) or (LQuery.SortCount <> 0) then
        begin
          raise ENyxCollection.Create('Filter edit requires only a predicate');
        end;
        Result := Result.Filter(LChannel, LPredicate);
      end;
      nqseClearFilter: Result := Result.ClearFilter(LChannel);
      nqseSortBy: Result := Result.SortBy(LQuery);
      nqseResetSort: Result := Result.ResetSort;
      nqseClearOwner: Result := Result.ClearOwner;
      nqseClearAllFilters: Result := Result.ClearAllFilters;
      nqseResetAll: Result := Result.ResetAll;
      nqseRemoveFilter: Result := Result.RemoveFilter(NyxQueryOwnerID(LOwner), LChannel);
    end;
  end;

  if EditsData(Result).ToJSON <> AData.ToJSON then
  begin
    raise ENyxCollection.Create('Query edit contains unrelated or noncanonical fields');
  end;
end;

function TNyxQueryWorkbenchChange.GetBefore: TNyxQueryWorkbenchObservation;
begin
  Result := FBefore.Copy;
end;

function TNyxQueryWorkbenchChange.GetAfter: TNyxQueryWorkbenchObservation;
begin

  if not FHasAfter then
  begin
    raise ENyxCollection.Create('A query proposal has no committed after observation');
  end;
  Result := FAfter.Copy;
end;

function TNyxQueryWorkbenchChange.GetEdits: TNyxQuerySessionEdits;
begin
  Result := FEdits.Copy;
end;

function TNyxQueryWorkbenchChange.Copy: TNyxQueryWorkbenchChange;
begin
  Result := Self;
  Result.FBefore := FBefore.Copy;
  Result.FAfter := FAfter.Copy;
  Result.FEdits := FEdits.Copy;
end;

class function TNyxQueryWorkbenchChange.Proposal(ACommand: TNyxQueryWorkbenchCommand;
  AColumnIndex: Integer; const AActor: TNyxQueryOwnerInfo;
  const ABefore: TNyxQueryWorkbenchObservation;
  const AEdits: TNyxQuerySessionEdits): TNyxQueryWorkbenchChange;
begin

  if (Ord(ACommand) < Ord(Low(TNyxQueryWorkbenchCommand))) or
    (Ord(ACommand) > Ord(High(TNyxQueryWorkbenchCommand))) or
    (AColumnIndex < -1) or not AActor.ID.Defined then
  begin
    raise ENyxCollection.Create('Invalid query command proposal');
  end;
  Result := Default(TNyxQueryWorkbenchChange);
  Result.FCommand := ACommand;
  Result.FColumnIndex := AColumnIndex;
  Result.FActor := AActor;
  Result.FBefore := ABefore.Copy;
  Result.FEdits := AEdits.Copy;
end;

function TNyxQueryWorkbenchChange.Committed(
  const AAfter: TNyxQueryWorkbenchObservation): TNyxQueryWorkbenchChange;
begin
  Result := Copy;
  Result.FHasAfter := True;
  Result.FAfter := AAfter.Copy;
end;

function TNyxQueryWorkbenchChange.ToData: TNyxDataValue;
var
  LAfter: TNyxDataValue;
begin
  LAfter := NyxNull;

  if FHasAfter then
  begin
    LAfter := FAfter.ToData;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('command', NyxData(CCommands[FCommand])), NyxField('column', NyxData(FColumnIndex)),
    NyxField('actor', OwnerData(FActor)), NyxField('before', FBefore.ToData),
    NyxField('after', LAfter), NyxField('edits', EditsData(FEdits))]);
end;

class function TNyxQueryWorkbenchChange.FromData(
  const AData: TNyxDataValue): TNyxQueryWorkbenchChange;
var
  LCommand: Integer;
  LAfter: TNyxDataValue;
begin
  Shape(AData, ['version', 'command', 'column', 'actor', 'before', 'after', 'edits']);

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxCollection.Create('Unknown query command payload version');
  end;
  LCommand := 0;
  while (LCommand < Length(CCommands)) and
    (CCommands[TNyxQueryWorkbenchCommand(LCommand)] <> AData.Field('command').AsText) do
  begin
    Inc(LCommand);
  end;

  if LCommand = Length(CCommands) then
  begin
    raise ENyxCollection.Create('Unknown query workbench command');
  end;
  Result := Proposal(TNyxQueryWorkbenchCommand(LCommand), AData.Field('column').AsInteger,
    ReadOwner(AData.Field('actor')), TNyxQueryWorkbenchObservation.FromData(AData.Field('before')),
    ReadEdits(AData.Field('edits')));
  LAfter := AData.Field('after');

  if LAfter.Kind <> ndNull then
  begin
    Result := Result.Committed(TNyxQueryWorkbenchObservation.FromData(LAfter));
  end;
end;

function NyxQueryWorkbenchEvent(AEvent: TNyxQueryWorkbenchEvent): TNyxEventRef;
const
  CNames: array[TNyxQueryWorkbenchEvent] of TNyxText = ('query-changing', 'query-changed');
begin

  if (Ord(AEvent) < Ord(Low(TNyxQueryWorkbenchEvent))) or
    (Ord(AEvent) > Ord(High(TNyxQueryWorkbenchEvent))) then
  begin
    raise ENyxCollection.Create('Unknown query workbench event');
  end;
  Result := NyxNamedEvent(CNames[AEvent]);
end;

function NyxQueryWorkbenchChange(const AEvent: TNyxEventInfo): TNyxQueryWorkbenchChange;
begin

  if not AEvent.HasDetails or
    ((AEvent.Name.Name <> NyxQueryWorkbenchEvent(nqweBeforeChange).Name) and
     (AEvent.Name.Name <> NyxQueryWorkbenchEvent(nqweChanged).Name)) then
  begin
    raise ENyxCollection.Create('Event is not a query workbench change');
  end;
  Result := TNyxQueryWorkbenchChange.FromData(AEvent.Details);

  if Result.HasAfter <> (AEvent.Name.Name = NyxQueryWorkbenchEvent(nqweChanged).Name) then
  begin
    raise ENyxCollection.Create('Query event phase and payload disagree');
  end;
end;

function NyxQueryFormStatusState: TNyxTextStateRef;
begin
  Result := NyxTextState('nyx-query-form-status');
end;

function NyxQueryWorkbenchHeader(const AObservation: TNyxQueryWorkbenchObservation;
  const ASpec: TNyxCollectionViewSpec; AColumn: Integer): TNyxText;
var
  LIndex: Integer;
  LField: TNyxText;
  LSort: TNyxCollectionSort;
  LFiltered: Boolean;
  LPredicate: INyxCollectionPredicate;
  LQuery: TNyxCollectionQuery;

  function UsesField(const AValue: INyxCollectionPredicate): Boolean;
  var
    LChild: Integer;
  begin
    Result := (AValue.Kind = nqpField) and (AValue.FieldName = LField);
    for LChild := 0 to AValue.ChildCount - 1 do
    begin
      Result := Result or UsesField(AValue.Child(LChild));
    end;
  end;

begin
  LField := ASpec.ColumnAt(AColumn).FieldName;
  Result := ASpec.ColumnAt(AColumn).Title;
  LFiltered := False;
  LQuery := AObservation.Query;
  LPredicate := LQuery.Filter;

  if LPredicate <> nil then
  begin
    LFiltered := UsesField(LPredicate);
  end;

  if LFiltered then
  begin
    Result := Result + ' [filtered]';
  end;
  for LIndex := 0 to LQuery.SortCount - 1 do
  begin
    LSort := LQuery.SortAt(LIndex);

    if LSort.FieldName = LField then
    begin

      if LSort.Direction = nsdAscending then
      begin
        Result := Result + ' [asc ' + TNyxText(IntToStr(LIndex + 1)) + ']';
      end
      else
      begin
        Result := Result + ' [desc ' + TNyxText(IntToStr(LIndex + 1)) + ']';
      end;
    end;
  end;

  if AColumn = 0 then
  begin
    Result := Result + ' [' + TNyxText(IntToStr(AObservation.MatchedRows)) + ' / ' +
      TNyxText(IntToStr(AObservation.SourceRows)) + ' rows]';
  end;
end;

end.
