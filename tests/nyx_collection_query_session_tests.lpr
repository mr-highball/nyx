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
program nyx_collection_query_session_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.data,
  nyx.state,
  nyx.publication,
  nyx.collections,
  nyx.collections.query,
  nyx.collections.query.publication,
  nyx.collections.query.session,
  nyx.collections.view,
  nyx.collections.view.types,
  nyx.collections.selection,
  nyx.collections.paging,
  nyx.collections.bindings,
  nyx.model,
  nyx.composition,
  nyx.codec
  {$ifdef PAS2JS}, Web{$endif};

type
  { Public model fixture with one shared query surface, an independent peer,
    two live origin leases and a real scalar state publication owner. }
  TFixture = class
  public
    Store: INyxCollection;
    View: INyxCollectionView;
    Peer: INyxCollectionView;
    Session: INyxCollectionQuerySession;
    PeerSession: INyxCollectionQuerySession;
    A: INyxCollectionQueryOwner;
    B: INyxCollectionQueryOwner;
    State: TNyxState;
    Token: INyxCollectionViewSubscription;
    StateToken: TNyxStateSubscription;
    Calls: Integer;
    StateCalls: Integer;
    ExpectedRows: TNyxText;
    ExpectedState: TNyxText;
    ExpectedContributions: Integer;
    ObserveCoherence: Boolean;
    Coherent: Boolean;
    FailObserver: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Attach;
    procedure Expect(const ARows, AState: TNyxText; AContributions: Integer);
    procedure Observe;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure StateChanged(AState: TNyxState; AChanges: TNyxStateChanges);
  end;

  { A later group validator refuses or revokes an actor's owned flags. It uses
    the standard participant contract without calling another stage's phases. }
  TGate = class(TInterfacedObject, INyxPreparedPublication)
  public
    Refuse: Boolean;
    Actor: INyxCollectionQueryOwner;
    Installs: Integer;
    Notifications: Integer;
    Retired: Boolean;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { An observation token borrows this receiver. Its lifetime is ended before
    the receiver is freed, independently of retained owner/snapshot interfaces. }
  TObservation = class
  public
    Calls: Integer;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Query session: ' + AReason);
  end;
  Inc(GChecks);
end;

function Key: TNyxCollectionRef;
begin
  Result := NyxCollection('people');
end;

function Item(const AName: TNyxText): TNyxItemRef;
begin
  Result := NyxItem(Key, AName);
end;

function Schema: TNyxCollectionSchema;
begin
  Result := NyxCollectionSchema.Text(NyxTextField('name'), '')
    .Integer(NyxIntegerField('score'), 0)
    .Boolean(NyxBooleanField('active'), False)
    .Text(NyxTextField('team'), '');
end;

function Person(const AID, AName: TNyxText; AScore: Integer;
  AActive: Boolean; const ATeam: TNyxText): TNyxCollectionItem;
begin
  Result := NyxCollectionItem(Item(AID))
    .WithValue(NyxTextField('name'), AName)
    .WithValue(NyxIntegerField('score'), AScore)
    .WithValue(NyxBooleanField('active'), AActive)
    .WithValue(NyxTextField('team'), ATeam);
end;

function NewStore: INyxCollection;
begin
  Result := NewNyxCollection(Key, Schema, [
    Person('a', 'Ada', 4, True, 'red'),
    Person('b', 'Bea', 1, False, 'red'),
    Person('c', 'Cy', 3, True, 'blue'),
    Person('d', 'Dee', 2, True, 'red'),
    Person('e', 'Eli', 5, False, 'blue'),
    Person('f', 'Fox', 6, True, 'red')]);
end;

function Baseline: TNyxCollectionQuery;
begin
  Result := NyxCollectionQuery
    .Where(NyxWhere(NyxBooleanField('active')).EqualTo(True))
    .OrderBy(NyxTextField('name'), nsdDescending);
end;

function Spec: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(Key)
    .Column(NyxTextField('name'), 'Name', cmEditable)
    .Column(NyxIntegerField('score'), 'Score', cmEditable)
    .Selection(nsmMultiple).Query(Baseline);
end;

function ScoreSort(ADirection: TNyxSortDirection): TNyxCollectionQuery;
begin
  Result := NyxCollectionQuery.OrderBy(NyxIntegerField('score'), ADirection);
end;

function Search: TNyxQueryChannelRef;
begin
  Result := NyxQueryChannel('search');
end;

function Team: TNyxQueryChannelRef;
begin
  Result := NyxQueryChannel('team');
end;

function Rows(const AView: INyxCollectionView): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  for LIndex := 0 to AView.Snapshot.Count - 1 do
  begin

    if LIndex > 0 then
    begin
      Result := Result + '|';
    end;
    Result := Result + AView.Snapshot.ItemAt(LIndex).Ref.ID;
  end;
end;

function SameQuery(const ALeft, ARight: TNyxCollectionQuery): Boolean;
begin
  Result := ALeft.ToData.ToJSON = ARight.ToData.ToJSON;
end;

procedure Apply(const ASession: INyxCollectionQuerySession;
  const AOwner: INyxCollectionQueryOwner; const AEdits: TNyxQuerySessionEdits);
var
  LStage: INyxPreparedPublication;
begin
  LStage := ASession.PrepareSession(ASession.Capture, AOwner, AEdits);
  try
    PublishNyxGroup([LStage]);
  finally
    LStage.Retire;
  end;
end;

procedure Refuse(const ASession: INyxCollectionQuerySession;
  const ACapture: INyxCollectionQuerySessionSnapshot;
  const AOwner: INyxCollectionQueryOwner; const AEdits: TNyxQuerySessionEdits;
  const AReason: TNyxText);
var
  LStage: INyxPreparedPublication;
  LRefused: Boolean;
begin
  LRefused := False;
  try
    try
      LStage := ASession.PrepareSession(ACapture, AOwner, AEdits);
      LStage.Validate;
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
  finally

    if LStage <> nil then
    begin
      LStage.Retire;
    end;
  end;
  Check(LRefused, AReason);
end;

constructor TFixture.Create;
begin
  inherited Create;
  Store := NewStore;
  View := NewNyxCollectionView(Store, Spec, cpTable);
  Peer := NewNyxCollectionView(Store, Spec, cpTable);
  Session := NyxCollectionQuerySession(View);
  PeerSession := NyxCollectionQuerySession(Peer);
  A := Session.AcquireOwner(NyxQueryOrigin('toolbar'), 'Search toolbar');
  B := Session.AcquireOwner(NyxQueryOrigin('column'), 'Column filters');
  View.SetSelection([Item('a'), Item('d')], Item('a'), Item('d'));
  State := TNyxState.Create;
  State.SetValue(NyxTextState('phase'), 'before');
  Coherent := True;
end;

destructor TFixture.Destroy;
begin

  if Token <> nil then
  begin
    Token.Disconnect;
  end;
  Token := nil;
  StateToken.Free;
  State.Free;
  A := nil;
  B := nil;
  Session := nil;
  PeerSession := nil;
  View := nil;
  Peer := nil;
  Store := nil;
  inherited Destroy;
end;

procedure TFixture.Attach;
begin
  Token := View.Subscribe(Changed);
  StateToken := State.Subscribe(StateChanged);
end;

procedure TFixture.Expect(const ARows, AState: TNyxText;
  AContributions: Integer);
begin
  ExpectedRows := ARows;
  ExpectedState := AState;
  ExpectedContributions := AContributions;
  ObserveCoherence := True;
end;

procedure TFixture.Observe;
begin

  if ObserveCoherence then
  begin
    Coherent := Coherent and (Rows(View) = ExpectedRows)
      and (State.GetValue(NyxTextState('phase')) = ExpectedState)
      and (Session.Capture.Count = ExpectedContributions);
  end;
end;

procedure TFixture.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Calls);
  Coherent := Coherent and (AView = View) and (AChanges = nil);
  Observe;

  if FailObserver then
  begin
    raise ENyxCollection.Create('Expected shared session observer failure');
  end;
end;

procedure TFixture.StateChanged(AState: TNyxState;
  AChanges: TNyxStateChanges);
begin
  Inc(StateCalls);
  Coherent := Coherent and (AState = State) and (AChanges.Count = 1);
  Observe;
end;

procedure TGate.Validate;
begin

  if Refuse then
  begin
    raise ENyxCollection.Create('Expected later session refusal');
  end;

  if Actor <> nil then
  begin
    Actor.Revoke;
  end;
end;

procedure TGate.Install;
begin
  Inc(Installs);
end;

procedure TGate.Notify;
begin
  Inc(Notifications);
end;

procedure TGate.Retire;
begin
  Retired := True;
  Actor := nil;
end;

procedure TObservation.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Calls);
end;

{ Public batches are independent closed values; this is not query execution. }
procedure BatchValues;
var
  LEdits: TNyxQuerySessionEdits;
  LCopy: TNyxQuerySessionEdits;
  LEdit: TNyxQuerySessionEdit;
  LInfo: TNyxQueryOwnerInfo;
  LRefused: Boolean;
  LIndex: Integer;
begin
  LInfo := Default(TNyxQueryOwnerInfo);
  Check(not LInfo.ID.Defined and (LInfo.ID.Value = 0),
    'default copied owner identity is safely undefined');
  LEdits := NyxQuerySessionEdits.Filter(Search,
    NyxWhere(NyxIntegerField('score')).AtLeast(3));
  LCopy := LEdits.SortBy(ScoreSort(nsdDescending));
  Check((LEdits.Count = 1) and (LCopy.Count = 2),
    'fluent batch copies do not append into another value');
  LEdit := LEdits.EditAt(0);
  Check((LEdit.Kind = nqseFilter) and (LEdit.Channel.Name = 'search')
    and (LEdit.Query.SortCount = 0) and (LEdit.Query.Filter <> nil),
    'filter edit exposes typed channel and independently closed predicate');
  LCopy := LEdits.Copy.ClearFilter(Search).ResetSort.ClearOwner
    .ClearAllFilters.ResetAll.RemoveFilter(NyxQueryOwnerID(1), Team);
  Check((LEdits.Count = 1) and (LCopy.Count = 7),
    'all explicit reset/chip operations compose without mutating prior batch');
  LRefused := False;
  try
    LCopy := NyxQuerySessionEdits.SortBy(Baseline);
  except
    on ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'a shared sort refuses a hidden predicate');
  LRefused := False;
  try
    LEdit := LEdits.EditAt(1);
  except
    on ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'copied batch indexing is bounded');
  LCopy := NyxQuerySessionEdits;
  for LIndex := 1 to NyxMaximumQuerySessionEdits do
  begin
    LCopy := LCopy.ResetSort;
  end;
  LRefused := False;
  try
    LCopy := LCopy.ResetSort;
  except
    on ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LCopy.Count = NyxMaximumQuerySessionEdits),
    'over-budget batch refuses without replacing its accepted caller value');
end;

{ A toolbar predicate and a header predicate share the same real query owner.
  One sort has an explicit current owner; no per-surface sort history exists. }
procedure SharedCommands;
var
  LFixture: TFixture;
  LBefore: INyxCollectionSnapshot;
  LSnapshot: INyxCollectionQuerySessionSnapshot;
  LOldCapture: INyxCollectionQuerySessionSnapshot;
  LContribution: TNyxQueryContribution;
begin
  LFixture := TFixture.Create;
  try
    LBefore := LFixture.Store.Snapshot;
    Check(SameQuery(LFixture.Session.Capture.Baseline, Baseline)
      and (Rows(LFixture.View) = 'f|d|c|a'),
      'initial session baseline is exactly the accepted authored query');
    Apply(LFixture.Session, LFixture.A, NyxQuerySessionEdits
      .Filter(Search, NyxWhere(NyxIntegerField('score')).AtLeast(3))
      .SortBy(ScoreSort(nsdAscending)));
    Check(Rows(LFixture.View) = 'c|a|f',
      'first owner filters actual rows and applies typed shared numeric sort');
    LOldCapture := LFixture.Session.Capture;
    Apply(LFixture.Session, LFixture.B, NyxQuerySessionEdits
      .Filter(Team, NyxWhere(NyxTextField('team')).EqualTo('red')));
    Check(Rows(LFixture.View) = 'a|f',
      'independent owner predicate ANDs with baseline and peer contribution');
    Check(LFixture.View.Selection.Contains(Item('d')),
      'filtering preserves the hidden selected source identity');
    LSnapshot := LFixture.Session.Capture;
    Check((LSnapshot.Count = 2) and LSnapshot.HasUserSort
      and (LSnapshot.SortOwner.ID.Value = LFixture.A.ID.Value)
      and (LSnapshot.SortOwner.Origin.Name = 'toolbar')
      and (LSnapshot.SortOwner.Caption = 'Search toolbar'),
      'copied snapshot advertises independent contributions and sort-only owner');
    LContribution := LSnapshot.ContributionAt(0);
    Check(LContribution.Owner.ID.Defined and LContribution.Owner.Connected
      and (LContribution.Filter.Kind = ndObject),
      'public contribution metadata is a closed predicate with copied owner');
    Check(LOldCapture.Count = 1,
      'an earlier snapshot does not grow when another owner contributes');
    Refuse(LFixture.Session, LOldCapture, LFixture.A,
      NyxQuerySessionEdits.ClearOwner,
      'whole-session capture refuses a stale Clear after a peer contribution');
    Apply(LFixture.Session, LFixture.B,
      NyxQuerySessionEdits.SortBy(ScoreSort(nsdDescending)));
    Check((Rows(LFixture.View) = 'f|a')
      and (LFixture.Session.Capture.SortOwner.ID.Value = LFixture.B.ID.Value),
      'second surface replaces the one shared sort with explicit ownership');
    Apply(LFixture.Session, LFixture.A, NyxQuerySessionEdits.ClearOwner);
    Check((Rows(LFixture.View) = 'f|a|d')
      and (LFixture.Session.Capture.Count = 1)
      and (LFixture.Session.Capture.SortOwner.ID.Value = LFixture.B.ID.Value),
      'owner Clear preserves peer predicate and sort instead of stale rollback');
    Apply(LFixture.Session, LFixture.B, NyxQuerySessionEdits.ClearOwner);
    Check((Rows(LFixture.View) = 'f|d|c|a')
      and (LFixture.Session.Capture.Count = 0)
      and not LFixture.Session.Capture.HasUserSort,
      'current sort owner Clear returns to the exact admitted baseline');
    Check((LFixture.Store.Snapshot = LBefore)
      and (LFixture.Store.Snapshot.Revision = 0)
      and (Rows(LFixture.Peer) = 'f|d|c|a'),
      'shared commands change no rows/defaults or independent peer view');
  finally
    LSnapshot := nil;
    LOldCapture := nil;
    LFixture.Free;
  end;
end;

{ Explicit global resets and empty sorting are different typed intents. }
procedure SortAndReset;
var
  LFixture: TFixture;
begin
  LFixture := TFixture.Create;
  try
    Apply(LFixture.Session, LFixture.A, NyxQuerySessionEdits
      .Filter(Search, NyxWhere(NyxIntegerField('score')).AtLeast(3))
      .SortBy(ScoreSort(nsdDescending)));
    Apply(LFixture.Session, LFixture.B, NyxQuerySessionEdits
      .Filter(Team, NyxWhere(NyxTextField('team')).EqualTo('blue')));
    Check(Rows(LFixture.View) = 'c', 'combined filters can narrow to one real row');
    Apply(LFixture.Session, LFixture.A, NyxQuerySessionEdits.ClearAllFilters);
    Check((Rows(LFixture.View) = 'f|a|c|d')
      and (LFixture.Session.Capture.Count = 0)
      and LFixture.Session.Capture.HasUserSort,
      'ClearAllFilters preserves the current shared sort');
    Apply(LFixture.Session, LFixture.B,
      NyxQuerySessionEdits.SortBy(NyxCollectionQuery));
    Check((Rows(LFixture.View) = 'a|c|d|f')
      and LFixture.Session.Capture.HasUserSort
      and (LFixture.Session.Capture.UserSort.SortCount = 0),
      'empty user sort explicitly selects source order rather than baseline');
    Apply(LFixture.Session, LFixture.A, NyxQuerySessionEdits.ClearOwner);
    Check((Rows(LFixture.View) = 'a|c|d|f')
      and LFixture.Session.Capture.HasUserSort,
      'a peer Clear cannot reset another owner explicit unsorted choice');
    Apply(LFixture.Session, LFixture.A, NyxQuerySessionEdits.ResetSort);
    Check((Rows(LFixture.View) = 'f|d|c|a')
      and not LFixture.Session.Capture.HasUserSort,
      'explicit ResetSort restores admitted baseline ordering');
    Apply(LFixture.Session, LFixture.B, NyxQuerySessionEdits
      .Filter(Team, NyxWhere(NyxTextField('team')).EqualTo('missing'))
      .SortBy(ScoreSort(nsdAscending)));
    Check(LFixture.View.Snapshot.Count = 0,
      'a legitimate user contribution can produce an empty result');
    Apply(LFixture.Session, LFixture.A, NyxQuerySessionEdits.ResetAll);
    Check((Rows(LFixture.View) = 'f|d|c|a')
      and (LFixture.Session.Capture.Count = 0)
      and not LFixture.Session.Capture.HasUserSort,
      'one ResetAll clears user predicates and sort without removing baseline');
  finally
    LFixture.Free;
  end;
end;

{ Canonical metadata, computed rows and real scalar resets publish together.
  A no-op preserves authority, while same-sort ownership is an actual command. }
procedure GroupedAndNoOp;
var
  LFixture: TFixture;
  LQuery: INyxPreparedPublication;
  LState: INyxPreparedPublication;
  LCapture: INyxCollectionQuerySessionSnapshot;
  LBefore: INyxCollectionSnapshot;
  LEdits: TNyxQuerySessionEdits;
begin
  LFixture := TFixture.Create;
  try
    LFixture.Attach;
    LFixture.Expect('c|a|f', 'after', 1);
    LBefore := LFixture.View.Snapshot;
    LEdits := NyxQuerySessionEdits.Filter(Search,
      NyxWhere(NyxIntegerField('score')).AtLeast(3))
      .SortBy(ScoreSort(nsdAscending));
    LQuery := LFixture.Session.PrepareSession(LFixture.Session.Capture,
      LFixture.A, LEdits);
    LState := LFixture.State.PrepareApply([
      NyxStateValue(NyxTextState('phase'), 'after')]);
    Check((LFixture.View.Snapshot = LBefore)
      and (LFixture.Session.Capture.Count = 0)
      and (LFixture.State.GetValue(NyxTextState('phase')) = 'before')
      and (LFixture.Calls = 0) and (LFixture.StateCalls = 0),
      'preparation publishes no rows, metadata, scalar reset or notification');
    PublishNyxGroup([LState, LQuery]);
    Check(LFixture.Coherent and (LFixture.Calls = 1)
      and (LFixture.StateCalls = 1),
      'each real observer sees committed rows, contributions and scalar state');
    LQuery := nil;
    LState := nil;
    LBefore := LFixture.View.Snapshot;
    LCapture := LFixture.Session.Capture;
    Apply(LFixture.Session, LFixture.A, LEdits);
    Check((LFixture.View.Snapshot = LBefore) and (LFixture.Calls = 1),
      'same-owner same-filter and same-sort batch is a true silent no-op');
    LQuery := LFixture.Session.PrepareSession(LCapture, LFixture.A,
      NyxQuerySessionEdits.ClearFilter(Search));
    LQuery.Validate;
    LQuery.Retire;
    LQuery := nil;
    Check((LFixture.View.Snapshot = LBefore)
      and (LFixture.Session.Capture.Count = 1),
      'no-op preserves capture authority and abandoned edit keeps accepted state');
    Apply(LFixture.Session, LFixture.B,
      NyxQuerySessionEdits.SortBy(ScoreSort(nsdAscending)));
    Check((Rows(LFixture.View) = 'c|a|f')
      and (LFixture.Session.Capture.SortOwner.ID.Value = LFixture.B.ID.Value),
      'same sort keys with another owner commit the new ownership claim');
    Refuse(LFixture.Session, LCapture, LFixture.A,
      NyxQuerySessionEdits.ClearOwner,
      'metadata-only ownership change invalidates the earlier global intent');
    LCapture := LFixture.Session.Capture;
    LBefore := LFixture.View.Snapshot;
    LFixture.View.ConfigureQuery(LFixture.View.QueryPolicy);
    Check(LFixture.A.Connected and LFixture.B.Connected
      and (LFixture.View.Snapshot = LBefore)
      and (LFixture.Session.Capture.SortOwner.ID.Value = LFixture.B.ID.Value),
      'equal raw ConfigureQuery preserves session generation and sort owner');
    LQuery := LFixture.Session.PrepareSession(LCapture, LFixture.B,
      NyxQuerySessionEdits.ClearOwner);
    LQuery.Validate;
    LQuery.Retire;
    LQuery := nil;
    Check(LFixture.Coherent, 'all metadata-only observers retain coherent models');
  finally

    if LQuery <> nil then
    begin
      LQuery.Retire;
    end;

    if LState <> nil then
    begin
      LState.Retire;
    end;
    LCapture := nil;
    LQuery := nil;
    LState := nil;
    LFixture.Free;
  end;
end;

{ A refused group and a later actor revocation leave every accepted model exact.
  The revoked actor itself is deliberately a separate owned lifetime fact. }
procedure RefusalAndFinalFence;
var
  LFixture: TFixture;
  LQuery: INyxPreparedPublication;
  LState: INyxPreparedPublication;
  LGate: INyxPreparedPublication;
  LGateObject: TGate;
  LBefore: INyxCollectionSnapshot;
  LRefused: Boolean;
begin
  LFixture := TFixture.Create;
  try
    LFixture.Attach;
    LBefore := LFixture.View.Snapshot;
    LQuery := LFixture.Session.PrepareSession(LFixture.Session.Capture,
      LFixture.A, NyxQuerySessionEdits.Filter(Search,
      NyxWhere(NyxIntegerField('score')).AtLeast(3)));
    LState := LFixture.State.PrepareApply([
      NyxStateValue(NyxTextState('phase'), 'after')]);
    LGateObject := TGate.Create;
    LGate := LGateObject;
    LGateObject.Refuse := True;
    LRefused := False;
    try
      PublishNyxGroup([LQuery, LState, LGate]);
    except
      on E: ENyxCollection do
      begin
        LRefused := E.Message = 'Expected later session refusal';
      end;
    end;
    Check(LRefused and (LGateObject.Installs = 0)
      and (LGateObject.Notifications = 0) and LGateObject.Retired,
      'later validation refuses and retires without installation');
    Check((LFixture.View.Snapshot = LBefore)
      and (LFixture.Session.Capture.Count = 0)
      and (LFixture.State.GetValue(NyxTextState('phase')) = 'before')
      and (LFixture.State.Revision = 1)
      and (LFixture.Calls = 0) and (LFixture.StateCalls = 0),
      'late refusal leaves exact rows, metadata, state and notifications');
    LQuery := nil;
    LState := nil;
    LGate := nil;
    LQuery := LFixture.Session.PrepareSession(LFixture.Session.Capture,
      LFixture.A, NyxQuerySessionEdits.Filter(Search,
      NyxWhere(NyxIntegerField('score')).AtLeast(3)));
    LState := LFixture.State.PrepareApply([
      NyxStateValue(NyxTextState('phase'), 'after')]);
    LGateObject := TGate.Create;
    LGate := LGateObject;
    LGateObject.Actor := LFixture.A;
    LRefused := False;
    try
      PublishNyxGroup([LQuery, LState, LGate]);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LFixture.A.Connected
      and (LGateObject.Installs = 0),
      'later pure actor revocation is caught by the final readiness fence');
    Check((LFixture.View.Snapshot = LBefore)
      and (LFixture.Session.Capture.Count = 0)
      and (LFixture.State.GetValue(NyxTextState('phase')) = 'before')
      and (LFixture.Calls = 0) and (LFixture.StateCalls = 0),
      'final-fence refusal publishes neither canonical session nor scalar reset');
    LQuery := nil;
    LState := nil;
    LGate := nil;
    Apply(LFixture.Session, LFixture.B, NyxQuerySessionEdits.Filter(Team,
      NyxWhere(NyxTextField('team')).EqualTo('red')));
    Check(Rows(LFixture.View) = 'f|d|a',
      'independent actor works after refused preparation retirement');
  finally

    if LQuery <> nil then
    begin
      LQuery.Retire;
    end;

    if LState <> nil then
    begin
      LState.Retire;
    end;
    LGate := nil;
    LQuery := nil;
    LState := nil;
    LFixture.Free;
  end;
end;

{ Lease revocation and final release never silently roll back a user's query.
  New same-named actors cannot steal the previous identity; chip removal is explicit. }
procedure OrphansAndIdentity;
var
  LFixture: TFixture;
  LOld: INyxCollectionQuerySessionSnapshot;
  LNewActor: INyxCollectionQueryOwner;
  LBefore: INyxCollectionSnapshot;
  LContribution: TNyxQueryContribution;
  LOldID: TNyxQueryOwnerID;
begin
  LFixture := TFixture.Create;
  try
    LFixture.Attach;
    Apply(LFixture.Session, LFixture.A, NyxQuerySessionEdits
      .Filter(Search, NyxWhere(NyxIntegerField('score')).AtLeast(3)));
    LOld := LFixture.Session.Capture;
    LOldID := LFixture.A.ID;
    LBefore := LFixture.View.Snapshot;
    LFixture.A.Revoke;
    LFixture.A.Revoke;
    Check(not LFixture.A.Connected and (LFixture.View.Snapshot = LBefore)
      and (LFixture.Calls = 1),
      'repeated owner revocation changes flags only, not rows or notification');
    LContribution := LFixture.Session.Capture.ContributionAt(0);
    Check(not LContribution.Owner.Connected
      and LOld.ContributionAt(0).Owner.Connected,
      'fresh snapshot exposes orphan while prior copied liveness remains exact');
    Refuse(LFixture.Session, LFixture.Session.Capture, LFixture.A,
      NyxQuerySessionEdits.ClearOwner, 'revoked actor cannot clear its orphan');
    LFixture.A := nil;
    LNewActor := LFixture.Session.AcquireOwner(NyxQueryOrigin('toolbar'),
      'Replacement toolbar');
    Check(LNewActor.ID.Defined and (LNewActor.ID.Value <> LOldID.Value),
      'same-named new actor has an independent exact identity');
    Apply(LFixture.Session, LNewActor, NyxQuerySessionEdits.ClearOwner);
    Check((LFixture.View.Snapshot = LBefore)
      and (LFixture.Session.Capture.Count = 1),
      'new same-named Clear cannot steal the previous orphan');
    Apply(LFixture.Session, LNewActor, NyxQuerySessionEdits
      .RemoveFilter(LOldID, Search)
      .Filter(Search, NyxWhere(NyxIntegerField('score')).AtLeast(3)));
    Check((LFixture.Session.Capture.Count = 1)
      and (LFixture.Session.Capture.ContributionAt(0).Owner.ID.Value =
        LNewActor.ID.Value) and (Rows(LFixture.View) = 'f|c|a'),
      'explicit remove-and-filter transfers copied intent atomically to new owner');
    LBefore := LFixture.View.Snapshot;
    LNewActor := nil;
    Check((LFixture.View.Snapshot = LBefore)
      and not LFixture.Session.Capture.ContributionAt(0).Owner.Connected,
      'last lease release leaves its accepted predicate as a visible orphan');
    Apply(LFixture.Session, LFixture.B, NyxQuerySessionEdits.ResetAll);
    Check((Rows(LFixture.View) = 'f|d|c|a')
      and (LFixture.Session.Capture.Count = 0),
      'explicit global reset is useful orphan recovery');
  finally
    LOld := nil;
    LNewActor := nil;
    LFixture.Free;
  end;
end;

{ CurrentQuery is default even after a legacy override. Only explicit checked
  authored rebase can replace that admitted baseline; raw takeover revokes leases. }
procedure RawAndRebase;
var
  LFixture: TFixture;
  LRuntime: TNyxCollectionQuery;
  LCapture: INyxCollectionQuerySessionSnapshot;
  LQuery: INyxPreparedPublication;
  LNewActor: INyxCollectionQueryOwner;
  LPeerActor: INyxCollectionQueryOwner;
  LRefused: Boolean;
  LBefore: INyxCollectionSnapshot;
begin
  LFixture := TFixture.Create;
  try
    LRuntime := NyxCollectionQuery
      .Where(NyxWhere(NyxIntegerField('score')).AtLeast(2))
      .OrderBy(NyxIntegerField('score'), nsdDescending);
    LFixture.View.ConfigureQuery(LRuntime);
    Check(not LFixture.A.Connected and not LFixture.B.Connected,
      'actual raw ConfigureQuery revokes the former contribution generation');
    LNewActor := LFixture.Session.AcquireOwner(NyxQueryOrigin('fresh'),
      'Fresh toolbar');
    Check(SameQuery(LFixture.Session.Capture.Baseline, LRuntime)
      and (Rows(LFixture.View) = 'f|e|a|c|d'),
      'new session joins the exact accepted runtime policy rather than authored text');
    LCapture := LFixture.Session.Capture;
    Apply(LFixture.Session, LNewActor, NyxQuerySessionEdits.Filter(Search,
      NyxWhere(NyxTextField('team')).EqualTo('blue')));
    Check(Rows(LFixture.View) = 'e|c',
      'user predicate composes with runtime baseline including inactive source row');
    LFixture.Session.AcquireOwner(NyxQueryOrigin('temporary'), 'Temporary');
    Apply(LFixture.Session, LNewActor, NyxQuerySessionEdits.ClearOwner);
    Check(SameQuery(LFixture.Session.Capture.Baseline, LRuntime)
      and (Rows(LFixture.View) = 'f|e|a|c|d'),
      'additional lease and Clear never recapture composed query or restore authored');
    Refuse(LFixture.Session, LCapture, LNewActor, NyxQuerySessionEdits.ResetAll,
      'session A-to-B-to-A canonical changes cannot revive an old capture');
    LBefore := LFixture.View.Snapshot;
    LRefused := False;
    try
      LQuery := LFixture.Session.PrepareRebase(LFixture.Session.Capture,
        LNewActor, Spec.Column(NyxTextField('team'), 'Other'));
      LQuery.Validate;
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;

    if LQuery <> nil then
    begin
      LQuery.Retire;
      LQuery := nil;
    end;
    Check(LRefused and (LFixture.View.Snapshot = LBefore)
      and SameQuery(LFixture.Session.Capture.Baseline, LRuntime),
      'authored rebase refuses an owner proof with a different complete spec');
    LQuery := LFixture.Session.PrepareRebase(LFixture.Session.Capture,
      LNewActor, Spec);
    PublishNyxGroup([LQuery]);
    LQuery := nil;
    Check(SameQuery(LFixture.Session.Capture.Baseline, Baseline)
      and (Rows(LFixture.View) = 'f|d|c|a'),
      'explicit full-spec authored rebase restores the admitted authored policy');
    LNewActor := LFixture.Session.AcquireOwner(NyxQueryOrigin('after-rebase'),
      'After rebase');
    LCapture := LFixture.Session.Capture;
    LFixture.View.ConfigureQuery(LRuntime);
    LFixture.View.ConfigureQuery(Baseline);
    Refuse(LFixture.Session, LCapture, LNewActor, NyxQuerySessionEdits.ResetAll,
      'raw A-to-B-to-A invalidates former session identity and lease');
    LNewActor := LFixture.Session.AcquireOwner(NyxQueryOrigin('latest'),
      'Latest toolbar');
    LPeerActor := LFixture.PeerSession.AcquireOwner(NyxQueryOrigin('peer'),
      'Independent peer');
    Refuse(LFixture.PeerSession, LFixture.Session.Capture,
      LPeerActor, NyxQuerySessionEdits.ResetAll,
      'same-valued peer cannot accept a foreign exact-session capture');
    Refuse(LFixture.PeerSession, LFixture.PeerSession.Capture,
      LNewActor, NyxQuerySessionEdits.ResetAll,
      'same-valued peer cannot accept an otherwise connected foreign actor');
  finally

    if LQuery <> nil then
    begin
      LQuery.Retire;
    end;
    LQuery := nil;
    LNewActor := nil;
    LPeerActor := nil;
    LCapture := nil;
    LFixture.Free;
  end;
end;

{ Invalid schema commands and reserved source/selection writes refuse through
  the existing model owners. Losing the last actor lease revokes its fence. }
procedure AdmissionAndLeaseLoss;
var
  LFixture: TFixture;
  LStage: INyxPreparedPublication;
  LBefore: INyxCollectionSnapshot;
  LSource: INyxCollectionSnapshot;
  LRefused: Boolean;
begin
  LFixture := TFixture.Create;
  try
    LFixture.Attach;
    LBefore := LFixture.View.Snapshot;
    LSource := LFixture.Store.Snapshot;
    Refuse(LFixture.Session, LFixture.Session.Capture, LFixture.A,
      NyxQuerySessionEdits.Filter(Search,
      NyxWhere(NyxTextField('missing')).EqualTo('not admitted')),
      'wrong schema field refuses the entire canonical session candidate');
    Refuse(LFixture.Session, LFixture.Session.Capture, LFixture.A,
      NyxQuerySessionEdits.SortBy(
      NyxCollectionQuery.OrderBy(NyxTextField('score'))),
      'shared sort refuses a mismatched typed field family');
    Check((LFixture.View.Snapshot = LBefore)
      and (LFixture.Session.Capture.Count = 0) and (LFixture.Calls = 0),
      'failed schema admission preserves rows, metadata and notifications');
    LStage := LFixture.Session.PrepareSession(LFixture.Session.Capture,
      LFixture.A, NyxQuerySessionEdits.Filter(Search,
      NyxWhere(NyxIntegerField('score')).AtLeast(3)));
    Refuse(LFixture.Session, LFixture.Session.Capture, LFixture.B,
      NyxQuerySessionEdits.ClearOwner,
      'peer actor cannot bypass the same existing view reservation');
    LRefused := False;
    try
      LFixture.View.ClearSelection;
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'selection cannot mutate a reserved shared-query candidate');
    LRefused := False;
    try
      LFixture.Store.Update(Person('a', 'Changed', 4, True, 'red'));
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFixture.Store.Snapshot = LSource)
      and (LFixture.Peer.Snapshot.Revision = 0),
      'same-source row publication cannot partially commit through session stage');
    LFixture.A := nil;
    LRefused := False;
    try
      PublishNyxGroup([LStage]);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFixture.View.Snapshot = LBefore)
      and (LFixture.Session.Capture.Count = 0) and (LFixture.Calls = 0),
      'last actor lease release revokes staged authority without publishing');
    LStage := nil;
    Apply(LFixture.Session, LFixture.B, NyxQuerySessionEdits.Filter(Team,
      NyxWhere(NyxTextField('team')).EqualTo('blue')));
    Check(Rows(LFixture.View) = 'c',
      'independent actor works after lease-loss retirement');
  finally

    if LStage <> nil then
    begin
      LStage.Retire;
    end;
    LStage := nil;
    LFixture.Free;
  end;
end;

{ Retained public snapshots/owner flags are not hidden view owners. A borrowed
  token observes actual view retirement; copied historical liveness stays data. }
procedure SnapshotLifetime;
var
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LSession: INyxCollectionQuerySession;
  LActor: INyxCollectionQueryOwner;
  LSnapshot: INyxCollectionQuerySessionSnapshot;
  LToken: INyxCollectionViewSubscription;
  LObservation: TObservation;
begin
  LObservation := TObservation.Create;
  try
    LStore := NewStore;
    LView := NewNyxCollectionView(LStore, Spec, cpTable);
    LSession := NyxCollectionQuerySession(LView);
    LActor := LSession.AcquireOwner(NyxQueryOrigin('temporary'), 'Temporary');
    Apply(LSession, LActor, NyxQuerySessionEdits.Filter(Search,
      NyxWhere(NyxIntegerField('score')).AtLeast(3)));
    LSnapshot := LSession.Capture;
    LToken := LView.Subscribe(LObservation.Changed);
    LSession := nil;
    LView := nil;
    LStore := nil;
    Check(not LToken.Connected and not LActor.Connected,
      'snapshot and retained actor do not keep the retired view alive');
    Check((LSnapshot.Count = 1) and LSnapshot.ContributionAt(0).Owner.Connected
      and SameQuery(LSnapshot.Baseline, Baseline),
      'copied session data remains readable after its source view retires');
  finally

    if LToken <> nil then
    begin
      LToken.Disconnect;
    end;
    LToken := nil;
    LSnapshot := nil;
    LActor := nil;
    LSession := nil;
    LView := nil;
    LStore := nil;
    LObservation.Free;
  end;
end;

{ Import is an explicit copied metadata takeover. The source can keep changing
  independently; it is not retained/revoked or promised latest-at-commit. }
procedure CopiedImport;
var
  LFixture: TFixture;
  LTarget: INyxCollectionView;
  LSession: INyxCollectionQuerySession;
  LActor: INyxCollectionQueryOwner;
  LOldActor: INyxCollectionQueryOwner;
  LSource: INyxCollectionQuerySessionSnapshot;
  LTargetCapture: INyxCollectionQuerySessionSnapshot;
  LImported: INyxCollectionQuerySessionSnapshot;
  LStage: INyxPreparedPublication;
  LBefore: INyxCollectionSnapshot;
  LContribution: TNyxQueryContribution;
begin
  LFixture := TFixture.Create;
  try
    Apply(LFixture.Session, LFixture.A, NyxQuerySessionEdits
      .Filter(Search, NyxWhere(NyxIntegerField('score')).AtLeast(3))
      .SortBy(ScoreSort(nsdDescending)));
    LSource := LFixture.Session.Capture;
    LTarget := NewNyxCollectionView(LFixture.Store, Spec, cpTable);
    LSession := NyxCollectionQuerySession(LTarget);
    LOldActor := LSession.AcquireOwner(NyxQueryOrigin('old-target'),
      'Previous target');
    Apply(LSession, LOldActor, NyxQuerySessionEdits.Filter(Team,
      NyxWhere(NyxTextField('team')).EqualTo('blue')));
    LTargetCapture := LSession.Capture;
    LBefore := LTarget.Snapshot;
    LStage := LSession.PrepareImport(LTargetCapture, LSource);
    Check((LTarget.Snapshot = LBefore) and LOldActor.Connected,
      'prepared metadata takeover preserves accepted target and live actor');
    Apply(LFixture.Session, LFixture.B, NyxQuerySessionEdits.Filter(Team,
      NyxWhere(NyxTextField('team')).EqualTo('red')));
    PublishNyxGroup([LStage]);
    LStage := nil;
    LImported := LSession.Capture;
    Check((Rows(LTarget) = 'f|a|c') and (LImported.Count = 1)
      and SameQuery(LImported.Baseline, LSource.Baseline),
      'import recomputes actual target rows from the exact copied snapshot');
    Check((Rows(LFixture.View) = 'f|a')
      and LFixture.A.Connected and LFixture.B.Connected
      and (LFixture.Session.Capture.Count = 2),
      'later source query stays independent and its actors remain live');
    LContribution := LImported.ContributionAt(0);
    Check(not LOldActor.Connected and not LContribution.Owner.Connected
      and not LImported.SortOwner.Connected
      and (LContribution.Owner.Origin.Name = 'toolbar')
      and (LContribution.Owner.Caption = 'Search toolbar')
      and (LContribution.Filter.ToJSON =
        LSource.ContributionAt(0).Filter.ToJSON),
      'target generation revokes its old actor and copies only orphan metadata');
    LActor := LSession.AcquireOwner(NyxQueryOrigin('toolbar'),
      'New target toolbar');
    Refuse(LSession, LTargetCapture, LActor, NyxQuerySessionEdits.ResetAll,
      'target capture before import cannot revive old generation');
    Refuse(LSession, LSource, LActor, NyxQuerySessionEdits.ResetAll,
      'source data snapshot is not target command authority');
    Refuse(LSession, LSession.Capture, LFixture.A,
      NyxQuerySessionEdits.ClearOwner,
      'source live owner does not become target authority through metadata copy');
    LBefore := LTarget.Snapshot;
    Apply(LSession, LActor, NyxQuerySessionEdits.ClearOwner);
    Check((LTarget.Snapshot = LBefore) and (LSession.Capture.Count = 1),
      'same-named target actor cannot silently steal imported orphan');
    Apply(LSession, LActor, NyxQuerySessionEdits
      .RemoveFilter(LContribution.Owner.ID, Search)
      .Filter(Search, NyxWhere(NyxTextField('team')).EqualTo('red')));
    Check((Rows(LTarget) = 'f|a|d')
      and (LSession.Capture.ContributionAt(0).Owner.ID.Value = LActor.ID.Value),
      'explicit chip withdrawal and new filter publish together after import');
    Check((LSource.Count = 1) and LSource.ContributionAt(0).Owner.Connected,
      'retained source snapshot remains historical copied data');
  finally

    if LStage <> nil then
    begin
      LStage.Retire;
    end;
    LStage := nil;
    LImported := nil;
    LTargetCapture := nil;
    LSource := nil;
    LActor := nil;
    LOldActor := nil;
    LSession := nil;
    LTarget := nil;
    LFixture.Free;
  end;
end;

procedure RefuseTransfer(const AFormer, ANext: INyxCollectionView;
  const AReason: TNyxText);
var
  LStage: INyxPreparedPublication;
  LRefused: Boolean;
begin
  LRefused := False;
  try
    try
      LStage := PrepareNyxCollectionQueryTransfer(AFormer, ANext);

      if LStage <> nil then
      begin
        LStage.Validate;
      end;
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
  finally

    if LStage <> nil then
    begin
      LStage.Retire;
    end;
  end;
  Check(LRefused, AReason);
end;

{ Actual existing binding recomposition uses its own realized tree/context.
  Data/spec matches transfer copied provenance before paging and selection. }
procedure BindingRecompose;
var
  LDocument: TNyxDocument;
  LPage: TNyxNode;
  LTable: TNyxNode;
  LRuntime: TNyxNode;
  LContext: INyxCollectionContext;
  LBindings: INyxCollectionBindings;
  LNext: INyxCollectionBindings;
  LView: INyxCollectionView;
  LNextView: INyxCollectionView;
  LSession: INyxCollectionQuerySession;
  LNextSession: INyxCollectionQuerySession;
  LActor: INyxCollectionQueryOwner;
  LNewActor: INyxCollectionQueryOwner;
  LSnapshot: INyxCollectionQuerySessionSnapshot;
  LPages: INyxCollectionPages;
  LOther: INyxCollectionView;
  LOtherStore: INyxCollection;
  LDefaults: INyxCollection;
  LObservation: TObservation;
  LToken: INyxCollectionViewSubscription;
  LBefore: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  LRuntime := nil;
  LObservation := TObservation.Create;
  try
    LDefaults := NewStore;
    LDocument.Collections.Define(LDefaults.Snapshot);
    LPage := TNyxNode.Create(nkPage, 'home');
    LDocument.AddPage(LPage);
    LTable := TNyxNode.Create(nkTable, 'table');
    LTable.SetCollectionView(Spec);
    LPage.Add(LTable);
    LBefore := TNyxCodec.Encode(LDocument);
    LRuntime := RealizeNyxView(LDocument, LPage);
    LContext := NewNyxCollectionContext(LDocument.Collections);
    LBindings := NewNyxCollectionBindings(LRuntime, LContext);
    LView := LBindings.ViewFor('table');
    LSession := NyxCollectionQuerySession(LView);
    LActor := LSession.AcquireOwner(NyxQueryOrigin('toolbar'), 'Original');
    Apply(LSession, LActor, NyxQuerySessionEdits
      .Filter(Search, NyxWhere(NyxIntegerField('score')).AtLeast(3))
      .SortBy(ScoreSort(nsdAscending)));
    LView.SetSelection([Item('a'), Item('d')], Item('a'), Item('d'));
    LPages := NyxCollectionPages(LView);
    LPages.ConfigurePaging(NyxCollectionPaging.PageSize(2).InitialPage(2));
    LSnapshot := LSession.Capture;
    LToken := LView.Subscribe(LObservation.Changed);
    LNext := LBindings.Recompose(LRuntime);
    LNextView := LNext.ViewFor('table');
    LNextSession := NyxCollectionQuerySession(LNextView);
    Check((LView <> LNextView) and (LView.Store = LNextView.Store)
      and (LView.Spec.ToData.ToJSON = LNextView.Spec.ToData.ToJSON),
      'recompose creates independent views over the exact same store and spec');
    Check((LNextSession.Capture.Count = 1)
      and SameQuery(LNextSession.Capture.Baseline, LSnapshot.Baseline)
      and not LNextSession.Capture.ContributionAt(0).Owner.Connected
      and not LNextSession.Capture.SortOwner.Connected,
      'recompose preserves baseline/filter/sort provenance as disconnected copies');
    Check((NyxCollectionPages(LNextView).Window.Page = 2)
      and (Rows(LNextView) = 'f')
      and LNextView.Selection.Contains(Item('a'))
      and LNextView.Selection.Contains(Item('d')),
      'metadata transfer precedes actual page and hidden-selection reconciliation');
    Check(LActor.Connected and (LObservation.Calls = 0),
      'candidate recompose does not revoke or notify the accepted source actor');
    LNewActor := LNextSession.AcquireOwner(NyxQueryOrigin('toolbar'),
      'Replacement');
    Refuse(LNextSession, LNextSession.Capture, LActor,
      NyxQuerySessionEdits.ClearOwner,
      'old actor cannot address replacement even with a fresh target capture');
    Apply(LNextSession, LNewActor, NyxQuerySessionEdits.ClearOwner);
    Check((LNextSession.Capture.Count = 1) and (Rows(LNextView) = 'f'),
      'same-named new actor does not take over transferred orphan');
    LOtherStore := NewStore;
    LOther := NewNyxCollectionView(LOtherStore, Spec, cpTable);
    RefuseTransfer(LView, LOther,
      'checked transfer refuses identical data/schema from a different store');
    LOther := NewNyxCollectionView(LView.Store, Spec, cpList);
    RefuseTransfer(LView, LOther,
      'checked transfer refuses a different physical projection');
    LOther := NewNyxCollectionView(LView.Store,
      Spec.Column(NyxTextField('team'), 'Team'), cpTable);
    RefuseTransfer(LView, LOther,
      'checked transfer refuses a different complete view specification');
    LOther := nil;
    Apply(LSession, LActor, NyxQuerySessionEdits.Filter(Search,
      NyxWhere(NyxIntegerField('score')).AtLeast(4)));
    Check((Rows(LView) = 'a|f') and (Rows(LNextView) = 'f')
      and (LNextSession.Capture.Count = 1),
      'old source commands remain independent after exact snapshot transfer');
    LPages := nil;
    LSession := nil;
    LView := nil;
    LBindings := nil;
    Check(not LActor.Connected and not LToken.Connected,
      'source retirement revokes old actor while copied snapshot retains no view');
    Check((LSnapshot.Count = 1) and LSnapshot.ContributionAt(0).Owner.Connected
      and (Rows(LNextView) = 'f') and LNewActor.Connected,
      'new view and immutable source snapshot survive independent old retirement');
    Check((TNyxCodec.Encode(LDocument) = LBefore)
      and (LNextView.Store.Snapshot.Revision = 0)
      and (LNextView.Store.Snapshot.Count = 6),
      'recompose and session commands never rewrite authoring or source defaults');
  finally

    if LToken <> nil then
    begin
      LToken.Disconnect;
    end;
    LToken := nil;
    LSnapshot := nil;
    LPages := nil;
    LOther := nil;
    LOtherStore := nil;
    LActor := nil;
    LNewActor := nil;
    LSession := nil;
    LNextSession := nil;
    LView := nil;
    LNextView := nil;
    LBindings := nil;
    LNext := nil;
    LContext := nil;
    LDefaults := nil;
    LRuntime.Free;
    LDocument.Free;
    LObservation.Free;
  end;
end;

{ A failed observer reports committed metadata/output and does not skip the
  real scalar observer or an independent later participant notification. }
procedure CommittedFailure;
var
  LFixture: TFixture;
  LQuery: INyxPreparedPublication;
  LState: INyxPreparedPublication;
  LGate: INyxPreparedPublication;
  LGateObject: TGate;
  LFailure: Boolean;
begin
  LFixture := TFixture.Create;
  try
    LFixture.Attach;
    LFixture.Expect('f|c|a', 'after', 1);
    LFixture.FailObserver := True;
    LQuery := LFixture.Session.PrepareSession(LFixture.Session.Capture,
      LFixture.A, NyxQuerySessionEdits.Filter(Search,
      NyxWhere(NyxIntegerField('score')).AtLeast(3)));
    LState := LFixture.State.PrepareApply([
      NyxStateValue(NyxTextState('phase'), 'after')]);
    LGateObject := TGate.Create;
    LGate := LGateObject;
    LFailure := False;
    try
      PublishNyxGroup([LQuery, LState, LGate]);
    except
      on E: ENyxPublicationNotification do
      begin
        LFailure := Pos('Expected shared session observer failure', E.Message) > 0;
      end;
    end;
    Check(LFailure and LFixture.Coherent and (LFixture.Calls = 1)
      and (LFixture.StateCalls = 1),
      'observer failure reports committed coherent metadata/rows/scalar state');
    Check((LGateObject.Installs = 1) and (LGateObject.Notifications = 1)
      and LGateObject.Retired,
      'independent notification and retirement continue after observer failure');
    LQuery := nil;
    LState := nil;
    LGate := nil;
    LFixture.FailObserver := False;
    LFixture.Expect('f|d|c|a', 'after', 0);
    Apply(LFixture.Session, LFixture.A, NyxQuerySessionEdits.ClearOwner);
    Check(LFixture.Coherent and (LFixture.Calls = 2)
      and (Rows(LFixture.View) = 'f|d|c|a'),
      'a new prepared command works after committed-failure retirement');
  finally

    if LQuery <> nil then
    begin
      LQuery.Retire;
    end;

    if LState <> nil then
    begin
      LState.Retire;
    end;
    LQuery := nil;
    LState := nil;
    LGate := nil;
    LFixture.Free;
  end;
end;

begin
  try
    BatchValues;
    SharedCommands;
    SortAndReset;
    GroupedAndNoOp;
    RefusalAndFinalFence;
    OrphansAndIdentity;
    RawAndRebase;
    AdmissionAndLeaseLoss;
    SnapshotLifetime;
    CopiedImport;
    BindingRecompose;
    CommittedFailure;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks)
      + ' canonical query session checks';
    document.body.setAttribute('data-query-session', 'passed');
    document.body.setAttribute('data-query-session-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' canonical query session checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      document.body.setAttribute('data-query-session', 'failed');
      document.body.setAttribute('data-query-session-checks', IntToStr(GChecks));
      document.body.setAttribute('data-query-session-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message, ' at check count ', GChecks);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
begin
end.
