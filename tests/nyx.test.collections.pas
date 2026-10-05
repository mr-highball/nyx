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

unit nyx.test.collections;

{$mode delphi}{$H+}
{$codepage utf8}

interface

{ Shared public-contract consumer. Rejection, retained snapshots, scoped identity
  and callback lifetimes must execute on both FPC and pas2js, not merely compile. }
function RunNyxCollectionTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.state,
  nyx.contract,
  nyx.collections;

type
  TCollectionProbeMode = (cpmObserve, cpmAtomic, cpmReject, cpmReentrant,
    cpmDisconnect, cpmThrow, cpmReleaseOwner);
  TCollectionProbe = class
  public
    Store: INyxCollection;
    Last: INyxCollectionChanges;
    Proposed: INyxCollectionSnapshot;
    OtherToken: INyxCollectionSubscription;
    Mode: TCollectionProbeMode;
    Calls: Integer;
    Checks: Integer;
    procedure Validate(const ACandidate: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure Observe(const ACollection: INyxCollection;
      const AChanges: INyxCollectionChanges);
  end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxCollection.Create('FAIL collections: ' + AMessage);
  end;
  Inc(ACount);
end;

function CaptionField: TNyxTextFieldRef;
begin
  Result := NyxTextField('caption/🌙');
end;

function CountField: TNyxIntegerFieldRef;
begin
  Result := NyxIntegerField('count');
end;

function FixtureSchema: TNyxCollectionSchema;
begin
  Result := NyxCollectionSchema
    .Text(CaptionField, 'Unnamed / 🌙')
    .Boolean(NyxBooleanField('enabled'), False)
    .Integer(CountField, 0, NyxIntegerDomain.Range(0, 100))
    .Number(NyxNumberField('ratio'), 0.1, NyxNumberDomain.Range(0, 1));
end;

procedure TCollectionProbe.Validate(const ACandidate: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
var
  LRejected: Boolean;
  LToken: INyxCollectionSubscription;
begin
  Proposed := ACandidate;

  if Mode = cpmAtomic then
  begin
    Check((Store.Snapshot.Revision = 1) and (ACandidate.Revision = 2) and
      (Store.Snapshot.ItemAt(0).Ref.ID = 'a') and
      (ACandidate.ItemAt(0).Ref.ID = 'c') and
      (ACandidate.ItemAt(1).GetValue(CountField) = 42) and
      (AChanges.Count = 4), 'validator sees whole proposal and unchanged baseline', Checks);
  end;

  if Mode = cpmReject then
  begin
    raise ENyxCollection.Create('Rejected proposal / 🌙');
  end;

  if Mode = cpmReentrant then
  begin
    LRejected := False;
    try
      Store.Apply([]);
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'validator cannot mutate the live store', Checks);
    LRejected := False;
    try
      LToken := Store.Subscribe(Observe);
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LToken = nil), 'validator cannot add subscriptions', Checks);
  end;
end;

procedure TCollectionProbe.Observe(const ACollection: INyxCollection;
  const AChanges: INyxCollectionChanges);
var
  LRejected: Boolean;
  LToken: INyxCollectionSubscription;
begin
  Inc(Calls);
  Last := AChanges;

  if Mode = cpmAtomic then
  begin
    Check((ACollection.Snapshot.Revision = 2) and
      (AChanges.Before.ItemAt(0).GetValue(CountField) = 1) and
      (AChanges.After.ItemAt(1).GetValue(CountField) = 42),
      'observer sees committed revision and owned before/after datasets', Checks);
    Check((AChanges.Kind(0) = nceUpdate) and
      (AChanges.BeforeItem(0).GetValue(CountField) = 1) and
      (AChanges.AfterItem(0).GetValue(CountField) = 42) and
      (AChanges.Kind(1) = nceInsert) and (AChanges.BeforeIndex(1) = -1) and
      (AChanges.AfterIndex(1) = 1) and
      (AChanges.Kind(2) = nceMove) and (AChanges.BeforeIndex(2) = 0) and
      (AChanges.AfterIndex(2) = 2) and
      (AChanges.Kind(3) = nceRemove) and (AChanges.BeforeIndex(3) = 1) and
      (AChanges.AfterIndex(3) = -1),
      'ordered operation log retains exact per-step values and positions', Checks);
  end;

  if Mode = cpmReentrant then
  begin
    LRejected := False;
    try
      ACollection.Remove(AChanges.After.ItemAt(0).Ref);
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'observer cannot mutate the committed batch', Checks);
    LRejected := False;
    try
      LToken := ACollection.Subscribe(Observe);
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LToken = nil), 'observer cannot add subscriptions', Checks);
  end;

  if Mode = cpmDisconnect then
  begin
    { Cancellation is explicit even when a compiler keeps an interface return
      temporary alive until the calling routine exits. No extra reference can
      delay disconnecting a user-removed registration. }
    OtherToken.Disconnect;
    OtherToken := nil;
  end;

  if Mode = cpmThrow then
  begin
    raise ENyxCollection.Create('Observer failed / 🌙');
  end;

  if Mode = cpmReleaseOwner then
  begin
    Store := nil;
    Check(ACollection.Snapshot.Revision = AChanges.After.Revision,
      'callback can release last application owner while execution stays alive', Checks);
  end;
end;

function SameDataset(const ALeft, ARight: INyxCollectionSnapshot): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if (ALeft.Key.Name <> ARight.Key.Name) or (ALeft.Count <> ARight.Count) or
    not ALeft.Schema.SameSchema(ARight.Schema) then
  begin
    Exit;
  end;
  for LIndex := 0 to ALeft.Count - 1 do
  begin

    if not ALeft.ItemAt(LIndex).SameItem(ARight.ItemAt(LIndex)) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

procedure PrepareOwnerRelease(AProbe: TCollectionProbe;
  out AToken: INyxCollectionSubscription);
var
  LKey: TNyxCollectionRef;
begin
  { Isolate the last-owner check from ignored fluent interface results in the
    main journey. Both compilers may retain those temporaries to routine exit.
    This helper's complete return scope leaves one application-owned store. }
  LKey := NyxCollection('messages/🌙');
  AProbe.Store := NewNyxCollection(LKey, FixtureSchema);
  AProbe.Store.Apply([NyxInsert(0, NyxCollectionItem(NyxItem(LKey, 'a')))]);
  AToken := AProbe.Store.Subscribe(AProbe.Observe);
end;

function RunNyxCollectionTests: Integer;
var
  LKey: TNyxCollectionRef;
  LA: TNyxItemRef;
  LB: TNyxItemRef;
  LC: TNyxItemRef;
  LSchema: TNyxCollectionSchema;
  LExpanded: TNyxCollectionSchema;
  LRow: TNyxCollectionItem;
  LChangedRow: TNyxCollectionItem;
  LStore: INyxCollection;
  LClone: INyxCollection;
  LOther: INyxCollection;
  LBefore: INyxCollectionSnapshot;
  LChanges: INyxCollectionChanges;
  LProbe: TCollectionProbe;
  LSecond: TCollectionProbe;
  LToken: INyxCollectionSubscription;
  LOtherToken: INyxCollectionSubscription;
  LRejected: Boolean;
  LRevision: Integer;
  LCalls: Integer;
  LCase: Integer;
  LValue: TNyxStateValue;
  LRef: TNyxCollectionRef;
  LDefaultEdit: TNyxCollectionEdit;
  LDefaultSchema: TNyxCollectionSchema;
  LDefaultItem: TNyxItemRef;
  LDefaultField: TNyxTextFieldRef;
  LEdits: array of TNyxCollectionEdit;
  LTokens: array of INyxCollectionSubscription;
  LMessage: TNyxText;
  LUnicodeName: TNyxText;
  LBatch: Integer;
  LPayload: TNyxText;
begin
  Result := 0;
  LRef := Default(TNyxCollectionRef);
  LDefaultEdit := Default(TNyxCollectionEdit);
  LDefaultSchema := Default(TNyxCollectionSchema);
  LDefaultItem := Default(TNyxItemRef);
  LDefaultField := Default(TNyxTextFieldRef);
  LKey := NyxCollection('messages/🌙');
  LA := NyxItem(LKey, 'a');
  LB := NyxItem(LKey, 'b');
  LC := NyxItem(LKey, 'c');
  LSchema := FixtureSchema;
  LExpanded := LSchema.Text(NyxTextField('detail'), 'More / 漢字');
  Check((LSchema.Count = 4) and (LExpanded.Count = 5),
    'fluent schema extension preserves its original definition', Result);
  Check(LSchema.FieldAt(0).Name = TNyxText('caption/🌙'),
    'ordered schema exposes exact Unicode field identity', Result);
  Check(LSchema.FieldAt(2).Kind = nskInteger,
    'ordered schema exposes typed scalar families', Result);
  Check(LSchema.FieldAt(3).DefaultValue.SameValue(TNyxStateValue.FromNumber(0.1)),
    'schema retains the exact portable Double default', Result);
  LValue := LSchema.FieldAt(0).DefaultValue;
  LValue := TNyxStateValue.FromText('Changed locally');
  Check(LSchema.FieldAt(0).DefaultValue.TextValue = TNyxText('Unnamed / 🌙'),
    'a copied default cannot mutate its schema', Result);

  LRow := NyxCollectionItem(LA).WithValue(CaptionField, 'Alpha / 🌙 漢字' + #0 + 'tail')
    .WithValue(CountField, 1).WithValue(NyxBooleanField('enabled'), True);
  LChangedRow := LRow.WithValue(CaptionField, 'Independent');
  Check((LRow.GetValue(CaptionField) = TNyxText('Alpha / 🌙 漢字') + #0 + 'tail') and
    (LChangedRow.GetValue(CaptionField) = 'Independent'),
    'immutable row builders preserve supplementary Unicode/NUL and previous values', Result);
  LStore := NewNyxCollection(LKey, LSchema);
  LStore.Apply([NyxInsert(0, LRow), NyxInsert(1, NyxCollectionItem(LB).WithValue(CaptionField, 'Beta'))]);
  LBefore := LStore.Snapshot;
  Check((LBefore.Count = 2) and (LBefore.Revision = 1) and
    LBefore.Item(LB).FieldValue(3).SameValue(TNyxStateValue.FromNumber(0.1)) and
    not LBefore.Item(LB).GetValue(NyxBooleanField('enabled')),
    'one atomic batch admits ordered independent rows and schema defaults', Result);
  Check((LBefore.ItemAt(0).Count = 4) and (LBefore.ItemAt(0).FieldName(3) = 'ratio'),
    'materialized item fields follow deliberate schema order', Result);
  Check((LBefore.IndexOf(LB) = 1) and not LBefore.Has(LC) and (LBefore.DataBytes > 40),
    'indexed item identity and logical payload measurements remain typed', Result);
  LRow := LRow.WithValue(CountField, 20);
  Check(LStore.Snapshot.Item(LA).GetValue(CountField) = 1,
    'changing the caller row leaves admitted values independent', Result);

  LProbe := TCollectionProbe.Create;
  LSecond := TCollectionProbe.Create;
  try
    LProbe.Store := LStore;
    LProbe.Mode := cpmAtomic;
    LToken := LStore.Subscribe(LProbe.Observe, LProbe.Validate);
    LStore.Apply([
      NyxUpdate(NyxCollectionItem(LA).WithValue(CountField, 42)),
      NyxInsert(1, NyxCollectionItem(LC).WithValue(CaptionField, 'Charlie')),
      NyxMove(LA, 2), NyxRemove(LB)], 1);
    Check((LProbe.Calls = 1) and (LProbe.Checks = 3), 'one publication runs typed validator and observer', Result);
    Inc(Result, LProbe.Checks);
    LProbe.Checks := 0;
    LChanges := LProbe.Last;
    Check((LBefore.ItemAt(0).Ref.ID = 'a') and (LBefore.Item(LA).GetValue(CountField) = 1) and
      (LStore.Snapshot.ItemAt(0).Ref.ID = 'c') and (LStore.Snapshot.IndexOf(LA) = 1),
      'retained baseline and stable selection identity survive reorder/remove', Result);
    LChangedRow := LChanges.After.Item(LA).WithValue(CountField, 40);
    Check((LChanges.After.Item(LA).GetValue(CountField) = 42) and
      (LChangedRow.GetValue(CountField) = 40),
      'change snapshots expose independent immutable row copies', Result);

    LProbe.Mode := cpmObserve;
    for LCase := 0 to 14 do
    begin
      LBefore := LStore.Snapshot;
      LCalls := LProbe.Calls;
      LRejected := False;
      try
        case LCase of
          0: LStore.Insert(0, NyxCollectionItem(LA));
          1: LStore.Remove(NyxItem(NyxCollection('other'), 'a'));
          2: LStore.Update(NyxCollectionItem(LA).WithValue(NyxTextField('unknown'), 'No'));
          3: LStore.Update(NyxCollectionItem(LA).WithValue(NyxTextField('count'), '42'));
          4: LStore.Insert(-1, NyxCollectionItem(LB));
          5: LStore.Move(LA, LStore.Snapshot.Count);
          6: LStore.Move(LB, 0);
          7: LStore.Update(NyxCollectionItem(LA).WithValue(CountField, 101));
          8: LStore.Apply([], LBefore.Revision - 1);
          9: LStore.Apply([LDefaultEdit]);
          10: LStore.Apply([NyxUpdate(NyxCollectionItem(LA).WithValue(CountField, 2)),
            NyxInsert(0, NyxCollectionItem(LC))]);
          11: LStore.Snapshot.Item(LB);
          12: LStore.Snapshot.Item(LA).GetValue(NyxBooleanField('count'));
          13: LStore.Apply([], -2);
          14: LStore.Snapshot.ItemAt(-1);
        end;
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'invalid typed identity/schema/domain/order/batch access rejects case ' + IntToStr(LCase), Result);
      Check((LStore.Snapshot.Revision = LBefore.Revision) and
        SameDataset(LStore.Snapshot, LBefore) and (LProbe.Calls = LCalls),
        'rejection preserves complete dataset/revision/observers case ' + IntToStr(LCase), Result);
    end;

    LBefore := LStore.Snapshot;
    LRevision := LBefore.Revision;
    LCalls := LProbe.Calls;
    LStore.Update(NyxCollectionItem(LA));
    LStore.Move(LA, 1);
    LStore.Apply([NyxMove(LA, 0), NyxMove(LA, 1),
      NyxInsert(0, NyxCollectionItem(LB)), NyxRemove(LB)]);
    Check((LStore.Snapshot.Revision = LRevision) and (LProbe.Calls = LCalls) and
      SameDataset(LStore.Snapshot, LBefore), 'individual and net batch no-ops preserve revision/notification', Result);

    LProbe.Mode := cpmReject;
    LRejected := False;
    try
      LStore.Update(NyxCollectionItem(LA).WithValue(CountField, 43));
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and SameDataset(LStore.Snapshot, LBefore) and
      (LStore.Snapshot.Revision = LRevision), 'validator veto retains accepted values and revision', Result);
    Check((LProbe.Proposed.Item(LA).GetValue(CountField) = 43) and
      (LProbe.Proposed.Revision = LRevision + 1),
      'retained rejected proposal remains an independent owned snapshot', Result);

    LProbe.Mode := cpmReentrant;
    LStore.Update(NyxCollectionItem(LA).WithValue(CountField, 44));
    Check(LProbe.Checks = 4, 'both callback phases refuse writes/new listeners', Result);
    Inc(Result, LProbe.Checks);
    LProbe.Checks := 0;
    LProbe.Mode := cpmObserve;

    LClone := LStore.Clone;
    Check((LClone.Snapshot.Revision = LStore.Snapshot.Revision) and
      SameDataset(LClone.Snapshot, LStore.Snapshot), 'clone preserves independent data/revision without listeners', Result);
    LCalls := LProbe.Calls;
    LClone.Update(NyxCollectionItem(LA).WithValue(CountField, 5));
    Check((LStore.Snapshot.Item(LA).GetValue(CountField) = 44) and
      (LClone.Snapshot.Item(LA).GetValue(CountField) = 5) and (LProbe.Calls = LCalls),
      'cloned instance updates keep defaults/other store values and subscriptions independent', Result);
    LStore.Replace(NyxCollectionItem(LA).WithValue(CaptionField, 'Replacement'));
    Check((LStore.Snapshot.Item(LA).GetValue(CountField) = 0) and
      (LStore.Snapshot.Item(LA).GetValue(CaptionField) = 'Replacement'),
      'replace restores omitted defaults while update preserves omitted fields', Result);
    LStore.Assign(LClone.Snapshot);
    Check(SameDataset(LStore.Snapshot, LClone.Snapshot),
      'dataset assignment admits whole independent values/order', Result);
    Check((LProbe.Last.Count = 4) and (LProbe.Last.Kind(0) = nceRemove) and
      (LProbe.Last.BeforeIndex(0) = 1) and (LProbe.Last.Kind(2) = nceInsert) and
      (LProbe.Last.AfterIndex(2) = 0), 'full assignment reports replayable remove/insert steps', Result);

    LBefore := LStore.Snapshot;
    LCalls := LProbe.Calls;
    LStore.Assign(LBefore);
    Check((LStore.Snapshot.Revision = LBefore.Revision) and (LProbe.Calls = LCalls),
      'identical assignment is a no-op', Result);
    LOther := NewNyxCollection(NyxCollection('other'), LSchema);
    LRejected := False;
    try
      LStore.Assign(LOther.Snapshot);
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and SameDataset(LStore.Snapshot, LBefore),
      'foreign dataset assignment cannot erase the accepted scope', Result);
    LOther := NewNyxCollection(LKey, LExpanded);
    LRejected := False;
    try
      LStore.Assign(LOther.Snapshot);
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and SameDataset(LStore.Snapshot, LBefore),
      'different schema assignment cannot replace a live contract', Result);

    LSecond.Mode := cpmObserve;
    LOtherToken := LStore.Subscribe(LSecond.Observe);
    LProbe.Mode := cpmThrow;
    LRejected := False;
    try
      LStore.Update(NyxCollectionItem(LA).WithValue(CountField, 6));
    except
      on LException: ENyxCollectionNotification do
      begin
        LRejected := True;
        {$IFDEF PAS2JS}
        LMessage := LException.Message;
        {$ELSE}
        LMessage := RawByteString(LException.Message);
        SetCodePage(RawByteString(LMessage), CP_UTF8, False);
        {$ENDIF}
        Check(Pos(TNyxText('Observer failed / 🌙'), LMessage) > 0,
          'committed observer failure preserves exact UTF-8 diagnostic', Result);
      end;
    end;
    Check(LRejected and (LStore.Snapshot.Item(LA).GetValue(CountField) = 6) and
      (LSecond.Calls = 1), 'observer failure reports committed values and notifies remaining receivers', Result);
    LProbe.Mode := cpmObserve;
    LStore.Update(NyxCollectionItem(LA).WithValue(CountField, 7));
    Check(LSecond.Calls = 2, 'notification failure clears callback-phase guard', Result);

    LOtherToken := nil;
    LProbe.Mode := cpmDisconnect;
    LProbe.OtherToken := LStore.Subscribe(LSecond.Observe);
    LCalls := LSecond.Calls;
    LStore.Update(NyxCollectionItem(LA).WithValue(CountField, 8));
    Check((LSecond.Calls = LCalls) and (LProbe.OtherToken = nil),
      'disconnecting/releasing a later listener safely skips its notification serial', Result);
    LToken.Disconnect;
    LCalls := LProbe.Calls;
    LStore.Update(NyxCollectionItem(LA).WithValue(CountField, 9));
    Check(not LToken.Connected and (LProbe.Calls = LCalls),
      'explicit managed-token disconnect stops callbacks', Result);

    LToken := nil;
    LProbe.Mode := cpmReleaseOwner;
    LStore := nil;
    PrepareOwnerRelease(LProbe, LToken);
    LProbe.Store.Apply([NyxUpdate(NyxCollectionItem(LA).WithValue(CountField, 10))]);
    Check((LProbe.Store = nil) and not LToken.Connected and
      (LProbe.Last.After.Item(LA).GetValue(CountField) = 10),
      'last owner release safely disposes store and detaches a retained token', Result);
    Inc(Result, LProbe.Checks);
    LProbe.Checks := 0;
    Check((LChanges.Before.Item(LA).GetValue(CountField) = 1) and
      (LChanges.After.Item(LA).GetValue(CountField) = 42),
      'retained original changes survive later edits and application owner release', Result);
    Check((LProbe.Last.Before.Item(LA).GetValue(CountField) = 0) and
      (LProbe.Last.After.Item(LA).GetValue(CountField) = 10),
      'owned before/after changes survive verified source store disposal', Result);

    LOther := NewNyxCollection(LKey, LSchema);
    LToken := LOther.Subscribe(LSecond.Observe);
    LOther := nil;
    Check(not LToken.Connected, 'managed subscription does not keep its source store alive', Result);
    LToken := nil;
  finally
    LToken := nil;
    LOtherToken := nil;
    LProbe.OtherToken := nil;
    LProbe.Free;
    LSecond.Free;
  end;

  for LCase := 0 to 7 do
  begin
    LRejected := False;
    try
      case LCase of
        0: LExpanded := LSchema.Text(CaptionField, 'Duplicate');
        1: LExpanded := NyxCollectionSchema.Integer(CountField, 5, NyxIntegerDomain.Range(0, 4));
        2: LOther := NewNyxCollection(LKey, LDefaultSchema);
        3: LOther := NewNyxCollection(LRef, LSchema);
        4: LRow := NyxCollectionItem(LDefaultItem);
        5: LRow := NyxCollectionItem(LA).WithValue(LDefaultField, 'Missing field ref');
        6: LRef := NyxCollection(' ');
        7: LRef := NyxCollection('Control' + #0);
      end;
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'uninitialized/duplicate/bad default/reference case ' + IntToStr(LCase), Result);
  end;
  LUnicodeName := '';
  for LCase := 1 to 128 do
  begin
    LUnicodeName := LUnicodeName + TNyxText('🌙');
  end;
  LRef := NyxCollection(LUnicodeName);
  Check(LRef.Name = LUnicodeName, 'reference bound counts Unicode scalars on both targets', Result);
  LRejected := False;
  try
    LRef := NyxCollection(LUnicodeName + TNyxText('🌙'));
  except
    on LException: ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, '129-scalar references reject without a byte/code-unit shortcut', Result);

  LExpanded := NyxCollectionSchema;
  for LCase := 0 to NyxMaximumCollectionFields - 1 do
  begin
    LExpanded := LExpanded.Integer(NyxIntegerField('f' + TNyxText(IntToStr(LCase))), LCase);
  end;
  Check(LExpanded.Count = NyxMaximumCollectionFields, 'schema admits its exact field budget', Result);
  LRejected := False;
  try
    LExpanded := LExpanded.Integer(NyxIntegerField('overflow'), 0);
  except
    on LException: ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LExpanded.Count = NyxMaximumCollectionFields),
    'excess field rejection preserves immutable schema baseline', Result);

  LStore := NewNyxCollection(LKey, LSchema);
  SetLength(LEdits, NyxMaximumCollectionEdits + 1);
  LRejected := False;
  try
    LStore.Apply(LEdits);
  except
    on LException: ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LStore.Snapshot.Revision = 0),
    'oversized edit request rejects before candidate publication', Result);
  LSecond := TCollectionProbe.Create;
  try
    SetLength(LTokens, NyxMaximumCollectionSubscriptions);
    for LCase := 0 to High(LTokens) do
    begin
      LTokens[LCase] := LStore.Subscribe(LSecond.Observe);
    end;
    LRejected := False;
    try
      LToken := LStore.Subscribe(LSecond.Observe);
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LToken = nil), 'subscription limit rejects without publishing another token', Result);
    LTokens[0] := nil;
    LToken := LStore.Subscribe(LSecond.Observe);
    Check(LToken.Connected, 'released tokens restore subscription capacity', Result);
  finally
    LToken := nil;
    for LCase := 0 to High(LTokens) do
    begin
      LTokens[LCase] := nil;
    end;
    LSecond.Free;
  end;

  { Exercise actual admitted limits with the public API. Empty-schema rows make
    the large identity/order case inexpensive without hiding field/domain checks
    in the mixed schema fixtures above. All stores remain renderer-independent. }
  LStore := NewNyxCollection(LKey, NyxCollectionSchema);
  SetLength(LEdits, NyxMaximumCollectionEdits);
  for LBatch := 0 to (NyxMaximumCollectionItems div NyxMaximumCollectionEdits) - 1 do
  begin
    for LCase := 0 to High(LEdits) do
    begin
      LEdits[LCase] := NyxInsert(LStore.Snapshot.Count + LCase,
        NyxCollectionItem(NyxItem(LKey,
          'row-' + TNyxText(IntToStr(LBatch * Length(LEdits) + LCase)))));
    end;
    LStore.Apply(LEdits);
  end;
  LBefore := LStore.Snapshot;
  Check((LBefore.Count = NyxMaximumCollectionItems) and
    (LBefore.IndexOf(NyxItem(LKey, 'row-16383')) = 16383),
    'large collection admits its full row bound with indexed stable identities', Result);
  LRejected := False;
  try
    LStore.Append(NyxCollectionItem(LB));
  except
    on LException: ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LStore.Snapshot.Revision = LBefore.Revision) and
    (LStore.Snapshot.Count = LBefore.Count),
    'row-limit rejection retains full accepted data and revision', Result);
  LStore.Move(NyxItem(LKey, 'row-0'), LBefore.Count - 1);
  Check((LStore.Snapshot.IndexOf(NyxItem(LKey, 'row-0')) = LBefore.Count - 1) and
    (LBefore.IndexOf(NyxItem(LKey, 'row-0')) = 0),
    'large reorder keeps current selection lookup and retained baseline independent', Result);
  LStore := nil;
  LBefore := nil;

  LPayload := TNyxText(StringOfChar('x', 256 * 1024));
  LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(CaptionField, ''));
  SetLength(LEdits, 33);
  for LCase := 0 to High(LEdits) do
  begin
    LEdits[LCase] := NyxInsert(LCase, NyxCollectionItem(NyxItem(LKey,
      'large-' + TNyxText(IntToStr(LCase)))).WithValue(CaptionField, LPayload));
  end;
  LRejected := False;
  try
    LStore.Apply(LEdits);
  except
    on LException: ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LStore.Snapshot.Count = 0) and (LStore.Snapshot.Revision = 0),
    'aggregate UTF-8 payload budget rejects a whole otherwise valid batch', Result);
  LPayload := TNyxText(StringOfChar('x', 128 * 1024));
  LExpanded := NyxCollectionSchema;
  for LCase := 0 to 62 do
  begin
    LExpanded := LExpanded.Text(NyxTextField('large-' + TNyxText(IntToStr(LCase))), LPayload);
  end;
  LRejected := False;
  try
    LExpanded := LExpanded.Text(NyxTextField('overflow'), LPayload);
  except
    on LException: ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LExpanded.Count = 63),
    'collective schema/default/domain budget preserves the preceding definition', Result);
end;

end.
