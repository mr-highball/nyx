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
program nyx_collection_query_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.state, nyx.data, nyx.collections,
  nyx.collections.selection, nyx.collections.query, nyx.collections.query.view,
  nyx.collections.view.types, nyx.collections.view, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.source,
  {$ifdef NYX_COMPILED_QUERY}nyx.query.fixture,{$endif}
  {$ifdef PAS2JS}Web;{$else}Classes;{$endif}

type
  TQueryObserver = class
    Calls: Integer;
    procedure Changed(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Collection query: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TQueryObserver.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Calls);
  Check(AView.Snapshot.Revision = AView.Store.Snapshot.Revision,
    'query results keep the actual source revision');
end;

function Key: TNyxCollectionRef;
begin
  Result := NyxCollection('work-items');
end;

function Item(const AID: TNyxText): TNyxItemRef;
begin
  Result := NyxItem(Key, AID);
end;

function Schema: TNyxCollectionSchema;
begin
  Result := NyxCollectionSchema.Text(NyxTextField('task'), '')
    .Integer(NyxIntegerField('priority'), 0).Boolean(NyxBooleanField('complete'), False)
    .Number(NyxNumberField('estimate'), 0).Text(NyxTextField('parent'), '');
end;

function WorkItem(const AID, ATask: TNyxText; APriority: Integer;
  AComplete: Boolean; AEstimate: Double): TNyxCollectionItem;
begin
  Result := NyxCollectionItem(Item(AID)).WithValue(NyxTextField('task'), ATask)
    .WithValue(NyxIntegerField('priority'), APriority)
    .WithValue(NyxBooleanField('complete'), AComplete)
    .WithValue(NyxNumberField('estimate'), AEstimate);
end;

function Store: INyxCollection;
begin
  Result := NewNyxCollection(Key, Schema, [
    WorkItem('plan', 'Plan the next idea', 1, False, 1.5),
    WorkItem('design', 'Sketch the experience', 3, False, 2),
    WorkItem('build', 'Build something useful', 3, False, 2),
    WorkItem('share', 'Share the result', 2, True, 0.5)]);
end;

function Spec: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(Key).Column(NyxTextField('task'), 'Task', cmEditable)
    .Column(NyxIntegerField('priority'), 'Priority', cmEditable).Selection(nsmMultiple);
end;

function Policy: TNyxCollectionQuery;
begin
  Result := NyxCollectionQuery.Where(NyxWhere(NyxBooleanField('complete')).EqualTo(False)
    .AndAlso(NyxWhere(NyxIntegerField('priority')).AtLeast(2))
    .AndAlso(NyxWhere(NyxNumberField('estimate')).AtMost(10).Negated.Negated)
    .AndAlso(NyxWhere(NyxIntegerField('priority')).GreaterThan(Low(Integer)))
    .AndAlso(NyxWhere(NyxTextField('task')).Contains('🌙').Negated))
    .OrderBy(NyxIntegerField('priority'), nsdDescending)
    .ThenBy(NyxTextField('task'), nsdAscending, nqtAsciiInsensitive);
end;

procedure Predicates;
var
  LRow: TNyxCollectionItem;
  LQuery: TNyxCollectionQuery;
  LCopy: TNyxCollectionQuery;
  LPredicate: INyxCollectionPredicate;
  LSort: TNyxCollectionSort;
  LRefused: Boolean;
  LIndex: Integer;
begin
  LRow := WorkItem('unicode', 'A🌙abaBABaé', Low(Integer), False, 0.125);
  Check(NyxWhere(NyxTextField('task')).Contains('🌙ab').Matches(LRow), 'supplementary text search');
  Check(NyxWhere(NyxTextField('task')).Contains('ABABABA', nqtAsciiInsensitive).Matches(LRow),
    'overlapping scalar search uses explicit ASCII folding');
  Check(NyxWhere(NyxTextField('task')).StartsWith('a🌙', nqtAsciiInsensitive).Matches(LRow),
    'prefix comparison keeps supplementary scalars exact');
  Check(NyxWhere(NyxTextField('task')).EndsWith('Baé').Matches(LRow), 'exact suffix search');
  Check(not NyxWhere(NyxTextField('task')).EndsWith('É', nqtAsciiInsensitive).Matches(LRow),
    'ASCII folding never claims locale or accent case folding');
  Check(NyxWhere(NyxTextField('task')).Contains('').Matches(LRow), 'empty needle matches');
  Check(NyxWhere(NyxIntegerField('priority')).LessThan(High(Integer)).Matches(LRow),
    'integer endpoints compare without subtraction overflow');
  Check(NyxWhere(NyxNumberField('estimate')).EqualTo(0.125).Matches(LRow), 'finite exact number equality');
  Check(NyxWhere(NyxBooleanField('complete')).NotEqualTo(True).Matches(LRow), 'typed Boolean comparison');
  Check(NyxWhere(NyxIntegerField('priority')).GreaterThan(0)
    .OrElse(NyxWhere(NyxTextField('task')).Contains('🌙')).Matches(LRow), 'disjunction');
  LSort := NyxSort(NyxTextField('task'));
  Check(LSort.Compare(TNyxStateValue.FromText(NyxScalarText($E000)),
    TNyxStateValue.FromText(NyxScalarText($10000))) < 0,
    'Unicode scalar ordering agrees across UTF-8 and UTF-16 storage');
  LSort := NyxSort(NyxIntegerField('priority'), nsdDescending);
  Check(LSort.Compare(TNyxStateValue.FromInteger(Low(Integer)),
    TNyxStateValue.FromInteger(High(Integer))) = 1, 'descending integer endpoints remain bounded');
  LQuery := Policy;
  LCopy := LQuery.ThenBy(NyxNumberField('estimate'));
  Check((LQuery.SortCount = 2) and (LCopy.SortCount = 3), 'fluent sort arrays are independent');
  Check(TNyxCollectionQuery.FromData(LQuery.ToData).ToData.ToJSON = LQuery.ToData.ToJSON,
    'typed predicate/order wire round trip');
  Check(not LQuery.WithoutFilter.Unsorted.Defined, 'clear methods restore empty policy');
  LRefused := False;
  try
    LCopy := NyxCollectionQuery.ThenBy(NyxTextField('task'));
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'secondary ordering requires a primary');
  LRefused := False;
  try
    LCopy := LQuery.ThenBy(NyxIntegerField('priority'));
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'duplicate ordering fields refuse');
  LRefused := False;
  try
    LCopy := TNyxCollectionQuery.FromData(TNyxDataValue.ParseJSON(
      '{"version":1,"filter":null,"order":[],"unexpected":true}'));
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'unknown wire members refuse');
  LRefused := False;
  try
    LPredicate := NyxPredicateFromData(TNyxDataValue.ParseJSON(
      '{"op":"field","field":"priority","kind":"integer","comparison":"contains",' +
      '"textComparison":"exact","value":1}'));
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'inappropriate field operation refuses at the wire boundary');
  LPredicate := NyxWhere(NyxBooleanField('complete')).EqualTo(False);
  LRefused := False;
  try
    for LIndex := 1 to NyxMaximumQueryDepth do
    begin
      LPredicate := LPredicate.Negated;
    end;
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LPredicate.Depth = NyxMaximumQueryDepth),
    'depth overflow leaves the prior immutable predicate intact');
end;

procedure Views;
var
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LOther: INyxCollectionView;
  LBefore: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
  LObserver: TQueryObserver;
  LToken: INyxCollectionViewSubscription;
  LRefused: Boolean;
  LCalls: Integer;
begin
  LStore := Store;
  LView := NewNyxCollectionView(LStore, Spec, cpTable);
  LOther := NewNyxCollectionView(LStore, Spec, cpTable);
  LView.SetSelection([Item('plan'), Item('build')], Item('plan'), Item('plan'));
  LObserver := TQueryObserver.Create;
  LToken := nil;
  try
    LToken := LView.Subscribe(LObserver.Changed);
    LBefore := LStore.Snapshot;
    LView.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxIntegerField('priority'), nsdDescending));
    Check((LView.Snapshot.ItemAt(0).Ref.ID = 'design') and
      (LView.Snapshot.ItemAt(1).Ref.ID = 'build'), 'descending equal keys preserve source order');
    Check(LStore.Snapshot = LBefore, 'query never mutates source-store snapshot/order');
    Check(LOther.Snapshot = LBefore, 'independent view retains its own policy');
    LView.ConfigureQuery(Policy);
    Check((LView.Snapshot.Count = 2) and (LView.Snapshot.ItemAt(0).Ref.ID = 'build'),
      'typed predicate with secondary ordering');
    Check(LView.Selection.Contains(Item('plan')) and LView.Selection.Contains(Item('build')),
      'filtering keeps hidden membership');
    Check(LView.CellText(Item('plan'), 0) = 'Plan the next idea',
      'hidden selected source identities remain readable');
    Check(LView.Selection.Focus.Defined and LView.Snapshot.Has(LView.Selection.Focus),
      'hidden focus moves to a visible row');
    Check(LView.Selection.Anchor.ID = 'plan', 'hidden anchor retains source identity');
    LView.Select(Item('design'), nsaToggle);
    Check(LView.Selection.Count = 3, 'toggle preserves hidden selected rows');
    LView.Select(Item('build'), nsaFocus);
    Check(LView.Selection.Count = 3, 'focus preserves all membership');
    LBefore := LView.Snapshot;
    LSelection := LView.Selection;
    LCalls := LObserver.Calls;
    LRefused := False;
    try
      LView.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxTextField('priority')));
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LView.Snapshot = LBefore) and
      (LView.Selection = LSelection) and (LObserver.Calls = LCalls),
      'invalid query admission is atomic');
    LView.ConfigureQuery(Policy);
    Check(LObserver.Calls = LCalls, 'equal query policy is a no-op');
    LView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(99)));
    Check((LView.Snapshot.Count = 0) and (LView.Selection.Count = 3) and
      not LView.Selection.Focus.Defined, 'empty result keeps membership without a phantom cursor');
    LStore.Remove(Item('plan'));
    Check((LView.Selection.Count = 2) and not LView.Selection.Contains(Item('plan')),
      'actual source deletion prunes a hidden selection');
    LView.ConfigureQuery(NyxCollectionQuery);
    Check((LView.Snapshot = LStore.Snapshot) and (LView.Selection.Count = 2),
      'clear policy restores complete source identity');
    LView.ConfigureQuery(Policy);
    LView.SelectRange(Item('design'), [Item('build'), Item('design')], True);
    Check(LView.Selection.Count = 2, 'result-order range preserves admitted membership');
    LView.SelectAll;
    Check(LView.Selection.Count = 2, 'SelectAll includes query result, not excluded source rows');
    Check(not LView.Spec.QueryPolicy.Defined, 'runtime policy never changes authored defaults');
  finally

    if LToken <> nil then
    begin
      LToken.Disconnect;
    end;
    LToken := nil;
    LObserver.Free;
  end;
end;

procedure Trees;
var
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LTreeSpec: TNyxCollectionViewSpec;
begin
  LStore := NewNyxCollection(Key, Schema, [
    WorkItem('leaf', 'Matching child', 5, False, 1).WithValue(NyxTextField('parent'), 'root'),
    WorkItem('other', 'Other root', 0, False, 1),
    WorkItem('root', 'Required parent', 0, False, 1)]);
  LTreeSpec := Spec.Parent(NyxTextField('parent'));
  LView := NewNyxCollectionView(LStore, LTreeSpec, cpTree);
  LView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(5))
    .OrderBy(NyxTextField('task')));
  Check((LView.Snapshot.Count = 2) and LView.Snapshot.Has(Item('root')) and
    not LView.Snapshot.Has(Item('other')), 'tree filtering retains only required ancestry');
  Check(LView.ParentIndex(LView.Snapshot.IndexOf(Item('leaf'))) =
    LView.Snapshot.IndexOf(Item('root')), 'result parent indexes follow stable identities');
end;

procedure SourceAndPersistence;
var
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LRejected: TNyxDocument;
  LRejectedWorkspace: TNyxSourceWorkspace;
  LRefused: Boolean;
  LPage: INyxPage;
  LTable: INyxTable;
  LStore: INyxCollection;
  LSpec: TNyxCollectionViewSpec;
  LSource: TNyxText;
  {$ifndef PAS2JS}LStream: TFileStream;{$endif}
begin
  LDocument := TNyxDocument.Create;
  LDecoded := nil;
  LParsed := nil;
  LWorkspace := nil;
  try
    LStore := Store;
    LDocument.Collections.Define(Key, Schema, [LStore.Snapshot.ItemAt(0),
      LStore.Snapshot.ItemAt(1), LStore.Snapshot.ItemAt(2), LStore.Snapshot.ItemAt(3)]);
    LPage := NewNyxPage('query-review');
    LDocument.AddPage(LPage);
    LTable := NewNyxTable('work-table');
    LPage.Add(LTable);
    LSpec := Spec.Query(Policy);
    LTable.Binds.Collection(LSpec).Done;
    Check(Spec.ToData.Field('version').AsInteger = 2, 'old query-free descriptor stays version 2');
    Check(LSpec.ToData.Field('version').AsInteger = 3, 'query uses explicit version-3 descriptor');
    Check(TNyxCollectionViewSpec.FromData(LSpec.ToData).ToData.ToJSON = LSpec.ToData.ToJSON,
      'authored query/default descriptor round trip');
    LDecoded := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
    Check(TNyxCodec.Encode(LDecoded) = TNyxCodec.Encode(LDocument), 'complete design persistence');
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.query.fixture');
    Check((Pos('.Where(', LSource) > 0) and (Pos('.ThenBy(', LSource) > 0) and
      (Pos('nqtAsciiInsensitive', LSource) > 0) and (Pos('SetProp(', LSource) = 0),
      'generated query uses readable strongly typed fluent expressions');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDocument),
      'generated Pascal admission reproduces the entire query design');
    LRejected := nil;
    LRejectedWorkspace := nil;
    LRefused := False;
    try
      try
        LRejected := TNyxSourceWorkspace.PrepareDraft(
          StringReplace(LSource, '.AtLeast(2)', '.AtLeast(''2'')', []), LRejectedWorkspace);
      except
        on E: Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LRejected = nil) and (LRejectedWorkspace = nil),
        'raw text numeric predicate refuses the whole source candidate');
    finally
      LRejectedWorkspace.Free;
      LRejected.Free;
    end;
    {$ifdef NYX_COMPILED_QUERY}
    LParsed.Free;
    LParsed := nyx.query.fixture.BuildNyxDocument;
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDocument),
      'ordinary compiled generated builder reproduces the entire design');
    {$endif}
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      LStream := TFileStream.Create(ParamStr(1), fmCreate);
      try

        if Length(LSource) > 0 then
        begin
          LStream.WriteBuffer(LSource[1], Length(LSource));
        end;
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LWorkspace.Free;
    LParsed.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Predicates;
    Views;
    Trees;
    SourceAndPersistence;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' typed collection query checks';
    document.body.setAttribute('data-collection-query', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' typed collection query checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      document.body.setAttribute('data-event-error', E.Message);
      document.body.setAttribute('data-collection-query', 'failed');
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
