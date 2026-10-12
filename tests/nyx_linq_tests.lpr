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
program nyx_linq_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

uses
  SysUtils,
  nyx.text,
  nyx.collections,
  nyx.linq,
  nyx.linq.collections,
  nyx.linq.reductions,
  nyx.test.linq.order,
  nyx.test.linq.projection,
  nyx.test.linq.joins,
  nyx.test.linq.sets,
  nyx.test.linq.partitions,
  nyx.test.linq.aggregates,
  nyx.test.linq.materialization,
  nyx.test.linq.callbacks,
  nyx.test.linq.combinations,
  nyx.test.linq.authoring,
  nyx.test.linq.precision,
  nyx.test.linq.pipeline,
  {$ifdef NYX_COMPILED_LINQ}nyx.model, nyx.generated.view,{$endif}
  {$ifdef PAS2JS}Web;{$else}Classes;{$endif}

type
  EQueryCallback = class(Exception);
  TSampleValue = record
    Name: TNyxText;
    Number: Double;
  end;
  TSampleChoice = (scFirst, scSecond);
  {$ifndef PAS2JS}
  { The installed pas2js compiler refuses explicitly numbered enum declarations.
    Keep their native generic coverage separate from portable enum qualification. }
  TNumberedChoice = (ncFirst = 5, ncSecond = 6);
  {$endif}
  { A test-owned counter outlives the retained callback and requires no native
    pointer bridge, allowing identical lifetime assertions in pas2js. }
  TLifetimeCounter = class
    Destroyed: Integer;
  end;
  TCountingPredicate = class(TInterfacedObject, INyxPredicate<Integer>)
  public
    Calls: Integer;
    Lifetime: TLifetimeCounter;
    function Test(const AValue: Integer): Boolean;
    destructor Destroy; override;
  end;

const
  CMoon: TNyxText = '🌙';
  CCallbackFailure: TNyxText = 'owned callback 🌙';
  CSecondName: TNyxText = 'Item 2 / 🌙';
  CExactText: TNyxText = 'Unicode 🌙' + #0 + ' retained';
  CBuildCaption: TNyxText = 'Build 🌙';

var
  GChecks: Integer;
  GCalls: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('LINQ: ' + AReason);
  end;
  Inc(GChecks);
end;

function Even(const AValue: Integer): Boolean;
begin
  Inc(GCalls);
  Result := (AValue mod 2) = 0;
end;

function BelowThree(const AValue: Integer): Boolean;
begin
  Inc(GCalls);
  Result := AValue < 3;
end;

function Positive(const AValue: Integer): Boolean;
begin
  Result := AValue > 0;
end;

function DoubleValue(const AValue: Integer): Integer;
begin
  Inc(GCalls);
  Result := AValue * 2;
end;

function Describe(const AValue: Integer): TNyxText;
begin
  Inc(GCalls);
  Result := 'Item ' + TNyxText(IntToStr(AValue)) + ' / ' + CMoon;
end;

function FailAtTwo(const AValue: Integer): Boolean;
begin
  Inc(GCalls);

  if AValue = 2 then
  begin
    raise EQueryCallback.Create(CCallbackFailure);
  end;
  Result := True;
end;

function FailProjection(const AValue: Integer): TNyxText;
begin

  if AValue = 2 then
  begin
    raise EQueryCallback.Create(CCallbackFailure);
  end;
  Result := Describe(AValue);
end;

function TCountingPredicate.Test(const AValue: Integer): Boolean;
begin
  Inc(Calls);
  Result := (AValue mod 2) = 0;
end;

destructor TCountingPredicate.Destroy;
begin
  Inc(Lifetime.Destroyed);
  inherited Destroy;
end;

procedure LazyAndIndependent;
var
  LInput: TNyxIntegers;
  LValues: TNyxSequenceArray<Integer>;
  LSource: INyxSequence<Integer>;
  LQuery: INyxSequence<Integer>;
  LCursor: INyxEnumerator<Integer>;
  LOther: INyxEnumerator<Integer>;
  LRejected: Boolean;
  LValue: Integer;
begin
  SetLength(LInput, 6);
  for LValue := 0 to 5 do
  begin
    LInput[LValue] := LValue + 1;
  end;
  LSource := LInput.AsNyx;
  LInput[0] := 99;
  LInput := nil;
  Check(LSource.First = 1, 'array helper captures a detached outer vector');
  GCalls := 0;
  LQuery := LSource.Where(TNyxPredicate<Integer>.From(Even)).Skip(1).Take(1);
  Check(GCalls = 0, 'query construction does not evaluate callbacks');
  Check(LQuery.Single = 4, 'filter then skip/take preserves pipeline order');
  Check(GCalls = 4, 'bounded query stops before the unused tail');
  LCursor := LQuery.GetEnumerator;
  LOther := LQuery.GetEnumerator;
  LRejected := False;
  try
    LValue := LCursor.Current;
  except
    on E: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Current refuses before first successful movement');
  LSource := nil;
  LQuery := nil;
  Check(LCursor.MoveNext and (LCursor.Current = 4),
    'cursor owns its upstream after all sequence references retire');
  Check(LOther.MoveNext and (LOther.Current = 4), 'independent cursors do not steal rows');
  Check(not LCursor.MoveNext, 'bounded cursor reaches EOF');
  Check(not LCursor.MoveNext, 'EOF remains terminal');
  LRejected := False;
  try
    LValue := LCursor.Current;
  except
    on E: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Current refuses after EOF');
  LCursor := nil;
  LOther := nil;
  LSource := TNyxSequence<Integer>.From([1, 2, 3, 4, 5]);
  GCalls := 0;
  Check(not LSource.Where(TNyxPredicate<Integer>.From(Even)).Take(0).Any,
    'Take zero is empty');
  Check(GCalls = 0, 'Take zero never invokes source callbacks');
  GCalls := 0;
  LValues := LSource.SkipWhile(TNyxPredicate<Integer>.From(BelowThree)).ToArray;
  Check((Length(LValues) = 3) and (LValues[0] = 3) and (LValues[2] = 5),
    'SkipWhile stops skipping after the first nonmatch');
  Check(GCalls = 3, 'SkipWhile does not retest retained tail rows');
  GCalls := 0;
  LValues := LSource.TakeWhile(TNyxPredicate<Integer>.From(BelowThree)).ToArray;
  Check((Length(LValues) = 2) and (LValues[0] = 1) and (LValues[1] = 2),
    'TakeWhile preserves the initial matching prefix');
  Check(GCalls = 3, 'TakeWhile stops at its first false result');
  LQuery := LSource.Concat(TNyxSequence<Integer>.From([6, 7]));
  Check((LQuery.Count = 7) and (LQuery.ElementAt(5) = 6) and (LQuery.Last = 7),
    'concatenation retains complete input order');
  LValues := LQuery.ToArray;
  LValues[0] := 88;
  Check(LQuery.First = 1, 'materialized writes do not change their sequence');
  Check(LSource.Where(TNyxPredicate<Integer>.From(Positive))
    .Where(TNyxPredicate<Integer>.From(Even)).Count = 2,
    'successive Where operators compose');
  GCalls := 0;
  Check(LSource.Any(TNyxPredicate<Integer>.From(Even)) and (GCalls = 2),
    'Any predicate short circuits');
  GCalls := 0;
  Check(not LSource.All(TNyxPredicate<Integer>.From(Even)) and (GCalls = 1),
    'All predicate short circuits');
  Check(LSource.Skip(High(Integer)).Count = 0, 'large skip cannot wrap');
  Check(LSource.Take(High(Integer)).Count = 5, 'large take stops at the source end');
end;

procedure ProjectionAndValues;
var
  LSource: INyxSequence<Integer>;
  LNames: INyxSequence<TNyxText>;
  LTexts: TNyxTexts;
  LNamesArray: TNyxSequenceArray<TNyxText>;
  LIndex: Integer;
  LValues: TNyxIntegers;
  LSamples: TNyxSequenceArray<TSampleValue>;
  LSampleSequence: INyxSequence<TSampleValue>;
  LSample: TSampleValue;
begin
  LSource := TNyxSequence<Integer>.From([1, 2, 3]);
  GCalls := 0;
  LNames := TNyxSelect<Integer, TNyxText>.From(LSource,
    TNyxSelector<Integer, TNyxText>.From(Describe));
  Check(GCalls = 0, 'type-changing Select remains deferred');
  LNamesArray := LNames.Take(2).ToArray;
  Check((Length(LNamesArray) = 2) and (LNamesArray[1] = CSecondName),
    'type-changing projection preserves supplementary Unicode');
  Check(GCalls = 2, 'projection only processes consumed rows');
  Check(LSource.Select(TNyxSelector<Integer, Integer>.From(DoubleValue)).Last = 6,
    'same-type Select supports ordinary fluent chaining');
  SetLength(LTexts, 3);
  LTexts[0] := 'English demo';
  LTexts[1] := CExactText;
  LTexts[2] := '';
  LNames := LTexts.AsNyx;
  LTexts[1] := 'replaced';
  Check(LNames.ElementAt(1) = CExactText,
    'text helper retains exact supplementary and NUL values');
  Check(LNames.Last = '', 'empty text is an ordinary element');
  SetLength(LValues, 4096);
  for LIndex := 0 to High(LValues) do
  begin
    LValues[LIndex] := LIndex;
  end;
  LSource := LValues.AsNyx;
  Check(LSource.Count = 4096, 'generic source does not reduce the large dataset');
  Check(LSource.Where(TNyxPredicate<Integer>.From(Even)).Count = 2048,
    'large deferred filter retains all matching values');
  Check(LSource.Skip(4095).Single = 4095, 'last large row remains reachable');
  SetLength(LSamples, 1);
  LSamples[0].Name := CExactText;
  LSamples[0].Number := 0.125;
  LSampleSequence := TNyxSequence<TSampleValue>.From(LSamples);
  LSamples[0].Name := 'Caller replacement';
  LSample := LSampleSequence.Single;
  Check((LSample.Name = CExactText) and (LSample.Number = 0.125),
    'generic record capture retains exact managed text/numeric fields');
  LSample.Name := 'Reader replacement';
  Check(LSampleSequence.Single.Name = CExactText,
    'read record replacement never changes the captured row');
  LSampleSequence := TNyxSequence<TSampleValue>.Empty;
  Check(not LSampleSequence.TryFirst(LSample)
    and (LSample.Name = '') and (LSample.Number = 0),
    'empty generic record outputs initialize both managed and numeric fields');
end;

procedure FailuresAndOwnership;
var
  LSource: INyxSequence<Integer>;
  LEmpty: INyxSequence<Integer>;
  LQuery: INyxSequence<Integer>;
  LCursor: INyxEnumerator<Integer>;
  LPredicate: INyxPredicate<Integer>;
  LOwned: TCountingPredicate;
  LLifetime: TLifetimeCounter;
  LValue: Integer;
  LRejected: Boolean;
  LCase: Integer;
  LText: TNyxText;
  LNumber: Double;
  LNames: INyxSequence<TNyxText>;
  LNameCursor: INyxEnumerator<TNyxText>;
  LSelector: INyxSelector<Integer, TNyxText>;
  LTextValue: TNyxText;
  LEmptyText: INyxSequence<TNyxText>;
  LEmptyNumbers: INyxSequence<Double>;
  LEmptyBoolean: INyxSequence<Boolean>;
  LEmptyChoice: INyxSequence<TSampleChoice>;
  LBoolean: Boolean;
  LChoice: TSampleChoice;
  {$ifndef PAS2JS}
  LEmptyNumbered: INyxSequence<TNumberedChoice>;
  LNumbered: TNumberedChoice;
  {$endif}
begin
  LSource := TNyxSequence<Integer>.From([1, 2, 3]);
  LEmpty := TNyxSequence<Integer>.Empty;
  Check((LEmpty.Count = 0) and not LEmpty.Any, 'empty count/Any agree');
  Check(LEmpty.All(TNyxPredicate<Integer>.From(Positive)), 'All is true on empty input');
  LValue := 99;
  Check(not LEmpty.TryFirst(LValue) and (LValue = 0), 'TryFirst clears absent output');
  LValue := 99;
  Check(not LEmpty.TryLast(LValue) and (LValue = 0), 'TryLast clears absent output');
  LValue := 99;
  Check(not LEmpty.TrySingle(LValue) and (LValue = 0), 'TrySingle clears absent output');
  LText := 'Prior text';
  LEmptyText := TNyxSequence<TNyxText>.Empty;
  Check(not LEmptyText.TryFirst(LText) and (LText = ''),
    'absent text output clears its managed value');
  LNumber := 0.125;
  LEmptyNumbers := TNyxSequence<Double>.Empty;
  Check(not LEmptyNumbers.TryFirst(LNumber) and (LNumber = 0),
    'absent floating output uses the same zero value on both targets');
  LEmptyBoolean := TNyxSequence<Boolean>.Empty;
  LBoolean := True;
  Check(not LEmptyBoolean.TryFirst(LBoolean) and not LBoolean,
    'absent Boolean output uses False');
  LEmptyChoice := TNyxSequence<TSampleChoice>.Empty;
  LChoice := scSecond;
  Check(not LEmptyChoice.TryFirst(LChoice) and (LChoice = scFirst),
    'absent portable enum uses its ordinary zero value');
  {$ifndef PAS2JS}
  LEmptyNumbered := TNyxSequence<TNumberedChoice>.Empty;
  LNumbered := ncSecond;
  Check(not LEmptyNumbered.TryFirst(LNumbered) and (Ord(LNumbered) = 0),
    'native explicitly numbered enum uses ordinary zero-initialized storage');
  {$endif}
  Check(LSource.Take(1).TrySingle(LValue) and (LValue = 1), 'TrySingle returns one value');
  for LCase := 0 to 19 do
  begin
    LRejected := False;
    try
      case LCase of
        0: LQuery := LSource.Skip(-1);
        1: LQuery := LSource.Take(-1);
        2: LQuery := LSource.Where(INyxPredicate<Integer>(nil));
        3: LQuery := LSource.SkipWhile(nil);
        4: LQuery := LSource.TakeWhile(nil);
        5: LQuery := LSource.Concat(nil);
        6: LQuery := LSource.Select(INyxSelector<Integer, Integer>(nil));
        7: LValue := LEmpty.First;
        8: LValue := LEmpty.Last;
        9: LValue := LEmpty.Single;
        10: LValue := LSource.Single;
        11: LValue := LSource.ElementAt(-1);
        12: LValue := LSource.ElementAt(3);
        13: LPredicate := TNyxPredicate<Integer>.From(nil);
        14: LSelector := TNyxSelector<Integer, TNyxText>.From(nil);
        15: LNames := TNyxSelect<Integer, TNyxText>.From(nil,
          TNyxSelector<Integer, TNyxText>.From(Describe));
        16: LNames := TNyxSelect<Integer, TNyxText>.From(LSource, nil);
        17: LSource.All(nil);
        18: LSource.Any(nil);
        19: LSource.TrySingle(LValue);
      end;
    except
      on E: ENyxQuery do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'invalid argument/cardinality refuses case ' + TNyxText(IntToStr(LCase)));
  end;
  LCursor := LSource.Where(TNyxPredicate<Integer>.From(FailAtTwo)).GetEnumerator;
  Check(LCursor.MoveNext and (LCursor.Current = 1), 'faulting cursor yields prior accepted row');
  LRejected := False;
  try
    LCursor.MoveNext;
  except
    on E: EQueryCallback do
    begin
      LRejected := TNyxText(E.Message) = CCallbackFailure;
    end;
  end;
  Check(LRejected, 'callback exception type and Unicode message remain exact');
  GCalls := 0;
  Check(not LCursor.MoveNext and (GCalls = 0), 'failed cursor is terminal without callback reentry');
  LRejected := False;
  try
    LValue := LCursor.Current;
  except
    on E: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'failure invalidates Current');
  LCursor := nil;
  LNameCursor := TNyxSelect<Integer, TNyxText>.From(LSource,
    TNyxSelector<Integer, TNyxText>.From(FailProjection)).GetEnumerator;
  Check(LNameCursor.MoveNext and (LNameCursor.Current = 'Item 1 / ' + CMoon),
    'projector cursor yields its first accepted value');
  LRejected := False;
  try
    LNameCursor.MoveNext;
  except
    on E: EQueryCallback do
    begin
      LRejected := TNyxText(E.Message) = CCallbackFailure;
    end;
  end;
  Check(LRejected, 'type-changing projection retains caller exception identity');
  Check(not LNameCursor.MoveNext, 'faulting projection cursor is terminal');
  LRejected := False;
  try
    LTextValue := LNameCursor.Current;
  except
    on E: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'projector failure invalidates its current result');
  LNameCursor := nil;
  LLifetime := TLifetimeCounter.Create;
  try
    LOwned := TCountingPredicate.Create;
    LOwned.Lifetime := LLifetime;
    LPredicate := LOwned;
    LQuery := LSource.Where(LPredicate);
    LPredicate := nil;
    LSource := nil;
    Check(LLifetime.Destroyed = 0, 'query retains its owned callback');
    LCursor := LQuery.GetEnumerator;
    LQuery := nil;
    Check(LLifetime.Destroyed = 0, 'independent cursor retains its callback');
    Check(LCursor.MoveNext and (LCursor.Current = 2), 'retained callback executes after caller release');
    LCursor := nil;
    Check(LLifetime.Destroyed = 1, 'last cursor retires callback exactly once');
  finally
    LCursor := nil;
    LQuery := nil;
    LPredicate := nil;
    LLifetime.Free;
  end;
end;

function Caption(const AItem: TNyxCollectionItem): TNyxText;
begin
  Result := AItem.GetValue(NyxTextField('caption'));
end;

{$ifdef NYX_COMPILED_LINQ}
procedure SemanticCompanion;
var
  LDocument: TNyxDocument;
  LSnapshot: INyxCollectionSnapshot;
  LRows: INyxSequence<TNyxCollectionItem>;
  LCaptions: INyxSequence<TNyxText>;
begin
  LDocument := BuildNyxDocument;
  try
    LSnapshot := LDocument.Collections.Snapshot(NyxCollection('measured'));
    LRows := LSnapshot.AsNyx;
    Check((LRows.Count = 3) and (LSnapshot.Revision = 0),
      'exact semantic builder exposes complete authored defaults without mutation');
    LCaptions := TNyxSelect<TNyxCollectionItem, TNyxText>.From(LRows,
      TNyxSelector<TNyxCollectionItem, TNyxText>.From(Caption));
    Check((LCaptions.First = 'Explore the collection') and
      (LCaptions.Skip(1).First = 'Plan the next chapter') and
      (LCaptions.Last = 'Build something useful'),
      'generic queries consume exact semantic authored captions in source order');
  finally
    LDocument.Free;
  end;
  Check(LCaptions.Last = 'Build something useful',
    'generic companion query outlives its owned document');
end;
{$endif}

function Urgent(const AItem: TNyxCollectionItem): Boolean;
begin
  Result := AItem.GetValue(NyxIntegerField('priority')) >= 2;
end;

procedure ManagedElements;
var
  LLifetime: TLifetimeCounter;
  LOwned: TCountingPredicate;
  LValue: IInterface;
  LRead: IInterface;
  LInput: TNyxSequenceArray<IInterface>;
  LSource: INyxSequence<IInterface>;
  LCursor: INyxEnumerator<IInterface>;
begin
  LLifetime := TLifetimeCounter.Create;
  try
    LOwned := TCountingPredicate.Create;
    LOwned.Lifetime := LLifetime;
    LValue := LOwned;
    SetLength(LInput, 1);
    LInput[0] := LValue;
    LSource := TNyxSequence<IInterface>.From(LInput);
    LValue := nil;
    LInput := nil;
    Check(LLifetime.Destroyed = 0, 'generic interface rows are retained by their captured source');
    LCursor := LSource.GetEnumerator;
    LSource := nil;
    Check(LLifetime.Destroyed = 0, 'cursor array retains managed elements after source retirement');
    Check(LCursor.MoveNext, 'managed element cursor reaches its row');
    { Assign the managed result deliberately. Comparing an interface-valued
      function inline can retain an FPC expression temporary until scope exit. }
    LRead := LCursor.Current;
    Check(LRead <> nil, 'managed element cursor exposes its row');
    LCursor := nil;
    Check(LLifetime.Destroyed = 0, 'read interface survives cursor disposal');
    LRead := nil;
    Check(LLifetime.Destroyed = 1, 'last generic interface reader releases its element exactly once');
    LSource := TNyxSequence<IInterface>.Empty;
    Check(not LSource.TryFirst(LValue) and (LValue = nil),
      'absent interface output is nil on both targets');
  finally
    LInput := nil;
    LValue := nil;
    LRead := nil;
    LCursor := nil;
    LSource := nil;
    LLifetime.Free;
  end;
end;

procedure SnapshotIntegration;
var
  LKey: TNyxCollectionRef;
  LStore: INyxCollection;
  LSnapshot: INyxCollectionSnapshot;
  LRows: INyxSequence<TNyxCollectionItem>;
  LCaptions: INyxSequence<TNyxText>;
  LItem: TNyxCollectionItem;
  LRejected: Boolean;
begin
  LKey := NyxCollection('query-demo');
  LStore := NewNyxCollection(LKey,
    NyxCollectionSchema.Text(NyxTextField('caption'), '')
      .Integer(NyxIntegerField('priority'), 0), [
    NyxCollectionItem(NyxItem(LKey, 'plan')).WithValue(NyxTextField('caption'), 'Plan')
      .WithValue(NyxIntegerField('priority'), 1),
    NyxCollectionItem(NyxItem(LKey, 'build')).WithValue(NyxTextField('caption'), CBuildCaption)
      .WithValue(NyxIntegerField('priority'), 2)]);
  LSnapshot := LStore.Snapshot;
  LRows := LSnapshot.AsNyx.Where(TNyxPredicate<TNyxCollectionItem>.From(Urgent));
  LStore.Apply([NyxUpdate(NyxCollectionItem(NyxItem(LKey, 'build'))
    .WithValue(NyxTextField('caption'), 'Changed'))]);
  Check((LSnapshot.Revision = 0) and (LStore.Snapshot.Revision = 1),
    'query capture does not change source/store revisions');
  LItem := LRows.Single;
  Check((LItem.Ref.ID = 'build') and (LItem.GetValue(NyxTextField('caption')) = CBuildCaption),
    'snapshot query retains exact identity and original values across edits');
  LCaptions := TNyxSelect<TNyxCollectionItem, TNyxText>.From(LRows,
    TNyxSelector<TNyxCollectionItem, TNyxText>.From(Caption));
  LItem := LItem.WithValue(NyxTextField('caption'), 'Detached');
  LStore := nil;
  LSnapshot := nil;
  LRows := nil;
  Check(LCaptions.Single = CBuildCaption,
    'owned projected rows survive store/snapshot release and detached item changes');
  LRejected := False;
  try
    LRows := NyxItems(nil);
  except
    on E: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'nil snapshot query refuses');
end;

begin
  try
    LazyAndIndependent;
    ProjectionAndValues;
    FailuresAndOwnership;
    ManagedElements;
    SnapshotIntegration;
    Inc(GChecks, RunNyxLinqOrderChecks);
    Inc(GChecks, RunNyxProjectionChecks);
    Inc(GChecks, RunNyxJoinChecks);
    Inc(GChecks, RunNyxSetChecks);
    Inc(GChecks, RunNyxPartitionChecks);
    Inc(GChecks, RunNyxAggregateChecks);
    Inc(GChecks, RunNyxMaterializationChecks);
    Inc(GChecks, RunNyxCallbackBoundaryChecks);
    Inc(GChecks, RunNyxCombinationChecks);
    Inc(GChecks, RunNyxLinqAuthoringChecks);
    Inc(GChecks, RunNyxPrecisionChecks);
    Inc(GChecks, RunNyxProgramChecks);
    {$ifdef NYX_COMPILED_LINQ}SemanticCompanion;{$endif}
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' typed deferred LINQ checks';
    document.body.setAttribute('data-nyx-linq', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' typed deferred LINQ checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      document.body.setAttribute('data-nyx-linq', 'failed');
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
