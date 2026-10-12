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
unit nyx.test.linq.order;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

interface

function RunNyxLinqOrderChecks: Integer;

implementation

uses
  SysUtils, Math, nyx.text, nyx.linq, nyx.callback.scope, nyx.scheduler
  {$ifndef PAS2JS}, Classes{$endif};

type
  TRow = record
    Group: Integer;
    Rank: Integer;
    Ordinal: Integer;
  end;
  TCounter = class
    Destroyed: Integer;
  end;
  TOwnedItem = class(TInterfacedObject)
    Counter: TCounter;
    destructor Destroy; override;
  end;
  ENyxOrderFixture = class(Exception);
  {$ifndef PAS2JS}
  { The worker retains an adapter/entry port, not the enclosing frame's lease.
    Its only attempted borrowed entry must refuse before calling the function. }
  TWrongThreadWork = class(TInterfacedObject, INyxWork)
    Predicate: INyxPredicate<Integer>;
    procedure Execute(const AExecution: INyxExecution);
  end;
  {$endif}

var
  GChecks: Integer;
  GGroups: Integer;
  GRanks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise ENyxOrderFixture.Create('LINQ ordering/callback: ' + AReason);
  end;
  Inc(GChecks);
end;

destructor TOwnedItem.Destroy;
begin
  Inc(Counter.Destroyed);
  inherited Destroy;
end;

function GroupKey(const AValue: TRow): Integer;
begin
  Inc(GGroups);
  Result := AValue.Group;
end;

function RankKey(const AValue: TRow): Integer;
begin
  Inc(GRanks);
  Result := AValue.Rank;
end;

function ExtremeCompare(const ALeft, ARight: Integer): Integer;
begin
  Result := 0;

  if ALeft < ARight then
  begin
    Result := Low(Integer);
  end
  else
  begin

    if ALeft > ARight then
    begin
      Result := High(Integer);
    end;
  end;
end;

function FailingKey(const AValue: Integer): Integer;
begin

  if AValue = 2 then
  begin
    raise ENyxOrderFixture.Create('original key failure');
  end;
  Result := AValue;
end;

procedure Ordering;
var
  LInput: TNyxSequenceArray<TRow>;
  LValues: TNyxSequenceArray<TRow>;
  LSource: INyxSequence<TRow>;
  LSorted: INyxSequence<TRow>;
  LPrimary: INyxOrdering<TRow>;
  LCombined: INyxOrdering<TRow>;
  LCursor: INyxEnumerator<TRow>;
  LOther: INyxEnumerator<TRow>;
  LRow: TRow;
  LIndex: Integer;
  LNumbers: INyxSequence<Integer>;
  LInts: TNyxSequenceArray<Integer>;
  LKeys: INyxOrderKeys;
  LRejected: Boolean;
begin
  SetLength(LInput, 4096);
  for LIndex := 0 to High(LInput) do
  begin
    LInput[LIndex].Group := (4095 - LIndex) mod 8;
    LInput[LIndex].Rank := LIndex mod 5;
    LInput[LIndex].Ordinal := LIndex;
  end;
  LSource := TNyxSequence<TRow>.From(LInput);
  LPrimary := TNyxKeyOrder<TRow, Integer>.By(
    TNyxSelector<TRow, Integer>.From(GroupKey), NewNyxIntegerComparer);
  LCombined := LPrimary.ThenBy(TNyxKeyOrder<TRow, Integer>.By(
    TNyxSelector<TRow, Integer>.From(RankKey), NewNyxIntegerComparer, nodDescending));
  GGroups := 0;
  GRanks := 0;
  LSorted := LSource.OrderBy(LCombined);
  LCursor := LSorted.GetEnumerator;
  LOther := LSorted.GetEnumerator;
  Check((GGroups = 0) and (GRanks = 0), 'recipe/query/cursor construction is deferred');
  Check(LCursor.MoveNext, 'large ordered cursor begins');
  LRow := LCursor.Current;
  Check((LRow.Group = 0) and (LRow.Rank = 4), 'primary ascending/secondary descending');
  Check((GGroups = 4096) and (GRanks = 4096), 'every key projects once per captured row');
  Check(LOther.MoveNext, 'second independent sorted cursor begins');
  Check(LOther.Current.Ordinal = LRow.Ordinal, 'independent sorted cursors retain stable first row');
  Check((GGroups = 8192) and (GRanks = 8192), 'each enumeration prepares its own keys');
  LSorted := nil;
  LSource := nil;
  LCombined := nil;
  LPrimary := nil;
  Check(LCursor.MoveNext and LOther.MoveNext, 'prepared cursors outlive query/recipe/source');
  LCursor := nil;
  LOther := nil;
  LSource := TNyxSequence<TRow>.From(LInput);
  LPrimary := TNyxKeyOrder<TRow, Integer>.By(
    TNyxSelector<TRow, Integer>.From(GroupKey), NewNyxIntegerComparer);
  LCombined := LPrimary.ThenBy(TNyxKeyOrder<TRow, Integer>.By(
    TNyxSelector<TRow, Integer>.From(RankKey), NewNyxIntegerComparer, nodDescending));
  LValues := LSource.OrderBy(LCombined).ToArray;
  Check(Length(LValues) = 4096, 'sorting retains the complete dataset');
  for LIndex := 1 to High(LValues) do
  begin
    Check((LValues[LIndex - 1].Group < LValues[LIndex].Group) or
      ((LValues[LIndex - 1].Group = LValues[LIndex].Group) and
       (LValues[LIndex - 1].Rank >= LValues[LIndex].Rank)), 'all primary/secondary keys ordered');

    if (LValues[LIndex - 1].Group = LValues[LIndex].Group) and
      (LValues[LIndex - 1].Rank = LValues[LIndex].Rank) then
    begin
      Check(LValues[LIndex - 1].Ordinal < LValues[LIndex].Ordinal,
        'equal keys preserve original source order');
    end;
  end;
  GGroups := 0;
  GRanks := 0;
  LValues := LSource.OrderBy(LPrimary).ToArray;
  Check((GGroups = 4096) and (GRanks = 0), 'ThenBy does not mutate its earlier recipe');
  LNumbers := TNyxSequence<Integer>.From([Low(Integer), 0, High(Integer), 0]);
  LInts := LNumbers.OrderBy(TNyxValueOrder<Integer>.By(
    TNyxComparer<Integer>.From(ExtremeCompare), nodDescending)).ToArray;
  Check((LInts[0] = High(Integer)) and (LInts[3] = Low(Integer)),
    'descending safely normalizes extreme comparer results');
  LKeys := TNyxValueOrder<Integer>.By(NewNyxIntegerComparer).Prepare([1, 2]);
  LRejected := False;
  try
    LKeys.Compare(-1, 0);
  except
    on E: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'prepared key bounds refuse');
  LRejected := False;
  try
    LNumbers := LNumbers.OrderBy(nil);
  except
    on E: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'nil ordering refuses');
  LRejected := False;
  try
    LPrimary := LPrimary.ThenBy(nil);
  except
    on E: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'nil secondary recipe refuses');
  LNumbers := TNyxSequence<Integer>.From([1, 2, 3]).OrderBy(
    TNyxKeyOrder<Integer, Integer>.By(
      TNyxSelector<Integer, Integer>.From(FailingKey), NewNyxIntegerComparer));
  LRejected := False;
  try
    LNumbers.First;
  except
    on E: ENyxOrderFixture do
    begin
      LRejected := E.Message = 'original key failure';
    end;
  end;
  Check(LRejected, 'key preparation preserves original failure before yielding');
end;

procedure Comparers;
var
  LText: INyxComparer<TNyxText>;
  LNumber: INyxComparer<Double>;
  LNames: INyxSequence<TNyxText>;
  LValues: TNyxSequenceArray<TNyxText>;
  LNumbers: INyxSequence<Double>;
  LDoubles: TNyxSequenceArray<Double>;
  LSupplementary: TNyxText;
  LHighBMP: TNyxText;
  LBad: TNyxText;
  LRejected: Boolean;
begin
  LSupplementary := NyxScalarText($10000);
  LHighBMP := NyxScalarText($e000);
  LText := NewNyxTextComparer;
  Check(LText.Compare(LHighBMP, LSupplementary) < 0, 'Unicode scalar order crosses UTF-16 surrogate boundary');
  Check(LText.Compare('A' + #0, 'A') > 0, 'NUL extends a prefix instead of terminating text');
  Check(LText.Compare('A', 'a') < 0, 'ordinal text preserves case');
  LText := NewNyxTextComparer(ntoAsciiInsensitive);
  Check(LText.Compare('A', 'a') = 0, 'explicit ASCII-insensitive ordering');
  LNames := TNyxSequence<TNyxText>.From(['a', 'B', 'A', 'b']);
  LValues := LNames.OrderBy(TNyxValueOrder<TNyxText>.By(LText)).ToArray;
  Check((LValues[0] = 'a') and (LValues[1] = 'A') and (LValues[2] = 'B') and (LValues[3] = 'b'),
    'case-insensitive ties stay stable');
  {$ifdef PAS2JS}
  LBad := 'different' + Chr($d800);
  {$else}
  LBad := 'different' + AnsiChar($ff);
  {$endif}
  LRejected := False;
  try
    LText.Compare(LBad, 'A');
  except
    on E: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'malformed tails refuse even after a differing prefix');
  Check(NewNyxBooleanComparer.Compare(False, True) < 0, 'Boolean False precedes True');
  LNumber := NewNyxNumberComparer;
  Check((LNumber.Compare(NaN, 0) < 0) and (LNumber.Compare(NaN, NaN) = 0),
    'default NaN ordering is explicit and consistent');
  Check((LNumber.Compare(NegInfinity, 0) < 0) and (LNumber.Compare(Infinity, 0) > 0),
    'infinities compare without subtraction');
  LNumber := NewNyxNumberComparer(nnoLast);
  Check(LNumber.Compare(NaN, 0) > 0, 'caller can place NaN last');
  LNumbers := TNyxSequence<Double>.From([3, NaN, NegInfinity, 1, Infinity]).OrderBy(
    TNyxValueOrder<Double>.By(LNumber));
  LDoubles := LNumbers.ToArray;
  Check((LDoubles[0] = NegInfinity) and (LDoubles[3] = Infinity) and IsNan(LDoubles[4]),
    'actual number sort includes finite/infinite/NaN values');
  LNumber := NewNyxNumberComparer(nnoRefuse);
  LRejected := False;
  try
    LNumber.Compare(NaN, 0);
  except
    on E: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'caller can refuse NaN');
end;

{$ifndef PAS2JS}
procedure TWrongThreadWork.Execute(const AExecution: INyxExecution);
begin
  Predicate.Test(1);
end;
{$endif}

procedure NestedCallbacks;
var
  LLease: INyxCallbackLease;
  LSource: INyxSequence<Integer>;
  LQuery: INyxSequence<Integer>;
  LSelector: INyxSelector<Integer, Integer>;
  LAction: INyxAction<Integer>;
  LCompare: INyxComparer<Integer>;
  LThreshold: Integer;
  LTotal: Integer;
  LEntries: Integer;
  LRejected: Boolean;
  LPredicateVariable: TNyxPredicateNested<Integer>;
  LSelectorVariable: TNyxSelectorNested<Integer, Integer>;
  LActionVariable: TNyxActionNested<Integer>;
  LCompareVariable: TNyxCompareNested<Integer>;
  {$ifndef PAS2JS}
  LScheduler: INyxScheduler;
  LExecution: INyxExecution;
  LWork: TWrongThreadWork;
  LMonitor: INyxSchedulerMonitor;
  LDeadline: QWord;
  {$endif}
  function Above(const AValue: Integer): Boolean;
  begin
    Inc(LEntries);
    Result := AValue > LThreshold;
  end;
  function Offset(const AValue: Integer): Integer;
  begin
    Result := AValue + LThreshold;
  end;
  procedure Add(const AValue: Integer);
  begin
    Inc(LTotal, AValue);
  end;
  function Compare(const ALeft, ARight: Integer): Integer;
  begin
    Result := ExtremeCompare(ALeft + LThreshold, ARight + LThreshold);
  end;
begin
  LLease := NewNyxCallbackLease;
  LThreshold := 2;
  LTotal := 0;
  LEntries := 0;
  try
    LSource := TNyxSequence<Integer>.From([1, 2, 3, 4]);
    LPredicateVariable := Above;
    LSelectorVariable := Offset;
    LActionVariable := Add;
    LCompareVariable := Compare;
    LQuery := LSource.Where(TNyxPredicate<Integer>.FromNested(LPredicateVariable, LLease));
    Check((LQuery.Count = 2) and (LEntries = 4), 'nested predicate reads its enclosing variable');
    LSelector := TNyxSelector<Integer, Integer>.FromNested(LSelectorVariable, LLease);
    Check(LSource.Select(LSelector).Last = 6, 'nested selector reads its enclosing variable');
    LAction := TNyxAction<Integer>.FromNested(LActionVariable, LLease);
    LSource.ForEach(LAction);
    Check(LTotal = 10, 'nested procedure writes its enclosing variable synchronously');
    LCompare := TNyxComparer<Integer>.FromNested(LCompareVariable, LLease);
    Check(LSource.OrderBy(TNyxValueOrder<Integer>.By(LCompare, nodDescending)).First = 4,
      'nested comparer participates in typed stable ordering');
    {$ifndef PAS2JS}
    LScheduler := NewNyxScheduler;
    try
      LEntries := 0;
      LWork := TWrongThreadWork.Create;
      LWork.Predicate := TNyxPredicate<Integer>.FromNested(Above, LLease);
      LExecution := LScheduler.Submit(LWork, neThreaded);
      LDeadline := GetTickCount64 + 10000;
      while (LExecution.Status in [nesPending, nesRunning]) and (GetTickCount64 < LDeadline) do
      begin
        CheckSynchronize(1);
      end;
      Check((LExecution.Status = nesFailed) and (LEntries = 0) and
        (LExecution.Failure = 'Nested callbacks require their originating thread'),
        'real native worker refuses before dereferencing the nested frame');
    finally
      LScheduler.Shutdown;
      Check(Supports(LScheduler, INyxSchedulerMonitor, LMonitor), 'native pool retirement can be observed');
      LDeadline := GetTickCount64 + 10000;
      while (LMonitor.WorkerLoad.ActiveWorkers > 0) and (GetTickCount64 < LDeadline) do
      begin
        CheckSynchronize(1);
      end;
      Check(LMonitor.WorkerLoad.ActiveWorkers = 0, 'borrowed-callback worker pool retires');
    end;
    {$endif}
  finally
    LLease.Close;
  end;
  LRejected := False;
  try
    LQuery.Count;
  except
    on E: ENyxCallbackExpired do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'retained nested query refuses after explicit scope close');
  LRejected := False;
  try
    LAction.Execute(1);
  except
    on E: ENyxCallbackExpired do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'retained nested procedure refuses after scope close');
  LLease := NewNyxCallbackLease;
  LQuery := LSource.Where(TNyxPredicate<Integer>.FromNested(Above, LLease));
  LLease := nil;
  LRejected := False;
  try
    LQuery.Count;
  except
    on E: ENyxCallbackExpired do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'releasing the last caller lease also revokes retained adapters');
end;

{$ifdef NYX_HAS_INLINE_CALLBACKS}
function CapturedQuery: INyxSequence<Integer>;
var
  LThreshold: Integer;
  LOffset: Integer;
begin
  LThreshold := 2;
  LOffset := 10;
  Result := TNyxSequence<Integer>.From([1, 2, 3, 4])
    .Where(function(const AValue: Integer): Boolean
      begin
        Result := AValue > LThreshold;
      end)
    .Select(function(const AValue: Integer): Integer
      begin
        Result := AValue + LOffset;
      end);
end;

function CapturedInterface(ACounter: TCounter): INyxSequence<Integer>;
var
  LItem: TOwnedItem;
  LKeep: IInterface;
begin
  LItem := TOwnedItem.Create;
  LItem.Counter := ACounter;
  LKeep := LItem;
  Result := TNyxSequence<Integer>.From([1]).Where(
    function(const AValue: Integer): Boolean
    begin
      Result := (LKeep <> nil) and (AValue = 1);
    end, NyxCapture([LKeep]));
end;

procedure InlineCallbacks;
var
  LQuery: INyxSequence<Integer>;
  LCounter: TCounter;
  LTotal: Integer;
  LText: INyxSequence<TNyxText>;
  LPrefix: TNyxText;
  LComparer: INyxComparer<Integer>;
begin
  Check(NyxHasInlineCallbacks, 'qualified compiler version exposes inline callbacks');
  LQuery := CapturedQuery;
  Check((LQuery.First = 13) and (LQuery.Last = 14), 'owning captured functions execute after their routine returns');
  LTotal := 0;
  LQuery.ForEach(procedure(const AValue: Integer)
    begin
      Inc(LTotal, AValue);
    end);
  Check(LTotal = 27, 'inline procedure capture supports synchronous ForEach');
  LPrefix := NyxScalarText($1f319);
  LText := TNyxSelect<Integer, TNyxText>.From(LQuery,
    TNyxSelector<Integer, TNyxText>.FromInline(
      function(const AValue: Integer): TNyxText
      begin
        Result := LPrefix + ' ' + TNyxText(IntToStr(AValue));
      end));
  Check(LText.First = LPrefix + ' 13', 'inline type-changing projection retains Unicode');
  LComparer := TNyxComparer<Integer>.FromInline(
    function(const ALeft, ARight: Integer): Integer
    begin
      Result := ExtremeCompare(ALeft, ARight);
    end);
  Check(LQuery.OrderBy(TNyxValueOrder<Integer>.By(LComparer, nodDescending)).First = 14,
    'inline comparer works with fluent ordering');
  LCounter := TCounter.Create;
  try
    LQuery := CapturedInterface(LCounter);
    Check(LCounter.Destroyed = 0, 'owning closure retains its captured interface after scope exit');
    Check(LQuery.Single = 1, 'retained interface capture executes');
    LQuery := nil;
    Check(LCounter.Destroyed = 1, 'last query retires the captured interface exactly once');
  finally
    LQuery := nil;
    LCounter.Free;
  end;
end;
{$endif}

function RunNyxLinqOrderChecks: Integer;
begin
  GChecks := 0;
  Ordering;
  Comparers;
  NestedCallbacks;
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  InlineCallbacks;
  {$else}
  Check(not NyxHasInlineCallbacks, 'disabled/older compiler keeps the non-inline contract');
  {$endif}
  Result := GChecks;
end;

end.
