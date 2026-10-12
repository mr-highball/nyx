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

unit nyx.test.linq.partitions;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

interface

function RunNyxPartitionChecks: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.linq, nyx.linq.grouping, nyx.callback.scope;

type
  TIntSequence = INyxSequence<Integer>;
  EPartitionCallback = class(Exception);
  TProbe = class
  public
    Starts: Integer;
    Ends: Integer;
    Moves: Integer;
    FailAt: Integer;
    NoCursor: Boolean;
    constructor Create;
  end;
  TProbeCursor = class(TEnumerator<Integer>)
  private
    FProbe: TProbe;
    FIndex: Integer;
    FCount: Integer;
  protected
    function Advance(out AValue: Integer): Boolean; override;
  public
    constructor Create(AProbe: TProbe; ACount: Integer);
    destructor Destroy; override;
  end;
  TProbeSequence = class(TNyxSequence<Integer>)
  private
    FProbe: TProbe;
    FCount: Integer;
  public
    constructor Create(AProbe: TProbe; ACount: Integer);
    function GetEnumerator: INyxEnumerator<Integer>; override;
  end;
  TLifetime = class
    Destroyed: Integer;
  end;
  TManaged = class(TInterfacedObject)
  private
    FLifetime: TLifetime;
  public
    constructor Create(ALifetime: TLifetime);
    destructor Destroy; override;
  end;

var
  GChecks: Integer;
  GNextCalls: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxQuery.Create('Partition qualification: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Rows(const ASource: TIntSequence; const AExpected: array of Integer;
  const AReason: TNyxText);
var
  LValues: TNyxSequenceArray<Integer>;
  LIndex: Integer;
  LEqual: Boolean;
begin
  LValues := ASource.ToArray;
  LEqual := Length(LValues) = Length(AExpected);

  if LEqual then
  begin
    for LIndex := 0 to High(LValues) do
    begin
      LEqual := LEqual and (LValues[LIndex] = AExpected[LIndex]);
    end;
  end;
  Check(LEqual, AReason);
end;

function NextValue(const AValue: Integer): Integer;
begin
  Inc(GNextCalls);
  Result := AValue + 2;
end;

function FailNext(const AValue: Integer): Integer;
begin
  Result := AValue;
  raise EPartitionCallback.Create('next failed / 🌙');
end;

constructor TProbe.Create;
begin
  inherited Create;
  FailAt := -1;
end;

constructor TProbeCursor.Create(AProbe: TProbe; ACount: Integer);
begin
  inherited Create;
  FProbe := AProbe;
  FCount := ACount;
  FIndex := -1;
  Inc(FProbe.Starts);
end;

destructor TProbeCursor.Destroy;
begin
  Inc(FProbe.Ends);
  inherited Destroy;
end;

function TProbeCursor.Advance(out AValue: Integer): Boolean;
begin
  Inc(FProbe.Moves);
  Inc(FIndex);

  if FIndex = FProbe.FailAt then
  begin
    raise EPartitionCallback.Create('source failed / 🌙');
  end;
  Result := FIndex < FCount;

  if Result then
  begin
    AValue := FIndex;
  end;
end;

constructor TProbeSequence.Create(AProbe: TProbe; ACount: Integer);
begin
  inherited Create;
  FProbe := AProbe;
  FCount := ACount;
end;

function TProbeSequence.GetEnumerator: INyxEnumerator<Integer>;
begin

  if FProbe.NoCursor then
  begin
    Exit(nil);
  end;
  Result := TProbeCursor.Create(FProbe, FCount);
end;

constructor TManaged.Create(ALifetime: TLifetime);
begin
  inherited Create;
  FLifetime := ALifetime;
end;

destructor TManaged.Destroy;
begin
  Inc(FLifetime.Destroyed);
  inherited Destroy;
end;

procedure GenerationAndBounds;
var
  LSource: TIntSequence;
  LCursor: INyxEnumerator<Integer>;
  LOther: INyxEnumerator<Integer>;
  LRefused: Boolean;
begin
  Rows(NyxRange(-2, 5), [-2, -1, 0, 1, 2], 'ascending range crosses zero exactly');
  Rows(NyxRange(5, 3, -2), [5, 3, 1], 'descending range preserves step');
  Rows(NyxRange(High(Integer), 1), [High(Integer)], 'last positive value does not increment afterward');
  Rows(NyxRange(Low(Integer), 1, -1), [Low(Integer)], 'last negative value does not decrement afterward');
  Rows(NyxRange(High(Integer), 2, Low(Integer)), [High(Integer), -1], 'Low(Integer) step is validated without negation overflow');
  Rows(NyxRange(Low(Integer), 3, High(Integer)), [Low(Integer), -1, High(Integer) - 1],
    'range validation preserves a span wider than signed Integer');
  Rows(NyxRange(Low(Integer), High(Integer), 2).Take(3),
    [Low(Integer), Low(Integer) + 2, Low(Integer) + 4], 'huge valid range streams a bounded prefix');
  Check(not NyxRange(High(Integer), 0).Any, 'empty range performs no arithmetic');
  LRefused := False;
  try
    NyxRange(High(Integer), 2);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'ascending overflow refuses at construction');
  LRefused := False;
  try
    NyxRange(Low(Integer), 2, -1);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'descending overflow refuses at construction');
  LRefused := False;
  try
    NyxRange(0, High(Integer), High(Integer));
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'unsafe huge span refuses before browser multiplication');
  LRefused := False;
  try
    NyxRange(0, -1);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'negative range count refuses');
  LRefused := False;
  try
    NyxRange(0, 2, 0);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'zero range step refuses');
  Rows(TNyxSequence<Integer>.RepeatValue(7, 3), [7, 7, 7], 'typed repetition preserves multiplicity');
  GNextCalls := 0;
  LSource := TNyxSequence<Integer>.Iterate(1, 4, TNyxSelector<Integer, Integer>.From(NextValue));
  LCursor := LSource.GetEnumerator;
  LOther := LSource.GetEnumerator;
  Check(GNextCalls = 0, 'generation construction/acquisition enters no callback');
  Check(LCursor.MoveNext and (LCursor.Current = 1) and (GNextCalls = 0), 'seed yields before next callback');
  Check(LCursor.MoveNext and (LCursor.Current = 3) and (GNextCalls = 1), 'next callback runs only between results');
  Check(LOther.MoveNext and (LOther.Current = 1), 'generated cursors start independently');
  GNextCalls := 0;
  Rows(LSource, [1, 3, 5, 7], 'iterate generates exact finite values');
  Check(GNextCalls = 3, 'no callback runs after the last generated value');
  LCursor := TNyxSequence<Integer>.Iterate(1, 2,
    TNyxSelector<Integer, Integer>.From(FailNext)).GetEnumerator;
  Check(LCursor.MoveNext and (LCursor.Current = 1), 'a later generation failure does not preempt seed');
  LRefused := False;
  try
    LCursor.MoveNext;
  except
    on EPartitionCallback do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LCursor.MoveNext, 'generation callback failure is original/terminal');
end;

procedure ValuesAndFallbacks;
var
  LSource: TIntSequence;
  LEmpty: TIntSequence;
  LRefused: Boolean;
  LValue: Integer;
  LText: INyxSequence<TNyxText>;
  LProbe: TProbe;
begin
  LSource := NyxRange(1, 3);
  LEmpty := TNyxSequence<Integer>.Empty;
  Rows(LSource.Append(4).Prepend(0), [0, 1, 2, 3, 4], 'fluent insertion preserves encounter order');
  Rows(LEmpty.DefaultIfEmpty, [0], 'implicit fallback is ordinary initialized Integer');
  Rows(LEmpty.DefaultIfEmpty(9), [9], 'explicit empty fallback emits once');
  Rows(LSource.DefaultIfEmpty(9), [1, 2, 3], 'nonempty source never emits fallback');
  Check((LEmpty.FirstOrDefault = 0) and (LEmpty.LastOrDefault = 0)
    and (LEmpty.SingleOrDefault = 0), 'default element terminals agree across targets');
  Check((LEmpty.FirstOrDefault(9) = 9) and (LEmpty.LastOrDefault(8) = 8)
    and (LEmpty.SingleOrDefault(7) = 7), 'caller fallbacks remain typed values');
  Check((LSource.FirstOrDefault(9) = 1) and (LSource.LastOrDefault(9) = 3),
    'nonempty terminals return source values');
  Check((LSource.ElementAtOrDefault(1) = 2) and (LSource.ElementAtOrDefault(9, 7) = 7),
    'element position returns value or explicit fallback');
  LRefused := False;
  try
    LSource.SingleOrDefault(9);
  except
    on ENyxQueryCardinality do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'single fallback never hides excessive cardinality');
  LText := TNyxSequence<TNyxText>.Empty;
  Check(LText.FirstOrDefault(TNyxText('🌙') + TNyxText(#0)) = TNyxText('🌙') + TNyxText(#0),
    'supplementary/NUL fallback text stays exact');
  LProbe := TProbe.Create;
  try
    LSource := TProbeSequence.Create(LProbe, 3);
    Check(not LSource.TryElementAt(-1, LValue) and (LValue = 0) and (LProbe.Starts = 0),
      'negative optional element is absent without source entry');
    Check((LSource.ElementAtOrDefault(-1, 9) = 9) and (LProbe.Starts = 0),
      'negative fallback element avoids source acquisition');
    Rows(LSource.Prepend(9).Take(1), [9], 'prepend prefix does not acquire source');
    Check(LProbe.Starts = 0, 'prepend defers source entry beyond inserted value');
  finally
    LSource := nil;
    LProbe.Free;
  end;
end;

procedure TailsAndChunks;
var
  LSource: TIntSequence;
  LChunks: INyxSequence<TIntSequence>;
  LCursor: INyxEnumerator<TIntSequence>;
  LOther: INyxEnumerator<TIntSequence>;
  LRetained: TIntSequence;
  LProbe: TProbe;
  LRefused: Boolean;
begin
  LSource := NyxRange(0, 5);
  Rows(LSource.SkipLast(2), [0, 1, 2], 'SkipLast keeps the prefix before its tail');
  Rows(LSource.TakeLast(2), [3, 4], 'TakeLast preserves final encounter order');
  Rows(LSource.Reverse, [4, 3, 2, 1, 0], 'Reverse preserves exact reversed order');
  Rows(LSource.SkipLast(0), [0, 1, 2, 3, 4], 'zero skip tail passes through');
  Rows(LSource.TakeLast(0), [], 'zero take tail is empty');
  Rows(LSource.SkipLast(High(Integer)), [], 'huge requested skip tail handles short input');
  Rows(LSource.TakeLast(High(Integer)), [0, 1, 2, 3, 4], 'huge requested take tail stores only actual rows');
  Rows(NyxRange(0, 100).TakeLast(3), [97, 98, 99], 'tail ring wraps many times without order loss');
  Rows(NyxRange(0, 100).SkipLast(3).TakeLast(3), [94, 95, 96], 'streaming lookahead composes with retained tail');
  LChunks := TNyxChunk<Integer>.From(LSource, 2);
  LCursor := LChunks.GetEnumerator;
  LOther := LChunks.GetEnumerator;
  Check(LCursor.MoveNext, 'first owned chunk exists');
  LRetained := LCursor.Current;
  Rows(LRetained, [0, 1], 'first chunk values are independent');
  Check(LOther.MoveNext, 'independent chunk cursor starts at first chunk');
  LChunks := nil;
  LSource := nil;
  Check(LCursor.MoveNext, 'second chunk survives recipe/source release');
  Rows(LCursor.Current, [2, 3], 'full chunk membership is exact');
  Check(LCursor.MoveNext, 'short final chunk exists');
  Rows(LCursor.Current, [4], 'short tail contains no default padding');
  Check(not LCursor.MoveNext and not LCursor.MoveNext, 'chunk EOF is terminal');
  LCursor := nil;
  LOther := nil;
  Rows(LRetained, [0, 1], 'retained chunk outlives both outer cursors');
  LChunks := TNyxChunk<Integer>.From(NyxRange(0, 2), High(Integer));
  Rows(LChunks.Single, [0, 1], 'huge chunk size grows only with actual short input');
  LRefused := False;
  try
    TNyxChunk<Integer>.From(NyxRange(0, 2), 0);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'zero chunk size refuses before source entry');
  LProbe := TProbe.Create;
  try
    LSource := TProbeSequence.Create(LProbe, 5);
    LChunks := TNyxChunk<Integer>.From(LSource, 2);
    LCursor := LChunks.GetEnumerator;
    Check((LProbe.Starts = 0) and (LProbe.Moves = 0), 'chunk acquisition is fully deferred');
    Check(LCursor.MoveNext and (LProbe.Moves = 2), 'chunk reads its size without next-row lookahead');
    LCursor := nil;
    Check(LProbe.Starts = LProbe.Ends, 'early chunk cursor disposal retires source');
    LSource.TakeLast(0).Count;
    Check(LProbe.Starts = LProbe.Ends, 'zero take tail enters no new cursor');
    LProbe.FailAt := 1;
    LCursor := LChunks.GetEnumerator;
    LRefused := False;
    try
      LCursor.MoveNext;
    except
      on EPartitionCallback do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LCursor.MoveNext, 'failed partial chunk emits no partial result and stays terminal');
  finally
    LCursor := nil;
    LChunks := nil;
    LSource := nil;
    LProbe.Free;
  end;
end;

procedure BufferedFailures;
var
  LProbe: TProbe;
  LSource: TIntSequence;
  LCursor: INyxEnumerator<Integer>;
  LRefused: Boolean;
begin
  LProbe := TProbe.Create;
  try
    LProbe.FailAt := 3;
    LSource := TProbeSequence.Create(LProbe, 5);
    LCursor := LSource.TakeLast(2).GetEnumerator;
    Check(LProbe.Starts = 0, 'tail cursor acquisition stays deferred');
    LRefused := False;
    try
      LCursor.MoveNext;
    except
      on EPartitionCallback do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LCursor.MoveNext and (LProbe.Starts = LProbe.Ends),
      'failed full tail preparation publishes nothing and retires source');
    LCursor := LSource.Reverse.GetEnumerator;
    LRefused := False;
    try
      LCursor.MoveNext;
    except
      on EPartitionCallback do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LCursor.MoveNext and (LProbe.Starts = LProbe.Ends),
      'failed reverse preparation publishes nothing and remains original/terminal');
    LProbe.NoCursor := True;
    LCursor := LSource.Append(9).GetEnumerator;
    LRefused := False;
    try
      LCursor.MoveNext;
    except
      on ENyxQuery do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LCursor.MoveNext, 'inserted value cannot conceal an invalid source cursor');
    LRefused := False;
    try
      LSource.FirstOrDefault(9);
    except
      on ENyxQuery do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'first fallback refuses an invalid source cursor');
    LRefused := False;
    try
      LSource.LastOrDefault(9);
    except
      on ENyxQuery do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'last fallback refuses an invalid source cursor');
    LRefused := False;
    try
      LSource.SingleOrDefault(9);
    except
      on ENyxQuery do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'single fallback refuses an invalid source cursor');
  finally
    LCursor := nil;
    LSource := nil;
    LProbe.Free;
  end;
end;

procedure ScopedGeneration;
var
  LOffset: Integer;
  LLease: INyxCallbackLease;
  LVariable: TNyxSelectorNested<Integer, Integer>;
  LSource: TIntSequence;
  LRefused: Boolean;

  function NestedNext(const AValue: Integer): Integer;
  begin
    Result := AValue + LOffset;
  end;

begin
  LOffset := 3;
  LLease := NewNyxCallbackLease;
  try
    LVariable := NestedNext;
    LSource := TNyxSequence<Integer>.Iterate(1, 3,
      TNyxSelector<Integer, Integer>.FromNested(LVariable, LLease));
    Rows(LSource, [1, 4, 7], 'named nested generation variables preserve their scope');
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    Rows(TNyxSequence<Integer>.Iterate(1, 3,
      TNyxSelector<Integer, Integer>.FromInline(
        function(const AValue: Integer): Integer
        begin
          Result := AValue + LOffset;
        end)), [1, 4, 7], 'qualified inline generation preserves scalar capture');
    {$endif}
  finally
    LLease.Close;
  end;
  LRefused := False;
  try
    LSource.ToArray;
  except
    on ENyxCallbackExpired do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'closed generation lease refuses borrowed frame entry');
end;

procedure ManagedChunks;
var
  LLifetime: TLifetime;
  LValue: IInterface;
  LRead: IInterface;
  LRows: TNyxSequenceArray<IInterface>;
  LSource: INyxSequence<IInterface>;
  LChunks: INyxSequence<INyxSequence<IInterface>>;
  LCursor: INyxEnumerator<INyxSequence<IInterface>>;
  LRetained: INyxSequence<IInterface>;
begin
  LLifetime := TLifetime.Create;
  try
    LValue := TManaged.Create(LLifetime);
    SetLength(LRows, 2);
    LRows[0] := LValue;
    LRows[1] := LValue;
    LSource := TNyxSequence<IInterface>.From(LRows);
    LRows := nil;
    LChunks := TNyxChunk<IInterface>.From(LSource, 1);
    LCursor := LChunks.GetEnumerator;
    Check(LCursor.MoveNext, 'managed owned chunk exists');
    LRetained := LCursor.Current;
    LValue := nil;
    LSource := nil;
    LChunks := nil;
    LCursor := nil;
    Check(LLifetime.Destroyed = 0, 'managed chunk retains payload after source and outer cursor disposal');
    LRead := LRetained.First;
    LRetained := nil;
    Check((LRead <> nil) and (LLifetime.Destroyed = 0), 'chunk reader independently retains its payload');
    LRead := nil;
    Check(LLifetime.Destroyed = 1, 'last managed chunk reader retires payload once');
  finally
    LRows := nil;
    LRead := nil;
    LRetained := nil;
    LSource := nil;
    LChunks := nil;
    LCursor := nil;
    LValue := nil;
    LLifetime.Free;
  end;
end;

procedure ManagedGenerationAndTails;
var
  LLifetime: TLifetime;
  LValue: IInterface;
  LRead: IInterface;
  LRepeated: INyxSequence<IInterface>;
  LTail: INyxSequence<IInterface>;
  LInserted: INyxSequence<IInterface>;
  LReversed: INyxSequence<IInterface>;
  LValues: TNyxSequenceArray<IInterface>;
begin
  LLifetime := TLifetime.Create;
  try
    LValue := TManaged.Create(LLifetime);
    { Explicit stages prevent a caller's self-overwrite temporary from extending
      lifetime past the last-owner assertion, as in the maintained set fixture. }
    LRepeated := TNyxSequence<IInterface>.RepeatValue(LValue, 3);
    LTail := LRepeated.TakeLast(2);
    LInserted := LTail.Append(LValue);
    LReversed := LInserted.Reverse;
    LValues := LReversed.ToArray;
    LValue := nil;
    LRepeated := nil;
    LTail := nil;
    LInserted := nil;
    LReversed := nil;
    Check((Length(LValues) = 3) and (LLifetime.Destroyed = 0),
      'generation/tail/insertion/reversal retain managed values in detached results');
    LRead := LValues[0];
    LValues := nil;
    Check((LRead <> nil) and (LLifetime.Destroyed = 0),
      'independent reader survives retirement of every sequence/result vector');
    LRead := nil;
    Check(LLifetime.Destroyed = 1, 'last generated managed reader retires its payload once');
    LValue := TManaged.Create(LLifetime);
    LRepeated := TNyxSequence<IInterface>.Empty;
    LInserted := LRepeated.DefaultIfEmpty(LValue);
    LRead := LInserted.SingleOrDefault;
    LValue := nil;
    LRepeated := nil;
    LInserted := nil;
    Check((LRead <> nil) and (LLifetime.Destroyed = 1), 'managed default sequence retains the explicit fallback');
    LRead := nil;
    Check(LLifetime.Destroyed = 2, 'last explicit fallback reader retires its payload');
  finally
    LValues := nil;
    LRead := nil;
    LValue := nil;
    LRepeated := nil;
    LTail := nil;
    LInserted := nil;
    LReversed := nil;
    LLifetime.Free;
  end;
end;

function RunNyxPartitionChecks: Integer;
begin
  GChecks := 0;
  GenerationAndBounds;
  ValuesAndFallbacks;
  TailsAndChunks;
  BufferedFailures;
  ScopedGeneration;
  ManagedChunks;
  ManagedGenerationAndTails;
  Result := GChecks;
end;

end.
