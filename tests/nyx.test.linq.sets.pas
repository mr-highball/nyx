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

unit nyx.test.linq.sets;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

interface

function RunNyxSetChecks: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.linq, nyx.linq.grouping, nyx.callback.scope;

type
  ESetCallback = class(Exception);
  { Fixture probes are owned by the enclosing procedure; every recipe/cursor is
    explicitly retired before the probe is freed. No product ownership is borrowed. }
  TProbe = class
  public
    Starts: Integer;
    Ends: Integer;
    Moves: Integer;
    NoCursor: Boolean;
    FailAt: Integer;
    constructor Create;
  end;
  TProbeCursor = class(TEnumerator<Integer>)
  private
    FProbe: TProbe;
    FValues: TNyxSequenceArray<Integer>;
    FIndex: Integer;
  protected
    function Advance(out AValue: Integer): Boolean; override;
  public
    constructor Create(AProbe: TProbe; const AValues: TNyxSequenceArray<Integer>);
    destructor Destroy; override;
  end;
  TProbeSequence = class(TNyxSequence<Integer>)
  private
    FProbe: TProbe;
    FValues: TNyxSequenceArray<Integer>;
  public
    constructor Create(AProbe: TProbe; const AValues: array of Integer);
    function GetEnumerator: INyxEnumerator<Integer>; override;
  end;
  TEqualityProbe = class(TInterfacedObject, INyxEquality<Integer>)
  public
    Hashes: Integer;
    Comparisons: Integer;
    Collision: Boolean;
    FailHash: Boolean;
    FailEqual: Boolean;
    function Equal(const ALeft, ARight: Integer): Boolean;
    function Hash(const AValue: Integer): Cardinal;
  end;
  TEntry = record
    Key: Integer;
    Text: TNyxText;
  end;
  TLifetime = class
    Destroyed: Integer;
  end;
  IEntryValue = interface
    ['{742D841D-EA3D-4DD8-B7AC-A2E1FE150CBB}']
    function GetKey: Integer;
  end;
  TManagedEquality = class(TInterfacedObject, INyxEquality<IEntryValue>)
    function Equal(const ALeft, ARight: IEntryValue): Boolean;
    function Hash(const AValue: IEntryValue): Cardinal;
  end;
  TEntryValue = class(TInterfacedObject, IEntryValue)
  private
    FKey: Integer;
    FLifetime: TLifetime;
  public
    constructor Create(AKey: Integer; ALifetime: TLifetime);
    destructor Destroy; override;
    function GetKey: Integer;
  end;

var
  GChecks: Integer;
  GKeys: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxQuery.Create('Set qualification: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Rows(const ASource: INyxSequence<Integer>; const AExpected: array of Integer;
  const AReason: TNyxText);
var
  LRows: TNyxSequenceArray<Integer>;
  LIndex: Integer;
  LEqual: Boolean;
begin
  LRows := ASource.ToArray;
  LEqual := Length(LRows) = Length(AExpected);

  if LEqual then
  begin
    for LIndex := 0 to High(LRows) do
    begin
      LEqual := LEqual and (LRows[LIndex] = AExpected[LIndex]);
    end;
  end;
  Check(LEqual, AReason);
end;

function ExactKey(const AValue: Integer): Integer;
begin
  Inc(GKeys);
  Result := AValue;
end;

function Parity(const AValue: Integer): Integer;
begin
  Inc(GKeys);
  Result := AValue mod 2;
end;

function FailingKey(const AValue: Integer): Integer;
begin
  Result := AValue;

  if AValue = 9 then
  begin
    raise ESetCallback.Create('key failed / 🌙');
  end;
end;

function EntryKey(const AValue: TEntry): Integer;
begin
  Result := AValue.Key;
end;

function ManagedKey(const AValue: IEntryValue): Integer;
begin
  Result := AValue.GetKey;
end;

function TManagedEquality.Equal(const ALeft, ARight: IEntryValue): Boolean;
begin
  Result := ALeft = ARight;
end;

function TManagedEquality.Hash(const AValue: IEntryValue): Cardinal;
begin
  { Deliberate collisions qualify managed interface keys without unportable
    pointer hashing or dependence on a browser wrapper's representation. }
  Result := 0;
end;

constructor TProbe.Create;
begin
  inherited Create;
  FailAt := -1;
end;

constructor TProbeSequence.Create(AProbe: TProbe; const AValues: array of Integer);
var
  LIndex: Integer;
begin
  inherited Create;
  FProbe := AProbe;
  SetLength(FValues, Length(AValues));
  for LIndex := 0 to High(FValues) do
  begin
    FValues[LIndex] := AValues[LIndex];
  end;
end;

function TProbeSequence.GetEnumerator: INyxEnumerator<Integer>;
begin

  if FProbe.NoCursor then
  begin
    Exit(nil);
  end;
  Result := TProbeCursor.Create(FProbe, FValues);
end;

constructor TProbeCursor.Create(AProbe: TProbe; const AValues: TNyxSequenceArray<Integer>);
begin
  inherited Create;
  FProbe := AProbe;
  FValues := AValues;
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
    raise ESetCallback.Create('source failed / 🌙');
  end;
  Result := FIndex < Length(FValues);

  if Result then
  begin
    AValue := FValues[FIndex];
  end;
end;

function TEqualityProbe.Equal(const ALeft, ARight: Integer): Boolean;
begin
  Inc(Comparisons);

  if FailEqual then
  begin
    raise ESetCallback.Create('equal failed / 🌙');
  end;
  Result := ALeft = ARight;
end;

function TEqualityProbe.Hash(const AValue: Integer): Cardinal;
begin
  Inc(Hashes);

  if FailHash then
  begin
    raise ESetCallback.Create('hash failed / 🌙');
  end;

  if Collision then
  begin
    Result := High(Cardinal);
  end
  else
  begin
    Result := NewNyxIntegerEquality.Hash(AValue);
  end;
end;

constructor TEntryValue.Create(AKey: Integer; ALifetime: TLifetime);
begin
  inherited Create;
  FKey := AKey;
  FLifetime := ALifetime;
end;

destructor TEntryValue.Destroy;
begin
  Inc(FLifetime.Destroyed);
  inherited Destroy;
end;

function TEntryValue.GetKey: Integer;
begin
  Result := FKey;
end;

procedure OrdinaryAndByKeys;
var
  LSource: INyxSequence<Integer>;
  LOther: INyxSequence<Integer>;
  LKeys: INyxSelector<Integer, Integer>;
  LEquality: INyxEquality<Integer>;
  LEmpty: INyxSequence<Integer>;
  LEntry: TEntry;
  LEntries: TNyxSequenceArray<TEntry>;
  LDistinct: TNyxSequenceArray<TEntry>;
  LTexts: INyxSequence<TNyxText>;
  LTextRows: TNyxSequenceArray<TNyxText>;
  LRefused: Boolean;
begin
  LSource := TNyxSequence<Integer>.From([3, 1, 3, 2, 1]);
  LOther := TNyxSequence<Integer>.From([2, 4, 2]);
  LEquality := NewNyxIntegerEquality;
  LEmpty := TNyxSequence<Integer>.Empty;
  Rows(LSource.Distinct(LEquality), [3, 1, 2], 'Distinct retains first encounter order');
  Rows(LSource.Union(LOther, LEquality), [3, 1, 2, 4], 'Union streams left then unseen right values');
  Rows(LSource.Intersect(LOther, LEquality), [2], 'Intersect emits one occurrence per matching key');
  Rows(LSource.ExceptWith(LOther, LEquality), [3, 1], 'Except emits first distinct excluded-left values');
  Rows(LEmpty.Union(LSource, LEquality), [3, 1, 2], 'empty left union enters and deduplicates right');
  Rows(LSource.Union(LEmpty, LEquality), [3, 1, 2], 'empty right union retains left values');
  Rows(LSource.Intersect(LEmpty, LEquality), [], 'empty membership input has no intersection');
  Rows(LSource.ExceptWith(LEmpty, LEquality), [3, 1, 2], 'empty exclusion input still removes left duplicates');
  Rows(LEmpty.Distinct(LEquality), [], 'empty distinct has ordinary EOF');
  Rows(LEmpty.Intersect(LOther, LEquality), [], 'empty left intersection remains empty');
  Rows(LEmpty.ExceptWith(LOther, LEquality), [], 'empty left exclusion remains empty');
  LKeys := TNyxSelector<Integer, Integer>.From(Parity);
  LSource := TNyxSequence<Integer>.From([7, 3, 8, 2]);
  GKeys := 0;
  Rows(TNyxSetBy<Integer, Integer>.DistinctBy(LSource, LKeys, LEquality),
    [7, 8], 'DistinctBy emits first source value, not its projected key');
  Check(GKeys = 4, 'key projection runs once for each consumed source row');
  Rows(TNyxSetBy<Integer, Integer>.UnionBy(LSource,
    TNyxSequence<Integer>.From([5, 10, 9]), LKeys, LEquality),
    [7, 8], 'UnionBy projects keys in both typed value inputs');
  Rows(TNyxSetBy<Integer, Integer>.IntersectBy(LSource,
    TNyxSequence<Integer>.From([1, 1]), LKeys, LEquality),
    [7], 'IntersectBy consumes matching typed keys once');
  Rows(TNyxSetBy<Integer, Integer>.ExceptBy(LSource,
    TNyxSequence<Integer>.From([1]), LKeys, LEquality),
    [8], 'ExceptBy excludes keys and deduplicates remaining keys');
  SetLength(LEntries, 3);
  LEntry.Key := 2;
  LEntry.Text := TNyxText('First / 🌙') + TNyxText(#0);
  LEntries[0] := LEntry;
  LEntry.Text := 'Second';
  LEntries[1] := LEntry;
  LEntry.Key := 4;
  LEntry.Text := 'Fourth';
  LEntries[2] := LEntry;
  LDistinct := TNyxSetBy<TEntry, Integer>.DistinctBy(TNyxSequence<TEntry>.From(LEntries),
    TNyxSelector<TEntry, Integer>.From(EntryKey), LEquality).ToArray;
  LEntries := nil;
  Check((Length(LDistinct) = 2) and (LDistinct[0].Text = TNyxText('First / 🌙') + TNyxText(#0))
    and (LDistinct[1].Key = 4), 'key-based operations preserve the original managed record and exact text');
  LTexts := TNyxSequence<TNyxText>.From([TNyxText('A🌙') + TNyxText(#0),
    TNyxText('a🌙') + TNyxText(#0), TNyxText('a🌙')]);
  LTextRows := LTexts.Distinct(NewNyxTextEquality(ntoAsciiInsensitive)).ToArray;
  Check((Length(LTextRows) = 2) and (LTextRows[0] = TNyxText('A🌙') + TNyxText(#0)),
    'scalar/ASCII equality resolves supplementary text and significant NUL without rewriting values');
  LRefused := False;
  try
    LSource.Distinct(nil);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'nil equality refuses during construction');
  LRefused := False;
  try
    LSource.Union(nil, LEquality);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'nil binary input refuses during construction');
  LRefused := False;
  try
    TNyxSetBy<Integer, Integer>.DistinctBy(LSource, nil, LEquality);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'nil key callback refuses during construction');
end;

procedure DeferredAndRetirement;
var
  LLeft: TProbe;
  LRight: TProbe;
  LSource: INyxSequence<Integer>;
  LOther: INyxSequence<Integer>;
  LQuery: INyxSequence<Integer>;
  LCursor: INyxEnumerator<Integer>;
  LSecond: INyxEnumerator<Integer>;
  LRefused: Boolean;
begin
  LLeft := TProbe.Create;
  LRight := TProbe.Create;
  try
    LSource := TProbeSequence.Create(LLeft, [3, 1, 3, 2]);
    LOther := TProbeSequence.Create(LRight, [1, 4]);
    LQuery := LSource.Union(LOther, NewNyxIntegerEquality);
    LCursor := LQuery.GetEnumerator;
    LSecond := LQuery.GetEnumerator;
    Check((LLeft.Starts = 0) and (LRight.Starts = 0), 'construction/cursor acquisition enters neither source');
    Check(LQuery.Take(0).Count = 0, 'zero take avoids source/index preparation');
    Check((LLeft.Starts = 0) and (LRight.Starts = 0), 'zero take has no hidden cursor acquisition');
    Check(LCursor.MoveNext and (LCursor.Current = 3) and (LRight.Starts = 0), 'Union first value avoids right source');
    Check(LSecond.MoveNext and (LSecond.Current = 3) and (LLeft.Starts = 2), 'independent cursors own separate indexes');
    LQuery := nil;
    LSource := nil;
    LOther := nil;
    LSecond := nil;
    Check(LCursor.MoveNext and (LCursor.Current = 1), 'cursor survives recipe and caller source release');
    Check(LCursor.MoveNext and (LCursor.Current = 2), 'streaming union skips duplicate left values');
    Check(LCursor.MoveNext and (LCursor.Current = 4) and (LRight.Starts = 1), 'right is acquired only after left EOF');
    Check(not LCursor.MoveNext and not LCursor.MoveNext, 'union EOF remains terminal');
    Check((LLeft.Starts = LLeft.Ends) and (LRight.Starts = LRight.Ends), 'EOF promptly retires both source cursors');
    LCursor := nil;
    LRight.FailAt := 1;
    LSource := TNyxSequence<Integer>.From([1, 2]);
    LOther := TProbeSequence.Create(LRight, [1, 4]);
    LCursor := LSource.Intersect(LOther, NewNyxIntegerEquality).GetEnumerator;
    LRefused := False;
    try
      LCursor.MoveNext;
    except
      on LException: ESetCallback do
      begin
        LRefused := Pos('source failed', LException.Message) > 0;
      end;
    end;
    Check(LRefused and not LCursor.MoveNext, 'membership preparation failure is original/terminal with no yielded value');
    Check(LRight.Starts = LRight.Ends, 'failed other-input preparation retires its cursor');
    LCursor := nil;
    LQuery := TNyxSequence<Integer>.Empty.ExceptWith(LOther, NewNyxIntegerEquality);
    Check(not LQuery.Any and (LRight.Starts = LRight.Ends), 'empty left skips the otherwise failing key input');
    LRight.NoCursor := True;
    LCursor := LSource.Union(TProbeSequence.Create(LRight, []), NewNyxIntegerEquality).GetEnumerator;
    Check(LCursor.MoveNext and LCursor.MoveNext, 'union yields available left values before an invalid right cursor');
    LRefused := False;
    try
      LCursor.MoveNext;
    except
      on ENyxQuery do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LCursor.MoveNext, 'invalid right cursor refuses and stays terminal');
  finally
    LCursor := nil;
    LSecond := nil;
    LQuery := nil;
    LSource := nil;
    LOther := nil;
    LLeft.Free;
    LRight.Free;
  end;
end;

procedure CollisionGrowthAndFailures;
var
  LProbe: TEqualityProbe;
  LEquality: INyxEquality<Integer>;
  LSource: INyxSequence<Integer>;
  LCursor: INyxEnumerator<Integer>;
  LValues: TNyxSequenceArray<Integer>;
  LIndex: Integer;
  LRefused: Boolean;
begin
  LProbe := TEqualityProbe.Create;
  LProbe.Collision := True;
  LEquality := LProbe;
  LSource := TNyxSequence<Integer>.From([3, 1, 3, 2, 1]);
  Rows(LSource.Distinct(LEquality), [3, 1, 2], 'constant hash collisions do not equate distinct values');
  Rows(LSource.Intersect(TNyxSequence<Integer>.From([2, 3]), LEquality),
    [3, 2], 'consumed colliding intersection keys preserve later probe chains');
  Check(LProbe.Comparisons > 0, 'collisions enter typed equality');
  LProbe.Collision := False;
  LProbe.Hashes := 0;
  LProbe.Comparisons := 0;
  SetLength(LValues, 8192);
  for LIndex := 0 to High(LValues) do
  begin
    LValues[LIndex] := LIndex mod 4096;
  end;
  LValues := TNyxSequence<Integer>.From(LValues).Distinct(LEquality).ToArray;
  Check((Length(LValues) = 4096) and (LProbe.Hashes = 8192) and (LProbe.Comparisons = 4096),
    'large set admission caches growth hashes instead of rescanning all keys');
  for LIndex := 0 to High(LValues) do
  begin
    Check(LValues[LIndex] = LIndex, 'complete large distinct encounter order');
  end;
  LCursor := TNyxSetBy<Integer, Integer>.DistinctBy(TNyxSequence<Integer>.From([3, 9, 4]),
    TNyxSelector<Integer, Integer>.From(FailingKey), NewNyxIntegerEquality).GetEnumerator;
  Check(LCursor.MoveNext and (LCursor.Current = 3), 'streaming keys yield prior valid rows');
  LRefused := False;
  try
    LCursor.MoveNext;
  except
    on LException: ESetCallback do
    begin
      LRefused := Pos('key failed', LException.Message) > 0;
    end;
  end;
  Check(LRefused and not LCursor.MoveNext, 'key failure preserves original exception and terminal cursor');
  LProbe.FailHash := True;
  LCursor := LSource.Distinct(LEquality).GetEnumerator;
  LRefused := False;
  try
    LCursor.MoveNext;
  except
    on ESetCallback do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LCursor.MoveNext, 'hash failure emits no current value and stays terminal');
  LProbe.FailHash := False;
  LProbe.FailEqual := True;
  LCursor := TNyxSequence<Integer>.From([3, 3]).Distinct(LEquality).GetEnumerator;
  Check(LCursor.MoveNext, 'first unique key requires no equality callback');
  LRefused := False;
  try
    LCursor.MoveNext;
  except
    on ESetCallback do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LCursor.MoveNext, 'collision equality failure is original and terminal');
end;

procedure Membership;
var
  LProbe: TProbe;
  LSource: INyxSequence<Integer>;
  LOther: INyxSequence<Integer>;
  LEquality: INyxEquality<Integer>;
  LComparer: TEqualityProbe;
  LRefused: Boolean;
begin
  LProbe := TProbe.Create;
  try
    LSource := TProbeSequence.Create(LProbe, [1, 2, 3]);
    LComparer := TEqualityProbe.Create;
    LComparer.FailHash := True;
    LEquality := LComparer;
    Check(LSource.Contains(2, LEquality) and (LProbe.Moves = 2) and (LComparer.Hashes = 0),
      'Contains short-circuits by Equal without hashing');
    Check(LProbe.Starts = LProbe.Ends, 'short-circuit membership releases its cursor');
    Check(not LSource.Contains(4, LEquality), 'absent membership scans to ordinary EOF');
    LOther := TNyxSequence<Integer>.From([1, 2, 3]);
    Check(LSource.SequenceEqual(LOther, LEquality), 'sequence equality preserves multiplicity and order');
    Check(not LSource.SequenceEqual(TNyxSequence<Integer>.From([1, 3, 2]), LEquality),
      'reordered equal members are not an equal sequence');
    Check(not LSource.SequenceEqual(TNyxSequence<Integer>.From([1, 2]), LEquality),
      'shorter right sequence is unequal');
    LOther := TNyxSequence<Integer>.From([1, 2]);
    Check(not LOther.SequenceEqual(LSource, LEquality),
      'longer right sequence is unequal');
    Check(TNyxSequence<Integer>.Empty.SequenceEqual(TNyxSequence<Integer>.Empty, LEquality),
      'two empty sequences are equal');
    LProbe.FailAt := 1;
    Check(not LSource.SequenceEqual(TNyxSequence<Integer>.From([8, 2, 3]), LEquality),
      'first mismatch avoids a later failing source row');
    Check(LSource.Contains(1, LEquality), 'first membership match avoids a later failing source row');
    LRefused := False;
    try
      LSource.SequenceEqual(nil, LEquality);
    except
      on ENyxQuery do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'nil comparison input refuses');
    LProbe.NoCursor := True;
    LRefused := False;
    try
      LSource.Contains(1, LEquality);
    except
      on ENyxQuery do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'nil membership cursor refuses');
  finally
    LSource := nil;
    LOther := nil;
    LEquality := nil;
    LProbe.Free;
  end;
end;

procedure CallbackScopes;
var
  LLease: INyxCallbackLease;
  LVariable: TNyxSelectorNested<Integer, Integer>;
  LSource: INyxSequence<Integer>;
  LQuery: INyxSequence<Integer>;
  LKey: INyxSelector<Integer, Integer>;
  LOffset: Integer;
  LRefused: Boolean;

  function NestedKey(const AValue: Integer): Integer;
  begin
    Result := (AValue mod 2) + LOffset;
  end;

begin
  LOffset := 10;
  LSource := TNyxSequence<Integer>.From([7, 3, 8, 2]);
  LLease := NewNyxCallbackLease;
  try
    LVariable := NestedKey;
    LKey := TNyxSelector<Integer, Integer>.FromNested(LVariable, LLease);
    LQuery := TNyxSetBy<Integer, Integer>.DistinctBy(LSource, LKey, NewNyxIntegerEquality);
    Rows(LQuery, [7, 8], 'named nested key variables preserve scalar scope');
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    LQuery := TNyxSetBy<Integer, Integer>.IntersectBy(LSource,
      TNyxSequence<Integer>.From([11]),
      TNyxSelector<Integer, Integer>.FromInline(
        function(const AValue: Integer): Integer
        begin
          Result := (AValue mod 2) + LOffset;
        end), NewNyxIntegerEquality);
    Rows(LQuery, [7], 'qualified inline key callback preserves scalar capture');
    {$endif}
    LQuery := TNyxSetBy<Integer, Integer>.DistinctBy(LSource, LKey, NewNyxIntegerEquality);
  finally
    LLease.Close;
  end;
  LRefused := False;
  try
    LQuery.Any;
  except
    on ENyxCallbackExpired do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'closed nested key lease refuses deferred frame entry');
end;

procedure ManagedValues;
var
  LLifetime: TLifetime;
  LFirst: IEntryValue;
  LSecond: IEntryValue;
  LRows: TNyxSequenceArray<IEntryValue>;
  LSource: INyxSequence<IEntryValue>;
  LDistinct: INyxSequence<IEntryValue>;
  LQuery: INyxSequence<IEntryValue>;
  LValues: TNyxSequenceArray<IEntryValue>;
  LRead: IEntryValue;
  LEquality: INyxEquality<IEntryValue>;
begin
  LLifetime := TLifetime.Create;
  try
    LFirst := TEntryValue.Create(3, LLifetime);
    LSecond := TEntryValue.Create(4, LLifetime);
    SetLength(LRows, 3);
    LRows[0] := LFirst;
    LRows[1] := LFirst;
    LRows[2] := LSecond;
    LSource := TNyxSequence<IEntryValue>.From(LRows);
    LRows := nil;
    LEquality := TManagedEquality.Create;
    { Keep the stages explicit: assigning a new generic interface expression
      over its own input can retain a caller temporary until routine exit. }
    LDistinct := LSource.Distinct(LEquality);
    LQuery := TNyxSetBy<IEntryValue, Integer>.DistinctBy(LDistinct,
      TNyxSelector<IEntryValue, Integer>.From(ManagedKey), NewNyxIntegerEquality);
    LDistinct := nil;
    LValues := LQuery.ToArray;
    LFirst := nil;
    LSecond := nil;
    LSource := nil;
    LDistinct := nil;
    LQuery := nil;
    Check((Length(LValues) = 2) and (LLifetime.Destroyed = 0),
      'detached distinct results retain managed payloads after source/recipe release');
    LRead := LValues[1];
    LValues := nil;
    Check((LLifetime.Destroyed = 1) and (LRead.GetKey = 4),
      'independent managed reader retains only its own result');
    LRead := nil;
    Check(LLifetime.Destroyed = 2, 'last managed set reader retires its payload once');
  finally
    LRows := nil;
    LValues := nil;
    LRead := nil;
    LSource := nil;
    LDistinct := nil;
    LQuery := nil;
    LFirst := nil;
    LSecond := nil;
    LLifetime.Free;
  end;
end;

function RunNyxSetChecks: Integer;
begin
  GChecks := 0;
  OrdinaryAndByKeys;
  DeferredAndRetirement;
  CollisionGrowthAndFailures;
  Membership;
  CallbackScopes;
  ManagedValues;
  Result := GChecks;
end;

end.
