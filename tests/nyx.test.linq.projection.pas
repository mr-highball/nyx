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
unit nyx.test.linq.projection;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

interface

function RunNyxProjectionChecks: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.linq, nyx.linq.projection, nyx.callback.scope;

type
  TIndexedInteger = INyxIndexedValue<Integer>;
  TIntegerPair = INyxPairValue<Integer, Integer>;
  TInnerSequence = INyxSequence<Integer>;
  TTextInnerSequence = INyxSequence<TNyxText>;
  TTextPair = INyxPairValue<TNyxText, TNyxText>;
  TObjectPair = INyxPairValue<IInterface, Integer>;
  { Separate test ownership observes retirement without retaining the observed
    cursors/sequences or introducing native pointer bridges on the browser. }
  TLifetime = class
    Sequences: Integer;
    Cursors: Integer;
    Calls: Integer;
    Objects: Integer;
  end;
  TCountedObject = class(TInterfacedObject)
  private
    FCounter: TLifetime;
  public
    constructor Create(ACounter: TLifetime);
    destructor Destroy; override;
  end;
  TInnerCursor = class(TEnumerator<Integer>)
  private
    FCounter: TLifetime;
    FValue: Integer;
    FUsed: Boolean;
    FFail: Boolean;
  protected
    function Advance(out AValue: Integer): Boolean; override;
  public
    constructor Create(ACounter: TLifetime; AValue: Integer; AFail: Boolean);
    destructor Destroy; override;
  end;
  TInnerOwned = class(TNyxSequence<Integer>)
  private
    FCounter: TLifetime;
    FValue: Integer;
    FFail: Boolean;
    FNilCursor: Boolean;
  public
    constructor Create(ACounter: TLifetime; AValue: Integer; AFail, ANilCursor: Boolean);
    destructor Destroy; override;
    function GetEnumerator: INyxEnumerator<Integer>; override;
  end;
  TOwnedInnerSelector = class(TInterfacedObject, INyxSelector<Integer, TInnerSequence>)
  private
    FCounter: TLifetime;
    FFail: Boolean;
    FNilCursor: Boolean;
  public
    constructor Create(ACounter: TLifetime; AFail, ANilCursor: Boolean);
    function Project(const AValue: Integer): TInnerSequence;
  end;

var
  GChecks: Integer;
  GCalls: Integer;
  GRetainedIndex: INyxIndexedValue<Integer>;
  GRetainedPair: TObjectPair;
  GKeepPair: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxQuery.Create('Projection qualification: ' + AReason);
  end;
  Inc(GChecks);
end;

function EvenOrdinal(const AValue: TIndexedInteger): Boolean;
begin
  Inc(GCalls);
  Result := (AValue.Index mod 2) = 0;
end;

function Prefix(const AValue: TIndexedInteger): Boolean;
begin
  Inc(GCalls);
  Result := AValue.Index < 2;
end;

function CombineIndex(const AValue: TIndexedInteger): Integer;
begin
  Result := AValue.Value + AValue.Index * 100;
end;

function RetainFirstIndex(const AValue: TIndexedInteger): Integer;
begin

  if AValue.Index = 0 then
  begin
    GRetainedIndex := AValue;
  end;
  Result := AValue.Value;
end;

function IndexText(const AValue: TIndexedInteger): TNyxText;
begin
  Result := TNyxText(IntToStr(AValue.Index)) + TNyxText(': ') +
    TNyxText(IntToStr(AValue.Value)) + TNyxText(' / 🌙') + TNyxText(#0);
end;

function Inner(const AValue: Integer): TInnerSequence;
begin
  Inc(GCalls);

  if AValue = 0 then
  begin
    Exit(TNyxSequence<Integer>.Empty);
  end;
  Result := TNyxSequence<Integer>.From([AValue, AValue + 10]);
end;

function IndexedInner(const AValue: TIndexedInteger): TInnerSequence;
begin
  Inc(GCalls);

  if AValue.Value = 0 then
  begin
    Exit(TNyxSequence<Integer>.Empty);
  end;
  Result := TNyxSequence<Integer>.From([AValue.Value + AValue.Index * 100]);
end;

function PairText(const AValue: TIntegerPair): TNyxText;
begin
  Result := TNyxText(IntToStr(AValue.Outer)) + TNyxText('/') +
    TNyxText(IntToStr(AValue.Inner)) + TNyxText(' / 🌙');
end;

function PairFail(const AValue: TIntegerPair): TNyxText;
begin

  if AValue.Inner > 10 then
  begin
    raise ENyxQuery.Create('Pair failed / 🌙');
  end;
  Result := PairText(AValue);
end;

function NilInner(const AValue: Integer): TInnerSequence;
begin
  Inc(GCalls);
  Result := nil;
end;

function TextInner(const AValue: TNyxText): TTextInnerSequence;
begin
  Result := TNyxSequence<TNyxText>.From([AValue + TNyxText('!'), AValue + TNyxText(#0)]);
end;

function TextPair(const AValue: TTextPair): TNyxText;
begin
  Result := AValue.Outer + TNyxText('/') + AValue.Inner;
end;

function ObjectInner(const AValue: IInterface): TInnerSequence;
begin
  Check(AValue <> nil, 'managed outer value enters inner projection');
  Result := TNyxSequence<Integer>.From([7]);
end;

function ObjectPair(const AValue: TObjectPair): TNyxText;
begin
  Check(AValue.Outer <> nil, 'managed outer context remains alive during result projection');

  if GKeepPair then
  begin
    GRetainedPair := AValue;
  end;
  Result := TNyxText('Owned ') + TNyxText(IntToStr(AValue.Inner));
end;

constructor TCountedObject.Create(ACounter: TLifetime);
begin
  inherited Create;
  FCounter := ACounter;
end;

destructor TCountedObject.Destroy;
begin
  Inc(FCounter.Objects);
  inherited Destroy;
end;

constructor TInnerCursor.Create(ACounter: TLifetime; AValue: Integer; AFail: Boolean);
begin
  inherited Create;
  FCounter := ACounter;
  FValue := AValue;
  FFail := AFail;
end;

destructor TInnerCursor.Destroy;
begin
  Inc(FCounter.Cursors);
  inherited Destroy;
end;

function TInnerCursor.Advance(out AValue: Integer): Boolean;
begin

  if FFail then
  begin
    raise ENyxQuery.Create('Inner failed / 🌙');
  end;
  Result := not FUsed;
  FUsed := True;
  AValue := FValue;
end;

constructor TInnerOwned.Create(ACounter: TLifetime; AValue: Integer; AFail, ANilCursor: Boolean);
begin
  inherited Create;
  FCounter := ACounter;
  FValue := AValue;
  FFail := AFail;
  FNilCursor := ANilCursor;
end;

destructor TInnerOwned.Destroy;
begin
  Inc(FCounter.Sequences);
  inherited Destroy;
end;

function TInnerOwned.GetEnumerator: INyxEnumerator<Integer>;
begin

  if FNilCursor then
  begin
    Exit(nil);
  end;
  Result := TInnerCursor.Create(FCounter, FValue, FFail);
end;

constructor TOwnedInnerSelector.Create(ACounter: TLifetime; AFail, ANilCursor: Boolean);
begin
  inherited Create;
  FCounter := ACounter;
  FFail := AFail;
  FNilCursor := ANilCursor;
end;

function TOwnedInnerSelector.Project(const AValue: Integer): TInnerSequence;
begin
  Inc(FCounter.Calls);
  Result := TInnerOwned.Create(FCounter, AValue, FFail, FNilCursor);
end;

procedure Indexed;
var
  LSource: INyxSequence<Integer>;
  LQuery: INyxSequence<Integer>;
  LValues: TNyxSequenceArray<Integer>;
  LTexts: TNyxSequenceArray<TNyxText>;
  LCursor: INyxEnumerator<Integer>;
  LOther: INyxEnumerator<Integer>;
  LIndex: Integer;
  LExhausted: Boolean;
  LRefused: Boolean;
begin
  LSource := TNyxSequence<Integer>.From([10, 20, 30, 40, 50]);
  GCalls := 0;
  LQuery := LSource.WhereIndexed(TNyxIndexedPredicate<Integer>.From(EvenOrdinal))
    .SelectIndexed(TNyxIndexedSelector<Integer, Integer>.From(CombineIndex));
  LCursor := LQuery.GetEnumerator;
  LOther := LQuery.GetEnumerator;
  Check(GCalls = 0, 'indexed construction/cursor acquisition is lazy');
  LQuery := nil;
  LSource := nil;
  Check(LCursor.MoveNext and (LCursor.Current = 10), 'first indexed value');
  Check(LCursor.MoveNext and (LCursor.Current = 130), 'projection ordinal follows filtered input');
  Check(LOther.MoveNext and (LOther.Current = 10), 'retained cursor owns independent ordinals/source');
  Check(LCursor.MoveNext and (LCursor.Current = 250) and not LCursor.MoveNext,
    'full indexed output and terminal EOF');
  Check(not LCursor.MoveNext, 'EOF does not reenter retired indexed input');
  LSource := TNyxSequence<Integer>.From([10, 20, 30, 40, 50]);
  LValues := LSource.WhereIndexed(TNyxIndexedPredicate<Integer>.From(EvenOrdinal))
    .WhereIndexed(TNyxIndexedPredicate<Integer>.From(EvenOrdinal)).ToArray;
  Check((Length(LValues) = 2) and (LValues[0] = 10) and (LValues[1] = 50),
    'successive indexed filters use their own input ordinals');
  GCalls := 0;
  LValues := LSource.SkipWhileIndexed(TNyxIndexedPredicate<Integer>.From(Prefix)).ToArray;
  Check((Length(LValues) = 3) and (LValues[0] = 30) and (GCalls = 3), 'skip prefix stops invoking predicate after boundary');
  GCalls := 0;
  LValues := LSource.TakeWhileIndexed(TNyxIndexedPredicate<Integer>.From(Prefix)).ToArray;
  Check((Length(LValues) = 2) and (LValues[1] = 20) and (GCalls = 3), 'take prefix stops at first refusal');
  LTexts := TNyxSelectIndexed<Integer, TNyxText>.From(LSource,
    TNyxIndexedSelector<Integer, TNyxText>.From(IndexText)).ToArray;
  Check((Length(LTexts) = 5) and (LTexts[4] = TNyxText('4: 50 / 🌙') + TNyxText(#0)),
    'indexed type-changing projection preserves supplementary text and NUL');
  LIndex := High(Integer);
  LExhausted := False;
  Check(NextNyxQueryIndex(LIndex, LExhausted) = High(Integer), 'last valid ordinal is admitted');
  LRefused := False;
  try
    NextNyxQueryIndex(LIndex, LExhausted);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'another ordinal refuses checked overflow before callback entry');
  LRefused := False;
  try
    LQuery := LSource.WhereIndexed(nil);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'nil indexed predicate refuses during construction');
  try
    LValues := LSource.SelectIndexed(TNyxIndexedSelector<Integer, Integer>.From(RetainFirstIndex)).ToArray;
    LSource := nil;
    Check((GRetainedIndex.Index = 0) and (GRetainedIndex.Value = 10) and (LValues[4] = 50),
      'retained immutable indexed context survives later rows and source release');
  finally
    GRetainedIndex := nil;
  end;
end;

procedure Flatten;
var
  LSource: INyxSequence<Integer>;
  LQuery: INyxSequence<Integer>;
  LValues: TNyxSequenceArray<Integer>;
  LTexts: TNyxSequenceArray<TNyxText>;
  LCursor: INyxEnumerator<Integer>;
  LOther: INyxEnumerator<Integer>;
  LTextCursor: INyxEnumerator<TNyxText>;
  LRefused: Boolean;
begin
  LSource := TNyxSequence<Integer>.From([0, 1, 0, 2]);
  GCalls := 0;
  LQuery := TNyxSelectMany<Integer, Integer>.From(LSource,
    TNyxSelector<Integer, TInnerSequence>.From(Inner));
  LCursor := LQuery.GetEnumerator;
  LOther := LQuery.GetEnumerator;
  Check(GCalls = 0, 'flatten construction/cursor acquisition enters no inner callback');
  Check(LQuery.Take(0).Count = 0, 'zero take consumes neither outer nor inner');
  Check(GCalls = 0, 'zero take remains fully lazy');
  LQuery := nil;
  LSource := nil;
  Check(LCursor.MoveNext and (LCursor.Current = 1), 'empty leading inner is skipped');
  Check(LCursor.MoveNext and (LCursor.Current = 11), 'inner order retained');
  Check(LOther.MoveNext and (LOther.Current = 1), 'flatten cursor remains independent after source/recipe release');
  Check(LCursor.MoveNext and (LCursor.Current = 2), 'empty middle inner is skipped');
  Check(LCursor.MoveNext and (LCursor.Current = 12) and not LCursor.MoveNext, 'complete outer/inner order');
  LSource := TNyxSequence<Integer>.From([0, 1, 0, 2]);
  LValues := TNyxSelectMany<Integer, Integer>.FromIndexed(LSource,
    TNyxIndexedSelector<Integer, TInnerSequence>.From(IndexedInner)).ToArray;
  Check((Length(LValues) = 2) and (LValues[0] = 101) and (LValues[1] = 302),
    'indexed flatten ordinal includes outer rows with empty inner sequences');
  LTexts := TNyxSelectManyResult<Integer, Integer, TNyxText>.From(LSource,
    TNyxSelector<Integer, TInnerSequence>.From(Inner),
    TNyxSelector<TIntegerPair, TNyxText>.From(PairText)).ToArray;
  Check((Length(LTexts) = 4) and (LTexts[0] = TNyxText('1/1 / 🌙'))
    and (LTexts[3] = TNyxText('2/12 / 🌙')), 'result selector retains the correct outer context');
  LTexts := TNyxSelectManyResult<Integer, Integer, TNyxText>.FromIndexed(LSource,
    TNyxIndexedSelector<Integer, TInnerSequence>.From(IndexedInner),
    TNyxSelector<TIntegerPair, TNyxText>.From(PairText)).ToArray;
  Check((Length(LTexts) = 2) and (LTexts[1] = TNyxText('2/302 / 🌙')), 'indexed collection/result mapping combines typed values');
  LTextCursor := TNyxSelectManyResult<Integer, Integer, TNyxText>.From(LSource,
    TNyxSelector<Integer, TInnerSequence>.From(Inner),
    TNyxSelector<TIntegerPair, TNyxText>.From(PairFail)).GetEnumerator;
  Check(LTextCursor.MoveNext, 'result failure consumer yields preceding valid pair');
  LRefused := False;
  try
    LTextCursor.MoveNext;
  except
    on LException: ENyxQuery do
    begin
      LRefused := Pos('Pair failed', LException.Message) > 0;
    end;
  end;
  Check(LRefused and not LTextCursor.MoveNext, 'result exception is original and cursor failure terminal');
  LRefused := False;
  try
    LQuery := TNyxSelectMany<Integer, Integer>.From(LSource,
      TNyxSelector<Integer, TInnerSequence>.From(NilInner));
    LQuery.ToArray;
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'nil inner sequence refuses instead of silently disappearing');
end;

procedure Retirement;
var
  LCounter: TLifetime;
  LQuery: INyxSequence<Integer>;
  LCursor: INyxEnumerator<Integer>;
  LRefused: Boolean;
begin
  LCounter := TLifetime.Create;
  try
    LQuery := TNyxSelectMany<Integer, Integer>.From(TNyxSequence<Integer>.From([1, 2]),
      TOwnedInnerSelector.Create(LCounter, False, False));
    LCursor := LQuery.GetEnumerator;
    LQuery := nil;
    Check(LCursor.MoveNext and (LCounter.Sequences = 1), 'inner sequence can retire while its independent cursor stays valid');
    Check(LCursor.MoveNext and (LCounter.Cursors = 1), 'prior inner cursor retires before next outer callback');
    Check(not LCursor.MoveNext and (LCounter.Cursors = 2), 'all inner cursors retire at flatten EOF');
    LCursor := nil;
    LQuery := TNyxSelectMany<Integer, Integer>.From(TNyxSequence<Integer>.From([1, 2]),
      TOwnedInnerSelector.Create(LCounter, False, False));
    LCursor := LQuery.GetEnumerator;
    Check(LCursor.MoveNext, 'partial flatten consumer enters one inner');
    LCursor := nil;
    Check(LCounter.Cursors = 3, 'early cursor disposal releases current inner without visiting later outer');
    LQuery := nil;
    LQuery := TNyxSelectMany<Integer, Integer>.From(TNyxSequence<Integer>.From([1]),
      TOwnedInnerSelector.Create(LCounter, True, False));
    LCursor := LQuery.GetEnumerator;
    LRefused := False;
    try
      LCursor.MoveNext;
    except
      on LException: ENyxQuery do
      begin
        LRefused := Pos('Inner failed', LException.Message) > 0;
      end;
    end;
    Check(LRefused and (LCounter.Cursors = 4), 'inner failure releases the active inner before returning');
    Check(not LCursor.MoveNext, 'inner failure never reenters disposed cursor');
    LCursor := nil;
    LQuery := TNyxSelectMany<Integer, Integer>.From(TNyxSequence<Integer>.From([1]),
      TOwnedInnerSelector.Create(LCounter, False, True));
    LRefused := False;
    try
      LQuery.ToArray;
    except
      on ENyxQuery do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'nil inner cursor is explicit contract failure');
  finally
    LCursor := nil;
    LQuery := nil;
    LCounter.Free;
  end;
end;

procedure ScopedCallbacks;
var
  LLease: INyxCallbackLease;
  LSource: INyxSequence<Integer>;
  LQuery: INyxSequence<Integer>;
  LSelector: INyxIndexedSelector<Integer, Integer>;
  LVariable: TNyxSelectorNested<TIndexedInteger, Integer>;
  LThreshold: Integer;
  LRefused: Boolean;

  function NestedIndex(const AValue: TIndexedInteger): Integer;
  begin
    Result := AValue.Value + AValue.Index + LThreshold;
  end;

begin
  LThreshold := 5;
  LLease := NewNyxCallbackLease;
  try
    LVariable := NestedIndex;
    LSelector := TNyxIndexedSelector<Integer, Integer>.Wrap(
      TNyxSelector<TIndexedInteger, Integer>.FromNested(LVariable, LLease));
    LSource := TNyxSequence<Integer>.From([1, 2]);
    LQuery := LSource.SelectIndexed(LSelector);
    Check(LQuery.Last = 8, 'named nested indexed callback preserves its typed scope');
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    LQuery := LSource.SelectIndexed(TNyxIndexedSelector<Integer, Integer>.Wrap(
      TNyxSelector<TIndexedInteger, Integer>.FromInline(
        function(const AValue: TIndexedInteger): Integer
        begin
          Result := AValue.Value + AValue.Index + LThreshold;
        end)));
    Check(LQuery.Last = 8, 'qualified inline indexed callback preserves scalar capture');
    {$endif}
    LQuery := LSource.SelectIndexed(LSelector);
  finally
    LLease.Close;
  end;
  LRefused := False;
  try
    LQuery.ToArray;
  except
    on ENyxCallbackExpired do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'closed indexed callback lease refuses deferred entry');
end;

procedure ManagedAndText;
var
  LCounter: TLifetime;
  LOwner: IInterface;
  LReadOwner: IInterface;
  LInput: TNyxSequenceArray<IInterface>;
  LSource: INyxSequence<IInterface>;
  LQuery: INyxSequence<TNyxText>;
  LCursor: INyxEnumerator<TNyxText>;
  LTexts: TNyxSequenceArray<TNyxText>;
  LTextSource: INyxSequence<TNyxText>;
begin
  LCounter := TLifetime.Create;
  try
    LOwner := TCountedObject.Create(LCounter);
    { pas2js retains a managed array-literal temporary until routine exit. Use
      an explicit caller vector, then release it before asserting last ownership. }
    SetLength(LInput, 1);
    LInput[0] := LOwner;
    LSource := TNyxSequence<IInterface>.From(LInput);
    LInput := nil;
    LQuery := TNyxSelectManyResult<IInterface, Integer, TNyxText>.From(LSource,
      TNyxSelector<IInterface, TInnerSequence>.From(ObjectInner),
      TNyxSelector<TObjectPair, TNyxText>.From(ObjectPair));
    LCursor := LQuery.GetEnumerator;
    LOwner := nil;
    LSource := nil;
    LQuery := nil;
    Check(LCounter.Objects = 0, 'flatten cursor retains managed outer after caller/source release');
    Check(LCursor.MoveNext and (LCursor.Current = 'Owned 7'), 'managed pair projects specialized value');
    Check(not LCursor.MoveNext and (LCounter.Objects = 1), 'managed outer retires once at flatten EOF');
    LCursor := nil;
    Check(LCounter.Objects = 1, 'later cursor release cannot destroy the outer twice');
    GKeepPair := True;
    LOwner := TCountedObject.Create(LCounter);
    SetLength(LInput, 1);
    LInput[0] := LOwner;
    LSource := TNyxSequence<IInterface>.From(LInput);
    LInput := nil;
    LQuery := TNyxSelectManyResult<IInterface, Integer, TNyxText>.From(LSource,
      TNyxSelector<IInterface, TInnerSequence>.From(ObjectInner),
      TNyxSelector<TObjectPair, TNyxText>.From(ObjectPair));
    LTexts := LQuery.ToArray;
    LOwner := nil;
    LInput := nil;
    LSource := nil;
    LQuery := nil;
    LReadOwner := GRetainedPair.Outer;
    Check((LCounter.Objects = 1) and (LReadOwner <> nil) and (GRetainedPair.Inner = 7),
      'retained immutable pair context owns managed values after source/recipe retirement');
    LReadOwner := nil;
    GRetainedPair := nil;
    GKeepPair := False;
    Check(LCounter.Objects = 2, 'last pair-context release retires the managed outer exactly once');
    LTextSource := TNyxSequence<TNyxText>.From([TNyxText('🌙'), TNyxText('A') + TNyxText(#0)]);
    LTexts := TNyxSelectManyResult<TNyxText, TNyxText, TNyxText>.From(LTextSource,
      TNyxSelector<TNyxText, TTextInnerSequence>.From(TextInner),
      TNyxSelector<TTextPair, TNyxText>.From(TextPair)).ToArray;
    Check((Length(LTexts) = 4) and (LTexts[1] = TNyxText('🌙/🌙') + TNyxText(#0)),
      'outer/inner text contexts preserve supplementary scalars and NUL');
    Check(LTexts[3] = TNyxText('A') + TNyxText(#0) + TNyxText('/A') + TNyxText(#0) + TNyxText(#0),
      'embedded NUL values are never shortened by flatten/result mapping');
  finally
    GRetainedPair := nil;
    GKeepPair := False;
    LCursor := nil;
    LQuery := nil;
    LSource := nil;
    LOwner := nil;
    LReadOwner := nil;
    LCounter.Free;
  end;
end;

function RunNyxProjectionChecks: Integer;
begin
  GChecks := 0;
  Indexed;
  Flatten;
  Retirement;
  ScopedCallbacks;
  ManagedAndText;
  Result := GChecks;
end;

end.
