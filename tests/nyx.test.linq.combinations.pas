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
unit nyx.test.linq.combinations;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

interface

function RunNyxCombinationChecks: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.linq, nyx.linq.projection, nyx.linq.combinations,
  nyx.linq.runtime, nyx.linq.numeric, nyx.linq.positions, nyx.linq.random,
  nyx.callback.scope;

type
  TIntPair = INyxPairValue<Integer, Integer>;
  TIntTriple = INyxTripleValue<Integer, Integer, Integer>;
  TIntIndex = INyxIndexedValue<Integer>;
  TIntOptional = INyxOptional<Integer>;
  ECombinationFailure = class(Exception);
  TMetrics = class
    Starts: Integer;
    Moves: Integer;
    Ends: Integer;
    Count: Integer;
    FailAt: Integer;
    NilCursor: Boolean;
  end;
  TSourceCursor = class(TEnumerator<Integer>)
  private
    FMetrics: TMetrics;
    FIndex: Integer;
  protected
    function Advance(out AValue: Integer): Boolean; override;
  public
    constructor Create(AMetrics: TMetrics);
    destructor Destroy; override;
  end;
  TSource = class(TNyxSequence<Integer>)
  private
    FMetrics: TMetrics;
  public
    constructor Create(AMetrics: TMetrics);
    function GetEnumerator: INyxEnumerator<Integer>; override;
  end;
  TBase = class
    Number: Integer;
  end;
  TChild = class(TBase);
  IItem = interface(IInterface)
    ['{409F712B-3B69-4BBA-BC17-CEAA7D6078F1}']
    function GetNumber: Integer;
    property Number: Integer read GetNumber;
  end;
  INoGuid = interface(IInterface)
    function GetNumber: Integer;
  end;
  TLifetime = class
    Destroyed: Integer;
  end;
  TItem = class(TInterfacedObject, IItem)
  private
    FLifetime: TLifetime;
  public
    constructor Create(ALifetime: TLifetime);
    destructor Destroy; override;
    function GetNumber: Integer;
  end;
  TOther = class(TInterfacedObject);
  TRandomMode = (rmIdentity, rmNegative, rmUpper, rmFailure, rmNil);
  TRandomMetrics = class
    Factories: Integer;
    Calls: Integer;
    Mode: TRandomMode;
  end;
  TTestRandom = class(TInterfacedObject, INyxRandom)
  private
    FMetrics: TRandomMetrics;
  public
    constructor Create(AMetrics: TRandomMetrics);
    function NextBelow(AExclusive: Integer): Integer;
  end;
  TTestRandomFactory = class(TInterfacedObject, INyxRandomFactory)
  private
    FMetrics: TRandomMetrics;
  public
    constructor Create(AMetrics: TRandomMetrics);
    function NewRandom: INyxRandom;
  end;

var
  GChecks: Integer;
  GCalls: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Combination: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TSourceCursor.Create(AMetrics: TMetrics);
begin
  inherited Create;
  FMetrics := AMetrics;
  Inc(FMetrics.Starts);
end;

destructor TSourceCursor.Destroy;
begin
  Inc(FMetrics.Ends);
  inherited Destroy;
end;

function TSourceCursor.Advance(out AValue: Integer): Boolean;
begin
  Inc(FMetrics.Moves);

  if FIndex = FMetrics.FailAt then
  begin
    raise ECombinationFailure.Create('original source failure');
  end;
  Result := FIndex < FMetrics.Count;

  if Result then
  begin
    AValue := FIndex;
    Inc(FIndex);
  end;
end;

constructor TSource.Create(AMetrics: TMetrics);
begin
  inherited Create;
  FMetrics := AMetrics;
end;

function TSource.GetEnumerator: INyxEnumerator<Integer>;
begin

  if FMetrics.NilCursor then
  begin
    Result := nil;
  end
  else
  begin
    Result := TSourceCursor.Create(FMetrics);
  end;
end;

function SumPair(const AValue: TIntPair): Integer;
begin
  Inc(GCalls);
  Result := AValue.Outer + AValue.Inner;
end;

function SumTriple(const AValue: TIntTriple): Integer;
begin
  Result := AValue.First + AValue.Second + AValue.Third;
end;

function FailPair(const AValue: TIntPair): Integer;
begin
  Inc(GCalls);
  Result := 0;
  raise ECombinationFailure.Create('original selector failure');
end;

procedure CallbackComposition;
var
  LLease: INyxCallbackLease;
  LOffset: Integer;
  LNested: INyxSequence<Integer>;
  LTyped: INyxSequence<Integer>;
  LInline: INyxSequence<Integer>;
  LRejected: Boolean;

  function Sum(const AValue: TIntPair): Integer;
  begin
    Result := AValue.Outer + AValue.Inner + LOffset;
  end;

  function Convert(const AValue: Integer): TIntOptional;
  begin
    Result := TNyxOptional<Integer>.Some(AValue + LOffset);
  end;

begin
  LOffset := 5;
  LLease := NewNyxCallbackLease;
  try
    LNested := TNyxZip<Integer, Integer, Integer>.From(NyxRange(1, 1), NyxRange(10, 1),
      TNyxSelector<TIntPair, Integer>.FromNested(Sum, LLease));
    Check(LNested.Single = 16, 'Zip result selector accepts a scoped named nested function');
    LTyped := TNyxTypeQuery<Integer, Integer>.Cast(NyxRange(1, 1),
      TNyxSelector<Integer, TIntOptional>.FromNested(Convert, LLease));
    Check(LTyped.Single = 6, 'typed runtime converter accepts a scoped named nested function');
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    LInline := TNyxZip<Integer, Integer, Integer>.From(NyxRange(1, 1), NyxRange(10, 1),
      TNyxSelector<TIntPair, Integer>.FromInline(function(const AValue: TIntPair): Integer
        begin
          Result := AValue.Outer + AValue.Inner + LOffset;
        end));
    Check(LInline.Single = 16, 'Zip result selector accepts a qualified inline capture');
    {$endif}
  finally
    LLease.Close;
  end;
  LRejected := False;
  try
    LNested.Count;
  except
    on LException: ENyxCallbackExpired do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Zip preserves closed nested callback lease');
  LRejected := False;
  try
    LTyped.Count;
  except
    on LException: ENyxCallbackExpired do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'typed conversion preserves closed nested callback lease');
end;

procedure ZipAndIndex;
var
  LLeftMetrics: TMetrics;
  LRightMetrics: TMetrics;
  LLeft: INyxSequence<Integer>;
  LRight: INyxSequence<Integer>;
  LQuery: INyxSequence<Integer>;
  LCursor: INyxEnumerator<Integer>;
  LOther: INyxEnumerator<Integer>;
  LPairs: INyxSequence<TIntPair>;
  LPair: TIntPair;
  LTriples: INyxSequence<TIntTriple>;
  LTriple: TIntTriple;
  LIndexed: INyxSequence<TIntIndex>;
  LIndexCursor: INyxEnumerator<TIntIndex>;
  LIndex: TIntIndex;
  LRejected: Boolean;
begin
  LLeftMetrics := TMetrics.Create;
  LRightMetrics := TMetrics.Create;
  try
    LLeftMetrics.Count := 3;
    LRightMetrics.Count := 1;
    LLeftMetrics.FailAt := -1;
    LRightMetrics.FailAt := -1;
    LLeft := TSource.Create(LLeftMetrics);
    LRight := TSource.Create(LRightMetrics);
    GCalls := 0;
    LQuery := TNyxZip<Integer, Integer, Integer>.From(LLeft, LRight,
      TNyxSelector<TIntPair, Integer>.From(SumPair));
    LCursor := LQuery.GetEnumerator;
    LOther := LQuery.GetEnumerator;
    Check((LLeftMetrics.Starts = 0) and (LRightMetrics.Starts = 0) and (GCalls = 0),
      'Zip construction and independent cursors enter no input');
    Check(LCursor.MoveNext and (LCursor.Current = 0), 'first corresponding pair projects');
    Check(LOther.MoveNext and (LOther.Current = 0), 'second cursor starts independently');
    Check(not LCursor.MoveNext, 'Zip ends at shortest input');
    Check((LLeftMetrics.Moves = 3) and (LRightMetrics.Moves = 3) and (GCalls = 2),
      'terminal attempt advances earlier input but projects no unmatched row');
    Check((LLeftMetrics.Ends = 1) and (LRightMetrics.Ends = 1),
      'shortest input promptly retires both completed cursors');
    Check(not LCursor.MoveNext, 'terminal Zip cursor never restarts');
    LCursor := nil;
    LOther := nil;
    LQuery := nil;
    LLeftMetrics.Count := 0;
    LQuery := TNyxZip<Integer, Integer, Integer>.From(LLeft, LRight,
      TNyxSelector<TIntPair, Integer>.From(SumPair));
    Check(LQuery.Count = 0, 'empty left input yields no row');
    Check(LRightMetrics.Starts = 2, 'empty left never obtains a later input cursor');
    LLeftMetrics.Count := 3;
    LRightMetrics.Count := 3;
    LPairs := TNyxZipPairs<Integer, Integer>.From(LLeft, LRight);
    LPair := LPairs.First;
    LPairs := nil;
    Check((LPair.Outer = 0) and (LPair.Inner = 0), 'retained pair survives input cursor retirement');
    LTriples := TNyxZipTriples<Integer, Integer, Integer>.From(
      NyxRange(1, 3), NyxRange(10, 2), NyxRange(100, 1));
    LTriple := LTriples.Single;
    LTriples := nil;
    Check((LTriple.First = 1) and (LTriple.Second = 10) and (LTriple.Third = 100),
      'three-way shortest Zip retains specialized triple values');
    LQuery := TNyxZipThree<Integer, Integer, Integer, Integer>.From(
      NyxRange(1, 3), NyxRange(10, 2), NyxRange(100, 1),
      TNyxSelector<TIntTriple, Integer>.From(SumTriple));
    Check(LQuery.Single = 111, 'three-way typed result selector executes');
    LRightMetrics.FailAt := 1;
    LQuery := TNyxZip<Integer, Integer, Integer>.From(LLeft, LRight,
      TNyxSelector<TIntPair, Integer>.From(SumPair));
    LCursor := LQuery.GetEnumerator;
    Check(LCursor.MoveNext, 'Zip can yield before a later source failure');
    LRejected := False;
    try
      LCursor.MoveNext;
    except
      on LException: ECombinationFailure do
      begin
        LRejected := LException.Message = 'original source failure';
      end;
    end;
    Check(LRejected and not LCursor.MoveNext, 'Zip source failure remains original and terminal');
    LRightMetrics.FailAt := -1;
    LQuery := TNyxZip<Integer, Integer, Integer>.From(LLeft, LRight,
      TNyxSelector<TIntPair, Integer>.From(FailPair));
    LCursor := LQuery.GetEnumerator;
    LRejected := False;
    try
      LCursor.MoveNext;
    except
      on LException: ECombinationFailure do
      begin
        LRejected := LException.Message = 'original selector failure';
      end;
    end;
    Check(LRejected and not LCursor.MoveNext, 'Zip selector failure remains original and terminal');
    LRightMetrics.NilCursor := True;
    LQuery := TNyxZip<Integer, Integer, Integer>.From(LLeft, LRight,
      TNyxSelector<TIntPair, Integer>.From(SumPair));
    LRejected := False;
    try
      LQuery.Count;
    except
      on LException: ENyxQuery do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'nil Zip cursor refuses');
    LIndexed := TNyxIndex<Integer>.From(NyxRange(20, 3));
    LIndexCursor := LIndexed.GetEnumerator;
    Check(LIndexCursor.MoveNext, 'Index yields a first context');
    LIndex := LIndexCursor.Current;
    Check((LIndex.Index = 0) and (LIndex.Value = 20), 'Index starts at zero');
    Check(LIndexCursor.MoveNext and (LIndexCursor.Current.Index = 1), 'Index increments once per row');
    LIndexCursor := nil;
    Check((LIndex.Index = 0) and (LIndex.Value = 20), 'retained Index context stays immutable');
    LIndex := LIndexed.Last;
    Check((LIndex.Index = 2) and (LIndex.Value = 22), 'independent Index enumeration restarts');
  finally
    LCursor := nil;
    LOther := nil;
    LQuery := nil;
    LPairs := nil;
    LLeft := nil;
    LRight := nil;
    LLeftMetrics.Free;
    LRightMetrics.Free;
  end;
end;

constructor TItem.Create(ALifetime: TLifetime);
begin
  inherited Create;
  FLifetime := ALifetime;
end;

destructor TItem.Destroy;
begin
  Inc(FLifetime.Destroyed);
  inherited Destroy;
end;

function TItem.GetNumber: Integer;
begin
  Result := 42;
end;

function EvenOptional(const AValue: Integer): TIntOptional;
begin
  Inc(GCalls);

  if (AValue mod 2) = 0 then
  begin
    Result := TNyxOptional<Integer>.Some(AValue);
  end
  else
  begin
    Result := TNyxOptional<Integer>.None;
  end;
end;

procedure PositionsAndSlices;
var
  LMetrics: TMetrics;
  LSource: INyxSequence<Integer>;
  LQuery: INyxSequence<Integer>;
  LCursor: INyxEnumerator<Integer>;
  LOther: INyxEnumerator<Integer>;
  LValues: TNyxSequenceArray<Integer>;
  LText: INyxSequence<TNyxText>;
  LPosition: TNyxPosition;
  LRejected: Boolean;
  LPresent: Boolean;
  LValue: Integer;
begin
  LMetrics := TMetrics.Create;
  try
    LMetrics.Count := 5;
    LMetrics.FailAt := -1;
    LSource := TSource.Create(LMetrics);
    LValue := 99;
    LPresent := TNyxPositions<Integer>.TryElementAt(LSource, TNyxPosition.FromEnd(0), LValue);
    Check(not LPresent and (LValue = 0) and (LMetrics.Starts = 0),
      'exclusive end position is absent without source entry');
    LValue := TNyxPositions<Integer>.ElementAt(LSource, TNyxPosition.FromEnd(1));
    Check((LValue = 4) and (LMetrics.Moves = 6), 'last-position access consumes and retires full input');
    LValue := TNyxPositions<Integer>.ElementAt(LSource, TNyxPosition.FromStart(1));
    Check((LValue = 1) and (LMetrics.Moves = 8), 'start-position access short-circuits');
    LPresent := TNyxPositions<Integer>.TryElementAt(LSource,
      TNyxPosition.FromEnd(High(Integer)), LValue);
    Check(not LPresent and (LValue = 0), 'oversized end distance uses actual rows and clears absence');
    LValue := TNyxPositions<Integer>.ElementAtOrDefault(LSource,
      TNyxPosition.FromEnd(6), 77);
    Check(LValue = 77, 'typed fallback handles absent end-relative element');
    LRejected := False;
    try
      LValue := TNyxPositions<Integer>.ElementAt(LSource, TNyxPosition.FromEnd(0));
    except
      on LException: ENyxQueryEmpty do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'required end boundary refuses absence');
    LRejected := False;
    try
      LPosition := TNyxPosition.FromStart(-1);
    except
      on LException: ENyxQuery do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'negative typed position refuses');
    LMetrics.Moves := 0;
    LMetrics.Starts := 0;
    LMetrics.Ends := 0;
    LQuery := TNyxSlice<Integer>.Between(LSource,
      TNyxPosition.FromStart(1), TNyxPosition.FromStart(3));
    LCursor := LQuery.GetEnumerator;
    LOther := LQuery.GetEnumerator;
    Check(LMetrics.Starts = 0, 'slice construction/cursor creation is deferred');
    Check(LCursor.MoveNext and (LCursor.Current = 1), 'start-only slice skips its prefix');
    Check(LOther.MoveNext and (LOther.Current = 1), 'slice cursors are independent');
    Check(LCursor.MoveNext and (LCursor.Current = 2), 'half-open slice excludes its upper boundary');
    Check(not LCursor.MoveNext and (LMetrics.Moves = 5), 'bounded slice enters no unused tail');
    LCursor := nil;
    LOther := nil;
    LQuery := TNyxSlice<Integer>.Between(LSource,
      TNyxPosition.FromStart(1), TNyxPosition.FromEnd(1));
    LValues := LQuery.ToArray;
    Check((Length(LValues) = 3) and (LValues[0] = 1) and (LValues[2] = 3),
      'mixed boundaries resolve against actual source length');
    LQuery := TNyxSlice<Integer>.Between(LSource,
      TNyxPosition.FromEnd(3), TNyxPosition.FromEnd(0));
    LValues := LQuery.ToArray;
    Check((Length(LValues) = 3) and (LValues[0] = 2) and (LValues[2] = 4),
      'end-relative slice preserves source order');
    LQuery := TNyxSlice<Integer>.Between(LSource,
      TNyxPosition.FromEnd(High(Integer)), TNyxPosition.FromStart(High(Integer)));
    Check(LQuery.Count = 5, 'oversized slice boundaries clamp without preallocating their offsets');
    LMetrics.Starts := 0;
    LQuery := TNyxSlice<Integer>.Between(LSource,
      TNyxPosition.FromEnd(1), TNyxPosition.FromEnd(2));
    Check((LQuery.Count = 0) and (LMetrics.Starts = 0), 'known reversed end interval enters no source');
    LMetrics.FailAt := 3;
    LQuery := TNyxSlice<Integer>.Between(LSource,
      TNyxPosition.FromStart(0), TNyxPosition.FromEnd(0));
    LCursor := LQuery.GetEnumerator;
    LRejected := False;
    try
      LCursor.MoveNext;
    except
      on LException: ECombinationFailure do
      begin
        LRejected := LException.Message = 'original source failure';
      end;
    end;
    Check(LRejected and not LCursor.MoveNext, 'complete-admission slice yields no partial failed data');
    LText := TNyxSequence<TNyxText>.From(['first', 'exact 🌙' + #0 + ' text']);
    Check(TNyxPositions<TNyxText>.ElementAt(LText, TNyxPosition.FromEnd(1)) =
      TNyxText('exact 🌙' + #0 + ' text'), 'position retains exact Unicode and embedded NUL');
  finally
    LSource := nil;
    LQuery := nil;
    LCursor := nil;
    LOther := nil;
    LMetrics.Free;
  end;
end;

procedure RuntimeTypes;
var
  LBase: TBase;
  LChild: TChild;
  LObjects: INyxSequence<TObject>;
  LChildren: INyxSequence<TChild>;
  LLifetime: TLifetime;
  LItems: array of IInterface;
  LInput: INyxSequence<IInterface>;
  LSelected: INyxSequence<IItem>;
  LKeep: IItem;
  LWithoutGuid: INyxSequence<INoGuid>;
  LQuery: INyxSequence<Integer>;
  LCursor: INyxEnumerator<Integer>;
  LRejected: Boolean;
begin
  LBase := TBase.Create;
  LChild := TChild.Create;
  try
    LChild.Number := 7;
    LObjects := TNyxSequence<TObject>.From([nil, LBase, LChild]);
    LChildren := TNyxClassQuery<TChild>.OfType(LObjects);
    Check(LChildren.Single.Number = 7, 'runtime class filtering excludes nil and incompatible objects');
    LChildren := TNyxClassQuery<TChild>.Cast(LObjects);
    Check(LChildren.First = nil, 'runtime class Cast preserves typed nil');
    LRejected := False;
    try
      LChildren.Count;
    except
      on LException: ENyxQueryCast do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'class Cast refuses incompatible values during enumeration');
  finally
    LChildren := nil;
    LObjects := nil;
    LBase.Free;
    LChild.Free;
  end;
  LLifetime := TLifetime.Create;
  try
    SetLength(LItems, 3);
    LItems[0] := nil;
    LItems[1] := TOther.Create;
    LItems[2] := TItem.Create(LLifetime);
    LInput := TNyxSequence<IInterface>.From(LItems);
    LItems := nil;
    LSelected := TNyxInterfaceQuery<IItem>.OfType(LInput);
    LKeep := LSelected.Single;
    Check(LKeep.Number = 42, 'runtime interface capability filtering preserves specialized methods');
    LSelected := TNyxInterfaceQuery<IItem>.Cast(LInput);
    Check(LSelected.First = nil, 'interface Cast preserves typed nil');
    LRejected := False;
    try
      LSelected.Count;
    except
      on LException: ENyxQueryCast do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'interface Cast refuses unsupported capabilities');
    {$ifdef PAS2JS}
    LRejected := TNyxInterfaceQuery<INoGuid>.Supported;
    Check(LRejected, 'browser explicitly reports compiler-generated interface identity');
    LWithoutGuid := TNyxInterfaceQuery<INoGuid>.OfType(LInput);
    Check(LWithoutGuid.Count = 0, 'generated identity still requires an actual supported capability');
    LWithoutGuid := nil;
    {$else}
    LRejected := TNyxInterfaceQuery<INoGuid>.Supported;
    Check(not LRejected, 'native explicitly reports absent runtime interface identity');
    LRejected := False;
    try
      LWithoutGuid := TNyxInterfaceQuery<INoGuid>.OfType(LInput);
    except
      on LException: ENyxQuery do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'target interface without declared GUID refuses');
    {$endif}
    LSelected := nil;
    LInput := nil;
    Check((LLifetime.Destroyed = 0) and (LKeep.Number = 42),
      'retained typed interface outlives source and conversion query');
    LKeep := nil;
    Check(LLifetime.Destroyed = 1, 'last typed interface retires payload exactly once');
  finally
    LItems := nil;
    LInput := nil;
    LSelected := nil;
    LKeep := nil;
    LLifetime.Free;
  end;
  GCalls := 0;
  LQuery := TNyxTypeQuery<Integer, Integer>.OfType(NyxRange(0, 5),
    TNyxSelector<Integer, TIntOptional>.From(EvenOptional));
  LCursor := LQuery.GetEnumerator;
  Check(GCalls = 0, 'explicit typed converter remains deferred until movement');
  Check((LQuery.Count = 3) and (GCalls = 5), 'typed OfType preserves zero and skips only explicit None');
  LQuery := TNyxTypeQuery<Integer, Integer>.Cast(NyxRange(0, 5),
    TNyxSelector<Integer, TIntOptional>.From(EvenOptional));
  LCursor := LQuery.GetEnumerator;
  Check(LCursor.MoveNext and (LCursor.Current = 0), 'typed Cast accepts explicit present zero');
  LRejected := False;
  try
    LCursor.MoveNext;
  except
    on LException: ENyxQueryCast do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and not LCursor.MoveNext, 'typed Cast failure is terminal');
end;

constructor TTestRandom.Create(AMetrics: TRandomMetrics);
begin
  inherited Create;
  FMetrics := AMetrics;
end;

function TTestRandom.NextBelow(AExclusive: Integer): Integer;
begin
  Inc(FMetrics.Calls);
  case FMetrics.Mode of
    rmNegative:
    begin
      Result := -1;
    end;
    rmUpper:
    begin
      Result := AExclusive;
    end;
    rmFailure:
    begin
      raise ECombinationFailure.Create('original random failure');
    end;
    else
    begin
      Result := AExclusive - 1;
    end;
  end;
end;

constructor TTestRandomFactory.Create(AMetrics: TRandomMetrics);
begin
  inherited Create;
  FMetrics := AMetrics;
end;

function TTestRandomFactory.NewRandom: INyxRandom;
begin
  Inc(FMetrics.Factories);

  if FMetrics.Mode = rmNil then
  begin
    Result := nil;
  end
  else
  begin
    Result := TTestRandom.Create(FMetrics);
  end;
end;

procedure Shuffle;
var
  LSourceMetrics: TMetrics;
  LRandomMetrics: TRandomMetrics;
  LFactory: INyxRandomFactory;
  LRandom: INyxRandom;
  LSource: INyxSequence<Integer>;
  LQuery: INyxSequence<Integer>;
  LCursor: INyxEnumerator<Integer>;
  LValues: TNyxSequenceArray<Integer>;
  LAgain: TNyxSequenceArray<Integer>;
  LSeen: array of Boolean;
  LMode: TRandomMode;
  LIndex: Integer;
  LValue: Integer;
  LRejected: Boolean;
begin
  LRandom := NyxSeededRandom(1).NewRandom;
  Check((LRandom.NextBelow(1000000) = 270368) and
    (LRandom.NextBelow(1000000) = 634688), 'seeded random has identical portable golden values');
  for LIndex := 0 to 127 do
  begin
    LValue := LRandom.NextBelow(High(Integer));
    Check((LValue >= 0) and (LValue < High(Integer)), 'large random bounds retain Integer domain');
  end;
  LSourceMetrics := TMetrics.Create;
  LRandomMetrics := TRandomMetrics.Create;
  try
    LSourceMetrics.Count := 7;
    LSourceMetrics.FailAt := -1;
    LSource := TSource.Create(LSourceMetrics);
    LFactory := TTestRandomFactory.Create(LRandomMetrics);
    LQuery := TNyxShuffle<Integer>.From(LSource, LFactory);
    LCursor := LQuery.GetEnumerator;
    Check((LSourceMetrics.Starts = 0) and (LRandomMetrics.Factories = 0),
      'shuffle construction/cursor creation enters neither input nor RNG');
    Check(LCursor.MoveNext and (LCursor.Current = 0), 'caller RNG can choose identity permutation');
    Check((LSourceMetrics.Moves = 8) and (LSourceMetrics.Ends = 1) and
      (LRandomMetrics.Factories = 1) and (LRandomMetrics.Calls = 6),
      'shuffle completes source admission before using independent RNG');
    LCursor := nil;
    LQuery := TNyxShuffle<Integer>.From(LSource, NyxSeededRandom(42));
    LValues := LQuery.ToArray;
    LAgain := LQuery.ToArray;
    SetLength(LSeen, 7);
    for LIndex := 0 to 6 do
    begin
      Check(LValues[LIndex] = LAgain[LIndex], 'seeded shuffle restarts independently');
      Check((LValues[LIndex] >= 0) and (LValues[LIndex] < 7) and not LSeen[LValues[LIndex]],
        'shuffle preserves membership without duplicate/lost rows');
      LSeen[LValues[LIndex]] := True;
    end;
    Check((LSource.First = 0) and (LSource.Last = 6), 'shuffle leaves caller source order unchanged');
    for LMode := rmNegative to rmNil do
    begin
      LRandomMetrics.Mode := LMode;
      LQuery := TNyxShuffle<Integer>.From(LSource, LFactory);
      LCursor := LQuery.GetEnumerator;
      LRejected := False;
      try
        LCursor.MoveNext;
      except
        on LException: ECombinationFailure do
        begin
          LRejected := (LMode = rmFailure) and
            (LException.Message = 'original random failure');
        end;
        on LException: ENyxQuery do
        begin
          LRejected := LMode <> rmFailure;
        end;
      end;
      Check(LRejected and not LCursor.MoveNext, 'invalid/failing random contract is terminal without partial rows');
    end;
    LRandomMetrics.Factories := 0;
    LSourceMetrics.FailAt := 2;
    LQuery := TNyxShuffle<Integer>.From(LSource, LFactory);
    LRejected := False;
    try
      LQuery.First;
    except
      on LException: ECombinationFailure do
      begin
        LRejected := LException.Message = 'original source failure';
      end;
    end;
    Check(LRejected and (LRandomMetrics.Factories = 0), 'source failure precedes RNG entry');
    LSourceMetrics.FailAt := -1;
    LSourceMetrics.Count := 1;
    LQuery := TNyxShuffle<Integer>.From(LSource, LFactory);
    Check((LQuery.Single = 0) and (LRandomMetrics.Factories = 0), 'one row needs no random generator');
    LSourceMetrics.Count := 0;
    Check((LQuery.Count = 0) and (LRandomMetrics.Factories = 0), 'empty shuffle needs no random generator');
  finally
    LCursor := nil;
    LQuery := nil;
    LSource := nil;
    LFactory := nil;
    LSourceMetrics.Free;
    LRandomMetrics.Free;
  end;
end;

procedure ManagedContexts;
type
  TItemPair = INyxPairValue<IItem, IItem>;
  TItemTriple = INyxTripleValue<IItem, IItem, IItem>;
  TItemIndex = INyxIndexedValue<IItem>;
var
  LLifetime: TLifetime;
  LValues: array of IItem;
  LSource: INyxSequence<IItem>;
  LPairs: INyxSequence<TItemPair>;
  LTriples: INyxSequence<TItemTriple>;
  LIndices: INyxSequence<TItemIndex>;
  LShuffled: INyxSequence<IItem>;
  LPair: TItemPair;
  LTriple: TItemTriple;
  LIndex: TItemIndex;
  LRead: IItem;
begin
  LLifetime := TLifetime.Create;
  try
    SetLength(LValues, 1);
    LValues[0] := TItem.Create(LLifetime);
    LSource := TNyxSequence<IItem>.From(LValues);
    LValues := nil;
    LPairs := TNyxZipPairs<IItem, IItem>.From(LSource, LSource);
    LPair := LPairs.Single;
    LTriples := TNyxZipTriples<IItem, IItem, IItem>.From(LSource, LSource, LSource);
    LTriple := LTriples.Single;
    LIndices := TNyxIndex<IItem>.From(LSource);
    LIndex := LIndices.Single;
    LShuffled := TNyxShuffle<IItem>.From(LSource, NyxSeededRandom(1));
    LRead := LShuffled.Single;
    Check(LRead.Number = 42, 'shuffle preserves managed payload');
    LRead := nil;
    LPairs := nil;
    LTriples := nil;
    LIndices := nil;
    LShuffled := nil;
    LSource := nil;
    Check(LLifetime.Destroyed = 0, 'owned pair/triple/index contexts survive every source/query');
    LRead := LPair.Outer;
    Check(LRead.Number = 42, 'retained pair keeps specialized interface');
    LRead := LTriple.Third;
    Check(LRead.Number = 42, 'retained triple keeps specialized interface');
    LRead := LIndex.Value;
    Check((LRead.Number = 42) and (LIndex.Index = 0), 'retained indexed value keeps specialized interface');
    LRead := nil;
    LPair := nil;
    LTriple := nil;
    LIndex := nil;
    Check(LLifetime.Destroyed = 1, 'last owned context retires managed payload exactly once');
  finally
    LValues := nil;
    LRead := nil;
    LPair := nil;
    LTriple := nil;
    LIndex := nil;
    LPairs := nil;
    LTriples := nil;
    LIndices := nil;
    LShuffled := nil;
    LSource := nil;
    LLifetime.Free;
  end;
end;

function RunNyxCombinationChecks: Integer;
begin
  GChecks := 0;
  ZipAndIndex;
  RuntimeTypes;
  PositionsAndSlices;
  Shuffle;
  ManagedContexts;
  CallbackComposition;
  Result := GChecks;
end;

end.
