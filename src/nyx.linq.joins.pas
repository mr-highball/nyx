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
unit nyx.linq.joins;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.linq, nyx.linq.projection, nyx.linq.grouping;

type
  { Immutable owned optional partners. Presence is separate from the value: zero,
    empty text and nil interface payloads can all be present values. An absent
    partner getter refuses; callers check HasLeft/HasRight instead of substituting
    untyped sentinels. Retained contexts own managed payloads independently. }
  INyxJoinValue<TLeft, TRight> = interface(IInterface)
    function GetHasLeft: Boolean;
    function GetHasRight: Boolean;
    function GetLeft: TLeft;
    function GetRight: TRight;
    property HasLeft: Boolean read GetHasLeft;
    property HasRight: Boolean read GetHasRight;
    property Left: TLeft read GetLeft;
    property Right: TRight read GetRight;
  end;
  TNyxJoinValue<TLeft, TRight> = class(TInterfacedObject, INyxJoinValue<TLeft, TRight>)
  private
    FLeft: TLeft;
    FRight: TRight;
    FHasLeft: Boolean;
    FHasRight: Boolean;
  public
    constructor Create(const ALeft: TLeft; const ARight: TRight; AHasLeft, AHasRight: Boolean);
    function GetHasLeft: Boolean;
    function GetHasRight: Boolean;
    function GetLeft: TLeft;
    function GetRight: TRight;
  end;
  INyxGroupJoinValue<TOuter, TInner> = interface(IInterface)
    function GetOuter: TOuter;
    function GetMatches: INyxSequence<TInner>;
    property Outer: TOuter read GetOuter;
    property Matches: INyxSequence<TInner> read GetMatches;
  end;
  TNyxGroupJoinValue<TOuter, TInner> = class(TInterfacedObject, INyxGroupJoinValue<TOuter, TInner>)
  private
    FOuter: TOuter;
    FMatches: INyxSequence<TInner>;
  public
    constructor Create(const AOuter: TOuter; const AMatches: INyxSequence<TInner>);
    function GetOuter: TOuter;
    function GetMatches: INyxSequence<TInner>;
  end;
  TNyxJoinKind = (njInner, njLeft, njFull);

  { Deferred typed joins. No source/callback enters at construction/GetEnumerator.
    Inner/left/full are driven by left encounter order; duplicate matches retain
    right encounter order and emit all Cartesian pairs. Full appends unmatched
    right rows in their original order. Right is the mirrored left join, driven
    by right order while preserving the caller's Left/Right context identities.
    Each cursor builds an independent right-side lookup on first movement.
    Empty driving input avoids the other source except for Full. Source/key/
    equality/result exceptions are original and terminal for that cursor; no
    sentinel-based absence, source/cursor/UI back reference or shared enumeration.
    Keys/payloads keep the grouping equality/ordinary ownership contract. }
  TNyxJoin<TLeft, TRight, TKey, TResult> = class
  public
    class function Inner(const ALeft: INyxSequence<TLeft>; const ARight: INyxSequence<TRight>;
      const ALeftKey: INyxSelector<TLeft, TKey>; const ARightKey: INyxSelector<TRight, TKey>;
      const AResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>;
      const AEquality: INyxEquality<TKey>): INyxSequence<TResult>; static;
    class function Left(const ALeft: INyxSequence<TLeft>; const ARight: INyxSequence<TRight>;
      const ALeftKey: INyxSelector<TLeft, TKey>; const ARightKey: INyxSelector<TRight, TKey>;
      const AResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
      const AEquality: INyxEquality<TKey>): INyxSequence<TResult>; static;
    class function Right(const ALeft: INyxSequence<TLeft>; const ARight: INyxSequence<TRight>;
      const ALeftKey: INyxSelector<TLeft, TKey>; const ARightKey: INyxSelector<TRight, TKey>;
      const AResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
      const AEquality: INyxEquality<TKey>): INyxSequence<TResult>; static;
    class function Full(const ALeft: INyxSequence<TLeft>; const ARight: INyxSequence<TRight>;
      const ALeftKey: INyxSelector<TLeft, TKey>; const ARightKey: INyxSelector<TRight, TKey>;
      const AResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
      const AEquality: INyxEquality<TKey>): INyxSequence<TResult>; static;
  end;
  TNyxGroupJoin<TOuter, TInner, TKey, TResult> = class
  public
    { One result per outer row, including unmatched rows with an empty immutable
      Matches sequence. Matches retain inner order and safely outlive the join.
      Key/result selection runs once per outer row, independent of group size. }
    class function From(const AOuter: INyxSequence<TOuter>; const AInner: INyxSequence<TInner>;
      const AOuterKey: INyxSelector<TOuter, TKey>; const AInnerKey: INyxSelector<TInner, TKey>;
      const AResult: INyxSelector<INyxGroupJoinValue<TOuter, TInner>, TResult>;
      const AEquality: INyxEquality<TKey>): INyxSequence<TResult>; static;
  end;

  { Native generic expansion dependencies. Contexts and index rows are managed
    objects rather than records containing browser-unsupported COM interfaces. }
  TJoinIndexSelector<T> = class(TInterfacedObject, INyxIndexedSelector<T, INyxIndexedValue<T>>)
    function Project(const AValue: T; AIndex: Integer): INyxIndexedValue<T>;
  end;
  TJoinIndexKey<T, TKey> = class(TInterfacedObject, INyxSelector<INyxIndexedValue<T>, TKey>)
  private
    FKey: INyxSelector<T, TKey>;
  public
    constructor Create(const AKey: INyxSelector<T, TKey>);
    function Project(const AValue: INyxIndexedValue<T>): TKey;
  end;
  TJoinIdentity<T> = class(TInterfacedObject, INyxSelector<T, T>)
    function Project(const AValue: T): T;
  end;
  TInnerJoinResult<TLeft, TRight, TResult> = class(TInterfacedObject,
    INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>)
  private
    FResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>;
  public
    constructor Create(const AResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>);
    function Project(const AValue: INyxJoinValue<TLeft, TRight>): TResult;
  end;
  TReverseJoinResult<TLeft, TRight, TResult> = class(TInterfacedObject,
    INyxSelector<INyxJoinValue<TRight, TLeft>, TResult>)
  private
    FResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
    FEmptyLeft: TLeft;
    FEmptyRight: TRight;
  public
    constructor Create(const AResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>);
    function Project(const AValue: INyxJoinValue<TRight, TLeft>): TResult;
  end;
  TJoinEnumerator<TLeft, TRight, TKey, TResult> = class(TEnumerator<TResult>)
  private
    FLeftSource: INyxSequence<TLeft>;
    FRightSource: INyxSequence<TRight>;
    FLeft: INyxEnumerator<TLeft>;
    FMatches: INyxEnumerator<INyxIndexedValue<TRight>>;
    FLookup: INyxLookup<TKey, INyxIndexedValue<TRight>>;
    FRightRows: TNyxSequenceArray<INyxIndexedValue<TRight>>;
    FMatched: array of Boolean;
    FLeftKey: INyxSelector<TLeft, TKey>;
    FRightKey: INyxSelector<TRight, TKey>;
    FResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
    FEquality: INyxEquality<TKey>;
    FKind: TNyxJoinKind;
    FStarted: Boolean;
    FHasLeft: Boolean;
    FLeftValue: TLeft;
    FAdvanceLeft: Boolean;
    FEmptyLeft: TLeft;
    FEmptyRight: TRight;
    FUnmatched: Integer;
    procedure Prepare;
    procedure Retire;
  protected
    function Advance(out AValue: TResult): Boolean; override;
  public
    constructor Create(const ALeft: INyxSequence<TLeft>; const ARight: INyxSequence<TRight>;
      const ALeftKey: INyxSelector<TLeft, TKey>; const ARightKey: INyxSelector<TRight, TKey>;
      const AResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
      const AEquality: INyxEquality<TKey>; AKind: TNyxJoinKind);
  end;
  TJoinSequence<TLeft, TRight, TKey, TResult> = class(TNyxSequence<TResult>)
  private
    FLeft: INyxSequence<TLeft>;
    FRight: INyxSequence<TRight>;
    FLeftKey: INyxSelector<TLeft, TKey>;
    FRightKey: INyxSelector<TRight, TKey>;
    FResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
    FEquality: INyxEquality<TKey>;
    FKind: TNyxJoinKind;
  public
    constructor Create(const ALeft: INyxSequence<TLeft>; const ARight: INyxSequence<TRight>;
      const ALeftKey: INyxSelector<TLeft, TKey>; const ARightKey: INyxSelector<TRight, TKey>;
      const AResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
      const AEquality: INyxEquality<TKey>; AKind: TNyxJoinKind);
    function GetEnumerator: INyxEnumerator<TResult>; override;
  end;
  TGroupJoinEnumerator<TOuter, TInner, TKey, TResult> = class(TEnumerator<TResult>)
  private
    FOuterSource: INyxSequence<TOuter>;
    FInnerSource: INyxSequence<TInner>;
    FOuter: INyxEnumerator<TOuter>;
    FLookup: INyxLookup<TKey, TInner>;
    FOuterKey: INyxSelector<TOuter, TKey>;
    FInnerKey: INyxSelector<TInner, TKey>;
    FResult: INyxSelector<INyxGroupJoinValue<TOuter, TInner>, TResult>;
    FEquality: INyxEquality<TKey>;
    procedure Retire;
  protected
    function Advance(out AValue: TResult): Boolean; override;
  public
    constructor Create(const AOuter: INyxSequence<TOuter>; const AInner: INyxSequence<TInner>;
      const AOuterKey: INyxSelector<TOuter, TKey>; const AInnerKey: INyxSelector<TInner, TKey>;
      const AResult: INyxSelector<INyxGroupJoinValue<TOuter, TInner>, TResult>;
      const AEquality: INyxEquality<TKey>);
  end;
  TGroupJoinSequence<TOuter, TInner, TKey, TResult> = class(TNyxSequence<TResult>)
  private
    FOuter: INyxSequence<TOuter>;
    FInner: INyxSequence<TInner>;
    FOuterKey: INyxSelector<TOuter, TKey>;
    FInnerKey: INyxSelector<TInner, TKey>;
    FResult: INyxSelector<INyxGroupJoinValue<TOuter, TInner>, TResult>;
    FEquality: INyxEquality<TKey>;
  public
    constructor Create(const AOuter: INyxSequence<TOuter>; const AInner: INyxSequence<TInner>;
      const AOuterKey: INyxSelector<TOuter, TKey>; const AInnerKey: INyxSelector<TInner, TKey>;
      const AResult: INyxSelector<INyxGroupJoinValue<TOuter, TInner>, TResult>;
      const AEquality: INyxEquality<TKey>);
    function GetEnumerator: INyxEnumerator<TResult>; override;
  end;

implementation

constructor TNyxJoinValue<TLeft, TRight>.Create(const ALeft: TLeft; const ARight: TRight;
  AHasLeft, AHasRight: Boolean);
begin
  inherited Create;
  { An absent side owns no supplied payload. Presence still permits nil as a
    real managed value; absence is never inferred from the value itself. }

  if AHasLeft then
  begin
    FLeft := ALeft;
  end;

  if AHasRight then
  begin
    FRight := ARight;
  end;
  FHasLeft := AHasLeft;
  FHasRight := AHasRight;
end;

function TNyxJoinValue<TLeft, TRight>.GetHasLeft: Boolean;
begin
  Result := FHasLeft;
end;

function TNyxJoinValue<TLeft, TRight>.GetHasRight: Boolean;
begin
  Result := FHasRight;
end;

function TNyxJoinValue<TLeft, TRight>.GetLeft: TLeft;
begin

  if not FHasLeft then
  begin
    raise ENyxQuery.Create('Join context has no left value');
  end;
  Result := FLeft;
end;

function TNyxJoinValue<TLeft, TRight>.GetRight: TRight;
begin

  if not FHasRight then
  begin
    raise ENyxQuery.Create('Join context has no right value');
  end;
  Result := FRight;
end;

constructor TNyxGroupJoinValue<TOuter, TInner>.Create(const AOuter: TOuter;
  const AMatches: INyxSequence<TInner>);
begin
  inherited Create;
  FOuter := AOuter;
  FMatches := AMatches;
end;

function TNyxGroupJoinValue<TOuter, TInner>.GetOuter: TOuter;
begin
  Result := FOuter;
end;

function TNyxGroupJoinValue<TOuter, TInner>.GetMatches: INyxSequence<TInner>;
begin
  Result := FMatches;
end;

function TJoinIndexSelector<T>.Project(const AValue: T; AIndex: Integer): INyxIndexedValue<T>;
begin
  Result := TNyxIndexedValue<T>.Create(AValue, AIndex);
end;

constructor TJoinIndexKey<T, TKey>.Create(const AKey: INyxSelector<T, TKey>);
begin
  inherited Create;
  FKey := AKey;
end;

function TJoinIndexKey<T, TKey>.Project(const AValue: INyxIndexedValue<T>): TKey;
var
  LValue: T;
begin
  LValue := AValue.Value;
  Result := FKey.Project(LValue);
end;

function TJoinIdentity<T>.Project(const AValue: T): T;
begin
  Result := AValue;
end;

constructor TInnerJoinResult<TLeft, TRight, TResult>.Create(
  const AResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>);
begin
  inherited Create;
  FResult := AResult;
end;

function TInnerJoinResult<TLeft, TRight, TResult>.Project(const AValue: INyxJoinValue<TLeft, TRight>): TResult;
var
  LLeft: TLeft;
  LRight: TRight;
  LPair: INyxPairValue<TLeft, TRight>;
begin
  LLeft := AValue.Left;
  LRight := AValue.Right;
  LPair := TNyxPairValue<TLeft, TRight>.Create(LLeft, LRight);
  Result := FResult.Project(LPair);
end;

constructor TReverseJoinResult<TLeft, TRight, TResult>.Create(
  const AResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>);
begin
  inherited Create;
  FResult := AResult;
end;

function TReverseJoinResult<TLeft, TRight, TResult>.Project(const AValue: INyxJoinValue<TRight, TLeft>): TResult;
var
  LLeft: TLeft;
  LRight: TRight;
  LContext: INyxJoinValue<TLeft, TRight>;
begin
  LLeft := FEmptyLeft;
  LRight := FEmptyRight;

  if AValue.HasRight then
  begin
    LLeft := AValue.Right;
  end;

  if AValue.HasLeft then
  begin
    LRight := AValue.Left;
  end;
  LContext := TNyxJoinValue<TLeft, TRight>.Create(LLeft, LRight, AValue.HasRight, AValue.HasLeft);
  Result := FResult.Project(LContext);
end;

constructor TJoinEnumerator<TLeft, TRight, TKey, TResult>.Create(const ALeft: INyxSequence<TLeft>;
  const ARight: INyxSequence<TRight>; const ALeftKey: INyxSelector<TLeft, TKey>;
  const ARightKey: INyxSelector<TRight, TKey>;
  const AResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
  const AEquality: INyxEquality<TKey>; AKind: TNyxJoinKind);
begin
  inherited Create;
  FLeftSource := ALeft;
  FRightSource := ARight;
  FLeftKey := ALeftKey;
  FRightKey := ARightKey;
  FResult := AResult;
  FEquality := AEquality;
  FKind := AKind;
end;

procedure TJoinEnumerator<TLeft, TRight, TKey, TResult>.Prepare;
type
  TIndexedRight = INyxIndexedValue<TRight>;
var
  LIndexed: INyxSequence<TIndexedRight>;
  LKey: INyxSelector<TIndexedRight, TKey>;
  LIdentity: INyxSelector<TIndexedRight, TIndexedRight>;
begin
  FStarted := True;
  FLeft := FLeftSource.GetEnumerator;
  FLeftSource := nil;

  if FLeft = nil then
  begin
    raise ENyxQuery.Create('Join left source returned no cursor');
  end;
  FHasLeft := FLeft.MoveNext;

  if not FHasLeft and (FKind <> njFull) then
  begin
    Exit;
  end;
  LIndexed := TNyxSelectIndexed<TRight, TIndexedRight>.From(FRightSource, TJoinIndexSelector<TRight>.Create);
  FRightRows := LIndexed.ToArray;
  FRightSource := nil;
  SetLength(FMatched, Length(FRightRows));
  LKey := TJoinIndexKey<TRight, TKey>.Create(FRightKey);
  LIdentity := TJoinIdentity<TIndexedRight>.Create;
  FLookup := TNyxLookup<TIndexedRight, TKey, TIndexedRight>.From(
    TNyxSequence<TIndexedRight>.From(FRightRows), LKey, LIdentity, FEquality);
  FRightKey := nil;
end;

procedure TJoinEnumerator<TLeft, TRight, TKey, TResult>.Retire;
begin
  FLeftSource := nil;
  FRightSource := nil;
  FLeft := nil;
  FMatches := nil;
  FLookup := nil;
  FRightRows := nil;
  FMatched := nil;
  FLeftKey := nil;
  FRightKey := nil;
  FResult := nil;
  FEquality := nil;
  FLeftValue := FEmptyLeft;
end;

function TJoinEnumerator<TLeft, TRight, TKey, TResult>.Advance(out AValue: TResult): Boolean;
var
  LRight: INyxIndexedValue<TRight>;
  LGroup: INyxGroup<TKey, INyxIndexedValue<TRight>>;
  LMatches: INyxSequence<INyxIndexedValue<TRight>>;
  LKey: TKey;
  LRightValue: TRight;
  LContext: INyxJoinValue<TLeft, TRight>;
begin
  try

    if not FStarted then
    begin
      Prepare;
    end;
    while True do
    begin

      if FAdvanceLeft then
      begin
        FAdvanceLeft := False;
        FHasLeft := FLeft.MoveNext;
      end;

      if FMatches <> nil then
      begin

        if FMatches.MoveNext then
        begin
          LRight := FMatches.Current;
          FMatched[LRight.Index] := True;
          LRightValue := LRight.Value;
          LContext := TNyxJoinValue<TLeft, TRight>.Create(FLeftValue, LRightValue, True, True);
          AValue := FResult.Project(LContext);
          Exit(True);
        end;
        FMatches := nil;
        FHasLeft := FLeft.MoveNext;
      end;

      if FHasLeft then
      begin

        if (FKind = njInner) and (FLookup.Count = 0) then
        begin
          Retire;
          Exit(False);
        end;
        FLeftValue := FLeft.Current;
        LKey := FLeftKey.Project(FLeftValue);
        LGroup := FLookup.GroupFor(LKey);

        if LGroup <> nil then
        begin
          LMatches := LGroup.Values;
          FMatches := LMatches.GetEnumerator;
          Continue;
        end;
        FMatches := nil;

        if FKind <> njInner then
        begin
          LContext := TNyxJoinValue<TLeft, TRight>.Create(FLeftValue, FEmptyRight, True, False);
          FAdvanceLeft := True;
          AValue := FResult.Project(LContext);
          Exit(True);
        end;
        FHasLeft := FLeft.MoveNext;
        Continue;
      end;

      if FKind = njFull then
      begin
        while FUnmatched < Length(FRightRows) do
        begin
          LRight := FRightRows[FUnmatched];
          Inc(FUnmatched);

          if not FMatched[LRight.Index] then
          begin
            LRightValue := LRight.Value;
            LContext := TNyxJoinValue<TLeft, TRight>.Create(FEmptyLeft, LRightValue, False, True);
            AValue := FResult.Project(LContext);
            Exit(True);
          end;
        end;
      end;
      Retire;
      Exit(False);
    end;
  except
    Retire;
    raise;
  end;
end;

constructor TJoinSequence<TLeft, TRight, TKey, TResult>.Create(const ALeft: INyxSequence<TLeft>;
  const ARight: INyxSequence<TRight>; const ALeftKey: INyxSelector<TLeft, TKey>;
  const ARightKey: INyxSelector<TRight, TKey>;
  const AResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
  const AEquality: INyxEquality<TKey>; AKind: TNyxJoinKind);
begin
  inherited Create;

  if (ALeft = nil) or (ARight = nil) or (ALeftKey = nil) or (ARightKey = nil)
    or (AResult = nil) or (AEquality = nil) then
  begin
    raise ENyxQuery.Create('Join requires sources, key/result callbacks and equality');
  end;
  FLeft := ALeft;
  FRight := ARight;
  FLeftKey := ALeftKey;
  FRightKey := ARightKey;
  FResult := AResult;
  FEquality := AEquality;
  FKind := AKind;
end;

function TJoinSequence<TLeft, TRight, TKey, TResult>.GetEnumerator: INyxEnumerator<TResult>;
begin
  Result := TJoinEnumerator<TLeft, TRight, TKey, TResult>.Create(FLeft, FRight, FLeftKey,
    FRightKey, FResult, FEquality, FKind);
end;

class function TNyxJoin<TLeft, TRight, TKey, TResult>.Inner(const ALeft: INyxSequence<TLeft>;
  const ARight: INyxSequence<TRight>; const ALeftKey: INyxSelector<TLeft, TKey>;
  const ARightKey: INyxSelector<TRight, TKey>;
  const AResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>;
  const AEquality: INyxEquality<TKey>): INyxSequence<TResult>;
begin

  if AResult = nil then
  begin
    raise ENyxQuery.Create('Inner join requires an owned pair result selector');
  end;
  Result := TJoinSequence<TLeft, TRight, TKey, TResult>.Create(ALeft, ARight, ALeftKey,
    ARightKey, TInnerJoinResult<TLeft, TRight, TResult>.Create(AResult), AEquality, njInner);
end;

class function TNyxJoin<TLeft, TRight, TKey, TResult>.Left(const ALeft: INyxSequence<TLeft>;
  const ARight: INyxSequence<TRight>; const ALeftKey: INyxSelector<TLeft, TKey>;
  const ARightKey: INyxSelector<TRight, TKey>;
  const AResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
  const AEquality: INyxEquality<TKey>): INyxSequence<TResult>;
begin
  Result := TJoinSequence<TLeft, TRight, TKey, TResult>.Create(ALeft, ARight, ALeftKey,
    ARightKey, AResult, AEquality, njLeft);
end;

class function TNyxJoin<TLeft, TRight, TKey, TResult>.Right(const ALeft: INyxSequence<TLeft>;
  const ARight: INyxSequence<TRight>; const ALeftKey: INyxSelector<TLeft, TKey>;
  const ARightKey: INyxSelector<TRight, TKey>;
  const AResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
  const AEquality: INyxEquality<TKey>): INyxSequence<TResult>;
begin

  if AResult = nil then
  begin
    raise ENyxQuery.Create('Right join requires an owned result selector');
  end;
  Result := TJoinSequence<TRight, TLeft, TKey, TResult>.Create(ARight, ALeft, ARightKey,
    ALeftKey, TReverseJoinResult<TLeft, TRight, TResult>.Create(AResult), AEquality, njLeft);
end;

class function TNyxJoin<TLeft, TRight, TKey, TResult>.Full(const ALeft: INyxSequence<TLeft>;
  const ARight: INyxSequence<TRight>; const ALeftKey: INyxSelector<TLeft, TKey>;
  const ARightKey: INyxSelector<TRight, TKey>;
  const AResult: INyxSelector<INyxJoinValue<TLeft, TRight>, TResult>;
  const AEquality: INyxEquality<TKey>): INyxSequence<TResult>;
begin
  Result := TJoinSequence<TLeft, TRight, TKey, TResult>.Create(ALeft, ARight, ALeftKey,
    ARightKey, AResult, AEquality, njFull);
end;

constructor TGroupJoinEnumerator<TOuter, TInner, TKey, TResult>.Create(const AOuter: INyxSequence<TOuter>;
  const AInner: INyxSequence<TInner>; const AOuterKey: INyxSelector<TOuter, TKey>;
  const AInnerKey: INyxSelector<TInner, TKey>;
  const AResult: INyxSelector<INyxGroupJoinValue<TOuter, TInner>, TResult>;
  const AEquality: INyxEquality<TKey>);
begin
  inherited Create;
  FOuterSource := AOuter;
  FInnerSource := AInner;
  FOuterKey := AOuterKey;
  FInnerKey := AInnerKey;
  FResult := AResult;
  FEquality := AEquality;
end;

procedure TGroupJoinEnumerator<TOuter, TInner, TKey, TResult>.Retire;
begin
  FOuterSource := nil;
  FInnerSource := nil;
  FOuter := nil;
  FLookup := nil;
  FOuterKey := nil;
  FInnerKey := nil;
  FResult := nil;
  FEquality := nil;
end;

function TGroupJoinEnumerator<TOuter, TInner, TKey, TResult>.Advance(out AValue: TResult): Boolean;
var
  LOuter: TOuter;
  LKey: TKey;
  LMatches: INyxSequence<TInner>;
  LContext: INyxGroupJoinValue<TOuter, TInner>;
  LIdentity: INyxSelector<TInner, TInner>;
begin
  try

    if FOuter = nil then
    begin
      FOuter := FOuterSource.GetEnumerator;
      FOuterSource := nil;

      if FOuter = nil then
      begin
        raise ENyxQuery.Create('GroupJoin source returned no cursor');
      end;
    end;

    if not FOuter.MoveNext then
    begin
      Retire;
      Exit(False);
    end;

    if FLookup = nil then
    begin
      LIdentity := TJoinIdentity<TInner>.Create;
      FLookup := TNyxLookup<TInner, TKey, TInner>.From(FInnerSource, FInnerKey,
        LIdentity, FEquality);
      FInnerSource := nil;
      FInnerKey := nil;
    end;
    LOuter := FOuter.Current;
    LKey := FOuterKey.Project(LOuter);
    LMatches := FLookup.ValuesFor(LKey);
    LContext := TNyxGroupJoinValue<TOuter, TInner>.Create(LOuter, LMatches);
    AValue := FResult.Project(LContext);
    Result := True;
  except
    Retire;
    raise;
  end;
end;

constructor TGroupJoinSequence<TOuter, TInner, TKey, TResult>.Create(const AOuter: INyxSequence<TOuter>;
  const AInner: INyxSequence<TInner>; const AOuterKey: INyxSelector<TOuter, TKey>;
  const AInnerKey: INyxSelector<TInner, TKey>;
  const AResult: INyxSelector<INyxGroupJoinValue<TOuter, TInner>, TResult>;
  const AEquality: INyxEquality<TKey>);
begin
  inherited Create;

  if (AOuter = nil) or (AInner = nil) or (AOuterKey = nil) or (AInnerKey = nil)
    or (AResult = nil) or (AEquality = nil) then
  begin
    raise ENyxQuery.Create('GroupJoin requires sources, key/result callbacks and equality');
  end;
  FOuter := AOuter;
  FInner := AInner;
  FOuterKey := AOuterKey;
  FInnerKey := AInnerKey;
  FResult := AResult;
  FEquality := AEquality;
end;

function TGroupJoinSequence<TOuter, TInner, TKey, TResult>.GetEnumerator: INyxEnumerator<TResult>;
begin
  Result := TGroupJoinEnumerator<TOuter, TInner, TKey, TResult>.Create(FOuter, FInner,
    FOuterKey, FInnerKey, FResult, FEquality);
end;

class function TNyxGroupJoin<TOuter, TInner, TKey, TResult>.From(const AOuter: INyxSequence<TOuter>;
  const AInner: INyxSequence<TInner>; const AOuterKey: INyxSelector<TOuter, TKey>;
  const AInnerKey: INyxSelector<TInner, TKey>;
  const AResult: INyxSelector<INyxGroupJoinValue<TOuter, TInner>, TResult>;
  const AEquality: INyxEquality<TKey>): INyxSequence<TResult>;
begin
  Result := TGroupJoinSequence<TOuter, TInner, TKey, TResult>.Create(AOuter, AInner,
    AOuterKey, AInnerKey, AResult, AEquality);
end;

end.
