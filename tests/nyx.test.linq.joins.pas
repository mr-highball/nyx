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
unit nyx.test.linq.joins;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

interface

function RunNyxJoinChecks: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.linq, nyx.linq.projection, nyx.linq.grouping,
  nyx.linq.joins, nyx.callback.scope;

type
  TIntGroup = INyxGroup<Integer, Integer>;
  TIntPair = INyxPairValue<Integer, Integer>;
  TIntJoin = INyxJoinValue<Integer, Integer>;
  TIntGroupJoin = INyxGroupJoinValue<Integer, Integer>;
  TObjectJoin = INyxJoinValue<IInterface, Integer>;
  TCountingEquality = class(TInterfacedObject, INyxEquality<Integer>)
  public
    Collide: Boolean;
    Hashes: Integer;
    Equals: Integer;
    function Equal(const ALeft, ARight: Integer): Boolean;
    function Hash(const AValue: Integer): Cardinal;
  end;
  TLifetime = class
    Destroyed: Integer;
  end;
  TManaged = class(TInterfacedObject)
  private
    FCounter: TLifetime;
  public
    constructor Create(ACounter: TLifetime);
    destructor Destroy; override;
  end;
  TLateCursor = class(TEnumerator<Integer>)
  private
    FUsed: Boolean;
  protected
    function Advance(out AValue: Integer): Boolean; override;
  end;
  TLateSource = class(TNyxSequence<Integer>)
    function GetEnumerator: INyxEnumerator<Integer>; override;
  end;
  TNilCursorSource = class(TNyxSequence<Integer>)
    function GetEnumerator: INyxEnumerator<Integer>; override;
  end;

var
  GChecks: Integer;
  GKeys: Integer;
  GElements: Integer;
  GRetainedMatches: INyxSequence<Integer>;
  GRetainedPartner: INyxJoinValue<IInterface, Integer>;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxQuery.Create('Grouping/join qualification: ' + AReason);
  end;
  Inc(GChecks);
end;

function Identity(const AValue: Integer): Integer;
begin
  Inc(GElements);
  Result := AValue;
end;

function RetainPartner(const AValue: TObjectJoin): Integer;
begin
  GRetainedPartner := AValue;
  Result := AValue.Right;
end;

function TNilCursorSource.GetEnumerator: INyxEnumerator<Integer>;
begin
  Result := nil;
end;

function Parity(const AValue: Integer): Integer;
begin
  Inc(GKeys);
  Result := AValue mod 2;
end;

function ExactKey(const AValue: Integer): Integer;
begin
  Inc(GKeys);
  Result := AValue;
end;

function FailKey(const AValue: Integer): Integer;
begin

  if AValue = 2 then
  begin
    raise ENyxQuery.Create('Key failed / 🌙');
  end;
  Result := AValue;
end;

function TextIdentity(const AValue: TNyxText): TNyxText;
begin
  Result := AValue;
end;

function ConstantObjectKey(const AValue: IInterface): Integer;
begin
  Result := 0;
end;

function ObjectIdentity(const AValue: IInterface): IInterface;
begin
  Result := AValue;
end;

function PairText(const AValue: TIntPair): TNyxText;
begin
  Result := TNyxText(IntToStr(AValue.Outer)) + TNyxText(':') + TNyxText(IntToStr(AValue.Inner));
end;

function OptionalText(const AValue: TIntJoin): TNyxText;
begin
  Result := '-';

  if AValue.HasLeft then
  begin
    Result := TNyxText(IntToStr(AValue.Left));
  end;
  Result := Result + TNyxText(':');

  if AValue.HasRight then
  begin
    Result := Result + TNyxText(IntToStr(AValue.Right));
  end
  else
  begin
    Result := Result + TNyxText('-');
  end;
end;

function GroupText(const AValue: TIntGroupJoin): TNyxText;
var
  LMatches: INyxSequence<Integer>;
begin
  LMatches := AValue.Matches;

  if AValue.Outer = 1 then
  begin
    GRetainedMatches := LMatches;
  end;
  Result := TNyxText(IntToStr(AValue.Outer)) + TNyxText('=') + TNyxText(IntToStr(LMatches.Count));
end;

function FailResult(const AValue: TIntJoin): TNyxText;
begin
  Result := '';
  raise ENyxQuery.Create('Result failed / 🌙');
end;

function TCountingEquality.Equal(const ALeft, ARight: Integer): Boolean;
begin
  Inc(Equals);
  Result := ALeft = ARight;
end;

function TCountingEquality.Hash(const AValue: Integer): Cardinal;
begin
  Inc(Hashes);

  if Collide then
  begin
    Result := High(Cardinal);
  end
  else
  begin
    Result := Cardinal(AValue);
  end;
end;

constructor TManaged.Create(ACounter: TLifetime);
begin
  inherited Create;
  FCounter := ACounter;
end;

destructor TManaged.Destroy;
begin
  Inc(FCounter.Destroyed);
  inherited Destroy;
end;

function TLateCursor.Advance(out AValue: Integer): Boolean;
begin

  if FUsed then
  begin
    raise ENyxQuery.Create('Late source failed / 🌙');
  end;
  FUsed := True;
  AValue := 9;
  Result := True;
end;

function TLateSource.GetEnumerator: INyxEnumerator<Integer>;
begin
  Result := TLateCursor.Create;
end;

procedure Grouping;
var
  LSource: INyxSequence<Integer>;
  LGroups: INyxSequence<TIntGroup>;
  LCursor: INyxEnumerator<TIntGroup>;
  LOther: INyxEnumerator<TIntGroup>;
  LGroup: TIntGroup;
  LValues: INyxSequence<Integer>;
  LRows: TNyxSequenceArray<Integer>;
  LLookup: INyxLookup<Integer, Integer>;
  LEquality: INyxEquality<Integer>;
  LCounter: TCountingEquality;
  LRefused: Boolean;
begin
  LSource := TNyxSequence<Integer>.From([3, 1, 3, 2, 1]);
  GKeys := 0;
  GElements := 0;
  LGroups := TNyxGroupBy<Integer, Integer, Integer>.From(LSource,
    TNyxSelector<Integer, Integer>.From(Parity), TNyxSelector<Integer, Integer>.From(Identity), NewNyxIntegerEquality);
  LCursor := LGroups.GetEnumerator;
  LOther := LGroups.GetEnumerator;
  Check((GKeys = 0) and (GElements = 0), 'group construction/acquisition is deferred');
  LGroups := nil;
  LSource := nil;
  Check(LCursor.MoveNext and (GKeys = 5) and (GElements = 5), 'one complete grouping projects each row/key once');
  LGroup := LCursor.Current;
  Check((LGroup.Key = 1) and (LGroup.Count = 4), 'first encountered key owns first stable group');
  LValues := LGroup.Values;
  LRows := LValues.ToArray;
  Check((LRows[0] = 3) and (LRows[1] = 1) and (LRows[2] = 3) and (LRows[3] = 1), 'group values preserve input order');
  Check(LOther.MoveNext and (GKeys = 10), 'independent cursor repeats grouping without shared state');
  Check(LCursor.MoveNext, 'second key group exists');
  LGroup := LCursor.Current;
  Check((LGroup.Key = 0) and (LGroup.Count = 1), 'first-key encounter order remains stable');
  Check(not LCursor.MoveNext and not LCursor.MoveNext, 'group cursor EOF is terminal');
  LCursor := nil;
  LOther := nil;
  LGroup := nil;
  Check((LValues.Count = 4) and (LValues.First = 3), 'retained group values outlive both cursors and recipe');
  LCounter := TCountingEquality.Create;
  LCounter.Collide := True;
  LEquality := LCounter;
  LLookup := TNyxLookup<Integer, Integer, Integer>.From(TNyxSequence<Integer>.From([3, 1, 3, 2]),
    TNyxSelector<Integer, Integer>.From(ExactKey), TNyxSelector<Integer, Integer>.From(Identity), LEquality);
  Check((LLookup.Count = 3) and (LLookup.ItemCount = 4) and (LCounter.Equals > 0), 'hash collisions still compare typed keys');
  LValues := LLookup.ValuesFor(3);
  Check((LValues.Count = 2) and (LValues.Last = 3), 'duplicate keys preserve their own rows');
  Check(not LLookup.Has(99) and (LLookup.GroupFor(99) = nil) and not LLookup.ValuesFor(99).Any,
    'absent key has explicit nil group/empty values');
  LRefused := False;
  try
    LLookup.GroupAt(-1);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'lookup group bounds refuse');
  LCursor := TNyxGroupBy<Integer, Integer, Integer>.From(TNyxSequence<Integer>.From([1, 2, 3]),
    TNyxSelector<Integer, Integer>.From(FailKey), TNyxSelector<Integer, Integer>.From(Identity), NewNyxIntegerEquality).GetEnumerator;
  LRefused := False;
  try
    LCursor.MoveNext;
  except
    on LException: ENyxQuery do
    begin
      LRefused := Pos('Key failed', LException.Message) > 0;
    end;
  end;
  Check(LRefused and not LCursor.MoveNext, 'key failure exposes no partial group and remains original/terminal');
end;

procedure EqualityAndLarge;
var
  LInts: INyxEquality<Integer>;
  LText: INyxEquality<TNyxText>;
  LLookup: INyxLookup<TNyxText, TNyxText>;
  LRows: TNyxSequenceArray<Integer>;
  LIndex: Integer;
  LGroups: INyxLookup<Integer, Integer>;
  LCounter: TCountingEquality;
  LEquality: INyxEquality<Integer>;
  LGroup: TIntGroup;
  LValues: INyxSequence<Integer>;
  LMalformed: TNyxText;
  LRefused: Boolean;
begin
  LInts := NewNyxIntegerEquality;
  Check((LInts.Hash(Low(Integer)) = 0) and (LInts.Hash(High(Integer)) = High(Cardinal)), 'integer hash admits both signed extremes without overflow');
  LText := NewNyxTextEquality(ntoAsciiInsensitive);
  Check(LText.Equal(TNyxText('A🌙') + TNyxText(#0), TNyxText('a🌙') + TNyxText(#0)) and
    (LText.Hash(TNyxText('A🌙') + TNyxText(#0)) = LText.Hash(TNyxText('a🌙') + TNyxText(#0))), 'explicit ASCII folding has equal portable scalar hashes');
  Check(not LText.Equal('Ω', 'ω'), 'ASCII folding never invents wider Unicode equivalence');
  LText := NewNyxTextEquality;
  Check(LText.Hash(TNyxText('🌙') + TNyxText(#0)) = 3960839, 'Unicode scalar hash agrees across UTF8/UTF16 storage');
  LLookup := TNyxLookup<TNyxText, TNyxText, TNyxText>.From(
    TNyxSequence<TNyxText>.From([TNyxText('🌙'), TNyxText('A') + TNyxText(#0), TNyxText('🌙')]),
    TNyxSelector<TNyxText, TNyxText>.From(TextIdentity), TNyxSelector<TNyxText, TNyxText>.From(TextIdentity), LText);
  Check((LLookup.Count = 2) and LLookup.Has(TNyxText('A') + TNyxText(#0)) and not LLookup.Has('A'), 'embedded NUL is a distinct lookup key');
  {$ifdef PAS2JS}LMalformed := String(WideChar($d800));{$else}
  SetLength(LMalformed, 1);
  LMalformed[1] := AnsiChar($ff);
  {$endif}
  LRefused := False;
  try
    LText.Hash(LMalformed);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'hash refuses malformed text even without a comparison');
  SetLength(LRows, 4096);
  for LIndex := 0 to High(LRows) do
  begin
    LRows[LIndex] := LIndex;
  end;
  LCounter := TCountingEquality.Create;
  LEquality := LCounter;
  LGroups := TNyxLookup<Integer, Integer, Integer>.From(TNyxSequence<Integer>.From(LRows),
    TNyxSelector<Integer, Integer>.From(ExactKey), TNyxSelector<Integer, Integer>.From(Identity), LEquality);
  LRows := nil;
  Check((LGroups.Count = 4096) and (LCounter.Hashes = 4096) and (LCounter.Equals < 8192), 'indexed admission caches hashes and avoids scanning all prior groups');
  for LIndex := 0 to 4095 do
  begin
    LGroup := LGroups.GroupAt(LIndex);
    LValues := LGroups.ValuesFor(LIndex);
    Check((LGroup.Key = LIndex) and (LValues.Single = LIndex), 'complete large group membership/order/lookup');
  end;
end;

procedure Joins;
var
  LLeft: INyxSequence<Integer>;
  LRight: INyxSequence<Integer>;
  LKey: INyxSelector<Integer, Integer>;
  LPair: INyxSelector<TIntPair, TNyxText>;
  LOptional: INyxSelector<TIntJoin, TNyxText>;
  LResult: INyxSequence<TNyxText>;
  LCursor: INyxEnumerator<TNyxText>;
  LOther: INyxEnumerator<TNyxText>;
  LTexts: TNyxSequenceArray<TNyxText>;
  LContext: TIntJoin;
  LRefused: Boolean;
  LAbsent: Integer;
begin
  LLeft := TNyxSequence<Integer>.From([1, 1, 2, 4]);
  LRight := TNyxSequence<Integer>.From([1, 1, 3, 0]);
  LKey := TNyxSelector<Integer, Integer>.From(ExactKey);
  LPair := TNyxSelector<TIntPair, TNyxText>.From(PairText);
  LOptional := TNyxSelector<TIntJoin, TNyxText>.From(OptionalText);
  GKeys := 0;
  LResult := TNyxJoin<Integer, Integer, Integer, TNyxText>.Inner(LLeft, LRight, LKey, LKey, LPair, NewNyxIntegerEquality);
  LCursor := LResult.GetEnumerator;
  LOther := LResult.GetEnumerator;
  Check(GKeys = 0, 'join construction/acquisition enters no source/key callback');
  Check(LResult.Take(0).Count = 0, 'zero take avoids join materialization');
  LResult := nil;
  Check(LCursor.MoveNext and (LCursor.Current = '1:1'), 'inner join yields first stable pair');
  Check(LOther.MoveNext and (LOther.Current = '1:1'), 'independent join cursor starts from its own lookup');
  LTexts := TNyxJoin<Integer, Integer, Integer, TNyxText>.Inner(LLeft, LRight, LKey, LKey, LPair, NewNyxIntegerEquality).ToArray;
  Check(Length(LTexts) = 4, 'duplicate left/right keys emit Cartesian multiplicity');
  LTexts := TNyxJoin<Integer, Integer, Integer, TNyxText>.Left(LLeft, LRight, LKey, LKey, LOptional, NewNyxIntegerEquality).ToArray;
  Check((Length(LTexts) = 6) and (LTexts[4] = '2:-') and (LTexts[5] = '4:-'), 'left join retains unmatched source rows in place');
  LTexts := TNyxJoin<Integer, Integer, Integer, TNyxText>.Right(LLeft, LRight, LKey, LKey, LOptional, NewNyxIntegerEquality).ToArray;
  Check((Length(LTexts) = 6) and (LTexts[4] = '-:3') and (LTexts[5] = '-:0'), 'right join reverses driving order while retaining partner identities');
  LTexts := TNyxJoin<Integer, Integer, Integer, TNyxText>.Full(LLeft, LRight, LKey, LKey, LOptional, NewNyxIntegerEquality).ToArray;
  Check((Length(LTexts) = 8) and (LTexts[4] = '2:-') and (LTexts[6] = '-:3') and (LTexts[7] = '-:0'), 'full join appends unmatched right identities in right order');
  LTexts := TNyxJoin<Integer, Integer, Integer, TNyxText>.Full(TNyxSequence<Integer>.Empty, LRight, LKey, LKey, LOptional, NewNyxIntegerEquality).ToArray;
  Check((Length(LTexts) = 4) and (LTexts[0] = '-:1'), 'full join admits right-only input');
  LTexts := TNyxJoin<Integer, Integer, Integer, TNyxText>.Left(TLateSource.Create,
    TNyxSequence<Integer>.Empty, LKey, LKey, LOptional, NewNyxIntegerEquality).Take(1).ToArray;
  Check((Length(LTexts) = 1) and (LTexts[0] = '9:-'), 'unmatched yield does not advance a later failing source prematurely');
  LContext := TNyxJoinValue<Integer, Integer>.Create(0, 0, True, False);
  Check(LContext.HasLeft and (LContext.Left = 0) and not LContext.HasRight, 'presence distinguishes an actual zero from absence');
  LRefused := False;
  try
    LAbsent := LContext.Right;
    Check(LAbsent = 0, 'absent getter unexpectedly returned');
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'absent partner getter refuses rather than returns a fake value');
  LCursor := TNyxJoin<Integer, Integer, Integer, TNyxText>.Left(LLeft, LRight, LKey, LKey,
    TNyxSelector<TIntJoin, TNyxText>.From(FailResult), NewNyxIntegerEquality).GetEnumerator;
  LRefused := False;
  try
    LCursor.MoveNext;
  except
    on LException: ENyxQuery do
    begin
      LRefused := Pos('Result failed', LException.Message) > 0;
    end;
  end;
  Check(LRefused and not LCursor.MoveNext, 'join result failure retains its original exception and terminal cursor');
  LTexts := TNyxGroupJoin<Integer, Integer, Integer, TNyxText>.From(LLeft, LRight, LKey, LKey,
    TNyxSelector<TIntGroupJoin, TNyxText>.From(GroupText), NewNyxIntegerEquality).ToArray;
  Check((Length(LTexts) = 4) and (LTexts[0] = '1=2') and (LTexts[2] = '2=0') and (LTexts[3] = '4=0'), 'group join yields one owned group per outer row, including empty groups');
  LLeft := nil;
  LRight := nil;
  LCursor := nil;
  LOther := nil;
  Check((GRetainedMatches.Count = 2) and (GRetainedMatches.First = 1), 'retained group-join matches outlive source and join cursors');
  GRetainedMatches := nil;
end;

procedure ManagedGroups;
var
  LCounter: TLifetime;
  LValue: IInterface;
  LRead: IInterface;
  LRows: TNyxSequenceArray<IInterface>;
  LSource: INyxSequence<IInterface>;
  LLookup: INyxLookup<Integer, IInterface>;
  LGroup: INyxGroup<Integer, IInterface>;
  LValues: INyxSequence<IInterface>;
begin
  LCounter := TLifetime.Create;
  try
    LValue := TManaged.Create(LCounter);
    SetLength(LRows, 1);
    LRows[0] := LValue;
    LSource := TNyxSequence<IInterface>.From(LRows);
    LRows := nil;
    LLookup := TNyxLookup<IInterface, Integer, IInterface>.From(LSource,
      TNyxSelector<IInterface, Integer>.From(ConstantObjectKey),
      TNyxSelector<IInterface, IInterface>.From(ObjectIdentity), NewNyxIntegerEquality);
    LGroup := LLookup.GroupFor(0);
    LValues := LGroup.Values;
    LValue := nil;
    LSource := nil;
    LLookup := nil;
    LGroup := nil;
    Check(LCounter.Destroyed = 0, 'group values independently retain managed elements after lookup/source release');
    LRead := LValues.First;
    LValues := nil;
    Check((LRead <> nil) and (LCounter.Destroyed = 0), 'independent group reader owns its managed result');
    LRead := nil;
    Check(LCounter.Destroyed = 1, 'last managed group reader retires its element once');
  finally
    LRead := nil;
    LValues := nil;
    LGroup := nil;
    LLookup := nil;
    LSource := nil;
    LValue := nil;
    LRows := nil;
    LCounter.Free;
  end;
end;

{ Qualify the new factories with the caller's named stack-frame procedure
  variable as well as compiler-selected inline syntax. Closing the lease must
  prevent a deferred grouping from entering its captured frame. }
procedure BoundariesAndCallbacks;
var
  LSource: INyxSequence<Integer>;
  LEmpty: INyxSequence<Integer>;
  LKey: INyxSelector<Integer, Integer>;
  LElement: INyxSelector<Integer, Integer>;
  LGroups: INyxSequence<TIntGroup>;
  LLookup: INyxLookup<Integer, Integer>;
  LCursor: INyxEnumerator<TIntGroup>;
  LTexts: INyxSequence<TNyxText>;
  LLease: INyxCallbackLease;
  LVariable: TNyxSelectorNested<Integer, Integer>;
  LOffset: Integer;
  LRefused: Boolean;

  function NestedKey(const AValue: Integer): Integer;
  begin
    Result := (AValue mod 2) + LOffset;
  end;

begin
  LSource := TNyxSequence<Integer>.From([1, 2, 3]);
  LEmpty := TNyxSequence<Integer>.Empty;
  LKey := TNyxSelector<Integer, Integer>.From(ExactKey);
  LElement := TNyxSelector<Integer, Integer>.From(Identity);
  LLookup := TNyxLookup<Integer, Integer, Integer>.From(LEmpty, LKey, LElement, NewNyxIntegerEquality);
  Check((LLookup.Count = 0) and (LLookup.ItemCount = 0) and not LLookup.Has(0)
    and not LLookup.Groups.Any, 'empty lookup has no phantom groups or values');
  LGroups := TNyxGroupBy<Integer, Integer, Integer>.From(LEmpty, LKey, LElement, NewNyxIntegerEquality);
  Check(not LGroups.Any, 'empty deferred grouping reaches ordinary EOF');
  LRefused := False;
  try
    LGroups := TNyxGroupBy<Integer, Integer, Integer>.From(nil, LKey, LElement, NewNyxIntegerEquality);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'nil grouping source refuses at construction');
  LCursor := TNyxGroupBy<Integer, Integer, Integer>.From(TNilCursorSource.Create,
    LKey, LElement, NewNyxIntegerEquality).GetEnumerator;
  LRefused := False;
  try
    LCursor.MoveNext;
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LCursor.MoveNext, 'nil source cursor refuses and stays terminal');
  LCursor := TNyxGroupBy<Integer, Integer, Integer>.From(TLateSource.Create,
    LKey, LElement, NewNyxIntegerEquality).GetEnumerator;
  LRefused := False;
  try
    LCursor.MoveNext;
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LCursor.MoveNext, 'late source failure publishes no partial groups');
  LTexts := TNyxJoin<Integer, Integer, Integer, TNyxText>.Inner(LEmpty,
    TLateSource.Create, LKey, LKey, TNyxSelector<TIntPair, TNyxText>.From(PairText), NewNyxIntegerEquality);
  Check(not LTexts.Any, 'empty driving inner join never enters the failing right source');
  LTexts := TNyxJoin<Integer, Integer, Integer, TNyxText>.Full(LEmpty,
    LEmpty, LKey, LKey, TNyxSelector<TIntJoin, TNyxText>.From(OptionalText), NewNyxIntegerEquality);
  Check(not LTexts.Any, 'both-empty full join has no fabricated partner');
  LOffset := 10;
  LLease := NewNyxCallbackLease;
  try
    LVariable := NestedKey;
    LKey := TNyxSelector<Integer, Integer>.FromNested(LVariable, LLease);
    LGroups := TNyxGroupBy<Integer, Integer, Integer>.From(LSource, LKey, LElement, NewNyxIntegerEquality);
    Check((LGroups.First.Key = 11) and (LGroups.Count = 2), 'named nested key callback preserves its typed scope');
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    LGroups := TNyxGroupBy<Integer, Integer, Integer>.From(LSource,
      TNyxSelector<Integer, Integer>.FromInline(
        function(const AValue: Integer): Integer
        begin
          Result := (AValue mod 2) + LOffset;
        end), LElement, NewNyxIntegerEquality);
    Check((LGroups.First.Key = 11) and (LGroups.Count = 2), 'qualified inline grouping key preserves scalar capture');
    LTexts := TNyxJoin<Integer, Integer, Integer, TNyxText>.Inner(LSource, LSource,
      LKey, LKey, TNyxSelector<TIntPair, TNyxText>.FromInline(
        function(const AValue: TIntPair): TNyxText
        begin
          Result := TNyxText(IntToStr(AValue.Outer + AValue.Inner + LOffset));
        end), NewNyxIntegerEquality);
    Check((LTexts.Count = 5) and (LTexts.First = '12'), 'qualified inline join result uses specialized partners and capture');
    {$endif}
    LGroups := TNyxGroupBy<Integer, Integer, Integer>.From(LSource, LKey, LElement, NewNyxIntegerEquality);
  finally
    LLease.Close;
  end;
  LRefused := False;
  try
    LGroups.Any;
  except
    on ENyxCallbackExpired do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'closed nested grouping key refuses deferred frame entry');
end;

procedure ManagedJoins;
var
  LCounter: TLifetime;
  LValue: IInterface;
  LRead: IInterface;
  LRows: TNyxSequenceArray<IInterface>;
  LSource: INyxSequence<IInterface>;
  LQuery: INyxSequence<Integer>;
  LValues: TNyxSequenceArray<Integer>;
  LContext: INyxJoinValue<IInterface, Integer>;
begin
  LCounter := TLifetime.Create;
  try
    LValue := TManaged.Create(LCounter);
    SetLength(LRows, 1);
    LRows[0] := LValue;
    LSource := TNyxSequence<IInterface>.From(LRows);
    LRows := nil;
    LQuery := TNyxJoin<IInterface, Integer, Integer, Integer>.Left(LSource,
      TNyxSequence<Integer>.From([0]), TNyxSelector<IInterface, Integer>.From(ConstantObjectKey),
      TNyxSelector<Integer, Integer>.From(ExactKey),
      TNyxSelector<TObjectJoin, Integer>.From(RetainPartner), NewNyxIntegerEquality);
    LValues := LQuery.ToArray;
    LValue := nil;
    LSource := nil;
    LQuery := nil;
    Check((Length(LValues) = 1) and (LValues[0] = 0) and (LCounter.Destroyed = 0),
      'heterogeneous retained join context independently owns managed left payload');
    LRead := GRetainedPartner.Left;
    GRetainedPartner := nil;
    Check((LRead <> nil) and (LCounter.Destroyed = 0), 'join partner getter returns an independently owned interface');
    LRead := nil;
    Check(LCounter.Destroyed = 1, 'last join reader retires its managed partner once');
    LValue := TManaged.Create(LCounter);
    LContext := TNyxJoinValue<IInterface, Integer>.Create(LValue, 0, False, True);
    LValue := nil;
    Check((LCounter.Destroyed = 2) and not LContext.HasLeft and LContext.HasRight,
      'absent partner does not retain a supplied managed payload');
    LContext := TNyxJoinValue<IInterface, Integer>.Create(nil, 0, True, True);
    LRead := LContext.Left;
    Check(LContext.HasLeft and (LRead = nil), 'nil interface can still be an explicitly present partner');
  finally
    GRetainedPartner := nil;
    LContext := nil;
    LRead := nil;
    LValue := nil;
    LRows := nil;
    LSource := nil;
    LQuery := nil;
    LCounter.Free;
  end;
end;

function RunNyxJoinChecks: Integer;
begin
  GChecks := 0;
  try
    Grouping;
    EqualityAndLarge;
    Joins;
    ManagedGroups;
    BoundariesAndCallbacks;
    ManagedJoins;
    Result := GChecks;
  finally
    GRetainedMatches := nil;
    GRetainedPartner := nil;
  end;
end;

end.
