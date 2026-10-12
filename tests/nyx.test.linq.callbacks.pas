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
unit nyx.test.linq.callbacks;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

interface

function RunNyxCallbackBoundaryChecks: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.linq, nyx.linq.counts, nyx.callback.scope;

type
  { The custom source makes count metadata failures observable without a huge
    allocation. Metadata refusal must not enter GetEnumerator. }
  TUnknownCount = class(TNyxSequence<Integer>, INyxSequenceCount)
  public
    function GetEnumerator: INyxEnumerator<Integer>; override;
    function NonEnumeratedCount(out AValue: TNyxQueryCount): Boolean;
  end;

var
  GChecks: Integer;
  GEntries: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Callback/count boundary: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Escape all four typed borrowed adapters from an actual departed native frame.
  Even a callback that reads its local variable must refuse before dereferencing
  the stale static link. The browser follows the same revocation contract. }
procedure RetainedNested(out APredicate: INyxPredicate<Integer>;
  out ASelector: INyxSelector<Integer, Integer>;
  out AAction: INyxAction<Integer>; out AComparer: INyxComparer<Integer>);
var
  LLease: INyxCallbackLease;
  LOffset: Integer;
  LPhase: Integer;
  LIndex: Integer;
  LRejected: Boolean;

  function Accept(const AValue: Integer): Boolean;
  begin
    Inc(GEntries);
    Result := AValue > LOffset;
  end;

  function Select(const AValue: Integer): Integer;
  begin
    Inc(GEntries);
    Result := AValue + LOffset;
  end;

  procedure Add(const AValue: Integer);
  begin
    Inc(GEntries);
    Inc(LOffset, AValue);
  end;

  function Compare(const ALeft, ARight: Integer): Integer;
  begin
    Inc(GEntries);
    Result := ALeft - ARight + LOffset;
  end;

begin
  LOffset := 10;
  LLease := NewNyxCallbackLease;
  try
    APredicate := TNyxPredicate<Integer>.FromNested(Accept, LLease);
    ASelector := TNyxSelector<Integer, Integer>.FromNested(Select, LLease);
    AAction := TNyxAction<Integer>.FromNested(Add, LLease);
    AComparer := TNyxComparer<Integer>.FromNested(Compare, LLease);
  finally
    LLease.Close;
  end;
  for LPhase := 0 to 1 do
  begin

    if LPhase = 1 then
    begin
      LLease := nil;
    end;
    for LIndex := 0 to 3 do
    begin
      LRejected := False;
      try
        case LIndex of
          0:
          begin
            APredicate := TNyxPredicate<Integer>.FromNested(Accept, LLease);
          end;
          1:
          begin
            ASelector := TNyxSelector<Integer, Integer>.FromNested(Select, LLease);
          end;
          2:
          begin
            AAction := TNyxAction<Integer>.FromNested(Add, LLease);
          end;
          3:
          begin
            AComparer := TNyxComparer<Integer>.FromNested(Compare, LLease);
          end;
        end;
      except
        on LException: ENyxCallbackExpired do
        begin
          LRejected := LPhase = 0;
        end;
        on LException: ENyxQuery do
        begin
          LRejected := LPhase = 1;
        end;
      end;
      Check(LRejected, 'closed or missing nested lease refuses at construction');
    end;
  end;
end;

procedure NestedBoundaries;
var
  LPredicate: INyxPredicate<Integer>;
  LSelector: INyxSelector<Integer, Integer>;
  LAction: INyxAction<Integer>;
  LComparer: INyxComparer<Integer>;
  LLease: INyxCallbackLease;
  LRejected: Boolean;
  LIndex: Integer;
  LValue: Integer;
  LAccepted: Boolean;
begin
  GEntries := 0;
  RetainedNested(LPredicate, LSelector, LAction, LComparer);
  for LIndex := 0 to 3 do
  begin
    LRejected := False;
    try
      case LIndex of
        0:
        begin
          LAccepted := LPredicate.Test(1);
        end;
        1:
        begin
          LValue := LSelector.Project(1);
        end;
        2:
        begin
          LAction.Execute(1);
        end;
        3:
        begin
          LValue := LComparer.Compare(1, 2);
        end;
      end;
    except
      on LException: ENyxCallbackExpired do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'departed frame refuses typed callback ' + TNyxText(IntToStr(LIndex)));
  end;
  Check(GEntries = 0, 'no departed frame callback body was entered');
  LLease := NewNyxCallbackLease;
  try
    for LIndex := 0 to 3 do
    begin
      LRejected := False;
      try
        case LIndex of
          0:
          begin
            LPredicate := TNyxPredicate<Integer>.FromNested(nil, LLease);
          end;
          1:
          begin
            LSelector := TNyxSelector<Integer, Integer>.FromNested(nil, LLease);
          end;
          2:
          begin
            LAction := TNyxAction<Integer>.FromNested(nil, LLease);
          end;
          3:
          begin
            LComparer := TNyxComparer<Integer>.FromNested(nil, LLease);
          end;
        end;
      except
        on LException: ENyxQuery do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'nil nested callback refuses at construction');
    end;
  finally
    LLease.Close;
  end;
end;

{$ifdef NYX_HAS_INLINE_CALLBACKS}
{ Owning references may safely escape their enclosing frame. Predicate/selector
  and interface capture checks already live in the ordering fixture; these add
  escaped procedure and comparer references rather than just synchronous use. }
procedure RetainedInline(out AAction: INyxAction<Integer>;
  out ARead: INyxSelector<Integer, Integer>; out AComparer: INyxComparer<Integer>);
var
  LTotal: Integer;
  LDirection: Integer;
begin
  LTotal := 7;
  LDirection := -1;
  AAction := TNyxAction<Integer>.FromInline(procedure(const AValue: Integer)
    begin
      Inc(LTotal, AValue);
    end);
  ARead := TNyxSelector<Integer, Integer>.FromInline(function(const AValue: Integer): Integer
    begin
      Result := LTotal + AValue;
    end);
  AComparer := TNyxComparer<Integer>.FromInline(
    function(const ALeft, ARight: Integer): Integer
    begin
      Result := LDirection * (ALeft - ARight);
    end);
end;

procedure InlineBoundaries;
var
  LPredicate: INyxPredicate<Integer>;
  LSelector: INyxSelector<Integer, Integer>;
  LAction: INyxAction<Integer>;
  LComparer: INyxComparer<Integer>;
  LRejected: Boolean;
  LIndex: Integer;
begin
  Check(NyxHasInlineCallbacks, 'qualified version gate enables inline references');
  RetainedInline(LAction, LSelector, LComparer);
  LAction.Execute(5);
  Check(LSelector.Project(0) = 12, 'escaped inline procedures share owned scalar capture');
  Check(LComparer.Compare(2, 3) = 1, 'escaped inline comparer retains its direction');
  for LIndex := 0 to 3 do
  begin
    LRejected := False;
    try
      case LIndex of
        0:
        begin
          LPredicate := TNyxPredicate<Integer>.FromInline(nil);
        end;
        1:
        begin
          LSelector := TNyxSelector<Integer, Integer>.FromInline(nil);
        end;
        2:
        begin
          LAction := TNyxAction<Integer>.FromInline(nil);
        end;
        3:
        begin
          LComparer := TNyxComparer<Integer>.FromInline(nil);
        end;
      end;
    except
      on LException: ENyxQuery do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'nil inline callback refuses at construction');
  end;
end;
{$endif}

function TUnknownCount.GetEnumerator: INyxEnumerator<Integer>;
begin
  Inc(GEntries);
  Result := nil;
end;

function TUnknownCount.NonEnumeratedCount(out AValue: TNyxQueryCount): Boolean;
begin
  AValue := NyxQueryCount(99);
  Result := False;
end;

function Even(const AValue: Integer): Boolean;
begin
  Inc(GEntries);
  Result := (AValue mod 2) = 0;
end;

procedure ExactCounts;
var
  LCount: TNyxQueryCount;
  LValue: Integer;
  LRejected: Boolean;
  LKnown: Boolean;
  LSource: INyxSequence<Integer>;
  LFiltered: INyxSequence<Integer>;
begin
  LCount := NyxQueryCount($ffffffff).Incremented;
  Check((LCount.LowPart = 0) and (LCount.HighPart = 1), 'wide count carries exactly');
  Check(LCount.ToText = '4294967296', 'wide decimal count crosses Cardinal');
  LCount := NyxQueryCount(1, $200000);
  Check(LCount.ToText = '9007199254740993', 'decimal count stays exact above 2^53');
  Check(not LCount.TryInteger(LValue) and (LValue = 0), 'wide count refuses narrowing');
  LCount := NyxQueryCount($ffffffff, $7fffffff);
  Check(LCount.ToText = '9223372036854775807', 'maximum count formats exactly');
  LRejected := False;
  try
    LCount := LCount.Incremented;
  except
    on LException: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'maximum count refuses overflow');
  LRejected := False;
  try
    LCount := NyxQueryCount(0, $80000000);
  except
    on LException: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'invalid high count limb refuses');
  LCount := NyxQueryCount(0);
  Check(LCount.ToText = '0', 'zero count decimal format');
  LCount := NyxQueryCount(Cardinal(High(Integer)));
  Check(LCount.TryInteger(LValue) and (LValue = High(Integer)), 'last Integer narrows exactly');
  LSource := TNyxSequence<Integer>.From([1, 2, 3, 4]);
  LCount := TNyxCounts<Integer>.LongCount(LSource);
  Check(LCount.ToText = '4', 'wide count enumerates ordinary values');
  GEntries := 0;
  LCount := TNyxCounts<Integer>.LongCount(LSource, TNyxPredicate<Integer>.From(Even));
  Check((LCount.ToText = '2') and (GEntries = 4), 'predicate count enters once per row');
  LKnown := TNyxCounts<Integer>.TryGetNonEnumeratedCount(LSource, LCount);
  Check(LKnown and
    (LCount.ToText = '4'), 'array exposes exact count metadata');
  LSource := NyxRange(0, High(Integer));
  LKnown := TNyxCounts<Integer>.TryGetNonEnumeratedCount(LSource, LCount);
  Check(LKnown and
    (LCount.ToText = '2147483647'), 'huge range count needs no enumeration');
  LSource := TNyxSequence<Integer>.RepeatValue(1, 10);
  LKnown := TNyxCounts<Integer>.TryGetNonEnumeratedCount(LSource, LCount);
  Check(LKnown and
    (LCount.ToText = '10'), 'repeat exposes exact count metadata');
  GEntries := 0;
  LFiltered := LSource.Where(TNyxPredicate<Integer>.From(Even));
  LKnown := TNyxCounts<Integer>.TryGetNonEnumeratedCount(LFiltered, LCount);
  Check(not LKnown and
    (LCount.ToText = '0') and (GEntries = 0), 'unknown filtered count does not evaluate');
  LSource := TUnknownCount.Create;
  LKnown := TNyxCounts<Integer>.TryGetNonEnumeratedCount(LSource, LCount);
  Check(not LKnown and
    (LCount.ToText = '0') and (GEntries = 0), 'metadata refusal clears output without cursor entry');
  LRejected := False;
  try
    LCount := TNyxCounts<Integer>.LongCount(LSource);
  except
    on LException: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'nil source cursor refuses wide count');
end;

function RunNyxCallbackBoundaryChecks: Integer;
begin
  GChecks := 0;
  NestedBoundaries;
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  InlineBoundaries;
  {$else}
  Check(not NyxHasInlineCallbacks, 'explicit fallback excludes inline syntax');
  {$endif}
  ExactCounts;
  Result := GChecks;
end;

end.
