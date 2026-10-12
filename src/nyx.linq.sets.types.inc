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

  { Key-based operators return the original typed source value for the first
    qualifying occurrence. UnionBy projects keys in both value inputs; the other
    IntersectBy/ExceptBy input is already a typed sequence of keys, matching the
    ordinary LINQ distinction. Owned/nested/inline selector adapters all apply.
    No source, cursor or callback enters at construction/GetEnumerator. }
  TNyxSetBy<T, TKey> = class
  public
    class function DistinctBy(const ASource: INyxSequence<T>;
      const AKey: INyxSelector<T, TKey>;
      const AEquality: INyxEquality<TKey>): INyxSequence<T>; static;
    class function UnionBy(const ASource, AOther: INyxSequence<T>;
      const AKey: INyxSelector<T, TKey>;
      const AEquality: INyxEquality<TKey>): INyxSequence<T>; static;
    class function IntersectBy(const ASource: INyxSequence<T>;
      const AKeys: INyxSequence<TKey>; const AKey: INyxSelector<T, TKey>;
      const AEquality: INyxEquality<TKey>): INyxSequence<T>; static;
    class function ExceptBy(const ASource: INyxSequence<T>;
      const AKeys: INyxSequence<TKey>; const AKey: INyxSelector<T, TKey>;
      const AEquality: INyxEquality<TKey>): INyxSequence<T>; static;
  end;

  { Compiler-visible support. This raw mutable index belongs exclusively to one
    cursor; no live collection/UI or shared enumerator is retained. Removed keys
    remain as probe witnesses until index retirement, preserving collision chains
    without tombstone accumulation. Hashes are cached during growth. }
  TQueryKeySet<TKey> = class
  private
    FKeys: TNyxSequenceArray<TKey>;
    FHashes: array of Cardinal;
    FPresent: array of Boolean;
    FSlots: array of Integer;
    FUsed: Integer;
    FCount: Integer;
    FEquality: INyxEquality<TKey>;
    procedure Grow;
    function FindSlot(const AKey: TKey; AHash: Cardinal): Integer;
  public
    constructor Create(const AEquality: INyxEquality<TKey>);
    function Add(const AKey: TKey): Boolean;
    { Stable insertion ordinal without a second hash/project pass. Used by
      per-key aggregates; removed keys keep their original ordinal on re-entry. }
    function FindOrAdd(const AKey: TKey; out AIndex: Integer): Boolean;
    { Read-only stable ordinal lookup; absent/removed keys return False/-1. }
    function TryIndexOf(const AKey: TKey; out AIndex: Integer): Boolean;
    function Remove(const AKey: TKey): Boolean;
    property Count: Integer read FCount;
  end;

  TQuerySetOperation = (qsoDistinct, qsoUnion, qsoIntersect, qsoExcept);
  TQuerySetIdentity<T> = class(TInterfacedObject, INyxSelector<T, T>)
    function Project(const AValue: T): T;
  end;
  TQuerySetCursor<T, TKey> = class(TEnumerator<T>)
  private
    FSource: INyxSequence<T>;
    FOther: INyxSequence<T>;
    FKeys: INyxSequence<TKey>;
    FKey: INyxSelector<T, TKey>;
    FEquality: INyxEquality<TKey>;
    FCursor: INyxEnumerator<T>;
    FSet: TQueryKeySet<TKey>;
    FOperation: TQuerySetOperation;
    FStarted: Boolean;
    FFirstPending: Boolean;
    FExhausted: Boolean;
    FReadingOther: Boolean;
    procedure Prepare;
    procedure Retire;
  protected
    function Advance(out AValue: T): Boolean; override;
  public
    constructor Create(const ASource, AOther: INyxSequence<T>;
      const AKeys: INyxSequence<TKey>; const AKey: INyxSelector<T, TKey>;
      const AEquality: INyxEquality<TKey>; AOperation: TQuerySetOperation);
    destructor Destroy; override;
  end;
  TQuerySetSequence<T, TKey> = class(TNyxSequence<T>)
  private
    FSource: INyxSequence<T>;
    FOther: INyxSequence<T>;
    FKeys: INyxSequence<TKey>;
    FKey: INyxSelector<T, TKey>;
    FEquality: INyxEquality<TKey>;
    FOperation: TQuerySetOperation;
  public
    constructor Create(const ASource, AOther: INyxSequence<T>;
      const AKeys: INyxSequence<TKey>; const AKey: INyxSelector<T, TKey>;
      const AEquality: INyxEquality<TKey>; AOperation: TQuerySetOperation);
    function GetEnumerator: INyxEnumerator<T>; override;
  end;
