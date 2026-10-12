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

unit nyx.linq.native;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch typehelpers}

interface

{$ifndef PAS2JS}
uses
  Generics.Collections, nyx.text, nyx.linq, nyx.linq.aggregates;

type
  { Native enumeration and stack pop order are different on qualified FPC.
    Caller choice is explicit; no items are removed during either capture. }
  TNyxStackOrder = (nsoNativeEncounter, nsoPopOrder);
  { Immediate detached capture from any native RTL enumerable, including list/
    queue/stack. The caller owns the container and synchronizes capture against
    concurrent mutation. Returned queries retain no RTL cursor/container; managed
    elements retain normally and raw object elements remain borrowed. }
  TNyxNativeQuery<T> = class
  public
    class function FromEnumerable(AValues: TEnumerable<T>): INyxSequence<T>; static;
    class function FromStack(AValues: TStack<T>;
      AOrder: TNyxStackOrder = nsoPopOrder): INyxSequence<T>; static;
    { Caller owns/frees the returned mutable RTL list. Its vector is independent;
      managed elements retain normally and raw objects remain borrowed. }
    class function ToRTLList(const AValues: INyxSequence<T>): TList<T>; static;
  end;
  TNyxNativeDictionary<TKey, TValue> = class
  public
    { Capture actual dictionary enumeration order, not an invented insertion
      order. Each entry is an immutable managed key/value object on return. }
    class function FromDictionary(AValues: TDictionary<TKey, TValue>):
      INyxSequence<INyxKeyValue<TKey, TValue>>; static;
  end;

  { Generic helpers are unsupported by the qualified FPC syntax probe.
    Common native specializations get C#-like extension calls; arbitrary types
    use the generic capture factory or these RTL-derived opt-in containers. }
  TNyxQueryList<T> = class(TList<T>)
    function AsNyx: INyxSequence<T>;
  end;
  TNyxQueryQueue<T> = class(TQueue<T>)
    function AsNyx: INyxSequence<T>;
  end;
  TNyxQueryStack<T> = class(TStack<T>)
    function AsNyx(AOrder: TNyxStackOrder = nsoPopOrder): INyxSequence<T>;
  end;
  TNyxIntegerList = TList<Integer>;
  TNyxTextList = TList<TNyxText>;
  TNyxIntegerQueue = TQueue<Integer>;
  TNyxIntegerStack = TStack<Integer>;
  TNyxIntegerListHelper = class helper for TNyxIntegerList
    function AsNyx: INyxSequence<Integer>;
  end;
  TNyxTextListHelper = class helper for TNyxTextList
    function AsNyx: INyxSequence<TNyxText>;
  end;
  TNyxIntegerQueueHelper = class helper for TNyxIntegerQueue
    function AsNyx: INyxSequence<Integer>;
  end;
  TNyxIntegerStackHelper = class helper for TNyxIntegerStack
    function AsNyx(AOrder: TNyxStackOrder = nsoPopOrder): INyxSequence<Integer>;
  end;
{$endif}

implementation

{$ifndef PAS2JS}
class function TNyxNativeQuery<T>.FromEnumerable(AValues: TEnumerable<T>): INyxSequence<T>;
var
  LCursor: Generics.Collections.TEnumerator<T>;
  LBuffer: TQueryBuffer<T>;
  LValue: T;
  LValues: TNyxSequenceArray<T>;
begin

  if AValues = nil then
  begin
    raise ENyxQuery.Create('Native query capture requires a container');
  end;
  LBuffer := TQueryBuffer<T>.Create(High(Integer));
  LCursor := nil;
  try
    LCursor := AValues.GetEnumerator;

    if LCursor = nil then
    begin
      raise ENyxQuery.Create('Native container returned no enumerator');
    end;
    while LCursor.MoveNext do
    begin
      LValue := LCursor.Current;
      LBuffer.Add(LValue);
    end;
    LValues := LBuffer.ToArray;
    Result := TNyxSequence<T>.From(LValues);
  finally
    LCursor.Free;
    LBuffer.Free;
  end;
end;

class function TNyxNativeQuery<T>.FromStack(AValues: TStack<T>;
  AOrder: TNyxStackOrder): INyxSequence<T>;
var
  LCaptured: INyxSequence<T>;
begin

  if AValues = nil then
  begin
    raise ENyxQuery.Create('Stack query capture requires a container');
  end;

  if (Ord(AOrder) < Ord(Low(TNyxStackOrder))) or
    (Ord(AOrder) > Ord(High(TNyxStackOrder))) then
  begin
    raise ENyxQuery.Create('Stack capture requires a valid order');
  end;

  if AOrder = nsoNativeEncounter then
  begin
    Exit(FromEnumerable(AValues));
  end;
  { Qualified native RTL stores stack items bottom-first in ToArray. Detached
    reversal gives pop order without Pop/Push mutation or retaining the stack. }
  LCaptured := TNyxSequence<T>.From(AValues.ToArray);
  Result := LCaptured.Reverse;
end;

class function TNyxNativeQuery<T>.ToRTLList(const AValues: INyxSequence<T>): TList<T>;
var
  LCursor: INyxEnumerator<T>;
  LCandidate: TList<T>;
  LValue: T;
begin

  if AValues = nil then
  begin
    raise ENyxQuery.Create('Native list materialization requires a source');
  end;
  LCandidate := TList<T>.Create;
  try
    LCursor := AValues.GetEnumerator;

    if LCursor = nil then
    begin
      raise ENyxQuery.Create('Native list source returned no cursor');
    end;
    while LCursor.MoveNext do
    begin
      LValue := LCursor.Current;
      LCandidate.Add(LValue);
    end;
    Result := LCandidate;
    LCandidate := nil;
  finally
    LCandidate.Free;
  end;
end;

class function TNyxNativeDictionary<TKey, TValue>.FromDictionary(
  AValues: TDictionary<TKey, TValue>): INyxSequence<INyxKeyValue<TKey, TValue>>;
type
  TEntry = INyxKeyValue<TKey, TValue>;
var
  LCursor: Generics.Collections.TEnumerator<TPair<TKey, TValue>>;
  LBuffer: TQueryBuffer<TEntry>;
  LPair: TPair<TKey, TValue>;
  LEntry: TEntry;
  LEntries: TNyxSequenceArray<TEntry>;
begin

  if AValues = nil then
  begin
    raise ENyxQuery.Create('Native dictionary capture requires a container');
  end;
  LBuffer := TQueryBuffer<TEntry>.Create(High(Integer));
  LCursor := nil;
  try
    LCursor := AValues.GetEnumerator;

    if LCursor = nil then
    begin
      raise ENyxQuery.Create('Native dictionary returned no enumerator');
    end;
    while LCursor.MoveNext do
    begin
      LPair := LCursor.Current;
      LEntry := TNyxKeyValue<TKey, TValue>.Create(LPair.Key, LPair.Value);
      LBuffer.Add(LEntry);
    end;
    LEntries := LBuffer.ToArray;
    Result := TNyxSequence<TEntry>.From(LEntries);
  finally
    LCursor.Free;
    LBuffer.Free;
  end;
end;

function TNyxQueryList<T>.AsNyx: INyxSequence<T>;
begin
  Result := TNyxNativeQuery<T>.FromEnumerable(Self);
end;

function TNyxQueryQueue<T>.AsNyx: INyxSequence<T>;
begin
  Result := TNyxNativeQuery<T>.FromEnumerable(Self);
end;

function TNyxQueryStack<T>.AsNyx(AOrder: TNyxStackOrder): INyxSequence<T>;
begin
  Result := TNyxNativeQuery<T>.FromStack(Self, AOrder);
end;

function TNyxIntegerListHelper.AsNyx: INyxSequence<Integer>;
begin
  Result := TNyxNativeQuery<Integer>.FromEnumerable(Self);
end;

function TNyxTextListHelper.AsNyx: INyxSequence<TNyxText>;
begin
  Result := TNyxNativeQuery<TNyxText>.FromEnumerable(Self);
end;

function TNyxIntegerQueueHelper.AsNyx: INyxSequence<Integer>;
begin
  Result := TNyxNativeQuery<Integer>.FromEnumerable(Self);
end;

function TNyxIntegerStackHelper.AsNyx(AOrder: TNyxStackOrder): INyxSequence<Integer>;
begin
  Result := TNyxNativeQuery<Integer>.FromStack(Self, AOrder);
end;
{$endif}

end.
