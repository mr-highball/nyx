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
unit nyx.linq.counts;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.linq;

type
  { Exact counts without imposing an Int64 representation on pas2js consumers.
    LongCount enumerates once, preserving source/callback failures and refusing
    signed-64-domain overflow. Predicate counts use ordinary owned filters.
    TryGetNonEnumeratedCount only queries optional metadata: it never obtains
    a cursor or invokes a query callback. Unknown counts return False and zero.
    Nil sources/predicates/cursors refuse explicitly. No count grants authority
    to mutate the source or the UI. }
  TNyxCounts<T> = class
  public
    { Consume one independent cursor; the completed result is exact on both
      targets. Unlike Count, the accumulator does not narrow to Integer. }
    class function LongCount(const ASource: INyxSequence<T>): TNyxQueryCount; overload; static;
    { Count matching rows with one invocation per consumed row. The predicate
      keeps its ordinary capture/lease contract, and failures propagate. }
    class function LongCount(const ASource: INyxSequence<T>;
      const APredicate: INyxPredicate<T>): TNyxQueryCount; overload; static;
    { Query optional metadata only. False clears the output even if a custom
      provider writes a candidate before refusing. Provider failures propagate. }
    class function TryGetNonEnumeratedCount(const ASource: INyxSequence<T>;
      out AValue: TNyxQueryCount): Boolean; static;
  end;

implementation

class function TNyxCounts<T>.LongCount(const ASource: INyxSequence<T>): TNyxQueryCount;
var
  LCursor: INyxEnumerator<T>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Wide count requires a source');
  end;
  Result := NyxQueryCount(0);
  LCursor := ASource.GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Wide count source returned no cursor');
  end;
  while LCursor.MoveNext do
  begin
    Result := Result.Incremented;
  end;
end;

class function TNyxCounts<T>.LongCount(const ASource: INyxSequence<T>;
  const APredicate: INyxPredicate<T>): TNyxQueryCount;
var
  LFiltered: INyxSequence<T>;
begin

  if (ASource = nil) or (APredicate = nil) then
  begin
    raise ENyxQuery.Create('Predicate count requires a source and predicate');
  end;
  LFiltered := ASource.Where(APredicate);
  Result := LongCount(LFiltered);
end;

class function TNyxCounts<T>.TryGetNonEnumeratedCount(const ASource: INyxSequence<T>;
  out AValue: TNyxQueryCount): Boolean;
var
  LMetadata: INyxSequenceCount;
  LCandidate: TNyxQueryCount;
begin
  AValue := NyxQueryCount(0);

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Count metadata requires a source');
  end;
  Result := Supports(ASource, INyxSequenceCount, LMetadata);

  if Result then
  begin
    Result := LMetadata.NonEnumeratedCount(LCandidate);

    if Result then
    begin
      AValue := LCandidate;
    end;
  end;
end;

end.
