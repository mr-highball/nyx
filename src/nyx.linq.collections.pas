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
unit nyx.linq.collections;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch typehelpers}

interface

uses
  nyx.text,
  nyx.collections,
  nyx.linq;

type
  { Optional extension-method syntax for portable immutable snapshots. Calling
    AsNyx captures owned item values at that revision; callbacks remain deferred.
    The sequence neither subscribes to nor mutates the originating live store. }
  TNyxCollectionSnapshotHelper = type helper for INyxCollectionSnapshot
    function AsNyx: INyxSequence<TNyxCollectionItem>;
  end;
  { Owned text storage participates without any ANSI TStrings conversion. }
  TNyxStringsQueryHelper = class helper for TNyxStrings
    function AsNyx: INyxSequence<TNyxText>;
  end;

{ Retain exact typed row identities/values after later edits or store disposal.
  Nil or invalid reported counts refuse. The snapshot interface promises admitted
  immutable rows; this query entry point is not a replacement for whole-snapshot
  validation at a persistence or foreign collection admission boundary. }
function NyxItems(const ASnapshot: INyxCollectionSnapshot): INyxSequence<TNyxCollectionItem>;
{ Immediate detached capture. The caller owns/synchronizes the strings object;
  subsequent edits/disposal cannot change the returned query's exact text. }
function NyxTextItems(AValues: TNyxStrings): INyxSequence<TNyxText>;

implementation

function NyxTextItems(AValues: TNyxStrings): INyxSequence<TNyxText>;
var
  LValues: TNyxSequenceArray<TNyxText>;
  LIndex: Integer;
begin

  if AValues = nil then
  begin
    raise ENyxQuery.Create('Text query capture requires owned strings');
  end;
  SetLength(LValues, AValues.Count);
  for LIndex := 0 to High(LValues) do
  begin
    LValues[LIndex] := AValues[LIndex];
  end;
  Result := TNyxSequence<TNyxText>.From(LValues);
end;

function TNyxStringsQueryHelper.AsNyx: INyxSequence<TNyxText>;
begin
  Result := NyxTextItems(Self);
end;

function NyxItems(const ASnapshot: INyxCollectionSnapshot): INyxSequence<TNyxCollectionItem>;
var
  LItems: TNyxSequenceArray<TNyxCollectionItem>;
  LCount: Integer;
  LIndex: Integer;
begin

  if ASnapshot = nil then
  begin
    raise ENyxQuery.Create('A collection query requires a snapshot');
  end;
  LCount := ASnapshot.Count;

  if (LCount < 0) or (LCount > NyxMaximumCollectionItems) then
  begin
    raise ENyxQuery.Create('Collection query snapshot count is outside its contract');
  end;
  SetLength(LItems, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LItems[LIndex] := ASnapshot.ItemAt(LIndex);
  end;
  Result := TNyxSequence<TNyxCollectionItem>.From(LItems);
end;

function TNyxCollectionSnapshotHelper.AsNyx: INyxSequence<TNyxCollectionItem>;
begin
  Result := NyxItems(Self);
end;

end.
