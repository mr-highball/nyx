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

{ Private source-reader/reconciler index. Keys and positions are owned per
  operation; nothing borrows a document, token array or renderer. Exact text
  equality resolves hash collisions. Pascal identifiers are normalized by their
  callers, while application identities remain exact, including empty keys.
  The table never determines output order: original ordered arrays own that. }
type
  TSourceIndexEntry = record
    Key: TNyxText;
    Value: Integer;
    Occupied: Boolean;
  end;
  TSourceIndexEntries = array of TSourceIndexEntry;
  TSourceIndex = class
  private
    FEntries: TSourceIndexEntries;
    FCount: Integer;
    function Slot(const AKey: TNyxText): Integer;
    procedure Grow;
  public
    { Missing keys return -1. AddFirst retains the original mapping on duplicate
      keys, matching the former ordered scan. Values must be nonnegative. }
    function IndexOf(const AKey: TNyxText): Integer;
    procedure AddFirst(const AKey: TNyxText; AValue: Integer);
  end;

function TSourceIndex.Slot(const AKey: TNyxText): Integer;
var
  LHash: Int64;
  LIndex: Integer;
begin
  LHash := 0;
  for LIndex := 1 to Length(AKey) do
  begin
    { Bound the arithmetic explicitly under checked FPC and JavaScript's exact
      integer range. Target storage encodings may hash differently; keys are
      never persisted and every lookup uses the same target's exact text. }
    LHash := (LHash * 31 + Ord(AKey[LIndex])) mod 2147483647;
  end;
  Result := Integer(LHash mod Length(FEntries));
  while FEntries[Result].Occupied and (FEntries[Result].Key <> AKey) do
  begin
    Inc(Result);

    if Result = Length(FEntries) then
    begin
      Result := 0;
    end;
  end;
end;

procedure TSourceIndex.Grow;
var
  LPrevious: TSourceIndexEntries;
  LCapacity: Integer;
  LIndex: Integer;
begin
  LPrevious := FEntries;
  LCapacity := Length(LPrevious) * 2;

  if LCapacity = 0 then
  begin
    LCapacity := 16;
  end;
  { Detach before writing: dynamic arrays may share their managed records. }
  FEntries := nil;
  SetLength(FEntries, LCapacity);
  FCount := 0;
  for LIndex := 0 to Length(LPrevious) - 1 do
  begin

    if LPrevious[LIndex].Occupied then
    begin
      AddFirst(LPrevious[LIndex].Key, LPrevious[LIndex].Value);
    end;
  end;
end;

function TSourceIndex.IndexOf(const AKey: TNyxText): Integer;
var
  LSlot: Integer;
begin
  Result := -1;

  if Length(FEntries) = 0 then
  begin
    Exit;
  end;
  LSlot := Slot(AKey);

  if FEntries[LSlot].Occupied then
  begin
    Result := FEntries[LSlot].Value;
  end;
end;

procedure TSourceIndex.AddFirst(const AKey: TNyxText; AValue: Integer);
var
  LSlot: Integer;
begin

  if AValue < 0 then
  begin
    raise EArgumentException.Create('Source index positions must be nonnegative');
  end;

  if FCount * 2 >= Length(FEntries) then
  begin
    Grow;
  end;
  LSlot := Slot(AKey);

  if not FEntries[LSlot].Occupied then
  begin
    FEntries[LSlot].Key := AKey;
    FEntries[LSlot].Value := AValue;
    FEntries[LSlot].Occupied := True;
    Inc(FCount);
  end;
end;
