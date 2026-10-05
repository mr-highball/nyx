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

unit nyx.text.index;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text;

{ Internal exact-text index. Each caller owns its keys and nonnegative scalar
  positions; nothing borrows a document, token array or renderer. Exact text
  equality resolves hash collisions. Pascal identifiers are normalized by their
  callers, while application identities remain exact, including empty keys.
  The table never determines output order: original ordered arrays own that.
  Instances are independently owned and mutable; callers must serialize access
  or publish only a fully initialized read-only instance to other consumers. }
type
  TNyxTextIndexEntry = record
    Key: TNyxText;
    Value: Integer;
    Occupied: Boolean;
  end;
  TNyxTextIndexEntries = array of TNyxTextIndexEntry;
  TNyxTextIndex = class
  private
    FEntries: TNyxTextIndexEntries;
    FCount: Integer;
    function Slot(const AKey: TNyxText): Integer;
    procedure Grow;
  public
    { Missing keys return -1. AddFirst retains the original mapping on duplicate
      keys, matching the former ordered scan. Values must be nonnegative. }
    function IndexOf(const AKey: TNyxText): Integer;
    procedure AddFirst(const AKey: TNyxText; AValue: Integer);
  end;

implementation

uses
  SysUtils;

function TNyxTextIndex.Slot(const AKey: TNyxText): Integer;
var
  {$IFDEF PAS2JS}
  LHash: NativeInt;
  {$ELSE}
  LHash: Int64;
  {$ENDIF}
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

procedure TNyxTextIndex.Grow;
var
  LPrevious: TNyxTextIndexEntries;
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

function TNyxTextIndex.IndexOf(const AKey: TNyxText): Integer;
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

procedure TNyxTextIndex.AddFirst(const AKey: TNyxText; AValue: Integer);
var
  LSlot: Integer;
begin

  if AValue < 0 then
  begin
    raise EArgumentException.Create('Text index positions must be nonnegative');
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

end.
