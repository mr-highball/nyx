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
unit nyx.linq.random;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.linq;

type
  { Caller-controlled bounded randomness. NextBelow must return [0, Exclusive).
    Exclusive is positive. A cursor exclusively owns its generator; mutable RNGs
    must not be shared between concurrent consumers without caller coordination. }
  INyxRandom = interface(IInterface)
    function NextBelow(AExclusive: Integer): Integer;
  end;
  INyxRandomFactory = interface(IInterface)
    { Return a fresh independently owned generator for each enumeration. }
    function NewRandom: INyxRandom;
  end;
  TNyxShuffle<T> = class
  public
    { Deferred full admission and Fisher-Yates permutation. No global RTL Random,
      seed, clock or input mutation. Source admits completely before RNG entry;
      each cursor then receives its own generator. Zero/one rows need no RNG.
      Invalid/nil RNGs refuse; original source/factory/RNG failures are terminal.
      Owned results retain managed values and ordinary borrowed-object semantics. }
    class function From(const ASource: INyxSequence<T>;
      const ARandom: INyxRandomFactory): INyxSequence<T>; static;
  end;
  TQuerySeededRandom = class(TInterfacedObject, INyxRandom)
  private
    FState: Cardinal;
  public
    constructor Create(ASeed: Cardinal);
    function NextBelow(AExclusive: Integer): Integer;
  end;
  TQueryRandomFactory = class(TInterfacedObject, INyxRandomFactory)
  private
    FSeed: Cardinal;
  public
    constructor Create(ASeed: Cardinal);
    function NewRandom: INyxRandom;
  end;
  TQueryShuffleCursor<T> = class(TEnumerator<T>)
  private
    FSource: INyxSequence<T>;
    FFactory: INyxRandomFactory;
    FValues: TNyxSequenceArray<T>;
    FIndex: Integer;
    FPrepared: Boolean;
    procedure Prepare;
  protected
    function Advance(out AValue: T): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<T>; const ARandom: INyxRandomFactory);
  end;
  TQueryShuffleSequence<T> = class(TNyxSequence<T>)
  private
    FSource: INyxSequence<T>;
    FFactory: INyxRandomFactory;
  public
    constructor Create(const ASource: INyxSequence<T>; const ARandom: INyxRandomFactory);
    function GetEnumerator: INyxEnumerator<T>; override;
  end;

{ Deterministic portable xorshift32 generator factory, independent per cursor.
  Zero maps to a fixed nonzero seed; no global random state changes. Rejection
  sampling avoids modulo bias. This is repeatable application randomness, not
  cryptographic randomness; callers can supply their own INyxRandomFactory. }
function NyxSeededRandom(ASeed: Cardinal): INyxRandomFactory;

implementation

constructor TQuerySeededRandom.Create(ASeed: Cardinal);
begin
  inherited Create;
  FState := ASeed;

  if FState = 0 then
  begin
    FState := $6d2b79f5;
  end;
end;

function TQuerySeededRandom.NextBelow(AExclusive: Integer): Integer;
var
  LLimit: Cardinal;
  LSample: Cardinal;
begin

  if AExclusive <= 0 then
  begin
    raise ENyxQuery.Create('Random bound must be positive');
  end;
  { Nonzero xorshift state visits 1..$ffffffff. Subtract one to obtain a uniform
    span of $ffffffff samples; discard its incomplete final modulo bucket. All
    arithmetic stays within Cardinal and exact browser integers below 2^32. }
  LLimit := High(Cardinal) - (High(Cardinal) mod Cardinal(AExclusive));
  repeat
    FState := FState xor (FState shl 13);
    FState := FState xor (FState shr 17);
    FState := FState xor (FState shl 5);
    LSample := FState - 1;
  until LSample < LLimit;
  Result := Integer(LSample mod Cardinal(AExclusive));
end;

constructor TQueryRandomFactory.Create(ASeed: Cardinal);
begin
  inherited Create;
  FSeed := ASeed;
end;

function TQueryRandomFactory.NewRandom: INyxRandom;
begin
  Result := TQuerySeededRandom.Create(FSeed);
end;

function NyxSeededRandom(ASeed: Cardinal): INyxRandomFactory;
begin
  Result := TQueryRandomFactory.Create(ASeed);
end;

constructor TQueryShuffleCursor<T>.Create(const ASource: INyxSequence<T>;
  const ARandom: INyxRandomFactory);
begin
  inherited Create;
  FSource := ASource;
  FFactory := ARandom;
end;

procedure TQueryShuffleCursor<T>.Prepare;
var
  LBuffer: TQueryBuffer<T>;
  LCursor: INyxEnumerator<T>;
  LRandom: INyxRandom;
  LIndex: Integer;
  LOther: Integer;
  LValue: T;
begin
  FPrepared := True;
  LCursor := FSource.GetEnumerator;
  FSource := nil;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Shuffle source returned no cursor');
  end;
  LBuffer := TQueryBuffer<T>.Create(High(Integer));
  try
    while LCursor.MoveNext do
    begin
      LBuffer.Add(LCursor.Current);
    end;
    FValues := LBuffer.ToArray;
  finally
    LBuffer.Free;
  end;
  LCursor := nil;

  if Length(FValues) > 1 then
  begin
    LRandom := FFactory.NewRandom;

    if LRandom = nil then
    begin
      raise ENyxQuery.Create('Shuffle factory returned no random generator');
    end;
    for LIndex := Length(FValues) - 1 downto 1 do
    begin
      LOther := LRandom.NextBelow(LIndex + 1);

      if (LOther < 0) or (LOther > LIndex) then
      begin
        raise ENyxQuery.Create('Shuffle random result exceeds its requested bound');
      end;
      LValue := FValues[LIndex];
      FValues[LIndex] := FValues[LOther];
      FValues[LOther] := LValue;
    end;
  end;
  FFactory := nil;
end;

function TQueryShuffleCursor<T>.Advance(out AValue: T): Boolean;
begin
  Result := False;
  try

    if not FPrepared then
    begin
      Prepare;
    end;
    Result := FIndex < Length(FValues);

    if Result then
    begin
      AValue := FValues[FIndex];
      Inc(FIndex);
    end
    else
    begin
      FValues := nil;
    end;
  except
    FSource := nil;
    FFactory := nil;
    FValues := nil;
    raise;
  end;
end;

constructor TQueryShuffleSequence<T>.Create(const ASource: INyxSequence<T>;
  const ARandom: INyxRandomFactory);
begin
  inherited Create;
  FSource := ASource;
  FFactory := ARandom;
end;

function TQueryShuffleSequence<T>.GetEnumerator: INyxEnumerator<T>;
begin
  Result := TQueryShuffleCursor<T>.Create(FSource, FFactory);
end;

class function TNyxShuffle<T>.From(const ASource: INyxSequence<T>;
  const ARandom: INyxRandomFactory): INyxSequence<T>;
begin

  if (ASource = nil) or (ARandom = nil) then
  begin
    raise ENyxQuery.Create('Shuffle requires a source and owned random factory');
  end;
  Result := TQueryShuffleSequence<T>.Create(ASource, ARandom);
end;

end.
