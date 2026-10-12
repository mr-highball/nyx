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
unit nyx.linq.positions;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.linq, nyx.linq.aggregates;

type
  { A typed zero-based start offset or distance from the exclusive end. FromEnd(1)
    means the last element; FromEnd(0) is the end boundary, not an element. Negative
    offsets refuse. Create through these factories; a default record is invalid. }
  TNyxPosition = record
  private
    FOffset: Integer;
    FFromEnd: Boolean;
    FInitialized: Boolean;
  public
    class function FromStart(AOffset: Integer): TNyxPosition; static;
    class function FromEnd(ADistance: Integer): TNyxPosition; static;
    { Refuse a non-factory/default position before entering any source. }
    procedure RequireValid;
    { Resolve a slice boundary, clamped to [0, Count]. Count must be nonnegative. }
    function Boundary(ACount: Integer): Integer;
    property Offset: Integer read FOffset;
    property IsFromEnd: Boolean read FFromEnd;
  end;
  TNyxPositions<T> = class
  public
    { Immediate access. Start positions stream to their index; end positions
      retain at most the requested distance and must consume the entire source.
      FromEnd(0) is absent without source entry. False clears ordinary initialized
      output; original failures propagate. Managed results retain normally. }
    class function TryElementAt(const ASource: INyxSequence<T>;
      const APosition: TNyxPosition; out AValue: T): Boolean; static;
    { Required access refuses absence with ENyxQueryEmpty. }
    class function ElementAt(const ASource: INyxSequence<T>;
      const APosition: TNyxPosition): T; static;
    { An explicit typed fallback never treats zero/nil as absence. }
    class function ElementAtOrDefault(const ASource: INyxSequence<T>;
      const APosition: TNyxPosition; const AFallback: T): T; static;
  end;
  TNyxSlice<T> = class
  public
    { Deferred half-open [Start, End) slice, with clamped boundaries. Start-only
      bounds stream and short-circuit; an end-relative bound requires complete
      owned admission before yielding. Empty/reversed known intervals enter no
      source. Each cursor owns its storage; original failures are terminal. }
    class function Between(const ASource: INyxSequence<T>;
      const AStart, AEnd: TNyxPosition): INyxSequence<T>; static;
  end;
  { Compiler-visible implementations; raw buffers belong only to their cursor. }
  TQuerySliceCursor<T> = class(TEnumerator<T>)
  private
    FSource: INyxSequence<T>;
    FCursor: INyxEnumerator<T>;
    FStart: TNyxPosition;
    FEnd: TNyxPosition;
    FSkip: Integer;
    FRemaining: Integer;
    FPrepared: Boolean;
    FValues: TNyxSequenceArray<T>;
    FIndex: Integer;
    FStop: Integer;
    procedure Retire;
  protected
    function Advance(out AValue: T): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<T>; const AStart, AEnd: TNyxPosition);
  end;
  TQuerySliceSequence<T> = class(TNyxSequence<T>)
  private
    FSource: INyxSequence<T>;
    FStart: TNyxPosition;
    FEnd: TNyxPosition;
  public
    constructor Create(const ASource: INyxSequence<T>; const AStart, AEnd: TNyxPosition);
    function GetEnumerator: INyxEnumerator<T>; override;
  end;

implementation

class function TNyxPosition.FromStart(AOffset: Integer): TNyxPosition;
begin

  if AOffset < 0 then
  begin
    raise ENyxQuery.Create('Position offset must be nonnegative');
  end;
  Result.FOffset := AOffset;
  Result.FFromEnd := False;
  Result.FInitialized := True;
end;

class function TNyxPosition.FromEnd(ADistance: Integer): TNyxPosition;
begin
  Result := FromStart(ADistance);
  Result.FFromEnd := True;
end;

procedure TNyxPosition.RequireValid;
begin

  if not FInitialized or (FOffset < 0) then
  begin
    raise ENyxQuery.Create('Position requires a typed factory');
  end;
end;

function TNyxPosition.Boundary(ACount: Integer): Integer;
begin
  RequireValid;

  if ACount < 0 then
  begin
    raise ENyxQuery.Create('Position boundary count must be nonnegative');
  end;
  Result := FOffset;

  if Result > ACount then
  begin
    Result := ACount;
  end;

  if FFromEnd then
  begin
    Result := ACount - Result;
  end;
end;

class function TNyxPositions<T>.TryElementAt(const ASource: INyxSequence<T>;
  const APosition: TNyxPosition; out AValue: T): Boolean;
var
  LEmpty: TQueryEmptyValue<T>;
  LTail: TNyxSequenceArray<T>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Position access requires a source');
  end;
  APosition.RequireValid;

  if not APosition.IsFromEnd then
  begin
    Result := ASource.TryElementAt(APosition.Offset, AValue);
    Exit;
  end;
  { Object initialization supplies ordinary zero/nil on both targets; pas2js
    Default(Integer) alone supplies Low(Integer) instead of native zero. }
  LEmpty := TQueryEmptyValue<T>.Create;
  try
    AValue := LEmpty.Value;
  finally
    LEmpty.Free;
  end;
  Result := False;

  if APosition.Offset = 0 then
  begin
    Exit;
  end;
  LTail := ASource.TakeLast(APosition.Offset).ToArray;
  Result := Length(LTail) = APosition.Offset;

  if Result then
  begin
    AValue := LTail[0];
  end;
end;

class function TNyxPositions<T>.ElementAt(const ASource: INyxSequence<T>;
  const APosition: TNyxPosition): T;
begin

  if not TryElementAt(ASource, APosition, Result) then
  begin
    raise ENyxQueryEmpty.Create('No element exists at the requested position');
  end;
end;

class function TNyxPositions<T>.ElementAtOrDefault(const ASource: INyxSequence<T>;
  const APosition: TNyxPosition; const AFallback: T): T;
begin

  if not TryElementAt(ASource, APosition, Result) then
  begin
    Result := AFallback;
  end;
end;

constructor TQuerySliceCursor<T>.Create(const ASource: INyxSequence<T>;
  const AStart, AEnd: TNyxPosition);
begin
  inherited Create;
  FSource := ASource;
  FStart := AStart;
  FEnd := AEnd;

  if not FStart.IsFromEnd and not FEnd.IsFromEnd then
  begin
    FSkip := FStart.Offset;
    FRemaining := FEnd.Offset - FStart.Offset;
  end;
end;

procedure TQuerySliceCursor<T>.Retire;
begin
  FSource := nil;
  FCursor := nil;
  FValues := nil;
end;

function TQuerySliceCursor<T>.Advance(out AValue: T): Boolean;
var
  LBuffer: TQueryBuffer<T>;
begin
  Result := False;
  try

    if not FStart.IsFromEnd and not FEnd.IsFromEnd then
    begin

      if FRemaining = 0 then
      begin
        Retire;
        Exit;
      end;

      if FCursor = nil then
      begin
        FCursor := FSource.GetEnumerator;
        FSource := nil;

        if FCursor = nil then
        begin
          raise ENyxQuery.Create('Slice input returned no cursor');
        end;
      end;
      while FSkip > 0 do
      begin

        if not FCursor.MoveNext then
        begin
          Retire;
          Exit;
        end;
        Dec(FSkip);
      end;
      Result := FCursor.MoveNext;

      if Result then
      begin
        AValue := FCursor.Current;
        Dec(FRemaining);
      end
      else
      begin
        Retire;
      end;
      Exit;
    end;

    if not FPrepared then
    begin
      FPrepared := True;
      FCursor := FSource.GetEnumerator;
      FSource := nil;

      if FCursor = nil then
      begin
        raise ENyxQuery.Create('Slice input returned no cursor');
      end;
      LBuffer := TQueryBuffer<T>.Create(High(Integer));
      try
        while FCursor.MoveNext do
        begin
          LBuffer.Add(FCursor.Current);
        end;
        FValues := LBuffer.ToArray;
      finally
        LBuffer.Free;
      end;
      FCursor := nil;
      FIndex := FStart.Boundary(Length(FValues));
      FStop := FEnd.Boundary(Length(FValues));
    end;
    Result := FIndex < FStop;

    if Result then
    begin
      AValue := FValues[FIndex];
      Inc(FIndex);
    end
    else
    begin
      Retire;
    end;
  except
    Retire;
    raise;
  end;
end;

constructor TQuerySliceSequence<T>.Create(const ASource: INyxSequence<T>;
  const AStart, AEnd: TNyxPosition);
begin
  inherited Create;
  FSource := ASource;
  FStart := AStart;
  FEnd := AEnd;
end;

function TQuerySliceSequence<T>.GetEnumerator: INyxEnumerator<T>;
begin
  Result := TQuerySliceCursor<T>.Create(FSource, FStart, FEnd);
end;

class function TNyxSlice<T>.Between(const ASource: INyxSequence<T>;
  const AStart, AEnd: TNyxPosition): INyxSequence<T>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Slice requires a source');
  end;
  AStart.RequireValid;
  AEnd.RequireValid;

  if (AStart.IsFromEnd and (AStart.Offset = 0)) or
    ((AStart.IsFromEnd = AEnd.IsFromEnd) and
    (((not AStart.IsFromEnd) and (AStart.Offset >= AEnd.Offset)) or
    (AStart.IsFromEnd and (AStart.Offset <= AEnd.Offset)))) then
  begin
    Result := TNyxSequence<T>.Empty;
  end
  else
  begin
    Result := TQuerySliceSequence<T>.Create(ASource, AStart, AEnd);
  end;
end;

end.
