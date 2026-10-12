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
unit nyx.linq.combinations;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.linq, nyx.linq.projection;

type
  { Owned immutable three-input context. Managed values retain normally; borrowed
    class pointers and inner mutable arrays keep the ordinary source contract.
    Contexts can outlive every input cursor without owning a sequence or UI. }
  INyxTripleValue<TFirst, TSecond, TThird> = interface(IInterface)
    function GetFirst: TFirst;
    function GetSecond: TSecond;
    function GetThird: TThird;
    property First: TFirst read GetFirst;
    property Second: TSecond read GetSecond;
    property Third: TThird read GetThird;
  end;

  { Zip streams corresponding rows until the shortest input ends. Construction
    and GetEnumerator enter no input. Movement is left-to-right: when a later
    input ends, earlier inputs have moved once on that terminal attempt. Later
    inputs are not entered if an earlier input is empty. No unmatched selector
    call, padding, source mutation or shared cursor occurs. Nil contracts refuse;
    original failures are terminal and retire all inputs immediately. Result
    selectors use the ordinary managed/nested/inline callback contracts. }
  TNyxZip<TLeft, TRight, TResult> = class
  public
    class function From(const ALeft: INyxSequence<TLeft>;
      const ARight: INyxSequence<TRight>;
      const AResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>):
      INyxSequence<TResult>; static;
  end;
  TNyxZipPairs<TLeft, TRight> = class
  public
    { Yield immutable typed pairs with the same shortest-input evaluation. }
    class function From(const ALeft: INyxSequence<TLeft>;
      const ARight: INyxSequence<TRight>):
      INyxSequence<INyxPairValue<TLeft, TRight>>; static;
  end;
  TNyxZipThree<TFirst, TSecond, TThird, TResult> = class
  private type
    TPair = INyxPairValue<TFirst, TSecond>;
    TInput = INyxPairValue<TPair, TThird>;
  public
    { Extend Zip's left-to-right movement to three inputs, with an owned triple
      result context. An empty third input may consume one row from each prior. }
    class function From(const AFirst: INyxSequence<TFirst>;
      const ASecond: INyxSequence<TSecond>; const AThird: INyxSequence<TThird>;
      const AResult: INyxSelector<INyxTripleValue<TFirst, TSecond, TThird>, TResult>):
      INyxSequence<TResult>; static;
  end;
  TNyxZipTriples<TFirst, TSecond, TThird> = class
  private type
    TTriple = INyxTripleValue<TFirst, TSecond, TThird>;
  public
    { Yield independently retained immutable three-input contexts. }
    class function From(const AFirst: INyxSequence<TFirst>;
      const ASecond: INyxSequence<TSecond>; const AThird: INyxSequence<TThird>):
      INyxSequence<TTriple>; static;
  end;
  TNyxIndex<T> = class
  public
    { Yield owned ordinal/value contexts. Ordinals are zero-based in this input,
      checked through High(Integer); each cursor restarts independently. }
    class function From(const ASource: INyxSequence<T>):
      INyxSequence<INyxIndexedValue<T>>; static;
  end;

  { Compiler-visible support for FPC generic expansion. Public callers use the
    factories and managed contracts above. No object here owns its creator. }
  TQueryTriple<TFirst, TSecond, TThird> = class(TInterfacedObject,
    INyxTripleValue<TFirst, TSecond, TThird>)
  private
    FFirst: TFirst;
    FSecond: TSecond;
    FThird: TThird;
  public
    constructor Create(const AFirst: TFirst; const ASecond: TSecond; const AThird: TThird);
    function GetFirst: TFirst;
    function GetSecond: TSecond;
    function GetThird: TThird;
  end;
  TQueryZipCursor<TLeft, TRight, TResult> = class(TEnumerator<TResult>)
  private
    FLeftSource: INyxSequence<TLeft>;
    FRightSource: INyxSequence<TRight>;
    FLeft: INyxEnumerator<TLeft>;
    FRight: INyxEnumerator<TRight>;
    FResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>;
    procedure Retire;
  protected
    function Advance(out AValue: TResult): Boolean; override;
  public
    constructor Create(const ALeft: INyxSequence<TLeft>;
      const ARight: INyxSequence<TRight>;
      const AResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>);
  end;
  TQueryZipSequence<TLeft, TRight, TResult> = class(TNyxSequence<TResult>)
  private
    FLeft: INyxSequence<TLeft>;
    FRight: INyxSequence<TRight>;
    FResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>;
  public
    constructor Create(const ALeft: INyxSequence<TLeft>;
      const ARight: INyxSequence<TRight>;
      const AResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>);
    function GetEnumerator: INyxEnumerator<TResult>; override;
  end;
  TQueryTripleProject<TFirst, TSecond, TThird, TResult> = class(TInterfacedObject,
    INyxSelector<INyxPairValue<INyxPairValue<TFirst, TSecond>, TThird>, TResult>)
  private type
    TPair = INyxPairValue<TFirst, TSecond>;
    TInput = INyxPairValue<TPair, TThird>;
    TTriple = INyxTripleValue<TFirst, TSecond, TThird>;
  private
    FResult: INyxSelector<TTriple, TResult>;
  public
    constructor Create(const AResult: INyxSelector<TTriple, TResult>);
    function Project(const AValue: TInput): TResult;
  end;
  TQueryIndexCursor<T> = class(TEnumerator<INyxIndexedValue<T>>)
  private
    FSource: INyxSequence<T>;
    FCursor: INyxEnumerator<T>;
    FIndex: Integer;
    FExhausted: Boolean;
  protected
    function Advance(out AValue: INyxIndexedValue<T>): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<T>);
  end;
  TQueryIndexSequence<T> = class(TNyxSequence<INyxIndexedValue<T>>)
  private
    FSource: INyxSequence<T>;
  public
    constructor Create(const ASource: INyxSequence<T>);
    function GetEnumerator: INyxEnumerator<INyxIndexedValue<T>>; override;
  end;

implementation

constructor TQueryTriple<TFirst, TSecond, TThird>.Create(const AFirst: TFirst;
  const ASecond: TSecond; const AThird: TThird);
begin
  inherited Create;
  FFirst := AFirst;
  FSecond := ASecond;
  FThird := AThird;
end;

function TQueryTriple<TFirst, TSecond, TThird>.GetFirst: TFirst;
begin
  Result := FFirst;
end;

function TQueryTriple<TFirst, TSecond, TThird>.GetSecond: TSecond;
begin
  Result := FSecond;
end;

function TQueryTriple<TFirst, TSecond, TThird>.GetThird: TThird;
begin
  Result := FThird;
end;

constructor TQueryZipCursor<TLeft, TRight, TResult>.Create(
  const ALeft: INyxSequence<TLeft>; const ARight: INyxSequence<TRight>;
  const AResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>);
begin
  inherited Create;
  FLeftSource := ALeft;
  FRightSource := ARight;
  FResult := AResult;
end;

procedure TQueryZipCursor<TLeft, TRight, TResult>.Retire;
begin
  FLeft := nil;
  FRight := nil;
  FLeftSource := nil;
  FRightSource := nil;
  FResult := nil;
end;

function TQueryZipCursor<TLeft, TRight, TResult>.Advance(out AValue: TResult): Boolean;
var
  LPair: INyxPairValue<TLeft, TRight>;
begin
  Result := False;
  try

    if FLeft = nil then
    begin
      FLeft := FLeftSource.GetEnumerator;
      FLeftSource := nil;

      if FLeft = nil then
      begin
        raise ENyxQuery.Create('Zip left input returned no cursor');
      end;
    end;

    if not FLeft.MoveNext then
    begin
      Retire;
      Exit;
    end;

    if FRight = nil then
    begin
      FRight := FRightSource.GetEnumerator;
      FRightSource := nil;

      if FRight = nil then
      begin
        raise ENyxQuery.Create('Zip right input returned no cursor');
      end;
    end;

    if not FRight.MoveNext then
    begin
      Retire;
      Exit;
    end;
    LPair := TNyxPairValue<TLeft, TRight>.Create(FLeft.Current, FRight.Current);
    AValue := FResult.Project(LPair);
    Result := True;
  except
    Retire;
    raise;
  end;
end;

constructor TQueryZipSequence<TLeft, TRight, TResult>.Create(
  const ALeft: INyxSequence<TLeft>; const ARight: INyxSequence<TRight>;
  const AResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>);
begin
  inherited Create;
  FLeft := ALeft;
  FRight := ARight;
  FResult := AResult;
end;

function TQueryZipSequence<TLeft, TRight, TResult>.GetEnumerator: INyxEnumerator<TResult>;
begin
  Result := TQueryZipCursor<TLeft, TRight, TResult>.Create(FLeft, FRight, FResult);
end;

class function TNyxZip<TLeft, TRight, TResult>.From(const ALeft: INyxSequence<TLeft>;
  const ARight: INyxSequence<TRight>;
  const AResult: INyxSelector<INyxPairValue<TLeft, TRight>, TResult>): INyxSequence<TResult>;
begin

  if (ALeft = nil) or (ARight = nil) or (AResult = nil) then
  begin
    raise ENyxQuery.Create('Zip requires both inputs and an owned result selector');
  end;
  Result := TQueryZipSequence<TLeft, TRight, TResult>.Create(ALeft, ARight, AResult);
end;

class function TNyxZipPairs<TLeft, TRight>.From(const ALeft: INyxSequence<TLeft>;
  const ARight: INyxSequence<TRight>): INyxSequence<INyxPairValue<TLeft, TRight>>;
type
  TPair = INyxPairValue<TLeft, TRight>;
var
  LIdentity: INyxSelector<TPair, TPair>;
begin
  LIdentity := TQuerySetIdentity<TPair>.Create;
  Result := TNyxZip<TLeft, TRight, TPair>.From(ALeft, ARight, LIdentity);
end;

constructor TQueryTripleProject<TFirst, TSecond, TThird, TResult>.Create(
  const AResult: INyxSelector<TTriple, TResult>);
begin
  inherited Create;
  FResult := AResult;
end;

function TQueryTripleProject<TFirst, TSecond, TThird, TResult>.Project(
  const AValue: TInput): TResult;
var
  LPair: TPair;
  LTriple: TTriple;
begin
  LPair := AValue.Outer;
  LTriple := TQueryTriple<TFirst, TSecond, TThird>.Create(
    LPair.Outer, LPair.Inner, AValue.Inner);
  Result := FResult.Project(LTriple);
end;

class function TNyxZipThree<TFirst, TSecond, TThird, TResult>.From(
  const AFirst: INyxSequence<TFirst>; const ASecond: INyxSequence<TSecond>;
  const AThird: INyxSequence<TThird>;
  const AResult: INyxSelector<INyxTripleValue<TFirst, TSecond, TThird>, TResult>):
  INyxSequence<TResult>;
var
  LPairs: INyxSequence<TPair>;
  LProject: INyxSelector<TInput, TResult>;
begin

  if (AFirst = nil) or (ASecond = nil) or (AThird = nil) or (AResult = nil) then
  begin
    raise ENyxQuery.Create('Three-way Zip requires three inputs and a result selector');
  end;
  LPairs := TNyxZipPairs<TFirst, TSecond>.From(AFirst, ASecond);
  LProject := TQueryTripleProject<TFirst, TSecond, TThird, TResult>.Create(AResult);
  Result := TNyxZip<TPair, TThird, TResult>.From(LPairs, AThird, LProject);
end;

class function TNyxZipTriples<TFirst, TSecond, TThird>.From(
  const AFirst: INyxSequence<TFirst>; const ASecond: INyxSequence<TSecond>;
  const AThird: INyxSequence<TThird>): INyxSequence<TTriple>;
var
  LIdentity: INyxSelector<TTriple, TTriple>;
begin
  LIdentity := TQuerySetIdentity<TTriple>.Create;
  Result := TNyxZipThree<TFirst, TSecond, TThird, TTriple>.From(
    AFirst, ASecond, AThird, LIdentity);
end;

constructor TQueryIndexCursor<T>.Create(const ASource: INyxSequence<T>);
begin
  inherited Create;
  FSource := ASource;
end;

function TQueryIndexCursor<T>.Advance(out AValue: INyxIndexedValue<T>): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  try

    if FCursor = nil then
    begin
      FCursor := FSource.GetEnumerator;
      FSource := nil;

      if FCursor = nil then
      begin
        raise ENyxQuery.Create('Index input returned no cursor');
      end;
    end;

    if not FCursor.MoveNext then
    begin
      FCursor := nil;
      Exit;
    end;
    LIndex := NextNyxQueryIndex(FIndex, FExhausted);
    AValue := TNyxIndexedValue<T>.Create(FCursor.Current, LIndex);
    Result := True;
  except
    FSource := nil;
    FCursor := nil;
    raise;
  end;
end;

constructor TQueryIndexSequence<T>.Create(const ASource: INyxSequence<T>);
begin
  inherited Create;
  FSource := ASource;
end;

function TQueryIndexSequence<T>.GetEnumerator: INyxEnumerator<INyxIndexedValue<T>>;
begin
  Result := TQueryIndexCursor<T>.Create(FSource);
end;

class function TNyxIndex<T>.From(const ASource: INyxSequence<T>):
  INyxSequence<INyxIndexedValue<T>>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Index requires a source');
  end;
  Result := TQueryIndexSequence<T>.Create(ASource);
end;

end.
