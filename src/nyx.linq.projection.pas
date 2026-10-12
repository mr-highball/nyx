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
unit nyx.linq.projection;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.linq;

type
  { Owned immutable outer/inner context preserves specialized properties without casts.
    Existing global/owned/scoped/inline selector adapters operate on this interface.
    Managed fields retain ordinary Pascal ownership; mutable inner arrays and raw
    class pointers are not implicitly cloned or made thread-safe. }
  INyxPairValue<TOuter, TInner> = interface(IInterface)
    function GetOuter: TOuter;
    function GetInner: TInner;
    property Outer: TOuter read GetOuter;
    property Inner: TInner read GetInner;
  end;
  TNyxPairValue<TOuter, TInner> = class(TInterfacedObject, INyxPairValue<TOuter, TInner>)
  private
    FOuter: TOuter;
    FInner: TInner;
  public
    constructor Create(const AOuter: TOuter; const AInner: TInner);
    function GetOuter: TOuter;
    function GetInner: TInner;
  end;

  { Deferred flattening in outer then inner order. Empty inner sequences are
    skipped. A nil returned sequence/cursor is an invalid callback contract,
    refused rather than silently treated as empty. Construction/GetEnumerator
    invokes no selector or inner source. Each cursor owns independent outer/
    current inner enumeration; retired inner cursors are released before the next
    outer callback. EOF/failure is terminal and original exceptions propagate.
    Take(0) never enters either source; short-circuiting/disposal releases both.
    Indexed selectors use the outer INPUT ordinal, including empty inner results.
    The ordinary fluent TResult sequence can feed ordering or parallel Submit. }
  TNyxSelectMany<TSource, TResult> = class
  public
    class function From(const ASource: INyxSequence<TSource>;
      const ASelector: INyxSelector<TSource, INyxSequence<TResult>>): INyxSequence<TResult>; static;
    class function FromIndexed(const ASource: INyxSequence<TSource>;
      const ASelector: INyxIndexedSelector<TSource, INyxSequence<TResult>>): INyxSequence<TResult>; static;
  end;

  { SelectMany with a distinct inner type and result selector. Each inner mapping
    owns its outer context until that inner cursor retires. The result selector
    executes once per yielded pair, never for empty inner sequences. Existing
    nested leases/inline captures retain their ordinary entry/lifetime guarantees. }
  TNyxSelectManyResult<TOuter, TInner, TResult> = class
  public
    class function From(const ASource: INyxSequence<TOuter>;
      const ASelector: INyxSelector<TOuter, INyxSequence<TInner>>;
      const AResult: INyxSelector<INyxPairValue<TOuter, TInner>, TResult>): INyxSequence<TResult>; static;
    class function FromIndexed(const ASource: INyxSequence<TOuter>;
      const ASelector: INyxIndexedSelector<TOuter, INyxSequence<TInner>>;
      const AResult: INyxSelector<INyxPairValue<TOuter, TInner>, TResult>): INyxSequence<TResult>; static;
  end;

  { Compiler-visible implementations for native template expansion. No cursor
    retains its creating sequence or a UI/document back reference. }
  TFlattenEnumerator<TSource, TResult> = class(TEnumerator<TResult>)
  private
    FOuterSource: INyxSequence<TSource>;
    FOuter: INyxEnumerator<TSource>;
    FInner: INyxEnumerator<TResult>;
    FSelector: INyxSelector<TSource, INyxSequence<TResult>>;
    FIndexed: INyxIndexedSelector<TSource, INyxSequence<TResult>>;
    FIndex: Integer;
    FExhausted: Boolean;
    procedure Retire;
  protected
    function Advance(out AValue: TResult): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<TSource>;
      const ASelector: INyxSelector<TSource, INyxSequence<TResult>>;
      const AIndexed: INyxIndexedSelector<TSource, INyxSequence<TResult>>);
  end;
  TFlattenSequence<TSource, TResult> = class(TNyxSequence<TResult>)
  private
    FSource: INyxSequence<TSource>;
    FSelector: INyxSelector<TSource, INyxSequence<TResult>>;
    FIndexed: INyxIndexedSelector<TSource, INyxSequence<TResult>>;
  public
    constructor Create(const ASource: INyxSequence<TSource>;
      const ASelector: INyxSelector<TSource, INyxSequence<TResult>>;
      const AIndexed: INyxIndexedSelector<TSource, INyxSequence<TResult>>);
    function GetEnumerator: INyxEnumerator<TResult>; override;
  end;
  TBoundPairSelector<TOuter, TInner, TResult> = class(TInterfacedObject,
    INyxSelector<TInner, TResult>)
  private
    FOuter: TOuter;
    FResult: INyxSelector<INyxPairValue<TOuter, TInner>, TResult>;
  public
    constructor Create(const AOuter: TOuter;
      const AResult: INyxSelector<INyxPairValue<TOuter, TInner>, TResult>);
    function Project(const AValue: TInner): TResult;
  end;
  TPairSequenceSelector<TOuter, TInner, TResult> = class(TInterfacedObject,
    INyxSelector<TOuter, INyxSequence<TResult>>,
    INyxIndexedSelector<TOuter, INyxSequence<TResult>>)
  private
    FSelector: INyxSelector<TOuter, INyxSequence<TInner>>;
    FIndexed: INyxIndexedSelector<TOuter, INyxSequence<TInner>>;
    FResult: INyxSelector<INyxPairValue<TOuter, TInner>, TResult>;
  public
    constructor Create(const ASelector: INyxSelector<TOuter, INyxSequence<TInner>>;
      const AIndexed: INyxIndexedSelector<TOuter, INyxSequence<TInner>>;
      const AResult: INyxSelector<INyxPairValue<TOuter, TInner>, TResult>);
    function Project(const AValue: TOuter): INyxSequence<TResult>; overload;
    function Project(const AValue: TOuter; AIndex: Integer): INyxSequence<TResult>; overload;
  end;

implementation

constructor TNyxPairValue<TOuter, TInner>.Create(const AOuter: TOuter; const AInner: TInner);
begin
  inherited Create;
  FOuter := AOuter;
  FInner := AInner;
end;

function TNyxPairValue<TOuter, TInner>.GetOuter: TOuter;
begin
  Result := FOuter;
end;

function TNyxPairValue<TOuter, TInner>.GetInner: TInner;
begin
  Result := FInner;
end;

constructor TFlattenEnumerator<TSource, TResult>.Create(const ASource: INyxSequence<TSource>;
  const ASelector: INyxSelector<TSource, INyxSequence<TResult>>;
  const AIndexed: INyxIndexedSelector<TSource, INyxSequence<TResult>>);
begin
  inherited Create;
  FOuterSource := ASource;
  FSelector := ASelector;
  FIndexed := AIndexed;
end;

procedure TFlattenEnumerator<TSource, TResult>.Retire;
begin
  FInner := nil;
  FOuter := nil;
  FOuterSource := nil;
  FSelector := nil;
  FIndexed := nil;
end;

function TFlattenEnumerator<TSource, TResult>.Advance(out AValue: TResult): Boolean;
var
  LInner: INyxSequence<TResult>;
  LIndex: Integer;
  LOuterValue: TSource;
begin
  try

    if FOuter = nil then
    begin
      FOuter := FOuterSource.GetEnumerator;
      FOuterSource := nil;

      if FOuter = nil then
      begin
        raise ENyxQuery.Create('SelectMany source returned no cursor');
      end;
    end;
    while True do
    begin

      if FInner <> nil then
      begin

        if FInner.MoveNext then
        begin
          AValue := FInner.Current;
          Exit(True);
        end;
        FInner := nil;
      end;

      if not FOuter.MoveNext then
      begin
        Retire;
        Exit(False);
      end;
      { Keep the managed outer value in an explicit local through inner setup;
        both compilers then express its temporary ownership directly. }
      LOuterValue := FOuter.Current;

      if FIndexed <> nil then
      begin
        LIndex := NextNyxQueryIndex(FIndex, FExhausted);
        LInner := FIndexed.Project(LOuterValue, LIndex);
      end
      else
      begin
        LInner := FSelector.Project(LOuterValue);
      end;

      if LInner = nil then
      begin
        raise ENyxQuery.Create('SelectMany selector returned no sequence');
      end;
      FInner := LInner.GetEnumerator;
      LInner := nil;

      if FInner = nil then
      begin
        raise ENyxQuery.Create('SelectMany inner sequence returned no cursor');
      end;
    end;
  except
    Retire;
    raise;
  end;
end;

constructor TFlattenSequence<TSource, TResult>.Create(const ASource: INyxSequence<TSource>;
  const ASelector: INyxSelector<TSource, INyxSequence<TResult>>;
  const AIndexed: INyxIndexedSelector<TSource, INyxSequence<TResult>>);
begin
  inherited Create;
  FSource := ASource;
  FSelector := ASelector;
  FIndexed := AIndexed;
end;

function TFlattenSequence<TSource, TResult>.GetEnumerator: INyxEnumerator<TResult>;
begin
  Result := TFlattenEnumerator<TSource, TResult>.Create(FSource, FSelector, FIndexed);
end;

class function TNyxSelectMany<TSource, TResult>.From(const ASource: INyxSequence<TSource>;
  const ASelector: INyxSelector<TSource, INyxSequence<TResult>>): INyxSequence<TResult>;
begin

  if (ASource = nil) or (ASelector = nil) then
  begin
    raise ENyxQuery.Create('SelectMany requires a source and owned selector');
  end;
  Result := TFlattenSequence<TSource, TResult>.Create(ASource, ASelector, nil);
end;

class function TNyxSelectMany<TSource, TResult>.FromIndexed(const ASource: INyxSequence<TSource>;
  const ASelector: INyxIndexedSelector<TSource, INyxSequence<TResult>>): INyxSequence<TResult>;
begin

  if (ASource = nil) or (ASelector = nil) then
  begin
    raise ENyxQuery.Create('Indexed SelectMany requires a source and owned selector');
  end;
  Result := TFlattenSequence<TSource, TResult>.Create(ASource, nil, ASelector);
end;

constructor TBoundPairSelector<TOuter, TInner, TResult>.Create(const AOuter: TOuter;
  const AResult: INyxSelector<INyxPairValue<TOuter, TInner>, TResult>);
begin
  inherited Create;
  FOuter := AOuter;
  FResult := AResult;
end;

function TBoundPairSelector<TOuter, TInner, TResult>.Project(const AValue: TInner): TResult;
var
  LPair: INyxPairValue<TOuter, TInner>;
begin
  LPair := TNyxPairValue<TOuter, TInner>.Create(FOuter, AValue);
  Result := FResult.Project(LPair);
end;

constructor TPairSequenceSelector<TOuter, TInner, TResult>.Create(
  const ASelector: INyxSelector<TOuter, INyxSequence<TInner>>;
  const AIndexed: INyxIndexedSelector<TOuter, INyxSequence<TInner>>;
  const AResult: INyxSelector<INyxPairValue<TOuter, TInner>, TResult>);
begin
  inherited Create;
  FSelector := ASelector;
  FIndexed := AIndexed;
  FResult := AResult;
end;

function TPairSequenceSelector<TOuter, TInner, TResult>.Project(const AValue: TOuter;
  AIndex: Integer): INyxSequence<TResult>;
var
  LInner: INyxSequence<TInner>;
  LResult: INyxSelector<TInner, TResult>;
begin

  if FIndexed <> nil then
  begin
    LInner := FIndexed.Project(AValue, AIndex);
  end
  else
  begin
    LInner := FSelector.Project(AValue);
  end;

  if LInner = nil then
  begin
    raise ENyxQuery.Create('SelectMany collection selector returned no sequence');
  end;
  LResult := TBoundPairSelector<TOuter, TInner, TResult>.Create(AValue, FResult);
  Result := TNyxSelect<TInner, TResult>.From(LInner, LResult);
end;

function TPairSequenceSelector<TOuter, TInner, TResult>.Project(const AValue: TOuter): INyxSequence<TResult>;
begin
  Result := Project(AValue, 0);
end;

class function TNyxSelectManyResult<TOuter, TInner, TResult>.From(const ASource: INyxSequence<TOuter>;
  const ASelector: INyxSelector<TOuter, INyxSequence<TInner>>;
  const AResult: INyxSelector<INyxPairValue<TOuter, TInner>, TResult>): INyxSequence<TResult>;
var
  LSelector: INyxSelector<TOuter, INyxSequence<TResult>>;
begin

  if (ASource = nil) or (ASelector = nil) or (AResult = nil) then
  begin
    raise ENyxQuery.Create('Result SelectMany requires source, collection and result selectors');
  end;
  LSelector := TPairSequenceSelector<TOuter, TInner, TResult>.Create(ASelector, nil, AResult);
  Result := TNyxSelectMany<TOuter, TResult>.From(ASource, LSelector);
end;

class function TNyxSelectManyResult<TOuter, TInner, TResult>.FromIndexed(const ASource: INyxSequence<TOuter>;
  const ASelector: INyxIndexedSelector<TOuter, INyxSequence<TInner>>;
  const AResult: INyxSelector<INyxPairValue<TOuter, TInner>, TResult>): INyxSequence<TResult>;
var
  LSelector: INyxIndexedSelector<TOuter, INyxSequence<TResult>>;
begin

  if (ASource = nil) or (ASelector = nil) or (AResult = nil) then
  begin
    raise ENyxQuery.Create('Indexed result SelectMany requires source, collection and result selectors');
  end;
  LSelector := TPairSequenceSelector<TOuter, TInner, TResult>.Create(nil, ASelector, AResult);
  Result := TNyxSelectMany<TOuter, TResult>.FromIndexed(ASource, LSelector);
end;

end.
