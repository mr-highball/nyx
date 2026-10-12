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
unit nyx.linq.runtime;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, TypInfo, nyx.linq, nyx.linq.numeric;

type
  ENyxQueryCast = class(ENyxQuery);
  { Explicit typed runtime conversion: Some is compatible, None is incompatible.
    Some(nil), zero and empty text are present values. A nil optional interface
    refuses. OfType skips incompatible rows; Cast raises ENyxQueryCast on the
    first incompatible row. There is no untyped reinterpretation or automatic
    numeric/text conversion. Construction/GetEnumerator enter no input/callback;
    independent cursors preserve original failures and retire promptly. Ordinary
    owned/nested/inline adapters retain their existing lifetime contract. }
  TNyxTypeQuery<TSource, TResult> = class
  public
    class function OfType(const ASource: INyxSequence<TSource>;
      const AConvert: INyxSelector<TSource, INyxOptional<TResult>>): INyxSequence<TResult>; static;
    class function Cast(const ASource: INyxSequence<TSource>;
      const AConvert: INyxSelector<TSource, INyxOptional<TResult>>): INyxSequence<TResult>; static;
  end;
  { Built-in class membership uses the Pascal runtime class relationship on both
    targets. Objects are borrowed, never freed by a query. OfType excludes nil;
    Cast preserves nil as a typed present value and refuses incompatible classes. }
  TNyxClassQuery<T: class> = class
  public
    class function OfType(const ASource: INyxSequence<TObject>): INyxSequence<T>; static;
    class function Cast(const ASource: INyxSequence<TObject>): INyxSequence<T>; static;
  end;
  { Built-in managed capability filtering uses the target interface's compiler
    identity, never a caller-supplied string. Missing/zero identity refuses before
    source entry. Native FPC requires a declared GUID; pas2js can supply an identity
    for GUID-less interfaces. Supported exposes this target capability. Declare
    GUIDs for portable application contracts; compiler-generated identities are
    runtime-local and are not a persistence or cross-process contract.
    Supported payloads retain ordinary reference counting;
    OfType skips nil/unsupported rows, Cast preserves nil and refuses unsupported
    rows. Native RTTI and pas2js interface metadata remain behind this adapter. }
  TNyxInterfaceQuery<T: IInterface> = class
  public
    { Inspect the target's runtime capability without a source or callback. }
    class function Supported: Boolean; static;
    class function OfType(const ASource: INyxSequence<IInterface>): INyxSequence<T>; static;
    class function Cast(const ASource: INyxSequence<IInterface>): INyxSequence<T>; static;
  end;

  { Compiler-visible support; application code uses the factories above. }
  TQueryTypeCursor<TSource, TResult> = class(TEnumerator<TResult>)
  private
    FSource: INyxSequence<TSource>;
    FCursor: INyxEnumerator<TSource>;
    FConvert: INyxSelector<TSource, INyxOptional<TResult>>;
    FRequired: Boolean;
    procedure Retire;
  protected
    function Advance(out AValue: TResult): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<TSource>;
      const AConvert: INyxSelector<TSource, INyxOptional<TResult>>; ARequired: Boolean);
  end;
  TQueryTypeSequence<TSource, TResult> = class(TNyxSequence<TResult>)
  private
    FSource: INyxSequence<TSource>;
    FConvert: INyxSelector<TSource, INyxOptional<TResult>>;
    FRequired: Boolean;
  public
    constructor Create(const ASource: INyxSequence<TSource>;
      const AConvert: INyxSelector<TSource, INyxOptional<TResult>>; ARequired: Boolean);
    function GetEnumerator: INyxEnumerator<TResult>; override;
  end;
  TQueryClassConvert<T: class> = class(TInterfacedObject,
    INyxSelector<TObject, INyxOptional<T>>)
  private
    FPermitNil: Boolean;
  public
    constructor Create(APermitNil: Boolean);
    function Project(const AValue: TObject): INyxOptional<T>;
  end;
  TQueryInterfaceConvert<T: IInterface> = class(TInterfacedObject,
    INyxSelector<IInterface, INyxOptional<T>>)
  private
    FPermitNil: Boolean;
    FGuid: TGuid;
    FEmpty: T;
  public
    class function TryIdentity(out AValue: TGuid): Boolean; static;
    constructor Create(APermitNil: Boolean);
    function Project(const AValue: IInterface): INyxOptional<T>;
  end;

implementation

constructor TQueryTypeCursor<TSource, TResult>.Create(const ASource: INyxSequence<TSource>;
  const AConvert: INyxSelector<TSource, INyxOptional<TResult>>; ARequired: Boolean);
begin
  inherited Create;
  FSource := ASource;
  FConvert := AConvert;
  FRequired := ARequired;
end;

procedure TQueryTypeCursor<TSource, TResult>.Retire;
begin
  FSource := nil;
  FCursor := nil;
  FConvert := nil;
end;

function TQueryTypeCursor<TSource, TResult>.Advance(out AValue: TResult): Boolean;
var
  LConverted: INyxOptional<TResult>;
begin
  Result := False;
  try

    if FCursor = nil then
    begin
      FCursor := FSource.GetEnumerator;
      FSource := nil;

      if FCursor = nil then
      begin
        raise ENyxQuery.Create('Runtime conversion source returned no cursor');
      end;
    end;
    while FCursor.MoveNext do
    begin
      LConverted := FConvert.Project(FCursor.Current);

      if LConverted = nil then
      begin
        raise ENyxQuery.Create('Runtime converter returned no optional value');
      end;

      if LConverted.HasValue then
      begin
        AValue := LConverted.Value;
        Result := True;
        Exit;
      end;

      if FRequired then
      begin
        raise ENyxQueryCast.Create('Source value is incompatible with the requested type');
      end;
    end;
    Retire;
  except
    Retire;
    raise;
  end;
end;

constructor TQueryTypeSequence<TSource, TResult>.Create(const ASource: INyxSequence<TSource>;
  const AConvert: INyxSelector<TSource, INyxOptional<TResult>>; ARequired: Boolean);
begin
  inherited Create;
  FSource := ASource;
  FConvert := AConvert;
  FRequired := ARequired;
end;

function TQueryTypeSequence<TSource, TResult>.GetEnumerator: INyxEnumerator<TResult>;
begin
  Result := TQueryTypeCursor<TSource, TResult>.Create(FSource, FConvert, FRequired);
end;

class function TNyxTypeQuery<TSource, TResult>.OfType(const ASource: INyxSequence<TSource>;
  const AConvert: INyxSelector<TSource, INyxOptional<TResult>>): INyxSequence<TResult>;
begin

  if (ASource = nil) or (AConvert = nil) then
  begin
    raise ENyxQuery.Create('OfType requires a source and typed converter');
  end;
  Result := TQueryTypeSequence<TSource, TResult>.Create(ASource, AConvert, False);
end;

class function TNyxTypeQuery<TSource, TResult>.Cast(const ASource: INyxSequence<TSource>;
  const AConvert: INyxSelector<TSource, INyxOptional<TResult>>): INyxSequence<TResult>;
begin

  if (ASource = nil) or (AConvert = nil) then
  begin
    raise ENyxQuery.Create('Cast requires a source and typed converter');
  end;
  Result := TQueryTypeSequence<TSource, TResult>.Create(ASource, AConvert, True);
end;

constructor TQueryClassConvert<T>.Create(APermitNil: Boolean);
begin
  inherited Create;
  FPermitNil := APermitNil;
end;

function TQueryClassConvert<T>.Project(const AValue: TObject): INyxOptional<T>;
begin

  if AValue is T then
  begin
    Result := TNyxOptional<T>.Some(T(AValue));
  end
  else
  begin

    if (AValue = nil) and FPermitNil then
    begin
      Result := TNyxOptional<T>.Some(nil);
    end
    else
    begin
      Result := TNyxOptional<T>.None;
    end;
  end;
end;

class function TNyxClassQuery<T>.OfType(const ASource: INyxSequence<TObject>): INyxSequence<T>;
var
  LConvert: INyxSelector<TObject, INyxOptional<T>>;
begin
  LConvert := TQueryClassConvert<T>.Create(False);
  Result := TNyxTypeQuery<TObject, T>.OfType(ASource, LConvert);
end;

class function TNyxClassQuery<T>.Cast(const ASource: INyxSequence<TObject>): INyxSequence<T>;
var
  LConvert: INyxSelector<TObject, INyxOptional<T>>;
begin
  LConvert := TQueryClassConvert<T>.Create(True);
  Result := TNyxTypeQuery<TObject, T>.Cast(ASource, LConvert);
end;

class function TQueryInterfaceConvert<T>.TryIdentity(out AValue: TGuid): Boolean;
{$ifdef PAS2JS}
var
  LGuidText: String;
{$endif}
begin
  AValue := GUID_NULL;
  Result := False;
  {$ifdef PAS2JS}
  LGuidText := TTypeInfoInterface(TypeInfo(T)).InterfaceInfo.GUID;

  if LGuidText = '' then
  begin
    Exit;
  end;
  AValue := StringToGUID(LGuidText);
  {$else}

  if not (ifHasGuid in GetTypeData(TypeInfo(T))^.IntfFlags) then
  begin
    Exit;
  end;
  AValue := GetTypeData(TypeInfo(T))^.GUID;
  {$endif}
  Result := not IsEqualGUID(AValue, GUID_NULL);
end;

constructor TQueryInterfaceConvert<T>.Create(APermitNil: Boolean);
begin
  inherited Create;
  FPermitNil := APermitNil;

  if not TryIdentity(FGuid) then
  begin
    raise ENyxQuery.Create('This target cannot query the requested interface identity');
  end;
end;

class function TNyxInterfaceQuery<T>.Supported: Boolean;
var
  LIdentity: TGuid;
begin
  Result := TQueryInterfaceConvert<T>.TryIdentity(LIdentity);
end;

function TQueryInterfaceConvert<T>.Project(const AValue: IInterface): INyxOptional<T>;
var
  LSupported: T;
begin

  if (AValue = nil) and FPermitNil then
  begin
    Result := TNyxOptional<T>.Some(FEmpty);
  end
  else
  begin

    if Supports(AValue, FGuid, LSupported) then
    begin
      Result := TNyxOptional<T>.Some(LSupported);
    end
    else
    begin
      Result := TNyxOptional<T>.None;
    end;
  end;
end;

class function TNyxInterfaceQuery<T>.OfType(const ASource: INyxSequence<IInterface>):
  INyxSequence<T>;
var
  LConvert: INyxSelector<IInterface, INyxOptional<T>>;
begin
  LConvert := TQueryInterfaceConvert<T>.Create(False);
  Result := TNyxTypeQuery<IInterface, T>.OfType(ASource, LConvert);
end;

class function TNyxInterfaceQuery<T>.Cast(const ASource: INyxSequence<IInterface>):
  INyxSequence<T>;
var
  LConvert: INyxSelector<IInterface, INyxOptional<T>>;
begin
  LConvert := TQueryInterfaceConvert<T>.Create(True);
  Result := TNyxTypeQuery<IInterface, T>.Cast(ASource, LConvert);
end;

end.
