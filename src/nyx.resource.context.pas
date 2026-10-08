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


unit nyx.resource.context;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.resources;

type
  { Immutable portable runtime context. The retained interface owns catalog
    membership and locale choices, never an application/document/view. Snapshot
    returns independent mutable membership; changing it cannot alter this frame.
    Immutable definitions may be shared safely across snapshots and runtimes. }
  INyxResourceContext = interface
    ['{40537581-80D4-4BEC-A77A-898E5A4F49B6}']
    function Snapshot: INyxResources;
    function GetLocale: TNyxLocaleRef;
    function GetFallback: TNyxLocaleRef;
    property Locale: TNyxLocaleRef read GetLocale;
    property Fallback: TNyxLocaleRef read GetFallback;
  end;

  { A renderer may retain this portable application port. Context is immutable;
    Wake only requests deferred work and must never publish inside a render or
    input callback. The application retires the port before freeing receivers. }
  INyxResourceUpdateQueue = interface
    ['{35A0BD12-589E-4DA0-A4B0-9EB4C905546C}']
    function GetContext: INyxResourceContext;
    procedure Wake;
    property Context: INyxResourceContext read GetContext;
  end;

{ Normalize foreign catalogs once through the strict portable contract before
  retaining them. Nil refuses; empty catalogs remain valid. No transport runs. }
function NewNyxResourceContext(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef): INyxResourceContext;

implementation

type
  TResourceContext = class(TInterfacedObject, INyxResourceContext)
  private
    FResources: INyxResources;
    FLocale: TNyxLocaleRef;
    FFallback: TNyxLocaleRef;
  public
    constructor Create(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef);
    function Snapshot: INyxResources;
    function GetLocale: TNyxLocaleRef;
    function GetFallback: TNyxLocaleRef;
  end;

constructor TResourceContext.Create(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef);
begin
  inherited Create;

  if AResources = nil then
  begin
    raise ENyxResource.Create('A runtime resource context requires a catalog');
  end;
  FResources := NyxResourcesFromData(AResources.ToData);
  FLocale := ALocale;
  FFallback := AFallback;
end;

function TResourceContext.Snapshot: INyxResources;
begin
  Result := FResources.Clone;
end;

function TResourceContext.GetLocale: TNyxLocaleRef;
begin
  Result := FLocale;
end;

function TResourceContext.GetFallback: TNyxLocaleRef;
begin
  Result := FFallback;
end;

function NewNyxResourceContext(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef): INyxResourceContext;
begin
  Result := TResourceContext.Create(AResources, ALocale, AFallback);
end;

end.
