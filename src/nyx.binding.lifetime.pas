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

unit nyx.binding.lifetime;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.state;

type
  { One explicit raw-state owner shared by accepted and entered binding
    generations. State is borrowed and remains valid only while a holder lease
    is retained. Borrowed external stores require no holder.

    Create this owner ONCE for a raw store, then share the SAME interface. Raw
    pointer equality never transfers or proves ownership. The holder has no
    renderer, subscription, scheduler or generation backreference; ordinary
    state thread/lifetime rules still apply. This alone does not keep a live
    binding coordinator, realized root or physical view alive. }
  INyxBindingOwnedState = interface(IInterface)
    ['{86816DF9-0507-4C91-9D80-56F910F2E1AE}']
    function GetState: TNyxState;
    property State: TNyxState read GetState;
  end;

{ Takes sole raw ownership only on successful return. Nil refuses without
  adopting anything. On failure the caller retains its raw state; on success
  the caller must neither Free it nor pass it to another owner factory. Share
  the returned interface with an exact successor before releasing the old
  accepted generation. Final interface release frees the state exactly once. }
function OwnNyxBindingState(AState: TNyxState): INyxBindingOwnedState;

implementation

type
  { No outward calls occur between storing the admitted raw pointer and the
    successful interface return. Destruction removes that pointer before the
    state's destructor can perform any caller-defined cleanup. }
  TBindingOwnedState = class(TInterfacedObject, INyxBindingOwnedState)
  private
    FState: TNyxState;
  public
    constructor Create(AState: TNyxState);
    destructor Destroy; override;
    function GetState: TNyxState;
  end;

constructor TBindingOwnedState.Create(AState: TNyxState);
begin
  inherited Create;
  FState := AState;
end;

destructor TBindingOwnedState.Destroy;
var
  LState: TNyxState;
begin
  LState := FState;
  FState := nil;
  try
    LState.Free;
  finally
    inherited Destroy;
  end;
end;

function TBindingOwnedState.GetState: TNyxState;
begin
  Result := FState;
end;

function OwnNyxBindingState(AState: TNyxState): INyxBindingOwnedState;
begin

  if AState = nil then
  begin
    raise ENyxState.Create('Owned binding state requires an actual state');
  end;
  Result := TBindingOwnedState.Create(AState);
end;

end.
