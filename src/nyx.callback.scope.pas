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
unit nyx.callback.scope;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils;

type
  ENyxCallbackScope = class(Exception);
  ENyxCallbackExpired = class(ENyxCallbackScope);
  ENyxCallbackThread = class(ENyxCallbackScope);

  { A revocable entry port retains no callback, lease, caller stack frame or UI.
    Entry refuses after Close, or on another native thread, before a borrowed
    nested function's frame pointer can be dereferenced. It is not a frame clone. }
  INyxCallbackLifetime = interface(IInterface)
    ['{B8A120C3-C514-4CD2-9267-F973B1BF1141}']
    procedure RequireEntry;
  end;

  { The caller owns this lease for the enclosing nested routine's lifetime.
    Always Close in a finally block before returning from that routine; retaining
    a lease cannot make a native stack frame live longer. Releasing the last
    lease reference also closes its port. Adapters retain the port, not the lease,
    so releasing a caller's lease invalidates retained queries/cursors.
    Close is idempotent, irreversible and callable from any native thread. }
  INyxCallbackLease = interface(IInterface)
    ['{B8A120C3-C514-4CD2-9267-F973B1BF1142}']
    procedure Close;
    function GetLifetime: INyxCallbackLifetime;
    property Lifetime: INyxCallbackLifetime read GetLifetime;
  end;
  { Explicit owning captures bridge pas2js lexical closures and Pascal reference
    counting. The immutable object retains the supplied interfaces until the last
    callback releases it. It owns no callback/query or caller stack frame.
    Raw class pointers still require an independent caller lifetime contract. }
  INyxCallbackCapture = interface(IInterface)
    ['{B8A120C3-C514-4CD2-9267-F973B1BF1143}']
  end;

{ Create a lease on the thread that owns the enclosing frame. Browser callbacks
  run on their host event loop; the same explicit scope contract still applies.
  For work escaping the enclosing routine, use qualified owning function
  references or a reference-counted callback implementation instead. }
function NewNyxCallbackLease: INyxCallbackLease;
{ Copy/retain a set of interface captures. Nil entries are harmless. Portable
  anonymous callbacks that capture interface locals pass this object explicitly:
  browser lexical functions alone do not retain Pascal interface references. }
function NyxCapture(const AOwners: array of IInterface): INyxCallbackCapture;

implementation

uses
  nyx.scheduler;

type
  TCallbackLifetime = class(TInterfacedObject, INyxCallbackLifetime)
  private
    FCancelled: INyxCancellationScope;
    {$ifndef PAS2JS}
    FThread: TThreadID;
    {$endif}
  public
    constructor Create;
    procedure Close;
    procedure RequireEntry;
  end;
  TCallbackLease = class(TInterfacedObject, INyxCallbackLease)
  private
    FLifetime: INyxCallbackLifetime;
    FOwner: TCallbackLifetime;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close;
    function GetLifetime: INyxCallbackLifetime;
  end;
  TCallbackCapture = class(TInterfacedObject, INyxCallbackCapture)
  private
    FOwners: array of IInterface;
  public
    constructor Create(const AOwners: array of IInterface);
  end;

constructor TCallbackLifetime.Create;
begin
  inherited Create;
  FCancelled := NewNyxCancellationScope;
  {$ifndef PAS2JS}
  FThread := GetCurrentThreadID;
  {$endif}
end;

procedure TCallbackLifetime.Close;
begin
  FCancelled.Cancel;
end;

procedure TCallbackLifetime.RequireEntry;
begin

  if FCancelled.Cancelled then
  begin
    raise ENyxCallbackExpired.Create('Nested callback scope has closed');
  end;
  {$ifndef PAS2JS}

  if GetCurrentThreadID <> FThread then
  begin
    raise ENyxCallbackThread.Create('Nested callbacks require their originating thread');
  end;
  {$endif}
end;

constructor TCallbackLease.Create;
begin
  inherited Create;
  FOwner := TCallbackLifetime.Create;
  FLifetime := FOwner;
end;

destructor TCallbackLease.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TCallbackLease.Close;
begin

  if FLifetime <> nil then
  begin
    FOwner.Close;
  end;
end;

function TCallbackLease.GetLifetime: INyxCallbackLifetime;
begin
  Result := FLifetime;
end;

function NewNyxCallbackLease: INyxCallbackLease;
begin
  Result := TCallbackLease.Create;
end;

constructor TCallbackCapture.Create(const AOwners: array of IInterface);
var
  LIndex: Integer;
begin
  inherited Create;
  SetLength(FOwners, Length(AOwners));
  for LIndex := 0 to High(AOwners) do
  begin
    FOwners[LIndex] := AOwners[LIndex];
  end;
end;

function NyxCapture(const AOwners: array of IInterface): INyxCallbackCapture;
begin
  Result := TCallbackCapture.Create(AOwners);
end;

end.
