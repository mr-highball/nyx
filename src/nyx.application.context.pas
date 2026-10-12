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

unit nyx.application.context;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.state, nyx.collections.registry, nyx.scheduler;

type
  ENyxApplicationContext = class(Exception);

  { UI-only application capability. State is borrowed for this active invocation/
    owner lifetime; do not retain its raw class after application disposal. Returned
    stores and generation tokens have their ordinary independent managed ownership.
    Capture detached inputs, scheduler and generation on UI before starting workers.
    This context retains NO application, renderer, document, event router or tree.
    Every accessor refuses after revocation, before entering a borrowed resolver. }
  INyxApplicationContext = interface(IInterface)
    ['{490EC29D-1A7D-4938-83F6-97A35211EF12}']
    function GetActive: Boolean;
    function GetState: TNyxState;
    function GetCollections: INyxCollections;
    function GetScheduler: INyxScheduler;
    function GetGeneration: INyxCancellationScope;
    property Active: Boolean read GetActive;
    property State: TNyxState read GetState;
    property Collections: INyxCollections read GetCollections;
    property Scheduler: INyxScheduler read GetScheduler;
    property Generation: INyxCancellationScope read GetGeneration;
  end;

  { A host owns this lease and closes it BEFORE teardown or a failed mount.
    Retained callback contexts own only its weak port; they cannot extend the
    borrowed host's lifetime. Last-lease release also closes, on the UI thread. }
  INyxApplicationContextLease = interface(IInterface)
    ['{E9223484-53A5-4B17-A5C1-C1E49242712D}']
    procedure Close;
    function GetContext: INyxApplicationContext;
    property Context: INyxApplicationContext read GetContext;
  end;
  TNyxContextState = function: TNyxState of object;
  TNyxContextCollections = function: INyxCollections of object;
  TNyxContextGeneration = function: INyxCancellationScope of object;

{ Resolver methods are borrowed from an owner that outlives the lease. Construction
  is UI-only and validates every dependency without entering any resolver. Closed
  access and wrong-thread access raise before dereferencing the host's methods. }
function NewNyxApplicationContext(const AScheduler: INyxScheduler;
  AState: TNyxContextState; ACollections: TNyxContextCollections;
  AGeneration: TNyxContextGeneration): INyxApplicationContextLease;

implementation

type
  TContextPort = class(TInterfacedObject, INyxApplicationContext)
  private
    FScheduler: INyxScheduler;
    FState: TNyxContextState;
    FCollections: TNyxContextCollections;
    FGeneration: TNyxContextGeneration;
    FActive: Boolean;
    procedure RequireActive;
  public
    constructor Create(const AScheduler: INyxScheduler; AState: TNyxContextState;
      ACollections: TNyxContextCollections; AGeneration: TNyxContextGeneration);
    procedure Close;
    function GetActive: Boolean;
    function GetState: TNyxState;
    function GetCollections: INyxCollections;
    function GetScheduler: INyxScheduler;
    function GetGeneration: INyxCancellationScope;
  end;
  TContextLease = class(TInterfacedObject, INyxApplicationContextLease)
  private
    FPort: TContextPort;
    FContext: INyxApplicationContext;
  public
    constructor Create(const AScheduler: INyxScheduler; AState: TNyxContextState;
      ACollections: TNyxContextCollections; AGeneration: TNyxContextGeneration);
    destructor Destroy; override;
    procedure Close;
    function GetContext: INyxApplicationContext;
  end;

constructor TContextPort.Create(const AScheduler: INyxScheduler; AState: TNyxContextState;
  ACollections: TNyxContextCollections; AGeneration: TNyxContextGeneration);
begin
  inherited Create;

  if (AScheduler = nil) or not Assigned(AState) or not Assigned(ACollections) or
    not Assigned(AGeneration) then
  begin
    raise ENyxApplicationContext.Create('Application context requires a scheduler and complete resolvers');
  end;
  AScheduler.RequireUI;
  FScheduler := AScheduler;
  FState := AState;
  FCollections := ACollections;
  FGeneration := AGeneration;
  FActive := True;
end;

procedure TContextPort.RequireActive;
begin
  FScheduler.RequireUI;

  if not FActive then
  begin
    raise ENyxApplicationContext.Create('Application context is retired');
  end;
end;

procedure TContextPort.Close;
begin
  FScheduler.RequireUI;
  FActive := False;
  FState := nil;
  FCollections := nil;
  FGeneration := nil;
end;

function TContextPort.GetActive: Boolean;
begin
  FScheduler.RequireUI;
  Result := FActive;
end;

function TContextPort.GetState: TNyxState;
begin
  RequireActive;
  Result := FState();
end;

function TContextPort.GetCollections: INyxCollections;
begin
  RequireActive;
  Result := FCollections();
end;

function TContextPort.GetScheduler: INyxScheduler;
begin
  RequireActive;
  Result := FScheduler;
end;

function TContextPort.GetGeneration: INyxCancellationScope;
begin
  RequireActive;
  Result := FGeneration();
end;

constructor TContextLease.Create(const AScheduler: INyxScheduler; AState: TNyxContextState;
  ACollections: TNyxContextCollections; AGeneration: TNyxContextGeneration);
begin
  inherited Create;
  FPort := TContextPort.Create(AScheduler, AState, ACollections, AGeneration);
  FContext := FPort;
end;

destructor TContextLease.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TContextLease.Close;
begin

  if FPort <> nil then
  begin
    FPort.Close;
  end;
end;

function TContextLease.GetContext: INyxApplicationContext;
begin
  Result := FContext;
end;

function NewNyxApplicationContext(const AScheduler: INyxScheduler;
  AState: TNyxContextState; ACollections: TNyxContextCollections;
  AGeneration: TNyxContextGeneration): INyxApplicationContextLease;
begin
  Result := TContextLease.Create(AScheduler, AState, ACollections, AGeneration);
end;

end.
