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

program nyx_application_context_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.application.context, nyx.callbacks, nyx.events,
  nyx.scheduler, nyx.state, nyx.collections.registry, nyx.behavior
  {$ifdef PAS2JS}, Web{$endif};

type
  { Resolvers belong to an explicitly shorter-lived host. Entry counting makes
    refusal-before-dereference observable, including real native worker access. }
  THost = class
  public
    State: TNyxState;
    Scope: INyxCancellationScope;
    Entries: Integer;
    constructor Create;
    destructor Destroy; override;
    function ResolveState: TNyxState;
    function ResolveCollections: INyxCollections;
    function ResolveGeneration: INyxCancellationScope;
  end;
  TCallback = class(TNyxApplicationEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  {$ifndef PAS2JS}
  TWorker = class(TInterfacedObject, INyxWork)
  public
    Context: INyxApplicationContext;
    Refusals: Integer;
    procedure Execute(const AExecution: INyxExecution);
  end;
  {$endif}

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

constructor THost.Create;
begin
  inherited Create;
  State := TNyxState.Create;
  Scope := NewNyxCancellationScope;
end;

destructor THost.Destroy;
begin
  State.Free;
  Scope := nil;
  inherited Destroy;
end;

function THost.ResolveState: TNyxState;
begin
  Inc(Entries);
  Result := State;
end;

function THost.ResolveCollections: INyxCollections;
begin
  Inc(Entries);
  Result := nil;
end;

function THost.ResolveGeneration: INyxCancellationScope;
begin
  Inc(Entries);
  Result := Scope;
end;

procedure TCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Context.Scheduler.RequireUI;
end;

procedure AccessAll(const AContext: INyxApplicationContext; AIndex: Integer);
var
  LState: TNyxState;
  LCollections: INyxCollections;
  LScope: INyxCancellationScope;
  LScheduler: INyxScheduler;
  LActive: Boolean;
begin
  case AIndex of
    0:
    begin
      LState := AContext.State;
    end;
    1:
    begin
      LCollections := AContext.Collections;
    end;
    2:
    begin
      LScope := AContext.Generation;
    end;
    3:
    begin
      LScheduler := AContext.Scheduler;
    end;
    4:
    begin
      LActive := AContext.Active;
    end;
  end;
end;

{$ifndef PAS2JS}
procedure TWorker.Execute(const AExecution: INyxExecution);
var
  LIndex: Integer;
begin
  for LIndex := 0 to 4 do
  begin
    try
      AccessAll(Context, LIndex);
    except
      on LException: ENyxSchedule do
      begin
        Inc(Refusals);
      end;
    end;
  end;
end;
{$endif}

procedure Run;
var
  LScheduler: INyxScheduler;
  LLease: INyxApplicationContextLease;
  LOtherLease: INyxApplicationContextLease;
  LContext: INyxApplicationContext;
  LCallback: TCallback;
  LCallbackOwner: INyxEventCallback;
  LHost: THost;
  LIndex: Integer;
  LRefused: Boolean;
  {$ifndef PAS2JS}
  LWorker: TWorker;
  LWork: INyxWork;
  LTicket: INyxExecution;
  LMonitor: INyxSchedulerMonitor;
  LPolls: Integer;
  {$endif}
begin
  LScheduler := NewNyxScheduler;
  LHost := THost.Create;
  LCallback := TCallback.Create;
  LCallbackOwner := LCallback;
  try
    LRefused := False;
    try
      LContext := LCallback.Context;
    except
      on LException: ENyxApplicationContext do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'unbound callback refuses runtime context');
    LLease := NewNyxApplicationContext(LScheduler, LHost.ResolveState,
      LHost.ResolveCollections, LHost.ResolveGeneration);
    LContext := LLease.Context;
    Check(LHost.Entries = 0, 'construction never enters a borrowed resolver');
    Check(LContext.Active, 'fresh context is active');
    Check(LContext.State = LHost.State, 'state resolves the exact borrowed instance');
    Check(LContext.Collections = nil, 'collections resolver retains its documented result');
    Check(LContext.Generation = LHost.Scope, 'generation resolves the exact owned scope');
    Check((LHost.Entries = 3) and (LContext.Scheduler = LScheduler),
      'only model access enters host resolvers');
    LCallback.AttachContext(LContext);
    LCallback.AttachContext(LContext);
    Check(LCallback.Context = LContext, 'same-context attachment is idempotent');
    LOtherLease := NewNyxApplicationContext(LScheduler, LHost.ResolveState,
      LHost.ResolveCollections, LHost.ResolveGeneration);
    LRefused := False;
    try
      LCallback.AttachContext(LOtherLease.Context);
    except
      on LException: ENyxApplicationContext do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LCallback.Context = LContext), 'cross-application reattachment refuses atomically');
    LOtherLease := nil;
    {$ifndef PAS2JS}
    LWorker := TWorker.Create;
    LWork := LWorker;
    LWorker.Context := LContext;
    LTicket := LScheduler.Submit(LWork, neThreaded);
    LPolls := 0;
    while LTicket.Status in [nesPending, nesRunning] do
    begin
      Inc(LPolls);

      if LPolls > 2000 then
      begin
        raise Exception.Create('Native context worker observation expired');
      end;
      CheckSynchronize(1);
      Sleep(1);
    end;
    Check((LTicket.Status = nesSucceeded) and (LWorker.Refusals = 5),
      'all actual worker context accesses refuse UI affinity');
    Check(LHost.Entries = 3, 'wrong-thread access never enters a host resolver');
    LTicket := nil;
    LWork := nil;
    {$endif}
    LLease.Close;
    LLease.Close;
    Check(not LContext.Active, 'explicit revocation is idempotent');

    for LIndex := 0 to 3 do
    begin
      LRefused := False;
      try
        AccessAll(LContext, LIndex);
      except
        on LException: ENyxApplicationContext do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'closed accessor refuses before entering its resolver');
    end;
    Check(LHost.Entries = 3, 'revocation retains no callable borrowed resolver');
    FreeAndNil(LHost);
    LRefused := False;
    try
      LContext := LCallback.Context;
    except
      on LException: ENyxApplicationContext do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'retained callback refuses after actual host destruction');
    LLease := nil;
    Check(not LContext.Active, 'retained context remains safely revoked after lease destruction');

    LHost := THost.Create;
    LLease := NewNyxApplicationContext(LScheduler, LHost.ResolveState,
      LHost.ResolveCollections, LHost.ResolveGeneration);
    LContext := LLease.Context;
    LLease := nil;
    Check(not LContext.Active, 'last lease release revokes a separately retained context');
    Check(LHost.Entries = 0, 'automatic closure never enters a host resolver');
  finally
    LOtherLease := nil;
    LLease := nil;
    LCallbackOwner := nil;
    LContext := nil;
    LHost.Free;
    LScheduler.Shutdown;
    {$ifndef PAS2JS}
    Check(Supports(LScheduler, INyxSchedulerMonitor, LMonitor), 'native retirement monitor exists');
    LPolls := 0;
    while LMonitor.WorkerLoad.ActiveWorkers <> 0 do
    begin
      Inc(LPolls);

      if LPolls > 2000 then
      begin
        raise Exception.Create('Native context scheduler retirement expired');
      end;
      CheckSynchronize(1);
      Sleep(1);
    end;
    {$endif}
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' revocable callback context checks';
    document.body.setAttribute('data-nyx-application-context', 'passed');
    document.body.setAttribute('data-nyx-application-context-disposed', 'true');
    {$else}
    WriteLn('PASS ', GChecks, ' revocable callback context checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-application-context', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
