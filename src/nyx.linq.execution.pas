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
unit nyx.linq.execution;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.linq, nyx.scheduler;

type
  { Computation and publication have separate phases. Succeeded means the whole
    result reached its receiver; Discarded means its receiver had been revoked.
    Failed retains an exact diagnostic (which may be empty). Cancellation is
    cooperative: an entered callback finishes before its inputs are released.
    A cancellation request cannot roll back an already entered/committed receiver.
    No blocking Wait/Join is exposed; the UI must keep pumping its event loop. }
  TNyxQueryPhase = (nqpPending, nqpComputing, nqpAwaitingUI, nqpSucceeded,
    nqpDiscarded, nqpCancelled, nqpFailed);
  INyxQueryRun = interface
    ['{41A7345D-6921-4D77-9854-F3D09A2A731B}']
    procedure Cancel;
    function GetPhase: TNyxQueryPhase;
    function GetFailure: TNyxText;
    property Phase: TNyxQueryPhase read GetPhase;
    property Failure: TNyxText read GetFailure;
  end;

  { Owned publication capability. Publish runs only on the submitting scheduler's
    UI thread, once, after complete successful enumeration. False discards a
    revoked destination. Exceptions fail delivery; a receiver that commits then
    throws must document that committed behavior, just as collection observers do.
    Worker code neither enters this port nor reads a live control/collection. }
  INyxQueryResultPort<T> = interface(IInterface)
    function Publish(const AValues: TNyxSequenceArray<T>): Boolean;
  end;
  TNyxQueryResultMethod<T> = procedure(const AValues: TNyxSequenceArray<T>) of object;

  { Borrowed method lifetime is explicit. Keep this lease while its receiver is
    alive; Close on the UI thread BEFORE destruction/navigation. Last-lease release
    also closes it and must occur on UI. Retained queued ports do not retain the
    lease or receiver.
    Rebinding creates a new generation/port; old work cannot enter the new one.
    Optional AGeneration adds automatic view retirement: use the mounted router's
    NyxEventGeneration. Retained ports refuse entry after navigation/disposal even
    when a receiver lease is still retained. Close still retires a receiver earlier
    than its page; nil preserves explicitly managed non-view receivers. }
  INyxQueryReceiver<T> = interface(IInterface)
    procedure Close;
    function GetPort: INyxQueryResultPort<T>;
    property Port: INyxQueryResultPort<T> read GetPort;
  end;
  TNyxQueryReceiver<T> = class
  public
    class function Bind(const AScheduler: INyxScheduler;
      AReceiver: TNyxQueryResultMethod<T>;
      const AGeneration: INyxCancellationScope = nil): INyxQueryReceiver<T>; static;
  end;

  { Submit captures managed inputs without evaluating them. Default is deferred
    UI enumeration on BOTH targets. neThreaded explicitly requires real workers;
    neAsynchronous uses native workers/browser event-loop capability as documented
    by the scheduler. Neither mode implies concurrent enumeration of one query.
    For worker modes the caller supplies independently owned immutable inputs and
    thread-safe callbacks; an interface/record alone cannot establish that promise.
    Nested scoped callbacks enforce their origin-thread constraint on entry.
    Complete results are queued to UI even for neSequential computation.
    Invalid arguments/capability/capacity refuse synchronously before ownership
    transfer. Compute/receiver failures appear on the retained run, not on UI.
    Cancellation is checked around each MoveNext/Current, not inside arbitrary
    caller code or an operator's blocking preparation (such as sorting).
    Arrays are detached outer vectors; element ownership follows the query contract.
    Optional AGeneration is captured BEFORE any work entry. Cancellation of the
    owning view is observed at computation/delivery checkpoints and by Phase reads;
    queued/entered work retires cooperatively. Completed results retain their
    terminal phase. A generation owns no scheduler, view or receiver. nil leaves
    ordinary application-owned work independent of page navigation. }
  TNyxQuery<T> = class
  public
    class function Submit(const ASequence: INyxSequence<T>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<T>;
      APolicy: TNyxExecutionPolicy = neUIQueue;
      const AGeneration: INyxCancellationScope = nil): INyxQueryRun; static;
  end;

  { Compiler-visible generic implementation types. Application code uses the
    contracts/factories above; these types exist here for native template expansion. }
  INyxQueryProgress = interface(INyxQueryRun)
    ['{407D8C9D-DB25-46C1-A881-E5012F012877}']
    function IsCancelled: Boolean;
    procedure BeginCompute;
    procedure AwaitUI;
    procedure Finish(ADelivered: Boolean);
    procedure Fail(const AMessage: TNyxText);
    procedure Attach(const AExecution: INyxExecution; ADelivery: Boolean);
  end;
  TNyxQueryExecutions = array of INyxExecution;
  TNyxQueryMethodPort<T> = class(TInterfacedObject, INyxQueryResultPort<T>)
  private
    FScheduler: INyxScheduler;
    FReceiver: TNyxQueryResultMethod<T>;
    FGeneration: INyxCancellationScope;
  public
    constructor Create(const AScheduler: INyxScheduler; AReceiver: TNyxQueryResultMethod<T>;
      const AGeneration: INyxCancellationScope);
    procedure Close;
    function Publish(const AValues: TNyxSequenceArray<T>): Boolean;
  end;
  TNyxQueryMethodLease<T> = class(TInterfacedObject, INyxQueryReceiver<T>)
  private
    FPort: TNyxQueryMethodPort<T>;
    FOwnedPort: INyxQueryResultPort<T>;
  public
    constructor Create(const AScheduler: INyxScheduler; AReceiver: TNyxQueryResultMethod<T>;
      const AGeneration: INyxCancellationScope);
    destructor Destroy; override;
    procedure Close;
    function GetPort: INyxQueryResultPort<T>;
  end;
  TNyxQueryDelivery<T> = class(TInterfacedObject, INyxWork)
  private
    FValues: TNyxSequenceArray<T>;
    FPort: INyxQueryResultPort<T>;
    FProgress: INyxQueryProgress;
  public
    constructor Create(const AValues: TNyxSequenceArray<T>;
      const APort: INyxQueryResultPort<T>; const AProgress: INyxQueryProgress);
    procedure Execute(const AExecution: INyxExecution);
  end;
  TNyxQueryCompute<T> = class(TInterfacedObject, INyxWork)
  private
    FSequence: INyxSequence<T>;
    FScheduler: INyxScheduler;
    FPort: INyxQueryResultPort<T>;
    FProgress: INyxQueryProgress;
  public
    constructor Create(const ASequence: INyxSequence<T>; const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<T>; const AProgress: INyxQueryProgress);
    procedure Execute(const AExecution: INyxExecution);
  end;

{ Compiler support functions used by expanded native generic bodies. Application
  authors use Submit; these create independent receipts and preserve native ANSI
  exception messages at the portable UTF-8 diagnostic boundary. }
function NewNyxQueryProgress(const AGeneration: INyxCancellationScope = nil): INyxQueryProgress;
{ Owned callback context, containing no work or UI receiver. Status/Failure
  describe the entered scheduler execution. Cancelled additionally observes the
  whole query/view generation. Cancel cancels the whole run. Pass this context
  as PostUI's parent so late child work follows navigation, even before another
  query row observes retirement. Child executions keep their own diagnostics. }
function NewNyxQueryExecution(const AExecution: INyxExecution;
  const AProgress: INyxQueryProgress): INyxExecution;
function NyxQueryDiagnostic(AException: Exception): TNyxText;

implementation

{ This independent receipt owns diagnostics/tokens, never a query, scheduler,
  view or receiver. Its lock covers native worker/UI reads and cancellation.
  No user callback or scheduler operation executes while that lock is held. }
type
  TQueryExecution = class(TInterfacedObject, INyxExecution)
  private
    FExecution: INyxExecution;
    FProgress: INyxQueryProgress;
  public
    constructor Create(const AExecution: INyxExecution; const AProgress: INyxQueryProgress);
    procedure Cancel;
    function GetCancelled: Boolean;
    function GetStatus: TNyxExecutionStatus;
    function GetFailure: TNyxText;
  end;
  TQueryProgress = class(TInterfacedObject, INyxQueryProgress, INyxQueryRun)
  private
    FPhase: TNyxQueryPhase;
    FFailure: TNyxText;
    FCancelled: Boolean;
    FComputes: TNyxQueryExecutions;
    FDelivery: INyxExecution;
    FGeneration: INyxCancellationScope;
    {$ifndef PAS2JS}FLock: TRTLCriticalSection;{$endif}
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(const AGeneration: INyxCancellationScope);
    destructor Destroy; override;
    procedure Cancel;
    function GetPhase: TNyxQueryPhase;
    function GetFailure: TNyxText;
    function IsCancelled: Boolean;
    procedure BeginCompute;
    procedure AwaitUI;
    procedure Finish(ADelivered: Boolean);
    procedure Fail(const AMessage: TNyxText);
    procedure Attach(const AExecution: INyxExecution; ADelivery: Boolean);
  end;

constructor TQueryExecution.Create(const AExecution: INyxExecution;
  const AProgress: INyxQueryProgress);
begin
  inherited Create;
  FExecution := AExecution;
  FProgress := AProgress;
end;

procedure TQueryExecution.Cancel;
begin
  FExecution.Cancel;
  FProgress.Cancel;
end;

function TQueryExecution.GetCancelled: Boolean;
begin
  Result := FExecution.Cancelled or FProgress.IsCancelled;
end;

function TQueryExecution.GetStatus: TNyxExecutionStatus;
begin
  Result := FExecution.Status;
end;

function TQueryExecution.GetFailure: TNyxText;
begin
  Result := FExecution.Failure;
end;

function NewNyxQueryExecution(const AExecution: INyxExecution;
  const AProgress: INyxQueryProgress): INyxExecution;
begin

  if (AExecution = nil) or (AProgress = nil) then
  begin
    raise ENyxQuery.Create('Query execution context requires an execution and progress');
  end;
  Result := TQueryExecution.Create(AExecution, AProgress);
end;

function NewNyxQueryProgress(const AGeneration: INyxCancellationScope): INyxQueryProgress;
begin
  Result := TQueryProgress.Create(AGeneration);
end;

function NyxQueryDiagnostic(AException: Exception): TNyxText;
begin
  {$ifdef PAS2JS}Result := AException.Message;{$else}
  Result := UTF8Encode(UnicodeString(AException.Message));{$endif}
end;

constructor TQueryProgress.Create(const AGeneration: INyxCancellationScope);
begin
  inherited Create;
  FGeneration := AGeneration;
  {$ifndef PAS2JS}InitCriticalSection(FLock);{$endif}
end;

destructor TQueryProgress.Destroy;
begin
  {$ifndef PAS2JS}DoneCriticalSection(FLock);{$endif}
  inherited Destroy;
end;

procedure TQueryProgress.Lock;
begin
  {$ifndef PAS2JS}EnterCriticalSection(FLock);{$endif}
end;

procedure TQueryProgress.Unlock;
begin
  {$ifndef PAS2JS}LeaveCriticalSection(FLock);{$endif}
end;

procedure TQueryProgress.Cancel;
var
  LComputes: TNyxQueryExecutions;
  LDelivery: INyxExecution;
  LIndex: Integer;
begin
  Lock;
  try
    FCancelled := True;
    LComputes := Copy(FComputes, 0, Length(FComputes));
    LDelivery := FDelivery;

    if FPhase in [nqpPending, nqpComputing, nqpAwaitingUI] then
    begin
      FPhase := nqpCancelled;
    end;
  finally
    Unlock;
  end;
  { Cancel scheduler tickets outside the receipt lock. Pending couriers can then
    reclaim bounded pool capacity without entering a cancelled query body. }
  for LIndex := 0 to High(LComputes) do
  begin
    LComputes[LIndex].Cancel;
  end;

  if LDelivery <> nil then
  begin
    LDelivery.Cancel;
  end;
end;

function TQueryProgress.IsCancelled: Boolean;
begin
  Lock;
  try
    Result := FCancelled;
  finally
    Unlock;
  end;
  { The immutable link owns only a cancellation token. Observe outside our lock;
    neither the view nor a user callback can be entered from a native worker.
    Already-cancelled polling need not repeatedly copy the scheduler ticket list. }

  if not Result and (FGeneration <> nil) and FGeneration.Cancelled then
  begin
    Cancel;
    Result := True;
  end;
end;

function TQueryProgress.GetPhase: TNyxQueryPhase;
var
  LComputes: TNyxQueryExecutions;
  LDelivery: INyxExecution;
  LIndex: Integer;
begin
  IsCancelled;
  Lock;
  try
    LComputes := Copy(FComputes, 0, Length(FComputes));
    LDelivery := FDelivery;
  finally
    Unlock;
  end;
  { Scheduler shutdown may skip a courier entirely. Reflect that cancellation
    even though its Execute method never entered this receipt. }

  if (LDelivery <> nil) and LDelivery.Cancelled then
  begin
    Cancel;
  end;
  for LIndex := 0 to High(LComputes) do
  begin

    if LComputes[LIndex].Cancelled then
    begin
      Cancel;
      Break;
    end;
  end;
  Lock;
  try
    Result := FPhase;
  finally
    Unlock;
  end;
end;

function TQueryProgress.GetFailure: TNyxText;
begin
  Lock;
  try
    Result := FFailure;
  finally
    Unlock;
  end;
end;

procedure TQueryProgress.BeginCompute;
begin
  Lock;
  try

    if not FCancelled then
    begin
      FPhase := nqpComputing;
    end;
  finally
    Unlock;
  end;
end;

procedure TQueryProgress.AwaitUI;
begin
  Lock;
  try

    if not FCancelled then
    begin
      FPhase := nqpAwaitingUI;
    end;
  finally
    Unlock;
  end;
end;

procedure TQueryProgress.Finish(ADelivered: Boolean);
begin
  Lock;
  try

    if not FCancelled then
    begin

      if ADelivered then
      begin
        FPhase := nqpSucceeded;
      end
      else
      begin
        FPhase := nqpDiscarded;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TQueryProgress.Fail(const AMessage: TNyxText);
begin
  Lock;
  try
    { Parallel siblings can fail after the first observed failure has revoked
      them. Keep that first terminal diagnostic rather than race to overwrite it. }

    if FPhase = nqpFailed then
    begin
      Exit;
    end;
    FFailure := AMessage;

    if not FCancelled then
    begin
      FPhase := nqpFailed;
    end;
  finally
    Unlock;
  end;
end;

procedure TQueryProgress.Attach(const AExecution: INyxExecution; ADelivery: Boolean);
var
  LIndex, LKept: Integer;
  LCancelled: Boolean;
begin
  Lock;
  try

    if ADelivery then
    begin
      FDelivery := AExecution;
    end
    else
    begin
      { Keep only active tickets. A partitioned query can submit many chunks;
        retaining every completed ticket would grow memory with total partitions.
        Scheduler status access is synchronized and enters no user callbacks. }
      LKept := 0;
      for LIndex := 0 to High(FComputes) do
      begin

        if FComputes[LIndex].Status in [nesPending, nesRunning] then
        begin
          FComputes[LKept] := FComputes[LIndex];
          Inc(LKept);
        end;
      end;
      SetLength(FComputes, LKept + 1);
      FComputes[LKept] := AExecution;
    end;
    LCancelled := FCancelled;
  finally
    Unlock;
  end;

  if LCancelled then
  begin
    AExecution.Cancel;
  end;
end;

constructor TNyxQueryMethodPort<T>.Create(const AScheduler: INyxScheduler;
  AReceiver: TNyxQueryResultMethod<T>; const AGeneration: INyxCancellationScope);
begin
  inherited Create;

  if (AScheduler = nil) or not Assigned(AReceiver) then
  begin
    raise ENyxQuery.Create('A query receiver requires a scheduler and method');
  end;
  AScheduler.RequireUI;
  FScheduler := AScheduler;
  FReceiver := AReceiver;
  FGeneration := AGeneration;
end;

procedure TNyxQueryMethodPort<T>.Close;
begin
  FScheduler.RequireUI;
  FReceiver := nil;
end;

function TNyxQueryMethodPort<T>.Publish(const AValues: TNyxSequenceArray<T>): Boolean;
begin
  FScheduler.RequireUI;
  { Retained ports may outlive the borrowed method's page. The generation check
    precedes EVERY possible method entry, including direct extension callers. }

  if (FGeneration <> nil) and FGeneration.Cancelled then
  begin
    Close;
  end;
  Result := Assigned(FReceiver);

  if Result then
  begin
    FReceiver(AValues);
  end;
end;

constructor TNyxQueryMethodLease<T>.Create(const AScheduler: INyxScheduler;
  AReceiver: TNyxQueryResultMethod<T>; const AGeneration: INyxCancellationScope);
begin
  inherited Create;
  FPort := TNyxQueryMethodPort<T>.Create(AScheduler, AReceiver, AGeneration);
  FOwnedPort := FPort;
end;

destructor TNyxQueryMethodLease<T>.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TNyxQueryMethodLease<T>.Close;
begin

  if FPort <> nil then
  begin
    FPort.Close;
  end;
end;

function TNyxQueryMethodLease<T>.GetPort: INyxQueryResultPort<T>;
begin
  Result := FOwnedPort;
end;

class function TNyxQueryReceiver<T>.Bind(const AScheduler: INyxScheduler;
  AReceiver: TNyxQueryResultMethod<T>;
  const AGeneration: INyxCancellationScope): INyxQueryReceiver<T>;
begin
  Result := TNyxQueryMethodLease<T>.Create(AScheduler, AReceiver, AGeneration);
end;

constructor TNyxQueryDelivery<T>.Create(const AValues: TNyxSequenceArray<T>;
  const APort: INyxQueryResultPort<T>; const AProgress: INyxQueryProgress);
var
  LIndex: Integer;
begin
  inherited Create;
  SetLength(FValues, Length(AValues));
  for LIndex := 0 to High(FValues) do
  begin
    FValues[LIndex] := AValues[LIndex];
  end;
  FPort := APort;
  FProgress := AProgress;
end;

procedure TNyxQueryDelivery<T>.Execute(const AExecution: INyxExecution);
begin

  if FProgress.IsCancelled or AExecution.Cancelled then
  begin
    FProgress.Cancel;
    Exit;
  end;
  try
    FProgress.Finish(FPort.Publish(FValues));
  except
    on LException: Exception do
    begin
      FProgress.Fail(NyxQueryDiagnostic(LException));
    end;
  end;
end;

constructor TNyxQueryCompute<T>.Create(const ASequence: INyxSequence<T>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<T>;
  const AProgress: INyxQueryProgress);
begin
  inherited Create;
  FSequence := ASequence;
  FScheduler := AScheduler;
  FPort := APort;
  FProgress := AProgress;
end;

procedure TNyxQueryCompute<T>.Execute(const AExecution: INyxExecution);
var
  LCursor: INyxEnumerator<T>;
  LValues: TNyxSequenceArray<T>;
  LCount, LCapacity: Integer;
  LDelivery: INyxWork;
  LExecution: INyxExecution;
begin
  try

    if FProgress.IsCancelled or AExecution.Cancelled then
    begin
      FProgress.Cancel;
      Exit;
    end;
    FProgress.BeginCompute;
    LCursor := FSequence.GetEnumerator;
    LCount := 0;
    LCapacity := 0;
    while not FProgress.IsCancelled and not AExecution.Cancelled do
    begin

      if not LCursor.MoveNext then
      begin
        Break;
      end;

      if FProgress.IsCancelled or AExecution.Cancelled then
      begin
        Break;
      end;

      if LCount = LCapacity then
      begin

        if LCapacity > (High(Integer) div 2) then
        begin
          raise ENyxQuery.Create('Query result capacity exceeds Integer range');
        end;

        if LCapacity = 0 then
        begin
          LCapacity := 16;
        end
        else
        begin
          LCapacity := LCapacity * 2;
        end;
        SetLength(LValues, LCapacity);
      end;
      LValues[LCount] := LCursor.Current;
      Inc(LCount);
    end;

    if FProgress.IsCancelled or AExecution.Cancelled then
    begin
      FProgress.Cancel;
      Exit;
    end;
    SetLength(LValues, LCount);
    LCursor := nil;
    FSequence := nil;
    LDelivery := TNyxQueryDelivery<T>.Create(LValues, FPort, FProgress);
    FProgress.AwaitUI;
    LExecution := FScheduler.PostUI(LDelivery, AExecution);
    FProgress.Attach(LExecution, True);
  except
    on LException: Exception do
    begin
      FProgress.Fail(NyxQueryDiagnostic(LException));
    end;
  end;
end;

class function TNyxQuery<T>.Submit(const ASequence: INyxSequence<T>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<T>;
  APolicy: TNyxExecutionPolicy; const AGeneration: INyxCancellationScope): INyxQueryRun;
var
  LProgress: INyxQueryProgress;
  LWork: INyxWork;
  LExecution: INyxExecution;
begin

  if (ASequence = nil) or (AScheduler = nil) or (APort = nil) then
  begin
    raise ENyxQuery.Create('Query submission requires a sequence, scheduler and result port');
  end;
  AScheduler.Admit(APolicy);
  LProgress := NewNyxQueryProgress(AGeneration);
  LWork := TNyxQueryCompute<T>.Create(ASequence, AScheduler, APort, LProgress);
  LExecution := AScheduler.Submit(LWork, APolicy);
  LProgress.Attach(LExecution, False);
  Result := LProgress;
end;

end.
