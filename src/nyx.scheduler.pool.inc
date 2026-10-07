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

{ Native implementation included only by nyx.scheduler. A manual-reset event
  represents queue availability/closure. Inspect/reset it under the same lock as
  queue mutation: a worker cannot miss a publication between empty inspection
  and waiting. No user work or interface destruction runs inside that lock. }
constructor TNyxWorkerQueue.Create(const AOptions: TNyxSchedulerOptions);
begin
  inherited Create;
  InitCriticalSection(FLock);
  FOptions := AOptions;
  SetLength(FItems, AOptions.PendingLimit);
  FReady := TEvent.Create(nil, True, False, '');
end;

destructor TNyxWorkerQueue.Destroy;
begin
  Close;
  FReady.Free;
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TNyxWorkerQueue.StartWorkers;
var
  LIndex: Integer;
  LWorker: TNyxPoolWorker;
begin
  { Called on the scheduler's UI owner with an already retained queue interface.
    Each successfully started thread retains its own independent queue lease.
    Partial startup failure closes the queue and wakes earlier idle workers. }

  if FStarted then
  begin
    Exit;
  end;
  FStarted := True;
  try
    for LIndex := 1 to FOptions.WorkerLimit do
    begin
      LWorker := TNyxPoolWorker.Create(Self);
      try
        LWorker.Start;
      except
        Close;
        LWorker.FreeOnTerminate := False;
        LWorker.Free;
        raise;
      end;
    end;
  except
    Close;
    raise;
  end;
end;

procedure TNyxWorkerQueue.Push(ACourier: TNyxCourier);
var
  LIndex: Integer;
  LSlot: Integer;
  LKept: Integer;
  LRemovedCount: Integer;
  LRemoved: array of TNyxCourier;
  LItem: TNyxCourier;
begin
  LRemovedCount := 0;
  LRemoved := nil;
  try
    EnterCriticalSection(FLock);
    try

      if FClosed then
      begin
        raise ENyxSchedule.Create('Worker queue is closed');
      end;
      { Cancelled queued executions reclaim capacity before a new admission.
        Compact in FIFO order. Detach couriers under the lock, but release their
        work afterward because a user destructor may reenter its scheduler. }
      LKept := 0;
      for LIndex := 0 to FCount - 1 do
      begin
        LSlot := (FHead + LIndex) mod Length(FItems);
        LItem := FItems[LSlot];

        if LItem.FExecution.Cancelled then
        begin

          if Length(LRemoved) = 0 then
          begin
            SetLength(LRemoved, FCount);
          end;
          LRemoved[LRemovedCount] := LItem;
          Inc(LRemovedCount);
          FItems[LSlot] := nil;
        end
        else
        begin
          FItems[LSlot] := nil;
          FItems[(FHead + LKept) mod Length(FItems)] := LItem;
          Inc(LKept);
        end;
      end;
      FCount := LKept;

      if FCount = Length(FItems) then
      begin
        raise ENyxScheduleCapacity.Create('Native worker pending capacity is exhausted');
      end;
      FItems[(FHead + FCount) mod Length(FItems)] := ACourier;
      Inc(FCount);
      FReady.SetEvent;
    finally
      LeaveCriticalSection(FLock);
    end;
  finally
    for LIndex := 0 to LRemovedCount - 1 do
    begin
      LRemoved[LIndex].Free;
    end;
  end;
end;

function TNyxWorkerQueue.Take: TNyxCourier;
begin
  while True do
  begin
    EnterCriticalSection(FLock);
    try

      if FClosed then
      begin
        Exit(nil);
      end;

      if FCount > 0 then
      begin
        Result := FItems[FHead];
        FItems[FHead] := nil;
        FHead := (FHead + 1) mod Length(FItems);
        Dec(FCount);
        Inc(FRunning);

        if FCount = 0 then
        begin
          FReady.ResetEvent;
        end;
        Exit;
      end;
      FReady.ResetEvent;
    finally
      LeaveCriticalSection(FLock);
    end;
    FReady.WaitFor(INFINITE);
  end;
end;

procedure TNyxWorkerQueue.WorkerEntered;
begin
  EnterCriticalSection(FLock);
  try
    Inc(FWorkers);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TNyxWorkerQueue.WorkerExited;
begin
  EnterCriticalSection(FLock);
  try
    Dec(FWorkers);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TNyxWorkerQueue.Finished;
begin
  EnterCriticalSection(FLock);
  try
    Dec(FRunning);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TNyxWorkerQueue.Close;
var
  LItems: array of TNyxCourier;
  LIndex: Integer;
begin
  EnterCriticalSection(FLock);
  try

    if FClosed then
    begin
      Exit;
    end;
    FClosed := True;
    { Transfer the whole slot array without destroying user-owned interfaces
      under the lock. Running couriers are owned solely by their workers. }
    LItems := FItems;
    FItems := nil;
    FCount := 0;

    if FReady <> nil then
    begin
      FReady.SetEvent;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
  for LIndex := 0 to Length(LItems) - 1 do
  begin

    if LItems[LIndex] <> nil then
    begin
      LItems[LIndex].FExecution.Cancel;
      LItems[LIndex].Free;
    end;
  end;
end;

function TNyxWorkerQueue.Snapshot: TNyxWorkerPoolSnapshot;
begin
  EnterCriticalSection(FLock);
  try
    Result.WorkerLimit := FOptions.WorkerLimit;
    Result.PendingLimit := FOptions.PendingLimit;
    Result.ActiveWorkers := FWorkers;
    Result.Running := FRunning;
    Result.Pending := FCount;
    Result.Closed := FClosed;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

constructor TNyxPoolWorker.Create(const AQueue: INyxWorkerQueue);
begin
  inherited Create(True);
  FQueue := AQueue;
  FreeOnTerminate := True;
end;

procedure TNyxPoolWorker.Execute;
var
  LCourier: TNyxCourier;
begin
  FQueue.WorkerEntered;
  try
    while True do
    begin
      LCourier := FQueue.Take;

      if LCourier = nil then
      begin
        Break;
      end;
      try
        LCourier.Deliver;
      finally
        FQueue.Finished;
      end;
    end;
  finally
    FQueue.WorkerExited;
  end;
end;
