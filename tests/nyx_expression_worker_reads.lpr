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

program nyx_expression_worker_reads;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, SyncObjs, nyx.text, nyx.state, nyx.expressions, nyx.scheduler;

type
  { Both native tasks retain the SAME immutable expression/frame. Events are
    test-only barriers; no UI or live state is retained or read by either task. }
  TReadWork = class(TInterfacedObject, INyxWork)
  public
    Expression: INyxTextExpression;
    Inputs: INyxExpressionInputs;
    Entered: TEvent;
    ReleaseGate: TEvent;
    WorkerID: TThreadID;
    ValuesMatch: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Execute(const AExecution: INyxExecution);
  end;

constructor TReadWork.Create;
begin
  inherited Create;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseGate := TEvent.Create(nil, True, False, '');
end;

destructor TReadWork.Destroy;
begin
  Entered.Free;
  ReleaseGate.Free;
  inherited Destroy;
end;

procedure TReadWork.Execute(const AExecution: INyxExecution);
var
  LIndex: Integer;
begin
  WorkerID := TThread.CurrentThread.ThreadID;
  Entered.SetEvent;

  if ReleaseGate.WaitFor(5000) <> wrSignaled then
  begin
    raise Exception.Create('Worker release gate timed out');
  end;
  ValuesMatch := True;
  for LIndex := 1 to 1000 do
  begin
    ValuesMatch := ValuesMatch and
      (Expression.Evaluate(Inputs) = TNyxText('Moon 🌙: 28'));

    if AExecution.Cancelled then
    begin
      Break;
    end;
  end;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Concurrent expression reads: ' + AReason);
  end;
end;

var
  LState: TNyxState;
  LInputs: INyxExpressionInputs;
  LExpression: INyxTextExpression;
  LScheduler: INyxScheduler;
  LFirst: TReadWork;
  LSecond: TReadWork;
  LFirstLease: INyxWork;
  LSecondLease: INyxWork;
  LFirstRun: INyxExecution;
  LSecondRun: INyxExecution;
  LStarted: QWord;
begin
  LState := nil;
  try
    LState := TNyxState.Create;
    LState.SetValue(NyxIntegerState('quantity'), 7);
    LInputs := CaptureNyxExpressionInputs(LState);
    LExpression := NyxTextExpression(TNyxText('Moon 🌙: ')).Append(
      NyxIntegerExpression(NyxIntegerState('quantity')).Times(NyxIntegerExpression(4)).AsText);
    FreeAndNil(LState);
    LScheduler := NewNyxScheduler(TNyxSchedulerOptions.Defaults.Workers(2));
    LFirst := TReadWork.Create;
    LFirstLease := LFirst;
    LSecond := TReadWork.Create;
    LSecondLease := LSecond;
    LFirst.Expression := LExpression;
    LSecond.Expression := LExpression;
    LFirst.Inputs := LInputs;
    LSecond.Inputs := LInputs;
    LFirstRun := LScheduler.Submit(LFirstLease, neThreaded);
    LSecondRun := LScheduler.Submit(LSecondLease, neThreaded);
    Check((LFirst.Entered.WaitFor(5000) = wrSignaled) and
      (LSecond.Entered.WaitFor(5000) = wrSignaled), 'both worker tasks entered before either was released');
    Check((LFirst.WorkerID <> MainThreadID) and (LSecond.WorkerID <> MainThreadID) and
      (LFirst.WorkerID <> LSecond.WorkerID), 'two distinct real workers entered outside UI');
    LFirst.ReleaseGate.SetEvent;
    LSecond.ReleaseGate.SetEvent;
    LStarted := GetTickCount64;
    while (LFirstRun.Status in [nesPending, nesRunning]) or
      (LSecondRun.Status in [nesPending, nesRunning]) do
    begin
      CheckSynchronize;

      if GetTickCount64 - LStarted > 10000 then
      begin
        raise Exception.Create('Concurrent expression worker retirement timed out');
      end;
      Sleep(1);
    end;
    Check((LFirstRun.Status = nesSucceeded) and (LSecondRun.Status = nesSucceeded),
      'both shared immutable enumerations finish successfully');
    Check(LFirst.ValuesMatch and LSecond.ValuesMatch,
      '2000 evaluations preserve exact supplementary Unicode and arithmetic');
    WriteLn('PASS 4 concurrent native worker checks / 2000 evaluations');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  { Release gates even on failure before retirement. Worker-owned work leases
    keep their events/frame alive while UI pumps ordinary scheduler completion. }
  if LFirst <> nil then
  begin
    LFirst.ReleaseGate.SetEvent;
  end;

  if LSecond <> nil then
  begin
    LSecond.ReleaseGate.SetEvent;
  end;

  if LScheduler <> nil then
  begin
    LScheduler.Shutdown;
  end;
  LFirstRun := nil;
  LSecondRun := nil;
  LFirstLease := nil;
  LSecondLease := nil;
  LScheduler := nil;
  LInputs := nil;
  LExpression := nil;
  LState.Free;
end.
