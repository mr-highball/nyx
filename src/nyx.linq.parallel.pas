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
unit nyx.linq.parallel;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.linq, nyx.linq.execution, nyx.scheduler;

type
  { Workers requires actual native threads, even for an empty input. DeferredUI
    explicitly chooses browser/native event-loop work, never a thread simulation.
    Source order preserves every input ordinal. Completion order preserves order
    inside each partition, but concatenates partitions as their UI replies arrive.
    Stop cancels siblings at the first observed partition failure. Collect runs
    other partitions and retains one diagnostic per failed partition; the failing
    partition stops at its first failing row. Neither policy publishes partial data. }
  TNyxParallelExecution = (npxWorkers, npxDeferredUI);
  TNyxParallelOrder = (npoSource, npoCompletion);
  TNyxParallelFailurePolicy = (npfStop, npfCollect);
  TNyxParallelOptions = record
  private
    FConcurrency: Integer;
    FPartitionSize: Integer;
    FMaximumInput: Integer;
    FExecution: TNyxParallelExecution;
    FOrder: TNyxParallelOrder;
    FFailures: TNyxParallelFailurePolicy;
  public
    { Four in-flight partitions, 128 rows each, at most 1,048,576 input rows,
      required workers, source order and stop-on-first-observed failure. }
    class function Defaults: TNyxParallelOptions; static;
    function Concurrency(ACount: Integer): TNyxParallelOptions;
    function PartitionSize(ACount: Integer): TNyxParallelOptions;
    function MaximumInput(ACount: Integer): TNyxParallelOptions;
    function Execution(AMode: TNyxParallelExecution): TNyxParallelOptions;
    function Order(AMode: TNyxParallelOrder): TNyxParallelOptions;
    function Failures(APolicy: TNyxParallelFailurePolicy): TNyxParallelOptions;
    procedure Validate;
    property ConcurrentPartitions: Integer read FConcurrency;
    property RowsPerPartition: Integer read FPartitionSize;
    property InputLimit: Integer read FMaximumInput;
    property ExecutionMode: TNyxParallelExecution read FExecution;
    property ResultOrder: TNyxParallelOrder read FOrder;
    property FailurePolicy: TNyxParallelFailurePolicy read FFailures;
  end;

  { Factory entry is on UI, once per admitted partition. Each returned managed
    selector owns independent mutable state or explicitly shared synchronized/
    immutable state. It must outlive its work and must not access live UI/stores.
    Sharing a raw class or captured interface does not make that object safe.
    SourceOffset identifies the partition's first zero-based input ordinal. }
  INyxPartitionSelectorFactory<TSource, TResult> = interface(IInterface)
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<TSource, TResult>;
  end;
  TNyxPartitionSelector<TSource, TResult> = class
  public
    { Global pure functions need no borrowed frame. A fresh adapter is created for
      each partition; thread safety of global data remains the caller's contract. }
    class function From(AFunction: TNyxSelectorFunction<TSource, TResult>):
      INyxPartitionSelectorFactory<TSource, TResult>; static;
  end;

  TNyxPartitionFailure = record
    Partition: Integer;
    SourceOffset: Integer;
    Message: TNyxText;
  end;
  { Diagnostics own exact text and outlive all work/UI. Failures appear in UI
    observation order, independent of result order. Getters are native synchronized;
    FailureAt refuses an absent index. Cancellation/failed delivery may have no
    partition diagnostic; the ordinary Phase/Failure still explain that outcome. }
  INyxParallelRun = interface(INyxQueryRun)
    ['{D7BDA0B4-2636-46BD-9401-24B52DAA91A3}']
    function GetFailureCount: Integer;
    function FailureAt(AIndex: Integer): TNyxPartitionFailure;
    property FailureCount: Integer read GetFailureCount;
  end;

  { Capture the outer input vector synchronously on UI, then defer admission of
    selector partitions. Managed elements follow ordinary Pascal ownership: raw
    class pointers and mutable inner arrays are not deep cloned. Worker inputs
    must be independently owned immutable values. An explicit query.ToArray may
    supply the capture; this factory never shares a mutable query enumerator.
    Concurrency is an upper bound on in-flight partitions, also limited by the
    shared scheduler's actual pool. No thread is created/joined by this layer.
    Capacity/factory failures after submission fail the retained receipt and stop
    siblings; there is no silent UI fallback/retry. Callbacks already entered may
    finish, so pure selectors are recommended. Result delivery is once on UI via
    the same revocable/revision-aware ports as ordinary queries. No partial result
    reaches the destination under either failure policy. Cancellation is checked
    between rows and cannot undo a callback/receiver that has already entered. }
  TNyxParallelSelect<TSource, TResult> = class
  public
    class function Submit(const AValues: array of TSource;
      const AFactory: INyxPartitionSelectorFactory<TSource, TResult>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
      const AOptions: TNyxParallelOptions): INyxParallelRun; static;
  end;

  { Compiler-visible support contracts/types for native generic expansion. No
    application, renderer or collection handle crosses the worker completion. }
  INyxPartitionDiagnostics = interface
    ['{DA7819C1-757A-43BE-889C-EAE6DCA92AA9}']
    procedure Add(APartition, ASourceOffset: Integer; const AMessage: TNyxText);
    function Count: Integer;
    function Item(AIndex: Integer): TNyxPartitionFailure;
  end;
  INyxPartitionSink<TResult> = interface(IInterface)
    procedure Completed(APartition, ASourceOffset: Integer;
      const AValues: TNyxSequenceArray<TResult>; AFailed: Boolean;
      const AFailure: TNyxText);
  end;
  TNyxGlobalPartitionFactory<TSource, TResult> = class(TInterfacedObject,
    INyxPartitionSelectorFactory<TSource, TResult>)
  private
    FFunction: TNyxSelectorFunction<TSource, TResult>;
  public
    constructor Create(AFunction: TNyxSelectorFunction<TSource, TResult>);
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<TSource, TResult>;
  end;
  TNyxPartitionReply<TResult> = class(TInterfacedObject, INyxWork)
  private
    FSink: INyxPartitionSink<TResult>;
    FProgress: INyxQueryProgress;
    FValues: TNyxSequenceArray<TResult>;
    FPartition: Integer;
    FOffset: Integer;
    FFailed: Boolean;
    FFailure: TNyxText;
  public
    constructor Create(const ASink: INyxPartitionSink<TResult>; const AProgress: INyxQueryProgress;
      APartition, AOffset: Integer; const AValues: TNyxSequenceArray<TResult>;
      AFailed: Boolean; const AFailure: TNyxText);
    procedure Execute(const AExecution: INyxExecution);
  end;
  TNyxPartitionWork<TSource, TResult> = class(TInterfacedObject, INyxWork)
  private
    FValues: TNyxSequenceArray<TSource>;
    FSelector: INyxSelector<TSource, TResult>;
    FScheduler: INyxScheduler;
    FSink: INyxPartitionSink<TResult>;
    FProgress: INyxQueryProgress;
    FPartition: Integer;
    FOffset: Integer;
  public
    constructor Create(const AValues: array of TSource;
      const ASelector: INyxSelector<TSource, TResult>; const AScheduler: INyxScheduler;
      const ASink: INyxPartitionSink<TResult>; const AProgress: INyxQueryProgress;
      APartition, AOffset: Integer);
    procedure Execute(const AExecution: INyxExecution);
  end;
  TNyxPartitionCoordinator<TSource, TResult> = class(TInterfacedObject,
    INyxWork, INyxPartitionSink<TResult>)
  private
    FInput: TNyxSequenceArray<TSource>;
    FResults: TNyxSequenceArray<TResult>;
    FFactory: INyxPartitionSelectorFactory<TSource, TResult>;
    FScheduler: INyxScheduler;
    FPort: INyxQueryResultPort<TResult>;
    FProgress: INyxQueryProgress;
    FDiagnostics: INyxPartitionDiagnostics;
    FOptions: TNyxParallelOptions;
    FNext: Integer;
    FPartition: Integer;
    FActive: Integer;
    FWritten: Integer;
    FFinished: Boolean;
    procedure Launch;
    procedure Complete;
  public
    constructor Create(const AValues: array of TSource;
      const AFactory: INyxPartitionSelectorFactory<TSource, TResult>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
      const AProgress: INyxQueryProgress; const ADiagnostics: INyxPartitionDiagnostics;
      const AOptions: TNyxParallelOptions);
    procedure Execute(const AExecution: INyxExecution);
    procedure Completed(APartition, ASourceOffset: Integer;
      const AValues: TNyxSequenceArray<TResult>; AFailed: Boolean; const AFailure: TNyxText);
  end;

function NewNyxPartitionDiagnostics: INyxPartitionDiagnostics;
function NewNyxParallelRun(const AProgress: INyxQueryProgress;
  const ADiagnostics: INyxPartitionDiagnostics): INyxParallelRun;

implementation

type
  TPartitionDiagnostics = class(TInterfacedObject, INyxPartitionDiagnostics)
  private
    FItems: array of TNyxPartitionFailure;
    {$ifndef PAS2JS}FLock: TRTLCriticalSection;{$endif}
    procedure Lock;
    procedure Unlock;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(APartition, ASourceOffset: Integer; const AMessage: TNyxText);
    function Count: Integer;
    function Item(AIndex: Integer): TNyxPartitionFailure;
  end;
  TParallelRun = class(TInterfacedObject, INyxParallelRun, INyxQueryRun)
  private
    FProgress: INyxQueryProgress;
    FDiagnostics: INyxPartitionDiagnostics;
  public
    constructor Create(const AProgress: INyxQueryProgress; const ADiagnostics: INyxPartitionDiagnostics);
    procedure Cancel;
    function GetPhase: TNyxQueryPhase;
    function GetFailure: TNyxText;
    function GetFailureCount: Integer;
    function FailureAt(AIndex: Integer): TNyxPartitionFailure;
  end;

class function TNyxParallelOptions.Defaults: TNyxParallelOptions;
begin
  Result.FConcurrency := 4;
  Result.FPartitionSize := 128;
  Result.FMaximumInput := 1048576;
  Result.FExecution := npxWorkers;
  Result.FOrder := npoSource;
  Result.FFailures := npfStop;
end;

function TNyxParallelOptions.Concurrency(ACount: Integer): TNyxParallelOptions;
begin

  if (ACount < 1) or (ACount > 64) then
  begin
    raise ENyxQuery.Create('Parallel concurrency must be between 1 and 64');
  end;
  Result := Self;
  Result.FConcurrency := ACount;
end;

function TNyxParallelOptions.PartitionSize(ACount: Integer): TNyxParallelOptions;
begin

  if (ACount < 1) or (ACount > 65536) then
  begin
    raise ENyxQuery.Create('Partition size must be between 1 and 65536');
  end;
  Result := Self;
  Result.FPartitionSize := ACount;
end;

function TNyxParallelOptions.MaximumInput(ACount: Integer): TNyxParallelOptions;
begin

  if (ACount < 1) or (ACount > 16777216) then
  begin
    raise ENyxQuery.Create('Parallel input limit must be between 1 and 16777216');
  end;
  Result := Self;
  Result.FMaximumInput := ACount;
end;

function TNyxParallelOptions.Execution(AMode: TNyxParallelExecution): TNyxParallelOptions;
begin
  Result := Self;
  Result.FExecution := AMode;
end;

function TNyxParallelOptions.Order(AMode: TNyxParallelOrder): TNyxParallelOptions;
begin
  Result := Self;
  Result.FOrder := AMode;
end;

function TNyxParallelOptions.Failures(APolicy: TNyxParallelFailurePolicy): TNyxParallelOptions;
begin
  Result := Self;
  Result.FFailures := APolicy;
end;

procedure TNyxParallelOptions.Validate;
begin

  if (FConcurrency < 1) or (FConcurrency > 64) or
    (FPartitionSize < 1) or (FPartitionSize > 65536) or
    (FMaximumInput < 1) or (FMaximumInput > 16777216) then
  begin
    raise ENyxQuery.Create('Use initialized valid parallel options or Defaults');
  end;
end;

constructor TPartitionDiagnostics.Create;
begin
  inherited Create;
  {$ifndef PAS2JS}InitCriticalSection(FLock);{$endif}
end;

destructor TPartitionDiagnostics.Destroy;
begin
  {$ifndef PAS2JS}DoneCriticalSection(FLock);{$endif}
  inherited Destroy;
end;

procedure TPartitionDiagnostics.Lock;
begin
  {$ifndef PAS2JS}EnterCriticalSection(FLock);{$endif}
end;

procedure TPartitionDiagnostics.Unlock;
begin
  {$ifndef PAS2JS}LeaveCriticalSection(FLock);{$endif}
end;

procedure TPartitionDiagnostics.Add(APartition, ASourceOffset: Integer; const AMessage: TNyxText);
var
  LIndex: Integer;
begin
  Lock;
  try
    LIndex := Length(FItems);
    SetLength(FItems, LIndex + 1);
    FItems[LIndex].Partition := APartition;
    FItems[LIndex].SourceOffset := ASourceOffset;
    FItems[LIndex].Message := AMessage;
  finally
    Unlock;
  end;
end;

function TPartitionDiagnostics.Count: Integer;
begin
  Lock;
  try
    Result := Length(FItems);
  finally
    Unlock;
  end;
end;

function TPartitionDiagnostics.Item(AIndex: Integer): TNyxPartitionFailure;
begin
  Lock;
  try

    if (AIndex < 0) or (AIndex >= Length(FItems)) then
    begin
      raise ENyxQuery.Create('Partition failure index is absent');
    end;
    Result := FItems[AIndex];
  finally
    Unlock;
  end;
end;

constructor TParallelRun.Create(const AProgress: INyxQueryProgress;
  const ADiagnostics: INyxPartitionDiagnostics);
begin
  inherited Create;
  FProgress := AProgress;
  FDiagnostics := ADiagnostics;
end;

procedure TParallelRun.Cancel;
begin
  FProgress.Cancel;
end;

function TParallelRun.GetPhase: TNyxQueryPhase;
begin
  Result := FProgress.Phase;
end;

function TParallelRun.GetFailure: TNyxText;
begin
  Result := FProgress.Failure;
end;

function TParallelRun.GetFailureCount: Integer;
begin
  Result := FDiagnostics.Count;
end;

function TParallelRun.FailureAt(AIndex: Integer): TNyxPartitionFailure;
begin
  Result := FDiagnostics.Item(AIndex);
end;

function NewNyxPartitionDiagnostics: INyxPartitionDiagnostics;
begin
  Result := TPartitionDiagnostics.Create;
end;

function NewNyxParallelRun(const AProgress: INyxQueryProgress;
  const ADiagnostics: INyxPartitionDiagnostics): INyxParallelRun;
begin
  Result := TParallelRun.Create(AProgress, ADiagnostics);
end;

constructor TNyxGlobalPartitionFactory<TSource, TResult>.Create(
  AFunction: TNyxSelectorFunction<TSource, TResult>);
begin
  inherited Create;

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('A partition selector function is required');
  end;
  FFunction := AFunction;
end;

function TNyxGlobalPartitionFactory<TSource, TResult>.NewSelector(APartition,
  ASourceOffset: Integer): INyxSelector<TSource, TResult>;
begin
  Result := TNyxSelector<TSource, TResult>.From(FFunction);
end;

class function TNyxPartitionSelector<TSource, TResult>.From(
  AFunction: TNyxSelectorFunction<TSource, TResult>): INyxPartitionSelectorFactory<TSource, TResult>;
begin
  Result := TNyxGlobalPartitionFactory<TSource, TResult>.Create(AFunction);
end;

constructor TNyxPartitionReply<TResult>.Create(const ASink: INyxPartitionSink<TResult>;
  const AProgress: INyxQueryProgress; APartition, AOffset: Integer;
  const AValues: TNyxSequenceArray<TResult>; AFailed: Boolean; const AFailure: TNyxText);
var
  LIndex: Integer;
begin
  inherited Create;
  FSink := ASink;
  FProgress := AProgress;
  FPartition := APartition;
  FOffset := AOffset;
  FFailed := AFailed;
  FFailure := AFailure;
  SetLength(FValues, Length(AValues));
  for LIndex := 0 to High(FValues) do
  begin
    FValues[LIndex] := AValues[LIndex];
  end;
end;

procedure TNyxPartitionReply<TResult>.Execute(const AExecution: INyxExecution);
begin

  if FProgress.IsCancelled or AExecution.Cancelled then
  begin
    FProgress.Cancel;
    Exit;
  end;
  try
    FSink.Completed(FPartition, FOffset, FValues, FFailed, FFailure);
  except
    on LException: Exception do
    begin
      FProgress.Fail(NyxQueryDiagnostic(LException));
      FProgress.Cancel;
    end;
  end;
end;

constructor TNyxPartitionWork<TSource, TResult>.Create(const AValues: array of TSource;
  const ASelector: INyxSelector<TSource, TResult>; const AScheduler: INyxScheduler;
  const ASink: INyxPartitionSink<TResult>; const AProgress: INyxQueryProgress;
  APartition, AOffset: Integer);
var
  LIndex: Integer;
begin
  inherited Create;
  SetLength(FValues, Length(AValues));
  for LIndex := 0 to High(FValues) do
  begin
    FValues[LIndex] := AValues[LIndex];
  end;
  FSelector := ASelector;
  FScheduler := AScheduler;
  FSink := ASink;
  FProgress := AProgress;
  FPartition := APartition;
  FOffset := AOffset;
end;

procedure TNyxPartitionWork<TSource, TResult>.Execute(const AExecution: INyxExecution);
var
  LValues: TNyxSequenceArray<TResult>;
  LIndex: Integer;
  LFailed: Boolean;
  LFailure: TNyxText;
  LReply: INyxWork;
  LExecution: INyxExecution;
begin
  try
    LFailed := False;
    LFailure := '';
    try
      SetLength(LValues, Length(FValues));
      for LIndex := 0 to High(FValues) do
      begin

        if FProgress.IsCancelled or AExecution.Cancelled then
        begin
          FProgress.Cancel;
          Exit;
        end;
        LValues[LIndex] := FSelector.Project(FValues[LIndex]);
      end;
    except
      on LException: Exception do
      begin
        LFailed := True;
        LFailure := NyxQueryDiagnostic(LException);
        LValues := nil;
      end;
    end;

    if FProgress.IsCancelled or AExecution.Cancelled then
    begin
      FProgress.Cancel;
      Exit;
    end;
    { Release the partition callback/input before its UI completion can schedule
      a replacement. Workers retain no live collection or target control. }
    FValues := nil;
    FSelector := nil;
    LReply := TNyxPartitionReply<TResult>.Create(FSink, FProgress, FPartition,
      FOffset, LValues, LFailed, LFailure);
    LExecution := FScheduler.PostUI(LReply, AExecution);
    FProgress.Attach(LExecution, False);
  except
    on LException: Exception do
    begin
      FProgress.Fail(NyxQueryDiagnostic(LException));
      FProgress.Cancel;
    end;
  end;
end;

constructor TNyxPartitionCoordinator<TSource, TResult>.Create(const AValues: array of TSource;
  const AFactory: INyxPartitionSelectorFactory<TSource, TResult>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
  const AProgress: INyxQueryProgress; const ADiagnostics: INyxPartitionDiagnostics;
  const AOptions: TNyxParallelOptions);
var
  LIndex: Integer;
begin
  inherited Create;
  SetLength(FInput, Length(AValues));
  SetLength(FResults, Length(AValues));
  for LIndex := 0 to High(FInput) do
  begin
    FInput[LIndex] := AValues[LIndex];
  end;
  FFactory := AFactory;
  FScheduler := AScheduler;
  FPort := APort;
  FProgress := AProgress;
  FDiagnostics := ADiagnostics;
  FOptions := AOptions;
end;

procedure TNyxPartitionCoordinator<TSource, TResult>.Launch;
var
  LCount, LIndex: Integer;
  LValues: TNyxSequenceArray<TSource>;
  LSelector: INyxSelector<TSource, TResult>;
  LWork: INyxWork;
  LExecution: INyxExecution;
  LPolicy: TNyxExecutionPolicy;
begin
  FScheduler.RequireUI;
  LPolicy := neThreaded;

  if FOptions.ExecutionMode = npxDeferredUI then
  begin
    LPolicy := neUIQueue;
  end;
  while (FNext < Length(FInput)) and (FActive < FOptions.ConcurrentPartitions)
    and not FProgress.IsCancelled do
  begin
    LCount := Length(FInput) - FNext;

    if LCount > FOptions.RowsPerPartition then
    begin
      LCount := FOptions.RowsPerPartition;
    end;
    LSelector := FFactory.NewSelector(FPartition, FNext);

    if LSelector = nil then
    begin
      raise ENyxQuery.Create('Partition factory returned no selector');
    end;
    SetLength(LValues, LCount);
    for LIndex := 0 to LCount - 1 do
    begin
      LValues[LIndex] := FInput[FNext + LIndex];
    end;
    LWork := TNyxPartitionWork<TSource, TResult>.Create(LValues, LSelector,
      FScheduler, Self, FProgress, FPartition, FNext);
    LExecution := FScheduler.Submit(LWork, LPolicy);
    FProgress.Attach(LExecution, False);
    Inc(FActive);
    Inc(FPartition);
    Inc(FNext, LCount);
  end;
end;

procedure TNyxPartitionCoordinator<TSource, TResult>.Complete;
var
  LDelivery: INyxWork;
  LExecution: INyxExecution;
begin

  if FFinished or (FNext <> Length(FInput)) or (FActive <> 0) then
  begin
    Exit;
  end;
  FFinished := True;

  if FDiagnostics.Count <> 0 then
  begin
    FProgress.Fail(FDiagnostics.Item(0).Message);
    Exit;
  end;
  FInput := nil;
  FFactory := nil;
  FProgress.AwaitUI;
  LDelivery := TNyxQueryDelivery<TResult>.Create(FResults, FPort, FProgress);
  LExecution := FScheduler.PostUI(LDelivery);
  FProgress.Attach(LExecution, True);
end;

procedure TNyxPartitionCoordinator<TSource, TResult>.Execute(const AExecution: INyxExecution);
begin
  try

    if FProgress.IsCancelled or AExecution.Cancelled then
    begin
      FProgress.Cancel;
      Exit;
    end;
    FProgress.BeginCompute;
    Launch;
    Complete;
  except
    on LException: Exception do
    begin
      FProgress.Fail(NyxQueryDiagnostic(LException));
      FProgress.Cancel;
    end;
  end;
end;

procedure TNyxPartitionCoordinator<TSource, TResult>.Completed(APartition,
  ASourceOffset: Integer; const AValues: TNyxSequenceArray<TResult>;
  AFailed: Boolean; const AFailure: TNyxText);
var
  LIndex, LOffset: Integer;
begin
  FScheduler.RequireUI;
  Dec(FActive);

  if FProgress.IsCancelled then
  begin
    Exit;
  end;

  if AFailed then
  begin
    FDiagnostics.Add(APartition, ASourceOffset, AFailure);

    if FOptions.FailurePolicy = npfStop then
    begin
      FProgress.Fail(AFailure);
      FProgress.Cancel;
      Exit;
    end;
  end
  else
  begin
    LOffset := ASourceOffset;

    if FOptions.ResultOrder = npoCompletion then
    begin
      LOffset := FWritten;
    end;
    for LIndex := 0 to High(AValues) do
    begin
      FResults[LOffset + LIndex] := AValues[LIndex];
    end;
    Inc(FWritten, Length(AValues));
  end;
  Launch;
  Complete;
end;

class function TNyxParallelSelect<TSource, TResult>.Submit(const AValues: array of TSource;
  const AFactory: INyxPartitionSelectorFactory<TSource, TResult>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
  const AOptions: TNyxParallelOptions): INyxParallelRun;
var
  LProgress: INyxQueryProgress;
  LDiagnostics: INyxPartitionDiagnostics;
  LWork: INyxWork;
  LExecution: INyxExecution;
begin
  AOptions.Validate;

  if (AFactory = nil) or (AScheduler = nil) or (APort = nil) then
  begin
    raise ENyxQuery.Create('Parallel submission requires a selector factory, scheduler and result port');
  end;

  if Length(AValues) > AOptions.InputLimit then
  begin
    raise ENyxQuery.Create('Parallel input exceeds its configured limit');
  end;
  AScheduler.Admit(neUIQueue);

  if AOptions.ExecutionMode = npxWorkers then
  begin
    AScheduler.Admit(neThreaded);
  end;
  LProgress := NewNyxQueryProgress;
  LDiagnostics := NewNyxPartitionDiagnostics;
  LWork := TNyxPartitionCoordinator<TSource, TResult>.Create(AValues, AFactory,
    AScheduler, APort, LProgress, LDiagnostics, AOptions);
  LExecution := AScheduler.Submit(LWork, neUIQueue);
  LProgress.Attach(LExecution, False);
  Result := NewNyxParallelRun(LProgress, LDiagnostics);
end;

end.
