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
program nyx_linq_streaming_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.model, nyx.codec, nyx.generated.view,
  nyx.collections, nyx.linq, nyx.linq.grouping, nyx.linq.grouping.parallel,
  nyx.linq.numeric.parallel, nyx.linq.numeric, nyx.linq.aggregates,
  nyx.linq.parallel, nyx.linq.pipeline, nyx.linq.execution, nyx.scheduler
  {$ifdef PAS2JS}, Web, nyx.application.browser
  {$else}, Interfaces, Forms, Grids, SyncObjs, nyx.application.lcl{$endif};

type
  {$ifdef PAS2JS}TApplicationHost = TNyxBrowserApplication;
  {$else}TApplicationHost = TNyxLCLApplication;{$endif}
  IProbe = interface(IInterface)
    ['{D678F5C4-0879-4D39-BDB6-AC65C2DA56C5}']
  end;
  { Settings are immutable after admission. Counters use atomic native reads/
    writes; no probe retains an application, source cursor or UI receiver. }
  TProbe = class(TInterfacedObject, IProbe)
  public
    Total: Integer;
    MoveFailure: Integer;
    CurrentFailure: Integer;
    FailOdd: Boolean;
    MissingCursor: Boolean;
    MissingFactory: Boolean;
    Hold: Boolean;
    HoldFirstOnly: Boolean;
    Reads: LongInt;
    Moves: LongInt;
    EOFSeen: LongInt;
    Calls: LongInt;
    Early: LongInt;
    Processed: LongInt;
    PeakOutstanding: LongInt;
    Cursors: LongInt;
    Selectors: LongInt;
    Entered: LongInt;
    WrongThread: LongInt;
    Factories: LongInt;
    FirstRead: LongInt;
    {$ifndef PAS2JS}Gate: TEvent;{$endif}
    constructor Create;
    destructor Destroy; override;
    procedure Worker(const AValue: Integer);
    procedure ObserveRead;
  end;
  TCursor = class(TInterfacedObject, INyxEnumerator<Integer>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
    FScheduler: INyxScheduler;
    FIndex: Integer;
  public
    constructor Create(AProbe: TProbe; const AScheduler: INyxScheduler);
    destructor Destroy; override;
    function MoveNext: Boolean;
    function GetCurrent: Integer;
  end;
  TSource = class(TNyxSequence<Integer>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
    FScheduler: INyxScheduler;
  public
    constructor Create(AProbe: TProbe; const AScheduler: INyxScheduler);
    function GetEnumerator: INyxEnumerator<Integer>; override;
  end;
  TSelector = class(TInterfacedObject, INyxSelector<Integer, TNyxText>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    destructor Destroy; override;
    function Project(const AValue: Integer): TNyxText;
  end;
  TSelectors = class(TInterfacedObject, INyxPartitionSelectorFactory<Integer, TNyxText>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<Integer, TNyxText>;
  end;
  TSum = class(TInterfacedObject, INyxNumericKernel<Integer, Integer, Integer>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    function Empty: Integer;
    procedure Add(var APartial: Integer; const AValue: Integer);
    procedure Merge(var APartial: Integer; const AOther: Integer);
    function Finish(const APartial: Integer): Integer;
  end;
  TAction = class(TInterfacedObject, INyxParallelAction<Integer>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    procedure Invoke(const AValue: Integer; ASourceIndex: Integer; const AExecution: INyxExecution);
  end;
  TActions = class(TInterfacedObject, INyxPartitionActionFactory<Integer>)
  private
    FProbe: TProbe;
    FOwner: IProbe;
  public
    constructor Create(AProbe: TProbe);
    function NewAction(APartition, ASourceOffset: Integer): INyxParallelAction<Integer>;
  end;
  TPair = INyxKeyValue<TNyxText, Integer>;
  TPairs = INyxSequence<TPair>;
  TJourney = class
  private
    FDocument: TNyxDocument;
    FAuthored: TNyxText;
    FApp: TApplicationHost;
    FScheduler: INyxScheduler;
    FStore: INyxCollection;
    FProbe: TProbe;
    FProbeOwner: IProbe;
    FRun: INyxParallelRun;
    FTextsLease: INyxQueryReceiver<TNyxText>;
    FNumberLease: INyxQueryReceiver<Integer>;
    FPairsLease: INyxQueryReceiver<TPairs>;
    FTexts: TNyxSequenceArray<TNyxText>;
    FNumber: Integer;
    FPairs: TPairs;
    FStage: Integer;
    FChecks: Integer;
    FPolls: Integer;
    FReceived: Integer;
    FHoldPolls: Integer;
    FSteered: Boolean;
    FStarted: Double;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Published;
    procedure ReceiveTexts(const AValues: TNyxSequenceArray<TNyxText>);
    procedure ReceiveNumber(const AValues: TNyxSequenceArray<Integer>);
    procedure ReceivePairs(const AValues: TNyxSequenceArray<TPairs>);
    procedure StartStage;
    procedure CompleteStage;
    procedure Finish(const AFailure: TNyxText);
  public
    Done: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Poll;
  end;

var
  GJourney: TJourney;

function NowMilliseconds: Double;
begin
  {$ifdef PAS2JS}Result := window.performance.now;{$else}Result := GetTickCount64;{$endif}
end;

procedure Increment(var AValue: LongInt);
begin
  {$ifdef PAS2JS}Inc(AValue);{$else}InterlockedIncrement(AValue);{$endif}
end;

function ReadCounter(var AValue: LongInt): LongInt;
begin
  {$ifdef PAS2JS}Result := AValue;{$else}Result := InterlockedCompareExchange(AValue, 0, 0);{$endif}
end;

constructor TProbe.Create;
begin
  inherited Create;
  Total := 12;
  MoveFailure := -1;
  CurrentFailure := -1;
  FirstRead := -1;
  {$ifndef PAS2JS}Gate := TEvent.Create(nil, True, False, '');{$endif}
end;

destructor TProbe.Destroy;
begin
  {$ifndef PAS2JS}Gate.Free;{$endif}
  inherited Destroy;
end;

procedure TProbe.ObserveRead;
var
  LOutstanding: Integer;
begin
  Increment(Reads);
  LOutstanding := ReadCounter(Reads) - ReadCounter(Processed);

  if LOutstanding > PeakOutstanding then
  begin
    PeakOutstanding := LOutstanding;
  end;
end;

procedure TProbe.Worker(const AValue: Integer);
begin
  Increment(Calls);

  if ReadCounter(EOFSeen) = 0 then
  begin
    Increment(Early);
  end;
  {$ifdef PAS2JS}

  if FirstRead = -1 then
  begin
    FirstRead := Reads;
  end;
  {$else}
  InterlockedCompareExchange(FirstRead, ReadCounter(Reads), -1);

  if GetCurrentThreadID = MainThreadID then
  begin
    Increment(WrongThread);
  end;

  if Hold and ((AValue = 0) or ((AValue = 2) and not HoldFirstOnly)) then
  begin
    Increment(Entered);
    Gate.WaitFor(INFINITE);
  end;
  {$endif}

  if FailOdd and Odd(AValue) then
  begin
    raise ENyxQuery.Create('Streaming worker refused an odd row');
  end;
  Increment(Processed);
end;

constructor TCursor.Create(AProbe: TProbe; const AScheduler: INyxScheduler);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
  FScheduler := AScheduler;
  FIndex := -1;
  Increment(FProbe.Cursors);
end;

destructor TCursor.Destroy;
begin
  {$ifndef PAS2JS}

  if GetCurrentThreadID <> MainThreadID then
  begin
    Increment(FProbe.WrongThread);
  end;
  InterlockedDecrement(FProbe.Cursors);
  {$else}Dec(FProbe.Cursors);{$endif}
  inherited Destroy;
end;

function TCursor.MoveNext: Boolean;
begin
  FScheduler.RequireUI;
  Increment(FProbe.Moves);
  Inc(FIndex);

  if FIndex = FProbe.MoveFailure then
  begin
    raise ENyxQuery.Create('Source MoveNext refused after admitted partitions');
  end;
  Result := FIndex < FProbe.Total;

  if not Result then
  begin
    Increment(FProbe.EOFSeen);
  end;
end;

function TCursor.GetCurrent: Integer;
begin
  FScheduler.RequireUI;

  if FIndex = FProbe.CurrentFailure then
  begin
    raise ENyxQuery.Create('Source Current refused after admitted partitions');
  end;
  FProbe.ObserveRead;
  Result := FIndex;
end;

constructor TSource.Create(AProbe: TProbe; const AScheduler: INyxScheduler);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
  FScheduler := AScheduler;
end;

function TSource.GetEnumerator: INyxEnumerator<Integer>;
begin
  FScheduler.RequireUI;

  if FProbe.MissingCursor then
  begin
    Exit(nil);
  end;
  Result := TCursor.Create(FProbe, FScheduler);
end;

constructor TSelector.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
  Increment(FProbe.Selectors);
end;

destructor TSelector.Destroy;
begin
  {$ifdef PAS2JS}Dec(FProbe.Selectors);{$else}InterlockedDecrement(FProbe.Selectors);{$endif}
  inherited Destroy;
end;

function TSelector.Project(const AValue: Integer): TNyxText;
begin
  FProbe.Worker(AValue);
  Result := 'Row ' + IntToStr(AValue);
end;

constructor TSelectors.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TSelectors.NewSelector(APartition, ASourceOffset: Integer): INyxSelector<Integer, TNyxText>;
begin
  Increment(FProbe.Factories);

  if FProbe.MissingFactory and (APartition = 2) then
  begin
    Exit(nil);
  end;
  Result := TSelector.Create(FProbe);
end;

constructor TSum.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TSum.Empty: Integer;
begin
  Result := 0;
end;

procedure TSum.Add(var APartial: Integer; const AValue: Integer);
begin
  FProbe.Worker(AValue);
  APartial := NyxAddQueryInteger(APartial, AValue);
end;

procedure TSum.Merge(var APartial: Integer; const AOther: Integer);
begin
  APartial := NyxAddQueryInteger(APartial, AOther);
end;

function TSum.Finish(const APartial: Integer): Integer;
begin
  Result := APartial;
end;

constructor TAction.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

procedure TAction.Invoke(const AValue: Integer; ASourceIndex: Integer; const AExecution: INyxExecution);
begin
  FProbe.Worker(AValue);
end;

constructor TActions.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TActions.NewAction(APartition, ASourceOffset: Integer): INyxParallelAction<Integer>;
begin
  Result := TAction.Create(FProbe);
end;

function IsEven(const AValue: Integer): Boolean;
begin
  Result := not Odd(AValue);
end;

function Doubled(const AValue: Integer): Integer;
begin
  Result := AValue * 2;
end;

function Bucket(const AValue: Integer): TNyxText;
begin
  Result := IntToStr(AValue mod 16);
end;

function TextEquality: INyxEquality<TNyxText>;
begin
  Result := NewNyxTextEquality;
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxQuery.Create('Streaming stage ' + IntToStr(FStage) + ': ' + AReason);
  end;
  Inc(FChecks);
end;

constructor TJourney.Create;
begin
  inherited Create;
  FDocument := BuildNyxDocument;
  FAuthored := TNyxCodec.Encode(FDocument);
  FApp := TApplicationHost.Create;
  {$ifdef PAS2JS}FApp.Run(FDocument, TJSHTMLElement(document.body));{$else}FApp.Mount(FDocument);{$endif}
  FScheduler := FApp.Context.Scheduler;
  FStore := FApp.Collections.Collection(NyxCollection('measured'));
  Check(FDocument.Count = 2, 'exact semantic companion supplies navigation');
  StartStage;
end;

destructor TJourney.Destroy;
begin
  {$ifndef PAS2JS}

  if FProbe <> nil then
  begin
    FProbe.Gate.SetEvent;
  end;
  {$endif}

  if FRun <> nil then
  begin
    FRun.Cancel;
  end;
  FTextsLease := nil;
  FNumberLease := nil;
  FPairsLease := nil;
  FPairs := nil;
  FApp.Free;
  FStore := nil;
  FDocument.Free;
  FScheduler.Shutdown;
  FProbeOwner := nil;
  inherited Destroy;
end;

procedure TJourney.Published;
var
  LCaption: TNyxText;
begin
  FScheduler.RequireUI;
  Inc(FReceived);
  LCaption := 'Stream completed / ' + IntToStr(FStage);
  FStore.Update(FStore.Snapshot.ItemAt(0).WithValue(NyxTextField('caption'), LCaption));
  {$ifdef PAS2JS}
  Check(Pos(LCaption, FApp.View.ElementFor('measured-table').textContent) > 0,
    'actual browser table observes only completed stream');
  {$else}
  Check(TNyxText(TStringGrid(FApp.View.ControlFor('measured-table')).Cells[0, 1]) = LCaption,
    'actual native table observes only completed stream');
  {$endif}
end;

procedure TJourney.ReceiveTexts(const AValues: TNyxSequenceArray<TNyxText>);
begin
  FTexts := AValues;
  Published;
end;

procedure TJourney.ReceiveNumber(const AValues: TNyxSequenceArray<Integer>);
begin
  Check(Length(AValues) = 1, 'one completed numeric result reaches UI');
  FNumber := AValues[0];
  Published;
end;

procedure TJourney.ReceivePairs(const AValues: TNyxSequenceArray<TPairs>);
begin
  Check((Length(AValues) = 1) and (AValues[0] <> nil), 'one completed keyed sequence reaches UI');
  FPairs := AValues[0];
  Published;
end;

procedure TJourney.StartStage;
var
  LOptions: TNyxParallelOptions;
  LSource: INyxSequence<Integer>;
  LGeneration: INyxCancellationScope;
  LPlan: INyxNumericPlan<Integer, Integer, Integer>;
  LProgram: INyxParallelProgram<Integer, Integer>;
begin
  FReceived := 0;
  FPolls := 0;
  FHoldPolls := 0;
  FSteered := False;
  FTexts := nil;
  FPairs := nil;
  FProbe := TProbe.Create;
  FProbeOwner := FProbe;
  LOptions := TNyxParallelOptions.Defaults.Input(npiStreaming)
    .Concurrency(2).PartitionSize(2).CaptureRowsPerTurn(1);
  {$ifdef PAS2JS}LOptions := LOptions.Execution(npxDeferredUI);{$endif}
  case FStage of
    0:
    begin
      FProbe.Hold := True;
      FProbe.Total := 13;
    end;
    7, 18: FProbe.Hold := True;
    1:
    begin
      FProbe.Total := 13;
      LOptions := LOptions.Input(npiSnapshot);
    end;
    2, 14: FProbe.MoveFailure := 5;
    3: FProbe.CurrentFailure := 5;
    4: LOptions := LOptions.MaximumInput(5);
    5:
    begin
      FProbe.FailOdd := True;
      LOptions := LOptions.Failures(npfCollect);
    end;
    6: FProbe.FailOdd := True;
    8: FProbe.MissingCursor := True;
    9: FProbe.Total := 0;
    10, 11, 12, 16:
    begin
      FProbe.Total := 8192;
      LOptions := LOptions.PartitionSize(512).CaptureRowsPerTurn(128);

      if FStage = 16 then
      begin
        LOptions := LOptions.Input(npiSnapshot);
      end;
    end;
    15: FProbe.MissingFactory := True;
    17:
    begin
      FProbe.Hold := True;
      FProbe.HoldFirstOnly := True;
      LOptions := LOptions.Order(npoCompletion);
    end;
  end;
  LGeneration := FApp.Context.Generation;
  FTextsLease := TNyxQueryReceiver<TNyxText>.Bind(FScheduler, ReceiveTexts, LGeneration);
  FNumberLease := TNyxQueryReceiver<Integer>.Bind(FScheduler, ReceiveNumber, LGeneration);
  FPairsLease := TNyxQueryReceiver<TPairs>.Bind(FScheduler, ReceivePairs, LGeneration);
  LSource := TSource.Create(FProbe, FScheduler);
  FStarted := NowMilliseconds;

  if FStage in [10, 12, 16] then
  begin
    LPlan := TNyxNumericPlan<Integer, Integer, Integer>.From(TSum.Create(FProbe)).WithOptions(LOptions);

    if FStage = 12 then
    begin
      LProgram := TNyxParallelSource<Integer>.From(LSource)
        .Where(TNyxPartitionPredicate<Integer>.From(IsEven));
      LProgram := TNyxParallelMap<Integer, Integer, Integer>.From(LProgram,
        TNyxPartitionSelector<Integer, Integer>.From(Doubled));
      FRun := TNyxNumericProgram<Integer, Integer, Integer, Integer>.Submit(
        LProgram, LPlan, FScheduler, FNumberLease.Port, LGeneration);
    end
    else
    begin
      FRun := LPlan.Submit(LSource, FScheduler, FNumberLease.Port, LGeneration);
    end;
  end
  else if FStage = 11 then
  begin
    FRun := TNyxParallelCountBy<Integer, TNyxText>.From(
      TNyxPartitionSelector<Integer, TNyxText>.From(Bucket),
      TNyxParallelEquality<TNyxText>.From(TextEquality)).WithOptions(LOptions)
      .Submit(LSource, FScheduler, FPairsLease.Port, LGeneration);
  end
  else if FStage in [13, 14] then
  begin
    FRun := TNyxParallelForAll<Integer>.Submit(LSource, TActions.Create(FProbe),
      FScheduler, LOptions, LGeneration);
  end
  else
  begin
    FRun := TNyxParallelSelect<Integer, TNyxText>.Submit(LSource, TSelectors.Create(FProbe),
      FScheduler, FTextsLease.Port, LOptions, LGeneration);
  end;
  {$ifdef PAS2JS}

  if FStage = 7 then
  begin
    FApp.ShowPage('next-view');
    FSteered := True;
  end;
  {$endif}
end;

procedure TJourney.CompleteStage;
var
  LIndex: Integer;
  LPairs: TNyxSequenceArray<TPair>;
begin
  Check((ReadCounter(FProbe.Cursors) = 0) and (ReadCounter(FProbe.Selectors) = 0),
    'source cursor and independent selectors retire');
  Check(ReadCounter(FProbe.WrongThread) = 0, 'source lifetime stays UI-only and native row work stays off UI');

  if FStage in [7, 18] then
  begin
    Check((FRun.Phase = nqpCancelled) and (FReceived = 0), 'navigation/shutdown cancels stream without stale publication');

    if FStage = 7 then
    begin
      FApp.ShowPage('measured-view');
    end;
  end
  else if FStage in [2, 3, 4, 5, 6, 8, 14, 15] then
  begin
    Check((FRun.Phase = nqpFailed) and (FReceived = 0), 'late source/worker/factory failure publishes no partial result');

    if FStage in [2, 3, 4, 15] then
    begin
      Check(ReadCounter(FProbe.Calls) > 0, 'source/admission failure occurs after streaming worker entry');
    end;

    if FStage = 5 then
    begin
      Check((FRun.FailureCount = 6) and (ReadCounter(FProbe.Reads) = 12), 'collect retains all six streaming partition failures');
    end;

    if FStage = 14 then
    begin
      Check(ReadCounter(FProbe.Calls) > 0, 'explicit streaming actions may enter before later source failure');
    end;
  end
  else
  begin
    Check(FRun.Phase = nqpSucceeded, 'successful stream reaches completed receipt');

    if FStage = 13 then
    begin
      Check((FReceived = 0) and (ReadCounter(FProbe.Calls) = 12), 'streaming ForAll invokes every action without a result receiver');
    end
    else
    begin
      Check(FReceived = 1, 'exactly one complete result publishes');
      case FStage of
        0, 1:
        begin
          Check(Length(FTexts) = FProbe.Total, 'all projection rows and final partial partition survive');
          for LIndex := 0 to High(FTexts) do
          begin
            Check(FTexts[LIndex] = 'Row ' + IntToStr(LIndex), 'source result order survives overlapping capture');
          end;

          if FStage = 0 then
          begin
            Check(ReadCounter(FProbe.Early) > 0, 'row callbacks enter before source EOF');
            Check(FProbe.PeakOutstanding <= 6, 'bounded source pressure covers two admitted partitions plus one buffer');
          end
          else
          begin
            Check(ReadCounter(FProbe.Early) = 0, 'snapshot default finishes enumeration before callbacks');
          end;
        end;
        9: Check((Length(FTexts) = 0) and (ReadCounter(FProbe.Factories) = 0), 'empty stream enters no partition factory');
        10, 16:
        begin
          Check((FNumber = 33550336) and (ReadCounter(FProbe.Calls) = 8192), 'matched 8192-row numeric reductions retain exact total');

          if FStage = 10 then
          begin
            Check(ReadCounter(FProbe.Early) > 0, 'numeric kernel overlaps bounded source capture');
            Check(FProbe.PeakOutstanding <= 1536, 'large reduction retains bounded owned input pressure');
          end
          else
          begin
            Check((ReadCounter(FProbe.Early) = 0) and (ReadCounter(FProbe.FirstRead) = 8192), 'matched snapshot waits for the complete input');
          end;
        end;
        11:
        begin
          LPairs := FPairs.ToArray;
          Check(Length(LPairs) = 16, 'streaming keyed count retains sixteen final keys');
          for LIndex := 0 to High(LPairs) do
          begin
            Check(LPairs[LIndex].Value = 512, 'streaming keyed merge preserves exact per-key counts');
          end;
        end;
        12: Check(FNumber = 33546240, 'streaming fused filter/map feeds exact compact numeric reduction');
        17:
        begin
          Check(Length(FTexts) = 12, 'completion-order stream retains all rows');
          {$ifndef PAS2JS}
          for LIndex := 0 to 9 do
          begin
            Check(FTexts[LIndex] = 'Row ' + IntToStr(LIndex + 2), 'observed later partitions precede blocked first partition');
          end;
          Check((FTexts[10] = 'Row 0') and (FTexts[11] = 'Row 1'), 'blocked first partition appends last in completion order');
          {$endif}
        end;
      end;
    end;
  end;
  Check(TNyxCodec.Encode(FDocument) = FAuthored, 'streaming runtime leaves authored defaults exact');
  {$ifdef PAS2JS}
  document.body.setAttribute('data-nyx-stream-stage-' + IntToStr(FStage),
    IntToStr(Trunc(NowMilliseconds - FStarted)) + '/' + IntToStr(ReadCounter(FProbe.FirstRead)));
  {$else}
  WriteLn('stage ', FStage, ' / ms ', Trunc(NowMilliseconds - FStarted),
    ' / first worker read ', ReadCounter(FProbe.FirstRead));
  {$endif}
end;

{$ifdef PAS2JS}
procedure ReleaseJourney;
begin
  FreeAndNil(GJourney);
  document.body.setAttribute('data-nyx-stream-disposed', 'true');
end;
{$endif}

procedure TJourney.Finish(const AFailure: TNyxText);
begin
  Done := True;
  {$ifdef PAS2JS}

  if AFailure = '' then
  begin
    document.body.setAttribute('data-nyx-stream', 'passed');
    document.body.setAttribute('data-nyx-stream-checks', IntToStr(FChecks));
  end
  else
  begin
    document.body.textContent := 'FAIL ' + AFailure;
    document.body.setAttribute('data-nyx-stream', 'failed');
  end;
  window.setTimeout(@ReleaseJourney, 0);
  {$else}

  if AFailure = '' then
  begin
    WriteLn('PASS ', FChecks, ' bounded streaming / actual UI checks');
  end
  else
  begin
    WriteLn('FAIL ', AFailure);
    ExitCode := 1;
  end;
  {$endif}
end;

procedure TJourney.Poll;
var
  LMonitor: INyxSchedulerMonitor;
begin
  try
    Inc(FPolls);

    if FPolls > 6000 then
    begin
      raise ENyxQuery.Create('Streaming observation expired');
    end;
    Supports(FScheduler, INyxSchedulerMonitor, LMonitor);
    {$ifndef PAS2JS}

    if (FStage in [0, 7, 18]) and not FSteered and (ReadCounter(FProbe.Entered) = 2) then
    begin
      Check((ReadCounter(FProbe.Reads) = 4) and (ReadCounter(FProbe.Moves) = 4), 'full worker slots pause source enumeration exactly');
      Check(FReceived = 0, 'paused incomplete stream publishes nothing');
      Inc(FHoldPolls);

      if FHoldPolls = 3 then
      begin

        if FStage = 7 then
        begin
          FApp.ShowPage('next-view');
        end;

        if FStage = 18 then
        begin
          FScheduler.Shutdown;
        end;
        FProbe.Gate.SetEvent;
        FSteered := True;
      end;
    end;

    if (FStage = 17) and not FSteered and (ReadCounter(FProbe.Entered) = 1) and
      (ReadCounter(FProbe.Processed) = 10) and (LMonitor.WorkerLoad.Running = 1) then
    begin
      { Every later partition has posted its reply before first may return. }
      FProbe.Gate.SetEvent;
      FSteered := True;
    end;
    {$else}

    if (FStage = 18) and not FSteered and (ReadCounter(FProbe.Reads) >= 2) then
    begin
      FScheduler.Shutdown;
      FSteered := True;
    end;
    {$endif}

    if (FRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI]) or
      (LMonitor.WorkerLoad.Running <> 0) or
      (ReadCounter(FProbe.Cursors) <> 0) or (ReadCounter(FProbe.Selectors) <> 0) then
    begin
      { A terminal receipt cancels tickets; already queued UI couriers still
        own their callbacks until the host delivers them. Drain that ownership
        before asserting lifetime or replacing the probe for the next stage. }
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;

    if FStage <= 18 then
    begin
      CompleteStage;
      Inc(FStage);

      if FStage <= 18 then
      begin
        StartStage;
        {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
        Exit;
      end;
      FScheduler.Shutdown;
    end;

    if LMonitor.WorkerLoad.ActiveWorkers <> 0 then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;
    Finish('');
  except
    on LException: Exception do
    begin
      Finish(NyxQueryDiagnostic(LException));
    end;
  end;
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    GJourney := TJourney.Create;
    {$ifdef PAS2JS}GJourney.Poll;{$else}
    while not GJourney.Done do
    begin
      CheckSynchronize(1);
      Application.ProcessMessages;
      GJourney.Poll;
      Sleep(1);
    end;
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-stream', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}GJourney.Free;{$endif}
end.
