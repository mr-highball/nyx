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

program nyx_linq_numeric_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, Math, nyx.text, nyx.model, nyx.codec, nyx.data, nyx.generated.view,
  nyx.collections, nyx.linq, nyx.linq.numeric, nyx.linq.numeric.parallel,
  nyx.linq.parallel, nyx.linq.pipeline, nyx.linq.execution, nyx.scheduler,
  nyx.numbers.integer64, nyx.numbers.decimal, nyx.numbers.single
  {$ifdef PAS2JS}, Web, nyx.application.browser
  {$else}, Interfaces, Forms, Grids, nyx.application.lcl{$endif};

type
  {$ifdef PAS2JS}
  TApplicationHost = TNyxBrowserApplication;
  {$else}
  TApplicationHost = TNyxLCLApplication;
  {$endif}
  TOptionalNumber = INyxOptional<Double>;
  TOptionalInteger = INyxOptional<Integer>;
  TOptionalSingle = INyxOptional<TNyxSingleNumber>;
  TOptionalDecimal = INyxOptional<TNyxDecimalNumber>;
  IMeasure = interface(IInterface)
    ['{20D9B469-D44C-40AF-8AE8-FBF36BB072C1}']
  end;
  { Synchronized, test-only workload counters; immutable row/partial values and
    these owned counters are the only inputs shared with native workers. }
  TMeasure = class(TInterfacedObject, IMeasure)
  public
    Rows: LongInt;
    Merges: LongInt;
    Seeds: LongInt;
    UIRows: LongInt;
    FirstClaim: LongInt;
    FirstWorker: Double;
    MergeStarted: Double;
    WorkFinished: Double;
  end;
  TMeasured64 = class(TInterfacedObject,
    INyxNumericKernel<TNyxInteger64, TNyxInteger64, TNyxInteger64>)
  private
    FMeasure: TMeasure;
    FOwner: IMeasure;
  public
    constructor Create(AMeasure: TMeasure);
    function Empty: TNyxInteger64;
    procedure Add(var AState: TNyxInteger64; const AValue: TNyxInteger64);
    procedure Merge(var AState: TNyxInteger64; const AOther: TNyxInteger64);
    function Finish(const AState: TNyxInteger64): TNyxInteger64;
  end;
  TJourney = class
  private
    FDocument: TNyxDocument;
    FAuthored: TNyxText;
    FApp: TApplicationHost;
    FScheduler: INyxScheduler;
    FStore: INyxCollection;
    FRun: INyxParallelRun;
    FIntegerLease: INyxQueryReceiver<Integer>;
    FNumberLease: INyxQueryReceiver<Double>;
    FMeanLease: INyxQueryReceiver<TOptionalNumber>;
    F64Lease: INyxQueryReceiver<TNyxInteger64>;
    FSingleLease: INyxQueryReceiver<TNyxSingleNumber>;
    FSingleMeanLease: INyxQueryReceiver<TOptionalSingle>;
    FDecimalLease: INyxQueryReceiver<TNyxDecimalNumber>;
    FDecimalMeanLease: INyxQueryReceiver<TOptionalDecimal>;
    FValue: Double;
    FHasValue: Boolean;
    FText: TNyxText;
    FExpected: TNyxText;
    FReceived: Integer;
    FStage: Integer;
    FChecks: Integer;
    FPolls: Integer;
    FFailure: Boolean;
    FMeasure: TMeasure;
    FMeasureOwner: IMeasure;
    FRows: Integer;
    FStarted: Double;
    FTimings: TNyxText;
    FGeneration: INyxCancellationScope;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Published;
    procedure SnapshotContracts;
    procedure ReceiveInteger(const AValues: TNyxSequenceArray<Integer>);
    procedure ReceiveNumber(const AValues: TNyxSequenceArray<Double>);
    procedure ReceiveMean(const AValues: TNyxSequenceArray<TOptionalNumber>);
    procedure Receive64(const AValues: TNyxSequenceArray<TNyxInteger64>);
    procedure ReceiveSingle(const AValues: TNyxSequenceArray<TNyxSingleNumber>);
    procedure ReceiveSingleMean(const AValues: TNyxSequenceArray<TOptionalSingle>);
    procedure ReceiveDecimal(const AValues: TNyxSequenceArray<TNyxDecimalNumber>);
    procedure ReceiveDecimalMean(const AValues: TNyxSequenceArray<TOptionalDecimal>);
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

const
  { Store precision-sensitive expectations in the same declared binary64
    domain as the portable contract. Native Extended literals compare at a
    different precision even when the computed Double is exactly correct. }
  CMaximumNumber: Double = 1.7976931348623157e308;
  CMaximumSingle: Double = 3.4028234663852886e38;

function Decimal(const AText: TNyxText): TNyxDecimalNumber;
begin
  Result := TNyxDecimalNumber.FromDecimal(NyxDecimal(AText));
end;

function To64(const AValue: Integer): TNyxInteger64;
begin
  Result := TNyxInteger64.FromInteger(AValue);
end;

function DoubleValue(const AValue: Integer): Double;
begin
  Result := AValue * 2;
end;

function NowMilliseconds: Double;
begin
  {$ifdef PAS2JS}Result := window.performance.now;{$else}Result := GetTickCount64;{$endif}
end;

procedure Increment(var AValue: LongInt);
begin
  {$ifdef PAS2JS}Inc(AValue);{$else}InterlockedIncrement(AValue);{$endif}
end;

constructor TMeasured64.Create(AMeasure: TMeasure);
begin
  inherited Create;
  FMeasure := AMeasure;
  FOwner := AMeasure;
end;

function TMeasured64.Empty: TNyxInteger64;
begin
  Increment(FMeasure.Seeds);
  Result := TNyxInteger64.FromInteger(0);
end;

procedure TMeasured64.Add(var AState: TNyxInteger64; const AValue: TNyxInteger64);
begin
  {$ifdef PAS2JS}

  if FMeasure.FirstClaim = 0 then
  begin
    FMeasure.FirstClaim := 1;
    FMeasure.FirstWorker := NowMilliseconds;
  end;
  {$else}

  if InterlockedCompareExchange(FMeasure.FirstClaim, 1, 0) = 0 then
  begin
    FMeasure.FirstWorker := NowMilliseconds;
  end;
  {$endif}
  Increment(FMeasure.Rows);
  {$ifndef PAS2JS}

  if GetCurrentThreadID = MainThreadID then
  begin
    Increment(FMeasure.UIRows);
  end;
  {$endif}
  AState := AState.Add(AValue);
end;

procedure TMeasured64.Merge(var AState: TNyxInteger64; const AOther: TNyxInteger64);
begin

  if FMeasure.Merges = 0 then
  begin
    FMeasure.MergeStarted := NowMilliseconds;
  end;
  Increment(FMeasure.Merges);
  AState := AState.Add(AOther);
end;

function TMeasured64.Finish(const AState: TNyxInteger64): TNyxInteger64;
begin
  FMeasure.WorkFinished := NowMilliseconds;
  Result := AState;
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxQuery.Create('Stage ' + IntToStr(FStage) + ': ' + AReason);
  end;
  Inc(FChecks);
end;

constructor TJourney.Create;
begin
  inherited Create;
  SnapshotContracts;
  FDocument := BuildNyxDocument;
  FAuthored := TNyxCodec.Encode(FDocument);
  FApp := TApplicationHost.Create;
  {$ifdef PAS2JS}FApp.Run(FDocument, TJSHTMLElement(document.body));{$else}FApp.Mount(FDocument);{$endif}
  FScheduler := FApp.Context.Scheduler;
  FStore := FApp.Collections.Collection(NyxCollection('measured'));
  Check(FDocument.Count = 2, 'exact semantic companion contains both pages');
  StartStage;
end;

procedure TJourney.SnapshotContracts;
var
  LOwner: TNyxDecimalAccumulator;
  LOther: TNyxDecimalAccumulator;
  LSnapshot: TNyxDecimalAccumulation;
  LCandidate: TNyxDecimalAccumulation;
  LMaximum: TNyxDecimalNumber;
  LRefused: Boolean;
  LIndex: Integer;
begin
  LSnapshot := Default(TNyxDecimalAccumulation);
  Check((LSnapshot.Count = 0) and (LSnapshot.Total.ToText = '0'), 'default wide decimal state is the empty identity');
  LMaximum := Decimal('79228162514264337593543950335');
  LOwner := TNyxDecimalAccumulator.Create;
  LOther := TNyxDecimalAccumulator.Create;
  try
    LOwner.Push(LMaximum);
    LOwner.Push(LMaximum);
    LSnapshot := LOwner.Snapshot;
    Check((LSnapshot.Count = 2) and (LSnapshot.Average.ToText = LMaximum.ToText),
      'wide snapshot preserves an overflowing total with a valid mean');
    LRefused := False;
    try
      LSnapshot.Total;
    except
      on LException: ENyxDecimalOverflow do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSnapshot.Count = 2), 'refused final total preserves exact snapshot');
    LOwner.Push(Decimal('-79228162514264337593543950335'));
    Check((LOwner.Count = 3) and (LOwner.Total.ToText = LMaximum.ToText) and (LSnapshot.Count = 2),
      'later builder edits retain the independently owned snapshot');
    LOther.Merge(LSnapshot);
    LOther.Push(Decimal('-79228162514264337593543950335'));
    Check((LOther.Count = 3) and (LOther.Total.ToText = LMaximum.ToText), 'builder merge adopts wide state without early rounding');
    FreeAndNil(LOwner);
    Check(LSnapshot.Average.ToText = LMaximum.ToText, 'snapshot survives destruction of its originating owner');
  finally
    LOwner.Free;
    LOther.Free;
  end;
  LSnapshot := TNyxDecimalAccumulation.Empty.Added(TNyxDecimalNumber.FromInteger(1));
  for LIndex := 1 to 30 do
  begin
    LSnapshot := LSnapshot.Merged(LSnapshot);
  end;
  Check((LSnapshot.Count = 1073741824) and (LSnapshot.Total.ToText = '1073741824'),
    'wide merge count remains exact through repeated independent composition');
  LRefused := False;
  try
    LCandidate := LSnapshot.Merged(LSnapshot);
  except
    on LException: ENyxDecimalOverflow do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LSnapshot.Count = 1073741824), 'count overflow refuses atomically without source mutation');
end;

destructor TJourney.Destroy;
begin

  if FRun <> nil then
  begin
    FRun.Cancel;
  end;
  FIntegerLease := nil;
  FNumberLease := nil;
  FMeanLease := nil;
  F64Lease := nil;
  FSingleLease := nil;
  FSingleMeanLease := nil;
  FDecimalLease := nil;
  FDecimalMeanLease := nil;
  FMeasureOwner := nil;
  FApp.Free;
  FStore := nil;
  FDocument.Free;
  FScheduler.Shutdown;
  inherited Destroy;
end;

procedure TJourney.Published;
var
  LCaption: TNyxText;
begin
  FScheduler.RequireUI;
  Inc(FReceived);
  LCaption := 'Numeric result / ' + IntToStr(FStage);
  FStore.Update(FStore.Snapshot.ItemAt(0).WithValue(NyxTextField('caption'), LCaption));
  {$ifdef PAS2JS}
  Check(Pos(LCaption, FApp.View.ElementFor('measured-table').textContent) > 0,
    'actual browser table observes the completed numeric result');
  {$else}
  Check(TNyxText(TStringGrid(FApp.View.ControlFor('measured-table')).Cells[0, 1]) = LCaption,
    'actual native table observes the completed numeric result');
  {$endif}
end;

procedure TJourney.ReceiveInteger(const AValues: TNyxSequenceArray<Integer>);
begin
  Check(Length(AValues) = 1, 'one completed Integer reaches UI');
  FValue := AValues[0];
  Published;
end;

procedure TJourney.ReceiveNumber(const AValues: TNyxSequenceArray<Double>);
begin
  Check(Length(AValues) = 1, 'one completed Double reaches UI');
  FValue := AValues[0];
  Published;
end;

procedure TJourney.ReceiveMean(const AValues: TNyxSequenceArray<TOptionalNumber>);
begin
  Check((Length(AValues) = 1) and (AValues[0] <> nil), 'one completed optional mean reaches UI');
  FHasValue := AValues[0].HasValue;

  if FHasValue then
  begin
    FValue := AValues[0].Value;
  end;
  Published;
end;

procedure TJourney.Receive64(const AValues: TNyxSequenceArray<TNyxInteger64>);
begin
  Check(Length(AValues) = 1, 'one exact signed64 result reaches UI');
  FText := AValues[0].ToText;
  Published;
end;

procedure TJourney.ReceiveSingle(const AValues: TNyxSequenceArray<TNyxSingleNumber>);
begin
  Check(Length(AValues) = 1, 'one narrowed binary32 result reaches UI');
  FValue := AValues[0].Value;
  Published;
end;

procedure TJourney.ReceiveSingleMean(const AValues: TNyxSequenceArray<TOptionalSingle>);
begin
  Check((Length(AValues) = 1) and (AValues[0] <> nil), 'one typed optional binary32 mean reaches UI');
  FHasValue := AValues[0].HasValue;

  if FHasValue then
  begin
    FValue := AValues[0].Value.Value;
  end;
  Published;
end;

procedure TJourney.ReceiveDecimal(const AValues: TNyxSequenceArray<TNyxDecimalNumber>);
begin
  Check(Length(AValues) = 1, 'one exact decimal result reaches UI');
  FText := AValues[0].ToText;
  Published;
end;

procedure TJourney.ReceiveDecimalMean(const AValues: TNyxSequenceArray<TOptionalDecimal>);
begin
  Check((Length(AValues) = 1) and (AValues[0] <> nil), 'one typed optional decimal mean reaches UI');
  FHasValue := AValues[0].HasValue;

  if FHasValue then
  begin
    FText := AValues[0].Value.ToText;
  end;
  Published;
end;

procedure TJourney.StartStage;
var
  LOptions: TNyxParallelOptions;
  LIntegers: INyxSequence<Integer>;
  LNumbers: INyxSequence<Double>;
  L64: INyxSequence<TNyxInteger64>;
  LSingles: INyxSequence<TNyxSingleNumber>;
  LDecimals: INyxSequence<TNyxDecimalNumber>;
  LMean: INyxNumberMeanPlan;
  LSum64: INyxInteger64SumPlan;
  LProgram: INyxParallelProgram<Integer, Double>;
  LBudgetProgram: INyxParallelProgram<Integer, TNyxInteger64>;
  LOptionalMean: INyxNumericPlan<INyxOptional<Integer>, TNyxNumericMean, TOptionalNumber>;
  LOptionalSum: INyxNumericPlan<INyxOptional<Integer>, Integer, Integer>;
  LMaximum: TNyxDecimalNumber;
  LSingleMaximum: TNyxSingleNumber;
begin
  FRun := nil;
  FMeasureOwner := nil;
  FMeasure := nil;
  FReceived := 0;
  FPolls := 0;
  FText := '';
  FExpected := '';
  FHasValue := False;
  FValue := 0;
  FFailure := FStage in [2, 5, 7, 10, 11, 16, 20, 24, 29];
  FGeneration := FApp.Context.Generation;
  FIntegerLease := TNyxQueryReceiver<Integer>.Bind(FScheduler, ReceiveInteger, FGeneration);
  FNumberLease := TNyxQueryReceiver<Double>.Bind(FScheduler, ReceiveNumber, FGeneration);
  FMeanLease := TNyxQueryReceiver<TOptionalNumber>.Bind(FScheduler, ReceiveMean, FGeneration);
  F64Lease := TNyxQueryReceiver<TNyxInteger64>.Bind(FScheduler, Receive64, FGeneration);
  FSingleLease := TNyxQueryReceiver<TNyxSingleNumber>.Bind(FScheduler, ReceiveSingle, FGeneration);
  FSingleMeanLease := TNyxQueryReceiver<TOptionalSingle>.Bind(FScheduler, ReceiveSingleMean, FGeneration);
  FDecimalLease := TNyxQueryReceiver<TNyxDecimalNumber>.Bind(FScheduler, ReceiveDecimal, FGeneration);
  FDecimalMeanLease := TNyxQueryReceiver<TOptionalDecimal>.Bind(FScheduler, ReceiveDecimalMean, FGeneration);
  LOptions := TNyxParallelOptions.Defaults.Concurrency(2).PartitionSize(2);

  if not (nscWorkerThreads in FScheduler.Capabilities) then
  begin
    LOptions := LOptions.Execution(npxDeferredUI);
  end;
  LIntegers := NyxRange(1, 12);
  LNumbers := TNyxSequence<Double>.From([1.0, 2.0, 3.0]);
  L64 := TNyxSequence<TNyxInteger64>.From([
    TNyxInteger64.FromParts($200000, 1), TNyxInteger64.FromInteger(2)]);
  LSingleMaximum := TNyxSingleNumber.FromNumber(CMaximumSingle);
  LMaximum := Decimal('79228162514264337593543950335');
  LDecimals := TNyxSequence<TNyxDecimalNumber>.From([Decimal('0.1'), Decimal('0.2')]);
  LSingles := TNyxSequence<TNyxSingleNumber>.From([
    TNyxSingleNumber.FromNumber(16777216), TNyxSingleNumber.FromNumber(1),
    TNyxSingleNumber.FromNumber(-16777216)]);
  case FStage of
    0, 1, 2:
    begin

      if FStage = 1 then
      begin
        LIntegers := TNyxSequence<Integer>.Empty;
      end;

      if FStage = 2 then
      begin
        LIntegers := TNyxSequence<Integer>.From([High(Integer), 1]);
      end;
      FRun := TNyxParallelInteger.Sum.WithOptions(LOptions).Submit(
        LIntegers, FScheduler, FIntegerLease.Port, FGeneration);
    end;
    3, 4, 5:
    begin
      LIntegers := TNyxSequence<Integer>.From([High(Integer), High(Integer)]);

      if FStage in [4, 5] then
      begin
        LIntegers := TNyxSequence<Integer>.Empty;
      end;

      if FStage = 5 then
      begin
        FRun := TNyxParallelInteger.Average(nmaRequired).WithOptions(LOptions).Submit(
          LIntegers, FScheduler, FMeanLease.Port, FGeneration);
      end
      else
      begin
        FRun := TNyxParallelInteger.Average.WithOptions(LOptions).Submit(
          LIntegers, FScheduler, FMeanLease.Port, FGeneration);
      end;
    end;
    6, 7:
    begin
      FExpected := '9007199254740995';

      if FStage = 7 then
      begin
        L64 := TNyxSequence<TNyxInteger64>.From([
          TNyxInteger64.FromParts($7FFFFFFF, $FFFFFFFF), TNyxInteger64.FromInteger(1)]);
      end;
      FRun := TNyxParallelInteger64.Sum.WithOptions(LOptions).Submit(
        L64, FScheduler, F64Lease.Port, FGeneration);
    end;
    8:
    begin
      FRun := TNyxParallelInteger64.ApproximateAverage.WithOptions(LOptions).Submit(
        L64, FScheduler, FMeanLease.Port, FGeneration);
    end;
    9, 10, 11, 12:
    begin
      LNumbers := TNyxSequence<Double>.From([1e16, 1.0, -1e16]);

      if FStage = 10 then
      begin
        LNumbers := TNyxSequence<Double>.From([1.7976931348623157e308, 1.7976931348623157e308]);
      end;

      if FStage in [11, 12] then
      begin
        LNumbers := TNyxSequence<Double>.From([NaN, 1.0]);
      end;

      if FStage = 12 then
      begin
        FRun := TNyxParallelNumber.Sum(nnpIEEE).WithOptions(LOptions).Submit(
          LNumbers, FScheduler, FNumberLease.Port, FGeneration);
      end
      else
      begin
        FRun := TNyxParallelNumber.Sum.WithOptions(LOptions).Submit(
          LNumbers, FScheduler, FNumberLease.Port, FGeneration);
      end;
    end;
    13, 14:
    begin
      LNumbers := TNyxSequence<Double>.From([1.7976931348623157e308, 1.7976931348623157e308]);

      if FStage = 14 then
      begin
        LNumbers := TNyxSequence<Double>.From([1.7976931348623157e308, -1.7976931348623157e308]);
      end;
      FRun := TNyxParallelNumber.Average.WithOptions(LOptions).Submit(
        LNumbers, FScheduler, FMeanLease.Port, FGeneration);
    end;
    15, 16:
    begin

      if FStage = 16 then
      begin
        LSingles := TNyxSequence<TNyxSingleNumber>.From([LSingleMaximum, LSingleMaximum]);
      end;
      FRun := TNyxParallelSingle.Sum.WithOptions(LOptions).Submit(
        LSingles, FScheduler, FSingleLease.Port, FGeneration);
    end;
    17:
    begin
      LSingles := TNyxSequence<TNyxSingleNumber>.From([LSingleMaximum, LSingleMaximum]);
      FRun := TNyxParallelSingle.Average.WithOptions(LOptions).Submit(
        LSingles, FScheduler, FSingleMeanLease.Port, FGeneration);
    end;
    18, 19, 20:
    begin
      FExpected := '0.3';

      if FStage = 19 then
      begin
        LDecimals := TNyxSequence<TNyxDecimalNumber>.From([
          LMaximum, LMaximum, Decimal('-79228162514264337593543950335')]);
        FExpected := LMaximum.ToText;
      end;

      if FStage = 20 then
      begin
        LDecimals := TNyxSequence<TNyxDecimalNumber>.From([LMaximum, LMaximum]);
      end;
      FRun := TNyxParallelDecimal.Sum.WithOptions(LOptions).Submit(
        LDecimals, FScheduler, FDecimalLease.Port, FGeneration);
    end;
    21, 22, 23, 24, 25:
    begin
      LDecimals := TNyxSequence<TNyxDecimalNumber>.From([LMaximum, LMaximum]);
      FExpected := LMaximum.ToText;

      if FStage = 22 then
      begin
        LDecimals := TNyxSequence<TNyxDecimalNumber>.From([Decimal('1'), Decimal('2')]);
        FExpected := '1.5';
      end;

      if FStage in [23, 24, 25] then
      begin
        LDecimals := TNyxSequence<TNyxDecimalNumber>.From([Decimal('1'), Decimal('0'), Decimal('0')]);
        FExpected := '0.3333333333333333333333333333';
      end;

      if FStage = 24 then
      begin
        FRun := TNyxParallelDecimal.Average(ndrExact).WithOptions(LOptions).Submit(
          LDecimals, FScheduler, FDecimalMeanLease.Port, FGeneration);
      end
      else if FStage = 25 then
      begin
        FExpected := '0.3333333333333333333333333334';
        FRun := TNyxParallelDecimal.Average(ndrCeiling).WithOptions(LOptions).Submit(
          LDecimals, FScheduler, FDecimalMeanLease.Port, FGeneration);
      end
      else
      begin
        FRun := TNyxParallelDecimal.Average.WithOptions(LOptions).Submit(
          LDecimals, FScheduler, FDecimalMeanLease.Port, FGeneration);
      end;
    end;
    26:
    begin
      LOptionalSum := TNyxNumericOptionals<Integer, Integer, Integer>.IgnoreAbsent(
        TNyxParallelInteger.Sum.WithOptions(LOptions));
      FRun := LOptionalSum.Submit(TNyxSequence<TOptionalInteger>.From([
        TNyxOptional<Integer>.None, TNyxOptional<Integer>.Some(0), TNyxOptional<Integer>.Some(2)]),
        FScheduler, FIntegerLease.Port, FGeneration);
    end;
    27, 28, 29:
    begin
      LOptionalMean := TNyxNumericOptionals<Integer, TNyxNumericMean, TOptionalNumber>.IgnoreAbsent(
        TNyxParallelInteger.Average.WithOptions(LOptions));

      if FStage = 28 then
      begin
        FRun := LOptionalMean.Submit(TNyxSequence<TOptionalInteger>.From([
          TNyxOptional<Integer>.None, TNyxOptional<Integer>.None]), FScheduler, FMeanLease.Port, FGeneration);
      end
      else if FStage = 29 then
      begin
        FRun := LOptionalMean.Submit(TNyxSequence<TOptionalInteger>.From([
          TNyxOptional<Integer>.Some(1), nil]), FScheduler, FMeanLease.Port, FGeneration);
      end
      else
      begin
        FRun := LOptionalMean.Submit(TNyxSequence<TOptionalInteger>.From([
          TNyxOptional<Integer>.None, TNyxOptional<Integer>.Some(0), TNyxOptional<Integer>.Some(1)]),
          FScheduler, FMeanLease.Port, FGeneration);
      end;
    end;
    30:
    begin
      LProgram := TNyxParallelMap<Integer, Integer, Double>.From(
        TNyxParallelSource<Integer>.From(NyxRange(1, 3)),
        TNyxPartitionSelector<Integer, Double>.From(DoubleValue));
      LMean := TNyxParallelNumber.Average.WithOptions(LOptions);
      FRun := TNyxNumericProgram<Integer, Double, TNyxNumericMean, TOptionalNumber>.Submit(
        LProgram, LMean, FScheduler, FMeanLease.Port, FGeneration);
    end;
    31:
    begin
      FRun := TNyxParallelInteger.Sum.WithOptions(LOptions).Submit(
        LIntegers, FScheduler, FIntegerLease.Port, FGeneration);
      FApp.ShowPage('next-view');
    end;
    32, 33, 34, 35:
    begin
      FRows := 128;

      if FStage = 33 then
      begin
        FRows := 8192;
      end;

      if FStage in [34, 35] then
      begin
        FRows := 131072;
      end;
      FMeasure := TMeasure.Create;
      FMeasureOwner := FMeasure;
      LOptions := LOptions.MaximumInput(262144).PartitionSize(2048).Concurrency(4);

      if FStage = 35 then
      begin
        LOptions := LOptions.CaptureRowsPerTurn(2048);
      end;
      LBudgetProgram := TNyxParallelMap<Integer, Integer, TNyxInteger64>.From(
        TNyxParallelSource<Integer>.From(NyxRange(1, FRows)),
        TNyxPartitionSelector<Integer, TNyxInteger64>.From(To64));
      LSum64 := TNyxNumericPlan<TNyxInteger64, TNyxInteger64, TNyxInteger64>.From(
        TMeasured64.Create(FMeasure)).WithOptions(LOptions);
      FExpected := TNyxInteger64.FromInteger(FRows).ToText;
      FStarted := NowMilliseconds;
      FRun := TNyxNumericProgram<Integer, TNyxInteger64, TNyxInteger64, TNyxInteger64>.Submit(
        LBudgetProgram, LSum64, FScheduler, F64Lease.Port, FGeneration);
    end;
  end;
end;

procedure TJourney.CompleteStage;
var
  LExpected: TNyxInteger64;
  LHalf: Integer;
  LOther: Integer;
  LPartials: Integer;
  LElapsed: Integer;
begin

  if FFailure then
  begin
    Check((FRun.Phase = nqpFailed) and (FReceived = 0), 'numeric failure publishes no partial');
  end
  else if FStage = 31 then
  begin
    Check((FRun.Phase = nqpCancelled) and (FReceived = 0), 'navigation suppresses the numeric result');
    FApp.ShowPage('measured-view');
  end
  else
  begin
    Check((FRun.Phase = nqpSucceeded) and (FReceived = 1), 'numeric plan completes with exactly one UI result');
    case FStage of
      0:
      begin
        Check(FValue = 78, 'checked Integer sum is exact');
      end;
      1:
      begin
        Check(FValue = 0, 'empty sum has a typed zero identity');
      end;
      3:
      begin
        Check(FHasValue and (FValue = High(Integer)), 'integer mean does not require a fitting Integer total');
      end;
      4, 28:
      begin
        Check(not FHasValue, 'empty/all-absent mean preserves explicit None');
      end;
      6:
      begin
        Check(FText = FExpected, 'signed64 sum preserves exact digits above 2^53');
      end;
      8:
      begin
        Check(FHasValue and (Abs(FValue - 4503599627370497) <= 1), 'signed64 mean explicitly exposes approximation');
      end;
      9, 15:
      begin
        Check(FValue = 1, 'compensated sum retains the small cancelled addend');
      end;
      12:
      begin
        Check(IsNan(FValue), 'IEEE sum propagates NaN explicitly');
      end;
      13:
      begin
        Check(FHasValue and (FValue = CMaximumNumber), 'finite mean succeeds while its ordinary sum overflows');
      end;
      14:
      begin
        Check(FHasValue and (FValue = 0), 'opposite extreme mean avoids overflowing subtraction');
      end;
      17:
      begin
        Check(FHasValue and (FValue = CMaximumSingle), 'Single mean narrows once after Double merge');
      end;
      18, 19, 21, 22, 23, 25:
      begin
        Check(FText = FExpected, 'exact decimal partials admit once under the requested rounding');
      end;
      26:
      begin
        Check(FValue = 2, 'optional sum skips None and retains present zero');
      end;
      27:
      begin
        Check(FHasValue and (FValue = 0.5), 'optional mean counts only present inputs');
      end;
      30:
      begin
        Check(FHasValue and (FValue = 4), 'type-changing program uses explicitly configured numeric-plan options');
      end;
      32, 33, 34, 35:
      begin
        LElapsed := Trunc(NowMilliseconds - FStarted);
        { Exact arithmetic reference: n*(n+1)/2, factoring before multiplication.
          The current input sizes keep this intermediate below native Integer
          only after division; use repeated exact additions as an independent
          reference rather than unsafe browser/native wide multiplication. }
        LExpected := TNyxInteger64.FromInteger(0);
        LHalf := FRows div 2;
        LOther := FRows + 1;
        while LHalf > 0 do
        begin
          LExpected := LExpected.Add(TNyxInteger64.FromInteger(LOther));
          Dec(LHalf);
        end;
        Check(FText = LExpected.ToText, 'larger workload delivers the exact independent triangular total');
        LPartials := (FRows + 2047) div 2048;
        Check((FMeasure.Rows = FRows) and (FMeasure.Merges = LPartials) and
          (FMeasure.Seeds = LPartials + 1), 'workload retains one independent state per partition and final merge');
        {$ifndef PAS2JS}
        Check(FMeasure.UIRows = 0, 'actual larger workload row additions stay off UI');
        {$endif}
        FTimings := FTimings + IntToStr(FStage) + '/' + IntToStr(FRows) + ':' +
          IntToStr(LElapsed) + 'ms,capture=' + IntToStr(Trunc(FMeasure.FirstWorker - FStarted)) +
          'ms,work=' + IntToStr(Trunc(FMeasure.WorkFinished - FMeasure.FirstWorker)) + 'ms;';
        {$ifndef PAS2JS}
        WriteLn('MEASURE stage=', FStage, ' rows=', FRows, ' elapsed-ms=', LElapsed,
          ' capture-ms=', Trunc(FMeasure.FirstWorker - FStarted),
          ' work-ms=', Trunc(FMeasure.WorkFinished - FMeasure.FirstWorker),
          ' final-merge-ms=', Trunc(FMeasure.WorkFinished - FMeasure.MergeStarted),
          ' partials=', LPartials);
        {$endif}
      end;
    end;
  end;
  Check(TNyxCodec.Encode(FDocument) = FAuthored, 'numeric runtime leaves exact authored defaults unchanged');
end;

{$ifdef PAS2JS}
procedure ReleaseJourney;
begin
  FreeAndNil(GJourney);
  document.body.setAttribute('data-nyx-numeric-disposed', 'true');
end;
{$endif}

procedure TJourney.Finish(const AFailure: TNyxText);
begin
  Done := True;
  {$ifdef PAS2JS}

  if AFailure = '' then
  begin
    document.body.setAttribute('data-nyx-numeric', 'passed');
    document.body.setAttribute('data-nyx-numeric-checks', IntToStr(FChecks));
    document.body.setAttribute('data-nyx-numeric-timings', FTimings);
  end
  else
  begin
    document.body.textContent := 'FAIL ' + AFailure;
    document.body.setAttribute('data-nyx-numeric', 'failed');
  end;
  window.setTimeout(@ReleaseJourney, 0);
  {$else}

  if AFailure = '' then
  begin
    WriteLn('PASS ', FChecks, ' numeric plans / actual UI checks');
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
      raise ENyxQuery.Create('Numeric observation expired at stage ' + IntToStr(FStage));
    end;

    if FRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;
    Supports(FScheduler, INyxSchedulerMonitor, LMonitor);

    if LMonitor.WorkerLoad.Running <> 0 then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;

    if FStage <= 35 then
    begin
      CompleteStage;
      Inc(FStage);

      if FStage <= 35 then
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
    {$ifdef PAS2JS}
    GJourney.Poll;
    {$else}
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
      document.body.setAttribute('data-nyx-numeric', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}GJourney.Free;{$endif}
end.
