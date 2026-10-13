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

unit nyx.linq.numeric.parallel;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.linq, nyx.linq.numeric, nyx.linq.parallel, nyx.linq.pipeline,
  nyx.linq.reductions, nyx.linq.execution, nyx.scheduler, nyx.numbers.integer64,
  nyx.numbers.decimal, nyx.numbers.single;

type
  { Optional means publish None on empty/all-absent input. Required means fail
    before UI publication. Present zero is never absence. }
  TNyxMeanAbsence = (nmaOptional, nmaRequired);

  { Stateless or explicitly immutable/synchronized numeric law. A builder owns
    its mutable partial; Empty must return independent state. Add/Merge/Finish
    execute on the selected policy, never touch UI, and may raise ordinary
    numeric exceptions. Built-ins are immutable. This contract also supports
    application-defined numeric domains without behavioral strings or casts.
    Empty runs during UI partition construction and again on the final merge
    policy, so it must also remain independent of live UI/store state. }
  INyxNumericKernel<T, TPartial, TResult> = interface(IInterface)
    function Empty: TPartial;
    procedure Add(var AState: TPartial; const AValue: T);
    procedure Merge(var AState: TPartial; const AOther: TPartial);
    function Finish(const AState: TPartial): TResult;
  end;
  INyxNumericPlan<T, TPartial, TResult> = interface(IInterface)
    function WithOptions(const AOptions: TNyxParallelOptions): INyxNumericPlan<T, TPartial, TResult>;
    function Submit(const ASource: INyxSequence<T>; const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<TResult>;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun;
    function GetKernel: INyxNumericKernel<T, TPartial, TResult>;
    function GetOptions: TNyxParallelOptions;
    property Kernel: INyxNumericKernel<T, TPartial, TResult> read GetKernel;
    property Options: TNyxParallelOptions read GetOptions;
  end;
  TNyxNumericPlan<T, TPartial, TResult> = class
  public
    class function From(const AKernel: INyxNumericKernel<T, TPartial, TResult>):
      INyxNumericPlan<T, TPartial, TResult>; static;
  end;
  TNyxNumericProgram<TInput, TValue, TPartial, TResult> = class
  public
    { The plan's explicitly configured options govern this run. The source
      program remains immutable; a separately configured composition is used. }
    class function Submit(const AProgram: INyxParallelProgram<TInput, TValue>;
      const APlan: INyxNumericPlan<TValue, TPartial, TResult>;
      const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
      const AGeneration: INyxCancellationScope = nil): INyxParallelRun; static;
  end;
  TNyxNumericOptionals<T, TPartial, TResult> = class
  public
    { Skip only explicit None; nil optional interfaces refuse. Result type,
      empty laws, rounding/overflow and options stay those of the original plan. }
    class function IgnoreAbsent(const APlan: INyxNumericPlan<T, TPartial, TResult>):
      INyxNumericPlan<INyxOptional<T>, TPartial, TResult>; static;
  end;

  { Compensated binary64 partial and bounded-count stable mean partial. Input
    counts follow the coordinator's Integer admission limit; partial grouping
    affects binary rounding, never silently changes finite/IEEE policy. }
  TNyxNumericSum = record
    Sum: Double;
    Correction: Double;
  end;
  TNyxNumericMean = record
    Mean: Double;
    Count: Integer;
  end;
  INyxIntegerSumPlan = INyxNumericPlan<Integer, Integer, Integer>;
  INyxIntegerMeanPlan = INyxNumericPlan<Integer, TNyxNumericMean, INyxOptional<Double>>;
  INyxInteger64SumPlan = INyxNumericPlan<TNyxInteger64, TNyxInteger64, TNyxInteger64>;
  INyxInteger64MeanPlan = INyxNumericPlan<TNyxInteger64, TNyxNumericMean, INyxOptional<Double>>;
  INyxNumberSumPlan = INyxNumericPlan<Double, TNyxNumericSum, Double>;
  INyxNumberMeanPlan = INyxNumericPlan<Double, TNyxNumericMean, INyxOptional<Double>>;
  INyxSingleSumPlan = INyxNumericPlan<TNyxSingleNumber, TNyxNumericSum, TNyxSingleNumber>;
  INyxSingleMeanPlan = INyxNumericPlan<TNyxSingleNumber, TNyxNumericMean, INyxOptional<TNyxSingleNumber>>;
  INyxDecimalSumPlan = INyxNumericPlan<TNyxDecimalNumber, TNyxDecimalAccumulation, TNyxDecimalNumber>;
  INyxDecimalMeanPlan = INyxNumericPlan<TNyxDecimalNumber, TNyxDecimalAccumulation,
    INyxOptional<TNyxDecimalNumber>>;

  TNyxParallelInteger = class
  public
    { Explicit checked additions per partition and final merge. Mean instead
      uses stable Double values, so a large integer total need not overflow. }
    class function Sum: INyxIntegerSumPlan; static;
    class function Average(AAbsence: TNyxMeanAbsence = nmaOptional): INyxIntegerMeanPlan; static;
  end;
  TNyxParallelInteger64 = class
  public
    class function Sum: INyxInteger64SumPlan; static;
    { Approximation is explicit; exact sums never narrow above 2^53. }
    class function ApproximateAverage(AAbsence: TNyxMeanAbsence = nmaOptional):
      INyxInteger64MeanPlan; static;
  end;
  TNyxParallelNumber = class
  public
    class function Sum(APolicy: TNyxNumberPolicy = nnpFinite): INyxNumberSumPlan; static;
    class function Average(APolicy: TNyxNumberPolicy = nnpFinite;
      AAbsence: TNyxMeanAbsence = nmaOptional): INyxNumberMeanPlan; static;
  end;
  TNyxParallelSingle = class
  public
    { Binary32 inputs accumulate in Double; the final result narrows once.
      Finite narrowing overflow refuses. IEEE explicitly allows nonfinite values. }
    class function Sum(APolicy: TNyxNumberPolicy = nnpFinite): INyxSingleSumPlan; static;
    class function Average(APolicy: TNyxNumberPolicy = nnpFinite;
      AAbsence: TNyxMeanAbsence = nmaOptional): INyxSingleMeanPlan; static;
  end;
  TNyxParallelDecimal = class
  public
    { Exact wide partials merge before ONE final admission/rounding step.
      A valid average may succeed even when its total exceeds decimal96. }
    class function Sum(ARounding: TNyxDecimalRounding = ndrExact): INyxDecimalSumPlan; static;
    class function Average(ARounding: TNyxDecimalRounding = ndrToEven;
      AAbsence: TNyxMeanAbsence = nmaOptional): INyxDecimalMeanPlan; static;
  end;

  { Compiler-visible owned support; mutable state is independent per partition
    and final merger. No result vector proportional to input row count is built. }
  TNumericAccumulator<T, TPartial, TResult> = class(TInterfacedObject,
    INyxPartitionAccumulator<T, TPartial>)
  private
    FKernel: INyxNumericKernel<T, TPartial, TResult>;
    FState: TPartial;
  public
    constructor Create(const AKernel: INyxNumericKernel<T, TPartial, TResult>);
    procedure Add(const AValue: T; ASourceIndex: Integer; const AExecution: INyxExecution);
    function Finish(const AExecution: INyxExecution): TPartial;
  end;
  TNumericFactory<T, TPartial, TResult> = class(TInterfacedObject,
    INyxPartitionAccumulatorFactory<T, TPartial>)
  private
    FKernel: INyxNumericKernel<T, TPartial, TResult>;
  public
    constructor Create(const AKernel: INyxNumericKernel<T, TPartial, TResult>);
    function NewAccumulator(APartition, ASourceOffset: Integer): INyxPartitionAccumulator<T, TPartial>;
  end;
  TNumericMerger<T, TPartial, TResult> = class(TInterfacedObject,
    INyxParallelMerger<TPartial, TResult>)
  private
    FKernel: INyxNumericKernel<T, TPartial, TResult>;
  public
    constructor Create(const AKernel: INyxNumericKernel<T, TPartial, TResult>);
    function Merge(const AValues: TNyxSequenceArray<TPartial>; const AExecution: INyxExecution): TResult;
  end;
  TNumericMergerFactory<T, TPartial, TResult> = class(TInterfacedObject,
    INyxParallelMergerFactory<TPartial, TResult>)
  private
    FKernel: INyxNumericKernel<T, TPartial, TResult>;
  public
    constructor Create(const AKernel: INyxNumericKernel<T, TPartial, TResult>);
    function NewMerger: INyxParallelMerger<TPartial, TResult>;
  end;
  TNumericPlan<T, TPartial, TResult> = class(TInterfacedObject, INyxNumericPlan<T, TPartial, TResult>)
  private
    FKernel: INyxNumericKernel<T, TPartial, TResult>;
    FOptions: TNyxParallelOptions;
  public
    constructor Create(const AKernel: INyxNumericKernel<T, TPartial, TResult>;
      const AOptions: TNyxParallelOptions);
    function WithOptions(const AOptions: TNyxParallelOptions): INyxNumericPlan<T, TPartial, TResult>;
    function Submit(const ASource: INyxSequence<T>; const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<TResult>; const AGeneration: INyxCancellationScope): INyxParallelRun;
    function GetKernel: INyxNumericKernel<T, TPartial, TResult>;
    function GetOptions: TNyxParallelOptions;
  end;
  TOptionalKernel<T, TPartial, TResult> = class(TInterfacedObject,
    INyxNumericKernel<INyxOptional<T>, TPartial, TResult>)
  private
    FInner: INyxNumericKernel<T, TPartial, TResult>;
  public
    constructor Create(const AInner: INyxNumericKernel<T, TPartial, TResult>);
    function Empty: TPartial;
    procedure Add(var AState: TPartial; const AValue: INyxOptional<T>);
    procedure Merge(var AState: TPartial; const AOther: TPartial);
    function Finish(const AState: TPartial): TResult;
  end;
  TIntegerSumKernel = class(TInterfacedObject, INyxNumericKernel<Integer, Integer, Integer>)
    function Empty: Integer;
    procedure Add(var AState: Integer; const AValue: Integer);
    procedure Merge(var AState: Integer; const AOther: Integer);
    function Finish(const AState: Integer): Integer;
  end;
  TInteger64SumKernel = class(TInterfacedObject,
    INyxNumericKernel<TNyxInteger64, TNyxInteger64, TNyxInteger64>)
    function Empty: TNyxInteger64;
    procedure Add(var AState: TNyxInteger64; const AValue: TNyxInteger64);
    procedure Merge(var AState: TNyxInteger64; const AOther: TNyxInteger64);
    function Finish(const AState: TNyxInteger64): TNyxInteger64;
  end;
  TSumKernel<T, TResult> = class(TInterfacedObject, INyxNumericKernel<T, TNyxNumericSum, TResult>)
  protected
    FPolicy: TNyxNumberPolicy;
    function Project(const AValue: T): Double; virtual; abstract;
    function Narrow(AValue: Double): TResult; virtual; abstract;
  public
    constructor Create(APolicy: TNyxNumberPolicy);
    function Empty: TNyxNumericSum;
    procedure Add(var AState: TNyxNumericSum; const AValue: T);
    procedure Merge(var AState: TNyxNumericSum; const AOther: TNyxNumericSum);
    function Finish(const AState: TNyxNumericSum): TResult;
  end;
  TNumberSumKernel = class(TSumKernel<Double, Double>)
    function Project(const AValue: Double): Double; override;
    function Narrow(AValue: Double): Double; override;
  end;
  TSingleSumKernel = class(TSumKernel<TNyxSingleNumber, TNyxSingleNumber>)
    function Project(const AValue: TNyxSingleNumber): Double; override;
    function Narrow(AValue: Double): TNyxSingleNumber; override;
  end;
  TMeanKernel<T, TResult> = class(TInterfacedObject,
    INyxNumericKernel<T, TNyxNumericMean, INyxOptional<TResult>>)
  protected
    FPolicy: TNyxNumberPolicy;
    FAbsence: TNyxMeanAbsence;
    function Project(const AValue: T): Double; virtual; abstract;
    function Narrow(AValue: Double): TResult; virtual; abstract;
  public
    constructor Create(APolicy: TNyxNumberPolicy; AAbsence: TNyxMeanAbsence);
    function Empty: TNyxNumericMean;
    procedure Add(var AState: TNyxNumericMean; const AValue: T);
    procedure Merge(var AState: TNyxNumericMean; const AOther: TNyxNumericMean);
    function Finish(const AState: TNyxNumericMean): INyxOptional<TResult>;
  end;
  TIntegerMeanKernel = class(TMeanKernel<Integer, Double>)
    function Project(const AValue: Integer): Double; override;
    function Narrow(AValue: Double): Double; override;
  end;
  TInteger64MeanKernel = class(TMeanKernel<TNyxInteger64, Double>)
    function Project(const AValue: TNyxInteger64): Double; override;
    function Narrow(AValue: Double): Double; override;
  end;
  TNumberMeanKernel = class(TMeanKernel<Double, Double>)
    function Project(const AValue: Double): Double; override;
    function Narrow(AValue: Double): Double; override;
  end;
  TSingleMeanKernel = class(TMeanKernel<TNyxSingleNumber, TNyxSingleNumber>)
    function Project(const AValue: TNyxSingleNumber): Double; override;
    function Narrow(AValue: Double): TNyxSingleNumber; override;
  end;
  TDecimalSumKernel = class(TInterfacedObject,
    INyxNumericKernel<TNyxDecimalNumber, TNyxDecimalAccumulation, TNyxDecimalNumber>)
  private
    FRounding: TNyxDecimalRounding;
  public
    constructor Create(ARounding: TNyxDecimalRounding);
    function Empty: TNyxDecimalAccumulation;
    procedure Add(var AState: TNyxDecimalAccumulation; const AValue: TNyxDecimalNumber);
    procedure Merge(var AState: TNyxDecimalAccumulation; const AOther: TNyxDecimalAccumulation);
    function Finish(const AState: TNyxDecimalAccumulation): TNyxDecimalNumber;
  end;
  TDecimalMeanKernel = class(TInterfacedObject,
    INyxNumericKernel<TNyxDecimalNumber, TNyxDecimalAccumulation, INyxOptional<TNyxDecimalNumber>>)
  private
    FRounding: TNyxDecimalRounding;
    FAbsence: TNyxMeanAbsence;
  public
    constructor Create(ARounding: TNyxDecimalRounding; AAbsence: TNyxMeanAbsence);
    function Empty: TNyxDecimalAccumulation;
    procedure Add(var AState: TNyxDecimalAccumulation; const AValue: TNyxDecimalNumber);
    procedure Merge(var AState: TNyxDecimalAccumulation; const AOther: TNyxDecimalAccumulation);
    function Finish(const AState: TNyxDecimalAccumulation): INyxOptional<TNyxDecimalNumber>;
  end;

{ Checked helpers are exported for native generic template expansion. Candidate
  state is assigned only after all policy/count/arithmetic checks succeed. }
procedure NyxPushNumericSum(var AState: TNyxNumericSum; AValue: Double; APolicy: TNyxNumberPolicy);
procedure NyxMergeNumericMean(var AState: TNyxNumericMean; const AOther: TNyxNumericMean;
  APolicy: TNyxNumberPolicy);
procedure NyxRequireMeanAbsence(AOrdinal: Integer);

implementation

constructor TNumericAccumulator<T, TPartial, TResult>.Create(
  const AKernel: INyxNumericKernel<T, TPartial, TResult>);
begin
  inherited Create;
  FKernel := AKernel;
  FState := FKernel.Empty;
end;

procedure TNumericAccumulator<T, TPartial, TResult>.Add(const AValue: T;
  ASourceIndex: Integer; const AExecution: INyxExecution);
begin

  if not AExecution.Cancelled then
  begin
    FKernel.Add(FState, AValue);
  end;
end;

function TNumericAccumulator<T, TPartial, TResult>.Finish(const AExecution: INyxExecution): TPartial;
begin
  Result := FState;
end;

constructor TNumericFactory<T, TPartial, TResult>.Create(
  const AKernel: INyxNumericKernel<T, TPartial, TResult>);
begin
  inherited Create;
  FKernel := AKernel;
end;

function TNumericFactory<T, TPartial, TResult>.NewAccumulator(APartition, ASourceOffset: Integer):
  INyxPartitionAccumulator<T, TPartial>;
begin
  Result := TNumericAccumulator<T, TPartial, TResult>.Create(FKernel);
end;

constructor TNumericMerger<T, TPartial, TResult>.Create(
  const AKernel: INyxNumericKernel<T, TPartial, TResult>);
begin
  inherited Create;
  FKernel := AKernel;
end;

function TNumericMerger<T, TPartial, TResult>.Merge(const AValues: TNyxSequenceArray<TPartial>;
  const AExecution: INyxExecution): TResult;
var
  LState: TPartial;
  LIndex: Integer;
begin
  LState := FKernel.Empty;
  for LIndex := 0 to High(AValues) do
  begin

    if AExecution.Cancelled then
    begin
      Exit(Default(TResult));
    end;
    FKernel.Merge(LState, AValues[LIndex]);
  end;
  Result := FKernel.Finish(LState);
end;

constructor TNumericMergerFactory<T, TPartial, TResult>.Create(
  const AKernel: INyxNumericKernel<T, TPartial, TResult>);
begin
  inherited Create;
  FKernel := AKernel;
end;

function TNumericMergerFactory<T, TPartial, TResult>.NewMerger: INyxParallelMerger<TPartial, TResult>;
begin
  Result := TNumericMerger<T, TPartial, TResult>.Create(FKernel);
end;

constructor TNumericPlan<T, TPartial, TResult>.Create(
  const AKernel: INyxNumericKernel<T, TPartial, TResult>; const AOptions: TNyxParallelOptions);
begin
  inherited Create;

  if AKernel = nil then
  begin
    raise ENyxQuery.Create('Numeric plan requires an owned kernel');
  end;
  AOptions.Validate;
  FKernel := AKernel;
  FOptions := AOptions;
end;

function TNumericPlan<T, TPartial, TResult>.WithOptions(const AOptions: TNyxParallelOptions):
  INyxNumericPlan<T, TPartial, TResult>;
begin
  Result := TNumericPlan<T, TPartial, TResult>.Create(FKernel, AOptions);
end;

function TNumericPlan<T, TPartial, TResult>.GetKernel: INyxNumericKernel<T, TPartial, TResult>;
begin
  Result := FKernel;
end;

function TNumericPlan<T, TPartial, TResult>.GetOptions: TNyxParallelOptions;
begin
  Result := FOptions;
end;

function TNumericPlan<T, TPartial, TResult>.Submit(const ASource: INyxSequence<T>;
  const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<TResult>;
  const AGeneration: INyxCancellationScope): INyxParallelRun;
begin
  Result := TNyxParallelReduction<T, TPartial, TResult>.Submit(ASource,
    TNumericFactory<T, TPartial, TResult>.Create(FKernel),
    TNumericMergerFactory<T, TPartial, TResult>.Create(FKernel),
    AScheduler, APort, FOptions, AGeneration);
end;

class function TNyxNumericPlan<T, TPartial, TResult>.From(
  const AKernel: INyxNumericKernel<T, TPartial, TResult>): INyxNumericPlan<T, TPartial, TResult>;
begin
  Result := TNumericPlan<T, TPartial, TResult>.Create(AKernel, TNyxParallelOptions.Defaults);
end;

class function TNyxNumericProgram<TInput, TValue, TPartial, TResult>.Submit(
  const AProgram: INyxParallelProgram<TInput, TValue>;
  const APlan: INyxNumericPlan<TValue, TPartial, TResult>; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<TResult>; const AGeneration: INyxCancellationScope): INyxParallelRun;
var
  LProgram: INyxParallelProgram<TInput, TValue>;
begin

  if (AProgram = nil) or (APlan = nil) then
  begin
    raise ENyxQuery.Create('Numeric program requires a program and plan');
  end;
  LProgram := AProgram.WithOptions(APlan.Options);
  Result := TNyxProgramReduction<TInput, TValue, TPartial, TResult>.Submit(LProgram,
    TNumericFactory<TValue, TPartial, TResult>.Create(APlan.Kernel),
    TNumericMergerFactory<TValue, TPartial, TResult>.Create(APlan.Kernel), AScheduler, APort, AGeneration);
end;

constructor TOptionalKernel<T, TPartial, TResult>.Create(
  const AInner: INyxNumericKernel<T, TPartial, TResult>);
begin
  inherited Create;
  FInner := AInner;
end;

function TOptionalKernel<T, TPartial, TResult>.Empty: TPartial;
begin
  Result := FInner.Empty;
end;

procedure TOptionalKernel<T, TPartial, TResult>.Add(var AState: TPartial; const AValue: INyxOptional<T>);
begin

  if AValue = nil then
  begin
    raise ENyxQuery.Create('Numeric optional input requires explicit Some/None');
  end;

  if AValue.HasValue then
  begin
    FInner.Add(AState, AValue.Value);
  end;
end;

procedure TOptionalKernel<T, TPartial, TResult>.Merge(var AState: TPartial; const AOther: TPartial);
begin
  FInner.Merge(AState, AOther);
end;

function TOptionalKernel<T, TPartial, TResult>.Finish(const AState: TPartial): TResult;
begin
  Result := FInner.Finish(AState);
end;

class function TNyxNumericOptionals<T, TPartial, TResult>.IgnoreAbsent(
  const APlan: INyxNumericPlan<T, TPartial, TResult>):
  INyxNumericPlan<INyxOptional<T>, TPartial, TResult>;
type
  TOptionalInput = INyxOptional<T>;
begin

  if APlan = nil then
  begin
    raise ENyxQuery.Create('Optional numeric input requires a plan');
  end;
  Result := TNumericPlan<TOptionalInput, TPartial, TResult>.Create(
    TOptionalKernel<T, TPartial, TResult>.Create(APlan.Kernel), APlan.Options);
end;

function TIntegerSumKernel.Empty: Integer;
begin
  Result := 0;
end;

procedure TIntegerSumKernel.Add(var AState: Integer; const AValue: Integer);
begin
  AState := NyxAddQueryInteger(AState, AValue);
end;

procedure TIntegerSumKernel.Merge(var AState: Integer; const AOther: Integer);
begin
  Add(AState, AOther);
end;

function TIntegerSumKernel.Finish(const AState: Integer): Integer;
begin
  Result := AState;
end;

function TInteger64SumKernel.Empty: TNyxInteger64;
begin
  Result := TNyxInteger64.FromInteger(0);
end;

procedure TInteger64SumKernel.Add(var AState: TNyxInteger64; const AValue: TNyxInteger64);
begin
  AState := AState.Add(AValue);
end;

procedure TInteger64SumKernel.Merge(var AState: TNyxInteger64; const AOther: TNyxInteger64);
begin
  Add(AState, AOther);
end;

function TInteger64SumKernel.Finish(const AState: TNyxInteger64): TNyxInteger64;
begin
  Result := AState;
end;

procedure NyxPushNumericSum(var AState: TNyxNumericSum; AValue: Double; APolicy: TNyxNumberPolicy);
var
  LCandidate: TNyxNumericSum;
  LNext: Double;
  LDelta: Double;
begin
  LCandidate := AState;
  LNext := NyxAddQueryNumber(AState.Sum, AValue, APolicy);

  if not IsNan(LNext) and not IsInfinite(LNext) then
  begin

    if Abs(AState.Sum) >= Abs(AValue) then
    begin
      LDelta := AState.Sum - LNext;
      LDelta := LDelta + AValue;
    end
    else
    begin
      LDelta := AValue - LNext;
      LDelta := LDelta + AState.Sum;
    end;
    LCandidate.Correction := NyxAddQueryNumber(AState.Correction, LDelta, APolicy);
  end
  else
  begin
    LCandidate.Correction := 0;
  end;
  LCandidate.Sum := LNext;
  AState := LCandidate;
end;

procedure NyxRequireMeanAbsence(AOrdinal: Integer);
begin

  if (AOrdinal < Ord(Low(TNyxMeanAbsence))) or (AOrdinal > Ord(High(TNyxMeanAbsence))) then
  begin
    raise ENyxQuery.Create('Unknown numeric mean absence policy');
  end;
end;

procedure NyxMergeNumericMean(var AState: TNyxNumericMean; const AOther: TNyxNumericMean;
  APolicy: TNyxNumberPolicy);
var
  LCandidate: TNyxNumericMean;
  LRight: Double;
  LWeight: Double;
  LLeftWeight: Double;
  LPrevious: Double;
  LContribution: Double;
begin

  if (AState.Count < 0) or (AOther.Count < 0) then
  begin
    raise ENyxQuery.Create('Numeric mean count cannot be negative');
  end;

  if AOther.Count = 0 then
  begin
    Exit;
  end;
  LRight := NyxAddQueryNumber(0, AOther.Mean, APolicy);

  if AState.Count = 0 then
  begin
    AState := AOther;
    AState.Mean := LRight;
    Exit;
  end;
  LCandidate.Count := NyxAddQueryInteger(AState.Count, AOther.Count);

  if IsNan(AState.Mean) or IsNan(LRight) or IsInfinite(AState.Mean) or IsInfinite(LRight) then
  begin
    LCandidate.Mean := NyxAddQueryNumber(AState.Mean, LRight, APolicy);
  end
  else
  begin
    LWeight := AOther.Count / LCandidate.Count;

    if ((AState.Mean < 0) and (LRight > 0)) or ((AState.Mean > 0) and (LRight < 0)) then
    begin
      LLeftWeight := 1 - LWeight;
      LPrevious := AState.Mean * LLeftWeight;
      LContribution := LRight * LWeight;
      LCandidate.Mean := NyxAddQueryNumber(LPrevious, LContribution, APolicy);
    end
    else
    begin
      LContribution := LRight - AState.Mean;
      LContribution := LContribution * LWeight;
      LCandidate.Mean := NyxAddQueryNumber(AState.Mean, LContribution, APolicy);
    end;
  end;
  AState := LCandidate;
end;

constructor TSumKernel<T, TResult>.Create(APolicy: TNyxNumberPolicy);
begin
  inherited Create;
  NyxAddQueryNumber(0, 0, APolicy);
  FPolicy := APolicy;
end;

function TSumKernel<T, TResult>.Empty: TNyxNumericSum;
begin
  Result.Sum := 0;
  Result.Correction := 0;
end;

procedure TSumKernel<T, TResult>.Add(var AState: TNyxNumericSum; const AValue: T);
begin
  NyxPushNumericSum(AState, Project(AValue), FPolicy);
end;

procedure TSumKernel<T, TResult>.Merge(var AState: TNyxNumericSum; const AOther: TNyxNumericSum);
var
  LCandidate: TNyxNumericSum;
begin
  LCandidate := AState;
  NyxPushNumericSum(LCandidate, AOther.Sum, FPolicy);
  NyxPushNumericSum(LCandidate, AOther.Correction, FPolicy);
  AState := LCandidate;
end;

function TSumKernel<T, TResult>.Finish(const AState: TNyxNumericSum): TResult;
begin
  Result := Narrow(NyxAddQueryNumber(AState.Sum, AState.Correction, FPolicy));
end;

function TNumberSumKernel.Project(const AValue: Double): Double;
begin
  Result := AValue;
end;

function TNumberSumKernel.Narrow(AValue: Double): Double;
begin
  Result := AValue;
end;

function TSingleSumKernel.Project(const AValue: TNyxSingleNumber): Double;
begin
  Result := AValue.Value;
end;

function TSingleSumKernel.Narrow(AValue: Double): TNyxSingleNumber;
begin
  Result := TNyxSingleNumber.FromNumber(AValue, FPolicy);
end;

constructor TMeanKernel<T, TResult>.Create(APolicy: TNyxNumberPolicy; AAbsence: TNyxMeanAbsence);
begin
  inherited Create;
  NyxAddQueryNumber(0, 0, APolicy);
  NyxRequireMeanAbsence(Ord(AAbsence));
  FPolicy := APolicy;
  FAbsence := AAbsence;
end;

function TMeanKernel<T, TResult>.Empty: TNyxNumericMean;
begin
  Result.Mean := 0;
  Result.Count := 0;
end;

procedure TMeanKernel<T, TResult>.Add(var AState: TNyxNumericMean; const AValue: T);
var
  LSingle: TNyxNumericMean;
begin
  LSingle.Mean := Project(AValue);
  LSingle.Count := 1;
  NyxMergeNumericMean(AState, LSingle, FPolicy);
end;

procedure TMeanKernel<T, TResult>.Merge(var AState: TNyxNumericMean; const AOther: TNyxNumericMean);
begin
  NyxMergeNumericMean(AState, AOther, FPolicy);
end;

function TMeanKernel<T, TResult>.Finish(const AState: TNyxNumericMean): INyxOptional<TResult>;
begin

  if AState.Count = 0 then
  begin

    if FAbsence = nmaRequired then
    begin
      raise ENyxQueryEmpty.Create('Numeric average requires at least one value');
    end;
    Exit(TNyxOptional<TResult>.None);
  end;
  Result := TNyxOptional<TResult>.Some(Narrow(AState.Mean));
end;

function TIntegerMeanKernel.Project(const AValue: Integer): Double;
begin
  Result := AValue;
end;

function TIntegerMeanKernel.Narrow(AValue: Double): Double;
begin
  Result := AValue;
end;

function TInteger64MeanKernel.Project(const AValue: TNyxInteger64): Double;
begin
  Result := AValue.ApproximateDouble;
end;

function TInteger64MeanKernel.Narrow(AValue: Double): Double;
begin
  Result := AValue;
end;

function TNumberMeanKernel.Project(const AValue: Double): Double;
begin
  Result := AValue;
end;

function TNumberMeanKernel.Narrow(AValue: Double): Double;
begin
  Result := AValue;
end;

function TSingleMeanKernel.Project(const AValue: TNyxSingleNumber): Double;
begin
  Result := AValue.Value;
end;

function TSingleMeanKernel.Narrow(AValue: Double): TNyxSingleNumber;
begin
  Result := TNyxSingleNumber.FromNumber(AValue, FPolicy);
end;

constructor TDecimalSumKernel.Create(ARounding: TNyxDecimalRounding);
begin
  inherited Create;
  TNyxDecimalNumber.FromInteger(0).Quantize(0, ARounding);
  FRounding := ARounding;
end;

function TDecimalSumKernel.Empty: TNyxDecimalAccumulation;
begin
  Result := TNyxDecimalAccumulation.Empty;
end;

procedure TDecimalSumKernel.Add(var AState: TNyxDecimalAccumulation; const AValue: TNyxDecimalNumber);
begin
  AState := AState.Added(AValue);
end;

procedure TDecimalSumKernel.Merge(var AState: TNyxDecimalAccumulation; const AOther: TNyxDecimalAccumulation);
begin
  AState := AState.Merged(AOther);
end;

function TDecimalSumKernel.Finish(const AState: TNyxDecimalAccumulation): TNyxDecimalNumber;
begin
  Result := AState.Total(FRounding);
end;

constructor TDecimalMeanKernel.Create(ARounding: TNyxDecimalRounding; AAbsence: TNyxMeanAbsence);
begin
  inherited Create;
  TNyxDecimalNumber.FromInteger(0).Quantize(0, ARounding);
  NyxRequireMeanAbsence(Ord(AAbsence));
  FRounding := ARounding;
  FAbsence := AAbsence;
end;

function TDecimalMeanKernel.Empty: TNyxDecimalAccumulation;
begin
  Result := TNyxDecimalAccumulation.Empty;
end;

procedure TDecimalMeanKernel.Add(var AState: TNyxDecimalAccumulation; const AValue: TNyxDecimalNumber);
begin
  AState := AState.Added(AValue);
end;

procedure TDecimalMeanKernel.Merge(var AState: TNyxDecimalAccumulation; const AOther: TNyxDecimalAccumulation);
begin
  AState := AState.Merged(AOther);
end;

function TDecimalMeanKernel.Finish(const AState: TNyxDecimalAccumulation): INyxOptional<TNyxDecimalNumber>;
begin

  if AState.Count = 0 then
  begin

    if FAbsence = nmaRequired then
    begin
      raise ENyxQueryEmpty.Create('Decimal average requires at least one value');
    end;
    Exit(TNyxOptional<TNyxDecimalNumber>.None);
  end;
  Result := TNyxOptional<TNyxDecimalNumber>.Some(AState.Average(FRounding));
end;

class function TNyxParallelInteger.Sum: INyxIntegerSumPlan;
begin
  Result := TNyxNumericPlan<Integer, Integer, Integer>.From(TIntegerSumKernel.Create);
end;

class function TNyxParallelInteger.Average(AAbsence: TNyxMeanAbsence): INyxIntegerMeanPlan;
begin
  Result := TNyxNumericPlan<Integer, TNyxNumericMean, INyxOptional<Double>>.From(
    TIntegerMeanKernel.Create(nnpFinite, AAbsence));
end;

class function TNyxParallelInteger64.Sum: INyxInteger64SumPlan;
begin
  Result := TNyxNumericPlan<TNyxInteger64, TNyxInteger64, TNyxInteger64>.From(TInteger64SumKernel.Create);
end;

class function TNyxParallelInteger64.ApproximateAverage(AAbsence: TNyxMeanAbsence): INyxInteger64MeanPlan;
begin
  Result := TNyxNumericPlan<TNyxInteger64, TNyxNumericMean, INyxOptional<Double>>.From(
    TInteger64MeanKernel.Create(nnpFinite, AAbsence));
end;

class function TNyxParallelNumber.Sum(APolicy: TNyxNumberPolicy): INyxNumberSumPlan;
begin
  Result := TNyxNumericPlan<Double, TNyxNumericSum, Double>.From(TNumberSumKernel.Create(APolicy));
end;

class function TNyxParallelNumber.Average(APolicy: TNyxNumberPolicy; AAbsence: TNyxMeanAbsence):
  INyxNumberMeanPlan;
begin
  Result := TNyxNumericPlan<Double, TNyxNumericMean, INyxOptional<Double>>.From(
    TNumberMeanKernel.Create(APolicy, AAbsence));
end;

class function TNyxParallelSingle.Sum(APolicy: TNyxNumberPolicy): INyxSingleSumPlan;
begin
  Result := TNyxNumericPlan<TNyxSingleNumber, TNyxNumericSum, TNyxSingleNumber>.From(
    TSingleSumKernel.Create(APolicy));
end;

class function TNyxParallelSingle.Average(APolicy: TNyxNumberPolicy; AAbsence: TNyxMeanAbsence):
  INyxSingleMeanPlan;
begin
  Result := TNyxNumericPlan<TNyxSingleNumber, TNyxNumericMean, INyxOptional<TNyxSingleNumber>>.From(
    TSingleMeanKernel.Create(APolicy, AAbsence));
end;

class function TNyxParallelDecimal.Sum(ARounding: TNyxDecimalRounding): INyxDecimalSumPlan;
begin
  Result := TNyxNumericPlan<TNyxDecimalNumber, TNyxDecimalAccumulation, TNyxDecimalNumber>.From(
    TDecimalSumKernel.Create(ARounding));
end;

class function TNyxParallelDecimal.Average(ARounding: TNyxDecimalRounding; AAbsence: TNyxMeanAbsence):
  INyxDecimalMeanPlan;
begin
  Result := TNyxNumericPlan<TNyxDecimalNumber, TNyxDecimalAccumulation, INyxOptional<TNyxDecimalNumber>>.From(
    TDecimalMeanKernel.Create(ARounding, AAbsence));
end;

end.
