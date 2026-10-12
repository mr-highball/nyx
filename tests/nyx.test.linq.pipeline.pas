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
unit nyx.test.linq.pipeline;

{$mode delphi}{$H+}{$codepage utf8}

interface

function RunNyxProgramChecks: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.linq, nyx.linq.parallel, nyx.linq.pipeline;

type
  EProgramCallback = class(Exception);
  TMetrics = class
    Factories: Integer;
    Live: Integer;
    Calls: Integer;
    Limit: Integer;
    NilPredicate: Boolean;
  end;
  TLimitedPredicate = class(TInterfacedObject, INyxPredicate<Integer>)
  private
    FMetrics: TMetrics;
    FCalls: Integer;
  public
    constructor Create(AMetrics: TMetrics);
    destructor Destroy; override;
    function Test(const AValue: Integer): Boolean;
  end;
  TPredicateFactory = class(TInterfacedObject, INyxPartitionPredicateFactory<Integer>)
  private
    FMetrics: TMetrics;
  public
    constructor Create(AMetrics: TMetrics);
    function NewPredicate(APartition, ASourceOffset: Integer): INyxPredicate<Integer>;
  end;

var
  GChecks: Integer;
  GMapCalls: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Parallel program: ' + AReason);
  end;
  Inc(GChecks);
end;

function Even(const AValue: Integer): Boolean;
begin
  Result := (AValue mod 2) = 0;
end;

function Never(const AValue: Integer): Boolean;
begin
  Result := False;
end;

function Twice(const AValue: Integer): Integer;
begin
  Inc(GMapCalls);
  Result := AValue * 2;
end;

function Describe(const AValue: Integer): TNyxText;
begin
  Inc(GMapCalls);
  Result := 'Value / ' + TNyxText(IntToStr(AValue));
end;

function Fail(const AValue: Integer): Integer;
begin
  Result := 0;
  raise EProgramCallback.Create('original program failure');
end;

constructor TLimitedPredicate.Create(AMetrics: TMetrics);
begin
  inherited Create;
  FMetrics := AMetrics;
  Inc(FMetrics.Live);
end;

destructor TLimitedPredicate.Destroy;
begin
  Dec(FMetrics.Live);
  inherited Destroy;
end;

function TLimitedPredicate.Test(const AValue: Integer): Boolean;
begin
  Inc(FMetrics.Calls);
  Inc(FCalls);
  Result := FCalls <= FMetrics.Limit;
end;

constructor TPredicateFactory.Create(AMetrics: TMetrics);
begin
  inherited Create;
  FMetrics := AMetrics;
end;

function TPredicateFactory.NewPredicate(APartition, ASourceOffset: Integer): INyxPredicate<Integer>;
begin
  Inc(FMetrics.Factories);

  if FMetrics.NilPredicate then
  begin
    Result := nil;
  end
  else
  begin
    Result := TLimitedPredicate.Create(FMetrics);
  end;
end;

procedure SequentialPrograms;
var
  LBase: INyxParallelProgram<Integer, Integer>;
  LFiltered: INyxParallelProgram<Integer, Integer>;
  LSelected: INyxParallelProgram<Integer, Integer>;
  LText: INyxParallelProgram<Integer, TNyxText>;
  LChanged: INyxParallelProgram<Integer, Integer>;
  LQuery: INyxSequence<Integer>;
  LCursor: INyxEnumerator<Integer>;
  LOther: INyxEnumerator<Integer>;
  LValues: TNyxSequenceArray<Integer>;
  LMetrics: TMetrics;
  LFactory: INyxPartitionPredicateFactory<Integer>;
  LRejected: Boolean;
begin
  LMetrics := TMetrics.Create;
  try
    LMetrics.Limit := 1;
    LFactory := TPredicateFactory.Create(LMetrics);
    LBase := TNyxParallelSource<Integer>.From(NyxRange(0, 6));
    LFiltered := LBase.Where(TNyxPartitionPredicate<Integer>.From(Even));
    LSelected := LFiltered.Select(TNyxPartitionSelector<Integer, Integer>.From(Twice));
    GMapCalls := 0;
    Check((LBase.Source.Count = 6) and (GMapCalls = 0), 'program construction retains original immutable source');
    LQuery := LSelected.AsSequential;
    LCursor := LQuery.GetEnumerator;
    Check(GMapCalls = 0, 'sequential boundary creates no callback before movement');
    LValues := LQuery.ToArray;
    Check((Length(LValues) = 3) and (LValues[0] = 0) and (LValues[2] = 8) and
      (GMapCalls = 3), 'filter/map chain preserves order and skips filtered selectors');
    Check(LBase.AsSequential.Count = 6, 'derived program never changes its earlier recipe');
    LText := TNyxParallelMap<Integer, Integer, TNyxText>.From(LSelected,
      TNyxPartitionSelector<Integer, TNyxText>.From(Describe));
    Check(LText.AsSequential.Last = 'Value / 8', 'typed result-changing map composes without casts');
    LChanged := LSelected.WithOptions(LSelected.Options.Execution(npxDeferredUI).Concurrency(2));
    Check((LChanged.Options.ExecutionMode = npxDeferredUI) and
      (LBase.Options.ExecutionMode = npxWorkers), 'options are independently retained values');
    LChanged := LBase.Where(LFactory);
    LQuery := LChanged.AsSequential;
    LCursor := LQuery.GetEnumerator;
    LOther := LQuery.GetEnumerator;
    Check(LMetrics.Factories = 0, 'sequential cursor creation does not enter callback factory');
    Check(LCursor.MoveNext and LOther.MoveNext and (LCursor.Current = 0) and
      (LOther.Current = 0), 'sequential cursors each own fresh mutable callback state');
    Check(not LCursor.MoveNext and not LOther.MoveNext, 'independent one-row predicates finish separately');
    Check((LMetrics.Factories = 2) and (LMetrics.Live = 0), 'EOF promptly retires both callback chains');
    LChanged := LBase.Where(TNyxPartitionPredicate<Integer>.From(Never))
      .Select(TNyxPartitionSelector<Integer, Integer>.From(Fail));
    Check(LChanged.AsSequential.Count = 0, 'filtered rows never enter a failing later stage');
    LChanged := LBase.Select(TNyxPartitionSelector<Integer, Integer>.From(Fail));
    LCursor := LChanged.AsSequential.GetEnumerator;
    LRejected := False;
    try
      LCursor.MoveNext;
    except
      on LException: EProgramCallback do
      begin
        LRejected := LException.Message = 'original program failure';
      end;
    end;
    Check(LRejected and not LCursor.MoveNext, 'original stage exception stays terminal');
    LMetrics.NilPredicate := True;
    LChanged := LBase.Where(LFactory);
    LCursor := LChanged.AsSequential.GetEnumerator;
    LRejected := False;
    try
      LCursor.MoveNext;
    except
      on LException: ENyxQuery do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LCursor.MoveNext, 'incomplete callback factory refuses before row execution');
    LBase := TNyxParallelSource<Integer>.From(TNyxSequence<Integer>.Empty);
    LChanged := LBase.Where(LFactory);
    LMetrics.Factories := 0;
    Check((LChanged.AsSequential.Count = 0) and (LMetrics.Factories = 0),
      'empty sequential source never enters a callback factory');
  finally
    LCursor := nil;
    LOther := nil;
    LQuery := nil;
    LBase := nil;
    LFiltered := nil;
    LSelected := nil;
    LChanged := nil;
    LText := nil;
    LFactory := nil;
    LMetrics.Free;
  end;
end;

function RunNyxProgramChecks: Integer;
begin
  GChecks := 0;
  SequentialPrograms;
  Result := GChecks;
end;

end.
