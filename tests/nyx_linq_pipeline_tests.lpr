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
program nyx_linq_pipeline_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

uses
  SysUtils, Classes, nyx.text, nyx.model, nyx.codec, nyx.generated.view,
  nyx.collections, nyx.collections.mount, nyx.linq, nyx.linq.collections,
  nyx.linq.projection, nyx.linq.grouping, nyx.linq.joins, nyx.linq.aggregates,
  nyx.linq.materialization,
  nyx.linq.counts,
  nyx.linq.combinations, nyx.linq.runtime, nyx.linq.numeric,
  nyx.linq.positions, nyx.linq.random,
  nyx.linq.integer64, nyx.numbers.integer64,
  nyx.linq.execution, nyx.linq.publication, nyx.linq.parallel,
  nyx.callback.scope, nyx.scheduler
  {$ifdef PAS2JS}, Web, nyx.render.browser
  {$else}, Interfaces, Forms, Grids, nyx.render.lcl{$endif};

type
  TParentGroup = INyxGroup<TNyxText, TNyxCollectionItem>;
  TParentPair = INyxPairValue<TNyxCollectionItem, TNyxText>;
  TRowSequence = INyxSequence<TNyxCollectionItem>;
  TIndexedRow = INyxIndexedValue<TNyxCollectionItem>;
  TPositionedRow = INyxTripleValue<TIndexedRow, Integer, Integer>;
  TOptionalRow = INyxOptional<TNyxCollectionItem>;
  TParentCount = INyxKeyValue<TNyxText, Integer>;
  TParentCountPair = INyxPairValue<TNyxCollectionItem, TParentCount>;
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  {$endif}

  TSourceMetrics = record
    Starts: Integer;
    Ends: Integer;
    Moves: Integer;
    Reads: Integer;
    Factories: Integer;
  end;
  { The source is UI-confined; managed metrics outlive its cursors. Factory entry
    is checked at the exact full-admission boundary, not inferred from output. }
  ISourceProbe = interface
    ['{D5D1D771-055B-4F63-BE0D-4FBAE08D1930}']
    procedure Started;
    procedure Ended;
    procedure Moved(AIndex: Integer);
    procedure Read;
    procedure Factory;
    function Metrics: TSourceMetrics;
  end;
  TSourceProbe = class(TInterfacedObject, ISourceProbe)
  private
    FScheduler: INyxScheduler;
    FMetrics: TSourceMetrics;
    FFail: Boolean;
  public
    constructor Create(const AScheduler: INyxScheduler; AFail: Boolean);
    procedure Started;
    procedure Ended;
    procedure Moved(AIndex: Integer);
    procedure Read;
    procedure Factory;
    function Metrics: TSourceMetrics;
  end;
  TSourceCursor = class(TInterfacedObject, INyxEnumerator<Integer>)
  private
    FProbe: ISourceProbe;
    FCount: Integer;
    FIndex: Integer;
  public
    constructor Create(const AProbe: ISourceProbe; ACount: Integer);
    destructor Destroy; override;
    function MoveNext: Boolean;
    function GetCurrent: Integer;
  end;
  TSourceSequence = class(TNyxSequence<Integer>)
  private
    FProbe: ISourceProbe;
    FCount: Integer;
  public
    constructor Create(const AProbe: ISourceProbe; ACount: Integer);
    function GetEnumerator: INyxEnumerator<Integer>; override;
  end;
  TSourceFactory = class(TInterfacedObject, INyxPartitionSelectorFactory<Integer, TNyxText>)
  private
    FProbe: ISourceProbe;
  public
    constructor Create(const AProbe: ISourceProbe);
    function NewSelector(APartition, ASourceOffset: Integer): INyxSelector<Integer, TNyxText>;
  end;
  { Existing alternative adapters remain valid for ordinary queries. They must
    explicitly enroll turn capability for deferred parallel source capture. }
  TNoTurns = class(TInterfacedObject, INyxScheduler)
  private
    FBase: INyxScheduler;
  public
    constructor Create(const ABase: INyxScheduler);
    function GetCapabilities: TNyxSchedulerCapabilities;
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    function Submit(const AWork: INyxWork; APolicy: TNyxExecutionPolicy): INyxExecution;
    function PostUI(const AWork: INyxWork; const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
  end;
  TJourney = class
  private
    FDocument: TNyxDocument;
    FAuthored: TNyxText;
    FRenderer: TRenderer;
    FHost: THost;
    FScheduler: INyxScheduler;
    FSource: ISourceProbe;
    FRun: INyxParallelRun;
    FOther: INyxParallelRun;
    FTextLease: INyxQueryReceiver<TNyxText>;
    FRowsLease: INyxCollectionQueryReceiver;
    FTexts: TNyxSequenceArray<TNyxText>;
    FStore: INyxCollection;
    FMount: INyxCollectionMount;
    FObserver: INyxCollectionSubscription;
    FStage: Integer;
    FChecks: Integer;
    FPolls: Integer;
    FReceived: Integer;
    FNotifies: Integer;
    FRevision: Integer;
    FCancelMoves: Integer;
    FObservedTurn: Boolean;
    FSteered: Boolean;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Receive(const AValues: TNyxSequenceArray<TNyxText>);
    procedure Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
    procedure StartStage;
    procedure CompleteStage;
    procedure Cell(const AExpected: TNyxText);
    procedure Finish(const AFailure: TNyxText);
  public
    Done: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Poll;
  end;

var
  GJourney: TJourney;
  GExpiredEntries: Integer;

{$ifdef PAS2JS}
procedure ReleaseJourney;
begin
  GJourney.Free;
  GJourney := nil;
  document.body.setAttribute('data-nyx-pipeline-disposed', 'true');
end;
{$endif}

function Triple(const AValue: Integer): Integer;
begin
  Result := AValue * 3;
end;

function KeepEvenOrdinal(const AValue: INyxIndexedValue<Integer>): Boolean;
begin
  Result := (AValue.Index mod 2) = 0;
end;

function DuplicateValue(const AValue: Integer): INyxSequence<Integer>;
begin
  Result := TNyxSequence<Integer>.From([AValue, AValue]);
end;

function KeepFirstCopy(const AValue: INyxIndexedValue<Integer>): Boolean;
begin
  Result := (AValue.Index mod 2) = 0;
end;

function DuplicateRow(const AValue: TNyxCollectionItem): INyxSequence<TNyxCollectionItem>;
begin
  Result := TNyxSequence<TNyxCollectionItem>.From([AValue, AValue]);
end;

function RowIdentity(const AValue: TNyxCollectionItem): TNyxText;
begin
  Result := AValue.Ref.ID;
end;

function PositionedRow(const AValue: TPositionedRow): TNyxCollectionItem;
var
  LIndexed: TIndexedRow;
  LRow: TNyxCollectionItem;
begin
  LIndexed := AValue.First;

  if LIndexed.Index <> AValue.Second then
  begin
    raise ENyxQuery.Create('Row ordinal and paired position disagree');
  end;
  LRow := LIndexed.Value;
  Result := LRow.WithValue(NyxIntegerField('priority'),
    LRow.GetValue(NyxIntegerField('priority')) + AValue.Third);
end;

function AdmitRow(const AValue: TNyxCollectionItem): TOptionalRow;
begin

  if AValue.GetValue(NyxIntegerField('priority')) < 0 then
  begin
    Result := TNyxOptional<TNyxCollectionItem>.None;
  end
  else
  begin
    Result := TNyxOptional<TNyxCollectionItem>.Some(AValue);
  end;
end;

function ChunkRows(const AValue: TRowSequence): TRowSequence;
begin
  Result := AValue;
end;

function Describe(const AValue: Integer): TNyxText;
begin
  Result := TNyxText('Value ') + TNyxText(IntToStr(AValue)) + TNyxText(' / 🌙');
end;

function Caption(const AValue: TNyxCollectionItem): TNyxText;
begin
  Result := AValue.GetValue(NyxTextField('caption'));
end;

function ParentKey(const AValue: TNyxCollectionItem): TNyxText;
begin
  Result := AValue.GetValue(NyxTextField('parent'));
end;

function CountKey(const AValue: TParentCount): TNyxText;
begin
  Result := AValue.Key;
end;

function CountValue(const AValue: TParentCount): Integer;
begin
  Result := AValue.Value;
end;

function CountedRow(const AValue: TParentCountPair): TNyxCollectionItem;
var
  LRow: TNyxCollectionItem;
  LCount: TParentCount;
begin
  LRow := AValue.Outer;
  LCount := AValue.Inner;
  Result := LRow.WithValue(NyxIntegerField('priority'), LCount.Value);
end;

function KeepRow(const AValue: TNyxCollectionItem): TNyxCollectionItem;
begin
  Result := AValue;
end;

function ParentRows(const AValue: TParentGroup): INyxSequence<TNyxCollectionItem>;
begin
  Result := AValue.Values;
end;

function KeepParent(const AValue: TNyxText): TNyxText;
begin
  Result := AValue;
end;

function JoinedRow(const AValue: TParentPair): TNyxCollectionItem;
var
  LRow: TNyxCollectionItem;
  LParent: TNyxText;
begin
  LRow := AValue.Outer;
  LParent := AValue.Inner;
  { A typed join changes real data, rather than only passing a no-op operator
    through a fixture. The independent candidate retains identity and parent. }
  Result := LRow.WithValue(NyxIntegerField('priority'), Length(LParent));
end;

function PrepareRow(const AValue: TNyxCollectionItem): TNyxCollectionItem;
var
  LAmounts: TNyxSequenceArray<TNyxInteger64>;
  LSource: INyxSequence<TNyxInteger64>;
  LTotal: TNyxInteger64;
begin
  { Each worker owns its exact record vector. Values above 2^53 are never routed
    through Double before the result reaches real LCL/browser control text. }
  SetLength(LAmounts, 2);
  LAmounts[0] := TNyxInteger64.FromParts($200000, 1);
  LAmounts[1] := TNyxInteger64.FromInteger(AValue.GetValue(NyxIntegerField('priority')));
  LSource := TNyxSequence<TNyxInteger64>.From(LAmounts);
  LTotal := TNyxInteger64Aggregate.Sum(LSource);
  Result := AValue.WithValue(NyxTextField('caption'), TNyxText('Pipeline / 🌙 / ') + LTotal.ToText);
end;

function ExpiredSource(const ASource: INyxSequence<Integer>): INyxSequence<Integer>;
var
  LLease: INyxCallbackLease;
  LVariable: TNyxPredicateNested<Integer>;

  function Borrowed(const AValue: Integer): Boolean;
  begin
    Inc(GExpiredEntries);
    Result := AValue >= 0;
  end;

begin
  LLease := NewNyxCallbackLease;
  try
    LVariable := Borrowed;
    Result := ASource.Where(TNyxPredicate<Integer>.FromNested(LVariable, LLease));
  finally
    LLease.Close;
  end;
end;

constructor TSourceProbe.Create(const AScheduler: INyxScheduler; AFail: Boolean);
begin
  inherited Create;
  FScheduler := AScheduler;
  FFail := AFail;
end;

procedure TSourceProbe.Started;
begin
  FScheduler.RequireUI;
  Inc(FMetrics.Starts);
end;

procedure TSourceProbe.Ended;
begin
  FScheduler.RequireUI;
  Inc(FMetrics.Ends);
end;

procedure TSourceProbe.Moved(AIndex: Integer);
begin
  FScheduler.RequireUI;
  Inc(FMetrics.Moves);

  if FFail and (AIndex = 3) then
  begin
    raise ENyxQuery.Create('Source failed / 🌙');
  end;
end;

procedure TSourceProbe.Read;
begin
  FScheduler.RequireUI;
  Inc(FMetrics.Reads);
end;

procedure TSourceProbe.Factory;
begin
  FScheduler.RequireUI;

  if FMetrics.Ends = 0 then
  begin
    raise ENyxQuery.Create('Partition factory entered before source cursor retired');
  end;
  Inc(FMetrics.Factories);
end;

function TSourceProbe.Metrics: TSourceMetrics;
begin
  FScheduler.RequireUI;
  Result := FMetrics;
end;

constructor TSourceCursor.Create(const AProbe: ISourceProbe; ACount: Integer);
begin
  inherited Create;
  FProbe := AProbe;
  FCount := ACount;
  FIndex := -1;
  FProbe.Started;
end;

destructor TSourceCursor.Destroy;
begin
  FProbe.Ended;
  inherited Destroy;
end;

function TSourceCursor.MoveNext: Boolean;
begin
  Inc(FIndex);
  FProbe.Moved(FIndex);
  Result := FIndex < FCount;
end;

function TSourceCursor.GetCurrent: Integer;
begin

  if (FIndex < 0) or (FIndex >= FCount) then
  begin
    raise ENyxQuery.Create('Source Current is outside its cursor');
  end;
  FProbe.Read;
  Result := FIndex;
end;

constructor TSourceSequence.Create(const AProbe: ISourceProbe; ACount: Integer);
begin
  inherited Create;
  FProbe := AProbe;
  FCount := ACount;
end;

function TSourceSequence.GetEnumerator: INyxEnumerator<Integer>;
begin
  Result := TSourceCursor.Create(FProbe, FCount);
end;

constructor TSourceFactory.Create(const AProbe: ISourceProbe);
begin
  inherited Create;
  FProbe := AProbe;
end;

function TSourceFactory.NewSelector(APartition, ASourceOffset: Integer): INyxSelector<Integer, TNyxText>;
begin
  FProbe.Factory;
  Result := TNyxSelector<Integer, TNyxText>.From(Describe);
end;

constructor TNoTurns.Create(const ABase: INyxScheduler);
begin
  inherited Create;
  FBase := ABase;
end;

function TNoTurns.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := FBase.Capabilities;
end;

procedure TNoTurns.RequireUI;
begin
  FBase.RequireUI;
end;

procedure TNoTurns.Admit(APolicy: TNyxExecutionPolicy);
begin
  FBase.Admit(APolicy);
end;

function TNoTurns.Submit(const AWork: INyxWork; APolicy: TNyxExecutionPolicy): INyxExecution;
begin
  Result := FBase.Submit(AWork, APolicy);
end;

function TNoTurns.PostUI(const AWork: INyxWork; const AParent: INyxExecution): INyxExecution;
begin
  Result := FBase.PostUI(AWork, AParent);
end;

procedure TNoTurns.Shutdown;
begin
  FBase.Shutdown;
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxQuery.Create('Pipeline / stage ' + IntToStr(FStage) + ' / ' + AReason);
  end;
  Inc(FChecks);
end;

procedure TJourney.Receive(const AValues: TNyxSequenceArray<TNyxText>);
var
  LIndex: Integer;
begin
  FScheduler.RequireUI;
  Inc(FReceived);
  SetLength(FTexts, Length(AValues));
  for LIndex := 0 to High(FTexts) do
  begin
    FTexts[LIndex] := AValues[LIndex];
  end;
end;

procedure TJourney.Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
begin
  FScheduler.RequireUI;
  Inc(FNotifies);
  Check(AChanges.After.Count = 3, 'actual collection observer sees complete rows');
end;

constructor TJourney.Create;
begin
  inherited Create;
  FScheduler := NewNyxScheduler(TNyxSchedulerOptions.Defaults.Workers(2).PendingCapacity(8));
  FDocument := BuildNyxDocument;
  FAuthored := TNyxCodec.Encode(FDocument);
  FRenderer := TRenderer.Create;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(FHost);
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(0, 0, 800, 700);
  {$endif}
  FRenderer.Render(FDocument, FDocument.Pages[0], FHost);
  FStore := FRenderer.CollectionView('measured-table').Store;
  FMount := FRenderer.CollectionMount('measured-table');
  FObserver := FStore.Subscribe(Changed);
  Cell('Explore the collection');
  StartStage;
end;

destructor TJourney.Destroy;
begin

  if FRun <> nil then
  begin
    FRun.Cancel;
  end;

  if FOther <> nil then
  begin
    FOther.Cancel;
  end;
  FTextLease := nil;
  FRowsLease := nil;
  FObserver := nil;
  FRenderer.Free;
  {$ifdef PAS2JS}

  if FHost <> nil then
  begin
    FHost.remove;
  end;
  {$else}FHost.Free;{$endif}
  FDocument.Free;

  if FScheduler <> nil then
  begin
    FScheduler.Shutdown;
  end;
  inherited Destroy;
end;

procedure TJourney.Cell(const AExpected: TNyxText);
begin
  {$ifdef PAS2JS}
  Check(Pos(AExpected, FRenderer.ElementFor('measured-table').textContent) > 0,
    'actual browser table reflects accepted pipeline rows');
  {$else}
  Check(TNyxText(TStringGrid(FRenderer.ControlFor('measured-table')).Cells[0, 1]) = AExpected,
    'actual LCL table reflects accepted pipeline rows');
  {$endif}
end;

procedure TJourney.StartStage;
var
  LSource: INyxSequence<Integer>;
  LFactory: INyxPartitionSelectorFactory<Integer, TNyxText>;
  LRows: INyxSequence<TNyxCollectionItem>;
  LGroups: INyxSequence<TParentGroup>;
  LChunks: INyxSequence<TRowSequence>;
  LCounts: INyxSequence<TParentCount>;
  LCountMap: INyxDictionary<TNyxText, Integer>;
  LAllowedKeys: INyxHashSet<TNyxText>;
  LMaterialRows: INyxList<TNyxCollectionItem>;
  LWideCount: TNyxQueryCount;
  LIndexedRows: INyxSequence<TIndexedRow>;
  LSnapshot: INyxCollectionSnapshot;
  LOptions: TNyxParallelOptions;
  LAlternative: INyxScheduler;
  LRejected: Boolean;
begin
  FRun := nil;
  FOther := nil;
  FTextLease := nil;
  FRowsLease := nil;
  FReceived := 0;
  FNotifies := 0;
  FTexts := nil;
  FPolls := 0;
  FObservedTurn := False;
  FSteered := False;
  FRevision := FStore.Snapshot.Revision;
  FSource := TSourceProbe.Create(FScheduler, FStage = 3);
  FTextLease := TNyxQueryReceiver<TNyxText>.Bind(FScheduler, Receive);
  LFactory := TSourceFactory.Create(FSource);
  LSource := TSourceSequence.Create(FSource, 32);
  LOptions := TNyxParallelOptions.Defaults.Concurrency(2).PartitionSize(4).CaptureRowsPerTurn(1);
  {$ifdef PAS2JS}LOptions := LOptions.Execution(npxDeferredUI);{$endif}
  case FStage of
    0, 1, 7:
    begin
      LSource := LSource.WhereIndexed(TNyxIndexedPredicate<Integer>.From(KeepEvenOrdinal));

      if FStage = 7 then
      begin
        LSource := LSource.OrderBy(TNyxValueOrder<Integer>.By(NewNyxIntegerComparer, nodDescending));
      end;
      LSource := LSource.Select(TNyxSelector<Integer, Integer>.From(Triple));
      LSource := TNyxSelectMany<Integer, Integer>.From(LSource,
        TNyxSelector<Integer, INyxSequence<Integer>>.From(DuplicateValue))
        .WhereIndexed(TNyxIndexedPredicate<Integer>.From(KeepFirstCopy));
    end;
    2: LOptions := LOptions.MaximumInput(4);
    5: LSource := TSourceSequence.Create(FSource, 0);
    6: LSource := ExpiredSource(LSource);
  end;
  { An alternative scheduler cannot silently claim cooperative turns. Refusal
    must occur without entering the source, factory or receiver. }

  if FStage = 0 then
  begin
    LAlternative := TNoTurns.Create(FScheduler);
    LRejected := False;
    try
      FRun := TNyxParallelSelect<Integer, TNyxText>.Submit(LSource, LFactory,
        LAlternative, FTextLease.Port, LOptions);
    except
      on ENyxSchedule do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (FRun = nil) and (FSource.Metrics.Starts = 0),
      'missing turn capability refuses before source adoption/evaluation');
  end;

  if FStage in [8, 9, 10] then
  begin
    LSnapshot := FStore.Snapshot;
    LGroups := TNyxGroupBy<TNyxCollectionItem, TNyxText, TNyxCollectionItem>.From(
      LSnapshot.AsNyx, TNyxSelector<TNyxCollectionItem, TNyxText>.From(ParentKey),
      TNyxSelector<TNyxCollectionItem, TNyxCollectionItem>.From(KeepRow), NewNyxTextEquality);
    LRows := TNyxSelectMany<TParentGroup, TNyxCollectionItem>.From(LGroups,
      TNyxSelector<TParentGroup, INyxSequence<TNyxCollectionItem>>.From(ParentRows));
    LRows := TNyxJoin<TNyxCollectionItem, TNyxText, TNyxText, TNyxCollectionItem>.Inner(
      LRows, TNyxSequence<TNyxText>.From(['', 'row-0']),
      TNyxSelector<TNyxCollectionItem, TNyxText>.From(ParentKey),
      TNyxSelector<TNyxText, TNyxText>.From(KeepParent),
      TNyxSelector<TParentPair, TNyxCollectionItem>.From(JoinedRow), NewNyxTextEquality);
    LCounts := TNyxCountBy<TNyxCollectionItem, TNyxText>.From(LSnapshot.AsNyx,
      TNyxSelector<TNyxCollectionItem, TNyxText>.From(ParentKey), NewNyxTextEquality);
    LCountMap := TNyxDictionary<TParentCount, TNyxText, Integer>.From(LCounts,
      TNyxSelector<TParentCount, TNyxText>.From(CountKey),
      TNyxSelector<TParentCount, Integer>.From(CountValue), NewNyxTextEquality);
    LCounts := LCountMap.Entries;
    LRows := TNyxJoin<TNyxCollectionItem, TParentCount, TNyxText, TNyxCollectionItem>.Inner(
      LRows, LCounts, TNyxSelector<TNyxCollectionItem, TNyxText>.From(ParentKey),
      TNyxSelector<TParentCount, TNyxText>.From(CountKey),
      TNyxSelector<TParentCountPair, TNyxCollectionItem>.From(CountedRow), NewNyxTextEquality);
    LRows := LRows.OrderBy(TNyxKeyOrder<TNyxCollectionItem, TNyxText>.By(
      TNyxSelector<TNyxCollectionItem, TNyxText>.From(Caption), NewNyxTextComparer));
    LRows := TNyxSelectMany<TNyxCollectionItem, TNyxCollectionItem>.From(LRows,
      TNyxSelector<TNyxCollectionItem, INyxSequence<TNyxCollectionItem>>.From(DuplicateRow));
    { Deduplicate by typed stable identity before candidate publication. Duplicate
      rows would otherwise refuse whole collection admission; no partial data is
      published to conceal an unsuccessful set operation. }
    LRows := TNyxSetBy<TNyxCollectionItem, TNyxText>.DistinctBy(LRows,
      TNyxSelector<TNyxCollectionItem, TNyxText>.From(RowIdentity), NewNyxTextEquality);
    LAllowedKeys := TNyxMaterialize<TNyxText>.ToHashSet(
      TNyxSequence<TNyxText>.From(['row-0', 'row-1', 'row-2']), NewNyxTextEquality);
    LRows := TNyxSetBy<TNyxCollectionItem, TNyxText>.IntersectBy(LRows,
      LAllowedKeys.AsSequence,
      TNyxSelector<TNyxCollectionItem, TNyxText>.From(RowIdentity), NewNyxTextEquality);
    { Reusable owned chunks cross the real publication path, including a short
      final chunk. Reverse twice preserves the authored sorted encounter order;
      a huge tail bound verifies actual-row storage before worker admission. }
    LChunks := TNyxChunk<TNyxCollectionItem>.From(LRows.Reverse, 2);
    LRows := TNyxSelectMany<TRowSequence, TNyxCollectionItem>.From(LChunks,
      TNyxSelector<TRowSequence, TRowSequence>.From(ChunkRows)).Reverse
      .TakeLast(High(Integer));
    LMaterialRows := TNyxMaterialize<TNyxCollectionItem>.ToList(LRows);
    LRows := LMaterialRows.AsSequence;
    LRows := TNyxShuffle<TNyxCollectionItem>.From(LRows, NyxSeededRandom(23));
    LRows := LRows.OrderBy(TNyxKeyOrder<TNyxCollectionItem, TNyxText>.By(
      TNyxSelector<TNyxCollectionItem, TNyxText>.From(Caption), NewNyxTextComparer));
    LIndexedRows := TNyxIndex<TNyxCollectionItem>.From(LRows);
    LRows := TNyxZipThree<TIndexedRow, Integer, Integer, TNyxCollectionItem>.From(
      LIndexedRows, NyxRange(0, 3), NyxRange(1, 3),
      TNyxSelector<TPositionedRow, TNyxCollectionItem>.From(PositionedRow));
    LRows := TNyxTypeQuery<TNyxCollectionItem, TNyxCollectionItem>.Cast(LRows,
      TNyxSelector<TNyxCollectionItem, TOptionalRow>.From(AdmitRow));
    LRows := TNyxSlice<TNyxCollectionItem>.Between(LRows,
      TNyxPosition.FromEnd(3), TNyxPosition.FromEnd(0));
    LWideCount := TNyxCounts<TNyxCollectionItem>.LongCount(LRows);
    Check(LWideCount.ToText = '3', 'exact wide count preserves complete control publication input');
    FRowsLease := NyxCollectionQueryReceiver(FScheduler, FStore);
    FRun := TNyxParallelSelect<TNyxCollectionItem, TNyxCollectionItem>.Submit(LRows,
      TNyxPartitionSelector<TNyxCollectionItem, TNyxCollectionItem>.From(PrepareRow),
      FScheduler, FRowsLease.Port, LOptions.PartitionSize(1));

    if FStage = 9 then
    begin
      FStore.Update(FStore.Snapshot.ItemAt(0).WithValue(NyxTextField('caption'), 'Local edit'));
      FRevision := FStore.Snapshot.Revision;
      FNotifies := 0;
    end;

    if FStage = 10 then
    begin
      FRowsLease.Close;
      FRenderer.Unmount;
    end;
  end
  else
  begin
    FRun := TNyxParallelSelect<Integer, TNyxText>.Submit(LSource, LFactory,
      FScheduler, FTextLease.Port, LOptions);

    if FStage = 1 then
    begin
      FOther := TNyxParallelSelect<Integer, TNyxText>.Submit(LSource, LFactory,
        FScheduler, FTextLease.Port, LOptions);
    end;
    Check((FSource.Metrics.Starts = 0) and (FSource.Metrics.Moves = 0),
      'Submit neither obtains a cursor nor evaluates a deferred source');
  end;
end;

procedure TJourney.CompleteStage;
var
  LMetrics: TSourceMetrics;
  LIndex: Integer;
  LValue: Integer;
begin
  LMetrics := FSource.Metrics;
  Check(LMetrics.Starts = LMetrics.Ends, 'all captured cursors retire independently');
  case FStage of
    0, 1, 7:
    begin
      Check((FRun.Phase = nqpSucceeded) and (Length(FTexts) = 16), 'shaped query reaches parallel projection');
      for LIndex := 0 to High(FTexts) do
      begin
        LValue := LIndex * 6;

        if FStage = 7 then
        begin
          LValue := 90 - LValue;
        end;
        Check(FTexts[LIndex] = Describe(LValue), 'filter/order/project retain exact partitioned values');
      end;

      if FStage = 1 then
      begin
        Check((FOther.Phase = nqpSucceeded) and (FReceived = 2) and
          (LMetrics.Starts = 2) and (LMetrics.Moves = 66), 'same recipe supports independent concurrent submissions');
      end
      else
      begin
        Check((FReceived = 1) and (LMetrics.Starts = 1) and (LMetrics.Moves = 33),
          'one submission enumerates exactly once');
      end;

      if FStage in [0, 1] then
      begin
        Check(FObservedTurn, 'real UI polling interleaves capture before worker factory admission');
      end;
    end;
    2:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.Failure = TNyxText('Deferred parallel input exceeds its configured limit')),
        'oversized deferred input refuses exact complete admission');
      Check((LMetrics.Moves = 5) and (LMetrics.Reads = 4) and (LMetrics.Factories = 0)
        and (FReceived = 0), 'overflow row is not read and no partial worker work enters');
    end;
    3:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.Failure = TNyxText('Source failed / 🌙')),
        'deferred source failure preserves original Unicode diagnostic');
      Check((LMetrics.Factories = 0) and (FReceived = 0), 'source failure schedules no partial partitions');
    end;
    4, 11:
    begin
      Check((FRun.Phase = nqpCancelled) and (LMetrics.Factories = 0) and (FReceived = 0),
        'capture cancellation/shutdown admits no worker or receiver');
      Check(LMetrics.Moves = FCancelMoves, 'cancelled queued turns never reenter the source');
    end;
    5:
    begin
      Check((FRun.Phase = nqpSucceeded) and (FReceived = 1) and (Length(FTexts) = 0)
        and (LMetrics.Starts = 1) and (LMetrics.Moves = 1) and (LMetrics.Factories = 0),
        'empty deferred source enumerates once and delivers without selector creation');
    end;
    6:
    begin
      Check((FRun.Phase = nqpFailed) and (GExpiredEntries = 0) and (LMetrics.Factories = 0),
        'expired named nested callback refuses before borrowed frame or worker entry');
    end;
    8:
    begin
      Check((FRun.Phase = nqpSucceeded) and (FNotifies = 1) and
        (FStore.Snapshot.Revision = FRevision + 1), 'sorted snapshot pipeline publishes one complete collection revision');
      Cell('Pipeline / 🌙 / 9007199254740996');
      Check(FStore.Snapshot.Count = 3, 'group/flatten/join pipeline retains complete membership');
      Check((FStore.Snapshot.ItemAt(0).GetValue(NyxIntegerField('priority')) = 3)
        and (FStore.Snapshot.ItemAt(1).GetValue(NyxIntegerField('priority')) = 3)
        and (FStore.Snapshot.ItemAt(2).GetValue(NyxIntegerField('priority')) = 5),
        'typed keyed counts and zipped positions survive complete parallel publication');
      {$ifdef PAS2JS}
      Check(TJSHTMLInputElement(FRenderer.ElementFor('measured-table')
        .querySelector('td[data-nyx-column="1"] input')).value = '3',
        'actual browser table displays the positioned aggregate value');
      {$else}
      Check(TStringGrid(FRenderer.ControlFor('measured-table')).Cells[1, 1] = '3',
        'actual LCL table displays the positioned aggregate value');
      {$endif}
    end;
    9:
    begin
      Check((FRun.Phase = nqpFailed) and (FNotifies = 0) and (FStore.Snapshot.Revision = FRevision),
        'revision-aware receiver refuses the stale complete pipeline result');
      Check(FStore.Snapshot.ItemAt(0).GetValue(NyxTextField('caption')) = 'Local edit',
        'refusal preserves exact user edit');
    end;
    10:
    begin
      Check((FRun.Phase = nqpDiscarded) and (FNotifies = 0) and not FMount.Connected,
        'retired actual controls discard late pipeline publication');
    end;
  end;
  Check(TNyxCodec.Encode(FDocument) = FAuthored, 'runtime pipeline preserves exact authored companion');
end;

procedure TJourney.Finish(const AFailure: TNyxText);
begin
  Done := True;
  {$ifdef PAS2JS}

  if AFailure = '' then
  begin
    document.body.setAttribute('data-nyx-pipeline', 'passed');
    document.body.setAttribute('data-nyx-pipeline-checks', IntToStr(FChecks));
  end
  else
  begin
    document.body.setAttribute('data-nyx-pipeline', 'failed');
    document.body.textContent := 'FAIL ' + AFailure;
  end;
  window.setTimeout(@ReleaseJourney, 0);
  {$else}

  if AFailure = '' then
  begin
    WriteLn('PASS ', FChecks, ' deferred pipeline / actual collection-control checks');
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
  LMetrics: TSourceMetrics;
  LMonitor: INyxSchedulerMonitor;
begin
  try
    Inc(FPolls);

    if FPolls > 1500 then
    begin
      raise ENyxQuery.Create('Deferred source or worker retirement exceeded its deadline');
    end;
    LMetrics := FSource.Metrics;

    if (LMetrics.Moves > 0) and (LMetrics.Moves < 33) and (LMetrics.Factories = 0) then
    begin
      FObservedTurn := True;

      if not FSteered and (FStage in [4, 11]) then
      begin
        FSteered := True;
        FCancelMoves := LMetrics.Moves;

        if FStage = 4 then
        begin
          FRun.Cancel;
        end
        else
        begin
          FScheduler.Shutdown;
        end;
      end;
    end;

    if not Supports(FScheduler, INyxSchedulerMonitor, LMonitor) then
    begin
      raise ENyxQuery.Create('Scheduler monitor is absent');
    end;

    if (FRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI]) or
      ((FOther <> nil) and (FOther.Phase in [nqpPending, nqpComputing, nqpAwaitingUI]))
      or (LMetrics.Starts <> LMetrics.Ends) or (LMonitor.WorkerLoad.Running <> 0)
      or (LMonitor.WorkerLoad.Pending <> 0) then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 2);{$endif}
      Exit;
    end;

    if FStage <= 11 then
    begin
      CompleteStage;
      Inc(FStage);
    end;

    if FStage <= 11 then
    begin
      StartStage;
      {$ifdef PAS2JS}window.setTimeout(@Poll, 2);{$endif}
      Exit;
    end;

    if LMonitor.WorkerLoad.ActiveWorkers <> 0 then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 2);{$endif}
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
      document.body.setAttribute('data-nyx-pipeline', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}WriteLn('FAIL ', LException.Message); ExitCode := 1;{$endif}
    end;
  end;
  {$ifndef PAS2JS}GJourney.Free;{$endif}
end.
