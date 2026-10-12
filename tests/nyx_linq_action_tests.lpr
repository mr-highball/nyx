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

program nyx_linq_action_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

uses
  SysUtils, Classes, nyx.text, nyx.model, nyx.codec, nyx.generated.view,
  nyx.collections, nyx.linq, nyx.linq.execution, nyx.linq.parallel,
  nyx.linq.pipeline, nyx.events, nyx.scheduler, nyx.callback.scope
  {$ifdef PAS2JS}, Web, nyx.application.browser
  {$else}, Interfaces, Forms, Grids, SyncObjs, nyx.application.lcl{$endif};

type
  {$ifdef PAS2JS}
  TApplicationHost = TNyxBrowserApplication;
  {$else}
  TApplicationHost = TNyxLCLApplication;
  {$endif}
  TActionMode = (amNormal, amCollectFailure, amStopFailure, amCancel, amMissing, amNavigate);
  TNavigation = procedure of object;
  IInlineOffset = interface
    ['{970871B3-1774-4D2A-8646-2038D369F89B}']
    function Value: Integer;
  end;
  TInlineOffset = class(TInterfacedObject, IInlineOffset)
    constructor Create;
    destructor Destroy; override;
    function Value: Integer;
  end;
  {$ifndef PAS2JS}
  TScopedWork = class(TInterfacedObject, INyxWork)
  private
    FAction: INyxParallelAction<Integer>;
  public
    constructor Create(const AAction: INyxParallelAction<Integer>);
    procedure Execute(const AExecution: INyxExecution);
  end;
  {$endif}
  TActionSnapshot = record
    Calls: Integer;
    Sum: Integer;
    Factories: Integer;
    Created: Integer;
    Destroyed: Integer;
    WorkerCalls: Integer;
    Seen: array[0..63] of Integer;
  end;

  { Child work owns exact values, a receiver capability and its cancellation
    parent. It never stores an application/widget/borrowed method directly. }
  TRowDelivery = class(TInterfacedObject, INyxWork)
  private
    FPort: INyxQueryResultPort<Integer>;
  public
    constructor Create(const APort: INyxQueryResultPort<Integer>);
    procedure Execute(const AExecution: INyxExecution);
  end;

  { Native shared measurements are synchronized; immutable policy/ports are fixed
    before admission. The navigation method is borrowed ONLY in explicit DeferredUI
    mode while the journey remains alive. Worker modes receive no such callback. }
  TProbe = class(TInterfacedObject)
  private
    FScheduler: INyxScheduler;
    FPort: INyxQueryResultPort<Integer>;
    FMode: TActionMode;
    FTransformed: Boolean;
    FData: TActionSnapshot;
    FContext: INyxExecution;
    FPosts: array of INyxExecution;
    FNavigate: TNavigation;
    {$ifndef PAS2JS}
    FLock: TRTLCriticalSection;
    FBoth: TEvent;
    FReleased: TEvent;
    FGates: Integer;
    FGate: Boolean;
    {$endif}
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(const AScheduler: INyxScheduler; const APort: INyxQueryResultPort<Integer>;
      AMode: TActionMode; ATransformed: Boolean; ANavigate: TNavigation);
    destructor Destroy; override;
    procedure Factory;
    procedure Created;
    procedure Destroyed;
    procedure Accept(AValue, ASourceIndex: Integer; const AExecution: INyxExecution);
    function Snapshot: TActionSnapshot;
    function ChildrenDone: Boolean;
    function Context: INyxExecution;
    {$ifndef PAS2JS}
    function BothEntered: Boolean;
    procedure Release;
    {$endif}
  end;

  TAction = class(TInterfacedObject, INyxParallelAction<Integer>)
  private
    FProbe: TProbe;
    FOwner: IInterface;
  public
    constructor Create(AProbe: TProbe);
    destructor Destroy; override;
    procedure Invoke(const AValue: Integer; ASourceIndex: Integer;
      const AExecution: INyxExecution);
  end;
  TFactory = class(TInterfacedObject, INyxPartitionActionFactory<Integer>)
  private
    FProbe: TProbe;
    FOwner: IInterface;
  public
    constructor Create(AProbe: TProbe);
    function NewAction(APartition, ASourceOffset: Integer): INyxParallelAction<Integer>;
  end;
  TTextAction = class(TInterfacedObject, INyxParallelAction<TNyxText>)
  private
    FProbe: TProbe;
    FOwner: IInterface;
  public
    constructor Create(AProbe: TProbe);
    destructor Destroy; override;
    procedure Invoke(const AValue: TNyxText; ASourceIndex: Integer;
      const AExecution: INyxExecution);
  end;
  TTextFactory = class(TInterfacedObject, INyxPartitionActionFactory<TNyxText>)
  private
    FProbe: TProbe;
    FOwner: IInterface;
  public
    constructor Create(AProbe: TProbe);
    function NewAction(APartition, ASourceOffset: Integer): INyxParallelAction<TNyxText>;
  end;

  TJourney = class
  private
    FDocument: TNyxDocument;
    FAuthored: TNyxText;
    FApp: TApplicationHost;
    FScheduler: INyxScheduler;
    FStore: INyxCollection;
    FGeneration: INyxCancellationScope;
    FReceiver: INyxQueryReceiver<Integer>;
    FProbe: TProbe;
    FProbeOwner: IInterface;
    FRun: INyxParallelRun;
    FStage: Integer;
    FChecks: Integer;
    FPolls: Integer;
    FUI: Integer;
    FBefore: Integer;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Receive(const AValues: TNyxSequenceArray<Integer>);
    procedure Navigate;
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
  GGlobalCalls: Integer;
  GScopedEntries: Integer;
  GInlineCalls: Integer;
  GInlineCreated: Integer;
  GInlineDestroyed: Integer;
  {$ifndef PAS2JS}GInlineWorkerCalls: LongInt;{$endif}

{$ifdef PAS2JS}
procedure ReleaseJourney;
begin
  GJourney.Free;
  GJourney := nil;
  document.body.setAttribute('data-nyx-actions-disposed', 'true');
end;
{$endif}

function Even(const AValue: Integer): Boolean;
begin
  Result := (AValue mod 2) = 0;
end;

constructor TInlineOffset.Create;
begin
  inherited Create;
  Inc(GInlineCreated);
end;

destructor TInlineOffset.Destroy;
begin
  Inc(GInlineDestroyed);
  inherited Destroy;
end;

function TInlineOffset.Value: Integer;
begin
  Result := 7;
end;

{$ifndef PAS2JS}
constructor TScopedWork.Create(const AAction: INyxParallelAction<Integer>);
begin
  inherited Create;
  FAction := AAction;
end;

procedure TScopedWork.Execute(const AExecution: INyxExecution);
begin
  FAction.Invoke(7, 0, AExecution);
end;
{$endif}

{ Keep this actual frame alive for direct/worker entry, then return a retained
  factory after closing its borrowed lease. Later asynchronous factory admission
  must refuse BEFORE touching the departed frame's static link. }
function EscapedNestedFactory(const AScheduler: INyxScheduler; out AChecks: Integer):
  INyxPartitionActionFactory<Integer>;
var
  LOffset: Integer;
  LLease: INyxCallbackLease;
  LAction: INyxParallelAction<Integer>;
  LExpired: Boolean;
  {$ifndef PAS2JS}
  LExecution: INyxExecution;
  LWork: INyxWork;
  LStart: QWord;
  {$endif}

  procedure Add(const AValue: Integer; ASourceIndex: Integer;
    const AExecution: INyxExecution);
  begin
    Inc(GScopedEntries);
    Inc(LOffset, AValue);
  end;

begin
  AChecks := 0;
  LOffset := 0;
  LLease := NewNyxCallbackLease;
  try
    Result := TNyxPartitionAction<Integer>.FromNested(Add, LLease);
    LAction := Result.NewAction(0, 0);
    LAction.Invoke(7, 0, NewNyxFailedExecution('Direct fixture context'));

    if (LOffset <> 7) or (GScopedEntries <> 1) then
    begin
      raise ENyxQuery.Create('Live nested action lost its borrowed frame');
    end;
    Inc(AChecks);
    {$ifndef PAS2JS}
    LWork := TScopedWork.Create(LAction);
    LExecution := AScheduler.Submit(LWork, neThreaded);
    LStart := GetTickCount64;
    while LExecution.Status in [nesPending, nesRunning] do
    begin

      if GetTickCount64 - LStart > 10000 then
      begin
        raise ENyxQuery.Create('Scoped worker entry observation expired');
      end;
      CheckSynchronize(1);
      Application.ProcessMessages;
      Sleep(1);
    end;

    if (LExecution.Status <> nesFailed) or (GScopedEntries <> 1) then
    begin
      raise ENyxQuery.Create('Nested worker action entered the live foreign frame');
    end;
    Inc(AChecks);
    {$endif}
  finally
    LLease.Close;
  end;
  LExpired := False;
  try
    LAction.Invoke(7, 0, NewNyxFailedExecution('Retained fixture context'));
  except
    on LException: ENyxCallbackExpired do
    begin
      LExpired := True;
    end;
  end;

  if not LExpired or (GScopedEntries <> 1) then
  begin
    raise ENyxQuery.Create('Closed retained nested action entered its frame');
  end;
  Inc(AChecks);
end;

{$ifdef NYX_HAS_INLINE_CALLBACKS}
{ Interface captures are explicit and independent of the returned lexical frame.
  Factory/action wrappers retain them until all partition callbacks are retired. }
function RetainedInlineFactory: INyxPartitionActionFactory<Integer>;
var
  LOffset: IInlineOffset;
begin
  LOffset := TInlineOffset.Create;
  Result := TNyxPartitionAction<Integer>.FromInline(
    procedure(const AValue: Integer; ASourceIndex: Integer; const AExecution: INyxExecution)
    begin

      if AValue <> LOffset.Value then
      begin
        raise ENyxQuery.Create('Inline action lost its exact owned capture');
      end;
      {$ifdef PAS2JS}
      Inc(GInlineCalls);
      {$else}
      InterlockedIncrement(GInlineCalls);

      if GetCurrentThreadID <> MainThreadID then
      begin
        InterlockedIncrement(GInlineWorkerCalls);
      end;
      {$endif}
    end, NyxCapture([LOffset]));
end;
{$endif}

function DoubleValue(const AValue: Integer): Integer;
begin
  Result := AValue * 2;
end;

function Describe(const AValue: Integer): TNyxText;
begin
  Result := 'Item ' + IntToStr(AValue);
end;

function FailSource(const AValue: Integer): Integer;
begin

  if AValue = 3 then
  begin
    raise ENyxQuery.Create('Source admission failed');
  end;
  Result := AValue;
end;

procedure GlobalAction(const AValue: Integer; ASourceIndex: Integer;
  const AExecution: INyxExecution);
begin
  Inc(GGlobalCalls);
end;

constructor TRowDelivery.Create(const APort: INyxQueryResultPort<Integer>);
begin
  inherited Create;
  FPort := APort;
end;

procedure TRowDelivery.Execute(const AExecution: INyxExecution);
begin

  if not AExecution.Cancelled then
  begin
    FPort.Publish([1]);
  end;
end;

constructor TProbe.Create(const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<Integer>; AMode: TActionMode;
  ATransformed: Boolean; ANavigate: TNavigation);
begin
  inherited Create;
  FScheduler := AScheduler;
  FPort := APort;
  FMode := AMode;
  FTransformed := ATransformed;
  FNavigate := ANavigate;
  {$ifndef PAS2JS}
  InitCriticalSection(FLock);
  FBoth := TEvent.Create(nil, True, False, '');
  FReleased := TEvent.Create(nil, True, False, '');
  FGate := ATransformed;
  {$endif}
end;

destructor TProbe.Destroy;
begin
  {$ifndef PAS2JS}
  FReleased.Free;
  FBoth.Free;
  DoneCriticalSection(FLock);
  {$endif}
  inherited Destroy;
end;

procedure TProbe.Lock;
begin
  {$ifndef PAS2JS}EnterCriticalSection(FLock);{$endif}
end;

procedure TProbe.Unlock;
begin
  {$ifndef PAS2JS}LeaveCriticalSection(FLock);{$endif}
end;

procedure TProbe.Factory;
begin
  FScheduler.RequireUI;
  Inc(FData.Factories);
end;

procedure TProbe.Created;
begin
  Lock;
  try
    Inc(FData.Created);
  finally
    Unlock;
  end;
end;

procedure TProbe.Destroyed;
begin
  Lock;
  try
    Inc(FData.Destroyed);
  finally
    Unlock;
  end;
end;

procedure TProbe.Accept(AValue, ASourceIndex: Integer; const AExecution: INyxExecution);
var
  LExpected: Integer;
  LChild: INyxExecution;
  LWork: INyxWork;
  {$ifndef PAS2JS}LGate: Boolean;{$endif}
begin
  LExpected := ASourceIndex;

  if FTransformed then
  begin
    LExpected := ASourceIndex * 2;
  end;

  if (AValue <> LExpected) or (ASourceIndex < 0) or (ASourceIndex > 63) then
  begin
    raise ENyxQuery.Create('Action lost its typed value/original input ordinal');
  end;

  if AExecution.Cancelled then
  begin
    raise ENyxQuery.Create('A cancelled context entered the action');
  end;
  Lock;
  try
    Inc(FData.Calls);
    Inc(FData.Sum, AValue);
    Inc(FData.Seen[ASourceIndex]);
    FContext := AExecution;
    {$ifndef PAS2JS}

    if GetCurrentThreadID <> MainThreadID then
    begin
      Inc(FData.WorkerCalls);
    end;
    LGate := FGate and ((ASourceIndex = 0) or (ASourceIndex = 16));

    if LGate then
    begin
      Inc(FGates);

      if FGates = 2 then
      begin
        FBoth.SetEvent;
      end;
    end;
    {$endif}
  finally
    Unlock;
  end;
  {$ifndef PAS2JS}

  if LGate and (FReleased.WaitFor(10000) <> wrSignaled) then
  begin
    raise ENyxQuery.Create('UI did not release overlapping action fixtures');
  end;
  {$endif}

  if (FMode in [amCollectFailure, amStopFailure]) and
    ((ASourceIndex = 2) or (ASourceIndex = 6)) then
  begin
    raise ENyxQuery.Create('Action failed at ' + IntToStr(ASourceIndex));
  end;

  if (FMode = amCancel) and (ASourceIndex = 2) then
  begin
    AExecution.Cancel;
  end;

  if FPort <> nil then
  begin
    LWork := TRowDelivery.Create(FPort);
    LChild := FScheduler.PostUI(LWork, AExecution);
    Lock;
    try
      SetLength(FPosts, Length(FPosts) + 1);
      FPosts[High(FPosts)] := LChild;
    finally
      Unlock;
    end;
  end;

  if FMode = amNavigate then
  begin
    FNavigate;
  end;
end;

function TProbe.Snapshot: TActionSnapshot;
begin
  Lock;
  try
    Result := FData;
  finally
    Unlock;
  end;
end;

function TProbe.ChildrenDone: Boolean;
var
  LChildren: array of INyxExecution;
  LIndex: Integer;
begin
  Lock;
  try
    LChildren := Copy(FPosts, 0, Length(FPosts));
  finally
    Unlock;
  end;
  Result := True;
  for LIndex := 0 to High(LChildren) do
  begin

    if LChildren[LIndex].Status in [nesPending, nesRunning] then
    begin
      Exit(False);
    end;

    if LChildren[LIndex].Status = nesFailed then
    begin
      raise ENyxQuery.Create(LChildren[LIndex].Failure);
    end;
  end;
end;

function TProbe.Context: INyxExecution;
begin
  Lock;
  try
    Result := FContext;
  finally
    Unlock;
  end;
end;

{$ifndef PAS2JS}
function TProbe.BothEntered: Boolean;
begin
  Result := FBoth.WaitFor(0) = wrSignaled;
end;

procedure TProbe.Release;
begin
  FReleased.SetEvent;
end;
{$endif}

constructor TAction.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
  FProbe.Created;
end;

destructor TAction.Destroy;
begin
  FProbe.Destroyed;
  inherited Destroy;
end;

procedure TAction.Invoke(const AValue: Integer; ASourceIndex: Integer;
  const AExecution: INyxExecution);
begin
  FProbe.Accept(AValue, ASourceIndex, AExecution);
end;

constructor TFactory.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TFactory.NewAction(APartition, ASourceOffset: Integer): INyxParallelAction<Integer>;
begin
  FProbe.Factory;

  if FProbe.FMode = amMissing then
  begin
    Exit(nil);
  end;
  Result := TAction.Create(FProbe);
end;

constructor TTextAction.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
  FProbe.Created;
end;

destructor TTextAction.Destroy;
begin
  FProbe.Destroyed;
  inherited Destroy;
end;

procedure TTextAction.Invoke(const AValue: TNyxText; ASourceIndex: Integer;
  const AExecution: INyxExecution);
begin

  if AValue <> Describe(ASourceIndex) then
  begin
    raise ENyxQuery.Create('Type-changing program lost its text result');
  end;
  FProbe.Accept(ASourceIndex, ASourceIndex, AExecution);
end;

constructor TTextFactory.Create(AProbe: TProbe);
begin
  inherited Create;
  FProbe := AProbe;
  FOwner := AProbe;
end;

function TTextFactory.NewAction(APartition, ASourceOffset: Integer): INyxParallelAction<TNyxText>;
begin
  FProbe.Factory;
  Result := TTextAction.Create(FProbe);
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
  FScheduler := NewNyxScheduler(TNyxSchedulerOptions.Defaults.Workers(2).PendingCapacity(8));
  FDocument := BuildNyxDocument;
  FAuthored := TNyxCodec.Encode(FDocument);
  FApp := TApplicationHost.Create;
  {$ifdef PAS2JS}FApp.Run(FDocument, TJSHTMLElement(document.body));
  {$else}FApp.Mount(FDocument);{$endif}
  FStore := FApp.Collections.Collection(NyxCollection('measured'));
  StartStage;
end;

destructor TJourney.Destroy;
begin
  {$ifndef PAS2JS}

  if FProbe <> nil then
  begin
    FProbe.Release;
  end;
  {$endif}

  if FRun <> nil then
  begin
    FRun.Cancel;
  end;
  FRun := nil;
  FReceiver := nil;
  FProbeOwner := nil;
  FProbe := nil;
  FApp.Free;
  FStore := nil;
  FDocument.Free;
  FScheduler.Shutdown;
  inherited Destroy;
end;

procedure TJourney.Receive(const AValues: TNyxSequenceArray<Integer>);
begin
  FScheduler.RequireUI;
  Check((Length(AValues) = 1) and (AValues[0] = 1), 'child work supplies exact owned values on UI');
  Inc(FUI);
  FStore.Update(FStore.Snapshot.ItemAt(0).WithValue(NyxTextField('caption'),
    'Processed ' + IntToStr(FUI) + ' items'));
end;

procedure TJourney.Navigate;
begin
  FScheduler.RequireUI;
  FApp.ShowPage('next-view');
  Check(FGeneration.Cancelled, 'action navigation retires actual mounted generation');
  Check(FProbe.Context.Cancelled, 'child parent observes retirement inside entered action');
end;

procedure TJourney.StartStage;
var
  LFactory: INyxPartitionActionFactory<Integer>;
  LRows: INyxSequence<Integer>;
  LProgram: INyxParallelProgram<Integer, Integer>;
  LTextProgram: INyxParallelProgram<Integer, TNyxText>;
  LOptions: TNyxParallelOptions;
  LMode: TActionMode;
  LPort: INyxQueryResultPort<Integer>;
  LNavigation: TNavigation;
  LValues: TNyxSequenceArray<Integer>;
  LIndex: Integer;
  LRefused: Boolean;
  LScopeChecks: Integer;
begin
  FRun := nil;
  FReceiver := nil;
  FProbeOwner := nil;
  FProbe := nil;
  FPolls := 0;
  FUI := 0;
  FApp.ShowPage('measured-view');
  FGeneration := NyxEventGeneration(FApp.View.Events);
  FBefore := FStore.Snapshot.Revision;
  LOptions := TNyxParallelOptions.Defaults.Execution(npxDeferredUI).PartitionSize(4).Concurrency(2);
  LMode := amNormal;
  LNavigation := nil;

  if FStage in [0, 1, 9] then
  begin
    { Stage 9 deliberately uses an unbound receiver to establish cancellation
      through PostUI's query-context parent independently of direct-port guards. }
    FReceiver := TNyxQueryReceiver<Integer>.Bind(FScheduler, Receive);
    LPort := FReceiver.Port;
  end;

  if FStage = 9 then
  begin
    LMode := amNavigate;
    LNavigation := Navigate;
  end;

  if FStage = 2 then
  begin
    LMode := amCollectFailure;
    LOptions := LOptions.Failures(npfCollect);
  end;

  if FStage = 3 then
  begin
    LMode := amStopFailure;
    LOptions := LOptions.Concurrency(1);
  end;

  if FStage = 4 then
  begin
    LMode := amCancel;
    LOptions := LOptions.Concurrency(1);
  end;
  {$ifndef PAS2JS}

  if FStage in [2, 3, 4] then
  begin
    LOptions := LOptions.Execution(npxWorkers);
  end;
  {$endif}

  if FStage = 7 then
  begin
    LMode := amMissing;
  end;
  FProbe := TProbe.Create(FScheduler, LPort, LMode, FStage = 0, LNavigation);
  FProbeOwner := FProbe;
  LFactory := TFactory.Create(FProbe);
  LRows := NyxRange(0, 12);
  case FStage of
    0:
    begin
      LOptions := LOptions.PartitionSize(16);
      {$ifndef PAS2JS}LOptions := LOptions.Execution(npxWorkers);{$endif}
      LProgram := TNyxParallelSource<Integer>.From(NyxRange(0, 64))
        .Where(TNyxPartitionPredicate<Integer>.From(Even))
        .Select(TNyxPartitionSelector<Integer, Integer>.From(DoubleValue))
        .WithOptions(LOptions);
      FRun := LProgram.ForAll(FScheduler, LFactory, FGeneration);
    end;
    1:
    begin
      SetLength(LValues, 8);
      for LIndex := 0 to High(LValues) do
      begin
        LValues[LIndex] := LIndex;
      end;
      FRun := TNyxParallelForAll<Integer>.Submit(LValues, LFactory, FScheduler, LOptions, FGeneration);
    end;
    5:
    begin
      FRun := TNyxParallelForAll<Integer>.Submit(TNyxSequence<Integer>.Empty,
        LFactory, FScheduler, LOptions, FGeneration);
    end;
    6:
    begin
      LRows := TNyxSelect<Integer, Integer>.From(LRows, TNyxSelector<Integer, Integer>.From(FailSource));
      FRun := TNyxParallelForAll<Integer>.Submit(LRows, LFactory, FScheduler, LOptions, FGeneration);
    end;
    8:
    begin
      FRun := TNyxParallelForAll<Integer>.Submit(LRows, LFactory, FScheduler, LOptions, FGeneration);
      FApp.ShowPage('next-view');
      Check(FRun.Phase = nqpCancelled, 'pending terminal actions follow page navigation');
    end;
    10:
    begin
      LRefused := False;
      try
        TNyxPartitionAction<Integer>.From(nil);
      except
        on LException: ENyxQuery do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'nil global procedure refuses before admission');
      LRefused := False;
      try
        TNyxParallelForAll<Integer>.Submit(LRows, nil, FScheduler, LOptions);
      except
        on LException: ENyxQuery do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'nil action factory refuses before adoption');
      FRun := TNyxParallelForAll<Integer>.Submit([0, 1],
        TNyxPartitionAction<Integer>.From(GlobalAction), FScheduler, LOptions, FGeneration);
    end;
    11:
    begin
      LTextProgram := TNyxParallelMap<Integer, Integer, TNyxText>.From(
        TNyxParallelSource<Integer>.From(NyxRange(0, 4)),
        TNyxPartitionSelector<Integer, TNyxText>.From(Describe)).WithOptions(LOptions);
      FRun := LTextProgram.ForAll(FScheduler, TTextFactory.Create(FProbe), FGeneration);
    end;
    12:
    begin
      LFactory := EscapedNestedFactory(FScheduler, LScopeChecks);
      Inc(FChecks, LScopeChecks);
      FRun := TNyxParallelForAll<Integer>.Submit([7], LFactory,
        FScheduler, LOptions, FGeneration);
    end;
    13:
    begin
      {$ifdef NYX_HAS_INLINE_CALLBACKS}
      LRefused := False;
      try
        TNyxPartitionAction<Integer>.FromInline(nil);
      except
        on LException: ENyxQuery do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'nil inline procedure refuses before admission');
      LFactory := RetainedInlineFactory;
      {$ifndef PAS2JS}
      LOptions := LOptions.Execution(npxWorkers).PartitionSize(1).Concurrency(2);
      {$endif}
      FRun := TNyxParallelForAll<Integer>.Submit([7, 7], LFactory,
        FScheduler, LOptions, FGeneration);
      {$else}
      Check(not NyxHasInlineCallbacks, 'disabled inline syntax preserves named/nested action APIs');
      FRun := TNyxParallelForAll<Integer>.Submit([], LFactory,
        FScheduler, LOptions, FGeneration);
      {$endif}
    end;
    else
    begin
      FRun := TNyxParallelForAll<Integer>.Submit(LRows, LFactory, FScheduler, LOptions, FGeneration);
    end;
  end;
  Check(TNyxCodec.Encode(FDocument) = FAuthored, 'submission preserves exact semantic-authored defaults');
end;

procedure TJourney.CompleteStage;
var
  LData: TActionSnapshot;
  LIndex: Integer;
  LExpected: Integer;
  LCaption: TNyxText;
begin
  LData := FProbe.Snapshot;
  Check(LData.Created = LData.Destroyed, 'all independently owned partition actions retire');
  Check(TNyxCodec.Encode(FDocument) = FAuthored, 'terminal execution preserves exact authored defaults');
  case FStage of
    0, 1, 11:
    begin
      Check(FRun.Phase = nqpSucceeded, 'complete terminal action run succeeds');
      LExpected := 4;

      if FStage = 0 then
      begin
        LExpected := 32;
        Check(LData.Sum = 1984, 'filtered transformed actions retain exact values');
        for LIndex := 0 to 63 do
        begin
          Check(LData.Seen[LIndex] = Ord((LIndex mod 2) = 0),
            'each accepted original ordinal enters once and rejected rows never enter');
        end;
        {$ifndef PAS2JS}
        Check(LData.WorkerCalls = 32, 'actual native workers execute all accepted actions');
        {$else}
        Check(not (nscWorkerThreads in FScheduler.Capabilities), 'browser declares event-loop execution');
        {$endif}
      end;

      if FStage = 1 then
      begin
        LExpected := 8;
        Check(LData.Sum = 28, 'eager input has exact action values');
      end;
      Check(LData.Calls = LExpected, 'every accepted action completes exactly once');

      if FStage in [0, 1] then
      begin
        Check(FUI = LExpected, 'independent UI children finish without blocking their parent worker');
        Check(FStore.Snapshot.Revision = FBefore + LExpected, 'UI changes use ordinary admitted collection updates');
        LCaption := 'Processed ' + IntToStr(LExpected) + ' items';
        {$ifdef PAS2JS}
        Check(Pos(LCaption, FApp.View.ElementFor('measured-table').textContent) > 0,
          'actual browser application displays UI action effects');
        {$else}
        Check(TNyxText(TStringGrid(FApp.View.ControlFor('measured-table')).Cells[0, 1]) = LCaption,
          'actual native application displays UI action effects');
        {$endif}
      end;
    end;
    2:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.FailureCount = 2), 'Collect keeps both failed partition diagnostics');
      Check((LData.Calls = 10) and (LData.Seen[3] = 0) and (LData.Seen[7] = 0),
        'failed partitions stop while other action effects remain committed');
      Check(((FRun.FailureAt(0).SourceOffset = 0) and (FRun.FailureAt(1).SourceOffset = 4)) or
        ((FRun.FailureAt(0).SourceOffset = 4) and (FRun.FailureAt(1).SourceOffset = 0)),
        'owned failure offsets preserve both exact identities in UI observation order');
    end;
    3:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.FailureCount = 1) and (LData.Calls = 3),
        'Stop suppresses later partition admission without rolling back entered actions');
    end;
    4:
    begin
      Check((FRun.Phase = nqpCancelled) and (LData.Calls = 3),
        'action context cancels the whole run before subsequent rows');
    end;
    5:
    begin
      Check((FRun.Phase = nqpSucceeded) and (LData.Factories = 0) and (LData.Calls = 0),
        'empty source completes without action factory entry');
    end;
    6:
    begin
      Check((FRun.Phase = nqpFailed) and (FRun.Failure = 'Source admission failed') and
        (LData.Factories = 0), 'source failure starts no partial action work');
    end;
    7:
    begin
      Check((FRun.Phase = nqpFailed) and (LData.Calls = 0), 'missing partition action fails explicitly');
    end;
    8:
    begin
      Check((FRun.Phase = nqpCancelled) and (LData.Factories = 0),
        'navigation prevents pending action admission');
    end;
    9:
    begin
      Check((FRun.Phase = nqpCancelled) and (LData.Calls = 1) and (FUI = 0),
        'query-parent cancellation suppresses a UI child posted before navigation');
    end;
    10:
    begin
      Check((FRun.Phase = nqpSucceeded) and (GGlobalCalls = 2), 'typed global procedure adapter executes');
    end;
    12:
    begin
      Check((FRun.Phase = nqpFailed) and (GScopedEntries = 1),
        'departed nested factory refuses before borrowed callback entry');
    end;
    13:
    begin
      {$ifdef NYX_HAS_INLINE_CALLBACKS}
      Check((FRun.Phase = nqpSucceeded) and (GInlineCalls = 2),
        'retained owning inline action survives its original lexical frame');
      Check((GInlineCreated = 1) and (GInlineDestroyed = 1),
        'explicit captured interface retires exactly once after terminal actions');
      {$ifndef PAS2JS}
      Check(GInlineWorkerCalls = 2, 'owning inline captures execute on actual native workers');
      {$endif}
      {$else}
      Check(FRun.Phase = nqpSucceeded, 'ordinary action path survives inline-disabled compiler gate');
      {$endif}
    end;
  end;

  if not (FStage in [0, 1]) then
  begin
    Check(FStore.Snapshot.Revision = FBefore, 'absent/cancelled UI children preserve accepted collection');
  end;
end;

procedure TJourney.Finish(const AFailure: TNyxText);
begin
  Done := True;
  {$ifdef PAS2JS}

  if AFailure <> '' then
  begin
    document.body.textContent := 'FAIL ' + AFailure;
    document.body.setAttribute('data-nyx-actions', 'failed');
  end
  else
  begin
    document.body.setAttribute('data-nyx-actions', 'passed');
    document.body.setAttribute('data-nyx-action-checks', IntToStr(FChecks));
  end;
  window.setTimeout(@ReleaseJourney, 0);
  {$else}

  if AFailure <> '' then
  begin
    WriteLn('FAIL ', AFailure);
    ExitCode := 1;
  end
  else
  begin
    WriteLn('PASS ', FChecks, ' parallel terminal actions / actual UI checks');
  end;
  {$endif}
end;

procedure TJourney.Poll;
var
  LMonitor: INyxSchedulerMonitor;
begin
  try
    Inc(FPolls);

    if FPolls > 2000 then
    begin
      raise ENyxQuery.Create('Parallel action observation expired');
    end;
    {$ifndef PAS2JS}

    if (FStage = 0) and FProbe.BothEntered then
    begin
      FProbe.Release;
    end;
    {$endif}

    if FRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;
    Supports(FScheduler, INyxSchedulerMonitor, LMonitor);

    if not FProbe.ChildrenDone or (LMonitor.WorkerLoad.Running <> 0) then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;

    if FStage <= 13 then
    begin
      CompleteStage;
      Inc(FStage);

      if FStage <= 13 then
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
      document.body.setAttribute('data-nyx-actions', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}GJourney.Free;{$endif}
end.
