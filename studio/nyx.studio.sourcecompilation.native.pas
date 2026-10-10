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

unit nyx.studio.sourcecompilation.native;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.directories, nyx.studio.sourcecompilation,
  nyx.studio.buildexecutor, nyx.studio.projectionstorage, nyx.scheduler;

type
  { Optional native diagnostic, immutable after terminal publication. It holds
    no host/editor and describes producer or completion-port failure, never
    claims that an editor accepted the projection. Empty while nonterminal. }
  INyxNativeSourceCompilation = interface(INyxSourceCompilation)
    ['{6C080403-81B5-4E91-B222-101026100009}']
    function GetFailure: TNyxText;
    property Failure: TNyxText read GetFailure;
  end;

{ Explicit trusted local execution host. Profile/directories/limits are copied
  machine configuration, independent of any output choice or editor project.
  Each Apply owns its own executor and cancellation family on a native worker.
  Missing tools report through the source command without replacing its pair.
  Creation verifies configuration only; it starts no process and creates no file. }
function NewNyxNativeSourceCompiler(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const ALimits: TNyxCompilerLimits): INyxSourceCompiler; overload;
{ Explicit host override. Each worker receives its own copied storage policy;
  it cannot come from a source command or an editor project. }
function NewNyxNativeSourceCompiler(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const ALimits: TNyxCompilerLimits;
  const AStorage: TNyxProjectionStoragePolicy): INyxSourceCompiler; overload;
{ Trusted host concurrency options, copied into an exclusively owned scheduler.
  Defaults retain four workers/1024 pending slots. Closing the strategy cancels
  its own queued/running operations without blocking the UI for their joins.
  Tokens become terminal only after their independent producer/port retires.
  Pending cancellation calls the independent port on the cancelling thread;
  running delivery uses its worker. Ports must stage/queue rather than wait on
  the UI. A delivery already begun may win over a concurrent cancellation. }
function NewNyxNativeSourceCompiler(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const ALimits: TNyxCompilerLimits;
  const AStorage: TNyxProjectionStoragePolicy;
  const AOptions: TNyxSchedulerOptions): INyxSourceCompiler; overload;

implementation

uses SysUtils, nyx.model, nyx.source, nyx.studio.outputs,
  nyx.studio.sourceprojection, nyx.studio.builds;

type
  TNativeCompiler = class(TInterfacedObject, INyxSourceCompiler)
  private
    FJobs: array of INyxSourceCompilation;
    FClosing: Boolean;
    procedure Prune;
  public
    Directories: TNyxStudioDirectories;
    Profile: TNyxText;
    Limits: TNyxCompilerLimits;
    Storage: TNyxProjectionStoragePolicy;
    Scheduler: INyxScheduler;
    destructor Destroy; override;
    function Start(const ASource: TNyxText;
      const APort: INyxSourceCompilationPort): INyxSourceCompilation;
  end;
  TNativeCompilation = class(TInterfacedObject, INyxSourceCompilation,
    INyxNativeSourceCompilation, INyxWork)
  private
    FState: LongInt;
    FFailure: TNyxText;
  public
    Directories: TNyxStudioDirectories;
    Profile: TNyxText;
    Limits: TNyxCompilerLimits;
    Storage: TNyxProjectionStoragePolicy;
    Source: TNyxText;
    Port: INyxSourceCompilationPort;
    Cancellation: INyxBuildCancellation;
    { Diagnostic/cancellation token only; it never retains its scheduler/work.
      Bound before Start returns. The worker uses its supplied context instead. }
    Execution: INyxExecution;
    procedure Cancel;
    function GetState: TNyxSourceCompilationState;
    function GetFailure: TNyxText;
    procedure Execute(const AExecution: INyxExecution);
  end;
  { Combine caller cancellation with scheduler shutdown, without a host/thread
    reference. The executor polls this interface while draining its child pipes. }
  TInvocationCancellation = class(TInterfacedObject, INyxBuildCancellation)
  public
    Requested: INyxBuildCancellation;
    Execution: INyxExecution;
    procedure Cancel;
    function Cancelled: Boolean;
    procedure Check;
  end;

procedure TInvocationCancellation.Cancel;
begin
  Requested.Cancel;
end;

function TInvocationCancellation.Cancelled: Boolean;
begin
  Result := Requested.Cancelled or Execution.Cancelled;
end;

procedure TInvocationCancellation.Check;
begin

  if Cancelled then
  begin
    raise ENyxBuildCancelled.Create('Source compilation cancellation requested');
  end;
end;

procedure TNativeCompilation.Cancel;
var
  LPort: INyxSourceCompilationPort;
  LState: TNyxSourceCompilationState;
  LProjection: INyxSourceProjection;
begin
  Cancellation.Cancel;

  if Execution <> nil then
  begin
    Execution.Cancel;
  end;
  { Only one of dispatch and pending cancellation can own the port. Running
    here means the winning cancellation is retiring its completion callback,
    not that a compiler was started. Other callers cannot claim terminal while
    that callback still runs. An already running worker owns its own finalization. }

  if InterlockedCompareExchange(FState, Ord(scsRunning), Ord(scsPending)) <>
    Ord(scsPending) then
  begin
    Exit;
  end;
  LState := scsCancelled;
  FFailure := 'Source compilation was cancelled before execution';
  LPort := Port;
  Port := nil;
  try
    try

      if LPort <> nil then
      begin
        LProjection := NyxSourceProjectionFailure(Source, btNativeLCL,
          spsCancelled, FFailure);
        LPort.Complete(LProjection, '');
      end;
    except
      on LException: Exception do
      begin
        FFailure := UTF8Encode(UnicodeString(LException.Message));
        LState := scsFailed;
      end;
    end;
  finally
    LPort := nil;
    InterlockedExchange(FState, Ord(LState));
  end;
end;

function TNativeCompilation.GetFailure: TNyxText;
begin
  Result := '';

  if GetState in [scsCompleted, scsCancelled, scsFailed] then
  begin
    Result := FFailure;
  end;
end;

function TNativeCompilation.GetState: TNyxSourceCompilationState;
begin
  Result := TNyxSourceCompilationState(InterlockedCompareExchange(FState, 0, 0));
end;

procedure TNativeCompilation.Execute(const AExecution: INyxExecution);
var
  LExecutor: TNyxBuildExecutor;
  LBuild: INyxSourceProjectionBuild;
  LFailure: TNyxText;
  LState: TNyxSourceCompilationState;
  LCancelOwner: TInvocationCancellation;
  LCancellation: INyxBuildCancellation;
  LProjection: INyxSourceProjection;
begin

  if InterlockedCompareExchange(FState, Ord(scsRunning), Ord(scsPending)) <>
    Ord(scsPending) then
  begin
    Exit;
  end;
  LExecutor := nil;
  LState := scsFailed;
  LFailure := '';
  try
    try
      { Allocation belongs inside the producer failure boundary too: an admitted
        job must not stay Running with its port retained if setup raises. }
      LCancelOwner := TInvocationCancellation.Create;
      LCancellation := LCancelOwner;
      LCancelOwner.Requested := Cancellation;
      LCancelOwner.Execution := AExecution;
      LExecutor := TNyxBuildExecutor.Create(Directories, Profile);
      LExecutor.ConfigureLimits(Limits);
      LExecutor.ConfigureProjectionStorage(Storage);
      LBuild := LExecutor.ProjectSource(Source, NyxPascalUnit(
        NyxCompanionUnitName(Source)), btNativeLCL, spcDefault, LCancellation);

      if LBuild.Projection.State = spsExecuted then
      begin
        LState := scsCompleted;
      end;

    except
      on LException: Exception do
      begin
        LFailure := UTF8Encode(UnicodeString(LException.Message));
      end;
    end;
    { Retire executor/process ownership before reporting terminal. The independent
      result/port may outlive both compiler strategy and editor context. }
    FreeAndNil(LExecutor);

    if LBuild <> nil then
    begin
      LProjection := LBuild.Projection;
    end;

    if Cancellation.Cancelled or AExecution.Cancelled then
    begin
      LState := scsCancelled;
      LFailure := '';

      if (LProjection = nil) or (LProjection.State <> spsCancelled) then
      begin
        LProjection := NyxSourceProjectionFailure(Source, btNativeLCL,
          spsCancelled, 'Source compilation was cancelled');
      end;
    end;
    try

      { Typed compiler/constructor/cancellation failures retain their diagnostic
        path. AFailure is reserved for infrastructure/port failure, so the
        ordinary editor still classifies a throwing constructor as Rejected. }
      Port.Complete(LProjection, LFailure);
    except
      on LException: Exception do
      begin
        LFailure := UTF8Encode(UnicodeString(LException.Message));
        LState := scsFailed;
      end;
    end;
    FFailure := LFailure;

    if (FFailure = '') and (LProjection <> nil) and
      (LProjection.State <> spsExecuted) then
    begin
      FFailure := LProjection.Message;
    end;
  finally
    LExecutor.Free;
    Port := nil;
    LBuild := nil;
    LProjection := nil;
    LCancellation := nil;
    InterlockedExchange(FState, Ord(LState));
  end;
end;

procedure TNativeCompiler.Prune;
var
  LRetained: array of INyxSourceCompilation;
  LIndex: Integer;
  LCount: Integer;
begin
  { Independent array, so compaction never overwrites an operation still owned
    by a caller or an earlier teardown snapshot. No user callback runs here. }
  SetLength(LRetained, Length(FJobs));
  LCount := 0;
  for LIndex := 0 to High(FJobs) do
  begin

    if FJobs[LIndex].State in [scsPending, scsRunning] then
    begin
      LRetained[LCount] := FJobs[LIndex];
      Inc(LCount);
    end;
  end;
  SetLength(LRetained, LCount);
  FJobs := LRetained;
end;

destructor TNativeCompiler.Destroy;
var
  LJobs: array of INyxSourceCompilation;
  LIndex: Integer;
begin
  FClosing := True;
  { Scheduler closure prevents another pending dispatch and cancels execution
    contexts. Workers own their queue leases; this never joins them on the UI.
    The copied jobs then retire pending ports or request active family retirement. }

  if Scheduler <> nil then
  begin
    Scheduler.Shutdown;
  end;
  LJobs := FJobs;
  FJobs := nil;
  for LIndex := 0 to High(LJobs) do
  begin
    LJobs[LIndex].Cancel;
  end;
  LJobs := nil;
  Scheduler := nil;
  inherited Destroy;
end;

function TNativeCompiler.Start(const ASource: TNyxText;
  const APort: INyxSourceCompilationPort): INyxSourceCompilation;
var
  LJob: TNativeCompilation;
  LWork: INyxWork;
  LCount: Integer;
begin
  Scheduler.RequireUI;

  if FClosing then
  begin
    raise ENyxModel.Create('Native source compiler is closing');
  end;

  if APort = nil then
  begin
    raise ENyxModel.Create('Source compilation needs an independent completion port');
  end;
  ValidateNyxProjectionSource(ASource);
  NyxCompanionUnitName(ASource);
  Prune;
  LJob := TNativeCompilation.Create;
  Result := LJob;
  LWork := LJob;
  LJob.Directories := Directories;
  LJob.Profile := Profile;
  LJob.Limits := Limits;
  LJob.Storage := Storage;
  LJob.Source := ASource;
  LJob.Port := APort;
  LJob.Cancellation := NewNyxBuildCancellation;
  LCount := Length(FJobs);
  SetLength(FJobs, LCount + 1);
  FJobs[LCount] := Result;
  try
    LJob.Execution := Scheduler.Submit(LWork, neThreaded);
  except
    { Admission refusal retains no port and sends no completion for a Start
      that never returned a token. Other already admitted jobs stay untouched. }
    LJob.Port := nil;
    SetLength(FJobs, LCount);
    Result := nil;
    raise;
  end;
end;

function NewNyxNativeSourceCompiler(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const ALimits: TNyxCompilerLimits): INyxSourceCompiler;
begin
  Result := NewNyxNativeSourceCompiler(ADirectories, AProfile, ALimits,
    TNyxProjectionStoragePolicy.Default);
end;

function NewNyxNativeSourceCompiler(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const ALimits: TNyxCompilerLimits;
  const AStorage: TNyxProjectionStoragePolicy): INyxSourceCompiler;
begin
  Result := NewNyxNativeSourceCompiler(ADirectories, AProfile, ALimits, AStorage,
    TNyxSchedulerOptions.Defaults);
end;

function NewNyxNativeSourceCompiler(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const ALimits: TNyxCompilerLimits;
  const AStorage: TNyxProjectionStoragePolicy;
  const AOptions: TNyxSchedulerOptions): INyxSourceCompiler;
var
  LOwner: TNativeCompiler;
  LConfiguration: TNyxOutputConfiguration;
begin
  ADirectories.Validate;
  ALimits.Validate;
  AStorage.Validate;
  AOptions.Validate;
  LConfiguration := TNyxOutputConfiguration.Decode(AProfile);
  LConfiguration.Free;
  LOwner := TNativeCompiler.Create;
  Result := LOwner;
  LOwner.Directories := ADirectories;
  LOwner.Profile := AProfile;
  LOwner.Limits := ALimits;
  LOwner.Storage := AStorage;
  LOwner.Scheduler := NewNyxScheduler(AOptions);
end;

end.
