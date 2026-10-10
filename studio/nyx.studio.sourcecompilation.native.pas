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
  nyx.studio.buildexecutor;

{ Explicit trusted local execution host. Profile/directories/limits are copied
  machine configuration, independent of any output choice or editor project.
  Each Apply owns its own executor and cancellation family on a native worker.
  Missing tools report through the source command without replacing its pair.
  Creation verifies configuration only; it starts no process and creates no file. }
function NewNyxNativeSourceCompiler(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const ALimits: TNyxCompilerLimits): INyxSourceCompiler;

implementation

uses SysUtils, nyx.model, nyx.source, nyx.scheduler, nyx.studio.outputs,
  nyx.studio.sourceprojection, nyx.studio.builds;

type
  TNativeCompiler = class(TInterfacedObject, INyxSourceCompiler)
  public
    Directories: TNyxStudioDirectories;
    Profile: TNyxText;
    Limits: TNyxCompilerLimits;
    Scheduler: INyxScheduler;
    function Start(const ASource: TNyxText;
      const APort: INyxSourceCompilationPort): INyxSourceCompilation;
  end;
  TNativeCompilation = class(TInterfacedObject, INyxSourceCompilation, INyxWork)
  private
    FState: LongInt;
  public
    Directories: TNyxStudioDirectories;
    Profile: TNyxText;
    Limits: TNyxCompilerLimits;
    Source: TNyxText;
    Port: INyxSourceCompilationPort;
    Cancellation: INyxBuildCancellation;
    procedure Cancel;
    function GetState: TNyxSourceCompilationState;
    procedure Execute(const AExecution: INyxExecution);
  end;

procedure TNativeCompilation.Cancel;
begin
  Cancellation.Cancel;
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
begin
  InterlockedExchange(FState, Ord(scsRunning));
  LExecutor := nil;
  LState := scsFailed;
  LFailure := '';
  try
    try
      LExecutor := TNyxBuildExecutor.Create(Directories, Profile);
      LExecutor.ConfigureLimits(Limits);
      LBuild := LExecutor.ProjectSource(Source, NyxPascalUnit(
        NyxCompanionUnitName(Source)), btNativeLCL, spcDefault, Cancellation);

      if LBuild.Projection.State = spsExecuted then
      begin
        LState := scsCompleted;
      end;
    except
      on LException: Exception do
      begin
        LFailure := LException.Message;
      end;
    end;
    { Retire executor/process ownership before reporting terminal. The independent
      result/port may outlive both compiler strategy and editor context. }
    FreeAndNil(LExecutor);

    if LBuild <> nil then
    begin
      Port.Complete(LBuild.Projection, LFailure);
    end
    else
    begin
      Port.Complete(nil, LFailure);
    end;

    if Cancellation.Cancelled then
    begin
      LState := scsCancelled;
    end;
  finally
    LExecutor.Free;
    Port := nil;
    LBuild := nil;
    InterlockedExchange(FState, Ord(LState));
  end;
end;

function TNativeCompiler.Start(const ASource: TNyxText;
  const APort: INyxSourceCompilationPort): INyxSourceCompilation;
var
  LJob: TNativeCompilation;
  LWork: INyxWork;
begin
  Scheduler.RequireUI;

  if APort = nil then
  begin
    raise ENyxModel.Create('Source compilation needs an independent completion port');
  end;
  NyxCompanionUnitName(ASource);
  LJob := TNativeCompilation.Create;
  Result := LJob;
  LWork := LJob;
  LJob.Directories := Directories;
  LJob.Profile := Profile;
  LJob.Limits := Limits;
  LJob.Source := ASource;
  LJob.Port := APort;
  LJob.Cancellation := NewNyxBuildCancellation;
  Scheduler.Submit(LWork, neThreaded);
end;

function NewNyxNativeSourceCompiler(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const ALimits: TNyxCompilerLimits): INyxSourceCompiler;
var
  LOwner: TNativeCompiler;
  LConfiguration: TNyxOutputConfiguration;
begin
  ADirectories.Validate;
  LConfiguration := TNyxOutputConfiguration.Decode(AProfile);
  LConfiguration.Free;
  LOwner := TNativeCompiler.Create;
  Result := LOwner;
  LOwner.Directories := ADirectories;
  LOwner.Profile := AProfile;
  LOwner.Limits := ALimits;
  LOwner.Scheduler := NewNyxScheduler;
end;

end.
