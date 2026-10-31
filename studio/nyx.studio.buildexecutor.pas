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

unit nyx.studio.buildexecutor;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Process, SyncObjs, fpjson, nyx.text, nyx.model, nyx.studio.outputs,
  nyx.studio.directories, nyx.studio.builds, nyx.source,
  nyx.studio.sourceprojection, nyx.studio.projectionstorage, nyx.studio.compiler,
  nyx.pascal.project, nyx.studio.projectinputs;

type
  { Synchronous worker-thread receiver of independent immutable report snapshots.
    It must not touch UI or borrow a mutable source tree. The executor retains
    this interface through actual process join. Receiver failures follow the
    owning Build/ProjectSource failure path after retirement; progress never
    grants terminal/artifact/publication authority. Cancellation prevents later
    progress delivery. }
  INyxCompilerProgress = interface
    ['{662B7680-F23D-4A3C-82AF-80522F740887}']
    procedure Publish(const AReport: INyxCompilerReport);
  end;

  { A monotonic, thread-safe request to retire an owned compiler. Holding this
    interface never retains a job, document or transport. Cancellation of an
    HTTP request alone is deliberately not a compiler cancellation request. }
  INyxBuildCancellation = interface
    ['{D1C91834-5DA4-48F2-A6D1-B719A29A5F38}']
    { Idempotent and irreversible for this invocation. Safe from any thread. }
    procedure Cancel;
    function Cancelled: Boolean;
    { Raises ENyxBuildCancelled when retirement was requested. }
    procedure Check;
  end;

  { Cancellation leaves publication to the existing owning job lifecycle. }
  ENyxBuildCancelled = class(ENyxModel);

  { Trusted host policy, copied by value. Wire requests cannot change budgets.
    A cold library build has a five-minute compilation default; execution of
    a saved constructor retains its separate one-minute default. Positive
    Cardinal durations are finite host choices, not authority from a request.
    Log admission retains the one-MiB transport/evidence ceiling. A zero-filled
    record is invalid: start with Default and override the desired axis. }
  TNyxCompilerLimits = record
  private
    FCompilationMilliseconds: Cardinal;
    FExecutionMilliseconds: Cardinal;
    FLogBytes: Integer;
  public
    class function Default: TNyxCompilerLimits; static;
    { Compatibility shorthand: set both time budgets in the returned copy.
      Independent setters retain the other time budget and the log budget.
      Zero durations and invalid log limits refuse with ENyxModel. Cancellation
      and process-family retirement remain active under every admitted budget. }
    function TimeMilliseconds(AValue: Cardinal): TNyxCompilerLimits;
    function CompilationMilliseconds(AValue: Cardinal): TNyxCompilerLimits;
    function ExecutionMilliseconds(AValue: Cardinal): TNyxCompilerLimits;
    function LogBytes(AValue: Integer): TNyxCompilerLimits;
    procedure Validate;
    property CompilationTimeMilliseconds: Cardinal read FCompilationMilliseconds;
    property ExecutionTimeMilliseconds: Cardinal read FExecutionMilliseconds;
    property MaximumLogBytes: Integer read FLogBytes;
  end;

  { One independent compiler invocation. Owns an immutable machine profile and
    borrows the caller's detached document only during Build. Both HTTP and MCP
    use this fixed-argument implementation; requests cannot supply shell commands,
    compiler options, paths or environment overrides. Artifacts are confined to
    the admitted runtime Jobs root. On Windows, owned library sources are leased
    against writes/replacement and tree changes refuse the compiler result.
    External toolchain/configuration/includes retain their separate host trust.
    Repository-mode callers retain build/studio/jobs. No editor/model lock is held here. }
  TNyxBuildExecutor = class
  private
    FDirectories: TNyxStudioDirectories;
    FJobRoot: TNyxText;
    FOutputs: TNyxOutputConfiguration;
    FLimits: TNyxCompilerLimits;
    FProjectionStorage: TNyxProjectionStoragePolicy;
    FFailure: TNyxCompilerFailure;
    FProgress: INyxCompilerProgress;
    procedure CheckOutput(const ATarget: TNyxText);
    { Compiler runs lease owned library inputs on Windows. Constructor execution
      passes False: trusted application side effects do not borrow source files.
      Both modes retain identical process-family/cancellation ownership. }
    function RunCompiler(const AExecutable, ADirectory: TNyxText;
      AArguments: TStrings; out ALog: TNyxText;
      const ACancellation: INyxBuildCancellation;
      ACompiler: Boolean = True;
      const AReader: INyxCompilerReportReader = nil): Boolean;
    function ProjectSourceInputs(const ASource: TNyxText; const AUnit: TNyxPascalUnitRef;
      ATarget: TNyxBuildTarget; AChecks: TNyxSourceProjectionChecks;
      const ACancellation: INyxBuildCancellation;
      const AInputs: INyxPascalProject): INyxSourceProjectionBuild;
    function BuildCaptured(ADocument: TNyxDocument;
      const ATarget, AScope, APage, ACompanion: TNyxText;
      const ACheckpoint: TNyxSourceCheckpoint;
      const ACancellation: INyxBuildCancellation;
      const AProject: INyxPascalProjectBuild): TJSONObject;
  public
    constructor Create(const ARepository, AProfile: TNyxText); overload;
    { Retains the admitted directory value separately from the immutable profile.
      Compiler input leases last through actual family join; artifacts belong
      to runtime Jobs. A built editor still launches without any compiler/lease. }
    constructor Create(const ADirectories: TNyxStudioDirectories;
      const AProfile: TNyxText); overload;
    destructor Destroy; override;
    { Readiness is diagnostic only; absent compilers never prevent authoring.
      Empty means ready. Contains no machine path values. }
    function Readiness(const ATarget: TNyxText): TNyxText;
    { Called by the trusted host before delegation; never during Build. }
    procedure ConfigureLimits(const ALimits: TNyxCompilerLimits);
    { Trusted host configuration before delegation; nil disables streaming.
      Final compiler receipts are identical with or without this observer. }
    procedure ConfigureProgress(const AProgress: INyxCompilerProgress);
    { Trusted host configuration before delegation, copied by value. Native
      constructors retain source/result/log/project-input evidence and retire unchanged
      compiler derivatives after join. Browser worker packages remain intact.
      Captured-project application builds preserve all outputs and use the same
      capacity preflight. Zero explicitly opts out of that check. }
    procedure ConfigureProjectionStorage(const AValue: TNyxProjectionStoragePolicy);
    function Build(ADocument: TNyxDocument;
      const ATarget, AScope, APage, ACompanion: TNyxText;
      const ACancellation: INyxBuildCancellation = nil): TJSONObject; overload;
    { Trusted captured admission, never a public wire option. The immutable
      checkpoint must match the complete design/source exactly. Executed units
      retain their original bytes; focused views isolate their constructed tree
      at runtime instead of replacing arbitrary handwritten builder statements.
      An empty checkpoint retains strict declarative companion admission. }
    function Build(ADocument: TNyxDocument;
      const ATarget, AScope, APage, ACompanion: TNyxText;
      const ACheckpoint: TNyxSourceCheckpoint;
      const ACancellation: INyxBuildCancellation = nil): TJSONObject; overload;
    { Explicit execution of a complete trusted Pascal constructor. Unlike Build,
      this does not reconstruct a fluent subset before invoking the compiler.
      Each compiler/child owns a separately bounded process family. Native returns
      an admitted detached design; browser returns only a compiled worker receipt.
      No editor pair, source workspace, output selection or enrollment is changed.
      Invalid source/unit/policy arguments raise before allocation; unavailable
      tools, compiler/execution failures and cancellation return unusable results.
      Native model evaluation needs FPC, not a Lazarus widgetset or display. }
    function ProjectSource(const ASource: TNyxText; const AUnit: TNyxPascalUnitRef;
      ATarget: TNyxBuildTarget; AChecks: TNyxSourceProjectionChecks = spcDefault;
      const ACancellation: INyxBuildCancellation = nil): INyxSourceProjectionBuild; overload;
    { Capture every unit/include before delegation and retain the exact frozen
      inventory with the ordinary actual compiler/construction result. Native
      can construct; browser compilation alone still grants no design. Existing
      project/recovery/MCP envelopes do not accept this local-only attachment. }
    function ProjectSource(const AProject: INyxPascalProject;
      ATarget: TNyxBuildTarget; AChecks: TNyxSourceProjectionChecks = spcDefault;
      const ACancellation: INyxBuildCancellation = nil): INyxPascalProjectBuild; overload;
    { Compile only the exact input snapshot retained by this executor's actual
      executed project receipt. No caller can substitute new helper files beside
      an older main-source/design pair. Fresh application storage preserves/pins
      source inputs through compiler join; focus constructs once before cloning.
      This changes no editor source/history, output selection or enrollment. }
    function BuildProject(const AProject: INyxPascalProjectBuild;
      ATarget: TNyxBuildTarget; AScope: TNyxBuildScope;
      const AView: TNyxText = '';
      const ACancellation: INyxBuildCancellation = nil): TJSONObject;
  end;

{ Returns a fresh reference-counted cancellation lifetime, initially uncancelled.
  It owns only its native event, never the caller's job or editor. }
function NewNyxBuildCancellation: INyxBuildCancellation;

implementation

uses
  nyx.json, nyx.bytes, nyx.schema, nyx.codec, nyx.codegen, nyx.callbacks,
  nyx.scheduler, nyx.composition, nyx.editing,
  nyx.studio.compilerprocess
  {$ifdef MSWINDOWS}, nyx.studio.compilerinputs{$endif};

type
  { A native cancellation event owns no editor/worker and can be retained by
    detached jobs until their real process and source-input ownership retires. }
  TNyxBuildCancellation = class(TInterfacedObject, INyxBuildCancellation)
  private
    FCancelled: TEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cancel;
    function Cancelled: Boolean;
    procedure Check;
  end;

{$I nyx.studio.buildexecutor.project.inc}

constructor TNyxBuildCancellation.Create;
begin
  inherited Create;
  FCancelled := TEvent.Create(nil, True, False, '');
end;

destructor TNyxBuildCancellation.Destroy;
begin
  FCancelled.Free;
  inherited Destroy;
end;

procedure TNyxBuildCancellation.Cancel;
begin
  FCancelled.SetEvent;
end;

function TNyxBuildCancellation.Cancelled: Boolean;
begin
  Result := FCancelled.WaitFor(0) = wrSignaled;
end;

procedure TNyxBuildCancellation.Check;
begin

  if Cancelled then
  begin
    raise ENyxBuildCancelled.Create('Compiler cancellation requested');
  end;
end;

function NewNyxBuildCancellation: INyxBuildCancellation;
begin
  Result := TNyxBuildCancellation.Create;
end;

class function TNyxCompilerLimits.Default: TNyxCompilerLimits;
begin
  Result.FCompilationMilliseconds := 5 * 60 * 1000;
  Result.FExecutionMilliseconds := 60 * 1000;
  Result.FLogBytes := 1024 * 1024;
end;

function TNyxCompilerLimits.TimeMilliseconds(AValue: Cardinal): TNyxCompilerLimits;
begin
  Result := Self;
  Result.FCompilationMilliseconds := AValue;
  Result.FExecutionMilliseconds := AValue;
  Result.Validate;
end;

function TNyxCompilerLimits.CompilationMilliseconds(AValue: Cardinal): TNyxCompilerLimits;
begin
  Result := Self;
  Result.FCompilationMilliseconds := AValue;
  Result.Validate;
end;

function TNyxCompilerLimits.ExecutionMilliseconds(AValue: Cardinal): TNyxCompilerLimits;
begin
  Result := Self;
  Result.FExecutionMilliseconds := AValue;
  Result.Validate;
end;

function TNyxCompilerLimits.LogBytes(AValue: Integer): TNyxCompilerLimits;
begin
  Result := Self;
  Result.FLogBytes := AValue;
  Result.Validate;
end;

procedure TNyxCompilerLimits.Validate;
begin

  if (FCompilationMilliseconds = 0) or (FExecutionMilliseconds = 0) or
    (FLogBytes < 1) or (FLogBytes > 1024 * 1024) then
  begin
    raise ENyxModel.Create('Compilation/execution budgets require positive milliseconds and 1..1048576 log bytes');
  end;
end;

procedure TNyxBuildExecutor.ConfigureLimits(const ALimits: TNyxCompilerLimits);
begin
  ALimits.Validate;
  FLimits := ALimits;
end;

procedure TNyxBuildExecutor.ConfigureProgress(const AProgress: INyxCompilerProgress);
begin
  FProgress := AProgress;
end;

procedure TNyxBuildExecutor.ConfigureProjectionStorage(
  const AValue: TNyxProjectionStoragePolicy);
begin
  AValue.Validate;
  FProjectionStorage := AValue;
end;

function ReadFile(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, LStream.Size);
    SetCodePage(RawByteString(Result), CP_UTF8, False);

    if LStream.Size > 0 then
    begin
      LStream.ReadBuffer(Result[1], LStream.Size);
    end;
  finally
    LStream.Free;
  end;
end;

procedure WriteFile(const APath, AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

function HostHTML(const AProgram: TNyxText): TNyxText;
begin
  { Runtime and compiled Pascal are served from the same admitted artifact root.
    The tiny bootstrap is the only handwritten target glue: all product behavior
    is compiled Pascal. Relative URLs work under both / and /builds/job-N/. }
  Result := '<!doctype html><html lang="en"><head><meta charset="utf-8">' +
    '<meta name="viewport" content="width=device-width,initial-scale=1">' +
    '<title>Nyx</title></head><body style="margin:0">' +
    '<script src="rtl.js"></script><script src="' + AProgram + '.js"></script>' +
    '<script>rtl.run();</script></body></html>';
end;

constructor TNyxBuildExecutor.Create(const ARepository, AProfile: TNyxText);
begin
  Create(TNyxStudioDirectories.ForRepository(ARepository), AProfile);
end;

constructor TNyxBuildExecutor.Create(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText);
begin
  inherited Create;
  ADirectories.Validate;
  FDirectories := ADirectories;
  FJobRoot := FDirectories.Jobs;
  FOutputs := TNyxOutputConfiguration.Decode(AProfile);
  FLimits := TNyxCompilerLimits.Default;
  FProjectionStorage := TNyxProjectionStoragePolicy.Default;
end;

destructor TNyxBuildExecutor.Destroy;
begin
  FOutputs.Free;
  inherited Destroy;
end;

function TNyxBuildExecutor.Readiness(const ATarget: TNyxText): TNyxText;
begin
  Result := '';
  try
    CheckOutput(ATarget);
  except
    on LException: ENyxModel do
    begin
      Result := LException.Message;
    end;
  end;
end;

function IsOutputIdentifier(const AText: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := (AText <> '') and (Length(AText) <= 80);
  for LIndex := 1 to Length(AText) do
  begin

    if not (AText[LIndex] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) then
    begin
      Exit(False);
    end;
  end;
end;

procedure TNyxBuildExecutor.CheckOutput(const ATarget: TNyxText);
var
  LRoot: TNyxText;
  LPlatform: TNyxText;
  LWidgetset: TNyxText;
begin
  ValidateNyxOutputTarget(ATarget);

  if ATarget = '' then
  begin
    raise ENyxModel.Create('Choose an output in Studio Target / output before building');
  end;

  if ATarget = 'browser' then
  begin

    if not FileExists(FOutputs.Field('pas2js')) then
    begin
      raise ENyxModel.Create('Browser output: configure the pas2js compiler in Target / output');
    end;

    if not FileExists(FOutputs.Field('runtime')) then
    begin
      raise ENyxModel.Create('Browser output: configure matching rtl.js in Target / output');
    end;
  end
  else
  begin

    if not FileExists(FOutputs.Field('fpc')) then
    begin
      raise ENyxModel.Create('Native LCL output: configure the FPC compiler in Target / output');
    end;
    LRoot := IncludeTrailingPathDelimiter(FOutputs.Field('lazarus'));
    LPlatform := FOutputs.Field('platform');
    LWidgetset := FOutputs.Field('widgetset');

    if not DirectoryExists(LRoot + 'lcl') then
    begin
      raise ENyxModel.Create('Native LCL output: configure the Lazarus root in Target / output');
    end;

    if not IsOutputIdentifier(LPlatform) or not IsOutputIdentifier(LWidgetset) then
    begin
      raise ENyxModel.Create('Native LCL output: configure platform and widgetset in Target / output');
    end;

    if FOutputs.HTTP and (LPlatform <> 'i386-win32') and (LPlatform <> 'x86_64-win64') then
    begin
      raise ENyxModel.Create('Native HTTP output: the built-in transport requires Windows; ' +
        'disable HTTP in Outputs or supply a supported platform adapter');
    end;

    if not DirectoryExists(LRoot + 'lcl/units/' + LPlatform + '/' + LWidgetset) then
    begin
      raise ENyxModel.Create('Native LCL output: matching Lazarus platform/widgetset units are missing');
    end;
  end;
end;

function TNyxBuildExecutor.RunCompiler(const AExecutable, ADirectory: TNyxText;
  AArguments: TStrings; out ALog: TNyxText;
  const ACancellation: INyxBuildCancellation; ACompiler: Boolean;
  const AReader: INyxCompilerReportReader): Boolean;
var
  LProcess: TNyxCompilerProcess;
  LProgress: INyxCompilerProgress;
  {$ifdef MSWINDOWS}
  LInputs: TNyxCompilerInputs;
  {$endif}
  LBuffer: array[0..4095] of Byte;
  LRead: Integer;
  LChunk: TNyxText;
  LStarted: QWord;
  LTimeLimit: Cardinal;

  function LogPrefix: TNyxText;
  var
    LIndex: Integer;
    LEnd: Integer;
    LScalar: Integer;
  begin
    { The byte budget may land inside a UTF-8 scalar. Keep only complete
      scalars at this diagnostic boundary; never cut a supplementary character
      into malformed text. Invalid compiler bytes end the readable prefix. }
    LIndex := 1;
    LEnd := 0;
    while LIndex <= FLimits.FLogBytes do
    begin

      if not NyxNextScalar(ALog, LIndex, LScalar) or
        (LIndex > FLimits.FLogBytes + 1) then
      begin
        Break;
      end;
      LEnd := LIndex - 1;
    end;
    Result := Copy(ALog, 1, LEnd);
  end;

  procedure CheckRequest;
  begin

    if ACancellation <> nil then
    begin
      ACancellation.Check;
    end;
  end;

  procedure CheckCancellation;
  begin
    CheckRequest;
    {$ifdef MSWINDOWS}

    if LInputs <> nil then
    begin
      LInputs.Check;
    end;
    {$endif}
  end;

  function InTime: Boolean;
  begin
    Result := GetTickCount64 - LStarted < LTimeLimit;

    if not Result then
    begin
      FFailure := bcfTimeBudget;

      if ACompiler then
      begin
        ALog := ALog + #10 + 'Compiler time budget exceeded (' + UIntToStr(QWord(LTimeLimit)) + ' ms).';
      end
      else
      begin
        ALog := ALog + #10 + 'Constructor execution time budget exceeded (' + UIntToStr(QWord(LTimeLimit)) + ' ms).';
      end;
    end;
  end;
begin
  LProcess := nil;
  LProgress := FProgress;
  {$ifdef MSWINDOWS}
  LInputs := nil;
  {$endif}
  CheckRequest;
  FLimits.Validate;
  LTimeLimit := FLimits.CompilationTimeMilliseconds;

  if not ACompiler then
  begin
    LTimeLimit := FLimits.ExecutionTimeMilliseconds;
  end;

  if (AExecutable = '') or not FileExists(AExecutable) then
  begin
    ALog := 'Compiler executable is missing; configure NYX_PAS2JS or NYX_FPC.';
    Exit(False);
  end;
  ALog := '';
  try
    try
      {$ifdef MSWINDOWS}

      if ACompiler then
      begin
        LInputs := TNyxCompilerInputs.Create(FDirectories.SourceRoot, CheckRequest);
      end;
      {$endif}
      LProcess := TNyxCompilerProcess.Create(nil);
      LProcess.Executable := AExecutable;
      LProcess.CurrentDirectory := ADirectory;
      LProcess.Parameters.Assign(AArguments);
      LProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
      LProcess.Execute;
      LStarted := GetTickCount64;
      repeat
        { Drain pipes while the compiler runs. Waiting for exit before reading can
          deadlock a compiler that fills its pipe with warnings/diagnostics. }
        while LProcess.Output.NumBytesAvailable > 0 do
        begin
          { Check within the drain loop: a continuously writing compiler must not
            starve cancellation or the deadline by keeping its pipe nonempty. }
          CheckCancellation;

          if not InTime then
          begin
            Exit(False);
          end;
          LRead := LProcess.Output.Read(LBuffer, SizeOf(LBuffer));
          SetLength(LChunk, LRead);

          if LRead > 0 then
          begin
            Move(LBuffer[0], LChunk[1], LRead);
            ALog := ALog + LChunk;
          end;

          if Length(ALog) > FLimits.FLogBytes then
          begin
            FFailure := bcfLogBudget;
            ALog := LogPrefix + #10 + 'Compiler log budget exceeded.';
            Exit(False);
          end;

          if (LProgress <> nil) and (AReader <> nil) and AReader.Append(LChunk) then
          begin
            CheckCancellation;
            LProgress.Publish(AReader.Snapshot);
          end;
        end;

        CheckCancellation;

        if not InTime then
        begin
          Exit(False);
        end;

        if not LProcess.Running and (LProcess.ExitStatus <> 0) then
        begin
          { Preserve the compiler's own failure instead of waiting for a stranded
            helper to turn it into a deadline failure. Finally retires the family. }
          FFailure := bcfCompiler;
          Exit(False);
        end;

        if LProcess.FamilyRunning then
        begin
          Sleep(10);
        end;
      until not LProcess.FamilyRunning and (LProcess.Output.NumBytesAvailable = 0);
      { Check after the last owned child exits, before admitting any artifact.
        The lease stays live through the finally block's explicit join. }
      CheckCancellation;
      { A final unterminated line is complete only after real compiler EOF.
        Cancellation and budget exits deliberately do not seal a partial line. }

      if (LProgress <> nil) and (AReader <> nil) and AReader.Finish then
      begin
        LProgress.Publish(AReader.Snapshot);
      end;
      Result := LProcess.ExitStatus = 0;

      if not Result then
      begin
        FFailure := bcfCompiler;
      end;
    {$ifdef MSWINDOWS}
    except
      on LException: ENyxCompilerInputs do
      begin
        FFailure := bcfCompiler;
        ALog := ALog + #10 + LException.Message;
        Result := False;
      end;
    {$else}
    finally
      { Other native hosts retain their existing source borrowing policy. }
    {$endif}
    end;
  finally
    { Every exit (including failed suspended admission, cancellation, read
      failure, deadline/log failure) retires the invocation's process family
      and joins its exact compiler. A failed OS retirement keeps the worker
      active; never release its slot, advertise an artifact or orphan a child.
      No editor lock is held. The budget bounds compiler execution, not an OS
      that cannot reap its process. Normal Windows retirement is qualified. }

    if LProcess <> nil then
    begin
      LProcess.RetireAndJoin;
      LProcess.Free;
    end;
    {$ifdef MSWINDOWS}
    LInputs.Free;
    {$endif}
  end;
end;

function TNyxBuildExecutor.ProjectSource(const ASource: TNyxText;
  const AUnit: TNyxPascalUnitRef; ATarget: TNyxBuildTarget;
  AChecks: TNyxSourceProjectionChecks;
  const ACancellation: INyxBuildCancellation): INyxSourceProjectionBuild;
begin
  Result := ProjectSourceInputs(ASource, AUnit, ATarget, AChecks, ACancellation, nil);
end;

function TNyxBuildExecutor.ProjectSource(const AProject: INyxPascalProject;
  ATarget: TNyxBuildTarget; AChecks: TNyxSourceProjectionChecks;
  const ACancellation: INyxBuildCancellation): INyxPascalProjectBuild;
var
  LInputs: INyxPascalProject;
  LBuild: INyxSourceProjectionBuild;
begin
  LInputs := CaptureNyxPascalProject(AProject);
  LBuild := ProjectSourceInputs(LInputs.Source, LInputs.MainUnit, ATarget,
    AChecks, ACancellation, LInputs);
  Result := NewNyxPascalProjectBuild(LInputs, LBuild);
end;

function TNyxBuildExecutor.ProjectSourceInputs(const ASource: TNyxText;
  const AUnit: TNyxPascalUnitRef; ATarget: TNyxBuildTarget;
  AChecks: TNyxSourceProjectionChecks; const ACancellation: INyxBuildCancellation;
  const AInputs: INyxPascalProject): INyxSourceProjectionBuild;
var
  LID: TGUID;
  LReference: TNyxSourceProjectionRef;
  LDirectory: TNyxText;
  LFileName: TNyxText;
  LExecutable: TNyxText;
  LLog: TNyxText;
  LRuntimeLog: TNyxText;
  LArtifact: TNyxText;
  LArguments: TStringList;
  LBytes: TNyxBytes;
  LReport: INyxCompilerReport;
  LProjection: INyxSourceProjection;
  LReader: INyxCompilerReportReader;
  LFailureState: TNyxSourceProjectionState;
  LStorage: TNyxOwnedProjectionStorage;
  LStorageReport: TNyxProjectionStorageReport;
  LCompiled: Boolean;
  LInputIndex: Integer;
begin
  ValidateNyxProjectionSource(ASource);
  ValidateNyxProjectionTarget(ATarget);
  TNyxCodegen.AdmitUnitName(AUnit.Name);
  FProjectionStorage.Validate;

  if (Ord(AChecks) < Ord(Low(TNyxSourceProjectionChecks))) or
    (Ord(AChecks) > Ord(High(TNyxSourceProjectionChecks))) then
  begin
    raise ENyxModel.Create('Unknown source projection check policy');
  end;
  CreateGUID(LID);
  LReference := NyxSourceProjectionRef('job-' + Copy(GUIDToString(LID), 2, 36));
  LArguments := TStringList.Create;
  LStorage := nil;
  LStorageReport := TNyxProjectionStorageReport.Unallocated;
  LFailureState := spsCompilationFailed;
  LArtifact := '';
  LRuntimeLog := '';
  LLog := '';
  FFailure := bcfNone;
  try
    try

      if ACancellation <> nil then
      begin
        ACancellation.Check;
      end;
      LExecutable := FOutputs.Field('fpc');

      if ATarget = btBrowser then
      begin
        LExecutable := FOutputs.Field('pas2js');
      end;

      if not FileExists(LExecutable) or
        ((ATarget = btBrowser) and not FileExists(FOutputs.Field('runtime'))) then
      begin
        LProjection := NyxSourceProjectionFailure(ASource, ATarget, spsUnavailable,
          'Configure the requested compiler and matching browser runtime before evaluation');
      end
      else
      begin
        LStorage := TNyxOwnedProjectionStorage.Create(FJobRoot, LReference,
          AUnit.Name, ATarget, FProjectionStorage, LStorageReport);
        LDirectory := LStorage.Directory;
        LFileName := LDirectory + AUnit.Name + '.pas';
        { Copy exact source bytes. In particular, do not run PrepareNyxCompanion,
          a fluent expression reader or source workspace admission beforehand. }
        LStorage.WriteEvidence(AUnit.Name + '.pas', ASource);
        if AInputs <> nil then
        begin
          for LInputIndex := 0 to AInputs.Count - 1 do
          begin
            LStorage.WriteProjectInput(AInputs.Input(LInputIndex));
          end;
        end;
        LStorage.WriteEvidence('nyx_projection.lpr',
          GenerateNyxSourceProjectionProgram(AUnit, LReference, ATarget));
        LArguments.Add('-Mdelphi');
        LArguments.Add('-B');
        LArguments.Add('-vb');
        LArguments.Add('-Fu' + FDirectories.CompilerUnits);
        LArguments.Add('-Fu' + FDirectories.SourceRoot + PathDelim + 'studio');
        LArguments.Add('-Fu' + LDirectory);
        LArguments.Add('-FE' + LDirectory);

        if ATarget = btBrowser then
        begin
          { A Pascal worker program includes its matched RTL and rtl.run entry.
            No editor DOM, handwritten JavaScript evaluator or framework is used. }
          LArguments.Add('-Tmodule');
          LArguments.Add('-Jirtl.js');
        end
        else
        begin
          LArguments.Add('-FU' + LDirectory + 'units');

          if AChecks = spcChecked then
          begin
            LArguments.Add('-Sa');
            LArguments.Add('-Cr');
            LArguments.Add('-Co');
            LArguments.Add('-Ci');
            LArguments.Add('-gl');
            LArguments.Add('-gh');
          end;
        end;
        if (AInputs <> nil) and (AInputs.Count > 0) then
        begin
          LArguments.Add('-Fu' + LDirectory + 'project');
          LArguments.Add('-Fi' + LDirectory + 'project');
        end;
        LArguments.Add(LDirectory + 'nyx_projection.lpr');

        try

          if FProgress <> nil then
          begin
            LReader := NewNyxProjectCompilerReportReader(ASource, ASource, LFileName,
              LDirectory + 'project', AInputs);
          end;
          LCompiled := RunCompiler(LExecutable, LDirectory, LArguments, LLog,
            ACancellation, True, LReader);
        finally
          { RunCompiler's finally joins even on cancellation/failure. Capture
            now, before trusted constructor code can alter any compiler output. }
          LStorage.CaptureCompilerDerivatives;
        end;

        if not LCompiled then
        begin
          LReport := ReadNyxProjectCompilerReport(ASource, ASource, LFileName,
            LDirectory + 'project', LLog, AInputs);
          LProjection := NyxSourceProjectionFailure(ASource, ATarget,
            spsCompilationFailed, 'Source projection compilation failed', LReport);
        end
        else
        begin
          LReport := ReadNyxProjectCompilerReport(ASource, ASource, LFileName,
            LDirectory + 'project', LLog, AInputs);

          if ACancellation <> nil then
          begin
            ACancellation.Check;
          end;

          if ATarget = btBrowser then
          begin
            LArtifact := 'builds/' + LReference.Name + '/nyx_projection.js';
            LProjection := NyxSourceProjectionFailure(ASource, ATarget, spsCompiled,
              'Execute the owned browser worker before admitting a design', LReport);
          end
          else
          begin
            LFailureState := spsExecutionFailed;
            LArguments.Clear;
            LExecutable := LDirectory + 'nyx_projection';
            {$IFDEF MSWINDOWS}
            LExecutable := LExecutable + '.exe';
            {$ENDIF}
            { The same process-family owner/log ceiling applies to execution,
              with its independently captured execution time budget. Compiler
              diagnostics stay in their own report. }

            if not RunCompiler(LExecutable, LDirectory, LArguments, LRuntimeLog,
              ACancellation, False) then
            begin
              LProjection := NyxSourceProjectionFailure(ASource, ATarget,
                spsExecutionFailed, 'Source projection execution failed', LReport);
            end
            else
            begin
              LFailureState := spsInvalidDesign;
              { Bound byte allocation and reject links before reading/decoding;
                stdout cannot masquerade as an owned model reply. }
              LBytes := LStorage.ReadResult;

              if ACancellation <> nil then
              begin
                ACancellation.Check;
              end;
              LProjection := ReceiveNyxSourceProjection(ASource, LReference, ATarget,
                NyxDecodeUTF8(LBytes), LReport);
            end;
          end;
        end;
      end;
    except
      on LException: ENyxProjectionCapacity do
      begin
        LProjection := NyxSourceProjectionFailure(ASource, ATarget, spsUnavailable,
          LStorageReport.Message);
      end;
      on LException: ENyxBuildCancelled do
      begin
        LArtifact := '';

        if (LReport = nil) and (LLog <> '') then
        begin
          LReport := ReadNyxProjectCompilerReport(ASource, ASource, LFileName,
            LDirectory + 'project', LLog, AInputs);
        end;
        LProjection := NyxSourceProjectionFailure(ASource, ATarget, spsCancelled,
          'Source projection was cancelled', LReport);
      end;
      on LException: Exception do
      begin
        LArtifact := '';

        if LStorage = nil then
        begin
          LProjection := NyxSourceProjectionFailure(ASource, ATarget, spsUnavailable,
            'Cannot establish owned build storage on this host');
        end
        else
        begin
          LProjection := NyxSourceProjectionFailure(ASource, ATarget, LFailureState,
            'Source projection could not complete its owned stage', LReport);
        end;
      end;
    end;

    if LStorage <> nil then
    begin
      { Diagnostics also survive failure/cancellation. Evidence writes refuse
        existing constructor files; refusal never changes admitted model meaning.
        No process remains when RunCompiler has returned or raised to this scope. }
      try
        LStorage.WriteEvidence('compiler.log', LLog);
        LStorage.WriteEvidence('execution.log', LRuntimeLog);
      except
        on LException: Exception do
        begin
          { Storage records the evidence refusal separately from compilation. }
        end;
      end;
      LStorage.FinishAfterJoin;
      LStorageReport := LStorage.Snapshot;
    end;
    Result := WithNyxProjectionStorage(NewNyxSourceProjectionBuild(LReference,
      LProjection, LArtifact, LRuntimeLog), LStorageReport);
  finally
    LStorage.Free;
    LArguments.Free;
  end;
end;

function TNyxBuildExecutor.Build(ADocument: TNyxDocument;
  const ATarget, AScope, APage, ACompanion: TNyxText;
  const ACancellation: INyxBuildCancellation): TJSONObject;
begin
  Result := Build(ADocument, ATarget, AScope, APage, ACompanion,
    Default(TNyxSourceCheckpoint), ACancellation);
end;

function TNyxBuildExecutor.Build(ADocument: TNyxDocument;
  const ATarget, AScope, APage, ACompanion: TNyxText;
  const ACheckpoint: TNyxSourceCheckpoint;
  const ACancellation: INyxBuildCancellation): TJSONObject;
begin
  Result := BuildCaptured(ADocument, ATarget, AScope, APage, ACompanion,
    ACheckpoint, ACancellation, nil);
end;

function TNyxBuildExecutor.BuildProject(const AProject: INyxPascalProjectBuild;
  ATarget: TNyxBuildTarget; AScope: TNyxBuildScope; const AView: TNyxText;
  const ACancellation: INyxBuildCancellation): TJSONObject;
var
  LCaptured: INyxCapturedProjectBuild;
  LDocument: TNyxDocument;
  LBuild: INyxSourceProjectionBuild;
  LScope: TNyxText;
begin
  ValidateNyxProjectionTarget(ATarget);

  if (Ord(AScope) < Ord(Low(TNyxBuildScope))) or
    (Ord(AScope) > Ord(High(TNyxBuildScope))) then
  begin
    raise ENyxModel.Create('Unknown captured project build scope');
  end;

  if (AProject = nil) or not Supports(AProject, INyxCapturedProjectBuild, LCaptured) then
  begin
    raise ENyxModel.Create('Application compilation requires a local captured project receipt');
  end;
  LBuild := LCaptured.GetCapturedBuild;

  if LBuild.Projection.State <> spsExecuted then
  begin
    raise ENyxModel.Create('Construct the complete captured project before application compilation');
  end;
  LDocument := LBuild.Projection.CopyDocument;
  try
    LScope := 'view';

    if AScope = bsApplication then
    begin

      if AView <> '' then
      begin
        raise ENyxModel.Create('A complete project application has no focused root');
      end;
      LScope := 'application';
    end
    else if (AScope = bsReusable) and (LDocument.FindComponent(AView) = nil) then
    begin
      raise ENyxModel.Create('A reusable project build requires an exact reusable root');
    end;
    Result := BuildCaptured(LDocument, NyxBuildTargetName(ATarget),
      LScope, AView, LCaptured.GetInputs.Source,
      Default(TNyxSourceCheckpoint), ACancellation, AProject);
  finally
    LDocument.Free;
  end;
end;

function TNyxBuildExecutor.BuildCaptured(ADocument: TNyxDocument;
  const ATarget, AScope, APage, ACompanion: TNyxText;
  const ACheckpoint: TNyxSourceCheckpoint;
  const ACancellation: INyxBuildCancellation;
  const AProject: INyxPascalProjectBuild): TJSONObject;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LIndex: Integer;
  LJob: TNyxText;
  LDirectory: TNyxText;
  LArguments: TStringList;
  LSource: TNyxText;
  LUnitName: TNyxText;
  LCompanionSource: TNyxText;
  LLog: TNyxText;
  LExecutable: TNyxText;
  LRuntime: TNyxText;
  LLazarus: TNyxText;
  LPlatform: TNyxText;
  LWidgetset: TNyxText;
  LOK: Boolean;
  LReport: INyxCompilerReport;
  LReader: INyxCompilerReportReader;
  LSubmittedSource: TNyxText;
  LExecuted: Boolean;
  LScopeDeclarations: TNyxText;
  LScopeConstruction: TNyxText;
  LFileUnits: TNyxText;
  LFileSetup: TNyxText;
  LFileDeclarations: TNyxText;
  LClipboardSource: TNyxText;
  LViewSetup: TNyxText;
  LViewIndent: TNyxText;
  LID: TGUID;
  LCaptured: INyxCapturedProjectBuild;
  LProjectInputs: INyxPascalProject;
  LProjectBuild: INyxSourceProjectionBuild;
  LProjectStorage: TNyxOwnedProjectionStorage;
  LStorageReport: TNyxProjectionStorageReport;
  LEntry: TNyxOwnedCompilerEntry;
  LIncludes: TNyxStrings;

  procedure AdmitBrowserPolicies(ANode: TNyxNode);
  var
    LEvents: TNyxAuthoredEventInfos;
    LEventIndex: Integer;
    LChildIndex: Integer;
  begin
    LEvents := NyxAuthoredEvents(ANode);
    for LEventIndex := 0 to High(LEvents) do
    begin

      if LEvents[LEventIndex].Policy = neThreaded then
      begin
        raise ENyxModel.Create('Browser output cannot execute threaded callbacks on ' +
          ANode.ID + '; choose asynchronous or a native output');
      end;
    end;
    for LChildIndex := 0 to ANode.Count - 1 do
    begin
      AdmitBrowserPolicies(ANode.Children[LChildIndex]);
    end;
  end;

begin
  FFailure := bcfNone;
  LProjectStorage := nil;
  LIncludes := nil;

  if ACancellation <> nil then
  begin
    ACancellation.Check;
  end;

  ValidateNyxOutputTarget(ATarget);

  if (AScope <> 'view') and (AScope <> 'application') then
  begin
    raise ENyxModel.Create('Scope must be view or application');
  end;
  ValidateNyxDocumentProperties(ADocument);
  LExecuted := False;

  if AProject <> nil then
  begin

    if not Supports(AProject, INyxCapturedProjectBuild, LCaptured) then
    begin
      raise ENyxModel.Create('A project build must retain its local constructor input receipt');
    end;
    LProjectInputs := LCaptured.GetInputs;
    LProjectBuild := LCaptured.GetCapturedBuild;

    if (LProjectBuild.Projection.State <> spsExecuted) or
      (LProjectBuild.Projection.Source <> ACompanion) or
      (LProjectInputs.Source <> ACompanion) or
      (LProjectBuild.Projection.Design <> TNyxCodec.Encode(ADocument)) then
    begin
      raise ENyxModel.Create('Captured project input/design meaning differs from its construction');
    end;
    LExecuted := True;
  end
  else if (ACheckpoint.Design <> '') or (ACheckpoint.Source <> '') then
  begin

    if (ACheckpoint.Design <> TNyxCodec.Encode(ADocument)) or
      (ACheckpoint.Source <> ACompanion) or (ACompanion = '') then
    begin
      raise ENyxModel.Create('Application build differs from its captured accepted source checkpoint');
    end;
    LExecuted := ACheckpoint.Origin = nsoExecuted;

    if not ACheckpoint.Files.IsEmpty then
    begin

      if not LExecuted then
      begin
        raise ENyxModel.Create('Supporting application files require an admitted executed checkpoint');
      end;
      LProjectInputs := ACheckpoint.Files.Open(ACompanion);
    end;
  end;
  CheckOutput(ATarget);
  LDocument := nil;
  LArguments := TStringList.Create;
  try

    if AScope = 'view' then
    begin
      LRoot := ADocument.Find(APage);

      if LRoot = nil then
      begin
        raise ENyxModel.Create('Requested view is missing');
      end;
      { Focused contexts preserve exact identity and named-declaration closure.
        The requested node may remain reusable or nested under a retained owner;
        the harness selects it explicitly without a duplicate/renamed root. }
      LDocument := CloneNyxViewDocument(ADocument, LRoot);
    end
    else
    begin
      { Use the complete ownership contract. Copying trees/state manually lost
        collection defaults and produced a saved job design differing from its
        companion. Clone carries every admitted document feature independently. }
      LDocument := ADocument.Clone;
    end;

    if (AScope = 'application') and (LDocument.Count = 0) then
    begin
      raise ENyxModel.Create('Add a page before building an application');
    end;

    if ATarget = 'browser' then
    begin
      for LIndex := 0 to LDocument.Count - 1 do
      begin
        AdmitBrowserPolicies(LDocument.Pages[LIndex]);
      end;
      for LIndex := 0 to LDocument.ComponentCount - 1 do
      begin
        AdmitBrowserPolicies(LDocument.Components[LIndex]);
      end;
    end;
    { Companion admission precedes job allocation and compilation. Application
      builds retain the exact accepted source; view builds replace only the
      managed builder and preserve real imports, helpers and handwritten code. }
    LUnitName := 'nyx.generated.view';
    LCompanionSource := TNyxCodegen.Generate(LDocument);

    if ACompanion <> '' then
    begin
      if LProjectInputs <> nil then
      begin
        LIncludes := TNyxStrings.Create;
        for LIndex := 0 to LProjectInputs.Count - 1 do
        begin

          if LProjectInputs.Input(LIndex).Kind = npiInclude then
          begin
            LIncludes.Add(LProjectInputs.Input(LIndex).Name);
          end;
        end;
        LUnitName := NyxCompanionUnitName(ACompanion, LIncludes);
      end
      else
      begin
        LUnitName := NyxCompanionUnitName(ACompanion);
      end;

      if LExecuted then
      begin
        LCompanionSource := ACompanion;
      end
      else
      begin
        LCompanionSource := PrepareNyxCompanion(ADocument, LDocument, ACompanion,
          AScope = 'view');
      end;
    end;
    LScopeDeclarations := '';
    LScopeConstruction := '';
    LViewSetup := '';

    if AScope = 'view' then
    begin

      if ATarget = 'browser' then
      begin
        LViewIndent := '  ';
      end
      else
      begin
        LViewIndent := '    ';
      end;
      LViewSetup := LViewIndent + 'LApplication.ConfigureView(TNyxApplicationView.Focused(' + #10 +
        LViewIndent + '  NyxControl(' + TNyxCodegen.TextLiteral(APage) + ')));' + #10;
    end;

    if LExecuted and (AScope = 'view') then
    begin
      { Preserve every original expression/helper and construct once. Clone
        transfers only this requested root's reachable definitions/defaults to
        an independent preview owner, then releases the full construction. }
      LScopeDeclarations := '  LConstruction: TNyxDocument;' + #10 +
        '  LView: TNyxNode;' + #10;
      LScopeConstruction := '  LConstruction := LDocument;' + #10 +
        '  LDocument := nil;' + #10 +
        '  try' + #10 +
        '    LView := LConstruction.Find(' + TNyxCodegen.TextLiteral(APage) + ');' + #10#10 +
        '    if LView = nil then' + #10 +
        '    begin' + #10 +
        '      raise ENyxModel.Create(''Requested compiled view is missing'');' + #10 +
        '    end;' + #10 +
        '    LDocument := CloneNyxViewDocument(LConstruction, LView);' + #10 +
        '  finally' + #10 +
        '    LConstruction.Free;' + #10 +
        '  end;' + #10;
    end;
    LSubmittedSource := ACompanion;

    if LSubmittedSource = '' then
    begin
      LSubmittedSource := LCompanionSource;
    end;
    CreateGUID(LID);
    { A per-invocation identity also separates concurrent HTTP/MCP workers. }
    LJob := 'job-' + Copy(GUIDToString(LID), 2, 36);
    LDirectory := FJobRoot + LJob + PathDelim;

    if LProjectInputs <> nil then
    begin
      LEntry := oceNativeApplication;

      if ATarget = 'browser' then
      begin
        LEntry := oceBrowserApplication;
      end;
      LProjectStorage := TNyxOwnedProjectionStorage.Create(FJobRoot,
        NyxSourceProjectionRef(LJob), LUnitName, ParseNyxBuildTarget(ATarget),
        FProjectionStorage.Retaining(prAllFiles), LStorageReport, LEntry);
      LDirectory := LProjectStorage.Directory;
      LProjectStorage.WriteEvidence(LUnitName + '.pas', LCompanionSource);
      for LIndex := 0 to LProjectInputs.Count - 1 do
      begin
        LProjectStorage.WriteProjectInput(LProjectInputs.Input(LIndex));
      end;
    end
    else
    begin

      if not ForceDirectories(LDirectory + 'units') then
      begin
        raise ENyxModel.Create('Cannot prepare the admitted compiler job directory');
      end;
      WriteFile(LDirectory + LUnitName + '.pas', LCompanionSource);
    end;
    WriteFile(LDirectory + 'design.nyx', TNyxCodec.Encode(LDocument));
    LArguments.Add('-Mdelphi');
    LArguments.Add('-B');
    { Both providers support full diagnostic paths. Bare filenames cannot
      establish that an error belongs to this admitted companion rather than
      a same-named dependency, so navigation requires this fixed option. }
    LArguments.Add('-vb');
    LArguments.Add('-Fu' + FDirectories.CompilerUnits);
    { Studio instrumentation is optional private launch behavior. Exported
      companion source and the portable library retain no reporting context. }
    LArguments.Add('-Fu' + FDirectories.SourceRoot + PathDelim + 'studio');
    LArguments.Add('-Fu' + LDirectory);
    LArguments.Add('-FE' + LDirectory);

    if FProgress <> nil then
    begin
      LReader := NewNyxProjectCompilerReportReader(LSubmittedSource, LCompanionSource,
        LDirectory + LUnitName + '.pas', LDirectory + 'project', LProjectInputs);
    end;

    if ATarget = 'browser' then
    begin
      LExecutable := FOutputs.Field('pas2js');
      LRuntime := FOutputs.Field('runtime');
      LFileUnits := '';
      LFileSetup := '';
      LFileDeclarations := '';
      LClipboardSource := 'nil';

      if FOutputs.FileSaving then
      begin
        LFileUnits := ', nyx.files.save, nyx.files.save.browser, nyx.files.save.activation.browser';
        LFileDeclarations := '  LFileSaveHost: INyxFileSaveHost;' + #10;
        LFileSetup := '  LFileSaveHost := NewNyxBrowserActivatedFileSaveHost(' + #10 +
          '    NewNyxBrowserFileSaveHost);' + #10 +
          '  LApplication.ConfigureFileSaving(LFileSaveHost);' + #10;
      end;

      if FOutputs.ClipboardWriting then
      begin
        LFileUnits := LFileUnits + ', nyx.clipboard.browser';
        LClipboardSource := 'NewNyxBrowserClipboardWriteHost';
      end;

      if FOutputs.ClipboardWriting or FOutputs.FileSaving then
      begin
        LFileUnits := LFileUnits + ', nyx.collections.workbench.runtime';

        if FOutputs.FileSaving then
        begin
          LFileUnits := LFileUnits + ', nyx.collections.export';
          LFileSetup := LFileSetup + '  LApplication.ConfigureTableWorkbench(' + #10 +
            '    NewNyxTableWorkbenchServices(' + LClipboardSource + ', LFileSaveHost,' + #10 +
            '      TNyxCollectionExportOptions.Defaults));' + #10;
        end
        else
        begin
          LFileSetup := LFileSetup + '  LApplication.ConfigureTableWorkbench(' + #10 +
            '    NewNyxTableWorkbenchServices(' + LClipboardSource + '));' + #10;
        end;
      end;

      if FOutputs.FileOpening then
      begin
        LFileUnits := LFileUnits + ', nyx.files.open.browser';
        LFileSetup := LFileSetup + '  LApplication.ConfigureFileOpening(' + #10 +
          '    NewNyxBrowserFileOpenHost);' + #10;
      end;

      if FOutputs.HTTP then
      begin
        LFileUnits := LFileUnits + ', nyx.logic.http, nyx.resources.http.browser';
        LFileSetup := LFileSetup + '  LApplication.ConfigureLogic(' + #10 +
          '    NewNyxHTTPLogicResolver(NewNyxBrowserHTTPTransport));' + #10;
      end;

      LSource := 'program nyx_preview;' + #10 + '{$mode delphi}{$H+}' + #10 +
        '{$codepage utf8}' + #10 +
        'uses' + #10 +
        '  Web, nyx.text, nyx.types, nyx.model, nyx.composition,' + #10 +
        '  nyx.application.view, nyx.application.browser, nyx.studio.runtimeclient' +
        LFileUnits + ', ' + LUnitName + ';' + #10 +
        'var' + #10 + '  LDocument: TNyxDocument;' + #10 +
        LScopeDeclarations +
        LFileDeclarations +
        '  LApplication: TNyxBrowserApplication;' + #10 +
        '  LReporter: TNyxStudioRuntimeReporter;' + #10 +
        'begin' + #10 +
        '  LDocument := BuildNyxDocument;' + #10 +
        LScopeConstruction +
        '  LApplication := TNyxBrowserApplication.Create;' + #10 +
        LViewSetup +
        LFileSetup +
        '  LApplication.Run(LDocument, TJSHTMLElement(document.body));' + #10 +
        '  document.body.setAttribute(''data-nyx-ready'', ''true'');' + #10 +
        '  { Optional Studio diagnostics must never prevent an application from running. }' + #10 +
        '  try' + #10 +
        '    LReporter := ObserveNyxStudioResources(LApplication.Resources);' + #10 +
        '  except' + #10 +
        '    document.body.setAttribute(''data-nyx-resource-reporting'', ''unavailable'');' + #10 +
        '  end;' + #10 +
        'end.' + #10;
      if LProjectStorage <> nil then
      begin
        LProjectStorage.WriteEvidence('nyx_preview.lpr', LSource);

        if LProjectInputs.Count > 0 then
        begin
          LArguments.Add('-Fu' + LDirectory + 'project');
          LArguments.Add('-Fi' + LDirectory + 'project');
        end;
      end
      else
      begin
        WriteFile(LDirectory + 'nyx_preview.lpr', LSource);
      end;
      LArguments.Add(LDirectory + 'nyx_preview.lpr');
      LOK := RunCompiler(LExecutable, LDirectory, LArguments, LLog, ACancellation,
        True, LReader);
      WriteFile(LDirectory + 'rtl.js', ReadFile(LRuntime));
      WriteFile(LDirectory + 'index.html', HostHTML('nyx_preview'));
    end
    else
    begin
      LExecutable := FOutputs.Field('fpc');
      LLazarus := IncludeTrailingPathDelimiter(FOutputs.Field('lazarus'));

      LArguments.Add('-FU' + LDirectory + 'units');
      LPlatform := FOutputs.Field('platform');
      LWidgetset := FOutputs.Field('widgetset');
      LFileUnits := '';
      LFileSetup := '';
      LFileDeclarations := '';
      LClipboardSource := 'nil';

      if FOutputs.FileSaving then
      begin
        LFileUnits := ', nyx.files.save, nyx.files.save.lcl, nyx.files.save.activation.lcl';
        LFileDeclarations := '  LFileSaveHost: INyxFileSaveHost;' + #10;
        LFileSetup := '    LFileSaveHost := NewNyxLCLActivatedFileSaveHost(nil,' + #10 +
          '      NewNyxLCLFileSaveHost);' + #10 +
          '    LApplication.ConfigureFileSaving(LFileSaveHost);' + #10;
      end;

      if FOutputs.ClipboardWriting then
      begin
        LFileUnits := LFileUnits + ', nyx.clipboard.lcl';
        LClipboardSource := 'NewNyxLCLClipboardWriteHost';
      end;

      if FOutputs.ClipboardWriting or FOutputs.FileSaving then
      begin
        LFileUnits := LFileUnits + ', nyx.collections.workbench.runtime';

        if FOutputs.FileSaving then
        begin
          LFileUnits := LFileUnits + ', nyx.collections.export';
          LFileSetup := LFileSetup + '    LApplication.ConfigureTableWorkbench(' + #10 +
            '      NewNyxTableWorkbenchServices(' + LClipboardSource + ', LFileSaveHost,' + #10 +
            '        TNyxCollectionExportOptions.Defaults));' + #10;
        end
        else
        begin
          LFileSetup := LFileSetup + '    LApplication.ConfigureTableWorkbench(' + #10 +
            '      NewNyxTableWorkbenchServices(' + LClipboardSource + '));' + #10;
        end;
      end;

      if FOutputs.FileOpening then
      begin
        LFileUnits := LFileUnits + ', nyx.files.open.lcl';
        LFileSetup := LFileSetup + '    LApplication.ConfigureFileOpening(' + #10 +
          '      NewNyxLCLFileOpenHost);' + #10;
      end;

      if FOutputs.HTTP then
      begin
        LFileUnits := LFileUnits + ', nyx.logic.http, nyx.resources.http.lcl';
        LFileSetup := LFileSetup + '    LApplication.ConfigureLogic(' + #10 +
          '      NewNyxHTTPLogicResolver(NewNyxNativeHTTPTransport(' + #10 +
          '        LApplication.View.Events.Scheduler)));' + #10;
      end;

      LArguments.Add('-Fu' + LLazarus + 'lcl/units/' + LPlatform);
      LArguments.Add('-Fu' + LLazarus + 'lcl/units/' + LPlatform + '/' + LWidgetset);
      LArguments.Add('-Fu' + LLazarus + 'components/lazutils/lib/' + LPlatform);
      LArguments.Add('-Fu' + LLazarus + 'packager/units/' + LPlatform);
      LSource := 'program nyx_native;' + #10 + '{$mode delphi}{$H+}' + #10 +
        '{$codepage utf8}' + #10 +
        'uses' + #10 +
        '  Interfaces, Forms, nyx.text, nyx.types, nyx.model, nyx.composition,' + #10 +
        '  nyx.application.view, nyx.application.lcl, nyx.studio.runtimeclient' +
        LFileUnits + ', ' + LUnitName + ';' + #10 +
        'var' + #10 + '  LDocument: TNyxDocument;' + #10 +
        LScopeDeclarations +
        LFileDeclarations +
        '  LApplication: TNyxLCLApplication;' + #10 +
        '  LReporter: TNyxStudioRuntimeReporter;' + #10 +
        'begin' + #10 + '  Application.Initialize;' + #10 +
        '  LDocument := BuildNyxDocument;' + #10 +
        LScopeConstruction +
        '  LApplication := TNyxLCLApplication.Create;' + #10 +
        '  LReporter := nil;' + #10 + '  try' + #10 +
        LFileSetup +
        LViewSetup +
        '    LApplication.Mount(LDocument);' + #10 +
        '    { A refused diagnostic context leaves ordinary application interaction usable. }' + #10 +
        '    try' + #10 +
        '      LReporter := ObserveNyxStudioResources(LApplication.Resources);' + #10 +
        '    except' + #10 +
        '      LReporter := nil;' + #10 +
        '    end;' + #10 +
        '    Application.Run;' + #10 +
        '  finally' + #10 + '    LReporter.Free;' + #10 + '    LApplication.Free;' + #10 +
        '    LDocument.Free;' + #10 +
        '  end;' + #10 + 'end.' + #10;
      if LProjectStorage <> nil then
      begin
        LProjectStorage.WriteEvidence('nyx_native.lpr', LSource);

        if LProjectInputs.Count > 0 then
        begin
          LArguments.Add('-Fu' + LDirectory + 'project');
          LArguments.Add('-Fi' + LDirectory + 'project');
        end;
      end
      else
      begin
        WriteFile(LDirectory + 'nyx_native.lpr', LSource);
      end;
      LArguments.Add(LDirectory + 'nyx_native.lpr');
      LOK := RunCompiler(LExecutable, LDirectory, LArguments, LLog, ACancellation,
        True, LReader);
    end;

    if ACancellation <> nil then
    begin
      ACancellation.Check;
    end;
    WriteFile(LDirectory + 'compiler.log', LLog);
    LReport := ReadNyxProjectCompilerReport(LSubmittedSource, LCompanionSource,
      LDirectory + LUnitName + '.pas', LDirectory + 'project', LLog, LProjectInputs);
    Result := TJSONObject.Create;
    Result.Add('ok', LOK);
    Result.Add('failure', NyxCompilerFailureName(FFailure));
    Result.Add('build', LJob);
    Result.Add('target', ATarget);
    Result.Add('scope', AScope);
    Result.Add('log', LLog);
    Result.Add('diagnostics', DecodeNyxJSON(LReport.Encode));

    if ATarget = 'browser' then
    begin
      Result.Add('artifact', 'builds/' + LJob + '/index.html');
    end
    else
    begin
      Result.Add('artifact', 'builds/' + LJob + '/nyx_native.exe');
    end;
    Result.Add('source', 'builds/' + LJob + '/' + LUnitName + '.pas');
    Result.Add('companion', ACompanion <> '');
  finally
    LArguments.Free;
    LDocument.Free;
    if LProjectStorage <> nil then
    begin
      LProjectStorage.FinishAfterJoin;
    end;
    LProjectStorage.Free;
    LIncludes.Free;
  end;
end;

end.
