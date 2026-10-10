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

interface

uses
  Classes, SysUtils, Process, SyncObjs, fpjson, nyx.text, nyx.model, nyx.studio.outputs,
  nyx.studio.directories, nyx.studio.builds, nyx.source,
  nyx.studio.sourceprojection;

type
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

  ENyxBuildCancelled = class(ENyxModel);

  { Trusted host policy, copied by value. Wire requests cannot change budgets.
    Defaults preserve the existing 60-second / 1-MiB compiler limits. }
  TNyxCompilerLimits = record
  private
    FTimeMilliseconds: Cardinal;
    FLogBytes: Integer;
  public
    class function Default: TNyxCompilerLimits; static;
    { Return copied policies, retaining the other axis. Admission rejects zero
      and values above 60000 milliseconds / 1048576 bytes with ENyxModel. }
    function TimeMilliseconds(AValue: Cardinal): TNyxCompilerLimits;
    function LogBytes(AValue: Integer): TNyxCompilerLimits;
    procedure Validate;
  end;

  { One independent compiler invocation. Owns an immutable machine profile and
    borrows the caller's detached document only during Build. Both HTTP and MCP
    use this fixed-argument implementation; requests cannot supply shell commands,
    compiler options, paths or environment overrides. Artifacts are confined to
    the admitted runtime Jobs root. Compiler sources remain borrowed read-only;
    repository-mode callers retain build/studio/jobs. No editor/model lock is held here. }
  TNyxBuildExecutor = class
  private
    FDirectories: TNyxStudioDirectories;
    FJobRoot: TNyxText;
    FOutputs: TNyxOutputConfiguration;
    FLimits: TNyxCompilerLimits;
    FFailure: TNyxCompilerFailure;
    procedure CheckOutput(const ATarget: TNyxText);
    function RunCompiler(const AExecutable, ADirectory: TNyxText;
      AArguments: TStrings; out ALog: TNyxText;
      const ACancellation: INyxBuildCancellation): Boolean;
  public
    constructor Create(const ARepository, AProfile: TNyxText); overload;
    { Retains the admitted directory value separately from the immutable profile.
      Compiler units are borrowed read-only; artifacts belong to runtime Jobs. }
    constructor Create(const ADirectories: TNyxStudioDirectories;
      const AProfile: TNyxText); overload;
    destructor Destroy; override;
    { Readiness is diagnostic only; absent compilers never prevent authoring.
      Empty means ready. Contains no machine path values. }
    function Readiness(const ATarget: TNyxText): TNyxText;
    { Called by the trusted host before delegation; never during Build. }
    procedure ConfigureLimits(const ALimits: TNyxCompilerLimits);
    function Build(ADocument: TNyxDocument;
      const ATarget, AScope, APage, ACompanion: TNyxText;
      const ACancellation: INyxBuildCancellation = nil): TJSONObject;
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
      const ACancellation: INyxBuildCancellation = nil): INyxSourceProjectionBuild;
  end;

{ Returns a fresh reference-counted cancellation lifetime, initially uncancelled.
  It owns only its native event, never the caller's job or editor. }
function NewNyxBuildCancellation: INyxBuildCancellation;

implementation

uses
  nyx.json, nyx.bytes, nyx.schema, nyx.codec, nyx.codegen, nyx.callbacks,
  nyx.scheduler, nyx.composition, nyx.studio.compiler, nyx.editing,
  nyx.studio.compilerprocess;

type
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
  Result.FTimeMilliseconds := 60000;
  Result.FLogBytes := 1024 * 1024;
end;

function TNyxCompilerLimits.TimeMilliseconds(AValue: Cardinal): TNyxCompilerLimits;
begin
  Result := Self;
  Result.FTimeMilliseconds := AValue;
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

  if (FTimeMilliseconds < 1) or (FTimeMilliseconds > 60000) or
    (FLogBytes < 1) or (FLogBytes > 1024 * 1024) then
  begin
    raise ENyxModel.Create('Compiler budgets are 1..60000 ms and 1..1048576 log bytes');
  end;
end;

procedure TNyxBuildExecutor.ConfigureLimits(const ALimits: TNyxCompilerLimits);
begin
  ALimits.Validate;
  FLimits := ALimits;
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

    if not DirectoryExists(LRoot + 'lcl/units/' + LPlatform + '/' + LWidgetset) then
    begin
      raise ENyxModel.Create('Native LCL output: matching Lazarus platform/widgetset units are missing');
    end;
  end;
end;

function TNyxBuildExecutor.RunCompiler(const AExecutable, ADirectory: TNyxText;
  AArguments: TStrings; out ALog: TNyxText;
  const ACancellation: INyxBuildCancellation): Boolean;
var
  LProcess: TNyxCompilerProcess;
  LBuffer: array[0..4095] of Byte;
  LRead: Integer;
  LChunk: TNyxText;
  LStarted: QWord;

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

  procedure CheckCancellation;
  begin

    if ACancellation <> nil then
    begin
      ACancellation.Check;
    end;
  end;

  function InTime: Boolean;
  begin
    Result := GetTickCount64 - LStarted < FLimits.FTimeMilliseconds;

    if not Result then
    begin
      FFailure := bcfTimeBudget;
      ALog := ALog + #10 + 'Compiler time budget exceeded.';
    end;
  end;
begin
  CheckCancellation;
  FLimits.Validate;

  if (AExecutable = '') or not FileExists(AExecutable) then
  begin
    ALog := 'Compiler executable is missing; configure NYX_PAS2JS or NYX_FPC.';
    Exit(False);
  end;
  LProcess := TNyxCompilerProcess.Create(nil);
  try
    LProcess.Executable := AExecutable;
    LProcess.CurrentDirectory := ADirectory;
    LProcess.Parameters.Assign(AArguments);
    LProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    ALog := '';
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
    Result := LProcess.ExitStatus = 0;

    if not Result then
    begin
      FFailure := bcfCompiler;
    end;
  finally
    { Every exit (including failed suspended admission, cancellation, read
      failure, deadline/log failure) retires the invocation's process family
      and joins its exact compiler. A failed OS retirement keeps the worker
      active; never release its slot, advertise an artifact or orphan a child.
      No editor lock is held. The budget bounds compiler execution, not an OS
      that cannot reap its process. Normal Windows retirement is qualified. }

    LProcess.RetireAndJoin;
    LProcess.Free;
  end;
end;

function TNyxBuildExecutor.ProjectSource(const ASource: TNyxText;
  const AUnit: TNyxPascalUnitRef; ATarget: TNyxBuildTarget;
  AChecks: TNyxSourceProjectionChecks;
  const ACancellation: INyxBuildCancellation): INyxSourceProjectionBuild;
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
  LFile: TFileStream;
  LBytes: TNyxBytes;
  LReport: INyxCompilerReport;
  LProjection: INyxSourceProjection;
  LFailureState: TNyxSourceProjectionState;
begin
  ValidateNyxProjectionSource(ASource);
  ValidateNyxProjectionTarget(ATarget);
  TNyxCodegen.AdmitUnitName(AUnit.Name);

  if (Ord(AChecks) < Ord(Low(TNyxSourceProjectionChecks))) or
    (Ord(AChecks) > Ord(High(TNyxSourceProjectionChecks))) then
  begin
    raise ENyxModel.Create('Unknown source projection check policy');
  end;
  CreateGUID(LID);
  LReference := NyxSourceProjectionRef('job-' + Copy(GUIDToString(LID), 2, 36));
  LArguments := TStringList.Create;
  LFailureState := spsCompilationFailed;
  LArtifact := '';
  LRuntimeLog := '';
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
        LDirectory := FJobRoot + LReference.Name + PathDelim;

        if not ForceDirectories(LDirectory + 'units') then
        begin
          raise ENyxModel.Create('Cannot prepare the owned projection directory');
        end;
        LFileName := LDirectory + AUnit.Name + '.pas';
        { Copy exact source bytes. In particular, do not run PrepareNyxCompanion,
          a fluent expression reader or source workspace admission beforehand. }
        WriteFile(LFileName, ASource);
        WriteFile(LDirectory + 'nyx_projection.lpr',
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
        LArguments.Add(LDirectory + 'nyx_projection.lpr');

        if not RunCompiler(LExecutable, LDirectory, LArguments, LLog, ACancellation) then
        begin
          LReport := ReadNyxCompilerReport(ASource, ASource, LFileName, LLog);
          LProjection := NyxSourceProjectionFailure(ASource, ATarget,
            spsCompilationFailed, 'Source projection compilation failed', LReport);
        end
        else
        begin
          LReport := ReadNyxCompilerReport(ASource, ASource, LFileName, LLog);

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
            { The same process-family owner and per-invocation time/log budgets
              apply to execution. Compiler diagnostics stay in their own report. }

            if not RunCompiler(LExecutable, LDirectory, LArguments, LRuntimeLog,
              ACancellation) then
            begin
              LProjection := NyxSourceProjectionFailure(ASource, ATarget,
                spsExecutionFailed, 'Source projection execution failed', LReport);
            end
            else
            begin
              LFailureState := spsInvalidDesign;
              LFile := TFileStream.Create(LDirectory + NyxProjectionResultFile,
                fmOpenRead or fmShareDenyWrite);
              try
                { Bound byte allocation before reading/decoding; stdout remains
                  diagnostic text and cannot masquerade as a model reply. }

                if (LFile.Size < 1) or
                  (LFile.Size > NyxProjectionMaximumResultBytes) then
                begin
                  raise ENyxModel.Create('Projection result file exceeds its byte budget');
                end;
                SetLength(LBytes, LFile.Size);
                LFile.ReadBuffer(LBytes[0], Length(LBytes));
              finally
                LFile.Free;
              end;

              if ACancellation <> nil then
              begin
                ACancellation.Check;
              end;
              LProjection := ReceiveNyxSourceProjection(ASource, LReference, ATarget,
                NyxDecodeUTF8(LBytes), LReport);
            end;
          end;
        end;
        WriteFile(LDirectory + 'compiler.log', LLog);
        WriteFile(LDirectory + 'execution.log', LRuntimeLog);
      end;
    except
      on LException: ENyxBuildCancelled do
      begin
        LArtifact := '';
        LProjection := NyxSourceProjectionFailure(ASource, ATarget, spsCancelled,
          'Source projection was cancelled', LReport);
      end;
      on LException: Exception do
      begin
        LArtifact := '';
        LProjection := NyxSourceProjectionFailure(ASource, ATarget, LFailureState,
          'Source projection could not complete its owned stage', LReport);
      end;
    end;
    Result := NewNyxSourceProjectionBuild(LReference, LProjection,
      LArtifact, LRuntimeLog);
  finally
    LArguments.Free;
  end;
end;

function TNyxBuildExecutor.Build(ADocument: TNyxDocument;
  const ATarget, AScope, APage, ACompanion: TNyxText;
  const ACancellation: INyxBuildCancellation): TJSONObject;
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
  LSubmittedSource: TNyxText;
  LID: TGUID;

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
      { The public view-cloning contract preserves authored identity and takes
        only reachable definitions. A maximum-length or definition-root preview
        therefore needs neither a prefix nor a duplicate definition root. }
      LDocument := CloneNyxViewDocument(ADocument, LRoot);
    end
    else
    begin
      { Use the complete ownership contract. Copying trees/state manually lost
        collection defaults and produced a saved job design differing from its
        companion. Clone carries every admitted document feature independently. }
      LDocument := ADocument.Clone;
    end;

    if LDocument.Count = 0 then
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
      LUnitName := NyxCompanionUnitName(ACompanion);
      LCompanionSource := PrepareNyxCompanion(ADocument, LDocument, ACompanion,
        AScope = 'view');
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

    if not ForceDirectories(LDirectory + 'units') then
    begin
      raise ENyxModel.Create('Cannot prepare the admitted compiler job directory');
    end;
    WriteFile(LDirectory + LUnitName + '.pas', LCompanionSource);
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

    if ATarget = 'browser' then
    begin
      LExecutable := FOutputs.Field('pas2js');
      LRuntime := FOutputs.Field('runtime');

      LSource := 'program nyx_preview;' + #10 + '{$mode delphi}{$H+}' + #10 +
        '{$codepage utf8}' + #10 +
        'uses Web, nyx.model, nyx.application.browser, nyx.studio.runtimeclient, ' + LUnitName + ';' + #10 +
        'var' + #10 + '  LDocument: TNyxDocument;' + #10 +
        '  LApplication: TNyxBrowserApplication;' + #10 +
        '  LReporter: TNyxStudioRuntimeReporter;' + #10 +
        'begin' + #10 +
        '  LDocument := BuildNyxDocument;' + #10 +
        '  LApplication := TNyxBrowserApplication.Create;' + #10 +
        '  LApplication.Run(LDocument, TJSHTMLElement(document.body));' + #10 +
        '  document.body.setAttribute(''data-nyx-ready'', ''true'');' + #10 +
        '  { Optional Studio diagnostics must never prevent an application from running. }' + #10 +
        '  try' + #10 +
        '    LReporter := ObserveNyxStudioResources(LApplication.Resources);' + #10 +
        '  except' + #10 +
        '    document.body.setAttribute(''data-nyx-resource-reporting'', ''unavailable'');' + #10 +
        '  end;' + #10 +
        'end.' + #10;
      WriteFile(LDirectory + 'nyx_preview.lpr', LSource);
      LArguments.Add(LDirectory + 'nyx_preview.lpr');
      LOK := RunCompiler(LExecutable, LDirectory, LArguments, LLog, ACancellation);
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

      LArguments.Add('-Fu' + LLazarus + 'lcl/units/' + LPlatform);
      LArguments.Add('-Fu' + LLazarus + 'lcl/units/' + LPlatform + '/' + LWidgetset);
      LArguments.Add('-Fu' + LLazarus + 'components/lazutils/lib/' + LPlatform);
      LArguments.Add('-Fu' + LLazarus + 'packager/units/' + LPlatform);
      LSource := 'program nyx_native;' + #10 + '{$mode delphi}{$H+}' + #10 +
        '{$codepage utf8}' + #10 +
        'uses Interfaces, Forms, nyx.model, nyx.application.lcl, nyx.studio.runtimeclient, ' + LUnitName + ';' + #10 +
        'var' + #10 + '  LDocument: TNyxDocument;' + #10 +
        '  LApplication: TNyxLCLApplication;' + #10 +
        '  LReporter: TNyxStudioRuntimeReporter;' + #10 +
        'begin' + #10 + '  Application.Initialize;' + #10 +
        '  LDocument := BuildNyxDocument;' + #10 +
        '  LApplication := TNyxLCLApplication.Create;' + #10 +
        '  LReporter := nil;' + #10 + '  try' + #10 +
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
      WriteFile(LDirectory + 'nyx_native.lpr', LSource);
      LArguments.Add(LDirectory + 'nyx_native.lpr');
      LOK := RunCompiler(LExecutable, LDirectory, LArguments, LLog, ACancellation);
    end;

    if ACancellation <> nil then
    begin
      ACancellation.Check;
    end;
    WriteFile(LDirectory + 'compiler.log', LLog);
    LReport := ReadNyxCompilerReport(LSubmittedSource, LCompanionSource,
      LDirectory + LUnitName + '.pas', LLog);
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
  end;
end;

end.
