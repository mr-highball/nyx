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
unit nyx.studio.pascalprofiles;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.context, nyx.pascal.compilertrace,
  nyx.pascal.files, nyx.pascal.modules, nyx.pascal.translation, nyx.pascal.compilerplan,
  nyx.pascal.requirements, nyx.pascal.fulfillment;

type
  { Copied limits for explicit native module preparation. Probe/translation work
    is bounded across the whole operation, while each compiler retains its own
    timeout/transcript limits. Default plans prefetch bounded conditional-symbol
    candidates, including one captured literal-include pool; callers may select
    retained-only/minimal plans. No host/interface borrow lives here. }
  TNyxFPCModulePreparationOptions = record
  private
    FDefined: Boolean;
    FProbes: Integer;
    FTokens: Integer;
    FTimeout: Cardinal;
    FPlan: TNyxPascalCompilerPlanOptions;
    FIncludes: Boolean;
    FTrace: TNyxPascalCompilerTraceOptions;
  public
    class function Defaults: TNyxFPCModulePreparationOptions; static;
    function UpToProbes(AValue: Integer): TNyxFPCModulePreparationOptions;
    function UpToTokens(AValue: Integer): TNyxFPCModulePreparationOptions;
    function TimeoutMilliseconds(AValue: Cardinal): TNyxFPCModulePreparationOptions;
    function WithPlan(const AValue: TNyxPascalCompilerPlanOptions): TNyxFPCModulePreparationOptions;
    { Build a speculative captured-include pool once when conditional lookahead
      is enabled. False retains the previous translated-row-only preparation. }
    function LookAheadKnownIncludes(AValue: Boolean): TNyxFPCModulePreparationOptions;
    function WithTrace(const AValue: TNyxPascalCompilerTraceOptions): TNyxFPCModulePreparationOptions;
    procedure Validate;
  end;
  { Immutable prepared descriptor plus exact remaining source requirements.
    Probes counts only jobs launched here. Unresolved scoped sizeof/declared,
    includes and unsupported syntax remain explicit; preparation is neither
    successful type binding nor application compilation/execution provenance. }
  INyxFPCModulePreparation = interface(IInterface)
    ['{814416FA-B8F7-45DC-B6AC-83D29D238905}']
    function ModuleSource: INyxPascalModuleSource;
    function Source: INyxPascalTranslation;
    function Requirements: INyxPascalCompilerPlan;
    function Probes: Integer;
  end;

{ Explicit synchronous native compiler capture; a caller uses a worker when UI
  responsiveness matters. The fresh directory is a caller-authorized private
  scratch location, never an application project or shared Studio checkpoint.
  Its source/log/intermediates remain available for review. Existing directories
  refuse. No executable, assembler or linker produced by the probe is run.
  The configured FPC and its normal configuration select the target; the probe's
  one typed source MODE supplies its per-unit defaults. No host/compiler install
  or project setting is changed. Numbered English FPC messages are required.
  Timeout/cancellation/output/IO failure retires and joins this exact compiler
  family before raising. The ordinary compiler adapter's native platform limits
  apply. Bootstrap requests use -Us and stop intentionally only after their full
  packet; exit 1 is admitted solely with the exact owned fatal/abort receipts.
  This opt-in primitive is not default Studio adoption or a build job. }
function CaptureNyxFPCCompilerContext(const ACompiler, AFreshDirectory: TNyxText;
  const AProbe: INyxPascalCompilerProbe;
  const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope = nil;
  ATimeoutMilliseconds: Cardinal = 30000): INyxPascalCompilerContext;
{ The same owned process/source/transcript gate, retaining a separate observed
  compatibility anchor for verified cohort merging. Both snapshots publish only
  after exact compiler-family retirement; no host or producer lease survives. }
function CaptureNyxFPCCompilerCapture(const ACompiler, AFreshDirectory: TNyxText;
  const AProbe: INyxPascalCompilerProbe;
  const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope = nil;
  ATimeoutMilliseconds: Cardinal = 30000): INyxFPCCompilerCapture;

{ Fulfill one explicit typed primitive request against its exact owning capture.
  Preflight freezes both producers and refuses context mismatch before creating
  scratch. The ordinary capture gate joins the configured compiler; a compatible
  reply adds only the requested size/presence fact. Source-local contexts cannot
  be silently retagged as pristine or bootstrap. This is one separate opt-in job,
  not an implicit increase of module preparation's compiler allowance. }
function FulfillNyxFPCCompilerRequirement(const ACompiler, AFreshDirectory: TNyxText;
  const ARequirement: INyxPascalCompilerRequirement;
  const AOwner: INyxFPCCompilerCapture; const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope = nil;
  ATimeoutMilliseconds: Cardinal = 30000): INyxFPCCompilerCapture;
{ Preflight all unique typed jobs and exact source/capture owners before creating
  one fresh private batch directory. Explicit one-through-64 job allowance is
  separate from symbol preparation. Serial joined jobs retain partial scratch on
  failure, but publish updated modules/captures only after every one-fact reply
  validates. Empty requests need no compiler/directory. The caller owns replay
  and worker scheduling; no project, source byte, target configuration or service
  is changed and no compiled probe program is executed. }
function FulfillNyxFPCCompilerRequirements(const ACompiler, AFreshDirectory: TNyxText;
  const AFiles: INyxPascalFileCatalog; const AModules: INyxPascalModuleCatalog;
  const AResponse: IInterface; const ACaptures: INyxPascalCompilerCaptures;
  AMaximumJobs: Integer; const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope = nil;
  ATimeoutMilliseconds: Cardinal = 30000): INyxPascalCompilerFulfillment;

{ Opt-in synchronous host operation, intended for an owning worker. Inputs are
  frozen before writing a fresh caller-owned scratch directory. The descriptor
  must supply a real FPC context compatible with the base probe's starting mode.
  Only newly requested compiler symbols trigger another joined capture, including
  optional retained-directive candidates measured by the actual compiler; scoped
  conditions never become pristine size queries. Complete current requests or
  preserve their remaining source gaps. Limit/cancellation/IO/identity failure
  publishes no partial result and retains scratch evidence. No Studio/project
  configuration, SDK bytes, installed service or application compiler job changes. }
function PrepareNyxFPCModule(const ACompiler, AFreshDirectory: TNyxText;
  const AFiles: INyxPascalFileCatalog; const AModule: INyxPascalModuleSource;
  const ABaseProbe: INyxPascalCompilerProbe; const AOptions: TNyxFPCModulePreparationOptions;
  const ACancellation: INyxCancellationScope = nil): INyxFPCModulePreparation;

implementation

uses Classes, SysUtils, Process, LazFileUtils, nyx.bytes, nyx.pascal.lexing,
  nyx.studio.compilerprocess
  {$ifdef windows}, Windows{$endif};

const
  CProbeFile: TNyxText = 'nyxcompilercontext.pas';

type
  { Published data owns only portable snapshots and a detached descriptor.
    Temporary compiler/cancellation/directory owners stay on the host stack. }
  TPreparedModule = class(TInterfacedObject, INyxFPCModulePreparation)
  public
    Descriptor: INyxPascalModuleSource;
    Translation: INyxPascalTranslation;
    Plan: INyxPascalCompilerPlan;
    ProbeCount: Integer;
    function ModuleSource: INyxPascalModuleSource;
    function Source: INyxPascalTranslation;
    function Requirements: INyxPascalCompilerPlan;
    function Probes: Integer;
  end;

procedure CheckCancellation(const ACancellation: INyxCancellationScope);
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Native compiler context capture cancelled');
  end;
end;

procedure WriteFresh(const APath: TNyxText; const ABytes: TNyxBytes);
var
  LHandle: THandle;
  LStream: THandleStream;
begin
  {$ifdef windows}
  LHandle := CreateFileW(PWideChar(UTF8Decode(APath)), GENERIC_WRITE, 0,
    nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  {$else}
  LHandle := FileCreate(String(RawByteString(APath)));
  {$endif}

  if LHandle = THandle(-1) then
  begin
    raise ENyxPascalContext.Create('Could not create a fresh compiler profile file');
  end;
  LStream := nil;
  try
    LStream := THandleStream.Create(LHandle);

    if Length(ABytes) > 0 then
    begin
      LStream.WriteBuffer(ABytes[0], Length(ABytes));
    end;
  finally
    LStream.Free;
    FileClose(LHandle);
  end;
end;

function CaptureNyxFPCCompilerContext(const ACompiler, AFreshDirectory: TNyxText;
  const AProbe: INyxPascalCompilerProbe;
  const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope;
  ATimeoutMilliseconds: Cardinal): INyxPascalCompilerContext;
begin
  Result := CaptureNyxFPCCompilerCapture(ACompiler, AFreshDirectory, AProbe,
    AOptions, ACancellation, ATimeoutMilliseconds).Context;
end;

function CaptureNyxFPCCompilerCapture(const ACompiler, AFreshDirectory: TNyxText;
  const AProbe: INyxPascalCompilerProbe;
  const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope;
  ATimeoutMilliseconds: Cardinal): INyxFPCCompilerCapture;
var
  LProbe: INyxPascalCompilerProbe;
  LCancellation: INyxCancellationScope;
  LProcess: TNyxCompilerProcess;
  LBytes: TNyxBytes;
  LChunk: array[0..8191] of Byte;
  LSource: TNyxText;
  LDirectory: TNyxText;
  LSourcePath: TNyxText;
  LProbeFile: TNyxText;
  LLog: TNyxText;
  LCount: Integer;
  LBefore: Integer;
  LCode: Integer;
  LStart: QWord;
  LSourceHandle: THandle;
  LResult: INyxFPCCompilerCapture;

  procedure CheckDeadline;
  begin
    CheckCancellation(LCancellation);

    if GetTickCount64 - LStart >= ATimeoutMilliseconds then
    begin
      raise ENyxPascalContext.Create('The owned compiler context query exceeded its time budget');
    end;
  end;

  { Consume available bytes without waiting for a whole line or pipe EOF.
    Retirement owns the process family; closing a stream is not cancellation. }
  procedure Drain;
  begin
    while LProcess.Output.NumBytesAvailable > 0 do
    begin
      CheckDeadline;
      LCount := LProcess.Output.Read(LChunk, SizeOf(LChunk));

      if LCount <= 0 then
      begin
        Break;
      end;
      LBefore := Length(LBytes);

      if LCount > AOptions.MaximumBytes - LBefore then
      begin
        raise ENyxPascalContext.Create('Compiler profile output exceeds its byte budget');
      end;
      SetLength(LBytes, LBefore + LCount);
      Move(LChunk[0], LBytes[LBefore], LCount);
    end;
  end;

begin
  { Pin a caller's inline interfaced scope through retirement on every exit.
    A const parameter alone does not own that temporary on native FPC. }
  LCancellation := ACancellation;
  AOptions.Validate;
  CheckCancellation(LCancellation);

  if (AProbe = nil) or (ATimeoutMilliseconds = 0) or not FileExistsUTF8(ACompiler) or
    (Pos(#0, ACompiler) > 0) or (Pos(#0, AFreshDirectory) > 0) or
    (AFreshDirectory = '') or DirectoryExistsUTF8(AFreshDirectory) or FileExistsUTF8(AFreshDirectory) then
  begin
    raise ENyxPascalContext.Create('Compiler fact capture needs a compiler, bounded time and fresh private directory');
  end;
  LProbe := CaptureNyxFPCCompilerProbe(AProbe);
  LSource := LProbe.Source;
  LDirectory := IncludeTrailingPathDelimiter(ExpandFileNameUTF8(AFreshDirectory));

  if not CreateDirUTF8(LDirectory) then
  begin
    raise ENyxPascalContext.Create('The private compiler profile directory could not be created');
  end;
  LProbeFile := CProbeFile;

  if NyxFPCProbeScope(LProbe) = pcsoSystemBootstrap then
  begin
    LProbeFile := 'system.pp';
  end;
  LSourcePath := LDirectory + LProbeFile;
  WriteFresh(LSourcePath, NyxEncodeUTF8(LSource));
  {$ifdef windows}
  LSourceHandle := CreateFileW(PWideChar(UTF8Decode(LSourcePath)), GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  {$else}
  LSourceHandle := FileOpen(String(RawByteString(LSourcePath)), fmOpenRead or fmShareDenyWrite);
  {$endif}

  if LSourceHandle = THandle(-1) then
  begin
    raise ENyxPascalContext.Create('The exact compiler probe source could not be leased');
  end;
  LProcess := nil;
  try
    LProcess := TNyxCompilerProcess.Create(nil);
    LProcess.Executable := String(ACompiler);
    LProcess.CurrentDirectory := String(LDirectory);
    LProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];

    if NyxFPCProbeScope(LProbe) = pcsoSystemBootstrap then
    begin
      LProcess.Parameters.Add('-Mfpc');
      LProcess.Parameters.Add('-Us');
    end
    else
    begin
      LProcess.Parameters.Add('-Mdelphi');
    end;
    LProcess.Parameters.Add('-s');
    LProcess.Parameters.Add('-Cn');
    LProcess.Parameters.Add('-v0');
    LProcess.Parameters.Add('-vc');
    LProcess.Parameters.Add('-vi');
    LProcess.Parameters.Add('-vn');
    LProcess.Parameters.Add('-vq');
    LProcess.Parameters.Add('-FU' + String(LDirectory));
    LProcess.Parameters.Add('-FE' + String(LDirectory));
    LProcess.Parameters.Add(String(LProbeFile));
    LStart := GetTickCount64;
    CheckCancellation(LCancellation);
    LProcess.Execute;
    while LProcess.FamilyRunning do
    begin
      Drain;
      CheckDeadline;
      Sleep(5);
    end;
    Drain;
    CheckDeadline;
    LProcess.WaitOnExit;
    LCode := LProcess.ExitStatus;
    LLog := NyxDecodeUTF8(LBytes);
    WriteFresh(LDirectory + 'compiler-profile.log', LBytes);
    LResult := ReadNyxFPCCompilerCapture(LProbe, LProbeFile, LLog, LCode,
      AOptions, LCancellation);
  finally

    if LProcess <> nil then
    begin
      try
        LProcess.RetireAndJoin;
      finally
        LProcess.Free;
      end;
    end;
    FileClose(LSourceHandle);
  end;
  Result := LResult;
end;

function TPreparedModule.ModuleSource: INyxPascalModuleSource;
begin
  Result := Descriptor;
end;

function TPreparedModule.Source: INyxPascalTranslation;
begin
  Result := Translation;
end;

function TPreparedModule.Requirements: INyxPascalCompilerPlan;
begin
  Result := Plan;
end;

function TPreparedModule.Probes: Integer;
begin
  Result := ProbeCount;
end;

class function TNyxFPCModulePreparationOptions.Defaults: TNyxFPCModulePreparationOptions;
begin
  Result := Default(TNyxFPCModulePreparationOptions);
  Result.FDefined := True;
  Result.FProbes := 4;
  Result.FTokens := 4000000;
  Result.FTimeout := 30000;
  Result.FPlan := TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True);
  Result.FIncludes := True;
  Result.FTrace := TNyxPascalCompilerTraceOptions.Defaults;
end;

procedure TNyxFPCModulePreparationOptions.Validate;
begin
  FPlan.Validate;
  FTrace.Validate;

  if not FDefined or (FProbes < 1) or (FTokens < 1) or (FTimeout = 0) then
  begin
    raise ENyxPascalContext.Create('Use explicit positive FPC module preparation limits');
  end;
end;

function TNyxFPCModulePreparationOptions.UpToProbes(AValue: Integer): TNyxFPCModulePreparationOptions;
begin
  Result := Self;
  Result.FProbes := AValue;
  Result.Validate;
end;

function TNyxFPCModulePreparationOptions.UpToTokens(AValue: Integer): TNyxFPCModulePreparationOptions;
begin
  Result := Self;
  Result.FTokens := AValue;
  Result.Validate;
end;

function TNyxFPCModulePreparationOptions.TimeoutMilliseconds(AValue: Cardinal): TNyxFPCModulePreparationOptions;
begin
  Result := Self;
  Result.FTimeout := AValue;
  Result.Validate;
end;

function TNyxFPCModulePreparationOptions.WithPlan(const AValue: TNyxPascalCompilerPlanOptions): TNyxFPCModulePreparationOptions;
begin
  Result := Self;
  Result.FPlan := AValue;
  Result.Validate;
end;

function TNyxFPCModulePreparationOptions.LookAheadKnownIncludes(
  AValue: Boolean): TNyxFPCModulePreparationOptions;
begin
  Result := Self;
  Result.FIncludes := AValue;
  Result.Validate;
end;

function TNyxFPCModulePreparationOptions.WithTrace(const AValue: TNyxPascalCompilerTraceOptions): TNyxFPCModulePreparationOptions;
begin
  Result := Self;
  Result.FTrace := AValue;
  Result.Validate;
end;

{ The same host-capability preflight serves single and batch operations. Building
  a typed probe performs no file/process effect and cannot retag source contexts. }
function RequiredFactProbe(const ARequirement: INyxPascalCompilerRequirement;
  const AOwner: INyxFPCCompilerCapture): INyxPascalCompilerProbe;
begin

  if not AOwner.Context.SameFacts(ARequirement.Context) then
  begin
    raise ENyxPascalContext.Create('A compiler requirement needs its exact owning capture');
  end;

  if NyxPascalCompilerScope(ARequirement.Context) = pcsoSystemBootstrap then
  begin

    if ARequirement.Context.Mode <> ppmFPC then
    begin
      raise ENyxPascalContext.Create('This host qualifies compiler-root requests in FPC mode');
    end;
    Result := NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('NYX_REQUIRED_FACT'));
  end
  else
  begin
    Result := NewNyxFPCCompilerProbe(NyxPascalDefine('NYX_REQUIRED_FACT'), ARequirement.Context.Mode);
  end;
  Result := NyxFPCProbeSizeOf(Result, ARequirement.Reference);
end;

function FulfillNyxFPCCompilerRequirement(const ACompiler, AFreshDirectory: TNyxText;
  const ARequirement: INyxPascalCompilerRequirement;
  const AOwner: INyxFPCCompilerCapture; const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope;
  ATimeoutMilliseconds: Cardinal): INyxFPCCompilerCapture;
var
  LRequirement: INyxPascalCompilerRequirement;
  LOwner: INyxFPCCompilerCapture;
  LProbe: INyxPascalCompilerProbe;
  LFresh: INyxFPCCompilerCapture;
begin
  AOptions.Validate;
  CheckCancellation(ACancellation);
  LRequirement := CaptureNyxPascalRequirement(ARequirement);
  LOwner := FreezeNyxFPCCompilerCapture(AOwner);

  LProbe := RequiredFactProbe(LRequirement, LOwner);
  LFresh := CaptureNyxFPCCompilerCapture(ACompiler, AFreshDirectory, LProbe,
    AOptions, ACancellation, ATimeoutMilliseconds);
  Result := MergeNyxFPCSizeCapture(LOwner, LFresh, LRequirement.Reference);
end;

function FulfillNyxFPCCompilerRequirements(const ACompiler, AFreshDirectory: TNyxText;
  const AFiles: INyxPascalFileCatalog; const AModules: INyxPascalModuleCatalog;
  const AResponse: IInterface; const ACaptures: INyxPascalCompilerCaptures;
  AMaximumJobs: Integer; const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope;
  ATimeoutMilliseconds: Cardinal): INyxPascalCompilerFulfillment;
var
  LPlan: INyxPascalCompilerFulfillmentPlan;
  LReplies: INyxPascalCompilerCaptures;
  LDirectory: TNyxText;
  LAt: Integer;
begin
  AOptions.Validate;
  CheckCancellation(ACancellation);
  LPlan := PlanNyxPascalCompilerFulfillment(AFiles, AModules, AResponse,
    ACaptures, AMaximumJobs, ACancellation);
  for LAt := 0 to LPlan.Count - 1 do
  begin
    CheckCancellation(ACancellation);
    RequiredFactProbe(LPlan.RequirementAt(LAt), LPlan.OwnerAt(LAt));
  end;
  LReplies := NewNyxPascalCompilerCaptures;

  if LPlan.Count = 0 then
  begin
    Exit(LPlan.ApplyReplies(LReplies, ACancellation));
  end;

  if not FileExistsUTF8(ACompiler) or (Pos(#0, ACompiler) > 0) or
    (AFreshDirectory = '') or (Pos(#0, AFreshDirectory) > 0) or
    DirectoryExistsUTF8(AFreshDirectory) or FileExistsUTF8(AFreshDirectory) or
    (ATimeoutMilliseconds = 0) then
  begin
    raise ENyxPascalContext.Create('Primitive batches need a configured compiler and fresh private scratch');
  end;
  LDirectory := IncludeTrailingPathDelimiter(ExpandFileNameUTF8(AFreshDirectory));

  if not CreateDirUTF8(LDirectory) then
  begin
    raise ENyxPascalContext.Create('The primitive batch directory could not be created');
  end;
  for LAt := 0 to LPlan.Count - 1 do
  begin
    CheckCancellation(ACancellation);
    LReplies := LReplies.WithCapture(FulfillNyxFPCCompilerRequirement(ACompiler,
      LDirectory + 'fact-' + TNyxText(IntToStr(LAt + 1)), LPlan.RequirementAt(LAt),
      LPlan.OwnerAt(LAt), AOptions, ACancellation, ATimeoutMilliseconds));
  end;
  Result := LPlan.ApplyReplies(LReplies, ACancellation);
end;

function PrepareNyxFPCModule(const ACompiler, AFreshDirectory: TNyxText;
  const AFiles: INyxPascalFileCatalog; const AModule: INyxPascalModuleSource;
  const ABaseProbe: INyxPascalCompilerProbe; const AOptions: TNyxFPCModulePreparationOptions;
  const ACancellation: INyxCancellationScope): INyxFPCModulePreparation;
var
  LFiles: INyxPascalFileCatalog;
  LModules: INyxPascalModuleCatalog;
  LModule: INyxPascalModuleSource;
  LProbe: INyxPascalCompilerProbe;
  LNext: INyxPascalCompilerProbe;
  LContext: INyxPascalCompilerContext;
  LInitial: INyxPascalCompilerContext;
  LCancel: INyxCancellationScope;
  LOwner: TPreparedModule;
  LLease: INyxFPCModulePreparation;
  LOptions: TNyxPascalTranslationOptions;
  LRemaining: Integer;
  LPlanOptions: TNyxPascalCompilerPlanOptions;
  LCandidates: INyxPascalCompilerCandidates;
  LFresh: INyxFPCCompilerCapture;
  LAccumulated: INyxFPCCompilerCapture;
  LDirectory: TNyxText;
  LIncludes: INyxPascalCompilerIncludeCandidates;

  function MissingFacts(const AProbe: INyxPascalCompilerProbe): Boolean;
  var
    LAt: Integer;
    LSizes: INyxPascalSizeProbe;
  begin
    for LAt := 0 to AProbe.Count - 1 do
    begin

      if LContext.Symbol(AProbe.Symbol(LAt)).Kind = ppdUnknown then
      begin
        Exit(True);
      end;
    end;

    if Supports(AProbe, INyxPascalSizeProbe, LSizes) then
    begin
      for LAt := 0 to LSizes.SizeCount - 1 do
      begin

        if NyxPascalCapturedSize(LContext, LSizes.SizeName(LAt)).State = pssUnknown then
        begin
          Exit(True);
        end;
      end;
    end;
    Result := False;
  end;
begin
  AOptions.Validate;
  LCancel := ACancellation;
  CheckCancellation(LCancel);
  LFiles := CaptureNyxPascalFiles(AFiles);
  LProbe := CaptureNyxFPCCompilerProbe(ABaseProbe);
  LModules := CaptureNyxPascalModules(NewNyxPascalModuleCatalog.WithModule(AModule), LFiles);
  LModule := LModules.Entry(0);
  LInitial := LModule.Context;

  if (LInitial = nil) or (LInitial.Family <> ppcFPC) or (LInitial.Mode <> LProbe.Mode) or
    (NyxFPCProbeScope(LProbe) <> pcsoPristine) or
    (NyxPascalCompilerScope(LInitial) <> pcsoPristine) or
    (LInitial.Version.FullNumber = 0) or not FileExistsUTF8(ACompiler) or
    (Pos(#0, ACompiler) > 0) or (Pos(#0, AFreshDirectory) > 0) or
    (AFreshDirectory = '') or DirectoryExistsUTF8(AFreshDirectory) or FileExistsUTF8(AFreshDirectory) then
  begin
    raise ENyxPascalContext.Create('FPC module preparation needs explicit compatible facts and fresh scratch');
  end;
  LDirectory := IncludeTrailingPathDelimiter(ExpandFileNameUTF8(AFreshDirectory));

  if not CreateDirUTF8(LDirectory) then
  begin
    raise ENyxPascalContext.Create('The private FPC module preparation directory could not be created');
  end;
  LContext := LInitial;
  LAccumulated := nil;
  LRemaining := AOptions.FTokens;
  LOwner := TPreparedModule.Create;
  LLease := LOwner;
  LIncludes := nil;

  if AOptions.FIncludes and AOptions.FPlan.IncludeConditionalLookahead then
  begin
    LPlanOptions := AOptions.FPlan;

    if LPlanOptions.MaximumTokens > LRemaining then
    begin
      LPlanOptions := LPlanOptions.UpToTokens(LRemaining);
    end;
    LIncludes := PlanNyxPascalCompilerIncludeCandidates(LFiles, LModule.Root,
      LModule.Translation.LexicalOptions, LPlanOptions, LCancel);
    { The pool is built/charged once. Per-round plans charge enumeration and
      observed requirements separately under the existing aggregate allowance. }
    Dec(LRemaining, LIncludes.WorkUnits);
  end;
  while True do
  begin
    CheckCancellation(LCancel);
    LOptions := LModule.Translation;

    if LRemaining < 1 then
    begin
      raise ENyxPascalTranslationLimit.Create('FPC module preparation exhausted its aggregate token budget');
    end;

    if LOptions.MaximumTokens > LRemaining then
    begin
      LOptions := LOptions.UpToTokens(LRemaining);
    end;
    LOwner.Translation := NewNyxPascalTranslation(LFiles, LModule.Root, LContext, LOptions, LCancel);
    Dec(LRemaining, LOwner.Translation.Count);
    LPlanOptions := AOptions.FPlan;

    if (LPlanOptions.MaximumTokens >= LOwner.Translation.Count) and
      (LPlanOptions.MaximumTokens - LOwner.Translation.Count > LRemaining) then
    begin
      LPlanOptions := LPlanOptions.UpToTokens(LOwner.Translation.Count + LRemaining);
    end;

    if LIncludes = nil then
    begin
      LOwner.Plan := PlanNyxPascalCompilerRequirements(LOwner.Translation, LPlanOptions, LCancel);
    end
    else
    begin
      LOwner.Plan := PlanNyxPascalCompilerRequirementsWithIncludes(LOwner.Translation,
        LPlanOptions, LIncludes, LCancel);
    end;

    if Supports(LOwner.Plan, INyxPascalCompilerCandidates, LCandidates) then
    begin
      { Input rows were already charged above. Candidate work consumes the same
        aggregate budget, so prefetch cannot hide a second unbounded scan. }
      Dec(LRemaining, LCandidates.WorkUnits - LOwner.Translation.Count);
    end;
    LNext := LOwner.Plan.CompilerProbe(LProbe);

    if not MissingFacts(LNext) then
    begin
      Break;
    end;

    if LOwner.ProbeCount >= AOptions.FProbes then
    begin
      raise ENyxPascalContext.Create('FPC module preparation exhausted its compiler probe budget');
    end;
    LFresh := CaptureNyxFPCCompilerCapture(ACompiler,
      LDirectory + 'probe-' + TNyxText(IntToStr(LOwner.ProbeCount + 1)), LNext,
      AOptions.FTrace, LCancel, AOptions.FTimeout);
    Inc(LOwner.ProbeCount);

    if LAccumulated = nil then
    begin
      LAccumulated := LFresh;
    end
    else
    begin
      LAccumulated := LAccumulated.Merge(LFresh);
    end;
    LContext := LAccumulated.Context;

    if (LContext.Family <> LInitial.Family) or
      (LContext.Version.FullNumber <> LInitial.Version.FullNumber) or (LContext.Mode <> LInitial.Mode) then
    begin
      raise ENyxPascalContext.Create('The module probe changed its captured compiler identity');
    end;
    { Symbol captures are pristine program evidence. Retain the independent
      compiler-root packet supplied by this module's owner across every probe;
      source MODE/H changes never recapture or reassign those primitive facts. }
    LContext := NyxPascalWithBootstrapContext(LContext, NyxPascalBootstrapContext(LInitial));
  end;
  LOwner.Descriptor := LModule.InContext(LContext, LModule.Translation);
  Result := LLease;
end;
end.
