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

unit nyx.studio.buildjobs;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, SyncObjs, nyx.text, nyx.data, nyx.studio.projects,
  nyx.studio.builds, nyx.studio.compiler, nyx.studio.reviews, nyx.studio.workspaces,
  nyx.studio.directories, nyx.studio.sourcepublications, nyx.studio.sourceprojection;

type
  { Closed internal worker purpose. Source projection shares the application
    worker budget but has no application launch/report publication authority. }
  TNyxCompilerJobPurpose = (cjpApplication, cjpSourceProjection);
  { Borrowed pure comparison during the serialized List call. Never retained
    by a job or worker; no callback may mutate a session or reenter admission. }
  TNyxBuildPairCurrent = function(const APair: TNyxProjectPair): Boolean of object;
  { Native compiler jobs own immutable accepted text and a private machine
    profile. Entry methods are serialized by the MCP transport; workers touch
    only their own guarded result. No worker borrows the editor or its nodes.
    At most two invocations run concurrently, eight await a slot in FIFO order,
    and sixteen job handles are retained. Host polling advances the queue;
    cancellation keeps a slot until both child and worker have been joined.
    Terminal handles expire oldest first; artifacts retain the existing service
    lifecycle. Retry receipts (64) cannot silently submit an expired job again. }
  TNyxBuildJobs = class
  private
    FDirectories: TNyxStudioDirectories;
    FProfile: TNyxText;
    FJobs: TList;
    FReceiptKeys: array of TNyxText;
    FReceiptRequests: array of TNyxText;
    FReceipts: array of TNyxDataValue;
    FSourceLeaseMS: Integer;
    procedure Pump;
    procedure Remember(const AOwner: TNyxText; const AArguments,
      AReceipt: TNyxDataValue);
    function Enqueue(const AActor: TNyxText; const AArguments: TNyxDataValue;
      const APair: TNyxProjectPair; const AReview: TNyxReviewRef;
      const AOwner: TNyxText; const AWorkspace: TNyxWorkspaceRef;
      APurpose: TNyxCompilerJobPurpose; const ASource: TNyxText): TNyxDataValue;
    function EnqueueCaptured(const AActor: TNyxText; const AArguments: TNyxDataValue;
      const APair: TNyxProjectPair; const AReview: TNyxReviewRef;
      const AOwner: TNyxText; const AWorkspace: TNyxWorkspaceRef;
      APurpose: TNyxCompilerJobPurpose; const ASource: TNyxText;
      const APublication: TNyxStudioSourcePublication): TNyxDataValue;
    function SourcePublicationIndex(const AReference: TNyxSourceProjectionRef;
      const AWorkspace: TNyxWorkspaceRef; const AOwner: TNyxText): Integer;
  public
    constructor Create(const ARepository, AProfile: TNyxText); overload;
    { The typed source/runtime value is copied into every admitted worker. Later
      host navigation/configuration cannot redirect that worker's artifact root. }
    constructor Create(const ADirectories: TNyxStudioDirectories;
      const AProfile: TNyxText); overload;
    destructor Destroy; override;
    procedure Configure(const AProfile: TNyxText);
    function Outputs: TNyxDataValue;
    { Trusted editor configuration only. Public MCP output metadata never calls
      this accessor and never receives machine paths. Returns an owned copy. }
    function OperatorProfile: TNyxText;
    { Private operator source-compilation boundary. Caller authenticates editor
      authority and resolves the exact workspace/revision before RequestSource.
      These jobs share all slots/queue/retention and never publish a document or
      application report. Status/cancel requires the captured workspace/owner;
      cancellation retains its slot until the worker and process family join. }
    function RequestSource(const AArguments: TNyxDataValue;
      const APair: TNyxProjectPair; const AWorkspace: TNyxWorkspaceRef;
      const AOwner: TNyxText): TNyxDataValue; overload;
    { Opt-in shared work captures publication before the worker starts. Default
      compile-only jobs retain no publication authority or document reference. }
    function RequestSource(const AArguments: TNyxDataValue;
      const APair: TNyxProjectPair; const AWorkspace: TNyxWorkspaceRef;
      const AOwner: TNyxText; const APublication: TNyxStudioSourcePublication): TNyxDataValue; overload;
    { Serialized owning producer handoff. True returns an exact successful replay;
      False stages a small next-revision receipt before document publication.
      The server still authenticates and admits the executed producer response.
      Seal must run under the same registry lock, only after durable success. }
    function PrepareSourcePublication(const AReference: TNyxSourceProjectionRef;
      const AWorkspace: TNyxWorkspaceRef; const AOwner, AIssuer, AProducerText: TNyxText;
      out APublication: TNyxStudioSourcePublication; out ABuild: INyxSourceProjectionBuild;
      out AReceipt: TNyxDataValue): Boolean;
    procedure SealSourcePublication(const AReference: TNyxSourceProjectionRef;
      const AWorkspace: TNyxWorkspaceRef; const AOwner: TNyxText);
    { Validate complete immutable source arguments before an exact retry lookup. }
    procedure AdmitSourceRequest(const AArguments: TNyxDataValue);
    { Trusted host whole-job budget, including queue time. Default 120 seconds;
      1..120000 ms are admitted. Existing jobs retain their captured deadlines. }
    procedure ConfigureSourceLease(AMilliseconds: Integer);
    function SourceStatus(const AJob: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; const AOwner: TNyxText;
      ACancel: Boolean = False): TNyxDataValue;
    { At most ten active source handles in this exact private context. Omits
      source, diagnostics, profiles and artifacts; polling also joins retirement. }
    function SourceJobs(const AWorkspace: TNyxWorkspaceRef;
      const AOwner: TNyxText): TNyxDataValue;
    { Context-filtered bounded metadata, including exact currentness and caller
      cancellation authority. Omits private owners, source, logs and artifacts.
      Pump joins completed workers before reporting terminal states. }
    function List(const AArguments: TNyxDataValue; const AReview: TNyxReviewRef;
      const AWorkspace: TNyxWorkspaceRef; const AOwner: TNyxText;
      AOperator, ACanCancel: Boolean; ACurrent: TNyxBuildPairCurrent): TNyxDataValue;
    { Serialized host admission. Agents can cancel only their own job; trusted
      operators may cancel any job in the already resolved project. A queued
      job never spawns. Cancelling a terminal job is an explicit no-op. }
    function Cancel(const AOwner: TNyxText; const AArguments: TNyxDataValue;
      AOperator: Boolean): TNyxDataValue;
    procedure AdmitCancel(const AArguments: TNyxDataValue);
    { Shape/type admission precedes retry lookup or immutable pair capture. }
    procedure AdmitRequest(const AArguments: TNyxDataValue);
    function Retry(const AActor: TNyxText; const AArguments: TNyxDataValue;
      out AReceipt: TNyxDataValue): Boolean;
    function Submit(const AActor: TNyxText; const AArguments: TNyxDataValue;
      const APair: TNyxProjectPair): TNyxDataValue; overload;
    { Reviews share the original two-worker/sixteen-handle budget. Their retry
      owner is transport/context scoped; the display actor remains readable.
      Context is an immutable reference, never a pointer into a review session. }
    function Submit(const AActor: TNyxText; const AArguments: TNyxDataValue;
      const APair: TNyxProjectPair; const AReview: TNyxReviewRef;
      const ARetryOwner: TNyxText): TNyxDataValue; overload;
    { User project identity is captured beside review identity. The two scopes
      are mutually exclusive and remain immutable through editor navigation. }
    function Submit(const AActor: TNyxText; const AArguments: TNyxDataValue;
      const APair: TNyxProjectPair; const AReview: TNyxReviewRef;
      const ARetryOwner: TNyxText; const AWorkspace: TNyxWorkspaceRef): TNyxDataValue; overload;
    function Context(const AJob: TNyxText): TNyxReviewRef;
    function WorkspaceContext(const AJob: TNyxText): TNyxWorkspaceRef;
    { Bounded immutable results; no source, compiler log or machine profile dump.
      Current-source and navigation flags are added by the model-owning transport. }
    function Status(const AArguments: TNyxDataValue;
      out APair: TNyxProjectPair; out ACurrentOutput: Boolean): TNyxDataValue;
    { Drains terminal notifications once. Caller publishes under its document
      lock only if Pair is still exact. Stale jobs remain queryable independently. }
    function TakeCompletion(out AActor, AOutcome: TNyxText;
      out APair: TNyxProjectPair; out AReport: INyxCompilerReport): Boolean; overload;
    function TakeCompletion(out AActor, AOutcome: TNyxText;
      out APair: TNyxProjectPair; out AReport: INyxCompilerReport;
      out AReview: TNyxReviewRef): Boolean; overload;
    function TakeCompletion(out AActor, AOutcome: TNyxText;
      out APair: TNyxProjectPair; out AReport: INyxCompilerReport;
      out AReview: TNyxReviewRef; out AWorkspace: TNyxWorkspaceRef): Boolean; overload;
    { Completion publication must admit both its accepted pair and its captured
      output. A stale machine profile cannot replace the last accepted report. }
    function TakeCompletion(out AActor, AOutcome: TNyxText;
      out APair: TNyxProjectPair; out AReport: INyxCompilerReport;
      out AReview: TNyxReviewRef; out AWorkspace: TNyxWorkspaceRef;
      out ACurrentOutput: Boolean): Boolean; overload;
  end;

{ Optimistic byte fingerprint, explicitly MD5 rather than an authentication
  credential. Model currentness and retry identity compare complete exact text,
  never hashes. Buffer overload avoids Windows ANSI conversion of UTF-8. }
function NyxBuildFingerprint(const AText: TNyxText): TNyxText;

implementation

uses
  md5, fpjson, nyx.model, nyx.codec, nyx.studio.outputs,
  nyx.studio.agents, nyx.studio.buildexecutor, nyx.editing,
  nyx.source, nyx.studio.sourcebuilds, nyx.studio.editorbuild;

type
  TBuildJob = class;
  TBuildWorker = class(TThread)
  private
    FJob: TBuildJob;
  protected
    procedure Execute; override;
  public
    constructor Create(AJob: TBuildJob);
    destructor Destroy; override;
  end;

  TBuildJob = class
  public
    Purpose: TNyxCompilerJobPurpose;
    Source: TNyxText;
    SourceBuild: INyxSourceProjectionBuild;
    { Retained within the same sixteen-job budget. Captured before delegation;
      a successful receipt is exact retry identity, never another history step. }
    SourcePublication: TNyxStudioSourcePublication;
    SourceProducerText: TNyxText;
    SourcePublicationReceipt: TNyxDataValue;
    SourcePublished: Boolean;
    { Whole source-job lease includes queue time. Expired queued work never
      starts on a later poll; running work cancels and retains its slot to join. }
    SourceDeadline: QWord;
    ID: TNyxText;
    Actor: TNyxText;
    Owner: TNyxText;
    Review: TNyxReviewRef;
    Workspace: TNyxWorkspaceRef;
    Directories: TNyxStudioDirectories;
    Profile: TNyxText;
    Pair: TNyxProjectPair;
    Arguments: TNyxDataValue;
    Guard: TCriticalSection;
    Worker: TBuildWorker;
    State: TNyxBuildJobState;
    CompletedState: TNyxBuildJobState;
    Failure: TNyxCompilerFailure;
    Cancellation: INyxBuildCancellation;
    Error: TNyxText;
    Output: TNyxDataValue;
    Manifest: TNyxDataValue;
    Report: INyxCompilerReport;
    Announced: Boolean;
    constructor Create;
    destructor Destroy; override;
    function Terminal: Boolean;
    function SourceSnapshot: TNyxDataValue;
    function Snapshot(AOffset, ALimit: Integer;
      const ASeverity: TNyxText = 'all'): TNyxDataValue;
  end;

function NyxBuildFingerprint(const AText: TNyxText): TNyxText;
var
  LEmpty: Byte;
  LBytes: TNyxText;
begin
  LEmpty := 0;

  if AText = '' then
  begin
    Exit(MD5Print(MD5Buffer(LEmpty, 0)));
  end;
  LBytes := AText;
  Result := MD5Print(MD5Buffer(LBytes[1], Length(LBytes)));
end;

function BoundedText(const AText: TNyxText; AMaximum: Integer): TNyxText;
var
  LIndex: Integer;
  LCount: Integer;
  LScalar: Integer;
begin
  LIndex := 1;
  LCount := 0;
  while (LIndex <= Length(AText)) and (LCount < AMaximum) do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Compiler context contains malformed Unicode');
    end;
    Inc(LCount);
  end;
  Result := Copy(AText, 1, LIndex - 1);
end;

function ReadBytes(const APath: TNyxText): TNyxText;
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

function FingerprintFile(const ARoot, ARelative: TNyxText): TNyxDataValue;
var
  LText: TNyxText;
begin
  LText := ReadBytes(ARoot + ARelative);
  Result := NyxObject([NyxField('path', NyxData('builds/' + ARelative)),
    NyxField('bytes', NyxData(Length(LText))),
    NyxField('md5', NyxData(NyxBuildFingerprint(LText)))]);
end;

constructor TBuildJob.Create;
begin
  inherited Create;
  Guard := TCriticalSection.Create;
  State := bjsQueued;
  CompletedState := bjsFailed;
  Cancellation := NewNyxBuildCancellation;
  Output := NyxNull;
  Manifest := NyxArray([]);
end;

destructor TBuildJob.Destroy;
begin
  { The owner joins before releasing immutable inputs/guard. No forced thread
    termination or callbacks into a destroyed Studio session can occur. }
  if Cancellation <> nil then
  begin
    Cancellation.Cancel;
  end;
  Worker.Free;
  Guard.Free;
  inherited Destroy;
end;

function TBuildJob.Terminal: Boolean;
begin
  Guard.Acquire;
  try
    Result := NyxBuildJobTerminal(State);
  finally
    Guard.Release;
  end;
end;

function TBuildJob.SourceSnapshot: TNyxDataValue;
var
  LReceipt: TNyxDataValue;
begin
  Guard.Acquire;
  try
    LReceipt := NyxNull;

    if NyxBuildJobTerminal(State) and (State <> bjsCancelled) and
      (SourceBuild <> nil) then
    begin
      LReceipt := EncodeNyxBrowserSourceBuild(SourceBuild);
    end;
    Result := NyxObject([NyxField('job', NyxData(ID)),
      NyxField('state', NyxData(NyxBuildJobStateName(State))),
      NyxField('receipt', LReceipt), NyxField('error', NyxData(BoundedText(Error, 1024)))]);
  finally
    Guard.Release;
  end;
end;

function TBuildJob.Snapshot(AOffset, ALimit: Integer;
  const ASeverity: TNyxText): TNyxDataValue;
var
  LPublicManifest: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LItem: TNyxCompilerDiagnostic;
  LIndex: Integer;
  LTotal: Integer;
  LCount: Integer;
  LArtifact: TNyxText;
  LSource: TNyxText;
  LView: TNyxText;
  LOrder: TNyxCompilerDiagnosticIndices;
  LFiltered: TNyxCompilerDiagnosticIndices;
begin

  if Purpose = cjpSourceProjection then
  begin
    Exit(SourceSnapshot);
  end;
  Guard.Acquire;
  try
    LTotal := 0;
    LArtifact := '';
    LSource := '';
    LView := '';

    if NyxAgentHas(Arguments, 'view') then
    begin
      LView := Arguments.Field('view').AsText;
    end;

    if Report <> nil then
    begin
      LTotal := Report.Count;
    end;

    if Output.Kind = ndObject then
    begin
      LSource := Output.Field('source').AsText;

      if State = bjsSucceeded then
      begin
        LArtifact := Output.Field('artifact').AsText;
      end;
    end;
    LCount := 0;
    LOrder := nil;

    if NyxBuildJobTerminal(State) then
    begin
      LOrder := NyxCompilerDiagnosticOrder(Report);
    end;
    SetLength(LFiltered, Length(LOrder));
    for LIndex := 0 to High(LOrder) do
    begin

      if (ASeverity = 'all') or
        (LowerCase(NyxCompilerSeverityName(Report.Item(LOrder[LIndex]).Severity)) = ASeverity) then
      begin
        LFiltered[LCount] := LOrder[LIndex];
        Inc(LCount);
      end;
    end;
    LTotal := LCount;
    LCount := 0;
    SetLength(LItems, ALimit);
    for LIndex := AOffset to LTotal - 1 do
    begin

      if LCount = ALimit then
      begin
        Break;
      end;
      LItem := Report.Item(LFiltered[LIndex]);
      LItems[LCount] := NyxObject([
        NyxField('file', NyxData(BoundedText(LItem.FileName, 512))),
        NyxField('severity', NyxData(LowerCase(NyxCompilerSeverityName(LItem.Severity)))),
        NyxField('message', NyxData(BoundedText(LItem.Message, 1024))),
        NyxField('line', NyxData(LItem.SourceLine)),
        NyxField('column', NyxData(LItem.SourceColumn)),
        NyxField('mapped', NyxData(LItem.Navigable))]);
      Inc(LCount);
    end;
    SetLength(LItems, LCount);
    LPublicManifest := NyxArray([]);

    if State = bjsSucceeded then
    begin
      LPublicManifest := Manifest;
    end;
    Result := NyxObject([NyxField('job', NyxData(ID)),
      NyxField('state', NyxData(NyxBuildJobStateName(State))),
      NyxField('failure', NyxData(NyxCompilerFailureName(Failure))),
      NyxField('revision', Arguments.Field('expectedRevision')),
      NyxField('target', Arguments.Field('target')),
      NyxField('scope', Arguments.Field('scope')),
      NyxField('view', NyxData(LView)),
      NyxField('sourceFingerprint', NyxData(NyxBuildFingerprint(Pair.Source))),
      NyxField('designFingerprint', NyxData(NyxBuildFingerprint(Pair.Design))),
      NyxField('outputID', NyxData(NyxBuildFingerprint(Profile))),
      NyxField('fingerprintAlgorithm', NyxData('md5')),
      NyxField('error', NyxData(BoundedText(Error, 1024))),
      NyxField('artifact', NyxData(LArtifact)),
      NyxField('compiledSource', NyxData(LSource)),
      NyxField('manifest', LPublicManifest),
      NyxField('diagnostics', NyxObject([
        NyxField('order', NyxData('severity')),
        NyxField('severity', NyxData(ASeverity)),
        NyxField('available', NyxData(Length(LOrder))),
        NyxField('offset', NyxData(AOffset)), NyxField('total', NyxData(LTotal)),
        NyxField('items', NyxArray(LItems))]))]);


  finally
    Guard.Release;
  end;
end;

constructor TBuildWorker.Create(AJob: TBuildJob);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FJob := AJob;
end;

destructor TBuildWorker.Destroy;
begin

  if Suspended then
  begin
    Terminate;
    Start;
  end;
  inherited Destroy;
end;

procedure TBuildWorker.Execute;
var
  LExecutor: TNyxBuildExecutor;
  LDocument: TNyxDocument;
  LResult: TJSONObject;
  LOutput: TNyxDataValue;
  LManifest: TNyxDataValue;
  LScope: TNyxText;
  LView: TNyxText;
  LRoot: TNyxText;
  LDirectory: TNyxText;
  LReport: INyxCompilerReport;
  LSourceBuild: INyxSourceProjectionBuild;
begin
  LExecutor := nil;
  LDocument := nil;
  LResult := nil;

  if Terminated then
  begin
    Exit;
  end;
  try
    try
      LExecutor := TNyxBuildExecutor.Create(FJob.Directories, FJob.Profile);

      if FJob.Purpose = cjpSourceProjection then
      begin
        LSourceBuild := LExecutor.ProjectSource(FJob.Source,
          NyxPascalUnit(NyxCompanionUnitName(FJob.Source)), btBrowser,
          spcDefault, FJob.Cancellation);
        FJob.Guard.Acquire;
        try

          if FJob.Cancellation.Cancelled then
          begin
            FJob.CompletedState := bjsCancelled;
          end
          else
          begin
            FJob.SourceBuild := LSourceBuild;
            FJob.CompletedState := bjsFailed;

            if LSourceBuild.Projection.State = spsCompiled then
            begin
              FJob.CompletedState := bjsSucceeded;
            end;
          end;
        finally
          FJob.Guard.Release;
        end;
        Exit;
      end;
      LDocument := TNyxCodec.Decode(FJob.Pair.Design);
      LScope := FJob.Arguments.Field('scope').AsText;
      LView := '';

      if NyxAgentHas(FJob.Arguments, 'view') then
      begin
        LView := FJob.Arguments.Field('view').AsText;
      end;

      if LScope = 'reusable' then
      begin
        LScope := 'view';
      end;
      LResult := LExecutor.Build(LDocument, FJob.Arguments.Field('target').AsText,
        LScope, LView, FJob.Pair.Source, FJob.Cancellation);
      LOutput := TNyxDataValue.ParseJSON(TNyxText(LResult.AsJSON));
      LReport := DecodeNyxCompilerReport(LOutput.Field('diagnostics').ToJSON);
      LRoot := FJob.Directories.Jobs;
      LDirectory := LOutput.Field('build').AsText + '/';
      LManifest := NyxArray([]);

      if LOutput.Field('ok').AsBoolean then
      begin

        if FJob.Arguments.Field('target').AsText = 'browser' then
        begin
          LManifest := NyxArray([
            FingerprintFile(LRoot, LDirectory + 'index.html'),
            FingerprintFile(LRoot, LDirectory + 'nyx_preview.js'),
            FingerprintFile(LRoot, LDirectory + 'rtl.js'),
            FingerprintFile(LRoot, Copy(LOutput.Field('source').AsText, 8, MaxInt)),
            FingerprintFile(LRoot, LDirectory + 'design.nyx')]);
        end
        else
        begin
          LManifest := NyxArray([
            FingerprintFile(LRoot, LDirectory + 'nyx_native.exe'),
            FingerprintFile(LRoot, Copy(LOutput.Field('source').AsText, 8, MaxInt)),
            FingerprintFile(LRoot, LDirectory + 'design.nyx')]);
        end;
      end;
      FJob.Guard.Acquire;
      try
        { The serialized owner can request cancellation after the child exits
          but before publication. That race still retires the whole job and
          never replaces the accepted report or advertises a late artifact. }

        if FJob.Cancellation.Cancelled then
        begin
          FJob.CompletedState := bjsCancelled;
        end
        else if LOutput.Field('ok').AsBoolean then
        begin
          FJob.Output := LOutput;
          FJob.Manifest := LManifest;
          FJob.Report := LReport;
          FJob.CompletedState := bjsSucceeded;
        end
        else
        begin
          FJob.Output := LOutput;
          FJob.Report := LReport;
          FJob.Failure := ParseNyxCompilerFailure(LOutput.Field('failure').AsText);
          case FJob.Failure of
            bcfTimeBudget:
              begin
                FJob.Error := 'Compiler time budget exceeded';
              end;
            bcfLogBudget:
              begin
                FJob.Error := 'Compiler log budget exceeded';
              end;
          else
            begin
              FJob.Error := 'Compiler refused the accepted companion; inspect bounded diagnostics';
            end;
          end;
          FJob.CompletedState := bjsFailed;
        end;
      finally
        FJob.Guard.Release;
      end;
    except
      on LException: Exception do
      begin
        FJob.Guard.Acquire;
        try
          if FJob.Cancellation.Cancelled then
          begin
            FJob.CompletedState := bjsCancelled;
          end
          else
          begin
            FJob.Error := LException.Message;
            FJob.CompletedState := bjsFailed;
          end;
        finally
          FJob.Guard.Release;
        end;
      end;
    end;
  finally
    LResult.Free;
    LDocument.Free;
    LExecutor.Free;
  end;
end;

constructor TNyxBuildJobs.Create(const ARepository, AProfile: TNyxText);
begin
  Create(TNyxStudioDirectories.ForRepository(ARepository), AProfile);
end;

constructor TNyxBuildJobs.Create(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText);
begin
  inherited Create;
  ADirectories.Validate;
  FDirectories := ADirectories;
  FJobs := TList.Create;
  FSourceLeaseMS := 120000;
  Configure(AProfile);
  { Prepare the shared parent on the serialized owner before workers start.
    Older FPC ForceDirectories can race while recursively creating that parent;
    each worker subsequently creates only its unique invocation directory. }

  if not ForceDirectories(FDirectories.Jobs) then
  begin
    raise ENyxModel.Create('Cannot prepare the compiler artifact root');
  end;
end;

destructor TNyxBuildJobs.Destroy;
var
  LIndex: Integer;
begin

  if FJobs <> nil then
  begin
    { Signal every running job before joining any one. Queued jobs own no
      worker/process and cannot start during teardown. }
    for LIndex := 0 to FJobs.Count - 1 do
    begin
      TBuildJob(FJobs[LIndex]).Cancellation.Cancel;
    end;
    for LIndex := 0 to FJobs.Count - 1 do
    begin
      TBuildJob(FJobs[LIndex]).Free;
    end;
  end;
  FJobs.Free;
  inherited Destroy;
end;

procedure TNyxBuildJobs.Pump;
var
  LIndex: Integer;
  LActive: Integer;
  LJob: TBuildJob;
begin
  LActive := 0;
  for LIndex := 0 to FJobs.Count - 1 do
  begin
    LJob := TBuildJob(FJobs[LIndex]);

    if (LJob.Purpose = cjpSourceProjection) and not LJob.Terminal and
      (GetTickCount64 >= LJob.SourceDeadline) then
    begin
      LJob.Cancellation.Cancel;

      if LJob.State = bjsQueued then
      begin
        LJob.State := bjsCancelled;
      end
      else
      begin
        LJob.State := bjsCancelling;
      end;
    end;

    if LJob.Worker <> nil then
    begin

      if LJob.Worker.Finished then
      begin
        { Finished is only a hint. WaitFor joins the actual OS thread before
          the terminal state becomes visible or another compiler takes a slot. }
        LJob.Worker.WaitFor;
        FreeAndNil(LJob.Worker);
        LJob.Guard.Acquire;
        try

          if LJob.Cancellation.Cancelled then
          begin
            LJob.State := bjsCancelled;
            LJob.Output := NyxNull;
            LJob.Manifest := NyxArray([]);
            LJob.Report := nil;
            LJob.SourceBuild := nil;
            LJob.Failure := bcfNone;
            LJob.Error := '';
          end
          else
          begin
            LJob.State := LJob.CompletedState;
          end;
        finally
          LJob.Guard.Release;
        end;
      end
      else
      begin
        Inc(LActive);
      end;
    end;
  end;
  for LIndex := 0 to FJobs.Count - 1 do
  begin
    LJob := TBuildJob(FJobs[LIndex]);

    if (LActive < 2) and (LJob.State = bjsQueued) then
    begin
      try
        { Allocation and OS-thread creation belong to the same retirement
          boundary as Start. A failed constructor cannot strand a queued job. }
        LJob.Worker := TBuildWorker.Create(LJob);
        LJob.State := bjsRunning;
        LJob.Worker.Start;
        Inc(LActive);
      except
        LJob.Cancellation.Cancel;
        FreeAndNil(LJob.Worker);
        LJob.State := bjsFailed;
        LJob.Error := 'Cannot start the owned compiler worker';
      end;
    end;
  end;
end;

procedure TNyxBuildJobs.Configure(const AProfile: TNyxText);
var
  LProfile: TNyxOutputConfiguration;
begin
  LProfile := TNyxOutputConfiguration.Decode(AProfile);
  try
    FProfile := LProfile.Encode;
  finally
    LProfile.Free;
  end;
end;

function TNyxBuildJobs.Outputs: TNyxDataValue;
var
  LExecutor: TNyxBuildExecutor;
  LItems: array[TNyxBuildTarget] of TNyxDataValue;
  LTarget: TNyxBuildTarget;
  LIssue: TNyxText;
begin
  LExecutor := TNyxBuildExecutor.Create(FDirectories, FProfile);
  try
    for LTarget := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
    begin
      LIssue := LExecutor.Readiness(NyxBuildTargetName(LTarget));
      LItems[LTarget] := NyxObject([
        NyxField('target', NyxData(NyxBuildTargetName(LTarget))),
        NyxField('ready', NyxData(LIssue = '')),
        NyxField('issue', NyxData(LIssue))]);
    end;
    Result := NyxObject([NyxField('outputID', NyxData(NyxBuildFingerprint(FProfile))),
      NyxField('outputs', NyxArray([LItems[btBrowser], LItems[btNativeLCL]]))]);
  finally
    LExecutor.Free;
  end;
end;

function TNyxBuildJobs.OperatorProfile: TNyxText;
begin
  Result := FProfile;
end;

function TNyxBuildJobs.List(const AArguments: TNyxDataValue;
  const AReview: TNyxReviewRef; const AWorkspace: TNyxWorkspaceRef;
  const AOwner: TNyxText; AOperator, ACanCancel: Boolean;
  ACurrent: TNyxBuildPairCurrent): TNyxDataValue;
var
  LIndex: Integer;
  LOffset: Integer;
  LLimit: Integer;
  LTotal: Integer;
  LCount: Integer;
  LQueued: Integer;
  LRunning: Integer;
  LCancelling: Integer;
  LActiveOnly: Boolean;
  LJob: TBuildJob;
  LView: TNyxText;
  LCurrent: Boolean;
  LItems: array of TNyxDataValue;
begin
  NyxAgentFields(AArguments, '|mode|filter|offset|limit|');
  LActiveOnly := True;
  LOffset := 0;
  LLimit := 10;

  if NyxAgentHas(AArguments, 'filter') then
  begin

    if (AArguments.Field('filter').AsText <> 'active') and
      (AArguments.Field('filter').AsText <> 'all') then
    begin
      raise ENyxModel.Create('Compiler job filter is active or all');
    end;
    LActiveOnly := AArguments.Field('filter').AsText = 'active';
  end;

  if NyxAgentHas(AArguments, 'offset') then
  begin
    LOffset := AArguments.Field('offset').AsInteger;
  end;

  if NyxAgentHas(AArguments, 'limit') then
  begin
    LLimit := AArguments.Field('limit').AsInteger;
  end;

  if (LOffset < 0) or (LOffset > 16) or (LLimit < 1) or (LLimit > 16) or
    not Assigned(ACurrent) then
  begin
    raise ENyxModel.Create('Compiler job window is offset 0..16, limit 1..16');
  end;
  Pump;
  LTotal := 0;
  LCount := 0;
  LQueued := 0;
  LRunning := 0;
  LCancelling := 0;
  SetLength(LItems, LLimit);
  for LIndex := 0 to FJobs.Count - 1 do
  begin
    LJob := TBuildJob(FJobs[LIndex]);

    if (LJob.Purpose <> cjpApplication) or
      (LJob.Review.ID <> AReview.ID) or (LJob.Workspace.ID <> AWorkspace.ID) then
    begin
      Continue;
    end;
    LJob.Guard.Acquire;
    try
      case LJob.State of
        bjsQueued:
          begin
            Inc(LQueued);
          end;
        bjsRunning:
          begin
            Inc(LRunning);
          end;
        bjsCancelling:
          begin
            Inc(LCancelling);
          end;
        bjsSucceeded, bjsFailed, bjsCancelled:
          begin
            { Retained terminal metadata contributes only to the all filter. }
          end;
      end;

      if LActiveOnly and NyxBuildJobTerminal(LJob.State) then
      begin
        Continue;
      end;
      Inc(LTotal);

      if (LTotal <= LOffset) or (LCount = LLimit) then
      begin
        Continue;
      end;
      LView := '';

      if NyxAgentHas(LJob.Arguments, 'view') then
      begin
        LView := LJob.Arguments.Field('view').AsText;
      end;
      LCurrent := ACurrent(LJob.Pair);
      LItems[LCount] := NyxObject([
        NyxField('job', NyxData(LJob.ID)), NyxField('actor', NyxData(BoundedText(LJob.Actor, 120))),
        NyxField('state', NyxData(NyxBuildJobStateName(LJob.State))),
        NyxField('failure', NyxData(NyxCompilerFailureName(LJob.Failure))),
        NyxField('revision', LJob.Arguments.Field('expectedRevision')),
        NyxField('target', LJob.Arguments.Field('target')),
        NyxField('scope', LJob.Arguments.Field('scope')), NyxField('view', NyxData(LView)),
        NyxField('currentSource', NyxData(LCurrent)),
        NyxField('currentOutput', NyxData(LJob.Profile = FProfile)),
        NyxField('canCancel', NyxData(ACanCancel and (AOperator or (LJob.Owner = AOwner)) and
          (LJob.State in [bjsQueued, bjsRunning])))]);
      Inc(LCount);
    finally
      LJob.Guard.Release;
    end;
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
    NyxField('queued', NyxData(LQueued)), NyxField('running', NyxData(LRunning)),
    NyxField('cancelling', NyxData(LCancelling)), NyxField('items', NyxArray(LItems))]);
end;

procedure TNyxBuildJobs.AdmitRequest(const AArguments: TNyxDataValue);
var
  LScope: TNyxBuildScope;
  LID: TNyxText;
begin
  NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|outputID|target|scope|view|');
  AArguments.Field('expectedRevision').AsInteger;
  ParseNyxBuildTarget(AArguments.Field('target').AsText);
  LScope := ParseNyxBuildScope(AArguments.Field('scope').AsText);
  LID := AArguments.Field('operationId').AsText;

  if (NyxTextScalarCount(LID) < 1) or (NyxTextScalarCount(LID) > 120) or
    (Pos(#0, LID) > 0) or (Pos(#10, LID) > 0) or (Pos(#13, LID) > 0) then
  begin
    raise ENyxModel.Create('Build operationId must contain 1..120 characters');
  end;

  if Length(AArguments.Field('outputID').AsText) <> 32 then
  begin
    raise ENyxModel.Create('Inspect nyx_build outputs for the exact outputID');
  end;

  if LScope = bsApplication then
  begin

    if NyxAgentHas(AArguments, 'view') then
    begin
      raise ENyxModel.Create('Application builds omit view');
    end;
  end
  else
  begin

    if AArguments.Field('view').AsText = '' then
    begin
      raise ENyxModel.Create('View/reusable builds require an exact root ID');
    end;
  end;
end;

function ReceiptKey(const AActor: TNyxText; const AArguments: TNyxDataValue): TNyxText;
begin
  Result := NyxObject([NyxField('actor', NyxData(AActor)),
    NyxField('id', AArguments.Field('operationId'))]).ToJSON;
end;

function TNyxBuildJobs.Retry(const AActor: TNyxText;
  const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
var
  LIndex: Integer;
  LKey: TNyxText;
begin
  Result := False;
  AReceipt := NyxNull;
  LKey := ReceiptKey(AActor, AArguments);
  for LIndex := 0 to High(FReceiptKeys) do
  begin

    if FReceiptKeys[LIndex] = LKey then
    begin

      if FReceiptRequests[LIndex] <> AArguments.ToJSON then
      begin
        raise ENyxModel.Create('Build operationId was already used with different arguments');
      end;
      AReceipt := FReceipts[LIndex];
      Exit(True);
    end;
  end;
end;

function TNyxBuildJobs.Submit(const AActor: TNyxText;
  const AArguments: TNyxDataValue; const APair: TNyxProjectPair): TNyxDataValue;
begin
  Result := Submit(AActor, AArguments, APair, NyxActiveWorkspace, AActor);
end;

function TNyxBuildJobs.Context(const AJob: TNyxText): TNyxReviewRef;
var
  LIndex: Integer;
begin
  for LIndex := 0 to FJobs.Count - 1 do
  begin

    if TBuildJob(FJobs[LIndex]).ID = AJob then
    begin
      Exit(TBuildJob(FJobs[LIndex]).Review);
    end;
  end;
  raise ENyxModel.Create('Unknown or expired build job; no context fallback');
end;

function TNyxBuildJobs.Submit(const AActor: TNyxText;
  const AArguments: TNyxDataValue; const APair: TNyxProjectPair;
  const AReview: TNyxReviewRef; const ARetryOwner: TNyxText): TNyxDataValue;
begin
  Result := Submit(AActor, AArguments, APair, AReview, ARetryOwner, NyxPrimaryWorkspace);
end;

function TNyxBuildJobs.WorkspaceContext(const AJob: TNyxText): TNyxWorkspaceRef;
var
  LIndex: Integer;
begin
  for LIndex := 0 to FJobs.Count - 1 do
  begin

    if TBuildJob(FJobs[LIndex]).ID = AJob then
    begin
      Exit(TBuildJob(FJobs[LIndex]).Workspace);
    end;
  end;
  raise ENyxModel.Create('Unknown or expired build job; no project context fallback');
end;

function TNyxBuildJobs.Submit(const AActor: TNyxText;
  const AArguments: TNyxDataValue; const APair: TNyxProjectPair;
  const AReview: TNyxReviewRef; const ARetryOwner: TNyxText;
  const AWorkspace: TNyxWorkspaceRef): TNyxDataValue;
var
  LExecutor: TNyxBuildExecutor;
  LIssue: TNyxText;
begin

  if (AReview.ID <> '') and (AWorkspace.ID <> '') then
  begin
    raise ENyxModel.Create('Compiler jobs require one exact project or review context');
  end;

  if AArguments.Field('outputID').AsText <> NyxBuildFingerprint(FProfile) then
  begin
    raise ENyxModel.Create('Output configuration changed; inspect readiness before submitting');
  end;
  LExecutor := TNyxBuildExecutor.Create(FDirectories, FProfile);
  try
    LIssue := LExecutor.Readiness(AArguments.Field('target').AsText);
  finally
    LExecutor.Free;
  end;

  if LIssue <> '' then
  begin
    raise ENyxModel.Create(LIssue);
  end;
  Result := Enqueue(AActor, AArguments, APair, AReview, ARetryOwner,
    AWorkspace, cjpApplication, '');
end;

function TNyxBuildJobs.Enqueue(const AActor: TNyxText;
  const AArguments: TNyxDataValue; const APair: TNyxProjectPair;
  const AReview: TNyxReviewRef; const AOwner: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; APurpose: TNyxCompilerJobPurpose;
  const ASource: TNyxText): TNyxDataValue;
begin
  Result := EnqueueCaptured(AActor, AArguments, APair, AReview, AOwner,
    AWorkspace, APurpose, ASource, Default(TNyxStudioSourcePublication));
end;

function TNyxBuildJobs.EnqueueCaptured(const AActor: TNyxText;
  const AArguments: TNyxDataValue; const APair: TNyxProjectPair;
  const AReview: TNyxReviewRef; const AOwner: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; APurpose: TNyxCompilerJobPurpose;
  const ASource: TNyxText; const APublication: TNyxStudioSourcePublication): TNyxDataValue;
var
  LJob: TBuildJob;
  LActive: Integer;
  LEvict: Integer;
  LIndex: Integer;
  LID: TGUID;
begin
  Pump;
  LActive := 0;
  LEvict := -1;
  for LIndex := 0 to FJobs.Count - 1 do
  begin

    if not TBuildJob(FJobs[LIndex]).Terminal then
    begin
      Inc(LActive);
    end
    else if LEvict < 0 then
    begin
      LEvict := LIndex;
    end;
  end;

  if LActive >= 10 then
  begin
    raise ENyxModel.Create('Two compiler slots and eight queued jobs are full; query or cancel owned jobs');
  end;

  if FJobs.Count = 16 then
  begin
    TBuildJob(FJobs[LEvict]).Free;
    FJobs.Delete(LEvict);
  end;
  LJob := TBuildJob.Create;
  try
    CreateGUID(LID);
    LJob.ID := Copy(GUIDToString(LID), 2, 36);
    LJob.Actor := AActor;
    LJob.Owner := AOwner;
    LJob.Purpose := APurpose;
    LJob.Source := ASource;
    LJob.SourcePublication := APublication;

    if APurpose = cjpSourceProjection then
    begin
      LJob.SourceDeadline := GetTickCount64 + QWord(FSourceLeaseMS);
    end;
    LJob.Review := AReview;
    LJob.Workspace := AWorkspace;
    LJob.Directories := FDirectories;
    LJob.Profile := FProfile;
    LJob.Arguments := AArguments.Copy;
    LJob.Pair := APair;
    Result := LJob.Snapshot(0, 1);
    { Admission owns immutable queued input before delegation. The original
      receipt remains queued even when the slot starts immediately; use status
      for current state. No retry launches a second worker. }
    FJobs.Add(LJob);
    LJob := nil;
  finally
    LJob.Free;
  end;
  Remember(AOwner, AArguments, Result);
  Pump;
end;

procedure TNyxBuildJobs.AdmitSourceRequest(const AArguments: TNyxDataValue);
var
  LSource: TNyxText;
  LOperation: TNyxText;
begin
  NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|source|publish|issuer|');

  if AArguments.Field('mode').AsText <> 'request' then
  begin
    raise ENyxModel.Create('Request source compilation with its closed request mode');
  end;
  AArguments.Field('expectedRevision').AsInteger;
  { Private wire booleans express an explicit closed intent. Only shared requests
    carry the claimed server identity; neither member is an execution flag. }

  if NyxAgentHas(AArguments, 'publish') then
  begin

    if AArguments.Field('publish').AsBoolean then
    begin

      if (AArguments.Field('issuer').AsText = '') or
        (Length(AArguments.Field('issuer').AsText) > 128) then
      begin
        raise ENyxModel.Create('Shared source compilation requires its owning server identity');
      end;
    end
    else if NyxAgentHas(AArguments, 'issuer') then
    begin
      raise ENyxModel.Create('Compile-only source requests have no publication issuer');
    end;
  end
  else if NyxAgentHas(AArguments, 'issuer') then
  begin
    raise ENyxModel.Create('Only shared source compilation carries a publication issuer');
  end;
  LSource := AArguments.Field('source').AsText;
  ValidateNyxProjectionSource(LSource);
  NyxCompanionUnitName(LSource);
  LOperation := AArguments.Field('operationId').AsText;

  if (NyxTextScalarCount(LOperation) < 1) or (NyxTextScalarCount(LOperation) > 120) or
    (Pos(#0, LOperation) > 0) or (Pos(#10, LOperation) > 0) or (Pos(#13, LOperation) > 0) then
  begin
    raise ENyxModel.Create('Source compilation operation requires 1..120 characters');
  end;

end;

procedure TNyxBuildJobs.ConfigureSourceLease(AMilliseconds: Integer);
begin

  if (AMilliseconds < 1) or (AMilliseconds > 120000) then
  begin
    raise ENyxModel.Create('Source compiler whole-job budget requires 1..120000 milliseconds');
  end;
  FSourceLeaseMS := AMilliseconds;
end;

function TNyxBuildJobs.RequestSource(const AArguments: TNyxDataValue;
  const APair: TNyxProjectPair; const AWorkspace: TNyxWorkspaceRef;
  const AOwner: TNyxText): TNyxDataValue;
begin
  Result := RequestSource(AArguments, APair, AWorkspace, AOwner,
    Default(TNyxStudioSourcePublication));
end;

function TNyxBuildJobs.RequestSource(const AArguments: TNyxDataValue;
  const APair: TNyxProjectPair; const AWorkspace: TNyxWorkspaceRef;
  const AOwner: TNyxText; const APublication: TNyxStudioSourcePublication): TNyxDataValue;
begin
  AdmitSourceRequest(AArguments);

  if (NyxAgentHas(AArguments, 'publish') and
    AArguments.Field('publish').AsBoolean) <> APublication.IsCaptured then
  begin
    raise ENyxModel.Create('Shared source compilation requires its precompile publication capture');
  end;

  if APublication.IsCaptured and
    ((APublication.Source <> AArguments.Field('source').AsText) or
    (APublication.Workspace.ID <> AWorkspace.ID) or
    (APublication.Revision <> AArguments.Field('expectedRevision').AsInteger) or
    (EncodeNyxProject(APublication.Baseline) <> EncodeNyxProject(APair))) then
  begin
    raise ENyxModel.Create('Shared source job differs from its complete captured editor pair');
  end;

  if Retry(AOwner, AArguments, Result) then
  begin
    Exit;
  end;
  { Readiness belongs to the actual job, not designer launch or target choice.
    ProjectSource returns a typed unavailable receipt for missing tools. }
  Result := EnqueueCaptured('Studio', AArguments, APair, NyxActiveWorkspace,
    AOwner, AWorkspace, cjpSourceProjection, AArguments.Field('source').AsText, APublication);
end;

function TNyxBuildJobs.SourcePublicationIndex(const AReference: TNyxSourceProjectionRef;
  const AWorkspace: TNyxWorkspaceRef; const AOwner: TNyxText): Integer;
var
  LIndex: Integer;
  LJob: TBuildJob;
begin
  for LIndex := 0 to FJobs.Count - 1 do
  begin
    LJob := TBuildJob(FJobs[LIndex]);
    LJob.Guard.Acquire;
    try

      if (LJob.Purpose = cjpSourceProjection) and (LJob.SourceBuild <> nil) and
        (LJob.SourceBuild.Reference.Name = AReference.Name) and
        (LJob.Workspace.ID = AWorkspace.ID) and (LJob.Owner = AOwner) then
      begin
        Exit(LIndex);
      end;
    finally
      LJob.Guard.Release;
    end;
  end;
  raise ENyxProjectConflict.Create('Source publication producer is missing, retired or owned by another project');
end;

function TNyxBuildJobs.PrepareSourcePublication(const AReference: TNyxSourceProjectionRef;
  const AWorkspace: TNyxWorkspaceRef; const AOwner, AIssuer, AProducerText: TNyxText;
  out APublication: TNyxStudioSourcePublication; out ABuild: INyxSourceProjectionBuild;
  out AReceipt: TNyxDataValue): Boolean;
var
  LJob: TBuildJob;
begin
  Pump;
  LJob := TBuildJob(FJobs[SourcePublicationIndex(AReference, AWorkspace, AOwner)]);

  if not LJob.SourcePublication.IsCaptured or not LJob.Terminal or
    (LJob.Worker <> nil) or (LJob.State <> bjsSucceeded) or
    (LJob.SourceBuild.Projection.State <> spsCompiled) then
  begin
    raise ENyxProjectConflict.Create('Only a joined successful shared source job can publish construction');
  end;

  if LJob.SourcePublished then
  begin

    if LJob.SourceProducerText <> AProducerText then
    begin
      raise ENyxProjectConflict.Create('Source completion retry changed its original producer result');
    end;
    APublication := LJob.SourcePublication;
    ABuild := LJob.SourceBuild;
    AReceipt := LJob.SourcePublicationReceipt;
    Exit(True);
  end;

  if (LJob.Profile <> FProfile) or
    (LJob.SourcePublication.Revision = High(Integer)) then
  begin
    raise ENyxProjectConflict.Create('Source publication output or revision is no longer available');
  end;
  APublication := LJob.SourcePublication;
  ABuild := LJob.SourceBuild;
  AReceipt := EncodeNyxSourcePublicationReceipt(NyxSourcePublicationReceipt(
    AIssuer, AWorkspace, NyxBuildJob(LJob.ID), AReference, APublication.Revision + 1));
  { Allocate before paired publication/durable save. Sealing afterward only sets
    a Boolean under the same host registry lock; it cannot allocate a late reply. }
  LJob.SourceProducerText := AProducerText;
  LJob.SourcePublicationReceipt := AReceipt;
  Result := False;
end;

procedure TNyxBuildJobs.SealSourcePublication(const AReference: TNyxSourceProjectionRef;
  const AWorkspace: TNyxWorkspaceRef; const AOwner: TNyxText);
var
  LJob: TBuildJob;
begin
  { Do not Pump or retire handles between staging and sealing. The owning server
    holds its registry lock across this synchronous durable publication boundary. }
  LJob := TBuildJob(FJobs[SourcePublicationIndex(AReference, AWorkspace, AOwner)]);
  LJob.SourcePublished := True;
end;

function TNyxBuildJobs.SourceJobs(const AWorkspace: TNyxWorkspaceRef;
  const AOwner: TNyxText): TNyxDataValue;
var
  LIndex: Integer;
  LCount: Integer;
  LJob: TBuildJob;
  LItems: array of TNyxDataValue;
begin
  Pump;
  LCount := 0;
  SetLength(LItems, 10);
  for LIndex := 0 to FJobs.Count - 1 do
  begin
    LJob := TBuildJob(FJobs[LIndex]);

    if (LJob.Purpose = cjpSourceProjection) and
      (LJob.Workspace.ID = AWorkspace.ID) and (LJob.Owner = AOwner) and
      not LJob.Terminal then
    begin
      LItems[LCount] := NyxObject([NyxField('job', NyxData(LJob.ID)),
        NyxField('state', NyxData(NyxBuildJobStateName(LJob.State)))]);
      Inc(LCount);
    end;
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('active', NyxData(LCount)),
    NyxField('items', NyxArray(LItems))]);
end;

function TNyxBuildJobs.SourceStatus(const AJob: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; const AOwner: TNyxText;
  ACancel: Boolean): TNyxDataValue;
var
  LIndex: Integer;
  LJob: TBuildJob;
begin
  Pump;
  for LIndex := 0 to FJobs.Count - 1 do
  begin
    LJob := TBuildJob(FJobs[LIndex]);

    if (LJob.ID = AJob) and (LJob.Purpose = cjpSourceProjection) and
      (LJob.Workspace.ID = AWorkspace.ID) and (LJob.Owner = AOwner) then
    begin

      if ACancel then
      begin
        LJob.Guard.Acquire;
        try

          if not NyxBuildJobTerminal(LJob.State) then
          begin
            LJob.Cancellation.Cancel;

            if LJob.State = bjsQueued then
            begin
              LJob.State := bjsCancelled;
            end
            else
            begin
              LJob.State := bjsCancelling;
            end;
          end;
        finally
          LJob.Guard.Release;
        end;
      end;
      Exit(LJob.SourceSnapshot);
    end;
  end;
  raise ENyxModel.Create('Unknown or expired source compiler job in this exact context');
end;

procedure TNyxBuildJobs.Remember(const AOwner: TNyxText;
  const AArguments, AReceipt: TNyxDataValue);
var
  LIndex: Integer;
begin
  LIndex := Length(FReceiptKeys);

  if LIndex = 64 then
  begin
    for LIndex := 1 to 63 do
    begin
      FReceiptKeys[LIndex - 1] := FReceiptKeys[LIndex];
      FReceiptRequests[LIndex - 1] := FReceiptRequests[LIndex];
      FReceipts[LIndex - 1] := FReceipts[LIndex];
    end;
    LIndex := 63;
  end
  else
  begin
    SetLength(FReceiptKeys, LIndex + 1);
    SetLength(FReceiptRequests, LIndex + 1);
    SetLength(FReceipts, LIndex + 1);
  end;
  FReceiptKeys[LIndex] := ReceiptKey(AOwner, AArguments);
  FReceiptRequests[LIndex] := AArguments.ToJSON;
  FReceipts[LIndex] := AReceipt;
end;

function TNyxBuildJobs.Status(const AArguments: TNyxDataValue;
  out APair: TNyxProjectPair; out ACurrentOutput: Boolean): TNyxDataValue;
var
  LIndex: Integer;
  LOffset: Integer;
  LLimit: Integer;
  LID: TNyxText;
  LSeverity: TNyxText;
  LKind: TNyxCompilerSeverity;
  LValid: Boolean;
begin
  NyxAgentFields(AArguments, '|mode|job|offset|limit|severity|');
  LID := AArguments.Field('job').AsText;
  LOffset := 0;
  LLimit := 10;
  LSeverity := 'all';

  if NyxAgentHas(AArguments, 'severity') then
  begin
    LSeverity := AArguments.Field('severity').AsText;
  end;
  LValid := LSeverity = 'all';
  for LKind := Low(TNyxCompilerSeverity) to High(TNyxCompilerSeverity) do
  begin
    LValid := LValid or (LSeverity = LowerCase(NyxCompilerSeverityName(LKind)));
  end;

  if not LValid then
  begin
    raise ENyxModel.Create('Build diagnostic severity is not published');
  end;

  if NyxAgentHas(AArguments, 'offset') then
  begin
    LOffset := AArguments.Field('offset').AsInteger;
  end;

  if NyxAgentHas(AArguments, 'limit') then
  begin
    LLimit := AArguments.Field('limit').AsInteger;
  end;

  if (LOffset < 0) or (LOffset > 512) or (LLimit < 1) or (LLimit > 20) then
  begin
    raise ENyxModel.Create('Build diagnostic window is offset 0..512, limit 1..20');
  end;
  Pump;
  for LIndex := 0 to FJobs.Count - 1 do
  begin

    if (TBuildJob(FJobs[LIndex]).ID = LID) and
      (TBuildJob(FJobs[LIndex]).Purpose = cjpApplication) then
    begin
      APair := TBuildJob(FJobs[LIndex]).Pair;
      ACurrentOutput := TBuildJob(FJobs[LIndex]).Profile = FProfile;
      Exit(TBuildJob(FJobs[LIndex]).Snapshot(LOffset, LLimit, LSeverity));
    end;
  end;
  raise ENyxModel.Create('Unknown or expired build job; no implicit resubmission');
end;

function TNyxBuildJobs.TakeCompletion(out AActor, AOutcome: TNyxText;
  out APair: TNyxProjectPair; out AReport: INyxCompilerReport): Boolean;
var
  LReview: TNyxReviewRef;
begin
  Result := TakeCompletion(AActor, AOutcome, APair, AReport, LReview);
end;

function TNyxBuildJobs.TakeCompletion(out AActor, AOutcome: TNyxText;
  out APair: TNyxProjectPair; out AReport: INyxCompilerReport;
  out AReview: TNyxReviewRef): Boolean;
var
  LWorkspace: TNyxWorkspaceRef;
begin
  Result := TakeCompletion(AActor, AOutcome, APair, AReport, AReview, LWorkspace);
end;

function TNyxBuildJobs.TakeCompletion(out AActor, AOutcome: TNyxText;
  out APair: TNyxProjectPair; out AReport: INyxCompilerReport;
  out AReview: TNyxReviewRef; out AWorkspace: TNyxWorkspaceRef): Boolean;
var
  LCurrentOutput: Boolean;
begin
  Result := TakeCompletion(AActor, AOutcome, APair, AReport, AReview, AWorkspace, LCurrentOutput);
end;

function TNyxBuildJobs.TakeCompletion(out AActor, AOutcome: TNyxText;
  out APair: TNyxProjectPair; out AReport: INyxCompilerReport;
  out AReview: TNyxReviewRef; out AWorkspace: TNyxWorkspaceRef;
  out ACurrentOutput: Boolean): Boolean;
const
  CSeparator: TNyxText = ' · ';
var
  LIndex: Integer;
  LJob: TBuildJob;
begin
  Result := False;
  AReport := nil;
  AReview := NyxActiveWorkspace;
  AWorkspace := NyxPrimaryWorkspace;
  Pump;
  for LIndex := 0 to FJobs.Count - 1 do
  begin
    LJob := TBuildJob(FJobs[LIndex]);
    LJob.Guard.Acquire;
    try

      if (LJob.Purpose = cjpApplication) and
        NyxBuildJobTerminal(LJob.State) and not LJob.Announced then
      begin
        LJob.Announced := True;
        AActor := LJob.Actor;
        AOutcome := NyxBuildJobStateName(LJob.State) + CSeparator + LJob.Arguments.Field('scope').AsText +
          CSeparator + LJob.Arguments.Field('target').AsText;

        if LJob.Error <> '' then
        begin
          AOutcome := AOutcome + CSeparator + BoundedText(LJob.Error, 200);
        end;
        APair := LJob.Pair;
        AReview := LJob.Review;
        AWorkspace := LJob.Workspace;
        AReport := LJob.Report;
        ACurrentOutput := LJob.Profile = FProfile;
        Exit(True);
      end;
    finally
      LJob.Guard.Release;
    end;
  end;
end;

procedure TNyxBuildJobs.AdmitCancel(const AArguments: TNyxDataValue);
var
  LID: TNyxText;
begin
  NyxAgentFields(AArguments, '|mode|job|expectedRevision|operationId|');
  AArguments.Field('expectedRevision').AsInteger;
  LID := AArguments.Field('operationId').AsText;

  if (NyxTextScalarCount(LID) < 1) or (NyxTextScalarCount(LID) > 120) or
    (Pos(#0, LID) > 0) or (Pos(#10, LID) > 0) or (Pos(#13, LID) > 0) then
  begin
    raise ENyxModel.Create('Cancel operationId must contain 1..120 characters');
  end;

  if Length(AArguments.Field('job').AsText) <> 36 then
  begin
    raise ENyxModel.Create('Cancel requires an exact retained job ID');
  end;
end;

function TNyxBuildJobs.Cancel(const AOwner: TNyxText;
  const AArguments: TNyxDataValue; AOperator: Boolean): TNyxDataValue;
var
  LIndex: Integer;
  LJob: TBuildJob;
begin
  AdmitCancel(AArguments);
  { Validate ownership before retry lookup. An operator may cancel someone
    else's job only through the already authenticated operator/context route. }
  for LIndex := 0 to FJobs.Count - 1 do
  begin
    LJob := TBuildJob(FJobs[LIndex]);

    if (LJob.ID = AArguments.Field('job').AsText) and
      (LJob.Purpose = cjpApplication) then
    begin

      if not AOperator and (LJob.Owner <> AOwner) then
      begin
        raise ENyxModel.Create('Compiler cancellation requires the admitting connection');
      end;

      if Retry(AOwner, AArguments, Result) then
      begin
        Exit;
      end;
      LJob.Guard.Acquire;
      try

        if not NyxBuildJobTerminal(LJob.State) then
        begin
          LJob.Cancellation.Cancel;

          if LJob.State = bjsQueued then
          begin
            LJob.State := bjsCancelled;
          end
          else
          begin
            LJob.State := bjsCancelling;
          end;
        end;
      finally
        LJob.Guard.Release;
      end;
      Result := LJob.Snapshot(0, 1);
      Remember(AOwner, AArguments, Result);
      Exit;
    end;
  end;
  raise ENyxModel.Create('Unknown or expired compiler job; cancellation has no context fallback');
end;

end.
