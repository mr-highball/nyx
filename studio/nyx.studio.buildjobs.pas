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
  nyx.studio.builds, nyx.studio.compiler, nyx.studio.reviews, nyx.studio.workspaces;

type
  { Native compiler jobs own immutable accepted text and a private machine
    profile. Entry methods are serialized by the MCP transport; workers touch
    only their own guarded result. No worker borrows the editor or its nodes.
    At most two invocations run concurrently and sixteen job handles are retained.
    Terminal handles expire oldest first; artifacts retain the existing service
    lifecycle. Retry receipts (64) cannot silently submit an expired job again. }
  TNyxBuildJobs = class
  private
    FRepository: TNyxText;
    FProfile: TNyxText;
    FJobs: TList;
    FReceiptKeys: array of TNyxText;
    FReceiptRequests: array of TNyxText;
    FReceipts: array of TNyxDataValue;
  public
    constructor Create(const ARepository, AProfile: TNyxText);
    destructor Destroy; override;
    procedure Configure(const AProfile: TNyxText);
    function Outputs: TNyxDataValue;
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
  end;

{ Optimistic byte fingerprint, explicitly MD5 rather than an authentication
  credential. Model currentness and retry identity compare complete exact text,
  never hashes. Buffer overload avoids Windows ANSI conversion of UTF-8. }
function NyxBuildFingerprint(const AText: TNyxText): TNyxText;

implementation

uses
  md5, fpjson, nyx.model, nyx.codec, nyx.studio.outputs,
  nyx.studio.agents, nyx.studio.buildexecutor;

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
    ID: TNyxText;
    Actor: TNyxText;
    Review: TNyxReviewRef;
    Workspace: TNyxWorkspaceRef;
    Repository: TNyxText;
    Profile: TNyxText;
    Pair: TNyxProjectPair;
    Arguments: TNyxDataValue;
    Guard: TCriticalSection;
    Worker: TBuildWorker;
    State: TNyxText;
    Error: TNyxText;
    Output: TNyxDataValue;
    Manifest: TNyxDataValue;
    Report: INyxCompilerReport;
    Announced: Boolean;
    constructor Create;
    destructor Destroy; override;
    function Terminal: Boolean;
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
  State := 'running';
  Output := NyxNull;
  Manifest := NyxArray([]);
end;

destructor TBuildJob.Destroy;
begin
  { The owner joins before releasing immutable inputs/guard. No forced thread
    termination or callbacks into a destroyed Studio session can occur. }
  Worker.Free;
  Guard.Free;
  inherited Destroy;
end;

function TBuildJob.Terminal: Boolean;
begin
  Guard.Acquire;
  try
    Result := State <> 'running';
  finally
    Guard.Release;
  end;
end;

function TBuildJob.Snapshot(AOffset, ALimit: Integer;
  const ASeverity: TNyxText): TNyxDataValue;
var
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

      if State = 'succeeded' then
      begin
        LArtifact := Output.Field('artifact').AsText;
      end;
    end;
    LCount := 0;
    LOrder := NyxCompilerDiagnosticOrder(Report);
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
    Result := NyxObject([NyxField('job', NyxData(ID)),
      NyxField('state', NyxData(State)),
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
      NyxField('manifest', Manifest),
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
      LExecutor := TNyxBuildExecutor.Create(FJob.Repository, FJob.Profile);
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
        LScope, LView, FJob.Pair.Source);
      LOutput := TNyxDataValue.ParseJSON(TNyxText(LResult.AsJSON));
      LReport := DecodeNyxCompilerReport(LOutput.Field('diagnostics').ToJSON);
      LRoot := FJob.Repository + 'build' + PathDelim + 'studio' + PathDelim + 'jobs' + PathDelim;
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
        FJob.Output := LOutput;
        FJob.Manifest := LManifest;
        FJob.Report := LReport;

        if LOutput.Field('ok').AsBoolean then
        begin
          FJob.State := 'succeeded';
        end
        else
        begin
          FJob.Error := 'Compiler refused the accepted companion; inspect bounded diagnostics';
          FJob.State := 'failed';
        end;
      finally
        FJob.Guard.Release;
      end;
    except
      on LException: Exception do
      begin
        FJob.Guard.Acquire;
        try
          FJob.Error := LException.Message;
          FJob.State := 'failed';
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
  inherited Create;
  FRepository := IncludeTrailingPathDelimiter(ExpandFileName(ARepository));
  FJobs := TList.Create;
  Configure(AProfile);
  { Prepare the shared parent on the serialized owner before workers start.
    Older FPC ForceDirectories can race while recursively creating that parent;
    each worker subsequently creates only its unique invocation directory. }

  if not ForceDirectories(FRepository + 'build' + PathDelim + 'studio' +
    PathDelim + 'jobs') then
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
    for LIndex := 0 to FJobs.Count - 1 do
    begin
      TBuildJob(FJobs[LIndex]).Free;
    end;
  end;
  FJobs.Free;
  inherited Destroy;
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
  LExecutor := TNyxBuildExecutor.Create(FRepository, FProfile);
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

  if (Length(LID) < 1) or (Length(LID) > 120) then
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
  LJob: TBuildJob;
  LActive: Integer;
  LEvict: Integer;
  LIndex: Integer;
  LID: TGUID;
begin

  if (AReview.ID <> '') and (AWorkspace.ID <> '') then
  begin
    raise ENyxModel.Create('Compiler jobs require one exact project or review context');
  end;

  if AArguments.Field('outputID').AsText <> NyxBuildFingerprint(FProfile) then
  begin
    raise ENyxModel.Create('Output configuration changed; inspect readiness before submitting');
  end;
  LExecutor := TNyxBuildExecutor.Create(FRepository, FProfile);
  try
    LIssue := LExecutor.Readiness(AArguments.Field('target').AsText);
  finally
    LExecutor.Free;
  end;

  if LIssue <> '' then
  begin
    raise ENyxModel.Create(LIssue);
  end;
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

  if LActive >= 2 then
  begin
    raise ENyxModel.Create('Two compiler jobs are active; query status before submitting another');
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
    LJob.Review := AReview;
    LJob.Workspace := AWorkspace;
    LJob.Repository := FRepository;
    LJob.Profile := FProfile;
    LJob.Arguments := AArguments.Copy;
    LJob.Pair := APair;
    LJob.Worker := TBuildWorker.Create(LJob);
    Result := LJob.Snapshot(0, 1);
    LJob.Worker.Start;
    { Publish ownership only after the worker starts successfully. If starting
      or adding the handle fails, the local owner joins/frees it without leaving
      a dangling entry in the retained-job list. The transport lock prevents an
      observer from seeing this short preparation interval. }
    FJobs.Add(LJob);
    LJob := nil;
  finally
    LJob.Free;
  end;
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
  FReceiptKeys[LIndex] := ReceiptKey(ARetryOwner, AArguments);
  FReceiptRequests[LIndex] := AArguments.ToJSON;
  FReceipts[LIndex] := Result;
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
  for LIndex := 0 to FJobs.Count - 1 do
  begin

    if TBuildJob(FJobs[LIndex]).ID = LID then
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
  for LIndex := 0 to FJobs.Count - 1 do
  begin
    LJob := TBuildJob(FJobs[LIndex]);
    LJob.Guard.Acquire;
    try

      if (LJob.State <> 'running') and not LJob.Announced then
      begin
        LJob.Announced := True;
        AActor := LJob.Actor;
        AOutcome := LJob.State + CSeparator + LJob.Arguments.Field('scope').AsText +
          CSeparator + LJob.Arguments.Field('target').AsText;

        if LJob.Error <> '' then
        begin
          AOutcome := AOutcome + CSeparator + BoundedText(LJob.Error, 200);
        end;
        APair := LJob.Pair;
        AReview := LJob.Review;
        AWorkspace := LJob.Workspace;
        AReport := LJob.Report;
        Exit(True);
      end;
    finally
      LJob.Guard.Release;
    end;
  end;
end;

end.
