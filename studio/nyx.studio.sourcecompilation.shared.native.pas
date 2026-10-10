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

unit nyx.studio.sourcecompilation.shared.native;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.scheduler, nyx.studio.transport,
  nyx.studio.sourcecompilation, nyx.studio.sourcecompilation.shared;

type
  { Trusted machine settings, copied before any operation. WholeOperation covers
    queueing, compilation, retirement and publication retries (1..600000 ms).
    Polling accepts 10..5000 ms. Requests snapshots the existing bounded policy;
    a single request also cannot exceed the remaining whole-operation budget.
    Worker/queue limits use the ordinary public scheduler contract. Defaults:
    150 seconds, 100 ms polling, 15 second requests, four workers/1024 slots.
    A zeroed record refuses; settings never enter exported designs or history. }
  TNyxNativeSourceServiceOptions = record
  private
    FOperationMS: Integer;
    FPollMS: Integer;
    FRequests: TNyxTransportLimits;
    FScheduler: TNyxSchedulerOptions;
  public
    class function Defaults: TNyxNativeSourceServiceOptions; static;
    function WholeOperation(AMilliseconds: Integer): TNyxNativeSourceServiceOptions;
    function Polling(AMilliseconds: Integer): TNyxNativeSourceServiceOptions;
    function Requests(const APolicy: INyxTransportPolicy): TNyxNativeSourceServiceOptions;
    function Scheduling(const AOptions: TNyxSchedulerOptions): TNyxNativeSourceServiceOptions;
    procedure Validate;
  end;

  { Exact private reply bytes decoded as UTF-8 by the transport. Zero status
    means delivery is unconfirmed, never that server admission was rolled back. }
  TNyxNativeSourceReply = record
    Status: Integer;
    Text: TNyxText;
  end;

  { Worker-only bounded transport seam. Implementations must own request work,
    enforce AMaximumReplyBytes before allocation/JSON parsing, and finish within
    ARemainingMS. They borrow no UI, editor/session or accepted tree. Independent
    workers may call simultaneously. HTTP authority/context stay in copied inputs;
    no portable project/file input can install an alternative transport. }
  INyxNativeSourceTransport = interface(IInterface)
    ['{D371EF0B-88E5-4246-98F6-071056CEDD01}']
    function Post(const ACapability, ABody: TNyxText; AMaximumReplyBytes,
      ARemainingMS: Integer): TNyxNativeSourceReply;
  end;

  { Immutable terminal diagnostic, including a throwing completion port. It is
    empty while pending/running and never claims editor admission or remote join. }
  INyxNativeSharedSourceCompilation = interface(INyxSourceCompilation)
    ['{D371EF0B-88E5-4246-98F6-071056CEDD02}']
    function GetFailure: TNyxText;
    property Failure: TNyxText read GetFailure;
  end;

{ Native Apply/visual factory, created idle on the UI thread. Every compiler
  copies the bridge's exact capability/issuer/workspace/revision. Workers use the
  existing deadline HTTP adapter and own no bridge/controller. Cancel requests
  remote retirement and observes its join; a failed observation reports uncertainty.
  Publication names only the retained service result. Lost replies recover the
  same request/producer, never another job. Completions run on independent native
  workers (pending cancellation on its caller), so ports must stage/queue safely.
  Factory retirement cancels its scheduler/jobs without blocking the UI for join.
  Keep the factory while its strategy is enabled; no compiler is needed at creation. }
function NewNyxSharedNativeSourceCompilerFactory(
  const AOrigin: TNyxText): INyxSharedSourceCompilerFactory; overload;
function NewNyxSharedNativeSourceCompilerFactory(const AOrigin: TNyxText;
  const AOptions: TNyxNativeSourceServiceOptions): INyxSharedSourceCompilerFactory; overload;
{ Explicit trusted embedding/qualification seam; ownership and guards are the
  same as HTTP. This does not prove network behavior for an in-process transport. }
function NewNyxSharedNativeSourceCompilerFactory(const ATransport: INyxNativeSourceTransport;
  const AOptions: TNyxNativeSourceServiceOptions): INyxSharedSourceCompilerFactory; overload;

implementation

uses
  Classes, SysUtils, nyx.bytes, nyx.model, nyx.source, nyx.editing,
  nyx.studio.transport.native, nyx.studio.sourceprojection,
  nyx.studio.sourcebuilds, nyx.studio.sourcepublications, nyx.studio.workspaces,
  nyx.studio.builds, nyx.studio.editorbuild, nyx.studio.session;

type
  TReplyBytes = class(TMemoryStream)
  public
    Maximum: Integer;
    function Write(const ABuffer; ACount: Longint): Longint; override;
  end;
  THTTPTransport = class(TInterfacedObject, INyxNativeSourceTransport)
  public
    Origin: TNyxText;
    function Post(const ACapability, ABody: TNyxText; AMaximumReplyBytes,
      ARemainingMS: Integer): TNyxNativeSourceReply;
  end;
  TNativeSharedOperation = class(TInterfacedObject, INyxSourceCompilation,
    INyxNativeSharedSourceCompilation, INyxWork)
  private
    FState: LongInt;
    FFailure: TNyxText;
    FStarted: QWord;
    FRemoteJoined: Boolean;
    FAdmissionUncertain: Boolean;
    FPublishAttempted: Boolean;
    FPublishUncertain: Boolean;
    FJob: TNyxBuildJobRef;
    function Cancelled(const AExecution: INyxExecution): Boolean;
    function Send(const AArguments: TNyxDataValue; AMaximumBytes: Integer): TNyxNativeSourceReply;
    function Compile(const AExecution: INyxExecution): INyxSourceProjectionBuild;
    function Publish(const ABuild: INyxSourceProjectionBuild;
      const AExecution: INyxExecution): TNyxSourcePublicationReceipt;
    procedure Deliver(AState: TNyxSourceCompilationState; AOutcome: TNyxSourcePublicationOutcome;
      const AProjection: INyxSourceProjection; const AReceipt: TNyxSourcePublicationReceipt;
      const AMessage: TNyxText);
  public
    Capability: TNyxText;
    Issuer: TNyxText;
    Workspace: TNyxWorkspaceRef;
    Revision: Integer;
    Source: TNyxText;
    Arguments: TNyxDataValue;
    Options: TNyxNativeSourceServiceOptions;
    Transport: INyxNativeSourceTransport;
    Scope: INyxCancellationScope;
    Port: INyxSharedSourceCompilationPort;
    Execution: INyxExecution;
    function GetState: TNyxSourceCompilationState;
    function GetFailure: TNyxText;
    procedure Cancel;
    procedure Execute(const AExecution: INyxExecution);
  end;
  { A short-lived compiler retains its provider only until Start. Operations
    retain copied context/transport/execution, never this owner. No cycle exists. }
  INativeSourceOwner = interface(IInterface)
    ['{D371EF0B-88E5-4246-98F6-071056CEDD03}']
    function Submit(AOperation: TNativeSharedOperation): INyxSourceCompilation;
  end;
  TNativeSharedCompiler = class(TInterfacedObject, INyxSharedSourceCompiler)
  public
    Owner: INativeSourceOwner;
    Capability: TNyxText;
    Issuer: TNyxText;
    Workspace: TNyxWorkspaceRef;
    Revision: Integer;
    Intent: TNyxDataValue;
    function Start(const ASource: TNyxText;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
  end;
  TNativeSharedFactory = class(TInterfacedObject, INyxSharedSourceCompilerFactory,
    INyxSharedVisualSourceCompilerFactory, INativeSourceOwner)
  private
    FJobs: array of INyxSourceCompilation;
    function Compiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
      const AIntent: TNyxDataValue): INyxSharedSourceCompiler;
  public
    Transport: INyxNativeSourceTransport;
    Options: TNyxNativeSourceServiceOptions;
    Scheduler: INyxScheduler;
    destructor Destroy; override;
    function CreateCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer): INyxSharedSourceCompiler;
    function CreateVisualCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
      const AIntent: TNyxDataValue): INyxSharedSourceCompiler;
    function Submit(AOperation: TNativeSharedOperation): INyxSourceCompilation;
  end;

class function TNyxNativeSourceServiceOptions.Defaults: TNyxNativeSourceServiceOptions;
begin
  Result := Default(TNyxNativeSourceServiceOptions);
  Result.FOperationMS := 150000;
  Result.FPollMS := 100;
  Result.FRequests := NewNyxTransportPolicy.Snapshot;
  Result.FScheduler := TNyxSchedulerOptions.Defaults;
end;

procedure TNyxNativeSourceServiceOptions.Validate;
begin

  if (FOperationMS < 1) or (FOperationMS > 600000) or
    (FPollMS < 10) or (FPollMS > 5000) then
  begin
    raise ENyxModel.Create('Native source service requires bounded operation and polling settings');
  end;
  ValidateNyxTransportLimits(FRequests);
  FScheduler.Validate;
end;

function TNyxNativeSourceServiceOptions.WholeOperation(AMilliseconds: Integer): TNyxNativeSourceServiceOptions;
begin
  Result := Self;
  Result.FOperationMS := AMilliseconds;
  Result.Validate;
end;

function TNyxNativeSourceServiceOptions.Polling(AMilliseconds: Integer): TNyxNativeSourceServiceOptions;
begin
  Result := Self;
  Result.FPollMS := AMilliseconds;
  Result.Validate;
end;

function TNyxNativeSourceServiceOptions.Requests(const APolicy: INyxTransportPolicy): TNyxNativeSourceServiceOptions;
begin

  if APolicy = nil then
  begin
    raise ENyxModel.Create('Native source requests require an explicit bounded policy');
  end;
  Result := Self;
  Result.FRequests := APolicy.Snapshot;
  Result.Validate;
end;

function TNyxNativeSourceServiceOptions.Scheduling(const AOptions: TNyxSchedulerOptions): TNyxNativeSourceServiceOptions;
begin
  Result := Self;
  Result.FScheduler := AOptions;
  Result.Validate;
end;

function TReplyBytes.Write(const ABuffer; ACount: Longint): Longint;
begin

  if (ACount < 0) or (Position > Maximum - ACount) then
  begin
    raise ENyxModel.Create('Native source response exceeds its byte budget');
  end;
  Result := inherited Write(ABuffer, ACount);
end;

function THTTPTransport.Post(const ACapability, ABody: TNyxText;
  AMaximumReplyBytes, ARemainingMS: Integer): TNyxNativeSourceReply;
var
  LLifetime: TNyxHTTPRequestLifetime;
  LClient: TNyxDeadlineHTTPClient;
  LRequest: TMemoryStream;
  LReply: TReplyBytes;
  LBytes: TNyxBytes;
begin
  Result := Default(TNyxNativeSourceReply);
  LLifetime := nil;
  LClient := nil;
  LRequest := nil;
  LReply := nil;
  try
    LLifetime := TNyxHTTPRequestLifetime.Create(
      NewNyxTransportPolicy.WholeRequest(ARemainingMS).Snapshot);
    LClient := TNyxDeadlineHTTPClient.CreateFor(LLifetime);
    LRequest := TMemoryStream.Create;
    LReply := TReplyBytes.Create;
    LReply.Maximum := AMaximumReplyBytes;
    LBytes := NyxEncodeUTF8(ABody);

    if Length(LBytes) > 0 then
    begin
      LRequest.WriteBuffer(LBytes[0], Length(LBytes));
    end;
    LRequest.Position := 0;
    LClient.AddHeader('Content-Type', 'application/json; charset=utf-8');
    LClient.AddHeader('Origin', Origin);
    LClient.AddHeader('X-Nyx-Editor', ACapability);
    LClient.RequestBody := LRequest;
    LClient.HTTPMethod('POST', Origin + '/api/agents/source', LReply, []);
    LLifetime.Check;
    Result.Status := LClient.ResponseStatusCode;
    SetLength(LBytes, LReply.Size);
    LReply.Position := 0;

    if Length(LBytes) > 0 then
    begin
      LReply.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result.Text := NyxDecodeUTF8(LBytes);
  finally

    if LClient <> nil then
    begin
      LClient.RequestBody := nil;
    end;
    LClient.Free;
    LRequest.Free;
    LReply.Free;
    LLifetime.Free;
  end;
end;

function TNativeSharedOperation.GetState: TNyxSourceCompilationState;
begin
  Result := TNyxSourceCompilationState(InterlockedCompareExchange(FState, 0, 0));
end;

function TNativeSharedOperation.GetFailure: TNyxText;
begin
  Result := '';

  if GetState in [scsCompleted, scsCancelled, scsFailed] then
  begin
    Result := FFailure;
  end;
end;

function TNativeSharedOperation.Cancelled(const AExecution: INyxExecution): Boolean;
begin
  Result := Scope.Cancelled or AExecution.Cancelled;
end;

procedure TNativeSharedOperation.Deliver(AState: TNyxSourceCompilationState;
  AOutcome: TNyxSourcePublicationOutcome; const AProjection: INyxSourceProjection;
  const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText);
var
  LPort: INyxSharedSourceCompilationPort;
begin
  LPort := Port;
  Port := nil;
  FFailure := AMessage;
  try
    try

      if LPort <> nil then
      begin
        LPort.Complete(AOutcome, AProjection, AReceipt, AMessage);
      end;
    except
      on LException: Exception do
      begin
        FFailure := UTF8Encode(UnicodeString(LException.Message));
        AState := scsFailed;
      end;
    end;
  finally
    LPort := nil;
    Transport := nil;
    InterlockedExchange(FState, Ord(AState));
  end;
end;

procedure TNativeSharedOperation.Cancel;
begin
  Scope.Cancel;

  if Execution <> nil then
  begin
    Execution.Cancel;
  end;
  { Dispatch and pending retirement choose exactly one completion owner. Running
    cancellation merely requests retirement; its worker owns the remote join. }

  if InterlockedCompareExchange(FState, Ord(scsRunning), Ord(scsPending)) = Ord(scsPending) then
  begin
    Deliver(scsCancelled, npoRefused, nil, Default(TNyxSourcePublicationReceipt),
      'Shared source command cancelled before dispatch');
  end;
end;

function TNativeSharedOperation.Send(const AArguments: TNyxDataValue;
  AMaximumBytes: Integer): TNyxNativeSourceReply;
var
  LElapsed: QWord;
  LRemaining: Integer;
  LBody: TNyxText;
begin
  LElapsed := GetTickCount64 - FStarted;

  if LElapsed >= QWord(Options.FOperationMS) then
  begin
    raise ENyxTransportDeadline.Create('Shared source operation deadline expired; remote delivery may be unconfirmed');
  end;
  LRemaining := Options.FOperationMS - Integer(LElapsed);

  if LRemaining > Options.FRequests.DeadlineMS then
  begin
    LRemaining := Options.FRequests.DeadlineMS;
  end;
  LBody := NyxWithWorkspace(NyxObject([NyxField('compile', AArguments)]), Workspace).ToJSON;
  try
    Result := Transport.Post(Capability, LBody, AMaximumBytes, LRemaining);
  except
    { Keep the same operation/ref after any transport exception. Do not disclose
      capability/URL/native HTTP exception text through editor chrome. }
    Result := Default(TNyxNativeSourceReply);
  end;

  if NyxUTF8ByteCount(Result.Text) > AMaximumBytes then
  begin
    raise ENyxModel.Create('Shared source transport returned an oversized response');
  end;
end;

function TNativeSharedOperation.Compile(const AExecution: INyxExecution): INyxSourceProjectionBuild;
var
  LArguments: TNyxDataValue;
  LReply: TNyxNativeSourceReply;
  LValue: TNyxDataValue;
  LJob: TNyxBuildJobRef;
  LState: TNyxBuildJobState;
  LPreviouslyUncertain: Boolean;
begin
  repeat
    LArguments := Arguments;

    if FJob.ID <> '' then
    begin
      LArguments := NyxObject([NyxField('mode', NyxData('status')),
        NyxField('job', NyxData(FJob.ID))]);

      if Cancelled(AExecution) then
      begin
        LArguments := NyxObject([NyxField('mode', NyxData('cancel')),
          NyxField('job', NyxData(FJob.ID))]);
      end;
    end;
    LPreviouslyUncertain := FAdmissionUncertain;
    FAdmissionUncertain := True;
    LReply := Send(LArguments, NyxNativeSourceBuildMaximumReplyBytes + 8192);

    if LReply.Status = 0 then
    begin
      FAdmissionUncertain := True;
      Sleep(Options.FPollMS);
      Continue;
    end;
    LValue := TNyxDataValue.ParseJSON(LReply.Text);

    if LReply.Status <> 200 then
    begin

      if (LReply.Status >= 400) and (LReply.Status < 500) and
        not LPreviouslyUncertain and (FJob.ID = '') then
      begin
        FAdmissionUncertain := False;
      end;
      raise ENyxModel.Create('Shared source compiler refused: ' + LValue.Field('error').AsText);
    end;

    if NyxWorkspaceArgument(LValue).ID <> Workspace.ID then
    begin
      raise ENyxModel.Create('Shared source compiler response belongs to another project');
    end;
    LJob := NyxBuildJob(LValue.Field('job').AsText);

    if (FJob.ID <> '') and (FJob.ID <> LJob.ID) then
    begin
      raise ENyxModel.Create('Shared source compiler substituted its retained job');
    end;
    FJob := LJob;
    LState := ParseNyxBuildJobState(LValue.Field('state').AsText);

    if not NyxBuildJobTerminal(LState) then
    begin
      Sleep(Options.FPollMS);
      Continue;
    end;
    FRemoteJoined := True;

    if LState = bjsCancelled then
    begin
      Exit(nil);
    end;
    Result := DecodeNyxNativeSourceBuild(Source, LValue.Field('receipt'));

    if (LState = bjsSucceeded) <> (Result.Projection.State = spsExecuted) then
    begin
      raise ENyxModel.Create('Shared source compiler state differs from its native receipt');
    end;
    Exit;
  until False;
end;

function TNativeSharedOperation.Publish(const ABuild: INyxSourceProjectionBuild;
  const AExecution: INyxExecution): TNyxSourcePublicationReceipt;
var
  LArguments: TNyxDataValue;
  LReply: TNyxNativeSourceReply;
  LValue: TNyxDataValue;
begin
  LArguments := NyxObject([NyxField('mode', NyxData('complete')),
    NyxField('reference', NyxData(ABuild.Reference.Name))]);
  repeat

    if Cancelled(AExecution) then
    begin
      raise ENyxModel.Create('Shared source publication cancelled; observing reconciliation is required');
    end;
    FPublishAttempted := True;
    LReply := Send(LArguments, 8192);

    if LReply.Status = 0 then
    begin
      FPublishUncertain := True;
      Sleep(Options.FPollMS);
      Continue;
    end;
    { A committed HTTP success or server failure may have crossed the write
      boundary even when its body cannot be parsed. Mark uncertainty BEFORE
      decoding; malformed JSON cannot turn an actual commit into a refusal. }

    if (LReply.Status = 200) or (LReply.Status >= 500) then
    begin
      FPublishUncertain := True;
    end;
    LValue := TNyxDataValue.ParseJSON(LReply.Text);

    if LReply.Status <> 200 then
    begin

      raise ENyxModel.Create('Shared source publication refused: ' + LValue.Field('error').AsText);
    end;
    { Success can have committed before a malformed/substituted acknowledgement
      is detected. Every such failure requires observing reconciliation. }
    FPublishUncertain := True;
    Result := DecodeNyxSourcePublicationReceipt(LValue, Issuer, Workspace, ABuild.Reference);

    if (Result.Job.ID <> FJob.ID) or (Result.Revision <> Revision + 1) then
    begin
      raise ENyxModel.Create('Shared source publication acknowledgement changed its captured job/revision');
    end;
    FPublishUncertain := False;
    Exit;
  until False;
end;

procedure TNativeSharedOperation.Execute(const AExecution: INyxExecution);
var
  LBuild: INyxSourceProjectionBuild;
  LReceipt: TNyxSourcePublicationReceipt;
  LProjection: INyxSourceProjection;
  LOutcome: TNyxSourcePublicationOutcome;
  LState: TNyxSourceCompilationState;
  LMessage: TNyxText;
begin

  if InterlockedCompareExchange(FState, Ord(scsRunning), Ord(scsPending)) <> Ord(scsPending) then
  begin
    Exit;
  end;
  LOutcome := npoRefused;
  LState := scsFailed;
  LReceipt := Default(TNyxSourcePublicationReceipt);
  LMessage := '';
  try
    try

      if Cancelled(AExecution) then
      begin
        LState := scsCancelled;
        LMessage := 'Shared source command cancelled before request';
      end
      else
      begin
        LBuild := Compile(AExecution);

        if Cancelled(AExecution) or (LBuild = nil) then
        begin
          LState := scsCancelled;
          LMessage := 'Owned shared source compiler job retired';
        end
        else
        begin
          LProjection := LBuild.Projection;

          if LProjection.State <> spsExecuted then
          begin
            LMessage := LProjection.Message;
          end
          else
          begin
            LReceipt := Publish(LBuild, AExecution);
            LOutcome := npoCommitted;
            LState := scsCompleted;
          end;
        end;
      end;
    except
      on LException: Exception do
      begin
        LMessage := UTF8Encode(UnicodeString(LException.Message));

        if FPublishUncertain or (FPublishAttempted and Cancelled(AExecution)) or
          (not FRemoteJoined and (FAdmissionUncertain or (FJob.ID <> ''))) then
        begin
          LOutcome := npoUnconfirmed;
        end;
      end;
    end;
  finally
    Deliver(LState, LOutcome, LProjection, LReceipt, LMessage);
  end;
end;

function TNativeSharedCompiler.Start(const ASource: TNyxText;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
var
  LOperation: TNativeSharedOperation;
  LLease: INyxSourceCompilation;
  LID: TGUID;
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  ValidateNyxProjectionSource(ASource);
  NyxCompanionUnitName(ASource);

  if APort = nil then
  begin
    raise ENyxModel.Create('Shared native source requires an independent completion port');
  end;
  LOperation := TNativeSharedOperation.Create;
  LLease := LOperation;
  LOperation.Capability := Capability;
  LOperation.Issuer := Issuer;
  LOperation.Workspace := Workspace;
  LOperation.Revision := Revision;
  LOperation.Source := ASource;
  LOperation.Port := APort;
  CreateGUID(LID);
  LOperation.Arguments := NyxObject([
    NyxField('mode', NyxData('request')),
    NyxField('operationId', NyxData('source-native-' + GUIDToString(LID))),
    NyxField('expectedRevision', NyxData(Revision)),
    NyxField('source', NyxData(ASource)),
    NyxField('target', NyxData(NyxBuildTargetName(btNativeLCL))),
    NyxField('publish', NyxData(True)), NyxField('issuer', NyxData(Issuer))]);

  if Intent.Kind <> ndNull then
  begin
    SetLength(LFields, LOperation.Arguments.Count + 1);
    for LIndex := 0 to LOperation.Arguments.Count - 1 do
    begin
      LFields[LIndex] := NyxField(LOperation.Arguments.Key(LIndex),
        LOperation.Arguments.Field(LOperation.Arguments.Key(LIndex)));
    end;
    LFields[High(LFields)] := NyxField('visual', Intent.Copy);
    LOperation.Arguments := NyxObject(LFields);
  end;

  if NyxUTF8ByteCount(NyxWithWorkspace(NyxObject([
    NyxField('compile', LOperation.Arguments)]), Workspace).ToJSON) >
    NyxSourceBuildMaximumRequestBytes then
  begin
    raise ENyxModel.Create('Shared native source request exceeds its formatted byte budget');
  end;
  { The lease also owns the candidate if admission raises before the scheduler
    adopts it. On success the caller receives the same independent token. }
  LLease := Owner.Submit(LOperation);
  Result := LLease;
end;

function TNativeSharedFactory.Compiler(const ACapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const AIntent: TNyxDataValue): INyxSharedSourceCompiler;
var
  LCompiler: TNativeSharedCompiler;
begin
  Scheduler.RequireUI;

  if (NyxTextScalarCount(ACapability) < 1) or (NyxTextScalarCount(ACapability) > 512) or
    (Pos(#0, ACapability) > 0) or (Pos(#10, ACapability) > 0) or (Pos(#13, ACapability) > 0) or
    (AIssuer = '') or (Length(AIssuer) > 128) or (ARevision < 1) or
    (ARevision = High(Integer)) then
  begin
    raise ENyxModel.Create('Shared native source requires exact private capability, issuer and revision');
  end;
  LCompiler := TNativeSharedCompiler.Create;
  Result := LCompiler;
  LCompiler.Owner := Self;
  LCompiler.Capability := ACapability;
  LCompiler.Issuer := AIssuer;
  LCompiler.Workspace := AWorkspace;
  LCompiler.Revision := ARevision;
  LCompiler.Intent := AIntent.Copy;
end;

function TNativeSharedFactory.CreateCompiler(const ACapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer): INyxSharedSourceCompiler;
begin
  Result := Compiler(ACapability, AIssuer, AWorkspace, ARevision, NyxNull);
end;

function TNativeSharedFactory.CreateVisualCompiler(const ACapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const AIntent: TNyxDataValue): INyxSharedSourceCompiler;
begin
  ReadNyxStudioDesignIntent(AIntent);
  Result := Compiler(ACapability, AIssuer, AWorkspace, ARevision, AIntent);
end;

function TNativeSharedFactory.Submit(AOperation: TNativeSharedOperation): INyxSourceCompilation;
var
  LRetained: array of INyxSourceCompilation;
  LCount: Integer;
  LIndex: Integer;
  LWork: INyxWork;
begin
  Scheduler.RequireUI;
  SetLength(LRetained, Length(FJobs) + 1);
  LCount := 0;
  for LIndex := 0 to High(FJobs) do
  begin

    if FJobs[LIndex].State in [scsPending, scsRunning] then
    begin
      LRetained[LCount] := FJobs[LIndex];
      Inc(LCount);
    end;
  end;
  AOperation.Options := Options;
  AOperation.Transport := Transport;
  AOperation.Scope := NewNyxCancellationScope;
  AOperation.FStarted := GetTickCount64;
  Result := AOperation;
  LWork := AOperation;
  LRetained[LCount] := Result;
  SetLength(LRetained, LCount + 1);
  try
    AOperation.Execution := Scheduler.Submit(LWork, neThreaded);
  except
    AOperation.Port := nil;
    Result := nil;
    raise;
  end;
  FJobs := LRetained;
end;

destructor TNativeSharedFactory.Destroy;
var
  LJobs: array of INyxSourceCompilation;
  LIndex: Integer;
begin

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

function NewNyxSharedNativeSourceCompilerFactory(
  const AOrigin: TNyxText): INyxSharedSourceCompilerFactory;
begin
  Result := NewNyxSharedNativeSourceCompilerFactory(AOrigin,
    TNyxNativeSourceServiceOptions.Defaults);
end;

function NewNyxSharedNativeSourceCompilerFactory(const AOrigin: TNyxText;
  const AOptions: TNyxNativeSourceServiceOptions): INyxSharedSourceCompilerFactory;
var
  LTransport: THTTPTransport;
  LLease: INyxNativeSourceTransport;
begin
  ValidateNyxLocalStudioOrigin(AOrigin);
  AOptions.Validate;
  LTransport := THTTPTransport.Create;
  LLease := LTransport;
  LTransport.Origin := AOrigin;
  Result := NewNyxSharedNativeSourceCompilerFactory(LLease, AOptions);
end;

function NewNyxSharedNativeSourceCompilerFactory(const ATransport: INyxNativeSourceTransport;
  const AOptions: TNyxNativeSourceServiceOptions): INyxSharedSourceCompilerFactory;
var
  LFactory: TNativeSharedFactory;
begin

  if ATransport = nil then
  begin
    raise ENyxModel.Create('Shared native source requires its trusted transport');
  end;
  AOptions.Validate;
  LFactory := TNativeSharedFactory.Create;
  Result := LFactory;
  LFactory.Options := AOptions;
  LFactory.Transport := ATransport;
  LFactory.Scheduler := NewNyxScheduler(AOptions.FScheduler);
end;

end.
