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
unit nyx.studio.recovery;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, nyx.text, nyx.studio.directories, nyx.studio.agents, nyx.studio.workspaces,
  nyx.studio.sourceprojection;

type
  { Explicit trusted startup execution strategy, separate from output selection
    and serialized files. Verify executes exactly this accepted source and
    returns a bound projection. A compiled-only browser receipt cannot admit
    recovery. The caller invokes this before starting listeners or live editing;
    implementations bound/join their processes and retain no editor pointers. }
  INyxRuntimeSourceVerifier = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100040}']
    { Refuse cancelled/retired work even when exact source has already been
      verified in this load. Called before pairs and final registry publication. }
    procedure RequireActive;
    function Verify(const ASource: TNyxText): INyxSourceProjection;
  end;

  { Native host-only owned rollback. The caller holds its protocol lock through
    cloning, mutation, durable publication or restoration. Primary/workspaces
    are independent mutable owners; transient authority stays in memory only. }
  TNyxStudioRuntimeRollback = class
  public
    Primary: TNyxAgentSession;
    Workspaces: TNyxStudioWorkspaces;
    constructor Create(APrimary: TNyxAgentSession; AWorkspaces: TNyxStudioWorkspaces);
    destructor Destroy; override;
    function Changed(APrimary: TNyxAgentSession; AWorkspaces: TNyxStudioWorkspaces): Boolean;
  end;

  { One native runtime checkpoint, independent of portable project exports.
    Version 1 streams exact UTF-8 fields and accepted paired history instead of
    escaping a potentially large registry into one 4 MiB JSON value. Version 2
    adds exact unfinished draft/base to each pending history entry. Accepted-only
    history retains the byte-identical legacy layout; either version is admitted.
    Older version-1 readers refuse extended files without rewriting them.
    The file is bounded
    to 512 MiB, each text to 4 MiB, nine sessions and fifty history entries per
    session. MD5 detects accidental corruption, not hostile replacement.

    Save flushes a unique temporary sibling before an atomic OS replacement.
    Failure leaves the prior committed file intact. The owning protocol must
    also restore its prepared in-memory rollback before returning a refusal.
    Load stages every session/history pair and validates the complete digest
    before returning any new owner. Corruption refuses; it never falls back to
    a sample and overwrites unknown user work. No listener or enrollment is owned. }
  TNyxStudioRuntimeStore = class
  private
    FDirectories: TNyxStudioDirectories;
    FLock: TFileStream;
  public
    constructor Create(const ADirectories: TNyxStudioDirectories);
    destructor Destroy; override;
    { False means genuinely absent; outputs remain nil. On success the caller
      owns both outputs and must keep Primary alive until Workspaces is freed.
      AVerifier explicitly executes accepted source after full digest/static
      admission. Exact repeated sources share evidence only within this load;
      every pair, pending buffer and history entry is admitted independently.
      Failure returns no owners and never rewrites committed checkpoint bytes.
      Nil retains compiler-independent strict literal recovery. }
    function Load(out APrimary: TNyxAgentSession;
      out AWorkspaces: TNyxStudioWorkspaces;
      const AVerifier: INyxRuntimeSourceVerifier = nil): Boolean;
    procedure Save(APrimary: TNyxAgentSession; AWorkspaces: TNyxStudioWorkspaces);
  end;

implementation

uses
  SysUtils, md5, nyx.model, nyx.source, nyx.studio.projects,
  nyx.studio.session, nyx.studio.history, nyx.studio.release, nyx.editing,
  nyx.codec, nyx.schema
  {$IFDEF MSWINDOWS}, Windows{$ELSE}, Unix, BaseUnix{$ENDIF};

const
  CRecoveryMagic: TNyxText = 'NYX-STUDIO-RUNTIME';
  CRecoveryVersion = 1;
  CRecoveryDraftVersion = 2;
  CMaximumFileBytes = Int64(512) * 1024 * 1024;
  CMaximumTextBytes = 4 * 1024 * 1024;
  { The older supported Windows unit omits the named write-through flag. }
  CMoveFileWriteThrough = $00000008;

type
  { Unadmitted file values. Neither serialized origins nor partially decoded
    history acquire a live source checkpoint. Read the complete digest/EOF
    before any optional application execution or candidate session construction. }
  TRecoverySessionInput = record
    Frame: TNyxAgentRecoveryFrame;
    Undo: array of TNyxProjectPair;
    Redo: array of TNyxProjectPair;
  end;
  { One load-scoped verifier cache. Exact accepted units may recur across current
    files and draft-only history; compile them once, compare every saved design,
    and never retain these execution capabilities across restarts or loads. }
  TRecoveryAdmission = class
  private
    FVerifier: INyxRuntimeSourceVerifier;
    FAccepted: array of record
      Source: TNyxText;
      Checkpoint: TNyxSourceCheckpoint;
    end;
    function AdmitPair(const APair: TNyxProjectPair): TNyxSourceCheckpoint;
  public
    constructor Create(const AVerifier: INyxRuntimeSourceVerifier);
    procedure RequireActive;
    function AdmitSession(const AInput: TRecoverySessionInput): TNyxAgentRecoveryFrame;
  end;
  { Both restored mutable owners stay private until their complete construction
    succeeds under the final creator-generation guard. No listener, application
    callback or filesystem publication runs inside this schema action. }
  TRecoveryPublication = class(TInterfacedObject, INyxSchemaAction)
  public
    PrimaryFrame: TNyxAgentRecoveryFrame;
    Registry: TNyxWorkspaceRecoveryFrame;
    Primary: TNyxAgentSession;
    Workspaces: TNyxStudioWorkspaces;
    destructor Destroy; override;
    procedure Execute;
  end;
  { Borrowed byte stream; the store owns opening/closing/flushing. Every framed
    byte enters the digest once. Digest bytes themselves are deliberately outside
    the digest, and exact EOF prevents accepting an appended/mixed checkpoint. }
  TRecoveryCodec = class
  private
    FStream: TStream;
    FDigest: TMD5Context;
    FVersion: Integer;
    procedure WriteBytes(const ABuffer; ACount: Integer);
    procedure ReadBytes(var ABuffer; ACount: Integer);
  public
    constructor Create(AStream: TStream);
    procedure WriteNumber(AValue: Integer);
    function ReadNumber: Integer;
    procedure WriteText(const AText: TNyxText);
    function ReadText: TNyxText;
    procedure WriteSession(const AFrame: TNyxAgentRecoveryFrame);
    function ReadSession: TRecoverySessionInput;
    property Version: Integer read FVersion write FVersion;
    procedure WriteDigest;
    procedure ReadDigest;
end;

constructor TRecoveryAdmission.Create(const AVerifier: INyxRuntimeSourceVerifier);
begin
  inherited Create;
  FVerifier := AVerifier;
end;

procedure TRecoveryAdmission.RequireActive;
begin

  if FVerifier <> nil then
  begin
    FVerifier.RequireActive;
  end;
end;

function TRecoveryAdmission.AdmitPair(const APair: TNyxProjectPair): TNyxSourceCheckpoint;
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
  LProjection: INyxSourceProjection;
  LIndex: Integer;
begin
  RequireActive;
  LDocument := nil;
  LWorkspace := nil;
  try
    for LIndex := 0 to High(FAccepted) do
    begin

      if FAccepted[LIndex].Source = APair.Source then
      begin
        AdmitNyxCapturedProject(APair, FAccepted[LIndex].Checkpoint,
          LDocument, LWorkspace, LResolved);
        Result := LWorkspace.Capture;
        Exit;
      end;
    end;

    if FVerifier = nil then
    begin
      AdmitNyxProject(APair, nprRequireMatch, LDocument, LWorkspace, LResolved);
    end
    else
    begin
      LProjection := FVerifier.Verify(APair.Source);

      if (LProjection = nil) or (LProjection.State <> spsExecuted) then
      begin

        if LProjection = nil then
        begin
          raise ENyxModel.Create('Runtime recovery compiler returned no execution result');
        end;
        raise ENyxModel.Create('Runtime recovery could not execute accepted Pascal: ' +
          LProjection.Message);
      end;
      AdmitNyxProjectedProject(APair, LProjection, LDocument, LWorkspace, LResolved);
    end;

    if (LResolved.Design <> APair.Design) or (LResolved.Source <> APair.Source) or
      (LResolved.Pending <> APair.Pending) or (LResolved.Draft <> APair.Draft) or
      (LResolved.DraftBase <> APair.DraftBase) then
    begin
      raise ENyxModel.Create('Runtime recovery must retain its exact canonical pair');
    end;
    Result := LWorkspace.Capture;
    LIndex := Length(FAccepted);
    SetLength(FAccepted, LIndex + 1);
    FAccepted[LIndex].Source := APair.Source;
    FAccepted[LIndex].Checkpoint := Result;
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

function TRecoveryAdmission.AdmitSession(
  const AInput: TRecoverySessionInput): TNyxAgentRecoveryFrame;
var
  LIndex: Integer;
begin
  Result := AInput.Frame;
  Result.Session.AcceptedCheckpoint := AdmitPair(AInput.Frame.Session.Pair);
  SetLength(Result.Session.Undo, Length(AInput.Undo));
  SetLength(Result.Session.Redo, Length(AInput.Redo));
  for LIndex := 0 to High(AInput.Undo) do
  begin
    Result.Session.Undo[LIndex] := NyxStudioCheckpoint(AdmitPair(AInput.Undo[LIndex]),
      AInput.Undo[LIndex].Draft, AInput.Undo[LIndex].DraftBase, AInput.Undo[LIndex].Pending);
  end;
  for LIndex := 0 to High(AInput.Redo) do
  begin
    Result.Session.Redo[LIndex] := NyxStudioCheckpoint(AdmitPair(AInput.Redo[LIndex]),
      AInput.Redo[LIndex].Draft, AInput.Redo[LIndex].DraftBase, AInput.Redo[LIndex].Pending);
  end;
end;

destructor TRecoveryPublication.Destroy;
begin
  Workspaces.Free;
  Primary.Free;
  inherited Destroy;
end;

procedure TRecoveryPublication.Execute;
begin
  Primary := TNyxAgentSession.CreateRecovered(PrimaryFrame);
  Workspaces := TNyxStudioWorkspaces.CreateRecovered(Primary, Registry);
end;

{ Static packet/property/navigation admission for the entire registry precedes
  execution. A corrupt late project must not cause earlier source initializers
  to run. Draft/base values are valid Unicode data, not Pascal to be interpreted. }
procedure ValidateRecoveryPair(const APair: TNyxProjectPair;
  const ASelection: TNyxText = ''; const AView: TNyxText = '');
var
  LDocument: TNyxDocument;
begin
  DecodeNyxProject(EncodeNyxProject(APair));
  NyxCompanionUnitName(APair.Source);
  LDocument := TNyxCodec.Decode(APair.Design);
  try
    ValidateNyxDocumentProperties(LDocument);

    if TNyxCodec.Encode(LDocument) <> APair.Design then
    begin
      raise ENyxModel.Create('Runtime recovery requires exact canonical design');
    end;

    if ((ASelection <> '') and (LDocument.Find(ASelection) = nil)) or
      ((AView <> '') and ((LDocument.Find(AView) = nil) or
        (LDocument.Find(AView).Parent <> nil))) then
    begin
      raise ENyxModel.Create('Runtime recovery navigation is outside the accepted document');
    end;
  finally
    LDocument.Free;
  end;
end;

procedure ValidateRecoveryInput(const AInput: TRecoverySessionInput);
var
  LIndex: Integer;
begin

  if AInput.Frame.Revision < 1 then
  begin
    raise ENyxModel.Create('Runtime recovery session revision must be positive');
  end;
  ValidateRecoveryPair(AInput.Frame.Session.Pair,
    AInput.Frame.Session.Selection, AInput.Frame.Session.View);
  for LIndex := 0 to High(AInput.Undo) do
  begin
    ValidateRecoveryPair(AInput.Undo[LIndex]);
  end;
  for LIndex := 0 to High(AInput.Redo) do
  begin
    ValidateRecoveryPair(AInput.Redo[LIndex]);
  end;
end;

constructor TRecoveryCodec.Create(AStream: TStream);
begin
  inherited Create;
  FVersion := CRecoveryVersion;
  FStream := AStream;
  MD5Init(FDigest);
end;

procedure TRecoveryCodec.WriteBytes(const ABuffer; ACount: Integer);
begin

  if FStream.Position + ACount > CMaximumFileBytes - 32 then
  begin
    raise ENyxModel.Create('Runtime checkpoint exceeds its file budget');
  end;
  FStream.WriteBuffer(ABuffer, ACount);
  { Older MD5 units declare their read-only buffer as var. The byte view avoids
    altering a const source or copying a potentially large authored text. }
  MD5Update(FDigest, PByte(@ABuffer)^, ACount);
end;

procedure TRecoveryCodec.ReadBytes(var ABuffer; ACount: Integer);
begin
  FStream.ReadBuffer(ABuffer, ACount);
  MD5Update(FDigest, ABuffer, ACount);
end;

procedure TRecoveryCodec.WriteNumber(AValue: Integer);
var
  LBytes: array[0..3] of Byte;
  LIndex: Integer;
begin

  if AValue < 0 then
  begin
    raise ENyxModel.Create('Runtime checkpoint integer must be nonnegative');
  end;
  for LIndex := 0 to 3 do
  begin
    LBytes[LIndex] := (LongWord(AValue) shr (LIndex * 8)) and $FF;
  end;
  WriteBytes(LBytes, SizeOf(LBytes));
end;

function TRecoveryCodec.ReadNumber: Integer;
var
  LBytes: array[0..3] of Byte;
  LValue: LongWord;
  LIndex: Integer;
begin
  ReadBytes(LBytes, SizeOf(LBytes));
  LValue := 0;
  for LIndex := 0 to 3 do
  begin
    LValue := LValue or (LongWord(LBytes[LIndex]) shl (LIndex * 8));
  end;

  if LValue > LongWord(High(Integer)) then
  begin
    raise ENyxModel.Create('Runtime checkpoint integer exceeds its budget');
  end;
  Result := Integer(LValue);
end;

procedure TRecoveryCodec.WriteText(const AText: TNyxText);
begin

  if Length(AText) > CMaximumTextBytes then
  begin
    raise ENyxModel.Create('Runtime checkpoint text exceeds 4 MiB');
  end;
  WriteNumber(Length(AText));

  if AText <> '' then
  begin
    WriteBytes(AText[1], Length(AText));
  end;
end;

function TRecoveryCodec.ReadText: TNyxText;
var
  LCount: Integer;
begin
  LCount := ReadNumber;

  if (LCount > CMaximumTextBytes) or (LCount > FStream.Size - FStream.Position - 32) then
  begin
    raise ENyxModel.Create('Runtime checkpoint text is truncated or exceeds its budget');
  end;
  SetLength(Result, LCount);
  SetCodePage(RawByteString(Result), CP_UTF8, False);

  if LCount > 0 then
  begin
    ReadBytes(Result[1], LCount);
  end;
  { Even unparsed pending drafts must be well-formed portable Unicode. }
  NyxTextScalarCount(Result);
end;

procedure TRecoveryCodec.WriteSession(const AFrame: TNyxAgentRecoveryFrame);
var
  LIndex: Integer;
begin
  WriteNumber(AFrame.Revision);
  WriteNumber(Ord(AFrame.Permission));
  WriteNumber(Ord(AFrame.Claimed));
  WriteText(AFrame.Session.Pair.Design);
  WriteText(AFrame.Session.Pair.Source);
  WriteText(AFrame.Session.Pair.Draft);
  WriteText(AFrame.Session.Pair.DraftBase);
  WriteNumber(Ord(AFrame.Session.Pair.Pending));
  WriteText(AFrame.Session.Selection);
  WriteText(AFrame.Session.View);
  WriteNumber(AFrame.Session.NextID);
  WriteNumber(Length(AFrame.Session.Undo));
  WriteNumber(Length(AFrame.Session.Redo));
  for LIndex := 0 to High(AFrame.Session.Undo) do
  begin
    WriteText(AFrame.Session.Undo[LIndex].Design);
    WriteText(AFrame.Session.Undo[LIndex].Source);

    if FVersion = 2 then
    begin
      WriteNumber(Ord(AFrame.Session.Undo[LIndex].Pending));

      if AFrame.Session.Undo[LIndex].Pending then
      begin
        WriteText(AFrame.Session.Undo[LIndex].Draft);
        WriteText(AFrame.Session.Undo[LIndex].DraftBase);
      end;
    end;
  end;
  for LIndex := 0 to High(AFrame.Session.Redo) do
  begin
    WriteText(AFrame.Session.Redo[LIndex].Design);
    WriteText(AFrame.Session.Redo[LIndex].Source);

    if FVersion = 2 then
    begin
      WriteNumber(Ord(AFrame.Session.Redo[LIndex].Pending));

      if AFrame.Session.Redo[LIndex].Pending then
      begin
        WriteText(AFrame.Session.Redo[LIndex].Draft);
        WriteText(AFrame.Session.Redo[LIndex].DraftBase);
      end;
    end;
  end;
end;

function TRecoveryCodec.ReadSession: TRecoverySessionInput;
var
  LValue: Integer;
  LUndo: Integer;
  LRedo: Integer;
  LIndex: Integer;
  LPair: TNyxProjectPair;
begin
  Result := Default(TRecoverySessionInput);
  Result.Frame.Revision := ReadNumber;
  LValue := ReadNumber;

  if LValue > Ord(High(TNyxAgentPermission)) then
  begin
    raise ENyxModel.Create('Runtime checkpoint permission is not supported');
  end;
  Result.Frame.Permission := TNyxAgentPermission(LValue);
  LValue := ReadNumber;

  if LValue > 1 then
  begin
    raise ENyxModel.Create('Runtime checkpoint claim must be Boolean');
  end;
  Result.Frame.Claimed := LValue = 1;
  Result.Frame.Session.Pair.Design := ReadText;
  Result.Frame.Session.Pair.Source := ReadText;
  Result.Frame.Session.Pair.Draft := ReadText;
  Result.Frame.Session.Pair.DraftBase := ReadText;
  LValue := ReadNumber;

  if LValue > 1 then
  begin
    raise ENyxModel.Create('Runtime checkpoint pending flag must be Boolean');
  end;
  Result.Frame.Session.Pair.Pending := LValue = 1;
  Result.Frame.Session.Selection := ReadText;
  Result.Frame.Session.View := ReadText;
  Result.Frame.Session.NextID := ReadNumber;
  LUndo := ReadNumber;
  LRedo := ReadNumber;

  if (LUndo > 50) or (LRedo > 50) or (LUndo + LRedo > 50) then
  begin
    raise ENyxModel.Create('Runtime checkpoint history exceeds fifty paired entries');
  end;
  SetLength(Result.Undo, LUndo);
  SetLength(Result.Redo, LRedo);
  for LIndex := 0 to LUndo + LRedo - 1 do
  begin
    LPair.Design := ReadText;
    LPair.Source := ReadText;
    LPair.Draft := '';
    LPair.DraftBase := '';
    LPair.Pending := False;

    if FVersion = 2 then
    begin
      LValue := ReadNumber;

      if LValue > 1 then
      begin
        raise ENyxModel.Create('Runtime history pending flag must be Boolean');
      end;
      LPair.Pending := LValue = 1;

      if LPair.Pending then
      begin
        LPair.Draft := ReadText;
        LPair.DraftBase := ReadText;
      end;
    end;

    if LIndex < LUndo then
    begin
      Result.Undo[LIndex] := LPair;
    end
    else
    begin
      Result.Redo[LIndex - LUndo] := LPair;
    end;
  end;
end;

procedure TRecoveryCodec.WriteDigest;
var
  LDigest: TMD5Digest;
  LText: TNyxText;
begin
  MD5Final(FDigest, LDigest);
  LText := MD5Print(LDigest);
  FStream.WriteBuffer(LText[1], 32);
end;

procedure TRecoveryCodec.ReadDigest;
var
  LDigest: TMD5Digest;
  LExpected: TNyxText;
  LActual: TNyxText;
begin
  MD5Final(FDigest, LDigest);
  LExpected := MD5Print(LDigest);
  SetLength(LActual, 32);
  FStream.ReadBuffer(LActual[1], 32);

  if (LExpected <> LActual) or (FStream.Position <> FStream.Size) then
  begin
    raise ENyxModel.Create('Runtime checkpoint digest or complete byte length disagrees');
  end;
end;

constructor TNyxStudioRuntimeRollback.Create(APrimary: TNyxAgentSession;
  AWorkspaces: TNyxStudioWorkspaces);
begin
  inherited Create;
  Primary := APrimary.Clone;
  Workspaces := AWorkspaces.Clone(Primary);
end;

destructor TNyxStudioRuntimeRollback.Destroy;
begin
  Workspaces.Free;
  Primary.Free;
  inherited Destroy;
end;

function TNyxStudioRuntimeRollback.Changed(APrimary: TNyxAgentSession;
  AWorkspaces: TNyxStudioWorkspaces): Boolean;
begin
  Result := (Primary.RecoveryStamp <> APrimary.RecoveryStamp) or
    (Workspaces.RecoveryStamp <> AWorkspaces.RecoveryStamp);
end;

procedure RequireOrdinaryCheckpoint(const APath: TNyxText);
{$IFDEF MSWINDOWS}
var
  LAttributes: DWORD;
  LPath: UnicodeString;
{$ENDIF}
begin

  if DirectoryExists(APath) then
  begin
    raise ENyxModel.Create('Runtime checkpoint path is a directory');
  end;
  {$IFDEF MSWINDOWS}
  LPath := UTF8Decode(APath);
  LAttributes := GetFileAttributesW(PWideChar(LPath));

  if (LAttributes <> INVALID_FILE_ATTRIBUTES) and
    ((LAttributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0) then
  begin
    raise ENyxModel.Create('Runtime checkpoint must not be a link or junction');
  end;
  {$ELSE}
  { The parent/root admission already refuses links. Existing file links also
    refuse rather than following a target outside private runtime storage. }

  if FileExists(APath) and (faSymLink and FileGetAttr(APath) <> 0) then
  begin
    raise ENyxModel.Create('Runtime checkpoint must not be a link');
  end;
  {$ENDIF}
end;

constructor TNyxStudioRuntimeStore.Create(const ADirectories: TNyxStudioDirectories);
var
  LLockPath: TNyxText;
begin
  inherited Create;
  ADirectories.Validate;
  FDirectories := ADirectories;
  ValidateNyxStudioDirectoryPath(ExtractFileDir(FDirectories.SessionCheckpoint), True);
  { OS admission owns the runtime for this store's process lifetime.
    Another new host must not overwrite its checkpoint or rotate its enrollment.
    The OS releases this handle even when the owning process terminates abruptly.
    Windows uses exclusive sharing; POSIX uses nonblocking advisory flock.
    Windows execution is qualified here; other OS/filesystem qualification remains
    part of the original native service/platform matrix. }
  LLockPath := FDirectories.SessionCheckpoint + '.lock';
  RequireOrdinaryCheckpoint(LLockPath);

  if not ForceDirectories(ExtractFileDir(LLockPath)) then
  begin
    raise ENyxModel.Create('Cannot create the private runtime lock directory');
  end;

  if FileExists(LLockPath) then
  begin
    FLock := TFileStream.Create(LLockPath, fmOpenReadWrite or fmShareExclusive);
  end
  else
  begin
    FLock := TFileStream.Create(LLockPath, fmCreate or fmShareExclusive);
  end;
  {$IFNDEF MSWINDOWS}

  if (fpFlock(FLock.Handle, LOCK_EX or LOCK_NB) <> 0) or
    (fpFcntl(FLock.Handle, F_SETFD, 1) <> 0) then
  begin
    raise ENyxModel.Create('Runtime is already owned or cannot admit a noninherited lock');
  end;
  {$ENDIF}
end;

destructor TNyxStudioRuntimeStore.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

function TNyxStudioRuntimeStore.Load(out APrimary: TNyxAgentSession;
  out AWorkspaces: TNyxStudioWorkspaces;
  const AVerifier: INyxRuntimeSourceVerifier): Boolean;
var
  LStream: TFileStream;
  LCodec: TRecoveryCodec;
  LInputs: array of TRecoverySessionInput;
  LRegistry: TNyxWorkspaceRecoveryFrame;
  LAdmission: TRecoveryAdmission;
  LPublication: TRecoveryPublication;
  LAction: INyxSchemaAction;
  LSchemaRevision: Integer;
  LIndex: Integer;
  LCount: Integer;
begin
  APrimary := nil;
  AWorkspaces := nil;
  FDirectories.Validate;
  ValidateNyxStudioDirectoryPath(ExtractFileDir(FDirectories.SessionCheckpoint), True);
  RequireOrdinaryCheckpoint(FDirectories.SessionCheckpoint);
  Result := FileExists(FDirectories.SessionCheckpoint);

  if not Result then
  begin
    Exit;
  end;
  LAdmission := nil;
  LSchemaRevision := NyxSchemaRevision;
  LRegistry := Default(TNyxWorkspaceRecoveryFrame);
  LStream := TFileStream.Create(FDirectories.SessionCheckpoint, fmOpenRead or fmShareDenyWrite);
  LCodec := nil;
  try

    if LStream.Size > CMaximumFileBytes then
    begin
      raise ENyxModel.Create('Runtime checkpoint exceeds its file budget');
    end;
    LCodec := TRecoveryCodec.Create(LStream);

    if LCodec.ReadText <> CRecoveryMagic then
    begin
      raise ENyxModel.Create('Runtime checkpoint format/version is not supported');
    end;
    LCodec.Version := LCodec.ReadNumber;

    if not (LCodec.Version in [1, 2]) then
    begin
      raise ENyxModel.Create('Runtime checkpoint format/version is not supported');
    end;
    SetLength(LInputs, 1);
    LInputs[0] := LCodec.ReadSession;
    LRegistry.Identity := LCodec.ReadText;
    LRegistry.Serial := LCodec.ReadNumber;
    LCount := LCodec.ReadNumber;

    if LCount > 8 then
    begin
      raise ENyxModel.Create('Runtime checkpoint contains too many ordinary projects');
    end;
    SetLength(LRegistry.Entries, LCount);
    SetLength(LInputs, LCount + 1);
    for LIndex := 0 to LCount - 1 do
    begin
      LRegistry.Entries[LIndex].Reference := NyxWorkspace(LCodec.ReadText);
      LRegistry.Entries[LIndex].LabelText := LCodec.ReadText;
      LInputs[LIndex + 1] := LCodec.ReadSession;
    end;
    LCodec.ReadDigest;
    TNyxStudioWorkspaces.ValidateRecoveryFrame(LRegistry);
    for LIndex := 0 to High(LInputs) do
    begin
      ValidateRecoveryInput(LInputs[LIndex]);
    end;
    LAdmission := TRecoveryAdmission.Create(AVerifier);
    LPublication := TRecoveryPublication.Create;
    LAction := LPublication;
    LPublication.PrimaryFrame := LAdmission.AdmitSession(LInputs[0]);
    for LIndex := 0 to LCount - 1 do
    begin
      LRegistry.Entries[LIndex].Session := LAdmission.AdmitSession(LInputs[LIndex + 1]);
    end;
    LPublication.Registry := LRegistry;
    LAdmission.RequireActive;

    if not CommitNyxSchemaRevision(LSchemaRevision, LAction) then
    begin
      raise ENyxModel.Create('Runtime recovery creators changed during source validation');
    end;
    APrimary := LPublication.Primary;
    LPublication.Primary := nil;
    AWorkspaces := LPublication.Workspaces;
    LPublication.Workspaces := nil;
  finally
    LAdmission.Free;
    LCodec.Free;
    LStream.Free;
  end;
end;

{ A current pending buffer was already supported by version 1. Only historical
  buffers need the extension; inspecting immutable values owns no live session. }
function HasDraftHistory(const AFrame: TNyxAgentRecoveryFrame): Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(AFrame.Session.Undo) do
  begin

    if AFrame.Session.Undo[LIndex].Pending then
    begin
      Exit(True);
    end;
  end;
  for LIndex := 0 to High(AFrame.Session.Redo) do
  begin

    if AFrame.Session.Redo[LIndex].Pending then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure TNyxStudioRuntimeStore.Save(APrimary: TNyxAgentSession;
  AWorkspaces: TNyxStudioWorkspaces);
var
  LStream: TFileStream;
  LCodec: TRecoveryCodec;
  LRegistry: TNyxWorkspaceRecoveryFrame;
  LPrimaryFrame: TNyxAgentRecoveryFrame;
  LTemporary: TNyxText;
  LIdentity: TGUID;
  LIndex: Integer;
  LVersion: Integer;
  {$IFDEF MSWINDOWS}
  LTemporaryWide: UnicodeString;
  LDestinationWide: UnicodeString;
  {$ENDIF}
begin
  { Keep ordinary accepted-only checkpoints in their exact version-1 layout.
    Version 2 is needed only when an Undo/Redo entry owns unfinished source.
    All frames are captured once before publication; mixed versions cannot occur. }
  LPrimaryFrame := APrimary.RecoveryFrame;
  LRegistry := AWorkspaces.RecoveryFrame;
  LVersion := CRecoveryVersion;

  if HasDraftHistory(LPrimaryFrame) then
  begin
    LVersion := CRecoveryDraftVersion;
  end;
  for LIndex := 0 to High(LRegistry.Entries) do
  begin

    if HasDraftHistory(LRegistry.Entries[LIndex].Session) then
    begin
      LVersion := CRecoveryDraftVersion;
    end;
  end;
  FDirectories.Validate;
  ValidateNyxStudioDirectoryPath(ExtractFileDir(FDirectories.SessionCheckpoint), True);
  RequireOrdinaryCheckpoint(FDirectories.SessionCheckpoint);

  if not ForceDirectories(ExtractFileDir(FDirectories.SessionCheckpoint)) then
  begin
    raise ENyxModel.Create('Cannot create the private runtime checkpoint directory');
  end;
  CreateGUID(LIdentity);
  LTemporary := FDirectories.SessionCheckpoint + '.' + GUIDToString(LIdentity) + '.next';
  LStream := nil;
  LCodec := nil;
  try
    LStream := TFileStream.Create(LTemporary, fmCreate or fmShareExclusive);
    LCodec := TRecoveryCodec.Create(LStream);
    LCodec.WriteText(CRecoveryMagic);
    LCodec.Version := LVersion;
    LCodec.WriteNumber(LVersion);
    LCodec.WriteSession(LPrimaryFrame);
    LCodec.WriteText(LRegistry.Identity);
    LCodec.WriteNumber(LRegistry.Serial);
    LCodec.WriteNumber(Length(LRegistry.Entries));
    for LIndex := 0 to High(LRegistry.Entries) do
    begin
      LCodec.WriteText(LRegistry.Entries[LIndex].Reference.ID);
      LCodec.WriteText(LRegistry.Entries[LIndex].LabelText);
      LCodec.WriteSession(LRegistry.Entries[LIndex].Session);
    end;
    LCodec.WriteDigest;

    if not FileFlush(LStream.Handle) then
    begin
      raise ENyxModel.Create('Cannot flush the complete runtime checkpoint');
    end;
    FreeAndNil(LCodec);
    FreeAndNil(LStream);
    {$IFDEF MSWINDOWS}
    LTemporaryWide := UTF8Decode(LTemporary);
    LDestinationWide := UTF8Decode(FDirectories.SessionCheckpoint);

    if not MoveFileExW(PWideChar(LTemporaryWide), PWideChar(LDestinationWide),
      MOVEFILE_REPLACE_EXISTING or CMoveFileWriteThrough) then
    {$ELSE}

    if not RenameFile(LTemporary, FDirectories.SessionCheckpoint) then
    {$ENDIF}
    begin
      raise ENyxModel.Create('Cannot atomically publish the runtime checkpoint; previous recovery is retained');
    end;
  finally
    LCodec.Free;
    LStream.Free;
    { Delete only our unique unpublished sibling, never a committed checkpoint
      or directory. A process crash may leave this inert sibling for inspection. }

    if FileExists(LTemporary) then
    begin
      SysUtils.DeleteFile(LTemporary);
    end;
  end;
end;

end.
