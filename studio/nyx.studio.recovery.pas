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
  Classes, nyx.text, nyx.studio.directories, nyx.studio.agents, nyx.studio.workspaces;

type
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
    Version 1 streams exact UTF-8 fields and paired history instead of escaping
    a potentially large registry into one 4 MiB JSON value. The file is bounded
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
      owns both outputs and must keep Primary alive until Workspaces is freed. }
    function Load(out APrimary: TNyxAgentSession;
      out AWorkspaces: TNyxStudioWorkspaces): Boolean;
    procedure Save(APrimary: TNyxAgentSession; AWorkspaces: TNyxStudioWorkspaces);
  end;

implementation

uses
  SysUtils, md5, nyx.model, nyx.source, nyx.studio.projects,
  nyx.studio.session, nyx.studio.release, nyx.editing
  {$IFDEF MSWINDOWS}, Windows{$ELSE}, Unix, BaseUnix{$ENDIF};

const
  CRecoveryMagic: TNyxText = 'NYX-STUDIO-RUNTIME';
  CRecoveryVersion = 1;
  CMaximumFileBytes = Int64(512) * 1024 * 1024;
  CMaximumTextBytes = 4 * 1024 * 1024;
  { The older supported Windows unit omits the named write-through flag. }
  CMoveFileWriteThrough = $00000008;

type
  { Borrowed byte stream; the store owns opening/closing/flushing. Every framed
    byte enters the digest once. Digest bytes themselves are deliberately outside
    the digest, and exact EOF prevents accepting an appended/mixed checkpoint. }
  TRecoveryCodec = class
  private
    FStream: TStream;
    FDigest: TMD5Context;
    procedure WriteBytes(const ABuffer; ACount: Integer);
    procedure ReadBytes(var ABuffer; ACount: Integer);
  public
    constructor Create(AStream: TStream);
    procedure WriteNumber(AValue: Integer);
    function ReadNumber: Integer;
    procedure WriteText(const AText: TNyxText);
    function ReadText: TNyxText;
    procedure WriteSession(const AFrame: TNyxAgentRecoveryFrame);
    function ReadSession: TNyxAgentRecoveryFrame;
    procedure WriteDigest;
    procedure ReadDigest;
  end;

constructor TRecoveryCodec.Create(AStream: TStream);
begin
  inherited Create;
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
  end;
  for LIndex := 0 to High(AFrame.Session.Redo) do
  begin
    WriteText(AFrame.Session.Redo[LIndex].Design);
    WriteText(AFrame.Session.Redo[LIndex].Source);
  end;
end;

function TRecoveryCodec.ReadSession: TNyxAgentRecoveryFrame;
var
  LValue: Integer;
  LUndo: Integer;
  LRedo: Integer;
  LIndex: Integer;
  LPair: TNyxProjectPair;
  LResolved: TNyxProjectPair;
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LCheckpoint: TNyxSourceCheckpoint;
begin
  Result.Revision := ReadNumber;
  LValue := ReadNumber;

  if LValue > Ord(High(TNyxAgentPermission)) then
  begin
    raise ENyxModel.Create('Runtime checkpoint permission is not supported');
  end;
  Result.Permission := TNyxAgentPermission(LValue);
  LValue := ReadNumber;

  if LValue > 1 then
  begin
    raise ENyxModel.Create('Runtime checkpoint claim must be Boolean');
  end;
  Result.Claimed := LValue = 1;
  Result.Session.Pair.Design := ReadText;
  Result.Session.Pair.Source := ReadText;
  Result.Session.Pair.Draft := ReadText;
  Result.Session.Pair.DraftBase := ReadText;
  LValue := ReadNumber;

  if LValue > 1 then
  begin
    raise ENyxModel.Create('Runtime checkpoint pending flag must be Boolean');
  end;
  Result.Session.Pair.Pending := LValue = 1;
  Result.Session.Selection := ReadText;
  Result.Session.View := ReadText;
  Result.Session.NextID := ReadNumber;
  LUndo := ReadNumber;
  LRedo := ReadNumber;

  if (LUndo > 50) or (LRedo > 50) or (LUndo + LRedo > 50) then
  begin
    raise ENyxModel.Create('Runtime checkpoint history exceeds fifty paired entries');
  end;
  SetLength(Result.Session.Undo, LUndo);
  SetLength(Result.Session.Redo, LRedo);
  for LIndex := 0 to LUndo + LRedo - 1 do
  begin
    LPair.Design := ReadText;
    LPair.Source := ReadText;
    LPair.Draft := '';
    LPair.DraftBase := '';
    LPair.Pending := False;
    AdmitNyxProject(LPair, nprRequireMatch, LDocument, LWorkspace, LResolved);
    try

      if (LResolved.Design <> LPair.Design) or (LResolved.Source <> LPair.Source) then
      begin
        raise ENyxModel.Create('Runtime history must retain its exact canonical pair');
      end;
      LCheckpoint := LWorkspace.Capture;

      if LIndex < LUndo then
      begin
        Result.Session.Undo[LIndex] := LCheckpoint;
      end
      else
      begin
        Result.Session.Redo[LIndex - LUndo] := LCheckpoint;
      end;
    finally
      LWorkspace.Free;
      LDocument.Free;
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
  out AWorkspaces: TNyxStudioWorkspaces): Boolean;
var
  LStream: TFileStream;
  LCodec: TRecoveryCodec;
  LPrimaryFrame: TNyxAgentRecoveryFrame;
  LRegistry: TNyxWorkspaceRecoveryFrame;
  LPrimary: TNyxAgentSession;
  LWorkspaces: TNyxStudioWorkspaces;
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
  LPrimary := nil;
  LWorkspaces := nil;
  LStream := TFileStream.Create(FDirectories.SessionCheckpoint, fmOpenRead or fmShareDenyWrite);
  LCodec := nil;
  try

    if LStream.Size > CMaximumFileBytes then
    begin
      raise ENyxModel.Create('Runtime checkpoint exceeds its file budget');
    end;
    LCodec := TRecoveryCodec.Create(LStream);

    if (LCodec.ReadText <> CRecoveryMagic) or (LCodec.ReadNumber <> CRecoveryVersion) then
    begin
      raise ENyxModel.Create('Runtime checkpoint format/version is not supported');
    end;
    LPrimaryFrame := LCodec.ReadSession;
    LRegistry.Identity := LCodec.ReadText;
    LRegistry.Serial := LCodec.ReadNumber;
    LCount := LCodec.ReadNumber;

    if LCount > 8 then
    begin
      raise ENyxModel.Create('Runtime checkpoint contains too many ordinary projects');
    end;
    SetLength(LRegistry.Entries, LCount);
    for LIndex := 0 to LCount - 1 do
    begin
      LRegistry.Entries[LIndex].Reference := NyxWorkspace(LCodec.ReadText);
      LRegistry.Entries[LIndex].LabelText := LCodec.ReadText;
      LRegistry.Entries[LIndex].Session := LCodec.ReadSession;
    end;
    LCodec.ReadDigest;
    LPrimary := TNyxAgentSession.CreateRecovered(LPrimaryFrame);
    LWorkspaces := TNyxStudioWorkspaces.CreateRecovered(LPrimary, LRegistry);
    APrimary := LPrimary;
    LPrimary := nil;
    AWorkspaces := LWorkspaces;
    LWorkspaces := nil;
  finally
    LWorkspaces.Free;
    LPrimary.Free;
    LCodec.Free;
    LStream.Free;
  end;
end;

procedure TNyxStudioRuntimeStore.Save(APrimary: TNyxAgentSession;
  AWorkspaces: TNyxStudioWorkspaces);
var
  LStream: TFileStream;
  LCodec: TRecoveryCodec;
  LRegistry: TNyxWorkspaceRecoveryFrame;
  LTemporary: TNyxText;
  LIdentity: TGUID;
  LIndex: Integer;
  {$IFDEF MSWINDOWS}
  LTemporaryWide: UnicodeString;
  LDestinationWide: UnicodeString;
  {$ENDIF}
begin
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
    LCodec.WriteNumber(CRecoveryVersion);
    LCodec.WriteSession(APrimary.RecoveryFrame);
    LRegistry := AWorkspaces.RecoveryFrame;
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
