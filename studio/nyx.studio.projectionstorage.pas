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

unit nyx.studio.projectionstorage;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, nyx.text, nyx.bytes, nyx.model, nyx.codegen, nyx.studio.builds,
  nyx.studio.sourceprojection
  {$ifdef MSWINDOWS}, Windows{$endif};

type
  { Retention concerns a fresh constructor job, never application outputs or
    existing jobs. Evidence retains source, result and compiler/execution logs.
    Browser packages are always preserved regardless of this native choice. }
  TNyxProjectionRetention = (prEvidence, prAllFiles);

  { Trusted host configuration, copied before worker delegation. A wire request
    cannot alter retention, pass a directory or disable capacity admission.
    Default: native evidence only, at least 128 MiB available before allocation.
    The check is a preflight, not a reservation or a guarantee against disk full.
    Zero explicitly disables that check; callers may raise or lower the threshold. }
  TNyxProjectionStoragePolicy = record
  private
    FRetention: TNyxProjectionRetention;
    FMinimumFreeBytes: Int64;
  public
    class function Default: TNyxProjectionStoragePolicy; static;
    function Retaining(AValue: TNyxProjectionRetention): TNyxProjectionStoragePolicy;
    function MinimumFreeBytes(AValue: Int64): TNyxProjectionStoragePolicy;
    { Unknown choices and negative byte counts raise before filesystem access. }
    procedure Validate;
    property Retention: TNyxProjectionRetention read FRetention;
    property RequiredFreeBytes: Int64 read FMinimumFreeBytes;
  end;

  TNyxProjectionStorageState = (psNotAllocated, psPreserved, psRetired,
    psDeferred, psCapacityRefused);

  { Detached observation, not deletion authority. DeferredItems counts bounded
    file/directory/admission refusals, rather than promising a recursive inventory.
    A deferred report may still have retired other independently verified files.
    The projection's compile/execution state stays independent of this report. }
  TNyxProjectionStorageReport = record
    State: TNyxProjectionStorageState;
    CapturedFiles: Integer;
    RetiredFiles: Integer;
    DeferredItems: Integer;
    RetiredBytes: Int64;
    Message: TNyxText;
    class function Unallocated: TNyxProjectionStorageReport; static;
  end;

  { Optional native host facet. Existing projection/wire receipt shapes and GUIDs
    stay intact. Transport-decoded receipts cannot claim local storage evidence.
    Retaining this interface holds only immutable values and the detached build,
    never a filesystem handle, executor, process or editor. }
  INyxProjectionStorageBuild = interface(INyxSourceProjectionBuild)
    ['{ACC6A3A6-9D2C-4D39-89A7-101026000001}']
    function GetStorage: TNyxProjectionStorageReport;
    property Storage: TNyxProjectionStorageReport read GetStorage;
  end;

  ENyxProjectionCapacity = class(ENyxModel);

  {$ifdef MSWINDOWS}
  TNyxProjectionDerivative = record
    Name: TNyxText;
    Handle: THandle;
    Information: BY_HANDLE_FILE_INFORMATION;
    Digest: String;
  end;
  {$endif}

  { Native host adapter for one exclusively new invocation. The Win32 adapter
    pins ordinary ancestors and both job directories, refuses reparses/links,
    and never adopts an existing invocation. It does not sweep any old job.
    Other native hosts refuse before allocation until their ownership adapter
    is qualified; the existing application Build path is independent.

    The caller MUST capture only after the compiler family joins, and Finish
    only after the constructor family also joins. Destructor only closes owned
    handles: it never guesses that an active compiler has retired. Enumeration
    is flat and bounded; deletion uses verified opened handles, never a name.
    This is not a sandbox for the trusted Pascal constructor's own file access. }
  TNyxOwnedProjectionStorage = class
  private
    FDirectory: TNyxText;
    FSourceName: TNyxText;
    FPolicy: TNyxProjectionStoragePolicy;
    FTarget: TNyxBuildTarget;
    FReport: TNyxProjectionStorageReport;
    FCaptured: Boolean;
    FFinished: Boolean;
    {$ifdef MSWINDOWS}
    FDirectoryHandles: array of THandle;
    FEvidenceHandles: array of THandle;
    FEntries: array of TNyxProjectionDerivative;
    FHashedBytes: Int64;
    procedure PinDirectory(const APath: TNyxText);
    procedure CaptureFile(const AName: TNyxText);
    procedure CaptureDirectory(const ARelative: TNyxText; AFinal: Boolean);
    {$endif}
    procedure Defer(const AMessage: TNyxText);
    function EvidenceName(const AName: TNyxText): Boolean;
  public
    { AAdmission receives the refusal even when construction raises. Jobs may
      be created as admitted parent directories, but the invocation itself is
      never allocated when the capacity check refuses. References must be fresh
      host-minted job identities; unit names have already passed codegen admission. }
    constructor Create(const AJobs: TNyxText; const AReference: TNyxSourceProjectionRef;
      const AUnitName: TNyxText; ATarget: TNyxBuildTarget;
      const APolicy: TNyxProjectionStoragePolicy;
      out AAdmission: TNyxProjectionStorageReport);
    destructor Destroy; override;
    { Closed source/wrapper/log evidence names only. Exclusive byte creation
      refuses existing files rather than overwriting a constructor's changes. }
    procedure WriteEvidence(const AName, AText: TNyxText);
    { Native result read is bounded before allocation and refuses linked files.
      The exact bytes are independent of the stream lifetime. }
    function ReadResult: TNyxBytes;
    { One capture after compiler join. Native derivatives are ordinary flat
      units and the fixed wrapper executable/linker products. Unknown entries
      and budget failures defer; no recursive traversal follows them. }
    procedure CaptureCompilerDerivatives;
    { Idempotent after all owned processes join. Identity, single-link count,
      size, timestamps and SHA-256 must still match; locked/changed/moved files
      remain. Both browser packages and the all-files native override stay intact. }
    procedure FinishAfterJoin;
    function Snapshot: TNyxProjectionStorageReport;
    property Directory: TNyxText read FDirectory;
  end;

{ Attach a detached storage observation without changing the existing build API. }
function WithNyxProjectionStorage(const ABuild: INyxSourceProjectionBuild;
  const AReport: TNyxProjectionStorageReport): INyxSourceProjectionBuild;

implementation

{$ifdef MSWINDOWS}
uses
  fpsha256;

const
  CDeleteAccess = $00010000;
  CFileDisposition = 4;
  CMaximumFiles = 4096;
  CMaximumFileBytes = 64 * Int64(1024) * 1024;
  CMaximumHashBytes = 256 * Int64(1024) * 1024;

{ Microsoft FILE_DISPOSITION_INFO has one Win32 BOOLEAN (one byte), unlike BOOL.
  This missing RTL surface is declared exactly, with no ABI-dependent record.
  See docs/build-storage.md for the authoritative Win32 references. }
function SetFileInformationByHandle(AFile: THandle; AClass: Integer;
  AInformation: Pointer; ALength: Cardinal): BOOL;
  stdcall; external 'kernel32' name 'SetFileInformationByHandle';
function GetDiskFreeSpaceExW(ADirectory: PWideChar; AAvailable, ATotal,
  AFree: Pointer): BOOL;
  stdcall; external 'kernel32' name 'GetDiskFreeSpaceExW';

function OpenFile(const APath: TNyxText; AAccess, ASharing: Cardinal): THandle;
var
  LPath: UnicodeString;
begin
  LPath := UTF8Decode(APath);
  Result := CreateFileW(PWideChar(LPath), AAccess, ASharing, nil, OPEN_EXISTING,
    FILE_FLAG_OPEN_REPARSE_POINT or FILE_FLAG_BACKUP_SEMANTICS, 0);
end;

function OrdinaryFile(AHandle: THandle; out AInfo: BY_HANDLE_FILE_INFORMATION): Boolean;
begin
  Result := GetFileInformationByHandle(AHandle, @AInfo) and
    ((AInfo.dwFileAttributes and (FILE_ATTRIBUTE_REPARSE_POINT or
      FILE_ATTRIBUTE_DIRECTORY)) = 0) and (AInfo.nNumberOfLinks = 1);
end;

function SameFile(const ALeft, ARight: BY_HANDLE_FILE_INFORMATION): Boolean;
begin
  { Ignore access times: reading retained evidence must not invalidate ownership. }
  Result := (ALeft.dwVolumeSerialNumber = ARight.dwVolumeSerialNumber) and
    (ALeft.nFileIndexHigh = ARight.nFileIndexHigh) and
    (ALeft.nFileIndexLow = ARight.nFileIndexLow) and
    (ALeft.nFileSizeHigh = ARight.nFileSizeHigh) and
    (ALeft.nFileSizeLow = ARight.nFileSizeLow) and
    (ALeft.ftCreationTime.dwHighDateTime = ARight.ftCreationTime.dwHighDateTime) and
    (ALeft.ftCreationTime.dwLowDateTime = ARight.ftCreationTime.dwLowDateTime) and
    (ALeft.ftLastWriteTime.dwHighDateTime = ARight.ftLastWriteTime.dwHighDateTime) and
    (ALeft.ftLastWriteTime.dwLowDateTime = ARight.ftLastWriteTime.dwLowDateTime);
end;

function FileBytes(const AInfo: BY_HANDLE_FILE_INFORMATION): Int64;
begin
  Result := Int64(AInfo.nFileSizeHigh) * Int64($100000000) + AInfo.nFileSizeLow;
end;

function Digest(AHandle: THandle): String;
var
  LStream: THandleStream;
begin
  LStream := THandleStream.Create(AHandle);
  try
    LStream.Position := 0;
    Result := TSHA256.StreamHexa(LStream);
  finally
    { THandleStream borrows its handle; the invocation retains identity. }
    LStream.Free;
  end;
end;
{$endif}

procedure AdmitSourceFile(const AUnitName: TNyxText);
var
  LStem: TNyxText;
  LDot: Integer;
begin
  TNyxCodegen.AdmitUnitName(AUnitName);
  { Windows recognizes DOS devices even with an extension. A valid Pascal
    identifier therefore is not sufficient admission for an ordinary source
    file. These ASCII comparisons also protect the host-independent API. }
  LDot := Pos('.', AUnitName);
  LStem := AUnitName;

  if LDot > 0 then
  begin
    LStem := Copy(AUnitName, 1, LDot - 1);
  end;
  LStem := TNyxText(UpperCase(LStem));

  if (LStem = 'CON') or (LStem = 'PRN') or (LStem = 'AUX') or (LStem = 'NUL') or
    ((Length(LStem) = 4) and ((Copy(LStem, 1, 3) = 'COM') or
      (Copy(LStem, 1, 3) = 'LPT')) and (LStem[4] in ['1'..'9'])) then
  begin
    raise ENyxModel.Create('Projection unit name must identify an ordinary source file');
  end;
end;

type
  TNyxProjectionStorageBuild = class(TInterfacedObject, INyxSourceProjectionBuild,
    INyxProjectionStorageBuild)
  private
    FBuild: INyxSourceProjectionBuild;
    FReport: TNyxProjectionStorageReport;
  public
    constructor Create(const ABuild: INyxSourceProjectionBuild;
      const AReport: TNyxProjectionStorageReport);
    function GetReference: TNyxSourceProjectionRef;
    function GetProjection: INyxSourceProjection;
    function GetArtifact: TNyxText;
    function GetRuntimeLog: TNyxText;
    function GetStorage: TNyxProjectionStorageReport;
  end;

class function TNyxProjectionStoragePolicy.Default: TNyxProjectionStoragePolicy;
begin
  Result.FRetention := prEvidence;
  Result.FMinimumFreeBytes := 128 * Int64(1024) * 1024;
end;

function TNyxProjectionStoragePolicy.Retaining(
  AValue: TNyxProjectionRetention): TNyxProjectionStoragePolicy;
begin
  Result := Self;
  Result.FRetention := AValue;
  Result.Validate;
end;

function TNyxProjectionStoragePolicy.MinimumFreeBytes(
  AValue: Int64): TNyxProjectionStoragePolicy;
begin
  Result := Self;
  Result.FMinimumFreeBytes := AValue;
  Result.Validate;
end;

procedure TNyxProjectionStoragePolicy.Validate;
begin

  if (Ord(FRetention) < Ord(Low(TNyxProjectionRetention))) or
    (Ord(FRetention) > Ord(High(TNyxProjectionRetention))) or
    (FMinimumFreeBytes < 0) then
  begin
    raise ENyxModel.Create('Invalid projection storage policy');
  end;
end;

class function TNyxProjectionStorageReport.Unallocated: TNyxProjectionStorageReport;
begin
  Result.State := psNotAllocated;
  Result.CapturedFiles := 0;
  Result.RetiredFiles := 0;
  Result.DeferredItems := 0;
  Result.RetiredBytes := 0;
  Result.Message := '';
end;

procedure TNyxOwnedProjectionStorage.Defer(const AMessage: TNyxText);
begin
  FReport.State := psDeferred;
  Inc(FReport.DeferredItems);

  if FReport.Message = '' then
  begin
    FReport.Message := AMessage;
  end;
end;

{$ifdef MSWINDOWS}
procedure TNyxOwnedProjectionStorage.PinDirectory(const APath: TNyxText);
var
  LHandle: THandle;
  LInfo: BY_HANDLE_FILE_INFORMATION;
  LIndex: Integer;
begin
  { Share neither writes nor deletes on the directory itself. Ordinary child
    file creation is still permitted. Each ancestor is pinned before resolving
    its next child; opening the final component without reparse processing alone
    would not protect linked or replaceable ancestors. Handles are non-inherited. }
  { Metadata-only opens do not participate in Win32 sharing enforcement.
    Actual read access is required to deny a reparse writer on this directory. }
  LHandle := OpenFile(APath, GENERIC_READ, FILE_SHARE_READ);

  if LHandle = INVALID_HANDLE_VALUE then
  begin
    raise ENyxModel.Create('Cannot pin projection directory ownership');
  end;
  try

    if not GetFileInformationByHandle(LHandle, @LInfo) or
      ((LInfo.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0) or
      ((LInfo.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0) then
    begin
      raise ENyxModel.Create('Projection directory ownership refuses linked paths');
    end;
    LIndex := Length(FDirectoryHandles);
    SetLength(FDirectoryHandles, LIndex + 1);
    FDirectoryHandles[LIndex] := LHandle;
  except
    CloseHandle(LHandle);
    raise;
  end;
end;
{$endif}

constructor TNyxOwnedProjectionStorage.Create(const AJobs: TNyxText;
  const AReference: TNyxSourceProjectionRef; const AUnitName: TNyxText;
  ATarget: TNyxBuildTarget; const APolicy: TNyxProjectionStoragePolicy;
  out AAdmission: TNyxProjectionStorageReport);
{$ifdef MSWINDOWS}
var
  LRoot: TNyxText;
  LPath: TNyxText;
  LWide: UnicodeString;
  LIndex: Integer;
  LAvailable: QWord;
  LTotal: QWord;
  LFree: QWord;
{$endif}
begin
  inherited Create;
  AAdmission := TNyxProjectionStorageReport.Unallocated;
  FReport := AAdmission;
  APolicy.Validate;
  ValidateNyxProjectionTarget(ATarget);
  AdmitSourceFile(AUnitName);
  NyxSourceProjectionRef(AReference.Name);

  if (Copy(AReference.Name, 1, 4) <> 'job-') or (AUnitName = '') or
    (Pos('/', AUnitName) <> 0) or (Pos('\', AUnitName) <> 0) or
    (Pos(':', AUnitName) <> 0) then
  begin
    raise ENyxModel.Create('Projection storage requires admitted host identities');
  end;
  FSourceName := AUnitName + '.pas';
  FPolicy := APolicy;
  FTarget := ATarget;
  try
    {$ifdef MSWINDOWS}
    LRoot := IncludeTrailingPathDelimiter(ExpandFileName(AJobs));

    if (Length(LRoot) < 3) or (LRoot[2] <> ':') or (LRoot[3] <> '\') or
      (Pos(#0, LRoot) <> 0) then
    begin
      raise ENyxModel.Create('Projection storage requires an ordinary local drive');
    end;
    LPath := Copy(LRoot, 1, 3);
    PinDirectory(LPath);
    LIndex := 4;
    while LIndex <= Length(LRoot) do
    begin

      if LRoot[LIndex] = '\' then
      begin
        LPath := Copy(LRoot, 1, LIndex - 1);
        LWide := UTF8Decode(LPath);

        if not CreateDirectoryW(PWideChar(LWide), nil) and
          (GetLastError <> ERROR_ALREADY_EXISTS) then
        begin
          raise ENyxModel.Create('Cannot prepare projection parent directory');
        end;
        PinDirectory(LPath);
      end;
      Inc(LIndex);
    end;
    LWide := UTF8Decode(LRoot);

    if not GetDiskFreeSpaceExW(PWideChar(LWide), @LAvailable, @LTotal, @LFree) then
    begin
      raise ENyxModel.Create('Cannot establish available projection storage');
    end;

    if LAvailable < QWord(APolicy.RequiredFreeBytes) then
    begin
      FReport.State := psCapacityRefused;
      FReport.Message := 'Insufficient available build storage; free space or tune the host policy';
      AAdmission := FReport;
      raise ENyxProjectionCapacity.Create(FReport.Message);
    end;
    FDirectory := LRoot + AReference.Name + PathDelim;
    LWide := UTF8Decode(ExcludeTrailingPathDelimiter(FDirectory));

    if not CreateDirectoryW(PWideChar(LWide), nil) then
    begin
      raise ENyxModel.Create('Projection invocation directory must be exclusively new');
    end;
    PinDirectory(ExcludeTrailingPathDelimiter(FDirectory));
    LWide := UTF8Decode(FDirectory + 'units');

    if not CreateDirectoryW(PWideChar(LWide), nil) then
    begin
      raise ENyxModel.Create('Projection units directory must be exclusively new');
    end;
    PinDirectory(FDirectory + 'units');
    FReport.State := psPreserved;
    AAdmission := FReport;
    {$else}
    raise ENyxModel.Create('Projection storage ownership adapter is unavailable on this host');
    {$endif}
  except
    on LException: ENyxProjectionCapacity do
    begin
      raise;
    end;
    on LException: Exception do
    begin
      Defer('Projection directory ownership could not be established');
      AAdmission := FReport;
      raise;
    end;
  end;
end;

destructor TNyxOwnedProjectionStorage.Destroy;
{$ifdef MSWINDOWS}
var
  LIndex: Integer;
{$endif}
begin
  {$ifdef MSWINDOWS}
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex].Handle <> INVALID_HANDLE_VALUE then
    begin
      CloseHandle(FEntries[LIndex].Handle);
    end;
  end;
  for LIndex := High(FDirectoryHandles) downto 0 do
  begin
    CloseHandle(FDirectoryHandles[LIndex]);
  end;
  for LIndex := 0 to High(FEvidenceHandles) do
  begin
    CloseHandle(FEvidenceHandles[LIndex]);
  end;
  {$endif}
  inherited Destroy;
end;

function TNyxOwnedProjectionStorage.EvidenceName(const AName: TNyxText): Boolean;
begin
  Result := (AName = FSourceName) or (AName = 'nyx_projection.lpr') or
    (AName = 'compiler.log') or (AName = 'execution.log') or
    (AName = NyxProjectionResultFile);
end;

procedure TNyxOwnedProjectionStorage.WriteEvidence(const AName, AText: TNyxText);
{$ifdef MSWINDOWS}
var
  LWide: UnicodeString;
  LHandle: THandle;
  LStream: THandleStream;
  LInfo: BY_HANDLE_FILE_INFORMATION;
  LReadInfo: BY_HANDLE_FILE_INFORMATION;
  LRead: TNyxBytes;
  LExpected: TNyxBytes;
  LIndex: Integer;
{$endif}
begin

  if FFinished or not EvidenceName(AName) or (AName = NyxProjectionResultFile) then
  begin
    raise ENyxModel.Create('Projection evidence write requires an owned source or log name');
  end;
  {$ifdef MSWINDOWS}
  LWide := UTF8Decode(FDirectory + AName);
  LHandle := CreateFileW(PWideChar(LWide), GENERIC_WRITE, 0, nil, CREATE_NEW,
    FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OPEN_REPARSE_POINT, 0);

  if LHandle = INVALID_HANDLE_VALUE then
  begin
    Defer('Projection evidence could not be created exclusively');
    raise ENyxModel.Create('Cannot retain projection evidence');
  end;
  try
    LStream := THandleStream.Create(LHandle);
    try

      if AText <> '' then
      begin
        LStream.WriteBuffer(AText[1], Length(AText));
      end;
    finally
      LStream.Free;
    end;

    if not GetFileInformationByHandle(LHandle, @LInfo) then
    begin
      raise ENyxModel.Create('Cannot establish retained evidence identity');
    end;
  except
    on LException: Exception do
    begin
      CloseHandle(LHandle);
      Defer('Projection evidence byte write did not complete');
      raise;
    end;
  end;
  CloseHandle(LHandle);
  LHandle := OpenFile(FDirectory + AName, GENERIC_READ, FILE_SHARE_READ);

  if LHandle = INVALID_HANDLE_VALUE then
  begin
    Defer('Projection evidence could not be pinned after creation');
    raise ENyxModel.Create('Cannot pin retained projection evidence');
  end;
  try

    if not OrdinaryFile(LHandle, LReadInfo) or
      (LInfo.dwVolumeSerialNumber <> LReadInfo.dwVolumeSerialNumber) or
      (LInfo.nFileIndexHigh <> LReadInfo.nFileIndexHigh) or
      (LInfo.nFileIndexLow <> LReadInfo.nFileIndexLow) or
      (FileBytes(LReadInfo) <> Length(AText)) then
    begin
      raise ENyxModel.Create('Retained projection evidence changed during creation');
    end;
    LExpected := NyxEncodeUTF8(AText);
    SetLength(LRead, Length(LExpected));
    LStream := THandleStream.Create(LHandle);
    try

      if Length(LRead) > 0 then
      begin
        LStream.ReadBuffer(LRead[0], Length(LRead));
      end;
    finally
      LStream.Free;
    end;

    if (Length(LRead) > 0) and
      not CompareMem(@LRead[0], @LExpected[0], Length(LRead)) then
    begin
      raise ENyxModel.Create('Retained projection evidence bytes changed during creation');
    end;
    LIndex := Length(FEvidenceHandles);
    SetLength(FEvidenceHandles, LIndex + 1);
    FEvidenceHandles[LIndex] := LHandle;
    LHandle := INVALID_HANDLE_VALUE;
  except
    on LException: Exception do
    begin
      CloseHandle(LHandle);
      Defer('Projection evidence could not retain its exact bytes');
      raise;
    end;
  end;
  {$else}
  raise ENyxModel.Create('Projection storage ownership adapter is unavailable');
  {$endif}
end;

function TNyxOwnedProjectionStorage.ReadResult: TNyxBytes;
{$ifdef MSWINDOWS}
var
  LHandle: THandle;
  LInfo: BY_HANDLE_FILE_INFORMATION;
  LStream: THandleStream;
  LIndex: Integer;
{$endif}
begin
  Result := nil;
  {$ifdef MSWINDOWS}
  LHandle := OpenFile(FDirectory + NyxProjectionResultFile, GENERIC_READ, FILE_SHARE_READ);

  if LHandle = INVALID_HANDLE_VALUE then
  begin
    raise ENyxModel.Create('Cannot read the owned projection result');
  end;
  try

    if not OrdinaryFile(LHandle, LInfo) or (FileBytes(LInfo) < 1) or
      (FileBytes(LInfo) > NyxProjectionMaximumResultBytes) then
    begin
      raise ENyxModel.Create('Projection result refuses linked or excessive data');
    end;
    LStream := THandleStream.Create(LHandle);
    try
      SetLength(Result, FileBytes(LInfo));
      LStream.ReadBuffer(Result[0], Length(Result));
    finally
      LStream.Free;
    end;
    LIndex := Length(FEvidenceHandles);
    SetLength(FEvidenceHandles, LIndex + 1);
    FEvidenceHandles[LIndex] := LHandle;
    LHandle := INVALID_HANDLE_VALUE;
  finally

    if LHandle <> INVALID_HANDLE_VALUE then
    begin
      CloseHandle(LHandle);
    end;
  end;
  {$else}
  raise ENyxModel.Create('Projection storage ownership adapter is unavailable');
  {$endif}
end;

{$ifdef MSWINDOWS}
procedure TNyxOwnedProjectionStorage.CaptureFile(const AName: TNyxText);
var
  LHandle: THandle;
  LFrozen: THandle;
  LInfo: BY_HANDLE_FILE_INFORMATION;
  LReadInfo: BY_HANDLE_FILE_INFORMATION;
  LIndex: Integer;
  LDigest: String;
begin
  { Metadata-only identity retention permits a constructor's ordinary exclusive
    writer to change a derivative. A separate read handle freezes capture; the
    later compare still refuses retirement after any such change. }
  LHandle := OpenFile(FDirectory + AName, 0,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE);

  if LHandle = INVALID_HANDLE_VALUE then
  begin
    Defer('Compiler derivative could not be captured');
    Exit;
  end;
  try
    LFrozen := OpenFile(FDirectory + AName, GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_DELETE);

    if LFrozen = INVALID_HANDLE_VALUE then
    begin
      Defer('Compiler derivative is locked during capture');
      Exit;
    end;
    try

      if not OrdinaryFile(LHandle, LInfo) or not OrdinaryFile(LFrozen, LReadInfo) or
        not SameFile(LInfo, LReadInfo) or (FileBytes(LInfo) > CMaximumFileBytes) or
        (FHashedBytes + FileBytes(LInfo) > CMaximumHashBytes) then
      begin
        Defer('Compiler derivative identity or hash budget refused');
        Exit;
      end;
      LDigest := Digest(LFrozen);
    finally
      CloseHandle(LFrozen);
    end;
    LIndex := Length(FEntries);
    SetLength(FEntries, LIndex + 1);
    FEntries[LIndex].Handle := INVALID_HANDLE_VALUE;
    FEntries[LIndex].Name := AName;
    FEntries[LIndex].Information := LInfo;
    FEntries[LIndex].Digest := LDigest;
    FEntries[LIndex].Handle := LHandle;
    LHandle := INVALID_HANDLE_VALUE;
    FHashedBytes := FHashedBytes + FileBytes(LInfo);
    Inc(FReport.CapturedFiles);
  finally

    if LHandle <> INVALID_HANDLE_VALUE then
    begin
      CloseHandle(LHandle);
    end;
  end;
end;

procedure TNyxOwnedProjectionStorage.CaptureDirectory(const ARelative: TNyxText;
  AFinal: Boolean);
var
  LSearch: TWin32FindDataW;
  LFind: THandle;
  LWide: UnicodeString;
  LName: TNyxText;
  LExtension: TNyxText;
  LCount: Integer;
  LDerivative: Boolean;
  LIndex: Integer;
  LFound: Boolean;
begin
  LWide := UTF8Decode(FDirectory + ARelative + '*');
  LFind := FindFirstFileW(PWideChar(LWide), LSearch);

  if LFind = INVALID_HANDLE_VALUE then
  begin

    if GetLastError <> ERROR_FILE_NOT_FOUND then
    begin
      Defer('Compiler derivative enumeration failed');
    end;
    Exit;
  end;
  try
    LCount := 0;
    repeat
      LName := UTF8Encode(UnicodeString(PWideChar(@LSearch.cFileName[0])));

      if (LName <> '.') and (LName <> '..') then
      begin
        Inc(LCount);

        if LCount > CMaximumFiles then
        begin
          Defer('Compiler derivative enumeration exceeded its item budget');
          Break;
        end;

        if not ((ARelative = '') and ((LName = 'units') or EvidenceName(LName))) then
        begin
          LExtension := ExtractFileExt(LName);
          LDerivative := ((ARelative = '') and ((LName = 'nyx_projection.exe') or
            (LName = 'nyx_projection') or (LName = 'link.res') or
            (LName = 'ppas.bat') or (LName = 'ppas.sh'))) or
            ((ARelative <> '') and ((LExtension = '.o') or (LExtension = '.ppu') or
              (LExtension = '.or') or (LExtension = '.rst') or (LExtension = '.rsj')));

          if not LDerivative or
            ((LSearch.dwFileAttributes and (FILE_ATTRIBUTE_DIRECTORY or
              FILE_ATTRIBUTE_REPARSE_POINT)) <> 0) then
          begin
            Defer('Unexpected or linked compiler entry remains preserved');
          end
          else
          begin

            if AFinal then
            begin
              LFound := False;
              for LIndex := 0 to High(FEntries) do
              begin
                LFound := LFound or (FEntries[LIndex].Name = ARelative + LName);
              end;

              if not LFound then
              begin
                Defer('Uncaptured compiler derivative remains preserved');
              end;
            end
            else
            begin
              CaptureFile(ARelative + LName);
            end;
          end;
        end;
      end;
    until not FindNextFileW(LFind, LSearch);

    if (LCount <= CMaximumFiles) and (GetLastError <> ERROR_NO_MORE_FILES) then
    begin
      Defer('Compiler derivative enumeration did not finish');
    end;
  finally
    Windows.FindClose(LFind);
  end;
end;
{$endif}

procedure TNyxOwnedProjectionStorage.CaptureCompilerDerivatives;
begin

  if FCaptured or FFinished then
  begin
    raise ENyxModel.Create('Compiler derivatives can be captured only once after join');
  end;
  FCaptured := True;

  if (FTarget = btBrowser) or (FPolicy.Retention = prAllFiles) then
  begin
    Exit;
  end;
  {$ifdef MSWINDOWS}
  try
    CaptureDirectory('', False);
    CaptureDirectory('units' + PathDelim, False);
  except
    on LException: Exception do
    begin
      Defer('Compiler derivative capture could not finish');
    end;
  end;
  {$endif}
end;

procedure TNyxOwnedProjectionStorage.FinishAfterJoin;
{$ifdef MSWINDOWS}
var
  LIndex: Integer;
  LHandle: THandle;
  LInfo: BY_HANDLE_FILE_INFORMATION;
  LDelete: Byte;
{$endif}
begin

  if FFinished then
  begin
    Exit;
  end;
  FFinished := True;

  if (FTarget = btBrowser) or (FPolicy.Retention = prAllFiles) then
  begin
    Exit;
  end;

  if not FCaptured then
  begin
    Defer('Compiler join capture was not completed; derivatives remain preserved');
    Exit;
  end;
  {$ifdef MSWINDOWS}
  try
    CaptureDirectory('', True);
    CaptureDirectory('units' + PathDelim, True);
  except
    on LException: Exception do
    begin
      Defer('Final compiler derivative inventory could not finish');
    end;
  end;
  for LIndex := 0 to High(FEntries) do
  begin
    { No write/delete sharing on this verification handle: once it opens,
      another caller cannot change or move this file during compare/retirement.
      The captured handle prevents file-ID recycling after replacement. }
    LHandle := OpenFile(FDirectory + FEntries[LIndex].Name,
      GENERIC_READ or CDeleteAccess, FILE_SHARE_READ);

    if LHandle = INVALID_HANDLE_VALUE then
    begin
      Defer('Compiler derivative is missing or locked; retirement deferred');
      Continue;
    end;
    try
      try

        if not OrdinaryFile(LHandle, LInfo) or
          not SameFile(FEntries[LIndex].Information, LInfo) or
          (Digest(LHandle) <> FEntries[LIndex].Digest) then
        begin
          Defer('Compiler derivative changed identity or contents; retirement deferred');
          Continue;
        end;
        LDelete := 1;

        if not SetFileInformationByHandle(LHandle, CFileDisposition, @LDelete,
          SizeOf(LDelete)) then
        begin
          Defer('Compiler derivative retirement was refused by its host');
          Continue;
        end;
        CloseHandle(FEntries[LIndex].Handle);
        FEntries[LIndex].Handle := INVALID_HANDLE_VALUE;
        Inc(FReport.RetiredFiles);
        FReport.RetiredBytes := FReport.RetiredBytes + FileBytes(LInfo);
      except
        on LException: Exception do
        begin
          Defer('Compiler derivative retirement could not finish');
        end;
      end;
    finally
      CloseHandle(LHandle);
    end;
  end;

  if FReport.DeferredItems = 0 then
  begin
    FReport.State := psRetired;
  end;
  {$endif}
end;

function TNyxOwnedProjectionStorage.Snapshot: TNyxProjectionStorageReport;
begin
  Result := FReport;
end;

constructor TNyxProjectionStorageBuild.Create(const ABuild: INyxSourceProjectionBuild;
  const AReport: TNyxProjectionStorageReport);
begin
  inherited Create;

  if ABuild = nil then
  begin
    raise ENyxModel.Create('Storage observation requires a detached build');
  end;
  FBuild := ABuild;
  FReport := AReport;
end;

function TNyxProjectionStorageBuild.GetReference: TNyxSourceProjectionRef;
begin
  Result := FBuild.Reference;
end;

function TNyxProjectionStorageBuild.GetProjection: INyxSourceProjection;
begin
  Result := FBuild.Projection;
end;

function TNyxProjectionStorageBuild.GetArtifact: TNyxText;
begin
  Result := FBuild.Artifact;
end;

function TNyxProjectionStorageBuild.GetRuntimeLog: TNyxText;
begin
  Result := FBuild.RuntimeLog;
end;

function TNyxProjectionStorageBuild.GetStorage: TNyxProjectionStorageReport;
begin
  Result := FReport;
end;

function WithNyxProjectionStorage(const ABuild: INyxSourceProjectionBuild;
  const AReport: TNyxProjectionStorageReport): INyxSourceProjectionBuild;
begin
  Result := TNyxProjectionStorageBuild.Create(ABuild, AReport);
end;

end.
