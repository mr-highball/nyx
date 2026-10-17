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
unit nyx.studio.compilerinputs;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, nyx.text;

type
  { Local input admission or observation failed. No successful compiler result
    may cross this boundary; the owner still joins its process before release. }
  ENyxCompilerInputs = class(Exception);

  { Synchronous borrowed stack callback for cancellation/deadline checks during
    bounded acquisition. The lease must be freed before the enclosing stack
    frame returns. No scheduler, worker or platform callback invokes this value. }
  TNyxCompilerInputCheck = procedure is nested;

  { A worker-owned read lease over this host's ordinary src/studio trees. Win32
    pins ancestors and every file against replacement/writes, and watches both
    trees before inventory to reject new/transient inputs and watch failure.
    Multiple compiler readers may coexist. Bounds are 4096 total entries, 256
    directories, 64 MiB per file and 256 MiB total file bytes. No files are copied,
    deleted or persisted. Unavailable host/filesystem support refuses acquisition.
    This covers owned library inputs, not external compiler/RTL/LCL/configuration
    or arbitrary handwritten includes. It supplies no artifact-cache authority. }
  TNyxCompilerInputs = class
  private
    FHandles: array of THandle;
    FDirectories: TNyxStrings;
    FWatches: TList;
    FCheck: TNyxCompilerInputCheck;
    FEntryCount: Integer;
    FFileCount: Integer;
    FByteCount: Int64;
    procedure PinDirectory(const APath: TNyxText);
    procedure CaptureTree(const APath: TNyxText);
    procedure CaptureFile(const APath: TNyxText);
  public
    { SourceRoot is trusted host configuration, not a wire/editor input. Both
      source trees must already exist. Failure releases all acquired ownership. }
    constructor Create(const ASourceRoot: TNyxText;
      ACheck: TNyxCompilerInputCheck = nil);
    { The compiler family must be joined first. Pending directory I/O is
      cancelled and actually completed before its buffer/event are released.
      An OS which cannot complete cancellation keeps the owning worker here. }
    destructor Destroy; override;
    { Raises after any observed tree change, overflow, watch failure or caller
      cancellation. Call during compilation and after the complete family joins,
      before admitting output. Last-access changes alone do not invalidate reads. }
    procedure Check;
    property FileCount: Integer read FFileCount;
    property ByteCount: Int64 read FByteCount;
  end;

implementation

{$ifdef MSWINDOWS}
uses
  Windows;

const
  CMaximumEntries = 4096;
  CMaximumDirectories = 256;
  CMaximumFileBytes = 64 * Int64(1024) * 1024;
  CMaximumTotalBytes = 256 * Int64(1024) * 1024;

type
  { One non-inherited Win32 ReadDirectoryChangesW request. OVERLAPPED and its
    DWORD-aligned buffer live at stable object addresses until actual I/O join.
    Any completion, including overflow, revokes this read lease permanently;
    events are never rearmed or interpreted as an authoritative new inventory. }
  TCompilerDirectoryWatch = class
  private
    FHandle: THandle;
    FOperation: TOverlapped;
    FBuffer: array[0..1023] of Cardinal;
    FPending: Boolean;
    FChanged: Boolean;
  public
    constructor Create(const APath: TNyxText);
    destructor Destroy; override;
    procedure Check;
  end;

function OpenInput(const APath: TNyxText; AOverlapped: Boolean): THandle;
var
  LPath: UnicodeString;
  LFlags: Cardinal;
begin
  LPath := UTF8Decode(ExcludeTrailingPathDelimiter(APath));
  { Preserve a drive root's trailing separator: E: is a drive-relative path. }

  if (Length(LPath) = 2) and (LPath[2] = ':') then
  begin
    LPath := LPath + '\';
  end;
  LFlags := FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT;

  if AOverlapped then
  begin
    LFlags := LFlags or FILE_FLAG_OVERLAPPED;
  end;
  Result := CreateFileW(PWideChar(LPath), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, LFlags, 0);
end;

constructor TCompilerDirectoryWatch.Create(const APath: TNyxText);
begin
  inherited Create;
  FHandle := INVALID_HANDLE_VALUE;
  FOperation.hEvent := CreateEvent(nil, True, False, nil);

  if FOperation.hEvent = 0 then
  begin
    raise ENyxCompilerInputs.Create('Cannot allocate a compiler input observation');
  end;
  FHandle := OpenInput(APath, True);

  if FHandle = INVALID_HANDLE_VALUE then
  begin
    raise ENyxCompilerInputs.Create('Cannot observe the compiler input tree');
  end;

  { File data is already protected by deny-write handles. Observe membership
    only: directory last-write notifications may be flushed by a later read
    after an earlier writer closed, and do not establish a new compiler input. }
  if not ReadDirectoryChangesW(FHandle, @FBuffer[0], SizeOf(FBuffer), True,
    FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME,
    nil, @FOperation, nil) then
  begin
    raise ENyxCompilerInputs.Create('This filesystem cannot observe compiler inputs');
  end;
  FPending := True;
end;

destructor TCompilerDirectoryWatch.Destroy;
var
  LBytes: Cardinal;
begin

  if FPending then
  begin
    { Cancellation is a request, not I/O completion. ERROR_NOT_FOUND can mean
      completion won the race; either way the same operation is joined below. }
    CancelIoEx(FHandle, @FOperation);
    GetOverlappedResult(FHandle, FOperation, LBytes, True);
    FPending := False;
  end;

  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(FHandle);
  end;

  if FOperation.hEvent <> 0 then
  begin
    CloseHandle(FOperation.hEvent);
  end;
  inherited Destroy;
end;

procedure TCompilerDirectoryWatch.Check;
var
  LBytes: Cardinal;
begin

  if not FChanged and FPending then
  begin

    if GetOverlappedResult(FHandle, FOperation, LBytes, False) then
    begin
      FPending := False;
      FChanged := True;
    end
    else
    begin

      if GetLastError <> ERROR_IO_INCOMPLETE then
      begin
        { A failed/overflowed observation cannot certify an unchanged input set. }
        FPending := False;
        FChanged := True;
      end;
    end;
  end;

  if FChanged then
  begin
    raise ENyxCompilerInputs.Create(
      'Nyx library inputs changed during compilation; build again from stable sources');
  end;
end;
{$endif}

constructor TNyxCompilerInputs.Create(const ASourceRoot: TNyxText;
  ACheck: TNyxCompilerInputCheck);
var
  LRoot: TNyxText;
begin
  inherited Create;
  FCheck := ACheck;
  FDirectories := TNyxStrings.Create;
  FWatches := TList.Create;
  {$ifdef MSWINDOWS}
  LRoot := IncludeTrailingPathDelimiter(ExpandFileName(ASourceRoot));
  PinDirectory(LRoot + 'src');
  PinDirectory(LRoot + 'studio');
  FWatches.Add(TCompilerDirectoryWatch.Create(LRoot + 'src'));
  FWatches.Add(TCompilerDirectoryWatch.Create(LRoot + 'studio'));
  CaptureTree(LRoot + 'src');
  CaptureTree(LRoot + 'studio');
  Check;
  {$else}
  LRoot := ASourceRoot;
  raise ENyxCompilerInputs.Create('Compiler input leases are unavailable on this host');
  {$endif}
end;

destructor TNyxCompilerInputs.Destroy;
var
  LIndex: Integer;
begin

  if FWatches <> nil then
  begin
    for LIndex := FWatches.Count - 1 downto 0 do
    begin
      TObject(FWatches[LIndex]).Free;
    end;
    FWatches.Free;
  end;
  {$ifdef MSWINDOWS}
  for LIndex := High(FHandles) downto 0 do
  begin
    CloseHandle(FHandles[LIndex]);
  end;
  {$endif}
  FDirectories.Free;
  inherited Destroy;
end;

procedure TNyxCompilerInputs.Check;
var
  LIndex: Integer;
begin

  if Assigned(FCheck) then
  begin
    FCheck;
  end;
  {$ifdef MSWINDOWS}
  for LIndex := 0 to FWatches.Count - 1 do
  begin
    TCompilerDirectoryWatch(FWatches[LIndex]).Check;
  end;
  {$endif}
end;

procedure TNyxCompilerInputs.PinDirectory(const APath: TNyxText);
{$ifdef MSWINDOWS}
var
  LPath: TNyxText;
  LParent: TNyxText;
  LHandle: THandle;
  LInfo: BY_HANDLE_FILE_INFORMATION;
  LIndex: Integer;
{$endif}
begin
  {$ifdef MSWINDOWS}
  LPath := ExcludeTrailingPathDelimiter(APath);

  if FDirectories.IndexOf(LPath) >= 0 then
  begin
    Exit;
  end;
  Check;

  if FDirectories.Count >= CMaximumDirectories then
  begin
    raise ENyxCompilerInputs.Create('Compiler input directory budget exceeded');
  end;
  LParent := ExtractFileDir(LPath);

  if (LParent <> '') and (LParent <> LPath) then
  begin
    PinDirectory(LParent);
  end;

  if FDirectories.Count >= CMaximumDirectories then
  begin
    raise ENyxCompilerInputs.Create('Compiler input directory budget exceeded');
  end;
  LHandle := OpenInput(LPath, False);

  if LHandle = INVALID_HANDLE_VALUE then
  begin
    raise ENyxCompilerInputs.Create('Cannot pin ordinary compiler input directories');
  end;
  try

    if not GetFileInformationByHandle(LHandle, @LInfo) or
      ((LInfo.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0) or
      ((LInfo.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0) then
    begin
      raise ENyxCompilerInputs.Create('Compiler input directories must be ordinary paths');
    end;
    LIndex := Length(FHandles);
    SetLength(FHandles, LIndex + 1);
    FHandles[LIndex] := LHandle;
  except
    CloseHandle(LHandle);
    raise;
  end;
  FDirectories.Add(LPath);
  {$endif}
end;

procedure TNyxCompilerInputs.CaptureFile(const APath: TNyxText);
{$ifdef MSWINDOWS}
var
  LHandle: THandle;
  LInfo: BY_HANDLE_FILE_INFORMATION;
  LBytes: Int64;
  LIndex: Integer;
{$endif}
begin
  {$ifdef MSWINDOWS}
  LHandle := OpenInput(APath, False);

  if LHandle = INVALID_HANDLE_VALUE then
  begin
    raise ENyxCompilerInputs.Create('Compiler inputs are being written or are unavailable');
  end;
  try

    if not GetFileInformationByHandle(LHandle, @LInfo) or
      ((LInfo.dwFileAttributes and (FILE_ATTRIBUTE_REPARSE_POINT or
        FILE_ATTRIBUTE_DIRECTORY)) <> 0) or (LInfo.nNumberOfLinks <> 1) then
    begin
      raise ENyxCompilerInputs.Create('Compiler inputs must be ordinary single-link files');
    end;
    LBytes := Int64(LInfo.nFileSizeHigh) * Int64($100000000) + LInfo.nFileSizeLow;

    if (LBytes > CMaximumFileBytes) or
      (LBytes > CMaximumTotalBytes - FByteCount) then
    begin
      raise ENyxCompilerInputs.Create('Compiler input byte budget exceeded');
    end;
    LIndex := Length(FHandles);
    SetLength(FHandles, LIndex + 1);
    FHandles[LIndex] := LHandle;
  except
    CloseHandle(LHandle);
    raise;
  end;
  Inc(FFileCount);
  Inc(FByteCount, LBytes);
  {$endif}
end;

procedure TNyxCompilerInputs.CaptureTree(const APath: TNyxText);
{$ifdef MSWINDOWS}
var
  LSearch: THandle;
  LEntry: TWin32FindDataW;
  LPattern: UnicodeString;
  LName: TNyxText;
  LPath: TNyxText;
  LError: Cardinal;
{$endif}
begin
  {$ifdef MSWINDOWS}
  PinDirectory(APath);
  LPattern := UTF8Decode(IncludeTrailingPathDelimiter(APath) + '*');
  LSearch := FindFirstFileW(PWideChar(LPattern), LEntry);

  if LSearch = INVALID_HANDLE_VALUE then
  begin
    raise ENyxCompilerInputs.Create('Cannot inventory compiler inputs');
  end;
  try
    repeat
      Check;
      LName := UTF8Encode(UnicodeString(LEntry.cFileName));

      if (LName <> '.') and (LName <> '..') then
      begin
        Inc(FEntryCount);

        if FEntryCount > CMaximumEntries then
        begin
          raise ENyxCompilerInputs.Create('Compiler input entry budget exceeded');
        end;
        LPath := IncludeTrailingPathDelimiter(APath) + LName;

        if (LEntry.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
        begin
          CaptureTree(LPath);
        end
        else
        begin
          CaptureFile(LPath);
        end;
      end;
    until not FindNextFileW(LSearch, LEntry);
    LError := GetLastError;

    if LError <> ERROR_NO_MORE_FILES then
    begin
      raise ENyxCompilerInputs.Create('Compiler input inventory did not complete');
    end;
  finally
    Windows.FindClose(LSearch);
  end;
  {$endif}
end;

end.
