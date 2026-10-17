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

unit nyx.studio.compilerprocess;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Process;

type
  { Native compiler-host adapter. One instance owns one invocation, its pipes
    and process handles. On Windows it additionally owns a non-inherited,
    unnamed job containing the compiler and its descendants. No PID scans,
    editor references or process-global jobs are involved.

    Other native hosts retain direct-process retirement only. They require a
    separately qualified family adapter before claiming compiler-host parity. }
  TNyxCompilerProcess = class(TProcess)
  private
    FRetired: Boolean;
    FInvoked: Boolean;
    {$ifdef MSWINDOWS}
    FFamily: THandle;
    FFamilyPort: THandle;
    FFamilyEmpty: Boolean;
    FAssigned: Boolean;
    procedure CreateFamily;
    function QueryFamily(out AActive: Cardinal): Boolean;
    function ObserveFamilyEmpty: Boolean;
    {$endif}
  protected
    {$if defined(MSWINDOWS) and (FPC_FULLVERSION >= 30301)}
    { Use FCL's virtual startup seam and its owned descriptors/handle fields;
      no dependency source patch or process-global API interception is needed. }
    procedure SysExecute; override;
    {$endif}
  public
    { Single use. Windows 10+ creates the suspended compiler inside its private
      kill-on-close job with STARTUPINFOEX/JOB_LIST, before any user instruction.
      Host death during admission therefore closes the job without an unowned
      suspended child. Unsupported OS/FCL startup capability refuses before
      creation, without reverting to create-then-assign. The backend itself needs
      FPC 3.3.1+ for this startup seam; selected application compilers are separate.
      The executor configures structured Executable/Parameters and pipes. Legacy
      free-form command lines are outside this compiler adapter's contract. }
    procedure Execute; override;
    { Includes Windows descendants even after the compiler exits. Query failure
      raises before result publication; retirement still retains ownership. }
    function FamilyRunning: Boolean;
    { Idempotent. Ends every owned family member and joins the exact compiler.
      Returns only after Windows job accounting confirms zero active members.
      An OS failure to confirm retirement keeps this worker/slot active; no
      timeout may release ownership and claim a terminal result. }
    procedure RetireAndJoin;
    { Retires before freeing pipes, process handles or the job. Kill-on-close
      is additional protection, not a substitute for explicit retirement. }
    destructor Destroy; override;
  end;

implementation

{$ifdef MSWINDOWS}
uses
  Windows;

const
  CJobBasicAccounting = 1;
  CJobExtendedLimit = 9;
  CJobKillOnClose = $00002000;
  CJobCompletionPort = 7;
  CJobActiveZero = 4;

type
  { Exact Win32/Win64 ABI for the small kernel32 surface missing in FPC 3.2's
    Windows declarations. Explicit padding avoids relying on Pascal's default
    Int64 alignment on i386. SIZE_T/ULONG_PTR use native pointer width.
    Layouts: basic 48/64 bytes, extended 112/144 bytes, accounting 48 bytes.
    Sources: Microsoft JOBOBJECT_BASIC_LIMIT_INFORMATION,
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION and BASIC_ACCOUNTING_INFORMATION. }
  TJobBasicLimits = packed record
    ProcessTime: Int64;
    JobTime: Int64;
    Flags: Cardinal;
    {$ifdef CPU64}
    WorkingSetPadding: Cardinal;
    {$endif}
    MinimumWorkingSet: PtrUInt;
    MaximumWorkingSet: PtrUInt;
    ActiveProcessLimit: Cardinal;
    {$ifdef CPU64}
    AffinityPadding: Cardinal;
    {$endif}
    Affinity: PtrUInt;
    PriorityClass: Cardinal;
    SchedulingClass: Cardinal;
    {$ifndef CPU64}
    TailPadding: Cardinal;
    {$endif}
  end;

  { Native JOBOBJECT_EXTENDED_LIMIT_INFORMATION owns no handles; its basic limits
    request kill-on-close for this adapter's independently owned unnamed job.
    Pointer-width fields preserve the documented i386/x64 kernel ABI. }
  TJobExtendedLimits = packed record
    Basic: TJobBasicLimits;
    IOReadOperations: QWord;
    IOWriteOperations: QWord;
    IOOtherOperations: QWord;
    IOReadBytes: QWord;
    IOWriteBytes: QWord;
    IOOtherBytes: QWord;
    ProcessMemory: PtrUInt;
    JobMemory: PtrUInt;
    PeakProcessMemory: PtrUInt;
    PeakJobMemory: PtrUInt;
  end;

  { Copied JOBOBJECT_BASIC_ACCOUNTING_INFORMATION from the borrowed job handle.
    ActiveProcesses is the retirement authority; compiler exit alone is weaker. }
  TJobAccounting = packed record
    UserTime: Int64;
    KernelTime: Int64;
    PeriodUserTime: Int64;
    PeriodKernelTime: Int64;
    PageFaults: Cardinal;
    TotalProcesses: Cardinal;
    ActiveProcesses: Cardinal;
    TerminatedProcesses: Cardinal;
  end;

type
  { JOBOBJECT_ASSOCIATE_COMPLETION_PORT borrows this invocation's native port.
    Key is an opaque tag, never a Pascal object pointer. Association precedes
    the first child so an early exit cannot precede monitor attachment. }
  TJobCompletionLink = packed record
    Key: Pointer;
    Port: THandle;
  end;

function CreateJobObjectW(AAttributes: Pointer; AName: PWideChar): THandle;
  stdcall; external 'kernel32' name 'CreateJobObjectW';
function SetInformationJobObject(AJob: THandle; AClass: Integer;
  AInformation: Pointer; ALength: Cardinal): BOOL;
  stdcall; external 'kernel32' name 'SetInformationJobObject';
function QueryInformationJobObject(AJob: THandle; AClass: Integer;
  AInformation: Pointer; ALength: Cardinal; AReturned: Pointer): BOOL;
  stdcall; external 'kernel32' name 'QueryInformationJobObject';
function TerminateJobObject(AJob: THandle; AExitCode: Cardinal): BOOL;
  stdcall; external 'kernel32' name 'TerminateJobObject';
function ReadCompilerCompletion(APort: THandle; out ACode: Cardinal;
  out AKey: PtrUInt; out AMessage: Pointer; ATimeout: Cardinal): BOOL;
  stdcall; external 'kernel32' name 'GetQueuedCompletionStatus';

procedure TNyxCompilerProcess.CreateFamily;
var
  LLimits: TJobExtendedLimits;
  LActive: Cardinal;
  LCompletion: TJobCompletionLink;
begin
  { nil security attributes produce a non-inherited handle. Do not set either
    breakaway limit: CreateProcess descendants stay in this invocation's job,
    including descendants assigned to nested jobs on current Windows hosts. }
  FFamily := CreateJobObjectW(nil, nil);

  if FFamily = 0 then
  begin
    RaiseLastOSError;
  end;
  FillChar(LLimits, SizeOf(LLimits), 0);
  LLimits.Basic.Flags := CJobKillOnClose;

  if not SetInformationJobObject(FFamily, CJobExtendedLimit, @LLimits,
    SizeOf(LLimits)) then
  begin
    RaiseLastOSError;
  end;

  FFamilyPort := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 1);

  if FFamilyPort = 0 then
  begin
    RaiseLastOSError;
  end;
  LCompletion.Key := Pointer(1);
  LCompletion.Port := FFamilyPort;

  if not SetInformationJobObject(FFamily, CJobCompletionPort, @LCompletion,
    SizeOf(LCompletion)) then
  begin
    RaiseLastOSError;
  end;

  if not QueryFamily(LActive) then
  begin
    RaiseLastOSError;
  end;
end;

function TNyxCompilerProcess.QueryFamily(out AActive: Cardinal): Boolean;
var
  LAccounting: TJobAccounting;
begin
  FillChar(LAccounting, SizeOf(LAccounting), 0);
  Result := QueryInformationJobObject(FFamily, CJobBasicAccounting,
    @LAccounting, SizeOf(LAccounting), nil);
  AActive := LAccounting.ActiveProcesses;
end;

function TNyxCompilerProcess.ObserveFamilyEmpty: Boolean;
var
  LCode: Cardinal;
  LKey: PtrUInt;
  LMessage: Pointer;
begin
  { Accounting is a snapshot. Require the job's own all-process-exit notification
    as well before publishing retirement. No message payload is dereferenced and
    no recycled process ID is opened. Missing OS notification retains ownership;
    a timeout is never converted into successful retirement or slot reuse. }
  while ReadCompilerCompletion(FFamilyPort, LCode, LKey, LMessage, 0) do
  begin

    if (LKey = 1) and (LCode = CJobActiveZero) then
    begin
      FFamilyEmpty := True;
    end;
  end;
  Result := FFamilyEmpty;
end;
{$endif}

{$if defined(MSWINDOWS) and (FPC_FULLVERSION >= 30301)}
{$I nyx.studio.compilerprocess.win.inc}
{$endif}

procedure TNyxCompilerProcess.Execute;
begin

  if FInvoked or FRetired then
  begin
    raise EProcess.Create('A compiler invocation cannot be reused');
  end;
  FInvoked := True;
  {$ifdef MSWINDOWS}
  {$if FPC_FULLVERSION < 30301}
  raise EProcess.Create('Windows compiler ownership requires a backend built with FPC 3.3.1 or newer');
  {$else}
  CreateFamily;
  Options := Options + [poRunSuspended];
  inherited Execute;

  if Resume = -1 then
  begin
    RaiseLastOSError;
  end;
  {$endif}
  {$else}
  inherited Execute;
  {$endif}
end;

function TNyxCompilerProcess.FamilyRunning: Boolean;
{$ifdef MSWINDOWS}
var
  LActive: Cardinal;
{$endif}
begin
  {$ifdef MSWINDOWS}

  if not QueryFamily(LActive) then
  begin
    RaiseLastOSError;
  end;
  Result := (LActive > 0) or Running or not ObserveFamilyEmpty;
  {$else}
  Result := Running;
  {$endif}
end;

procedure TNyxCompilerProcess.RetireAndJoin;
var
  LJoined: Boolean;
  LFamilyGone: Boolean;
  {$ifdef MSWINDOWS}
  LActive: Cardinal;
  {$endif}
begin

  if FRetired then
  begin
    Exit;
  end;

  if ProcessHandle <> 0 then
  begin
    repeat
      {$ifdef MSWINDOWS}
      LFamilyGone := not FAssigned;

      if FAssigned then
      begin
        { Termination is asynchronous. Keep the job open until accounting
          confirms actual retirement, independently of the compiler handle. }
        TerminateJobObject(FFamily, 1);
        LFamilyGone := QueryFamily(LActive) and (LActive = 0) and ObserveFamilyEmpty;
      end;
      {$else}
      LFamilyGone := True;
      {$endif}

      if Running then
      begin
        Terminate(1);
      end;
      LJoined := WaitOnExit(25);

      if LJoined and LFamilyGone then
      begin
        Break;
      end;
      Sleep(10);
    until False;
  end;
  FRetired := True;
end;

destructor TNyxCompilerProcess.Destroy;
begin
  RetireAndJoin;
  {$ifdef MSWINDOWS}

  if FFamily <> 0 then
  begin
    CloseHandle(FFamily);
    FFamily := 0;
  end;

  if FFamilyPort <> 0 then
  begin
    CloseHandle(FFamilyPort);
    FFamilyPort := 0;
  end;
  {$endif}
  inherited Destroy;
end;

end.
