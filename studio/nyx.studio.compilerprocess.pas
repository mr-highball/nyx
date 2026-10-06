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
    FAssigned: Boolean;
    procedure CreateFamily;
    function QueryFamily(out AActive: Cardinal): Boolean;
    {$endif}
  public
    { Single use. Windows creates suspended, assigns before Resume and refuses
      an incompatible outer job instead of running an uncontained compiler.
      TProcess pipes/options are configured by the executor before this call.
      This two-call admission is not crash-atomic: abrupt host death between
      creation and assignment can leave a suspended, unassigned compiler.
      Kill-on-close protects assigned members; atomic job-list creation remains
      separate compiler-host hardening. Normal failures retire before raising. }
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

function CreateJobObjectW(AAttributes: Pointer; AName: PWideChar): THandle;
  stdcall; external 'kernel32' name 'CreateJobObjectW';
function SetInformationJobObject(AJob: THandle; AClass: Integer;
  AInformation: Pointer; ALength: Cardinal): BOOL;
  stdcall; external 'kernel32' name 'SetInformationJobObject';
function QueryInformationJobObject(AJob: THandle; AClass: Integer;
  AInformation: Pointer; ALength: Cardinal; AReturned: Pointer): BOOL;
  stdcall; external 'kernel32' name 'QueryInformationJobObject';
function AssignProcessToJobObject(AJob, AProcess: THandle): BOOL;
  stdcall; external 'kernel32' name 'AssignProcessToJobObject';
function TerminateJobObject(AJob: THandle; AExitCode: Cardinal): BOOL;
  stdcall; external 'kernel32' name 'TerminateJobObject';

procedure TNyxCompilerProcess.CreateFamily;
var
  LLimits: TJobExtendedLimits;
  LActive: Cardinal;
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
{$endif}

procedure TNyxCompilerProcess.Execute;
begin

  if FInvoked or FRetired then
  begin
    raise EProcess.Create('A compiler invocation cannot be reused');
  end;
  FInvoked := True;
  {$ifdef MSWINDOWS}
  CreateFamily;
  Options := Options + [poRunSuspended];
  inherited Execute;

  if not AssignProcessToJobObject(FFamily, ProcessHandle) then
  begin
    { Never resume or fall back. The executor's finally/destructor retires this
      exact suspended process even if assignment or pipe setup raises. }
    RaiseLastOSError;
  end;
  FAssigned := True;

  if Resume = -1 then
  begin
    RaiseLastOSError;
  end;
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
  Result := (LActive > 0) or Running;
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
        LFamilyGone := QueryFamily(LActive) and (LActive = 0);
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
  {$endif}
  inherited Destroy;
end;

end.
