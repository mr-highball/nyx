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
{ Windows-only FPC 3.3 startup adapter. FCL retains descriptor/stream ownership
  and base process handles; only the kernel creation call needs an extended
  startup structure to make job membership part of process creation. }

const
  CStartupExtended = $00080000;
  CAttributeJobs = $0002000D;
  CAttributeHandles = $00020002;

type
  { Exact STARTUPINFOEXW layout: native STARTUPINFOW followed by an opaque
    attribute-list pointer. The invocation owns that allocation; kernel32 borrows
    the list and its referenced handle arrays only until CreateProcessW returns. }
  TCompilerStartup = packed record
    Info: STARTUPINFOW;
    Attributes: Pointer;
  end;

  { Protected FCL descriptor access, without adding fields or changing ownership.
    All instances remain borrowed from TProcess; virtual methods preserve custom
    descriptors' pipe/file/handle preparation and per-invocation retirement. }
  TCompilerDescriptor = class(TIODescriptor);

function InitializeCompilerAttributes(AList: Pointer; ACount, AFlags: Cardinal;
  var ASize: PtrUInt): BOOL; stdcall;
  external 'kernel32' name 'InitializeProcThreadAttributeList';
function UpdateCompilerAttribute(AList: Pointer; AFlags: Cardinal; AKey: PtrUInt;
  AValue: Pointer; ASize: PtrUInt; APrevious, AReturned: Pointer): BOOL; stdcall;
  external 'kernel32' name 'UpdateProcThreadAttribute';
procedure DeleteCompilerAttributes(AList: Pointer); stdcall;
  external 'kernel32' name 'DeleteProcThreadAttributeList';

{ Preserve FCL's compiler argument spelling, rather than substituting C-runtime
  quoting for FPC's own parser. Caller-authored quotes remain explicit; automatic
  quoting surrounds a space-bearing value only when it has no existing quote.
  No shell is involved, and the actual executable is a separate kernel argument. }
function CompilerArgument(const AValue: UnicodeString): UnicodeString;
begin

  if Pos(#0, AValue) > 0 then
  begin
    raise EProcess.Create('Compiler arguments cannot contain NUL');
  end;
  Result := AValue;

  if (Pos(' ', AValue) > 0) and (Pos('"', AValue) = 0) then
  begin
    Result := '"' + AValue + '"';
  end;
end;

procedure TNyxCompilerProcess.SysExecute;
const
  CWindow: array[TShowWindowOptions] of Word = (0, SW_HIDE, SW_MAXIMIZE,
    SW_MINIMIZE, SW_RESTORE, SW_SHOW, SW_SHOWDEFAULT, SW_SHOWMAXIMIZED,
    SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE, SW_SHOWNA, SW_SHOWNOACTIVATE, SW_SHOWNORMAL);
  CPriority: array[TProcessPriority] of Cardinal = (HIGH_PRIORITY_CLASS,
    IDLE_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS,
    BELOW_NORMAL_PRIORITY_CLASS, ABOVE_NORMAL_PRIORITY_CLASS);
var
  LStartup: TCompilerStartup;
  LInformation: TProcessInformation;
  LAttributeSize: PtrUInt;
  LAttributesReady: Boolean;
  LInput: TCompilerDescriptor;
  LOutput: TCompilerDescriptor;
  LError: TCompilerDescriptor;
  LDefaults: Boolean;
  LHandles: array[0..2] of THandle;
  LHandleCount: Integer;
  LName: UnicodeString;
  LCommand: UnicodeString;
  LDirectory: UnicodeString;
  LEnvironment: UnicodeString;
  LDesktop: UnicodeString;
  LTitle: UnicodeString;
  LEnvironmentPointer: Pointer;
  LDirectoryPointer: PWideChar;
  LFlags: Cardinal;
  LIndex: Integer;

  procedure IncludeHandle(AHandle: THandle);
  var
    LPosition: Integer;
  begin

    if (AHandle = 0) or (AHandle = INVALID_HANDLE_VALUE) then
    begin
      Exit;
    end;
    for LPosition := 0 to LHandleCount - 1 do
    begin

      if LHandles[LPosition] = AHandle then
      begin
        Exit;
      end;
    end;
    LHandles[LHandleCount] := AHandle;
    Inc(LHandleCount);
  end;

begin
  LName := UnicodeString(Executable);

  if (LName = '') or (Pos(#0, LName) > 0) then
  begin
    raise EProcess.Create('Compiler creation requires a structured executable name');
  end;
  LCommand := LName;

  if not SkipCommandLineQuotes then
  begin
    LCommand := CompilerArgument(LName);
  end;
  for LIndex := 0 to Parameters.Count - 1 do
  begin

    if Pos(#0, Parameters[LIndex]) > 0 then
    begin
      raise EProcess.Create('Compiler arguments cannot contain NUL');
    end;

    if SkipCommandLineQuotes then
    begin
      LCommand := LCommand + ' ' + UnicodeString(Parameters[LIndex]);
    end
    else
    begin
      LCommand := LCommand + ' ' + CompilerArgument(UnicodeString(Parameters[LIndex]));
    end;
  end;

  if Length(LCommand) >= 32767 then
  begin
    raise EProcess.Create('Compiler arguments exceed the Windows command-line limit');
  end;
  LDirectory := UnicodeString(CurrentDirectory);
  LDesktop := UnicodeString(Desktop);
  LTitle := UnicodeString(ConsoleTitle);

  if (Pos(#0, LDirectory) > 0) or (Pos(#0, LDesktop) > 0) or (Pos(#0, LTitle) > 0) then
  begin
    raise EProcess.Create('Compiler startup names cannot contain NUL');
  end;
  LEnvironment := '';
  for LIndex := 0 to Environment.Count - 1 do
  begin

    if Pos(#0, Environment[LIndex]) > 0 then
    begin
      raise EProcess.Create('Compiler environment entries cannot contain NUL');
    end;
    LEnvironment := LEnvironment + UnicodeString(Environment[LIndex]) + #0;
  end;
  LEnvironmentPointer := nil;

  if LEnvironment <> '' then
  begin
    LEnvironment := LEnvironment + #0;
    LEnvironmentPointer := PWideChar(LEnvironment);
  end;
  LDirectoryPointer := nil;

  if LDirectory <> '' then
  begin
    LDirectoryPointer := PWideChar(LDirectory);
  end;
  LInput := TCompilerDescriptor(InputDescriptor);
  LOutput := TCompilerDescriptor(OutputDescriptor);
  LError := TCompilerDescriptor(ErrorDescriptor);
  LDefaults := (LInput.IOType = iotDefault) and (LOutput.IOType = iotDefault) and
    (LError.IOType = iotDefault) and not (poStderrToOutput in Options);
  LStartup := Default(TCompilerStartup);
  LInformation := Default(TProcessInformation);
  LAttributesReady := False;
  try
    LInput.PrepareHandles;
    LOutput.PrepareHandles;

    if not (poStderrToOutput in Options) then
    begin
      LError.PrepareHandles;
    end;
    LStartup.Info.cb := SizeOf(LStartup);
    LStartup.Info.lpDesktop := PWideChar(LDesktop);
    LStartup.Info.lpTitle := PWideChar(LTitle);
    LStartup.Info.wShowWindow := CWindow[ShowWindow];

    if (ShowWindow <> swoNone) or (suoUseShowWindow in StartupOptions) then
    begin
      LStartup.Info.dwFlags := LStartup.Info.dwFlags or STARTF_USESHOWWINDOW;
    end;

    if suoUseSize in StartupOptions then
    begin
      LStartup.Info.dwFlags := LStartup.Info.dwFlags or STARTF_USESIZE;
    end;

    if suoUsePosition in StartupOptions then
    begin
      LStartup.Info.dwFlags := LStartup.Info.dwFlags or STARTF_USEPOSITION;
    end;

    if suoUseCountChars in StartupOptions then
    begin
      LStartup.Info.dwFlags := LStartup.Info.dwFlags or STARTF_USECOUNTCHARS;
    end;

    if (FillAttribute <> 0) or (suoUseFillAttribute in StartupOptions) then
    begin
      LStartup.Info.dwFlags := LStartup.Info.dwFlags or STARTF_USEFILLATTRIBUTE;
    end;
    LStartup.Info.dwFillAttribute := FillAttribute;
    LStartup.Info.dwXCountChars := WindowColumns;
    LStartup.Info.dwYCountChars := WindowRows;
    LStartup.Info.dwXSize := WindowWidth;
    LStartup.Info.dwYSize := WindowHeight;
    LStartup.Info.dwX := WindowLeft;
    LStartup.Info.dwY := WindowTop;
    LHandleCount := 0;

    if not LDefaults then
    begin
      LStartup.Info.dwFlags := LStartup.Info.dwFlags or STARTF_USESTDHANDLES;
      LStartup.Info.hStdInput := LInput.ResolveProcessHandle;
      LStartup.Info.hStdOutput := LOutput.ResolveProcessHandle;
      LStartup.Info.hStdError := LStartup.Info.hStdOutput;

      if not (poStderrToOutput in Options) then
      begin
        LStartup.Info.hStdError := LError.ResolveProcessHandle;
      end;
      IncludeHandle(LStartup.Info.hStdInput);
      IncludeHandle(LStartup.Info.hStdOutput);
      IncludeHandle(LStartup.Info.hStdError);
    end;
    LAttributeSize := 0;
    InitializeCompilerAttributes(nil, 2, 0, LAttributeSize);

    if LAttributeSize = 0 then
    begin
      RaiseLastOSError;
    end;
    GetMem(LStartup.Attributes, LAttributeSize);

    if not InitializeCompilerAttributes(LStartup.Attributes, 2, 0, LAttributeSize) then
    begin
      RaiseLastOSError;
    end;
    LAttributesReady := True;

    if not UpdateCompilerAttribute(LStartup.Attributes, 0, CAttributeJobs,
      @FFamily, SizeOf(FFamily), nil, nil) then
    begin
      raise EProcess.CreateFmt('Windows refused creation-time compiler job ownership (%d)', [GetLastError]);
    end;

    if InheritHandles and (LHandleCount > 0) and not UpdateCompilerAttribute(
      LStartup.Attributes, 0, CAttributeHandles, @LHandles[0],
      LHandleCount * SizeOf(THandle), nil, nil) then
    begin
      RaiseLastOSError;
    end;
    LFlags := CREATE_UNICODE_ENVIRONMENT or CREATE_SUSPENDED or
      CStartupExtended or CPriority[Priority];

    if poNoConsole in Options then
    begin
      LFlags := LFlags or CREATE_NO_WINDOW;
    end;

    if poNewConsole in Options then
    begin
      LFlags := LFlags or CREATE_NEW_CONSOLE;
    end;

    if poNewProcessGroup in Options then
    begin
      LFlags := LFlags or CREATE_NEW_PROCESS_GROUP;
    end;

    if poDefaultErrorMode in Options then
    begin
      LFlags := LFlags or CREATE_DEFAULT_ERROR_MODE;
    end;

    if poDetached in Options then
    begin
      LFlags := LFlags or DETACHED_PROCESS;
    end;

    if poDebugProcess in Options then
    begin
      LFlags := LFlags or DEBUG_PROCESS;
    end;

    if poDebugOnlyThisProcess in Options then
    begin
      LFlags := LFlags or DEBUG_ONLY_THIS_PROCESS;
    end;
    UniqueString(LCommand);

    if not CreateProcessW(PWideChar(LName), PWideChar(LCommand), nil, nil,
      InheritHandles, LFlags, LEnvironmentPointer, LDirectoryPointer,
      LStartup.Info, LInformation) then
    begin
      raise EProcess.CreateFmt('Windows could not create the owned compiler (%d)', [GetLastError]);
    end;
    { No allocation/callback may intervene between success and adopting these
      handles. The child is already in FFamily even if this host dies here. }
    FProcessHandle := LInformation.hProcess;
    FThreadHandle := LInformation.hThread;
    FProcessID := LInformation.dwProcessId;
    FThreadID := LInformation.dwThreadId;
    FAssigned := True;
    FRunning := True;
  finally

    if LAttributesReady then
    begin
      DeleteCompilerAttributes(LStartup.Attributes);
    end;
    FreeMem(LStartup.Attributes);
    LInput.CloseTheirHandle;
    LOutput.CloseTheirHandle;

    if not (poStderrToOutput in Options) then
    begin
      LError.CloseTheirHandle;
    end;
  end;
end;
