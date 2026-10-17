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
program nyx_compiler_admission_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, Windows, Process, nyx.text, nyx.studio.compilerprocess;

type
  { Owned child host stops at the real virtual FCL startup return, before the
    outer compiler adapter can Resume. The parent fixture owns its exact process
    handle and only the child identity published by this invocation. }
  TAdmissionProbe = class(TNyxCompilerProcess)
  protected
    procedure SysExecute; override;
  end;

var
  GChecks: Integer;

function NativeHandleCount(AProcess: THandle; out ACount: Cardinal): BOOL;
  stdcall; external 'kernel32' name 'GetProcessHandleCount';
function NativeProcessImage(AProcess: THandle; AFlags: Cardinal; AName: PWideChar;
  var ASize: Cardinal): BOOL;
  stdcall; external 'kernel32' name 'QueryFullProcessImageNameW';

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TAdmissionProbe.SysExecute;
var
  LFile: TFileStream;
  LIdentity: String;
begin
  inherited SysExecute;
  LIdentity := IntToStr(ProcessID);
  LFile := TFileStream.Create(ParamStr(2) + '.partial', fmCreate);
  try
    LFile.WriteBuffer(LIdentity[1], Length(LIdentity));
  finally
    LFile.Free;
  end;

  if not RenameFile(ParamStr(2) + '.partial', ParamStr(2)) then
  begin
    raise Exception.Create('Cannot publish exact owned child identity');
  end;
  { The parent deliberately kills this host without Pascal finalizers. A
    normal return/destructor would prove cleanup, not creation-time ownership. }
  Sleep(60000);
  raise Exception.Create('Admission host was not retired by its owning fixture');
end;

procedure HostMode;
var
  LProcess: TAdmissionProbe;
begin
  LProcess := TAdmissionProbe.Create(nil);
  try
    LProcess.Executable := ParamStr(0);
    LProcess.Parameters.Add('--child');
    LProcess.Options := [poUsePipes, poStderrToOutput, poNoConsole];
    LProcess.Execute;
  finally
    LProcess.Free;
  end;
end;

procedure CrashDuringAdmission(const ADirectory: String);
var
  LHost: TProcess;
  LChild: THandle;
  LChildPID: Cardinal;
  LReady: String;
  LText: TStringList;
  LStarted: QWord;
  LCreation: TFileTime;
  LExit: TFileTime;
  LKernel: TFileTime;
  LUser: TFileTime;
  LParentCreation: TFileTime;
  LParentImage: array[0..2047] of WideChar;
  LChildImage: array[0..2047] of WideChar;
  LImageSize: Cardinal;
  LVerifiedChild: Boolean;
begin
  LReady := IncludeTrailingPathDelimiter(ADirectory) + 'admission.ready';
  LChild := 0;
  LVerifiedChild := False;
  LHost := TProcess.Create(nil);
  try
    LHost.Executable := ParamStr(0);
    LHost.Parameters.Add('--host');
    LHost.Parameters.Add(LReady);
    LHost.Options := [poNoConsole];
    LHost.Execute;
    Check(GetProcessTimes(LHost.ProcessHandle, LParentCreation, LExit, LKernel, LUser),
      'owning host birth time is captured before observing its child');
    LStarted := GetTickCount64;
    while not FileExists(LReady) and LHost.Running and (GetTickCount64 - LStarted < 10000) do
    begin
      Sleep(5);
    end;
    Check(FileExists(LReady), 'actual owning host reaches the pre-resume boundary');
    LText := TStringList.Create;
    try
      LText.LoadFromFile(LReady);
      LChildPID := StrToInt(LText.Text.Trim);
    finally
      LText.Free;
    end;
    Check((LChildPID <> GetCurrentProcessId) and (LChildPID <> Cardinal(LHost.ProcessID)),
      'the marker identifies a distinct invocation-owned child');
    LChild := OpenProcess(SYNCHRONIZE or PROCESS_QUERY_INFORMATION or PROCESS_TERMINATE,
      False, LChildPID);
    Check(LChild <> 0, 'the parent retains the exact actual child handle');
    Check(GetProcessTimes(LChild, LCreation, LExit, LKernel, LUser),
      'captured child has a real process lifetime');
    LImageSize := Length(LChildImage);
    Check((GetModuleFileNameW(0, @LParentImage[0], Length(LParentImage)) > 0) and
      NativeProcessImage(LChild, 0, @LChildImage[0], LImageSize) and
      (lstrcmpiW(@LParentImage[0], @LChildImage[0]) = 0) and
      ((LCreation.dwHighDateTime > LParentCreation.dwHighDateTime) or
       ((LCreation.dwHighDateTime = LParentCreation.dwHighDateTime) and
        (LCreation.dwLowDateTime >= LParentCreation.dwLowDateTime))),
      'the exact executable and post-host birth qualify child cleanup authority');
    LVerifiedChild := True;
    Check(WaitForSingleObject(LChild, 0) = WAIT_TIMEOUT, 'child is still alive before abrupt host exit');
    Check(LHost.Terminate(71) and LHost.WaitOnExit(5000), 'owned host exits abruptly without running finalizers');
    Check(WaitForSingleObject(LChild, 5000) = WAIT_OBJECT_0,
      'creation-time job ownership retires the suspended child after abrupt host death');
  finally
    { This also joins an unowned child from the deliberately failing old-source
      comparison. Never leave a qualification orphan behind after an assertion. }

    if LChild <> 0 then
    begin

      if LVerifiedChild and (WaitForSingleObject(LChild, 0) <> WAIT_OBJECT_0) then
      begin
        TerminateProcess(LChild, 72);
        WaitForSingleObject(LChild, 5000);
      end;
      CloseHandle(LChild);
    end;

    if LHost.Running then
    begin
      LHost.Terminate(73);
      LHost.WaitOnExit(5000);
    end;
    LHost.Free;
  end;
end;

procedure ArgumentsAndRefusal;
const
  CValues: array[0..4] of String = ('one', 'two words', 'folder\', '-Fuspace folder\', 'literal$&value');
var
  LProcess: TNyxCompilerProcess;
  LExpected: String;
  LActual: String;
  LHeap: String;
  LChunk: String;
  LBuffer: array[0..1023] of Byte;
  LRead: Integer;
  LIndex: Integer;
  LStarted: QWord;
  LFailed: Boolean;
  LBefore: Cardinal;
  LAfter: Cardinal;
begin
  LProcess := TNyxCompilerProcess.Create(nil);
  try
    LProcess.Executable := ParamStr(0);
    LProcess.Parameters.Add('--echo');
    LProcess.CurrentDirectory := ExpandFileName(ParamStr(1));
    LProcess.Environment.Add('NYX_COMPILER_ADMISSION=Owned value');
    LExpected := '';
    for LIndex := 0 to High(CValues) do
    begin
      LProcess.Parameters.Add(CValues[LIndex]);
      LExpected := LExpected + '[' + CValues[LIndex] + ']' + LineEnding;
    end;
    LExpected := LExpected + '[directory=' + ExpandFileName(ParamStr(1)) + ']' + LineEnding +
      '[environment=Owned value]' + LineEnding;
    LProcess.Options := [poUsePipes, poNoConsole];
    LProcess.Execute;
    LActual := '';
    LHeap := '';
    LStarted := GetTickCount64;
    repeat
      while LProcess.Output.NumBytesAvailable > 0 do
      begin
        LRead := LProcess.Output.Read(LBuffer, SizeOf(LBuffer));
        SetLength(LChunk, LRead);

        if LRead > 0 then
        begin
          Move(LBuffer[0], LChunk[1], LRead);
          LActual := LActual + LChunk;
        end;
      end;
      while LProcess.Stderr.NumBytesAvailable > 0 do
      begin
        LRead := LProcess.Stderr.Read(LBuffer, SizeOf(LBuffer));
        SetLength(LChunk, LRead);

        if LRead > 0 then
        begin
          Move(LBuffer[0], LChunk[1], LRead);
          LHeap := LHeap + LChunk;
        end;
      end;

      if GetTickCount64 - LStarted >= 10000 then
      begin
        raise Exception.Create('Normal invocation exceeded its bounded fixture deadline');
      end;
      Sleep(1);
    until not LProcess.FamilyRunning and (LProcess.Output.NumBytesAvailable = 0) and
      (LProcess.Stderr.NumBytesAvailable = 0);
    Check((LProcess.ExitStatus = 0) and (LActual = LExpected),
      'structured compiler arguments retain FCL spaces, trailing backslashes and literal shell characters / got=' + LActual);
    Check(Pos('0 unfreed memory blocks : 0', LHeap) > 0, 'the independently executed echo child has a clean heap');
    LProcess.RetireAndJoin;
    LProcess.RetireAndJoin;
    LFailed := False;
    try
      LProcess.Execute;
    except
      on E: EProcess do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'an invocation cannot be reused after retirement');
  finally
    LProcess.Free;
  end;
  LProcess := TNyxCompilerProcess.Create(nil);
  try
    LProcess.Executable := ParamStr(0);
    LProcess.Parameters.Add('invalid' + #0 + 'argument');
    LProcess.Options := [poUsePipes, poNoConsole];
    LFailed := False;
    try
      LProcess.Execute;
    except
      on E: EProcess do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LProcess.ProcessHandle = 0), 'NUL input refuses before creating a child');
  finally
    LProcess.Free;
  end;
  Check(NativeHandleCount(GetCurrentProcess, LBefore), 'capture native handle baseline');
  for LIndex := 1 to 12 do
  begin
    LProcess := TNyxCompilerProcess.Create(nil);
    try
      LProcess.Executable := ParamStr(0) + '.missing';
      LProcess.Options := [poUsePipes, poStderrToOutput, poNoConsole];
      LFailed := False;
      try
        LProcess.Execute;
      except
        on E: EProcess do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed and (LProcess.ProcessHandle = 0), 'failed OS creation publishes no child handle');
    finally
      LProcess.Free;
    end;
  end;
  Check(NativeHandleCount(GetCurrentProcess, LAfter) and (LBefore = LAfter),
    'failed creation retires attribute lists, descriptors and job handles');
end;

var
  LIndex: Integer;
begin
  try
    if ParamStr(1) = '--child' then
    begin
      Sleep(60000);
    end
    else if ParamStr(1) = '--host' then
    begin
      HostMode;
    end
    else if ParamStr(1) = '--echo' then
    begin
      for LIndex := 2 to ParamCount do
      begin
        WriteLn('[', ParamStr(LIndex), ']');
      end;
      WriteLn('[directory=', GetCurrentDir, ']');
      WriteLn('[environment=', SysUtils.GetEnvironmentVariable('NYX_COMPILER_ADMISSION'), ']');
    end
    else
    begin

      if (ParamCount <> 1) or DirectoryExists(ParamStr(1)) then
      begin
        raise Exception.Create('Supply a fresh owned admission-fixture directory');
      end;
      ForceDirectories(ParamStr(1));
      CrashDuringAdmission(ParamStr(1));
      ArgumentsAndRefusal;
      WriteLn('PASS ', GChecks, ' actual compiler admission checks');
    end;
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
