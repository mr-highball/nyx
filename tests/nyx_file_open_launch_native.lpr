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

program nyx_file_open_launch_native;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, Process, Windows, ShellApi, nyx.text, nyx.bytes, nyx.data,
  nyx.logic, nyx.test.files.openfixture;

var
  GProcess: TProcess;
  GWindow: HWND;
  GDialog: HWND;
  GButton: HWND;
  GFilename: HWND;
  GExpected: TNyxText;
  GReport: Boolean;
  GDump: Boolean;
  GChecks: Integer;

{ Read the OS command line as UTF-16. Native ANSI ParamStr cannot transport a
  supplementary filename reliably, even when the caller supplied exact bytes. }
function InputPath: UnicodeString;
var
  LArguments: PLPWSTR;
  LCount: Longint;
  LValue: UnicodeString;
  LLength: DWORD;
begin
  LArguments := CommandLineToArgvW(GetCommandLineW, @LCount);

  if LArguments = nil then
  begin
    raise Exception.Create('Cannot inspect the owned input path');
  end;
  try

    if LCount <> 4 then
    begin
      raise Exception.Create('Expected executable, input and mode arguments');
    end;
    LValue := UnicodeString(PLPWSTR(PtrUInt(LArguments) + 2 * SizeOf(PWideChar))^);
  finally
    LocalFree(HLOCAL(LArguments));
  end;
  SetLength(Result, 32768);
  LLength := GetFullPathNameW(PWideChar(LValue), Length(Result), PWideChar(Result), PLPWSTR(nil));

  if (LLength = 0) or (LLength >= DWORD(Length(Result))) then
  begin
    raise Exception.Create('Cannot resolve the owned UTF-16 input path');
  end;
  SetLength(Result, LLength);
end;

procedure Check(AValue: Boolean; const AMessage: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

function Text(AWindow: HWND): TNyxText;
var
  LBuffer: array[0..16383] of WideChar;
  LReply: DWORD_PTR;
begin
  Result := '';
  LBuffer[0] := #0;

  if SendMessageTimeoutW(AWindow, WM_GETTEXT, Length(LBuffer), LPARAM(@LBuffer[0]),
    SMTO_ABORTIFHUNG, 200, @LReply) <> 0 then
  begin
    Result := UTF8Encode(UnicodeString(PWideChar(@LBuffer[0])));
  end;
end;

{ Normalize only host line endings. Keep each original UTF-8 byte typed; ANSI
  RTL replacement would silently corrupt supplementary text in this oracle. }
function Lines(const AText: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  for LIndex := 1 to Length(AText) do
  begin

    if AText[LIndex] <> #13 then
    begin
      Result := Result + Copy(AText, LIndex, 1);
    end;
  end;
end;

function WindowFace(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LProcessID: DWORD;
  LCaption: TNyxText;
begin
  Result := True;
  GetWindowThreadProcessId(AWindow, @LProcessID);

  if (LProcessID <> GProcess.ProcessID) or not IsWindowVisible(AWindow) then
  begin
    Exit;
  end;
  LCaption := Text(AWindow);

  if LCaption = 'A report worth keeping' then
  begin
    GWindow := AWindow;
  end
  else if LCaption = 'Choose a report' then
  begin
    GDialog := AWindow;
  end;
end;

function ChildFace(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LText: TNyxText;
begin
  Result := True;
  LText := Text(AWindow);

  if GDump and (LText <> '') then
  begin
    { Failure-only ASCII receipt preserves exact observed UTF-8 bytes independently
      of the console codepage; no other process or application text is collected. }
    WriteLn('Observed face base64: ', NyxEncodeBase64(NyxEncodeUTF8(LText), 65536));
  end;

  if LText = 'Open report' then
  begin
    GButton := AWindow;
  end;

  if (GExpected <> '') and (Pos(GExpected, Lines(LText)) > 0) then
  begin
    GReport := True;
  end;
end;

function FilenameFace(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LClass: array[0..63] of WideChar;
begin
  Result := True;
  GetClassNameW(AWindow, LClass, Length(LClass));

  if (UnicodeString(LClass) = 'Edit') and
    ((GetDlgCtrlID(AWindow) = 1001) or (GetDlgCtrlID(AWindow) = 1148)) then
  begin
    GFilename := AWindow;
    Result := False;
  end;
end;

procedure Open;
begin
  GButton := 0;
  EnumChildWindows(GWindow, @ChildFace, 0);
  Check(GButton <> 0, 'generated native application mounts its actual Open control');
  PostMessageW(GButton, WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(8, 8));
  PostMessageW(GButton, WM_LBUTTONUP, 0, MakeLParam(8, 8));
end;

procedure WaitDialog;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    GDialog := 0;
    EnumWindows(@WindowFace, 0);

    if GDialog <> 0 then
    begin
      Exit;
    end;

    if not GProcess.Running or (GetTickCount64 - LStarted > 15000) then
    begin
      raise Exception.Create('Generated native application did not open its file dialog');
    end;
    Sleep(20);
  until False;
end;

procedure WaitReport;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    GReport := False;
    EnumChildWindows(GWindow, @ChildFace, 0);

    if GReport then
    begin
      Exit;
    end;

    if not GProcess.Running or (GetTickCount64 - LStarted > 15000) then
    begin
      GDump := True;
      EnumChildWindows(GWindow, @ChildFace, 0);
      GDump := False;
      raise Exception.Create('Generated native application did not publish its complete report');
    end;
    Sleep(20);
  until False;
end;

var
  LEnabled: Boolean;
  LStarted: QWord;
  LPath: UnicodeString;
  LReply: DWORD_PTR;
begin
  GProcess := nil;
  try

    if (ParamCount <> 3) or not ((ParamStr(3) = 'enabled') or (ParamStr(3) = 'disabled')) then
    begin
      raise Exception.Create('Supply owned compiled nyx_native.exe, input file, enabled|disabled');
    end;
    LEnabled := ParamStr(3) = 'enabled';
    GExpected := NewNyxJSONFormat(njlIndented, 2).Format(TNyxDataValue.ParseJSON(OpenFixtureJSON));
    GProcess := TProcess.Create(nil);
    GProcess.Executable := ExpandFileName(ParamStr(1));
    GProcess.CurrentDirectory := ExtractFileDir(GProcess.Executable);
    GProcess.Options := [poNoConsole];
    GProcess.Execute;
    LStarted := GetTickCount64;
    repeat
      EnumWindows(@WindowFace, 0);

      if GWindow <> 0 then
      begin
        Break;
      end;

      if not GProcess.Running or (GetTickCount64 - LStarted > 15000) then
      begin
        raise Exception.Create('Owned generated native application did not mount');
      end;
      Sleep(20);
    until False;
    Open;

    if LEnabled then
    begin
      WaitDialog;
      GFilename := 0;
      EnumChildWindows(GDialog, @FilenameFace, 0);
      Check(GFilename <> 0, 'owned native picker exposes its real filename edit');
      LPath := InputPath;
      Check(GetFileAttributesW(PWideChar(LPath)) <> INVALID_FILE_ATTRIBUTES,
        'owned UTF-16 picker input resolves to the actual fixture file');
      Check(SendMessageTimeoutW(GFilename, WM_SETTEXT, 0, LPARAM(PWideChar(LPath)),
        SMTO_ABORTIFHUNG, 1000, @LReply) <> 0, 'actual native picker accepts the owned input path');
      PostMessageW(GDialog, WM_COMMAND, IDOK, 0);
      WaitReport;
      Check(GReport, 'ordinary generated native launcher reads and formats exact Unicode/NUL/numeric data');
      Open;
      WaitDialog;
      PostMessageW(GDialog, WM_COMMAND, IDCANCEL, 0);
      Sleep(300);
      WaitReport;
      Check(GReport, 'actual native cancellation retains the preceding report');
    end
    else
    begin
      Sleep(500);
      GDialog := 0;
      EnumWindows(@WindowFace, 0);
      GReport := False;
      EnumChildWindows(GWindow, @ChildFace, 0);
      Check((GDialog = 0) and not GReport,
        'ungranted native launcher opens no picker and publishes no report');
    end;
    PostMessageW(GWindow, WM_CLOSE, 0, 0);
    LStarted := GetTickCount64;
    while GProcess.Running and (GetTickCount64 - LStarted < 5000) do
    begin
      Sleep(20);
    end;
    Check(not GProcess.Running and (GProcess.ExitStatus = 0), 'owned compiled application closes normally');
    WriteLn('PASS ', GChecks, ' ordinary generated native opening checks / ', ParamStr(3));
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;

  if GProcess <> nil then
  begin

    if GProcess.Running then
    begin
      { Exceptional cleanup uses only this held owned child process. No existing
        Studio/server or user process is discovered or stopped by this tool. }
      GProcess.Terminate(1);
      GProcess.WaitOnExit;
    end;
    GProcess.Free;
  end;
end.
