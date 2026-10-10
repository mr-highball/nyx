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

program nyx_build_compiler_fixture;
{$mode delphi}{$H+}{$codepage utf8}
uses Classes, SysUtils, Process, nyx.text, nyx.test.compiler.fixture,
  {$ifdef MSWINDOWS}Windows{$else}BaseUnix{$endif};

procedure Ready(ARole: TNyxCompilerFixtureRole);
var
  LStream: TFileStream;
  LIdentity: UTF8String;
  LName: TNyxText;
begin
  { The producer's dedicated child permits atomic marker publication while the
    actual source-invocation root remains pinned. Never silently ignore a failed
    rename: readers must observe a complete PID written by this physical process. }
  LName := NyxCompilerFixtureMarkerPath(TNyxText(GetCurrentDir), ARole);

  if not ForceDirectories(ExtractFileDir(LName)) then
  begin
    raise Exception.Create('Cannot create compiler fixture status directory');
  end;
  {$ifdef MSWINDOWS}
  LIdentity := IntToStr(GetCurrentProcessId);
  {$else}
  LIdentity := IntToStr(fpGetPID);
  {$endif}
  LStream := TFileStream.Create(LName + '.tmp', fmCreate);
  try
    LStream.WriteBuffer(LIdentity[1], Length(LIdentity));
  finally
    LStream.Free;
  end;

  if not RenameFile(LName + '.tmp', LName) then
  begin
    raise Exception.Create('Cannot atomically publish compiler fixture identity');
  end;
end;

procedure SpawnPart(const ARole, APolicy: String);
var
  LPart: TProcess;
begin
  { This deliberate detached handle exercises the production invocation job.
    Each descendant belongs to that job even when its immediate parent exits.
    Do not inherit stdout: family accounting must work independently of pipes. }
  LPart := TProcess.Create(nil);
  try
    LPart.Executable := ParamStr(0);
    LPart.CurrentDirectory := GetCurrentDir;
    LPart.Options := [poNoConsole];
    LPart.InheritHandles := False;
    LPart.Parameters.Add('--nyx-fixture-child');
    LPart.Parameters.Add(ARole);
    LPart.Parameters.Add(APolicy);
    LPart.Execute;
  finally
    LPart.Free;
  end;
end;

procedure AwaitFamilyGate;
var
  LStart: QWord;
begin
  LStart := GetTickCount64;
  while not FileExists('family.continue') do
  begin

    if GetTickCount64 - LStart > 10000 then
    begin
      raise Exception.Create('Owned family fixture was not released');
    end;
    Sleep(10);
  end;
end;

var
  LFile: TFileStream;
  LText: UTF8String;
  LMode: TStringList;
  LStarted: QWord;
  LPolicy: String;
  LOutput: THandleStream;
begin
  { Descendants publish their own identity. The helper immediately creates a
    grandchild so checks cover nested ownership, not just one child PID. }

  if ParamStr(1) = '--nyx-fixture-child' then
  begin

    if ParamStr(2) = 'helper' then
    begin
      SpawnPart('grandchild', ParamStr(3));
      Ready(cfrHelper);
    end
    else if ParamStr(2) = 'grandchild' then
    begin
      Ready(cfrGrandchild);
    end
    else
    begin
      raise Exception.Create('Unknown compiler fixture child role');
    end;

    if ParamStr(3) = 'family-success' then
    begin
      AwaitFamilyGate;
      Sleep(300);
    end
    else
    begin
      Sleep(30000);
    end;
    Exit;
  end;
  { Owned substitute solely for lifetime/resource admission checks. Actual MCP
    qualification uses real pas2js/FPC. It sleeps long enough to establish the
    two-worker ceiling, then emits a tiny deterministic browser output. }
  { Each actual child records its own identity in its unique invocation root.
    Only this fixture interprets the parent-owned policy file; production
    executors still supply their fixed compiler arguments unchanged. }
  Ready(cfrCompiler);
  LPolicy := '';
  LMode := TStringList.Create;
  try

    if FileExists('../fixture.mode') then
    begin
      LMode.LoadFromFile('../fixture.mode');
      LPolicy := Trim(LMode.Text);
    end;
  finally
    LMode.Free;
  end;

  if Pos('family-', LPolicy) = 1 then
  begin
    SpawnPart('helper', LPolicy);
    AwaitFamilyGate;
  end;

  if LPolicy = 'family-error' then
  begin
    WriteLn('Error: Owned compiler failure before helper retirement');
    ExitCode := 1;
    Exit;
  end;

  if (LPolicy = 'hold') or (LPolicy = 'family-hold') then
  begin
    Sleep(30000);
  end
  else if LPolicy = 'editor-hold' then
  begin
    { The checked native editor can spend seconds in a layout/heap-trace paint.
      Keep the actual compiler alive until its visible action retires it. The
      production invocation deadline still owns the sixty-second upper bound. }
    Sleep(60000);
  end
  else if (LPolicy = 'flood') or (LPolicy = 'unicode-flood') or
    (LPolicy = 'family-flood') then
  begin
    LText := StringOfChar('x', 4096);

    if LPolicy = 'unicode-flood' then
    begin
      LText := UTF8String(StringOfChar('x', 4093));
      LText := LText + UTF8String('🌙');
    end;
    LText := LText + UTF8String(LineEnding);
    LStarted := GetTickCount64;
    { Stdout is an HTTP/compiler byte boundary. Text-file output can transcode
      UTF8String through the console/ANSI codepage even when stdout is a pipe.
      Borrow the handle; the stream frees no OS handle and changes no codepage. }
    LOutput := THandleStream.Create(TTextRec(Output).Handle);
    try
      repeat
        LOutput.WriteBuffer(LText[1], Length(LText));
      until GetTickCount64 - LStarted > 30000;
    finally
      LOutput.Free;
    end;
  end
  else if Pos('family-', LPolicy) <> 1 then
  begin
    Sleep(750);
  end;
  LText := '/* Nyx job resource fixture; not an application */' + #10;
  LFile := TFileStream.Create('nyx_preview.js', fmCreate);
  try
    LFile.WriteBuffer(LText[1], Length(LText));
  finally
    LFile.Free;
  end;
end.
