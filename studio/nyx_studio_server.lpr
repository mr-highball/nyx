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

program nyx_studio_server;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  nyx.text,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  nyx.studio.server,
  nyx.studio.recovery,
  nyx.studio.directories;

var
  LServer: TNyxStudioServer;
  LRepository: TNyxText;
  LBindAddress: TNyxText;
  LPort: Integer;
  LMCPPort: Integer;
  LWebRoot: TNyxText;
  LDirectories: TNyxStudioDirectories;
  LRecoveryMode: TNyxRuntimeRecoveryMode;
  LRecoveryChoice: TNyxText;
const
  { Host CLI boundary only; portable designs never contain this execution choice. }
  CImmediateRecovery = 'immediate';
  CBrowserWorkerRecovery = 'browser-worker';
begin
  { Launch from the repository by default. An explicit root lets an IDE or build
    script run the same service without depending on its current directory. }
  LRepository := GetCurrentDir;

  if ParamCount > 0 then
  begin
    LRepository := ParamStr(1);
  end;
  LPort := 8088;

  if ParamCount > 1 then
  begin
    LPort := StrToInt(ParamStr(2));
  end;
  { The third argument selects an interface independently of output compilers
    and the design. Explicit LAN binding retains ordinary loopback access too. }
  LBindAddress := '127.0.0.1';

  if ParamCount > 2 then
  begin
    LBindAddress := ParamStr(3);
  end;
  LMCPPort := 0;

  if ParamCount > 3 then
  begin
    LMCPPort := StrToInt(ParamStr(4));
  end;
  LWebRoot := '';

  if ParamCount > 4 then
  begin
    LWebRoot := ParamStr(5);
  end;
  LRecoveryMode := rrmImmediate;

  if ParamCount > 7 then
  begin
    LRecoveryChoice := ParamStr(8);

    if LRecoveryChoice = CBrowserWorkerRecovery then
    begin
      LRecoveryMode := rrmBrowserWorker;
    end
    else if LRecoveryChoice <> CImmediateRecovery then
    begin
      raise Exception.Create('Recovery mode must be immediate or browser-worker');
    end;
  end;

  if ParamCount > 8 then
  begin
    raise Exception.Create('Studio accepts at most eight host arguments');
  end;
  LDirectories := TNyxStudioDirectories.ForRepository(LRepository);

  if ParamCount > 5 then
  begin
    { The optional sixth argument selects a pristine release with a separate
      private runtime home. Earlier repository invocations retain their layout. }
    LDirectories := TNyxStudioDirectories.ForRelease(LRepository, ParamStr(6));
  end;

  if ParamCount > 6 then
  begin
    LDirectories := LDirectories.EnrollingProject(ParamStr(7));
  end;
  { Immediate preserves compiler-independent literal startup. Explicit browser
    recovery retains saved projects privately while the shell/configuration load.
    Its visible operator action controls later execution; choosing a document
    output target alone cannot start recovery or supply execution authority. }
  LServer := TNyxStudioServer.Create(LDirectories, LPort, LBindAddress, LMCPPort,
    LWebRoot, nil, LRecoveryMode);
  try
    LServer.Run;
  finally
    LServer.Free;
  end;
end.
