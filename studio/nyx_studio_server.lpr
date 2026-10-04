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
  nyx.studio.server;

var
  LServer: TNyxStudioServer;
  LRepository: TNyxText;
  LBindAddress: TNyxText;
  LPort: Integer;
  LMCPPort: Integer;
  LWebRoot: TNyxText;
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
  LServer := TNyxStudioServer.Create(LRepository, LPort, LBindAddress, LMCPPort, LWebRoot);
  try
    LServer.Run;
  finally
    LServer.Free;
  end;
end.
