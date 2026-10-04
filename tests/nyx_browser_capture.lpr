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

program nyx_browser_capture;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, Process;

var
  LProcess: TProcess;
  LDirectory: String;
  LOutput: RawByteString;
  LChunk: RawByteString;
  LBuffer: array[0..16383] of Byte;
  LRead: Integer;
  LStarted: QWord;
  LFile: TFileStream;
  LExpected: RawByteString;

begin
  LProcess := nil;
  try

    if (ParamCount <> 3) or (Pos('http://127.0.0.1:', ParamStr(1)) <> 1) then
    begin
      raise Exception.Create('Supply localhost fixture URL, build artifact directory and expected passed attribute');
    end;
    LDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
    ForceDirectories(LDirectory);
    LExpected := ParamStr(3) + '="passed"';
    LProcess := TProcess.Create(nil);
    LProcess.Executable := GetEnvironmentVariable('ProgramFiles(x86)') +
      '\Microsoft\Edge\Application\msedge.exe';
    LProcess.Options := [poUsePipes, poStderrToOutput, poNoConsole];
    LProcess.Parameters.Add('--headless=new');
    LProcess.Parameters.Add('--disable-gpu');
    LProcess.Parameters.Add('--no-first-run');
    LProcess.Parameters.Add('--no-default-browser-check');
    LProcess.Parameters.Add('--disable-extensions');
    LProcess.Parameters.Add('--user-data-dir=' + LDirectory + 'profile');
    LProcess.Parameters.Add('--window-size=1100,1000');
    LProcess.Parameters.Add('--virtual-time-budget=90000');
    LProcess.Parameters.Add('--dump-dom');
    LProcess.Parameters.Add('--screenshot=' + LDirectory + 'capture.png');
    LProcess.Parameters.Add(ParamStr(1));
    LProcess.Execute;
    LStarted := GetTickCount64;
    { This helper only orchestrates a browser artifact. Pascal in the loaded
      fixture owns every semantic assertion. Drain the process continuously,
      retain exact bytes and bound its lifetime rather than waiting on a pipe. }
    repeat
      while LProcess.Output.NumBytesAvailable > 0 do
      begin
        LRead := LProcess.Output.Read(LBuffer, SizeOf(LBuffer));
        SetLength(LChunk, LRead);

        if LRead > 0 then
        begin
          Move(LBuffer[0], LChunk[1], LRead);
        end;
        LOutput := LOutput + LChunk;

        if Length(LOutput) > 8 * 1024 * 1024 then
        begin
          raise Exception.Create('Browser fixture output exceeds capture budget');
        end;
      end;

      if GetTickCount64 - LStarted > 360000 then
      begin
        raise Exception.Create('Browser fixture exceeded functional capture lifetime');
      end;

      if LProcess.Running then
      begin
        Sleep(10);
      end;
    until not LProcess.Running and (LProcess.Output.NumBytesAvailable = 0);
    LFile := TFileStream.Create(LDirectory + 'capture.dom.html', fmCreate);
    try

      if LOutput <> '' then
      begin
        LFile.WriteBuffer(LOutput[1], Length(LOutput));
      end;
    finally
      LFile.Free;
    end;

    if Pos(LExpected, LOutput) = 0 then
    begin
      raise Exception.Create('Pascal browser fixture did not publish its passed marker');
    end;
    LProcess.Free;
    LProcess := nil;
    WriteLn('PASS browser artifact ', ParamStr(1));
  except
    on LException: Exception do
    begin

      if (LProcess <> nil) and LProcess.Running then
      begin
        LProcess.Terminate(1);
        LProcess.WaitOnExit;
      end;
      LProcess.Free;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
