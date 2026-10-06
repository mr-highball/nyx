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
uses Classes, SysUtils, {$ifdef MSWINDOWS}Windows{$else}BaseUnix{$endif};

var
  LFile: TFileStream;
  LText: UTF8String;
  LMode: TStringList;
  LStarted: QWord;
  LPolicy: String;
  LOutput: THandleStream;
begin
  { Owned substitute solely for lifetime/resource admission checks. Actual MCP
    qualification uses real pas2js/FPC. It sleeps long enough to establish the
    two-worker ceiling, then emits a tiny deterministic browser output. }
  { Each actual child records its own identity in its unique invocation root.
    Only this fixture interprets the parent-owned policy file; production
    executors still supply their fixed compiler arguments unchanged. }
  {$ifdef MSWINDOWS}
  LText := IntToStr(GetCurrentProcessId);
  {$else}
  LText := IntToStr(fpGetPID);
  {$endif}
  LFile := TFileStream.Create('compiler.ready.tmp', fmCreate);
  try
    LFile.WriteBuffer(LText[1], Length(LText));
  finally
    LFile.Free;
  end;
  RenameFile('compiler.ready.tmp', 'compiler.ready');
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

  if LPolicy = 'hold' then
  begin
    Sleep(30000);
  end
  else if (LPolicy = 'flood') or (LPolicy = 'unicode-flood') then
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
  else
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
