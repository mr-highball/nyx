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
unit nyx.studio.pascalprofiles;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.context, nyx.pascal.compilertrace;

{ Explicit synchronous native compiler capture; a caller uses a worker when UI
  responsiveness matters. The fresh directory is a caller-authorized private
  scratch location, never an application project or shared Studio checkpoint.
  Its source/log/intermediates remain available for review. Existing directories
  refuse. No executable, assembler or linker produced by the probe is run.
  The configured FPC and its normal configuration select the target; the probe's
  one typed source MODE supplies its per-unit defaults. No host/compiler install
  or project setting is changed. Numbered English FPC messages are required.
  Timeout/cancellation/output/IO failure retires and joins this exact compiler
  family before raising. The ordinary compiler adapter's native platform limits
  apply. This opt-in primitive is not default Studio adoption or a build job. }
function CaptureNyxFPCCompilerContext(const ACompiler, AFreshDirectory: TNyxText;
  const AProbe: INyxPascalCompilerProbe;
  const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope = nil;
  ATimeoutMilliseconds: Cardinal = 30000): INyxPascalCompilerContext;

implementation

uses Classes, SysUtils, Process, LazFileUtils, nyx.bytes, nyx.pascal.lexing,
  nyx.studio.compilerprocess
  {$ifdef windows}, Windows{$endif};

const
  CProbeFile: TNyxText = 'nyxcompilercontext.pas';

procedure CheckCancellation(const ACancellation: INyxCancellationScope);
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Native compiler context capture cancelled');
  end;
end;

procedure WriteFresh(const APath: TNyxText; const ABytes: TNyxBytes);
var
  LHandle: THandle;
  LStream: THandleStream;
begin
  {$ifdef windows}
  LHandle := CreateFileW(PWideChar(UTF8Decode(APath)), GENERIC_WRITE, 0,
    nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  {$else}
  LHandle := FileCreate(String(RawByteString(APath)));
  {$endif}

  if LHandle = THandle(-1) then
  begin
    raise ENyxPascalContext.Create('Could not create a fresh compiler profile file');
  end;
  LStream := nil;
  try
    LStream := THandleStream.Create(LHandle);

    if Length(ABytes) > 0 then
    begin
      LStream.WriteBuffer(ABytes[0], Length(ABytes));
    end;
  finally
    LStream.Free;
    FileClose(LHandle);
  end;
end;

function CaptureNyxFPCCompilerContext(const ACompiler, AFreshDirectory: TNyxText;
  const AProbe: INyxPascalCompilerProbe;
  const AOptions: TNyxPascalCompilerTraceOptions;
  const ACancellation: INyxCancellationScope;
  ATimeoutMilliseconds: Cardinal): INyxPascalCompilerContext;
var
  LProbe: INyxPascalCompilerProbe;
  LCancellation: INyxCancellationScope;
  LProcess: TNyxCompilerProcess;
  LBytes: TNyxBytes;
  LChunk: array[0..8191] of Byte;
  LSource: TNyxText;
  LDirectory: TNyxText;
  LSourcePath: TNyxText;
  LLog: TNyxText;
  LCount: Integer;
  LAt: Integer;
  LBefore: Integer;
  LCode: Integer;
  LQueryCount: Integer;
  LStart: QWord;
  LSourceHandle: THandle;

  procedure CheckDeadline;
  begin
    CheckCancellation(LCancellation);

    if GetTickCount64 - LStart >= ATimeoutMilliseconds then
    begin
      raise ENyxPascalContext.Create('The owned compiler context query exceeded its time budget');
    end;
  end;

  { Consume available bytes without waiting for a whole line or pipe EOF.
    Retirement owns the process family; closing a stream is not cancellation. }
  procedure Drain;
  begin
    while LProcess.Output.NumBytesAvailable > 0 do
    begin
      CheckDeadline;
      LCount := LProcess.Output.Read(LChunk, SizeOf(LChunk));

      if LCount <= 0 then
      begin
        Break;
      end;
      LBefore := Length(LBytes);

      if LCount > AOptions.MaximumBytes - LBefore then
      begin
        raise ENyxPascalContext.Create('Compiler profile output exceeds its byte budget');
      end;
      SetLength(LBytes, LBefore + LCount);
      Move(LChunk[0], LBytes[LBefore], LCount);
    end;
  end;

begin
  { Pin a caller's inline interfaced scope through retirement on every exit.
    A const parameter alone does not own that temporary on native FPC. }
  LCancellation := ACancellation;
  AOptions.Validate;
  CheckCancellation(LCancellation);

  if (AProbe = nil) or (ATimeoutMilliseconds = 0) or not FileExistsUTF8(ACompiler) or
    (Pos(#0, ACompiler) > 0) or (Pos(#0, AFreshDirectory) > 0) or
    (AFreshDirectory = '') or DirectoryExistsUTF8(AFreshDirectory) or FileExistsUTF8(AFreshDirectory) then
  begin
    raise ENyxPascalContext.Create('Compiler fact capture needs a compiler, bounded time and fresh private directory');
  end;
  LProbe := NewNyxFPCCompilerProbe(AProbe.Identity, AProbe.Mode);
  LQueryCount := AProbe.Count;

  if (LQueryCount < 0) or (LQueryCount > 256) then
  begin
    raise ENyxPascalContext.Create('Compiler probe queries exceed their bounded vector');
  end;
  for LAt := 0 to LQueryCount - 1 do
  begin
    LProbe := LProbe.WithSymbol(AProbe.Symbol(LAt));
  end;
  LSource := LProbe.Source;

  if (LProbe.Count <> LQueryCount) or (LSource <> AProbe.Source) then
  begin
    raise ENyxPascalContext.Create('An alternate compiler probe changed its checked source/query');
  end;
  LDirectory := IncludeTrailingPathDelimiter(ExpandFileNameUTF8(AFreshDirectory));

  if not CreateDirUTF8(LDirectory) then
  begin
    raise ENyxPascalContext.Create('The private compiler profile directory could not be created');
  end;
  LSourcePath := LDirectory + CProbeFile;
  WriteFresh(LSourcePath, NyxEncodeUTF8(LSource));
  {$ifdef windows}
  LSourceHandle := CreateFileW(PWideChar(UTF8Decode(LSourcePath)), GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  {$else}
  LSourceHandle := FileOpen(String(RawByteString(LSourcePath)), fmOpenRead or fmShareDenyWrite);
  {$endif}

  if LSourceHandle = THandle(-1) then
  begin
    raise ENyxPascalContext.Create('The exact compiler probe source could not be leased');
  end;
  LProcess := nil;
  try
    LProcess := TNyxCompilerProcess.Create(nil);
    LProcess.Executable := String(ACompiler);
    LProcess.CurrentDirectory := String(LDirectory);
    LProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    LProcess.Parameters.Add('-Mdelphi');
    LProcess.Parameters.Add('-s');
    LProcess.Parameters.Add('-Cn');
    LProcess.Parameters.Add('-v0');
    LProcess.Parameters.Add('-vc');
    LProcess.Parameters.Add('-vi');
    LProcess.Parameters.Add('-vn');
    LProcess.Parameters.Add('-vq');
    LProcess.Parameters.Add('-FU' + String(LDirectory));
    LProcess.Parameters.Add('-FE' + String(LDirectory));
    LProcess.Parameters.Add(String(CProbeFile));
    LStart := GetTickCount64;
    CheckCancellation(LCancellation);
    LProcess.Execute;
    while LProcess.FamilyRunning do
    begin
      Drain;
      CheckDeadline;
      Sleep(5);
    end;
    Drain;
    CheckDeadline;
    LProcess.WaitOnExit;
    LCode := LProcess.ExitStatus;
    LLog := NyxDecodeUTF8(LBytes);
    WriteFresh(LDirectory + 'compiler-profile.log', LBytes);
    Result := ReadNyxFPCCompilerContext(LProbe, CProbeFile, LLog, LCode,
      AOptions, LCancellation);
  finally

    if LProcess <> nil then
    begin
      try
        LProcess.RetireAndJoin;
      finally
        LProcess.Free;
      end;
    end;
    FileClose(LSourceHandle);
  end;
end;

end.
