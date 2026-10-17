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

unit nyx.studio.completion.native;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.pascal.completion;

type
  { Machine-only helper configuration. Paths belong to the host and never enter
    documents or semantic replies. Values are copied into a provider; callers
    explicitly choose the executable and its trusted SDK/compiler sources. }
  TNyxPascalCompletionHost = record
  private
    FExecutable: String;
    FSDK: String;
    FCompiler: String;
    FFPCSource: String;
    FLazarus: String;
    FScratch: String;
    FDeadlineMS: Integer;
  public
    { The optional worker is an independent executable, not a Studio dependency.
      Scratch receives fresh query directories, retained for host-managed cleanup. }
    class function Using(const AExecutable, ASDK, ACompiler, AFPCSource,
      ALazarus, AScratch: String): TNyxPascalCompletionHost; static;
    { One query gets 30 seconds by default; callers may select 1..120000 ms.
      Expiry retires and joins its exact process family before returning failure. }
    function WithinMilliseconds(AValue: Integer): TNyxPascalCompletionHost;
  end;

{ Each call freezes its query and owns one independent helper process/directory.
  Calls are synchronous worker operations, never UI callbacks. Windows family
  ownership uses the same compiler-process adapter as ordinary builds; other
  native platforms still require their own family/lifetime qualification.
  No provider is installed by this factory. Missing tools return unavailable. }
function NewNyxNativePascalCompletionProvider(
  const AHost: TNyxPascalCompletionHost): INyxPascalCompletionProvider;

implementation

uses SysUtils, Classes, Process, nyx.text, nyx.bytes,
  nyx.pascal.project, nyx.pascal.completion.wire, nyx.studio.compilerprocess;

type
  { Copied private host settings have no editor/session reference. Every invocation
    owns its pipes and exact child family until join, including refusal paths. }
  TNativeCompletionProvider = class(TInterfacedObject, INyxPascalCompletionProvider)
  private
    FHost: TNyxPascalCompletionHost;
  public
    function Complete(const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionResult;
  end;

class function TNyxPascalCompletionHost.Using(const AExecutable, ASDK,
  ACompiler, AFPCSource, ALazarus, AScratch: String): TNyxPascalCompletionHost;
begin
  Result.FExecutable := AExecutable;
  Result.FSDK := ASDK;
  Result.FCompiler := ACompiler;
  Result.FFPCSource := AFPCSource;
  Result.FLazarus := ALazarus;
  Result.FScratch := AScratch;
  Result.FDeadlineMS := 30000;
end;

function TNyxPascalCompletionHost.WithinMilliseconds(
  AValue: Integer): TNyxPascalCompletionHost;
begin

  if (AValue < 1) or (AValue > 120000) then
  begin
    raise ENyxPascalProject.Create('Completion deadline must be between 1 and 120000 milliseconds');
  end;
  Result := Self;
  Result.FDeadlineMS := AValue;
end;

procedure WritePacket(const APath: String; const AText: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
end;

function ReadReply(const APath: String): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if LStream.Size > 1024 * 1024 then
    begin
      raise ENyxPascalProject.Create('Completion reply exceeds its byte budget');
    end;
    SetLength(LBytes, LStream.Size);

    if Length(LBytes) > 0 then
    begin
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

function TNativeCompletionProvider.Complete(
  const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionResult;
var
  LRequest: INyxPascalCompletionRequest;
  LProcess: TNyxCompilerProcess;
  LID: TGUID;
  LIdentifier: String;
  LDirectory: String;
  LRequestFile: String;
  LReplyFile: String;
  LLog: TFileStream;
  LBuffer: array[0..8191] of Byte;
  LRead: Integer;
  LStarted: QWord;

  function Failure(const AMessage: TNyxText): INyxPascalCompletionResult;
  begin
    Result := NewNyxPascalCompletionResult(LRequest, pcsFailed,
      AMessage, 0, '', [], 0);
  end;

  function InTime: Boolean;
  begin
    Result := GetTickCount64 - LStarted < QWord(FHost.FDeadlineMS);
  end;
begin
  LRequest := CaptureNyxPascalCompletionRequest(ARequest);

  if (FHost.FExecutable = '') or not FileExists(FHost.FExecutable) or
    not FileExists(FHost.FCompiler) or not DirectoryExists(FHost.FSDK) or
    not DirectoryExists(FHost.FFPCSource) or not DirectoryExists(FHost.FLazarus) or
    (FHost.FScratch = '') then
  begin
    Exit(NewNyxPascalCompletionResult(LRequest, pcsUnavailable,
      'The optional Pascal assistance host is not configured', 0, '', [], 0));
  end;
  CreateGUID(LID);
  LIdentifier := GUIDToString(LID);
  LDirectory := IncludeTrailingPathDelimiter(ExpandFileName(FHost.FScratch)) +
    'completion-' + LIdentifier;
  LProcess := nil;
  LLog := nil;
  try
    try

      if (FHost.FDeadlineMS < 1) or (FHost.FDeadlineMS > 120000) or
        DirectoryExists(LDirectory) or not ForceDirectories(LDirectory) then
      begin
        raise ENyxPascalProject.Create('Completion host directory or deadline is invalid');
      end;
      LRequestFile := IncludeTrailingPathDelimiter(LDirectory) + 'request.json';
      LReplyFile := IncludeTrailingPathDelimiter(LDirectory) + 'reply.json';
      WritePacket(LRequestFile, EncodeNyxCompletionRequest(LRequest, TNyxText(LIdentifier)));
      LLog := TFileStream.Create(IncludeTrailingPathDelimiter(LDirectory) + 'worker.log', fmCreate);
      LProcess := TNyxCompilerProcess.Create(nil);
      LProcess.Executable := ExpandFileName(FHost.FExecutable);
      LProcess.CurrentDirectory := LDirectory;
      LProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
      LProcess.Parameters.Add(LRequestFile);
      LProcess.Parameters.Add(LReplyFile);
      LProcess.Parameters.Add(LDirectory);
      LProcess.Parameters.Add(ExpandFileName(FHost.FSDK));
      LProcess.Parameters.Add(ExpandFileName(FHost.FCompiler));
      LProcess.Parameters.Add(ExpandFileName(FHost.FFPCSource));
      LProcess.Parameters.Add(ExpandFileName(FHost.FLazarus));
      LStarted := GetTickCount64;
      LProcess.Execute;
      repeat
        while LProcess.Output.NumBytesAvailable > 0 do
        begin

          if not InTime then
          begin
            Exit(Failure('The assistance query exceeded its time budget'));
          end;
          LRead := LProcess.Output.Read(LBuffer, SizeOf(LBuffer));

          if LRead > 0 then
          begin

            if LLog.Size + LRead > 1024 * 1024 then
            begin
              Exit(Failure('The assistance worker exceeded its diagnostic budget'));
            end;
            LLog.WriteBuffer(LBuffer[0], LRead);
          end;
        end;

        if not InTime then
        begin
          Exit(Failure('The assistance query exceeded its time budget'));
        end;

        if not LProcess.Running and (LProcess.ExitStatus <> 0) then
        begin
          Exit(Failure('The assistance worker could not complete the query'));
        end;

        if LProcess.FamilyRunning then
        begin
          Sleep(5);
        end;
      until not LProcess.FamilyRunning and (LProcess.Output.NumBytesAvailable = 0);
      { Join precedes response admission. A child that outlives the worker never
        transfers authority through a merely present reply file. }
      LProcess.RetireAndJoin;
      Result := DecodeNyxCompletionReply(ReadReply(LReplyFile),
        TNyxText(LIdentifier), LRequest);
    except
      on E: Exception do
      begin
        { Keep host paths/platform details in the private worker directory.
          Finally must still confirm retirement before this result can escape. }
        Result := Failure('The assistance host could not produce a valid bounded reply');
      end;
    end;
  finally
    try

      if LProcess <> nil then
      begin
        LProcess.RetireAndJoin;
      end;
    finally
      LProcess.Free;
      LLog.Free;
    end;
  end;
end;

function NewNyxNativePascalCompletionProvider(
  const AHost: TNyxPascalCompletionHost): INyxPascalCompletionProvider;
var
  LProvider: TNativeCompletionProvider;
begin
  LProvider := TNativeCompletionProvider.Create;
  Result := LProvider;
  LProvider.FHost := AHost;
end;

end.
