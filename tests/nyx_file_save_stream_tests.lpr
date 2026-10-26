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
program nyx_file_save_stream_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.bytes.sources, nyx.files,
  nyx.files.save, nyx.scheduler
  {$ifdef PAS2JS}, Web{$endif};

type
  { Portable test exposes only the shared request's protected chunk/terminal
    boundary. It owns no dialog, file path, renderer or OS write authority. }
  TRequest = class(TNyxFileSaveRequest)
  public
    function Next: TNyxBytes;
    procedure Finish;
  end;

  { Borrowed receiver outlives every test request. Its scalar receipt is useful
    after completion without retaining the source/reader or request. }
  TReply = class
  public
    Calls: Integer;
    Last: TNyxFileSaveResult;
    procedure Receive(const AResult: TNyxFileSaveResult);
  end;

  { Intentionally invalid source/cursor fixture. Each scenario opens once and
    reports controlled early EOF, excess bytes, oversize chunks or cancellation.
    It is deliberately not a reusable independent-cursor implementation. }
  TBadSource = class(TInterfacedObject, INyxByteSource, INyxByteReader)
  public
    Declared: Integer;
    Chunk: Integer;
    Opens: Integer;
    Scope: INyxCancellationScope;
    function GetByteCount: Integer;
    function Open: INyxByteReader;
    function Read(AMaximumBytes: Integer): TNyxBytes;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create('Streaming save: ' + AReason);
  end;
end;

function TRequest.Next: TNyxBytes;
begin
  Result := ReadPayloadChunk;
end;

procedure TRequest.Finish;
begin
  Complete(nfsoWritten, FOptions.SuggestedName, FExpectedBytes);
end;

procedure TReply.Receive(const AResult: TNyxFileSaveResult);
begin
  Inc(Calls);
  Last := AResult;
end;

function TBadSource.GetByteCount: Integer;
begin
  Result := Declared;
end;

function TBadSource.Open: INyxByteReader;
begin
  Inc(Opens);
  Result := Self;
end;

function TBadSource.Read(AMaximumBytes: Integer): TNyxBytes;
begin
  Result := nil;

  if Scope <> nil then
  begin
    Scope.Cancel;
  end;
  SetLength(Result, Chunk);
end;

procedure Run;
const
  CText: TNyxText = 'A🌙' + #0 + #13#10 + 'Z';
var
  LSource: INyxByteSource;
  LReader: INyxByteReader;
  LOther: INyxByteReader;
  LChunk: TNyxBytes;
  LExpected: TNyxBytes;
  LJoined: TNyxBytes;
  LIndex: Integer;
  LPosition: Integer;
  LSize: Integer;
  LOptions: TNyxFileSaveStreamOptions;
  LScheduler: INyxScheduler;
  LRequest: TRequest;
  LLease: INyxFileSaveRequest;
  LReply: TReply;
  LRefused: Boolean;
  LBad: TBadSource;
  LBadLease: INyxByteSource;
  LScope: INyxCancellationScope;
  LLarge: TNyxText;
  LCount: Integer;
begin
  LSource := NyxUTF8ByteSource(CText);
  LExpected := NyxEncodeUTF8(CText);
  Check(LSource.ByteCount = Length(LExpected), 'known length counts exact UTF-8 and NUL');
  for LSize := 1 to 7 do
  begin
    LReader := LSource.Open;
    LOther := LSource.Open;
    LPosition := 0;
    SetLength(LJoined, LSource.ByteCount);
    repeat
      LChunk := LReader.Read(LSize);
      for LIndex := 0 to High(LChunk) do
      begin
        LJoined[LPosition] := LChunk[LIndex];
        Inc(LPosition);
      end;
    until Length(LChunk) = 0;
    Check((LPosition = Length(LExpected)) and (NyxDecodeUTF8(LJoined) = CText),
      'all chunk boundaries preserve supplementary scalars, NUL and line endings');
    LChunk := LOther.Read(1);
    Check((Length(LChunk) = 1) and (LChunk[0] = LExpected[0]),
      'opening and consuming a peer never advances another cursor');
  end;
  LScheduler := NewNyxScheduler;
  LReply := TReply.Create;
  try
    LOptions := NyxFileSaveStreamOptions(NyxFileSaveOptions('unfinished.txt'))
      .MaximumBytes(8 * 1024 * 1024).ChunkBytes(65536).DownloadBufferBytes(0);
    LLarge := TNyxText(StringOfChar('x', 5 * 1024 * 1024)) + TNyxText('🌙') + TNyxText(#0);
    LSource := NyxUTF8ByteSource(LLarge);
    LRequest := TRequest.CreateStream(LOptions, LSource, LScheduler, nil, LReply.Receive);
    LLease := LRequest;
    LRefused := False;
    try
      LRequest.Finish;
    except
      on E: ENyxFile do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LReply.Calls = 0), 'delivery refuses before verified EOF, even with declared length');
    LCount := 0;
    repeat
      LChunk := LRequest.Next;
      Check(Length(LChunk) <= LOptions.ChunkLimit, 'actual large-payload read stays within the chunk budget');
      Inc(LCount, Length(LChunk));
    until Length(LChunk) = 0;
    LRequest.Finish;
    Check((LCount = 5 * 1024 * 1024 + 5) and (LReply.Calls = 1) and
      (LReply.Last.ByteCount = LCount), 'more than 4 MiB completes through bounded chunks and an exact receipt');
    LRequest.Finish;
    Check(LReply.Calls = 1, 'terminal completion cannot invoke its borrowed receiver again');
    LLease := nil;
    for LIndex := 0 to 3 do
    begin
      LBad := TBadSource.Create;
      LBadLease := LBad;
      LBad.Declared := 1;
      LBad.Chunk := LIndex;
      LScope := NewNyxCancellationScope;

      if LIndex = 3 then
      begin
        LBad.Scope := LScope;
      end;
      LRequest := TRequest.CreateStream(LOptions.ChunkBytes(1), LBadLease,
        LScheduler, LScope, LReply.Receive);
      LLease := LRequest;
      LRefused := False;
      try
        LChunk := LRequest.Next;

        if LIndex = 1 then
        begin
          LChunk := LRequest.Next;
        end;
      except
        on E: ENyxFile do
        begin
          LRefused := True;
        end;
      end;

      if LIndex = 3 then
      begin
        LRequest.Finish;
        Check(not LRefused and (Length(LChunk) = 0) and (LReply.Calls = 1),
          'reader cancellation suppresses data and late completion before any host effect');
      end
      else
      begin
        Check(LRefused and (LReply.Calls = 1), 'early EOF, extra bytes and oversize chunks refuse without delivery');
      end;
      LLease.Cancel;
      LLease := nil;
    end;
    LBad := TBadSource.Create;
    LBadLease := LBad;
    LBad.Declared := 2;
    LRefused := False;
    try
      LRequest := TRequest.CreateStream(LOptions.MaximumBytes(1), LBadLease,
        LScheduler, nil, LReply.Receive);
      LLease := LRequest;
    except
      on E: ENyxFile do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LBad.Opens = 0), 'total-byte refusal precedes opening the source');
    LRefused := False;
    try
      LOptions := LOptions.ChunkBytes(0);
    except
      on E: ENyxFile do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'invalid chunk policy refuses before changing its caller value');
  finally

    if LLease <> nil then
    begin
      LLease.Cancel;
    end;
    LScheduler.Shutdown;
    LReply.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-stream-save-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' streaming source/request checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-stream-save-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
