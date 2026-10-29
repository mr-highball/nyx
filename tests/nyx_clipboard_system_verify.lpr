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

program nyx_clipboard_system_verify;

{$mode delphi}{$H+}{$codepage utf8}
{$ifndef MSWINDOWS}{$fatal This qualifier reads the Windows system clipboard}{$endif}

uses Classes, SysUtils, Windows, nyx.text, nyx.bytes;

{ The caller supplies the known value of its own completed Copy operation.
  Never print clipboard content or write an expected value to the clipboard.
  This qualifies the interoperable UTF-16 channel, not private custom formats. }
function ExpectedText(const APath: String): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if LStream.Size > 4 * 1024 * 1024 then
    begin
      raise Exception.Create('Expected clipboard fixture exceeds 4 MiB');
    end;
    SetLength(LBytes, LStream.Size);

    if Length(LBytes) > 0 then
    begin
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);

    if Pos(#0, Result) > 0 then
    begin
      raise Exception.Create('NUL values require the separate custom-format qualifier');
    end;
  finally
    LStream.Free;
  end;
end;

{ Windows owns the borrowed global-memory handle. Hold its clipboard lock while
  reading at most the expected UTF-16 units plus their terminator. A mismatch
  exposes no unrelated content and never scans an arbitrary external string. }
function MatchesSystemClipboard(const AExpected: TNyxText): Boolean;
var
  LHandle: THandle;
  LText: PWideChar;
  LIndex: Integer;
begin
  Result := False;

  if not OpenClipboard(0) then
  begin
    raise Exception.Create('The system clipboard is currently unavailable');
  end;
  try
    LHandle := GetClipboardData(CF_UNICODETEXT);

    if (LHandle = 0) or
      (GlobalSize(LHandle) < (PtrUInt(Length(AExpected)) + 1) * SizeOf(WideChar)) then
    begin
      Exit;
    end;
    LText := PWideChar(GlobalLock(LHandle));

    if LText = nil then
    begin
      Exit;
    end;
    try
      for LIndex := 1 to Length(AExpected) do
      begin

        if LText[LIndex - 1] <> AExpected[LIndex] then
        begin
          Exit;
        end;
      end;
      Result := LText[Length(AExpected)] = #0;
    finally
      GlobalUnlock(LHandle);
    end;
  finally
    CloseClipboard;
  end;
end;

var
  LExpected: TNyxText;
begin
  try

    if ParamCount <> 1 then
    begin
      raise Exception.Create('Supply the expected UTF-8 fixture file');
    end;
    LExpected := ExpectedText(ParamStr(1));

    if not MatchesSystemClipboard(LExpected) then
    begin
      raise Exception.Create('System clipboard does not match the expected value');
    end;
    WriteLn('PASS system clipboard exactly matches ', Length(LExpected), ' expected UTF-16 units');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.Message);
      ExitCode := 1;
    end;
  end;
end.
