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
program nyx_file_save_stream_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Forms, ExtCtrls, Windows, Classes, SysUtils,
  nyx.text, nyx.bytes, nyx.bytes.sources, nyx.files.save, nyx.files.save.lcl,
  nyx.scheduler;

type
  { Shared inert instrumentation outlives the writer and each copied cursor.
    Cancellation belongs to this test operation, never another process/dialog. }
  TProbe = class(TInterfacedObject)
  public
    Reads: Integer;
    Largest: Integer;
    CancelAfter: Integer;
    Scope: INyxCancellationScope;
  end;

  { Independent source wrapper deliberately changes only declared length in the
    failure scenario. The underlying exact text source remains immutable. }
  TObservedSource = class(TInterfacedObject, INyxByteSource)
  private
    FInner: INyxByteSource;
    FProbe: TProbe;
    FProbeLease: IInterface;
    FDelta: Integer;
  public
    constructor Create(const AInner: INyxByteSource; AProbe: TProbe; ADelta: Integer);
    function GetByteCount: Integer;
    function Open: INyxByteReader;
  end;

  { Reader instrumentation owns its probe and inner cursor without a source,
    callback, renderer or application reference. It can cancel during Read. }
  TObservedReader = class(TInterfacedObject, INyxByteReader)
  private
    FInner: INyxByteReader;
    FProbe: TProbe;
    FProbeLease: IInterface;
  public
    constructor Create(const AInner: INyxByteReader; AProbe: TProbe);
    function Read(AMaximumBytes: Integer): TNyxBytes;
  end;

  { Fixture-owned borrowed receiver and modal-dialog driver. It addresses only
    an exact titled window belonging to this process and a newly owned path. }
  TJourney = class
  public
    FileName: TNyxText;
    Replies: Integer;
    Result: TNyxFileSaveResult;
    Dialogs: Integer;
    Started: QWord;
    Turns: Integer;
    Scope: INyxCancellationScope;
    procedure Reply(const AResult: TNyxFileSaveResult);
    procedure DialogTurn(ASender: TObject);
  end;

var
  GDialog: HWND;
  GEdit: HWND;
  GOverwrite: HWND;
  GChecks: Integer;
  GDescribe: Boolean;

constructor TObservedSource.Create(const AInner: INyxByteSource; AProbe: TProbe; ADelta: Integer);
begin
  inherited Create;
  FInner := AInner;
  FProbe := AProbe;
  FProbeLease := AProbe;
  FDelta := ADelta;
end;

function TObservedSource.GetByteCount: Integer;
begin
  Result := FInner.ByteCount + FDelta;
end;

function TObservedSource.Open: INyxByteReader;
begin
  Result := TObservedReader.Create(FInner.Open, FProbe);
end;

constructor TObservedReader.Create(const AInner: INyxByteReader; AProbe: TProbe);
begin
  inherited Create;
  FInner := AInner;
  FProbe := AProbe;
  FProbeLease := AProbe;
end;

function TObservedReader.Read(AMaximumBytes: Integer): TNyxBytes;
begin
  Inc(FProbe.Reads);

  if AMaximumBytes > FProbe.Largest then
  begin
    FProbe.Largest := AMaximumBytes;
  end;
  Result := FInner.Read(AMaximumBytes);

  if (FProbe.CancelAfter > 0) and (FProbe.Reads >= FProbe.CancelAfter) then
  begin
    FProbe.Scope.Cancel;
  end;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create('Native streaming save: ' + AReason);
  end;
end;

function FindConfirmationButton(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LClass: array[0..63] of WideChar;
  LCaption: array[0..127] of WideChar;
  LID: Integer;
begin
  Result := True;
  GetClassNameW(AWindow, LClass, Length(LClass));

  if UnicodeString(LClass) <> 'Button' then
  begin
    Exit;
  end;
  GetWindowTextW(AWindow, LCaption, Length(LCaption));
  LID := GetDlgCtrlID(AWindow);

  if GDescribe then
  begin
    WriteLn('TRACE owned button / ', LID, ' / ', TNyxText(UTF8Encode(UnicodeString(LCaption))));
    Flush(Output);
  end;

  if IsWindowVisible(AWindow) and IsWindowEnabled(AWindow) and
    ((LID = IDYES) or (UnicodeString(LCaption) = '&Yes') or
     (UnicodeString(LCaption) = 'Yes') or (UnicodeString(LCaption) = '&Replace')) then
  begin
    GOverwrite := AWindow;
  end;
end;

function FindDialog(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LProcess: DWORD;
  LTitle: array[0..255] of WideChar;
begin
  Result := True;
  GetWindowThreadProcessId(AWindow, @LProcess);

  if LProcess <> GetCurrentProcessId then
  begin
    Exit;
  end;
  GetWindowTextW(AWindow, LTitle, Length(LTitle));

  if GDescribe then
  begin
    WriteLn('TRACE owned window / ', TNyxText(UTF8Encode(UnicodeString(LTitle))));
    Flush(Output);
  end;

  if UnicodeString(LTitle) = 'Stream a Nyx draft' then
  begin
    GDialog := AWindow;
  end;

  { Modern task dialogs can put buttons below a nested child rather than on
    the top-level dialog. Enumerate only descendants of this owned window. }
  EnumChildWindows(AWindow, @FindConfirmationButton, 0);
end;

function FindEdit(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LClass: array[0..63] of WideChar;
begin
  Result := True;
  GetClassNameW(AWindow, LClass, Length(LClass));

  if (UnicodeString(LClass) = 'Edit') and
    ((GetDlgCtrlID(AWindow) = 1001) or (GetDlgCtrlID(AWindow) = 1148)) then
  begin
    GEdit := AWindow;
    Result := False;
  end;
end;

procedure TJourney.DialogTurn(ASender: TObject);
begin
  GDialog := 0;
  GOverwrite := 0;
  Inc(Turns);
  GDescribe := (Turns = 1) or (Turns = 5) or (Turns = 20);
  EnumWindows(@FindDialog, 0);

  if GetTickCount64 - Started > 10000 then
  begin
    Scope.Cancel;
    Exit;
  end;

  if GOverwrite <> 0 then
  begin
    SendMessage(GOverwrite, BM_CLICK, 0, 0);
    Exit;
  end;

  if GDialog = 0 then
  begin
    Exit;
  end;

  GEdit := 0;
  EnumChildWindows(GDialog, @FindEdit, 0);

  if GEdit = 0 then
  begin
    Exit;
  end;
  Inc(Dialogs);
  SetWindowTextW(GEdit, PWideChar(UTF8Decode(FileName)));
  PostMessage(GDialog, WM_COMMAND, IDOK, 0);
end;

procedure TJourney.Reply(const AResult: TNyxFileSaveResult);
begin
  Inc(Replies);
  Result := AResult;
end;

procedure Run;
var
  LDirectory: TNyxText;
  LFile: TNyxText;
  LScheduler: INyxScheduler;
  LHost: INyxFileSaveHost;
  LStreamHost: INyxFileSaveStreamHost;
  LRequest: INyxFileSaveRequest;
  LSource: INyxByteSource;
  LObserved: INyxByteSource;
  LReader: INyxByteReader;
  LProbe: TProbe;
  LProbeLease: IInterface;
  LJourney: TJourney;
  LTimer: TTimer;
  LForm: TForm;
  LStream: TFileStream;
  LChunk: TNyxBytes;
  LActual: TNyxBytes;
  LExact: Boolean;
  LIndex: Integer;
  LCase: Integer;
  LStarted: QWord;
  LFind: TSearchRec;
  LFindResult: Integer;
begin
  LDirectory := TNyxText(ExpandFileName(ParamStr(1)));

  if DirectoryExists(String(LDirectory)) then
  begin
    raise Exception.Create('Native stream fixture requires a fresh owned output directory');
  end;
  ForceDirectories(String(LDirectory));
  LFile := LDirectory + TNyxText(PathDelim) + 'large-draft.txt';
  LScheduler := NewNyxScheduler;
  LHost := NewNyxLCLFileSaveHost(LDirectory);
  Check(Supports(LHost, INyxFileSaveStreamHost, LStreamHost), 'ordinary LCL host exposes optional streaming capability');
  LSource := NyxUTF8ByteSource(TNyxText(StringOfChar('x', 5 * 1024 * 1024)) +
    TNyxText('🌙') + TNyxText(#0));
  LJourney := TJourney.Create;
  LTimer := TTimer.Create(nil);
  LForm := TForm.Create(nil);
  try
    LForm.Show;
    Application.ProcessMessages;
    LJourney.FileName := LFile;
    LTimer.Interval := 20;
    LTimer.OnTimer := LJourney.DialogTurn;
    { The legacy byte-only path still owns/copies its small payload and keeps
      its existing receipt/quota behavior after adding the optional stream facet. }
    LJourney.Started := GetTickCount64;
    LJourney.Scope := NewNyxCancellationScope;
    LTimer.Enabled := True;
    LChunk := NyxEncodeUTF8(TNyxText('Small 🌙') + TNyxText(#0));
    LRequest := LHost.BeginSave(NyxFileSaveOptions('large-draft.txt')
      .Title('Stream a Nyx draft'), LChunk, LScheduler, LJourney.Scope, LJourney.Reply);
    LTimer.Enabled := False;
    Check((LJourney.Replies = 1) and (LJourney.Result.Outcome = nfsoWritten) and
      (LJourney.Result.ByteCount = Length(LChunk)), 'legacy direct native save retains exact delivery semantics');
    LStream := TFileStream.Create(String(LFile), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(LActual, Length(LChunk));
      LStream.ReadBuffer(LActual[0], Length(LActual));
      Check((LStream.Size = Length(LChunk)) and
        (NyxDecodeUTF8(LActual) = TNyxText('Small 🌙') + TNyxText(#0)),
        'legacy direct disk bytes remain exact before streamed replacement');
    finally
      LStream.Free;
    end;
    for LCase := 0 to 3 do
    begin
      { The same accepted file must survive cancelled and malformed replacement
        attempts. Never remove it or staging evidence before those comparisons. }
      LProbe := TProbe.Create;
      LProbeLease := LProbe;
      LProbe.Scope := NewNyxCancellationScope;

      if LCase = 1 then
      begin
        LProbe.CancelAfter := 3;
      end;
      LObserved := TObservedSource.Create(LSource, LProbe, -Ord(LCase = 2));
      LJourney.Replies := 0;
      LJourney.Dialogs := 0;
      LJourney.Started := GetTickCount64;
      LJourney.Turns := 0;
      LJourney.Scope := LProbe.Scope;
      WriteLn('TRACE native stream case ', LCase, ' opening owned dialog');
      Flush(Output);
      LTimer.Enabled := True;
      LRequest := LStreamHost.BeginStream(NyxFileSaveStreamOptions(
        NyxFileSaveOptions('large-draft.txt').Title('Stream a Nyx draft'))
        .MaximumBytes(8 * 1024 * 1024).ChunkBytes(65536),
        LObserved, LScheduler, LProbe.Scope, LJourney.Reply);
      LTimer.Enabled := False;
      WriteLn('TRACE native stream dialog returned; pumping bounded writes');
      Flush(Output);
      LStarted := GetTickCount64;

      if LCase = 3 then
      begin
        { No first chunk is eligible until a later cooperative turn. Shutdown
          must retire the armed work and release its private staging ownership. }
        LScheduler.Shutdown;
      end
      else
      begin
        repeat
          Application.ProcessMessages;
          CheckSynchronize(1);
          Sleep(1);

          if GetTickCount64 - LStarted > 15000 then
          begin
            raise Exception.Create('Cooperative native stream completion exceeded its test deadline');
          end;
        until (LJourney.Replies > 0) or LProbe.Scope.Cancelled;
      end;

      if LCase = 3 then
      begin
        Check((LProbe.Reads = 0) and (LJourney.Replies = 0),
          'scheduler shutdown retires pending stream work before entering its reader or reply');
        LFindResult := FindFirst(String(LDirectory + TNyxText(PathDelim) + '.nyx-*.tmp'), faAnyFile, LFind);

        if LFindResult = 0 then
        begin
          FindClose(LFind);
        end;
        Check(LFindResult <> 0, 'skipped-work retirement removes its stage before an explicit request Cancel');
      end
      else
      begin
        Check((LProbe.Reads > 1) and (LProbe.Largest <= 65536),
          'real native save uses bounded cooperative reads across host turns');
      end;
      LRequest.Cancel;
      Application.ProcessMessages;

      if LCase = 0 then
      begin
        Check((LJourney.Replies = 1) and (LJourney.Result.Outcome = nfsoWritten) and
          (LJourney.Result.ByteCount = LSource.ByteCount), 'completed native stage reports exact written count');
      end
      else if LCase = 1 then
      begin
        Check(LJourney.Replies = 0, 'cancellation inside a source read detaches the borrowed receiver');
      end
      else if LCase = 2 then
      begin
        Check((LJourney.Replies = 1) and (LJourney.Result.Outcome = nfsoFailed),
          'excess source bytes fail before replacing accepted data');
      end;
      LStream := TFileStream.Create(String(LFile), fmOpenRead or fmShareDenyWrite);
      try
        Check(LStream.Size = LSource.ByteCount, 'accepted disk file retains the complete source size');
        LReader := LSource.Open;
        LExact := True;
        repeat
          LChunk := LReader.Read(65536);
          SetLength(LActual, Length(LChunk));

          if Length(LChunk) > 0 then
          begin
            LStream.ReadBuffer(LActual[0], Length(LActual));
          end;
          for LIndex := 0 to High(LChunk) do
          begin
            LExact := LExact and (LChunk[LIndex] = LActual[LIndex]);
          end;
        until Length(LChunk) = 0;
        Check(LExact, 'actual disk bytes retain exact UTF-8/NUL after success, cancellation and failed replacement');
      finally
        LStream.Free;
      end;
      LFindResult := FindFirst(String(LDirectory + TNyxText(PathDelim) + '.nyx-*.tmp'), faAnyFile, LFind);

      if LFindResult = 0 then
      begin
        FindClose(LFind);
      end;
      Check(LFindResult <> 0,
        'request retirement leaves no owned staging file behind');
    end;
    Check(LJourney.Dialogs > 0, 'only this process owned and drove each actual native save dialog');
  finally

    if LRequest <> nil then
    begin
      LRequest.Cancel;
    end;
    LScheduler.Shutdown;
    LTimer.Free;
    LJourney.Free;
    LForm.Free;
  end;
end;

begin
  try
    Application.Initialize;
    Application.CaptureExceptions := False;
    Run;
    WriteLn('PASS ', GChecks, ' actual native streamed file checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
