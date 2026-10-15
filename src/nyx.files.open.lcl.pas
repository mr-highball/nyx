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

unit nyx.files.open.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.files.open;

{ Runtime initial directory is a private host hint, never saved operation data.
  Dialogs stay on UI; bounded reads use scheduler workers and pure result channels.
  Win32 dialog cancellation is qualified separately from other LCL widgetsets. }
function NewNyxLCLFileOpenHost(const AInitialDirectory: TNyxText = ''): INyxFileOpenHost;

implementation

uses Classes, SysUtils, SyncObjs, Dialogs, ExtCtrls, LazFileUtils, nyx.bytes,
  nyx.files, nyx.scheduler
  {$ifdef windows}, Windows{$endif};

type
  { Thread-safe pure result channel. It retains no request, widget, callback,
    scheduler or document. Worker and UI independently retain this channel. }
  IReadChannel = interface(IInterface)
    ['{60CAFF21-C6E4-43D0-8855-940000000013}']
    procedure Publish(const AFile: INyxFileSnapshot; const AError: TNyxText);
    function Capture(out AFile: INyxFileSnapshot; out AError: TNyxText): Boolean;
  end;

  { A lock protects one immutable terminal frame. Its destruction is safe on
    either thread because its only contents are owned data and synchronization. }
  TReadChannel = class(TInterfacedObject, IReadChannel)
  private
    FLock: SyncObjs.TCriticalSection;
    FComplete: Boolean;
    FFile: INyxFileSnapshot;
    FError: TNyxText;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Publish(const AFile: INyxFileSnapshot; const AError: TNyxText);
    function Capture(out AFile: INyxFileSnapshot; out AError: TNyxText): Boolean;
  end;

  { Worker owns a private selected path, options, scope and pure output channel.
    It never touches a request or UI object. Cancellation is cooperative between
    bounded chunks; an underlying filesystem call may still finish later. }
  TReadWork = class(TInterfacedObject, INyxWork)
  public
    Path: TNyxText;
    Options: TNyxFileOpenOptions;
    Scope: INyxCancellationScope;
    Channel: IReadChannel;
    procedure Execute(const AExecution: INyxExecution);
  end;

  { UI-only selector/observer. No worker retains this object. The scoped dialog
    lease covers modal reentry; shared delayed observations retire on shutdown.
    Worker results are captured only on UI and sealed by the portable base. }
  TOpenRequest = class(TNyxFileOpenRequest)
  private
    FDirectory: TNyxText;
    FDialog: TOpenDialog;
    FModalTimer: TTimer;
    FChannel: IReadChannel;
    FWork: INyxExecution;
    {$ifdef windows}FDialogWindow: HWND;{$endif}
    procedure ObserveModal(ASender: TObject);
  protected
    procedure EndHost; override;
    procedure Observe; override;
  public
    destructor Destroy; override;
    procedure Start;
  end;

  { Reference-counted immutable runtime adapter. Every BeginOpen creates a fresh
    dialog/channel/worker. No active selection or machine path is shared between
    requests or reconstructed from portable operation configuration. }
  TOpenHost = class(TInterfacedObject, INyxFileOpenHost)
  public
    Directory: TNyxText;
    function BeginOpen(const AOptions: TNyxFileOpenOptions;
      const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
      AReply: TNyxFileOpenReply): INyxFileOpenRequest;
  end;

{$ifdef windows}
const
  COpenRequestProperty: PWideChar = 'Nyx.FileOpen.Request';

var
  { UI-thread stack head exists only during Execute. Modal/reentrant operations
    restore the preceding owner before leaving their own scoped hook. }
  GOpeningRequest: TOpenRequest;

function OpenDialogHook(ACode: Integer; AWindow: WPARAM; AData: LPARAM): LRESULT; stdcall;
var
  LClass: array[0..63] of WideChar;
  LTitle: array[0..1024] of WideChar;
begin

  if (ACode = HCBT_ACTIVATE) and (GOpeningRequest <> nil) and
    (GOpeningRequest.FDialogWindow = 0) then
  begin
    GetClassNameW(HWND(AWindow), LClass, Length(LClass));
    GetWindowTextW(HWND(AWindow), LTitle, Length(LTitle));

    if (UnicodeString(LClass) = '#32770') and
      (UnicodeString(LTitle) = UTF8Decode(GOpeningRequest.FOptions.DialogTitle)) then
    begin
      GOpeningRequest.FDialogWindow := HWND(AWindow);
      SetPropW(HWND(AWindow), COpenRequestProperty, THandle(PtrUInt(GOpeningRequest)));

      if not GOpeningRequest.Pending then
      begin
        GOpeningRequest.EndHost;
      end;
    end;
  end;
  Result := CallNextHookEx(0, ACode, AWindow, AData);
end;
{$endif}

constructor TReadChannel.Create;
begin
  inherited Create;
  FLock := SyncObjs.TCriticalSection.Create;
end;

destructor TReadChannel.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TReadChannel.Publish(const AFile: INyxFileSnapshot; const AError: TNyxText);
begin
  FLock.Enter;
  try

    if not FComplete then
    begin
      FFile := AFile;
      FError := AError;
      FComplete := True;
    end;
  finally
    FLock.Leave;
  end;
end;

function TReadChannel.Capture(out AFile: INyxFileSnapshot; out AError: TNyxText): Boolean;
begin
  FLock.Enter;
  try
    Result := FComplete;
    AFile := FFile;
    AError := FError;
  finally
    FLock.Leave;
  end;
end;

function LeafName(const APath: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  { Delimiters are ASCII; preserve every UTF-8 byte without an ANSI RTL path
    conversion. The result still passes ordinary portable leaf validation. }
  for LIndex := Length(APath) downto 1 do
  begin

    if (APath[LIndex] = '/') or (APath[LIndex] = '\') then
    begin
      Exit(Copy(APath, LIndex + 1, MaxInt));
    end;
  end;
  Result := APath;
end;

procedure TReadWork.Execute(const AExecution: INyxExecution);
var
  LHandle: THandle;
  LStream: THandleStream;
  LBytes: TNyxBytes;
  LSize: Int64;
  LOffset: Integer;
  LCount: Integer;
  LExtra: Byte;
  LMetadata: TNyxFileMetadata;
  LFile: INyxFileSnapshot;
  {$ifdef windows}
  LModified: TFileTime;
  LRaw: QWord;
  LMilliseconds: Int64;
  {$endif}

  function Cancelled: Boolean;
  begin
    Result := AExecution.Cancelled or ((Scope <> nil) and Scope.Cancelled);
  end;

begin

  if Cancelled then
  begin
    Exit;
  end;
  LHandle := THandle(-1);
  LStream := nil;
  try
    try
      {$ifdef windows}
      LHandle := CreateFileW(PWideChar(UTF8Decode(Path)), GENERIC_READ, FILE_SHARE_READ,
        nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      {$else}
      LHandle := FileOpenUTF8(String(RawByteString(Path)), fmOpenRead or fmShareDenyWrite);
      {$endif}

      if LHandle = THandle(-1) then
      begin
        raise ENyxFile.Create('The selected file is not available for reading');
      end;
      LStream := THandleStream.Create(LHandle);
      LSize := LStream.Size;

      if (LSize < 0) or (LSize > Options.ByteLimit) then
      begin
        raise ENyxFile.Create('Selected file exceeds the byte budget');
      end;
      SetLength(LBytes, Integer(LSize));
      LOffset := 0;
      while LOffset < Length(LBytes) do
      begin

        if Cancelled then
        begin
          Exit;
        end;
        LCount := Length(LBytes) - LOffset;

        if LCount > 65536 then
        begin
          LCount := 65536;
        end;
        LCount := LStream.Read(LBytes[LOffset], LCount);

        if LCount <= 0 then
        begin
          raise ENyxFile.Create('Selected file changed during its read');
        end;
        Inc(LOffset, LCount);
      end;

      if (LStream.Read(LExtra, 1) <> 0) or (LStream.Size <> LSize) then
      begin
        raise ENyxFile.Create('Selected file changed during its read');
      end;
      LMetadata := NyxFileMetadata(LeafName(Path), Length(LBytes));
      {$ifdef windows}

      if GetFileTime(LHandle, nil, nil, @LModified) then
      begin
        LRaw := (QWord(LModified.dwHighDateTime) shl 32) or LModified.dwLowDateTime;

        if LRaw <= QWord(High(Int64)) then
        begin

          if LRaw >= 116444736000000000 then
          begin
            LMilliseconds := Int64((LRaw - 116444736000000000) div 10000);
          end
          else
          begin
            LMilliseconds := -Int64((116444736000000000 - LRaw + 9999) div 10000);
          end;
          LMetadata := LMetadata.ModifiedMilliseconds(LMilliseconds);
        end;
      end;
      {$endif}
      LFile := NyxFileSnapshot(LMetadata, LBytes);
    finally
      LStream.Free;

      if LHandle <> THandle(-1) then
      begin
        FileClose(LHandle);
      end;
    end;

    if not Cancelled then
    begin
      Channel.Publish(LFile, '');
    end;
  except
    on E: ENyxFile do
    begin
      Channel.Publish(nil, TNyxText(E.Message));
    end;
    else
    begin
      Channel.Publish(nil, 'The selected file could not be read completely');
    end;
  end;
end;

procedure TOpenRequest.EndHost;
begin

  if FModalTimer <> nil then
  begin
    FModalTimer.Enabled := False;
  end;

  if FWork <> nil then
  begin
    FWork.Cancel;
    FWork := nil;
  end;
  FChannel := nil;
  {$ifdef windows}

  if (FDialogWindow <> 0) and
    (GetPropW(FDialogWindow, COpenRequestProperty) = THandle(PtrUInt(Self))) then
  begin
    PostMessageW(FDialogWindow, WM_COMMAND, IDCANCEL, 0);
  end;
  {$else}

  if (FDialog <> nil) and FDialog.HandleAllocated then
  begin
    FDialog.Close;
  end;
  {$endif}
end;

destructor TOpenRequest.Destroy;
begin
  FModalTimer.Free;
  FDialog.Free;
  inherited Destroy;
end;

procedure TOpenRequest.ObserveModal(ASender: TObject);
var
  LMonitor: INyxSchedulerMonitor;
begin
  { Native common dialogs run their own message pump. Their ordinary widgetset
    timer also observes retirement when a queued RTL UI turn cannot run yet. }

  if not Pending then
  begin
    Cancel;
  end
  else if Supports(FScheduler, INyxSchedulerMonitor, LMonitor) and
    LMonitor.WorkerLoad.Closed then
  begin
    Cancel;
  end;
end;

procedure TOpenRequest.Observe;
var
  LFile: INyxFileSnapshot;
  LError: TNyxText;
  LStatus: TNyxExecutionStatus;
begin

  if FWork <> nil then
  begin

    if FWork.Cancelled then
    begin
      Cancel;
      Exit;
    end;

    { Capture terminal status before the channel. A worker may publish between
      these reads; observing status afterward could falsely reject that result. }
    LStatus := FWork.Status;

    if (FChannel <> nil) and FChannel.Capture(LFile, LError) then
    begin

      if LError <> '' then
      begin
        Complete(nfooFailed, nil, LError);
      end
      else
      begin
        Complete(nfooRead, LFile);
      end;
      Exit;
    end;

    if LStatus in [nesFailed, nesSucceeded] then
    begin
      Complete(nfooFailed, nil, 'The background file reader did not return a complete result');
      Exit;
    end;
  end;
  inherited Observe;
end;

procedure TOpenRequest.Start;
var
  LLease: INyxFileOpenRequest;
  LSelected: Boolean;
  LPath: TNyxText;
  LFilter: TNyxText;
  LIndex: Integer;
  LWork: TReadWork;
  LWorkLease: INyxWork;
  {$ifdef windows}
  LPrevious: TOpenRequest;
  LHook: HHOOK;
  {$endif}
begin
  FScheduler.RequireUI;
  LLease := Self;
  RetainPending;
  try

    if not Pending then
    begin
      Cancel;
      Exit;
    end;

    if not (nscWorkerThreads in FScheduler.Capabilities) then
    begin
      Complete(nfooUnavailable, nil, 'This native file reader requires scheduler workers');
      Exit;
    end;
    Watch;

    if not Pending then
    begin
      Exit;
    end;
    FDialog := TOpenDialog.Create(nil);
    try
      FModalTimer := TTimer.Create(nil);
      FModalTimer.Interval := 25;
      FModalTimer.OnTimer := ObserveModal;
      FDialog.Title := String(RawByteString(FOptions.DialogTitle));
      FDialog.InitialDir := String(RawByteString(FDirectory));
      FDialog.Options := [ofFileMustExist, ofPathMustExist, ofEnableSizing];
      LFilter := '';
      for LIndex := 0 to FOptions.ExtensionCount - 1 do
      begin

        if LIndex > 0 then
        begin
          LFilter := LFilter + ';';
        end;
        LFilter := LFilter + '*.' + FOptions.Extension(LIndex);
      end;

      if LFilter <> '' then
      begin
        FDialog.Filter := String(RawByteString('Suggested file types|' + LFilter + '|All files|*'));
      end
      else
      begin
        FDialog.Filter := 'All files|*';
      end;
      {$ifdef windows}
      LPrevious := GOpeningRequest;
      LHook := SetWindowsHookExW(WH_CBT, @OpenDialogHook, 0, GetCurrentThreadId);

      if LHook = 0 then
      begin
        raise ENyxFile.Create('Cannot establish open dialog cancellation ownership');
      end;
      GOpeningRequest := Self;
      try
        LSelected := FDialog.Execute;
      finally

        if (FDialogWindow <> 0) and
          (GetPropW(FDialogWindow, COpenRequestProperty) = THandle(PtrUInt(Self))) then
        begin
          RemovePropW(FDialogWindow, COpenRequestProperty);
        end;
        FDialogWindow := 0;
        GOpeningRequest := LPrevious;
        UnhookWindowsHookEx(LHook);
      end;
      {$else}
      LSelected := FDialog.Execute;
      {$endif}
      LPath := TNyxText(RawByteString(FDialog.FileName));
    finally
      FreeAndNil(FModalTimer);
      FreeAndNil(FDialog);
    end;

    if not Pending then
    begin
      Cancel;
      Exit;
    end;

    if not LSelected then
    begin
      Complete(nfooCancelled);
      Exit;
    end;
    FChannel := TReadChannel.Create;
    LWork := TReadWork.Create;
    LWorkLease := LWork;
    LWork.Path := LPath;
    LWork.Options := FOptions.Copy;
    LWork.Scope := FScope;
    LWork.Channel := FChannel;
    FWork := FScheduler.Submit(LWorkLease, neThreaded);
  except
    Complete(nfooFailed, nil, 'The native file-open operation could not complete');
  end;
end;

function TOpenHost.BeginOpen(const AOptions: TNyxFileOpenOptions;
  const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
  AReply: TNyxFileOpenReply): INyxFileOpenRequest;
var
  LRequest: TOpenRequest;
begin
  LRequest := TOpenRequest.Create(AOptions, AScheduler, AScope, AReply);
  Result := LRequest;
  LRequest.FDirectory := Directory;
  LRequest.Start;
end;

function NewNyxLCLFileOpenHost(const AInitialDirectory: TNyxText): INyxFileOpenHost;
var
  LHost: TOpenHost;
begin

  if (Length(AInitialDirectory) > 4096) or (Pos(#0, AInitialDirectory) > 0) then
  begin
    raise ENyxFile.Create('Invalid runtime file-open directory hint');
  end;
  LHost := TOpenHost.Create;
  Result := LHost;
  LHost.Directory := AInitialDirectory;
end;

end.
