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

unit nyx.files.save.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.files.save, nyx.bytes.sources;

{ Runtime-only initial directory is never serialized. Native Download mode is
  unavailable; picker modes use the real LCL save dialog and exact byte writer. }
function NewNyxLCLFileSaveHost(const AInitialDirectory: TNyxText = ''): INyxFileSaveHost;

implementation

uses
  SysUtils, Classes, Dialogs, ExtCtrls, LazFileUtils,
  nyx.bytes, nyx.files, nyx.scheduler
  {$ifdef windows}, Windows{$else}, BaseUnix{$endif};

type
  { Forward for one queued UI turn; its retained request lease protects the
    borrowed receiver until execution or scheduler retirement. }
  TSaveRequest = class;

  { One cooperative UI continuation. Skipped armed work cancels its request,
    closing staged output and detaching the borrowed reply during retirement.
    PostTurn is UI-owned and cannot execute inline; arm only after admission. }
  TStreamTurn = class(TInterfacedObject, INyxWork)
  public
    Request: TSaveRequest;
    Lease: INyxFileSaveRequest;
    Armed: Boolean;
    Ran: Boolean;
    destructor Destroy; override;
    procedure Execute(const AExecution: INyxExecution);
  end;

  { One UI-thread save through a real LCL dialog and a staged byte writer.
    Owns its ownerless dialog/timer and copied runtime directory; shared-base and
    local leases protect nested modal turns. Cancellation closes only this
    request's dialog and revokes its borrowed reply before cleanup. The selected
    machine path remains private; portable completion exposes only its leaf. }
  TSaveRequest = class(TNyxFileSaveRequest)
  private
    FDirectory: TNyxText;
    FDialog: TSaveDialog;
    FTimer: TTimer;
    FStage: TNyxText;
    FSelected: TNyxText;
    FStageHandle: THandle;
    FStageStream: THandleStream;
    {$ifdef windows}
    FDialogWindow: HWND;
    {$endif}
    procedure ObserveCancellation(ASender: TObject);
    procedure WriteSelected(const APath: TNyxText);
    procedure OpenStreamStage(const APath: TNyxText);
    procedure ReleaseStreamStage;
    procedure QueueStreamTurn;
    procedure WriteStreamTurn;
  protected
    procedure EndHost; override;
  public
    constructor Create(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply; const ADirectory: TNyxText);
    constructor CreateStream(const AOptions: TNyxFileSaveStreamOptions;
      const ASource: INyxByteSource; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply;
      const ADirectory: TNyxText);
    destructor Destroy; override;
    procedure Start;
  end;

  { Reference-counted native adapter retaining only a runtime initial directory.
    BeginSave owns no shared dialog: every call creates an independent request
    and may complete inline while the LCL modal loop runs. Native hosts refuse
    browser Download mode instead of reporting a different kind of delivery. }
  TSaveHost = class(TInterfacedObject, INyxFileSaveHost, INyxFileSaveStreamHost)
  private
    FDirectory: TNyxText;
  public
    constructor Create(const ADirectory: TNyxText);
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
    function BeginStream(const AOptions: TNyxFileSaveStreamOptions;
      const ASource: INyxByteSource; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

{$ifdef windows}
const
  CSaveRequestProperty: PWideChar = 'Nyx.FileSave.Request';

var
  { Borrowed UI-thread stack head only during Execute. Nested saves restore the
    previous request in finally; active request leases outlive all hook calls. }
  GOpeningRequest: TSaveRequest;

function SaveDialogHook(ACode: Integer; AWindow: WPARAM; AData: LPARAM): LRESULT; stdcall;
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
      (UnicodeString(LTitle) = UTF8Decode(TNyxText(GOpeningRequest.FDialog.Title))) then
    begin

      if SetPropW(HWND(AWindow), CSaveRequestProperty, THandle(PtrUInt(GOpeningRequest))) then
      begin
        GOpeningRequest.FDialogWindow := HWND(AWindow);
      end;
    end;
  end
  else if (ACode = HCBT_DESTROYWND) and (GOpeningRequest <> nil) and
    (GOpeningRequest.FDialogWindow = HWND(AWindow)) then
  begin
    GOpeningRequest.FDialogWindow := 0;
  end;
  { Observe only; never suppress activation, creation or another hook's result. }
  Result := CallNextHookEx(0, ACode, AWindow, AData);
end;
{$endif}

constructor TSaveHost.Create(const ADirectory: TNyxText);
begin
  inherited Create;
  FDirectory := ADirectory;
end;

constructor TSaveRequest.Create(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply;
  const ADirectory: TNyxText);
begin
  FStageHandle := THandle(-1);
  inherited Create(AOptions, ABytes, AScheduler, ACancellation, AReply);
  FDirectory := ADirectory;
end;

constructor TSaveRequest.CreateStream(const AOptions: TNyxFileSaveStreamOptions;
  const ASource: INyxByteSource; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply;
  const ADirectory: TNyxText);
begin
  { Initialize before admission: a constructor failure still runs destruction. }
  FStageHandle := THandle(-1);
  inherited CreateStream(AOptions, ASource, AScheduler, ACancellation, AReply);
  FDirectory := ADirectory;

  if not NyxSchedulerTurns(AScheduler).CanYieldUI then
  begin
    raise ENyxFile.Create('Native streaming save requires cooperative UI turns');
  end;
end;

destructor TStreamTurn.Destroy;
begin

  if Armed and not Ran and (Lease <> nil) then
  begin
    Lease.Cancel;
  end;
  inherited Destroy;
end;

procedure TStreamTurn.Execute(const AExecution: INyxExecution);
begin
  Ran := True;

  if AExecution.Cancelled then
  begin
    Lease.Cancel;
  end
  else
  begin
    Request.WriteStreamTurn;
  end;
end;

procedure TSaveRequest.QueueStreamTurn;
var
  LTurn: TStreamTurn;
  LWork: INyxWork;
begin
  LTurn := TStreamTurn.Create;
  LTurn.Request := Self;
  LTurn.Lease := Self;
  LWork := LTurn;
  NyxSchedulerTurns(FScheduler).PostTurn(LWork);
  LTurn.Armed := True;
end;

procedure TSaveRequest.ReleaseStreamStage;
begin
  FreeAndNil(FStageStream);

  if FStageHandle <> THandle(-1) then
  begin
    FileClose(FStageHandle);
    FStageHandle := THandle(-1);
  end;

  if FStage <> '' then
  begin
    DeleteFileUTF8(FStage);
    FStage := '';
  end;
end;

procedure TSaveRequest.OpenStreamStage(const APath: TNyxText);
var
  LIdentifier: TGUID;
begin
  FSelected := APath;
  CreateGUID(LIdentifier);
  FStage := IncludeTrailingPathDelimiter(ExtractFilePath(APath)) +
    TNyxText('.nyx-') + TNyxText(GUIDToString(LIdentifier)) + TNyxText('.tmp');
  {$ifdef windows}
  FStageHandle := CreateFileW(PWideChar(UTF8Decode(FStage)), GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  {$else}
  FStageHandle := fpOpen(PChar(FStage), O_WRONLY or O_CREAT or O_EXCL, &600);
  {$endif}

  if FStageHandle = THandle(-1) then
  begin
    { No ownership was acquired: cleanup must not delete another owner's file. }
    FStage := '';
    raise ENyxFile.Create('Cannot exclusively stage the selected file');
  end;
  FStageStream := THandleStream.Create(FStageHandle);
end;

procedure TSaveRequest.WriteStreamTurn;
var
  LChunk: TNyxBytes;
  LInstalled: Boolean;
  LFinished: Boolean;
  LFailed: Boolean;
begin

  if not Pending then
  begin
    Cancel;
    Exit;
  end;
  LFinished := False;
  LFailed := False;
  try
    LChunk := ReadPayloadChunk;

    if not Pending then
    begin
      Cancel;
      Exit;
    end;

    if Length(LChunk) > 0 then
    begin
      FStageStream.WriteBuffer(LChunk[0], Length(LChunk));
      QueueStreamTurn;
    end
    else
    begin
      FreeAndNil(FStageStream);
      FileClose(FStageHandle);
      FStageHandle := THandle(-1);
      {$ifdef windows}
      LInstalled := MoveFileExW(PWideChar(UTF8Decode(FStage)), PWideChar(UTF8Decode(FSelected)),
        MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH);
      {$else}
      LInstalled := RenameFileUTF8(FStage, FSelected);
      {$endif}

      if not LInstalled then
      begin
        raise ENyxFile.Create('Cannot replace the selected file');
      end;
      FStage := '';
      LFinished := True;
    end;
  except
    LFailed := True;
  end;

  if LFailed then
  begin
    Complete(nfsoFailed, FOptions.SuggestedName, 0,
      'The streamed file could not be committed; the previous file was retained');
  end
  else if LFinished then
  begin
    Complete(nfsoWritten, TNyxText(ExtractFileName(FSelected)), FExpectedBytes);
  end;
end;

procedure TSaveRequest.EndHost;
begin
  ReleaseStreamStage;

  {$ifdef windows}

  if (FDialogWindow <> 0) and IsWindow(FDialogWindow) and
    (GetPropW(FDialogWindow, CSaveRequestProperty) = THandle(PtrUInt(Self))) then
  begin
    { LCL's opaque Handle may be an IFileDialog pointer, not an HWND. A thread-
      local CBT observation captures and tags this exact dialog window while it
      opens. The tag prevents a reused HWND from targeting a different dialog. }
    PostMessage(FDialogWindow, WM_COMMAND, IDCANCEL, 0);

    if FTimer <> nil then
    begin
      FTimer.Enabled := False;
    end;
  end;
  { If cancellation precedes activation, keep observing until this request's
    window is captured. Cleanup after Execute always frees the timer. }
  {$else}

  if FTimer <> nil then
  begin
    FTimer.Enabled := False;
  end;

  if (FDialog <> nil) and FDialog.HandleAllocated then
  begin
    FDialog.Close;
  end;
  {$endif}
end;

destructor TSaveRequest.Destroy;
begin
  ReleaseStreamStage;
  FTimer.Free;
  FDialog.Free;
  inherited Destroy;
end;

procedure TSaveRequest.ObserveCancellation(ASender: TObject);
begin

  if not Pending then
  begin
    Cancel;
  end;
end;

procedure TSaveRequest.WriteSelected(const APath: TNyxText);
var
  LIdentifier: TGUID;
  LStage: TNyxText;
  LHandle: THandle;
  LStream: THandleStream;
  LInstalled: Boolean;
begin
  CreateGUID(LIdentifier);
  LStage := IncludeTrailingPathDelimiter(ExtractFilePath(APath)) +
    TNyxText('.nyx-') + TNyxText(GUIDToString(LIdentifier)) + TNyxText('.tmp');
  LHandle := THandle(-1);
  LInstalled := False;
  LStream := nil;
  try
    {$ifdef windows}
    { Exclusive creation cannot truncate another file at the staging name.
      Replace happens only after the complete new bytes close successfully. }
    LHandle := CreateFileW(PWideChar(UTF8Decode(LStage)), GENERIC_WRITE, 0, nil,
      CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
    {$else}
    LHandle := fpOpen(PChar(LStage), O_WRONLY or O_CREAT or O_EXCL, &600);
    {$endif}

    if LHandle = THandle(-1) then
    begin
      { Exclusive creation acquired no ownership. Never remove a colliding
        existing path merely because the selected file could not be staged. }
      LStage := '';
      raise Exception.Create('Cannot stage the selected file');
    end;
    LStream := THandleStream.Create(LHandle);

    if Length(FBytes) > 0 then
    begin
      LStream.WriteBuffer(FBytes[0], Length(FBytes));
    end;
    FreeAndNil(LStream);
    FileClose(LHandle);
    LHandle := THandle(-1);

    if not Pending then
    begin
      Exit;
    end;
    {$ifdef windows}
    LInstalled := MoveFileExW(PWideChar(UTF8Decode(LStage)), PWideChar(UTF8Decode(APath)),
      MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH);
    {$else}
    LInstalled := RenameFileUTF8(LStage, APath);
    {$endif}

    if not LInstalled and (LStage <> '') then
    begin
      raise Exception.Create('Cannot replace the selected file');
    end;
  finally
    LStream.Free;

    if LHandle <> THandle(-1) then
    begin
      FileClose(LHandle);
    end;

    if not LInstalled then
    begin
      DeleteFileUTF8(LStage);
    end;
  end;
end;

procedure TSaveRequest.Start;
var
  LLease: INyxFileSaveRequest;
  LSelected: Boolean;
  LPath: TNyxText;
  LFilter: TNyxText;
  LIndex: Integer;
  LCount: Integer;
  LFailed: Boolean;
  LLeaf: INyxTextFile;
  {$ifdef windows}
  LHook: HHOOK;
  LPrevious: TSaveRequest;
  {$endif}
begin
  FScheduler.RequireUI;
  LLease := Self;
  RetainPending;

  if not Pending then
  begin
    Cancel;
    Exit;
  end;

  if FOptions.SaveMode = nfsmDownload then
  begin
    Complete(nfsoUnavailable, FOptions.SuggestedName, 0,
      'This host provides a save picker rather than browser downloads');
    Exit;
  end;
  LSelected := False;
  LPath := '';
  LFailed := False;
  FDialog := TSaveDialog.Create(nil);
  FTimer := TTimer.Create(nil);
  try
    FDialog.Title := FOptions.DialogTitle;

    if FDialog.Title = '' then
    begin
      FDialog.Title := 'Save file';
    end;
    FDialog.FileName := FOptions.SuggestedName;
    FDialog.InitialDir := FDirectory;
    FDialog.Options := [ofPathMustExist, ofOverwritePrompt, ofEnableSizing];
    LFilter := '';
    for LIndex := 0 to FOptions.ExtensionCount - 1 do
    begin

      if LFilter <> '' then
      begin
        LFilter := LFilter + ';';
      end;
      LFilter := LFilter + '*.' + FOptions.Extension(LIndex);
    end;

    if LFilter <> '' then
    begin
      FDialog.Filter := 'Allowed file types|' + LFilter;
      FDialog.DefaultExt := FOptions.Extension(0);
    end;

    if FOptions.IncludeAllTypes then
    begin

      if FDialog.Filter <> '' then
      begin
        FDialog.Filter := FDialog.Filter + '|';
      end;
      FDialog.Filter := FDialog.Filter + 'All files|*';
    end;
    FTimer.Interval := 25;
    FTimer.OnTimer := ObserveCancellation;
    FTimer.Enabled := True;
    {$ifdef windows}
    LPrevious := GOpeningRequest;
    LHook := SetWindowsHookExW(WH_CBT, @SaveDialogHook, 0, GetCurrentThreadId);

    if LHook = 0 then
    begin
      raise ENyxFile.Create('Cannot establish save dialog cancellation ownership');
    end;
    GOpeningRequest := Self;
    try
      LSelected := FDialog.Execute;
    finally

      if (FDialogWindow <> 0) and
        (GetPropW(FDialogWindow, CSaveRequestProperty) = THandle(PtrUInt(Self))) then
      begin
        RemovePropW(FDialogWindow, CSaveRequestProperty);
      end;
      FDialogWindow := 0;
      GOpeningRequest := LPrevious;
      UnhookWindowsHookEx(LHook);
    end;
    {$else}
    LSelected := FDialog.Execute;
    {$endif}
    LPath := FDialog.FileName;
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  FreeAndNil(FTimer);
  FreeAndNil(FDialog);

  if not Pending then
  begin
    Cancel;
    Exit;
  end;

  if LFailed then
  begin
    Complete(nfsoFailed, FOptions.SuggestedName, 0, 'The save dialog could not complete');
    Exit;
  end;

  if not LSelected then
  begin
    Complete(nfsoCancelled, FOptions.SuggestedName, 0);
    Exit;
  end;
  LCount := Length(FBytes);
  try
    { Check the portable receipt before any file effect, including host-selected
      names which differ from the original suggestion. }
    LLeaf := NyxTextFile(TNyxText(ExtractFileName(LPath)), '');

    if FStreamed then
    begin
      OpenStreamStage(LPath);
      QueueStreamTurn;
      Exit;
    end;
    WriteSelected(LPath);
  except
    on E: Exception do
    begin
      Complete(nfsoFailed, FOptions.SuggestedName, 0,
        'The selected file could not be written; the previous file was retained');
      Exit;
    end;
  end;

  if Pending then
  begin
    Complete(nfsoWritten, TNyxText(ExtractFileName(LPath)), LCount);
  end
  else
  begin
    Cancel;
  end;
end;

function TSaveHost.BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LRequest: TSaveRequest;
begin
  LRequest := TSaveRequest.Create(AOptions, ABytes, AScheduler, ACancellation, AReply, FDirectory);
  Result := LRequest;
  LRequest.Start;
end;

function NewNyxLCLFileSaveHost(const AInitialDirectory: TNyxText): INyxFileSaveHost;
begin
  Result := TSaveHost.Create(AInitialDirectory);
end;

function TSaveHost.BeginStream(const AOptions: TNyxFileSaveStreamOptions;
  const ASource: INyxByteSource; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LRequest: TSaveRequest;
begin
  LRequest := TSaveRequest.CreateStream(AOptions, ASource, AScheduler, ACancellation,
    AReply, FDirectory);
  Result := LRequest;
  LRequest.Start;
end;

end.
