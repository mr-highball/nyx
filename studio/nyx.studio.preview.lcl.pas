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

unit nyx.studio.preview.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, ExtCtrls, Process, nyx.text, nyx.studio.preview;

type
  TNyxPreviewPrepared = procedure(ASucceeded: Boolean; const AError: TNyxText) of object;

  { Native compiled-preview adapter. Owns a byte-only download worker, UI timer,
    private artifact directory and the process it launches. The prepared receiver
    is borrowed and detached before joining. Prepare never calls it inline.
    Launch is explicit, after the controller rechecks exact project/profile
    currentness. Native windows run separately; browser output uses the system
    browser. Neither preview is the uncompiled designer projection.
    The machine origin/directory stay outside portable documents and history. }
  TNyxLCLCompiledPreview = class
  private
    FBase: TNyxText;
    FDirectory: TNyxText;
    FWorker: TThread;
    FTimer: TTimer;
    FPrepared: TNyxPreviewPrepared;
    FArtifact: TNyxCompiledArtifact;
    FReady: Boolean;
    FCanceled: Boolean;
    FProcess: TProcess;
    FFile: TNyxText;
    FProcessFile: TNyxText;
    procedure RetireFile(const AFile: TNyxText);
    procedure Poll(ASender: TObject);
    function GetProcessID: Integer;
    function GetRunning: Boolean;
  public
    constructor Create(const ABase, ADirectory: TNyxText);
    destructor Destroy; override;
    { One preparation at a time. Download only the admitted native executable,
      cap received bytes, compare exact size/MD5 and commit a unique local file.
      Browser preparation retains its validated immutable resource URL. }
    procedure Prepare(const AArtifact: TNyxCompiledArtifact; APrepared: TNyxPreviewPrepared);
    { Detach delivery immediately. Server/download work may finish independently;
      destruction joins it before freeing any borrowed callback owner. }
    procedure Cancel;
    { Requires a successful preparation. Arguments/shell commands are absent.
      Launch failure retains the previous owned native process. }
    procedure Launch;
    { Stops only the process handle this adapter created. Never touches another
      Studio, compiler server or independently launched user application. }
    procedure Stop;
    property Ready: Boolean read FReady;
    property Running: Boolean read GetRunning;
    property ProcessID: Integer read GetProcessID;
  end;

implementation

uses
  SysUtils, fphttpclient, md5, LCLIntf, nyx.studio.builds, nyx.studio.exchange.lcl;

type
  TArtifactBytes = class(TMemoryStream)
  public
    Limit: Integer;
    function Write(const ABuffer; ACount: Longint): Longint; override;
  end;

  { Immutable request bytes and file metadata; no widgets, sessions or editor
    callback owner are accessed on this thread. IO/connect waits are five seconds;
    those bounds are not a whole-request elapsed-time deadline. }
  TArtifactWorker = class(TThread)
  private
    FBase: TNyxText;
    FDirectory: TNyxText;
    FArtifact: TNyxCompiledArtifact;
  protected
    procedure Execute; override;
  public
    FileName: TNyxText;
    Error: TNyxText;
    constructor Create(const ABase, ADirectory: TNyxText; const AArtifact: TNyxCompiledArtifact);
  end;

function TArtifactBytes.Write(const ABuffer; ACount: Longint): Longint;
begin

  if (ACount < 0) or (Position > Limit - ACount) then
  begin
    raise Exception.Create('Compiled artifact exceeds its admitted byte count');
  end;
  Result := inherited Write(ABuffer, ACount);
end;

constructor TArtifactWorker.Create(const ABase, ADirectory: TNyxText;
  const AArtifact: TNyxCompiledArtifact);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FBase := ABase;
  FDirectory := ADirectory;
  FArtifact := AArtifact;
end;

procedure TArtifactWorker.Execute;
var
  LClient: TFPHTTPClient;
  LBytes: TArtifactBytes;
  LFile: TFileStream;
  LIdentity: TGUID;
  LDirectory: TNyxText;
begin
  LClient := nil;
  LBytes := nil;
  LFile := nil;
  try
    try
      LClient := TFPHTTPClient.Create(nil);
      LClient.ConnectTimeout := 5000;
      LClient.IOTimeout := 5000;
      LClient.AllowRedirect := False;
      LBytes := TArtifactBytes.Create;
      LBytes.Limit := FArtifact.ByteCount;
      LClient.HTTPMethod('GET', FBase + '/' + FArtifact.RelativePath, LBytes, [200]);

      if Terminated then
      begin
        Exit;
      end;

      if (LBytes.Size <> FArtifact.ByteCount) or
        (MD5Print(MD5Buffer(LBytes.Memory^, LBytes.Size)) <> FArtifact.MD5) then
      begin
        raise Exception.Create('Compiled artifact does not match its admitted byte manifest');
      end;
      CreateGUID(LIdentity);
      LDirectory := FDirectory + 'preview-' + Copy(GUIDToString(LIdentity), 2, 36) + PathDelim;

      if not ForceDirectories(LDirectory) then
      begin
        raise Exception.Create('Cannot prepare the local compiled preview directory');
      end;
      FileName := LDirectory + 'nyx_native.exe';
      LFile := TFileStream.Create(FileName, fmCreate);
      LBytes.Position := 0;
      LFile.CopyFrom(LBytes, LBytes.Size);
    except
      on LException: Exception do
      begin
        { Return bounded local help; do not expose origin/path/transport details. }
        Error := 'Compiled preview could not be downloaded or verified';
        { Keep any partially written owned path for UI-thread retirement after
          this thread has closed the file. Error prevents it becoming runnable. }
      end;
    end;
  finally
    LFile.Free;
    LBytes.Free;
    LClient.Free;
  end;
end;

constructor TNyxLCLCompiledPreview.Create(const ABase, ADirectory: TNyxText);
begin
  inherited Create;
  ValidateNyxLocalStudioOrigin(ABase);
  FBase := ABase;
  FDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory));
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 25;
  FTimer.OnTimer := Poll;
end;

destructor TNyxLCLCompiledPreview.Destroy;
begin
  Cancel;
  FTimer.Free;

  if FWorker <> nil then
  begin
    FWorker.WaitFor;
    RetireFile(TArtifactWorker(FWorker).FileName);
    FWorker.Free;
  end;
  Stop;
  RetireFile(FFile);
  inherited Destroy;
end;

procedure TNyxLCLCompiledPreview.RetireFile(const AFile: TNyxText);
var
  LAbsolute: TNyxText;
  LRelative: TNyxText;
begin

  if AFile = '' then
  begin
    Exit;
  end;
  LAbsolute := ExpandFileName(AFile);

  if Copy(LAbsolute, 1, Length(FDirectory)) <> FDirectory then
  begin
    raise Exception.Create('Preview retirement refused a path outside its owned directory');
  end;
  LRelative := Copy(LAbsolute, Length(FDirectory) + 1, MaxInt);

  if (Copy(LRelative, 1, 8) <> 'preview-') or
    (ExtractFileName(LAbsolute) <> 'nyx_native.exe') then
  begin
    raise Exception.Create('Preview retirement requires its own artifact file');
  end;
  { No recursive removal: delete the exact created file and its now-empty
    private directory, after its process handle has retired. }
  DeleteFile(LAbsolute);
  RemoveDir(ExtractFileDir(LAbsolute));
end;

procedure TNyxLCLCompiledPreview.Cancel;
begin
  FPrepared := nil;
  FCanceled := True;
  FReady := False;

  if FTimer <> nil then
  begin
    FTimer.Enabled := FWorker <> nil;
  end;

  if FWorker <> nil then
  begin
    FWorker.Terminate;
  end;
end;

procedure TNyxLCLCompiledPreview.Prepare(const AArtifact: TNyxCompiledArtifact;
  APrepared: TNyxPreviewPrepared);
begin

  if not Assigned(APrepared) or (AArtifact.RelativePath = '') then
  begin
    raise Exception.Create('Compiled preview requires its admitted artifact and UI receiver');
  end;

  if FWorker <> nil then
  begin
    raise Exception.Create('A compiled preview download is still pending');
  end;
  FArtifact := AArtifact;
  FReady := False;
  FCanceled := False;
  FPrepared := APrepared;

  if AArtifact.Target = btNativeLCL then
  begin
    FWorker := TArtifactWorker.Create(FBase, FDirectory, AArtifact);
    try
      FWorker.Start;
    except
      FreeAndNil(FWorker);
      FPrepared := nil;
      raise;
    end;
  end;
  FTimer.Enabled := True;
end;

procedure TNyxLCLCompiledPreview.Poll(ASender: TObject);
var
  LPrepared: TNyxPreviewPrepared;
  LError: TNyxText;
begin

  if (FWorker <> nil) and not FWorker.Finished then
  begin
    Exit;
  end;
  FTimer.Enabled := False;
  LPrepared := FPrepared;
  FPrepared := nil;
  LError := '';

  if FWorker <> nil then
  begin
    FWorker.WaitFor;

    if FFile <> FProcessFile then
    begin
      RetireFile(FFile);
    end;
    FFile := TArtifactWorker(FWorker).FileName;
    LError := TArtifactWorker(FWorker).Error;
    FreeAndNil(FWorker);
  end;
  FReady := not FCanceled and (LError = '') and
    ((FArtifact.Target = btBrowser) or (FFile <> ''));

  if Assigned(LPrepared) then
  begin
    LPrepared(FReady, LError);
  end;
end;

procedure TNyxLCLCompiledPreview.Launch;
var
  LCandidate: TProcess;
begin

  if not FReady then
  begin
    raise Exception.Create('Prepare and verify the current compiled artifact before running it');
  end;

  if FArtifact.Target = btBrowser then
  begin

    if not OpenURL(FBase + '/' + FArtifact.RelativePath) then
    begin
      raise Exception.Create('The system browser could not open the compiled preview');
    end;
    Stop;
    Exit;
  end;
  LCandidate := TProcess.Create(nil);
  try
    LCandidate.Executable := FFile;
    LCandidate.CurrentDirectory := ExtractFileDir(FFile);
    LCandidate.Options := [poNoConsole];
    LCandidate.Execute;
    Stop;
    FProcess := LCandidate;
    FProcessFile := FFile;
    LCandidate := nil;
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxLCLCompiledPreview.Stop;
begin

  if (FProcess <> nil) and FProcess.Running then
  begin
    FProcess.Terminate(0);
    FProcess.WaitOnExit;
  end;
  FreeAndNil(FProcess);

  if FProcessFile <> FFile then
  begin
    RetireFile(FProcessFile);
  end;
  FProcessFile := '';
end;

function TNyxLCLCompiledPreview.GetProcessID: Integer;
begin
  Result := 0;

  if (FProcess <> nil) and FProcess.Running then
  begin
    Result := FProcess.ProcessID;
  end;
end;

function TNyxLCLCompiledPreview.GetRunning: Boolean;
begin
  Result := GetProcessID <> 0;
end;

end.
