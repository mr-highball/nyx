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

unit nyx.files.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.files;

{ Explicit native path boundary. Handles retire before immutable UTF-8 text
  escapes. Read refuses malformed bytes or files above the per-file budget. }
function ReadNyxTextFile(const AFileName: TNyxText): INyxTextFile;
{ Host-selected destination only. Exact UTF-8 bytes are written without RTL
  newline/codepage conversion. This is one ordinary file write, not a paired
  journal or an atomic project publication. }
procedure WriteNyxTextFile(const AFileName: TNyxText; const AFile: INyxTextFile);
function NewNyxLCLTextFiles: INyxTextFileExchange;

implementation

uses Classes, SysUtils, Dialogs, LazFileUtils, nyx.bytes;

type
  TTextFiles = class(TInterfacedObject, INyxTextFileExchange)
  private
    FReply: TNyxTextFileReply;
    FGeneration: Integer;
    FPicking: Boolean;
  public
    procedure Pick(const ASelection: TNyxTextFileSelection; AReply: TNyxTextFileReply);
    function ExportFiles(const AFiles: TNyxTextFiles): Boolean;
    procedure Cancel;
  end;

function ReadNyxTextFile(const AFileName: TNyxText): INyxTextFile;
var
  LHandle: THandle;
  LStream: THandleStream;
  LBytes: TNyxBytes;
begin
  LHandle := FileOpenUTF8(AFileName, fmOpenRead or fmShareDenyWrite);

  if LHandle = THandle(-1) then
  begin
    raise ENyxFile.Create('Cannot open the selected text file');
  end;
  LStream := nil;
  try
    LStream := THandleStream.Create(LHandle);

    if (LStream.Size < 0) or (LStream.Size > NyxTextFileMaximumBytes) then
    begin
      raise ENyxFile.Create('Choose a text file up to 4 MiB');
    end;
    SetLength(LBytes, Integer(LStream.Size));

    if Length(LBytes) > 0 then
    begin
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxTextFileBytes(TNyxText(ExtractFileName(AFileName)), LBytes);
  finally
    LStream.Free;
    FileClose(LHandle);
  end;
end;

procedure WriteNyxTextFile(const AFileName: TNyxText; const AFile: INyxTextFile);
var
  LHandle: THandle;
  LStream: THandleStream;
  LBytes: TNyxBytes;
begin

  if AFile = nil then
  begin
    raise ENyxFile.Create('Supply an immutable text file before writing');
  end;
  LBytes := NyxEncodeUTF8(AFile.Text);

  if (Length(LBytes) <> AFile.ByteCount) or (Length(LBytes) > NyxTextFileMaximumBytes) then
  begin
    raise ENyxFile.Create('Text-file delivery has an invalid byte count');
  end;
  LHandle := FileCreateUTF8(AFileName);

  if LHandle = THandle(-1) then
  begin
    raise ENyxFile.Create('Cannot create the selected UTF-8 text file');
  end;
  LStream := nil;
  try
    LStream := THandleStream.Create(LHandle);

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LStream.Free;
    FileClose(LHandle);
  end;
end;

procedure TTextFiles.Cancel;
begin
  FReply := nil;
  Inc(FGeneration);
end;

procedure TTextFiles.Pick(const ASelection: TNyxTextFileSelection;
  AReply: TNyxTextFileReply);
var
  LDialog: TOpenDialog;
  LGeneration: Integer;
  LStatus: TNyxFilePickStatus;
  LFiles: TNyxTextFiles;
  LIndex: Integer;
  LFilter: TNyxText;
  LTotal: Integer;
  LError: TNyxText;
  LReply: TNyxTextFileReply;
  LLease: INyxTextFileExchange;
begin
  ASelection.Validate;

  if FPicking then
  begin
    raise ENyxFile.Create('A native text-file picker is already open');
  end;
  LLease := Self;
  Cancel;
  FReply := AReply;
  LGeneration := FGeneration;
  FPicking := True;
  LStatus := fpsCancelled;
  LFiles := nil;
  LError := '';
  LDialog := TOpenDialog.Create(nil);
  try
    LDialog.Title := 'Open UTF-8 text files';
    LFilter := '*';

    if ASelection.ExtensionCount > 0 then
    begin
      LFilter := '';
      for LIndex := 0 to ASelection.ExtensionCount - 1 do
      begin

        if LFilter <> '' then
        begin
          LFilter := LFilter + ';';
        end;
        LFilter := LFilter + '*.' + ASelection.Extension(LIndex);
      end;
    end;
    LDialog.Filter := 'Text files|' + LFilter;
    LDialog.Options := [ofFileMustExist, ofPathMustExist, ofEnableSizing];

    if ASelection.MaximumFiles > 1 then
    begin
      LDialog.Options := LDialog.Options + [ofAllowMultiSelect];
    end;
    try

      if LDialog.Execute then
      begin

        if (LDialog.Files.Count < 1) or (LDialog.Files.Count > ASelection.MaximumFiles) then
        begin
          raise ENyxFile.Create('The selected file count exceeds the request');
        end;
        SetLength(LFiles, LDialog.Files.Count);
        LTotal := 0;
        for LIndex := 0 to LDialog.Files.Count - 1 do
        begin
          LFiles[LIndex] := ReadNyxTextFile(TNyxText(LDialog.Files[LIndex]));
          Inc(LTotal, LFiles[LIndex].ByteCount);

          if LTotal > NyxTextFileMaximumSelectionBytes then
          begin
            raise ENyxFile.Create('Selected text files exceed the complete byte budget');
          end;
        end;
        ValidateNyxTextFiles(LFiles, ASelection);
        LStatus := fpsSelected;
      end;
    except
      on LException: Exception do
      begin
        LStatus := fpsFailed;
        LFiles := nil;
        LError := LException.Message;
      end;
    end;
  finally
    LDialog.Free;
    FPicking := False;
  end;
  LReply := nil;

  if LGeneration = FGeneration then
  begin
    LReply := FReply;
    FReply := nil;
  end;

  if Assigned(LReply) then
  begin
    LReply(LStatus, LFiles, LError);
  end;
  LLease := nil;
end;

function TTextFiles.ExportFiles(const AFiles: TNyxTextFiles): Boolean;
var
  LDialog: TSaveDialog;
  LIndex: Integer;
  LLease: INyxTextFileExchange;
begin
  ValidateNyxTextFiles(AFiles, NyxTextFileSelection.UpToFiles(NyxTextFileMaximumFiles));
  LLease := Self;
  LDialog := TSaveDialog.Create(nil);
  try
    LDialog.Title := 'Save UTF-8 text file';
    LDialog.Filter := 'UTF-8 text|*';
    LDialog.Options := [ofPathMustExist, ofOverwritePrompt, ofEnableSizing];
    for LIndex := 0 to High(AFiles) do
    begin
      LDialog.FileName := AFiles[LIndex].Name;
      LDialog.DefaultExt := Copy(ExtractFileExt(AFiles[LIndex].Name), 2, MaxInt);

      if not LDialog.Execute then
      begin
        Exit(False);
      end;
      WriteNyxTextFile(TNyxText(LDialog.FileName), AFiles[LIndex]);
    end;
    Result := True;
  finally
    LDialog.Free;
    LLease := nil;
  end;
end;

function NewNyxLCLTextFiles: INyxTextFileExchange;
begin
  Result := TTextFiles.Create;
end;

end.
