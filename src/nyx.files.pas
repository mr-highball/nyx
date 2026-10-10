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

unit nyx.files;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.bytes;

type
  ENyxFile = class(Exception);

  { Immutable exact UTF-8 text file. Name is a portable leaf, never a machine
    path. Text may include NUL or supplementary Unicode. No editor, stream,
    dialog or borrowed array survives construction. }
  INyxTextFile = interface
    ['{63E2E12D-D4A7-4B03-8710-524E916FB9ED}']
    function Name: TNyxText;
    function Text: TNyxText;
    function ByteCount: Integer;
  end;
  TNyxTextFiles = array of INyxTextFile;

  { Value configuration for one selection. Allow copies the extension vector
    before changing it on either compiler. Empty filters permit any text file.
    Extensions are ASCII suffixes without dots/wildcards, not executable input.
    Default construction must use NyxTextFileSelection, not an empty record. }
  TNyxTextFileSelection = record
  private
    FExtensions: array of TNyxText;
    FMaximumFiles: Integer;
    FMaximumBytes: Integer;
  public
    function Allow(const AExtension: TNyxText): TNyxTextFileSelection;
    function UpToFiles(ACount: Integer): TNyxTextFileSelection;
    function UpToBytes(ACount: Integer): TNyxTextFileSelection;
    function Accepts(const AName: TNyxText): Boolean;
    function Extension(AIndex: Integer): TNyxText;
    function ExtensionCount: Integer;
    procedure Validate;
    property MaximumFiles: Integer read FMaximumFiles;
    property MaximumBytes: Integer read FMaximumBytes;
  end;

  TNyxFilePickStatus = (fpsSelected, fpsCancelled, fpsFailed);
  { Copied immutable payloads are owned by the reply. The exchange borrows the
    receiver only until completion/Cancel. Native dialogs may complete inline;
    browser reads require user activation and complete asynchronously. }
  TNyxTextFileReply = procedure(AStatus: TNyxFilePickStatus;
    const AFiles: TNyxTextFiles; const AError: TNyxText) of object;

  { UI-thread adapter, independent of DOM/LCL and project syntax. Cancel silently
    detaches the pending picker receiver and retires its temporary input/readers.
    Reentrant completion may release the exchange; adapters lease themselves.
    Export never changes supplied values or editor state. True means the host
    accepted every delivery, not proof of a browser disk save. Cancellation may
    leave earlier files delivered; multi-file exports are separate host writes. }
  INyxTextFileExchange = interface
    ['{8FE95F3B-1906-40AE-9626-D1AC34E8BF02}']
    procedure Pick(const ASelection: TNyxTextFileSelection; AReply: TNyxTextFileReply);
    function ExportFiles(const AFiles: TNyxTextFiles): Boolean;
    procedure Cancel;
  end;

const
  NyxTextFileMaximumBytes = 4 * 1024 * 1024;
  NyxTextFileMaximumSelectionBytes = 8 * 1024 * 1024;
  NyxTextFileMaximumFiles = 32;

{ Exact scalar admission and byte budget precede immutable publication. Invalid
  portable leaf names, malformed Unicode or over-budget text refuse. }
function NyxTextFile(const AName, AText: TNyxText): INyxTextFile;
{ Strict UTF-8 byte boundary; invalid bytes never become replacement characters. }
function NyxTextFileBytes(const AName: TNyxText;
  const ABytes: TNyxBytes): INyxTextFile;
function NyxTextFileSelection: TNyxTextFileSelection;
{ Refuse nil payloads/empty groups and over-budget delivery before host effects. }
procedure ValidateNyxTextFiles(const AFiles: TNyxTextFiles;
  const ASelection: TNyxTextFileSelection);

implementation

uses nyx.editing;

type
  TTextFile = class(TInterfacedObject, INyxTextFile)
  private
    FName: TNyxText;
    FText: TNyxText;
    FBytes: Integer;
  public
    constructor Create(const AName, AText: TNyxText);
    function Name: TNyxText;
    function Text: TNyxText;
    function ByteCount: Integer;
  end;

constructor TTextFile.Create(const AName, AText: TNyxText);
var
  LIndex: Integer;
begin
  inherited Create;

  if (NyxTextScalarCount(AName) < 1) or (NyxTextScalarCount(AName) > 255) or
    (AName = '.') or (AName = '..') or
    (AName[Length(AName)] = '.') or (AName[Length(AName)] = ' ') then
  begin
    raise ENyxFile.Create('A text file needs a portable leaf name');
  end;
  for LIndex := 1 to Length(AName) do
  begin

    if (Ord(AName[LIndex]) < 32) or
      (Pos(AName[LIndex], '/\:*?"<>|') > 0) then
    begin
      raise ENyxFile.Create('A text file name cannot contain paths or reserved characters');
    end;
  end;
  FBytes := NyxUTF8ByteCount(AText);

  if FBytes > NyxTextFileMaximumBytes then
  begin
    raise ENyxFile.Create('A text file exceeds the 4 MiB byte budget');
  end;
  FName := AName;
  FText := AText;
end;

function TTextFile.Name: TNyxText;
begin
  Result := FName;
end;

function TTextFile.Text: TNyxText;
begin
  Result := FText;
end;

function TTextFile.ByteCount: Integer;
begin
  Result := FBytes;
end;

function NyxTextFile(const AName, AText: TNyxText): INyxTextFile;
begin
  Result := TTextFile.Create(AName, AText);
end;

function NyxTextFileBytes(const AName: TNyxText;
  const ABytes: TNyxBytes): INyxTextFile;
begin

  if Length(ABytes) > NyxTextFileMaximumBytes then
  begin
    raise ENyxFile.Create('A selected text file exceeds the byte budget');
  end;
  Result := NyxTextFile(AName, NyxDecodeUTF8(ABytes));
end;

function NyxTextFileSelection: TNyxTextFileSelection;
begin
  Result := Default(TNyxTextFileSelection);
  Result.FMaximumFiles := 1;
  Result.FMaximumBytes := NyxTextFileMaximumBytes;
end;

function TNyxTextFileSelection.Allow(const AExtension: TNyxText): TNyxTextFileSelection;
var
  LIndex: Integer;
  LExtension: TNyxText;
  LExtensions: array of TNyxText;
begin

  if (Length(AExtension) < 1) or (Length(AExtension) > 24) then
  begin
    raise ENyxFile.Create('Use a nonempty file extension without a dot');
  end;
  LExtension := LowerCase(AExtension);
  for LIndex := 1 to Length(LExtension) do
  begin

    if not (LExtension[LIndex] in ['a'..'z', '0'..'9', '-']) then
    begin
      raise ENyxFile.Create('File extensions require ASCII letters, digits or hyphens');
    end;
  end;
  LExtensions := nil;
  SetLength(LExtensions, Length(FExtensions) + 1);
  for LIndex := 0 to High(FExtensions) do
  begin
    LExtensions[LIndex] := FExtensions[LIndex];
  end;
  LExtensions[Length(FExtensions)] := LExtension;
  Result := Self;
  Result.FExtensions := LExtensions;
end;

function TNyxTextFileSelection.UpToFiles(ACount: Integer): TNyxTextFileSelection;
begin

  if (ACount < 1) or (ACount > NyxTextFileMaximumFiles) then
  begin
    raise ENyxFile.Create('Text-file selection counts range from one through 32');
  end;
  Result := Self;
  Result.FMaximumFiles := ACount;
end;

function TNyxTextFileSelection.UpToBytes(ACount: Integer): TNyxTextFileSelection;
begin

  if (ACount < 1) or (ACount > NyxTextFileMaximumBytes) then
  begin
    raise ENyxFile.Create('Text-file byte limits range from one byte through 4 MiB');
  end;
  Result := Self;
  Result.FMaximumBytes := ACount;
end;

procedure TNyxTextFileSelection.Validate;
begin

  if (FMaximumFiles < 1) or (FMaximumFiles > NyxTextFileMaximumFiles) or
    (FMaximumBytes < 1) or (FMaximumBytes > NyxTextFileMaximumBytes) then
  begin
    raise ENyxFile.Create('Initialize a typed text-file selection before use');
  end;
end;

function TNyxTextFileSelection.Accepts(const AName: TNyxText): Boolean;
var
  LIndex: Integer;
  LExtension: TNyxText;
begin
  Validate;
  Result := Length(FExtensions) = 0;
  LExtension := LowerCase(ExtractFileExt(AName));

  if LExtension <> '' then
  begin
    Delete(LExtension, 1, 1);
  end;
  for LIndex := 0 to High(FExtensions) do
  begin

    if FExtensions[LIndex] = LExtension then
    begin
      Exit(True);
    end;
  end;
end;

function TNyxTextFileSelection.Extension(AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= Length(FExtensions)) then
  begin
    raise ENyxFile.Create('File extension index is outside the selection');
  end;
  Result := FExtensions[AIndex];
end;

function TNyxTextFileSelection.ExtensionCount: Integer;
begin
  Result := Length(FExtensions);
end;

procedure ValidateNyxTextFiles(const AFiles: TNyxTextFiles;
  const ASelection: TNyxTextFileSelection);
var
  LIndex: Integer;
  LTotal: Integer;
begin
  ASelection.Validate;

  if (Length(AFiles) < 1) or (Length(AFiles) > ASelection.MaximumFiles) then
  begin
    raise ENyxFile.Create('The selected text-file count exceeds the request');
  end;
  LTotal := 0;
  for LIndex := 0 to High(AFiles) do
  begin

    if (AFiles[LIndex] = nil) or not ASelection.Accepts(AFiles[LIndex].Name) or
      (AFiles[LIndex].ByteCount > ASelection.MaximumBytes) then
    begin
      raise ENyxFile.Create('A selected text file is missing, filtered or over budget');
    end;
    Inc(LTotal, AFiles[LIndex].ByteCount);
  end;

  if LTotal > NyxTextFileMaximumSelectionBytes then
  begin
    raise ENyxFile.Create('Selected text files exceed the complete byte budget');
  end;
end;

end.
