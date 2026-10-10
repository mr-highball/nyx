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

unit nyx.test.resource.workbench.files;

{$mode delphi}{$H+}{$codepage utf8}
{$ifndef MSWINDOWS}{$fatal This fixture owns Windows browser host files}{$endif}

interface

uses nyx.text;

type
  { Closed file roles for the existing complete ordinary Studio journey.
    These are qualification inputs, never an import API or design namespace. }
  TNyxWorkbenchFile = (nwfCopy, nwfNotes, nwfPacked);

{ Create three exact files and a marker in a nonexistent directory whose parent
  already exists. No existing file is overwritten. Failure retains partial owned
  evidence; the caller chooses another fresh directory instead of deleting it. }
procedure PrepareNyxWorkbenchFiles(const ADirectory: TNyxText);
{ Admit the marker and every complete byte sequence before a browser starts.
  Reads have a 1 MiB bound and close their native handles before returning. }
procedure AdmitNyxWorkbenchFiles(const ADirectory: TNyxText);
{ Return an exact closed role's local filename. Neither function reads files,
  starts a browser nor modifies a Studio/document. The directory is borrowed. }
function NyxWorkbenchFilePath(const ADirectory: TNyxText; AFile: TNyxWorkbenchFile): TNyxText;
function NyxWorkbenchFileRole(const AName: TNyxText): TNyxWorkbenchFile;

implementation

uses Classes, SysUtils, Windows, nyx.bytes, nyx.test.resource.workbench;

const
  CMarker = 'nyx-resource-workbench.files';
  CMarkerText: TNyxText = 'Nyx resource workbench files / version 1';
  CFileNames: array[TNyxWorkbenchFile] of TNyxText = ('copy.dat', 'notes.dat', 'packed.dat');
  CRoleNames: array[TNyxWorkbenchFile] of TNyxText = ('copy', 'notes', 'packed');

function LocalPath(const ADirectory, AName: TNyxText): TNyxText;
begin
  Result := ADirectory;

  if (Result = '') or (Length(Result) > 4000) then
  begin
    raise Exception.Create('Supply a bounded owned workbench file directory');
  end;

  if not (Result[Length(Result)] in ['/', '\']) then
  begin
    Result := Result + TNyxText(PathDelim);
  end;
  Result := Result + AName;
end;

function NyxWorkbenchFilePath(const ADirectory: TNyxText; AFile: TNyxWorkbenchFile): TNyxText;
begin
  Result := LocalPath(ADirectory, CFileNames[AFile]);
end;

function NyxWorkbenchFileRole(const AName: TNyxText): TNyxWorkbenchFile;
var
  LFile: TNyxWorkbenchFile;
begin
  for LFile := Low(TNyxWorkbenchFile) to High(TNyxWorkbenchFile) do
  begin

    if AName = CRoleNames[LFile] then
    begin
      Exit(LFile);
    end;
  end;
  raise Exception.Create('Unknown workbench chooser file role');
end;

function FixtureBytes(AFile: TNyxWorkbenchFile): TNyxBytes;
begin
  Result := nil;
  case AFile of
    nwfCopy: Result := NyxEncodeUTF8(WorkbenchJSON);
    nwfNotes: Result := NyxEncodeUTF8(WorkbenchNotes);
    nwfPacked:
      begin
        SetLength(Result, 3);
        Result[0] := 0;
        Result[1] := 1;
        Result[2] := 255;
      end;
  end;
end;

procedure WriteNew(const AName: TNyxText; const ABytes: TNyxBytes);
var
  LName: UnicodeString;
  LHandle: THandle;
  LStream: THandleStream;
begin
  LName := UTF8Decode(AName);
  LHandle := CreateFileW(PWideChar(LName), GENERIC_WRITE, 0, nil, CREATE_NEW,
    FILE_ATTRIBUTE_NORMAL, 0);

  if LHandle = INVALID_HANDLE_VALUE then
  begin
    raise Exception.Create('Cannot create a new owned workbench file');
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
    CloseHandle(LHandle);
  end;
end;

function ReadOwned(const AName: TNyxText): TNyxBytes;
var
  LName: UnicodeString;
  LHandle: THandle;
  LStream: THandleStream;
begin
  Result := nil;
  LName := UTF8Decode(AName);
  LHandle := CreateFileW(PWideChar(LName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  if LHandle = INVALID_HANDLE_VALUE then
  begin
    raise Exception.Create('Cannot read an admitted workbench fixture file');
  end;
  LStream := nil;
  try
    LStream := THandleStream.Create(LHandle);

    if (LStream.Size < 0) or (LStream.Size > NyxMaximumPackedBytes) then
    begin
      raise Exception.Create('Workbench fixture file exceeds its byte budget');
    end;
    SetLength(Result, Integer(LStream.Size));

    if Length(Result) > 0 then
    begin
      LStream.ReadBuffer(Result[0], Length(Result));
    end;
  finally
    LStream.Free;
    CloseHandle(LHandle);
  end;
end;

procedure PrepareNyxWorkbenchFiles(const ADirectory: TNyxText);
var
  LDirectory: UnicodeString;
  LFile: TNyxWorkbenchFile;
begin
  LocalPath(ADirectory, CMarker);
  LDirectory := UTF8Decode(ADirectory);

  if not CreateDirectoryW(PWideChar(LDirectory), nil) then
  begin
    raise Exception.Create('Workbench files require a nonexistent owned directory');
  end;
  for LFile := Low(TNyxWorkbenchFile) to High(TNyxWorkbenchFile) do
  begin
    WriteNew(NyxWorkbenchFilePath(ADirectory, LFile), FixtureBytes(LFile));
  end;
  WriteNew(LocalPath(ADirectory, CMarker), NyxEncodeUTF8(CMarkerText));
  AdmitNyxWorkbenchFiles(ADirectory);
end;

procedure AdmitNyxWorkbenchFiles(const ADirectory: TNyxText);
var
  LFile: TNyxWorkbenchFile;
begin

  if NyxDecodeUTF8(ReadOwned(LocalPath(ADirectory, CMarker))) <> CMarkerText then
  begin
    raise Exception.Create('Workbench fixture marker does not match');
  end;
  for LFile := Low(TNyxWorkbenchFile) to High(TNyxWorkbenchFile) do
  begin

    if NyxEncodeBase64(ReadOwned(NyxWorkbenchFilePath(ADirectory, LFile))) <>
      NyxEncodeBase64(FixtureBytes(LFile)) then
    begin
      raise Exception.Create('Workbench fixture bytes do not match the complete journey');
    end;
  end;
end;

end.
