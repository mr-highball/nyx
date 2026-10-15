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

unit nyx.studio.outputstore;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.outputs;

{ Native machine-profile persistence shared by the server and embedded hosts.
  Borrows the candidate, encodes before touching disk, and closes its complete
  UTF-8 temporary file before renaming the previous profile to a backup. Failure
  never authorizes in-memory publication. A failed final rename attempts rollback;
  if rollback also fails, the previous bytes remain in the backup and a specific
  diagnostic is raised. The owner serializes writers (the backend holds FGuard).
  This is completed file/rename I/O, not a power-loss durability guarantee or a
  cross-process locking protocol. Paths never enter portable designs. }
procedure SaveNyxOutputProfile(const APath: TNyxText;
  AProfile: TNyxOutputConfiguration);

implementation

uses Classes, SysUtils;

procedure SaveNyxOutputProfile(const APath: TNyxText;
  AProfile: TNyxOutputConfiguration);
var
  LText: TNyxText;
  LTemporary: TNyxText;
  LBackup: TNyxText;
  LHadPrevious: Boolean;
  LStream: TFileStream;
begin

  if (APath = '') or (AProfile = nil) then
  begin
    raise EArgumentException.Create('Output persistence requires a path and a complete profile');
  end;
  LText := AProfile.Encode;
  ForceDirectories(ExtractFilePath(APath));
  LTemporary := APath + '.tmp';
  LBackup := APath + '.bak';
  LStream := TFileStream.Create(LTemporary, fmCreate);
  try

    if LText <> '' then
    begin
      LStream.WriteBuffer(LText[1], Length(LText));
    end;
  finally
    LStream.Free;
  end;
  LHadPrevious := FileExists(APath);

  if FileExists(LBackup) and not DeleteFile(LBackup) then
  begin
    raise Exception.Create('Cannot replace the previous output configuration backup');
  end;

  if LHadPrevious and not RenameFile(APath, LBackup) then
  begin
    raise Exception.Create('Cannot preserve the previous output configuration');
  end;

  if not RenameFile(LTemporary, APath) then
  begin

    if LHadPrevious and not RenameFile(LBackup, APath) then
    begin
      raise Exception.Create('Cannot save output configuration; previous bytes remain in its backup');
    end;
    raise Exception.Create('Cannot save output configuration');
  end;

  if LHadPrevious then
  begin
    DeleteFile(LBackup);
  end;
end;

end.
