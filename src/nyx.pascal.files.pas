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
unit nyx.pascal.files;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.pascal.context;

type
  { Captured virtual-file comparison is explicit, independent of the machine
    running analysis. ASCII-insensitive preserves all non-ASCII spelling exactly. }
  TNyxPascalFileCase = (pfcExact, pfcASCIIInsensitive);
  { A normalized relative virtual path, distinct from disk paths or source refs.
    Slash components cannot be empty, dot, parent, control, drive or root aliases.
    SDK and project files share this data contract without admission authority. }
  TNyxPascalPhysicalFile = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  { Immutable captured file/search catalog. No disk fallback or host enumeration
    occurs. Every text is exact validated UTF-8/Unicode, up to four MiB per file;
    at most 4096 files/256 MiB and 256 explicit include directories are retained.
    Persistent changes detach vectors and keep earlier texts/search order exact. }
  INyxPascalFileCatalog = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C02}']
    function CasePolicy: TNyxPascalFileCase;
    function Count: Integer;
    function FileAt(AIndex: Integer): TNyxPascalPhysicalFile;
    function TextAt(AIndex: Integer): TNyxText;
    function ByteCount: Integer;
    function Find(const AFile: TNyxPascalPhysicalFile): Integer;
    { Add or replace one captured text; this never reads or writes its name. }
    function WithFile(const AFile: TNyxPascalPhysicalFile;
      const AText: TNyxText): INyxPascalFileCatalog;
    { Root is empty; other directory spellings follow normalized virtual paths.
      Caller supplies actual search order; current physical directory is first. }
    function WithIncludeDirectory(const ADirectory: TNyxText): INyxPascalFileCatalog;
    function DirectoryCount: Integer;
    function DirectoryAt(AIndex: Integer): TNyxText;
    { Resolve only captured names. Dot/parent segments normalize within the
      virtual root; escaping that root and backslash/absolute aliases refuse.
      Missing files return -1. No unit path or filesystem is searched implicitly. }
    function ResolveInclude(const AFrom: TNyxPascalPhysicalFile;
      const AName: TNyxText): Integer;
    function SameFiles(const AOther: INyxPascalFileCatalog): Boolean;
  end;

{ Validate a bounded 1024-scalar relative virtual file spelling. No I/O occurs. }
function NyxPascalPhysicalFile(const AName: TNyxText): TNyxPascalPhysicalFile; overload;
{ Empty capture with explicit file case policy. A complete source set and include
  search directories must be supplied by the host, never inferred from disk. }
function NewNyxPascalFileCatalog(ACase: TNyxPascalFileCase): INyxPascalFileCatalog;
{ Freeze alternate catalogs once. Caller serializes mutable providers during
  capture; subsequent reads retain only owned data and exact search order. }
function CaptureNyxPascalFiles(const AFiles: INyxPascalFileCatalog): INyxPascalFileCatalog; overload;
implementation

uses SysUtils, nyx.bytes, nyx.text.index, nyx.pascal.lexing;

type
  { Copied virtual-file payload; managed text only, with no interfaces in records. }
  TPhysicalEntry = record
    FileReference: TNyxPascalPhysicalFile;
    Text: TNyxText;
    Bytes: Integer;
  end;
  { Owns detached file/search vectors and an exact immutable lookup index.
    There are no callbacks or borrowed source providers after construction. }
  TFileCatalog = class(TInterfacedObject, INyxPascalFileCatalog)
  private
    FCase: TNyxPascalFileCase;
    FFiles: array of TPhysicalEntry;
    FDirectories: array of TNyxText;
    FNames: TNyxTextIndex;
    FBytes: Integer;
    function Key(const AName: TNyxText): TNyxText;
    function Clone: TFileCatalog;
  public
    constructor Create(ACase: TNyxPascalFileCase);
    destructor Destroy; override;
    function CasePolicy: TNyxPascalFileCase;
    function Count: Integer;
    function FileAt(AIndex: Integer): TNyxPascalPhysicalFile;
    function TextAt(AIndex: Integer): TNyxText;
    function ByteCount: Integer;
    function Find(const AFile: TNyxPascalPhysicalFile): Integer;
    function WithFile(const AFile: TNyxPascalPhysicalFile;
      const AText: TNyxText): INyxPascalFileCatalog;
    function WithIncludeDirectory(const ADirectory: TNyxText): INyxPascalFileCatalog;
    function DirectoryCount: Integer;
    function DirectoryAt(AIndex: Integer): TNyxText;
    function ResolveInclude(const AFrom: TNyxPascalPhysicalFile;
      const AName: TNyxText): Integer;
    function SameFiles(const AOther: INyxPascalFileCatalog): Boolean;
  end;

procedure CheckPathText(const AName: TNyxText);
var
  LAt: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  LAt := 1;
  LCount := 0;
  while LAt <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LAt, LScalar) then
    begin
      raise ENyxPascalContext.Create('A virtual source path contains malformed Unicode');
    end;
    Inc(LCount);

    if (LCount > 1024) or (LScalar < 32) or
      (LScalar in [Ord(':'), Ord('\'), 127]) then
    begin
      raise ENyxPascalContext.Create('A virtual source path contains a host alias or exceeds its bound');
    end;
  end;
end;

function NyxPascalPhysicalFile(const AName: TNyxText): TNyxPascalPhysicalFile;
var
  LAt: Integer;
  LStart: Integer;
  LPart: TNyxText;
begin
  CheckPathText(AName);

  if (AName = '') or (AName[1] = '/') then
  begin
    raise ENyxPascalContext.Create('A captured source needs a relative virtual filename');
  end;
  LStart := 1;
  for LAt := 1 to Length(AName) + 1 do
  begin

    if (LAt > Length(AName)) or (AName[LAt] = '/') then
    begin
      LPart := Copy(AName, LStart, LAt - LStart);

      if (LPart = '') or (LPart = '.') or (LPart = '..') then
      begin
        raise ENyxPascalContext.Create('A captured source filename must already be normalized');
      end;
      LStart := LAt + 1;
    end;
  end;
  Result.FName := AName;
end;

constructor TFileCatalog.Create(ACase: TNyxPascalFileCase);
begin
  inherited Create;

  if (Ord(ACase) < Ord(Low(TNyxPascalFileCase))) or
    (Ord(ACase) > Ord(High(TNyxPascalFileCase))) then
  begin
    raise ENyxPascalContext.Create('Captured file case policy is outside its closed choices');
  end;
  FCase := ACase;
  FNames := TNyxTextIndex.Create;
end;

destructor TFileCatalog.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

function TFileCatalog.Key(const AName: TNyxText): TNyxText;
begin
  Result := AName;

  if FCase = pfcASCIIInsensitive then
  begin
    Result := NyxPascalASCIIKey(AName);
  end;
end;

function TFileCatalog.Clone: TFileCatalog;
var
  LAt: Integer;
begin
  Result := TFileCatalog.Create(FCase);
  try
    Result.FBytes := FBytes;
    SetLength(Result.FFiles, Length(FFiles));
    for LAt := 0 to High(FFiles) do
    begin
      Result.FFiles[LAt] := FFiles[LAt];
      Result.FNames.AddFirst(Key(FFiles[LAt].FileReference.Name), LAt);
    end;
    SetLength(Result.FDirectories, Length(FDirectories));
    for LAt := 0 to High(FDirectories) do
    begin
      Result.FDirectories[LAt] := FDirectories[LAt];
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TFileCatalog.CasePolicy: TNyxPascalFileCase;
begin
  Result := FCase;
end;

function TFileCatalog.Count: Integer;
begin
  Result := Length(FFiles);
end;

function TFileCatalog.FileAt(AIndex: Integer): TNyxPascalPhysicalFile;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalContext.Create('Captured source index is outside its catalog');
  end;
  Result := FFiles[AIndex].FileReference;
end;

function TFileCatalog.TextAt(AIndex: Integer): TNyxText;
begin
  FileAt(AIndex);
  Result := FFiles[AIndex].Text;
end;

function TFileCatalog.ByteCount: Integer;
begin
  Result := FBytes;
end;

function TFileCatalog.Find(const AFile: TNyxPascalPhysicalFile): Integer;
begin
  NyxPascalPhysicalFile(AFile.Name);
  Result := FNames.IndexOf(Key(AFile.Name));
end;

function TFileCatalog.WithFile(const AFile: TNyxPascalPhysicalFile;
  const AText: TNyxText): INyxPascalFileCatalog;
var
  LAt: Integer;
  LBytes: Integer;
  LTotal: Integer;
  LCopy: TFileCatalog;
begin
  LAt := Find(AFile);
  LBytes := NyxUTF8ByteCount(AText);

  if LBytes > 4 * 1024 * 1024 then
  begin
    raise ENyxPascalContext.Create('A captured physical source exceeds four MiB');
  end;

  if (LAt < 0) and (Count >= 4096) then
  begin
    raise ENyxPascalContext.Create('A captured source catalog exceeds 4096 files');
  end;
  LTotal := FBytes;

  if LAt >= 0 then
  begin
    LTotal := LTotal - FFiles[LAt].Bytes;

    if FFiles[LAt].FileReference.Name <> AFile.Name then
    begin
      raise ENyxPascalContext.Create('A captured source case alias is ambiguous');
    end;
  end;

  if LBytes > 256 * 1024 * 1024 - LTotal then
  begin
    raise ENyxPascalContext.Create('A captured source catalog exceeds 256 MiB');
  end;
  LCopy := Clone;
  Result := LCopy;

  if LAt < 0 then
  begin
    LAt := Length(LCopy.FFiles);
    SetLength(LCopy.FFiles, LAt + 1);
    LCopy.FNames.AddFirst(Key(AFile.Name), LAt);
  end;
  LCopy.FFiles[LAt].FileReference := AFile;
  LCopy.FFiles[LAt].Text := AText;
  LCopy.FFiles[LAt].Bytes := LBytes;
  LCopy.FBytes := LTotal + LBytes;
end;

function TFileCatalog.WithIncludeDirectory(const ADirectory: TNyxText): INyxPascalFileCatalog;
var
  LAt: Integer;
  LCopy: TFileCatalog;
begin

  if ADirectory <> '' then
  begin
    NyxPascalPhysicalFile(ADirectory + '/directory-probe');
  end;
  for LAt := 0 to High(FDirectories) do
  begin

    if Key(FDirectories[LAt]) = Key(ADirectory) then
    begin
      Exit(Self);
    end;
  end;

  if Length(FDirectories) >= 256 then
  begin
    raise ENyxPascalContext.Create('Captured include directories exceed 256 entries');
  end;
  LCopy := Clone;
  Result := LCopy;
  LAt := Length(LCopy.FDirectories);
  SetLength(LCopy.FDirectories, LAt + 1);
  LCopy.FDirectories[LAt] := ADirectory;
end;

function TFileCatalog.DirectoryCount: Integer;
begin
  Result := Length(FDirectories);
end;

function TFileCatalog.DirectoryAt(AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= DirectoryCount) then
  begin
    raise ENyxPascalContext.Create('Captured include directory index is outside its catalog');
  end;
  Result := FDirectories[AIndex];
end;

function ParentDirectory(const AName: TNyxText): TNyxText;
var
  LAt: Integer;
begin
  LAt := Length(AName);
  while (LAt > 0) and (AName[LAt] <> '/') do
  begin
    Dec(LAt);
  end;
  Result := Copy(AName, 1, LAt - 1);
end;

function ResolvePath(const ADirectory, AName: TNyxText;
  out AFile: TNyxPascalPhysicalFile): Boolean;
var
  LParts: TNyxStrings;
  LText: TNyxText;
  LPart: TNyxText;
  LAt: Integer;
  LStart: Integer;
begin
  CheckPathText(AName);

  if (AName = '') or (AName[1] = '/') then
  begin
    raise ENyxPascalContext.Create('An include needs a relative virtual filename');
  end;
  LText := AName;

  if ADirectory <> '' then
  begin
    LText := ADirectory + '/' + AName;
  end;
  LParts := TNyxStrings.Create;
  try
    LStart := 1;
    for LAt := 1 to Length(LText) + 1 do
    begin

      if (LAt > Length(LText)) or (LText[LAt] = '/') then
      begin
        LPart := Copy(LText, LStart, LAt - LStart);

        if LPart = '..' then
        begin

          if LParts.Count = 0 then
          begin
            Exit(False);
          end;
          LParts.Delete(LParts.Count - 1);
        end
        else if (LPart <> '.') and (LPart <> '') then
        begin
          LParts.Add(LPart);
        end;
        LStart := LAt + 1;
      end;
    end;
    AFile := NyxPascalPhysicalFile(LParts.Join('/'));
    Result := True;
  finally
    LParts.Free;
  end;
end;

function TFileCatalog.ResolveInclude(const AFrom: TNyxPascalPhysicalFile;
  const AName: TNyxText): Integer;
var
  LAt: Integer;
  LFile: TNyxPascalPhysicalFile;
  LContained: Boolean;
begin
  NyxPascalPhysicalFile(AFrom.Name);
  Result := -1;
  LContained := ResolvePath(ParentDirectory(AFrom.Name), AName, LFile);

  if LContained then
  begin
    Result := Find(LFile);
  end;

  if Result >= 0 then
  begin
    Exit;
  end;
  for LAt := 0 to High(FDirectories) do
  begin

    if not ResolvePath(FDirectories[LAt], AName, LFile) then
    begin
      Continue;
    end;
    LContained := True;
    Result := Find(LFile);

    if Result >= 0 then
    begin
      Exit;
    end;
  end;

  if not LContained then
  begin
    raise ENyxPascalContext.Create('An include escapes every captured virtual search root');
  end;
end;

function TFileCatalog.SameFiles(const AOther: INyxPascalFileCatalog): Boolean;
var
  LAt: Integer;
begin
  Result := False;

  if (AOther = nil) or (AOther.CasePolicy <> FCase) or (AOther.Count <> Count) or
    (AOther.DirectoryCount <> DirectoryCount) or (AOther.ByteCount <> FBytes) then
  begin
    Exit;
  end;
  for LAt := 0 to Count - 1 do
  begin

    if (AOther.FileAt(LAt).Name <> FFiles[LAt].FileReference.Name) or
      (AOther.TextAt(LAt) <> FFiles[LAt].Text) then
    begin
      Exit;
    end;
  end;
  for LAt := 0 to DirectoryCount - 1 do
  begin

    if AOther.DirectoryAt(LAt) <> FDirectories[LAt] then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function NewNyxPascalFileCatalog(ACase: TNyxPascalFileCase): INyxPascalFileCatalog;
begin
  Result := TFileCatalog.Create(ACase);
end;

function CaptureNyxPascalFiles(const AFiles: INyxPascalFileCatalog): INyxPascalFileCatalog;
var
  LAt: Integer;
begin

  if AFiles = nil then
  begin
    raise ENyxPascalContext.Create('Source analysis requires a captured file catalog');
  end;

  if (AFiles.Count < 0) or (AFiles.Count > 4096) or
    (AFiles.DirectoryCount < 0) or (AFiles.DirectoryCount > 256) then
  begin
    raise ENyxPascalContext.Create('An alternate file catalog exceeds its bounds');
  end;
  Result := NewNyxPascalFileCatalog(AFiles.CasePolicy);
  for LAt := 0 to AFiles.Count - 1 do
  begin

    if Result.Find(AFiles.FileAt(LAt)) >= 0 then
    begin
      raise ENyxPascalContext.Create('An alternate file catalog contains duplicate names');
    end;
    Result := Result.WithFile(AFiles.FileAt(LAt), AFiles.TextAt(LAt));
  end;
  for LAt := 0 to AFiles.DirectoryCount - 1 do
  begin
    Result := Result.WithIncludeDirectory(AFiles.DirectoryAt(LAt));
  end;
end;

end.
