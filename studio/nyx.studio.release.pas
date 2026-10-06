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

unit nyx.studio.release;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data;

type
  { Native packaging failure. All paths are local tool inputs, never document
    properties or agent-supplied compiler commands. No operation starts a server,
    enrolls credentials, changes projects, overwrites files or deletes a tree. }
  ENyxStudioRelease = class(Exception);

{ Copy the owned compiler sources, the two production HTML hosts, the MIT license
  and the explicitly matched runtime into a NEW directory. Source directories
  are flat and only Pascal units/includes/programs are admitted. Configuration,
  saved projects, tests, dependencies and prior compiler artifacts are excluded.
  Compile backend/editor/worker/preview from this snapshot before sealing it.
  A failed copy leaves an unsealed directory for inspection; it is never reused. }
procedure PrepareNyxStudioRelease(const ARepository, ARuntime, ADestination: TNyxText);

{ Seal a completed snapshot with a sorted complete file inventory, byte lengths
  and MD5 fingerprints. MD5 detects accidental artifact mixing; it is not a
  signature or an authentication claim. Revision is the source checkpoint, not a
  branch name. Compiler values contain versions only, never executable paths.
  Verification runs before publishing the manifest. Existing manifests refuse. }
procedure SealNyxStudioRelease(const ARoot, ARevision, AFPCVersion,
  APas2jsVersion: TNyxText);

{ Read and verify a strict manifest, every file and directory, and the required
  production artifact closure. Returned metadata is an independent immutable
  value. Unexpected files, links/reparse points, duplicate/case-colliding names,
  unsafe paths, missing artifacts or mismatched bytes refuse. No writes occur.
  This local trusted staging tool does not protect against concurrent hostile
  filesystem changes, qualify widget behavior or establish HTTP deployment. }
function VerifyNyxStudioRelease(const ARoot: TNyxText): TNyxDataValue;

implementation

uses
  Classes, MD5
  {$IFDEF MSWINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

const
  CManifestName = 'release.nyx';
  CMaximumFiles = 1024;
  CMaximumFileBytes = 32 * 1024 * 1024;
  CMaximumTotalBytes = 128 * 1024 * 1024;

function RootPath(const APath: TNyxText): TNyxText;
begin
  Result := IncludeTrailingPathDelimiter(ExpandFileName(APath));
end;

procedure RequireOrdinaryPath(const APath: TNyxText);
{$IFDEF MSWINDOWS}
var
  LAttributes: DWORD;
  LWide: UnicodeString;
{$ENDIF}
{$IFDEF UNIX}
var
  LStatus: Stat;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  LWide := UTF8Decode(APath);
  LAttributes := GetFileAttributesW(PWideChar(LWide));

  if (LAttributes = INVALID_FILE_ATTRIBUTES) or
    ((LAttributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0) then
  begin
    raise ENyxStudioRelease.Create('Release path is missing or is a reparse point');
  end;
  {$ENDIF}
  {$IFDEF UNIX}

  if (fpLStat(APath, LStatus) <> 0) or fpS_ISLNK(LStatus.st_mode) then
  begin
    raise ENyxStudioRelease.Create('Release path is missing or is a symbolic link');
  end;
  {$ENDIF}
end;

procedure RequireOrdinaryAncestors(const APath: TNyxText);
var
  LPath: TNyxText;
  LParent: TNyxText;
begin
  LPath := ExcludeTrailingPathDelimiter(ExpandFileName(APath));
  repeat
    RequireOrdinaryPath(LPath);
    LParent := ExcludeTrailingPathDelimiter(ExtractFileDir(LPath));

    if (LParent = '') or (LParent = LPath) then
    begin
      Break;
    end;
    LPath := LParent;
  until False;
end;

function IsSourceName(const AName: TNyxText): Boolean;
var
  LExtension: TNyxText;
  LIndex: Integer;
begin
  LExtension := ExtractFileExt(AName);
  Result := (Copy(AName, 1, 3) = 'nyx') and
    ((LExtension = '.pas') or (LExtension = '.inc') or (LExtension = '.lpr'));
  for LIndex := 1 to Length(AName) do
  begin

    if not (AName[LIndex] in ['a'..'z', '0'..'9', '_', '.']) then
    begin
      Result := False;
    end;
  end;
end;

procedure RequireReleasePath(const APath: TNyxText);
var
  LSlash: Integer;
  LDirectory: TNyxText;
  LName: TNyxText;
begin

  if APath = 'LICENSE' then
  begin
    Exit;
  end;
  LSlash := Pos('/', APath);
  LDirectory := Copy(APath, 1, LSlash - 1);
  LName := Copy(APath, LSlash + 1, MaxInt);

  if ((LDirectory = 'src') or (LDirectory = 'studio')) and IsSourceName(LName) then
  begin
    Exit;
  end;

  if (APath = 'bin/nyx_studio_server.exe') or (APath = 'bin/nyx_studio_server') or
    (APath = 'web/index.html') or (APath = 'web/agent-preview.html') or
    (APath = 'web/rtl.js') or (APath = 'web/nyx_studio.js') or
    (APath = 'web/nyx_source_worker.js') or (APath = 'web/nyx_studio_preview.js') then
  begin
    Exit;
  end;
  raise ENyxStudioRelease.Create('Unexpected release member: ' + APath);
end;

function LocalMember(const ARoot, AMember: TNyxText): TNyxText;
begin
  RequireReleasePath(AMember);
  Result := ARoot + StringReplace(AMember, '/', PathDelim, [rfReplaceAll]);
end;

function FileBytes(const APath: TNyxText): Integer;
var
  LStream: TFileStream;
begin
  RequireOrdinaryPath(APath);
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LStream.Size <= 0) or (LStream.Size > CMaximumFileBytes) then
    begin
      raise ENyxStudioRelease.Create('Release file is empty or exceeds its byte budget');
    end;
    Result := LStream.Size;
  finally
    LStream.Free;
  end;
end;

function FileFingerprint(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LContext: TMD5Context;
  LDigest: TMD5Digest;
  LBuffer: array[0..16383] of Byte;
  LRead: Integer;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    MD5Init(LContext);
    repeat
      LRead := LStream.Read(LBuffer, SizeOf(LBuffer));

      if LRead > 0 then
      begin
        MD5Update(LContext, LBuffer, LRead);
      end;
    until LRead = 0;
    MD5Final(LContext, LDigest);
    Result := MD5Print(LDigest);
  finally
    LStream.Free;
  end;
end;

procedure CopyNewFile(const ASource, ADestination: TNyxText);
var
  LInput: TFileStream;
  LOutput: TFileStream;
begin
  FileBytes(ASource);

  if FileExists(ADestination) or DirectoryExists(ADestination) then
  begin
    raise ENyxStudioRelease.Create('Release member already exists');
  end;
  LInput := TFileStream.Create(ASource, fmOpenRead or fmShareDenyWrite);
  try
    LOutput := TFileStream.Create(ADestination, fmCreate);
    try
      LOutput.CopyFrom(LInput, 0);
    finally
      LOutput.Free;
    end;
  finally
    LInput.Free;
  end;
end;

procedure CopySources(const ASource, ADestination: TNyxText);
var
  LSearch: TSearchRec;
  LCount: Integer;
begin
  RequireOrdinaryAncestors(ASource);
  LCount := 0;

  if FindFirst(ASource + '*', faAnyFile, LSearch) <> 0 then
  begin
    raise ENyxStudioRelease.Create('Compiler source directory is missing');
  end;
  try
    repeat

      if (LSearch.Attr and faDirectory = 0) and IsSourceName(LSearch.Name) then
      begin
        Inc(LCount);

        if LCount > CMaximumFiles div 2 then
        begin
          raise ENyxStudioRelease.Create('Compiler source inventory exceeds its budget');
        end;
        CopyNewFile(ASource + LSearch.Name, ADestination + LSearch.Name);
      end;
    until FindNext(LSearch) <> 0;
  finally
    SysUtils.FindClose(LSearch);
  end;

  if LCount = 0 then
  begin
    raise ENyxStudioRelease.Create('No owned compiler sources were found');
  end;
end;

procedure PrepareNyxStudioRelease(const ARepository, ARuntime, ADestination: TNyxText);
var
  LRepository: TNyxText;
  LDestination: TNyxText;
begin
  LRepository := RootPath(ARepository);
  LDestination := RootPath(ADestination);
  RequireOrdinaryAncestors(LRepository);
  RequireOrdinaryAncestors(ExtractFileDir(ExcludeTrailingPathDelimiter(LDestination)));

  if DirectoryExists(LDestination) or FileExists(ExcludeTrailingPathDelimiter(LDestination)) then
  begin
    raise ENyxStudioRelease.Create('Release destination already exists; choose a new directory');
  end;
  FileBytes(ARuntime);

  if not CreateDir(ExcludeTrailingPathDelimiter(LDestination)) then
  begin
    raise ENyxStudioRelease.Create('Cannot create new release directory');
  end;

  if not CreateDir(LDestination + 'src') or not CreateDir(LDestination + 'studio') or
    not CreateDir(LDestination + 'web') or not CreateDir(LDestination + 'bin') then
  begin
    raise ENyxStudioRelease.Create('Cannot create release layout');
  end;
  CopySources(LRepository + 'src' + PathDelim, LDestination + 'src' + PathDelim);
  CopySources(LRepository + 'studio' + PathDelim, LDestination + 'studio' + PathDelim);
  CopyNewFile(LRepository + 'LICENSE', LDestination + 'LICENSE');
  CopyNewFile(LRepository + 'studio' + PathDelim + 'web' + PathDelim + 'index.html',
    LDestination + 'web' + PathDelim + 'index.html');
  CopyNewFile(LRepository + 'studio' + PathDelim + 'web' + PathDelim + 'agent-preview.html',
    LDestination + 'web' + PathDelim + 'agent-preview.html');
  CopyNewFile(ARuntime, LDestination + 'web' + PathDelim + 'rtl.js');
end;

function CompareReleaseMembers(AFiles: TStringList; ALeft, ARight: Integer): Integer;
begin
  { TStringList.Sort can use host collation: punctuation in dotted unit names
    and underscore program names then disagrees with Pascal ordinal comparisons.
    The release wire order uses byte/ordinal ASCII on every host. }
  Result := 0;

  if AFiles[ALeft] < AFiles[ARight] then
  begin
    Result := -1;
  end
  else if AFiles[ALeft] > AFiles[ARight] then
  begin
    Result := 1;
  end;
end;

function Inventory(const ARoot: TNyxText): TStringList;
var
  LSearch: TSearchRec;
  LMemberSearch: TSearchRec;
  LName: TNyxText;
  LDirectory: TNyxText;
  LMember: TNyxText;
begin
  Result := TStringList.Create;
  { All admitted file names are ASCII. This RTL list stores no user text. Exact
    ordinal order makes manifests repeatable across source enumeration order. }
  Result.CaseSensitive := True;
  try
    RequireOrdinaryAncestors(ARoot);

    if FindFirst(ARoot + '*', faAnyFile, LSearch) <> 0 then
    begin
      raise ENyxStudioRelease.Create('Release directory is missing');
    end;
    try
      repeat
        LName := LSearch.Name;

        if (LName = '.') or (LName = '..') then
        begin
          Continue;
        end;
        RequireOrdinaryPath(ARoot + LName);

        if LSearch.Attr and faDirectory = 0 then
        begin

          if LName <> CManifestName then
          begin
            RequireReleasePath(LName);
            Result.Add(LName);
          end;
          Continue;
        end;

        if (LName <> 'src') and (LName <> 'studio') and
          (LName <> 'web') and (LName <> 'bin') then
        begin
          raise ENyxStudioRelease.Create('Unexpected release directory: ' + LName);
        end;
        LDirectory := ARoot + LName + PathDelim;

        if FindFirst(LDirectory + '*', faAnyFile, LMemberSearch) = 0 then
        begin
          try
            repeat

              if (LMemberSearch.Name = '.') or (LMemberSearch.Name = '..') then
              begin
                Continue;
              end;
              LMember := LName + '/' + LMemberSearch.Name;
              RequireReleasePath(LMember);
              RequireOrdinaryPath(LocalMember(ARoot, LMember));

              if LMemberSearch.Attr and faDirectory <> 0 then
              begin
                raise ENyxStudioRelease.Create('Nested release directories are not admitted');
              end;
              Result.Add(LMember);

              if Result.Count > CMaximumFiles then
              begin
                raise ENyxStudioRelease.Create('Release inventory exceeds its file budget');
              end;
            until FindNext(LMemberSearch) <> 0;
          finally
            SysUtils.FindClose(LMemberSearch);
          end;
        end;
      until FindNext(LSearch) <> 0;
    finally
      SysUtils.FindClose(LSearch);
    end;
    Result.CustomSort(CompareReleaseMembers);
  except
    Result.Free;
    raise;
  end;
end;

procedure RequireClosure(AFiles: TStringList);
const
  CRequired: array[0..12] of String = (
    'LICENSE', 'web/index.html', 'web/agent-preview.html', 'web/rtl.js',
    'web/nyx_studio.js', 'web/nyx_source_worker.js', 'web/nyx_studio_preview.js',
    'src/nyx.model.pas', 'src/nyx.content.pas', 'src/nyx.content.editor.pas',
    'studio/nyx_studio_server.lpr', 'studio/nyx_studio.lpr', 'studio/nyx_source_worker.lpr');
var
  LIndex: Integer;
begin
  for LIndex := Low(CRequired) to High(CRequired) do
  begin

    if AFiles.IndexOf(CRequired[LIndex]) < 0 then
    begin
      raise ENyxStudioRelease.Create('Required release artifact is missing: ' + CRequired[LIndex]);
    end;
  end;

  if (AFiles.IndexOf('bin/nyx_studio_server.exe') < 0) =
    (AFiles.IndexOf('bin/nyx_studio_server') < 0) then
  begin
    raise ENyxStudioRelease.Create('Release must contain exactly one backend executable');
  end;
end;

procedure RequireHex(const AValue: TNyxText; ALength: Integer);
var
  LIndex: Integer;
begin

  if Length(AValue) <> ALength then
  begin
    raise ENyxStudioRelease.Create('Invalid release fingerprint or revision');
  end;
  for LIndex := 1 to ALength do
  begin

    if not (AValue[LIndex] in ['0'..'9', 'a'..'f']) then
    begin
      raise ENyxStudioRelease.Create('Invalid release fingerprint or revision');
    end;
  end;
end;

procedure RequireVersion(const AValue: TNyxText);
var
  LIndex: Integer;
begin

  if (Length(AValue) < 3) or (Length(AValue) > 32) then
  begin
    raise ENyxStudioRelease.Create('Compiler version must be a bounded version label');
  end;
  for LIndex := 1 to Length(AValue) do
  begin

    if not (AValue[LIndex] in ['0'..'9', 'a'..'z', 'A'..'Z', '.', '-', '+']) then
    begin
      raise ENyxStudioRelease.Create('Compiler version cannot contain a machine path');
    end;
  end;
end;

procedure VerifyManifest(const ARoot: TNyxText; const AManifest: TNyxDataValue);
var
  LFiles: TStringList;
  LEntries: TNyxDataValue;
  LEntry: TNyxDataValue;
  LCompilers: TNyxDataValue;
  LPath: TNyxText;
  LLastPath: TNyxText;
  LIndex: Integer;
  LBytes: Integer;
  LTotal: Int64;
begin

  if (AManifest.Kind <> ndObject) or (AManifest.Count <> 4) or
    (AManifest.Field('version').AsInteger <> 1) then
  begin
    raise ENyxStudioRelease.Create('Unsupported release manifest');
  end;
  RequireHex(AManifest.Field('revision').AsText, 40);
  LCompilers := AManifest.Field('compilers');

  if (LCompilers.Kind <> ndObject) or (LCompilers.Count <> 2) then
  begin
    raise ENyxStudioRelease.Create('Invalid compiler version metadata');
  end;
  RequireVersion(LCompilers.Field('fpc').AsText);
  RequireVersion(LCompilers.Field('pas2js').AsText);
  LEntries := AManifest.Field('files');

  if (LEntries.Kind <> ndArray) or (LEntries.Count > CMaximumFiles) then
  begin
    raise ENyxStudioRelease.Create('Invalid release file inventory');
  end;
  LFiles := Inventory(ARoot);
  try
    RequireClosure(LFiles);

    if LEntries.Count <> LFiles.Count then
    begin
      raise ENyxStudioRelease.Create('Release inventory differs from the actual files');
    end;
    LTotal := 0;
    LLastPath := '';
    for LIndex := 0 to LEntries.Count - 1 do
    begin
      LEntry := LEntries.Item(LIndex);

      if (LEntry.Kind <> ndObject) or (LEntry.Count <> 3) then
      begin
        raise ENyxStudioRelease.Create('Invalid release file entry');
      end;
      LPath := LEntry.Field('path').AsText;
      RequireReleasePath(LPath);

      if (LPath <> LFiles[LIndex]) or (LPath <= LLastPath) then
      begin
        raise ENyxStudioRelease.Create('Release paths must be exact, unique and sorted');
      end;
      LLastPath := LPath;
      LBytes := FileBytes(LocalMember(ARoot, LPath));
      Inc(LTotal, LBytes);

      if (LTotal > CMaximumTotalBytes) or (LBytes <> LEntry.Field('bytes').AsInteger) then
      begin
        raise ENyxStudioRelease.Create('Release byte lengths differ or exceed their budget');
      end;
      RequireHex(LEntry.Field('md5').AsText, 32);

      if FileFingerprint(LocalMember(ARoot, LPath)) <> LEntry.Field('md5').AsText then
      begin
        raise ENyxStudioRelease.Create('Release fingerprint differs: ' + LPath);
      end;
    end;
  finally
    LFiles.Free;
  end;
end;

procedure SealNyxStudioRelease(const ARoot, ARevision, AFPCVersion,
  APas2jsVersion: TNyxText);
var
  LRoot: TNyxText;
  LFiles: TStringList;
  LItems: array of TNyxDataValue;
  LManifest: TNyxDataValue;
  LText: TNyxText;
  LIndex: Integer;
  LOutput: TFileStream;
begin
  LRoot := RootPath(ARoot);

  if FileExists(LRoot + CManifestName) then
  begin
    raise ENyxStudioRelease.Create('Release is already sealed');
  end;
  LFiles := Inventory(LRoot);
  try
    RequireClosure(LFiles);
    SetLength(LItems, LFiles.Count);
    for LIndex := 0 to LFiles.Count - 1 do
    begin
      LItems[LIndex] := NyxObject([
        NyxField('path', NyxData(TNyxText(LFiles[LIndex]))),
        NyxField('bytes', NyxData(FileBytes(LocalMember(LRoot, LFiles[LIndex])))),
        NyxField('md5', NyxData(FileFingerprint(LocalMember(LRoot, LFiles[LIndex]))))]);
    end;
    LManifest := NyxObject([
      NyxField('version', NyxData(1)),
      NyxField('revision', NyxData(ARevision)),
      NyxField('compilers', NyxObject([
        NyxField('fpc', NyxData(AFPCVersion)),
        NyxField('pas2js', NyxData(APas2jsVersion))])),
      NyxField('files', NyxArray(LItems))]);
    VerifyManifest(LRoot, LManifest);
    LText := LManifest.ToJSON;
    LOutput := TFileStream.Create(LRoot + CManifestName, fmCreate);
    try
      LOutput.WriteBuffer(LText[1], Length(LText));
    finally
      LOutput.Free;
    end;
  finally
    LFiles.Free;
  end;
end;

function VerifyNyxStudioRelease(const ARoot: TNyxText): TNyxDataValue;
var
  LRoot: TNyxText;
  LInput: TFileStream;
  LText: TNyxText;
begin
  LRoot := RootPath(ARoot);
  RequireOrdinaryAncestors(LRoot);
  RequireOrdinaryPath(LRoot + CManifestName);
  LInput := TFileStream.Create(LRoot + CManifestName, fmOpenRead or fmShareDenyWrite);
  try

    if (LInput.Size <= 0) or (LInput.Size > 512 * 1024) then
    begin
      raise ENyxStudioRelease.Create('Release manifest exceeds its byte budget');
    end;
    SetLength(LText, LInput.Size);
    LInput.ReadBuffer(LText[1], Length(LText));
  finally
    LInput.Free;
  end;
  Result := TNyxDataValue.ParseJSON(LText);
  VerifyManifest(LRoot, Result);
end;

end.
