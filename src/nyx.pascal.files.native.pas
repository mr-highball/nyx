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
unit nyx.pascal.files.native;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.modules, nyx.pascal.translation;

type
  { Copied native capture policy, separate from application compilation and
    portable source admission. Prefix is virtual; the real directory is supplied
    only to Capture. Budgets include seed files, not just newly read entries.
    Recursion is explicit and bounded; links inside the directory refuse. }
  TNyxPascalDirectoryOptions = record
  private
    FDefined: Boolean;
    FPrefix: TNyxText;
    FRecursive: Boolean;
    FFiles: Integer;
    FBytes: Integer;
    FDepth: Integer;
    FHeader: Integer;
    FEntries: Integer;
    FTranslation: TNyxPascalTranslationOptions;
  public
    class function Defaults: TNyxPascalDirectoryOptions; static;
    function Under(const APrefix: TNyxText): TNyxPascalDirectoryOptions;
    function Recursively(AValue: Boolean = True): TNyxPascalDirectoryOptions;
    function UpToFiles(AValue: Integer): TNyxPascalDirectoryOptions;
    function UpToBytes(AValue: Integer): TNyxPascalDirectoryOptions;
    function UpToDepth(AValue: Integer): TNyxPascalDirectoryOptions;
    function HeaderScalars(AValue: Integer): TNyxPascalDirectoryOptions;
    function UpToDirectoryEntries(AValue: Integer): TNyxPascalDirectoryOptions;
    function WithTranslation(const AValue: TNyxPascalTranslationOptions): TNyxPascalDirectoryOptions;
    procedure Validate;
  end;
  { Native read result owns frozen files/index and explicit unindexed roots.
    A .pas/.pp file without a recognizable bounded unit header stays captured
    but is not guessed from its filename. Includes never become independent units.
    No private host path, open handle or mutable directory survives capture. }
  INyxPascalDirectoryCapture = interface(IInterface)
    ['{2D65A080-0690-44CD-887C-35AD8B0C7003}']
    function Files: INyxPascalFileCatalog;
    function Modules: INyxPascalModuleCatalog;
    function UnindexedCount: Integer;
    function Unindexed(AIndex: Integer): TNyxPascalPhysicalFile;
  end;

{ Read only .pas/.pp/.inc from an explicit native directory. Byte streams decode
  strict UTF-8 without changing global codepages or authored text. Alternate
  encodings refuse and require an explicit future encoding contract. Windows
  opens Unicode paths with a read lease denying writers; other native hosts use
  their byte-path API. Each file is stable during its read; the caller serializes
  SDK edits across this multi-file capture. Cancellation/budget/IO failure
  publishes no partial capture. Optional unit facts are frozen, never inferred
  from the analyzer's own build; nil inherits the completion profile. }
function CaptureNyxPascalSourceDirectory(const ADirectory: TNyxText;
  const AFiles: INyxPascalFileCatalog; const AModules: INyxPascalModuleCatalog;
  const AOptions: TNyxPascalDirectoryOptions;
  const AContext: INyxPascalCompilerContext = nil;
  const ACancellation: INyxCancellationScope = nil): INyxPascalDirectoryCapture;

implementation

uses Classes, SysUtils, LazFileUtils, nyx.bytes, nyx.pascal.lexing
  {$ifdef windows}, Windows{$endif};

type
  { Private synchronous read owner. Fields are populated before publication;
    cancellation/facts and host directory borrows end with Build. Returned
    interfaces retain only portable frozen data and unindexed virtual leaves. }
  TDirectoryCapture = class(TInterfacedObject, INyxPascalDirectoryCapture)
  private
    FFiles: INyxPascalFileCatalog;
    FModules: INyxPascalModuleCatalog;
    FUnindexed: array of TNyxPascalPhysicalFile;
    FOptions: TNyxPascalDirectoryOptions;
    FVisited: Integer;
    procedure Check(const ACancellation: INyxCancellationScope);
    function Read(const APath: TNyxText; const ACancellation: INyxCancellationScope): TNyxText;
    function Header(const AText: TNyxText; const ACancellation: INyxCancellationScope): TNyxText;
    procedure SortNames(ANames: TNyxStrings; const ACancellation: INyxCancellationScope);
    procedure Walk(const ADirectory, ARelative: TNyxText; ADepth: Integer;
      const AContext: INyxPascalCompilerContext; const ACancellation: INyxCancellationScope);
  public
    procedure Build(const ADirectory: TNyxText; const AFiles: INyxPascalFileCatalog;
      const AModules: INyxPascalModuleCatalog; const AOptions: TNyxPascalDirectoryOptions;
      const AContext: INyxPascalCompilerContext; const ACancellation: INyxCancellationScope);
    function Files: INyxPascalFileCatalog;
    function Modules: INyxPascalModuleCatalog;
    function UnindexedCount: Integer;
    function Unindexed(AIndex: Integer): TNyxPascalPhysicalFile;
  end;

class function TNyxPascalDirectoryOptions.Defaults: TNyxPascalDirectoryOptions;
begin
  Result.FDefined := True;
  Result.FPrefix := '';
  Result.FRecursive := False;
  Result.FFiles := 4096;
  Result.FBytes := 256 * 1024 * 1024;
  Result.FDepth := 32;
  Result.FHeader := 8192;
  Result.FEntries := 65536;
  Result.FTranslation := TNyxPascalTranslationOptions.Defaults;
end;

procedure TNyxPascalDirectoryOptions.Validate;
begin

  if not FDefined or (FFiles < 1) or (FFiles > 4096) or
    (FBytes < 1) or (FBytes > 256 * 1024 * 1024) or (FDepth < 1) or
    (FHeader < 1) or (FHeader > 1024 * 1024) or (FEntries < 1) then
  begin
    raise ENyxPascalContext.Create('Use positive bounded directory capture limits');
  end;

  if FPrefix <> '' then
  begin
    NyxPascalPhysicalFile(FPrefix);
  end;
  FTranslation.Validate;
end;

function TNyxPascalDirectoryOptions.Under(const APrefix: TNyxText): TNyxPascalDirectoryOptions;
begin
  Result := Self;
  Result.FPrefix := APrefix;
  Result.Validate;
end;

function TNyxPascalDirectoryOptions.Recursively(AValue: Boolean): TNyxPascalDirectoryOptions;
begin
  Result := Self;
  Result.FRecursive := AValue;
  Result.Validate;
end;

function TNyxPascalDirectoryOptions.UpToFiles(AValue: Integer): TNyxPascalDirectoryOptions;
begin
  Result := Self;
  Result.FFiles := AValue;
  Result.Validate;
end;

function TNyxPascalDirectoryOptions.UpToBytes(AValue: Integer): TNyxPascalDirectoryOptions;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function TNyxPascalDirectoryOptions.UpToDepth(AValue: Integer): TNyxPascalDirectoryOptions;
begin
  Result := Self;
  Result.FDepth := AValue;
  Result.Validate;
end;

function TNyxPascalDirectoryOptions.HeaderScalars(AValue: Integer): TNyxPascalDirectoryOptions;
begin
  Result := Self;
  Result.FHeader := AValue;
  Result.Validate;
end;

function TNyxPascalDirectoryOptions.WithTranslation(
  const AValue: TNyxPascalTranslationOptions): TNyxPascalDirectoryOptions;
begin
  Result := Self;
  Result.FTranslation := AValue;
  Result.Validate;
end;

function TNyxPascalDirectoryOptions.UpToDirectoryEntries(AValue: Integer): TNyxPascalDirectoryOptions;
begin
  Result := Self;
  Result.FEntries := AValue;
  Result.Validate;
end;

procedure TDirectoryCapture.Check(const ACancellation: INyxCancellationScope);
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Native source capture cancelled');
  end;
end;

function TDirectoryCapture.Read(const APath: TNyxText;
  const ACancellation: INyxCancellationScope): TNyxText;
var
  LHandle: THandle;
  LStream: THandleStream;
  LBytes: TNyxBytes;
  LAt: Integer;
  LCount: Integer;
  LSize: Int64;
  LExtra: Byte;
begin
  Check(ACancellation);
  {$ifdef windows}
  { CreateFileW borrows the decoded path only during this call. The owned handle
    denies concurrent writers and closes after the borrowed THandleStream retires. }
  LHandle := CreateFileW(PWideChar(UTF8Decode(APath)), GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  {$else}
  LHandle := FileOpen(String(RawByteString(APath)), fmOpenRead or fmShareDenyWrite);
  {$endif}

  if LHandle = THandle(-1) then
  begin
    raise ENyxPascalContext.Create('A configured SDK source could not be opened');
  end;
  LStream := nil;
  try
    LStream := THandleStream.Create(LHandle);
    LSize := LStream.Size;

    if (LSize < 0) or (LSize > 4 * 1024 * 1024) then
    begin
      raise ENyxPascalContext.Create('A captured SDK source exceeds the four MiB file limit');
    end;
    SetLength(LBytes, Integer(LSize));
    LAt := 0;
    while LAt < Length(LBytes) do
    begin
      Check(ACancellation);
      LCount := Length(LBytes) - LAt;

      if LCount > 65536 then
      begin
        LCount := 65536;
      end;
      LCount := LStream.Read(LBytes[LAt], LCount);

      if LCount <= 0 then
      begin
        raise ENyxPascalContext.Create('The configured source changed during capture');
      end;
      Inc(LAt, LCount);
    end;

    if (LStream.Read(LExtra, 1) <> 0) or (LStream.Size <> LSize) then
    begin
      raise ENyxPascalContext.Create('The configured source changed during capture');
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
    FileClose(LHandle);
  end;
end;

function TDirectoryCapture.Header(const AText: TNyxText;
  const ACancellation: INyxCancellationScope): TNyxText;
var
  LLex: INyxPascalLexicalSnapshot;
  LAt: Integer;
  LOffset: Integer;
  LCount: Integer;
  LScalar: Integer;

  procedure Skip;
  begin
    while (LAt < LLex.Count) and
      (LLex.Token(LAt).Kind in [ptkWhitespace, ptkComment, ptkDirective]) do
    begin
      Inc(LAt);
    end;
  end;

begin
  LOffset := 1;
  LCount := 0;
  while (LOffset <= Length(AText)) and (LCount < FOptions.FHeader) do
  begin

    if (LCount mod 1024) = 0 then
    begin
      Check(ACancellation);
    end;
    NyxNextScalar(AText, LOffset, LScalar);
    Inc(LCount);
  end;
  LLex := NewNyxPascalLexicalSnapshot(Copy(AText, 1, LOffset - 1),
    TNyxPascalLexicalOptions.Defaults, ACancellation);
  LAt := 0;
  Skip;
  Result := '';

  if (LLex.Token(LAt).Word <> pkwUnit) or LLex.Token(LAt).Escaped then
  begin
    Exit;
  end;
  Inc(LAt);
  Skip;
  while LAt < LLex.Count do
  begin

    if LLex.Token(LAt).Kind <> ptkWord then
    begin
      Exit('');
    end;
    Result := Result + LLex.Identifier(LAt);
    Inc(LAt);
    Skip;

    if LLex.Token(LAt).Symbol = psoSemicolon then
    begin
      NyxPascalModule(Result);
      Exit;
    end;

    if LLex.Token(LAt).Symbol <> psoDot then
    begin
      Exit('');
    end;
    Result := Result + '.';
    Inc(LAt);
    Skip;
  end;
  Result := '';
end;

procedure TDirectoryCapture.SortNames(ANames: TNyxStrings;
  const ACancellation: INyxCancellationScope);
var
  LStart: Integer;
  LEnd: Integer;
  LSwap: TNyxText;

  { Iterative heap descent bounds stack and comparison work. TNyxText stays in
    owned storage throughout; no ANSI RTL sort or locale collation is involved. }
  procedure Sift(ARoot, AEnd: Integer);
  var
    LChild: Integer;
    LValue: TNyxText;
  begin
    LValue := ANames[ARoot];
    while ARoot <= (AEnd - 1) div 2 do
    begin
      Check(ACancellation);
      LChild := ARoot * 2 + 1;

      if (LChild < AEnd) and (ANames[LChild] < ANames[LChild + 1]) then
      begin
        Inc(LChild);
      end;

      if LValue >= ANames[LChild] then
      begin
        Break;
      end;
      ANames[ARoot] := ANames[LChild];
      ARoot := LChild;
    end;
    ANames[ARoot] := LValue;
  end;

begin

  if ANames.Count < 2 then
  begin
    Exit;
  end;
  for LStart := ANames.Count div 2 - 1 downto 0 do
  begin
    Sift(LStart, ANames.Count - 1);
  end;
  for LEnd := ANames.Count - 1 downto 1 do
  begin
    LSwap := ANames[LEnd];
    ANames[LEnd] := ANames[0];
    ANames[0] := LSwap;

    if LEnd > 1 then
    begin
      Sift(0, LEnd - 1);
    end;
  end;
end;

procedure TDirectoryCapture.Walk(const ADirectory, ARelative: TNyxText; ADepth: Integer;
  const AContext: INyxPascalCompilerContext; const ACancellation: INyxCancellationScope);
var
  LSearch: TSearchRec;
  LNames: TNyxStrings;
  LAt: Integer;
  LPath: TNyxText;
  LRelative: TNyxText;
  LVirtual: TNyxText;
  LFile: TNyxPascalPhysicalFile;
  LExtension: TNyxText;
  LText: TNyxText;
  LName: TNyxText;
  LSource: INyxPascalModuleSource;
  LIndex: Integer;
  LExisting: Integer;
  LStatus: Integer;
begin
  Check(ACancellation);

  if ADepth > FOptions.FDepth then
  begin
    raise ENyxPascalContext.Create('SDK directory recursion exceeds its depth limit');
  end;
  LNames := TNyxStrings.Create;
  try

    LStatus := FindFirstUTF8(IncludeTrailingPathDelimiter(ADirectory) + '*', faAnyFile, LSearch);

    if LStatus = 0 then
    begin
      try
        repeat
          Check(ACancellation);

          if FVisited >= FOptions.FEntries then
          begin
            raise ENyxPascalContext.Create('SDK enumeration exceeds its directory-entry budget');
          end;
          Inc(FVisited);

          if (LSearch.Name <> '.') and (LSearch.Name <> '..') then
          begin
            LNames.Add(TNyxText(LSearch.Name));
          end;
          LStatus := FindNextUTF8(LSearch);
        until LStatus <> 0;
      finally
        FindCloseUTF8(LSearch);
      end;
    end;
    { FPC's directory API uses the native/DOS no-match/no-more-files statuses.
      Other errors must not turn an unreadable directory into an empty SDK. }
    if not (LStatus in [2, 18]) or not DirectoryExistsUTF8(ADirectory) then
    begin
      raise ENyxPascalContext.Create('The configured SDK directory could not be completely enumerated');
    end;
    SortNames(LNames, ACancellation);
    for LAt := 0 to LNames.Count - 1 do
    begin
      Check(ACancellation);
      LPath := IncludeTrailingPathDelimiter(ADirectory) + LNames[LAt];
      LRelative := ARelative + LNames[LAt];

      if FileIsSymlink(LPath) then
      begin
        raise ENyxPascalContext.Create('SDK capture refuses linked directory entries');
      end;

      if DirectoryExistsUTF8(LPath) then
      begin

        if FOptions.FRecursive then
        begin
          Walk(LPath, LRelative + '/', ADepth + 1, AContext, ACancellation);
        end;
        Continue;
      end;
      LExtension := NyxPascalASCIIKey(ExtractFileExt(LRelative));

      if (LExtension <> '.pas') and (LExtension <> '.pp') and (LExtension <> '.inc') then
      begin
        Continue;
      end;
      LVirtual := LRelative;

      if FOptions.FPrefix <> '' then
      begin
        LVirtual := FOptions.FPrefix + '/' + LRelative;
      end;
      LFile := NyxPascalPhysicalFile(LVirtual);

      if (FFiles.Find(LFile) < 0) and (FFiles.Count >= FOptions.FFiles) then
      begin
        raise ENyxPascalContext.Create('SDK capture exceeds its file-count budget');
      end;
      LText := Read(LPath, ACancellation);
      FFiles := FFiles.WithFile(LFile, LText);

      if FFiles.ByteCount > FOptions.FBytes then
      begin
        raise ENyxPascalContext.Create('SDK capture exceeds its byte budget');
      end;

      if LExtension = '.inc' then
      begin
        Continue;
      end;
      LName := Header(LText, ACancellation);
      for LExisting := 0 to FModules.Count - 1 do
      begin
        Check(ACancellation);

        if (FFiles.Find(FModules.Entry(LExisting).Root) = FFiles.Find(LFile)) and
          ((LName = '') or
            (NyxPascalASCIIKey(FModules.Entry(LExisting).Name.Name) <> NyxPascalASCIIKey(LName))) then
        begin
          raise ENyxPascalContext.Create('A refreshed SDK root cannot silently change its indexed namespace');
        end;
      end;

      if LName = '' then
      begin
        SetLength(FUnindexed, Length(FUnindexed) + 1);
        FUnindexed[High(FUnindexed)] := LFile;
        Continue;
      end;
      LIndex := FModules.Find(NyxPascalModule(LName));

      if (LIndex >= 0) and
        (FFiles.Find(FModules.Entry(LIndex).Root) <> FFiles.Find(LFile)) then
      begin
        raise ENyxPascalContext.Create('Configured SDK directories contain duplicate module namespaces');
      end;
      LSource := NewNyxPascalModuleSource(NyxPascalModule(LName), LFile);

      if AContext <> nil then
      begin
        LSource := LSource.InContext(AContext, FOptions.FTranslation);
      end;
      FModules := FModules.WithModule(LSource);
    end;
  finally
    LNames.Free;
  end;
end;

procedure TDirectoryCapture.Build(const ADirectory: TNyxText; const AFiles: INyxPascalFileCatalog;
  const AModules: INyxPascalModuleCatalog; const AOptions: TNyxPascalDirectoryOptions;
  const AContext: INyxPascalCompilerContext; const ACancellation: INyxCancellationScope);
var
  LDirectory: TNyxText;
  LFacts: INyxPascalCompilerContext;
begin
  AOptions.Validate;
  FOptions := AOptions;
  Check(ACancellation);

  if Trim(ADirectory) = '' then
  begin
    raise ENyxPascalContext.Create('A source directory must be explicitly selected');
  end;
  FFiles := CaptureNyxPascalFiles(AFiles);
  FModules := CaptureNyxPascalModules(AModules, FFiles);

  if (FFiles.Count > FOptions.FFiles) or (FFiles.ByteCount > FOptions.FBytes) then
  begin
    raise ENyxPascalContext.Create('Seed SDK files exceed the directory capture budget');
  end;
  LFacts := nil;

  if AContext <> nil then
  begin
    LFacts := CaptureNyxPascalCompilerContext(AContext);
  end;
  LDirectory := ExpandFileNameUTF8(ADirectory);

  if not DirectoryExistsUTF8(LDirectory) or FileIsSymlink(LDirectory) then
  begin
    raise ENyxPascalContext.Create('A source directory must be explicit, existing and unlinked');
  end;
  Walk(LDirectory, '', 1, LFacts, ACancellation);
  FModules := CaptureNyxPascalModules(FModules, FFiles);
end;

function TDirectoryCapture.Files: INyxPascalFileCatalog;
begin
  Result := FFiles;
end;

function TDirectoryCapture.Modules: INyxPascalModuleCatalog;
begin
  Result := FModules;
end;

function TDirectoryCapture.UnindexedCount: Integer;
begin
  Result := Length(FUnindexed);
end;

function TDirectoryCapture.Unindexed(AIndex: Integer): TNyxPascalPhysicalFile;
begin

  if (AIndex < 0) or (AIndex >= Length(FUnindexed)) then
  begin
    raise ENyxPascalContext.Create('Unindexed source is outside the capture');
  end;
  Result := FUnindexed[AIndex];
end;

function CaptureNyxPascalSourceDirectory(const ADirectory: TNyxText;
  const AFiles: INyxPascalFileCatalog; const AModules: INyxPascalModuleCatalog;
  const AOptions: TNyxPascalDirectoryOptions; const AContext: INyxPascalCompilerContext;
  const ACancellation: INyxCancellationScope): INyxPascalDirectoryCapture;
var
  LCapture: TDirectoryCapture;
begin
  LCapture := TDirectoryCapture.Create;
  Result := LCapture;
  LCapture.Build(ADirectory, AFiles, AModules, AOptions, AContext, ACancellation);
end;
end.
