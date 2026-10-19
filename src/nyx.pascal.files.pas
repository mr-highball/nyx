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

uses nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.context, nyx.pascal.encoding;

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
  { One immutable validated physical source. Optional decoding remains tied to
    these exact original bytes; this data grants no compiler or source meaning. }
  INyxPascalFilePacket = interface(IInterface)
    ['{2C2CB2F8-0C77-4B99-A1AA-3A530ACC7B8C}']
    function FileReference: TNyxPascalPhysicalFile;
    function Original: INyxPascalSourceBytes;
    function Decoded: INyxPascalSourceView;
  end;
  { Bounded ordered source producer, borrowed only during atomic overlay. The
    caller serializes mutable implementations; no producer survives publication.
    Each normalized name occurs once, independently of replacements in the seed. }
  INyxPascalFilePackets = interface(IInterface)
    ['{19C979C0-75B6-4185-84FB-45A2505E4E33}']
    function Count: Integer;
    function Packet(AIndex: Integer): INyxPascalFilePacket;
  end;
  { Exact virtual origin of a source whose bytes have no admitted text view.
    Kept portable so traversal can report an opaque include without opening a
    host file. The byte-error base remains compatible with native capture users. }
  ENyxPascalSourceEncoding = class(ENyxBytes)
  private
    FFile: TNyxPascalPhysicalFile;
  public
    constructor CreateForFile(const AFile: TNyxPascalPhysicalFile);
    property FileReference: TNyxPascalPhysicalFile read FFile;
  end;
  { Immutable captured file/search catalog. No disk fallback or host enumeration
    occurs. Available text is exact validated UTF-8/Unicode. Optional original
    byte snapshots may remain opaque; TextAt then raises a typed encoding gap.
    Per-file accounting charges the larger of original and decoded UTF-8 bytes;
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
  { Optional original-byte/text-view capability. A nil view deliberately keeps
    the complete file uninterpreted. Updates freeze alternate inputs and retain
    independent prior snapshots; no original bytes are transcoded or discarded. }
  INyxPascalByteFileCatalog = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C05}']
    function SourceBytesAt(AIndex: Integer): INyxPascalSourceBytes;
    function SourceViewAt(AIndex: Integer): INyxPascalSourceView;
    function OriginalByteCount: Integer;
    function WithByteSource(const AFile: TNyxPascalPhysicalFile;
      const ABytes: INyxPascalSourceBytes;
      const AView: INyxPascalSourceView = nil): INyxPascalFileCatalog;
  end;

{ Validate a bounded 1024-scalar relative virtual file spelling. No I/O occurs. }
function NyxPascalPhysicalFile(const AName: TNyxText): TNyxPascalPhysicalFile; overload;
{ Empty capture with explicit file case policy. A complete source set and include
  search directories must be supplied by the host, never inferred from disk. }
function NewNyxPascalFileCatalog(ACase: TNyxPascalFileCase): INyxPascalFileCatalog;
{ Freeze alternate catalogs once. Caller serializes mutable providers during
  capture; subsequent reads retain only owned data and exact search order.
  An exactly verified Nyx-owned immutable catalog is already frozen: capture
  returns that same owner without rebuilding file/search vectors. Fluent edits
  still obtain a new owner/identity; sharing grants no prior semantic answers. }
function CaptureNyxPascalFiles(const AFiles: INyxPascalFileCatalog): INyxPascalFileCatalog; overload;
{ Freeze one source packet with the existing four-MiB byte/view bound. Alternate
  bytes/views undergo complete capture; a view must match the supplied bytes. }
function NewNyxPascalFilePacket(const AFile: TNyxPascalPhysicalFile;
  const ABytes: INyxPascalSourceBytes; const AView: INyxPascalSourceView = nil;
  const ACancellation: INyxCancellationScope = nil): INyxPascalFilePacket;
{ Apply all packets to one unpublished clone, retaining seed order/directories
  and appending new names in packet order. Duplicate packet names, case aliases,
  invalid data, quota/cancellation or callback failures publish nothing. Existing
  file/byte limits remain unchanged. Actual clone rows and packets are admitted
  through an optional current analysis work scope, never through stored counters. }
function OverlayNyxPascalFilePackets(const AFiles: INyxPascalFileCatalog;
  const APackets: INyxPascalFilePackets;
  const ACancellation: INyxCancellationScope = nil): INyxPascalFileCatalog;
{ Process-local identity of an owned immutable input set. Detached captures of
  that exact owner retain it; fluent edits obtain a new identity. It is not a
  content hash, disk receipt or authority supplied by alternate providers. }
function NyxPascalFileCatalogIdentity(const AFiles: INyxPascalFileCatalog): TNyxText;
{ Legacy text-only providers explicitly mean canonical authoring UTF-8, not
  proof of bytes read from disk. Owned byte catalogs retain actual byte origin. }
function NyxPascalSourceBytesAt(const AFiles: INyxPascalFileCatalog;
  AIndex: Integer): INyxPascalSourceBytes;
function NyxPascalSourceViewAt(const AFiles: INyxPascalFileCatalog;
  AIndex: Integer): INyxPascalSourceView;
implementation

uses SysUtils, nyx.text.index, nyx.pascal.lexing, nyx.pascal.work;

type
  { Copied virtual identity/cached text/accounting. Byte/view interfaces live in
    the owning class's parallel arrays, never in managed interface records. }
  TPhysicalEntry = record
    FileReference: TNyxPascalPhysicalFile;
    Text: TNyxText;
    Bytes: Integer;
  end;
  { Sealed prepared data owns byte/view interfaces in the class, not its value
    record. Only its canonical ordinary interface may bypass repeated capture. }
  TFilePacket = class(TInterfacedObject, INyxPascalFilePacket)
  public
    Entry: TPhysicalEntry;
    Source: INyxPascalSourceBytes;
    View: INyxPascalSourceView;
    function FileReference: TNyxPascalPhysicalFile;
    function Original: INyxPascalSourceBytes;
    function Decoded: INyxPascalSourceView;
  end;
  { Forward catalog implementation, borrowed only while its interface is held. }
  TFileCatalog = class;
  { Sealed identity owner. Alternate data getters never provide this receipt;
    the concrete object and its ordinary interface identity must both match. }
  IFileCatalogOwner = interface(IInterface)
    ['{84F6E6F3-CCAF-45F2-A18D-9153CF573653}']
    function CatalogOwner: TFileCatalog;
  end;
  { Owns detached file/search vectors and an exact immutable lookup index.
    There are no callbacks or borrowed source providers after construction. }
  TFileCatalog = class(TInterfacedObject, INyxPascalFileCatalog, INyxPascalByteFileCatalog,
    IFileCatalogOwner)
  private
    FCase: TNyxPascalFileCase;
    FIdentity: TNyxText;
    FFiles: array of TPhysicalEntry;
    FDirectories: array of TNyxText;
    FNames: TNyxTextIndex;
    FBytes: Integer;
    FOriginalBytes: Integer;
    FSources: array of INyxPascalSourceBytes;
    FViews: array of INyxPascalSourceView;
    function Key(const AName: TNyxText): TNyxText;
    function Clone(const ACancellation: INyxCancellationScope = nil): TFileCatalog;
    procedure ValidatePacket(AData: TFilePacket; ALiveCount: Integer;
      out AIndex, ATotal: Integer);
    procedure StorePacket(AData: TFilePacket; AIndex, ATotal: Integer;
      var ALiveCount: Integer);
    function WithPacket(AData: TFilePacket;
      const ACancellation: INyxCancellationScope): INyxPascalFileCatalog;
    function WithDirectory(const ADirectory: TNyxText;
      const ACancellation: INyxCancellationScope): INyxPascalFileCatalog;
  public
    constructor Create(ACase: TNyxPascalFileCase);
    function CatalogOwner: TFileCatalog;
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
    function SourceBytesAt(AIndex: Integer): INyxPascalSourceBytes;
    function SourceViewAt(AIndex: Integer): INyxPascalSourceView;
    function OriginalByteCount: Integer;
    function WithByteSource(const AFile: TNyxPascalPhysicalFile;
      const ABytes: INyxPascalSourceBytes;
      const AView: INyxPascalSourceView = nil): INyxPascalFileCatalog;
  end;

{ Metadata admission owns real copied rows/packets. Byte/scalar bounds remain
  independent; a stored catalog or packet never retains this execution scope. }
procedure CheckPreparation(const ACancellation: INyxCancellationScope; AUnits: Integer = 0);
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
    raise ENyxPascalAnalysisCancelled.Create('Source preparation cancelled');

  if AUnits > 0 then NyxPascalAdmitAnalysis(ACancellation, papSourceProof, AUnits);
end;

function CaptureFilesScoped(const AFiles: INyxPascalFileCatalog;
  const ACancellation: INyxCancellationScope): INyxPascalFileCatalog; forward;

function TFilePacket.FileReference: TNyxPascalPhysicalFile;
begin Result := Entry.FileReference; end;
function TFilePacket.Original: INyxPascalSourceBytes;
begin Result := Source; end;
function TFilePacket.Decoded: INyxPascalSourceView;
begin Result := View; end;

function NewNyxPascalFilePacket(const AFile: TNyxPascalPhysicalFile;
  const ABytes: INyxPascalSourceBytes; const AView: INyxPascalSourceView;
  const ACancellation: INyxCancellationScope): INyxPascalFilePacket;
var
  LData: TFilePacket;
  LOwner: INyxPascalFilePacket;
  LBytes: INyxPascalSourceBytes;
  LView: INyxPascalSourceView;
  LExecution: INyxCancellationScope;
  LFile: TNyxPascalPhysicalFile;
begin
  LFile := AFile;
  LBytes := ABytes;
  LView := AView;
  LExecution := ACancellation;
  CheckPreparation(LExecution);
  LData := TFilePacket.Create;
  LOwner := LData;
  LData.Entry.FileReference := NyxPascalPhysicalFile(LFile.Name);

  if LView <> nil then
  begin
    LData.View := CaptureNyxPascalSourceView(LView, LExecution);
    LData.Source := LData.View.Original;

    if not LData.Source.SameBytes(LBytes) then
      raise ENyxPascalContext.Create('A decoded view belongs to different original bytes');
    LData.Entry.Text := LData.View.Text;
  end
  else
    LData.Source := CaptureNyxPascalSourceBytes(LBytes, LExecution);
  LData.Entry.Bytes := LData.Source.Count;

  if (LData.View <> nil) and (NyxUTF8ByteCount(LData.Entry.Text) > LData.Entry.Bytes) then
    LData.Entry.Bytes := NyxUTF8ByteCount(LData.Entry.Text);

  if LData.Entry.Bytes > 4 * 1024 * 1024 then
    raise ENyxPascalContext.Create('A captured physical source exceeds four MiB');
  CheckPreparation(LExecution);
  Result := LOwner;
end;

{ Alternate packets may own mutable callbacks. Read/capture their fields before
  touching the unpublished catalog; only exact concrete immutable data leases. }
function CapturePacket(const APacket: INyxPascalFilePacket;
  const ACancellation: INyxCancellationScope): INyxPascalFilePacket;
var
  LInput: INyxPascalFilePacket;
  LFile: TNyxPascalPhysicalFile;
  LBytes: INyxPascalSourceBytes;
  LView: INyxPascalSourceView;
begin
  LInput := APacket;
  CheckPreparation(ACancellation);

  if LInput = nil then raise ENyxPascalContext.Create('A source packet is required');

  if (LInput is TFilePacket) and
    (((LInput as TFilePacket) as INyxPascalFilePacket) = LInput) then Exit(LInput);
  LFile := LInput.FileReference;
  LBytes := LInput.Original;
  LView := LInput.Decoded;
  Result := NewNyxPascalFilePacket(LFile, LBytes, LView, ACancellation);
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
var
  LIdentity: TGUID;
begin
  inherited Create;

  if (Ord(ACase) < Ord(Low(TNyxPascalFileCase))) or
    (Ord(ACase) > Ord(High(TNyxPascalFileCase))) then
  begin
    raise ENyxPascalContext.Create('Captured file case policy is outside its closed choices');
  end;
  FCase := ACase;

  if CreateGUID(LIdentity) <> 0 then
  begin
    raise ENyxPascalContext.Create('A captured input set needs an independent session identity');
  end;
  FIdentity := TNyxText(GUIDToString(LIdentity));
  FNames := TNyxTextIndex.Create;
end;

function TFileCatalog.CatalogOwner: TFileCatalog;
begin Result := Self; end;

function OwnedCatalog(const AFiles: INyxPascalFileCatalog): TFileCatalog;
var
  LOwner: IFileCatalogOwner;
begin
  Result := nil;

  if Supports(AFiles, IFileCatalogOwner, LOwner) then
  begin
    Result := LOwner.CatalogOwner;

    if (Result = nil) or not (Result is TFileCatalog) or
      ((Result as INyxPascalFileCatalog) <> AFiles) then
    begin
      raise ENyxPascalContext.Create('A source input identity has no exact immutable owner');
    end;
  end;
end;

function NyxPascalFileCatalogIdentity(const AFiles: INyxPascalFileCatalog): TNyxText;
var
  LOwner: TFileCatalog;
begin
  LOwner := OwnedCatalog(AFiles);

  if LOwner = nil then
  begin
    raise ENyxPascalContext.Create('Source identity requires a frozen captured input set');
  end;
  Result := LOwner.FIdentity;
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

function TFileCatalog.Clone(const ACancellation: INyxCancellationScope): TFileCatalog;
var
  LAt: Integer;
begin
  CheckPreparation(ACancellation, 1);
  Result := TFileCatalog.Create(FCase);
  try
    Result.FBytes := FBytes;
    Result.FOriginalBytes := FOriginalBytes;
    SetLength(Result.FFiles, Length(FFiles));
    SetLength(Result.FSources, Length(FFiles));
    SetLength(Result.FViews, Length(FFiles));
    for LAt := 0 to High(FFiles) do
    begin
      CheckPreparation(ACancellation, 1);
      Result.FFiles[LAt] := FFiles[LAt];
      Result.FSources[LAt] := FSources[LAt];
      Result.FViews[LAt] := FViews[LAt];
      Result.FNames.AddFirst(Key(FFiles[LAt].FileReference.Name), LAt);
    end;
    SetLength(Result.FDirectories, Length(FDirectories));
    for LAt := 0 to High(FDirectories) do
    begin
      CheckPreparation(ACancellation, 1);
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

  if FViews[AIndex] = nil then
  begin
    raise ENyxPascalSourceEncoding.CreateForFile(FFiles[AIndex].FileReference);
  end;
  Result := FFiles[AIndex].Text;
end;

constructor ENyxPascalSourceEncoding.CreateForFile(const AFile: TNyxPascalPhysicalFile);
begin
  inherited Create('A captured source requires an explicit readable encoding');
  FFile := AFile;
end;

function TFileCatalog.SourceBytesAt(AIndex: Integer): INyxPascalSourceBytes;
begin
  FileAt(AIndex);
  Result := FSources[AIndex];
end;

function TFileCatalog.SourceViewAt(AIndex: Integer): INyxPascalSourceView;
begin
  FileAt(AIndex);
  Result := FViews[AIndex];
end;

function TFileCatalog.OriginalByteCount: Integer;
begin
  Result := FOriginalBytes;
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
  LBytes: INyxPascalSourceBytes;
  LView: INyxPascalSourceView;
begin
  LBytes := NewNyxPascalSourceBytes(NyxEncodeUTF8(AText), pboAuthoringUTF8);
  LView := DecodeNyxPascalSource(LBytes, NyxPascalUTF8Codec);
  Result := WithByteSource(AFile, LBytes, LView);
end;

function TFileCatalog.WithByteSource(const AFile: TNyxPascalPhysicalFile;
  const ABytes: INyxPascalSourceBytes;
  const AView: INyxPascalSourceView): INyxPascalFileCatalog;
var
  LPacket: INyxPascalFilePacket;
  LLease: INyxPascalFileCatalog;
begin
  { Alternate byte/view callbacks can retire the caller's interface variable.
    The current immutable seed must survive until its new result is complete. }
  LLease := Self;
  LPacket := NewNyxPascalFilePacket(AFile, ABytes, AView);
  Result := (LLease as TFileCatalog).WithPacket(LPacket as TFilePacket, nil);
end;

procedure TFileCatalog.ValidatePacket(AData: TFilePacket; ALiveCount: Integer;
  out AIndex, ATotal: Integer);
begin
  AIndex := Find(AData.Entry.FileReference);

  if (AIndex < 0) and (ALiveCount >= 4096) then
  begin
    raise ENyxPascalContext.Create('A captured source catalog exceeds 4096 files');
  end;
  ATotal := FBytes;

  if AIndex >= 0 then
  begin
    ATotal := ATotal - FFiles[AIndex].Bytes;

    if FFiles[AIndex].FileReference.Name <> AData.Entry.FileReference.Name then
    begin
      raise ENyxPascalContext.Create('A captured source case alias is ambiguous');
    end;
  end;

  if AData.Entry.Bytes > 256 * 1024 * 1024 - ATotal then
  begin
    raise ENyxPascalContext.Create('A captured source catalog exceeds 256 MiB');
  end;
end;

procedure TFileCatalog.StorePacket(AData: TFilePacket; AIndex, ATotal: Integer;
  var ALiveCount: Integer);
begin

  if AIndex < 0 then
  begin
    AIndex := ALiveCount;
    Inc(ALiveCount);

    if ALiveCount > Length(FFiles) then
    begin
      SetLength(FFiles, ALiveCount);
      SetLength(FSources, ALiveCount);
      SetLength(FViews, ALiveCount);
    end;
    FNames.AddFirst(Key(AData.Entry.FileReference.Name), AIndex);
  end;

  if FSources[AIndex] <> nil then
  begin
    Dec(FOriginalBytes, FSources[AIndex].Count);
  end;
  FFiles[AIndex] := AData.Entry;
  FSources[AIndex] := AData.Source;
  FViews[AIndex] := AData.View;
  Inc(FOriginalBytes, AData.Source.Count);
  FBytes := ATotal + AData.Entry.Bytes;
end;

function TFileCatalog.WithPacket(AData: TFilePacket;
  const ACancellation: INyxCancellationScope): INyxPascalFileCatalog;
var
  LAt, LTotal, LCount: Integer;
  LCopy: TFileCatalog;
  LOwner: INyxPascalFileCatalog;
begin
  CheckPreparation(ACancellation);
  LCount := Count;
  ValidatePacket(AData, LCount, LAt, LTotal);
  LCopy := Clone(ACancellation);
  LOwner := LCopy;
  CheckPreparation(ACancellation, 1);
  LCopy.StorePacket(AData, LAt, LTotal, LCount);
  CheckPreparation(ACancellation);
  Result := LOwner;
end;

function TFileCatalog.WithIncludeDirectory(const ADirectory: TNyxText): INyxPascalFileCatalog;
begin
  Result := WithDirectory(ADirectory, nil);
end;

function TFileCatalog.WithDirectory(const ADirectory: TNyxText;
  const ACancellation: INyxCancellationScope): INyxPascalFileCatalog;
var
  LAt: Integer;
  LCopy: TFileCatalog;
  LOwner: INyxPascalFileCatalog;
begin
  CheckPreparation(ACancellation);

  if ADirectory <> '' then
  begin
    NyxPascalPhysicalFile(ADirectory + '/directory-probe');
  end;
  for LAt := 0 to High(FDirectories) do
  begin
    CheckPreparation(ACancellation, 1);

    if Key(FDirectories[LAt]) = Key(ADirectory) then
    begin
      Exit(Self);
    end;
  end;

  if Length(FDirectories) >= 256 then
  begin
    raise ENyxPascalContext.Create('Captured include directories exceed 256 entries');
  end;
  LCopy := Clone(ACancellation);
  LOwner := LCopy;
  LAt := Length(LCopy.FDirectories);
  SetLength(LCopy.FDirectories, LAt + 1);
  LCopy.FDirectories[LAt] := ADirectory;
  CheckPreparation(ACancellation);
  Result := LOwner;
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
  LOtherView: INyxPascalSourceView;
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
      not FSources[LAt].SameBytes(NyxPascalSourceBytesAt(AOther, LAt)) then
    begin
      Exit;
    end;
    LOtherView := NyxPascalSourceViewAt(AOther, LAt);

    if (FViews[LAt] = nil) <> (LOtherView = nil) then
    begin
      Exit;
    end;

    if (FViews[LAt] <> nil) and not FViews[LAt].SameView(LOtherView) then
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

function CaptureFilesScoped(const AFiles: INyxPascalFileCatalog;
  const ACancellation: INyxCancellationScope): INyxPascalFileCatalog;
var
  LAt: Integer;
  LCount: Integer;
  LDirectories: Integer;
  LByteFiles: INyxPascalByteFileCatalog;
  LCaptured: INyxPascalFileCatalog;
  LInput: INyxPascalFileCatalog;
  LOwned: TFileCatalog;
  LPacket: INyxPascalFilePacket;
  LFile: TNyxPascalPhysicalFile;
  LSource: INyxPascalSourceBytes;
  LView: INyxPascalSourceView;
begin
  { Keep the input alive before touching the possibly aliased managed result.
    Only the concrete catalog's sealed receipt plus exact ordinary-interface
    identity admits this path. Alternate getters/counters cannot claim it. }
  LInput := AFiles;
  CheckPreparation(ACancellation);

  if LInput = nil then
  begin
    raise ENyxPascalContext.Create('Source analysis requires a captured file catalog');
  end;
  LOwned := OwnedCatalog(LInput);

  if LOwned <> nil then
  begin
    Result := LInput;
    Exit;
  end;
  LCount := LInput.Count;
  LDirectories := LInput.DirectoryCount;

  if (LCount < 0) or (LCount > 4096) or
    (LDirectories < 0) or (LDirectories > 256) then
  begin
    raise ENyxPascalContext.Create('An alternate file catalog exceeds its bounds');
  end;
  LCaptured := NewNyxPascalFileCatalog(LInput.CasePolicy);
  Supports(LInput, INyxPascalByteFileCatalog, LByteFiles);
  for LAt := 0 to LCount - 1 do
  begin
    CheckPreparation(ACancellation, 1);
    LFile := LInput.FileAt(LAt);

    if LCaptured.Find(LFile) >= 0 then
    begin
      raise ENyxPascalContext.Create('An alternate file catalog contains duplicate names');
    end;

    if LByteFiles <> nil then
    begin
      LPacket := NewNyxPascalFilePacket(LFile, LByteFiles.SourceBytesAt(LAt),
        LByteFiles.SourceViewAt(LAt), ACancellation);
    end
    else
    begin
      LSource := NewNyxPascalSourceBytes(NyxEncodeUTF8(LInput.TextAt(LAt)), pboAuthoringUTF8,
        ACancellation);
      LView := DecodeNyxPascalSource(LSource, NyxPascalUTF8Codec, ACancellation);
      LPacket := NewNyxPascalFilePacket(LFile, LSource, LView, ACancellation);
    end;
    LCaptured := (LCaptured as TFileCatalog).WithPacket(LPacket as TFilePacket, ACancellation);
  end;
  for LAt := 0 to LDirectories - 1 do
  begin
    CheckPreparation(ACancellation, 1);
    LCaptured := (LCaptured as TFileCatalog).WithDirectory(LInput.DirectoryAt(LAt), ACancellation);
  end;
  { Alternate callbacks can fail at any entry. Only a complete immutable
    capture may enter the caller's potentially aliased managed result. }
  CheckPreparation(ACancellation);
  Result := LCaptured;
end;

function CaptureNyxPascalFiles(const AFiles: INyxPascalFileCatalog): INyxPascalFileCatalog;
begin
  Result := CaptureFilesScoped(AFiles, nil);
end;

function OverlayNyxPascalFilePackets(const AFiles: INyxPascalFileCatalog;
  const APackets: INyxPascalFilePackets;
  const ACancellation: INyxCancellationScope): INyxPascalFileCatalog;
var
  LBase, LOwner: INyxPascalFileCatalog;
  LInput: INyxPascalFilePackets;
  LPacket: INyxPascalFilePacket;
  LCopy: TFileCatalog;
  LData: TFilePacket;
  LSeen: TNyxTextIndex;
  LAt, LCount, LLive, LCapacity, LIndex, LTotal: Integer;
  LKey: TNyxText;
  LExecution: INyxCancellationScope;
begin
  { Lease both inputs before any possibly aliased result assignment. Source
    callbacks see no candidate, including its temporarily reserved unused slots. }
  LBase := AFiles;
  LInput := APackets;
  LExecution := ACancellation;
  CheckPreparation(LExecution);

  if LInput = nil then raise ENyxPascalContext.Create('A source packet producer is required');
  LCount := LInput.Count;

  if (LCount < 0) or (LCount > 4096) then
    raise ENyxPascalContext.Create('Source packets exceed their 4096-file bound');
  LBase := CaptureFilesScoped(LBase, LExecution);

  if LCount = 0 then Exit(LBase);
  LLive := LBase.Count;
  LCopy := (LBase as TFileCatalog).Clone(LExecution);
  LOwner := LCopy;
  LCapacity := LLive + LCount;

  if LCapacity > 4096 then LCapacity := 4096;
  SetLength(LCopy.FFiles, LCapacity);
  SetLength(LCopy.FSources, LCapacity);
  SetLength(LCopy.FViews, LCapacity);
  LSeen := TNyxTextIndex.Create;
  try
    for LAt := 0 to LCount - 1 do
    begin
      CheckPreparation(LExecution, 1);
      LPacket := CapturePacket(LInput.Packet(LAt), LExecution);
      LData := LPacket as TFilePacket;
      LKey := LCopy.Key(LData.Entry.FileReference.Name);

      if LSeen.IndexOf(LKey) >= 0 then
        raise ENyxPascalContext.Create('Source packets contain duplicate names');
      LSeen.AddFirst(LKey, LAt);
      LCopy.ValidatePacket(LData, LLive, LIndex, LTotal);
      LCopy.StorePacket(LData, LIndex, LTotal, LLive);
    end;
    { Only real rows may be visible through Count/FileAt after publication. }
    SetLength(LCopy.FFiles, LLive);
    SetLength(LCopy.FSources, LLive);
    SetLength(LCopy.FViews, LLive);
    CheckPreparation(LExecution);
    Result := LOwner;
  finally
    LSeen.Free;
  end;
end;

function NyxPascalSourceBytesAt(const AFiles: INyxPascalFileCatalog;
  AIndex: Integer): INyxPascalSourceBytes;
var
  LBytes: INyxPascalByteFileCatalog;
begin

  if AFiles = nil then
  begin
    raise ENyxPascalContext.Create('An original source requires its file catalog');
  end;
  AFiles.FileAt(AIndex);

  if Supports(AFiles, INyxPascalByteFileCatalog, LBytes) then
  begin
    Result := LBytes.SourceBytesAt(AIndex);
  end
  else
  begin
    Result := NewNyxPascalSourceBytes(NyxEncodeUTF8(AFiles.TextAt(AIndex)), pboAuthoringUTF8);
  end;
end;

function NyxPascalSourceViewAt(const AFiles: INyxPascalFileCatalog;
  AIndex: Integer): INyxPascalSourceView;
var
  LBytes: INyxPascalByteFileCatalog;
begin

  if AFiles = nil then
  begin
    raise ENyxPascalContext.Create('A decoded source requires its file catalog');
  end;
  AFiles.FileAt(AIndex);

  if Supports(AFiles, INyxPascalByteFileCatalog, LBytes) then
  begin
    Result := LBytes.SourceViewAt(AIndex);
  end
  else
  begin
    Result := DecodeNyxPascalSource(NyxPascalSourceBytesAt(AFiles, AIndex), NyxPascalUTF8Codec);
  end;
end;

end.
