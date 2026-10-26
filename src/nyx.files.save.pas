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

unit nyx.files.save;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.bytes, nyx.bytes.sources, nyx.data, nyx.files, nyx.scheduler;

type
  { Picker support and download delivery have distinct meanings. PreferPicker
    uses a picker when that host exposes one; unsupported hosts may request a
    download. Required activation remains a host rule, never fabricated data. }
  TNyxFileSaveMode = (nfsmPreferPicker, nfsmPicker, nfsmDownload);

  { Closed host outcomes keep committed writes, requested downloads and user
    cancellation distinct from activation, capability and operation failures.
    A download request alone never proves that a file reached disk. }
  TNyxFileSaveOutcome = (nfsoWritten, nfsoDownloadRequested, nfsoCancelled,
    nfsoActivationRequired, nfsoUnavailable, nfsoFailed);

  { Download-only activation policy: require a current gesture or defer to the
    browser's rules. Neither choice bypasses a save picker's own activation gate. }
  TNyxDownloadActivation = (nfdaRequired, nfdaPlatform);

  { Convenient closed media choices mapped to MIME references by NyxMediaType.
    These describe payloads; they neither validate bytes nor grant file access. }
  TNyxFileMediaType = (nfmtBinary, nfmtJSON, nfmtText, nfmtPNG, nfmtJPEG, nfmtSVG);

  { Open media names are validated references, separate from user captions. }
  TNyxMediaTypeRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;

  { Immutable portable save description. Name is a leaf suggestion; it confers
    no path or file-system authority. Extensions are hints and users may choose
    another name. AllTypes controls the picker filter, not write permission.
    MaximumBytes includes zero (empty files only), with a 4 MiB direct-host cap.
    Download activation can explicitly defer to host policy; a picker always
    retains the browser's own secure-context/user-activation requirements. }
  TNyxFileSaveOptions = record
  private
    FName: TNyxText;
    FTitle: TNyxText;
    FMode: TNyxFileSaveMode;
    FMediaType: TNyxMediaTypeRef;
    FExtensions: array of TNyxText;
    FAllTypes: Boolean;
    FMaximumBytes: Integer;
    FActivation: TNyxDownloadActivation;
  public
    function Copy: TNyxFileSaveOptions;
    function Suggest(const AName: TNyxText): TNyxFileSaveOptions;
    function Title(const AText: TNyxText): TNyxFileSaveOptions;
    function Mode(AValue: TNyxFileSaveMode): TNyxFileSaveOptions;
    function MediaType(const AValue: TNyxMediaTypeRef): TNyxFileSaveOptions;
    function Allow(const AExtension: TNyxText): TNyxFileSaveOptions;
    function AllTypes(AValue: Boolean): TNyxFileSaveOptions;
    function MaximumBytes(AValue: Integer): TNyxFileSaveOptions;
    function DownloadActivation(AValue: TNyxDownloadActivation): TNyxFileSaveOptions;
    function Extension(AIndex: Integer): TNyxText;
    function ExtensionCount: Integer;
    procedure Validate;
    procedure Admit(const ABytes: TNyxBytes);
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxFileSaveOptions; static;
    property SuggestedName: TNyxText read FName;
    property DialogTitle: TNyxText read FTitle;
    property SaveMode: TNyxFileSaveMode read FMode;
    property ContentType: TNyxMediaTypeRef read FMediaType;
    property IncludeAllTypes: Boolean read FAllTypes;
    property ByteLimit: Integer read FMaximumBytes;
    property ActivationPolicy: TNyxDownloadActivation read FActivation;
  end;

  { Written means a completed host write/close, not crash-proof storage.
    DownloadRequested means only that a browser download was requested.
    The actual selected leaf may differ from the suggestion. Machine paths,
    native handles and browser capabilities never enter portable result data.
    Host diagnostics are exact UTF-8 text bounded to 8192 bytes. }
  TNyxFileSaveResult = record
    Outcome: TNyxFileSaveOutcome;
    Name: TNyxText;
    ByteCount: Integer;
    Error: TNyxText;
  end;

  { Borrowed method receiver called on the UI thread at most once per request.
    Its owner must cancel before disposal; the result carries portable values,
    while request cancellation silently detaches this callback. }
  TNyxFileSaveReply = procedure(const AResult: TNyxFileSaveResult) of object;

  { Reference-counted token for one independent host operation. Releasing a token
    alone does not cancel pending work; explicit Cancel detaches its borrowed
    receiver. A host may retain private platform work until promises settle. }
  INyxFileSaveRequest = interface(IInterface)
    ['{60CAFF21-C6E4-43D0-8855-940000000001}']
    { UI-only, idempotent silent cancellation detaches the borrowed receiver.
      Hosts abort uncommitted work when possible. An already-committing external
      file effect cannot be represented as a rolled-back UI transaction. }
    procedure Cancel;
  end;

  { Explicit runtime host authority, never reconstructed from serialized data.
    BeginSave snapshots options/bytes before host effects. Completion is UI-only,
    exactly once and may be inline. Pending work returns an owned request token.
    Receivers are borrowed until completion/cancellation; caller owners must cancel
    before disposal. Each request is independent, including reentrant calls. }
  INyxFileSaveHost = interface(IInterface)
    ['{60CAFF21-C6E4-43D0-8855-940000000002}']
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

  { Runtime streaming budgets are separate from the existing whole-buffer file
    options/wire contract. Total bytes, per-read chunk and browser Blob buffering
    each have explicit caller limits. Zero total permits empty output; zero
    download buffering permits only an empty Blob, without disabling pickers.
    Defaults: 64 MiB total/download, 64 KiB chunks. No machine path is retained. }
  TNyxFileSaveStreamOptions = record
  private
    FFile: TNyxFileSaveOptions;
    FMaximum: Integer;
    FChunk: Integer;
    FDownload: Integer;
  public
    function MaximumBytes(AValue: Integer): TNyxFileSaveStreamOptions;
    function ChunkBytes(AValue: Integer): TNyxFileSaveStreamOptions;
    function DownloadBufferBytes(AValue: Integer): TNyxFileSaveStreamOptions;
    procedure Validate;
    property FileOptions: TNyxFileSaveOptions read FFile;
    property ByteLimit: Integer read FMaximum;
    property ChunkLimit: Integer read FChunk;
    property DownloadLimit: Integer read FDownload;
  end;

  { Optional capability of the same host. The immutable source is opened once
    per request on UI. Writes stage bounded chunks and commit only at exact EOF;
    cancellation suppresses the borrowed reply and aborts uncommitted work.
    Browser downloads may buffer a complete Blob within DownloadLimit, reporting
    DownloadRequested rather than disk completion. Old byte-only hosts stay valid. }
  INyxFileSaveStreamHost = interface(IInterface)
    ['{243EB91F-79EC-40B2-A69F-32A33E6973BA}']
    function BeginStream(const AOptions: TNyxFileSaveStreamOptions;
      const ASource: INyxByteSource; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

  { Shared request lifetime for target/creator adapters. Self-retention lasts
    only while pending. Cancel and Complete revoke the receiver before cleanup/
    callbacks; subclasses must eventually retire outstanding platform callbacks.
    Scope cancellation suppresses outcomes. EndHost must be idempotent and must
    not invoke the client receiver. Detached delayed callbacks check Pending.
    The base owns copied payloads, never application/tree/renderer objects. }
  TNyxFileSaveRequest = class(TInterfacedObject, INyxFileSaveRequest)
  private
    FReply: TNyxFileSaveReply;
    FLease: INyxFileSaveRequest;
  protected
    FOptions: TNyxFileSaveOptions;
    FBytes: TNyxBytes;
    FScheduler: INyxScheduler;
    FScope: INyxCancellationScope;
    FStreamed: Boolean;
    FStreamOptions: TNyxFileSaveStreamOptions;
    FReader: INyxByteReader;
    FExpectedBytes: Integer;
    FReadBytes: Integer;
    FEOFChecked: Boolean;
    function Pending: Boolean;
    procedure EndHost; virtual;
    procedure Complete(AOutcome: TNyxFileSaveOutcome; const AName: TNyxText;
      AByteCount: Integer; const AError: TNyxText = '');
    procedure RetainPending;
    { Returns an owned bounded chunk and validates exact declared EOF. Reader
      callbacks may cancel; retain their cursor locally and recheck Pending. }
    function ReadPayloadChunk: TNyxBytes;
  public
    constructor Create(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply);
    constructor CreateStream(const AOptions: TNyxFileSaveStreamOptions;
      const ASource: INyxByteSource; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply);
    procedure Cancel; virtual;
  end;

function NyxMediaType(const AName: TNyxText): TNyxMediaTypeRef; overload;
function NyxMediaType(AValue: TNyxFileMediaType): TNyxMediaTypeRef; overload;
function NyxFileSaveOptions(const ASuggestedName: TNyxText): TNyxFileSaveOptions;
function NyxFileSaveStreamOptions(const AFile: TNyxFileSaveOptions): TNyxFileSaveStreamOptions;
function NyxFileSaveOutcomeName(AOutcome: TNyxFileSaveOutcome): TNyxText;
function NyxFileSaveWritten(const AResult: TNyxFileSaveResult): Boolean;
function NyxFileSaveDelivered(const AResult: TNyxFileSaveResult): Boolean;
{ Validate a host receipt against the complete admitted payload before publishing
  it. Names remain portable leaves; delivered outcomes account for every byte,
  while other outcomes report zero delivered bytes. Raises ENyxFile on refusal. }
procedure ValidateNyxFileSaveResult(const AResult: TNyxFileSaveResult;
  AExpectedBytes: Integer);
{ A stream's explicit caller limit replaces only the old direct payload ceiling.
  Outcome/name/error/exact-byte receipt rules are otherwise identical. }
procedure ValidateNyxStreamFileSaveResult(const AResult: TNyxFileSaveResult;
  AExpectedBytes, AMaximumBytes: Integer);

implementation

uses nyx.flow.types, nyx.editing;

function NyxMediaType(AValue: TNyxFileMediaType): TNyxMediaTypeRef;
const
  CNames: array[TNyxFileMediaType] of TNyxText = ('application/octet-stream',
    'application/json', 'text/plain', 'image/png', 'image/jpeg', 'image/svg+xml');
begin

  if (Ord(AValue) < Ord(Low(TNyxFileMediaType))) or
    (Ord(AValue) > Ord(High(TNyxFileMediaType))) then
  begin
    raise ENyxFile.Create('Unknown well-known file media type');
  end;
  Result := NyxMediaType(CNames[AValue]);
end;

function NyxMediaType(const AName: TNyxText): TNyxMediaTypeRef;
var
  LIndex: Integer;
  LSlash: Integer;
begin
  LSlash := Pos('/', AName);

  if (Length(AName) > 128) or (LSlash < 2) or (LSlash = Length(AName)) then
  begin
    raise ENyxFile.Create('A media type needs a type/subtype reference');
  end;
  for LIndex := 1 to Length(AName) do
  begin

    if LIndex = LSlash then
    begin
      Continue;
    end;

    if not (AName[LIndex] in ['a'..'z', 'A'..'Z', '0'..'9', '!', '#', '$', '&',
      '-', '^', '_', '.', '+']) then
    begin
      raise ENyxFile.Create('A media type requires a portable MIME token pair');
    end;
  end;
  Result.FName := LowerCase(AName);
end;

function NyxFileSaveOptions(const ASuggestedName: TNyxText): TNyxFileSaveOptions;
begin
  Result := Default(TNyxFileSaveOptions);
  Result.FName := ASuggestedName;
  Result.FTitle := 'Save file';
  Result.FMediaType := NyxMediaType('application/octet-stream');
  Result.FAllTypes := True;
  Result.FMaximumBytes := NyxMaximumPackedBytes;
  Result.Validate;
end;

function TNyxFileSaveOptions.Copy: TNyxFileSaveOptions;
var
  LIndex: Integer;
begin
  Result := Self;
  Result.FExtensions := nil;
  SetLength(Result.FExtensions, Length(FExtensions));
  for LIndex := 0 to High(FExtensions) do
  begin
    Result.FExtensions[LIndex] := FExtensions[LIndex];
  end;
end;

procedure TNyxFileSaveOptions.Validate;
var
  LFile: INyxTextFile;
  LSelection: TNyxTextFileSelection;
  LIndex: Integer;
begin
  LFile := NyxTextFile(FName, '');
  NyxUTF8ByteCount(FTitle);
  NyxMediaType(FMediaType.Name);

  if (NyxTextScalarCount(FTitle) > 256) or
    (Ord(FMode) < Ord(Low(TNyxFileSaveMode))) or
    (Ord(FMode) > Ord(High(TNyxFileSaveMode))) or
    (Ord(FActivation) < Ord(Low(TNyxDownloadActivation))) or
    (Ord(FActivation) > Ord(High(TNyxDownloadActivation))) or
    (FMaximumBytes < 0) or (FMaximumBytes > NyxTextFileMaximumBytes) or
    (Length(FExtensions) > 16) then
  begin
    raise ENyxFile.Create('Invalid file-save choices or byte budget');
  end;
  LSelection := NyxTextFileSelection;
  for LIndex := 0 to High(FExtensions) do
  begin
    LSelection := LSelection.Allow(FExtensions[LIndex]);
  end;

  if not FAllTypes and (Length(FExtensions) = 0) then
  begin
    raise ENyxFile.Create('A restricted picker needs an allowed extension');
  end;
end;

procedure TNyxFileSaveOptions.Admit(const ABytes: TNyxBytes);
begin
  Validate;

  if Length(ABytes) > FMaximumBytes then
  begin
    raise ENyxFile.Create('The prepared file exceeds its configured byte budget');
  end;
end;

function TNyxFileSaveOptions.Suggest(const AName: TNyxText): TNyxFileSaveOptions;
begin
  Result := Copy;
  Result.FName := AName;
  Result.Validate;
end;

function TNyxFileSaveOptions.Title(const AText: TNyxText): TNyxFileSaveOptions;
begin
  Result := Copy;
  Result.FTitle := AText;
  Result.Validate;
end;

function TNyxFileSaveOptions.Mode(AValue: TNyxFileSaveMode): TNyxFileSaveOptions;
begin
  Result := Copy;
  Result.FMode := AValue;
  Result.Validate;
end;

function TNyxFileSaveOptions.MediaType(const AValue: TNyxMediaTypeRef): TNyxFileSaveOptions;
begin
  Result := Copy;
  Result.FMediaType := NyxMediaType(AValue.Name);
end;

function TNyxFileSaveOptions.Allow(const AExtension: TNyxText): TNyxFileSaveOptions;
var
  LSelection: TNyxTextFileSelection;
  LIndex: Integer;
begin
  LSelection := NyxTextFileSelection.Allow(AExtension);
  Result := Copy;
  for LIndex := 0 to High(FExtensions) do
  begin

    if FExtensions[LIndex] = LSelection.Extension(0) then
    begin
      Exit;
    end;
  end;
  SetLength(Result.FExtensions, Length(FExtensions) + 1);
  Result.FExtensions[High(Result.FExtensions)] := LSelection.Extension(0);
  Result.Validate;
end;

function TNyxFileSaveOptions.AllTypes(AValue: Boolean): TNyxFileSaveOptions;
begin
  Result := Copy;
  Result.FAllTypes := AValue;
  Result.Validate;
end;

function TNyxFileSaveOptions.MaximumBytes(AValue: Integer): TNyxFileSaveOptions;
begin
  Result := Copy;
  Result.FMaximumBytes := AValue;
  Result.Validate;
end;

function TNyxFileSaveOptions.DownloadActivation(AValue: TNyxDownloadActivation): TNyxFileSaveOptions;
begin
  Result := Copy;
  Result.FActivation := AValue;
  Result.Validate;
end;

function TNyxFileSaveOptions.Extension(AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= Length(FExtensions)) then
  begin
    raise ENyxFile.Create('File-save extension index is out of range');
  end;
  Result := FExtensions[AIndex];
end;

function TNyxFileSaveOptions.ExtensionCount: Integer;
begin
  Result := Length(FExtensions);
end;

function TNyxFileSaveOptions.ToData: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  Validate;
  SetLength(LItems, Length(FExtensions));
  for LIndex := 0 to High(LItems) do
  begin
    LItems[LIndex] := NyxData(FExtensions[LIndex]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('name', NyxData(FName)),
    NyxField('title', NyxData(FTitle)), NyxField('mode', NyxData(Ord(FMode))),
    NyxField('mediaType', NyxData(FMediaType.Name)), NyxField('extensions', NyxArray(LItems)),
    NyxField('allTypes', NyxData(FAllTypes)), NyxField('maximumBytes', NyxData(FMaximumBytes)),
    NyxField('downloadActivation', NyxData(Ord(FActivation)))]);
end;

class function TNyxFileSaveOptions.FromData(const AData: TNyxDataValue): TNyxFileSaveOptions;
var
  LIndex: Integer;
  LMode: Integer;
  LActivation: Integer;
  LItems: TNyxDataValue;
begin
  ValidateNyxFlowFields(AData, ['version', 'name', 'title', 'mode', 'mediaType',
    'extensions', 'allTypes', 'maximumBytes', 'downloadActivation']);

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxFile.Create('Unknown file-save option version');
  end;
  LMode := AData.Field('mode').AsInteger;
  LActivation := AData.Field('downloadActivation').AsInteger;

  if (LMode < Ord(Low(TNyxFileSaveMode))) or (LMode > Ord(High(TNyxFileSaveMode))) or
    (LActivation < Ord(Low(TNyxDownloadActivation))) or
    (LActivation > Ord(High(TNyxDownloadActivation))) then
  begin
    raise ENyxFile.Create('Unknown file-save choice');
  end;
  Result := NyxFileSaveOptions(AData.Field('name').AsText)
    .Title(AData.Field('title').AsText).Mode(TNyxFileSaveMode(LMode))
    .MediaType(NyxMediaType(AData.Field('mediaType').AsText));
  LItems := AData.Field('extensions');

  if (LItems.Kind <> ndArray) or (LItems.Count > 16) then
  begin
    raise ENyxFile.Create('File-save extensions require a bounded array');
  end;
  for LIndex := 0 to LItems.Count - 1 do
  begin
    Result := Result.Allow(LItems.Item(LIndex).AsText);

    if Result.ExtensionCount <> LIndex + 1 then
    begin
      raise ENyxFile.Create('Repeated file-save extensions refuse');
    end;
  end;
  Result := Result.AllTypes(AData.Field('allTypes').AsBoolean)
    .MaximumBytes(AData.Field('maximumBytes').AsInteger)
    .DownloadActivation(TNyxDownloadActivation(LActivation));
end;

function NyxFileSaveOutcomeName(AOutcome: TNyxFileSaveOutcome): TNyxText;
const
  CNames: array[TNyxFileSaveOutcome] of TNyxText =
    ('written', 'download-requested', 'cancelled', 'activation-required', 'unavailable', 'failed');
begin

  if (Ord(AOutcome) < Ord(Low(TNyxFileSaveOutcome))) or
    (Ord(AOutcome) > Ord(High(TNyxFileSaveOutcome))) then
  begin
    raise ENyxFile.Create('Unknown save outcome');
  end;
  Result := CNames[AOutcome];
end;

function NyxFileSaveWritten(const AResult: TNyxFileSaveResult): Boolean;
begin
  Result := AResult.Outcome = nfsoWritten;
end;

function NyxFileSaveDelivered(const AResult: TNyxFileSaveResult): Boolean;
begin
  Result := AResult.Outcome in [nfsoWritten, nfsoDownloadRequested];
end;

function NyxFileSaveStreamOptions(const AFile: TNyxFileSaveOptions): TNyxFileSaveStreamOptions;
begin
  AFile.Validate;
  Result := Default(TNyxFileSaveStreamOptions);
  Result.FFile := AFile.Copy;
  Result.FMaximum := 64 * 1024 * 1024;
  Result.FDownload := Result.FMaximum;
  Result.FChunk := 64 * 1024;
end;

function TNyxFileSaveStreamOptions.MaximumBytes(AValue: Integer): TNyxFileSaveStreamOptions;
begin
  Result := Self;
  Result.FMaximum := AValue;
  Result.Validate;
end;

function TNyxFileSaveStreamOptions.ChunkBytes(AValue: Integer): TNyxFileSaveStreamOptions;
begin
  Result := Self;
  Result.FChunk := AValue;
  Result.Validate;
end;

function TNyxFileSaveStreamOptions.DownloadBufferBytes(AValue: Integer): TNyxFileSaveStreamOptions;
begin
  Result := Self;
  Result.FDownload := AValue;
  Result.Validate;
end;

procedure TNyxFileSaveStreamOptions.Validate;
begin
  FFile.Validate;

  if (FMaximum < 0) or (FDownload < 0) or (FChunk <= 0) or
    (FChunk > NyxMaximumByteChunk) then
  begin
    raise ENyxFile.Create('Streaming save requires nonnegative byte limits and a bounded positive chunk');
  end;
end;

procedure ValidateNyxStreamFileSaveResult(const AResult: TNyxFileSaveResult;
  AExpectedBytes, AMaximumBytes: Integer);
var
  LFile: INyxTextFile;
begin
  NyxFileSaveOutcomeName(AResult.Outcome);
  LFile := NyxTextFile(AResult.Name, '');

  if NyxUTF8ByteCount(AResult.Error) > 8192 then
  begin
    raise ENyxFile.Create('The file-save diagnostic exceeds its UTF-8 byte budget');
  end;

  if (AMaximumBytes < 0) or (AExpectedBytes < 0) or (AExpectedBytes > AMaximumBytes) or
    (AResult.ByteCount < 0) then
  begin
    raise ENyxFile.Create('Invalid file-save byte receipt');
  end;

  if NyxFileSaveDelivered(AResult) then
  begin

    if (AResult.ByteCount <> AExpectedBytes) or (AResult.Error <> '') then
    begin
      raise ENyxFile.Create('File-save delivery must account for the complete payload');
    end;
  end
  else
  begin

    if AResult.ByteCount <> 0 then
    begin
      raise ENyxFile.Create('An undelivered file-save outcome cannot claim delivered bytes');
    end;
  end;
end;

procedure ValidateNyxFileSaveResult(const AResult: TNyxFileSaveResult;
  AExpectedBytes: Integer);
begin
  ValidateNyxStreamFileSaveResult(AResult, AExpectedBytes, NyxTextFileMaximumBytes);
end;

constructor TNyxFileSaveRequest.Create(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply);
var
  LIndex: Integer;
begin
  inherited Create;

  if (AScheduler = nil) or not Assigned(AReply) then
  begin
    raise ENyxFile.Create('File save requires a UI scheduler and reply');
  end;
  AScheduler.RequireUI;
  AOptions.Admit(ABytes);
  FOptions := AOptions.Copy;
  FExpectedBytes := Length(ABytes);
  SetLength(FBytes, Length(ABytes));
  for LIndex := 0 to High(FBytes) do
  begin
    FBytes[LIndex] := ABytes[LIndex];
  end;
  FScheduler := AScheduler;
  FScope := ACancellation;
  FReply := AReply;
end;

constructor TNyxFileSaveRequest.CreateStream(const AOptions: TNyxFileSaveStreamOptions;
  const ASource: INyxByteSource; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply);
begin
  inherited Create;

  if (ASource = nil) or (AScheduler = nil) or not Assigned(AReply) then
  begin
    raise ENyxFile.Create('Streaming save requires an immutable source, scheduler and reply');
  end;
  AScheduler.RequireUI;
  AOptions.Validate;
  FExpectedBytes := ASource.ByteCount;

  if (FExpectedBytes < 0) or (FExpectedBytes > AOptions.ByteLimit) then
  begin
    raise ENyxFile.Create('Streaming source exceeds the admitted total byte limit');
  end;
  FReader := ASource.Open;

  if FReader = nil then
  begin
    raise ENyxFile.Create('Streaming source did not open an independent cursor');
  end;
  FStreamed := True;
  FStreamOptions := AOptions;
  FOptions := AOptions.FileOptions.Copy;
  FScheduler := AScheduler;
  FScope := ACancellation;
  FReply := AReply;
end;

function TNyxFileSaveRequest.ReadPayloadChunk: TNyxBytes;
var
  LReader: INyxByteReader;
  LLimit: Integer;
begin
  Result := nil;
  FScheduler.RequireUI;

  if not FStreamed then
  begin
    raise ENyxFile.Create('Chunk reads require an admitted streaming save');
  end;

  if not Pending then
  begin
    Exit;
  end;
  LLimit := FExpectedBytes - FReadBytes;

  if LLimit > FStreamOptions.ChunkLimit then
  begin
    LLimit := FStreamOptions.ChunkLimit;
  end;
  LReader := FReader;

  if FEOFChecked then
  begin
    Exit;
  end;

  if LLimit = 0 then
  begin
    Result := LReader.Read(1);

    if not Pending then
    begin
      Result := nil;
      Exit;
    end;

    if Length(Result) <> 0 then
    begin
      raise ENyxFile.Create('Streaming source exceeded its declared byte length');
    end;
    FEOFChecked := True;
    Exit;
  end;
  Result := LReader.Read(LLimit);

  if not Pending then
  begin
    Result := nil;
    Exit;
  end;

  if (Length(Result) = 0) or (Length(Result) > LLimit) then
  begin
    raise ENyxFile.Create('Streaming source returned an invalid bounded chunk');
  end;
  Inc(FReadBytes, Length(Result));
end;

procedure TNyxFileSaveRequest.RetainPending;
begin
  FLease := Self;
end;

function TNyxFileSaveRequest.Pending: Boolean;
begin
  Result := Assigned(FReply) and ((FScope = nil) or not FScope.Cancelled);
end;

procedure TNyxFileSaveRequest.EndHost;
begin
  { Alternative adapters release their private temporary resources here. }
end;

procedure TNyxFileSaveRequest.Cancel;
var
  LLease: INyxFileSaveRequest;
begin
  FScheduler.RequireUI;
  LLease := Self;
  FReply := nil;
  FLease := nil;
  FBytes := nil;
  FReader := nil;
  FScope := nil;
  EndHost;
end;

procedure TNyxFileSaveRequest.Complete(AOutcome: TNyxFileSaveOutcome;
  const AName: TNyxText; AByteCount: Integer; const AError: TNyxText);
var
  LLease: INyxFileSaveRequest;
  LReply: TNyxFileSaveReply;
  LResult: TNyxFileSaveResult;
  LReader: INyxByteReader;
begin
  FScheduler.RequireUI;
  LLease := Self;

  if not Pending then
  begin
    Cancel;
    Exit;
  end;
  NyxFileSaveOutcomeName(AOutcome);
  LResult := Default(TNyxFileSaveResult);
  LResult.Outcome := AOutcome;
  LResult.Name := AName;
  LResult.ByteCount := AByteCount;
  LResult.Error := AError;

  if FStreamed then
  begin

    if NyxFileSaveDelivered(LResult) and not FEOFChecked then
    begin
      raise ENyxFile.Create('Streaming save cannot report delivery before exact EOF');
    end;
    ValidateNyxStreamFileSaveResult(LResult, FExpectedBytes, FStreamOptions.ByteLimit);
  end
  else
  begin
    ValidateNyxFileSaveResult(LResult, FExpectedBytes);
  end;
  LReply := FReply;
  FReply := nil;
  FLease := nil;
  FBytes := nil;
  { Keep arbitrary cursor finalization after receiver delivery. A custom
    reader's last release must not retire the borrowed owner between cleanup
    and invoking the already admitted terminal reply. }
  LReader := FReader;
  FReader := nil;
  FScope := nil;
  EndHost;
  LReply(LResult);
end;

end.
