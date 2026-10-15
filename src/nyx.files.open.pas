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

unit nyx.files.open;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.files, nyx.files.save,
  nyx.resources, nyx.scheduler;

const
  NyxFileOpenMaximumBytes = 4 * 1024 * 1024;

type
  { Reading is distinct from a user cancellation, missing activation, unsupported
    host capability or a read failure. Scope cancellation silently revokes replies. }
  TNyxFileOpenOutcome = (nfooRead, nfooCancelled, nfooActivationRequired,
    nfooUnavailable, nfooFailed);

  { Immutable hints for one user-selected file, never a machine path or grant.
    Extension filters guide the picker; they do not validate content or forbid a
    user's alternative choice. Byte limit includes zero and is checked before
    allocation/read. Construct with NyxFileOpenOptions; a zeroed record refuses. }
  TNyxFileOpenOptions = record
  private
    FTitle: TNyxText;
    FFilter: TNyxTextFileSelection;
    FMaximumBytes: Integer;
  public
    function Copy: TNyxFileOpenOptions;
    function Title(const AValue: TNyxText): TNyxFileOpenOptions;
    function Allow(const AExtension: TNyxText): TNyxFileOpenOptions;
    function MaximumBytes(AValue: Integer): TNyxFileOpenOptions;
    function ExtensionCount: Integer;
    function Extension(AIndex: Integer): TNyxText;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AValue: TNyxDataValue): TNyxFileOpenOptions; static;
    property DialogTitle: TNyxText read FTitle;
    property ByteLimit: Integer read FMaximumBytes;
  end;

  { Copied portable facts about an already-read snapshot. Name is a leaf, not a
    path. MIME and modification time are optional advisory host observations,
    never byte validation. Time is exact integral Unix milliseconds within the
    browser Date range; unknown values remain explicit rather than invented.
    This value contains no stream, application, platform object or interface. }
  TNyxFileMetadata = record
  private
    FName: TNyxText;
    FByteCount: Integer;
    FMediaType: TNyxMediaTypeRef;
    FModified: Double;
    FHasModified: Boolean;
    function GetHasMediaType: Boolean;
  public
    function MediaType(const AValue: TNyxMediaTypeRef): TNyxFileMetadata;
    function ModifiedMilliseconds(AValue: Double): TNyxFileMetadata;
    procedure Validate;
    function ToData: TNyxDataValue;
    property Name: TNyxText read FName;
    property ByteCount: Integer read FByteCount;
    property ContentType: TNyxMediaTypeRef read FMediaType;
    property HasContentType: Boolean read GetHasMediaType;
    property HasModified: Boolean read FHasModified;
    property LastModifiedMilliseconds: Double read FModified;
  end;

  { Immutable byte snapshot after selection/read. Bytes returns an independent
    array on both compilers. Conversion to a packed resource is explicit and
    retains that resource kind's own validation and smaller packing budget.
    No permission, path or handle can be recovered from this value. }
  INyxFileSnapshot = interface(IInterface)
    ['{60CAFF21-C6E4-43D0-8855-940000000010}']
    function Metadata: TNyxFileMetadata;
    function Bytes: TNyxBytes;
    function AsResource(AKind: TNyxResourceKind): INyxResourceDefinition;
  end;

  { Borrowed UI receiver, called at most once while its request remains active.
    Payload is nil for all outcomes except Read. The receiver must cancel before
    disposal. A referenced snapshot can safely outlive its request or application. }
  TNyxFileOpenReply = procedure(AOutcome: TNyxFileOpenOutcome;
    const AFile: INyxFileSnapshot; const AError: TNyxText) of object;

  { One independent operation. Cancel silently revokes its borrowed receiver;
    native background I/O may finish cooperatively, touching no UI/request owner. }
  INyxFileOpenRequest = interface(IInterface)
    ['{60CAFF21-C6E4-43D0-8855-940000000011}']
    procedure Cancel;
  end;

  { Explicit runtime selection/read authority. Options are copied before host
    effects; each operation returns an independent token and may finish inline.
    Browser selection retains actual activation rules. Native reads must use a
    worker or report unavailable, rather than silently blocking the UI on I/O. }
  INyxFileOpenHost = interface(IInterface)
    ['{60CAFF21-C6E4-43D0-8855-940000000012}']
    function BeginOpen(const AOptions: TNyxFileOpenOptions;
      const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
      AReply: TNyxFileOpenReply): INyxFileOpenRequest;
  end;

  { Shared UI-only operation lifetime for target/creator hosts. Complete seals
    an independent snapshot before revoking callbacks and host presentation.
    Self-retention covers pending platform callbacks; subclasses must retire
    them. EndHost is idempotent and never calls the client. Workers must retain
    pure channels, not this request or any borrowed widget/receiver. }
  TNyxFileOpenRequest = class(TInterfacedObject, INyxFileOpenRequest)
  private
    FReply: TNyxFileOpenReply;
    FLease: INyxFileOpenRequest;
  protected
    FOptions: TNyxFileOpenOptions;
    FScheduler: INyxScheduler;
    FScope: INyxCancellationScope;
    function Pending: Boolean;
    procedure RetainPending;
    { Admit one delayed UI observation. Skipped admitted work cancels on scheduler
      retirement; a request never owns its watcher, avoiding a permanent cycle. }
    procedure Watch;
    procedure Observe; virtual;
    procedure EndHost; virtual;
    procedure Complete(AOutcome: TNyxFileOpenOutcome;
      const AFile: INyxFileSnapshot = nil; const AError: TNyxText = '');
  public
    constructor Create(const AOptions: TNyxFileOpenOptions;
      const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
      AReply: TNyxFileOpenReply);
    procedure Cancel; virtual;
  end;

function NyxFileOpenOptions: TNyxFileOpenOptions;
function NyxFileMetadata(const AName: TNyxText; AByteCount: Integer): TNyxFileMetadata;
function NyxFileSnapshot(const AMetadata: TNyxFileMetadata;
  const ABytes: TNyxBytes): INyxFileSnapshot;
function NyxFileOpenOutcomeName(AValue: TNyxFileOpenOutcome): TNyxText;
{ Revalidate and detach a foreign host result before ordinary graph publication.
  Invalid outcome/payload combinations and over-budget diagnostics refuse. }
function AdmitNyxFileOpenResult(AOutcome: TNyxFileOpenOutcome;
  const AFile: INyxFileSnapshot; const AError: TNyxText;
  AMaximumBytes: Integer): INyxFileSnapshot;

implementation

uses Math, nyx.flow.types;

type
  { One admitted delayed UI turn retains its request without a back reference.
    Failed admission does not cancel by destruction; skipped admitted work does.
    The scheduler guarantees this courier's retirement on its owning UI thread. }
  TOpenWatch = class(TInterfacedObject, INyxWork)
  public
    Owner: TNyxFileOpenRequest;
    Lease: INyxFileOpenRequest;
    Entered: Boolean;
    Queued: Boolean;
    destructor Destroy; override;
    procedure Execute(const AExecution: INyxExecution);
  end;

  { Reference-counted immutable pure data. Its owned bytes are detached on entry
    and every read; metadata is a copied value, never a borrowed host capability. }
  TSnapshot = class(TInterfacedObject, INyxFileSnapshot)
  private
    FMetadata: TNyxFileMetadata;
    FBytes: TNyxBytes;
  public
    constructor Create(const AMetadata: TNyxFileMetadata; const ABytes: TNyxBytes);
    function Metadata: TNyxFileMetadata;
    function Bytes: TNyxBytes;
    function AsResource(AKind: TNyxResourceKind): INyxResourceDefinition;
  end;

function NyxFileOpenOptions: TNyxFileOpenOptions;
begin
  Result := Default(TNyxFileOpenOptions);
  Result.FTitle := 'Open file';
  Result.FFilter := NyxTextFileSelection.UpToFiles(1);
  Result.FMaximumBytes := 1024 * 1024;
end;

function TNyxFileOpenOptions.Copy: TNyxFileOpenOptions;
var
  LIndex: Integer;
begin
  Result := Self;
  Result.FFilter := NyxTextFileSelection.UpToFiles(1);
  for LIndex := 0 to FFilter.ExtensionCount - 1 do
  begin
    Result.FFilter := Result.FFilter.Allow(FFilter.Extension(LIndex));
  end;
end;

function TNyxFileOpenOptions.Title(const AValue: TNyxText): TNyxFileOpenOptions;
begin
  Result := Copy;
  Result.FTitle := AValue;
  Result.Validate;
end;

function TNyxFileOpenOptions.Allow(const AExtension: TNyxText): TNyxFileOpenOptions;
begin

  if FFilter.ExtensionCount >= 16 then
  begin
    raise ENyxFile.Create('Use at most sixteen file extension hints');
  end;
  Result := Copy;
  Result.FFilter := Result.FFilter.Allow(AExtension);
  Result.Validate;
end;

function TNyxFileOpenOptions.MaximumBytes(AValue: Integer): TNyxFileOpenOptions;
begin
  Result := Copy;
  Result.FMaximumBytes := AValue;
  Result.Validate;
end;

function TNyxFileOpenOptions.ExtensionCount: Integer;
begin
  Result := FFilter.ExtensionCount;
end;

function TNyxFileOpenOptions.Extension(AIndex: Integer): TNyxText;
begin
  Result := FFilter.Extension(AIndex);
end;

procedure TNyxFileOpenOptions.Validate;
begin
  FFilter.Validate;

  if (FTitle = '') or (NyxUTF8ByteCount(FTitle) > 1024) or
    (Pos(#0, FTitle) > 0) or (FMaximumBytes < 0) or
    (FMaximumBytes > NyxFileOpenMaximumBytes) or (FFilter.ExtensionCount > 16) then
  begin
    raise ENyxFile.Create('Invalid file-open title, extension hints or byte budget');
  end;
end;

function TNyxFileOpenOptions.ToData: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  Validate;
  SetLength(LItems, ExtensionCount);
  for LIndex := 0 to High(LItems) do
  begin
    LItems[LIndex] := NyxData(Extension(LIndex));
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('title', NyxData(FTitle)), NyxField('maximumBytes', NyxData(FMaximumBytes)),
    NyxField('extensions', NyxArray(LItems))]);
end;

class function TNyxFileOpenOptions.FromData(const AValue: TNyxDataValue): TNyxFileOpenOptions;
var
  LItems: TNyxDataValue;
  LIndex: Integer;
begin
  ValidateNyxFlowFields(AValue, ['version', 'title', 'maximumBytes', 'extensions']);

  if AValue.Field('version').AsInteger <> 1 then
  begin
    raise ENyxFile.Create('Unknown file-open option version');
  end;
  Result := NyxFileOpenOptions.Title(AValue.Field('title').AsText)
    .MaximumBytes(AValue.Field('maximumBytes').AsInteger);
  LItems := AValue.Field('extensions');

  if (LItems.Kind <> ndArray) or (LItems.Count > 16) then
  begin
    raise ENyxFile.Create('Invalid file-open extension hints');
  end;
  for LIndex := 0 to LItems.Count - 1 do
  begin
    Result := Result.Allow(LItems.Item(LIndex).AsText);
  end;
end;

function NyxFileMetadata(const AName: TNyxText; AByteCount: Integer): TNyxFileMetadata;
begin
  Result := Default(TNyxFileMetadata);
  Result.FName := AName;
  Result.FByteCount := AByteCount;
  Result.Validate;
end;

function TNyxFileMetadata.GetHasMediaType: Boolean;
begin
  Result := FMediaType.Name <> '';
end;

function TNyxFileMetadata.MediaType(const AValue: TNyxMediaTypeRef): TNyxFileMetadata;
begin
  Result := Self;
  Result.FMediaType := NyxMediaType(AValue.Name);
  Result.Validate;
end;

function TNyxFileMetadata.ModifiedMilliseconds(AValue: Double): TNyxFileMetadata;
begin
  Result := Self;
  Result.FModified := AValue;
  Result.FHasModified := True;
  Result.Validate;
end;

procedure TNyxFileMetadata.Validate;
var
  LLeaf: INyxTextFile;
begin
  LLeaf := NyxTextFile(FName, '');

  if (FByteCount < 0) or (FByteCount > NyxFileOpenMaximumBytes) then
  begin
    raise ENyxFile.Create('Invalid file snapshot byte count');
  end;

  if FHasModified and (IsNan(FModified) or IsInfinite(FModified) or
    (Abs(FModified) > 8640000000000000.0) or (Frac(FModified) <> 0)) then
  begin
    raise ENyxFile.Create('File modification time requires exact integral Unix milliseconds');
  end;
end;

function TNyxFileMetadata.ToData: TNyxDataValue;
var
  LMedia: TNyxDataValue;
  LModified: TNyxDataValue;
begin
  Validate;
  LMedia := NyxNull;
  LModified := NyxNull;

  if HasContentType then
  begin
    LMedia := NyxData(FMediaType.Name);
  end;

  if FHasModified then
  begin
    LModified := NyxData(FModified);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('name', NyxData(FName)), NyxField('byteCount', NyxData(FByteCount)),
    NyxField('mediaType', LMedia), NyxField('lastModifiedMilliseconds', LModified)]);
end;

constructor TSnapshot.Create(const AMetadata: TNyxFileMetadata; const ABytes: TNyxBytes);
var
  LIndex: Integer;
begin
  inherited Create;
  AMetadata.Validate;

  if Length(ABytes) <> AMetadata.ByteCount then
  begin
    raise ENyxFile.Create('Snapshot metadata must account for every byte');
  end;
  FMetadata := AMetadata;
  SetLength(FBytes, Length(ABytes));
  for LIndex := 0 to High(ABytes) do
  begin
    FBytes[LIndex] := ABytes[LIndex];
  end;
end;

function TSnapshot.Metadata: TNyxFileMetadata;
begin
  Result := FMetadata;
end;

function TSnapshot.Bytes: TNyxBytes;
var
  LIndex: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FBytes));
  for LIndex := 0 to High(FBytes) do
  begin
    Result[LIndex] := FBytes[LIndex];
  end;
end;

function TSnapshot.AsResource(AKind: TNyxResourceKind): INyxResourceDefinition;
begin
  Result := NyxResourceFromBytes(AKind, FBytes);
end;

function NyxFileSnapshot(const AMetadata: TNyxFileMetadata;
  const ABytes: TNyxBytes): INyxFileSnapshot;
begin
  Result := TSnapshot.Create(AMetadata, ABytes);
end;

function NyxFileOpenOutcomeName(AValue: TNyxFileOpenOutcome): TNyxText;
const
  CNames: array[TNyxFileOpenOutcome] of TNyxText =
    ('read', 'cancelled', 'activation-required', 'unavailable', 'failed');
begin

  if (Ord(AValue) < Ord(Low(TNyxFileOpenOutcome))) or
    (Ord(AValue) > Ord(High(TNyxFileOpenOutcome))) then
  begin
    raise ENyxFile.Create('Unknown file-open outcome');
  end;
  Result := CNames[AValue];
end;

function AdmitNyxFileOpenResult(AOutcome: TNyxFileOpenOutcome;
  const AFile: INyxFileSnapshot; const AError: TNyxText;
  AMaximumBytes: Integer): INyxFileSnapshot;
var
  LMetadata: TNyxFileMetadata;
  LBytes: TNyxBytes;
begin
  NyxFileOpenOutcomeName(AOutcome);
  Result := nil;

  if (AMaximumBytes < 0) or (AMaximumBytes > NyxFileOpenMaximumBytes) or
    (NyxUTF8ByteCount(AError) > 8192) then
  begin
    raise ENyxFile.Create('Invalid file-open result budget or diagnostic');
  end;

  if AOutcome = nfooRead then
  begin

    if (AFile = nil) or (AError <> '') then
    begin
      raise ENyxFile.Create('A completed file read requires a snapshot and no error');
    end;
    LMetadata := AFile.Metadata;
    LMetadata.Validate;

    if LMetadata.ByteCount > AMaximumBytes then
    begin
      raise ENyxFile.Create('Selected file exceeds the byte budget');
    end;
    LBytes := AFile.Bytes;
    Result := NyxFileSnapshot(LMetadata, LBytes);
  end
  else if AFile <> nil then
  begin
    raise ENyxFile.Create('An unread outcome cannot expose a file payload');
  end;
end;

constructor TNyxFileOpenRequest.Create(const AOptions: TNyxFileOpenOptions;
  const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
  AReply: TNyxFileOpenReply);
begin
  inherited Create;

  if (AScheduler = nil) or not Assigned(AReply) then
  begin
    raise ENyxFile.Create('File opening requires a scheduler and borrowed receiver');
  end;
  AScheduler.RequireUI;
  AOptions.Validate;
  FOptions := AOptions.Copy;
  FScheduler := AScheduler;
  FScope := AScope;
  FReply := AReply;
end;

function TNyxFileOpenRequest.Pending: Boolean;
begin
  Result := Assigned(FReply) and ((FScope = nil) or not FScope.Cancelled);
end;

destructor TOpenWatch.Destroy;
begin

  if Queued and not Entered then
  begin
    Owner.Cancel;
  end;
  inherited Destroy;
end;

procedure TOpenWatch.Execute(const AExecution: INyxExecution);
begin
  Entered := True;

  if not Owner.Pending or AExecution.Cancelled then
  begin
    Owner.Cancel;
    Exit;
  end;
  try
    Owner.Observe;
  except
    Owner.Complete(nfooFailed, nil, 'The file-open observer could not continue');
    raise;
  end;
end;

procedure TNyxFileOpenRequest.Watch;
var
  LDelays: INyxSchedulerDelays;
  LWatch: TOpenWatch;
  LWork: INyxWork;
begin
  NyxSchedulerTurns(FScheduler);
  LDelays := NyxSchedulerDelays(FScheduler);
  LWatch := TOpenWatch.Create;
  LWork := LWatch;
  LWatch.Owner := Self;
  LWatch.Lease := Self;
  LDelays.PostAfter(LWork, 25);
  LWatch.Queued := True;
end;

procedure TNyxFileOpenRequest.Observe;
begin
  Watch;
end;

procedure TNyxFileOpenRequest.RetainPending;
begin
  FLease := Self;
end;

procedure TNyxFileOpenRequest.EndHost;
begin
end;

procedure TNyxFileOpenRequest.Cancel;
var
  LLease: INyxFileOpenRequest;
begin
  FScheduler.RequireUI;
  LLease := Self;
  FReply := nil;
  FLease := nil;
  FScope := nil;
  EndHost;
end;

procedure TNyxFileOpenRequest.Complete(AOutcome: TNyxFileOpenOutcome;
  const AFile: INyxFileSnapshot; const AError: TNyxText);
var
  LLease: INyxFileOpenRequest;
  LFile: INyxFileSnapshot;
  LReply: TNyxFileOpenReply;
begin
  FScheduler.RequireUI;
  LLease := Self;

  if not Pending then
  begin
    Cancel;
    Exit;
  end;
  LFile := AdmitNyxFileOpenResult(AOutcome, AFile, AError, FOptions.ByteLimit);

  if not Pending then
  begin
    Cancel;
    Exit;
  end;
  LReply := FReply;
  FReply := nil;
  FLease := nil;
  FScope := nil;
  EndHost;
  LReply(AOutcome, LFile, AError);
end;

end.
