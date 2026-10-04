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
unit nyx.gestures;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, Math, nyx.text, nyx.data;

const
  { Admission budgets are portable scalar/item counts, independent of the
    platform's UTF-8/UTF-16 representation. Transfers never retain native files
    or DOM DataTransfer handles; file observations describe metadata only. }
  MaximumNyxTransferItems = 16;
  MaximumNyxTransferScalars = 65536;
  MaximumNyxTransferFiles = 64;

type
  ENyxGesture = class(Exception);
  { Renderer diagnostics own identities/text and borrow no component. A sink may
    navigate or dispose its view; adapters stop using borrowed handles after it. }
  TNyxGestureFailure = procedure(const AOriginID, AReason: TNyxText) of object;
  TNyxDropOperation = (ndoNone, ndoCopy, ndoMove, ndoLink);
  TNyxDropOperations = set of TNyxDropOperation;
  TNyxDragPhase = (ndpStart, ndpDrag, ndpEnter, ndpOver, ndpExit, ndpDrop, ndpEnd);
  TNyxPointerRequest = (nprUnchanged, nprCapture, nprRelease);
  TNyxGestureCapability = (ngcCapturePointer, ngcReleasePointer,
    ngcOfferDrag, ngcAcceptDrop);
  TNyxGestureCapabilities = set of TNyxGestureCapability;

  { An open, validated MIME identity. Application authoring normally uses the
    named format constructors below. Exact format matching uses canonical ASCII
    lowercase tokens, with no parameters or platform-private magic strings. }
  TNyxTransferFormatRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;

  { Owned metadata, not a file-content capability or local path. Size is an exact
    nonnegative integer representable by both compilers; Modified is epoch
    milliseconds. Protected hover observations advertise files without exposing
    their names, sizes or content. }
  TNyxTransferFileInfo = record
    Name: TNyxText;
    MediaType: TNyxText;
    Size: Double;
    Modified: Double;
  end;
  TNyxTransferFiles = array of TNyxTransferFileInfo;

  { Immutable transfer data. A protected snapshot retains formats only: reading
    values or file metadata raises explicitly, rather than inventing empty data.
    A present empty string differs from an absent format. WithItem/WithFiles
    return independent values and never mutate a retained callback snapshot. }
  TNyxTransferSnapshot = record
  private
    FItems: TNyxDataValue;
    FReadable: Boolean;
    FFileAdvertised: Boolean;
    FFiles: TNyxTransferFiles;
    function GetDefined: Boolean;
    function GetReadable: Boolean;
    function GetCount: Integer;
    function GetFormat(AIndex: Integer): TNyxTransferFormatRef;
    function GetHasFiles: Boolean;
    function GetFileCount: Integer;
    function GetFile(AIndex: Integer): TNyxTransferFileInfo;
  public
    function HasFormat(const AFormat: TNyxTransferFormatRef): Boolean;
    function TextFor(const AFormat: TNyxTransferFormatRef): TNyxText;
    function Value: TNyxDataValue;
    function WithItem(const AItem: TNyxTransferSnapshot): TNyxTransferSnapshot;
    function WithFiles(const AFiles: array of TNyxTransferFileInfo): TNyxTransferSnapshot;
    function ProtectedCopy: TNyxTransferSnapshot;
    property Defined: Boolean read GetDefined;
    property Readable: Boolean read GetReadable;
    property Count: Integer read GetCount;
    property Formats[AIndex: Integer]: TNyxTransferFormatRef read GetFormat;
    property HasFiles: Boolean read GetHasFiles;
    property FileCount: Integer read GetFileCount;
    property Files[AIndex: Integer]: TNyxTransferFileInfo read GetFile;
  end;

  { Physical drag observation with immutable transfer context. Start and Drop
    permit data access; hover, leave and end expose protected formats. Allowed
    operations come from the source, Operation is the host's current proposal or
    final result. CanRespond describes an actual synchronous negotiation window. }
  TNyxDragSnapshot = record
  private
    FTransfer: TNyxTransferSnapshot;
    FPhase: TNyxDragPhase;
    FAllowed: TNyxDropOperations;
    FOperation: TNyxDropOperation;
    FSourceID: TNyxText;
    FCanRespond: Boolean;
    function GetDefined: Boolean;
  public
    property Defined: Boolean read GetDefined;
    property Phase: TNyxDragPhase read FPhase;
    property Transfer: TNyxTransferSnapshot read FTransfer;
    property Allowed: TNyxDropOperations read FAllowed;
    property Operation: TNyxDropOperation read FOperation;
    property SourceID: TNyxText read FSourceID;
    property CanRespond: Boolean read FCanRespond;
  end;

  { Sealed adapter result. Callback decisions are applied after synchronous
    dispatch, only while the original mounted view is still valid. Last valid
    sequential request wins; observations and queued work cannot make requests. }
  TNyxGestureResult = record
  private
    FPointer: TNyxPointerRequest;
    FOffered: Boolean;
    FTransfer: TNyxTransferSnapshot;
    FAllowed: TNyxDropOperations;
    FAccepted: Boolean;
    FOperation: TNyxDropOperation;
  public
    property PointerRequest: TNyxPointerRequest read FPointer;
    property Offered: Boolean read FOffered;
    property Transfer: TNyxTransferSnapshot read FTransfer;
    property Allowed: TNyxDropOperations read FAllowed;
    property Accepted: Boolean read FAccepted;
    property Operation: TNyxDropOperation read FOperation;
  end;

  { Adapter boundary, containing no widget or renderer references. Seal ends the
    response window even if an execution interface or decision is retained. }
  INyxGestureDecision = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001008000001}']
    function CanRequest(ACapability: TNyxGestureCapability): Boolean;
    procedure CapturePointer;
    procedure ReleasePointer;
    procedure OfferDrag(const ATransfer: TNyxTransferSnapshot; AAllowed: TNyxDropOperations);
    procedure AcceptDrop(AOperation: TNyxDropOperation);
    function Seal: TNyxGestureResult;
  end;

function NyxTransferFormat(const AName: TNyxText): TNyxTransferFormatRef;
function NyxTextTransferFormat: TNyxTransferFormatRef;
function NyxMarkupTransferFormat: TNyxTransferFormatRef;
function NyxURITransferFormat: TNyxTransferFormatRef;
function NyxValueTransferFormat: TNyxTransferFormatRef;
function NyxTransferText(const AText: TNyxText): TNyxTransferSnapshot;
function NyxTransferMarkup(const AText: TNyxText): TNyxTransferSnapshot;
function NyxTransferURIs(const AText: TNyxText): TNyxTransferSnapshot;
function NyxTransferValue(const AValue: TNyxDataValue): TNyxTransferSnapshot;
function NyxTransferCustom(const AFormat: TNyxTransferFormatRef;
  const AText: TNyxText): TNyxTransferSnapshot;
{ Explicit adapter construction: array of objects with format and, when readable,
  text. Duplicate or malformed formats and oversized transfers fail atomically. }
function NyxTransferFromData(const AItems: TNyxDataValue; AReadable: Boolean;
  AHasFiles: Boolean = False): TNyxTransferSnapshot;
function NyxDragSnapshot(APhase: TNyxDragPhase; const ATransfer: TNyxTransferSnapshot;
  AAllowed: TNyxDropOperations; AOperation: TNyxDropOperation;
  const ASourceID: TNyxText; ACanRespond: Boolean): TNyxDragSnapshot;
function NewNyxGestureDecision(ACapabilities: TNyxGestureCapabilities;
  AAllowed: TNyxDropOperations = []): INyxGestureDecision;
function NyxDropOperationName(AOperation: TNyxDropOperation): TNyxText;
function NyxDropOperationsName(AOperations: TNyxDropOperations): TNyxText;
function TryNyxDropOperation(const AName: TNyxText; out AOperation: TNyxDropOperation): Boolean;
function TryNyxDropOperations(const AName: TNyxText; out AOperations: TNyxDropOperations): Boolean;

implementation

uses nyx.editing;

type
  TNyxGestureDecision = class(TInterfacedObject, INyxGestureDecision)
  private
    FActive: Boolean;
    FCapabilities: TNyxGestureCapabilities;
    FSourceAllowed: TNyxDropOperations;
    FResult: TNyxGestureResult;
    procedure Require(ACapability: TNyxGestureCapability);
  public
    constructor Create(ACapabilities: TNyxGestureCapabilities; AAllowed: TNyxDropOperations);
    function CanRequest(ACapability: TNyxGestureCapability): Boolean;
    procedure CapturePointer;
    procedure ReleasePointer;
    procedure OfferDrag(const ATransfer: TNyxTransferSnapshot; AAllowed: TNyxDropOperations);
    procedure AcceptDrop(AOperation: TNyxDropOperation);
    function Seal: TNyxGestureResult;
  end;

function NyxTransferFormat(const AName: TNyxText): TNyxTransferFormatRef;
var
  LIndex: Integer;
  LSlash: Integer;
  LCharacter: Char;
begin
  LSlash := 0;

  if (Length(AName) < 3) or (Length(AName) > 128) then
  begin
    raise ENyxGesture.Create('Transfer format requires a bounded MIME identity');
  end;
  for LIndex := 1 to Length(AName) do
  begin
    LCharacter := AName[LIndex];

    if LCharacter = '/' then
    begin
      Inc(LSlash);

      if (LIndex = 1) or (LIndex = Length(AName)) then
      begin
        raise ENyxGesture.Create('Transfer MIME identity requires type and subtype');
      end;
    end
    else if not (LCharacter in ['a'..'z', 'A'..'Z', '0'..'9', '!', '#', '$', '&',
      '^', '_', '.', '+', '-']) then
    begin
      raise ENyxGesture.Create('Transfer MIME identity contains unsupported characters');
    end;
  end;

  if LSlash <> 1 then
  begin
    raise ENyxGesture.Create('Transfer MIME identity requires exactly one slash');
  end;
  Result.FName := LowerCase(AName);
end;

function NyxTextTransferFormat: TNyxTransferFormatRef;
begin
  Result := NyxTransferFormat('text/plain');
end;

function NyxMarkupTransferFormat: TNyxTransferFormatRef;
begin
  Result := NyxTransferFormat('text/html');
end;

function NyxURITransferFormat: TNyxTransferFormatRef;
begin
  Result := NyxTransferFormat('text/uri-list');
end;

function NyxValueTransferFormat: TNyxTransferFormatRef;
begin
  Result := NyxTransferFormat('application/x-nyx+json');
end;

function TNyxTransferSnapshot.GetDefined: Boolean;
begin
  Result := FItems.Kind = ndArray;
end;

function TNyxTransferSnapshot.GetReadable: Boolean;
begin
  Result := GetDefined and FReadable;
end;

function TNyxTransferSnapshot.GetCount: Integer;
begin
  Result := 0;

  if GetDefined then
  begin
    Result := FItems.Count;
  end;
end;

function TNyxTransferSnapshot.GetFormat(AIndex: Integer): TNyxTransferFormatRef;
begin

  if (AIndex < 0) or (AIndex >= GetCount) then
  begin
    raise ENyxGesture.Create('Transfer format index is outside the snapshot');
  end;
  Result := NyxTransferFormat(FItems.Item(AIndex).Field('format').AsText);
end;

function TNyxTransferSnapshot.HasFormat(const AFormat: TNyxTransferFormatRef): Boolean;
var
  LIndex: Integer;
  LFormat: TNyxTransferFormatRef;
begin
  LFormat := NyxTransferFormat(AFormat.Name);
  Result := False;
  for LIndex := 0 to GetCount - 1 do
  begin

    if GetFormat(LIndex).Name = LFormat.Name then
    begin
      Exit(True);
    end;
  end;
end;

function TNyxTransferSnapshot.TextFor(const AFormat: TNyxTransferFormatRef): TNyxText;
var
  LIndex: Integer;
  LFormat: TNyxTransferFormatRef;
begin
  LFormat := NyxTransferFormat(AFormat.Name);

  if not GetReadable then
  begin
    raise ENyxGesture.Create('Transfer payload is unavailable in this protected phase');
  end;
  for LIndex := 0 to GetCount - 1 do
  begin

    if GetFormat(LIndex).Name = LFormat.Name then
    begin
      Exit(FItems.Item(LIndex).Field('text').AsText);
    end;
  end;
  raise ENyxGesture.Create('Requested transfer format is absent');
end;

function TNyxTransferSnapshot.Value: TNyxDataValue;
begin
  Result := TNyxDataValue.ParseJSON(TextFor(NyxValueTransferFormat));
end;

function TNyxTransferSnapshot.GetHasFiles: Boolean;
begin
  Result := GetDefined and FFileAdvertised;
end;

function TNyxTransferSnapshot.GetFileCount: Integer;
begin

  if not GetReadable then
  begin
    raise ENyxGesture.Create('File metadata is unavailable in this protected phase');
  end;
  Result := Length(FFiles);
end;

function TNyxTransferSnapshot.GetFile(AIndex: Integer): TNyxTransferFileInfo;
begin

  if (AIndex < 0) or (AIndex >= GetFileCount) then
  begin
    raise ENyxGesture.Create('Transfer file index is outside the snapshot');
  end;
  Result := FFiles[AIndex];
end;

function NyxTransferFromData(const AItems: TNyxDataValue; AReadable: Boolean;
  AHasFiles: Boolean): TNyxTransferSnapshot;
var
  LItems: array of TNyxDataValue;
  LItem: TNyxDataValue;
  LFormat: TNyxTransferFormatRef;
  LIndex: Integer;
  LPrevious: Integer;
  LScalars: Integer;
  LText: TNyxText;
begin

  if (AItems.Kind <> ndArray) or (AItems.Count > MaximumNyxTransferItems) then
  begin
    raise ENyxGesture.Create('Transfer requires a bounded item array');
  end;
  SetLength(LItems, AItems.Count);
  LScalars := 0;
  for LIndex := 0 to AItems.Count - 1 do
  begin
    LItem := AItems.Item(LIndex);

    if LItem.Kind <> ndObject then
    begin
      raise ENyxGesture.Create('Transfer item requires a format object');
    end;
    LFormat := NyxTransferFormat(LItem.Field('format').AsText);
    for LPrevious := 0 to LIndex - 1 do
    begin

      if LItems[LPrevious].Field('format').AsText = LFormat.Name then
      begin
        raise ENyxGesture.Create('Transfer contains a duplicate format');
      end;
    end;

    if AReadable then
    begin
      LText := LItem.Field('text').AsText;
      Inc(LScalars, NyxTextScalarCount(LText));

      if LScalars > MaximumNyxTransferScalars then
      begin
        raise ENyxGesture.Create('Transfer payload exceeds its Unicode scalar budget');
      end;
      LItems[LIndex] := NyxObject([NyxField('format', NyxData(LFormat.Name)),
        NyxField('text', NyxData(LText))]);
    end
    else
    begin
      LItems[LIndex] := NyxObject([NyxField('format', NyxData(LFormat.Name))]);
    end;
  end;
  Result := Default(TNyxTransferSnapshot);
  Result.FItems := NyxArray(LItems);
  Result.FReadable := AReadable;
  Result.FFileAdvertised := AHasFiles;
end;

function NyxTransferCustom(const AFormat: TNyxTransferFormatRef;
  const AText: TNyxText): TNyxTransferSnapshot;
begin
  Result := NyxTransferFromData(NyxArray([NyxObject([
    NyxField('format', NyxData(NyxTransferFormat(AFormat.Name).Name)),
    NyxField('text', NyxData(AText))])]), True);
end;

function NyxTransferText(const AText: TNyxText): TNyxTransferSnapshot;
begin
  Result := NyxTransferCustom(NyxTextTransferFormat, AText);
end;

function NyxTransferMarkup(const AText: TNyxText): TNyxTransferSnapshot;
begin
  Result := NyxTransferCustom(NyxMarkupTransferFormat, AText);
end;

function NyxTransferURIs(const AText: TNyxText): TNyxTransferSnapshot;
begin
  Result := NyxTransferCustom(NyxURITransferFormat, AText);
end;

function NyxTransferValue(const AValue: TNyxDataValue): TNyxTransferSnapshot;
begin
  Result := NyxTransferCustom(NyxValueTransferFormat, AValue.ToJSON);
end;

function TNyxTransferSnapshot.WithItem(const AItem: TNyxTransferSnapshot): TNyxTransferSnapshot;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LMatch: Integer;
  LOther: Integer;
begin

  if not GetReadable or not AItem.Readable or AItem.HasFiles then
  begin
    raise ENyxGesture.Create('Combining transfer items requires readable item snapshots without files');
  end;
  SetLength(LItems, GetCount);
  for LIndex := 0 to GetCount - 1 do
  begin
    LItems[LIndex] := FItems.Item(LIndex);
  end;
  for LIndex := 0 to AItem.Count - 1 do
  begin
    LMatch := -1;
    for LOther := 0 to High(LItems) do
    begin

      if LItems[LOther].Field('format').AsText = AItem.Formats[LIndex].Name then
      begin
        LMatch := LOther;
        Break;
      end;
    end;

    if LMatch < 0 then
    begin
      LMatch := Length(LItems);
      SetLength(LItems, LMatch + 1);
    end;
    LItems[LMatch] := AItem.FItems.Item(LIndex);
  end;
  Result := NyxTransferFromData(NyxArray(LItems), True, FFileAdvertised);
  Result.FFiles := Copy(FFiles);
end;

function TNyxTransferSnapshot.WithFiles(const AFiles: array of TNyxTransferFileInfo): TNyxTransferSnapshot;
var
  LIndex: Integer;
  LFile: TNyxTransferFileInfo;
begin

  if not GetReadable or (Length(AFiles) > MaximumNyxTransferFiles) then
  begin
    raise ENyxGesture.Create('File metadata requires a readable bounded transfer');
  end;
  Result := Self;
  SetLength(Result.FFiles, 0);
  SetLength(Result.FFiles, Length(AFiles));
  for LIndex := 0 to High(AFiles) do
  begin
    LFile := AFiles[LIndex];

    if (NyxTextScalarCount(LFile.Name) > 1024) or
      (NyxTextScalarCount(LFile.MediaType) > 128) or
      IsNan(LFile.Size) or IsInfinite(LFile.Size) or (LFile.Size < 0) or
      (LFile.Size > 9007199254740991.0) or (Frac(LFile.Size) <> 0) or
      IsNan(LFile.Modified) or IsInfinite(LFile.Modified) or
      (Abs(LFile.Modified) > 9007199254740991.0) or (Frac(LFile.Modified) <> 0) then
    begin
      raise ENyxGesture.Create('Transfer contains invalid file metadata');
    end;
    Result.FFiles[LIndex] := LFile;
  end;
  Result.FFileAdvertised := FFileAdvertised or (Length(AFiles) > 0);
end;

function TNyxTransferSnapshot.ProtectedCopy: TNyxTransferSnapshot;
begin

  if not GetDefined then
  begin
    raise ENyxGesture.Create('Cannot protect an undefined transfer');
  end;
  Result := NyxTransferFromData(FItems, False, FFileAdvertised);
end;

function TNyxDragSnapshot.GetDefined: Boolean;
begin
  Result := FTransfer.Defined;
end;

function NyxDragSnapshot(APhase: TNyxDragPhase; const ATransfer: TNyxTransferSnapshot;
  AAllowed: TNyxDropOperations; AOperation: TNyxDropOperation;
  const ASourceID: TNyxText; ACanRespond: Boolean): TNyxDragSnapshot;
begin

  if not ATransfer.Defined or (ndoNone in AAllowed) then
  begin
    raise ENyxGesture.Create('Drag requires defined transfer metadata and real allowed operations');
  end;
  NyxTextScalarCount(ASourceID);
  Result := Default(TNyxDragSnapshot);
  Result.FPhase := APhase;
  Result.FTransfer := ATransfer;

  if not (APhase in [ndpStart, ndpDrop]) then
  begin
    Result.FTransfer := ATransfer.ProtectedCopy;
  end;
  Result.FAllowed := AAllowed;
  Result.FOperation := AOperation;
  Result.FSourceID := ASourceID;
  Result.FCanRespond := ACanRespond and (APhase in [ndpStart, ndpEnter, ndpOver, ndpDrop]);
end;

constructor TNyxGestureDecision.Create(ACapabilities: TNyxGestureCapabilities;
  AAllowed: TNyxDropOperations);
begin
  inherited Create;

  if ndoNone in AAllowed then
  begin
    raise ENyxGesture.Create('None is not an allowed drop operation');
  end;
  FCapabilities := ACapabilities;
  FSourceAllowed := AAllowed;
  FActive := True;
  FResult := Default(TNyxGestureResult);
end;

function TNyxGestureDecision.CanRequest(ACapability: TNyxGestureCapability): Boolean;
begin
  Result := FActive and (ACapability in FCapabilities);
end;

procedure TNyxGestureDecision.Require(ACapability: TNyxGestureCapability);
begin

  if not CanRequest(ACapability) then
  begin
    raise ENyxGesture.Create('Gesture response is unavailable or its physical window has closed');
  end;
end;

procedure TNyxGestureDecision.CapturePointer;
begin
  Require(ngcCapturePointer);
  FResult.FPointer := nprCapture;
end;

procedure TNyxGestureDecision.ReleasePointer;
begin
  Require(ngcReleasePointer);
  FResult.FPointer := nprRelease;
end;

procedure TNyxGestureDecision.OfferDrag(const ATransfer: TNyxTransferSnapshot;
  AAllowed: TNyxDropOperations);
begin
  Require(ngcOfferDrag);

  if not ATransfer.Readable or ATransfer.HasFiles or
    (AAllowed = []) or (ndoNone in AAllowed) then
  begin
    raise ENyxGesture.Create('Offering a drag requires readable items and real allowed operations; native file handles are unsupported');
  end;
  FResult.FTransfer := ATransfer;
  FResult.FAllowed := AAllowed;
  FResult.FOffered := True;
end;

procedure TNyxGestureDecision.AcceptDrop(AOperation: TNyxDropOperation);
begin
  Require(ngcAcceptDrop);

  if (AOperation <> ndoNone) and not (AOperation in FSourceAllowed) then
  begin
    raise ENyxGesture.Create('Requested drop operation is not allowed by its source');
  end;
  FResult.FAccepted := True;
  FResult.FOperation := AOperation;
end;

function TNyxGestureDecision.Seal: TNyxGestureResult;
begin
  FActive := False;
  Result := FResult;
end;

function NewNyxGestureDecision(ACapabilities: TNyxGestureCapabilities;
  AAllowed: TNyxDropOperations): INyxGestureDecision;
begin
  Result := TNyxGestureDecision.Create(ACapabilities, AAllowed);
end;

function NyxDropOperationName(AOperation: TNyxDropOperation): TNyxText;
const
  CNames: array[TNyxDropOperation] of TNyxText = ('none', 'copy', 'move', 'link');
begin
  Result := CNames[AOperation];
end;

function NyxDropOperationsName(AOperations: TNyxDropOperations): TNyxText;
begin

  if ndoNone in AOperations then
  begin
    raise ENyxGesture.Create('None is not an allowed drop operation');
  end;
  Result := 'none';

  if AOperations = [ndoCopy] then
  begin
    Result := 'copy';
  end
  else if AOperations = [ndoMove] then
  begin
    Result := 'move';
  end
  else if AOperations = [ndoLink] then
  begin
    Result := 'link';
  end
  else if AOperations = [ndoCopy, ndoMove] then
  begin
    Result := 'copyMove';
  end
  else if AOperations = [ndoCopy, ndoLink] then
  begin
    Result := 'copyLink';
  end
  else if AOperations = [ndoMove, ndoLink] then
  begin
    Result := 'linkMove';
  end
  else if AOperations = [ndoCopy, ndoMove, ndoLink] then
  begin
    Result := 'all';
  end;
end;

function TryNyxDropOperation(const AName: TNyxText; out AOperation: TNyxDropOperation): Boolean;
var
  LOperation: TNyxDropOperation;
begin
  for LOperation := Low(TNyxDropOperation) to High(TNyxDropOperation) do
  begin

    if AName = NyxDropOperationName(LOperation) then
    begin
      AOperation := LOperation;
      Exit(True);
    end;
  end;
  AOperation := ndoNone;
  Result := False;
end;

function TryNyxDropOperations(const AName: TNyxText; out AOperations: TNyxDropOperations): Boolean;
var
  LBits: Integer;
begin
  for LBits := 0 to 7 do
  begin
    AOperations := [];

    if LBits and 1 <> 0 then
    begin
      Include(AOperations, ndoCopy);
    end;

    if LBits and 2 <> 0 then
    begin
      Include(AOperations, ndoMove);
    end;

    if LBits and 4 <> 0 then
    begin
      Include(AOperations, ndoLink);
    end;

    if AName = NyxDropOperationsName(AOperations) then
    begin
      Exit(True);
    end;
  end;

  if AName = 'uninitialized' then
  begin
    AOperations := [ndoCopy, ndoMove, ndoLink];
    Exit(True);
  end;
  AOperations := [];
  Result := False;
end;

end.
