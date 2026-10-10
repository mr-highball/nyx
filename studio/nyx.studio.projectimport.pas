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
unit nyx.studio.projectimport;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.studio.projects;

const
  { Matches the portable JSON admission ceiling. Transfer windows are Unicode
    scalars; the reservation and complete file limit are exact UTF-8 bytes. }
  NyxMaximumProjectImportBytes = 4194304;
  NyxMaximumProjectImportChunk = 4096;
  NyxMaximumProjectImportChunks = 4096;

type
  { Copied, typed admission context. Counts describe the resolved candidate,
    never the active project. Changed flags disclose canonical design/source
    regeneration. Pending retains its exact saved draft and source baseline. }
  TNyxProjectImportSummary = record
    Title: TNyxText;
    Pages: Integer;
    Components: Integer;
    Resources: Integer;
    StateDefaults: Integer;
    Collections: Integer;
    SourceScalars: Integer;
    DraftScalars: Integer;
    DraftBaseScalars: Integer;
    Pending: Boolean;
    DesignChanged: Boolean;
    SourceChanged: Boolean;
    Resolution: TNyxProjectResolution;
  end;

  { Immutable managed value. No borrowed document, source workspace, renderer,
    session or compiler profile survives review. Applying remains the caller's
    explicit ordinary Studio history operation, after authority/revision checks. }
  INyxProjectImportCandidate = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001004004001}']
    function Pair: TNyxProjectPair;
    function Summary: TNyxProjectImportSummary;
  end;

  { Immutable bounded upload. Append returns a new owner and retains this value
    on refusal. Exact scalar offsets forbid missing/reordered chunks. Chunks are
    stored separately and joined only for complete admission, avoiding quadratic
    whole-file string concatenation. Cancellation/revision expiry belongs to the
    transport owner; this contract knows no filenames or hosted URLs. }
  INyxProjectImportUpload = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001004004002}']
    function ExpectedBytes: Integer;
    function ReceivedBytes: Integer;
    function ScalarCount: Integer;
    function Complete: Boolean;
    function Append(AOffset: Integer; const AText: TNyxText): INyxProjectImportUpload;
    { Exact original input, available only when the declared byte count arrives.
      Read malformed/conflicting input before choosing a deliberate resolution. }
    function Packet: TNyxText;
    function Review(AResolution: TNyxProjectResolution): INyxProjectImportCandidate;
  end;

{ Rejects invalid byte reservations before allocating upload storage. }
function NewNyxProjectImport(AExpectedBytes: Integer): INyxProjectImportUpload;

implementation

uses
  SysUtils, nyx.model, nyx.source, nyx.bytes, nyx.editing;

type
  TNyxProjectImportCandidate = class(TInterfacedObject, INyxProjectImportCandidate)
  private
    FPair: TNyxProjectPair;
    FSummary: TNyxProjectImportSummary;
  public
    constructor Create(const APacket: TNyxText; AResolution: TNyxProjectResolution);
    function Pair: TNyxProjectPair;
    function Summary: TNyxProjectImportSummary;
  end;

  TNyxProjectImportUpload = class(TInterfacedObject, INyxProjectImportUpload)
  private
    FExpectedBytes: Integer;
    FReceivedBytes: Integer;
    FScalarCount: Integer;
    FChunks: array of TNyxText;
  public
    constructor Create(AExpectedBytes: Integer);
    function ExpectedBytes: Integer;
    function ReceivedBytes: Integer;
    function ScalarCount: Integer;
    function Complete: Boolean;
    function Append(AOffset: Integer; const AText: TNyxText): INyxProjectImportUpload;
    function Packet: TNyxText;
    function Review(AResolution: TNyxProjectResolution): INyxProjectImportCandidate;
  end;

constructor TNyxProjectImportCandidate.Create(const APacket: TNyxText;
  AResolution: TNyxProjectResolution);
var
  LInput: TNyxProjectPair;
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
begin
  inherited Create;
  LInput := DecodeNyxProject(APacket);
  LDocument := nil;
  LWorkspace := nil;
  try
    AdmitNyxProject(LInput, AResolution, LDocument, LWorkspace, FPair);
    { Ordinary session adoption renders its admitted managed builder. Capture
      that exact source here so the review describes the pair actually installed,
      including a no-op saved draft that ordinary Studio clears. }
    FPair.Source := LWorkspace.Render(LDocument);
    FPair.Pending := FPair.Pending and (FPair.Draft <> FPair.Source);

    if not FPair.Pending then
    begin
      FPair.Draft := '';
      FPair.DraftBase := '';
    end;
    FSummary.Title := LDocument.Title;
    FSummary.Pages := LDocument.Count;
    FSummary.Components := LDocument.ComponentCount;
    FSummary.Resources := LDocument.Resources.Count;
    FSummary.StateDefaults := LDocument.State.Count;
    FSummary.Collections := LDocument.Collections.Count;
    FSummary.SourceScalars := NyxTextScalarCount(FPair.Source);
    FSummary.DraftScalars := NyxTextScalarCount(FPair.Draft);
    FSummary.DraftBaseScalars := NyxTextScalarCount(FPair.DraftBase);
    FSummary.Pending := FPair.Pending;
    FSummary.DesignChanged := FPair.Design <> LInput.Design;
    FSummary.SourceChanged := FPair.Source <> LInput.Source;
    FSummary.Resolution := AResolution;
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

function TNyxProjectImportCandidate.Pair: TNyxProjectPair;
begin
  Result := FPair;
end;

function TNyxProjectImportCandidate.Summary: TNyxProjectImportSummary;
begin
  Result := FSummary;
end;

constructor TNyxProjectImportUpload.Create(AExpectedBytes: Integer);
begin
  inherited Create;

  if (AExpectedBytes < 1) or (AExpectedBytes > NyxMaximumProjectImportBytes) then
  begin
    raise ENyxModel.Create('Project import must reserve 1..4194304 UTF-8 bytes');
  end;
  FExpectedBytes := AExpectedBytes;
end;

function TNyxProjectImportUpload.ExpectedBytes: Integer;
begin
  Result := FExpectedBytes;
end;

function TNyxProjectImportUpload.ReceivedBytes: Integer;
begin
  Result := FReceivedBytes;
end;

function TNyxProjectImportUpload.ScalarCount: Integer;
begin
  Result := FScalarCount;
end;

function TNyxProjectImportUpload.Complete: Boolean;
begin
  Result := FReceivedBytes = FExpectedBytes;
end;

function TNyxProjectImportUpload.Append(AOffset: Integer;
  const AText: TNyxText): INyxProjectImportUpload;
var
  LScalars: Integer;
  LBytes: Integer;
  LNext: TNyxProjectImportUpload;
begin

  if AOffset <> FScalarCount then
  begin
    raise ENyxModel.Create('Project import offset must equal received Unicode scalars');
  end;
  { Bound target storage units before scanning adversarial text. A valid chunk
    cannot use more than four UTF-8 bytes/two UTF-16 units per admitted scalar. }

  if Length(AText) > NyxMaximumProjectImportChunk * 4 then
  begin
    raise ENyxModel.Create('Project import chunk exceeds its text window');
  end;
  LScalars := NyxTextScalarCount(AText);
  LBytes := NyxUTF8ByteCount(AText);

  if (LScalars < 1) or (LScalars > NyxMaximumProjectImportChunk) or
    (LBytes > FExpectedBytes - FReceivedBytes) or
    (Length(FChunks) >= NyxMaximumProjectImportChunks) then
  begin
    raise ENyxModel.Create('Project import chunk exceeds the scalar, byte or chunk budget');
  end;
  LNext := TNyxProjectImportUpload.Create(FExpectedBytes);
  Result := LNext;
  LNext.FChunks := Copy(FChunks);
  SetLength(LNext.FChunks, Length(FChunks) + 1);
  LNext.FChunks[High(LNext.FChunks)] := AText;
  LNext.FReceivedBytes := FReceivedBytes + LBytes;
  LNext.FScalarCount := FScalarCount + LScalars;
end;

function TNyxProjectImportUpload.Packet: TNyxText;
var
  LParts: TNyxStrings;
  LIndex: Integer;
begin

  if not Complete then
  begin
    raise ENyxModel.Create('Project import has not received its declared UTF-8 byte count');
  end;
  LParts := TNyxStrings.Create;
  try
    for LIndex := 0 to High(FChunks) do
    begin
      LParts.Add(FChunks[LIndex]);
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function TNyxProjectImportUpload.Review(
  AResolution: TNyxProjectResolution): INyxProjectImportCandidate;
begin
  Result := TNyxProjectImportCandidate.Create(Packet, AResolution);
end;

function NewNyxProjectImport(AExpectedBytes: Integer): INyxProjectImportUpload;
begin
  Result := TNyxProjectImportUpload.Create(AExpectedBytes);
end;

end.
