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
unit nyx.pascal.encoding;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.lexing;

type
  { Byte ownership distinguishes captured external bytes from canonical UTF-8
    produced from an existing authoring buffer. Neither grants file/process or
    compiler-admission authority; no disk encoding is inferred from text. }
  TNyxPascalByteOrigin = (pboCapturedBytes, pboAuthoringUTF8);
  { Byte coordinates are deliberately distinct from Pascal scalar spans. Empty
    EOF ranges are valid; construction refuses negative/overflowing ranges. }
  TNyxPascalByteSpan = record
  private
    FStart: Integer;
    FLength: Integer;
  public
    function Finish: Integer;
    property Start: Integer read FStart;
    property Length: Integer read FLength;
  end;
  { Open codec identity is descriptive, never an instruction to dispatch a
    decoder by a string. Callers pass an explicit codec object. Names normalize
    ASCII case and permit bounded letters/digits/dot/underscore/hyphen. }
  TNyxPascalEncodingRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  { Known codec families do not imply compiler acceptance. Custom codecs may
    supply other alphabets; byte width/Unicode validity remain checked. }
  TNyxPascalSourceEncodingKind = (psekUTF8, psekUTF16LE, psekUTF16BE, psekSingleByte, psekCustom);
  { Exact external byte snapshot. Getters never expose its mutable backing
    vector. Empty input is valid; all constructors cap it at four MiB. }
  INyxPascalSourceBytes = interface(IInterface)
    ['{EAFF81A0-7686-40FA-B87F-AD2ACBF81201}']
    function Count: Integer;
    function Origin: TNyxPascalByteOrigin;
    function ByteAt(AIndex: Integer): Byte;
    function Slice(const ASpan: TNyxPascalByteSpan): TNyxBytes;
    { Exact byte and explicit origin identity, not evidence of disk I/O. }
    function SameBytes(const AOther: INyxPascalSourceBytes): Boolean;
  end;
  { One checked scalar candidate. A codec consumes 1..255 original bytes per
    scalar, including any BOM as U+FEFF. Surrogates and out-of-range scalars
    refuse; codecs may not silently discard bytes or emit replacement text. }
  TNyxPascalDecodedScalar = record
    Scalar: Integer;
    Bytes: Integer;
  end;
  { Construction-only decoder callback. Input has read-only byte access, not a
    writable array. The completed view retains no codec/caller/cancellation.
    Read raises ENyxPascalSourceDecodeError for undecodable input; other failures
    remain failures, never an opaque fallback or a partially published view. }
  INyxPascalSourceCodec = interface(IInterface)
    ['{EAFF81A0-7686-40FA-B87F-AD2ACBF81202}']
    function Reference: TNyxPascalEncodingRef;
    function Kind: TNyxPascalSourceEncodingKind;
    function Read(const ABytes: INyxPascalSourceBytes; AOffset: Integer): TNyxPascalDecodedScalar;
  end;
  { Unicode map for an explicitly selected single-byte alphabet. -1 means that
    byte is unsupported. Every other entry must be a valid Unicode scalar. }
  TNyxPascalSingleByteMap = array[Byte] of Integer;
  { Undecodable input has an exact original byte offset. The error retains no
    packet, codec or private host path. Budget/cancellation failures are separate
    and must never be converted into an opaque-source fallback. }
  ENyxPascalSourceDecodeError = class(ENyxBytes)
  private
    FOffset: Integer;
  public
    constructor AtByte(AOffset: Integer);
    property ByteOffset: Integer read FOffset;
  end;
  { A byte/work/text limit refuses the whole decode; original input stays owned. }
  ENyxPascalSourceDecodeLimit = class(ENyxBytes);
  { Detached readable interpretation plus exact original coordinates. Original
    bytes and BOMs remain intact. ScalarBoundaryAt returns -1 inside an encoded
    scalar; it never rounds to a nearby character. Getters are checked and pure. }
  INyxPascalSourceView = interface(IInterface)
    ['{EAFF81A0-7686-40FA-B87F-AD2ACBF81203}']
    function Original: INyxPascalSourceBytes;
    function Encoding: TNyxPascalEncodingRef;
    function Kind: TNyxPascalSourceEncodingKind;
    function Text: TNyxText;
    function Scalars: Integer;
    function ByteOffset(AScalar: Integer): Integer;
    function ScalarBoundaryAt(AByteOffset: Integer): Integer;
    function OriginalSpan(const ASpan: TNyxPascalSpan): TNyxPascalByteSpan;
    function SameView(const AOther: INyxPascalSourceView): Boolean;
  end;

{ Only coordinates/identity are constructed here; no I/O or decoder selection. }
function NyxPascalByteSpan(AStart, ALength: Integer): TNyxPascalByteSpan;
function NyxPascalEncoding(const AName: TNyxText): TNyxPascalEncodingRef;
{ Copy caller-serialized alternate bytes. Origin is retained as explicit metadata;
  the callback is not kept, and returned slices are independent mutable arrays. }
function CaptureNyxPascalSourceBytes(const ABytes: INyxPascalSourceBytes;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSourceBytes;
function NewNyxPascalSourceBytes(const ABytes: TNyxBytes;
  AOrigin: TNyxPascalByteOrigin = pboCapturedBytes;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSourceBytes;
{ UTF-8/UTF-16 preserve a BOM as a scalar. Single-byte decoding needs a supplied
  map; no system ANSI codepage or encoding-sniffing fallback is used. }
function NyxPascalUTF8Codec: INyxPascalSourceCodec;
function NyxPascalUTF16Codec(ABigEndian: Boolean = False): INyxPascalSourceCodec;
function NewNyxPascalSingleByteCodec(const AName: TNyxPascalEncodingRef;
  const AMap: TNyxPascalSingleByteMap): INyxPascalSourceCodec;
{ Decode once with bounded byte/Unicode work. Four MiB decoded UTF-8 and four MiB
  original bytes are independent hard limits. Limits/cancellation abort before
  publication. Sparse checkpoints and byte widths avoid a full integer map per
  scalar; fixed-width input needs neither width nor checkpoint vectors. }
function DecodeNyxPascalSource(const ABytes: INyxPascalSourceBytes;
  const ACodec: INyxPascalSourceCodec;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSourceView;
{ Freeze an alternate view's text, bytes, metadata and complete scalar partition.
  This validates coordinate/Unicode ownership, not the truth of a custom codec's
  interpretation. The host owns its explicit encoding declaration. }
function CaptureNyxPascalSourceView(const AView: INyxPascalSourceView;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSourceView;
{ Exact immutable text owner prepared before an owned decoded view publishes.
  Alternate views return nil; capture them explicitly first. This carries no
  original-byte/codec/compiler authority into lexical token donation. }
function NyxPascalSourceLexicalInput(const AView: INyxPascalSourceView): INyxPascalLexicalInput;
{ Canonical authoring UTF-8 from one captured immutable text owner. Encoding and
  decoding validate that owner's actual text; the resulting view shares only
  its lexical text identity, never a caller-supplied byte/provenance claim. }
function NyxPascalSourceViewFromLexicalInput(const AInput: INyxPascalLexicalInput): INyxPascalSourceView;

implementation

uses SysUtils;

const
  CMaximumBytes = 4 * 1024 * 1024;
  CCheckpoint = 128;

type
  { Owns its copied byte vector. No accessor returns the backing array. }
  TSourceBytes = class(TInterfacedObject, INyxPascalSourceBytes)
  public
    Data: TNyxBytes;
    ByteOrigin: TNyxPascalByteOrigin;
    function Count: Integer;
    function Origin: TNyxPascalByteOrigin;
    function ByteAt(AIndex: Integer): Byte;
    function Slice(const ASpan: TNyxPascalByteSpan): TNyxBytes;
    function SameBytes(const AOther: INyxPascalSourceBytes): Boolean;
  end;
  { Stateless built-in Unicode decoder, or an owned immutable 256-entry explicit
    map. It borrows only the duration of each checked Read invocation. }
  TSourceCodec = class(TInterfacedObject, INyxPascalSourceCodec)
  public
    Name: TNyxPascalEncodingRef;
    EncodingKind: TNyxPascalSourceEncodingKind;
    Map: TNyxPascalSingleByteMap;
    function Reference: TNyxPascalEncodingRef;
    function Kind: TNyxPascalSourceEncodingKind;
    function Read(const ABytes: INyxPascalSourceBytes; AOffset: Integer): TNyxPascalDecodedScalar;
  end;
  { Owns detached text/bytes and a compact immutable scalar partition. Builders
    alone mutate vectors; no decoder or callback survives publication. }
  TSourceView = class(TInterfacedObject, INyxPascalSourceView)
  public
    Source: INyxPascalSourceBytes;
    EncodingName: TNyxPascalEncodingRef;
    EncodingKind: TNyxPascalSourceEncodingKind;
    SourceText: TNyxText;
    LexicalInput: INyxPascalLexicalInput;
    ScalarCount: Integer;
    Stride: Integer;
    Widths: TNyxBytes;
    Checkpoints: array of Integer;
    procedure AddWidth(AWidth, AByteOffset: Integer);
    procedure Finish;
    function Original: INyxPascalSourceBytes;
    function Encoding: TNyxPascalEncodingRef;
    function Kind: TNyxPascalSourceEncodingKind;
    function Text: TNyxText;
    function Scalars: Integer;
    function ByteOffset(AScalar: Integer): Integer;
    function ScalarBoundaryAt(AByteOffset: Integer): Integer;
    function OriginalSpan(const ASpan: TNyxPascalSpan): TNyxPascalByteSpan;
    function SameView(const AOther: INyxPascalSourceView): Boolean;
  end;

procedure Check(const ACancellation: INyxCancellationScope);
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Source byte interpretation cancelled');
  end;
end;

function ValidScalar(AValue: Integer): Boolean;
begin
  Result := (AValue >= 0) and (AValue <= $10FFFF) and
    not ((AValue >= $D800) and (AValue <= $DFFF));
end;

function NyxPascalByteSpan(AStart, ALength: Integer): TNyxPascalByteSpan;
begin

  if (AStart < 0) or (ALength < 0) or (ALength > High(Integer) - AStart) then
  begin
    raise ENyxBytes.Create('Source byte span exceeds its nonnegative coordinate domain');
  end;
  Result.FStart := AStart;
  Result.FLength := ALength;
end;

function TNyxPascalByteSpan.Finish: Integer;
begin
  Result := FStart + FLength;
end;

function NyxPascalEncoding(const AName: TNyxText): TNyxPascalEncodingRef;
var
  LAt: Integer;
begin

  if (Length(AName) < 1) or (Length(AName) > 128) then
  begin
    raise ENyxBytes.Create('A source codec identity needs 1 to 128 ASCII characters');
  end;
  for LAt := 1 to Length(AName) do
  begin

    if not (AName[LAt] in ['a'..'z', 'A'..'Z', '0'..'9', '.', '_', '-']) then
    begin
      raise ENyxBytes.Create('A source codec identity contains an unsupported character');
    end;
  end;
  Result.FName := NyxPascalASCIIKey(AName);
end;

constructor ENyxPascalSourceDecodeError.AtByte(AOffset: Integer);
begin
  inherited Create('Source bytes do not form a complete scalar in the selected encoding');
  FOffset := AOffset;
end;

function TSourceBytes.Count: Integer;
begin
  Result := Length(Data);
end;

function TSourceBytes.Origin: TNyxPascalByteOrigin;
begin
  Result := ByteOrigin;
end;

function TSourceBytes.ByteAt(AIndex: Integer): Byte;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxBytes.Create('Source byte index is outside its snapshot');
  end;
  Result := Data[AIndex];
end;

function TSourceBytes.Slice(const ASpan: TNyxPascalByteSpan): TNyxBytes;
var
  LAt: Integer;
begin
  Result := nil;

  if ASpan.Finish > Count then
  begin
    raise ENyxBytes.Create('Source byte slice is outside its snapshot');
  end;
  SetLength(Result, ASpan.Length);
  for LAt := 0 to ASpan.Length - 1 do
  begin
    Result[LAt] := Data[ASpan.Start + LAt];
  end;
end;

function TSourceBytes.SameBytes(const AOther: INyxPascalSourceBytes): Boolean;
var
  LAt: Integer;
begin

  if AOther = (Self as INyxPascalSourceBytes) then
  begin
    Exit(True);
  end;
  Result := False;

  if (AOther = nil) or (AOther.Count <> Count) or (AOther.Origin <> ByteOrigin) then
  begin
    Exit;
  end;
  for LAt := 0 to Count - 1 do
  begin

    if AOther.ByteAt(LAt) <> Data[LAt] then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function NewNyxPascalSourceBytes(const ABytes: TNyxBytes;
  AOrigin: TNyxPascalByteOrigin; const ACancellation: INyxCancellationScope): INyxPascalSourceBytes;
var
  LSource: TSourceBytes;
  LAt: Integer;
  LCancellation: INyxCancellationScope;
  LOwner: INyxPascalSourceBytes;
begin
  LCancellation := ACancellation;
  Check(LCancellation);

  if (Length(ABytes) > CMaximumBytes) or
    not (AOrigin in [pboCapturedBytes, pboAuthoringUTF8]) then
  begin
    raise ENyxPascalSourceDecodeLimit.Create('Original source exceeds its byte/origin bounds');
  end;
  LSource := TSourceBytes.Create;
  LOwner := LSource;
  LSource.ByteOrigin := AOrigin;
  SetLength(LSource.Data, Length(ABytes));
  for LAt := 0 to High(ABytes) do
  begin

    if LAt mod 1024 = 0 then
    begin
      Check(LCancellation);
    end;
    LSource.Data[LAt] := ABytes[LAt];
  end;
  Result := LOwner;
end;

function CaptureNyxPascalSourceBytes(const ABytes: INyxPascalSourceBytes;
  const ACancellation: INyxCancellationScope): INyxPascalSourceBytes;
var
  LCount: Integer;
  LAt: Integer;
  LValue: Integer;
  LSource: TSourceBytes;
  LCancellation: INyxCancellationScope;
  LOwner: INyxPascalSourceBytes;
begin
  LCancellation := ACancellation;
  Check(LCancellation);

  if ABytes = nil then
  begin
    raise ENyxBytes.Create('Original source bytes must be supplied explicitly');
  end;

  if ABytes is TSourceBytes then
  begin
    Exit(ABytes);
  end;
  LCount := ABytes.Count;

  if (LCount < 0) or (LCount > CMaximumBytes) then
  begin
    raise ENyxPascalSourceDecodeLimit.Create('Alternate source exceeds its byte bounds');
  end;
  LSource := TSourceBytes.Create;
  LOwner := LSource;
  LSource.ByteOrigin := ABytes.Origin;

  if not (LSource.ByteOrigin in [pboCapturedBytes, pboAuthoringUTF8]) then
  begin
    raise ENyxBytes.Create('Alternate source has an unsupported byte origin');
  end;
  SetLength(LSource.Data, LCount);
  for LAt := 0 to LCount - 1 do
  begin

    if LAt mod 1024 = 0 then
    begin
      Check(LCancellation);
    end;
    LValue := ABytes.ByteAt(LAt);

    if (LValue < 0) or (LValue > 255) then
    begin
      raise ENyxBytes.Create('Alternate source returned a value outside a byte');
    end;
    LSource.Data[LAt] := Byte(LValue);
  end;
  Result := LOwner;
end;

function TSourceCodec.Reference: TNyxPascalEncodingRef;
begin
  Result := Name;
end;

function TSourceCodec.Kind: TNyxPascalSourceEncodingKind;
begin
  Result := EncodingKind;
end;

function TSourceCodec.Read(const ABytes: INyxPascalSourceBytes;
  AOffset: Integer): TNyxPascalDecodedScalar;
var
  LFirst: Integer;
  LNext: Integer;
  LAt: Integer;
  LMinimum: Integer;

  function WordAt(AAt: Integer): Integer;
  begin

    if AAt > ABytes.Count - 2 then
    begin
      raise ENyxPascalSourceDecodeError.AtByte(AOffset);
    end;
    Result := ABytes.ByteAt(AAt) + Integer(ABytes.ByteAt(AAt + 1)) * 256;

    if EncodingKind = psekUTF16BE then
    begin
      Result := ABytes.ByteAt(AAt + 1) + Integer(ABytes.ByteAt(AAt)) * 256;
    end;
  end;

begin

  if (ABytes = nil) or (AOffset < 0) or (AOffset >= ABytes.Count) then
  begin
    raise ENyxPascalSourceDecodeError.AtByte(AOffset);
  end;
  Result.Bytes := 1;
  LFirst := ABytes.ByteAt(AOffset);

  if EncodingKind = psekSingleByte then
  begin
    Result.Scalar := Map[LFirst];
  end
  else if EncodingKind in [psekUTF16LE, psekUTF16BE] then
  begin
    LFirst := WordAt(AOffset);
    Result.Bytes := 2;
    Result.Scalar := LFirst;

    if (LFirst >= $D800) and (LFirst <= $DBFF) then
    begin
      LNext := WordAt(AOffset + 2);

      if (LNext < $DC00) or (LNext > $DFFF) then
      begin
        raise ENyxPascalSourceDecodeError.AtByte(AOffset);
      end;
      Result.Scalar := $10000 + (LFirst - $D800) * 1024 + LNext - $DC00;
      Result.Bytes := 4;
    end;
  end
  else
  begin
    Result.Scalar := LFirst;
    LMinimum := 0;

    if LFirst >= $C2 then
    begin

      if LFirst <= $DF then
      begin
        Result.Bytes := 2;
        Result.Scalar := LFirst and $1F;
        LMinimum := $80;
      end
      else if LFirst <= $EF then
      begin
        Result.Bytes := 3;
        Result.Scalar := LFirst and $0F;
        LMinimum := $800;
      end
      else if LFirst <= $F4 then
      begin
        Result.Bytes := 4;
        Result.Scalar := LFirst and $07;
        LMinimum := $10000;
      end
      else
      begin
        raise ENyxPascalSourceDecodeError.AtByte(AOffset);
      end;
    end
    else if LFirst >= $80 then
    begin
      raise ENyxPascalSourceDecodeError.AtByte(AOffset);
    end;

    if Result.Bytes > ABytes.Count - AOffset then
    begin
      raise ENyxPascalSourceDecodeError.AtByte(AOffset);
    end;
    for LAt := 1 to Result.Bytes - 1 do
    begin
      LNext := ABytes.ByteAt(AOffset + LAt);

      if (LNext < $80) or (LNext > $BF) then
      begin
        raise ENyxPascalSourceDecodeError.AtByte(AOffset);
      end;
      Result.Scalar := Result.Scalar * 64 + LNext - $80;
    end;

    if Result.Scalar < LMinimum then
    begin
      raise ENyxPascalSourceDecodeError.AtByte(AOffset);
    end;
  end;

  if not ValidScalar(Result.Scalar) then
  begin
    raise ENyxPascalSourceDecodeError.AtByte(AOffset);
  end;
end;

function NyxPascalUTF8Codec: INyxPascalSourceCodec;
var
  LCodec: TSourceCodec;
  LOwner: INyxPascalSourceCodec;
begin
  LCodec := TSourceCodec.Create;
  LOwner := LCodec;
  LCodec.Name := NyxPascalEncoding('utf-8');
  LCodec.EncodingKind := psekUTF8;
  Result := LOwner;
end;

function NyxPascalUTF16Codec(ABigEndian: Boolean): INyxPascalSourceCodec;
var
  LCodec: TSourceCodec;
  LOwner: INyxPascalSourceCodec;
begin
  LCodec := TSourceCodec.Create;
  LOwner := LCodec;
  LCodec.Name := NyxPascalEncoding('utf-16le');
  LCodec.EncodingKind := psekUTF16LE;

  if ABigEndian then
  begin
    LCodec.Name := NyxPascalEncoding('utf-16be');
    LCodec.EncodingKind := psekUTF16BE;
  end;
  Result := LOwner;
end;

function NewNyxPascalSingleByteCodec(const AName: TNyxPascalEncodingRef;
  const AMap: TNyxPascalSingleByteMap): INyxPascalSourceCodec;
var
  LCodec: TSourceCodec;
  LAt: Integer;
  LOwner: INyxPascalSourceCodec;
begin
  LCodec := TSourceCodec.Create;
  LOwner := LCodec;
  LCodec.Name := NyxPascalEncoding(AName.Name);
  LCodec.EncodingKind := psekSingleByte;
  for LAt := 0 to 255 do
  begin

    if (AMap[LAt] <> -1) and not ValidScalar(AMap[LAt]) then
    begin
      raise ENyxBytes.Create('A single-byte source map contains an invalid Unicode scalar');
    end;
    LCodec.Map[LAt] := AMap[LAt];
  end;
  Result := LOwner;
end;

procedure TSourceView.AddWidth(AWidth, AByteOffset: Integer);
var
  LAt: Integer;
  LCapacity: Integer;
begin

  if (AWidth < 1) or (AWidth > 255) or (ScalarCount >= CMaximumBytes) then
  begin
    raise ENyxPascalSourceDecodeLimit.Create('Decoded source exceeds its scalar/unit budget');
  end;

  if ScalarCount = 0 then
  begin
    Stride := AWidth;
  end
  else if (Stride > 0) and (Stride <> AWidth) then
  begin
    SetLength(Widths, ScalarCount);
    for LAt := 0 to ScalarCount - 1 do
    begin
      Widths[LAt] := Byte(Stride);
    end;
    { Earlier fixed-width checkpoints are arithmetic, so allocate them only
      when a varying width actually makes them necessary. }
    LCapacity := ScalarCount div CCheckpoint + 1;
    SetLength(Checkpoints, LCapacity + 64);
    for LAt := 0 to LCapacity - 1 do
    begin
      Checkpoints[LAt] := LAt * CCheckpoint * Stride;
    end;
    Stride := 0;
  end;

  if Stride = 0 then
  begin

    if ScalarCount = Length(Widths) then
    begin
      LCapacity := CMaximumBytes;

      if ScalarCount <= CMaximumBytes - 4096 then
      begin
        LCapacity := ScalarCount + 4096;
      end;
      SetLength(Widths, LCapacity);
    end;
    Widths[ScalarCount] := Byte(AWidth);
  end;

  if (Stride = 0) and (ScalarCount mod CCheckpoint = 0) then
  begin
    LAt := ScalarCount div CCheckpoint;

    if LAt >= Length(Checkpoints) then
    begin
      SetLength(Checkpoints, LAt + 64);
    end;
    Checkpoints[LAt] := AByteOffset;
  end;
  Inc(ScalarCount);
end;

procedure TSourceView.Finish;
begin
  LexicalInput := NewNyxPascalLexicalInput(SourceText);

  if Stride > 0 then
  begin
    SetLength(Checkpoints, 0);
  end
  else
  begin
    SetLength(Widths, ScalarCount);
    SetLength(Checkpoints, (ScalarCount + CCheckpoint - 1) div CCheckpoint);
  end;
end;

function TSourceView.Original: INyxPascalSourceBytes;
begin
  Result := Source;
end;

function TSourceView.Encoding: TNyxPascalEncodingRef;
begin
  Result := EncodingName;
end;

function TSourceView.Kind: TNyxPascalSourceEncodingKind;
begin
  Result := EncodingKind;
end;

function TSourceView.Text: TNyxText;
begin
  Result := SourceText;
end;

function TSourceView.Scalars: Integer;
begin
  Result := ScalarCount;
end;

function TSourceView.ByteOffset(AScalar: Integer): Integer;
var
  LAt: Integer;
begin

  if (AScalar < 0) or (AScalar > ScalarCount) then
  begin
    raise ENyxBytes.Create('Decoded source scalar boundary is outside its view');
  end;

  if AScalar = ScalarCount then
  begin
    Exit(Source.Count);
  end;

  if Stride > 0 then
  begin
    Exit(AScalar * Stride);
  end;
  LAt := AScalar div CCheckpoint;
  Result := Checkpoints[LAt];
  LAt := LAt * CCheckpoint;
  while LAt < AScalar do
  begin
    Inc(Result, Widths[LAt]);
    Inc(LAt);
  end;
end;

function TSourceView.ScalarBoundaryAt(AByteOffset: Integer): Integer;
var
  LLow: Integer;
  LHigh: Integer;
  LMid: Integer;
  LOffset: Integer;
begin

  if (AByteOffset < 0) or (AByteOffset > Source.Count) then
  begin
    raise ENyxBytes.Create('Original source byte boundary is outside its snapshot');
  end;
  LLow := 0;
  LHigh := ScalarCount;
  while LLow <= LHigh do
  begin
    LMid := LLow + (LHigh - LLow) div 2;
    LOffset := ByteOffset(LMid);

    if LOffset = AByteOffset then
    begin
      Exit(LMid);
    end;

    if LOffset < AByteOffset then
    begin
      LLow := LMid + 1;
    end
    else
    begin
      LHigh := LMid - 1;
    end;
  end;
  Result := -1;
end;

function TSourceView.OriginalSpan(const ASpan: TNyxPascalSpan): TNyxPascalByteSpan;
var
  LStart: Integer;
begin
  LStart := ByteOffset(ASpan.Start);
  Result := NyxPascalByteSpan(LStart, ByteOffset(ASpan.Finish) - LStart);
end;

function TSourceView.SameView(const AOther: INyxPascalSourceView): Boolean;
var
  LAt: Integer;
  LOwned: TSourceView;
begin

  if AOther = (Self as INyxPascalSourceView) then
  begin
    Exit(True);
  end;
  Result := False;

  if (AOther = nil) or (AOther.Scalars <> ScalarCount) or
    (AOther.Kind <> EncodingKind) or (AOther.Encoding.Name <> EncodingName.Name) or
    (AOther.Text <> SourceText) or not Source.SameBytes(AOther.Original) then
  begin
    Exit;
  end;

  if AOther is TSourceView then
  begin
    LOwned := AOther as TSourceView;

    if (Stride <> LOwned.Stride) or (Length(Widths) <> Length(LOwned.Widths)) then
    begin
      Exit;
    end;
    for LAt := 0 to High(Widths) do
    begin

      if Widths[LAt] <> LOwned.Widths[LAt] then
      begin
        Exit;
      end;
    end;
    Exit(True);
  end;
  for LAt := 0 to ScalarCount do
  begin

    if AOther.ByteOffset(LAt) <> ByteOffset(LAt) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function DecodeNyxPascalSource(const ABytes: INyxPascalSourceBytes;
  const ACodec: INyxPascalSourceCodec;
  const ACancellation: INyxCancellationScope): INyxPascalSourceView;
var
  LView: TSourceView;
  LCodec: INyxPascalSourceCodec;
  LCancellation: INyxCancellationScope;
  LUTF8: TNyxBytes;
  LCount: Integer;
  LAt: Integer;
  LCapacity: Integer;
  LScalar: TNyxPascalDecodedScalar;
  LOwner: INyxPascalSourceView;

  procedure Append(AValue: Byte);
  begin

    if LCount >= CMaximumBytes then
    begin
      raise ENyxPascalSourceDecodeLimit.Create('Decoded UTF-8 source exceeds four MiB');
    end;

    if LCount = Length(LUTF8) then
    begin
      LCapacity := CMaximumBytes;

      if LCount <= CMaximumBytes - 8192 then
      begin
        LCapacity := LCount + 8192;
      end;
      SetLength(LUTF8, LCapacity);
    end;
    LUTF8[LCount] := AValue;
    Inc(LCount);
  end;

begin
  LCancellation := ACancellation;
  Check(LCancellation);
  LCodec := ACodec;

  if LCodec = nil then
  begin
    raise ENyxBytes.Create('A decoded source needs an explicitly selected codec');
  end;
  LView := TSourceView.Create;
  LOwner := LView;
  LView.Source := CaptureNyxPascalSourceBytes(ABytes, LCancellation);
  LView.EncodingName := NyxPascalEncoding(LCodec.Reference.Name);
  LView.EncodingKind := LCodec.Kind;

  if not (LView.EncodingKind in [psekUTF8, psekUTF16LE, psekUTF16BE, psekSingleByte, psekCustom]) then
  begin
    raise ENyxBytes.Create('A source codec has an unsupported kind');
  end;
  LAt := 0;
  LCount := 0;
  while LAt < LView.Source.Count do
  begin
    Check(LCancellation);
    LScalar := LCodec.Read(LView.Source, LAt);

    if not ValidScalar(LScalar.Scalar) or (LScalar.Bytes < 1) or
      (LScalar.Bytes > 255) or (LScalar.Bytes > LView.Source.Count - LAt) then
    begin
      raise ENyxPascalSourceDecodeError.AtByte(LAt);
    end;
    LView.AddWidth(LScalar.Bytes, LAt);
    Inc(LAt, LScalar.Bytes);

    if LScalar.Scalar < $80 then
    begin
      Append(Byte(LScalar.Scalar));
    end
    else if LScalar.Scalar < $800 then
    begin
      Append(Byte($C0 or (LScalar.Scalar shr 6)));
      Append(Byte($80 or (LScalar.Scalar and $3F)));
    end
    else if LScalar.Scalar < $10000 then
    begin
      Append(Byte($E0 or (LScalar.Scalar shr 12)));
      Append(Byte($80 or ((LScalar.Scalar shr 6) and $3F)));
      Append(Byte($80 or (LScalar.Scalar and $3F)));
    end
    else
    begin
      Append(Byte($F0 or (LScalar.Scalar shr 18)));
      Append(Byte($80 or ((LScalar.Scalar shr 12) and $3F)));
      Append(Byte($80 or ((LScalar.Scalar shr 6) and $3F)));
      Append(Byte($80 or (LScalar.Scalar and $3F)));
    end;
  end;
  SetLength(LUTF8, LCount);
  LView.SourceText := NyxDecodeUTF8(LUTF8);
  LView.Finish;
  Check(LCancellation);
  { Native managed-result storage can alias the caller's assignment target.
    Publish only after construction; caught failures must expose no partial view. }
  Result := LOwner;
end;

function NyxPascalSourceLexicalInput(const AView: INyxPascalSourceView): INyxPascalLexicalInput;
var
  LView: TSourceView;
begin
  Result := nil;

  if AView is TSourceView then
  begin
    LView := AView as TSourceView;

    if (LView as INyxPascalSourceView) = AView then Result := LView.LexicalInput;
  end;
end;

function NyxPascalSourceViewFromLexicalInput(const AInput: INyxPascalLexicalInput): INyxPascalSourceView;
var
  LInput: INyxPascalLexicalInput;
  LView: INyxPascalSourceView;
begin
  LInput := CaptureNyxPascalLexicalInput(AInput);
  LView := DecodeNyxPascalSource(NewNyxPascalSourceBytes(NyxEncodeUTF8(LInput.Text),
    pboAuthoringUTF8), NyxPascalUTF8Codec);
  { This fresh view has not escaped. Its bytes/text were built solely from this
    exact owned input; there is no independent caller text to compare or trust. }
  (LView as TSourceView).LexicalInput := LInput;
  Result := LView;
end;

function CaptureNyxPascalSourceView(const AView: INyxPascalSourceView;
  const ACancellation: INyxCancellationScope): INyxPascalSourceView;
var
  LView: TSourceView;
  LCancellation: INyxCancellationScope;
  LCount: Integer;
  LAt: Integer;
  LScalar: Integer;
  LStorage: Integer;
  LOffset: Integer;
  LNext: Integer;
  LOwner: INyxPascalSourceView;
begin
  LCancellation := ACancellation;
  Check(LCancellation);

  if AView = nil then
  begin
    raise ENyxBytes.Create('An alternate source view must be supplied explicitly');
  end;

  if AView is TSourceView then
  begin
    Exit(AView);
  end;
  LCount := AView.Scalars;

  if (LCount < 0) or (LCount > CMaximumBytes) then
  begin
    raise ENyxPascalSourceDecodeLimit.Create('Alternate decoded source exceeds its scalar bound');
  end;
  LView := TSourceView.Create;
  LOwner := LView;
  LView.Source := CaptureNyxPascalSourceBytes(AView.Original, LCancellation);
  LView.EncodingName := NyxPascalEncoding(AView.Encoding.Name);
  LView.EncodingKind := AView.Kind;
  LView.SourceText := AView.Text;

  if (NyxUTF8ByteCount(LView.SourceText) > CMaximumBytes) or
    not (LView.EncodingKind in [psekUTF8, psekUTF16LE, psekUTF16BE, psekSingleByte, psekCustom]) then
  begin
    raise ENyxPascalSourceDecodeLimit.Create('Alternate decoded source exceeds its text/kind bounds');
  end;
  LStorage := 1;
  LOffset := AView.ByteOffset(0);

  if LOffset <> 0 then
  begin
    raise ENyxBytes.Create('An alternate decoded view discarded leading source bytes');
  end;
  for LAt := 0 to LCount - 1 do
  begin
    Check(LCancellation);

    if not NyxNextScalar(LView.SourceText, LStorage, LScalar) then
    begin
      raise ENyxBytes.Create('An alternate decoded view has inconsistent Unicode/scalar data');
    end;
    LNext := AView.ByteOffset(LAt + 1);

    if (LNext <= LOffset) or (LNext > LView.Source.Count) then
    begin
      raise ENyxBytes.Create('An alternate decoded view has an invalid original partition');
    end;
    LView.AddWidth(LNext - LOffset, LOffset);
    LOffset := LNext;
  end;

  if (LStorage <= Length(LView.SourceText)) or (LOffset <> LView.Source.Count) then
  begin
    raise ENyxBytes.Create('An alternate decoded view discarded trailing text or original bytes');
  end;
  LView.Finish;
  Result := LOwner;
end;

end.
