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
program nyx_pascal_source_bytes_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.editing,
  nyx.pascal.encoding, nyx.pascal.lexing, nyx.pascal.files, nyx.pascal.context,
  nyx.pascal.translation, nyx.pascal.modules, nyx.pascal.sources,
  nyx.pascal.project, nyx.pascal.names, nyx.pascal.drafts,
  nyx.pascal.completion, nyx.pascal.completion.owned, nyx.pascal.binding;

type
  { Mutable alternate byte producer. Publication must detach its vector and
    reject excess counts before consulting any element. }
  TMutableBytes = class(TInterfacedObject, INyxPascalSourceBytes)
  public
    Data: TNyxBytes;
    Excess: Boolean;
    Calls: Integer;
    function Count: Integer;
    function Origin: TNyxPascalByteOrigin;
    function ByteAt(AIndex: Integer): Byte;
    function Slice(const ASpan: TNyxPascalByteSpan): TNyxBytes;
    function SameBytes(const AOther: INyxPascalSourceBytes): Boolean;
  end;
  { A caller-owned codec demonstrates replacement base implementations and
    retirement. The malformed mode deliberately emits a nonprogressing unit. }
  TTransientCodec = class(TInterfacedObject, INyxPascalSourceCodec)
  public
    Broken: Boolean;
    destructor Destroy; override;
    function Reference: TNyxPascalEncodingRef;
    function Kind: TNyxPascalSourceEncodingKind;
    function Read(const ABytes: INyxPascalSourceBytes; AOffset: Integer): TNyxPascalDecodedScalar;
  end;

var
  GChecks: Integer;
  GCodecsRetired: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function TMutableBytes.Count: Integer;
begin
  Result := Length(Data);

  if Excess then
  begin
    Result := High(Integer);
  end;
end;

function TMutableBytes.Origin: TNyxPascalByteOrigin;
begin
  Result := pboCapturedBytes;
end;

function TMutableBytes.ByteAt(AIndex: Integer): Byte;
begin
  Inc(Calls);
  Result := Data[AIndex];
end;

function TMutableBytes.Slice(const ASpan: TNyxPascalByteSpan): TNyxBytes;
begin
  Result := Copy(Data, ASpan.Start, ASpan.Length);
end;

function TMutableBytes.SameBytes(const AOther: INyxPascalSourceBytes): Boolean;
begin
  Result := False;
end;

destructor TTransientCodec.Destroy;
begin
  Inc(GCodecsRetired);
  inherited Destroy;
end;

function TTransientCodec.Reference: TNyxPascalEncodingRef;
begin
  Result := NyxPascalEncoding('explicit-ascii-test');
end;

function TTransientCodec.Kind: TNyxPascalSourceEncodingKind;
begin
  Result := psekCustom;
end;

function TTransientCodec.Read(const ABytes: INyxPascalSourceBytes;
  AOffset: Integer): TNyxPascalDecodedScalar;
begin
  Result.Scalar := ABytes.ByteAt(AOffset);
  Result.Bytes := 1;

  if Broken then
  begin
    Result.Bytes := 0;
  end;
end;

function Packet(const ABytes: array of Byte): INyxPascalSourceBytes;
var
  LData: TNyxBytes;
  LAt: Integer;
begin
  SetLength(LData, Length(ABytes));
  for LAt := 0 to High(ABytes) do
  begin
    LData[LAt] := ABytes[LAt];
  end;
  Result := NewNyxPascalSourceBytes(LData);
end;

function UTF8(const AText: TNyxText): INyxPascalSourceView;
begin
  Result := DecodeNyxPascalSource(NewNyxPascalSourceBytes(NyxEncodeUTF8(AText)), NyxPascalUTF8Codec);
end;

procedure UnicodeAndOrigins;
const
  COffsets: array[0..7] of Integer = (0, 1, 5, 7, 8, 9, 10, 11);
var
  LData: TNyxBytes;
  LCopy: TNyxBytes;
  LBytes: INyxPascalSourceBytes;
  LView: INyxPascalSourceView;
  LLE: INyxPascalSourceView;
  LBE: INyxPascalSourceView;
  LAt: Integer;
begin
  LData := NyxEncodeUTF8(TNyxText('A🚀é') + #13#10#0 + 'Z');
  LBytes := NewNyxPascalSourceBytes(LData);
  LData[1] := 0;
  LView := DecodeNyxPascalSource(LBytes, NyxPascalUTF8Codec);
  Check((LView.Text = TNyxText('A🚀é') + #13#10#0 + 'Z') and (LView.Scalars = 7),
    'Supplementary Unicode, combining/storage widths, NUL and physical newlines remain exact');
  for LAt := 0 to High(COffsets) do
  begin
    Check((LView.ByteOffset(LAt) = COffsets[LAt]) and
      (LView.ScalarBoundaryAt(COffsets[LAt]) = LAt),
      'Independent UTF-8 byte/scalar boundary expectations round trip');
  end;
  Check((LView.ScalarBoundaryAt(2) = -1) and (LView.ScalarBoundaryAt(6) = -1),
    'Offsets inside UTF-8 sequences never round to a different source position');
  LCopy := LView.Original.Slice(LView.OriginalSpan(NyxPascalSpan(1, 1)));
  Check((Length(LCopy) = 4) and (LCopy[0] = $F0) and (LCopy[3] = $80),
    'A supplementary scalar maps to its exact four original bytes');
  LCopy[0] := 0;
  Check(LView.Original.ByteAt(1) = $F0, 'Input and output byte vectors cannot mutate captured originals');
  LLE := DecodeNyxPascalSource(Packet([$FF, $FE, $41, 0, $3D, $D8, $80, $DE, 13, 0, 10, 0]),
    NyxPascalUTF16Codec);
  LBE := DecodeNyxPascalSource(Packet([$FE, $FF, 0, $41, $D8, $3D, $DE, $80, 0, 13, 0, 10]),
    NyxPascalUTF16Codec(True));
  Check((LLE.Text = LBE.Text) and (LLE.Scalars = 5) and
    (LLE.ByteOffset(1) = 2) and (LLE.ByteOffset(2) = 4) and
    (LLE.ByteOffset(3) = 8) and (LLE.ScalarBoundaryAt(6) = -1),
    'Explicit UTF-16 orders retain the BOM, surrogate pair and precise byte boundaries');
  Check((LLE.OriginalSpan(NyxPascalSpan(0, LLE.Scalars)).Length = 12) and
    (LLE.OriginalSpan(NyxPascalSpan(LLE.Scalars, 0)).Start = 12),
    'Whole-source and empty EOF spans cover original UTF-16 bytes exactly');
  LView := UTF8(TNyxText(StringOfChar('x', 260)) + TNyxText('🚀') +
    TNyxText(StringOfChar('y', 260)) + TNyxText('é'));
  Check((LView.ByteOffset(128) = 128) and (LView.ByteOffset(261) = 264) and
    (LView.ByteOffset(512) = 515) and (LView.ByteOffset(522) = 526),
    'Late fixed-to-variable transitions preserve sparse checkpoints across several blocks');
  Check(CaptureNyxPascalSourceView(LView).SameView(LView),
    'Owned immutable source views retain exact identity when captured again');
  LView := DecodeNyxPascalSource(Packet([]), NyxPascalUTF8Codec);
  Check((LView.Text = '') and (LView.Scalars = 0) and (LView.ByteOffset(0) = 0),
    'Empty source has a valid zero-length original partition');
end;

procedure RejectDecode(const ABytes: INyxPascalSourceBytes;
  const ACodec: INyxPascalSourceCodec; AExpected: Integer);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    DecodeNyxPascalSource(ABytes, ACodec);
  except
    on LError: ENyxPascalSourceDecodeError do
    begin
      LRejected := LError.ByteOffset = AExpected;
    end;
  end;
  Check(LRejected, 'Malformed encoded input refuses at its exact original byte offset');
end;

procedure CodecsAndLimits;
var
  LMutable: TMutableBytes;
  LAlternate: INyxPascalSourceBytes;
  LBytes: INyxPascalSourceBytes;
  LView: INyxPascalSourceView;
  LCodec: INyxPascalSourceCodec;
  LTemporary: TTransientCodec;
  LMap: TNyxPascalSingleByteMap;
  LAt: Integer;
  LRejected: Boolean;
  LCancel: INyxCancellationScope;
  LData: TNyxBytes;
begin
  LMutable := TMutableBytes.Create;
  LAlternate := LMutable;
  LMutable.Data := NyxEncodeUTF8('abc');
  LBytes := CaptureNyxPascalSourceBytes(LAlternate);
  LMutable.Data[0] := Ord('z');
  Check(LBytes.ByteAt(0) = Ord('a'), 'Alternate mutable producers are detached before publication');
  LMutable.Excess := True;
  LMutable.Calls := 0;
  LRejected := False;
  try
    CaptureNyxPascalSourceBytes(LAlternate);
  except
    on ENyxPascalSourceDecodeLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LMutable.Calls = 0), 'Excess alternate byte counts refuse before enumeration');
  LTemporary := TTransientCodec.Create;
  LCodec := LTemporary;
  LView := DecodeNyxPascalSource(LBytes, LCodec);
  LCodec := nil;
  Check((GCodecsRetired = 1) and (LView.Text = TNyxText('abc')),
    'An alternate decoder retires while its independent byte/text view stays valid');
  LTemporary := TTransientCodec.Create;
  LCodec := LTemporary;
  LTemporary.Broken := True;
  RejectDecode(LBytes, LCodec, 0);
  for LAt := 0 to 255 do
  begin
    LMap[LAt] := -1;

    if LAt < 128 then
    begin
      LMap[LAt] := LAt;
    end;
  end;
  LMap[$81] := $FC;
  LCodec := NewNyxPascalSingleByteCodec(NyxPascalEncoding('explicit-test-map'), LMap);
  LMap[$81] := Ord('X');
  LView := DecodeNyxPascalSource(Packet([$41, $81, 0]), LCodec);
  Check((LView.Text = TNyxText('Aü') + #0) and (LView.ByteOffset(2) = 2) and
    (LView.OriginalSpan(NyxPascalSpan(1, 1)).Length = 1),
    'An explicit independent single-byte map preserves original bytes and decoded glyphs');
  RejectDecode(Packet([$80]), LCodec, 0);
  RejectDecode(Packet([$41, $C0, $80]), NyxPascalUTF8Codec, 1);
  RejectDecode(Packet([$ED, $A0, $80]), NyxPascalUTF8Codec, 0);
  RejectDecode(Packet([$F4, $90, $80, $80]), NyxPascalUTF8Codec, 0);
  RejectDecode(Packet([$F0, $9F]), NyxPascalUTF8Codec, 0);
  RejectDecode(Packet([$00, $DC]), NyxPascalUTF16Codec, 0);
  RejectDecode(Packet([$00, $D8, $41, 0]), NyxPascalUTF16Codec, 0);
  RejectDecode(Packet([$41]), NyxPascalUTF16Codec, 0);
  LView := nil;
  try
    LView := DecodeNyxPascalSource(Packet([$81]), NyxPascalUTF8Codec);
  except
    on ENyxPascalSourceDecodeError do
    begin
      { The assignment target must not receive the builder's incomplete object. }
    end;
  end;
  Check(LView = nil, 'Caught native decode failures cannot expose a partial source interface');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    DecodeNyxPascalSource(LBytes, NyxPascalUTF8Codec, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Cancellation refuses decode independently of malformed-input policy');
  LMap[$81] := $10FFFF;
  LCodec := NewNyxPascalSingleByteCodec(NyxPascalEncoding('explicit-expansion-limit'), LMap);
  SetLength(LData, 1024 * 1024 + 1);
  for LAt := 0 to High(LData) do
  begin
    LData[LAt] := $81;
  end;
  LBytes := NewNyxPascalSourceBytes(LData);
  LRejected := False;
  LView := nil;
  try
    LView := DecodeNyxPascalSource(LBytes, LCodec);
  except
    on ENyxPascalSourceDecodeLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LView = nil) and (LBytes.Count = Length(LData)),
    'Decoded expansion limits retain original bytes and publish no partial source');
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
end;

procedure CatalogAndTranslation;
var
  LFiles: INyxPascalFileCatalog;
  LBefore: INyxPascalFileCatalog;
  LByteFiles: INyxPascalByteFileCatalog;
  LView: INyxPascalSourceView;
  LSource: INyxPascalTranslation;
  LRejected: Boolean;
  LData: TNyxBytes;
  LText: TNyxText;
  LAt: Integer;
begin
  LBefore := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('main.pas'),
    'unit Main; interface {$I opaque.inc} implementation end.');
  Supports(LBefore, INyxPascalByteFileCatalog, LByteFiles);
  LFiles := LByteFiles.WithByteSource(NyxPascalPhysicalFile('opaque.inc'), Packet([$81]));
  LBefore := CaptureNyxPascalFiles(LFiles);
  Check(LBefore.SameFiles(LFiles) and
    (NyxPascalSourceBytesAt(LBefore, 1).ByteAt(0) = $81) and
    (NyxPascalSourceViewAt(LBefore, 1) = nil),
    'Catalog capture retains opaque bytes and exact independent identity');
  LRejected := False;
  try
    LFiles.TextAt(1);
  except
    on LError: ENyxPascalSourceEncoding do
    begin
      LRejected := LError.FileReference.Name = TNyxText('opaque.inc');
    end;
  end;
  Check(LRejected, 'Unreadable text refuses with its exact virtual source reference');
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), Facts,
    TNyxPascalTranslationOptions.Defaults);
  Check(not LSource.Resolved and (LSource.DiagnosticCount = 1) and
    (LSource.Diagnostic(0).Issue = ptiSourceEncoding) and (LSource.OccurrenceCount = 1),
    'A reached opaque include stops semantic certainty at the authored include');
  LText := 'unit Encoded; interface type TValue=record end; implementation end.';
  SetLength(LData, Length(LText) * 2);
  for LAt := 1 to Length(LText) do
  begin
    LData[(LAt - 1) * 2] := Ord(LText[LAt]);
    LData[(LAt - 1) * 2 + 1] := 0;
  end;
  LView := DecodeNyxPascalSource(NewNyxPascalSourceBytes(LData), NyxPascalUTF16Codec);
  Supports(LFiles, INyxPascalByteFileCatalog, LByteFiles);
  LFiles := LByteFiles.WithByteSource(NyxPascalPhysicalFile('encoded.pas'), LView.Original, LView);
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('encoded.pas'), Facts,
    TNyxPascalTranslationOptions.Defaults);
  Check(LSource.Resolved and (NyxPascalOriginalSpan(LSource, 0,
    NyxPascalSpan(5, 7)).Start = 10) and
    (NyxPascalOriginalSpan(LSource, 0, NyxPascalSpan(5, 7)).Length = 14),
    'Translated physical scalar locations map to exact declared UTF-16 bytes');
  Check((LBefore.Count = 2) and (LFiles.Count = 3) and not LBefore.SameFiles(LFiles),
    'Adding a readable source never mutates an earlier opaque catalog');
  Supports(LFiles, INyxPascalByteFileCatalog, LByteFiles);
  LRejected := False;
  try
    LByteFiles.WithByteSource(NyxPascalPhysicalFile('wrong.pas'), Packet([$41]), LView);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A decoded view cannot be attached to different original bytes');
  LText := '{$codepage utf8} unit Conflicting; interface implementation end.';
  SetLength(LData, Length(LText) * 2);
  for LAt := 1 to Length(LText) do
  begin
    LData[(LAt - 1) * 2] := Ord(LText[LAt]);
    LData[(LAt - 1) * 2 + 1] := 0;
  end;
  LView := DecodeNyxPascalSource(NewNyxPascalSourceBytes(LData), NyxPascalUTF16Codec);
  LFiles := LByteFiles.WithByteSource(NyxPascalPhysicalFile('conflicting.pas'), LView.Original, LView);
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('conflicting.pas'), Facts,
    TNyxPascalTranslationOptions.Defaults);
  Check(not LSource.Resolved and (LSource.DiagnosticCount = 1) and
    (LSource.Diagnostic(0).Issue = ptiSourceEncoding),
    'An explicit source directive cannot silently contradict the supplied decoded view');
end;

procedure CompletionOrigins;
var
  LFiles: INyxPascalFileCatalog;
  LBytes: INyxPascalByteFileCatalog;
  LView: INyxPascalSourceView;
  LModules: INyxPascalModuleCatalog;
  LSources: INyxPascalSourceSnapshot;
  LProvider: INyxPascalCompletionProvider;
  LReply: INyxPascalCompletionResult;
  LOriginal: INyxPascalCompletionByteOrigins;
  LMetadata: INyxOwnedPascalCompletionResult;
  LPrefix: TNyxText;
  LByteStart: Integer;
  LSpan: TNyxPascalByteSpan;
  LRaw: TNyxBytes;
begin
  LView := UTF8('unit ByteFixture; {$mode delphi} interface ' +
    TNyxText('// Dedicated Unicode qualification 🚀') + #10 +
    'type TValue=record end; TData=record AlphaField:TValue; end; implementation end.');
  LByteStart := NyxUTF8ByteCount(Copy(LView.Text, 1, Pos('AlphaField', LView.Text) - 1));
  LFiles := NewNyxPascalFileCatalog(pfcExact);
  Supports(LFiles, INyxPascalByteFileCatalog, LBytes);
  LFiles := LBytes.WithByteSource(NyxPascalPhysicalFile('sdk/bytefixture.pas'), LView.Original, LView);
  LModules := NewNyxPascalModuleCatalog.WithModule(NewNyxPascalModuleSource(
    NyxPascalModule('ByteFixture'), NyxPascalPhysicalFile('sdk/bytefixture.pas')));
  LPrefix := 'unit Consumer; {$mode delphi} interface uses ByteFixture; implementation ' +
    'procedure Run; var Data:TData; begin Data.';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),
    'unit Consumer; interface implementation end.'))
    .WithBuffer(TNyxPascalSourceRef.Main, LPrefix + 'Alphx; end; end.');
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LFiles, LModules,
    TNyxOwnedPascalCompletionOptions.Defaults);
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(LPrefix) + 4));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1),
    'Declared receiver completion consumes an explicitly decoded external source');
  Supports(LReply, INyxPascalCompletionByteOrigins, LOriginal);
  LProvider := nil;
  LFiles := nil;
  LView := nil;
  LSpan := LOriginal.DefinitionByteSpan(0);
  LRaw := LOriginal.DefinitionView(0).Original.Slice(LSpan);
  Check((LSpan.Start = LByteStart) and (NyxDecodeUTF8(LRaw) = TNyxText('AlphaField')) and
    (LOriginal.DefinitionView(0).Original.Origin = pboCapturedBytes),
    'Detached definition byte locations retain original ownership after provider retirement');
  LFiles := NewNyxPascalFileCatalog(pfcExact);
  Supports(LFiles, INyxPascalByteFileCatalog, LBytes);
  LFiles := LBytes.WithByteSource(NyxPascalPhysicalFile('sdk/bytefixture.pas'), Packet([$81]));
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LFiles, LModules,
    TNyxOwnedPascalCompletionOptions.Defaults);
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(LPrefix) + 4));
  Supports(LReply, INyxOwnedPascalCompletionResult, LMetadata);
  Check((LReply.Status = pcsUnavailable) and (LMetadata.BindingIssue = pbiCompilerContext),
    'An opaque indexed root reports an encoding prerequisite instead of an internal failure');
end;

begin
  try
    UnicodeAndOrigins;
    CodecsAndLimits;
    CatalogAndTranslation;
    CompletionOrigins;
    WriteLn('PASS ', GChecks, ' original source byte checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
