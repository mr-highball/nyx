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
unit nyx.pascal.lexing.append;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.lexing;

type
  { Actual append work over immutable lexical values. RetainedFrom returns the
    unchanged leading token count for the exact direct predecessor (or self),
    -1 for another lineage. It grants lexical reuse only, not syntax, compiler
    context or source admission. LexedTokens includes the rescanned tail's EOF. }
  INyxPascalLexicalAppendWork = interface(IInterface)
    ['{DCA5316C-BE5A-44FC-8B17-559D4E7F2A4B}']
    function RetainedFrom(const APrevious: INyxPascalLexicalSnapshot): Integer;
    function LexedTokens: Integer;
    function SharedTokens: Integer;
    function CopiedTokens: Integer;
  end;

{ Start a fixed-policy append arena with ordinary complete lexical semantics.
  Segmented immutable storage supports later appends without joining old text.
  Source assembles the whole text only when explicitly requested. }
function NewNyxPascalAppendLexical(const AText: TNyxText;
  const AOptions: TNyxPascalLexicalOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalLexicalSnapshot;

{ Append valid Unicode text to an arena from this factory. Re-scan the last two
  non-EOF tokens and appended text under their exact entry region. This covers
  scanner lookahead, unfinished comments/strings, numeric dot seams and CRLF.
  Earlier blocks remain immutable; provisional tail diagnostics are replaced.
  Budgets are global and unchanged. Cancellation/refusal never changes APrevious.
  Contextual physical lexers and arbitrary alternate snapshots require an
  explicit fresh start instead of donating unknown scanner state. }
function AppendNyxPascalLexical(const APrevious: INyxPascalLexicalSnapshot;
  const ATail: TNyxText;
  const ACancellation: INyxCancellationScope = nil): INyxPascalLexicalSnapshot;

{ Sealed lexical continuity for parser construction. Returns the unchanged
  leading token count for an owned direct predecessor/self, -1 otherwise.
  An alternate implementation's public work counters grant no authority. }
function NyxPascalRetainedLexicalTokens(const APrevious,
  ACurrent: INyxPascalLexicalSnapshot): Integer;

implementation

uses SysUtils, nyx.pascal.work;

const
  { Bounds partial-tail copying while complete directories share value blocks. }
  CBlockSize = 256;

type
  { Value-only row. Spelling owns its exact target String storage; scalar,
    UTF-8 byte and target-storage origins remain independent. No interface is
    stored inside the record or exposed through a mutable caller-owned array. }
  TAppendRow = record
    Value: TNyxPascalToken;
    Spelling: TNyxText;
    ByteStart: Integer;
    StorageStart: Integer;
  end;
  { A private immutable block of value rows, with no source/arena back link. }
  IAppendBlock = interface(IInterface)
    function Row(AIndex: Integer): TAppendRow;
  end;
  { Filled only by a constructing arena, then shared read-only. Unused tail
    slots never become accessible through the arena's checked public count. }
  TAppendBlock = class(TInterfacedObject, IAppendBlock)
  public
    Rows: array of TAppendRow;
    function Row(AIndex: Integer): TAppendRow;
  end;
  { Sealed in-unit continuation authority. Stamps contain no source or parent
    references; directories retain blocks without chaining prior snapshots. }
  IAppendArena = interface(IInterface)
    ['{BC24369E-C28D-4CF3-B63F-59E333F8951C}']
    function Identity: IInterface;
    function BlockAt(AIndex: Integer): IAppendBlock;
    function Row(AIndex: Integer): TAppendRow;
    function Bytes: Integer;
    function StorageUnits: Integer;
    function RetainedFrom(const APrevious: INyxPascalLexicalSnapshot): Integer;
  end;
  { Owns a flat block directory, exact diagnostics and independent immutable
    identity. Construction alone may fill its private partial block; published
    snapshots retain neither cancellation nor an earlier arena/reader. }
  TAppendLexical = class(TInterfacedObject, INyxPascalLexicalSnapshot,
    INyxPascalLexicalAppendWork, IAppendArena)
  private
    FOptions: TNyxPascalLexicalOptions;
    FBlocks: array of IAppendBlock;
    FDiagnostics: array of TNyxPascalLexicalDiagnostic;
    FCount: Integer;
    FScalars: Integer;
    FBytes: Integer;
    FStorage: Integer;
    FShared: Integer;
    FLexed: Integer;
    FRetained: Integer;
    FIdentity: IInterface;
    FPrevious: IInterface;
    FWriting: TAppendBlock;
    procedure Add(const ARow: TAppendRow);
    procedure Import(const ASource: INyxPascalLexicalSnapshot;
      AScalarBase, AStorageBase, AByteBase, ALineBase, AColumnBase: Integer;
      const ACancellation: INyxCancellationScope);
  public
    constructor Create(const AText: TNyxText; const AOptions: TNyxPascalLexicalOptions;
      const ACancellation: INyxCancellationScope);
    constructor CreateAppend(const APrevious: INyxPascalLexicalSnapshot;
      const ABase: IAppendArena; const ATail: TNyxText;
      const ACancellation: INyxCancellationScope);
    function Source: TNyxText;
    function Options: TNyxPascalLexicalOptions;
    function Scalars: Integer;
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    function Identifier(AIndex: Integer): TNyxText;
    function TokenAt(AOffset: Integer): Integer;
    function Slice(const ASpan: TNyxPascalSpan): TNyxText;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalLexicalDiagnostic;
    function Identity: IInterface;
    function BlockAt(AIndex: Integer): IAppendBlock;
    function Row(AIndex: Integer): TAppendRow;
    function Bytes: Integer;
    function StorageUnits: Integer;
    function RetainedFrom(const APrevious: INyxPascalLexicalSnapshot): Integer;
    function LexedTokens: Integer;
    function SharedTokens: Integer;
    function CopiedTokens: Integer;
  end;

procedure CheckCancellation(const ACancellation: INyxCancellationScope);
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Append lexical analysis cancelled');
  end;
end;

{ Count exact boundary bytes with bounded cancellation polls. Resource codecs'
  exception family and uncancellable whole-text counting do not cross this
  lexical factory's failure/lifetime contract. }
function CheckedBytes(const AText: TNyxText; AMaximum: Integer;
  const ACancellation: INyxCancellationScope): Integer;
var
  LAt: Integer;
  LScalar: Integer;
  LBytes: Integer;
  LPolls: Integer;
begin
  CheckCancellation(ACancellation);

  if Length(AText) > AMaximum then
  begin
    raise ENyxPascalLexicalLimit.Create('Append source exceeds its UTF-8 byte budget');
  end;
  Result := 0;
  LAt := 1;
  LPolls := 0;
  while LAt <= Length(AText) do
  begin

    if LPolls mod 256 = 0 then
    begin
      CheckCancellation(ACancellation);
    end;
    Inc(LPolls);

    if not NyxNextScalar(AText, LAt, LScalar) then
    begin
      raise ENyxPascalLexical.Create('Appended source contains malformed Unicode');
    end;
    LBytes := 1;

    if LScalar > $FFFF then
    begin
      LBytes := 4;
    end
    else if LScalar > $7FF then
    begin
      LBytes := 3;
    end
    else if LScalar > $7F then
    begin
      LBytes := 2;
    end;

    if LBytes > AMaximum - Result then
    begin
      raise ENyxPascalLexicalLimit.Create('Append source exceeds its UTF-8 byte budget');
    end;
    Inc(Result, LBytes);
  end;
end;

function TAppendBlock.Row(AIndex: Integer): TAppendRow;
begin

  if (AIndex < 0) or (AIndex >= Length(Rows)) then
  begin
    raise ENyxPascalLexical.Create('Append row is outside its immutable block');
  end;
  Result := Rows[AIndex];
end;

procedure TAppendLexical.Add(const ARow: TAppendRow);
var
  LAt: Integer;
  LBlock: TAppendBlock;
  LLease: IAppendBlock;
begin

  if FCount >= FOptions.MaximumTokens then
  begin
    raise ENyxPascalLexicalLimit.Create('Append tokens exceed the global lexical budget');
  end;

  if FCount mod CBlockSize = 0 then
  begin
    LBlock := TAppendBlock.Create;
    LLease := LBlock;
    LAt := Length(FBlocks);
    SetLength(FBlocks, LAt + 1);
    FBlocks[LAt] := LLease;
    FWriting := LBlock;
    SetLength(FWriting.Rows, CBlockSize);
  end;
  FWriting.Rows[FCount mod CBlockSize] := ARow;
  Inc(FCount);
end;

procedure TAppendLexical.Import(const ASource: INyxPascalLexicalSnapshot;
  AScalarBase, AStorageBase, AByteBase, ALineBase, AColumnBase: Integer;
  const ACancellation: INyxCancellationScope);
var
  LAt: Integer;
  LRow: TAppendRow;
  LBytes: Integer;
  LStorage: Integer;
begin
  LBytes := AByteBase;
  LStorage := AStorageBase;
  for LAt := 0 to ASource.Count - 1 do
  begin
    CheckCancellation(ACancellation);
    NyxPascalAdmitAnalysis(ACancellation, papLexicalRow);
    LRow.Value := ASource.Token(LAt).Rebased(AScalarBase, AStorageBase, ALineBase, AColumnBase);
    LRow.Spelling := ASource.Text(LAt);
    LRow.ByteStart := LBytes;
    LRow.StorageStart := LStorage;
    Inc(LBytes, CheckedBytes(LRow.Spelling, FBytes - LBytes, ACancellation));
    Inc(LStorage, Length(LRow.Spelling));
    Add(LRow);
  end;

  if (LBytes <> FBytes) or (LStorage <> FStorage) then
  begin
    raise ENyxPascalLexical.Create('Append source lost its byte/storage accounting');
  end;
  FWriting := nil;
end;

constructor TAppendLexical.Create(const AText: TNyxText;
  const AOptions: TNyxPascalLexicalOptions; const ACancellation: INyxCancellationScope);
var
  LSource: INyxPascalLexicalSnapshot;
  LAt: Integer;
begin
  inherited Create;
  CheckCancellation(ACancellation);
  AOptions.Validate;
  FOptions := AOptions;
  FIdentity := TInterfacedObject.Create;
  LSource := NewNyxPascalLexicalSnapshot(AText, AOptions, ACancellation);
  FScalars := LSource.Scalars;
  FBytes := CheckedBytes(AText, FOptions.MaximumSourceBytes, ACancellation);
  FStorage := Length(AText);
  FLexed := LSource.Count;
  Import(LSource, 0, 0, 0, 0, 0, ACancellation);
  SetLength(FDiagnostics, LSource.DiagnosticCount);
  for LAt := 0 to High(FDiagnostics) do
  begin
    CheckCancellation(ACancellation);
    NyxPascalAdmitAnalysis(ACancellation, papDiagnosticRow);
    FDiagnostics[LAt] := LSource.Diagnostic(LAt);
  end;
end;

constructor TAppendLexical.CreateAppend(const APrevious: INyxPascalLexicalSnapshot;
  const ABase: IAppendArena; const ATail: TNyxText;
  const ACancellation: INyxCancellationScope);
var
  LAt: Integer;
  LDiagnostics: Integer;
  LPrefix: TAppendRow;
  LParts: TNyxStrings;
  LSource: INyxPascalLexicalSnapshot;
  LPolicy: TNyxPascalLexicalOptions;
  LDiagnostic: TNyxPascalLexicalDiagnostic;
  LTailBytes: Integer;
begin
  inherited Create;
  CheckCancellation(ACancellation);
  FOptions := APrevious.Options;
  FIdentity := TInterfacedObject.Create;
  FPrevious := ABase.Identity;
  LTailBytes := CheckedBytes(ATail, FOptions.MaximumSourceBytes - ABase.Bytes, ACancellation);

  if LTailBytes > FOptions.MaximumSourceBytes - ABase.Bytes then
  begin
    raise ENyxPascalLexicalLimit.Create('Append source exceeds the global byte budget');
  end;
  FBytes := ABase.Bytes + LTailBytes;
  FStorage := ABase.StorageUnits + Length(ATail);
  FRetained := APrevious.Count - 3;

  if FRetained < 0 then
  begin
    FRetained := 0;
  end;
  LPrefix := ABase.Row(FRetained);
  FShared := FRetained div CBlockSize * CBlockSize;
  SetLength(FBlocks, FShared div CBlockSize);
  for LAt := 0 to High(FBlocks) do
  begin
    CheckCancellation(ACancellation);
    NyxPascalAdmitAnalysis(ACancellation, papLexicalRow);
    FBlocks[LAt] := ABase.BlockAt(LAt);
  end;
  FCount := FShared;
  for LAt := FShared to FRetained - 1 do
  begin
    CheckCancellation(ACancellation);
    NyxPascalAdmitAnalysis(ACancellation, papLexicalRow);
    Add(ABase.Row(LAt));
  end;
  LParts := TNyxStrings.Create;
  try
    for LAt := FRetained to APrevious.Count - 2 do
    begin
      CheckCancellation(ACancellation);
      NyxPascalAdmitAnalysis(ACancellation, papLexicalRow);
      LParts.Add(ABase.Row(LAt).Spelling);
    end;
    LParts.Add(ATail);
    LPolicy := FOptions.UpToTokens(FOptions.MaximumTokens - FRetained)
      .UpToSourceBytes(FOptions.MaximumSourceBytes - LPrefix.ByteStart);
    LSource := NewNyxPascalLexicalFragment(LParts.Join, LPolicy,
      LPrefix.Value.Region, LPrefix.Value.Span.Start = 0, ACancellation);
  finally
    LParts.Free;
  end;
  FScalars := LPrefix.Value.Span.Start + LSource.Scalars;
  FLexed := LSource.Count;
  Import(LSource, LPrefix.Value.Span.Start, LPrefix.StorageStart,
    LPrefix.ByteStart, LPrefix.Value.Line - 1, LPrefix.Value.Column - 1, ACancellation);
  LDiagnostics := 0;
  for LAt := 0 to APrevious.DiagnosticCount - 1 do
  begin
    CheckCancellation(ACancellation);
    NyxPascalAdmitAnalysis(ACancellation, papDiagnosticRow);
    LDiagnostic := APrevious.Diagnostic(LAt);

    if (LDiagnostic.Span.Start < LPrefix.Value.Span.Start) and
      (LDiagnostic.Span.Finish <= LPrefix.Value.Span.Start) then
    begin
      Inc(LDiagnostics);
    end;
  end;

  if LSource.DiagnosticCount > FOptions.MaximumDiagnostics - LDiagnostics then
  begin
    raise ENyxPascalLexicalLimit.Create('Append diagnostics exceed the global lexical budget');
  end;
  SetLength(FDiagnostics, LDiagnostics + LSource.DiagnosticCount);
  LDiagnostics := 0;
  for LAt := 0 to APrevious.DiagnosticCount - 1 do
  begin
    CheckCancellation(ACancellation);
    NyxPascalAdmitAnalysis(ACancellation, papDiagnosticRow);
    LDiagnostic := APrevious.Diagnostic(LAt);

    if (LDiagnostic.Span.Start < LPrefix.Value.Span.Start) and
      (LDiagnostic.Span.Finish <= LPrefix.Value.Span.Start) then
    begin
      FDiagnostics[LDiagnostics] := LDiagnostic;
      Inc(LDiagnostics);
    end;
  end;
  for LAt := 0 to LSource.DiagnosticCount - 1 do
  begin
    CheckCancellation(ACancellation);
    NyxPascalAdmitAnalysis(ACancellation, papDiagnosticRow);
    FDiagnostics[LDiagnostics + LAt] := LSource.Diagnostic(LAt).Rebased(LPrefix.Value.Span.Start);
  end;
end;

function TAppendLexical.Row(AIndex: Integer): TAppendRow;
begin

  if (AIndex < 0) or (AIndex >= FCount) then
  begin
    raise ENyxPascalLexical.Create('Token is outside its immutable append snapshot');
  end;
  Result := FBlocks[AIndex div CBlockSize].Row(AIndex mod CBlockSize);
end;

function TAppendLexical.BlockAt(AIndex: Integer): IAppendBlock;
begin

  if (AIndex < 0) or (AIndex >= Length(FBlocks)) then
  begin
    raise ENyxPascalLexical.Create('Block is outside its immutable append directory');
  end;
  Result := FBlocks[AIndex];
end;

function TAppendLexical.Source: TNyxText;
var
  LParts: TNyxStrings;
  LAt: Integer;
begin
  LParts := TNyxStrings.Create;
  try
    for LAt := 0 to FCount - 2 do
    begin
      LParts.Add(Row(LAt).Spelling);
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function TAppendLexical.Options: TNyxPascalLexicalOptions;
begin
  Result := FOptions;
end;

function TAppendLexical.Scalars: Integer;
begin
  Result := FScalars;
end;

function TAppendLexical.Count: Integer;
begin
  Result := FCount;
end;

function TAppendLexical.Token(AIndex: Integer): TNyxPascalToken;
begin
  Result := Row(AIndex).Value;
end;

function TAppendLexical.Text(AIndex: Integer): TNyxText;
begin
  Result := Row(AIndex).Spelling;
end;

function TAppendLexical.Identifier(AIndex: Integer): TNyxText;
begin

  if Token(AIndex).Kind <> ptkWord then
  begin
    raise ENyxPascalLexical.Create('Append identifier requires a word token');
  end;
  Result := Text(AIndex);

  if Token(AIndex).Escaped then
  begin
    Delete(Result, 1, 1);
  end;
end;

function TAppendLexical.TokenAt(AOffset: Integer): Integer;
var
  LLow: Integer;
  LHigh: Integer;
  LMid: Integer;
begin

  if (AOffset < 0) or (AOffset > FScalars) then
  begin
    raise ENyxPascalLexical.Create('Scalar is outside its immutable append snapshot');
  end;
  LLow := 0;
  LHigh := FCount - 1;
  Result := LHigh;
  while LLow <= LHigh do
  begin
    LMid := LLow + (LHigh - LLow) div 2;

    if Token(LMid).Span.Start <= AOffset then
    begin
      Result := LMid;
      LLow := LMid + 1;
    end
    else
    begin
      LHigh := LMid - 1;
    end;
  end;
end;

function TAppendLexical.Slice(const ASpan: TNyxPascalSpan): TNyxText;
var
  LAt: Integer;
  LStart: Integer;
  LFinish: Integer;
  LScalar: Integer;
  LOffset: Integer;
  LRow: TAppendRow;
  LParts: TNyxStrings;
begin

  if ASpan.Finish > FScalars then
  begin
    raise ENyxPascalLexical.Create('Slice is outside its immutable append snapshot');
  end;
  LParts := TNyxStrings.Create;
  try
    LAt := TokenAt(ASpan.Start);
    while (LAt < FCount) and (Token(LAt).Span.Start < ASpan.Finish) do
    begin
      LRow := Row(LAt);
      LOffset := LRow.Value.Span.Start;
      LStart := 1;
      while LOffset < ASpan.Start do
      begin
        NyxNextScalar(LRow.Spelling, LStart, LScalar);
        Inc(LOffset);
      end;
      LFinish := LStart;
      while (LOffset < ASpan.Finish) and (LOffset < LRow.Value.Span.Finish) do
      begin
        NyxNextScalar(LRow.Spelling, LFinish, LScalar);
        Inc(LOffset);
      end;
      LParts.Add(Copy(LRow.Spelling, LStart, LFinish - LStart));
      Inc(LAt);
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function TAppendLexical.DiagnosticCount: Integer;
begin
  Result := Length(FDiagnostics);
end;

function TAppendLexical.Diagnostic(AIndex: Integer): TNyxPascalLexicalDiagnostic;
begin

  if (AIndex < 0) or (AIndex >= Length(FDiagnostics)) then
  begin
    raise ENyxPascalLexical.Create('Diagnostic is outside its immutable append snapshot');
  end;
  Result := FDiagnostics[AIndex];
end;

function TAppendLexical.Identity: IInterface;
begin
  Result := FIdentity;
end;

function TAppendLexical.Bytes: Integer;
begin
  Result := FBytes;
end;

function TAppendLexical.StorageUnits: Integer;
begin
  Result := FStorage;
end;

function TAppendLexical.RetainedFrom(const APrevious: INyxPascalLexicalSnapshot): Integer;
var
  LPrevious: IAppendArena;
begin
  Result := -1;

  if not Supports(APrevious, IAppendArena, LPrevious) then
  begin
    Exit;
  end;

  if LPrevious.Identity = FIdentity then
  begin
    Result := FCount;
  end
  else if (FPrevious <> nil) and (LPrevious.Identity = FPrevious) then
  begin
    Result := FRetained;
  end;
end;

function TAppendLexical.LexedTokens: Integer;
begin
  Result := FLexed;
end;

function TAppendLexical.SharedTokens: Integer;
begin
  Result := FShared;
end;

function TAppendLexical.CopiedTokens: Integer;
begin
  Result := FCount - FShared;
end;

function NewNyxPascalAppendLexical(const AText: TNyxText;
  const AOptions: TNyxPascalLexicalOptions;
  const ACancellation: INyxCancellationScope): INyxPascalLexicalSnapshot;
begin
  Result := TAppendLexical.Create(AText, AOptions, ACancellation);
end;

function NyxPascalRetainedLexicalTokens(const APrevious,
  ACurrent: INyxPascalLexicalSnapshot): Integer;
var
  LCurrent: IAppendArena;
begin
  Result := -1;

  if Supports(ACurrent, IAppendArena, LCurrent) then
  begin
    Result := LCurrent.RetainedFrom(APrevious);
  end;
end;

function AppendNyxPascalLexical(const APrevious: INyxPascalLexicalSnapshot;
  const ATail: TNyxText; const ACancellation: INyxCancellationScope): INyxPascalLexicalSnapshot;
var
  LBase: IAppendArena;
begin
  CheckCancellation(ACancellation);

  if not Supports(APrevious, IAppendArena, LBase) then
  begin
    raise ENyxPascalLexical.Create('Append requires an owned fixed-policy lexical arena');
  end;

  if ATail = '' then
  begin
    Exit(APrevious);
  end;
  Result := TAppendLexical.CreateAppend(APrevious, LBase, ATail, ACancellation);
end;

end.
