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
    FDiagnosticCount: Integer;
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
    procedure AddDiagnostic(const AValue: TNyxPascalLexicalDiagnostic);
    function Scan(const AText: TNyxText; const APolicy: TNyxPascalLexicalOptions;
      ARegion: TNyxPascalLexicalRegion;
      AScalarBase, AStorageBase, AByteBase, ALineBase, AColumnBase: Integer;
      const ACancellation: INyxCancellationScope): TNyxPascalLexicalScanReceipt;
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
  { Synchronous final-row destination. Its raw owner is borrowed only while
    Scan holds a local interface lease; finally revokes it on every exit. The
    scanner supplies local exact byte/storage origins, avoiding any spelling
    recount or intermediate token vector. It owns no reader/execution scope. }
  TAppendReceiver = class(TInterfacedObject, INyxPascalLexicalFragmentReceiver)
  public
    Owner: TAppendLexical;
    ScalarBase, StorageBase, ByteBase, LineBase, ColumnBase: Integer;
    procedure ReceiveToken(const AToken: TNyxPascalToken; const ASpelling: TNyxText;
      AByteStart, AStorageStart: Integer);
    procedure ReceiveDiagnostic(const ADiagnostic: TNyxPascalLexicalDiagnostic);
  end;

procedure CheckCancellation(const ACancellation: INyxCancellationScope);
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Append lexical analysis cancelled');
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

procedure TAppendLexical.AddDiagnostic(const AValue: TNyxPascalLexicalDiagnostic);
var
  LCapacity: Integer;
begin

  if FDiagnosticCount >= FOptions.MaximumDiagnostics then
  begin
    raise ENyxPascalLexicalLimit.Create('Append diagnostics exceed the global lexical budget');
  end;

  if FDiagnosticCount = Length(FDiagnostics) then
  begin
    LCapacity := FOptions.MaximumDiagnostics;

    if 16 <= FOptions.MaximumDiagnostics - FDiagnosticCount then
    begin
      LCapacity := FDiagnosticCount + 16;
    end;
    SetLength(FDiagnostics, LCapacity);
  end;
  FDiagnostics[FDiagnosticCount] := AValue;
  Inc(FDiagnosticCount);
end;

procedure TAppendReceiver.ReceiveToken(const AToken: TNyxPascalToken;
  const ASpelling: TNyxText; AByteStart, AStorageStart: Integer);
var
  LRow: TAppendRow;
begin

  if Owner = nil then
  begin
    raise ENyxPascalLexical.Create('Append scan receiver has retired');
  end;
  LRow.Value := AToken.Rebased(ScalarBase, StorageBase, LineBase, ColumnBase);
  LRow.Spelling := ASpelling;
  LRow.ByteStart := ByteBase + AByteStart;
  LRow.StorageStart := StorageBase + AStorageStart;
  Owner.Add(LRow);
end;

procedure TAppendReceiver.ReceiveDiagnostic(const ADiagnostic: TNyxPascalLexicalDiagnostic);
begin

  if Owner = nil then
  begin
    raise ENyxPascalLexical.Create('Append scan receiver has retired');
  end;
  Owner.AddDiagnostic(ADiagnostic.Rebased(ScalarBase));
end;

function TAppendLexical.Scan(const AText: TNyxText;
  const APolicy: TNyxPascalLexicalOptions; ARegion: TNyxPascalLexicalRegion;
  AScalarBase, AStorageBase, AByteBase, ALineBase, AColumnBase: Integer;
  const ACancellation: INyxCancellationScope): TNyxPascalLexicalScanReceipt;
var
  LReceiver: TAppendReceiver;
  LLease: INyxPascalLexicalFragmentReceiver;
begin
  LReceiver := TAppendReceiver.Create;
  LLease := LReceiver;
  LReceiver.Owner := Self;
  LReceiver.ScalarBase := AScalarBase;
  LReceiver.StorageBase := AStorageBase;
  LReceiver.ByteBase := AByteBase;
  LReceiver.LineBase := ALineBase;
  LReceiver.ColumnBase := AColumnBase;
  try
    Result := ScanNyxPascalLexicalFragment(AText, APolicy, ARegion,
      AScalarBase = 0, LLease, ACancellation);
  finally
    LReceiver.Owner := nil;
    FWriting := nil;
  end;
end;

constructor TAppendLexical.Create(const AText: TNyxText;
  const AOptions: TNyxPascalLexicalOptions; const ACancellation: INyxCancellationScope);
var
  LScan: TNyxPascalLexicalScanReceipt;
begin
  inherited Create;
  CheckCancellation(ACancellation);
  AOptions.Validate;
  FOptions := AOptions;
  FIdentity := TInterfacedObject.Create;
  LScan := Scan(AText, AOptions, plrPascal, 0, 0, 0, 0, 0, ACancellation);
  FScalars := LScan.Scalars;
  FBytes := LScan.UTF8Bytes;
  FStorage := LScan.StorageUnits;
  FLexed := LScan.Tokens;
  SetLength(FDiagnostics, FDiagnosticCount);
end;

constructor TAppendLexical.CreateAppend(const APrevious: INyxPascalLexicalSnapshot;
  const ABase: IAppendArena; const ATail: TNyxText;
  const ACancellation: INyxCancellationScope);
var
  LAt: Integer;
  LDiagnostics: Integer;
  LPrefix: TAppendRow;
  LParts: TNyxStrings;
  LScan: TNyxPascalLexicalScanReceipt;
  LPolicy: TNyxPascalLexicalOptions;
  LDiagnostic: TNyxPascalLexicalDiagnostic;
begin
  inherited Create;
  CheckCancellation(ACancellation);
  FOptions := APrevious.Options;
  FIdentity := TInterfacedObject.Create;
  FPrevious := ABase.Identity;
  { Storage length is a cheap UTF-8 lower bound. The one fragment scan validates
    Unicode/counts exact bytes under the remaining global allowance; no separate
    tail/spelling walk duplicates that work. Earlier bytes are already captured. }

  if Length(ATail) > FOptions.MaximumSourceBytes - ABase.Bytes then
  begin
    raise ENyxPascalLexicalLimit.Create('Append source exceeds the global byte budget');
  end;
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

  SetLength(FDiagnostics, LDiagnostics);
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
  FDiagnosticCount := LDiagnostics;
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
    LScan := Scan(LParts.Join, LPolicy, LPrefix.Value.Region,
      LPrefix.Value.Span.Start, LPrefix.StorageStart, LPrefix.ByteStart,
      LPrefix.Value.Line - 1, LPrefix.Value.Column - 1, ACancellation);
  finally
    LParts.Free;
  end;

  if LPrefix.StorageStart + LScan.StorageUnits <> FStorage then
  begin
    raise ENyxPascalLexical.Create('Append source lost its byte/storage accounting');
  end;
  FBytes := LPrefix.ByteStart + LScan.UTF8Bytes;
  FScalars := LPrefix.Value.Span.Start + LScan.Scalars;
  FLexed := LScan.Tokens;
  SetLength(FDiagnostics, FDiagnosticCount);
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
