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
unit nyx.pascal.syntax.translated;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.types;

type
  { One mapped significant token. VirtualSpan belongs to the analysis projection;
    PhysicalSpan belongs to Source's exact physical file/occurrence. State is its
    pre-token captured compiler context. No interface or mutable array is retained. }
  TNyxPascalSyntaxOrigin = record
  private
    FVirtual: TNyxPascalSpan;
    FPhysical: TNyxPascalSpan;
    FFile: Integer;
    FOccurrence: Integer;
    FToken: Integer;
    FState: Integer;
    FExpansion: Integer;
  public
    property VirtualSpan: TNyxPascalSpan read FVirtual;
    property PhysicalSpan: TNyxPascalSpan read FPhysical;
    property FileIndex: Integer read FFile;
    property Occurrence: Integer read FOccurrence;
    property TranslationToken: Integer read FToken;
    property State: Integer read FState;
    { -1 is a linear authored token. Replacement parts retain their complete
      invocation span; use Source's macro trace for definition fragment origins. }
    property Expansion: Integer read FExpansion;
  end;
  { Owns a frozen translation and a declared-syntax projection. Projection text
    separates significant tokens explicitly and is analysis-only: never authored
    source, an admission candidate or an editor replacement. Physical files stay
    exact in Source. Every mapped token retains its actual occurrence/state.
    The first significant unknown participant ends the safe prefix; no branch
    alternative is silently discarded to manufacture a complete declaration tree. }
  INyxPascalTranslatedSyntax = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570D02}']
    function Source: INyxPascalTranslation;
    function Syntax: INyxPascalSyntaxSnapshot;
    function CompleteTranslation: Boolean;
    { Translation token of the first unknown significant participant, -1 absent.
      Separate Source diagnostics still matter even when no participant blocks. }
    function BlockedAt: Integer;
    function OriginCount: Integer;
    function Origin(AIndex: Integer): TNyxPascalSyntaxOrigin;
    { Exact virtual character lookup; synthetic separators and EOF return -1.
      No implicit previous/next affinity invents an authored source position. }
    function OriginAt(AVirtualOffset: Integer): Integer;
    { Exact occurrence + physical scalar lookup, -1 if not represented. Repeated
      includes are intentionally not collapsed into an ambiguous filename match. }
    function VirtualAt(AOccurrence, APhysicalOffset: Integer): Integer;
    { Intersect a logical declaration/type span with one mapped token. Refuses
      an empty intersection; callers page Origin records to retain multiple files. }
    function Part(AIndex: Integer; const ASpan: TNyxPascalSpan): TNyxPascalSyntaxOrigin;
    { Parse a declaration's retained type/result syntax. The returned type tree
      owns its projection lexical data; retain this interface for physical mapping.
      Structured class/member scopes remain with Syntax, never re-inferred here. }
    function TypeOfDeclaration(AIndex: Integer;
      const AOptions: TNyxPascalTypeOptions;
      const ACancellation: INyxCancellationScope = nil): INyxPascalTypeSyntax;
  end;

{ Freeze a declared view of known-active tokens under the existing grammar options.
  Unknown significant participation stops at a visible incomplete prefix. Physical
  and logical coordinates remain distinct. Budgets/cancellation refuse atomically;
  this factory edits/compiles/executes no application or active Studio project. }
function NewNyxPascalTranslatedSyntax(const ASource: INyxPascalTranslation;
  const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalTranslatedSyntax;

{ Continue an owned, unblocked projection along the same captured translation
  lineage with exactly matching options. Existing physical origins stay exact;
  only new significant source tokens extend the lexical projection. The parser
  still reconstructs declarations and no reduced work charge is implied.
  Unrelated, shorter, alternate or uncertain predecessors refuse atomically. }
function AppendNyxPascalTranslatedSyntax(const APrevious: INyxPascalTranslatedSyntax;
  const ASource: INyxPascalTranslation; const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalTranslatedSyntax;

implementation

uses SysUtils, nyx.bytes, nyx.pascal.context, nyx.pascal.lexing.append;

type
  { Forward owner for the private borrowed construction seed below. }
  TTranslatedSyntax = class;
  { In-unit immutable projection authority. The pointer is borrowed only while
    its interface lease is held; no appended result retains the prior owner. }
  IProjectionOwner = interface(IInterface)
    ['{7B440252-DB39-4370-A7EC-D51E2BE12FB9}']
    function Projection: TTranslatedSyntax;
  end;
  { Owns its input translation, projection parser and detached origin vector.
    Cancellation exists during construction only; no document/editor cycle occurs. }
  TTranslatedSyntax = class(TInterfacedObject, INyxPascalTranslatedSyntax, IProjectionOwner)
  private
    FSource: INyxPascalTranslation;
    FSyntax: INyxPascalSyntaxSnapshot;
    FOrigins: array of TNyxPascalSyntaxOrigin;
    FCount: Integer;
    FBlocked: Integer;
    FBytes: Integer;
    FOptions: TNyxPascalSyntaxOptions;
    procedure Build(const AOptions: TNyxPascalSyntaxOptions;
      const ACancellation: INyxCancellationScope; APrevious: TTranslatedSyntax);
  public
    constructor Create(const ASource: INyxPascalTranslation;
      const AOptions: TNyxPascalSyntaxOptions;
      const ACancellation: INyxCancellationScope; APrevious: TTranslatedSyntax = nil);
    function Projection: TTranslatedSyntax;
    function Source: INyxPascalTranslation;
    function Syntax: INyxPascalSyntaxSnapshot;
    function CompleteTranslation: Boolean;
    function BlockedAt: Integer;
    function OriginCount: Integer;
    function Origin(AIndex: Integer): TNyxPascalSyntaxOrigin;
    function OriginAt(AVirtualOffset: Integer): Integer;
    function VirtualAt(AOccurrence, APhysicalOffset: Integer): Integer;
    function Part(AIndex: Integer; const ASpan: TNyxPascalSpan): TNyxPascalSyntaxOrigin;
    function TypeOfDeclaration(AIndex: Integer;
      const AOptions: TNyxPascalTypeOptions;
      const ACancellation: INyxCancellationScope): INyxPascalTypeSyntax;
  end;

constructor TTranslatedSyntax.Create(const ASource: INyxPascalTranslation;
  const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope; APrevious: TTranslatedSyntax);
begin
  inherited Create;

  if ASource = nil then
  begin
    raise ENyxPascalLexical.Create('Translated syntax needs its immutable source translation');
  end;
  AOptions.Validate;
  FOptions := AOptions;
  FSource := ASource;
  FBlocked := -1;
  Build(AOptions, ACancellation, APrevious);
end;

procedure TTranslatedSyntax.Build(const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope; APrevious: TTranslatedSyntax);
var
  LParts: TNyxStrings;
  LAt: Integer;
  LOffset: Integer;
  LBytes: Integer;
  LTokenBytes: Integer;
  LText: TNyxText;
  LToken: TNyxPascalTranslatedToken;
  LPhysical: TNyxPascalToken;
  LEffective: TNyxPascalToken;
  LOrigin: TNyxPascalSyntaxOrigin;
  LCapacity: Integer;
  LFirst: Integer;
  LLexical: INyxPascalLexicalSnapshot;
begin
  LParts := TNyxStrings.Create;
  try
    LOffset := 0;
    LBytes := 0;
    LFirst := 0;

    if APrevious <> nil then
    begin
      FCount := APrevious.FCount;
      SetLength(FOrigins, FCount);
      for LAt := 0 to FCount - 1 do
      begin

        if (ACancellation <> nil) and ACancellation.Cancelled then
        begin
          raise ENyxPascalAnalysisCancelled.Create('Translated origin continuation cancelled');
        end;
        FOrigins[LAt] := APrevious.FOrigins[LAt];
      end;
      LFirst := APrevious.FSource.Count;
      LOffset := APrevious.FSyntax.Lexical.Scalars;
      LBytes := APrevious.FBytes;
    end;
    for LAt := LFirst to FSource.Count - 1 do
    begin

      if (ACancellation <> nil) and ACancellation.Cancelled then
      begin
        raise ENyxPascalAnalysisCancelled.Create('Translated Pascal syntax cancelled');
      end;
      LToken := FSource.Token(LAt);
      LPhysical := FSource.PhysicalToken(LAt);
      LEffective := NyxPascalEffectiveToken(FSource, LAt);

      if LEffective.Kind in [ptkWhitespace, ptkComment, ptkDirective, ptkEOF] then
      begin
        Continue;
      end;

      if LToken.Activity = ppkFalse then
      begin
        Continue;
      end;

      if LToken.Activity = ppkUnknown then
      begin
        FBlocked := LAt;
        Break;
      end;
      LText := NyxPascalEffectiveText(FSource, LAt);
      LTokenBytes := NyxUTF8ByteCount(LText);

      if FCount > 0 then
      begin
        Inc(LTokenBytes);
      end;

      if LTokenBytes > AOptions.LexicalOptions.MaximumSourceBytes - LBytes then
      begin
        raise ENyxPascalLexicalLimit.Create('Pascal analysis projection exceeds its byte budget');
      end;
      Inc(LBytes, LTokenBytes);

      if FCount > 0 then
      begin
        LParts.Add(' ');
        Inc(LOffset);
      end;
      LOrigin := Default(TNyxPascalSyntaxOrigin);
      LOrigin.FVirtual := NyxPascalSpan(LOffset, LEffective.Span.Length);
      LOrigin.FPhysical := LPhysical.Span;
      LOrigin.FFile := FSource.Occurrence(LToken.Occurrence).FileIndex;
      LOrigin.FOccurrence := LToken.Occurrence;
      LOrigin.FToken := LAt;
      LOrigin.FState := LToken.State;
      LOrigin.FExpansion := -1;

      if LToken.Expanded then
      begin
        LOrigin.FExpansion := LToken.Expansion;
      end;

      if FCount = Length(FOrigins) then
      begin
        LCapacity := FSource.Count;

        if 128 + FCount div 2 <= FSource.Count - FCount then
        begin
          LCapacity := FCount + 128 + FCount div 2;
        end;
        SetLength(FOrigins, LCapacity);
      end;
      FOrigins[FCount] := LOrigin;
      Inc(FCount);
      Inc(LOffset, LEffective.Span.Length);
      LParts.Add(LText);
    end;
    FBytes := LBytes;

    if APrevious <> nil then
    begin
      LLexical := AppendNyxPascalLexical(APrevious.FSyntax.Lexical, LParts.Join, ACancellation);
    end
    else
    begin
      LLexical := NewNyxPascalAppendLexical(LParts.Join, AOptions.LexicalOptions, ACancellation);
    end;
    FSyntax := NewNyxPascalSyntaxFromLexical(LLexical, AOptions, ACancellation);
  finally
    LParts.Free;
  end;
end;

function TTranslatedSyntax.Projection: TTranslatedSyntax;
begin
  Result := Self;
end;

function TTranslatedSyntax.Source: INyxPascalTranslation;
begin
  Result := FSource;
end;

function TTranslatedSyntax.Syntax: INyxPascalSyntaxSnapshot;
begin
  Result := FSyntax;
end;

function TTranslatedSyntax.CompleteTranslation: Boolean;
begin
  Result := (FBlocked < 0) and FSource.Resolved;
end;

function TTranslatedSyntax.BlockedAt: Integer;
begin
  Result := FBlocked;
end;

function TTranslatedSyntax.OriginCount: Integer;
begin
  Result := FCount;
end;

function TTranslatedSyntax.Origin(AIndex: Integer): TNyxPascalSyntaxOrigin;
begin

  if (AIndex < 0) or (AIndex >= FCount) then
  begin
    raise ENyxPascalLexical.Create('Syntax origin index is outside its immutable projection');
  end;
  Result := FOrigins[AIndex];
end;

function TTranslatedSyntax.OriginAt(AVirtualOffset: Integer): Integer;
var
  LLow: Integer;
  LHigh: Integer;
  LMid: Integer;
begin

  if (AVirtualOffset < 0) or (AVirtualOffset > FSyntax.Lexical.Scalars) then
  begin
    raise ENyxPascalLexical.Create('Logical source offset is outside its analysis projection');
  end;
  Result := -1;
  LLow := 0;
  LHigh := FCount - 1;
  while LLow <= LHigh do
  begin
    LMid := LLow + (LHigh - LLow) div 2;

    if AVirtualOffset < FOrigins[LMid].VirtualSpan.Start then
    begin
      LHigh := LMid - 1;
    end
    else if AVirtualOffset >= FOrigins[LMid].VirtualSpan.Finish then
    begin
      LLow := LMid + 1;
    end
    else
    begin
      Exit(LMid);
    end;
  end;
end;

function TTranslatedSyntax.VirtualAt(AOccurrence, APhysicalOffset: Integer): Integer;
var
  LAt: Integer;
begin

  if (APhysicalOffset < 0) or
    (APhysicalOffset > NyxPascalOccurrenceLexical(FSource, AOccurrence).Scalars) then
  begin
    raise ENyxPascalLexical.Create('Physical source offset is outside its exact include occurrence');
  end;
  for LAt := 0 to FCount - 1 do
  begin

    if (FOrigins[LAt].Occurrence = AOccurrence) and
      (APhysicalOffset >= FOrigins[LAt].PhysicalSpan.Start) and
      (APhysicalOffset < FOrigins[LAt].PhysicalSpan.Finish) then
    begin

      if FOrigins[LAt].Expansion >= 0 then
      begin
        { One invocation can produce many unrelated lengths/tokens. A scalar
          caret cannot choose a unique expanded character without new intent. }
        Exit(-1);
      end;
      Exit(FOrigins[LAt].VirtualSpan.Start + APhysicalOffset - FOrigins[LAt].PhysicalSpan.Start);
    end;
  end;
  Result := -1;
end;

function TTranslatedSyntax.Part(AIndex: Integer; const ASpan: TNyxPascalSpan): TNyxPascalSyntaxOrigin;
var
  LStart: Integer;
  LFinish: Integer;
begin

  if ASpan.Finish > FSyntax.Lexical.Scalars then
  begin
    raise ENyxPascalLexical.Create('A mapped range is outside its analysis projection');
  end;
  Result := Origin(AIndex);
  LStart := ASpan.Start;
  LFinish := ASpan.Finish;

  if LStart < Result.VirtualSpan.Start then
  begin
    LStart := Result.VirtualSpan.Start;
  end;

  if LFinish > Result.VirtualSpan.Finish then
  begin
    LFinish := Result.VirtualSpan.Finish;
  end;

  if LFinish <= LStart then
  begin
    raise ENyxPascalLexical.Create('A logical range does not intersect this physical token');
  end;

  if Result.Expansion < 0 then
  begin
    Result.FPhysical := NyxPascalSpan(Result.PhysicalSpan.Start + LStart - Result.VirtualSpan.Start,
      LFinish - LStart);
  end;
  Result.FVirtual := NyxPascalSpan(LStart, LFinish - LStart);
end;

function TTranslatedSyntax.TypeOfDeclaration(AIndex: Integer;
  const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope): INyxPascalTypeSyntax;
begin
  Result := NewNyxPascalDeclaredTypeSyntax(FSyntax, FSyntax.Declaration(AIndex).TypeSpan,
    AOptions, ACancellation);
end;

function NewNyxPascalTranslatedSyntax(const ASource: INyxPascalTranslation;
  const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope): INyxPascalTranslatedSyntax;
begin
  Result := TTranslatedSyntax.Create(ASource, AOptions, ACancellation);
end;

function SameOptions(const ALeft, ARight: TNyxPascalSyntaxOptions): Boolean;
var
  LLeft: TNyxPascalLexicalOptions;
  LRight: TNyxPascalLexicalOptions;
begin
  LLeft := ALeft.LexicalOptions;
  LRight := ARight.LexicalOptions;
  Result := (ALeft.StartingContext = ARight.StartingContext) and
    (ALeft.MaximumDeclarations = ARight.MaximumDeclarations) and
    (ALeft.MaximumScopes = ARight.MaximumScopes) and
    (ALeft.MaximumDepth = ARight.MaximumDepth) and
    (ALeft.MaximumDiagnostics = ARight.MaximumDiagnostics) and
    (LLeft.MaximumSourceBytes = LRight.MaximumSourceBytes) and
    (LLeft.MaximumTokens = LRight.MaximumTokens) and
    (LLeft.MaximumDiagnostics = LRight.MaximumDiagnostics) and
    (LLeft.MaximumCommentDepth = LRight.MaximumCommentDepth) and
    (LLeft.AllowNestedComments = LRight.AllowNestedComments) and
    (LLeft.AllowUnicodeIdentifiers = LRight.AllowUnicodeIdentifiers);
end;

function AppendNyxPascalTranslatedSyntax(const APrevious: INyxPascalTranslatedSyntax;
  const ASource: INyxPascalTranslation; const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope): INyxPascalTranslatedSyntax;
var
  LPrevious: IProjectionOwner;
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Translated continuation cancelled');
  end;
  AOptions.Validate;

  if not Supports(APrevious, IProjectionOwner, LPrevious) or (ASource = nil) then
  begin
    raise ENyxPascalLexical.Create('Translated continuation needs its owned immutable predecessor and source');
  end;

  if not APrevious.CompleteTranslation or
    (NyxPascalSharedTranslationTokens(APrevious.Source, ASource) <> APrevious.Source.Count) or
    not SameOptions(LPrevious.Projection.FOptions, AOptions) then
  begin
    raise ENyxPascalLexical.Create('Translated continuation needs exact lineage, order, activity and options');
  end;
  Result := TTranslatedSyntax.Create(ASource, AOptions, ACancellation, LPrevious.Projection);
end;

end.
