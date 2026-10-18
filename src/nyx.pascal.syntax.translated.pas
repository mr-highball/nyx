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

implementation

uses SysUtils, nyx.bytes, nyx.pascal.context;

type
  { Owns its input translation, projection parser and detached origin vector.
    Cancellation exists during construction only; no document/editor cycle occurs. }
  TTranslatedSyntax = class(TInterfacedObject, INyxPascalTranslatedSyntax)
  private
    FSource: INyxPascalTranslation;
    FSyntax: INyxPascalSyntaxSnapshot;
    FOrigins: array of TNyxPascalSyntaxOrigin;
    FCount: Integer;
    FBlocked: Integer;
    procedure Build(const AOptions: TNyxPascalSyntaxOptions;
      const ACancellation: INyxCancellationScope);
  public
    constructor Create(const ASource: INyxPascalTranslation;
      const AOptions: TNyxPascalSyntaxOptions;
      const ACancellation: INyxCancellationScope);
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
  const ACancellation: INyxCancellationScope);
begin
  inherited Create;

  if ASource = nil then
  begin
    raise ENyxPascalLexical.Create('Translated syntax needs its immutable source translation');
  end;
  AOptions.Validate;
  FSource := ASource;
  FBlocked := -1;
  Build(AOptions, ACancellation);
end;

procedure TTranslatedSyntax.Build(const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope);
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
begin
  LParts := TNyxStrings.Create;
  try
    LOffset := 0;
    LBytes := 0;
    for LAt := 0 to FSource.Count - 1 do
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
    FSyntax := NewNyxPascalSyntaxSnapshot(LParts.Join, AOptions, ACancellation);
  finally
    LParts.Free;
  end;
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
  Result := NewNyxPascalTypeSyntax(FSyntax.Lexical, FSyntax.Declaration(AIndex).TypeSpan,
    AOptions, ACancellation);
end;

function NewNyxPascalTranslatedSyntax(const ASource: INyxPascalTranslation;
  const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope): INyxPascalTranslatedSyntax;
begin
  Result := TTranslatedSyntax.Create(ASource, AOptions, ACancellation);
end;

end.
