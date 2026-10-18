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
program nyx_pascal_append_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler,
  nyx.pascal.lexing, nyx.pascal.lexing.append, nyx.pascal.syntax,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.syntax.translated, nyx.pascal.binding;

type
  { Fixture-only source truth. Each callback compares an appended projection
    with independent complete projection/parsing while an enclosing type/routine
    remains unfinished. Neither projection may retain this callback owner. }
  TProjectionCollector = class(TInterfacedObject, INyxPascalConditionalFacts,
    INyxPascalPrefixConditionalFacts)
  public
    Previous: INyxPascalTranslatedSyntax;
    First: INyxPascalTranslatedSyntax;
    PreviousWorld: INyxPascalSemanticWorld;
    Calls: Integer;
    AbortAfter: Integer;
    function PreparePrefix(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): Boolean;
    function EvaluatePrefix(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
      const APrefix: INyxPascalTranslation): TNyxPascalKnowledge;
    function Evaluate(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): TNyxPascalKnowledge;
  end;
  { Cancels during borrowed construction work. Only counters are owned; no
    lexical arena or caller survives through this cooperative scope. }
  TCountingCancellation = class(TInterfacedObject, INyxCancellationScope)
  private
    FCancelled: Boolean;
    FPolls: Integer;
  public
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

procedure TCountingCancellation.Cancel;
begin
  FCancelled := True;
end;

function TCountingCancellation.GetCancelled: Boolean;
begin
  Inc(FPolls);
  Result := FCancelled or (FPolls >= 8);
end;

function SameToken(const ALeft, ARight: TNyxPascalToken): Boolean;
begin
  Result := (ALeft.Span.Start = ARight.Span.Start) and
    (ALeft.Span.Length = ARight.Span.Length) and (ALeft.Kind = ARight.Kind) and
    (ALeft.Word = ARight.Word) and (ALeft.Symbol = ARight.Symbol) and
    (ALeft.NumberBase = ARight.NumberBase) and (ALeft.Line = ARight.Line) and
    (ALeft.Column = ARight.Column) and (ALeft.Complete = ARight.Complete) and
    (ALeft.Escaped = ARight.Escaped) and (ALeft.RealNumber = ARight.RealNumber) and
    (ALeft.Region = ARight.Region);
end;

procedure EqualLexical(const ALeft, ARight: INyxPascalLexicalSnapshot);
var
  LAt: Integer;
  LEqual: Boolean;
begin
  LEqual := (ALeft.Source = ARight.Source) and (ALeft.Scalars = ARight.Scalars) and
    (ALeft.Count = ARight.Count) and (ALeft.DiagnosticCount = ARight.DiagnosticCount);

  if LEqual then
  begin
    for LAt := 0 to ALeft.Count - 1 do
    begin
      LEqual := LEqual and SameToken(ALeft.Token(LAt), ARight.Token(LAt)) and
        (ALeft.Text(LAt) = ARight.Text(LAt)) and
        (ALeft.TokenAt(ALeft.Token(LAt).Span.Start) = ARight.TokenAt(ARight.Token(LAt).Span.Start));
    end;
    for LAt := 0 to ALeft.DiagnosticCount - 1 do
    begin
      LEqual := LEqual and (ALeft.Diagnostic(LAt).Issue = ARight.Diagnostic(LAt).Issue) and
        (ALeft.Diagnostic(LAt).Span.Start = ARight.Diagnostic(LAt).Span.Start) and
        (ALeft.Diagnostic(LAt).Span.Length = ARight.Diagnostic(LAt).Span.Length);
    end;
  end;
  Check(LEqual, 'Append differs from an independent whole-source scan: '+String(ARight.Source));
end;

procedure Splits;
const
  CCases: array[0..20] of TNyxText = (
    '', '1.25e+3..42', '1.e3 12.0 12..2', '&type &77 $ff %101 #$1f #65',
    'a:=b <= c <> d >> 2 << 3 (.x.)',
    '''text''''quote'' ''two strings''',
    '{ outer (* nested *) tail } X', '(* open { inner } tail *) Y',
    '// line comment' + #13#10 + 'Value',
    '''unfinished' + #13#10 + 'X',
    'asm mov ax, $42; db "hello" end;',
    'asm db "open\"tail"; mov %eax, 1 end;',
    'asm { note } mov ax, 0',
    'unit LongName; interface const Value = 42; implementation end.',
    'a' + #13#10 + #13 + 'b' + #10 + 'c',
    '𝒜name café { 🚀 ' + #0 + ' } ''𐐷''',
    'a﻿b',
    '{$ifdef A}type X=Byte;{$else}type X=Word;{$endif}',
    '$ % & 1e+ #$ #', '{ unfinished many' + #13#10 + 'lines',
    'asm db ''unfinished' + #13#10 + 'end;');
var
  LCase: Integer;
  LAt: Integer;
  LNext: Integer;
  LScalar: Integer;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LOptions: TNyxPascalLexicalOptions;
  LBase: INyxPascalLexicalSnapshot;
  LAppended: INyxPascalLexicalSnapshot;
  LWhole: INyxPascalLexicalSnapshot;
  LChain: INyxPascalLexicalSnapshot;
begin
  LOptions := TNyxPascalLexicalOptions.Defaults.NestedComments(True).UnicodeIdentifiers(True);
  for LCase := 0 to High(CCases) do
  begin
    LWhole := NewNyxPascalLexicalSnapshot(CCases[LCase], LOptions);
    LAt := 1;
    LChain := NewNyxPascalAppendLexical('', LOptions);
    repeat
      LBefore := Copy(CCases[LCase], 1, LAt - 1);
      LAfter := Copy(CCases[LCase], LAt, Length(CCases[LCase]));
      LBase := NewNyxPascalAppendLexical(LBefore, LOptions);
      LAppended := AppendNyxPascalLexical(LBase, LAfter);
      EqualLexical(LAppended, LWhole);
      Check(LBase.Source = LBefore, 'An appended result cannot mutate its exact predecessor text');

      if LAt > Length(CCases[LCase]) then
      begin
        Break;
      end;
      LNext := LAt;
      NyxNextScalar(CCases[LCase], LNext, LScalar);
      LChain := AppendNyxPascalLexical(LChain, Copy(CCases[LCase], LAt, LNext - LAt));
      EqualLexical(LChain, NewNyxPascalLexicalSnapshot(Copy(CCases[LCase], 1, LNext - 1), LOptions));
      LAt := LNext;
    until False;
    EqualLexical(LChain, LWhole);
  end;
end;

procedure OwnershipAndSlices;
var
  LText: TNyxText;
  LBase: INyxPascalLexicalSnapshot;
  LNext: INyxPascalLexicalSnapshot;
  LFork: INyxPascalLexicalSnapshot;
  LWork: INyxPascalLexicalAppendWork;
  LWhole: INyxPascalLexicalSnapshot;
  LSyntax: INyxPascalSyntaxSnapshot;
  LOrdinary: INyxPascalSyntaxSnapshot;
  LAt: Integer;
  LStart: Integer;
  LLength: Integer;
begin
  LText := 'unit AppendCase; interface const ';
  for LAt := 1 to 150 do
  begin
    LText := LText + 'V' + TNyxText(IntToStr(LAt)) + '=' + TNyxText(IntToStr(LAt)) + '; ';
  end;
  LBase := NewNyxPascalAppendLexical(LText, TNyxPascalLexicalOptions.Defaults);
  LNext := AppendNyxPascalLexical(LBase, 'Caption = ''🚀''; implementation end.');
  LFork := AppendNyxPascalLexical(LBase, 'Other = 42; implementation end.');
  LWhole := NewNyxPascalLexicalSnapshot(LText + TNyxText('Caption = ''🚀''; implementation end.'));
  EqualLexical(LNext, LWhole);
  Check(Supports(LNext, INyxPascalLexicalAppendWork, LWork) and
    (LWork.RetainedFrom(LBase) > 0) and (LWork.RetainedFrom(LFork) = -1) and
    (LWork.RetainedFrom(LNext) = LNext.Count) and (LWork.SharedTokens > 0) and
    (LWork.LexedTokens < LNext.Count div 10),
    'An exact direct predecessor grants lexical retention while a sibling grants none');
  LSyntax := NewNyxPascalSyntaxFromLexical(LNext, TNyxPascalSyntaxOptions.Defaults);
  LOrdinary := NewNyxPascalSyntaxSnapshot(LWhole.Source);
  Check((LSyntax.Lexical = LNext) and (LSyntax.DeclarationCount = LOrdinary.DeclarationCount) and
    (LSyntax.DiagnosticCount = LOrdinary.DiagnosticCount),
    'Ordinary declaration parsing consumes the owned lexical arena without assembling it again');
  for LAt := 0 to LNext.Count - 1 do
  begin
    LStart := LNext.Token(LAt).Span.Start;
    LLength := LNext.Token(LAt).Span.Length;
    Check(LNext.Slice(NyxPascalSpan(LStart, LLength)) = LWhole.Slice(NyxPascalSpan(LStart, LLength)),
      'Slices preserve complete tokens, target storage and supplementary Unicode');
  end;
  Check(LNext.Slice(NyxPascalSpan(1, LNext.Scalars - 2)) =
    LWhole.Slice(NyxPascalSpan(1, LWhole.Scalars - 2)), 'A scalar slice crosses shared and private blocks exactly');
  LBase := nil;
  LNext := nil;
  LFork := nil;
  Check(LSyntax.Lexical.Source = LWhole.Source, 'Retired predecessor snapshots leave the constructed syntax independently owned');
end;

procedure Refusals;
var
  LBase: INyxPascalLexicalSnapshot;
  LCandidate: INyxPascalLexicalSnapshot;
  LCancellation: INyxCancellationScope;
  LRefused: Boolean;
  LTail: TNyxText;
  LMode: Integer;
begin
  LBase := NewNyxPascalAppendLexical('abc', TNyxPascalLexicalOptions.Defaults.UpToSourceBytes(3));
  LRefused := False;
  try
    LCandidate := AppendNyxPascalLexical(LBase, 'd');
  except
    on ENyxPascalLexicalLimit do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LCandidate = nil) and (LBase.Source = TNyxText('abc')),
    'The unchanged whole-source byte cap refuses before publication and preserves its predecessor');
  LCancellation := NewNyxCancellationScope;
  LCancellation.Cancel;
  LRefused := False;
  try
    LCandidate := AppendNyxPascalLexical(LBase, '', LCancellation);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LCandidate = nil), 'Even an empty append respects prior cancellation');
  LRefused := False;
  try
    LCandidate := AppendNyxPascalLexical(NewNyxPascalLexicalSnapshot('abc'), 'd');
  except
    on ENyxPascalLexical do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LCandidate = nil), 'Unproven alternate/contextual lexical state cannot donate append authority');
  for LMode := 0 to 2 do
  begin
    case LMode of
      0:
        begin
          LBase := NewNyxPascalAppendLexical('a ', TNyxPascalLexicalOptions.Defaults.UpToTokens(3));
          LTail := 'b';
        end;
      1:
        begin
          LBase := NewNyxPascalAppendLexical('#', TNyxPascalLexicalOptions.Defaults.UpToDiagnostics(1));
          LTail := ' ' + #0;
        end;
      2:
        begin
          LBase := NewNyxPascalAppendLexical('🚀', TNyxPascalLexicalOptions.Defaults.UpToSourceBytes(4));
          Check(NyxUTF8ByteCount(LBase.Source) = 4, 'Supplementary Unicode consumes its actual UTF-8 byte quota');
          LTail := 'x';
        end;
    end;
    LRefused := False;
    try
      LCandidate := AppendNyxPascalLexical(LBase, LTail);
    except
      on ENyxPascalLexicalLimit do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LCandidate = nil), 'Global token, diagnostic and UTF-8 byte limits survive append reuse');
  end;
  LBase := NewNyxPascalAppendLexical('word', TNyxPascalLexicalOptions.Defaults);
  { Keep an explicit interface lease across deliberate exceptions; a native
    constructor-to-interface temporary is not the receiver's lifetime owner. }
  LCancellation := TCountingCancellation.Create;
  LRefused := False;
  try
    LCandidate := AppendNyxPascalLexical(LBase, TNyxText(StringOfChar('x', 9000)), LCancellation);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LCandidate = nil) and (LBase.Source = TNyxText('word')),
    'Cancellation during append work leaves the predecessor exact and publishes no candidate');
  {$ifdef PAS2JS}
  LTail := #$D800;
  {$else}
  SetLength(LTail, 1);
  LTail[1] := AnsiChar($FF);
  {$endif}
  LRefused := False;
  try
    LCandidate := AppendNyxPascalLexical(LBase, LTail);
  except
    on ENyxPascalLexical do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LCandidate = nil) and (LBase.Source = TNyxText('word')),
    'Malformed target Unicode cannot enter the appended source or alter its predecessor');
end;

procedure EqualSyntax(const ALeft, ARight: INyxPascalSyntaxSnapshot);
var
  LAt: Integer;
  LEqual: Boolean;
  LLeft: TNyxPascalSyntaxDeclaration;
  LRight: TNyxPascalSyntaxDeclaration;
begin
  LEqual := (ALeft.ScopeCount = ARight.ScopeCount) and
    (ALeft.DeclarationCount = ARight.DeclarationCount) and
    (ALeft.ImportCount = ARight.ImportCount) and
    (ALeft.DiagnosticCount = ARight.DiagnosticCount);

  if LEqual then
  begin
    for LAt := 0 to ALeft.ScopeCount - 1 do
    begin
      LEqual := LEqual and (ALeft.Scope(LAt).Name = ARight.Scope(LAt).Name) and
        (ALeft.Scope(LAt).Kind = ARight.Scope(LAt).Kind) and
        (ALeft.Scope(LAt).Parent = ARight.Scope(LAt).Parent) and
        (ALeft.Scope(LAt).OwnerDeclaration = ARight.Scope(LAt).OwnerDeclaration) and
        (ALeft.Scope(LAt).Span.Start = ARight.Scope(LAt).Span.Start) and
        (ALeft.Scope(LAt).Span.Length = ARight.Scope(LAt).Span.Length) and
        (ALeft.Scope(LAt).Complete = ARight.Scope(LAt).Complete) and
        (ALeft.Scope(LAt).UnsupportedSyntax = ARight.Scope(LAt).UnsupportedSyntax);
    end;
    for LAt := 0 to ALeft.DeclarationCount - 1 do
    begin
      LLeft := ALeft.Declaration(LAt);
      LRight := ARight.Declaration(LAt);
      LEqual := LEqual and (LLeft.Name = LRight.Name) and (LLeft.OwnerName = LRight.OwnerName) and
        (LLeft.Kind = LRight.Kind) and (LLeft.OperatorKind = LRight.OperatorKind) and
        (LLeft.Scope = LRight.Scope) and (LLeft.NestedScope = LRight.NestedScope) and
        (LLeft.TypeForm = LRight.TypeForm) and (LLeft.Flags = LRight.Flags) and
        (LLeft.Next = LRight.Next) and (LLeft.NextNamed = LRight.NextNamed) and
        (LLeft.NameSpan.Start = LRight.NameSpan.Start) and (LLeft.NameSpan.Length = LRight.NameSpan.Length) and
        (LLeft.HeaderSpan.Start = LRight.HeaderSpan.Start) and (LLeft.HeaderSpan.Length = LRight.HeaderSpan.Length) and
        (LLeft.TypeSpan.Start = LRight.TypeSpan.Start) and (LLeft.TypeSpan.Length = LRight.TypeSpan.Length) and
        (LLeft.ExpressionSpan.Start = LRight.ExpressionSpan.Start) and
        (LLeft.ExpressionSpan.Length = LRight.ExpressionSpan.Length);
    end;
  end;
  Check(LEqual, 'Append projection preserves independent whole parsing, unfinished owners and declaration ranges');
end;

function TProjectionCollector.PreparePrefix(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): Boolean;
begin
  Result := True;
end;

function TProjectionCollector.EvaluatePrefix(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
  const APrefix: INyxPascalTranslation): TNyxPascalKnowledge;
var
  LWhole: INyxPascalTranslatedSyntax;
  LNext: INyxPascalTranslatedSyntax;
  LAt: Integer;
  LLeft: TNyxPascalSyntaxOrigin;
  LRight: TNyxPascalSyntaxOrigin;
  LEqual: Boolean;
  LWork: INyxPascalLexicalAppendWork;
  LStorage: INyxPascalProjectionStorage;
  LWorld: INyxPascalSemanticWorld;
  LFreshWorld: INyxPascalSemanticWorld;
  LWorldWork: INyxPascalWorldBuildWork;
  LFreshWork: INyxPascalWorldBuildWork;
  LAnswer: INyxPascalResolution;
begin
  Inc(Calls);
  LWhole := NewNyxPascalTranslatedSyntax(APrefix, TNyxPascalSyntaxOptions.Defaults);

  if Previous = nil then
  begin
    LNext := LWhole;
    First := LNext;
  end
  else
  begin
    LNext := AppendNyxPascalTranslatedSyntax(Previous, APrefix, TNyxPascalSyntaxOptions.Defaults);
    Check(Supports(LNext.Syntax.Lexical, INyxPascalLexicalAppendWork, LWork) and
      (LWork.RetainedFrom(Previous.Syntax.Lexical) > 0),
      'Chronological translated queries actually consume append lexical authority');
    Check(Supports(LNext, INyxPascalProjectionStorage, LStorage) and
      (LStorage.SharedOriginRows >= 256) and (LStorage.CopiedOriginRows < 256) and
      (LStorage.SharedOriginRows + LStorage.CopiedOriginRows = Previous.OriginCount) and
      (LStorage.OriginDirectoryEntries = (LNext.OriginCount + 255) div 256),
      'Exact physical mapping shares complete blocks and copies only a bounded independent partial tail');
  end;
  EqualLexical(LNext.Syntax.Lexical,
    NewNyxPascalLexicalSnapshot(LWhole.Syntax.Lexical.Source));
  EqualSyntax(LNext.Syntax, NewNyxPascalSyntaxSnapshot(LWhole.Syntax.Lexical.Source));
  LEqual := (LNext.OriginCount = LWhole.OriginCount) and
    (LNext.CompleteTranslation = LWhole.CompleteTranslation) and (LNext.BlockedAt = LWhole.BlockedAt);
  for LAt := 0 to LNext.OriginCount - 1 do
  begin
    LLeft := LNext.Origin(LAt);
    LRight := LWhole.Origin(LAt);
    LEqual := LEqual and (LLeft.VirtualSpan.Start = LRight.VirtualSpan.Start) and
      (LLeft.VirtualSpan.Length = LRight.VirtualSpan.Length) and
      (LLeft.PhysicalSpan.Start = LRight.PhysicalSpan.Start) and
      (LLeft.PhysicalSpan.Length = LRight.PhysicalSpan.Length) and
      (LLeft.FileIndex = LRight.FileIndex) and (LLeft.Occurrence = LRight.Occurrence) and
      (LLeft.TranslationToken = LRight.TranslationToken) and (LLeft.State = LRight.State) and
      (LLeft.Expansion = LRight.Expansion) and
      (LNext.OriginAt(LLeft.VirtualSpan.Start) = LWhole.OriginAt(LRight.VirtualSpan.Start));
  end;
  Check(LEqual, 'Appended origins retain exact occurrence, pre-state, macro trace and virtual/physical mapping');
  LFreshWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LWhole);

  if PreviousWorld = nil then
  begin
    LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LNext);
  end
  else
  begin
    LWorld := PreviousWorld.WithModule(LNext);
    Check(Supports(LWorld, INyxPascalWorldBuildWork, LWorldWork) and
      Supports(LFreshWorld, INyxPascalWorldBuildWork, LFreshWork) and
      (LWorldWork.ReusedBoundaryTokens > 0) and
      (LWorldWork.BoundaryTokenReads < LFreshWork.BoundaryTokenReads),
      'Binding module preparation actually skips its proven unchanged lexical prefix');
  end;
  Check((LWorld.Count = LFreshWorld.Count) and
    (LWorld.InterfaceBoundary(0) = LFreshWorld.InterfaceBoundary(0)),
    'Continued module boundaries match independent whole preparation at each actual source callback');
  LAnswer := LWorld.Lookup(LWorld.At(0, 0, LNext.Syntax.Lexical.Scalars),
    NyxPascalBindingName('TBox'));
  Check((LAnswer.Status = pbsBound) and (LAnswer.Count = 1) and
    (LAnswer.World = LWorld) and (LAnswer.SymbolAt(0).World = LWorld),
    'Resolutions retain the exact current world rather than a cached earlier binding result');
  PreviousWorld := LWorld;
  Previous := LNext;

  if (AbortAfter > 0) and (Calls >= AbortAfter) then
  begin
    raise EAbort.Create('Deliberate appended projection retirement');
  end;
  Result := ppkTrue;
end;

function TProjectionCollector.Evaluate(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): TNyxPascalKnowledge;
begin
  Result := ppkUnknown;
  raise Exception.Create('Projection fixture requires its typed prefix boundary');
end;

procedure Projections;
const
  CPart: TNyxText = '{$if declared(Seed)}{$endif} const Part=VALUE; {$if declared(Part)}{$endif}';
var
  LText: TNyxText;
  LAt: Integer;
  LFiles: INyxPascalFileCatalog;
  LFacts: INyxPascalCompilerContext;
  LCollector: TProjectionCollector;
  LOwner: INyxPascalConditionalFacts;
  LSource: INyxPascalTranslation;
  LForeign: INyxPascalTranslation;
  LFirst: INyxPascalTranslatedSyntax;
  LLast: INyxPascalTranslatedSyntax;
  LFinal: INyxPascalTranslatedSyntax;
  LRejected: INyxPascalTranslatedSyntax;
  LRefused: Boolean;
  LCase: Integer;
  LWorld: INyxPascalSemanticWorld;
  LWorldWork: INyxPascalWorldBuildWork;
  LCancellation: INyxCancellationScope;
begin
  LText := 'unit ProjectionCase; {$macro on}{$define VALUE:=42}{$push}{$H-} interface type TBox=class const ';
  for LAt := 1 to 100 do
  begin
    LText := LText + 'V' + TNyxText(IntToStr(LAt)) + '=VALUE; ';
  end;
  LText := LText + 'Seed=VALUE; {$if declared(TBox)}{$endif}{$include part.inc}' +
    'public procedure Run(AValue:Integer); end; {$define VALUE:=84} ' +
    'type TMirror=record {$include part.inc} end; {$pop}' +
    '{$if declared(TMirror)}{$endif} implementation procedure TBox.Run(AValue:Integer); ' +
    'var LData:Integer; begin {$if declared(AValue)}{$endif} LData:=AValue; end; end.';
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('root.pas'), LText)
    .WithFile(NyxPascalPhysicalFile('part.inc'), CPart);
  LFacts := NewNyxPascalCompilerContext(ppcFPC, TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithFeature(ppfNestedComments, ppkFalse).WithOption('H', ppkTrue);
  LCollector := TProjectionCollector.Create;
  LOwner := LCollector;
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('root.pas'),
    LFacts, TNyxPascalTranslationOptions.Defaults, nil, LOwner);
  Check(LSource.Resolved and (LCollector.Calls = 7) and (LSource.OccurrenceCount = 3),
    'Seven actual source callbacks include unfinished class/record/routine owners and repeated includes');
  LFirst := LCollector.First;
  LLast := LCollector.Previous;
  LFinal := AppendNyxPascalTranslatedSyntax(LLast, LSource, TNyxPascalSyntaxOptions.Defaults);
  LWorld := LCollector.PreviousWorld.WithModule(LFinal);
  Check(Supports(LWorld, INyxPascalWorldBuildWork, LWorldWork) and
    (LWorldWork.BoundaryTokenReads = 0) and (LWorldWork.ReusedBoundaryTokens > 0),
    'An unchanged closed interface boundary requires no lexical rescan after its module body grows');
  EqualSyntax(LFinal.Syntax, NewNyxPascalSyntaxSnapshot(LFinal.Syntax.Lexical.Source));
  LOwner := nil;
  Check(LFinal.Syntax.Scope(0).Complete and not LFirst.Syntax.Scope(0).Complete and
    (LFirst.Source.Files.TextAt(0) = LText),
    'Final and earlier projections survive construction authority retirement independently');
  LCollector := TProjectionCollector.Create;
  LOwner := LCollector;
  LForeign := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('root.pas'),
    LFacts, TNyxPascalTranslationOptions.Defaults, nil, LOwner);
  Check(NyxPascalSharedTranslationTokens(LSource, LForeign) = -1,
    'Identical source and compiler facts from another construction do not invent shared lineage');
  LWorld := LWorld.WithModule(NewNyxPascalTranslatedSyntax(LForeign,
    TNyxPascalSyntaxOptions.Defaults));
  Check(Supports(LWorld, INyxPascalWorldBuildWork, LWorldWork) and
    (LWorldWork.ReusedBoundaryTokens = 0) and (LWorldWork.BoundaryTokenReads > 0),
    'An identical foreign module receives a fresh boundary scan rather than guessed append authority');
  for LCase := 0 to 2 do
  begin
    LRefused := False;
    try
      case LCase of
        0:
          begin
            LRejected := AppendNyxPascalTranslatedSyntax(LLast, LForeign, TNyxPascalSyntaxOptions.Defaults);
          end;
        1:
          begin
            LRejected := AppendNyxPascalTranslatedSyntax(LLast, LFirst.Source, TNyxPascalSyntaxOptions.Defaults);
          end;
        2:
          begin
            LRejected := AppendNyxPascalTranslatedSyntax(LLast, LSource,
              TNyxPascalSyntaxOptions.Defaults.UpToDiagnostics(5));
          end;
      end;
    except
      on ENyxPascalLexical do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRejected = nil), 'Foreign lineage, backwards source and changed options refuse without mutation');
  end;
  LCancellation := TCountingCancellation.Create;
  LRefused := False;
  try
    LRejected := AppendNyxPascalTranslatedSyntax(LLast, LSource,
      TNyxPascalSyntaxOptions.Defaults, LCancellation);
  except
    on LError: ENyxPascalAnalysisCancelled do
    begin
      LRefused := Pos('Translated ', LError.Message) = 1;
    end;
  end;
  Check(LRefused and (LRejected = nil) and (LLast.Source.Files.TextAt(0) = LText),
    'Cancellation during origin block construction publishes no candidate and preserves exact source');
  LOwner := nil;
  LCollector := TProjectionCollector.Create;
  LOwner := LCollector;
  LCollector.AbortAfter := 3;
  LForeign := nil;
  LRefused := False;
  try
    LForeign := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('root.pas'),
      LFacts, TNyxPascalTranslationOptions.Defaults, nil, LOwner);
  except
    on EAbort do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LForeign = nil) and (LCollector.Calls = 3) and
    (LCollector.Previous.Source.Files.TextAt(0) = LText),
    'Callback failure after real projection reuse retires construction while detached results remain valid');
end;

begin
  try
    Splits;
    OwnershipAndSlices;
    Refusals;
    Projections;
    WriteLn('PASS ', GChecks, ' append lexical/projection checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
