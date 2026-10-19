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
program nyx_pascal_checkpoint_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.lexing.append, nyx.pascal.syntax;

type
  { Cancels at an explicit cooperative-work poll, during filter directory copying
    or retained metadata restoration. An interface lease owns this fixture across
    deliberate exceptions; it retains neither parser nor caller. }
  TRestoreCancellation = class(TInterfacedObject, INyxCancellationScope)
  private
    FPolls: Integer;
    FAfter: Integer;
    FCancelled: Boolean;
  public
    constructor Create(AAfter: Integer = 20);
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

procedure TRestoreCancellation.Cancel;
begin
  FCancelled := True;
end;

constructor TRestoreCancellation.Create(AAfter: Integer);
begin
  inherited Create;
  FAfter := AAfter;
end;

function TRestoreCancellation.GetCancelled: Boolean;
begin
  Inc(FPolls);
  Result := FCancelled or (FPolls >= FAfter);
end;

{ Deterministic full public metadata, including all optional tables and derived
  name/body indexes. Nested writers borrow only this synchronous builder; no
  callback or mutable source survives the comparison. }
function SnapshotText(const ASource: INyxPascalSyntaxSnapshot): TNyxText;
var
  LParts: TNyxStrings;
  LAt: Integer;
  LScope: TNyxPascalSyntaxScope;
  LEntry: TNyxPascalSyntaxDeclaration;
  LFlag: TNyxPascalSyntaxFlag;
  LLink: TNyxPascalLinkageFlag;
  LVariants: INyxPascalVariantSyntax;
  LHints: INyxPascalHintSyntax;
  LLabels: INyxPascalStatementLabelSyntax;
  LIntrinsics: INyxPascalIntrinsicSyntax;
  LStructures: INyxPascalStructuredSyntax;
  LOperators: INyxPascalOperatorSyntax;

  procedure Number(AValue: Integer);
  begin
    LParts.Add(TNyxText(IntToStr(AValue)) + ';');
  end;

  procedure Text(const AText: TNyxText);
  begin
    LParts.Add(TNyxText(IntToStr(Length(AText))) + ':' + AText);
  end;

  procedure Span(const ASpan: TNyxPascalSpan);
  begin
    Number(ASpan.Start);
    Number(ASpan.Length);
  end;

begin
  LParts := TNyxStrings.Create;
  try
    Text(ASource.Lexical.Source);
    Number(ASource.ScopeCount);
    for LAt := 0 to ASource.ScopeCount - 1 do
    begin
      LScope := ASource.Scope(LAt);
      Text(LScope.Name);
      Number(Ord(LScope.Kind));
      Number(LScope.Parent);
      Number(LScope.OwnerDeclaration);
      Number(LScope.FirstDeclaration);
      Number(LScope.DeclarationCount);
      Span(LScope.Span);
      Number(Ord(LScope.Complete));
      Number(Ord(LScope.UnsupportedSyntax));
    end;
    Supports(ASource, INyxPascalStructuredSyntax, LStructures);
    Supports(ASource, INyxPascalOperatorSyntax, LOperators);
    Number(ASource.DeclarationCount);
    for LAt := 0 to ASource.DeclarationCount - 1 do
    begin
      LEntry := ASource.Declaration(LAt);
      Text(LEntry.Name);
      Text(LEntry.OwnerName);
      Number(Ord(LEntry.Kind));
      Number(Ord(LEntry.OperatorKind));
      Number(LEntry.VariantArm);
      Number(LEntry.FirstHint);
      Number(Ord(LEntry.Anonymous));
      Number(Ord(LEntry.TypeForm));
      Number(Ord(LEntry.Visibility));
      Number(Ord(LEntry.ParameterMode));
      for LFlag := Low(TNyxPascalSyntaxFlag) to High(TNyxPascalSyntaxFlag) do
      begin
        Number(Ord(LFlag in LEntry.Flags));
      end;
      for LLink := Low(TNyxPascalLinkageFlag) to High(TNyxPascalLinkageFlag) do
      begin
        Number(Ord(LLink in LEntry.Linkage));
      end;
      Span(LEntry.LinkageSpan);
      Number(LEntry.FirstIntrinsic);
      Span(LEntry.NameSpan);
      Span(LEntry.HeaderSpan);
      Span(LEntry.TypeSpan);
      Span(LEntry.AncestorSpan);
      Span(LEntry.GenericSpan);
      Span(LEntry.ExpressionSpan);
      Number(LEntry.Scope);
      Number(LEntry.NestedScope);
      Number(LEntry.Next);
      Number(LEntry.NextNamed);
      Number(ASource.Named(LEntry.Scope, LEntry.Name));
      Number(LStructures.TypeAt(LEntry.TypeSpan.Start));

      if LEntry.OperatorKind in [Succ(pokUnknown)..High(TNyxPascalOperatorKind)] then
      begin
        Number(LOperators.NamedOperator(LEntry.Scope, LEntry.OperatorKind));
      end;
    end;
    Number(ASource.ImportCount);
    for LAt := 0 to ASource.ImportCount - 1 do
    begin
      Text(ASource.ImportAt(LAt).Name);
      Span(ASource.ImportAt(LAt).Span);
      Number(ASource.ImportAt(LAt).Scope);
      Number(Ord(ASource.ImportAt(LAt).Conditional));
    end;
    Number(ASource.DiagnosticCount);
    for LAt := 0 to ASource.DiagnosticCount - 1 do
    begin
      Number(Ord(ASource.Diagnostic(LAt).Issue));
      Span(ASource.Diagnostic(LAt).Span);
      Text(ASource.Diagnostic(LAt).MessageText);
    end;
    Supports(ASource, INyxPascalVariantSyntax, LVariants);
    Number(LVariants.SelectorCount);
    for LAt := 0 to LVariants.SelectorCount - 1 do
    begin
      Number(LVariants.SelectorAt(LAt).RecordScope);
      Number(LVariants.SelectorAt(LAt).ParentArm);
      Number(LVariants.SelectorAt(LAt).TagDeclaration);
      Span(LVariants.SelectorAt(LAt).TypeSpan);
      Span(LVariants.SelectorAt(LAt).Span);
      Number(LVariants.SelectorAt(LAt).FirstArm);
      Number(Ord(LVariants.SelectorAt(LAt).Complete));
    end;
    Number(LVariants.ArmCount);
    for LAt := 0 to LVariants.ArmCount - 1 do
    begin
      Number(LVariants.ArmAt(LAt).SelectorIndex);
      Span(LVariants.ArmAt(LAt).Span);
      Number(LVariants.ArmAt(LAt).FirstLabel);
      Number(LVariants.ArmAt(LAt).NextArm);
      Number(Ord(LVariants.ArmAt(LAt).Complete));
    end;
    Number(LVariants.LabelCount);
    for LAt := 0 to LVariants.LabelCount - 1 do
    begin
      Number(LVariants.LabelAt(LAt).ArmIndex);
      Span(LVariants.LabelAt(LAt).Span);
      Number(LVariants.LabelAt(LAt).NextLabel);
    end;
    Supports(ASource, INyxPascalHintSyntax, LHints);
    Number(LHints.HintCount);
    for LAt := 0 to LHints.HintCount - 1 do
    begin
      Number(LHints.HintAt(LAt).DeclarationIndex);
      Number(Ord(LHints.HintAt(LAt).Kind));
      Span(LHints.HintAt(LAt).Span);
      Span(LHints.HintAt(LAt).MessageSpan);
      Number(LHints.HintAt(LAt).NextHint);
    end;
    Supports(ASource, INyxPascalStatementLabelSyntax, LLabels);
    Number(LLabels.StatementLabelCount);
    for LAt := 0 to LLabels.StatementLabelCount - 1 do
    begin
      Number(Ord(LLabels.StatementLabelAt(LAt).Kind));
      Number(LLabels.StatementLabelAt(LAt).DeclarationIndex);
      Text(LLabels.StatementLabelAt(LAt).Name);
      Number(LLabels.StatementLabelAt(LAt).Scope);
      Span(LLabels.StatementLabelAt(LAt).NameSpan);
      Span(LLabels.StatementLabelAt(LAt).SectionSpan);
      Number(Ord(LLabels.StatementLabelAt(LAt).Complete));
      Number(Ord(LLabels.StatementLabelAt(LAt).Conditional));
    end;
    Supports(ASource, INyxPascalIntrinsicSyntax, LIntrinsics);
    Number(LIntrinsics.IntrinsicCount);
    for LAt := 0 to LIntrinsics.IntrinsicCount - 1 do
    begin
      Number(LIntrinsics.IntrinsicAt(LAt).DeclarationIndex);
      Number(Ord(LIntrinsics.IntrinsicAt(LAt).Kind));
      Span(LIntrinsics.IntrinsicAt(LAt).Span);
      Span(LIntrinsics.IntrinsicAt(LAt).KeySpan);
      Span(LIntrinsics.IntrinsicAt(LAt).OperandSpan);
      Number(LIntrinsics.IntrinsicAt(LAt).NextIntrinsic);
      Number(Ord(LIntrinsics.IntrinsicAt(LAt).Complete));
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

procedure CompareEveryAppend(const AText: TNyxText; const AOptions: TNyxPascalSyntaxOptions);
var
  LLexical: INyxPascalLexicalSnapshot;
  LNextLexical: INyxPascalLexicalSnapshot;
  LPrevious: INyxPascalSyntaxSnapshot;
  LNext: INyxPascalSyntaxSnapshot;
  LWhole: INyxPascalSyntaxSnapshot;
  LBefore: TNyxText;
  LPrefix: TNyxText;
  LAt: Integer;
  LNextAt: Integer;
  LScalar: Integer;
begin
  LLexical := NewNyxPascalAppendLexical('', AOptions.LexicalOptions);
  LPrevious := NewNyxPascalSyntaxFromLexical(LLexical, AOptions);
  LAt := 1;
  while LAt <= Length(AText) do
  begin
    LBefore := SnapshotText(LPrevious);
    LNextAt := LAt;
    NyxNextScalar(AText, LNextAt, LScalar);
    LNextLexical := AppendNyxPascalLexical(LLexical, Copy(AText, LAt, LNextAt - LAt));
    LNext := AppendNyxPascalSyntax(LPrevious, LNextLexical, AOptions);
    LPrefix := Copy(AText, 1, LNextAt - 1);
    LWhole := NewNyxPascalSyntaxSnapshot(LPrefix, AOptions);
    Check(SnapshotText(LNext) = SnapshotText(LWhole),
      'Checkpoint metadata differs at scalar append: '+String(LPrefix));
    Check(SnapshotText(LPrevious) = LBefore, 'Resumed parsing mutated its prior immutable snapshot');
    LPrevious := LNext;
    LLexical := LNextLexical;
    LAt := LNextAt;
  end;
end;

procedure Differential;
const
  CModules: array[0..7] of TNyxText = (
    'unit Basic; interface const A=1; B=2; type T=record X:Integer; end; implementation end.',
    'unit Classes; interface type T=class private F:Integer; public const A=1; property P:Integer read F; procedure Run; overload; end; implementation procedure T.Run; var L:Integer; begin L:=1; end; end.',
    'unit Variants; interface type T=record case Tag:Byte of 0:(X:Integer); 1:(Y:record Z:Byte; end); end deprecated ''use next''; TInline=array of record F:Integer; end; implementation end.',
    'unit Routines; interface procedure Run(A:Integer); overload; procedure Run(A:Byte); overload; implementation procedure Run(A:Integer); label Start,Stop; var X:Integer; begin X:=A; end; end.',
    'unit Conditional; interface {$ifdef FEATURE}type T=Byte;{$else}type T=Word;{$endif} {$i external.inc} const A=42; implementation end.',
    'unit Hints; interface type T=Integer deprecated ''old''; const A=1 deprecated; var X,Y:Integer platform; procedure Call; cdecl; external ''host'' name ''entry''; implementation end.',
    'unit Unicode; interface type T𝒜=record Value:Integer; end; const Caption=''🚀''; implementation end.',
    'unit Recovery; interface type Broken=record Value:Integer; class Bad; const Ready=1; implementation end.');
var
  LAt: Integer;
  LOptions: TNyxPascalSyntaxOptions;
begin
  LOptions := TNyxPascalSyntaxOptions.Defaults.Lexical(
    TNyxPascalLexicalOptions.Defaults.UnicodeIdentifiers(True).NestedComments(True));
  for LAt := 0 to High(CModules) do
  begin
    CompareEveryAppend(CModules[LAt], LOptions);
  end;
  CompareEveryAppend('unit ThreadStorage; interface threadvar First,Second:Integer; ' +
    'Third:Integer; var Plain:Integer; type T=class public class threadvar Local:Integer; ' +
    'Continued:Integer; class var Shared:Integer; const Offset=2; public Field:Integer; end; implementation end.',
    LOptions);
  CompareEveryAppend('class threadvar First,Second:Integer; Third:Integer; class var Shared:Integer; ' +
    'class threadvar Next:Integer; const Offset=2; public Field:Integer;', LOptions.Fragment(psfMembers));
  CompareEveryAppend('T=record X:Integer; end; U=class procedure Run; end; V=array of T;',
    LOptions.Fragment(psfTypes));
  CompareEveryAppend('private F:Integer; public const A=1; class var X:Integer; procedure Run; overload; property P:Integer read F;',
    LOptions.Fragment(psfMembers));
  CompareEveryAppend('uses Classes; label Start,Stop; const A=42; var X:Integer; procedure Run; begin end;',
    LOptions.Fragment(psfDeclarations));
  CompareEveryAppend('uses SysUtils; type T=record X:Byte; end; procedure Run; cdecl;',
    LOptions.Fragment(psfInterfaceDeclarations));
end;

procedure ActualReuse;
var
  LText: TNyxText;
  LAt: Integer;
  LBaseLexical: INyxPascalLexicalSnapshot;
  LNextLexical: INyxPascalLexicalSnapshot;
  LBase: INyxPascalSyntaxSnapshot;
  LNext: INyxPascalSyntaxSnapshot;
  LWhole: INyxPascalSyntaxSnapshot;
  LNextWork: INyxPascalSyntaxWork;
  LWholeWork: INyxPascalSyntaxWork;
  LFilterWork: INyxPascalSyntaxFilterWork;
  LMetadataWork: INyxPascalSyntaxMetadataWork;
  LWholeMetadataWork: INyxPascalSyntaxMetadataWork;
  LBefore: TNyxText;
  LCancellation: INyxCancellationScope;
  LCancelled: Boolean;
  LRejected: INyxPascalSyntaxSnapshot;
begin
  LText := 'unit ReuseCase; interface type ';
  for LAt := 1 to 80 do
  begin
    LText := LText + 'T' + TNyxText(IntToStr(LAt)) + '=record Value:Integer; end; ';
  end;
  LText := LText + 'const Ready=42; ';
  LBaseLexical := NewNyxPascalAppendLexical(LText, TNyxPascalLexicalOptions.Defaults);
  LBase := NewNyxPascalSyntaxFromLexical(LBaseLexical, TNyxPascalSyntaxOptions.Defaults);
  LBefore := SnapshotText(LBase);
  LNextLexical := AppendNyxPascalLexical(LBaseLexical, 'var Tail:T80; implementation end.');
  LNext := AppendNyxPascalSyntax(LBase, LNextLexical, TNyxPascalSyntaxOptions.Defaults);
  LWhole := NewNyxPascalSyntaxSnapshot(LText + 'var Tail:T80; implementation end.');
  Check(SnapshotText(LNext) = SnapshotText(LWhole), 'Large retained declaration bodies equal a whole parse');
  Check(Supports(LNext, INyxPascalSyntaxWork, LNextWork) and
    Supports(LWhole, INyxPascalSyntaxWork, LWholeWork) and
    (LNextWork.ReusedDeclarations >= 160) and (LNextWork.RestartToken > 0) and
    (LNextWork.ParserTokenReads < LWholeWork.ParserTokenReads div 4),
    'The parser actually skips completed bodies rather than only reporting a compatible fresh result');
  Check((LNextWork.FilteredTokens = LNextLexical.Count) and
    (LNextWork.CopiedEntries > 0) and (LNextWork.CopiedEntries < LNextWork.ReusedDeclarations div 4),
    'The logical lexical domain stays exact while only affected retained metadata detaches');
  Check(Supports(LNext, INyxPascalSyntaxMetadataWork, LMetadataWork) and
    Supports(LWhole, INyxPascalSyntaxMetadataWork, LWholeMetadataWork) and
    (LMetadataWork.RetainedMetadataValues >= LNextWork.ReusedDeclarations) and
    (LMetadataWork.AllocatedMetadataNodes < LWholeMetadataWork.AllocatedMetadataNodes div 4) and
    (LMetadataWork.MetadataTreeHeight <= 24),
    'Checkpoint metadata and name/structure indexes actually share a balanced immutable root');
  WriteLn('metadata work / retained=', LMetadataWork.RetainedMetadataValues,
    ' copied=', LMetadataWork.CopiedMetadataValues, ' created=', LMetadataWork.CreatedMetadataValues,
    ' nodes=', LMetadataWork.AllocatedMetadataNodes, '/', LWholeMetadataWork.AllocatedMetadataNodes,
    ' height=', LMetadataWork.MetadataTreeHeight);
  Check(Supports(LNext, INyxPascalSyntaxFilterWork, LFilterWork) and
    (LFilterWork.FilteredTokenReads < LNextLexical.Count div 4) and
    (LFilterWork.FilteredTokenReads + LFilterWork.ReusedFilterTokens = LNextLexical.Count) and
    (LFilterWork.SharedFilterRows >= 256) and (LFilterWork.CopiedFilterRows < 256),
    'Completed filter blocks actually share, with only the exact repaired tail rescanned');
  Check((SnapshotText(LBase) = LBefore) and
    (AppendNyxPascalSyntax(LNext, LNextLexical, TNyxPascalSyntaxOptions.Defaults) = LNext),
    'The immutable predecessor stays exact and an unchanged lexical snapshot needs no new parser');
  LCancellation := TRestoreCancellation.Create(3);
  LCancelled := False;
  try
    LRejected := AppendNyxPascalSyntax(LBase, LNextLexical,
      TNyxPascalSyntaxOptions.Defaults, LCancellation);
  except
    on LError: ENyxPascalAnalysisCancelled do
    begin
      LCancelled := LError.Message = 'Filter directory continuation cancelled';
    end;
  end;
  Check(LCancelled and (LRejected = nil) and (SnapshotText(LBase) = LBefore),
    'Cancellation during actual shared filter construction preserves its immutable predecessor');
end;

procedure MetadataBranches;
var
  LText: TNyxText;
  LAt: Integer;
  LBaseLexical: INyxPascalLexicalSnapshot;
  LLeftLexical: INyxPascalLexicalSnapshot;
  LRightLexical: INyxPascalLexicalSnapshot;
  LBase: INyxPascalSyntaxSnapshot;
  LLeft: INyxPascalSyntaxSnapshot;
  LRight: INyxPascalSyntaxSnapshot;
  LBefore: TNyxText;
  LLeftBefore: TNyxText;
  LWork: INyxPascalSyntaxMetadataWork;
  LAccess: INyxPascalSyntaxMetadataLookupWork;
  LLookups: Integer;
  LHits: Integer;
  LTreeReads: Integer;
  LLastBaseRun: Integer;
  LBaseIdentity: IInterface;
  LReadsValid: Boolean;
  LFamily: Integer;
  LCount: Integer;
begin
  LText := 'unit Branches; interface ';
  for LAt := 1 to 80 do
  begin
    LText := LText + 'type T' + TNyxText(IntToStr(LAt)) + '=record Value:Byte; end; ' +
      'procedure Run(A:T' + TNyxText(IntToStr(LAt)) + '); overload; ';
  end;
  LText := LText + 'const Boundary=1; ';
  LBaseLexical := NewNyxPascalAppendLexical(LText, TNyxPascalLexicalOptions.Defaults);
  LBase := NewNyxPascalSyntaxFromLexical(LBaseLexical, TNyxPascalSyntaxOptions.Defaults);
  LBefore := SnapshotText(LBase);
  Check(Supports(LBase, INyxPascalSyntaxMetadataLookupWork, LAccess) and
    (LAccess.MetadataLookups > LAccess.MetadataLookupHits) and
    (LAccess.MetadataLookupHits > 0) and (LAccess.MetadataLookupTreeReads > 0) and
    (LAccess.MetadataLookupPeak = 8) and (LAccess.RetainedMetadataLookups = 0),
    'The large constructor exercises bounded lookup hits, eviction and complete publication retirement');
  LLookups := LAccess.MetadataLookups;
  LHits := LAccess.MetadataLookupHits;
  LTreeReads := LAccess.MetadataLookupTreeReads;
  LReadsValid := True;
  for LAt := 1 to 80 do
  begin
    LFamily := LBase.Named(0, 'T' + TNyxText(IntToStr(LAt)));
    LReadsValid := LReadsValid and (LFamily >= 0) and
      (LBase.Declaration(LFamily).Name = 'T' + TNyxText(IntToStr(LAt))) and
      (LBase.Named(0, 'Missing' + TNyxText(IntToStr(LAt))) = -1);
  end;
  Check(LReadsValid, 'Published reads retain every distinct declared type and exact absent name');
  Check((LAccess.MetadataLookups = LLookups) and (LAccess.MetadataLookupHits = LHits) and
    (LAccess.MetadataLookupTreeReads = LTreeReads) and (LAccess.RetainedMetadataLookups = 0),
    'Published queries cannot refill constructor entries or mutate construction receipts');
  LLastBaseRun := LBase.Named(0, 'Run');
  while LBase.Declaration(LLastBaseRun).NextNamed >= 0 do
  begin
    LLastBaseRun := LBase.Declaration(LLastBaseRun).NextNamed;
  end;
  LBaseIdentity := NyxPascalDeclarationIdentity(LBase, LLastBaseRun);
  LLeftLexical := AppendNyxPascalLexical(LBaseLexical,
    'procedure Run(A:Byte); overload; implementation end.');
  LLeft := AppendNyxPascalSyntax(LBase, LLeftLexical, TNyxPascalSyntaxOptions.Defaults);
  LLeftBefore := SnapshotText(LLeft);
  LRightLexical := AppendNyxPascalLexical(LBaseLexical,
    'procedure Run(A:Word); overload; implementation end.');
  LRight := AppendNyxPascalSyntax(LBase, LRightLexical, TNyxPascalSyntaxOptions.Defaults);
  Check((LBaseIdentity <> nil) and
    (NyxPascalDeclarationIdentity(LBase, LLastBaseRun) = LBaseIdentity) and
    (NyxPascalDeclarationIdentity(LLeft, LLastBaseRun) <> LBaseIdentity) and
    (NyxPascalDeclarationIdentity(LRight, LLastBaseRun) <> LBaseIdentity) and
    (NyxPascalDeclarationIdentity(LLeft, LLastBaseRun) <>
      NyxPascalDeclarationIdentity(LRight, LLastBaseRun)) and
    (LBase.Declaration(LLastBaseRun).NextNamed = -1),
    'A cached old row still detaches under a new COW stamp independently in each sibling');
  Check((SnapshotText(LBase) = LBefore) and (SnapshotText(LLeft) = LLeftBefore),
    'A sibling parser cannot mutate earlier declaration/scope/overload links or indexes');
  Check(SnapshotText(LLeft) = SnapshotText(NewNyxPascalSyntaxSnapshot(LText +
    'procedure Run(A:Byte); overload; implementation end.')),
    'The left overload branch equals an independent complete parse');
  Check(SnapshotText(LRight) = SnapshotText(NewNyxPascalSyntaxSnapshot(LText +
    'procedure Run(A:Word); overload; implementation end.')),
    'The right overload branch equals an independent complete parse');
  LBase := nil;
  LBaseLexical := nil;
  LFamily := LRight.Named(0, 'Run');
  LCount := 0;
  while LFamily >= 0 do
  begin
    Inc(LCount);
    LFamily := LRight.Declaration(LFamily).NextNamed;
  end;
  Check((LCount = 81) and (SnapshotText(LLeft) = LLeftBefore),
    'Detached branches retain their own complete overload families after their predecessor retires');
  Check(Supports(LRight, INyxPascalSyntaxMetadataWork, LWork) and
    (LWork.RetainedMetadataValues > LRight.DeclarationCount) and
    (LWork.CopiedMetadataValues < 20) and (LWork.MetadataTreeHeight < 32),
    'Distant overload links copy only affected rows without rebuilding retained indexes');
  Check(Supports(LRight, INyxPascalSyntaxMetadataLookupWork, LAccess) and
    (LAccess.MetadataLookupHits > 0) and (LAccess.MetadataLookupPeak <= 8) and
    (LAccess.RetainedMetadataLookups = 0),
    'Resumed construction uses only its own bounded entries and retires every row lease');
  WriteLn('metadata access / lookups=', LLookups, ' hits=', LHits,
    ' tree-reads=', LTreeReads, ' peak=8 retained=0');
end;

procedure OrdinalPages;
const
  CEdges: array[0..10] of Integer = (0, 1, 3, 4, 15, 16, 63, 64, 255, 256, 1024);
var
  LText, LBefore: TNyxText;
  LBaseLexical, LLeftLexical, LRightLexical: INyxPascalLexicalSnapshot;
  LBase, LLeft, LRight: INyxPascalSyntaxSnapshot;
  LWork, LLeftWork: INyxPascalSyntaxOrdinalMetadataWork;
  LAccess: INyxPascalSyntaxMetadataLookupWork;
  LAt, LIndex, LMode: Integer;
  LLookups, LReads, LNodes, LRoots: Integer;
  LIdentity: IInterface;
  LRejected: Boolean;
begin
  LText := 'unit OrdinalPages; interface const ';
  for LAt := 0 to 1099 do
  begin
    LText := LText + 'Value' + TNyxText(IntToStr(LAt)) + '=' + TNyxText(IntToStr(LAt)) + '; ';
  end;
  LText := LText + 'procedure Run(A:Integer); overload; ';
  LBaseLexical := NewNyxPascalAppendLexical(LText, TNyxPascalLexicalOptions.Defaults);
  LBase := NewNyxPascalSyntaxFromLexical(LBaseLexical, TNyxPascalSyntaxOptions.Defaults);
  LBefore := SnapshotText(LBase);
  Check(Supports(LBase, INyxPascalSyntaxOrdinalMetadataWork, LWork) and
    (LWork.OrdinalMetadataLookups > 0) and (LWork.OrdinalMetadataPageReads > 0) and
    (LWork.AllocatedOrdinalPages > 0) and (LWork.CapturedMetadataRoots > 0) and
    (LWork.OrdinalMetadataDepth >= 6),
    'Typed ordinal rows grow real persistent pages across four-slot directory levels');
  LLookups := LWork.OrdinalMetadataLookups;
  LReads := LWork.OrdinalMetadataPageReads;
  LNodes := LWork.AllocatedOrdinalPages;
  LRoots := LWork.CapturedMetadataRoots;
  for LAt := Low(CEdges) to High(CEdges) do
  begin
    LIndex := LBase.Named(0, 'Value' + TNyxText(IntToStr(CEdges[LAt])));
    Check((LIndex >= 0) and (LBase.Declaration(LIndex).Name =
      'Value' + TNyxText(IntToStr(CEdges[LAt]))),
      'Ordinal page growth preserves independently named rows on every boundary');
  end;
  for LMode := 0 to 2 do
  begin
    LRejected := False;
    try
      case LMode of
        0: LBase.Declaration(-1);
        1: LBase.Declaration(LBase.DeclarationCount);
        2: LBase.Declaration(High(Integer));
      end;
    except
      on ENyxPascalLexical do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Negative, next and maximum ordinals cannot wrap into an existing page slot');
  end;
  LIndex := LBase.Named(0, 'Run');
  LIdentity := NyxPascalDeclarationIdentity(LBase, LIndex);
  LLeftLexical := AppendNyxPascalLexical(LBaseLexical,
    'procedure Run(A:Byte); overload; implementation end.');
  LLeft := AppendNyxPascalSyntax(LBase, LLeftLexical, TNyxPascalSyntaxOptions.Defaults);
  LRightLexical := AppendNyxPascalLexical(LBaseLexical,
    'procedure Run(A:Word); overload; implementation end.');
  LRight := AppendNyxPascalSyntax(LBase, LRightLexical, TNyxPascalSyntaxOptions.Defaults);
  Check((SnapshotText(LLeft) = SnapshotText(NewNyxPascalSyntaxSnapshot(LText +
    'procedure Run(A:Byte); overload; implementation end.'))) and
    (SnapshotText(LRight) = SnapshotText(NewNyxPascalSyntaxSnapshot(LText +
    'procedure Run(A:Word); overload; implementation end.'))) and
    (SnapshotText(LBase) = LBefore),
    'Independent ordinal-root siblings preserve exact whole-source meaning and their earlier snapshot');
  Check((LIdentity <> nil) and (NyxPascalDeclarationIdentity(LBase, LIndex) = LIdentity) and
    (NyxPascalDeclarationIdentity(LLeft, LIndex) <> LIdentity) and
    (NyxPascalDeclarationIdentity(LRight, LIndex) <> LIdentity) and
    (NyxPascalDeclarationIdentity(LLeft, LIndex) <> NyxPascalDeclarationIdentity(LRight, LIndex)),
    'Typed cached rows still detach before sibling overload-link mutation');
  Check((LWork.OrdinalMetadataLookups = LLookups) and
    (LWork.OrdinalMetadataPageReads = LReads) and (LWork.AllocatedOrdinalPages = LNodes) and
    (LWork.CapturedMetadataRoots = LRoots) and
    Supports(LBase, INyxPascalSyntaxMetadataLookupWork, LAccess) and
    (LAccess.RetainedMetadataLookups = 0),
    'Published ordinal reads and sibling construction change no earlier receipt or retired cache');
  Check(Supports(LLeft, INyxPascalSyntaxOrdinalMetadataWork, LLeftWork) and
    (LLeftWork.AllocatedOrdinalPages < LNodes div 10) and
    (LLeftWork.CapturedMetadataRoots > 0),
    'A resumed ordinal directory allocates only the affected paths and bounded checkpoint roots');
  WriteLn('ordinal metadata / lookups=', LLookups, ' reads=', LReads,
    ' pages=', LNodes, ' roots=', LRoots, ' depth=', LWork.OrdinalMetadataDepth,
    ' append-pages=', LLeftWork.AllocatedOrdinalPages);
  LBase := nil;
  LWork := nil;
  LAccess := nil;
  LBaseLexical := nil;
  Check((LLeft.Declaration(LIndex).NextNamed >= 0) and
    (LRight.Declaration(LIndex).NextNamed >= 0),
    'Compound roots retain detached row pages after the source predecessor retires');
end;

procedure FilterSeams;
const
  CCases: array[0..7] of TNyxText = (
    'unit Raw; interface {$if A}const A=1;{$if B} B=2;{$endif}{$else} C=3;{$ifend}implementation end.',
    'unit Raw; interface {$endif}const A=1; {$ifdef OPEN} B=2; {$endif} implementation end.',
    'unit Raw; interface {$i a.inc}const A=1; {$include b.inc} B=2; implementation end.',
    'unit Raw; interface const A=1; {$if A} B=2; {$if B} C=3; {$endif}',
    'unit Raw; interface const A=1; (*{$if A}*) B=2; //{$if B}' + #13#10 + 'C=3; implementation end.',
    'unit Raw; interface const A=1; Caption=''{$ifdef TEXT}''; B=2; implementation end.',
    'unit Raw; interface type T=record Value:Byte; end; {$ifdef A}const B=2;{$endif} implementation end.',
    'unit Raw; interface const A=1; {$if A}{ unfinished (* nested *) comment }B=2;{$endif}implementation end.');
var
  LAt: Integer;
begin
  for LAt := 0 to High(CCases) do
  begin
    CompareEveryAppend(CCases[LAt], TNyxPascalSyntaxOptions.Defaults.Lexical(
      TNyxPascalLexicalOptions.Defaults.NestedComments(True)));
  end;
end;

procedure RetainedMetadata;
const
  CPrefix: TNyxText = 'unit Rich; interface uses ExternalUnit; ' +
    'type T=record case Tag:Byte of 0:(X:Integer); 1:(Y:Byte); end deprecated ''old''; ' +
    'function ConstantRole(X:Word):Word; [internconst:104]; ' +
    'procedure RuntimeRole; [internproc:8]; ' +
    'procedure Over(A:Integer); overload; procedure Over(A:Byte); overload; ' +
    'implementation procedure Local; label Start,Stop; const L=42; begin end; ' +
    'const Pad=2; Boundary=1; ';
var
  LBaseLex: INyxPascalLexicalSnapshot;
  LNextLex: INyxPascalLexicalSnapshot;
  LBase: INyxPascalSyntaxSnapshot;
  LNext: INyxPascalSyntaxSnapshot;
  LWork: INyxPascalSyntaxWork;
  LVariants: INyxPascalVariantSyntax;
  LHints: INyxPascalHintSyntax;
  LLabels: INyxPascalStatementLabelSyntax;
  LIntrinsics: INyxPascalIntrinsicSyntax;
begin
  LBaseLex := NewNyxPascalAppendLexical(CPrefix, TNyxPascalLexicalOptions.Defaults);
  LBase := NewNyxPascalSyntaxFromLexical(LBaseLex, TNyxPascalSyntaxOptions.Defaults);
  LNextLex := AppendNyxPascalLexical(LBaseLex, 'var Tail:T; end.');
  LNext := AppendNyxPascalSyntax(LBase, LNextLex, TNyxPascalSyntaxOptions.Defaults);
  Supports(LNext, INyxPascalSyntaxWork, LWork);
  Supports(LBase, INyxPascalVariantSyntax, LVariants);
  Supports(LBase, INyxPascalHintSyntax, LHints);
  Supports(LBase, INyxPascalStatementLabelSyntax, LLabels);
  Supports(LBase, INyxPascalIntrinsicSyntax, LIntrinsics);
  Check((LVariants.SelectorCount = 1) and (LVariants.ArmCount = 2) and
    (LHints.HintCount > 0) and (LLabels.StatementLabelCount = 2) and
    (LIntrinsics.IntrinsicCount = 2), 'The retained fixture actually owns every optional metadata family');
  Check(LWork.ReusedDeclarations > LLabels.StatementLabelAt(1).DeclarationIndex,
    'Optional metadata lies inside the retained parsed prefix rather than only being reconstructed');
  Check(SnapshotText(LNext) = SnapshotText(NewNyxPascalSyntaxSnapshot(CPrefix + 'var Tail:T; end.')),
    'Cloned variants, hints, labels, intrinsic entries, overload chains and indexes match a fresh whole parse');
end;

procedure Refusals;
var
  LOptions: TNyxPascalSyntaxOptions;
  LBaseLex: INyxPascalLexicalSnapshot;
  LNextLex: INyxPascalLexicalSnapshot;
  LForeignLex: INyxPascalLexicalSnapshot;
  LBase: INyxPascalSyntaxSnapshot;
  LCandidate: INyxPascalSyntaxSnapshot;
  LCancellation: INyxCancellationScope;
  LBefore: TNyxText;
  LText: TNyxText;
  LTail: TNyxText;
  LAt: Integer;
  LMode: Integer;
  LRefused: Boolean;
  LFreshRefused: Boolean;
begin
  LText := 'unit Refusal; interface type ';
  for LAt := 1 to 80 do
  begin
    LText := LText + 'T' + TNyxText(IntToStr(LAt)) + '=record X:Integer; end; ';
  end;
  LText := LText + 'const Boundary=1; ';
  LOptions := TNyxPascalSyntaxOptions.Defaults;
  LBaseLex := NewNyxPascalAppendLexical(LText, LOptions.LexicalOptions);
  LBase := NewNyxPascalSyntaxFromLexical(LBaseLex, LOptions);
  LNextLex := AppendNyxPascalLexical(LBaseLex, 'var Tail:T80; end.');
  LForeignLex := NewNyxPascalAppendLexical(LNextLex.Source, LOptions.LexicalOptions);
  LBefore := SnapshotText(LBase);
  for LMode := 0 to 2 do
  begin
    LCandidate := nil;
    LRefused := False;
    try
      case LMode of
        0:
          begin
            LCandidate := AppendNyxPascalSyntax(LBase, LForeignLex, LOptions);
          end;
        1:
          begin
            LCandidate := AppendNyxPascalSyntax(LBase, LNextLex, LOptions.UpToDiagnostics(5));
          end;
        2:
          begin
            LCancellation := TRestoreCancellation.Create;
            LCandidate := AppendNyxPascalSyntax(LBase, LNextLex, LOptions, LCancellation);
          end;
      end;
    except
      on LError: ENyxPascalAnalysisCancelled do
      begin
        LRefused := LMode = 2;
        Check(LError.Message = 'Declaration checkpoint construction cancelled',
          'Cancellation is exercised during actual restoration rather than only at factory entry');
      end;
      on ENyxPascalLexical do
      begin
        LRefused := LMode <> 2;
      end;
    end;
    Check(LRefused and (LCandidate = nil) and (SnapshotText(LBase) = LBefore),
      'Foreign authority, changed policy and cancelled restoration preserve exact prior metadata');
  end;
  for LMode := 0 to 3 do
  begin
    LOptions := TNyxPascalSyntaxOptions.Defaults;
    LText := 'unit Limits; interface type T=record X:Integer; end; const Boundary=1; ';
    LTail := 'var Tail:T; end.';
    case LMode of
      0:
        begin
          LOptions := LOptions.UpToDeclarations(3);
        end;
      1:
        begin
          LOptions := LOptions.UpToScopes(2);
          LTail := 'type Next=record Value:Integer; end; end.';
        end;
      2:
        begin
          LOptions := LOptions.UpToDiagnostics(1);
          LText := 'unit Limits; interface nonsense; const Boundary=1; ';
          LTail := '{$ifdef FEATURE}{$endif} implementation end.';
        end;
      3:
        begin
          LOptions := LOptions.UpToDepth(2);
          LText := 'unit Limits; interface const Boundary=1; ';
          LTail := 'type Next=record Child:record Value:Integer; end; end; end.';
        end;
    end;
    LBaseLex := NewNyxPascalAppendLexical(LText, LOptions.LexicalOptions);
    LBase := NewNyxPascalSyntaxFromLexical(LBaseLex, LOptions);
    LBefore := SnapshotText(LBase);
    LNextLex := AppendNyxPascalLexical(LBaseLex, LTail);
    LCandidate := nil;
    LRefused := False;
    LFreshRefused := False;
    try
      LCandidate := AppendNyxPascalSyntax(LBase, LNextLex, LOptions);
    except
      on ENyxPascalSyntaxLimit do
      begin
        LRefused := True;
      end;
    end;
    try
      LCandidate := NewNyxPascalSyntaxSnapshot(LText + LTail, LOptions);
    except
      on ENyxPascalSyntaxLimit do
      begin
        LFreshRefused := True;
      end;
    end;
    Check(LRefused and LFreshRefused and (LCandidate = nil) and (SnapshotText(LBase) = LBefore),
      'Shared node/scope/diagnostic/depth budgets refuse identically to fresh parsing after restoration');
  end;
end;

begin
  try
    Differential;
    FilterSeams;
    ActualReuse;
    MetadataBranches;
    OrdinalPages;
    RetainedMetadata;
    Refusals;
    WriteLn('PASS ', GChecks, ' declaration checkpoint checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
