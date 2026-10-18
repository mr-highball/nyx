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
program nyx_pascal_variant_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding;

const
  CSource: TNyxText = 'unit VariantShapes; interface type ' +
    'TValue=record Fixed:LongInt; case Kind:LongInt of ' +
    '-2,0..1:(Left,Right:LongInt); ' +
    '2:(Prefix:LongInt; case Boolean of False:(Low:LongInt); True:(High:LongInt)); ' +
    '3:() end; ' +
    'TOther=record case Byte of 0:(Value:LongInt) end; ' +
    'TAlias=TValue deprecated ''Use TValue'' platform; ' +
    'TNames=record &deprecated:LongInt; List:array of &deprecated; end; ' +
    'TCallback=procedure; cdecl deprecated; ' +
    'TObjectView=class private FValue:LongInt; public ' +
    'property Value:LongInt read FValue deprecated ''Use Current''; end; ' +
    'const OldCount=3 deprecated; ' +
    'var OldA,OldB:LongInt experimental; ' +
    'procedure OldWork; deprecated ''Use Work''; platform; ' +
    'procedure FutureWork; unimplemented; implementation end.';

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

{ A controlled symbolic world isolates syntax/member contracts from full RTL
  meaning. The real compiler companion separately exercises original source. }
function View(const ASource: TNyxText): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('source.pas'), ASource),
    NyxPascalPhysicalFile('source.pas'), NewNyxPascalCompilerContext(ppcFPC,
      TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True),
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
end;

procedure StructureAndMembers;
var
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LVariants: INyxPascalVariantSyntax;
  LHints: INyxPascalHintSyntax;
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LType: INyxPascalSemanticType;
  LReply: INyxPascalResolution;
  LMembers: INyxPascalMemberQueries;
  LPage: INyxPascalMemberPage;
  LScope: Integer;
  LOther: Integer;
  LAt: Integer;
  LHint: Integer;
  LArm: Integer;
  LLabel: Integer;
  LRejected: Boolean;
begin
  LView := View(CSource);
  LSyntax := NewNyxPascalSyntaxSnapshot(CSource);
  Check(LView.CompleteTranslation and (LSyntax.DiagnosticCount = 0),
    'Nested/tagged/untagged/empty variants and declaration hints retain complete syntax');
  Check(Supports(LSyntax, INyxPascalVariantSyntax, LVariants) and
    Supports(LSyntax, INyxPascalHintSyntax, LHints),
    'Existing syntax exposes optional immutable variant and hint capabilities');
  Check((LVariants.SelectorCount = 3) and (LVariants.ArmCount = 6) and
    (LVariants.LabelCount = 7), 'Selectors, arms and grouped/range labels retain separate entries');
  LScope := LSyntax.Declaration(LSyntax.Named(0, 'TValue')).NestedScope;
  LOther := LSyntax.Declaration(LSyntax.Named(0, 'TOther')).NestedScope;
  Check((LVariants.SelectorAt(0).RecordScope = LScope) and
    (LVariants.SelectorAt(0).ParentArm = -1) and LVariants.SelectorAt(0).Complete and
    (LVariants.SelectorAt(0).TagDeclaration = LSyntax.Named(LScope, 'Kind')),
    'A named selector contributes its actual tag field in the enclosing record');
  Check((LSyntax.Lexical.Slice(LVariants.SelectorAt(0).TypeSpan) = TNyxText('LongInt')) and
    (LSyntax.Declaration(LSyntax.Named(LScope, 'Kind')).VariantArm = -1) and
    (LSyntax.Declaration(LSyntax.Named(LScope, 'Fixed')).VariantArm = -1),
    'Selector type and fixed/tag fields stay separate from alternative membership');
  LArm := LVariants.SelectorAt(0).FirstArm;
  Check((LSyntax.Declaration(LSyntax.Named(LScope, 'Left')).VariantArm = LArm) and
    (LSyntax.Declaration(LSyntax.Named(LScope, 'Right')).VariantArm = LArm) and
    LVariants.ArmAt(LArm).Complete, 'Grouped fields preserve the same complete arm');
  LLabel := LVariants.ArmAt(LArm).FirstLabel;
  Check((LSyntax.Lexical.Slice(LVariants.LabelAt(LLabel).Span) = TNyxText('-2')) and
    (LSyntax.Lexical.Slice(LVariants.LabelAt(LVariants.LabelAt(LLabel).NextLabel).Span) =
      TNyxText('0..1')), 'Label signs/ranges preserve exact source without evaluation');
  LArm := LVariants.ArmAt(LArm).NextArm;
  Check((LVariants.SelectorAt(1).ParentArm = LArm) and
    (LVariants.SelectorAt(1).TagDeclaration = -1) and
    (LSyntax.Declaration(LSyntax.Named(LScope, 'Prefix')).VariantArm = LArm) and
    (LSyntax.Declaration(LSyntax.Named(LScope, 'Low')).VariantArm = LVariants.SelectorAt(1).FirstArm),
    'Nested selector/field relationships remain in one record member scope');
  Check((LVariants.SelectorAt(2).RecordScope = LOther) and
    (LVariants.SelectorAt(2).ParentArm = -1) and (LSyntax.Named(LScope, 'Boolean') < 0),
    'Separate records and unnamed selector types never become invented tag fields');
  LAt := LSyntax.Named(0, 'TAlias');
  LHint := LSyntax.Declaration(LAt).FirstHint;
  Check((LSyntax.Lexical.Slice(LSyntax.Declaration(LAt).TypeSpan) = TNyxText('TValue')) and
    (LHints.HintAt(LHint).Kind = phkDeprecated) and
    (LSyntax.Lexical.Slice(LHints.HintAt(LHint).MessageSpan) = TNyxText('''Use TValue''')) and
    (LHints.HintAt(LHints.HintAt(LHint).NextHint).Kind = phkPlatform),
    'Typed ordered hints and unevaluated messages never contaminate alias type syntax');
  Check((LHints.HintAt(LSyntax.Declaration(LSyntax.Named(0, 'OldA')).FirstHint).DeclarationIndex =
    LSyntax.Named(0, 'OldA')) and
    (LHints.HintAt(LSyntax.Declaration(LSyntax.Named(0, 'OldB')).FirstHint).DeclarationIndex =
    LSyntax.Named(0, 'OldB')), 'Grouped declarations own independent hint links');
  Check(LHints.HintAt(LSyntax.Declaration(LSyntax.Named(0, 'OldCount')).FirstHint).Kind = phkDeprecated,
    'Constants retain typed hints separately from their value expressions');
  LHint := LSyntax.Declaration(LSyntax.Named(0, 'OldWork')).FirstHint;
  Check((LHints.HintAt(LHint).Kind = phkDeprecated) and
    (LHints.HintAt(LHints.HintAt(LHint).NextHint).Kind = phkPlatform) and
    (LHints.HintAt(LSyntax.Declaration(LSyntax.Named(0, 'FutureWork')).FirstHint).Kind = phkUnimplemented),
    'Routine directives retain ordered standard hints and messages');
  LAt := LSyntax.Declaration(LSyntax.Named(0, 'TObjectView')).NestedScope;
  Check(LHints.HintAt(LSyntax.Declaration(LSyntax.Named(LAt, 'Value')).FirstHint).Kind = phkDeprecated,
    'Property accessor expressions leave their declaration hint visible');
  LAt := LSyntax.Declaration(LSyntax.Named(0, 'TNames')).NestedScope;
  Check((LSyntax.Named(LAt, 'deprecated') >= 0) and
    (LSyntax.Declaration(LSyntax.Named(LAt, 'deprecated')).FirstHint = -1) and
    (LSyntax.Lexical.Slice(LSyntax.Declaration(LSyntax.Named(LAt, 'List')).TypeSpan) =
      TNyxText('array of &deprecated')), 'Escaped/type-operand identifiers are never consumed as hints');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LSite := LWorld.At(0, 0, LSyntax.Lexical.Scalars);
  LReply := LWorld.Lookup(LSite, NyxPascalBindingName('TAlias'));
  LType := LWorld.TypeOfSymbol(LReply.SymbolAt(0)).BoundType;
  LReply := LWorld.Member(LType, LSite, NyxPascalBindingName('High'));
  Check((LReply.Status = pbsBound) and (LReply.Count = 1) and
    (LReply.SymbolAt(0).Declaration.Scope = LScope),
    'Ordinary alias/member binding reaches a nested variant field without an active-arm guess');
  Supports(LWorld, INyxPascalMemberQueries, LMembers);
  LPage := LMembers.Members(LType, LSite, TNyxPascalMemberQuery.Defaults.Matching(''));
  Check(LPage.ResultSet.Count = 7, 'Ordinary bounded completion lists fixed/tag and all alternative fields');
  LRejected := False;
  try
    LVariants.ArmAt(LVariants.ArmCount);
  except
    on ENyxPascalLexical do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Snapshot-local metadata queries reject out-of-range ordinals');
end;

procedure Refusals;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LVariants: INyxPascalVariantSyntax;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
  LAt: Integer;
begin
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Broken; interface type T=record case Byte of 0:(X:LongInt end; implementation end.');
  Supports(LSyntax, INyxPascalVariantSyntax, LVariants);
  LAt := LSyntax.Declaration(LSyntax.Named(0, 'T')).NestedScope;
  Check((LSyntax.DiagnosticCount > 0) and LSyntax.Scope(LAt).UnsupportedSyntax and
    not LVariants.SelectorAt(0).Complete and not LVariants.ArmAt(0).Complete,
    'Unfinished variants retain unsafe surrounding scope and incomplete metadata');
  LSyntax := nil;
  LRejected := False;
  try
    LSyntax := NewNyxPascalSyntaxSnapshot(CSource,
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(8));
  except
    on ENyxPascalSyntaxLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSyntax = nil), 'Variant/label metadata shares the atomic declaration budget');
  LRejected := False;
  try
    LSyntax := NewNyxPascalSyntaxSnapshot('unit H; interface type T=LongInt deprecated platform;',
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(2));
  except
    on ENyxPascalSyntaxLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSyntax = nil), 'Hint metadata cannot grow outside the declaration budget');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LSyntax := NewNyxPascalSyntaxSnapshot(CSource, TNyxPascalSyntaxOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSyntax = nil), 'Cancelled candidates publish no partial syntax');
end;

begin
  try
    StructureAndMembers;
    Refusals;
    WriteLn('PASS ', GChecks, ' variant record/hint syntax and member checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
