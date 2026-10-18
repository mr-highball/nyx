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
program nyx_pascal_anonymous_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.editing, nyx.pascal.lexing, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax, nyx.pascal.types,
  nyx.pascal.syntax.translated, nyx.pascal.binding, nyx.pascal.names,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.completion, nyx.pascal.completion.owned;

const
  CSource: TNyxText = 'unit AnonymousShapes; interface type ' +
    'TParent=record Left,Right:record Value:LongInt; Nest:record Caption:LongInt; end; end; ' +
    'Items:array[0..1] of packed record Key:LongInt; end; ' +
    'case Mode:Byte of 0:(Extra:record case Tag:Boolean of False:(Low:LongInt); ' +
    'True:(High:LongInt); end); 1:(Other:LongInt) end; ' +
    'TBag=array[0..1] of record Code:LongInt; end; ' +
    'TBox<T>=record Data:record Value:T; end; end; TIntBox=TBox<LongInt>; ' +
    'TMethod=procedure of object; ' +
    'var First,Second:record Count:LongInt; end; ' +
    'const Settings:record Limit:LongInt; end=(Limit:8); implementation end.';
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

{ This controlled symbolic world qualifies declared types without substituting
  synthetic System/ObjPas exports for the separately required full SDK meaning. }
function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
end;

function View: INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('source.pas'), CSource),
    NyxPascalPhysicalFile('source.pas'), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

function NamedType(const AWorld: INyxPascalSemanticWorld;
  const ASite: INyxPascalBindingSite; const AName: TNyxText): INyxPascalSemanticType;
var
  LReply: INyxPascalResolution;
begin
  LReply := AWorld.Lookup(ASite, NyxPascalBindingName(AName));
  Check((LReply.Status = pbsBound) and (LReply.Count = 1), 'Lookup retains ' + String(AName));
  LReply := AWorld.TypeOfSymbol(LReply.SymbolAt(0));
  Check(LReply.Status = pbsBound, 'Type binding retains ' + String(AName));
  Result := LReply.BoundType;
end;

function FieldType(const AWorld: INyxPascalSemanticWorld;
  const ASite: INyxPascalBindingSite; const AType: INyxPascalSemanticType;
  const AName: TNyxText): INyxPascalSemanticType;
var
  LReply: INyxPascalResolution;
begin
  LReply := AWorld.Member(AType, ASite, NyxPascalBindingName(AName));
  Check((LReply.Status = pbsBound) and (LReply.Count = 1), 'Member lookup retains ' + String(AName));
  LReply := AWorld.TypeOfSymbol(LReply.SymbolAt(0));
  Check(LReply.Status = pbsBound, 'Member type binding retains ' + String(AName));
  Result := LReply.BoundType;
end;

procedure OwnedTypes;
var
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LOther: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LParent: INyxPascalSemanticType;
  LLeft: INyxPascalSemanticType;
  LRight: INyxPascalSemanticType;
  LNested: INyxPascalSemanticType;
  LMembers: INyxPascalMemberQueries;
  LPage: INyxPascalMemberPage;
  LStructures: INyxPascalStructuredSyntax;
  LType: INyxPascalTypeSyntax;
  LDeclared: INyxPascalDeclaredTypeSyntax;
  LVariants: INyxPascalVariantSyntax;
  LAt: Integer;
  LScope: Integer;
  LRejected: Boolean;
begin
  LView := View;
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'Whole nested/container/grouped/generic/constant syntax retains complete boundaries');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LSite := LWorld.At(0, 0, LView.Syntax.Lexical.Scalars);
  LParent := NamedType(LWorld, LSite, 'TParent');
  LLeft := FieldType(LWorld, LSite, LParent, 'Left');
  LRight := FieldType(LWorld, LSite, LParent, 'Right');
  Check(LLeft.Definition.Declaration.Anonymous and
    (LLeft.Definition.Declaration.Name = '') and
    (LLeft.Definition.DeclarationIndex = LRight.Definition.DeclarationIndex),
    'Grouped fields share one unnamed type identity instead of duplicated scopes or fake exports');
  LNested := FieldType(LWorld, LSite, LLeft, 'Nest');
  Check(LNested.Definition.Declaration.Anonymous and
    (LNested.Definition.DeclarationIndex <> LLeft.Definition.DeclarationIndex),
    'Nested bodies retain separate declared identities');
  Check(FieldType(LWorld, LSite, LNested, 'Caption').Builtin = pbbLongInt,
    'Ordinary member/type binding follows multiple anonymous levels');
  Supports(LWorld, INyxPascalMemberQueries, LMembers);
  LPage := LMembers.Members(LParent, LSite, TNyxPascalMemberQuery.Defaults.Matching(''));
  Check(LPage.ResultSet.Count = 6, 'Ordinary completion never exposes anonymous declaration names');
  LNested := FieldType(LWorld, LSite, LParent, 'Items');
  Check((LNested.Kind = pstArray) and LNested.Target.Definition.Declaration.Anonymous and
    (FieldType(LWorld, LSite, LNested.Target, 'Key').Builtin = pbbLongInt),
    'Container elements retain their packed anonymous declaration identity');
  LNested := NamedType(LWorld, LSite, 'TBag');
  Check((LNested.Kind = pstAlias) and (LNested.Target.Kind = pstArray) and
    (FieldType(LWorld, LSite, LNested.Target.Target, 'Code').Builtin = pbbLongInt),
    'Named container aliases reach their original anonymous elements');
  LNested := NamedType(LWorld, LSite, 'TIntBox');
  LNested := FieldType(LWorld, LSite, LNested, 'Data');
  Check(FieldType(LWorld, LSite, LNested, 'Value').Builtin = pbbLongInt,
    'Anonymous fields preserve concrete generic environments');
  LLeft := NamedType(LWorld, LSite, 'First');
  LRight := NamedType(LWorld, LSite, 'Second');
  Check(LLeft.Definition.DeclarationIndex = LRight.Definition.DeclarationIndex,
    'Grouped variables share the same owned inline type');
  LNested := NamedType(LWorld, LSite, 'Settings');
  Check(FieldType(LWorld, LSite, LNested, 'Limit').Builtin = pbbLongInt,
    'Typed constants keep inline body ownership separate from initializer syntax');
  LAt := LView.Syntax.Named(LParent.Definition.Declaration.NestedScope, 'Left');
  LType := LView.TypeOfDeclaration(LAt, TNyxPascalTypeOptions.Defaults);
  Supports(LType, INyxPascalDeclaredTypeSyntax, LDeclared);
  Supports(LView.Syntax, INyxPascalStructuredSyntax, LStructures);
  Check(LType.Complete and (LType.Node(LType.Root).Kind = ptxStructured) and
    (LDeclared.Declarations = LView.Syntax) and
    (LStructures.TypeAt(LType.Node(LType.Root).Span.Start) = LDeclared.DeclarationOf(LType.Root)),
    'Type leaves retain the exact immutable declaration owner/source-position index');
  LType := NewNyxPascalTypeSyntax(LView.Syntax.Lexical, LView.Syntax.Declaration(LAt).TypeSpan,
    TNyxPascalTypeOptions.Defaults);
  Check(not LType.Complete, 'Standalone type scanning never invents ownership for a balanced body');
  LType := LView.TypeOfDeclaration(LView.Syntax.Named(0, 'TMethod'), TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).Kind = ptxProcedure) and
    (ptfOfObject in LType.Node(LType.Root).Flags),
    'A procedural of object qualifier never becomes an anonymous structured body');
  LScope := LParent.Definition.Declaration.NestedScope;
  Check((LView.Syntax.Named(LScope, '') < 0) and (LView.Syntax.Named(LScope, 'record') < 0),
    'Anonymous identities never pollute the named declaration index');
  Supports(LView.Syntax, INyxPascalVariantSyntax, LVariants);
  Check((LVariants.SelectorCount = 2) and (LVariants.SelectorAt(0).ParentArm = -1) and
    (LVariants.SelectorAt(1).ParentArm = -1) and
    (LVariants.SelectorAt(0).RecordScope <> LVariants.SelectorAt(1).RecordScope),
    'Entering a new record resets variant ownership even inside an outer alternative');
  LOther := LWorld.WithModule(View);
  LRejected := False;
  try
    LOther.Member(LParent, LOther.At(0, 0, LView.Syntax.Lexical.Scalars), NyxPascalBindingName('Left'));
  except
    on ENyxPascalBinding do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'World replacement rejects old receiver/site ownership');
end;

procedure OrdinaryCompletion;
var
  LProvider: INyxPascalCompletionProvider;
  LSources: INyxPascalSourceSnapshot;
  LReply: INyxPascalCompletionResult;
  LRoots: array of TNyxPascalPhysicalFile;
  LPrefix: TNyxText;
begin
  SetLength(LRoots, 0);
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts, NewNyxPascalFileCatalog(pfcExact),
    LRoots, TNyxOwnedPascalCompletionOptions.Defaults.WithBinding(TNyxPascalBindingOptions.Defaults));
  LPrefix := 'unit Consumer; interface implementation procedure Run; ' +
    'var Host:record Event:record Caption:LongInt; end; end; begin Host.Event.Cap';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),
    'unit Consumer; interface implementation end.')).WithBuffer(TNyxPascalSourceRef.Main,
    LPrefix + '; end; end.');
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(LPrefix)));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and
    (LReply.Item(0).Name = TNyxText('Caption')),
    'The existing completion provider follows an ordinary anonymous local-field receiver');
end;

procedure Refusals;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LType: INyxPascalTypeSyntax;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
  LAt: Integer;
begin
  LSyntax := NewNyxPascalSyntaxSnapshot(
    'unit Broken; interface var Value:record Count:LongInt;');
  LAt := LSyntax.Named(0, 'Value');
  LType := NewNyxPascalDeclaredTypeSyntax(LSyntax, LSyntax.Declaration(LAt).TypeSpan,
    TNyxPascalTypeOptions.Defaults);
  Check((LSyntax.DiagnosticCount > 0) and not LType.Complete,
    'An unclosed body never gains a complete structured type leaf');
  LSyntax := nil;
  LRejected := False;
  try
    LSyntax := NewNyxPascalSyntaxSnapshot(CSource,
      TNyxPascalSyntaxOptions.Defaults.UpToScopes(2));
  except
    on ENyxPascalSyntaxLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSyntax = nil), 'Inline bodies share atomic scope construction budgets');
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
  Check(LRejected and (LSyntax = nil), 'Cancelled inline-body construction publishes no candidate');
end;

begin
  try
    OwnedTypes;
    OrdinaryCompletion;
    Refusals;
    WriteLn('PASS ', GChecks, ' anonymous type ownership/member/completion checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
