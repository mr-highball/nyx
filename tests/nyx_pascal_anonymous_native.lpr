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
program nyx_pascal_anonymous_native;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, LazFileUtils, nyx.text, anonymous.fixture,
  nyx.pascal.files, nyx.pascal.files.native, nyx.pascal.modules,
  nyx.pascal.context, nyx.pascal.compilertrace, nyx.studio.pascalprofiles,
  nyx.pascal.lexing, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.types, nyx.pascal.binding;

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

function MemberType(const AWorld: INyxPascalSemanticWorld;
  const ASite: INyxPascalBindingSite; const AType: INyxPascalSemanticType;
  const AName: TNyxText): INyxPascalSemanticType;
var
  LReply: INyxPascalResolution;
begin
  LReply := AWorld.Member(AType, ASite, NyxPascalBindingName(AName));
  Check((LReply.Status = pbsBound) and (LReply.Count = 1), 'Original member retains '+String(AName));
  LReply := AWorld.TypeOfSymbol(LReply.SymbolAt(0));
  Check(LReply.Status = pbsBound, 'Original member type retains '+String(AName));
  Result := LReply.BoundType;
end;

procedure SourceAndCompiler(const ARoot, ACompiler, AOutput: TNyxText);
var
  LCapture: INyxPascalDirectoryCapture;
  LFacts: INyxPascalCompilerContext;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LReply: INyxPascalResolution;
  LScene: INyxPascalSemanticType;
  LFirst: INyxPascalSemanticType;
  LSecond: INyxPascalSemanticType;
  LNested: INyxPascalSemanticType;
  LOrigin: TNyxPascalSyntaxOrigin;
  LAt: Integer;
  LScope: Integer;
  LTypeSyntax: INyxPascalTypeSyntax;
  LDeclared: INyxPascalDeclaredTypeSyntax;
begin
  Check(RunAnonymousTypes = 44,
    'The actual compiler executes grouped/nested/array/generic/local/constant inline record access');
  LCapture := CaptureNyxPascalSourceDirectory(ARoot + '/tests',
    NewNyxPascalFileCatalog(pfcExact), NewNyxPascalModuleCatalog,
    TNyxPascalDirectoryOptions.Defaults.Under('source').OnlyFile('anonymous.fixture.pas'));
  LCapture := CaptureNyxPascalSourceDirectory(ARoot + '/tests', LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('source').OnlyFile('anonymous.fixture.inc'));
  LFacts := CaptureNyxFPCCompilerContext(ACompiler, AOutput + '/compiler',
    NewNyxFPCCompilerProbe(NyxPascalDefine('ANONYMOUS_SOURCE'), ppmDelphi),
    TNyxPascalCompilerTraceOptions.Defaults);
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LCapture.Files,
    NyxPascalPhysicalFile('source/anonymous.fixture.pas'), LFacts,
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'Whole original compiled input retains complete nested type and following declaration boundaries');
  { Symbolic compatibility qualifies owned declared shapes independently of the
    separately required actual implicit-unit/complete compiler meaning. }
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LSite := LWorld.At(0, 0, LView.Syntax.Lexical.Scalars);
  LReply := LWorld.Lookup(LSite, NyxPascalBindingName('TAnonymousScene'));
  LScene := LWorld.TypeOfSymbol(LReply.SymbolAt(0)).BoundType;
  LFirst := MemberType(LWorld, LSite, LScene, 'First');
  LSecond := MemberType(LWorld, LSite, LScene, 'Second');
  Check(LFirst.Definition.Declaration.Anonymous and (LFirst.Definition.Declaration.Name = '') and
    (LFirst.Definition.DeclarationIndex = LSecond.Definition.DeclarationIndex),
    'Actual grouped declarations share one original anonymous type owner');
  LNested := MemberType(LWorld, LSite, LFirst, 'Detail');
  Check(MemberType(LWorld, LSite, LNested, 'Level').Builtin = pbbLongInt,
    'Ordinary binding follows actual nested record members');
  LReply := LWorld.Member(LNested, LSite, NyxPascalBindingName('Level'));
  LOrigin := LReply.SymbolAt(0).Origin;
  Check((LView.Source.Files.FileAt(LOrigin.FileIndex).Name = TNyxText('source/anonymous.fixture.inc')) and
    (LView.Source.Physical(LOrigin.FileIndex).Slice(LOrigin.PhysicalSpan) = TNyxText('Level')),
    'Included nested fields retain exact original scalar/token identities after supplementary Unicode');
  LNested := MemberType(LWorld, LSite, LScene, 'Rows');
  Check((LNested.Kind = pstArray) and LNested.Target.Definition.Declaration.Anonymous and
    (MemberType(LWorld, LSite, LNested.Target, 'Count').Builtin = pbbLongInt),
    'The actual packed anonymous array element remains a declared receiver');
  LReply := LWorld.Lookup(LSite, NyxPascalBindingName('TAnonymousIntegerBox'));
  LNested := LWorld.TypeOfSymbol(LReply.SymbolAt(0)).BoundType;
  LNested := MemberType(LWorld, LSite, LNested, 'Data');
  Check(MemberType(LWorld, LSite, LNested, 'Value').Builtin = pbbLongInt,
    'The compiler-qualified generic instance retains its inline field environment');
  LReply := LWorld.Lookup(LSite, NyxPascalBindingName('Settings'));
  LNested := LWorld.TypeOfSymbol(LReply.SymbolAt(0)).BoundType;
  Check(MemberType(LWorld, LSite, LNested, 'Limit').Builtin = pbbLongInt,
    'Actual typed constant bodies stay separate from their authored initializer');
  LAt := LView.Syntax.Named(0, 'RunAnonymousTypes');
  LAt := LView.Syntax.Declaration(LAt).NextNamed;
  LScope := LView.Syntax.Declaration(LAt).NestedScope;
  LAt := LView.Syntax.Named(LScope, 'LLocal');
  LNested := LWorld.TypeOfSymbol(LWorld.Symbol(0, LAt)).BoundType;
  LNested := MemberType(LWorld, LSite, LNested, 'Metadata');
  Check(MemberType(LWorld, LSite, LNested, 'Level').Builtin = pbbLongInt,
    'Whole-source analysis retains real local anonymous declarations');
  LAt := LView.Syntax.Named(LScene.Definition.Declaration.NestedScope, 'First');
  LTypeSyntax := LView.TypeOfDeclaration(LAt, TNyxPascalTypeOptions.Defaults);
  Supports(LTypeSyntax, INyxPascalDeclaredTypeSyntax, LDeclared);
  Check(LTypeSyntax.Complete and (LDeclared.Declarations = LView.Syntax) and
    (LDeclared.DeclarationOf(LTypeSyntax.Root) = LFirst.Definition.DeclarationIndex),
    'The actual type leaf retains the same declared source owner rather than rewritten source');
end;

begin
  try
    Check(ParamCount = 3, 'Supply repository, configured FPC and fresh owned output directory');
    Check(not DirectoryExistsUTF8(ParamStr(3)) and CreateDirUTF8(ParamStr(3)),
      'The actual source consumer owns fresh compiler output');
    SourceAndCompiler(TNyxText(ParamStr(1)), TNyxText(ParamStr(2)), TNyxText(ParamStr(3)));
    WriteLn('PASS ', GChecks, ' actual anonymous type source checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
