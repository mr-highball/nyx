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
program nyx_pascal_variants_native;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, LazFileUtils, nyx.text, variant.fixture,
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

procedure SourceAndCompiler(const ARoot, ACompiler, AOutput: TNyxText);
var
  LCapture: INyxPascalDirectoryCapture;
  LFacts: INyxPascalCompilerContext;
  LSource: INyxPascalTranslation;
  LView: INyxPascalTranslatedSyntax;
  LVariants: INyxPascalVariantSyntax;
  LHints: INyxPascalHintSyntax;
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LReply: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LMembers: INyxPascalMemberQueries;
  LPage: INyxPascalMemberPage;
  LOrigin: TNyxPascalSyntaxOrigin;
  LAt: Integer;
  LScope: Integer;
  LHint: Integer;
begin
  Check(RunVariants = 34,
    'Actual configured compiler executes ordinary/nested fields and overlapping ordinal views');
  LCapture := CaptureNyxPascalSourceDirectory(ARoot + '/tests',
    NewNyxPascalFileCatalog(pfcExact), NewNyxPascalModuleCatalog,
    TNyxPascalDirectoryOptions.Defaults.Under('source').OnlyFile('variant.fixture.pas'));
  LCapture := CaptureNyxPascalSourceDirectory(ARoot + '/tests', LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('source').OnlyFile('variant.fixture.inc'));
  LFacts := CaptureNyxFPCCompilerContext(ACompiler, AOutput + '/compiler',
    NewNyxFPCCompilerProbe(NyxPascalDefine('VARIANT_SOURCE'), ppmDelphi),
    TNyxPascalCompilerTraceOptions.Defaults);
  LSource := NewNyxPascalTranslation(LCapture.Files,
    NyxPascalPhysicalFile('source/variant.fixture.pas'), LFacts,
    TNyxPascalTranslationOptions.Defaults);
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  Check(LSource.Resolved and LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The whole original compiled unit/include retains supported variant/hint syntax');
  Check(Supports(LView.Syntax, INyxPascalVariantSyntax, LVariants) and
    (LVariants.SelectorCount = 3) and (LVariants.ArmCount = 6),
    'Whole source retains anonymous/named/nested selector relationships');
  LScope := LView.Syntax.Declaration(LView.Syntax.Named(0, 'TPacket')).NestedScope;
  Check((LVariants.SelectorAt(2).ParentArm = LVariants.ArmAt(LVariants.SelectorAt(1).FirstArm).NextArm) and
    (LVariants.SelectorAt(2).TagDeclaration = LView.Syntax.Named(LScope, 'Mode')),
    'The original nested selector retains its actual tag declaration and parent arm');
  { This low-level world does not replace complete System/ObjPas capture. It
    qualifies ordinary declared-member/alias access, never target offsets. }
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LSite := LWorld.At(0, 0, LView.Syntax.Lexical.Scalars);
  LReply := LWorld.Lookup(LSite, NyxPascalBindingName('TPacket'));
  LType := LWorld.TypeOfSymbol(LReply.SymbolAt(0)).BoundType;
  LReply := LWorld.Member(LType, LSite, NyxPascalBindingName('Detail'));
  Check((LReply.Status = pbsBound) and (LReply.Count = 1) and
    (LReply.SymbolAt(0).Declaration.Scope = LScope),
    'Ordinary binding resolves an original nested variant field');
  LOrigin := LReply.SymbolAt(0).Origin;
  Check((LSource.Files.FileAt(LOrigin.FileIndex).Name = TNyxText('source/variant.fixture.inc')) and
    (LSource.Physical(LOrigin.FileIndex).Slice(LOrigin.PhysicalSpan) = TNyxText('Detail')),
    'Field identity maps to the exact authored include after supplementary Unicode');
  Supports(LWorld, INyxPascalMemberQueries, LMembers);
  LPage := LMembers.Members(LType, LSite, TNyxPascalMemberQuery.Defaults.Matching(''));
  Check(LPage.ResultSet.Count = 8, 'Completion lists fixed/tag and all nested alternative fields');
  LAt := LView.Syntax.Named(0, 'TLegacyCoordinates');
  Supports(LView.Syntax, INyxPascalHintSyntax, LHints);
  LHint := LView.Syntax.Declaration(LAt).FirstHint;
  Check((LHints.HintAt(LHint).Kind = phkDeprecated) and
    LView.TypeOfDeclaration(LAt, TNyxPascalTypeOptions.Defaults).Complete,
    'A real compiled deprecated alias retains a complete uncontaminated type expression');
  LOrigin := LView.Origin(LView.OriginAt(LHints.HintAt(LHint).MessageSpan.Start));
  Check(LSource.Physical(LOrigin.FileIndex).Slice(LOrigin.PhysicalSpan) = TNyxText('''Use TCoordinates'''),
    'Hint messages retain original physical string spelling');
  LReply := LWorld.Lookup(LSite, NyxPascalBindingName('TLegacyCoordinates'));
  LType := LWorld.TypeOfSymbol(LReply.SymbolAt(0)).BoundType;
  LReply := LWorld.Member(LType, LSite, NyxPascalBindingName('X'));
  Check((LReply.Status = pbsBound) and (LReply.Count = 1),
    'The existing alias/member consumer reaches fields through a hinted declaration');
end;

begin
  try
    Check(ParamCount = 3, 'Supply repository, configured FPC and fresh owned output directory');
    Check(not DirectoryExistsUTF8(ParamStr(3)) and CreateDirUTF8(ParamStr(3)),
      'The native source consumer owns fresh compiler output');
    SourceAndCompiler(TNyxText(ParamStr(1)), TNyxText(ParamStr(2)), TNyxText(ParamStr(3)));
    WriteLn('PASS ', GChecks, ' actual variant record source checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
