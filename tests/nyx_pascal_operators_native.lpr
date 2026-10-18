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
program nyx_pascal_operators_native;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, LazFileUtils, nyx.text, operator.fixture,
  nyx.pascal.files, nyx.pascal.files.native, nyx.pascal.modules,
  nyx.pascal.context, nyx.pascal.compilertrace, nyx.studio.pascalprofiles,
  nyx.pascal.lexing, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding;

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
  LGlobal: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LReply: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LOperators: INyxPascalOperatorQueries;
  LOrigin: TNyxPascalSyntaxOrigin;
  LAt: Integer;
  LBinary: Integer;
  LOperatorSyntax: INyxPascalOperatorSyntax;
  LScope: Integer;
begin
  Check(RunOperatorForms = 30,
    'Whole handwritten input executes symbolic/named/unary/equality/conversion operators and its ordinary Add method');
  LCapture := CaptureNyxPascalSourceDirectory(ARoot + '/tests',
    NewNyxPascalFileCatalog(pfcExact), NewNyxPascalModuleCatalog,
    TNyxPascalDirectoryOptions.Defaults.Under('source').OnlyFile('operator.fixture.pas'));
  LCapture := CaptureNyxPascalSourceDirectory(ARoot + '/tests',
    LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('source').OnlyFile('operator.fixture.inc'));
  LCapture := CaptureNyxPascalSourceDirectory(ARoot + '/tests',
    LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('source').OnlyFile('operator.fixture.global.pas'));
  LFacts := CaptureNyxFPCCompilerContext(ACompiler, AOutput + '/compiler',
    NewNyxFPCCompilerProbe(NyxPascalDefine('OPERATOR_SOURCE'), ppmDelphi),
    TNyxPascalCompilerTraceOptions.Defaults);
  LSource := NewNyxPascalTranslation(LCapture.Files,
    NyxPascalPhysicalFile('source/operator.fixture.pas'), LFacts,
    TNyxPascalTranslationOptions.Defaults);
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  Check(LSource.Resolved and LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The whole original unit/include retains complete operator signatures and implementation scopes');
  LGlobal := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LCapture.Files,
    NyxPascalPhysicalFile('source/operator.fixture.global.pas'), LFacts,
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  Check(LGlobal.CompleteTranslation and (LGlobal.Syntax.DiagnosticCount = 0),
    'The actual global-operator input retains its independent ObjFPC source mode');
  { Low-level symbolic compatibility isolates typed lookup from unqualified full
    RTL capture. This does not prove actual System/ObjPas or expression inference. }
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(LView).WithModule(LGlobal);
  LSite := LWorld.At(0, 0, LView.Syntax.Lexical.Scalars);
  LReply := LWorld.Lookup(LSite, NyxPascalBindingName('TOperatorVector'));
  LType := LWorld.TypeOfSymbol(LReply.SymbolAt(0)).BoundType;
  Supports(LWorld, INyxPascalOperatorQueries, LOperators);
  LReply := LOperators.OperatorMembers(LType, LSite, pokAdd);
  Check((LReply.Status = pbsOverloads) and (LReply.Count = 3),
    'Typed lookup retains the actual three addition signatures without selecting an operand overload');
  LBinary := 0;
  for LAt := 0 to LReply.Count - 1 do
  begin
    Check(LReply.SymbolAt(LAt).Declaration.OperatorKind = pokAdd,
      'Every whole-source alternative preserves its typed family');
    LOrigin := LReply.SymbolAt(LAt).Origin;
    Check(LSource.Files.FileAt(LOrigin.FileIndex).Name = TNyxText('source/operator.fixture.inc'),
      'Declaration origins point to the original included prototypes');

    if LSource.Physical(LOrigin.FileIndex).Slice(LOrigin.PhysicalSpan) = TNyxText('+') then
    begin
      Inc(LBinary);
    end;
  end;
  Check(LBinary = 2, 'Symbolic/unary spellings stay original rather than becoming synthetic identifiers');
  LReply := LWorld.Member(LType, LSite, NyxPascalBindingName('Add'));
  Check((LReply.Count = 1) and (LReply.SymbolAt(0).Declaration.OperatorKind = pokNone),
    'The compiler-qualified ordinary method stays separate from the named operator');
  Supports(LGlobal.Syntax, INyxPascalOperatorSyntax, LOperatorSyntax);
  LAt := LOperatorSyntax.NamedOperator(0, pokMultiply);
  Check((LAt >= 0) and (LGlobal.Syntax.Declaration(LAt).OwnerName = '') and
    (LWorld.TypeOfSymbol(LWorld.Symbol(1, LAt)).BoundType.Definition.Declaration.Name = TNyxText('TGlobalProduct')),
    'The actual global operator retains its own typed family and declared result');
  LScope := LGlobal.Syntax.Declaration(LAt).NestedScope;
  LReply := LWorld.Lookup(LWorld.At(1, LScope, LGlobal.Syntax.Scope(LScope).Span.Finish),
    NyxPascalBindingName('AResult'));
  Check((LReply.Status = pbsBound) and (LReply.Count = 1),
    'The actual FPC result identifier belongs to its operator scope');
end;

begin
  try
    Check(ParamCount = 3, 'Supply repository, configured FPC and fresh owned output directory');
    Check(not DirectoryExistsUTF8(ParamStr(3)) and CreateDirUTF8(ParamStr(3)),
      'The native source/measurement consumer owns fresh compiler output');
    SourceAndCompiler(TNyxText(ParamStr(1)), TNyxText(ParamStr(2)), TNyxText(ParamStr(3)));
    WriteLn('PASS ', GChecks, ' actual record operator source checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
