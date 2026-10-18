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
program nyx_pascal_routines_native;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, LazFileUtils, nyx.text, routine.fixture,
  nyx.pascal.files, nyx.pascal.files.native, nyx.pascal.modules,
  nyx.pascal.context, nyx.pascal.compilertrace, nyx.studio.pascalprofiles,
  nyx.pascal.lexing, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.types, nyx.pascal.syntax.translated;

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
  LType: INyxPascalTypeSyntax;
  LOrigin: TNyxPascalSyntaxOrigin;
  LAt: Integer;
  LMembers: Integer;
begin
  Check(RunCallbackForms = 34,
    'Whole handwritten compiler input invokes aliased, inline-field and inline-variable callbacks');
  LCapture := CaptureNyxPascalSourceDirectory(ARoot + '/tests',
    NewNyxPascalFileCatalog(pfcExact), NewNyxPascalModuleCatalog,
    TNyxPascalDirectoryOptions.Defaults.Under('source').OnlyFile('routine.fixture.pas'));
  LCapture := CaptureNyxPascalSourceDirectory(ARoot + '/tests',
    LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('source').OnlyFile('routine.fixture.inc'));
  LFacts := CaptureNyxFPCCompilerContext(ACompiler, AOutput + '/compiler',
    NewNyxFPCCompilerProbe(NyxPascalDefine('ROUTINE_SOURCE'), ppmDelphi),
    TNyxPascalCompilerTraceOptions.Defaults);
  LSource := NewNyxPascalTranslation(LCapture.Files,
    NyxPascalPhysicalFile('source/routine.fixture.pas'), LFacts,
    TNyxPascalTranslationOptions.Defaults);
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  Check(LSource.Resolved and LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The same whole root/include parse without source substitutions or callback suffix diagnostics');
  LAt := LView.Syntax.Named(0, 'TCompute');
  LType := LView.TypeOfDeclaration(LAt, TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).CallingConvention = pccCdecl),
    'The actual included callback retains its typed calling convention');
  LOrigin := LView.Origin(LView.OriginAt(LType.Node(LType.Root).Span.Finish - 1));
  Check((LSource.Files.FileAt(LOrigin.FileIndex).Name = TNyxText('source/routine.fixture.inc')) and
    (LSource.Physical(LOrigin.FileIndex).Slice(LOrigin.PhysicalSpan) = TNyxText('cdecl')),
    'The suffix has its exact original include/scalar origin');
  Check(LView.Syntax.Named(0, 'stdcall') >= 0,
    'The actual escaped convention name remains an independently compiled alias');
  LMembers := LView.Syntax.Declaration(LView.Syntax.Named(0, 'THandlers')).NestedScope;
  LType := LView.TypeOfDeclaration(LView.Syntax.Named(LMembers, 'Compute'),
    TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).CallingConvention = pccCdecl) and
    (LView.Syntax.Named(LMembers, 'After') >= 0),
    'Actual record fields retain callback typing and the following member');
end;

begin
  try
    Check(ParamCount = 3, 'Supply repository, configured FPC and fresh owned output directory');
    Check(not DirectoryExistsUTF8(ParamStr(3)) and CreateDirUTF8(ParamStr(3)),
      'The native source/measurement consumer owns fresh compiler output');
    SourceAndCompiler(TNyxText(ParamStr(1)), TNyxText(ParamStr(2)), TNyxText(ParamStr(3)));
    WriteLn('PASS ', GChecks, ' actual procedural callback source checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
