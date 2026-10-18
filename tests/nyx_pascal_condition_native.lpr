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
program nyx_pascal_condition_native;
{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, LazFileUtils, condition.fixture, nyx.text, nyx.bytes,
  nyx.pascal.lexing, nyx.pascal.context, nyx.pascal.compilertrace,
  nyx.studio.pascalprofiles, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.binding,
  nyx.pascal.conditions;

var
  GChecks: System.LongInt;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function ReadText(const AName: String): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(AName, fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size < 65536), 'Compiler companion input is bounded');
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

procedure Run;
var
  LProbe: INyxPascalCompilerProbe;
  LFacts: INyxPascalCompilerContext;
  LRootContext: INyxPascalCompilerContext;
  LWorld: INyxPascalSemanticWorld;
  LFiles: INyxPascalFileCatalog;
  LModel: INyxPascalFileCatalog;
  LOptions: TNyxPascalTranslationOptions;
  LResult: INyxPascalSemanticTranslation;
  LView: INyxPascalTranslatedSyntax;
  LAt: System.LongInt;
  LFirst: System.LongInt;
  LSecond: System.LongInt;
  LRoot: TNyxText;
  LPart: TNyxText;
begin
  Check(ParamCount = 3, 'Supply repository, selected FPC and fresh owned runtime');
  Check(not DirectoryExistsUTF8(ParamStr(3)) and CreateDirUTF8(ParamStr(3)),
    'The native semantic consumer owns a fresh explicit runtime');
  Check(CRootCorrect and NestedCorrect,
    'The exact handwritten companion actually executes the compiler-selected local/nested/System branches');
  LProbe := NewNyxFPCCompilerProbe(NyxPascalDefine('SEMANTIC_PRIMITIVES'), ppmFPC);
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('System.Byte'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('System.SmallInt'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('System.QWord'));
  LFacts := CaptureNyxFPCCompilerContext(TNyxText(ParamStr(2)),
    TNyxText(IncludeTrailingPathDelimiter(ParamStr(3))) + 'compiler', LProbe,
    TNyxPascalCompilerTraceOptions.Defaults);
  LRootContext := LFacts.InMode(ppmDelphi);
  LModel := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('system.pas'),
      'unit System; interface type Integer=SmallInt; implementation end.')
    .WithFile(NyxPascalPhysicalFile('objpas.pas'),
      'unit ObjPas; {$mode objfpc} interface type Integer=LongInt; implementation end.');
  LOptions := TNyxPascalTranslationOptions.Defaults.Lexical(
    TNyxPascalLexicalOptions.Defaults.NestedComments(True));
  { Explicit small scope models isolate language lookup. Real primitive widths
    come from the selected compiler; these views do not replace the full RTL. }
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits))
    .WithModule(NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LModel,
      NyxPascalPhysicalFile('system.pas'), LFacts, LOptions), TNyxPascalSyntaxOptions.Defaults))
    .WithModule(NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LModel,
      NyxPascalPhysicalFile('objpas.pas'), LRootContext, TNyxPascalTranslationOptions.Defaults),
      TNyxPascalSyntaxOptions.Defaults));
  LRoot := ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'tests/condition.fixture.pas');
  LPart := ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'tests/condition.fixture.inc');
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('fixtures/condition.fixture.pas'), LRoot)
    .WithFile(NyxPascalPhysicalFile('fixtures/condition.fixture.inc'), LPart);
  LResult := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('fixtures/condition.fixture.pas'),
    LRootContext, LWorld, TNyxPascalTranslationOptions.Defaults,
    TNyxPascalSyntaxOptions.Defaults, TNyxPascalConditionalOptions.Defaults);
  WriteLn('native semantic / answers=', LResult.Answers, ' passes=', LResult.Passes,
    ' pending=', LResult.Pending, ' issue=', Ord(LResult.BindingIssue), ' ', LResult.RelatedName);
  Check(LResult.Source.Resolved and not LResult.Pending and (LResult.Answers = 5),
    'Ordered semantic translation agrees with the executed whole companion');
  Check((LResult.Source.Files.TextAt(0) = LRoot) and (LResult.Source.Files.TextAt(1) = LPart),
    'Both complete compiled source files remain exact through refinement');
  Check((LResult.Source.State(LResult.Source.StateCount - 1).Symbol(
      NyxPascalDefine('SAW_BEFORE')).IsDefined = ppkTrue) and
    (LResult.Source.State(LResult.Source.StateCount - 1).Symbol(
      NyxPascalDefine('SAW_AFTER')).IsDefined = ppkTrue),
    'The same include retains different scoped decisions and ordinary state effects at each visit');
  LView := NewNyxPascalTranslatedSyntax(LResult.Source, TNyxPascalSyntaxOptions.Defaults);
  LFirst := -1;
  LSecond := -1;
  for LAt := 0 to LView.Syntax.DeclarationCount - 1 do
  begin

    if LView.Syntax.Declaration(LAt).Name = TNyxText('CRootCorrect') then
    begin
      LFirst := LAt;
    end;

    if LView.Syntax.Declaration(LAt).Name = TNyxText('NestedCorrect') then
    begin
      LSecond := LAt;
    end;
  end;
  Check((LFirst >= 0) and (LSecond >= 0), 'Final ordinary syntax contains the actual selected declarations');
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' selected compiler and executed conditional companion checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
