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
program nyx_pascal_intrinsic_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.lexing, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated
  {$ifndef PAS2JS}, intrinsic.fixture{$endif};

{$ifndef PAS2JS}
const
  { This is the configured native compiler's constant use of the authored unit,
    not a value computed or admitted by the portable Nyx declaration reader. }
  CFoldedWord = SwapFixtureWord($0102);
{$endif}

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

function View(const AText: TNyxText): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('attributes.pas'), AText),
    NyxPascalPhysicalFile('attributes.pas'), NewNyxPascalCompilerContext(ppcFPC,
      TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True),
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
end;

function Decl(const ASyntax: INyxPascalSyntaxSnapshot; const AName: TNyxText): TNyxPascalSyntaxDeclaration;
var
  LAt: Integer;
begin
  LAt := ASyntax.Named(0, AName);

  if LAt < 0 then
  begin
    raise Exception.Create('The attributed routine lost its source declaration');
  end;
  Result := ASyntax.Declaration(LAt);
end;

{ Multi-token translated ranges include synthetic separators. Compare authored
  spelling only through the retained physical occurrence and scalar spans. }
function OriginalText(const AView: INyxPascalTranslatedSyntax; const ASpan: TNyxPascalSpan): TNyxText;
var
  LFirst: TNyxPascalSyntaxOrigin;
  LLast: TNyxPascalSyntaxOrigin;
begin
  LFirst := AView.Origin(AView.OriginAt(ASpan.Start));
  LLast := AView.Origin(AView.OriginAt(ASpan.Finish - 1));

  if LFirst.Occurrence <> LLast.Occurrence then
  begin
    raise Exception.Create('This fixture operand requires one original occurrence');
  end;
  Result := NyxPascalOccurrenceLexical(AView.Source, LFirst.Occurrence).Slice(
    NyxPascalSpan(LFirst.PhysicalSpan.Start, LLast.PhysicalSpan.Finish - LFirst.PhysicalSpan.Start));
end;

procedure MetadataAndBodies;
var
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LIntrinsics: INyxPascalIntrinsicSyntax;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LEntry: TNyxPascalIntrinsicDeclaration;
  LAt: Integer;
  LRejected: Boolean;
  LCancel: INyxCancellationScope;
begin
  LView := View('unit Roles; interface function ConstantRole(X:Word):Word; ' +
    '[internconst: CompilerKind + (2*3)]; function RuntimeRole(X:Word):Word; ' +
    '[internproc: 12]; function After:Word; implementation end.');
  LSyntax := LView.Syntax;
  Check(LView.CompleteTranslation and (LSyntax.DiagnosticCount = 0),
    'Constant/runtime annotations retain complete routine sibling syntax');
  Check(Supports(LSyntax, INyxPascalIntrinsicSyntax, LIntrinsics) and
    (LIntrinsics.IntrinsicCount = 2), 'Optional typed metadata shares the exact immutable declaration arena');
  LDeclaration := Decl(LSyntax, 'ConstantRole');
  LEntry := LIntrinsics.IntrinsicAt(LDeclaration.FirstIntrinsic);
  Check((LEntry.Kind = pikConstant) and LEntry.Complete and
    (LEntry.DeclarationIndex = LSyntax.Named(0, 'ConstantRole')),
    'Each constant role owns its independent typed source entry');
  Check((LSyntax.Lexical.Slice(LEntry.KeySpan) = TNyxText('internconst')) and
    (OriginalText(LView, LEntry.OperandSpan) = TNyxText('CompilerKind + (2*3)')),
    'Compiler operands remain exact source expressions rather than guessed numeric values');
  Check((plfConstantIntrinsic in LDeclaration.Linkage) and
    not (plfIntrinsic in LDeclaration.Linkage) and not (psdfForward in LDeclaration.Flags),
    'Constant role does not acquire bodyless runtime-intrinsic identity');
  LEntry := LIntrinsics.IntrinsicAt(Decl(LSyntax, 'RuntimeRole').FirstIntrinsic);
  Check((LEntry.Kind = pikRuntime) and (psdfForward in Decl(LSyntax, 'RuntimeRole').Flags),
    'Runtime intrinsic retains its original bodyless declaration contract');
  Check(Decl(LSyntax, 'After').Scope = 0, 'Following routine remains a unit-interface sibling');
  LView := View('unit Bodies; interface implementation function One(X:Word):Word; ' +
    '[internconst:104]; begin Result:=X; end; function Two:Word; begin Result:=2; end; end.');
  LSyntax := LView.Syntax;
  Check(LSyntax.DiagnosticCount = 0, 'Constant annotations preserve ordinary authored implementation bodies');
  LDeclaration := Decl(LSyntax, 'One');
  Check(LSyntax.Scope(LDeclaration.NestedScope).Complete and
    not (psdfForward in LDeclaration.Flags) and (Decl(LSyntax, 'Two').Scope = 0),
    'Attributed body completion and subsequent scope ownership remain independent');
  LView := View('unit Many; interface function Both:Word; [internconst:ID,internproc:Other]; implementation end.');
  Supports(LView.Syntax, INyxPascalIntrinsicSyntax, LIntrinsics);
  LAt := Decl(LView.Syntax, 'Both').FirstIntrinsic;
  Check((LIntrinsics.IntrinsicAt(LAt).Kind = pikConstant) and
    (LIntrinsics.IntrinsicAt(LIntrinsics.IntrinsicAt(LAt).NextIntrinsic).Kind = pikRuntime),
    'Ordered metadata retains both declared roles without granting compiler compatibility');
  LView := View('unit Missing; interface function Bad:Word; [internconst:]; ' +
    'function After:Word; implementation end.');
  Supports(LView.Syntax, INyxPascalIntrinsicSyntax, LIntrinsics);
  LDeclaration := Decl(LView.Syntax, 'Bad');
  Check((psdfIncomplete in LDeclaration.Flags) and
    not LIntrinsics.IntrinsicAt(LDeclaration.FirstIntrinsic).Complete and
    (Decl(LView.Syntax, 'After').Scope = 0),
    'Missing intrinsic operands remain incomplete without consuming a separated sibling');
  LView := View('unit MissingClose; interface function Bad:Word; [internconst:104; ' +
    'function After:Word; implementation end.');
  Check((psdfIncomplete in Decl(LView.Syntax, 'Bad').Flags) and
    (Decl(LView.Syntax, 'After').Scope = 0), 'Missing bracket keeps the next routine outside the damaged annotation');
  LView := View('unit NoColon; interface function Bad:Word; [internconst 104]; ' +
    'function After:Word; implementation end.');
  Supports(LView.Syntax, INyxPascalIntrinsicSyntax, LIntrinsics);
  Check(not LIntrinsics.IntrinsicAt(Decl(LView.Syntax, 'Bad').FirstIntrinsic).Complete and
    (psdfIncomplete in Decl(LView.Syntax, 'Bad').Flags),
    'An operand without its required colon cannot become complete intrinsic metadata');
  LView := View('unit Opaque; interface function Unknown:Word; [unknown_attribute:1]; ' +
    'function After:Word; implementation end.');
  Check((psdfUnsupported in Decl(LView.Syntax, 'Unknown').Flags) and
    (Decl(LView.Syntax, 'After').Scope = 0), 'Unknown attributes retain explicit uncertainty');
  LView := View(TNyxText('unit Origin; // 🚀 dedicated origin qualification') + #10 +
    'interface function Direct:Word; internconst: CompilerChoice; implementation end.');
  Supports(LView.Syntax, INyxPascalIntrinsicSyntax, LIntrinsics);
  LEntry := LIntrinsics.IntrinsicAt(Decl(LView.Syntax, 'Direct').FirstIntrinsic);
  Check(LEntry.Complete and (LEntry.Kind = pikConstant) and
    (OriginalText(LView, LEntry.Span) = TNyxText('internconst: CompilerChoice')),
    'Direct suffix metadata preserves exact physical scalar boundaries after supplementary text');
  LRejected := False;
  try
    LIntrinsics.IntrinsicAt(LIntrinsics.IntrinsicCount);
  except
    on ENyxPascalLexical do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Checked intrinsic getters refuse out-of-snapshot ordinals');
  LSyntax := nil;
  LRejected := False;
  try
    LSyntax := NewNyxPascalSyntaxSnapshot('unit Budget; interface function One:Word; [internconst:104];',
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(1));
  except
    on ENyxPascalSyntaxLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSyntax = nil), 'Intrinsic nodes share the atomic declaration quota');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LView := NewNyxPascalTranslatedSyntax(View('unit C; interface implementation end.').Source,
      TNyxPascalSyntaxOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Cancellation publishes no partial intrinsic declaration view');
end;

{$ifndef PAS2JS}
{ Inspect the exact compiler-owned companion bytes; no closing text or substitute
  declarations are constructed for the native attribute example. }
procedure NativeCompilerAndSource(const ARoot: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LText: TNyxText;
  LView: INyxPascalTranslatedSyntax;
  LIntrinsics: INyxPascalIntrinsicSyntax;
  LEntry: TNyxPascalIntrinsicDeclaration;
  LValue: Word;
begin
  Check((CFoldedWord = $0201) and (IntrinsicBodyCalls = 0),
    'Configured FPC folds the authored constant use without calling its runtime body');
  LValue := Word(StrToInt(ParamStr(2)));
  Check((SwapFixtureWord(LValue) = $2A00) and (IntrinsicBodyCalls = 1),
    'A runtime input executes the same attributed function body exactly once');
  LStream := TFileStream.Create(String(ARoot + '/tests/intrinsic.fixture.pas'), fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size < 1024 * 1024), 'The compiler companion input is bounded');
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  LText := NyxDecodeUTF8(LBytes);
  LView := View(LText);
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The exact whole compiled Delphi-mode attribute fixture has complete declared syntax');
  Check(LView.Source.Files.TextAt(0) = LText, 'Every original compiler companion scalar stays exact');
  Supports(LView.Syntax, INyxPascalIntrinsicSyntax, LIntrinsics);
  LEntry := LIntrinsics.IntrinsicAt(Decl(LView.Syntax, 'SwapFixtureWord').FirstIntrinsic);
  Check(LEntry.Complete and (LView.Syntax.Lexical.Slice(LEntry.OperandSpan) = TNyxText('CWordSwapIntrinsic')),
    'The resolver retains the declared symbolic operation without borrowing compiler constant execution');
end;
{$endif}

begin
  try
    MetadataAndBodies;
    {$ifndef PAS2JS}
    Check(ParamCount = 2, 'Supply repository root and runtime input 42');
    NativeCompilerAndSource(TNyxText(ParamStr(1)));
    {$endif}
    WriteLn('PASS ', GChecks, ' declared intrinsic metadata and body checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
