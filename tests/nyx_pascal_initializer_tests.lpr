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
program nyx_pascal_initializer_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.editing, nyx.scheduler,
  nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.types,
  nyx.pascal.binding, nyx.pascal.names, nyx.pascal.project, nyx.pascal.drafts,
  nyx.pascal.sources, nyx.pascal.completion, nyx.pascal.completion.owned;

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

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
end;

function View(const AText: TNyxText): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('case.pas'), AText),
    NyxPascalPhysicalFile('case.pas'), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

function Decl(const ASyntax: INyxPascalSyntaxSnapshot; const AName: TNyxText): TNyxPascalSyntaxDeclaration;
var
  LAt: Integer;
begin
  LAt := ASyntax.Named(0, AName);

  if LAt < 0 then
  begin
    raise Exception.Create('Expected owned declaration is absent: ' + String(AName));
  end;
  Result := ASyntax.Declaration(LAt);
end;

{ Map a contiguous span through its retained occurrence. Virtual separators
  are projection data and cannot supply authored whitespace or Unicode bytes. }
function OriginalText(const AView: INyxPascalTranslatedSyntax; const ASpan: TNyxPascalSpan): TNyxText;
var
  LFirst: TNyxPascalSyntaxOrigin;
  LLast: TNyxPascalSyntaxOrigin;
begin
  LFirst := AView.Origin(AView.OriginAt(ASpan.Start));
  LLast := AView.Origin(AView.OriginAt(ASpan.Finish - 1));

  if LFirst.Occurrence <> LLast.Occurrence then
  begin
    raise Exception.Create('The fixture span needs one exact source occurrence');
  end;
  Result := NyxPascalOccurrenceLexical(AView.Source, LFirst.Occurrence).Slice(
    NyxPascalSpan(LFirst.PhysicalSpan.Start, LLast.PhysicalSpan.Finish - LFirst.PhysicalSpan.Start));
end;

procedure RangesAndRecovery;
const
  CText: TNyxText = 'unit Hinted; interface var Flag:Boolean platform=False; ' +
    'Count:LongInt deprecated ''Use count 🚀''=40; ' +
    'Following:LongInt=2; procedure NextRoutine; implementation end.';
var
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LHints: INyxPascalHintSyntax;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LOther: TNyxPascalSyntaxDeclaration;
  LHint: Integer;
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LRefused: Boolean;
begin
  LView := View(CText);
  LSyntax := LView.Syntax;
  Check(LView.CompleteTranslation and (LSyntax.DiagnosticCount = 0),
    'Hints before initial values retain complete sibling declaration syntax');
  Supports(LSyntax, INyxPascalHintSyntax, LHints);
  LDeclaration := Decl(LSyntax, 'Flag');
  Check((OriginalText(LView, LDeclaration.TypeSpan) = TNyxText('Boolean')) and
    (OriginalText(LView, LDeclaration.ExpressionSpan) = TNyxText('False')),
    'The declared type and initializer exclude the intervening hint');
  Check((LHints.HintAt(LDeclaration.FirstHint).Kind = phkPlatform) and
    (OriginalText(LView, LHints.HintAt(LDeclaration.FirstHint).Span) = TNyxText('platform')),
    'Typed hint metadata retains its exact separate source span');
  LDeclaration := Decl(LSyntax, 'Count');
  Check((OriginalText(LView, LDeclaration.ExpressionSpan) = TNyxText('40')) and
    (OriginalText(LView, LHints.HintAt(LDeclaration.FirstHint).MessageSpan) =
      TNyxText('''Use count 🚀''')), 'Unicode hint messages remain outside the numeric initial value');
  Check((Decl(LSyntax, 'Following').Scope = 0) and (Decl(LSyntax, 'NextRoutine').Scope = 0),
    'Later variables and routine prototypes remain unit siblings');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LAnswer := LWorld.TypeOfSymbol(LWorld.Symbol(0, LSyntax.Named(0, 'Flag')));
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbBoolean),
    'Ordinary type binding reads Boolean independently of its hint and initial value');
  LView := View('unit Grouped; interface var A,B:LongInt deprecated ''old'' platform = 1 experimental; ' +
    'Tail:LongInt; implementation end.');
  LSyntax := LView.Syntax;
  Supports(LSyntax, INyxPascalHintSyntax, LHints);
  LDeclaration := Decl(LSyntax, 'A');
  LOther := Decl(LSyntax, 'B');
  Check((LSyntax.DiagnosticCount = 0) and (LHints.HintCount = 6) and
    (LDeclaration.FirstHint <> LOther.FirstHint),
    'Grouped structural inspection owns independent hint lists before and after values');
  LHint := LDeclaration.FirstHint;
  Check((LHints.HintAt(LHint).Kind = phkDeprecated) and
    (LHints.HintAt(LHints.HintAt(LHint).NextHint).Kind = phkPlatform) and
    (LHints.HintAt(LHints.HintAt(LHints.HintAt(LHint).NextHint).NextHint).Kind = phkExperimental),
    'Hint order follows original source across the initializer');
  LHint := LOther.FirstHint;
  while LHint >= 0 do
  begin
    Check(LHints.HintAt(LHint).DeclarationIndex = LSyntax.Named(0, 'B'),
      'Every cloned hint belongs to its independent grouped declaration');
    LHint := LHints.HintAt(LHint).NextHint;
  end;
  Check((OriginalText(LView, LOther.ExpressionSpan) = TNyxText('1')) and
    (Decl(LSyntax, 'Tail').Scope = 0), 'Group values and subsequent siblings retain exact ownership');
  LView := View('unit Values; interface var Value:LongInt=Defaults.platform + deprecated; ' +
    'Callback:function(X:LongInt):LongInt platform=nil; implementation end.');
  Check((OriginalText(LView, Decl(LView.Syntax, 'Value').ExpressionSpan) =
    TNyxText('Defaults.platform + deprecated')) and (LView.Syntax.DiagnosticCount = 0),
    'Qualified and operand hint words remain value syntax');
  LDeclaration := Decl(LView.Syntax, 'Callback');
  Check((OriginalText(LView, LDeclaration.TypeSpan) = TNyxText('function(X:LongInt):LongInt')) and
    (OriginalText(LView, LDeclaration.ExpressionSpan) = TNyxText('nil')),
    'Procedural type syntax stays separate from its hint and initializer');
  LView := View('unit Boundary; interface var Callback:function(X:LongInt):LongInt; cdecl; ' +
    'platform:LongInt=2; implementation end.');
  Check((LView.Syntax.DiagnosticCount = 0) and
    (OriginalText(LView, Decl(LView.Syntax, 'platform').ExpressionSpan) = TNyxText('2')) and
    (Decl(LView.Syntax, 'Callback').FirstHint < 0),
    'A same-named declaration after a calling-convention separator remains a sibling');
  LView := View('unit Invalid; interface var Callback:function(X:LongInt):LongInt; cdecl; ' +
    'platform=nil; implementation end.');
  Check((LView.Syntax.DiagnosticCount > 0) and
    (Decl(LView.Syntax, 'Callback').ExpressionSpan.Length = 0),
    'The compiler-refused procedural separator form never becomes a confident callback initializer');
  LView := View('unit Broken; interface var Missing:LongInt platform=; ' +
    'Following:LongInt=2; implementation end.');
  LSyntax := LView.Syntax;
  LDeclaration := Decl(LSyntax, 'Missing');
  Check((psdfIncomplete in LDeclaration.Flags) and (LDeclaration.ExpressionSpan.Length = 0) and
    (LSyntax.DiagnosticCount > 0), 'An explicit empty initializer remains incomplete with a diagnostic');
  Check(OriginalText(LView, Decl(LSyntax, 'Following').ExpressionSpan) = TNyxText('2'),
    'A missing value does not consume the next separated declaration');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  Check(LWorld.TypeOfSymbol(LWorld.Symbol(0, LSyntax.Named(0, 'Missing'))).Issue = pbiIncompleteSource,
    'The ordinary binder refuses a declaration with an explicit missing value');
  LSyntax := nil;
  LRefused := False;
  try
    LSyntax := NewNyxPascalSyntaxSnapshot(CText, TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(2));
  except
    on ENyxPascalSyntaxLimit do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LSyntax = nil), 'Hint-node exhaustion publishes no partial syntax snapshot');
end;

{$ifndef PAS2JS}
{ Read the original compiled fixture/include bytes once, without an ANSI text
  collection or source rewrite. Private repository paths remain caller inputs. }
function ReadSource(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(String(APath), fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size < 1024 * 1024), 'The compiler companion has a bounded original byte packet');
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  Result := NyxDecodeUTF8(LBytes);
end;

procedure WholeSourceAndCompletion(const ARoot: TNyxText);
var
  LFiles: INyxPascalFileCatalog;
  LText: TNyxText;
  LInclude: TNyxText;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LHints: INyxPascalHintSyntax;
  LOrigin: TNyxPascalSyntaxOrigin;
  LRoots: array of TNyxPascalPhysicalFile;
  LProvider: INyxPascalCompletionProvider;
  LSources: INyxPascalSourceSnapshot;
  LReply: INyxPascalCompletionResult;
  LMetadata: INyxOwnedPascalCompletionResult;
  LPrefix: TNyxText;
begin
  LText := ReadSource(ARoot + '/tests/hint.initializer.fixture.pas');
  LInclude := ReadSource(ARoot + '/tests/hint.initializer.fixture.inc');
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('hint.initializer.fixture.pas'), LText)
    .WithFile(NyxPascalPhysicalFile('hint.initializer.fixture.inc'), LInclude);
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('hint.initializer.fixture.pas'), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The exact whole compiled unit/include has complete declaration syntax');
  Check((LView.Source.Files.TextAt(0) = LText) and (LView.Source.Files.TextAt(1) = LInclude),
    'Whole-source inspection preserves every original Unicode scalar');
  LDeclaration := Decl(LView.Syntax, 'CurrentData');
  Supports(LView.Syntax, INyxPascalHintSyntax, LHints);
  LOrigin := LView.Origin(LView.OriginAt(LHints.HintAt(LDeclaration.FirstHint).Span.Start));
  Check((LView.Source.Files.FileAt(LOrigin.FileIndex).Name = TNyxText('hint.initializer.fixture.inc')) and
    (OriginalText(LView, LDeclaration.ExpressionSpan) = TNyxText('(platform: 40; deprecated: 2)')),
    'Hint/value origins retain the actual included file and nested contextual labels');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  Check(LWorld.TypeOfSymbol(LWorld.Symbol(0, LView.Syntax.Named(0, 'CurrentData'))).Status = pbsBound,
    'Whole-source record binding uses the declared type before its platform hint');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('system.pas'),
    'unit System; interface implementation end.').WithFile(NyxPascalPhysicalFile('objpas.pas'),
    'unit ObjPas; interface implementation end.');
  SetLength(LRoots, 3);
  LRoots[0] := NyxPascalPhysicalFile('system.pas');
  LRoots[1] := NyxPascalPhysicalFile('objpas.pas');
  LRoots[2] := NyxPascalPhysicalFile('hint.initializer.fixture.pas');
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts, LFiles, LRoots,
    TNyxOwnedPascalCompletionOptions.Defaults);
  LPrefix := 'unit Consumer; interface uses hint.initializer.fixture; implementation ' +
    'procedure Inspect; begin CurrentData.de';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),
    'unit Consumer; interface implementation end.')).WithBuffer(TNyxPascalSourceRef.Main, LPrefix);
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(LPrefix)).Take(1));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and
    (LReply.Item(0).Name = TNyxText('deprecated')),
    'Ordinary completion follows the hinted initialized record into its actual member');
  Supports(LReply, INyxOwnedPascalCompletionResult, LMetadata);
  Check((LMetadata.Definition(0).FileReference.Name = TNyxText('hint.initializer.fixture.inc')) and
    (LMetadata.DefinitionSource(0).Slice(LMetadata.Definition(0).Span) = TNyxText('deprecated')),
    'Detached member navigation retains the exact original included definition');
end;
{$endif}

begin
  try
    RangesAndRecovery;
    {$ifndef PAS2JS}
    Check(ParamCount = 1, 'Supply repository root for the unchanged compiled companion');
    WholeSourceAndCompletion(TNyxText(ParamStr(1)));
    {$endif}
    WriteLn('PASS ', GChecks, ' hinted initializer ownership and completion checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
