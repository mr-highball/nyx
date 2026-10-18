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
program nyx_pascal_noreturn_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.lexing, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated
  {$ifndef PAS2JS}, noreturn.fixture{$endif};

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

{ Controlled compiler facts permit source classification only. Neither this
  reader nor a declaration flag proves compiler eligibility or body effects. }
function View(const AText: TNyxText): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('effects.pas'), AText),
    NyxPascalPhysicalFile('effects.pas'), NewNyxPascalCompilerContext(ppcFPC,
      TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True),
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
end;

function Decl(const ASyntax: INyxPascalSyntaxSnapshot;
  const AName: TNyxText): TNyxPascalSyntaxDeclaration;
var
  LAt: Integer;
begin
  LAt := ASyntax.Named(0, AName);

  if LAt < 0 then
  begin
    raise Exception.Create('The original routine declaration is absent');
  end;
  Result := ASyntax.Declaration(LAt);
end;

{ Exact suffix spelling comes from its retained physical occurrence, including
  supplementary text before the declaration. Projected whitespace is not authored. }
function OriginalText(const AView: INyxPascalTranslatedSyntax;
  const ASpan: TNyxPascalSpan): TNyxText;
var
  LFirst: TNyxPascalSyntaxOrigin;
  LLast: TNyxPascalSyntaxOrigin;
begin
  LFirst := AView.Origin(AView.OriginAt(ASpan.Start));
  LLast := AView.Origin(AView.OriginAt(ASpan.Finish - 1));

  if LFirst.Occurrence <> LLast.Occurrence then
  begin
    raise Exception.Create('This original suffix must belong to one occurrence');
  end;
  Result := NyxPascalOccurrenceLexical(AView.Source, LFirst.Occurrence).Slice(
    NyxPascalSpan(LFirst.PhysicalSpan.Start, LLast.PhysicalSpan.Finish - LFirst.PhysicalSpan.Start));
end;

procedure DeclaredEffects;
var
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LParameter: TNyxPascalSyntaxDeclaration;
  LScope: Integer;
  LCancel: INyxCancellationScope;
  LFailed: Boolean;
  LPartial: INyxPascalTranslatedSyntax;
begin
  LView := View('unit Effects; interface procedure Stop(out Code:LongInt); ' +
    'noreturn; function After:LongInt; implementation end.');
  LSyntax := LView.Syntax;
  LDeclaration := Decl(LSyntax, 'Stop');
  Check(LView.CompleteTranslation and (LSyntax.DiagnosticCount = 0),
    'Non-returning intent retains complete unit declarations');
  Check((LDeclaration.Linkage = [plfNoReturn]) and
    not (psdfForward in LDeclaration.Flags) and (LDeclaration.FirstIntrinsic < 0),
    'A non-returning routine stays distinct from forward and runtime intrinsic declarations');
  Check(OriginalText(LView, LDeclaration.LinkageSpan) = TNyxText('noreturn;'),
    'The exact original effect suffix remains owned by its routine');
  LParameter := LSyntax.Declaration(LSyntax.Named(LDeclaration.NestedScope, 'Code'));
  Check((LParameter.ParameterMode = pspmOut) and
    (OriginalText(LView, LParameter.TypeSpan) = TNyxText('LongInt')),
    'Parameter mode and original type survive the non-returning annotation');
  Check(Decl(LSyntax, 'After').Scope = 0, 'The following prototype remains a unit sibling');
  LView := View('unit Bodies; interface implementation procedure Stop; noreturn; ' +
    'begin Halt(42); end; function After:LongInt; begin Result:=42; end; end.');
  LDeclaration := Decl(LView.Syntax, 'Stop');
  Check((LView.Syntax.DiagnosticCount = 0) and
    LView.Syntax.Scope(LDeclaration.NestedScope).Complete and
    not (psdfForward in LDeclaration.Flags), 'The annotated implementation still owns its ordinary body');
  Check(Decl(LView.Syntax, 'After').Scope = 0, 'A non-returning body does not capture its next sibling');
  LView := View('unit Functions; interface function Stop:LongInt; noreturn; implementation end.');
  Check(OriginalText(LView, Decl(LView.Syntax, 'Stop').TypeSpan) = TNyxText('LongInt'),
    'A declared function retains its result type without inferred body meaning');
  LView := View('unit Foreign; interface implementation procedure Stop; noreturn; external; end.');
  LDeclaration := Decl(LView.Syntax, 'Stop');
  Check((LView.Syntax.DiagnosticCount = 0) and
    (LDeclaration.Linkage = [plfNoReturn, plfExternal]) and (psdfForward in LDeclaration.Flags),
    'External linkage independently supplies bodyless ownership');
  LView := View('unit Forwarded; interface implementation procedure Stop; noreturn; forward; end.');
  Check((LView.Syntax.DiagnosticCount = 0) and (psdfForward in Decl(LView.Syntax, 'Stop').Flags),
    'An explicit forward remains independent of non-returning intent');
  LView := View('unit Members; interface type TOne=class private procedure Stop; ' +
    'noreturn; public procedure After; end; implementation end.');
  LScope := Decl(LView.Syntax, 'TOne').NestedScope;
  Check((LView.Syntax.DiagnosticCount = 0) and
    (LView.Syntax.Declaration(LView.Syntax.Named(LScope, 'After')).Visibility = psavPublic),
    'A following class visibility section keeps its original role');
  LView := View('unit Bracket; interface procedure Stop; [noreturn]; implementation end.');
  Check((LView.Syntax.DiagnosticCount = 0) and
    (Decl(LView.Syntax, 'Stop').Linkage = [plfNoReturn, plfPlatformAttributes]),
    'Bracket syntax retains declared intent without granting compiler eligibility');
  LView := View('unit Bad; interface procedure Stop; noreturn:42; procedure After; implementation end.');
  Check((LView.Syntax.DiagnosticCount > 0) and
    (psdfUnsupported in Decl(LView.Syntax, 'Stop').Flags) and (Decl(LView.Syntax, 'After').Scope = 0),
    'An operand on the operand-free suffix remains unsupported without losing the next sibling');
  LView := View('unit BadBracket; interface procedure Stop; [noreturn:42]; ' +
    'procedure After; implementation end.');
  Check((LView.Syntax.DiagnosticCount > 0) and
    (psdfUnsupported in Decl(LView.Syntax, 'Stop').Flags) and (Decl(LView.Syntax, 'After').Scope = 0),
    'Malformed bracket operands retain explicit uncertainty and following declaration ownership');
  LView := View('unit Unknown; interface procedure Stop; [noreturn,other_effect:1]; implementation end.');
  Check((psdfUnsupported in Decl(LView.Syntax, 'Stop').Flags) and
    (plfNoReturn in Decl(LView.Syntax, 'Stop').Linkage),
    'Unknown neighbors do not inherit a preceding effect key');
  LView := View('unit Names; interface var noreturn:LongInt; procedure After; implementation end.');
  Check((LView.Syntax.DiagnosticCount = 0) and (Decl(LView.Syntax, 'noreturn').Kind = psdkVariable) and
    (Decl(LView.Syntax, 'noreturn').Linkage = []), 'The suffix key remains contextual outside routines');
  LView := View('unit Escaped; interface procedure &noreturn; procedure After; implementation end.');
  Check((LView.Syntax.DiagnosticCount = 0) and (Decl(LView.Syntax, 'noreturn').Linkage = []),
    'An escaped authored routine name does not acquire non-returning intent');
  LView := View('unit Missing; interface procedure Stop; noreturn implementation end.');
  Check((psdfIncomplete in Decl(LView.Syntax, 'Stop').Flags) and (LView.Syntax.DiagnosticCount > 0),
    'A missing separator keeps the annotated routine incomplete');
  LView := View(TNyxText('unit Original; // 🚀 dedicated physical-origin input') + #10 +
    'interface procedure Stop; noreturn; implementation end.');
  Check(OriginalText(LView, Decl(LView.Syntax, 'Stop').LinkageSpan) = TNyxText('noreturn;'),
    'Supplementary Unicode preserves original scalar suffix coordinates');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LPartial := nil;
  LFailed := False;
  try
    LPartial := NewNyxPascalTranslatedSyntax(LView.Source, TNyxPascalSyntaxOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LPartial = nil), 'Cancellation publishes no partial effect snapshot');
  LView := View('unit Budget; interface procedure Stop; noreturn; ' +
    'procedure After; implementation end.');
  LFailed := False;
  try
    LPartial := NewNyxPascalTranslatedSyntax(LView.Source,
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(1));
  except
    on ENyxPascalSyntaxLimit do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LPartial = nil), 'Construction exhaustion publishes no partial effect snapshot');
end;

{$ifndef PAS2JS}
{ ParamStr owns a temporary string. Return from this ordinary helper before a
  deliberate Halt so the caller carries no live argument-comparison temporary. }
function TerminationChild: Boolean;
begin
  Result := (ParamCount = 1) and (ParamStr(1) = '--noreturn-child');
end;

{ The caller retains exact UTF-8 source bytes; routine body/marker inspection is
  separate from the explicitly launched child's actual process termination. }
procedure NativeSource(const ARoot: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LText: TNyxText;
  LView: INyxPascalTranslatedSyntax;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LAt: Integer;
  LBodies: Integer;
begin
  Check(FollowingFixtureValue = 42, 'The original ordinary following routine executes independently');
  LStream := TFileStream.Create(String(ARoot + '/tests/noreturn.fixture.pas'), fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size < 1024 * 1024), 'The native companion packet is bounded');
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  LText := NyxDecodeUTF8(LBytes);
  LView := View(LText);
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The whole compiler-accepted Delphi-mode source retains complete syntax');
  Check(LView.Source.Files.TextAt(0) = LText, 'Source inspection retains every original companion scalar');
  LDeclaration := Decl(LView.Syntax, 'StopFixture');
  Check((LDeclaration.Linkage = [plfNoReturn]) and
    (OriginalText(LView, LDeclaration.LinkageSpan) = TNyxText('noreturn;')),
    'The native declaration retains exact non-returning source intent');
  LBodies := 0;
  for LAt := 0 to LView.Syntax.DeclarationCount - 1 do
  begin
    LDeclaration := LView.Syntax.Declaration(LAt);

    if (LDeclaration.Kind = psdkRoutine) and
      (LDeclaration.Visibility = psavImplementation) and
      (LDeclaration.NestedScope >= 0) and LView.Syntax.Scope(LDeclaration.NestedScope).Complete then
    begin
      Inc(LBodies);
    end;
  end;
  Check(LBodies = 2, 'Both original implementation bodies remain independent and complete');
end;
{$endif}

begin
  {$ifndef PAS2JS}
  { Child mode runs before analysis allocations and must terminate this process.
    The external native runner checks the exit status and marker independently. }

  if TerminationChild then
  begin
    StopFixture(42);
  end;
  {$endif}
  try
    DeclaredEffects;
    {$ifndef PAS2JS}
    Check(ParamCount = 1, 'Supply repository root for the exact native compiler companion');
    NativeSource(TNyxText(ParamStr(1)));
    {$endif}
    WriteLn('PASS ', GChecks, ' non-returning declaration and body checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
