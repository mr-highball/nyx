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
program nyx_pascal_rtlproc_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.lexing, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated
  {$ifndef PAS2JS}, rtl.roles.fixture{$endif};

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
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('roles.pas'), AText),
    NyxPascalPhysicalFile('roles.pas'), NewNyxPascalCompilerContext(ppcFPC,
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
    raise Exception.Create('The original RTL-role declaration is absent');
  end;
  Result := ASyntax.Declaration(LAt);
end;

{ Source expressions map through the retained occurrence, never projection
  whitespace. The caller retains the view owning every returned scalar span. }
function OriginalText(const AView: INyxPascalTranslatedSyntax; const ASpan: TNyxPascalSpan): TNyxText;
var
  LFirst: TNyxPascalSyntaxOrigin;
  LLast: TNyxPascalSyntaxOrigin;
begin
  LFirst := AView.Origin(AView.OriginAt(ASpan.Start));
  LLast := AView.Origin(AView.OriginAt(ASpan.Finish - 1));

  if LFirst.Occurrence <> LLast.Occurrence then
  begin
    raise Exception.Create('This fixture span needs one original occurrence');
  end;
  Result := NyxPascalOccurrenceLexical(AView.Source, LFirst.Occurrence).Slice(
    NyxPascalSpan(LFirst.PhysicalSpan.Start, LLast.PhysicalSpan.Finish - LFirst.PhysicalSpan.Start));
end;

procedure RolesAndBoundaries;
var
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LParameter: TNyxPascalSyntaxDeclaration;
  LScope: Integer;
  LRejected: Boolean;
  LCancel: INyxCancellationScope;
begin
  LView := View('unit Roles; interface procedure Copy(out Target:RawByteString; ' +
    'const Source:RawByteString); rtlproc; compilerproc; function After:LongInt; implementation end.');
  LSyntax := LView.Syntax;
  LDeclaration := Decl(LSyntax, 'Copy');
  Check(LView.CompleteTranslation and (LSyntax.DiagnosticCount = 0),
    'RTL and compiler-helper suffixes retain complete unit declarations');
  Check((LDeclaration.Linkage = [plfRTLProcedure, plfCompilerHelper]) and
    not (psdfForward in LDeclaration.Flags), 'RTL role stays distinct from a bodyless runtime intrinsic');
  Check(OriginalText(LView, LDeclaration.LinkageSpan) = TNyxText('rtlproc; compilerproc;'),
    'Adjacent declared roles retain the exact original suffix envelope');
  LScope := LDeclaration.NestedScope;
  LParameter := LSyntax.Declaration(LSyntax.Named(LScope, 'Target'));
  Check((LParameter.ParameterMode = pspmOut) and
    (OriginalText(LView, LParameter.TypeSpan) = TNyxText('RawByteString')),
    'Out parameter syntax is retained independently of compiler-specific matching');
  LParameter := LSyntax.Declaration(LSyntax.Named(LScope, 'Source'));
  Check(LParameter.ParameterMode = pspmConst, 'The neighboring const parameter keeps its original mode');
  Check(Decl(LSyntax, 'After').Scope = 0, 'Following prototypes remain unit siblings');
  LView := View('unit Bodies; interface implementation function One(X:LongInt):LongInt; ' +
    'rtlproc; compilerproc; begin Result:=X; end; function After:LongInt; begin Result:=2; end; end.');
  LDeclaration := Decl(LView.Syntax, 'One');
  Check((LView.Syntax.DiagnosticCount = 0) and
    LView.Syntax.Scope(LDeclaration.NestedScope).Complete and not (psdfForward in LDeclaration.Flags),
    'RTL/helper roles preserve the ordinary implementation body');
  Check(Decl(LView.Syntax, 'After').Scope = 0, 'An attributed body does not capture the following sibling');
  LView := View('unit ClassRoles; interface type TOne=class private ' +
    'procedure One; rtlproc; public procedure Two; end; implementation end.');
  LScope := Decl(LView.Syntax, 'TOne').NestedScope;
  LDeclaration := LView.Syntax.Declaration(LView.Syntax.Named(LScope, 'Two'));
  Check((LView.Syntax.DiagnosticCount = 0) and (LDeclaration.Visibility = psavPublic),
    'A following class visibility section retains its lexical role');
  LView := View('unit Bracket; interface procedure One; [rtlproc,compilerproc]; implementation end.');
  Check((LView.Syntax.DiagnosticCount = 0) and
    (Decl(LView.Syntax, 'One').Linkage = [plfPlatformAttributes, plfRTLProcedure, plfCompilerHelper]),
    'Bracket syntax preserves both declared classifications without compiler admission claims');
  LView := View('unit Bad; interface procedure One; rtlproc:42; procedure After; implementation end.');
  Check((psdfUnsupported in Decl(LView.Syntax, 'One').Flags) and
    (LView.Syntax.DiagnosticCount > 0) and (Decl(LView.Syntax, 'After').Scope = 0),
    'An operand on the operand-free marker remains explicit unsupported syntax');
  LView := View('unit BadBracket; interface procedure One; [rtlproc:42]; procedure After; implementation end.');
  Check((psdfUnsupported in Decl(LView.Syntax, 'One').Flags) and (Decl(LView.Syntax, 'After').Scope = 0),
    'The bracket form retains the same operand-free role boundary');
  LView := View('unit Unknown; interface procedure One; [rtlproc,unknown_attribute:1]; implementation end.');
  Check((psdfUnsupported in Decl(LView.Syntax, 'One').Flags) and
    (plfRTLProcedure in Decl(LView.Syntax, 'One').Linkage),
    'Unknown neighboring attributes retain uncertainty without inheriting the previous key');
  LView := View(TNyxText('unit Physical; // 🚀 dedicated origin check') + #10 +
    'interface procedure One; rtlproc; implementation end.');
  Check(OriginalText(LView, Decl(LView.Syntax, 'One').LinkageSpan) = TNyxText('rtlproc;'),
    'Supplementary text does not change the original role envelope');
  LView := View('unit Missing; interface procedure One; rtlproc implementation end.');
  Check((psdfIncomplete in Decl(LView.Syntax, 'One').Flags) and (LView.Syntax.DiagnosticCount > 0),
    'A missing marker separator retains incomplete declaration ownership');
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
  Check(LRejected, 'Cancellation publishes no partial RTL procedure source view');
end;

{$ifndef PAS2JS}
{ Read exact compiler companion bytes without an ANSI text collection, source
  rewrite or invented missing SDK declaration. Native ABI use is compiler evidence. }
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
  Check(RunRTLProcedureFixture = 42, 'Whole compiled callable RTL-only signature and body calls execute as authored');
  LStream := TFileStream.Create(String(ARoot + '/tests/rtl.roles.fixture.pas'), fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size < 1024 * 1024), 'The native compiler source packet is bounded');
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  LText := NyxDecodeUTF8(LBytes);
  LView := View(LText);
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The exact whole compiled Delphi-mode companion retains complete syntax');
  Check(LView.Source.Files.TextAt(0) = LText, 'Source inspection preserves every original companion scalar');
  LDeclaration := Decl(LView.Syntax, 'nyx_compiler_fixture_copy');
  Check((LDeclaration.Linkage = [plfRTLProcedure, plfCompilerHelper]) and
    (OriginalText(LView, LDeclaration.LinkageSpan) = TNyxText('rtlproc; compilerproc;')),
    'Compiler-accepted helper roles keep exact source ownership');
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
  Check(LBodies = 4, 'All original implementation bodies remain independent and complete');
end;
{$endif}

begin
  try
    RolesAndBoundaries;
    {$ifndef PAS2JS}
    Check(ParamCount = 1, 'Supply repository root for the original native compiler companion');
    NativeSource(TNyxText(ParamStr(1)));
    {$endif}
    WriteLn('PASS ', GChecks, ' declared RTL procedure and signature checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
