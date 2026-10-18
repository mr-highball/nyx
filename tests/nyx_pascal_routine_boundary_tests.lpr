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
program nyx_pascal_routine_boundary_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.types,
  nyx.pascal.binding
  {$ifndef PAS2JS}, routine.boundary.fixture{$endif};

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
    raise Exception.Create('The routine declaration is absent: ' + String(AName));
  end;
  Result := ASyntax.Declaration(LAt);
end;

{ Follow physical occurrence ranges rather than synthetic projection spacing. }
function OriginalText(const AView: INyxPascalTranslatedSyntax; const ASpan: TNyxPascalSpan): TNyxText;
var
  LFirst: TNyxPascalSyntaxOrigin;
  LLast: TNyxPascalSyntaxOrigin;
begin
  LFirst := AView.Origin(AView.OriginAt(ASpan.Start));
  LLast := AView.Origin(AView.OriginAt(ASpan.Finish - 1));

  if LFirst.Occurrence <> LLast.Occurrence then
  begin
    raise Exception.Create('This exact range comparison requires one retained occurrence');
  end;
  Result := NyxPascalOccurrenceLexical(AView.Source, LFirst.Occurrence).Slice(
    NyxPascalSpan(LFirst.PhysicalSpan.Start, LLast.PhysicalSpan.Finish - LFirst.PhysicalSpan.Start));
end;

procedure ClausesAndTypes;
const
  CText: TNyxText = 'unit Clauses; interface type TValue=record Count:LongInt; end; ' +
    '&cdecl=LongInt; procedure Copy(out Data:array of AnsiChar;const Text:RawByteString)compilerproc inline; ' +
    'function Build:TValue cdecl inline overload; ' +
    'function Escaped:&cdecl stdcall; procedure Later; implementation end.';
var
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LNode: TNyxPascalSyntaxDeclaration;
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LScope: Integer;
begin
  LView := View(CText);
  LSyntax := LView.Syntax;
  Check(LView.CompleteTranslation and (LSyntax.DiagnosticCount = 0),
    'Supported adjacent header and modifier boundaries retain complete syntax');
  LNode := Decl(LSyntax, 'Copy');
  Check((LNode.Linkage = [plfCompilerHelper]) and not (psdfIncomplete in LNode.Flags),
    'Compiler-helper intent survives an omitted initial and intermediate separator');
  LScope := LNode.NestedScope;
  Check((LSyntax.Scope(LScope).DeclarationCount = 2) and
    (LSyntax.Declaration(LSyntax.Named(LScope, 'Data')).ParameterMode = pspmOut),
    'Untouched grouped parameter syntax retains its actual owner and passing mode');
  Check(OriginalText(LView, LNode.LinkageSpan) = TNyxText('compilerproc'),
    'The linkage range ends before the neighboring INLINE clause');
  LNode := Decl(LSyntax, 'Build');
  Check((OriginalText(LView, LNode.TypeSpan) = TNyxText('TValue')) and
    (psdfOverload in LNode.Flags), 'The result excludes calling clauses and retains adjacent OVERLOAD intent');
  Check(OriginalText(LView, Decl(LSyntax, 'Escaped').TypeSpan) = TNyxText('&cdecl'),
    'An escaped contextual type name cannot become a convention clause');
  Check((Decl(LSyntax, 'Later').Scope = 0) and LSyntax.Scope(LNode.NestedScope).Complete,
    'Following routines remain complete module siblings');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LAnswer := LWorld.TypeOfSymbol(LWorld.Symbol(0, LSyntax.Named(0, 'Build')));
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Kind = pstDeclared),
    'Ordinary routine result binding sees the declared record without calling modifiers');
  LView := View('unit Qualified; interface type TBox<T>=record Value:T; end; ' +
    'function First:Names.cdecl inline; function Second:TBox<&cdecl> cdecl; implementation end.');
  Check((LView.Syntax.DiagnosticCount = 0) and
    (OriginalText(LView, Decl(LView.Syntax, 'First').TypeSpan) = TNyxText('Names.cdecl')) and
    (OriginalText(LView, Decl(LView.Syntax, 'Second').TypeSpan) = TNyxText('TBox<&cdecl>')),
    'Qualified and nested generic type names remain inside the result range');
  LView := View('unit Markers; interface procedure Stop noreturn compilerproc; ' +
    'procedure Rtl rtlproc inline; implementation end.');
  Check((LView.Syntax.DiagnosticCount = 0) and
    (Decl(LView.Syntax, 'Stop').Linkage = [plfNoReturn, plfCompilerHelper]) and
    (Decl(LView.Syntax, 'Rtl').Linkage = [plfRTLProcedure]),
    'Adjacent zero-operand role clauses retain separate typed intent');
  LView := View('unit Forwarded; interface implementation function Later(A:LongInt):LongInt ' +
    'cdecl forward; function Later(A:LongInt):LongInt; begin Result:=A; end; end.');
  Check((LView.Syntax.DiagnosticCount = 0) and (psdfForward in Decl(LView.Syntax, 'Later').Flags) and
    (Decl(LView.Syntax, 'Later').NextNamed >= 0),
    'Adjacent forward intent retains its following independent implementation declaration');
end;

procedure RefusalAndOwnership;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LOther: INyxPascalSyntaxSnapshot;
  LScope: Integer;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
begin
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Missing; interface procedure Run cdecl implementation end.');
  Check((LSyntax.DiagnosticCount > 0) and (psdfIncomplete in Decl(LSyntax, 'Run').Flags),
    'A missing final separator stays incomplete rather than becoming an accepted modifier list');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit MissingRole; interface procedure Run compilerproc implementation end.');
  Check((LSyntax.DiagnosticCount > 0) and (psdfIncomplete in Decl(LSyntax, 'Run').Flags),
    'A compiler-helper marker still requires its final separator');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Visibility; interface type TBox=class ' +
    'procedure Run cdecl public Field:LongInt; end; implementation end.');
  Check(LSyntax.DiagnosticCount > 0, 'A member visibility section cannot silently terminate an unterminated directive');
  LScope := LSyntax.Declaration(LSyntax.Named(0, 'TBox')).NestedScope;
  Check((LSyntax.Named(LScope, 'Field') >= 0) and
    (LSyntax.Declaration(LSyntax.Named(LScope, 'Field')).Visibility = psavPublic),
    'Visibility recovery preserves the following real member owner');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Unknown; interface procedure Run unexpected; implementation end.');
  Check((LSyntax.DiagnosticCount > 0) and (psdfIncomplete in Decl(LSyntax, 'Run').Flags),
    'Arbitrary words do not gain routine-directive authority');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit BadMarker; interface procedure Run noreturn 7; implementation end.');
  Check((LSyntax.DiagnosticCount > 0) and (psdfUnsupported in Decl(LSyntax, 'Run').Flags),
    'A role marker cannot acquire an unrelated operand');
  LOther := nil;
  LRejected := False;
  try
    LOther := NewNyxPascalSyntaxSnapshot('unit Limited; interface procedure First inline; procedure Second; implementation end.',
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(1));
  except
    on ENyxPascalSyntaxLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LOther = nil), 'The existing declaration quota publishes no partial directive candidate');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LOther := NewNyxPascalSyntaxSnapshot('unit Cancelled; interface procedure Run compilerproc; implementation end.',
      TNyxPascalSyntaxOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LOther = nil), 'Cancellation publishes no partial routine header');
  Check(Decl(LSyntax, 'Run').Linkage = [plfNoReturn], 'Independent failures retain the prior immutable snapshot');
end;

{$ifndef PAS2JS}
function ReadSource(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(String(APath), fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size < 1024 * 1024), 'The exact compiled companion has a bounded byte packet');
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  Result := NyxDecodeUTF8(LBytes);
end;

procedure WholeCompiledSource(const ARoot: TNyxText);
var
  LFiles: INyxPascalFileCatalog;
  LView: INyxPascalTranslatedSyntax;
  LText: TNyxText;
  LInclude: TNyxText;
  LNode: TNyxPascalSyntaxDeclaration;
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LOrigin: TNyxPascalSyntaxOrigin;
begin
  Check(RoutineBoundaryValue = 42, 'The actual configured compiler executes ordinary adjacent-clause routines');
  LText := ReadSource(ARoot + '/tests/routine.boundary.fixture.pas');
  LInclude := ReadSource(ARoot + '/tests/routine.boundary.fixture.inc');
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('routine.boundary.fixture.pas'), LText)
    .WithFile(NyxPascalPhysicalFile('routine.boundary.fixture.inc'), LInclude);
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('routine.boundary.fixture.pas'), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The entire unchanged compiled unit/include has complete directive syntax');
  LNode := Decl(LView.Syntax, 'CopyChars');
  Check((LNode.Linkage = [plfCompilerHelper]) and
    (OriginalText(LView, LNode.LinkageSpan) = TNyxText('compilerproc;')),
    'The compiled adjacent compiler-helper declaration retains its exact original role range');
  LOrigin := LView.Origin(LView.OriginAt(LNode.LinkageSpan.Start));
  Check(LView.Source.Files.FileAt(LOrigin.FileIndex).Name = TNyxText('routine.boundary.fixture.inc'),
    'Routine directive navigation retains the actual included source');
  LNode := Decl(LView.Syntax, 'MakeState');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LAnswer := LWorld.TypeOfSymbol(LWorld.Symbol(0, LView.Syntax.Named(0, 'MakeState')));
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Kind = pstDeclared) and
    (OriginalText(LView, LNode.TypeSpan) = TNyxText('TRoutineValue')),
    'The exact compiled routine binds through its own record result type');
  Check((LView.Source.Files.TextAt(0) = LText) and (LView.Source.Files.TextAt(1) = LInclude),
    'Parsing preserves every scalar of the compiled source and include');
end;
{$endif}

begin
  try
    ClausesAndTypes;
    RefusalAndOwnership;
    {$ifndef PAS2JS}
    Check(ParamCount = 1, 'Supply the repository root for exact compiled companions');
    WholeCompiledSource(TNyxText(ParamStr(1)));
    {$endif}
    WriteLn('PASS ', GChecks, ' routine directive boundary and result-type checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
