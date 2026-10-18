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
program nyx_pascal_label_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.scheduler,
  nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.syntax.translated, nyx.pascal.types,
  nyx.pascal.binding, nyx.pascal.conditions
  {$ifndef PAS2JS}, scope.labels.fixture{$endif};

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

{ Compare authored bytes/scalars through a single retained occurrence. The
  analysis projection may insert token separators and is never the source oracle. }
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
    raise Exception.Create('This exact-span check requires one physical occurrence');
  end;
  Result := NyxPascalOccurrenceLexical(AView.Source, LFirst.Occurrence).Slice(
    NyxPascalSpan(LFirst.PhysicalSpan.Start, LLast.PhysicalSpan.Finish - LFirst.PhysicalSpan.Start));
end;

procedure StructureAndOwnership;
const
  CText: TNyxText = 'unit Labels; interface type TEntry=record Value:LongInt; end; ' +
    'implementation label Shared, 001; var Entry,Finish:TEntry; procedure Outer; ' +
    'label Finish; var Local:TEntry; procedure Inner; label Finish; begin end; ' +
    'begin end; procedure AfterLabels; begin end; end.';
var
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LLabels: INyxPascalStatementLabelSyntax;
  LLabel: TNyxPascalStatementLabel;
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LOuter: Integer;
  LInner: Integer;
  LAt: Integer;
  LRefused: Boolean;
begin
  LView := View(CText);
  LSyntax := LView.Syntax;
  Check(LView.CompleteTranslation and (LSyntax.DiagnosticCount = 0),
    'Label declarations retain closed ordinary unit and routine scopes');
  Check(Supports(LSyntax, INyxPascalStatementLabelSyntax, LLabels) and
    (LLabels.StatementLabelCount = 4), 'Labels have their own bounded optional metadata');
  LLabel := LLabels.StatementLabelAt(0);
  Check((LLabel.Kind = pslkIdentifier) and (LLabel.Name = TNyxText('Shared')) and
    (LLabel.Scope = 0) and LLabel.Complete, 'A module label retains its source and owner');
  Check((OriginalText(LView, LLabel.SectionSpan) = TNyxText('label Shared, 001;')) and
    (OriginalText(LView, LLabel.NameSpan) = TNyxText('Shared')),
    'Section and name ranges preserve the exact declaration spelling');
  LLabel := LLabels.StatementLabelAt(1);
  Check((LLabel.Kind = pslkDecimal) and (LLabel.Name = TNyxText('001')) and
    (OriginalText(LView, LLabel.NameSpan) = TNyxText('001')),
    'Numeric labels retain leading zeros without host numeric conversion');
  LOuter := LSyntax.Declaration(LSyntax.Named(0, 'Outer')).NestedScope;
  LInner := LSyntax.Declaration(LSyntax.Named(LOuter, 'Inner')).NestedScope;
  Check((LLabels.StatementLabelAt(2).Scope = LOuter) and
    (LLabels.StatementLabelAt(3).Scope = LInner) and (LOuter <> LInner),
    'Nested same-spelling labels retain independent routine owners');
  Check((LSyntax.Declaration(LSyntax.Named(0, 'Shared')).Kind = psdkLabel) and
    (LSyntax.Named(LOuter, 'Finish') = LLabels.StatementLabelAt(2).DeclarationIndex) and
    (LSyntax.Named(LInner, 'Finish') = LLabels.StatementLabelAt(3).DeclarationIndex),
    'Labels retain typed lexical identities without becoming values or fields');
  Check((LSyntax.Named(LOuter, 'Local') >= 0) and
    (LSyntax.Declaration(LSyntax.Named(0, 'AfterLabels')).Scope = 0),
    'Local variables and following module routines keep their real owners');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LAnswer := LWorld.Lookup(LWorld.At(0, 0, LSyntax.Lexical.Scalars),
    NyxPascalBindingName('AfterLabels'));
  Check((LAnswer.Status = pbsBound) and (LAnswer.Count = 1),
    'Ordinary semantic lookup no longer refuses a valid label-bearing module');
  LAnswer := LWorld.Lookup(LWorld.At(0, LOuter, LSyntax.Scope(LOuter).Span.Finish - 1),
    NyxPascalBindingName('Finish'));
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).Declaration.Kind = psdkLabel) and
    (LAnswer.SymbolAt(0).Declaration.Scope = LOuter),
    'A local label shadows the outer same-spelling variable in lexical lookup');
  Check(LWorld.TypeOfSymbol(LAnswer.SymbolAt(0)).Issue = pbiUnsupportedType,
    'A bound label cannot donate a value type or member receiver');
  for LAt := 0 to LSyntax.ScopeCount - 1 do
  begin
    Check(LSyntax.Scope(LAt).Complete and not LSyntax.Scope(LAt).UnsupportedSyntax,
      'Labels do not damage any enclosing or sibling body scope');
  end;
  LView := nil;
  LSyntax := nil;
  LWorld := nil;
  LAnswer := nil;
  Check(LLabels.StatementLabelAt(2).Name = TNyxText('Finish'),
    'The optional interface independently retains its immutable snapshot');
  for LAt := 0 to 1 do
  begin
    LRefused := False;
    try
      LLabel := LLabels.StatementLabelAt(-1 + LAt * 5);
    except
      on ENyxPascalLexical do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Negative and past-end label ordinals refuse');
  end;
end;

procedure RecoveryAndLimits;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LLabels: INyxPascalStatementLabelSyntax;
  LCancel: INyxCancellationScope;
  LRefused: Boolean;
begin
  LSyntax := NewNyxPascalSyntaxSnapshot('program Missing; label Finish begin end.',
    TNyxPascalSyntaxOptions.Defaults);
  Supports(LSyntax, INyxPascalStatementLabelSyntax, LLabels);
  Check((LSyntax.DiagnosticCount > 0) and LSyntax.Scope(0).UnsupportedSyntax and
    not LLabels.StatementLabelAt(0).Complete and (LSyntax.ScopeCount = 2),
    'A missing label separator stays incomplete without swallowing BEGIN');
  LSyntax := NewNyxPascalSyntaxSnapshot('program Trailing; label Finish,; var Tail:Byte; begin end.',
    TNyxPascalSyntaxOptions.Defaults);
  Supports(LSyntax, INyxPascalStatementLabelSyntax, LLabels);
  Check((LLabels.StatementLabelCount = 1) and not LLabels.StatementLabelAt(0).Complete and
    (LSyntax.Named(0, 'Tail') >= 0), 'A trailing comma retains the next separated declaration');
  LSyntax := NewNyxPascalSyntaxSnapshot('program Empty; label ; begin end.', TNyxPascalSyntaxOptions.Defaults);
  Supports(LSyntax, INyxPascalStatementLabelSyntax, LLabels);
  Check((LLabels.StatementLabelCount = 0) and LSyntax.Scope(0).UnsupportedSyntax,
    'An empty label list cannot become confident syntax');
  LSyntax := NewNyxPascalSyntaxSnapshot('program Radix; label $FF; begin end.', TNyxPascalSyntaxOptions.Defaults);
  Supports(LSyntax, INyxPascalStatementLabelSyntax, LLabels);
  Check((LLabels.StatementLabelCount = 0) and (LSyntax.DiagnosticCount > 0),
    'Nondecimal number tokens do not masquerade as decimal statement labels');
  LSyntax := NewNyxPascalSyntaxSnapshot('program Partial; label Finish,', TNyxPascalSyntaxOptions.Defaults);
  Supports(LSyntax, INyxPascalStatementLabelSyntax, LLabels);
  Check((LLabels.StatementLabelCount = 1) and not LLabels.StatementLabelAt(0).Complete,
    'An unfinished label list remains inspectable at EOF');
  LSyntax := NewNyxPascalSyntaxSnapshot('program Conditional; {$ifdef Unknown} label Target; {$endif} begin end.',
    TNyxPascalSyntaxOptions.Defaults);
  Supports(LSyntax, INyxPascalStatementLabelSyntax, LLabels);
  Check((LSyntax.DiagnosticCount > 0) and LLabels.StatementLabelAt(0).Conditional,
    'Untranslated conditional labels retain explicit uncertainty');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Member; interface type TInvalid=class label Target; end; implementation end.',
    TNyxPascalSyntaxOptions.Defaults);
  Supports(LSyntax, INyxPascalStatementLabelSyntax, LLabels);
  Check((LLabels.StatementLabelCount = 0) and (LSyntax.DiagnosticCount > 0),
    'Class member recovery never publishes a routine label scope');
  LSyntax := nil;
  LRefused := False;
  try
    LSyntax := NewNyxPascalSyntaxSnapshot('program Limited; label A,B; var V:Byte; begin end.',
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(2));
  except
    on ENyxPascalSyntaxLimit do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LSyntax = nil), 'Labels share the declaration quota and publish no partial index');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRefused := False;
  try
    LSyntax := NewNyxPascalSyntaxSnapshot('program Cancelled; label A; begin end.',
      TNyxPascalSyntaxOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LSyntax = nil), 'Cancelled label parsing publishes no partial snapshot');
end;

procedure ProceduralInitializers;
var
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LNode: TNyxPascalSyntaxDeclaration;
  LTypes: INyxPascalTypeSyntax;
begin
  LView := View('unit Callbacks; interface var Plain:function(X:LongInt):LongInt = @Next; ' +
    'Convention:function(X:LongInt):LongInt; stdcall = @Next; ' +
    'Unassigned:procedure; cdecl; Tail:LongInt=2; implementation end.');
  LSyntax := LView.Syntax;
  Check(LSyntax.DiagnosticCount = 0, 'Initialized procedural variables retain separated declarations');
  LNode := LSyntax.Declaration(LSyntax.Named(0, 'Plain'));
  Check((OriginalText(LView, LNode.ExpressionSpan) = TNyxText('@Next')) and
    (OriginalText(LView, LNode.TypeSpan) = TNyxText('function(X:LongInt):LongInt')),
    'An outer initializer is separate from a procedural signature');
  LNode := LSyntax.Declaration(LSyntax.Named(0, 'Convention'));
  Check((OriginalText(LView, LNode.ExpressionSpan) = TNyxText('@Next')) and
    (OriginalText(LView, LNode.TypeSpan) = TNyxText('function(X:LongInt):LongInt; stdcall')),
    'A separated calling convention remains inside the type before its initial value');
  LTypes := NewNyxPascalDeclaredTypeSyntax(LSyntax, LNode.TypeSpan,
    TNyxPascalTypeOptions.Defaults);
  Check((LTypes.DiagnosticCount = 0) and
    (LTypes.Node(LTypes.Root).CallingConvention = pccStdcall),
    'The ordinary type reader consumes the convention without an initializer suffix');
  Check((LSyntax.Declaration(LSyntax.Named(0, 'Unassigned')).ExpressionSpan.Length = 0) and
    (OriginalText(LView, LSyntax.Declaration(LSyntax.Named(0, 'Tail')).ExpressionSpan) = TNyxText('2')),
    'A convention separator cannot consume the next variable declaration');
end;

procedure ModuleGotoRequests;
var
  LView: INyxPascalTranslatedSyntax;
  LContext: INyxPascalCompilerContext;
begin
  LView := View('unit Policy; {$goto on}{$push}{$goto off}{$pop} interface implementation end.');
  LContext := LView.Source.State(LView.Source.StateCount - 1);
  Check(LView.CompleteTranslation and
    (NyxPascalCompilerPolicies(LContext).GotoRequested = ppkFalse),
    'Local settings restoration preserves the current module GOTO request');
  Check(NyxPascalCompilerPolicies(Facts).GotoRequested = ppkUnknown,
    'Source requests never mutate the caller compiler context');
  LView := View('unit Policy; {$goto off}{$mode delphi} interface implementation end.');
  LContext := LView.Source.State(LView.Source.StateCount - 1);
  Check(LView.CompleteTranslation and
    (NyxPascalCompilerPolicies(LContext).GotoRequested = ppkTrue),
    'The qualified FPC Delphi mode transition enables its module switch');
  Check(NyxPascalCompilerPolicies(LContext.UnknownState).GotoRequested = ppkUnknown,
    'An unknown side effect revokes module GOTO knowledge');
  Check(not LContext.SameFacts(NyxPascalWithCompilerPolicies(LContext,
    NyxPascalCompilerPolicies(LContext).RequestGoto(ppkFalse))),
    'GOTO requests participate in detached exact context identity');
  LView := View('unit Policy; {$ifdef UndefinedTest}{$goto on}{$endif} interface implementation end.');
  Check(LView.CompleteTranslation and
    (NyxPascalCompilerPolicies(LView.Source.State(LView.Source.StateCount - 1)).GotoRequested = ppkUnknown),
    'An inactive module directive cannot add source authority');
end;

procedure DeclaredLabels;
const
  CText: TNyxText = 'unit DeclaredLabel; {$goto on} interface implementation ' +
    'procedure Run; label Target; {$if declared(Target)}const Answer=42;' +
    '{$else}const Wrong=-1;{$endif}begin end;end.';
var
  LFiles: INyxPascalFileCatalog;
  LConditional: INyxPascalSemanticTranslation;
  LView: INyxPascalTranslatedSyntax;
  LScope: Integer;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('declared.pas'), CText);
  LConditional := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('declared.pas'),
    Facts, nil, TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults);
  LView := NewNyxPascalTranslatedSyntax(LConditional.Source, TNyxPascalSyntaxOptions.Defaults);
  Check(not LConditional.Pending and (LConditional.Answers = 1) and LView.CompleteTranslation,
    'Ordered DECLARED sees the actual prior label symbol');
  LScope := LView.Syntax.Declaration(LView.Syntax.Named(0, 'Run')).NestedScope;
  Check((LView.Syntax.Named(LScope, 'Answer') >= 0) and (LView.Syntax.Named(LScope, 'Wrong') < 0),
    'Label declaration evidence selects only the supported branch without a value-type guess');
end;

{$ifndef PAS2JS}
function ReadSource(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(String(APath), fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size < 1024 * 1024), 'The compiled fixture has a bounded byte packet');
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  Result := NyxDecodeUTF8(LBytes);
end;

procedure WholeCompiledSource(const ARoot: TNyxText);
var
  LText: TNyxText;
  LInclude: TNyxText;
  LFiles: INyxPascalFileCatalog;
  LView: INyxPascalTranslatedSyntax;
  LLabels: INyxPascalStatementLabelSyntax;
  LOrigin: TNyxPascalSyntaxOrigin;
  LAt: Integer;
  LConditional: INyxPascalSemanticTranslation;
begin
  Check((LabelledValue = 42) and (CallbackValue = 42) and (ModuleSwitchValue = 42),
    'Actual configured native compilation executes the labelled body and initialized callbacks');
  LText := ReadSource(ARoot + '/tests/scope.labels.fixture.pas');
  LInclude := ReadSource(ARoot + '/tests/scope.labels.fixture.inc');
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('scope.labels.fixture.pas'), LText)
    .WithFile(NyxPascalPhysicalFile('scope.labels.fixture.inc'), LInclude);
  LConditional := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('scope.labels.fixture.pas'),
    Facts, nil, TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults);
  LView := NewNyxPascalTranslatedSyntax(LConditional.Source, TNyxPascalSyntaxOptions.Defaults);
  for LAt := 0 to LView.Source.DiagnosticCount - 1 do
  begin
    WriteLn('compiled fixture translation / ', LView.Source.Diagnostic(LAt).MessageText);
  end;
  for LAt := 0 to LView.Syntax.DiagnosticCount - 1 do
  begin
    WriteLn('compiled fixture syntax / ', LView.Syntax.Diagnostic(LAt).MessageText);
  end;
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The entire unchanged compiled unit/include has complete declaration syntax');
  Check(not LConditional.Pending and (LConditional.Answers = 1),
    'The unchanged compiled source selects the same label-dependent conditional branch');
  Supports(LView.Syntax, INyxPascalStatementLabelSyntax, LLabels);
  Check(LLabels.StatementLabelCount = 4, 'Whole-source labels retain outer/nested owners');
  LOrigin := LView.Origin(LView.OriginAt(LLabels.StatementLabelAt(0).NameSpan.Start));
  Check((LView.Source.Files.FileAt(LOrigin.FileIndex).Name = TNyxText('scope.labels.fixture.inc')) and
    (NyxPascalOccurrenceLexical(LView.Source, LOrigin.Occurrence).Slice(LOrigin.PhysicalSpan) = TNyxText('Finish')),
    'Label navigation retains the actual included name token');
  Check((LView.Source.Files.TextAt(0) = LText) and (LView.Source.Files.TextAt(1) = LInclude),
    'Inspection preserves both exact source packets');
end;
{$endif}

begin
  try
    StructureAndOwnership;
    RecoveryAndLimits;
    ProceduralInitializers;
    ModuleGotoRequests;
    DeclaredLabels;
    {$ifndef PAS2JS}
    Check(ParamCount = 1, 'Supply the repository root for exact compiled source inspection');
    WholeCompiledSource(TNyxText(ParamStr(1)));
    {$endif}
    WriteLn('PASS ', GChecks, ' statement label and procedural initializer checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
