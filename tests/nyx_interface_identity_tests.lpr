{ Capability queries must never accept an unrelated method layout. MIT. }
program nyx_interface_identity_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.logic, nyx.logic.collections,
  nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.types,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.syntax.translated, nyx.pascal.binding, nyx.pascal.conditions
  {$ifdef PAS2JS}, Web, nyx.tool.canvas.browser{$endif};

type
  { An ordinary visitor records real scanner callbacks. Its destruction proves
    that the returned snapshot did not keep its construction-only observer. }
  TCountingObserver = class(TInterfacedObject, INyxPascalLexicalObserver)
  public
    Calls: Integer;
    destructor Destroy; override;
    procedure AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
      const AText: TNyxText; var AAllowNestedComments: Boolean);
  end;

const
  CSource: TNyxText = 'unit IdentityCase; interface type TSample=record ' +
    'Value:Byte; end; implementation end.';
  CQuestions: TNyxText = 'unit IdentityCase; interface ' +
    '{$if declared(TAbsent)}type TUnseen=Byte;{$endif} implementation end.';
var
  GChecks, GObserversDestroyed: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

destructor TCountingObserver.Destroy;
begin
  Inc(GObserversDestroyed);
  inherited Destroy;
end;

procedure TCountingObserver.AfterToken(AIndex: Integer;
  const AToken: TNyxPascalToken; const AText: TNyxText;
  var AAllowNestedComments: Boolean);
begin
  Inc(Calls);
end;

{ These are controlled source-grammar facts, not a captured SDK or target-size
  claim. The identity checks require no compiler process or primitive probe. }
function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
end;

procedure ObserverAndSnapshot;
var
  LObject: TCountingObserver;
  LObserver, LFoundObserver, LWrongObserver: INyxPascalLexicalObserver;
  LLexical: INyxPascalLexicalSnapshot;
  LSyntax, LFoundSyntax, LWrongSyntax: INyxPascalSyntaxSnapshot;
begin
  LObject := TCountingObserver.Create;
  LObserver := LObject;
  Check(Supports(LObserver, INyxPascalLexicalObserver, LFoundObserver),
    'A real lexical visitor exposes its own callback contract');
  Check(not Supports(LObserver, INyxPascalSyntaxSnapshot, LWrongSyntax),
    'A visitor cannot be admitted as immutable syntax with a different vtable');
  Check(LWrongSyntax = nil, 'An unsupported syntax query leaves no typed lease');
  LLexical := NewNyxPascalContextualLexicalSnapshot(CSource,
    TNyxPascalLexicalOptions.Defaults, LFoundObserver);
  Check(LObject.Calls = LLexical.Count, 'Every real scanned token still reaches the visitor');
  LSyntax := NewNyxPascalSyntaxFromLexical(LLexical,
    TNyxPascalSyntaxOptions.Defaults);
  Check(Supports(LSyntax, INyxPascalSyntaxSnapshot, LFoundSyntax),
    'The production parser retains its primary syntax capability');
  Check(not Supports(LSyntax, INyxPascalLexicalObserver, LWrongObserver),
    'A syntax snapshot cannot be admitted as a construction visitor');
  Check(LWrongObserver = nil, 'An unsupported visitor query leaves no typed lease');
  LObserver := nil;
  Check(GObserversDestroyed = 0, 'A successful capability query retains its actual owner');
  LFoundObserver := nil;
  Check(GObserversDestroyed = 1, 'The scanner releases its observer before publication');
  LSyntax := nil;
  LLexical := nil;
  Check((LFoundSyntax.Lexical.Source = CSource) and (LFoundSyntax.DiagnosticCount = 0),
    'A retained real syntax capability keeps exact source after its siblings retire');
end;

procedure DeclaredTypeAndProjection;
var
  LFiles: INyxPascalFileCatalog;
  LRaw: INyxPascalTranslation;
  LView, LFoundView, LWrongView: INyxPascalTranslatedSyntax;
  LType, LApplied: INyxPascalTypeSyntax;
  LDeclared, LWrongDeclared: INyxPascalDeclaredTypeSyntax;
  LPlan: INyxPascalTypePlan;
  LIndex: Integer;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('identity.pas'), CSource);
  LRaw := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('identity.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
  LView := NewNyxPascalTranslatedSyntax(LRaw, TNyxPascalSyntaxOptions.Defaults);
  Check(Supports(LView, INyxPascalTranslatedSyntax, LFoundView),
    'The production translated projection keeps its primary contract');
  Check(not Supports(LView, INyxPascalDeclaredTypeSyntax, LWrongDeclared),
    'A projection cannot be mistaken for the two-slot declared-type capability');
  Check(LWrongDeclared = nil, 'A projection grants no false declared-type lease');
  LIndex := LView.Syntax.Named(0, 'TSample');
  Check(LIndex >= 0, 'The actual source owns the declared record');
  LType := LView.TypeOfDeclaration(LIndex, TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and Supports(LType, INyxPascalDeclaredTypeSyntax, LDeclared),
    'An actual structured type exposes its own declared-source capability');
  Check((LDeclared.Declarations = LView.Syntax) and
    (LDeclared.DeclarationOf(LType.Root) = LIndex),
    'The declared-type slots dispatch to their actual source and record ordinal');
  Check(not Supports(LType, INyxPascalTranslatedSyntax, LWrongView),
    'Type syntax cannot masquerade as its containing translated projection');
  Check(LWrongView = nil, 'Type syntax grants no false projection lease');
  LPlan := CaptureNyxPascalTypePlan(LType, LIndex);
  Check(LPlan <> nil, 'The existing owner can capture the complete record plan');
  LApplied := ApplyNyxPascalTypePlan(LPlan, LView.Syntax, LIndex,
    TNyxPascalTypeOptions.Defaults);
  Check((LApplied <> nil) and Supports(LApplied, INyxPascalDeclaredTypeSyntax, LDeclared),
    'The independently rehosted plan implements the corrected optional identity');
  Check(not Supports(LApplied, INyxPascalTranslatedSyntax, LWrongView),
    'A rehosted plan also refuses the unrelated projection contract');
  LView := nil;
  LRaw := nil;
  LFiles := nil;
  Check((LFoundView.Source.Files.TextAt(0) = CSource) and
    (LDeclared.Declarations = LFoundView.Syntax),
    'Retained capabilities keep their genuine source owners independently');
end;

procedure QuestionsAndSemanticResult;
var
  LFiles: INyxPascalFileCatalog;
  LRaw, LPrefix: INyxPascalTranslation;
  LQuestions, LWrongQuestions: INyxPascalUnresolvedConditions;
  LResult, LFoundResult, LWrongResult: INyxPascalSemanticTranslation;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('questions.pas'), CQuestions)
    .WithFile(NyxPascalPhysicalFile('plain.pas'), CSource);
  LRaw := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('questions.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
  Check(Supports(LRaw, INyxPascalUnresolvedConditions, LQuestions) and
    (LQuestions.UnresolvedCount = 1),
    'Raw translation exposes its actual unresolved source question');
  Check(not Supports(LRaw, INyxPascalSemanticTranslation, LWrongResult),
    'A raw source question is not a semantic-refinement result');
  Check(LWrongResult = nil, 'Raw translation grants no false refinement lease');
  { Prefixes end before an existing token; the final EOF is a valid boundary. }
  LPrefix := NewNyxPascalTranslationPrefix(LRaw, LRaw.Count - 1,
    TNyxPascalTranslationOptions.Defaults);
  Check(Supports(LPrefix, INyxPascalUnresolvedConditions, LQuestions) and
    (LQuestions.UnresolvedCount = 1), 'A real immutable prefix retains its question capability');
  Check(not Supports(LPrefix, INyxPascalSemanticTranslation, LWrongResult),
    'An immutable prefix still grants no refinement-result slots');
  LResult := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('plain.pas'),
    Facts, nil, TNyxPascalTranslationOptions.Defaults,
    TNyxPascalSyntaxOptions.Defaults, TNyxPascalConditionalOptions.Defaults);
  Check(Supports(LResult, INyxPascalSemanticTranslation, LFoundResult),
    'The production semantic result retains its primary contract');
  Check(not Supports(LResult, INyxPascalUnresolvedConditions, LWrongQuestions),
    'A semantic receipt cannot masquerade as an unresolved-question vector');
  Check(LWrongQuestions = nil, 'The semantic result grants no false question lease');
  LResult := nil;
  LRaw := nil;
  LPrefix := nil;
  LFiles := nil;
  Check(LFoundResult.Source.Resolved and not LFoundResult.Pending and
    (LFoundResult.Answers = 0) and (LFoundResult.Source.Files.TextAt(1) = CSource),
    'A retained semantic capability still dispatches its real complete result');
  Check(LQuestions.UnresolvedAt(0).Expression <> '',
    'A separately retained source-question capability keeps its genuine data');
end;

procedure CollectionCapability;
var
  LFilter, LFoundFilter: INyxJSONFilter;
  LInput, LOutput: TNyxDataValue;
  {$ifdef PAS2JS}
  LWrongView: INyxBrowserToolCanvasView;
  {$endif}
begin
  LFilter := NewNyxJSONFilter(NyxDataPath, njcEqual, NyxData(True));
  Check(Supports(LFilter, INyxJSONFilter, LFoundFilter),
    'The production collection operation retains its specialized filter contract');
  {$ifdef PAS2JS}
  Check(not Supports(LFilter, INyxBrowserToolCanvasView, LWrongView),
    'A real JSON operation cannot be admitted as a mounted browser canvas view');
  Check(LWrongView = nil, 'A filter grants no false renderer-bearing lease');
  {$endif}
  LFilter := nil;
  LInput := NyxArray([NyxData(False), NyxData(True), NyxData(False)]);
  LOutput := LFoundFilter.Filter(LInput);
  Check((LOutput.Count = 1) and (LOutput.Item(0).ToJSON = 'true') and (LInput.Count = 3),
    'The retained actual filter still preserves input and returns the typed result');
end;

begin
  try
    ObserverAndSnapshot;
    DeclaredTypeAndProjection;
    QuestionsAndSemanticResult;
    CollectionCapability;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' portable interface identity checks';
    {$else}
    WriteLn('PASS ', GChecks, ' interface identity checks / production owners inspected');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.ClassName, ': ', E.Message, ' at check count ', GChecks);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
